/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan PSL v1.
 * You can use this software according to the terms and conditions of the Mulan PSL v1.
 * You may obtain a copy of Mulan PSL v1 at:
 *
 *     http://license.coscl.org.cn/MulanPSL
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v1 for more details.
 */

#include <cstdio>
#include <cinttypes>
#include <algorithm>

#include "aarch64_rt_support.h"
#include "aarch64_cg.h"
#include "cg_assert.h"
#include "mir_builder.h"
#include "be_common.h"
#include "special_func.h"

using namespace std;

namespace maplebe {

#include "aarch64_opnd.def"
const AArch64MD AArch64CG::kMd[kMopLast] = {
#include "aarch64_md.def"
};

bool AArch64CG::IsExclusiveFunc(MIRFunction *mirFunc) {
  const std::string &funcname = mirFunc->GetName();
  for (auto it : eh_exclusive_name_vec) {
    if (it.compare(funcname) == 0) {
      return true;
    }
  }
  return false;
}

// Generate object maps.
//
// Currently, object maps are generated in a separate section
// (OBJECT_MAP_SECTION_NAME) of the resulting ELF image together with GCTIBs.  A
// GCTIB starts with a label (GCTIB_PREFIX_STR + ClassName), followed by the
// GCTIB content as described in `mapleall/runtime/doc/object-layout.markdown`.
//
// For example, if a scalar object has five ptr fields at offsets 0, 8, 24, 40,
// 64, the generated code will be like:
//
// MCC_GCTIB__TheClassName:
// .quad 1         // one word in the bitmap
// .quad 0x12b     // 0xab = 100101011
// ...

static const uint32_t kBitsPerMapWord = 64;
static const uint32_t kLogBitsPerMapWord = 6;
#ifdef USE_32BIT_REF
static const uint32_t kReferenceWordSize = 4;
static const uint32_t kLog2ReferenceWordSize = 2;
#else
static const uint32_t kReferenceWordSize = 8;
static const uint32_t kLog2ReferenceWordSize = 3;
#endif
static const uint32_t kInMapWordOffsetMask = (((kReferenceWordSize * kBitsPerMapWord)) - 1);
static const uint32_t kInMapWordIndexShift = (kLog2ReferenceWordSize);
static const uint32_t kMapWordIndexShift = (kLog2ReferenceWordSize + kLogBitsPerMapWord);
/*
 * Give a structrue type, calculate its bitmap_vector
 */
static void GetGCTIBBitMapWords(BECommon &becommon, MIRStructType *structty, vector<uint64_t> &bitmapWords) {
  bitmapWords.clear();
  if (structty->typeKind == kTypeClass) {
    uint64_t curBitmap = 0;
    uint32_t curBitmapIndex = 0;
    uint32_t prevOffset = 0;
    for (auto fieldInfo : becommon.GetJClassLayout(static_cast<MIRClassType *>(structty))) {
      if (fieldInfo.is_ref) {
        uint32_t curOffset = fieldInfo.offset;
        // skip meta field
        if (curOffset == 0) {
          continue;
        }
        CHECK_FATAL(curOffset > prevOffset || (prevOffset == 0), "not ascending offset");
        uint32_t wordIndex = curOffset >> kMapWordIndexShift;
        if (wordIndex > curBitmapIndex) {
          bitmapWords.push_back(curBitmap);
          for (uint32_t i = curBitmapIndex + 1; i < wordIndex; i++) {
            bitmapWords.push_back(0);
          }
          curBitmap = 0;
          curBitmapIndex = wordIndex;
        }
        uint32_t bit_offset = (curOffset & kInMapWordOffsetMask) >> kInMapWordIndexShift;
        if (fieldInfo.is_unowned == false) {
          // ref
          curBitmap |= ((((uint64_t)1) << bit_offset));
        }
        prevOffset = curOffset;
      }
    }
    if (curBitmap != 0) {
      bitmapWords.push_back(curBitmap);
    }
  } else if (structty->typeKind != kTypeInterface) {
    // interface doesn't have reference fields
    CHECK_FATAL(false, "GetGCTIBBitMapWords unexpected type");
  }
}

/*
 * Find if there exist same GCTIB (both rcheader and bitmap are smae)
 * for different class. If ture reuse, if not emit and record new GCTIB.
 */
void AArch64CG::FindOrCreateRepresentiveSym(vector<uint64_t> &bitmapWords, uint32_t rcHeader, string name) {
  GCTIBKey *key = mirModule->memPool->New<GCTIBKey>(rcHeader, bitmapWords);
  string gctibName = GCTIB_PREFIX_STR + name;
  unordered_map<GCTIBKey, GCTIBPattern, Hasher, EqualFn>::const_iterator iter = key2pattern.find(*key);

  if (iter == key2pattern.end()) {
    // Emit the GCTIB label for the class
    GCTIBPattern *ptn = mirModule->memPool->New<GCTIBPattern>(*key);

    string str = GCTIB_PREFIX_STR + string(NameMangler::kJavaLangObjectStr);
    if (gctibName.compare(str) == 0) {
      ptn->SetName(str);
    }
    key2pattern.insert(make_pair(*key, *ptn));
    sym2pattern.insert(make_pair(gctibName, *ptn));

    // Emit GCTIB pattern
    string ptnString = "\t.type " + ptn->GetName() + ", \%object\n" + "\t.data\n" + "\t.align 3\n";

    MIRSymbol *gctibSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(gctibName.c_str()));
    if (gctibSym && gctibSym->GetStorageClass() == kScFstatic) {
      ptnString += "\t.local ";
    } else {
      ptnString += "\t.global ";
    }

    emitter_->Emit(ptnString);
    emitter_->Emit(ptn->GetName());
    emitter_->Emit("\n");

    // Emit the GCTIB pattern label for the class
    emitter_->Emit(ptn->GetName());
    emitter_->Emit(":\n");

    emitter_->Emit("\t.long ");
    emitter_->EmitHexUnsigned(rcHeader);
    emitter_->Emit("\n");

    // generate n_bitmap word
    emitter_->Emit("\t.long ");  // AArch64-specific. Generate a 64-bit value.
    emitter_->EmitDecUnsigned(bitmapWords.size());
    emitter_->Emit("\n");

    // Emit each bitmap word
    for (auto bitmapWord : bitmapWords) {
      if (!DoItQuietly()) {
        printf("  bitmap_word: 0x%" PRIx64 "\n", bitmapWord);
      }
      emitter_->Emit("\t.quad ");  // AArch64-specific. Generate a 64-bit value.
      emitter_->EmitHexUnsigned(bitmapWord);
      emitter_->Emit("\n");
    }
  } else {
    sym2pattern.insert(make_pair(gctibName, iter->second));
  }
}

string AArch64CG::FindGCTIBPatternName(const string &name) {
  CHECK_FATAL(sym2pattern.find(name) != sym2pattern.end(), "No GCTIB pattern found for symbol: %s", name.c_str());

  unordered_map<string, GCTIBPattern>::const_iterator iter = sym2pattern.find(name);
  CHECK_FATAL(iter != sym2pattern.end(), "map find return error");
  return iter->second.GetName();
}

void AArch64CG::GenerateObjectMaps(BECommon &becommon) {
  if (!DoItQuietly()) {
    printf("DEBUG: Generating object maps...\n");
  }

  // Create a new section for object map.
  // emitter_->emit(".section ");
  // emitter_->emit(RTSupport::OBJECT_MAP_SECTION_NAME);
  // No "w" flag indicates this is read-only
  // emitter_->emit(", \"a\", @progbits\n");

  for (auto tyid : mirModule->classList) {
    if (!DoItQuietly()) {
      printf("Class tyIdx: %" PRIu32 "\n", tyid);
    }
    TyIdx tyIdx(tyid);
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);

    // Only emit GCTIB for classes owned by this module
    CHECK_FATAL(dynamic_cast<MIRStructType *>(ty) != nullptr, "ty isn't MIRStructType* in AArch64CG::GenerateObjectMaps");
    if (!dynamic_cast<MIRStructType *>(ty)->IsLocal()) {
      continue;
    }

    GStrIdx nameIdx = ty->nameStrIdx;

    const string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(nameIdx);

    // Emit for a class
    if (!DoItQuietly()) {
      printf("  name: %s\n", name.c_str());
    }

    MIRStructType *strty = dynamic_cast<MIRStructType *>(ty);
    if (strty != nullptr) {
      vector<uint64_t> bitmapWords;
      GetGCTIBBitMapWords(becommon, strty, bitmapWords);
      uint32_t rcHeader = (bitmapWords.size() > 0) ? 0x40 : 0;
      FindOrCreateRepresentiveSym(bitmapWords, rcHeader, name);
    } else {
      if (!DoItQuietly()) {
        printf("  ** Not a struct. This is weird, because it is from the class list.\n");
      }
    }
  }
}

void AArch64Insn::CheckOpnd(Operand *opnd, OpndProp *prop) {
  AArch64OpndProp *mopd = static_cast<AArch64OpndProp *>(prop);
  CG_ASSERT(mopd, "an empty operand");
  switch (opnd->op_kind_) {
    case Operand::Opd_Register:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_Register, "expect reg");
      break;
    case Operand::Opd_Immediate:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_Immediate, "expect imm");
      break;
    case Operand::Opd_FPImmediate:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_FPImmediate, "expect fpimm");
      break;
    case Operand::Opd_FPZeroImmediate:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_FPZeroImmediate, "expect fpzero");
      break;
    case Operand::Opd_Mem:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_Mem, "expect mem");
      break;
    case Operand::Opd_BbAddress:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_BbAddress, "expect address");
      break;
    case Operand::Opd_List:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_List, "expect list operand");
      break;
    case Operand::Opd_Cond:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_Cond, "expect cond operand");
      break;
    case Operand::Opd_Shift:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_Shift, "expect LSL operand");
      break;
    case Operand::Opd_StImmediate:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_StImmediate, "expect symbol name (literal)");
      break;
    case Operand::Opd_String:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_String, "expect a string");
      break;
    default:
      CG_ASSERT(false, "NYI");
  }
}

bool AArch64Insn::Check() {
  MOperator mop = GetMachineOpcode();
  const AArch64MD *md = &AArch64CG::kMd[mop];
  for (int i = 0; i < kMaxOperandNum; ++i) {
    Operand *opnd = GetOperand(i);
    // maybe if !opnd, break ?
    if (opnd) {
      CheckOpnd(opnd, md->operand_[i]);
    }
  }
  return true;
}

void AArch64Insn::dump() {
  MOperator mop = GetMachineOpcode();
  const AArch64MD *md = &AArch64CG::kMd[mop];

  LogInfo::MapleLogger() << "< " << id << " > ";
  LogInfo::MapleLogger() << md->name_ << "(" << mop << ")";

  for (int i = 0; i < kMaxOperandNum; ++i) {
    Operand *opnd = GetOperand(i);
    // maybe if !opnd, break ?
    if (opnd) {
      LogInfo::MapleLogger() << " (opnd" << i << ": ";
      opnd->dump();
      LogInfo::MapleLogger() << ")";
    }
  }
  LogInfo::MapleLogger() << std::endl;
}

bool AArch64Insn::IsDefinition() const {
  // check if we are seeing ldp or not
  CG_ASSERT(!AArch64CG::kMd[mop_].GetOperand(1) || !AArch64CG::kMd[mop_].GetOperand(1)->IsRegDef(), "");
  if (AArch64CG::kMd[mop_].GetOperand(0) == nullptr) {
    return false;
  }
  return AArch64CG::kMd[mop_].GetOperand(0)->IsRegDef();
}

bool AArch64Insn::IsDestRegAlsoSrcReg() const {
  AArch64OpndProp *prop0 = static_cast<AArch64OpndProp *>(AArch64CG::kMd[mop_].GetOperand(0));
  return prop0->IsRegDef() && prop0->IsRegUse();
}

bool AArch64Insn::IsDataMoveInstruction() const {
  return ((AArch64CG::kMd[mop_].properties_ & ISMOVE) != 0);
}

bool AArch64Insn::IsConversionInstruction() const {
  return ((AArch64CG::kMd[mop_].properties_ & ISCONVERSION) != 0);
}

bool AArch64Insn::IsConditionalSet() const {
  return ((AArch64CG::kMd[mop_].properties_ & ISCONDSET) != 0);
}

RegOperand *AArch64Insn::GetOperandDefined() {
  CG_ASSERT(IsDefinition(), "");
  return static_cast<RegOperand *>(GetOperand(0));
}

}  // namespace maplebe
