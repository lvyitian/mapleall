/*
 * Copyright (c) [2020] Huawei Technologies Co., Ltd. All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan Permissive Software License v2.
 * You can use this software according to the terms and conditions of the MulanPSL - 2.0.
 * You may obtain a copy of MulanPSL - 2.0 at:
 *
 *   https://opensource.org/licenses/MulanPSL-2.0
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the MulanPSL - 2.0 for more details.
 */

#include "emit.h"
#include "cg.h"
#include "cg_assert.h"
#include <unistd.h>
#include <cmath>
#include "lsda.h"
#include "name_mangler.h"
#include "reflection_analysis.h"
#include "muid_replacement.h"
#include "file_layout.h"
#include "metadata_layout.h"

namespace maplebe {

#define CLANG (cg_->mirModule->IsCModule())
#define JAVALANG (cg_->mirModule->IsJavaModule())

extern bool doPie;

using namespace maple;

void Emitter::EmitLabelRef(const char *name, LabelIdx labidx) {
  const char *idx = static_cast<const char*>(strdup(std::to_string(CG::curPuIdx).c_str()));
  out << ".label." << idx << "__" << labidx;
}

void Emitter::EmitStmtLabel(const char *name, LabelIdx labidx) {
  EmitLabelRef(name, labidx);
  out << ":\n";
}

void Emitter::EmitBBHeaderLabel(const char * funcName, const char * lineCommentHeader, LabelIdx labelIdx, CGFunc & cgFunc) {
  LabelOperand *lablel = cgFunc.GetOrCreateLabelOperand(labelIdx);
  if (lablel->GetLabelOrder() == -1u) {
    lablel->SetLabelOrder(cgFunc.cg->label_order_cnt_);
    cgFunc.cg->label_order_cnt_++;
  }

  const char *puIdx = strdup(std::to_string(CG::curPuIdx).c_str());
  const std::string& labelName = cgFunc.func->labelTab->GetName(labelIdx);

  Emit(".label.").Emit(puIdx).Emit("__").Emit(labelIdx).Emit(":\t\t")
    .Emit(lineCommentHeader).Emit(" CG order: ").Emit(lablel->GetLabelOrder());

  if (!labelName.empty() && labelName.at(0) != '@') {
    //If label name has @ as its first char, it is not from MIR
    Emit(", MIR: @").Emit(labelName).Emit("\n");
  } else {
    Emit("\n");
  }
}

void Emitter::EmitLabelPair(const char *name, const LabelPair &pairlabel) {
  CG_ASSERT((pairlabel.end_offset || pairlabel.start_offset), "NYI");
  EmitLabelRef(name, pairlabel.end_offset->labelIdx);
  out << " - ";
  EmitLabelRef(name, pairlabel.start_offset->labelIdx);
  out << "\n";
}

void Emitter::EmitLabelForFunc(MIRFunction *func, LabelIdx labidx) {
  const char *idx = static_cast<const char*>(strdup(std::to_string(func->puIdx).c_str()));
  out << ".label." << idx << "__" << labidx;
}

Asmlabel Emitter::GetTypeAsmInfoName(PrimType pty) {
  uint32 size = GetPrimTypeSize(pty);
  switch (size) {
    case 1:
      return kAsmByte;
    case 2:
#if TARGAARCH64 || TARGARK || TARGRISCV64
      return kAsmShort;
#else
      return kAsmValue;
#endif
    case 4:
      return kAsmLong;
    case 8:
      return kAsmQuad;
    default:
      CG_ASSERT(false, "NYI");
      return kAsmLong;
  }
}

void Emitter::EmitFileInfo(const std::string &fileName) {
  char *curDirName = get_current_dir_name();
  CHECK_FATAL(curDirName, "null ptr check ");
  std::string path(curDirName);
  std::string cgFile(path.append("/mplcg"));
  Emit(asminfo_->asm_cmnt);
  Emit(cgFile.c_str());
  Emit("\n");

  std::string compile("Compiling ");
  ;
  Emit(asminfo_->asm_cmnt);
  Emit(compile.c_str());
  Emit("\n");

  // TO DO things
  std::string beOptions("Be options");
  Emit(asminfo_->asm_cmnt);
  Emit(beOptions.c_str());
  Emit("\n");

  path = curDirName;
  path.append("/").append(fileName);
  // strip path before out/
  std::size_t pos = path.find("/out/", 0, 5);
  if (pos != std::string::npos) {
    path.erase(0, pos + 1);
  }
  std::string irFile("\"");
  irFile.append(path).append("\"");
  Emit(asminfo_->asm_file);
  Emit(irFile.c_str());
  Emit("\n");

  // .file #num src_file_name
  if (cg_->cgopt_.WithLoc()) {
    if (cg_->cgopt_.WithAsm()) {
      Emit("\t// ");
    }
    Emit(asminfo_->asm_file);
    // .file 1 is specifically reserved for mpl file. All other source files will be assigned a file-no starting from 2
    Emit("1 ");
    Emit(irFile.c_str());
    Emit("\n");
    if (cg_->cgopt_.WithSrc()) {
      // insert a list of src files
      for (auto it : cg_->mirModule->srcFileInfo) {
        if (cg_->cgopt_.WithAsm()) {
          Emit("\t// ");
        }
        Emit(asminfo_->asm_file);
        Emit(it.second).Emit(" \"");
        const std::string kStr = GlobalTables::GetStrTable().GetStringFromStrIdx(it.first);
        Emit(kStr.c_str());
        Emit("\"\n");
      }
    }
  }
  free(curDirName);
#if TARGARM
  emit("\t.syntax unified\n");
  // "The Thumb instruction set is a subset of
  //  the most commonly used 32-bit ARM instructions."
  // http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0210c/CACBCAAE.html
  emit("\t.thumb\n");
#endif /* TARGARM */
}

void Emitter::EmitAsmLabel(Asmlabel al) {
  switch (al) {
    case kAsmData: {
      Emit(asminfo_->asm_data);
      Emit("\n");
      return;
    }
    case kAsmByte: {
      Emit(asminfo_->asm_byte);
      return;
    }
    case kAsmShort: {
      Emit(asminfo_->asm_short);
      return;
    }
    case kAsmValue: {
      Emit(asminfo_->asm_value);
      return;
    }
    case kAsmLong: {
      Emit(asminfo_->asm_long);
      return;
    }
    case kAsmQuad: {
      Emit(asminfo_->asm_quad);
      return;
    }
    case kAsmZero:
      Emit(asminfo_->asm_zero);
      return;
    case kAsmBss:
    default:
      CG_ASSERT(false, "");
  }
}

void Emitter::EmitAsmLabel(const MIRSymbol *st, Asmlabel al) {
  MIRType *ty = st->GetType();
  std::string syName;
  if (st->storageClass == kScPstatic && st->IsLocal()) {
    syName = st->GetName() + to_string(CG::curPuIdx);
  } else {
    syName = st->GetName();
  }

  switch (al) {
    case kAsmGlbl: {
      Emit(asminfo_->asm_global);
      Emit(syName.c_str());
      Emit("\n");
      return;
    }
    case kAsmHidden: {
      Emit(asminfo_->asm_hidden);
      Emit(syName.c_str());
      Emit("\n");
      return;
    }
    case kAsmLocal: {
      Emit(asminfo_->asm_local);
      Emit(syName.c_str());
      Emit("\n");
      return;
    }
    case kAsmWeak: {
      Emit(asminfo_->asm_weak);
      Emit(syName.c_str());
      Emit("\n");
      return;
    }
    case kAsmComm: {
      std::string size;
      if (isFlexibleArray) {
        size = std::to_string(g->becommon->type_size_table.at(ty->tyIdx.GetIdx()) + arraySize);
      } else {
        size = std::to_string(g->becommon->type_size_table.at(ty->tyIdx.GetIdx()));
      }
      Emit(asminfo_->asm_comm);
      Emit(syName.c_str());
      Emit(", ");
      Emit(size.c_str());
      Emit(", ");
#if PECOFF
#if TARGARM || TARGAARCH64 || TARGARK || TARGRISCV64
      std::string align = std::to_string(static_cast<int>(log2(g->becommon->type_align_table[ty->tyIdx.GetIdx()])));
#else
      std::string align = std::to_string(g->becommon->type_align_table[ty->tyIdx.GetIdx()]);
#endif
      emit(align.c_str());
#else /* ELF */
      if (syName.find("__ClassInitProtectRegion__") == 0) {
        Emit(4096);
      } else if ((st->GetType()->typeKind == kTypeStruct ||
                  st->GetType()->typeKind == kTypeClass ||
                  st->GetType()->typeKind == kTypeArray ||
                  st->GetType()->typeKind == kTypeUnion)
                 &&
                 (st->storageClass == kScGlobal ||
                  st->storageClass == kScPstatic ||
                  st->storageClass == kScFstatic)) {
        int32 align = g->becommon->type_align_table[ty->tyIdx.GetIdx()];
        if (SIZEOFPTR < align) {
          Emit(std::to_string(align).c_str());
        } else {
          Emit(std::to_string(SIZEOFPTR));
        }
      } else {
        Emit(std::to_string(g->becommon->type_align_table[ty->tyIdx.GetIdx()]).c_str());
      }
#endif
      Emit("\n");
      return;
    }
    case kAsmAlign: {
      std::string align;
      if (st->GetType()->typeKind == kTypeStruct ||
          st->GetType()->typeKind == kTypeClass ||
          st->GetType()->typeKind == kTypeArray ||
          st->GetType()->typeKind == kTypeUnion) {
        align = "3";
      } else {

#if TARGARM || TARGAARCH64 || TARGARK || TARGRISCV64
        align = std::to_string(static_cast<int>(log2(g->becommon->type_align_table[ty->tyIdx.GetIdx()])));
#else
        align = std::to_string(g->becommon->type_align_table[ty->tyIdx.GetIdx()]);
#endif
      }
      Emit(asminfo_->asm_align);
      Emit(align.c_str());
      Emit("\n");
      return;
    }
    case kAsmSyname: {
      Emit(syName.c_str());
      Emit(":\n");
      return;
    }
    case kAsmSize: {
      std::string size;
      if (isFlexibleArray) {
        size = std::to_string(g->becommon->type_size_table.at(ty->tyIdx.GetIdx()) + arraySize);
      } else {
        size = std::to_string(g->becommon->type_size_table.at(ty->tyIdx.GetIdx()));
      }
      Emit(asminfo_->asm_size);
      Emit(syName.c_str());
      Emit(", ");
      Emit(size.c_str());
      Emit("\n");
      return;
    }
    case kAsmType: {
      Emit(asminfo_->asm_type);
      if (CLANG && (syName == "sys_nerr" || syName == "sys_errlist")) {
        // HACK, eliminate warning from deprecated C name
        Emit("strerror");
      } else {
        Emit(syName.c_str());
      }
      Emit(",");
      Emit(asminfo_->asm_atobt);
      Emit("\n");
      return;
    }
    default:
      CG_ASSERT(false, "");
  }
}

void Emitter::EmitNullConstant(uint32 size) {
  EmitAsmLabel(kAsmZero);
  Emit(std::to_string(size).c_str());
  Emit("\n");
}

void Emitter::EmitCombineBfldValue(StructEmitInfo *semitinfo) {
  uint8 charbitwidth = GetPrimTypeSize(PTY_i8) * 8;
  while (semitinfo->combinebfldwidth_ > charbitwidth) {
    uint64 tmp = semitinfo->combinebfldvalue_ & 0x00000000000000ffLL;
    EmitAsmLabel(kAsmByte);
    Emit(std::to_string(tmp).c_str());
    Emit("\n");
    semitinfo->combinebfldwidth_ -= charbitwidth;
    semitinfo->combinebfldvalue_ = semitinfo->combinebfldvalue_ >> charbitwidth;
  }
  if (0 != semitinfo->combinebfldwidth_) {
    EmitAsmLabel(kAsmByte);
    Emit(std::to_string(semitinfo->combinebfldvalue_).c_str());
    Emit("\n");
  }
  if (0 != (semitinfo->nextfieldOffset_ % charbitwidth)) {
    semitinfo->nextfieldOffset_ += (charbitwidth - semitinfo->nextfieldOffset_ % charbitwidth);
  }
  semitinfo->tsize_ = semitinfo->nextfieldOffset_ / charbitwidth;
  semitinfo->combinebfldvalue_ = 0;
  semitinfo->combinebfldwidth_ = 0;
}

void Emitter::EmitBitFieldConstant(StructEmitInfo *semitinfo, MIRConst *ct, const MIRType *nty, uint32 fieldoffset) {
  MIRType *ty = ct->type;
  if (fieldoffset > semitinfo->nextfieldOffset_) {
    uint8 curfieldOffset = semitinfo->nextfieldOffset_ - semitinfo->combinebfldwidth_;
    semitinfo->combinebfldwidth_ = fieldoffset - curfieldOffset;
    EmitCombineBfldValue(semitinfo);
    CG_ASSERT(semitinfo->nextfieldOffset_ <= fieldoffset, "");
    semitinfo->nextfieldOffset_ = fieldoffset;
  }
  uint32 fieldsize = static_cast<MIRBitfieldType *>(ty)->fieldSize;
  MIRIntConst *fieldvalue = static_cast<MIRIntConst *>(ct);

  // Truncate the size of FieldValue to the bit field size.
  if (fieldsize < fieldvalue->GetBitWidth()) {
    fieldvalue->Trunc(fieldsize);
  }
  semitinfo->combinebfldvalue_ =
      ((static_cast<uint64>(fieldvalue->value)) << semitinfo->combinebfldwidth_) + semitinfo->combinebfldvalue_;
  semitinfo->combinebfldwidth_ += fieldsize;
  semitinfo->nextfieldOffset_ += fieldsize;
  if (!nty || (nty && kTypeBitField != nty->typeKind)) {
    // emit semitinfo->combinebfldvalue_
    EmitCombineBfldValue(semitinfo);
  }
}

void Emitter::EmitStr(const std::string& mplStr, bool emitAscii, bool emitNewline) {
  const char *str = mplStr.c_str();
  size_t len = mplStr.size();

  if (emitAscii) {
    Emit("\t.ascii\t\"");  // Do not terminate with \0
  } else {
    Emit("\t.string\t\"");
  }

  // don't expand special character in a writeout to .s,
  // convert all \s to \\s in string for storing in .string
  for (int i = 0; i < len; i++) {
    // Referred to GNU AS: 3.6.1.1 Strings
    char buf[5];
    if (isprint(*str)) {
      buf[0] = *str;
      buf[1] = 0;
      if (*str == '\\' || *str == '\"') {
        buf[0] = '\\';
        buf[1] = *str;
        buf[2] = 0;
      }
      Emit(buf);
    } else if (*str == '\b') {
      Emit("\\b");
    } else if (*str == '\n') {
      Emit("\\n");
    } else if (*str == '\r') {
      Emit("\\r");
    } else if (*str == '\t') {
      Emit("\\t");
    } else if (*str == '\0') {
      buf[0] = '\\';
      buf[1] = '0';
      buf[2] = 0;
      Emit(buf);
    } else {
      // all others, print as number
      int ret = snprintf_s(buf, sizeof(buf), 4, "\\%03o", (*str) & 0xFF);
      if (ret < 0) {
        FATAL(kLncFatal, "snprintf_s failed");
      }
      buf[4] = '\0';
      Emit(buf);
    }
    str++;
  }

  Emit("\"");
  if (emitNewline) Emit("\n");
}

void Emitter::EmitStrConstant(MIRStrConst *ct, bool isAscii, bool isIndirect) {
  if (isIndirect) {
    uint32 strId = ct->value.GetIdx();
    if (stringPtr[strId] == 0) {
      stringPtr[strId] = ct->value;
    }
    Emit("\t.dword\t").Emit(".LSTR__").Emit(std::to_string(strId).c_str());
    return;
  }

  const string ustr = GlobalTables::GetUStrTable().GetStringFromStrIdx(ct->value);
  size_t len = ustr.size();
  if (isAscii) {
    if (isFlexibleArray) {
      arraySize += len;
    }
  } else {
    if (isFlexibleArray) {
      arraySize += len + 1;
    }
  }
  EmitStr(ustr, isAscii, false);
}

void Emitter::EmitStr16Constant(MIRStr16Const *ct) {
  Emit("\t.byte ");
  // note: for now, u16string is emitted 2 bytes without any \u indication
  const std::u16string &str16 = GlobalTables::GetU16StrTable().GetStringFromStrIdx(ct->value);
  char buf[9];
  char16_t c = str16[0];
  int ret1 = snprintf_s(buf, sizeof(buf), 8, "%d,%d", (c >> 8) & 0xFF, c & 0xFF);
  if (ret1 < 0) {
    FATAL(kLncFatal, "snprintf_s failed");
  }
  buf[8] = '\0';
  Emit(buf);
  for (uint32 i = 1; i < str16.length(); i++) {
    c = str16[i];
    int ret2 = snprintf_s(buf, sizeof(buf), 8, ",%d,%d", (c >> 8) & 0xFF, c & 0xFF);
    if (ret2 < 0) {
      FATAL(kLncFatal, "snprintf_s failed");
    }
    buf[8] = '\0';
    Emit(buf);
  }
  if ((str16.length() & 0x1) == 1) {
    Emit(",0,0");
  }
}

uint32 Emitter::EmitPadForNextField(MIRConst *ct, uint32 byteUsed, uint32 align) {
  uint32 dataSize;
  if (ct->kind == kConstAggConst) {
    MIRType *ty = ct->type;
    MIRStructType *sty = static_cast<MIRStructType *>(ty);
    dataSize = g->becommon->type_size_table.at(sty->tyIdx.GetIdx());
  } else if (dynamic_cast<MIRIntConst *>(ct)) {
    dataSize = GetPrimTypeBitSize(ct->type->primType) >> 3;
  } else if (dynamic_cast<MIRFloatConst *>(ct)) {
    dataSize = 4;
  } else  {
    dataSize = 8;
  }

  if (byteUsed == 0) {
    return dataSize;
  }

  uint32 pad;
  if ((dataSize + byteUsed) > align) {
    pad = align - byteUsed;
    if (pad) {
      Emit("\t.zero\t");
      Emit(std::to_string(pad).c_str());
      Emit("\n");
    }
    return (dataSize + pad);
  }
  pad = (dataSize - 1) & byteUsed;
  if (pad) {
    Emit("\t.zero\t");
    Emit(std::to_string(pad).c_str());
    Emit("\n");
  }

  return (dataSize + pad);
}

void Emitter::EmitAggConst(MIRConst *ct, bool newline, bool flag32) {
  MIRType *ty = ct->type;
  MIRStructType *sty;
  if (ty->GetKind() == kTypeArray) {
    EmitArrayConstant(ct);
    return;
  } else {
    sty = static_cast<MIRStructType *>(ty);
  }

  uint32 ssize = g->becommon->type_size_table.at(sty->tyIdx.GetIdx());
  uint32 salign = g->becommon->type_align_table.at(sty->tyIdx.GetIdx());

  MIRAggConst *aggCt = dynamic_cast<MIRAggConst *>(ct);
  uint32 alignUsed = 0; // #bytes used up to alignment
  uint32 bitSize = 0;   // max number of bits for bitfield as defined in source
  uint32 bitsUsed = 0;  // Cumulative bits of previous bitfields.
  uint64 bitData = 0;
  uint32 fieldCount = 0;
  FieldVector fields = sty->fields;
  bool terminate = false;
  bool hasData = false;
  MapleVector<MIRConst *>::iterator it;
  MIRConst *ctIt;
  ctIt = *(aggCt->constVec.begin());
  if (ctIt->fieldID != 1) {
    Emit("\t.skip\t").Emit(g->becommon->type_size_table[fields[0].second.first.GetIdx()]).Emit("\n");
    fieldCount = 1;
  }
  for (it = aggCt->constVec.begin(); it != aggCt->constVec.end(); ) {
    ctIt = *it;
    TyIdx fieldtyidx = fields[fieldCount].second.first;
    MIRType *fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);
    uint32 oldFieldSize = 0;
    while (fieldty->typeKind == kTypeBitField) {
      if ((fieldCount + 1) != ctIt->fieldID) {
        // A bitfield is skipped, and it is 0 length
        TyIdx skippedFieldIdx = fields[fieldCount].second.first;
        MIRType *skippedFieldTy = GlobalTables::GetTypeTable().GetTypeFromTyIdx(skippedFieldIdx);
        MIRBitfieldType *skippedBitTy = static_cast<MIRBitfieldType *>(skippedFieldTy);
        uint32 skippedBitSize = skippedBitTy->fieldSize;
        CHECK_FATAL(skippedFieldTy->typeKind == kTypeBitField, "Skipped init field should be bitfield");
        CHECK_FATAL(skippedBitSize == 0, "Skipped bitfield size not 0");

        uint32 numBytes = (bitsUsed >> 3) + ((bitsUsed & 0x7) > 0);
        for (int byte = 0; byte < numBytes; ++byte) {
          uint64 dt;
          Emit("\t.byte\t");
          dt = (bitData >> (byte * BITS_PER_BYTE)) & 0x0ffLL;
          Emit(std::to_string(dt).c_str());
          Emit("\n");
        }
        uint32 maxSize = g->becommon->type_size_table[skippedFieldIdx.GetIdx()];
        uint32 numZero = maxSize - ((bitsUsed >> 3) + ((bitsUsed & 0x7) > 0));
        if (numZero > 0) {
          Emit("\t.zero\t");
          Emit(std::to_string(numZero).c_str());
          Emit("\n");
          alignUsed = (alignUsed + numZero) & (salign - 1);
        }

        fieldCount++;
        fieldtyidx = fields[fieldCount].second.first;
        fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);
        bitData = 0;
        bitsUsed = 0;
        alignUsed = 0;
      }

      uint32 fieldSize = g->becommon->type_size_table[fieldtyidx.GetIdx()];
      if (fieldSize < oldFieldSize) {
        fieldSize = oldFieldSize;
      } else {
        oldFieldSize = fieldSize;
      }
      MIRIntConst *intCt;
      MIRBitfieldType *bitty = static_cast<MIRBitfieldType *>(fieldty);
      bitSize = bitty->fieldSize;
      intCt = dynamic_cast<MIRIntConst *>(ctIt);
      uint32 potentialBytes = ((bitsUsed + bitSize) >> 3) + (((bitsUsed + bitSize) & 0x7) > 0);
      if ((bitSize + bitsUsed) <= (salign * BITS_PER_BYTE) &&
          potentialBytes <= fieldSize) {
        bitData |= (intCt->value << bitsUsed);
        bitsUsed += bitSize;
        hasData = true;
      } else {
        uint32 numBytes = (bitsUsed >> 3) + ((bitsUsed & 0x7) > 0);
        for (int byte = 0; byte < numBytes; ++byte) {
          uint64 dt;
          Emit("\t.byte\t");
          dt = (bitData >> (byte * BITS_PER_BYTE)) & 0x0ffLL;
          Emit(std::to_string(dt).c_str());
          Emit("\n");
        }
        if (potentialBytes > fieldSize) {
          // The base type of bitfield might not be identical to source.
          // However it must not change the intended layout.
          int32 numZero = fieldSize - numBytes;
          if (numZero > 0) {
            Emit("\t.zero\t");
            Emit(std::to_string(numZero).c_str());
            Emit("\n");
            alignUsed = (alignUsed + numZero) & (salign - 1);
          }
        }

        bitData = intCt->value;
        hasData = true;
        bitsUsed = bitSize;
        alignUsed = (alignUsed + numBytes) & (salign - 1);
      }

      fieldCount++;
      it++;
      if (it == aggCt->constVec.end()) {
        terminate = true;
        break;
      }
      ctIt = *it;
      fieldtyidx = fields[fieldCount].second.first;
      fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);
    }
    if (hasData) {
      uint32 numBytes = (bitsUsed >> 3) + ((bitsUsed & 0x7) > 0);
      for (int byte = 0; byte < numBytes; ++byte) {
        uint64 dt;
        Emit("\t.byte\t");
        dt = (bitData >> (byte * BITS_PER_BYTE)) & 0x0ffLL;
        Emit(std::to_string(dt).c_str());
        Emit("\n");
      }
      bitData = 0;
      hasData = false;
      alignUsed = (alignUsed + numBytes) & (salign - 1);
    }
    if (terminate) {
      break;
    }
    alignUsed = (alignUsed + EmitPadForNextField(ctIt, alignUsed, salign)) & (salign - 1);
    EmitScalarConstant(ctIt, newline, flag32, true);

    fieldCount++;
    it++;
  }

  if (alignUsed && alignUsed < salign) {
    Emit("\t.zero\t");
    Emit(std::to_string(salign - alignUsed).c_str());
    Emit("\n");
  }
}

void Emitter::EmitScalarConstant(MIRConst *ct, bool newline = true, bool flag32 = false, bool isIndirect = false) {
  // Handle aggregate const separately, as each constant is in a vector.
  if (ct->kind == kConstAggConst) {
    return EmitAggConst(ct, newline, flag32);
  }
  if (MIRIntConst *intCt = dynamic_cast<MIRIntConst *>(ct)) {
    MIRType *ty = ct->type;
    Asmlabel asmname = GetTypeAsmInfoName(ty->primType);
    uint32 sizeinbits = GetPrimTypeBitSize(ty->primType);
    if (intCt->GetBitWidth() > sizeinbits) {
      intCt->Trunc(sizeinbits);
    }
    if (flag32) {
      EmitAsmLabel(Asmlabel::kAsmLong);
    } else {
      EmitAsmLabel(asmname);
    }
    Emit(std::to_string(intCt->value).c_str());
    if (isFlexibleArray) {
      arraySize += (sizeinbits / BITS_PER_BYTE);
    }
  } else if (MIRFloatConst *floatCt = dynamic_cast<MIRFloatConst *>(ct)) {
    MIRType *ty = ct->type;
    Asmlabel asmname = GetTypeAsmInfoName(ty->primType);
    EmitAsmLabel(asmname);
    Emit(std::to_string(floatCt->GetIntValue()).c_str());
    if (isFlexibleArray) {
      arraySize += 4;
    }
  } else if (MIRDoubleConst *doubleCt = dynamic_cast<MIRDoubleConst *>(ct)) {
    MIRType *ty = ct->type;
    Asmlabel asmname = GetTypeAsmInfoName(ty->primType);
    EmitAsmLabel(asmname);
    Emit(std::to_string(doubleCt->GetIntValue()).c_str());
    if (isFlexibleArray) {
      arraySize += 8;
    }
  } else if (MIRStrConst *strCt = dynamic_cast<MIRStrConst *>(ct)) {
    if (CLANG) {
      EmitStrConstant(strCt, false, isIndirect);
    } else {
      EmitStrConstant(strCt);
    }
  } else if (MIRStr16Const *str16Ct = dynamic_cast<MIRStr16Const *>(ct)) {
    EmitStr16Constant(str16Ct);
  } else if (MIRAddrofConst *symaddr = dynamic_cast<MIRAddrofConst *>(ct)) {
    StIdx stidx = symaddr->GetSymbolIndex();
    bool isGlobal = stidx.IsGlobal();
    MIRSymbol *symaddrSym = isGlobal ? GlobalTables::GetGsymTable().GetSymbolFromStIdx(stidx.Idx())
                                : CG::curCgFunc->mirModule.CurFunction()->symTab->GetSymbolFromStIdx(stidx.Idx());
    if (isGlobal == false && symaddrSym->storageClass == kScPstatic) {
      Emit("\t.quad\t" + symaddrSym->GetName() + to_string(CG::curPuIdx));
    } else {
      Emit("\t.quad\t" + symaddrSym->GetName());
    }
    if (symaddr->GetOffset() != 0) {
      Emit(" + ").Emit(symaddr->GetOffset());
    }
  } else if (MIRAddroffuncConst *funcaddr = dynamic_cast<MIRAddroffuncConst *>(ct)) {
    Emit("\t.quad\t");
    Emit(GlobalTables::GetFunctionTable().GetFunctionFromPuidx(funcaddr->GetValue())->GetName());
  } else if (MIRLblConst *lbl = dynamic_cast<MIRLblConst *>(ct)) {
    Emit("\t.dword\t");
    MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(CG::curCgFunc->mirModule.CurFunction()->stIdx.Idx());
    EmitLabelRef(funcSt->GetName().c_str(), lbl->value);
  } else {
    CG_ASSERT(false, "NYI");
  }
  if (newline) {
    Emit("\n");
  }
}

static int32 GetPrimitiveTypeSize(std::string &name) {
  if (name.length() != 1) {
    return -1;
  }
  char tname = name[0];

  switch (tname) {
    case 'Z':
      return (static_cast<int32>(GetPrimTypeSize(PTY_u1)));
    case 'B':
      return (static_cast<int32>(GetPrimTypeSize(PTY_i8)));
    case 'S':
      return (static_cast<int32>(GetPrimTypeSize(PTY_i16)));
    case 'C':
      return (static_cast<int32>(GetPrimTypeSize(PTY_u16)));
    case 'I':
      return (static_cast<int32>(GetPrimTypeSize(PTY_i32)));
    case 'J':
      return (static_cast<int32>(GetPrimTypeSize(PTY_i64)));
    case 'F':
      return (static_cast<int32>(GetPrimTypeSize(PTY_f32)));
    case 'D':
      return (static_cast<int32>(GetPrimTypeSize(PTY_f64)));
    case 'V':
      return (static_cast<int32>(GetPrimTypeSize(PTY_void)));
    default:
      return -1;
  }
}

void Emitter::EmitConstantTable(MIRSymbol *st, MIRConst *ct, const std::map<GStrIdx, MIRType *> &stridx2type) {
  const char *tblname = st->GetName().c_str();
  MIRAggConst *aggconst = static_cast<MIRAggConst *>(ct);

  bool fieldTypeIsName = false;
  if (st->IsReflectionFieldInfo() && (kTypeStruct == aggconst->type->GetKind())) {
    // the shadow's value is reused to indicated whether
    // the field.type is the field type name (= 0), other than __classinfo (=1)
    fieldTypeIsName = (static_cast<MIRIntConst *>(aggconst->constVec[FIELD::kShadow]))->value == 0;
    if (fieldTypeIsName == false) {  // it's the classinfo, reset the shadow for emit
      (static_cast<MIRIntConst *>(aggconst->constVec[FIELD::kShadow]))->value = 0;
    }
  }

  uint32 itabConflictIndex = 0;
  CHECK_FATAL(aggconst != nullptr, "null ptr check");
  for (uint32 i = 0; i < aggconst->constVec.size(); i++) {
    MIRConst *elemcst = aggconst->constVec[i];
    if (st->GetName().find(ITAB_CONFLICT_PREFIX_STR) == 0 && i == 0) {
      itabConflictIndex = (static_cast<MIRIntConst *>(elemcst))->value;
    }

    if (IsPrimitiveScalar(elemcst->type->primType)) {
      MIRAddroffuncConst *funcaddr = dynamic_cast<MIRAddroffuncConst *>(elemcst);
      MIRAddrofConst *symaddr = dynamic_cast<MIRAddrofConst *>(elemcst);

      if (funcaddr) {  // addroffunc const
        MIRFunction *func = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(funcaddr->GetValue());
        const std::string &funcName = func->GetName();
        bool isMethodinfoCompact =
            (GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(std::string(NameMangler::kMethodsInfoCompactPrefixStr) + func->GetBaseClassName())) != nullptr);
        bool isLocalFunction = (func->body != nullptr);
        if (st->IsMuidFuncInfTab() && (i == FUNC_DEF_NAME_INDEX)) {
          Emit("\t.long\t.label.name.");
          Emit(funcName + " - .");
          if (isMethodinfoCompact) {
            Emit(" + 1");
          }
          Emit("\n");
          continue;
        }
        if (st->IsMuidFuncInfTab() && (i == FUNC_DEF_SIZE_INDEX)) {
          Emit("\t.long\t.label.end.");
          Emit(funcName + " - ");
          Emit(funcName + "\n");
          continue;
        }
        if (st->IsReflectionMethodInfoCompact() && i == static_cast<uint32>(METHOD_COMPACT::kAddr)) {
          Emit("\t.long\t");
          Emit(funcName + " - .\n");
          continue;
        }

        if (st->IsMuidFuncDefTab() && i == FUNC_DEF_ADDR_INDEX) {
#ifdef USE_32BIT_REF
          Emit("\t.long\t");
#else
          Emit("\t.quad\t");
#endif  // USE_32BIT_REF
          Emit(funcName + "\n");
          continue;
        }

        Emit("\t.quad\t");
        Emit(funcName);

        if (st->GetName().find(VTAB_PREFIX_STR) == 0 || st->GetName().find(ITAB_PREFIX_STR) == 0 ||
            st->GetName().find(ITAB_CONFLICT_PREFIX_STR) == 0 ||
            (st->IsReflectionMethodInfoRO() && i == static_cast<uint32>(METHOD_RO::kAddr) && isLocalFunction)) {
          Emit(" - .\n");
          continue;
        }
        if (cg_->cgopt_.GeneratePositionIndependentExecutable()) {
          Emit(" - ");
          Emit(tblname);
        }
        Emit("\n");
      } else if (symaddr) {  // addrof symbol const
        StIdx stidx = symaddr->GetSymbolIndex();
        bool isGlobal = stidx.IsGlobal();
        MIRSymbol *symaddrSym = isGlobal ? GlobalTables::GetGsymTable().GetSymbolFromStIdx(stidx.Idx())
                                    : CG::curCgFunc->mirModule.CurFunction()->symTab->GetSymbolFromStIdx(stidx.Idx());
        const std::string &symaddrName = symaddrSym->GetName();

        if ((st->IsReflectionMethodInfoCompact() && i == static_cast<uint32>(METHOD_COMPACT::kDeclarclass)) ||
            (st->IsReflectionFieldInfoCompact() && i == static_cast<uint32>(FIELD_COMPACT::kOffset))) {
          Emit("\t.long\t");
          Emit(symaddrName + " - .\n");
          continue;
        }

        if ((st->IsReflectionMethodInfoRO() && (i == static_cast<uint32>(METHOD_RO::kDeclarclass))) ||
            (st->IsReflectionFieldInfoRO() && (i == static_cast<uint32>(FIELD_RO::kDeclarclass)))) {
#if USE_32BIT_REF
          Emit("\t.long\t");
#else
          Emit("\t.quad\t");
#endif  // USE_32BIT_REF

          Emit(symaddrName + " - .\n");
          continue;
        }

        if ((st->IsMuidDataUndefTab() || st->IsMuidDataDefTab()) && i == DATA_DEF_ADDR_INDEX) {
          if (symaddrSym->IsReflectionClassInfo()) {
            Emit("DW.ref." + symaddrName + ":\n");
          }
          Emit(std::string(NameMangler::kPtrPrefixStr) + symaddrName + ":\n");
#ifdef USE_32BIT_REF
          Emit("\t.long\t");
#else
          Emit("\t.quad\t");
#endif  // USE_32BIT_REF
          if (st->IsMuidDataUndefTab()) {
            Emit("0\n");
          } else {
            Emit(symaddrName + "\n");
          }
          continue;
        }

#ifdef TARGARK
        if (symaddrName.find(GCTIB_PREFIX_STR) == 0) {
          std::string patternName = cg_->FindGCTIBPatternName(symaddrName);
          std::string specialName(NameMangler::kJavaLangObjectStr);
          if (patternName.compare(GCTIB_PREFIX_STR + specialName) == 0) {
            Emit("\t.long\t"+cg_->FindGCTIBPatternName(symaddrName) + " - .\n");
            Emit("\t.long\t0xffffffff\n");
            continue;
          }
        }
#endif

#ifdef USE_32BIT_REF
        if (st->IsReflectionHashTabBucket() || st->GetName().find(ITAB_PREFIX_STR) == 0) {
          Emit("\t.long\t");
        } else {
          Emit("\t.quad\t");
        }
#else
        Emit("\t.quad\t");
#endif  // USE_32BIT_REF

        if (st->GetName().find(ITAB_CONFLICT_PREFIX_STR) == 0 || st->GetName().find(ITAB_PREFIX_STR) == 0) {
          Emit(symaddrName + " - .\n");
          continue;
        }
        if (st->IsMuidRangeTab()) {
          if (i == RANGE_BEGIN_INDEX) {
            Emit(symaddrSym->GetMuidTabName() + "_begin\n");
          } else {
            Emit(symaddrSym->GetMuidTabName() + "_end\n");
          }
          continue;
        }

        if (symaddrName.find(GCTIB_PREFIX_STR) == 0) {
          Emit(cg_->FindGCTIBPatternName(symaddrName));
        } else {
          Emit(symaddrName);
        }

        if ((st->IsReflectionClassInfoRO() && (i == static_cast<uint32>(CLASS_RO::kIfields) ||
                                               i == static_cast<uint32>(CLASS_RO::kMethods))) ||
            (st->IsReflectionClassInfo() && (i == static_cast<uint32>(CLASS::kGctib))) ||
            (st->IsReflectionFieldInfoRO() && (i == static_cast<uint32>(FIELD_RO::kOffset))) ||
            (st->IsReflectionHashTabBucket())) {
          Emit(" - .");
          if (symaddrSym->IsReflectionMethodInfoCompact() || symaddrSym->IsReflectionFieldInfoCompact()) {
            // Mark the least significant bit as 1 for compact methodinfo/fieldinfo
            Emit(" + 1");
          }
        }
        if (cg_->cgopt_.GeneratePositionIndependentExecutable()) {
          Emit(" - ");
          Emit(tblname);
        }
        Emit("\n");
      } else {  // intconst
        MIRIntConst *intCt = static_cast<MIRIntConst *>(elemcst);
        CG_ASSERT(intCt, "Uexpected const type");

        if (CGOptions::maplelinker && st->IsReflectionMethodInfo() && (i == static_cast<uint32>(METHOD::kShadow))) {
          if (intCt->value) {
            Emit(".label.name." + GlobalTables::GetFunctionTable().GetFunctionFromPuidx(static_cast<PUIdx>(intCt->value))->GetName());
            Emit(":\n");
            intCt->value = 0;  // reset it to 0
          }
        }

        if (st->IsReflectionMethodInfoCompact() && i == static_cast<uint32>(METHOD_COMPACT::kMod)) {
          MIRAddroffuncConst *funcaddr = dynamic_cast<MIRAddroffuncConst *>(aggconst->constVec[METHOD_COMPACT::kAddr]);
          if (funcaddr) {
            Emit(".label.name." + GlobalTables::GetFunctionTable().GetFunctionFromPuidx(funcaddr->GetValue())->GetName());
            Emit(":\n");
          }
        }
        if ((st->IsReflectionClassInfoRO() &&
             (i == static_cast<uint32>(CLASS_RO::kClassname) ||
              i == static_cast<uint32>(CLASS_RO::kAnnotation))) ||
            (st->IsReflectionMethodInfoRO() &&
             (i == static_cast<uint32>(METHOD_RO::kMethodname) ||
              i == static_cast<uint32>(METHOD_RO::kSigname) ||
              i == static_cast<uint32>(METHOD_RO::kAnnotationvalue))) ||
            (st->IsReflectionMethodInfoCompact() &&
             (i == static_cast<uint32>(METHOD_COMPACT::kMethodname) ||
              i == static_cast<uint32>(METHOD_COMPACT::kSigname) ||
              i == static_cast<uint32>(METHOD_COMPACT::kAnnotation))) ||
            (st->IsReflectionFieldInfo() && ((i == static_cast<uint32>(FIELD::kType) && fieldTypeIsName))) ||
            (st->IsReflectionFieldInfoCompact() &&
             (i == static_cast<uint32>(FIELD_COMPACT::kTypeName) ||
              i == static_cast<uint32>(FIELD_COMPACT::kName) ||
              i == static_cast<uint32>(FIELD_COMPACT::kAnnotation))) ||
            (st->IsReflectionFieldInfoRO() &&
             (i == static_cast<uint32>(FIELD_RO::kName) ||
              i == static_cast<uint32>(FIELD_RO::kAnnotation))) ||
            (st->IsRegJNITab()) ||  // RegisterTable has been Int Array, visit element instead of field.
            (st->GetName().find(std::string(NameMangler::kVtabOffsetTabStr)) == 0 && ((i == 1) || (i == 2))) ||
            (st->GetName().find(std::string(NameMangler::kFieldOffsetTabStr)) == 0 && ((i == 1))) ||
            (st->GetName().find(ITAB_CONFLICT_PREFIX_STR) == 0 && (i >= (itabConflictIndex * 2 + 2) && (i % 2 == 0)))) {
          uint32 index = (static_cast<MIRIntConst *>(elemcst))->value & 0xFFFFFFFF;
          bool isHotReflectStr = (index & 0xC0000000) != 0;
          std::string hotStr;
          if (isHotReflectStr) {
            uint32 tag = index >> 30;
            if (tag == HOT_LAYOUT::kStartUpHot) {
              hotStr = NameMangler::kReflectionStartHotStrtabPrefixStr;
            } else if (tag == HOT_LAYOUT::kBothHot) {
              hotStr = NameMangler::kReflectionBothHotStrTabPrefixStr;
            } else {
              hotStr = NameMangler::kReflectionRunHotStrtabPrefixStr;
            }
          }
          std::string reflectStrTabPrefix = isHotReflectStr ? hotStr : NameMangler::kReflectionStrtabPrefixStr;
          std::string strtabName = reflectStrTabPrefix + cg_->mirModule->GetFileNameAsPostfix();
          (static_cast<MIRIntConst *>(elemcst))->value = index & 0x3FFFFFFF;
          // field-type has dual-mode, need to do a special encoding to let runtime knows
          if (fieldTypeIsName && (i == static_cast<uint32>(FIELD::kType))) {
            // split the 64b value into 32b taboffset and 32b offset
            // taboffset
            Emit("\t.long\t").Emit(strtabName).Emit("+1-.\n");
            // offset inside table
            EmitScalarConstant(elemcst, false, true);  // emit it as .long instead of .quad
            Emit("\n");
            continue;
          }

#ifdef USE_32BIT_REF
          if (st->GetName().find(ITAB_CONFLICT_PREFIX_STR) == 0) {
            EmitScalarConstant(elemcst, false, true);
          } else {
            EmitScalarConstant(elemcst, false);
          }
#else
          EmitScalarConstant(elemcst, false);
#endif  // USE_32BIT_REF
          Emit("+").Emit(strtabName);
          if (st->IsRegJNITab()) {
            Emit("-.");
          }
          if (st->IsReflectionMethodInfo() || st->IsReflectionMethodInfoRO() || st->IsReflectionMethodInfoCompact() ||
              st->IsReflectionFieldInfo() || st->IsReflectionFieldInfoRO() || st->IsReflectionFieldInfoCompact()) {
            Emit("-.");
          }
          if (st->GetName().find(ITAB_PREFIX_STR) == 0 || st->GetName().find(ITAB_CONFLICT_PREFIX_STR) == 0) {
            Emit("-.");
          }
          if (st->GetName().find(NameMangler::kVtabOffsetTabStr) == 0 && ((i == 1) || (i == 2))) {
            Emit("-.");
          }
          if (st->GetName().find(NameMangler::kFieldOffsetTabStr) == 0 && ((i == 1))) {
            Emit("-.");
          }
          Emit("\n");
        } else if (st->IsRegJNIFuncTab()) {
          std::string strtabName = NameMangler::kRegJNITabPrefixStr + cg_->mirModule->GetFileNameAsPostfix();
          EmitScalarConstant(elemcst, false);
          Emit("+").Emit(strtabName);
          Emit("\n");
        } else if ((st->IsReflectionFieldInfoRO() && i == static_cast<uint32>(FIELD_RO::kOffset)) ||
                   (st->IsReflectionFieldInfoCompact() && i == static_cast<uint32>(FIELD_COMPACT::kOffset))) {
          // Figure out field offset now.
          std::string stName = st->GetName();
          std::string delimiter = "$$";
          std::string typeName;
          std::string widthFlag;
          if (st->IsReflectionFieldInfoRO()) {
            typeName =
                stName.substr(strlen(FIELDINFO_RO_PREFIX_STR), stName.find(delimiter) - strlen(NameMangler::kFieldsInfoPrefixStr));
            widthFlag = ".quad";
          } else {
            typeName = stName.substr(strlen(NameMangler::kFieldsInfoCompactPrefixStr),
                                     stName.find(delimiter) - strlen(NameMangler::kFieldsInfoCompactPrefixStr));
            widthFlag = ".long";
          }
          CG_ASSERT(typeName[0] != '$', "array does not have fields");
          uint8 charbitwidth = GetPrimTypeSize(PTY_i8) * 8;
          GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(typeName);
          auto it = stridx2type.find(strIdx);
          CG_ASSERT(it != stridx2type.end(), "Can not find type");
          MIRType *ty = it->second;

          MIRStructType *sty = static_cast<MIRStructType *>(ty);
          int64 fieldidx = intCt->value;
          uint32 fieldoffset = g->becommon->GetFieldOffset(sty, fieldidx).first * charbitwidth +
                               g->becommon->GetFieldOffset(sty, fieldidx).second;

          Emit("\t//  ").Emit(typeName).Emit("\t field").Emit(std::to_string(fieldidx).c_str()).Emit("\n");

          Emit("\t").Emit(widthFlag).Emit("\t").Emit(std::to_string(fieldoffset).c_str());
          Emit("\n");
        } else if ((st->IsReflectionClassInfo() && i == static_cast<uint32>(CLASS::kObjsize))) {
          std::string stName = st->GetName();
          std::string delimiter = "$$";
          std::string typeName =
              stName.substr(strlen(CLASSINFO_PREFIX_STR), stName.find(delimiter) - strlen(CLASSINFO_PREFIX_STR));
          uint32 objsize = 0;
          std::string comments;

          if (typeName[0] == '$') {
            // fill element size for array class;
            std::string newTypeName = typeName.substr(1);
            // another $(arraysplitter)
            CHECK_FATAL(newTypeName.find("$") != std::string::npos, "can not find $ in string");
            typeName = newTypeName.substr(newTypeName.find("$") + 1);
            int32 ptypesize;

            // we only need to calculate primitive type in arrays.
            if ((ptypesize = GetPrimitiveTypeSize(typeName)) != -1) {
              objsize = static_cast<uint32>(ptypesize);
            }
            comments = "// elemobjsize";
          } else {
            comments = "// objsize";
          }

          // cerr << type_name << endl;
          if (!objsize) {
            GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(typeName);
            auto it = stridx2type.find(strIdx);
            CG_ASSERT(it != stridx2type.end(), "Can not find type");
            MIRType *ty = it->second;

            objsize = g->becommon->type_size_table[ty->tyIdx.GetIdx()];
          }
          CG_ASSERT((objsize <= 0xffff), "Error:the objsize is too large");
          Emit("\t.short\t").Emit(std::to_string(objsize).c_str()).Emit(comments);
          Emit("\n");
        } else if (st->IsReflectionFieldInfo() && i == static_cast<uint32>(FIELD::kInfoRo)) {
          MIRIntConst *intCt = static_cast<MIRIntConst *>(elemcst);
          int fieldIdx = intCt->value;
          // emit field info ro ptr
#ifdef USE_32BIT_REF
          Emit("\t.long ");
#else
          Emit("\t.quad ");
#endif  // USE_32BIT_REF
          std::string stName = st->GetName();
          std::string className =
              stName.substr(strlen(NameMangler::kFieldsInfoPrefixStr), stName.length() - strlen(NameMangler::kFieldsInfoPrefixStr));
          MIRSymbol *fieldROSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(
              GlobalTables::GetStrTable().GetStrIdxFromName(std::string(FIELDINFO_RO_PREFIX_STR) + className));
          ;
          Emit(fieldROSt->GetName().c_str());
          Emit("+");
          Emit(std::to_string(fieldIdx * (sizeof(FieldMetadataRO))).c_str());
          Emit(" - .\n");
        } else if (st->IsReflectionMethodInfo() && i == METHOD::kInfoR0) {
          MIRIntConst *intCt = dynamic_cast<MIRIntConst *>(elemcst);
          CHECK_FATAL(intCt != nullptr, "null ptr check");
          int methodIdx = intCt->value;
          // emit method info ro ptr
#ifdef USE_32BIT_REF
          Emit("\t.long ");
#else
          Emit("\t.quad ");
#endif  // USE_32BIT_REF
          std::string stName = st->GetName();
          std::string className =
              stName.substr(strlen(NameMangler::kMethodsInfoPrefixStr), stName.length() - strlen(NameMangler::kMethodsInfoPrefixStr));
          MIRSymbol *methodROSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(
              GlobalTables::GetStrTable().GetStrIdxFromName(std::string(METHODINFO_RO_PREFIX_STR) + className));
          ;
          Emit(methodROSt->GetName().c_str());
          Emit("+");
          Emit(std::to_string(methodIdx * (sizeof(MethodMetadataRO))).c_str());
          Emit(" -.");
          Emit("\n");
        } else if (st->IsMuidRangeTab()) {
          MIRIntConst *intCt = static_cast<MIRIntConst *>(elemcst);
          int flag = intCt->value;
          std::string prefix;
          switch (flag) {
            case RangeIdx::kVtab:
              prefix = NameMangler::kMuidVtabPrefixStr;
              break;
            case RangeIdx::kItab:
              prefix = NameMangler::kMuidItabPrefixStr;
              break;
            case RangeIdx::kVtabOffset:
              prefix = NameMangler::kMuidVtabOffsetPrefixStr;
              break;
            case RangeIdx::kFieldOffset:
              prefix = NameMangler::kMuidFieldOffsetPrefixStr;
              break;
            case RangeIdx::kValueOffset:
              prefix = NameMangler::kMuidValueOffsetPrefixStr;
              break;
            case RangeIdx::kLocalClassInfo:
              prefix = NameMangler::kMuidLocalClassInfoStr;
              break;
            case RangeIdx::kConststr:
              prefix = NameMangler::kMuidConststrPrefixStr;
              break;
            case RangeIdx::kSuperclass:
              prefix = NameMangler::kMuidSuperclassPrefixStr;
              break;
            case RangeIdx::kFieldInfo:
              prefix = MUID_FIELDINFO_PREFIX_STR;
              break;
            case RangeIdx::kGlobalRootlist:
              prefix = NameMangler::kMuidGlobalRootlistPrefixStr;
              break;
            case RangeIdx::kClassmetaData:
              prefix = NameMangler::kMuidClassMetadataPrefixStr;
              break;
            case RangeIdx::kClassBucket:
              prefix = NameMangler::kMuidClassMetadataBucketPrefixStr;
              break;
            case RangeIdx::kJavatext:
              prefix = NameMangler::kMuidJavatextPrefixStr;
              break;
            case RangeIdx::kJavajni:
              prefix = NameMangler::kRegJNITabPrefixStr;
              break;
            case RangeIdx::kJavajniFunc:
              prefix = NameMangler::kRegJNIFuncTabPrefixStr;
              break;
            default:
              // This could MD5 code or 0
              EmitScalarConstant(elemcst, false);
              Emit("\n");
              continue;
          }
          Emit("\t.quad\t");
          if (i == RANGE_BEGIN_INDEX) {
            Emit(prefix + "_begin\n");
          } else {
            Emit(prefix + "_end\n");
          }
        } else {
#ifdef USE_32BIT_REF
          if (st->GetName().find(ITAB_CONFLICT_PREFIX_STR) == 0 || st->GetName().find(ITAB_PREFIX_STR) == 0 ||
              st->GetName().find(VTAB_PREFIX_STR) == 0) {
            EmitScalarConstant(elemcst, false, true);
          } else {
            EmitScalarConstant(elemcst, false);
          }
#else
          EmitScalarConstant(elemcst, false);
#endif  // USE_32BIT_REF
          Emit("\n");
        }
      }
    } else if (kTypeArray == elemcst->type->GetKind() || kTypeStruct == elemcst->type->GetKind()) {
      EmitConstantTable(st, elemcst, stridx2type);
    }
  }
}

void Emitter::EmitArrayConstant(MIRConst *ct) {
  MIRType *ty = ct->type;
  if (ct->kind == kConstStrConst) {
    // This is to distinguish between
    //   char *v = "...."
    // and
    //   char v[] = "...."
    // Here it is an array of char disguised as string.
    MIRStrConst *strCt = static_cast<MIRStrConst *>(ct);
    EmitStrConstant(strCt, true);
    Emit("\n");
    return;
  }
  MIRAggConst *arrCt = static_cast<MIRAggConst *>(ct);
  MIRArrayType *aty = static_cast<MIRArrayType *>(ty);
  CG_ASSERT(aty, "");
  int64 inum = arrCt->constVec.size();
  uint32 dim = aty->sizeArray[0];
  TyIdx scalarIdx = aty->eTyIdx;
  if (inum == 0 && dim) {
    MIRType *subTy = GlobalTables::GetTypeTable().GetTypeFromTyIdx(scalarIdx);
    while ( subTy->typeKind == kTypeArray) {
      MIRArrayType *aSubTy = static_cast<MIRArrayType *>(subTy);
      if (aSubTy->sizeArray[0] > 0) {
        dim *= (aSubTy->sizeArray[0]);
      }
      scalarIdx = aSubTy->eTyIdx;
      subTy = GlobalTables::GetTypeTable().GetTypeFromTyIdx(scalarIdx);
    }
  }
  int64 unum = dim > 0 ? (static_cast<int64>(dim)) - inum : 0;
  for (uint32 i = 0; i < inum; i++) {
    MIRConst *elemcst = arrCt->constVec[i];
    if (IsPrimitiveScalar(elemcst->type->primType)) {
      if (CLANG) {
        bool strLiteral = false;
        if (aty->dim == 1) {
          MIRType *ety = GlobalTables::GetTypeTable().GetTypeFromTyIdx(aty->eTyIdx);
          if (ety->primType == PTY_i8 || ety->primType == PTY_u8) {
            strLiteral = true;
          }
        }
        EmitScalarConstant(elemcst, true, false, strLiteral == false);
      } else {
        EmitScalarConstant(elemcst);
      }
    } else if (kTypeArray == elemcst->type->GetKind()) {
      EmitArrayConstant(elemcst);
    } else if (kTypeStruct == elemcst->type->GetKind() || kTypeClass == elemcst->type->GetKind() ||
               kTypeUnion == elemcst->type->GetKind()) {
      EmitStructConstant(elemcst);
    } else {
      CG_ASSERT(false, "");
    }
  }
  if (unum > 0) {
    if (inum > 0) {
      uint64 uninsizeinbyte = unum * (g->becommon->type_size_table.at(arrCt->constVec[0]->type->tyIdx.GetIdx()));
      if (uninsizeinbyte) {
        EmitNullConstant(uninsizeinbyte);
      }
    } else {
      uint32 ssize = g->becommon->type_size_table.at(scalarIdx.GetIdx()) * dim;
      Emit("\t.zero\t").Emit(ssize).Emit("\n");
    }
  }
  Emit("\n");
}

void Emitter::EmitStructConstant(MIRConst *ct) {
  StructEmitInfo *semitinfo = cg_->mirModule->memPool->New<StructEmitInfo>();
  MIRType *ty = ct->type;
  MIRAggConst *structCt = static_cast<MIRAggConst *>(ct);
  MIRStructType *sty = static_cast<MIRStructType *>(ty);
  // all elements of struct.
  uint8 anum;
  if (sty->typeKind == kTypeUnion) {
    anum = 1;
  } else {
    anum = sty->fields.size();
  }
  // total size of emitted elements size.
  uint32 ssize = g->becommon->type_size_table.at(sty->tyIdx.GetIdx());
  uint32 fieldidx = 1;
  if (sty->typeKind == kTypeUnion) {
    fieldidx = structCt->constVec[0]->fieldID;
  }
  for (uint32 i = 0; i < anum; i++) {
    if (((i + 1) == anum) && CLANG) {
      isFlexibleArray = g->becommon->type_has_flexible_array[ty->tyIdx.GetIdx()];
      arraySize = 0;
    }
    MIRConst *elemcst = structCt->GetAggConstElement(fieldidx);
    MIRType *ety = sty->GetElemType(i);
    if (sty->typeKind == kTypeUnion) {
      ety = elemcst->type;
    }
    MIRType *nety = nullptr;
    if (i != uint32(anum - 1)) {
      nety = sty->GetElemType(i + 1);
    }
    uint32 esize = g->becommon->type_size_table[ety->tyIdx.GetIdx()];
    uint8 charbitwidth = GetPrimTypeSize(PTY_i8) * 8;
    if (kTypeBitField == ety->GetKind()) {
      if (!elemcst) {
        MIRIntConst *zerofill = cg_->mirModule->memPool->New<MIRIntConst>(0, ety);
        zerofill->fieldID = fieldidx;
        elemcst = zerofill;
      }
      uint32 fieldoffset = g->becommon->GetFieldOffset(sty, fieldidx).first * charbitwidth +
                           g->becommon->GetFieldOffset(sty, fieldidx).second;
      EmitBitFieldConstant(semitinfo, elemcst, nety, fieldoffset);
    } else {
      if (elemcst) {
        if (IsPrimitiveScalar(ety->primType)) {
          EmitScalarConstant(elemcst, true, false, true);
        } else if (kTypeArray == ety->GetKind()) {
          if (ety->GetSize() != 0) {
            EmitArrayConstant(elemcst);
          }
        } else if (kTypeStruct == ety->GetKind() || kTypeUnion == ety->GetKind()) {
          EmitStructConstant(elemcst);
        } else if (kTypeClass == ety->GetKind()) {
          EmitStructConstant(elemcst);
        } else {
          CG_ASSERT(false, "");
        }
      } else {
        EmitNullConstant(esize);
      }
      semitinfo->tsize_ += esize;
      semitinfo->nextfieldOffset_ = semitinfo->tsize_ * charbitwidth;
    }

    if (nety && kTypeBitField != nety->typeKind) {
      uint32 nalign = 1;
      CG_ASSERT(i < uint32(anum - 1), "NYI");
      nalign = g->becommon->type_align_table[nety->tyIdx.GetIdx()];
      // append size, append 0 when align need.
      uint32 psize = 0;
      psize = (0 == semitinfo->tsize_ % nalign) ? 0 : (nalign - (semitinfo->tsize_ % nalign));
      if (psize) {
        EmitNullConstant(psize);
        semitinfo->tsize_ += psize;
        semitinfo->nextfieldOffset_ = semitinfo->tsize_ * charbitwidth;
      }
      // element is uninitialized, emit null constant.
    }

    fieldidx++;
  }

  uint32 opsize = ssize - semitinfo->tsize_;
  if (opsize) {
    EmitNullConstant(opsize);
  }
}

// BlockMarker is for Debugging/Profiling
void Emitter::EmitBlockMarker(const char *markerName) {
  /*
     .type $marker_name$, %object
     .global $marker_name$
     .data
     .align 3
     $marker_name$:
     .quad 0xdeadbeefdeadbeef
     .size $marker_name$, 8
   */
  Emit(asminfo_->asm_type).Emit(markerName).Emit(", %object\n");
  if (CGOptions::emitBlockMarker) {  // exposed as global symbol, for profiling
    Emit(asminfo_->asm_global).Emit(markerName).Emit("\n");
  } else {  // exposed as local symbol, for release.
    Emit(asminfo_->asm_local).Emit(markerName).Emit("\n");
  }
  // emit(asminfo_->asm_hidden).emit(marker_name).emit("\n");
  EmitAsmLabel(kAsmData);
  Emit(asminfo_->asm_align).Emit("  3\n");
  Emit(markerName).Emit(":\n");
  Emit("\t.quad\t0xdeadbeefdeadbeef\n");
  Emit(asminfo_->asm_size).Emit(markerName).Emit(", 8\n");
}

// BlockMarker is for Debugging/Profiling
void Emitter::EmitBlockMarkerWithAddr(const char *markerName, const std::string &addrName) {
  /*
     .type $marker_name$, %object
     .global $marker_name$
     .data
     .align 3
     $marker_name$:
     .quad 0xdeadbeefdeadbeef
     .size $marker_name$, 8
   */
  Emit(asminfo_->asm_type).Emit(markerName).Emit(", %object\n");
  if (CGOptions::emitBlockMarker) {  // exposed as global symbol, for profiling
    Emit(asminfo_->asm_global).Emit(markerName).Emit("\n");
  } else {  // exposed as local symbol, for release.
    Emit(asminfo_->asm_local).Emit(markerName).Emit("\n");
  }
  // emit(asminfo_->asm_hidden).emit(marker_name).emit("\n");
  EmitAsmLabel(kAsmData);
  Emit(asminfo_->asm_align).Emit("  3\n");
  Emit(markerName).Emit(":\n");
  Emit("\t.quad ");
  Emit(addrName);
  Emit("\n");
  Emit(asminfo_->asm_size).Emit(markerName).Emit(", 8\n");
}

void Emitter::EmitLiteral(MIRSymbol *literal, const std::map<GStrIdx, MIRType *> &stridx2type) {
  /*
     .type _C_STR_xxxx, %object
     .local _C_STR_xxxx
     .data
     .align 3
     _C_STR_xxxx:
     .quad __cinf_Ljava_2Flang_2FString_3B
     ....
     .size _C_STR_xxxx, 40
   */
  if (literal->storageClass == kScUnused) {
    return;
  }
  EmitAsmLabel(literal, kAsmType);
  // literal should always be fstatic and readonly?
  EmitAsmLabel(literal, kAsmLocal);  // alwasy fstatic
  EmitAsmLabel(kAsmData);
#if TARGARK
  Emit("\t.p2align 3\n");
#else
  EmitAsmLabel(literal, kAsmAlign);
#endif
  EmitAsmLabel(literal, kAsmSyname);
  // literal is an array
  MIRConst *ct = literal->GetConst();
  if (literal->HasAddrOfValues()) {
    EmitConstantTable(literal, ct, stridx2type);
  } else {
    EmitArrayConstant(ct);
  }

  EmitAsmLabel(literal, kAsmSize);
}

void Emitter::EmitFuncLayoutInfo(const MIRSymbol *layout) {
  /*
     .type $marker_name$, %object
     .global $marker_name$
     .data
     .align 3
     $marker_name$:
     .quad funcaddr
     .size $marker_name$, 8
   */
  MIRConst *ct = layout->GetConst();
  MIRAggConst *aggconst = static_cast<MIRAggConst *>(ct);

  if (aggconst->constVec.size() != static_cast<uint32_t>(LayoutType::kLayoutTypeCount)) {
    cerr << "something wrong happen in funclayoutsym\t"
         << "const_vec size\t" << aggconst->constVec.size() << endl;
    return;
  }
  for (uint32_t i = 0; i < static_cast<uint32_t>(LayoutType::kLayoutTypeCount); ++i) {
    std::string markerName = "__MBlock_" + GetLayoutTypeString(i) + "_func_start";
    MIRAddroffuncConst *funcaddr = dynamic_cast<MIRAddroffuncConst *>(aggconst->constVec[i]);
    CHECK_FATAL(funcaddr, "null ptr check");
    Emit(asminfo_->asm_type).Emit(markerName).Emit(", %object\n");
    Emit(asminfo_->asm_global).Emit(markerName).Emit("\n");
    EmitAsmLabel(kAsmData);
    Emit(asminfo_->asm_align).Emit("  3\n");
    Emit(markerName).Emit(":\n");
    Emit("\t.quad ");
    Emit(GlobalTables::GetFunctionTable().GetFunctionFromPuidx(funcaddr->GetValue())->GetName().c_str());
    Emit("\n");
    Emit(asminfo_->asm_size).Emit(markerName).Emit(", 8\n");
  }
}

void Emitter::EmitStaticFields(std::vector<MIRSymbol *> &fields) {
  for (auto iter = fields.begin(); iter != fields.end(); iter++) {
    EmitAsmLabel(*iter, kAsmType);
    // literal should always be fstatic and readonly?
    EmitAsmLabel(*iter, kAsmLocal);  // alwasy fstatic
    EmitAsmLabel(kAsmData);
    EmitAsmLabel(*iter, kAsmAlign);
    EmitAsmLabel(*iter, kAsmSyname);
    // literal is an array
    MIRConst *ct = (*iter)->GetConst();
    EmitArrayConstant(ct);
  }
}

void Emitter::EmitLiterals(std::vector<std::pair<MIRSymbol *, bool>> &literals,
                           const std::map<GStrIdx, MIRType *> &stridx2type) {
  if (JAVALANG == false) {
    return;
  }
  // load literals profile
  // currently only used here, so declare it as local
  std::unordered_set<std::string> hotLiterals;
  ifstream infile;
  infile.open(CGOptions::literalProFile);
  if (infile.fail()) {
    // no profile available, emit literals as usual
    cerr << "Cannot open literal profile file " << CGOptions::literalProFile << "\n";
    for (auto literalPair : literals) {
      EmitLiteral(literalPair.first, stridx2type);
    }
    return;
  }

  string literalName;
  while (infile >> literalName) {
    hotLiterals.insert(literalName);
  }
  infile.close();

  // emit hot literal start symbol
  EmitBlockMarker("__MBlock_literal_hot_begin");
  // emit literals into .data section
  // emit literals in the profile first
  for (auto iter = literals.begin(); iter != literals.end(); iter++) {
    if (hotLiterals.find(iter->first->GetName()) != hotLiterals.end()) {
      // it's in the literal profiling data, means it's "hot"
      EmitLiteral(iter->first, stridx2type);
      iter->second = true;
    }
  }
  // emit hot literal end symbol
  EmitBlockMarker("__MBlock_literal_hot_end");

  // emit cold literal start symbol
  EmitBlockMarker("__MBlock_literal_cold_begin");
  // emit other literals (not in the profile) next.
  for (auto literalPair : literals) {
    if (literalPair.second == false) {  // not emit yet
      EmitLiteral(literalPair.first, stridx2type);
    }
  }
  // emit cold literal end symbol
  EmitBlockMarker("__MBlock_literal_cold_end");
}

void Emitter::GetHotandColdMetaSymbolinfo(std::vector<MIRSymbol *> &symVIn, std::vector<MIRSymbol *> &hotStV,
                                          std::vector<MIRSymbol *> &coldStV, std::string prefixStr) {
  for (auto st : symVIn) {
    std::string name = st->GetName().substr(prefixStr.length());
    if (cg_->mirModule->profile.CheckClassHot(name)) {
      hotStV.push_back(st);
    } else {
      coldStV.push_back(st);
    }
  }
}

void Emitter::EmitMetaDataSymbolWithMarkFlag(std::vector<MIRSymbol *> &symV,
                                             const std::map<GStrIdx, MIRType *> &stridx2type,
                                             std::string prefixStr,
                                             bool isHotFlag) {
  std::string markString = "__MBlock" + prefixStr;
  std::string hotorcold = isHotFlag ? "hot" : "cold";
  EmitBlockMarker((markString + hotorcold + "_begin").c_str());
  std::string sectionName;
  for (auto s : symV) {
    EmitClassinfoSequential(s, stridx2type, sectionName);
  }
  EmitBlockMarker((markString + hotorcold + "_end").c_str());
}

void Emitter::EmitMetaDataSymbols(std::vector<MIRSymbol *> &symV, std::map<GStrIdx, MIRType *> &stridx2type,
                                  std::string prefixStr) {
  std::vector<MIRSymbol *> hotSymV;
  std::vector<MIRSymbol *> coldSymV;

  GetHotandColdMetaSymbolinfo(symV, hotSymV, coldSymV, prefixStr);

  EmitMetaDataSymbolWithMarkFlag(hotSymV, stridx2type, prefixStr, true);
  EmitMetaDataSymbolWithMarkFlag(coldSymV, stridx2type, prefixStr, false);
}

void Emitter::MarkVtabOrItabEndFlag(std::vector<MIRSymbol *> &symV, std::vector<MIRSymbol *> &hotSymV) {
  for (auto st : symV) {
    MIRConst *ct = st->GetConst();
    MIRAggConst *aggconst = static_cast<MIRAggConst *>(ct);
    size_t size = aggconst->constVec.size();
    MIRConst *elemcst = aggconst->constVec[size - 1];
    MIRAddroffuncConst *funcaddr = dynamic_cast<MIRAddroffuncConst *>(elemcst);
    if (funcaddr) {
      std::cerr << "ERROR: the last vtab/itab content should not be funcAddr" << std::endl;
    } else {
      MIRIntConst *tabConst = static_cast<MIRIntConst *>(elemcst);
#ifdef USE_32BIT_REF
      tabConst->value =
          (uint64_t)(tabConst->value) | 0X40000000;  //#define COLD VTAB ITAB END FLAG  0X4000000000000000
#else
      tabConst->value =
          (uint64_t)(tabConst->value) | 0X4000000000000000;  //#define COLD VTAB ITAB END FLAG  0X4000000000000000
#endif
    }
  }
}

void Emitter::EmitStringPointers() {
  Emit(asminfo_->asm_section).Emit(asminfo_->asm_data).Emit("\n");
  for (auto idx: stringPtr) {
    if (idx == 0) {
      continue;
    }
    uint32 strId = idx.GetIdx();
    const char *str = GlobalTables::GetUStrTable().GetStringFromStrIdx(idx).c_str();
    Emit("\t.align 3\n");
    Emit(".LSTR__").Emit(strId).Emit(":\n");
    std::string mplstr(str);
    EmitStr(mplstr, false, true);
  }
}

void Emitter::EmitGlobalVar(MIRSymbol *globalvar) {
  EmitAsmLabel(globalvar, kAsmType);
  EmitAsmLabel(globalvar, kAsmLocal);
  EmitAsmLabel(globalvar, kAsmComm);
}

void Emitter::EmitGlobalVars(std::vector<std::pair<MIRSymbol *, bool>> &globalvars) {
  // load globalvars profile
  if (globalvars.empty()) {
    return;
  }
  std::unordered_set<std::string> hotVars;
  ifstream infile;
  if (!CGOptions::globalVarProFile.empty()) {
    infile.open(CGOptions::globalVarProFile);
    if (infile.fail()) {
      cerr << "Cannot open globalVar profile file " << CGOptions::globalVarProFile << "\n";
    }
  }
  if (CGOptions::globalVarProFile.empty() || infile.fail()) {
    for (auto globalvarPair : globalvars) {
      EmitGlobalVar(globalvarPair.first);
    }
    return;
  }
  string globalVarName;
  while (infile >> globalVarName) {
    hotVars.insert(globalVarName);
  }
  infile.close();
  bool hotBeginSet = false;
  bool coldBeginSet = false;
  for (auto iter = globalvars.begin(); iter != globalvars.end(); iter++) {
    if (hotVars.find(iter->first->GetName()) != hotVars.end()) {
      if (!hotBeginSet) {
        // emit hot globalvar start symbol
        EmitBlockMarkerWithAddr("__MBlock_globalVars_hot_begin", iter->first->GetName());
        hotBeginSet = true;
      }
      EmitGlobalVar(iter->first);
      iter->second = true;
    }
  }
  for (auto globalvarPair : globalvars) {
    if (globalvarPair.second == false) {  // not emit yet
      if (!coldBeginSet) {
        // emit hot globalvar start symbol
        EmitBlockMarkerWithAddr("__MBlock_globalVars_cold_begin", globalvarPair.first->GetName());
        coldBeginSet = true;
      }
      EmitGlobalVar(globalvarPair.first);
    }
  }
  MIRSymbol *endSym = globalvars.back().first;
  MIRType *ty = endSym->GetType();
  const std::string kStaticVarEndAdd =
      std::to_string(g->becommon->type_size_table.at(ty->tyIdx.GetIdx())) + "+" + endSym->GetName();
  EmitBlockMarkerWithAddr("__MBlock_globalVars_cold_end", kStaticVarEndAdd);
}

void Emitter::EmitLocalVariable(CGFunc *cgfunc) {
  // function local pstatic initialization
  if (CLANG == false) {
    return;
  }
  MIRSymbolTable *lSymTab = cgfunc->mirModule.CurFunction()->symTab;
  if (lSymTab == nullptr) {
    return;
  }
  // anything larger than is created by cg
  size_t lsize = cgfunc->GetLsymSize();
  for (size_t i = 0; i < lsize; i++) {
    MIRSymbol *st = lSymTab->GetSymbolFromStIdx(i);
    if (st == nullptr) {
      continue;
    }
    if (st->storageClass != kScPstatic) {
      continue;
    }

    // Local static names can repeat.
    // Append the current program unit index to the name.
    string localname = st->GetName() + to_string(CG::curPuIdx);
    static vector<string> emittedLocalSym;
    bool found = false;
    for (auto name : emittedLocalSym) {
      if (name == localname) {
        found = true;
        break;
      }
    }
    if (found) {
      continue;
    }
    emittedLocalSym.push_back(localname);

    Emit(asminfo_->asm_section);
    Emit(asminfo_->asm_data);
    Emit("\n");
    EmitAsmLabel(st, kAsmAlign);
    MIRConst *ct = st->GetConst();
    if (ct == nullptr) {
      EmitAsmLabel(st, kAsmComm);
    } else {
      EmitAsmLabel(st, kAsmSyname);
      EmitScalarConstant(ct, true, false, true/*isIndirect*/);
    }
  }
}

void Emitter::EmitGlobalVariable() {
  std::vector<MIRSymbol *> typeStVec;
  std::vector<MIRSymbol *> typenameStVec;
  std::map<GStrIdx, MIRType *> stridx2type;

  // Create name2type map which will be used by reflection.
  for (MIRType *type : GlobalTables::GetTypeTable().typeTable) {
    if (!type || (type->typeKind != kTypeClass && type->typeKind != kTypeInterface)) {
      continue;
    }

    GStrIdx strIdx = type->nameStrIdx;
    stridx2type[strIdx] = type;
  }

  // sort symbols; classinfo-->field-->method
  size_t size = GlobalTables::GetGsymTable().GetSymbolTableSize();
  std::vector<MIRSymbol *> classinfoV;
  std::vector<MIRSymbol *> vtabVec;
  std::vector<MIRSymbol *> staticFieldsVec;
  std::vector<std::pair<MIRSymbol *, bool>> globalVarVec;
  std::vector<MIRSymbol *> itabVec;
  std::vector<MIRSymbol *> itabConflictV;
  std::vector<MIRSymbol *> vtabOffsetVec;
  std::vector<MIRSymbol *> fieldOffsetVec;
  std::vector<MIRSymbol *> valueOffsetVec;
  std::vector<MIRSymbol *> localClassinfoVec;
  std::vector<MIRSymbol *> constStrVec;
  std::vector<std::pair<MIRSymbol *, bool>> literalVec;
  std::vector<MIRSymbol *> muidVec = { nullptr };

  for (size_t i = 0; i < size; i++) {
    MIRSymbol *st = GlobalTables::GetGsymTable().GetSymbolFromStIdx(i);
    if (st == nullptr || st->IsDeleted() || st->storageClass == kScUnused) {
      continue;
    }
    if (st->GetName().find(VTAB_PREFIX_STR) == 0) {
      vtabVec.push_back(st);
      continue;
    } else if (st->GetName().find(ITAB_PREFIX_STR) == 0) {
      itabVec.push_back(st);
      continue;
    } else if (st->GetName().find(ITAB_CONFLICT_PREFIX_STR) == 0) {
      itabConflictV.push_back(st);
      continue;
    } else if (st->GetName().find(NameMangler::kVtabOffsetTabStr) == 0) {
      vtabOffsetVec.push_back(st);
      continue;
    } else if (st->GetName().find(NameMangler::kFieldOffsetTabStr) == 0) {
      fieldOffsetVec.push_back(st);
      continue;
    } else if (st->GetName().find(NameMangler::kOffsetTabStr) == 0) {
      valueOffsetVec.push_back(st);
      continue;
    } else if (st->GetName().find(NameMangler::kLocalClassInfoStr) == 0) {
      localClassinfoVec.push_back(st);
      continue;
    } else if (st->IsLiteral()) {
      literalVec.push_back(std::make_pair(st, false));
      continue;
    } else if (st->IsConstString() || st->IsLiteralPtr()) {
      MIRConst *ct = st->GetConst();
      if (ct && ct->kind == kConstAddrof) {
        constStrVec.push_back(st);
        continue;
      }
    } else if (st->IsReflectionClassInfoPtr()) {
      // _PTR__cinf is emitted in dataDefTab and dataUndefTab
      continue;
    } else if (st->IsMuidTab()) {
      if (JAVALANG) {
        muidVec[0] = st;
        EmitMuidTable(muidVec, stridx2type, st->GetMuidTabName());
      }
      continue;
    } else if (st->IsCodeLayoutInfo()) {
      if (JAVALANG) {
        EmitFuncLayoutInfo(st);
      }
      continue;
    } else if (st->GetName().find(NameMangler::kStaticFieldNamePrefixStr) == 0) {
      staticFieldsVec.push_back(st);
      continue;
    } else if (st->GetName().find(NameMangler::kGcRootList) == 0) {
      EmitGlobalRootList(st);
      continue;
    } else if (st->GetName().find(NameMangler::kFunctionProfileTabPrefixStr) == 0) {
      muidVec[0] = st;
      EmitMuidTable(muidVec, stridx2type, NameMangler::kFunctionProfileTabPrefixStr);
      continue;
    }

    if (st->IsReflectionInfo()) {
      if (st->IsReflectionClassInfo()) {
        classinfoV.push_back(st);
      }
      continue;
    }
    // symbols we do not emit here.
    if (kStFunc == st->sKind || kStJavaClass == st->sKind || kStJavaInterface == st->sKind) {
      continue;
    }
    if (kScTypeInfo == st->storageClass) {
      typeStVec.push_back(st);
      continue;
    }
    if (kScTypeInfoName == st->storageClass) {
      typenameStVec.push_back(st);
      continue;
    }
    if (kScTypeCxxAbi == st->storageClass) {
      continue;
    }

    MIRType *ty = st->GetType();
    if (!ty) {
      continue;
    }

    // emit uninitialized global/static variables.
    // these variables store in .comm section.
    if ((kScGlobal == st->storageClass || kScFstatic == st->storageClass) && !st->IsConst()) {
      if (st->IsGctibSym()) {
        // GCTIB symbols are generated in GenerateObjectMaps
        continue;
      }
      if (kScGlobal == st->storageClass) {
        EmitAsmLabel(st, kAsmType);
        EmitAsmLabel(st, kAsmComm);
      } else {
        globalVarVec.push_back(std::make_pair(st, false));
      }
      continue;
    }
    EmitAsmLabel(st, kAsmType);
    // emit initialized global/static variables.
    if (kScGlobal == st->storageClass || (kScFstatic == st->storageClass && !st->IsReadOnly())) {
      // Emit section
      if (st->IsReflectionStrtab()) {
        Emit("\t.section\t.reflection_strtab").Emit(",\"a\",%progbits\n");
      } else if (st->IsRegJNITab()) {
        Emit("\t.section\t.reg_jni_tab").Emit(",\"a\", %progbits\n");
      } else if (st->IsRegJNIFuncTab()) {
        Emit("\t.section\t.reg_jni_func_tab").Emit(",\"wa\", %progbits\n");
      } else if (st->IsReflectionPrimitiveClassInfo()) {
        Emit("\t.section\t.primitive_classinfo").Emit(",\"awG\", %progbits,__primitive_classinfo__,comdat\n");
      } else if (st->IsReflectionHashTabBucket()) {
        std::string stName = st->GetName();
        std::string delimiter = "$$";
        if (stName.find(delimiter) == std::string::npos) {
          FATAL(kLncFatal, "Can not find delimiter in target ");
        }
        std::string secName = stName.substr(0, stName.find(delimiter));
        // remove leading "__" in sec name.
        secName.erase(0, 2);
        Emit("\t.section\t.").Emit(secName).Emit(",\"a\",%progbits\n");
      } else {
        EmitAsmLabel(kAsmData);
      }
      // Emit size and align by type
      if (st->storageClass == kScGlobal) {
        if (st->IsReflectionPrimitiveClassInfo() || st->GetAttr(ATTR_weak)) {
          EmitAsmLabel(st, kAsmWeak);
        } else {
          EmitAsmLabel(st, kAsmGlbl);
        }
        EmitAsmLabel(st, kAsmHidden);
      } else if (st->storageClass == kScFstatic) {
        EmitAsmLabel(st, kAsmLocal);
      }
      if (st->IsReflectionStrtab()) {
        Emit("\t.p2align 3\n");  // reflection-string-tab also aligned to 8B boundaries.
      } else {
        EmitAsmLabel(st, kAsmAlign);
      }
      EmitAsmLabel(st, kAsmSyname);
      MIRConst *ct = st->GetConst();
      if (IsPrimitiveScalar(ty->primType)) {
        if (IsAddress(ty->primType)) {
          uint32 sizeinbits = GetPrimTypeBitSize(ct->type->primType);
          CHECK_FATAL(sizeinbits == 64, "EmitGlobalVariable: pointer must be of size 8");
        }
        if (CLANG) {
          EmitScalarConstant(ct, true, false, true);
        } else {
          EmitScalarConstant(ct);
        }
      } else if (kTypeArray == ty->GetKind()) {
        if (st->HasAddrOfValues()) {
          EmitConstantTable(st, ct, stridx2type);
        } else {
          EmitArrayConstant(ct);
        }
      } else if (kTypeStruct == ty->GetKind() || kTypeClass == ty->GetKind() || kTypeUnion == ty->GetKind()) {
        if (st->HasAddrOfValues()) {
          EmitConstantTable(st, ct, stridx2type);
        } else {
          EmitStructConstant(ct);
        }
      } else {
        CG_ASSERT(false, "NYI");
      }
      EmitAsmLabel(st, kAsmSize);
      // emit constant float/double
    } else if (st->IsReadOnly()) {
      MIRConst *ct = st->GetConst();
      // emit .section
      Emit(asminfo_->asm_section);
      Emit(asminfo_->asm_rodata);
      Emit("\n");
#if TARGARK
      if (ct->kind == kConstStrConst) {
        Emit("\t.p2align 3\n");
      } else {
        EmitAsmLabel(st, kAsmAlign);
      }
#else
      EmitAsmLabel(st, kAsmAlign);
#endif
      EmitAsmLabel(st, kAsmSyname);
      EmitScalarConstant(ct);
    } else if (st->storageClass == kScPstatic) {
      Emit(asminfo_->asm_section);
      Emit(asminfo_->asm_data);
      Emit("\n");
      EmitAsmLabel(st, kAsmAlign);
      MIRConst *ct = st->GetConst();
      if (ct == nullptr) {
        EmitAsmLabel(st, kAsmComm);
      } else {
        EmitAsmLabel(st, kAsmSyname);
        EmitScalarConstant(ct, true, false, true);
      }
    }
    if (CLANG && st->GetName().compare(0, 4, "_ZTI") == 0) {
      EmitDWRef(st->GetName().c_str());
    }
  }
  EmitStringPointers();
  // emit global var
  EmitGlobalVars(globalVarVec);
  // emit literal strings
  EmitLiterals(literalVec, stridx2type);
  // emit static field strings
  EmitStaticFields(staticFieldsVec);

  if (CLANG) {
    return;
  }

  EmitMuidTable(constStrVec, stridx2type, NameMangler::kMuidConststrPrefixStr);

  // emit classinfo, field, method
  std::vector<MIRSymbol *> superclassStV;
  std::vector<MIRSymbol *> fieldinfoStV;
  std::vector<MIRSymbol *> fieldinfoStRoV;
  std::vector<MIRSymbol *> fieldinfoStCompactV;
  std::vector<MIRSymbol *> methodinfoStV;
  std::vector<MIRSymbol *> methodinfoStRoV;
  std::vector<MIRSymbol *> methodinfoStCompactV;

  std::string sectionName = NameMangler::kMuidClassMetadataPrefixStr;
  Emit("\t.section  ." + sectionName).Emit(",\"aw\",%progbits\n");
  Emit(sectionName + "_begin:\n");

  for (int i = 0; i < classinfoV.size(); i++) {
    MIRSymbol *st = classinfoV[i];
    if (st && st->IsReflectionClassInfo() && st->GetConst()) {
      // Emit classinfo
      EmitClassinfoSequential(st, stridx2type, sectionName);
      std::string stName = st->GetName();
      std::string className =
          stName.substr(strlen(CLASSINFO_PREFIX_STR), stName.length() - strlen(CLASSINFO_PREFIX_STR));
      // Get classinfo ro symbol
      MIRSymbol *classinfoROSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(CLASSINFO_RO_PREFIX_STR + className));
      EmitClassinfoSequential(classinfoROSt, stridx2type, sectionName);
      // Get fields
      MIRSymbol *fieldSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kFieldsInfoPrefixStr + className));
      MIRSymbol *fieldStRO = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(FIELDINFO_RO_PREFIX_STR + className));
      MIRSymbol *fieldStCompact = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kFieldsInfoCompactPrefixStr + className));
      // Get methods
      MIRSymbol *methodSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kMethodsInfoPrefixStr + className));
      MIRSymbol *methodStRO = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(METHODINFO_RO_PREFIX_STR + className));
      MIRSymbol *methodStCompact = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kMethodsInfoCompactPrefixStr + className));
      // Get superclass
      MIRSymbol *superclassSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(SUPERCLASSINFO_PREFIX_STR + className));

      if (fieldSt) {
        fieldinfoStV.push_back(fieldSt);
        fieldinfoStRoV.push_back(fieldStRO);
      }
      if (fieldStCompact) {
        fieldinfoStCompactV.push_back(fieldStCompact);
      }
      if (methodSt) {
        methodinfoStV.push_back(methodSt);
        methodinfoStRoV.push_back(methodStRO);
      }
      if (methodStCompact) {
        methodinfoStCompactV.push_back(methodStCompact);
      }
      if (superclassSt) {
        superclassStV.push_back(superclassSt);
      }
    }
  }
  Emit(sectionName + "_end:\n");

  std::vector<MIRSymbol *> hotvtabStV, coldvtabStV;
  std::vector<MIRSymbol *> hotitabStV, colditabStV;
  std::vector<MIRSymbol *> hotitabCStV, colditabCStV;

  std::vector<MIRSymbol *> hotSuperCassStV, coldSuperClassStV;
  GetHotandColdMetaSymbolinfo(vtabVec, hotvtabStV, coldvtabStV, VTAB_PREFIX_STR);
  GetHotandColdMetaSymbolinfo(itabVec, hotitabStV, colditabStV, ITAB_PREFIX_STR);
  GetHotandColdMetaSymbolinfo(itabConflictV, hotitabCStV, colditabCStV, ITAB_CONFLICT_PREFIX_STR);
  GetHotandColdMetaSymbolinfo(superclassStV, hotSuperCassStV, coldSuperClassStV, SUPERCLASSINFO_PREFIX_STR);

  // RW part, hot
  // fieldinfo
  EmitMuidTable(fieldinfoStV, stridx2type, MUID_FIELDINFO_PREFIX_STR);
  // methodinfo
  EmitMetaDataSymbolWithMarkFlag(methodinfoStV, stridx2type, NameMangler::kMethodsInfoPrefixStr, true);

  // RO part, hot
  // fieldinfoRO
  EmitMetaDataSymbolWithMarkFlag(fieldinfoStRoV, stridx2type, FIELDINFO_RO_PREFIX_STR, true);
  // methodinfoRO
  EmitMetaDataSymbolWithMarkFlag(methodinfoStRoV, stridx2type, METHODINFO_RO_PREFIX_STR, true);

  // RO part, cold
  // Compact fieldinfo means cold
  EmitMetaDataSymbolWithMarkFlag(fieldinfoStCompactV, stridx2type, NameMangler::kFieldsInfoCompactPrefixStr, false);
  // Compact methodinfo means cold
  EmitMetaDataSymbolWithMarkFlag(methodinfoStCompactV, stridx2type, NameMangler::kMethodsInfoCompactPrefixStr, false);

  // itabConflict
  MarkVtabOrItabEndFlag(colditabCStV, hotitabCStV);
  EmitMuidTable(hotitabCStV, stridx2type, NameMangler::kMuidItabPrefixStr);
  EmitMetaDataSymbolWithMarkFlag(colditabCStV, stridx2type, ITAB_PREFIX_STR, false);

  // vtab
  // And itab to vtab section
  for (auto sym : hotitabStV) {
    hotvtabStV.push_back(sym);
  }
  for (auto sym : colditabStV) {
    coldvtabStV.push_back(sym);
  }
  MarkVtabOrItabEndFlag(coldvtabStV, hotvtabStV);
  EmitMuidTable(hotvtabStV, stridx2type, NameMangler::kMuidVtabPrefixStr);
  EmitMetaDataSymbolWithMarkFlag(coldvtabStV, stridx2type, VTAB_PREFIX_STR, false);

  // vtab_offset
  EmitMuidTable(vtabOffsetVec, stridx2type, NameMangler::kMuidVtabOffsetPrefixStr);
  // field_offset
  EmitMuidTable(fieldOffsetVec, stridx2type, NameMangler::kMuidFieldOffsetPrefixStr);
  // value_offset
  EmitMuidTable(valueOffsetVec, stridx2type, NameMangler::kMuidValueOffsetPrefixStr);
  // local clasinfo
  EmitMuidTable(localClassinfoVec, stridx2type, NameMangler::kMuidLocalClassInfoStr);
  // super class
  EmitMuidTable(hotSuperCassStV, stridx2type, NameMangler::kMuidSuperclassPrefixStr);
  EmitMetaDataSymbolWithMarkFlag(coldSuperClassStV, stridx2type, SUPERCLASSINFO_PREFIX_STR, false);

  if (typeStVec.size() > 0) {
    EmitTypeInfo(typeStVec, typenameStVec);
  }

  return;
}

void Emitter::EmitMplPersonalityV0() {
  // finally emit __gxx_personality_v0 DW.ref
  EmitDWRef("__mpl_personality_v0");
}

void Emitter::EmitGxxPersonalityV0() {
  EmitDWRef("__gxx_personality_v0");
}

void Emitter::EmitInitArraySection() {
  bool hasSectionHeader = false;
  for (PUIdx puIdx = 1; puIdx < GlobalTables::GetFunctionTable().funcTable.size(); puIdx++) {
    MIRFunction *func = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
    if (func->GetAttr(FUNCATTR_constructor)) {
      if (!hasSectionHeader) {
        Emit("\t.section .init_array").Emit(",\"aw\",%init_array\n");
        Emit("\t.align 3\n");
        hasSectionHeader = true;
      }
      Emit("\t.dword ").Emit(func->GetName().c_str()).Emit("\n");
    }
  }
}

void Emitter::EmitGlobalRootList(const MIRSymbol *gcrootsSt) {
  Emit("\t.section .maple.gcrootsmap").Emit(",\"aw\",%progbits\n");
  std::vector<std::string> nameVec;
  std::string name = gcrootsSt->GetName();
  nameVec.push_back(name);
  nameVec.push_back(name + "Size");
  bool gcrootsFlag = true;
  uint64_t vecSize = 0;
  for (auto gcrootsName : nameVec) {
    Emit("\t.type\t").Emit(gcrootsName).Emit(", @object\n");
    Emit("\t.section .maple.gcrootsmap\n");
    Emit("\t.p2align 3\n");
    Emit("\t.global\t").Emit(gcrootsName).Emit("\n");
    if (gcrootsFlag) {
      Emit(NameMangler::kMuidGlobalRootlistPrefixStr).Emit("_begin:\n");
    }
    Emit(gcrootsName).Emit(":\n");
    if (gcrootsFlag) {
      MIRAggConst *aggconst = static_cast<MIRAggConst *>(gcrootsSt->GetConst());
      uint64_t i = 0;
      while (i < aggconst->constVec.size()) {
        MIRConst *elemcst = aggconst->constVec[i];
        MIRAddrofConst *symaddr = dynamic_cast<MIRAddrofConst *>(elemcst);
        if (symaddr) {
          StIdx stidx = symaddr->GetSymbolIndex();
          bool isGlobal = stidx.IsGlobal();
          MIRSymbol *symaddrSym = isGlobal ? GlobalTables::GetGsymTable().GetSymbolFromStIdx(stidx.Idx())
                                      : CG::curCgFunc->mirModule.CurFunction()->symTab->GetSymbolFromStIdx(stidx.Idx());
          const std::string &symaddrName = symaddrSym->GetName();
          Emit("\t.quad\t").Emit(symaddrName).Emit("\n");
        } else {
          EmitScalarConstant(elemcst);
        }
        i++;
      }
      vecSize = i;
    } else {
      Emit("\t.quad\t").Emit(vecSize).Emit("\n");
    }
    Emit("\t.size\t").Emit(gcrootsName).Emit(",.-").Emit(gcrootsName).Emit("\n");
    if (gcrootsFlag) {
      Emit(NameMangler::kMuidGlobalRootlistPrefixStr).Emit("_end:\n");
    }
    gcrootsFlag = false;
  }
}

void Emitter::EmitMuidTable(const std::vector<MIRSymbol *> &ve, const std::map<GStrIdx, MIRType *> &stridx2type,
                            const std::string &sectionName) {
  Emit("\t.section  ." + sectionName).Emit(",\"aw\",%progbits\n");
  Emit(sectionName + "_begin:\n");
  for (int i = 0; i < ve.size(); i++) {
    MIRSymbol *st1 = ve[i];
    if (st1->storageClass == kScUnused) {
      continue;
    }
    EmitAsmLabel(st1, kAsmType);
    Emit("\t.section  ." + sectionName).Emit("\n");
    if (st1->storageClass == kScFstatic) {
      EmitAsmLabel(st1, kAsmLocal);
    } else {
      EmitAsmLabel(st1, kAsmGlbl);
      EmitAsmLabel(st1, kAsmHidden);
    }
    EmitAsmLabel(st1, kAsmAlign);
    EmitAsmLabel(st1, kAsmSyname);
    MIRConst *ct = st1->GetConst();
    if (ct->kind == kConstAddrof) {
      MIRAddrofConst *symaddr = dynamic_cast<MIRAddrofConst *>(ct);
      CHECK_FATAL(symaddr != nullptr, "call dynamic_cast failed in EmitMuidTable");
      StIdx stidx = symaddr->GetSymbolIndex();
      bool isGlobal = stidx.IsGlobal();
      MIRSymbol *symaddrSym = isGlobal ? GlobalTables::GetGsymTable().GetSymbolFromStIdx(stidx.Idx())
                                  : CG::curCgFunc->mirModule.CurFunction()->symTab->GetSymbolFromStIdx(stidx.Idx());
      Emit("\t.quad\t" + symaddrSym->GetName() + "\n");
    } else {
      EmitConstantTable(st1, ct, stridx2type);
    }
    EmitAsmLabel(st1, kAsmSize);
  }
  Emit(sectionName + "_end:\n");
}

void Emitter::EmitClassinfoSequential(MIRSymbol *st, const std::map<GStrIdx, MIRType *> &stridx2type,
                                      const std::string &sectionName) {
  EmitAsmLabel(st, kAsmType);
  if (!sectionName.empty()) {
    Emit("\t.section  ." + sectionName).Emit("\n");
  } else {
    EmitAsmLabel(kAsmData);
  }
  if (st->storageClass == kScFstatic) {
    EmitAsmLabel(st, kAsmLocal);
  } else {
    EmitAsmLabel(st, kAsmGlbl);
    EmitAsmLabel(st, kAsmHidden);
  }
  EmitAsmLabel(st, kAsmAlign);
  EmitAsmLabel(st, kAsmSyname);
  MIRConst *ct = st->GetConst();
  EmitConstantTable(st, ct, stridx2type);
  EmitAsmLabel(st, kAsmSize);
}

void Emitter::EmitClassinfoSequential(MIRSymbol *st1, MIRSymbol *st2,
                                      const std::map<GStrIdx, MIRType *> &stridx2type) {
  EmitAsmLabel(st1, kAsmType);
  EmitAsmLabel(kAsmData);
  if (st1->storageClass == kScFstatic) {
    EmitAsmLabel(st1, kAsmLocal);
  } else {
    EmitAsmLabel(st1, kAsmGlbl);
  }
  EmitAsmLabel(st1, kAsmAlign);
  EmitAsmLabel(st1, kAsmSyname);
  MIRConst *ct = st1->GetConst();
  EmitConstantTable(st1, ct, stridx2type);
  EmitConstantTable(st2, st2->GetConst(), stridx2type);
  MIRType *ty1 = st1->GetType();
  MIRType *ty2 = st2->GetType();
  Emit(asminfo_->asm_size);
  Emit(st1->GetName());
  Emit(", ");
  Emit(std::to_string(g->becommon->type_size_table[ty1->tyIdx.GetIdx()] + g->becommon->type_size_table[ty2->tyIdx.GetIdx()]));
  Emit("\n");
}

void Emitter::EmitDummyReflectionInfo(const char *name) {
  Emit("\t.section .reflection.dummy").Emit(",\"a\",%progbits\n");
  Emit("\t.type ").Emit(name).Emit(", \%object\n");
  Emit("\t.size ").Emit(name).Emit(",1\n");
  Emit("\t").Emit(name).Emit(":\n");
  Emit("\t.byte 0").Emit("\n");
}

void Emitter::EmitDWRef(const char *name) {
  /*
       .hidden DW.ref._ZTI3xxx
       .weak DW.ref._ZTI3xxx
       .section  .data.DW.ref._ZTI3xxx,"awG",@progbits,DW.ref._ZTI3xxx,comdat
       .align  3
       .type DW.ref._ZTI3xxx, %object
       .size DW.ref._ZTI3xxx, 8
     DW.ref._ZTI3xxx:
       .xword  _ZTI3xxx
   */
  Emit("\t.hidden DW.ref.").Emit(name).Emit("\n");
  Emit("\t.weak DW.ref.").Emit(name).Emit("\n");
  Emit("\t.section .data.DW.ref.").Emit(name).Emit(",\"awG\",%progbits,DW.ref.").Emit(name).Emit(",comdat\n");
  Emit("\t.p2align 3\n");
  Emit("\t.type ").Emit("DW.ref.").Emit(name).Emit(", \%object\n");
  Emit("\t.size ").Emit("DW.ref.").Emit(name).Emit(",8\n");
  Emit("DW.ref.").Emit(name).Emit(":\n");
#if TARGARK
  Emit("\t.quad ").Emit(name).Emit("\n");
#else
  Emit("\t.dword ").Emit(name).Emit("\n");
#endif
}

/*
   C++ excepton handling requires two sets of symbol
   definitions per each exception thrown.
   A class that inherits from another requires extra
   information to encode its inheritance relation
   in its type info symbol.

   Ex 1) no inheritance
   class nnn {...};
   1) Type string
       .weak   _ZTS3nnn
       .section  .rodata._ZTS3nnn,"aG",@progbits,_ZTS3nnn,comdat
       .type _ZTS3nnn, %object
       .size _ZTS3nnn, 5 ; length of the string
     _ZTS3nnn:
       .string "3nnn"

   2) Type info
       .weak   _ZTI3nnn
       .section  .rodata._ZTI3nnn,"aG",@progbits,_ZTI3nnn,comdat
       .align 8
       .type   _ZTI3nnn, %object
       .size   _ZTI3nnn, 16 ; sizeof(uintptr_t) * 2 entries
     _ZTI3nnn:
       .quad   _ZTVN10__cxxabiv117__class_type_infoE+16
       .quad   _ZTS3nnn  ; nnn's type string

   Ex 2) inheritance
   class eee : public std::exception {...};
   1) Type string
       .weak   _ZTS3eee
       .section  .rodata._ZTS3eee,"aG",@progbits,_ZTS3eee,comdat
       .type _ZTS3eee, %object
       .size _ZTS3eee, 5 ; length of the string
     _ZTS3eee:
       .string "3eee"

   2) Type info
       .weak   _ZTI3eee
       .section  .rodata._ZTI3eee,"aG",@progbits,_ZTI3eee,comdat
       .align 8
       .type   _ZTI3eee, %object
       .size   _ZTI3eee, 24 ; sizeof(uintptr_t) * 3 entries
     _ZTI3eee:
       .quad   _ZTVN10__cxxabiv120__si_class_type_infoE+16
       .quad   _ZTS3eee     ; eee's type string
       .quad   _ZTISt9exception
 */

void Emitter::EmitTypeInfo(const std::vector<MIRSymbol *> &typeStVec,
                           const std::vector<MIRSymbol *> &typenameStVec) const {
}

void Emitter::EmitAlignDirective(int pow2) {
  // Just use .p2align which is always power-of-two regardless of platforms
  Emit(".p2align ");
  EmitDecSigned(pow2);
  Emit("\n");
}

void Emitter::EmitLabel(const char *label) {
  Emit(label);
  Emit(":\n");
}

void Emitter::EmitDecSigned(int64 num) {
  ios::fmtflags f(out.flags());
  out << dec << num;
  out.flags(f);
}

void Emitter::EmitHexSigned(int64 num) {
  ios::fmtflags f(out.flags());
  out << "0x" << hex << num;
  out.flags(f);
}

void Emitter::EmitDecUnsigned(uint64 num) {
  ios::fmtflags f(out.flags());
  out << dec << num;
  out.flags(f);
}

void Emitter::EmitHexUnsigned(uint64 num) {
  ios::fmtflags f(out.flags());
  out << "0x" << hex << num;
  out.flags(f);
}

void Emitter::EmitGCHeader() {
  Emit("  .long 0 // GC header\n");
  Emit("  .long 10000 // RC\n");
}

void ImmOperand::dump() {
  LogInfo::MapleLogger() << "imm:" << val_;
}

void LabelOperand::Emit(Emitter &emitter, OpndProp *opndprop) {
  emitter.EmitLabelRef(parent_func_, labidx_);
}

void LabelOperand::dump() {
  LogInfo::MapleLogger() << "label:" << labidx_;
}

AnalysisResult *CgDoEmission::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  cgfunc->Emit();
  return nullptr;
}

}  // namespace maplebe
