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
#include "ark_mir_emit.h"
#include "ark_cg.h"
#include "cg_assert.h"
#include <ctype.h>
#include <stdio.h>
#include <string>
#include <algorithm>
#include "reflection_analysis.h"
#include <sys/stat.h>
#include "mpl_logging.h"
#include "securec.h"
#include "metadata_layout.h"
#include "special_func.h"

namespace maplebe {
using namespace maple;

std::string ArkCGFunc::GetReflectString(uint32_t offset) {
  std::stringstream ssfunc;
  MIRAggConst *stragg = nullptr;
  if ((offset & (3 << 30)) != 0) {
    uint32_t tag = offset >> 30;
    if (tag == HOT_LAYOUT::kStartUpHot) {
      stragg = static_cast<MIRAggConst *>(reflect_start_hot_strtab_sym->GetConst());
    } else if (tag == HOT_LAYOUT::kBothHot) {
      stragg = static_cast<MIRAggConst *>(reflect_both_hot_strtab_sym->GetConst());
    } else {
      stragg = static_cast<MIRAggConst *>(reflect_run_hot_strtab_sym->GetConst());
    }
    offset &= 0x3FFFFFFF;
  } else {
    stragg = static_cast<MIRAggConst *>(reflect_strtab_sym->GetConst());
  }

  for (auto starti = offset; starti < stragg->constVec.size(); starti++) {
    MIRIntConst *onechar = static_cast<MIRIntConst *>(stragg->constVec[starti]);
    CG_ASSERT(onechar != nullptr, "onechar is nullptr in ArkCGFunc::GetReflectString");
    char cc = static_cast<char>(onechar->value);
    if (!onechar->IsZero()) {
      ssfunc << cc;
    } else {
      break;
    }
  }
  return ssfunc.str();
}

static std::string &GetMethodDescLabel(const std::string &methodName, std::string &methodInfoLabel) {
  methodInfoLabel.clear();
  methodInfoLabel.append("__method_desc__");
  methodInfoLabel.append(methodName);
  return methodInfoLabel;
}

void ArkCGFunc::EmitRefToMethodDesc(Emitter &emitter) {
  if (!func->IsJava()) {
    return;
  }

  std::string methodDescLabel;
  GetMethodDescLabel(func->GetFuncSymbol()->GetName(), methodDescLabel);
  emitter.Emit("\t.long ").Emit(methodDescLabel).Emit("-.\n");
}

void ArkCGFunc::EmitRefToMethodInfo(Emitter &emitter) {
  if (func->module->IsJavaModule()) {
    std::string classname = func->GetBaseClassName();
    std::string methodinfostr = METHODINFO_RO_PREFIX_STR + classname;
    MIRSymbol *st = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(methodinfostr));
    bool methodinfoIsCompact = false;
    if (!st) {
      // methodinfo is in the cold format
      methodinfoIsCompact = true;
      methodinfostr = NameMangler::kMethodsInfoCompactPrefixStr + classname;
      st = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(methodinfostr));
    }

    if (st) {
      MIRAggConst *aggconst = static_cast<MIRAggConst *>(st->GetConst());
      CG_ASSERT(aggconst != nullptr, "aggconst is nullptr in ArkCGFunc::EmitRefToMethodInfo");
      bool found = false;
      uint32 offset = 0;
      for (uint32 i = 0; i < aggconst->constVec.size(); i++) {
        MIRConst *elemcst = aggconst->constVec[i];
        MIRAggConst *onemethodconst = static_cast<MIRAggConst *>(elemcst);
        CG_ASSERT(onemethodconst != nullptr, "onemethodconst is nullptr in ArkCGFunc::EmitRefToMethodInfo");
        MIRConst *funcnameindex = methodinfoIsCompact ? onemethodconst->constVec[METHOD_COMPACT::kMethodname]
                                                      : onemethodconst->constVec[METHOD_RO::kMethodname];
        MIRIntConst *intCt = static_cast<MIRIntConst *>(funcnameindex);
        CG_ASSERT(intCt != nullptr, "int_ct is nullptr in ArkCGFunc::EmitRefToMethodInfo");
        std::string enfuncname = NameMangler::EncodeName(GetReflectString((uint32_t)intCt->value).c_str());

        MIRConst *tttindex = methodinfoIsCompact ? onemethodconst->constVec[METHOD_COMPACT::kSigname]
                                                 : onemethodconst->constVec[METHOD_RO::kSigname];
        MIRIntConst *wwintCt = static_cast<MIRIntConst *>(tttindex);
        std::string wenfuncname = NameMangler::EncodeName(GetReflectString((uint32_t)wwintCt->value).c_str());

        if (func->GetBaseFuncName() == enfuncname && func->GetSignature() == wenfuncname) {
          found = true;
          offset = i;
          break;
        }
      }

      if (found) {
        if (methodinfoIsCompact) {
          // Mark this is a compact format
          emitter.Emit("\t.long ").Emit(methodinfostr);
          emitter.Emit("+1");
          offset *= sizeof(MethodMetadataCompact);
        } else {
          // here we still emit the pointer to MethodMetadata instead of MethodMetadataRO,
          // to let runtime have a consistent interface.
          emitter.Emit("\t.long ").Emit(NameMangler::kMethodsInfoPrefixStr + classname);
          offset *= sizeof(MethodMetadata);
        }
        if (offset > 0) {
          emitter.Emit("+").Emit(offset);
        }
        emitter.Emit("-.\n");
      } else {
        if (ehfunc && (ehfunc->NeedFullLSDA() || ehfunc->NeedFastLSDA())) {
          CG_ASSERT(false, "cant't find method metadata");
        }
      }
    }
  }
}

// emit java method description which contains address and size of local reference area
// as well as method metadata.
void ArkCGFunc::EmitMethodDesc(Emitter &emitter) {
  if (!func->IsJava()) {
    return;
  }

  emitter.Emit("\t.section\t.rodata\n");
  emitter.Emit("\t.p2align\t2\n");

  std::string methodInfoLabel;
  GetMethodDescLabel(func->GetFuncSymbol()->GetName(), methodInfoLabel);
  emitter.Emit(methodInfoLabel).Emit(":\n");

  EmitRefToMethodInfo(emitter);

  // local reference area
  ArkMemLayout *memLayout = static_cast<ArkMemLayout *>(this->memlayout);
  int32 refOffset = memLayout->GetReflocbaseLoc();
  uint32 refNum = memLayout->GetSizeOfRefLocals() / 8/*kIntregBytelen*/;

  emitter.Emit("\t.word ").Emit(refOffset).Emit("\n");
  emitter.Emit("\t.word ").Emit(refNum).Emit("\n");
}

void ArkCGFunc::Emit() {
  // emit header of this function
  Emitter &emitter = *cg->emitter_;

  emitter.Emit("\n");
  EmitMethodDesc(emitter);

  // emit java code to the java section.
  if (func->IsJava()) {
    std::string sectionName = NameMangler::kMuidJavatextPrefixStr;
    emitter.Emit("\t.section  ." + sectionName).Emit(",\"aw\"\n");
  } else {
    emitter.Emit("\t.text\n");
  }

  emitter.Emit("\t.p2align 2\n");
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());

  if (funcSt->GetFunction()->GetAttr(FUNCATTR_weak)) {
    emitter.Emit("\t.weak\t").Emit(funcSt->GetName()).Emit("\n");
    emitter.Emit("\t.hidden\t").Emit(funcSt->GetName()).Emit("\n");
  } else if (funcSt->GetFunction()->GetAttr(FUNCATTR_local)) {
    emitter.Emit("\t.local\t").Emit(funcSt->GetName()).Emit("\n");
  } else if (funcSt->value.mirFunc && funcSt->value.mirFunc->classTyIdx == 0 && funcSt->value.mirFunc->IsStatic()) {
    // nothing
  } else {
    emitter.Emit("\t.globl\t").Emit(funcSt->GetName()).Emit("\n");
//    emitter.Emit("\t.hidden\t").Emit(funcSt->GetName()).Emit("\n");
  }

  emitter.Emit("\t.type\t").Emit(funcSt->GetName()).Emit(", %function\n");

  // add these messege , solve the simpleperf tool error
  EmitRefToMethodDesc(emitter);

  emitter.Emit(funcSt->GetName()).Emit(":\n");

  emitter.Emit("\t// mir2bin func begin: ========================\n");
  emitter.Emit("\t.cfi_startproc\n");
//  emitter.Emit("\t.cfi_personality 155, DW.ref.__mpl_personality_v0\n");
  emitter.mirg_->EmitFunc(cg->curCgFunc->func);
  emitter.Emit("\t.cfi_endproc\n");
  emitter.Emit(".label.end."+funcSt->GetName()+":\n");
  emitter.Emit("\t// mir2bin func end: ========================\n");
  emitter.out << std::dec;
}

}  // namespace maplebe
