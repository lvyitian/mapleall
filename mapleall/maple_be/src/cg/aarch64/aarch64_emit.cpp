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
#include "aarch64_cg.h"
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

const char *AArch64CG::intRegNames[kRVLast][MAXREG] = {
  { "err",   "err0",  "err1",  "err2",  "err3",  "err4",  "err5",  "err6",  "err7",  "err8",  "err9",  "err10",
    "err11", "err12", "err13", "err14", "err15", "err16", "err17", "err18", "err19", "err20", "err21", "err22",
    "err23", "err24", "err25", "err26", "err27", "err28", "err",   "err",   "errsp", "errzr", "rflag", // x29 is fp
    "b0",    "b1",    "b2",    "b3",    "b4",    "b5",    "b6",    "b7",    "b8",    "b9",    "b10",   "b11",
    "b12",   "b13",   "b14",   "b15",   "b16",   "b17",   "b18",   "b19",   "b20",   "b21",   "b22",   "b23",
    "b24",   "b25",   "b26",   "b27",   "b28",   "b29",   "b30",   "b31" },
  { "err",   "err0",  "err1",  "err2",  "err3",  "err4",  "err5",  "err6",  "err7",  "err8",  "err9",  "err10",
    "err11", "err12", "err13", "err14", "err15", "err16", "err17", "err18", "err19", "err20", "err21", "err22",
    "err23", "err24", "err25", "err26", "err27", "err28", "err29", "err30", "errsp", "errzr", "rflag", // x29 is fp
    "h0",    "h1",    "h2",    "h3",    "h4",    "h5",    "h6",    "h7",    "h8",    "h9",    "h10",   "h11",
    "h12",   "h13",   "h14",   "h15",   "h16",   "h17",   "h18",   "h19",   "h20",   "h21",   "h22",   "h23",
    "h24",   "h25",   "h26",   "h27",   "h28",   "h29",   "h30",   "h31" },
  { "err", "w0",  "w1",  "w2",  "w3",  "w4",  "w5",  "w6",  "w7",  "w8",  "w9",  "w10", "w11", "w12", "w13", "w14",
    "w15", "w16", "w17", "w18", "w19", "w20", "w21", "w22", "w23", "w24", "w25", "w26", "w27", "w28", "err", "err",
    "wsp", "wzr", "rflag",  // x29 is fp
    "s0",  "s1",  "s2",  "s3",  "s4",  "s5",  "s6",  "s7",  "s8",  "s9",  "s10", "s11", "s12", "s13", "s14", "s15",
    "s16", "s17", "s18", "s19", "s20", "s21", "s22", "s23", "s24", "s25", "s26", "s27", "s28", "s29", "s30", "s31" },
  { "err", "x0",  "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",  "x8",  "x9",  "x10", "x11", "x12", "x13", "x14",
    "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30",
    "sp",  "xzr", "rflag",  // x29 is fp
    "d0",  "d1",  "d2",  "d3",  "d4",  "d5",  "d6",  "d7",  "d8",  "d9",  "d10", "d11", "d12", "d13", "d14", "d15",
    "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23", "d24", "d25", "d26", "d27", "d28", "d29", "d30", "d31" },
  { "err", "x0",  "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",  "x8",  "x9",  "x10", "x11", "x12", "x13", "x14",
    "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30",
    "sp",  "xzr", "rflag",  // x29 is fp
    "v0",  "v1",  "v2",  "v3",  "v4",  "v5",  "v6",  "v7",  "v8",  "v9",  "v10", "v11", "v12", "v13", "v14", "v15",
    "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31" },
  { "err", "x0",  "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",  "x8",  "x9",  "x10", "x11", "x12", "x13", "x14",
    "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30",
    "sp",  "xzr", "rflag",  // x29 is fp
    "q0",  "q1",  "q2",  "q3",  "q4",  "q5",  "q6",  "q7",  "q8",  "q9",  "q10", "q11", "q12", "q13", "q14", "q15",
    "q16", "q17", "q18", "q19", "q20", "q21", "q22", "q23", "q24", "q25", "q26", "q27", "q28", "q29", "q30", "q31" }
};

void AArch64CGFunc::EmitCfiDirectives(Insn *insn, bool inEpilog) {
}

bool AArch64CGFunc::IsInEpilogBB(BB *bb) {
  if (cg->GenerateCfiDirectives() && bb->lastinsn) {
    AArch64Insn *arminsn = static_cast<AArch64Insn *>(bb->lastinsn);
    if (arminsn->mop_ == MOP_xret) {
      return true;
    }
  }
  return false;
}

void AArch64Insn::EmitClinit(CG &cg, Emitter &emitter) {
  /* adrp    x3, __muid_data_undef_tab$$GetBoolean+144
   * ldr     x3, [x3, #:lo12:__muid_data_undef_tab$$GetBoolean+144]
   * or,
   * adrp    x3, _PTR__cinf_Ljava
   * ldr     x3, [x3, #:lo12:_PTR__cinf_Ljava]
   *
   * ldr x3, [x3,#112]
   * ldr wzr, [x3]
   */
  const AArch64MD *md = &AArch64CG::kMd[MOP_clinit];

  Operand *opnd0 = opnds[0];
  Operand *opnd1 = opnds[1];
  OpndProp *prop0 = md->operand_[0];
  StImmOperand *stopnd = static_cast<StImmOperand *>(opnd1);
  CHECK_FATAL(stopnd != nullptr, "stopnd is null in AArch64Insn::EmitClinit");
  // emit nop for breakpoint
  if (cg.cgopt_.WithDwarf()) {
    emitter.Emit("\t").Emit("nop").Emit("\n");
  }

  if (stopnd->GetSymbol()->IsMuidDataUndefTab()) {
    // emit adrp
    emitter.Emit("\t").Emit("adrp").Emit("\t");
    opnd0->Emit(emitter, prop0);
    emitter.Emit(",");
    emitter.Emit(stopnd->GetName());
    emitter.Emit("+").Emit(stopnd->GetOffset());
    emitter.Emit("\n");
    // emit ldr
    emitter.Emit("\t").Emit("ldr").Emit("\t");
    opnd0->Emit(emitter, prop0);
    emitter.Emit(",");
    emitter.Emit("[");
    opnd0->Emit(emitter, prop0);
    emitter.Emit(",");
    emitter.Emit("#");
    emitter.Emit(":lo12:").Emit(stopnd->GetName());
    emitter.Emit("+").Emit(stopnd->GetOffset());
    emitter.Emit("]");
    emitter.Emit("\n");
  } else {
    // adrp    x3, _PTR__cinf_Ljava_2Futil_2Fconcurrent_2Fatomic_2FAtomicInteger_3B
    emitter.Emit("\tadrp\t");
    opnd0->Emit(emitter, prop0);
    emitter.Emit(",");
    emitter.Emit(NameMangler::kPtrPrefixStr + stopnd->GetName());
    emitter.Emit("\n");

    // ldr     x3, [x3, #:lo12:_PTR__cinf_Ljava_2Futil_2Fconcurrent_2Fatomic_2FAtomicInteger_3B]
    emitter.Emit("\tldr\t");
    opnd0->Emit(emitter, prop0);
    emitter.Emit(", [");
    opnd0->Emit(emitter, prop0);
    emitter.Emit(", #:lo12:");
    emitter.Emit(NameMangler::kPtrPrefixStr + stopnd->GetName());
    emitter.Emit("]\n");
  }
  // emit "ldr  x0,[x0,#48]"
  emitter.Emit("\t").Emit("ldr").Emit("\t");
  opnd0->Emit(emitter, prop0);
  emitter.Emit(",");
  emitter.Emit("[");
  opnd0->Emit(emitter, prop0);
  emitter.Emit(",#");
  emitter.Emit(static_cast<uint32>(ClassMetadataOffsetOfInitFlag()));
  emitter.Emit("]");
  emitter.Emit("\n");

  // emit "ldr  xzr, [x0]"
  emitter.Emit("\t").Emit("ldr\txzr, [");
  opnd0->Emit(emitter, prop0);
  emitter.Emit("]\n");
}

void AArch64Insn::EmitAdrpLdr(CG &cg, Emitter &emitter) {
  // adrp    xd, _PTR__cinf_Ljava_2Futil_2Fconcurrent_2Fatomic_2FAtomicInteger_3B
  // ldr     xd, [xd, #:lo12:_PTR__cinf_Ljava_2Futil_2Fconcurrent_2Fatomic_2FAtomicInteger_3B]
  const AArch64MD *md = &AArch64CG::kMd[MOP_adrp_ldr];

  Operand *opnd0 = opnds[0];
  Operand *opnd1 = opnds[1];
  OpndProp *prop0 = md->operand_[0];
  StImmOperand *stopnd = static_cast<StImmOperand *>(opnd1);
  CHECK_FATAL(stopnd != nullptr, "stopnd is null in AArch64Insn::EmitAdrpLdr");
  // emit nop for breakpoint
  if (cg.cgopt_.WithDwarf()) {
    emitter.Emit("\t").Emit("nop").Emit("\n");
  }

  // adrp    xd, _PTR__cinf_Ljava_2Futil_2Fconcurrent_2Fatomic_2FAtomicInteger_3B
  emitter.Emit("\t").Emit("adrp").Emit("\t");
  opnd0->Emit(emitter, prop0);
  emitter.Emit(",");
  emitter.Emit(stopnd->GetName());
  // emitter.emit("+").emit(stopnd->GetOffset());
  emitter.Emit("\n");

  // ldr     xd, [xd, #:lo12:_PTR__cinf_Ljava_2Futil_2Fconcurrent_2Fatomic_2FAtomicInteger_3B]
  emitter.Emit("\tldr\t");
  static_cast<AArch64RegOperand *>(opnd0)->isreffield_ = true;
  opnd0->Emit(emitter, prop0);
  static_cast<AArch64RegOperand *>(opnd0)->isreffield_ = false;
  emitter.Emit(",");
  emitter.Emit("[");
  opnd0->Emit(emitter, prop0);
  emitter.Emit(",");
  emitter.Emit("#");
  emitter.Emit(":lo12:").Emit(stopnd->GetName());
  emitter.Emit("]\n");
}

void AArch64Insn::EmitClinitTail(CG &cg, Emitter &emitter) {
  // ldr x17, [xs, #112]
  // ldr wzr, [x17]
  const AArch64MD *md = &AArch64CG::kMd[MOP_clinit_tail];

  Operand *opnd0 = opnds[0];
  OpndProp *prop0 = md->operand_[0];

  // emit "ldr  x17,[xs,#112]"
  emitter.Emit("\t").Emit("ldr").Emit("\tx17, [");
  opnd0->Emit(emitter, prop0);
  emitter.Emit(", #");
  emitter.Emit(static_cast<uint32>(ClassMetadataOffsetOfInitFlag()));
  emitter.Emit("]");
  emitter.Emit("\n");

  // emit "ldr  xzr, [x17]"
  emitter.Emit("\t").Emit("ldr\txzr, [x17]\n");
}

void AArch64Insn::EmitAdrpLabel(CG &cg, Emitter &emitter) {
  // adrp    xd, label
  // add     xd, xd, #lo12:label
  const AArch64MD *md = &AArch64CG::kMd[MOP_adrp_label];

  Operand *opnd0 = opnds[0];
  Operand *opnd1 = opnds[1];
  OpndProp *prop0 = md->operand_[0];

  LabelIdx lidx = static_cast<ImmOperand *>(opnd1)->GetValue();

  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(CG::curCgFunc->mirModule.CurFunction()->stIdx.Idx());

  //GStrIdx strIdx = CG::curCgFunc->mirModule.CurFunction()->labelTab->labelTable[lidx];
  //string labelStr = GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);

  // adrp    xd, label
  emitter.Emit("\t").Emit("adrp").Emit("\t");
  opnd0->Emit(emitter, prop0);
  emitter.Emit(", ");
  const char *idx = std::to_string(CG::curPuIdx).c_str();
  emitter.Emit(".label.").Emit(idx).Emit("__").Emit(lidx).Emit("\n");

  // add     xd, xd, #lo12:label
  emitter.Emit("\tadd\t");
  opnd0->Emit(emitter, prop0);
  emitter.Emit(", ");
  opnd0->Emit(emitter, prop0);
  emitter.Emit(", ");
  emitter.Emit(":lo12:").Emit(".label.").Emit(idx).Emit("__").Emit(lidx).Emit("\n");
  emitter.Emit("\n");
}

void AArch64Insn::EmitCheckThrowPendingException(CG &cg, Emitter &emitter) {
  /* mrs x16, TPIDR_EL0
   * ldr x16, [x16, #64]
   * ldr x16, [x16, #8]
   * cbz x16, .lnoexception
   * bl MCC_ThrowPendingException
   * .lnoexception:
   */
  emitter.Emit("\t").Emit("mrs").Emit("\tx16, TPIDR_EL0");
  emitter.Emit("\n");
  emitter.Emit("\t").Emit("ldr").Emit("\tx16, [x16, #64]");
  emitter.Emit("\n");
  emitter.Emit("\t").Emit("ldr").Emit("\tx16, [x16, #8]");
  emitter.Emit("\n");
  emitter.Emit("\t").Emit("cbz").Emit("\tx16, .lnoeh.").Emit(cg.curCgFunc->GetName());
  emitter.Emit("\n");
  emitter.Emit("\t").Emit("bl").Emit("\t").Emit(GetIntrinsicFuncName(INTRN_MCCThrowPendingException));
  emitter.Emit("\n");
  emitter.Emit(".lnoeh.").Emit(cg.curCgFunc->GetName()).Emit(":");
  emitter.Emit("\n");

  return;
}

void AArch64Insn::Emit(CG &cg, Emitter &emitter) {
  MOperator mop = GetMachineOpcode();
  emitter.SetCurrentMOP(mop);
  const AArch64MD *md = &AArch64CG::kMd[mop];

  if (!cg.GenerateVerboseAsm() && mop == MOP_comment) {
    return;
  }
  if (mop == MOP_clinit) {
    EmitClinit(cg, emitter);
    return;
  } else if (mop == MOP_adrp_ldr) {
    EmitAdrpLdr(cg, emitter);
    return;
  } else if (mop == MOP_clinit_tail) {
    EmitClinitTail(cg, emitter);
    return;
  } else if (mop == MOP_adrp_label) {
    EmitAdrpLabel(cg, emitter);
    return;
  }

  if (CGOptions::nativeopt == true && mop == MOP_xbl) {
    FuncNameOperand *nameopnd = static_cast<FuncNameOperand *>(opnds[0]);
    if (nameopnd->GetName() == GetIntrinsicFuncName(INTRN_MCCCheckThrowPendingException)) {
      EmitCheckThrowPendingException(cg, emitter);
      return;
    }
  }

  std::string format(md->format_);
  emitter.Emit("\t").Emit(md->name_).Emit("\t");
  int seq[5];
  std::string prefix[5];  // used for print prefix like "*" in icall *rax
  int index = 0;

  errno_t eNum = memset_s(seq, sizeof(seq), -1, sizeof(seq));
  if (eNum) {
    FATAL(kLncFatal, "memset_s failed");
  }
  int commaNum = 0;
  for (uint32 i = 0; i < format.length(); i++) {
    char c = format[i];
    if (c >= '0' && c <= '5') {
      seq[index++] = c - '0';
      commaNum++;
    } else if (c != ',') {
      prefix[index].push_back(c);
    }
  }

  bool isreffield = false;
  // set opnd0 ref-field flag, so we can emit the right register
  if (IsAccessRefField() && AccessMem()) {
    Operand *opnd0 = opnds[seq[0]];
    if (opnd0->IsRegister()) {
      static_cast<AArch64RegOperand *>(opnd0)->isreffield_ = true;
      isreffield = true;
    }
  }

  for (int i = 0; i < commaNum; i++) {
    if (seq[i] == -1) {
      continue;
    }
    if (prefix[i].length() > 0) {
      emitter.Emit(prefix[i]);
    }

    opnds[seq[i]]->Emit(emitter, md->operand_[seq[i]]);
    // reset opnd0 ref-field flag, so following instruction has correct register
    if (isreffield && (i == 0)) {
      static_cast<AArch64RegOperand *>(opnds[seq[0]])->isreffield_ = false;
    }
    if (i != (commaNum - 1)) {
      emitter.Emit(", ");
    }
  }

  if (cg.GenerateVerboseAsm()) {
    std::string comment = GetComment();
    if (comment.c_str() && strlen(comment.c_str()) > 0) {
      emitter.Emit("\t\t// ").Emit(comment.c_str());
    }
  }

  emitter.Emit("\n");
}

void AArch64CGFunc::EmitBBHeaderLabel(const char *funcName, LabelIdx labelIdx) {
  cg->emitter_->EmitBBHeaderLabel(funcName, "//", labelIdx, *this);
}

std::string AArch64CGFunc::GetReflectString(uint32_t offset) {
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
    CG_ASSERT(onechar != nullptr, "onechar is nullptr in AArch64CGFunc::GetReflectString");
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

// emit java method description which contains address and size of local reference area
// as well as method metadata.
void AArch64CGFunc::EmitMethodDesc(Emitter &emitter) {
  if (!func->IsJava()) {
    return;
  }

  emitter.Emit("\t.section\t.rodata\n");
  emitter.Emit("\t.align\t2\n");

  std::string methodInfoLabel;
  GetMethodDescLabel(func->GetFuncSymbol()->GetName(), methodInfoLabel);
  emitter.Emit(methodInfoLabel).Emit(":\n");

  EmitRefToMethodInfo(emitter);

  // local reference area
  AArch64MemLayout *memLayout = static_cast<AArch64MemLayout *>(this->memlayout);
  int32 refOffset = memLayout->GetReflocbaseLoc();
  uint32 refNum = memLayout->GetSizeOfRefLocals() / kIntregBytelen;

  emitter.Emit("\t.short ").Emit(refOffset).Emit("\n");
  emitter.Emit("\t.short ").Emit(refNum).Emit("\n");
}

void AArch64CGFunc::EmitRefToMethodDesc(Emitter &emitter) {
  if (!func->IsJava()) {
    return;
  }

  std::string methodDescLabel;
  GetMethodDescLabel(func->GetFuncSymbol()->GetName(), methodDescLabel);
  emitter.Emit("\t.word ").Emit(methodDescLabel).Emit("-.\n");
}

void AArch64CGFunc::EmitRefToMethodInfo(Emitter &emitter) {
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
      CG_ASSERT(aggconst != nullptr, "aggconst is nullptr in AArch64CGFunc::EmitRefToMethodInfo");
      bool found = false;
      uint32 offset = 0;
      for (uint32 i = 0; i < aggconst->constVec.size(); i++) {
        MIRConst *elemcst = aggconst->constVec[i];
        MIRAggConst *onemethodconst = static_cast<MIRAggConst *>(elemcst);
        CG_ASSERT(onemethodconst != nullptr, "onemethodconst is nullptr in AArch64CGFunc::EmitRefToMethodInfo");
        MIRConst *funcnameindex = methodinfoIsCompact ? onemethodconst->constVec[METHOD_COMPACT::kMethodname]
                                                      : onemethodconst->constVec[METHOD_RO::kMethodname];
        MIRIntConst *intCt = static_cast<MIRIntConst *>(funcnameindex);
        CG_ASSERT(intCt != nullptr, "int_ct is nullptr in AArch64CGFunc::EmitRefToMethodInfo");
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
          emitter.Emit("\t.word ").Emit(methodinfostr);
          emitter.Emit("+1");
          offset *= sizeof(MethodMetadataCompact);
        } else {
          // here we still emit the pointer to MethodMetadata instead of MethodMetadataRO,
          // to let runtime have a consistent interface.
          emitter.Emit("\t.word ").Emit(NameMangler::kMethodsInfoPrefixStr + classname);
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

void AArch64CGFunc::Emit() {
  // check the size of  ImmOperand and MemOperand
  if (g_enableDebug) {
    CheckImmMemSize();
  }
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

  emitter.Emit("\t.align 2\n");
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
    emitter.Emit("\t.hidden\t").Emit(funcSt->GetName()).Emit("\n");
  }

  emitter.Emit("\t.type\t").Emit(funcSt->GetName()).Emit(", %function\n");

  // add these messege , solve the simpleperf tool error
  EmitRefToMethodDesc(emitter);

  emitter.Emit(funcSt->GetName()).Emit(":\n");

  // if the last  insn is call, then insert nop
  bool found = false;
  FOR_ALL_BB_REV(bb, this) {
    FOR_BB_INSNS_REV(insn, bb) {
      if (insn->IsMachineInstruction()) {
        if (insn->IsCall()) {
          Insn *newinsn = cg->BuildInstruction<AArch64Insn>(MOP_nop, nullptr, nullptr);
          bb->InsertInsnAfter(insn, newinsn);
        }
        found = true;
        break;
      }
    }
    if (found) {
      break;
    }
  }
  // emit instructions
  FOR_ALL_BB(bb, this) {
    if (bb->frequency) {
      emitter.Emit("#    ").Emit("freq:").Emit(bb->frequency).Emit("\n");
    }
    // emit bb headers
    if (bb->labidx != 0) {
      EmitBBHeaderLabel(funcSt->GetName().c_str(), bb->labidx);
    }

    FOR_BB_INSNS(insn, bb) {
      insn->Emit(*cg, emitter);
    }
  }
  if (CGOptions::maplelinker) {
    // Emit a label for calculating method size
    emitter.Emit(".label.end.").Emit(funcSt->GetName()).Emit(":\n");
  }
  emitter.Emit("\t.size\t").Emit(funcSt->GetName()).Emit(", .-").Emit(funcSt->GetName()).Emit("\n");

  // emit LSDA
  if (cg->GenerateCfiDirectives() && ehfunc) {
    if (!hasProEpilogue) {
      emitter.Emit("\t.word 0xFFFFFFFF\n");
      emitter.Emit("\t.word 0\n");
    } else if (ehfunc->NeedFullLSDA()) {
      LSDAHeader *lsdaheader = ehfunc->lsda_header;
      const char *funcname = funcSt->GetName().c_str();
      //  .word .label.lsda_label-func_start_label
      const char *idx = std::to_string(CG::curPuIdx).c_str();
      emitter.Emit("\t.word ")
        .Emit(".label.")
        .Emit(idx)
        .Emit("__")
        .Emit(lsdaheader->lsda_label->labelIdx)
        .Emit("-")
        .Emit(".label.")
        .Emit(idx)
        .Emit("__")
        .Emit(start_label->labelIdx)
        .Emit("\n");
    } else if (ehfunc->NeedFastLSDA()) {
      EmitFastLSDA();
    }
  }
  uint32 size = (func->symTab == nullptr ? 0 : func->symTab->GetSymbolTableSize());
  for (size_t i = 0; i < size; i++) {
    MIRSymbol *st = func->symTab->GetSymbolFromStIdx(i);
    if (st == nullptr) {
      continue;
    }
    MIRStorageClass storageClass = st->storageClass;
    MIRSymKind sKind = st->sKind;
    if (storageClass == kScPstatic && sKind == kStConst) {
      emitter.Emit("\t.align 2\n");
      emitter.Emit(st->GetName().c_str());
      emitter.Emit(":\n");
      if (MIRStr16Const *str16const = dynamic_cast<MIRStr16Const *>(st->GetConst())) {
        emitter.EmitStr16Constant(str16const);
        emitter.Emit("\n");
        continue;
      } else if (MIRStrConst *strconst = dynamic_cast<MIRStrConst *>(st->GetConst())) {
        emitter.EmitStrConstant(strconst);
        emitter.Emit("\n");
        continue;
      }

      switch (st->GetConst()->type->GetPrimType()) {
        case PTY_u32: {
          MIRIntConst *intconst = static_cast<MIRIntConst *>(st->GetConst());
          emitter.Emit("\t.long ").Emit(static_cast<uint32>(intconst->value)).Emit("\n");
          break;
        }
        case PTY_f32: {
          MIRFloatConst *floatconst = static_cast<MIRFloatConst *>(st->GetConst());
          emitter.Emit("\t.word ").Emit(static_cast<uint32>(floatconst->GetIntValue())).Emit("\n");
          break;
        }
        case PTY_f64: {
          MIRDoubleConst *doubleconst = static_cast<MIRDoubleConst *>(st->GetConst());
          emitter.Emit("\t.word ").Emit(doubleconst->GetIntLow32()).Emit("\n");
          emitter.Emit("\t.word ").Emit(doubleconst->GetIntHigh32()).Emit("\n");
          break;
        }
        case PTY_v4i32: {
          MIRVectorIntConst *vecIntconst = static_cast<MIRVectorIntConst *> (st->GetConst());
          for (uint8 j = 0; j < vecIntconst->vecSize; j++) {
            emitter.Emit("\t.long ").Emit(static_cast<uint32>(vecIntconst->vecElems[i])).Emit("\n");
          }
          break;
        }
        default:
          CG_ASSERT(false, "NYI");
          break;
      }
    }
  }

  for (MapleVector<MIRSymbol *>::iterator stit = emitstvec_.begin(); stit != emitstvec_.end();
       stit++) {  // emit switch table only here
    MIRSymbol *st = *stit;
    CG_ASSERT(st->IsReadOnly(), "NYI");
    emitter.Emit("\n");
    emitter.Emit("\t.align 3\n");
    emitter.Emit(st->GetName().c_str()).Emit(":\n");
    MIRAggConst *arrayConst = static_cast<MIRAggConst *>(st->GetConst());
    CHECK_FATAL(arrayConst, "null ptr check");
    for (uint32 i = 0; i < arrayConst->constVec.size(); i++) {
      MIRLblConst *lblconst = static_cast<MIRLblConst *>(arrayConst->constVec[i]);
      CHECK_FATAL(lblconst, "null ptr check");
      emitter.Emit("\t.quad\t");
      const char *idx = std::to_string(CG::curPuIdx).c_str();
      emitter.Emit(".label.").Emit(idx).Emit("__").Emit(lblconst->value);
      emitter.Emit(" - ").Emit(st->GetName().c_str());
      emitter.Emit("\n");
    }
  }

  if (ehfunc && ehfunc->NeedFullLSDA()) {
    EmitFullLSDA();
  }
}

void AArch64CGFunc::EmitFastLSDA()  // the fast_exception_handling lsda
{
  Emitter *emitter = cg->emitter_;
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  const char *funcname = funcSt->GetName().c_str();
  //  .word 0xFFFFFFFF
  //  .word .label.LTest_3B_7C_3Cinit_3E_7C_28_29V3-func_start_label
  emitter->Emit("\t.word 0xFFFFFFFF\n");
  const char *idx = std::to_string(CG::curPuIdx).c_str();
  if (NeedCleanup()) {
    emitter->Emit("\t.word ")
      .Emit(".label.")
      .Emit(idx)
      .Emit("__")
      .Emit(cleanup_label->labelIdx)
      .Emit("-")
      .Emit(".label.")
      .Emit(idx)
      .Emit("__")
      .Emit(start_label->labelIdx)
      .Emit("\n");
  } else {
    CG_ASSERT(!exitbbsvec.empty(), "exitbbsvec is empty in AArch64CGFunc::EmitFastLSDA");
    emitter->Emit("\t.word ")
      .Emit(".label.")
      .Emit(idx)
      .Emit("__")
      .Emit(exitbbsvec[0]->labidx)
      .Emit("-")
      .Emit(".label.")
      .Emit(idx)
      .Emit("__")
      .Emit(start_label->labelIdx)
      .Emit("\n");
  }
}

void AArch64CGFunc::EmitFullLSDA()  // the normal gcc_except_table
{
  Emitter *emitter = cg->emitter_;

  // emit header
  emitter->Emit("\t.align 2\n");
  emitter->Emit("\t.section .gcc_except_table,\"a\",@progbits\n");
  emitter->Emit("\t.align 2\n");

  // emit LSDA header
  LSDAHeader *lsdaheader = ehfunc->lsda_header;
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  const char *funcname = funcSt->GetName().c_str();
  emitter->EmitStmtLabel(funcname, lsdaheader->lsda_label->labelIdx);
  emitter->Emit("\t.byte ").Emit(lsdaheader->lpstart_encoding).Emit("\n");
  emitter->Emit("\t.byte ").Emit(lsdaheader->ttype_encoding).Emit("\n");
  emitter->Emit("\t.uleb128 ");
  emitter->EmitLabelPair(funcname, lsdaheader->ttype_offset);
  emitter->EmitStmtLabel(funcname, lsdaheader->ttype_offset.start_offset->labelIdx);

  // emit call site table
  emitter->Emit("\t.byte ").Emit(lsdaheader->callsite_encoding).Emit("\n");
  // callsite table size
  emitter->Emit("\t.uleb128 ");
  emitter->EmitLabelPair(funcname, ehfunc->lsda_callsite_table->cs_table);
  // callsite start
  emitter->EmitStmtLabel(funcname, ehfunc->lsda_callsite_table->cs_table.start_offset->labelIdx);

  MapleVector<LSDACallSite *> &callsiteTable = ehfunc->lsda_callsite_table->callsite_table;
  std::sort(callsiteTable.begin(), callsiteTable.end(), [this](LSDACallSite *a, LSDACallSite *b) {
    LabelIDOrder id1 = GetOrCreateLabelOperand(a->cs_start.end_offset->labelIdx)->GetLabelOrder();
    LabelIDOrder id2 = GetOrCreateLabelOperand(b->cs_start.end_offset->labelIdx)->GetLabelOrder();
    CG_ASSERT(id1 != -1u && id2 != -1u, "illegal label order assigned");
    return id1 <= id2;
  });

  for (uint32 i = 0; i < callsiteTable.size(); i++) {
    LSDACallSite *lsdacallsite = callsiteTable[i];
    emitter->Emit("\t.uleb128 ");
    emitter->EmitLabelPair(funcname, lsdacallsite->cs_start);

    emitter->Emit("\t.uleb128 ");
    emitter->EmitLabelPair(funcname, lsdacallsite->cs_length);

    if (lsdacallsite->cs_landing_pad.start_offset) {
      emitter->Emit("\t.uleb128 ");
      emitter->EmitLabelPair(funcname, lsdacallsite->cs_landing_pad);
    } else {
      CG_ASSERT(lsdacallsite->cs_action == 0, "");
      if (NeedCleanup()) {
        // if landing pad is 0, we emit this call site as cleanup code
        LabelPair cleaupCode;
        cleaupCode.start_offset = start_label;
        cleaupCode.end_offset = cleanup_label;
        emitter->Emit("\t.uleb128 ");
        emitter->EmitLabelPair(funcname, cleaupCode);
      } else if (func->IsJava()) {
        CG_ASSERT(!exitbbsvec.empty(), "exitbbsvec is empty in AArch64CGFunc::EmitFullLSDA");
        const char *idx = std::to_string(CG::curPuIdx).c_str();
        emitter->Emit("\t.uleb128 ")
          .Emit(".label.")
          .Emit(idx)
          .Emit("__")
          .Emit(exitbbsvec[0]->labidx)
          .Emit(" - ")
          .Emit(".label.")
          .Emit(idx)
          .Emit("__")
          .Emit(start_label->labelIdx)
          .Emit("\n");
      } else {
        emitter->Emit("\t.uleb128 0\n");
      }
    }

    emitter->Emit("\t.uleb128 ").Emit(lsdacallsite->cs_action).Emit("\n");
  }

  // quick hack: insert a call site entry for the whole function body.
  // this will hand in any pending (uncaught) exception to its caller. Note that
  // __gxx_personality_v0 in libstdc++ is coded so that if exception table exists,
  // the call site table must have an entry for any possibly raised exception,
  // otherwise __cxa_call_terminate will be invoked immediately, thus the caller
  // does not get the chance to take charge.
  if (NeedCleanup() || func->IsJava()) {
    // call site for clean-up
    LabelPair funcStart;
    funcStart.start_offset = start_label;
    funcStart.end_offset = start_label;
    emitter->Emit("\t.uleb128 ");
    emitter->EmitLabelPair(funcname, funcStart);

    LabelPair funcLength;
    funcLength.start_offset = start_label;
    funcLength.end_offset = cleanup_label;
    emitter->Emit("\t.uleb128 ");
    emitter->EmitLabelPair(funcname, funcLength);

    LabelPair cleaupCode;
    cleaupCode.start_offset = start_label;
    cleaupCode.end_offset = cleanup_label;
    if (NeedCleanup()) {
      emitter->Emit("\t.uleb128 ");
      emitter->EmitLabelPair(funcname, cleaupCode);
    } else {
      CG_ASSERT(!exitbbsvec.empty(), "exitbbsvec is empty in AArch64CGFunc::EmitFullLSDA");
      const char *idx = std::to_string(CG::curPuIdx).c_str();
      emitter->Emit("\t.uleb128 ")
        .Emit(".label.")
        .Emit(idx)
        .Emit("__")
        .Emit(exitbbsvec[0]->labidx)
        .Emit(" - ")
        .Emit(".label.")
        .Emit(idx)
        .Emit("__")
        .Emit(start_label->labelIdx)
        .Emit("\n");
    }
    emitter->Emit("\t.uleb128 0\n");
    if (!func->IsJava()) {
      // call site for stack unwind
      LabelPair unwindStart;
      unwindStart.start_offset = start_label;
      unwindStart.end_offset = cleanup_label;
      emitter->Emit("\t.uleb128 ");
      emitter->EmitLabelPair(funcname, unwindStart);

      LabelPair unwindLength;
      unwindLength.start_offset = cleanup_label;
      unwindLength.end_offset = end_label;
      emitter->Emit("\t.uleb128 ");
      emitter->EmitLabelPair(funcname, unwindLength);

      emitter->Emit("\t.uleb128 0\n");
      emitter->Emit("\t.uleb128 0\n");
    }
  }

  // callsite end label
  emitter->EmitStmtLabel(funcname, ehfunc->lsda_callsite_table->cs_table.end_offset->labelIdx);

  // tt
  LSDAActionTable *lsdaactiontable = ehfunc->lsda_action_table;
  for (uint32 i = 0; i < lsdaactiontable->action_table.size(); i++) {
    LSDAAction *lsdaaction = lsdaactiontable->action_table[i];
    emitter->Emit("\t.byte ").Emit(lsdaaction->action_index).Emit("\n");
    emitter->Emit("\t.byte ").Emit(lsdaaction->action_filter).Emit("\n");
  }
  emitter->Emit("\t.align 2\n");
  if (mirModule.IsJavaModule()) {
    for (int32 i = ehfunc->eh_ty_table.size() - 1; i >= 0; i--) {
      MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ehfunc->eh_ty_table[i]);
      if (mirType->typeKind == kTypeScalar && mirType->primType == PTY_void) {
        continue;
      }
      CG_ASSERT(mirType->typeKind == kTypeClass, "NYI");
      const std::string &tyname = GlobalTables::GetStrTable().GetStringFromStrIdx(mirType->nameStrIdx);
      std::string dwrefstring("DW.ref.");
      dwrefstring.append(CLASSINFO_PREFIX_STR);
      dwrefstring.append(tyname);
      dwrefstring.append(" - .");
      emitter->Emit("\t.4byte ").Emit(dwrefstring.c_str()).Emit("\n");
    }
  } else {
    GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName("__TYPEINFO_TABLE__");
    MIRSymbol *typeinfoTableSym = func->symTab->GetSymbolFromStrIdx(strIdx);
    if (typeinfoTableSym != nullptr) {
      MIRAggConst *arrayConst = static_cast<MIRAggConst *>(typeinfoTableSym->value.konst);
      if (arrayConst != nullptr) {
        for (MIRConst *typeinfoEntryConst : arrayConst->constVec) {
          MIRAggConst *aggconst = static_cast<MIRAggConst *>(typeinfoEntryConst);
          if (aggconst->constVec[0]->kind != kConstAddrof) {
            continue;
          }
          MIRAddrofConst *addrofConst = static_cast<MIRAddrofConst *>(aggconst->constVec[0]);
          MIRSymbol *typeinfoSymbol = GlobalTables::GetGsymTable().GetSymbolFromStIdx(addrofConst->GetSymbolIndex().Idx());
          std::string dwrefstring("DW.ref.");
          dwrefstring.append(typeinfoSymbol->GetName());
          dwrefstring.append(" - .");
          emitter->Emit("\t.4byte ").Emit(dwrefstring.c_str()).Emit("\n");
        }
      }
    }
  }
  // end of lsda
  emitter->EmitStmtLabel(funcname, lsdaheader->ttype_offset.end_offset->labelIdx);
}

/*
   Table C1-6 A64 Load/Store addressing modes
 |         Offset
   Addressing Mode    | Immediate     | Register             | Extended Register

   Base register only | [base{,#0}]   | -                    | -
   (no offset)        | B_OI_NONE     |                      |
                     imm=0

   Base plus offset   | [base{,#imm}] | [base,Xm{,LSL #imm}] | [base,Wm,(S|U)XTW {#imm}]
                     B_OI_NONE     | B_OR_X               | B_OR_X
                                     imm=0,1 (0,3)        | imm=00,01,10,11 (0/2,s/u)

   Pre-indexed        | [base, #imm]! | -                    | -

   Post-indexed       | [base], #imm  | [base], Xm(a)        | -

   Literal            | label         | -                    | -
   (PC-relative)

   a) The post-indexed by register offset mode can be used with the SIMD Load/Store
   structure instructions described in Load/Store Vector on page C3-154. Otherwise
   the post-indexed by register offset mode is not available.
 */
void AArch64CGFunc::EmitOperand(Operand *opnd, OpndProp *prop) {
  ofstream &outf = cg->emitter_->out;
  AArch64OpndProp *opndprop = static_cast<AArch64OpndProp *>(prop);
  if (RegOperand *regopnd = dynamic_cast<RegOperand *>(opnd)) {
    RegType regtype = regopnd->GetRegisterType();
    CG_ASSERT((!opndprop || (opndprop->opnd_ty_ == Operand::Opd_Register)), "operand type doesn't match");
    uint8 size = opndprop ? opndprop->size_ : regopnd->size_;  // opndprop null means a sub emit, i.e from MemOperand
    regno_t regNo = regopnd->GetRegisterNumber();
    switch (regtype) {
      case kRegTyInt:
        CG_ASSERT((size == 32 || size == 64), "illegal register size");
        outf << AArch64CG::intRegNames[((size == 32) ? AArch64CG::kR32List : AArch64CG::kR64List)][regNo];
        break;
      case kRegTyFloat: {
        CG_ASSERT((size == 8 || size == 16 || size == 32 || size == 64), "illegal register size");
        AArch64RegOperand *a64regopnd = static_cast<AArch64RegOperand *>(regopnd);
        if (a64regopnd->IsSimdVectorMode()) {
          // see if it is slot 0 or 1 of simd
          if (regNo >= VB64) {
            regNo -= VB64 + V0;
          } else if (regNo >= VB32) {
            regNo -= VB32 + V0;
          }
          outf << AArch64CG::intRegNames[(AArch64CG::kV64List)][regNo];
          if (a64regopnd->GetSimdVectorType() == 0) {
            // 2 * 64bits
            outf << ".d[";
          } else {
            CG_ASSERT(a64regopnd->GetSimdVectorType() == 1, "EmitOperand : wrong simd type");
            outf << ".s[";
          }
          outf << a64regopnd->GetSimdVectorPosition() << "]";
        } else {
          outf << AArch64CG::intRegNames[__builtin_ctz(size) - 3][regNo];
        }
        break;
      }
      default:
        CG_ASSERT(false, "NYI");
        break;
    }
  } else if (ImmOperand *immopnd = dynamic_cast<ImmOperand *>(opnd)) {
    outf << (opndprop->IsLoadLiteral() ? "=" : "#");
    outf << (immopnd->size_ == 64 ? immopnd->GetValue() : static_cast<int64>(static_cast<int32>(immopnd->GetValue())));
  } else if (dynamic_cast<ImmFPZeroOperand *>(opnd)) {
    outf << "#0.0";
  } else if (AArch64OfstOperand *ofstopnd = dynamic_cast<AArch64OfstOperand *>(opnd)) {
    outf << "#" << ofstopnd->GetOffsetValue();
  } else if (AArch64MemOperand *memopnd = dynamic_cast<AArch64MemOperand *>(opnd)) {
    AArch64MemOperand::AArch64AddressingMode addrMode = memopnd->GetAddrMode();
    if (addrMode == AArch64MemOperand::kAddrModeBOi) {
      outf << "[";
      EmitOperand(memopnd->GetBaseRegister(), nullptr);
      OfstOperand *offset = memopnd->GetOffsetImmediate();
      if (offset) {
        if (memopnd->IsPostIndexed()) {
          outf << "]";
          if (!offset->IsZero()) {
            outf << ", ";
            EmitOperand(offset, nullptr);
          }
        } else if (memopnd->IsPreIndexed()) {
          if (!offset->IsZero()) {
            outf << ",";
            EmitOperand(offset, nullptr);
          }
          outf << "]!";
        } else {
          if (!offset->IsZero()) {
            outf << ",";
            EmitOperand(offset, nullptr);
          }
          outf << "]";
        }
      } else {
        outf << "]";
      }
    } else if (addrMode == AArch64MemOperand::kAddrModeBOrX) {
      // Base plus offset   | [base{, #imm}]  [base, Xm{, LSL #imm}]   [base, Wm, (S|U)XTW {#imm}]
      //                      offset_opnds=nullptr
      //                                      offset_opnds=64          offset_opnds=32
      //                                      imm=0 or 3               imm=0 or 2, s/u
      outf << "[";
      EmitOperand(memopnd->GetBaseRegister(), nullptr);
      outf << ",";
      EmitOperand(memopnd->GetOffsetRegister(), nullptr);
      if (memopnd->ShouldEmitExtend()) {
        outf << ",";
        // extend, #0, of #3/#2
        outf << memopnd->GetExtendAsString();
        outf << " #";
        outf << memopnd->ShiftAmount();
      }
      outf << "]";
    } else if (addrMode == AArch64MemOperand::kAddrModeLiteral) {
      if (opndprop->IsMemLow12()) {
        outf << "#:lo12:";
      }
      outf << memopnd->GetSymbol()->GetName();
    } else {
      CG_ASSERT(false, "nyi");
    }
  } else if (StImmOperand *stopnd = dynamic_cast<StImmOperand *>(opnd)) {
    if (opndprop->IsLiteralLow12()) {
      outf << "#:lo12:";
    }
    if (CGOptions::doPIC &&
        (stopnd->GetSymbol()->GetStorageClass() == kScGlobal || stopnd->GetSymbol()->GetStorageClass() == kScExtern)) {
      outf << ":got:" + stopnd->GetName();
    } else {
      outf << stopnd->GetName();
    }
  } else if (LabelOperand *lblopnd = dynamic_cast<LabelOperand *>(opnd)) {
    const char *idx = std::to_string(CG::curPuIdx).c_str();
    outf << ".label." << idx << "__" << lblopnd->labidx_;
  } else if (FuncNameOperand *fn = dynamic_cast<FuncNameOperand *>(opnd)) {
    outf << fn->GetName();
  } else if (ListOperand *listopnd = dynamic_cast<ListOperand *>(opnd)) {
    uint32 size = listopnd->GetOperands().size();
    uint32 i = 0;
    for (auto opnd : listopnd->GetOperands()) {
      EmitOperand(opnd, nullptr);
      if (i != (size - 1)) {
        outf << ", ";
      }
      ++i;
    }
  } else if (CondOperand *condOperand = dynamic_cast<CondOperand *>(opnd)) {
    outf << CondOperand::ccStrs[condOperand->GetCode()];
  } else if (LogicalShiftLeftOperand *lslOperand = dynamic_cast<LogicalShiftLeftOperand *>(opnd)) {
    outf << " LSL #" << lslOperand->shift_amount;
  } else if (CommentOperand *comment = dynamic_cast<CommentOperand *>(opnd)) {
    outf << comment->GetComment();
  } else {
    CG_ASSERT(false, "NYI");
  }
}

}  // namespace maplebe
