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

#include "cfi.h"
#include "emit.h"
#include <iostream>

using namespace maplebe;
namespace cfi {

struct CfiDescr {
  const char *name;
  int32 opnd_count;
  Operand::OperandType opnd_types[3];
};

static CfiDescr cfiDescrTable[kOpCfiLast + 1] = {
#define CFI_DEFINE(k, sub, n, o0, o1, o2) \
  { ".cfi_" #k, n, { Operand::Opd_##o0, Operand::Opd_##o1, Operand::Opd_##o2 } },
#include "cfi.def"
#undef CFI_DEFINE
  { ".cfi_undef", 0, { Operand::Opd_Undef, Operand::Opd_Undef, Operand::Opd_Undef } }
};

void CfiInsn::dump() {
  MOperator mop = GetMachineOpcode();
  CfiDescr &d = cfiDescrTable[mop];
  LogInfo::MapleLogger() << "CFI " << d.name;
  for (int32 i = 0; i < d.opnd_count; ++i) {
    LogInfo::MapleLogger() << (i == 0 ? " : " : " ");
    GetOperand(i)->dump();
  }
  LogInfo::MapleLogger() << endl;
}

bool CfiInsn::Check() {
  CfiDescr &d = cfiDescrTable[GetMachineOpcode()];
  CG_ASSERT(!GetOperand(2) && !GetOperand(3) && !GetOperand(4), "too many operands");
  for (int i = 0; i < 2; ++i) {
    Operand *o = GetOperand(i);
    if (!o) {
      CG_ASSERT(d.opnd_count == i, "too few operands");
      return true;
    }
    CG_ASSERT(o->GetKind() == d.opnd_types[i], "incorrect operand");
  }
  return true;
}

void CfiInsn::Emit(CG &cg, Emitter &emitter) {
  MOperator mop = GetMachineOpcode();
  CfiDescr &d = cfiDescrTable[mop];
  emitter.Emit("\t").Emit(d.name);
  for (int32 i = 0; i < d.opnd_count; ++i) {
    emitter.Emit(" ");
    GetOperand(i)->Emit(emitter, nullptr);
    if (i < d.opnd_count - 1) {
      emitter.Emit(",");
    }
  }
  emitter.Emit("\n");
}

void RegOperand::Emit(Emitter &emitter, OpndProp *) {
  emitter.Emit(reg_no_);
}

void RegOperand::dump() {
  LogInfo::MapleLogger() << "reg: " << reg_no_ << "[ size: " << GetSize() << "] ";
}

void ImmOperand::Emit(Emitter &emitter, OpndProp *) {
  emitter.Emit(val_);
}

void ImmOperand::dump() {
  LogInfo::MapleLogger() << "imm: " << val_ << "[ size: " << GetSize() << "] ";
}

void StrOperand::Emit(Emitter &emitter, OpndProp *opndprop) {
  emitter.Emit(str_);
}

void StrOperand::dump() {
  LogInfo::MapleLogger() << str_;
}

void LabelOperand::Emit(Emitter &emitter, OpndProp *opndprop) {
  const char *idx = std::to_string(CG::curPuIdx).c_str();
  emitter.Emit(".label.").Emit(idx).Emit("__").Emit(labidx_);
}

void LabelOperand::dump() {
  LogInfo::MapleLogger() << "label:" << labidx_;
}

}  // namespace cfi

namespace maplebe {

void Emitter::EmitCFISectionNames(const char *const names[]) {
  Emit("\t");
  Emit(cfi::cfiDescrTable[cfi::OP_CFI_sections].name);
  Emit("\t");
  for (int i = 0; names[i] != nullptr; ++i) {
    if (i > 0) {
      Emit(", ");
    }
    Emit(names[i]);
  }
  Emit("\n");
}

AnalysisResult *CgDoGenCfi::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  cgfunc->GenerateCfiPrologEpilog();
  return nullptr;
}

}  // namespace maplebe
