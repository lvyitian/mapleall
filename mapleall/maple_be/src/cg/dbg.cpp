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

#include "dbg.h"
#include "emit.h"
#include <iostream>

using namespace maplebe;

namespace mpldbg {

struct DbgDescr {
  const char *name;
  int32 opnd_count;
  Operand::OperandType opnd_types[3];
};

static DbgDescr dbgDescrTable[kOpDbgLast + 1] = {
#define DBG_DEFINE(k, sub, n, o0, o1, o2) { #k, n, { Operand::Opd_##o0, Operand::Opd_##o1, Operand::Opd_##o2 } },
#include "dbg.def"
#undef DBG_DEFINE
  { ".dbg_undef", 0, { Operand::Opd_Undef, Operand::Opd_Undef, Operand::Opd_Undef } }
};

void DbgInsn::dump() {
  DbgDescr &d = dbgDescrTable[mop_];
  LogInfo::MapleLogger() << "DBG " << d.name << " ( " << hex << this << " ) " << dec;
  for (int32 i = 0; i < d.opnd_count; ++i) {
    LogInfo::MapleLogger() << (i == 0 ? " : " : " ");
    GetOperand(i)->dump();
  }
  LogInfo::MapleLogger() << endl;
}

bool DbgInsn::Check() {
  DbgDescr &d = dbgDescrTable[GetMachineOpcode()];
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

void DbgInsn::Emit(CG &cg, Emitter &emitter) {
  DbgDescr &d = dbgDescrTable[mop_];
  if (cg.cgopt_.WithAsm() && strcmp(d.name, "loc") == 0) {
    emitter.Emit("\t// ");
  }
  emitter.Emit("\t.").Emit(d.name);
  for (int32 i = 0; i < d.opnd_count; ++i) {
    emitter.Emit(" ");
    GetOperand(i)->Emit(emitter, nullptr);
  }
  emitter.Emit("\n");
}

uint32 DbgInsn::GetLoc() const {
  if (mop_ != OP_DBG_loc) {
    return 0;
  }
  return static_cast<ImmOperand *>(opnds[0])->GetVal();
}

void ImmOperand::Emit(Emitter &emitter, OpndProp *) {
  emitter.Emit(val_);
}

void ImmOperand::dump() {
  LogInfo::MapleLogger() << " " << val_;
}

}  // namespace mpldbg
