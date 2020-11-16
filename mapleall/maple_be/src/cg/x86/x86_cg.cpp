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

#include "cg.h"
#include "x86_isa.h"
#include "x86_cg.h"
#include "cg_assert.h"
#include <iostream>

namespace maplebe {

#include "x86_opnd.def"
#define DEFINE_MOP(op,a2,a3,a4,a5,a6,a7,a8,a9) {op,a2,a3,a4,a5,a6,a7,a8,a9},
const X86MD X86CG::thex86machine[MOP_last] = {
#include "x86_md.def"
};
#undef DEFINE_MOP

void X86Insn::CheckOpnd(Operand *opnd, OpndProp *prop) {
  X86OpndProp *mopd = static_cast<X86OpndProp *>(prop);
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
    case Operand::Opd_Mem:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_Mem, "expect mem");
      break;
    case Operand::Opd_BbAddress:
      CG_ASSERT(mopd->opnd_ty_ == Operand::Opd_BbAddress, "expect address");
      break;
    default:
      CG_ASSERT(false, "NYI");
  }
}

bool X86Insn::Check() {
  MOperator mop = GetMachineOpcode();
  const X86MD *md = &X86CG::thex86machine[mop];
  for (int i = 0; i < MAX_OPERAND_NUM; ++i) {
    Operand *opnd = GetOperand(i);
    if (opnd) {
      CheckOpnd(opnd, md->operand_[i]);
    }
  }
  return true;
}

void X86Insn::dump() {
  MOperator mop = GetMachineOpcode();
  const X86MD *md = &X86CG::thex86machine[mop];
  cout << " " << md->name_ << "(" << mop << ")";

  for (int i = 0; i < MAX_OPERAND_NUM; ++i) {
    Operand *opnd = GetOperand(i);
    if (opnd) {
      cout << " (opnd" << i << ": ";
      opnd->dump();
      cout << ")";
    }
  }
  std::cout << std::endl;
}

}  // namespace maplebe
