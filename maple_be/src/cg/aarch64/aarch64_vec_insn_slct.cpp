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

#include "aarch64_insn.h"
#include "aarch64_cg.h"
#include "insn.h"
#include "cg_assert.h"
#include <iostream>

namespace maplebe {
void AArch64CGFunc::SelectVecAdd(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtyp) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  RegOperand *resOpnd = CreateRegisterOperandOfType(prmtyp);
  Operand *newOpnd0 = opnd0;
  Operand *newOpnd1 = opnd1;
  if (opnd0ty != Operand::Opd_Register) {
    newOpnd0 = SelectCopy(opnd0, prmtyp, prmtyp);
  }
  if (opnd1ty != Operand::Opd_Register) {
    newOpnd1 = SelectCopy(opnd1, prmtyp, prmtyp);
  }
  MOperator mop;
  switch (prmtyp) {
    case PTY_v4i32: {
      mop = MOP_vadd32rrr;
      break;
    }
    case PTY_v2i64:
    case PTY_v8i16:
    case PTY_v16i8:
    case PTY_v2f64:
    case PTY_v4f32:
     CHECK_FATAL(false, "NYI");
    default:
     CHECK_FATAL(false, "not supposed to be here");
  }
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, newOpnd0, newOpnd1));
}
} // namespace maplebe
