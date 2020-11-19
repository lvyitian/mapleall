/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64PEEP_H
#define MAPLEBE_INCLUDE_CG_AARCH64PEEP_H

#include "riscv64_cg.h"
#include "cg_assert.h"
#include <vector>
#include "optimize_common.h"
#include "mir_builder.h"

namespace maplebe {

class Riscv64Peep {
 private:
  CGFunc *cgfunc;
  CG *cg;

 private:
  bool IsMemOperandsIdentical(Insn *insn, Insn *ninsn);
  bool IsSameReg(Operand *firstOpnd, Operand *secondOpnd);
  bool PredBBCheck(BB *bb, bool checkcbz, Operand *opnd);
  bool OpndDefByMovZero(Insn *insn);
  bool NoPreDefine(Insn *testInsn);
  bool OpndDefByOneValidBit(Insn *defInsn);
  Insn *DefInsnOfOperandInBB(BB *bb, Insn *startinsn, Insn *checkinsn, int opndidx);
  bool PatternMatch(std::vector<Insn *> &optInsn);

 public:
  explicit Riscv64Peep(CGFunc *cf) : cgfunc(cf) {
    cg = cgfunc->cg;
  }

  ~Riscv64Peep(){};
  void RemoveIdenticalLoadAndStore();
  void Peephole0();
  void Peephole();
  void PostRemoveSext();
  void RemoveSext();
  void ConvertPseudoOps();
  void PeepholeOpt();
  void DeleteMovAfterCbzOrCbnz();
  void CmpCsetOptimize();
  void LongIntCompareWithZ();
  void ComplexMemOperandOpt();
  void ComplexMemOperandOptLSL();
  void ReplaceDivisionToMultiplication();
  void PrePeepholeOpt();
  void PrePeepholeOpt1();
  void ReplaceInstruction();
  bool IfOperandIsLiveAfterInsn(RegOperand *regOpnd, Insn *insn);

};  // class Riscv64Peep

}  // namespace maplebe
#endif
