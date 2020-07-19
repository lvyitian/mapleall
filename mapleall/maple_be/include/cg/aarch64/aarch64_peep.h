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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64PEEP_H
#define MAPLEBE_INCLUDE_CG_AARCH64PEEP_H

#include "aarch64_cg.h"
#include "cg_assert.h"
#include <vector>
#include "optimize_common.h"
#include "mir_builder.h"

namespace maplebe {

class AArch64Peep {
 private:
  CGFunc *cgfunc;
  CG *cg;

 private:
  bool IsMemOperandsIdentical(Insn *insn, Insn *ninsn);
  bool DistanceCheck(BB *bb, LabelIdx targLabidx, uint32 targId);
  bool FindComputationTree(std::vector<Insn *> &optInsn, Insn *insn);
  bool PatternIsMatch(std::vector<Insn *> &optInsn);
  bool IsSameReg(Operand *firstOpnd, Operand *secondOpnd);
  bool IsSameRegNumAndRegSize(Operand *firstOpnd, Operand *secondOpnd);
  bool PredBBCheck(BB *bb, bool checkcbz, Operand *opnd);
  bool OpndDefByMovZero(Insn *insn);
  bool NoPreDefine(Insn *testInsn);
  bool OpndDefByOneValidBit(Insn *defInsn);
  Insn *DefInsnOfOperandInBB(BB *bb, Insn *startinsn, Insn *checkinsn, int opndidx);
  bool CheckOpndDefPoints(Insn *checkinsn, int opndidx);
  bool FlagNotUsedLaterInCurBB(BB *bb, Insn *startinsn);
  bool FindLondIntCmpWithZ(std::vector<Insn *> &optInsn, Insn *insn);
  bool PatternMatch(std::vector<Insn *> &optInsn);

 public:
  explicit AArch64Peep(CGFunc *cf) : cgfunc(cf) {
    cg = cgfunc->cg;
  }

  ~AArch64Peep(){};
  void RemoveIdenticalLoadAndStore();
  void Peephole0();
  void Peephole();
  void FixShortBranches();
  int IsPowerOf2(int64 val);
  void OptOneHoleBranches();
  void OptComputationTree();
  void OptZeroCmpBranches();
  void PeepholeOpt();
  void PreOptOneHoleBranches();
  void DeleteMovAfterCbzOrCbnz();
  void CmpCsetOptimize();
  void RemoveIncDecRef1Param();
  void RemoveIncDecRef2Param();
  void RemoveDecRef();
  void ReplaceIncDecWithInc();
  void LongIntCompareWithZ();
  bool IfOperandIsLiveAfterInsn(RegOperand *regOpnd, Insn *insn);
  void ComplexMemOperandOpt();
  void ComplexMemOperandOptLSL();
  void ReplaceDivisionToMultiplication();
  void PrePeepholeOpt();
  void PrePeepholeOpt1();
  void ReplaceInstruction();
  void OptAndCmpBranchesToTbz();
  void OptAndCmpBranchesToCset();

};  // class AArch64Peep

}  // namespace maplebe
#endif
