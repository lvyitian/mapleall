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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64EBO_H_
#define MAPLEBE_INCLUDE_CG_AARCH64EBO_H_

#include "ebo.h"
#include "riscv64_isa.h"
#include "operand.h"
#include "riscv64_operand.h"
#include "cg.h"
#include "emit.h"
#include "cg_assert.h"
#include <iostream>
#include <limits>

namespace maplebe {

using namespace maple;
using namespace std;

class Riscv64Ebo : public Ebo {
  friend class Ebo;

 private:
  void SetOpnd(Insn *insn, int i, Operand *opnd);
  bool IsZeroRegister(Operand *opnd);
  int32_t GetOffsetVal(MemOperand *mem) override;
  bool OperandEqSpecial(Operand *op1, Operand *op2) override;
  bool DoConstProp(Insn *insn, int i, Operand *opnd) override;
  bool ConstantOperand(Insn *insn, Operand **opnds, OpndInfo **opndInfo) override;
  bool ResoveCondBranch(Insn *insn, Operand **opnds) override;
  bool RemoveRedundantLoad(BB *bb, Insn *insn, Operand **opnds, OpndInfo **opndInfo, OpndInfo **origInfo) override;
  bool DeleteDupInsn(Insn *insn, OpndInfo **opndInfo, InsnInfo *insninfo) override;
  bool DeleteDupMemInsn(Insn *insn, OpndInfo **opndInfo, InsnInfo *insninfo, InsnInfo *prevInfo) override;
  bool IsImplicit(Insn *insn) override;
  void InitCallerSaveRegisters() override;
  void InitCallAndReturnUseRegisters() override;
  void DefineCallerSaveRegisters(InsnInfo *insninfo) override;
  void DefineReturnUseRegister(Insn *insn) override;
  void DefineCallUseSpecialRegister(Insn *insn) override;
  void DefineClinitSpecialRegisters(InsnInfo *insninfo) override;
  bool SpecialSequence(Insn *insn, Operand **opnds, OpndInfo **origInfo) override;
  bool ReplaceMovToVmov(Insn *insn, Insn *replaceInsn) override;
  bool IsMovToSIMDVmov(Insn *insn, Insn *replaceInsn) override;
  bool ChangeLdrMop(Insn *insn, Operand *opnd) override;
  bool IsAdd(Insn *insn) override;
  bool IsVecReg(Operand *opnd) override;
  bool IsFmov(Insn *insn) override;
  bool IsClinitCheck(Insn *insn) override;
  regno_t GetLowVec(Operand *opnd) override;
  regno_t GetHighVec(Operand *opnd) override;
  bool IsFloatReg(RegOperand *opnd) override;
  MOperator SelectMovMop(RegOperand *dstOpnd, RegOperand *srcOpnd);

 public:
  explicit Riscv64Ebo(CGFunc *func, MemPool *mp, LiveAnalysis *live, bool before, const char *phase)
    : Ebo(func, mp, live, before, phase), initCallerSaveRegs(false), initCallAndReturnRegs(false) {}
  ~Riscv64Ebo() {}

  RegOperand *regOperand[kMaxRegNum];
  bool initCallerSaveRegs;
  bool initCallAndReturnRegs;
};
}  // namespace maplebe
#endif
