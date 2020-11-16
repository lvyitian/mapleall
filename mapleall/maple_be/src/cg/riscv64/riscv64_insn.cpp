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

#include "riscv64_insn.h"
#include "riscv64_cg.h"
#include "insn.h"
#include "cg_assert.h"
#include <iostream>

namespace maplebe {

int32_t Riscv64Insn::GetResultNum() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->resnum;
}

int32_t Riscv64Insn::GetOpndNum() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->opndnum;
}

Operand *Riscv64Insn::GetResult(int32_t i) {
  CG_ASSERT(i < GetResultNum(), "index out of range");
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  if (md->IsStore()) {
    return GetOperand(GetOpndNum() + i);
  } else {
    return GetOperand(i);
  }
}

Operand *Riscv64Insn::GetResultMemOpnd() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  if (md->IsStore()) {
    return GetOperand(GetOpndNum());
  } else {
    return GetOperand(GetResultNum());
  }
}

void Riscv64Insn::SetOpnd(int32_t i, Operand *opnd) {
  CG_ASSERT(i < GetOpndNum(), "index out of range");
  if (IsDestRegAlsoSrcReg()) {
    opnds[i] = opnd;
  }
  if (IsStore()) {
    opnds[i] = opnd;
  } else {
    opnds[GetResultNum() + i] = opnd;
  }
}

Operand *Riscv64Insn::GetOpnd(int32_t i) {
  CG_ASSERT(i < GetOpndNum(), "index out of range");
  if (IsDestRegAlsoSrcReg()) {
    return GetOperand(i);
  }
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  if (md->IsStore()) {
    return GetOperand(i);
  } else {
    return GetOperand(GetResultNum() + i);
  }
}

Operand *Riscv64Insn::GetMemOpnd() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  if (md->IsLoad()) {
    return GetOperand(GetResultNum());
  }
  return GetOperand(GetOpndNum());
}

bool Riscv64Insn::IsVolatile() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsVolatile();
}

bool Riscv64Insn::IsMemAccessBar() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsMemAccessBar();
}

bool Riscv64Insn::IsBranch() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsBranch();
}

bool Riscv64Insn::IsCall() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsCall();
}

bool Riscv64Insn::IsTailCall() const {
  return mop_ == MOP_tail_call_opt_xbl || mop_ == MOP_tail_call_opt_xblr;
}

bool Riscv64Insn::IsClinit() {
  if (mop_ == MOP_clinit || mop_ == MOP_clinit_tail) {
    return true;
  }
  return false;
}

bool Riscv64Insn::CanThrow() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->CanThrow();
}

bool Riscv64Insn::IsMemAccess() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsMemAccess();
}

bool Riscv64Insn::MayThrow() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  if (md->IsMemAccess()) {
    Riscv64MemOperand *aarchMemOpnd = static_cast<Riscv64MemOperand *>(GetMemOpnd());
    CG_ASSERT(aarchMemOpnd, "CG invalid memory operand.");
    RegOperand *baseRegister = aarchMemOpnd->GetBaseRegister();
    if (baseRegister &&
        (baseRegister->GetRegisterNumber() == RFP || baseRegister->GetRegisterNumber() == RSP)) {
      return false;
    }
  }
  return md->CanThrow();
}

bool Riscv64Insn::IsCallToFunctionThatNeverReturns() {
  if (IsIndirectCall()) {
    return false;
  }
  FuncNameOperand *target = static_cast<FuncNameOperand *>(GetCallTargetOperand());
  CHECK_FATAL(target, "target is null in Riscv64Insn::IsCallToFunctionThatNeverReturns");
  MIRSymbol *funcst = target->GetFunctionSymbol();
  CG_ASSERT(funcst->sKind == kStFunc, "");
  MIRFunction *func = funcst->value.mirFunc;
  return func->NeverReturns();
}

bool Riscv64Insn::IsDMBInsn() const {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsDMB();
}

bool Riscv64Insn::IsMove() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsMove();
}

bool Riscv64Insn::IsLoad() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsLoad();
}

bool Riscv64Insn::IsStore() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsStore();
}

bool Riscv64Insn::IsLoadPair() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsLoadPair();
}

bool Riscv64Insn::IsStorePair() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsStorePair();
}

bool Riscv64Insn::IsLoadStorePair() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsLoadStorePair();
}

bool Riscv64Insn::IsLoadAddress() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsLoadAddress();
}

bool Riscv64Insn::IsAtomic() {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsAtomic();
}

bool Riscv64Insn::IsPartDef() const {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->IsPartDef();
}

uint32 Riscv64Insn::GetLatencyType() const {
  const Riscv64MD *md = &Riscv64CG::kMd[mop_];
  return md->GetLatencyType();
}

bool Riscv64Insn::IsYieldpoint() {
  // It is a yieldpoint if loading from a dedicated
  // register holding polling page address:
  // ldr  wzr, [RYP]
  if (IsLoad()) {
    auto mem = static_cast<MemOperand *>(GetOpnd(0));
    return (mem != nullptr && mem->GetBaseRegister() != nullptr && mem->GetBaseRegister()->GetRegisterNumber() == RYP);
  }
  return false;
}

int32_t Riscv64Insn::CopyOperands() {
  MOperator opc = mop_;
  if (this->IsMove()) {
    return 0;
  } else if ((mop_ >= MOP_xaddrrr && mop_ <= MOP_ssub) || (mop_ >= MOP_xlslrri6 && mop_ <= MOP_wlsrrrr)) {
    ImmOperand *immopnd = nullptr;
    Operand *opnd2 = GetOpnd(1);
    if (opnd2 != nullptr && opnd2->IsIntImmediate()) {
      immopnd = static_cast<ImmOperand *>(opnd2);
      if (immopnd != nullptr && immopnd->IsZero()) {
        return 0;
      }
    }
  } else if (opc > MOP_xmulrrr && opc <= MOP_xvmuld) {
    Operand *opnd2 = GetOpnd(1);
    if (opnd2 != nullptr && opnd2->IsIntImmediate()) {
      ImmOperand *immopnd = static_cast<ImmOperand *>(opnd2);
      if (immopnd != nullptr && immopnd->GetValue() == 1) {
        return 0;
      }
    }
  }
  return -1;
}

/*
 * Precondition: The given insn is a jump instruction.
 * Get the jump target label operand index from the given instruction.
 * Note: MOP_xbr is a jump instruction, but the target is unknown at compile time,
 * because a register instead of label. So we don't take it as a branching instruction.
 */
int Riscv64Insn::GetJumpTargetIdx() const {
  int operandIdx = 0;
  switch (mop_) {
    // unconditional jump
    case MOP_xuncond: {
      operandIdx = 0;
      break;
    }
    // conditional jump
    case MOP_beq:
    case MOP_bne:
    case MOP_blt:
    case MOP_ble:
    case MOP_bgt:
    case MOP_bge:
    case MOP_blo:
    case MOP_bls:
    case MOP_bhs:
    case MOP_bhi:
    {
      operandIdx = 2;
      break;
    }
    case MOP_beqz:
    case MOP_bnez:
    case MOP_bltz:
    case MOP_blez:
    case MOP_bgtz:
    case MOP_bgez:
      operandIdx = 1;
      break;
    default:
      ASSERT(0, "Not a jump insn");
  }

  return operandIdx;
}

}  // namespace maplebe
