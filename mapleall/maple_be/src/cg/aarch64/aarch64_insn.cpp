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

#include "aarch64_insn.h"
#include "aarch64_cg.h"
#include "insn.h"
#include "cg_assert.h"
#include <iostream>

namespace maplebe {

int32_t AArch64Insn::GetResultNum() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->resnum;
}

int32_t AArch64Insn::GetOpndNum() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->opndnum;
}

Operand *AArch64Insn::GetResult(int32_t i) {
  CG_ASSERT(i < GetResultNum(), "index out of range");
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  if (md->IsStore()) {
    return GetOperand(GetOpndNum() + i);
  } else {
    return GetOperand(i);
  }
}

Operand *AArch64Insn::GetResultMemOpnd() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  if (md->IsStore()) {
    return GetOperand(GetOpndNum());
  } else {
    return GetOperand(GetResultNum());
  }
}

void AArch64Insn::SetOpnd(int32_t i, Operand *opnd) {
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

Operand *AArch64Insn::GetOpnd(int32_t i) {
  CG_ASSERT(i < GetOpndNum(), "index out of range");
  if (IsDestRegAlsoSrcReg()) {
    return GetOperand(i);
  }
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  if (md->IsStore()) {
    return GetOperand(i);
  } else {
    return GetOperand(GetResultNum() + i);
  }
}

Operand *AArch64Insn::GetMemOpnd() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  if (md->IsLoad()) {
    return GetOperand(GetResultNum());
  }
  return GetOperand(GetOpndNum());
}

bool AArch64Insn::IsVolatile() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsVolatile();
}

bool AArch64Insn::IsMemAccessBar() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsMemAccessBar();
}

bool AArch64Insn::IsBranch() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsBranch();
}

bool AArch64Insn::IsCall() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsCall();
}

bool AArch64Insn::IsTailCall() const {
  return mop_ == MOP_tail_call_opt_xbl || mop_ == MOP_tail_call_opt_xblr;
}

bool AArch64Insn::IsClinit() {
  if (mop_ == MOP_clinit || mop_ == MOP_clinit_tail) {
    return true;
  }
  return false;
}

bool AArch64Insn::CanThrow() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->CanThrow();
}

bool AArch64Insn::IsMemAccess() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsMemAccess();
}

bool AArch64Insn::MayThrow() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  if (md->IsMemAccess()) {
    AArch64MemOperand *aarchMemOpnd = static_cast<AArch64MemOperand *>(GetMemOpnd());
    CG_ASSERT(aarchMemOpnd, "CG invalid memory operand.");
    RegOperand *baseRegister = aarchMemOpnd->GetBaseRegister();
    if (baseRegister &&
        (baseRegister->GetRegisterNumber() == RFP || baseRegister->GetRegisterNumber() == RSP)) {
      return false;
    }
  }
  return md->CanThrow();
}

bool AArch64Insn::IsCallToFunctionThatNeverReturns() {
  if (IsIndirectCall()) {
    return false;
  }
  FuncNameOperand *target = static_cast<FuncNameOperand *>(GetCallTargetOperand());
  CHECK_FATAL(target, "target is null in AArch64Insn::IsCallToFunctionThatNeverReturns");
  MIRSymbol *funcst = target->GetFunctionSymbol();
  CG_ASSERT(funcst->sKind == kStFunc, "");
  MIRFunction *func = funcst->value.mirFunc;
  return func->NeverReturns();
}

bool AArch64Insn::IsDMBInsn() const {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsDMB();
}

bool AArch64Insn::IsMove() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsMove();
}

bool AArch64Insn::IsLoad() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsLoad();
}

bool AArch64Insn::IsStore() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsStore();
}

bool AArch64Insn::IsLoadPair() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsLoadPair();
}

bool AArch64Insn::IsStorePair() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsStorePair();
}

bool AArch64Insn::IsLoadStorePair() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsLoadStorePair();
}

bool AArch64Insn::IsLoadAddress() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsLoadAddress();
}

bool AArch64Insn::IsAtomic() {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsAtomic();
}

bool AArch64Insn::IsPartDef() const {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->IsPartDef();
}

uint32 AArch64Insn::GetLatencyType() const {
  const AArch64MD *md = &AArch64CG::kMd[mop_];
  return md->GetLatencyType();
}

bool AArch64Insn::IsYieldpoint() {
  // It is a yieldpoint if loading from a dedicated
  // register holding polling page address:
  // ldr  wzr, [RYP]
  if (IsLoad()) {
    auto mem = static_cast<MemOperand *>(GetOpnd(0));
    return (mem != nullptr && mem->GetBaseRegister() != nullptr && mem->GetBaseRegister()->GetRegisterNumber() == RYP);
  }
  return false;
}

int32_t AArch64Insn::CopyOperands() {
  MOperator opc = mop_;
  if (mop_ >= MOP_xmovrr && mop_ <= MOP_xvmovrv) {
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
 * Howeer for special long range branch patch, the label is installed in this case.
 */
int AArch64Insn::GetJumpTargetIdx() const {
  int operandIdx = 0;
  switch (mop_) {
    // unconditional jump
    case MOP_xuncond: {
      operandIdx = 0;
      break;
    }
    case MOP_xbr: {
      CHECK_FATAL(opnds[1] != 0, "ERR");
      operandIdx = 1;
      break;
    }
    // conditional jump
    case MOP_bmi:
    case MOP_bvc:
    case MOP_bls:
    case MOP_blt:
    case MOP_ble:
    case MOP_blo:
    case MOP_beq:
    case MOP_bpl:
    case MOP_bhs:
    case MOP_bvs:
    case MOP_bhi:
    case MOP_bgt:
    case MOP_bge:
    case MOP_bal:
    case MOP_bne:
    case MOP_wcbz:
    case MOP_xcbz:
    case MOP_wcbnz:
    case MOP_xcbnz: {
      operandIdx = 1;
      break;
    }
    case MOP_wtbz:
    case MOP_xtbz:
    case MOP_wtbnz:
    case MOP_xtbnz: {
      operandIdx = 2;
      break;
    }
    default:
      ASSERT(0, "Not a jump insn");
  }

  return operandIdx;
}

}  // namespace maplebe
