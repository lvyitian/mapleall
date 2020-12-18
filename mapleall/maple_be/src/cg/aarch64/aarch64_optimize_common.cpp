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

#include "aarch64_optimize_common.h"
#include "aarch64_isa.h"
#include "aarch64_cg_func.h"
#include "cg_bb.h"
#include "dbg.h"

namespace maplebe {

bool AArch64InsnVisitor::ModifyBrInsn(maple::LabelIdx targetLabel, BB *&curbb) {
  int targetIdx = curbb->lastinsn->GetJumpTargetIdx();
  MOperator flippedOp;
  try {
    flippedOp = FlipConditionOp(curbb->lastinsn->mop_, targetIdx);
  } catch (...) {
    return false;
  }
  LabelOperand *targetOperand = static_cast<AArch64CGFunc *>(GetCGFunc())->GetOrCreateLabelOperand(targetLabel);
  curbb->lastinsn->mop_ = flippedOp;
  curbb->lastinsn->SetOperand(targetIdx, targetOperand);
  return true;
}

MOperator AArch64InsnVisitor::FlipConditionOp(MOperator originalOp, int &targetIdx) {
  MOperator flippedOp = 0;
  targetIdx = 1;
  switch (originalOp) {
    case AArch64MOP_t::MOP_beq:
      flippedOp = AArch64MOP_t::MOP_bne;
      break;
    case AArch64MOP_t::MOP_bge:
      flippedOp = AArch64MOP_t::MOP_blt;
      break;
    case AArch64MOP_t::MOP_bgt:
      flippedOp = AArch64MOP_t::MOP_ble;
      break;
    case AArch64MOP_t::MOP_bhi:
      flippedOp = AArch64MOP_t::MOP_bls;
      break;
    case AArch64MOP_t::MOP_bhs:
      flippedOp = AArch64MOP_t::MOP_blo;
      break;
    case AArch64MOP_t::MOP_ble:
      flippedOp = AArch64MOP_t::MOP_bgt;
      break;
    case AArch64MOP_t::MOP_blo:
      flippedOp = AArch64MOP_t::MOP_bhs;
      break;
    case AArch64MOP_t::MOP_bls:
      flippedOp = AArch64MOP_t::MOP_bhi;
      break;
    case AArch64MOP_t::MOP_blt:
      flippedOp = AArch64MOP_t::MOP_bge;
      break;
    case AArch64MOP_t::MOP_bne:
      flippedOp = AArch64MOP_t::MOP_beq;
      break;
    case AArch64MOP_t::MOP_xcbnz:
      flippedOp = AArch64MOP_t::MOP_xcbz;
      break;
    case AArch64MOP_t::MOP_wcbnz:
      flippedOp = AArch64MOP_t::MOP_wcbz;
      break;
    case AArch64MOP_t::MOP_xcbz:
      flippedOp = AArch64MOP_t::MOP_xcbnz;
      break;
    case AArch64MOP_t::MOP_wcbz:
      flippedOp = AArch64MOP_t::MOP_wcbnz;
      break;
    case AArch64MOP_t::MOP_wtbnz:
      flippedOp = AArch64MOP_t::MOP_wtbz;
      targetIdx = 2;
      break;
    case AArch64MOP_t::MOP_wtbz:
      flippedOp = AArch64MOP_t::MOP_wtbnz;
      targetIdx = 2;
      break;
    case AArch64MOP_t::MOP_xtbnz:
      flippedOp = AArch64MOP_t::MOP_xtbz;
      targetIdx = 2;
      break;
    case AArch64MOP_t::MOP_xtbz:
      flippedOp = AArch64MOP_t::MOP_xtbnz;
      targetIdx = 2;
      break;
    // Don't need to flip for always stands
    /*  case AArch64MOP_t::MOP_bal:
       case AArch64MOP_t::MOP_bnv:*/
    default:
      break;
  }
  return flippedOp;
}

void AArch64InsnVisitor::ModifyJumpTarget(Operand *targetOperand, BB *&bb) {
  if (bb->GetKind() == BB::kBBIgoto) {
    bool modified = false;
    for (Insn *insn = bb->lastinsn; insn != nullptr; insn = insn->prev) {
      if (insn->GetMachineOpcode() == MOP_adrp_label) {
        maple::LabelIdx labidx = static_cast<LabelOperand *>(targetOperand)->labidx_;
        ImmOperand *immopnd = static_cast<AArch64CGFunc *>(GetCGFunc())->CreateImmOperand(labidx, 8, false);
        insn->SetOperand(1, immopnd);
        modified = true;
      }
    }
    CHECK_FATAL(modified, "ModifyJumpTarget: Could not change jump target");
    return;
  } else if (bb->GetKind() == BB::kBBGoto) {
    for (Insn *insn = bb->lastinsn; insn != nullptr; insn = insn->prev) {
      if (insn->GetMachineOpcode() == MOP_adrp_label) {
        maple::LabelIdx labidx = static_cast<LabelOperand *>(targetOperand)->labidx_;
        LabelOperand *label = static_cast<AArch64CGFunc *>(GetCGFunc())->GetOrCreateLabelOperand(labidx);
        insn->SetOperand(1, label);
        break;
      }
    }
    // fallthru below to patch the branch insn
  }
  int targetIdx = bb->lastinsn->GetJumpTargetIdx();
  bb->lastinsn->SetOperand(targetIdx, targetOperand);
}

void AArch64InsnVisitor::ModifyJumpTarget(maple::LabelIdx targetLabel, BB *&bb) {
  LabelOperand *targetOperand = static_cast<AArch64CGFunc *>(GetCGFunc())->GetOrCreateLabelOperand(targetLabel);
  ModifyJumpTarget(targetOperand, bb);
}

void AArch64InsnVisitor::ModifyJumpTarget(BB *newTarget, BB *&bb) {
  int targetIdx = newTarget->lastinsn->GetJumpTargetIdx();
  Operand *targetOperand = newTarget->lastinsn->GetOperand(targetIdx);
  ModifyJumpTarget(targetOperand, bb);
}

Insn *AArch64InsnVisitor::CreateGotoInsn(Insn *condBrInsn) {
  int targetIdx = condBrInsn->GetJumpTargetIdx();
  Operand *target = condBrInsn->opnds[targetIdx];
  return new AArch64Insn(MOP_xuncond, target);
}

bool AArch64InsnVisitor::IsConditionAlwaysHold(Insn *cmpInsn, Insn *conditionBrInsn, Operand *operands[]) {
  switch (cmpInsn->mop_) {
    case MOP_wcmpri:
    case MOP_wcmprr:
    case MOP_xcmpri:
    case MOP_xcmprr: {
      AArch64ImmOperand *operand0 = static_cast<AArch64ImmOperand *>(operands[0]);
      AArch64ImmOperand *operand1 = static_cast<AArch64ImmOperand *>(operands[1]);

      int64 result = operand0->GetValue() - operand1->GetValue();
      return IsConditionAlwaysHold(conditionBrInsn->mop_, result);
    }
    default:
      return false;
  }
}

Insn *AArch64InsnVisitor::CloneInsn(Insn *originalInsn) {
  MemPool *mp = CG::curCgFunc->memPool;
  if (dynamic_cast<AArch64Insn *>(originalInsn)) {
    // Cannot just plain duplicate the memory, as operands are shared
    // across all instructions.  Example - operand for vreg 200 is
    // pointed to the same vreg:200 object.
    // Further, operand objects are tracked in tables.  Duplicating
    // memory will lose track of these objects.
    // For now, replace the operands, take care of leaks later.
    AArch64Insn *tobeCloned = static_cast<AArch64Insn *>(originalInsn);
    Insn *newInsn = mp->Clone<AArch64Insn>(*tobeCloned);
    for (uint32 i = 0; i < Insn::kMaxOperandNum; i++) {
      ImmOperand *iopnd = dynamic_cast<ImmOperand *>(originalInsn->opnds[i]);
      if (iopnd && iopnd->IsVary()) {
        AArch64ImmOperand *cloneImm = static_cast<AArch64ImmOperand *>(originalInsn->opnds[i]);
        ImmOperand *newIopnd = mp->Clone<AArch64ImmOperand>(*cloneImm);
        newInsn->opnds[i] = newIopnd;
      } else {
        newInsn->opnds[i] = originalInsn->opnds[i];
      }
    }
    return newInsn;
  } else if (dynamic_cast<mpldbg::DbgInsn *>(originalInsn)) {
    mpldbg::DbgInsn *tobeCloned = static_cast<mpldbg::DbgInsn *>(originalInsn);
    return mp->Clone<mpldbg::DbgInsn>(*tobeCloned);
  } else if (dynamic_cast<cfi::CfiInsn *>(originalInsn)) {
    cfi::CfiInsn *tobeCloned = static_cast<cfi::CfiInsn *>(originalInsn);
    return mp->Clone<cfi::CfiInsn>(*tobeCloned);
  } else {
    CG_ASSERT(false, "Cannot clone");
    return nullptr;
  }
}

/**
 * Precondition: The given insn is a jump instruction.
 * Get the jump target label from the given instruction.
 * Note: MOP_xbr is a branching instruction, but the target is unknown at compile time,
 * because a register instead of label. So we don't take it as a branching instruction.
 */
LabelIdx AArch64InsnVisitor::GetJumpLabel(Insn *insn) {
  int operandIdx = insn->GetJumpTargetIdx();

  if (dynamic_cast<LabelOperand *>(insn->opnds[operandIdx])) {
    return static_cast<LabelOperand *>(insn->opnds[operandIdx])->labidx_;
  }

  ASSERT(0, "Operand is not label");
  return 0;
}

bool AArch64InsnVisitor::IsConditionAlwaysHold(MOperator mop, int64 cmpRet) {
  const char *erroMsg = "Not conditional branch instruction";
  if (cmpRet > 0) {
    switch (mop) {
      case MOP_bmi:
      case MOP_bvc:
      case MOP_bls:
      case MOP_blt:
      case MOP_ble:
      case MOP_blo:
      case MOP_beq:
        return false;
      case MOP_bpl:
      case MOP_bhs:
      case MOP_bvs:
      case MOP_bhi:
      case MOP_bgt:
      case MOP_bge:
      case MOP_bal:
      case MOP_bne:
        return true;
      default:
        CG_ASSERT(false, "Not conditional branch instruction");
        return false;
    }
  } else if (cmpRet < 0) {
    switch (mop) {
      case MOP_bmi:
      case MOP_bvc:
      case MOP_bls:
      case MOP_blt:
      case MOP_ble:
      case MOP_blo:
      case MOP_bne:
      case MOP_bal:
        return true;
      case MOP_beq:
      case MOP_bpl:
      case MOP_bhs:
      case MOP_bvs:
      case MOP_bhi:
      case MOP_bgt:
      case MOP_bge:
        return false;
      default:
        CG_ASSERT(false, "Not conditional branch instruction");
        return false;
    }
  } else {
    switch (mop) {
      case MOP_ble:
      case MOP_bge:
      case MOP_bal:
      case MOP_beq:
      case MOP_bhs:
      case MOP_bls:
        return true;
      case MOP_bmi:
      case MOP_bvc:
      case MOP_blt:
      case MOP_blo:
      case MOP_bne:
      case MOP_bpl:
      case MOP_bvs:
      case MOP_bhi:
      case MOP_bgt:
        return false;
      default:
        CG_ASSERT(false, "Not conditional branch instruction");
        return false;
    }
  }
}

Operand *AArch64InsnVisitor::GetStrTarget(Insn *insn) {
  return static_cast<MemOperand *>(static_cast<AArch64Insn *>(insn)->opnds[1]);
}

Operand *AArch64InsnVisitor::GetStrSource(Insn *insn) {
  return static_cast<RegOperand *>(static_cast<AArch64Insn *>(insn)->opnds[0]);
}

Operand *AArch64InsnVisitor::GetLdrTarget(Insn *insn) {
  return static_cast<RegOperand *>(static_cast<AArch64Insn *>(insn)->opnds[0]);
}

Operand *AArch64InsnVisitor::GetLdrSource(Insn *insn) {
  return static_cast<MemOperand *>(static_cast<AArch64Insn *>(insn)->opnds[1]);
}

bool AArch64InsnVisitor::EqualMemOperands(Operand *op1, Operand *op2) {
  return static_cast<AArch64MemOperand *>(op1)->Equals(static_cast<AArch64MemOperand *>(op2));
}

bool AArch64InsnVisitor::IsCompareInsn(Insn *insn) {
  switch (insn->mop_) {
    case MOP_wcmpri:
    case MOP_wcmprr:
    case MOP_xcmpri:
    case MOP_xcmprr:
    case MOP_hcmperi:
    case MOP_hcmperr:
    case MOP_scmperi:
    case MOP_scmperr:
    case MOP_dcmperi:
    case MOP_dcmperr:
    case MOP_hcmpqri:
    case MOP_hcmpqrr:
    case MOP_scmpqri:
    case MOP_scmpqrr:
    case MOP_dcmpqri:
    case MOP_dcmpqrr:
    case MOP_wcmnri:
    case MOP_wcmnrr:
    case MOP_xcmnri:
    case MOP_xcmnrr:
      return true;
    default:
      return false;
  }
}

bool AArch64InsnVisitor::IsCompareAndBranchInsn(Insn *insn) {
  switch (insn->mop_) {
    case MOP_wcbnz:
    case MOP_xcbnz:
    case MOP_wcbz:
    case MOP_xcbz:
      return true;
    default:
      return false;
  }
}

bool AArch64InsnVisitor::ModifyInsnOpnds(Insn *insn, Operand *src, Operand *tar) {
  if (!insn) {
    return false;
  }
  if (insn->IsLoad() || insn->IsMove()) {
    if (src) {
      insn->opnds[1] = src;
    }
    if (tar) {
      insn->opnds[0] = tar;
    }
  } else if (insn->IsStore()) {
    if (src) {
      insn->opnds[0] = src;
    }
    if (tar) {
      insn->opnds[1] = tar;
    }
  }

  return true;
}

bool AArch64InsnVisitor::SyncRegs(Insn *lastMemAccessInsn, Insn *csel) {
  if (!lastMemAccessInsn || !csel) {
    return false;
  }
  switch (csel->mop_) {
    case MOP_wcselrrrc:
    case MOP_xcselrrrc:
    case MOP_scselrrrc:
    case MOP_dcselrrrc:
      break;
    default:
      return false;
  }
  lastMemAccessInsn->opnds[0] = csel->opnds[0];
  return true;
}

RegOperand *AArch64InsnVisitor::CreateVregFromReg(RegOperand *pReg) {
  return static_cast<AArch64CGFunc *>(GetCGFunc())
    ->CreateRegisterOperandOfType(pReg->GetRegisterType(), pReg->GetSize() / 8);
}

Insn *AArch64InsnVisitor::CreateMoveInsn(RegOperand *dest, RegOperand *src) {
  MOperator mop = (dest->GetSize() == 64 ? MOP_xmovrr : MOP_wmovrr);
  return GetCGFunc()->cg->BuildInstruction<AArch64Insn>(mop, dest, src);
}

Insn *AArch64InsnVisitor::CreateLdrInsn(MOperator branchMemOp, RegOperand *reg, MemOperand *mem) {
  MOperator mop;
  switch (branchMemOp) {
    case MOP_wstrh:
    case MOP_wldrh:
      mop = MOP_wldrh;
      break;
    case MOP_wstr:
    case MOP_wldr:
      mop = MOP_wldr;
      break;
    case MOP_xstr:
    case MOP_xldr:
      mop = MOP_xldr;
      break;
    case MOP_sstr:
    case MOP_sldr:
      mop = MOP_sldr;
      break;
    case MOP_dstr:
    case MOP_dldr:
      mop = MOP_dldr;
      break;
    case MOP_wstrb:
    case MOP_wldrb:
      mop = MOP_wldrb;
      break;

    default:
      mop = 0;
  }
  if (!mop) {
    return nullptr;
  } else {
    return GetCGFunc()->cg->BuildInstruction<AArch64Insn>(mop, reg, mem);
  }
}

Insn *AArch64InsnVisitor::CreateCondSelectionInsn(Insn *branchInsn, MOperator originalMop, Operand *ret, Operand *srcIf,
                                                  Operand *srcElse) {
  MOperator mop;
  switch (originalMop) {
    case MOP_wstrb:
    case MOP_wldrb:
    case MOP_wstrh:
    case MOP_wldrh:
    case MOP_wstr:
    case MOP_wldr:
      mop = MOP_wcselrrrc;
      break;
    case MOP_xstr:
    case MOP_xldr:
      mop = MOP_xcselrrrc;
      break;
    case MOP_sstr:
    case MOP_sldr:
      mop = MOP_scselrrrc;
      break;
    case MOP_dstr:
    case MOP_dldr:
      mop = MOP_dcselrrrc;
      break;
    default:
      mop = 0;
  }
  if (mop == 0) {
    return nullptr;
  } else {
    AArch64CC_t ccCode = Encode(branchInsn->mop_, false);
    if (ccCode != kCcLast) {
      CondOperand *cond = static_cast<AArch64CGFunc *>(GetCGFunc())->GetCondOperand(ccCode);
      return GetCGFunc()->cg->BuildInstruction<AArch64Insn>(mop, ret, srcIf, srcElse, cond);
    }
    return nullptr;
  }
}

Insn *AArch64InsnVisitor::CreateCmpInsn(Insn *condbr) {
  AArch64CGFunc *f = static_cast<AArch64CGFunc *>(GetCGFunc());
  RegOperand *reg = static_cast<RegOperand *>(condbr->GetOperand(0));
  PrimType primType = (reg->GetSize() == 64) ? PTY_u64 : PTY_u32;
  ImmOperand *numZero = f->CreateImmOperand(primType, 0);
  Operand *rflag = f->GetOrCreateRflag();
  MOperator mopcode = (reg->GetSize() == 64) ? MOP_xcmpri : MOP_wcmpri;
  Insn *cmpinsn = f->cg->BuildInstruction<AArch64Insn>(mopcode, rflag, reg, numZero);
  return cmpinsn;
}

AArch64CC_t AArch64InsnVisitor::Encode(MOperator mop, bool inverse) {
  AArch64CC_t ccCode;
  switch (mop) {
    case MOP_bmi:
      ccCode = inverse ? CC_PL : CC_MI;
      break;
    case MOP_bvc:
      ccCode = inverse ? CC_VS : CC_VC;
      break;
    case MOP_bls:
      ccCode = inverse ? CC_HI : CC_LS;
      break;
    case MOP_blt:
      ccCode = inverse ? CC_GE : CC_LT;
      break;
    case MOP_ble:
      ccCode = inverse ? CC_GT : CC_LE;
      break;
    case MOP_beq:
      ccCode = inverse ? CC_NE : CC_EQ;
      break;
    case MOP_bne:
      ccCode = inverse ? CC_EQ : CC_NE;
      break;
    case MOP_blo:
      ccCode = inverse ? CC_HS : CC_LO;
      break;
    case MOP_bpl:
      ccCode = inverse ? CC_MI : CC_PL;
      break;
    case MOP_bhs:
      ccCode = inverse ? CC_LO : CC_HS;
      break;
    case MOP_bvs:
      ccCode = inverse ? CC_VC : CC_VS;
      break;
    case MOP_bhi:
      ccCode = inverse ? CC_LS : CC_HI;
      break;
    case MOP_bgt:
      ccCode = inverse ? CC_LE : CC_GT;
      break;
    case MOP_bge:
      ccCode = inverse ? CC_LT : CC_GE;
      break;
    case MOP_bal:
      ccCode = inverse ? kCcLast : CC_AL;
      break;
    case MOP_wcbnz:
      ccCode = inverse ? CC_EQ : CC_NE;
      break;
    case MOP_xcbnz:
      ccCode = inverse ? CC_EQ : CC_NE;
      break;
    case MOP_wcbz:
      ccCode = inverse ? CC_NE : CC_EQ;
      break;
    case MOP_xcbz:
      ccCode = inverse ? CC_NE : CC_EQ;
      break;
    default:
      ccCode = kCcLast;
      break;
  }
  return ccCode;
}

bool AArch64InsnVisitor::CanDoIco(Insn *branch) {
  AArch64CC_t ccCode = Encode(branch->mop_, false);
  return ccCode != kCcLast;
}

Insn *AArch64InsnVisitor::BuildFmoveZero(RegOperand *dst, uint32 dsize) {
  MOperator mop = dsize == 64 ? MOP_xvmovdr : MOP_xvmovsr;
  AArch64CGFunc *f = static_cast<AArch64CGFunc *>(GetCGFunc());
  RegOperand *zero = f->GetOrCreatePhysicalRegisterOperand(RZR, dsize, kRegTyInt);
  return GetCGFunc()->cg->BuildInstruction<AArch64Insn>(mop, dst, zero);
}

Insn *AArch64InsnVisitor::BuildCondSet(Insn *branch, RegOperand *reg, bool inverse) {
  AArch64CC_t ccCode = Encode(branch->mop_, inverse);

  if (ccCode != kCcLast) {
    AArch64CGFunc *f = static_cast<AArch64CGFunc *>(GetCGFunc());
    CondOperand *cond = f->GetCondOperand(ccCode);
    MOperator mopcode = (reg->GetSize() == 64) ? MOP_xcsetrc : MOP_wcsetrc;
    return f->cg->BuildInstruction<AArch64Insn>(mopcode, reg, cond);
  }
  return nullptr;
}

Insn *AArch64InsnVisitor::BuildCondSel(Insn *branch, MOperator mop, RegOperand *dst, RegOperand *src1,
                                       RegOperand *src2) {
  AArch64CC_t ccCode = Encode(branch->mop_, false);
  if (ccCode != kCcLast) {
    CondOperand *cond = static_cast<AArch64CGFunc *>(GetCGFunc())->GetCondOperand(ccCode);
    return GetCGFunc()->cg->BuildInstruction<AArch64Insn>(mop, dst, src1, src2, cond);
  }
  return nullptr;
}

}  // namespace maplebe
