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

#include "riscv64_optimize_common.h"
#include "riscv64_isa.h"
#include "riscv64_cg_func.h"
#include "cg_bb.h"
#include "dbg.h"

namespace maplebe {

bool Riscv64InsnVisitor::ModifyBrInsn(maple::LabelIdx targetLabel, BB *&curbb) {
  int targetIdx = curbb->lastinsn->GetJumpTargetIdx();
  MOperator flippedOp;
  try {
    flippedOp = FlipConditionOp(curbb->lastinsn->mop_, targetIdx);
  } catch (...) {
    return false;
  }
  LabelOperand *targetOperand = static_cast<Riscv64CGFunc *>(GetCGFunc())->GetOrCreateLabelOperand(targetLabel);
  curbb->lastinsn->mop_ = flippedOp;
  curbb->lastinsn->SetOperand(targetIdx, targetOperand);
  return true;
}

MOperator Riscv64InsnVisitor::FlipConditionOp(MOperator originalOp, int &targetIdx) {
  MOperator flippedOp = 0;
  targetIdx = 2;
  switch (originalOp) {
    case Riscv64MOP_t::MOP_beq:
      flippedOp = Riscv64MOP_t::MOP_bne;
      break;
    case Riscv64MOP_t::MOP_bne:
      flippedOp = Riscv64MOP_t::MOP_beq;
      break;
    case Riscv64MOP_t::MOP_blt:
      flippedOp = Riscv64MOP_t::MOP_bge;
      break;
    case Riscv64MOP_t::MOP_ble:
      flippedOp = Riscv64MOP_t::MOP_bgt;
      break;
    case Riscv64MOP_t::MOP_bgt:
      flippedOp = Riscv64MOP_t::MOP_ble;
      break;
    case Riscv64MOP_t::MOP_bge:
      flippedOp = Riscv64MOP_t::MOP_blt;
      break;
    case Riscv64MOP_t::MOP_blo:
      flippedOp = Riscv64MOP_t::MOP_bhs;
      break;
    case Riscv64MOP_t::MOP_bls:
      flippedOp = Riscv64MOP_t::MOP_bhi;
      break;
    case Riscv64MOP_t::MOP_bhs:
      flippedOp = Riscv64MOP_t::MOP_blo;
      break;
    case Riscv64MOP_t::MOP_bhi:
      flippedOp = Riscv64MOP_t::MOP_bls;
      break;
    default:
      // Don't need to flip for always
      break;
  }
  return flippedOp;
}

void Riscv64InsnVisitor::ModifyJumpTarget(Operand *targetOperand, BB *&bb) {
  if (bb->GetKind() == BB::kBBIgoto) {
    bool modified = false;
    for (Insn *insn = bb->lastinsn; insn != nullptr; insn = insn->prev) {
      if (insn->GetMachineOpcode() == MOP_adrp_label ||
          insn->GetMachineOpcode() == MOP_laddr) {
        maple::LabelIdx labidx = static_cast<LabelOperand *>(targetOperand)->labidx_;
        ImmOperand *immopnd = static_cast<Riscv64CGFunc *>(GetCGFunc())->CreateImmOperand(labidx, 8, false);
        insn->SetOperand(1, immopnd);
        modified = true;
        break;
      }
    }
    CHECK_FATAL(modified, "ModifyJumpTarget: Could not change jump target");
    return;
  } else if (bb->GetKind() == BB::kBBGoto) {
    for (Insn *insn = bb->lastinsn; insn != nullptr; insn = insn->prev) {
      if (insn->GetMachineOpcode() == MOP_laddr) {
        maple::LabelIdx labidx = static_cast<LabelOperand *>(targetOperand)->labidx_;
        ImmOperand *immopnd = static_cast<Riscv64CGFunc *>(GetCGFunc())->CreateImmOperand(labidx, 8, false);
        insn->SetOperand(1, immopnd);
        break;
      }
    }
  }
  int targetIdx = bb->lastinsn->GetJumpTargetIdx();
  bb->lastinsn->SetOperand(targetIdx, targetOperand);
}

void Riscv64InsnVisitor::ModifyJumpTarget(maple::LabelIdx targetLabel, BB *&bb) {
  LabelOperand *targetOperand = static_cast<Riscv64CGFunc *>(GetCGFunc())->GetOrCreateLabelOperand(targetLabel);
  ModifyJumpTarget(targetOperand, bb);
}

void Riscv64InsnVisitor::ModifyJumpTarget(BB *newTarget, BB *&bb) {
  int targetIdx = newTarget->lastinsn->GetJumpTargetIdx();
  Operand *targetOperand = newTarget->lastinsn->GetOperand(targetIdx);
  ModifyJumpTarget(targetOperand, bb);
}

Insn *Riscv64InsnVisitor::CreateGotoInsn(Insn *condBrInsn) {
  int targetIdx = condBrInsn->GetJumpTargetIdx();
  Operand *target = condBrInsn->opnds[targetIdx];
  return new Riscv64Insn(MOP_xuncond, target);
}

bool Riscv64InsnVisitor::IsConditionAlwaysHold(Insn *cmpInsn, Insn *conditionBrInsn, Operand *operands[]) {
  switch (cmpInsn->mop_) {
    case MOP_wcmpri:
    case MOP_wcmprr:
    case MOP_xcmpri:
    case MOP_xcmprr: {
      Riscv64ImmOperand *operand0 = static_cast<Riscv64ImmOperand *>(operands[0]);
      Riscv64ImmOperand *operand1 = static_cast<Riscv64ImmOperand *>(operands[1]);

      int64 result = operand0->GetValue() - operand1->GetValue();
      return IsConditionAlwaysHold(conditionBrInsn->mop_, result);
    }
    default:
      return false;
  }
}

Insn *Riscv64InsnVisitor::CloneInsn(Insn *originalInsn) {
  MemPool *mp = CG::curCgFunc->memPool;
  if (dynamic_cast<Riscv64Insn *>(originalInsn)) {
    // Cannot just plain duplicate the memory, as operands are shared
    // across all instructions.  Example - operand for vreg 200 is
    // pointed to the same vreg:200 object.
    // Further, operand objects are tracked in tables.  Duplicating
    // memory will lose track of these objects.
    // For now, replace the operands, take care of leaks later.
    Riscv64Insn *tobeCloned = static_cast<Riscv64Insn *>(originalInsn);
    Insn *newInsn = mp->Clone<Riscv64Insn>(*tobeCloned);
    for (uint32 i = 0; i < Insn::kMaxOperandNum; i++) {
      ImmOperand *iopnd = dynamic_cast<ImmOperand *>(originalInsn->opnds[i]);
      if (iopnd && iopnd->IsVary()) {
        Riscv64ImmOperand *cloneImm = static_cast<Riscv64ImmOperand *>(originalInsn->opnds[i]);
        ImmOperand *newIopnd = mp->Clone<Riscv64ImmOperand>(*cloneImm);
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
LabelIdx Riscv64InsnVisitor::GetJumpLabel(Insn *insn) {
  int operandIdx = insn->GetJumpTargetIdx();

  if (dynamic_cast<LabelOperand *>(insn->opnds[operandIdx])) {
    return static_cast<LabelOperand *>(insn->opnds[operandIdx])->labidx_;
  }

  ASSERT(0, "Operand is not label");
  return 0;
}

bool Riscv64InsnVisitor::IsConditionAlwaysHold(MOperator mop, int64 cmpRet) {
  const char *erroMsg = "Not conditional branch instruction";
  if (cmpRet > 0) {
    switch (mop) {
      case MOP_bls:
      case MOP_blt:
      case MOP_ble:
      case MOP_blo:
      case MOP_beq:
        return false;
      case MOP_bpl:
      case MOP_bhs:
      case MOP_bhi:
      case MOP_bgt:
      case MOP_bge:
      case MOP_bne:
        return true;
      default:
        CG_ASSERT(false, "Not conditional branch instruction");
        return false;
    }
  } else if (cmpRet < 0) {
    switch (mop) {
      case MOP_bls:
      case MOP_blt:
      case MOP_ble:
      case MOP_blo:
      case MOP_bne:
        return true;
      case MOP_beq:
      case MOP_bpl:
      case MOP_bhs:
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
      case MOP_beq:
      case MOP_bhs:
      case MOP_bls:
        return true;
      case MOP_blt:
      case MOP_blo:
      case MOP_bne:
      case MOP_bpl:
      case MOP_bhi:
      case MOP_bgt:
        return false;
      default:
        CG_ASSERT(false, "Not conditional branch instruction");
        return false;
    }
  }
}

Operand *Riscv64InsnVisitor::GetStrTarget(Insn *insn) {
  return static_cast<MemOperand *>(static_cast<Riscv64Insn *>(insn)->opnds[1]);
}

Operand *Riscv64InsnVisitor::GetStrSource(Insn *insn) {
  return static_cast<RegOperand *>(static_cast<Riscv64Insn *>(insn)->opnds[0]);
}

Operand *Riscv64InsnVisitor::GetLdrTarget(Insn *insn) {
  return static_cast<RegOperand *>(static_cast<Riscv64Insn *>(insn)->opnds[0]);
}

Operand *Riscv64InsnVisitor::GetLdrSource(Insn *insn) {
  return static_cast<MemOperand *>(static_cast<Riscv64Insn *>(insn)->opnds[1]);
}

bool Riscv64InsnVisitor::EqualMemOperands(Operand *op1, Operand *op2) {
  return static_cast<Riscv64MemOperand *>(op1)->Equals(static_cast<Riscv64MemOperand *>(op2));
}

bool Riscv64InsnVisitor::IsCompareInsn(Insn *insn) {
  switch (insn->mop_) {
    case MOP_wcmpri:
    case MOP_wcmprr:
    case MOP_xcmpri:
    case MOP_xcmprr:
    case MOP_scmperi:
    case MOP_scmperr:
    case MOP_dcmperi:
    case MOP_dcmperr:
    case MOP_scmpqri:
    case MOP_scmpqrr:
    case MOP_dcmpqri:
    case MOP_dcmpqrr:
      return true;
    default:
      return false;
  }
}

bool Riscv64InsnVisitor::IsCompareAndBranchInsn(Insn *insn) {
  switch (insn->mop_) {
    case MOP_beq:
    case MOP_bne:
    case MOP_blt:
    case MOP_ble:
    case MOP_bgt:
    case MOP_bge:
    case MOP_beqz:
    case MOP_bnez:
    case MOP_bltz:
    case MOP_blez:
    case MOP_bgtz:
    case MOP_bgez:
      return true;
    default:
      return false;
  }
}

bool Riscv64InsnVisitor::ModifyInsnOpnds(Insn *insn, Operand *src, Operand *tar) {
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

RegOperand *Riscv64InsnVisitor::CreateVregFromReg(RegOperand *pReg) {
  return static_cast<Riscv64CGFunc *>(GetCGFunc())
    ->CreateRegisterOperandOfType(pReg->GetRegisterType(), pReg->GetSize() / 8);
}

Insn *Riscv64InsnVisitor::CreateMoveInsn(RegOperand *dest, RegOperand *src) {
  MOperator mop = (dest->GetSize() == 64 ? MOP_xmovrr : MOP_wmovrr);
  return GetCGFunc()->cg->BuildInstruction<Riscv64Insn>(mop, dest, src);
}

Insn *Riscv64InsnVisitor::CreateLdrInsn(MOperator branchMemOp, RegOperand *reg, MemOperand *mem) {
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
    return GetCGFunc()->cg->BuildInstruction<Riscv64Insn>(mop, reg, mem);
  }
}

Insn *Riscv64InsnVisitor::CreateCmpInsn(Insn *condbr) {
  Riscv64CGFunc *f = static_cast<Riscv64CGFunc *>(GetCGFunc());
  RegOperand *reg = static_cast<RegOperand *>(condbr->GetOperand(0));
  PrimType primType = (reg->GetSize() == 64) ? PTY_u64 : PTY_u32;
  ImmOperand *numZero = f->CreateImmOperand(primType, 0);
  MOperator mopcode = (reg->GetSize() == 64) ? MOP_xcmpri : MOP_wcmpri;
  Insn *cmpinsn = f->cg->BuildInstruction<Riscv64Insn>(mopcode, reg, numZero);
  return cmpinsn;
}

}  // namespace maplebe
