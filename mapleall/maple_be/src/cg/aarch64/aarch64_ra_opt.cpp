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


#include "aarch64_ra_opt.h"

#include <iostream>

namespace maplebe {

using namespace std;

bool RaX0Opt::PropagateX0CanReplace(Operand *opnd, regno_t replaceReg) {
  if (opnd) {
    RegOperand *regopnd = static_cast<RegOperand *>(opnd);
    regno_t regCandidate = regopnd->GetRegisterNumber();
    if (regCandidate == replaceReg) {
      return true;
    }
  }
  return false;
}

/*
 * Replace replace_reg with rename_reg.
 * return true if there is a redefinition that needs to terminate the propagation.
 */
bool RaX0Opt::PropagateRenameReg(Insn *ninsn, X0OptInfo &optVal) {
  uint32 renameReg = static_cast<RegOperand *>(optVal.GetRenameOpnd())->GetRegisterNumber();
  const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(ninsn)->mop_];
  for (int32_t i = Insn::kMaxOperandNum - 1; i >= 0; i--) {
    Operand *opnd = ninsn->GetOperand(i);
    if (opnd == nullptr) {
      continue;
    }
    if (opnd->IsList()) {
      // call parameters
    } else if (opnd->IsMemoryAccessOperand()) {
      MemOperand *memopnd = static_cast<MemOperand *>(opnd);
      if (PropagateX0CanReplace(memopnd->GetBaseRegister(), optVal.GetReplaceReg())) {
        RegOperand *renameOpnd = static_cast<RegOperand *>(optVal.GetRenameOpnd());
        memopnd->SetBaseRegister(renameOpnd);
      }
      if (PropagateX0CanReplace(memopnd->GetIndexRegister(), optVal.GetReplaceReg())) {
        RegOperand *renameOpnd = static_cast<RegOperand *>(optVal.GetRenameOpnd());
        memopnd->SetIndexRegister(renameOpnd);
      }
    } else if (opnd->IsRegister()) {
      bool isdef = static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegDef();
      RegOperand *regopnd = static_cast<RegOperand *>(opnd);
      regno_t regCandidate = regopnd->GetRegisterNumber();
      if (isdef) {
        // Continue if both replace_reg & rename_reg are not redefined.
        if (regCandidate == optVal.GetReplaceReg() || regCandidate == renameReg) {
          return true;
        }
      } else {
        if (regCandidate == optVal.GetReplaceReg()) {
          ninsn->SetOperand(i, optVal.GetRenameOpnd());
        }
      }
    }
  }
  return false;  // false == no redefinition
}

bool RaX0Opt::PropagateX0DetectX0(Insn *insn, X0OptInfo &optVal) {
  /* Propagate x0 from a call return value to a def of x0.
   * This eliminates some local reloads under high register pressure, since
   * the use has been replaced by x0.
   */
  if (insn->GetMachineOpcode() != MOP_xmovrr && insn->GetMachineOpcode() != MOP_wmovrr) {
    return false;
  }
  RegOperand *movSrc = static_cast<RegOperand *>(insn->GetOperand(1));
  if (movSrc->GetRegisterNumber() != R0) {
    return false;
  }

  optVal.SetMovSrc(movSrc);
  return true;
}

bool RaX0Opt::PropagateX0DetectRedefine(const AArch64MD *md, Insn *ninsn, const X0OptInfo &optVal,
                                                       uint32 index) {
  bool isdef = static_cast<AArch64OpndProp *>(md->operand_[index])->IsRegDef();
  if (isdef) {
    RegOperand *opnd = static_cast<RegOperand *>(ninsn->GetOperand(index));
    if (opnd->GetRegisterNumber() == optVal.GetReplaceReg()) {
      return true;
    }
  }
  return false;
}

bool RaX0Opt::PropagateX0Optimize(const BB *bb, const Insn *insn, X0OptInfo &optVal) {
  bool redefined = false;
  for (Insn *ninsn = insn->next; ninsn && ninsn != bb->lastinsn->next; ninsn = ninsn->next) {
    if (!ninsn->IsMachineInstruction()) {
      continue;
    }

    // Will continue as long as the reg being replaced is not redefined.
    // Does not need to check for x0 redefinition.  The mov instruction src
    // being replaced already defines x0 and will terminate this loop.
    const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(ninsn)->mop_];
    for (uint32 i = 0; i < ninsn->GetResultNum(); i++) {
      redefined = PropagateX0DetectRedefine(md, ninsn, optVal, i);
      if (redefined) {
        break;
      }
    }

    // Look for move where src is the register equivalent to x0.
    if (ninsn->GetMachineOpcode() != MOP_xmovrr && ninsn->GetMachineOpcode() != MOP_wmovrr) {
      continue;
    }

    Operand *src = ninsn->GetOperand(1);
    RegOperand *srcreg = static_cast<RegOperand *>(src);
    if (srcreg->GetRegisterNumber() != optVal.GetReplaceReg()) {
      if (redefined) {
        break;
      }
      continue;
    }

    // Setup for the next optmization pattern.
    Operand *dst = ninsn->GetOperand(0);
    RegOperand *dstreg = static_cast<RegOperand *>(dst);
    if (dstreg->GetRegisterNumber() != R0) {
      // This is to set up for further propagation later.
      if (srcreg->GetRegisterNumber() == optVal.GetReplaceReg()) {
        if (optVal.GetRenameInsn() != nullptr) {
          redefined = true;
          break;
        } else {
          optVal.SetRenameInsn(ninsn);
          optVal.SetRenameOpnd(dst);
          optVal.SetRenameReg(dstreg->GetRegisterNumber());
        }
      }
      continue;
    }

    if (redefined) {
      break;
    }

    // x0 = x0
    ninsn->SetOperand(1, optVal.GetMovSrc());
    break;
  }

  return redefined;
}

bool RaX0Opt::PropagateX0ForCurrBb(BB *bb, X0OptInfo &optVal) {
  bool redefined = false;
  for (Insn *ninsn = optVal.GetRenameInsn()->next; ninsn && ninsn != bb->lastinsn->next; ninsn = ninsn->next) {
    if (!ninsn->IsMachineInstruction()) {
      continue;
    }
    redefined = PropagateRenameReg(ninsn, optVal);
    if (redefined) {
      break;
    }
  }
  if (redefined == false) {
    auto it = bb->liveout_regno.find(optVal.GetReplaceReg());
    if (it != bb->liveout_regno.end()) {
      bb->liveout_regno.erase(it);
    }
    uint32 renameReg = static_cast<RegOperand *>(optVal.GetRenameOpnd())->GetRegisterNumber();
    bb->liveout_regno.insert(renameReg);
  }
  return redefined;
}

void RaX0Opt::PropagateX0ForNextBb(BB *nextBb, X0OptInfo &optVal) {
  bool redefined = false;
  for (Insn *ninsn = nextBb->firstinsn; ninsn != nextBb->lastinsn->next; ninsn = ninsn->next) {
    if (!ninsn->IsMachineInstruction()) {
      continue;
    }
    redefined = PropagateRenameReg(ninsn, optVal);
    if (redefined) {
      break;
    }
  }
  if (redefined == false) {
    auto it = nextBb->livein_regno.find(optVal.GetReplaceReg());
    if (it != nextBb->livein_regno.end()) {
      nextBb->livein_regno.erase(it);
    }
    uint32 renameReg = static_cast<RegOperand *>(optVal.GetRenameOpnd())->GetRegisterNumber();
    nextBb->livein_regno.insert(renameReg);
  }
}

// Perform optimization.
// First propagate x0 in a bb.
// Second propagation see comment in function.
void RaX0Opt::PropagateX0(CGFunc *cgfunc) {
  FOR_ALL_BB(bb, cgfunc) {
    X0OptInfo optVal;

    Insn *insn = bb->firstinsn;
    while (insn && !insn->IsMachineInstruction()) {
      insn = insn->next;
      continue;
    }
    if (insn == nullptr) {
      continue;
    }
    if (PropagateX0DetectX0(insn, optVal) == false) {
      continue;
    }

    // At this point the 1st insn is a mov from x0.
    RegOperand *movDst = static_cast<RegOperand *>(insn->GetOperand(0));
    optVal.SetReplaceReg(movDst->GetRegisterNumber());

    optVal.ResetRenameInsn();
    bool redefined = PropagateX0Optimize(bb, insn, optVal);

    if (redefined || (optVal.GetRenameInsn() == nullptr)) {
      continue;
    }

    /* Next pattern to help LSRA.  Short cross bb live interval.
     *  Straight line code.  Convert reg2 into bb local.
     *  bb1
     *     mov  reg2 <- x0          =>   mov  reg2 <- x0
     *     mov  reg1 <- reg2             mov  reg1 <- reg2
     *     call                          call
     *  bb2  : livein< reg1 reg2 >
     *     use          reg2             use          reg1
     *     ....
     *     reg2 not liveout
     *
     * Can allocate caller register for reg2.
     */

    // Further propagation of very short live interval cross bb reg
    if (optVal.GetRenameReg() < kMaxRegNum) {  // dont propagate physical reg
      continue;
    }
    BB *nextBb = bb->next;
    if (nextBb == nullptr) {
      break;
    }
    if (bb->succs.size() != 1 || nextBb->preds.size() != 1) {
      continue;
    }
    if (bb->succs.front() != nextBb || nextBb->preds.front() != bb) {
      continue;
    }
    if (bb->liveout_regno.find(optVal.GetReplaceReg()) == bb->liveout_regno.end() ||
        bb->liveout_regno.find(optVal.GetRenameReg()) == bb->liveout_regno.end() ||
        nextBb->liveout_regno.find(optVal.GetReplaceReg()) != nextBb->liveout_regno.end()) {
      continue;
    }

    // Replace replace_reg by rename_reg.
    redefined = PropagateX0ForCurrBb(bb, optVal);

    if (redefined) {
      continue;
    }

    PropagateX0ForNextBb(nextBb, optVal);
  }
}

}  // namespace maplebe
