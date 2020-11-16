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

#include "cfg_optimizer.h"
#include "cg_bb.h"
#include "cg.h"
#if TARGAARCH64
#include "aarch64_insn.h"
#include "aarch64_operand.h"
#elif TARGRISCV64
#include "riscv64_insn.h"
#include "riscv64_operand.h"
#endif
#include "cg_assert.h"
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <stdio.h>
#include <string.h>

// This phase traverses all basic block of cgfunc and finds special
// basic block patterns, like continuous fallthrough basic block, continuous
// uncondition jump basic block, unreachable basic block and empty basic block,
// then do basic mergering, basic block placement transformations,
// unnecessary jumps elimination, and remove unreachable or empty basic block.
// This optimization is done on control flow graph basis.

namespace maplebe {

using namespace maple;

#define CFGODUMP CGDEBUGFUNC(cgfunc)

void CFGOptimizer::InitOptimizePatterns() {
  // Initialize cfg optimization patterns
  diffPassPatterns.push_back(memPool->New<ChainingPattern>(cgfunc));
  diffPassPatterns.push_back(memPool->New<SequentialJumpPattern>(cgfunc));
  diffPassPatterns.push_back(memPool->New<FlipBRPattern>(cgfunc));
  diffPassPatterns.push_back(memPool->New<DuplicateBBPattern>(cgfunc));
  diffPassPatterns.push_back(memPool->New<UnreachBBPattern>(cgfunc));
  diffPassPatterns.push_back(memPool->New<EmptyBBPattern>(cgfunc));
}

// return true if to is put after from and there is no real insns between from and to,
bool ChainingPattern::NoInsnBetween(BB *from, BB *to) {
  BB *bb = nullptr;
  for (bb = from->next; bb && bb != to && bb != cgfunc->lastbb; bb = bb->next) {
    if (!cgfunc->theCFG->IsEmptyOrCommentOnly(bb) || bb->unreachable || bb->GetKind() != BB::kBBFallthru) {
      return false;
    }
  }
  if (bb == to) {
    return true;
  }
  return false;
}

// return true if insns in bb1 and bb2 are the same except the last goto insn.
bool ChainingPattern::DoSameThing(BB *bb1, Insn *last1, BB *bb2, Insn *last2) {
#if TARGAARCH64 || TARGRISCV64
  Insn *insn1 = bb1->firstinsn;
  Insn *insn2 = bb2->firstinsn;
  while (insn1 && insn1 != last1->next && insn2 && insn2 != last2->next) {
    if (!insn1->IsMachineInstruction()) {
      insn1 = insn1->next;
      continue;
    }
    if (!insn2->IsMachineInstruction()) {
      insn2 = insn2->next;
      continue;
    }
    if (insn1->GetMachineOpcode() != insn2->GetMachineOpcode()) {
      return false;
    }
#if TARGAARCH64
    for (int i = 0; i < AArch64Insn::kMaxOperandNum; i++) {
#elif TARGRISCV64
    for (int i = 0; i < Riscv64Insn::kMaxOperandNum; i++) {
#endif
      Operand *op1 = insn1->GetOperand(i);
      Operand *op2 = insn2->GetOperand(i);
      if (op1 == op2) {
        continue;
      }
      CG_ASSERT(op1 && op2, "same mop should has same numbers of operands");
      if (!op1->Equals(op2)) {
        return false;
      }
    }
    insn1 = insn1->next;
    insn2 = insn2->next;
  }
  if (insn1 == last1->next && insn2 == last2->next) {
    return true;
  }
#endif
  return false;
}

// Following optimizations are performed:
// 1. Basic block merging
// 2. unnecessary jumps elimination
// 3. Remove duplicates Basic block.
bool ChainingPattern::Optimize(BB *&curbb) {
#if TARGAARCH64 || TARGRISCV64
  if (curbb->GetKind() == BB::kBBFallthru) {
    /*BB2 can be merged into BB1, if
     *   1. BB1's kind is fallthrough;
     *   2. BB2 has only one predecessor which is BB1 and BB2 is not the lastbb
     *   3. BB2 is neither catch BB nor switch case BB
     *
     */
    BB *sucBB = curbb->next;
    if (sucBB && sucBB != cgfunc->lastbb && !cgfunc->theCFG->InLSDA(sucBB->labidx, cgfunc->ehfunc) &&
        !cgfunc->theCFG->InSwitchTable(sucBB->labidx, cgfunc) && cgfunc->theCFG->CanMerge(curbb, sucBB)) {
      Log(curbb->id);
      if (checkOnly) {
        return false;
      }
      cgfunc->theCFG->MergeBB(curbb, sucBB, cgfunc);
      keepPosition = true;
      return true;
    }
  }
  if (curbb->GetKind() == BB::kBBGoto && !curbb->IsEmpty()) {
    BB *sucBB = cgfunc->theCFG->GetTargetSuc(curbb);
    /*BB2 can be merged into BB1, if
     *   1. BB1 ends with a goto;
     *   2. BB2 has only one predecessor which is BB1
     *   3. BB2 is of goto kind. Otherwise, the original fall through will be broken
     *   4. BB2 is neither catch BB nor switch case BB
     *
     */
    if (sucBB == nullptr) {
      return false;
    }
    if (sucBB->GetKind() == BB::kBBGoto && !cgfunc->theCFG->InLSDA(sucBB->labidx, cgfunc->ehfunc) &&
        !cgfunc->theCFG->InSwitchTable(sucBB->labidx, cgfunc) && cgfunc->theCFG->CanMerge(curbb, sucBB)) {
      Log(curbb->id);
      if (checkOnly) {
        return false;
      }
      cgfunc->theCFG->MergeBB(curbb, sucBB, cgfunc);
      keepPosition = true;
      return true;
    } else if (sucBB != curbb && curbb->next != sucBB && !sucBB->IsPredecessor(sucBB->prev) &&
               !(sucBB->next && sucBB->next->IsPredecessor(sucBB)) &&
               !cgfunc->theCFG->InLSDA(sucBB->labidx, cgfunc->ehfunc) &&
               !cgfunc->theCFG->InSwitchTable(sucBB->labidx, cgfunc)
               && sucBB->GetKind() != BB::kBBThrow) {
      // without the judge below, there is
      // Assembler Error: CFI state restore without previous remember
      if (sucBB->firstinsn && sucBB->firstinsn->IsCfiInsn()) {
        return false;
      }
      Log(curbb->id);
      if (checkOnly) {
        return false;
      }
      // put sucBB as curBB's next.
      CG_ASSERT(sucBB->prev != nullptr, "the target of current goto BB will not be the first bb");
      sucBB->prev->next = sucBB->next;
      if (sucBB->next != nullptr) {
        sucBB->next->prev = sucBB->prev;
      }
      sucBB->next = curbb->next;
      CG_ASSERT(curbb->next != nullptr, "current goto BB will not be the last bb");
      curbb->next->prev = sucBB;
      sucBB->prev = curbb;
      curbb->next = sucBB;
      curbb->RemoveInsn(curbb->lastinsn);
      curbb->SetKind(BB::kBBFallthru);
      return true;
    }
    /**
     * Last goto instruction can be removed, if:
     *  1. The goto target is physically the next one to current BB.
     */
    else if (sucBB == curbb->next ||
             (NoInsnBetween(curbb, sucBB) && !cgfunc->theCFG->InLSDA(curbb->next->labidx, cgfunc->ehfunc) &&
              !cgfunc->theCFG->InSwitchTable(curbb->next->labidx, cgfunc))) {
      Log(curbb->id);
      if (checkOnly) {
        return false;
      }
      if (sucBB != curbb->next) {
        CG_ASSERT(curbb->next, "");
        curbb->succs.remove(sucBB);
        curbb->succs.push_back(curbb->next);
        curbb->next->preds.push_back(curbb);
        sucBB->preds.remove(curbb);
      }
      curbb->RemoveInsn(curbb->lastinsn);
      curbb->SetKind(BB::kBBFallthru);
      return true;
    }
    /* Clear curbb and target it to sucBB->prev
     *  if sucBB->prev and curbb's insns are the same.
     *
     * curbb:           curbb:
     *   insn_x0          b prevbb
     *   b sucBB        ...
     * ...         ==>  prevbb:
     * prevbb:            insn_x0
     *   insn_x0        sucBB:
     * sucBB:
     *
     */
    else if (sucBB != curbb->next && !cgfunc->theCFG->IsSoloGoto(curbb) &&
             !cgfunc->theCFG->InLSDA(curbb->labidx, cgfunc->ehfunc) &&
             !cgfunc->theCFG->InSwitchTable(curbb->labidx, cgfunc) && sucBB->GetKind() == BB::kBBReturn &&
             sucBB->preds.size() > 1 && sucBB->prev && sucBB->IsPredecessor(sucBB->prev) &&
             (sucBB->prev->GetKind() == BB::kBBFallthru || sucBB->prev->GetKind() == BB::kBBGoto)) {
      if (curbb->firstinsn && curbb->firstinsn->IsCfiInsn()) {
        return false;
      }
      Insn *brInsn = nullptr;
      for (brInsn = curbb->lastinsn; brInsn != curbb->firstinsn->prev; brInsn = brInsn->prev) {
        if (brInsn->mop_ == MOP_xuncond) {
          break;
        }
      }
      CG_ASSERT(brInsn, "goto BB has no branch");
      BB *newTarget = sucBB->prev;
      Insn *last1 = newTarget->lastinsn;
      if (newTarget->GetKind() == BB::kBBGoto) {
        Insn *br = nullptr;
        for (br = newTarget->lastinsn; br != newTarget->firstinsn->prev; br = brInsn->prev) {
          if (br->mop_ == MOP_xuncond) {
            break;
          }
        }
        last1 = br->prev;
        CG_ASSERT(br, "goto BB has no branch");
      }
      if (last1 == nullptr || !DoSameThing(newTarget, last1, curbb, brInsn->prev)) {
        return false;
      }

      Log(curbb->id);
      if (checkOnly) {
        return false;
      }

      LabelIdx tgtlabidx = newTarget->labidx;
      if (newTarget->labidx == MIRLabelTable::kDummyLabel) {
        tgtlabidx = cgfunc->CreateLabel();
        newTarget->AddLabel(tgtlabidx);
      }
      LabelOperand *brTarget = cgfunc->GetOrCreateLabelOperand(tgtlabidx);
      brInsn->SetOperand(0, brTarget);
      curbb->RemoveInsnSequence(curbb->firstinsn, brInsn->prev);

      curbb->RemoveFromSuccessorList(sucBB);
      curbb->succs.push_back(newTarget);
      sucBB->RemoveFromPredecessorList(curbb);
      newTarget->preds.push_back(curbb);

      sucBB->prev->unreachable = false;
      keepPosition = true;
      return true;
    }
  }
#endif
  return false;
}

/*
 * curbb:             curbb:
 *   insn_x0            insn_x0
 *   b targetBB         b BB
 * ...           ==>  ...
 * targetBB:          targetBB:
 *   b BB               b BB
 * ...                ...
 * BB:                BB:
 **------------------------------
 * curbb:             curbb:
 *   insn_x0            insn_x0
 *   cond_br brBB       cond_br BB
 * ...                ...
 * brBB:         ==>  brBB:
 *   b BB               b BB
 * ...                ...
 * BB:                BB:
 *
 * conditions:
 *   1. only goto and comment in brBB;
 */
bool SequentialJumpPattern::Optimize(BB *&curbb) {
  if (curbb->unreachable) {
    return false;
  }
  if (curbb->GetKind() == BB::kBBGoto && !curbb->IsEmpty()) {
    BB *sucBB = cgfunc->theCFG->GetTargetSuc(curbb);

    CHECK_FATAL(sucBB != nullptr, "sucBB is null in SequentialJumpPattern::Optimize");
    if (cgfunc->theCFG->IsSoloGoto(sucBB) && cgfunc->theCFG->GetTargetSuc(sucBB)) {
      Log(curbb->id);
      if (checkOnly) {
        return false;
      }
      cgfunc->theCFG->RetargetJump(sucBB, curbb);
      SkipSucBB(curbb, sucBB);
      return true;
    }
  } else if (curbb->GetKind() == BB::kBBIf) {
    for (BB *sucBB : curbb->succs) {
      if (sucBB != curbb->next && cgfunc->theCFG->IsSoloGoto(sucBB) && cgfunc->theCFG->GetTargetSuc(sucBB)) {
        Log(curbb->id);
        if (checkOnly) {
          return false;
        }
        cgfunc->theCFG->RetargetJump(sucBB, curbb);
        SkipSucBB(curbb, sucBB);
        break;
      }
    }
    return true;
  }
  return false;
}

/**
 * preCond:
 * sucBB is one of curBB's successor.
 *
 * Change curBB's successor to sucBB's successor
 */
void SequentialJumpPattern::SkipSucBB(BB *curBB, BB *sucBB) {
  BB *gotoTarget = cgfunc->theCFG->GetTargetSuc(sucBB);
  CHECK_FATAL(gotoTarget != nullptr, "gotoTarget is null in SequentialJumpPattern::SkipSucBB");
  curBB->succs.remove(sucBB);
  curBB->succs.push_back(gotoTarget);
  sucBB->preds.remove(curBB);
  gotoTarget->preds.push_back(curBB);
  cgfunc->theCFG->FlushUnReachableStatusAndRemoveRelations(sucBB, cgfunc);
}

/* Found pattern             (1) ftBB->preds.size() == 1
 * curbb:                      curbb: cond1_br target
 *       ...            ==>    brBB:
 *       cond_br brBB           ...
 * ftBB:                       targetBB: (ftBB,targetBB)
 *       goto target         (2) ftBB->preds.size() > 1
 * brBB:                       curbb : cond1_br ftBB
 *       ...                   brBB:
 * targetBB                      ...
 *                            ftBB
 *                            targetBB
 */
bool FlipBRPattern::Optimize(BB *&curbb) {
  if (curbb->GetKind() == BB::kBBIf && !curbb->IsEmpty()) {
    BB *ftBB = curbb->next;
    BB *brBB = cgfunc->theCFG->GetTargetSuc(curbb);
    CHECK_FATAL(brBB != nullptr, "brBB is null in  FlipBRPattern::Optimize");
    // Check if it can be optimized
    if (ftBB->GetKind() == BB::kBBGoto && ftBB->next == brBB/* &&
                                                               IsSoloGoto(ftBB) ftBB->preds.size() == 1*/) {
      Insn *cbrInsn = nullptr;
      for (cbrInsn = curbb->lastinsn; cbrInsn; cbrInsn = cbrInsn->prev) {
        if (cbrInsn->IsBranch()) {
          break;
        }
      }
      CG_ASSERT(cbrInsn, "FlipBRPattern: curbb has no branch");
      Insn *brInsn = nullptr;
      for (brInsn = ftBB->lastinsn; brInsn != ftBB->firstinsn->prev; brInsn = brInsn->prev) {
#if !TARGARK
        if (brInsn->mop_ == MOP_xuncond) {
          break;
        }
#endif
      }
      CG_ASSERT(brInsn, "FlipBRPattern: ftbb has no branch");

      // Reverse the branch
      int targetIdx = cbrInsn->GetJumpTargetIdx();
      MOperator mop = cgfunc->theCFG->GetInsnModifier()->FlipConditionOp(cbrInsn->mop_, targetIdx);
      if (mop == 0) {
        return false;
      }
      auto it = ftBB->succs.begin();
      BB *tgtBB = *it;
      if (ftBB->preds.size() == 1 &&
          (cgfunc->theCFG->IsSoloGoto(ftBB) ||
           (!cgfunc->theCFG->InLSDA(tgtBB->labidx, cgfunc->ehfunc) &&
            !cgfunc->theCFG->InSwitchTable(tgtBB->labidx, cgfunc) && cgfunc->theCFG->CanMerge(ftBB, tgtBB)))) {
        cbrInsn->mop_ = mop;
        Operand *brTarget = brInsn->GetOperand(brInsn->GetJumpTargetIdx());
        cbrInsn->SetOperand(targetIdx, brTarget);
        // Insert ftBB's insn at the beginning of tgtBB.
        if (!cgfunc->theCFG->IsSoloGoto(ftBB)) {
          ftBB->RemoveInsn(brInsn);
          tgtBB->InsertAtBeginning(ftBB);
        }
        // Patch pred and succ lists
        ftBB->succs.erase(it);
        ftBB->succs.push_back(brBB);
        it = curbb->succs.begin();
        if (*it == brBB) {
          curbb->succs.erase(it);
          curbb->succs.push_back(tgtBB);
        } else {
          it++;
          curbb->succs.erase(it);
          curbb->succs.push_front(tgtBB);
        }
        for (it = tgtBB->preds.begin(); it != tgtBB->preds.end(); it++) {
          if (*it == ftBB) {
            tgtBB->preds.erase(it);
            break;
          }
        }
        tgtBB->preds.push_back(curbb);
        for (it = brBB->preds.begin(); it != brBB->preds.end(); it++) {
          if (*it == curbb) {
            brBB->preds.erase(it);
            break;
          }
        }
        brBB->preds.push_front(ftBB);
        // Remove instructions from ftBB so curbb falls thru to brBB
        ftBB->firstinsn = nullptr;
        ftBB->lastinsn = nullptr;
        ftBB->SetKind(BB::kBBFallthru);
      } else if (!cgfunc->theCFG->InLSDA(ftBB->labidx, cgfunc->ehfunc) &&
                 !cgfunc->theCFG->InSwitchTable(ftBB->labidx, cgfunc) && !tgtBB->IsPredecessor(tgtBB->prev)) {
        cbrInsn->mop_ = mop;
        LabelIdx tgtlabidx = ftBB->labidx;
        if (ftBB->labidx == MIRLabelTable::kDummyLabel) {
          tgtlabidx = cgfunc->CreateLabel();
          ftBB->AddLabel(tgtlabidx);
        }
        LabelOperand *brTarget = cgfunc->GetOrCreateLabelOperand(tgtlabidx);
        cbrInsn->SetOperand(targetIdx, brTarget);
        curbb->next = brBB;
        brBB->prev = curbb;
        ftBB->prev = tgtBB->prev;
        tgtBB->prev->next = ftBB;
        ftBB->next = tgtBB;
        tgtBB->prev = ftBB;

        ftBB->RemoveInsn(brInsn);
        ftBB->SetKind(BB::kBBFallthru);
      }
    }
  }
  return false;
}

bool FlipBRPattern::NeedExtraGoto(BB *bb) {
  switch (bb->prev->GetKind()) {
    case BB::kBBGoto:
    case BB::kBBReturn:
      return false;
    case BB::kBBFallthru:
    case BB::kBBCall:
    case BB::kBBThrow:
    case BB::kBBIf:
    default:
      return true;
  }
}

bool FlipBRPattern::NoFallthrough(BB *bb) {
  BB::BBKind kind = bb->GetKind();
  if (kind == BB::kBBGoto || kind == BB::kBBReturn) {
    return true;
  } else {
    return false;
  }
}

// remove a basic block that contains nothing
bool EmptyBBPattern::Optimize(BB *&curbb) {
  if (curbb->unreachable) {
    return false;
  }
  // Empty bb but do not have cleanup label.
  if (curbb->prev != nullptr && curbb->firststmt != cgfunc->cleanup_label && curbb->firstinsn == nullptr &&
      curbb->lastinsn == nullptr && curbb != cgfunc->lastbb && curbb->GetKind() != BB::kBBReturn &&
      !cgfunc->theCFG->InLSDA(curbb->labidx, cgfunc->ehfunc) && !cgfunc->theCFG->InSwitchTable(curbb->labidx, cgfunc)) {
    Log(curbb->id);
    if (checkOnly) {
      return false;
    }
    CHECK_FATAL(cgfunc->theCFG->GetTargetSuc(curbb), "null ptr check");
    if (cgfunc->theCFG->GetTargetSuc(curbb)->firststmt == cgfunc->cleanup_label) {
      return false;
    }
    cgfunc->theCFG->RemoveBB(curbb);
    return true;
  }
  return false;
}

bool AlwaysPattern::Optimize(BB *&curbb) {
  return false;
}

void AlwaysPattern::NeverHold(Insn *cmpInsn) {}

void AlwaysPattern::AlwaysHold(Insn *cmpInsn) {}

bool AlwaysPattern::Optimize(Insn *cmpInsn, Operand *operands[]) {
  if (cmpInsn->next->IsBranch()) {
    InsnVisitor *insnM = cgfunc->theCFG->GetInsnModifier();
    try {
      BB *bb = cmpInsn->bb;
      if (insnM->IsConditionAlwaysHold(cmpInsn, cmpInsn->next, operands)) {
        Insn *gotoInsn = insnM->CreateGotoInsn(cmpInsn->next);
        CHECK_FATAL(gotoInsn != nullptr, "gotoInsn is null in AlwaysPattern::Optimize");
        bb->RemoveInsn(cmpInsn->next);
        bb->RemoveInsn(cmpInsn);
        bb->AppendInsn(gotoInsn);
        bb->SetKind(BB::kBBGoto);
        bb->succs.remove(bb->next);
        bb->next->preds.remove(bb);
      } else {
        bb->RemoveInsn(cmpInsn->next);
        bb->RemoveInsn(cmpInsn);
        bb->SetKind(BB::kBBFallthru);
        for (BB *suc : bb->succs) {
          if (suc != bb->next) {
            bb->succs.remove(suc);
            suc->preds.remove(bb);
          }
        }
      }
    } catch (...) {
      return false;
    }
  }
  return false;
}

/* remove unreachable BB
 * condition:
 *   1. unreachable BB can't have cfi instruction when postcfgo.
 */
bool UnreachBBPattern::Optimize(BB *&curbb) {
  if (cgfunc->theCFG->IsUnreachable(curbb, cgfunc)) {
    Log(curbb->id);
    if (checkOnly) {
      return false;
    }

    // if curbb in exitbbsvec,return false.
    EHFunc *ehfunc = cgfunc->ehfunc;
    for (int i = 0; i < cgfunc->exitbbsvec.size(); i++) {
      if (cgfunc->exitbbsvec.at(i) == curbb) {
        curbb->unreachable = false;
        return false;
      }
    }

    if (curbb && curbb->firstinsn && curbb->firstinsn->IsCfiInsn()) {
      return false;
    }

    // if curbb InLSDA ,replace curbb's label with nextreachablebb before remove it.
    if (ehfunc && ehfunc->NeedFullLSDA() && cgfunc->theCFG->InLSDA(curbb->labidx, ehfunc)) {
      // find nextreachablebb
      BB *nextreachablebb = nullptr;
      for (BB *bb = curbb; bb; bb = bb->next) {
        if (!bb->unreachable) {
          nextreachablebb = bb;
          break;
        }
      }
      if (nextreachablebb->labidx == 0) {
        LabelIdx labidx = cgfunc->CreateLabel();
        nextreachablebb->AddLabel(labidx);
        cgfunc->lab2bbmap[labidx] = nextreachablebb;
      }

      MapleVector<LSDACallSite *> &callsiteTable = ehfunc->lsda_callsite_table->callsite_table;
      for (uint32 i = 0; i < callsiteTable.size(); i++) {
        LSDACallSite *lsdacallsite = callsiteTable[i];

        if (lsdacallsite->cs_start.end_offset) {
          if (lsdacallsite->cs_start.end_offset->labelIdx == curbb->labidx) {
            lsdacallsite->cs_start.end_offset->labelIdx = nextreachablebb->labidx;
          }
        }

        if (lsdacallsite->cs_length.end_offset->labelIdx == curbb->labidx) {
          lsdacallsite->cs_length.end_offset->labelIdx = nextreachablebb->labidx;
        }

        if (lsdacallsite->cs_landing_pad.end_offset) {
          if (lsdacallsite->cs_landing_pad.end_offset->labelIdx == curbb->labidx) {
            lsdacallsite->cs_landing_pad.end_offset->labelIdx = nextreachablebb->labidx;
          }
        }
      }
    }

    curbb->prev->next = curbb->next;
    curbb->next->prev = curbb->prev;

    // flush after remove;
    for (BB *bb : curbb->succs) {
      bb->preds.remove(curbb);
      cgfunc->theCFG->FlushUnReachableStatusAndRemoveRelations(bb, cgfunc);
    }
    for (BB *bb : curbb->eh_succs) {
      bb->eh_preds.remove(curbb);
      cgfunc->theCFG->FlushUnReachableStatusAndRemoveRelations(bb, cgfunc);
    }
    curbb->succs.clear();
    curbb->eh_succs.clear();
    return true;
  }
  return false;
}

/* BB_pred1:        BB_pred1:
 *   b curbb          insn_x0
 * ...                b BB2
 * BB_pred2:   ==>  ...
 *   b curbb        BB_pred2:
 * ...                insn_x0
 * curbb:             b BB2
 *   insn_x0        ...
 *   b BB2          curbb:
 *                    insn_x0
 *                    b BB2
 * condition:
 *   1. The number of instruct in curbb
 *        is less than THRESHOLD;
 *   2. curbb can't have cfi instruction when postcfgo.
 */
bool DuplicateBBPattern::Optimize(BB *&curbb) {
  if (curbb->unreachable) {
    return false;
  }
  if (CGOptions::noDupBB == true) {
    return false;
  }
  /**
   * It is possible curbb jump to itself
   */
  int numPreds = curbb->NumPreds();
  for (BB *bb : curbb->preds) {
    if (bb == curbb) {
      numPreds--;
    }
  }
  if (curbb->GetKind() == BB::kBBGoto && numPreds > 1 && !cgfunc->theCFG->InLSDA(curbb->labidx, cgfunc->ehfunc) &&
      !cgfunc->theCFG->InSwitchTable(curbb->labidx, cgfunc) && cgfunc->theCFG->GetTargetSuc(curbb) != nullptr &&
      cgfunc->theCFG->GetTargetSuc(curbb)->NumPreds() > 1) {
    vector<BB *> candidates;
    for (BB *bb : curbb->preds) {
      if (bb->GetKind() == BB::kBBGoto && bb->next != curbb && bb != curbb && !bb->IsEmpty()) {
        candidates.push_back(bb);
      }
    }
    if (candidates.size() == 0) {
      return false;
    }
    if (curbb->NumInsn() <= THRESHOLD) {
      if (curbb->firstinsn && curbb->firstinsn->IsCfiInsn()) {
        return false;
      }
      Log(curbb->id);
      if (checkOnly) {
        return false;
      }
      for (BB *bb : candidates) {
        bb->RemoveInsn(bb->lastinsn);
        FOR_BB_INSNS(insn, curbb) {
          try {
            Insn *clonedInsn = cgfunc->theCFG->CloneInsn(insn);
            clonedInsn->prev = nullptr;
            clonedInsn->next = nullptr;
            clonedInsn->bb = nullptr;
            bb->AppendInsn(clonedInsn);
          } catch (...) {
            // Don't optimize if fail to duplicate the instructions
            return false;
          }
        }
        bb->succs.remove(curbb);
        for (BB *item : curbb->succs) {
          bb->succs.push_back(item);
          item->preds.push_back(bb);
        }
        curbb->preds.remove(bb);
      }
      cgfunc->theCFG->FlushUnReachableStatusAndRemoveRelations(curbb, cgfunc);
      return true;
    }
  }
  return false;
}

AnalysisResult *CgDoCfgo::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  MemPool *cfgmp = mempoolctrler.NewMemPool("cfgO");
  CFGOptimizer *cfgo = cfgmp->New<CFGOptimizer>(cgfunc, cfgmp);
  MIRFunction *mirFunc = cgfunc->func;
  string funcclass = mirFunc->GetBaseClassName();
  string funcname = mirFunc->GetBaseFuncName();
  string name = funcclass + funcname;
  std::string phaseName = PhaseName();

  if (CFGODUMP) {
    DotGenerator::GenerateDot("before-cfgo", cgfunc, &(cgfunc->mirModule));
  }

  cfgo->Run(name.c_str());

  if (CFGODUMP) {
    DotGenerator::GenerateDot("after-cfgo", cgfunc, &(cgfunc->mirModule));
  }

  mempoolctrler.DeleteMemPool(cfgmp);
  return nullptr;
}

AnalysisResult *CgDoPostCfgo::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  MemPool *cfgmp = mempoolctrler.NewMemPool("postcfgo");
  CFGOptimizer *cfgo = cfgmp->New<CFGOptimizer>(cgfunc, cfgmp);
  MIRFunction *mirFunc = cgfunc->func;
  string funcclass = mirFunc->GetBaseClassName();
  string funcname = mirFunc->GetBaseFuncName();
  string name = funcclass + funcname;
  std::string phaseName = PhaseName();

  if (CFGODUMP) {
    DotGenerator::GenerateDot("before-postcfgo", cgfunc, &(cgfunc->mirModule));
  }

  cfgo->Run(name.c_str());

  if (CFGODUMP) {
    DotGenerator::GenerateDot("after-postcfgo", cgfunc, &(cgfunc->mirModule));
  }

  mempoolctrler.DeleteMemPool(cfgmp);
  return nullptr;
}

}  // namespace maplebe
