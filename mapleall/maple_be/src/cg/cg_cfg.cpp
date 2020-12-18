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

#include "cg_cfg.h"
#include "cg.h"
#if TARGAARCH64
#include "aarch64/aarch64_insn.h"
#include "aarch64/aarch64_operand.h"
#elif TARGRISCV64
#include "riscv64/riscv64_insn.h"
#include "riscv64/riscv64_operand.h"
#endif
#include "cg_option.h"
#include "securec.h"

namespace maplebe {

#define CLANG (cgfunc->func->module->IsCModule())

void CGCFG::BuildCFG() {
  // Second Pass:
  // Link preds/succs in the BBs
  BB *curbb = cgfunc->curbb;
  BB *firstbb = cgfunc->firstbb;
  MapleMap<LabelIdx, BB *> lab2bbmap = cgfunc->lab2bbmap;
  EHFunc *ehfunc = cgfunc->ehfunc;
  for (curbb = firstbb; curbb; curbb = curbb->next) {
    BB::BBKind k = curbb->kind;
    switch (k) {
      case BB::kBBIntrinsic:
        if (!curbb->lastinsn->IsBranch()) {
          break;
        }
      // else fall through
      case BB::kBBIf: {
        BB *fallthrubb = curbb->next;
        curbb->succs.push_back(fallthrubb);
        fallthrubb->preds.push_back(curbb);

        Insn *last = curbb->lastinsn;

        CHECK_FATAL(last->IsBranch(), "If BB has no cond br");
        // Assume the last non-null operand is the branch target
        int i = Insn::kMaxOperandNum - 1;
        while (last->GetOperand(i) == nullptr) {
          i--;
        }
        BB *brtobb = lab2bbmap[static_cast<LabelOperand *>(last->GetOperand(i))->labidx_];
        curbb->succs.push_back(brtobb);
        brtobb->preds.push_back(curbb);
        break;
      }
      case BB::kBBGoto: {
        BB *gotobb = nullptr;
        if (curbb->laststmt && dynamic_cast<GotoNode *>(curbb->laststmt)) {
          gotobb = lab2bbmap[static_cast<GotoNode *>(curbb->laststmt)->offset];
        } else {
          Insn *insn = curbb->lastinsn;
          // add assert here;
          LabelIdx labelid = static_cast<LabelOperand *>(insn->opnds[0])->labidx_;
          gotobb = lab2bbmap[labelid];
        }
        curbb->succs.push_back(gotobb);
        CHECK_FATAL(gotobb, "gotobb is null");
        gotobb->preds.push_back(curbb);
        break;
      }
      case BB::kBBIgoto: {
        for (auto lidx : cgfunc->func->labelTab->addrTakenLabels) {
          BB *igotobb = lab2bbmap[lidx];
          CHECK_FATAL(igotobb, "igotobb is null");
          curbb->succs.push_back(igotobb);
          igotobb->preds.push_back(curbb);
        }
        break;
      }
      case BB::kBBRangegoto: {
        RangegotoNode *rn = static_cast<RangegotoNode *>(curbb->laststmt);
        SmallCaseVector &switchTable = rn->rangegotoTable;
        for (uint32 i = 0; i < switchTable.size(); i++) {
          LabelIdx lidx = switchTable[i].second;
          BB *gotobb = lab2bbmap[lidx];
          curbb->succs.push_back(gotobb);
          gotobb->preds.push_back(curbb);
        }
        break;
      }
      case BB::kBBThrow:
        break;
      case BB::kBBCall:
      case BB::kBBFallthru: {
        BB *fallthrubb = curbb->next;
        if (fallthrubb) {
          curbb->succs.push_back(fallthrubb);
          fallthrubb->preds.push_back(curbb);
        }
        if (k == BB::kBBCall) {
          Insn *insnLast = curbb->lastinsn;
          CG_ASSERT((insnLast && insnLast->IsCall()), "BB_call should end with call insn.");
          curbb->callInsns.push_back(insnLast);
        }
        break;
      }
      default:
        break;
    }  // end switch

    // Check exception table. If curbb is in a try block, add catch BB to its succs
    if (ehfunc && ehfunc->lsda_callsite_table) {
      // Determine if insn in bb can actually except
      if (cgfunc->CanBBThrow(curbb)) {
        MapleVector<LSDACallSite *> &callsiteTable = ehfunc->lsda_callsite_table->callsite_table;
        for (uint32 i = 0; i < callsiteTable.size(); i++) {
          LSDACallSite *lsdacallsite = callsiteTable[i];
          BB *endTry = lab2bbmap[lsdacallsite->cs_length.end_offset->labelIdx];
          BB *startTry = lab2bbmap[lsdacallsite->cs_length.start_offset->labelIdx];
          if (curbb->id >= startTry->id && curbb->id <= endTry->id && lsdacallsite->cs_landing_pad.end_offset) {
            BB *landingPad = lab2bbmap[lsdacallsite->cs_landing_pad.end_offset->labelIdx];
            curbb->eh_succs.push_back(landingPad);
            landingPad->eh_preds.push_back(curbb);
          }
        }
      }
    }
  }
}  // CGCFG::BuildCFG;

InsnVisitor *CGCFG::insnVisitor;

void CGCFG::InitInsnVisitor(CGFunc *cgfunc, MemPool *mp) {
  insnVisitor = cgfunc->cg->NewInsnModifier(cgfunc, mp);
}

Insn *CGCFG::CloneInsn(Insn *originalInsn) {
  cgfunc->IncTotalNumberOfInstructions();
  return insnVisitor->CloneInsn(originalInsn);
}

RegOperand *CGCFG::CreateVregFromReg(RegOperand *pReg) {
  return insnVisitor->CreateVregFromReg(pReg);
}

// return true if there is only goto insn in bb.
bool CGCFG::IsSoloGoto(BB *bb) {
  if (bb->GetKind() != BB::kBBGoto) {
    return false;
  }
  for (Insn *insn = bb->firstinsn; insn; insn = insn->next) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }
#if !TARGARK
    if (insn->mop_ != MOP_xuncond) {
      return false;
    } else {
      return true;
    }
#endif
    if (insn == bb->lastinsn) {
      break;
    }
  }
  return false;
}

// return true if bb has no real insns.
bool CGCFG::IsEmptyOrCommentOnly(BB *bb) {
  if (bb->IsEmpty()) {
    return true;
  }
  if (bb->GetKind() != BB::kBBFallthru) {
    return false;
  }
  for (Insn *insn = bb->firstinsn; insn && insn != bb->lastinsn->next; insn = insn->next) {
    if (insn->IsMachineInstruction()) {
      return false;
    }
  }
  return true;
}

/* return true if:
 * mergee has only one predecessor which is merger, or mergee has
 * other comments only predecessors.
 * mergee can't have cfi instruction when postcfgo.
 */
bool CGCFG::BBJudge(BB *first, BB *second) {
  if (!first || !second || first->GetKind() == BB::kBBReturn || second->GetKind() == BB::kBBReturn) {
    return false;
  }
  if (second->preds.size() == 1 && second->preds.front() == first) {
    return true;
  }
  for (BB *bb : second->preds) {
    if (bb != first && !AreCommentAllPreds(bb)) {
      return false;
    }
  }
  return true;
}

bool CGCFG::CanICO(BB *cmpBB, BB *branchBB) {
  return BBJudge(cmpBB, branchBB) && insnVisitor->CanDoIco(cmpBB->lastinsn);
}

/**
 * Check if a given BB mergee can be merged into BB merger.
 * Returns true if:
   1. mergee has only one predecessor which is merger, or mergee has
 *   other comments only predecessors.
   2. merge has only one successor which is mergee.
   3. mergee can't have cfi instruction when postcfgo.
 */
bool CGCFG::CanMerge(BB *merger, BB *mergee) {
  bool base = BBJudge(merger, mergee);
  if (!base) {
    return false;
  }
  if (mergee->firstinsn && mergee->firstinsn->IsCfiInsn()) {
    return false;
  }
  return (merger->succs.size() == 1 && merger->succs.front() == mergee);
}

int CGCFG::NumStore(BB *bb) {
  if (!bb) {
    return 0;
  }
  int size = 0;
  for (Insn *insn = bb->firstinsn; insn && insn != bb->lastinsn->next; insn = insn->next) {
    if (insn->IsStore()) {
      size++;
    }
  }
  return size;
}

/**
 * Check if bb2 is next to bb1. When ignoreComment is set true,
 * ignore the possible comment BBs in between.
 */
bool CGCFG::IsNext(BB *bb1, BB *bb2, bool ignoreComment) {
  if (!bb1 && !bb2) {
    return false;
  }
  CHECK_FATAL(bb1 != nullptr, "bb is null in CGCFG::IsNext");
  if (!ignoreComment) {
    return bb1->next == bb2;
  } else {
    BB *bb = bb1->next;
    while (bb) {
      if (bb == bb2) {
        return true;
      } else if (bb != bb2 && !CGCFG::IsCommentBB(bb)) {
        return false;
      }
      bb = bb->next;
    }
    return false;
  }
}

/**
 * Check if the given BB contains only comments and all its predecessors are comments
 */
bool CGCFG::AreCommentAllPreds(BB *bb) {
  if (!IsCommentBB(bb)) {
    return false;
  }
  for (BB *pred : bb->preds) {
    if (!AreCommentAllPreds(pred)) {
      return false;
    }
  }
  return true;
}

bool CGCFG::IsCommentBB(BB *bb) {
  if (!bb->laststmt || !bb->firststmt) {
    return false;
  }
  for (StmtNode *stmt = bb->laststmt; stmt && stmt != bb->firststmt->GetPrev(); stmt = stmt->GetPrev()) {
    if (!dynamic_cast<CommentNode *>(stmt)) {
      return false;
    }
  }
  return true;
}

/**
 * Merge sucBB into curBB.
 */
void CGCFG::MergeBB(BB *merger, BB *mergee, CGFunc *cgfunc) {
  MergeBB(merger, mergee);
  if (mergee->GetKind() == BB::kBBReturn) {
    for (int i = 0; i < cgfunc->exitbbsvec.size(); i++) {
      if (cgfunc->exitbbsvec.at(i) == mergee) {
        cgfunc->exitbbsvec.erase(cgfunc->exitbbsvec.begin() + i);
      }
    }
    cgfunc->exitbbsvec.push_back(merger);
  }
}

void CGCFG::MergeBB(BB *merger, BB *mergee) {
  if (merger->GetKind() == BB::kBBGoto) {
    CHECK_FATAL(merger->lastinsn->IsBranch(), "unexpected insn kind");
    merger->RemoveInsn(merger->lastinsn);
  }
  merger->AppendBBInsns(mergee);
  if (mergee->prev != nullptr) {
    mergee->prev->next = mergee->next;
  }
  if (mergee->next != nullptr) {
    mergee->next->prev = mergee->prev;
  }
  merger->succs.remove(mergee);
  if (merger->eh_succs.size() != 0) {
    for (BB *bb : merger->eh_succs) {
      CHECK_FATAL((bb != mergee), "CGCFG::MergeBB: Merging of EH bb");
    }
  }
  if (mergee->eh_succs.size() != 0) {
    for (BB *bb : mergee->eh_succs) {
      bb->eh_preds.remove(mergee);
      bb->eh_preds.push_back(merger);
      merger->eh_succs.push_back(bb);
    }
  }
  for (BB *bb : mergee->succs) {
    bb->preds.remove(mergee);
    bb->preds.push_back(merger);
    merger->succs.push_back(bb);
  }
  merger->SetKind(mergee->GetKind());
  mergee->next = nullptr;
  mergee->prev = nullptr;
  mergee->preds.clear();
  mergee->succs.clear();
  mergee->eh_preds.clear();
  mergee->eh_succs.clear();
  mergee->firstinsn = nullptr;
  mergee->lastinsn = nullptr;
}

/**
 * Find all reachable BBs by dfs in cgfunc and mark their field<unreachable> false, then all other bbs should be
 * unreachable.
 */
void CGCFG::FindAndMarkUnreachable(CGFunc *cgfunc) {
  BB *firstBB = cgfunc->firstbb;
  std::stack<BB *> toBeAnalyzedBBs;
  toBeAnalyzedBBs.push(firstBB);
  std::set<BB *> instackBBs;

  BB *bb = firstBB;
  // set all bb's unreacable to true
  while (bb) {
    // Check if bb is the first or the last BB of the function
    bool isFirstBBInfunc = (bb == cgfunc->firstbb);
    bool isLastBBInfunc = (bb == cgfunc->lastbb);
    if (bb->firststmt == cgfunc->cleanup_label || InSwitchTable(bb->labidx, cgfunc) || isFirstBBInfunc ||
        isLastBBInfunc) {
      toBeAnalyzedBBs.push(bb);
    } else if (bb->labelTaken == false) {
      bb->unreachable = true;
    }
    bb = bb->next;
  }

  // do a dfs to see which bbs are reachable
  while (!toBeAnalyzedBBs.empty()) {
    bb = toBeAnalyzedBBs.top();
    toBeAnalyzedBBs.pop();
    instackBBs.insert(bb);

    bb->unreachable = false;

    for (BB *succBB : bb->succs) {
      if (instackBBs.count(succBB) == 0) {
        toBeAnalyzedBBs.push(succBB);
        instackBBs.insert(succBB);
      }
    }
    for (BB *succBB : bb->eh_succs) {
      if (instackBBs.count(succBB) == 0) {
        toBeAnalyzedBBs.push(succBB);
        instackBBs.insert(succBB);
      }
    }
  }
}

/**
 * Theoretically, every time you remove from a bb's preds, you should consider invoking this method.
 *
 * @param bb
 * @param cgfunc
 */
void CGCFG::FlushUnReachableStatusAndRemoveRelations(BB *bb, CGFunc *cgfunc) {
  // Check if bb is the first or the last BB of the function
  bool isFirstBBInfunc = (bb == cgfunc->firstbb);
  bool isLastBBInfunc = (bb == cgfunc->lastbb);
  if (bb->firststmt == cgfunc->cleanup_label || InSwitchTable(bb->labidx, cgfunc) || isFirstBBInfunc ||
      isLastBBInfunc) {
    return;
  } else {
    stack<BB *> toBeAnalyzedBBs;
    toBeAnalyzedBBs.push(bb);
    set<uint32_t> instackBBs;
    BB *it = nullptr;
    while (!toBeAnalyzedBBs.empty()) {
      it = toBeAnalyzedBBs.top();
      instackBBs.insert(it->id);
      toBeAnalyzedBBs.pop();
      // Check if bb is the first or the last BB of the function
      bool isFirstBBInfunc = (it == cgfunc->firstbb);
      bool isLastBBInfunc = (it == cgfunc->lastbb);
      if (it->firststmt != cgfunc->cleanup_label &&
          (it->preds.empty() || (it->preds.size() == 1 && it->preds.front() == it)) && it->eh_preds.empty() &&
          !isFirstBBInfunc && !isLastBBInfunc && !InSwitchTable(it->labidx, cgfunc) && (bb->labelTaken == false)) {
        it->unreachable = true;

        /*
         * Handle exception in aarch64_store_load_opt.cpp CG_ASSERT(insn->defs[0], "Internal error.");
         * suggested by William.
         */
        it->firstinsn = nullptr;
        it->lastinsn = nullptr;

        for (BB *succ : it->succs) {
          if (instackBBs.count(succ->id) == 0) {
            toBeAnalyzedBBs.push(succ);
            instackBBs.insert(succ->id);
          }
          succ->preds.remove(it);
          succ->eh_preds.remove(it);
        }
        it->succs.clear();
        for (BB *succ : it->eh_succs) {
          if (instackBBs.count(succ->id) == 0) {
            toBeAnalyzedBBs.push(succ);
            instackBBs.insert(succ->id);
          }
          succ->eh_preds.remove(it);
          succ->preds.remove(it);
        }
        it->eh_succs.clear();
      }
    }
  }
}

bool CGCFG::IsUnreachable(BB *curbb, CGFunc *cgfunc) {
  /*
   * After CGCFG::FindAndMarkUnreachable, all the bbs' unreachable field is correctly assigned, so we
   * just return this `unreachable` field.
   *
   * The former logic of this method has been removed.
   */
  return curbb->unreachable;
}

/**
 * Return true if bb1 dominates bb2
 */
bool CGCFG::Dominates(BB *bb1, BB *bb2, vector<BB *> searched, CGFunc *cgfunc) {
  if (bb2 == nullptr) {
    return false;
  }
  if (bb1 == cgfunc->firstbb || bb1 == bb2 || IsUnreachable(bb2, cgfunc)) {
    return true;
  }
  if (bb2 == cgfunc->firstbb) {
    return false;
  }
  for (BB *bb : searched) {
    if (bb == bb2) {
      return true;
    }
  }
  searched.push_back(bb2);
  bool ret = true;
  for (BB *bb : bb2->preds) {
    ret = Dominates(bb1, bb, searched, cgfunc) && ret;
    if (!ret) {
      return ret;
    }
  }
  return ret;
}

void CGCFG::RemoveBB(BB *curbb, bool isGotoIf) {
  if (!curbb) {
    return;
  }
  BB *sucBB = CGCFG::GetTargetSuc(curbb, false, isGotoIf);
  if (sucBB) {
    sucBB->preds.remove(curbb);
  }
  BB *fallthruSuc = nullptr;
  if (isGotoIf) {
    for (BB *succ : curbb->succs) {
      if (succ == sucBB) {
        continue;
      } else {
        fallthruSuc = succ;
        break;
      }
    }
    CG_ASSERT(fallthruSuc == curbb->next, "fallthru succ should be its next bb.");
    if (fallthruSuc) {
      fallthruSuc->preds.remove(curbb);
    }
  }

  for (BB *preBB : curbb->preds) {
    // If curbb is the target of its predecessor, change
    // the jump target.
    if (curbb == GetTargetSuc(preBB, true, isGotoIf)) {
      LabelIdx targetLabel;
      if (curbb->next->labidx == 0) {
        targetLabel = insnVisitor->GetCGFunc()->CreateLabel();
        curbb->next->labidx = targetLabel;
      } else {
        targetLabel = curbb->next->labidx;
      }
      insnVisitor->ModifyJumpTarget(targetLabel, preBB);
    }
    if (fallthruSuc && !fallthruSuc->IsPredecessor(preBB)) {
      preBB->succs.push_back(fallthruSuc);
      fallthruSuc->preds.push_back(preBB);
    }
    if (sucBB && !sucBB->IsPredecessor(preBB)) {
      preBB->succs.push_back(sucBB);
      sucBB->preds.push_back(preBB);
    }
    preBB->succs.remove(curbb);
  }
  for (BB *ehSucc : curbb->eh_succs) {
    ehSucc->eh_preds.remove(curbb);
  }
  for (BB *ehPred : curbb->eh_preds) {
    ehPred->eh_succs.remove(curbb);
  }
  curbb->next->preds.remove(curbb);
  curbb->prev->next = curbb->next;
  curbb->next->prev = curbb->prev;
}

void CGCFG::RetargetJump(BB *&srcBB, BB *targetBB) {
  insnVisitor->ModifyJumpTarget(srcBB, targetBB);
}

BB *CGCFG::GetTargetSuc(BB *curbb, bool branchOnly, bool isGotoIf) {
  if (curbb) {
    switch (curbb->GetKind()) {
      case BB::kBBGoto:
      case BB::kBBIntrinsic:
      case BB::kBBIf: {
        Insn* origLastInsn = curbb->lastinsn;
        if (isGotoIf && (curbb->GetKind() == BB::kBBGoto) && (curbb->prev && curbb->prev->GetKind() == BB::kBBGoto)) {
          origLastInsn = curbb->prev->lastinsn;
        } else if (isGotoIf && (curbb->GetKind() == BB::kBBIf) && (curbb->prev && curbb->prev->GetKind() == BB::kBBIf)) {
          origLastInsn = curbb->prev->lastinsn;
        }
        LabelIdx label = insnVisitor->GetJumpLabel(origLastInsn);
        for (BB *bb : curbb->succs) {
          if (bb->labidx == label) {
            return bb;
          }
        }
        break;
      }
      case BB::kBBIgoto: {
        for (Insn *insn = curbb->lastinsn; insn != nullptr; insn = insn->prev) {
          if (insn->GetMachineOpcode() == MOP_adrp_label) {
            LabelIdx label = static_cast<ImmOperand *>(insn->GetOperand(1))->GetValue();
            for (BB *bb : curbb->succs) {
              if (bb->labidx == label) {
                return bb;
              }
            }
          }
        }
        CHECK_FATAL(0, "Cannot find label in Igoto bb");
      }
      case BB::kBBCall:
      case BB::kBBFallthru: {
        if (branchOnly) {
          return nullptr;
        } else {
          return curbb->next;
        }
      }
      case BB::kBBThrow:
        return nullptr;
      default:
        return nullptr;
    }
  }
  return nullptr;
}

bool CGCFG::InLSDA(LabelIdx label, EHFunc *ehfunc) {
  if (label && ehfunc && ehfunc->lsda_callsite_table) {
    if (label == ehfunc->lsda_callsite_table->cs_table.end_offset->labelIdx ||
        label == ehfunc->lsda_callsite_table->cs_table.start_offset->labelIdx) {
      return true;
    }
    MapleVector<LSDACallSite *> &callsiteTable = ehfunc->lsda_callsite_table->callsite_table;
    for (uint32 i = 0; i < callsiteTable.size(); i++) {
      LSDACallSite *lsdacallsite = callsiteTable[i];
      if (label == lsdacallsite->cs_start.end_offset->labelIdx ||
          label == lsdacallsite->cs_start.start_offset->labelIdx) {
        return true;
      }
      if (label == lsdacallsite->cs_length.end_offset->labelIdx ||
          label == lsdacallsite->cs_length.start_offset->labelIdx) {
        return true;
      }
      if (lsdacallsite->cs_landing_pad.start_offset) {
        if (label == lsdacallsite->cs_landing_pad.end_offset->labelIdx ||
            label == lsdacallsite->cs_landing_pad.start_offset->labelIdx) {
          return true;
        }
      }
    }
    return false;
  } else {
    return false;
  }
}

bool CGCFG::InSwitchTable(LabelIdx label, CGFunc *cgfunc) {
  if (label) {
    for (MapleVector<MIRSymbol *>::iterator stit = cgfunc->emitstvec_.begin(); stit != cgfunc->emitstvec_.end();
         stit++) {  // emit switch table only here
      MIRSymbol *st = *stit;
      MIRAggConst *arrayConst = dynamic_cast<MIRAggConst *>(st->GetConst());
      CHECK_FATAL(arrayConst != nullptr, "array_const is null in CGCFG::InSwitchTable");
      for (uint32 i = 0; i < arrayConst->constVec.size(); i++) {
        MIRLblConst *lblconst = dynamic_cast<MIRLblConst *>(arrayConst->constVec[i]);
        CHECK_FATAL(lblconst != nullptr, "lblconst is null in CGCFG::InSwitchTable");
        if (label == lblconst->value) {
          return true;
        }
      }
    }
  }
  return false;
}

Insn *CGCFG::CheckIntegrity(BB *bb) {
  if (!bb) {
    return nullptr;
  }
  Insn *insn = bb->firstinsn;
  if (!insn) {
    return nullptr;
  }
  while (insn) {
    if (insn->next)
      if (insn->next->prev != insn) {
        LogInfo::MapleLogger() << "insn->next->prev != insn" << endl;
        return insn;
      }
    if (insn->prev) {
      if (insn->prev->next != insn) {
        LogInfo::MapleLogger() << "insn->prev->next != insn" << endl;
        return insn;
      }
    }
    insn = insn->next;
  }

  return nullptr;
}

void CGCFG::DumpBBInsn(BB *bb) {
  LogInfo::MapleLogger() << "BB Id:" << bb->id << "\n";
  if (bb->labidx) {
    LogInfo::MapleLogger() << "label idx:" << bb->labidx << "\n";
  }
  Insn *insn = bb->firstinsn;
  while (insn) {
    insn->dump();
    insn = insn->next;
  }
}

/**
 * Find the last accessed memory operand in the given bb.
 * If the last memory access instruction in store, return the target operand.
 * If the last memory access instruction in load, return the source operand.
 */
MemOperand *CGCFG::FindLastAccessedMem(BB *bb) {
  for (Insn *insn = bb->lastinsn; insn && insn != bb->firstinsn->prev; insn = insn->prev) {
    MemOperand *mem = GetMemOperand(insn);
    if (mem) {
      return mem;
    }
  }
  return nullptr;
}

/**
 * Return the memory operand of the given insn.
 */
MemOperand *CGCFG::GetMemOperand(Insn *insn, ACCESS access) {
  if (!insn) {
    return nullptr;
  }
  if (insn->IsStore() && (access == ACCESS::kEither || access == ACCESS::kStore)) {
    return static_cast<MemOperand *>(insnVisitor->GetStrTarget(insn));
  }
  if (insn->IsLoad() && (access == ACCESS::kEither || access == ACCESS::kLoad)) {
    return static_cast<MemOperand *>(insnVisitor->GetLdrSource(insn));
  }
  return nullptr;
}

/**
 * Return the register operand of the given insn.
 */
RegOperand *CGCFG::GetRegOperand(Insn *insn) {
  if (!insn) {
    return nullptr;
  }
  if (insn->IsStore()) {
    return static_cast<RegOperand *>(insnVisitor->GetStrSource(insn));
  }
  if (insn->IsLoad()) {
    return static_cast<RegOperand *>(insnVisitor->GetLdrTarget(insn));
  }
  return nullptr;
}

/**
 * Find the last instruction that accesses the given mem in given bb.
 * Access could be load, store or either.
 */
Insn *CGCFG::FindLastInsnAccessMem(BB *bb, MemOperand *mem, ACCESS access) {
  if (!bb || !mem) {
    return nullptr;
  }
  for (Insn *insn = bb->lastinsn; insn && insn != bb->firstinsn->prev; insn = insn->prev) {
    Operand *accessedMem = GetMemOperand(insn, access);
    if (accessedMem && static_cast<MemOperand *>(accessedMem)->Equals(mem)) {
      return insn;
    }
  }
  return nullptr;
}

/**
 * Find the first instruction that accesses the given mem in given bb.
 * Access could be load, store or either.
 */
Insn *CGCFG::FindFirstInsnAccessMem(BB *bb, MemOperand *mem, ACCESS access) {
  if (!bb || !mem) {
    return nullptr;
  }
  for (Insn *insn = bb->firstinsn; insn && insn != bb->lastinsn->next; insn = insn->next) {
    Operand *accessedMem = GetMemOperand(insn, access);
    if (accessedMem && static_cast<MemOperand *>(accessedMem)->Equals(mem)) {
      return insn;
    }
  }
  return nullptr;
}

Insn *CGCFG::FindLastMemAccessInsn(BB *bb) {
  if (!bb) {
    return nullptr;
  }
  for (Insn *insn = bb->lastinsn; insn && insn != bb->firstinsn->prev; insn = insn->prev) {
    if (insn->IsStore() || insn->IsLoad()) {
      return insn;
    }
  }
  return nullptr;
}

Insn *CGCFG::FindLastLdrInsn(BB *bb) {
  for (Insn *insn = bb->lastinsn; insn && insn != bb->firstinsn->prev; insn = insn->prev) {
    if (insn->IsLoad()) {
      return insn;
    }
  }
  return nullptr;
}

Insn *CGCFG::FindLastStrInsn(BB *bb) {
  for (Insn *insn = bb->lastinsn; insn && insn != bb->firstinsn->prev; insn = insn->prev) {
    if (insn->IsStore()) {
      return insn;
    }
  }
  return nullptr;
}

/*
 * Get the source register operand of the last store instruction
 * of the given BB.
 */
Operand *CGCFG::FindLastStrSource(BB *bb) {
  Insn *str = FindLastStrInsn(bb);
  if (str) {
    return insnVisitor->GetStrSource(str);
  }
  return nullptr;
}

Operand *CGCFG::FindLastStrTarget(BB *bb) {
  Insn *str = FindLastStrInsn(bb);
  if (str) {
    return insnVisitor->GetStrTarget(str);
  }
  return nullptr;
}

Operand *CGCFG::FindLastLdrTarget(BB *bb) {
  Insn *ldr = FindLastLdrInsn(bb);
  if (ldr) {
    return insnVisitor->GetLdrTarget(ldr);
  }
  return nullptr;
}

bool CGCFG::IsAccessSameMem(BB *bb1, BB *bb2) {
  Insn *insn1 = FindLastMemAccessInsn(bb1);
  Insn *insn2 = FindLastMemAccessInsn(bb2);
  if (insn1 && insn2 && ((insn1->IsLoad() && insn2->IsLoad()) || (insn1->IsStore() && insn2->IsStore()))) {
    Operand *op1 = GetMemOperand(insn1);
    Operand *op2 = GetMemOperand(insn2);
    if (op1 && op2 && insnVisitor->EqualMemOperands(op1, op2)) {
      return true;
    }
  }
  return false;
}

Insn *CGCFG::FindLastDefInsn(BB *bb) {
  for (Insn *insn = bb->lastinsn; insn && insn != bb->firstinsn->prev; insn = insn->prev) {
    if (insn->IsDefinition() && !insn->IsLoad()) {
      return insn;
    }
  }
  return nullptr;
}

bool CGCFG::IsDefSameReg(BB *bb1, BB *bb2) {
  if (bb1 == nullptr || bb2 == nullptr) {
    return false;
  }
  Insn *insn1 = FindLastDefInsn(bb1);
  Insn *insn2 = FindLastDefInsn(bb2);
  if (insn1 && insn2) {
    CHECK_FATAL(insn1->GetOperand(0)->IsRegister(), "not a register");
    CHECK_FATAL(insn2->GetOperand(0)->IsRegister(), "not a register");
    RegOperand *regopnd1 = static_cast<RegOperand *>(insn1->GetOperand(0));
    RegOperand *regopnd2 = static_cast<RegOperand *>(insn2->GetOperand(0));
    if (regopnd1->Equals(regopnd2)) {
      return true;
    }
  }
  return false;
}

bool CGCFG::IsCompareInsn(Insn *insn) {
  return insnVisitor->IsCompareInsn(insn);
}

bool CGCFG::IsCompareAndBranchInsn(Insn *insn) {
  return insnVisitor->IsCompareAndBranchInsn(insn);
}

Insn *CGCFG::FindLastCmpInsn(BB *bb) {
  if (bb->GetKind() == BB::kBBIf) {
    for (Insn *insn = bb->lastinsn; insn && insn != bb->firstinsn->prev; insn = insn->prev) {
      if (insnVisitor->IsCompareInsn(insn)) {
        return insn;
      }
    }
    return nullptr;
  }
  return nullptr;
}

Insn *CGCFG::FindLastCondBrInsn(BB *bb) {
  if (bb->GetKind() == BB::kBBIf) {
    for (Insn *insn = bb->lastinsn; insn && insn != bb->firstinsn->prev; insn = insn->prev) {
      if (insn->IsBranch()) {
        return insn;
      }
    }
    return nullptr;
  }
  return nullptr;
}

/**
 * Find out if the data is manipulated between memory or register.
 * Return 0 for empty bb, 1 for register, 2 for memory, 3 for neither.
 */
int CGCFG::IsLastRegOrMem(BB *bb) {
  if (bb) {
    for (Insn *insn = bb->lastinsn; insn && insn != bb->firstinsn->prev; insn = insn->prev) {
      if (insn->IsLoad() || insn->IsStore()) {
        return 2;
      }
      if (insn->IsMove()) {
        return 1;
      }
    }
    return 3;
  }
  return 0;
}

bool CGCFG::ChangeInsnTar(Insn *insn, RegOperand *newTar) {
  if (!insn) {
    return false;
  }
  if (insn->IsLoad() || insn->IsMove() || insn->IsStore()) {
    return insnVisitor->ModifyInsnOpnds(insn, nullptr, newTar);
  } else {
    return false;
  }
}

RegOperand *CGCFG::CheckSrcForDef(BB *bb, Insn *insn) {
  CHECK_FATAL(insn->IsDefinition(), "insn not defined");
  RegOperand *dest = static_cast<RegOperand *>(insn->GetOperand(0));
  if (insn->IsEffectiveCopy()) {
    int32_t idx = insn->CopyOperands();
    Operand *opnd = insn->GetOpnd(idx);
    CHECK_FATAL(opnd != nullptr, "opnd is null in CGCFG::CheckSrcForDef");
    if (opnd->IsRegister()) {
      return static_cast<RegOperand *>(opnd);
    }
  } else {
#if !TARGARK
    // "movk" only modify some bits of the source register, the other bits will not changed.
    // Can not do optimization on these cases.
    if (insn->IsPartDef()) {
      return nullptr;
    }
#endif
    RegOperand *tempTar = CreateVregFromReg(dest);
    ChangeInsnTar(insn, tempTar);
    Insn *newinsn = insnVisitor->CreateMoveInsn(dest, tempTar);
    CHECK_FATAL(newinsn != nullptr, "newinsn is null in CGCFG::CheckSrcForDef");
    bb->InsertInsnAfter(insn, newinsn);
    return tempTar;
  }
  return nullptr;
}

int CGCFG::NumOfInsn(BB *bb) {
  int bbSize = 0;

  for (Insn *i = bb->firstinsn; i && i != bb->lastinsn->next; i = i->next) {
    if (!i->IsMachineInstruction()) {
      continue;
    }
    bbSize++;
  }
  return bbSize;
}

bool CGCFG::CheckCondMoveBB(BB *bb, map<Operand *, Operand *> &destSrcMap, vector<Operand *> &destRegs,
                            Operand *flagOpnd) {
  if (!bb) {
    return false;
  }
  for (Insn *insn = bb->firstinsn; insn && insn != bb->lastinsn->next; insn = insn->next) {
    if (!insn->IsMachineInstruction() || insn->IsBranch()) {
      continue;
    }
    Operand *dest = nullptr;
    Operand *src = nullptr;

    bool isSetInsn = IsSetInsn(insn, dest, src);
    if (!isSetInsn) {
      return false;
    }
    CG_ASSERT(dest != nullptr, "null ptr check");
    CG_ASSERT(src != nullptr, "null ptr check");

    if (!dest->IsRegister()) {
      return false;
    }

    if (!src->IsConstant() && !src->IsRegister()) {
      return false;
    }

    if (flagOpnd) {
      RegOperand *flagReg = static_cast<RegOperand *>(flagOpnd);
      regno_t flagRegNo = flagReg->GetRegisterNumber();
      for (auto it = bb->liveout_regno.begin(); it != bb->liveout_regno.end();
           it++) {
        regno_t regno = static_cast<regno_t>(*it);
        if (flagRegNo == regno) {
          return false;
        }
      }
    }

    // src was modified in this blcok earlier
    if (src->IsRegister()) {
      RegOperand *srcReg = dynamic_cast<RegOperand *>(src);
      for (map<Operand *, Operand *>::iterator itr = destSrcMap.begin(); itr != destSrcMap.end(); itr++) {
        RegOperand *mapSrcReg = dynamic_cast<RegOperand *>(itr->first);
        if (mapSrcReg->GetRegisterNumber() == srcReg->GetRegisterNumber()) {
          return false;
        }
      }
    }

    // dest register was modified earlier in this block
    CG_ASSERT(dest->IsRegister(), "opnd must be register");
    RegOperand *destReg = dynamic_cast<RegOperand *>(dest);
    for (map<Operand *, Operand *>::iterator itr = destSrcMap.begin(); itr != destSrcMap.end(); itr++) {
      CG_ASSERT(itr->first->IsRegister(), "opnd must be register");
      RegOperand *mapSrcReg = dynamic_cast<RegOperand *>(itr->first);
      if (mapSrcReg->GetRegisterNumber() == destReg->GetRegisterNumber()) {
        return false;
      }
    }

    // src register is modified later in this block, will not be processed
    if (src->IsRegister()) {
      RegOperand *srcReg = dynamic_cast<RegOperand *>(src);
      for (Insn *tmpInsn = insn; tmpInsn && tmpInsn != bb->lastinsn->next; tmpInsn = tmpInsn->next) {
        Operand *tmpDest = nullptr;
        Operand *tmpSrc = nullptr;
        if (IsSetInsn(tmpInsn, tmpDest, tmpSrc) && tmpDest->Equals(src)) {
          CG_ASSERT(tmpDest->IsRegister(), "opnd must be register");
          RegOperand *tmpDestReg = dynamic_cast<RegOperand *>(tmpDest);
          if (srcReg->GetRegisterNumber() == tmpDestReg->GetRegisterNumber()) {
            return false;
          }
        }
      }
    }

    destSrcMap.insert(map<Operand *, Operand *>::value_type(dest, src));
    destRegs.push_back(dest);
  }
  return true;
}

bool CGCFG::IsSetInsn(Insn *insn, Operand *&dest, Operand *&src) {
#if TARGAARCH64 || TARGRISCV64
  MOperator mopcode = insn->mop_;
  if (mopcode >= MOP_xmovrr && mopcode <= MOP_xvmovd) {
    dest = insn->GetOperand(0);
    src = insn->GetOperand(1);
    return true;
  }

  dest = nullptr;
  src = nullptr;
#endif
  return false;
}

bool CGCFG::BuildCondMovInsn(BB *cmpBB, BB *bb, const map<Operand *, Operand *> &ifDestSrcMap,
                             const map<Operand *, Operand *> &elseDestSrcMap, bool elseBBisProcessed,
                             vector<Insn *> &generateInsn) {
#if !TARGAARCH64
  return false;
#else
  if (!bb) {
    return false;
  }
  Insn *branchInsn = FindLastCondBrInsn(cmpBB);
  for (Insn *insn = bb->firstinsn; insn && insn != bb->lastinsn->next; insn = insn->next) {
    if (!insn->IsMachineInstruction() || insn->IsBranch()) {
      continue;
    }
    Operand *dest = nullptr;
    Operand *src = nullptr;
    Operand *t = nullptr;
    Operand *e = nullptr;

    bool isSetInsn = IsSetInsn(insn, dest, src);
    CG_ASSERT((isSetInsn != false) && dest->IsRegister() , "insn check && register check");
    RegOperand *destReg = static_cast<RegOperand *>(dest);

    for (map<Operand *, Operand *>::const_iterator itr = elseDestSrcMap.begin(); itr != elseDestSrcMap.end(); itr++) {
      CG_ASSERT(itr->first->IsRegister(), "opnd must be register");
      RegOperand *destRegInMap = dynamic_cast<RegOperand *>(itr->first);
      if (destRegInMap->GetRegisterNumber() == destReg->GetRegisterNumber()) {
        e = itr->second;
        break;
      }
    }

    for (map<Operand *, Operand *>::const_iterator itr = ifDestSrcMap.begin(); itr != ifDestSrcMap.end(); itr++) {
      CG_ASSERT(itr->first->IsRegister(), "opnd must be register");
      RegOperand *destRegInMap = dynamic_cast<RegOperand *>(itr->first);
      if (destRegInMap->GetRegisterNumber() == destReg->GetRegisterNumber()) {
        t = itr->second;
        break;
      }
    }

    if (elseBBisProcessed) {
      if (e) {
        continue;
      }
      e = dest;
      CG_ASSERT(t != nullptr, "null ptr check");
      if (bb->liveout_regno.find(destReg->GetRegisterNumber()) == bb->liveout_regno.end()) {
        continue;
      }
    } else {
      CG_ASSERT(e != nullptr, "null ptr check");
      if (!t) {
        if (bb->liveout_regno.find(destReg->GetRegisterNumber()) == bb->liveout_regno.end()) {
          continue;
        }
        t = dest;
      }
    }

    // generate cset or csel instruction
    if (t->IsIntImmediate() && e->IsIntImmediate()) {
      ImmOperand *imm1 = static_cast<ImmOperand *>(t);
      ImmOperand *imm2 = static_cast<ImmOperand *>(e);
      bool inverse = imm1->IsZero() && imm2->IsOne();
      if (inverse || (imm2->IsZero() && imm1->IsOne())) {
        Insn *csetInsn = insnVisitor->BuildCondSet(branchInsn, destReg, inverse);
        CG_ASSERT(csetInsn != nullptr, "null ptr check");
        generateInsn.push_back(csetInsn);
      } else if (imm1->GetValue() == imm2->GetValue()) {
        MOperator mop = (destReg->GetSize() == 64 ? MOP_xmovri64 : MOP_xmovri32);
        Insn *tempinsn = insnVisitor->GetCGFunc()->cg->BuildInstruction<AArch64Insn>(mop, destReg, imm1);
        generateInsn.push_back(tempinsn);
      } else {
        MOperator mop = (destReg->GetSize() == 64 ? MOP_xmovri64 : MOP_xmovri32);
        RegOperand *tempTarIf = CreateVregFromReg(destReg);
        Insn *tempinsnif = insnVisitor->GetCGFunc()->cg->BuildInstruction<AArch64Insn>(mop, tempTarIf, imm1);
        generateInsn.push_back(tempinsnif);

        RegOperand *tempTarElse = CreateVregFromReg(destReg);
        Insn *tempinsnelse = insnVisitor->GetCGFunc()->cg->BuildInstruction<AArch64Insn>(mop, tempTarElse, imm2);
        generateInsn.push_back(tempinsnelse);

        uint32 dsize = destReg->GetSize();
        uint32 isIntty = destReg->IsOfIntClass();
        MOperator mopcode = isIntty ? (dsize == 64 ? MOP_xcselrrrc : MOP_wcselrrrc)
                                    : (dsize == 64 ? MOP_dcselrrrc : (dsize == 32 ? MOP_scselrrrc : MOP_hcselrrrc));
        Insn *cselInsn = insnVisitor->BuildCondSel(branchInsn, mopcode, destReg, tempTarIf, tempTarElse);
        CG_ASSERT(cselInsn != nullptr, "null ptr check");
        generateInsn.push_back(cselInsn);
      }
    }

    else {
      RegOperand *tReg = nullptr;
      RegOperand *eReg = nullptr;
      if (!t->IsRegister()) {
        MOperator mop = (destReg->GetSize() == 64 ? MOP_xmovri64 : MOP_xmovri32);
        tReg = CreateVregFromReg(destReg);
        ImmOperand *tempSrcIf = static_cast<ImmOperand *>(t);
        Insn *tempinsnif = insnVisitor->GetCGFunc()->cg->BuildInstruction<AArch64Insn>(mop, tReg, tempSrcIf);
        generateInsn.push_back(tempinsnif);
      } else {
        tReg = static_cast<RegOperand *>(t);
      }

      if (!e->IsRegister()) {
        MOperator mop = (destReg->GetSize() == 64 ? MOP_xmovri64 : MOP_xmovri32);
        eReg = CreateVregFromReg(destReg);
        ImmOperand *tempSrcElse = static_cast<ImmOperand *>(e);
        Insn *tempinsnelse = insnVisitor->GetCGFunc()->cg->BuildInstruction<AArch64Insn>(mop, eReg, tempSrcElse);
        generateInsn.push_back(tempinsnelse);
      } else {
        eReg = static_cast<RegOperand *>(e);
      }
      // mov w0, w1   mov w0, w1  --> mov w0, w1
      if (eReg->GetRegisterNumber() == tReg->GetRegisterNumber()) {
        uint32 dsize = destReg->GetSize();
        bool srcIsIntty = tReg->IsOfIntClass();
        bool destIsIntty = destReg->IsOfIntClass();
        MOperator mop;

        if (dsize == 64) {
          mop = srcIsIntty ? (destIsIntty ? MOP_xmovrr : MOP_xvmovdr) : (destIsIntty ? MOP_xvmovrd : MOP_xvmovd);
        } else {
          mop = srcIsIntty ? (destIsIntty ? MOP_wmovrr : MOP_xvmovsr) : (destIsIntty ? MOP_xvmovrs : MOP_xvmovs);
        }
        Insn *tempinsnif = insnVisitor->GetCGFunc()->cg->BuildInstruction<AArch64Insn>(mop, destReg, tReg);
        generateInsn.push_back(tempinsnif);
      } else {
        uint32 dsize = destReg->GetSize();
        uint32 isIntty = destReg->IsOfIntClass();
        MOperator mopcode = isIntty ? (dsize == 64 ? MOP_xcselrrrc : MOP_wcselrrrc)
                                    : (dsize == 64 ? MOP_dcselrrrc : (dsize == 32 ? MOP_scselrrrc : MOP_hcselrrrc));

        if (isIntty == false) {
          if (tReg->GetRegisterNumber() == RZR || eReg->GetRegisterNumber() == RZR) {
            Insn *cselInsn;
            RegOperand *movDest = CreateVregFromReg(tReg);
            Insn *mov0 = insnVisitor->BuildFmoveZero(movDest, dsize);
            generateInsn.push_back(mov0);
            if (tReg->GetRegisterNumber() == R0) {
              cselInsn = insnVisitor->BuildCondSel(branchInsn, mopcode, destReg, movDest, eReg);
            } else {
              cselInsn = insnVisitor->BuildCondSel(branchInsn, mopcode, destReg, tReg, movDest);
            }
            CG_ASSERT(cselInsn != nullptr, "null ptr check");
            generateInsn.push_back(cselInsn);
            return true;
          }
        }
        Insn *cselInsn = insnVisitor->BuildCondSel(branchInsn, mopcode, destReg, tReg, eReg);
        CG_ASSERT(cselInsn != nullptr, "null ptr check");
        generateInsn.push_back(cselInsn);
      }
    }
  }

  return true;
#endif
}

Insn *CGCFG::BuildCmpInsn(Insn *condbr) {
  return insnVisitor->CreateCmpInsn(condbr);
}

Insn *CGCFG::BuildConditionSelectionInsn(BB *testBB, BB *ifBB, BB *elseBB) {
  if (IsLastRegOrMem(ifBB) == 1 || IsLastRegOrMem(elseBB) == 1) {
    return nullptr;
  }
  Insn *branchInsn = FindLastCondBrInsn(testBB);
  // Prepare for necessary instructions and operands
  Insn *lastMemAccessInsnIf = FindLastMemAccessInsn(ifBB);
  Insn *lastMemAccessInsnElse = FindLastMemAccessInsn(elseBB);
  Insn *lastBranchMemAccessInsn = lastMemAccessInsnIf ? lastMemAccessInsnIf : lastMemAccessInsnElse;
  if (!lastBranchMemAccessInsn) {
    // No need to check further if no memory access instruction is found in both branches
    return nullptr;
  }
  RegOperand *retReg = nullptr;
  RegOperand *ifReg = GetRegOperand(lastMemAccessInsnIf);
  MemOperand *ifMem = GetMemOperand(lastMemAccessInsnIf);
  RegOperand *elseReg = GetRegOperand(lastMemAccessInsnElse);
  MemOperand *elseMem = GetMemOperand(lastMemAccessInsnElse);
  Operand *lastBranchWriteTar = nullptr;
  if (lastBranchMemAccessInsn->IsStore()) {
    lastBranchWriteTar = GetMemOperand(lastBranchMemAccessInsn);
  } else if (lastBranchMemAccessInsn->IsLoad()) {
    lastBranchWriteTar = GetRegOperand(lastBranchMemAccessInsn);
  }

  // Check if it's the safe situation or unsafe
  if (ifMem && !(ifMem->GetBaseRegister()->IsSPOrFP())) {
    return nullptr;
  }
  if (elseMem && !(elseMem->GetBaseRegister()->IsSPOrFP())) {
    return nullptr;
  }

  RegOperand *originalReg = nullptr;
  // IF-THEN-ELSE situation
  if (ifBB && elseBB) {
    CHECK_FATAL(ifReg != nullptr, "ifReg is null in CGCFG::BuildConditionSelectionInsn");
    if (ifReg->Equals(elseReg)) {
      RegOperand *tempTar = CreateVregFromReg(dynamic_cast<RegOperand *>(ifReg));
      ChangeInsnTar(lastMemAccessInsnIf, tempTar);
      ifReg = tempTar;
    }
  }
  // IF-THEN situation
  else if (!ifBB || !elseBB) {
    if (dynamic_cast<MemOperand *>(lastBranchWriteTar)) {
      Insn *lastAccessInsn = FindLastInsnAccessMem(testBB, static_cast<MemOperand *>(lastBranchWriteTar));

      Insn *lastAccessRegInsn = nullptr;
      bool needNewLdr = false;
      // Need to create a new ldr to load the value from memory.
      if (!lastAccessInsn) {
        needNewLdr = true;
      }

      if (!needNewLdr) {
        RegOperand *lastAccessReg = GetRegOperand(lastAccessInsn);
        originalReg = lastAccessReg;
        for (Insn *insn = testBB->lastinsn; insn && insn != lastAccessInsn; insn = insn->prev) {
          if (insn->IsLoad() && GetRegOperand(insn)->Equals(lastAccessReg)) {
            needNewLdr = true;
            lastAccessRegInsn = insn;
            break;
          }
        }
      }
      if (needNewLdr) {
        originalReg = insnVisitor->CreateVregFromReg(GetRegOperand(lastBranchMemAccessInsn));
        Insn *newLdr = insnVisitor->CreateLdrInsn(lastBranchMemAccessInsn->mop_, originalReg,
                                                  static_cast<MemOperand *>(lastBranchWriteTar));
        if (!newLdr) {
          return nullptr;
        }
        if (lastAccessRegInsn) {
          testBB->InsertInsnAfter(lastAccessRegInsn, newLdr);
        } else {
          testBB->InsertInsnBefore(branchInsn, newLdr);
        }
      }

      if (!ifReg) {
        ifReg = originalReg;
      }
      if (!elseReg) {
        elseReg = originalReg;
      }
    } else if (dynamic_cast<RegOperand *>(lastBranchWriteTar)) {
      // Change branch's last ldr's target register
      RegOperand *tempTar = CreateVregFromReg(dynamic_cast<RegOperand *>(lastBranchWriteTar));
      ChangeInsnTar(lastBranchMemAccessInsn, tempTar);
      elseReg = tempTar;
      ifReg = dynamic_cast<RegOperand *>(lastBranchWriteTar);
    }
  }
  if (branchInsn && elseReg) {
    retReg = insnVisitor->CreateVregFromReg(ifReg);
    return insnVisitor->CreateCondSelectionInsn(branchInsn, lastBranchMemAccessInsn->mop_, retReg, ifReg, elseReg);
  } else {
    return nullptr;
  }
}

void CGCFG::MarkLabelTakenBB() {
  if (!CLANG) {
    return;
  }
  for (BB *bb = cgfunc->firstbb; bb != nullptr; bb = bb->next) {
    if (cgfunc->func->labelTab->addrTakenLabels.find(bb->labidx) !=
        cgfunc->func->labelTab->addrTakenLabels.end()) {
      cgfunc->SetHasTakenLabel();
      bb->labelTaken = true;
    }
  }
}

/*
 *analyse the CFG to find the BBs that are not reachable from function entries
 *and delete them
 */
void CGCFG::UnreachCodeAnalysis() {
  if (CLANG &&
      (cgfunc->HasTakenLabel() ||
      (cgfunc->ehfunc && cgfunc->ehfunc->lsda_header))) {
    return;
  }
  /**
   * Find all reachable BBs by dfs in cgfunc and mark their field<unreachable> false,
   * then all other bbs should be unreachable.
   */
  BB *firstBB = cgfunc->firstbb;
  std::forward_list<BB *> toBeAnalyzedBBs;
  toBeAnalyzedBBs.push_front(firstBB);
  std::set<BB *> unreachBBs;

  BB *bb = firstBB;
  // set all bb's unreacable to true
  while (bb) {
    // Check if bb is the first or the last BB of the function
    if (bb->firststmt == cgfunc->cleanup_label || InSwitchTable(bb->labidx, cgfunc) || bb == cgfunc->firstbb ||
        bb == cgfunc->lastbb || (bb->GetKind() == BB::kBBReturn)) {
      toBeAnalyzedBBs.push_front(bb);
    } else {
      unreachBBs.insert(bb);
    }
    if (bb->labelTaken == false) {
      bb->unreachable = true;
    }
    bb = bb->next;
  }

  // do a dfs to see which bbs are reachable
  while (!toBeAnalyzedBBs.empty()) {
    bb = toBeAnalyzedBBs.front();
    toBeAnalyzedBBs.pop_front();
    if (bb->unreachable == false) {
      continue;
    }
    bb->unreachable = false;
    for (BB *succBB : bb->succs) {
      toBeAnalyzedBBs.push_front(succBB);
      unreachBBs.erase(succBB);
    }
    for (BB *succBB : bb->eh_succs) {
      toBeAnalyzedBBs.push_front(succBB);
      unreachBBs.erase(succBB);
    }
  }
  // Don't remove unreach code if withDwarf is enabled.
  if (CGOptions::withDwarf) {
    return;
  }
  // remove unreachable bb
  std::set<BB *>::iterator it;
  for (it = unreachBBs.begin(); it != unreachBBs.end(); it++) {
    BB *bb = *it;
    for (int i = 0; i < cgfunc->exitbbsvec.size(); i++) {
      if (cgfunc->exitbbsvec.at(i) == bb) {
        bb->unreachable = false;
      }
    }
    EHFunc* ehfunc = cgfunc->ehfunc;
    //if bb InLSDA ,replace bb's label with nextReachableBB before remove it.
    if (cgfunc->func->module->IsJavaModule() &&
        ehfunc->NeedFullLSDA() && cgfunc->theCFG->InLSDA(bb->labidx, ehfunc)) {
      //find next reachable bb
      BB* nextReachableBB = nullptr;
      for (BB* curbb = bb;curbb;curbb = curbb->next) {
        if (!curbb->unreachable) {
          nextReachableBB = curbb;
          break;
        }
      }
      if (nextReachableBB->labidx == 0) {
        LabelIdx labidx = cgfunc->CreateLabel();
        nextReachableBB->AddLabel(labidx);
        cgfunc->lab2bbmap[labidx] = nextReachableBB;
      }

      MapleVector<LSDACallSite *>& callsite_table = ehfunc->lsda_callsite_table->callsite_table;
      for (uint32 i = 0; i < callsite_table.size(); i ++) {
        LSDACallSite *lsdaCallsite = callsite_table[i];

        if (lsdaCallsite->cs_start.end_offset) {
          if (lsdaCallsite->cs_start.end_offset->labelIdx == bb->labidx) {
            lsdaCallsite->cs_start.end_offset->labelIdx = nextReachableBB->labidx;
          }
        }

        if (lsdaCallsite->cs_length.end_offset->labelIdx == bb->labidx) {
          lsdaCallsite->cs_length.end_offset->labelIdx = nextReachableBB->labidx;
        }

        if (lsdaCallsite->cs_landing_pad.end_offset) {
          if (lsdaCallsite->cs_landing_pad.end_offset->labelIdx == bb->labidx) {
            lsdaCallsite->cs_landing_pad.end_offset->labelIdx = nextReachableBB->labidx;
          }
        }
      }
    }

    bb->prev->next = bb->next;
    bb->next->prev = bb->prev;

    for (BB* sucbb : bb->succs) {
      sucbb->preds.remove(bb);
    }
    for (BB *ehsucbb : bb->eh_succs) {
      ehsucbb->eh_preds.remove(bb);
    }

    bb->succs.clear();
    bb->eh_succs.clear();
  }
}

}  // namespace maplebe
