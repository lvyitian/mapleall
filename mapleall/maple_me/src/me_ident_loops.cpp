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

#include "me_ident_loops.h"

// This phase analyses the CFG and identify the loops.  The implementation is
// based on the idea that, given two basic block a and b, if b is a's pred and
// a dominates b, then there is a loop from a to b.  Loop identification is done
// in a preorder traversal of the dominator tree.  In this order, outer loop is
// always detected before its nested loop(s).  The building of the LoopDesc data
// structure takes advantage of this ordering.

namespace maple {

void IdentifyLoops::SetLoopParent4BB(const BB *bb, LoopDesc *aloop) {
  if (bbloopparent[bb->id.idx] != nullptr) {
    if (aloop->parent == nullptr) {
      aloop->parent = bbloopparent[bb->id.idx];
      aloop->nestdepth = aloop->parent->nestdepth + 1;
    }
  }
  bbloopparent[bb->id.idx] = aloop;
}

void IdentifyLoops::SetLoopEntry(LoopDesc *aloop) {
  BB *headBB = aloop->head;
  // the entry BB is the predecessor of headBB that dominates headBB
  MapleVector<BB*>::iterator predit = headBB->pred.begin();
  for ( ; predit != headBB->pred.end(); predit++) {
    if (dominance->Dominate(*predit, headBB))
      break;
  }
  if (predit == headBB->pred.end()) {
    return;  // not a well-formed loop
  }
  aloop->entry = *predit;
}

void IdentifyLoops::SetExitBB(LoopDesc *aloop) {
  BB *headBB = aloop->head;
  // the exit BB is the succeessor of headBB that does not belong to the loop
  if (headBB->succ.size() != 2) {
    return;
  }
  if (aloop->loop_bbs.count(headBB->succ[0]->id) != 1) {
    aloop->exitBB = headBB->succ[0];
    aloop->startbodyBB = headBB->succ[1];
  } else {
    aloop->exitBB = headBB->succ[1];
    aloop->startbodyBB = headBB->succ[0];
  }
}

// process each BB in preorder traversal of dominator tree
void IdentifyLoops::ProcessBB(BB *bb) {
  if (bb == nullptr || bb == func->theCFG->commonExitBB) {
    return;
  }
  for (BB *pred : bb->pred) {
    if (dominance->Dominate(bb, pred)) {
      // create a loop with bb as loop head and pred as loop tail
      LoopDesc *aloop = alloc.mp->New<LoopDesc>(&alloc, bb, pred);
      meloops.push_back(aloop);
      std::list<BB *> bodylist;
      bodylist.push_back(pred);
      while (!bodylist.empty()) {
        BB *curr = bodylist.front();
        bodylist.pop_front();
        // skip bb or if it has already been dealt with
        if (curr == bb || aloop->loop_bbs.count(curr->id) == 1) {
          continue;
        }
        aloop->loop_bbs.insert(curr->id);
        SetLoopParent4BB(curr, aloop);
        for (BB *predi : curr->pred) {
          bodylist.push_back(predi);
        }
      }
      aloop->loop_bbs.insert(bb->id);
      SetLoopParent4BB(bb, aloop);
      SetLoopEntry(aloop);
      SetExitBB(aloop);
    }
  }

  // recursive call
  MapleSet<BBId> *domChildren = &dominance->domChildren[bb->id.idx];
  for (MapleSet<BBId>::iterator bbit = domChildren->begin(); bbit != domChildren->end(); bbit++) {
    ProcessBB(func->theCFG->bbVec.at(bbit->idx));
  }
}

void IdentifyLoops::Dump() {
  for (uint32 i = 0; i < meloops.size(); i++) {
    LoopDesc *mploop = meloops[i];
    // loop
    LogInfo::MapleLogger() << "nest depth: " << mploop->nestdepth << " loop head BB: " << mploop->head->id.idx
              << " tail BB:" << mploop->tail->id.idx << std::endl;
    LogInfo::MapleLogger() << "loop body:";
    for (MapleSet<BBId>::iterator it = mploop->loop_bbs.begin(); it != mploop->loop_bbs.end(); it++) {
      BBId bdid = *it;
      LogInfo::MapleLogger() << bdid.idx << " ";
    }
    LogInfo::MapleLogger() << std::endl;
  }
}

AnalysisResult *MeDoIdentLoops::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func, !MeOption::quiet));
  CHECK_FATAL(dom, "dominance phase has problem");
  MemPool *meloopmp = mempoolctrler.NewMemPool(PhaseName().c_str());
  IdentifyLoops *identloops = meloopmp->New<IdentifyLoops>(meloopmp, func, dom);
  identloops->ProcessBB(func->theCFG->commonEntryBB);
  if (DEBUGFUNC(func)) {
    identloops->Dump();
  }

  return identloops;
}

}  // namespace maple
