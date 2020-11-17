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

#include <iostream>
#include "me_critical_edge.h"
#include "me_cfg.h"
#include "me_option.h"
#include "dominance.h"
#include "me_function.h"

/* This phase finds critical edges and split them into two, because their
 * presence would restrict the optimizations performed by SSAPRE-based phases.
 *
 * An edge is a critical edge when its pred has more than one succ and its succ
 * has more than one pred
 *     pred
 *    /    \
 *  newbb   \   <-- newbb (newbb is an empty bb but may carry a label)
 * \ /       \
 * succ
 *
 * newbb is always appended at the end of bbVec and pred/succ will be updated.
 *
 * The bblayout phase will determine the final layout order of the bbs.
 */

namespace maple {

void MeDoSplitCEdge::BreakCriticalEdge(MeFunction *func, BB *pred, BB *succ) {
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "******before break : critical edge : BB" << pred->id.idx << " -> BB" << succ->id.idx << "\n";
    pred->Dump(&func->mirModule);
    succ->Dump(&func->mirModule);
  }
  CHECK_FATAL(!succ->IsCatch(), "BreakCriticalEdge: cannot break an EH edge");

  /* create newbb and set pred/succ */
  BB *newbb = func->NewBasicBlock();
  newbb->kind = kBBFallthru; /* default kind */
  newbb->artificial = true;
  /* use replace instead of remove/add to keep position in pred/succ */
  if (pred == func->theCFG->commonEntryBB) {
    pred->ReplaceSuccOfCommonEntryBB(succ, newbb);
    newbb->succ.push_back(succ);
    succ->pred.push_back(newbb);
    newbb->isEntry = true;
    succ->isEntry = false;
    func->theCFG->first_bb = newbb;
  } else {
    pred->ReplaceSucc(succ, newbb);
    succ->ReplacePred(pred, newbb);
  }
  /* update statement offset if succ is goto target */
  StmtNode *predLast = pred->stmtNodeList.last;
  if (pred->kind == kBBCondGoto) {
    CHECK_FATAL((predLast != nullptr) && (pred->isTry || pred->succ.size() == 2), "");
    CondGotoNode *gotostmt = static_cast<CondGotoNode *>(predLast);
    BB *gotobb = pred->succ.at(1);
    LabelIdx oldlabIdx = gotostmt->offset;
    if (oldlabIdx != gotobb->bbLabel) {
      /* original gotobb is replaced by newbb */
      CHECK_FATAL((gotobb->bbLabel == 0) && (newbb == gotobb), ""); /* gotobb is the newbb */
      func->CreateBBLabel(gotobb);
      gotostmt->offset = gotobb->bbLabel;
    }
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "******after break: dump updated condgoto_BB *****\n";
      pred->Dump(&func->mirModule);
      newbb->Dump(&func->mirModule);
      succ->Dump(&func->mirModule);
    }
  } else if (pred->kind == kBBSwitch) {
    CHECK_FATAL(predLast != nullptr, "");
    SwitchNode *switchstmt = static_cast<SwitchNode *>(predLast);
    CHECK_FATAL(succ->bbLabel != 0, "");
    LabelIdx oldlabIdx = succ->bbLabel;
    CHECK_FATAL(newbb->bbLabel == 0, ""); /* newbb is no label */
    func->CreateBBLabel(newbb);
    if (switchstmt->defaultLabel == oldlabIdx) {
      switchstmt->defaultLabel = newbb->bbLabel;
    }
    for (uint32_t i = 0; i < switchstmt->switchTable.size(); i++) {
      LabelIdx lblidx = switchstmt->switchTable[i].second;
      if (lblidx == oldlabIdx) {
        switchstmt->switchTable[i] = std::make_pair(switchstmt->switchTable[i].first, newbb->bbLabel);
      }
    }
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "******after break: dump updated switchBB *****\n";
      pred->Dump(&func->mirModule);
      newbb->Dump(&func->mirModule);
      succ->Dump(&func->mirModule);
    }
  }
}

AnalysisResult *MeDoSplitCEdge::Run(MeFunction *func, MeFuncResultMgr *m) {
  /* create local mempool */
  MemPool *localmp = mempoolctrler.NewMemPool("localsplitcriticaledge");
  MapleAllocator localAlloc(localmp);
  MapleVector<std::pair<BB *, BB *>> criticaledge(localAlloc.Adapter());

  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    if (bb == func->theCFG->commonExitBB) {
      continue;
    }
    MapleVector<BB *> &preds = bb->pred;
    /* skip fallthrough bb or bb is handler block */
    if (preds.size() < 2 || bb->IsCatch()) {
      continue;
    }
    /* current BB is a merge */
    for (BB *pred : preds) {
      if (pred->kind == kBBGoto || pred->kind == kBBIgoto) {
        continue;
      }
      if (pred->succ.size() > 1) {
        /* pred has more than one succ */
        criticaledge.push_back(std::make_pair(pred, bb));
      }
    }
  }
  // separate treatment for commonEntryBB's succ BBs
  for (BB *entrybb : func->theCFG->commonEntryBB->succ) {
    if (entrybb->pred.size() > 0 && !entrybb->IsCatch()) {
      criticaledge.push_back(std::make_pair(func->theCFG->commonEntryBB, entrybb));
    }
  }
  if (criticaledge.size() > 0) {
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "*******************before break dump function*****************\n";
      func->DumpFunctionNoSSA();
      func->theCFG->DumpToFile("cfgbeforebreak");
    }

    for (auto it = criticaledge.begin(); it != criticaledge.end(); it++) {
      BreakCriticalEdge(func, (*it).first, (*it).second);
    }
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "******************after break dump function******************\n";
      func->DumpFunctionNoSSA();
      func->theCFG->DumpToFile("cfgafterbreak");
    }
     m->InvalidAnalysisResult(MeFuncPhase_DOMINANCE, func);
  }
  mempoolctrler.DeleteMemPool(localmp);
  return nullptr;
}

}  // namespace maple
