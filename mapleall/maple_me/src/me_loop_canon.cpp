/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan PSL v1.
 * You can use this software according to the terms and conditions of the Mulan PSL v1.
 * You may obtain a copy of Mulan PSL v1 at:
 *
 *     http://license.coscl.org.cn/MulanPSL
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v1 for more details.
 */

#include <iostream>
#include <algorithm>
#include "me_cfg.h"
#include "me_loop_canon.h"
#include "me_option.h"
#include "dominance.h"
using namespace std;

namespace maple {

// This phase changes the control flow graph to canonicalize loops so that the
// resulting loop form can provide places to insert loop-invariant code hoisted
// from the loop body.  This means converting loops to exhibit the
//      do {} while (condition)
// loop form.
//
// Step 1: Loops are identified by the existence of a BB that dominates one of
// its predecessors.  For each such backedge, the function NeedConvert() is
// called to check if it needs to be re-structured.  If so, the backedges are
// collected.
//
// Step2: The collected backedges are sorted so that their loop restructuring
// can be processed in just one pass.
//
// Step3: Perform the conversion for the loop represented by each collected
// backedge in sorted order.

/* sort backedges if bb is used as pred in one backedge and bb in another backedge
 * deal with the bb used as pred first */
static bool CompareBackedge(const pair<BB *, BB *> &a, const pair<BB *, BB *> &b) {
  /* second is pred, first is bb */
  if ((a.second)->id == (b.first)->id) {
    return true;
  } else if ((a.first)->id == (b.second)->id) {
    return false;
  } else {
    return (a.first)->id < (b.first)->id;
  }
}

bool MeDoLoopCanon::NeedConvert(BB *bb, BB *pred, MapleAllocator &localAlloc, MapleMap<Key, bool> &swapSuccs) {
  ASSERT(bb && pred, "bb and pred should not be null");
  bb->isInLoop = true;
  pred->isInLoop = true;
  /* do not convert do-while loop */
  if ((bb->kind != kBBCondGoto) || (pred == bb) || bb->isTry || bb->IsCatch()) {
    return false;
  }

  ASSERT(bb->succ.size() == 2, "");
  /* if both succs are equal, return false */
  if (bb->succ.front() == bb->succ.back()) {
    return false;
  }
  /* check bb's succ both in loop body or not, such as
   *   1  <--
   *  / \   |
   *  2 |   |
   *  \ |   |
   *   3 ---|
   *  / */
  MapleSet<BBId> inloop(std::less<BBId>(), localAlloc.Adapter());
  MapleList<BB *> bodylist(localAlloc.Adapter());
  bodylist.push_back(pred);
  while (!bodylist.empty()) {
    BB *curr = bodylist.front();
    bodylist.pop_front();
    // skip bb and bb is already in loop body(has been dealt with)
    if (curr == bb || inloop.count(curr->id) == 1) {
      continue;
    }
    inloop.insert(curr->id);
    for (BB *pred : curr->pred) {
      ASSERT(pred != nullptr, "");
      bodylist.push_back(pred);
      pred->isInLoop = true;
    }
  }
  if ((inloop.count(bb->succ[0]->id) == 1) && (inloop.count(bb->succ[1]->id) == 1)) {
    return false;
  }
  /* other case */
  // fallthru is in loop body, latchbb need swap succs
  if (inloop.count(bb->succ.at(0)->id) == 1) {
    swapSuccs.insert(make_pair(make_pair(bb, pred), true));
  }
  return true;
}

void MeDoLoopCanon::Convert(MeFunction *func, BB *bb, BB *pred, MapleMap<Key, bool> &swapSuccs) {
  ASSERT((bb != nullptr) && (pred != nullptr), "");

  /* if bb->fallthru is in loopBody, latchbb need convert condgoto and make original target as its fallthru */
  bool swapsuccofLatch = (swapSuccs.find(make_pair(bb, pred)) != swapSuccs.end());
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "***loop convert: backedge bb->id " << bb->id.idx << " pred->id " << pred->id.idx;
    if (swapsuccofLatch) {
      LogInfo::MapleLogger() << " need swap succs\n";
    } else {
      LogInfo::MapleLogger() << endl;
    }
  }

  /* new latchbb */
  BB *latchbb = func->NewBasicBlock();
  latchbb->artificial = true;

  /* update pred bb */
  bb->RemoveBBFromPred(pred);
  pred->ReplaceSucc(bb, latchbb); /* replace pred->succ with latchbb and set pred of latchbb */
  /* update pred stmt if needed */
  if (pred->kind == kBBGoto) {
    ASSERT((pred->isTry || pred->succ.size() == 1) && pred->stmtNodeList.last && (pred->stmtNodeList.last->op == OP_goto), "");
    /* delete goto stmt and make pred is fallthru */
    pred->RemoveLastStmt();
    pred->kind = kBBFallthru;
  } else if (pred->kind == kBBCondGoto) {
    /* if replaced bb is goto target */
    ASSERT((pred->isTry || pred->succ.size() == 2) && pred->stmtNodeList.last, "");
    CondGotoNode *cgotostmt = static_cast<CondGotoNode *>(static_cast<base_node_t *>(pred->stmtNodeList.last));
    if (latchbb == pred->succ.at(1)) {
      /* latchbb is the new target */
      func->CreateBBLabel(latchbb);
      cgotostmt->offset = latchbb->bbLabel;
    }
  } else if (pred->kind == kBBFallthru) {
    /* donothing */
  } else if (pred->kind == kBBSwitch) {
    ASSERT(pred->stmtNodeList.last != nullptr, "");
    SwitchNode *switchstmt = static_cast<SwitchNode *>(static_cast<base_node_t *>(pred->stmtNodeList.last));
    ASSERT(bb->bbLabel != 0, "");
    LabelIdx oldlabIdx = bb->bbLabel;
    func->CreateBBLabel(latchbb);
    if (switchstmt->defaultLabel == oldlabIdx) {
      switchstmt->defaultLabel = latchbb->bbLabel;
    }
    for (uint32_t i = 0; i < switchstmt->switchTable.size(); i++) {
      LabelIdx lblidx = switchstmt->switchTable[i].second;
      if (lblidx == oldlabIdx) {
        switchstmt->switchTable[i] = make_pair(switchstmt->switchTable[i].first, latchbb->bbLabel);
      }
    }
  } else {
    ASSERT(0, "TODO:: unexpected pred kind");
  }
  /* clone instructions of bb to latchbb */
  func->CloneBasicBlock(latchbb, bb);
  /* clone bb's succ to latchbb */
  for (BB *succ : bb->succ) {
    ASSERT(!latchbb->isTry || latchbb->succ.size() == 0,
            "loopcanon TODO: tryblock should insert succ before handler");
    latchbb->AddSuccBB(succ);
  }
  latchbb->kind = bb->kind;
  /* swap latchbb's succ if needed */
  if (swapsuccofLatch) {
    /* modify condbr stmt */
    ASSERT(latchbb->kind == kBBCondGoto, "");
    CondGotoNode *cgotostmt = static_cast<CondGotoNode *>(static_cast<base_node_t *>(latchbb->stmtNodeList.last));
    ASSERT(cgotostmt->IsCondBr(), "");
    cgotostmt->op = ((cgotostmt->op == OP_brfalse) ? OP_brtrue : OP_brfalse);
    BB *fallthru = latchbb->succ[0];
    if (fallthru->bbLabel == 0) {
      func->CreateBBLabel(fallthru);
    }
    cgotostmt->offset = fallthru->bbLabel;
    /* swap succ */
    BB *tmp = latchbb->succ[0];
    latchbb->succ[0] = latchbb->succ[1];
    latchbb->succ[1] = tmp;
  }
  return;
}

AnalysisResult *MeDoLoopCanon::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
  CHECK_FATAL(dom != nullptr, "dom is null in MeDoLoopCanon::Run");

  // set MirCFG's hasDoWhile flag
  MirCFG *cfg = func->theCFG;
  for (BB *bb : func->bbVec) {
    if (bb == nullptr || bb == func->commonEntryBB || bb == func->commonExitBB) {
      continue;
    }
    if (bb->kind != kBBCondGoto) {
      continue;
    }
    StmtNode *stmt = bb->stmtNodeList.last;
    if (stmt == nullptr) {
      continue;
    }
    CondGotoNode *condbr = dynamic_cast<CondGotoNode *>(stmt);
    if (condbr == nullptr) {
      continue;
    }
    BB *brTargetbb = bb->succ[1];
    if (dom->Dominate(brTargetbb, bb)) {
      cfg->hasDoWhile = true;
      break;
    }
  }

  MemPool *loopcanonMp = mempoolctrler.NewMemPool(PhaseName().c_str());
  MapleAllocator localAlloc(loopcanonMp);

  MapleVector<pair<BB *, BB *>> backedges(localAlloc.Adapter());
  typedef pair<BB *, BB *> Key;
  MapleMap<Key, bool> swapSuccs(std::less<Key>(), localAlloc.Adapter());

  /* collect backedge first: if bb dominator its pred, then the edge pred->bb is a backedge */
  for (BB *bb : func->bbVec) {
    if (bb == nullptr || bb == func->commonEntryBB || bb == func->commonExitBB) {
      continue;
    }
    MapleVector<BB *> &preds = bb->pred;
    for (BB *pred : preds) {
      ASSERT(func->commonEntryBB && pred, "");
      /* bb is reachable from entry && bb dominator pred */
      if (dom->Dominate(func->commonEntryBB, bb) && dom->Dominate(bb, pred) && !pred->wontExit &&
          (NeedConvert(bb, pred, localAlloc, swapSuccs))) {
        if (DEBUGFUNC(func)) {
          LogInfo::MapleLogger() << "find backedge " << bb->id.idx << " <-- " << pred->id.idx << endl;
        }
        backedges.push_back(make_pair(bb, pred));
      }
    }
  }

  /* l with the edge which shared bb is used as pred
   * if backedge 4->3 is converted first, it will create a new backedge
   * <new latchbb-> BB1>, which needs iteration to deal with.
   *                 1  <---
   *               /  \    |
   *              6   2    |
   *                  /    |
   *          |---> 3 -----|
   *          |     |
   *          ------4
   */
  sort(backedges.begin(), backedges.end(), CompareBackedge);
  if (backedges.size() > 0) {
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "-----------------Dump mefunction before loop convert----------\n";
      func->DumpFunctionNoSSA();
    }
    for (auto it = backedges.begin(); it != backedges.end(); it++) {
      Convert(func, (*it).first, (*it).second, swapSuccs);
      if (DEBUGFUNC(func)) {
        LogInfo::MapleLogger() << "-----------------Dump mefunction after loop convert-----------\n";
        func->DumpFunctionNoSSA();
      }
    }
    m->InvalidAnalysisResult(MeFuncPhase_DOMINANCE, func);
  }
  mempoolctrler.DeleteMemPool(loopcanonMp);
  return nullptr;
}

}  // namespace maple
