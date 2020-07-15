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

#include "dominance.h"
#include <iostream>

/* ================= for Dominance ================= */

namespace maple {

void Dominance::PostOrderWalk(BB *bb, int32 &pid, std::vector<bool> &visitedMap) {
  CHECK_FATAL(bb->id.idx < visitedMap.size(), "index out of range in Dominance::PostOrderWalk");
  if (visitedMap[bb->id.idx]) {
    return;
  }
  visitedMap[bb->id.idx] = true;
  for (BB *suc : bb->succ) {
    PostOrderWalk(suc, pid, visitedMap);
  }
  CHECK_FATAL(bb->id.idx < postOrderIDVec.size(), "index out of range in Dominance::PostOrderWalk");
  postOrderIDVec[bb->id.idx] = pid++;
  return;
}

void Dominance::GenPostOrderID() {
  CHECK_FATAL(bbVec.size() > 0, "size to be allocated is 0");
  std::vector<bool> visitedMap(bbVec.size(), false);
  int32 pid = 0;
  PostOrderWalk(commonEntryBB, pid, visitedMap);

  // initialize reversePostOrder
  int32 maxPid = pid - 1;
  reversePostOrder.resize(maxPid + 1);
  for (uint32 i = 0; i < postOrderIDVec.size(); i++) {
    int32 postorderNo = postOrderIDVec[i];
    if (postorderNo == -1) {
      continue;
    }
    reversePostOrder[maxPid - postorderNo] = bbVec[i];
  }
}

BB *Dominance::Intersect(BB *bb1, const BB *bb2) {
  while (bb1 != bb2) {
    while (postOrderIDVec[bb1->id.idx] < postOrderIDVec[bb2->id.idx]) {
      bb1 = doms[bb1->id.idx];
    }
    while (postOrderIDVec[bb2->id.idx] < postOrderIDVec[bb1->id.idx]) {
      bb2 = doms[bb2->id.idx];
    }
  }
  return bb1;
}

bool Dominance::CommonEntryBBIsPred(const BB *bb) {
  for (BB *suc : commonEntryBB->succ)
    if (suc == bb) {
      return true;
    }
  return false;
}

// Figure 3 in "A Simple, Fast Dominance Algorithm" by Keith Cooper et al.
void Dominance::ComputeDominance() {
  doms.at(commonEntryBB->id.idx) = commonEntryBB;
  bool changed;
  do {
    changed = false;
    for (uint32 i = 1; i < reversePostOrder.size(); i++) {
      BB *bb = reversePostOrder[i];
      BB *pre;
      if (CommonEntryBBIsPred(bb) || bb->pred.size() == 0) {
        pre = commonEntryBB;
      } else {
        pre = bb->pred[0];
      }
      uint32 j = 1;
      while ((doms[pre->id.idx] == nullptr || pre == bb) && j < bb->pred.size()) {
        pre = bb->pred[j];
        j++;
      }
      BB *newIdom = pre;
      for (; j < bb->pred.size(); j++) {
        pre = bb->pred[j];
        if (doms[pre->id.idx] != nullptr && pre != bb) {
          newIdom = Intersect(pre, newIdom);
        }
      }
      if (doms[bb->id.idx] != newIdom) {
        doms[bb->id.idx] = newIdom;
        changed = true;
      }
    }
  } while (changed);
}

// Figure 5 in "A Simple, Fast Dominance Algorithm" by Keith Cooper et al.
void Dominance::ComputeDomFrontiers() {
  for (BB *bb : bbVec) {
    if (bb == nullptr || bb == commonExitBB) {
      continue;
    }
    if (bb->pred.size() < 2) {
      continue;
    }
    for (BB *pre : bb->pred) {
      BB *runner = pre;
      while (runner != doms[bb->id.idx] && runner != commonEntryBB) {
        domFrontier[runner->id.idx].insert(bb->id);
        runner = doms[runner->id.idx];
      }
    }
  }
}

void Dominance::ComputeDomChildren() {
  for (BB *bb : bbVec) {
    if (bb == nullptr) {
      continue;
    }
    if (doms[bb->id.idx] == nullptr) {
      continue;
    }
    BB *parent = doms[bb->id.idx];
    if (parent == bb) {
      continue;
    }
    domChildren[parent->id.idx].insert(bb->id);
  }
}

// bbidMarker indicates that the iterDomFrontier results for bbid < bbidMarker
// have been computed
void Dominance::GetIterDomFrontier(BB *bb, MapleSet<BBId> *dfset, uint32 bbidMarker, std::vector<bool> &visitedMap) {
  if (visitedMap[bb->id.idx]) {
    return;
  }
  visitedMap[bb->id.idx] = true;
  for (BBId frontierbbid : domFrontier[bb->id.idx]) {
    dfset->insert(frontierbbid);
    if (frontierbbid.idx < bbidMarker) {  // union with its computed result
      dfset->insert(iterDomFrontier[frontierbbid.idx].begin(), iterDomFrontier[frontierbbid.idx].end());
    } else {  // recursive call
      BB *frontierbb = bbVec[frontierbbid.idx];
      GetIterDomFrontier(frontierbb, dfset, bbidMarker, visitedMap);
    }
  }
}

void Dominance::ComputeIterDomFrontiers() {
  for (BB *bb : bbVec) {
    if (bb == nullptr || bb == commonExitBB) {
      continue;
    }
    std::vector<bool> visitedMap(bbVec.size(), false);
    GetIterDomFrontier(bb, &iterDomFrontier[bb->id.idx], bb->id.idx, visitedMap);
  }
}

void Dominance::ComputeDtPreorder(const BB *bb, uint32 &num) {
  CHECK(num < dtPreOrder.size(), "index out of range in Dominance::ComputeDtPreorder");
  dtPreOrder[num++] = bb->id;
  for (BBId k : domChildren[bb->id.idx]) {
    ComputeDtPreorder(bbVec[k.idx], num);
  }
}

void Dominance::ComputeDtDfn() {
  for (uint32 i = 0; i < dtPreOrder.size(); i++) {
    dtDfn[dtPreOrder[i].idx] = i;
  }
}

// true if b1 dominates b2
bool Dominance::Dominate(const BB *b1, BB *b2) {
  if (b1 == b2) {
    return true;
  }
  CHECK(b2->id.idx < doms.size(), "index out of range in Dominance::Dominate ");
  if (doms[b2->id.idx] == nullptr) {
    return false;
  }
  BB *imdom = b2;
  do {
    imdom = doms[imdom->id.idx];
    if (imdom == b1) {
      return true;
    }
    CHECK_FATAL(imdom != nullptr, "");
  } while (imdom != commonEntryBB);
  return false;
}

/* ================= for PostDominance ================= */

void Dominance::PdomPostOrderWalk(BB *bb, int32 &pid, std::vector<bool> &visitedMap) {
  CHECK_FATAL(bb->id.idx < visitedMap.size(), "index out of range in  Dominance::PdomPostOrderWalk");
  if (bbVec[bb->id.idx] == nullptr) {
    return;
  }
  if (visitedMap[bb->id.idx]) {
    return;
  }
  visitedMap[bb->id.idx] = true;
  for (BB *pre : bb->pred) {
    PdomPostOrderWalk(pre, pid, visitedMap);
  }
  CHECK(bb->id.idx < pdomPostOrderIDVec.size(), "index out of range in  Dominance::PdomPostOrderWalk");
  pdomPostOrderIDVec[bb->id.idx] = pid++;
  return;
}

void Dominance::PdomGenPostOrderID() {
  CHECK_FATAL(bbVec.size() > 0, "call calloc failed in Dominance::PdomGenPostOrderID");
  std::vector<bool> visitedMap(bbVec.size(), false);
  int32 pid = 0;
  PdomPostOrderWalk(commonExitBB, pid, visitedMap);

  // initialize pdomReversePostOrder
  int32 maxPid = pid - 1;
  pdomReversePostOrder.resize(maxPid + 1);
  for (uint32 i = 0; i < pdomPostOrderIDVec.size(); i++) {
    int32 postorderNo = pdomPostOrderIDVec[i];
    if (postorderNo == -1) {
      continue;
    }
    pdomReversePostOrder[maxPid - postorderNo] = bbVec[i];
  }
}

BB *Dominance::PdomIntersect(BB *bb1, const BB *bb2) {
  while (bb1 != bb2) {
    while (pdomPostOrderIDVec[bb1->id.idx] < pdomPostOrderIDVec[bb2->id.idx]) {
      bb1 = pdoms[bb1->id.idx];
    }
    while (pdomPostOrderIDVec[bb2->id.idx] < pdomPostOrderIDVec[bb1->id.idx]) {
      bb2 = pdoms[bb2->id.idx];
    }
  }
  return bb1;
}

// Figure 3 in "A Simple, Fast Dominance Algorithm" by Keith Cooper et al.
void Dominance::ComputePostDominance() {
  pdoms.at(commonExitBB->id.idx) = commonExitBB;
  bool changed;
  do {
    changed = false;
    for (uint32 i = 1; i < pdomReversePostOrder.size(); i++) {
      BB *bb = pdomReversePostOrder[i];
      BB *suc;
      if (bb->IsExit() || bb->succ.size() == 0) {
        suc = commonExitBB;
      } else {
        suc = bb->succ[0];
      }
      uint32 j = 1;
      while ((pdoms[suc->id.idx] == nullptr || suc == bb) && j < bb->succ.size()) {
        suc = bb->succ[j];
        j++;
      }
      if (pdoms[suc->id.idx] == nullptr) {
        suc = commonExitBB;
      }
      BB *newIdom = suc;
      for (; j < bb->succ.size(); j++) {
        suc = bb->succ[j];
        if (pdoms[suc->id.idx] != nullptr && suc != bb) {
          newIdom = PdomIntersect(suc, newIdom);
        }
      }
      if (pdoms[bb->id.idx] != newIdom) {
        pdoms[bb->id.idx] = newIdom;
        ASSERT(pdoms[newIdom->id.idx] != nullptr, "");
        changed = true;
      }
    }
  } while (changed);
}

// Figure 5 in "A Simple, Fast Dominance Algorithm" by Keith Cooper et al.
void Dominance::ComputePdomFrontiers() {
  for (BB *bb : bbVec) {
    if (bb == nullptr || bb == commonEntryBB) {
      continue;
    }
    if (bb->succ.size() < 2) {
      continue;
    }
    for (BB *suc : bb->succ) {
      BB *runner = suc;
      while (runner != pdoms[bb->id.idx] && runner != commonExitBB) {
        pdomFrontier[runner->id.idx].insert(bb->id);
        ASSERT(pdoms[runner->id.idx] != nullptr, "ComputePdomFrontiers: pdoms[] is nullptr");
        runner = pdoms[runner->id.idx];
      }
    }
  }
}

void Dominance::ComputePdomChildren() {
  for (BB *bb : bbVec) {
    if (bb == nullptr) {
      continue;
    }
    if (pdoms[bb->id.idx] == nullptr) {
      continue;
    }
    BB *parent = pdoms[bb->id.idx];
    if (parent == bb) {
      continue;
    }
    pdomChildren[parent->id.idx].insert(bb->id);
  }
}

// bbidMarker indicates that the iterPdomFrontier results for bbid < bbidMarker
// have been computed
void Dominance::GetIterPdomFrontier(BB *bb, MapleSet<BBId> *dfset, uint32 bbidMarker, std::vector<bool> &visitedMap) {
  if (visitedMap[bb->id.idx]) {
    return;
  }
  visitedMap[bb->id.idx] = true;
  for (BBId frontierbbid : pdomFrontier[bb->id.idx]) {
    dfset->insert(frontierbbid);
    if (frontierbbid.idx < bbidMarker) {  // union with its computed result
      dfset->insert(iterPdomFrontier[frontierbbid.idx].begin(), iterPdomFrontier[frontierbbid.idx].end());
    } else {  // recursive call
      BB *frontierbb = bbVec[frontierbbid.idx];
      GetIterPdomFrontier(frontierbb, dfset, bbidMarker, visitedMap);
    }
  }
}

void Dominance::ComputeIterPdomFrontiers() {
  for (BB *bb : bbVec) {
    if (bb == nullptr || bb == commonEntryBB) {
      continue;
    }
    std::vector<bool> visitedMap(bbVec.size(), false);
    GetIterPdomFrontier(bb, &iterPdomFrontier[bb->id.idx], bb->id.idx, visitedMap);
  }
}

void Dominance::ComputePdtPreorder(const BB *bb, uint32 &num) {
  CHECK_FATAL(num < pdtPreOrder.size(), "index out of range in Dominance::ComputePdtPreorder");
  pdtPreOrder[num++] = bb->id;
  for (BBId k : pdomChildren[bb->id.idx]) {
    ComputePdtPreorder(bbVec[k.idx], num);
  }
}

void Dominance::ComputePdtDfn() {
  for (uint32 i = 0; i < pdtPreOrder.size(); i++) {
    pdtDfn[pdtPreOrder[i].idx] = i;
  }
}

// true if b1 postdominates b2
bool Dominance::Postdominate(const BB *b1, BB *b2) {
  if (b1 == b2) {
    return true;
  }
  CHECK(b2->id.idx < pdoms.size(), "index out of range in Dominance::Postdominate");
  if (pdoms[b2->id.idx] == nullptr) {
    return false;
  }
  BB *impdom = b2;
  do {
    impdom = pdoms[impdom->id.idx];
    if (impdom == b1) {
      return true;
    }
    CHECK_FATAL(impdom != nullptr, "");
  } while (impdom != commonExitBB);
  return false;
}

void Dominance::DumpDoms() {
  for (uint32 i = 0; i < reversePostOrder.size(); i++) {
    BB *bb = reversePostOrder[i];
    LogInfo::MapleLogger() << "postorder no " << postOrderIDVec[bb->id.idx];
    LogInfo::MapleLogger() << " is bb:" << bb->id.idx;
    LogInfo::MapleLogger() << " im_dom is bb:" << doms[bb->id.idx]->id.idx;
    LogInfo::MapleLogger() << " domFrontier: [";
    for (BBId id : domFrontier[bb->id.idx]) {
      LogInfo::MapleLogger() << id.idx << " ";
    }
    LogInfo::MapleLogger() << "] iterDomFrontier: [";
    for (BBId id : iterDomFrontier[bb->id.idx]) {
      LogInfo::MapleLogger() << id.idx << " ";
    }
    LogInfo::MapleLogger() << "] domChildren: [";
    for (BBId id : domChildren[bb->id.idx]) {
      LogInfo::MapleLogger() << id.idx << " ";
    }
    LogInfo::MapleLogger() << "]" << std::endl;
  }
  LogInfo::MapleLogger() << std::endl;

  LogInfo::MapleLogger() << "preorder traversal of dominator tree:";
  for (BBId id : dtPreOrder) {
    LogInfo::MapleLogger() << id.idx << " ";
  }
  LogInfo::MapleLogger() << std::endl << std::endl;
}

void Dominance::DumpPdoms() {
  for (uint32 i = 0; i < pdomReversePostOrder.size(); i++) {
    BB *bb = pdomReversePostOrder[i];
    LogInfo::MapleLogger() << "pdom_postorder no " << pdomPostOrderIDVec[bb->id.idx];
    LogInfo::MapleLogger() << " is bb:" << bb->id.idx;
    LogInfo::MapleLogger() << " im_pdom is bb:" << pdoms[bb->id.idx]->id.idx;
    LogInfo::MapleLogger() << " pdomFrontier: [";
    for (BBId id : pdomFrontier[bb->id.idx]) {
      LogInfo::MapleLogger() << id.idx << " ";
    }
    LogInfo::MapleLogger() << "] pdomChildren: [";
    for (BBId id : pdomChildren[bb->id.idx]) {
      LogInfo::MapleLogger() << id.idx << " ";
    }
    LogInfo::MapleLogger() << "]" << std::endl;
  }
  LogInfo::MapleLogger() << std::endl;

  LogInfo::MapleLogger() << "preorder traversal of post-dominator tree:";
  for (BBId id : pdtPreOrder) {
    LogInfo::MapleLogger() << id.idx << " ";
  }
  LogInfo::MapleLogger() << std::endl << std::endl;
}

}  // namespace maple
