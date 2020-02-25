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

#ifndef MAPLE_ME_INCLUDE_DOMINANCE_H
#define MAPLE_ME_INCLUDE_DOMINANCE_H
#include "phase.h"
#include "bb.h"

namespace maple {

class Dominance : public AnalysisResult {
  // dominator tree
 protected:
  MapleAllocator domAllocator;  // stores the analysis results
  MapleAllocator tmpAllocator;  // can be freed after dominator computation

 public:
  MapleVector<BB *> &bbVec;
  BB *commonEntryBB;
  BB *commonExitBB;

 protected:
  MapleVector<int32> postOrderIDVec;   // index is bb id
  MapleVector<BB *> reversePostOrder;  // an ordering of the BB in reverse postorder
  MapleVector<BB *> doms;               // index is bb id; immediate dominator for each BB

  // following is for post-dominance
  MapleVector<int32> pdomPostOrderIDVec;   // index is bb id
  MapleVector<BB *> pdomReversePostOrder;  // an ordering of the BB in reverse postorder
  MapleVector<BB *> pdoms;                   // index is bb id; immediate dominator for each BB

 public:
  MapleVector<MapleSet<BBId>> domFrontier;   // index is bb id
  MapleVector<MapleSet<BBId>> domChildren;   // index is bb id; for dom tree
  MapleVector<MapleSet<BBId>> iterDomFrontier;   // index is bb id
  MapleVector<BBId> dtPreOrder;             // ordering of the BBs in a preorder traversal of the dominator tree
  MapleVector<uint32> dtDfn;                  // gives position of each BB in dtPreOrder
  MapleVector<MapleSet<BBId>> pdomFrontier;  // index is bb id
  MapleVector<MapleSet<BBId>> pdomChildren;  // index is bb id; for pdom tree
  MapleVector<MapleSet<BBId>> iterPdomFrontier;  // index is bb id
  MapleVector<BBId> pdtPreOrder;            // ordering of the BBs in a preorder traversal of the post-dominator tree
  MapleVector<uint32> pdtDfn;                 // gives position of each BB in pdtPreOrder

  explicit Dominance(MemPool *mp, MemPool *tmppool, MapleVector<BB *> *bbVec, BB *commonEntryBb, BB *commonExitBb)
    : AnalysisResult(mp),
      domAllocator(mp),
      tmpAllocator(tmppool),
      bbVec(*bbVec),
      commonEntryBB(commonEntryBb),
      commonExitBB(commonExitBb),
      postOrderIDVec(bbVec->size(), -1, tmpAllocator.Adapter()),
      reversePostOrder(tmpAllocator.Adapter()),
      doms(bbVec->size(), nullptr, domAllocator.Adapter()),
      pdomPostOrderIDVec(bbVec->size(), -1, tmpAllocator.Adapter()),
      pdomReversePostOrder(tmpAllocator.Adapter()),
      pdoms(bbVec->size(), nullptr, domAllocator.Adapter()),
      domFrontier(bbVec->size(), MapleSet<BBId>(std::less<BBId>(), domAllocator.Adapter()),
                  domAllocator.Adapter()),
      domChildren(bbVec->size(), MapleSet<BBId>(std::less<BBId>(), domAllocator.Adapter()),
                  domAllocator.Adapter()),
      iterDomFrontier(bbVec->size(), MapleSet<BBId>(std::less<BBId>(), domAllocator.Adapter()),
                  domAllocator.Adapter()),
      dtPreOrder(bbVec->size(), BBId(0), domAllocator.Adapter()),
      dtDfn(bbVec->size(), -1, domAllocator.Adapter()),
      pdomFrontier(bbVec->size(), MapleSet<BBId>(std::less<BBId>(), domAllocator.Adapter()),
                   domAllocator.Adapter()),
      pdomChildren(bbVec->size(), MapleSet<BBId>(std::less<BBId>(), domAllocator.Adapter()),
                   domAllocator.Adapter()),
      iterPdomFrontier(bbVec->size(), MapleSet<BBId>(std::less<BBId>(), domAllocator.Adapter()),
                   domAllocator.Adapter()),
      pdtPreOrder(bbVec->size(), BBId(0), domAllocator.Adapter()),
      pdtDfn(bbVec->size(), -1, domAllocator.Adapter()) {}

  ~Dominance() {}

 protected:
  void PostOrderWalk(BB *bb, int32 &pid, std::vector<bool> &visitedMap);
  BB *Intersect(BB *bb1, const BB *bb2);
  bool CommonEntryBBIsPred(const BB *bb);

  void PdomPostOrderWalk(BB *bb, int32 &pid, std::vector<bool> &visitedMap);
  BB *PdomIntersect(BB *bb1, const BB *bb2);

 public:
  void GenPostOrderID();
  void ComputeDominance();
  void ComputeDomFrontiers();
  void ComputeDomChildren();
  void GetIterDomFrontier(BB *bb, MapleSet<BBId> *dfset, uint32 bbidMarker, std::vector<bool> &visitedMap);
  void ComputeIterDomFrontiers();
  void ComputeDtPreorder(const BB *bb, uint32 &num);
  void ComputeDtDfn();
  bool Dominate(const BB *b1, BB *b2);  // true if b1 dominates b2
  void DumpDoms();

  void PdomGenPostOrderID();
  void ComputePostDominance();
  void ComputePdomFrontiers();
  void ComputePdomChildren();
  void GetIterPdomFrontier(BB *bb, MapleSet<BBId> *dfset, uint32 bbidMarker, std::vector<bool> &visitedMap);
  void ComputeIterPdomFrontiers();
  void ComputePdtPreorder(const BB *bb, uint32 &num);
  void ComputePdtDfn();
  bool Postdominate(const BB *b1, BB *b2);  // true if b1 postdominates b2
  void DumpPdoms();
  const MapleVector<BB *> &Getdoms() const {
    return doms;
  }
};
}  // namespace maple
#endif
