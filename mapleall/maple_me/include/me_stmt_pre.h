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

#ifndef MAPLE_ME_INCLUDE_ME_STMT_PRE_H
#define MAPLE_ME_INCLUDE_ME_STMT_PRE_H
#include "me_function.h"
#include "me_irmap.h"
#include "ssa_epre.h"

namespace maple {
class MeStmtPre : public SSAEPre {
 protected:
  MeFunction *func;
  MapleVector<MapleStack<ScalarMeExpr *> *> versionStackVec;  // top of stack gives last version during BuildWorkList()
  MapleMap<OStIdx, MapleSet<uint32> *> use_occur_map;  // give the set of BBs (in dfn) that contain uses of the symbol
 public:
  // a symbol is a candidate for ssaupdate if its ostidx key exists in the map;
  // the mapped set gives bbs where dassign's are inserted by stmtpre for the symbol
  MapleMap<OStIdx, MapleSet<BBId> *> cands_for_ssaupdate;

  explicit MeStmtPre(MeFunction *f, IRMap *map, Dominance *dom, MemPool *mp, MemPool *mp2, uint32 limit)
    : SSAEPre(map, dom, mp, mp2, kStmtPre, limit, true, false),
      func(f),
      versionStackVec(ssaTab->originalStTable.original_st_vector_.size(), nullptr, ssapre_allocator.Adapter()),
      use_occur_map(std::less<OStIdx>(), ssapre_allocator.Adapter()),
      cands_for_ssaupdate(std::less<OStIdx>(), ssapre_allocator.Adapter()) {}

  bool ScreenPhiBB(BBId bbid) {
    return true;
  }

 private:
  // code motion phase
  void CodeMotion();
  // finalize phase
  void Finalize1();
  void Finalize2(){};
  // fully available (replaces downsafety, canbeavail and later under SSAFRE)
  void ResetFullyAvail(MePhiOcc *occg);
  void ComputeFullyAvail();
  // rename phase
  bool AllVarsSameVersion(MeRealOcc *realocc1, MeRealOcc *realocc2);
  bool AllVarsSameVersionStmtFre(MeRealOcc *topocc, MeRealOcc *curocc) const;
  void CollectVarForMeStmt(MeStmt *mestmt, MeExpr *meexpr, std::vector<MeExpr *> &varvec);
  void CollectVarForCand(MeRealOcc *realocc, std::vector<MeExpr *> &varvec);
  MeStmt *PhiOpndFromRes4Stmt(MeRealOcc *realz, uint32 j, MeExpr *&lhsVar);
  void Rename1StmtFre();
  void Rename2();
  // phi insertion phase
  void ComputeVarAndDfPhis();
  void CreateSortedOccs();

  void ConstructUseOccurMapExpr(uint32 bbdfn, MeExpr *x);
  void ConstructUseOccurMap();  // build use_occur_map for dassign candidates
  PreStmtWorkCand *CreateStmtRealOcc(MeStmt *mestmt, int seqstmt);
  void VersionStackChiListUpdate(const MapleMap<OStIdx, ChiMeNode *> &chilist);
  void BuildWorkListBB(BB *bb);
  void BuildWorkList();
  void RemoveUnecessaryDassign(DassignMeStmt *);
  void DoSSAFRE();
  BB *GetBB(BBId id) {
    return func->bbVec[id.idx];
  }

  PUIdx GetPuidx(MeStmt *stmt) {
    return func->mirFunc->puIdx;
  }

  PUIdx GetPuidx(BB *bb) {
    return func->mirFunc->puIdx;
  }
};

class MeDoStmtPre : public MeFuncPhase {
 public:
  MeDoStmtPre(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "stmtpre";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_STMT_PRE_H
