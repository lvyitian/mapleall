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

#ifndef MAPLE_ME_INCLUDE_ME_SSA_LPRE_H
#define MAPLE_ME_INCLUDE_ME_SSA_LPRE_H
#include "me_irmap.h"
#include "ssa_pre.h"
#include "me_ident_loops.h"

const int kDolpreBbsLimit = 0x7fffff;

namespace maple {

class MeSSALPre : public SSAPre {
 public:
  MeIRMap *meirmap;
  MeFunction *func;

 public:
  explicit MeSSALPre(MeFunction *f, MeIRMap *hmap, Dominance *dom, MemPool *mp, MemPool *mp2, PreKind kind,
                     uint32 limit)
    : SSAPre(hmap, dom, mp, mp2, kind, limit),
      meirmap(hmap),
      func(f),
      assignedFormals(std::less<OStIdx>(), ssapre_allocator.Adapter()),
      loophead_bbs(ssapre_allocator.Adapter()) {}

 private:
  MapleSet<OStIdx> assignedFormals;  // set of formals that are assigned
  MapleSet<BBId> loophead_bbs;

  void GenerateSaveRealocc(MeRealOcc *);
  void GenerateReloadRealocc(MeRealOcc *);
  MeExpr *PhiOpndFromRes(MeRealOcc *, uint32);
  void ComputeVarAndDfPhis();
  bool ScreenPhiBB(BBId bbid) {
    return true;
  }

  void CollectVarForMeExpr(MeExpr *meexpr, std::vector<MeExpr *> &varvec) {
    if (meexpr->meOp == kMeOpAddrof || meexpr->meOp == kMeOpAddroffunc) {
      return;
    }
    varvec.push_back(meexpr);
  }

  void CollectVarForCand(MeRealOcc *realocc, std::vector<MeExpr *> &varvec) {
    if (realocc->meexpr->meOp == kMeOpAddrof || realocc->meexpr->meOp == kMeOpAddroffunc) {
      return;
    }
    varvec.push_back(realocc->meexpr);
  }

  void BuildEntryLhsOcc4Formals();
  bool ScreenRHS4LHSoccur(const MeExpr *rhs) const;
  void BuildWorkListLHSOcc(MeStmt *mestmt, int32 seqstmt);
  void CreateMembarOccAtCatch(BB *bb);
  void BuildWorkListExpr(MeStmt *, int32, MeExpr *, bool, MeExpr *, bool isRootExpr);
  void BuildWorkList();
  BB *GetBB(BBId id) {
    CHECK(id.idx < func->bbVec.size(), "index out of range in MeSSALPre::GetBB");
    return func->bbVec[id.idx];
  }

  PUIdx GetPuidx(MeStmt *stmt) {
    return func->mirFunc->puIdx;
  }

  PUIdx GetPuidx(BB *bb) {
    return func->mirFunc->puIdx;
  }

  bool IsLoopHeadBB(BBId bbid) {
    return loophead_bbs.find(bbid) != loophead_bbs.end();
  }

 public:
  void FindLoopHeadBBs(IdentifyLoops *identloops);
};

class MeDoSSALPre : public MeFuncPhase {
 public:
  MeDoSSALPre(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "lpre";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_SSA_LPRE_H
