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

#ifndef MAPLE_ME_INCLUDE_ME_SSA_EPRE_H
#define MAPLE_ME_INCLUDE_ME_SSA_EPRE_H
#include "me_function.h"
#include "me_irmap.h"
#include "me_cfg.h"
#include "ssa_epre.h"

namespace maple {
class MeSSAEPre : public SSAEPre {
 protected:
  MeFunction *func;
  bool epre_localrefvar;

 public:
  // a symbol is a candidate for ssaupdate if its ostidx key exists in the map;
  // the mapped set gives bbs where dassign's are inserted by ssaepre for the symbol
  MapleMap<OStIdx, MapleSet<BBId> *> cands_for_ssaupdate;

  explicit MeSSAEPre(MeFunction *f, IRMap *map, Dominance *dom, MemPool *mp, MemPool *mp2, uint32 limit,
                     bool includeRef, bool eprelocalrefvar, bool lhsivar)
    : SSAEPre(map, dom, mp, mp2, kExprPre, limit, includeRef, lhsivar),
      func(f),
      epre_localrefvar(eprelocalrefvar),
      cands_for_ssaupdate(std::less<OStIdx>(), ssapre_allocator.Adapter()) {
    placementrc_on = f->placementRCOn;
  }

  bool ScreenPhiBB(BBId bbid) {
    return true;
  }

 private:
  void BuildWorkList();
  BB *GetBB(BBId id) {
    CHECK(id.idx < func->theCFG->bbVec.size(), "index out of range in MeSSAEPre::GetBB");
    return func->theCFG->bbVec[id.idx];
  }

  PUIdx GetPuidx(MeStmt *stmt) {
    return func->mirFunc->puIdx;
  }

  PUIdx GetPuidx(BB *bb) {
    return func->mirFunc->puIdx;
  }

  bool CfgHasDoWhile() {
    return func->theCFG->hasDoWhile;
  }

  bool EpreLocalrefvar() {
    return epre_localrefvar;
  }

  void EnterCandsForSsaupdate(OStIdx oidx, BB *bb) {
    if (cands_for_ssaupdate.find(oidx) == cands_for_ssaupdate.end()) {
      MapleSet<BBId> *bbset = ssapre_mp->New<MapleSet<BBId>>(std::less<BBId>(), ssapre_allocator.Adapter());
      bbset->insert(bb->id);
      cands_for_ssaupdate[oidx] = bbset;
    } else {
      cands_for_ssaupdate[oidx]->insert(bb->id);
    }
  }
};

class MeDoSSAEPre : public MeFuncPhase {
 public:
  MeDoSSAEPre(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "epre";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_SSA_EPRE_H
