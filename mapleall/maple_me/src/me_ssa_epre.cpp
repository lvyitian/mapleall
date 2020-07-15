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

#include "me_dominance.h"
#include "me_ssa_epre.h"
#include "me_ssa_update.h"

// accumulate the BBs that are in the iterated dominance frontiers of bb in
// the set dfset, visiting each BB only once

namespace maple {

void MeSSAEPre::BuildWorkList() {
  const MapleVector<BBId> &preorderDt = dominance->dtPreOrder;
  for (uint32 i = 0; i < preorderDt.size(); i++) {
    BB *bb = func->bbVec[preorderDt[i].idx];
    BuildWorkListBB(bb);
  }
}

AnalysisResult *MeDoSSAEPre::Run(MeFunction *func, MeFuncResultMgr *m) {
  static uint32 pUcount = 0;  // count PU to support the eprePULimit option
  if (pUcount > MeOption::eprePULimit) {
    pUcount++;
    return nullptr;
  }
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
  ASSERT(dom != nullptr, "dominance phase has problem");

  MeIRMap *irMap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func));
  ASSERT(irMap != nullptr, "irMap phase has problem");
  MemPool *ssapremp = mempoolctrler.NewMemPool(PhaseName().c_str());
  MemPool *percandmp = mempoolctrler.NewMemPool("Per EPRE Candidate");

  bool eprePuLimitSpecified = MeOption::eprePULimit != UINT32_MAX;
  uint32 eprelimitUsed =
    (eprePuLimitSpecified && pUcount != MeOption::eprePULimit) ? UINT32_MAX : MeOption::epreLimit;
  MeSSAEPre ssapre(func, irMap, dom, ssapremp, percandmp, eprelimitUsed, MeOption::epreIncludeRef,
                   MeOption::eprelocalrefvar, MeOption::eprelhsivar);
  if (MeOption::spillatcatch) {
    ssapre.spillatcatch = true;
  }
  if (MeOption::placementrc) {
    ssapre.placementrc_on = true;
  }
  if (MeOption::strengthreduction) {
    ssapre.strengthreduction = true;
  }
  if (eprePuLimitSpecified && pUcount == MeOption::eprePULimit && eprelimitUsed != UINT32_MAX)
    LogInfo::MapleLogger() << "applying EPRE limit " << eprelimitUsed << " in function " << func->mirFunc->GetName() << std::endl;

  MIRFunction *mirfunction = func->mirFunc;
  if (DEBUGFUNC(func)) {
    ssapre.ssapredebug = true;
  }

  ssapre.ApplySSAPRE();

  if (!ssapre.cands_for_ssaupdate.empty()) {
    SSAUpdate ssaupdate(func, func->meSSATab, dom, &ssapre.cands_for_ssaupdate);
    ssaupdate.Run();
  }

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============== EPRE =============" << std::endl;
    irMap->Dump();
  }

  mempoolctrler.DeleteMemPool(ssapremp);
  mempoolctrler.DeleteMemPool(percandmp);

  pUcount++;
  return nullptr;
}

}  // namespace maple
