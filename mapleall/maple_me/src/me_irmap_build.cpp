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

#include "me_irmap_build.h"
#include "irmap_build.h"
#include "me_ssa.h"
#include "me_prop.h"
#include "me_alias_class.h"
#include "me_dse.h"

// This phase converts Maple IR to MeIR.

namespace maple {

AnalysisResult *MeDoIrMapBuild::Run(MeFunction *func, MeFuncResultMgr *m) {
  MirCFG *cfg = static_cast<MirCFG *>(m->GetAnalysisResult(MeFuncPhase_CFGBUILD, func, !MeOption::quiet));
  ASSERT(cfg != nullptr, "cfgbuild phase has problem");
  SSATab *ssatab = static_cast<SSATab *>(m->GetAnalysisResult(MeFuncPhase_SSATAB, func, !MeOption::quiet));
  CHECK_FATAL(ssatab, "ssatab phase has problem");
  MeAliasClass *aliasclass = static_cast<MeAliasClass *>(m->GetAnalysisResult(MeFuncPhase_ALIASCLASS, func, !MeOption::quiet));
  ASSERT(aliasclass != nullptr, "aliasclass phase has problem");
  MeSSA *ssa = static_cast<MeSSA *>(m->GetAnalysisResult(MeFuncPhase_SSA, func, !MeOption::quiet));
  CHECK_FATAL(ssa, "ssa phase has problem");
  MeDSE *dse = static_cast<MeDSE *>(m->GetAnalysisResult(MeFuncPhase_DSE, func, !MeOption::quiet));
  CHECK_FATAL(dse, "dse phase has problem");
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func, !MeOption::quiet));
  CHECK_FATAL(dom, "dominance phase has problem");

  MemPool *irmapmp = mempoolctrler.NewMemPool(PhaseName().c_str());
  MemPool *tempmp = mempoolctrler.NewMemPool("meirmap temporaries");

  MeIRMap *irMap = irmapmp->New<MeIRMap>(func, dom, irmapmp, tempmp);
  func->irMap = irMap;
#if DEBUG
  g_irmap = irMap;
#endif
  // create propgation
  MemPool *propMp = mempoolctrler.NewMemPool("meirbuild prop");
  MeProp meprop(irMap, dom, propMp, false, false, false, false, false, false);
  IrMapBuild irmapbuild(irMap, &meprop);
  std::vector<bool> bbIrmapProcessed(func->theCFG->NumBBs(), false);
  irmapbuild.BuildBB(func->theCFG->commonEntryBB, bbIrmapProcessed);
  if (DEBUGFUNC(func)) {
    irMap->Dump();
  }

  // delete mempool for meirmap temporaries
  mempoolctrler.DeleteMemPool(tempmp);
  irMap->tempAlloc.SetMemPool(nullptr);
  // delete input IR code for current function
  MIRFunction *mirFunc = func->mirFunc;
  mempoolctrler.DeleteMemPool(mirFunc->codeMemPool);
  mirFunc->codeMemPool = nullptr;

// delete versionst_table
  // nullify all references to the versionst_table contents
  for (uint32 i = 0; i < func->meSSATab->versionStTable.versionStVector.size(); i++) {
    func->meSSATab->versionStTable.versionStVector[i] = nullptr;
  }
  // clear BB's phiList which uses versionst; nullify first_stmt_, last_stmt_
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    bb->phiList->clear();
    bb->phiList = nullptr;
    bb->stmtNodeList.clear();
  }
  m->InvalidAnalysisResult(MeFuncPhase_SSA, func);
  m->InvalidAnalysisResult(MeFuncPhase_DSE, func);
  mempoolctrler.DeleteMemPool(func->meSSATab->vers_mp);
  mempoolctrler.DeleteMemPool(propMp);
  return irMap;
}

}  // namespace maple
