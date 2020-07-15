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

#include "me_irmap_build.h"
#include "irmap_build.h"
#include "me_prop.h"

// This phase converts Maple IR to MeIR.

namespace maple {

AnalysisResult *MeDoIrMapBuild::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
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
  std::vector<bool> bbIrmapProcessed(func->NumBBs(), false);
  irmapbuild.BuildBB(func->commonEntryBB, bbIrmapProcessed);
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
  for (BB *bb : func->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    bb->phiList.clear();
    bb->stmtNodeList.clear();
  }
  mempoolctrler.DeleteMemPool(func->meSSATab->vers_mp);
  mempoolctrler.DeleteMemPool(propMp);
  return irMap;
}

}  // namespace maple
