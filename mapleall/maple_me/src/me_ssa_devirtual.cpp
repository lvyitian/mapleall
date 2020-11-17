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

#include "me_ssa_devirtual.h"
#include "me_function.h"
#include "me_option.h"

namespace maple {

AnalysisResult *MeDoSSADevirtual::Run(MeFunction *func, MeFuncResultMgr *frm, ModuleResultMgr *mrm) {
  Dominance *dom = static_cast<Dominance *>(frm->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
  ASSERT(dom, "dominance phase has problem");
  MeIRMap *hmap = static_cast<MeIRMap *>(frm->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func));
  ASSERT(hmap, "hssamap has problem");
  CHECK_FATAL(mrm != nullptr, "Needs module result manager for ipa");
  KlassHierarchy *kh = static_cast<KlassHierarchy *>(mrm->GetAnalysisResult(MoPhase_CHA, &func->mirModule));
  ASSERT(kh != nullptr, "");
  Clone *clone = nullptr;
  if (Options::O2) {
    clone = static_cast<Clone *>(mrm->GetAnalysisResult(MoPhase_CLONE, &func->mirModule));
  }
  MemPool *ssadevirtualmp = mempoolctrler.NewMemPool(PhaseName().c_str());
  MeSSADevirtual *messadevirtual =
    ssadevirtualmp->New<MeSSADevirtual>(ssadevirtualmp, &func->mirModule, func, hmap, kh, dom, clone);

  MIRFunction *mirfunction = func->mirFunc;
  if (DEBUGFUNC(func)) {
    SSADevirtual::debug = true;
  }

  messadevirtual->Perform(func->theCFG->commonEntryBB);
  /* this is a transform phase, delete mempool */
  mempoolctrler.DeleteMemPool(ssadevirtualmp);

  if (DEBUGFUNC(func)) {
    SSADevirtual::debug = false;
    LogInfo::MapleLogger() << "\n============== After SSA Devirtualization  =============" << endl;
    hmap->Dump();
  }
  return nullptr;
}

}  // namespace maple
