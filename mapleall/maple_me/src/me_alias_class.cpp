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

#include <cstdlib>
#include "me_option.h"
#include "mpl_logging.h"
#include "me_alias_class.h"
#include "ssa_mir_nodes.h"
#include "ssa_tab.h"
#include "me_function.h"
#include "me_cfg.h"
#include "mpl_timer.h"

using namespace std;

namespace maple {

// This phase performs alias analysis based on Steensgaard's algorithm and
// represent the resulting alias relationships in the Maple IR representation

void MeAliasClass::PerformAliasClass() {
  MPLTimer timer;
  timer.Start();

  // pass 1 through the program statements
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============ Alias Classification Pass 1 ============" << endl;
  }
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->stmtNodeList) {
      ApplyUnionForCopies(stmt);
    }
  }

  CreateAssignSets();
  if (DEBUGFUNC(func)) {
    DumpAssignSets();
  }

  unionfind.Reinit();

  if (MeOption::noSteensgaard) {
    UnionAllPointedTos();
  } else {
    ApplyUnionForPointedTos();
    UnionForNotAllDefsSeen();
    if (mirModule->IsCModule()) {
      ApplyUnionForStorageOverlaps();
    }
  }

  // TBAA
  if (!MeOption::noTBAA && !mirModule->IsCModule()) {
    ReconstructAliasGroups();
  }

  CreateClassSets();
  if (DEBUGFUNC(func)) {
    DumpClassSets();
  }

  // pass 2 through the program statements
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============ Alias Classification Pass 2 ============" << endl;
  }
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->stmtNodeList) {
      GenericInsertMayDefUse(stmt, bb->id);
    }
  }
  timer.Stop();
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "ssaTab + aliasclass passes consume cumulatively " << timer.Elapsed() << "seconds " << endl;
  }
}

AnalysisResult *MeDoAliasClass::Run(MeFunction *func, MeFuncResultMgr *m, ModuleResultMgr *mrm) {
  MirCFG *cfg = static_cast<MirCFG *>(m->GetAnalysisResult(MeFuncPhase_CFGBUILD, func));
  SSATab *ssaTab = static_cast<SSATab *>(m->GetAnalysisResult(MeFuncPhase_SSATAB, func));
  ASSERT(ssaTab != nullptr, "ssatbb phase has problem");
  MemPool *aliasclassmp = mempoolctrler.NewMemPool(PhaseName().c_str());

  KlassHierarchy *kh = static_cast<KlassHierarchy *>(mrm->GetAnalysisResult(MoPhase_CHA, &func->mirModule));

  MeAliasClass *aliasclass = aliasclassmp->New<MeAliasClass>(
    aliasclassmp, &func->mirModule, func->meSSATab, func, MeOption::lessThrowAlias, MeOption::finalFieldAlias,
    MeOption::ignoreIPA, DEBUGFUNC(func), MeOption::setCalleeHasSideEffect, kh);

  aliasclass->PerformAliasClass();

  return aliasclass;
}

}  // namespace maple
