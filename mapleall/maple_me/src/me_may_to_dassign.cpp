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

#include "me_may_to_dassign.h"
#include "me_hdse.h"

// this phase converts all maydassign back to dassign

namespace maple {

bool May2Dassign::Doit() {
  bool changed = false;
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->meStmtList) {
      if (stmt->op != OP_maydassign) {
        continue;
      }
      MaydassignMeStmt *mass = static_cast<MaydassignMeStmt *>(stmt);
      // chiList for Maydassign has only 1 element
      CHECK_FATAL(mass->chiList.size() > 0, "chiList is empty in Doit");
      VarMeExpr *thelhs = static_cast<VarMeExpr *>(mass->chiList.begin()->second->lhs);
      DassignMeStmt *dass = static_cast<DassignMeStmt *>(irMap->CreateAssignMeStmt(thelhs, mass->rhs, bb));
      dass->needDecref = mass->needDecref;
      dass->needIncref = mass->needIncref;
      dass->wasMayDassign = true;
      dass->chiList = mass->chiList;
      dass->chiList.clear();
      bb->ReplaceMeStmt(mass, dass);
      changed = true;
    }
  }
  return changed;
}

AnalysisResult *MeDoMay2Dassign::Run(MeFunction *func, MeFuncResultMgr *m) {
  May2Dassign may2dassign(func);
  may2dassign.Doit();

  return nullptr;
}

}  // namespace maple
