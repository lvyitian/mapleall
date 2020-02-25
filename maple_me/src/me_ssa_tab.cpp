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

#include <cstdlib>
#include "mpl_timer.h"
#include "me_ssa_tab.h"

// allocate the data structure to store SSA information

namespace maple {

AnalysisResult *MeDoSSATab::Run(MeFunction *func, MeFuncResultMgr *m) {
  MPLTimer timer;
  timer.Start();
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============== SSA and AA preparation =============" << std::endl;
  }
  MemPool *mp = mempoolctrler.NewMemPool(PhaseName().c_str());
  // allocate ssaTab including its SSAPart to store SSA information for statements
  SSATab *ssaTab = mp->New<SSATab>(mp, func->versMemPool, &func->mirModule, func->bbVec.size());
  func->meSSATab = ssaTab;
#if DEBUG
  g_ssatab = ssaTab;
#endif

  // pass through the program statements
  for (BB *bb : func->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->stmtNodeList) {
      ssaTab->CreateSSAStmt(stmt, bb);  // this adds the SSANodes for exprs
    }
  }

  if (DEBUGFUNC(func)) {
    timer.Stop();
    LogInfo::MapleLogger() << "ssaTab consumes cumulatively " << timer.Elapsed() << "seconds " << std::endl;
  }

  return ssaTab;
}

}  // namespace maple
