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

#include "me_irmap.h"
#include "irmap_build.h"
#include "dominance.h"
#include "mir_builder.h"

namespace maple {

void MeIRMap::Dump() {
  // back up mempool and use a new mempool every time
  // we dump IRMap, restore the mempool afterwards
  MIRFunction *mirfunction = mirFunc->mirFunc;
  MemPool *backup = mirfunction->codeMemPool;
  mirfunction->SetMemPool(mempoolctrler.NewMemPool("IR Dump"));
  LogInfo::MapleLogger() << "===================Me IR dump==================\n";
  for (BB *bb : mirFunc->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    bb->DumpHeader(mirModule);
    LogInfo::MapleLogger() << "frequency : " << bb->frequency << "\n";
    bb->DumpMePhiList(this);
    int i = 0;
    for (auto mestmt : bb->meStmtList) {
      if (dumpStmtNum) {
        LogInfo::MapleLogger() << "(" << i++ << ") ";
      }
      StmtNode *stmt = mestmt->EmitStmt(ssaTab);
      if (stmt != nullptr) {
        stmt->Dump(mirModule, 0);
        mestmt->Dump(this);
      }
    }
  }
  mempoolctrler.DeleteMemPool(mirfunction->codeMemPool);
  mirfunction->SetMemPool(backup);
}

}  // namespace maple
