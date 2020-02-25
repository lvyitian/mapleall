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

#include "cg_assert.h"
#include "store_load_opt.h"
#include "optimize_common.h"
#include "cg.h"
#include <algorithm>

namespace maplebe {

using namespace maple;

/* Set internal_flag1 true if bb has backedge through depth first search.
 */
void StoreLoadOpt::DFS(BB *startBB) {
  MapleVector<BB *>::iterator it;

  startBB->internal_flag3 = GRAY;
  dfs_bb.push_back(startBB);

  for (auto succBB : startBB->succs) {
    if (succBB->internal_flag3 == kWhite) {
      DFS(succBB);
    } else {
      it = find(dfs_bb.begin(), dfs_bb.end(), succBB);
      if (it != dfs_bb.end()) {
        succBB->internal_flag1 = true;
      }
    }
  }

  for (auto ehSuccBb : startBB->eh_succs) {
    if (ehSuccBb->internal_flag3 == kWhite) {
      DFS(ehSuccBb);
    } else {
      it = find(dfs_bb.begin(), dfs_bb.end(), ehSuccBb);
      if (it != dfs_bb.end()) {
        ehSuccBb->internal_flag1 = true;
      }
    }
  }

  dfs_bb.pop_back();
}

void StoreLoadOpt::FindBackEdge() {
  FOR_ALL_BB(bb, cgfunc) {
    bb->internal_flag1 = false;
    bb->internal_flag3 = kWhite;
  }

  FOR_ALL_BB(bb, cgfunc) {
    if (bb->internal_flag3 == kWhite) {
      DFS(bb);
    }
  }
}

AnalysisResult *CgDoStoreLoadOpt::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (SCHDDUMP) {
    DotGenerator::GenerateDot("storeloadopt", cgfunc, &cgfunc->mirModule, true);
  }

  ReachingDefinition *rd = nullptr;
  if (g->optim_level >= 2) {
    rd = static_cast<ReachingDefinition *>(m->GetAnalysisResult(CgFuncPhase_REACHDEF, cgfunc));
  }

  if (rd == nullptr || !cgfunc->GetRDStatus()) {
    m->InvalidAnalysisResult(CgFuncPhase_REACHDEF, cgfunc);
    return nullptr;
  }

  m->GetAnalysisResult(CgFuncPhase_LOOPNOEH, cgfunc);
  MemPool *slomp = mempoolctrler.NewMemPool("storeloadopt");
  StoreLoadOpt *slo = cgfunc->NewStoreLoadOpt(cgfunc, slomp, rd);
  CG_ASSERT(slo, "No enough memory space.");
  slo->run();
  m->InvalidAnalysisResult(CgFuncPhase_LOOPNOEH, cgfunc);
  mempoolctrler.DeleteMemPool(slomp);
  return nullptr;
}

}  // namespace maplebe
