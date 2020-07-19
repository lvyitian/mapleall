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

#include "me_dominance.h"
#include "me_option.h"
#include <iostream>

// This phase analyses the CFG of the given MeFunction, generates the dominator tree,
// and the dominance frontiers of each basic block using Keith Cooper's algorithm.
// For some backward data-flow problems, such as LiveOut,
// the reverse CFG(The CFG with its edges reversed) is always useful,
// so we also generates the above two structures on the reverse CFG.

namespace maple {

void MeDominance::Run() {
  GenPostOrderID();
  ComputeDominance();
  ComputeDomFrontiers();
  ComputeDomChildren();
  ComputeIterDomFrontiers();
  uint32 num = 0;
  ComputeDtPreorder(func->commonEntryBB, num);
  dtPreOrder.resize(num);
  ComputeDtDfn();

  PdomGenPostOrderID();
  ComputePostDominance();
  ComputePdomFrontiers();
  ComputePdomChildren();
  ComputeIterPdomFrontiers();
  num = 0;
  ComputePdtPreorder(func->commonExitBB, num);
  pdtPreOrder.resize(num);
  ComputePdtDfn();
}

AnalysisResult *MeDoDominance::Run(MeFunction *func, MeFuncResultMgr *m) {
  MemPool *mp = mempoolctrler.NewMemPool(PhaseName().c_str());
  MemPool *tmppool = mempoolctrler.NewMemPool("dominance temps");

  MeDominance *dom =
    mp->New<MeDominance>(mp, tmppool, (MapleVector<BB *> *)&func->bbVec, func->commonEntryBB, func->commonExitBB, func);

  dom->Run();

  if (DEBUGFUNC(func)) {
    dom->DumpDoms();
    dom->DumpPdoms();
  }

  mempoolctrler.DeleteMemPool(tmppool);
  return dom;
}

}  // namespace maple
