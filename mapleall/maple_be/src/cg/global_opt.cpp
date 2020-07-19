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

#include "global_opt.h"
#include "cg.h"
#include "store_load_opt.h"
#include "cg_assert.h"

/*this phase do some optimization using use-def chain and def-use chain.
   each function in run() is a optimization. mainly include 2 parts:
   1. find the number of valid bits for register by finding the definition insn of register,
    and then using the valid bits to delete redundant insns.
   2. copy Propagate:
    a. forward copy propagate
        this optimization aim to optimize following:
      mov x100, x200;
      BBs:
      ...
      mop ..., x100  /// multiple site that use x100
      =>
      mov x200, x200
      BBs:
      ...
      mop ..., x200 // multiple site that use x100
     b. backward copy propagate
        this optimization aim to optimize following:
      mop x200, ...  // Define insn of x200
      ...
      mop ..., x200  // use site of x200
      mov x100, x200;
        =>
      mop x100, ...  // Define insn of x200
      ...
      mop ..., x100  // use site of x200
      mov x100, x100;

   NOTE: after insn is modified, UD-chain and DU-chain should be maintained by self. currently several common
     interface has been implemented in RD, but they must be used reasonably. specific instructions for use
     can be found at the begining of corresponding function.
 */

namespace maplebe {

using namespace maple;

AnalysisResult *CgDoGlobalOpt::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  ReachingDefinition *rd = nullptr;
  if (g->optim_level >= 2) {
    rd = static_cast<ReachingDefinition *>(m->GetAnalysisResult(CgFuncPhase_REACHDEF, cgfunc));
  }
  if (rd == nullptr || !cgfunc->GetRDStatus()) {
    m->InvalidAnalysisResult(CgFuncPhase_REACHDEF, cgfunc);
    return nullptr;
  }
  MemPool *gomp = mempoolctrler.NewMemPool("globalopt");
  GlobalOpt *go = cgfunc->NewGlobalOpt(cgfunc, gomp);
  CHECK_FATAL(go, "go should not be nullptr");
  go->Run();

  // the live range info may changed, so invalid the info.
  rd->ClearDefUseInfo();
  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  m->InvalidAnalysisResult(CgFuncPhase_REACHDEF, cgfunc);
  mempoolctrler.DeleteMemPool(gomp);
  return nullptr;
}

}  // namespace maplebe
