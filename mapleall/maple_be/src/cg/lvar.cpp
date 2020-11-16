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

#include "lvar.h"
#include "cg_func.h"
#include "cg.h"
#include "insn.h"

namespace maplebe {

void OptLocalRef::Run() {
  DoOptLocalRef();
}

AnalysisResult *CgDoOptLocalRef::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (cgfunc->func->module->IsCModule() || (CGOptions::doLvarPathOpt == false) ||
      (g->optim_level < 2)) {
    return nullptr;
  }
  MemPool *refmp = mempoolctrler.NewMemPool("optlocalref");
  OptLocalRef *ref = cgfunc->NewOptLocalRef(cgfunc, refmp);
  CHECK_FATAL(ref, "OptLocalRef:Run failed");
  ref->Run();
  mempoolctrler.DeleteMemPool(refmp);
  return nullptr;
}

}  // namespace maplebe
