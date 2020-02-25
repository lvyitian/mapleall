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

#include "lvar.h"
#include "cg_func.h"
#include "cg.h"
#include "insn.h"

namespace maplebe {

void OptLocalRef::Run() {
  DoOptLocalRef();
}

AnalysisResult *CgDoOptLocalRef::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (CGOptions::doLvarPathOpt == false) {
    return nullptr;
  }
  if (g->optim_level < 2) {
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
