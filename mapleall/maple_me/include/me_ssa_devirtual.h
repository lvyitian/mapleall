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

#ifndef MAPLE_ME_INCLUDE_ME_ME_SSA_DEVIRTUAL_H
#define MAPLE_ME_INCLUDE_ME_ME_SSA_DEVIRTUAL_H
#include "ssa_devirtual.h"
#include "clone.h"

namespace maple {
class MeSSADevirtual : public SSADevirtual {
 public:
  MeSSADevirtual(MemPool *mp, MIRModule *mod, MeFunction *f, IRMap *irMap, KlassHierarchy *kh, Dominance *dom,
                 Clone *clone)
    : SSADevirtual(mp, mod, irMap, kh, dom, f->theCFG->bbVec.size(), clone), func(f) {}

  ~MeSSADevirtual() {}

 protected:
  BB *GetBB(BBId id) override {
    return func->theCFG->bbVec[id.idx];
  }

  MIRFunction *GetMIRFunction() override {
    return func->mirFunc;
  }

 private:
  MeFunction *func;
};

class MeDoSSADevirtual : public MeFuncPhase {
 public:
  explicit MeDoSSADevirtual(MePhaseID id) : MeFuncPhase(id) {}

  virtual ~MeDoSSADevirtual() {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *frm, ModuleResultMgr *mrm) override;
  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m) override {
    ASSERT(false, "should not be here");
    return nullptr;
  }

  virtual std::string PhaseName() const override {
    return "ssadevirt";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_ME_SSA_DEVIRTUAL_H
