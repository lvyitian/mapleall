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

#ifndef MAPLE_ME_INCLUDE_ME_ALIAS_CLASS_H
#define MAPLE_ME_INCLUDE_ME_ALIAS_CLASS_H
#include "alias_class.h"
#include "me_phase.h"
#include "me_function.h"
#include "me_cfg.h"

namespace maple {

class MeAliasClass : public AliasClass {
 public:
  MeAliasClass(MemPool *mp, MIRModule *mod, SSATab *ssatb, MeFunction *f, bool lessaliasatthrow,
               bool finalfieldhasalias, bool ignoreipa, bool debug, bool setCalleeHasSideEffect, KlassHierarchy *kh)
      : AliasClass(mp, mod, ssatb, lessaliasatthrow, finalfieldhasalias, ignoreipa, debug, setCalleeHasSideEffect, kh),
        func(f) {}

  void PerformAliasClass();
 private:
  MeFunction *func;

  BB *GetBB(BBId id) {
    if (func->theCFG->bbVec.size() < id.idx) {
      return nullptr;
    }
    return func->theCFG->bbVec[id.idx];
  }

  bool InConstructorFunc() {
    return func->mirFunc->IsConstructor();
  }

  std::string PhaseName() const {
    return "aliasclass";
  }
};

class MeDoAliasClass : public MeFuncPhase {
 public:
  explicit MeDoAliasClass(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m, ModuleResultMgr *mrm) override;
  std::string PhaseName() const override {
    return "aliasclass";
  }
};
}  // namespace maple
#endif  // end MAPLE_ME_INCLUDE_ME_ALIAS_CLASS_H
