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

#ifndef MAPLE_ME_INCLUDE_ME_SSA_H
#define MAPLE_ME_INCLUDE_ME_SSA_H
#include "me_function.h"
#include "me_cfg.h"
#include "me_ident_loops.h"
#include "ssa.h"

namespace maple {

class Dominance;

class MeSSA : public maple::SSA {
 public:
  MeFunction *func;

 private:
  bool VerifySSAOpnd(BaseNode *node);

 public:
  explicit MeSSA(MeFunction *f, SSATab *stab, Dominance *dom, MemPool *mp) :
      SSA(mp, stab, dom, f->theCFG->bbVec), func(f) {}

  ~MeSSA() {}

  void InsertPhiNode();
  bool VerifySSA();
  void InsertIdentifyAssignments(IdentifyLoops *identloops);
  std::string PhaseName() const {
    return "ssa";
  }
};

class MeDoSSA : public MeFuncPhase {
 public:
  MeDoSSA(MePhaseID id) : MeFuncPhase(id) {}

  ~MeDoSSA() {}

  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "ssa";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_SSA_H
