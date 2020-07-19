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

#ifndef MAPLE_ME_INCLUDE_ME_CONDBASED_OPT_H
#define MAPLE_ME_INCLUDE_ME_CONDBASED_OPT_H
#include "me_function.h"
#include "me_phase.h"
#include "me_irmap.h"

namespace maple {

class CondBased {
 public:
  MeFunction *func;
  Dominance *dominance;

  CondBased(MeFunction *f, Dominance *dom) : func(f), dominance(dom) {}

  bool NullValueFromTestCond(VarMeExpr *, BB *, bool);
  bool NullValueFromOneTestCond(VarMeExpr *, BB *, BB *, bool);
  bool IsNotNullValue(VarMeExpr *, UnaryMeStmt *, BB *);
  bool PointerWasDereferencedBefore(VarMeExpr *, UnaryMeStmt *, BB *);
  bool PointerWasDereferencedRightAfter(VarMeExpr *, UnaryMeStmt *);
  bool IsIreadWithTheBase(VarMeExpr *, MeExpr *);
  bool StmtHasDereferencedBase(MeStmt *, VarMeExpr *);
};

class MeDoCondBasedRC : public MeFuncPhase {
 public:
  MeDoCondBasedRC(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;

  std::string PhaseName() const override {
    return "condbasedrc";
  }
};

class MeDoCondBasedNPC : public MeFuncPhase {
 public:
  MeDoCondBasedNPC(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;

  std::string PhaseName() const override {
    return "condbasednpc";
  }
};

}  // namespace maple

#endif  // MAPLE_ME_INCLUDE_ME_CONDBASED_OPT_H
