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


#ifndef MAPLE_ME_INCLUDE_ME_HDSE_H
#define MAPLE_ME_INCLUDE_ME_HDSE_H
#include "bb.h"
#include "me_cfg.h"
#include <iostream>
#include "me_phase.h"
#include "me_option.h"
#include "me_dominance.h"
#include "hdse.h"

namespace maple {

class MeHDSE : public HDSE {
 public:
  explicit MeHDSE(MeFunction *f, Dominance *pdom, IRMap *map)
    : HDSE(&f->mirModule, f->meSSATab, pdom, map, mempoolctrler.NewMemPool(PhaseName().c_str()), f->theCFG->bbVec),
      func(f),
      verstUseCounts(dse_allocator.Adapter()),
      backSubsCands(dse_allocator.Adapter()) {}

  ~MeHDSE() {
    if (dse_allocator.mp) {
      mempoolctrler.DeleteMemPool(dse_allocator.mp);
    }
  }

  void DseInit();
  void DseInitFull();
  void BackwardSubstitution();
 private:
  void DetermineUseCounts(MeExpr *x);
  void CheckBackSubsCandidacy(DassignMeStmt *dass);
  void UpdateStmt(BB *);
  std::string PhaseName() const {
    return "hdse";
  }

  MeFunction *func;
  MapleVector<uint32> verstUseCounts; // index is vstIdx
  MapleForwardList<DassignMeStmt *> backSubsCands; // backward substitution candidates
};

class MeDohDSE : public MeFuncPhase {
 public:
  MeDohDSE(MePhaseID id) : MeFuncPhase(id) {}

  void MakeEmptyTrysUnreachable(MeFunction *func);
  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "hdse";
  }
};
}  // namespace maple
#endif
