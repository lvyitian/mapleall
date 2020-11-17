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


#ifndef MAPLE_ME_INCLUDE_ME_DSE_H
#define MAPLE_ME_INCLUDE_ME_DSE_H
#include <iostream>
#include "bb.h"
#include "me_phase.h"
#include "me_option.h"
#include "dominance.h"
#include "me_function.h"
#include "me_cfg.h"
#include "dse.h"

namespace maple {

class MeDSE : private DSE {
 public:
  explicit MeDSE(MeFunction *f, Dominance *dom, MemPool *mp)
    : DSE(&f->mirModule, &f->meSSATab->stmtsSSAPart, dom, mp), func(f), cfg(f->theCFG), cfg_updated(false) {}

  void DseInit();
  void DseProcess();
  void UpdateStmt(BB *bb);
  void Update();
  void Dse();
  const bool UpdatedCfg() const {
    return cfg_updated;
  }

  void VerifyPhi();
  std::string PhaseName() const {
    return "dse";
  }

 private:
  MeFunction *func;
  MirCFG *cfg;
  bool cfg_updated;

  BB *GetBB(BBId id) {
    return cfg->bbVec[id.idx];
  }
};

class MeDoDSE : public MeFuncPhase {
 public:
  MeDoDSE(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "dse";
  }
};
}  // namespace maple
#endif
