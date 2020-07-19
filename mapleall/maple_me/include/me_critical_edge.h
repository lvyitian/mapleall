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

#ifndef MAPLE_ME_INCLUDE_ME_CRITICAL_EDGE_H
#define MAPLE_ME_INCLUDE_ME_CRITICAL_EDGE_H

#include "me_phase.h"
#include "bb.h"

namespace maple {

/* Split critical edge */
class MeDoSplitCEdge : public MeFuncPhase {
 public:
  MeDoSplitCEdge(MePhaseID id) : MeFuncPhase(id) {}

  ~MeDoSplitCEdge() {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;

  std::string PhaseName() const override {
    return "splitcriticaledge";
  }

 private:
  void BreakCriticalEdge(MeFunction *func, BB *pred, BB *succ);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_CRITICAL_EDGE_H
