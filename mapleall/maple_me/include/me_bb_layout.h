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

#ifndef MAPLE_ME_INCLUDE_ME_BB_LAYOUT_H
#define MAPLE_ME_INCLUDE_ME_BB_LAYOUT_H
#include "me_function.h"
#include "me_cfg.h"
#include "me_phase.h"

namespace maple {

class BBLayout : public AnalysisResult {
 private:
  MeFunction *func;
  MirCFG *cfg;
  MapleAllocator layoutAlloc;
  MapleVector<BB *> layoutBBs;  // gives the determined layout order
  BBId curBBId;               // to index into func->bbVec to return the next BB
  bool bbCreated;               // new create bb will change mefunction::bbVec and
                                 // related analysis result
 public:
  MapleVector<bool> laidOut;  // indexed by bbid to tell if has been laid out
  bool tryOutstanding;       // true if a try laid out but not its endtry

  explicit BBLayout(MemPool *mp, MeFunction *f)
    : AnalysisResult(mp),
      func(f),
      cfg(f->theCFG),
      layoutAlloc(mp),
      layoutBBs(layoutAlloc.Adapter()),
      curBBId(0),
      bbCreated(false),
      laidOut(f->theCFG->bbVec.size(), false, layoutAlloc.Adapter()),
      tryOutstanding(false) {
    laidOut[func->theCFG->commonEntryBB->id.idx] = true;
    laidOut[func->theCFG->commonExitBB->id.idx] = true;
  }

  BB *NextBB() {
    // return the next BB following strictly program input order
    curBBId.idx++;
    while (curBBId.idx < cfg->bbVec.size()) {
      BB *nextbb = cfg->bbVec[curBBId.idx];
      if (nextbb != nullptr && !laidOut[nextbb->id.idx]) {
        return nextbb;
      }
      curBBId.idx++;
    }
    return nullptr;
  }

  void OptimizeBranchTarget(BB *bb);
  bool BBEmptyAndFallthru(BB *bb);
  bool BBContainsOnlyGoto(BB *bb);
  bool BBContainsOnlyCondGoto(BB *bb);
  bool HasSameBranchCond(BB *bb1, BB *bb2);
  bool BBCanBeMoved(BB *frombb, const BB *toafterBb);
  void AddBB(BB *bb);
  BB *GetFallThruBBSkippingEmpty(BB *bb);
  void ResolveUnconditionalFallThru(BB *bb, BB *nextbb);
  void ChangeToFallthruFromGoto(BB *bb);
  const MapleVector<BB *> *GetBBs() const {
    return &layoutBBs;
  }

  const bool NewBBInLayout() const {
    return bbCreated;
  }

  void SetNewBBInLayout() {
    bbCreated = true;
  }

  std::string PhaseName() const {
    return "bblayout";
  }
};

class MeDoBBLayout : public MeFuncPhase {
 public:
  MeDoBBLayout(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "bblayout";
  }
};
}  // namespace maple
#endif
