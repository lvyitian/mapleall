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

#ifndef MAPLE_ME_INCLUDE_LFO_IV_CANON_H
#define MAPLE_ME_INCLUDE_LFO_IV_CANON_H

#include "lfo_function.h"
#include "me_ident_loops.h"
#include "me_irmap.h"
#include "me_phase.h"

namespace maple {

// describe characteristics of one IV
class IVDesc {
 public:
  OriginalSt *ost;
  int32 stepValue = 0;
  MeExpr *initExpr = nullptr;

 public:
  explicit IVDesc(OriginalSt *o) : ost(o) {}
};

// this is for processing a single loop
class IVCanon {
 public:
  MemPool *mp;
  MapleAllocator alloc;
  MeFunction *func;
  SSATab *ssatab;
  LoopDesc *aloop;
  LfoWhileInfo *whileInfo;
  MapleVector<IVDesc *> ivvec;

 public:
  explicit IVCanon(MemPool *m, MeFunction *f, LoopDesc *ldesc, LfoWhileInfo *winfo) :
        mp(m), alloc(m), func(f), ssatab(f->meSSATab), aloop(ldesc), whileInfo(winfo),
        ivvec(alloc.Adapter()) {}
  bool ResolveExprValue(MeExpr *x, ScalarMeExpr *philhs);
  int32 ComputeIncrAmt(MeExpr *x, ScalarMeExpr *philhs, int32 *appearances);
  void CharacterizeIV(ScalarMeExpr *initversion, ScalarMeExpr *loopbackversion, ScalarMeExpr *philhs);
  void PerformIVCanon();
  std::string PhaseName() const { return "ivcanon"; }
};

class DoLfoIVCanon : public MeFuncPhase {
 public:
  DoLfoIVCanon(MePhaseID id) : MeFuncPhase(id) {}

  ~DoLfoIVCanon() {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;

  std::string PhaseName() const override { return "ivcanon"; }

 private:
  void IVCanonLoop(LoopDesc *aloop, LfoWhileInfo *whileInfo);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_LFO_IV_CANON_H