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

#ifndef MAPLE_ME_INCLUDE_ME_IDENT_LOOP_H
#define MAPLE_ME_INCLUDE_ME_IDENT_LOOP_H
#include "me_function.h"
#include "me_cfg.h"
#include "bb.h"
#include "me_phase.h"
#include "dominance.h"

namespace maple {

class IdentifyLoops;

// describes a specific loop, including the loop head, tail and sets of bb.
class LoopDesc {
 public:
  BB *head;
  BB *tail;
  MapleSet<BBId> loop_bbs;
  LoopDesc *parent;  // points to its closest nesting loop
  uint32 nestdepth;  // the nesting depth

  LoopDesc(MapleAllocator *alloc, BB *headbb, BB *tailbb)
    : head(headbb), tail(tailbb), loop_bbs(alloc->Adapter()), parent(nullptr), nestdepth(0) {}

  bool Has(const BB *bb) {
    return loop_bbs.find(bb->id) != loop_bbs.end();
  }
};

// IdentifyLoop records all the loops in a MeFunction.
class IdentifyLoops : public AnalysisResult {
 public:
  MemPool *meloop_mp;
  MapleAllocator meloop_alloc;
  MeFunction *func;
  Dominance *dominance;
  MapleVector<LoopDesc *> meloops;
  MapleVector<LoopDesc *> bbloopparent;  // gives closest nesting loop for each bb

  explicit IdentifyLoops(MemPool *mp, MeFunction *mf, Dominance *dm)
    : AnalysisResult(mp),
      meloop_mp(mp),
      meloop_alloc(mp),
      func(mf),
      dominance(dm),
      meloops(meloop_alloc.Adapter()),
      bbloopparent(func->theCFG->bbVec.size(), nullptr, meloop_alloc.Adapter()) {}

  LoopDesc *CreateLoopDesc(BB *hd, BB *tail);
  void SetLoopParent4BB(const BB *bb, LoopDesc *aloop);
  void ProcessBB(BB *bb);
  void Dump();
};

class MeDoMeLoop : public MeFuncPhase {
 public:
  explicit MeDoMeLoop(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "identloops";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_IDENT_LOOP_H
