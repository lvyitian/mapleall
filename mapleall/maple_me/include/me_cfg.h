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

#ifndef MAPLE_ME_INCLUDE_ME_CFG_H
#define MAPLE_ME_INCLUDE_ME_CFG_H

#include "me_function.h"
#include "me_phase.h"

namespace maple {

class MirCFG : public AnalysisResult {
 public:
  MeFunction *func;
  MapleAllocator cfgAlloc;
  BB *first_bb;
  BB *last_bb;
  BB *commonEntryBB;
  BB *commonExitBB;
  uint32 nextBBId;
  MapleVector<BB *> bbVec;
  MapleUnorderedMap<LabelIdx, BB *> labelBBIdMap;
  MapleUnorderedMap<BB *, StmtNode *> bbTryNodeMap;  // maps isTry bb to its try stmt
  MapleUnorderedMap<BB *, BB *> endTryBB2TryBB; // maps endtry bb to its try bb
  MapleSet<LabelIdx> pattern_set_;
  bool hasDoWhile;

  explicit MirCFG(MeFunction *f, MemPool *mp) : AnalysisResult(mp),
    func(f),
    cfgAlloc(mp),
    first_bb(nullptr),
    last_bb(nullptr),
    commonEntryBB(nullptr),
    commonExitBB(nullptr),
    nextBBId(0),
    bbVec(cfgAlloc.Adapter()),
    labelBBIdMap(cfgAlloc.Adapter()),
    bbTryNodeMap(cfgAlloc.Adapter()),
    endTryBB2TryBB(cfgAlloc.Adapter()),
    pattern_set_(cfgAlloc.Adapter()),
    hasDoWhile(false) {}

  ~MirCFG() {}

  uint32 NumBBs(void) const { return nextBBId; }

  BB *NextBB(const BB *bb);
  BB *PrevBB(const BB *bb);
  void DeleteBasicBlock(const BB *bb);
  bool IfReplaceWithAssertnonnull(BB *bb);
  void BuildMirCFG();
  void ReplaceWithAssertnonnull();
  void ConvertPhis2IdentityAssigns(BB *mebb);
  void UnreachCodeAnalysis(bool updatePhi = false);
  void WontExitAnalysis();
  void Verify();
  void VerifyLabels();
  void Dump();
  void DumpToFile(const char *prefix, bool dumpinstrs = false);
  void AddAuxilaryBB();
};

class MeDoCfgBuild : public MeFuncPhase {
 public:
  explicit MeDoCfgBuild(MePhaseID id) : MeFuncPhase(id) {}

  ~MeDoCfgBuild() {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "cfgbuild";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_CFG_H
