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

#ifndef LIVEANALYSIS_H
#define LIVEANALYSIS_H

#include "cg_func.h"
#include "cg_phase.h"

namespace maplebe {

#define FOR_SUBBB_INSNS(INSN, BLOCK) \
    for (Insn * (INSN) = FIRST_INSN(BLOCK); (INSN) && (INSN) != (LAST_INSN(BLOCK))->next; (INSN) = (INSN)->next)

#define FOR_SUBBB_INSNS_REV(INSN, BLOCK) \
    for (Insn * (INSN) = LAST_INSN(BLOCK); (INSN) && (INSN) != (FIRST_INSN(BLOCK))->prev; (INSN) = (INSN)->prev)

class LiveAnalysis : public AnalysisResult {
 public:
  void AnalysisLive();
  void AnalysisFinish() {
    if (CGOptions::lsraSimdMode != 2) {
      FinalizeLiveAnalysis();
    }
  }

  void Dump();

 protected:
  int iteration;
  CGFunc *cgfunc_;
  MemPool *memPool;

 private:
  bool doEhLiveAnalysis;

 public:
  MapleVector<std::vector<BB *>> *subBB;  // For each BB, list of subBBs

  explicit LiveAnalysis(CGFunc *func, MemPool *mp)
    : AnalysisResult(mp),
      iteration(0),
      cgfunc_(func),
      memPool(mp),
      subBB(nullptr) {}

  void InitAnalysis();
  void InitBB(BB *bb);
  bool ComputeBBLiveness(BB *bb);
  void AnalysisFinalizeCleanupBB();
  void CopySubBBInfoToBB(BB *bb);
  void DumpBBLivenessInfo(BB *bb);
  void PrintBB(BB *bb, bool isSubBB = false);
  void PrintLivenessInfo();

  virtual void GetBBDefUse(BB *bb) = 0;
  virtual bool CleanupBBIgnoreReg(uint32 reg) = 0;
  virtual void FinalizeLiveAnalysis() = 0;
  virtual void InitEhDefine(BB *bb) = 0;

  void ClearDoEhLiveAnalysis() { doEhLiveAnalysis = false; }
  void SetDoEhLiveAnalysis() { doEhLiveAnalysis = true; }
  bool GetDoEhLiveAnalysis() { return doEhLiveAnalysis; }
};

CGFUNCPHASE(CgDoLiveAnalysis, "liveanalysis")
CGFUNCPHASE(CgDoLiveAnalysisEh, "liveanalysiseh")
}  // namespace maplebe

#endif /* LIVEANALYSIS_H */
