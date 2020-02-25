/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan PSL v1.
 * You can use this software according to the terms and conditions of the Mulan PSL v1.
 * You may obtain a copy of Mulan PSL v1 at:
 *
 *     http://license.coscl.org.cn/MulanPSL
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v1 for more details.
 */

#ifndef MAPLE_PHASE_INCLUDE_PHASE_H
#define MAPLE_PHASE_INCLUDE_PHASE_H
#include <map>
#include <string>
#include <iostream>
#include "mempool.h"
#include "maple_string.h"
#include "mempool_allocator.h"
#include "option.h"

using PhaseID  = int;

namespace maple {
/* base class of analysisPhase's result */
class AnalysisResult {
 private:
  MemPool *memPool;

 public:
  explicit AnalysisResult(MemPool *mempool) {
    ASSERT(mempool, "mempool is null in AnalysisResult::AnalysisResult");
    memPool = mempool;
  }

  MemPool *GetMempool() const {
    return memPool;
  }

  virtual ~AnalysisResult() {}
};

class Phase {
 public:
  Phase() {}

  virtual std::string PhaseName() const {
    ASSERT(false, "The base Phase::PhaseName should not be called");
    return "";
  }

  virtual ~Phase(){};
};

template <typename UnitIR, typename PhaseIDT, typename PhaseT>
class AnalysisResultManager {
 private:
  MapleAllocator *allocator; /* allocator used in local field */
  /* map < pair<phaseId, ir*>, AnalysisResult_T> */
  typedef std::pair<PhaseIDT, UnitIR *> analysisResultKey;
  MapleMap<analysisResultKey, AnalysisResult *> analysisResults;
  MapleMap<PhaseIDT, PhaseT *> analysisPhases;

 public:
  explicit AnalysisResultManager(MapleAllocator *alloc)
    : analysisResults(std::less<analysisResultKey>(), alloc->Adapter()),
      analysisPhases(std::less<PhaseIDT>(), alloc->Adapter()) {
    allocator = alloc;
  }

  virtual ~AnalysisResultManager() {
    InvalidAllResults();
    /* global variable mirModule which use same mempool control is not delete yet */
  }

  /* analysis result use global mempool and allocator */
  AnalysisResult *GetAnalysisResult(PhaseIDT id, UnitIR *ir) {
    ASSERT(ir, "ir is null in AnalysisResultManager::GetAnalysisResult");
    std::pair<PhaseIDT, UnitIR *> key = std::make_pair(id, ir);
    if (analysisResults.find(key) != analysisResults.end()) {
      return analysisResults[key];
    }
    PhaseT *anaphase = GetAnalysisPhase(id);
    ASSERT(anaphase != nullptr, "anaphse is null in AnalysisResultManager::GetAnalysisResult");
    if (std::string(anaphase->PhaseName()) != Options::skipPhase) {
      AnalysisResult *result = anaphase->Run(ir, this);
      analysisResults[key] = result; /* add r to analysisResults */
      return result;
    } else {
      return nullptr;
    }
  }

  void AddResult(PhaseIDT id, UnitIR *ir, AnalysisResult *ar) {
    ASSERT(ar, "ar is null in AnalysisResultManager::AddResult");
    std::pair<PhaseIDT, UnitIR *> key = std::make_pair(id, ir);
    if (analysisResults.find(key) != analysisResults.end()) {
      InvalidAnalysisResult(id, ir);
    }
    analysisResults.insert(std::make_pair(key, ar));
  }

  void InvalidAnalysisResult(PhaseIDT id, UnitIR *ir) {
    std::pair<PhaseIDT, UnitIR *> key = std::make_pair(id, ir);
    auto it = analysisResults.find(key);
    if (it != analysisResults.end()) {
      AnalysisResult *r = analysisResults[key];
      mempoolctrler.DeleteMemPool(r->GetMempool());
      analysisResults.erase(it);
    }
  }

  void InvalidIRbaseAnalysisResult(UnitIR *ir) {
    PhaseIDT id;
    for (auto it = analysisPhases.begin(); it != analysisPhases.end(); it++) {
      id = it->first;
      InvalidAnalysisResult(id, ir);
    }
  }

  void InvalidAllResults() {
    for (auto it = analysisResults.begin(); it != analysisResults.end(); it++) {
      AnalysisResult *r = it->second;
      ASSERT(r, "r is null in AnalysisResultManager::InvalidAllResults");
      mempoolctrler.DeleteMemPool(r->GetMempool());
    }
    analysisResults.clear();
  }

  void AddAnalysisPhase(PhaseIDT id, PhaseT *p) {
    ASSERT(p, "p is null in AnalysisResultManager::AddAnalysisPhase");
    analysisPhases[id] = p;
  }

  PhaseT *GetAnalysisPhase(PhaseIDT id) {
    if (analysisPhases.find(id) != analysisPhases.end()) {
      return analysisPhases[id];
    } else {
      ASSERT(false, "Invalid analysis phase");
      return nullptr;
    }
  }

  void ClearAnalysisPhase() {
    analysisPhases.clear();
  }
};

}  // namespace maple
#endif  // MAPLE_PHASE_IMCLUDE_PHASE_H
