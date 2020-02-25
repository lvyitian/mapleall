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

#ifndef MAPLE_CG_INCLUDE_CGPHASEMANAGER_H
#define MAPLE_CG_INCLUDE_CGPHASEMANAGER_H
#include <vector>
#include <string>
#include "mempool.h"
#include "mempool_allocator.h"
#include "phase_manager.h"
#include "mir_module.h"
#include "cg_phase.h"
#include "cg_option.h"
namespace maplebe {

typedef enum { kCgPhaseInvalid, kCgPhaseMainopt, kCgPhaseLno } CgPhaseType;

class CGFunc;
/* driver of Cg */
class CgFuncPhaseManager : public PhaseManager {
 private:
  /* analysis phase result manager */
  CgFuncResultMgr arFuncManager;
#if !TARGARK
  MIRModule &module;
#endif

 public:
  CgPhaseType cgphase_type;

 public:
  CgFuncPhaseManager(MemPool *mp, MIRModule *mod, ModuleResultMgr *mrm = nullptr)
    : PhaseManager(mp, "cg manager"),
      arFuncManager(GetMemAllocator()),
#if !TARGARK
      module(*mod),
#endif
      cgphase_type(kCgPhaseInvalid) {}

  ~CgFuncPhaseManager() {
    arFuncManager.InvalidAllResults();
    if (CGOptions::timePhases) {
      DumpTimers();
    }
  }

  void RunFuncPhase(CGFunc *func, FuncPhase *p);
  void RegisterFuncPhases();
  void AddPhases(std::vector<std::string> &phases);

  void SetCgPhase(CgPhaseType cgphase) {
    cgphase_type = cgphase;
  }

  void Emit(CGFunc *func);
  void Run(CGFunc *func);
  void Run() override {}

  CgFuncResultMgr *GetAnalysisResultManager(void) {
    return &arFuncManager;
  }
};
}  // namespace maplebe
#endif
