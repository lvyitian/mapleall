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

#ifndef MAPLE_ME_INCLUDE_ME_PHASE_MANAGER_H
#define MAPLE_ME_INCLUDE_ME_PHASE_MANAGER_H
#include <vector>
#include <string>
#include "mempool.h"
#include "mempool_allocator.h"
#include "phase_manager.h"
#include "mir_module.h"
#include "me_phase.h"

namespace maple {
typedef enum { kMePhaseInvalid, kMePhaseMainopt, kMePhaseLno } MePhaseType;

/* driver of Me */
class MeFuncPhaseManager : public PhaseManager {
 public:
  /* analysis phase result manager */
  MeFuncResultMgr arFuncManager;
  MIRModule &module;
  ModuleResultMgr *modResMgr;
  MePhaseType mePhaseType;
  bool genMeMpl;
  bool timePhases;

  MeFuncPhaseManager(MemPool *mp, MIRModule *mod, ModuleResultMgr *mrm = nullptr)
    : PhaseManager(mp, "mephase"),
      arFuncManager(GetMemAllocator()),
      module(*mod),
      modResMgr(mrm),
      mePhaseType(kMePhaseInvalid),
      genMeMpl(false),
      timePhases(false) {}

  ~MeFuncPhaseManager() {
    arFuncManager.InvalidAllResults();
  }

  void RunFuncPhase(MeFunction *, MeFuncPhase *);
  void RegisterFuncPhases();
  void AddPhases(const std::unordered_set<std::string> &skipPhases);
  void AddPhasesNoDefault(const std::vector<std::string> &phases);

  void SetMePhase(MePhaseType mephase) {
    mePhaseType = mephase;
  }

  void SetModResMgr(ModuleResultMgr *mrm) {
    modResMgr = mrm;
  }

  void Run(MIRFunction *, uint64, const std::string &);
  void Run() override {}

  MeFuncResultMgr *GetAnalysisResultManager(void) {
    return &arFuncManager;
  }

  ModuleResultMgr *GetModResultMgr() {
    return modResMgr;
  }

  bool FuncFilter(const std::string &, const std::string &);

 private:
   void RunMainOpt(MIRFunction *, uint64, const std::string &);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_PHASE_MANAGER_H
