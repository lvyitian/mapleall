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

#ifndef MAPLE_IPA_INCLUDE_MODULE_PHASE_MANAGER_H
#define MAPLE_IPA_INCLUDE_MODULE_PHASE_MANAGER_H
#include "module_phase.h"
#include "me_phase_manager.h"

static constexpr char kDotStr[] = ".";
static constexpr char kDotMplStr[] = ".mpl";

namespace maple {

class ModulePhaseManager : public PhaseManager {
 public:
  ModulePhaseManager(MemPool *mp, MIRModule *mod, ModuleResultMgr *mrm = nullptr)
    : PhaseManager(mp, "modulephase"), mir_module(*mod) {
    if (mrm) {
      arModuleMgr = mrm;
    } else {
      arModuleMgr = mp->New<ModuleResultMgr>(GetMemAllocator());
    }
  }

  ~ModulePhaseManager() {}

  /* register all module phases defined in module_phases.def */
  void RegisterModulePhases();
  // Add module phases which are going to be run
  void AddModulePhases(const std::vector<std::string> &phases);
  void RunModulePhases() const;
  ModuleResultMgr *GetModResultMgr() {
    return arModuleMgr;
  }

  void SetTimePhases(bool val) {
    timePhases = val;
  }

  void Run() override;
  void Emit(const char *passName);

 private:
  bool timePhases = false;
  MIRModule &mir_module;
  ModuleResultMgr *arModuleMgr; /* module level analysis result */
};
}  // namespace maple
#endif  // MAPLE_IPA_INCLUDE_MODULE_PHASE_MANAGER_H
