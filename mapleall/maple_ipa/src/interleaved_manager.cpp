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


#include "../include/interleaved_manager.h"

#include <string>
#include <vector>
#include <iomanip>
#include "module_phase.h"
#include "mir_function.h"
#include "mir_module.h"
#include "me_function.h"
#include "me_option.h"
#include "me_ssa_devirtual.h"
#include "mempool.h"
#include "phase_manager.h"

void InterleavedManager::AddPhases(const std::vector<std::string> &phases, bool isModulePhase, bool timePhases, bool genmpl) {
  ModuleResultMgr *mrm = nullptr;
  if (phaseManagers.size() != 0) {
    // ModuleResult such class hierarchy need to be carried on
    ModulePhaseManager *mpm = dynamic_cast<ModulePhaseManager *>(phaseManagers[phaseManagers.size() - 1]);
    MeFuncPhaseManager *mepm = dynamic_cast<MeFuncPhaseManager *>(phaseManagers[phaseManagers.size() - 1]);

    if (mpm) {
      mrm = mpm->GetModResultMgr();
    } else if (mepm) {
      mrm = mepm->GetModResultMgr();
    }
  }

  if (isModulePhase) {
    ModulePhaseManager *mpm = GetMempool()->New<ModulePhaseManager>(GetMempool(), &mirModule, mrm);
    mpm->RegisterModulePhases();
    mpm->AddModulePhases(phases);
    if (timePhases) {
      mpm->SetTimePhases(true);
    }
    phaseManagers.push_back(mpm);
  } else {  // MeFuncPhase
    MeFuncPhaseManager *fpm = GetMempool()->New<MeFuncPhaseManager>(GetMempool(), &mirModule, mrm);
    fpm->RegisterFuncPhases();
    fpm->mePhaseType = kMePhaseMainopt;
    if (genmpl) {
      fpm->genMeMpl = true;
    }
    if (timePhases) {
      fpm->timePhases = true;
    }
    fpm->AddPhasesNoDefault(phases);
    phaseManagers.push_back(fpm);
  }
}

void InterleavedManager::Run() {
  for (PhaseManager *const &pm : phaseManagers) {
    if (dynamic_cast<MeFuncPhaseManager *>(pm)) {
      MeFuncPhaseManager *fpm = static_cast<MeFuncPhaseManager *>(pm);
      unsigned long rangeNum = 0;
      MapleVector<MIRFunction *> *compList;
      if (mirModule.compilationList.size() != 0) {
        CHECK_FATAL((mirModule.compilationList.size() == mirModule.functionList.size() ||
                mirModule.compilationList.size() == mirModule.functionList.size() - mirModule.optimizedFuncs.size()),
               "should be equal");
        compList = &mirModule.compilationList;
      } else {
        compList = &mirModule.functionList;
      }
      bool isFirstFunc = true;
      for (MIRFunction *func : *compList) {
        if (MeOption::useRange && (rangeNum < MeOption::range[0] || rangeNum > MeOption::range[1])) {
          rangeNum++;
          continue;
        }
        if (func->body == nullptr) {
          rangeNum++;
          continue;
        }
        if (fpm->GetphaseSeq()->empty()) {
          continue;
        }
        mirModule.SetCurFunction(func);
        // lower, create BB and build cfg
        fpm->Run(func, rangeNum, meInput);
        rangeNum++;
        if (fpm->genMeMpl) {
          func->Emit("comb.me.mpl", isFirstFunc);
          isFirstFunc = false;
        }
      }
    } else {
      pm->Run();
    }
  }
}

void InterleavedManager::DumpTimers() {
  std::ios_base::fmtflags f(LogInfo::MapleLogger().flags());
  std::vector<std::pair<std::string, time_t>> timeVec;
  long total = 0;
  LogInfo::MapleLogger() << "=================== TIMEPHASES =================\n";
  for (auto manager : phaseManagers) {
    long temp = manager->DumpTimers();
    total += temp;
    timeVec.push_back(std::pair<std::string, time_t>(manager->mgr_name, temp));
    LogInfo::MapleLogger() << "================================================\n";
  }
  LogInfo::MapleLogger() << "==================== SUMMARY ===================\n";
  for (auto lapse : timeVec) {
    CHECK_FATAL(total != 0, "calculation check");
    LogInfo::MapleLogger() << std::left << std::setw(25) << lapse.first << std::setw(10) << std::right << std::fixed
              << std::setprecision(2) << (100.0 * lapse.second / total) << "%" << std::setw(10)
              << (lapse.second / 1000) << "ms" << std::endl;
  }
  LogInfo::MapleLogger() << "================================================\n";
  LogInfo::MapleLogger().flags(f);
}

void InterleavedManager::InitSupportPhaseManagers() {
  ASSERT(supportPhaseManagers.empty(), "Phase managers already initialized");

  auto *mpm = GetMempool()->New<ModulePhaseManager>(GetMempool(), &mirModule, nullptr);
  mpm->RegisterModulePhases();
  supportPhaseManagers.push_back(mpm);

  ModuleResultMgr *mrm = mpm->GetModResultMgr();

  auto *fpm = GetMempool()->New<MeFuncPhaseManager>(GetMempool(), &mirModule, mrm);
  fpm->RegisterFuncPhases();
  supportPhaseManagers.push_back(fpm);
}

const PhaseManager *InterleavedManager::GetSupportPhaseManager(const std::string &phase) {
  if (supportPhaseManagers.empty()) {
    InitSupportPhaseManagers();
  }

  for (auto pm : supportPhaseManagers) {
    if (pm->ExistPhase(phase)) {
      return pm;
    }
  }

  return nullptr;
}
