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


#ifndef MAPLE_IPA_INCLUDE_INTERLEAVED_MANAGER_H
#define MAPLE_IPA_INCLUDE_INTERLEAVED_MANAGER_H

#include "../../maple_ipa/include/module_phase_manager.h"
namespace maple {

class InterleavedManager {
 public:
  InterleavedManager(MemPool *mp, MIRModule *mirm, std::string input, bool timer)
      : allocator(mp),
        mirModule(*mirm),
        phaseManagers(allocator.Adapter()),
        supportPhaseManagers(allocator.Adapter()),
        meInput(input),
        timePasses(timer) {}

  InterleavedManager(MemPool *mp, MIRModule *mirm)
      : allocator(mp),
        mirModule(*mirm),
        phaseManagers(allocator.Adapter()),
        supportPhaseManagers(allocator.Adapter()) {}

  ~InterleavedManager() {
    if (timePasses) {
      DumpTimers();
    }
  }

  void DumpTimers();

  MapleAllocator *GetMemAllocator() {
    return &allocator;
  }

  MemPool *GetMempool() {
    return allocator.GetMemPool();
  }

  void AddPhases(const std::vector<std::string> &phases, bool isModulePhase, bool timePhases = false, bool genmpl = false);
  void Run();

  PhaseManager *AccessPhaseManager(int i) const {
    return phaseManagers.at(i);
  }

  const PhaseManager *GetSupportPhaseManager(const std::string &phase);

 private:
  MapleAllocator allocator;
  MIRModule &mirModule;
  MapleVector<PhaseManager *> phaseManagers;
  MapleVector<PhaseManager*> supportPhaseManagers;  // Used to check whether a phase is supported and by which manager
  std::string meInput;
  bool timePasses = false;

  void InitSupportPhaseManagers();
};

}  // namespace maple
#endif
