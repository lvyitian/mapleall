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

#ifndef MAPLEPHASE_INCLUDE_PHASEMANAGER_H
#define MAPLEPHASE_INCLUDE_PHASEMANAGER_H
#include "phase.h"
#include <iomanip>

using namespace maple;

class PhaseManager {
 protected:
  MapleAllocator allocator;
  MapleMap<PhaseID, Phase *> registered_phases;
  MapleVector<PhaseID> phase_sequences;
  MapleVector<long> phase_timers;

 public:
  explicit PhaseManager(MemPool *mp, const char *name)
    : allocator(mp),
      registered_phases(std::less<PhaseID>(), allocator.Adapter()),
      phase_sequences(allocator.Adapter()),
      phase_timers(allocator.Adapter()),
      mgr_name(name) {}

  virtual ~PhaseManager() {}

  std::string mgr_name;
  void AddPhase(const char *pname) {
    for (auto it = RegPhaseBegin(); it != RegPhaseEnd(); it++) {
      if (GetPhaseName(it) == pname) {
        phase_sequences.push_back(GetPhaseId(it));
        phase_timers.push_back(0);
        return;
      }
    }
    fprintf(stderr, "%s is not a valid phase name\n", pname);
    abort();
  }

  void RegisterPhase(PhaseID id, Phase *p) {
    registered_phases[id] = p;
  }

  Phase *GetPhase(PhaseID id) {
    if (registered_phases.find(id) != registered_phases.end()) {
      return registered_phases[id];
    }
    MIR_WARNING("not a valid phase");
    return nullptr;
  }

  virtual void Run() {
    MIR_ASSERT(false && "should not run here");
  }

  MapleAllocator *GetMemAllocator() {
    return &allocator;
  }

  MemPool *GetMemPool() {
    return allocator.GetMemPool();
  }

  MapleVector<PhaseID> *GetphaseSeq() {
    return &phase_sequences;
  }

  /* iterator for register_phases */
  typedef MapleMap<PhaseID, Phase *>::iterator iterator;
  iterator RegPhaseBegin() {
    return registered_phases.begin();
  }

  iterator RegPhaseEnd() {
    return registered_phases.end();
  }

  const std::string GetPhaseName(iterator it) {
    return (*it).second->PhaseName();
  }

  PhaseID GetPhaseId(iterator it) const {
    return (*it).first;
  }

  /* iterator for phaseSeq */
  typedef MapleVector<PhaseID>::iterator phaseSeq_iterator;
  phaseSeq_iterator PhaseSeqBegin() {
    return phase_sequences.begin();
  }

  phaseSeq_iterator PhaseSeqEnd() {
    return phase_sequences.end();
  }

  PhaseID GetPhaseId(phaseSeq_iterator it) const {
    return (*it);
  }

  bool ExistPhase(const std::string &name) {
    for (auto it = RegPhaseBegin(); it != RegPhaseEnd(); ++it) {
      if (GetPhaseName(it) == name) {
        return true;
      }
    }
    return false;
  }

  long DumpTimers() {
    long total = 0;
    for (size_t i = 0; i < phase_timers.size(); ++i) {
      total += phase_timers[i];
    }
    for (size_t i = 0; i < phase_timers.size(); ++i) {
      ASSERT(total != 0, "calculation check");
      std::cout << std::left << std::setw(25) << registered_phases[phase_sequences[i]]->PhaseName() << std::setw(10)
                << std::right << std::fixed << std::setprecision(2) << (100.0 * phase_timers[i] / total) << "%"
                << std::setw(10) << std::setprecision(0) << (phase_timers[i] / 1000.0) << "ms" << std::endl;
    }
    return total;
  }
};

#endif
