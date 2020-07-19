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

#include "module_phase_manager.h"
#include "class_hierarchy.h"
#include "class_init.h"
#include "callgraph.h"
#include "inline.h"
#include "greedyinline.h"
#include "option.h"
#if MIR_JAVA
#include "native_stub_func.h"
#include "vtable_analysis.h"
#include "reflection_analysis.h"
#include "vtable_impl.h"
#include "decouple.h"
#include "java_lowering.h"
#include "analyzector.h"
#include "barrierinsertion.h"
#include "deferralbarrier.h"
#include "java_eh_lower.h"
#include "native_stub_func.h"
#include "coderelayout.h"
#include "muid_replacement.h"
#include "simplify.h"
#include "gen_check_cast.h"
#include "scalarreplacement.h"
#endif
#include "bin_mpl_export.h"
#include "clone.h"
#include "mpl_timer.h"

namespace maple {

// Manage the phases of middle
AnalysisResult *DoKlassHierarchy::Run(MIRModule *module, ModuleResultMgr *m) {
  MemPool *mp = mempoolctrler.NewMemPool("classhierarchy mempool");
  KlassHierarchy *kh = mp->New<KlassHierarchy>(module, mp);
  KlassHierarchy::traceFlag = TRACE_PHASE;
  kh->BuildHierarchy();
#if MIR_JAVA
  if (!Options::skipVirtualMethod)
#endif
    kh->CountVirtualMethods();
  if (KlassHierarchy::traceFlag) {
    kh->Dump();
  }
  m->AddResult(GetPhaseId(), module, kh);
  return kh;
}

void ModulePhaseManager::RegisterModulePhases() {
#define MODAPHASE(id, modphase)                                                      \
  do {                                                                               \
    MemPool *mp = GetMemPool();                                                      \
    ModulePhase *phase = new (mp->Malloc(sizeof(modphase(id)))) modphase(id);        \
    RegisterPhase(id, phase);                                                        \
    arModuleMgr->AddAnalysisPhase(id, (static_cast<ModulePhase *>(GetPhase(id)))); \
  } while (0);

#define MODTPHASE(id, modphase)                                               \
  do {                                                                        \
    MemPool *mp = GetMemPool();                                               \
    ModulePhase *phase = new (mp->Malloc(sizeof(modphase(id)))) modphase(id); \
    RegisterPhase(id, phase);                                                 \
  } while (0);

#include "module_phases.def"
#undef MODAPHASE
#undef MODTPHASE
}

void ModulePhaseManager::AddModulePhases(const std::vector<std::string> &phases) {
  for (std::string const &phase : phases) {
    AddPhase(phase.c_str());
  }
}

void ModulePhaseManager::RunModulePhases() const {}

void ModulePhaseManager::Run() {
  int phaseIndex = 0;
  for (auto it = PhaseSeqBegin(); it != PhaseSeqEnd(); it++, ++phaseIndex) {
    PhaseID id = GetPhaseId(it);
    ModulePhase *p = static_cast<ModulePhase *>(GetPhase(id));
    MIR_ASSERT(p && "Invalid ModulePhase pointer");

    // if we need to skip after certain pass
    if (Options::skipFrom.compare(p->PhaseName().c_str()) == 0) {
      break;
    }
    if (!Options::quiet) {
      LogInfo::MapleLogger() << "---Run Module Phase [ " << p->PhaseName() << " ]---\n";
    }
    MPLTimer timer;
    if (timePhases) {
      timer.Start();
    }
    p->Run(&mir_module, arModuleMgr);
    if (timePhases) {
      timer.Stop();
      phase_timers[phaseIndex] += timer.ElapsedMicroseconds();
    }
    if (Options::skipAfter.compare(p->PhaseName().c_str()) == 0) {
      break;
    }
  }
}

void ModulePhaseManager::Emit(const char *passName) {
  // Form output file name.
  std::string outFileName;
  std::string::size_type lastdot = mir_module.fileName.find_last_of(kDotStr);
  if (lastdot == std::string::npos) {
    outFileName = mir_module.fileName.append(kDotStr);
  } else {
    outFileName = mir_module.fileName.substr(0, lastdot).append(kDotStr);
  }
  outFileName = outFileName.append(passName);
  outFileName = outFileName.append(kDotMplStr);
  mir_module.DumpToFile(outFileName);
}

}  // namespace maple
