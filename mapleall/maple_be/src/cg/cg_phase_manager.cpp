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

#include <iostream>
#include <vector>
#include <string>

#include "cg_phase_manager.h"
#include "cg_option.h"
#include "ebo.h"
#include "cfg_optimizer.h"
#include "ico.h"
#include "mpl_timer.h"

using namespace maple;
using namespace std;
#define PHASEBEFOREMIT "fixshortbranch"

namespace maplebe {
#define JAVALANG (module.IsJavaModule())
#define CLANG (module.IsCModule())
void CgFuncPhaseManager::RunFuncPhase(CGFunc *func, FuncPhase *phase) {
  {
    // 1. check options.enable(phase.id())
    // 2. options.tracebeforePhase(phase.id()) dumpIR before
    if (!CGOptions::quiet) {
      LogInfo::MapleLogger() << "---Run Phase [ " << phase->PhaseName() << " ]---\n";
    }
  }

  // 3. run: skip mplcg phase except "emit" if no cfg in CGFunc
  AnalysisResult *r = nullptr;
  if ((func->NumBBs() > 0) || (phase->GetPhaseId() == CgFuncPhaes_EMIT)) {
    r = phase->Run(func, &arFuncManager);
  }

  if (r != nullptr) {
    /*if phase is an analysis Phase, add result to arm */
    arFuncManager.AddResult(phase->GetPhaseId(), func, r);
  } else {
    /* it's an optimization phase */
  }
}

void CgFuncPhaseManager::RegisterFuncPhases() {
  /*register all Funcphases defined in cg_phases.def */
#define FUNCTPHASE(id, cgphase)                                                                          \
  do {                                                                                                   \
    RegisterPhase(id, (new (GetMemAllocator()->GetMemPool()->Malloc(sizeof(cgphase(id)))) cgphase(id))); \
  } while (0);

#define FUNCAPHASE(id, cgphase)                                                                          \
  do {                                                                                                   \
    RegisterPhase(id, (new (GetMemAllocator()->GetMemPool()->Malloc(sizeof(cgphase(id)))) cgphase(id))); \
    arFuncManager.AddAnalysisPhase(id, (static_cast<FuncPhase *>(GetPhase(id))));                      \
  } while (0);

#include "cg_phases.def"
#undef FUNCTPHASE
#undef FUNCAPHASE
}

#define ADDPHASE(phaseName)                                                 \
  if (CGOptions::skipPhases.find(phaseName) == CGOptions::skipPhases.end()) \
    phases.push_back(phaseName);
void CgFuncPhaseManager::AddPhases(std::vector<std::string> &phases) {
#if !TARGARK
  if (phases.size() == 0) {
    if (cgphase_type == kCgPhaseMainopt) {
      /*default phase sequence */
      ADDPHASE("layoutstackframe");
      ADDPHASE("createstartendlabel");
      if (CGOptions::genEH && module.srcLang != kSrcLangC) {
        ADDPHASE("buildehfunc");
      }
      ADDPHASE("handlefunction");
      if (JAVALANG) {
        ADDPHASE("optlocalref");
      }
      ADDPHASE("movevregargs");
      ADDPHASE("moveregargs");
      if (CGOptions::doEbo) {
        ADDPHASE("ebo");
      }
      if (CGOptions::doPrePeephole) {
        ADDPHASE("prepeephole");
      }
      if (CGOptions::doIco) {
        ADDPHASE("ico");
      }
      if (CGOptions::doCfgo) {
        ADDPHASE("cfgo");
      }

      if (JAVALANG && CGOptions::doStoreLoadOpt) {
        ADDPHASE("storeloadopt");
      }

      if (CGOptions::doGlobalOpt) {
        ADDPHASE("globalopt");
      }

      if (CGOptions::doPrePeephole) {
        ADDPHASE("prepeephole1");
      }

      if (CGOptions::doEbo) {
        ADDPHASE("ebo1");
      }

      if (CGOptions::doPreSchedule) {
        ADDPHASE("merge");
        ADDPHASE("prescheduling");
        ADDPHASE("split");
      }

      ADDPHASE("regalloc");
      ADDPHASE("generateproepilog");
      ADDPHASE("offsetadjustforfplr");
      ADDPHASE("dbgfixcallframeoffsets");

      if (CGOptions::doPeephole) {
        ADDPHASE("peephole0");
      }
      if (CGOptions::doEbo) {
        ADDPHASE("postebo");
      }

      if (CGOptions::doCfgo) {
        ADDPHASE("postcfgo");
      }
      if (CGOptions::doPeephole) {
        ADDPHASE("peephole");
      }
      if (CGOptions::genCfi) {
        ADDPHASE("gencfi");
      }
      if (JAVALANG && CGOptions::insertYieldpoint) {
        ADDPHASE("yieldpoint");
      }
      if (CGOptions::doSchedule) {
        ADDPHASE("merge");
        ADDPHASE("scheduling");
        ADDPHASE("split");
      }
      ADDPHASE("fixshortbranch");
      ADDPHASE("emit");
    }
  }
#endif
  for (unsigned i = 0; i < phases.size(); i++) {
    AddPhase(phases[i].c_str());
  }
  ASSERT(phases.size() == GetphaseSeq()->size(), "invalid phase name");
}

// match sub string of function name
bool FuncFilter(const string &filter, const std::string &name) {
  if (filter.compare("*") == 0 || name.find(filter.c_str()) != string::npos) {
    return true;
  }
  return false;
}

void CgFuncPhaseManager::Emit(CGFunc *func) {
  std::string phaseName = PHASEBEFOREMIT;
  bool dumpFunc = FuncFilter(CGOptions::dumpFunc, func->GetName());
  int phaseIndex = 0;
  bool timePhases = CGOptions::timePhases;

  PhaseID id = CgFuncPhaes_EMIT;
  FuncPhase *p = static_cast<FuncPhase *>(GetPhase(id));
  CHECK_FATAL(p != nullptr, "p is null in CgFuncPhaseManager::Run");

  p->SetPreviousPhaseName(phaseName); /*prev phase name is for filename used in emission after phase*/
  phaseName = p->PhaseName();         // new phase name

  MPLTimer timer;
  if (timePhases) {
    timer.Start();
  }
  RunFuncPhase(func, p);
  if (timePhases) {
    timer.Stop();
    phase_timers[phaseIndex] += timer.ElapsedMicroseconds();
  }
#if !TARGARK
  bool dumpPhases = CGOptions::DumpPhase(phaseName.c_str());
  if (((CGOptions::dumpAfter && dumpPhases) || dumpPhases) && dumpFunc) {
    LogInfo::MapleLogger() << "******** CG IR After " << phaseName << ": *********" << endl;
    func->DumpCGIR();
  }
#endif
}

void CgFuncPhaseManager::Run(CGFunc *func) {
  if (!CGOptions::quiet) {
    LogInfo::MapleLogger() << ">>>>>>>>>>>>>>>>>>>>>>>>>>>>> Optimizing Function  < " << func->GetName() << " >---\n";
  }
  std::string phaseName;
  /* each function level phase */
  bool dumpFunc = FuncFilter(CGOptions::dumpFunc, func->GetName());
  int phaseIndex = 0;
  bool timePhases = CGOptions::timePhases;
  bool skipFromFlag = false;
  bool skipAfterFlag = false;
  MPLTimer timer;
  for (auto it = PhaseSeqBegin(); it != PhaseSeqEnd(); it++, ++phaseIndex) {
    PhaseID id = GetPhaseId(it);
    if (id == CgFuncPhaes_EMIT) {
      continue;
    }
    FuncPhase *p = static_cast<FuncPhase *>(GetPhase(id));
    CHECK_FATAL(p != nullptr, "p is null in CgFuncPhaseManager::Run");

    if (!skipFromFlag && CGOptions::skipFrom.compare(p->PhaseName()) == 0) {
      skipFromFlag = true;
    }
    if (skipFromFlag) {
      while (p->CanSkip() && ++it != PhaseSeqEnd()) {
        id = GetPhaseId(it);
        p = static_cast<FuncPhase *>(GetPhase(id));
        CHECK_FATAL(p, "null ptr check ");
      }
    }

    p->SetPreviousPhaseName(phaseName); /*prev phase name is for filename used in emission after phase*/
    phaseName = p->PhaseName();         // new phase name
    if (CGOptions::useRange) {
      if (!CGOptions::inRange &&
          ((phaseName == "ebo") || (phaseName == "ebo1") || (phaseName == "postebo") ||
           (phaseName == "ico") || (phaseName == "cfgo") ||
           (phaseName == "peephole0") || (phaseName == "peephole"))) {
        continue;
      }
    }
#if !TARGARK
    bool dumpPhase = IS_STR_IN_SET(CGOptions::dumpPhases, phaseName);
    if (CGOptions::dumpBefore && dumpFunc && dumpPhase) {
      LogInfo::MapleLogger() << "******** CG IR Before " << phaseName << ": *********" << endl;
      func->DumpCGIR();
    }
#endif
    if (timePhases) {
      timer.Start();
    }
    RunFuncPhase(func, p);
    if (timePhases) {
      timer.Stop();
      phase_timers[phaseIndex] += timer.ElapsedMicroseconds();
    }
#if !TARGARK
    bool dumpPhases = CGOptions::DumpPhase(phaseName.c_str());
    if (((CGOptions::dumpAfter && dumpPhase) || dumpPhases) && dumpFunc) {
      LogInfo::MapleLogger() << "******** CG IR After " << phaseName << ": *********" << endl;
      func->DumpCGIR();
    }
#endif
    if (!skipAfterFlag && CGOptions::skipAfter.compare(p->PhaseName()) == 0) {
      skipAfterFlag = true;
    }
    if (skipAfterFlag) {
      while (++it != PhaseSeqEnd()) {
        id = GetPhaseId(it);
        p = static_cast<FuncPhase *>(GetPhase(id));
        CHECK_FATAL(p, "null ptr check ");
        if (!p->CanSkip()) {
          break;
        }
      }
      --it;  // restore iterator
    }
  }
}

}  // namespace maplebe
