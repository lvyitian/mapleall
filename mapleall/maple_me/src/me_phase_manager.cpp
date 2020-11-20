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
#include "me_dominance.h"
#include "me_phase.h"
#include "me_cfg.h"
#include "me_critical_edge.h"
#include "me_loop_canon.h"
#include "me_alias_class.h"
#include "me_bdc_opt.h"
#include "me_ident_loops.h"
#include "me_ssa.h"
#include "me_dse.h"
#include "me_hdse.h"
#include "me_cfg_opt.h"
#include "me_prop.h"
#include "me_irmap_build.h"
#include "me_hti.h"
#include "me_ssa_epre.h"
#include "me_rename_to_preg.h"
#include "me_ssa_lpre.h"
#include "me_store_pre.h"
#include "me_phase_manager.h"
#include "me_bb_layout.h"
#include "me_emit.h"
#include "me_const_prop.h"
#include "me_delegate_rc.h"
#include "me_may_to_dassign.h"
#include "me_condbased_opt.h"
#include "me_analyze_rc.h"
#include "me_rc_lowering.h"
#include "me_predict.h"
#include "ipa_side_effect.h"
#include "me_ssa_devirtual.h"
#include "gen_check_cast.h"
#if MIR_JAVA
#include "sync_select.h"
#endif  // MIR_JAVA
#include "me_ssa_tab.h"
#include "me_stmt_pre.h"
#include "me_fsaa.h"
#include "me_sym_rename.h"
#include "mpl_timer.h"
#include "constant_fold.h"
#include "mir_lower.h"

using namespace std;

#define JAVALANG (module.IsJavaModule())

namespace maple {
void MeFuncPhaseManager::RunFuncPhase(MeFunction *func, MeFuncPhase *phase) {
  {
    // 1. check options.enable(phase.id())
    // 2. options.tracebeforePhase(phase.id()) dumpIR before
    if (!MeOption::quiet) {
      LogInfo::MapleLogger() << "---Run Phase [ " << phase->PhaseName() << " ]---\n";
    }

    // LogInfo::MapleLogger() << "\n-----CFG before Phase [ " << phase->PhaseName() << " ]-----" << std::endl;
    // func->DumpFunction();
    // 3. tracetime(phase.id())
  }
#ifdef DEBUG_TIMER
  MPLTimer timer;
  timer.Start();
#endif

  // 4. run: skip mplme phase except "emit" if no cfg in MeFunction
  AnalysisResult *r = nullptr;
  MePhaseID phaseid = phase->GetPhaseId();
  if ((phaseid == MeFuncPhase_CFGBUILD) || (func->theCFG->NumBBs() > 0) || (phaseid == MeFuncPhase_EMIT)) {
    r = phase->Run(func, &arFuncManager, modResMgr);
  }
#ifdef DEBUG_TIMER
  timer.Stop();
  cerr << " Phase " << phase->PhaseName() << " function " << func->mirFunc->GetName() << " takes "
       << timer.ElapsedMicroseconds() / 1000000.0 << " seconds\b" << endl;
  if (phase->PhaseName() == "hdse") {
    cerr << endl;
  }
#endif

  if (r != nullptr) {
    /* if phase is an analysis Phase, add result to arm */
    arFuncManager.AddResult(phase->GetPhaseId(), func, r);
  } else {
    /* it's an optimization phase */
    // 5. verify ir if needed
    // 6. options.traceafterPhase(phase.id()) cfg->dumpIR after
    // LogInfo::MapleLogger() << "\n------------CFG after Phase [ " << phase->PhaseName() << " ]-------------------" << std::endl;
    // func->DumpFunction();
  }
}

void MeFuncPhaseManager::RegisterFuncPhases() {
  /* register all Funcphases defined in me_phases.def */
#define FUNCTPHASE(id, mephase)                                                                          \
  do {                                                                                                   \
    RegisterPhase(id, (new (GetMemAllocator()->GetMemPool()->Malloc(sizeof(mephase(id)))) mephase(id))); \
  } while (0);

#define FUNCAPHASE(id, mephase)                                                                          \
  do {                                                                                                   \
    RegisterPhase(id, (new (GetMemAllocator()->GetMemPool()->Malloc(sizeof(mephase(id)))) mephase(id))); \
    arFuncManager.AddAnalysisPhase(id, (static_cast<MeFuncPhase *>(GetPhase(id))));                    \
  } while (0);

#include "me_phases.def"
#undef FUNCTPHASE
#undef FUNCAPHASE
}

void MeFuncPhaseManager::AddPhasesNoDefault(const std::vector<std::string> &phases) {
  for (unsigned i = 0; i < phases.size(); i++) {
    PhaseManager::AddPhase(phases[i].c_str());
  }
  ASSERT(phases.size() == GetphaseSeq()->size(), "invalid phase name");
}

void MeFuncPhaseManager::AddPhases(const std::unordered_set<std::string> &skipPhases) {
  auto addPhase = [&](const std::string &phase) {
    std::unordered_set<std::string>::const_iterator it = skipPhases.find(phase);
    if (it == skipPhases.end()) {
      PhaseManager::AddPhase(phase.c_str());
    }
  };

  bool o2 = MeOption::optLevel >= 2;
  bool o3 = MeOption::optLevel == 3;
  /* default phase sequence */
  if (o3) {
    addPhase("ssatab");
    addPhase("aliasclass");
    addPhase("ssa");
    addPhase("dse");
    addPhase("irmapbuild");
    //addPhase("loopivcan");
    addPhase("hprop");
    addPhase("hdse");
    addPhase("lnoemit");
    addPhase("loopinfo");
    if (JAVALANG) {
      addPhase("lfobdce");
    } else {
      addPhase("autosimd");
    }
  }
  addPhase("cfgbuild");
  addPhase("ssatab");
  addPhase("aliasclass");
  addPhase("ssa");
  addPhase("dse");
  addPhase("irmapbuild");
  if (JAVALANG) {
    addPhase("bdcopt");
    addPhase("syncselect");
    // addPhase("ssadevirt");
    // addPhase("ea");
  }
  addPhase("hprop");
  addPhase("symrename");
  addPhase("hdse");
  if (o2 && JAVALANG) {
    addPhase("cfgopt");
  }
  if (JAVALANG) {
    addPhase("may2dassign");
    addPhase("condbasednpc");
  }
  if (o2) {
    addPhase("epre");
    if (JAVALANG) {
      addPhase("stmtpre");
    }
  }
  if (JAVALANG && !MeOption::noRC) {
    addPhase("analyzerc");
    if (MeOption::rcLowering) {
      addPhase("rclowering");
    }
  }
  addPhase("rename2preg");
  if (o2) {
    addPhase("lpre");
    // addPhase("storepre");
  }
  addPhase("emit");
}

// match sub string of function name
bool MeFuncPhaseManager::FuncFilter(const string &filter, const std::string &name) {
  if (filter.compare("*") == 0 || name.find(filter.c_str()) != string::npos) {
    return true;
  }
  return false;
}

void MeFuncPhaseManager::Run(MIRFunction *mirFunc, uint64 rangenum, const string &meinput) {
  if (!MeOption::quiet)
    LogInfo::MapleLogger() << ">>>>>>>>>>>>>>>>>>>>>>>>>>>>> Optimizing Function  < " << mirFunc->GetName()
         << " id=" << mirFunc->puIdxOrigin << " >---\n";
  if (mirFunc->HasSetjmp()) {
    if (!MeOption::quiet)
      LogInfo::MapleLogger() << "Function  < " << mirFunc->GetName() << "not optimized because it has setjmp\n";
    return;
  }
  MeFunction func(&module, mirFunc, meinput, false, MeOption::optLevel == 3);
#if DEBUG
  g_mirmodule = &module;
  g_func = &func;
#endif
  // call constant folding
  maple::ConstantFold cf(&module);
  cf.Simplify(mirFunc->body);

  if (!MeOption::quiet)
    LogInfo::MapleLogger() << "---Preparing Function  < " << module.CurFunction()->GetName() << " > [" << rangenum << "] ---\n";
  if (MeOption::optLevel < 3) { // lower for mainopt
    MIRLower mirlowerer(module, mirFunc);
    mirlowerer.SetLowerME();
    mirlowerer.SetLowerExpandArray();
    mirlowerer.LowerFunc(mirFunc);
  }
  std::string phaseName;
  /* each function level phase */
  bool dumpFunc = FuncFilter(MeOption::dumpFunc, func.GetName());
  int phaseIndex = 0;
  for (auto it = PhaseSeqBegin(); it != PhaseSeqEnd(); it++, ++phaseIndex) {
    PhaseID id = GetPhaseId(it);
    MeFuncPhase *p = static_cast<MeFuncPhase *>(GetPhase(id));
    CHECK_FATAL(p, "null ptr check ");

    if (MeOption::skipFrom.compare(p->PhaseName()) == 0) {
      // fast-forward to emit pass, which is last pass
      while (++it != PhaseSeqEnd())
        ;
      --it;  // restore iterator
      id = GetPhaseId(it);
      p = static_cast<MeFuncPhase *>(GetPhase(id));
      CHECK_FATAL(p, "null ptr check ");
    }

    p->SetPreviousPhaseName(phaseName); /* prev phase name is for filename used in emission after phase */
    phaseName = p->PhaseName();         // new phase name
    bool dumpPhase = MeOption::DumpPhase(phaseName);

    if (MeOption::dumpBefore && dumpFunc && dumpPhase) {
      LogInfo::MapleLogger() << ">>>>> Dump before " << phaseName << " <<<<<\n";
      func.DumpFunction();
    }

    MPLTimer timer;
    timer.Start();
    RunFuncPhase(&func, p);
    if (timePhases) {
      timer.Stop();
      phase_timers[phaseIndex] += timer.ElapsedMicroseconds();
    }
    if (MeOption::dumpAfter && dumpFunc && dumpPhase) {
      LogInfo::MapleLogger() << ">>>>> Dump after " << phaseName << " <<<<<\n";
      func.DumpFunction();
    }
    if (MeOption::skipAfter.compare(phaseName) == 0) {
      // fast-forward to emit pass, which is last pass
      while (++it != PhaseSeqEnd())
        ;
      --it;
      --it;  // restore iterator to emit
    }
  }
  GetAnalysisResultManager()->InvalidAllResults();
}

}  // namespace maple
