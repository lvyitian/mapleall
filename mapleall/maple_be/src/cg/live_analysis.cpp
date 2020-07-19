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

#include "cg_option.h"
#include "cg_func.h"
#include "live_analysis.h"
#include <iostream>

/* There are two live analysis here.
   1) Take BB as is, do not take into account for EH BB where instructions
      in the middle of a BB might except and jump to exception handling code.
   2) Factor in EH, an instruction that can except resides in a BB with
      exception handler can behave like a conditional branch.
 */
/*
   LiveAnalysis for EH is similar to LiveAnalysis, except it takes into
   account of potential exceptions within BB which can jump to the EH BB.

   Example: bb2 can throw an exception and potentially can jump to eh-bb2
            before defintion of x in bb2.  Therefore eh-bb2 will see the
            live-out of x from bb1.
            With the previous LiveAnalysis, defintion of x in bb1 is dead
            as there is no use since x is redefined in bb2. Therefore
            bb1->liveout_regno has no {x}.
      bb1
         ...
         x =
         ...
      bb2
         ...
         x =   ----> eh-bb2
         ...
      bb3                    = x
         ...
           = x

   LiveAnalysis:                     LiveAnalysisEh
     bb1->liveout_regno   { }        bb1->liveout_regno   {x}
     bb2->livein_regno    { }        bb2->livein_regno    {x}
     bb2->liveout_regno   {x}        bb2->liveout_regno   {x}
     bb3->livein_regno    {x}        bb3->livein_regno    {x}
     eh-bb2->livein_regno {x}        eh-bb2->livein_regno {x}

   The algorithm uses the same basic support as LiveAnalysis, but if a
   BB can throw exception and has a target EH-BB, then the BB is chopped
   into smaller sub-BB with each sub-BB terminates right before the
   potential excepting instruction.
   In the example above, BB2 will be chopped into two sub-BBs, with the
   first sub-BB does not contain the definition of x, and the second
   sub-BB has the defintion of x as its first instruction.  All of the
   sub-BBs have eh-bb2 as a eh_succs.
 */

/* this phase build two sets: liveout_regno and livein_regno of each BB.
   this algorithm mainly include 3 parts:
   1. find and mark cleanup BB;
   2. initialize and get def[]/use[] of each BB;
   3. build live_in and live_out based on this algorithm
     Out[B] = U In[S] //S means B's successor;
     In[B] = use[B] U (Out[B]-def[B]);
   4. deal with cleanup BB.
 */
namespace maplebe {
using namespace std;

#define LIVEANALYZEDUMP CGDEBUGFUNC(cgfunc)

void LiveAnalysis::DumpBBLivenessInfo(BB *bb) {
    set<regno_t> myset;
    LogInfo::MapleLogger() << "    DEF: ";
    for (auto li : bb->def_regno) {
      myset.insert(li);
    }
    for (auto li : myset) {
      LogInfo::MapleLogger() << li << " ";
    }
    myset.clear();
    LogInfo::MapleLogger() << "\n    USE: ";
    for (auto li : bb->use_regno) {
      myset.insert(li);
    }
    for (auto li : myset) {
      LogInfo::MapleLogger() << li << " ";
    }
    myset.clear();
    LogInfo::MapleLogger() << "\n    Live IN: ";
    for (auto li : bb->livein_regno) {
      myset.insert(li);
    }
    for (auto li : myset) {
      LogInfo::MapleLogger() << li << " ";
    }
    myset.clear();
    LogInfo::MapleLogger() << "\n    Live OUT: ";
    for (auto li : bb->liveout_regno) {
      myset.insert(li);
    }
    for (auto li : myset) {
      LogInfo::MapleLogger() << li << " ";
    }
    LogInfo::MapleLogger() << "\n";
}

void LiveAnalysis::PrintBB(BB *bb, bool isSubBB) {
  if (isSubBB) {
    LogInfo::MapleLogger() << "subBB-";
  } else {
    LogInfo::MapleLogger() << "BB-";
  }
  LogInfo::MapleLogger() << bb->id << " succ:";
  for (auto succ: bb->succs) {
    LogInfo::MapleLogger() << " " << succ->id ;
  }
  LogInfo::MapleLogger() << " ehsucc:";
  for (auto ehsucc: bb->eh_succs) {
    LogInfo::MapleLogger() << " " << ehsucc->id ;
  }
  LogInfo::MapleLogger() << endl;
  FOR_SUBBB_INSNS(insn, bb) {
    insn->dump();
  }
}

void LiveAnalysis::PrintLivenessInfo() {
  LogInfo::MapleLogger() << "=============Liveness Info For All BBs==============\n";
  FOR_ALL_BB(bb, cgfunc_) {
    if (((*subBB)[bb->id]).size() > 0) {
      PrintBB(bb, false);
      DumpBBLivenessInfo(bb);
      for (auto sbb: (*subBB)[bb->id]) {
        PrintBB(sbb, true);
        DumpBBLivenessInfo(sbb);
      }
    } else {
      PrintBB(bb, false);
      DumpBBLivenessInfo(bb);
    }
  }
  LogInfo::MapleLogger() << "===========================\n";
}

void LiveAnalysis::InitAnalysis() {
  FOR_ALL_BB(bb, cgfunc_) {
    if (!bb->eh_preds.empty()) {
      InitEhDefine(bb);
    }
    InitBB(bb);
    GetBBDefUse(bb);
    if (!bb->eh_preds.empty()) {
      bb->RemoveInsn(bb->firstinsn->next);
      bb->RemoveInsn(bb->firstinsn);
    }
  }
}

void LiveAnalysis::AnalysisFinalizeCleanupBB() {
  BB *cleanupBB = cgfunc_->cleanupEntrybb;
  if (cleanupBB) {
    for (auto reg : cleanupBB->livein_regno) {
      if (CleanupBBIgnoreReg(reg)) {
        continue;
      }

      /* After regreturn patch, a param vreg may used in cleanup bb.
         So this param vreg will live on the hole function since everywhere in function body may occur exceptions.
       */
      FOR_ALL_BB(bb, cgfunc_) {
        if (!bb->is_cleanup) {
          // If bb is not a cleanup bb, then insert reg to both livein and liveout.
          if (bb != cgfunc_->firstbb && find(bb->def_regno.begin(), bb->def_regno.end(), reg) == bb->def_regno.end()) {
            bb->livein_regno.insert(reg);
          }
          bb->liveout_regno.insert(reg);
        }
      }
    }
  }
}

bool LiveAnalysis::ComputeBBLiveness(BB *bb) {
  bool changed = false;
  uint32 oldliveoutsize = bb->liveout_regno.size();
  uint32 oldliveinsize = bb->livein_regno.size();
  MapleList<BB *>::iterator it = bb->succs.begin();
  while (it != bb->succs.end()) {
    if ((*it)->livein_change) {
      for (auto lin : (*it)->livein_regno) {
        bb->liveout_regno.insert(lin);
      }
    }
    it++;
  }
  // including exception handling successor
  it = bb->eh_succs.begin();
  while (it != bb->eh_succs.end()) {
    if ((*it)->livein_change) {
      for (auto lin : (*it)->livein_regno) {
        bb->liveout_regno.insert(lin);
      }
    }
    it++;
  }

  if (bb->liveout_regno.size() > oldliveoutsize || !bb->insertuse) {
    if (!bb->insertuse) {
      for (auto luse : bb->use_regno) {
        bb->livein_regno.insert(luse);
      }
      bb->insertuse = true;
    }

    for (auto lout : bb->liveout_regno) {
      bb->livein_regno.insert(lout);
    }
    for (auto ldef : bb->def_regno) {
      bb->livein_regno.erase(ldef);
    }
  }

  if (bb->livein_regno.size() > oldliveinsize) {
    bb->livein_change = true;
    changed = true;

  } else {
    bb->livein_change = false;
  }
  return changed;
}

/* Main function for building livein_regno and liveout_regno.
   Using a loop to implement algorithm until the value of livein_rego unchanged.
 */
void LiveAnalysis::AnalysisLive() {
  InitAnalysis();
  iteration = 0;
  bool hasSubBB = false;
  bool haschange;
  do {
    iteration++;
    haschange = false;
    FOR_ALL_BB_REV(bb, cgfunc_) {
      if (GetDoEhLiveAnalysis() && ((*subBB)[bb->id]).size() > 0) {
        hasSubBB = true;
        for (auto sbb = (*subBB)[bb->id].rbegin(); sbb != (*subBB)[bb->id].rend(); sbb++) {
          if (ComputeBBLiveness(*sbb)) {
            haschange = true;
          }
        }
        CopySubBBInfoToBB(bb);
      } else {
        if (ComputeBBLiveness(bb)) {
          haschange = true;
        }
      }
    }
  } while (haschange);

  AnalysisFinish();
  AnalysisFinalizeCleanupBB();

  if (hasSubBB) {
    FOR_ALL_BB(bb, cgfunc_) {
      CopySubBBInfoToBB(bb);
    }
  }
}

void LiveAnalysis::CopySubBBInfoToBB(BB *bb) {
  if (((*subBB)[bb->id]).size() == 0) {
    return;
  }
  bb->livein_regno.clear();
  bb->livein_regno.insert(((*subBB)[bb->id])[0]->livein_regno.begin(), ((*subBB)[bb->id])[0]->livein_regno.end());
  bb->liveout_regno.clear();
  uint32 numSubBB = (*subBB)[bb->id].size();
  bb->liveout_regno.insert(((*subBB)[bb->id])[numSubBB-1]->liveout_regno.begin(), ((*subBB)[bb->id])[numSubBB-1]->liveout_regno.end());
}

/* dump the current info of def/use/livein/liveout*/
void LiveAnalysis::Dump() {
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(cgfunc_->func->stIdx.Idx());
  LogInfo::MapleLogger() << "\n---------  liveness for " << funcSt->GetName() << "  iteration ";
  LogInfo::MapleLogger() << iteration << " ---------\n";
  FOR_ALL_BB(bb, cgfunc_) {
    LogInfo::MapleLogger() << "  === BB_" << bb->id << " <" << bb->GetKindName();
    if (bb->labidx != MIRLabelTable::kDummyLabel) {
      LogInfo::MapleLogger() << "[labeled with " << bb->labidx << "]";
    }
    LogInfo::MapleLogger() << "> idx " << bb->id << " ===\n";

    if (bb->preds.size() > 0) {
      LogInfo::MapleLogger() << "    pred [ ";
      MapleList<BB *>::iterator it = bb->preds.begin();
      while (it != bb->preds.end()) {
        LogInfo::MapleLogger() << (*it)->id << " ";
        it++;
      }
      LogInfo::MapleLogger() << "]\n";
    }
    if (bb->succs.size() > 0) {
      LogInfo::MapleLogger() << "    succ [ ";
      MapleList<BB *>::iterator it = bb->succs.begin();
      while (it != bb->succs.end()) {
        LogInfo::MapleLogger() << (*it)->id << dec << " ";
        it++;
      }
      LogInfo::MapleLogger() << "]\n";
    }
    DumpBBLivenessInfo(bb);
  }
  LogInfo::MapleLogger() << "---------------------------\n";
}

/* initialize dependent info and container of BB.*/
void LiveAnalysis::InitBB(BB *bb) {
  bb->livein_change = true;
  bb->insertuse = false;
  bb->use_regno.clear();
  bb->def_regno.clear();
  bb->livein_regno.clear();
  bb->liveout_regno.clear();
}

/* entry for LiveAnalysis*/
AnalysisResult *CgDoLiveAnalysis::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  MemPool *mp = mempoolctrler.NewMemPool(PhaseName().c_str());
  CHECK_FATAL(mp != nullptr, "mp is null in CgDoLiveAnalysis::Run");
  LiveAnalysis *liveanalysis = cgfunc->NewLiveAnalysis(cgfunc, mp);
  CHECK_FATAL(liveanalysis != nullptr, "liveanalysis is null in CgDoLiveAnalysis::Run");

  liveanalysis->ClearDoEhLiveAnalysis();

  liveanalysis->AnalysisLive();

  if (LIVEANALYZEDUMP) {
    liveanalysis->Dump();
  }
  return liveanalysis;
}

AnalysisResult *CgDoLiveAnalysisEh::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  MemPool *mp = mempoolctrler.NewMemPool(PhaseName().c_str());
  CHECK_FATAL(mp != nullptr, "mp is null in CgDoLiveAnalysis::Run");
  LiveAnalysis *liveanalysis = cgfunc->NewLiveAnalysis(cgfunc, mp);
  CHECK_FATAL(liveanalysis != nullptr, "liveanalysis is null in CgDoLiveAnalysis::Run");

  liveanalysis->SetDoEhLiveAnalysis();

  MapleAllocator phaseAllocator(mp);
  MapleVector<vector<BB *>> tmpSubBB(phaseAllocator.Adapter());
  liveanalysis->subBB = &tmpSubBB;
  liveanalysis->subBB->clear();
  liveanalysis->subBB->resize(cgfunc->NumBBs());

  liveanalysis->AnalysisLive();

  if (LIVEANALYZEDUMP) {
    liveanalysis->Dump();
  }
  return liveanalysis;
}

}  // namespace maplebe
