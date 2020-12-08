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

#include "me_emit.h"
#include "me_bb_layout.h"
#include "me_irmap.h"
#include "constant_fold.h"
#include "me_cfg.h"
#include "me_predict.h"

static const std::set<std::string> kHssaPhases{
  "irmapbuild",  "hprop",      "hssa",        "hdse",        "ssadevirt",    "ea",          "epre",
  "lpre",   "stmtpre",    "rename2preg", "ti",          "analyzerc",
  "spre",   "rclowering", "delegaterc",  "condbasedrc", "condbasednpc", "may2dassign", "pregrename",
  "bdcopt", "syncselect", "cfgopt",      "ssadevirt",   "placementrc",  "symrename",
};

namespace maple {

/* emit IR to specified file */
AnalysisResult *MeDoEmission::Run(MeFunction *func, MeFuncResultMgr *m) {
  std::string passName = GetPreviousPhaseName();  // get previous phase
  if (passName == "lfopreemit" || passName == "cfgopt") {
    return nullptr;
  }

  MirCFG *cfg = static_cast<MirCFG *>(m->GetAnalysisResult(MeFuncPhase_CFGBUILD, func, !MeOption::quiet));
  ASSERT(cfg != nullptr, "cfgbuild phase has problem");

  bool emitHssaOrAfter = kHssaPhases.find(std::string(passName)) != kHssaPhases.end();

  if (func->theCFG->NumBBs() > 0) {
    /* generate bblist after layout (bb physical position) */
    if (emitHssaOrAfter) {
      MePrediction *predict = static_cast<MePrediction *>(m->GetAnalysisResult(MeFuncPhase_PREDICT, func, !MeOption::quiet));
      ASSERT(predict != nullptr, "predict phase has problem");
    }
    BBLayout *layoutbbs = static_cast<BBLayout *>(m->GetAnalysisResult(MeFuncPhase_BBLAYOUT, func, !MeOption::quiet));
    ASSERT(layoutbbs != nullptr, "layout phase has problem");
    if (emitHssaOrAfter) {
      ASSERT(func->irMap != nullptr, "");
      MIRFunction *mirfunction = func->mirFunc;
      if (mirfunction->codeMemPool != nullptr) {
        mempoolctrler.DeleteMemPool(mirfunction->codeMemPool);
      }
      mirfunction->codeMemPool = mempoolctrler.NewMemPool("IR from IRMap::Emit()");
      mirfunction->codeMemPoolAllocator.SetMemPool(mirfunction->codeMemPool);
      mirfunction->body = mirfunction->codeMemPool->New<BlockNode>();
      // initialize isDeleted field to true; will reset when emitting Maple IR
      for (size_t k = 1; k < mirfunction->symTab->GetSymbolTableSize(); k++) {
        MIRSymbol *sym = mirfunction->symTab->GetSymbolFromStIdx(k);
        if (sym->sKind == kStVar &&
            sym->GetName() != "__Exc_Ptr__" &&
            sym->GetName() != "__Exc_Filter__") {
          sym->SetIsDeleted();
        }
      }
      for (BB *bb : *layoutbbs->GetBBs()) {
        ASSERT(bb != NULL, "");
        bb->EmitBB(func->meSSATab, mirfunction->body, mirfunction->freqMap);
      }
    } else {
      func->EmitBeforeHSSA(func->mirFunc, layoutbbs->GetBBs());
    }
    if (!DEBUGFUNC(func)) {
      // constantfolding does not update BB's stmtNodeList, which breaks MirCFG::DumpToFile()
      maple::ConstantFold cf(&func->mirModule);
      cf.Simplify(func->mirFunc->body);
    }
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "\n==============after meemit =============" << std::endl;
      func->mirFunc->Dump();
      LogInfo::MapleLogger() << "\nRC operations do not need locking" << std::endl;
      for (BB *bb : func->theCFG->bbVec) {
        if (bb == nullptr) {
          continue;
        }
        for (auto stmt : bb->stmtNodeList) {
          if (func->mirModule.rcNotNeedingLock.find(stmt->stmtID) != func->mirModule.rcNotNeedingLock.end()) {
            LogInfo::MapleLogger() << func->mirModule.GetFilenameFromFilenum(stmt->srcPosition.Filenum()) << ":" << stmt->srcPosition.Linenum()
                 << "\t";
            stmt->Dump(&func->mirModule, 0);
          }
        }
      }
    }
    if (DEBUGFUNC(func)) {
      func->theCFG->DumpToFile("emit", true);
    }
  }
  return nullptr;
}

}  // namespace maple
