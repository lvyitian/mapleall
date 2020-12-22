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

#ifndef MAPLE_ME_INCLUDE_LFO_PRE_EMIT_H
#define MAPLE_ME_INCLUDE_LFO_PRE_EMIT_H
#include "me_irmap.h"
#include "me_phase.h"

namespace maple {

class LfoPreEmitter {
 private:
  MeIRMap *meirmap;
  LfoFunction *lfoFunc;
  MIRFunction *mirFunc;
  MemPool *codeMP;
  MapleAllocator *codeMPAlloc;
  MirCFG *cfg;

 public:
  LfoPreEmitter(MeIRMap *hmap, LfoFunction *f) : meirmap(hmap),
    lfoFunc(f),
    mirFunc(f->meFunc->mirFunc),
    codeMP(f->meFunc->mirFunc->codeMemPool),
    codeMPAlloc(&f->meFunc->mirFunc->codeMemPoolAllocator),
    cfg(f->meFunc->theCFG) {}

 private:
  LfoParentPart *EmitLfoExpr(MeExpr*, LfoParentPart *);
  StmtNode* EmitLfoStmt(MeStmt *, LfoParentPart *);
  void EmitBB(BB *, LfoBlockNode *);
  DoloopNode *EmitLfoDoloop(BB *, LfoBlockNode *, LfoWhileInfo *);
  WhileStmtNode *EmitLfoWhile(BB *, LfoBlockNode *);
  uint32 Raise2LfoWhile(uint32, LfoBlockNode *);
  uint32 Raise2LfoIf(uint32, LfoBlockNode *);
 public:
  uint32 EmitLfoBB(uint32, LfoBlockNode *);
};

/*emit ir to specified file*/
class DoLfoPreEmission : public MeFuncPhase {
 public:
  DoLfoPreEmission(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;
  std::string PhaseName() const override { return "lfopreemit"; }
};

}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_LFO_PRE_EMIT_H
