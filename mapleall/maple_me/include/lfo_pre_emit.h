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
 public:
  MeIRMap *meirmap;
  MIRModule *mirModule;
  LfoFunction *lfoFunc;
  MIRFunction *mirFunc;
  MirCFG *cfg;
  MapleMap<MeStmt *, StmtNode *> meir2stmtMap;

 public:
  LfoPreEmitter(MeIRMap *hmap, LfoFunction *f) : meirmap(hmap),
    mirModule(hmap->mirModule),
    lfoFunc(f),
    mirFunc(f->meFunc->mirFunc),
    cfg(f->meFunc->theCFG),
    meir2stmtMap(lfoFunc->lfoAlloc.Adapter()) {}

 private:
   void EmitBBNoLabel(BB *, BlockNode *, bool);
   void EmitBBStmts(BB *bb, BlockNode *);
   void UpdateUsesDefs(BaseNode *, MeExpr *);
   void UpdateUsesDefs4Dread(LfoDreadNode *, VarMeExpr *, std::set<MePhiNode *> &);
   StmtNode *GetOrCreateStmtNodeFromMestmt(MeStmt *);

 public:
  LfoParentPart *EmitLfoDread(VarMeExpr *, LfoParentPart *, bool);
  LfoParentPart *EmitLfoExpr(MeExpr*, LfoParentPart *, bool);
  StmtNode *EmitLfoDassign(DassignMeStmt *, LfoParentPart *);
  StmtNode *EmitLfoRegassign(AssignMeStmt *, LfoParentPart *);
  StmtNode *EmitLfoIassign(IassignMeStmt *, LfoParentPart *);
  StmtNode* EmitLfoStmt(MeStmt *, LfoParentPart *);
  LfoBlockNode *EmitLfoBlockNode (LfoParentPart *);
  void InitBaseNodeByMeExpr (BaseNode *bd, MeExpr *meExpr) {
    bd->primType = meExpr->primType;
    bd->numOpnds = meExpr->numOpnds;
  }
  void EmitBB(BB *, LfoBlockNode *);
  DoloopNode *EmitLfoDoloop(BB *, LfoBlockNode *, LfoWhileInfo *);
  WhileStmtNode *EmitLfoWhile(BB *, LfoBlockNode *);
  uint32 Raise2LfoWhile(uint32, LfoBlockNode *);
  uint32 Raise2LfoIf(uint32, LfoBlockNode *);
  uint32 EmitLfoBB(uint32, LfoBlockNode *);
};

/*emit ir to specified file*/
class DoLfoPreEmission : public MeFuncPhase {
 public:
  DoLfoPreEmission(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "lfopreemit";
  }
};

}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_LFO_PRE_EMIT_H
