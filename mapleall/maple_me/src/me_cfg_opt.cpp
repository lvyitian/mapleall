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

// We find the following pattern in JAVA code:
// if (empty) {
//    a = 1;
// } else {
//    a = 3;
// }
//
// It will generate the following IR in mpl file:
// brfalse @label1 (dread u1 %Reg4_Z)
// dassign %Reg1_I 0 (cvt i32 i8 (constVal i8 1))
// goto @label2
// @label1   dassign %Reg1_I 0 (cvt i32 i8 (constVal i8 3))
//
// The mecfgopt phase overwrite the upper pattern with following code:
// regassign i32 %26 (select i32 (
//    select u1 (
//      ne u1 i32 (regread i32 %1, constVal i32 0),
//      constVal u1 0,
//      constVal u1 1),
//    constVal i32 1,
//    constVal i32 3))
#include "me_cfg_opt.h"
#include "me_bb_layout.h"
#include "me_irmap.h"

namespace maple {

bool MeCfgOpt::IsOk2Select(MeExpr *expr0, MeExpr *expr1) {
  if (expr0->exprID == expr1->exprID) {
    return true;
  }
  std::set<int32> expensiveops1;
  std::set<int32> expensiveops2;
  if (CollectExpensiveOps(expr0, expensiveops1)) {
    return false;
  }
  if (CollectExpensiveOps(expr1, expensiveops2)) {
    return false;
  }

  if (expensiveops1.size() != expensiveops2.size()) {
    return false;
  }

  for (std::set<int32>::iterator it = expensiveops1.begin(); it != expensiveops1.end(); it++) {
    int32 expr1id = *it;
    if (expensiveops2.find(expr1id) == expensiveops2.end()) {
      return false;
    }
  }
  return true;
}

bool MeCfgOpt::HasFloatCmp(MeExpr *meexpr) {
  Opcode op = meexpr->op;
  if ((op == OP_cmpl || op == OP_cmpg || op == OP_cmp) && IsPrimitiveFloat(static_cast<OpMeExpr *>(meexpr)->opndType)) {
    return true;
  }
  for (int32 i = 0; i < meexpr->NumMeExprOpnds(); i++) {
    if (HasFloatCmp(meexpr->GetOpnd(i))) {
      return true;
    }
  }
  return false;
}

bool MeCfgOpt::PreCheck(MeFunction *func) {
  const uint32 kSize = func->theCFG->bbVec.size();
  MapleVector<BB *> &bbvec = func->theCFG->bbVec;
  for (uint32 i = 0; i < kSize; i++) {
    BB *bb = bbvec[i];
    if (!bb) {
      continue;
    }
    if (bb->isTry && bb->bbLabel != 0) {
      for (uint32 ii = 0; ii < bb->pred.size(); ii++) {
        BB *predbb = bb->pred[ii];
        if (!predbb->isTry) {
          // comes from outside of try. give up folding select. TODO: fixme later
          return false;
        }
      }
    }
    for (auto stmt : bb->meStmtList) {
      for (int32 ii = 0; ii < stmt->NumMeStmtOpnds(); ii++) {
        MeExpr *meexpr = stmt->GetMeStmtOpnd(ii);
        if (HasFloatCmp(meexpr)) {
          return false;
        }
      }
    }
  }
  return true;
}

bool MeCfgOpt::Run(MeFunction *func) {
  if (!PreCheck(func)) {
    return false;
  }
  uint32 i = 0;
  uint32 nexti = -1;
  const uint32 kSize = func->theCFG->bbVec.size();
  MapleVector<BB *> &bbvec = func->theCFG->bbVec;
  while (i < kSize) {
    nexti = i + 1;
    BB *bb = bbvec[i];
    if (bb == nullptr) {
      i = nexti;
      continue;
    }
    if (bb->kind == kBBCondGoto && bb->succ.size() == 2) {
      // && bbvec[i+1] && bbvec[i+2]) {
      MeStmt *condmestmt = bb->GetCondBrStmt();
      if (!condmestmt || !condmestmt->IsCondBr()) {
        i = nexti;
        continue;
      }
      BB *i1bb = bb->succ[0];
      BB *i2bb = bb->succ[1];
      if (i1bb->succ.size() != 1 || i1bb->pred.size() != 1 || i2bb->pred.size() != 1 || i2bb->succ.size() != 1) {
        i = nexti;
        continue;
      }
      CondGotoMeStmt *condgotomestmt = static_cast<CondGotoMeStmt *>(condmestmt);
      if (i1bb->bbLabel && i1bb->bbLabel == condgotomestmt->offset) {
        BB *tmpbb = i1bb;
        i1bb = i2bb;
        i2bb = tmpbb;
      }
      if (i1bb->kind != kBBGoto || i1bb->pred[0]->id != bb->id || i2bb->kind != kBBFallthru ||
          (i2bb->pred[0]->id != bb->id) || (i1bb->succ[0] != i2bb->succ[0])) {
        i = nexti;
        continue;
      }
      MeStmt *assstmt1 = i1bb->GetTheOnlyMeStmtWithGoto();
      if (!(assstmt1 && (assstmt1->op == OP_iassign || assstmt1->op == OP_dassign))) {
        i = nexti;
        continue;
      }
      MeStmt *assstmt2 = i2bb->GetTheOnlyMeStmt();
      if (!(assstmt2 && (assstmt2->op == OP_iassign || assstmt2->op == OP_dassign) && assstmt2->op == assstmt1->op)) {
        i = nexti;
        continue;
      }
      bool istruebr = (condmestmt->op == OP_brtrue);
      bool isiassign = (assstmt1->op == OP_iassign);
      MeExpr *selectop1 = nullptr;
      MeExpr *selectop2 = nullptr;
      if (isiassign) {
        IassignMeStmt *iass1 = static_cast<IassignMeStmt *>(assstmt1);
        IassignMeStmt *iass2 = static_cast<IassignMeStmt *>(assstmt2);
        if (iass1->lhsVar->base != iass2->lhsVar->base || iass1->lhsVar->fieldID != iass2->lhsVar->fieldID) {
          i = nexti;
          continue;
        }
        // patter match
        selectop1 = iass1->rhs;
        selectop2 = iass2->rhs;
      } else {
        DassignMeStmt *iass1 = static_cast<DassignMeStmt *>(assstmt1);
        DassignMeStmt *iass2 = static_cast<DassignMeStmt *>(assstmt2);
        if (!iass1->lhs->IsUseSameSymbol(iass2->lhs)) {
          i = nexti;
          continue;
        }
        selectop1 = iass1->rhs;
        selectop2 = iass2->rhs;
      }
      if (!IsOk2Select(selectop1, selectop2)) {
        i = nexti;
        continue;
      }
      MeExpr *selectmeexpr = meirmap->CreateMeExprSelect(
        selectop1->primType, condgotomestmt->opnd, istruebr ? selectop2 : selectop1, istruebr ? selectop1 : selectop2);
      if (isiassign) {
        // use bb as the new bb and put new iassign there
        static_cast<IassignMeStmt *>(assstmt1)->rhs = selectmeexpr;
      } else {
        static_cast<DassignMeStmt *>(assstmt1)->rhs = selectmeexpr;
      }
      bb->ReplaceMeStmt(condmestmt, assstmt1);
      // update the preds and succs
      bb->succ.clear();
      BB *succbb = i1bb->succ[0];
      bb->succ.push_back(succbb);
      std::vector<BB *> tmpvec;
      for (uint32 ii = 0; ii < succbb->pred.size(); ii++) {
        if (succbb->pred[ii] != i1bb && succbb->pred[ii] != i2bb) {
          tmpvec.push_back(succbb->pred[ii]);
        }
      }
      succbb->pred.clear();
      for (uint32 ii = 0; ii < tmpvec.size(); ii++) {
        succbb->pred.push_back(tmpvec[ii]);
      }
      succbb->pred.push_back(bb);
      bb->kind = kBBFallthru;
      bbvec[i1bb->id.idx] = nullptr;
      bbvec[i2bb->id.idx] = nullptr;
      SetCfgChanged();
    }
    i = nexti;
  }
  return IsCfgChanged();
}

void MeDoCfgOpt::EmitMapleir(MeFunction *func, MeFuncResultMgr *m) {
  if (func->theCFG->NumBBs() > 0) {
    BBLayout *layoutbbs = static_cast<BBLayout *>(m->GetAnalysisResult(MeFuncPhase_BBLAYOUT, func, !MeOption::quiet));
    CHECK_FATAL(func->irMap != nullptr, "");
    MIRFunction *mirfunction = func->mirFunc;
    if (mirfunction->codeMemPool != nullptr) {
      mempoolctrler.DeleteMemPool(mirfunction->codeMemPool);
    }
    mirfunction->codeMemPool = mempoolctrler.NewMemPool("IR from IRMap::Emit()");
    mirfunction->codeMemPoolAllocator.SetMemPool(mirfunction->codeMemPool);
    mirfunction->body = mirfunction->codeMemPool->New<BlockNode>();
    for (size_t k = 1; k < mirfunction->symTab->GetSymbolTableSize(); k++) {
      MIRSymbol *sym = mirfunction->symTab->GetSymbolFromStIdx(k);
      if (sym->sKind == kStVar) {
        sym->SetIsDeleted();
      }
    }
    for (BB *bb : *layoutbbs->GetBBs()) {
      ASSERT(bb != nullptr, "");
      bb->EmitBB(func->meSSATab, func->mirFunc->body, func->mirFunc->freqMap, true);
    }
  }
}

AnalysisResult *MeDoCfgOpt::Run(MeFunction *func, MeFuncResultMgr *m) {
  MIRFunction *mirfunction = func->mirFunc;
  MeIRMap *irMap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func, !MeOption::quiet));
  ASSERT(irMap != nullptr, "");

  MeCfgOpt cfgopt(irMap);
  if (cfgopt.Run(func)) {
    EmitMapleir(func, m);
    m->InvalidAllResults();
    func->meSSATab = nullptr;
    func->irMap = nullptr;
    return nullptr;
  }

  return nullptr;
}

}  // namespace maple
