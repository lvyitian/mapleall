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

/**
 *
 * We do two types of condition based optimization here:
 *
 * 1. condition based null pointer check(NPC) elimination
 * 2. condition based RC elimination
 *
 * both use post dominance data to see if the symbol they are dealing with is
 * zero or not. To be specific, retrieve every stmt of all the bbs', if the stmt
 * is an assertnonnull/MCC_DecRef_NaiveRCFast, then retrieve the enclosing bb's
 * every post dominance frontiers in a backward order, if a frontier is found of
 * conditional branch, and the `expr` it compared is the same as that of
 * the assertnonnull/MCC_DecRef_NaiveRCFast, then the value of the symbol can be
 * known. If it's not zero, condbasednpc could remove the assertnonnull
 * statement; if it is a zero, then MCC_DecRef_NaiveRCFast could be removed.
 *
 * There are other conditions that the assertnonnull can be removed, e.g. if
 * the statement next to assertnonnull is an iread and the base is the
 * symbol assertnonnull is dealing with, then the assertnonnull can be removed,
 * because the exception will be thrown at this same place by iread, as the
 * symbol's value is zero.
 *
 */

#include "me_condbased_opt.h"
#include "me_const.h"

namespace maple {

// check if varmeexpr is under the condition varmeexpr == 0 when expectedeq0 is true,
// or varmeexpr != 0 if expectedeq0 is false
bool CondBased::NullValueFromOneTestCond(VarMeExpr *varmeexpr, BB *cdbb, BB *bb, bool expectedeq0) {
  MeStmt* lastMeStmt = cdbb->meStmtList.last;
  if (lastMeStmt == nullptr) {
    return false;
  }
  if (!lastMeStmt->IsCondBr()) {
    return false;
  }
  CondGotoMeStmt *condbrmestmt = static_cast<CondGotoMeStmt *>(lastMeStmt);
  bool istruebr = condbrmestmt->op == OP_brtrue;
  MeExpr *testmeexpr = condbrmestmt->opnd;
  if (testmeexpr->op != OP_eq && testmeexpr->op != OP_ne) {
    return false;
  }
  bool iseq = testmeexpr->op == OP_eq;
  OpMeExpr *cmpmeexpr = static_cast<OpMeExpr *>(testmeexpr);
  if (cmpmeexpr->GetOpnd(0)->meOp != kMeOpVar || cmpmeexpr->GetOpnd(1)->op != OP_constval) {
    return false;
  }
  ConstMeExpr *constmeexpr = static_cast<ConstMeExpr *>(cmpmeexpr->GetOpnd(1));
  if (!constmeexpr->IsZero()) {
    return false;
  }
  if (!cmpmeexpr->GetOpnd(0)->IsSameVariableValue(varmeexpr)) {
    return false;
  }
  BB *sucdombb = nullptr;
  for (uint32 i = 0; i < cdbb->succ.size(); i++) {
    BB *sucbb = cdbb->succ[i];
    if (dominance->Dominate(sucbb, bb)) {
      sucdombb = sucbb;
      break;
    }
  }
  if (!sucdombb) {
    return false;
  }
  bool isjumptobb = sucdombb->bbLabel == condbrmestmt->offset;
  if (expectedeq0) {
    iseq = !iseq;
  }
  return isjumptobb ? ((istruebr && !iseq) || (!istruebr && iseq)) : ((istruebr && iseq) || (!istruebr && !iseq));
}

bool CondBased::NullValueFromTestCond(VarMeExpr *varmeexpr, BB *bb, bool expectedeq0) {
  MapleSet<BBId> *pdomfrt = &dominance->pdomFrontier[bb->id.idx];
  uint32 sizeofbb = dominance->bbVec.size();
  std::vector<bool> visitedmap(sizeofbb, false);
  bool provennull = false;

  while (pdomfrt->size() == 1) {
    BB *cdbb = func->bbVec[(pdomfrt->begin())->idx];
    if (visitedmap[cdbb->id.idx]) {
      break;
    }
    visitedmap[cdbb->id.idx] = true;
    if (NullValueFromOneTestCond(varmeexpr, cdbb, bb, expectedeq0)) {
      provennull = true;
      break;
    }
    pdomfrt = &dominance->pdomFrontier[cdbb->id.idx];
  }
  return provennull;
}

bool CondBased::IsIreadWithTheBase(VarMeExpr *x, MeExpr *meexpr) {
  if (meexpr->op == OP_iread) {
    IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(meexpr);
    if (ivarmeexpr->base->exprID == x->exprID) {
      return true;
    }
  }
  for (int32 i = 0; i < meexpr->NumMeExprOpnds(); i++) {
    if (IsIreadWithTheBase(x, meexpr->GetOpnd(i))) {
      return true;
    }
  }
  return false;
}

bool CondBased::StmtHasDereferencedBase(MeStmt *stmt, VarMeExpr *x) {
  if (stmt->op == OP_iassign) {
    IassignMeStmt *iastmt = static_cast<IassignMeStmt *>(stmt);
    if (iastmt->lhsVar->base->exprID == x->exprID) {
      return true;
    }
  }
  if (stmt->op == OP_syncenter || stmt->op == OP_syncexit) {
    SyncMeStmt *syncmestmt = static_cast<SyncMeStmt *>(stmt);
    MapleVector<MeExpr *> &opnds = syncmestmt->opnds;
    for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
      if ((*it)->exprID == x->exprID) {
        return true;
      }
    }
  }
  for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
    MeExpr *meexpr = stmt->GetMeStmtOpnd(i);
    if (IsIreadWithTheBase(x, meexpr)) {
      return true;
    }
  }
  return false;
}

bool CondBased::PointerWasDereferencedBefore(VarMeExpr *x, UnaryMeStmt *assertmestmt, BB *bb) {
  // If x is defined in the function, let BBx be the BB that defines x.
  // If x is not defined, then let BBx be the function entry BB.
  // Let BBy be the current BB that contains the assertnonnull.
  // Search backward along the path in the dominator tree from BBy to BBx.
  // If it sees an iread or iassign whose base is x, then the assertnonnull can be deleted.
  MeStmt *defmestmt = nullptr;
  BB *bbx = x->GetDefByBBMeStmt(dominance, defmestmt);

  if (!bbx) {
    return false;
  }
  CHECK_FATAL(dominance->Dominate(bbx, bb), "bbx should dominate bb at this point");

  const MapleVector<BB *> &doms = dominance->Getdoms();
  for (MeStmt *stmt = assertmestmt->prev; stmt; stmt = stmt->prev) {
    if (StmtHasDereferencedBase(stmt, x)) {
      return true;
    }
  }
  if (bbx == bb) {
    return false;
  }
  BB *itbb = doms[bb->id.idx];
  while (itbb != bbx) {
    // check if there is an iread or iassign in itbb whose base is x
    for (MeStmt *stmt = itbb->meStmtList.last; stmt; stmt = stmt->prev) {
      if (StmtHasDereferencedBase(stmt, x)) {
        return true;
      }
    }
    itbb = doms[itbb->id.idx];
  }
  for (MeStmt *stmt = bbx->meStmtList.last; stmt != defmestmt; stmt = stmt->prev) {
    if (StmtHasDereferencedBase(stmt, x)) {
      return true;
    }
  }
  return false;
}

bool CondBased::PointerWasDereferencedRightAfter(VarMeExpr *x, UnaryMeStmt *assertmestmt) {
  // assertnonnull(x)
  // t = iread(x, 0)
  // we can safely delete assertnonnull(x)
  // if we got
  // assertnonnull(x)
  // t2 <-
  // t = iread (x)
  // we can't remove assertnonnull(x) safely, because if x is null, then the position of throwing exception is not the
  // same.
  MeStmt *nextmestmt = assertmestmt->GetNextMeStmt();
  if (nextmestmt && StmtHasDereferencedBase(nextmestmt, x)) {
    return true;
  }
  return false;
}

bool CondBased::IsNotNullValue(VarMeExpr *varmeexpr, UnaryMeStmt *assertmestmt, BB *bb) {
  OriginalSt *varost = varmeexpr->ost;
  if (varost->isFormal && varost->GetMIRSymbol()->GetName() == kStrThisPointer) {
    return true;
  }
  if (varmeexpr->defBy == kDefByStmt && varmeexpr->def.defStmt->op == OP_dassign) {
    DassignMeStmt *xdass = static_cast<DassignMeStmt *>(varmeexpr->def.defStmt);
    MeExpr *rhs = xdass->rhs;
    if (rhs->op == OP_gcmalloc || rhs->op == OP_gcmallocjarray) {
      return true;
    }
    if (rhs->meOp == kMeOpVar) {
      VarMeExpr *rhsvarmeexpr = static_cast<VarMeExpr *>(rhs);
      OriginalSt *ost = rhsvarmeexpr->ost;
      if (ost->isFormal && ost->GetMIRSymbol()->GetName() == kStrThisPointer) {
        return true;
      }
    }
  }
  if (PointerWasDereferencedBefore(varmeexpr, assertmestmt, bb)) {
    return true;
  }
  if (PointerWasDereferencedRightAfter(varmeexpr, assertmestmt)) {
    return true;
  }
  return false;
}

AnalysisResult *MeDoCondBasedRC::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));

  CondBased condbasedrc(func, dom);

  for (BB *bb : func->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->meStmtList) {
      if (stmt->op == OP_decref) {
        UnaryMeStmt *decref = static_cast<UnaryMeStmt *>(stmt);
        if (decref->opnd->meOp != kMeOpVar) {
          continue;
        }
        VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(decref->opnd);
        OriginalSt *ost = varmeexpr->ost;
        if (!ost->isLocal && ost->IsVolatile()) {
          // global volatile cannot be optimized
          continue;
        }
        if (condbasedrc.NullValueFromTestCond(varmeexpr, bb, true)) {
          MeStmt *refassign = stmt->next;
          bb->RemoveMeStmt(stmt);  // delete the decref
          if (refassign != nullptr) {
            refassign->SetNeedDecref(false);
          } else {
            break;
          }
          stmt = refassign;  // next iteration will process the stmt after refassign
        }
      } else if (stmt->op == OP_decrefreset) {
        DecrefresetMeStmt *decrefrst = static_cast<DecrefresetMeStmt *>(stmt);
        if (condbasedrc.NullValueFromTestCond(decrefrst->thevar, bb, true)) {
          bb->RemoveMeStmt(stmt);  // delete the decrefreset
        }
      }
    }
  }

  return nullptr;
}

AnalysisResult *MeDoCondBasedNPC::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));

  CondBased condbasednpc(func, dom);
  for (BB *bb : func->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->meStmtList) {
      if (stmt->op != OP_assertnonnull) {
        continue;
      }
      UnaryMeStmt *assertmestmt = static_cast<UnaryMeStmt *>(stmt);
      if (assertmestmt->opnd->meOp != kMeOpVar) {
        continue;
      }

      VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(assertmestmt->opnd);
      if (condbasednpc.NullValueFromTestCond(varmeexpr, bb, false) ||
          condbasednpc.IsNotNullValue(varmeexpr, assertmestmt, bb)) {
        bb->RemoveMeStmt(stmt);
      }
    }
  }
  return nullptr;
}

}  // namespace maple
