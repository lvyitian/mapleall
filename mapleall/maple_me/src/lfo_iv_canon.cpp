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
#include "lfo_iv_canon.h"
#include "me_option.h"
#include "lfo_function.h"

// This phase implements Step 4 of the paper:
//   S.-M. Liu, R. Lo and F. Chow, "Loop Induction Variable
//   Canonicalization in Parallelizing Compiler", Intl. Conf. on Parallel
//   Architectures and Compilation Techniques (PACT 96), Oct 1996.

using namespace std;

namespace maple {

// Resolve value of x; return false if result is not of induction expression
// form; goal is to resolve to an expression where the only non-constant is
// philhs
bool IVCanon::ResolveExprValue(MeExpr *x, ScalarMeExpr *philhs) {
  switch (x->meOp) {
  case kMeOpConst: return IsPrimitiveInteger(x->primType);
  case kMeOpVar:
  case kMeOpReg:{
    if (x == philhs) {
      return true;
    }
    ScalarMeExpr *scalar = static_cast<ScalarMeExpr *>(x);
    if (scalar->defBy != kDefByStmt) {
      return false;
    }
    AssignMeStmt *defstmt = scalar->def.defStmt;
    return ResolveExprValue(defstmt->rhs, philhs);
  }
  case kMeOpOp: {  // restricting to only + and - for now
    if (x->op != OP_add && x->op != OP_sub) {
      return false;
    }
    OpMeExpr *opexp = static_cast<OpMeExpr *>(x);
    return ResolveExprValue(opexp->GetOpnd(0), philhs) &&
           ResolveExprValue(opexp->GetOpnd(1), philhs);
  }
  default: ;
  }
  return false;
}

// appearances accumulates the number of appearances of the induction variable;
// it is negative if it is subtracted
int32 IVCanon::ComputeIncrAmt(MeExpr *x, ScalarMeExpr *philhs, int32 *appearances) {
  switch (x->meOp) {
  case kMeOpConst: {
    MIRConst *konst = static_cast<ConstMeExpr *>(x)->constVal;
    CHECK_FATAL(konst->kind == kConstInt, "ComputeIncrAmt: must be integer constant");
    MIRIntConst *intconst = static_cast<MIRIntConst *>(konst);
    return intconst->value;
  }
  case kMeOpVar:
  case kMeOpReg:{
    if (x == philhs) {
      *appearances = 1;
      return 0;
    }
    ScalarMeExpr *scalar = static_cast<ScalarMeExpr *>(x);
    CHECK_FATAL(scalar->defBy == kDefByStmt, "ComputeIncrAmt: cannot be here");
    AssignMeStmt *defstmt = scalar->def.defStmt;
    return ComputeIncrAmt(defstmt->rhs, philhs, appearances);
  }
  case kMeOpOp: {
    CHECK_FATAL(x->op == OP_add || x->op == OP_sub, "ComputeIncrAmt: cannot be here");
    OpMeExpr *opexp = static_cast<OpMeExpr *>(x);
    int32 appear0 = 0;
    int64 incrAmt0 = ComputeIncrAmt(opexp->GetOpnd(0), philhs, &appear0);
    int32 appear1 = 0;
    int64 incrAmt1 = ComputeIncrAmt(opexp->GetOpnd(1), philhs, &appear1);
    if (x->op == OP_sub) {
      *appearances = appear0 - appear1;
      return incrAmt0 - incrAmt1;
    } else {
      *appearances = appear0 + appear1;
      return incrAmt0 + incrAmt1;
    }
  }
  default: ;
  }
  CHECK_FATAL(false, "ComputeIncrAmt: should not be here");
  return 0;
}

// determine the initial and step values of the IV and push info to ivvec
void IVCanon::CharacterizeIV(ScalarMeExpr *initversion, ScalarMeExpr *loopbackversion, ScalarMeExpr *philhs) {
  IVDesc *ivdesc = mp->New<IVDesc>(initversion->ost);
  if (initversion->defBy == kDefByStmt) {
    AssignMeStmt *defstmt = initversion->def.defStmt;
    ivdesc->initExpr = defstmt->rhs;
  } else {
    ivdesc->initExpr = initversion;
  }
  int32 appearances = 0;
  ivdesc->stepValue = ComputeIncrAmt(loopbackversion, philhs, &appearances);
  if (appearances == 1) {
    ivvec.push_back(ivdesc);
  }
}

void IVCanon::FindPrimaryIV() {
  for (uint32 i = 0; i < ivvec.size(); i++) {
    IVDesc *ivdesc = ivvec[i];
    if (ivdesc->stepValue == 1) {
      bool injected = false;
      if (ivdesc->ost->IsSymbol() &&
          strncmp(ivdesc->ost->GetMIRSymbol()->GetName().c_str(), "injected.iv", 11) == 0) {
        injected = true;
      }
      if (!injected) {
        idxPrimaryIV = i;
        return;
      }
      idxPrimaryIV = i;
    }
  }
}

bool IVCanon::IsLoopInvariant(MeExpr *x) {
  if (x == nullptr) {
    return true;
  }
  switch (x->meOp) {
  case kMeOpAddrof:
  case kMeOpAddroffunc:
  case kMeOpConst:
  case kMeOpConststr:
  case kMeOpConststr16:
  case kMeOpSizeoftype:
  case kMeOpFieldsDist: return true;
  case kMeOpVar:
  case kMeOpReg: {
    ScalarMeExpr *scalar = static_cast<ScalarMeExpr *>(x);
    BB *defBB = scalar->DefByBB();
    return defBB == nullptr || dominance->Dominate(defBB, aloop->head);
  }
  case kMeOpIvar: {
    IvarMeExpr *ivar = static_cast<IvarMeExpr *>(x);
    BB *defBB = ivar->mu->DefByBB();
    return defBB == nullptr || dominance->Dominate(defBB, aloop->head);
  }
  case kMeOpOp: {
    OpMeExpr *opexp = static_cast<OpMeExpr *>(x);
    return IsLoopInvariant(opexp->GetOpnd(0)) &&
           IsLoopInvariant(opexp->GetOpnd(1)) &&
           IsLoopInvariant(opexp->GetOpnd(2));
  }
  case kMeOpNary: {
    NaryMeExpr *opexp = static_cast<NaryMeExpr *>(x);
    for (uint32 i = 0; i < opexp->numOpnds; i++) {
      if (!IsLoopInvariant(opexp->GetOpnd(i))) {
        return false;
      }
    }
    return true;
  }
  default: ;
  }
  return false;
}

void IVCanon::ComputeTripCount() {
  MeIRMap *irMap = func->irMap;
  // find the termination test expression
  if (!aloop->head->meStmtList.last->IsCondBr()) {
    return;
  }
  CondGotoMeStmt *condbr = static_cast<CondGotoMeStmt *>(aloop->head->meStmtList.last);
  if (!kOpcodeInfo.IsCompare(condbr->opnd->op)) {
    return;
  }
  OpMeExpr *testexp = static_cast<OpMeExpr *>(condbr->opnd);
  // make the side that consists of a single IV the left operand
  // check left operand
  ScalarMeExpr *iv = dynamic_cast<ScalarMeExpr *>(testexp->GetOpnd(0));
  IVDesc *ivdesc = nullptr;
  if (iv) {
    for (uint32 i = 0; i < ivvec.size(); i++) {
      if (iv->ost == ivvec[i]->ost) {
        ivdesc = ivvec[i];
        break;
      }
    }
  }
  if (ivdesc == nullptr) { // check second operand
    iv = dynamic_cast<ScalarMeExpr *>(testexp->GetOpnd(1));
    if (iv) {
      for (uint32 i = 0; i < ivvec.size(); i++) {
        if (iv->ost == ivvec[i]->ost) {
          ivdesc = ivvec[i];
          break;
        }
      }
    }
    if (ivdesc) {  // swap the 2 sides
      Opcode newop = testexp->op;
      switch (testexp->op) {
      case OP_lt: newop = OP_gt; break;
      case OP_le: newop = OP_ge; break;
      case OP_gt: newop = OP_lt; break;
      case OP_ge: newop = OP_le; break;
      default: ;
      }
      OpMeExpr opmeexpr(-1, newop, testexp->primType, 2);
      opmeexpr.SetOpnd(testexp->GetOpnd(1), 0);
      opmeexpr.SetOpnd(testexp->GetOpnd(0), 1);
      testexp = static_cast<OpMeExpr *>(irMap->HashMeExpr(&opmeexpr));
      condbr->opnd = testexp;
    }
  }
  if (ivdesc == nullptr || ivdesc->stepValue == 0) {
    return;  // no IV in the termination test
  }
  if (!IsLoopInvariant(testexp->GetOpnd(1))) {
    return; // the right side is not loop-invariant
  }

  // check the termination test is in the right sense
  if (ivdesc->stepValue > 0) {
    if (condbr->opnd->op == OP_gt || condbr->opnd->op == OP_ge) {
      return;
    }
  } else {
    if (condbr->opnd->op == OP_lt || condbr->opnd->op == OP_le) {
      return;
    }
  }

  // form the trip count expression
  PrimType primTypeUsed = testexp->GetOpnd(0)->primType;
  OpMeExpr subtract(-1, OP_sub, primTypeUsed, 2);
  OpMeExpr divide(-1, OP_div, primTypeUsed, 2);
  OpMeExpr add(-1, OP_add, primTypeUsed, 2);
  add.SetOpnd(testexp->GetOpnd(1), 0); // IV bound
  add.SetOpnd(irMap->CreateIntConstMeExpr(ivdesc->stepValue-1, primTypeUsed), 1);
  subtract.SetOpnd(irMap->HashMeExpr(&add), 0);
  subtract.SetOpnd(ivdesc->initExpr, 1);
  divide.SetOpnd(irMap->HashMeExpr(&subtract), 0);
  divide.SetOpnd(irMap->CreateIntConstMeExpr(ivdesc->stepValue, primTypeUsed),1);
  tripCount = irMap->HashMeExpr(&divide);
  tripCount = irMap->SimplifyMeExpr(dynamic_cast<OpMeExpr *>(tripCount));
}

void IVCanon::PerformIVCanon() {
  BB *headbb = aloop->head;
  uint32 phiOpndIdxOfInit = 1;
  uint32 phiOpndIdxOfLoopBack = 0;
  if (aloop->loop_bbs.count(headbb->pred[0]->id) == 0) {
    phiOpndIdxOfInit = 0;
    phiOpndIdxOfLoopBack = 1;
  }
  CHECK_FATAL(aloop->tail == headbb->pred[phiOpndIdxOfLoopBack], "PerformIVCanon: tail BB inaccurate");
  // go thru the list of phis at the loop head
  for (std::pair<OStIdx, MePhiNode*> mapEntry: headbb->mePhiList) {
    OriginalSt *ost = ssatab->GetOriginalStFromid(mapEntry.first);
    if (!ost->IsIVCandidate()) {
      continue;
    }
    ScalarMeExpr *philhs = mapEntry.second->lhs;
    ScalarMeExpr *initVersion = mapEntry.second->opnds[phiOpndIdxOfInit];
    ScalarMeExpr *loopbackVersion = mapEntry.second->opnds[phiOpndIdxOfLoopBack];
    if (ResolveExprValue(loopbackVersion, philhs)) {
      CharacterizeIV(initVersion, loopbackVersion, philhs);
    }
  }
  FindPrimaryIV();
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "****** while loop at label " << "@" << func->mirFunc->GetLabelName(headbb->bbLabel);
    LogInfo::MapleLogger() << ", BB id:" << headbb->id.idx << " has IVs:" << endl;
    for (uint32 i = 0; i < ivvec.size(); i++) {
      IVDesc *ivdesc = ivvec[i];
      ivdesc->ost->Dump();
      LogInfo::MapleLogger() << "  step: " << ivdesc->stepValue << " initExpr: ";
      ivdesc->initExpr->Dump(0);
      if (i == idxPrimaryIV) {
        LogInfo::MapleLogger() << " [PRIMARY IV]";
      }
      LogInfo::MapleLogger() << endl;
    }
  }
  ComputeTripCount();
  if (DEBUGFUNC(func) && tripCount) {
    LogInfo::MapleLogger() << "****** trip count is: ";
    tripCount->Dump(func->irMap, 0);
    LogInfo::MapleLogger() << endl;
  }
}

AnalysisResult *DoLfoIVCanon::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func, !MeOption::quiet));
  ASSERT(dom != nullptr, "dominance phase has problem");

  MeIRMap *irmap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func, !MeOption::quiet));
  ASSERT(irmap != nullptr, "hssamap has problem");

  IdentifyLoops *identloops = static_cast<IdentifyLoops *>(m->GetAnalysisResult(MeFuncPhase_IDENTLOOPS, func, !MeOption::quiet));
  CHECK_FATAL(identloops != nullptr, "identloops has problem");

  LfoFunction *lfoFunc = func->lfoFunc;

  // loop thru all the loops in reverse order so inner loops are processed first
  for (int32 i = identloops->meloops.size()-1; i >= 0; i--) {
    LoopDesc *aloop = identloops->meloops[i];
    BB *headbb = aloop->head;
    // check if the label has associated LfoWhileInfo
    if (headbb->bbLabel == 0) {
      continue;
    }
    MapleMap<LabelIdx, LfoWhileInfo*>::iterator it = lfoFunc->label2WhileInfo.find(headbb->bbLabel);
    if (it == lfoFunc->label2WhileInfo.end()) {
      continue;
    }
    LfoWhileInfo *whileInfo = it->second;
    if (whileInfo->injectedIVSym == nullptr) {
      continue;
    }
    MemPool *ivmp = mempoolctrler.NewMemPool(PhaseName().c_str());
    IVCanon ivCanon(ivmp, func, dom, aloop, whileInfo);
    ivCanon.PerformIVCanon();
    mempoolctrler.DeleteMemPool(ivmp);
  }

  return nullptr;
}

}  // namespace maple
