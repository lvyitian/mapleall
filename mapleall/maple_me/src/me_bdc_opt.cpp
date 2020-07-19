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

// do array boundary check elimination
#include "me_bdc_opt.h"
#include "me_function.h"
#include "me_option.h"
#include "me_irmap.h"

/** This phase eliminates unnecessary array boundary check
 *
 * #1 eliminate loop boundary check in the loop
 * for (int idx=array.length - 1; idx>=0; idx--)
 *   array[idx] <-
 * or
 * for (int i = 0; i < array.length; i++)
 *   array[idx] <-
 * first, we test if the loop is suitable for eliminating the array boundary check.
 * we only handle all bbs that can be jumped from nodes in the loop.
 * second, we test the index of loop is under the range of [0,array.length -1] by using SSA information
 *
 * #2 eliminate array boundary check for array[0]
 * first, we test if the index is zero
 * second, we test if array.length > 0
 *
 * #3 eliminate array boundary check for array[i], if there is condition 0 <= i < array.length
 * first, using control dependence by post dominator to look if there is condition that matches i >= 0;
 * second, using the same way to look if there is i < array.length
 *
 * #4 eliminate array boundary check for newly allocated array
 * int[] ret = new int[N + 1];
 * ret[N] = val;
 * get definition of base of array by SSA, if we find the base is defined by dassign and the right
 * hand side of the dassign is operator gcmallocjarray, then we compare the index of array against
 * gcmallocajarray's length, if less than, then the array boundary check is safe to be eliminated
 *
 */

namespace maple {

void MeBDC::CollectArrayExpr(MeStmt *mestmt, MeExpr *meexpr, std::vector<std::pair<MeStmt *, NaryMeExpr *>> &arrayvec) {
  if (meexpr->op == OP_array) {
    NaryMeExpr *arrayexpr = static_cast<NaryMeExpr *>(meexpr);
    if (arrayexpr->boundCheck) {
      arrayvec.push_back(std::pair<MeStmt *, NaryMeExpr *>(mestmt, arrayexpr));
    }
  }
  for (int32 i = 0; i < meexpr->NumMeExprOpnds(); i++) {
    CollectArrayExpr(mestmt, meexpr->GetOpnd(i), arrayvec);
  }
}

// collect all definition via iterating phi, if there is no definition of an operand from phi, return false
bool MeBDC::CollectDef(ScalarMeExpr *meexpr, std::set<MeStmt *> &defvec, std::set<MePhiNode *> &phiprocessedset) {
  if (meexpr->defBy == kDefByNo) {
    return false;
  }
  switch (meexpr->defBy) {
    case kDefByStmt: {
      defvec.insert(meexpr->def.defStmt);
      break;
    }
    case kDefByPhi: {
      MePhiNode *defPhi = meexpr->def.defPhi;
      if (phiprocessedset.find(defPhi) == phiprocessedset.end()) {
        phiprocessedset.insert(defPhi);
        for (uint32 i = 0; i < defPhi->opnds.size(); i++) {
          if (!CollectDef(defPhi->opnds[i], defvec, phiprocessedset)) {
            return false;
          }
        }
      }
      break;
    }
    default:
      return false;
  }
  return true;
}

bool MeBDC::AnalyzeInitDefFromUpper(std::set<MeStmt *> &devvec, const MeExpr *base) {
  for (std::set<MeStmt *>::iterator it = devvec.begin(); it != devvec.end(); it++) {
    MeStmt *defStmt = *it;
    if (defStmt->op != OP_dassign) {
      return false;
    }
    DassignMeStmt *dssdefstmt = static_cast<DassignMeStmt *>(defStmt);
    MeExpr *rhs = dssdefstmt->rhs;
    if (rhs->op != OP_sub) {
      return false;
    }
    OpMeExpr *oprhs = static_cast<OpMeExpr *>(rhs);
    MeExpr *opnd0 = oprhs->GetOpnd(0);
    MeExpr *opnd1 = oprhs->GetOpnd(1);

    if (!dssdefstmt->lhs->IsUseSameSymbol(opnd0)) {
      return false;
    }
    if (opnd1->op != OP_constval) {
      return false;
    }
    if (!static_cast<ConstMeExpr *>(opnd1)->IsOne()) {
      return false;
    }
    VarMeExpr *varopnd0 = static_cast<VarMeExpr *>(opnd0);
    std::set<MePhiNode *> phiprocessedset;
    std::set<MeStmt *> deepdefvec;
    if (!CollectDef(varopnd0, deepdefvec, phiprocessedset)) {
      return false;
    }
    for (std::set<MeStmt *>::iterator iit = deepdefvec.begin(); iit != deepdefvec.end(); iit++) {
      MeStmt *deepdefstmt = *iit;
      if (devvec.find(deepdefstmt) != devvec.end()) {
        continue;  // might be in the loop and recursively defined
      }
      DassignMeStmt *defvaropnd0 = static_cast<DassignMeStmt *>(deepdefstmt);
      if (defvaropnd0->rhs->op != OP_intrinsicop) {
        return false;
      }
      NaryMeExpr *intrinmeexpr = static_cast<NaryMeExpr *>(defvaropnd0->rhs);
      if (intrinmeexpr->intrinsic != INTRN_JAVA_ARRAY_LENGTH) {
        return false;
      }
      if (intrinmeexpr->GetOpnd(0) != base) {
        return false;
      }
    }
  }
  return true;
}

// test if there is a condition dependence that makes the index in the range of 0 to length-1
bool MeBDC::AnalyzeInitCD(LoopDesc *mploop, const ScalarMeExpr *meexpr, bool isfromupper, const MeExpr *arrybase) {
  BB *loophead = mploop->head;
  // get head's first control dependence bb
  BB *cdbb = (loophead->pred[0] == mploop->tail) ? loophead->pred[1] : loophead->pred[0];
  Dominance *dom = mefunc->irMap->GetDominance();
  CHECK_FATAL(dom->Dominate(cdbb, loophead), "wrong");
  while (cdbb->kind == kBBFallthru && cdbb->pred.size() == 1) {
    cdbb = cdbb->pred[0];
  }
  if (cdbb->kind != kBBCondGoto) {
    return false;
  }
  MeStmt *lastmestmt = cdbb->meStmtList.last;
  Opcode brop = lastmestmt->op;
  if (!lastmestmt->IsCondBr()) {
    return false;
  }
  bool istruebr = brop == OP_brtrue;
  CondGotoMeStmt *condgoto = static_cast<CondGotoMeStmt *>(lastmestmt);
  MeExpr *cmpexpr = condgoto->opnd;
  if (!kOpcodeInfo.IsCompare(cmpexpr->op)) {
    return false;
  }
  OpMeExpr *opcmpexpr = static_cast<OpMeExpr *>(cmpexpr);
  LabelIdx gotolbl = (LabelIdx)condgoto->offset;
  LabelIdx bblabel = loophead->bbLabel;
  if (isfromupper) {
    if (opcmpexpr->GetOpnd(0) != meexpr) {
      return false;
    }
    if (opcmpexpr->GetOpnd(1)->meOp != kMeOpConst) {
      return false;
    }
    if (!static_cast<ConstMeExpr *>(opcmpexpr->GetOpnd(1))->IsZero()) {
      return false;
    }
    // assume it compares with zero
    Opcode cmpop = opcmpexpr->op;
    switch (cmpop) {
      case OP_lt:
      case OP_ne:
        if (!((istruebr && gotolbl != bblabel) || (!istruebr && gotolbl == bblabel))) {
          return false;
        }
        break;
      case OP_ge:
      case OP_eq:
        if (!((istruebr && gotolbl == bblabel) || (!istruebr && gotolbl != bblabel))) {
          return false;
        }
        break;
      default:
        return false;
    }
  } else {
    // make sure the array.length > 0
    MeExpr *cmpop0 = (opcmpexpr->GetOpnd(0))->ResolveMeExprValue();
    MeExpr *cmpop1 = (opcmpexpr->GetOpnd(1))->ResolveMeExprValue();
    if (!cmpop0 || !cmpop1) {
      return false;
    }
    // make sure one operand is constVal another one is intrinsic JAVA_ARRAY_LENGTH
    Opcode cmopo0op = cmpop0->op;
    Opcode cmopo1op = cmpop1->op;
    if (cmopo0op == cmopo1op) {
      return false;
    }
    if ((cmopo0op != OP_constval && cmopo1op != OP_constval)) {
      return false;
    }
    if (cmopo0op != OP_intrinsicop && cmopo1op != OP_intrinsicop) {
      return false;
    }
    // now one is intrinsic another one is constVal
    bool opndswaped = false;
    if (cmopo0op != OP_constval) {
      opndswaped = true;
      MeExpr *tmp = cmpop0;
      cmpop0 = cmpop1;
      cmpop1 = tmp;
    }
    ConstMeExpr *constmeexpr = static_cast<ConstMeExpr *>(cmpop0);
    if (!constmeexpr->IsZero()) {
      return false;
    }
    NaryMeExpr *intrinmeexpr = static_cast<NaryMeExpr *>(cmpop1);
    if (intrinmeexpr->intrinsic != INTRN_JAVA_ARRAY_LENGTH) {
      return false;
    }
    if (intrinmeexpr->GetOpnd(0) != arrybase) {
      return false;
    }
    Opcode cmpop = opcmpexpr->op;
    switch (cmpop) {
      case OP_lt:
        if (!((istruebr && gotolbl == bblabel && !opndswaped) || (!istruebr && gotolbl != bblabel && !opndswaped))) {
          return false;
        }
        break;
      case OP_ge:
        if (!((istruebr && gotolbl != bblabel && !opndswaped) || (!istruebr && gotolbl == bblabel && !opndswaped))) {
          return false;
        }
        break;
      case OP_le:
        if (!((istruebr && gotolbl != bblabel && opndswaped) || (!istruebr && gotolbl == bblabel && opndswaped))) {
          return false;
        }
      case OP_gt:
      default:
        return false;
    }
  }
  return true;
}

bool MeBDC::OperandGE0(MeExpr *opnd, std::set<MePhiNode *> &phiset) {
  if (opnd->op == OP_constval && static_cast<ConstMeExpr *>(opnd)->GeZero()) {
    return true;
  }
  if (opnd->meOp == kMeOpVar) {
    VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(opnd);
    switch (varmeexpr->defBy) {
      case kDefByStmt: {
        MeStmt *defStmt = varmeexpr->def.defStmt;
        if (defStmt->op != OP_dassign) {
          return false;
        }
        return OperandGE0(static_cast<DassignMeStmt *>(defStmt)->rhs, phiset);
      }
      case kDefByPhi: {
        MePhiNode *phinode = varmeexpr->def.defPhi;
        if (phiset.find(phinode) != phiset.end()) {
          return true;
        }
        phiset.insert(phinode);
        for (uint32 i = 0; i < phinode->opnds.size(); i++) {
          if (!OperandGE0(phinode->opnds[i], phiset)) {
            return false;
          }
        }
        return true;
      }
      default:
        return false;
    }
  }
  return false;
}

// this function check the pattern of induction variable
// is like i = i + C or i = i - C
bool MeBDC::IndexIsInductionVar(LoopDesc *meloop, ScalarMeExpr *lhs, MeExpr *&rhs, bool isfromupper) {
  while (rhs->meOp == kMeOpVar) {
    VarMeExpr *varrhs = static_cast<VarMeExpr *>(rhs);
    if (varrhs->defBy != kDefByStmt) {
      return false;
    }
    MeStmt *defStmt = varrhs->def.defStmt;
    if (!meloop->Has(defStmt->bb)) {
      return false;
    }
    if (defStmt->op != OP_dassign) {
      return false;
    }
    rhs = static_cast<DassignMeStmt *>(defStmt)->rhs;
  }
  if (rhs->op != OP_sub && rhs->op != OP_add) {
    return false;
  }
  OpMeExpr *opmeexpr = static_cast<OpMeExpr *>(rhs);
  MeExpr *opnd0 = opmeexpr->GetOpnd(0);
  MeExpr *opnd1 = opmeexpr->GetOpnd(1);
  std::set<MePhiNode *> phiset;
  if (!OperandGE0(opnd1, phiset)) {
    return false;
  }

  if (isfromupper) {
    if (rhs->op != OP_sub) {
      return false;
    }
  } else {
    if (rhs->op != OP_add) {
      return false;
    }
  }
  if (lhs->IsUseSameSymbol(opnd0)) {
    return true;
  } else {
    if (opnd0->meOp != kMeOpVar) {
      return false;
    }
    VarMeExpr *varopnd0 = static_cast<VarMeExpr *>(opnd0);
    if (varopnd0->defBy != kDefByStmt) {
      return false;
    }
    MeStmt *defStmt = varopnd0->def.defStmt;
    if (!meloop->Has(defStmt->bb)) {
      return false;
    }
    if (defStmt->op != OP_dassign) {
      return false;
    }
    DassignMeStmt *dassdefstmt = static_cast<DassignMeStmt *>(defStmt);
    MeExpr *dassdefstmtrhs = dassdefstmt->rhs;
    if (lhs->IsUseSameSymbol(dassdefstmtrhs)) {
      return true;
    }
    return IndexIsInductionVar(meloop, lhs, dassdefstmtrhs, isfromupper);
  }
}

bool MeBDC::AnalyzeStep(LoopDesc *meloop, const std::set<MeStmt *> &defvec, bool isfromupper) {
  // when the index is sign-extended to a shorter value and casted back. it's not safe
  // FOR example: for (int i = 0; i < a.length; ) {
  //  a[i] = i;
  //  i = (byte) (i + 1);
  // }
  // after i = (byte) (i + 1); i might become a -1343434343 which needs array boundary check
  for (std::set<MeStmt *>::iterator it = defvec.begin(); it != defvec.end(); it++) {
    DassignMeStmt *defmestmt = static_cast<DassignMeStmt *>(*it);
    MeExpr *rhs = defmestmt->rhs;
    ScalarMeExpr *lhs = defmestmt->GetVarLhs();
    if (!IndexIsInductionVar(meloop, lhs, rhs, isfromupper)) {
      return false;
    }
  }
  return true;
}

bool MeBDC::AnalyzeOneExit(LoopDesc *mploop, BB *cdbb, ScalarMeExpr *meexpr, bool isfromupper, const MeExpr *arrybase) {
  MeStmt *lastmestmt = cdbb->meStmtList.last;
  if (!lastmestmt) {
    return false;
  }
  Opcode brop = lastmestmt->op;
  if (!lastmestmt->IsCondBr()) {
    return false;
  }
  bool istruebr = brop == OP_brtrue;
  CondGotoMeStmt *condgoto = static_cast<CondGotoMeStmt *>(lastmestmt);
  MeExpr *cmpexpr = condgoto->opnd;
  if (!kOpcodeInfo.IsCompare(cmpexpr->op)) {
    return false;
  }
  OpMeExpr *opcmpexpr = static_cast<OpMeExpr *>(cmpexpr);
  LabelIdx gotolbl = (LabelIdx)condgoto->offset;
  BB *inloopbb = cdbb->succ[0];
  BB *anotherbb = cdbb->succ[1];
  if (mploop->Has(inloopbb)) {
    if (mploop->Has(anotherbb)) {
      // dead loop?
      return false;
    }
  } else {
    CHECK_FATAL(mploop->Has(cdbb->succ[1]), "wrong?");
    anotherbb = inloopbb;
    inloopbb = cdbb->succ[1];
  }
  LabelIdx bblabel = inloopbb->bbLabel;
  if (!meexpr->IsUseSameSymbol(opcmpexpr->GetOpnd(0))) {
    return false;
  }
  // make sure the definition of condition op0 is from inside of the loop, otherwise it's not safe
  VarMeExpr *varcondop0 = static_cast<VarMeExpr *>(opcmpexpr->GetOpnd(0));
  if (varcondop0->defBy != kDefByStmt) {
    return false;
  }
  if (!mploop->Has(varcondop0->def.defStmt->bb)) {
    return false;
  }
  Opcode cmpop = opcmpexpr->op;
  if (isfromupper) {
    if (opcmpexpr->GetOpnd(1)->meOp != kMeOpConst) {
      return false;
    }
    if (!static_cast<ConstMeExpr *>(opcmpexpr->GetOpnd(1))->IsZero()) {
      return false;
    }
    if (bblabel != 0) {
      // assume it compares with zero
      switch (cmpop) {
        case OP_lt:
        case OP_ne:
          if (!((istruebr && gotolbl != bblabel) || (!istruebr && gotolbl == bblabel))) {
            return false;
          }
          break;
        case OP_ge:
        case OP_eq:
          if (!((istruebr && gotolbl == bblabel) || (!istruebr && gotolbl != bblabel))) {
            return false;
          }
          break;
        default:
          return false;
      }
    } else {
      LabelIdx otherlbl = anotherbb->bbLabel;
      CHECK_FATAL(otherlbl != 0, "how come jump to nothing?");
      switch (cmpop) {
        case OP_lt:
        case OP_ne: {
          if (!((istruebr && gotolbl == otherlbl) || (!istruebr && gotolbl != otherlbl))) {
            return false;
          }
          break;
        }
        case OP_ge:
        case OP_eq:
          if (!((istruebr && gotolbl != otherlbl) || (!istruebr && gotolbl == otherlbl))) {
            return false;
          }
          break;
        default:
          return false;
      }
    }
  } else {
    MeExpr *cmpop1 = (opcmpexpr->GetOpnd(1))->ResolveMeExprValue();
    if (!cmpop1 || cmpop1->op != OP_intrinsicop) {
      return false;
    }
    NaryMeExpr *intrncmpop1 = static_cast<NaryMeExpr *>(cmpop1);
    if (intrncmpop1->intrinsic != INTRN_JAVA_ARRAY_LENGTH && arrybase != cmpop1) {
      return false;
    }
    if (bblabel != 0) {
      // assume it compares with zero
      switch (cmpop) {
        case OP_lt:
          if (!((istruebr && gotolbl == bblabel) || (!istruebr && gotolbl != bblabel))) {
            return false;
          }
          break;
        case OP_ge:
          if (!((istruebr && gotolbl != bblabel) || (!istruebr && gotolbl == bblabel))) {
            return false;
          }
          break;
        default:
          return false;
      }
    } else {
      LabelIdx otherlbl = anotherbb->bbLabel;
      CHECK_FATAL(otherlbl != 0, "how come jump to nothing?");
      switch (cmpop) {
        case OP_lt: {
          if (!((istruebr && gotolbl != otherlbl) || (!istruebr && gotolbl == otherlbl))) {
            return false;
          }
          break;
        }
        case OP_ge:
          if (!((istruebr && gotolbl == otherlbl) || (!istruebr && gotolbl != otherlbl))) {
            return false;
          }
          break;
        default:
          return false;
      }
    }
  }
  return true;
}

bool MeBDC::AnalyzeExitCondGoto(LoopDesc *mploop, ScalarMeExpr *meexpr, const BB *bb, bool isfromupper,
                                const MeExpr *base) {
  Dominance *dom = mefunc->irMap->GetDominance();
  CHECK_FATAL(bb->id.idx < dom->pdomFrontier.size(), "out of range in MeBDC::AnalyzeExitCondGoto");
  MapleSet<BBId> *pdomfrt = &dom->pdomFrontier[bb->id.idx];
  for (MapleSet<BBId>::iterator it = pdomfrt->begin(); it != pdomfrt->end(); it++) {
    BB *cdbb = mefunc->bbVec[it->idx];
    if (!mploop->Has(cdbb)) {
      continue;
    }
    if (AnalyzeOneExit(mploop, cdbb, meexpr, isfromupper, base)) {
      return true;
    }
  }
  return false;
}

void MeBDC::DoBdcForArray(LoopDesc *mploop, const MeStmt *mestmt, NaryMeExpr *arrexpr) {
  if (arrexpr->NumMeExprOpnds() != 2) {
    return;
  }
  if (!arrexpr->boundCheck) {
    return;
  }
  MeExpr *base = arrexpr->GetOpnd(0);
  MeExpr *index = arrexpr->GetOpnd(1);
  if (index->op == OP_dread || index->op == OP_regread) {
    ScalarMeExpr *idxvar = static_cast<ScalarMeExpr*>(index);
    if (idxvar->defBy == kDefByPhi) {
      MePhiNode *phinode = idxvar->def.defPhi;
      MapleVector<ScalarMeExpr *> &phiopnds = phinode->opnds;
      if (phiopnds.size() == 2) {
        // for the definition of each phi opnd,
        // make sure one candidate is from outside of the loop, the other one is inside the loop
        ScalarMeExpr *defopnd0 = phiopnds[0];
        ScalarMeExpr *defopnd1 = phiopnds[1];
        std::set<MeStmt *> defvec0;
        std::set<MePhiNode *> processedphiset0;
        std::set<MeStmt *> defvec1;
        std::set<MePhiNode *> processedphiset1;
        if (!CollectDef(defopnd0, defvec0, processedphiset0) || !CollectDef(defopnd1, defvec1, processedphiset1)) {
          return;
        }
        bool isdefoutsideloop0 = true;
        for (std::set<MeStmt *>::iterator setit = defvec0.begin(); setit != defvec0.end(); setit++) {
          if (mploop->Has((*setit)->bb)) {
            isdefoutsideloop0 = false;
          }
        }
        if (defvec1.size() != 1) {
          return;
        }
        MeStmt *defstmt1 = *(defvec1.begin());
        if (isdefoutsideloop0 && (mploop->Has(defstmt1->bb))) {
          // get the init value
          bool isfromupper = AnalyzeInitDefFromUpper(defvec0, base);
          if (!AnalyzeInitCD(mploop, defopnd0, isfromupper, base)) {
            // analyze the control dependence to make sure the init value is between 1 to length -1
            return;
          }
          if (!AnalyzeStep(mploop, defvec1, isfromupper)) {
            return;
          }
          if (!AnalyzeExitCondGoto(mploop, defopnd0, mestmt->bb, isfromupper, base)) {
            return;
          }
          // now we are sure the index is between 1 to length-1, get rid of the boundary check
          arrexpr->boundCheck = false;
          if (is_debug) {
            LogInfo::MapleLogger() << "function: " << mefunc->GetName() << std::endl;
            LogInfo::MapleLogger() << "there is a boundary check that was eliminated in loop with head: " << mploop->head->id.idx
                      << "mx id" << arrexpr->exprID << std::endl;
          }
        }
      }
    }
  }
}

void MeBDC::DoBdcForLoop(LoopDesc *mploop) {
  MapleSet<BBId> &loopbbs = mploop->loop_bbs;
  std::vector<std::pair<MeStmt *, NaryMeExpr *>> arrayvec;
  for (MapleSet<BBId>::iterator stit = loopbbs.begin(); stit != loopbbs.end(); stit++) {
    BB *bb = mefunc->bbVec[stit->idx];
    for (auto mestmt : bb->meStmtList) {
      for (int32 iii = 0; iii < mestmt->NumMeStmtOpnds(); iii++) {
        MeExpr *meexpr = mestmt->GetMeStmtOpnd(iii);
        CHECK_FATAL(meexpr != nullptr, "meexpr is null in MeBDC::DoBdcForLoop");
        CollectArrayExpr(mestmt, meexpr, arrayvec);
      }
    }
  }
  for (uint32 i = 0; i < arrayvec.size(); i++) {
    DoBdcForArray(mploop, arrayvec[i].first, arrayvec[i].second);
  }
}

// #2 eliminate array boundary check for array[0]
// if we match this kind of array, then we eliminate the array boundary check
// otherwise we return false
bool MeBDC::ArrayWith0AsIndexOpnd(BB *bb, NaryMeExpr *arrayexpr, BB *condgotobb) {
  Dominance *dom = mefunc->irMap->GetDominance();
  if (!dom->Dominate(condgotobb, bb)) {
    return false;
  }
  CHECK_FATAL(arrayexpr->numOpnds > 1, "out of range in MeBDC::ArrayWith0AsIndexOpnd");
  MeExpr *index = (arrayexpr->GetOpnd(1))->ResolveMeExprValue();
  if (index && index->op == OP_constval && static_cast<ConstMeExpr *>(index)->IsZero()) {
    // need to know if the base.length > 0;
    MeStmt *lastmestmt = condgotobb->meStmtList.last;
    if (!lastmestmt || !lastmestmt->IsCondBr()) {
      return false;
    }
    bool istruebr = lastmestmt->op == OP_brtrue;
    CondGotoMeStmt *condgotostmt = static_cast<CondGotoMeStmt *>(lastmestmt);
    if (!kOpcodeInfo.IsCompare(condgotostmt->opnd->op)) {
      return false;
    }
    OpMeExpr *cmpop = static_cast<OpMeExpr *>(condgotostmt->opnd);
    MeExpr *cmpop0 = cmpop->GetOpnd(0);
    MeExpr *cmpop1 = cmpop->GetOpnd(1);
    if (cmpop0->op != OP_constval && cmpop1->op != OP_constval) {
      return false;
    }
    bool opndswaped = false;
    if (cmpop0->op == OP_constval) {
      opndswaped = true;
      MeExpr *tmp = cmpop0;
      cmpop0 = cmpop1;
      cmpop1 = tmp;
    }
    if (!static_cast<ConstMeExpr *>(cmpop1)->IsZero()) {
      return false;
    }
    // now if we can make sure the array length > 0, we can get rid of the stupid array boundary check
    if (!IsCmp2ArrayLength(arrayexpr->GetOpnd(0), cmpop0)) {
      return false;
    }
    // now check if the jump matches the target
    // find the closest bb to condgotobb
    BB *sucdombb = nullptr;
    for (uint32 i = 0; i < condgotobb->succ.size(); i++) {
      BB *sucbb = condgotobb->succ[i];
      if (dom->Dominate(sucbb, bb)) {
        sucdombb = sucbb;
        break;
      }
    }
    if (!sucdombb) {
      return false;
    }
    bool isjumptobb = sucdombb->bbLabel == condgotostmt->offset;
    switch (cmpop->op) {
      case OP_le: {
        if (!((istruebr && !isjumptobb && !opndswaped) || (!istruebr && isjumptobb && !opndswaped))) {
          return false;
        }
        break;
      }
      case OP_gt: {
        if (!((istruebr && isjumptobb && !opndswaped) || (!istruebr && !isjumptobb && !opndswaped))) {
          return false;
        }
        break;
      }
      case OP_lt: {
        if (!((istruebr && isjumptobb && opndswaped) || (!istruebr && !isjumptobb && opndswaped))) {
          return false;
        }
        break;
      }
      case OP_ge: {
        if (!((istruebr && !isjumptobb && opndswaped) || (!istruebr && isjumptobb && opndswaped))) {
          return false;
        }
        break;
      }
      default:
        return false;
    }
    // there we go, it's safe to emliminate the array boundary check now
    arrayexpr->boundCheck = false;
    if (is_debug) {
      LogInfo::MapleLogger() << "there is a boundary check that was eliminated with index == 0 and array.length > 0: " << std::endl;
    }
    return true;
  }
  return false;
}

bool MeBDC::IsCmp2ArrayLength(const MeExpr *base, MeExpr *opnd) {
  MeExpr *valueopnd = (opnd)->ResolveMeExprValue();
  if (!valueopnd || valueopnd->op != OP_intrinsicop) {
    return false;
  }
  NaryMeExpr *intrinmeexpr = static_cast<NaryMeExpr *>(valueopnd);
  if (intrinmeexpr->intrinsic != INTRN_JAVA_ARRAY_LENGTH) {
    return false;
  }
  MeExpr *realbase = intrinmeexpr->GetOpnd(0);
  if (realbase->meOp != kMeOpVar) {
    return false;
  }
  VarMeExpr *varrealbase = (static_cast<VarMeExpr *>(realbase))->ResolveVarMeValue();
  if (varrealbase->exprID != base->exprID) {
    return false;
  }
  return true;
}

bool MeBDC::FindCondArrayLength(bool istruebr, bool istargetjump, const VarMeExpr *varopnd, const MeExpr *base,
                                OpMeExpr *cmpopnd) {
  // try to find varopnd < arr.length
  MeExpr *opnd0 = cmpopnd->GetOpnd(0);
  MeExpr *opnd1 = cmpopnd->GetOpnd(1);
  if (opnd0 != varopnd && opnd1 != varopnd) {
    return false;
  }
  bool isswapped = false;
  if (opnd0 != varopnd) {
    MeExpr *tmp = opnd0;
    opnd0 = opnd1;
    opnd1 = tmp;
    isswapped = true;
  }
  if (!IsCmp2ArrayLength(base, opnd1)) {
    return false;
  }
  switch (cmpopnd->op) {
    case OP_lt: {
      if (isswapped) {
        return false;
      }
      if (!((istruebr && istargetjump) || (!istruebr && !istargetjump))) {
        return false;
      }
      break;
    }
    case OP_ge: {
      if (isswapped) {
        return false;
      }
      if (!((istruebr && !istargetjump) || (!istruebr && istargetjump))) {
        return false;
      }
      break;
    }
    case OP_le: {
      if (!isswapped) {
        return false;
      }
      if (!((istruebr && !istargetjump) || (!istruebr && istargetjump))) {
        return false;
      }
      break;
    }
    case OP_gt: {
      if (!isswapped) {
        return false;
      }
      if (!(istruebr && istargetjump)) {
        return false;
      }
    }
    default:
      return false;
  }
  return true;
}

// return true, if condge0var >=0 to makes the meexpr without throwing exception
bool MeBDC::FoundCondMeExpr(MeExpr *meexpr, std::set<MeExpr *> &condge0varset) {
  if (meexpr->op == OP_iread) {
    MeExpr *opnd = static_cast<IvarMeExpr *>(meexpr)->base;
    if (opnd->op == OP_array) {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(opnd);
      condge0varset.insert(narymeexpr->GetOpnd(1));
      return true;
    }
  }
  return false;
}

bool MeBDC::ExpectValue(MeStmt *mestmt, const MeExpr *opnd0, int64 value) {
  // expect the value of opnd0 is bewteen (-value, value), then return false;
  BB *mestmtbb = mestmt->bb;
  // find it's previous bb to see if there are some op that may exception
  if (mestmtbb->pred.size() != 1) {
    return false;
  }
  BB *prevbb = mestmtbb->pred[0];
  std::set<MeExpr *> condge0varset;
  for (MeStmt *itstmt = prevbb->meStmtList.last; itstmt; itstmt = itstmt->prev) {
    for (int32 i = 0; i < itstmt->NumMeStmtOpnds(); i++) {
      MeExpr *opnd = itstmt->GetMeStmtOpnd(i);
      FoundCondMeExpr(opnd, condge0varset);
    }
  }
  if (condge0varset.size() == 0) {
    return false;
  }
  for (std::set<MeExpr *>::iterator it = condge0varset.begin(); it != condge0varset.end(); it++) {
    MeExpr *condmeexpr = *it;
    if (condmeexpr->meOp != kMeOpVar) {
      continue;
    }
    VarMeExpr *varcondmeexpr = static_cast<VarMeExpr *>(condmeexpr);
    if (varcondmeexpr->defBy == kDefByChi) {
      ChiMeNode *chimenode = varcondmeexpr->def.defChi;
      MeStmt *chibasestmt = chimenode->base;
      if (chibasestmt->op != OP_maydassign) {
        continue;
      }
      MaydassignMeStmt *dssbasestmt = static_cast<MaydassignMeStmt *>(chibasestmt);
      MeExpr *baserhs = dssbasestmt->rhs;
      if (baserhs->op != OP_div) {
        continue;
      }
      OpMeExpr *opbaserhs = static_cast<OpMeExpr *>(baserhs);
      MeExpr *divopnd = opbaserhs->GetOpnd(1);
      if (divopnd->meOp != kMeOpConst) {
        continue;
      }
      ConstMeExpr *constdivopnd = static_cast<ConstMeExpr *>(divopnd);
      if (static_cast<MIRIntConst *>(constdivopnd->constVal)->value == value && opnd0 == opbaserhs->GetOpnd(0)) {
        return true;
      }
    }
  }
  return false;
}

bool MeBDC::IsOperandGE0(MeExpr *opnd) {
  if (opnd->meOp != kMeOpVar) {
    return false;
  }
  VarMeExpr *varmeexpr = (static_cast<VarMeExpr *>(opnd))->ResolveVarMeValue();
  if (varmeexpr->defBy == kDefByChi) {
    ChiMeNode *chidef = varmeexpr->def.defChi;
    MeStmt *mestmt = chidef->base;
    if (mestmt->op != OP_maydassign) {
      return false;
    }
    MeExpr *defrhs = static_cast<MaydassignMeStmt *>(mestmt)->rhs;
    if (defrhs->op != OP_rem) {
      return false;
    }
    OpMeExpr *remrhs = static_cast<OpMeExpr *>(defrhs);
    if (remrhs->GetOpnd(1)->op != OP_constval) {
      return false;
    }
    ConstMeExpr *constopnd = static_cast<ConstMeExpr *>(remrhs->GetOpnd(1));
    MIRIntConst *intconst = static_cast<MIRIntConst *>(constopnd->constVal);
    int64 value = intconst->value;
    if (value == 0) {
      return false;
    }
    if (!ExpectValue(mestmt, remrhs->GetOpnd(0), value)) {
      return false;
    }
    return true;
  } else {
    return false;
  }
}

bool MeBDC::FindIndexGE0(bool istruebr, bool istargetjump, const VarMeExpr *varopnd, MeExpr *base, OpMeExpr *cmpopnd) {
  // try to find varopnd >= 0;
  MeExpr *opnd0 = cmpopnd->GetOpnd(0);
  MeExpr *opnd1 = cmpopnd->GetOpnd(1);
  if (opnd0 != varopnd && opnd1 != varopnd) {
    return false;
  }
  bool isswapped = false;
  if (opnd0 != varopnd) {
    MeExpr *tmp = opnd0;
    opnd0 = opnd1;
    opnd1 = tmp;
    isswapped = true;
  }
  std::set<MePhiNode *> phiset;
  bool isopnd1ge0 = OperandGE0(opnd0, phiset);
  if (!isopnd1ge0) {
    if (IsOperandGE0(opnd0)) {
      // issue #1862
      return true;
    }
  }
  if (!isopnd1ge0) {
    return false;
  }
  switch (cmpopnd->op) {
    case OP_ge: {
      if (!((!isswapped && istruebr && istargetjump) || (!isswapped && !istruebr && !istargetjump))) {
        return false;
      }
      break;
    }
    case OP_eq: {
      if (!(istruebr && istargetjump)) {
        return false;
      }
      break;
    }
    case OP_ne: {
      if (!(istruebr && !istargetjump)) {
        return false;
      }
      break;
    }
    case OP_lt: {
      if (!((isswapped && istruebr && istargetjump) || (isswapped && !istruebr && !istargetjump))) {
        return false;
      }
      break;
    }
    default:
      return false;
  }
  return true;
}

// #3 eliminate array boundary check for array[i], if there is condition 0 <= i < array.length`
bool MeBDC::AnalyzeABDC4VariableIndexOneCondBB(BB *bb, NaryMeExpr *arrayexpr, BB *condgotobb, bool &findupbound,
                                               bool &findlowbound) {
  Dominance *dom = mefunc->irMap->GetDominance();
  if (!dom->Dominate(condgotobb, bb)) {
    return false;
  }
  MeStmt *laststmt = condgotobb->meStmtList.last;
  if (!laststmt) {
    return false;
  }
  if (!laststmt->IsCondBr()) {
    return false;
  }
  CondGotoMeStmt *condbrmestmt = static_cast<CondGotoMeStmt *>(laststmt);
  MeExpr *opnd = condbrmestmt->opnd;
  if (!kOpcodeInfo.IsCompare(opnd->op)) {
    return false;
  }
  OpMeExpr *cmpopnd = static_cast<OpMeExpr *>(opnd);
  MeExpr *base = arrayexpr->GetOpnd(0);
  if (base->meOp != kMeOpVar) {
    return false;
  }
  VarMeExpr *varbase = (static_cast<VarMeExpr *>(base))->ResolveVarMeValue();
  VarMeExpr *varopnd = static_cast<VarMeExpr *>(arrayexpr->GetOpnd(1));
  bool istargetjump = condbrmestmt->offset == bb->bbLabel;
  bool istruebr = (condbrmestmt->op == OP_brtrue);
  if (!findupbound) {
    findupbound = FindCondArrayLength(istruebr, istargetjump, varopnd, varbase, cmpopnd);
  }
  if (!findlowbound) {
    findlowbound = FindIndexGE0(istruebr, istargetjump, varopnd, varbase, cmpopnd);
  }
  return findupbound && findlowbound;
}

// #2 eliminate array boundary check for array[0]
// #3 eliminate array boundary check for array[i], if there is condition 0 <= i < array.length
bool MeBDC::AnalyzeABDCByCondArrayExpr(BB *bb, NaryMeExpr *arrayexpr) {
  CHECK_FATAL(arrayexpr->numOpnds > 1, "out of range in MeBDC::AnalyzeABDCByCondArrayExpr");
  MeExpr *opnd1 = arrayexpr->GetOpnd(1);
  Dominance *dom = mefunc->irMap->GetDominance();
  if (opnd1->meOp != kMeOpVar) {
    return false;
  }
  bool findupbound = false;
  bool findlowbound = false;
  bool findarraybdccond = false;
  CHECK_FATAL(bb->id.idx < dom->pdomFrontier.size(), "out of range in MeBDC::AnalyzeABDCByCondArrayExpr");
  MapleSet<BBId> *pdomfrt = &dom->pdomFrontier[bb->id.idx];
  uint32 sizeofbb = dom->bbVec.size();
  std::vector<bool> visitedmap(sizeofbb, false);
  while (pdomfrt->size() == 1) {
    BB *cdbb = mefunc->bbVec[(pdomfrt->begin())->idx];
    if (visitedmap[cdbb->id.idx]) {
      break;
    }
    visitedmap[cdbb->id.idx] = true;
    if (ArrayWith0AsIndexOpnd(bb, arrayexpr, cdbb)) {
      // #2 eliminate array boundary check for array[0]
      // if the array boundary check was eliminated by this step, then we don't need to do #3
      findarraybdccond = true;
      break;
    }
    // do #3 eliminate array boundary check for array[i], if there is condition 0 <= i < array.length
    if (AnalyzeABDC4VariableIndexOneCondBB(bb, arrayexpr, cdbb, findupbound, findlowbound)) {
      break;
    }

    pdomfrt = &dom->pdomFrontier[cdbb->id.idx];
  }
  return findarraybdccond || (findupbound && findlowbound);
}

bool MeBDC::IndexLtJarrayLength(NaryMeExpr *arraymeexpr, MeExpr *indexexpr, MeExpr *jarraylenexpr) {
  MeExpr *opnd0 = (indexexpr)->ResolveMeExprValue();
  MeExpr *opnd1 = (jarraylenexpr)->ResolveMeExprValue();
  if (!opnd0 || !opnd1) {
    return false;
  }
  if (opnd0->meOp == kMeOpConst && opnd1->meOp == kMeOpConst) {
    ConstMeExpr *constopnd0 = static_cast<ConstMeExpr *>(opnd0);
    ConstMeExpr *constopnd1 = static_cast<ConstMeExpr *>(opnd1);
    MIRIntConst *intconst0 = dynamic_cast<MIRIntConst *>(constopnd0->constVal);
    MIRIntConst *intconst1 = dynamic_cast<MIRIntConst *>(constopnd1->constVal);
    if (!intconst0 || !intconst1) {
      return false;
    }
    if (intconst0->value < intconst1->value) {
      return true;
    }
  }
  if (opnd1->op == OP_add) {
    // array length = a + b, when a is index and b > 0, then it's ok for array boundary check
    OpMeExpr *opmeexpr = static_cast<OpMeExpr *>(opnd1);
    MeExpr *addopnd0 = opmeexpr->GetOpnd(0);
    MeExpr *addopnd1 = opmeexpr->GetOpnd(1);
    if (addopnd1->meOp == kMeOpConst && static_cast<ConstMeExpr *>(addopnd1)->GtZero() &&
        addopnd0->exprID == indexexpr->exprID) {
      return true;
    }
  }
  return false;
}

// #4 eliminate array boundary check for newly allocated array
void MeBDC::AnalyzeNewlyOneABDC(BB *bb, NaryMeExpr *meexpr) {
  MeExpr *base = meexpr->GetOpnd(0);
  MeExpr *index = meexpr->GetOpnd(1);
  if (base->meOp != kMeOpVar) {
    return;
  }
  VarMeExpr *varbase = static_cast<VarMeExpr *>(base);
  if (varbase->defBy != kDefByStmt) {
    return;
  }
  MeStmt *defStmt = varbase->def.defStmt;
  if (defStmt->op != OP_dassign) {
    return;
  }
  DassignMeStmt *dssdefstmt = static_cast<DassignMeStmt *>(defStmt);
  if (dssdefstmt->rhs->op != OP_gcmallocjarray) {
    return;
  }
  OpMeExpr *gcnode = static_cast<OpMeExpr *>(dssdefstmt->rhs);
  MeExpr *jarraylenexpr = gcnode->GetOpnd(0);
  if (IndexLtJarrayLength(meexpr, index, jarraylenexpr)) {
    meexpr->boundCheck = false;
    if (is_debug) {
      LogInfo::MapleLogger() << "there is a newly allocated array boundary check that was eliminated\n";
    }
  }
}

void MeBDC::BDCCollectArrayExpr(MeStmt *mestmt, MeExpr *meexpr,
                                std::vector<std::pair<MeStmt *, NaryMeExpr *>> &arrayvec) {
  if (meexpr->op == OP_array) {
    NaryMeExpr *nmeexpr = static_cast<NaryMeExpr *>(meexpr);
    if (nmeexpr->boundCheck) {
      arrayvec.push_back(std::pair<MeStmt *, NaryMeExpr *>(mestmt, nmeexpr));
    }
  }
  for (int32 i = 0; i < meexpr->NumMeExprOpnds(); i++) {
    BDCCollectArrayExpr(mestmt, meexpr->GetOpnd(i), arrayvec);
  }
}

void MeBDC::DoIt() {
  // do loop array boundary check elimination for the index that between 0 to length -1
  for (uint32 i = 0; i < meloop->meloops.size(); i++) {
    // do #1 eliminate loop boundary check in the loop
    DoBdcForLoop(meloop->meloops[i]);
  }
  // do normal control depenence array boundary check
  // do #2 - #4
  // first collect all the array expr
  std::vector<std::pair<MeStmt *, NaryMeExpr *>> arrayvec;
  for (BB *bb : mefunc->bbVec) {
    if (!bb) {
      continue;
    }
    for (auto mestmt : bb->meStmtList) {
      for (int32 iii = 0; iii < mestmt->NumMeStmtOpnds(); iii++) {
        BDCCollectArrayExpr(mestmt, mestmt->GetMeStmtOpnd(iii), arrayvec);
      }
    }
  }
  for (uint32 i = 0; i < arrayvec.size(); i++) {
    std::pair<MeStmt *, NaryMeExpr *> pairarray = arrayvec[i];
    NaryMeExpr *arrayexpr = pairarray.second;
    BB *bb = pairarray.first->bb;
    if (arrayexpr->boundCheck) {
      // try #2 and #3 to see if the array boundary check could be removed
      if (!AnalyzeABDCByCondArrayExpr(bb, arrayexpr)) {
        // if #2 and #3 can't eliminate the array boundary check, then
        // do #4 eliminate array boundary check for newly allocated array
        AnalyzeNewlyOneABDC(bb, arrayexpr);
      }
    }
  }
  return;
}

AnalysisResult *MeDoBDCOpt::Run(MeFunction *func, MeFuncResultMgr *frm, ModuleResultMgr *mrm) {
  if (func->secondPass) {
    return nullptr;
  }
  Dominance *dom = static_cast<Dominance *>(frm->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
  ASSERT(dom, "dominance phase has problem");
  MeIRMap *hmap = static_cast<MeIRMap *>(frm->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func));
  ASSERT(hmap, "hssamap has problem");
  IdentifyLoops *meloop = static_cast<IdentifyLoops *>(frm->GetAnalysisResult(MeFuncPhase_MELOOP, func));
  CHECK_FATAL(meloop, "meloop has problem");
  MemPool *dobdcoptmp = mempoolctrler.NewMemPool(PhaseName().c_str());
  MeBDC *mebdc = dobdcoptmp->New<MeBDC>(func, meloop);

  MIRFunction *mirfunction = func->mirFunc;
  if (DEBUGFUNC(func)) {
    mebdc->is_debug = true;
  }

  mebdc->DoIt();
  /* this is a transform phase, delete mempool */
  mempoolctrler.DeleteMemPool(dobdcoptmp);

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============== After boundary check optimization  =============" << std::endl;
    hmap->Dump();
  }
  return nullptr;
}

}  // namespace maple
