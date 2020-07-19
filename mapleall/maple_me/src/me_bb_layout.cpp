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
#include "me_bb_layout.h"
#include "me_cfg.h"
#include "bb.h"
#include "me_irmap.h"
#include "me_option.h"

// This BB layout strategy strictly obeys source ordering when inside try blocks.
// This Optimization will reorder the bb layout. it start from the first bb of func.
// All bbs will be put into layoutBBs and it gives the determined layout order.
// The entry of the optimization is MeDoBBLayout::Run. It starts from the first bb.
// 1. If curbb is condtion goto or goto kind, do OptimizeBranchTarget for bb.
// 2. Find curbb's next bb nextbb, and based on nextbb do the following:
// 3. (1) For fallthru/catch/finally, fix curbb's fallthru
//    (2) For condtion goto curbb:
//        i) If the target bb can be moved, then put it as currbb's next
//        and retarget curbb to it's fallthru bb. add targetbb as next.
//        ii) If curbb's fallthru is not its next bb add fallthru as its next if
//            fallthru can be moved else create a new fallthru contains a goto to
//            the original fallthru
//    (3) For goto curbb see if goto target can be placed as next.
// 5. do step 3 for nextbb until all bbs are laid out

using namespace std;

namespace maple {
static void CreateGoto(BB *bb, MeFunction *func, BB *fallthru) {
  if (fallthru->bbLabel == 0) {
    func->CreateBBLabel(fallthru);
  }
  if (func->irMap != nullptr) {
    GotoNode stmt(OP_goto);
    GotoMeStmt *newgoto = func->irMap->New<GotoMeStmt>(&stmt);
    newgoto->offset = fallthru->bbLabel;
    bb->AddMeStmtLast(newgoto);
  } else {
    GotoNode *newgoto = func->mirFunc->codeMemPool->New<GotoNode>(OP_goto);
    newgoto->offset = fallthru->bbLabel;
    bb->AddStmtNode(newgoto);
  }
  bb->kind = kBBGoto;
}

// return true if bb is empty and its kind is fallthru.
bool BBLayout::BBEmptyAndFallthru(BB *bb) {
  if (bb->IsEndTry()) {
    return false;
  }
  if (bb->kind == kBBFallthru) {
    if (func->irMap != nullptr) {
      return bb->IsMeStmtEmpty();
    } else {
      return bb->IsEmpty();
    }
  }
  return false;
}

// Return true if bb only has conditonal branch stmt except comment
bool BBLayout::BBContainsOnlyCondGoto(BB *bb) {
  if (bb->IsEndTry()) {
    return false;
  }
  if (bb->kind == kBBCondGoto) {
    if (func->irMap != nullptr) {
      MeStmt *mestmt = bb->meStmtList.first;
      MeStmt *meLast = bb->meStmtList.last;
      if (mestmt == nullptr) {
        return false;
      }
      for (; mestmt != meLast; mestmt = mestmt->next) {
        if (!mestmt->IsCondBr() && mestmt->op != OP_comment) {
          return false;
        }
      }
      return (meLast->IsCondBr());
    } else {
      StmtNode *stmt = bb->stmtNodeList.first;
      if (stmt == nullptr) {
        return false;
      }
      for (StmtNode *stmt = bb->stmtNodeList.first; stmt != bb->stmtNodeList.last; stmt = stmt->GetNext()) {
        if (!stmt->IsCondBr() && stmt->op != OP_comment) {
          return false;
        }
      }
      return (bb->stmtNodeList.last->IsCondBr());
    }
  }
  return false;
}

// Return the opposite opcode for condition/compare opcode.
static Opcode GetOppositeOp(Opcode opc1) {
  Opcode opc = kOpUndef;
  switch (opc1) {
    case OP_brtrue:
      opc = OP_brfalse;
      break;
    case OP_brfalse:
      opc = OP_brtrue;
      break;
    case OP_ne:
      opc = OP_eq;
      break;
    case OP_eq:
      opc = OP_ne;
      break;
    case OP_gt:
      opc = OP_le;
      break;
    case OP_le:
      opc = OP_gt;
      break;
    case OP_lt:
      opc = OP_ge;
      break;
    case OP_ge:
      opc = OP_lt;
      break;
    default:
      break;
  }
  return opc;
}

bool BBLayout::BBContainsOnlyGoto(BB *bb) {
  if (bb->IsEndTry()) {
    return false;
  }
  if (bb->kind == kBBGoto) {
    if (func->irMap != nullptr) {
      MeStmt *mestmt = bb->meStmtList.first;
      MeStmt *meLast = bb->meStmtList.last;
      if (mestmt == nullptr) {
        return false;
      }
      for (; mestmt != meLast; mestmt = mestmt->next) {
        if (mestmt->op != OP_goto && mestmt->op != OP_comment) {
          return false;
        }
      }
      return meLast->op == OP_goto;
    } else {
      StmtNode *stmt = bb->stmtNodeList.first;
      if (stmt == nullptr) {
        return false;
      }
      for (StmtNode *stmt = bb->stmtNodeList.first; stmt != bb->stmtNodeList.last; stmt = stmt->GetNext()) {
        if (stmt->op != OP_goto && stmt->op != OP_comment) {
          return false;
        }
      }
      return bb->stmtNodeList.last->op == OP_goto;
    }
  }
  return false;
}

// Return true if all the following are satisfied:
// 1.frombb only has one predecessor
// 2.frombb has not been laid out.
// 3.frombb has only one succor when frombb is artifical or frombb and
//   toafter_bb are both not in try block.
// The other case is frombb has one predecessor and one successor and
// contains only goto stmt.
bool BBLayout::BBCanBeMoved(BB *frombb, const BB *toafterBb) {
  if (frombb->pred.size() > 1) {
    return false;
  }
  if (laidOut[frombb->id.idx]) {
    return false;
  }
  if (frombb->Artificial() || (!frombb->InTryBlock() && !toafterBb->InTryBlock())) {
    return frombb->succ.size() == 1;
  }
  return BBContainsOnlyGoto(frombb);
}

// Return true if bb1 and bb2 has the branch conditon.such as
// bb1 : brfalse (a > 3)  bb2: brfalse (a > 3)/ brtrue (a <= 3)
bool BBLayout::HasSameBranchCond(BB *bb1, BB *bb2) {
  if (func->irMap != nullptr) {
    CondGotoMeStmt *mestmt1 = static_cast<CondGotoMeStmt *>(bb1->meStmtList.last);
    CondGotoMeStmt *mestmt2 = static_cast<CondGotoMeStmt *>(bb2->meStmtList.last);
    CHECK_FATAL(mestmt1 && mestmt2, "null ptr check");
    MeExpr *expr1 = mestmt1->opnd;
    MeExpr *expr2 = mestmt2->opnd;
    // Compare the opcode.
    if (!(mestmt1->op == mestmt2->op && expr1->op == expr2->op) &&
        !(mestmt1->op == GetOppositeOp(mestmt2->op) && expr1->op == GetOppositeOp(expr2->op))) {
      return false;
    }
    if (expr1->meOp != expr2->meOp || expr1->meOp != kMeOpOp) {
      return false;
    }
    OpMeExpr *opmeexpr1 = static_cast<OpMeExpr *>(expr1);
    OpMeExpr *opmeexpr2 = static_cast<OpMeExpr *>(expr2);
    // Compare the two operands to make sure they are both equal.
    if (opmeexpr1->GetOpnd(0) != opmeexpr2->GetOpnd(0)) {
      return false;
    }
    // If one side is const, assume it is always the rhs.
    if ((opmeexpr1->GetOpnd(1) != opmeexpr2->GetOpnd(1)) &&
        !(opmeexpr1->GetOpnd(1)->IsZero() && opmeexpr2->GetOpnd(1)->IsZero())) {
      return false;
    }
  } else {
    CondGotoNode *stmt1 = static_cast<CondGotoNode *>(bb1->stmtNodeList.last);
    CondGotoNode *stmt2 = static_cast<CondGotoNode *>(bb2->stmtNodeList.last);
    CHECK_FATAL(stmt1 && stmt2, "null ptr check");
    if (stmt1->op != stmt2->op && stmt1->op != GetOppositeOp(stmt2->op)) {
      return false;
    }
    base_node_t *expr1 = stmt1->uOpnd;
    base_node_t *expr2 = stmt2->uOpnd;
    if (expr1->op != expr2->op && expr1->op != GetOppositeOp(expr2->op)) {
      return false;
    }
    if (stmt1->op == stmt2->op) {
      if (expr1->op != expr2->op) {
        return false;
      }
    } else {
      if (expr1->op != GetOppositeOp(expr2->op)) {
        return false;
      }
    }
    return false;
  }
  return true;
}

// (1) bb's last statement is a conditional or unconditional branch; if the branch
// target is a BB with only a single goto statement, optimize the branch target
// to the eventual target
// (2) bb's last statement is a conditonal branch, if the branch target is a BB with a single
// condtioal branch statement and has the same condtion as bb's last statement, optimize the
// branch target to the eventual target.
void BBLayout::OptimizeBranchTarget(BB *bb) {
  if (func->irMap != nullptr) {
    MeStmt *lastMeStmt = bb->meStmtList.last;
    if (lastMeStmt == nullptr) {
      return;
    }
    if (lastMeStmt->op != OP_goto && !lastMeStmt->IsCondBr()) {
      return;
    }
  } else {
    StmtNode *lastStmt = bb->stmtNodeList.last;
    if (lastStmt == nullptr) {
      return;
    }
    if (lastStmt->op != OP_goto && !lastStmt->IsCondBr()) {
      return;
    }
  }
  std::unordered_set<BB *> visited_targetbbs;  // for avoiding infinite loop
start_:
  BB *brtargetbb = bb->succ[0];
  if (bb->kind == kBBCondGoto) {
    brtargetbb = bb->succ[1];
  }
  if (brtargetbb->WontExit()) {
    return;
  }
  if (visited_targetbbs.find(brtargetbb) == visited_targetbbs.end()) {
    visited_targetbbs.insert(brtargetbb);
  } else {
    return;
  }
  if (!BBContainsOnlyGoto(brtargetbb) && !BBEmptyAndFallthru(brtargetbb) &&
      !(bb->kind == kBBCondGoto && brtargetbb->kind == kBBCondGoto && bb != brtargetbb &&
        BBContainsOnlyCondGoto(brtargetbb) && HasSameBranchCond(bb, brtargetbb))) {
    return;
  }
  // optimize stmt
  BB *newtargetbb = brtargetbb->succ.front();
  if (brtargetbb->kind == kBBCondGoto) {
    newtargetbb = brtargetbb->succ[1];
  }
  if (newtargetbb->bbLabel == 0) {
    func->CreateBBLabel(newtargetbb);
  }
  LabelIdx newtargetlabel = newtargetbb->bbLabel;
  if (func->irMap != nullptr) {
    MeStmt *lastMeStmt = bb->meStmtList.last;
    if (lastMeStmt->op == OP_goto) {
      GotoMeStmt *gotomestmt = static_cast<GotoMeStmt *>(lastMeStmt);
      CHECK_FATAL(brtargetbb->bbLabel == gotomestmt->offset, "OptimizeBranchTarget: wrong branch target BB");
      gotomestmt->offset = newtargetlabel;
    } else {
      CondGotoMeStmt *gotomestmt = static_cast<CondGotoMeStmt *>(lastMeStmt);
      CHECK_FATAL(brtargetbb->bbLabel == gotomestmt->offset, "OptimizeBranchTarget: wrong branch target BB");
      gotomestmt->offset = newtargetlabel;
    }
  } else {
    StmtNode *lastStmt = bb->stmtNodeList.last;
    if (lastStmt->op == OP_goto) {
      GotoNode *gotonode = static_cast<GotoNode *>(lastStmt);
      CHECK_FATAL(brtargetbb->bbLabel == gotonode->offset, "OptimizeBranchTarget: wrong branch target BB");
      gotonode->offset = newtargetlabel;
    } else {
      CondGotoNode *gotonode = static_cast<CondGotoNode *>(lastStmt);
      CHECK_FATAL(brtargetbb->bbLabel == gotonode->offset, "OptimizeBranchTarget: wrong branch target BB");
      gotonode->offset = newtargetlabel;
    }
  }
  // update CFG
  if (bb->kind == kBBCondGoto) {
    bb->succ[1] = newtargetbb;
  } else {
    bb->succ[0] = newtargetbb;
  }
  newtargetbb->pred.push_back(bb);
  bb->RemoveBBFromVector(brtargetbb->pred);
  if (brtargetbb->pred.empty() && !laidOut[brtargetbb->id.idx]) {
    laidOut[brtargetbb->id.idx] = true;
    brtargetbb->RemoveBBFromVector(newtargetbb->pred);
  }
  goto start_;  // repeat until no more opportunity
}

void BBLayout::AddBB(BB *bb) {
  CHECK(bb->id.idx < laidOut.size(), "index oout of range in BBLayout::AddBB");
  CHECK_FATAL(!laidOut[bb->id.idx], "AddBB: bb already laid out");
  layoutBBs.push_back(bb);
  CHECK(bb->id.idx < laidOut.size(), "index out of range in BBLayout::AddBB");
  laidOut[bb->id.idx] = true;
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "bb id " << bb->id.idx << " kind is " << bb->StrAttribute();
  }
  bool isJavatry = false;
  if (func->irMap != nullptr) {
    MeStmt *firstMeStmt = bb->meStmtList.first;
    isJavatry = firstMeStmt != nullptr && (firstMeStmt->op == OP_javatry || firstMeStmt->op == OP_try);
  } else {
    StmtNode *firstStmt = bb->stmtNodeList.first;
    isJavatry = firstStmt != nullptr && (firstStmt->op == OP_javatry || firstStmt->op == OP_try);
  }

  if (isJavatry) {
    CHECK_FATAL(!tryOutstanding, "BBLayout::AddBB: cannot lay out another javatry without ending the last one");
    tryOutstanding = true;
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << " javatry";
    }
  }
  if (bb->isTryEnd && func->mirModule.IsJavaModule()) {
    tryOutstanding = false;
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << " endtry";
    }
  }
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << endl;
  }
  return;
}

BB *BBLayout::GetFallThruBBSkippingEmpty(BB *bb) {
  ASSERT(
    bb->kind == kBBFallthru || bb->kind == kBBCondGoto, "GetFallThruSkippingEmpty: unexpected BB kind");
  BB *fallthru = bb->succ.front();
  do {
    if (fallthru->pred.size() > 1) {
      return fallthru;
    }
    if (fallthru->isTryEnd) {
      return fallthru;
    }
    if (func->irMap != nullptr) {
      if (!fallthru->IsMeStmtEmpty()) {
        return fallthru;
      }
    } else {
      if (!fallthru->IsEmpty()) {
        return fallthru;
      }
    }
    if (fallthru->succ.empty()) {
      return fallthru;
    }
    laidOut[fallthru->id.idx] = true;
    fallthru->RemoveBBFromPred(bb);
    BB *oldfallthru = fallthru;
    fallthru = oldfallthru->succ.front();
    bb->ReplaceSucc(oldfallthru, fallthru);
  } while (true);
}

// bb end with a goto statement; remove the goto stmt if its target
// is its fallthru nextbb.
void BBLayout::ChangeToFallthruFromGoto(BB *bb) {
  ASSERT(bb->kind == kBBGoto, "ChangeToFallthruFromGoto: unexpected BB kind");
  if (func->irMap != nullptr) {
    bb->RemoveMeStmt(bb->meStmtList.last);
  } else {
    bb->RemoveLastStmt();
  }
  bb->kind = kBBFallthru;
}

// bb does not end with a branch statement; if its fallthru is not nextbb,
// perform the fix by either laying out the fallthru immediately or adding a goto
void BBLayout::ResolveUnconditionalFallThru(BB *bb, BB *nextbb) {
  ASSERT(bb->kind == kBBFallthru || bb->kind == kBBGoto,
          "ResolveUnconditionalFallThru: unexpected BB kind");
  if (bb->kind == kBBGoto) {
    return;
  }
  ASSERT(bb->isTry || bb->wontExit || bb->succ.size() == 1, "");
  BB *fallthru = GetFallThruBBSkippingEmpty(bb);
  if (fallthru != nextbb) {
    if (BBCanBeMoved(fallthru, bb)) {
      AddBB(fallthru);
      ResolveUnconditionalFallThru(fallthru, nextbb);
      OptimizeBranchTarget(fallthru);
    } else {
      CreateGoto(bb, func, fallthru);
      OptimizeBranchTarget(bb);
    }
  }
}

AnalysisResult *MeDoBBLayout::Run(MeFunction *func, MeFuncResultMgr *m) {
  // mempool used in analysisresult
  MemPool *layoutMp = mempoolctrler.NewMemPool(PhaseName().c_str());
  BBLayout *bblayout = layoutMp->New<BBLayout>(layoutMp, func);

  // assume commonEntryBB is always bb 0
  CHECK_FATAL(func->bbVec[0] == func->commonEntryBB, "assume bb[0] is the commont entry bb");
  BB *bb = func->first_bb_;
  while (bb != nullptr) {
    bblayout->AddBB(bb);
    if (bb->kind == kBBCondGoto || bb->kind == kBBGoto) {
      bblayout->OptimizeBranchTarget(bb);
    }
    BB *nextbb = bblayout->NextBB();
    if (nextbb != nullptr) {
      // check javatry-endtry correspondence
      bool isJavatry = false;
      if (func->irMap != nullptr) {
        MeStmt * firstMeStmt = nextbb->meStmtList.first;
        isJavatry = firstMeStmt != nullptr && (firstMeStmt->op == OP_javatry || firstMeStmt->op == OP_try);
      } else {
        StmtNode * firstStmt = nextbb->stmtNodeList.first;
        isJavatry = firstStmt != nullptr && (firstStmt->op == OP_javatry || firstStmt->op == OP_try);
      }
      CHECK_FATAL(!(isJavatry && bblayout->tryOutstanding),
             "cannot emit another javatry if last javatry has not been ended");
      if (nextbb->isTryEnd) {
        BB *trybb = func->endTryBB2TryBB[nextbb];
        CHECK_FATAL(trybb == nextbb || bblayout->laidOut[trybb->id.idx],
               "cannot emit endtry bb before its corresponding try bb");
      }
    }
    // based on nextbb, may need to fix current bb's fall-thru
    if (bb->kind == kBBFallthru) {
      bblayout->ResolveUnconditionalFallThru(bb, nextbb);
    } else if (bb->kind == kBBCondGoto) {
      BB *fallthru = bblayout->GetFallThruBBSkippingEmpty(bb);
      BB *brtargetbb = bb->succ[1];
      if (brtargetbb != fallthru && fallthru->pred.size() > 1 && bblayout->BBCanBeMoved(brtargetbb, bb)) {
        // flip the sense of the condgoto and lay out brtargetbb right here
        if (fallthru->bbLabel == 0) {
          func->CreateBBLabel(fallthru);
        }
        if (func->irMap != nullptr) {
          CondGotoMeStmt *cgotomestmt = static_cast<CondGotoMeStmt *>(bb->meStmtList.last);
          CHECK_FATAL(brtargetbb->bbLabel == cgotomestmt->offset, "bblayout: wrong branch target BB");
          cgotomestmt->offset = fallthru->bbLabel;
          cgotomestmt->op = (cgotomestmt->op == OP_brtrue) ? OP_brfalse : OP_brtrue;
        } else {
          CondGotoNode *cgotonode = static_cast<CondGotoNode *>(bb->stmtNodeList.last);
          CHECK_FATAL(brtargetbb->bbLabel == cgotonode->offset, "bblayout: wrong branch target BB");
          cgotonode->offset = fallthru->bbLabel;
          cgotonode->op = (cgotonode->op == OP_brtrue) ? OP_brfalse : OP_brtrue;
        }
        bblayout->AddBB(brtargetbb);
        bblayout->ResolveUnconditionalFallThru(brtargetbb, nextbb);
        bblayout->OptimizeBranchTarget(brtargetbb);
      } else if (fallthru != nextbb) {
        if (bblayout->BBCanBeMoved(fallthru, bb)) {
          bblayout->AddBB(fallthru);
          bblayout->ResolveUnconditionalFallThru(fallthru, nextbb);
          bblayout->OptimizeBranchTarget(fallthru);
        } else {
          // create a new fallthru that contains a goto to the original fallthru
          BB *newfallthru = func->NewBasicBlock();
          newfallthru->artificial = true;
          bblayout->laidOut.push_back(false);
          newfallthru->kind = kBBGoto;
          bblayout->SetNewBBInLayout();
          if (fallthru->bbLabel == 0) {
            func->CreateBBLabel(fallthru);
          }
          if (func->irMap != nullptr) {
            GotoNode stmt(OP_goto);
            GotoMeStmt *newgoto = func->irMap->New<GotoMeStmt>(&stmt);
            newgoto->offset = fallthru->bbLabel;
            newfallthru->SetFirstMe(newgoto);
            newfallthru->SetLastMe(newfallthru->meStmtList.first);
          } else {
            GotoNode *newgoto = func->mirFunc->codeMemPool->New<GotoNode>(OP_goto);
            newgoto->offset = fallthru->bbLabel;
            newfallthru->SetFirst(newgoto);
            newfallthru->SetLast(newfallthru->stmtNodeList.first);
          }
          /* replace pred and succ */
          bb->ReplaceSucc(fallthru, newfallthru);
          fallthru->ReplacePred(bb, newfallthru);
          newfallthru->pred.push_back(bb);
          newfallthru->succ.push_back(fallthru);
          newfallthru->frequency = fallthru->frequency;
          if (DEBUGFUNC(func)) {
            LogInfo::MapleLogger() << "Created fallthru and goto original fallthru" << endl;
          }
          bblayout->AddBB(newfallthru);
          bblayout->OptimizeBranchTarget(newfallthru);
        }
      }
    }
    if (bb->kind == kBBGoto) {
      // see if goto target can be placed here
      BB *gototarget = bb->succ.front();
      if (gototarget != nextbb && bblayout->BBCanBeMoved(gototarget, bb)) {
        bblayout->AddBB(gototarget);
        bblayout->ChangeToFallthruFromGoto(bb);
        bblayout->ResolveUnconditionalFallThru(gototarget, nextbb);
        bblayout->OptimizeBranchTarget(gototarget);
      } else if (gototarget->kind == kBBCondGoto && gototarget->pred.size() == 1) {
        BB *targetnext = gototarget->succ.front();
        if (targetnext != nextbb && bblayout->BBCanBeMoved(targetnext, bb)) {
          bblayout->AddBB(gototarget);
          bblayout->ChangeToFallthruFromGoto(bb);
          bblayout->OptimizeBranchTarget(gototarget);
          bblayout->AddBB(targetnext);
          bblayout->ResolveUnconditionalFallThru(targetnext, nextbb);
          bblayout->OptimizeBranchTarget(targetnext);
        }
      }
    }
    if (nextbb && bblayout->laidOut[nextbb->id.idx]) {
      nextbb = bblayout->NextBB();
    }
    bb = nextbb;
  }

  if (bblayout->NewBBInLayout()) {
    m->InvalidAnalysisResult(MeFuncPhase_DOMINANCE, func);
  }

  if (DEBUGFUNC(func)) {
    func->theCFG->DumpToFile("afterbblayout", false);
  }

  return bblayout;
}

}  // namespace maple
