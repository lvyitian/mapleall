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

#include "me_hdse.h"
#include <iostream>
#include "ssa_mir_nodes.h"
#include "ver_symbol.h"
#include "dominance.h"
#include "me_irmap.h"
#include "me_ssa.h"

#define JAVALANG (mirModule->IsJavaModule())

namespace maple {

// The hdse phase performs dead store elimination using the well-known algorithm
// based on SSA.  The algorithm works as follows:
// 0. Initially, assume all stmts and expressions are not needed.
// 1. In a pass over the program, mark stmts that cannot be deleted as needed.
//    These include return/eh/call statements and assignments to volatile fields
//    and variables.  Expressions used by needed statements are put into a
//    worklist.
// 2. Process each node in the worklist. For any variable used in the expression,
//    mark the def stmt for the variable as needed and push expressions used
//    by the def stmt to the worklist.
// 3. When the worklist becomes empty, perform a pass over the program to delete
//    statements that have not been marked as needed.
//
// The backward substitution optimization is also performed in this phase by
// piggy-backing on Step 3.  In the pass over the program in Step 3, it counts
// the number of uses for each variable version.  It also create a list of
// backward substitution candidates, which are dassign statements of the form:
//              x_i = y_j
// where x is a local variable with no alias, and y_j is defined via the return
// value of a call which is in the same BB.  Then,
// 4. For each backward substitution candidate x_i = y_j, if the use count of y_j
//    is 1, then replace the return value definition of y_j by x_i, and delete
//    the statement x_i = y_j.
// Before the hdse phase finishes, it performs 2 additional optimizations:
// 5. When earlier deletion caused a try block to become empty, delete the empty
//    try block while fixing up the CFG.
// 6. Perform unreacble code analysis, delete the BBs found unreachable and fix
//    up the CFG.

void MeHDSE::DseInit() {
  // Init bb's required flag
  bb_required.resize(0);
  bb_required.resize(func->theCFG->bbVec.size(), false);
  if (func->theCFG->commonEntryBB != func->theCFG->first_bb) {
    bb_required[func->theCFG->commonEntryBB->id.idx] = true;
  }
  if (func->theCFG->commonExitBB != func->theCFG->last_bb) {
    bb_required[func->theCFG->commonExitBB->id.idx] = true;
  }

  // Init all MeExpr to be dead;
  expr_live.resize(0);
  expr_live.resize(irMap->exprID, false);
  // Init all use counts to be 0
  verstUseCounts.resize(0);
  verstUseCounts.resize(irMap->verst2MeExprTable.size(), 0);
}

void MeHDSE::DseInitFull() {
  DseInit();
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    // make the phi nodes dead
    for (std::pair<OStIdx, MePhiNode *> phipair : bb->mePhiList) {
      phipair.second->isLive = false;
    }


    for (auto stmt : bb->meStmtList) {
      // make stmt dead
      stmt->isLive = false;

      // make chi nodes dead
      MapleMap<OStIdx, ChiMeNode *> *chilist = stmt->GetChiList();
      if (chilist != nullptr) {
        for (std::pair<OStIdx, ChiMeNode *> mapelem : *chilist) {
          mapelem.second->isLive = false;
        }
      }

      // make mustDef nodes dead
      if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
        MapleVector<MustDefMeNode> *mustdefList = stmt->GetMustDefList();
        for (MustDefMeNode &mustDef : *mustdefList) {
          mustDef.isLive = false;
        }
      }
    }
  }
}

void MeHDSE::ProcessWhileInfos() {
  MapleMap<LabelIdx, LfoWhileInfo *>::iterator it = func->lfoFunc->label2WhileInfo.begin();
  for ( ; it != func->lfoFunc->label2WhileInfo.end(); it++) {
    if (it->second->initExpr != nullptr) {
      worklist.push_front(it->second->initExpr);
    }
  }
}

void MeHDSE::DetermineUseCounts(MeExpr *x) {
  if (x->meOp == kMeOpVar) {
    VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(x);
    verstUseCounts[varmeexpr->vstIdx]++;
    return;
  }
  for (int32 i = 0; i < x->NumMeExprOpnds(); i++) {
    DetermineUseCounts(x->GetOpnd(i));
  }
}

void MeHDSE::CheckBackSubsCandidacy(DassignMeStmt *dass) {
  if (!dass->chiList.empty()) {
    return;
  }
  if (dass->rhs->meOp != kMeOpVar && dass->rhs->meOp != kMeOpReg) {
    return;
  }
  ScalarMeExpr *lhsscalar = static_cast<ScalarMeExpr *>(dass->lhs);
  if (!lhsscalar->ost->isLocal) {
    return;
  }
  ScalarMeExpr *rhsscalar = static_cast<ScalarMeExpr *>(dass->rhs);
  if (rhsscalar->defBy != kDefByMustdef) {
    return;
  }
  if (rhsscalar->DefByBB() != dass->bb) {
    return;
  }
  backSubsCands.push_front(dass);
}

void MeHDSE::UpdateStmt(BB *bb) {
  MeStmt *mestmt = bb->meStmtList.first;
  MeStmt *nextstmt = nullptr;
  while (mestmt) {
    nextstmt = mestmt->next;
    if (!mestmt->isLive) {
      if (hdsedebug) {
        LogInfo::MapleLogger() << "========== hssa dse deletes this stmt: ";
        mestmt->Dump(irMap);
      }
      if (mestmt->IsCondBr() || mestmt->op == OP_switch || mestmt->op == OP_igoto) {
        // update CFG
        while (bb->succ.size() != 1) {
          BB *succbb = bb->succ.back();
          succbb->RemoveBBFromPred(bb);
          bb->succ.pop_back();
        }
        bb->kind = kBBFallthru;
      }
      bb->RemoveMeStmt(mestmt);
    }
    else {
      if (mestmt->IsCondBr()) { // see if foldable to unconditional branch
        CondGotoMeStmt *condbr = static_cast<CondGotoMeStmt *>(mestmt);
        if (!JAVALANG && condbr->opnd->meOp == kMeOpConst) {
          CHECK_FATAL(IsPrimitiveInteger(condbr->opnd->primType), "MeHDSE::DseProcess: branch condition must be integer type");
          if ((condbr->op == OP_brtrue && condbr->opnd->IsZero()) ||
              (condbr->op == OP_brfalse && !condbr->opnd->IsZero())) {
            // delete the conditional branch
            BB *succbb = bb->succ.back();
            succbb->RemoveBBFromPred(bb);
            bb->succ.pop_back();
            bb->kind = kBBFallthru;
            bb->RemoveMeStmt(mestmt);
          } else {
            // change to unconditional branch
            BB *succbb = bb->succ.front();
            succbb->RemoveBBFromPred(bb);
            bb->succ.erase(bb->succ.begin());
            bb->kind = kBBGoto;
            GotoMeStmt *gotomestmt = irMap->New<GotoMeStmt>(condbr->offset);
            bb->ReplaceMeStmt(condbr, gotomestmt);
          }
        } else {
          DetermineUseCounts(condbr->opnd);
        }
      } else {
        for (int32 i = 0; i < mestmt->NumMeStmtOpnds(); i++) {
          DetermineUseCounts(mestmt->GetMeStmtOpnd(i));
        }
        if (mestmt->op == OP_dassign) {
          CheckBackSubsCandidacy(static_cast<DassignMeStmt *>(mestmt));
        }
      }
    }
    mestmt = nextstmt;
  }
  // update verstUseCOunts for uses in phi operands
  for (std::pair<OStIdx, MePhiNode *> phipair : bb->mePhiList) {
    if (phipair.second->isLive) {
      for (ScalarMeExpr *phiOpnd : phipair.second->opnds) {
        VarMeExpr *varx = dynamic_cast<VarMeExpr *>(phiOpnd);
        if (varx) {
          verstUseCounts[varx->vstIdx]++;
        }
      }
    }
  }
}

void MeHDSE::BackwardSubstitution() {
  for (DassignMeStmt *dass : backSubsCands) {
    ScalarMeExpr *rhsscalar = static_cast<ScalarMeExpr *>(dass->rhs);
    if (verstUseCounts[rhsscalar->vstIdx] != 1) {
      continue;
    }
    ScalarMeExpr *lhsscalar = dass->lhs;
    // check that lhsscalar has no use after rhsscalar's definition
    CHECK_FATAL(rhsscalar->defBy == kDefByMustdef, "MeHDSE::BackwardSubstitution: rhs not defined by mustDef");
    MustDefMeNode *mustDef = rhsscalar->def.defMustDef;
    MeStmt *defStmt = mustDef->base;
    bool hasAppearance = false;
    MeStmt *curstmt = dass->prev;
    while (curstmt != defStmt && !hasAppearance) {
      for (int32 i = 0; i < curstmt->NumMeStmtOpnds(); i++) {
        if (curstmt->GetMeStmtOpnd(i)->SymAppears(lhsscalar->ost->index)) {
          hasAppearance = true;
        }
      }
      curstmt = curstmt->prev;
    }
    if (hasAppearance) {
      continue;
    }
    // perform the backward substitution
    if (hdsedebug) {
      LogInfo::MapleLogger() << "------ hdse backward substitution deletes this stmt: ";
      dass->Dump(irMap);
    }
    mustDef->UpdateLhs(lhsscalar);
    dass->bb->RemoveMeStmt(dass);
  }
}

void MeDohDSE::MakeEmptyTrysUnreachable(MeFunction *func) {
  MapleVector<BB *> &bbVec = func->theCFG->bbVec;
  for (uint32_t i = 0; i < bbVec.size(); i++) {
    BB *trybb = bbVec[i];
    if (trybb != nullptr && trybb->isTry && trybb->meStmtList.first != nullptr &&
        (trybb->meStmtList.first->op == OP_javatry || trybb->meStmtList.first->op == OP_try || trybb->meStmtList.first->op == OP_cpptry) &&
        trybb->GetTheOnlyMeStmt() == trybb->meStmtList.first &&
        trybb->mePhiList.empty() && i + 1 < bbVec.size() &&
        bbVec[i + 1] != nullptr && bbVec[i + 1]->isTryEnd && bbVec[i + 1]->IsMeStmtEmpty()) {
      // we found a try BB followed by an empty endtry BB
      BB *endtry = bbVec[i + 1];
      BB *targetbb = endtry->succ[0];
      std::vector<BB *> toDeleteTrypreds;
      int32_t indexOfEndtryInTargetbbpreds = -1;

      for (auto trypred = trybb->pred.begin(); trypred != trybb->pred.end(); trypred++) {
        MapleVector<BB *> &allSucc = (*trypred)->succ;
        toDeleteTrypreds.push_back(*trypred);
        // replace trybb in the pred's succ list by targetbb
        uint32_t j = 0;
        for (; j < allSucc.size(); j++) {
          if (allSucc[j] == trybb) {
            break;
          }
        }
        CHECK_FATAL(j != allSucc.size(),
               "MakeEmptyTrysUnreachable: cannot find trybb among the succ list of one of its pred BB");
        allSucc[j] = targetbb;

        // update targetbb's predecessors
        uint32_t k = 0;
        for (; k < targetbb->pred.size(); k++) {
          if (targetbb->pred[k] == endtry) {
            indexOfEndtryInTargetbbpreds = k;
          } else if (targetbb->pred[k] == *trypred) {
            break;
          }
        }
        if (k == targetbb->pred.size()) {
          targetbb->pred.push_back(*trypred);
          CHECK_FATAL(indexOfEndtryInTargetbbpreds != -1, "MakeEmptyTrysUnreachable: processing error");
          // push additional phi operand for each phi at targetbb
          MapleMap<OStIdx, MePhiNode *>::iterator phiit = targetbb->mePhiList.begin();
          for (; phiit != targetbb->mePhiList.end(); phiit++) {
            MePhiNode *mevarphi = phiit->second;
            mevarphi->opnds.push_back(mevarphi->opnds[indexOfEndtryInTargetbbpreds]);
          }
        }

        // if needed, update branch label
        MeStmt *br = (*trypred)->meStmtList.last;
        if (br != nullptr) {
          if (br->IsCondBr()) {
            if (static_cast<CondGotoMeStmt *>(br)->offset == trybb->bbLabel) {
              func->CreateBBLabel(targetbb);
              static_cast<CondGotoMeStmt *>(br)->offset = targetbb->bbLabel;
            }
          } else if (br->op == OP_goto) {
            func->CreateBBLabel(targetbb);
            ASSERT(static_cast<GotoMeStmt *>(br)->offset == trybb->bbLabel, "Wrong label");
            static_cast<GotoMeStmt *>(br)->offset = targetbb->bbLabel;
          } else if (br->op == OP_multiway || br->op == OP_rangegoto) {
            CHECK_FATAL(false, "OP_multiway and OP_rangegoto are not supported");
          } else if (br->op == OP_switch) {
            func->CreateBBLabel(targetbb);
            SwitchMeStmt *switchnode = static_cast<SwitchMeStmt *>(br);
            if (switchnode->defaultLabel == trybb->bbLabel) {
              switchnode->defaultLabel = targetbb->bbLabel;
            }
            for (uint32 j = 0; j < switchnode->switchTable.size(); j++) {
              if (switchnode->switchTable[j].second == trybb->bbLabel) {
                switchnode->switchTable[j].second = targetbb->bbLabel;
              }
            }
          }
        }
      }
      // trybb has no phis, no need to update them
      for (auto bb : toDeleteTrypreds) {
        bb->RemoveBBFromVector(trybb->pred);
      }
    }
  }
}

AnalysisResult *MeDohDSE::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *pdom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func, !MeOption::quiet));

  MeIRMap *hmap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func, !MeOption::quiet));
  ASSERT(hmap != nullptr, "hssamap is nullptr");

  MeHDSE hdse(func, pdom, hmap);
  if (DEBUGFUNC(func)) {
    hdse.hdsedebug = true;
  }
  hdse.dsekeepref = MeOption::dsekeepref;
  MIRFunction *mirfunction = func->mirFunc;

  hdse.DseInitFull();
  hdse.DseProcess();
  hdse.Update();
  hdse.BackwardSubstitution();
  MakeEmptyTrysUnreachable(func);
  func->theCFG->UnreachCodeAnalysis(/* update_phi = */ true);
  func->theCFG->WontExitAnalysis();
  m->InvalidAnalysisResult(MeFuncPhase_DOMINANCE, func);
  m->InvalidAnalysisResult(MeFuncPhase_IDENTLOOPS, func);
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============== HDSE =============" << std::endl;
    hmap->Dump();
  }
  return nullptr;
}

}  // namespace maple
