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

#include "me_store_pre.h"

namespace maple {

#define JAVALANG (mirModule->IsJavaModule())

// ================ Step 6: Code Motion ================

void MeStorePre::CheckCreateCurTemp() {
  if (cur_temp != NULL) {
    return;
  }
  // try to use the same preg that LPRE used for the variable
  MapleUnorderedMap<OStIdx, RegMeExpr *>::iterator mapit = irMap->lpreTemps.find(work_cand->theost->index);
  if (mapit == irMap->lpreTemps.end()) {
    cur_temp = irMap->CreateRegMeExpr(work_cand->thevar->primType);
  } else {
    cur_temp = mapit->second;
  }
}

// Starting at bb's bottom, search for the definition of work_cand->theost and
// save its RHS in cur_temp.  Since the def must dominate, searching by ascending
// the dominator tree is sufficient.  If the def is by a phi, call recursively
// for each phi operand and also insert phi for cur_temp if needed.
// bb_cur_temp_map maps bb to cur_temp_version and is used to avoid re-processing
// each bb.  The return value is the cur_temp version that contains the RHS value
// at the entry to bb;
RegMeExpr* MeStorePre::EnsureRhsInCurTemp(BB *bb) {
  CHECK_FATAL(bb != func->commonEntryBB, "EnsureRhsInCurTemp: cannot find earlier definition");
  // see if processed before
  MapleUnorderedMap<BB *, RegMeExpr *>::iterator map_it = bb_cur_temp_map.find(bb);
  if (map_it != bb_cur_temp_map.end()) {
    return map_it->second;
  }
  // traverse statements
  for (MeStmt *stmt = bb->meStmtList.last; stmt; stmt = stmt->prev) {
    if (stmt->op == OP_dassign) {
      DassignMeStmt *dass = static_cast<DassignMeStmt *>(stmt);
      if (dass->lhs->ost != work_cand->theost) {
        continue;
      }
      if (DEBUGFUNC(func)) {
        LogInfo::MapleLogger() << "EnsureRhsInCurTemp: found dassign at BB" << bb->id.idx << std::endl;
      }
      if (dass->rhs->meOp == kMeOpReg && static_cast<RegMeExpr *>(dass->rhs)->ost == cur_temp->ost) {
        return static_cast<RegMeExpr *>(dass->rhs);
      }
      // create and insert regassign before dass
      RegMeExpr *lhsreg = irMap->CreateRegMeExprVersion(cur_temp);
      AssignMeStmt *rass = irMap->CreateAssignMeStmt(lhsreg, dass->rhs, bb);
      rass->srcPos = stmt->srcPos;
      lhsreg->SetDefByStmt(rass);
      bb->InsertMeStmtBefore(dass, rass);
      // change dass's rhs to lhsreg
      dass->rhs = lhsreg;
      bb_cur_temp_map[bb] = lhsreg;
      return lhsreg;
    } else if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
      MapleVector<MustDefMeNode> *mustDefList = stmt->GetMustDefList();
      CHECK_FATAL(mustDefList != nullptr, "null ptr check");
      if (!mustDefList->empty()) {
        MeExpr *mdlhs = mustDefList->front().lhs;
        if (mdlhs->meOp != kMeOpVar) {
          continue;
        }
        VarMeExpr *lhsVar = static_cast<VarMeExpr *>(mdlhs);
        if (lhsVar->ost != work_cand->theost) {
          continue;
        }
        if (DEBUGFUNC(func)) {
          LogInfo::MapleLogger() << "EnsureRhsInCurTemp: found callassigned at BB" << bb->id.idx << std::endl;
        }
        // change mustDefList
        RegMeExpr *lhsreg = irMap->CreateRegMeExprVersion(cur_temp);
        mustDefList->front().UpdateLhs(lhsreg);
        // create dassign
        AssignMeStmt *dass = irMap->CreateAssignMeStmt(lhsVar, lhsreg, bb);
        dass->srcPos = stmt->srcPos;
        lhsVar->SetDefByStmt(dass);
        bb->InsertMeStmtAfter(stmt, dass);
        bb_cur_temp_map[bb] = lhsreg;
        return lhsreg;
      }
    }
  }
  // check if there is def by phi
  MapleMap<OStIdx, MePhiNode *>::iterator phiit = bb->mePhiList.find(work_cand->theost->index);
  if (phiit != bb->mePhiList.end()) {
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "EnsureRhsInCurTemp: found def-by-phi at BB" << bb->id.idx << std::endl;
    }
    RegMeExpr *lhsreg = irMap->CreateRegMeExprVersion(cur_temp);
    bb_cur_temp_map[bb] = lhsreg;
    // form a new phi for the temp
    MePhiNode *regphi = irMap->NewInPool<MePhiNode>();
    regphi->lhs = lhsreg;
    regphi->defBB = bb;

    MePhiNode *varphi = phiit->second;
    // call recursively for each varphi operands
    for (BB *pred : bb->pred) {
      RegMeExpr *regphiopnd = EnsureRhsInCurTemp(pred);
      CHECK_FATAL(regphiopnd != nullptr,"null ptr check");
      regphi->opnds.push_back(regphiopnd);
    }
    // insert the regphi
    bb->mePhiList.insert(std::make_pair(lhsreg->ost->index, regphi));
    return lhsreg;
  }
  // continue at immediate dominator
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "EnsureRhsInCurTemp: cannot find def at BB" << bb->id.idx << std::endl;
  }
  const MapleVector<BB *> &doms = dominance->Getdoms();
  const uint32 domsSize = doms.size();
  CHECK_FATAL(domsSize > bb->id.idx, "container check");
  RegMeExpr *saved_cur_temp = EnsureRhsInCurTemp(doms[bb->id.idx]);
  CHECK_FATAL(saved_cur_temp != nullptr, "EnsureRhsInCurTemp: did not succeed");
  bb_cur_temp_map[bb] = saved_cur_temp;
  return saved_cur_temp;
}

void MeStorePre::CodeMotion() {
  // pass 1 only donig insertion
  for (SOcc *occ : all_occs) {
    if (occ->occty != kSOccLambdaRes) {
      continue;
    }
    SLambdaResOcc *lambdaResOcc = static_cast<SLambdaResOcc *>(occ);
    if (lambdaResOcc->inserthere) {
      // form the lhs VarMeExpr node
      VarMeExpr *lhsVar = irMap->CreateVarMeExprVersion(work_cand->thevar);
      // create a new dassign
      BB *insertbb = lambdaResOcc->mirbb;
      CheckCreateCurTemp();
      CHECK_FATAL(insertbb->pred.size() == 1, "CodeMotion: encountered critical edge");
      RegMeExpr *rhsreg = EnsureRhsInCurTemp(insertbb->pred[0]);
      AssignMeStmt *newdass = irMap->CreateAssignMeStmt(lhsVar, rhsreg, insertbb);
      lhsVar->SetDefByStmt(newdass);
      // insert at earliest point in BB, but after statements required to be
      // first statements in BB
      MeStmt *curstmt = insertbb->meStmtList.first;
      while (curstmt &&
             (curstmt->op == OP_javacatch || curstmt->op == OP_javatry ||
              curstmt->op == OP_catch || curstmt->op == OP_try ||
              curstmt->op == OP_cpptry || curstmt->op == OP_cppcatch ||
              curstmt->op == OP_comment)) {
        curstmt = curstmt->next;
      }
      if (curstmt == nullptr) {
        insertbb->AddMeStmtLast(newdass);
      } else {
        insertbb->InsertMeStmtBefore(curstmt, newdass);
      }
    }
  }
  // pass 2 only doing deletion
  for (SOcc *occ : work_cand->real_occs) {
    if (occ->occty != kSOccReal) {
      continue;
    }
    SRealOcc *realocc = static_cast<SRealOcc *>(occ);
    if (realocc->redundant) {
      if (realocc->mestmt->op == OP_dassign) {
        DassignMeStmt *dass = static_cast<DassignMeStmt *>(realocc->mestmt);
        if (dass->rhs->CouldThrowException()) {
       // insert a new eval statement
          UnaryMeStmt *evalstmt = irMap->New<UnaryMeStmt>(OP_eval);
          evalstmt->bb = dass->bb;
          evalstmt->srcPos = dass->srcPos;
          evalstmt->opnd = dass->rhs;
          realocc->mirbb->InsertMeStmtBefore(dass, evalstmt);
        }
        realocc->mirbb->RemoveMeStmt(dass);
      } else {
        CHECK_FATAL(kOpcodeInfo.IsCallAssigned(realocc->mestmt->op), "CodeMotion: callassign expected");
        MapleVector<MustDefMeNode> *mustDefList = realocc->mestmt->GetMustDefList();
        CHECK_FATAL(mustDefList != nullptr, "null ptr check");
        mustDefList->clear();
      }
    }
  }
}

// ================ Step 0: collect occurrences ================

// create a new real occurrence for the store of mestmt of symbol oidx
void MeStorePre::CreateRealOcc(OStIdx oidx, MeStmt *mestmt) {
  SSUPreWorkCand *wkcand = nullptr;
  MapleMap<OStIdx, SSUPreWorkCand *>::iterator mapit = workcand_map.find(oidx);
  if (mapit != workcand_map.end()) {
    wkcand = mapit->second;
  } else {
    OriginalSt *ost = ssaTab->GetSymbolOriginalStFromid(oidx);
    wkcand = ssuPreMempool->New<SSUPreWorkCand>(&ssuPreAlloc, ost);
    workcand_map[oidx] = wkcand;
    // if it is local symbol, insert artificial real occ at commonExitBB
    if (ost->isLocal) {
      SRealOcc *artocc = ssuPreMempool->New<SRealOcc>();
      artocc->mirbb = func->commonExitBB;
      wkcand->real_occs.push_back(artocc);
    }
  }
  if (wkcand->thevar == NULL) {
    if (mestmt->op == OP_dassign) {
      wkcand->thevar = static_cast<DassignMeStmt *>(mestmt)->GetVarLhs();
    } else {
      ASSERT(kOpcodeInfo.IsCallAssigned(mestmt->op), "CreateRealOcc: callassign expected");
      MapleVector<MustDefMeNode> *mustDefList = mestmt->GetMustDefList();
      CHECK_FATAL((mustDefList != nullptr) && (!mustDefList->empty()), "CreateRealOcc: mustDefList cannot be empty");
      wkcand->thevar = static_cast<VarMeExpr *>(mustDefList->front().lhs);
    }
  }

  SRealOcc *newocc = ssuPreMempool->New<SRealOcc>(mestmt);
  wkcand->real_occs.push_back(newocc);
  wkcand->IncDefCnt();
}

// create a new kill occurrence for symbol oidx in given bb
void MeStorePre::CreateKillOcc(OStIdx oidx, BB *bb) {
  SSUPreWorkCand *wkcand = nullptr;
  MapleMap<OStIdx, SSUPreWorkCand *>::iterator mapit = workcand_map.find(oidx);
  if (mapit == workcand_map.end()) {
    return;
  }
  wkcand = mapit->second;
  CHECK_FATAL(wkcand->real_occs.size() > 0, "empty container check");
  SOcc *lastocc = wkcand->real_occs.back();
  if (lastocc->occty == kSOccKill && lastocc->mirbb == bb) {
    return;  // no need to push consecutive kill occurrences at same BB
  }
  SKillOcc *newocc = ssuPreMempool->New<SKillOcc>(bb);
  wkcand->real_occs.push_back(newocc);
}

// create kill occurs for all the symbols that alias with muost
void MeStorePre::CreateSpreKillOccsThruAliasing(const OriginalSt *muost, BB *bb) {
  if (muost->indexRenamedFrom.idx != 0) {  // change to use the original ost
    muost = ssaTab->GetSymbolOriginalStFromid(muost->indexRenamedFrom);
  }
  if (muost->index.idx >= aliasclass->osym2Elem.size()) {
    return;
  }
  AliasElem *ae = aliasclass->osym2Elem[muost->index.idx];
  if (ae->classSet == nullptr) {
    return;
  }
  for (MapleSet<uint>::iterator setit = ae->classSet->begin(); setit != ae->classSet->end(); setit++) {
    uint elemid = *setit;
    AliasElem *ae0 = aliasclass->id2Elem[elemid];
    if (ae0->ost->indirectLev == 0) {
      CreateKillOcc(ae0->ost->index, bb);
    }
  }
}

void MeStorePre::FindAndCreateSpreKillOccs(MeExpr *x, BB *bb) {
  if (x->meOp == kMeOpVar) {
    VarMeExpr *varx = static_cast<VarMeExpr *>(x);
    OriginalSt *ost = varx->ost;
    if (!ost->IsVolatile()) {
      CreateKillOcc(varx->ost->index, bb);
    }
    return;
  }
  for (int32 i = 0; i < x->NumMeExprOpnds(); i++) {
    FindAndCreateSpreKillOccs(x->GetOpnd(i), bb);
  }
  if (JAVALANG) {
    return;
  }
  if (x->meOp == kMeOpIvar) {
    IvarMeExpr *ivarx = static_cast<IvarMeExpr *>(x);
    if (ivarx->mu != nullptr) {
      CreateSpreKillOccsThruAliasing(ivarx->mu->ost, bb);
    }
  }
}

void MeStorePre::CreateSpreKillOccsForAll(BB *bb) {
  // go thru all workcands and insert a kill occurrence for each of them
  for (std::pair<OStIdx, SSUPreWorkCand *> wkcandpair : workcand_map) {
    SSUPreWorkCand *wkcand = wkcandpair.second;
    CHECK_FATAL(wkcand != nullptr, "null ptr check");
    SOcc *lastocc = wkcand->real_occs.back();
    if (lastocc->occty == kSOccKill && lastocc->mirbb == bb) {
      continue;  // no need to push consecutive kill occurrences at same BB
    }
    SKillOcc *newocc = ssuPreMempool->New<SKillOcc>(bb);
    wkcand->real_occs.push_back(newocc);
  }
}

void MeStorePre::BuildWorkListBB(BB *bb) {
  if (bb == nullptr) {
    return;
  }

  // traverse statements backwards
  for (MeStmt *stmt = bb->meStmtList.last; stmt; stmt = stmt->prev) {
    // look for real occurrence of stores
    OStIdx lhsostidx(0);
    if (stmt->op == OP_dassign) {
      DassignMeStmt *dass = static_cast<DassignMeStmt *>(stmt);
      if (dass->lhs->primType != PTY_ref) {
        lhsostidx = dass->lhs->ost->index;
      }
    } else if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
      MapleVector<MustDefMeNode> *mustDefList = stmt->GetMustDefList();
      CHECK_FATAL(mustDefList != nullptr, "null ptr check");
      if (!mustDefList->empty()) {
        MeExpr *mdlhs = mustDefList->front().lhs;
        if (mdlhs->meOp == kMeOpVar && mdlhs->primType != PTY_ref) {
          lhsostidx = static_cast<VarMeExpr *>(mdlhs)->ost->index;
        }
      }
    }
    if (lhsostidx.idx != 0) {
      OriginalSt *ost = ssaTab->GetOriginalStFromid(lhsostidx);
      if (!ost->IsVolatile()) {
        CreateRealOcc(lhsostidx, stmt);
      }
    }

    // look for kill occurrence of stores
    for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
      FindAndCreateSpreKillOccs(stmt->GetMeStmtOpnd(i), stmt->bb);
    }
    if (!JAVALANG) {
      // go thru mu list
      NaryMeStmt *nstmt = dynamic_cast<NaryMeStmt *>(stmt);
      if (nstmt != nullptr) {
        CHECK_FATAL(nstmt->GetMuList() != nullptr, "null ptr check");
        for (std::pair<OStIdx, ScalarMeExpr *> mupair : *(nstmt->GetMuList())) {
          CreateSpreKillOccsThruAliasing(mupair.second->ost, bb);
        }
      }
    }
  }

  if (bb->IsCatch()) {
    CreateSpreKillOccsForAll(bb);
  }
  if (bb->isEntry) {
    CreateEntryOcc(bb);
  }

  // recurse on child BBs in post-dominator tree
  for (BBId bbid : dominance->pdomChildren[bb->id.idx]) {
    BuildWorkListBB(func->bbVec[bbid.idx]);
  }
}

AnalysisResult *MeDoStorePre::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
  ASSERT(dom != nullptr, "dominance phase has problem");

  AliasClass *aliasclass = static_cast<AliasClass *>(m->GetAnalysisResult(MeFuncPhase_ALIASCLASS, func));
  ASSERT(aliasclass != nullptr, "aliasclass phase has problem");

  MeIRMap *irMap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func));
  CHECK_FATAL(irMap != nullptr, "irMap phase has problem");

  MemPool *spremp = mempoolctrler.NewMemPool(PhaseName().c_str());

  MeStorePre storepre(func, dom, aliasclass, spremp);

  storepre.ApplySSUPre();

  if (DEBUGFUNC(func)) {
    irMap->Dump();
  }

  mempoolctrler.DeleteMemPool(spremp);

  return nullptr;
}

}  // namespace maple
