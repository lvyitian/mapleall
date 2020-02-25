/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan PSL v1.
 * You can use this software according to the terms and conditions of the Mulan PSL v1.
 * You may obtain a copy of Mulan PSL v1 at:
 *
 *     http://license.coscl.org.cn/MulanPSL
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v1 for more details.
 */

#include "me_dominance.h"
#include "me_stmt_pre.h"
#include "me_option.h"
#include "me_ssa_update.h"

static const std::set<std::string> kStmtpreWhiteList{
#define DOSTMTPRE(funcname) #funcname,
#include "stmt_pre_list.def"
#undef DOSTMTPRE
};

// Note: after the movement of assignments, some phi nodes that used to be dead
// can become live.  Before we run another round of dead store elimination, we
// should NOT trust the isLive flag in phi nodes.

// accumulate the BBs that are in the iterated dominance frontiers of bb in
// the set dfset, visiting each BB only once

namespace maple {

void MeStmtPre::CodeMotion() {
  for (MeOccur *occ : all_occs) {
    switch (occ->occty) {
      case kOccReal: {
        MeRealOcc *realocc = static_cast<MeRealOcc *>(occ);
        if (realocc->is_save) {
          CHECK_FATAL(realocc->is_reload == false, "");
        } else if (realocc->is_reload) {
          realocc->mirbb->RemoveMeStmt(realocc->mestmt);
          if (realocc->mestmt->op == OP_dassign) {
            DassignMeStmt *dass = static_cast<DassignMeStmt *>(realocc->mestmt);
            OStIdx oidx = dass->lhs->ost->index;
            if (cands_for_ssaupdate.find(oidx) == cands_for_ssaupdate.end()) {
              cands_for_ssaupdate[oidx] =
                ssapre_mp->New<MapleSet<BBId>>(std::less<BBId>(), ssapre_allocator.Adapter());
            }
          }
        }
        break;
      }
      case kOccPhiopnd: {
        MePhiOpndOcc *phiopnd = static_cast<MePhiOpndOcc *>(occ);
        if (!phiopnd->def_phiocc->IsWillBeAvail()) {
          break;
        }
        MeOccur *defocc = phiopnd->def;
        if (defocc->occty == kOccInserted) {
          if (!phiopnd->is_phiopndreload) {
            MeInsertedOcc *insertedocc = static_cast<MeInsertedOcc *>(defocc);
            if (insertedocc->mestmt->op == OP_dassign) {
              DassignMeStmt *dass = static_cast<DassignMeStmt *>(insertedocc->mestmt);
              OStIdx oidx = dass->lhs->ost->index;
              if (cands_for_ssaupdate.find(oidx) == cands_for_ssaupdate.end()) {
                MapleSet<BBId> *bbset =
                  ssapre_mp->New<MapleSet<BBId>>(std::less<BBId>(), ssapre_allocator.Adapter());
                bbset->insert(occ->mirbb->id);
                cands_for_ssaupdate[oidx] = bbset;
              } else {
                cands_for_ssaupdate[oidx]->insert(occ->mirbb->id);
              }
              // create a new LHS for the dassign in insertedocc->mestmt
              VarMeExpr *newvarversion = irMap->CreateVarMeExprVersion(static_cast<VarMeExpr*>(dass->lhs));
              dass->UpdateLhs(newvarversion);
            }
            if (insertedocc->mestmt->op == OP_intrinsiccallwithtype &&
                static_cast<IntrinsiccallMeStmt *>(insertedocc->mestmt)->intrinsic == INTRN_JAVA_CLINIT_CHECK) {
              BB *insertbb = insertedocc->mirbb;
              // insert at earlist point in BB, but after statements required
              // to be first statement in BB
              MeStmt *curstmt = insertbb->meStmtList.first;
              while (curstmt &&
                     (curstmt->op == OP_javacatch || curstmt->op == OP_javatry ||
                      curstmt->op == OP_catch || curstmt->op == OP_try ||
                      curstmt->op == OP_cppcatch || curstmt->op == OP_cpptry ||
                      curstmt->op == OP_comment)) {
                curstmt = curstmt->next;
              }
              if (curstmt == nullptr) {
                insertbb->AddMeStmtLast(insertedocc->mestmt);
              } else {
                insertbb->InsertMeStmtBefore(curstmt, insertedocc->mestmt);
              }
            } else {
              insertedocc->mirbb->InsertMeStmtLastBr(insertedocc->mestmt);
            }
          }
        }
        break;
      }
      case kOccPhiocc:
      case kOccExit:
      case kOccUse:
      case kOccMembar:
        break;
      default:
        ASSERT(false, "");
    }
  }
}

void MeStmtPre::Finalize1() {
  std::vector<MeOccur *> availDefVec(class_count, nullptr);
  // traversal in preoder DT
  for (MeOccur *occ : all_occs) {
    uint32 classx = occ->classid;
    switch (occ->occty) {
      case kOccPhiocc: {
        MePhiOcc *phiocc = static_cast<MePhiOcc *>(occ);
        if (phiocc->IsWillBeAvail()) {
          availDefVec[classx] = phiocc;
        }
        break;
      }
      case kOccReal: {
        MeOccur *availDef = availDefVec[classx];
        MeRealOcc *realocc = static_cast<MeRealOcc *>(occ);
        if (availDef == nullptr || !availDef->IsDominate(dominance, occ)) {
          realocc->is_reload = false;
          availDefVec[classx] = realocc;
        } else {
          realocc->is_reload = true;
          ASSERT(!realocc->is_save, "real occ with is_save cannot be set is_reload");
          realocc->def = availDefVec[classx];
          CHECK_FATAL(realocc->def != nullptr, "");
        }
        break;
      }
      case kOccPhiopnd: {
        // we assume one phiopnd has only one phiocc use because critical edge split the blocks
        MePhiOpndOcc *phiopnd = static_cast<MePhiOpndOcc *>(occ);
        MePhiOcc *phiocc = phiopnd->def_phiocc;
        if (phiocc->IsWillBeAvail()) {
          if (phiopnd->IsOkToInsert()) {
            // insert the current expression at the end of the block containing phiopnd
            if (phiopnd->mirbb->succ.size() > 1) {
              CHECK_FATAL(!work_cand->redo2handle_crit_edges, "Finalize1: insertion at critical edge; aborting");
              work_cand->redo2handle_crit_edges = true;
              if (ssapredebug)
                LogInfo::MapleLogger() << "<<<<< Re-doing this candidate due to existence of critical edge >>>>>\n";
              return;
            }
            MeStmt *insertedStmt = phiopnd->GetCurrentMeStmt();
            ASSERT(insertedStmt != nullptr, "NYI");
            MeInsertedOcc *insertedOcc =
              percand_mp->New<MeInsertedOcc>(static_cast<MeExpr*>(nullptr), insertedStmt, phiopnd->mirbb);
            insertedOcc->classid = class_count++;
            phiopnd->def = insertedOcc;
            phiopnd->classid = insertedOcc->classid;
            if (work_cand->puIdx != GetPuidx(phiopnd->mirbb)) {
              ASSERT(!work_cand->has_local_opnd, "candidate with local opnd cannot be inserted outside its PU");
              work_cand->puIdx = 0;
            }
            phiopnd->is_insertedocc = true;
          } else {
            phiopnd->def = availDefVec[classx];
          }
        }
        break;
      }
      case kOccExit:
      case kOccUse:
      case kOccMembar:
        break;
      default:
        ASSERT(false, "");
    }
  }

  if (ssapredebug) {
    PreWorkCand *curCand = work_cand;
    LogInfo::MapleLogger() << "========ssapre candidate " << curCand->index << " after Finalize1===================\n";
    for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
      MePhiOcc *phiocc = *it;
      if (phiocc->IsWillBeAvail()) {
        MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
        for (uint32 i = 0; i < phiopnds.size(); i++) {
          MePhiOpndOcc *phiopnd = phiopnds[i];
          ASSERT(phiopnd->def, "EPhiFinalizer::DumpFinalize1: phiopndocc cannot have no def");
          MeOccur *defocc = phiopnd->def;
          if (defocc->occty == kOccInserted) {
            MeInsertedOcc *realdefocc = static_cast<MeInsertedOcc *>(defocc);
            phiopnd->Dump(irMap);
            LogInfo::MapleLogger() << " was inserted by ";
            realdefocc->Dump(irMap);
            LogInfo::MapleLogger() << "\n";
          }
        }
      }
    }
  }
}

bool MeStmtPre::AllVarsSameVersion(MeRealOcc *realocc1, MeRealOcc *realocc2) {
  switch (realocc1->mestmt->op) {
    case OP_assertnonnull: {
      UnaryMeStmt *unarystmt1 = static_cast<UnaryMeStmt *>(realocc1->mestmt);
      UnaryMeStmt *unarystmt2 = static_cast<UnaryMeStmt *>(realocc2->mestmt);
      return unarystmt1->opnd == unarystmt2->opnd;
    }
    case OP_dassign: {
      DassignMeStmt *dass1 = static_cast<DassignMeStmt *>(realocc1->mestmt);
      DassignMeStmt *dass2 = static_cast<DassignMeStmt *>(realocc2->mestmt);
      return dass1->rhs == dass2->rhs && realocc1->meexpr == realocc2->meexpr;
    }
    case OP_intrinsiccallwithtype:
      return true;
    case OP_intrinsiccall: {
      IntrinsiccallMeStmt *intrn1 = static_cast<IntrinsiccallMeStmt *>(realocc1->mestmt);
      IntrinsiccallMeStmt *intrn2 = static_cast<IntrinsiccallMeStmt *>(realocc2->mestmt);
      for (int32 i = 0; i < intrn1->NumMeStmtOpnds(); i++)
        if (intrn1->opnds[i] != intrn2->opnds[i]) {
          return false;
        }
      return true;
    }
    case OP_callassigned: {
      CallMeStmt *callass1 = static_cast<CallMeStmt *>(realocc1->mestmt);
      CallMeStmt *callass2 = static_cast<CallMeStmt *>(realocc2->mestmt);
      for (int32 i = 0; i < callass1->NumMeStmtOpnds(); i++)
        if (callass1->opnds[i] != callass2->opnds[i]) {
          return false;
        }
      return true;
    }
    default:
      ASSERT(false, "MeStmtEPre::AllVarsSameVersion: NYI");
  }
  return false;
}

// collect meexpr's variables and put them into varvec;
// varvec can only store RegMeExpr and VarMeExpr
void MeStmtPre::CollectVarForMeStmt(MeStmt *stmt, MeExpr *meexpr, std::vector<MeExpr *> &varvec) {
  switch (stmt->op) {
    case OP_assertnonnull: {
      UnaryMeStmt *unarystmt = static_cast<UnaryMeStmt *>(stmt);
      if (unarystmt->opnd->meOp == kMeOpVar || unarystmt->opnd->meOp == kMeOpReg) {
        varvec.push_back(unarystmt->opnd);
      }
      break;
    }
    case OP_dassign: {
      DassignMeStmt *dassmestmt = static_cast<DassignMeStmt *>(stmt);
      if (dassmestmt->rhs->meOp == kMeOpVar || dassmestmt->rhs->meOp == kMeOpReg) {
        varvec.push_back(dassmestmt->rhs);
      }
      if (meexpr) {
        CHECK_FATAL(meexpr->meOp == kMeOpVar, "CollectVarForMeStmt:bad meexpr field in realocc node");
        varvec.push_back(meexpr);
      }
      break;
    }
    case OP_intrinsiccallwithtype:
    case OP_intrinsiccall:
    case OP_callassigned: {
      NaryMeStmt *nstmt = static_cast<NaryMeStmt *>(stmt);
      for (int32 i = 0; i < nstmt->NumMeStmtOpnds(); i++)
        if (nstmt->opnds[i]->meOp == kMeOpVar || nstmt->opnds[i]->meOp == kMeOpReg) {
          varvec.push_back(nstmt->opnds[i]);
        }
      break;
    }
    default:
      CHECK_FATAL(false, "MeStmtEPre::CollectVarForCand: NYI");
  }
}

void MeStmtPre::CollectVarForCand(MeRealOcc *realocc, std::vector<MeExpr *> &varvec) {
  CollectVarForMeStmt(realocc->mestmt, realocc->meexpr, varvec);
}

static MeStmt *CopyMeStmt(IRMap *irMap, MeStmt *mestmt) {
  switch (mestmt->op) {
    case OP_assertnonnull: {
      UnaryMeStmt *unarystmt = static_cast<UnaryMeStmt *>(mestmt);
      UnaryMeStmt *newunarystmt = irMap->New<UnaryMeStmt>(unarystmt);
      return newunarystmt;
    }
    case OP_dassign: {
      DassignMeStmt *dass = static_cast<DassignMeStmt *>(mestmt);
      DassignMeStmt *newdass = irMap->New<DassignMeStmt>(&irMap->irMapAlloc, dass);
      return newdass;
    }
    case OP_intrinsiccallwithtype: {
      IntrinsiccallMeStmt *intrnstmt = static_cast<IntrinsiccallMeStmt *>(mestmt);
      IntrinsiccallMeStmt *newintrnstmt = irMap->NewInPool<IntrinsiccallMeStmt>(intrnstmt);
      return newintrnstmt;
    }
    case OP_intrinsiccall: {
      IntrinsiccallMeStmt *intrnstmt = static_cast<IntrinsiccallMeStmt *>(mestmt);
      IntrinsiccallMeStmt *newintrnstmt = irMap->NewInPool<IntrinsiccallMeStmt>(intrnstmt);
      return newintrnstmt;
    }
    case OP_callassigned: {
      CallMeStmt *callass = static_cast<CallMeStmt *>(mestmt);
      CallMeStmt *newcallass = irMap->NewInPool<CallMeStmt>(callass);
      return newcallass;
    }
    default:
      CHECK_FATAL(false, "MeStmtEPre::CopyMeStmt: NYI");
  }
}

// for each variable in realz that is defined by a phi, replace it by the jth
// phi opnd; the tagged lhs is returned in the reference parameter lhsVar
MeStmt *MeStmtPre::PhiOpndFromRes4Stmt(MeRealOcc *realz, uint32 j, MeExpr *&lhsVar) {
  MeOccur *defz = realz->def;
  CHECK_FATAL(defz && (defz->occty == kOccPhiocc), "must be def by phiocc");
  MeStmt *stmtq = CopyMeStmt(irMap, realz->mestmt);
  lhsVar = realz->meexpr;
  BB *ephibb = defz->mirbb;
  switch (stmtq->op) {
    case OP_assertnonnull: {
      UnaryMeStmt *unarystmtq = static_cast<UnaryMeStmt *>(stmtq);
      MeExpr *retopnd = GetReplaceMeExpr(unarystmtq->opnd, ephibb, j);
      if (retopnd) {
        unarystmtq->opnd = retopnd;
      }
      break;
    }
    case OP_dassign: {
      DassignMeStmt *dassq = static_cast<DassignMeStmt *>(stmtq);
      MeExpr *retopnd = GetReplaceMeExpr(dassq->rhs, ephibb, j);
      if (retopnd) {
        dassq->rhs = retopnd;
      }
      retopnd = GetReplaceMeExpr(realz->meexpr, ephibb, j);
      if (retopnd) {
        lhsVar = retopnd;
      }
      break;
    }
    case OP_intrinsiccall:
    case OP_intrinsiccallwithtype:
    case OP_callassigned: {
      NaryMeStmt *nstmtq = static_cast<NaryMeStmt *>(stmtq);
      for (int32 i = 0; i < nstmtq->NumMeStmtOpnds(); i++) {
        MeExpr *retopnd = GetReplaceMeExpr(nstmtq->opnds[i], ephibb, j);
        if (retopnd) {
          nstmtq->opnds[i] = retopnd;
        }
      }
      break;
    }
    default:
      ASSERT(false, "MeStmtPre::PhiOpndFromRes4Stmt: NYI");
  }
  return stmtq;
}

void MeStmtPre::Rename2() {
  while (!rename2_set.empty()) {
    MapleSet<uint32_t>::iterator it = rename2_set.begin();
    MeRealOcc *realocc = work_cand->real_occs[*it];
    rename2_set.erase(it);
    MeOccur *defocc = realocc->def;
    CHECK_FATAL(defocc && defocc->occty == kOccPhiocc, "should be def by phiocc");
    MePhiOcc *defphiocc = static_cast<MePhiOcc *>(defocc);
    MapleVector<MePhiOpndOcc *> &phiopnds = defphiocc->phiopnds;
    for (uint32 i = 0; i < phiopnds.size(); i++) {
      MePhiOpndOcc *phioccopnd = phiopnds[i];
      if (!phioccopnd->is_processed) {
        phioccopnd->is_processed = true;
        MeExpr *varY = nullptr;
        MeStmt *stmtY = PhiOpndFromRes4Stmt(realocc, i, varY);
        stmtY->bb = phioccopnd->mirbb;
        phioccopnd->current_expr.mestmt = stmtY;  // stmt_y might be inserted at the end of the block
        MeOccur *defx = phioccopnd->def;
        if (defx == nullptr) {
          continue;
        }
        if (defx->occty == kOccReal) {
          MeRealOcc *realdefx = static_cast<MeRealOcc *>(defx);
          std::vector<MeExpr *> varvecX;
          std::vector<MeExpr *> varvecY;
          CollectVarForCand(realdefx, varvecX);
          CollectVarForMeStmt(stmtY, varY, varvecY);
          CHECK_FATAL(varvecX.size() == varvecY.size(), "");
          bool hasSameVersion = true;
          uint32 checklimit = varvecY.size();
          if (varY != nullptr) {
            if (varvecY[checklimit - 1] == static_cast<DassignMeStmt *>(stmtY)->lhs) {
              checklimit--;
            }
          }
          for (uint32 ii = 0; ii < checklimit; ii++) {
            if (varvecX[ii] != varvecY[ii]) {
              hasSameVersion = false;
            }
          }
          if (!hasSameVersion) {
            phioccopnd->def = nullptr;
            phioccopnd->has_real_use = false;
          }
        } else if (defx->occty == kOccPhiocc) {
          std::vector<MeExpr *> varvecY;
          bool alldom = true;
          CollectVarForMeStmt(stmtY, varY, varvecY);
          uint32 checklimit = varvecY.size();
          if (varY != nullptr) {
            if (varvecY[checklimit - 1] == static_cast<DassignMeStmt *>(stmtY)->lhs) {
              checklimit--;
            }
          }
          for (uint32 ii = 0; ii < checklimit; ii++) {
            if (!DefVarDominateOcc(varvecY[ii], defx)) {
              alldom = false;
            }
          }
          if (alldom) {
            // create a realocc and add to rename2 set
            MeRealOcc *occy = percand_mp->New<MeRealOcc>(stmtY, 0, varY);
            occy->position = work_cand->real_occs.size();
            work_cand->real_occs.push_back(occy);
            occy->def = defx;
            occy->classid = defx->classid;
            rename2_set.insert(occy->position);
            if (ssapredebug) {
              LogInfo::MapleLogger() << "--- rename2 adds to rename2_set manufactured ";
              occy->Dump(irMap);
              LogInfo::MapleLogger() << std::endl;
            }
          } else {
            phioccopnd->def = nullptr;
            phioccopnd->has_real_use = false;
            MePhiOcc *phidefx = static_cast<MePhiOcc *>(defx);
            phidefx->is_downsafety = false;
          }
        }
      }
    }
  }

  if (ssapredebug) {
    PreWorkCand *curCand = work_cand;
    LogInfo::MapleLogger() << "========ssapre candidate " << curCand->index << " after rename2===================\n";
    for (MeOccur *occ : all_occs) {
      occ->Dump(irMap);
      LogInfo::MapleLogger() << std::endl;
    }
  }
}

// Df phis are computed into the df_phis set; Var Phis in the var_phis set
void MeStmtPre::ComputeVarAndDfPhis() {
  varphi_dfns.clear();
  dfphi_dfns.clear();
  const MapleVector<MeRealOcc *> &realoccList = work_cand->real_occs;
  CHECK_FATAL(dominance->bbVec.size() > 0, "size to be allocated is 0");
  for (MapleVector<MeRealOcc *>::const_iterator it = realoccList.begin(); it != realoccList.end(); it++) {
    MeRealOcc *realocc = *it;
    if (realocc->occty == kOccMembar) {
      continue;
    }
    BB *defBb = realocc->mestmt->bb;
    GetIterDomFrontier(defBb, &dfphi_dfns);

    MeStmt *stmt = realocc->mestmt;
    switch (stmt->op) {
      case OP_assertnonnull: {
        UnaryMeStmt *unarystmt = static_cast<UnaryMeStmt *>(stmt);
        SetVarPhis(unarystmt->opnd);
        break;
      }
      case OP_dassign: {
        DassignMeStmt *dassmestmt = static_cast<DassignMeStmt *>(stmt);
        SetVarPhis(dassmestmt->rhs);
        SetVarPhis(realocc->meexpr);
        break;
      }
      case OP_intrinsiccall:
      case OP_intrinsiccallwithtype:
      case OP_callassigned: {
        NaryMeStmt *nstmt = static_cast<NaryMeStmt *>(stmt);
        for (int32 i = 0; i < nstmt->NumMeStmtOpnds(); i++) {
          SetVarPhis(nstmt->opnds[i]);
        }
        break;
      }
      default:
        CHECK_FATAL(false, "NYI");
    }
  }
}

// Based on ssapre->work_cand's real_occs and dfphi_dfns (which will privides all
// the inserted phis) and the mapped set in use_occur_map,
// create the phi, phiopnd occ nodes and use_occ nodes; link them all
// up in order of dtPreOrder in ssapre->all_occs; the phi occ nodes are in
// addition provided in order of dtPreOrder in ssapre->phi_occs.
// When a real_occ has uses before it in its bb, ignore (do not insert)
// the real_occ.
void MeStmtPre::CreateSortedOccs() {
  // get set of bb dfns that contain uses if candidate is dassign
  MapleSet<uint32> *useDfns;
  PreStmtWorkCand *stmtwkcand = static_cast<PreStmtWorkCand *>(work_cand);
  if (stmtwkcand->themestmt->op == OP_dassign && !stmtwkcand->lhs_is_final) {
    OStIdx oidx = static_cast<DassignMeStmt *>(stmtwkcand->themestmt)->lhs->ost->index;
    MapleMap<OStIdx, MapleSet<uint32> *>::iterator umapit = use_occur_map.find(oidx);
    CHECK_FATAL(umapit != use_occur_map.end(), "MeStmtPre::CreateSortedOccs: missing entry in use_occur_map");
    useDfns = umapit->second;
  } else {
    // create empty MapleSet<uint32> to be pointed to by use_dfns
    useDfns = percand_mp->New<MapleSet<uint32>>(percand_allocator.Adapter());
  }

  // merge varphi_dfns to dfphi_dfns
  dfphi_dfns.insert(varphi_dfns.begin(), varphi_dfns.end());
  // form phiopnd_dfns
  std::multiset<uint32> phiopndDfns;
  for (uint32 dfn : dfphi_dfns) {
    BBId bbid = dominance->dtPreOrder[dfn];
    BB *bb = GetBB(bbid);
    CHECK_FATAL(bb != nullptr, "GetBB error");
    for (BB *pred : bb->pred) {
      phiopndDfns.insert(dominance->dtDfn[pred->id.idx]);
    }
  }

  std::unordered_map<BBId, std::forward_list<MePhiOpndOcc *>> bb2phiopndMap;
  MapleVector<MeRealOcc *>::iterator realoccIt = work_cand->real_occs.begin();
  MapleVector<MeExitOcc *>::iterator exitoccIt = exit_occs.begin();
  auto phidfnIt = dfphi_dfns.begin();
  auto phiopnddfnIt = phiopndDfns.begin();
  auto usedfnIt = useDfns->begin();
  MeUseOcc *nextUseocc = nullptr;
  if (usedfnIt != useDfns->end()) {
    CHECK_FATAL(GetBB(dominance->dtPreOrder[*usedfnIt]) != nullptr, "");
    nextUseocc = percand_mp->New<MeUseOcc>(GetBB(dominance->dtPreOrder[*usedfnIt]));
  }
  MeRealOcc *nextRealocc = nullptr;
  if (realoccIt != work_cand->real_occs.end()) {
    nextRealocc = *realoccIt;
  }
  MeExitOcc *nextExitocc = nullptr;
  if (exitoccIt != exit_occs.end()) {
    nextExitocc = *exitoccIt;
  }
  MePhiOcc *nextPhiocc = nullptr;
  if (phidfnIt != dfphi_dfns.end()) {
    CHECK_FATAL(GetBB(dominance->dtPreOrder[*phidfnIt]) != nullptr, "");
    nextPhiocc = percand_mp->New<MePhiOcc>(GetBB(dominance->dtPreOrder[*phidfnIt]), &percand_allocator);
  }

  MePhiOpndOcc *nextPhiopndocc = nullptr;
  if (phiopnddfnIt != phiopndDfns.end()) {
    nextPhiopndocc = percand_mp->New<MePhiOpndOcc>(GetBB(dominance->dtPreOrder[*phiopnddfnIt]));
    std::unordered_map<BBId, std::forward_list<MePhiOpndOcc *>>::iterator it =
      bb2phiopndMap.find(dominance->dtPreOrder[*phiopnddfnIt]);
    if (it == bb2phiopndMap.end()) {
      std::forward_list<MePhiOpndOcc *> newlist = { nextPhiopndocc };
      CHECK(*phiopnddfnIt < dominance->dtPreOrder.size(), "index out of range in SSAPre::CreateSortedOccs");
      bb2phiopndMap[dominance->dtPreOrder[*phiopnddfnIt]] = newlist;
    } else {
      it->second.push_front(nextPhiopndocc);
    }
  }

  bool realoccInserted = false;
  MeOccur *pickedocc;  // the next picked occ in order of preorder traveral of dominator tree
  do {
    pickedocc = nullptr;
    // the 5 kinds of occ must be checked in this order, so it will be right
    // if more than 1 has the same dfn
    if (nextPhiocc) {
      pickedocc = nextPhiocc;
    }
    if (nextRealocc && (pickedocc == nullptr || dominance->dtDfn[nextRealocc->mirbb->id.idx] <
                                                  dominance->dtDfn[pickedocc->mirbb->id.idx])) {
      pickedocc = nextRealocc;
    }
    if (nextUseocc && (pickedocc == nullptr ||
                       dominance->dtDfn[nextUseocc->mirbb->id.idx] < dominance->dtDfn[pickedocc->mirbb->id.idx])) {
      pickedocc = nextUseocc;
    }
    if (nextExitocc && (pickedocc == nullptr || dominance->dtDfn[nextExitocc->mirbb->id.idx] <
                                                  dominance->dtDfn[pickedocc->mirbb->id.idx])) {
      pickedocc = nextExitocc;
    }
    if (nextPhiopndocc && (pickedocc == nullptr || *phiopnddfnIt < dominance->dtDfn[pickedocc->mirbb->id.idx])) {
      pickedocc = nextPhiopndocc;
    }

    if (pickedocc != nullptr) {
      all_occs.push_back(pickedocc);
      switch (pickedocc->occty) {
        case kOccUse: {
          // get the next use occ
          usedfnIt++;
          if (usedfnIt != useDfns->end()) {
            CHECK_FATAL(GetBB(dominance->dtPreOrder[*usedfnIt]) != nullptr, "");
            nextUseocc = percand_mp->New<MeUseOcc>(GetBB(dominance->dtPreOrder[*usedfnIt]));
          } else {
            nextUseocc = nullptr;
          }
          break;
        }
        case kOccReal:
        case kOccMembar:
          // get the next real occ
          realoccIt++;
          if (realoccIt != work_cand->real_occs.end()) {
            nextRealocc = *realoccIt;
          } else {
            nextRealocc = nullptr;
          }
          realoccInserted = true;
          break;
        case kOccExit:
          exitoccIt++;
          if (exitoccIt != exit_occs.end()) {
            nextExitocc = *exitoccIt;
          } else {
            nextExitocc = nullptr;
          }
          break;
        case kOccPhiocc:
          phi_occs.push_back(static_cast<MePhiOcc *>(pickedocc));
          phidfnIt++;
          if (phidfnIt != dfphi_dfns.end()) {
            CHECK_FATAL(GetBB(dominance->dtPreOrder[*phidfnIt]) != nullptr,
                   "GetBB return null in SSAPre::CreateSortedOccs");
            nextPhiocc = percand_mp->New<MePhiOcc>(GetBB(dominance->dtPreOrder[*phidfnIt]), &percand_allocator);
          } else {
            nextPhiocc = nullptr;
          }
          break;
        case kOccPhiopnd:
          phiopnddfnIt++;
          if (phiopnddfnIt != phiopndDfns.end()) {
            nextPhiopndocc = percand_mp->New<MePhiOpndOcc>(GetBB(dominance->dtPreOrder[*phiopnddfnIt]));
            std::unordered_map<BBId, std::forward_list<MePhiOpndOcc *>>::iterator it =
              bb2phiopndMap.find(dominance->dtPreOrder[*phiopnddfnIt]);
            if (it == bb2phiopndMap.end()) {
              std::forward_list<MePhiOpndOcc *> newlist = { nextPhiopndocc };
              bb2phiopndMap[dominance->dtPreOrder[*phiopnddfnIt]] = newlist;
            } else {
              it->second.push_front(nextPhiopndocc);
            }
          } else {
            nextPhiopndocc = nullptr;
          }
          break;
        default:
          ASSERT(false, "CreateSortedOccs: unexpected occty");
      }
    }
  } while (pickedocc != nullptr);

  // if no real occ inserted, no more work for this work_cand
  if (!realoccInserted) {
    work_cand->real_occs.clear();
  }

  // initialize phiopnds vector in each MePhiOcc node and  def_phiocc field in
  // each MePhiOpndOcc node
  for (MePhiOcc *phiocc : phi_occs)
    for (BB *pred : phiocc->mirbb->pred) {
      MePhiOpndOcc *phiopndocc = bb2phiopndMap[pred->id].front();
      phiocc->phiopnds.push_back(phiopndocc);
      phiopndocc->def_phiocc = phiocc;
      bb2phiopndMap[pred->id].pop_front();
    }

  if (ssapredebug) {
    LogInfo::MapleLogger() << "========ssapre candidate " << work_cand->index << " after phi insert===================\n";
    for (MeOccur *occ : all_occs) {
      occ->Dump(irMap);
      LogInfo::MapleLogger() << std::endl;
    }
  }
}

void MeStmtPre::ConstructUseOccurMapExpr(uint32 bbdfn, MeExpr *x) {
  if (x->meOp == kMeOpVar) {
    OStIdx oidx = static_cast<VarMeExpr *>(x)->ost->index;
    MapleMap<OStIdx, MapleSet<uint32> *>::iterator mapit;
    mapit = use_occur_map.find(oidx);
    if (mapit == use_occur_map.end()) {
      return;
    }
    MapleSet<uint32> *bbdfnSet = mapit->second;
    bbdfnSet->insert(bbdfn);
    return;
  }
  for (int32 i = 0; i < x->NumMeExprOpnds(); i++) {
    ConstructUseOccurMapExpr(bbdfn, x->GetOpnd(i));
  }
}

void MeStmtPre::ConstructUseOccurMap() {
  for (PreWorkCand *wkcand : worklist) {
    PreStmtWorkCand *stmtwkcand = static_cast<PreStmtWorkCand *>(wkcand);
    if (stmtwkcand->themestmt->op != OP_dassign) {
      continue;
    }
    if (stmtwkcand->lhs_is_final) {
      continue;
    }
    OStIdx oidx = static_cast<DassignMeStmt *>(stmtwkcand->themestmt)->lhs->ost->index;
    if (use_occur_map.find(oidx) == use_occur_map.end()) {
      // add an entry for oidx
      use_occur_map[oidx] = ssapre_mp->New<MapleSet<uint32>>(ssapre_allocator.Adapter());
    }
  }
  // do a pass over the program
  const MapleVector<BBId> &preorderDt = dominance->dtPreOrder;
  for (uint32 i = 0; i < preorderDt.size(); i++) {
    BB *bb = func->bbVec[preorderDt[i].idx];
    for (auto stmt : bb->meStmtList) {
      for (int32 j = 0; j < stmt->NumMeStmtOpnds(); j++) {
        ConstructUseOccurMapExpr(i, stmt->GetMeStmtOpnd(j));
      }
    }
  }
}

// create a new realocc based on mestmt
PreStmtWorkCand *MeStmtPre::CreateStmtRealOcc(MeStmt *mestmt, int seqstmt) {
  uint32 hidx = PreStmtWorkCand::ComputeStmtWorkCandHashIndex(mestmt);
  PreStmtWorkCand *wkcand = static_cast<PreStmtWorkCand *>(PreWorkCand::workcandHashTable[hidx]);
  while (wkcand != nullptr) {
    MeStmt *x = wkcand->themestmt;
    ASSERT(x != nullptr, "CreateStmtRealOcc: found workcand with themestmt as nullptr");
    if (x->IsTheSameWorkcand(mestmt)) {
      break;
    }
    wkcand = static_cast<PreStmtWorkCand *>(wkcand->next);
  }
  MeExpr *meexpr = nullptr;
  if (mestmt->op == OP_dassign) {
    DassignMeStmt *dass = static_cast<DassignMeStmt *>(mestmt);
    MapleStack<VarMeExpr *> *pstack = versionStackVec.at(dass->lhs->ost->index.idx);
    meexpr = pstack->top();
  }
  MeRealOcc *newocc = ssapre_mp->New<MeRealOcc>(mestmt, seqstmt, meexpr);
  if (wkcand != nullptr) {
    wkcand->AddRealOccAsLast(newocc, GetPuidx(mestmt));
    return wkcand;
  }
  // workcand not yet created; create a new one and add to worklist
  wkcand = ssapre_mp->New<PreStmtWorkCand>(&ssapre_allocator, worklist.size(), mestmt, GetPuidx(mestmt));
  wkcand->has_local_opnd = true;  // dummy
  worklist.push_back(wkcand);
  wkcand->AddRealOccAsLast(newocc, GetPuidx(mestmt));
  // add to bucket at workcandHashTable[hidx]
  wkcand->next = PreWorkCand::workcandHashTable[hidx];
  PreWorkCand::workcandHashTable[hidx] = wkcand;
  return wkcand;
}

void MeStmtPre::VersionStackChiListUpdate(const MapleMap<OStIdx, ChiMeNode *> &chilist) {
  for (MapleMap<OStIdx, ChiMeNode *>::const_iterator it = chilist.begin(); it != chilist.end(); it++) {
    const OriginalSt *ost = it->second->lhs->ost;
    if (!ost->IsSymbol() || ost->indirectLev != 0) {
      continue;
    }
    MapleStack<VarMeExpr *> *pstack = versionStackVec.at(it->second->lhs->ost->index.idx);
    pstack->push(it->second->lhs);
  }
}

// verify that there is no prior use of lhsVar before stmt in its BB
static bool NoPriorUseInBB(const ScalarMeExpr *lhsVar, MeStmt *defStmt) {
  for (MeStmt *stmt = defStmt->prev; stmt != nullptr; stmt = stmt->prev) {
    for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
      CHECK_FATAL(stmt->GetMeStmtOpnd(i), "null ptr check");
      if (stmt->GetMeStmtOpnd(i)->SymAppears(lhsVar->ost->index)) {
        return false;
      }
    }
  }
  return true;
}

void MeStmtPre::BuildWorkListBB(BB *bb) {
  if (bb == nullptr) {
    return;
  }

  // record stack size for variable versions, used for stack pop up at return
  std::vector<uint32> curStackSizeVec;
  curStackSizeVec.resize(versionStackVec.size());
  for (uint32 i = 1; i < versionStackVec.size(); i++) {
    if (versionStackVec[i] == nullptr) {
      continue;
    }
    curStackSizeVec[i] = versionStackVec[i]->size();
  }

  // traverse var phi nodes to update versionStack
  MapleMap<OStIdx, MePhiNode *> &mePhiList = bb->mePhiList;
  for (std::pair<OStIdx, MePhiNode *> phiEntry : mePhiList) {
    MePhiNode *phimenode = phiEntry.second;
    const OriginalSt *ost = phimenode->lhs->ost;
    if (!ost->IsSymbol() || ost->indirectLev != 0) {
      continue;
    }
    MapleStack<VarMeExpr *> *pstack = versionStackVec.at(phimenode->lhs->ost->index.idx);
    pstack->push(static_cast<VarMeExpr*>(phimenode->lhs));
  }

  // traverse statements
  uint32 seqStmt = 0;
  MeStmt *nextstmt = nullptr;
  for (auto stmt : bb->meStmtList) {
    nextstmt = stmt->next;
    seqStmt++;
    switch (stmt->op) {
      case OP_jstry:
      case OP_jscatch:
      case OP_finally:
      case OP_endtry:
      case OP_cleanuptry:
      case OP_try:
      case OP_javatry:
      case OP_cpptry:
      case OP_catch:
      case OP_javacatch:
      case OP_cppcatch:
      case OP_goto:
      case OP_comment:
      case OP_brtrue:
      case OP_brfalse:
      case OP_switch:
        break;

      case OP_membaracquire:
      case OP_membarrelease:
      case OP_membarstoreload:
      case OP_membarstorestore:
        CreateMembarOcc(stmt, seqStmt);
        break;

      case OP_gosub:
      case OP_retsub:
      case OP_throw:
      case OP_return:
        //    CreateExitOcc(bb);
        break;

      case OP_iassign: {
        IassignMeStmt *iass = static_cast<IassignMeStmt *>(stmt);
        VersionStackChiListUpdate(iass->chiList);
        break;
      }
      case OP_maydassign: {
        MaydassignMeStmt *maydstmt = static_cast<MaydassignMeStmt *>(stmt);
        VersionStackChiListUpdate(maydstmt->chiList);
        break;
      }
      case OP_regassign:
      case OP_eval:
      case OP_decref:
      case OP_decrefreset:
      case OP_incref:
      case OP_free:
      case OP_syncenter:
      case OP_syncexit:
      case OP_assertlt:
      case OP_assertge:
        break;

      case OP_call:
      case OP_virtualcall:
      case OP_virtualicall:
      case OP_superclasscall:
      case OP_interfacecall:
      case OP_interfaceicall:
      case OP_customcall:
      case OP_polymorphiccall:
      case OP_virtualcallassigned:
      case OP_virtualicallassigned:
      case OP_superclasscallassigned:
      case OP_interfacecallassigned:
      case OP_interfaceicallassigned:
      case OP_customcallassigned:
      case OP_polymorphiccallassigned: {
        CallMeStmt *callmestmt = static_cast<CallMeStmt *>(stmt);
        VersionStackChiListUpdate(callmestmt->chiList);
        break;
      }

      case OP_icall:
      case OP_icallassigned: {
        IcallMeStmt *icallmestmt = static_cast<IcallMeStmt *>(stmt);
        VersionStackChiListUpdate(icallmestmt->chiList);
        break;
      }

      case OP_xintrinsiccall:
      case OP_intrinsiccallassigned:
      case OP_xintrinsiccallassigned:
      case OP_intrinsiccallwithtypeassigned: {
        IntrinsiccallMeStmt *intrinstmt = static_cast<IntrinsiccallMeStmt *>(stmt);
        VersionStackChiListUpdate(intrinstmt->chiList);
        break;
      }

      case OP_assertnonnull: {
        UnaryMeStmt *unarystmt = static_cast<UnaryMeStmt *>(stmt);
        if (!unarystmt->opnd->IsLeaf()) {
          break;
        }
        CreateStmtRealOcc(stmt, seqStmt);
        break;
      }

      case OP_dassign: {
        DassignMeStmt *dassmestmt = static_cast<DassignMeStmt *>(stmt);
        if (!MeOption::dassignpre || !dassmestmt->rhs->IsLeaf() ||
            !dassmestmt->chiList.empty() || dassmestmt->NeedIncref() ||
            (dassmestmt->rhs->op == OP_regread &&
             static_cast<RegMeExpr *>(dassmestmt->rhs)->regIdx == -kSregThrownval)) {
          // update version stacks
          MapleStack<VarMeExpr *> *pstack = versionStackVec.at(dassmestmt->lhs->ost->index.idx);
          pstack->push(dassmestmt->GetVarLhs());
          VersionStackChiListUpdate(dassmestmt->chiList);
          break;
        }
        ScalarMeExpr *varmeexpr = dassmestmt->lhs;
        const OriginalSt *ost = varmeexpr->ost;
        if (ost->isFinal) {
          PreStmtWorkCand *stmtwkcand = CreateStmtRealOcc(stmt, seqStmt);
          stmtwkcand->lhs_is_final = true;
        } else if (!dassmestmt->rhs->SymAppears(varmeexpr->ost->index) && dassmestmt->rhs->Pure() && !ost->IsSymRenamed()) {
          if (NoPriorUseInBB(dassmestmt->lhs, stmt)) {
            CreateStmtRealOcc(stmt, seqStmt);
          }
        } else if (dassmestmt->lhs->IsUseSameSymbol(dassmestmt->rhs)) {
          RemoveUnecessaryDassign(dassmestmt);
        }
        // update version stacks
        MapleStack<VarMeExpr *> *pstack = versionStackVec.at(dassmestmt->lhs->ost->index.idx);
        pstack->push(dassmestmt->GetVarLhs());
        VersionStackChiListUpdate(dassmestmt->chiList);
        break;
      }

      case OP_intrinsiccallwithtype: {
        IntrinsiccallMeStmt *intrnstmt = static_cast<IntrinsiccallMeStmt *>(stmt);
        VersionStackChiListUpdate(intrnstmt->chiList);
        if (!MeOption::clinitpre) {
          VersionStackChiListUpdate(intrnstmt->chiList);
          break;
        }
        if (intrnstmt->intrinsic == INTRN_JAVA_CLINIT_CHECK) {
          CreateStmtRealOcc(stmt, seqStmt);
        }
        break;
      }

      case OP_intrinsiccall: {
        IntrinsiccallMeStmt *intrnstmt = static_cast<IntrinsiccallMeStmt *>(stmt);
        bool allOperandsAreLeaf = true;
        for (int32 i = 0; i < intrnstmt->NumMeStmtOpnds(); i++) {
          if (!intrnstmt->opnds[i]->IsLeaf()) {
            allOperandsAreLeaf = false;
            break;
          }
        }
        if (!allOperandsAreLeaf) {
          break;
        }
        if (intrnstmt->intrinsic == INTRN_MPL_BOUNDARY_CHECK) {
          CreateStmtRealOcc(stmt, seqStmt);
        }
        VersionStackChiListUpdate(intrnstmt->chiList);
        break;
      }

      case OP_callassigned: {
        CallMeStmt *callass = static_cast<CallMeStmt *>(stmt);
        VersionStackChiListUpdate(callass->chiList);
        if (!MeOption::assign2finalpre) {
          break;
        }
        MIRFunction *callee = GlobalTables::GetFunctionTable().funcTable[callass->puIdx];
        MIRSymbol *calleeSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(callee->stIdx.Idx());
        const std::string &calleename = calleeSt->GetName();
        if (kStmtpreWhiteList.find(calleename) == kStmtpreWhiteList.end()) {
          break;
        }
        bool allOperandsAreLeaf = true;
        for (MeExpr *o : callass->opnds)
          if (!o->IsLeaf()) {
            allOperandsAreLeaf = false;
            break;
          }
        if (!allOperandsAreLeaf) {
          break;
        }
        if (callass->mustDefList.empty()) {
          CreateStmtRealOcc(stmt, seqStmt);
          break;
        }
        VarMeExpr *varmeexpr = dynamic_cast<VarMeExpr *>(callass->mustDefList.front().lhs);
        CHECK_FATAL(varmeexpr != nullptr, "null ptr check");
        const OriginalSt *ost = varmeexpr->ost;
        MIRSymbol *sym = ost->GetMIRSymbol();
        if (ost->isFinal || sym->GetName().substr(0, 6) == "L_STR_") {
          CreateStmtRealOcc(stmt, seqStmt);
        }
        break;
      }

      default:
        ASSERT(stmt->op == OP_comment, "");
        break;
    }

    if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
      // update version stacks
      MapleVector<MustDefMeNode> *mustdefList = stmt->GetMustDefList();
      if (!mustdefList->empty()) {
        MeExpr *melhs = mustdefList->front().lhs;
        if (melhs->meOp == kMeOpVar) {
          VarMeExpr *lhsVar = static_cast<VarMeExpr *>(melhs);
          MapleStack<VarMeExpr *> *pstack = versionStackVec.at(lhsVar->ost->index.idx);
          pstack->push(lhsVar);
        }
      }
    }
  }
  if (bb->IsExit() || bb->WontExit()) {
    CreateExitOcc(bb);
  }

  // recurse on child BBs in dominator tree
  MapleSet<BBId> *domChildren = &dominance->domChildren[bb->id.idx];
  for (MapleSet<BBId>::iterator bbit = domChildren->begin(); bbit != domChildren->end(); bbit++) {
    BBId childbbid = *bbit;
    BuildWorkListBB(GetBB(childbbid));
  }

  // pop the stacks back to their levels on entry
  for (uint32 i = 1; i < versionStackVec.size(); i++) {
    MapleStack<VarMeExpr *> *pstack = versionStackVec[i];
    if (pstack == nullptr) {
      continue;
    }
    uint32 curSize = curStackSizeVec[i];
    while (pstack->size() > curSize) {
      pstack->pop();
    }
  }
}

void MeStmtPre::BuildWorkList() {
  // initialize version stack
  const MapleVector<OriginalSt *> &originalStVec = ssaTab->originalStTable.original_st_vector_;
  for (uint32 i = 1; i < originalStVec.size(); i++) {
    OriginalSt *ost = originalStVec[i];
    if (!ost->IsSymbol() || ost->indirectLev != 0) {
      continue;
    }
    MapleStack<VarMeExpr *> *versStack = ssapre_mp->New<MapleStack<VarMeExpr *>>(ssapre_allocator.Adapter());
    versStack->push(
      static_cast<VarMeExpr *>(irMap->GetOrCreateZeroVersionVarMeExpr(ost)));
    versionStackVec[ost->index.idx] = versStack;
  }

  BuildWorkListBB(func->commonEntryBB);
}

void MeStmtPre::RemoveUnecessaryDassign(DassignMeStmt *dssmestmt) {
  BB *bb = dssmestmt->bb;
  bb->RemoveMeStmt(dssmestmt);
  OStIdx ostidx = dssmestmt->lhs->ost->index;
  MapleSet<BBId> *bbset = nullptr;
  if (cands_for_ssaupdate.find(ostidx) == cands_for_ssaupdate.end()) {
    bbset = ssapre_mp->New<MapleSet<BBId>>(std::less<BBId>(), ssapre_allocator.Adapter());
    cands_for_ssaupdate[ostidx] = bbset;
  } else {
    bbset = cands_for_ssaupdate[ostidx];
  }
  bbset->insert(bb->id);
}

AnalysisResult *MeDoStmtPre::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
  ASSERT(dom != nullptr, "dominance phase has problem");

  MeIRMap *irMap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func));
  ASSERT(irMap != nullptr, "irMap phase has problem");

  MemPool *ssapremp = mempoolctrler.NewMemPool(PhaseName().c_str());
  MemPool *percandmp = mempoolctrler.NewMemPool("Per STMTPRE Candidate");

  MeStmtPre ssapre(func, irMap, dom, ssapremp, percandmp, MeOption::stmtprePULimit);

  MIRFunction *mirfunction = func->mirFunc;
  if (DEBUGFUNC(func)) {
    ssapre.ssapredebug = true;
  }

  ssapre.ApplySSAPRE();

  if (!ssapre.cands_for_ssaupdate.empty()) {
    SSAUpdate ssaupdate(func, func->meSSATab, dom, &ssapre.cands_for_ssaupdate);
    ssaupdate.Run();
  }

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============== STMTPRE =============" << std::endl;
    irMap->Dump();
  }

  mempoolctrler.DeleteMemPool(ssapremp);
  mempoolctrler.DeleteMemPool(percandmp);

  return nullptr;
}

}  // namespace maple
