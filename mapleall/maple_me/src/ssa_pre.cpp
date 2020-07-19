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

#include "dominance.h"
#include "ssa_pre.h"
#include "mir_builder.h"

namespace maple {

// Implementation of SSAPRE based on the paper "Partial Redundancy Elimination
// in SSA Form" Kennedy et al.
//
// This file represents the base SSAPRE implementation.  There are different
// variants of SSAPRE-based optimizations, and they are derived from this base
// implementation and implemented in additional files.  The optimizations are
// and the files they are implemented in are:
// 1. EPRE (PRE for Expressions) - ssa_epre.cpp
//    Because EPRE are deployed in both mapleme and maplewpo, ssa_epre.cpp
//    contains code shared by them.  Code specific to mapleme and maplewpo are
//    implemented in me_ssa_epre.cpp and wpo_ssa_epre.cpp respectively.
// 2. LPRE (PRE for Loads) - me_ssa_lpre.cpp (only deployed in mapleme).
// 3. STMTPRE (PRE for Statements) - me_stmt_pre.cpp (only deployed in mapleme).
// 4. STMTFRE (Full Redundancy Elimination for Statements) - me_stmt_fre.cpp
//    (called when performing STMTPRE).

// ================ Step 6: Code Motion =================

ScalarMeExpr *SSAPre::CreateNewCurTemp(MeExpr *meexpr) {
  if (work_cand->needlocalrefvar && placementrc_on) {
    cur_temp = CreateNewCurLocalrefvar();
    return cur_temp;
  }

  if (cur_temp) {
    // only need to create a new version
    if (cur_temp->meOp == kMeOpReg) {
      RegMeExpr *regvar = irMap->CreateRegMeExprVersion(static_cast<RegMeExpr *>(cur_temp));
      return regvar;
    } else {
      VarMeExpr *tempvar = irMap->CreateVarMeExprVersion(static_cast<VarMeExpr *>(cur_temp));
      return tempvar;
    }
  } else {
    // allocate a new temp
    if (work_cand->puIdx != 0) {
      SetCurFunction(work_cand->puIdx);  // this is for maplewpo only
      RegMeExpr *regvar = irMap->CreateRegMeExpr(meexpr);
      cur_temp = regvar;
      if (prekind == kLoadPre) {
        irMap->lpreTemps[static_cast<VarMeExpr *>(meexpr)->ost->index] = regvar;
      }
      return regvar;
    } else {
      cur_temp = irMap->CreateNewVar(NewTempStridx(), meexpr->primType, true);
      return cur_temp;
    }
  }
}

VarMeExpr *SSAPre::CreateNewCurLocalrefvar() {
  if (cur_localrefvar) {
    // only need to create a new version
    VarMeExpr *tempvar = irMap->CreateVarMeExprVersion(static_cast<VarMeExpr *>(cur_localrefvar));
    return tempvar;
  } else {
    // allocate a new temp
    IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(work_cand->themeexpr);
    cur_localrefvar = irMap->CreateNewLocalrefvarTemp(NewTempStridx(), ivarmeexpr->tyIdx);
    OriginalSt *ost = cur_localrefvar->ost;
    ost->epreLocalrefvar = true;
    addedNewLocalrefvars = true;
    return cur_localrefvar;
  }
}

void SSAPre::GenerateSaveInsertedocc(MeInsertedOcc *insertedocc) {
  ASSERT(GetPuidx(insertedocc->mirbb) == work_cand->puIdx || work_cand->puIdx == 0,
          "GenerateSaveInsertedocc: inconsistent puIdx");
  ScalarMeExpr *regorvar = CreateNewCurTemp(insertedocc->meexpr);
  AssignMeStmt *newmestmt = nullptr;
  if (!work_cand->needlocalrefvar || placementrc_on) {
    newmestmt = irMap->CreateAssignMeStmt(regorvar, insertedocc->meexpr, insertedocc->mirbb);
    regorvar->SetDefByStmt(newmestmt);
    insertedocc->mirbb->InsertMeStmtLastBr(newmestmt);
    insertedocc->saved_expr = regorvar;
  } else {
    // regorvar is MeOp_reg and lcoalrefvar is kMeOpVar
    VarMeExpr *localrefvar = CreateNewCurLocalrefvar();
    temp2localrefvar_map[static_cast<RegMeExpr *>(regorvar)] = localrefvar;
    // localrefvar = meexpr;
    newmestmt = irMap->CreateAssignMeStmt(localrefvar, insertedocc->meexpr, insertedocc->mirbb);
    localrefvar->SetDefByStmt(newmestmt);
    insertedocc->mirbb->InsertMeStmtLastBr(newmestmt);
    EnterCandsForSsaupdate(localrefvar->ost->index, insertedocc->mirbb);

    // reg = localrefvar
    newmestmt = irMap->CreateAssignMeStmt(regorvar, localrefvar, insertedocc->mirbb);
    regorvar->SetDefByStmt(newmestmt);
    insertedocc->mirbb->InsertMeStmtLastBr(newmestmt);
    insertedocc->saved_expr = regorvar;
  }
}

void SSAPre::GenerateSavePhiocc(MePhiOcc *phiocc) {
  // generate a regorvar
  ASSERT(GetPuidx(phiocc->mirbb) == work_cand->puIdx || work_cand->puIdx == 0,
          "GenerateSavePhiocc: inconsistent puIdx");
  MeExpr *regorvar = CreateNewCurTemp(work_cand->themeexpr);
  if (dynamic_cast<ScalarMeExpr*>(regorvar)) {
    MePhiNode *phiNode = irMap->CreateMePhi(static_cast<ScalarMeExpr*>(regorvar));
    phiNode->defBB = phiocc->mirbb;
    if (dynamic_cast<VarMeExpr*>(regorvar)) {
      phiocc->varPhi = phiNode;
    } else {
      phiocc->regPhi = phiNode;
    }
  }
  // update the phi opnds later
  if (work_cand->needlocalrefvar && !placementrc_on) {
    VarMeExpr *localrefvar = CreateNewCurLocalrefvar();
    temp2localrefvar_map[static_cast<RegMeExpr *>(regorvar)] = localrefvar;
    // create a var phi
    MePhiNode *phivar = irMap->CreateMePhi(localrefvar);
    phivar->defBB = phiocc->mirbb;
    phiocc->varPhi = phivar;
    EnterCandsForSsaupdate(localrefvar->ost->index, phiocc->mirbb);
    // update the phi opnds later
  }
}

void SSAPre::UpdateInsertedPhioccOpnd() {
  for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
    MePhiOcc *phiocc = *it;
    if (phiocc->IsWillBeAvail() && !phiocc->is_removed) {
      if (phiocc->regPhi) {
        MePhiNode *phireg = phiocc->regPhi;
        const MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
        for (uint32 i = 0; i < phiopnds.size(); i++) {
          RegMeExpr *regopnd = static_cast<RegMeExpr *>(phiopnds[i]->phiopnd4temp);
          if (regopnd == nullptr) {
            // create a zero version
            CHECK_FATAL(cur_temp != nullptr, "cur_temp can't be null in SSAPre::UpdateInsertedPhioccOpnd");
            regopnd = irMap->CreateRegMeExprVersion(static_cast<RegMeExpr *>(cur_temp));
          }
          phireg->opnds.push_back(regopnd);
        }
        phiocc->mirbb->mePhiList.insert(make_pair(phireg->opnds[0]->ost->index, phireg));
        if (work_cand->needlocalrefvar && phiocc->varPhi) {
          MePhiNode *phivar = phiocc->varPhi;
          const MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
          for (uint32 i = 0; i < phiopnds.size(); i++) {
            RegMeExpr *regopnd = static_cast<RegMeExpr *>(phiopnds[i]->phiopnd4temp);
            VarMeExpr *localrefvaropnd = nullptr;
            if (regopnd == nullptr) {
              // create a zero version
              CHECK_FATAL(cur_localrefvar != nullptr, "null ptr check");
              OriginalSt *ost = cur_localrefvar->ost;
              localrefvaropnd = irMap->GetOrCreateZeroVersionVarMeExpr(ost);
            } else {
              MapleMap<RegMeExpr *, VarMeExpr *>::iterator mapit = temp2localrefvar_map.find(regopnd);
              if (mapit == temp2localrefvar_map.end()) {
                OriginalSt *ost = cur_localrefvar->ost;
                localrefvaropnd = irMap->GetOrCreateZeroVersionVarMeExpr(ost);
              } else {
                localrefvaropnd = mapit->second;
              }
            }
            phivar->opnds.push_back(localrefvaropnd);
          }
          phiocc->mirbb->mePhiList.insert(make_pair(phivar->opnds[0]->ost->index, phivar));
        }
      } else {
        MePhiNode *phivar = phiocc->varPhi;
        const MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
        for (uint32 i = 0; i < phiopnds.size(); i++) {
          // CHECK_FATAL(false, "NYI");
          // phivar->opnds.push_back(varopnd);
          VarMeExpr *varopnd = static_cast<VarMeExpr *>(phiopnds[i]->phiopnd4temp);
          if (varopnd == nullptr) {
            CHECK_FATAL(cur_temp != nullptr, "cur_temp can't be null in SSAPre::UpdateInsertedPhioccOpnd");
            varopnd = irMap->CreateVarMeExprVersion(static_cast<VarMeExpr *>(cur_temp));
          }
          phivar->opnds.push_back(varopnd);
        }
        phiocc->mirbb->mePhiList.insert(make_pair(phivar->opnds[0]->ost->index, phivar));
      }
    }
  }
}

void SSAPre::CodeMotion() {
  std::set<MeStmt *> needRepairInjuringDefs; // for marking injuring defs that have been repaired

  if (work_cand->isSRCand) {  // pre-pass needed by strength reduction
    for (MeOccur *occ : all_occs) {
      switch (occ->occty) {
        case kOccReal: {
          MeRealOcc *realocc = static_cast<MeRealOcc *>(occ);
          if (realocc->is_reload) {
            SRSetNeedRepair(realocc, &needRepairInjuringDefs);
          }
          break;
        }
        case kOccPhiopnd: {
          MePhiOpndOcc *phiopnd = static_cast<MePhiOpndOcc *>(occ);
          if (phiopnd->def_phiocc->is_removed || !phiopnd->def_phiocc->IsWillBeAvail()) {
            break;
          }
          if (phiopnd->def->occty == kOccInserted) {
            break;
          }
          SRSetNeedRepair(phiopnd, &needRepairInjuringDefs);
          break;
        }
        default: ;
      }
    }
  }

  cur_temp = nullptr;
  cur_localrefvar = nullptr;
  temp2localrefvar_map.clear();
  rebuilt_occ_index = worklist.size();  // so we know the elements added due to rebuilding
  std::set<MeStmt *> repairedInjuringDefs;   // for marking injuring defs that have been repaired

  for (MeOccur *occ : all_occs) {
    switch (occ->occty) {
      case kOccReal: {
        MeRealOcc *realocc = static_cast<MeRealOcc *>(occ);
        if (realocc->is_save) {
          CHECK_FATAL(realocc->is_reload == false, "");
          GenerateSaveRealocc(realocc);
        } else if (realocc->is_reload) {
          if (!work_cand->isSRCand) {
            GenerateReloadRealocc(realocc);
          } else {
            MeExpr *regorvar = SRRepairInjuries(realocc, &needRepairInjuringDefs, &repairedInjuringDefs);
            // replace realocc->mestmt's occ with regorvar
            bool isreplaced = irMap->ReplaceMeExprStmt(realocc->mestmt, realocc->meexpr, regorvar);
            // update worklist
            if (isreplaced) {
              BuildWorkListStmt(realocc->mestmt, realocc->seq, true, regorvar);
            }
          }
        }
        break;
      }
      case kOccPhiopnd: {
        MePhiOpndOcc *phiopnd = static_cast<MePhiOpndOcc *>(occ);
        if (phiopnd->def_phiocc->is_removed || !phiopnd->def_phiocc->IsWillBeAvail()) {
          break;
        }
        MeOccur *defocc = phiopnd->def;
        if (defocc->occty == kOccInserted) {
          // generate a save of the result in to a new version of t
          if (!phiopnd->is_phiopndreload) {
            GenerateSaveInsertedocc(static_cast<MeInsertedOcc *>(defocc));
            phiopnd->phiopnd4temp = static_cast<MeInsertedOcc *>(defocc)->saved_expr;
          }
        } else {
          // set phiopnd4temp needed in the temp's phi
          if (work_cand->isSRCand) {
            phiopnd->phiopnd4temp = SRRepairInjuries(phiopnd, &needRepairInjuringDefs, &repairedInjuringDefs);
          } else {
            if (defocc->occty == kOccReal) {
              phiopnd->phiopnd4temp = static_cast<MeRealOcc *>(defocc)->saved_expr;
            } else {
              MePhiOcc *defphiocc = static_cast<MePhiOcc *>(defocc);
              MePhiNode *scalarPhi = (defphiocc->regPhi ? defphiocc->regPhi : defphiocc->varPhi);
              phiopnd->phiopnd4temp = scalarPhi->lhs;
            }
          }
        }
        break;
      }
      case kOccPhiocc: {
        MePhiOcc *phiocc = static_cast<MePhiOcc *>(occ);
        if (phiocc->is_removed || !phiocc->IsWillBeAvail()) {
          break;
        };
        GenerateSavePhiocc(phiocc);
        break;
      }
      case kOccExit:
        break;
      case kOccMembar:
      case kOccUse:
        break;
      default:
        ASSERT(false, "");
    }
  }

  // update the inserted phiocc's operand
  UpdateInsertedPhioccOpnd();

  if (ssapredebug) {
    LogInfo::MapleLogger() << "========ssapre candidate " << work_cand->index << " after CodeMotion ===================\n";
    if (cur_temp) {
      LogInfo::MapleLogger() << "cur_temp is";
      cur_temp->Dump(irMap, 0);
      LogInfo::MapleLogger() << "\n";
    }
  }
}

// ================ Step 5: Finalize =================

void SSAPre::Finalize1() {
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
              CHECK_FATAL(!work_cand->redo2handle_crit_edges, "Finalize1: insertion at critical edge, aborting");
              work_cand->redo2handle_crit_edges = true;
              if (ssapredebug)
                LogInfo::MapleLogger() << "<<<<< Re-doing this candidate due to existence of critical edge >>>>>\n";
              return;
            }
            MeExpr *insertedExpr = phiopnd->GetCurrentMeExpr();
            ASSERT(insertedExpr != nullptr, "NYI");
            MeInsertedOcc *insertedOcc =
              percand_mp->New<MeInsertedOcc>(insertedExpr, static_cast<MeStmt*>(nullptr), phiopnd->mirbb);
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
        break;
      case kOccMembar:
      case kOccUse:
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

// set save the real occurrence of definition to register
// set the PHI occurence to be needed (extraneous being false)
void SSAPre::SetSave(MeOccur *defx) {
  CHECK_FATAL(defx, "");
  if (defx->occty == kOccReal) {
    MeRealOcc *realocc = static_cast<MeRealOcc *>(defx);
    realocc->is_save = true;
    ASSERT(!realocc->is_reload, "real occ with is_reload cannot be set is_save");
  } else if (defx->occty == kOccPhiocc) {
    const MapleVector<MePhiOpndOcc *> &phiopnds = static_cast<MePhiOcc *>(defx)->phiopnds;
    for (uint32 i = 0; i < phiopnds.size(); i++) {
      MePhiOpndOcc *phiopnd = phiopnds[i];
      if (!phiopnd->is_processed) {
        phiopnd->is_processed = true;
        ASSERT(phiopnd->def, "EPhiFinalizer::SetSave: phiopndocc cannot have no def");
        SetSave(phiopnd->def);
      }
    }
  }

  if (defx->occty == kOccReal || defx->occty == kOccInserted) {
    BB *frombb = defx->GetBB();
    MapleSet<uint32> itFrontier(percand_allocator.Adapter());
    CHECK_FATAL(dominance->bbVec.size() > 0, "the size to be allocated is 0");
    GetIterDomFrontier(frombb, &itFrontier);
    for (MePhiOcc *phiocc : phi_occs) {
      if (!phiocc->IsWillBeAvail()) {
        continue;
      }
      if (itFrontier.find(dominance->dtDfn[phiocc->mirbb->id.idx]) == itFrontier.end()) {
        continue;
      }
      phiocc->is_extraneous = false;
    }
  }
}

void SSAPre::SetReplacement(MePhiOcc *occg, MeOccur *repdef) {
  occg->is_removed = true;
  for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
    MePhiOcc *phiocc = *it;
    if (phiocc->is_removed) {
      continue;
    }
    const MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
    for (uint32 i = 0; i < phiopnds.size(); i++) {
      MePhiOpndOcc *phiopnd = phiopnds[i];
      if (phiopnd->def == occg) {
        if (phiocc->is_extraneous) {
          // exclude recursive def
          SetReplacement(phiocc, repdef);
        } else {
          phiopnd->def = repdef;  // replace phiopnd of phiocc by replacing def repdef
          phiopnd->classid = repdef->classid;
          phiopnd->is_phiopndreload = true;
        }
      }
    }
  }

  for (MapleVector<MeRealOcc *>::iterator it = work_cand->real_occs.begin(); it != work_cand->real_occs.end(); it++) {
    MeRealOcc *realocc = *it;
    // when realocc satisfying reload and def of it is occg, do the replacement
    if (realocc->is_reload && realocc->def == occg) {
      realocc->def = repdef;
      realocc->classid = repdef->classid;
    }
  }
}

void SSAPre::Finalize2() {
  for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
    MePhiOcc *phiocc = *it;
    // initialize extraneouse for each MePhiOcc
    if (!work_cand->isSRCand) {
      phiocc->is_extraneous = phiocc->IsWillBeAvail();
    }

    // initialize each operand of phiocc
    const MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
    for (uint32 i = 0; i < phiopnds.size(); i++) {
      phiopnds[i]->is_processed = false;
    }
  }
  for (MapleVector<MeRealOcc *>::iterator it = work_cand->real_occs.begin(); it != work_cand->real_occs.end(); it++) {
    MeRealOcc *realocc = *it;
    if (realocc->is_reload) {
      SetSave(realocc->def);
    }
  }

  if (!work_cand->isSRCand) {
    for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
      MePhiOcc *phiocc = *it;
      if (phiocc->is_removed || !phiocc->is_extraneous) {
        continue;
      }
      if (!phiocc->IsWillBeAvail()) {
        phiocc->is_removed = true;
        continue;
      }
      const MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
      for (uint32 i = 0; i < phiopnds.size(); i++) {
        MePhiOpndOcc *phiopnd = phiopnds[i];
        MeOccur *defocc = phiopnd->def;
        switch (defocc->occty) {
          case kOccReal:
            SetReplacement(phiocc, defocc);
            break;
          case kOccInserted:
            break;
          case kOccPhiocc:
            if (!static_cast<MePhiOcc *>(defocc)->is_extraneous) {
              SetReplacement(phiocc, defocc);
            }
            break;
          default:
            CHECK_FATAL(false, "");
        }
      }
    }
  }

  if (ssapredebug) {
    LogInfo::MapleLogger() << "========after Finalize2====================\n";
    for (MeOccur *occ : all_occs) {
      if (occ->occty == kOccPhiocc) {
        MePhiOcc *phiocc = static_cast<MePhiOcc *>(occ);
        phiocc->Dump(irMap);
        if (phiocc->is_removed) {
          LogInfo::MapleLogger() << " was removed in Finalize2";
        }
        LogInfo::MapleLogger() << std::endl;
      } else if (occ->occty == kOccReal) {
        MeRealOcc *realocc = static_cast<MeRealOcc *>(occ);
        if (realocc->is_reload) {
          realocc->Dump(irMap);
          LogInfo::MapleLogger() << " is_reload\n";
        }
        if (realocc->is_save) {
          realocc->Dump(irMap);
          LogInfo::MapleLogger() << " is_save\n";
        }
      } else if (occ->occty == kOccPhiopnd) {
        MePhiOpndOcc *phiopndocc = static_cast<MePhiOpndOcc *>(occ);
        if (phiopndocc->is_insertedocc) {
          phiopndocc->Dump(irMap);
          LogInfo::MapleLogger() << " inserthere\n";
        }
      }
    }
  }
}

// ================ Step 4: WillBeAvail Computation =================

// propagate not-can_be_avail attribute forward
// along def-use chain to not-downsafety nodes
void SSAPre::ResetCanBeAvail(MePhiOcc *occg) {
  occg->is_canbeavail = false;
  // the following loop is to find occg's use list and reset them
  for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
    MePhiOcc *phiocc = *it;
    MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
    for (uint32 i = 0; i < phiopnds.size(); i++) {
      MePhiOpndOcc *phiopnd = phiopnds[i];
      if (phiopnd->def && phiopnd->def == occg) {
        // when comes here, phiopnd->def is a use of occg
        if (!phiopnd->has_real_use && !phiocc->is_downsafety && phiocc->is_canbeavail) {
          ResetCanBeAvail(phiocc);
        }
      }
    }
  }
}

void SSAPre::ComputeCanBeAvail() {
  for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
    MePhiOcc *phiocc = *it;
    if (!phiocc->is_downsafety && phiocc->is_canbeavail) {
      bool existNullDef = false;
      MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
      for (uint32 i = 0; i < phiopnds.size(); i++) {
        MePhiOpndOcc *phiopnd = phiopnds[i];
        if (phiopnd->def == nullptr) {
          existNullDef = true;
          break;
        }
      }
      if (existNullDef) {
        ResetCanBeAvail(phiocc);
      }
    }
    if (work_cand->redo2handle_crit_edges && phiocc->is_canbeavail) {
      // check critical edges
      bool existCritEdge = false;
      for (BB *pred : phiocc->mirbb->pred)
        if (pred->succ.size() > 1) {
          existCritEdge = true;
          break;
        }
      if (existCritEdge) {
        ResetCanBeAvail(phiocc);
      }
    }
  }
}

void SSAPre::ResetLater(MePhiOcc *occg) {
  occg->is_later = false;
  // the following loop is to find occg's use list and reset them
  for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
    MePhiOcc *phiocc = *it;
    MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
    for (uint32 i = 0; i < phiopnds.size(); i++) {
      MePhiOpndOcc *phiopnd = phiopnds[i];
      if (phiopnd->def && phiopnd->def == occg) {
        // when comes here, phiopnd->def is a use of occg
        if (phiocc->is_later) {
          ResetLater(phiocc);
        }
      }
    }
  }
}

void SSAPre::ComputeLater() {
  for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
    MePhiOcc *phiocc = *it;
    phiocc->is_later = phiocc->is_canbeavail;
  }

  for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
    MePhiOcc *phiocc = *it;
    if (phiocc->is_later) {
      bool existNonNullDef = false;
      MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
      for (uint32 i = 0; i < phiopnds.size(); i++) {
        MePhiOpndOcc *phiopnd = phiopnds[i];
        if (phiopnd->def != nullptr && phiopnd->has_real_use) {
          existNonNullDef = true;
          break;
        }
      }
      if (existNonNullDef || phiocc->speculative_downsafe) {
        ResetLater(phiocc);
      }
    }
  }

  if (ssapredebug) {
    LogInfo::MapleLogger() << "========ssapre candidate " << work_cand->index << " after WillBeAvail===================\n";
    for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
      MePhiOcc *phiocc = *it;
      phiocc->Dump(irMap);
      if (phiocc->is_canbeavail) {
        LogInfo::MapleLogger() << " can be avail;";
      } else {
        LogInfo::MapleLogger() << " not can be avail;";
      }
      if (phiocc->is_later) {
        LogInfo::MapleLogger() << " later;";
      } else {
        LogInfo::MapleLogger() << " not later;";
      }
      if (phiocc->is_canbeavail && !phiocc->is_later) {
        LogInfo::MapleLogger() << " will be avail";
      } else {
        LogInfo::MapleLogger() << " not will be avail";
      }
      LogInfo::MapleLogger() << "\n";
    }
  }
}

// ================ Step 3: Downsafe Computation =================

// reset downsafety
void SSAPre::ResetDS(MePhiOpndOcc *phiopnd) {
  if (phiopnd->has_real_use) {
    return;
  }
  MeOccur *defocc = phiopnd->def;
  if (!defocc || defocc->occty != kOccPhiocc) {
    return;
  }

  MePhiOcc *defphiocc = static_cast<MePhiOcc *>(defocc);
  if (defphiocc->speculative_downsafe) {
    return;
  }
  if (!defphiocc->is_downsafety) {
    return;
  }
  defphiocc->is_downsafety = false;
  MapleVector<MePhiOpndOcc *> &phiopnds = defphiocc->phiopnds;
  for (uint32 i = 0; i < phiopnds.size(); i++) {
    MePhiOpndOcc *phiopnd = phiopnds[i];
    ResetDS(phiopnd);
  }
}

// compute downsafety for each PHI
void SSAPre::ComputeDS() {
  for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
    MePhiOcc *phiocc = *it;
    if (!phiocc->is_downsafety) {
      // propagate not-downsafety along use-def edges
      MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
      for (uint32 i = 0; i < phiopnds.size(); i++) {
        MePhiOpndOcc *phiopnd = phiopnds[i];
        ResetDS(phiopnd);
      }
    }
  }

  if (ssapredebug) {
    LogInfo::MapleLogger() << "========ssapre candidate " << work_cand->index << " after DownSafety===================\n";
    for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
      MePhiOcc *phiocc = *it;
      phiocc->Dump(irMap);
      if (phiocc->speculative_downsafe) {
        LogInfo::MapleLogger() << " spec_downsafe /";
      }
      if (phiocc->is_downsafety) {
        LogInfo::MapleLogger() << " is downsafe\n";
        MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
        for (uint32 i = 0; i < phiopnds.size(); i++) {
          MePhiOpndOcc *phiopndocc = phiopnds[i];
          if (!phiopndocc->is_processed) {
            phiopndocc->Dump(irMap);
            LogInfo::MapleLogger() << " has not been processed by Rename2\n";
          }
        }
      } else {
        LogInfo::MapleLogger() << " is not downsafe\n";
      }
    }
  }
}

// ================ Step 2: Renaming =================

void SSAPre::Rename1() {
  std::stack<MeOccur *> occStack;
  rename2_set.clear();
  class_count = 1;
  // iterate the occurrence according to its preorder dominator tree
  for (MeOccur *occ : all_occs) {
    while (!occStack.empty() && !occStack.top()->IsDominate(dominance, occ)) {
      occStack.pop();
    }
    switch (occ->occty) {
      case kOccReal: {
        if (occStack.empty()) {
          // assign new class
          occ->classid = class_count++;
          occStack.push(occ);
          break;
        }
        MeOccur *topoccur = occStack.top();
        if (topoccur->occty == kOccUse || topoccur->occty == kOccMembar) {
          occ->classid = class_count++;
          occStack.push(occ);
          break;
        }
        MeRealOcc *realocc = static_cast<MeRealOcc *>(occ);
        if (topoccur->occty == kOccReal) {
          MeRealOcc *realtopoccur = static_cast<MeRealOcc *>(topoccur);
          if (AllVarsSameVersion(realtopoccur, realocc)) {
            // all corresponding variables are the same
            realocc->classid = realtopoccur->classid;
            if (realtopoccur->def != nullptr) {
              realocc->def = realtopoccur->def;
            } else {
              realocc->def = realtopoccur;
            }
          } else {
            // assign new class
            occ->classid = class_count++;
            occStack.push(occ);
          }
        } else {
          // top of stack is a PHI occurrence
          ASSERT(topoccur->occty == kOccPhiocc, "");
          vector<MeExpr *> varVec;
          CollectVarForCand(realocc, varVec);
          bool isalldom = true;
          if (realocc->is_lhs) {
            isalldom = false;
          } else {
            for (vector<MeExpr *>::iterator varit = varVec.begin(); varit != varVec.end(); varit++) {
              MeExpr *varmeexpr = *varit;
              if (!DefVarDominateOcc(varmeexpr, topoccur)) {
                isalldom = false;
              }
            }
          }
          if (isalldom) {
            realocc->classid = topoccur->classid;
            realocc->def = topoccur;
            rename2_set.insert(realocc->position);
            occStack.push(realocc);
            if (IsLoopHeadBB(topoccur->mirbb->id)) {
              static_cast<MePhiOcc *>(topoccur)->speculative_downsafe = true;
              static_cast<MePhiOcc *>(topoccur)->is_downsafety = true;
            }
          } else {
            MePhiOcc *phitopoccur = static_cast<MePhiOcc *>(topoccur);
            if (!phitopoccur->speculative_downsafe) {
              phitopoccur->is_downsafety = false;
            }
            // assign new class
            occ->classid = class_count++;
            occStack.push(occ);
          }
        }
        break;
      }
      case kOccPhiocc: {
        // assign new class
        occ->classid = class_count++;
        occStack.push(occ);
        break;
      }
      case kOccPhiopnd: {
        // stow away the use occurrences at the stack top
        MeOccur *stowedUseOcc = nullptr;
        if (!occStack.empty() && occStack.top()->occty == kOccUse) {
          stowedUseOcc = occStack.top();
          occStack.pop();
          CHECK_FATAL(occStack.empty() || occStack.top()->occty != kOccUse,
                 "Rename1: cannot have 2 consecutive use occurs on stack");
        }
        if (occStack.empty() || occStack.top()->occty == kOccMembar) {
          occ->def = nullptr;
        } else {
          MeOccur *topoccur = occStack.top();
          occ->def = topoccur;
          occ->classid = topoccur->classid;
          if (topoccur->occty == kOccReal) {
            static_cast<MePhiOpndOcc *>(occ)->has_real_use = true;
          }
        }
        // push stowed use_occ back
        if (stowedUseOcc) {
          occStack.push(stowedUseOcc);
        }
        break;
      }
      case kOccExit: {
        if (occStack.empty()) {
          break;
        }
        MeOccur *topoccur = occStack.top();
        if (topoccur->occty == kOccPhiocc) {
          MePhiOcc *phitopoccur = static_cast<MePhiOcc *>(topoccur);
          if (!phitopoccur->speculative_downsafe) {
            phitopoccur->is_downsafety = false;
          }
        }
        break;
      }
      case kOccMembar:
      case kOccUse: {
        if (!occStack.empty()) {
          MeOccur *topoccur = occStack.top();
          if (topoccur->occty == kOccPhiocc) {
            MePhiOcc *phitopoccur = static_cast<MePhiOcc *>(topoccur);
            phitopoccur->is_downsafety = false;
          } else if (topoccur->occty != occ->occty) {
            occStack.push(occ);
          }
        } else {
          occStack.push(occ);
        }
        break;
      }
      default:
        ASSERT(false, "");
    }
  }

  if (ssapredebug) {
    PreWorkCand *curCand = work_cand;
    LogInfo::MapleLogger() << "========ssapre candidate " << curCand->index << " after rename1===================\n";

    for (MeOccur *occ : all_occs) {
      occ->Dump(irMap);
      LogInfo::MapleLogger() << endl;
    }
    LogInfo::MapleLogger() << "\n"
                    << "rename2 set:\n";
    for (uint32_t pos : rename2_set) {
      MeRealOcc *occur = work_cand->real_occs[pos];
      occur->Dump(irMap);
      LogInfo::MapleLogger() << " with def at\n";
      occur->def->Dump(irMap);
      LogInfo::MapleLogger() << "\n";
    }
    LogInfo::MapleLogger() << "\n";
  }
}

// if opnd is defined by phi, return the jth opnd of the phi;
// return nullptr otherwise
MeExpr *SSAPre::GetReplaceMeExpr(MeExpr *opnd, const BB *ephibb, uint32 j) {
  if (opnd->meOp != kMeOpVar && opnd->meOp != kMeOpReg) {
    return nullptr;
  }
  MeExpr *retexpr = nullptr;

  ScalarMeExpr *expr = static_cast<ScalarMeExpr*>(opnd);
  if (expr->IsDefByPhi()) {
    MePhiNode *defPhi = expr->def.defPhi;
    if (ephibb->id == defPhi->defBB->id) {
      ASSERT(j < defPhi->opnds.size(), "index out of range in SSAPre::GetReplaceMeExpr");
      ASSERT(defPhi->opnds[j]->meOp == opnd->meOp, "");
      retexpr = defPhi->opnds[j];
    }
  }

  if (retexpr && retexpr->primType == kPtyInvalid) {
    retexpr->primType = work_cand->GetPrimType();
  }
  return retexpr;
}

void SSAPre::Rename2() {
  while (!rename2_set.empty()) {
    MapleSet<uint32_t>::iterator it = rename2_set.begin();
    MeRealOcc *realocc = work_cand->real_occs[*it];
    rename2_set.erase(it);
    MeOccur *defocc = realocc->def;
    if (!defocc || defocc->occty != kOccPhiocc) {
      CHECK_FATAL(false, "should be def by phiocc");
    }
    MePhiOcc *defphiocc = static_cast<MePhiOcc *>(defocc);
    MapleVector<MePhiOpndOcc *> &phiopnds = defphiocc->phiopnds;
    for (uint32 i = 0; i < phiopnds.size(); i++) {
      MePhiOpndOcc *phioccopnd = phiopnds[i];
      if (!phioccopnd->is_processed) {
        phioccopnd->is_processed = true;
        MeExpr *exprY = PhiOpndFromRes(realocc, i);
        phioccopnd->current_expr.meexpr = exprY;
        MeOccur *defx = phioccopnd->def;
        if (defx == nullptr) {
          continue;
        }
        if (defx->occty == kOccReal) {
          MeRealOcc *realdefx = static_cast<MeRealOcc *>(defx);
          vector<MeExpr *> varvecX;
          vector<MeExpr *> varvecY;
          CollectVarForMeExpr(realdefx->meexpr, varvecX);
          CollectVarForMeExpr(exprY, varvecY);
          CHECK_FATAL(varvecX.size() == varvecY.size(), "");
          bool hasSameVersion = true;
          for (uint32 ii = 0; ii < varvecX.size(); ii++) {
            MeExpr *resolvedY = ResolveAllInjuringDefs(varvecY[ii]);
            if (varvecX[ii] != resolvedY) {
              hasSameVersion = false;
            }
          }
          if (!hasSameVersion) {
            phioccopnd->def = nullptr;
            phioccopnd->has_real_use = false;
          }
        } else if (defx->occty == kOccPhiocc) {
          vector<MeExpr *> varvecY;
          bool alldom = true;
          CollectVarForMeExpr(exprY, varvecY);
          if (exprY->meOp == kMeOpOp) {
            OpMeExpr opmeexpr(*static_cast<OpMeExpr *>(exprY), -1);
            for (uint32 ii = 0; ii < varvecY.size(); ii++) {
              MeExpr *resolvedY = ResolveAllInjuringDefs(varvecY[ii]);
              if (resolvedY != varvecY[ii]) {
                SubstituteOpnd(&opmeexpr, varvecY[ii], resolvedY);
              }
              if (!DefVarDominateOcc(resolvedY, defx)) {
                alldom = false;
              }
            }
            exprY = irMap->HashMeExpr(&opmeexpr);
          } else {
            for (uint32 ii = 0; ii < varvecY.size(); ii++) {
              if (!DefVarDominateOcc(varvecY[ii], defx)) {
                alldom = false;
              }
            }
          }
          if (alldom) {
            // create a realocc and add to rename2 set
            MeRealOcc *occy = percand_mp->New<MeRealOcc>(static_cast<MeStmt*>(nullptr), 0, exprY);
            occy->position = work_cand->real_occs.size();
            work_cand->real_occs.push_back(occy);
            occy->def = defx;
            occy->classid = defx->classid;
            rename2_set.insert(occy->position);
            if (ssapredebug) {
              LogInfo::MapleLogger() << "--- rename2 adds to rename2_set manufactured ";
              occy->Dump(irMap);
              LogInfo::MapleLogger() << endl;
            }
          } else {
            phioccopnd->def = nullptr;
            phioccopnd->has_real_use = false;
            MePhiOcc *phidefx = static_cast<MePhiOcc *>(defx);
            if (!phidefx->speculative_downsafe) {
              phidefx->is_downsafety = false;
            }
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
      LogInfo::MapleLogger() << endl;
    }
  }
}

// ================ Step 1: Phi Insertion =================

void SSAPre::SetVarPhis(MeExpr *meexpr) {
  MeExprOp meOp = meexpr->meOp;
  if (meOp != kMeOpVar && meOp != kMeOpReg) {
    return;
  }

  if (meOp == kMeOpVar) {
    VarMeExpr *varx = static_cast<VarMeExpr *>(meexpr);
    if (work_cand->isSRCand) {
      varx = ResolveAllInjuringDefs(varx);
    }
    if (varx->IsDefByPhi()) {
      MePhiNode *phimenode = varx->def.defPhi;
      BBId defbbid = phimenode->defBB->id;
      if (varphi_dfns.find(dominance->dtDfn[defbbid.idx]) == varphi_dfns.end() && ScreenPhiBB(defbbid)) {
        varphi_dfns.insert(dominance->dtDfn[defbbid.idx]);
        for (auto opndit : phimenode->opnds) {
          SetVarPhis(opndit);
        }
      }
    }
  } else {
    RegMeExpr *regx = static_cast<RegMeExpr *>(meexpr);
    if (work_cand->isSRCand) {
      regx = ResolveAllInjuringDefs(regx);
    }
    if (regx->IsDefByPhi()) {
      MePhiNode *phimenode = regx->def.defPhi;
      BBId defbbid = phimenode->defBB->id;
      CHECK(defbbid.idx < dominance->dtDfn.size(), "defbbid.idx out of range in SSAPre::SetVarPhis");
      if (varphi_dfns.find(dominance->dtDfn[defbbid.idx]) == varphi_dfns.end() && ScreenPhiBB(defbbid)) {
        varphi_dfns.insert(dominance->dtDfn[defbbid.idx]);
        for (auto opndit : phimenode->opnds) {
          SetVarPhis(opndit);
        }
      }
    }
  }
}

// based on ssapre->work_cand's real_occs and dfphi_dfns (which now privides all
// the inserted phis), create the phi and phiopnd occ nodes; link them all up in
// order of dtPreOrder in ssapre->all_occs; the phi occ nodes are in addition
// provided in order of dtPreOrder in ssapre->phi_occs
void SSAPre::CreateSortedOccs() {
  // merge varphi_dfns to dfphi_dfns
  dfphi_dfns.insert(varphi_dfns.begin(), varphi_dfns.end());
  // form phiopnd_dfns
  std::multiset<uint32> phiopndDfns;
  for (uint32 dfn : dfphi_dfns) {
    BBId bbid = dominance->dtPreOrder[dfn];
    BB *bb = GetBB(bbid);
    ASSERT(bb != nullptr, "GetBB return null in SSAPre::CreateSortedOccs");
    for (BB *pred : bb->pred) {
      phiopndDfns.insert(dominance->dtDfn[pred->id.idx]);
    }
  }

  // under lpre, form lhs occ for formals at function entry
  if (rclowering_on) {
    BuildEntryLhsOcc4Formals();
  }

  std::unordered_map<BBId, std::forward_list<MePhiOpndOcc *>> bb2phiopndMap;
  MapleVector<MeRealOcc *>::iterator realoccIt = work_cand->real_occs.begin();
  MapleVector<MeExitOcc *>::iterator exitoccIt = exit_occs.begin();
  auto phidfnIt = dfphi_dfns.begin();
  auto phiopnddfnIt = phiopndDfns.begin();
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
  MeOccur *pickedocc;  // the next picked occ in order of preorder traveral of dominator tree
  do {
    pickedocc = nullptr;
    // the 4 kinds of occ must be checked in this order, so it will be right
    // if more than 1 has the same dfn
    if (nextPhiocc) {
      pickedocc = nextPhiocc;
    }
    if (nextRealocc && (pickedocc == nullptr || dominance->dtDfn[nextRealocc->mirbb->id.idx] <
                                                  dominance->dtDfn[pickedocc->mirbb->id.idx])) {
      pickedocc = nextRealocc;
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
        case kOccReal:
        case kOccMembar:
          realoccIt++;
          if (realoccIt != work_cand->real_occs.end()) {
            nextRealocc = *realoccIt;
          } else {
            nextRealocc = nullptr;
          }
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
      LogInfo::MapleLogger() << endl;
    }
  }
}

// ================ End of Step 1: Phi Insertion =================

IassignMeStmt *SSAPre::CopyIassignMeStmt(const IassignMeStmt *iastmt) {
  IassignMeStmt *mestmt = irMap->NewInPool<IassignMeStmt>(*iastmt);
  return mestmt;
}

// if the definition of varmeexpr dominate meocc then return true. otherwise return false;
bool SSAPre::DefVarDominateOcc(MeExpr *meexpr, MeOccur *meocc) {
  if (meexpr == nullptr) {
    // can be nullptr in the case of LHS occurrence
    return false;
  }
  CHECK_FATAL(meocc->occty == kOccPhiocc || meocc->occty == kOccInserted, "");
  BB *occbb = (static_cast<MePhiOcc *>(meocc))->mirbb;
  if (meexpr->meOp == kMeOpVar) {
    VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(meexpr);
    switch (varmeexpr->defBy) {
      case kDefByNo:
        return true;  // it's an original variable which dominates everything
      case kDefByStmt: {
        varmeexpr = ResolveAllInjuringDefs(varmeexpr);
        MeStmt *mestmt = varmeexpr->def.defStmt;
        CHECK_FATAL(mestmt, "should have a def mestmt");
        BB *defBB = mestmt->bb;
        if (occbb == defBB) {
          return false;
        } else {
          return dominance->Dominate(defBB, occbb);
        }
      }
      case kDefByPhi: {
        MePhiNode *phimenode = varmeexpr->def.defPhi;
        BB *defBB = phimenode->defBB;
        if (defBB->id == occbb->id) {
          return true;
        }
        return dominance->Dominate(defBB, occbb);
      }
      case kDefByMustdef: {
        MeStmt *mestmt = varmeexpr->def.defMustDef->base;
        if (!mestmt) {
          return true;  // it's a original variable dominate everything
        }
        BB *defBB = mestmt->bb;
        if (occbb == defBB) {
          return false;
        } else {
          return dominance->Dominate(defBB, occbb);
        }
      }
      case kDefByChi: {
        MeStmt *mestmt = varmeexpr->def.defChi->base;
        if (!mestmt) {
          return true;  // it's a original variable dominate everything
        }
        BB *defBB = mestmt->bb;
        if (occbb == defBB) {
          return false;
        } else {
          return dominance->Dominate(defBB, occbb);
        }
      }
      default:
        CHECK_FATAL(false, "to be done");
    }
  } else {
    CHECK_FATAL(meexpr->meOp == kMeOpReg, "");
    RegMeExpr *regmeexpr = static_cast<RegMeExpr *>(meexpr);
    switch (regmeexpr->defBy) {
      case kDefByNo:
        return true;  // original st dominates everything
      case kDefByStmt: {
        regmeexpr = ResolveAllInjuringDefs(regmeexpr);
        MeStmt *mestmt = regmeexpr->def.defStmt;
        CHECK_FATAL(mestmt, "");
        BB *defBB = mestmt->bb;
        if (occbb == defBB) {
          return false;
        } else {
          return dominance->Dominate(defBB, occbb);
        }
      }
      case kDefByPhi: {
        MePhiNode *phimenode = regmeexpr->def.defPhi;
        BB *defBB = phimenode->defBB;
        if (defBB->id == occbb->id) {
          return true;
        }
        return dominance->Dominate(defBB, occbb);
      }
      case kDefByMustdef: {
        MeStmt *mestmt = regmeexpr->def.defMustDef->base;
        if (!mestmt) {
          return true;  // it's a original variable dominate everything
        }
        BB *defBB = mestmt->bb;
        if (occbb == defBB) {
          return false;
        } else {
          return dominance->Dominate(defBB, occbb);
        }
      }
      case kDefByChi: {
        MeStmt *mestmt = regmeexpr->def.defChi->base;
        if (!mestmt) {
          return true;  // it's a original variable dominate everything
        }
        BB *defBB = mestmt->bb;
        if (occbb == defBB) {
          return false;
        } else {
          return dominance->Dominate(defBB, occbb);
        }
      }
      default:
        CHECK_FATAL(false, "to be done");
    }
  }
}

bool SSAPre::CheckIfAnyLocalOpnd(MeExpr *x) {
  switch (x->meOp) {
    case kMeOpReg:
      return true;
    case kMeOpVar: {
      VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(x);
      MIRSymbol *st = varmeexpr->ost->GetMIRSymbol();
      return st->IsLocal();
    }
    case kMeOpIvar: {
      IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(x);
      return CheckIfAnyLocalOpnd(ivarmeexpr->base);
    }
    case kMeOpOp: {
      OpMeExpr *opmeexpr = static_cast<OpMeExpr *>(x);
      if (CheckIfAnyLocalOpnd(opmeexpr->GetOpnd(0))) {
        return true;
      }
      if (opmeexpr->GetOpnd(1) == nullptr) {
        return false;
      }
      if (CheckIfAnyLocalOpnd(opmeexpr->GetOpnd(1))) {
        return true;
      }
      if (opmeexpr->GetOpnd(2) == nullptr) {
        return false;
      }
      return CheckIfAnyLocalOpnd(opmeexpr->GetOpnd(2));
    }
    case kMeOpNary: {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(x);
      for (size_t i = 0; i < narymeexpr->numOpnds; i++) {
        if (CheckIfAnyLocalOpnd(narymeexpr->GetOpnd(i))) {
          return true;
        }
      }
      return false;
    }
    default:
      return false;
  }
}

// create a new realocc based on the mestmt and meexpr
MeRealOcc *SSAPre::CreateRealOcc(MeStmt *mestmt, int seqstmt, MeExpr *meexpr, bool isrebuilt, bool islhs) {
  uint32 hidx = PreWorkCand::ComputeWorkCandHashIndex(meexpr);
  PreWorkCand *wkcand = PreWorkCand::workcandHashTable[hidx];
  while (wkcand != nullptr) {
    MeExpr *x = wkcand->themeexpr;
    ASSERT(x != nullptr, "CreateRealOcc: found workcand with themeexpr as nullptr");
    if (x->IsTheSameWorkcand(meexpr)) {
      break;
    }
    wkcand = static_cast<PreWorkCand *>(wkcand->next);
  }
  MeRealOcc *newocc = ssapre_mp->New<MeRealOcc>(mestmt, seqstmt, meexpr);
  newocc->is_lhs = islhs;
  if (wkcand != nullptr) {
    if (isrebuilt) {
      CHECK_FATAL(wkcand->index >= rebuilt_occ_index, "new ssapre work candidate is found as old work candidate");
      // insert to real_occs in dtPreOrder of the BBs and seq in each BB
      wkcand->AddRealOccSorted(dominance, newocc, GetPuidx(mestmt));
    } else {
      wkcand->AddRealOccAsLast(newocc, GetPuidx(mestmt));
    }
    return newocc;
  }
  // workcand not yet created; create a new one and add to worklist
  wkcand = ssapre_mp->New<PreWorkCand>(&ssapre_allocator, worklist.size(), meexpr, GetPuidx(mestmt));
  wkcand->has_local_opnd = CheckIfAnyLocalOpnd(meexpr);
  if (EpreLocalrefvar() && wkcand->themeexpr->meOp == kMeOpIvar) {
    // set wkcand->needlocalrefvar flag
    IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(wkcand->themeexpr);
    MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ivarmeexpr->tyIdx);
    CHECK_FATAL(mirType->typeKind == kTypePointer, "must be point type for ivar");
    MIRPtrType *ptrmirtype = static_cast<MIRPtrType *>(mirType);
    FieldID fieldID = ivarmeexpr->fieldID;
    TyidxFieldAttrPair fldpair = ptrmirtype->GetPointedTyidxFldAttrPairWithFieldId(fieldID);
    MIRType *ty = GlobalTables::GetTypeTable().typeTable.at(fldpair.first.GetIdx());
    bool isFinal = fldpair.second.GetAttr(FLDATTR_final);
    wkcand->needlocalrefvar = ty->primType == PTY_ref && !isFinal;
  }
  else if (strengthreduction && meexpr->StrengthReducible()) {
    wkcand->isSRCand = true;
  }
  worklist.push_back(wkcand);
  wkcand->AddRealOccAsLast(newocc, GetPuidx(mestmt));
  // add to bucket at workcandHashTable[hidx]
  wkcand->next = PreWorkCand::workcandHashTable[hidx];
  PreWorkCand::workcandHashTable[hidx] = wkcand;
  return newocc;
}

void SSAPre::CreateMembarOcc(MeStmt *mestmt, int seqstmt) {
  if (prekind == kLoadPre || prekind == kAddrPre) {
    return;
  }
  // go thru all workcands and insert a membar occurrence for each of them
  for (uint32 i = 0; i < worklist.size() && i <= prelimit; i++) {
    PreWorkCand *wkcand = worklist[i];
    if (prekind == kExprPre) {
      if (wkcand->themeexpr->meOp != kMeOpIvar) {
        continue;
      }
    } else if (prekind == kStmtPre) {
      if (static_cast<PreStmtWorkCand *>(wkcand)->themestmt->op != OP_dassign) {
        continue;
      }
    }
    MeRealOcc *newocc = ssapre_mp->New<MeRealOcc>(mestmt, seqstmt, wkcand->themeexpr);
    newocc->occty = kOccMembar;
    wkcand->AddRealOccAsLast(newocc, GetPuidx(mestmt));
  }
}

void SSAPre::CreateMembarOccAtCatch(BB *bb) {
  // go thru all workcands and insert a membar occurrence for each of them
  for (uint32 i = 0; i < worklist.size() && i <= prelimit; i++) {
    PreWorkCand *wkcand = worklist[i];
    MeRealOcc *newocc = ssapre_mp->New<MeRealOcc>(nullptr, 0, wkcand->themeexpr);
    newocc->occty = kOccMembar;
    newocc->mirbb = bb;
    wkcand->AddRealOccAsLast(newocc, GetPuidx(bb));
  }
}

void SSAPre::BuildWorkListStmt(MeStmt *stmt, uint32 seqStmt, bool isrebuilt, MeExpr *tempvar) {
  IncTreeid();
  Opcode op = stmt->op;
  switch (op) {
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
      return;
    case OP_membaracquire:
    case OP_membarrelease:
    case OP_membarstoreload:
    case OP_membarstorestore:
      CreateMembarOcc(stmt, seqStmt);
      break;
    case OP_gosub:
    case OP_retsub:
      //      if (!isrebuilt)
      //        CreateExitOcc(bb);
      break;
    case OP_throw: {
      ThrowMeStmt *thrmestmt = static_cast<ThrowMeStmt *>(stmt);
      BuildWorkListExpr(stmt, seqStmt, thrmestmt->opnd, isrebuilt, tempvar, true);
      //      if (!isrebuilt)
      //        CreateExitOcc(bb);
      break;
    }
    case OP_iassign: {
      IassignMeStmt *ivarstmt = static_cast<IassignMeStmt *>(stmt);
      BuildWorkListExpr(stmt, seqStmt, ivarstmt->rhs, isrebuilt, tempvar, true);
      BuildWorkListExpr(stmt, seqStmt, ivarstmt->lhsVar->base, isrebuilt, tempvar, true);
      BuildWorkListIvarLHSOcc(stmt, seqStmt, isrebuilt, tempvar);
      break;
    }
    case OP_brtrue:
    case OP_brfalse: {
      CondGotoMeStmt *condgotostmt = static_cast<CondGotoMeStmt *>(stmt);
      BuildWorkListExpr(stmt, seqStmt, condgotostmt->opnd, isrebuilt, tempvar, true);
      break;
    }
    case OP_switch: {
      SwitchMeStmt *switchstmt = static_cast<SwitchMeStmt *>(stmt);
      BuildWorkListExpr(stmt, seqStmt, switchstmt->opnd, isrebuilt, tempvar, true);
      break;
    }
    case OP_dassign: {
      DassignMeStmt *dassmestmt = static_cast<DassignMeStmt *>(stmt);
      if (dassmestmt->isIncDecStmt && prekind == kExprPre) {
        break;
      }
      BuildWorkListExpr(stmt, seqStmt, dassmestmt->rhs, isrebuilt, tempvar, true);
      BuildWorkListLHSOcc(stmt, seqStmt);
      break;
    }
    case OP_regassign: {
      AssignMeStmt *rassmestmt = static_cast<AssignMeStmt *>(stmt);
      if (rassmestmt->isIncDecStmt && prekind == kExprPre) {
        break;
      }
      BuildWorkListExpr(stmt, seqStmt, rassmestmt->rhs, isrebuilt, tempvar, true);
      break;
    }
    case OP_maydassign: {
      MaydassignMeStmt *dassmestmt = static_cast<MaydassignMeStmt *>(stmt);
      BuildWorkListExpr(stmt, seqStmt, dassmestmt->rhs, isrebuilt, tempvar, true);
      BuildWorkListLHSOcc(stmt, seqStmt);
      break;
    }
    case OP_decref: {
      UnaryMeStmt *unarystmt = static_cast<UnaryMeStmt *>(stmt);
      if (!rclowering_on && unarystmt->opnd->IsLeaf() && unarystmt->opnd->primType == PTY_ref) {
        // affects LPRE only; will cause CI failure if this is allowed
        break;
      }
      BuildWorkListExpr(stmt, seqStmt, unarystmt->opnd, isrebuilt, tempvar, true);
      break;
    }
    case OP_incref:
    case OP_decrefreset:
    case OP_eval:
    case OP_igoto:
    case OP_assertnonnull:
    case OP_free: {
      UnaryMeStmt *unarystmt = static_cast<UnaryMeStmt *>(stmt);
      BuildWorkListExpr(stmt, seqStmt, unarystmt->opnd, isrebuilt, tempvar, true);
      break;
    }
    case OP_syncenter:
    case OP_syncexit: {
      SyncMeStmt *syncmestmt = static_cast<SyncMeStmt *>(stmt);
      MapleVector<MeExpr *> &opnds = syncmestmt->opnds;
      for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
        BuildWorkListExpr(stmt, seqStmt, *it, isrebuilt, tempvar, true);
      }
      break;
    }
    case OP_return: {
      RetMeStmt *retmestmt = static_cast<RetMeStmt *>(stmt);
      MapleVector<MeExpr *> &opnds = retmestmt->opnds;
      for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
        if ((*it)->IsLeaf() && (*it)->primType == PTY_ref) {
          // affects LPRE only; will cause CI failure if this is allowed
          if (!rclowering_on) {
            continue;
          }
          if (!regreadAtReturn) {
            if ((*it)->meOp == kMeOpVar) {
              VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(*it);
              if (!varmeexpr->ost->isFormal) {
                continue;
              }
            }
          }
        }
        BuildWorkListExpr(stmt, seqStmt, *it, isrebuilt, tempvar, true);
      }
      //      if (!isrebuilt)
      //        CreateExitOcc(bb);
      break;
    }
    case OP_call:
    case OP_virtualcall:
    case OP_virtualicall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_interfaceicall:
    case OP_customcall:
    case OP_polymorphiccall:
    case OP_icall:
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_icallassigned: {
      NaryMeStmt *narymestmt = static_cast<NaryMeStmt *>(stmt);
      MapleVector<MeExpr *> &opnds = narymestmt->opnds;
      for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
        BuildWorkListExpr(stmt, seqStmt, *it, isrebuilt, tempvar, true);
      }
      break;
    }
    case OP_intrinsiccall: {
      IntrinsiccallMeStmt *intrn = static_cast<IntrinsiccallMeStmt *>(stmt);
      if (intrn->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS ||
          intrn->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS_SKIP) {
        break;
      }
      // fall thru
    }
    case OP_xintrinsiccall:
    case OP_intrinsiccallwithtype:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned: {
      NaryMeStmt *narymestmt = static_cast<NaryMeStmt *>(stmt);
      MapleVector<MeExpr *> &opnds = narymestmt->opnds;
      for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
        if (!rclowering_on && (*it)->IsLeaf() && (*it)->meOp == kMeOpVar) {
          // affects LPRE only; some later phase needs to transform dread to addrof
          VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(*it);
          MIRSymbol *sym = varmeexpr->ost->GetMIRSymbol();
          if (sym->GetAttr(ATTR_static)) {
            // its address may be taken
            continue;
          }
        }
        BuildWorkListExpr(stmt, seqStmt, *it, isrebuilt, tempvar, true);
      }
      break;
    }
    case OP_assertlt:
    case OP_assertge: {
      AssertMeStmt *assmestmt = static_cast<AssertMeStmt *>(stmt);
      BuildWorkListExpr(stmt, seqStmt, assmestmt->opnds[0], isrebuilt, tempvar, true);
      BuildWorkListExpr(stmt, seqStmt, assmestmt->opnds[1], isrebuilt, tempvar, true);
      break;
    }
    default:
      CHECK_FATAL(op == OP_comment, "");
      break;
  }
  if (kOpcodeInfo.IsCallAssigned(op)) {
    BuildWorkListLHSOcc(stmt, seqStmt);
  }
}

void SSAPre::BuildWorkListBB(BB *bb) {
  if (spillatcatch && bb->IsCatch()) {
    CreateMembarOccAtCatch(bb);
  }
  uint32 seqStmt = 0;
  for (auto stmt : bb->meStmtList) {
    BuildWorkListStmt(stmt, ++seqStmt, false);
  }
  if (bb->IsExit() || bb->WontExit()) {
    CreateExitOcc(bb);
  }
}

void SSAPre::DumpWorkListWrap() {
  if (ssapredebug) {
    DumpWorkList();
  }
}

void SSAPre::DumpWorkList() {
  LogInfo::MapleLogger() << "======== in SSAPRE worklist==============\n";
  for (uint32 i = 0; i < worklist.size(); i++) {
    PreWorkCand *workcand = worklist[i];
    workcand->Dump(irMap);
  }
}

GStrIdx SSAPre::NewTempStridx() {
  std::string preStr("ssapre_tempvar");
  preStr.append(to_string(stridx_count++));
  return GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(preStr);
}

void SSAPre::ApplySSAPRE() {
  // #0 build worklist
  BuildWorkList();
  if (ssapredebug)
    LogInfo::MapleLogger() << " worklist initial size " << worklist.size() << std::endl;
  ConstructUseOccurMap();
  for (uint32 i = 0; i < worklist.size() && i <= prelimit; i++) {
    percand_mp->Push();
    work_cand = worklist[i];
    if (work_cand->real_occs.size() == 0) {
      continue;
    }
    if ((prekind == kExprPre && work_cand->themeexpr->meOp == kMeOpIvar) || (prekind == kLoadPre)) {
      // if only LHS real occur, skip this candidate
      bool hasNonLhs = false;
      for (MeRealOcc *realocc : work_cand->real_occs) {
        if (realocc->occty == kOccReal && !realocc->is_lhs) {
          hasNonLhs = true;
          break;
        }
      }
      if (!hasNonLhs) {
        continue;
      }
    }
    if (ssapredebug) {
      LogInfo::MapleLogger() << "||||||| SSAPRE candidate " << i << " at worklist index " << work_cand->index << ": ";
      work_cand->DumpCand(irMap);
      LogInfo::MapleLogger() << std::endl;
    }

    all_occs.clear();
    phi_occs.clear();
    // #1 Insert PHI; results in all_occs and phi_occs
    ComputeVarAndDfPhis();
    CreateSortedOccs();
    if (work_cand->real_occs.size() == 0) {
      continue;
    }
    // set the position field in the MeRealOcc nodes
    for (uint32 j = 0; j < work_cand->real_occs.size(); j++) {
      work_cand->real_occs[j]->position = j;
    }

    // #2 Rename
    Rename1();
    Rename2();
    if (!phi_occs.empty()) {
      // if no PHI inserted, no need to compute DSafety, WBAvail
      // #3 DownSafty
      ComputeDS();
      // #4 WillBeAvail
      ComputeCanBeAvail();
      ComputeLater();
    }
    // #5 Finalize
    Finalize1();
    if (work_cand->redo2handle_crit_edges) {
      // reinitialize def field to nullptr
      for (MeOccur *occ : all_occs) {
        occ->def = nullptr;
        if (occ->occty == kOccPhiopnd) {
          MePhiOpndOcc *phiopndocc = static_cast<MePhiOpndOcc *>(occ);
          phiopndocc->is_processed = false;
        }
      }
      Rename1();
      Rename2();
      ComputeDS();
      ComputeCanBeAvail();
      ComputeLater();
      Finalize1();
    }
    Finalize2();
    // #6 CodeMotion and recompute worklist based on newly occurrence
    CodeMotion();
    if (prekind == kStmtPre && work_cand->real_occs.front()->mestmt->op == OP_dassign) {
      // apply full redundancy elimination
      DoSSAFRE();
    }
    percand_mp->Pop();
  }
}

}  // namespace maple
