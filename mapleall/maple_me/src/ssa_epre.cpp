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

#include "ssa_epre.h"

namespace maple {

void SSAEPre::GenerateSaveLhsRealocc(MeRealOcc *realocc, ScalarMeExpr *regorvar) {
  CHECK_FATAL(realocc->mestmt->op == OP_iassign, "GenerateSaveLhsReal: only iassign expected");
  IassignMeStmt *iass = static_cast<IassignMeStmt *>(realocc->mestmt);
  IvarMeExpr *thelhs = iass->lhsVar;
  MeExpr *savedRhs = iass->rhs;
  TyIdx savedTyidx = iass->tyIdx;
  MapleMap<OStIdx, ChiMeNode *> savedChiList = iass->chiList;
  iass->chiList.clear();
  SrcPosition savedSrcpos = iass->srcPos;
  BB *savedBb = iass->bb;
  MeStmt *savedPrev = iass->prev;
  MeStmt *savedNext = iass->next;
  AssignMeStmt *rass = nullptr;
  if (!work_cand->needlocalrefvar || placementrc_on) {
    CHECK_FATAL(regorvar->meOp == kMeOpReg, "GenerateSaveLhsRealocc: EPRE temp must b e preg here");
    PrimType lhsPrimType = thelhs->GetType()->primType;
    if (GetPrimTypeSize(savedRhs->primType) > GetPrimTypeSize(lhsPrimType)) {
      // insert integer truncation to the rhs
      if (GetPrimTypeSize(lhsPrimType) >= 4) {
        savedRhs = irMap->CreateMeExprTypeCvt(lhsPrimType, savedRhs->primType, savedRhs);
      } else {
        Opcode extOp = IsSignedInteger(lhsPrimType) ? OP_sext : OP_zext;
        PrimType newPrimType = PTY_u32;
        if (IsSignedInteger(lhsPrimType)) {
          newPrimType = PTY_i32;
        }
        OpMeExpr opmeexpr(-1, extOp, newPrimType, 1);
        opmeexpr.bitsSize = GetPrimTypeSize(lhsPrimType) * 8;
        opmeexpr.SetOpnd(savedRhs, 0);
        savedRhs = irMap->HashMeExpr(&opmeexpr);
      }
    }
    // change original iassign to regassign;
    // use placement new to modify in place, because other occ nodes are pointing
    // to this statement in order to get to the rhs expression;
    // this assumes AssignMeStmt has smaller size then IassignMeStmt
    rass = new (iass) AssignMeStmt(OP_regassign, static_cast<RegMeExpr *>(regorvar), savedRhs);
    rass->srcPos = savedSrcpos;
    rass->bb = savedBb;
    rass->prev = savedPrev;
    rass->next = savedNext;
    regorvar->SetDefByStmt(rass);
  } else {
    // regorvar is kMeOpReg and localrefvar is kMeOpVar
    VarMeExpr *localrefvar = CreateNewCurLocalrefvar();
    temp2localrefvar_map[static_cast<RegMeExpr *>(regorvar)] = localrefvar;
    // generate localrefvar = saved_rhs by changing original iassign to dassign;
    // use placement new to modify in place, because other occ nodes are pointing
    // to this statement in order to get to the rhs expression;
    // this assumes DassignMeStmt has smaller size then IassignMeStmt
    DassignMeStmt *dass = new (iass) DassignMeStmt(&irMap->irMapAlloc, localrefvar, savedRhs);
    dass->srcPos = savedSrcpos;
    dass->bb = savedBb;
    dass->prev = savedPrev;
    dass->next = savedNext;
    localrefvar->SetDefByStmt(dass);

    // regorvar = localrefvar
    rass = irMap->CreateAssignMeStmt(regorvar, localrefvar, savedBb);
    regorvar->SetDefByStmt(rass);
    savedBb->InsertMeStmtAfter(dass, rass);
    EnterCandsForSsaupdate(localrefvar->ost->index, savedBb);
  }
  // create new iassign for original lhs
  IassignMeStmt *newiass = irMap->NewInPool<IassignMeStmt>(savedTyidx, thelhs, regorvar, &savedChiList);
  thelhs->defStmt = newiass;
  newiass->bb = savedBb;
  savedBb->InsertMeStmtAfter(rass, newiass);
  // go throu saved_chi_list to update each chi base to point to newiass
  for (MapleMap<OStIdx, ChiMeNode *>::iterator it = newiass->chiList.begin(); it != newiass->chiList.end(); it++) {
    ChiMeNode *chi = it->second;
    chi->base = newiass;
  }

  realocc->saved_expr = regorvar;
}

void SSAEPre::GenerateSaveRealocc(MeRealOcc *realocc) {
  ASSERT(GetPuidx(realocc->mestmt) == work_cand->puIdx || work_cand->puIdx == 0,
          "GenerateSaveRealocc: inconsistent puIdx");
  ScalarMeExpr *regorvar = CreateNewCurTemp(realocc->meexpr);
  if (realocc->is_lhs) {
    GenerateSaveLhsRealocc(realocc, regorvar);
    return;
  }
  // create a new mestmt before realocc->mestmt
  AssignMeStmt *newmestmt = nullptr;
  bool isRhsOfDassign = false;
  if (work_cand->needlocalrefvar && (realocc->mestmt->op == OP_dassign) &&
      (realocc->mestmt->GetMeStmtOpnd(0) == realocc->meexpr)) {
    isRhsOfDassign = true;
    static_cast<DassignMeStmt *>(realocc->mestmt)->GetVarLhs()->noDelegateRC = 1;  // setting flag so delegaterc will skip
  }
  if (!work_cand->needlocalrefvar || isRhsOfDassign || placementrc_on) {
    newmestmt = irMap->CreateAssignMeStmt(regorvar, realocc->meexpr, realocc->mestmt->bb);
    regorvar->SetDefByStmt(newmestmt);
    realocc->mestmt->bb->InsertMeStmtBefore(realocc->mestmt, newmestmt);
  } else {
    // regorvar is MeOp_reg and localrefvar is kMeOpVar
    VarMeExpr *localrefvar = CreateNewCurLocalrefvar();
    temp2localrefvar_map[static_cast<RegMeExpr *>(regorvar)] = localrefvar;
    // localrefvar = meexpr
    newmestmt = irMap->CreateAssignMeStmt(localrefvar, realocc->meexpr, realocc->mestmt->bb);
    localrefvar->SetDefByStmt(newmestmt);
    realocc->mestmt->bb->InsertMeStmtBefore(realocc->mestmt, newmestmt);

    // reg = localrefvar
    newmestmt = irMap->CreateAssignMeStmt(regorvar, localrefvar, realocc->mestmt->bb);
    regorvar->SetDefByStmt(newmestmt);
    realocc->mestmt->bb->InsertMeStmtBefore(realocc->mestmt, newmestmt);
    EnterCandsForSsaupdate(localrefvar->ost->index, realocc->mestmt->bb);
  }
  // replace realocc->mestmt's occ with regorvar
  bool isreplaced = irMap->ReplaceMeExprStmt(realocc->mestmt, realocc->meexpr, regorvar);
  // rebuild worklist
  if (isreplaced) {
    BuildWorkListStmt(realocc->mestmt, realocc->seq, true, regorvar);
  }
  realocc->saved_expr = regorvar;
}

void SSAEPre::GenerateReloadRealocc(MeRealOcc *realocc) {
  CHECK_FATAL(!realocc->is_lhs, "GenerateReloadRealocc: cannot be LHS occurrence");
  MeExpr *regorvar = nullptr;
  MeOccur *defocc = realocc->def;
  if (defocc->occty == kOccReal) {
    MeRealOcc *defrealocc = static_cast<MeRealOcc *>(defocc);
    regorvar = defrealocc->saved_expr;
  } else if (defocc->occty == kOccPhiocc) {
    MePhiOcc *defphiocc = static_cast<MePhiOcc *>(defocc);
    MePhiNode *scalarPhi = (defphiocc->regPhi ? defphiocc->regPhi : defphiocc->varPhi);
    regorvar = scalarPhi->lhs;
  } else if (defocc->occty == kOccInserted) {
    MeInsertedOcc *definsertedocc = static_cast<MeInsertedOcc *>(defocc);
    regorvar = definsertedocc->saved_expr;
  } else {
    CHECK_FATAL(false, "NYI");
  }
  ASSERT(regorvar, "temp not yet generated");
  // replace realocc->mestmt's occ with regorvar
  bool isreplaced = irMap->ReplaceMeExprStmt(realocc->mestmt, realocc->meexpr, regorvar);
  // update worklist
  if (isreplaced) {
    BuildWorkListStmt(realocc->mestmt, realocc->seq, true, regorvar);
  }
}

// for each variable in realz that is defined by a phi, replace it by the jth
// phi opnd
MeExpr *SSAEPre::PhiOpndFromRes(MeRealOcc *realz, uint32 j) {
  MeOccur *defz = realz->def;
  CHECK_FATAL(defz && (defz->occty == kOccPhiocc), "must be def by phiocc");
  BB *ephibb = defz->mirbb;
  switch (realz->meexpr->meOp) {
    case kMeOpOp: {
      OpMeExpr opmeexpr(*static_cast<OpMeExpr *>(realz->meexpr), -1);
      for (uint32 i = 0; i < opmeexpr.numOpnds; i++) {
        MeExpr *retopnd = GetReplaceMeExpr(opmeexpr.GetOpnd(i), ephibb, j);
        if (retopnd) {
          opmeexpr.SetOpnd(retopnd, i);
        }
      }
      return irMap->HashMeExpr(&opmeexpr);
    }
    case kMeOpNary: {
      NaryMeExpr narymeexpr(&irMap->irMapAlloc, -1, *static_cast<NaryMeExpr *>(realz->meexpr));
      for (uint32 i = 0; i < narymeexpr.numOpnds; i++) {
        MeExpr *retopnd = GetReplaceMeExpr(narymeexpr.GetOpnd(i), ephibb, j);
        if (retopnd) {
          narymeexpr.SetOpnd(retopnd, i);
        }
      }
      return irMap->HashMeExpr(&narymeexpr);
    }
    case kMeOpIvar: {
      IvarMeExpr ivarmeexpr(-1, *static_cast<IvarMeExpr *>(realz->meexpr));
      MeExpr *retopnd = GetReplaceMeExpr(ivarmeexpr.base, ephibb, j);
      if (retopnd) {
        ivarmeexpr.base = retopnd;
      }
      MeExpr *muopnd = GetReplaceMeExpr(ivarmeexpr.mu, ephibb, j);
      if (muopnd) {
        ivarmeexpr.mu = static_cast<VarMeExpr *>(muopnd);
      }
      return irMap->HashMeExpr(&ivarmeexpr);
    }
    default:
      ASSERT(false, "NYI");
  }
  return nullptr;
}

bool SSAEPre::AllVarsSameVersion(MeRealOcc *realocc1, MeRealOcc *realocc2) {
  if (realocc1->meexpr == realocc2->meexpr) {
    return true;
  } else if (!work_cand->isSRCand) {
    return false;
  }
  // for each var operand in realocc2, check if it can resolve to the
  // corresponding operand in realocc1 via ResolveOneInjuringDef()
  for (int32 i = 0; i < realocc2->meexpr->NumMeExprOpnds(); i++) {
    MeExpr *curopnd = realocc2->meexpr->GetOpnd(i);
    if (curopnd->meOp != kMeOpVar && curopnd->meOp != kMeOpReg) {
      continue;
    }
    if (curopnd == realocc1->meexpr->GetOpnd(i)) {
      continue;
    }
    MeExpr *resolvedOpnd = ResolveAllInjuringDefs(curopnd);
    if (resolvedOpnd == curopnd || resolvedOpnd != realocc1->meexpr->GetOpnd(i)) {
      return false;
    }
    else {
      continue;
    }
  }
  return true;
}

// Df phis are computed into the df_phis set; Var Phis in the var_phis set
void SSAEPre::ComputeVarAndDfPhis() {
  varphi_dfns.clear();
  dfphi_dfns.clear();
  const MapleVector<MeRealOcc *> &realoccList = work_cand->real_occs;
  CHECK_FATAL(dominance->bbVec.size() > 0, "size to be allocated is 0");
  for (MapleVector<MeRealOcc *>::const_iterator it = realoccList.begin(); it != realoccList.end(); it++) {
    MeRealOcc *realocc = *it;
    BB *defBb = realocc->mirbb;
    GetIterDomFrontier(defBb, &dfphi_dfns);

    MeExpr *meexpr = realocc->meexpr;
    for (int32 i = 0; i < meexpr->NumMeExprOpnds(); i++) {
      SetVarPhis(meexpr->GetOpnd(i));
    }
  }
}

// build worklist for each expression;
// isrebuild means the expression is built from second time, in which case,
// tempvar is not nullptr, and it matches only expressions with tempvar as one of
// its operands; isrebuild is true only when called from the code motion phase
void SSAEPre::BuildWorkListExpr(MeStmt *mestmt, int32 seqstmt, MeExpr *meexpr, bool isrebuild, MeExpr *tempvar,
                                bool isRootExpr) {
  if (meexpr->treeID == (cur_treeid + 1)) {
    return;  // already visited twice in the same tree
  }
  MeExprOp meOp = meexpr->meOp;
  switch (meOp) {
    case kMeOpOp: {
      OpMeExpr *meopexpr = static_cast<OpMeExpr *>(meexpr);
      bool isFirstOrder = true;
      bool hasTempvarAs1Opnd = false;
      for (uint32 i = 0; i < meopexpr->numOpnds; i++) {
        MeExpr *opnd = meopexpr->GetOpnd(i);
        if (!opnd->IsLeaf()) {
          BuildWorkListExpr(mestmt, seqstmt, opnd, isrebuild, tempvar, false);
          isFirstOrder = false;
        } else if (LeafIsVolatile(opnd)) {
          isFirstOrder = false;
        } else {
          if (tempvar != nullptr && opnd->IsUseSameSymbol(tempvar)) {
            hasTempvarAs1Opnd = true;
          }
        }
      }
      if (!isFirstOrder) {
        break;
      }
      if (meexpr->primType == PTY_agg) {
        break;
      }
      if (isRootExpr && kOpcodeInfo.IsCompare(meopexpr->op)) {
        break;
      }
      if (!epre_include_ref && meopexpr->primType == PTY_ref) {
        break;
      }
      if (meopexpr->op == OP_gcmallocjarray || meopexpr->op == OP_gcmalloc) {
        break;
      }
      if (isrebuild && !hasTempvarAs1Opnd) {
        break;
      }
      CreateRealOcc(mestmt, seqstmt, meexpr, isrebuild);
      break;
    }
    case kMeOpNary: {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(meexpr);
      bool isFirstOrder = true;
      bool hasTempvarAs1Opnd = false;
      for (size_t i = 0; i < narymeexpr->numOpnds; i++) {
        MeExpr *opnd = narymeexpr->GetOpnd(i);
        if (!opnd->IsLeaf()) {
          BuildWorkListExpr(mestmt, seqstmt, opnd, isrebuild, tempvar, false);
          isFirstOrder = false;
        } else if (LeafIsVolatile(opnd)) {
          isFirstOrder = false;
        } else if (tempvar != nullptr && opnd->IsUseSameSymbol(tempvar)) {
          hasTempvarAs1Opnd = true;
        }
      }
      if (meexpr->primType == PTY_agg) {
        isFirstOrder = false;
      }
      if (isFirstOrder && (!isrebuild || hasTempvarAs1Opnd) && narymeexpr->primType != PTY_u1 &&
          (GetPrimTypeSize(narymeexpr->primType) >= 4 || IsPrimitivePoint(narymeexpr->primType) ||
           (narymeexpr->op == OP_intrinsicop && IntrinDesc::intrintable[narymeexpr->intrinsic].IsPure())) &&
          (epre_include_ref || narymeexpr->primType != PTY_ref)) {
        if (meexpr->op == OP_array) {
          MIRType *mirType = GlobalTables::GetTypeTable().typeTable.at(narymeexpr->tyIdx.GetIdx());
          CHECK_FATAL(mirType->typeKind == kTypePointer, "array must have pointer type");
          MIRPtrType *ptrmirtype = static_cast<MIRPtrType *>(mirType);
          MIRJarrayType *arrytype = dynamic_cast<MIRJarrayType *>(ptrmirtype->GetPointedType());
          if (arrytype == nullptr) {
            CreateRealOcc(mestmt, seqstmt, meexpr, isrebuild);
          } else {
            int dim = arrytype->GetDim();  // to compute the dim field
            if (dim < 2) {
              CreateRealOcc(mestmt, seqstmt, meexpr, isrebuild);
            } else {
              if (ssapredebug)
                LogInfo::MapleLogger() << "----- real occ suppressed for jarray with dim " << dim << std::endl;
            }
          }
        } else {
          IntrinDesc *intrindesc = &IntrinDesc::intrintable[narymeexpr->intrinsic];
          if (CfgHasDoWhile() && narymeexpr->intrinsic == INTRN_JAVA_ARRAY_LENGTH) {
            VarMeExpr *varopnd = dynamic_cast<VarMeExpr *>(narymeexpr->GetOpnd(0));
            if (varopnd != nullptr) {
              OriginalSt *ost = varopnd->ost;
              if (ost->IsFormal()) {
                break;
              }
            }
          }
          if (!intrindesc->IsLoadMem()) {
            CreateRealOcc(mestmt, seqstmt, meexpr, isrebuild);
          }
        }
      }
      break;
    }
    case kMeOpIvar: {
      IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(meexpr);
      MeExpr *base = ivarmeexpr->base;
      if (meexpr->primType == PTY_agg) {
        break;
      }
      if (!base->IsLeaf())
        BuildWorkListExpr(mestmt, seqstmt, ivarmeexpr->base, isrebuild, tempvar, false);
      else if (ivarmeexpr->IsVolatile()) {
        break;
      } else if (!epre_include_ref && ivarmeexpr->primType == PTY_ref) {
        break;
      } else if (!isrebuild || base->IsUseSameSymbol(tempvar)) {
        CreateRealOcc(mestmt, seqstmt, meexpr, isrebuild);
      }
      break;
    }
    case kMeOpVar:
    case kMeOpReg:
    case kMeOpAddrof:
    case kMeOpAddroffunc:
    case kMeOpAddroflabel:
    case kMeOpGcmalloc:
    case kMeOpConst:
    case kMeOpConststr:
    case kMeOpConststr16:
    case kMeOpSizeoftype:
      break;
    default:
      CHECK_FATAL(false, "MeOP NIY");
  }
  if (meexpr->treeID == cur_treeid) {
    meexpr->treeID = cur_treeid + 1;  // just processed 2nd time; not
  }
  // to be processed again in this tree
  else {
    meexpr->treeID = cur_treeid;  // just processed 1st time; willing to
  }
  // process one more time
  return;
}

void SSAEPre::BuildWorkListIvarLHSOcc(MeStmt *mestmt, int32 seqstmt, bool isrebuild, MeExpr *tempvar) {
  if (!enable_lhs_ivar || placementrc_on) {
    return;
  }
  if (mestmt->op != OP_iassign) {
    return;
  }
  IassignMeStmt *iass = static_cast<IassignMeStmt *>(mestmt);
  IvarMeExpr *ivarmeexpr = iass->lhsVar;
  if (ivarmeexpr->primType == PTY_agg) {
    return;
  }
  if (ivarmeexpr->IsVolatile()) {
    return;
  }
  if (!epre_include_ref && ivarmeexpr->primType == PTY_ref) {
    return;
  }
  MeExpr *base = ivarmeexpr->base;
  if (!base->IsLeaf()) {
    return;
  }
  if (!isrebuild || base->IsUseSameSymbol(tempvar)) {
    CreateRealOcc(mestmt, seqstmt, ivarmeexpr, isrebuild, true);
  }
}

// collect meexpr's variables and put them into varvec
// varvec can only store RegMeExpr and VarMeExpr
void SSAEPre::CollectVarForMeExpr(MeExpr *meexpr, std::vector<MeExpr *> &varvec) {
  for (int32 i = 0; i < meexpr->NumMeExprOpnds(); i++) {
    MeExpr *opnd = meexpr->GetOpnd(i);
    if (opnd->meOp == kMeOpVar || opnd->meOp == kMeOpReg) {
      varvec.push_back(opnd);
    }
  }
  if (meexpr->meOp == kMeOpIvar) {
    varvec.push_back(static_cast<IvarMeExpr *>(meexpr)->mu);
  }
}

void SSAEPre::CollectVarForCand(MeRealOcc *realocc, std::vector<MeExpr *> &varvec) {
  CollectVarForMeExpr(realocc->meexpr, varvec);
}

}  // namespace maple
