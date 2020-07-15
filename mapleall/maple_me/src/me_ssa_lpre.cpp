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

#include "me_ssa_lpre.h"
#include "mir_builder.h"
#include "me_lower_globals.h"

#define JAVALANG (mirModule->IsJavaModule())

namespace maple {

void MeSSALPre::GenerateSaveRealocc(MeRealOcc *realocc) {
  ASSERT(GetPuidx(realocc->mestmt) == work_cand->puIdx || work_cand->puIdx == 0,
          "GenerateSaveRealocc: inconsistent puIdx");
  ScalarMeExpr *regorvar = static_cast<ScalarMeExpr *>(CreateNewCurTemp(realocc->meexpr));
  if (!realocc->is_lhs) {
    // create a new mestmt before realocc->mestmt
    AssignMeStmt *newmestmt = irMap->CreateAssignMeStmt(regorvar, realocc->meexpr, realocc->mestmt->bb);
    regorvar->SetDefByStmt(newmestmt);
    realocc->mestmt->bb->InsertMeStmtBefore(realocc->mestmt, newmestmt);
    // replace realocc->mestmt's occ with regorvar
    irMap->ReplaceMeExprStmt(realocc->mestmt, realocc->meexpr, regorvar);
  } else if (realocc->is_formal_at_entry) {
    // no need generate any code, but change formal declaration to preg
    CHECK_FATAL(regorvar->meOp == kMeOpReg, "formals not promoted to register");
    VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(realocc->meexpr);
    MIRSymbol *oldformalst = varmeexpr->ost->GetMIRSymbol();
    RegMeExpr *regformal = static_cast<RegMeExpr *>(regorvar);
    MIRSymbol *newformalst =
      mirModule->mirBuilder->CreatePregFormalSymbol(oldformalst->tyIdx, regformal->regIdx, func->mirFunc);
    uint32 i = 0;
    for (; i < func->mirFunc->formalDefVec.size(); i++)
      if (func->mirFunc->formalDefVec[i].formalSym == oldformalst) {
        func->mirFunc->formalDefVec[i].formalSym = newformalst;
        break;
      }
    CHECK_FATAL(i < func->mirFunc->formalDefVec.size(), "Cannot replace promoted formal");
  } else if (realocc->mestmt->op == OP_dassign || realocc->mestmt->op == OP_maydassign) {
    VarMeExpr *thelhs = realocc->mestmt->GetVarLhs();
    MeExpr *savedRhs = realocc->mestmt->GetRhs();
    CHECK_FATAL(thelhs != nullptr && savedRhs != nullptr, "null ptr check");
    CHECK_FATAL(realocc->mestmt->GetChiList() != nullptr, "null ptr check");
    realocc->mestmt->GetChiList()->clear();
    SrcPosition savedSrcpos = realocc->mestmt->srcPos;
    BB *savedBb = realocc->mestmt->bb;
    MeStmt *savedPrev = realocc->mestmt->prev;
    MeStmt *savedNext = realocc->mestmt->next;
    // change original dassign/maydassign to regassign;
    // use placement new to modify in place, because other occ nodes are pointing
    // to this statement in order to get to the rhs expression;
    // this assume AssignMeStmt has smaller size then DassignMeStmt and
    // MaydassignMeStmt
    AssignMeStmt *rass = new (realocc->mestmt) AssignMeStmt(OP_regassign, static_cast<RegMeExpr *>(regorvar), savedRhs);
    rass->srcPos = savedSrcpos;
    rass->bb = savedBb;
    rass->prev = savedPrev;
    rass->next = savedNext;
    regorvar->SetDefByStmt(rass);
    // create new dassign for original lhs
    AssignMeStmt *newdass = irMap->CreateAssignMeStmt(thelhs, regorvar, savedBb);
    thelhs->SetDefByStmt(newdass);
    savedBb->InsertMeStmtAfter(realocc->mestmt, newdass);
  } else {
    CHECK_FATAL(kOpcodeInfo.IsCallAssigned(realocc->mestmt->op), "LHS real occurrence has unrecognized stmt type");
    MapleVector<MustDefMeNode> *mustdefList = realocc->mestmt->GetMustDefList();
    ASSERT(mustdefList != nullptr && !mustdefList->empty(), "empty mustDef in callassigned stmt");
    MustDefMeNode *mustdefmenode = &mustdefList->front();
    if (regorvar->meOp == kMeOpReg) {
      VarMeExpr *thelhs = static_cast<VarMeExpr *>(mustdefmenode->lhs);
      // change mustDef lhs to regorvar
      mustdefmenode->UpdateLhs(regorvar);
      // create new dassign for original lhs
      AssignMeStmt *newdass = irMap->CreateAssignMeStmt(thelhs, regorvar, realocc->mestmt->bb);
      thelhs->SetDefByStmt(newdass);
      realocc->mestmt->bb->InsertMeStmtAfter(realocc->mestmt, newdass);
    } else {
      CHECK_FATAL(false, "GenerateSaveRealocc: non-reg temp for callassigned LHS occurrence NYI");
    }
  }
  realocc->saved_expr = regorvar;
}

void MeSSALPre::GenerateReloadRealocc(MeRealOcc *realocc) {
  CHECK_FATAL(!realocc->is_lhs, "GenerateReloadRealocc: cannot be LHS occurrence");
  MeExpr *regorvar = nullptr;
  MeOccur *defocc = realocc->def;
  if (defocc->occty == kOccReal) {
    MeRealOcc *defrealocc = static_cast<MeRealOcc *>(defocc);
    regorvar = defrealocc->saved_expr;
  } else if (defocc->occty == kOccPhiocc) {
    MePhiOcc *defphiocc = static_cast<MePhiOcc *>(defocc);
    regorvar = defphiocc->regPhi->lhs;
  } else if (defocc->occty == kOccInserted) {
    MeInsertedOcc *definsertedocc = static_cast<MeInsertedOcc *>(defocc);
    regorvar = definsertedocc->saved_expr;
  } else {
    CHECK_FATAL(false, "NYI");
  }
  ASSERT(regorvar, "temp not yet generated");
  // replace realocc->mestmt's occ with regorvar
  irMap->ReplaceMeExprStmt(realocc->mestmt, realocc->meexpr, regorvar);
}

// the variable in realz is defined by a phi; replace it by the jth phi opnd
MeExpr *MeSSALPre::PhiOpndFromRes(MeRealOcc *realz, uint32 j) {
  MeOccur *defz = realz->def;
  CHECK_FATAL(defz && (defz->occty == kOccPhiocc), "must be def by phiocc");
  MeExpr *meexprz = realz->meexpr;
  BB *ephibb = static_cast<MePhiOcc *>(defz)->mirbb;
  MeExpr *retvar = GetReplaceMeExpr(meexprz, ephibb, j);
  return retvar ? retvar : meexprz;
}

void MeSSALPre::ComputeVarAndDfPhis() {
  varphi_dfns.clear();
  dfphi_dfns.clear();
  const MapleVector<MeRealOcc *> &realoccList = work_cand->real_occs;
  CHECK_FATAL(dominance->bbVec.size() > 0, "size to be allocated is 0");
  for (MapleVector<MeRealOcc *>::const_iterator it = realoccList.begin(); it != realoccList.end(); it++) {
    MeRealOcc *realocc = *it;
    BB *defBb = realocc->mirbb;
    GetIterDomFrontier(defBb, &dfphi_dfns);
    MeExpr *meexpr = realocc->meexpr;
    if (meexpr->meOp == kMeOpVar) {
      SetVarPhis(meexpr);
    }
  }
}

// return true if it passes screening
bool MeSSALPre::ScreenRHS4LHSoccur(const MeExpr *rhs) const {
  if (MeOption::rcLowering) {
    return true;  // no need to screen because there is no more rcinsertion later
  }
  switch (rhs->meOp) {
    case kMeOpGcmalloc:
      return false;
    case kMeOpOp:
      return rhs->op != OP_gcmallocjarray;
    case kMeOpVar:
      return rhs->primType != PTY_ref;
    case kMeOpIvar:
      return rhs->primType != PTY_ref;
    case kMeOpReg:
      return !((rhs->primType == PTY_ref || rhs->primType == PTY_ptr) &&
               static_cast<const RegMeExpr *>(rhs)->regIdx == -kSregThrownval);
    default:
      return true;
  }
}

void MeSSALPre::BuildEntryLhsOcc4Formals() {
  if (prekind == kAddrPre) {
    return;
  }
  VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(work_cand->themeexpr);
  OriginalSt *ost = varmeexpr->ost;
  if (!ost->isFormal || ost->addressTaken) {
    return;
  }
  if (ost->fieldID != 0) {
    return;
  }
  if (JAVALANG && assignedFormals.find(ost->index) != assignedFormals.end()) {
    return;  // the formal's memory location has to be preserved
  }
  // if (ost->GetMIRSymbol()->GetName() == "_this")
  //  return;  // because later phases will manifest uses of _this
  // get the zero version VarMeExpr node
  VarMeExpr *zerovers = irMap->GetOrCreateZeroVersionVarMeExpr(ost);
  MeRealOcc *occ = ssapre_mp->New<MeRealOcc>(static_cast<MeStmt*>(nullptr), 0, zerovers);
  MapleVector<MeRealOcc *>::iterator occit = work_cand->real_occs.begin();
  work_cand->real_occs.insert(occit, occ);  // insert at beginning
  occ->is_lhs = true;
  occ->is_formal_at_entry = true;
  occ->mirbb = func->first_bb_;
}

void MeSSALPre::BuildWorkListLHSOcc(MeStmt *mestmt, int32 seqstmt) {
  if (prekind == kAddrPre) {
    return;
  }
  if (mestmt->op == OP_dassign || mestmt->op == OP_maydassign) {
    VarMeExpr *lhs = mestmt->GetVarLhs();
    CHECK_FATAL(lhs != nullptr, "null ptr check");
    OriginalSt *ost = lhs->ost;
    if (mirModule->IsCModule() && ost->fieldID != 0) {
      MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ost->tyIdx);
      if (ty->typeKind == kTypeBitField) {
        return;  // no advantage for bitfields
      }
    }
    if (ost->isFormal) {
      assignedFormals.insert(ost->index);
    }
    CHECK_FATAL(mestmt->GetRhs() != nullptr, "null ptr check");
    if (!ScreenRHS4LHSoccur(mestmt->GetRhs())) {
      return;
    }
    if (ost->IsVolatile()) {
      return;
    }
    if (lhs->primType == PTY_agg) {
      return;
    }
    CreateRealOcc(mestmt, seqstmt, lhs, false, true);
  } else if (kOpcodeInfo.IsCallAssigned(mestmt->op)) {
    MapleVector<MustDefMeNode> *mustdefList = mestmt->GetMustDefList();
    if (mustdefList->empty()) {
      return;
    }
    if (mustdefList->front().lhs->meOp != kMeOpVar) {
      return;
    }
    VarMeExpr *thelhs = static_cast<VarMeExpr *>(mustdefList->front().lhs);
    OriginalSt *ost = thelhs->ost;
    if (ost->isFormal) {
      assignedFormals.insert(ost->index);
    }
    if (thelhs->primType == PTY_ref && !MeOption::rcLowering) {
      return;
    }
    if (ost->IsVolatile()) {
      return;
    }
    if (thelhs->primType == PTY_agg) {
      return;
    }
    CreateRealOcc(mestmt, seqstmt, thelhs, false, true);
  } else {
    return;
  }
}

void MeSSALPre::CreateMembarOccAtCatch(BB *bb) {
  // go thru all workcands and insert a membar occurrence for each of them
  for (uint32 i = 0; i < worklist.size() && i <= prelimit; i++) {
    PreWorkCand *wkcand = worklist[i];
    MeRealOcc *newocc = ssapre_mp->New<MeRealOcc>(nullptr, 0, wkcand->themeexpr);
    newocc->occty = kOccMembar;
    newocc->mirbb = bb;
    wkcand->AddRealOccAsLast(newocc, GetPuidx(bb));
    if (prekind == kAddrPre) {
      continue;
    }
    VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(wkcand->themeexpr);
    if (varmeexpr->ost->isFormal) {
      assignedFormals.insert(varmeexpr->ost->index);
    }
  }
}

/// only handle the leaf of load, because all other expressions has been done by
/// previous SSAPre
void MeSSALPre::BuildWorkListExpr(MeStmt *mestmt, int32 seqstmt, MeExpr *meexpr, bool isrebuild, MeExpr *tempvar,
                                  bool isRootExpr) {
  MeExprOp meOp = meexpr->meOp;
  switch (meOp) {
    case kMeOpVar: {
      if (prekind != kLoadPre) {
        break;
      }
      VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(meexpr);
      OriginalSt *ost = varmeexpr->ost;
      if (ost->IsVolatile()) {
        break;
      }
      MIRSymbol *sym = ost->GetMIRSymbol();
      if (sym->instrumented == 1 && !MeOption::placementrc) {
        // not doing because its SSA form is not complete
        break;
      }
      if (meexpr->primType == PTY_agg) {
        break;
      }
      CreateRealOcc(mestmt, seqstmt, meexpr, false);
      break;
    }
    case kMeOpAddrof: {
      if (prekind != kAddrPre) {
        break;
      }
      AddrofMeExpr *addrofmeexpr = static_cast<AddrofMeExpr *>(meexpr);
      OriginalSt *ost = ssaTab->GetOriginalStFromid(addrofmeexpr->ostIdx);
      if (ost->isLocal) { // skip lpre for stack addresses as they are cheap
        break;
      }
      CreateRealOcc(mestmt, seqstmt, meexpr, false);
      break;
    }
    case kMeOpAddroffunc: {
      if (prekind != kAddrPre) {
        break;
      }
      CreateRealOcc(mestmt, seqstmt, meexpr, false);
      break;
    }
    case kMeOpIvar:
    case kMeOpOp:
    case kMeOpNary: {
      for (uint32 i = 0; i < meexpr->NumMeExprOpnds(); i++) {
        BuildWorkListExpr(mestmt, seqstmt, meexpr->GetOpnd(i), false, nullptr, false);
      }
      break;
    }
    default:
      break;
  }
  return;
}

void MeSSALPre::BuildWorkList() {
  MeFunction *func = meirmap->mirFunc;
  uint32 numbbs = dominance->dtPreOrder.size();

  if (numbbs > kDolpreBbsLimit) {
    return;
  }

  const MapleVector<BBId> &preorderDt = dominance->dtPreOrder;
  for (uint32 i = 0; i < numbbs; i++) {
    CHECK(preorderDt[i].idx < func->bbVec.size(), "index out of range in MeSSALPre::BuildWorkList");
    BB *bb = func->bbVec[preorderDt[i].idx];
    BuildWorkListBB(bb);
  }
}

void MeSSALPre::FindLoopHeadBBs(IdentifyLoops *identloops) {
  for (LoopDesc *mapleloop : identloops->meloops) {
    if (mapleloop->head != nullptr) {
      loophead_bbs.insert(mapleloop->head->id);
    }
  }
}

AnalysisResult *MeDoSSALPre::Run(MeFunction *func, MeFuncResultMgr *m) {
  static uint32 pUcount = 0;  // count PU to support the lprePULimit option
  if (pUcount > MeOption::lprePULimit) {
    pUcount++;
    return nullptr;
  }
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
  ASSERT(dom != nullptr, "");

  MeIRMap *irMap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func));
  ASSERT(irMap != nullptr, "");

  IdentifyLoops *identloops = static_cast<IdentifyLoops *>(m->GetAnalysisResult(MeFuncPhase_MELOOP, func));
  CHECK_FATAL(identloops != nullptr, "meloop has problem");

  bool lprePuLimitSpecified = MeOption::lprePULimit != UINT32_MAX;
  uint32 lprelimitUsed =
    (lprePuLimitSpecified && pUcount != MeOption::lprePULimit) ? UINT32_MAX : MeOption::lpreLimit;

  {
    // first time: LoadPre
    MemPool *ssapremp = mempoolctrler.NewMemPool(PhaseName().c_str());
    MemPool *percandmp = mempoolctrler.NewMemPool("Per LoadPre Candidate");

    MeSSALPre ssalpre(func, irMap, dom, ssapremp, percandmp, SSAPre::kLoadPre, lprelimitUsed);
    if (MeOption::rcLowering) {
      ssalpre.rclowering_on = true;
    }
    if (MeOption::regreadAtReturn) {
      ssalpre.regreadAtReturn = true;
    }
    if (MeOption::spillatcatch) {
      ssalpre.spillatcatch = true;
    }
    if (lprePuLimitSpecified && pUcount == MeOption::lprePULimit && lprelimitUsed != UINT32_MAX)
      LogInfo::MapleLogger() << "applying LPRE limit " << lprelimitUsed << " in function " << func->mirFunc->GetName() << endl;
    MIRFunction *mirfunction = func->mirFunc;
    if (DEBUGFUNC(func)) {
      ssalpre.ssapredebug = true;
    }

    if (MeOption::lprespeculate && !func->HasException()) {
      ssalpre.FindLoopHeadBBs(identloops);
    }

    ssalpre.ApplySSAPRE();

    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "\n==============after LoadPre =============" << endl;
      irMap->Dump();
    }

    mempoolctrler.DeleteMemPool(ssapremp);
    mempoolctrler.DeleteMemPool(percandmp);
  }

  LowerGlobals lowerglobals(func, func->meSSATab);
  lowerglobals.Run();

  {
    // second time: LoadPre
    MemPool *ssapremp = mempoolctrler.NewMemPool(PhaseName().c_str());
    MemPool *percandmp = mempoolctrler.NewMemPool("Per AddrPre Candidate");
    MeSSALPre ssalpre(func, irMap, dom, ssapremp, percandmp, SSAPre::kAddrPre, lprelimitUsed);
    if (MeOption::spillatcatch) {
      ssalpre.spillatcatch = true;
    }
    MIRFunction *mirfunction = func->mirFunc;
    if (DEBUGFUNC(func)) {
      ssalpre.ssapredebug = true;
    }

    if (MeOption::lprespeculate && !func->HasException()) {
      ssalpre.FindLoopHeadBBs(identloops);
    }

    ssalpre.ApplySSAPRE();

    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "\n==============after AddrPre =============" << endl;
      irMap->Dump();
    }

    mempoolctrler.DeleteMemPool(ssapremp);
    mempoolctrler.DeleteMemPool(percandmp);
  }

  pUcount++;
  return nullptr;
}

}  // namespace maple
