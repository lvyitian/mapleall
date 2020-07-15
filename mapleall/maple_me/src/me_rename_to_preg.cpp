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

#include "me_rename_to_preg.h"
#include "alias_class.h"
#include "mir_builder.h"
#include "me_irmap.h"

// This phase mainly renames the variables to pseudo register.
// Only non-ref-type variables (including parameters) with no alias are
// workd on here.  Remaining variables are left to LPRE phase.  This is
// because for ref-type variables, their stores have to be left intact.

namespace maple {

RegMeExpr *SSARename2Preg::RenameVar(VarMeExpr *varmeexpr) {
  if (varmeexpr->ost->fieldID != 0) {
    return nullptr;
  }
  const OriginalSt *ost = varmeexpr->ost;
  if (ost->indirectLev != 0) {
    return nullptr;
  }
  MIRSymbol *mirst = ost->GetMIRSymbol();
  if (mirst->GetAttr(ATTR_localrefvar)) {
    return nullptr;
  }
  if (ost->isFormal && varmeexpr->primType == PTY_ref) {
    return nullptr;
  }
  if (sym2reg_map.find(ost->index) != sym2reg_map.end()) {
    // replaced previously
    MapleUnorderedMap<int, RegMeExpr *>::iterator verit = vstidx2reg_map.find(varmeexpr->exprID);
    RegMeExpr *varreg = nullptr;
    if (verit != vstidx2reg_map.end()) {
      varreg = verit->second;
    } else {
      OriginalSt *pregOst = sym2reg_map[ost->index];
      varreg = meirmap->CreateRegMeExprVersion(pregOst);
      vstidx2reg_map.insert(make_pair(varmeexpr->exprID, varreg));
    }
    return varreg;
  } else {
    const OriginalSt *origOst = ost;
    if (ost->indexRenamedFrom.idx != 0) {  // change to use the original ost
      origOst = ssaTab->GetSymbolOriginalStFromid(ost->indexRenamedFrom);
    }
    if (origOst->index.idx >= aliasclass->osym2Elem.size()) {
      return nullptr;
    }
    if (!mirst->IsLocal()) {
      return nullptr;
    }
    if (origOst->addressTaken) {
      return nullptr;
    }
    AliasElem *aliaselem = GetAliasElem(origOst);
    if (aliaselem && aliaselem->classSet) {
      return nullptr;
    }
    RegMeExpr *curtemp = nullptr;
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(mirst->tyIdx);
    if (ty->typeKind != kTypeScalar && ty->typeKind != kTypePointer) {
      return nullptr;
    }
    curtemp = meirmap->CreateRegMeExpr(ty);
    OriginalSt *pregOst = ssaTab->originalStTable.CreatePregOriginalSt(curtemp->regIdx, func->mirFunc->puIdx);
    pregOst->isFormal = ost->isFormal;
    sym2reg_map[ost->index] = pregOst;
    vstidx2reg_map.insert(make_pair(varmeexpr->exprID, curtemp));
    if (ost->isFormal) {
      uint32 parmindex = func->mirFunc->GetFormalIndex(mirst);
      CHECK_FATAL(parm_used_vec[parmindex], "parm_used_vec not set correctly");
      if (!reg_formal_vec[parmindex]) {
        reg_formal_vec[parmindex] = curtemp;
      }
    }
    if (DEBUGFUNC(func)) {
      ost->Dump();
      LogInfo::MapleLogger() << "(ost idx " << ost->index.idx << ") renamed to ";
      pregOst->Dump();
      LogInfo::MapleLogger() << endl;
    }
    return curtemp;
  }
}

void SSARename2Preg::Rename2PregCallReturn(MapleVector<MustDefMeNode> &mustdeflist) {
  if (mustdeflist.empty()) {
    return;
  }
  CHECK_FATAL(mustdeflist.size() == 1, "NYI");
  {
    MustDefMeNode &mustdefmenode = mustdeflist.front();
    MeExpr *melhs = mustdefmenode.lhs;
    if (melhs->meOp != kMeOpVar) {
      return;
    };
    VarMeExpr *lhs = static_cast<VarMeExpr *>(melhs);
    SetupParmUsed(lhs);

    RegMeExpr *varreg = RenameVar(lhs);
    if (varreg != nullptr) {
      mustdefmenode.UpdateLhs(varreg);
    }
  }
}

// update regphinode operands
void SSARename2Preg::UpdateRegPhi(MePhiNode *mevarphinode, MePhiNode *regphinode, const RegMeExpr *curtemp,
                                  const VarMeExpr *lhs) {
  // update phi's opnds
  for (uint32 i = 0; i < mevarphinode->opnds.size(); i++) {
    ScalarMeExpr *opndexpr = mevarphinode->opnds[i];
    ASSERT(opndexpr->ost->index == lhs->ost->index, "phi is not correct");
    MapleUnorderedMap<int, RegMeExpr *>::iterator verit = vstidx2reg_map.find(opndexpr->exprID);
    RegMeExpr *opndtemp = nullptr;
    if (verit == vstidx2reg_map.end()) {
      opndtemp = meirmap->CreateRegMeExprVersion(curtemp);
      vstidx2reg_map.insert(make_pair(opndexpr->exprID, opndtemp));
    } else {
      opndtemp = verit->second;
    }
    regphinode->opnds.push_back(opndtemp);
  }
}

void SSARename2Preg::Rename2PregPhi(BB *mebb, MePhiNode *mevarphinode,
       MapleMap<OStIdx, MePhiNode *> &regPhiList) {
  VarMeExpr *lhs = static_cast<VarMeExpr*>(mevarphinode->lhs);
  SetupParmUsed(lhs);
  RegMeExpr *lhsreg = RenameVar(lhs);
  if (lhsreg != nullptr) {
    MePhiNode *regphinode = meirmap->CreateMePhi(lhsreg);
    regphinode->defBB = mevarphinode->defBB;
    UpdateRegPhi(mevarphinode, regphinode, lhsreg, lhs);
    regPhiList.insert(make_pair(lhsreg->ost->index, regphinode));
  }
}

void SSARename2Preg::Rename2PregLeafRHS(MeStmt *mestmt, VarMeExpr *varmeexpr) {
  SetupParmUsed(varmeexpr);
  RegMeExpr *varreg = RenameVar(varmeexpr);
  if (varreg != nullptr) {
    meirmap->ReplaceMeExprStmt(mestmt, varmeexpr, varreg);
  }
}

void SSARename2Preg::Rename2PregLeafLHS(MeStmt *mestmt, VarMeExpr *varmeexpr) {
  SetupParmUsed(varmeexpr);
  RegMeExpr *varreg = RenameVar(varmeexpr);
  if (varreg != nullptr) {
    Opcode desop = mestmt->op;
    CHECK_FATAL(desop == OP_dassign || desop == OP_maydassign, "NYI");
    MeExpr *oldrhs = (desop == OP_dassign) ? (static_cast<DassignMeStmt *>(mestmt)->rhs)
                                           : (static_cast<MaydassignMeStmt *>(mestmt)->rhs);
    AssignMeStmt *regssmestmt = meirmap->New<AssignMeStmt>(OP_regassign, varreg, oldrhs);
    regssmestmt->CopyBase(mestmt);
    mestmt->bb->InsertMeStmtBefore(mestmt, regssmestmt);
    mestmt->bb->RemoveMeStmt(mestmt);
  }
}

void SSARename2Preg::SetupParmUsed(const VarMeExpr *varmeexpr) {
  const OriginalSt *ost = varmeexpr->ost;
  if (ost->isFormal && ost->IsSymbol()) {
    MIRSymbol *mirst = ost->GetMIRSymbol();
    uint32 index = func->mirFunc->GetFormalIndex(mirst);
    parm_used_vec[index] = true;
  }
}

// only handle the leaf of load, because all other expressions has been done by previous SSAPre
void SSARename2Preg::Rename2PregExpr(MeStmt *mestmt, MeExpr *meexpr) {
  MeExprOp meOp = meexpr->meOp;
  switch (meOp) {
    case kMeOpIvar:
    case kMeOpOp:
    case kMeOpNary: {
      for (uint32 i = 0; i < meexpr->NumMeExprOpnds(); i++) {
        Rename2PregExpr(mestmt, meexpr->GetOpnd(i));
      }
      break;
    }
    case kMeOpVar:
      Rename2PregLeafRHS(mestmt, static_cast<VarMeExpr *>(meexpr));
      break;
    case kMeOpAddrof: {
      AddrofMeExpr *addrofx = static_cast<AddrofMeExpr *>(meexpr);
      const OriginalSt *ost = ssaTab->GetOriginalStFromid(addrofx->ostIdx);
      if (ost->isFormal) {
        MIRSymbol *mirst = ost->GetMIRSymbol();
        uint32 index = func->mirFunc->GetFormalIndex(mirst);
        parm_used_vec[index] = true;
      }
      break;
    }
    default:
      break;
  }
  return;
}

void SSARename2Preg::Rename2PregStmt(MeStmt *stmt) {
  Opcode op = stmt->op;
  switch (op) {
    case OP_dassign:
    case OP_maydassign: {
      CHECK_FATAL(stmt->GetRhs() && stmt->GetVarLhs(), "null ptr check");
      Rename2PregExpr(stmt, stmt->GetRhs());
      Rename2PregLeafLHS(stmt, stmt->GetVarLhs());
      break;
    }
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_icallassigned:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned: {
      for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
        Rename2PregExpr(stmt, stmt->GetMeStmtOpnd(i));
      }
      MapleVector<MustDefMeNode> *mustdeflist = stmt->GetMustDefList();
      Rename2PregCallReturn(*mustdeflist);
      break;
    }
    case OP_iassign: {
      IassignMeStmt *ivarstmt = static_cast<IassignMeStmt *>(stmt);
      Rename2PregExpr(stmt, ivarstmt->rhs);
      Rename2PregExpr(stmt, ivarstmt->lhsVar->base);
      break;
    }
    default:
      for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
        Rename2PregExpr(stmt, stmt->GetMeStmtOpnd(i));
      }
      break;
  }
}

void SSARename2Preg::UpdateMirFunctionFormal() {
  MIRFunction *mirFunc = func->mirFunc;
  MIRBuilder *mirbuilder = mirModule->mirBuilder;
  for (uint32 i = 0; i < mirFunc->formalDefVec.size(); i++) {
    if (!parm_used_vec[i]) {
      // in this case, the paramter is not used by any statement, promote it
      MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(mirFunc->formalDefVec[i].formalTyIdx);
      PregIdx16 regIdx = mirFunc->pregTab->CreatePreg(mirType->primType, mirType->primType == PTY_ref ? mirType : nullptr);
      mirFunc->formalDefVec[i].formalSym = mirbuilder->CreatePregFormalSymbol(mirType->tyIdx, regIdx, mirFunc);
    } else {
      RegMeExpr *regformal = reg_formal_vec[i];
      if (regformal) {
        PregIdx16 regIdx = regformal->regIdx;
        MIRSymbol *oldformalst = mirFunc->formalDefVec[i].formalSym;
        MIRSymbol *newformalst = mirbuilder->CreatePregFormalSymbol(oldformalst->tyIdx, regIdx, mirFunc);
        mirFunc->formalDefVec[i].formalSym = newformalst;
      }
    }
  }
}

void SSARename2Preg::Init() {
  uint32 formalsize = func->mirFunc->formalDefVec.size();
  parm_used_vec.resize(formalsize);
  reg_formal_vec.resize(formalsize);
}

void SSARename2Preg::RunSelf() {
  Init();
  for (BB *mebb : func->bbVec) {
    if (mebb == nullptr) {
      continue;
    }
    MeStmt *nextstmt = nullptr;
    // rename the phi'ss
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << " working on phi part of BB" << mebb->id.idx << endl;
    }
    MapleMap<OStIdx, MePhiNode *> &phiList = mebb->mePhiList;
    MapleMap<OStIdx, MePhiNode *> regPhiList(func->alloc.Adapter());
    for (std::pair<const OStIdx, MePhiNode *> apair : phiList) {
      if (!apair.second->UseReg()) {
        Rename2PregPhi(mebb, apair.second, regPhiList);
      }
    }
    phiList.insert(regPhiList.begin(), regPhiList.end());

    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << " working on stmt part of BB" << mebb->id.idx << endl;
    }
    for (auto stmt : mebb->meStmtList) {
      nextstmt = stmt->next;
      Rename2PregStmt(stmt);
    }
  }

  UpdateMirFunctionFormal();
}

void SSARename2Preg::PromoteEmptyFunction() {
  Init();
  UpdateMirFunctionFormal();
}

AnalysisResult *MeDoSSARename2Preg::Run(MeFunction *func, MeFuncResultMgr *m) {
  MemPool *renamemp = mempoolctrler.NewMemPool(PhaseName().c_str());
  if (func->bbVec.size() == 0) {
    // empty function, we only promote the parameter
    SSARename2Preg emptyrenamer(renamemp, func, nullptr, nullptr);
    emptyrenamer.PromoteEmptyFunction();
    mempoolctrler.DeleteMemPool(renamemp);
    return nullptr;
  }

  MeIRMap *irMap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func));
  ASSERT(irMap != nullptr, "");

  AliasClass *aliasclass = static_cast<AliasClass *>(m->GetAnalysisResult(MeFuncPhase_ALIASCLASS, func));
  ASSERT(aliasclass != nullptr, "");

  MIRFunction *mirfunction = func->mirFunc;

  SSARename2Preg ssarename2preg(renamemp, func, irMap, aliasclass);
  ssarename2preg.RunSelf();
  if (DEBUGFUNC(func)) {
    irMap->Dump();
  }
  mempoolctrler.DeleteMemPool(renamemp);

  return nullptr;
}

}  // namespace maple
