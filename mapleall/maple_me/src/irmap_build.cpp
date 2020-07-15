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

#include "irmap_build.h"
#include "prop.h"

// Methods to convert Maple IR to MeIR

namespace maple {

VarMeExpr *IrMapBuild::GetOrCreateVarFromVerSt(const VersionSt *verst) {
  uint32 vindex = verst->index;
  ASSERT(vindex < irMap->verst2MeExprTable.size(), "GetOrCreateVarFromVerSt: index %d is out of range", vindex);
  MeExpr *meexpr = irMap->verst2MeExprTable.at(vindex);
  if (meexpr) {
    return static_cast<VarMeExpr *>(meexpr);
  }

  OriginalSt *ost = verst->ost;
  ASSERT(ost->ostType == OriginalSt::kSymbolOst, "GetOrCreateVarFromVerSt: wrong ost_type");
  VarMeExpr *varx = irMap->New<VarMeExpr>(irMap->exprID++, ost, vindex,
         GlobalTables::GetTypeTable().typeTable[ost->tyIdx.GetIdx()]->primType);
  varx->ost->fieldID = ost->fieldID;
  irMap->verst2MeExprTable[vindex] = varx;
  return varx;
}

RegMeExpr *IrMapBuild::GetOrCreateRegFromVerSt(const VersionSt *verst) {
  uint32 vindex = verst->index;
  ASSERT(vindex < irMap->verst2MeExprTable.size(), " GetOrCreateRegFromVerSt: index %d is out of range", vindex);
  MeExpr *meexpr = irMap->verst2MeExprTable[vindex];
  if (meexpr) {
    return static_cast<RegMeExpr *>(meexpr);
  }

  OriginalSt *ost = verst->ost;
  ASSERT(ost->ostType == OriginalSt::kPregOst, "GetOrCreateRegFromVerSt: PregOST expected");
  RegMeExpr *regx = irMap->New<RegMeExpr>(irMap->exprID++, ost, vindex, ost->GetMIRPreg()->primType);
  irMap->verst2MeExprTable[vindex] = regx;
  return regx;
}

MeExpr *IrMapBuild::BuildLhsVar(const VersionSt *verst, DassignMeStmt *defmestmt, DassignNode *dassign) {
  VarMeExpr *medef = GetOrCreateVarFromVerSt(verst);
  medef->def.defStmt = defmestmt;
  medef->defBy = kDefByStmt;
  irMap->verst2MeExprTable.at(verst->index) = medef;
  return medef;
}

MeExpr *IrMapBuild::BuildLhsReg(const VersionSt *verst, AssignMeStmt *defmestmt, const RegassignNode *regassign) {
  RegMeExpr *medef = GetOrCreateRegFromVerSt(verst);
  medef->primType = regassign->primType;
  medef->def.defStmt = defmestmt;
  medef->defBy = kDefByStmt;
  irMap->verst2MeExprTable.at(verst->index) = medef;
  return medef;
}

// build Me chilist from MayDefNode list
void IrMapBuild::BuildChiList(MeStmt *mestmt, MapleMap<OStIdx, MayDefNode> &mayDefNodes,
                         MapleMap<OStIdx, ChiMeNode *> &outlist) {
  for (MapleMap<OStIdx, MayDefNode>::iterator it = mayDefNodes.begin(); it != mayDefNodes.end(); it++) {
    MayDefNode &maydefnode = it->second;
    VersionSt *opndst = maydefnode.opnd;
    VersionSt *resst = maydefnode.result;
    ChiMeNode *chimestmt = irMap->New<ChiMeNode>(mestmt);
    chimestmt->rhs = GetOrCreateVarFromVerSt(opndst);
    VarMeExpr *lhs = GetOrCreateVarFromVerSt(resst);
    lhs->defBy = kDefByChi;
    lhs->def.defChi = chimestmt;
    chimestmt->lhs = lhs;
    outlist.insert(std::make_pair(lhs->ost->index, chimestmt));
  }
}

void IrMapBuild::BuildMustdefList(MeStmt *mestmt, MapleVector<MustDefNode> &mustdeflist,
                             MapleVector<MustDefMeNode> &mustdefList) {
  for (MapleVector<MustDefNode>::iterator it = mustdeflist.begin(); it != mustdeflist.end(); it++) {
    MustDefNode &mustdefnode = *it;
    VersionSt *verSt = mustdefnode.result;
    VarMeExpr *lhs = GetOrCreateVarFromVerSt(verSt);
    mustdefList.push_back(MustDefMeNode(lhs, mestmt));
  }
}

void IrMapBuild::BuildPhiMeNode(BB *bb) {
  for (MapleMap<OriginalSt *, PhiNode>::iterator phiit = bb->phiList.begin(); phiit != bb->phiList.end(); phiit++) {
    OriginalSt *origst = (*phiit).first;
    VersionSt *verst = (*phiit).second.result;
    MePhiNode *phimenode = irMap->NewInPool<MePhiNode>();

    if (origst->ostType == OriginalSt::kPregOst) {
      RegMeExpr *medef = GetOrCreateRegFromVerSt(verst);
      phimenode->UpdateLhs(medef);
      phimenode->defBB = bb;
      // build phi operands
      for (MapleVector<VersionSt *>::iterator opndit = (*phiit).second.phiOpnds.begin();
           opndit != (*phiit).second.phiOpnds.end(); opndit++) {
        phimenode->opnds.push_back(GetOrCreateRegFromVerSt(*opndit));
      }
      bb->mePhiList.insert(std::make_pair(medef->ost->index, phimenode));
    } else {
      VarMeExpr *medef = GetOrCreateVarFromVerSt(verst);
      phimenode->UpdateLhs(medef);
      phimenode->defBB = bb;
      // build phi operands
      for (MapleVector<VersionSt *>::iterator opndit = (*phiit).second.phiOpnds.begin();
           opndit != (*phiit).second.phiOpnds.end(); opndit++) {
        phimenode->opnds.push_back(GetOrCreateVarFromVerSt(*opndit));
      }
      bb->mePhiList.insert(std::make_pair(medef->ost->index, phimenode));
    }
  }
}

void IrMapBuild::BuildMuList(MapleMap<OStIdx, MayUseNode> &mayuselist, MapleMap<OStIdx, VarMeExpr *> &mulist) {
  for (std::pair<OStIdx, MayUseNode> mapitem : mayuselist) {
    MayUseNode &mayusenode = mapitem.second;
    VersionSt *verSt = mayusenode.opnd;
    VarMeExpr *varmeexpr = GetOrCreateVarFromVerSt(verSt);
    mulist.insert(std::make_pair(varmeexpr->ost->index, varmeexpr));
  }
}

MeExpr *IrMapBuild::BuildExpr(BaseNode *mirnode, bool atParm, bool noProp) {
  if (mirnode->IsBinaryNode()) {
    BinaryNode *binarynode = static_cast<BinaryNode *>(mirnode);
    if (binarynode->IsCommutative() && binarynode->bOpnd[0]->op == OP_constval) {
      // swap the 2 operands
      BaseNode *tmpnode = binarynode->bOpnd[0];
      binarynode->bOpnd[0] = binarynode->bOpnd[1];
      binarynode->bOpnd[1] = tmpnode;
    }
  }
  MeExpr *retmeexpr = nullptr;
  Opcode op = mirnode->op;
  switch (op) {
    case OP_addrof: {
      AddrofSSANode *addrofnode = static_cast<AddrofSSANode *>(mirnode);
      VersionSt *verst = addrofnode->ssaVar;
      OriginalSt *ost = verst->ost;
      AddrofMeExpr addrofme(-1, mirnode->primType,  ost->index);
      addrofme.fieldID = addrofnode->fieldID;
      retmeexpr = irMap->HashMeExpr(&addrofme);
      break;
    }
    case OP_dread: {
      AddrofSSANode *addrofnode = static_cast<AddrofSSANode *>(mirnode);
      VersionSt *verst = addrofnode->ssaVar;
      VarMeExpr *varmeexpr = GetOrCreateVarFromVerSt(verst);
      CHECK_FATAL(!verst->ost->IsPregSymbol(), "not expect preg symbol here");
      varmeexpr->primType = GlobalTables::GetTypeTable().typeTable[verst->ost->tyIdx.GetIdx()]->primType;
      varmeexpr->ost->fieldID = addrofnode->fieldID;
      if (propagater && !noProp) {
        MeExpr *propedMeExpr = propagater->PropVar(varmeexpr, atParm, true);
        MeExpr *simplifiedMeexpr = nullptr;
        if (propedMeExpr->meOp == kMeOpOp) {
           simplifiedMeexpr =propagater->SimplifyMeExpr(static_cast<OpMeExpr *>(propedMeExpr));
        }
        retmeexpr = simplifiedMeexpr ? simplifiedMeexpr : propedMeExpr;
      } else {
        retmeexpr = varmeexpr;
      }
      break;
    }
    case OP_regread: {
      RegreadSSANode *regnode = static_cast<RegreadSSANode *>(mirnode);
      VersionSt *verst = regnode->ssaVar;
      RegMeExpr *regmeexpr = GetOrCreateRegFromVerSt(verst);
      regmeexpr->primType = mirnode->primType;
      retmeexpr = regmeexpr;
      break;
    }
    case OP_addroffunc: {
      AddroffuncNode *addfuncnode = static_cast<AddroffuncNode *>(mirnode);
      PUIdx puIdx = addfuncnode->puIdx;
      AddroffuncMeExpr addrfuncme(-1, puIdx);
      retmeexpr = irMap->HashMeExpr(&addrfuncme);
      break;
    }
    case OP_gcmalloc:
    case OP_gcpermalloc:
    case OP_stackmalloc: {
      GCMallocNode *gcmallocnode = static_cast<GCMallocNode *>(mirnode);
      TyIdx tyIdx = gcmallocnode->tyIdx;
      GcmallocMeExpr gcmallocme(-1, op, mirnode->primType,  tyIdx);
      retmeexpr = irMap->HashMeExpr(&gcmallocme);
      break;
    }
    case OP_sizeoftype: {
      SizeoftypeNode *sizeoftypenode = static_cast<SizeoftypeNode *>(mirnode);
      SizeoftypeMeExpr sizemeexpr(-1, mirnode->primType, sizeoftypenode->tyIdx);
      retmeexpr = irMap->HashMeExpr(&sizemeexpr);
      break;
    }
    case OP_fieldsdist: {
      FieldsDistNode *fieldsDistNode = static_cast<FieldsDistNode*>(mirnode);
      FieldsDistMeExpr fieldsDistExpr(-1, mirnode->primType, fieldsDistNode->GetTyIdx(),
          fieldsDistNode->GetFiledID1(), fieldsDistNode->GetFiledID2());
      retmeexpr = irMap->HashMeExpr(&fieldsDistExpr);
      break;
    }
    case OP_iread: {
      IreadSSANode *ireadnode = static_cast<IreadSSANode *>(mirnode);
      IvarMeExpr ivarmeexpr(-1, ireadnode->primType, ireadnode->tyIdx, ireadnode->fieldID);
      ivarmeexpr.base = BuildExpr(ireadnode->Opnd(0), atParm, true);
      VersionSt *verSt = ireadnode->mayUse.opnd;
      if (verSt != nullptr) {
        VarMeExpr *varmeexpr = GetOrCreateVarFromVerSt(verSt);
        ivarmeexpr.mu = varmeexpr;
        if (verSt->ost->IsVolatile()) {
          ivarmeexpr.volatileFromBaseSymbol = true;
        }
      }
      IvarMeExpr *canIvar = static_cast<IvarMeExpr *>(irMap->HashMeExpr(&ivarmeexpr));
      CHECK_FATAL(canIvar->mu != nullptr, "BuildExpr: ivar node cannot have mu == nullptr");
      if (propagater && !noProp) {
        MeExpr *propedMeExpr = propagater->PropIvar(canIvar);
        MeExpr *simplifiedMeexpr = nullptr;
        if (propedMeExpr->meOp == kMeOpOp) {
           simplifiedMeexpr =propagater->SimplifyMeExpr(static_cast<OpMeExpr *>(propedMeExpr));
        }
        retmeexpr = simplifiedMeexpr ? simplifiedMeexpr : propedMeExpr;
      } else {
        retmeexpr = canIvar;
      }
      break;
    }
    case OP_constval: {
      ConstvalNode *constnode = static_cast<ConstvalNode *>(mirnode);
      retmeexpr = irMap->CreateConstMeExpr(constnode->primType, constnode->constVal);
      break;
    }
    case OP_conststr: {
      ConststrNode *constnode = static_cast<ConststrNode *>(mirnode);
      ConststrMeExpr conststrmeexpr(-1, constnode->strIdx, mirnode->primType);
      retmeexpr = irMap->HashMeExpr(&conststrmeexpr);
      break;
    }
    case OP_conststr16: {
      Conststr16Node *constnode = static_cast<Conststr16Node *>(mirnode);
      Conststr16MeExpr conststrmeexpr(-1, constnode->strIdx, mirnode->primType);
      retmeexpr = irMap->HashMeExpr(&conststrmeexpr);
      break;
    }
    case OP_eq:
    case OP_ne:
    case OP_lt:
    case OP_gt:
    case OP_le:
    case OP_ge:
    case OP_cmpg:
    case OP_cmpl:
    case OP_cmp: {
      OpMeExpr opmeexpr(-1, mirnode->op, mirnode->primType, mirnode->numOpnds);
      CompareNode *cmpnode = static_cast<CompareNode *>(mirnode);
      opmeexpr.SetOpnd(BuildExpr(cmpnode->Opnd(0), atParm, noProp), 0);
      opmeexpr.SetOpnd(BuildExpr(cmpnode->Opnd(1), atParm, noProp), 1);
      opmeexpr.opndType = cmpnode->opndType;
      retmeexpr = irMap->HashMeExpr(&opmeexpr);
      static_cast<OpMeExpr *>(retmeexpr)->opndType = cmpnode->opndType;
      break;
    }
    case OP_ceil:
    case OP_cvt:
    case OP_floor:
    case OP_trunc: {
      OpMeExpr opmeexpr(-1, mirnode->op, mirnode->primType, mirnode->numOpnds);
      TypeCvtNode *tycvtnode = static_cast<TypeCvtNode *>(mirnode);
      opmeexpr.SetOpnd(BuildExpr(tycvtnode->Opnd(0), atParm, noProp), 0);
      opmeexpr.opndType = tycvtnode->fromPrimType;
      retmeexpr = irMap->HashMeExpr(&opmeexpr);
      break;
    }
    case OP_retype: {
      OpMeExpr opmeexpr(-1, mirnode->op, mirnode->primType, mirnode->numOpnds);
      RetypeNode *tycvtnode = static_cast<RetypeNode *>(mirnode);
      opmeexpr.SetOpnd(BuildExpr(tycvtnode->Opnd(0), atParm, noProp), 0);
      opmeexpr.opndType = tycvtnode->fromPrimType;
      opmeexpr.tyIdx = tycvtnode->tyIdx;
      retmeexpr = irMap->HashMeExpr(&opmeexpr);
      break;
    }
    case OP_abs:
    case OP_bnot:
    case OP_lnot:
    case OP_neg:
    case OP_recip:
    case OP_sqrt:
    case OP_alloca:
    case OP_malloc: {
      OpMeExpr opmeexpr(-1, mirnode->op, mirnode->primType, mirnode->numOpnds);
      UnaryNode *unnode = static_cast<UnaryNode *>(mirnode);
      opmeexpr.SetOpnd(BuildExpr(unnode->Opnd(0), atParm, noProp), 0);
      retmeexpr = irMap->HashMeExpr(&opmeexpr);
      break;
    }
    case OP_iaddrof: {
      OpMeExpr opmeexpr(-1, mirnode->op, mirnode->primType, mirnode->numOpnds);
      IreadNode *unnode = static_cast<IreadNode *>(mirnode);
      opmeexpr.tyIdx = unnode->tyIdx;
      opmeexpr.fieldID = unnode->fieldID;
      opmeexpr.SetOpnd(BuildExpr(unnode->Opnd(0), atParm, noProp), 0);
      retmeexpr = irMap->HashMeExpr(&opmeexpr);
      break;
    }
    case OP_sext:
    case OP_zext:
    case OP_extractbits: {
      OpMeExpr opmeexpr(-1, mirnode->op, mirnode->primType, mirnode->numOpnds);
      ExtractbitsNode *extnode = static_cast<ExtractbitsNode *>(mirnode);
      opmeexpr.SetOpnd(BuildExpr(extnode->Opnd(0), atParm, noProp), 0);
      opmeexpr.bitsOffset = extnode->bitsOffset;
      opmeexpr.bitsSize = extnode->bitsSize;
      retmeexpr = irMap->HashMeExpr(&opmeexpr);
      break;
    }
    case OP_gcmallocjarray:
    case OP_gcpermallocjarray: {
      OpMeExpr opmeexpr(-1, mirnode->op, mirnode->primType, mirnode->numOpnds);
      JarrayMallocNode *gcnode = static_cast<JarrayMallocNode *>(mirnode);
      opmeexpr.SetOpnd(BuildExpr(gcnode->Opnd(0), atParm, noProp), 0);
      opmeexpr.tyIdx = gcnode->tyIdx;
      retmeexpr = irMap->HashMeExpr(&opmeexpr);
      break;
    }
    case OP_resolveinterfacefunc:
    case OP_resolvevirtualfunc: {
      OpMeExpr opmeexpr(-1, mirnode->op, mirnode->primType, mirnode->numOpnds);
      ResolveFuncNode *rsnode = static_cast<ResolveFuncNode *>(mirnode);
      opmeexpr.SetOpnd(BuildExpr(rsnode->Opnd(0), atParm, noProp), 0);
      opmeexpr.SetOpnd(BuildExpr(rsnode->Opnd(1), atParm, noProp), 1);
      opmeexpr.fieldID = rsnode->puIdx;
      retmeexpr = irMap->HashMeExpr(&opmeexpr);
      break;
    }
    case OP_sub:
    case OP_mul:
    case OP_div:
    case OP_rem:
    case OP_ashr:
    case OP_lshr:
    case OP_shl:
    case OP_max:
    case OP_min:
    case OP_band:
    case OP_bior:
    case OP_bxor:
    case OP_land:
    case OP_lior:
    case OP_cand:
    case OP_cior:
    case OP_CG_array_elem_add:
    case OP_add: {
      OpMeExpr opmeexpr(-1, mirnode->op, mirnode->primType, mirnode->numOpnds);
      BinaryNode *bnode = static_cast<BinaryNode *>(mirnode);
      opmeexpr.SetOpnd(BuildExpr(bnode->Opnd(0), atParm, noProp), 0);
      opmeexpr.SetOpnd(BuildExpr(bnode->Opnd(1), atParm, noProp), 1);
      retmeexpr = irMap->HashMeExpr(&opmeexpr);
      break;
    }
    case OP_select: {
      OpMeExpr opmeexpr(-1, mirnode->op, mirnode->primType, mirnode->numOpnds);
      TernaryNode *bnode = static_cast<TernaryNode *>(mirnode);
      opmeexpr.SetOpnd(BuildExpr(bnode->Opnd(0), atParm, noProp), 0);
      opmeexpr.SetOpnd(BuildExpr(bnode->Opnd(1), atParm, noProp), 1);
      opmeexpr.SetOpnd(BuildExpr(bnode->Opnd(2), atParm, noProp), 2);
      retmeexpr = irMap->HashMeExpr(&opmeexpr);
      break;
    }
    case OP_array: {
      ArrayNode *arrnode = static_cast<ArrayNode *>(mirnode);
      NaryMeExpr arrmeexpr(&irMap->tempAlloc, -1, mirnode->op, mirnode->primType, mirnode->numOpnds,
          arrnode->tyIdx, INTRN_UNDEFINED, arrnode->boundsCheck);
      for (int32 i = 0; i < arrnode->NumOpnds(); i++) {
        arrmeexpr.PushOpnd(BuildExpr(arrnode->Opnd(i), atParm, noProp));
      }
      retmeexpr = irMap->HashMeExpr(&arrmeexpr);
      break;
    }
    case OP_intrinsicop: {
      IntrinsicopNode *intrinnode = static_cast<IntrinsicopNode *>(mirnode);
      int32 numOpnds = intrinnode->NumOpnds();
      NaryMeExpr narymeexpr(&irMap->tempAlloc, -1, mirnode->op, mirnode->primType, mirnode->numOpnds,
          TyIdx(0), intrinnode->intrinsic, false);
      for (int32 i = 0; i < numOpnds; i++) {
        narymeexpr.PushOpnd(BuildExpr(intrinnode->Opnd(i), atParm, noProp));
      }
      retmeexpr = irMap->HashMeExpr(&narymeexpr);
      break;
    }
    case OP_intrinsicopwithtype: {
      IntrinsicopNode *intrinnode = static_cast<IntrinsicopNode *>(mirnode);
      int32 numOpnds = intrinnode->NumOpnds();
      NaryMeExpr narymeexpr(&irMap->irMapAlloc, -1, mirnode->op, mirnode->primType, mirnode->numOpnds,
          intrinnode->tyIdx, intrinnode->intrinsic, false);
      for (int32 i = 0; i < numOpnds; i++) {
        narymeexpr.PushOpnd(BuildExpr(intrinnode->Opnd(i), atParm, noProp));
      }
      retmeexpr = irMap->HashMeExpr(&narymeexpr);
      break;
    }
    default:
      CHECK_FATAL(false, "NIY BuildExpe");
  }
  return retmeexpr;
}

MeStmt *IrMapBuild::BuildMeStmt(StmtNode *stmt) {
  Opcode op = stmt->op;
  switch (op) {
    case OP_dassign: {
      DassignMeStmt *mestmt = irMap->NewInPool<DassignMeStmt>(stmt);
      DassignNode *dsssanode = static_cast<DassignNode *>(stmt);
      mestmt->rhs = BuildExpr(dsssanode->GetRhs(), false, false);
      MayDefPartWithVersionSt *thessapart =
        static_cast<MayDefPartWithVersionSt *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      ASSERT(thessapart != nullptr, "BuildMeStmt: ssapart of stmt cannot be nullptr");
      VarMeExpr *varlhs = static_cast<VarMeExpr *>(BuildLhsVar(thessapart->ssaVar, mestmt, dsssanode));
      mestmt->lhs = varlhs;
      mestmt->bb = curBB;
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      // determine isIncDecStmt
      if (mestmt->chiList.empty()) {
        MeExpr *rhs = mestmt->rhs;
        if (rhs->op == OP_add || rhs->op == OP_sub) {
          OpMeExpr *oprhs = static_cast<OpMeExpr *>(rhs);
          if (oprhs->GetOpnd(0)->meOp == kMeOpVar && oprhs->GetOpnd(1)->meOp == kMeOpConst) {
            mestmt->isIncDecStmt = varlhs->ost == static_cast<VarMeExpr *>(oprhs->GetOpnd(0))->ost;
          }
        }
      }
      if (propagater) {
        propagater->PropUpdateDef(mestmt->lhs);
        propagater->PropUpdateChiListDef(mestmt->chiList);
      }
      return mestmt;
    }
    case OP_regassign: {
      AssignMeStmt *mestmt = irMap->New<AssignMeStmt>(stmt);
      RegassignNode *regnode = static_cast<RegassignNode *>(stmt);
      mestmt->rhs = BuildExpr(regnode->Opnd(0), false, false);
      VersionSt *thessapart = static_cast<VersionSt *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      ASSERT(thessapart != nullptr, "BuildMeStmt: ssapart of stmt cannot be nullptr");
      RegMeExpr *reglhs = static_cast<RegMeExpr *>(BuildLhsReg(thessapart, mestmt, regnode));
      mestmt->lhs = reglhs;
      mestmt->bb = curBB;
      // determine isIncDecStmt
      MeExpr *rhs = mestmt->rhs;
      if (rhs->op == OP_add || rhs->op == OP_sub) {
        OpMeExpr *oprhs = static_cast<OpMeExpr *>(rhs);
        if (oprhs->GetOpnd(0)->meOp == kMeOpReg && oprhs->GetOpnd(1)->meOp == kMeOpConst) {
          mestmt->isIncDecStmt = reglhs->ost == static_cast<RegMeExpr *>(oprhs->GetOpnd(0))->ost;
        }
      }
      if (propagater) {
        propagater->PropUpdateDef(mestmt->lhs);
      }
      return mestmt;
    }
    case OP_maydassign: {
      MaydassignMeStmt *mestmt = irMap->NewInPool<MaydassignMeStmt>(stmt);
      DassignNode *dsssanode = static_cast<DassignNode *>(stmt);
      mestmt->rhs = BuildExpr(dsssanode->GetRhs(), false, false);
      MayDefPartWithVersionSt *thessapart =
        static_cast<MayDefPartWithVersionSt *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      mestmt->fieldID = dsssanode->fieldID;
      mestmt->bb = curBB;
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
      }
      return mestmt;
    }
    case OP_return: {
      NaryStmtNode *retstmt = static_cast<NaryStmtNode *>(stmt);
      RetMeStmt *mestmt = irMap->NewInPool<RetMeStmt>(stmt);
      bool isJava1OpndVar = (mirModule->IsJavaModule() && retstmt->NumOpnds() == 1 && retstmt->Opnd(0)->op == OP_dread);
      for (int32 i = 0; i < retstmt->NumOpnds(); i++) {
        mestmt->opnds.push_back(BuildExpr(retstmt->Opnd(i), false, isJava1OpndVar));
      }
      MayUsePart *thessapart = static_cast<MayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mestmt->muList);
      mestmt->bb = curBB;
      return mestmt;
    }
    case OP_call:
    case OP_virtualcall:
    case OP_virtualicall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_interfaceicall:
    case OP_customcall: {
      CallMeStmt *mestmt = irMap->NewInPool<CallMeStmt>(stmt);
      CallNode *intrinnode = static_cast<CallNode *>(stmt);
      mestmt->puIdx = intrinnode->puIdx;
      for (int32 i = 0; i < intrinnode->NumOpnds(); i++) {
        mestmt->opnds.push_back(BuildExpr(intrinnode->Opnd(i), true, false));
      }
      MayDefMayUsePart *thessapart = static_cast<MayDefMayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mestmt->muList);
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      mestmt->bb = curBB;
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
      }
      return mestmt;
    }
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
    case OP_customcallassigned: {
      CallMeStmt *mestmt = irMap->NewInPool<CallMeStmt>(stmt);
      CallNode *intrinnode = static_cast<CallNode *>(stmt);
      mestmt->puIdx = intrinnode->puIdx;
      for (int32 i = 0; i < intrinnode->NumOpnds(); i++) {
        mestmt->opnds.push_back(BuildExpr(intrinnode->Opnd(i), true, false));
      }
      MayDefMayUseMustDefPart *thessapart =
        static_cast<MayDefMayUseMustDefPart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mestmt->muList);
      BuildMustdefList(mestmt, thessapart->mustDefNodes, mestmt->mustDefList);
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      mestmt->bb = curBB;
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
        propagater->PropUpdateMustDefList(mestmt);
      }
      return mestmt;
    }
    case OP_polymorphiccall: {
      CallNode *cllstmt = static_cast<CallNode *>(stmt);
      CallMeStmt *mestmt = irMap->NewInPool<CallMeStmt>(stmt);
      for (int32 i = 0; i < cllstmt->NumOpnds(); i++) {
        mestmt->opnds.push_back(BuildExpr(cllstmt->Opnd(i), true, false));
      }
      MayDefMayUsePart *thessapart = static_cast<MayDefMayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mestmt->muList);
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      mestmt->bb = curBB;
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
      }
      return mestmt;
    }
    case OP_polymorphiccallassigned: {
      CallNode *cllstmt = static_cast<CallNode *>(stmt);
      CallMeStmt *mestmt = irMap->NewInPool<CallMeStmt>(stmt);
      for (int32 i = 0; i < cllstmt->NumOpnds(); i++) {
        mestmt->opnds.push_back(BuildExpr(cllstmt->Opnd(i), true, false));
      }
      MayDefMayUseMustDefPart *thessapart =
        static_cast<MayDefMayUseMustDefPart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mestmt->muList);
      BuildMustdefList(mestmt, thessapart->mustDefNodes, mestmt->mustDefList);
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      mestmt->bb = curBB;
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
        propagater->PropUpdateMustDefList(mestmt);
      }
      return mestmt;
    }
    case OP_icall: {
      IcallMeStmt *mestmt = irMap->NewInPool<IcallMeStmt>(stmt);
      IcallNode *intrinnode = static_cast<IcallNode *>(stmt);
      for (int32 i = 0; i < intrinnode->NumOpnds(); i++) {
        mestmt->opnds.push_back(BuildExpr(intrinnode->Opnd(i), true, false));
      }
      MayDefMayUsePart *thessapart = static_cast<MayDefMayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mestmt->muList);
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      mestmt->bb = curBB;
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
      }
      return mestmt;
    }
    case OP_icallassigned: {
      IcallMeStmt *mestmt = irMap->NewInPool<IcallMeStmt>(stmt);
      IcallNode *intrinnode = static_cast<IcallNode *>(stmt);
      for (int32 i = 0; i < intrinnode->NumOpnds(); i++) {
        mestmt->opnds.push_back(BuildExpr(intrinnode->Opnd(i), true, false));
      }
      MayDefMayUseMustDefPart *thessapart =
        static_cast<MayDefMayUseMustDefPart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mestmt->muList);
      BuildMustdefList(mestmt, thessapart->mustDefNodes, mestmt->mustDefList);
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      mestmt->bb = curBB;
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
        propagater->PropUpdateMustDefList(mestmt);
      }
      return mestmt;
    }
    case OP_intrinsiccall:
    case OP_xintrinsiccall: {
      IntrinsiccallNode *intrinnode = static_cast<IntrinsiccallNode *>(stmt);
      IntrinsiccallMeStmt *mestmt = irMap->NewInPool<IntrinsiccallMeStmt>(stmt);
      for (int32 i = 0; i < intrinnode->NumOpnds(); i++) {
        mestmt->opnds.push_back(BuildExpr(intrinnode->Opnd(i), true, false));
      }
      MayDefMayUsePart *thessapart = static_cast<MayDefMayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mestmt->muList);
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      mestmt->bb = curBB;
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
      }
      return mestmt;
    }
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned: {
      IntrinsiccallNode *intrinnode = static_cast<IntrinsiccallNode *>(stmt);
      IntrinsiccallMeStmt *mestmt = irMap->NewInPool<IntrinsiccallMeStmt>(stmt);
      for (int32 i = 0; i < intrinnode->NumOpnds(); i++) {
        mestmt->opnds.push_back(BuildExpr(intrinnode->Opnd(i), true, false));
      }
      MayDefMayUseMustDefPart *thessapart =
        static_cast<MayDefMayUseMustDefPart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mestmt->muList);
      BuildMustdefList(mestmt, thessapart->mustDefNodes, mestmt->mustDefList);
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      mestmt->bb = curBB;
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
        propagater->PropUpdateMustDefList(mestmt);
      }
      return mestmt;
    }
    case OP_intrinsiccallwithtype: {
      IntrinsiccallNode *intrinnode = static_cast<IntrinsiccallNode *>(stmt);
      IntrinsiccallMeStmt *mestmt = irMap->NewInPool<IntrinsiccallMeStmt>(stmt);
      for (int32 i = 0; i < intrinnode->NumOpnds(); i++) {
        mestmt->opnds.push_back(BuildExpr(intrinnode->Opnd(i), true, false));
      }
      MayDefMayUsePart *thessapart = static_cast<MayDefMayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mestmt->muList);
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      mestmt->bb = curBB;
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
      }
      return mestmt;
    }
    case OP_intrinsiccallwithtypeassigned: {
      IntrinsiccallNode *intrinnode = static_cast<IntrinsiccallNode *>(stmt);
      IntrinsiccallMeStmt *mestmt = irMap->NewInPool<IntrinsiccallMeStmt>(stmt);
      for (int32 i = 0; i < intrinnode->NumOpnds(); i++) {
        mestmt->opnds.push_back(BuildExpr(intrinnode->Opnd(i), true, false));
      }
      MayDefMayUseMustDefPart *thessapart =
        static_cast<MayDefMayUseMustDefPart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mestmt->muList);
      BuildMustdefList(mestmt, thessapart->mustDefNodes, mestmt->mustDefList);
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      mestmt->bb = curBB;
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
        propagater->PropUpdateMustDefList(mestmt);
      }
      return mestmt;
    }
    case OP_jscatch:
    case OP_finally:
    case OP_endtry:
    case OP_cleanuptry:
    case OP_membaracquire:
    case OP_membarrelease:
    case OP_membarstorestore:
    case OP_membarstoreload: {
      MeStmt *mestmt = irMap->New<MeStmt>(stmt);
      mestmt->bb = curBB;
      return mestmt;
    }
    case OP_retsub: {
      WithMuMeStmt *mumestmt = irMap->NewInPool<WithMuMeStmt>(stmt);
      MayUsePart *thessapart = static_cast<MayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mumestmt->muList);
      mumestmt->bb = curBB;
      return mumestmt;
    }
    case OP_gosub: {
      GosubMeStmt *mumestmt = irMap->NewInPool<GosubMeStmt>(stmt);
      MayUsePart *thessapart = static_cast<MayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, mumestmt->muList);
      mumestmt->bb = curBB;
      return mumestmt;
    }
    case OP_iassign: {
      IassignNode *iasnode = static_cast<IassignNode *>(stmt);
      IassignMeStmt *mestmt = irMap->NewInPool<IassignMeStmt>(stmt);
      mestmt->tyIdx = iasnode->tyIdx;
      mestmt->rhs = BuildExpr(iasnode->rhs, false, false);
      mestmt->lhsVar = irMap->BuildLhsIvar(BuildExpr(iasnode->addrExpr, false, true), mestmt, iasnode->fieldID);
      MayDefPart *thessapart = static_cast<MayDefPart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      if (mirModule->IsCModule()) {
        bool isVolt = false;
        for (std::pair<OStIdx, MayDefNode> maydefit : thessapart->mayDefNodes) {
          OriginalSt *ost = ssaTab->GetOriginalStFromid(maydefit.first);
          if (ost->IsVolatile()) {
            isVolt = true;
            break;
          }
        }
        if (isVolt) {
          mestmt->lhsVar->volatileFromBaseSymbol = true;
        }
      }
      BuildChiList(mestmt, thessapart->mayDefNodes, mestmt->chiList);
      mestmt->bb = curBB;
      if (propagater) {
        propagater->PropUpdateChiListDef(mestmt->chiList);
      }
      return mestmt;
    }
    case OP_goto: {
      GotoMeStmt *mestmt = irMap->New<GotoMeStmt>(stmt);
      mestmt->bb = curBB;
      return mestmt;
    }
    case OP_brfalse:
    case OP_brtrue: {
      CondGotoNode *condgoto = static_cast<CondGotoNode *>(stmt);
      CondGotoMeStmt *mestmt = irMap->New<CondGotoMeStmt>(stmt);
      mestmt->opnd = BuildExpr(condgoto->Opnd(0), false, false);
      mestmt->bb = curBB;
      return mestmt;
    }
    case OP_comment: {
      CommentMeStmt *mestmt = irMap->NewInPool<CommentMeStmt>(stmt);
      mestmt->bb = curBB;
      return mestmt;
    }
    case OP_jstry: {
      JsTryMeStmt *mestmt = irMap->New<JsTryMeStmt>(stmt);
      mestmt->bb = curBB;
      return mestmt;
    }
    case OP_cpptry:
    case OP_try:
    case OP_javatry: {
      TryNode *jtstmt = static_cast<TryNode *>(stmt);
      TryMeStmt *jtmestmt = irMap->NewInPool<TryMeStmt>(stmt);
      for (uint32 i = 0; i < jtstmt->offsets.size(); i++) {
        jtmestmt->offsets.push_back(jtstmt->offsets[i]);
      }
      jtmestmt->bb = curBB;
      return jtmestmt;
    }
    case OP_catch:
    case OP_javacatch: {
      JavaCatchMeStmt *mestmt = irMap->NewInPool<JavaCatchMeStmt>(stmt);
      mestmt->bb = curBB;
      return mestmt;
    }
    case OP_cppcatch: {
      CppCatchMeStmt *mestmt = irMap->New<CppCatchMeStmt>(stmt);
      mestmt->bb = curBB;
      return mestmt;
    }
    case OP_throw: {
      UnaryStmtNode *tstmt = static_cast<UnaryStmtNode *>(stmt);
      ThrowMeStmt *tmestmt = irMap->NewInPool<ThrowMeStmt>(stmt);
      tmestmt->opnd = BuildExpr(tstmt->Opnd(0), false, false);
      tmestmt->bb = curBB;
      MayUsePart *thessapart = static_cast<MayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, tmestmt->muList);
      return tmestmt;
    }
    case OP_syncenter:
    case OP_syncexit: {
      NaryStmtNode *tstmt = static_cast<NaryStmtNode *>(stmt);
      SyncMeStmt *tmestmt = irMap->NewInPool<SyncMeStmt>(stmt);
      for (int32 i = 0; i < tstmt->NumOpnds(); i++) {
        tmestmt->opnds.push_back(BuildExpr(tstmt->Opnd(i), false, false));
      }
      tmestmt->bb = curBB;
      MayDefMayUsePart *thessapart = static_cast<MayDefMayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      BuildMuList(thessapart->mayUseNodes, tmestmt->muList);
      BuildChiList(tmestmt, thessapart->mayDefNodes, tmestmt->chiList);
      return tmestmt;
    }
    case OP_assertnonnull:
    case OP_eval:
    case OP_free: {
      UnaryStmtNode *unarystmt = static_cast<UnaryStmtNode *>(stmt);
      UnaryMeStmt *unmestmt = irMap->New<UnaryMeStmt>(stmt);
      unmestmt->opnd = BuildExpr(unarystmt->Opnd(0), false, false);
      unmestmt->bb = curBB;
      return unmestmt;
    }
    case OP_switch: {
      SwitchNode *switchnode = static_cast<SwitchNode *>(stmt);
      SwitchMeStmt *mestmt = irMap->NewInPool<SwitchMeStmt>(stmt);
      mestmt->opnd = BuildExpr(switchnode->switchOpnd, false, false);
      mestmt->bb = curBB;
      return mestmt;
    }
    default: {
      CHECK_FATAL(false, "NYI");
      return nullptr;
    }
  }
}

// recursively invoke itself in a pre-order traversal of the dominator tree of
// the CFG to build the HSSA representation for the code in each BB
void IrMapBuild::BuildBB(BB *bb, std::vector<bool> &bbIrmapProcessed) {
  if (bb == nullptr) {
    return;
  }
  BBId bbid = bb->id;
  if (bbIrmapProcessed[bbid.idx]) {
    return;
  }
  bbIrmapProcessed[bbid.idx] = true;
  curBB = bb;
  irMap->SetCurFunction(bb);

  // iterate phi list to update the definition by phi
  BuildPhiMeNode(bb);
  std::vector<uint32> curStackSizeVec;
  if (propagater) { // proper is not null that means we will do propragation during build meir.
    propagater->SetCurBB(bb);
    uint32 vstVecsize = propagater->GetVstLiveStackVecSize();
    curStackSizeVec.resize(vstVecsize);
    for (uint32 i = 1; i < vstVecsize; i++) {
     curStackSizeVec[i] = propagater->GetVstLiveStackVec(i)->size();
    }
    // traversal phi nodes
    MapleMap<OStIdx, MePhiNode *> &mePhiList = bb->mePhiList;
    for (MapleMap<OStIdx, MePhiNode *>::iterator it = mePhiList.begin(); it != mePhiList.end(); it++) {
      MePhiNode *phimenode = it->second;
      propagater->PropUpdateDef(phimenode->lhs);
    }
  }

  if (!bb->IsEmpty()) {
    for (auto stmt : bb->stmtNodeList) {
      MeStmt *mestmt = BuildMeStmt(stmt);
      bb->AddMeStmtLast(mestmt);
    }
  }
  // travesal bb's dominated tree
  CHECK(bbid.idx < dominance->domChildren.size(), " index out of range in IrMapBuild::BuildBB");
  MapleSet<BBId> *domChildren = &dominance->domChildren[bbid.idx];
  for (MapleSet<BBId>::iterator bbit = domChildren->begin(); bbit != domChildren->end(); bbit++) {
    BBId childbbid = *bbit;
    BuildBB(irMap->GetBB(childbbid), bbIrmapProcessed);
  }
  if (propagater) {
    for (uint32 i = 1; i < propagater->GetVstLiveStackVecSize(); i++) {
      MapleStack<MeExpr *> *liveStack = propagater->GetVstLiveStackVec(i);
      uint32 curSize = curStackSizeVec[i];
      while (liveStack->size() > curSize) {
        liveStack->pop();
      }
    }
  }
}

}  // namespace maple
