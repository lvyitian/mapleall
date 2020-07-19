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

// This file contains methods to emit Maple IR nodes from MeExpr/MeStmt

#include "me_ir.h"
#include "irmap.h"
#include "mir_builder.h"
#include "orig_symbol.h"

namespace maple {

static bool IsValidVeridx(SSATab *ssaTab, VarMeExpr *varx) {
  OriginalSt *ost = varx->ost;
  if (ost == nullptr || !ost->IsSymbol()) {
    return false;
  }
  StIdx stIdx = ost->GetMIRSymbol()->stIdx;
  return stIdx.Islocal() ? ssaTab->mirModule.CurFunction()->symTab->IsValidIdx(stIdx.Idx())
                         : GlobalTables::GetGsymTable().IsValidIdx(stIdx.Idx());
}

BaseNode *VarMeExpr::EmitExpr(SSATab *ssaTab) {
  MIRSymbol *sym = ost->GetMIRSymbol();
  if (sym->IsLocal()) {
    sym->ResetIsDeleted();
  }
  AddrofNode *nd = ssaTab->mirModule.CurFunction()->codeMemPool->New<AddrofNode>(OP_dread, primType, sym->stIdx, ost->fieldID);
  CHECK_FATAL(nd->primType != kPtyInvalid, "");
  CHECK_FATAL(IsValidVeridx(ssaTab, this), "");
  return nd;
}

BaseNode *RegMeExpr::EmitExpr(SSATab *ssaTab) {
  RegreadNode *regread = ssaTab->mirModule.CurFunction()->codeMemPool->New<RegreadNode>();
  regread->primType = primType;
  regread->regIdx = regIdx;
  ASSERT(regIdx < 0 || static_cast<uint32>(regIdx) < ssaTab->mirModule.CurFunction()->pregTab->Size(),
          "RegMeExpr::EmitExpr: pregidx exceeds preg table size");
  return regread;
}

BaseNode *ConstMeExpr::EmitExpr(SSATab *ssaTab) {
  ConstvalNode *exprconst = ssaTab->mirModule.CurFunction()->codeMemPool->New<ConstvalNode>(primType, constVal);
  // if int const has been promoted from dyn int const, remove the type tag
  if (IsPrimitiveInteger(exprconst->primType)) {
    MIRIntConst *intconst = static_cast<MIRIntConst *>(exprconst->constVal);
    intconst->value = intconst->GetValueUnderType();
  }
  return exprconst;
}

BaseNode *ConststrMeExpr::EmitExpr(SSATab *ssaTab) {
  ConststrNode *exprconst = ssaTab->mirModule.CurFunction()->codeMemPool->New<ConststrNode>(primType, strIdx);
  return exprconst;
}

BaseNode *Conststr16MeExpr::EmitExpr(SSATab *ssaTab) {
  Conststr16Node *exprconst = ssaTab->mirModule.CurFunction()->codeMemPool->New<Conststr16Node>(primType, strIdx);
  return exprconst;
}

BaseNode *SizeoftypeMeExpr::EmitExpr(SSATab *ssaTab) {
  SizeoftypeNode *exprsizeoftype = ssaTab->mirModule.CurFunction()->codeMemPool->New<SizeoftypeNode>(primType, tyIdx);
  return exprsizeoftype;
}

BaseNode *FieldsDistMeExpr::EmitExpr(SSATab *ssaTab) {
  FieldsDistNode *exprSizeoftype = ssaTab->mirModule.CurFunction()->codeMemPool->New<FieldsDistNode>(primType, tyIdx, fieldID1, fieldID2);
  return exprSizeoftype;
}

BaseNode *AddrofMeExpr::EmitExpr(SSATab *ssaTab) {
  MIRSymbol *sym = ssaTab->GetMIRSymbolFromid(ostIdx);
  if (sym->IsLocal()) {
    sym->ResetIsDeleted();
  }
  AddrofNode *addrofnode =
    ssaTab->mirModule.CurFunction()->codeMemPool->New<AddrofNode>(OP_addrof, primType, sym->stIdx, fieldID);
  return addrofnode;
}

BaseNode *AddroffuncMeExpr::EmitExpr(SSATab *ssaTab) {
  AddroffuncNode *offuncnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<AddroffuncNode>(primType, puIdx);
  return offuncnode;
}

BaseNode *AddroflabelMeExpr::EmitExpr(SSATab *ssaTab) {
  AddroflabelNode *aolNode = ssaTab->mirModule.CurFunction()->codeMemPool->New<AddroflabelNode>();
  aolNode->primType = PTY_ptr;
  aolNode->offset = labelIdx;
  return aolNode;
}

BaseNode *GcmallocMeExpr::EmitExpr(SSATab *ssaTab) {
  GCMallocNode *gcmnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<GCMallocNode>(op, primType, tyIdx);
  return gcmnode;
}

BaseNode *OpMeExpr::EmitExpr(SSATab *ssaTab) {
  switch (op) {
    case OP_add:
    case OP_ashr:
    case OP_band:
    case OP_bior:
    case OP_bxor:
    case OP_div:
    case OP_land:
    case OP_lior:
    case OP_cand:
    case OP_cior:
    case OP_lshr:
    case OP_max:
    case OP_min:
    case OP_mul:
    case OP_rem:
    case OP_shl:
    case OP_sub: {
      BinaryNode *bnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<BinaryNode>(op, primType);
      BaseNode *opnd0 = opnds[0]->EmitExpr(ssaTab);
      if (opnd0->primType == PTY_agg) {
        opnd0->primType = primType;
      }
      bnode->bOpnd[0] = opnd0;
      BaseNode *opnd1 = opnds[1]->EmitExpr(ssaTab);
      if (opnd1->primType == PTY_agg) {
        opnd1->primType = primType;
      }
      bnode->bOpnd[1] = opnd1;
      return bnode;
    }
    case OP_eq:
    case OP_ne:
    case OP_lt:
    case OP_gt:
    case OP_le:
    case OP_ge:
    case OP_cmpl:
    case OP_cmpg:
    case OP_cmp: {
      CompareNode *cmpnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<CompareNode>(op, primType);
      BaseNode *opnd0 = opnds[0]->EmitExpr(ssaTab);
      if (opnd0->primType == PTY_agg) {
        opnd0->primType = opndType;
      }
      cmpnode->bOpnd[0] = opnd0;
      BaseNode *opnd1 = opnds[1]->EmitExpr(ssaTab);
      if (opnd1->primType == PTY_agg) {
        opnd1->primType = opndType;
      }
      cmpnode->bOpnd[1] = opnd1;
      cmpnode->opndType = opndType;
      return cmpnode;
    }
    case OP_abs:
    case OP_bnot:
    case OP_lnot:
    case OP_neg:
    case OP_recip:
    case OP_sqrt:
    case OP_alloca:
    case OP_malloc: {
      UnaryNode *unode = ssaTab->mirModule.CurFunction()->codeMemPool->New<UnaryNode>(op, primType);
      unode->uOpnd = opnds[0]->EmitExpr(ssaTab);
      return unode;
    }
    case OP_sext:
    case OP_zext:
    case OP_extractbits: {
      ExtractbitsNode *unode = ssaTab->mirModule.CurFunction()->codeMemPool->New<ExtractbitsNode>(op, primType);
      unode->uOpnd = opnds[0]->EmitExpr(ssaTab);
      unode->bitsOffset = bitsOffset;
      unode->bitsSize = bitsSize;
      return unode;
    }
    case OP_select: {
      TernaryNode *tnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<TernaryNode>(op, primType);
      tnode->topnd[0] = opnds[0]->EmitExpr(ssaTab);
      tnode->topnd[1] = opnds[1]->EmitExpr(ssaTab);
      tnode->topnd[2] = opnds[2]->EmitExpr(ssaTab);
      return tnode;
    }
    case OP_ceil:
    case OP_cvt:
    case OP_floor:
    case OP_trunc: {
      TypeCvtNode *cvtnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<TypeCvtNode>(op, primType);
      cvtnode->uOpnd = opnds[0]->EmitExpr(ssaTab);
      cvtnode->fromPrimType = opndType;
      return cvtnode;
    }
    case OP_retype: {
      RetypeNode *cvtnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<RetypeNode>(primType);
      cvtnode->uOpnd = opnds[0]->EmitExpr(ssaTab);
      cvtnode->fromPrimType = opndType;
      cvtnode->tyIdx = tyIdx;
      return cvtnode;
    }
    case OP_gcmallocjarray:
    case OP_gcpermallocjarray: {
      JarrayMallocNode *arrymalloc =
        ssaTab->mirModule.CurFunction()->codeMemPool->New<JarrayMallocNode>(op, primType);
      arrymalloc->uOpnd = opnds[0]->EmitExpr(ssaTab);
      arrymalloc->tyIdx = tyIdx;
      return arrymalloc;
    }
    case OP_resolveinterfacefunc:
    case OP_resolvevirtualfunc: {
      ResolveFuncNode *rsnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<ResolveFuncNode>(op, primType);
      rsnode->bOpnd[0] = opnds[0]->EmitExpr(ssaTab);
      rsnode->bOpnd[1] = opnds[1]->EmitExpr(ssaTab);
      rsnode->puIdx = fieldID;
      return rsnode;
    }
    case OP_iaddrof: {
      IaddrofNode *iaddrof = ssaTab->mirModule.CurFunction()->codeMemPool->New<IaddrofNode>(OP_iaddrof, primType);
      iaddrof->uOpnd = opnds[0]->EmitExpr(ssaTab);
      iaddrof->tyIdx = tyIdx;
      iaddrof->fieldID = fieldID;
      return iaddrof;
    }
    default:
      CHECK_FATAL(false, "unexpected op");
  }
}

BaseNode *NaryMeExpr::EmitExpr(SSATab *ssaTab) {
  BaseNode *nodeToReturn = nullptr;
  NaryOpnds *nopndpart = nullptr;
  if (op == OP_array) {
    ArrayNode *arrnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<ArrayNode>(&(ssaTab->mirModule), primType, tyIdx);
    arrnode->numOpnds = numOpnds;
    arrnode->boundsCheck = boundCheck;
    nopndpart = arrnode;
    nodeToReturn = arrnode;
  } else {
    IntrinsicopNode *intrinnode;
    if (op == OP_intrinsicopwithtype) {
      IntrinsicopNode *intrinwith =
        ssaTab->mirModule.CurFunction()->codeMemPool->New<IntrinsicopNode>(&(ssaTab->mirModule), OP_intrinsicopwithtype, primType);
      intrinwith->tyIdx = tyIdx;
      intrinnode = intrinwith;
    } else
      intrinnode =
        ssaTab->mirModule.CurFunction()->codeMemPool->New<IntrinsicopNode>(&(ssaTab->mirModule), OP_intrinsicop, primType);
    intrinnode->numOpnds = numOpnds;
    intrinnode->intrinsic = intrinsic;
    nopndpart = intrinnode;
    nodeToReturn = intrinnode;
  }
  for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
    nopndpart->nOpnd.push_back((*it)->EmitExpr(ssaTab));
  }
  return nodeToReturn;
}

BaseNode *IvarMeExpr::EmitExpr(SSATab *ssaTab) {
  IreadNode *ireadnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<IreadNode>(OP_iread, primType);
  ireadnode->uOpnd = base->EmitExpr(ssaTab);
  ireadnode->fieldID = fieldID;
  ireadnode->tyIdx = tyIdx;

  CHECK_FATAL(ireadnode->primType != kPtyInvalid, "");
  CHECK_FATAL(tyIdx != TyIdx(0), "wrong tyIdx for iread node in me emit");
  ireadnode->tyIdx = tyIdx;
  return ireadnode;
}

StmtNode *MeStmt::EmitStmt(SSATab *ssaTab) {
  StmtNode *stmt = ssaTab->mirModule.CurFunction()->codeMemPool->New<StmtNode>(op);
  stmt->srcPosition = srcPos;
  return stmt;
}

StmtNode *AssignMeStmt::EmitStmt(SSATab *ssaTab) {
  if (rhs->meOp == lhs->meOp && lhs->ost == static_cast<ScalarMeExpr *>(rhs)->ost) {
    // identify assignment converted from phi; omit it
    return nullptr;
  }
  if (lhs->meOp == kMeOpReg) {
    RegassignNode *regassignstmt =
    ssaTab->mirModule.mirBuilder->CreateStmtRegassign(lhs->primType, GetRegLhs()->regIdx, rhs->EmitExpr(ssaTab));
    regassignstmt->srcPosition = srcPos;
    return regassignstmt;
  } else {
    DassignNode *dsstmt = ssaTab->mirModule.CurFunction()->codeMemPool->New<DassignNode>();
    MIRSymbol *sym = lhs->ost->GetMIRSymbol();
    if (sym->IsLocal()) {
      sym->ResetIsDeleted();
    }
    dsstmt->stIdx = sym->stIdx;
    dsstmt->fieldID = GetVarLhs()->ost->fieldID;
    dsstmt->SetRhs(rhs->EmitExpr(ssaTab));
    dsstmt->srcPosition = srcPos;
    return dsstmt;
  }
}

StmtNode *DassignMeStmt::EmitStmt(SSATab *ssaTab) {
  if (rhs->meOp == lhs->meOp && lhs->ost == static_cast<ScalarMeExpr *>(rhs)->ost) {
    // identify assignment converted from phi; omit it
    return nullptr;
  }
  DassignNode *dsstmt = ssaTab->mirModule.CurFunction()->codeMemPool->New<DassignNode>();
  MIRSymbol *sym = lhs->ost->GetMIRSymbol();
  if (sym->IsLocal()) {
    sym->ResetIsDeleted();
  }
  dsstmt->stIdx = sym->stIdx;
  dsstmt->fieldID = GetVarLhs()->ost->fieldID;
  dsstmt->SetRhs(rhs->EmitExpr(ssaTab));
  dsstmt->srcPosition = srcPos;
  return dsstmt;
}

StmtNode *MaydassignMeStmt::EmitStmt(SSATab *ssaTab) {
  DassignNode *dsstmt = ssaTab->mirModule.CurFunction()->codeMemPool->New<DassignNode>();
  MIRSymbol *sym = GetLhsSym(ssaTab);
  if (sym->IsLocal()) {
    sym->ResetIsDeleted();
  }
  dsstmt->stIdx = sym->GetStIdx();
  dsstmt->fieldID = fieldID;
  dsstmt->SetRhs(rhs->EmitExpr(ssaTab));
  dsstmt->srcPosition = srcPos;
  return dsstmt;
}

StmtNode *IassignMeStmt::EmitStmt(SSATab *ssaTab) {
  IassignNode *iassnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<IassignNode>();
  iassnode->tyIdx = tyIdx;
  iassnode->fieldID = lhsVar->fieldID;
  iassnode->addrExpr = lhsVar->base->EmitExpr(ssaTab);
  iassnode->rhs = rhs->EmitExpr(ssaTab);
  iassnode->srcPosition = srcPos;
  return iassnode;
}

void MeStmt::EmitCallReturnVector(SSATab *ssaTab, CallReturnVector *returnValues) {
  MapleVector<MustDefMeNode> *mustdefs = GetMustDefList();
  if (mustdefs == nullptr || mustdefs->empty()) {
    return;
  }
  MeExpr *meexpr = mustdefs->front().lhs;
  if (meexpr->meOp == kMeOpVar) {
    OriginalSt *ost = static_cast<VarMeExpr*>(meexpr)->ost;
    MIRSymbol *sym = ost->GetMIRSymbol();
    returnValues->push_back(CallReturnPair(sym->GetStIdx(), RegFieldPair(0, 0)));
  } else if (meexpr->meOp == kMeOpReg) {
    returnValues->push_back(CallReturnPair(StIdx(), RegFieldPair(0, static_cast<RegMeExpr *>(meexpr)->regIdx)));
  }
}

StmtNode *CallMeStmt::EmitStmt(SSATab *ssaTab) {
  if (op != OP_icall && op != OP_icallassigned) {
    CallNode *callnode =
      ssaTab->mirModule.CurFunction()->codeMemPool->New<CallNode>(&(ssaTab->mirModule), op);
    callnode->puIdx = puIdx;
    callnode->tyIdx = tyIdx;
    callnode->nOpnd.resize(opnds.size());
    for (uint32 i = 0; i < opnds.size(); i++) {
      callnode->SetOpnd(opnds[i]->EmitExpr(ssaTab), i);
    }
    callnode->numOpnds = callnode->nOpnd.size();
    callnode->srcPosition = srcPos;
    EmitCallReturnVector(ssaTab, &callnode->returnValues);
    for (uint32 j = 0; j < callnode->returnValues.size(); j++) {
      CallReturnPair retpair = callnode->returnValues[j];
      if (!retpair.second.IsReg()) {
        StIdx stIdx = retpair.first;
        if (stIdx.Islocal()) {
          MIRSymbolTable *symtab = ssaTab->mirModule.CurFunction()->symTab;
          MIRSymbol *sym = symtab->GetSymbolFromStIdx(stIdx.Idx());
          sym->ResetIsDeleted();
        }
      }
    }
    return callnode;
  } else {
    IcallNode *icallnode =
      ssaTab->mirModule.CurFunction()->codeMemPool->New<IcallNode>(&(ssaTab->mirModule), OP_icallassigned);
    icallnode->nOpnd.resize(opnds.size());
    for (uint32 i = 0; i < opnds.size(); i++) {
      icallnode->SetOpnd(opnds[i]->EmitExpr(ssaTab), i);
    }
    icallnode->numOpnds = icallnode->nOpnd.size();
    icallnode->srcPosition = srcPos;
    EmitCallReturnVector(ssaTab, &icallnode->returnValues);
    icallnode->retTyIdx = TyIdx(PTY_void);
    for (uint32 j = 0; j < icallnode->returnValues.size(); j++) {
      CallReturnPair retpair = icallnode->returnValues[j];
      if (!retpair.second.IsReg()) {
        StIdx stIdx = retpair.first;
        MIRSymbolTable *symtab = ssaTab->mirModule.CurFunction()->symTab;
        MIRSymbol *sym = symtab->GetSymbolFromStIdx(stIdx.Idx());
        icallnode->retTyIdx = sym->GetType()->GetTypeIndex();
        if (stIdx.Islocal()) {
          sym->ResetIsDeleted();
        }
      } else {
        PregIdx pregidx = (PregIdx)retpair.second.pregIdx;
        MIRPreg *preg = ssaTab->mirModule.CurFunction()->pregTab->PregFromPregIdx(pregidx);
        icallnode->retTyIdx = TyIdx(preg->primType);
      }
    }
    return icallnode;
  }
}

StmtNode* IcallMeStmt::EmitStmt(SSATab *ssaTab) {
  IcallNode *icallnode =
      ssaTab->mirModule.CurFunction()->codeMemPool->New<IcallNode>(&(ssaTab->mirModule), OP_icallassigned);
  icallnode->nOpnd.resize(opnds.size());
  for (uint32 i = 0; i < opnds.size(); i++)
    icallnode->SetOpnd(opnds[i]->EmitExpr(ssaTab), i);
  icallnode->numOpnds = icallnode->nOpnd.size();
  icallnode->srcPosition = srcPos;
  EmitCallReturnVector(ssaTab, &icallnode->returnValues);
  icallnode->retTyIdx = TyIdx(PTY_void);
  for (uint32 j = 0; j < icallnode->returnValues.size(); j++) {
    CallReturnPair retpair = icallnode->returnValues[j];
    if (!retpair.second.IsReg()) {
      StIdx stIdx = retpair.first;
      MIRSymbolTable *symtab = ssaTab->mirModule.CurFunction()->symTab;
      MIRSymbol *sym = symtab->GetSymbolFromStIdx(stIdx.Idx());
      icallnode->retTyIdx = sym->GetType()->GetTypeIndex();
      if (stIdx.Islocal()) {
        sym->ResetIsDeleted();
      }
    } else {
      PregIdx pregidx = (PregIdx)retpair.second.pregIdx;
      MIRPreg *preg = ssaTab->mirModule.CurFunction()->pregTab->PregFromPregIdx(pregidx);
      icallnode->retTyIdx = TyIdx(preg->primType);
    }
  }
  return icallnode;
}

StmtNode *IntrinsiccallMeStmt::EmitStmt(SSATab *ssaTab) {
  IntrinsiccallNode *callnode =
    ssaTab->mirModule.CurFunction()->codeMemPool->New<IntrinsiccallNode>(&(ssaTab->mirModule), op);
  callnode->intrinsic = intrinsic;
  callnode->tyIdx = tyIdx;
  callnode->nOpnd.resize(opnds.size());
  for (uint32 i = 0; i < opnds.size(); i++) {
    callnode->SetOpnd(opnds[i]->EmitExpr(ssaTab), i);
  }
  callnode->numOpnds = callnode->nOpnd.size();
  callnode->srcPosition = srcPos;
  if (kOpcodeInfo.IsCallAssigned(op)) {
    EmitCallReturnVector(ssaTab, &callnode->returnValues);
    for (uint32 j = 0; j < callnode->returnValues.size(); j++) {
      CallReturnPair retpair = callnode->returnValues[j];
      if (!retpair.second.IsReg()) {
        StIdx stIdx = retpair.first;
        if (stIdx.Islocal()) {
          MIRSymbolTable *symtab = ssaTab->mirModule.CurFunction()->symTab;
          MIRSymbol *sym = symtab->GetSymbolFromStIdx(stIdx.Idx());
          sym->ResetIsDeleted();
        }
      }
    }
  }
  return callnode;
}

StmtNode *NaryMeStmt::EmitStmt(SSATab *ssaTab) {
  NaryStmtNode *ret = ssaTab->mirModule.CurFunction()->codeMemPool->New<NaryStmtNode>(&(ssaTab->mirModule), op);
  ret->nOpnd.resize(opnds.size());
  for (uint32 i = 0; i < opnds.size(); i++) {
    ret->SetOpnd(opnds[i]->EmitExpr(ssaTab), i);
  }
  ret->numOpnds = ret->nOpnd.size();
  ret->srcPosition = srcPos;
  return ret;
}

StmtNode *UnaryMeStmt::EmitStmt(SSATab *ssaTab) {
  UnaryStmtNode *ustmt = ssaTab->mirModule.CurFunction()->codeMemPool->New<UnaryStmtNode>(op);
  ustmt->SetOpnd(opnd->EmitExpr(ssaTab), 0);
  ustmt->srcPosition = srcPos;
  if (not_need_lock) {
    ssaTab->mirModule.rcNotNeedingLock.insert(ustmt->stmtID);
  }
  return ustmt;
}

StmtNode *GotoMeStmt::EmitStmt(SSATab *ssaTab) {
  GotoNode *gto = ssaTab->mirModule.CurFunction()->codeMemPool->New<GotoNode>(OP_goto);
  gto->offset = offset;
  gto->srcPosition = srcPos;
  return gto;
}

StmtNode *CondGotoMeStmt::EmitStmt(SSATab *ssaTab) {
  CondGotoNode *cgstmt = ssaTab->mirModule.CurFunction()->codeMemPool->New<CondGotoNode>(op);
  cgstmt->offset = offset;
  cgstmt->uOpnd = opnd->EmitExpr(ssaTab);
  cgstmt->srcPosition = srcPos;
  return cgstmt;
}

StmtNode *JsTryMeStmt::EmitStmt(SSATab *ssaTab) {
  JsTryNode *trynd = ssaTab->mirModule.CurFunction()->codeMemPool->New<JsTryNode>();
  trynd->catchOffset = catchOffset;
  trynd->finallyOffset = finallyOffset;
  trynd->srcPosition = srcPos;
  return trynd;
}

StmtNode *TryMeStmt::EmitStmt(SSATab *ssaTab) {
  TryNode *jtstmt = ssaTab->mirModule.CurFunction()->codeMemPool->New<TryNode>(&ssaTab->mirModule, op);
  jtstmt->offsets.resize(offsets.size());
  for (uint32 i = 0; i < offsets.size(); i++) {
    jtstmt->offsets[i] = offsets[i];
  }
  jtstmt->srcPosition = srcPos;
  return jtstmt;
}

StmtNode *CppCatchMeStmt::EmitStmt(SSATab *ssaTab) {
  CppCatchNode *catchnd = ssaTab->mirModule.CurFunction()->codeMemPool->New<CppCatchNode>();
  catchnd->exceptionTyIdx = exceptionTyIdx;
  catchnd->srcPosition = srcPos;
  return catchnd;
}

StmtNode *JavaCatchMeStmt::EmitStmt(SSATab *ssaTab) {
  CatchNode *javacatchnd = ssaTab->mirModule.CurFunction()->codeMemPool->New<CatchNode>(&(ssaTab->mirModule));
  javacatchnd->exceptionTyIdxVec = exceptionTyIdxVec;
  javacatchnd->srcPosition = srcPos;
  return javacatchnd;
}

StmtNode *SwitchMeStmt::EmitStmt(SSATab *ssaTab) {
  SwitchNode *swnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<SwitchNode>(&(ssaTab->mirModule));
  swnode->defaultLabel = defaultLabel;
  swnode->switchTable = switchTable;
  swnode->switchOpnd = opnd->EmitExpr(ssaTab);
  swnode->srcPosition = srcPos;
  return swnode;
}

StmtNode *CommentMeStmt::EmitStmt(SSATab *ssaTab) {
  CommentNode *cmmnt = ssaTab->mirModule.CurFunction()->codeMemPool->New<CommentNode>(&(ssaTab->mirModule));
  cmmnt->comment = comment;
  cmmnt->srcPosition = srcPos;
  return cmmnt;
}

StmtNode *ThrowMeStmt::EmitStmt(SSATab *ssaTab) {
  UnaryStmtNode *ustmt = ssaTab->mirModule.CurFunction()->codeMemPool->New<UnaryStmtNode>(OP_throw);
  ustmt->SetOpnd(opnd->EmitExpr(ssaTab), 0);
  ustmt->srcPosition = srcPos;
  return ustmt;
}

StmtNode *GosubMeStmt::EmitStmt(SSATab *ssaTab) {
  GotoNode *gosub = ssaTab->mirModule.CurFunction()->codeMemPool->New<GotoNode>(OP_gosub);
  gosub->offset = offset;
  gosub->srcPosition = srcPos;
  return gosub;
}

StmtNode *AssertMeStmt::EmitStmt(SSATab *ssaTab) {
  AssertStmtNode *assstmtnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<AssertStmtNode>(op);
  assstmtnode->bOpnd[0] = opnds[0]->EmitExpr(ssaTab);
  assstmtnode->bOpnd[1] = opnds[1]->EmitExpr(ssaTab);
  assstmtnode->srcPosition = srcPos;
  return assstmtnode;
}

void BB::EmitBB(SSATab *ssaTab, BlockNode *curblk, MapleMap<uint32, uint32> &freqmap, bool need_another_pass) {
  CHECK_FATAL(curblk != nullptr, "null ptr check");
  StmtNode *bbFirstStmt = nullptr;
  StmtNode *bbLastStmt = nullptr;
  // emit head. label
  LabelIdx labidx = bbLabel;
  if (labidx != 0) {
    // not a empty bb
    LabelNode *lbnode = ssaTab->mirModule.CurFunction()->codeMemPool->New<LabelNode>();
    lbnode->labelIdx = labidx;
    curblk->AddStatement(lbnode);
    bbFirstStmt = lbnode;
    bbLastStmt = lbnode;
  }
  for (auto mestmt : meStmtList) {
    if (!need_another_pass) {
      if (mestmt->op == OP_interfaceicall || mestmt->op == OP_virtualicall) {
        mestmt->op = OP_icall;
      } else if (mestmt->op == OP_interfaceicallassigned || mestmt->op == OP_virtualicallassigned) {
        mestmt->op = OP_icallassigned;
      }
    }
    StmtNode *stmt = mestmt->EmitStmt(ssaTab);
    if (stmt != nullptr) {
      curblk->AddStatement(stmt);
      if (bbFirstStmt == nullptr) {
        bbFirstStmt = stmt;
      }
      bbLastStmt = stmt;
    }
  }
  if (IsEndTry()) {
    /* generate op_endtry */
    StmtNode *endtry = ssaTab->mirModule.CurFunction()->codeMemPool->New<StmtNode>(OP_endtry);
    curblk->AddStatement(endtry);
    if (bbFirstStmt == nullptr) {
      bbFirstStmt = endtry;
    }
    bbLastStmt = endtry;
  }
  stmtNodeList.first = bbFirstStmt;
  stmtNodeList.last = bbLastStmt;
}

}  // namespace maple
