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

#include "irmap.h"
#include "ssa_mir_nodes.h"
#include "ssa.h"
#include <queue>
#include "mir_builder.h"
#include "constant_fold.h"

// This file contains infrastructure support for Hashed SSA Form.

namespace maple {

VarMeExpr *IRMap::CreateVarMeExprVersion(OriginalSt *ost) {
  VarMeExpr *varmeexpr = New<VarMeExpr>(exprID++, ost, verst2MeExprTable.size(),
        GlobalTables::GetTypeTable().typeTable[ost->tyIdx.GetIdx()]->primType);
  verst2MeExprTable.push_back(varmeexpr);
  return varmeexpr;
}

VarMeExpr *IRMap::GetOrCreateZeroVersionVarMeExpr(OriginalSt *ost) {
  ASSERT(ost->zeroVersionIndex != 0 && ost->zeroVersionIndex < verst2MeExprTable.size(),
          "GetOrCreateZeroVersionVarMeExpr: version index of osym's INIT_VERSION out of range");
  if (verst2MeExprTable[ost->zeroVersionIndex] == nullptr) {
    VarMeExpr *varmeexpr = New<VarMeExpr>(exprID++, ost, ost->zeroVersionIndex,
          GlobalTables::GetTypeTable().typeTable[ost->tyIdx.GetIdx()]->primType);
    verst2MeExprTable[ost->zeroVersionIndex] = varmeexpr;
    return varmeexpr;
  }
  return static_cast<VarMeExpr *>(verst2MeExprTable[ost->zeroVersionIndex]);
}

// create a new variable with the name given by strIdx
VarMeExpr *IRMap::CreateNewVar(GStrIdx strIdx, PrimType primType, bool isGlobal) {
  MIRSymbol *st =
      mirModule->mirBuilder->CreateSymbol((TyIdx)primType, strIdx, kStVar, isGlobal ? kScGlobal : kScAuto,
                   isGlobal ? kScopeGlobal : kScopeLocal, isGlobal? nullptr : mirModule->CurFunction());
  if (isGlobal)
    st->isTmp = 1;
  OriginalSt *ost = ssaTab->CreateSymbolOriginalSt(st, isGlobal ? 0 : mirModule->CurFunction()->puIdx , 0);
  ost->zeroVersionIndex = verst2MeExprTable.size();
  verst2MeExprTable.push_back(nullptr);
  ost->versionsIndex.push_back(ost->zeroVersionIndex);

  VarMeExpr *varx = New<VarMeExpr>(exprID++, ost, verst2MeExprTable.size(), primType);
  verst2MeExprTable.push_back(varx);
  return varx;
}

// create a new localrefvar with the name given by strIdx and tyIdx given by tidx
VarMeExpr *IRMap::CreateNewLocalrefvarTemp(GStrIdx strIdx, TyIdx tidx) {
  MIRSymbol *st = mirModule->mirBuilder->CreateLocalDecl(GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx).c_str(),
                                                         GlobalTables::GetTypeTable().GetTypeFromTyIdx(tidx));
  st->instrumented = 1;
  OriginalSt *ost = ssaTab->CreateSymbolOriginalSt(st, mirModule->CurFunction()->puIdx, 0);
  ost->zeroVersionIndex = verst2MeExprTable.size();
  verst2MeExprTable.push_back(nullptr);
  ost->versionsIndex.push_back(ost->zeroVersionIndex);

  VarMeExpr *newlocalrefvar = New<VarMeExpr>(exprID++, ost, verst2MeExprTable.size(), PTY_ref);
  verst2MeExprTable.push_back(newlocalrefvar);
  return newlocalrefvar;
}

RegMeExpr *IRMap::CreateRegMeExprVersion(OriginalSt *ost) {
  RegMeExpr *regreadexpr =
    New<RegMeExpr>(exprID++, ost, verst2MeExprTable.size(), kMeOpReg, OP_regread, ost->GetMIRPreg()->primType);
  verst2MeExprTable.push_back(regreadexpr);
  return regreadexpr;
}

RegMeExpr *IRMap::CreateRegMeExpr(PrimType primType) {
  MIRFunction *mirFunc = mirModule->CurFunction();
  PregIdx regIdx = mirFunc->pregTab->CreatePreg(primType);
  CHECK_FATAL(regIdx <= 0xffff, "register oversized");
  OriginalSt *ost = ssaTab->originalStTable.CreatePregOriginalSt(regIdx, mirFunc->puIdx);
  return CreateRegMeExprVersion(ost);
}

RegMeExpr *IRMap::CreateRegMeExpr(MIRType *mirType) {
  if (mirType->primType != PTY_ref && mirType->primType != PTY_ptr) {
    return CreateRegMeExpr(mirType->primType);
  }
  if (mirType->primType == PTY_ptr) {
    MIRType *pointedType = static_cast<MIRPtrType *>(mirType)->GetPointedType();
    if (pointedType == nullptr || pointedType->typeKind != kTypeFunction) {
      return CreateRegMeExpr(mirType->primType);
    }
  }
  MIRFunction *mirFunc = mirModule->CurFunction();
  PregIdx regIdx = mirFunc->pregTab->CreatePreg(mirType->primType, mirType);
  CHECK_FATAL(regIdx <= 0xffff, "register oversized");
  OriginalSt *ost = ssaTab->originalStTable.CreatePregOriginalSt(regIdx, mirFunc->puIdx);
  return CreateRegMeExprVersion(ost);
}

MeExpr *IRMap::CreateAddrofMeExpr(MeExpr *expr) {
  if (expr->meOp == kMeOpVar) {
    VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(expr);
    AddrofMeExpr addrofme(-1, PTY_ptr, varmeexpr->ost->index);
    return HashMeExpr(&addrofme);
  } else {
    CHECK_FATAL(expr->meOp == kMeOpIvar, "expecting IVarMeExpr");
    IvarMeExpr *ivarexpr = static_cast<IvarMeExpr *>(expr);
    OpMeExpr opmeexpr(-1, OP_iaddrof, PTY_ref, 1);
    opmeexpr.fieldID = ivarexpr->fieldID;
    opmeexpr.tyIdx = ivarexpr->tyIdx;
    opmeexpr.SetOpnd(ivarexpr->base, 0);
    return HashMeExpr(&opmeexpr);
  }
}

IvarMeExpr *IRMap::BuildLhsIvar(MeExpr *baseaddr, IassignMeStmt *iassmestmt, FieldID fieldID) {
  IvarMeExpr *medef = New<IvarMeExpr>(exprID++, iassmestmt->rhs->primType, iassmestmt->tyIdx, fieldID);
  medef->base = baseaddr;
  medef->defStmt = iassmestmt;
  PutToBucket(medef->GetHashIndex() % mapHashLength, medef);
  return medef;
}

IvarMeExpr *IRMap::BuildLhsIvarFromIassMeStmt(IassignMeStmt *iassmestmt) {
  IvarMeExpr *ivarx = BuildLhsIvar(iassmestmt->lhsVar->base, iassmestmt, iassmestmt->lhsVar->fieldID);
  ivarx->volatileFromBaseSymbol = iassmestmt->lhsVar->volatileFromBaseSymbol;
  return ivarx;
}

void IRMap::PutToBucket(uint32 hashidx, MeExpr *meexpr) {
  MeExpr *headexpr = hashTable[hashidx];
  if (!headexpr) {
    hashTable[hashidx] = meexpr;
  } else {
    meexpr->next = headexpr;
    hashTable[hashidx] = meexpr;
  }
}

MeExpr *IRMap::HashMeExpr(MeExpr *meexpr) {
  uint32 hidx = meexpr->GetHashIndex() % mapHashLength;
  MeExpr *itexpr = hashTable[hidx];
  if (!kOpcodeInfo.NotPure(meexpr->op)) {  // search for existing node
    while (itexpr != nullptr) {
      if (meexpr->IsIdentical(itexpr)) {
        return itexpr;
      }
      itexpr = itexpr->next;
    }
  }
  // create new node
  switch (meexpr->meOp) {
    case kMeOpIvar: {
      IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(meexpr);
      IvarMeExpr *newivarexpr = New<IvarMeExpr>(exprID++, *ivarmeexpr);
      newivarexpr->mu = ivarmeexpr->mu;
      PutToBucket(hidx, newivarexpr);
      return newivarexpr;
    }
    case kMeOpOp: {
      OpMeExpr *opmeexpr = static_cast<OpMeExpr *>(meexpr);
      OpMeExpr *newopmeexpr = New<OpMeExpr>(*opmeexpr, exprID++);
      PutToBucket(hidx, newopmeexpr);
      return newopmeexpr;
    }
    case kMeOpConst: {
      ConstMeExpr *constmeexpr = static_cast<ConstMeExpr *>(meexpr);
      ConstMeExpr *newconstmeexpr = New<ConstMeExpr>(exprID++, constmeexpr->constVal, constmeexpr->primType);
      PutToBucket(hidx, newconstmeexpr);
      return newconstmeexpr;
    }
    case kMeOpConststr: {
      ConststrMeExpr *constmeexpr = static_cast<ConststrMeExpr *>(meexpr);
      ConststrMeExpr *newconstmeexpr = New<ConststrMeExpr>(exprID++, constmeexpr->strIdx, constmeexpr->primType);
      PutToBucket(hidx, newconstmeexpr);
      return newconstmeexpr;
    }
    case kMeOpConststr16: {
      Conststr16MeExpr *constmeexpr = static_cast<Conststr16MeExpr *>(meexpr);
      Conststr16MeExpr *newconstmeexpr = New<Conststr16MeExpr>(exprID++, constmeexpr->strIdx, constmeexpr->primType);
      PutToBucket(hidx, newconstmeexpr);
      return newconstmeexpr;
    }
    case kMeOpSizeoftype: {
      SizeoftypeMeExpr *sizeoftypeexpr = static_cast<SizeoftypeMeExpr *>(meexpr);
      SizeoftypeMeExpr *newmeexpr = New<SizeoftypeMeExpr>(exprID++, meexpr->primType, sizeoftypeexpr->tyIdx);
      PutToBucket(hidx, newmeexpr);
      return newmeexpr;
    }
    case kMeOpFieldsDist: {
      FieldsDistMeExpr *fieldsDistExpr = static_cast<FieldsDistMeExpr*>(meexpr);
      FieldsDistMeExpr *newmeexpr = New<FieldsDistMeExpr>(exprID++, meexpr->primType,  fieldsDistExpr->GetTyIdx(),
                                                          fieldsDistExpr->GetFieldID1(), fieldsDistExpr->GetFieldID2());
      PutToBucket(hidx, newmeexpr);
      return newmeexpr;
    }
    case kMeOpAddrof: {
      AddrofMeExpr *addrofmeexpr = static_cast<AddrofMeExpr *>(meexpr);
      AddrofMeExpr *newaddrofmeexpr = New<AddrofMeExpr>(exprID++, meexpr->primType, addrofmeexpr->ostIdx);
      PutToBucket(hidx, newaddrofmeexpr);
      return newaddrofmeexpr;
    }
    case kMeOpNary: {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(meexpr);
      NaryMeExpr *newnarymeexpr = NewInPool<NaryMeExpr>(exprID++, *narymeexpr);
      PutToBucket(hidx, newnarymeexpr);
      return newnarymeexpr;
    }
    case kMeOpAddroffunc: {
      AddroffuncMeExpr *funcmeexpr = static_cast<AddroffuncMeExpr *>(meexpr);
      AddroffuncMeExpr *newexpr = New<AddroffuncMeExpr>(exprID++, funcmeexpr->puIdx);
      PutToBucket(hidx, newexpr);
      return newexpr;
    }
    case kMeOpAddroflabel: {
      AddroflabelMeExpr *labmeexpr = static_cast<AddroflabelMeExpr *>(meexpr);
      AddroflabelMeExpr *newexpr = New<AddroflabelMeExpr>(exprID++, labmeexpr->labelIdx);
      PutToBucket(hidx, newexpr);
      return newexpr;
    }
    case kMeOpGcmalloc: {
      GcmallocMeExpr *gcmeexpr = static_cast<GcmallocMeExpr *>(meexpr);
      GcmallocMeExpr *newexpr = New<GcmallocMeExpr>(exprID++, meexpr->op, meexpr->primType, gcmeexpr->tyIdx);
      PutToBucket(hidx, newexpr);
      return newexpr;
    }
    default:
      CHECK_FATAL(false, "not yet implement");
  }
}

// replace meexpr with repexpr. meexpr must be a kid of origexpr
// return repexpr's parent if replaced, otherwise return nullptr
MeExpr *IRMap::ReplaceMeExprExpr(MeExpr *origexpr, MeExpr *meexpr, MeExpr *repexpr) {
  if (origexpr->IsLeaf()) {
    return origexpr;
  }

  switch (origexpr->meOp) {
    case kMeOpOp: {
      OpMeExpr *opmeexpr = static_cast<OpMeExpr *>(origexpr);
      OpMeExpr newmeexpr(*opmeexpr, -1);
      bool needRehash = false;
      for (uint32 i = 0; i < opmeexpr->numOpnds; i++) {
        if (opmeexpr->GetOpnd(i) == meexpr) {
          needRehash = true;
          newmeexpr.SetOpnd(repexpr, i);
        } else if (!opmeexpr->GetOpnd(i)->IsLeaf()) {
          newmeexpr.SetOpnd(ReplaceMeExprExpr(newmeexpr.GetOpnd(i), meexpr, repexpr), i);
          if (newmeexpr.GetOpnd(i) != opmeexpr->GetOpnd(i)) {
            needRehash = true;
          }
        }
      }
      if (needRehash) {
        newmeexpr.opndType = opmeexpr->opndType;
        newmeexpr.bitsOffset = opmeexpr->bitsOffset;
        newmeexpr.bitsSize = opmeexpr->bitsSize;
        newmeexpr.tyIdx = opmeexpr->tyIdx;
        newmeexpr.fieldID = opmeexpr->fieldID;
        return HashMeExpr(&newmeexpr);
      }
      return origexpr;
    }
    case kMeOpNary: {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(origexpr);
      NaryMeExpr newmeexpr(&irMapAlloc, -1, *narymeexpr);
      bool needRehash = false;
      for (uint32 i = 0; i < narymeexpr->numOpnds; i++) {
        MeExpr *opnd = narymeexpr->GetOpnd(i);
        if (opnd == meexpr) {
          newmeexpr.SetOpnd(repexpr, i);
          needRehash = true;
        } else if (!opnd->IsLeaf()) {
          newmeexpr.SetOpnd(ReplaceMeExprExpr(newmeexpr.GetOpnd(i), meexpr, repexpr), i);
          if (newmeexpr.GetOpnd(i) != opnd) {
            needRehash = true;
          }
        }
      }
      return needRehash ? HashMeExpr(&newmeexpr) : origexpr;
    }
    case kMeOpIvar: {
      IvarMeExpr *ivarexpr = static_cast<IvarMeExpr *>(origexpr);
      IvarMeExpr newmeexpr(-1, *ivarexpr);
      bool needRehash = false;
      if (ivarexpr->base == meexpr) {
        newmeexpr.base = repexpr;
        needRehash = true;
      } else if (!ivarexpr->base->IsLeaf()) {
        newmeexpr.base = ReplaceMeExprExpr(newmeexpr.base, meexpr, repexpr);
        if (newmeexpr.base != ivarexpr->base) {
          needRehash = true;
        }
      }
      return needRehash ? HashMeExpr(&newmeexpr) : origexpr;
    }
    default:
      CHECK_FATAL(false, "NYI");
  }
}

// replace meexpr in mestmt with repexpr
bool IRMap::ReplaceMeExprStmt(MeStmt *mestmt, MeExpr *meexpr, MeExpr *repexpr) {
  bool isReplaced = false;
  switch (mestmt->op) {
    case OP_dassign:
    case OP_regassign: {
      AssignMeStmt *dsmestmt = static_cast<AssignMeStmt *>(mestmt);
      MeExpr *rhs = dsmestmt->rhs;
      if (rhs == meexpr) {
        dsmestmt->rhs = repexpr;
        isReplaced = true;
      } else {
        dsmestmt->rhs = ReplaceMeExprExpr(dsmestmt->rhs, meexpr, repexpr);
        isReplaced = dsmestmt->rhs != rhs;
      }
      break;
    }
    case OP_maydassign: {
      MaydassignMeStmt *dsmestmt = static_cast<MaydassignMeStmt *>(mestmt);
      MeExpr *rhs = dsmestmt->rhs;
      if (rhs == meexpr) {
        dsmestmt->rhs = repexpr;
        isReplaced = true;
      } else {
        dsmestmt->rhs = ReplaceMeExprExpr(dsmestmt->rhs, meexpr, repexpr);
        isReplaced = dsmestmt->rhs != rhs;
      }
      break;
    }
    case OP_syncenter:
    case OP_syncexit:
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
    case OP_icallassigned:
    case OP_return: {
      NaryMeStmt *narymestmt = static_cast<NaryMeStmt *>(mestmt);
      for (int32 i = 0; i < narymestmt->NumMeStmtOpnds(); i++) {
        MeExpr *opnd = narymestmt->GetMeStmtOpnd(i);
        if (opnd == meexpr) {
          narymestmt->SetMeStmtOpnd(i, repexpr);
          isReplaced = true;
        } else if (!opnd->IsLeaf()) {
          narymestmt->SetMeStmtOpnd(i, ReplaceMeExprExpr(opnd, meexpr, repexpr));
          if (narymestmt->GetMeStmtOpnd(i) != opnd) {
            isReplaced = true;
          }
        }
      }
      break;
    }
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_intrinsiccallwithtype:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned: {
      NaryMeStmt *narymestmt = static_cast<NaryMeStmt *>(mestmt);
      for (int32 i = 0; i < narymestmt->NumMeStmtOpnds(); i++) {
        MeExpr *opnd = narymestmt->GetMeStmtOpnd(i);
        if (opnd->IsLeaf() && opnd->meOp == kMeOpVar) {
          VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(opnd);
          OriginalSt *orgsym = varmeexpr->ost;
          if (orgsym->IsSymbol() && orgsym->GetMIRSymbol()->GetAttr(ATTR_static)) {
            // its address may be taken
            continue;
          }
        }
        if (opnd == meexpr) {
          narymestmt->SetMeStmtOpnd(i, repexpr);
          isReplaced = true;
        } else if (!opnd->IsLeaf()) {
          narymestmt->SetMeStmtOpnd(i, ReplaceMeExprExpr(opnd, meexpr, repexpr));
          if (narymestmt->GetMeStmtOpnd(i) != opnd) {
            isReplaced = true;
          }
        }
      }
      break;
    }
    case OP_decref:
    case OP_decrefreset:
    case OP_incref:
    case OP_assertnonnull:
    case OP_eval:
    case OP_free:
    case OP_igoto:
    case OP_switch:
    case OP_brtrue:
    case OP_brfalse: {
      UnaryMeStmt *unarystmt = static_cast<UnaryMeStmt *>(mestmt);
      MeExpr *opnd = unarystmt->opnd;
      if (unarystmt->opnd == meexpr) {
        unarystmt->opnd = repexpr;
        isReplaced = true;
      } else {
        unarystmt->opnd = ReplaceMeExprExpr(unarystmt->opnd, meexpr, repexpr);
        isReplaced = unarystmt->opnd != opnd;
      }
      break;
    }
    case OP_throw: {
      ThrowMeStmt *thrmestmt = static_cast<ThrowMeStmt *>(mestmt);
      MeExpr *opnd = thrmestmt->opnd;
      if (opnd == meexpr) {
        thrmestmt->opnd = repexpr;
        isReplaced = true;
      } else {
        thrmestmt->opnd = ReplaceMeExprExpr(thrmestmt->opnd, meexpr, repexpr);
        isReplaced = thrmestmt->opnd != opnd;
      }
      break;
    }
    case OP_iassign: {
      IassignMeStmt *ivarstmt = static_cast<IassignMeStmt *>(mestmt);
      MeExpr *rhs = ivarstmt->rhs;
      if (rhs == meexpr) {
        ivarstmt->rhs = repexpr;
        isReplaced = true;
      } else if (!rhs->IsLeaf()) {
        ivarstmt->rhs = ReplaceMeExprExpr(ivarstmt->rhs, meexpr, repexpr);
        isReplaced = ivarstmt->rhs != rhs;
      }

      MeExpr *newbase = nullptr;
      if (ivarstmt->lhsVar->base == meexpr) {
        newbase = repexpr;
        isReplaced = true;
      } else if (!ivarstmt->lhsVar->base->IsLeaf()) {
        newbase = ReplaceMeExprExpr(ivarstmt->lhsVar->base, meexpr, repexpr);
        if (newbase != ivarstmt->lhsVar->base) {
          isReplaced = true;
        } else {
          newbase = nullptr;
        }
      }
      if (newbase) {
        ivarstmt->lhsVar = BuildLhsIvar(newbase, ivarstmt, ivarstmt->lhsVar->fieldID);
      }
      break;
    }
    case OP_assertlt:
    case OP_assertge: {
      AssertMeStmt *assmestmt = static_cast<AssertMeStmt *>(mestmt);
      MeExpr *opnd0 = assmestmt->opnds[0];
      if (opnd0 == meexpr) {
        assmestmt->opnds[0] = repexpr;
        isReplaced = true;
      } else if (!opnd0->IsLeaf()) {
        assmestmt->opnds[0] = ReplaceMeExprExpr(opnd0, meexpr, repexpr);
        isReplaced = assmestmt->opnds[0] != opnd0;
      }
      MeExpr *opnd1 = assmestmt->opnds[1];
      if (opnd1 == meexpr) {
        assmestmt->opnds[1] = repexpr;
        isReplaced = true;
      } else if (!opnd1->IsLeaf()) {
        assmestmt->opnds[1] = ReplaceMeExprExpr(opnd1, meexpr, repexpr);
        isReplaced = assmestmt->opnds[1] != opnd1;
      }
      break;
    }
    default:
      CHECK_FATAL(false, "NYI");
  }
  return isReplaced;
}

MePhiNode *IRMap::CreateMePhi(ScalarMeExpr *mevar) {
  MePhiNode *phime = NewInPool<MePhiNode>();
  phime->UpdateLhs(mevar);
  return phime;
}

AssignMeStmt *IRMap::CreateAssignMeStmt(ScalarMeExpr *lhs, MeExpr *rhs, BB *curbb) {
  AssignMeStmt *mestmt = nullptr;
  if (lhs->meOp == kMeOpReg) {
    mestmt = New<AssignMeStmt>(OP_regassign, lhs, rhs);
  } else {
    mestmt = NewInPool<DassignMeStmt>(lhs, rhs);
  }
  lhs->defBy = kDefByStmt;
  lhs->def.defStmt = mestmt;
  mestmt->bb = curbb;
  return mestmt;
}

// get the false goto bb, if condgoto is brtrue, take the other bb of brture @lable
// otherwise, take the bb of @lable
BB *IRMap::GetFalseBrBb(CondGotoMeStmt *condgoto) {
  LabelIdx lblidx = (LabelIdx)condgoto->offset;
  BB *gotobb = GetBBForLabidx(lblidx);
  BB *bb = condgoto->bb;
  CHECK_FATAL(bb->succ.size() == 2, "");
  if (condgoto->op == OP_brfalse) {
    return gotobb;
  } else {
    return gotobb == bb->succ[0] ? bb->succ[1] : bb->succ[0];
  }
}

MeExpr *IRMap::CreateConstMeExpr(PrimType primType, MIRConst *mirconst) {
  ConstMeExpr constmeexpr(-1, mirconst, primType);
  return HashMeExpr(&constmeexpr);
}

MeExpr *IRMap::CreateIntConstMeExpr(int64 value, PrimType primType) {
  MIRIntConst *intconst =
      mirModule->memPool->New<MIRIntConst>(value, GlobalTables::GetTypeTable().GetPrimType(primType));
  return CreateConstMeExpr(primType, intconst);
}

MeExpr *IRMap::CreateMeExprUnary(Opcode op, PrimType primType, MeExpr *expr0) {
  OpMeExpr opmeexpr(-1, op, primType, 1);
  opmeexpr.SetOpnd(expr0, 0);
  return HashMeExpr(&opmeexpr);
}

MeExpr *IRMap::CreateMeExprBinary(Opcode op, PrimType primType, MeExpr *expr0, MeExpr *expr1) {
  OpMeExpr opmeexpr(-1, op, primType, 2);
  opmeexpr.SetOpnd(expr0, 0);
  opmeexpr.SetOpnd(expr1, 1);
  return HashMeExpr(&opmeexpr);
}

MeExpr *IRMap::CreateMeExprSelect(PrimType primType, MeExpr *expr0, MeExpr *expr1, MeExpr *expr2) {
  OpMeExpr opmeexpr(-1, OP_select, primType, 3);
  opmeexpr.SetOpnd(expr0, 0);
  opmeexpr.SetOpnd(expr1, 1);
  opmeexpr.SetOpnd(expr2, 2);
  return HashMeExpr(&opmeexpr);
}

MeExpr *IRMap::CreateMeExprCompare(Opcode op, PrimType resptyp, PrimType opndptyp, MeExpr *opnd0, MeExpr *opnd1) {
  OpMeExpr opmeexpr(-1, op, resptyp, 2);
  opmeexpr.SetOpnd(opnd0, 0);
  opmeexpr.SetOpnd(opnd1, 1);
  opmeexpr.opndType = opndptyp;
  MeExpr *retmeexpr = HashMeExpr(&opmeexpr);
  static_cast<OpMeExpr *>(retmeexpr)->opndType = opndptyp;
  return retmeexpr;
}

MeExpr *IRMap::CreateMeExprIntrinsiciop1(MIRIntrinsicID id, PrimType primType, MeExpr *opnd0) {
  NaryMeExpr narymeexpr(&tempAlloc, -1, OP_intrinsicop, primType, 1, TyIdx(0), id, false);
  narymeexpr.PushOpnd(opnd0);
  return HashMeExpr(&narymeexpr);
}

MeExpr *IRMap::CreateMeExprTypeCvt(PrimType primType, PrimType opndptyp, MeExpr *opnd0) {
  OpMeExpr opmeexpr(-1, OP_cvt, primType, 1);
  opmeexpr.SetOpnd(opnd0, 0);
  opmeexpr.opndType = opndptyp;
  return HashMeExpr(&opmeexpr);
}

IntrinsiccallMeStmt *IRMap::CreateIntrinsicCallMeStmt(MIRIntrinsicID idx, std::vector<MeExpr*> &opnds, TyIdx tyidx) {
  IntrinsiccallMeStmt *meStmt =
      NewInPool<IntrinsiccallMeStmt>(tyidx == 0 ? OP_intrinsiccall : OP_intrinsiccallwithtype, idx, tyidx);
  for (MeExpr *opnd : opnds) {
    meStmt->opnds.push_back(opnd);
  }
  return meStmt;
}

IntrinsiccallMeStmt *IRMap::CreateIntrinsicCallAssignedMeStmt(MIRIntrinsicID idx, std::vector<MeExpr*> &opnds,
                                                              MeExpr *ret, TyIdx tyidx) {
  IntrinsiccallMeStmt *meStmt = NewInPool<IntrinsiccallMeStmt>(
      tyidx == 0 ? OP_intrinsiccallassigned : OP_intrinsiccallwithtypeassigned, idx, tyidx);
  for (MeExpr *opnd : opnds) {
    meStmt->opnds.push_back(opnd);
  }
  if (ret != nullptr) {
    ASSERT(ret->meOp == kMeOpReg || ret->meOp == kMeOpVar, "unexpected opcode");
    MustDefMeNode *mustdef = New<MustDefMeNode>(static_cast<ScalarMeExpr*>(ret), meStmt);
    meStmt->GetMustDefList()->push_back(*mustdef);
  }
  return meStmt;
}

MeExpr *IRMap::CreateAddrofMeExprFromNewSymbol(MIRSymbol *st, PUIdx puIdx) {
  OriginalSt *baseOst = ssaTab->CreateSymbolOriginalSt(st, puIdx, 0);
  baseOst->zeroVersionIndex = verst2MeExprTable.size();
  verst2MeExprTable.push_back(nullptr);
  baseOst->versionsIndex.push_back(baseOst->zeroVersionIndex);

  AddrofMeExpr addrofme(-1, PTY_ptr, baseOst->index);
  return HashMeExpr(&addrofme);
}

MeExpr *IRMap::SimplifyOpMeExpr(OpMeExpr *opmeexpr) {
  Opcode opop = opmeexpr->op;
  switch (opop) {
    case OP_cvt: {
      // return nullptr;
      OpMeExpr *cvtmeexpr = static_cast<OpMeExpr *>(opmeexpr);
      MeExpr *opnd0 = cvtmeexpr->GetOpnd(0);
      if (opnd0->meOp == kMeOpConst) {
        ConstantFold cf(mirModule);
        MIRConst *tocvt =
          cf.FoldTypeCvtMIRConst(static_cast<ConstMeExpr *>(opnd0)->constVal, opnd0->primType, cvtmeexpr->primType);
        if (tocvt) {
          return CreateConstMeExpr(cvtmeexpr->primType, tocvt);
        }
      }
      if (opnd0->op == OP_cvt) {
        OpMeExpr *cvtopnd0 = static_cast<OpMeExpr *>(opnd0);
        // cvtopnd0 should have tha same type as cvtopnd0->GetOpnd(0) or cvtmeexpr,
        // and the type size of cvtopnd0 should be ge(>=) one of them.
        // Otherwise, deleting the cvt of cvtopnd0 may result in information loss.
        if (maple::GetPrimTypeSize(cvtopnd0->primType) >= maple::GetPrimTypeSize(cvtopnd0->GetOpnd(0)->primType)) {
          if ((maple::IsPrimitiveInteger(cvtopnd0->primType) && maple::IsPrimitiveInteger(cvtopnd0->GetOpnd(0)->primType)) ||
              (maple::IsPrimitiveFloat(cvtopnd0->primType) && maple::IsPrimitiveFloat(cvtopnd0->GetOpnd(0)->primType))) {
            return CreateMeExprTypeCvt(cvtmeexpr->primType, cvtopnd0->opndType, cvtopnd0->GetOpnd(0));
          }
        }
        if (maple::GetPrimTypeSize(cvtopnd0->primType) >= maple::GetPrimTypeSize(cvtmeexpr->primType)) {
          if ((maple::IsPrimitiveInteger(cvtopnd0->primType) && maple::IsPrimitiveInteger(cvtmeexpr->primType)) ||
              (maple::IsPrimitiveFloat(cvtopnd0->primType) && maple::IsPrimitiveFloat(cvtmeexpr->primType))) {
            return CreateMeExprTypeCvt(cvtmeexpr->primType, cvtopnd0->opndType, cvtopnd0->GetOpnd(0));
          }
        }
      }
      return nullptr;
    }
    case OP_add:
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
    case OP_cand:
    case OP_land:
    case OP_cior:
    case OP_lior:
    case OP_depositbits: {
      if (!IsPrimitiveInteger(opmeexpr->primType)) {
        return nullptr;
      }
      MeExpr *opnd0 = opmeexpr->GetOpnd(0);
      MeExpr *opnd1 = opmeexpr->GetOpnd(1);
      if (opop == OP_sub && opnd0 == opnd1) {
        return CreateIntConstMeExpr(0, opmeexpr->primType);
      }
      if (opnd0->meOp != kMeOpConst || opnd1->meOp != kMeOpConst) {
        return nullptr;
      }
      maple::ConstantFold cf(mirModule);
      MIRIntConst *opnd0const = static_cast<MIRIntConst *>(static_cast<ConstMeExpr *>(opnd0)->constVal);
      MIRIntConst *opnd1const = static_cast<MIRIntConst *>(static_cast<ConstMeExpr *>(opnd1)->constVal);
      if ((opop == OP_div || opop == OP_rem)) {
        int64 opnd0constValue = opnd0const->value;
        int64 opnd1constValue = opnd1const->value;
        PrimType resPtyp = opmeexpr->primType;
        if (opnd1constValue == 0 ||
            (opnd1constValue == -1 && ((resPtyp == PTY_i32 && opnd0constValue == MININT32) ||
                                       (resPtyp == PTY_i64 && opnd0constValue == MININT64)))) {
          return nullptr;
        }
      }
      MIRConst *resconst = cf.FoldIntConstBinaryMIRConst(opmeexpr->op,
          opmeexpr->primType, opnd0const, opnd1const);
      return CreateConstMeExpr(opmeexpr->primType, resconst);
    }
    case OP_ne:
    case OP_eq:
    case OP_lt:
    case OP_le:
    case OP_ge:
    case OP_gt:
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg: {
      MeExpr *opnd0 = opmeexpr->GetOpnd(0);
      MeExpr *opnd1 = opmeexpr->GetOpnd(1);
      bool isneeq = (opop == OP_ne || opop == OP_eq);
      if (opnd0->meOp == kMeOpConst && opnd1->meOp == kMeOpConst) {
        maple::ConstantFold cf(mirModule);
        MIRConst *opnd0const = static_cast<ConstMeExpr *>(opnd0)->constVal;
        MIRConst *opnd1const = static_cast<ConstMeExpr *>(opnd1)->constVal;
        MIRConst *resconst = cf.FoldConstComparisonMIRConst(opmeexpr->op, opmeexpr->primType, opmeexpr->opndType,
                                                            opnd0const, opnd0->primType, opnd1const, opnd1->primType);
        return CreateConstMeExpr(opmeexpr->primType, resconst);
      } else if (isneeq && ((opnd0->meOp == kMeOpAddrof && opnd1->meOp == kMeOpConst) ||
                            (opnd0->meOp == kMeOpConst && opnd1->meOp == kMeOpAddrof))) {
        MIRConst *resconst = nullptr;
        if (opnd0->meOp == kMeOpAddrof) {
          MIRConst *constopnd1 = static_cast<ConstMeExpr *>(opnd1)->constVal;
          if (constopnd1->IsZero()) {
            // addrof will not be zero, so this comparison can be replaced with a constant
            resconst = mirModule->memPool->New<MIRIntConst>((opop == OP_ne), GlobalTables::GetTypeTable().typeTable.at(PTY_u1));
          }
        } else {
          MIRConst *constopnd0 = static_cast<ConstMeExpr *>(opnd0)->constVal;
          if (constopnd0->IsZero()) {
            // addrof will not be zero, so this comparison can be replaced with a constant
            resconst = mirModule->memPool->New<MIRIntConst>((opop == OP_ne), GlobalTables::GetTypeTable().typeTable.at(PTY_u1));
          }
        }
        if (resconst) {
          return CreateConstMeExpr(opmeexpr->primType, resconst);
        }
      } else if (isneeq && opnd0->op == OP_select &&
                 (opnd1->meOp == kMeOpConst && IsPrimitivePureScalar(opnd1->primType))) {
        OpMeExpr *opmeopnd0 = static_cast<OpMeExpr *>(opnd0);
        if (opmeopnd0->op == OP_select) {
          MeExpr *opnd01 = opmeopnd0->GetOpnd(1);
          MeExpr *opnd02 = opmeopnd0->GetOpnd(2);
          if (opnd01->meOp == kMeOpConst && IsPrimitivePureScalar(opnd01->primType) && opnd02->meOp == kMeOpConst &&
              IsPrimitivePureScalar(opnd02->primType)) {
            MIRConst *constopnd1 = static_cast<ConstMeExpr *>(opnd1)->constVal;
            MIRConst *constopnd01 = static_cast<ConstMeExpr *>(opnd01)->constVal;
            MIRConst *constopnd02 = static_cast<ConstMeExpr *>(opnd02)->constVal;
            bool needswapopnd = false;
            bool canbereplaced = false;
            bool isne = opmeexpr->op == OP_ne;
            if (isne && constopnd1->IsZero() && constopnd01->IsOne() && constopnd02->IsZero()) {
              canbereplaced = true;
            } else if (isne && constopnd1->IsZero() && constopnd01->IsZero() && constopnd02->IsOne()) {
              canbereplaced = true;
            } else if (isne && constopnd1->IsOne() && constopnd01->IsOne() && constopnd02->IsZero()) {
              needswapopnd = true;
              canbereplaced = true;
            } else if (isne && constopnd1->IsOne() && constopnd01->IsZero() && constopnd02->IsOne()) {
              needswapopnd = true;
              canbereplaced = true;
            } else if (!isne && constopnd1->IsZero() && constopnd01->IsOne() && constopnd02->IsZero()) {
              needswapopnd = true;
              canbereplaced = true;
            } else if (!isne && constopnd1->IsZero() && constopnd01->IsZero() && constopnd02->IsOne()) {
              needswapopnd = true;
              canbereplaced = true;
            } else if (!isne && constopnd1->IsOne() && constopnd01->IsOne() && constopnd02->IsZero()) {
              canbereplaced = true;
            } else if (!isne && constopnd1->IsOne() && constopnd01->IsZero() && constopnd02->IsOne()) {
              canbereplaced = true;
            }
            if (canbereplaced) {
              OpMeExpr newopmeexpr(-1, OP_select, PTY_u1, 3);
              newopmeexpr.SetOpnd(opmeopnd0->GetOpnd(0), 0);
              ConstMeExpr xnewopnd01(-1, constopnd01, PTY_u1);
              MeExpr *newopnd01 = HashMeExpr(&xnewopnd01);
              ConstMeExpr xnewopnd02(-1, constopnd02, PTY_u1);
              MeExpr *newopnd02 = HashMeExpr(&xnewopnd02);
              if (needswapopnd) {
                newopmeexpr.SetOpnd(newopnd02, 1);
                newopmeexpr.SetOpnd(newopnd01, 2);
              } else {
                newopmeexpr.SetOpnd(newopnd01, 1);
                newopmeexpr.SetOpnd(newopnd02, 2);
              }
              return HashMeExpr(&newopmeexpr);
            }
          }
        }
      }
      return nullptr;
    }
    default:
      return nullptr;
  }
}

MeExpr *IRMap::SimplifyMeExpr(MeExpr *x) {
  switch (x->meOp) {
  case kMeOpAddrof:
  case kMeOpAddroffunc:
  case kMeOpConst:
  case kMeOpConststr:
  case kMeOpConststr16:
  case kMeOpSizeoftype:
  case kMeOpFieldsDist:
  case kMeOpVar:
  case kMeOpReg:
  case kMeOpIvar: return x;
  case kMeOpOp: {
    OpMeExpr *opexp = static_cast<OpMeExpr *>(x);
    opexp->SetOpnd(SimplifyMeExpr(opexp->GetOpnd(0)), 0);
    if (opexp->numOpnds > 1) {
      opexp->SetOpnd(SimplifyMeExpr(opexp->GetOpnd(1)), 1);
      if (opexp->numOpnds > 2) {
        opexp->SetOpnd(SimplifyMeExpr(opexp->GetOpnd(2)), 2);
      }
    }
    MeExpr *newexp = SimplifyOpMeExpr(opexp);
    if (newexp) {
      return newexp;
    }
    return opexp;
  }
  case kMeOpNary: {
    NaryMeExpr *opexp = static_cast<NaryMeExpr *>(x);
    for (uint32 i = 0; i < opexp->numOpnds; i++) {
      opexp->SetOpnd(SimplifyMeExpr(opexp->GetOpnd(i)), i);
    }
    // TODO do the simplification of this op
    return opexp;
  }
  default: ;
  }
  return x;
}

} // namespace maple
