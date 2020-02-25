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

#include "me_rc_lowering.h"

#include <cstring>
#include "me_option.h"
#include "class_hierarchy.h"

/*
 * RCLowering phase generate RC intrinsic for reference assignment
 * based on previous analyze results. RC intrinsic will later be lowered
 * in Code Generation
 */

namespace maple {

static MIRIntrinsicID rcIntrin[] = {
#define GETINTRINID(intrinid, index) INTRN_##intrinid,
  RC_INTRINSICS(GETINTRINID)
#undef GETINTRINID
};

// move to bool MIRStructType::IsUnownedRefField(FieldID id)
static bool IsUnownedRefField(MIRStructType *type, FieldID id) {
  if (type->typeKind == kTypeClass) {
    MIRClassType *classtype = static_cast<MIRClassType *>(type);
    return classtype->IsFieldRCUnownedRef(id);
  }
  return false;
}

// move to MeFunction::GetMeExprFromSym as func has ssaTab and irMap
VarMeExpr *RCLowering::GetMeExprFromSym(MIRSymbol *sym) const {
  OriginalSt *ost = ssaTab->FindOrCreateSymbolOriginalSt(sym, func->mirFunc->puIdx, 0);
  return irMap->GetOrCreateZeroVersionVarMeExpr(ost);
}

// note that RCInstrinsic creation will check the ref assignment and reuse lhs if possible
IntrinsiccallMeStmt *RCLowering::CreateRCIntrinsic(MrtRcCall index, MeStmt *stmt, bool assigned) {
  IntrinsiccallMeStmt *intrn = nullptr;
  MIRIntrinsicID intrnId = rcIntrin[index];
  if (assigned) {
    intrn = irMap->NewInPool<IntrinsiccallMeStmt>(OP_intrinsiccallassigned, intrnId);
    RegMeExpr *curTemp = nullptr;
    if (stmt && stmt->op == OP_regassign) {
      curTemp = static_cast<AssignMeStmt *>(stmt)->GetRegLhs();
    } else {
      curTemp = GetRegTemp(PTY_ptr);
    }
    MustDefMeNode *mustDef = irMap->New<MustDefMeNode>(curTemp, intrn);
    intrn->mustDefList.push_back(*mustDef);
  } else {
    intrn = irMap->NewInPool<IntrinsiccallMeStmt>(OP_intrinsiccall, intrnId);
  }
  if (stmt) {
    intrn->srcPos = stmt->srcPos;
  }
  return intrn;
}

IntrinsiccallMeStmt *RCLowering::CreateIntrinsicWithOneArg(MrtRcCall index, MeStmt *stmt,
                                                           MeExpr *arg0, bool assigned) {
  IntrinsiccallMeStmt *refcall = CreateRCIntrinsic(index, stmt, assigned);
  refcall->opnds.push_back(arg0);
  return refcall;
}

IntrinsiccallMeStmt *RCLowering::CreateIntrinsicWithTwoArg(MrtRcCall index, MeStmt *stmt,
                                                           MeExpr *arg0, MeExpr *arg1, bool assigned) {
  IntrinsiccallMeStmt *intrn = CreateRCIntrinsic(index, stmt, assigned);
  intrn->opnds.push_back(arg0);
  intrn->opnds.push_back(arg1);
  return intrn;
}

IntrinsiccallMeStmt *RCLowering::CreateIntrinsicWithThreeArg(MrtRcCall index, MeStmt *stmt,
                                                             MeExpr *arg0, MeExpr *arg1, MeExpr *arg2, bool assigned) {
  IntrinsiccallMeStmt *intrn = CreateRCIntrinsic(index, stmt, assigned);
  intrn->opnds.push_back(arg0);
  intrn->opnds.push_back(arg1);
  intrn->opnds.push_back(arg2);
  return intrn;
}

MrtRcCall RCLowering::PrepareVolatileCall(MeStmt *stmt, MrtRcCall index) {
  if (index == kLoadVolStatic || index == kLoadVolWeak || index == kLoadVol) {
    MeStmt *next = stmt->next;
    if (next && next->op == OP_membaracquire) {
      stmt->bb->RemoveMeStmt(next);
    }
  } else { // volatile store case
    MeStmt *prev = stmt->prev;
    if (prev && prev->op == OP_membarrelease) {
      stmt->bb->RemoveMeStmt(prev);
    }
    MeStmt *next = stmt->next;
    if (next && next->op == OP_membarstoreload) {
      stmt->bb->RemoveMeStmt(next);
    }
  }
  return index;
}

// this function returns true if we generated new MRT calls and replaced rhs
bool RCLowering::HandleSpecialRHS(MeStmt *stmt) {
  bool changed = false;
  MeExpr *rhs = stmt->GetRhs();
  CHECK_FATAL(rhs != nullptr, "rhs is nullptr in RCLowering::HandleSpecialRHS");
  if (rhs->meOp == kMeOpVar) {
    VarMeExpr *varRhs = static_cast<VarMeExpr *>(rhs);
    MIRSymbol *rhsSym = varRhs->ost->GetMIRSymbol();
    if (rhsSym->IsGlobal() && !rhsSym->IsFinal() && !rhsSym->IsLiteralPtr()) {
      // load global into temp and update rhs to temp
      bool isVolatile = varRhs->IsVolatile(ssaTab);
      if (MeOption::strictNaiverc && !isVolatile) {
        if (stmt->op == OP_regassign) {
          AssignMeStmt *regassstmt = static_cast<AssignMeStmt *>(stmt);
          MeStmt *inccall = CreateIntrinsicWithOneArg(kIncRef, stmt, regassstmt->lhs, true);
          stmt->bb->InsertMeStmtAfter(stmt, inccall);
        } else {
          MeStmt *tmp = irMap->CreateAssignMeStmt(GetRegTemp(PTY_ptr), varRhs, stmt->bb);
          stmt->bb->InsertMeStmtBefore(stmt, tmp);
          AssignMeStmt *regassstmt = static_cast<AssignMeStmt *>(tmp);
          MeStmt *inccall = CreateIntrinsicWithOneArg(kIncRef, regassstmt, regassstmt->lhs, true);
          stmt->bb->InsertMeStmtBefore(stmt, inccall);
          stmt->SetMeStmtOpnd(1, lastTempReg);
        }
      } else {
        MeStmt *loadcall =
            CreateIntrinsicWithOneArg((isVolatile ? PrepareVolatileCall(stmt, kLoadVolStatic) : kLoadStatic),
                                      stmt, irMap->CreateAddrofMeExpr(rhs), true);
        if (stmt->op == OP_regassign) {
          stmt->bb->ReplaceMeStmt(stmt, loadcall);
        } else {
          stmt->bb->InsertMeStmtBefore(stmt, loadcall);
        }
        stmt->SetMeStmtOpnd(1, lastTempReg);
      }
      changed = true;
    } else {
      // regular dread + incref which can be before stmt
    }
  } else if (rhs->meOp == kMeOpIvar) {
    IvarMeExpr *irhs = static_cast<IvarMeExpr *>(rhs);
    // @Weak annotation handling
    if (irhs->IsRCWeak()) {
      bool isVolatile = irhs->IsVolatile();
      MeStmt *loadcall = CreateIntrinsicWithTwoArg(isVolatile ? PrepareVolatileCall(stmt, kLoadVolWeak) : kLoadWeak, stmt,
                                                   irhs->base->GetAddrExprBase(), irMap->CreateAddrofMeExpr(irhs), true);
      if (stmt->op == OP_regassign) {
        stmt->bb->ReplaceMeStmt(stmt, loadcall);
      } else {
        stmt->bb->InsertMeStmtBefore(stmt, loadcall);
        stmt->SetMeStmtOpnd(1, lastTempReg);
      }
      changed = true;
    } else if (irhs->IsVolatile()) {
      MeStmt *loadcall = CreateIntrinsicWithTwoArg(PrepareVolatileCall(stmt, kLoadVol), stmt,
                                                   irhs->base->GetAddrExprBase(),
                                                   irMap->CreateAddrofMeExpr(irhs), true);
      if (stmt->op == OP_regassign) {
        stmt->bb->ReplaceMeStmt(stmt, loadcall);
      } else {
        stmt->bb->InsertMeStmtBefore(stmt, loadcall);
      }
      stmt->SetMeStmtOpnd(1, lastTempReg);
      changed = true;
    } else if (!irhs->IsFinal()) {
      if (MeOption::strictNaiverc) {
        // LogInfo::MapleLogger() << "strictNaiverc " << std::endl;
        if (stmt->op == OP_regassign) {
          // perform incref on regassign
          AssignMeStmt *regassstmt = static_cast<AssignMeStmt *>(stmt);
          MeStmt *inccall = CreateIntrinsicWithOneArg(kIncRef, stmt, regassstmt->lhs, true);
          stmt->bb->InsertMeStmtAfter(stmt, inccall);
        } else {
          // put rhs into reg and insert incref
          MeStmt *tmp = irMap->CreateAssignMeStmt(GetRegTemp(PTY_ptr), rhs, stmt->bb);
          stmt->bb->InsertMeStmtBefore(stmt, tmp);
          AssignMeStmt *regassstmt = static_cast<AssignMeStmt *>(tmp);
          MeStmt *inccall = CreateIntrinsicWithOneArg(kIncRef, regassstmt, regassstmt->lhs, true);
          stmt->bb->InsertMeStmtBefore(stmt, inccall);
          stmt->SetMeStmtOpnd(1, lastTempReg);
        }
      } else {
        MeStmt *loadcall = CreateIntrinsicWithTwoArg(kLoadRef, stmt, irhs->base->GetAddrExprBase(),
                                                     irMap->CreateAddrofMeExpr(irhs), true);
        if (stmt->op == OP_regassign) {
          stmt->bb->ReplaceMeStmt(stmt, loadcall);
        } else {
          stmt->bb->InsertMeStmtBefore(stmt, loadcall);
          stmt->SetMeStmtOpnd(1, lastTempReg);
        }
      }
      changed = true;
    }
  }
  return changed;
}

void RCLowering::HandleReturnDecRef(MeStmt *stmt, MeExpr *pendingDec, BB *bb) {
  CHECK_FATAL(stmt->GetMustDefList() != nullptr, "null ptr check");
  MapleVector<MustDefMeNode> *mustdefs = stmt->GetMustDefList();
  if (mustdefs->size()) {
    // decref for old value
    ScalarMeExpr *lhs = mustdefs->front().lhs;
    if (lhs->meOp != kMeOpVar) {
      return;
    }
    OriginalSt *ost = static_cast<VarMeExpr *>(lhs)->ost;
    if (!ost->IsSymbol()) {
      return;
    }
    MIRSymbol *retSym = ost->GetMIRSymbol();
    if (retSym == nullptr) {
      return;
    }
    // rcunowned needs special handling
    if (retSym->IgnoreRC() && !retSym->GetAttr(ATTR_rcunowned)) {
      return;
    }
    assignedPtrSym.insert(retSym);
    if (retSym->GetAttr(ATTR_rcunowned)) {
      // if retSym is rcunowned, we need to introduce a new localrefvar to decref in cleanup
      MeStmt *regToTemp = irMap->CreateAssignMeStmt(GetMeExprForNewTemp(true), lhs, bb);
      bb->InsertMeStmtAfter(stmt, regToTemp);
    } else if (stmt->NeedDecref()) {
      if (dynamic_cast<CallMeStmt *>(stmt)) {
        // simple optimization for callassign
        // instead of change callassign {dassign} to backup; callassign {dassign}; decref
        // callassign {regassign}; backup; dassign (regread); decref
        CallMeStmt *callstmt = static_cast<CallMeStmt *>(stmt);
        RegMeExpr *curTemp = GetRegTemp(PTY_ptr);
        MeStmt *regToVar = irMap->CreateAssignMeStmt(static_cast<VarMeExpr*>(lhs), lastTempReg, bb);
        callstmt->mustDefList.front().lhs = lastTempReg;
        MeStmt *backup = irMap->CreateAssignMeStmt(GetRegTemp(PTY_ptr), pendingDec, bb);
        MeStmt *decrefCall = CreateIntrinsicWithOneArg(kDecRef, stmt, static_cast<AssignMeStmt *>(backup)->lhs);
        bb->InsertMeStmtAfter(stmt, decrefCall);
        bb->InsertMeStmtAfter(stmt, regToVar);
        bb->InsertMeStmtAfter(stmt, backup);
      } else {
        MeStmt *backup = irMap->CreateAssignMeStmt(GetRegTemp(PTY_ptr), pendingDec, bb);
        bb->InsertMeStmtBefore(stmt, backup);
        MeStmt *decrefCall = CreateIntrinsicWithOneArg(kDecRef, stmt, static_cast<AssignMeStmt *>(backup)->lhs);
        bb->InsertMeStmtAfter(stmt, decrefCall);
      }
      pendingDec = nullptr;
    }
  } else {
    // introduce a ret and decref on it as callee has +1 return ref
    RegMeExpr *curTemp = GetRegTemp(PTY_ptr);
    stmt->GetMustDefList()->push_back(MustDefMeNode(curTemp, stmt));
    MeStmt *decrefCall = CreateIntrinsicWithOneArg(kDecRef, stmt, curTemp);
    bb->InsertMeStmtAfter(stmt, decrefCall);
  }
}

bool RCLowering::RCFirst(MeExpr *rhs) {
  // null, local var/reg read
  if (rhs->meOp == kMeOpConst) {
    return static_cast<ConstMeExpr *>(rhs)->IsZero();
  } else if (rhs->meOp == kMeOpVar) {
    VarMeExpr *rhsVar = static_cast<VarMeExpr *>(rhs);
    MIRSymbol *sym = rhsVar->ost->GetMIRSymbol();
    return sym->IsLocal();
  }
  return rhs->meOp == kMeOpReg;
}

void RCLowering::RCLower(BB *bb) {
  MeExpr *pendingDec = nullptr;
  for (auto stmt : bb->meStmtList) {
    Opcode opcode = stmt->op;
    switch (opcode) {
      case OP_incref: {
        UnaryMeStmt *incref = static_cast<UnaryMeStmt *>(stmt);
        MeStmt *inccall = CreateIntrinsicWithOneArg(kIncRef, stmt, incref->opnd);
        bb->InsertMeStmtBefore(stmt, inccall);
        bb->RemoveMeStmt(stmt);
        break;
      }
      case OP_decref: {
        // save the operand for decref to use or generate decref if at exit
        UnaryMeStmt *decref = static_cast<UnaryMeStmt *>(stmt);
        if (decref->decref_before_exit) {
          // lower into decref
          MeStmt *deccall = CreateIntrinsicWithOneArg(kDecRef, stmt, decref->opnd);
          bb->InsertMeStmtBefore(stmt, deccall);
        } else {
          pendingDec = decref->opnd;
        }
        bb->RemoveMeStmt(stmt);
        break;
      }
      case OP_decrefreset: {
        UnaryMeStmt *decrefreset = static_cast<UnaryMeStmt *>(stmt);
        MeStmt *resetcall = CreateIntrinsicWithOneArg(kDecReset, stmt, decrefreset->opnd);
        bb->InsertMeStmtBefore(stmt, resetcall);
        bb->RemoveMeStmt(stmt);
        break;
      }
      case OP_regassign: {
        if (stmt->NeedIncref()) {
          if (!HandleSpecialRHS(stmt)) {
            MeStmt *inccall = CreateIntrinsicWithOneArg(kIncRef, stmt, (static_cast<AssignMeStmt *>(stmt))->lhs);
            bb->InsertMeStmtAfter(stmt, inccall);
          }
        }
        break;
      }
      case OP_dassign:
      case OP_maydassign: {
        MeExpr *rhs = stmt->GetRhs();
        VarMeExpr *lhs = stmt->GetVarLhs();
        bool incWithLhs = stmt->NeedIncref();
        bool decWithLhs = stmt->NeedDecref();
        CHECK_FATAL(rhs != nullptr, "null ptr check");
        CHECK_FATAL(lhs != nullptr, "null ptr check");
        MIRSymbol *lsym = lhs->ost->GetMIRSymbol();

        if (opcode == OP_dassign) {
          if (lsym->storageClass == kScAuto || lsym->storageClass == kScFormal) {
            assignedPtrSym.insert(lsym);
          }
          if (rhs->meOp == kMeOpGcmalloc) {
            if (lsym->GetAttr(ATTR_rcunowned)) {
              // if new obj is assigned to unowned refvar, we need a localrefvar
              // to decref at exit
              // introduce new localrefvar = lhs after current stmt
              MeStmt *backup = irMap->CreateAssignMeStmt(GetMeExprForNewTemp(true), lhs, bb);
              // backup will not have any incref/decref
              bb->InsertMeStmtAfter(stmt, backup);
            }
          }
        }
        if (!incWithLhs && !decWithLhs) {
          break;
        }
        // CHECK_FATAL(lsym->IgnoreRC(), "symbol ignoring RC");
        if (lsym->IgnoreRC()) {
          pendingDec = nullptr;
          break;
        }
        if (incWithLhs && HandleSpecialRHS(stmt)) {
          // note that rhs may have been updated
          incWithLhs = false;
          rhs = stmt->GetRhs();
        }
        CHECK_FATAL(rhs != nullptr, "null ptr check");
        bool incDecFirst = RCFirst(rhs);
        if (lsym->IsGlobal()) {
          // global write can alias with formals
          globalWrite = true;
          // decref could be optimized away after if null check
          CHECK_FATAL(!decWithLhs || pendingDec, "no pendingDec");
          if (lhs->IsVolatile(ssaTab)) {
            PrepareVolatileCall(stmt); // default is volatile write
            MeStmt *writeRefCall =
                CreateIntrinsicWithTwoArg(incWithLhs ? (decWithLhs ? kWriteVolStatic : kWriteVolStaticNoDec)
                                          : (decWithLhs ? kWriteVolStaticNoInc : kWriteVolStaticNoRc),
                                          stmt, irMap->CreateAddrofMeExpr(lhs), rhs);
            bb->ReplaceMeStmt(stmt, writeRefCall);
          } else {
            MeStmt *writeRefCall =
                CreateIntrinsicWithTwoArg(incWithLhs ? (decWithLhs ? kWriteStatic : kWriteStaticNoDec)
                                          : (decWithLhs ? kWriteStaticNoInc : kWriteStaticNoRc),
                                          stmt, irMap->CreateAddrofMeExpr(lhs), rhs);
            bb->ReplaceMeStmt(stmt, writeRefCall);
          }
        } else {
          // local assign, backup old value and insert Inc and Dec after
          if (decWithLhs) {
            CHECK_FATAL(pendingDec, "no pendingDec");
            if (incDecFirst) {
              // temp is not needed
              if (incWithLhs) {
                MeStmt *inccall = CreateIntrinsicWithOneArg(kIncRef, stmt, rhs);
                bb->InsertMeStmtBefore(stmt, inccall);
                incWithLhs = false;
              }
              MeStmt *deccall = CreateIntrinsicWithOneArg(kDecRef, stmt, pendingDec);
              bb->InsertMeStmtBefore(stmt, deccall);
            } else {
              MeStmt *backup = irMap->CreateAssignMeStmt(GetRegTemp(PTY_ptr), pendingDec, bb);
              bb->InsertMeStmtBefore(stmt, backup);
              MeStmt *deccall = CreateIntrinsicWithOneArg(kDecRef, stmt, static_cast<DassignMeStmt *>(backup)->lhs);
              bb->InsertMeStmtAfter(stmt, deccall);
            }
          }
          if (incWithLhs) {
            MeStmt *inccall = CreateIntrinsicWithOneArg(kIncRef, stmt, lhs);
            bb->InsertMeStmtAfter(stmt, inccall);
          }
        }
        assignedPtrSym.insert(lsym);
        pendingDec = nullptr;
        break;
      }
      case OP_iassign: {
        bool incWithLhs = stmt->NeedIncref();
        if (!incWithLhs && !stmt->NeedDecref()) {
          break;
        }
        IassignMeStmt *iassign = static_cast<IassignMeStmt *>(stmt);
        IvarMeExpr *lhs = iassign->lhsVar;
        MIRType *basetype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(lhs->tyIdx);
        ASSERT(dynamic_cast<MIRPtrType *>(basetype), "unexpected type");
        MIRType *ptype = static_cast<MIRPtrType *>(basetype)->GetPointedType();
        MIRStructType *structType = dynamic_cast<MIRStructType *>(ptype);
        // skip RC operation if the field is unowned
        if (structType && IsUnownedRefField(structType, lhs->fieldID)) {
          break;
        }
        MeExpr *rhs = stmt->GetRhs();
        bool isReferent = false;
        if (incWithLhs && HandleSpecialRHS(stmt)) {
          incWithLhs = false;
          rhs = stmt->GetRhs();
        }
        // align with order in rcinsertion, otherwise missing weak volatile
        if (lhs->IsRCWeak()) {
          bool isVolatile = lhs->IsVolatile();
          MeStmt *writeRefCall =
            CreateIntrinsicWithThreeArg(isVolatile ? PrepareVolatileCall(stmt, kWriteVolWeak) : kWriteWeak, stmt,
                                        lhs->base->GetAddrExprBase(), irMap->CreateAddrofMeExpr(lhs), rhs);
          if (isReferent && isVolatile) { // redo storeload due to referent handling
            MeStmt *next = stmt->next;
            if (next) {
              next = next->next;
            }
            if (next && next->op == OP_membarstoreload) {
              stmt->bb->RemoveMeStmt(next);
            }
          }
          bb->ReplaceMeStmt(stmt, writeRefCall);
        } else if (lhs->IsVolatile()) {
          PrepareVolatileCall(stmt); // default to write
          MeStmt *writeRefCall =
            CreateIntrinsicWithThreeArg(incWithLhs ? kWriteVol : kWriteVolNoInc, stmt, lhs->base->GetAddrExprBase(),
                                   irMap->CreateAddrofMeExpr(lhs), rhs);
          if (isReferent) { // optimize storeload again due to referent handling
            MeStmt *next = stmt->next;
            if (next) {
              next = next->next;
            }
            if (next && next->op == OP_membarstoreload) {
              stmt->bb->RemoveMeStmt(next);
            }
          }
          bb->ReplaceMeStmt(stmt, writeRefCall);
        } else {
          // note that we are generating kWriteNoRc so write_barrier is supported,
          // otherwise iassign would be enough.
          bool needDecref = stmt->NeedDecref();
          MeStmt *writeRefCall = CreateIntrinsicWithThreeArg(
            incWithLhs ? (needDecref ? kWrite : kWriteNoDec) : (needDecref ? kWriteNoInc : kWriteNoRc), stmt,
            lhs->base->GetAddrExprBase(), irMap->CreateAddrofMeExpr(lhs), rhs);
          bb->ReplaceMeStmt(stmt, writeRefCall);
        }
        pendingDec = nullptr;
        break;
      }
      case OP_interfacecallassigned:
      case OP_interfaceicallassigned:
      case OP_virtualcallassigned:
      case OP_virtualicallassigned:
      case OP_superclasscallassigned:
      case OP_polymorphiccallassigned:
      case OP_customcallassigned:
      case OP_callassigned: {
        CallMeStmt *call = static_cast<CallMeStmt *>(stmt);
        MIRFunction *callee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(call->puIdx);
        MIRType *rettype = callee->GetReturnType();
        CHECK_FATAL(rettype != nullptr, "rettype is nullptr in RCLowering::RCLower");
        if (rettype->GetPrimType() != PTY_ref) {
          break;
        }
        HandleReturnDecRef(stmt, pendingDec, bb);
        break;
      }
      case OP_icallassigned: {
        IcallMeStmt *icallassigned = static_cast<IcallMeStmt *>(stmt);
        const TyIdx retTyidxIdx = icallassigned->retTyIdx;
        CHECK_FATAL(retTyidxIdx != TyIdx(0), "retTyIdx is invalid in RCLowering::RCLower");
        MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(retTyidxIdx);
        CHECK_FATAL(rettype != nullptr, "rettype is nullptr in RCLowering::RCLower");
        if (rettype->GetPrimType() != PTY_ref) {
          break;
        }
        HandleReturnDecRef(stmt, pendingDec, bb);
        break;
      }
      case OP_intrinsiccallassigned: {
        IntrinsiccallMeStmt *intrinsicCall = static_cast<IntrinsiccallMeStmt *>(stmt);
        /**
         * Method GetReturnType in IntrinDesc cannot be used here
         * Use GetReturnType in IntrinsiccallMeStmt instead
         */
        if (intrinsicCall->GetReturnType() != PTY_ref) {
          break;
        }
        HandleReturnDecRef(stmt, pendingDec, bb);
        break;
      }
      case OP_intrinsiccallwithtypeassigned: {
        IntrinsiccallMeStmt *intrinsicCall = static_cast<IntrinsiccallMeStmt *>(stmt);
        /**
         * Method GetReturnType in IntrinDesc cannot be used here
         * Use GetReturnType in IntrinsiccallMeStmt instead
         */
        if (intrinsicCall->GetReturnType() != PTY_ref) {
          break;
        }
        HandleReturnDecRef(stmt, pendingDec, bb);
        break;
      }
      case OP_return:
        rets.push_back(stmt);
        break;
      default:
        CHECK_FATAL(!kOpcodeInfo.IsCallAssigned(stmt->op), "missing callassign case");
        break;
    }
  }
}

IntrinsiccallMeStmt *FindCleanupIntrinsic(MeStmt *ret) {
  MeStmt *stmt = ret;
  CHECK_FATAL(ret->bb->meStmtList.last == ret, "return is not last stmt in bb");
  do {
    stmt = stmt->prev;
    if (stmt && stmt->op == OP_intrinsiccall) {
      IntrinsiccallMeStmt *intrinsicCall = static_cast<IntrinsiccallMeStmt *>(stmt);
      if (intrinsicCall->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS) {
        return intrinsicCall;
      }
    }
  } while (stmt && stmt != ret->bb->meStmtList.first);
  return nullptr;
}

// epre could have change dassign (iread) to regassign (iread) + dassign (regread)
// as a result, LoadRefField was not generated. This function fixes up the issue
// by checking rhs to see if it is regread and then try to
// propagate incref from current stmt back to regassign from iread which had no incref
void RCLowering::EpreFixup(BB *bb) {
  for (auto stmt : bb->meStmtList) {
    // remove decref as mpl2mpl will insert based on ref assign
    if (!stmt->NeedIncref()) {
      continue;
    }
    MeExpr *rhs = stmt->GetRhs();
    if (!rhs || rhs->op != OP_regread) {
      continue;
    }
    RegMeExpr *rhsvar = static_cast<RegMeExpr *>(rhs);
    if (rhsvar->defBy != kDefByStmt) {
      continue;
    }
    MeStmt *defStmt = rhsvar->def.defStmt;
    if (defStmt->bb != stmt->bb) {
      continue;  // we cannot fix up across bb
    }
    CHECK_FATAL(defStmt->GetRhs() != nullptr, "null ptr check");
    if (defStmt->NeedIncref() || defStmt->GetRhs()->op != OP_iread) {
      continue;
    }
    // pull incref from regread stmt to regassign
    defStmt->SetNeedIncref();
    stmt->SetNeedIncref(false);
  }  // next stmt
}

void RCLowering::PostBBLower() {
  // add localrefvar attr for  formals that are ever modified in this function
  MIRFunction *mirfunction = func->mirFunc;
  for (size_t i = (mirfunction->IsStatic() ? 0 : 1); i < mirfunction->formalDefVec.size(); ++i) {
    MIRSymbol *sym = mirfunction->formalDefVec[i].formalSym;
    if (sym && !sym->IgnoreRC()) {
      if (!globalWrite && assignedPtrSym.count(sym) == 0) {
        continue;
      }
      mirfunction->formalDefVec[i].formalAttrs.SetAttr(ATTR_localrefvar);
    }
  }

  // handle ret stmts
  bool returnRef = (mirfunction->GetReturnType()->primType == PTY_ref);
  bool updateCleanup = !tempLocalRefVars.empty();
  if (returnRef || updateCleanup) {
    for (auto stmt : rets) {
      RetMeStmt *ret = static_cast<RetMeStmt *>(stmt);
      IntrinsiccallMeStmt *cleanup = FindCleanupIntrinsic(ret);
      if (cleanup && updateCleanup) { // new localrefvar introduced in this phase
        for (auto tempVar: tempLocalRefVars) {
          cleanup->opnds.push_back(tempVar);
        }
      }
      if (ret->opnds.empty()) continue;
      MeExpr *retVal = ret->opnds[0];
      // handle nullptr return first
      if (retVal->meOp == kMeOpConst) {
        if (static_cast<ConstMeExpr *>(retVal)->IsZero()) {
          continue;
        }
      } else if (retVal->meOp == kMeOpVar) {
        VarMeExpr *retVar = static_cast<VarMeExpr *>(retVal);
        MIRSymbol *sym = retVar->ost->GetMIRSymbol();
        if (sym && sym->IgnoreRC()) {
          continue;
        }
        // if dread is local assigned var, no need inc because we will skip its dec
        // unless we are change it to regread in later optimization
        if (assignedPtrSym.count(sym) > 0 && sym->storageClass == kScAuto && !MeOption::regreadAtReturn) {
          continue;
        }
        if (sym && sym->IsGlobal() && !sym->IsFinal() && !sym->IsLiteralPtr()) {
          CHECK_FATAL(stmt, "null ptr check");
          if (MeOption::strictNaiverc) {
            MeStmt *tmp = irMap->CreateAssignMeStmt(GetRegTemp(PTY_ptr), retVal, stmt->bb);
            stmt->bb->InsertMeStmtBefore(stmt, tmp);
            AssignMeStmt *regassstmt = static_cast<AssignMeStmt *>(tmp);
            MeStmt *inccall = CreateIntrinsicWithOneArg(kIncRef, regassstmt, regassstmt->lhs, true);
            stmt->bb->InsertMeStmtBefore(stmt, inccall);
            ret->opnds[0] = lastTempReg;
          } else {
            stmt->bb->InsertMeStmtBefore(
              stmt, CreateIntrinsicWithOneArg(kLoadStatic, stmt, irMap->CreateAddrofMeExpr(retVar), true));
            ret->opnds[0] = lastTempReg;
          }
        } else if (assignedPtrSym.count(sym) > 0 && sym->storageClass == kScAuto && sym->GetAttr(ATTR_localrefvar)) {
          // must be regreadAtReturn
          // checking localrefvar because some objects are meta
          if (cleanup == nullptr) {
            stmt->bb->InsertMeStmtBefore(stmt, CreateIntrinsicWithOneArg(kIncRef, stmt, retVar));
          } else {
            // remove argument from intrinsiccall MPL_CLEANUP_LOCALREFVARS (dread ref %Reg1_R5678, ...
            MapleVector<MeExpr *> *opnds = &cleanup->opnds;
            for (auto iter = opnds->begin(); iter != opnds->end(); iter++) {
              if (*iter == retVal) {
                opnds->erase(iter);
                opnds->push_back(retVal);  // pin it to end of vector
                cleanup->intrinsic = INTRN_MPL_CLEANUP_LOCALREFVARS_SKIP;
                break;
              }
            }
          }
        } else if (!(MeOption::placementrc && sym && sym->storageClass == kScFormal && assignedPtrSym.count(sym) == 1)) {
          // if returning formal, incref unless placementrc is used and formal is NOT reassigned
          if (assignedPtrSym.count(sym) == 0) {
            MeStmt *increfstmt = CreateIntrinsicWithOneArg(kIncRef, stmt, retVar, true);
            ret->opnds[0] = lastTempReg;
            stmt->bb->InsertMeStmtBefore(stmt, increfstmt);
          } else { // since there must have been a cleanup decref inserted, delete the decref instead
            MeStmt *curstmt = ret->prev;
            bool found = false;
            while (curstmt != nullptr) {
              if (curstmt->op == OP_intrinsiccall) {
                IntrinsiccallMeStmt *intrn = static_cast<IntrinsiccallMeStmt *>(curstmt);
                if (intrn->intrinsic == INTRN_MCCDecRef && intrn->opnds[0] == retVar) {
                  found = true;
                  break;
                }
              }
              curstmt = curstmt->prev;
            }
            if (found) {
              stmt->bb->RemoveMeStmt(curstmt);
            }
            // if not found, must be because it was deleted by condbasedrc
          }
        }
      } else if (retVal->meOp == kMeOpIvar) {
        // insert calls
        IvarMeExpr *retIvar = static_cast<IvarMeExpr *>(retVal);
        if (retIvar->IsVolatile()) {
          MeStmt *loadcall = CreateIntrinsicWithTwoArg(PrepareVolatileCall(stmt, kLoadVol), stmt,
                                                       retIvar->base->GetAddrExprBase(),
                                                       irMap->CreateAddrofMeExpr(retIvar), true);
          stmt->bb->InsertMeStmtBefore(stmt, loadcall);
          ret->opnds[0] = lastTempReg;
        } else {
          if (MeOption::strictNaiverc) {
            MeStmt *tmp = irMap->CreateAssignMeStmt(GetRegTemp(PTY_ptr), retVal, stmt->bb);
            stmt->bb->InsertMeStmtBefore(stmt, tmp);
            AssignMeStmt *regassstmt = static_cast<AssignMeStmt *>(tmp);
            MeStmt *inccall = CreateIntrinsicWithOneArg(kIncRef, regassstmt, regassstmt->lhs, true);
            stmt->bb->InsertMeStmtBefore(stmt, inccall);
            ret->opnds[0] = lastTempReg;
          } else {
            MeStmt *loadcall = CreateIntrinsicWithTwoArg(kLoadRef, stmt, retIvar->base->GetAddrExprBase(),
                                                        irMap->CreateAddrofMeExpr(retIvar), true);
            stmt->bb->InsertMeStmtBefore(stmt, loadcall);
            ret->opnds[0] = lastTempReg;
          }
        }
      } else if (retVal->meOp == kMeOpReg) {
        // if the register with ref value is defined by callassigned or gcmalloc
        // return incref should have been delegated and not needed.
        RegMeExpr *retreg = static_cast<RegMeExpr *>(retVal);
        if (retreg->defBy == kDefByMustdef) {
          continue;
        }
        if (retreg->defBy == kDefByStmt && retreg->def.defStmt->op == OP_regassign) {
          MeExpr *rhs = retreg->def.defStmt->GetRhs();
          CHECK_FATAL(rhs != nullptr, "null ptr check");
          if (rhs->op == OP_gcmalloc || rhs->op == OP_gcmallocjarray) {
            continue;
          }
        }
        MeStmt *inccall = CreateIntrinsicWithOneArg(kIncRef, stmt, retVal, true);
        ret->opnds[0] = lastTempReg;
        stmt->bb->InsertMeStmtBefore(stmt, inccall);
      } else {
        // incref by default
        MeStmt *temp = irMap->CreateAssignMeStmt(GetRegTemp(PTY_ptr), retVal, stmt->bb);
        stmt->bb->InsertMeStmtBefore(stmt, temp);
        stmt->bb->InsertMeStmtBefore(stmt, CreateIntrinsicWithOneArg(kIncRef, stmt, lastTempReg));
        ret->opnds[0] = lastTempReg;
      }
    }
  } else {
    // any return value expression containing ivar has to be saved in a
    // temp with the temp being returned
    for (auto stmt : rets) {
      RetMeStmt *ret = static_cast<RetMeStmt *>(stmt);
      if (ret->opnds.size() == 0) {
        continue;
      }
      MeExpr *retVal = ret->opnds[0];
      if (!retVal->HasIvar()) {
        continue;
      }
      RegMeExpr *curTemp = GetRegTemp(retVal->primType);
      MeStmt *regass = irMap->CreateAssignMeStmt(curTemp, retVal, ret->bb);
      ret->bb->InsertMeStmtBefore(ret, regass);
      ret->opnds[0] = curTemp;
    }
  }
  // compact RC
  for (BB *bb : func->bbVec) {
    if (bb) {
      CompactRC(bb);
    }
  }
}

// iterate over stmts and find back-to-back incref/decref
// and merge them into one intrinsic if needed
void RCLowering::CompactRC(BB *bb) {
  MeStmt *pendingDec = nullptr;
  MeStmt *pendingInc = nullptr;
  for (auto stmt : bb->meStmtList) {
    if (stmt->op == OP_intrinsiccall) {
      IntrinsiccallMeStmt *callnode = static_cast<IntrinsiccallMeStmt *>(stmt);
      if (callnode->intrinsic == INTRN_MCCIncRef) {
        if (pendingDec) {
          if (callnode->opnds[0] == static_cast<IntrinsiccallMeStmt*>(pendingDec)->opnds[0]) {
            bb->RemoveMeStmt(stmt);
            bb->RemoveMeStmt(pendingDec);
          } else {
            bb->ReplaceMeStmt(stmt, CreateIntrinsicWithTwoArg(kIncDec, stmt, callnode->opnds[0],
                              static_cast<IntrinsiccallMeStmt*>(pendingDec)->opnds[0]));
            bb->RemoveMeStmt(pendingDec);
          }
          pendingDec = nullptr;
        } else {
          pendingInc = callnode;
        }
      } else if (callnode->intrinsic == INTRN_MCCDecRef) {
        if (pendingInc) {
          if (static_cast<IntrinsiccallMeStmt*>(pendingInc)->opnds[0] == callnode->opnds[0]) {
            bb->RemoveMeStmt(pendingInc);
            bb->RemoveMeStmt(stmt);
          } else {
            bb->ReplaceMeStmt(stmt, CreateIntrinsicWithTwoArg(kIncDec, stmt,
                static_cast<IntrinsiccallMeStmt*>(pendingInc)->opnds[0], callnode->opnds[0]));
            bb->RemoveMeStmt(pendingInc);
          }
          pendingInc = nullptr;
        } else {
          pendingDec = callnode;
        }
      } else {
        pendingInc = pendingDec = nullptr;
      }
    } else { // if not intrinsiccall, clean up both pendingInc and pendingDec
      if (pendingInc) {
        pendingInc = nullptr;
      }
      if (pendingDec) {
        pendingDec = nullptr;
      }
    }
  } // next stmt
}

OriginalSt *RCLowering::RetrieveOst(const char *name, bool isLocalrefvar) const {
  MIRSymbol *backupSym = mirModule->mirBuilder->GetOrCreateLocalDecl(name,
                         GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr)));  // use PTY_ptr for temp
  backupSym->isTmp = 1;
  if (isLocalrefvar) {
    backupSym->SetLocalRefVar();
  }
  OriginalSt *ost = ssaTab->FindOrCreateSymbolOriginalSt(backupSym, func->mirFunc->puIdx, 0);
  return ost;
}

// function for creating short-lived temp
VarMeExpr *RCLowering::GetTemp(const char *tempName, bool isLocalrefvar) {
  OriginalSt *ost = RetrieveOst(tempName, isLocalrefvar);
  if (ost->zeroVersionIndex == 0) {
    ost->zeroVersionIndex = irMap->verst2MeExprTable.size();
    irMap->verst2MeExprTable.push_back(nullptr);
    ost->versionsIndex.push_back(ost->zeroVersionIndex);
  }
  VarMeExpr *varmeexpr =
    irMap->New<VarMeExpr>(irMap->exprID++, ost, irMap->verst2MeExprTable.size(), PTY_ptr);
  varmeexpr->ost->fieldID = 0;
  irMap->verst2MeExprTable.push_back(varmeexpr);
  ost->versionsIndex.push_back(varmeexpr->vstIdx);
  if (isLocalrefvar) {
    tempLocalRefVars.insert(varmeexpr);
  }
  return varmeexpr;
}

// create a new temp and return MeExpr* for it
VarMeExpr *RCLowering::GetMeExprForNewTemp(bool isLocalrefvar) {
  string tempstr = string("__RCTemp__").append(to_string(++tempCount));
  return GetTemp(tempstr.c_str(), isLocalrefvar);
}

AnalysisResult *MeDoRCLowering::Run(MeFunction *func, MeFuncResultMgr *m, ModuleResultMgr *mrm) {
  KlassHierarchy *kh = static_cast<KlassHierarchy *>(mrm->GetAnalysisResult(MoPhase_CHA, &func->mirModule));
  RCLowering rcLowering(func, kh);
  func->mirModule.hints |= kRcLowered;

  MIRFunction *mirfunction = func->mirFunc;
  // do we need to set CurFunction??
  CHECK_FATAL(mirfunction->module->CurFunction() == mirfunction, "unexpected CurFunction");
  string funcname = mirfunction->GetName();
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "Handling function " << funcname << std::endl;
  }

  // preparation steps before going through basicblocks
  size_t bitsSize = mirfunction->symTab->GetSymbolTableSize();
  for (size_t i = 0; i < bitsSize; ++i) {
    MIRSymbol *sym = mirfunction->symTab->GetSymbolFromStIdx(i);
    if (sym && sym->storageClass == kScAuto && !sym->IgnoreRC()) {
      sym->SetLocalRefVar();
    }
  }

  for (BB *bb : func->bbVec) {
    if (bb == nullptr || bb == func->commonEntryBB || bb == func->commonExitBB) {
      continue;
    }
    rcLowering.EpreFixup(bb);
    rcLowering.RCLower(bb);
  }

  // handle all the extra RC work
  rcLowering.PostBBLower();

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============== After RC LOWERING =============" << endl;
    func->irMap->Dump();
  }

  return nullptr;
}

}  // namespace maple
