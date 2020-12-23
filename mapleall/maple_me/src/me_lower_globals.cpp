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

#include "me_lower_globals.h"
#include "name_mangler.h"

// the dassign/dread of global or static variables are lowered into
// iassign/iread to expose the addrof.  The inserted addrof is always of fieldID
// 0 to reduce the number of registers needed for storing addresses after the
// addrof's are allocated to pregs.  The fieldID's thus reside in the iassign's
// and iread's.
//
// By the same token, an addrof with non-zero field id is also lowered into
// iaddrof with same field id whose operand is addrof with 0 field id.
//
// call and callassigned are lowered into icall/icallassigned and addroffunc.
// The addroffunc is inserted as the extra first operand and the
// call/callassinged is renamed to icall/icallassigned.

namespace maple {

void LowerGlobals::LowerGlobalDreads(MeStmt *stmt, MeExpr *x) {
  switch (x->meOp) {
    case kMeOpIvar:
    case kMeOpOp:
    case kMeOpNary: {
      for (int32 i = 0; i < x->NumMeExprOpnds(); i++) {
        LowerGlobalDreads(stmt, x->GetOpnd(i));
      }
      break;
    }
    case kMeOpVar: {
      VarMeExpr *thevar = static_cast<VarMeExpr *>(x);
      OriginalSt *ost = thevar->ost;
      if (ost->isLocal) {
        break;
      }
      // lower to ivar to expose addrof
      OriginalSt *baseost = ost;
      if (ost->fieldID != 0)
        baseost = ssaTab->FindOrCreateSymbolOriginalSt(ost->GetMIRSymbol(), func->mirFunc->puIdx, (FieldID)0);

      AddrofMeExpr addrofmeexpr(-1, PTY_ptr, baseost->index);
      AddrofMeExpr *addrof = static_cast<AddrofMeExpr *>(irMap->HashMeExpr(&addrofmeexpr));

      MIRPtrType pointtype(baseost->tyIdx, PTY_ptr);
      TyIdx addrtyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&pointtype);

      IvarMeExpr ivarmeexpr(-1, thevar->primType, addrtyidx, thevar->ost->fieldID);
      ivarmeexpr.base = addrof;
      ivarmeexpr.mu = thevar;
      IvarMeExpr *theivar = static_cast<IvarMeExpr *>(irMap->HashMeExpr(&ivarmeexpr));

      irMap->ReplaceMeExprStmt(stmt, thevar, theivar);
      break;
    }
    case kMeOpAddrof: {
      AddrofMeExpr *theaddrof = static_cast<AddrofMeExpr *>(x);
      OriginalSt *ost = ssaTab->GetOriginalStFromid(theaddrof->ostIdx);
      if (ost->fieldID == 0) {
        break;
      }
      if (ost->isLocal) {
        break;
      }
      // lower to iaddrof to expose addrof with 0 fieldID
      OriginalSt *baseost = ssaTab->FindOrCreateSymbolOriginalSt(ost->GetMIRSymbol(), func->mirFunc->puIdx,
                                                                 (FieldID)0);

      AddrofMeExpr addrofmeexpr(-1, PTY_ptr, baseost->index);
      AddrofMeExpr *newaddrof = static_cast<AddrofMeExpr *>(irMap->HashMeExpr(&addrofmeexpr));

      MIRPtrType pointtype(baseost->tyIdx, PTY_ptr);
      TyIdx addrtyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&pointtype);

      OpMeExpr iaddrof(-1, OP_iaddrof, PTY_ptr, 1);
      iaddrof.tyIdx = addrtyidx;
      iaddrof.fieldID = ost->fieldID;
      iaddrof.SetOpnd(newaddrof, 0);
      OpMeExpr *theiaddrof = static_cast<OpMeExpr *>(irMap->HashMeExpr(&iaddrof));

      irMap->ReplaceMeExprStmt(stmt, theaddrof, theiaddrof);
      break;
    }
    default:
      break;
  }
  return;
}

void LowerGlobals::Run() {
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->meStmtList) {
      if (stmt->op == OP_dassign) { // skip identify assignment because it will be deleted
        DassignMeStmt *dass = static_cast<DassignMeStmt *>(stmt);
        OriginalSt *ost = dass->lhs->ost;
        if (dass->rhs->meOp == kMeOpVar && static_cast<VarMeExpr *>(dass->rhs)->ost == ost) {
          continue;
        }
      }

      for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
        LowerGlobalDreads(stmt, stmt->GetMeStmtOpnd(i));
      }

      if (stmt->op == OP_dassign) {
        DassignMeStmt *dass = static_cast<DassignMeStmt *>(stmt);
        OriginalSt *ost = dass->lhs->ost;
        if (ost->isLocal) {
          continue;
        }
        if (dass->rhs->meOp == dass->lhs->meOp && dass->lhs->ost == static_cast<ScalarMeExpr *>(dass->rhs)->ost) {
          continue; // identity assignment converted from phi
        }
        // lower to iassign to expose addrof
        OriginalSt *baseost = ost;
        if (ost->fieldID != 0)
          baseost = ssaTab->FindOrCreateSymbolOriginalSt(ost->GetMIRSymbol(), func->mirFunc->puIdx, (FieldID)0);

        AddrofMeExpr addrofmeexpr(-1, PTY_ptr, baseost->index);
        AddrofMeExpr *addrof = static_cast<AddrofMeExpr *>(irMap->HashMeExpr(&addrofmeexpr));

        MIRPtrType pointtype(baseost->tyIdx, PTY_ptr);
        TyIdx addrtyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&pointtype);
        VarMeExpr *varLhs = static_cast<VarMeExpr *>(dass->GetVarLhs());
        IvarMeExpr ivarmeexpr(-1, varLhs->primType, addrtyidx, varLhs->ost->fieldID);
        ivarmeexpr.base = addrof;
        ivarmeexpr.mu = varLhs;
        IvarMeExpr *lhsivar = static_cast<IvarMeExpr *>(irMap->HashMeExpr(&ivarmeexpr));

        IassignMeStmt *iass = irMap->NewInPool<IassignMeStmt>(addrtyidx, lhsivar, dass->rhs, &dass->chiList);
        iass->bb = bb;
        iass->srcPos = dass->srcPos;
        iass->isLive = true;

        bb->ReplaceMeStmt(stmt, iass);
      } else if (stmt->op == OP_call || stmt->op == OP_callassigned) {
        // don't do this if current function is in libcore-all
        if (!MeOption::optdirectcall) {
          continue;
        }
        if (GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangObjectStr)) !=
            nullptr) {
          continue;
        }
        CallMeStmt *callstmt = static_cast<CallMeStmt *>(stmt);
        MIRFunction *callee = GlobalTables::GetFunctionTable().funcTable.at(callstmt->puIdx);
        if (!callee->IsJava() && callee->GetBaseClassName().empty()) {
          continue;  // not a java callee
        }

        AddroffuncMeExpr addroffuncmeexpr(-1, callee->puIdx);
        addroffuncmeexpr.op = OP_addroffunc;
        addroffuncmeexpr.primType = PTY_ptr;
        addroffuncmeexpr.numOpnds = 0;
        AddroffuncMeExpr *addroffunc = static_cast<AddroffuncMeExpr *>(irMap->HashMeExpr(&addroffuncmeexpr));

        MapleVector<MeExpr *>::iterator insertpos = callstmt->opnds.begin();
        callstmt->opnds.insert(insertpos, addroffunc);
        if (stmt->op == OP_call) {
          IcallMeStmt *icallmestmt = irMap->NewInPool<IcallMeStmt>(OP_icall);
          icallmestmt->isLive = callstmt->isLive;
          icallmestmt->srcPos = callstmt->srcPos;
          for (MeExpr *o : callstmt->opnds) {
            icallmestmt->opnds.push_back(o);
          }
          icallmestmt->muList.insert(callstmt->muList.begin(), callstmt->muList.end());
          icallmestmt->chiList.insert(callstmt->chiList.begin(), callstmt->chiList.end());
          icallmestmt->retTyIdx = callee->GetReturnTyIdx();
          bb->ReplaceMeStmt(stmt, icallmestmt);
        } else {
          IcallMeStmt *icallassignedmestmt = irMap->NewInPool<IcallMeStmt>(OP_icallassigned);
          icallassignedmestmt->isLive = callstmt->isLive;
          icallassignedmestmt->srcPos = callstmt->srcPos;
          for (MeExpr *o : callstmt->opnds) {
            icallassignedmestmt->opnds.push_back(o);
          }
          icallassignedmestmt->muList.insert(callstmt->muList.begin(), callstmt->muList.end());
          icallassignedmestmt->chiList.insert(callstmt->chiList.begin(), callstmt->chiList.end());
          icallassignedmestmt->retTyIdx = callee->GetReturnTyIdx();
          icallassignedmestmt->SetNeedDecref(callstmt->NeedDecref());
          icallassignedmestmt->SetNeedIncref(callstmt->NeedIncref());
          icallassignedmestmt->mustDefList.assign(callstmt->mustDefList.begin(), callstmt->mustDefList.end());
          bb->ReplaceMeStmt(stmt, icallassignedmestmt);
        }
      }
    }
  }
}

}  // namespace maple
