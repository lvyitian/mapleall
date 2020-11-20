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

#include "me_delegate_rc.h"
#include "mir_builder.h"
#include "me_hdse.h"

// This phase finds local ref pointer variables that are delegated and thus can
// have their RC omitted.  This optimization is done on a per-SSA version basis.
// As a result, an overall criterion is that the SSA version must not have
// appeared as phi operand;
//
// There are two forms of delegation:
//
// Form A. The following conditions have to be satisfied for an SSA version to be
// delegated:
// (1) In the BB where it is defined, there is a last use of it as the assigned
//     value in an assignment statement, which is either iassign or
//     dassign-to-global, and the assignment statement is marked needIncref.
// (2) Between its definition and last use, there is no statement that may
//     potentially raise exception.
// For each such case, the optimization is as follows:
// (a) The SSA version is renamed to a new and unique preg with PTY_ptr type;
//     this has the effect of eliminating its store and loads and its
//     decref-before-return;
// (b) The needIncref flag in the respective assignment statement is cleared.
// The reference count correctness is maintained because the removed decref and
// incref are for the same offect.
//
// Form B. An SSA version is determined to not need incref at its def and not
// need decref when it becomes dead.  In this case, the SSA version is renamed
// to a new and unique preg with PTY_ptr type as in (a) above.
//
// There are different ways to establish that an SSA version does not any RC:
// B1: The SSA version is never dereferenced and never copied to another variable
//     or passed on as a parameter.
// B2: The RHS of its definition is iread of a final field with this as base or
//     dread of a static final field.
// B3: Within the SSA version's live range, there is no operation that can result
//     in decref of any object. (TODO)
// B4: The RHS of its definition is pointer to string literal.

// following intrinsics can throw exception
static const std::set<MIRIntrinsicID> kCanThrowIntrinsicsList{
  INTRN_MPL_CLINIT_CHECK,      INTRN_MPL_BOUNDARY_CHECK,    INTRN_JAVA_CLINIT_CHECK,
  INTRN_JAVA_CLINIT_CHECK_SGET, INTRN_JAVA_CLINIT_CHECK_SPUT, INTRN_JAVA_CLINIT_CHECK_NEW,
  INTRN_JAVA_CHECK_CAST,        INTRN_JAVA_THROW_ARITHMETIC,  INTRN_JAVA_THROW_CLASSCAST,
};

namespace maple {

void DelegateRC::SetCantDelegate(MapleMap<OStIdx, MePhiNode *> &mevarphilist) {
  MapleMap<OStIdx, MePhiNode *>::iterator it = mevarphilist.begin();
  for (; it != mevarphilist.end(); it++) {
    OriginalSt *ost = ssaTab->GetOriginalStFromid(it->first);
    if (!ost->IsSymbol() || ost->indirectLev != 0) {
      continue;
    }
    MePhiNode *mephi = it->second;
    if (!mephi->isLive) {
      continue;
    }
    for (ScalarMeExpr *phiopnd : mephi->opnds) {
      verst_cantdelegate[phiopnd->vstIdx] = true;
    }
  }
}

void DelegateRC::CollectUsesInfo(MeExpr *x) {
  MeExprOp meOp = x->meOp;
  switch (meOp) {
    case kMeOpOp: {
      OpMeExpr *meopexpr = static_cast<OpMeExpr *>(x);
      for (uint32 i = 0; i < 3; i++) {
        MeExpr *opnd = meopexpr->GetOpnd(i);
        if (opnd) {
          CollectUsesInfo(opnd);
        }
      }
      if (x->op == OP_iaddrof) {
        if (meopexpr->GetOpnd(0)->meOp == kMeOpVar) {
          VarMeExpr *basevar = static_cast<VarMeExpr *>(meopexpr->GetOpnd(0));
          verst_derefedcopied[basevar->vstIdx] = true;
        }
      } else if (x->op == OP_add) {
        if (meopexpr->GetOpnd(0)->meOp == kMeOpVar) {
          VarMeExpr *var = static_cast<VarMeExpr *>(meopexpr->GetOpnd(0));
          verst_derefedcopied[var->vstIdx] = true;
        }
        if (meopexpr->GetOpnd(1)->meOp == kMeOpVar) {
          VarMeExpr *var = static_cast<VarMeExpr *>(meopexpr->GetOpnd(1));
          verst_derefedcopied[var->vstIdx] = true;
        }
      } else if (x->op == OP_cvt) {
        // in some cases, we have cvt from int to ref
        // dassign %Reg0_I 0 (cvt i32 ref (dread ref %Reg0_R4095))
        // cvt ref i32 (dread i32 %Reg0_I)
        if (x->primType == PTY_ref && meopexpr->GetOpnd(0)->meOp == kMeOpVar) {
          VarMeExpr *basevar = static_cast<VarMeExpr *>(meopexpr->GetOpnd(0));
          verst_derefedcopied[basevar->vstIdx] = true;
          // find the def of basevar
          if (basevar->defBy == kDefByStmt) {
            MeStmt *defStmt = basevar->def.defStmt;
            if (defStmt->op == OP_dassign) {
              MeExpr *rhs = static_cast<DassignMeStmt *>(defStmt)->rhs;
              if (rhs->op == OP_cvt) {
                MeExpr *expr = static_cast<OpMeExpr *>(rhs)->GetOpnd(0);
                if (expr->meOp == kMeOpVar) {
                  verst_derefedcopied[static_cast<VarMeExpr *>(expr)->vstIdx] = true;
                }
              }
            }
          } else if (basevar->defBy == kDefByPhi) {
            MePhiNode *defPhi = basevar->def.defPhi;
            for (ScalarMeExpr *phiopnd : defPhi->opnds) {
              if (phiopnd->defBy == kDefByStmt) {
                MeStmt *defStmt = phiopnd->def.defStmt;
                if (defStmt->op == OP_dassign) {
                  MeExpr *rhs = static_cast<DassignMeStmt *>(defStmt)->rhs;
                  if (rhs->op == OP_cvt) {
                    MeExpr *expr = static_cast<OpMeExpr *>(rhs)->GetOpnd(0);
                    if (expr->meOp == kMeOpVar) {
                      verst_derefedcopied[static_cast<VarMeExpr *>(expr)->vstIdx] = true;
                    }
                  }
                }
              }
            }  // next phi opnd
          }    // end of phi
        }
      }  // end of cvt special handling
      if ((meopexpr->op == OP_eq || meopexpr->op == OP_ne) && meopexpr->opndType == PTY_ref) {
        VarMeExpr *opnd0 = dynamic_cast<VarMeExpr *>(meopexpr->GetOpnd(0));
        if (opnd0 == nullptr) {
          break;
        }
        VarMeExpr *opnd1 = dynamic_cast<VarMeExpr *>(meopexpr->GetOpnd(1));
        if (opnd1 == nullptr) {
          break;
        }
        BB *opnd0defbb = opnd0->DefByBB();
        if (opnd0defbb == nullptr) {
          break;
        }
        BB *opnd1defbb = opnd1->DefByBB();
        if (opnd1defbb == nullptr) {
          break;
        }
        if (opnd0defbb == opnd1defbb) {
          verst_cantdecrefearly[opnd0->vstIdx] = true;
          verst_cantdecrefearly[opnd1->vstIdx] = true;
        } else if (dominance->Dominate(opnd0defbb, opnd1defbb)) {
          verst_cantdecrefearly[opnd0->vstIdx] = true;
        } else {
          verst_cantdecrefearly[opnd1->vstIdx] = true;
        }
      }
      break;
    }
    case kMeOpNary: {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(x);
      for (size_t i = 0; i < narymeexpr->numOpnds; i++) {
        CollectUsesInfo(narymeexpr->GetOpnd(i));
      }
      if (x->op == OP_array) {
        if (narymeexpr->GetOpnd(0)->meOp == kMeOpVar) {
          VarMeExpr *basevar = static_cast<VarMeExpr *>(narymeexpr->GetOpnd(0));
          verst_derefedcopied[basevar->vstIdx] = true;
        }
      } else if (x->op == OP_intrinsicop) {
        if (narymeexpr->intrinsic == INTRN_JAVA_ARRAY_LENGTH) {
          if (narymeexpr->GetOpnd(0)->meOp == kMeOpVar) {
            VarMeExpr *basevar = static_cast<VarMeExpr *>(narymeexpr->GetOpnd(0));
            verst_derefedcopied[basevar->vstIdx] = true;
          }
        }
      } else if (x->op == OP_intrinsicopwithtype) {
        if (narymeexpr->intrinsic == INTRN_JAVA_INSTANCE_OF) {
          if (narymeexpr->GetOpnd(0)->meOp == kMeOpVar) {
            VarMeExpr *basevar = static_cast<VarMeExpr *>(narymeexpr->GetOpnd(0));
            verst_derefedcopied[basevar->vstIdx] = true;
          }
        }
      }
      break;
    }
    case kMeOpIvar: {
      IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(x);
      CollectUsesInfo(ivarmeexpr->base);
      if (ivarmeexpr->base->meOp == kMeOpVar) {
        VarMeExpr *basevar = static_cast<VarMeExpr *>(ivarmeexpr->base);
        verst_derefedcopied[basevar->vstIdx] = true;
      }
      break;
    }
    case kMeOpVar: {
      VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(x);
      ASSERT(varmeexpr->vstIdx < verst_usecounts.size(), "CollectUsesInfo: vstIdx out of bounds");
      verst_usecounts[varmeexpr->vstIdx]++;
      break;
    }
    default:
      break;
  }
  return;
}

// traverse expression x; at each occurrence of rhsvar in x, decrement
// remaining_uses
void DelegateRC::FindAndDecrUseCount(VarMeExpr *rhsvar, MeExpr *x, int32 &remainingUses) {
  CHECK_FATAL(x != nullptr, "null ptr check");
  MeExprOp meOp = x->meOp;
  switch (meOp) {
    case kMeOpOp: {
      OpMeExpr *meopexpr = static_cast<OpMeExpr *>(x);
      for (uint32 i = 0; i < 3; i++) {
        MeExpr *opnd = meopexpr->GetOpnd(i);
        if (opnd) {
          FindAndDecrUseCount(rhsvar, opnd, remainingUses);
        }
      }
      break;
    }
    case kMeOpNary: {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(x);
      for (size_t i = 0; i < narymeexpr->numOpnds; i++) {
        FindAndDecrUseCount(rhsvar, narymeexpr->GetOpnd(i), remainingUses);
      }
      break;
    }
    case kMeOpIvar: {
      IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(x);
      FindAndDecrUseCount(rhsvar, ivarmeexpr->base, remainingUses);
      break;
    }
    case kMeOpVar: {
      if (static_cast<VarMeExpr *>(x) == rhsvar) {
        remainingUses--;
      }
      break;
    }
    default:
      break;
  }
  return;
}

// Traverse backwards from fromstmt to tostmt to see if the single use of rhsvar
// in fromstmt is the last use of rhsvar; tostmt is the statement that defines
// rhsvar, so it can be assumed that tostmt does not contain any use; this check
// make use of verst_usecounts in its determination.  In addition, if it comes
// across any stmt that can raise exception, also return false.
bool DelegateRC::ContainAllTheUses(VarMeExpr *rhsvar, MeStmt *fromstmt, const MeStmt *tostmt) {
  int32 remainingUses = static_cast<uint32>(verst_usecounts[rhsvar->vstIdx]) - 1;
  for (MeStmt *cur = fromstmt->prev; cur != tostmt; cur = cur->prev) {
    if (cur->op == OP_decref &&  // do not count decref operands
        static_cast<UnaryMeStmt *>(cur)->opnd->meOp == kMeOpVar) {
      continue;
    }
    IntrinsiccallMeStmt *intrn = dynamic_cast<IntrinsiccallMeStmt *>(cur);
    CallMeStmt *callstmt = dynamic_cast<CallMeStmt *>(cur);
    // check if statement may throw exception
    if (callstmt != nullptr) {
      MIRFunction *callee = GlobalTables::GetFunctionTable().funcTable[callstmt->puIdx];
      bool maythrowexception = true;
      if (callee->GetAttrs().GetAttr(FUNCATTR_nothrow_exception)) {
        maythrowexception = false;
      } else if (!MeOption::ignoreIPA)
        maythrowexception = !GlobalTables::GetSideEffectTable().IsNoThrowException(callee->GetNameStridx());
      if (maythrowexception) {
        return false;
      }
    } else if (intrn != nullptr) {
      if (kCanThrowIntrinsicsList.find(intrn->intrinsic) != kCanThrowIntrinsicsList.end()) {
        return false;
      }
    } else if (cur->op == OP_maydassign) {
      return false;
    } else if (cur->op == OP_throw) {
      return false;
    } else if (cur->op == OP_dassign) {
      DassignMeStmt *dass = static_cast<DassignMeStmt *>(cur);
      if (dass->wasMayDassign) {
        return false;
      }
      if (dass->rhs->op == OP_gcmalloc || dass->rhs->op == OP_gcmallocjarray) {
        return false;
      }
    } else if (cur->op == OP_regassign) {
      MeExpr *rhs = cur->GetRhs();
      CHECK_FATAL(rhs != nullptr, "null ptr check");
      if (rhs->op == OP_gcmalloc || rhs->op == OP_gcmallocjarray) {
        return false;
      }
    }

    if (!intrn || (intrn->intrinsic != INTRN_MPL_CLEANUP_LOCALREFVARS)) {
      for (int32 i = 0; i < cur->NumMeStmtOpnds(); i++) {
        FindAndDecrUseCount(rhsvar, cur->GetMeStmtOpnd(i), remainingUses);
      }
    }
  }
  CHECK_FATAL(remainingUses >= 0, "ContainAllTheUses: inconsistent use count");
  return remainingUses == 0;
}

// return the RegMeExpr node to replace the original temp; nullptr if not successful
RegMeExpr *DelegateRC::RHSTempDelegated(MeExpr *rhs, MeStmt *usestmt) {
  if (rhs->meOp != kMeOpVar) {
    return nullptr;
  }
  VarMeExpr *rhsvar = static_cast<VarMeExpr *>(rhs);
  if (verst_cantdelegate[rhsvar->vstIdx]) {
    return nullptr;
  }
  if (refvar2reg_map.find(rhsvar) != refvar2reg_map.end()) {
    return nullptr;  // already delegated by another assignment
  }
  OriginalSt *ost = rhsvar->ost;
  if (ost->isFormal) {
    return nullptr;
  }
  if (ost->GetMIRSymbol()->stIdx.IsGlobal()) {
    return nullptr;
  }
  if (rhsvar->defBy == kDefByMustdef) {
    MustDefMeNode *mustDef = rhsvar->def.defMustDef;
    ASSERT(mustDef->lhs == rhsvar, "DelegateRCTemp: inconsistent mustDef");
    MeStmt *callstmt = mustDef->base;
    if (callstmt->bb != usestmt->bb) {
      return nullptr;
    }
    if (!ContainAllTheUses(rhsvar, usestmt, callstmt)) {
      return nullptr;
    }
    // replace temp by a new preg
    rhsvar->defBy = kDefByNo;
    RegMeExpr *curreg = irMap->CreateRegMeExpr(rhsvar->GetType());
    refvar2reg_map[rhsvar] = curreg;  // record this replacement
    mustDef->UpdateLhs(curreg);
    return curreg;
  } else if (rhsvar->defBy == kDefByStmt) {
    MeStmt *defStmt = rhsvar->def.defStmt;
    if (defStmt->bb != usestmt->bb) {
      return nullptr;
    }
    if (!ContainAllTheUses(rhsvar, usestmt, defStmt)) {
      return nullptr;
    }

    VarMeExpr *thelhs = static_cast<VarMeExpr *>(defStmt->GetVarLhs());
    MeExpr *rhsexpr = defStmt->GetRhs();
    bool defstmtNeedIncref = defStmt->NeedIncref();
    CHECK_FATAL((defStmt->op == OP_dassign || defStmt->op == OP_maydassign),
           "DelegateRCTemp: unexpected stmt op for kDefByStmt");
    ASSERT(thelhs == rhsvar, "DelegateRCTemp: inconsistent def by dassign");

    // replace temp by a new preg
    rhsvar->defBy = kDefByNo;
    RegMeExpr *curreg = irMap->CreateRegMeExpr(rhsvar->GetType());
    refvar2reg_map[rhsvar] = curreg;  // record this replacement
    // create new regassign statement
    AssignMeStmt *regass = irMap->CreateAssignMeStmt(curreg, rhsexpr, defStmt->bb);
    curreg->SetDefByStmt(regass);
    regass->SetNeedIncref(defstmtNeedIncref);
    defStmt->bb->ReplaceMeStmt(defStmt, regass);
    return curreg;
  }
  return nullptr;
}

// process each assignment statement for Form A delegation
void DelegateRC::DelegateRCTemp(MeStmt *stmt) {
  switch (stmt->op) {
    case OP_iassign: {
      if (!stmt->NeedIncref()) {
        break;
      }
      IvarMeExpr *lhs = (static_cast<IassignMeStmt *>(stmt))->lhsVar;
      if (lhs->IsRCWeak()) {
        break;
      }
      if (lhs->base->op == OP_array) {
        // array may raise exception
        break;
      }
      MIRType *basetype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(lhs->tyIdx);
      ASSERT(basetype->typeKind == kTypePointer, "DelegateRCTemp: unexpected type");
      MIRType *pointedtype = static_cast<MIRPtrType *>(basetype)->GetPointedType();
      if (pointedtype->typeKind == kTypeClass &&
          static_cast<MIRClassType *>(pointedtype)->IsFieldRCUnownedRef(lhs->fieldID)) {
        break;
      }
      MeExpr *rhs = stmt->GetRhs();
      CHECK_FATAL(rhs != nullptr, "null ptr check");
      RegMeExpr *curreg = RHSTempDelegated(rhs, stmt);
      if (curreg) {
        rhs = curreg;
        stmt->SetNeedIncref(false);
      }
      break;
    }
    case OP_dassign: {
      DassignMeStmt *dass = static_cast<DassignMeStmt *>(stmt);
      if (!stmt->NeedIncref()) {
        break;
      }
      ScalarMeExpr *lhsVar = stmt->GetVarLhs();
      CHECK_FATAL(lhsVar != nullptr, "null ptr check");
      if (!lhsVar->ost->GetMIRSymbol()->stIdx.IsGlobal()) {
        break;
      }
      MeExpr *rhs = stmt->GetRhs();
      CHECK_FATAL(rhs != nullptr, "null ptr check");
      RegMeExpr *curreg = RHSTempDelegated(rhs, stmt);
      if (curreg) {
        rhs = curreg;
        stmt->SetNeedIncref(false);
      }
      break;
    }
    case OP_return: {
      string funcname = func->mirFunc->GetName();

      RetMeStmt *retmestmt = static_cast<RetMeStmt *>(stmt);
      if (!retmestmt->opnds.size()) {
        break;
      }

      MeExpr *ret = retmestmt->opnds[0];
      if (ret->primType != PTY_ref && ret->primType != PTY_ptr) {
        break;
      }

      if (ret->meOp == kMeOpVar) {
        VarMeExpr *val = static_cast<VarMeExpr *>(ret);
        if (val->defBy == kDefByMustdef) {
          MeStmt *defStmt = val->def.defMustDef->base;
          if (retmestmt->bb == defStmt->bb && ContainAllTheUses(val, stmt, defStmt)) {
            RegMeExpr *curreg = RHSTempDelegated(ret, stmt);
            if (curreg != nullptr) {
              retmestmt->opnds[0] = curreg;
            }
          }
        } else if (val->defBy == kDefByStmt) {
          MeStmt *defStmt = val->def.defStmt;

          MeExpr *rhs = defStmt->GetRhs();
          CHECK_FATAL(rhs != nullptr, "null ptr check");
          OriginalSt *ost = nullptr;
          if (rhs->meOp == kMeOpVar) {
            VarMeExpr *thevar = static_cast<VarMeExpr *>(rhs);
            ost = thevar->ost;
          }

          if (rhs->IsGcmalloc() || (rhs->meOp == kMeOpIvar && !static_cast<IvarMeExpr *>(rhs)->IsFinal()) ||
              (rhs->meOp == kMeOpVar && !ost->isFinal && ost->GetMIRSymbol()->IsGlobal()) ||
              (rhs->op == OP_regread && static_cast<RegMeExpr *>(rhs)->regIdx == -kSregThrownval)) {
            if (retmestmt->bb == defStmt->bb && ContainAllTheUses(val, stmt, defStmt)) {
              RegMeExpr *curreg = RHSTempDelegated(ret, stmt);
              if (curreg != nullptr) {
                retmestmt->opnds[0] = curreg;

                // Convert following cases:
                //   dassign %Reg_xxx  (iread ref xxx)
                //   return (dread ref %Reg_xxx)
                // To:
                //   // iread will be converted to LoadRefField
                //   regassign %1 (iread ref xxx)  [RC+]
                //   return (regread ref %1)
                if (rhs->meOp == kMeOpIvar ||
                    (rhs->meOp == kMeOpVar && !ost->isFinal && ost->GetMIRSymbol()->IsGlobal())) {
                  curreg->def.defStmt->SetNeedIncref(true);
                }
              }
            }
          }
        }
      }
      break;
    }
    default:;
  }
}

bool DelegateRC::FinalRefNoRC(MeExpr *x) {
  if (x->meOp == kMeOpVar) {
    VarMeExpr *thevar = static_cast<VarMeExpr *>(x);
    OriginalSt *ost = thevar->ost;
    return ost->isFinal && ost->GetMIRSymbol()->IsGlobal();
  } else if (x->meOp == kMeOpIvar) {
    if (func->mirFunc->IsConstructor() || func->mirFunc->IsStatic() || func->mirFunc->formalDefVec.empty()) {
      return false;
    }
    IvarMeExpr *ivar = static_cast<IvarMeExpr *>(x);
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ivar->tyIdx);
    MIRType *pointedty = static_cast<MIRPtrType *>(ty)->GetPointedType();
    if (pointedty->GetKind() == kTypeClass &&
        static_cast<MIRStructType *>(pointedty)->IsFieldFinal(ivar->fieldID) &&
        ivar->base->meOp == kMeOpVar) {
      VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(ivar->base);
      MIRSymbol *mirst = varmeexpr->ost->GetMIRSymbol();
      return mirst == func->mirFunc->formalDefVec[0].formalSym;
    }
  }
  return false;
}

// return true if it is OK to omit reference counting for the LHS variable; if
// returning true, onlyWithDecref specifies whether a decref needs inserted
bool DelegateRC::CanOmitRC4LHSVar(MeStmt *stmt, bool &onlyWithDecref) {
  onlyWithDecref = false;
  switch (stmt->op) {
    case OP_dassign:
    case OP_maydassign: {
      VarMeExpr *thelhs = static_cast<VarMeExpr *>(stmt->GetVarLhs());
      MeExpr *therhs = stmt->GetRhs();
      CHECK_FATAL(thelhs != nullptr && therhs != nullptr, "null ptr check");
      if (thelhs->primType != PTY_ref || thelhs->noDelegateRC) {
        return false;
      }
      OriginalSt *ost = thelhs->ost;
      if (!ost->isLocal || ost->isFormal) {
        return false;
      }
      if (ost->GetMIRSymbol()->instrumented == 1) {
        return false;
      }
      if (verst_cantdelegate[thelhs->vstIdx]) {
        return false;
      }
      // condition B2
      if (FinalRefNoRC(therhs)) {
        return true;
      }
      // condition B4
      if (therhs->meOp == kMeOpVar &&
          static_cast<VarMeExpr*>(therhs)->ost->GetMIRSymbol()->IsLiteralPtr()) {
        return true;
      }
      // condition B1
      if (!verst_derefedcopied[thelhs->vstIdx]) {
        onlyWithDecref = therhs->op == OP_gcmalloc || therhs->op == OP_gcmallocjarray ||
                         (therhs->op == OP_regread && static_cast<RegMeExpr *>(therhs)->regIdx == -kSregThrownval);
        if (onlyWithDecref && verst_cantdecrefearly[thelhs->vstIdx]) {
          onlyWithDecref = false;
          return false;
        }
        return true;
      }
      break;
    }
    default:
      if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
        MapleVector<MustDefMeNode> *mustdefList = stmt->GetMustDefList();
        CHECK_FATAL(mustdefList != nullptr, "null ptr check");
        if (mustdefList->empty()) {
          return false;
        }
        MeExpr *lhs = mustdefList->front().lhs;
        if (lhs->meOp != kMeOpVar) {
          return false;
        }
        VarMeExpr *thelhs = static_cast<VarMeExpr *>(lhs);
        if (thelhs->primType != PTY_ref) {
          return false;
        }
        OriginalSt *ost = thelhs->ost;
        if (!ost->isLocal || ost->isFormal) {
          return false;
        }
        if (verst_cantdelegate[thelhs->vstIdx]) {
          return false;
        }
        if (!verst_derefedcopied[thelhs->vstIdx]) {
          // condition B1
          if (!verst_cantdecrefearly[thelhs->vstIdx]) {
            onlyWithDecref = true;
            return true;
          }
        }
      }
      break;
  }
  return false;
}

void DelegateRC::DelegateHandleNoRCStmt(MeStmt *stmt, bool addDecref) {
  VarMeExpr *thelhs = nullptr;
  MeExpr *rhsexpr = nullptr;
  MapleVector<MustDefMeNode> *mustdefList = nullptr;
  // bool defstmt_need_incref;
  if (stmt->op == OP_dassign || stmt->op == OP_maydassign) {
    thelhs = static_cast<VarMeExpr *>(stmt->GetVarLhs());
    rhsexpr = stmt->GetRhs();
  } else if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
    if (!addDecref) {
      return;  // must be due to func->mirFunc->GetAttr(FUNCATTR_rclocalunowned)
    }
    mustdefList = stmt->GetMustDefList();
    CHECK_FATAL(mustdefList && !mustdefList->empty(), "empty container check");
    thelhs = static_cast<VarMeExpr *>(mustdefList->front().lhs);
  } else {
    return;
  }
  CHECK_FATAL(thelhs != nullptr, "null ptr check");
  if (thelhs->primType != PTY_ref) {
    return;
  }

  // replace temp by a new preg
  thelhs->defBy = kDefByNo;
  RegMeExpr *curreg = irMap->CreateRegMeExpr(thelhs->GetType());
  refvar2reg_map[thelhs] = curreg;  // record this replacement
  if (rhsexpr != nullptr) {
    // create new regassign statement
    AssignMeStmt *regass = irMap->CreateAssignMeStmt(curreg, rhsexpr, stmt->bb);
    curreg->SetDefByStmt(regass);
    stmt->bb->ReplaceMeStmt(stmt, regass);
    stmt = regass;  // for inserting defref after it below
  } else {
    // callassigned
    stmt->SetCallReturn(curreg);
  }
  if (addDecref) {
    IntrinsiccallMeStmt *rccall = irMap->NewInPool<IntrinsiccallMeStmt>(OP_intrinsiccall, INTRN_MCCDecRef);
    rccall->srcPos = stmt->srcPos;
    rccall->opnds.push_back(curreg);
    stmt->bb->InsertMeStmtAfter(stmt, rccall);
  }
  return;
}

void DelegateRC::RenameDelegatedRefVarUses(MeStmt *mestmt, MeExpr *meexpr) {
  switch (meexpr->meOp) {
    case kMeOpOp: {
      OpMeExpr *meopexpr = static_cast<OpMeExpr *>(meexpr);
      for (uint32 i = 0; i < 3; i++) {
        MeExpr *opnd = meopexpr->GetOpnd(i);
        if (opnd) {
          RenameDelegatedRefVarUses(mestmt, opnd);
        }
      }
      break;
    }
    case kMeOpNary: {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(meexpr);
      for (size_t i = 0; i < narymeexpr->numOpnds; i++) {
        RenameDelegatedRefVarUses(mestmt, narymeexpr->GetOpnd(i));
      }
      break;
    }
    case kMeOpIvar: {
      IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(meexpr);
      RenameDelegatedRefVarUses(mestmt, ivarmeexpr->base);
      break;
    }
    case kMeOpVar: {
      VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(meexpr);
      MapleMap<VarMeExpr *, RegMeExpr *>::iterator it = refvar2reg_map.find(varmeexpr);
      if (it == refvar2reg_map.end()) {
        break;
      }
      irMap->ReplaceMeExprStmt(mestmt, varmeexpr, it->second);
      break;
    }
    default:
      break;
  }
  return;
}

AnalysisResult *MeDoDelegateRC::Run(MeFunction *func, MeFuncResultMgr *m) {
  static uint32 pUcount = 0;
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func, !MeOption::quiet));
  ASSERT(dom != nullptr, "dominance phase has problem");

  { // scope needed for mempool release
    // invoke hdse to update isLive only
    MeHDSE hdse(func, dom, func->irMap);
    hdse.DseInitFull();
    hdse.DseProcess();
  }

  MIRFunction *mirfunction = func->mirFunc;

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << " Processing " << mirfunction->GetName() << endl;
  }

  MemPool *tempmp = mempoolctrler.NewMemPool("delegaterc temps");
  DelegateRC delegaterc(func, dom, tempmp);

  if (pUcount > MeOption::delrcPULimit) {
    pUcount++;
    return nullptr;
  }
  if (pUcount == MeOption::delrcPULimit) {
    LogInfo::MapleLogger() << func->mirFunc->GetName() << " is last PU optimized by delegaterc under -delrcpulimit option" << endl;
  }

  // first pass: set cantdelegate flag and count uses
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    delegaterc.SetCantDelegate(bb->mePhiList);
    for (auto stmt : bb->meStmtList) {
      if (stmt->op == OP_decref &&  // do not count decref operands
          static_cast<UnaryMeStmt *>(stmt)->opnd->meOp == kMeOpVar) {
        continue;
      }
      if (stmt->op == OP_intrinsiccall) {
        // do not count the cleanup intrinsic
        IntrinsiccallMeStmt *intrn = static_cast<IntrinsiccallMeStmt *>(stmt);
        if (intrn->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS) {
          continue;
        }
      }
      for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
        delegaterc.CollectUsesInfo(stmt->GetMeStmtOpnd(i));
      }

      if (stmt->op == OP_dassign || stmt->op == OP_maydassign || stmt->op == OP_regassign || stmt->op == OP_syncenter ||
          stmt->op == OP_syncexit || stmt->op == OP_throw || (stmt->op == OP_return && stmt->NumMeStmtOpnds() > 0)) {
        // look at copied rhs
        MeExpr *curopnd = stmt->GetMeStmtOpnd(0);
        if (curopnd->op == OP_retype) {
          curopnd = curopnd->GetOpnd(0);
        }
        VarMeExpr *copiedvar = dynamic_cast<VarMeExpr *>(curopnd);
        if (copiedvar) {
          delegaterc.verst_derefedcopied[copiedvar->vstIdx] = true;
        }
      } else if (stmt->op == OP_iassign) {
        // look at copied rhs
        IassignMeStmt *iass = static_cast<IassignMeStmt *>(stmt);
        MeExpr *curopnd = iass->rhs;
        if (curopnd->op == OP_retype) {
          curopnd = curopnd->GetOpnd(0);
        }
        VarMeExpr *copiedvar = dynamic_cast<VarMeExpr *>(curopnd);
        if (copiedvar) {
          delegaterc.verst_derefedcopied[copiedvar->vstIdx] = true;
        }
        // check dereferencing
        IvarMeExpr *lhsivar = iass->lhsVar;
        if (lhsivar->base->meOp == kMeOpVar) {
          VarMeExpr *basevar = static_cast<VarMeExpr *>(lhsivar->base);
          delegaterc.verst_derefedcopied[basevar->vstIdx] = true;
        }
      } else if (kOpcodeInfo.IsCall(stmt->op)) {
        // processsed passed operands
        for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
          MeExpr *curopnd = stmt->GetMeStmtOpnd(i);
          if (curopnd->op == OP_retype) {
            curopnd = curopnd->GetOpnd(0);
          }
          VarMeExpr *passedvar = dynamic_cast<VarMeExpr *>(curopnd);
          if (passedvar) {
            delegaterc.verst_derefedcopied[passedvar->vstIdx] = true;
          }
        }
      }
    }
  }

  // main pass
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->meStmtList) {
      bool withDecref = false;
      if (delegaterc.CanOmitRC4LHSVar(stmt, withDecref)) {
        delegaterc.DelegateHandleNoRCStmt(stmt, withDecref);  // Form B
      } else {
        delegaterc.DelegateRCTemp(stmt);  // Form A
      }
    }
  }

  // final pass: rename the uses of the delegated ref pointer variable versions;
  // set live_localrefvars based on appearances on LHS
  std::set<OStIdx> liveLocalrefvars;  // to detect dead localrefvars
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->meStmtList) {
      if (stmt->op == OP_decref && static_cast<UnaryMeStmt *>(stmt)->opnd->meOp == kMeOpVar) {
        continue;  // it is wrong to replace decref operand as it is intended for the original localrefvar
      }
      if (stmt->op == OP_intrinsiccall) {
        // no need process the cleanup intrinsic
        IntrinsiccallMeStmt *intrn = static_cast<IntrinsiccallMeStmt *>(stmt);
        if (intrn->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS) {
          continue;
        }
      }
      for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
        delegaterc.RenameDelegatedRefVarUses(stmt, stmt->GetMeStmtOpnd(i));
      }
      // for live_localrefvars
      if (stmt->op == OP_dassign || stmt->op == OP_maydassign) {
        ScalarMeExpr *lhs = stmt->GetVarLhs();
        CHECK_FATAL(lhs != nullptr, "null ptr check");
        OriginalSt *ost = lhs->ost;
        if (ost->isLocal && !ost->isFormal && !ost->ignoreRC && lhs->primType == PTY_ref) {
          liveLocalrefvars.insert(lhs->ost->index);
        }
      } else if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
        MapleVector<MustDefMeNode> *mustdefList = stmt->GetMustDefList();
        CHECK_FATAL(mustdefList != nullptr, "null ptr check");
        if (!mustdefList->empty()) {
          VarMeExpr *thelhs = dynamic_cast<VarMeExpr *>(mustdefList->front().lhs);
          if (thelhs && thelhs->primType == PTY_ref) {
            OriginalSt *ost = thelhs->ost;
            if (ost->isLocal && !ost->isFormal && !ost->ignoreRC) {
              liveLocalrefvars.insert(thelhs->ost->index);
            }
          }
        }
      }
    }
  }

  // postpass: go through the cleanup intrinsics to delete dead localrefvars
  for (BB *bb : func->theCFG->commonExitBB->pred) {
    MeStmt *lastMeStmt = bb->meStmtList.last;
    if (lastMeStmt == nullptr || lastMeStmt->op != OP_return) {
      continue;
    }
    MeStmt *mestmt = lastMeStmt->prev;
    while (mestmt != nullptr && mestmt->op != OP_intrinsiccall) {
      mestmt = mestmt->prev;
    }
    if (mestmt == nullptr) {
      continue;
    }
    if (mestmt->op != OP_intrinsiccall) {
      continue;
    }
    IntrinsiccallMeStmt *intrn = static_cast<IntrinsiccallMeStmt *>(mestmt);
    if (intrn->intrinsic != INTRN_MPL_CLEANUP_LOCALREFVARS) {
      continue;
    }
    // delete the operands that are not live
    uint32 nextpos = 0;
    uint32 i = 0;
    for (; i < intrn->opnds.size(); i++) {
      VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(intrn->opnds[i]);
      if (liveLocalrefvars.find(varmeexpr->ost->index) == liveLocalrefvars.end()) {
        continue;
      }
      if (nextpos != i) {
        intrn->opnds[nextpos] = varmeexpr;
      }
      nextpos++;
    }
    while (nextpos < i) {
      intrn->opnds.pop_back();
      i--;
    }
  }
  if (MeOption::earlydecref) {  // delete decref if opnd not in livelocalrefvars
    for (BB *bb : func->theCFG->bbVec) {
      if (bb == nullptr) {
        continue;
      }
      for (auto stmt : bb->meStmtList) {
        if (stmt->op != OP_decref) {
          continue;
        }
        UnaryMeStmt *decref = static_cast<UnaryMeStmt *>(stmt);
        VarMeExpr *varmeexpr = dynamic_cast<VarMeExpr *>(decref->opnd);
        if (varmeexpr == nullptr) {
          continue;
        }
        OriginalSt *ost = varmeexpr->ost;
        if (ost->isLocal && !ost->isFormal && !ost->ignoreRC &&
            liveLocalrefvars.find(varmeexpr->ost->index) == liveLocalrefvars.end()) {
          bb->RemoveMeStmt(stmt);
        }
      }
    }
  }

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============== After DELEGATE RC =============" << endl;
    func->irMap->Dump();
  }

  mempoolctrler.DeleteMemPool(tempmp);

  pUcount++;
  return nullptr;
}

}  // namespace maple
