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

#include "ssa_devirtual.h"

// This phase performs devirtualization based on SSA. Ideally, we should have
// precise alias information, so that for each reference we know exactly the
// objects it refers to, then the exact method it calls. However, precise alias
// analysis costs a lot.
//
// For now, we only use a simple policy to help devirtualize, E.g.
// {
//   Base b = new Derived();
//   b.foo();
// }
// We can devirtual the b.foo to be Derived::foo().

namespace maple {
bool SSADevirtual::debug = false;

static bool NonNullRetValue(const MIRFunction *called) {
  return false;
}

static bool MaybeNull(MeExpr *expr) {
  if (expr->meOp == kMeOpVar) {
    return static_cast<VarMeExpr *>(expr)->maybeNull;
  }
  if (expr->meOp == kMeOpIvar) {
    return static_cast<IvarMeExpr *>(expr)->maybeNull;
  }
  if (expr->op == OP_retype) {
    MeExpr *retypeRhs = (static_cast<OpMeExpr *>(expr))->GetOpnd(0);
    if (retypeRhs->meOp == kMeOpVar) {
      return static_cast<VarMeExpr *>(retypeRhs)->maybeNull;
    }
  }
  return true;
}

static bool IsFinalMethod(const MIRFunction *mirFunc) {
  if (mirFunc == nullptr) {
    return false;
  }
  MIRClassType *ctype = dynamic_cast<MIRClassType *>(mirFunc->GetClassType());
  // Return true if the method or its class is declared as final
  return (ctype != nullptr && (mirFunc->IsFinal() || ctype->IsFinal()));
}

TyIdx SSADevirtual::GetInferredTyidx(MeExpr *expr) {
  if (expr->meOp == kMeOpVar) {
    VarMeExpr *vme = static_cast<VarMeExpr *>(expr);
    if (vme->inferredTyIdx == TyIdx(0)) {
      // If vme->inferredTyIdx has not been set, we can double check
      // if it is coming from a static final field
      MIRSymbol *mirsym = vme->ost->GetMIRSymbol();
      if (mirsym->IsStatic() && mirsym->IsFinal() && mirsym->inferredTyidx != kInitTyIdx &&
          mirsym->inferredTyidx != kNoneTyIdx) {
        vme->inferredTyIdx = mirsym->inferredTyidx;
      }
      if (mirsym->GetType()->GetKind() == kTypePointer) {
        MIRType *pointedType = (static_cast<MIRPtrType *>(mirsym->GetType()))->GetPointedType();
        if (pointedType->GetKind() == kTypeClass) {
          if ((static_cast<MIRClassType *>(pointedType))->IsFinal()) {
            vme->inferredTyIdx = pointedType->GetTypeIndex();
          }
        }
      }
    }
    return vme->inferredTyIdx;
  }
  if (expr->meOp == kMeOpIvar) {
    return static_cast<IvarMeExpr *>(expr)->inferredTyIdx;
  }
  if (expr->op == OP_retype) {
    MeExpr *retypeRhs = (static_cast<OpMeExpr *>(expr))->GetOpnd(0);
    if (retypeRhs->meOp == kMeOpVar) {
      return static_cast<VarMeExpr *>(retypeRhs)->inferredTyIdx;
    }
  }
  return TyIdx(0);
}

void SSADevirtual::ReplaceCall(CallMeStmt *callstmt, MIRFunction *targetFunc) {
  if (SSADevirtual::debug) {
    MIRFunction *mirFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callstmt->puIdx);
    LogInfo::MapleLogger() << "[SSA-DEVIRT] " << kOpcodeInfo.GetName(callstmt->op) << " "
         << NameMangler::DecodeName(mirFunc->GetName());
  }

  if (callstmt->op == OP_virtualicall || callstmt->op == OP_virtualicallassigned || callstmt->op == OP_interfaceicall ||
      callstmt->op == OP_interfaceicallassigned) {
    // delete 1st argument
    MapleVector<MeExpr *>::iterator opndit = callstmt->opnds.begin();
    callstmt->opnds.erase(opndit);
  }

  MeExpr *receiver = callstmt->opnds[0];
  if (NeedNullCheck(receiver)) {
    InsertNullCheck(callstmt, receiver);
    nullcheck_count++;
  }

  // Set the actuall callee puIdx
  callstmt->puIdx = targetFunc->puIdx;

  if (callstmt->op == OP_virtualcall || callstmt->op == OP_virtualicall) {
    callstmt->op = OP_call;
    opted_virtualcalls++;
  } else if (callstmt->op == OP_virtualcallassigned || callstmt->op == OP_virtualicallassigned) {
    callstmt->op = OP_callassigned;
    opted_virtualcalls++;
  } else if (callstmt->op == OP_interfacecall || callstmt->op == OP_interfaceicall) {
    callstmt->op = OP_call;
    opted_interfacecalls++;
  } else if (callstmt->op == OP_interfacecallassigned || callstmt->op == OP_interfaceicallassigned) {
    callstmt->op = OP_callassigned;
    opted_interfacecalls++;
  }
  if (clone != nullptr && OP_callassigned == callstmt->op) {
    clone->UpdateReturnVoidIfPossible(callstmt, targetFunc);
  }

  if (SSADevirtual::debug) {
    LogInfo::MapleLogger() << "\t -> \t" << kOpcodeInfo.GetName(callstmt->op) << " " << NameMangler::DecodeName(targetFunc->GetName());
    if (NeedNullCheck(receiver)) {
      LogInfo::MapleLogger() << " with null-check ";
    }
    LogInfo::MapleLogger() << "\t at " << mod_->GetFilenameFromFilenum(callstmt->srcPos.Filenum()) << ":" << callstmt->srcPos.Linenum()
         << endl;
  }
}

bool SSADevirtual::DevirtualizeCall(CallMeStmt *callstmt) {
  switch (callstmt->op) {
    case OP_interfacecall:
    case OP_interfaceicall:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
      total_interfacecalls++;  // FALLTHROUGH
    case OP_virtualcall:
    case OP_virtualicall:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned: {
      total_virtualcalls++;  // actually the number of interfacecalls + virtualcalls
      const MapleVector<MeExpr *> &parms = callstmt->opnds;
      if (parms.size() == 0 || parms[0] == nullptr) {
        break;
      }
      MeExpr *thisParm = parms[0];
      if (callstmt->op == OP_interfaceicall || callstmt->op == OP_interfaceicallassigned ||
          callstmt->op == OP_virtualicall || callstmt->op == OP_virtualicallassigned) {
        thisParm = parms[1];
      }

      TyIdx receiverInferredTyidx = GetInferredTyidx(thisParm);
      MIRFunction *mirFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callstmt->puIdx);
      if (thisParm->primType == PTY_ref && receiverInferredTyidx != TyIdx(0)) {
        Klass *inferredKlass = kh_->GetKlassFromTyidx(receiverInferredTyidx);
        CHECK_FATAL(inferredKlass,
               "should have built the klass.CALLER:%s, CALLEE:%s, call does not have a valid inferred type",
               GetMIRFunction()->GetName().c_str(), mirFunc->GetName().c_str());
        // if (Options::buildApp && inferredKlass->NeedDecoupling()) {
        //  break;
        // }
        GStrIdx funcname = mirFunc->GetBaseFuncNameWithTypeStridx();
        MIRFunction *inferredFunction = inferredKlass->GetClosestMethod(funcname);
        if (!inferredFunction) {
          if (SSADevirtual::debug) {
            LogInfo::MapleLogger() << "Can not get function for " << inferredKlass->GetKlassName() << mirFunc->GetBaseFuncNameWithType()
                 << endl;
          }
          break;
        }
        if (thisParm->meOp != kMeOpVar && thisParm->meOp != kMeOpIvar) {
          break;
        }
        ReplaceCall(callstmt, inferredFunction);
        return true;
      } else if (IsFinalMethod(mirFunc)) {
        // Calling a final method which can be safely devirtualized
        // Klass *inferredKlass = kh_->GetKlassFromFunc(mirFunc);
        // if (Options::buildApp && inferredKlass->NeedDecoupling()) {
        //  break;
        // }
        GStrIdx uniqFuncNameStridx = mirFunc->GetNameStridx();
        CHECK_FATAL(uniqFuncNameStridx != GStrIdx(0), "check");
        MIRSymbol *uniqFuncSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(uniqFuncNameStridx);
        ASSERT(uniqFuncSym, "The real callee %s has not been seen in any imported .mplt file",
                mirFunc->GetName().c_str());

        MIRFunction *uniqFunc = uniqFuncSym->GetFunction();
        ASSERT(uniqFunc && (mirFunc->GetBaseFuncNameWithType() == uniqFunc->GetBaseFuncNameWithType()),
                "Invalid function replacement in devirtualization");
        ReplaceCall(callstmt, uniqFunc);
        return true;
      }
      break;
    }
    default:
      break;
  }
  return false;
}

bool SSADevirtual::NeedNullCheck(MeExpr *receiver) const {
  return MaybeNull(receiver);
}

// Java requires to throw Null-Pointer-Execption if the receiver of
// the virtualcall is null. We insert an eval(iread recevier, 0)
// statment perform the null-check.
void SSADevirtual::InsertNullCheck(CallMeStmt *callstmt, MeExpr *receiver) {
  UnaryMeStmt *nullcheck = irmap_->New<UnaryMeStmt>(OP_assertnonnull);
  nullcheck->bb = callstmt->bb;
  nullcheck->srcPos = callstmt->srcPos;
  nullcheck->opnd = receiver;
  callstmt->bb->InsertMeStmtBefore(callstmt, nullcheck);
}

void SSADevirtual::PropVarInferredType(VarMeExpr *varmeexpr) {
  if (varmeexpr->inferredTyIdx != 0) {
    return;
  }
  if (varmeexpr->defBy == kDefByStmt) {
    DassignMeStmt *defStmt = dynamic_cast<DassignMeStmt *>(varmeexpr->def.defStmt);
    CHECK_FATAL(defStmt, "");
    MeExpr *rhs = defStmt->rhs;
    if (rhs->op == OP_gcmalloc) {
      varmeexpr->inferredTyIdx = static_cast<GcmallocMeExpr *>(rhs)->tyIdx;
      varmeexpr->maybeNull = false;
      if (SSADevirtual::debug) {
        MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(varmeexpr->inferredTyIdx);
        LogInfo::MapleLogger() << "[SSA-DEVIRT] [TYPE-INFERRING] mx" << varmeexpr->exprID << " ";
        type->Dump(0, false);
        LogInfo::MapleLogger() << endl;
      }
    } else {
      TyIdx tyIdx = GetInferredTyidx(rhs);
      varmeexpr->maybeNull = MaybeNull(rhs);
      if (tyIdx != TyIdx(0)) {
        varmeexpr->inferredTyIdx = tyIdx;
        if (SSADevirtual::debug) {
          MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(varmeexpr->inferredTyIdx);
          LogInfo::MapleLogger() << "[SSA-DEVIRT] [TYPE-INFERRING] mx" << varmeexpr->exprID << " ";
          type->Dump(0, false);
          LogInfo::MapleLogger() << endl;
        }
      }
    }
    if (varmeexpr->inferredTyIdx != TyIdx(0)) {
      OriginalSt *ost = defStmt->lhs->ost;
      MIRSymbol *mirsym = ost->GetMIRSymbol();
      if (mirsym->IsStatic() && mirsym->IsFinal()) {
        // static final field can store and propagate inferred typeinfo
        if (mirsym->inferredTyidx == kInitTyIdx) {
          // mirsym->inferredTyidx has not been set before
          mirsym->inferredTyidx = varmeexpr->inferredTyIdx;
        } else if (mirsym->inferredTyidx != varmeexpr->inferredTyIdx) {
          // If mirsym->inferredTyidx has been set before, it means we have
          // seen a divergence on control flow. Set to NONE if not all
          // branches reach the same conclusion.
          mirsym->inferredTyidx = kNoneTyIdx;
        }
      }
    }
  } else if (varmeexpr->defBy == kDefByPhi) {
    if (SSADevirtual::debug) {
      LogInfo::MapleLogger() << "[SSA-DEVIRT] [TYPE-INFERRING] "
           << "Def by phi " << endl;
    }
  }
}

void SSADevirtual::PropIvarInferredType(IvarMeExpr *ivar) {
  if (ivar->inferredTyIdx != TyIdx(0)) {
    return;
  }

  IassignMeStmt *defStmt = ivar->defStmt;
  if (!defStmt) {
    // not single definition
    return;
  }
  // if isGlobal(ivar->base) return;
  MeExpr *rhs = defStmt->rhs;
  if (rhs->op == OP_gcmalloc) {
    ivar->inferredTyIdx = static_cast<GcmallocMeExpr *>(rhs)->tyIdx;
    ivar->maybeNull = false;
    if (SSADevirtual::debug) {
      MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ivar->inferredTyIdx);
      LogInfo::MapleLogger() << "[SSA-DEVIRT] [TYPE-INFERRING] mx" << ivar->exprID << " ";
      type->Dump(0, false);
      LogInfo::MapleLogger() << endl;
    }
  } else {
    TyIdx tyIdx = GetInferredTyidx(rhs);
    ivar->maybeNull = MaybeNull(rhs);
    if (tyIdx != TyIdx(0)) {
      ivar->inferredTyIdx = tyIdx;
      if (SSADevirtual::debug) {
        MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ivar->inferredTyIdx);
        LogInfo::MapleLogger() << "[SSA-DEVIRT] [TYPE-INFERRING] mx" << ivar->exprID << " ";
        type->Dump(0, false);
        LogInfo::MapleLogger() << endl;
      }
    }
  }
}

void SSADevirtual::VisitVarPhiNode(MePhiNode *varphi) const {}

void SSADevirtual::VisitMeExpr(MeExpr *meexpr) {
  if (!meexpr) {
    return;
  }
  MeExprOp meOp = meexpr->meOp;
  switch (meOp) {
    case kMeOpVar: {
      VarMeExpr *varexpr = static_cast<VarMeExpr *>(meexpr);
      PropVarInferredType(varexpr);
      break;
    }
    case kMeOpReg:
      break;
    case kMeOpIvar: {
      IvarMeExpr *ivar = static_cast<IvarMeExpr *>(meexpr);
      PropIvarInferredType(ivar);
      break;
    }
    case kMeOpOp: {
      OpMeExpr *meopexpr = static_cast<OpMeExpr *>(meexpr);
      for (size_t i = 0; i < meopexpr->numOpnds; i++) {
        VisitMeExpr(meopexpr->GetOpnd(i));
      }
      break;
    }
    case kMeOpNary: {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(meexpr);
      for (size_t i = 0; i < narymeexpr->numOpnds; i++) {
        VisitMeExpr(narymeexpr->GetOpnd(i));
      }
      break;
    }
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
      break;
  }
}

void SSADevirtual::ReturnTyidxInferring(RetMeStmt *retmestmt) {
  MapleVector<MeExpr *> &opnds = retmestmt->opnds;
  CHECK_FATAL(opnds.size() <= 1, "Assume at most one return value for now");
  for (uint32 i = 0; i < opnds.size(); i++) {
    MeExpr *opnd = opnds[i];
    TyIdx tyIdx = GetInferredTyidx(opnd);
    if (retty_ == kNotSeen) {
      // seen the first return stmt
      retty_ = kSeen;
      inferred_rettyidx_ = tyIdx;
    } else if (retty_ == kSeen) {
      // has seen an inferred type before, check if they agreed
      if (inferred_rettyidx_ != tyIdx) {
        retty_ = FAILED;
        inferred_rettyidx_ = TyIdx(0);  // not agreed, cleared.
      }
    }
  }
}

void SSADevirtual::TraversalMeStmt(MeStmt *mestmt) {
  Opcode op = mestmt->op;
  switch (op) {
    case OP_dassign: {
      DassignMeStmt *varmestmt = static_cast<DassignMeStmt *>(mestmt);
      VisitMeExpr(varmestmt->rhs);
      break;
    }
    case OP_regassign: {
      AssignMeStmt *regmestmt = static_cast<AssignMeStmt *>(mestmt);
      VisitMeExpr(regmestmt->rhs);
      break;
    }
    case OP_maydassign: {
      MaydassignMeStmt *maydstmt = static_cast<MaydassignMeStmt *>(mestmt);
      VisitMeExpr(maydstmt->rhs);
      break;
    }
    case OP_iassign: {
      IassignMeStmt *ivarstmt = static_cast<IassignMeStmt *>(mestmt);
      VisitMeExpr(ivarstmt->rhs);
      break;
    }
    case OP_syncenter:
    case OP_syncexit: {
      SyncMeStmt *syncmestmt = static_cast<SyncMeStmt *>(mestmt);
      MapleVector<MeExpr *> &opnds = syncmestmt->opnds;
      for (uint32 i = 0; i < opnds.size(); i++) {
        VisitMeExpr(opnds[i]);
      }
      break;
    }
    case OP_throw: {
      ThrowMeStmt *thrmestmt = static_cast<ThrowMeStmt *>(mestmt);
      VisitMeExpr(thrmestmt->opnd);
      break;
    }
    case OP_assertnonnull:
    case OP_eval:
    case OP_igoto:
    case OP_free: {
      UnaryMeStmt *umestmt = static_cast<UnaryMeStmt *>(mestmt);
      VisitMeExpr(umestmt->opnd);
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
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned: {
      CallMeStmt *callmestmt = static_cast<CallMeStmt *>(mestmt);
      MapleVector<MeExpr *> &opnds = callmestmt->opnds;
      for (uint32 i = 0; i < opnds.size(); i++) {
        VisitMeExpr(opnds[i]);
      }
      DevirtualizeCall(callmestmt);

      if (clone != nullptr && OP_callassigned == callmestmt->op) {
        MIRFunction *targetFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callmestmt->puIdx);
        clone->UpdateReturnVoidIfPossible(callmestmt, targetFunc);
      }

      break;
    }
    case OP_icall:
    case OP_icallassigned: {
      IcallMeStmt *icallmestmt = static_cast<IcallMeStmt *>(mestmt);
      MapleVector<MeExpr *> &opnds = icallmestmt->opnds;
      for (uint32 i = 0; i < opnds.size(); i++) {
        VisitMeExpr(opnds[i]);
      }

      break;
    }
    case OP_intrinsiccallwithtype:
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_intrinsiccallwithtypeassigned:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned: {
      IntrinsiccallMeStmt *intrincallstmt = static_cast<IntrinsiccallMeStmt *>(mestmt);
      MapleVector<MeExpr *> &opnds = intrincallstmt->opnds;
      for (uint32 i = 0; i < opnds.size(); i++) {
        VisitMeExpr(opnds[i]);
      }
      break;
    }
    case OP_brtrue:
    case OP_brfalse: {
      CondGotoMeStmt *condgotostmt = static_cast<CondGotoMeStmt *>(mestmt);
      VisitMeExpr(condgotostmt->opnd);
      break;
    }
    case OP_switch: {
      SwitchMeStmt *switchstmt = static_cast<SwitchMeStmt *>(mestmt);
      VisitMeExpr(switchstmt->opnd);
      break;
    }
    case OP_return: {
      RetMeStmt *retmestmt = static_cast<RetMeStmt *>(mestmt);
      MapleVector<MeExpr *> &opnds = retmestmt->opnds;
      for (uint32 i = 0; i < opnds.size(); i++) {
        VisitMeExpr(opnds[i]);
      }
      ReturnTyidxInferring(retmestmt);
      break;
    }
    case OP_assertlt:
    case OP_assertge: {
      AssertMeStmt *assmestmt = static_cast<AssertMeStmt *>(mestmt);
      VisitMeExpr(assmestmt->opnds[0]);
      VisitMeExpr(assmestmt->opnds[1]);
      break;
    }
    case OP_jstry:
    case OP_jscatch:
    case OP_finally:
    case OP_endtry:
    case OP_cleanuptry:
    case OP_javatry:
    case OP_cpptry:
    case OP_javacatch:
    case OP_cppcatch:
    case OP_try:
    case OP_catch:
    case OP_goto:
    case OP_gosub:
    case OP_retsub:
    case OP_comment:
    case OP_membaracquire:
    case OP_membarrelease:
    case OP_membarstoreload:
    case OP_membarstorestore:
      break;
    default:
      CHECK_FATAL(false, "unexpected stmt in ssadevirt or NYI");
  }
  if (mestmt->op == OP_callassigned) {
    MapleVector<MustDefMeNode> *mustdefList = mestmt->GetMustDefList();
    if (!mustdefList->empty()) {
      MeExpr *melhs = mustdefList->front().lhs;
      if (melhs->meOp == kMeOpVar) {
        VarMeExpr *lhsVar = static_cast<VarMeExpr *>(melhs);
        PUIdx puIdx = static_cast<CallMeStmt *>(mestmt)->puIdx;
        MIRFunction *called = GlobalTables::GetFunctionTable().funcTable[puIdx];
        if (called->inferredReturnTyIdx != TyIdx(0)) {
          lhsVar->inferredTyIdx = called->inferredReturnTyIdx;
          if (NonNullRetValue(called)) {
            lhsVar->maybeNull = false;
          }
          if (SSADevirtual::debug) {
            MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(lhsVar->inferredTyIdx);
            LogInfo::MapleLogger() << "[SSA-DEVIRT] [TYPE-INFERRING] mx" << lhsVar->exprID << " ";
            type->Dump(0, false);
            LogInfo::MapleLogger() << endl;
          }
        }
      }
    }
  }
}

void SSADevirtual::TraversalBB(BB *bb) {
  if (!bb) {
    return;
  }
  if (bb_visited[bb->id.idx]) {
    return;
  }
  bb_visited[bb->id.idx] = true;

  // traversal var phi nodes
  MapleMap<OStIdx, MePhiNode *> &mePhiList = bb->mePhiList;
  for (std::pair<OStIdx, MePhiNode *> phiEntry : mePhiList) {
    if (phiEntry.second->UseReg()) {
      continue;
    }
    VisitVarPhiNode(phiEntry.second);
  }

  // traversal reg phi nodes (NYI)

  // traversal on stmt
  for (auto mestmt : bb->meStmtList) {
    TraversalMeStmt(mestmt);
  }
}

void SSADevirtual::Perform(BB *entryBb) {
  // Pre-order traverse the cominance tree, so that each def is traversed
  // before its use
  std::stack<BB *> bblist;
  bblist.push(entryBb);
  while (!bblist.empty()) {
    BB *bb = bblist.top();
    bblist.pop();
    TraversalBB(bb);
    MapleSet<BBId> *domChildren = &dom_->domChildren[bb->id.idx];
    for (const BBId &bbid : *domChildren) {
      bblist.push(GetBB(bbid));
    }
  }

  MIRFunction *mirFunc = GetMIRFunction();
  if (!mirFunc) {
    return;  // maybe wpo
  }

  if (retty_ == kSeen) {
    mirFunc->inferredReturnTyIdx = this->inferred_rettyidx_;
  }

  // Simple rule: if method's declared returning type is a final class, then
  // the actual returning type is same with the declared returning type.
  MIRType *declReturnType = mirFunc->GetReturnType();
  if (declReturnType->primType == PTY_ref && declReturnType->typeKind == maple::kTypePointer) {
    MIRType *pointedType = static_cast<MIRPtrType *>(declReturnType)->GetPointedType();
    MIRClassType *declReturnClass = dynamic_cast<MIRClassType *>(pointedType);
    if (declReturnClass && declReturnClass->IsFinal()) {
      mirFunc->inferredReturnTyIdx = declReturnClass->tyIdx;
    }
  }

  if (SSADevirtual::debug) {
    LogInfo::MapleLogger() << "[SSA-DEVIRT]"
         << " {virtualcalls: total " << total_virtualcalls - total_interfacecalls << ", devirtualized "
         << opted_virtualcalls << "}"
         << " {interfacecalls: total " << total_interfacecalls << ", devirtualized " << opted_interfacecalls << "}"
         << ", {null-checks: " << nullcheck_count << "}"
         << "\t" << mirFunc->GetName() << endl;
    if (mirFunc && mirFunc->inferredReturnTyIdx != TyIdx(0)) {
      MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(mirFunc->inferredReturnTyIdx);
      LogInfo::MapleLogger() << "[SSA-DEVIRT] [FUNC-RETTYPE] ";
      type->Dump(0, false);
      LogInfo::MapleLogger() << endl;
    }
  }
}

}  // namespace maple
