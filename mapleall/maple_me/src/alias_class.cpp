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

#include "mpl_logging.h"
#include "opcode_info.h"
#include "ssa_mir_nodes.h"
#include "ssa_tab.h"
#include "alias_class.h"
#include "mir_function.h"
#include "mir_builder.h"

using namespace std;
namespace maple {

inline bool IsReadOnlyOst(OriginalSt *ost) {
  if (ost == nullptr) {
    return false;
  }

  return ost->GetMIRSymbol()->HasAddrOfValues();
}

inline bool IsPotentialAddress(PrimType primType, MIRModule *mirModule) {
  return IsAddress(primType) || IsPrimitiveDynType(primType) ||
         (primType == PTY_u64 && mirModule->IsCModule());
}

inline bool IsPotentialAddress(TyIdx tyIdx, MIRModule *mirModule) {
  PrimType primType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->GetPrimType();
  return IsPotentialAddress(primType, mirModule);
}

// return true if this expression opcode can result in a valid address
static bool OpCanFormAddress(Opcode op) {
  switch (op) {
  case OP_dread:
  case OP_regread:
  case OP_iread:
  case OP_ireadoff:
  case OP_ireadfpoff:
  case OP_ireadpcoff:
  case OP_addrof:
  case OP_addroffunc:
  case OP_addroflabel:
  case OP_addroffpc:
  case OP_iaddrof:
  case OP_constval:
  case OP_conststr:
  case OP_conststr16:
  case OP_alloca:
  case OP_malloc:
  case OP_add:
  case OP_sub:
  case OP_select:
  case OP_array:
  case OP_intrinsicop:
    return true;
  default: ;
  }
  return false;
}

inline bool IsNullOrDummySymbolOst(OriginalSt *ost) {
  if ((ost == nullptr) || (ost && ost->IsSymbol() && (ost->GetMIRSymbol()->GetName() == "__nads_dummysym__"))) {
    return true;
  }
  return false;
}

inline bool OriginalStIsAuto(OriginalSt *ost) {
  if (ost->ostType != OriginalSt::kSymbolOst) {
    return false;
  }
  MIRSymbol *sym = ost->GetMIRSymbol();
  return sym->storageClass == kScAuto || sym->storageClass == kScFormal;
}

bool AliasClass::CallHasSideEffect(StmtNode *stmt) {
  if (calleeHasSideEffect) {
    return true;
  }
  CallNode *callstmt = dynamic_cast<CallNode *>(stmt);
  if (callstmt == nullptr) {
    return true;
  }
  bool hassideeffect = true;
  CHECK_FATAL(callstmt->puIdx < GlobalTables::GetFunctionTable().funcTable.size(), "index out of range in CallHasSideEffect()");
  MIRFunction *callee = GlobalTables::GetFunctionTable().funcTable[callstmt->puIdx];
  if (callee->GetAttrs().GetAttr(FUNCATTR_nosideeffect)) {
    hassideeffect = false;
  } else if (!ignoreIPA) {
    hassideeffect = !GlobalTables::GetSideEffectTable().IsNoDefEffect(callee->GetNameStridx());
    if (debugPrint && !hassideeffect) {
      LogInfo::MapleLogger() << "IPA found ";
      callee->GetFuncSymbol()->Dump(mirModule, false, 0);
      LogInfo::MapleLogger() << " has no side effect" << endl;
    }
  }
  if (hassideeffect) {
    const std::string &funcname = callee->GetName();
    hassideeffect = true;
  }
  return hassideeffect;
}

bool AliasClass::CallHasNoPrivateDefEffect(StmtNode *stmt) {
  if (calleeHasSideEffect) {
    return false;
  }
  CallNode *callstmt = dynamic_cast<CallNode *>(stmt);
  if (callstmt == nullptr) {
    return false;
  }
  bool hasnoprivatedefeffect = false;
  MIRFunction *callee = GlobalTables::GetFunctionTable().funcTable[callstmt->puIdx];
  if (callee->GetAttrs().GetAttr(FUNCATTR_noprivate_defeffect)) {
    hasnoprivatedefeffect = true;
  } else if (!ignoreIPA) {
    hasnoprivatedefeffect = GlobalTables::GetSideEffectTable().IsNoPrivateDefEffect(callee->GetNameStridx());
    if (debugPrint && hasnoprivatedefeffect) {
      LogInfo::MapleLogger() << "IPA found ";
      callee->GetFuncSymbol()->Dump(mirModule, false, 0);
      LogInfo::MapleLogger() << " has no private def effect" << endl;
    }
  }
  return hasnoprivatedefeffect;
}

// here starts pass 1 code

AliasElem *AliasClass::FindOrCreateAliasElem(OriginalSt *ost) {
  AliasElem *aelem = osym2Elem[ost->index.idx];
  if (aelem == nullptr) {
    aelem = acMemPool->New<AliasElem>(id2Elem.size(), ost);
    if (ost->ostType == OriginalSt::kSymbolOst && ost->indirectLev >= 0) {
      MIRSymbol *sym = ost->GetMIRSymbol();
      if (sym->stIdx.IsGlobal() &&
          (mirModule->IsCModule() ||
           (!sym->HasAddrOfValues() && !sym->isTmp))) {
        globalsMayAffectedByClinitCheck.insert(ost->index);
        if (ost->isFinal && !InConstructorFunc()) {
          aelem->nextLevNotAllDefsSeen = true;
          if (debugPrint) {
            sym->Dump(mirModule, false, 0);
            LogInfo::MapleLogger() << " is global final" << endl;
          }
        } else if (sym->IsReflectionClassInfo()) {
          if (debugPrint) {
            sym->Dump(mirModule, false, 0);
            LogInfo::MapleLogger() << " is related to reflection class info" << endl;
          }
        } else {
          globalsAffectedByCalls.insert(aelem->id);
          if (mirModule->IsCModule()) {
            aelem->notAllDefsSeen = true;
          }
          aelem->nextLevNotAllDefsSeen = true;
        }
      } else if (mirModule->IsCModule() &&
                 (sym->storageClass == kScPstatic || sym->storageClass == kScFstatic)) {
        globalsAffectedByCalls.insert(aelem->id);
        aelem->notAllDefsSeen = true;
        aelem->nextLevNotAllDefsSeen = true;
      }
    }
    if (aelem->ost->isFormal && ost->indirectLev != -1) {
      aelem->nextLevNotAllDefsSeen = true;
    }
    if (ost->indirectLev > 1) {
      aelem->notAllDefsSeen = true;
      aelem->nextLevNotAllDefsSeen = true;
    }
    id2Elem.push_back(aelem);
    osym2Elem.at(ost->index.idx) = aelem;
    unionfind.NewMember();
  }
  return aelem;
}

AliasInfo AliasClass::CreateAliasElemsExpr(BaseNode *expr) {
  switch (expr->op) {
    case OP_addrof: {
      AddrofSSANode *addrof = static_cast<AddrofSSANode *>(expr);
      addrof->ssaVar->ost->addressTaken = true;
      FindOrCreateAliasElem(addrof->ssaVar->ost);
      OriginalSt *newost = ssaTab->originalStTable.FindOrCreateAddrofSymbolOriginalSt(addrof->ssaVar->ost);
      if (newost->index.idx == osym2Elem.size()) {
        osym2Elem.push_back(nullptr);
        ssaTab->versionStTable.CreateZeroVersionSt(newost);
      }
      AliasElem *ae = FindOrCreateAliasElem(newost);
      return AliasInfo(ae, addrof->fieldID);
    }
    case OP_dread: {
      AddrofSSANode *dread = static_cast<AddrofSSANode *>(expr);
      AliasElem *ae = FindOrCreateAliasElem(dread->ssaVar->ost);
      return AliasInfo(ae, 0);
    }
    case OP_regread: {
      RegreadSSANode *rread = static_cast<RegreadSSANode *>(expr);
      if (rread->regIdx < 0) {
        break;
      }
      AliasElem *ae = FindOrCreateAliasElem(rread->ssaVar->ost);
      return AliasInfo(ae, 0);
    }
    case OP_iread: {
      IreadNode *iread = static_cast<IreadNode *>(expr);
      AliasInfo ainfo;
      if (iread->uOpnd->op == OP_cvt) {
        ainfo = CreateAliasElemsExpr(iread->uOpnd->Opnd(0));
      } else {
        ainfo = CreateAliasElemsExpr(iread->uOpnd);
      }
      if (!mirModule->IsCModule()) {
        if (ainfo.ae == nullptr) {
          return ainfo;
        }
      } else {
        if (ainfo.ae == nullptr ||
            (iread->fieldID && ainfo.ae->ost->indirectLev != -1 && ainfo.ae->ost->tyIdx != iread->tyIdx)) {
          return AliasInfo(FindOrCreateDummyNADSAe(), 0);
        }
      }
      OriginalSt *newost = nullptr;
      if (mirModule->IsCModule() && ainfo.ae->ost->tyIdx != iread->tyIdx) {
        newost = ssaTab->originalStTable.FindOrCreateExtraLevOriginalSt(ainfo.ae->ost,
                         ainfo.ae->ost->tyIdx, 0);
      } else {
        newost = ssaTab->originalStTable.FindOrCreateExtraLevOriginalSt(ainfo.ae->ost,
                          ainfo.fieldID ? ainfo.ae->ost->tyIdx : iread->tyIdx,
                          iread->fieldID + ainfo.fieldID);
      }
      CHECK_FATAL(newost, "null ptr check");
      if (newost->index.idx == osym2Elem.size()) {
        osym2Elem.push_back(nullptr);
        ssaTab->versionStTable.CreateZeroVersionSt(newost);
      }
      AliasElem *ae = FindOrCreateAliasElem(newost);
      return AliasInfo(ae, 0);
    }
    case OP_malloc:
    case OP_gcmalloc:
    case OP_gcpermalloc:
    case OP_gcmallocjarray:
      return AliasInfo();
    case OP_iaddrof: {
      IreadNode *iaddrof = static_cast<IreadNode *>(expr);
      AliasInfo ainfo;
      ainfo = CreateAliasElemsExpr(iaddrof->uOpnd);
      if (ainfo.ae == nullptr) {
        return ainfo;
      }
      if (iaddrof->uOpnd->op != OP_addrof && iaddrof->uOpnd->op != OP_iaddrof) {
        CHECK_FATAL(ainfo.fieldID == 0, "CreateAliasElemsExpr:: cannot have non-zero fieldID here");
        if (mirModule->IsCModule() && ainfo.ae->ost->tyIdx != iaddrof->tyIdx) {
          ainfo.ae = FindOrCreateDummyNADSAe();
          return ainfo;
        }
        OriginalSt *newost = ssaTab->originalStTable.FindOrCreateExtraLevOriginalSt(ainfo.ae->ost,
                ainfo.ae->ost->tyIdx, iaddrof->fieldID);
        if (newost->index.idx == osym2Elem.size()) {
          osym2Elem.push_back(nullptr);
          ssaTab->versionStTable.CreateZeroVersionSt(newost);
        }
        FindOrCreateAliasElem(newost);
        return AliasInfo(ainfo.ae, iaddrof->fieldID);
      }
      else {
        ainfo.fieldID += iaddrof->fieldID;
        return ainfo;
      }
    }
    case OP_add:
    case OP_sub: {
      AliasInfo ainfo = CreateAliasElemsExpr(expr->Opnd(0));
      CreateAliasElemsExpr(expr->Opnd(1));
      return ainfo;
    }
    case OP_array: {
      AliasInfo ainfo = CreateAliasElemsExpr(expr->Opnd(0));
      for (int32 i = 1; i < expr->NumOpnds(); i++) {
        CreateAliasElemsExpr(expr->Opnd(i));
      }
      return ainfo;
    }
    case OP_select: {
      CreateAliasElemsExpr(expr->Opnd(0));
      AliasInfo ainfo = CreateAliasElemsExpr(expr->Opnd(1));
      AliasInfo ainfo2 = CreateAliasElemsExpr(expr->Opnd(2));
      if (!OpCanFormAddress(expr->Opnd(1)->op) || !OpCanFormAddress(expr->Opnd(2)->op)) {
        break;
      }
      if (ainfo.ae == nullptr) {
        return ainfo2;
      }
      if (ainfo2.ae == nullptr) {
        return ainfo;
      }
      ApplyUnionForDassignCopy(ainfo.ae, ainfo2.ae, expr->Opnd(2));
      return ainfo;
    }
    case OP_intrinsicop: {
      IntrinsicopNode *intrn = static_cast<IntrinsicopNode *>(expr);
      if (intrn->intrinsic == INTRN_JAVA_MERGE && intrn->NumOpnds() == 1 && intrn->nOpnd[0]->op == OP_dread) {
        return CreateAliasElemsExpr(intrn->nOpnd[0]);
      }
      // fall-thru
    }
    default:
      for (int32 i = 0; i < expr->NumOpnds(); i++) {
        CreateAliasElemsExpr(expr->Opnd(i));
      }
  }
  return AliasInfo();
}

// when a mustDef is a pointer, set its pointees' notAllDefsSeen flag to true
void AliasClass::SetNotAllDefsSeenForMustDefs(const StmtNode *callas) {
  MapleVector<MustDefNode> *mustdefs = SSAGenericGetMustDefNode(callas, &ssaTab->stmtsSSAPart);
  MapleVector<MustDefNode>::iterator it = mustdefs->begin();
  for (; it != mustdefs->end(); it++) {
    VersionSt *vst = (*it).result;
    OriginalSt *ost = vst->ost;
    AliasElem *ae = FindOrCreateAliasElem(ost);
    ae->nextLevNotAllDefsSeen = true;
  }
}

void AliasClass::ApplyUnionForDassignCopy(const AliasElem *lhsAe, AliasElem *rhsAe, const BaseNode *rhs) {
  if (!IsPotentialAddress(rhs->primType, mirModule)) {
    return;
  }
  if (HasMallocOpnd(rhs)) {
    return;
  }
  if (rhs->op == OP_addrof && IsReadOnlyOst(rhsAe->ost)) {
    return;
  }
  if (rhsAe == nullptr || rhsAe->ost->indirectLev > 0 || rhsAe->notAllDefsSeen) {
    AliasElem *ae = FindAliasElem(lhsAe->ost);
    ae->nextLevNotAllDefsSeen = true;
    return;
  }
  unionfind.Union(lhsAe->id, rhsAe->id);
}

// Set ae of the pointer-type opnds of a call as next_level_not_all_defines_seen
void AliasClass::SetPtrOpndsNextLevNADS(uint start, uint end, MapleVector<BaseNode *> &opnds) {
  for (uint i = start; i < end; i++) {
    BaseNode *opnd = opnds[i];
    AliasInfo ainfo = CreateAliasElemsExpr(opnd);
    if (IsPotentialAddress(opnd->primType, mirModule) && ainfo.ae != nullptr) {
      if (opnd->op == OP_addrof && IsReadOnlyOst(ainfo.ae->ost)) {
        continue;
      }
      ainfo.ae->nextLevNotAllDefsSeen = true;
    }
  }
  return;
}

// based on ost1's extra level ost's, ensure corresponding ones exist for ost2
void AliasClass::CreateMirroringAliasElems(OriginalSt *ost1, OriginalSt *ost2) {
  if (!ost1->IsSymbol() || !ost2->IsSymbol()) {
    return;
  }
  if (ost1->GetMIRSymbol() == ost2->GetMIRSymbol()) {
    return;
  }
  MapleForwardList<OriginalSt *>::iterator it = ost1->nextlevelnodes.begin();
  for (; it != ost1->nextlevelnodes.end(); it++) {
    OriginalSt *nextLevelOst1 = *it;
    AliasElem *ae1 = FindOrCreateAliasElem(nextLevelOst1);
    OriginalSt *nextLevelOst2 = ssaTab->originalStTable.FindOrCreateExtraLevOriginalSt(ost2, ost2->tyIdx, nextLevelOst1->fieldID);
    if (nextLevelOst2->index.idx == osym2Elem.size()) {
      osym2Elem.push_back(nullptr);
      ssaTab->versionStTable.CreateZeroVersionSt(nextLevelOst2);
    }
    AliasElem *ae2 = FindOrCreateAliasElem(nextLevelOst2);
    unionfind.Union(ae1->id, ae2->id);
    CreateMirroringAliasElems(nextLevelOst1, nextLevelOst2); // recursive call
  }
}

void AliasClass::ApplyUnionForCopies(StmtNode *stmt) {
  switch (stmt->op) {
    case OP_maydassign:
    case OP_dassign: {
      DassignNode *dass = static_cast<DassignNode *>(stmt);
      // RHS
      AliasInfo rhsAinfo = CreateAliasElemsExpr(dass->GetRhs());
      // LHS
      MayDefPartWithVersionSt *theSSAPart =
        static_cast<MayDefPartWithVersionSt *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      AliasElem *lhsAe = FindOrCreateAliasElem(theSSAPart->ssaVar->ost);

      ApplyUnionForDassignCopy(lhsAe, rhsAinfo.ae, dass->Opnd(0));
      // at p = x, if the next level of either side exists, create other
      // side's next level
      if (mirModule->IsCModule() && rhsAinfo.ae && lhsAe->ost->tyIdx == rhsAinfo.ae->ost->tyIdx) {
        CreateMirroringAliasElems(rhsAinfo.ae->ost, lhsAe->ost);
        CreateMirroringAliasElems(lhsAe->ost, rhsAinfo.ae->ost);
      }
      return;
    }
    case OP_regassign: {
      RegassignNode *rass = static_cast<RegassignNode *>(stmt);
      // RHS
      AliasInfo rhsAinfo = CreateAliasElemsExpr(rass->Opnd(0));
      // LHS
      VersionSt *theSSAPart = static_cast<VersionSt *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      AliasElem *lhsAe = FindOrCreateAliasElem(theSSAPart->ost);

      ApplyUnionForDassignCopy(lhsAe, rhsAinfo.ae, rass->Opnd(0));
      return;
    }
    case OP_iassign: {
      IassignNode *iass = static_cast<IassignNode *>(stmt);
      AliasInfo rhsAinfo = CreateAliasElemsExpr(iass->rhs);
      AliasInfo lhsAinfo;
      if (iass->addrExpr->op == OP_cvt) {
        lhsAinfo = CreateAliasElemsExpr(iass->addrExpr->Opnd(0));
      } else {
        lhsAinfo = CreateAliasElemsExpr(iass->addrExpr);
      }
      if (!mirModule->IsCModule()) {
        if (lhsAinfo.ae == nullptr) {
          return;
        }
      } else {
        if (lhsAinfo.ae == nullptr ||
            (iass->fieldID && lhsAinfo.ae->ost->indirectLev != -1 && lhsAinfo.ae->ost->tyIdx != iass->tyIdx)) {
          lhsAinfo.ae = FindOrCreateDummyNADSAe();
          ApplyUnionForDassignCopy(lhsAinfo.ae, rhsAinfo.ae, iass->rhs);
          return;
        }
      }
      OriginalSt *newost = nullptr;
      if (mirModule->IsCModule() && lhsAinfo.ae->ost->tyIdx != iass->tyIdx) {
        newost = ssaTab->originalStTable.FindOrCreateExtraLevOriginalSt(lhsAinfo.ae->ost,
                      lhsAinfo.ae->ost->tyIdx, 0);
      } else {
        newost = ssaTab->originalStTable.FindOrCreateExtraLevOriginalSt(lhsAinfo.ae->ost,
                      lhsAinfo.fieldID ? lhsAinfo.ae->ost->tyIdx : iass->tyIdx,
                      iass->fieldID + lhsAinfo.fieldID);
      }
      CHECK_FATAL(newost, "null ptr check");
      if (newost->index.idx == osym2Elem.size()) {
        osym2Elem.push_back(nullptr);
        ssaTab->versionStTable.CreateZeroVersionSt(newost);
      }
      AliasElem *lhsAe = FindOrCreateAliasElem(newost);

      ApplyUnionForDassignCopy(lhsAe, rhsAinfo.ae, iass->rhs);
      return;
    }
    case OP_throw: {
      UnaryStmtNode *tstmt = static_cast<UnaryStmtNode *>(stmt);
      AliasInfo ainfo = CreateAliasElemsExpr(tstmt->uOpnd);
      if (IsPotentialAddress(tstmt->uOpnd->primType, mirModule) && ainfo.ae != nullptr) {
        if (!(tstmt->uOpnd->op == OP_addrof && IsReadOnlyOst(ainfo.ae->ost))) {
          ainfo.ae->nextLevNotAllDefsSeen = true;
        }
      }
      return;
    }
    case OP_call:
    case OP_callassigned: {
      for (int32 i = 0; i < stmt->NumOpnds(); i++) {
        CreateAliasElemsExpr(stmt->Opnd(i));
      }
      CallNode *call = static_cast<CallNode *>(stmt);
      CHECK(call->puIdx < GlobalTables::GetFunctionTable().funcTable.size(), "index out of range in AliasClass::ApplyUnionForCopies");
      SetPtrOpndsNextLevNADS(0, call->NumOpnds(), call->nOpnd);
      if (stmt->op == OP_callassigned) {
        SetNotAllDefsSeenForMustDefs(stmt);
      }
      return;
    }
    case OP_icall:
    case OP_icallassigned: {
      for (int32 i = 0; i < stmt->NumOpnds(); i++) {
        CreateAliasElemsExpr(stmt->Opnd(i));
      }
      IcallNode *icall = static_cast<IcallNode *>(stmt);
      SetPtrOpndsNextLevNADS(1, icall->NumOpnds(), icall->nOpnd);
      if (stmt->op == OP_icallassigned) {
        SetNotAllDefsSeenForMustDefs(stmt);
      }
      return;
    }
    case OP_virtualcall:
    case OP_virtualicall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_interfaceicall:
    case OP_customcall:
    case OP_polymorphiccall:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned: {
      if (CallHasSideEffect(stmt)) {
        bool hasnoprivatedefeffect = CallHasNoPrivateDefEffect(stmt);
        CallNode *call = static_cast<CallNode *>(stmt);
        for (int32 i = 1; i < call->NumOpnds(); i++) {
          AliasInfo ainfo = CreateAliasElemsExpr(call->nOpnd[i]);
          if (IsPotentialAddress(call->nOpnd[i]->primType, mirModule) && ainfo.ae != nullptr) {
            if (call->nOpnd[i]->op == OP_addrof && IsReadOnlyOst(ainfo.ae->ost)) {
              continue;
            }
            if (hasnoprivatedefeffect && ainfo.ae->ost->isPrivate) {
              continue;
            }
            ainfo.ae->nextLevNotAllDefsSeen = true;
          }
        }
      }
      if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
        SetNotAllDefsSeenForMustDefs(stmt);
      }
      return;
    }
    // Todo: needs to review and update intrinsic sideeffect description.
    case OP_intrinsiccall:
    case OP_intrinsiccallassigned: {
      for (int32 i = 0; i < stmt->NumOpnds(); i++) {
        CreateAliasElemsExpr(stmt->Opnd(i));
      }
      IntrinsiccallNode *innode = static_cast<IntrinsiccallNode *>(stmt);
      if (innode->intrinsic == INTRN_JAVA_POLYMORPHIC_CALL) {
        SetPtrOpndsNextLevNADS(0, innode->NumOpnds(), innode->nOpnd);
        if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
          SetNotAllDefsSeenForMustDefs(stmt);
        }
        return;
      }
    }
    default:
      for (int32 i = 0; i < stmt->NumOpnds(); i++) {
        CreateAliasElemsExpr(stmt->Opnd(i));
      }
      if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
        SetNotAllDefsSeenForMustDefs(stmt);
      }
      return;
  }
}

void AliasClass::CreateAssignSets() {
  // iterate through all the alias elems
  for (AliasElem *aliaselem : id2Elem) {
    uint id = aliaselem->id;
    uint rootid = unionfind.Root(id);
    if (unionfind.sz[rootid] > 1) {
      // only root id's have assignSet
      if (id2Elem[rootid]->assignSet == nullptr) {
        id2Elem[rootid]->assignSet = acMemPool->New<MapleSet<uint>>(std::less<uint>(), acAlloc.Adapter());
      }
      id2Elem[rootid]->assignSet->insert(id);
    }
  }
}

void AliasClass::DumpAssignSets() {
  LogInfo::MapleLogger() << "/////// assign sets ///////\n";
  for (AliasElem *aliaselem : id2Elem) {
    if (aliaselem->assignSet == nullptr) {
      if (unionfind.Root(aliaselem->id) == aliaselem->id) {
        LogInfo::MapleLogger() << "Alone: ";
        aliaselem->Dump(mirModule);
        LogInfo::MapleLogger() << endl;
      }
    } else if (unionfind.Root(aliaselem->id) == aliaselem->id) {
      LogInfo::MapleLogger() << "Members of assign set " << aliaselem->id << ": ";
      for (MapleSet<uint>::iterator setit = aliaselem->assignSet->begin(); setit != aliaselem->assignSet->end();
           setit++) {
        uint elemid = *setit;
        id2Elem[elemid]->Dump(mirModule);
      }
      LogInfo::MapleLogger() << endl;
    }
  }
}

void AliasClass::UnionAllPointedTos() {
  std::vector<AliasElem *> pointedTos;
  for (AliasElem *aliaselem : id2Elem) {
    if (aliaselem->ost->indirectLev > 0) {
      aliaselem->notAllDefsSeen = true;
      pointedTos.push_back(aliaselem);
    }
  }

  if (pointedTos.empty()) {
    return;
  }

  AliasElem *aliaselem0 = pointedTos[0];
  pointedTos.erase(pointedTos.begin());
  for (AliasElem *aliaselem : pointedTos) {
    unionfind.Union(aliaselem0->id, aliaselem->id);
  }
}

// process the union among the pointed's of assignsets
void AliasClass::ApplyUnionForPointedTos() {
  // first, process nextLevNotAllDefsSeen for alias elems with no assignment
  for (AliasElem *aliaselem : id2Elem) {
    if (aliaselem->assignSet == nullptr) {
      if (aliaselem->nextLevNotAllDefsSeen) {
        MapleForwardList<OriginalSt *>::iterator ostit = aliaselem->ost->nextlevelnodes.begin();
        for (; ostit != aliaselem->ost->nextlevelnodes.end(); ostit++) {
          AliasElem *indae = FindAliasElem(*ostit);
          if (!indae->ost->isFinal || finalFieldAlias) {
            indae->notAllDefsSeen = true;
          }
        }
      }
    }
  }

  MapleSet<uint> tempset(std::less<uint>(), acAlloc.Adapter());
  for (AliasElem *aliaselem : id2Elem) {
    if (aliaselem->assignSet == nullptr) {
      continue;
    }

    // iterate through all the alias elems to check if any has indirectLev > 0
    // or if any has nextLevNotAllDefsSeen being true
    bool hasNextLevNotAllDefsSeen = false;
    for (MapleSet<uint>::iterator setit = aliaselem->assignSet->begin(); setit != aliaselem->assignSet->end();
         setit++) {
      AliasElem *ae0 = id2Elem[*setit];
      if (ae0->ost->indirectLev > 0 || ae0->notAllDefsSeen || ae0->nextLevNotAllDefsSeen) {
        hasNextLevNotAllDefsSeen = true;
        break;
      }
    }
    if (hasNextLevNotAllDefsSeen) {
      // make all pointedto's in this assignSet notAllDefsSeen
      for (MapleSet<uint>::iterator setit = aliaselem->assignSet->begin(); setit != aliaselem->assignSet->end();
           setit++) {
        AliasElem *ae0 = id2Elem[*setit];
        MapleForwardList<OriginalSt *>::iterator ostit = ae0->ost->nextlevelnodes.begin();
        for (; ostit != ae0->ost->nextlevelnodes.end(); ostit++) {
          AliasElem *indae = FindAliasElem(*ostit);
          if (!indae->ost->isFinal || finalFieldAlias) {
            indae->notAllDefsSeen = true;
          }
        }
      }
      continue;
    }

    // apply union among the assignSet elements
    tempset = *(aliaselem->assignSet);
    do {
      // pick one alias element
      MapleSet<uint>::iterator pickit = tempset.begin();
      if (pickit == tempset.end()) {
        break;  // done processing all elements in assignSet
      }
      AliasElem *ae1 = id2Elem[*pickit];
      tempset.erase(pickit);
      for (MapleSet<uint>::iterator setit = tempset.begin(); setit != tempset.end(); setit++) {
        AliasElem *ae2 = id2Elem[*setit];
        MapleForwardList<OriginalSt *>::iterator ost1it = ae1->ost->nextlevelnodes.begin();
        for (; ost1it != ae1->ost->nextlevelnodes.end(); ost1it++) {
          MapleForwardList<OriginalSt *>::iterator ost2it = ae2->ost->nextlevelnodes.begin();
          for (; ost2it != ae2->ost->nextlevelnodes.end(); ost2it++) {
            bool hasFieldid0 = (*ost1it)->fieldID == 0 || (*ost2it)->fieldID == 0;
            if (((*ost1it)->fieldID != (*ost2it)->fieldID) && !hasFieldid0) {
              continue;
            }
            if (!finalFieldAlias && ((*ost1it)->isFinal || (*ost2it)->isFinal)) {
              continue;
            }
            AliasElem *indae1 = FindAliasElem(*ost1it);
            AliasElem *indae2 = FindAliasElem(*ost2it);
            unionfind.Union(indae1->id, indae2->id);
          }
        }
      }
    } while (true);
  }
}

// fabricate the imaginary not_all_def_seen AliasElem
AliasElem *AliasClass::FindOrCreateDummyNADSAe() {
  MIRSymbol *dummySym = mirModule->mirBuilder->GetOrCreateGlobalDecl("__nads_dummysym__", GlobalTables::GetTypeTable().GetInt32());
  dummySym->isTmp = true;
  dummySym->wpofakeRet = true;
  dummySym->SetIsDeleted();
  OriginalSt *dummyOst = ssaTab->originalStTable.FindOrCreateSymbolOriginalSt(dummySym, 0, 0);
  ssaTab->versionStTable.CreateZeroVersionSt(dummyOst);

  if (osym2Elem.size() > dummyOst->index.idx && osym2Elem[dummyOst->index.idx] != nullptr) {
    return osym2Elem[dummyOst->index.idx];
  } else {
    AliasElem *dummyAe = acMemPool->New<AliasElem>(id2Elem.size(), dummyOst);
    CHECK_FATAL(dummyAe->ost->index.idx == osym2Elem.size(), "UnionForNotAllDefsSeen:: osym2Elem size is out of sync");
    dummyAe->notAllDefsSeen = true;
    id2Elem.push_back(dummyAe);
    osym2Elem.push_back(dummyAe);
    unionfind.NewMember();
    return dummyAe;
  }
}

void AliasClass::UnionForNotAllDefsSeen() {
  std::vector<AliasElem *> notAllDefsSeenAes;
  for (AliasElem *ae : id2Elem) {
    if (ae->notAllDefsSeen) {
      notAllDefsSeenAes.push_back(ae);
    }
  }

  if (notAllDefsSeenAes.empty()) {
    return;
  }

  // notAllDefsSeenAe is the first notAllDefsSeen AliasElem.
  // Union notAllDefsSeenAe with the other notAllDefsSeen aes.
  AliasElem *notAllDefsSeenAe = notAllDefsSeenAes[0];
  notAllDefsSeenAes.erase(notAllDefsSeenAes.begin());
  for (AliasElem *ae : notAllDefsSeenAes) {
    unionfind.Union(notAllDefsSeenAe->id, ae->id);
  }

  uint rootIdOfNotAllDefsSeenAe = unionfind.Root(notAllDefsSeenAe->id);
  for (AliasElem *ae : id2Elem) {
    if (unionfind.Root(ae->id) == rootIdOfNotAllDefsSeenAe) {
      ae->notAllDefsSeen = true;
    }
  }

  // iterate through originalStTable; if the symbol (at level 0) is
  // notAllDefsSeen, then set the same for all its level 1 members; then
  // for each level 1 member of each symbol, if any is set notAllDefsSeen,
  // set all members at that level to same;
  for (uint32 i = 1; i < ssaTab->originalStTable.original_st_vector_.size(); i++) {
    OriginalSt *ost = ssaTab->originalStTable.original_st_vector_[i];
    if (ost->ostType != OriginalSt::kSymbolOst) {
      continue;
    }
    AliasElem *ae = osym2Elem[ost->index.idx];
    if (ae == nullptr) {
      continue;
    }
    OriginalSt *ostOfAe = ae->ost;
    bool hasNotAllDefsSeen = ae->notAllDefsSeen || IsFormalParm(ostOfAe);
    MapleForwardList<OriginalSt *>::iterator it;
    if (!hasNotAllDefsSeen) {
      // see if any at level 1 has notAllDefsSeen set
      for (OriginalSt *nextLevelNode : ost->nextlevelnodes) {
        ae = osym2Elem[nextLevelNode->index.idx];
        if (ae != nullptr && ae->notAllDefsSeen) {
          hasNotAllDefsSeen = true;
          break;
        }
      }
    }
    if (hasNotAllDefsSeen) {
      // set to true for all members at this level
      for (OriginalSt *nextLevelNode : ost->nextlevelnodes) {
        AliasElem *ae = osym2Elem[nextLevelNode->index.idx];
        if (ae != nullptr && (finalFieldAlias || !ae->ost->isFinal)) {
          ae->notAllDefsSeen = true;
          unionfind.Union(notAllDefsSeenAe->id, ae->id);
        }
      }
    }
  }
}

// This is applicable only for C language.  For each ost that is a struct,
// union all fields within the same struct
void AliasClass::ApplyUnionForStorageOverlaps() {
  // iterate through all the alias elems
  for (AliasElem *ae : id2Elem) {
    OriginalSt *ost = ae->ost;
    OriginalSt *prevLevOst = ost->prevlevelnode;
    if (prevLevOst == nullptr) {
      continue;
    }
    MIRType *prevLevType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(prevLevOst->tyIdx);
    if (prevLevType->typeKind != kTypePointer) {
      continue;
    }
    MIRType *prevLevPointedType = static_cast<MIRPtrType *>(prevLevType)->GetPointedType();
    MIRType *ostType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ost->tyIdx);
    if (!ostType->HasFields() && prevLevPointedType->typeKind != kTypeUnion) {
      continue;
    }
    for (OriginalSt *sameLevOst : prevLevOst->nextlevelnodes) {
      AliasElem *ae1 = FindAliasElem(sameLevOst);
      unionfind.Union(ae->id, ae1->id);
    }
  }
}


// TBAA
// Collect the alias groups. Each alias group is a map that maps the rootId to the ids aliasing with the root.
void AliasClass::CollectAliasGroups(std::map<uint, std::set<uint>> &aliasGroups) {
  // key is the root id. The set contains ids of aes that alias with the root.
  for (AliasElem *ae : id2Elem) {
    uint id = ae->id;
    uint rootId = unionfind.Root(ae->id);
    if (id != rootId) {
      if (aliasGroups.find(rootId) == aliasGroups.end()) {
        std::set<uint> idsAliasWithRoot;
        aliasGroups.insert(make_pair(rootId, idsAliasWithRoot));
      }
      aliasGroups[rootId].insert(id);
    }
  }
}

bool AliasClass::AliasAccordingToType(TyIdx tyidxA, TyIdx tyidxB) {
  MIRType *mirTypeA = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyidxA);
  MIRType *mirTypeB = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyidxB);
  if (mirTypeA == mirTypeB || mirTypeA == nullptr || mirTypeB == nullptr) {
    return true;
  }
  if (mirTypeA->typeKind != mirTypeB->typeKind) {
    return false;
  }

  switch (mirTypeA->typeKind) {
    case kTypeScalar: {
      if (mirTypeA->GetPrimType() == mirTypeB->GetPrimType()) {
        return true;
      }
      return false;
    }
    case kTypeClass: {
      Klass *klassA = klassHierarchy->GetKlassFromTyidx(mirTypeA->tyIdx);
      Klass *klassB = klassHierarchy->GetKlassFromTyidx(mirTypeB->tyIdx);
      if (klassA == klassB || klassA->GetKlassName() == NameMangler::kJavaLangObjectStr ||
          klassB->GetKlassName() == NameMangler::kJavaLangObjectStr) {
        return true;
      }
      if (klassHierarchy->IsSuperKlass(klassA, klassB) || klassHierarchy->IsSuperKlass(klassB, klassA)) {
        return true;
      }
      return false;
    }
    case kTypePointer: {
      MIRType *pointedTypeA = (static_cast<MIRPtrType *>(mirTypeA))->GetPointedType();
      MIRType *pointedTypeB = (static_cast<MIRPtrType *>(mirTypeB))->GetPointedType();
      return AliasAccordingToType(pointedTypeA->tyIdx, pointedTypeB->tyIdx);
    }
    case kTypeJArray: {
      MIRJarrayType *mirJarrayTypeA = static_cast<MIRJarrayType *>(mirTypeA);
      MIRJarrayType *mirJarrayTypeB = static_cast<MIRJarrayType *>(mirTypeB);
      return AliasAccordingToType(mirJarrayTypeA->elemTyIdx, mirJarrayTypeB->elemTyIdx);
    }
    default:
      return true;
  }
}

bool AliasClass::AliasAccordingToFieldId(const OriginalSt *ostA, const OriginalSt *ostB) const {
  if (ostA->fieldID == 0 || ostB->fieldID == 0) {
    return true;
  }

  MIRType *mirTypeA = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ostA->prevlevelnode->tyIdx);
  MIRType *mirTypeB = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ostB->prevlevelnode->tyIdx);

  if (mirTypeA->typeKind == kTypePointer && mirTypeB->typeKind == kTypePointer) {
    mirTypeA = (static_cast<MIRPtrType *>(mirTypeA))->GetPointedType();
    mirTypeB = (static_cast<MIRPtrType *>(mirTypeB))->GetPointedType();
  }

  if (mirTypeA->typeKind == kTypeClass && mirTypeB->typeKind == kTypeClass) {
    Klass *klassA = klassHierarchy->GetKlassFromTyidx(mirTypeA->tyIdx);
    Klass *klassB = klassHierarchy->GetKlassFromTyidx(mirTypeB->tyIdx);

    int offSet = 0;
    Klass *super = nullptr;
    if (klassHierarchy->IsSuperKlass(klassA, klassB)) {
      super = klassB;
      while (klassA != super) {
        super = super->GetSuperKlass();
        offSet++;
      }
    } else if (klassHierarchy->IsSuperKlass(klassB, klassA)) {
      super = klassA;
      while (klassB != super) {
        super = super->GetSuperKlass();
        offSet--;
      }
    } else {
      return false;
    }
    if ((ostA->fieldID + offSet) == ostB->fieldID) {
      return true;
    }
    return false;
  }
  return true;
}

void AliasClass::ReconstructAliasGroups() {
  // map the root id to the set contains the ae-id that alias with the root.
  std::map<uint, std::set<uint>> aliasGroups;
  CollectAliasGroups(aliasGroups);

  unionfind.Reinit();

  // kv.first is the root id. kv.second is the id the alias with the root.
  for (auto oneGroup : aliasGroups) {
    std::vector<uint> newGroups;  // contains one id of each new alias group.
    uint rootId = oneGroup.first;
    std::set<uint> idsAliasWithRoot = oneGroup.second;

    newGroups.push_back(rootId);
    for (uint idA : idsAliasWithRoot) {
      bool unioned = false;
      for (uint idB : newGroups) {
        OriginalSt *ostA = id2Elem[idA]->ost;
        OriginalSt *ostB = id2Elem[idB]->ost;
        if (AliasAccordingToType(ostA->prevlevelnode->tyIdx, ostB->prevlevelnode->tyIdx) &&
            AliasAccordingToFieldId(ostA, ostB)) {
          unionfind.Union(idA, idB);
          unioned = true;
          break;
        }
      }
      if (!unioned) {
        newGroups.push_back(idA);
      }
    }
  }
}

void AliasClass::CollectNotAllDefsSeenAes() {
  for (AliasElem *ae : id2Elem) {
    if (ae->notAllDefsSeen && ae->id == unionfind.Root(ae->id)) {
      notAllDefsSeenClassSetRoots.push_back(ae);
    }
  }
}


void AliasClass::CreateClassSets() {
  // iterate through all the alias elems
  for (AliasElem *aliaselem : id2Elem) {
    uint id = aliaselem->id;
    uint rootid = unionfind.Root(id);
    if (unionfind.sz[rootid] > 1) {
      if (id2Elem[rootid]->classSet == nullptr) {
        id2Elem[rootid]->classSet = acMemPool->New<MapleSet<uint>>(std::less<uint>(), acAlloc.Adapter());
      }
      aliaselem->classSet = id2Elem[rootid]->classSet;
      aliaselem->classSet->insert(id);
    }
  }

  CollectNotAllDefsSeenAes();

#if DEBUG
  for (AliasElem *aliaselem : id2Elem) {
    if (aliaselem->classSet != nullptr && aliaselem->notAllDefsSeen == false &&
        unionfind.Root(aliaselem->id) == aliaselem->id) {
      ASSERT(aliaselem->classSet->size() == unionfind.sz[aliaselem->id], "AliasClass::CreateClassSets: wrong result");
    }
  }
#endif
}

void AliasElem::Dump(MIRModule *mod) const {
  if (ost) {
    ost->Dump();
  }
  LogInfo::MapleLogger() << "id" << id;
  if (notAllDefsSeen) {
    LogInfo::MapleLogger() << "?";
  }
  LogInfo::MapleLogger() << " ";
}

void AliasClass::DumpClassSets() {
  LogInfo::MapleLogger() << "/////// class sets ///////\n";
  for (AliasElem *aliaselem : id2Elem) {
    if (aliaselem->classSet == nullptr) {
      if (unionfind.Root(aliaselem->id) == aliaselem->id) {
        LogInfo::MapleLogger() << "Alone: ";
        aliaselem->Dump(mirModule);
        LogInfo::MapleLogger() << endl;
      }
    } else if (unionfind.Root(aliaselem->id) == aliaselem->id) {
      LogInfo::MapleLogger() << "Members of alias class " << aliaselem->id << ": ";
      for (MapleSet<uint>::iterator setit = aliaselem->classSet->begin(); setit != aliaselem->classSet->end();
           setit++) {
        uint elemid = *setit;
        id2Elem[elemid]->Dump(mirModule);
      }
      LogInfo::MapleLogger() << endl;
    }
  }
}

// here starts pass 2 code

void AliasClass::InsertMayUseExpr(BaseNode *expr) {
  for (int32 i = 0; i < expr->NumOpnds(); i++) {
    InsertMayUseExpr(expr->Opnd(i));
  }
  if (expr->op != OP_iread) {
    return;
  }
  AliasInfo rhsAinfo = CreateAliasElemsExpr(expr);
  if (rhsAinfo.ae == nullptr) {
    rhsAinfo.ae = FindOrCreateDummyNADSAe();
  }
  IreadSSANode *ireadnode = static_cast<IreadSSANode *>(expr);
  ireadnode->mayUse.opnd = ssaTab->versionStTable.GetZeroVersionSt(rhsAinfo.ae->ost);
  CHECK_FATAL(ireadnode->mayUse.opnd != nullptr, "AliasClass::InsertMayUseExpr(): iread cannot have empty mayUse");
}

// collect the mayUses caused by globalsAffectedByCalls.
void AliasClass::CollectMayUseFromGlobalsAffectedByCalls(std::set<OriginalSt *> &mayUseOsts) {
  for (uint elemId : globalsAffectedByCalls) {
    mayUseOsts.insert(id2Elem[elemId]->ost);
  }
}

// collect the mayUses caused by not_all_def_seen_ae(NADS).
void AliasClass::CollectMayUseFromNADS(std::set<OriginalSt *> &mayUseOsts) {
  for (AliasElem *notAllDefsSeenAe : notAllDefsSeenClassSetRoots) {
    if (notAllDefsSeenAe->classSet == nullptr) {
      // single mayUse
      if (mirModule->IsCModule() || !IsNullOrDummySymbolOst(notAllDefsSeenAe->ost)) {
        mayUseOsts.insert(notAllDefsSeenAe->ost);
      }
    } else {
      for (uint elemId : *(notAllDefsSeenAe->classSet)) {
        AliasElem *ae = id2Elem[elemId];
        if (!mirModule->IsCModule() && ae->ost->indirectLev == 0 && OriginalStIsAuto(ae->ost)) {
          continue;
        }
        mayUseOsts.insert(ae->ost);
      }
    }
  }
}

// insert the ost of mayUseOsts into mayUseNodes
void AliasClass::InsertMayUseNode(std::set<OriginalSt *> &mayUseOsts, MapleMap<OStIdx, MayUseNode> *mayUseNodes) {
  for (OriginalSt *ost : mayUseOsts) {
    mayUseNodes->insert(
      std::make_pair(ost->index, MayUseNode(ssaTab->versionStTable.GetZeroVersionSt(ost))));
  }
}

// insert mayUse for Return-statement.
// two kinds of mayUse's are insert into the mayUseNodes:
// 1. mayUses caused by not_all_def_seen_ae;
// 2. mayUses caused by globalsAffectedByCalls.
void AliasClass::InsertMayUseReturn(const StmtNode *stmt) {
  std::set<OriginalSt *> mayUseOsts;
  // 1. collect mayUses caused by not_all_def_seen_ae.
  CollectMayUseFromNADS(mayUseOsts);
  // 2. collect mayUses caused by globals_affected_by_call.
  CollectMayUseFromGlobalsAffectedByCalls(mayUseOsts);

  MapleMap<OStIdx, MayUseNode> *mayUseNodes = SSAGenericGetMayUseNode(stmt, &ssaTab->stmtsSSAPart);
  InsertMayUseNode(mayUseOsts, mayUseNodes);
}

// collect next_level_nodes of the ost of ReturnOpnd into mayUseOsts
void AliasClass::CollectPtsToOfReturnOpnd(OriginalSt *ost, std::set<OriginalSt *> &mayUseOsts) {
  for (OriginalSt *nextLevelOst : ost->nextlevelnodes) {
    AliasElem *indAe = FindAliasElem(nextLevelOst);
    if (!indAe->notAllDefsSeen) {
      if (!indAe->ost->isFinal || finalFieldAlias) {
        if (indAe->classSet == nullptr) {
          mayUseOsts.insert(indAe->ost);
        } else {
          for (uint elemId : *(indAe->classSet)) {
            mayUseOsts.insert(id2Elem[elemId]->ost);
          }
        }
      }
    }
  }
}

// insert mayuses at a return stmt caused by its return operand being a pointer
void AliasClass::InsertReturnOpndMayUse(StmtNode *stmt) {
  if (stmt == nullptr || stmt->op != OP_return) {
    return;
  }
  if (stmt->NumOpnds() != 0) {
    // insert mayuses for the return operand's next level
    BaseNode *retv = stmt->Opnd(0);
    AliasInfo aInfo = CreateAliasElemsExpr(retv);
    if (IsPotentialAddress(retv->primType, mirModule) && aInfo.ae != nullptr) {
      if (retv->op == OP_addrof && IsReadOnlyOst(aInfo.ae->ost)) {
        return;
      }
      if (!aInfo.ae->nextLevNotAllDefsSeen) {
        std::set<OriginalSt *> mayUseOsts;
        if (aInfo.ae->assignSet == nullptr) {
          CollectPtsToOfReturnOpnd(aInfo.ae->ost, mayUseOsts);
        } else {
          for (uint elemId : *(aInfo.ae->assignSet)) {
            CollectPtsToOfReturnOpnd(id2Elem[elemId]->ost, mayUseOsts);
          }
        }
        // insert mayUses
        MapleMap<OStIdx, MayUseNode> *mayUseNodes = SSAGenericGetMayUseNode(stmt, &ssaTab->stmtsSSAPart);
        InsertMayUseNode(mayUseOsts, mayUseNodes);
      }
    }
  }
}

void AliasClass::InsertMayUseAll(const StmtNode *stmt) {
  MapleMap<OStIdx, MayUseNode> *mayUseNodes = SSAGenericGetMayUseNode(stmt, &ssaTab->stmtsSSAPart);
  for (AliasElem *ae : id2Elem) {
    if (ae->ost == nullptr) {
      continue;
    }
    if (ae->ost->indirectLev < 0) {
      continue;
    }
    if (ae->ost->ostType == OriginalSt::OriginalSt::kPregOst) {
      continue;
    }
    mayUseNodes->insert(std::make_pair(
      ae->ost->index, MayUseNode(ssaTab->versionStTable.GetZeroVersionSt(ae->ost))));
  }
}

void AliasClass::CollectMayDefForDassign(const StmtNode *stmt, std::set<OriginalSt *> &mayDefOsts) {
  MayDefPartWithVersionSt *theSSAPart = static_cast<MayDefPartWithVersionSt *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
  AliasElem *lhsAe = osym2Elem.at(theSSAPart->ssaVar->ost->index.idx);
  if (lhsAe->classSet == nullptr) {
    return;
  }

  OriginalSt *ostOfLhsAe = lhsAe->ost;
  MIRType *lhsAeType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ostOfLhsAe->tyIdx);
  MIRType *lhsSymType = ostOfLhsAe->symOrPreg.mirSt->GetType();
  for (uint elemId : *(lhsAe->classSet)) {
    if (elemId == lhsAe->id) {
      continue;
    }
    OriginalSt *ostOfAliasAe = id2Elem[elemId]->ost;
    if (!mirModule->IsCModule()) {
      if (ostOfAliasAe->tyIdx != ostOfLhsAe->tyIdx) {
        continue;
      }
    } else {
      if (ostOfAliasAe->symOrPreg.mirSt == ostOfLhsAe->symOrPreg.mirSt &&
          ostOfAliasAe->fieldID != ostOfLhsAe->fieldID &&
          lhsSymType->typeKind != kTypeUnion) {
        MIRType *aliasAeType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ostOfAliasAe->tyIdx);
        if (!lhsAeType->HasFields() && !aliasAeType->HasFields()) {
          continue;
        }
        if (lhsAeType->HasFields()) {
          if (ostOfAliasAe->fieldID < ostOfLhsAe->fieldID ||
              ostOfAliasAe->fieldID > (ostOfLhsAe->fieldID + (int32)lhsAeType->NumberOfFieldIDs())) {
            if (!aliasAeType->HasFields()) {
              continue;
            }
          }
        }
        if (aliasAeType->HasFields()) {
          if (ostOfLhsAe->fieldID < ostOfAliasAe->fieldID ||
              ostOfLhsAe->fieldID > (ostOfAliasAe->fieldID + (int32)aliasAeType->NumberOfFieldIDs())) {
            if (!lhsAeType->HasFields()) {
              continue;
            }
          }
        }
      }
    }
    mayDefOsts.insert(ostOfAliasAe);
  }
}

void AliasClass::InsertMayDefNode(std::set<OriginalSt *> &mayDefOsts, MapleMap<OStIdx, MayDefNode> *mayDefNodes,
                                  StmtNode *stmt, BBId bbid) {
  for (OriginalSt *mayDefOst : mayDefOsts) {
    mayDefNodes->insert(std::make_pair(
      mayDefOst->index, MayDefNode(ssaTab->versionStTable.GetZeroVersionSt(mayDefOst), stmt)));
    ssaTab->AddDefBB4Ost(mayDefOst->index, bbid);
  }
}

void AliasClass::InsertMayDefDassign(StmtNode *stmt, BBId bbid) {
  std::set<OriginalSt *> mayDefOsts;
  CollectMayDefForDassign(stmt, mayDefOsts);
  MapleMap<OStIdx, MayDefNode> *mayDefNodes = SSAGenericGetMayDefNodes(stmt, &ssaTab->stmtsSSAPart);
  InsertMayDefNode(mayDefOsts, mayDefNodes, stmt, bbid);
}

void AliasClass::CollectMayDefForIassign(StmtNode *stmt, std::set<OriginalSt *> &mayDefOsts) {
  IassignNode *iass = static_cast<IassignNode *>(stmt);
  AliasInfo baseAinfo = CreateAliasElemsExpr(iass->addrExpr);
  AliasElem *lhsAe = nullptr;
  if (baseAinfo.ae != nullptr &&
      (!mirModule->IsCModule() || iass->fieldID == 0 || baseAinfo.ae->ost->indirectLev == -1 || baseAinfo.ae->ost->tyIdx == iass->tyIdx)) {
    // get the next-level-ost that will be assigned to
    FieldID fieldIDUsed = iass->fieldID + baseAinfo.fieldID;
    if (mirModule->IsCModule() && baseAinfo.ae->ost->tyIdx != iass->tyIdx) {
      fieldIDUsed = 0;
    }
    OriginalSt *lhsOst = nullptr;
    for (OriginalSt *nextLevelNode : baseAinfo.ae->ost->nextlevelnodes) {
      if (nextLevelNode->fieldID == fieldIDUsed) {
        lhsOst = nextLevelNode;
        break;
      }
    }
    ASSERT(lhsOst != nullptr, "AliasClass::InsertMayUseExpr: cannot find next level ost");
    lhsAe = osym2Elem.at(lhsOst->index.idx);
  } else {
    lhsAe = FindOrCreateDummyNADSAe();
  }
  // lhsAe does not alias with any ae
  if (lhsAe->classSet == nullptr) {
    mayDefOsts.insert(lhsAe->ost);
    return;
  }
  OriginalSt *ostOfLhsAe = lhsAe->ost;
  TyIdx pointedTyIdx = ostOfLhsAe->tyIdx;
  MIRType *lhsAeType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointedTyIdx);
  for (uint elemId : *(lhsAe->classSet)) {
    AliasElem *ae = id2Elem[elemId];
    OriginalSt *ostOfAliasAe = ae->ost;
    if (!mirModule->IsCModule()) {
      if ((ostOfAliasAe->indirectLev == 0) && OriginalStIsAuto(ostOfAliasAe)) {
        continue;
      }
      if (ostOfAliasAe->tyIdx != pointedTyIdx && pointedTyIdx != 0) {
        continue;
      }
    } else {
      if (ostOfAliasAe->symOrPreg.mirSt == ostOfLhsAe->symOrPreg.mirSt &&
          ostOfAliasAe->fieldID != ostOfLhsAe->fieldID &&
          ostOfAliasAe->fieldID != 0 && ostOfLhsAe->fieldID != 0) {
        MIRType *aliasAeType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ostOfAliasAe->tyIdx);
        if (!lhsAeType->HasFields() && !aliasAeType->HasFields()) {
          continue;
        }
        if (lhsAeType->HasFields()) {
          if (ostOfAliasAe->fieldID < ostOfLhsAe->fieldID ||
              ostOfAliasAe->fieldID > (ostOfLhsAe->fieldID + (int32)lhsAeType->NumberOfFieldIDs())) {
            continue;
          }
        } else {
          if (ostOfLhsAe->fieldID < ostOfAliasAe->fieldID ||
              ostOfLhsAe->fieldID > (ostOfAliasAe->fieldID + (int32)aliasAeType->NumberOfFieldIDs())) {
            continue;
          }
        }
      }
    }
    mayDefOsts.insert(ostOfAliasAe);
  }
  CHECK_FATAL(!mayDefOsts.empty(), "CollectMayDefForIassign:: iassign cannot have empty mayDef");
}

void AliasClass::InsertMayDefNodeExcludeFinalOst(std::set<OriginalSt *> &mayDefOsts,
                                                 MapleMap<OStIdx, MayDefNode> *mayDefNodes, StmtNode *stmt,
                                                 BBId bbid) {
  for (OriginalSt *mayDefOst : mayDefOsts) {
    if (!mayDefOst->isFinal) {
      mayDefNodes->insert(std::make_pair(
        mayDefOst->index, MayDefNode(ssaTab->versionStTable.GetZeroVersionSt(mayDefOst), stmt)));
      ssaTab->AddDefBB4Ost(mayDefOst->index, bbid);
    }
  }
}

void AliasClass::InsertMayDefIassign(StmtNode *stmt, BBId bbid) {
  std::set<OriginalSt *> mayDefOsts;
  CollectMayDefForIassign(stmt, mayDefOsts);

  MapleMap<OStIdx, MayDefNode> *mayDefNodes = SSAGenericGetMayDefNodes(stmt, &ssaTab->stmtsSSAPart);

  if (mayDefOsts.size() == 1) {
    InsertMayDefNode(mayDefOsts, mayDefNodes, stmt, bbid);
  } else {
    InsertMayDefNodeExcludeFinalOst(mayDefOsts, mayDefNodes, stmt, bbid);
  }
  ASSERT(!mayDefNodes->empty(), "AliasClass::InsertMayUseIassign(): iassign cannot have empty mayDef");
  // go thru inserted MayDefNode to add the base info
  MapleMap<OStIdx, MayDefNode>::iterator it = mayDefNodes->begin();
  for (; it != mayDefNodes->end(); it++) {
    MayDefNode &mayDef = it->second;
    OriginalSt *ost = mayDef.result->ost;
    if (ost->indirectLev == 1) {
      mayDef.base = ssaTab->versionStTable.GetZeroVersionSt(ost->prevlevelnode);
    }
  }
}

void AliasClass::InsertMayDefUseSyncops(StmtNode *stmt, BBId bbid) {
  std::set<uint> aliasset;
  // collect the full alias set first
  for (int32 i = 0; i < stmt->NumOpnds(); i++) {
    BaseNode *addrbase = stmt->Opnd(i);
    if (addrbase->op == OP_addrof || addrbase->op == OP_dread || addrbase->op == OP_regread) {
      OriginalSt *ost;
      if (addrbase->op == OP_regread) {
        RegreadSSANode *n = static_cast<RegreadSSANode *>(addrbase);
        ost = n->ssaVar->ost;
      } else {
        AddrofSSANode *addrof = static_cast<AddrofSSANode *>(addrbase);
        ost = addrof->ssaVar->ost;
      }
      if (addrbase->op == OP_addrof) {
      } else {
        // addrbase->op == OP_dread or OP_regread
        for (OriginalSt *nextLevelOst : ost->nextlevelnodes) {
          AliasElem *opndAe = osym2Elem[nextLevelOst->index.idx];
          if (opndAe->classSet != nullptr) {
            aliasset.insert(opndAe->classSet->cbegin(), opndAe->classSet->cend());
          }
        }
      }
    } else {
      for (AliasElem *notAllDefsSeenAe : notAllDefsSeenClassSetRoots) {
        if (notAllDefsSeenAe->classSet != nullptr) {
          aliasset.insert(notAllDefsSeenAe->classSet->cbegin(), notAllDefsSeenAe->classSet->cend());
        } else if (notAllDefsSeenAe->ost != nullptr) {
          aliasset.insert(notAllDefsSeenAe->id);
        }
      }
    }
  }
  // do the insertion according to aliasset
  MayDefMayUsePart *theSSAPart = static_cast<MayDefMayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));

  for (uint elemid : aliasset) {
    AliasElem *ae = id2Elem[elemid];
    OriginalSt *ostOfAliasAe = ae->ost;
    if (ostOfAliasAe->indirectLev == 0 && OriginalStIsAuto(ostOfAliasAe)) {
      continue;
    }
    if (ostOfAliasAe->isFinal) {
      continue;
    }
    OStIdx ostIdx = ostOfAliasAe->index;
    VersionSt *vst0 = ssaTab->versionStTable.GetZeroVersionSt(ostOfAliasAe);
    theSSAPart->mayUseNodes.insert(std::make_pair(ostIdx, MayUseNode(vst0)));
    theSSAPart->mayDefNodes.insert(std::make_pair(ostIdx, MayDefNode(vst0, stmt)));
    ssaTab->AddDefBB4Ost(ostOfAliasAe->index, bbid);
  }
}

// collect mayDefs caused by mustDefs
void AliasClass::CollectMayDefForMustDefs(const StmtNode *stmt, std::set<OriginalSt *> &mayDefOsts) {
  MapleVector<MustDefNode> *mustDefs = SSAGenericGetMustDefNode(stmt, &ssaTab->stmtsSSAPart);
  for (MustDefNode mustDef : *mustDefs) {
    VersionSt *vst = mustDef.result;
    OriginalSt *ost = vst->ost;
    AliasElem *lhsAe = osym2Elem[ost->index.idx];
    if (lhsAe->classSet == nullptr) {
      continue;
    }
    for (uint elemId : *(lhsAe->classSet)) {
      bool isNotAllDefsSeen = false;
      for (AliasElem *notAllDefsSeenAe : notAllDefsSeenClassSetRoots) {
        if (notAllDefsSeenAe->classSet->find(elemId) != notAllDefsSeenAe->classSet->end()) {
          isNotAllDefsSeen = true;
          break;  // inserted already
        }
      }
      if (isNotAllDefsSeen) {
        continue;
      }
      if (elemId == lhsAe->id) {
        continue;  // don't insert itself
      }
      AliasElem *ae = id2Elem[elemId];
      OriginalSt *ostOfLhsAe = lhsAe->ost;
      if (ae->ost->tyIdx != ostOfLhsAe->GetMIRSymbol()->tyIdx) {
        continue;
      }
      mayDefOsts.insert(ae->ost);
    }
  }
}

void AliasClass::CollectMayUseForCallOpnd(StmtNode *stmt, std::set<OriginalSt *> &mayUseOsts) {
  for (int32 i = 0; i < stmt->NumOpnds(); i++) {
    BaseNode *expr = stmt->Opnd(i);
    InsertMayUseExpr(expr);
    AliasInfo aInfo = CreateAliasElemsExpr(expr);
    if (!IsPotentialAddress(expr->primType, mirModule) || aInfo.ae == nullptr) {
      continue;
    }
    if (aInfo.ae->nextLevNotAllDefsSeen && aInfo.ae->ost->indirectLev > 0) {
      continue;
    }
    if (GlobalTables::GetTypeTable().typeTable[aInfo.ae->ost->tyIdx.GetIdx()]->PointsToConstString()) {
      continue;
    }
    for (OriginalSt *nextLevelOst : aInfo.ae->ost->nextlevelnodes) {
      AliasElem *indAe = FindAliasElem(nextLevelOst);
      if (!indAe->ost->isFinal || finalFieldAlias) {
        if (indAe->classSet == nullptr) {
          mayUseOsts.insert(indAe->ost);
        } else {
          for (uint elemId : *(indAe->classSet)) {
            mayUseOsts.insert(id2Elem[elemId]->ost);
          }
        }
      }
    }
  }
}

void AliasClass::InsertMayDefNodeForCall(std::set<OriginalSt *> &mayDefOsts,
                                         MapleMap<OStIdx, MayDefNode> *mayDefNodes, StmtNode *stmt, BBId bbid,
                                         bool hasNoPrivateDefEffect) {
  for (OriginalSt *mayDefOst : mayDefOsts) {
    if (!hasNoPrivateDefEffect || !mayDefOst->isPrivate) {
      mayDefNodes->insert(std::make_pair(
        mayDefOst->index, MayDefNode(ssaTab->versionStTable.GetZeroVersionSt(mayDefOst), stmt)));
      ssaTab->AddDefBB4Ost(mayDefOst->index, bbid);
    }
  }
}

// Insert mayDefs and mayUses for the callees.
// Four kinds of mayDefs and mayUses are inserted, which are caused by callee
// opnds, not_all_def_seen_ae, globalsAffectedByCalls, and mustDefs.
void AliasClass::InsertMayDefUseCall(StmtNode *stmt, BBId bbid, bool hasSideEffect, bool hasNoPrivateDefEffect) {
  MayDefMayUsePart *theSSAPart = static_cast<MayDefMayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
  std::set<OriginalSt *> mayDefUseOstsA;
  // 1. collect mayDefs and mayUses caused by callee-opnds
  CollectMayUseForCallOpnd(stmt, mayDefUseOstsA);
  // 2. collect mayDefs and mayUses caused by not_all_def_seen_ae
  CollectMayUseFromNADS(mayDefUseOstsA);

  InsertMayUseNode(mayDefUseOstsA, &(theSSAPart->mayUseNodes));
  // insert may def node, if the callee has side-effect.
  if (hasSideEffect) {
    InsertMayDefNodeForCall(mayDefUseOstsA, &(theSSAPart->mayDefNodes), stmt, bbid, hasNoPrivateDefEffect);
  }
  // 3. insert mayDefs and mayUses caused by globalsAffectedByCalls
  std::set<OriginalSt *> mayDefUseOstsB;
  CollectMayUseFromGlobalsAffectedByCalls(mayDefUseOstsB);

  InsertMayUseNode(mayDefUseOstsB, &(theSSAPart->mayUseNodes));
  // insert may def node, if the callee has side-effect.
  if (hasSideEffect) {
    InsertMayDefNodeExcludeFinalOst(mayDefUseOstsB, &(theSSAPart->mayDefNodes), stmt, bbid);
  }

  if (!kOpcodeInfo.IsCallAssigned(stmt->op)) {
    return;
  }
  // 4. insert mayDefs caused by the mustDefs
  if (hasSideEffect) {
    std::set<OriginalSt *> mayDefOstsC;
    CollectMayDefForMustDefs(stmt, mayDefOstsC);
    InsertMayDefNodeExcludeFinalOst(mayDefOstsC, &(theSSAPart->mayDefNodes), stmt, bbid);
  }
}

void AliasClass::InsertMayUseNodeExcludeFinalOst(std::set<OriginalSt *> &mayUseOsts,
                                                 MapleMap<OStIdx, MayUseNode> *mayUseNodes) {
  for (OriginalSt *mayUseOst : mayUseOsts) {
    if (!mayUseOst->isFinal) {
      mayUseNodes->insert(std::make_pair(
        mayUseOst->index, MayUseNode(ssaTab->versionStTable.GetZeroVersionSt(mayUseOst))));
    }
  }
}

// Insert mayDefs and mayUses for intrinsiccall.
// Four kinds of mayDefs and mayUses are inserted, which are caused by callee
// opnds, not_all_def_seen_ae, globalsAffectedByCalls, and mustDefs.
void AliasClass::InsertMayDefUseIntrncall(StmtNode *stmt, BBId bbid) {
  MayDefMayUsePart *theSSAPart = static_cast<MayDefMayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
  IntrinsiccallNode *innode = static_cast<IntrinsiccallNode *>(stmt);
  IntrinDesc *intrindesc = &IntrinDesc::intrintable[innode->intrinsic];

  std::set<OriginalSt *> mayDefUseOsts;
  // 1. insert mayUses caused by intrinsiccall opnds
  if (!mirModule->IsCModule()) {
    for (int32 i = 0; i < stmt->NumOpnds(); i++) {
      InsertMayUseExpr(stmt->Opnd(i));
    }
  } else {
    CollectMayUseForCallOpnd(stmt, mayDefUseOsts);
  }

  // 2. collect mayDefs and mayUses caused by not_all_defs_seen_ae
  CollectMayUseFromNADS(mayDefUseOsts);
  // 3. collect mayDefs and mayUses caused by globalsAffectedByCalls
  CollectMayUseFromGlobalsAffectedByCalls(mayDefUseOsts);

  InsertMayUseNodeExcludeFinalOst(mayDefUseOsts, &(theSSAPart->mayUseNodes));
  if (!intrindesc->HasNoSideEffect() || calleeHasSideEffect) {
    InsertMayDefNodeExcludeFinalOst(mayDefUseOsts, &(theSSAPart->mayDefNodes), stmt, bbid);
  }

  if (!kOpcodeInfo.IsCallAssigned(stmt->op)) {
    return;
  }
  // 4. insert maydefs caused by the mustdefs
  std::set<OriginalSt *> mayDefOsts;
  CollectMayDefForMustDefs(stmt, mayDefOsts);
  InsertMayDefNodeExcludeFinalOst(mayDefOsts, &(theSSAPart->mayDefNodes), stmt, bbid);
}

void AliasClass::InsertMayDefUseClinitCheck(IntrinsiccallNode *stmt, BBId bbid) {
  MayDefMayUsePart *theSSAPart = static_cast<MayDefMayUsePart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
  for (OStIdx ostIdx : globalsMayAffectedByClinitCheck) {
    AliasElem *ae = osym2Elem[ostIdx.idx];
    if (ae->ost != nullptr) {
      OriginalSt *ostOfAe = ae->ost;
      std::string typeNameOfOst = ostOfAe->GetMIRSymbol()->GetName();
      std::string typeNameOfStmt = GlobalTables::GetTypeTable().GetTypeFromTyIdx(stmt->tyIdx)->GetName();
      if (typeNameOfOst.find(typeNameOfStmt) < typeNameOfOst.length()) {
        theSSAPart->mayDefNodes.insert(std::make_pair(
          ostOfAe->index, MayDefNode(ssaTab->versionStTable.GetZeroVersionSt(ostOfAe), stmt)));
        ssaTab->AddDefBB4Ost(ostOfAe->index, bbid);
      }
    }
  }
  return;
}

void AliasClass::GenericInsertMayDefUse(StmtNode *stmt, BBId bbid) {
  switch (stmt->op) {
    case OP_return: {
      InsertMayUseReturn(stmt);
      for (int32 i = 0; i < stmt->NumOpnds(); i++) {
        InsertMayUseExpr(stmt->Opnd(i));
      }
      // insert mayuses caused by its return operand being a pointer
      InsertReturnOpndMayUse(stmt);
      break;
    }
    case OP_throw:
      if (mirModule->srcLang != kSrcLangJs && lessThrowAlias) {
        CHECK_FATAL(GetBB(bbid) != nullptr, "GetBB(bbid) is nullptr in AliasClass::GenericInsertMayDefUse");
        if (!GetBB(bbid)->IsGoto()) {
          InsertMayUseReturn(stmt);
        }
        // if the throw is handled as goto, no alias consequence
      } else {
        InsertMayUseAll(stmt);
      }
      break;
    case OP_gosub:
    case OP_retsub:
      InsertMayUseAll(stmt);
      break;
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_icallassigned:
    case OP_call:
    case OP_virtualcall:
    case OP_virtualicall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_interfaceicall:
    case OP_customcall:
    case OP_polymorphiccall:
    case OP_icall: {
      bool hassideeffect = CallHasSideEffect(stmt);
      bool hasnoprivatedefeffect = CallHasNoPrivateDefEffect(stmt);
      InsertMayDefUseCall(stmt, bbid, hassideeffect, hasnoprivatedefeffect);
      break;
    }
    // Todo: needs to review and update intrinsic sideeffect description.
    case OP_intrinsiccallwithtype: {
      IntrinsiccallNode *innode = static_cast<IntrinsiccallNode *>(stmt);
      if (innode->intrinsic == INTRN_JAVA_CLINIT_CHECK) {
        InsertMayDefUseClinitCheck(innode, bbid);
      }
    }
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned: {
      InsertMayDefUseIntrncall(stmt, bbid);
      break;
    }
    case OP_maydassign:
    case OP_dassign: {
      InsertMayUseExpr(stmt->Opnd(0));
      InsertMayDefDassign(stmt, bbid);
      break;
    }
    case OP_regassign: {
      InsertMayUseExpr(stmt->Opnd(0));
      break;
    }
    case OP_iassign: {
      InsertMayUseExpr(stmt->Opnd(0));
      InsertMayUseExpr(stmt->Opnd(1));
      InsertMayDefIassign(stmt, bbid);
      break;
    }
    case OP_syncenter:
    case OP_syncexit:
      InsertMayDefUseSyncops(stmt, bbid);
    // fall-through
    default:
      for (int32 i = 0; i < stmt->NumOpnds(); i++) {
        BaseNode *argexpr = stmt->Opnd(i);
        InsertMayUseExpr(argexpr);
      }
      break;
  }
}

}  // namespace maple
