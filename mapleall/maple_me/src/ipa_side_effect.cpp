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

#include "ipa_side_effect.h"
#include "me_function.h"
#include "ver_symbol.h"
#include "dominance.h"
#include <jnimethod.h>
#include "me_ir.h"
#include "me_phase.h"
#include "me_irmap.h"
#include "dominance.h"

static FuncWithSideEffect mrtList[]{
#include <mrtinfo.def>
};

static std::unordered_map<MIRFunction *, std::pair<CGNode *, uint32>> cgNodeSccIdMap;

namespace maple {
static uint32 mrtListSz = 0;
static map<PUIdx, uint8> mrtPuIdx;
static map<PUIdx, uint8> jniPuIdx;
// keep track of union of all side effects in a SCC
static map<uint32, uint8> sccSe;

IpaSideEffect::IpaSideEffect(MeFunction *mf, MemPool *mp, CallGraph *callg, Dominance *dom)
  : notPure(false),
    hasUse(false),
    hasDef(false),
    hasRetallocobj(false),
    hasThrexception(false),
    hasPrivateUse(false),
    hasPrivateDef(false),
    mefunc(mf),
    alloc(mp),
    callgraph(callg),
    sccId(0),
    dominance(dom) {}

void IpaSideEffect::SetEffectsTrue() {
  notPure = hasUse = hasDef = hasRetallocobj = hasThrexception = hasPrivateUse = hasPrivateDef = true;
}

bool IpaSideEffect::IsIgnoreMethod(const MIRFunction *func) {
  // Ignore if virtual or no method
  if (func->IsAbstract()) {
    return true;
  }
  Klass *klass = callgraph->GetKlassh()->GetKlassFromFunc(func);
  if (klass == nullptr) {
    // An array, must have method, but has all effects
    SetEffectsTrue();
    return true;
  }
  auto methods = klass->GetMethods();
  for (auto it = methods.begin(); it != methods.end(); it++) {
    MIRFunction *methodFunc = *it;
    if (methodFunc == func) {
      return false;
    }
  }
  // Not found in methods
  return true;
}

void IpaSideEffect::CopySccSideEffectToAllFunctions(MIRFunction *mirFunc, uint8 seMask) {
  // For all members of the SCC, copy the sum of the side effect of SCC to each func member
  CGNode *cgnode = cgNodeSccIdMap[mirFunc].first;
  if (cgnode == nullptr) {
    CHECK_FATAL(false, "SetSccSideEffectToFunction: SCC not found");
  }
  SCCNode *sccn = cgnode->GetSCCNode();
  for (auto sccIt = sccn->cgNodes.begin(); sccIt != sccn->cgNodes.end(); sccIt++) {
    CGNode *memberCgnode = *sccIt;
    MIRFunction *memberFunc = memberCgnode->GetMIRFunction();

    memberFunc->SetIpaSeen();
    if (seMask & KNotPure) {
      memberFunc->UnsetPure();
    }
    if (seMask & kHasDef) {
      memberFunc->UnsetNoDefEffect();
    }
    if (seMask & kHasThrow) {
      memberFunc->UnsetNoThrowException();
    }
    if (seMask & kHasPrivateDef) {
      memberFunc->UnsetNoPrivateDefEffect();
    }
  }
}

void IpaSideEffect::SetFuncCalleeSideEffects(MIRFunction *callee, const MIRFunction *caller) {
  uint32 calleeScc = GetSCCNodeId(callee);
  if (IsCallingIntoSCC(calleeScc)) {
    // Call graph ensures that all methods in SCC are visited
    // before a call into the SCC.
    /* This is a call into the SCC. */
    auto it = sccSe.find(calleeScc);
    if (it == sccSe.end()) {
      // LogInfo::MapleLogger() << "from scc " << sccId << " to " << callee_scc << "\n";
      /* This is an imported SCC.  All calls from this SCC are also imported.
       * Need to create a new sccSe node for this SCC.
       */
      uint8 mask = 0;
      if (!callee->IsPure()) {
        mask |= KNotPure;
      }
      if (!callee->IsNoDefEffect()) {
        mask |= kHasDef;
      }
      if (!callee->IsNoThrowException()) {
        mask |= kHasThrow;
      }
      if (!callee->IsNoPrivateDefEffect()) {
        mask |= kHasPrivateDef;
      }
      sccSe[calleeScc] = mask;
    } else {
      if (!MeOption::quiet) {
        LogInfo::MapleLogger() << "CALL_INTO " << calleeScc << "\n";
      }
      uint8 mask = it->second;
      if (mask & KNotPure) {
        notPure = true;
      }
      if (mask & kHasUse) {
        hasUse = true;
      }
      if (mask & kHasDef) {
        hasDef = true;
      }
      if (mask & kHasObj) {
        hasRetallocobj = true;
      }
      if (mask & kHasThrow) {
        hasThrexception = true;
      }
      if (mask & kHasPrivateUse) {
        hasPrivateUse = true;
      }
      if (mask & kHasPrivateDef) {
        hasPrivateDef = true;
      }

      /* Since it is a call into this SCC, based on bottom up call graph,
       * all successors for the caller must be complete.  Copy side effect
       * info to all functions inside this SCC. When import mplt, all
       * functions within the SCC will have the most conservative side
       * effect info for the entire SCC.
       */
      CopySccSideEffectToAllFunctions(callee, mask);
    }
  } else if (callee->IsIpaSeen()) {
    if (!callee->IsPure()) {
      notPure = true;
    }
    if (!callee->IsNoDefEffect()) {
      hasDef = true;
    }
    if (!callee->IsNoThrowException()) {
      hasThrexception = true;
    }
    if (!callee->IsNoPrivateDefEffect()) {
      hasPrivateDef = true;
    }
  } else if (MatchPuidxAndSetSideEffects(callee->puIdx) == false) {
    /* function was not compiled before and it is not one of the predetermined
     * sideeffect functions, then assume
     *  - native function : no side effects
     *  - non-native function : all side effects
     */
    if (callee->IsNative() == false) {
      SetEffectsTrue();
      return;
    }
    /* If the caller and callee are in the same SCC, then its ok, since
     * the side effect of SCC is the sum of the methods in the SCC. The
     * side effect of the not yet visited method will be added in the
     * combined SCC side effects later when it is actually visited.
     */
    if (callee->GetName() == caller->GetName()) {
      return;
    }
    // !!!!!!!!!!!!MUST PRINT WARNING!!!!!!!!!!!
    if (calleeScc != sccId) {
      LogInfo::MapleLogger() << "WARNING: cross scc default no side effects " << callee->GetName() << " Native-function\n";
    }
    /* The order of the calls is not guarantee in a cycle, so ignore same scc warning.
           else if ( ! MeOption::quiet ) {
           LogInfo::MapleLogger() << "WARNING: same scc default no side effects " << callee->GetName();
           }
     */
  }
}

void IpaSideEffect::AnalyzeUseThrowEhExpr(MeExpr *expr) {
  Opcode op = expr->op;
  for (int32 i = 0; i < expr->NumMeExprOpnds(); i++) {
    AnalyzeUseThrowEhExpr(expr->GetOpnd(i));
  }
  switch (op) {
    case OP_iread: {
      hasThrexception = true;
      IvarMeExpr *ireadnode = static_cast<IvarMeExpr *>(expr);
      if (ireadnode->fieldID != 0) {
        // there is a use of an object
        hasUse = true;
        MIRType *basetype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ireadnode->tyIdx);
        MIRType *ptype = static_cast<MIRPtrType *>(basetype)->GetPointedType();
        if (ptype) {
          MIRStructType *structType = dynamic_cast<MIRStructType *>(ptype);
          if (structType) {
            FieldPair fldpair = structType->TraverseToField(ireadnode->fieldID);
            if (fldpair.second.second.GetAttr(FLDATTR_private)) {
              hasPrivateUse = true;
            }
          }
        }
      }
      break;
    }
    case OP_intrinsicop: {
      NaryMeExpr *intrnnode = static_cast<NaryMeExpr *>(expr);
      if (intrnnode->intrinsic == INTRN_JAVA_THROW_ARITHMETIC || intrnnode->intrinsic == INTRN_JAVA_THROW_CLASSCAST) {
        hasThrexception = true;
      }
      break;
    }
    case OP_div:
    case OP_rem:
    case OP_gcmalloc:
    case OP_gcmallocjarray: {
      hasThrexception = true;
      break;
    }
    default:
      break;
  }
}

bool IpaSideEffect::AnalyzeReturnAllocObjVst(MeExpr *baseExprMe, std::vector<MeExpr *> varVector) {
  CHECK_FATAL(baseExprMe->op == OP_dread, "Must be dread.");
  VarMeExpr *baseExpr = static_cast<VarMeExpr *>(baseExprMe);
  switch (baseExpr->defBy) {
    case kDefByStmt: {
      if (baseExpr->def.defStmt == nullptr) {
        return true;
      }
      MeStmt *defStmt = baseExpr->def.defStmt;
      CHECK_FATAL(defStmt->op == OP_dassign, "Must be dassign");
      DassignMeStmt *dassignMeStmt = static_cast<DassignMeStmt *>(defStmt);
      std::vector<MeExpr *>::iterator it = varVector.begin();
      for (; it != varVector.end(); it++) {
        // Cycle
        if (*it == dassignMeStmt->rhs) {
          break;
        }
      }
      if (it == varVector.end()) {
        varVector.push_back(dassignMeStmt->rhs);
        return (AnalyzeReturnAllocObj(dassignMeStmt->rhs, varVector));
      }
      return false;
    }
    case kDefByPhi: {
      for (unsigned int i = 0; i < baseExpr->def.defPhi->opnds.size(); i++) {
        VarMeExpr *expr = static_cast<VarMeExpr*>(baseExpr->def.defPhi->opnds[i]);
        std::vector<MeExpr *>::iterator it = varVector.begin();
        for (; it != varVector.end(); it++) {
          // Cycle
          if (*it == expr) {
            break;
          }
        }
        if (it == varVector.end()) {
          varVector.push_back(expr);
          if (AnalyzeReturnAllocObj(expr, varVector)) {
            return true;
          }
        }
      }
      return false;
    }
    case kDefByChi:
    case kDefByMustdef: {
      // Need to clarify further
      return true;
    }
    default: {
      return true;
    }
  }
}

bool IpaSideEffect::AnalyzeReturnAllocObj(MeExpr *expr, std::vector<MeExpr *> varVector) {
  Opcode op = expr->op;
  if (op == OP_dread) {
    VarMeExpr *drnode = static_cast<VarMeExpr *>(expr);
    std::vector<MeExpr *>::iterator it = varVector.begin();
    for (; it != varVector.end(); it++) {
      // Cycle
      if (*it == drnode) {
        break;
      }
    }
    if (it == varVector.end()) {
      varVector.push_back(drnode);
      if (drnode->primType == PTY_ref) {
        return AnalyzeReturnAllocObjVst(drnode, varVector);
      }
    }
  } else if (op == OP_gcmalloc || op == OP_gcmallocjarray) {
    return true;
  } else {
    for (int32 i = 0; i < expr->NumMeExprOpnds(); i++) {
      std::vector<MeExpr *>::iterator it = varVector.begin();
      for (; it != varVector.end(); it++) {
        // Cycle
        if (*it == expr->GetOpnd(i)) {
          break;
        }
      }
      if (it == varVector.end()) {
        varVector.push_back(expr->GetOpnd(i));
        if (AnalyzeReturnAllocObj(expr->GetOpnd(i), varVector)) {
          return true;
        }
      }
    }
  }
  return false;
}

bool IpaSideEffect::MEAnalyzeDefExpr(MeExpr *baseExprMe, std::vector<MeExpr *> &varVector) {
  CHECK_FATAL(baseExprMe->op == OP_dread, "Must be dread");
  VarMeExpr *baseExpr = static_cast<VarMeExpr *>(baseExprMe);
  const OriginalSt *ostSymbol = baseExpr->ost;
  if (!ostSymbol->isLocal) {
    return true;
  }

  switch (baseExpr->defBy) {
    case kDefByStmt: {
      if (baseExpr->def.defStmt == nullptr) {
        return true;
      }
      MeStmt *defStmt = baseExpr->def.defStmt;
      CHECK_FATAL(defStmt->op == OP_dassign, "Must be dassign");
      DassignMeStmt *dassignMeStmt = static_cast<DassignMeStmt *>(defStmt);
      switch (dassignMeStmt->rhs->op) {
        case OP_dread: {
          for (std::vector<MeExpr *>::iterator it = varVector.begin(); it != varVector.end(); it++) {
            // Cycle
            if (*it == dassignMeStmt->rhs) {
              return true;
            }
          }
          varVector.push_back(dassignMeStmt->rhs);
          if (MEAnalyzeDefExpr(dassignMeStmt->rhs, varVector)) {
            return true;
          }
          break;
        }
        case OP_gcmallocjarray:
        case OP_gcpermallocjarray:
        case OP_gcmalloc:
        case OP_constval: {
          return false;
        }
        case OP_iread: {
          return true;
        }
        case OP_cvt:
        case OP_retype: {
          OpMeExpr *node = static_cast<OpMeExpr *>(dassignMeStmt->rhs);
          CHECK_FATAL(node->GetOpnd(0)->op == OP_dread, "must be dread");
          for (std::vector<MeExpr *>::iterator it = varVector.begin(); it != varVector.end(); it++) {
            // Cycle
            if (*it == node->GetOpnd(0)) {
              return true;
            }
          }
          varVector.push_back(node->GetOpnd(0));
          if (MEAnalyzeDefExpr(node->GetOpnd(0), varVector)) {
            return true;
          }
          break;
        }
        default:
          CHECK_FATAL(false, "NYI");
      }
      break;
    }
    case kDefByPhi: {
      for (unsigned int i = 0; i < baseExpr->def.defPhi->opnds.size(); i++) {
        VarMeExpr *expr = static_cast<VarMeExpr*>(baseExpr->def.defPhi->opnds[i]);
        for (std::vector<MeExpr *>::iterator it = varVector.begin(); it != varVector.end(); it++) {
          // Cycle
          if (*it == baseExpr) {
            return true;
          }
        }
        varVector.push_back(expr);
        if (MEAnalyzeDefExpr(expr, varVector)) {
          return true;
        }
      }
      break;
    }
    case kDefByChi:
    case kDefByMustdef: {
      // Defined symbol is not a global
      return false;
    }
    default: {
      return true;
    }
  }
  return false;
}

// Def global variable or formal parameter
bool IpaSideEffect::AnalyzeDefExpr(VersionSt *baseVar, std::vector<VersionSt *> &varVector) {
  if (!baseVar->ost->isLocal) {
    return true;
  }

  switch (baseVar->defType) {
    case VersionSt::kDassign: {
      if (baseVar->defStmt.dassign == nullptr) {
        return true;
      }
      BaseNode *rhs = baseVar->defStmt.dassign->GetRhs();
      switch (rhs->op) {
        case OP_dread: {
          VersionSt *st = (static_cast<AddrofSSANode *>(rhs))->ssaVar;
          for (std::vector<VersionSt *>::iterator it = varVector.begin(); it != varVector.end(); it++) {
            // Cycle
            if (*it == st) {
              return true;
            }
          }
          varVector.push_back(st);
          if (AnalyzeDefExpr(st, varVector)) {
            return true;
          }
          break;
        }
        case OP_gcmallocjarray:
        case OP_gcmalloc:
        case OP_constval: {
          return false;
        }
        case OP_iread: {
          return true;
        }
        case OP_cvt:
        case OP_retype: {
          TypeCvtNode *node = static_cast<TypeCvtNode *>(rhs);
          CHECK_FATAL(node->uOpnd->op == OP_dread, "must be dread");
          VersionSt *st = (static_cast<AddrofSSANode *>(node->uOpnd))->ssaVar;
          for (std::vector<VersionSt *>::iterator it = varVector.begin(); it != varVector.end(); it++) {
            // Cycle
            if (*it == st) {
              return true;
            }
          }
          varVector.push_back(st);
          if (AnalyzeDefExpr(st, varVector)) {
            return true;
          }
          break;
        }
        default:
          CHECK_FATAL(false, "NYI");
      }
      break;
    }
    // case VersionSt::Regassign:
    case VersionSt::kPhi: {
      for (unsigned int i = 0; i < baseVar->defStmt.phi->phiOpnds.size(); i++) {
        VersionSt *st = baseVar->defStmt.phi->phiOpnds[i];
        for (std::vector<VersionSt *>::iterator it = varVector.begin(); it != varVector.end(); it++) {
          // Cycle
          if (*it == st) {
            return true;
          }
        }
        varVector.push_back(st);
        if (AnalyzeDefExpr(st, varVector)) {
          return true;
        }
      }
      break;
    }
    case VersionSt::kMayDef:
    case VersionSt::kMustDef: {
      return true;
    }
    default: {
      CHECK_FATAL(false, "NYI");
      break;
    }
  }
  return false;
}

/* ThrowException:
 *      throw, iread/iassign, div/rem(replaced by JAVA_THROW_ARITHMETIC, but actully not), gcmalloc/gcmallocjarray,
 *      mrt/jni function from manual file
 *      intrinsic call:
 *          JAVA_CHECK_CAST, JAVA_ARRAY_FILL, JAVA_FILL_NEW_ARRAY,
 *          JAVA_ARRAY_LENGTH/MPL_BOUNDARY_CHECK(replaced by iread/iassign), JAVA_THROW_ARITHMETIC
 *  Private_Def:
 *  Global/Formal Def:
 */
void IpaSideEffect::SideEffectAnalyzeBB(maple::BB *bb, vector<bool> &bbvisited) {
  if (bb == nullptr) {
    return;
  }
  BBId bbid = bb->id;
  if (bbvisited[bbid.idx]) {
    return;
  }
  bbvisited[bbid.idx] = true;
  for (auto mestmt : bb->meStmtList) {
    for (int32 i = 0; i < mestmt->NumMeStmtOpnds(); i++) {
      AnalyzeUseThrowEhExpr(mestmt->GetMeStmtOpnd(i));
    }
    Opcode op = mestmt->op;
    switch (op) {
      case OP_return: {
        NaryMeStmt *retstmt = static_cast<NaryMeStmt *>(mestmt);
        for (int32 i = 0; i < retstmt->NumMeStmtOpnds(); i++) {
          std::vector<MeExpr *> varVector;
          if (!hasRetallocobj && AnalyzeReturnAllocObj(retstmt->GetMeStmtOpnd(i), varVector)) {
            hasRetallocobj = true;
          }
        }
        break;
      }
      case OP_iassign: {
        hasThrexception = true;
        IassignMeStmt *iasnode = static_cast<IassignMeStmt *>(mestmt);
        CHECK_FATAL(iasnode->lhsVar->base->op == OP_dread || iasnode->lhsVar->base->op == OP_array, "Must be dread");
        MeExpr *baseNode = nullptr;
        if (iasnode->lhsVar->base->op == OP_array) {
          NaryMeExpr *arrayNode = static_cast<NaryMeExpr *>(iasnode->lhsVar->base);
          CHECK_FATAL(arrayNode->GetOpnd(0)->op == OP_dread, "Must be dread");
          baseNode = arrayNode->GetOpnd(0);
        } else {
          baseNode = iasnode->lhsVar->base;
        }
        VarMeExpr *dread = static_cast<VarMeExpr *>(baseNode);
        std::vector<MeExpr *> varVector;
        if (MEAnalyzeDefExpr(dread, varVector)) {
          hasDef = true;
          MIRType *basetype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(iasnode->tyIdx);
          MIRType *ptype = static_cast<MIRPtrType *>(basetype)->GetPointedType();
          if (ptype) {
            MIRStructType *structType = dynamic_cast<MIRStructType *>(ptype);
            if (structType) {
              FieldPair fldpair = structType->TraverseToField(iasnode->lhsVar->fieldID);
              if (fldpair.second.second.GetAttr(FLDATTR_private)) {
                hasPrivateDef = true;
              }
            }
          }
        }
        break;
      }
      case OP_throw: {
        hasThrexception = true;
        break;
      }
      case OP_intrinsiccall:
      case OP_intrinsiccallassigned:
      case OP_intrinsiccallwithtype:
      case OP_intrinsiccallwithtypeassigned: {
        IntrinsiccallMeStmt *callnode = dynamic_cast<IntrinsiccallMeStmt *>(mestmt);
        CHECK_FATAL(callnode != nullptr, "null ptr check");
        if (callnode->intrinsic == INTRN_JAVA_CHECK_CAST || callnode->intrinsic == INTRN_JAVA_ARRAY_FILL ||
            callnode->intrinsic == INTRN_JAVA_FILL_NEW_ARRAY) {
          hasThrexception = true;
        }
        break;
      }
      case OP_dassign: {
        DassignMeStmt *dassignNode = static_cast<DassignMeStmt *>(mestmt);
        if (dassignNode->lhs->ost->GetMIRSymbol()->stIdx.IsGlobal()) {
          hasDef = true;
        }
        break;
      }
      case OP_maydassign: {
        MaydassignMeStmt *maydass = static_cast<MaydassignMeStmt *>(mestmt);
        if (maydass->GetLhsSym(mefunc->meSSATab)->stIdx.IsGlobal()) {
          hasDef = true;
        }
        break;
      }
      case OP_call:
      case OP_callassigned:
      case OP_superclasscall:
      case OP_superclasscallassigned:
      case OP_virtualcall:
      case OP_virtualicall:
      case OP_virtualcallassigned:
      case OP_virtualicallassigned:
      case OP_interfacecall:
      case OP_interfaceicall:
      case OP_interfacecallassigned:
      case OP_interfaceicallassigned:
      case OP_customcall:
      case OP_customcallassigned:
      case OP_polymorphiccall:
      case OP_polymorphiccallassigned:
      case OP_icall:
      case OP_icallassigned:
      case OP_xintrinsiccall:
      case OP_xintrinsiccallassigned:
      case OP_jscatch:
      case OP_finally:
      case OP_endtry:
      case OP_cleanuptry:
      case OP_membaracquire:
      case OP_membarrelease:
      case OP_membarstoreload:
      case OP_membarstorestore:
      case OP_retsub:
      case OP_gosub:
      case OP_goto:
      case OP_brfalse:
      case OP_brtrue:
      case OP_comment:
      case OP_jstry:
      case OP_try:
      case OP_javatry:
      case OP_cpptry:
      case OP_catch:
      case OP_javacatch:
      case OP_cppcatch:
      case OP_syncenter:
      case OP_syncexit:
      case OP_assertnonnull:
      case OP_eval:
      case OP_free:
      case OP_switch:
      case OP_igoto:
      case OP_label:
        break;
      default: {
        CHECK_FATAL(false, "NYI");
      }
    }
  }
  CHECK(bbid.idx < dominance->domChildren.size(), " index out of range in IRMap::BuildBB");
  MapleSet<BBId> *domChildren = &dominance->domChildren[bbid.idx];
  for (const BBId &bbid : *domChildren) {
    BBId childbbid = bbid;
    SideEffectAnalyzeBB(mefunc->theCFG->bbVec[childbbid.idx], bbvisited);
  }
}

void IpaSideEffect::SetEffectsForAllCallees(MIRFunction *baseFunc) {
  MIRFunction *parent = baseFunc;
  CGNode *callGraphNode = cgNodeSccIdMap[parent].first;
  if (callGraphNode == nullptr) {
    CHECK_FATAL(false, "SetEffectsForAllCallees - null cgnode");
  }
  MapleVector<Callsite>::iterator it;
  for (it = callGraphNode->CalleeBegin(); it != callGraphNode->CalleeEnd(); ++it) {
    CGNode *n = it->second;
    MIRFunction *callee = n->GetMIRFunction();
    if (IsIgnoreMethod(callee)) {
      continue;
    }
    if (!MeOption::quiet) {
      LogInfo::MapleLogger() << "\t--> " << callee->GetName() << "\n";
    }
    SetFuncCalleeSideEffects(callee, baseFunc);
    //    }
  }
}

bool IpaSideEffect::MatchPuidxAndSetSideEffects(PUIdx idx) {
  if (idx == 0) {
    return false;
  }
  auto mrtIt = mrtPuIdx.find(idx);
  if (mrtIt != mrtPuIdx.end()) {
    uint8 mrtSe = mrtIt->second;
    if (!notPure) {
      notPure = !(mrtSe & kPure);
    }
    if (!hasUse) {
      hasUse = mrtSe & kHasUse;
    }
    if (!hasDef) {
      hasDef = mrtSe & kHasDef;
    }
    if (!hasRetallocobj) {
      hasRetallocobj = mrtSe & kHasObj;
    }
    if (!hasThrexception) {
      hasThrexception = mrtSe & kHasThrow;
    }
    if (!hasPrivateUse) {
      hasPrivateUse = mrtSe & kHasPrivateUse;
    }
    if (!hasPrivateDef) {
      hasPrivateDef = mrtSe & kHasPrivateDef;
    }
    return true;
  }
  auto jniIt = jniPuIdx.find(idx);
  if (jniIt != jniPuIdx.end()) {
    uint8 jniSe = jniIt->second;
    if (!notPure) {
      notPure = !(jniSe & kPure);
    }
    if (!hasUse) {
      hasUse = jniSe & kHasUse;
    }
    if (!hasDef) {
      hasDef = jniSe & kHasDef;
    }
    if (!hasRetallocobj) {
      hasRetallocobj = jniSe & kHasObj;
    }
    if (!hasThrexception) {
      hasThrexception = jniSe & kHasThrow;
    }
    if (!hasPrivateUse) {
      hasPrivateUse = jniSe & kHasPrivateUse;
    }
    if (!hasPrivateDef) {
      hasPrivateDef = jniSe & kHasPrivateDef;
    }
    return true;
  }
  return false;
}

void IpaSideEffect::MapFuncNameToPuidx() {
  if (mrtListSz != 0) {
    return;
  }

  mrtListSz = sizeof(mrtList) / sizeof(FuncWithSideEffect);
  for (uint32 i = 0; i < mrtListSz; i++) {
    MIRSymbol *sym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(mrtList[i].GetFuncName()));
    if (sym) {
      uint8 seMask = 0;
      if (mrtList[i].GetPure()) {
        seMask |= kPure;
      }
      if (mrtList[i].GetUse()) {
        seMask |= kHasUse;
      }
      if (mrtList[i].GetDef()) {
        seMask |= kHasDef;
      }
      if (mrtList[i].GetObject()) {
        seMask |= kHasObj;
      }
      if (mrtList[i].GetException()) {
        seMask |= kHasThrow;
      }
      if (mrtList[i].GetPrivateUse()) {
        seMask |= kHasPrivateUse;
      }
      if (mrtList[i].GetPrivateDef()) {
        seMask |= kHasPrivateDef;
      }
      MIRFunction *func = sym->GetFunction();
      mrtPuIdx[func->puIdx] = seMask;
    }
  }
}

void IpaSideEffect::DumpFuncInfo(const std::string msg, const std::string funcname) {
  LogInfo::MapleLogger() << msg << ": ";
  MIRFunction *func;
  if (funcname.empty()) {
    func = mefunc->mirFunc;
  } else {
    MIRSymbol *sym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(funcname));
    func = sym->GetFunction();
  }
  uint32 effectSeen = func->IsIpaSeen() ? 1 : 0;
  uint32 effectPure = func->IsPure() ? 1 : 0;
  uint32 effectDef = func->IsNoDefEffect() ? 1 : 0;
  uint32 effectExc = func->IsNoThrowException() ? 1 : 0;
  uint32 effectPrivDef = func->IsNoPrivateDefEffect() ? 1 : 0;
  LogInfo::MapleLogger() << "IPA-Func name " << func->GetName() << " seen " << effectSeen << " pure " << effectPure
       << " !def " << effectDef << " !throw-exception " << effectExc
       << " !private_def " << effectPrivDef << "\n";
}

void IpaSideEffect::InitCGNodeSccIdMap() {
  if (!cgNodeSccIdMap.empty()) {
    return;     // already initialized
  }
  for (CGNode *cgNode : callgraph->GetNodesMap()) {
    if (cgNode == nullptr) {
      continue;
    }
    MIRFunction *func = cgNode->mirFunc;
    SCCNode *sccn = cgNode->GetSCCNode();
    uint32 sccid = (sccn != nullptr && sccn->GetCGNodes().size() > 1) ? sccn->id : 0;
    cgNodeSccIdMap[func] = std::make_pair(cgNode, sccid);
  }
}

uint32 IpaSideEffect::GetSCCNodeId(MIRFunction *func) {
  return cgNodeSccIdMap[func].second;
}

bool IpaSideEffect::IsCallingIntoSCC(uint32 id) const {
  if (id == 0) {
    return false;
  }
  if (sccId == 0) {
    return false;
  }
  return (id != sccId);
}

void IpaSideEffect::UpdateExternalFuncSideEffects(MIRFunction *externCaller) {
  if (IsIgnoreMethod(externCaller)) {
    return;
  }
  /* This is an external caller read in from an already processed mplt file.
   * Need to process all callee and update the caller side effects.
   */
  if (!externCaller->IsPure()) {
    notPure = true;
  }
  if (!externCaller->IsNoDefEffect()) {
    hasDef = true;
  }
  if (!externCaller->IsNoThrowException()) {
    hasThrexception = true;
  }
  if (!externCaller->IsNoPrivateDefEffect()) {
    hasPrivateDef = true;
  }

  CGNode *cgnode = cgNodeSccIdMap[externCaller].first;
  if (cgnode == nullptr) {
    CHECK_FATAL(false, "UpdateExternalFuncSideEffects: SCC not found");
  }
  for (auto it = cgnode->CalleeBegin(); it != cgnode->CalleeEnd(); it++) {
    MIRFunction *callee = it->second->GetMIRFunction();
    SetFuncCalleeSideEffects(callee, externCaller);
  }
}

// Trim call graph based on the result of devirtualization
void IpaSideEffect::TrimCallGraph(maple::BB *bb, vector<bool> &bbvisited) {
  if (bb == nullptr) {
    return;
  }
  BBId bbid = bb->id;
  CHECK_FATAL(bbid.idx < bbvisited.size(), "out of range in IpaSideEffect::TrimCallGraph");
  if (bbvisited[bbid.idx]) {
    return;
  }
  bbvisited[bbid.idx] = true;
  for (auto meStmt : bb->meStmtList) {
    Opcode op = meStmt->op;
    switch (op) {
      case OP_call:
      case OP_callassigned: {
        CallMeStmt *callMeStmt = static_cast<CallMeStmt *>(meStmt);
        uint32 stmtID = callMeStmt->stmtID;
        MIRFunction *calleeFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callMeStmt->puIdx);
        {
          // Maybe has been devirtualized. Don't seperate original call from devirtualized
          CGNode *callGraphNode = cgNodeSccIdMap[mefunc->mirFunc].first;
          CHECK_FATAL(callGraphNode != nullptr, "SetEffectsForAllCallees - null cgnode");
          MapleVector<Callsite>::iterator it;
          for (uint32 i = 0; i < callGraphNode->callees.size();) {
            Callsite callSite = callGraphNode->callees[i];
            CallInfo *callInfo = callSite.first;
            CGNode *cgNode = callSite.second;
            if (callInfo->GetID() == stmtID) {
              if (callInfo->GetFunc() == calleeFunc)
                ;
              else {
                callGraphNode->callees.erase(callGraphNode->callees.begin() + i);
                // LogInfo::MapleLogger() << "Delete one edge of call graph TrimCallGraph\n";
                continue;
              }
            }
            i++;
          }
          // CHECK_FATAL(found, "Must be found in call graph");
        }
        break;
      }
      default:
        break;
    }
  }
  CHECK(bbid.idx < dominance->domChildren.size(), " index out of range in IRMap::BuildBB");
  MapleSet<BBId> *domChildren = &dominance->domChildren[bbid.idx];
  for (MapleSet<BBId>::iterator bbit = domChildren->begin(); bbit != domChildren->end(); bbit++) {
    BBId childbbid = *bbit;
    TrimCallGraph(mefunc->theCFG->bbVec[childbbid.idx], bbvisited);
  }
}

void IpaSideEffect::DoAnalysis() {
  MapFuncNameToPuidx();
  BB *entrybb = mefunc->theCFG->commonEntryBB;
  MIRFunction *mirFunc = mefunc->mirFunc;
  sccId = GetSCCNodeId(mirFunc);
  if (mirFunc->body == nullptr) {
    // External function from mplt, need to update effects
    UpdateExternalFuncSideEffects(mirFunc);
  } else {
    vector<bool> bbvisited(mefunc->theCFG->bbVec.size(), false);
    // Trim call graph based on the result of devirtualization
    TrimCallGraph(entrybb, bbvisited);
    SetEffectsForAllCallees(mirFunc);
    if (!(notPure && hasUse && hasDef && hasRetallocobj && hasThrexception)) {
      vector<bool> bbvisited(mefunc->theCFG->bbVec.size(), false);
      SideEffectAnalyzeBB(entrybb, bbvisited);
    }
  }
  uint8 mask = 0;
  if (sccId != 0) {
    auto it = sccSe.find(sccId);
    if (it != sccSe.end()) {
      mask = it->second;
    }
  }

  if (notPure) {
    mask |= KNotPure;
  } else {
    mirFunc->SetPure();
  }
  if (hasUse) {
    mask |= kHasUse;
  }
  if (hasDef) {
    mask |= kHasDef;
  } else {
    mirFunc->SetNoDefEffect();
  }
  if (hasRetallocobj) {
    mask |= kHasObj;
  }
  if (hasThrexception) {
    mask |= kHasThrow;
  } else {
    mirFunc->SetNoThrowException();
  }
  if (hasPrivateUse) {
    mask |= kHasPrivateUse;
  }
  if (hasPrivateDef) {
    mask |= kHasPrivateDef;
  } else {
    mirFunc->SetNoPrivateDefEffect();
  }
  mirFunc->SetIpaSeen();

  if (sccId != 0) {
    sccSe[sccId] = mask;
  }
}

AnalysisResult *DoIpaSideEffect::Run(MeFunction *func, MeFuncResultMgr *mfrm, ModuleResultMgr *mrm) {
  MemPool *mp = mempoolctrler.NewMemPool("ipasideeffect mempool");
  Dominance *dom = static_cast<Dominance *>(mfrm->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
  CHECK_FATAL(dom != nullptr, "Dominance must be built.");

  CallGraph *callg = static_cast<CallGraph *>(mrm->GetAnalysisResult(MoPhase_CALLGRAPH_ANALYSIS, &func->mirModule));
  CHECK_FATAL(callg != nullptr, "Call graph must be built.");

  IpaSideEffect ipase(func, mp, callg, dom);
  ipase.InitCGNodeSccIdMap();
  ipase.DoAnalysis();

  mempoolctrler.DeleteMemPool(mp);
  return nullptr;
}

}  // namespace maple
