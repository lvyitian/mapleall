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

#include "prop.h"
#include "me_irmap.h"
#include "dominance.h"
#include "constant_fold.h"
#include "name_mangler.h"

#define JAVALANG (irMap->ssaTab->mirModule.IsJavaModule())

const int kPropTreeLevel = 15; // tree height threshold to increase to

// following cannot be assumed no-alias even though they are final
static const std::set<std::string> kStaticConstructorBlackList{ std::string(NameMangler::kJavaLang) + "System_3B" + NameMangler::kClinitSuffix };

namespace maple {

using namespace std;

Prop::Prop(IRMap *hmap, Dominance *dom, MemPool *mp, uint32 bbvecsize, bool propbase, bool propiloadref,
           bool propglobalref, bool propfinaliloadref, bool propiloadrefNonparm, bool propatphi)
  : irMap(hmap),
    ssaTab(hmap->ssaTab),
    mirModule(&hmap->ssaTab->mirModule),
    dominance(dom),
    prop_map_alloc(mp),
    vst_live_stack_vec(prop_map_alloc.Adapter()),
    bb_visited(bbvecsize, false, prop_map_alloc.Adapter()),
    curbb(nullptr),
    propagate_base(propbase),
    propagate_iload_ref(propiloadref),
    propagate_global_ref(propglobalref),
    propagate_final_iload_ref(propfinaliloadref),
    propagate_iload_ref_nonparm(propiloadrefNonparm),
    propagate_at_phi(propatphi) {
  const MapleVector<OriginalSt *> &originalStVec = ssaTab->originalStTable.original_st_vector_;
  vst_live_stack_vec.resize(originalStVec.size());
  for (uint32 i = 1; i < originalStVec.size(); i++) {
    OriginalSt *ost = originalStVec[i];
    ASSERT(ost->index == OStIdx(i), "inconsistent originalst_table index");
    MapleStack<MeExpr *> *verstStack = prop_map_alloc.GetMemPool()->New<MapleStack<MeExpr *>>(prop_map_alloc.Adapter());
    verstStack->push(hmap->GetMeExpr(ost->zeroVersionIndex));
    vst_live_stack_vec[i] = verstStack;
  }
}

void Prop::PropUpdateDef(MeExpr *meexpr) {
  ASSERT(meexpr->meOp == kMeOpVar || meexpr->meOp == kMeOpReg, "");
  OStIdx ostIdx;
  if (meexpr->meOp == kMeOpVar) {
    ostIdx = static_cast<VarMeExpr *>(meexpr)->ost->index;
  } else {
    ostIdx = static_cast<RegMeExpr *>(meexpr)->ost->index;
    if (static_cast<RegMeExpr *>(meexpr)->regIdx < 0) {
      return;
    }
  }
  MapleStack<MeExpr *> *pstack = vst_live_stack_vec.at(ostIdx.idx);
  pstack->push(meexpr);
}

void Prop::PropUpdateChiListDef(const MapleMap<OStIdx, ChiMeNode *> &chilist) {
  for (MapleMap<OStIdx, ChiMeNode *>::const_iterator it = chilist.begin(); it != chilist.end(); it++) {
    PropUpdateDef(static_cast<VarMeExpr *>(it->second->lhs));
  }
}

void Prop::PropUpdateMustDefList(MeStmt *mestmt) {
  MapleVector<MustDefMeNode> *mustdefList = mestmt->GetMustDefList();
  if (!mustdefList->empty()) {
    MeExpr *melhs = mustdefList->front().lhs;
    PropUpdateDef(static_cast<VarMeExpr *>(melhs));
  }
}

// make sure varvec can be released, otherwise this function may
// cause memory leak
void Prop::CollectSubVarMeExpr(MeExpr *meexpr, vector<MeExpr *> &varvec) {
  switch (meexpr->meOp) {
    case kMeOpReg:
    case kMeOpVar:
      varvec.push_back(meexpr);
      break;
    case kMeOpIvar: {
      IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(meexpr);
      if (ivarmeexpr->mu != nullptr) {
        varvec.push_back(ivarmeexpr->mu);
      }
      break;
    }
    default:
      break;
  }
}

// check at the current statement, if the version symbol is consistent with its definition in the top of the stack
// for example:
// x1 = a1 + b1;
// a2 <-
//  <-x1
// the version of progation of x1 is a1, but the top of the stack of symbol a is a2, so it's not consistent
// warning: I suppose the vector vervec is on the stack, otherwise would cause memory leak
bool Prop::IsVersionConsistent(const vector<MeExpr *> &vervec,
                               const MapleVector<MapleStack<MeExpr *> *> &vstLiveStack) const {
  for (vector<MeExpr *>::const_iterator it = vervec.begin(); it != vervec.end(); it++) {
    // iterate each cur defintion of related symbols of rhs, check the version
    MeExpr *subexpr = *it;
    CHECK_FATAL(subexpr->meOp == kMeOpVar || subexpr->meOp == kMeOpReg, "");
    uint32 stackidx = 0;
    stackidx = static_cast<ScalarMeExpr*>(subexpr)->ost->index.idx;

    MapleStack<MeExpr *> *pstack = vstLiveStack[stackidx];
    if (!pstack) {
      // no definition so far go ahead
      continue;
    }
    const MeExpr *curdef = pstack->top();
    CHECK_FATAL(!curdef || curdef->meOp == kMeOpVar || curdef->meOp == kMeOpReg, "");
    if (subexpr != curdef) {
      return false;
    }
  }
  return true;
}

bool Prop::IvarIsFinalField(const IvarMeExpr *ivarmeexpr) {
  if (!propagate_final_iload_ref) {
    return false;
  }
  if (InConstructorFunc() && GetFunc() && GetFunc()->mirFunc) {
    if (kStaticConstructorBlackList.find(GetFunc()->mirFunc->GetName()) != kStaticConstructorBlackList.end()) {
      return false;
    }
  }
  if (ivarmeexpr->fieldID == 0) {
    return false;
  }
  MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ivarmeexpr->tyIdx);
  ASSERT(ty->typeKind == kTypePointer, "IvarIsFinalField: pointer type expected");
  MIRType *pointedty = static_cast<MIRPtrType *>(ty)->GetPointedType();
  MIRStructType *structty = dynamic_cast<MIRStructType *>(pointedty);
  CHECK_FATAL(structty, "Prop::IvarIsFinalField: cannot have non-zero fieldID for something other than a struct");
  FieldAttrs attrs = structty->TraverseToField(ivarmeexpr->fieldID).second.second;
  return attrs.GetAttr(FLDATTR_final);
}

// if x contains operations that has no accurate inverse, return -1; also return
// -1 if x contains any scalar other than x that is not current version;
// otherwise, the return value is the number of occurrences of scalar.
int32 Prop::InvertibleOccurrences(ScalarMeExpr *scalar, MeExpr *x) {
  switch (x->meOp) {
  case kMeOpConst: return 0;
  case kMeOpReg: {
    RegMeExpr *regreadx = static_cast<RegMeExpr *>(scalar);
    if (regreadx->regIdx < 0) {
      return -1;
    }
  }
    // fall thru
  case kMeOpVar:
    if (x == scalar) {
      return 1;
    }
    if (Propagatable(x, nullptr, false, false, nullptr) == kPropYes) {
      return 0;
    }
    return -1;
  case kMeOpOp:
    if (!IsPrimitiveInteger(x->primType)) {
      return -1;
    }
    if (x->op == OP_neg) {
      return InvertibleOccurrences(scalar, x->GetOpnd(0));
    }
    if (x->op == OP_add || x->op == OP_sub) {
      int32 invertibleOccs0 = InvertibleOccurrences(scalar, x->GetOpnd(0));
      if (invertibleOccs0 == -1) {
        return -1;
      }
      int32 invertibleOccs1 = InvertibleOccurrences(scalar, x->GetOpnd(1));
      if (invertibleOccs1 == -1 || (invertibleOccs0 + invertibleOccs1 > 1)) {
        return -1;
      }
      return invertibleOccs0 + invertibleOccs1;
    }
    // fall thru
  default: return -1;
  }
}

// return true if scalar can be expressed as a function of current version cur
bool Prop::IsFunctionOfCurVersion(ScalarMeExpr *scalar, ScalarMeExpr *cur) {
  if (cur == nullptr || cur->defBy != kDefByStmt) {
    return false;
  }
  AssignMeStmt *ass = cur->def.defStmt;
  return InvertibleOccurrences(scalar, ass->rhs) == 1;
}

// check if the expression x can legally forward-substitute the variable that it
// was assigned to; x is from bb; if checkInverse is true and there is live range
// overlap for a scalar within x, do the additional check of whether the scalar's
// previous version can be expressed in terms of its current version.
// propagatingScalar is used only if checkInverse is true; it gives the
// propagating scalar so we can avoid doing the checkInverse checking for it.
Propagatability Prop::Propagatable(MeExpr *x, BB *frombb, bool atParm, bool checkInverse, ScalarMeExpr *propagatingScalar) {
  MeExprOp meOp = x->meOp;
  switch (meOp) {
    case kMeOpAddrof:
    case kMeOpAddroffunc:
    case kMeOpAddroflabel:
    case kMeOpConst:
    case kMeOpConststr:
    case kMeOpConststr16:
    case kMeOpSizeoftype:
      return kPropYes;
    case kMeOpGcmalloc:
      return kPropNo;
    case kMeOpNary: {
      if (x->op == OP_intrinsicop || x->op == OP_intrinsicopwithtype) {
        return kPropNo;
      }
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(x);
      Propagatability propmin = kPropYes;
      for (uint32 i = 0; i < narymeexpr->numOpnds; i++) {
        Propagatability prop = Propagatable(narymeexpr->GetOpnd(i), frombb, false, checkInverse, propagatingScalar);
        if (prop == kPropNo) {
          return kPropNo;
        }
        propmin = std::min(propmin, prop);
      }
      return propmin;
    }
    case kMeOpReg: {
      RegMeExpr *regreadx = static_cast<RegMeExpr *>(x);
      if (regreadx->regIdx < 0) {
        return kPropNo;
      } else {
        // get the current definition version
        vector<MeExpr *> regreadxVec;
        CollectSubVarMeExpr(x, regreadxVec);
        if (IsVersionConsistent(regreadxVec, vst_live_stack_vec)) {
          return kPropYes;
        } else if (checkInverse && regreadx->ost != propagatingScalar->ost) {
          MapleStack<MeExpr *> *pstack = vst_live_stack_vec[regreadx->ost->index.idx];
          return IsFunctionOfCurVersion(regreadx, static_cast<ScalarMeExpr *>(pstack->top())) ? kPropOnlyWithInverse : kPropNo;
        } else {
          return kPropNo;
        }
      }
    }
    case kMeOpVar: {
      VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(x);
      if (varmeexpr->IsVolatile(ssaTab)) {
        return kPropNo;
      }
      MIRSymbol *st = varmeexpr->ost->GetMIRSymbol();
      if (st->wpofakeParm || st->wpofakeRet) {
        return kPropNo;
      }
      if (!propagate_global_ref && st->IsGlobal() && !st->IsFinal() && !st->IgnoreRC() && !st->IsLiteralPtr()) {
        return kPropNo;
      }
      if (LocalToDifferentPU(st->stIdx, frombb)) {
        return kPropNo;
      }
      // get the current definition version
      vector<MeExpr *> varmeexprVec;
      CollectSubVarMeExpr(x, varmeexprVec);
      if (IsVersionConsistent(varmeexprVec, vst_live_stack_vec)) {
        return kPropYes;
      } else if (checkInverse && varmeexpr->ost != propagatingScalar->ost &&
                 varmeexpr->GetType()->typeKind != kTypeBitField) {
        MapleStack<MeExpr *> *pstack = vst_live_stack_vec[varmeexpr->ost->index.idx];
        return IsFunctionOfCurVersion(varmeexpr, static_cast<ScalarMeExpr *>(pstack->top())) ? kPropOnlyWithInverse : kPropNo;
      } else {
        return kPropNo;
      }
    }
    case kMeOpIvar: {
      IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(x);
      if (!IvarIsFinalField(ivarmeexpr) && !GlobalTables::GetTypeTable().typeTable[ivarmeexpr->tyIdx.GetIdx()]->PointsToConstString()) {
        if ((!propagate_iload_ref || (propagate_iload_ref_nonparm && atParm)) && ivarmeexpr->primType == PTY_ref) {
          return kPropNo;
        }
      }
      if (frombb->InTryBlock() && !curbb->InTryBlock()) {
        return kPropNo;
      }
      if (ivarmeexpr->IsVolatile() || ivarmeexpr->IsRCWeak()) {
        return kPropNo;
      }
      Propagatability prop0 = Propagatable(ivarmeexpr->base, frombb, false, false, nullptr);
      if (prop0 == kPropNo) {
        return kPropNo;
      }
      // get the current definition version
      vector<MeExpr *> varmeexprVec;
      CollectSubVarMeExpr(x, varmeexprVec);
      return IsVersionConsistent(varmeexprVec, vst_live_stack_vec) ? prop0 : kPropNo;
    }
    case kMeOpOp: {
      if (kOpcodeInfo.NotPure(x->op)) {
        return kPropNo;
      }
      if (x->op == OP_gcmallocjarray) {
        return kPropNo;
      }
      OpMeExpr *meopexpr = static_cast<OpMeExpr *>(x);
      MeExpr *opnd0 = meopexpr->GetOpnd(0);
      Propagatability prop0 = Propagatable(opnd0, frombb, false, checkInverse, propagatingScalar);
      if (prop0 == kPropNo) {
        return kPropNo;
      }
      MeExpr *opnd1 = meopexpr->GetOpnd(1);
      if (!opnd1) {
        return prop0;
      }
      Propagatability prop1 = Propagatable(opnd1, frombb, false, checkInverse, propagatingScalar);
      if (prop1 == kPropNo) {
        return kPropNo;
      }
      prop1 = std::min(prop0, prop1);
      MeExpr *opnd2 = meopexpr->GetOpnd(2);
      if (!opnd2) {
        return prop1;
      }
      Propagatability prop2 = Propagatable(opnd2, frombb, false, checkInverse, propagatingScalar);
      return std::min(prop1, prop2);
    }
    default:
      CHECK_FATAL(false, "MeProp::Propagatable() NYI");
      return kPropNo;
  }
}

// Expression x contains v; form and return the inverse of this expression based
// on the current version of v by descending x; formingExp is the tree being
// constructed during the descent; x must contain one and only one occurrence of
// v; work is done when it reaches the v node inside x.
MeExpr *Prop::FormInverse(ScalarMeExpr *v, MeExpr *x, MeExpr *formingExp) {
  MeExpr *newx = nullptr;
  switch (x->meOp) {
  case kMeOpVar:
  case kMeOpReg:
    if (x == v) {
      return formingExp;
    };
    return x;
  case kMeOpOp: {
    OpMeExpr *opx = static_cast<OpMeExpr *>(x);
    if (opx->op == OP_neg) {  // negate formingExp and recurse down
      OpMeExpr negx(-1, OP_neg, opx->primType, 1);
      negx.SetOpnd(formingExp, 0);
      newx = irMap->HashMeExpr(&negx);
      return FormInverse(v, opx->GetOpnd(0), newx);
    }
    if (opx->op == OP_add) {  // 2 patterns depending on which side contains v
      OpMeExpr subx(-1, OP_sub, opx->primType, 2);
      subx.SetOpnd(formingExp, 0);
      if (InvertibleOccurrences(v, opx->GetOpnd(0)) == 0) {
        // ( ..i2.. ) = y + ( ..i1.. ) becomes  ( ..i2.. ) - y = ( ..i1.. )
        // form formingExp - opx->GetOpnd(0)
        subx.SetOpnd(opx->GetOpnd(0), 1);
        newx = irMap->HashMeExpr(&subx);
        return FormInverse(v, opx->GetOpnd(1), newx);
      } else {
        // ( ..i2.. ) = ( ..i1.. ) + y  becomes  ( ..i2.. ) - y = ( ..i1.. )
        // form formingExp - opx->GetOpnd(1)
        subx.SetOpnd(opx->GetOpnd(1), 1);
        newx = irMap->HashMeExpr(&subx);
        return FormInverse(v, opx->GetOpnd(0), newx);
      }
    }
    if (opx->op == OP_sub) {
      if (InvertibleOccurrences(v, opx->GetOpnd(0)) == 0) {
        // ( ..i2.. ) = y - ( ..i1.. ) becomes y - ( ..i2.. ) = ( ..i1.. )
        // form opx->GetOpnd(0) - formingExp
        OpMeExpr subx(-1, OP_sub, opx->primType, 2);
        subx.SetOpnd(opx->GetOpnd(0), 0);
        subx.SetOpnd(formingExp, 1);
        newx = irMap->HashMeExpr(&subx);
        return FormInverse(v, opx->GetOpnd(1), newx);
      } else {
        // ( ..i2.. ) = ( ..i1.. ) - y  becomes  ( ..i2.. ) + y = ( ..i1.. )
        // form formingExp + opx->GetOpnd(1)
        OpMeExpr addx(-1, OP_add, opx->primType, 2);
        addx.SetOpnd(formingExp, 0);
        addx.SetOpnd(opx->GetOpnd(1), 1);
        newx = irMap->HashMeExpr(&addx);
        return FormInverse(v, opx->GetOpnd(0), newx);
      }
    }
    // fall-thru
  }
  default: CHECK_FATAL(false, "FormInverse: should not see these nodes");
  }
}

// recurse down the expression tree x; at the scalar whose version is different
// from the current version, replace it by an expression corresponding to the
// inverse of how its current version is computed from it; if there is no change,
// return NULL; if there is change, rehash on the way back
MeExpr *Prop::RehashUsingInverse(MeExpr *x) {
  switch (x->meOp) {
  case kMeOpVar:
  case kMeOpReg: {
    ScalarMeExpr *scalar = static_cast<ScalarMeExpr *>(x);
    MapleStack<MeExpr *> *pstack = vst_live_stack_vec[scalar->ost->index.idx];
    if (pstack == nullptr || pstack->top() == scalar) {
      return nullptr;
    }
    ScalarMeExpr *curScalar = static_cast<ScalarMeExpr *>(pstack->top());
    return FormInverse(scalar, curScalar->def.defStmt->rhs, curScalar);
  }
  case kMeOpIvar: {
    IvarMeExpr *ivarx = static_cast<IvarMeExpr *>(x);
    MeExpr *result = RehashUsingInverse(ivarx->base);
    if (result != nullptr) {
      IvarMeExpr newivarx(-1, ivarx->primType, ivarx->tyIdx, ivarx->fieldID);
      newivarx.base = result;
      newivarx.mu = ivarx->mu;
      return irMap->HashMeExpr(&newivarx);
    }
    return nullptr;
  }
  case kMeOpOp: {
    OpMeExpr *opx = static_cast<OpMeExpr *>(x);
    MeExpr *res0 = RehashUsingInverse(opx->GetOpnd(0));
    MeExpr *res1 = nullptr;
    MeExpr *res2 = nullptr;
    if (opx->numOpnds > 1) {
      res1 = RehashUsingInverse(opx->GetOpnd(1));
      if (opx->numOpnds > 2) {
        res2 = RehashUsingInverse(opx->GetOpnd(2));
      }
    }
    if (res0 == nullptr && res1 == nullptr && res2 == nullptr) {
      return nullptr;
    }
    OpMeExpr newopx(-1, opx->op, opx->primType, opx->numOpnds);
    newopx.opndType = opx->opndType;
    newopx.bitsOffset = opx->bitsOffset;
    newopx.bitsSize = opx->bitsSize;
    newopx.tyIdx = opx->tyIdx;
    newopx.fieldID = opx->fieldID;
    if (res0) {
      newopx.SetOpnd(res0, 0);
    } else {
      newopx.SetOpnd(opx->GetOpnd(0), 0);
    }
    if (opx->numOpnds > 1) {
      if (res1) {
        newopx.SetOpnd(res1, 1);
      } else {
        newopx.SetOpnd(opx->GetOpnd(1), 1);
      }
      if (opx->numOpnds > 2) {
        if (res1) {
          newopx.SetOpnd(res2, 2);
        } else {
          newopx.SetOpnd(opx->GetOpnd(2), 2);
        }
      }
    }
    return irMap->HashMeExpr(&newopx);
  }
  case kMeOpNary: {
    NaryMeExpr *naryx = static_cast<NaryMeExpr *>(x);
    std::vector<MeExpr *> results(naryx->numOpnds, nullptr);
    bool needRehash = false;
    uint32 i;
    for (i = 0; i < naryx->numOpnds; i++) {
      results[i] = RehashUsingInverse(naryx->GetOpnd(i));
      if (results[i] != nullptr) {
        needRehash = true;
      }
    }
    if (!needRehash) {
      return nullptr;
    }
    NaryMeExpr newnaryx(&prop_map_alloc, -1, naryx->op, naryx->primType,
            naryx->numOpnds, naryx->tyIdx, naryx->intrinsic, naryx->boundCheck);
    for (i = 0; i < naryx->numOpnds; i++) {
      if (results[i] != nullptr) {
        newnaryx.SetOpnd(results[i], i);
      } else {
        newnaryx.SetOpnd(naryx->GetOpnd(i), i);
      }
    }
    return irMap->HashMeExpr(&newnaryx);
  }
  default: return nullptr;
  }
}

// if lhs is smaller than rhs, insert operation to simulate the truncation
// effect of rhs being stored into lhs; otherwise, just return rhs
MeExpr *Prop::CheckTruncation(MeExpr *lhs, MeExpr *rhs) {
  if (JAVALANG || !IsPrimitiveInteger(rhs->primType)) {
    return rhs;
  }
  TyIdx lhsTyIdx(0);
  MIRType *lhsTy = nullptr;
  if (lhs->meOp == kMeOpVar) {
    VarMeExpr *varx = static_cast<VarMeExpr *>(lhs);
    lhsTyIdx = varx->ost->tyIdx;
    lhsTy = GlobalTables::GetTypeTable().GetTypeFromTyIdx(lhsTyIdx);
  } else if (lhs->meOp == kMeOpIvar) {
    IvarMeExpr *ivarx = static_cast<IvarMeExpr *>(lhs);
    MIRPtrType *ptType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(ivarx->tyIdx));
    lhsTyIdx = ptType->pointedTyIdx;
    lhsTy = GlobalTables::GetTypeTable().GetTypeFromTyIdx(lhsTyIdx);
    if (ivarx->fieldID != 0) {
      lhsTy = static_cast<MIRStructType *>(lhsTy)->GetFieldType(ivarx->fieldID);
    }
  } else {
    return rhs;
  }
  if (lhsTy->typeKind == kTypeBitField) {
    MIRBitfieldType *bitfieldTy = static_cast<MIRBitfieldType *>(lhsTy);
    if (GetPrimTypeBitSize(rhs->primType) <= bitfieldTy->fieldSize) {
      return rhs;
    }
    // insert OP_zext or OP_sext
    Opcode extOp = IsSignedInteger(lhsTy->primType) ? OP_sext : OP_zext;
    PrimType newPrimType = PTY_u32;
    if (bitfieldTy->fieldSize <= 32) {
      if (IsSignedInteger(lhsTy->primType)) {
        newPrimType = PTY_i32;
      }
    } else {
      if (IsSignedInteger(lhsTy->primType)) {
        newPrimType = PTY_i64;
      } else {
        newPrimType = PTY_u64;
      }
    }
    OpMeExpr opmeexpr(-1, extOp, newPrimType, 1);
    opmeexpr.bitsSize = bitfieldTy->fieldSize;
    opmeexpr.SetOpnd(rhs, 0);
    return irMap->HashMeExpr(&opmeexpr);
  }
  if (IsPrimitiveInteger(lhsTy->primType) &&
      lhsTy->primType != PTY_ptr  && lhsTy->primType != PTY_ref &&
      GetPrimTypeSize(lhsTy->primType) < rhs->primType) {
    if (GetPrimTypeSize(lhsTy->primType) >= 4) {
      return irMap->CreateMeExprTypeCvt(lhsTy->primType, rhs->primType, rhs);
    } else {
      Opcode extOp = IsSignedInteger(lhsTy->primType) ? OP_sext : OP_zext;
      PrimType newPrimType = PTY_u32;
      if (IsSignedInteger(lhsTy->primType)) {
        newPrimType = PTY_i32;
      }
      OpMeExpr opmeexpr(-1, extOp, newPrimType, 1);
      opmeexpr.bitsSize = GetPrimTypeSize(lhsTy->primType) * 8;
      opmeexpr.SetOpnd(rhs, 0);
      return irMap->HashMeExpr(&opmeexpr);
    }
  }
  // if lhs is function pointer and rhs is not, insert a retype
  if (lhsTy->typeKind == kTypePointer) {
    MIRPtrType *lhsPtrType = static_cast<MIRPtrType *>(lhsTy);
    if (lhsPtrType->GetPointedType()->typeKind == kTypeFunction) {
      bool needRetype = true;
      MIRType *rhsTy = nullptr;
      if (rhs->meOp == kMeOpVar) {
        VarMeExpr *rhsvarx = static_cast<VarMeExpr *>(rhs);
        rhsTy = GlobalTables::GetTypeTable().GetTypeFromTyIdx(rhsvarx->ost->tyIdx);
      } else if (rhs->meOp == kMeOpIvar) {
        IvarMeExpr *rhsivarx = static_cast<IvarMeExpr *>(rhs);
        MIRPtrType *rhsPtrType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(rhsivarx->tyIdx));
        rhsTy = rhsPtrType->GetPointedType();
        if (rhsivarx->fieldID != 0) {
          rhsTy = static_cast<MIRStructType *>(rhsTy)->GetFieldType(rhsivarx->fieldID);
        }
      }
      if (rhsTy != nullptr && rhsTy == lhsPtrType) {
        needRetype = false;
      }
      if (needRetype) {
        OpMeExpr opmeexpr(-1, OP_retype, lhsPtrType->primType, 1);
        opmeexpr.tyIdx = lhsPtrType->tyIdx;
        opmeexpr.SetOpnd(rhs, 0);
        return irMap->HashMeExpr(&opmeexpr);
      }
    }
  }
  return rhs;
}

// return varmeexpr itself if no propagation opportunity
MeExpr *Prop::PropVar(VarMeExpr *varmeexpr, bool atParm, bool checkPhi) {
  MIRSymbol *st = varmeexpr->ost->GetMIRSymbol();
  if (st->wpofakeParm || st->wpofakeRet || st->instrumented || varmeexpr->IsVolatile(ssaTab)) {
    return varmeexpr;
  }
  MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(varmeexpr->ost->tyIdx);
  if (mirType->typeKind == kTypeBitField && IsSignedInteger(mirType->primType)) {
    return varmeexpr;
  }
  if (varmeexpr->defBy == kDefByStmt) {
    DassignMeStmt *defStmt = dynamic_cast<DassignMeStmt *>(varmeexpr->def.defStmt);
    CHECK_FATAL(defStmt != nullptr, "dynamic cast result is nullptr");
    MeExpr *rhs = defStmt->rhs;
    if (rhs->GetDepth() > kPropTreeLevel) {
      return varmeexpr;
    }
    Propagatability propagatable = Propagatable(rhs, defStmt->bb, atParm, true, varmeexpr);
    if (propagatable != kPropNo) {
      // mark propagated for iread ref
      if (rhs->meOp == kMeOpIvar && rhs->primType == PTY_ref) {
        defStmt->propagated = true;
      }
      if (propagatable == kPropOnlyWithInverse) {
        rhs = RehashUsingInverse(rhs);
      }
      return CheckTruncation(varmeexpr, rhs);
    } else {
      return varmeexpr;
    }
  } else if (checkPhi && varmeexpr->defBy == kDefByPhi && propagate_at_phi) {
    MePhiNode *defPhi = varmeexpr->def.defPhi;
    VarMeExpr *phiopndlast = static_cast<VarMeExpr*>(defPhi->opnds.back());
    MeExpr *opndlastprop = PropVar(phiopndlast, atParm, false);
    CHECK_FATAL(opndlastprop != nullptr, "opndlastprop is nullptr in Prop::PropVar");
    if (opndlastprop != varmeexpr && opndlastprop != phiopndlast && opndlastprop->meOp == kMeOpVar) {
      // one more call
      opndlastprop = PropVar(static_cast<VarMeExpr *>(opndlastprop), atParm, false);
    }
    if (opndlastprop == varmeexpr) {
      return varmeexpr;
    }
    for (int32 i = defPhi->opnds.size() - 2; i >= 0; i--) {
      VarMeExpr *phiopnd = static_cast<VarMeExpr*>(defPhi->opnds[i]);
      MeExpr *opndprop = PropVar(phiopnd, atParm, false);
      if (opndprop != opndlastprop) {
        return varmeexpr;
      }
    }
    return opndlastprop;
  }
  return varmeexpr;
}

MeExpr *Prop::PropReg(RegMeExpr *regmeexpr, bool atParm) {
  if (regmeexpr->defBy == kDefByStmt) {
    AssignMeStmt *defStmt = dynamic_cast<AssignMeStmt *>(regmeexpr->def.defStmt);
    if (defStmt == nullptr) {
      FATAL(kLncFatal, "dynamic cast result is nullptr ");
    }
    ASSERT(defStmt, "");
    MeExpr *rhs = defStmt->rhs;
    if (rhs->GetDepth() > kPropTreeLevel) {
      return regmeexpr;
    }
    Propagatability propagatable = Propagatable(rhs, defStmt->bb, atParm, true, regmeexpr);
    if (propagatable != kPropNo) {
      if (propagatable == kPropOnlyWithInverse) {
        rhs = RehashUsingInverse(rhs);
      }
      return rhs;
    }
  }
  return regmeexpr;
}

MeExpr *Prop::PropIvar(IvarMeExpr *ivarmeexpr) {
  IassignMeStmt *defStmt = ivarmeexpr->defStmt;
  if (!defStmt || ivarmeexpr->IsVolatile()) {
    return ivarmeexpr;
  }
  MeExpr *rhs = defStmt->rhs;
  if (rhs->GetDepth() <= kPropTreeLevel && Propagatable(rhs, defStmt->bb, false) != kPropNo) {
    return CheckTruncation(ivarmeexpr, rhs);
  }
  return ivarmeexpr;
}

MeExpr *Prop::PropMeExpr(MeExpr *meexpr, bool &isproped, bool atParm) {
  MeExprOp meOp = meexpr->meOp;
  bool subproped = false;
  switch (meOp) {
    case kMeOpVar: {
      VarMeExpr *varexpr = static_cast<VarMeExpr *>(meexpr);
      MeExpr *propmeexpr = PropVar(varexpr, atParm, true);
      if (propmeexpr != varexpr) {
        isproped = true;
      }
      return propmeexpr;
    }
    case kMeOpReg: {
      RegMeExpr *regexpr = static_cast<RegMeExpr *>(meexpr);
      if (regexpr->regIdx < 0) {
        return meexpr;
      }
      MeExpr *propmeexpr = PropReg(regexpr, atParm);
      if (propmeexpr != regexpr) {
        isproped = true;
      }
      return propmeexpr;
    }
    case kMeOpIvar: {
      IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(meexpr);
      CHECK_FATAL(ivarmeexpr->mu != nullptr, "PropMeExpr: ivar has mu == nullptr");
      bool baseproped = false;
      MeExpr *base = nullptr;
      if (ivarmeexpr->base->meOp != kMeOpVar || propagate_base) {
        base = PropMeExpr(ivarmeexpr->base, baseproped, false);
      }
      if (baseproped) {
        isproped = true;
        IvarMeExpr newmeexpr(-1, ivarmeexpr->primType, ivarmeexpr->tyIdx, ivarmeexpr->fieldID);
        newmeexpr.base = base;
        newmeexpr.mu = ivarmeexpr->mu;
        ivarmeexpr = static_cast<IvarMeExpr *>(irMap->HashMeExpr(&newmeexpr));
      }
      MeExpr *propivarexpr = PropIvar(ivarmeexpr);
      if (propivarexpr != ivarmeexpr) {
        isproped = true;
      }
      return propivarexpr;
    }
    case kMeOpOp: {
      OpMeExpr *meopexpr = static_cast<OpMeExpr *>(meexpr);
      OpMeExpr newmeexpr(-1, meopexpr->op, meopexpr->primType, meopexpr->numOpnds);
      newmeexpr.SetOpnd(PropMeExpr(meopexpr->GetOpnd(0), subproped, false), 0);
      if (meopexpr->GetOpnd(1)) {
        newmeexpr.SetOpnd(PropMeExpr(meopexpr->GetOpnd(1), subproped, false), 1);
        if (meopexpr->GetOpnd(2)) {
          newmeexpr.SetOpnd(PropMeExpr(meopexpr->GetOpnd(2), subproped, false), 2);
        }
      }
      if (subproped) {
        isproped = true;
        newmeexpr.opndType = meopexpr->opndType;
        newmeexpr.bitsOffset = meopexpr->bitsOffset;
        newmeexpr.bitsSize = meopexpr->bitsSize;
        newmeexpr.tyIdx = meopexpr->tyIdx;
        newmeexpr.fieldID = meopexpr->fieldID;
        MeExpr *splfmeexpr = irMap->SimplifyOpMeExpr(&newmeexpr);
        return splfmeexpr ? splfmeexpr : irMap->HashMeExpr(&newmeexpr);
      } else {
        return meopexpr;
      }
    }
    case kMeOpNary: {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(meexpr);
      NaryMeExpr newmeexpr(&prop_map_alloc, -1, meexpr->op, meexpr->primType, meexpr->numOpnds,
          narymeexpr->tyIdx, narymeexpr->intrinsic, narymeexpr->boundCheck);
      for (uint32 i = 0; i < narymeexpr->numOpnds; i++) {
        if (i == 0 && narymeexpr->op == OP_array && !propagate_base) {
          newmeexpr.PushOpnd(narymeexpr->GetOpnd(i));
        } else {
          newmeexpr.PushOpnd(PropMeExpr(narymeexpr->GetOpnd(i), subproped, false));
        }
      }
      if (subproped) {
        isproped = true;
        return irMap->HashMeExpr(&newmeexpr);
      } else {
        return narymeexpr;
      }
    }
    case kMeOpAddrof:
    case kMeOpAddroffunc:
    case kMeOpAddroflabel:
    case kMeOpGcmalloc:
    case kMeOpConst:
    case kMeOpConststr:
    case kMeOpConststr16:
    case kMeOpSizeoftype:
      return meexpr;
    default:
      CHECK_FATAL(false, "MeOP NIY");
      return nullptr;
  }
}

void Prop::TraversalMeStmt(MeStmt *mestmt) {
  Opcode op = mestmt->op;
  bool subproped = false;
  switch (op) {
    case OP_dassign: {
      DassignMeStmt *varmestmt = static_cast<DassignMeStmt *>(mestmt);
      varmestmt->rhs = PropMeExpr(varmestmt->rhs, subproped, false);
      if (subproped) {
        varmestmt->isIncDecStmt = false;
      }
      PropUpdateDef(static_cast<VarMeExpr *>(varmestmt->lhs));
      PropUpdateChiListDef(varmestmt->chiList);
      break;
    }
    case OP_regassign: {
      AssignMeStmt *regmestmt = static_cast<AssignMeStmt *>(mestmt);
      regmestmt->rhs = PropMeExpr(regmestmt->rhs, subproped, false);
      PropUpdateDef(static_cast<RegMeExpr *>(regmestmt->lhs));
      break;
    }
    case OP_maydassign: {
      MaydassignMeStmt *maydstmt = static_cast<MaydassignMeStmt *>(mestmt);
      maydstmt->rhs = PropMeExpr(maydstmt->rhs, subproped, false);
      PropUpdateChiListDef(maydstmt->chiList);
      break;
    }
    case OP_iassign: {
      IassignMeStmt *ivarstmt = static_cast<IassignMeStmt *>(mestmt);
      ivarstmt->rhs = PropMeExpr(ivarstmt->rhs, subproped, false);
      if (ivarstmt->lhsVar->base->meOp != kMeOpVar || propagate_base) {
        ivarstmt->lhsVar->base = PropMeExpr(ivarstmt->lhsVar->base, subproped, false);
      }
      if (subproped) {
        ivarstmt->lhsVar = irMap->BuildLhsIvarFromIassMeStmt(ivarstmt);
      }
      PropUpdateChiListDef(ivarstmt->chiList);
      break;
    }
    case OP_syncenter:
    case OP_syncexit: {
      SyncMeStmt *syncmestmt = static_cast<SyncMeStmt *>(mestmt);
      MapleVector<MeExpr *> &opnds = syncmestmt->opnds;
      for (uint32 i = 0; i < opnds.size(); i++) {
        MeExpr *opnd = opnds[i];
        opnds[i] = PropMeExpr(opnd, subproped, false);
      }
      break;
    }
    case OP_throw: {
      ThrowMeStmt *thrmestmt = static_cast<ThrowMeStmt *>(mestmt);
      thrmestmt->opnd = PropMeExpr(thrmestmt->opnd, subproped, false);
      break;
    }
    case OP_assertnonnull: {
      UnaryMeStmt *umestmt = static_cast<UnaryMeStmt *>(mestmt);
      umestmt->opnd = PropMeExpr(umestmt->opnd, subproped, false);
      if (umestmt->opnd->meOp == kMeOpAddrof) {
        // addrof indicates the object is statically allocated on stack
        mestmt->bb->RemoveMeStmt(mestmt);
      }
      break;
    }
    case OP_eval:
    case OP_igoto:
    case OP_free: {
      UnaryMeStmt *umestmt = static_cast<UnaryMeStmt *>(mestmt);
      umestmt->opnd = PropMeExpr(umestmt->opnd, subproped, false);
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
        MeExpr *opnd = opnds[i];
        opnds[i] = PropMeExpr(opnd, subproped, true /* at_parm */);
      }
      PropUpdateChiListDef(callmestmt->chiList);
      break;
    }
    case OP_icall:
    case OP_icallassigned: {
      IcallMeStmt *icallmestmt = static_cast<IcallMeStmt *>(mestmt);
      MapleVector<MeExpr *> &opnds = icallmestmt->opnds;
      for (uint32 i = 0; i < opnds.size(); i++) {
        MeExpr *opnd = opnds[i];
        opnds[i] = PropMeExpr(opnd, subproped, true /* at_parm */);
      }
      PropUpdateChiListDef(icallmestmt->chiList);
      break;
    }
    case OP_intrinsiccallwithtype:
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_intrinsiccallwithtypeassigned:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned: {
      IntrinsiccallMeStmt *intrincallstmt = static_cast<IntrinsiccallMeStmt *>(mestmt);
      if (intrincallstmt->intrinsic != maple::INTRN_JAVA_CLINIT_CHECK_SGET &&
          intrincallstmt->intrinsic != maple::INTRN_JAVA_CLINIT_CHECK_SPUT) {
        MapleVector<MeExpr *> &opnds = intrincallstmt->opnds;
        for (uint32 i = 0; i < opnds.size(); i++) {
          MeExpr *opnd = opnds[i];
          opnds[i] = PropMeExpr(opnd, subproped, true /* at_parm */);
        }
      }
      PropUpdateChiListDef(intrincallstmt->chiList);
      break;
    }
    case OP_brtrue:
    case OP_brfalse: {
      CondGotoMeStmt *condgotostmt = static_cast<CondGotoMeStmt *>(mestmt);
      condgotostmt->opnd = PropMeExpr(condgotostmt->opnd, subproped, false);
      break;
    }
    case OP_switch: {
      SwitchMeStmt *switchstmt = static_cast<SwitchMeStmt *>(mestmt);
      switchstmt->opnd = PropMeExpr(switchstmt->opnd, subproped, false);
      break;
    }
    case OP_return: {
      RetMeStmt *retmestmt = static_cast<RetMeStmt *>(mestmt);
      MapleVector<MeExpr *> &opnds = retmestmt->opnds;
      // java return operand cannot be expression because cleanup intrinsic is
      // inserted before the return statement
      if (JAVALANG && opnds.size() == 1 && opnds[0]->meOp == kMeOpVar) {
        break;
      }
      for (uint32 i = 0; i < opnds.size(); i++) {
        MeExpr *opnd = opnds[i];
        opnds[i] = PropMeExpr(opnd, subproped, false);
      }
      break;
    }
    case OP_assertlt:
    case OP_assertge: {
      AssertMeStmt *assmestmt = static_cast<AssertMeStmt *>(mestmt);
      assmestmt->opnds[0] = PropMeExpr(assmestmt->opnds[0], subproped, false);
      assmestmt->opnds[1] = PropMeExpr(assmestmt->opnds[1], subproped, false);
      break;
    }
    case OP_jstry:
    case OP_jscatch:
    case OP_finally:
    case OP_endtry:
    case OP_cleanuptry:
    case OP_try:
    case OP_javatry:
    case OP_cpptry:
    case OP_catch:
    case OP_javacatch:
    case OP_cppcatch:
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
      CHECK_FATAL(false, "unexpected stmt in hprop or NYI");
  }
  if (kOpcodeInfo.IsCallAssigned(op)) {
    PropUpdateMustDefList(mestmt);
  }
}

void Prop::TraversalBB(BB *bb) {
  if (bb == nullptr) {
    return;
  }
  if (bb_visited[bb->id.idx]) {
    return;
  }
  bb_visited[bb->id.idx] = true;
  curbb = bb;

  UpdateCurFunction(bb);

  // record stack size for variable versions before processing rename. It is used for stack pop up.
  MapleVector<uint32> curStackSizeVec(prop_map_alloc.Adapter());
  curStackSizeVec.resize(vst_live_stack_vec.size());

  for (uint32 i = 1; i < vst_live_stack_vec.size(); i++) {
    curStackSizeVec[i] = vst_live_stack_vec[i]->size();
  }

  // traversal var/reg phi nodes
  MapleMap<OStIdx, MePhiNode *> &mephiList = bb->mePhiList;
  for (MapleMap<OStIdx, MePhiNode *>::iterator it = mephiList.begin(); it != mephiList.end(); it++) {
    MePhiNode *phimenode = it->second;
    PropUpdateDef(phimenode->lhs);
  }

  // traversal on stmt
  if (!bb->meStmtList.empty()) {
    for (MeStmt *mestmt = bb->meStmtList.first; mestmt;) {
      // Record the next stmt because TraversalMeStmt may remove mestmt
      MeStmt *nextstmt = mestmt->next;
      TraversalMeStmt(mestmt);
      mestmt = nextstmt;
    }
  }
  CHECK(bb->id.idx < dominance->domChildren.size(), "index out of range in Prop::TraversalBB ");
  MapleSet<BBId> *domChildren = &dominance->domChildren[bb->id.idx];
  for (MapleSet<BBId>::iterator bbit = domChildren->begin(); bbit != domChildren->end(); bbit++) {
    BBId childbbid = *bbit;
    TraversalBB(GetBB(childbbid));
  }
  for (uint32 i = 1; i < vst_live_stack_vec.size(); i++) {
    MapleStack<MeExpr *> *liveStack = vst_live_stack_vec[i];
    uint32 curSize = curStackSizeVec[i];
    while (liveStack->size() > curSize) {
      liveStack->pop();
    }
  }
}

}  // namespace maple
