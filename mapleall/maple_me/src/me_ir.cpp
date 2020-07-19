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

#include "opcode_info.h"
#include "me_ir.h"
#include "irmap.h"
#include "printing.h"
#include "me_ssa.h"
#include "mir_function.h"
#include "name_mangler.h"

using namespace std;

namespace maple {

bool MeExpr::IsTheSameWorkcand(MeExpr *meexpr) {
  CHECK_FATAL((exprID != -1 || meexpr->exprID != -1), "at least one of them should not be none initialized");
  if (exprID == meexpr->exprID) {
    return true;
  }
  if (op != meexpr->op) {
    return false;
  }
  if (IsPrimitiveFloat(primType) != IsPrimitiveFloat(meexpr->primType)) {
    return false;
  }
  if (GetPrimTypeSize(primType) != GetPrimTypeSize(meexpr->primType)) {
    return false;
  }
  if (kOpcodeInfo.IsTypeCvt(op) || kOpcodeInfo.IsCompare(op)) {
    if (primType != meexpr->primType ||
        static_cast<OpMeExpr *>(this)->opndType != static_cast<OpMeExpr *>(meexpr)->opndType) {
      return false;
    }
  }
  if (op == OP_extractbits || op == OP_depositbits || op == OP_sext || op == OP_zext) {
    if (static_cast<OpMeExpr *>(this)->bitsOffset != static_cast<OpMeExpr *>(meexpr)->bitsOffset ||
        static_cast<OpMeExpr *>(this)->bitsSize != static_cast<OpMeExpr *>(meexpr)->bitsSize) {
      return false;
    }
  }
  if (op == OP_resolveinterfacefunc || op == OP_resolvevirtualfunc || op == OP_iaddrof)
    if (static_cast<OpMeExpr *>(this)->fieldID != static_cast<OpMeExpr *>(meexpr)->fieldID) {
      return false;
    }
  if (IsUseSameSymbol(meexpr)) {
    return true;
  }
  return false;
}

// get the definition of this
// for example:
// v2 = x + b;
// v1 = v2;
// then v1->ResolveMeExprValue() returns x+b
MeExpr *MeExpr::ResolveMeExprValue() {
  MeExpr *cmpop0 = this;
  while (cmpop0 && cmpop0->meOp == kMeOpVar) {
    VarMeExpr *varcmpop = static_cast<VarMeExpr *>(cmpop0);
    if (varcmpop->defBy == kDefByStmt) {
      cmpop0 = static_cast<DassignMeStmt *>(varcmpop->def.defStmt)->rhs;
    } else if (varcmpop->defBy == kDefByChi) {
      ChiMeNode *defChi = varcmpop->def.defChi;
      MeStmt *base = defChi->base;
      if (base->op == OP_maydassign) {
        cmpop0 = static_cast<MaydassignMeStmt *>(base)->rhs;
      } else {
        cmpop0 = nullptr;
      }
    } else {
      cmpop0 = nullptr;
    }
  }
  return cmpop0;
}

// get the definition VarMeExpr of this
// for expample:
// v2 = v3;
// v1 = v2;
// this = v1;
// this->ResolveVarMeValue() returns v3;
// if no resolved VarMeExpr, return this
VarMeExpr *VarMeExpr::ResolveVarMeValue() {
  VarMeExpr *cmpop0 = this;
  while (true) {
    if (cmpop0->defBy == kDefByStmt) {
      DassignMeStmt *defStmt = static_cast<DassignMeStmt *>(cmpop0->def.defStmt);
      if (defStmt->rhs->meOp != kMeOpVar) {
        break;
      }
      cmpop0 = static_cast<VarMeExpr *>(defStmt->rhs);
    } else {
      break;
    }
  }
  return cmpop0;
}

// *this can be any node, but *v must be VarMeExpr
bool MeExpr::IsSameVariableValue(VarMeExpr *v) {
  if (v == this) {
    return true;
  }
  // look up x's value if it is variable or register
  if (meOp == kMeOpVar) {
    VarMeExpr *xvar = static_cast<VarMeExpr *>(this);
    if (xvar->defBy == kDefByStmt && xvar->def.defStmt->op == OP_dassign) {
      DassignMeStmt *xdass = static_cast<DassignMeStmt *>(xvar->def.defStmt);
      if (xdass->rhs == v) {
        return true;
      }
    }
  } else if (meOp == kMeOpReg) {
    RegMeExpr *xreg = static_cast<RegMeExpr *>(this);
    if (xreg->defBy == kDefByStmt && xreg->def.defStmt->op == OP_regassign) {
      AssignMeStmt *xrass = static_cast<AssignMeStmt *>(xreg->def.defStmt);
      if (xrass->rhs == v) {
        return true;
      }
    }
  }
  // look up v's value
  if (v->defBy == kDefByStmt && v->def.defStmt->op == OP_dassign) {
    DassignMeStmt *vdass = static_cast<DassignMeStmt *>(v->def.defStmt);
    if (vdass->rhs == this) {
      return true;
    }
  }
  return false;
}

// return true if the expression could throw exception; needs to be in sync with
// BaseNode::MayThrowException()
bool MeExpr::CouldThrowException() {
  if (kOpcodeInfo.MayThrowException(op)) {
    if (op != OP_array) {
      return true;
    }
    if (static_cast<NaryMeExpr *>(this)->boundCheck) {
      return true;
    }
  } else if (op == OP_intrinsicop) {
    if (static_cast<NaryMeExpr *>(this)->intrinsic == INTRN_JAVA_ARRAY_LENGTH) {
      return true;
    }
  }
  for (int32 i = 0; i < NumMeExprOpnds(); i++) {
    if (GetOpnd(i)->CouldThrowException()) {
      return true;
    }
  }
  return false;
}

// search through the SSA graph to find a version with defBy == DefBYy_stmt;
// visited is for avoiding processing a node more than once
RegMeExpr *RegMeExpr::FindDefByStmt(std::set<RegMeExpr *> *visited) {
  if (visited->find(this) != visited->end()) {
    return nullptr;
  }
  visited->insert(this);
  if (defBy == kDefByStmt) {
    return this;
  }
  if (defBy == kDefByPhi) {
    MePhiNode *phireg = def.defPhi;
    for (auto phiopnd : phireg->opnds) {
      RegMeExpr *regopnd = static_cast<RegMeExpr*>(phiopnd);
      RegMeExpr *res = regopnd->FindDefByStmt(visited);
      if (res != nullptr) {
        return res;
      }
    }
  }
  return nullptr;
}

MeExpr *MeExpr::GetAddrExprBase() {
  switch (meOp) {
    case kMeOpAddrof:
    case kMeOpVar:
      return this;
    case kMeOpOp:
      if (op == OP_add || op == OP_sub) {
        OpMeExpr *opmeexpr = static_cast<OpMeExpr *>(this);
        return opmeexpr->GetOpnd(0)->GetAddrExprBase();
      }
      return this;
    case kMeOpNary:
      if (op == OP_array) {
        NaryMeExpr *naryexpr = static_cast<NaryMeExpr *>(this);
        CHECK_FATAL(naryexpr->GetOpnd(0) != nullptr, "");
        return naryexpr->GetOpnd(0)->GetAddrExprBase();
      }
      return this;
    case kMeOpReg: {
      RegMeExpr *basevar = static_cast<RegMeExpr *>(this);
      std::set<RegMeExpr *> visited;
      basevar = basevar->FindDefByStmt(&visited);
      if (basevar && basevar->defBy == kDefByStmt) {
        MeStmt *basedefstmt = basevar->def.defStmt;
        AssignMeStmt *regassign = static_cast<AssignMeStmt *>(basedefstmt);
        MeExpr *rhs = regassign->rhs;

        // Following we only consider array, add and sub
        // Prevent the following situation for reg %1
        // regassign ref %1 (gcmallocjarray ref ...)
        if (rhs->op == OP_array || rhs->op == OP_add || rhs->op == OP_sub) {
          return rhs->GetAddrExprBase();
        }
      }
      return this;
    }
    default:
      return this;
  }
}

bool NaryMeExpr::IsUseSameSymbol(MeExpr *meexpr) {
  NaryMeExpr *narymeexpr = dynamic_cast<NaryMeExpr *>(meexpr);
  if (!narymeexpr) {
    return false;
  }

  if (meexpr->op != op || narymeexpr->intrinsic != intrinsic || narymeexpr->tyIdx != tyIdx) {
    return false;
  }

  if (opnds.size() != narymeexpr->opnds.size()) {
    return false;
  }

  for (uint32 i = 0; i < opnds.size(); i++) {
    if (!GetOpnd(i)->IsUseSameSymbol(narymeexpr->GetOpnd(i))) {
      return false;
    }
  }
  return true;
}

/*
   Check if two IvarMeExpr are identical.
   If ivar_use is the first use of the same ivar coming from an iassign
   (ivar_def), then update its mu: ivar_def->mu = ivar_use->mu.
 */
static bool AreIvarIdentical(IvarMeExpr *ivarUse, IvarMeExpr *ivarDef) {
  if (ivarDef->base->exprID != ivarUse->base->exprID) {
    return false;
  }
  if (ivarDef->fieldID != ivarUse->fieldID) {
    return false;
  }
  if (ivarUse->tyIdx != ivarDef->tyIdx) {
    return false;
  }
  if (ivarDef->IsVolatile() || ivarUse->IsVolatile()) {
    return false;
  }

  // check the two mu being identical
  if (ivarUse->mu != ivarDef->mu) {
    if (ivarDef->mu != nullptr && ivarUse->mu != nullptr && ivarDef->mu->defBy == kDefByChi &&
        ivarUse->mu->defBy == kDefByChi) {
      ChiMeNode *ivarDefChi = ivarDef->mu->def.defChi;
      ChiMeNode *ivarUseChi = ivarUse->mu->def.defChi;
      if (ivarDefChi->base != nullptr && ivarDefChi->base == ivarUseChi->base) {
        return true;
      }
    }
    if (ivarDef->mu == nullptr && ivarDef->defStmt != nullptr && ivarUse->mu) {
      IassignMeStmt *iass = ivarDef->defStmt;
      if (iass->op != OP_iassign) {
        // this can happen due to use of placement new
        return false;
      }
      for (MapleMap<OStIdx, ChiMeNode *>::iterator xit = iass->chiList.begin(); xit != iass->chiList.end(); xit++) {
        ChiMeNode *chi = xit->second;
        if (chi->lhs->exprID == ivarUse->mu->exprID) {
          ivarDef->mu = ivarUse->mu;
          return true;
        }
      }
    }
    return false;
  }
  return true;
}

bool IvarMeExpr::IsIdentical(MeExpr *meexpr) {
  if (meexpr->op != op) {
    return false;
  }
  IvarMeExpr *x = static_cast<IvarMeExpr *>(meexpr);
  if (!AreIvarIdentical(this, x)) {
    return false;
  }
  return true;
}

bool OpMeExpr::IsIdentical(MeExpr *meexpr) {
  if (meexpr->op != op || kOpcodeInfo.NotPure(op)) {
    return false;
  }
  OpMeExpr *opx = static_cast<OpMeExpr *>(meexpr);
  if (opx->primType != primType || opx->opndType != opndType || opx->bitsOffset != bitsOffset ||
      opx->bitsSize != bitsSize || opx->tyIdx != tyIdx || opx->fieldID != fieldID) {
    return false;
  }
  if (opx->GetOpnd(0)->exprID != GetOpnd(0)->exprID) {
    return false;
  }
  if (numOpnds > 1) {
    if (opx->GetOpnd(1)->exprID != GetOpnd(1)->exprID) {
      return false;
    }
    if (numOpnds > 2) {
      if (opx->GetOpnd(2)->exprID != GetOpnd(2)->exprID) {
        return false;
      }
    }
  }
  return true;
}

bool NaryMeExpr::IsIdentical(MeExpr *meexpr) {
  if (meexpr->op != op) {
    return false;
  }
  if ((op == OP_intrinsicop || op == OP_intrinsicopwithtype) &&
      !IntrinDesc::intrintable[intrinsic].IsPure()) {
    return false;
  }
  NaryMeExpr *x = static_cast<NaryMeExpr *>(meexpr);
  if (x->tyIdx != tyIdx || x->intrinsic != intrinsic || x->boundCheck != boundCheck) {
    return false;
  }
  if (x->numOpnds != numOpnds) {
    return false;
  }
  for (int32 i = 0; i < numOpnds; i++) {
    if (GetOpnd(i)->exprID != x->GetOpnd(i)->exprID) {
      return false;
    }
  }
  return true;
}

bool IvarMeExpr::IsUseSameSymbol(MeExpr *meexpr) {
  if (meexpr->exprID == exprID) {
    return true;
  }
  IvarMeExpr *ivarmeexpr = dynamic_cast<IvarMeExpr *>(meexpr);
  if (!ivarmeexpr) {
    return false;
  }
  if (base->IsUseSameSymbol(ivarmeexpr->base) && fieldID == ivarmeexpr->fieldID) {
    return true;
  }

  return false;
}

bool IvarMeExpr::IsVolatile() {
  if (volatileFromBaseSymbol) {
    return true;
  }
  MIRPtrType *ty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx));
  if (ty->PointeeVolatile()) {
    return true;
  }
  MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ty->pointedTyIdx);
  if (fieldID == 0) {
    return pointedty->HasVolatileField();
  }
  return static_cast<MIRStructType *>(pointedty)->IsFieldVolatile(fieldID);
}

bool IvarMeExpr::IsFinal() {
  MIRPtrType *ty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx));
  MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ty->pointedTyIdx);
  if (fieldID == 0) {
    return false;
  }
  return static_cast<MIRStructType *>(pointedty)->IsFieldFinal(fieldID);
}

/*
 * check paragma
 *   pragma 0 var %keySet <$Ljava_2Flang_2Fannotation_2FRCWeakRef_3B>
 */
bool IvarMeExpr::IsRCWeak() {
  MIRPtrType *ty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx));
  MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ty->pointedTyIdx);
  if (pointedty->typeKind == kTypeClass) {
    MIRClassType *classtype = static_cast<MIRClassType *>(pointedty);
    return classtype->IsFieldRCWeak(fieldID);
  }
  return false;
}

BB *VarMeExpr::GetDefByBBMeStmt(Dominance *dominance, MeStmt *&defmestmt) {
  BB *bbx = nullptr;
  switch (defBy) {
    case kDefByNo: {
      bbx = dominance->commonEntryBB;
      break;
    }
    case kDefByStmt: {
      defmestmt = def.defStmt;
      bbx = defmestmt->bb;
      break;
    }
    case kDefByMustdef: {
      MustDefMeNode *defMustDef = def.defMustDef;
      defmestmt = defMustDef->base;
      bbx = defmestmt->bb;
      break;
    }
    case kDefByChi: {
      ChiMeNode *defChi = def.defChi;
      defmestmt = defChi->base;
      bbx = defmestmt->bb;
      break;
    }
    case kDefByPhi: {
      MePhiNode *defPhi = def.defPhi;
      bbx = defPhi->defBB;
      break;
    }
    default:
      break;
  }
  return bbx;
}

bool VarMeExpr::IsPureLocal(SSATab *ssaTab, const MIRFunction *mirFunc) {
  MIRSymbol *st = ost->GetMIRSymbol();
  return st->IsLocal() && !mirFunc->IsAFormal(st);
}

bool VarMeExpr::IsZeroVersion() {
  CHECK_FATAL(vstIdx != 0, "VarMeExpr::IsZeroVersion: cannot determine because vstIdx is 0");
  return ost->zeroVersionIndex == vstIdx;
}

bool AddrofMeExpr::IsUseSameSymbol(MeExpr *meexpr) {
  AddrofMeExpr *varmeexpr = dynamic_cast<AddrofMeExpr *>(meexpr);
  if (!varmeexpr) {
    return false;
  }
  return ostIdx == varmeexpr->ostIdx;
}

bool OpMeExpr::IsUseSameSymbol(MeExpr *meexpr) {
  if (meexpr->op != op) {
    return false;
  }
  OpMeExpr *opmeexpr = static_cast<OpMeExpr *>(meexpr);
  for (uint32 i = 0; i < numOpnds; i++) {
    if (!GetOpnd(i)->IsUseSameSymbol(opmeexpr->GetOpnd(i))) {
      return false;
    }
  }
  return true;
}

bool OpMeExpr::StrengthReducible() {
  if (op != OP_mul || !IsPrimitiveInteger(primType)) {
    return false;
  }
  return GetOpnd(1)->op == OP_constval;
}

int64 OpMeExpr::SRMultiplier() {
  ASSERT(StrengthReducible(), "OpMeExpr::SRMultiplier: operation is not strength reducible");
  MIRConst *constVal = static_cast<ConstMeExpr *>(GetOpnd(1))->constVal;
  ASSERT(constVal->kind == kConstInt, "OpMeExpr::SRMultiplier: multiplier not an integer constant");
  return static_cast<MIRIntConst *>(constVal)->GetValueUnderType();
}

// first, make sure it's int const and return true if the int const great or eq 0
bool ConstMeExpr::GeZero() {
  MIRIntConst *mirconst = dynamic_cast<MIRIntConst *>(this->constVal);
  CHECK_FATAL(mirconst, "");
  return (mirconst->value >= 0);
}

bool ConstMeExpr::GtZero() {
  MIRIntConst *mirconst = dynamic_cast<MIRIntConst *>(this->constVal);
  if (!mirconst) {
    return false;
  }
  return (mirconst->value >= 1);
}

bool ConstMeExpr::IsZero() {
  MIRIntConst *mirconst = dynamic_cast<MIRIntConst *>(this->constVal);
  return (mirconst && mirconst->value == 0);
}

bool ConstMeExpr::IsOne() {
  MIRIntConst *mirconst = dynamic_cast<MIRIntConst *>(this->constVal);
  return (mirconst && mirconst->value == 1);
}

int64 ConstMeExpr::GetIntValue() {
  MIRIntConst *mirconst = dynamic_cast<MIRIntConst *>(this->constVal);
  CHECK_FATAL(mirconst, "expect int const");
  return mirconst->value;
}

void MePhiNode::Dump(IRMap *irMap) {
  OriginalSt *ost = lhs->ost;
  bool isSym = ost->IsSymbol();
  if (isSym) {
    LogInfo::MapleLogger() << "VAR:";
    ost->Dump();
  } else {
    PregIdx16 regId = static_cast<RegMeExpr*>(lhs)->regIdx;
    LogInfo::MapleLogger() << "REGVAR: " << regId;
    LogInfo::MapleLogger() << "(%"
              << irMap->mirModule->CurFunction()->pregTab->PregFromPregIdx(static_cast<uint32>(regId))->pregNo
              << ")";
  }
  LogInfo::MapleLogger() << " mx" << lhs->exprID;
  LogInfo::MapleLogger() << " = MEPHI{";
  for (uint32 i = 0; i < opnds.size(); i++) {
    LogInfo::MapleLogger() << "mx" << opnds[i]->exprID;
    if (i != opnds.size() - 1) {
      LogInfo::MapleLogger() << ",";
    }
  }
  LogInfo::MapleLogger() << "}";
  if (!isLive) {
    LogInfo::MapleLogger() << " dead";
  }
  LogInfo::MapleLogger() << std::endl;
}

void VarMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << "VAR ";
  ost->Dump();
  LogInfo::MapleLogger() << " (field)" << ost->fieldID;
  LogInfo::MapleLogger() << " mx" << exprID;
  if (IsZeroVersion()) {
    LogInfo::MapleLogger() << "<Z>";
  }
}

void RegMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << "REGINDX:" << regIdx;
  LogInfo::MapleLogger() << " %" << irMap->mirModule->CurFunction()->pregTab->PregFromPregIdx(static_cast<uint32>(regIdx))->pregNo;
  LogInfo::MapleLogger() << " mx" << exprID;
}

void AddroffuncMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << "ADDROFFUNC:";
  LogInfo::MapleLogger() << GlobalTables::GetFunctionTable().funcTable.at(puIdx)->GetName();
  LogInfo::MapleLogger() << " mx" << exprID;
}

void AddroflabelMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << "ADDROFLABEL:";
  LogInfo::MapleLogger() << " @" << irMap->mirModule->CurFunction()->GetLabelName(labelIdx);
  LogInfo::MapleLogger() << " mx" << exprID;
}

void GcmallocMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
  LogInfo::MapleLogger() << " ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->Dump(0);
  LogInfo::MapleLogger() << " mx" << exprID;
  LogInfo::MapleLogger() << " ";
}

void ConstMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << "CONST";
  LogInfo::MapleLogger() << " ";
  constVal->Dump();
  LogInfo::MapleLogger() << " mx" << exprID;
}

void ConststrMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << "CONSTSTR";
  LogInfo::MapleLogger() << " ";
  LogInfo::MapleLogger() << strIdx.GetIdx();
  LogInfo::MapleLogger() << " mx" << exprID;
}

void Conststr16MeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << "CONSTSTR16";
  LogInfo::MapleLogger() << " ";
  LogInfo::MapleLogger() << strIdx.GetIdx();
  LogInfo::MapleLogger() << " mx" << exprID;
}

void SizeoftypeMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
  LogInfo::MapleLogger() << " TYIDX:" << tyIdx.GetIdx();
  MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  mirType->Dump(0);
  LogInfo::MapleLogger() << " mx" << exprID;
}

void FieldsDistMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << kOpcodeInfo.GetTableItemAt(op).name << " " << GetPrimTypeName(primType);
  LogInfo::MapleLogger() << " TYIDX:" << tyIdx.GetIdx();
  MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  mirType->Dump(0);
  LogInfo::MapleLogger() << " (field)" << fieldID1;
  LogInfo::MapleLogger() << " (field)" << fieldID2;
  LogInfo::MapleLogger() << " mx" << exprID;
}

void AddrofMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << "ADDROF:";
  irMap->ssaTab->GetOriginalStFromid(ostIdx)->Dump();
  LogInfo::MapleLogger() << " (field)" << fieldID;
  LogInfo::MapleLogger() << " mx" << exprID;
}

void OpMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << "OP " << kOpcodeInfo.GetName(op);
  LogInfo::MapleLogger() << " mx" << exprID;
  LogInfo::MapleLogger() << std::endl;
  CHECK_FATAL(GetOpnd(0) != nullptr, "OpMeExpr::Dump: cannot have 0 operand");
  PrintIndentation(indent + 1);
  LogInfo::MapleLogger() << "opnd[0] = ";
  GetOpnd(0)->Dump(irMap, indent + 1);
  if (GetOpnd(1)) {
    LogInfo::MapleLogger() << std::endl;
  } else {
    return;
  }
  PrintIndentation(indent + 1);
  LogInfo::MapleLogger() << "opnd[1] = ";
  GetOpnd(1)->Dump(irMap, indent + 1);
  if (GetOpnd(2)) {
    LogInfo::MapleLogger() << std::endl;
  } else {
    return;
  }
  PrintIndentation(indent + 1);
  LogInfo::MapleLogger() << "opnd[2] = ";
  GetOpnd(2)->Dump(irMap, indent + 1);
}

void IvarMeExpr::Dump(IRMap *irMap, int32 indent) {
  LogInfo::MapleLogger() << "IVAR mx" << exprID;
  LogInfo::MapleLogger() << " " << GetPrimTypeName(primType);
  LogInfo::MapleLogger() << " TYIDX:" << tyIdx.GetIdx();
  MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  mirType->Dump(0);
  LogInfo::MapleLogger() << " (field)" << fieldID << std::endl;
  PrintIndentation(indent + 1);
  LogInfo::MapleLogger() << "base = ";
  base->Dump(irMap, indent + 1);
  LogInfo::MapleLogger() << endl;
  PrintIndentation(indent + 1);
  LogInfo::MapleLogger() << "- MU: {";
  if (mu != nullptr) {
    mu->Dump(irMap);
  }
  LogInfo::MapleLogger() << "}";
}

void NaryMeExpr::Dump(IRMap *irMap, int32 indent) {
  CHECK_FATAL(static_cast<uint32>(numOpnds) == opnds.size(), "");
  if (op == OP_array) {
    LogInfo::MapleLogger() << "ARRAY ";
  } else if (op == OP_intrinsicop) {
    LogInfo::MapleLogger() << GetIntrinsicName(intrinsic);
  } else {
    CHECK_FATAL(op == OP_intrinsicopwithtype, "NaryMeExpr has bad opcode");
    LogInfo::MapleLogger() << "INTRINOPWTY[" << intrinsic << "]";
  }
  LogInfo::MapleLogger() << " mx" << exprID << std::endl;
  for (int32 i = 0; i < numOpnds; i++) {
    PrintIndentation(indent + 1);
    LogInfo::MapleLogger() << "opnd[" << i << "] = ";
    GetOpnd(i)->Dump(irMap, indent + 1);
    if (i != numOpnds - 1) {
      LogInfo::MapleLogger() << std::endl;
    }
  }
}

MeExpr *DassignMeStmt::GetLhsRef(SSATab *ssaTab, bool excludelocalrefvar) {
  ScalarMeExpr *l = GetVarLhs();
  if (l->primType != PTY_ref)
    return nullptr;
  OriginalSt *ost = lhs->ost;
  if (ost->ignoreRC)
    return nullptr;
  if (excludelocalrefvar && ost->isLocal)
    return nullptr;
  return l;
}

MeExpr *MaydassignMeStmt::GetLhsRef(SSATab *ssaTab, bool excludelocalrefvar) {
  ScalarMeExpr *lhs = GetVarLhs();
  if (lhs->primType != PTY_ref)
    return nullptr;
  OriginalSt *ost = lhs->ost;
  if (ost->ignoreRC)
    return nullptr;
  if (excludelocalrefvar && ost->isLocal)
    return nullptr;
  return lhs;
}

MeExpr *IassignMeStmt::GetLhsRef(SSATab *ssaTab, bool excludelocalrefvar) {
  MIRType *basetype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(lhsVar->tyIdx);
  MIRType *ptype = static_cast<MIRPtrType *>(basetype)->GetPointedType();
  MIRStructType *structType = dynamic_cast<MIRStructType *>(ptype);
  if (!structType) {
    if (dynamic_cast<MIRPtrType *>(ptype)) {
      if (lhsVar->fieldID == 0) {
        if (static_cast<MIRPtrType *>(ptype)->primType != PTY_ref) {
          return nullptr;
        }
      } else {
        MIRType *pptype = static_cast<MIRPtrType *>(ptype)->GetPointedType();
        TyIdx ftyidx =
            static_cast<MIRStructType *>(pptype)->TraverseToField(lhsVar->fieldID).second.first;
        if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx)->primType != PTY_ref) {
          return nullptr;
        };
      }
    } else if (ptype->GetKind() == kTypeJArray) {
      MIRType *pptype = static_cast<MIRPtrType *>(ptype)->GetPointedType();
      if (static_cast<MIRPtrType *>(pptype)->primType != PTY_ref) {
        return nullptr;
      }
    } else {
      return nullptr;
    }
  } else {
    if (lhsVar->fieldID == 0) {
      return nullptr;  // struct assign is not ref
    }
    TyIdx ftyidx = structType->TraverseToField(lhsVar->fieldID).second.first;
    if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx)->primType != PTY_ref) {
      return nullptr;
    }
  }
  return lhsVar;
}

VarMeExpr *AssignedPart::GetAssignedPartLhsRef(SSATab *ssaTab, bool excludelocalrefvar) {
  if (mustDefList.empty()) {
    return nullptr;
  }
  MeExpr *assignedlhs = mustDefList.front().lhs;
  VarMeExpr *thelhs = dynamic_cast<VarMeExpr *>(assignedlhs);
  if (thelhs == nullptr)
    return nullptr;
  if (thelhs->primType != PTY_ref) {
    return nullptr;
  }
  const OriginalSt *ost = thelhs->ost;
  if (ost->ignoreRC) {
    return nullptr;
  }
  if (excludelocalrefvar && ost->isLocal) {
    return nullptr;
  }
  return thelhs;
}

// default Dump
void MeStmt::Dump(IRMap *irMap) {
  if (op == OP_comment) {
    return;
  }
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << std::endl;
}

// get the real next mestmt that is not a comment
MeStmt *MeStmt::GetNextMeStmt() {
  MeStmt *nextmestmt = next;
  while (nextmestmt && nextmestmt->op == OP_comment) {
    nextmestmt = nextmestmt->next;
  }
  return nextmestmt;
}

// get the real prev mestmt that is not a comment
MeStmt *MeStmt::GetPrevMeStmt() {
  MeStmt *prevmestmt = prev;
  while (prevmestmt && prevmestmt->op == OP_comment) {
    prevmestmt = prevmestmt->prev;
  }
  return prevmestmt;
}

void AssignMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << " ";
  lhs->Dump(irMap);
  LogInfo::MapleLogger() << std::endl;
  PrintIndentation(5);
  LogInfo::MapleLogger() << "rhs = ";
  rhs->Dump(irMap, 5);
  if (needIncref) {
    LogInfo::MapleLogger() << " [RC+]";
  }
  if (needDecref) {
    LogInfo::MapleLogger() << " [RC-]";
  }
  LogInfo::MapleLogger() << std::endl;
}

void DassignMeStmt::Dump(IRMap *irMap) {
  AssignMeStmt::Dump(irMap);
  DumpChiList(irMap, chiList);
}

void MaydassignMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << std::endl;
  PrintIndentation(5);
  LogInfo::MapleLogger() << "rhs = ";
  rhs->Dump(irMap, 5);
  if (needIncref) {
    LogInfo::MapleLogger() << " [RC+]";
  }
  if (needDecref) {
    LogInfo::MapleLogger() << " [RC-]";
  }
  LogInfo::MapleLogger() << std::endl;
  DumpChiList(irMap, chiList);
}

void ChiMeNode::Dump(IRMap *irMap) {
  VarMeExpr *melhs = static_cast<VarMeExpr *>(lhs);
  VarMeExpr *merhs = static_cast<VarMeExpr *>(rhs);
  if (!DumpOptions::simpleDump) {
    LogInfo::MapleLogger() << "VAR:";
    melhs->ost->Dump();
  }
  LogInfo::MapleLogger() << " mx" << melhs->exprID << " = CHI{";
  LogInfo::MapleLogger() << "mx" << merhs->exprID << "}";
}

void DumpMuList(IRMap *irMap, MapleMap<OStIdx, ScalarMeExpr *> &mulist, int32 indent) {
  if (mulist.empty()) {
    return;
  }
  int count = 0;
  LogInfo::MapleLogger() << "---- MULIST: { ";
  for (MapleMap<OStIdx, ScalarMeExpr *>::iterator it = mulist.begin();;) {
    if (!DumpOptions::simpleDump) {
      (*it).second->Dump(irMap);
    } else {
      LogInfo::MapleLogger() << "mx" << (*it).second->exprID;
    }
    it++;
    if (it == mulist.end()) {
      break;
    } else {
      LogInfo::MapleLogger() << ", ";
    }
    if (DumpOptions::dumpVsymNum > 0 && ++count >= DumpOptions::dumpVsymNum) {
      LogInfo::MapleLogger() << " ... ";
      break;
    }
  }
  LogInfo::MapleLogger() << " }\n";
}

void DumpChiList(IRMap *irMap, MapleMap<OStIdx, ChiMeNode *> &chilist) {
  if (chilist.empty()) {
    return;
  }
  int count = 0;
  LogInfo::MapleLogger() << "---- CHILIST: { ";
  for (MapleMap<OStIdx, ChiMeNode *>::iterator it = chilist.begin();;) {
    it->second->Dump(irMap);
    it++;
    if (it == chilist.end()) {
      break;
    } else {
      LogInfo::MapleLogger() << ", ";
    }
    if (DumpOptions::dumpVsymNum > 0 && count++ >= DumpOptions::dumpVsymNum) {
      LogInfo::MapleLogger() << " ... ";
      break;
    }
  }
  LogInfo::MapleLogger() << " }\n";
}

void IassignMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << std::endl;
  PrintIndentation(5);
  LogInfo::MapleLogger() << "lhs = ";
  lhsVar->Dump(irMap, 5);
  LogInfo::MapleLogger() << std::endl;
  PrintIndentation(5);
  LogInfo::MapleLogger() << "rhs = ";
  rhs->Dump(irMap, 5);
  if (needIncref) {
    LogInfo::MapleLogger() << " [RC+]";
  }
  if (needDecref) {
    LogInfo::MapleLogger() << " [RC-]";
  }
  LogInfo::MapleLogger() << std::endl;
  DumpChiList(irMap, chiList);
}

void NaryMeStmt::DumpOpnds(IRMap *irMap) {
  for (uint32 i = 0; i < opnds.size(); i++) {
    PrintIndentation(5);
    LogInfo::MapleLogger() << "opnd[" << i << "] = ";
    opnds[i]->Dump(irMap, 5);
    LogInfo::MapleLogger() << std::endl;
  }
}

void NaryMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << " ";
  DumpOpnds(irMap);
}

void CallMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << " ";
  if (tyIdx != 0) {
    LogInfo::MapleLogger() << " TYIDX:" << tyIdx.GetIdx();
    MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
    mirType->Dump(0);
  }
  // target function name
  CHECK(puIdx < GlobalTables::GetFunctionTable().funcTable.size(), "index out of range in CallMeStmt::Dump");
  MIRFunction *func = GlobalTables::GetFunctionTable().funcTable[puIdx];
  LogInfo::MapleLogger() << NameMangler::DecodeName(func->GetName()) << std::endl;
  DumpOpnds(irMap);
  DumpMuList(irMap, muList, 0);
  DumpChiList(irMap, chiList);
  DumpAssignedPart(irMap);
}

void IcallMeStmt::Dump(IRMap *irMap)
{
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << " ";
  LogInfo::MapleLogger() << " TYIDX:" << retTyIdx.GetIdx();
  DumpOpnds(irMap);
  DumpMuList(irMap, muList, 0);
  DumpChiList(irMap, chiList);
  DumpAssignedPart(irMap);
}

void AssignedPart::DumpAssignedPart(IRMap *irMap) {
  LogInfo::MapleLogger() << "    assignedpart: {";
  MapleVector<MustDefMeNode>::iterator it;
  for (it = mustDefList.begin(); it != mustDefList.end(); it++) {
    MeExpr *lhsVar = (*it).lhs;
    lhsVar->Dump(irMap);
  }
  if (needIncref) {
    LogInfo::MapleLogger() << " [RC+]";
  }
  if (needDecref) {
    LogInfo::MapleLogger() << " [RC-]";
  }
  LogInfo::MapleLogger() << "}\n";
}

void IntrinsiccallMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << " ";
  LogInfo::MapleLogger() << "TYIDX:" << tyIdx.GetIdx();
  MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  if (mirType != nullptr) {
    mirType->Dump(0);
  }
  LogInfo::MapleLogger() << GetIntrinsicName(intrinsic) << std::endl;
  DumpOpnds(irMap);
  DumpMuList(irMap, muList, 0);
  DumpChiList(irMap, chiList);
  DumpAssignedPart(irMap);
}

void RetMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << std::endl;
  DumpOpnds(irMap);
  DumpMuList(irMap, muList, 1);
}

void CondGotoMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << std::endl;
  PrintIndentation(5);
  LogInfo::MapleLogger() << "cond: ";
  opnd->Dump(irMap, 5);
  LogInfo::MapleLogger() << std::endl;
}

void UnaryMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << std::endl;
  PrintIndentation(5);
  LogInfo::MapleLogger() << " unaryopnd: ";
  opnd->Dump(irMap, 5);
  LogInfo::MapleLogger() << std::endl;
}

void SwitchMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << std::endl;
  PrintIndentation(5);
  LogInfo::MapleLogger() << "switchOpnd: ";
  opnd->Dump(irMap, 5);
  LogInfo::MapleLogger() << std::endl;
}

void GosubMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << std::endl;
  DumpMuList(irMap, muList, 0);
  LogInfo::MapleLogger() << std::endl;
}

void ThrowMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << std::endl;
  PrintIndentation(5);
  LogInfo::MapleLogger() << "throwopnd: ";
  opnd->Dump(irMap, 5);
  LogInfo::MapleLogger() << std::endl;
  DumpMuList(irMap, muList, 0);
}

void SyncMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << std::endl;
  DumpOpnds(irMap);
  DumpMuList(irMap, muList, 0);
  DumpChiList(irMap, chiList);
}

bool MeStmt::IsTheSameWorkcand(MeStmt *mestmt) {
  if (op != mestmt->op) {
    return false;
  }
  if (op == OP_dassign) {
    if (this->GetVarLhs()->ost != mestmt->GetVarLhs()->ost) {
      return false;
    }
  } else if (op == OP_intrinsiccallwithtype) {
    if (static_cast<IntrinsiccallMeStmt *>(this)->tyIdx !=
        static_cast<IntrinsiccallMeStmt *>(mestmt)->tyIdx) {
      return false;
    }
    if (static_cast<IntrinsiccallMeStmt *>(this)->intrinsic !=
        static_cast<IntrinsiccallMeStmt *>(mestmt)->intrinsic) {
      return false;
    }
  } else if (op == OP_callassigned) {
    CallMeStmt *thiscass = static_cast<CallMeStmt *>(this);
    CallMeStmt *cass = static_cast<CallMeStmt *>(mestmt);
    if (thiscass->puIdx != cass->puIdx) {
      return false;
    }
    if (thiscass->mustDefList.size() != cass->mustDefList.size()) {
      return false;
    }
    if (!thiscass->mustDefList.empty()) {
      VarMeExpr *thisvarmeexpr = static_cast<VarMeExpr *>(thiscass->mustDefList.front().lhs);
      VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(cass->mustDefList.front().lhs);
      if (thisvarmeexpr->ost != varmeexpr->ost) {
        return false;
      }
    }
  }
  // check the operands
  for (int32 i = 0; i < NumMeStmtOpnds(); i++) {
    CHECK_FATAL(GetMeStmtOpnd(i) != nullptr, "null ptr check");
    if (!GetMeStmtOpnd(i)->IsUseSameSymbol(mestmt->GetMeStmtOpnd(i))) {
      return false;
    }
  }
  return true;
}

bool AssertMeStmt::IsTheSameWorkcand(AssertMeStmt *assmestmt) {
  if (op != assmestmt->op) {
    return false;
  }
  if (GetArrayExpr() != assmestmt->GetArrayExpr()) {
    return false;
  }
  if (!GetIndexExpr()->IsUseSameSymbol(assmestmt->GetIndexExpr())) {
    return false;
  }
  return true;
}

void AssertMeStmt::Dump(IRMap *irMap) {
  LogInfo::MapleLogger() << "||MEIR|| " << kOpcodeInfo.GetName(op) << std::endl;
  PrintIndentation(5);
  LogInfo::MapleLogger() << "opnd[0] = ";
  opnds[0]->Dump(irMap, 5);
  LogInfo::MapleLogger() << std::endl;
  PrintIndentation(5);
  LogInfo::MapleLogger() << "opnd[1] = ";
  opnds[1]->Dump(irMap, 5);
  LogInfo::MapleLogger() << std::endl;
}

bool ScalarMeExpr::IsUseSameSymbol(MeExpr *meexpr) {
  ScalarMeExpr *expr = dynamic_cast<ScalarMeExpr *>(meexpr);
  if (!expr) {
    return false;
  }
  return ost == expr->ost;
}

BB *ScalarMeExpr::DefByBB() {
  switch (defBy) {
    case kDefByNo:
      return nullptr;
    case kDefByStmt:
      CHECK_FATAL(def.defStmt, "DefByBB: defStmt cannot be nullptr");
      return def.defStmt->bb;
    case kDefByPhi:
      CHECK_FATAL(def.defPhi, "DefByBB: defPhi cannot be nullptr");
      return def.defPhi->defBB;
    case kDefByChi: {
      CHECK_FATAL(def.defChi, "DefByBB: defChi cannot be nullptr");
      CHECK_FATAL(def.defChi->base, "DefByBB: defChi->base cannot be nullptr");
      return def.defChi->base->bb;
    }
    case kDefByMustdef: {
      CHECK_FATAL(def.defMustDef, "DefByBB: defMustDef cannot be nullptr");
      CHECK_FATAL(def.defMustDef->base, "DefByBB: defMustDef->base cannot be nullptr");
      return def.defMustDef->base->bb;
    }
    default:
      CHECK_FATAL(false, "var define unknown");
  }
}

bool VarMeExpr::IsVolatile(SSATab *ssaTab) {
  if (ost->ostType != OriginalSt::kSymbolOst) {
    return false;
  }
  MIRSymbol *sym = ost->GetMIRSymbol();
  if (sym->IsVolatile()) {
    return true;
  }
  MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
  if (ost->fieldID == 0) {
    return (ty->HasVolatileField());
  }
  MIRStructType *structty = static_cast<MIRStructType *>(ty);
  return structty->IsFieldVolatile(ost->fieldID);
}

MeExpr* MeExpr::FindSymAppearance(OStIdx oidx) {
  if (meOp == kMeOpVar || meOp == kMeOpReg) {
    if (static_cast<ScalarMeExpr *>(this)->ost->index == oidx)
      return this;
    return nullptr;
  }
  for (int32 i = 0; i < NumMeExprOpnds(); i++) {
    MeExpr *retx = GetOpnd(i)->FindSymAppearance(oidx);
    if (retx != nullptr)
      return retx;
  }
  return nullptr;
}

bool MeExpr::SymAppears(OStIdx oidx) {
  return FindSymAppearance(oidx) != nullptr;
}

bool MeExpr::HasIvar() {
  if (meOp == kMeOpIvar) {
    return true;
  }
  for (int32 i = 0; i < NumMeExprOpnds(); i++) {
    if (GetOpnd(i)->HasIvar()) {
      return true;
    }
  }
  return false;
}

bool MeExpr::IsJavaMerge() {
  if (op != OP_intrinsicop) {
    return false;
  }
  NaryMeExpr *naryx = static_cast<NaryMeExpr *>(this);
  if (naryx->intrinsic != INTRN_JAVA_MERGE) {
    return false;
  }
  if (naryx->numOpnds != 1) {
    return false;
  }
  if (naryx->GetOpnd(0)->meOp == kMeOpVar || naryx->GetOpnd(0)->meOp == kMeOpIvar) {
    return true;
  }
  if (naryx->GetOpnd(0)->meOp == kMeOpReg) {
    RegMeExpr *r = static_cast<RegMeExpr *>(naryx->GetOpnd(0));
    return r->regIdx >= 0;
  }
  return false;
}

// check if MeExpr can be a pointer to something that requires incref for its
// assigned target
bool MeExpr::PointsToSomethingThatNeedsIncref() {
  if (op == OP_retype) {
    return true;
  }
  if (IsJavaMerge()) {
    return true;
  }
  if (meOp == kMeOpVar || meOp == kMeOpIvar) {
    return true;
  }
  if (meOp == kMeOpReg) {
    RegMeExpr *r = static_cast<RegMeExpr *>(this);
    return r->regIdx != -kSregThrownval;
  }
  return false;
}

MapleMap<OStIdx, ChiMeNode *> *GenericGetChiListFromVarMeExprInner(ScalarMeExpr *expr,
                                                                     std::unordered_set<ScalarMeExpr *> &visited) {
  if (expr == nullptr || expr->defBy == kDefByNo || visited.find(expr) != visited.end()) {
    return nullptr;
  }
  visited.insert(expr);

  if (expr->defBy == kDefByPhi) {
    MePhiNode *phime = expr->def.defPhi;
    for (ScalarMeExpr* it : phime->opnds) {
      MapleMap<OStIdx, ChiMeNode *> *chiList =
        GenericGetChiListFromVarMeExprInner(it, visited);
      if (chiList != nullptr) {
        return chiList;
      }
    }
  } else if (expr->defBy == kDefByChi) {
    return expr->def.defChi->base->GetChiList();
  } else {
    // NYI
    return nullptr;
  }
  return nullptr;
}

MapleMap<OStIdx, ChiMeNode *> *GenericGetChiListFromVarMeExpr(ScalarMeExpr *expr) {
  std::unordered_set<ScalarMeExpr *> visited;
  return GenericGetChiListFromVarMeExprInner(expr, visited);
}

void MeStmt::SetCallReturn(ScalarMeExpr *curexpr) {
  CHECK_FATAL(GetMustDefList() != nullptr, "null ptr check");
  MustDefMeNode &mustdefmenode = GetMustDefList()->front();
  mustdefmenode.UpdateLhs(curexpr);
}

bool DumpOptions::dumpEscape = false;
bool DumpOptions::simpleDump = false;
int DumpOptions::dumpVsymNum = 0;
}  // namespace maple
