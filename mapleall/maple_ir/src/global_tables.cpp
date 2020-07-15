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

#include "global_tables.h"
#include "mir_type.h"
#include "mir_symbol.h"
#include <cmath>
#include <cstring>
//#include "securec.h"

#if MIR_FEATURE_FULL

namespace maple {

TypeTable::TypeTable() : ptrTypeMap(), refTypeMap() {
  // enter the primitve types in typeTable
  typeTable.push_back(static_cast<MIRType *>(nullptr));
  ASSERT(typeTable.size() == static_cast<int>(PTY_void), "use PTY_void as the first index to type table");
  for (uint32 pti = static_cast<uint32>(PTY_void); pti <= static_cast<uint32>(PTY_v4f32); pti++) {
    MIRTypeKind defaultKind = pti == PTY_constStr ? kTypeConstString : kTypeScalar;
    MIRType *type = new MIRType(defaultKind, (PrimType)pti);
    type->tyIdx = TyIdx(pti);
    typeTable.push_back(type);
    typeHashTable.insert(type);
  }
  if (!voidPtrType) {
    voidPtrType = GetOrCreatePointerType(GetVoid(), PTY_ptr);
  }
}

TypeTable::~TypeTable() {
  for (uint32 pti = static_cast<uint32>(PTY_void); pti <= static_cast<uint32>(PTY_v4f32); pti++) {
    delete typeTable[pti];
    typeTable[pti] = nullptr;
  }
}

MIRType* TypeTable::CreateMIRTypeNode(MIRType *ptype) {
  MIRType *ntype = ptype->CopyMIRTypeNode();
  ntype->tyIdx = TyIdx(typeTable.size());
  typeTable.push_back(ntype);

  if (ptype->typeKind == kTypePointer) {
    MIRPtrType *pty = static_cast<MIRPtrType*>(ptype);
    if (pty->primType == PTY_ptr) {
      ptrTypeMap[pty->pointedTyIdx] = ntype->tyIdx;
    } else {
      refTypeMap[pty->pointedTyIdx] = ntype->tyIdx;
    }
  } else {
    typeHashTable.insert(ntype);
  }
  return ntype;
}

// is_create means force to create a new ty
MIRType* TypeTable::GetOrCreateMIRTypeNode(MIRType *ptype) {
  if (ptype->typeKind == kTypePointer) {
    MIRPtrType *type = static_cast<MIRPtrType *>(ptype);
    auto *pMap = (type->primType == PTY_ptr ? &ptrTypeMap : &refTypeMap);
    auto *otherPMap = (type->primType == PTY_ref ? &ptrTypeMap : &refTypeMap);
    auto it = pMap->find(type->pointedTyIdx);
    if (it != pMap->end()) {
      return GetTypeFromTyIdx(it->second);
    }
    CHECK_FATAL(!((PrimType)(type->pointedTyIdx.GetIdx()) >= kPtyDerived && type->primType == PTY_ref &&
        otherPMap->find(type->pointedTyIdx) != otherPMap->end()), "GetOrCreateMIRType: ref pointed-to type %d has previous ptr occurrence", type->pointedTyIdx.GetIdx());
    return CreateMIRTypeNode(ptype);
  }

  auto it = typeHashTable.find(ptype);
  if (it != typeHashTable.end()) {
    return *it;
  }
  return CreateMIRTypeNode(ptype);
}

MIRType *TypeTable::voidPtrType = nullptr;

// get or create a type that pointing to pointedTyIdx
MIRType *TypeTable::GetOrCreatePointerType(TyIdx pointedTyIdx, PrimType pty) {

  MIRPtrType type(pointedTyIdx, pty);
  TyIdx tyIdx = GetOrCreateMIRType(&type);
  ASSERT(tyIdx.GetIdx() < typeTable.size(), "index out of range in TypeTable::GetOrCreatePointerType");
  return typeTable.at(tyIdx.GetIdx());
}

MIRType *TypeTable::GetOrCreatePointerType(const MIRType *pointTo, PrimType pty) {
  if (pointTo->primType == PTY_constStr) {
    pty = PTY_ptr;
  }
  return GetOrCreatePointerType(pointTo->tyIdx, pty);
}

MIRType *TypeTable::GetPointedTypeIfApplicable(MIRType *type) const {
  if (type->typeKind != kTypePointer) {
    return type;
  }
  MIRPtrType *ptype = static_cast<MIRPtrType *>(type);
  return GetTypeFromTyIdx(ptype->pointedTyIdx);
}

MIRArrayType *TypeTable::GetOrCreateArrayType(const MIRType *elem, uint8_t dim, const uint32_t *sizeArray) {
  MIRArrayType type;
  type.eTyIdx = elem->tyIdx;
  type.dim = dim;

  if (sizeArray) {
    for (uint8_t i = 0; i < dim; i++) {
      type.sizeArray[i] = sizeArray[i];
    }
  } else {
    for (uint8_t i = 0; i < dim; i++) {
      type.sizeArray[i] = 0;
    }
  }

  TyIdx tyIdx = GetOrCreateMIRType(&type);
  return static_cast<MIRArrayType*>(typeTable.at(tyIdx.GetIdx()));
}

MIRArrayType *TypeTable::GetOrCreateArrayType(const MIRType *elem, uint32_t size) {
  return GetOrCreateArrayType(elem, 1, &size);
}

MIRType *TypeTable::GetOrCreateFarrayType(const MIRType *elem) {
  MIRFarrayType type;
  type.elemTyIdx = elem->tyIdx;
  TyIdx tyIdx = GetOrCreateMIRType(&type);
  return typeTable.at(tyIdx.GetIdx());
}

MIRType *TypeTable::GetOrCreateJarrayType(const MIRType *elem) {
  MIRJarrayType type;
  type.elemTyIdx = elem->tyIdx;
  TyIdx tyIdx = GetOrCreateMIRType(&type);
  return typeTable.at(tyIdx.GetIdx());
}

MIRType *TypeTable::GetOrCreateFunctionType(MIRModule *module, TyIdx retTyidx, std::vector<TyIdx> &vecType,
                                             std::vector<TypeAttrs> &vecAttrs, bool isVarg, bool isCreate) {
  MIRFuncType *funcType = module->memPool->New<MIRFuncType>(retTyidx, vecType, vecAttrs);
  funcType->isVarArgs = isVarg;
  TyIdx tyIdx = isCreate ? CreateMIRType(funcType) : GetOrCreateMIRType(funcType);
  return typeTable.at(tyIdx.GetIdx());
}

MIRType *TypeTable::GetOrCreateStructOrUnion(const char *name, const FieldVector &fields,
                                      const FieldVector &prntFields, MIRModule *module, bool isUnion, bool isCreate) {
  MIRStructType type(isUnion ? kTypeUnion : kTypeStruct);
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(name);
  type.fields = fields;
  type.parentFields = prntFields;
  TyIdx tyIdx = isCreate ? CreateMIRType(&type) : GetOrCreateMIRType(&type);
  // Global?
  module->typeNameTab->SetGStrIdxToTyIdx(strIdx, tyIdx);
  module->typeDefOrder.push_back(strIdx);
  if (typeTable[tyIdx.GetIdx()]->nameStrIdx == GStrIdx(0)) {
    typeTable[tyIdx.GetIdx()]->nameStrIdx = strIdx;
  }
  return typeTable.at(tyIdx.GetIdx());
}

void TypeTable::PushIntoFieldVector(FieldVector *fields, const char *name, MIRType* type) {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(name);
  fields->push_back(FieldPair(strIdx, TyidxFieldAttrPair(type->GetTypeIndex(), FieldAttrs())));
}

TyIdx TypeTable::CreatePlainStructType(MIRStructType *metaType, const char *name, MIRModule *module) {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(name);
  TyIdx tyIdx = CreateMIRType(metaType);
  // Global?
  module->typeNameTab->SetGStrIdxToTyIdx(strIdx, tyIdx);
  module->typeDefOrder.push_back(strIdx);
  if (typeTable[tyIdx.GetIdx()]->nameStrIdx == GStrIdx(0)) {
    CHECK_FATAL(tyIdx.GetIdx() < typeTable.size(), "");
    typeTable[tyIdx.GetIdx()]->nameStrIdx = strIdx;
  }

  return tyIdx;
}

MIRType *TypeTable::GetOrCreateClassOrInterface(const char *name, MIRModule *module, bool isCreate, bool forClass) {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(name);
  TyIdx tyIdx = module->typeNameTab->GetTyIdxFromGStrIdx(strIdx);

  if (!isCreate) {
    return typeTable[tyIdx.GetIdx()];
  }

  if (!tyIdx.GetIdx()) {
    if (forClass) {
      MIRClassType type(kTypeClassIncomplete); // for class type
      tyIdx = CreateMIRType(&type);
    } else {
      MIRInterfaceType type(kTypeInterfaceIncomplete); // for interface type
      tyIdx = CreateMIRType(&type);
    }
    module->typeDefOrder.push_back(strIdx);
  }

  // Global?
  module->typeNameTab->SetGStrIdxToTyIdx(strIdx, tyIdx);
  if (typeTable[tyIdx.GetIdx()]->nameStrIdx == GStrIdx(0)) {
    typeTable[tyIdx.GetIdx()]->nameStrIdx = strIdx;
  }
  return typeTable.at(tyIdx.GetIdx());
}

void TypeTable::AddFieldToStructType(MIRStructType *structType, const char *fieldName, MIRType *fieldType) {
  if (structType && fieldType) {
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(fieldName);
    FieldAttrs fldattrs;
    fldattrs.SetAttr(FLDATTR_final);  // Mark compiler-generated struct fields as final to improve AliasAnalysis
    structType->fields.push_back(FieldPair(strIdx, TyidxFieldAttrPair(fieldType->tyIdx, fldattrs)));
  }
}

FPConstTable::FPConstTable() : floatConstTable(), doubleConstTable() {
  // set up the FP constants
  nanFloatConst = new MIRFloatConst(NAN, GlobalTables::GetTypeTable().typeTable.at(PTY_f32));
  infFloatConst = new MIRFloatConst(INFINITY, GlobalTables::GetTypeTable().typeTable.at(PTY_f32));
  minusInfFloatConst = new MIRFloatConst(-INFINITY, GlobalTables::GetTypeTable().typeTable.at(PTY_f32));
  minusZeroFloatConst = new MIRFloatConst(-0.0, GlobalTables::GetTypeTable().typeTable.at(PTY_f32));
  nanDoubleConst = new MIRDoubleConst(NAN, GlobalTables::GetTypeTable().typeTable.at(PTY_f64));
  infDoubleConst = new MIRDoubleConst(INFINITY, GlobalTables::GetTypeTable().typeTable.at(PTY_f64));
  minusInfDoubleConst = new MIRDoubleConst(-INFINITY, GlobalTables::GetTypeTable().typeTable.at(PTY_f64));
  minusZeroDoubleConst = new MIRDoubleConst(-0.0, GlobalTables::GetTypeTable().typeTable.at(PTY_f64));
}

MIRFloatConst *FPConstTable::GetOrCreateFloatConst(float fval) {
  if (std::isnan(fval)) {
    return nanFloatConst;
  }
  if (std::isinf(fval)) {
    if (fval < 0) {
      return minusInfFloatConst;
    } else {
      return infFloatConst;
    }
  }
  if (fval == 0.0 && std::signbit(fval)) {
    return minusZeroFloatConst;
  }

  auto it = floatConstTable.find(fval);
  if (it == floatConstTable.end()) {
    // create a new one
    MIRFloatConst *fconst = new MIRFloatConst(fval, GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_f32));
    floatConstTable[fval] = fconst;
    return fconst;
  } else {
    return it->second;
  }
}

MIRDoubleConst *FPConstTable::GetOrCreateDoubleConst(double fval) {
  if (std::isnan(fval)) {
    return nanDoubleConst;
  }
  if (std::isinf(fval)) {
    if (fval < 0) {
      return minusInfDoubleConst;
    } else {
      return infDoubleConst;
    }
  }
  if (fval == 0.0 && std::signbit(fval)) {
    return minusZeroDoubleConst;
  }

  auto it = doubleConstTable.find(fval);
  if (it == doubleConstTable.end()) {
    // create a new one
    MIRDoubleConst *dconst = new MIRDoubleConst(fval, GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_f64));
    doubleConstTable[fval] = dconst;
    return dconst;
  } else {
    return it->second;
  }
}

FPConstTable::~FPConstTable() {
  delete nanFloatConst;
  delete infFloatConst;
  delete minusInfFloatConst;
  delete minusZeroFloatConst;
  delete nanDoubleConst;
  delete infDoubleConst;
  delete minusInfDoubleConst;
  delete minusZeroDoubleConst;
  for (auto it : floatConstTable) {
    delete it.second;
  }
  for (auto it : doubleConstTable) {
    delete it.second;
  }
}

GSymbolTable::GSymbolTable() : module(nullptr) {
  symbolTable.push_back(static_cast<MIRSymbol *>(nullptr));
}

GSymbolTable::~GSymbolTable() {
  for (unsigned int i = 1; i < symbolTable.size(); i++) {
    MIRSymbol *symbol = symbolTable[i];
    if (symbol) {
      delete symbol;
    }
  }
}

MIRSymbol *GSymbolTable::CreateSymbol(uint8 scopeID) {
  MIRSymbol *st = new MIRSymbol(symbolTable.size(), scopeID);
  ASSERT(st, "Failed to create global symbol");
  symbolTable.push_back(st);
  module->AddSymbol(st);
  return st;
}

bool GSymbolTable::AddToStringSymbolMap(const MIRSymbol *st) {
  GStrIdx strIdx = st->GetNameStridx();
  if (strIdxToStIdxMap[strIdx].FullIdx() != 0) {
    return false;
  }
  strIdxToStIdxMap[strIdx] = st->GetStIdx();
  return true;
}

bool GSymbolTable::RemoveFromStringSymbolMap(const MIRSymbol *st) {
  if (strIdxToStIdxMap.find(st->GetNameStridx()) != strIdxToStIdxMap.end()) {
    strIdxToStIdxMap.erase(st->GetNameStridx());
    return true;
  } else {
    return false;
  }
}

void GSymbolTable::Dump(bool isLocal, int32 indent) const {
  for (unsigned int i = 1; i < symbolTable.size(); i++) {
    MIRSymbol *symbol = symbolTable[i];
    if (symbol) {
      symbol->Dump(module, isLocal, indent);
    }
  }
}

GlobalTables GlobalTables::globalTables;
GlobalTables &GlobalTables::GetGlobalTables() {
  return globalTables;
}

}  // namespace maple
#endif  // MIR_FEATURE_FULL
