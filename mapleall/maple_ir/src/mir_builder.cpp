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

#include "mir_builder.h"
#include "mir_symbol_builder.h"
#include <string>
#include <iostream>

namespace maple {

// This is for compiler-generated metadata 1-level struct
void MIRBuilder::AddIntFieldConst(const MIRStructType *sType, MIRAggConst *newConst,
                                  uint32 fieldID, int64 constValue) {
  if (sType && newConst) {
    MIRConst *fieldconst = mirModule->memPool->New<MIRIntConst>(constValue, sType->GetElemType(fieldID - 1));
    MapleVector<MIRConst *> &constvec = newConst->constVec;
    fieldconst->fieldID = fieldID;
    constvec.push_back(fieldconst);
  }
}

// This is for compiler-generated metadata 1-level struct
void MIRBuilder::AddAddrofFieldConst(const MIRStructType *sType, MIRAggConst *newConst,
                                     uint32 fieldID, const MIRSymbol *fieldSt) {
  if (sType && newConst && fieldSt) {
    AddrofNode *fieldexpr = CreateExprAddrof(0, fieldSt, mirModule->memPool);
    MIRConst *fieldconst =
      mirModule->memPool->New<MIRAddrofConst>(fieldexpr->stIdx, fieldexpr->fieldID, sType->GetElemType(fieldID - 1));
    MapleVector<MIRConst *> &constvec = newConst->constVec;
    fieldconst->fieldID = fieldID;
    constvec.push_back(fieldconst);
  }
}

// This is for compiler-generated metadata 1-level struct
void MIRBuilder::AddAddroffuncFieldConst(const MIRStructType *sType, MIRAggConst *newConst, uint32 fieldID,
                                         const MIRSymbol *funcSt) {
  if (sType && newConst && funcSt) {
    MIRConst *fieldconst = nullptr;
    MIRFunction *vmethod = funcSt->GetFunction();
    if (vmethod->IsAbstract()) {
      fieldconst = mirModule->memPool->New<MIRIntConst>(0, sType->GetElemType(fieldID - 1));
    } else {
      AddroffuncNode *addroffuncExpr = CreateExprAddroffunc(funcSt->GetFunction()->puIdx, mirModule->memPool);
      fieldconst = mirModule->memPool->New<MIRAddroffuncConst>(addroffuncExpr->puIdx, sType->GetElemType(fieldID - 1));
    }
    fieldconst->fieldID = fieldID;
    MapleVector<MIRConst *> &constvec = newConst->constVec;
    constvec.push_back(fieldconst);
  }
}

// fieldID is continuously being updated during traversal;
// when the field is found, its field id is returned via fieldID
bool MIRBuilder::TraverseToNamedField(MIRStructType *structType, GStrIdx nameIdx, uint32 &fieldID) {
  TyIdx tid(0);
  return TraverseToNamedFieldWithTypeAndMatchStyle(structType, nameIdx, tid, fieldID, 2);
}

bool MIRBuilder::IsOfSameType(MIRType *type1, MIRType *type2) {
  if (type2->typeKind != type1->typeKind) {
    return false;
  }
  switch (type1->typeKind) {
    case kTypeScalar: {
      return (type1->tyIdx == type2->tyIdx);
    }
    case kTypePointer: {
      MIRPtrType *ptrtype = static_cast<MIRPtrType *>(type1);
      MIRPtrType *ptrtypeit = static_cast<MIRPtrType *>(type2);
      if (ptrtype && ptrtypeit) {
        if (ptrtype->pointedTyIdx == ptrtypeit->pointedTyIdx) {
          return true;
        } else {
          return IsOfSameType(GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptrtype->pointedTyIdx),
                              GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptrtypeit->pointedTyIdx));
        }
      }
      return false;
    }
    case kTypeJArray: {
      MIRJarrayType *atype1 = static_cast<MIRJarrayType *>(type1);
      MIRJarrayType *atype2 = static_cast<MIRJarrayType *>(type2);
      if (atype1->GetDim() != atype2->GetDim()) {
        return false;
      }
      return IsOfSameType(atype1->GetElemType(), atype2->GetElemType());
    }
    default: {
      return type1->tyIdx == type2->tyIdx;
    }
  }
}

// traverse parent first but match self first.

void MIRBuilder::TraverseToNamedFieldWithType(MIRStructType *structType, GStrIdx nameIdx, TyIdx typeIdx,
                                              uint32 &fieldID, uint32 &idx) {
  if (!structType) {
    return;
  }

  if (structType->IsIncomplete()) {
    incompleteTypeRefedSet.insert(structType->tyIdx);
  }

  // process parent
  if (structType->typeKind == kTypeClass || structType->typeKind == kTypeClassIncomplete) {
    MIRClassType *classType = static_cast<MIRClassType *>(structType);
    MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(classType->parentTyIdx);
    MIRStructType *parentType = static_cast<MIRStructType *>(type);
    if (parentType) {
      fieldID++;
      TraverseToNamedFieldWithType(parentType, nameIdx, typeIdx, fieldID, idx);
    }
  }
  for (uint32 fieldidx = 0; fieldidx < structType->fields.size(); fieldidx++) {
    fieldID++;
    if (structType->fields[fieldidx].first == nameIdx) {
      if ((typeIdx == 0 || structType->fields[fieldidx].second.first == typeIdx)) {
        idx = fieldID;
        continue;
      }
      // for pointer type, check their pointed type
      MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(typeIdx);
      MIRType *typeit = GlobalTables::GetTypeTable().GetTypeFromTyIdx(structType->fields[fieldidx].second.first);
      if (IsOfSameType(type, typeit)) {
        idx = fieldID;
      }
    }

    MIRType *fieldtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(structType->fields[fieldidx].second.first);
    if (fieldtype->typeKind == kTypeStruct || fieldtype->typeKind == kTypeStructIncomplete) {
      MIRStructType *substructtype = static_cast<MIRStructType *>(fieldtype);
      TraverseToNamedFieldWithType(substructtype, nameIdx, typeIdx, fieldID, idx);
    } else if (fieldtype->typeKind == kTypeClass || fieldtype->typeKind == kTypeClassIncomplete) {
      MIRClassType *subclasstype = static_cast<MIRClassType *>(fieldtype);
      TraverseToNamedFieldWithType(subclasstype, nameIdx, typeIdx, fieldID, idx);
    } else if (fieldtype->typeKind == kTypeInterface || fieldtype->typeKind == kTypeInterfaceIncomplete) {
      MIRInterfaceType *subinterfacetype = static_cast<MIRInterfaceType *>(fieldtype);
      TraverseToNamedFieldWithType(subinterfacetype, nameIdx, typeIdx, fieldID, idx);
    }
  }
}

// fieldID is continuously being updated during traversal;
// when the field is found, its field id is returned via fieldID
//
// typeidx: TyIdx(0) means do not check types.
// matchStyle: 0: do not match but traverse to update fieldID
//             1: match top level field only
//             2: match any field
//             4: traverse parent first
//          0xc: do not match but traverse to update fieldID, traverse parent first, found in child
bool MIRBuilder::TraverseToNamedFieldWithTypeAndMatchStyle(MIRStructType *structType, GStrIdx nameIdx,
                                                           TyIdx typeIdx, uint32 &fieldID, unsigned matchStyle) {
  if (structType == nullptr) {
    return false;
  }

  if (structType->IsIncomplete()) {
    incompleteTypeRefedSet.insert(structType->tyIdx);
  }

  if (matchStyle & kParentFirst) {
    // process parent
    if (structType->typeKind == kTypeClass || structType->typeKind == kTypeClassIncomplete) {
      MIRClassType *classType = static_cast<MIRClassType *>(structType);
      MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(classType->parentTyIdx);
      MIRStructType *parentType = static_cast<MIRStructType *>(type);
      if (parentType) {
        fieldID++;
        if (matchStyle == (kFoundInChild | kParentFirst | kUpdateFieldID)) {
          matchStyle = kParentFirst;
          uint32 idxBackup = nameIdx.GetIdx();
          nameIdx.SetIdx(0);
          // do not match but traverse to update fieldID, traverse parent first
          TraverseToNamedFieldWithTypeAndMatchStyle(parentType, nameIdx, typeIdx, fieldID, matchStyle);
          nameIdx.SetIdx(idxBackup);
        } else if (TraverseToNamedFieldWithTypeAndMatchStyle(parentType, nameIdx, typeIdx, fieldID, matchStyle)) {
          return true;
        }
      }
    } else {
      return false;
    }
  }

  for (uint32 fieldidx = 0; fieldidx < structType->fields.size(); fieldidx++) {
    fieldID++;
    if (matchStyle && structType->fields[fieldidx].first == nameIdx) {
      if ((typeIdx == 0 || structType->fields[fieldidx].second.first == typeIdx)) {
        return true;
      }
      // for pointer type, check their pointed type
      MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(typeIdx);
      MIRType *typeit = GlobalTables::GetTypeTable().GetTypeFromTyIdx(structType->fields[fieldidx].second.first);
      if (IsOfSameType(type, typeit)) {
        return true;
      }
    }

    unsigned style = matchStyle & kMatchAnyField;
    MIRType *fieldtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(structType->fields[fieldidx].second.first);
    if (fieldtype->typeKind == kTypeStruct || fieldtype->typeKind == kTypeStructIncomplete) {
      MIRStructType *substructtype = static_cast<MIRStructType *>(fieldtype);
      if (TraverseToNamedFieldWithTypeAndMatchStyle(substructtype, nameIdx, typeIdx, fieldID, style)) {
        return true;
      }
    } else if (fieldtype->typeKind == kTypeClass || fieldtype->typeKind == kTypeClassIncomplete) {
      MIRClassType *subclasstype = static_cast<MIRClassType *>(fieldtype);
      if (TraverseToNamedFieldWithTypeAndMatchStyle(subclasstype, nameIdx, typeIdx, fieldID, style)) {
        return true;
      }
    } else if (fieldtype->typeKind == kTypeInterface || fieldtype->typeKind == kTypeInterfaceIncomplete) {
      MIRInterfaceType *subinterfacetype = static_cast<MIRInterfaceType *>(fieldtype);
      if (TraverseToNamedFieldWithTypeAndMatchStyle(subinterfacetype, nameIdx, typeIdx, fieldID, style)) {
        return true;
      }
    }
  }
  return false;
}

// fieldID is continuously being updated negatively during traversal;
// when the field is found, its negative field id is returned via fieldID
//bool MIRBuilder::TraverseToNamedParentField(MIRStructType *structType,
//    GStrIdx nameidx, int32 &fieldID) const {
//  for (uint32 fieldidx = 0; fieldidx < structType->parentFields.size(); fieldidx++) {
//    fieldID--;
//    if (structType->fields[fieldidx].first == nameidx) {
//      return true;
//    }
//  }
//  return false;
//}

FieldID MIRBuilder::GetStructFieldIdFromNameAndType(MIRType *type, const string &name, TyIdx tid, unsigned int matchStyle) {
  MIRStructType *structType = static_cast<MIRStructType *>(type);
  uint32 fieldID = 0;
  GStrIdx strIdx = GetStringIndex(name);
  if (TraverseToNamedFieldWithTypeAndMatchStyle(structType, strIdx, tid, fieldID, matchStyle)) {
    return fieldID;
  }
  return 0;
}

//void MIRBuilder::SetStructFieldIdFromFieldName(MIRType *sType, const string &name, GStrIdx newStrIdx,
//                                               const MIRType *newFieldType) {
//  MIRStructType *structType = static_cast<MIRStructType *>(sType);
//  uint32 fieldId = 0;
//  GStrIdx strIdx = GetStringIndex(name);
//
//  while (true) {
//    if (structType->GetElemStridx(fieldId) == strIdx) {
//      if (newStrIdx != 0) {
//        structType->SetElemStridx(fieldId, newStrIdx);
//      }
//      if (newFieldType) {
//        structType->SetElemTyidx(fieldId, newFieldType->tyIdx);
//      }
//      return;
//    }
//    fieldId++;
//  }
//}

MIRFunction *MIRBuilder::GetFunctionFromSymbol(MIRSymbol *funcst) const {
  if (!funcst) {
    return nullptr;
  }
  CHECK_FATAL(funcst->sKind == kStFunc, "Symbol %s is not a function symbol", funcst->GetName().c_str());
  return funcst->value.mirFunc;
}

MIRFunction *MIRBuilder::GetFunctionFromName(const string &str) {
  return GetFunctionFromSymbol(GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(str)));
}

MIRFunction *MIRBuilder::GetFunctionFromStidx(StIdx stIdx) {
  return GetFunctionFromSymbol(GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx()));
}

// when checktype is true, it means match everything the of the symbol
MIRSymbol *MIRBuilder::GetSymbol(TyIdx tyIdx, GStrIdx strIdx, MIRSymKind sKind, MIRStorageClass storageClass,
                                 uint8 scpid, MIRFunction *func, bool checktype) {
  MIRSymbol *st = nullptr;
  if (func == nullptr) {
    // global
    st = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
  } else {
    // local
    StIdx stIdx = func->symTab->GetStIdxFromStrIdx(strIdx);
    if (stIdx.Idx() != 0) {
      st = func->symTab->GetSymbolFromStIdx(stIdx.Idx());
    }
  }
  if (st && checktype) {
    CHECK_FATAL(st->tyIdx == tyIdx && sKind == st->sKind && st->storageClass == storageClass,
           "trying to create a new symbol that has the same name and tyIdx. might cause problem");
  }
  return st;
}

// when func is null, create global symbol, otherwise create local symbol
MIRSymbol *MIRBuilder::CreateSymbol(TyIdx tyIdx, GStrIdx strIdx, MIRSymKind sKind, MIRStorageClass storageClass,
                                    uint8 scpid, MIRFunction *func) {
  MIRSymbol *st = nullptr;
  if (func) {
    st = func->symTab->CreateSymbol(scpid);
  } else {
    st = GlobalTables::GetGsymTable().CreateSymbol(scpid);
  }
  st->storageClass = storageClass;
  st->sKind = sKind;
  st->SetNameStridx(strIdx);
  st->SetTyIdx(tyIdx);
  if (func) {
    func->symTab->AddToStringSymbolMap(st);
  } else {
    GlobalTables::GetGsymTable().AddToStringSymbolMap(st);
  }
  return st;
}

MIRSymbol *MIRBuilder::GetLocalDecl(const string &name) {
  return GetSymbol(TyIdx(0), GetStringIndex(name), kStVar, kScAuto, kScopeLocal, GetCurrentFunction());
}

MIRSymbol *MIRBuilder::GetLocalDecl(const char *name) {
  return GetSymbol(TyIdx(0), GetStringIndex(name), kStVar, kScAuto, kScopeLocal, GetCurrentFunction());
}

MIRSymbol *MIRBuilder::CreateLocalDecl(const string &name, MIRType *type) {
  MIRFunction *currentFunctionInner = GetCurrentFunctionNotNull();
  return MIRSymbolBuilder::Instance().CreateLocalDecl(*currentFunctionInner->symTab,
                                                      GetOrCreateStringIndex(name), *type);
}

MIRSymbol *MIRBuilder::CreateLocalDecl(const char *name, MIRType *type) {
  MIRFunction *currentFunctionInner = GetCurrentFunctionNotNull();
  return MIRSymbolBuilder::Instance().CreateLocalDecl(*currentFunctionInner->symTab,
                                                      GetOrCreateStringIndex(name), *type);
}

MIRSymbol *MIRBuilder::GetOrCreateLocalDecl(const string &name, MIRType *type) {
  MIRSymbol *st = GetLocalDecl(name);
  if (!st) {
    st = CreateLocalDecl(name, type);
  }
  return st;
}

MIRSymbol *MIRBuilder::GetOrCreateLocalDecl(const char *name, MIRType *type) {
  MIRSymbol *st = GetLocalDecl(name);
  if (!st) {
    st = CreateLocalDecl(name, type);
  }
  return st;
}

MIRSymbol *MIRBuilder::GetGlobalDecl(const std::string &str) {
  return MIRSymbolBuilder::Instance().GetGlobalDecl(GetStringIndex(str));
}

MIRSymbol *MIRBuilder::GetGlobalDecl(const char *str) {
  return MIRSymbolBuilder::Instance().GetGlobalDecl(GetStringIndex(str));
}

MIRSymbol *MIRBuilder::CreateGlobalDecl(const std::string &str, const MIRType *type, MIRStorageClass sc) {
  return MIRSymbolBuilder::Instance().CreateGlobalDecl(GetOrCreateStringIndex(str), *type, sc);
}

MIRSymbol *MIRBuilder::CreateGlobalDecl(const char *str, const MIRType *type, MIRStorageClass sc) {
  return MIRSymbolBuilder::Instance().CreateGlobalDecl(GetOrCreateStringIndex(str), *type, sc);
}


MIRSymbol *MIRBuilder::GetOrCreateGlobalDecl(const string &name, MIRType *type, MIRStorageClass storageClass) {
  MIRSymbol *st = GetGlobalDecl(name);
  if (!st) {
    st = CreateGlobalDecl(name, type, storageClass);
  }
  return st;
}

MIRSymbol *MIRBuilder::GetOrCreateGlobalDecl(const char *name, MIRType *type, MIRStorageClass storageClass) {
  MIRSymbol *st = GetGlobalDecl(name);
  if (!st) {
    st = CreateGlobalDecl(name, type, storageClass);
  }
  return st;
}

MIRSymbol *MIRBuilder::CreatePregFormalSymbol(TyIdx tyIdx, PregIdx pregidx, MIRFunction *func) {
  MIRSymbol *st = func->symTab->CreateSymbol(kScopeLocal);
  st->storageClass = kScFormal;
  st->sKind = kStPreg;
  st->SetTyIdx(tyIdx);
  MIRPregTable *pregTab = func->pregTab;
  st->value.preg = pregTab->PregFromPregIdx(pregidx);
  return st;
}

ConstvalNode *MIRBuilder::CreateIntConst(int64 val, PrimType pty) {
  ConstvalNode *constvalNode = GetCurrentFuncCodeMp()->New<ConstvalNode>();
  MIRIntConst *intConst = GetCurrentFunction()->dataMemPool->New<MIRIntConst>(val, GlobalTables::GetTypeTable().GetPrimType(pty));
  constvalNode->primType = pty;
  constvalNode->constVal = intConst;
  return constvalNode;
}

ConstvalNode *MIRBuilder::CreateFloatConst(float val) {
  ConstvalNode *constvalNode = GetCurrentFuncCodeMp()->New<ConstvalNode>();
  MIRFloatConst *fltConst = GetCurrentFunction()->dataMemPool->New<MIRFloatConst>(val,
                                                                                   GlobalTables::GetTypeTable().GetPrimType(PTY_f32));
  constvalNode->primType = PTY_f32;
  constvalNode->constVal = fltConst;
  return constvalNode;
}

ConstvalNode *MIRBuilder::CreateDoubleConst(double val) {
  ConstvalNode *constvalNode = GetCurrentFuncCodeMp()->New<ConstvalNode>();
  MIRDoubleConst *dblConst = GetCurrentFunction()->dataMemPool->New<MIRDoubleConst>(val,
                                                                                     GlobalTables::GetTypeTable().GetPrimType(PTY_f64));
  constvalNode->primType = PTY_f64;
  constvalNode->constVal = dblConst;
  return constvalNode;
}

ConstvalNode *MIRBuilder::CreateFloat128Const(const uint64 *val) {
  ConstvalNode *constvalNode = GetCurrentFuncCodeMp()->New<ConstvalNode>();
  MIRFloat128Const *f128Const =
      GetCurrentFunction()->dataMemPool->New<MIRFloat128Const>(val, GlobalTables::GetTypeTable().GetPrimType(PTY_f128));
  constvalNode->primType = PTY_f128;
  constvalNode->constVal = f128Const;
  return constvalNode;
}

ConstvalNode *MIRBuilder::CreateAddrofConst(BaseNode *e) {
  ConstvalNode *constvalNode = GetCurrentFuncCodeMp()->New<ConstvalNode>();
  ASSERT(e->op == OP_addrof, "illegal op for addrof const");
  AddrofNode *anode = static_cast<AddrofNode *>(e);
  // determine the type of 'e' and create a pointer type, accordingly
  MIRSymbol *var = GetCurrentFunction()->GetLocalOrGlobalSymbol(static_cast<AddrofNode *>(e)->stIdx);
  TyIdx ptyidx = var->GetTyIdx();
  MIRPtrType ptrtype(ptyidx);
  ptyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&ptrtype);
  MIRType *exprty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptyidx);
  MIRAddrofConst *addr = mirModule->memPool->New<MIRAddrofConst>(anode->stIdx, anode->fieldID, exprty);
  constvalNode->primType = PTY_ptr;
  constvalNode->constVal = addr;
  return constvalNode;
}

ConstvalNode *MIRBuilder::CreateAddroffuncConst(BaseNode *e) {
  ConstvalNode *constvalNode = GetCurrentFuncCodeMp()->New<ConstvalNode>();
  ASSERT(e->op == OP_addroffunc, "illegal op for addroffunc const");
  MIRFunction *f = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(static_cast<AddroffuncNode *>(e)->puIdx);
  MIRSymbol *fname = f->GetFuncSymbol();
  TyIdx ptyidx = fname->GetTyIdx();
  MIRPtrType ptrtype(ptyidx);
  ptyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&ptrtype);
  MIRType *exprty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptyidx);
  MIRAddroffuncConst *fadr = mirModule->memPool->New<MIRAddroffuncConst>(static_cast<AddroffuncNode *>(e)->puIdx, exprty);
  constvalNode->primType = PTY_ptr;
  constvalNode->constVal = fadr;
  return constvalNode;
}

ConstvalNode *MIRBuilder::CreateStrConst(BaseNode *e) {
  ConstvalNode *constvalNode = GetCurrentFuncCodeMp()->New<ConstvalNode>();
  ASSERT(e->op == OP_conststr, "illegal op for conststr const");
  UStrIdx strIdx = static_cast<ConststrNode *>(e)->strIdx;
  CHECK(PTY_u8 < GlobalTables::GetTypeTable().typeTable.size(), "index is out of range in MIRBuilder::CreateStrConst");
  TyIdx ptyidx = GlobalTables::GetTypeTable().typeTable[PTY_u8]->tyIdx;
  MIRPtrType ptrtype(ptyidx);
  ptyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&ptrtype);
  MIRType *exprty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptyidx);
  MIRStrConst *cstr = mirModule->memPool->New<MIRStrConst>(strIdx, exprty);
  constvalNode->primType = PTY_ptr;
  constvalNode->constVal = cstr;
  return constvalNode;
}

ConstvalNode *MIRBuilder::CreateStr16Const(BaseNode *e) {
  ConstvalNode *constvalNode = GetCurrentFuncCodeMp()->New<ConstvalNode>();
  ASSERT(e->op == OP_conststr16, "illegal op for conststr16 const");
  U16StrIdx strIdx = static_cast<Conststr16Node *>(e)->strIdx;
  CHECK(PTY_u16 < GlobalTables::GetTypeTable().typeTable.size(), "index out of range in MIRBuilder::CreateStr16Const");
  TyIdx ptyidx = GlobalTables::GetTypeTable().typeTable[PTY_u16]->tyIdx;
  MIRPtrType ptrtype(ptyidx);
  ptyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&ptrtype);
  MIRType *exprty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptyidx);
  MIRStr16Const *cstr = mirModule->memPool->New<MIRStr16Const>(strIdx, exprty);
  constvalNode->primType = PTY_ptr;
  constvalNode->constVal = cstr;
  return constvalNode;
}

// create a int const vector for SIMD, caller is responsible to free the memory of fromVec,
// otherwise memory leak
ConstvalNode *MIRBuilder::CreateVectorIntConst(const std::vector<int64> &fromVec, PrimType primType, MIRType *ety) {
  uint32 vecSize = fromVec.size();
  CHECK_FATAL(vecSize == 4 || vecSize == 8 || vecSize == 16, "not a valid vec size");
  ConstvalNode *constvalNode = GetCurrentFuncCodeMp()->New<ConstvalNode>();
  MIRVectorIntConst *vecIntcon = mirModule->memPool->New<MIRVectorIntConst>(vecSize, ety);
  for (uint32 i = 0; i < vecSize; i++) {
    vecIntcon->AddValue(fromVec[i], i);
  }
  constvalNode->primType = primType;
  constvalNode->constVal = vecIntcon;
  return constvalNode;
}

MIRFunction *MIRBuilder::CreateFunction(const string &name, const MIRType *returnType,
                                        const ArgVector &arguments, bool isVarg, bool createBody) {
  MIRSymbol *funcst = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  GStrIdx strIdx = GetOrCreateStringIndex(name.c_str());
  funcst->SetNameStridx(strIdx);
  if (!GlobalTables::GetGsymTable().AddToStringSymbolMap(funcst)) {
    return nullptr;
  }
  funcst->storageClass = kScText;
  funcst->sKind = kStFunc;

  MIRFunction *fn = mirModule->memPool->New<MIRFunction>(mirModule, funcst->GetStIdx());
  // mirModule->AddFunction(fn);  // we add function later
  fn->puIdx = GlobalTables::GetFunctionTable().funcTable.size();
  GlobalTables::GetFunctionTable().funcTable.push_back(fn);

  std::vector<TyIdx> funcvectype;
  std::vector<TypeAttrs> funcvecattrs;
  for (uint32 i = 0; i < arguments.size(); i++) {
    MIRType *ty = arguments[i].second;
    FormalDef formalDef(GetOrCreateStringIndex(arguments[i].first.c_str()), nullptr, ty->tyIdx, TypeAttrs());
    fn->formalDefVec.push_back(formalDef);
    funcvectype.push_back(ty->tyIdx);
    funcvecattrs.push_back(TypeAttrs());
  }
  funcst->SetTyIdx(
      GlobalTables::GetTypeTable().GetOrCreateFunctionType(mirModule, returnType->tyIdx, funcvectype, funcvecattrs, isVarg)->tyIdx);
  MIRFuncType *funcType = dynamic_cast<MIRFuncType*>(funcst->GetType());
  fn->funcType = funcType;
  funcst->SetFunction(fn);
  if (createBody) {
    fn->NewBody();
  }
  return fn;
}

SizeoftypeNode *MIRBuilder::CreateExprSizeoftype(const MIRType *type) {
  SizeoftypeNode *sizeoftypeNode = GetCurrentFuncCodeMp()->New<SizeoftypeNode>();
  sizeoftypeNode->primType = PTY_u64;
  sizeoftypeNode->tyIdx = type->tyIdx;
  return sizeoftypeNode;
}

FieldsDistNode *MIRBuilder::CreateExprFieldsDist(const MIRType *type, FieldID field1, FieldID field2) {
  FieldsDistNode *node = GetCurrentFuncCodeMp()->New<FieldsDistNode>();
  node->primType = PTY_i32;
  node->tyIdx = type->tyIdx;
  node->fieldID1 = field1;
  node->fieldID2 = field2;
  return node;
}

AddrofNode *MIRBuilder::CreateExprAddrof(FieldID fieldID, const MIRSymbol *symbol, MemPool *mp) {
  if (mp == nullptr) {
    mp = GetCurrentFuncCodeMp();
  }
  AddrofNode *addrofNode = mp->New<AddrofNode>(OP_addrof);
  addrofNode->primType = PTY_ptr;
  addrofNode->stIdx = symbol->GetStIdx();
  addrofNode->fieldID = fieldID;
  return addrofNode;
}

AddrofNode *MIRBuilder::CreateExprAddrof(FieldID fieldID, StIdx symbolStIdx, MemPool *mp) {
  if (mp == nullptr) {
    mp = GetCurrentFuncCodeMp();
  }
  AddrofNode *addrofNode = mp->New<AddrofNode>(OP_addrof);
  addrofNode->primType = PTY_ptr;
  addrofNode->stIdx = symbolStIdx;
  addrofNode->fieldID = fieldID;
  return addrofNode;
}

AddroffuncNode *MIRBuilder::CreateExprAddroffunc(PUIdx puIdx, MemPool *mp) {
  if (mp == nullptr) {
    mp = GetCurrentFuncCodeMp();
  }
  AddroffuncNode *addroffunc = mp->New<AddroffuncNode>();
  addroffunc->primType = PTY_ptr;
  addroffunc->puIdx = puIdx;
  return addroffunc;
}

PrimType MIRBuilder::UpCvt32(PrimType pty) const {
  return pty;
}

AddrofNode *MIRBuilder::CreateExprDread(const MIRType *type, FieldID fieldID, const MIRSymbol *symbol) {
  AddrofNode *drn = GetCurrentFuncCodeMp()->New<AddrofNode>(OP_dread);
  drn->stIdx = symbol->GetStIdx();
  drn->fieldID = fieldID;
  CHECK(type->tyIdx.GetIdx() < GlobalTables::GetTypeTable().typeTable.size(), "index out of range in MIRBuilder::CreateExprDread");
  drn->primType = UpCvt32(GlobalTables::GetTypeTable().typeTable[type->tyIdx.GetIdx()]->primType);
  return drn;
}

RegreadNode *MIRBuilder::CreateExprRegread(PrimType pty, PregIdx regIdx) {
  RegreadNode *regread = GetCurrentFuncCodeMp()->New<RegreadNode>();
  regread->primType = UpCvt32(pty);
  regread->regIdx = regIdx;
  return regread;
}

AddrofNode *MIRBuilder::CreateExprDread(MIRType *type, MIRSymbol *symbol) {
  return CreateExprDread(type, 0, symbol);
}

AddrofNode *MIRBuilder::CreateExprDread(MIRSymbol *symbol, uint16 fieldID) {
  MIRType *ty = symbol->GetType();
  CHECK_FATAL(fieldID == 0 || ty->typeKind == kTypeStruct || ty->typeKind == kTypeStructIncomplete, "");
  if (fieldID == 0) {
    return CreateExprDread(symbol);
  } else {
    CHECK_FATAL(false, "NYI");
  }
}

AddrofNode *MIRBuilder::CreateExprDread(MIRSymbol *symbol) {
  return CreateExprDread(symbol->GetType(), 0, symbol);
}

AddrofNode *MIRBuilder::CreateExprDread(PregIdx pregid, PrimType pty) {
  AddrofNode *dread = GetCurrentFuncCodeMp()->New<AddrofNode>(OP_dread);
  dread->primType = UpCvt32(pty);
  dread->stIdx.SetFullIdx(pregid);

  return dread;
}

IreadNode *MIRBuilder::CreateExprIread(const MIRType *returnType, const MIRType *ptrType,
    FieldID fieldID, BaseNode *addr) {
  IreadNode *irn = GetCurrentFuncCodeMp()->New<IreadNode>(OP_iread);
  TyIdx ptrTypeIdx = ptrType->tyIdx;
  TyIdx returnTypeIdx = returnType->tyIdx;
  CHECK(returnTypeIdx.GetIdx() < GlobalTables::GetTypeTable().typeTable.size(), "index out of range in MIRBuilder::CreateExprIread");
  irn->primType = UpCvt32(GlobalTables::GetTypeTable().typeTable[returnTypeIdx.GetIdx()]->primType);
  irn->tyIdx = ptrTypeIdx;
  if (fieldID == 0 && ptrType->primType == PTY_agg) {
    std::cerr << "Error: Fieldid should not be 0 when trying to iread a field from type "
              << GlobalTables::GetStrTable().GetStringFromStrIdx(ptrType->nameStrIdx) << std::endl;
    exit(-1);
  }
  irn->fieldID = fieldID;
  irn->uOpnd = addr;
  return irn;
}

IreadoffNode *MIRBuilder::CreateExprIreadoff(PrimType pty, int32 offset, BaseNode *opnd0) {
  IreadoffNode *ireadoff = GetCurrentFuncCodeMp()->New<IreadoffNode>();
  ireadoff->primType = UpCvt32(pty);
  ireadoff->offset = offset;
  ireadoff->uOpnd = opnd0;
  return ireadoff;
}

IreadFPoffNode *MIRBuilder::CreateExprIreadFPoff(PrimType pty, int32 offset) {
  IreadFPoffNode *ireadoff = GetCurrentFuncCodeMp()->New<IreadFPoffNode>();
  ireadoff->primType = UpCvt32(pty);
  ireadoff->offset = offset;
  return ireadoff;
}

IaddrofNode *MIRBuilder::CreateExprIaddrof(const MIRType *returnType, const MIRType *ptrType, FieldID fieldID, BaseNode *addr) {
  IaddrofNode *iaddrof = CreateExprIread(returnType, ptrType, fieldID, addr);
  iaddrof->op = OP_iaddrof;
  return iaddrof;
}

IaddrofNode *MIRBuilder::CreateExprIaddrof(PrimType returnTypePty, TyIdx ptrTypeIdx, FieldID fieldID,
                                           BaseNode *addr) {
  IaddrofNode *ian = GetCurrentFuncCodeMp()->New<IreadNode>(OP_iaddrof);
  ian->primType = returnTypePty;
  ian->tyIdx = ptrTypeIdx;
  ian->fieldID = fieldID;
  ian->uOpnd = addr;
  return ian;
}

UnaryNode *MIRBuilder::CreateExprUnary(Opcode opcode, const MIRType *type, BaseNode *opnd) {
  UnaryNode *un = GetCurrentFuncCodeMp()->New<UnaryNode>(opcode);
  un->primType = type->GetPrimType();
  un->uOpnd = opnd;
  return un;
}

GCMallocNode *MIRBuilder::CreateExprGCMalloc(Opcode opcode, const MIRType *ptype, const MIRType *type) {
  GCMallocNode *un = GetCurrentFuncCodeMp()->New<GCMallocNode>(opcode);
  un->primType = ptype->GetPrimType();
  un->tyIdx = type->tyIdx;
  return un;
}

JarrayMallocNode *MIRBuilder::CreateExprJarrayMalloc(Opcode opcode, const MIRType *ptype,
                                                     const MIRType *type, BaseNode *opnd) {
  JarrayMallocNode *un = GetCurrentFuncCodeMp()->New<JarrayMallocNode>(opcode);
  un->primType = ptype->GetPrimType();
  un->tyIdx = type->tyIdx;
  un->uOpnd = opnd;
  return un;
}

JarrayMallocNode *MIRBuilder::CreateExprSTACKJarrayMalloc(const MIRType *ptype, const MIRType *type,
                                                               BaseNode *opnd) {
  JarrayMallocNode *un = GetCurrentFuncCodeMp()->New<JarrayMallocNode>(OP_stackmallocjarray);
  un->primType = ptype->GetPrimType();
  un->tyIdx = type->tyIdx;
  un->uOpnd = opnd;
  return un;
}

TypeCvtNode *MIRBuilder::CreateExprTypeCvt(Opcode o, const MIRType *type, const MIRType *fromType, BaseNode *opnd) {
  TypeCvtNode *un = GetCurrentFuncCodeMp()->New<TypeCvtNode>(o);
  un->primType = type->GetPrimType();
  un->fromPrimType = fromType->GetPrimType();
  un->uOpnd = static_cast<BaseNode *>(opnd);
  return un;
}

ExtractbitsNode *MIRBuilder::CreateExprExtractbits(Opcode o, const MIRType *type, uint32 bitsOffset, uint32 bitsSize,
                                                   BaseNode *opnd) {
  ExtractbitsNode *ext = GetCurrentFuncCodeMp()->New<ExtractbitsNode>(o);
  ext->primType = type->GetPrimType();
  ext->bitsOffset = bitsOffset;
  ext->bitsSize = bitsSize;
  ext->uOpnd = static_cast<BaseNode *>(opnd);
  return ext;
}

RetypeNode *MIRBuilder::CreateExprRetype(const MIRType *type, const MIRType *fromType, BaseNode *opnd) {
  RetypeNode *un = GetCurrentFuncCodeMp()->New<RetypeNode>();
  un->primType = type->GetPrimType();
  un->fromPrimType = fromType->GetPrimType();
  un->tyIdx = type->tyIdx;
  un->uOpnd = static_cast<BaseNode *>(opnd);
  return un;
}

BinaryNode *MIRBuilder::CreateExprBinary(Opcode opcode, const MIRType *type, BaseNode *opnd0, BaseNode *opnd1) {
  BinaryNode *bn = GetCurrentFuncCodeMp()->New<BinaryNode>(opcode);
  bn->primType = type->GetPrimType();
  bn->bOpnd[0] = opnd0;
  bn->bOpnd[1] = opnd1;
  return bn;
}

TernaryNode *MIRBuilder::CreateExprTernary(Opcode opcode, const MIRType *type, BaseNode *opnd0, BaseNode *opnd1,
                                           BaseNode *opnd2) {
  TernaryNode *tn = GetCurrentFuncCodeMp()->New<TernaryNode>(opcode);
  tn->primType = type->GetPrimType();
  tn->topnd[0] = opnd0;
  tn->topnd[1] = opnd1;
  tn->topnd[2] = opnd2;
  return tn;
}

CompareNode *MIRBuilder::CreateExprCompare(Opcode opcode, const MIRType *type, const MIRType *opndType,
                                           BaseNode *opnd0, BaseNode *opnd1) {
  CompareNode *bn = GetCurrentFuncCodeMp()->New<CompareNode>(opcode);
  bn->primType = type->GetPrimType();
  bn->opndType = opndType->GetPrimType();
  bn->bOpnd[0] = opnd0;
  bn->bOpnd[1] = opnd1;
  return bn;
}

ArrayNode *MIRBuilder::CreateExprArray(MIRType *arrayType, const MapleVector<BaseNode *> &opnds, bool bcheck) {
  ArrayNode *arraynode = GetCurrentFuncCodeMp()->New<ArrayNode>(GetCurrentFuncCodeMpAllocator(), bcheck);
  MIRType *addrType = GlobalTables::GetTypeTable().GetOrCreatePointerType(arrayType);
  arraynode->primType = addrType->primType;
  arraynode->tyIdx = addrType->tyIdx;
  arraynode->nOpnd = opnds;
  arraynode->numOpnds = opnds.size();
  return arraynode;
}

IntrinsicopNode *MIRBuilder::CreateExprIntrinsicop(MIRIntrinsicID idx, const MIRType *type,
                                                    const MapleVector<BaseNode *> &ops, bool withType) {
  Opcode op;
  if (withType) {
    op = OP_intrinsicopwithtype;
  } else {
    op = OP_intrinsicop;
  }
  IntrinsicopNode *expr = GetCurrentFuncCodeMp()->New<IntrinsicopNode>(GetCurrentFuncCodeMpAllocator(), op);
  expr->intrinsic = idx;
  uint32 numOpnds = ops.size();
  if (numOpnds > 0) {
    expr->nOpnd = ops;
    expr->numOpnds = numOpnds;
  }
  expr->primType = type->primType;
  if (withType) {
    expr->tyIdx = type->tyIdx;
  }
  return expr;
}

DassignNode *MIRBuilder::CreateStmtDassign(const MIRSymbol *symbol, FieldID fieldId, BaseNode *src) {
  DassignNode *stmt = GetCurrentFuncCodeMp()->New<DassignNode>();
  stmt->stIdx = symbol->GetStIdx();
  stmt->fieldID = fieldId;
  stmt->SetRhs(src);
  return stmt;
}

RegassignNode *MIRBuilder::CreateStmtRegassign(PrimType pty, PregIdx regIdx, BaseNode *src) {
  RegassignNode *stmt = GetCurrentFuncCodeMp()->New<RegassignNode>();
  stmt->primType = pty;
  stmt->regIdx = regIdx;
  stmt->uOpnd = src;
  return stmt;
}

DassignNode *MIRBuilder::CreateStmtDassign(StIdx sIdx, FieldID fieldId, BaseNode *src) {
  DassignNode *stmt = GetCurrentFuncCodeMp()->New<DassignNode>();
  stmt->stIdx = sIdx;
  stmt->fieldID = fieldId;
  stmt->SetRhs(src);
  return stmt;
}

IassignNode *MIRBuilder::CreateStmtIassign(const MIRType *type, FieldID fieldId, BaseNode *addr, BaseNode *src) {
  IassignNode *stmt = GetCurrentFuncCodeMp()->New<IassignNode>();
  stmt->tyIdx = type->tyIdx;
  stmt->fieldID = fieldId;
  stmt->addrExpr = addr;
  stmt->rhs = src;
  return stmt;
}

IassignoffNode *MIRBuilder::CreateStmtIassignoff(PrimType pty, int32 offset, BaseNode *addr, BaseNode *src) {
  IassignoffNode *stmt = GetCurrentFuncCodeMp()->New<IassignoffNode>();
  stmt->primType = pty;
  stmt->offset = offset;
  stmt->bOpnd[0] = addr;
  stmt->bOpnd[1] = src;
  return stmt;
}

IassignFPoffNode *MIRBuilder::CreateStmtIassignFPoff(PrimType pty, int32 offset, BaseNode *src) {
  IassignFPoffNode *stmt = GetCurrentFuncCodeMp()->New<IassignFPoffNode>();
  stmt->primType = pty;
  stmt->offset = offset;
  stmt->uOpnd = src;
  return stmt;
}

CallNode *MIRBuilder::CreateStmtCall(PUIdx puIdx, const MapleVector<BaseNode *> &args, Opcode opCode) {
  CallNode *stmt = GetCurrentFuncCodeMp()->New<CallNode>(GetCurrentFuncCodeMpAllocator(), OP_call);
  stmt->puIdx = puIdx;
  stmt->nOpnd = args;
  stmt->numOpnds = args.size();
  return stmt;
}

CallNode *MIRBuilder::CreateStmtCall(const string &callee, const MapleVector<BaseNode *> &args, Opcode opCode) {
  std::string string(callee);
  GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName(string);
  StIdx stIdx = GlobalTables::GetGsymTable().GetStIdxFromStrIdx(strIdx);
  MIRSymbol *st = GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx());
  MIRFunction *func = st->value.mirFunc;
  return CreateStmtCall(func->puIdx, args, opCode);
}

IcallNode *MIRBuilder::CreateStmtIcall(const MapleVector<BaseNode *> &args) {
  IcallNode *stmt = GetCurrentFuncCodeMp()->New<IcallNode>(GetCurrentFuncCodeMpAllocator(), OP_icall);
  stmt->nOpnd = args;
  stmt->numOpnds = args.size();
  return stmt;
}

IntrinsiccallNode *MIRBuilder::CreateStmtIntrinsicCall(MIRIntrinsicID idx,
                                                        const MapleVector<BaseNode *> &arguments) {
  IntrinsiccallNode *stmt =
    GetCurrentFuncCodeMp()->New<IntrinsiccallNode>(GetCurrentFuncCodeMpAllocator(), OP_intrinsiccall);
  stmt->intrinsic = idx;
  stmt->nOpnd = arguments;
  stmt->numOpnds = arguments.size();
  return stmt;
}

IntrinsiccallNode *MIRBuilder::CreateStmtXintrinsicCall(MIRIntrinsicID idx,
                                                         const MapleVector<BaseNode *> &arguments) {
  IntrinsiccallNode *stmt =
    GetCurrentFuncCodeMp()->New<IntrinsiccallNode>(GetCurrentFuncCodeMpAllocator(), OP_xintrinsiccall);
  stmt->intrinsic = idx;
  stmt->nOpnd = arguments;
  stmt->numOpnds = arguments.size();
  return stmt;
}

IntrinsiccallNode *MIRBuilder::CreateStmtIntrinsicCallwithtype(MIRIntrinsicID idx,
                                                                        const MIRType *type,
                                                                        const MapleVector<BaseNode *> &arguments) {
  return CreateStmtIntrinsicCallwithtype(idx, type->tyIdx, arguments);
}

IntrinsiccallNode *MIRBuilder::CreateStmtIntrinsicCallwithtype(MIRIntrinsicID idx, TyIdx tyIdx,
                                                                        const MapleVector<BaseNode *> &arguments) {
  IntrinsiccallNode *stmt =
    GetCurrentFuncCodeMp()->New<IntrinsiccallNode>(GetCurrentFuncCodeMpAllocator(), OP_intrinsiccallwithtype);
  stmt->intrinsic = idx;
  stmt->tyIdx = tyIdx;
  stmt->nOpnd = arguments;
  stmt->numOpnds = arguments.size();
  return stmt;
}

CallNode *MIRBuilder::CreateStmtCallAssigned(PUIdx puIdx,
                                                     const MapleVector<BaseNode *> &args,
                                                     const MIRSymbol *ret,
                                                     Opcode opcode) {
  CallNode *stmt = GetCurrentFuncCodeMp()->New<CallNode>(GetCurrentFuncCodeMpAllocator(), opcode);
  stmt->puIdx = puIdx;
  stmt->nOpnd = args;
  stmt->numOpnds = args.size();
  if (ret) {
    CHECK_FATAL(ret->storageClass == kScAuto || ret->storageClass == kScFormal || ret->storageClass == kScExtern || ret->storageClass == kScGlobal,
           "");
    stmt->returnValues.push_back(CallReturnPair(ret->GetStIdx(), RegFieldPair(0, 0)));
  }
  return stmt;
}

CallNode *MIRBuilder::CreateStmtCallRegassigned(PUIdx puIdx, const MapleVector<BaseNode *> &args,
                                                        PregIdx pregidxt, Opcode opcode) {
  CallNode *stmt = GetCurrentFuncCodeMp()->New<CallNode>(GetCurrentFuncCodeMpAllocator(), opcode);
  stmt->puIdx = puIdx;
  stmt->nOpnd = args;
  stmt->numOpnds = args.size();
  if (pregidxt > 0) {
    stmt->returnValues.push_back(CallReturnPair(StIdx(), RegFieldPair(0, pregidxt)));
  }
  return stmt;
}

CallNode *MIRBuilder::CreateStmtCallwithtypeAssigned(PUIdx puIdx,
    const MapleVector<BaseNode *> &args, const MIRSymbol *ret, uint32 prototyidx, Opcode opcode) {
  CallNode *stmt =
    GetCurrentFuncCodeMp()->New<CallNode>(GetCurrentFuncCodeMpAllocator(), opcode);
  stmt->puIdx = puIdx;
  stmt->tyIdx = TyIdx(prototyidx);
  stmt->nOpnd = args;
  stmt->numOpnds = args.size();
  if (ret) {
    CHECK_FATAL(ret->storageClass == kScAuto || ret->storageClass == kScFormal || ret->storageClass == kScExtern || ret->storageClass == kScGlobal,
           "");
    stmt->returnValues.push_back(CallReturnPair(ret->GetStIdx(), RegFieldPair(0, 0)));
  }
  return stmt;
}

IntrinsiccallNode *MIRBuilder::CreateStmtIntrinsicCallAssigned(MIRIntrinsicID idx,
                                                                        const MapleVector<BaseNode *> &args,
                                                                        PregIdx retpregidx) {
  IntrinsiccallNode *stmt =
      GetCurrentFuncCodeMp()->New<IntrinsiccallNode>(GetCurrentFuncCodeMpAllocator(), OP_intrinsiccallassigned);
  stmt->intrinsic = idx;
  stmt->nOpnd = args;
  stmt->numOpnds = args.size();
  if (retpregidx > 0) {
    stmt->returnValues.push_back(CallReturnPair(StIdx(), RegFieldPair(0, retpregidx)));
  }
  return stmt;
}

IntrinsiccallNode *MIRBuilder::CreateStmtIntrinsicCallAssigned(MIRIntrinsicID idx,
                                                                        const MapleVector<BaseNode *> &args,
                                                                        const MIRSymbol *ret) {
  IntrinsiccallNode *stmt =
      GetCurrentFuncCodeMp()->New<IntrinsiccallNode>(GetCurrentFuncCodeMpAllocator(), OP_intrinsiccallassigned);
  stmt->intrinsic = idx;
  if (ret) {
    CHECK_FATAL(ret->storageClass == kScAuto, "");
    stmt->returnValues.push_back(CallReturnPair(ret->GetStIdx(), RegFieldPair(0, 0)));
  }
  stmt->nOpnd = args;
  stmt->numOpnds = args.size();
  return stmt;
}

IntrinsiccallNode *MIRBuilder::CreateStmtXintrinsicCallAssigned(MIRIntrinsicID idx,
                                                                         const MapleVector<BaseNode *> &args,
                                                                         const MIRSymbol *ret) {
  IntrinsiccallNode *stmt = GetCurrentFuncCodeMp()->New<IntrinsiccallNode>(
      GetCurrentFuncCodeMpAllocator(), OP_xintrinsiccallassigned);
  stmt->intrinsic = idx;
  stmt->nOpnd = args;
  stmt->numOpnds = args.size();
  CallReturnVector returnValues(GetCurrentFuncCodeMpAllocator()->Adapter());
  if (ret) {
    CHECK_FATAL(ret->storageClass == kScAuto || ret->storageClass == kScFormal || ret->storageClass == kScExtern || ret->storageClass == kScGlobal,
           "");
    returnValues.push_back(CallReturnPair(ret->GetStIdx(), RegFieldPair(0, 0)));
  }
  stmt->returnValues = returnValues;
  return stmt;
}

IntrinsiccallNode *MIRBuilder::CreateStmtIntrinsicCallwithtypeAssigned(
    MIRIntrinsicID idx, const MIRType *type, const MapleVector<BaseNode *> &args, const MIRSymbol *ret) {
  IntrinsiccallNode *stmt = GetCurrentFuncCodeMp()->New<IntrinsiccallNode>(
      GetCurrentFuncCodeMpAllocator(), OP_intrinsiccallwithtypeassigned);
  stmt->intrinsic = idx;
  stmt->tyIdx = type->tyIdx;
  CallReturnVector returnValues(GetCurrentFuncCodeMpAllocator()->Adapter());
  if (ret) {
    CHECK_FATAL(ret->storageClass == kScAuto || ret->storageClass == kScFormal || ret->storageClass == kScExtern || ret->storageClass == kScGlobal,
           "");
    returnValues.push_back(CallReturnPair(ret->GetStIdx(), RegFieldPair(0, 0)));
  }
  stmt->returnValues = returnValues;
  stmt->nOpnd = args;
  stmt->numOpnds = args.size();
  return stmt;
}

CallinstantNode *MIRBuilder::CreateStmtCallinstant(PUIdx puIdx, TyIdx tyIdx, const MapleVector<BaseNode *> &args,
                                                   Opcode op) {
  CallinstantNode *stmt = GetCurrentFuncCodeMp()->New<CallinstantNode>(GetCurrentFuncCodeMpAllocator(), op, tyIdx);
  stmt->puIdx = puIdx;
  stmt->nOpnd = args;
  stmt->numOpnds = args.size();
  return stmt;
}

CallinstantNode *MIRBuilder::CreateStmtCallinstantAssigned(PUIdx puIdx, TyIdx tyIdx,
                                                                   const MapleVector<BaseNode *> &args,
                                                                   const MIRSymbol *ret, Opcode op) {
  CallinstantNode *stmt =
    GetCurrentFuncCodeMp()->New<CallinstantNode>(GetCurrentFuncCodeMpAllocator(), op, tyIdx);
  stmt->puIdx = puIdx;
  stmt->nOpnd = args;
  stmt->numOpnds = args.size();
  CallReturnVector returnValues(GetCurrentFuncCodeMpAllocator()->Adapter());
  if (ret) {
    CHECK_FATAL(ret->storageClass == kScAuto || ret->storageClass == kScFormal || ret->storageClass == kScExtern || ret->storageClass == kScGlobal,
           "");
    returnValues.push_back(CallReturnPair(ret->GetStIdx(), RegFieldPair(0, 0)));
  }
  stmt->returnValues = returnValues;
  return stmt;
}

NaryStmtNode *MIRBuilder::CreateStmtReturn(BaseNode *rVal) {
  NaryStmtNode *stmt = GetCurrentFuncCodeMp()->New<NaryStmtNode>(GetCurrentFuncCodeMpAllocator(), OP_return);
  if (rVal) {
    stmt->nOpnd.push_back(rVal);
  }
  stmt->numOpnds = stmt->nOpnd.size();
  return stmt;
}

NaryStmtNode *MIRBuilder::CreateStmtNary(Opcode op, const MapleVector<BaseNode *> &rVals) {
  NaryStmtNode *stmt = GetCurrentFuncCodeMp()->New<NaryStmtNode>(GetCurrentFuncCodeMpAllocator(), op);
  stmt->nOpnd = rVals;
  stmt->numOpnds = stmt->nOpnd.size();
  return stmt;
}

NaryStmtNode *MIRBuilder::CreateStmtNary(Opcode op, BaseNode *rVal) {
  NaryStmtNode *stmt = GetCurrentFuncCodeMp()->New<NaryStmtNode>(GetCurrentFuncCodeMpAllocator(), op);
  if (rVal) {
    stmt->nOpnd.push_back(rVal);
  }
  stmt->numOpnds = stmt->nOpnd.size();
  return stmt;
}

UnaryStmtNode *MIRBuilder::CreateStmtUnary(Opcode op, BaseNode *rVal) {
  UnaryStmtNode *stmt = GetCurrentFuncCodeMp()->New<UnaryStmtNode>(op);
  stmt->uOpnd = rVal;
  return stmt;
}

UnaryStmtNode *MIRBuilder::CreateStmtThrow(BaseNode *rVal) {
  return CreateStmtUnary(OP_throw, rVal);
}

IfStmtNode *MIRBuilder::CreateStmtIf(BaseNode *cond) {
  IfStmtNode *ifstmt = GetCurrentFuncCodeMp()->New<IfStmtNode>();
  ifstmt->uOpnd = cond;
  BlockNode *thenblock = GetCurrentFuncCodeMp()->New<BlockNode>();
  ifstmt->thenPart = thenblock;
  return ifstmt;
}

IfStmtNode *MIRBuilder::CreateStmtIfThenElse(BaseNode *cond) {
  IfStmtNode *ifstmt = GetCurrentFuncCodeMp()->New<IfStmtNode>();
  ifstmt->uOpnd = cond;
  BlockNode *thenblock = GetCurrentFuncCodeMp()->New<BlockNode>();
  ifstmt->thenPart = thenblock;
  BlockNode *elseblock = GetCurrentFuncCodeMp()->New<BlockNode>();
  ifstmt->elsePart = elseblock;
  ifstmt->numOpnds = 3;
  return ifstmt;
}

DoloopNode *MIRBuilder::CreateStmtDoloop(StIdx dovarStidx, bool isPreg, BaseNode *startExpr, BaseNode *condExpr,
                                         BaseNode *incrExpr) {
  DoloopNode *doloopnode = GetCurrentFuncCodeMp()->New<DoloopNode>();
  doloopnode->doVarStIdx = dovarStidx;
  doloopnode->isPreg = isPreg;
  doloopnode->startExpr = startExpr;
  doloopnode->condExpr = condExpr;
  doloopnode->incrExpr = incrExpr;
  doloopnode->doBody = GetCurrentFuncCodeMp()->New<BlockNode>();
  doloopnode->numOpnds = 4;
  return doloopnode;
}

SwitchNode *MIRBuilder::CreateStmtSwitch(BaseNode *opnd, LabelIdx defaultLabel, const CaseVector &switchTable) {
  SwitchNode *switchnode = GetCurrentFuncCodeMp()->New<SwitchNode>(GetCurrentFuncCodeMpAllocator());
  switchnode->switchOpnd = opnd;
  switchnode->defaultLabel = defaultLabel;
  switchnode->switchTable = switchTable;
  return switchnode;
}

GotoNode *MIRBuilder::CreateStmtGoto(Opcode o, LabelIdx labidx) {
  GotoNode *gotonode = GetCurrentFuncCodeMp()->New<GotoNode>(o);
  gotonode->offset = labidx;
  return gotonode;
}

TryNode *MIRBuilder::CreateStmtTry(Opcode o, MapleVector<LabelIdx> &cLabIdxs) {
  TryNode *trynode = GetCurrentFuncCodeMp()->New<TryNode>(GetCurrentFuncCodeMpAllocator(), o);
  trynode->offsets = cLabIdxs;
  return trynode;
}

JsTryNode *MIRBuilder::CreateStmtJsTry(Opcode o, LabelIdx cLabIdx, LabelIdx fLabidx) {
  JsTryNode *trynode = GetCurrentFuncCodeMp()->New<JsTryNode>();
  trynode->catchOffset = static_cast<uint16>(cLabIdx);
  trynode->finallyOffset = static_cast<uint16>(fLabidx);
  return trynode;
}

TryNode *MIRBuilder::CreateStmtJavaTry(MapleVector<LabelIdx> &cLabIdxs) {
  TryNode *trynode = GetCurrentFuncCodeMp()->New<TryNode>(GetCurrentFuncCodeMpAllocator(), OP_try);
  trynode->offsets = cLabIdxs;
  return trynode;
}

CppCatchNode *MIRBuilder::CreateStmtCppCatch(const TyIdx idx) {
  CppCatchNode *catchnode = GetCurrentFuncCodeMp()->New<CppCatchNode>();
  catchnode->exceptionTyIdx = idx;
  return catchnode;
}

CatchNode *MIRBuilder::CreateStmtJavaCatch(const MapleVector<TyIdx> &tyidxvec) {
  CatchNode *catchnode = GetCurrentFuncCodeMp()->New<CatchNode>(GetCurrentFuncCodeMpAllocator());
  catchnode->exceptionTyIdxVec = tyidxvec;
  return catchnode;
}

AssertStmtNode *MIRBuilder::CreateStmtAssert(bool isLt, BaseNode *opnd1, BaseNode *opnd2) {
  AssertStmtNode *stmt = GetCurrentFuncCodeMp()->New<AssertStmtNode>(isLt ? OP_assertlt : OP_assertge);
  stmt->bOpnd[0] = opnd1;
  stmt->bOpnd[1] = opnd2;
  return stmt;
}

LabelIdx MIRBuilder::GetorCreateMIRLabel(const string &name) {
  std::string lblstr(name);
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lblstr);
  LabelIdx labidx = GetCurrentFunction()->labelTab->strIdxToLabIdxMap[strIdx];
  if (labidx == 0) {
    labidx = GetCurrentFunction()->labelTab->CreateLabel();
    GetCurrentFunction()->labelTab->labelTable.at(labidx) = strIdx;
    GetCurrentFunction()->labelTab->AddToStringLabelMap(labidx);
  }
  return labidx;
}

LabelIdx MIRBuilder::CreateLabidx(MIRFunction *mirFunc) {
  LabelIdx lidx = mirFunc->labelTab->CreateLabel();
  mirFunc->labelTab->AddToStringLabelMap(lidx);
  return lidx;
}

LabelNode *MIRBuilder::CreateStmtLabel(LabelIdx labidx) {
  LabelNode *label = GetCurrentFuncCodeMp()->New<LabelNode>();
  label->labelIdx = labidx;
  return label;
}

StmtNode *MIRBuilder::CreateStmtComment(const string &cmnt) {
  CommentNode *cmntnode = GetCurrentFuncCodeMp()->New<CommentNode>(GetCurrentFuncCodeMpAllocator(), cmnt.c_str());
  return cmntnode;
}

void MIRBuilder::AddStmtInCurrentFunctionBody(StmtNode *stmt) {
  MIRFunction *fun = GetCurrentFunction();
  stmt->srcPosition.CondSetLinenum(lineNum);
  fun->body->AddStatement(stmt);
}

AddrofNode *MIRBuilder::CreateAddrof(const MIRSymbol *st, PrimType pty) {
  AddrofNode *addrofnode = GetCurrentFuncCodeMp()->New<AddrofNode>(OP_addrof);
  addrofnode->primType = pty;
  addrofnode->stIdx = st->GetStIdx();
  return addrofnode;
}

AddrofNode *MIRBuilder::CreateDread(const MIRSymbol *st, PrimType pty) {
  AddrofNode *dread = GetCurrentFuncCodeMp()->New<AddrofNode>(OP_dread);
  dread->primType = UpCvt32(pty);
  dread->stIdx = st->GetStIdx();
  dread->fieldID = 0;
  return dread;
}

CondGotoNode *MIRBuilder::CreateStmtCondGoto(BaseNode *cond, Opcode op, LabelIdx labidx) {
  CondGotoNode *condgoto = GetCurrentFuncCodeMp()->New<CondGotoNode>(op == OP_brtrue ? OP_brtrue : OP_brfalse);
  condgoto->offset = labidx;
  condgoto->uOpnd = cond;
  return condgoto;
}

void MIRBuilder::DeepCopyPointerField(MIRType *fieldtype, MIRSymbol *destst, MIRSymbol *srcst, BlockNode *blocknode,
                                      FieldID &fieldID) {
  MIRType *srctype = srcst->GetType();
  MIRType *desttype = destst->GetType();
  CHECK_FATAL(srctype == desttype, "different type, can't copy");
  CHECK_FATAL(srctype->typeKind == kTypePointer, "");
  switch (fieldtype->typeKind) {
    case kTypeScalar:
    case kTypePointer: {
      IreadNode *expr = CreateExprIread(fieldtype, srctype, fieldID, CreateExprDread(srctype, srcst));
      StmtNode *stmt = CreateStmtIassign(srctype, fieldID, CreateExprDread(srctype, destst), expr);
      stmt->srcPosition.CondSetLinenum(lineNum);
      blocknode->AddStatement(stmt);
      break;
    }
    case kTypeClass:
    case kTypeClassIncomplete: {
      MIRClassType *classtype = static_cast<MIRClassType *>(fieldtype);
      TyIdx parentTyidx = classtype->parentTyIdx;
      if (parentTyidx != TyIdx(0)) {
        MIRClassType *parentclassty = MIR_DYN_CAST(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyidx), MIRClassType *);
        if (parentclassty) {
          fieldID++;
          DeepCopyPointerField(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyidx), destst, srcst, blocknode, fieldID);
        }
      }
      for (uint32 i = 0; i < classtype->fields.size(); i++) {
        DeepCopyPointerField(GlobalTables::GetTypeTable().GetTypeFromTyIdx(classtype->fields[i].second.first), destst, srcst, blocknode,
                             fieldID);
        fieldID++;
      }
      break;
    }
    default:
      CHECK_FATAL(false, "");
  }
}

// deep copy pointer of srcst to destst, ptype is the type, result will put to blocknode
void MIRBuilder::DeepCopyPointer(MIRSymbol *destst, MIRSymbol *srcst, BlockNode *blocknode) {
  MIRType *srctype = srcst->GetType();
  MIRType *desttype = destst->GetType();
  CHECK_FATAL(srctype == desttype, "different type, can't copy");
  CHECK_FATAL(srctype->typeKind == kTypePointer, "");
  MIRType *pointedtype = static_cast<MIRPtrType *>(srctype)->GetPointedType();
  if (pointedtype->typeKind == kTypeClass || pointedtype->typeKind == kTypeClassIncomplete) {
    MIRClassType *classtype = static_cast<MIRClassType *>(pointedtype);
    FieldID fieldID = 1;
    TyIdx parentTyidx = classtype->parentTyIdx;
    if (parentTyidx != TyIdx(0)) {
      MIRClassType *parentclassty = MIR_DYN_CAST(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyidx), MIRClassType *);
      if (parentclassty) {
        fieldID++;
        DeepCopyPointerField(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyidx), destst, srcst, blocknode, fieldID);
      }
    }
    for (uint32 i = 0; i < classtype->fields.size(); i++) {
      TyIdx tyIdx = classtype->fields[i].second.first;
      MIRType *fieldtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
      DeepCopyPointerField(fieldtype, destst, srcst, blocknode, fieldID);
    }
  } else {
    CHECK_FATAL(false, "deep copy NYI");
  }
}

// this one create _ZTI string for type info
std::string MIRBuilder::GetZTIString(const MIRType *ty) {
  const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(ty->nameStrIdx);
  return std::string("_ZTI").append(std::to_string(name.size())).append(name);
  // return zti_str;
}

// create __ZTS string for type info
std::string MIRBuilder::GetZTSString(const MIRType *ty) {
  const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(ty->nameStrIdx);
  return std::string("_ZTS").append(std::to_string(name.size())).append(name);
  // return zts_str;
}

// create symbol off type. currently used for exception handling
MIRSymbol *MIRBuilder::GetOrCreateTypeSymbol(MIRType *ty, MIRClassType *rootty) {
  // Use a different name _ZTI leng
  // const std::string& name = globaltable.GetStringFromGstridx( ty->nameStrIdx );
  // std::string zti_str = std::string("_ZTI").append(std::to_string(name.size())).append(name);
  const std::string &ztiStr = GetZTIString(ty);
  GStrIdx ztiSt = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(ztiStr);

  MIRSymbol *typeinfost = GetSymbol(ty->tyIdx, ztiSt, kStConst, kScTypeInfo, kScopeGlobal, nullptr, true);

  if (!typeinfost) {
    typeinfost = CreateSymbol(ty->tyIdx, ztiSt, kStConst, kScTypeInfo, kScopeGlobal, nullptr);

    // std::string zts_str = std::string("_ZTS").append(std::to_string(name.size())).append(name);
    const std::string &ztsStr = GetZTSString(ty);
    GStrIdx ztsSt = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(ztsStr);

    MIRSymbol *
#if DEBUG
      typeinfonamest = GetSymbol(ty->tyIdx, ztsSt, kStConst, kScTypeInfoName, kScopeGlobal, nullptr);
    CHECK_FATAL(!typeinfonamest, "type was not created before, how type name created before?!");
#endif
    typeinfonamest = CreateSymbol(ty->tyIdx, ztsSt, kStConst, kScTypeInfoName, kScopeGlobal, nullptr);

    MIRStructType *tyinfostructtype = new MIRStructType(kTypeStruct);
    FieldPair fieldpair1(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("a64"),
                         TyidxFieldAttrPair(TyIdx(PTY_a64), FieldAttrs()));
    FieldPair fieldpair2(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("a64"),
                         TyidxFieldAttrPair(TyIdx(PTY_a64), FieldAttrs()));
    tyinfostructtype->fields.push_back(fieldpair1);
    tyinfostructtype->fields.push_back(fieldpair2);

    // _ZTI3xxx:
    // .xword  _ZTVN10__cxxabiv117__class_type_infoE+16
    // .xword  _ZTS3xxx  # name
    // .xword parent type symbol
    MIRStConst *tyinfostconst = mirModule->memPool->New<MIRStConst>(mirModule, tyinfostructtype);

    bool istyperoot = ty == rootty;
    string strTypeinfo =
      (istyperoot ? "_ZTVN10__cxxabiv117__class_type_infoE" : "_ZTVN10__cxxabiv120__si_class_type_infoE");
    GStrIdx classtypegstri = GetOrCreateStringIndex(strTypeinfo);
    MIRSymbol *cxxtypinfost =
      GetSymbol((TyIdx)PTY_a64, classtypegstri, kStConst, kScTypeCxxAbi, kScopeGlobal, nullptr);

    if (!cxxtypinfost) {
      cxxtypinfost = CreateSymbol((TyIdx)PTY_a64, classtypegstri, kStConst, kScTypeCxxAbi, kScopeGlobal, nullptr);
    }
    tyinfostconst->stVec.push_back(cxxtypinfost);
    tyinfostconst->stOffsetVec.push_back(16);

    tyinfostconst->stVec.push_back(typeinfonamest);
    tyinfostconst->stOffsetVec.push_back(0);

    if (!istyperoot) {
      CHECK_FATAL(ty->typeKind == kTypeClass || ty->typeKind == kTypeClassIncomplete, "");
      MIRClassType *classty = static_cast<MIRClassType *>(ty);
      MIRType *parenttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(classty->parentTyIdx);
      MIRSymbol *parenttyst = GetOrCreateTypeSymbol(parenttype, rootty);
      tyinfostconst->stVec.push_back(parenttyst);
      tyinfostconst->stOffsetVec.push_back(0);
    }
    typeinfost->value.konst = tyinfostconst;

    // MIRPtrType pointtype((TyIdx)PTY_void);
    MIRType *voidptype = GlobalTables::GetTypeTable().GetVoidPtr();
    CHECK_FATAL(voidptype, "");
    TyIdx ptyidx = voidptype->tyIdx;
    MIRStrConst *strconst =
      GetCurrentFuncCodeMp()->New<MIRStrConst>(UStrIdx(ty->nameStrIdx.GetIdx()), GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptyidx));
    typeinfonamest->value.konst = strconst;
  }
  return typeinfost;
}

MemPool *MIRBuilder::GetCurrentFuncCodeMp() {
  return mirModule->CurFuncCodeMemPool();
}

MapleAllocator *MIRBuilder::GetCurrentFuncCodeMpAllocator() {
  return mirModule->CurFuncCodeMemPoolAllocator();
}

FieldID MIRBuilder::GetStructFieldIdFromNameAndType(MIRType *type, const char *name, TyIdx tid, unsigned matchStyle) {
  MIRStructType *structType = static_cast<MIRStructType *>(type);
  uint32 fieldID = 0;
  GStrIdx strIdx = GetStringIndex(name);
  if (TraverseToNamedFieldWithTypeAndMatchStyle(structType, strIdx, tid, fieldID, matchStyle)) {
    return fieldID;
  }
  return 0;
}

//void MIRBuilder::SetStructFieldIdFromFieldName(MIRType *structtype, const char *name, GStrIdx newStridx,
//                                               const MIRType *newFieldtype) {
//  MIRStructType *structType = static_cast<MIRStructType *>(structtype);
//  uint32 fieldId = 0;
//  GStrIdx strIdx = GetStringIndex(name);
//
//  while (true) {
//    if (structType->GetElemStridx(fieldId) == strIdx) {
//      if (newStridx != 0) {
//        structType->SetElemStridx(fieldId, newStridx);
//      }
//      if (newFieldtype) {
//        structType->SetElemTyidx(fieldId, newFieldtype->tyIdx);
//      }
//      return;
//    }
//    fieldId++;
//  }
//}

// create a function named str
MIRFunction *MIRBuilder::GetOrCreateFunction(const char *str, TyIdx rettyidx) {
  GStrIdx strIdx = GetStringIndex(str);
  MIRSymbol *funcst = nullptr;
  if (strIdx != 0) {
    funcst = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
    if (funcst == nullptr) {
      funcst = CreateSymbol(TyIdx(0), strIdx, kStFunc, kScText, kScopeGlobal, nullptr);
    } else {
      CHECK_FATAL(funcst->sKind == kStFunc, "runtime check error");
      return funcst->value.mirFunc;
    }
  } else {
    strIdx = GetOrCreateStringIndex(str);
    funcst = CreateSymbol(TyIdx(0), strIdx, kStFunc, kScText, kScopeGlobal, nullptr);
  }
  MIRFunction *fn = mirModule->memPool->New<MIRFunction>(mirModule, funcst->GetStIdx());
  fn->puIdx = GlobalTables::GetFunctionTable().funcTable.size();
  MIRFuncType *funcType = mirModule->memPool->New<MIRFuncType>();
  fn->funcType = funcType;
  fn->SetReturnTyIdx(rettyidx);
  GlobalTables::GetFunctionTable().funcTable.push_back(fn);
  funcst->SetFunction(fn);
  return fn;
}

MIRFunction *MIRBuilder::GetFunctionFromName(const char *str) {
  return GetFunctionFromSymbol(GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(str)));
}

LabelIdx MIRBuilder::GetorCreateMIRLabel(const char *name) {
  std::string lblstr(name);
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lblstr);
  LabelIdx labidx = GetCurrentFunction()->labelTab->strIdxToLabIdxMap[strIdx];
  if (labidx == 0) {
    labidx = GetCurrentFunction()->labelTab->CreateLabel();
    GetCurrentFunction()->labelTab->labelTable.at(labidx) = strIdx;
    GetCurrentFunction()->labelTab->AddToStringLabelMap(labidx);
  }
  return labidx;
}

StmtNode *MIRBuilder::CreateStmtComment(const char *cmnt) {
  CommentNode *cmntnode = GetCurrentFuncCodeMp()->New<CommentNode>(GetCurrentFuncCodeMpAllocator(), cmnt);
  return cmntnode;
}

MIRBuilderExt::MIRBuilderExt(MIRModule *module, pthread_mutex_t *mutex) : MIRBuilder(module), mMutex(mutex) {}

MemPool *MIRBuilderExt::GetCurrentFuncCodeMp() {
  CHECK_FATAL(curFunction, "curFunction is null");
  return curFunction->GetCodeMp();
}

MapleAllocator *MIRBuilderExt::GetCurrentFuncCodeMpAllocator() {
  CHECK_FATAL(curFunction, "curFunction is null");
  return curFunction->GetCodeMpAllocator();
}

void MIRBuilderExt::GlobalLock() {
  if (mMutex) {
    pthread_mutex_lock(mMutex);
  }
}

void MIRBuilderExt::GlobalUnlock() {
  if (mMutex) {
    pthread_mutex_unlock(mMutex);
  }
}

}  // namespace maple
