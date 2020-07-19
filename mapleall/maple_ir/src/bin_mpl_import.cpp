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

#include "bin_mpl_import.h"
#include <sstream>
#include <vector>
#include <unordered_set>
#include <limits>
#include "bin_mplt.h"
#include "mir_function.h"
#include "name_mangler.h"
#include "opcode_info.h"
#include "mir_pragma.h"
#include "mir_builder.h"

using namespace std;

namespace maple {

uint8 BinaryMplImport::Read() {
  CHECK(bufI < buf.size(), "Index out of bound in BinaryMplImport::Read()");
  return buf[bufI++];
}

/* Little endian */
int32 BinaryMplImport::ReadInt() {
  uint32 x0 = static_cast<uint32>(Read());
  uint32 x1 = static_cast<uint32>(Read());
  uint32 x2 = static_cast<uint32>(Read());
  uint32 x3 = static_cast<uint32>(Read());
  return (((((x3 << 8u) + x2) << 8u) + x1) << 8u) + x0;
}

int64 BinaryMplImport::ReadInt64() {
  // casts to avoid sign extension
  uint32 x0 = static_cast<uint32>(ReadInt());
  uint64 x1 = static_cast<uint32>(ReadInt());
  return static_cast<int64>((x1 << 32) + x0);
}

/* LEB128 */
int64 BinaryMplImport::ReadNum() {
  uint64 n = 0;
  int64 y = 0;
  uint64 b = static_cast<uint64>(Read());
  while (b >= 0x80) {
    y += ((b - 0x80) << n);
    n += 7;
    b = static_cast<uint64>(Read());
  }
  b = (b & 0x3F) - (b & 0x40);
  return y + (b << n);
}

void BinaryMplImport::ReadAsciiStr(std::string &str) {
  int64 n = ReadNum();
  for (int64 i = 0; i < n; i++) {
    uint8 ch = Read();
    str.push_back(static_cast<char>(ch));
  }
}

void BinaryMplImport::ReadFileAt(const std::string &name, int32 offset) {
  FILE *f = fopen(name.c_str(), "rb");
  CHECK(f != nullptr, "Error while reading the binary file: %s", name.c_str());

  int seekRet = fseek(f, 0, SEEK_END);
  CHECK(seekRet == 0, "call fseek failed");

  long size = ftell(f);
  size -= offset;

  CHECK(size >= 0, "should not be negative");

  seekRet = fseek(f, offset, SEEK_SET);
  CHECK(seekRet == 0, "call fseek failed");
  buf.resize(size);

  long result = fread(&buf[0], sizeof(uint8), size, f);
  fclose(f);
  CHECK(result == size, "Error while reading the binary file: %s", name.c_str());
}

void BinaryMplImport::ImportConstBase(MIRConstKind &kind, MIRType* &type, uint32 &fieldID) {
  kind = (MIRConstKind)ReadNum();  // todo is kind ever used?
  TyIdx tyIdx = ImportType();
  type = GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()];
  fieldID = ReadNum();
}


MIRConst *BinaryMplImport::ImportConst(MIRFunction *func) {
  int64 tag = ReadNum();
  if (tag == 0) {
    return nullptr;
  }

  MIRConstKind kind;
  MIRType *type = nullptr;
  uint32 fieldID;
  MemPool *memPool = (func == nullptr) ? mod.memPool : func->dataMemPool;

  ImportConstBase(kind, type, fieldID);
  if (tag == kBinKindConstInt) {
    MIRIntConst *intconst = mod.memPool->New<MIRIntConst>(ReadNum(), type, fieldID);
    return intconst;
  } else if (tag == kBinKindConstAddrof) {
    MIRSymbol *sym = InSymbol(func);
    CHECK_FATAL(sym, "null ptr check");
    FieldID fi = ReadNum();
    int32 ofst = ReadNum();
    return memPool->New<MIRAddrofConst>(sym->stIdx, fi, type, ofst, fieldID);
  } else if (tag == kBinKindConstAddrofFunc) {
    PUIdx puIdx = ImportFunction();
    MIRAddroffuncConst *addrfunc;
    addrfunc = memPool->New<MIRAddroffuncConst>(puIdx, type, fieldID);
    return addrfunc;
  } else if (tag == kBinKindConstAddrofLabel) {
    LabelIdx lidx = ReadNum();
    PUIdx puIdx = func->puIdx;
    MIRLblConst *lblConst = memPool->New<MIRLblConst>(lidx, puIdx, type, fieldID);
    func->labelTab->addrTakenLabels.insert(lidx);
    return lblConst;
  } else if (tag == kBinKindConstStr) {
    UStrIdx ustr = ImportUsrStr();
    MIRStrConst *strc;
    strc = mod.memPool->New<MIRStrConst>(ustr, type, fieldID);
    return strc;
  } else if (tag == kBinKindConstStr16) {
    Conststr16Node *cs;
    cs = mod.memPool->New<Conststr16Node>();
    cs->primType = type->primType;
    int64 len = ReadNum();
    string str;
    for (int64 i = 0; i < len; i++) {
      str.push_back(Read());
    }
    std::u16string str16;
    NameMangler::UTF8ToUTF16(str16, str);
    cs->strIdx = GlobalTables::GetU16StrTable().GetOrCreateStrIdxFromName(str16);
    return mod.memPool->New<MIRStr16Const>(cs->strIdx, type, fieldID);
  } else if (tag == kBinKindConstFloat) {
    union {
      float fvalue;
      int32 ivalue;
    } value;

    value.ivalue = ReadNum();
    return GlobalTables::GetFpConstTable().GetOrCreateFloatConst(value.fvalue, fieldID);
  } else if (tag == kBinKindConstDouble) {
    union {
      double dvalue;
      int64 ivalue;
    } value;

    value.ivalue = ReadNum();
    return GlobalTables::GetFpConstTable().GetOrCreateDoubleConst(value.dvalue, fieldID);
  } else if (tag == kBinKindConstAgg) {
    MIRAggConst *aggconst = mod.memPool->New<MIRAggConst>(&mod, type, fieldID);
    int64 size = ReadNum();
    for (int64 i = 0; i < size; i++) {
      aggconst->constVec.push_back(ImportConst(func));
    }
    return aggconst;
  } else if (tag == kBinKindConstSt) {
    MIRStConst *stConst = mod.memPool->New<MIRStConst>(&mod, type, fieldID);
    int64 size = ReadNum();
    for (int64 i = 0; i < size; i++) {
      stConst->stVec.push_back(InSymbol(func));
    }
    size = ReadNum();
    for (int64 i = 0; i < size; i++) {
      stConst->stOffsetVec.push_back(ReadNum());
    }
    return stConst;
  } else {
    CHECK_FATAL(false, "Unhandled const type");
  }
  return nullptr;
}

GStrIdx BinaryMplImport::ImportStr() {
  int64 tag = ReadNum();
  if (tag == 0) {
    return GStrIdx(0);
  } else if (tag < 0) {
    CHECK(-tag < gstr_tab.size(), "index out of range in BinaryMplt::ImportStr");
    return gstr_tab[-tag];
  } else {
    CHECK_FATAL(tag == kBinString, "expecting kBinString");
    std::string str;
    ReadAsciiStr(str);
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(str);
    gstr_tab.push_back(strIdx);
    return strIdx;
  }
}

UStrIdx BinaryMplImport::ImportUsrStr() {
  int64 tag = ReadNum();
  if (tag == 0) {
    return UStrIdx(0);
  } else if (tag < 0) {
    CHECK(-tag < static_cast<int64>(ustr_tab.size()), "index out of range in BinaryMplt::ImportUsrStr");
    return ustr_tab[-tag];
  } else {
    CHECK_FATAL(tag == kBinUsrString, "expecting kBinUsrString");
    std::string str;
    ReadAsciiStr(str);
    UStrIdx strIdx = GlobalTables::GetUStrTable().GetOrCreateStrIdxFromName(str);
    ustr_tab.push_back(strIdx);
    return strIdx;
  }
}

MIRPragmaElement *BinaryMplImport::ImportPragmaElement() {
  MIRPragmaElement *element = mod.memPool->New<MIRPragmaElement>(&mod);
  element->namestridx_ = ImportStr();
  element->typestridx_ = ImportStr();
  element->type_ = (PragmaValueType)ReadNum();
  if (element->type_ == kValueString || element->type_ == kValueType || element->type_ == kValueField || element->type_ == kValueMethod ||
      element->type_ == kValueEnum) {
    element->val_.i = ImportStr().GetIdx();
  } else {
    element->val_.u = ReadInt64();
  }
  int64 size = ReadNum();
  for (int64 i = 0; i < size; i++) {
    element->subelemvec_.push_back(ImportPragmaElement());
  }
  return element;
}

MIRPragma *BinaryMplImport::ImportPragma() {
  MIRPragma *p = mod.memPool->New<MIRPragma>(&mod);
  p->pragmaKind = (PragmaKind)ReadNum();
  p->visibility = ReadNum();
  p->strIdx = ImportStr();
  p->tyIdx = ImportType();
  p->tyIdxEx = ImportType();
  p->paramNum = ReadNum();
  int64 size = ReadNum();
  for (int64 i = 0; i < size; i++) {
    p->elementVec.push_back(ImportPragmaElement());
  }
  return p;
}

void BinaryMplImport::ImportFieldPair(FieldPair &fp) {
  fp.first = ImportStr();
  fp.second.first = ImportType();
  fp.second.second.attrFlag = ReadNum();
  fp.second.second.attrAlign = ReadNum();
  FieldAttrs fa = fp.second.second;
  if (fa.GetAttr(FLDATTR_static) && fa.GetAttr(FLDATTR_final) &&
      (fa.GetAttr(FLDATTR_public) || fa.GetAttr(FLDATTR_protected))) {
    int64 tag = ReadNum();
    if (tag == kBinInitConst) {
      GlobalTables::GetConstPool().InsertConstPool(fp.first, ImportConst(nullptr));
    }
  }
}

void BinaryMplImport::ImportMethodPair(MethodPair &memPool) {
  std::string funcName;
  ReadAsciiStr(funcName);
  TyIdx funcTyidx = ImportType();
  int64 x = ReadNum();
  CHECK_FATAL(x >= 0, "ReadNum error, x: %d", x);
  uint64 attrFlag = static_cast<uint64>(x);

  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcName);
  MIRSymbol *prevFuncSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
  MIRSymbol *funcSt = nullptr;
  MIRFunction *fn = nullptr;

  if (prevFuncSt != nullptr && (prevFuncSt->storageClass == kScText && prevFuncSt->sKind == kStFunc)) {
    funcSt = prevFuncSt;
    fn = funcSt->GetFunction();
  } else {
    funcSt = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
    funcSt->SetNameStridx(strIdx);
    GlobalTables::GetGsymTable().AddToStringSymbolMap(funcSt);
    funcSt->storageClass = kScText;
    funcSt->sKind = kStFunc;
    funcSt->SetTyIdx(funcTyidx);
    funcSt->isImported = imported;
    method_symbols.push_back(funcSt);

    fn = mod.memPool->New<MIRFunction>(&mod, funcSt->GetStIdx());
    fn->puIdx = GlobalTables::GetFunctionTable().funcTable.size();
    GlobalTables::GetFunctionTable().funcTable.push_back(fn);
    funcSt->SetFunction(fn);
    MIRFuncType *funcType = dynamic_cast<MIRFuncType*>(funcSt->GetType());
    fn->funcType = funcType;
    fn->fileIndex = 0;
    fn->SetBaseClassFuncNames(funcSt->nameStrIdx);
    fn->funcAttrs.attrFlag = attrFlag;
  }
  memPool.first.SetFullIdx(funcSt->stIdx.FullIdx());
  memPool.second.first.SetIdx(funcTyidx.GetIdx());
  memPool.second.second.attrFlag = attrFlag;
}

void BinaryMplImport::UpdateMethodSymbols() {
  for (auto sym : method_symbols) {
    MIRFunction *fn = sym->GetFunction();
    CHECK_FATAL(fn != nullptr, "fn is null");
    MIRFuncType *funcType = static_cast<MIRFuncType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->GetTyIdx()));
    fn->funcType = funcType;
    fn->SetReturnStruct(GlobalTables::GetTypeTable().GetTypeFromTyIdx(funcType->retTyIdx));
    if (fn->formalDefVec.size() != 0) {
      continue;  // already updated in ImportFunction()
    }
    for (size_t i = 0; i < funcType->paramTypeList.size(); i++ ) {
      FormalDef formalDef(nullptr, funcType->paramTypeList[i], funcType->paramAttrsList[i]);
      fn->formalDefVec.push_back(formalDef);
    }
  }
}

void BinaryMplImport::ImportFieldsOfStructType(FieldVector &fields) {
  int64 size = ReadNum();
  bool isEmpty = fields.empty();
  for (int64 i = 0; i < size; i++) {
    FieldPair fp;
    ImportFieldPair(fp);
    if (isEmpty) {
      fields.push_back(fp);
    }
  }
}

void BinaryMplImport::ImportMethodsOfStructType(MethodVector &methods) {
  int64 size = ReadNum();
  bool isEmpty = methods.empty();
  for (int64 i = 0; i < size; i++) {
    MethodPair memPool;
    ImportMethodPair(memPool);
    if (isEmpty) {
      methods.push_back(memPool);
    }
  }
}

void BinaryMplImport::ImportStructTypeData(MIRStructType *type) {
  ImportFieldsOfStructType(type->fields);
  ImportFieldsOfStructType(type->staticFields);
  ImportFieldsOfStructType(type->parentFields);
  ImportMethodsOfStructType(type->methods);
  type->isImported = imported;
}

void BinaryMplImport::ImportInterfacesOfClassType(std::vector<TyIdx> &interfaces) {
  int64 size = ReadNum();
  bool isEmpty = interfaces.empty();
  for (int64 i = 0; i < size; i++) {
    TyIdx idx = ImportType();
    if (isEmpty) {
      interfaces.push_back(idx);
    }
  }
}

void BinaryMplImport::ImportInfoIsStringOfClassType(std::vector<bool> &infoIsString) {
  int64 size = ReadNum();
  bool isEmpty = infoIsString.empty();

  for (int64 i = 0; i < size; i++) {
    bool isString = static_cast<bool>(ReadNum());

    if (isEmpty) {
      infoIsString.push_back(isString);
    }
  }
}

void BinaryMplImport::ImportInfoOfClassType(std::vector<bool> &infoIsString, std::vector<MIRInfoPair> &infos) {
  int64 size = ReadNum();
  bool isEmpty = infos.empty();
  for (int64 i = 0; i < size; i++) {
    GStrIdx idx = ImportStr();
    int64 x = (infoIsString[i]) ? ImportStr().GetIdx() : ReadNum();
    CHECK_FATAL(x >= 0 && x <= std::numeric_limits<uint32_t>::max(), "ReadNum too large, x: %d", x);
    if (isEmpty) {
      infos.push_back(MIRInfoPair(idx, static_cast<uint32>(x)));
    }
  }
}

void BinaryMplImport::ImportPragmaOfClassType(std::vector<MIRPragma*> &pragmas) {
  int64 size = ReadNum();
  bool isEmpty = pragmas.empty();
  for (int64 i = 0; i < size; i++) {
    MIRPragma *pragma = ImportPragma();
    if (isEmpty) {
      pragmas.push_back(pragma);
    }
  }
}

void BinaryMplImport::SetClassTyidxOfMethods(MIRStructType *type) {
  if (type->GetTypeIndex() != 0) {
    // set up classTyIdx for methods
    for (size_t i = 0; i < type->methods.size(); i++) {
      StIdx stIdx = type->methods[i].first;
      MIRSymbol *st = GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx());
      CHECK(st != nullptr, "st is null");
      CHECK(st->sKind == kStFunc, "unexpected st->sKind");
      st->value.mirFunc->classTyIdx = type->GetTypeIndex();
    }
  }
}

void BinaryMplImport::ImportClassTypeData(MIRClassType *type) {
  type->parentTyIdx = ImportType();
  ImportInterfacesOfClassType(type->interfacesImplemented);
  ImportInfoIsStringOfClassType(type->infoIsString);
  if (!in_ipa) {
    ImportInfoOfClassType(type->infoIsString, type->info);
    ImportPragmaOfClassType(type->pragmaVec);
  }
  SetClassTyidxOfMethods(type);
}

void BinaryMplImport::ImportInterfaceTypeData(MIRInterfaceType *type) {
  ImportInterfacesOfClassType(type->parentsTyIdx);
  ImportInfoIsStringOfClassType(type->infoIsString);
  if (!in_ipa) {
    ImportInfoOfClassType(type->infoIsString, type->info);
    ImportPragmaOfClassType(type->pragmaVec);
  }
  SetClassTyidxOfMethods(type);
}

void BinaryMplImport::Reset() {
  buf.clear();
  bufI = 0;
  gstr_tab.clear();
  ustr_tab.clear();
  typ_tab.clear();
  func_tab.clear();
  sym_tab.clear();
  method_symbols.clear();
  typedef_idx_map.clear();
  _defined_labels.clear();
  gstr_tab.push_back(GStrIdx(0));  // Dummy
  ustr_tab.push_back(UStrIdx(0));  // Dummy
  sym_tab.push_back(nullptr);        // Dummy
  func_tab.push_back(nullptr);       // Dummy
  for (int32 pti = static_cast<int32>(PTY_begin); pti < static_cast<int32>(PTY_end); pti++) {
    typ_tab.push_back(GlobalTables::GetTypeTable().typeTable[pti]);
  }
}

TypeAttrs BinaryMplImport::ImportTypeAttrs() {
  TypeAttrs ta;
  ta.attrFlag = ReadNum();
  ta.attrAlign = ReadNum();
  return ta;
}

void BinaryMplImport::ImportTypePairs(std::vector<TypePair> &tpairs) {
  int64 size = ReadNum();
  for (int64 i = 0; i < size; i++) {
    TyIdx t0 = ImportType();
    TyIdx t1 = ImportType();
    TypePair tp(t0, t1);
    tpairs.push_back(tp);
  }
}

TyIdx BinaryMplImport::ImportType() {
  int64 tag = ReadNum();
  if (tag == 0) {
    return TyIdx(0);
  } else if (tag < 0) {
    ASSERT(-tag < typ_tab.size(), "index out of bounds");
    MIRType *cachedType = typ_tab[-tag];
    return cachedType->tyIdx;
  }
  if (tag == kBinKindTypeViaTypename) {
    GStrIdx typenameStrIdx = ImportStr();
    TyIdx tyIdx = mod.typeNameTab->GetTyIdxFromGStrIdx(typenameStrIdx);
    if (tyIdx != 0) {
      MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
      typ_tab.push_back(ty);
      return tyIdx;
    }
    MIRTypeByName ltype(typenameStrIdx);
    ltype.nameIsLocal = false;
    MIRType *type = GlobalTables::GetTypeTable().GetOrCreateMIRTypeNode(&ltype);
    typ_tab.push_back(type);
    return type->tyIdx;
  }
  PrimType primtype = (PrimType)0;
  GStrIdx strIdx(0);
  bool nameIsLocal = false;
  ImportTypeBase(primtype, strIdx, nameIsLocal);
  switch (tag) {
    case kBinKindTypeByName: {
      MIRTypeByName ltype(strIdx);
      ltype.primType = primtype;
      ltype.nameIsLocal = nameIsLocal;
      MIRType *type = GlobalTables::GetTypeTable().GetOrCreateMIRTypeNode(&ltype);
      typ_tab.push_back(type);
      return type->tyIdx;
    }
    case kBinKindTypeScalar: {
      return TyIdx(primtype);
    }
    case kBinKindTypePointer: {
      MIRPtrType *type = new MIRPtrType(primtype, strIdx);
      type->primType = mod.IsJavaModule() ? PTY_ref : PTY_ptr;
      type->nameIsLocal = nameIsLocal;
      MIRType *origType = InsertInTypeTables(type);
      typ_tab.push_back(origType);
      type->pointedTyIdx = ImportType();
      type->typeAttrs = ImportTypeAttrs();
      if (type->typeAttrs == TypeAttrs()) {
        auto *pMap = (type->primType == PTY_ptr ? &GlobalTables::GetTypeTable().ptrTypeMap : &GlobalTables::GetTypeTable().refTypeMap);
        auto it = pMap->find(type->pointedTyIdx);
        if (it == pMap->end()) {
          (*pMap)[type->pointedTyIdx] = type->tyIdx;
        }
      } else {
        GlobalTables::GetTypeTable().PutToHashTable(type);
      }
      return origType->tyIdx;
    }
    case kBinKindTypeFArray: {
      MIRFarrayType *type = new MIRFarrayType(strIdx);
      type->primType = primtype;
      type->nameIsLocal = nameIsLocal;
      MIRType *origType = InsertInTypeTables(type);
      typ_tab.push_back(origType);
      type->elemTyIdx = ImportType();
      GlobalTables::GetTypeTable().PutToHashTable(type);
      return origType->tyIdx;
    }
    case kBinKindTypeJarray: {
      MIRJarrayType *type = new MIRJarrayType(strIdx);
      type->primType = primtype;
      type->nameIsLocal = nameIsLocal;
      MIRType *origType = InsertInTypeTables(type);
      typ_tab.push_back(origType);
      type->elemTyIdx = ImportType();
      GlobalTables::GetTypeTable().PutToHashTable(type);
      return origType->tyIdx;
    }
    case kBinKindTypeArray: {
      MIRArrayType *type = new MIRArrayType(strIdx);
      type->primType = primtype;
      type->nameIsLocal = nameIsLocal;
      MIRType *origType = InsertInTypeTables(type);
      typ_tab.push_back(origType);
      type->dim = ReadNum();
      for (uint16 i = 0; i < type->dim; i++) {
        type->sizeArray[i] = ReadNum();
      }
      type->eTyIdx = ImportType();
      GlobalTables::GetTypeTable().PutToHashTable(type);
      return origType->tyIdx;
    }
    case kBinKindTypeFunction: {
      MIRFuncType *type = new MIRFuncType(strIdx);
      type->primType = primtype;
      type->nameIsLocal = nameIsLocal;
      MIRType *origType = InsertInTypeTables(type);
      typ_tab.push_back(origType);
      type->retTyIdx = ImportType();
      type->isVarArgs = ReadNum();
      int64 size = ReadNum();
      for (int64 i = 0; i < size; i++) {
        type->paramTypeList.push_back(ImportType());
      }
      size = ReadNum();
      for (int64 i = 0; i < size; i++) {
        type->paramAttrsList.push_back(ImportTypeAttrs());
      }
      GlobalTables::GetTypeTable().PutToHashTable(type);
      return origType->tyIdx;
    }
    case kBinKindTypeParam: {
      MIRTypeParam ltype(strIdx);
      ltype.primType = primtype;
      ltype.nameIsLocal = nameIsLocal;
      MIRType *type = GlobalTables::GetTypeTable().GetOrCreateMIRTypeNode(&ltype);
      typ_tab.push_back(type);
      return type->tyIdx;
    }
    case kBinKindTypeInstantVector: {
      MIRTypeKind kind = (MIRTypeKind)ReadNum();
      MIRInstantVectorType *type = new MIRInstantVectorType(kind, strIdx);
      type->primType = primtype;
      type->nameIsLocal = nameIsLocal;
      MIRType *origType = InsertInTypeTables(type);
      typ_tab.push_back(origType);
      ImportTypePairs(type->instantVec);
      GlobalTables::GetTypeTable().PutToHashTable(type);
      return origType->tyIdx;
    }
    case kBinKindTypeGenericInstant: {
      MIRGenericInstantType *type = new MIRGenericInstantType(strIdx);
      type->primType = primtype;
      type->nameIsLocal = nameIsLocal;
      MIRType *origType = InsertInTypeTables(type);
      typ_tab.push_back(origType);
      ImportTypePairs(type->instantVec);
      type->genericTyIdx = ImportType();
      GlobalTables::GetTypeTable().PutToHashTable(type);
      return origType->tyIdx;;
    }
    case kBinKindTypeBitField: {
      uint8 fieldsize = ReadNum();
      MIRBitfieldType ltype(fieldsize, primtype, strIdx);
      ltype.primType = primtype;
      ltype.nameIsLocal = nameIsLocal;
      MIRType *type = GlobalTables::GetTypeTable().GetOrCreateMIRTypeNode(&ltype);
      typ_tab.push_back(type);
      return type->tyIdx;
    }
    case kBinKindTypeStruct: {
      MIRTypeKind kind = (MIRTypeKind)ReadNum();
      MIRStructType *type = new MIRStructType(kind, strIdx);
      type->nameIsLocal = nameIsLocal;
      MIRType *origType = InsertInTypeTables(type);
      typ_tab.push_back(origType);
      if (kind != kTypeStructIncomplete) {
        ImportStructTypeData(type);
      }
      GlobalTables::GetTypeTable().PutToHashTable(type);
      return origType->tyIdx;
    }
    case kBinKindTypeClass: {
      MIRTypeKind kind = (MIRTypeKind)ReadNum();
      MIRClassType *type = new MIRClassType(kind, strIdx);
      type->nameIsLocal = nameIsLocal;
      MIRType *origType = InsertInTypeTables(type);
      typ_tab.push_back(origType);
      if (kind != kTypeClassIncomplete) {
        ImportStructTypeData(type);
        ImportClassTypeData(type);
      }
      GlobalTables::GetTypeTable().PutToHashTable(type);
      return origType->tyIdx;
    }
    case kBinKindTypeInterface: {
      MIRTypeKind kind = (MIRTypeKind)ReadNum();
      MIRInterfaceType *type = new MIRInterfaceType(kind, strIdx);
      type->nameIsLocal = nameIsLocal;
      MIRType *origType = InsertInTypeTables(type);
      typ_tab.push_back(origType);
      if (kind != kTypeInterfaceIncomplete) {
        ImportStructTypeData(type);
        ImportInterfaceTypeData(type);
      }
      GlobalTables::GetTypeTable().PutToHashTable(type);
      return origType->tyIdx;
    }
    default:
      CHECK_FATAL(false, "Unexpected binary kind");
      return TyIdx();
  }
}

void BinaryMplImport::ImportTypeBase(PrimType &primType, GStrIdx &strIdx, bool &nameIsLocal) {
  primType = (PrimType)ReadNum();
  strIdx = ImportStr();
  nameIsLocal = ReadNum();
}

inline static bool IsIncomplete(const MIRType *type) {
  return (type->typeKind == kTypeInterfaceIncomplete || type->typeKind == kTypeClassIncomplete ||
          type->typeKind == kTypeStructIncomplete);
}

inline static bool IsObject(const MIRType *type) {
  return (type->typeKind == kTypeClass || type->typeKind == kTypeClassIncomplete || type->typeKind == kTypeInterface ||
          type->typeKind == kTypeInterfaceIncomplete);
}

MIRType *BinaryMplImport::InsertInTypeTables(MIRType *type) {
  TyIdx prevTyidx = mod.typeNameTab->GetTyIdxFromGStrIdx(type->nameStrIdx);
  if (prevTyidx != 0 && !type->nameIsLocal) {
    MIRType *prevType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(prevTyidx);
    if ((IsIncomplete(prevType) && IsIncomplete(type)) || (!IsIncomplete(prevType) && !IsIncomplete(type)) ||
        (!IsIncomplete(prevType) && IsIncomplete(type))) {
      CHECK_FATAL((IsObject(prevType) && IsObject(type)) || (type->typeKind == type->typeKind), "impossible");
      type = prevType;
    } else {
      CHECK_FATAL((IsObject(prevType) && IsObject(type)) || (type->typeKind == type->typeKind), "impossible");
      // New definition wins
      type->tyIdx = prevTyidx;
      GlobalTables::GetTypeTable().typeTable[prevTyidx.GetIdx()] = type;
    }
  } else {
    // New type, no previous definition or anonymous type
    TyIdx tyIdx(GlobalTables::GetTypeTable().typeTable.size());
    GStrIdx strIdx = type->nameStrIdx;
    type->tyIdx = tyIdx;
    GlobalTables::GetTypeTable().typeTable.push_back(type);

    if (!type->nameIsLocal && strIdx != 0) {
      mod.typeNameTab->SetGStrIdxToTyIdx(strIdx, tyIdx);
      mod.typeDefOrder.push_back(strIdx);
      GlobalTables::GetTypeNameTable().SetGStrIdxToTyIdx(strIdx, tyIdx);
      if (IsObject(type)) {
        mod.AddClass(tyIdx);
      }
    }
  }
  return type;
}

void BinaryMplImport::SetupEhRootType() {
  // setup eh root type with most recent NameMangler::kJavaLangObjectStr
  GStrIdx gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangObjectStr);
  TyIdx tyIdx = GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx);
  if (gStrIdx != 0 && tyIdx != 0) {
    mod.throwableTyidx = tyIdx;
  }
}

MIRSymbol *BinaryMplImport::GetOrCreateSymbol(TyIdx tyIdx, GStrIdx strIdx, MIRSymKind mclass,
                                              MIRStorageClass storageClass, MIRFunction *func, uint8 scpid) {
  MIRSymbol *st = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
  if (st && st->storageClass == storageClass && st->sKind == mclass && scpid == kScopeGlobal) {
    return st;
  }
  return _mirbuilder.CreateSymbol(tyIdx, strIdx, mclass, storageClass, scpid, func);
}

MIRSymbol *BinaryMplImport::InSymbol(MIRFunction *func) {
  MIRSymbol *sym = nullptr;
  int64 tag = ReadNum();
  if (tag == 0) {
    sym = nullptr;
  } else if (tag < 0) {
    sym = sym_tab[-tag];
  } else {
    ASSERT(tag == kBinSymbol, "expecting kBinSymbol");
    int64 scope = ReadNum();
    GStrIdx strIdx = ImportStr();
    MIRSymKind sKind = (MIRSymKind)ReadNum();
    MIRStorageClass storageClass = (MIRStorageClass)ReadNum();
    TyIdx tyTmp(0);
    sym = GetOrCreateSymbol(tyTmp, strIdx, sKind, storageClass, func, scope);
    sym_tab.push_back(sym);
    sym->typeAttrs = ImportTypeAttrs();
    sym->isTmp = (ReadNum() != 0);
    sym->isImported = imported;
    uint32 thepregno = 0;
    if (sKind == kStPreg) {
      ASSERT(scope == kScopeLocal && func != nullptr, "Expecting kScopeLocal");
      thepregno = ReadNum();
    } else if (sKind == kStConst || sKind == kStVar) {
      sym->value.konst = ImportConst(func);
    } else if (sKind == kStFunc) {
      PUIdx puIdx = ImportFunction();
      if (puIdx != 0) {
        sym->value.mirFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
      }
    }
    TyIdx tyIdx = ImportType();
    sym->tyIdx = tyIdx;
    if (sKind == kStPreg) {
      MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
      PregIdx pregidx = func->pregTab->EnterPregNo(thepregno, mirType->primType, mirType);
      MIRPregTable *pregTab = func->pregTab;
      MIRPreg *preg = pregTab->PregFromPregIdx(pregidx);
      preg->primType = mirType->primType;
      sym->value.preg = preg;
    }

    // printf("IMPORT Reading symbol %s\n", sym->GetName().c_str());
  }
  return sym;
}

void BinaryMplImport::ImportLocalWords(MIRFunction *func) {
  func->frameSize = ReadNum();
  int64 tag = ReadNum();
  if (tag == kBinLocalWordsTypeTagged) {
    int32 size = ReadInt();
    func->localWordsTypeTagged = static_cast<uint8 *>(mod.memPool->Malloc(BlkSize2BitvectorSize(func->frameSize)));
    uint32 *p = reinterpret_cast<uint32 *>(func->localWordsTypeTagged);
    for (int32 i = 0; i < size; i++) {
      *p = static_cast<uint32>(ReadNum());
      p++;
    }
  }
  tag = ReadNum();
  if (tag == kBinLocalWordsRefCounter) {
    int32 size = ReadInt();
    func->localWordsRefCounted = static_cast<uint8 *>(mod.memPool->Malloc(BlkSize2BitvectorSize(func->frameSize)));
    uint32 *p = reinterpret_cast<uint32 *>(func->localWordsRefCounted);
    for (int32 i = 0; i < size; i++) {
      *p = static_cast<uint32>(ReadNum());
      p++;
    }
  }
}

void BinaryMplImport::ImportFormalWords(MIRFunction *func) {
  func->upFormalSize = ReadNum();
  int64 tag = ReadNum();
  if (tag == kBinFormalWordsTypeTagged) {
    int32 size = ReadInt();
    func->formalWordsTypeTagged = static_cast<uint8 *>(mod.memPool->Malloc(BlkSize2BitvectorSize(func->upFormalSize)));
    uint32 *p = reinterpret_cast<uint32 *>(func->formalWordsTypeTagged);
    for (int32 i = 0; i < size; i++) {
      *p = static_cast<uint32>(ReadNum());
      p++;
    }
  }
  tag = ReadNum();
  if (tag == kBinFormalWordsRefCounted) {
    int32 size = ReadInt();
    func->formalWordsRefCounted = static_cast<uint8 *>(mod.memPool->Malloc(BlkSize2BitvectorSize(func->upFormalSize)));
    uint32 *p = reinterpret_cast<uint32 *>(func->formalWordsRefCounted);
    for (int32 i = 0; i < size; i++) {
      *p = static_cast<uint32>(ReadNum());
      p++;
    }
  }
}

void BinaryMplImport::ImportSymTab(MIRFunction *func) {
  int32 size = ReadInt();
  for (int32 i = 0; i < size; i++) {
    InSymbol(func);
  }
}

PUIdx BinaryMplImport::ImportFunction() {
  int64 tag = ReadNum();
  if (tag == 0) {
    mod.SetCurFunction(nullptr);
    return 0;
  } else if (tag < 0) {
    ASSERT(-tag <= func_tab.size(), "index out of bounds");
    if (-tag == func_tab.size()) { // function was exported before its symbol
      return (PUIdx)0;
    }
    PUIdx puIdx =  func_tab[-tag]->puIdx;
    mod.SetCurFunction(GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx));
    return puIdx;
  }
  ASSERT(tag == kBinFunction, "expecting kBinFunction");
  MIRSymbol *funcSt = InSymbol(nullptr);
  CHECK_FATAL(funcSt, "null ptr check");
  MIRFunction *func = nullptr;
  if (funcSt->GetFunction() == nullptr) {
    func = mod.memPool->New<MIRFunction>(&mod, funcSt->GetStIdx());
    func->puIdx = GlobalTables::GetFunctionTable().funcTable.size();
    GlobalTables::GetFunctionTable().funcTable.push_back(func);
    func_tab.push_back(func);
  } else {
    func = funcSt->GetFunction();
    func_tab.push_back(func);
  }
  //LogInfo::MapleLogger() << func->GetName() << " pushed to func_tab\n";
  funcSt->SetFunction(func);
  method_symbols.push_back(funcSt);
  if (mod.IsJavaModule()) {
    func->SetBaseClassFuncNames(funcSt->nameStrIdx);
  }
  TyIdx funcTyIdx = ImportType();
  func->funcType = static_cast<MIRFuncType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(funcTyIdx));

  func->stIdx = funcSt->GetStIdx();

  if (in_cg) {
    if (!func->isDirty) {
      func->isDirty = true;
      func->funcAttrs.attrFlag = ReadNum();  // merge side effect
    } else {
      FuncAttrs tmp;
      tmp.attrFlag = ReadNum();
      if (func->IsNoDefEffect() != tmp.GetAttr(FUNCATTR_nodefeffect)) {
        tmp.SetAttr(FUNCATTR_nodefeffect, true);
      }
      if (func->IsNoThrowException() != tmp.GetAttr(FUNCATTR_nothrow_exception)) {
        tmp.SetAttr(FUNCATTR_nothrow_exception, true);
      }
      if (func->IsIpaSeen() != tmp.GetAttr(FUNCATTR_ipaseen)) {
        tmp.SetAttr(FUNCATTR_ipaseen);
      }
      if (func->IsPure() != tmp.GetAttr(FUNCATTR_pure)) {
        tmp.SetAttr(FUNCATTR_pure, true);
      }
      if (func->IsNoPrivateDefEffect() != tmp.GetAttr(FUNCATTR_noprivate_defeffect)) {
        tmp.SetAttr(FUNCATTR_noprivate_defeffect, true);
      }
      func->funcAttrs = tmp;
    }
  } else {
    func->funcAttrs.attrFlag = ReadNum();  // merge side effect
  }
  func->flag = ReadNum();
  /* func->classTyIdx = */ImportType();  // not set the field to mimic parser
  // import formal parameter informcation
  size_t size = ReadNum();
  if (func->formalDefVec.size() == 0) {
    for (size_t i = 0; i < size; i++) {
      GStrIdx strIdx = ImportStr();
      TyIdx tyIdx = ImportType();
      FormalDef formalDef(strIdx, nullptr, tyIdx, TypeAttrs());
      formalDef.formalAttrs.attrFlag = ReadNum();
      func->formalDefVec.push_back(formalDef);
    }
  } else {
    ASSERT(func->formalDefVec.size() >= size, "ImportFunction: inconsistent number of formals");
    for (size_t i = 0; i < size; i++) {
      func->formalDefVec[i].formalStrIdx = ImportStr();
      func->formalDefVec[i].formalTyIdx = ImportType();
      func->formalDefVec[i].formalAttrs.attrFlag = ReadNum();
    }
  }

  mod.SetCurFunction(func);
  return func->puIdx;
}

inline void BinaryMplImport::SkipTotalSize() {
  ReadInt();
}

void BinaryMplImport::ReadStrField() {
  SkipTotalSize();
  int32 size = ReadInt();
  for (int64 i = 0; i < size; i++) {
    GStrIdx strIdx = ImportStr();
    GlobalTables::GetConstPool().PutLiteralNameAsImported(strIdx);
  }
  int64 tag = ReadNum();
  CHECK_FATAL(tag == ~kBinStrStart, "pattern mismatch in Read STR");
  return;
}

void BinaryMplImport::ReadHeaderField() {
  SkipTotalSize();
  mod.flavor = (MIRFlavor)ReadNum();
  mod.srcLang = (MIRSrcLang)ReadNum();
  mod.id = ReadNum();
  mod.numFuncs = ReadNum();
  ReadAsciiStr(mod.entryFuncName);
  ImportInfoVector(mod.fileInfo, mod.fileInfoIsString);

  int32 size = ReadNum();
  MIRInfoPair infopair;
  for (int32 i = 0; i < size; i++) {
    infopair.first = ImportStr();
    infopair.second = ReadNum();
    mod.srcFileInfo.push_back(infopair);
  }

  size = ReadNum();
  for (int32 i = 0; i < size; i++) {
    GStrIdx gStrIdx  = ImportStr();
    mod.importFiles.push_back(gStrIdx);
    std::string importfilename = GlobalTables::GetStrTable().GetStringFromStrIdx(gStrIdx);
    // record the imported file for later reading summary info, if exists
    mod.importedMplt.push_back(importfilename);
    BinaryMplt *binMplt = new BinaryMplt(mod);
    binMplt->GetBinImport().imported = true;

    INFO(kLncInfo, "importing %s", importfilename.c_str());
    if (!binMplt->GetBinImport().Import(importfilename, false)) {  // not a binary mplt
      FATAL(kLncFatal, "cannot open binary MPLT file: %s\n", importfilename.c_str());
    } else {
      INFO(kLncInfo, "finished import of %s", importfilename.c_str());
    }
    if (i == 0) {
      binMplt->SetImportFileName(importfilename);
      mod.binMplt = binMplt;
    }
  }
  int32 tag = ReadNum();
  CHECK_FATAL(tag == ~kBinHeaderStart, "pattern mismatch in Read Import");
  return;
}

void BinaryMplImport::ReadTypeField() {
  SkipTotalSize();
  int32 size = ReadInt();
  for (int64 i = 0; i < size; i++) {
    ImportType();
  }
  int64 tag = ReadNum();
  CHECK_FATAL(tag == ~kBinTypeStart, "pattern mismatch in Read TYPE");
  return;
}

void BinaryMplImport::ReadSymField() {
  SkipTotalSize();
  int32 size = ReadInt();
  for (int64 i = 0; i < size; i++) {
    InSymbol(nullptr);
  }
  int64 tag = ReadNum();
  CHECK_FATAL(tag == ~kBinSymStart, "pattern mismatch in Read SYM");
  return;
}

void BinaryMplImport::ReadSymTabField() {
  SkipTotalSize();
  int32 size = ReadInt();
  for (int64 i = 0; i < size; i++) {
    std::string str;
    ReadAsciiStr(str);
    GStrIdx name = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(str);
    int64 mark = ReadNum();
    sym_table_cg[name] = mark;
  }
  int64 tag = ReadNum();
  CHECK_FATAL(tag == ~kBinSymTabStart, "pattern mismatch in Read TYPE");
  return;
}

CallInfo *BinaryMplImport::InCallInfo() {
  int64 tag = ReadNum();
  if (tag < 0) {
    CHECK_FATAL(-tag < callinfo_tab.size(), "index out of bounds");
    return callinfo_tab[-tag];
  }
  CHECK_FATAL(tag == kBinCallinfo, "expecting kBinCallinfo");
  CallType ctype = (CallType)ReadNum();  /// call type
  uint32 loopDepth = static_cast<uint32>(ReadInt());
  uint32 id = ReadInt();
  MIRSymbol *funcSym = InSymbol(nullptr);
  CHECK_FATAL(funcSym != nullptr, "func_sym is null in BinaryMplImport::InCallInfo");
  CallInfo *ret =
    mod.memPool->New<CallInfo>(ctype, static_cast<char *>(nullptr), funcSym->GetFunction(),
                            static_cast<StmtNode *>(nullptr), loopDepth, id);
  callinfo_tab.push_back(ret);
  return ret;
}

void BinaryMplImport::MergeDuplicated(PUIdx methodPuidx, std::vector<CallSite> &targetSet,
                                      std::vector<CallSite> &newSet) {
  if (targetSet.size() == 0) {
    targetSet.insert(targetSet.begin(), newSet.begin(), newSet.end());
    std::unordered_set<uint64> tmp;
    mod.method2TargetHash[methodPuidx] = tmp;
    for (uint32 i = 0; i < newSet.size(); i++) {
      uint64 key = (static_cast<uint64>(newSet[i].first->GetID()) << 32) + newSet[i].second;
      mod.method2TargetHash[methodPuidx].insert(key);
    }
  } else {
    for (uint32 i = 0; i < newSet.size(); i++) {
      CallSite newItem = newSet[i];
      uint64 key = (static_cast<uint64>(newItem.first->GetID()) << 32) + newItem.second;
      if (mod.method2TargetHash[methodPuidx].find(key) == mod.method2TargetHash[methodPuidx].end()) {
        targetSet.push_back(newItem);
        mod.method2TargetHash[methodPuidx].insert(key);
      }
    }
  }
}

void BinaryMplImport::ReadCgField() {
  SkipTotalSize();
  int32 size = ReadInt();
  int64 tag = 0;;
  // CHECK_FATAL(size, "should not be zero in %s", import_file_name.c_str());
  // LogInfo::MapleLogger() << "CG SIZE : " << size << " in " << import_file_name<<std::endl;
  for (int i = 0; i < size; i++) {
    tag = ReadNum();
    CHECK_FATAL(tag == kStartMethod, " should be start point of method");
    PUIdx methodPuidx = InSymbol(nullptr)->value.mirFunc->puIdx;
    CHECK_FATAL(methodPuidx, "should not be 0");
    if (mod.method2TargetMap.find(methodPuidx) != mod.method2TargetMap.end()) {
      // LogInfo::MapleLogger() << "@@@ Trige METHOD MERGE of Mplt : " << globaltable.funcTable[method_puidx]->GetName() <<
      // std::endl;
    } else {
      std::vector<CallSite> targetSetTmp;
      mod.method2TargetMap[methodPuidx] = targetSetTmp;
    }
    int32 targSize = ReadInt();
    std::vector<CallSite> targetSet;
    callinfo_tab.clear();
    callinfo_tab.push_back(nullptr);
    // LogInfo::MapleLogger() << "IN method : " << globaltable.funcTable[method_puidx]->GetName() << std::endl;
    for (int j = 0; j < targSize; j++) {
      CallInfo *callInfo = InCallInfo();
      // LogInfo::MapleLogger() << "    ID: " <<  call_info->GetID()<< std::endl;
      MIRSymbol *targetSym = InSymbol(nullptr);
      PUIdx targetPuidx = 0;
      if (targetSym) {
        targetPuidx = targetSym->value.mirFunc->puIdx;
      }
      // CHECK_FATAL(target_puidx, "should not be 0");
      targetSet.push_back(std::make_pair(callInfo, targetPuidx));
    }
    MergeDuplicated(methodPuidx, mod.method2TargetMap[methodPuidx], targetSet);
    /*
       LogInfo::MapleLogger() << "in method : " << globaltable.funcTable[method_puidx]->GetName() << std::endl;
       for(uint32 k = 0; k < mod.method2TargetMap[method_puidx].size(); k++){
       LogInfo::MapleLogger() << "    target : id:" << mod.method2TargetMap[method_puidx][k].first->GetID() << " name : " <<
       globaltable.funcTable[mod.method2TargetMap[method_puidx][k].second]->GetName() << std::endl;
       }
     */
    tag = ReadNum();
    CHECK_FATAL(tag == ~kStartMethod, " should be start point of method");
  }
  tag = ReadNum();
  CHECK_FATAL(tag == ~kBinCgStart, "pattern mismatch in Read CG");
  return;
}

void BinaryMplImport::ReadSeField() {
  SkipTotalSize();
  int32 size = ReadInt();
#ifdef MPLT_DEBUG
  LogInfo::MapleLogger() << "SE SIZE : " << size << std::endl;
#endif
  for (int64 i = 0; i < size; i++) {
    GStrIdx funcName = ImportStr();
    uint8 se = (uint8)Read();
    if (GlobalTables::GetStrTable().GetStringFromStrIdx(funcName) ==
        (NameMangler::EncodeName(NameMangler::kJavaLangObjectStr) + "_7Cwait_7C_28_29V")) {
      se = 0;
    }
    // LogInfo::MapleLogger() << "func : " << globaltable.GetStringFromStrIdx(func_name) << "  se : " << se << std::endl;
    GlobalTables::GetSideEffectTable().InsertSideEffect(funcName, se);
  }
  int64 tag = ReadNum();
  CHECK_FATAL(tag == ~kBinSeStart, "pattern mismatch in Read TYPE");
  return;
}

void BinaryMplImport::ReadContentField() {
  SkipTotalSize();
  int32 size = ReadInt();
  int64 item;
  int32 offset;
  for (int i = 0; i < size; i++) {
    item = ReadNum();
    offset = ReadInt();
    content[item] = offset;
  }
  CHECK_FATAL(ReadNum() == ~kBinContentStart, "pattern mismatch in Read CONTENT");
}

void BinaryMplImport::Jump2NextField() {
  uint32 totalSize = ReadInt();
  bufI += (totalSize - sizeof(uint32));
  ReadNum();  /// skip end tag for this field
}

bool BinaryMplImport::Import(const std::string &fname, bool readSymbols, bool readSe) {
  Reset();
  ReadFileAt(fname, 0);
  int32 magic = ReadInt();
  if (kMpltMagicNumber != magic && (kMpltMagicNumber+0x10) != magic) {
    buf.clear();
    return false;
  }
  importing_from_mplt = kMpltMagicNumber == magic;
  int64 fieldId = ReadNum();
  while (fieldId != kBinFinish) {
    switch (fieldId) {
      case kBinContentStart: {
        ReadContentField();
        break;
      }
      case kBinStrStart: {
        ReadStrField();
        break;
      }
      case kBinHeaderStart: {
        ReadHeaderField();
        break;
      }
      case kBinTypeStart: {
        ReadTypeField();
        break;
      }
      case kBinSymStart: {
        if (readSymbols) {
          ReadSymField();
        } else {
          Jump2NextField();
        }
        break;
      }
      case kBinSymTabStart: {
        ReadSymTabField();
        break;
      }
      case kBinCgStart: {
        if (readSymbols) {
#ifdef MPLT_DEBUG
          LogInfo::MapleLogger() << "read CG of : " << fname << std::endl;
#endif
          BinaryMplImport tmp(mod);
          tmp.Reset();
          tmp.in_ipa = true;
          tmp.buf = buf;
          tmp.bufI = bufI;
          tmp.import_file_name = fname;
          tmp.ReadCgField();
          Jump2NextField();
        } else {
          Jump2NextField();
        }
        break;
      }
      case kBinSeStart: {
        if (readSe) {
#ifdef MPLT_DEBUG
          LogInfo::MapleLogger() << "read SE of : " << fname << std::endl;
#endif
          BinaryMplImport tmp(mod);
          tmp.Reset();
          tmp.buf = buf;
          tmp.bufI = bufI;
          tmp.import_file_name = fname;
          tmp.ReadSeField();
          Jump2NextField();
        } else {
          Jump2NextField();
        }
        break;
      }
      case kBinFunctionBodyStart: {
        ReadFunctionBodyField();
        break;
      }
      default:
        CHECK_FATAL(false, "should not run here");
    }
    fieldId = ReadNum();
  }
  UpdateMethodSymbols();
  SetupEhRootType();
  return true;
}

}  // namespace maple
