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

#ifndef MAPLE_IR_INCLUDE_GLOBAL_TABLES_H
#define MAPLE_IR_INCLUDE_GLOBAL_TABLES_H

#include <iostream>
#include <functional>
#include "mempool.h"
#include "mempool_allocator.h"
#include "types_def.h"
#include "prim_types.h"
#include "mir_module.h"
#include "mir_type.h"

namespace maple {

class MIRType;
class TypeAttrs;
class MIRStructType;
class MIRArrayType;
class BinaryMplImport;

// to facilitate the use of unordered_map
class TyIdxHash
{
 public:
  std::size_t operator()(const TyIdx &tyIdx) const {
    return std::hash<uint32>{}(tyIdx.GetIdx());
  }
};

// to facilitate the use of unordered_map
class GStrIdxHash
{
 public:
  std::size_t operator()(const GStrIdx &gStrIdx) const {
    return std::hash<uint32>{}(gStrIdx.GetIdx());
  }
};

// to facilitate the use of unordered_map
class UStrIdxHash
{
 public:
  std::size_t operator()(const UStrIdx &uStrIdx) const {
    return std::hash<uint32>{}(uStrIdx.GetIdx());
  }
};

class TypeTable {
  friend BinaryMplImport;
 private:
  using MIRTypePtr = MIRType*;

  struct TyIdxHash {
    std::size_t operator()(const TyIdx &tyIdx) const {
      return std::hash<uint32>{}(tyIdx.GetIdx());
    }
  };

  struct Hash {
    uint32 operator()(const MIRTypePtr &ty) const {
      return ty->GetHashIndex();
    }
  };
  struct Equal {
    uint32 operator()(const MIRTypePtr &tx, const MIRTypePtr &ty) const {
      return tx->Equalto(*ty);
    }
  };
  std::unordered_multiset<MIRTypePtr, Hash, Equal> typeHashTable;
  std::unordered_map<TyIdx, TyIdx, TyIdxHash> ptrTypeMap;
  std::unordered_map<TyIdx, TyIdx, TyIdxHash> refTypeMap;

 public:
  std::vector<MIRType *> typeTable;
  static MIRType *voidPtrType;
  TyIdx lastDefaultTyIdx;

  TypeTable();
  ~TypeTable();
  void PutToHashTable(MIRType *mirType) {
    typeHashTable.insert(mirType);
  }
  MIRType *GetTypeFromTyIdx(TyIdx tyIdx) const {
    ASSERT(tyIdx.GetIdx() < typeTable.size(), "");
    return typeTable.at(tyIdx.GetIdx());
  }

  MIRType *CreateMIRTypeNode(MIRType *ptype);
  TyIdx CreateMIRType(MIRType *ptype) { return CreateMIRTypeNode(ptype)->tyIdx; }
  MIRType *GetOrCreateMIRTypeNode(MIRType *ptype);
  TyIdx GetOrCreateMIRType(MIRType *ptype) { return GetOrCreateMIRTypeNode(ptype)->tyIdx; }

  // Get primtive types.
  MIRType *GetPrimType(PrimType primType) const {
    return typeTable.at(primType);
  }

  MIRType *GetFloat() const {
    return typeTable.at(PTY_f32);
  }

  MIRType *GetDouble() const {
    return typeTable.at(PTY_f64);
  }

  MIRType *GetFloat128() const {
    return typeTable.at(PTY_f128);
  }

  MIRType *GetUInt1() const {
    return typeTable.at(PTY_u1);
  }

  MIRType *GetUInt8() const {
    return typeTable.at(PTY_u8);
  }

  MIRType *GetInt8() const {
    return typeTable.at(PTY_i8);
  }

  MIRType *GetUInt16() const {
    return typeTable.at(PTY_u16);
  }

  MIRType *GetInt16() const {
    return typeTable.at(PTY_i16);
  }

  MIRType *GetInt32() const {
    return typeTable.at(PTY_i32);
  }

  MIRType *GetUInt32() const {
    return typeTable.at(PTY_u32);
  }

  MIRType *GetInt64() const {
    return typeTable.at(PTY_i64);
  }

  MIRType *GetUInt64() const {
    return typeTable.at(PTY_u64);
  }

  MIRType *GetPtr() const {
    return typeTable.at(PTY_ptr);
  }

#ifdef USE_32BIT_REF
  MIRType *GetCompactPtr() const {
    return typeTable.at(PTY_u32);
  }
#else
  MIRType *GetCompactPtr() const {
    return typeTable.at(PTY_u64);
  }
#endif

  MIRType *GetRef() const {
    return typeTable.at(PTY_ref);
  }

  MIRType *GetAddr32() const {
    return typeTable.at(PTY_a32);
  }

  MIRType *GetAddr64() const {
    return typeTable.at(PTY_a64);
  }

  MIRType *GetVoid() const {
    return typeTable.at(PTY_void);
  }

  MIRType *GetDynundef() const {
    return typeTable.at(PTY_dynundef);
  }

#ifdef DYNAMICLANG
  MIRType *GetDynany() const {
    return typeTable.at(PTY_dynany);
  }

  MIRType *GetDyni32() const {
    return typeTable.at(PTY_dyni32);
  }

  MIRType *GetDynf64() const {
    return typeTable.at(PTY_dynf64);
  }

  MIRType *GetDynf32() const {
    return typeTable.at(PTY_dynf32);
  }

  MIRType *GetDynstr() const {
    return typeTable.at(PTY_dynstr);
  }

  MIRType *GetDynobj() const {
    return typeTable.at(PTY_dynobj);
  }

  MIRType *GetDynbool() const {
    return typeTable.at(PTY_dynbool);
  }
#endif

  MIRType *GetUnknown() const {
    return typeTable.at(PTY_unknown);
  }

  // Get or Create derived types.
  MIRType *GetOrCreatePointerType(TyIdx pointedTyIdx, PrimType pty = PTY_ptr);
  MIRType *GetOrCreatePointerType(const MIRType *pointTo, PrimType pty = PTY_ptr);
  MIRType *GetPointedTypeIfApplicable(MIRType *type) const;
  MIRType *GetVoidPtr() const {
    CHECK_FATAL(voidPtrType, "voidPtrType should not be null");
    return voidPtrType;
  }

  MIRArrayType *GetOrCreateArrayType(const MIRType *elem, uint8 dim, const uint32 *sizeArray);
  MIRArrayType *GetOrCreateArrayType(const MIRType *elem, uint32 size);  // For one dimention array
  MIRType *GetOrCreateFarrayType(const MIRType *elem);
  MIRType *GetOrCreateJarrayType(const MIRType *elem);
  MIRType *GetOrCreateFunctionType(MIRModule *module, TyIdx, std::vector<TyIdx> &, std::vector<TypeAttrs> &, bool isVarg = false,
                                   bool isCreate = false);

  MIRType *CreateStructType(const char *name, const FieldVector &fields, const FieldVector &prntFields,
                            MIRModule *module) {
    return GetOrCreateStructOrUnion(name, fields, prntFields, module, false, true);
  }

  MIRType *GetOrCreateStructType(const char *name, const FieldVector &fields, const FieldVector &prntFields,
                                 MIRModule *module) {
    return GetOrCreateStructOrUnion(name, fields, prntFields, module, false, false);
  }

  MIRType *GetOrCreateUnionType(const char *name, const FieldVector &fields, const FieldVector &parentFields,
                                MIRModule *module) {
    return GetOrCreateStructOrUnion(name, fields, parentFields, module, true, false);
  }

  MIRType *GetOrCreateClassType(const char *name, MIRModule *module) {
    return GetOrCreateClassOrInterface(name, module, true, true);
  }

  MIRType *GetClassType(const char *name, MIRModule *module) {
    return GetOrCreateClassOrInterface(name, module, false, true);
  }

  MIRType *GetOrCreateInterfaceType(const char *name, MIRModule *module) {
    return GetOrCreateClassOrInterface(name, module, true, false);
  }

  MIRType *GetInterfaceType(const char *name, MIRModule *module) {
    return GetOrCreateClassOrInterface(name, module, false, false);
  }

  void PushIntoFieldVector(FieldVector *fields, const char *name, MIRType* type);

  // For compiler-generated metadata struct
  TyIdx CreatePlainStructType(MIRStructType *metaType, const char *name, MIRModule *module);
  void AddFieldToStructType(MIRStructType *structType, const char *fieldName, MIRType *fieldType);

 private:
  MIRType *GetOrCreateStructOrUnion(const char *name, const FieldVector &fields, const FieldVector &prntFields,
                             MIRModule *module, bool isUnion, bool isCreate);
  MIRType *GetOrCreateClassOrInterface(const char *name, MIRModule *module, bool isCreate, bool forClass);
};

class StrPtrHash {
 public:
  size_t operator()(const std::string *str) const {
    return std::hash<std::string>{}(*str);
  }

  size_t operator()(const std::u16string *str) const {
    return std::hash<std::u16string>{}(*str);
  }
};

class StrPtrEqual {
 public:
  bool operator()(const std::string *str1, const std::string *str2) const {
    return *str1 == *str2;
  }

  bool operator()(const std::u16string *str1, const std::u16string *str2) const {
    return *str1 == *str2;
  }
};

// T can be std::string or std::u16string
// U can be GStrIdx, UStrIdx_t, or u16StrIdx_t
template <typename T, typename U>
class StringTable {
 public:
  std::vector<const T*> stringTable;  // index is uint32
  std::unordered_map<const T*, U, StrPtrHash, StrPtrEqual> stringTableMap;

 public:
  StringTable &operator = (const StringTable &) = delete;
  StringTable(const StringTable &) = delete;

  StringTable() {
    T *ptr = new (std::nothrow) T;
    CHECK_FATAL(ptr != nullptr, "null ptr check");
    stringTable.push_back(ptr);
  }

  virtual ~StringTable() {
    stringTableMap.clear();
    for (auto it : stringTable) {
      delete it;
    }
  }

  U GetStrIdxFromName(const T &str) const {
    auto it = stringTableMap.find(&str);
    if (it == stringTableMap.end()) {
      return U(0);
    }
    return it->second;
  }

  U GetOrCreateStrIdxFromName(const T &str) {
    U strIdx = GetStrIdxFromName(str);
    if (strIdx == 0) {
      strIdx.SetIdx(stringTable.size());
      T *newStr = new (std::nothrow) T(str);
      CHECK_FATAL(newStr != nullptr, "null ptr check");
      stringTable.push_back(newStr);
      stringTableMap[newStr] = strIdx;
    }
    return strIdx;
  }

  size_t StringTableSize(void) const {
    return stringTable.size();
  }

  const T &GetStringFromStrIdx(U strIdx) const {
    ASSERT(strIdx.GetIdx() < stringTable.size(), "array index out of range");
    return *stringTable[strIdx.GetIdx()];
  }

  const T &GetStringFromStrIdx(uint32 idx) const {
    ASSERT(idx < stringTable.size(), "array index out of range");
    return *stringTable[idx];
  }
};

class FPConstTable {
 private:
  std::unordered_map<float, MIRFloatConst *> floatConstTable;     // map float const value to the table;
  std::unordered_map<double, MIRDoubleConst *> doubleConstTable;  // map double const value to the table;
  MIRFloatConst *nanFloatConst, *infFloatConst, *minusInfFloatConst, *minusZeroFloatConst;
  MIRDoubleConst *nanDoubleConst, *infDoubleConst, *minusInfDoubleConst, *minusZeroDoubleConst;

 public:
  FPConstTable(const FPConstTable &p) = default;
  FPConstTable &operator=(const FPConstTable &p) = default;
  FPConstTable();
  ~FPConstTable();
  MIRFloatConst *GetOrCreateFloatConst(float, uint32 fieldID = 0);     // get the const from floatConstTable or create a new one
  MIRDoubleConst *GetOrCreateDoubleConst(double, uint32 fieldID = 0);  // get the const from doubleConstTable or create a new one
};

// STypeNameTable is only used to store class and interface types.
// Each module maintains its own MIRTypeNameTable.
class STypeNameTable {
 public:
  std::unordered_map<GStrIdx, TyIdx, GStrIdxHash> gStrIdxToTyIdxMap;

 public:
  STypeNameTable() : gStrIdxToTyIdxMap() {}

  virtual ~STypeNameTable() {}

  TyIdx GetTyIdxFromGStrIdx(GStrIdx idx) const {
    auto it = gStrIdxToTyIdxMap.find(idx);
    if (it == gStrIdxToTyIdxMap.end()) {
      return TyIdx(0);
    }
    return it->second;
  }

  void SetGStrIdxToTyIdx(GStrIdx gStrIdx, TyIdx tyIdx) {
    gStrIdxToTyIdxMap[gStrIdx] = tyIdx;
  }

};

class FunctionTable {
 public:
  std::vector<MIRFunction *> funcTable;  // index is PUIdx
 public:
  FunctionTable() {
    funcTable.push_back(nullptr);
  }  // puIdx 0 is reserved

  virtual ~FunctionTable() {}

  MIRFunction *GetFunctionFromPuidx(PUIdx pidx) const {
    ASSERT(pidx < funcTable.size(), "");
    return funcTable.at(pidx);
  }

  // virtual ~FunctionTable(){};
};

class MIRSymbol;

class GSymbolTable {
 public:
  MIRModule *module;

 private:
  // hash table mapping string index to st index
  std::unordered_map<GStrIdx, StIdx, GStrIdxHash> strIdxToStIdxMap;
  std::vector<MIRSymbol *> symbolTable;  // map symbol idx to symbol node
 public:
  GSymbolTable();
  ~GSymbolTable();

  bool IsValidIdx(uint32 idx) const {
    return idx < symbolTable.size();
  }

  MIRSymbol *GetSymbolFromStIdx(uint32 idx, bool checkfirst = false) const {
    if (checkfirst && idx >= symbolTable.size()) {
      return nullptr;
    }
    ASSERT(IsValidIdx(idx), "symbol table index out of range");
    return symbolTable[idx];
  }

  void SetStridxStidxMap(GStrIdx strIdx, StIdx stIdx) {
    strIdxToStIdxMap[strIdx] = stIdx;
  }

  StIdx GetStIdxFromStrIdx(GStrIdx idx) const {
    auto it = strIdxToStIdxMap.find(idx);
    if (it == strIdxToStIdxMap.end()) {
      return StIdx();
    }
    return it->second;
  }

  MIRSymbol *GetSymbolFromStrIdx(GStrIdx idx, bool checkfirst = false) const {
    return GetSymbolFromStIdx(GetStIdxFromStrIdx(idx).Idx(), checkfirst);
  }

  size_t GetSymbolTableSize() const {
    return symbolTable.size();
  }

  MIRSymbol *GetSymbol(uint32 idx) const {
    return symbolTable.at(idx);
  }

  MIRSymbol *CreateSymbol(uint8 scopeID);
  bool AddToStringSymbolMap(const MIRSymbol *st);
  bool RemoveFromStringSymbolMap(const MIRSymbol *st);
  void Dump(bool isLocal, int32 indent = 0) const;
};

class MIRConst;
class ConstPool {
 protected:
  std::unordered_map<GStrIdx, MIRConst *, GStrIdxHash> constMap;
  std::set<GStrIdx> importedLiteralNames;

 public:
  std::unordered_map<std::u16string, MIRSymbol *> constU16StringPool;

  void InsertConstPool(GStrIdx strIdx, MIRConst *cst) {
    constMap.insert(std::pair<GStrIdx, MIRConst *>(strIdx, cst));
  }

  MIRConst *GetConstFromPool(GStrIdx strIdx) {
    return constMap[strIdx];
  }

  void PutLiteralNameAsImported(GStrIdx gIdx) {
    importedLiteralNames.insert(gIdx);
  }

  bool LookUpLiteralNameFromImported(GStrIdx gIdx) {
    return importedLiteralNames.find(gIdx) != importedLiteralNames.end();
  }
};

class SideEffectTable {
 protected:
  std::unordered_map<GStrIdx, uint8, GStrIdxHash> func2SeMap;

 public:
  void InsertSideEffect(GStrIdx strIdx, uint8 se) {
    func2SeMap[strIdx] = se;
  }

  bool IsNoPrivateDefEffect(GStrIdx strIdx) {
    if (func2SeMap.find(strIdx) == func2SeMap.end()) {
      return false;
    }
    return ((func2SeMap[strIdx] & 0x80) == 0x80);
  }

  bool IsNoPrivateUseEffect(GStrIdx strIdx) {
    if (func2SeMap.find(strIdx) == func2SeMap.end()) {
      return false;
    }
    return ((func2SeMap[strIdx] & 0x40) == 0x40);
  }

  bool IsIpaSeen(GStrIdx strIdx) {
    if (func2SeMap.find(strIdx) == func2SeMap.end()) {
      return false;
    }
    return ((func2SeMap[strIdx] & 0x20) == 0x20);
  }

  bool IsPure(GStrIdx strIdx) {
    if (func2SeMap.find(strIdx) == func2SeMap.end()) {
      return false;
    }
    return ((func2SeMap[strIdx] & 0x10) == 0x10);
  }

  bool IsNoUseEffect(GStrIdx strIdx) {
    if (func2SeMap.find(strIdx) == func2SeMap.end()) {
      return false;
    }
    return ((func2SeMap[strIdx] & 0x08) == 0x08);
  }

  bool IsNoDefEffect(GStrIdx strIdx) {
    if (func2SeMap.find(strIdx) == func2SeMap.end()) {
      return false;
    }
    return ((func2SeMap[strIdx] & 0x04) == 0x04);
  }

  bool IsNoRetNewlyAllocObj(GStrIdx strIdx) {
    if (func2SeMap.find(strIdx) == func2SeMap.end()) {
      return false;
    }
    return ((func2SeMap[strIdx] & 0x02) == 0x02);
  }

  bool IsNoThrowException(GStrIdx strIdx) {
    if (func2SeMap.find(strIdx) == func2SeMap.end()) {
      return false;
    }
    return ((func2SeMap[strIdx] & 0x01) == 0x01);
  }
};

class GlobalTables {
 public:
  static GlobalTables &GetGlobalTables();

  static TypeTable &GetTypeTable() {
    return globalTables.typeTable;
  }

  static StringTable<std::string, GStrIdx> &GetStrTable() {
    return globalTables.stringTable;
  }

  static StringTable<std::string, UStrIdx> &GetUStrTable() {
    return globalTables.uStrTable;
  }

  static StringTable<std::u16string, U16StrIdx> &GetU16StrTable() {
    return globalTables.u16StringTable;
  }

  static FPConstTable &GetFpConstTable() {
    return globalTables.fpConstTable;
  }

  static STypeNameTable &GetTypeNameTable() {
    return globalTables.typeNameTable;
  }

  static FunctionTable &GetFunctionTable() {
    return globalTables.functionTable;
  }

  static GSymbolTable &GetGsymTable() {
    return globalTables.gSymbolTable;
  }

  static ConstPool &GetConstPool() {
    return globalTables.constPool;
  }

  static SideEffectTable &GetSideEffectTable() {
    return globalTables.sideEffectTable;
  }

 private:
  TypeTable typeTable;
  StringTable<std::string, GStrIdx> stringTable;
  StringTable<std::string, UStrIdx> uStrTable;
  StringTable<std::u16string, U16StrIdx> u16StringTable;
  FPConstTable fpConstTable;
  STypeNameTable typeNameTable;
  FunctionTable functionTable;
  GSymbolTable gSymbolTable;
  ConstPool constPool;
  SideEffectTable sideEffectTable;

  static GlobalTables globalTables;
  GlobalTables() = default;
  virtual ~GlobalTables() = default;
};

}  // namespace maple
#endif  // GLOBALTABLES_H
