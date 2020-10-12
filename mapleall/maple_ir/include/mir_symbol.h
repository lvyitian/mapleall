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

#ifndef MAPLE_IR_INCLUDE_MIR_SYMBOL_H
#define MAPLE_IR_INCLUDE_MIR_SYMBOL_H
#include "mir_const.h"
#include "mir_preg.h"
#include <sstream>

constexpr int kScopeLocal = 2;   // the default scope level for function variables
constexpr int kScopeGlobal = 1;  // the scope level for global variables

namespace maple {
enum MIRSymKind {
  kStInvalid,
  kStVar,
  kStFunc,
  kStConst,
  kStJavaClass,
  kStJavaInterface,
  kStPreg
};

enum MIRStorageClass : uint8 {
  kScInvalid,
  kScAuto,
  kScAliased,
  kScFormal,
  kScExtern,
  kScGlobal,
  kScPstatic,  // PU-static
  kScFstatic,  // file-static
  kScText,
  kScTypeInfo,      // used for eh type st
  kScTypeInfoName,  // used for eh type st name
  kScTypeCxxAbi,    // used for eh inherited from c++ __cxxabiv1
  kScEHRegionSupp,  // used for tables that control C++ exception handling
  kScUnused
};

// to represent a single symbol
class MIRSymbol {
 public:
  union SymbolType {  // a symbol can either be a const or a function or a preg which currently used for formal
    MIRConst *konst;
    MIRFunction *mirFunc;
    MIRPreg *preg;  // the MIRSymKind must be kStPreg
  };

  // Please keep order of the fields, avoid paddings.
  TyIdx tyIdx;
  TyIdx inferredTyidx;
  MIRStorageClass storageClass : 8;
  MIRSymKind sKind : 5;
  uint32 isTmp : 1;
  uint32 needForwDecl : 1;  // addrof of this symbol used in initialization, NOT serialized
  uint32 wpofakeParm : 1;    // fake symbol introduced in wpo phase for a parameter, NOT serialized
  uint32 wpofakeRet : 1;     // fake symbol introduced in wpo phase for return value, NOT serialized
  uint32 isDeleted : 1;      // tell if it is deleted, NOT serialized
  uint32 instrumented : 1;    // a local ref pointer instrumented by RC opt, NOT serialized
  uint32 isImported : 1;
  uint32 appearsincode : 1;  // only used for kStFunc
  uint32 istmpunused : 1;  // when parse the mplt_inline file, mark all the new symbol as tmpunused
  StIdx stIdx;
  TypeAttrs typeAttrs;
  GStrIdx nameStrIdx;
  SymbolType value;

  static GStrIdx reflectClassNameIdx;
  static GStrIdx reflectMethodNameIdx;
  static GStrIdx reflectFieldNameIdx;

 public:
  MIRSymbol()
    : tyIdx(0),
      inferredTyidx(kInitTyIdx),
      storageClass(kScInvalid),
      sKind(kStInvalid),
      isTmp(0),
      needForwDecl(0),
      wpofakeParm(0),
      wpofakeRet(0),
      isDeleted(0),
      instrumented(0),
      isImported(0),
      appearsincode(0),
      istmpunused(0),
      stIdx(0, 0),
      nameStrIdx(0),
      value({ nullptr }) {}

  MIRSymbol(uint32 idx, uint8 scp)
    : tyIdx(0),
      inferredTyidx(kInitTyIdx),
      storageClass(kScInvalid),
      sKind(kStInvalid),
      isTmp(0),
      needForwDecl(0),
      wpofakeParm(0),
      wpofakeRet(0),
      isDeleted(0),
      instrumented(0),
      isImported(0),
      appearsincode(0),
      istmpunused(0),
      stIdx(scp, idx),
      nameStrIdx(0),
      value({ nullptr }) {}

  ~MIRSymbol() {}

  void SetTyIdx(TyIdx tyIdx) {
    this->tyIdx = tyIdx;
  }

  TyIdx GetTyIdx() const {
    return tyIdx;
  }

  StIdx GetStIdx() const {
    return stIdx;
  }

  uint32 GetScopeIdx() const {
    return stIdx.Scope();
  }

  uint32 GetStIndex() const {
    return stIdx.Idx();
  }

  bool IsLocal() const {
    return stIdx.Islocal();
  }

  bool IsGlobal() const {
    return stIdx.IsGlobal();
  }

  TypeAttrs GetAttrs() const {
    return typeAttrs;
  }

  void SetAttrs(TypeAttrs attr) {
    typeAttrs = attr;
  }

  // AddAttrs adds more attributes instead of overrides the current one
  void AddAttrs(TypeAttrs attr) {
    typeAttrs.attrFlag |= attr.attrFlag;
  }

  bool GetAttr(AttrKind x) const {
    return typeAttrs.GetAttr(x);
  }

  void SetAttr(AttrKind x) {
    typeAttrs.SetAttr(x);
  }

  void ResetAttr(AttrKind x) {
    typeAttrs.ResetAttr(x);
  }

  bool IsVolatile() const {
    return typeAttrs.GetAttr(ATTR_volatile);
  }

  bool IsStatic() const {
    return typeAttrs.GetAttr(ATTR_static);
  }

  bool IsFinal() const {
    return typeAttrs.GetAttr(ATTR_final) || typeAttrs.GetAttr(ATTR_readonly);
  }

  bool IsWeak() const {
    return typeAttrs.GetAttr(ATTR_weak);
  }

  bool IsPrivate() const {
    return typeAttrs.GetAttr(ATTR_private);
  }

  bool IsRefType() const {
    return typeAttrs.GetAttr(ATTR_localrefvar);
  }

  void SetNameStridx(GStrIdx strIdx) {
    nameStrIdx = strIdx;
  }

  GStrIdx GetNameStridx() const {
    return nameStrIdx;
  }

  MIRStorageClass GetStorageClass() const {
    return storageClass;
  }

  void SetStorageClass(MIRStorageClass cl) {
    storageClass = cl;
  }

  bool IsReadOnly() const {
    return (kScFstatic == storageClass && kStConst == sKind);
  }

  bool IsConst() const {
    return sKind == kStConst || (sKind == kStVar && value.konst != nullptr);
  }

  MIRType *GetType() const;

  const std::string &GetName() const;

  MIRConst *GetConst() const {
    ASSERT((sKind == kStConst || sKind == kStVar), "must be const symbol");
    return value.konst;
  }

  void SetConst(MIRConst *mirconst) {
    ASSERT((sKind == kStConst || sKind == kStVar), "must be const symbol");
    value.konst = mirconst;
  }

  void SetIsDeleted() {
    isDeleted = 1;
  }

  void ResetIsDeleted() {
    isDeleted = 0;
  }

  bool IsDeleted() {
    return isDeleted == 1;
  }

  bool IsPreg() {
    return sKind == kStPreg;
  }

  bool IsJavaClassInterface() const {
    return sKind == kStJavaClass || sKind == kStJavaInterface;
  }

  MIRPreg *GetPreg() {
    CHECK_FATAL(IsPreg(), "must be Preg");
    return value.preg;
  }

  bool CanBeIgnored() {
    return isDeleted == 1 || wpofakeParm == 1 || wpofakeRet;
  }

  void SetLocalRefVar() {
    SetAttr(ATTR_localrefvar);
  }

  void ResetLocalRefVar() {
    ResetAttr(ATTR_localrefvar);
  }

  MIRFunction *GetFunction() const {
    ASSERT(sKind == kStFunc, "must be function symbol");
    return value.mirFunc;
  }

  void SetFunction(MIRFunction *func) {
    ASSERT(sKind == kStFunc, "must be function symbol");
    value.mirFunc = func;
  }

  bool IsEhIndex() {
    return GetName().compare("__eh_index__") == 0;  // TODO: seek a better way to represent eh index
  }

  bool HasAddrOfValues();
  bool IsLiteral() const;
  bool IsLiteralPtr() const;
  bool PointsToConstString() const;
  bool IsConstString() const;
  bool IsClassInitBridge();
  bool IsReflectionStrtab();
  bool IsReflectionHashTabBucket();
  bool IsReflectionInfo();
  bool IsReflectionFieldInfo();
  bool IsReflectionFieldInfoRO();
  bool IsReflectionFieldInfoCompact() const;
  bool IsReflectionSuperclassInfo();
  bool IsReflectionClassInfo();
  bool IsReflectionArrayClassInfo() const;
  bool IsReflectionClassInfoPtr() const;
  bool IsReflectionClassInfoRO();
  bool IsItabConflictInfo();
  bool IsReflectionPrimitiveClassInfo();
  bool IsReflectionMethodInfo();
  bool IsReflectionMethodInfoRO();
  bool IsReflectionMethodInfoCompact() const;
  bool IsRegJNITab();
  bool IsRegJNIFuncTab();
  bool IsMuidTab() const;
  bool IsCodeLayoutInfo() const;
  std::string GetMuidTabName();
  bool IsMuidFuncDefTab() const;
  bool IsMuidFuncInfTab() const;
  bool IsMuidFuncUndefTab() const;
  bool IsMuidDataDefTab() const;
  bool IsMuidDataUndefTab() const;
  bool IsMuidFuncDefMuidTab() const;
  bool IsMuidFuncUndefMuidTab() const;
  bool IsMuidDataDefMuidTab() const;
  bool IsMuidDataUndefMuidTab() const;
  bool IsMuidRangeTab() const;
  bool IsForcedGlobalFunc() const;
  bool IsForcedGlobalClassinfo() const;
  bool IsForcedGlobalStaticfield() const;
  bool IsGctibSym();
  bool IsPrimordialObject();
  bool IgnoreRC();
  void Dump(bool isLocal, int32 indent, bool suppressinit = false, const MIRSymbolTable *localsymtab = nullptr) const;
  void DumpAsLiteralVar(int32 indent);
  bool operator==(const MIRSymbol &msym) const {
    return nameStrIdx == msym.nameStrIdx;
  }

  bool operator!=(const MIRSymbol &msym) const {
    return nameStrIdx != msym.nameStrIdx;
  }

  bool operator<(const MIRSymbol &msym) const {
    return nameStrIdx < msym.nameStrIdx;
  }
};

class MIRSymbolTable {
 private:
  MapleAllocator *mAllocator;
  // hash table mapping string index to st index
  MapleMap<GStrIdx, StIdx> strIdxToStIdxMap;
  // map symbol idx to symbol node
  MapleVector<MIRSymbol *> symbolTable;

 public:
  MIRSymbolTable(MapleAllocator *allocator)
    : mAllocator(allocator),
      strIdxToStIdxMap(mAllocator->Adapter()),
      symbolTable(mAllocator->Adapter()) {
    symbolTable.push_back((MIRSymbol *)nullptr);
  }

  ~MIRSymbolTable() {}

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

  MIRSymbol *CreateSymbol(uint8 scopeID) {
    MIRSymbol *st = mAllocator->mp->New<MIRSymbol>(symbolTable.size(), scopeID);
    symbolTable.push_back(st);
    return st;
  }

  void PushNullSymbol() {
    symbolTable.push_back(nullptr);
  }

  // add sym from other symbol table, happens in inline
  bool AddStOutside(MIRSymbol *sym) {
    sym->stIdx = StIdx(sym->GetScopeIdx(), symbolTable.size());
    symbolTable.push_back(sym);
    return AddToStringSymbolMap(sym);
  }

  bool AddToStringSymbolMap(const MIRSymbol *st) {
    GStrIdx strIdx = st->GetNameStridx();
    if (strIdxToStIdxMap.find(strIdx) != strIdxToStIdxMap.end()) {
      return false;
    }
    strIdxToStIdxMap[strIdx] = st->GetStIdx();
    return true;
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

  void Dump(bool isLocal, int32 indent = 0, bool printdeleted = false) const;

  size_t GetSymbolTableSize() const {
    return symbolTable.size();
  }
};

class MIRLabelTable {
 public:
  MIRLabelTable(MapleAllocator *allocator)
    : mAllocator(allocator),
      strIdxToLabIdxMap(std::less<GStrIdx>(), mAllocator->Adapter()),
      labelTable(mAllocator->Adapter()),
      addrTakenLabels(mAllocator->Adapter()) {
    labelTable.push_back(GStrIdx(kDummyLabel));  // push dummy label index 0
  }

  ~MIRLabelTable() {}

  LabelIdx CreateLabel() {
    LabelIdx labidx = labelTable.size();
    labelTable.push_back(GStrIdx(0));  // insert dummy global string index for anonymous label
    return labidx;
  }

  LabelIdx CreateLabelWithPrefix(char c);

  LabelIdx AddLabel(GStrIdx nameIdx) {
    LabelIdx labidx = labelTable.size();
    labelTable.push_back(nameIdx);
    strIdxToLabIdxMap[nameIdx] = labidx;
    return labidx;
  }

  bool AddToStringLabelMap(LabelIdx lidx);
  size_t GetLabelTableSize() const {
    return labelTable.size();
  }

  const std::string &GetName(LabelIdx labidx) const;

  size_t Size() const {
    return labelTable.size();
  }

 public:
  static const uint32_t kDummyLabel = 0;
  MapleAllocator *mAllocator;
  MapleMap<GStrIdx, LabelIdx> strIdxToLabIdxMap;
  MapleVector<GStrIdx> labelTable;  // map label idx to label name
  MapleUnorderedSet<LabelIdx> addrTakenLabels; // those appeared in addroflabel or MIRLblConst
};

}  // namespace maple

#endif  // MAPLE_IR_INCLUDE_MIR_SYMBOL_H
