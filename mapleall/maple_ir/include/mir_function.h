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

#ifndef MAPLE_IR_INCLUDE_MIR_FUNCTION_H
#define MAPLE_IR_INCLUDE_MIR_FUNCTION_H
#include <string>
#include "mir_module.h"
#include "mir_const.h"
#include "mir_symbol.h"
#include "mir_preg.h"
#include "intrinsics.h"
#include "global_tables.h"
#include "file_layout.h"
#include "mir_nodes.h"

#define DEBUGME true

namespace maple {

enum MIRFuncProp {
  kFuncPropNone = 0,
  kFuncPropHasCall,      // the function has call
  kFuncPropRetStruct,    // the function returns struct
  kFuncPropUserFunc,     // the function is a user func
  kFuncPropInfoPrinted,  // to avoid printing frameSize/moduleid/funcSize info more than once per function since they
                         // can only be printed at the beginning of a block
  kFuncPropNeverReturn,  // the function when called never returns
  kFuncPropHasSetjmp,    // the function contains call to setjmp
};

#define FUNCHASCALL (1U << kFuncPropHasCall)
#define FUNCRETSTRUCT (1U << kFuncPropRetStruct)
#define FUNCUSER (1U << kFuncPropUserFunc)
#define FUNCINFOPRINTED (1U << kFuncPropInfoPrinted)
#define FUNCNEVERRETURN (1U << kFuncPropNeverReturn)
#define FUNCHASSETJMP (1U << kFuncPropHasSetjmp)

// mapping src (java) variable to mpl variables to display debug info
struct MIRAliasVars {
  GStrIdx memPoolStrIdx;
  TyIdx tyIdx;
  GStrIdx sigStrIdx;
};

// describe a formal definition in a function declaration
class FormalDef {
 public:
  GStrIdx formalStrIdx = GStrIdx(0);    // used when processing the prototype
  MIRSymbol *formalSym = nullptr;       // used in the function definition
  TyIdx formalTyIdx = TyIdx();
  TypeAttrs formalAttrs = TypeAttrs();  // the formal's type attributes

  FormalDef() {};
  FormalDef(MIRSymbol *s, TyIdx tidx, TypeAttrs at) : formalSym(s), formalTyIdx(tidx), formalAttrs(at) {}
  FormalDef(GStrIdx sidx, MIRSymbol *s, TyIdx tidx, TypeAttrs at) : formalStrIdx(sidx), formalSym(s), formalTyIdx(tidx), formalAttrs(at) {}
};

class MIRFunction : public mir_func_t {
 public:
  MIRModule *module;               // the module that owns this function
  PUIdx puIdx;           // the PU index of this function
  PUIdx puIdxOrigin;     // the original puIdx when initial generation
  StIdx stIdx;           // the symbol table index of this function
  MIRFuncType *funcType;
  TyIdx inferredReturnTyIdx;  // the actual return type of of this function (may be a
  // subclass of the above). 0 means can not be inferred.
  TyIdx classTyIdx;               // class/interface type this function belongs to
  MapleVector<FormalDef> formalDefVec;  // the formals in func definition
  MapleSet<MIRSymbol *> retRefSym;
  MIRSymbolTable *symTab = nullptr;
  MIRTypeNameTable *typeNameTab = nullptr;
  MIRLabelTable *labelTab = nullptr;
  MIRPregTable *pregTab = nullptr;
  MemPool *dataMemPool;
  MapleAllocator dataMPAllocator;
  MemPool *codeMemPool;
  MapleAllocator codeMemPoolAllocator;
  MemPool *codeMemPoolTmp;
  MapleAllocator codeMemPoolTmpAllocator;
  bool useTmpMp;
  BlockNode *body;
  FuncAttrs funcAttrs;
  uint32 flag;
  uint16 hashCode;     // for methodmetadata order
  uint32 fileIndex;  // this function belongs to which file, used by VM for plugin manager
  MIRInfoVector info;
  MapleVector<bool> infoIsString;                  // tells if an entry has string value
  MapleMap<GStrIdx, MIRAliasVars> aliasVarMap;  // source code alias variables for debuginfo
  MapleMap<MIRSymbol *, uint32> stackallocVarMap;
  MapleMap<uint32, uint32> freqMap; // save bb frequency in its last_stmt.
  bool hasVlaoralloca;
  bool withLocInfo;
  uint32 lockslotnum;
  enum { kEasumInvalid = -1, kEasumNosum = 0 };
  uint32 easummary;  // Used for escape analysis.
  bool isDirty;
  uint8_t layoutType;
  uint32 callTimes;

 private:
  // to hold unmangled class and function names
  GStrIdx baseClassStrIdx;  // the string table index of base class name
  GStrIdx baseFuncStrIdx;   // the string table index of base function name
  // the string table index of base function name mangled with type info
  GStrIdx baseFuncWithTypeStrIdx;
  // funcname + types of args, no type of retv
  GStrIdx baseFuncSigStridx;
  GStrIdx signatureStrIdx;

 public:
  MIRFunction(MIRModule *mod, const StIdx sidx)
    : module(mod),
      puIdx(0),
      stIdx(sidx),
      funcType(nullptr),
      inferredReturnTyIdx(0),
      classTyIdx(0),
      formalDefVec(mod->memPoolAllocator.Adapter()),
      retRefSym(mod->memPoolAllocator.Adapter()),
      dataMemPool(mempoolctrler.NewMemPool("func data mempool")),
      dataMPAllocator(dataMemPool),
      codeMemPool(mempoolctrler.NewMemPool("func code mempool")),
      codeMemPoolAllocator(codeMemPool),
      codeMemPoolTmp(mempoolctrler.NewMemPool("func code mempool tmp")),
      codeMemPoolTmpAllocator(codeMemPoolTmp),
      useTmpMp(false),
      body(nullptr),
      flag(0),
      fileIndex(0),
      info(mod->memPoolAllocator.Adapter()),
      infoIsString(mod->memPoolAllocator.Adapter()),
      aliasVarMap(std::less<GStrIdx>(), mod->memPoolAllocator.Adapter()),
      stackallocVarMap(std::less<MIRSymbol *>(), mod->memPoolAllocator.Adapter()),
      freqMap(std::less<uint32>(), mod->memPoolAllocator.Adapter()),
      hasVlaoralloca(false),
      withLocInfo(true),
      lockslotnum(0),
      easummary(kEasumNosum),
      isDirty(false),
      callTimes(0) {
    frameSize = 0;
    upFormalSize = 0;
    moduleID = 0;
    funcSize = 0;
    puIdxOrigin = 0;
    formalWordsTypeTagged = nullptr;
    localWordsTypeTagged = nullptr;
    formalWordsRefCounted = nullptr;
    localWordsRefCounted = nullptr;
    baseFuncStrIdx = GStrIdx(0);
    baseClassStrIdx = GStrIdx(0);
    baseFuncWithTypeStrIdx = GStrIdx(0);
    baseFuncSigStridx = GStrIdx(0);
    signatureStrIdx = GStrIdx(0);
    hashCode = 0;
    layoutType = kLayoutUnused;
  }

  ~MIRFunction() {}

  void Dump(bool withoutBody = false);
  MIRSymbol *GetFuncSymbol() const {
    return GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx());
  }

  void SetBaseClassFuncNames(GStrIdx strIdx);
  void SetMemPool(MemPool *mp) {
    codeMemPool = mp;
    codeMemPoolAllocator.SetMemPool(codeMemPool);
  }

  /// update signatureStrIdx, baseFuncStrIdx, baseClassStrIdx, baseFuncWithTypeStrIdx
  /// without considering baseClassStrIdx, baseFuncStrIdx's original non-zero values
  /// \param strIdx full_name strIdx of the new function name
  void OverrideBaseClassFuncNames(GStrIdx strIdx);

  const std::string &GetName() const {
    return GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx())->GetName();
  }

  GStrIdx GetNameStridx() const {
    return GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx())->GetNameStridx();
  }

  const std::string &GetBaseClassName() const {
    return GlobalTables::GetStrTable().GetStringFromStrIdx(baseClassStrIdx);
  }

  const std::string &GetBaseFuncName() const {
    return GlobalTables::GetStrTable().GetStringFromStrIdx(baseFuncStrIdx);
  }

  const std::string &GetBaseFuncNameWithType() const {
    return GlobalTables::GetStrTable().GetStringFromStrIdx(baseFuncWithTypeStrIdx);
  }

  const std::string &GetBaseFuncSig() const {
    return GlobalTables::GetStrTable().GetStringFromStrIdx(baseFuncSigStridx);
  }

  const std::string &GetSignature() const {
    return GlobalTables::GetStrTable().GetStringFromStrIdx(signatureStrIdx);
  }

  GStrIdx GetBaseClassNameStridx() const {
    return baseClassStrIdx;
  }

  GStrIdx GetBaseFuncNameStridx() const {
    return baseFuncStrIdx;
  }

  GStrIdx GetBaseFuncNameWithTypeStridx() const {
    return baseFuncWithTypeStrIdx;
  }

  GStrIdx GetBaseFuncSigStridx() const {
    return baseFuncSigStridx;
  }

  void SetBaseClassNameStridx(GStrIdx id) {
    baseClassStrIdx.SetIdx(id.GetIdx());
  }

  void SetBaseFuncNameStridx(GStrIdx id) {
    baseFuncStrIdx.SetIdx(id.GetIdx());
  }

  void SetBaseFuncNameWithTypeStridx(GStrIdx id) {
    baseFuncWithTypeStrIdx.SetIdx(id.GetIdx());
  }

  MIRType *GetReturnType() const {
    return GlobalTables::GetTypeTable().GetTypeFromTyIdx(funcType->retTyIdx);
  }

  bool IsReturnVoid() const {
    return GetReturnType()->primType == PTY_void;
  }

  TyIdx GetReturnTyIdx() const {
    return funcType->retTyIdx;
  }

  MIRType *GetClassType() const {
    return GlobalTables::GetTypeTable().typeTable.at(classTyIdx.GetIdx());
  }

  TyIdx GetClassTyIdx() const {
    return classTyIdx;
  }

  void SetReturnTyIdx(TyIdx tyIdx) {
    funcType->retTyIdx = tyIdx;
  }

  void SetClassTyIdx(TyIdx tyIdx) {
    classTyIdx = tyIdx;
  }

  void AddArgument(MIRSymbol *st) {
    FormalDef formalDef(st, st->GetTyIdx(), st->typeAttrs);
    formalDefVec.push_back(formalDef);
  }

  LabelIdx GetOrCreateLablidxFromName(const std::string &name) {
    // fixme? this function should never be used after parsing, so move to parser?
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(name);
    LabelIdx labidx = labelTab->strIdxToLabIdxMap[strIdx];
    if (labidx == 0) {
      labidx = labelTab->CreateLabel();
      labelTab->labelTable.at(labidx) = strIdx;
      labelTab->AddToStringLabelMap(labidx);
    }
    return labidx;
  }

  const GStrIdx GetLabelStringIndex(LabelIdx lbidx) {
    CHECK(lbidx < labelTab->labelTable.size(), "index out of range in GetLabelStringIndex");
    return labelTab->labelTable[lbidx];
  }

  const std::string &GetLabelName(LabelIdx lbidx) {
    GStrIdx strIdx = GetLabelStringIndex(lbidx);
    return GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
  }

  MIRSymbol *GetLocalOrGlobalSymbol(const StIdx &idx, bool checkfirst = false) const;
  FuncAttrs GetAttrs() const {
    return funcAttrs;
  }

  void SetAttrs(FuncAttrs attr) {
    funcAttrs = attr;
  }

  bool GetAttr(FuncAttrKind x) const {
    return funcAttrs.GetAttr(x);
  }

  void SetAttr(FuncAttrKind x) {
    funcAttrs.SetAttr(x);
  }

  bool IsVarargs() const {
    return funcAttrs.GetAttr(FUNCATTR_varargs);
  }

  bool IsWeak() const {
    return funcAttrs.GetAttr(FUNCATTR_weak);
  }

  bool IsStatic() const {
    return funcAttrs.GetAttr(FUNCATTR_static);
  }

  bool IsNative() const {
    return funcAttrs.GetAttr(FUNCATTR_native);
  }

  bool IsFinal() const {
    return funcAttrs.GetAttr(FUNCATTR_final);
  }

  bool IsAbstract() const {
    return funcAttrs.GetAttr(FUNCATTR_abstract);
  }

  bool IsPublic() const {
    return funcAttrs.GetAttr(FUNCATTR_public);
  }

  bool IsPrivate() const {
    return funcAttrs.GetAttr(FUNCATTR_private);
  }

  bool IsProtected() const {
    return funcAttrs.GetAttr(FUNCATTR_protected);
  }

  bool IsPackagePrivate() const {
    return !IsPublic() && !IsPrivate() && !IsProtected();
  }

  bool IsConstructor() const {
    return funcAttrs.GetAttr(FUNCATTR_constructor);
  }

  bool IsLocal() const {
    return funcAttrs.GetAttr(FUNCATTR_local);
  }

  bool IsNoDefEffect() const {
    return funcAttrs.GetAttr(FUNCATTR_nodefeffect);
  }

  bool IsNoThrowException() const {
    return funcAttrs.GetAttr(FUNCATTR_nothrow_exception);
  }

  bool IsNoPrivateDefEffect() const {
    return funcAttrs.GetAttr(FUNCATTR_noprivate_defeffect);
  }

  bool IsIpaSeen() const {
    return funcAttrs.GetAttr(FUNCATTR_ipaseen);
  }

  bool IsPure() const {
    return funcAttrs.GetAttr(FUNCATTR_pure);
  }

  void SetVarargs() {
    funcAttrs.SetAttr(FUNCATTR_varargs);
  }

  void SetNoDefEffect() {
    funcAttrs.SetAttr(FUNCATTR_nodefeffect);
  }

  void SetNoThrowException() {
    funcAttrs.SetAttr(FUNCATTR_nothrow_exception);
  }

  void SetNoPrivateDefEffect() {
    funcAttrs.SetAttr(FUNCATTR_noprivate_defeffect);
  }

  void SetIpaSeen() {
    funcAttrs.SetAttr(FUNCATTR_ipaseen);
  }

  void SetPure() {
    funcAttrs.SetAttr(FUNCATTR_pure);
  }

  void UnsetNoDefEffect() {
    funcAttrs.SetAttr(FUNCATTR_nodefeffect, true);
  }

  void UnsetNoThrowException() {
    funcAttrs.SetAttr(FUNCATTR_nothrow_exception, true);
  }

  void UnsetPure() {
    funcAttrs.SetAttr(FUNCATTR_pure, true);
  }

  void UnsetNoPrivateDefEffect() {
    funcAttrs.SetAttr(FUNCATTR_noprivate_defeffect, true);
  }

  bool HasCall() const {
    return flag & FUNCHASCALL;
  }

  void SetHasCall() {
    flag |= FUNCHASCALL;
  }

  bool IsReturnStruct() const {
    return flag & FUNCRETSTRUCT;
  }

  void SetReturnStruct() {
    flag |= FUNCRETSTRUCT;
  }

  bool IsUserFunc() const {
    return flag & FUNCUSER;
  }

  void SetUserFunc() {
    flag |= FUNCUSER;
  }

  bool IsInfoPrinted() const {
    return flag & FUNCINFOPRINTED;
  }

  void SetInfoPrinted() {
    flag |= FUNCINFOPRINTED;
  }

  void ResetInfoPrinted() {
    flag &= ~FUNCINFOPRINTED;
  }

  void SetNoReturn() {
    flag |= FUNCNEVERRETURN;
  }

  bool NeverReturns() const {
    return flag & FUNCNEVERRETURN;
  }

  void SetHasSetjmp() {
    flag |= FUNCHASSETJMP;
  }

  bool HasSetjmp() const {
    return flag & FUNCHASSETJMP;
  }

  void SetReturnStruct(MIRType *retType) {
    switch (retType->GetKind()) {
      case kTypeUnion:
      case kTypeStruct:
      case kTypeStructIncomplete:
      case kTypeClass:
      case kTypeClassIncomplete:
      case kTypeInterface:
      case kTypeInterfaceIncomplete:
        flag |= FUNCRETSTRUCT;
        break;
      default:;
    }
  }

  bool IsEmpty() const;
  bool IsClinit() const;
  void Emit(const std::string &outfileName, bool isFirstFunction);
  uint32 GetInfo(GStrIdx strIdx) const;
  uint32 GetInfo(const std::string &string) const;
  bool IsAFormal(const MIRSymbol *st) const {
    for (MapleVector<FormalDef>::const_iterator it = formalDefVec.begin(); it != formalDefVec.end(); it++) {
      if (st == it->formalSym) {
        return true;
      }
    }
    return false;
  }

  uint32 GetFormalIndex(const MIRSymbol *st) const {
    for (uint32 i = 0; i < formalDefVec.size(); i++)
      if (formalDefVec[i].formalSym == st) {
        return i;
      }

    return 0xffffffff;
  }

  // tell whether this function is a Java method
  bool IsJava() const {
    return classTyIdx.GetIdx();
  }

  bool IsC() const {
    return module->srcLang == kSrcLangC;
  }

  MIRType *GetNodeType(base_node_t *node);
#ifdef DEBUGME
  void SetUpGDBEnv();
  void ResetGDBEnv();
#endif
  void ReleaseMemory() {
    mempoolctrler.DeleteMemPool(codeMemPoolTmp);
    codeMemPoolTmp = mempoolctrler.NewMemPool("func code mempool tmp");
  }

  inline MemPool *GetCodeMp() {
    if (useTmpMp) {
      return codeMemPoolTmp;
    } else {
      return codeMemPool;
    }
  }

  inline MapleAllocator *GetCodeMpAllocator() {
    if (useTmpMp) {
      return &codeMemPoolTmpAllocator;
    } else {
      return &codeMemPoolAllocator;
    }
  }

  uint8 GetLayoutType() const {
    return layoutType;
  }
  void SetLayoutType(uint8 type) {
    layoutType = type;
  }

  void NewBody();
};

}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_MIR_FUNCTION_H
