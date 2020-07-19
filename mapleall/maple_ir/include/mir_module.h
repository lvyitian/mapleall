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

#ifndef MAPLE_IR_INCLUDE_MIR_MODULE_H
#define MAPLE_IR_INCLUDE_MIR_MODULE_H
#include "types_def.h"
#include "prim_types.h"
#include "intrinsics.h"
#include "opcodes.h"
#include "cmpl.h"
#include "mpl_logging.h"
#include "muid.h"
#include "profile.h"
//#include "../../maple_ipa/include/callgraph.h"
#if MIR_FEATURE_FULL
#include <string>
#include <unordered_set>
#include "mempool.h"
#include "mempool_allocator.h"
#include "maple_string.h"
#endif  // MIR_FEATURE_FULL

namespace maple {
class CallInfo;  // circular dependency exists, no other choice
class MIRModule;  // circular dependency exists, no other choice
class MIRBuilder;  // circular dependency exists, no other choice
using MIRModulePtr = MIRModule*;
using MIRBuilderPtr = MIRBuilder*;

class MIRType;
class MIRFunction;
class MIRSymbol;
class MIRSymbolTable;
class MIRTypeNameTable;
class MIRFloatConst;
class MIRDoubleConst;
class BinaryMplt;
using MIRInfoPair = std::pair<GStrIdx, uint32>;
using MIRInfoVector = MapleVector<MIRInfoPair>;
using MIRDataPair = std::pair<GStrIdx, std::vector<uint8>>;
using MIRDataVector = MapleVector<MIRDataPair>;
constexpr int kMaxEncodedValueLen = 10;
struct EncodedValue {
  uint8 encodedValue[kMaxEncodedValueLen] = { 0 };
};

class MIRTypeNameTable {
 public:
  MapleAllocator *mAllocator;
  MapleMap<GStrIdx, TyIdx> gStrIdxToTyIdxMap;

  MIRTypeNameTable(MapleAllocator *allocator)
      : mAllocator(allocator), gStrIdxToTyIdxMap(std::less<GStrIdx>(), mAllocator->Adapter()) {}

  ~MIRTypeNameTable() = default;

  TyIdx GetTyIdxFromGStrIdx(GStrIdx idx) {
    auto it = gStrIdxToTyIdxMap.find(idx);
    if (it == gStrIdxToTyIdxMap.end()) {
      return TyIdx(0);
    }
    return it->second;
  }

  void SetGStrIdxToTyIdx(GStrIdx gStrIdx, TyIdx tyIdx) {
    gStrIdxToTyIdxMap[gStrIdx] = tyIdx;
  }

  size_t Size() const {
    return gStrIdxToTyIdxMap.size();
  }
};

enum MIRModuleHint {
  kReserved = 0x00,    // reserved
  kRcAnalyzed = 0x01,  // module analyzerc by analyzerc
  kRcLowered = 0x02,   // module lowered by rcLowering
};

class MIRModule : public mir_module_t {
 public:
  MemPool *memPool;
  MapleAllocator memPoolAllocator;
  MapleVector<MIRFunction *> functionList;     // function table in the order of the appearance of function bodies; it
                                                 // excludes prototype-only functions
  MapleVector<MIRFunction *> compilationList;  // functions in the order of to be compiled.
  MapleVector<std::string> importedMplt;
  MIRTypeNameTable *typeNameTab;
  MapleVector<GStrIdx> typeDefOrder;
  MapleSet<TyIdx> externStructTypeSet;
  MapleSet<StIdx> symbolSet;  // the global symbols relevant to module
  MapleVector<StIdx> symbolDefOrder;
  Profile profile; //profile used for PGO optimization
  bool someSymbolNeedForwDecl;  // some symbols' addressses used in initialization
  uint32 hints;                    // 32-bits to flag hint across compilation pipeline
  std::ostream &out;

  MIRBuilder *mirBuilder;
  std::string entryFuncName;  // name of the entry function
  std::string fileName;
  MUID_t mplMd5;

  TyIdx throwableTyidx;  // a special type that is the base of java exception type. only used for java

  bool withProfileInfo;

  ////   for cg in mplt
  BinaryMplt *binMplt;
  typedef std::pair<CallInfo *, PUIdx> CallSite;
  // std::map<PUIdx, std::set<PUIdx> > method2TargetMap;
  std::map<PUIdx, std::vector<CallSite>> method2TargetMap;
  std::map<PUIdx, std::unordered_set<uint64>> method2TargetHash;

 private:
  MIRFunction *entryFunc;
  uint32 floatNum;

 public:
  MIRInfoVector fileInfo;               // store info provided under fileInfo keyword
  MapleVector<bool> fileInfoIsString;  // tells if an entry has string value
  MIRDataVector fileData;
  MIRInfoVector srcFileInfo;  // store info provided under srcFileInfo keyword

 public:
  MapleVector<GStrIdx> importFiles;
  MapleVector<GStrIdx> importPaths;
  MapleSet<uint32> classList;
  MapleVector<MIRFunction *> optimizedFuncs;
  MapleSet<uint32> rcNotNeedingLock;  // set of stmtID's which does incref/decref to an object not escaping
  // record all the fields that are initialized in the constructor. module scope,
  // if puIdx doesn't appear in this map, it writes to all field id
  // if puIdx appears in the map, but it's corresponding MapleSet is nullptr, it writes nothing fieldID
  // if puIdx appears in the map, and the value of first corresponding MapleSet is 0, the puIdx appears in this module
  // and writes to all field id otherwise, it writes the field ids in MapleSet
  MapleMap<PUIdx, MapleSet<FieldID> *> puIdxFieldInitializedMap;
  MapleSet<uint32_t> inliningGlobals;  // global symbols accessed, used for inlining

  void AddClass(TyIdx t);
  void RemoveClass(TyIdx t);

 public:
  MIRModule(MIRModule &p) = default;
  MIRModule(const char *fn = "");
  ~MIRModule() {
    mempoolctrler.DeleteMemPool(memPool);
  }

  MapleAllocator &GetMPAllocator() {
    return memPoolAllocator;
  }

  MIRFunction *CurFunction(void) const;
  MemPool *CurFuncCodeMemPool(void) const;
  MapleAllocator *CurFuncCodeMemPoolAllocator(void) const;
  void AddExternStructType(TyIdx tyIdx);
  void AddExternStructType(const MIRType *t);
  void AddSymbol(StIdx stIdx);
  void AddSymbol(const MIRSymbol *s);
  void AddFunction(MIRFunction *pf) {
    functionList.push_back(pf);
    compilationList.push_back(pf);
  }

  MIRFloatConst *GetOrCreateFloatConst(float);     // get the const from floatConstTable or create a new one
  MIRDoubleConst *GetOrCreateDoubleConst(double);  // get the const from doubleConstTable or create a new one
  void InitInfo();
  void Dumpinfo();
  void DumpGlobals(bool emitStructureType = true) const;
  void Dump(bool emitStructureType = true, std::unordered_set<std::string> *dumpFuncSet = nullptr);
  void DumpToFile(const std::string &fileName, bool emitStructureType = true);
  void DumpInlineCandidateToFile(const std::string &fileName);
  void DumpToHeaderFile(bool binaryMplt, std::string outputName = "");
  const std::string &GetFilenameFromFilenum(uint32 fileNum);

 private:
  void DumpTypeTreeToCxxHeaderFile(MIRType *ty, std::unordered_set<MIRType *> &dumpedClasses);

 public:
  void DumpToCxxHeaderFile(std::set<MapleString> &leafClasses, const char *pathToOutf);
  void DumpClassToFile(const char *path);
  void DumpFunctionList(std::unordered_set<std::string> *dumpFuncSet);
  void DumpGlobalArraySymbol();
  void Emit(const std::string &outFileName) const;

  uint32 GetAndIncFloatNum() {
    return floatNum++;
  }

  MIRFunction *GetLastFunction() const {
    return functionList.back();
  }

  void SetEntryFunction(MIRFunction *f) {
    entryFunc = f;
  }

  MIRFunction *GetEntryFunction() const {
    return entryFunc;
  }

  MIRFunction *FindEntryFunction();
  uint32 GetFileinfo(GStrIdx strIdx) const;
  void OutputAsciiMpl(const char *phaseName, const char *suffix,
                      std::unordered_set<std::string> *dumpFuncSet = nullptr,
                      bool emitStructureType = true, bool binaryform = false);
  void OutputFunctionListAsciiMpl(const char *phaseName);
  const std::string &GetFileName() const {
    return fileName;
  }

  std::string GetFileNameAsPostfix() const;
  void SetFileName(const std::string &name) {
    fileName = name;
  }

  void SetMplMd5(MUID_t md5number) {
    mplMd5 = md5number;
  }

  bool IsJavaModule() const {
    return srcLang == kSrcLangJava;
  }

  bool IsCModule() const {
    return srcLang == kSrcLangC || srcLang == kSrcLangCPlusPlus;
  }

  void ReleaseCurFuncMpTmp();
  inline void SetUseFuncCodeMpTmp() {
    useFuncCodeMpTmp = true;
  }

  inline void ResetUseFuncCodeMpTmp() {
    useFuncCodeMpTmp = false;
  }

 private:
  bool useFuncCodeMpTmp;
};

}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_MIR_MODULE_H
