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

#ifndef MPL2MPL_INCLUDE_MUIDREPLACEMENT_H
#define MPL2MPL_INCLUDE_MUIDREPLACEMENT_H

#include "module_phase.h"
#include "phase_impl.h"
#include "muid.h"
namespace maple {

// For func def table.
#define FUNC_DEF_ADDR_INDEX 0

// For func def table.
#define DATA_DEF_ADDR_INDEX 0

// For func def info. table.
#define FUNC_DEF_SIZE_INDEX 0
#define FUNC_DEF_NAME_INDEX 1

#define RANGE_BEGIN_INDEX 0
#define RANGE_END_INDEX 1

enum RangeIdx {
  kVtab = 1,
  kItab,
  kVtabOffset,
  kFieldOffset,
  kValueOffset,
  kLocalClassInfo,
  kConststr,
  kSuperclass,
  kFieldInfo,
  kGlobalRootlist,
  kClassmetaData,
  kClassBucket,
  kJavatext,
  kJavajni,
  kJavajniFunc,
  kMaxNum
};

class MUIDReplacement : public FuncOptimizeImpl {
 public:
  explicit MUIDReplacement(MIRModule *mod, KlassHierarchy *kh, bool dump);
  ~MUIDReplacement() {}
  FuncOptimizeImpl *Clone() override {
    return new MUIDReplacement(*this);
  }

  void ProcessFunc(MIRFunction *func) override;

 private:
  bool isLibcore = false;
  MIRSymbol *funcDefTabSym = nullptr;
  MIRSymbol *funcInfTabSym = nullptr;
  MIRSymbol *funcUndefTabSym = nullptr;
  MIRSymbol *dataDefTabSym = nullptr;
  MIRSymbol *dataUndefTabSym = nullptr;
  MIRSymbol *funcDefMuidTabSym = nullptr;
  MIRSymbol *funcUndefMuidTabSym = nullptr;
  MIRSymbol *dataDefMuidTabSym = nullptr;
  MIRSymbol *dataUndefMuidTabSym = nullptr;
  MIRSymbol *funcMuidIdxTabSym = nullptr;
  MIRSymbol *rangeTabSym = nullptr;
  MIRSymbol *funcProfileTabSym = nullptr;

  std::string mplMuidStr;

  using SymIdxPair = std::pair<MIRSymbol*, uint32>;
  std::map<MUID_t, SymIdxPair> funcDefMap;
  std::map<MUID_t, SymIdxPair> dataDefMap;
  std::map<MUID_t, SymIdxPair> funcUndefMap;
  std::map<MUID_t, SymIdxPair> dataUndefMap;
  std::map<MUID_t, uint32_t> defMuidIdxMap;

  void GenTables();
  void GenFuncDefTable();
  void GenDataDefTable();
  void GenUnifiedUndefTable();
  void GenRangeTable();

  uint32_t FindIndexFromDefTable(const MIRSymbol *mirsym, bool isFunc);
  uint32_t FindIndexFromUndefTable(const MIRSymbol *mirsym, bool isFunc);
  void ReplaceAddroffuncConst(MIRConst *&entry, uint32_t fieldID, bool isVtab);
  void ReplaceFuncTable(const std::string &name);
  void ReplaceAddrofConst(MIRConst *&entry);
  void ReplaceDataTable(const std::string &name);
  void ReplaceDirectInvokeOrAddroffunc(MIRFunction *curFunc, StmtNode *stmt);
  void ReplaceDassign(MIRFunction *curFunc, DassignNode *dassignNode);
  void ReplaceDreadStmt(MIRFunction *curFunc, StmtNode *stmt);
  BaseNode *ReplaceDreadExpr(MIRFunction *curFunc, StmtNode *stmt, BaseNode *expr);
  BaseNode *ReplaceDread(MIRFunction *curFunc, StmtNode *stmt, BaseNode *opnd);
  void CollectFieldCallSite();
  void CollectDreadStmt(MIRFunction *curFunc, StmtNode *stmt);
  BaseNode *CollectDreadExpr(MIRFunction *curFunc, StmtNode *stmt, BaseNode *expr);
  void CollectDread(MIRFunction *curFunc, StmtNode *stmt, BaseNode *opnd);
  void DumpMUIDFile(bool isFunc);
  void ReplaceStmts();
  void GenGlobalRootList();
  void CollectFuncAndData();
  void CollectImplicitUndefClassinfo(StmtNode *stmt);
  void InsertFunctionProfile(MIRFunction *curFunc, int64 index);

  // The following sets are for internal uses. Sorting order does not matter here.
  std::unordered_set<MIRFunction *> funcDefSet;
  std::unordered_set<MIRFunction *> funcUndefSet;
  std::unordered_set<MIRSymbol *> dataDefSet;
  std::unordered_set<MIRSymbol *> dataUndefSet;
  inline void AddDefFunc(MIRFunction *func) {
    funcDefSet.insert(func);
  }

  inline void AddUndefFunc(MIRFunction *func) {
    funcUndefSet.insert(func);
  }

  inline void AddDefData(MIRSymbol *sym) {
    dataDefSet.insert(sym);
  }

  inline void AddUndefData(MIRSymbol *sym) {
    dataUndefSet.insert(sym);
  }

#define CLASS_PREFIX(classname) (CLASSINFO_PREFIX_STR + classname),
  const std::unordered_set<std::string> kPreloadedClassinfo = {
#include "global_symbols.def"
  };
#undef CLASS_PREFIX

#define MUID_FUNC_PTR_STR "__muid_funcptr"
#define MUID_SYM_PTR_STR "__muid_symptr"
  const std::unordered_set<std::string> kReflectionlist = {
  };
};

class DoMUIDReplacement : public ModulePhase {
 public:
  explicit DoMUIDReplacement(ModulePhaseID id) : ModulePhase(id) {}

  ~DoMUIDReplacement() = default;

  std::string PhaseName() const override {
    return "MUIDReplacement";
  }

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(MUIDReplacement);
    return nullptr;
  }
};
}  // namespace maple
#endif
