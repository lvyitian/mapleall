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

#ifndef MAPLE_IPA_INCLUDE_CLONE_H
#define MAPLE_IPA_INCLUDE_CLONE_H

#include "mir_module.h"
#include "mir_function.h"
#include "mir_builder.h"
#include "mempool.h"
#include "mempool_allocator.h"
#include "class_hierarchy.h"
#include "me_ir.h"

#include "module_phase.h"

#define FULLNAME_STR "INFO_fullname"
#define CLASSNAME_STR "INFO_classname"
#define FUNCNAME_STR "INFO_funcname"

namespace maple {
class ReplaceRetIgnored {
  maple::MapleAllocator allocator;
  std::set<std::string> _to_be_cloned_func_names;
  static constexpr const char *kVoidRetSuffix = "CLONEDignoreret";

 public:
  ReplaceRetIgnored(maple::MemPool *mp);

  virtual ~ReplaceRetIgnored() {}

  bool ShouldReplaceWithVoidFunc(const CallMeStmt *stmt, const MIRFunction *calleeFunc) const;

  std::string GenerateNewBaseName(const MIRFunction *originalFunc);
  std::string GenerateNewFullName(MIRFunction *originalFunc);
  const std::set<std::string> *GetTobeClonedFuncNames() const {
    return &_to_be_cloned_func_names;
  }

  bool IsInCloneList(const std::string &funcName) {
    return _to_be_cloned_func_names.find(funcName) != _to_be_cloned_func_names.end();
  }

  static bool IsClonedFunc(const std::string &funcName) {
    return funcName.find(kVoidRetSuffix) != std::string::npos;
  }
};

class Clone : public AnalysisResult {
 public:
  MIRModule *mirModule;
  MapleAllocator allocator;
  MIRBuilder &mirBuilder;
  ReplaceRetIgnored *replaceRetIgnored;

 private:
  KlassHierarchy *kh;

 public:
  explicit Clone(MIRModule *mod, MemPool *mp, MIRBuilder &builder, KlassHierarchy *kh)
    : AnalysisResult(mp), mirModule(mod), allocator(mp), mirBuilder(builder), kh(kh) {
    replaceRetIgnored = mp->New<ReplaceRetIgnored>(mp);
  }

  ~Clone() {}

  static MIRSymbol *CloneLocalSymbol(const MIRSymbol *oldSym, MIRFunction *newFunc);

  static void CloneSymbols(MIRFunction *newfunc, const MIRFunction *oldfunc);

  static void CloneLabels(MIRFunction *newfunc, const MIRFunction *oldfunc);

  MIRFunction *CloneFunction(MIRFunction *const originalFunction, const std::string &newBaseFuncName,
                             MIRType *returnType = nullptr);

  MIRFunction *CloneFunctionNoReturn(MIRFunction *const originalFunction);

  void DoClone();

  void CopyFuncInfo(const MIRFunction *originalFunction, MIRFunction *newFunc) const;

  void UpdateFuncInfo(MIRFunction *newFunc);

  void CloneArgument(const MIRFunction *originalFunction, ArgVector &argument) const;

  ReplaceRetIgnored *GetReplaceRetIgnored() {
    return replaceRetIgnored;
  }

  void UpdateReturnVoidIfPossible(CallMeStmt *callmestmt, MIRFunction *targetFunc);
};

class DoClone : public ModulePhase {
 public:
  DoClone(ModulePhaseID id) : ModulePhase(id) {}

  ~DoClone() {}

  AnalysisResult *Run(MIRModule *module, ModuleResultMgr *m) override;

  std::string PhaseName() const override {
    return "clone";
  }
};
}  // namespace maple
#endif
