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

#ifndef MPL2MPL_INCLUDE_GENCHECKCAST_H
#define MPL2MPL_INCLUDE_GENCHECKCAST_H

#include "class_hierarchy.h"
#include "module_phase.h"
#include "phase_impl.h"

namespace maple {

class CheckCastGeneration : public FuncOptimizeImpl {
 private:
  MIRType *mPtrObjType = nullptr;
  MIRFunction *mThrowCastException = nullptr;
  MIRFunction *mCheckCastingNoArray = nullptr;
  MIRFunction *mCheckCastingArray = nullptr;
  static const std::unordered_set<std::string> kFuncWithoutCastCheck;

 public:
  explicit CheckCastGeneration(MIRModule *mod, KlassHierarchy *kh, bool dump);
  ~CheckCastGeneration() {}
  FuncOptimizeImpl *Clone() override {
    return new CheckCastGeneration(*this);
  }
  void ProcessFunc(MIRFunction *func) override;

 private:
  void InitTypes();
  void InitFuncs();

  void GenAllCheckCast();
  void GenCheckCast(BaseNode *stmt, BaseNode *latestInstanceOfStmt, StIdx lastOpndStidx);
  bool FindDef(BaseNode *x, MIRSymbol *symbol);
  BaseNode *GetObjectShadow(BaseNode *opnd);
  MIRSymbol *GetOrCreateClassinfoSymbol(const std::string &className);

  void OptimizeInstanceof();
  void CheckInstanceof(StmtNode *stmt, IntrinsicopNode *intrinsicNode);
  void ReplaceInstanceof(StmtNode *stmt, const MIRClassType *targetClassType,
                         const IntrinsicopNode *intrinsicNode);
};

class DoCheckCastGeneration : public ModulePhase {
 public:
  explicit DoCheckCastGeneration(ModulePhaseID id) : ModulePhase(id) {}

  ~DoCheckCastGeneration() = default;

  std::string PhaseName() const override {
    return "gencheckcast";
  }

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(CheckCastGeneration);
    return nullptr;
  }
};

}  // namespace maple
#endif
