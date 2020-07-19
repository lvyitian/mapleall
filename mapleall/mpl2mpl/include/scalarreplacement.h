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

#ifndef MPL2MPL_INCLUDE_SCALARRELACEMENT_H
#define MPL2MPL_INCLUDE_SCALARRELACEMENT_H

#include "phase_impl.h"
#include "module_phase.h"

namespace maple {

typedef std::vector<StmtNode *> STMT_VEC;
typedef std::unordered_map<FieldID, STMT_VEC> FLD_REF_MAP;

class ScalarReplacement : public FuncOptimizeImpl {
 private:
  MIRSymbol *curSym = nullptr;
  MIRSymbol *newScalarSym = nullptr;
  FieldID curFieldid = 0;
  std::vector<IntrinsiccallNode *> localRefCleanup;
  std::unordered_map<MIRSymbol *, FLD_REF_MAP> localVarMap;

  template <typename Func>
  BaseNode *IterateExpr(StmtNode *stmt, BaseNode *expr, Func const &applyFunc);
  template <typename Func>
  void IterateStmt(StmtNode *stmt, Func const &applyFunc);

  BaseNode *MarkDassignDread(StmtNode *stmt, BaseNode *expr);
  void CollectCandidates();
  void DumpCandidates() const;

  bool IsMemsetLocalvar(StmtNode *stmt) const;
  bool IsSetClass(StmtNode *stmt) const;
  bool IsCCWriteRefField(StmtNode *stmt) const;
  bool CanBeReplaced(const STMT_VEC *refs) const;
  BaseNode *ReplaceDassignDread(StmtNode *stmt, BaseNode *expr);
  void AppendLocalRefCleanup(MIRSymbol *sym);
  void ReplaceWithScalar(const STMT_VEC *refs);
  void FixRCCalls(const STMT_VEC *refs);
  void ReplaceLocalVars();

 public:
  ScalarReplacement(MIRModule *mod, KlassHierarchy *kh, bool trace) : FuncOptimizeImpl(mod, kh, trace) {}

  ~ScalarReplacement() {}

  FuncOptimizeImpl *Clone() {
    return new ScalarReplacement(*this);
  }

  void ProcessFunc(MIRFunction *func);
};

class DoScalarReplacement : public ModulePhase {
 public:
  explicit DoScalarReplacement(ModulePhaseID id) : ModulePhase(id) {}

  std::string PhaseName() const override {
    return "ScalarReplacement";
  }

  ~DoScalarReplacement() = default;

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(ScalarReplacement);
    return nullptr;
  }
};

}  // namespace maple

#endif
