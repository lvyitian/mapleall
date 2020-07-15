/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan PSL v1.
 * You can use this software according to the terms and conditions of the Mulan PSL v1.
 * You may obtain a copy of Mulan PSL v1 at:
 *
 *     http://license.coscl.org.cn/MulanPSL
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v1 for more details.
 */

#ifndef MPL2MPL_INCLUDE_ANALYZECTOR_H
#define MPL2MPL_INCLUDE_ANALYZECTOR_H

#include "module_phase.h"
#include "phase_impl.h"

namespace maple {
class AnalyzeCtor : public FuncOptimizeImpl {
 private:
  bool mHasSideEffect;
  std::unordered_set<FieldID> mFieldSet;

 public:
  AnalyzeCtor(MIRModule *mod, KlassHierarchy *kh, bool trace) : FuncOptimizeImpl(mod, kh, trace), mHasSideEffect(false) {}
  ~AnalyzeCtor() {}
  FuncOptimizeImpl *Clone() override {
    return new AnalyzeCtor(*this);
  }
  void ProcessFunc(MIRFunction *func) override;
  void Finish() override;

 private:
  void ProcessStmt(StmtNode *) override;
};

class DoAnalyzeCtor : public ModulePhase {
 public:
  explicit DoAnalyzeCtor(ModulePhaseID id) : ModulePhase(id) {}

  std::string PhaseName() const override {
    return "AnalyzeCtor";
  }

  ~DoAnalyzeCtor() = default;

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(AnalyzeCtor);
    return nullptr;
  }
};

}  // namespace maple
#endif  // MPL2MPL_INCLUDE_ANALYZECTOR_H
