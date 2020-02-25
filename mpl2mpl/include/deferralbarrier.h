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

#ifndef MPL2MPL_INCLUDE_DEFERRALBARRIER_H
#define MPL2MPL_INCLUDE_DEFERRALBARRIER_H

#include <set>

#include "module_phase.h"
#include "mir_nodes.h"
#include "mir_module.h"
#include "global_tables.h"

namespace maple {

class DeferralBarrier : public ModulePhase {
 public:
  DeferralBarrier(ModulePhaseID id) : ModulePhase(id) {
    reflectClassNameIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangClassStr);
    reflectMethodNameIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangReflectMethod);
    reflectFieldNameIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangReflectField);
  }

  ~DeferralBarrier() {}

  AnalysisResult *Run(MIRModule *module, ModuleResultMgr *m) override;
  std::string PhaseName() const override {
    return "deferralBarrier";
  }

 private:
  GStrIdx reflectClassNameIdx;
  GStrIdx reflectMethodNameIdx;
  GStrIdx reflectFieldNameIdx;

  class RunFunction;
  friend RunFunction;

  class RunFunction {
    friend DeferralBarrier;

    DeferralBarrier *phase;
    MIRModule *module;
    MIRFunction *func;
    MIRBuilder *builder;

    RunFunction(DeferralBarrier *phase, MIRModule *module, MIRFunction *func)
      : phase(phase), module(module), func(func), builder(module->mirBuilder) {}

    ~RunFunction() {}

    CallNode *CreateWriteRefFieldCall(BaseNode *obj, BaseNode *field, BaseNode *value);

    void HandleBlock(BlockNode *block);
    StmtNode *HandleStmt(StmtNode *stmt, BlockNode *block);
    void Run();

    bool IgnoreSymbol(MIRSymbol *symbol) {
      return phase->IgnoreSymbol(symbol);
    }

    std::string PhaseName() const {
      return phase->PhaseName();
    }

    bool SkipRhs(BaseNode *rhs);
  };

  void DeclareBarrierFunction(MIRModule *module);
  bool IgnoreSymbol(MIRSymbol *symbol);
};
}  // namespace maple
#endif  // MPL2MPL_INCLUDE_DEFERRALBARRIER_H
