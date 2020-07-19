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

#ifndef MPL2MPL_INCLUDE_BARRIERINSERTION_H
#define MPL2MPL_INCLUDE_BARRIERINSERTION_H

#include <set>

#include "module_phase.h"
#include "mir_nodes.h"
#include "mir_module.h"
#include "mir_builder.h"

namespace maple {

class BarrierInsertion : public ModulePhase {
 public:
  BarrierInsertion(ModulePhaseID id) : ModulePhase(id) {}

  ~BarrierInsertion() {}

  AnalysisResult *Run(MIRModule *module, ModuleResultMgr *m) override;
  std::string PhaseName() const override {
    return "barrierinsertion";
  }

 private:
  class RunFunction;
  friend RunFunction;

  class RunFunction {
    friend BarrierInsertion;

    BarrierInsertion *phase;
    MIRModule *module;
    MIRFunction *func;
    MIRBuilder *builder;

    int backupVarCount;
    std::set<StIdx> backupVarIndices;

    std::set<StIdx> assignedParams;

    std::set<NaryStmtNode *> rets;  // there could be multiple return in the function

    RunFunction(BarrierInsertion *phase, MIRModule *module, MIRFunction *func)
      : phase(phase), module(module), func(func), builder(module->mirBuilder), backupVarCount(0) {}

    ~RunFunction() {}

    CallNode *CreateWriteRefVarCall(BaseNode *var, BaseNode *value);
    CallNode *CreateWriteRefFieldCall(BaseNode *obj, BaseNode *field, BaseNode *value);
    CallNode *CreateReleaseRefVarCall(BaseNode *var);

    // MCC_CheckObjMem(address_t obj)
    CallNode *CreateMemCheckCall(BaseNode *obj);

    void HandleBlock(BlockNode *block);
    StmtNode *HandleStmt(StmtNode *stmt, BlockNode *block);
    StmtNode *CheckRefRead(BaseNode *opnd, StmtNode *stmt, BlockNode *block);
    void InsertPrologue();
    void HandleReturn(NaryStmtNode *retnode);
    MIRSymbol *NewBackupVariable(const char *suffix);
    void Run();
    std::string PhaseName() const {
      return phase->PhaseName();
    }

    bool SkipRhs(BaseNode *rhs);
  };

  void EnsureLibraryFunction(MIRModule *module, const char *name, MIRType *rettype, const ArgVector &args);
  void EnsureLibraryFunctions(MIRModule *module);
};
}  // namespace maple
#endif
