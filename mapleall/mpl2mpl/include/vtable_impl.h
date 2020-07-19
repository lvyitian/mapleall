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

#ifndef MPL2MPL_INCLUDE_VTABLEIMPL_H
#define MPL2MPL_INCLUDE_VTABLEIMPL_H

#include "module_phase.h"
#include "phase_impl.h"

namespace maple {

class VtableImpl : public FuncOptimizeImpl {
 public:
  explicit VtableImpl(MIRModule *mod, KlassHierarchy *kh, bool dump);
  ~VtableImpl() {}
  void ProcessFunc(MIRFunction *func) override;

  FuncOptimizeImpl *Clone() override {
    return new VtableImpl(*this);
  }

 private:
  MIRFunction *mccItabFunc;
  void ReplaceResolveInterface(StmtNode *stmt, const ResolveFuncNode *resolveNode);
};

class DoVtableImpl : public ModulePhase {
 public:
  explicit DoVtableImpl(ModulePhaseID id) : ModulePhase(id) {}

  std::string PhaseName() const override {
    return "VtableImpl";
  }

  ~DoVtableImpl() = default;

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(VtableImpl);
    return nullptr;
  }
};
}  // namespace maple

#endif
