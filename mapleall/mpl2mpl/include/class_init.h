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

#ifndef MPL2MPL_INCLUDE_CLASSINIT_H
#define MPL2MPL_INCLUDE_CLASSINIT_H

#include "phase_impl.h"
#include "module_phase.h"
#include "class_hierarchy.h"

namespace maple {
class ClassInit : public FuncOptimizeImpl {
 private:
  std::unordered_set<std::string> preloadedClass;

  void BuildPreloadedClass();

  void GenClassInitCheckProfile(MIRFunction *func, MIRSymbol *classinfo, StmtNode *clinit) const;
  void GenPreClassInitCheck(MIRFunction *func, MIRSymbol *classinfo, StmtNode *clinit);
  void GenPostClassInitCheck(MIRFunction *func, MIRSymbol *classinfo, StmtNode *clinit);

  MIRSymbol *GetClassInfo(const std::string &classname);
  bool CanRemoveClinitCheck(const std::string &clinitclassname);

public:
  explicit ClassInit(MIRModule *mod, KlassHierarchy *kh, bool dump);
  ~ClassInit() {}
  FuncOptimizeImpl *Clone() override {
    return new ClassInit(*this);
  }
  void ProcessFunc(MIRFunction *func) override;
};

class DoClassInit : public ModulePhase {
 public:
  explicit DoClassInit(ModulePhaseID id) : ModulePhase(id) {}

  ~DoClassInit() = default;

  std::string PhaseName() const override {
    return "clinit";
  }

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(ClassInit);
    return nullptr;
  }
};
}  // namespace maple

#endif /* MPL2MPL_INCLUDE_CLASSINIT_H */
