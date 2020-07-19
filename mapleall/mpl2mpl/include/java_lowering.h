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

#ifndef MPL2MPL_INCLUDE_JAVALOWERING_H
#define MPL2MPL_INCLUDE_JAVALOWERING_H

#include "phase_impl.h"
#include "module_phase.h"
#include <map>
#include <unordered_set>

namespace maple {

class JavaLowering : public FuncOptimizeImpl {
 private:
  std::string outfileName;
  std::unordered_set<std::string> clInterfaceSet;
  std::multimap<std::string, std::string> clInvocationMap;
  MIRFunction *mClassForname1Func = nullptr;
  MIRFunction *mClassForname3Func = nullptr;
  MIRFunction *mGetCurrentClassLoaderFunc = nullptr;
  MIRType *mClassLoaderPointerToType = nullptr;

 public:
  JavaLowering(MIRModule *mod, KlassHierarchy *kh, bool dump);
  ~JavaLowering() {}
  FuncOptimizeImpl *Clone() override {
    return new JavaLowering(*this);
  }

 private:
  void InitTypes();
  void InitFuncs();
  void InitLists();
  void ProcessStmt(StmtNode *stmt) override;
  void ProcessJavaMerge(StmtNode *assignNode, const IntrinsicopNode *intrinnode);
  BaseNode *JavaMergeToCvtType(PrimType dtyp, PrimType styp, BaseNode *src);

  void LoadClassLoaderInvocation(const std::string &list);
  void CheckClassloaderInvocation(const CallNode *callNode) const;
  void DumpClassloaderInvocation(const CallNode *callNode);
  void ProcessForNameClassloader(CallNode *callnode);

  void ProcessJavaFillNewArray(IntrinsiccallNode *intrinCall);
};

class DoJavaLowering : public ModulePhase {
 public:
  explicit DoJavaLowering(ModulePhaseID id) : ModulePhase(id) {}

  ~DoJavaLowering() = default;

  std::string PhaseName() const override {
    return "JavaLowering";
  }

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(JavaLowering);
    return nullptr;
  }
};
}  // namespace maple
#endif  // MPL2MPL_INCLUDE_JAVALOWERING_H
