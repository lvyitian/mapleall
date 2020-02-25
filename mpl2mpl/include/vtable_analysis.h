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

#ifndef MPL2MPL_VTABLEANALYSIS_H
#define MPL2MPL_VTABLEANALYSIS_H

#include "module_phase.h"
#include "phase_impl.h"

namespace maple {
#ifdef USE_32BIT_REF
#define TAB_ENTRY_SIZE 4
#else  //! USE_32BIT_REF
#define TAB_ENTRY_SIZE 8
#endif  // USE_32BIT_REF

class VtableAnalysis : public FuncOptimizeImpl {
 private:
  unordered_map<PUIdx, int> puidxToVtabIndex;
  MIRType *voidPtrType;
  MIRIntConst *zeroConst;
  MIRIntConst *oneConst;

  bool IsVtableCandidate(const MIRFunction *func) const;
  bool CheckOverrideForCrossPackage(const MIRFunction *baseMethod, const MIRFunction *currMethod) const;
  void AddMethodToTable(MethodPtrVector &methodTable, MethodPair *methodpair);
  void GenVtableList(Klass *klass);
  void DumpVtableList(const Klass *klass) const;

  void GenTableSymbol(const std::string &prefix, const std::string &klassName, MIRAggConst *newconst);
  void GenVtableDefinition(Klass *klass);
  void GenItableDefinition(Klass *klass);

  BaseNode *GenVtabItabBaseAddr(BaseNode *obj, bool isVirtual);
  int SearchWithoutRettype(const MIRFunction *callee, const MIRStructType *structType) const;
  void ReplaceVirtualInvoke(CallNode *stmt);
  void ReplaceInterfaceInvoke(CallNode *stmt);
  void ReplaceSuperclassInvoke(CallNode *stmt);
  void ReplacePolymorphicInvoke(CallNode *stmt);

 public:
  explicit VtableAnalysis(MIRModule *mod, KlassHierarchy *kh, bool dump);
  ~VtableAnalysis() {}
  void ProcessFunc(MIRFunction *func) override;

  FuncOptimizeImpl *Clone() override {
    return new VtableAnalysis(*this);
  }
};

class DoVtableAnalysis : public ModulePhase {
 public:
  explicit DoVtableAnalysis(ModulePhaseID id) : ModulePhase(id) {}

  ~DoVtableAnalysis() = default;

  std::string PhaseName() const override {
    return "vtableanalysis";
  }

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(VtableAnalysis);
    return nullptr;
  }
};
}  // namespace maple

#endif
