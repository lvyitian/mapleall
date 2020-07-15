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

#ifndef MAPLE_IR_INCLUDE_JAVA_EH_LOWER_H
#define MAPLE_IR_INCLUDE_JAVA_EH_LOWER_H
#include "module_phase.h"
#include "mir_nodes.h"
#include "mir_module.h"

namespace maple {
const std::string strDivOpnd = "__div_opnd1";
const std::string strDivRes = "__div_res";
const std::string strMCCThrowArrayIndexOutOfBoundsException = "MCC_ThrowArrayIndexOutOfBoundsException";
const std::string strMCCThrowNullPointerException = "MCC_ThrowNullPointerException";
class JavaEHLowerer : public ModulePhase {
 public:
  MIRModule *mirModule;
  uint32 divSTIndex;  // the index of divide operand and result
  bool useRegTmp;   // use register to save temp variable

 public:
  JavaEHLowerer(ModulePhaseID id) : ModulePhase(id), mirModule(nullptr), divSTIndex(0), useRegTmp(false) {}

  ~JavaEHLowerer() {}

  void DoLower(MIRFunction *func);
  AnalysisResult *Run(MIRModule *module, ModuleResultMgr *m) override;
  std::string PhaseName() const override {
    return "javaehlower";
  }

 private:
  BlockNode *DoLowerBlock(BlockNode *);
  BaseNode *DoLowerExpr(BaseNode *, BlockNode *);
  BaseNode *DoLowerDiv(BinaryNode *, BlockNode *);
  BaseNode *DoLowerRem(BinaryNode *expr, BlockNode *blknode) {
    return DoLowerDiv(expr, blknode);
  }

  void DoLowerBoundaryCheck(IntrinsiccallNode *, BlockNode *);
};
}  // namespace maple

#endif  // MAPLE_IR_INCLUDE_JAVA_EH_LOWER_H
