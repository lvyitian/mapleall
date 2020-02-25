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

#ifndef MPL2MPL_INCLUDE_CODERELAYOUT_H
#define MPL2MPL_INCLUDE_CODERELAYOUT_H

#include "phase_impl.h"
#include "module_phase.h"
#include "file_layout.h"
#include <fstream>

namespace maple {

class CodeReLayout : public FuncOptimizeImpl {
 public:
  explicit CodeReLayout(MIRModule *mod, KlassHierarchy *kh, bool dump);
  ~CodeReLayout() {}
  FuncOptimizeImpl *Clone() override {
    return new CodeReLayout(*this);
  }
  void ProcessFunc(MIRFunction *func) override;
  void Finish() override;

 private:
  const std::string kExeFuncTag = "executedFuncStart";
  const std::string kProfileStartTag = "#profile_start";
  const std::string kProfileSummaryTag = "#profile_summary";
  std::unordered_map<std::string, MIRSymbol *> str2sym_map;
  uint32_t layout_count[static_cast<uint32_t>(LayoutType::kLayoutTypeCount)] = {};

  std::string StaticFieldFilename(const std::string &mplfile);
  void GenLayoutSym();
  void AddStaticFieldRecord();
  CallNode *CreateRecordFieldStaticCall(BaseNode *node, const std::string &name);
  void FindDreadRecur(StmtNode *stmt, BaseNode *node);
  void InsertProfileBeforeDread(StmtNode *stmt, BaseNode *opnd);
  MIRSymbol *GetorCreateStaticFieldSym(const std::string &fieldname);
  MIRSymbol *GenStrSym(const std::string &str);

  template <typename Out>
  void Split(const std::string &s, char delim, Out result);
};

class DoCodeReLayout : public ModulePhase {
 public:
  explicit DoCodeReLayout(ModulePhaseID id) : ModulePhase(id) {}

  std::string PhaseName() const override {
    return "CodeReLayout";
  }

  ~DoCodeReLayout() = default;

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(CodeReLayout);
    return nullptr;
  }
};

}  // namespace maple
#endif
