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

#ifndef MAPLE_ME_INCLUDE_ME_ME_CONST_PROP_H
#define MAPLE_ME_INCLUDE_ME_ME_CONST_PROP_H
#include "me_ir.h"
#include "me_phase.h"

namespace maple {

class MeConstProp {
 public:
  MeConstProp(MeFunction *f, MemPool *mp) : func(f), constprop_alloc(mp) {}

  void IntraConstProp();
  void InterConstProp();
  std::string PhaseName() const {
    return "constantpropagation";
  }

 private:
  MeFunction *func;
  MapleAllocator constprop_alloc;
};

typedef std::pair<uint32, int64> ActualArgPair;
typedef std::vector<ActualArgPair> ActualArgVector;
class ClonedFunction {
 public:
  std::string oldfuncname;
  std::string clonedfuncname;
  ActualArgVector actualarg;
  PUIdx clonedidx;
};

class MeDoIntraConstProp : public MeFuncPhase {
 public:
  explicit MeDoIntraConstProp(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *frm, ModuleResultMgr *mrm) override;
  std::string PhaseName() const override {
    return "intraconstantpropagation";
  }
};

class MeDoInterConstProp : public MeFuncPhase {
 public:
  explicit MeDoInterConstProp(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *frm, ModuleResultMgr *mrm) override;
  std::string PhaseName() const override {
    return "interconstantpropagation";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_ME_CONST_PROP_H
