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

#ifndef MAPLE_ME_INCLUDE_ME_FSAA_H
#define MAPLE_ME_INCLUDE_ME_FSAA_H
#include "me_function.h"
#include "dominance.h"
#include "ssa_tab.h"

namespace maple {

class FSAA {
 public:
  explicit FSAA(MeFunction *f, Dominance *dm): func(f), mirModule(&f->mirModule), ssaTab(f->meSSATab), dom(dm) {}
  ~FSAA() {}

  BB *FindUniquePointerValueDefBB(VersionSt *vst);
  void ProcessBB(BB *bb);

  bool needUpdateSSA = false;
 private:
  MeFunction *func;
  MIRModule *mirModule;
  SSATab *ssaTab;
  Dominance *dom;

  std::string PhaseName() const {
    return "fsaa";
  }
};

class MeDoFSAA : public MeFuncPhase {
 public:
  MeDoFSAA(MePhaseID id) : MeFuncPhase(id) {}

  ~MeDoFSAA() {}

  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "fsaa";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_FSAA_H
