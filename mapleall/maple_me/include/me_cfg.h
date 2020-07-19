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

#ifndef MAPLE_ME_INCLUDE_ME_CFG_H
#define MAPLE_ME_INCLUDE_ME_CFG_H

#include "me_function.h"
#include "me_phase.h"

namespace maple {

class MirCFG {
 public:
  MeFunction *func;
  bool hasDoWhile;
  MapleSet<LabelIdx> pattern_set_;

  explicit MirCFG(MeFunction *f) : func(f), hasDoWhile(false), pattern_set_(f->alloc.Adapter()) {}

  ~MirCFG() {}

  bool IfReplaceWithAssertnonnull(BB *bb);
  void BuildMirCFG();
  void ReplaceWithAssertnonnull();
  void ConvertPhis2IdentityAssigns(BB *mebb);
  void UnreachCodeAnalysis(bool updatePhi = false);
  void WontExitAnalysis();
  void Verify();
  void VerifyLabels();
  void Dump();
  void DumpToFile(const char *prefix, bool dumpinstrs = false);
  void AddAuxilaryBB();
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_CFG_H
