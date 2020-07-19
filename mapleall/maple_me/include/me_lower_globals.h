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

#ifndef MAPLE_ME_INCLUDE_ME_LOWER_GLOBALS_H
#define MAPLE_ME_INCLUDE_ME_LOWER_GLOBALS_H

#include "me_function.h"
#include "me_irmap.h"

namespace maple {

class LowerGlobals {
 private:
  MeFunction *func;
  IRMap *irMap;
  SSATab *ssaTab;

 public:
  LowerGlobals &operator=(const LowerGlobals &p) = default;
  LowerGlobals(const LowerGlobals &p) = default;
  LowerGlobals(MeFunction *f, SSATab *stab) : func(f), irMap(f->irMap), ssaTab(stab) {}
  ~LowerGlobals() {}

  void Run();

 private:
  void LowerGlobalDreads(MeStmt *stmt, MeExpr *x);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_LOWER_GLOBALS_H
