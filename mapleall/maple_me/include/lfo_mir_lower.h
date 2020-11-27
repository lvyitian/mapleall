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

#ifndef MAPLE_ME_INCLUDE_LFO_MIR_LOWER_H
#define MAPLE_ME_INCLUDE_LFO_MIR_LOWER_H
#include "mir_lower.h"
#include "me_function.h"
class LFOMIRLower : public MIRLower {
 public:
  MeFunction *func;
  LfoFunction *lfoFunc;

 public:
  LFOMIRLower(MIRModule &mod, MeFunction *f) : MIRLower(mod, f->mirFunc),
        func(f),
        lfoFunc(f->lfoFunc) {}

  BlockNode *LowerWhileStmt(WhileStmtNode *);
  BlockNode *LowerIfStmt(IfStmtNode *ifstmt, bool recursive = true);
};
#endif  // MAPLE_ME_INCLUDE_LFO_MIR_LOWER_H
