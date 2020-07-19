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

#ifndef MAPLE_ME_INCLUDE_FUNC_EMIT_H
#define MAPLE_ME_INCLUDE_FUNC_EMIT_H
#include "bb.h"

/* Provide emit service for both MeFunction and WpoFunction.  */
namespace maple {
class FuncEmit {
 public:
  void EmitBeforeHSSA(MIRFunction *func, const MapleVector<BB *> *bblist);
  virtual ~FuncEmit() = default;

 private:
  void EmitLabelForBB(MIRFunction *func, BB *bb);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_FUNC_EMIT_H
