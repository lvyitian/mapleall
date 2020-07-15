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

#ifndef MAPLE_IPA_INCLUDE_RETYPE_H
#define MAPLE_IPA_INCLUDE_RETYPE_H
#include "../../maple_ipa/include/module_phase.h"
#include "mir_nodes.h"
#include "../../maple_ipa/include/inline.h"

namespace maple {
class Retype {
 public:
  MIRModule *mirModule;

  MapleAllocator allocator;

  MIRBuilder &mirBuilder;

  KlassHierarchy *klassh;

 public:
  explicit Retype(MIRModule *mod, MemPool *mp, MIRBuilder &builder, KlassHierarchy *k)
    : mirModule(mod), allocator(mp), mirBuilder(builder), klassh(k) {}

  virtual ~Retype() {}

  void ReplaceRetypeExpr(BaseNode *opnd);

  void Retypestmt(MIRFunction *func);

  void DoRetype();
};

}  // namespace maple
#endif  // MAPLE_IPA_INCLUDE_RETYPE_H
