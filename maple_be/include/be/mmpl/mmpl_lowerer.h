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

#ifndef MAPLEBE_INCLUDE_BE_MMPL_MMPLLOWERER_H_
#define MAPLEBE_INCLUDE_BE_MMPL_MMPLLOWERER_H_
#include "be_common.h"
#include "mmpl_mem_layout.h"
#include "be_lowerer.h"

namespace maplebe {

class MmplLowerer : public BELowerer {
 public:
  GlobalMemLayout *globmemlayout;
  MmplMemLayout *memlayout;

 public:
  explicit MmplLowerer(MIRModule &mod, BECommon &becmmn, MIRFunction *func, GlobalMemLayout *gmemlayout,
                       MmplMemLayout *lmemlayout)
    : BELowerer(mod, becmmn, kBeTargetMmpl, func), globmemlayout(gmemlayout), memlayout(lmemlayout) {
    SetOptions(kGenEh);
  }

  BaseNode *ReadregNodeForSymbol(MIRSymbol *);
  PregIdx GetSpecialRegFromSt(const MIRSymbol *);
  BaseNode *LowerDread(AddrofNode *);
  BaseNode *LowerAddrof(AddrofNode *);
  BaseNode *LowerIread(IreadNode *);
  void LowerAggDassign(BlockNode *, const DassignNode *);
  void LowerDassign(DassignNode *, BlockNode *);
  void LowerRegassign(RegassignNode *, BlockNode *);
  void LowerIassign(IassignNode *, BlockNode *);
  void LowerAggIassign(BlockNode *, IassignNode *);
  void LowerAggReturn(NaryStmtNode *, BlockNode *);
  BlockNode *LowerReturn(NaryStmtNode *);
  BaseNode *LowerIntrinsicop(BaseNode *, IntrinsicopNode *, BlockNode *);
  StmtNode *LowerIntrinsiccall(IntrinsiccallNode *, BlockNode *);
  /*virtual*/ StmtNode *LowerSyncEnterSyncExit(StmtNode *stmt) {
    CHECK_FATAL(false, "NYI");
    return stmt;
  }
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_BE_MMPL_MMPLLOWERER_H_
