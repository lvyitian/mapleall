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

#ifndef AARCH64LIVEANALYSIS_H
#define AARCH64LIVEANALYSIS_H

#include "live_analysis.h"

namespace maplebe {

class AArch64LiveAnalysis : public LiveAnalysis {
 public:
  uint32 subBBId;

  explicit AArch64LiveAnalysis(CGFunc *func, MemPool *mp)
    : LiveAnalysis(func, mp),
      subBBId(func->NumBBs()) {}

  ~AArch64LiveAnalysis() {}

  regno_t UpdateRegNum(RegOperand *opnd);
  void CollectLiveInfo(BB *bb, Operand *opnd, bool isDef, bool isUse);
  BB* CreateSubBB(BB *bb);
  bool CanInsnThrow(Insn *insn);
  void GetInsnDefUse(BB *bb, Insn *insn);
  void BreakBBIntoSubBB(BB *bb);
  void GetBBDefUse(BB *bb) override;
  bool CleanupBBIgnoreReg(uint32 reg) override;
  void FinalizeLiveAnalysis() override;
  void InitEhDefine(BB *bb) override;
};

}  // namespace maplebe

#endif /* AARCH64LIVEANALYSIS_H */
