/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
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

#ifndef AARCH64LIVEANALYSIS_H
#define AARCH64LIVEANALYSIS_H

#include "live_analysis.h"

namespace maplebe {

class Riscv64LiveAnalysis : public LiveAnalysis {
 public:
  uint32 subBBId;

  explicit Riscv64LiveAnalysis(CGFunc *func, MemPool *mp)
    : LiveAnalysis(func, mp),
      subBBId(func->NumBBs()) {}

  ~Riscv64LiveAnalysis() {}

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
