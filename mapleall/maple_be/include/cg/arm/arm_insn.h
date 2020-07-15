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

#ifndef MAPLEBE_INCLUDE_CG_ARMINSN_H_
#define MAPLEBE_INCLUDE_CG_ARMINSN_H_

#include "armabi.h"
#include "armisa.h"
#include "insn.h"

namespace maplebe {

class ArmCG;

class ArmInsn : public Insn {
 public:
  explicit ArmInsn(MOperator opc, Operand *opnd0 = nullptr, Operand *opnd1 = nullptr, Operand *opnd2 = nullptr,
                   Operand *opnd3 = nullptr, Operand *opnd4 = nullptr)
    : Insn(opc, opnd0, opnd1, opnd2, opnd3, opnd4) {}

  bool IsMachineInstruction() override {
    return true;
  }

  void Emit(CG &, Emitter &) override;

  void dump() override;

  bool Check() override;

  bool IsDefinition() const override {
    CG_ASSERT(0, "NYI");
    return false;
  }

  bool IsDataMoveInstruction() const override {
    CG_ASSERT(0, "NYI");
    return false;
  }

 private:
  void CheckOpnd(Operand *opnd, OpndProp *mopd);
};

}  // namespace maplebe

#endif  //  MAPLEBE_INCLUDE_CG_ARMINSN_H_
