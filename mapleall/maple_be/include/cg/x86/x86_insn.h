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

#ifndef MAPLEBE_INCLUDE_CG_X86INSN_H_
#define MAPLEBE_INCLUDE_CG_X86INSN_H_

#include "x86_abi.h"
#include "x86_isa.h"
#include "insn.h"

namespace maplebe {

class X86CG;

class X86Insn : public Insn {
 public:
  explicit X86Insn(MOperator opc, Operand *opnd0 = nullptr, Operand *opnd1 = nullptr, Operand *opnd2 = nullptr,
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

#endif  //  MAPLEBE_INCLUDE_CG_X86INSN_H_
