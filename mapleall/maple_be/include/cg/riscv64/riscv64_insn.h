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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64INSN_H
#define MAPLEBE_INCLUDE_CG_AARCH64INSN_H

#include "riscv64_isa.h"
#include "insn.h"
#include "riscv64_operand.h"
namespace maplebe {

class Riscv64Insn : public Insn {
 public:
  explicit Riscv64Insn(MOperator opc, Operand *opnd0 = nullptr, Operand *opnd1 = nullptr, Operand *opnd2 = nullptr,
                       Operand *opnd3 = nullptr, Operand *opnd4 = nullptr)
    : Insn(opc, opnd0, opnd1, opnd2, opnd3, opnd4) {}

  Riscv64Insn &operator=(const Riscv64Insn &p) = default;

  Riscv64Insn(const Riscv64Insn &originalInsn)
    : Insn(originalInsn.mop_, originalInsn.opnds[0], originalInsn.opnds[1], originalInsn.opnds[2],
           originalInsn.opnds[3], originalInsn.opnds[4]) {
    prev = originalInsn.prev;
    next = originalInsn.next;
    bb = originalInsn.bb;
    flags = originalInsn.flags;
    mop_ = originalInsn.mop_;
    for (int i = 0; i < kMaxOperandNum; i++) {
      if (originalInsn.opnds[i] == nullptr) {
        opnds[i] = nullptr;
      } else {
        opnds[i] = originalInsn.opnds[i]->Clone(CG::curCgFunc->memPool);
      }
    }
  }

  ~Riscv64Insn() {}

  bool IsReturn() override {
    return GetMachineOpcode() == MOP_xret;
  }

  bool IsFixedInsn() override {
    return (GetMachineOpcode() == MOP_clinit || GetMachineOpcode() == MOP_clinit_tail);
  }

  bool IsComment() override {
    return GetMachineOpcode() == MOP_comment;
  }

  bool IsImmaterialInsn() override {
    return (dynamic_cast<mpldbg::DbgInsn *>(this) || GetMachineOpcode() == MOP_comment);
  }

  bool IsMachineInstruction() override {
    MOperator mop = GetMachineOpcode();
    return (MOP_undef < mop && mop < MOP_comment);
  }

  bool IsPseudoInstruction() override {
    MOperator mop = GetMachineOpcode();
    return (mop >= MOP_pseudo_first && mop <= MOP_pseudo_last);
  }

  bool IsEffectiveCopy() override {
    return CopyOperands() >= 0;
  }

  int32_t GetResultNum() override;
  int32_t GetOpndNum() override;
  Operand *GetResult(int32_t i) override;
  Operand *GetOpnd(int32_t i) override;
  Operand *GetResultMemOpnd() override;
  Operand *GetMemOpnd() override;
  void SetOpnd(int32_t i, Operand *opnd) override;
  int32_t CopyOperands() override;
  bool IsGlobal() override final {
    return GetMachineOpcode() == MOP_adrp || GetMachineOpcode() == MOP_adrpl12;
  }

  bool IsCall() override final;
  bool IsTailCall() const override final;
  bool IsClinit() override final;
  bool CanThrow() override final;
  bool IsIndirectCall() override final {
    return GetMachineOpcode() == MOP_xblr;
  }

  bool IsCallToFunctionThatNeverReturns() override final;
  bool MayThrow() override final;
  bool IsBranch() override final;
  bool IsMove() override final;
  bool IsLoad() override final;
  bool IsLoadStorePair();
  bool IsStore() override final;
  bool IsLoadPair() override final;
  bool IsStorePair() override final;
  bool IsLoadAddress() override final;
  bool IsAtomic() override final;
  bool IsYieldpoint() override;
  bool IsVolatile() override;
  bool IsFallthruCall() override final {
    return (GetMachineOpcode() == MOP_xblr || GetMachineOpcode() == MOP_xbl);
  }
  bool IsMemAccessBar() override;
  bool IsMemAccess() override;

  Operand *GetCallTargetOperand() override {
    CG_ASSERT(IsCall(), "");
    return GetOperand(0);
  }

  ListOperand *GetCallArgumentOperand() override {
    CG_ASSERT(IsCall(), "");
    CG_ASSERT(GetOperand(1)->IsList(), "");
    return static_cast<ListOperand *>(GetOperand(1));
  }

  bool IsDMBInsn() const override;

  void Emit(CG &, Emitter &) override;

  void dump() override;

  bool Check() override;

  // dfa
  bool IsDefinition() const override;

  bool IsDestRegAlsoSrcReg() const override;

  bool IsDataMoveInstruction() const override;

  bool IsConversionInstruction() const override;

  bool IsConditionalSet() const override;

  RegOperand *GetOperandDefined() override;

  bool IsPartDef() const override;

  uint32 GetLatencyType() const override;

  int GetJumpTargetIdx() const override;

 private:
  void CheckOpnd(Operand *opnd, OpndProp *mopd);
  void EmitClinit(CG &, Emitter &);
  void EmitAdrpLdr(CG &, Emitter &);
  void EmitClinitTail(CG &, Emitter &);
  void EmitAdrpLabel(CG &, Emitter &);
  void EmitCheckThrowPendingException(CG &, Emitter &);
};

class Riscv64cleancallInsn : public Riscv64Insn {
 public:
  int ref_skipindex;
  explicit Riscv64cleancallInsn(MOperator opc, Operand *opnd0 = nullptr, Operand *opnd1 = nullptr,
                                Operand *opnd2 = nullptr, Operand *opnd3 = nullptr, Operand *opnd4 = nullptr)
    : Riscv64Insn(opc, opnd0, opnd1, opnd2, opnd3, opnd4), ref_skipindex(-1) {}

  Riscv64cleancallInsn &operator=(const Riscv64cleancallInsn &p) = default;

  Riscv64cleancallInsn(const Riscv64cleancallInsn &originalInsn) : Riscv64Insn(originalInsn) {
    ref_skipindex = originalInsn.ref_skipindex;
  }
  ~Riscv64cleancallInsn() {}
};

}  // namespace maplebe

#endif  //  MAPLEBE_INCLUDE_CG_AARCH64INSN_H
