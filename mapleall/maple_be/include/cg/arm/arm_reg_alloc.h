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

#ifndef MAPLEBE_INCLUDE_CG_ARMREGALLOC_H_
#define MAPLEBE_INCLUDE_CG_ARMREGALLOC_H_
#include "reg_alloc.h"
#include "arm_operand.h"
#include "arm_insn.h"
#include "arm_abi.h"

namespace maplebe {

class ArmRegAllocator : public RegAllocator {
 public:
  bool avail_reg_set_[MAXREG];
  MapleMap<regno_t, regno_t> reg_map_;  // mapping virtual register to physical register
  MapleSet<regno_t> live_reg_;          // a set of currently live physical registers
  MapleSet<Operand *> allocated_set_;   // already allocated

 private:
  void AllocSrcOpnd(Operand *opnd, OpndProp *opndprop = nullptr);
  void AllocDestOpnd(Operand *opnd, Insn *insn, uint32 index);
  bool IsCalleeSaveReg(Armregister reg) {
    return reg >= R4 && reg <= R11;
  }

 public:
  ArmRegAllocator(CGFunc *cgfunc, MapleAllocator *mallocator)
    : RegAllocator(cgfunc),
      reg_map_(std::less<regno_t>(), mallocator->Adapter()),
      live_reg_(std::less<regno_t>(), mallocator->Adapter()),
      allocated_set_(std::less<Operand *>(), mallocator->Adapter()) {}

  ~ArmRegAllocator() {}

  bool AllocateRegisters() override;
  void InitAvailReg();
  bool AllocateReg(ArmRegOperand *opnd, OpndProp *prop);
  void PreAllocate();
  void ReleaseReg(RegType regty, uint8 reg);
  void ReleaseReg(ArmRegOperand *regopnd, OpndProp *prop);
  void GetPhysicalRegisterBank(RegType regty, uint8 &start, uint8 &end);
  void AllocHandleCallee(Insn *insn);
  bool IsSpecialReg(Armregister reg);
  void SaveCalleeRegs(ArmRegOperand *opnd);
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_ARMREGALLOC_H_
