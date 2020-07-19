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

#ifndef MAPLEBE_INCLUDE_CG_X86REGALLOC_H_
#define MAPLEBE_INCLUDE_CG_X86REGALLOC_H_
#include "reg_alloc.h"
#include "x86_operand.h"
#include "x86_insn.h"
#include "x86_abi.h"

namespace maplebe {

class X86RegAllocator : public RegAllocator {
 public:
  bool avail_reg_set_[MAXREG];
  MapleMap<regno_t, regno_t> reg_map_;  // mapping virtual register to physical register
  MapleSet<regno_t> live_reg_;          // a set of currently live physical registers
  MapleSet<Operand *> allocated_set_;   // already allocated

 private:
  void AllocSrcOpnd(Operand *opnd, OpndProp *opndprop = nullptr);
  void AllocDestOpnd(Operand *opnd, Insn *insn, uint32 index);

 public:
  X86RegAllocator(CGFunc *cgfunc, MapleAllocator *mallocator)
    : RegAllocator(cgfunc),
      reg_map_(std::less<regno_t>(), mallocator->Adapter()),
      live_reg_(std::less<regno_t>(), mallocator->Adapter()),
      allocated_set_(std::less<Operand *>(), mallocator->Adapter()) {}

  ~X86RegAllocator() {}

  bool AllocateRegisters() override;
  void InitAvailReg();
  bool AllocateReg(X86RegOperand *opnd);
  void PreAllocate();
  void ReleaseReg(RegType regty, uint8 reg);
  void ReleaseReg(X86RegOperand *regopnd);
  void GetPhysicalRegisterBank(RegType regty, uint8 &start, uint8 &end);
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_X86REGALLOC_H_
