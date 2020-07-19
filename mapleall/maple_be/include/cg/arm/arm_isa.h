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

#ifndef MAPLEBE_INCLUDE_CG_ARMISA_H_
#define MAPLEBE_INCLUDE_CG_ARMISA_H_

enum armMOP_t {
#include "arm_isa.def"
  MOP_last
};

#include "isa.h"
#include "operand.h"
#include "insn.h"

namespace maplebe {

enum Armregister {
  RCC = 0,  // condition code
  R0,
  R1,
  R2,
  R3,
  R4,
  R5,
  R6,
  R7,
  R8,
  R9,
  R10,
  R11,
  R12,
  R13,
  R14,
  R15,
  S0,
  S1,
  S2,
  S3,
  S4,
  S5,
  S6,
  S7,
  S8,
  S9,
  S10,
  S11,
  S12,
  S13,
  S14,
  S15,
  S16,
  S17,
  S18,
  S19,
  S20,
  S21,
  S22,
  S23,
  S24,
  S25,
  S26,
  S27,
  S28,
  S29,
  S30,
  S31,
  MAXREGNUM
};

#define MAXREG MAXREGNUM
#define INTREGBEGIN R0
#define INTREGEND R15
#define FLOATREGBEGIN S0
#define FLOATREGEND S31
#define RIP R12
#define RSP R13
#define RLR R14
#define RPC R15

#define REGPROPUNDEF 0
#define REGPROPDEF 0x1
#define REGPROPUSE 0x2
#define REGHIGH 0x4
#define REGLOW 0x8
#define MEMHIGH16 0x10  // currently for read the address of this memory
#define MEMLOW16 0x20

struct RegProp {
  RegType regtype_;
  Armregister physical_reg_;
  uint32 def_use_;  // used for register use/define and other properties of other operand
  RegProp(RegType t_, Armregister r_, uint32 d_) : regtype_(t_), physical_reg_(r_), def_use_(d_) {}
};

struct ArmOpndProp : public OpndProp {
  Operand::OperandType opnd_ty_;
  RegProp regprop_;
  uint8 size_;

  ArmOpndProp(Operand::OperandType t_, RegProp p_, uint8 s_) : opnd_ty_(t_), regprop_(p_), size_(s_) {}

  bool IsPhysicalRegister() {
    return opnd_ty_ == Operand::Opd_Register && regprop_.physical_reg_ < MAXREGNUM;
  }

  bool IsRegDef() {
    return opnd_ty_ == Operand::Opd_Register && (regprop_.def_use_ & REGPROPDEF);
  }

  bool IsRegUse() {
    return opnd_ty_ == Operand::Opd_Register && (regprop_.def_use_ & REGPROPUSE);
  }

  bool IsRegHigh() {
    return opnd_ty_ == Operand::Opd_Register && (regprop_.def_use_ & REGHIGH);
  }

  bool IsRegLow() {
    return opnd_ty_ == Operand::Opd_Register && (regprop_.def_use_ & REGLOW);
  }

  bool IsMemLow16() {
    return opnd_ty_ == Operand::Opd_Mem && (regprop_.def_use_ & MEMLOW16);
  }

  bool IsMemHigh16() {
    return opnd_ty_ == Operand::Opd_Mem && (regprop_.def_use_ & MEMHIGH16);
  }
};

struct ARMMD {
  MOperator opc_;
  OpndProp *operand_[5];
  uint64 properties_;
  const char *name_;
  const char *format_;
  bool UseSpecReg() const {
    return properties_ & USESPECREG;
  }

  bool IsCall() const {
    return properties_ & ISCALL;
  }
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_ARMISA_H_
