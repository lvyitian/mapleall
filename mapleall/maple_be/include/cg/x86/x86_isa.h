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

#ifndef MAPLEBE_INCLUDE_CG_X86ISA_H_
#define MAPLEBE_INCLUDE_CG_X86ISA_H_

enum x86MOP_t {
#include "x86isa.def"
  MOP_last
};

#include "isa.h"
#include "operand.h"
#include "insn.h"

namespace maplebe {

enum X86register {
  RCC = 0,  // condition code
  RAX,
  RBX,
  RBP,
  RSP,
  RDI,
  RSI,
  RDX,
  RCX,
  R8,
  R9,
  R10,
  R11,
  R12,
  R13,
  R14,
  R15,
  XMM0,
  XMM1,
  XMM2,
  XMM3,
  XMM4,
  XMM5,
  XMM6,
  XMM7,
  XMM8,
  XMM9,
  XMM10,
  XMM11,
  XMM12,
  XMM13,
  XMM14,
  XMM15,
  MAXREGNUM
};

#define MAXREG MAXREGNUM
#define INTREGBEGIN RAX
#define INTREGEND R15
#define FLOATREGBEGIN XMM0
#define FLOATREGEND XMM15

struct RegProp {
  RegType regtype_;           // register type
  X86register physical_reg_;  // physical register
  RegProp(RegType t_, X86register r_) : regtype_(t_), physical_reg_(r_) {}
};

struct X86OpndProp : public OpndProp {
  Operand::OperandType opnd_ty_;  // type of this opnd
  RegProp regprop_;               // only vaid when opnd_ty_ is Opd_Register
  bool is_res_;                   // is this opnd is a result
  uint8 size_;                    // size of this opnd

  X86OpndProp(Operand::OperandType t_, RegProp p_, bool r_, uint8 s_)
    : opnd_ty_(t_), regprop_(p_), is_res_(r_), size_(s_) {}

  bool IsPhysicalRegister() {
    return opnd_ty_ == Operand::Opd_Register && regprop_.physical_reg_ < MAXREGNUM;
  }
};

struct X86MD {
  MOperator opc_;
  OpndProp *operand_[5];
  uint64 properties_;
  const char *name_;
  const char *format_;
  bool IsX86Style() const {
    return properties_ & ISX86STYLE;
  }

  bool UseSpecReg() const {
    return properties_ & USESPECREG;
  }
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_X86ISA_H_
