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

#ifndef MAPLEBE_INCLUDE_CG_X86OPERAND_H_
#define MAPLEBE_INCLUDE_CG_X86OPERAND_H_

#include "x86_abi.h"
#include "x86_isa.h"
#include "x86_mem_layout.h"
#include "operand.h"
#include "emit.h"
#include "cg_assert.h"

#include <iostream>
#include <limits>

namespace maplebe {

using namespace std;

class X86RegOperand : public RegOperand {
 public:
  explicit X86RegOperand(regno_t reg_no, uint8 size, RegType kind) : RegOperand(reg_no, size, kind, false) {
    if (kind == kRegTyUndef && reg_no < 100) {
      if (reg_no < (regno_t)RAX) {
        SetRegisterType(RegTy_cc);
      } else if (reg_no <= (regno_t)R15) {
        SetRegisterType(kRegTyInt);
      } else if (reg_no <= (regno_t)XMM15) {
        SetRegisterType(kRegTyFloat);
      }
    }
  }

  bool IsPhysicalRegister() {
    return reg_no_ < 100;
  }

  static RegType GetRegTypeFromReg(X86register regno) {
    if (regno < RAX) {
      return RegTy_cc;
    }
    if (regno <= R15) {
      return kRegTyInt;
    }
    if (regno <= XMM15) {
      return kRegTyFloat;
    }
    return kRegTyUndef;
  }

  bool operator<(const X86RegOperand &other) const {
    regno_t my_rn = GetRegisterNumber();
    regno_t ot_rn = other.GetRegisterNumber();
    uint32 my_sz = GetSize();
    regno_t ot_sz = other.GetSize();
    return ((my_rn < ot_rn) || (my_rn == ot_rn && my_sz < ot_sz));
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override;
};

class X86ImmOperand : public ImmOperand {
 public:
  explicit X86ImmOperand(int64 val, uint8 size, bool is_signed) : ImmOperand(val, size, is_signed) {}

  void Emit(Emitter &emitter, OpndProp *opndprop) override {
    emitter.emit("$").emit(val_);
  }
};

class X86OfstOperand : public OfstOperand {
 public:
  explicit X86OfstOperand(int32 val, uint8 size) : OfstOperand(val, size, true) {}

  bool operator<(const OfstOperand &opnd) const {
    return GetValue() < opnd.GetValue();
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override;

  void dump() override;
};

class StImmOperand : public Operand  // representing for global variables address
{
 public:
  MIRSymbol *st_;
  int64 offset_;
  int32 relocs_;

 public:
  explicit StImmOperand(MIRSymbol *st, int64 offset, int32 relocs)
    : Operand(Opd_StImmediate, 0), st_(st), offset_(offset), relocs_(relocs) {}

  void dump() {
    cout << st_->GetName();
    cout << "+offset:" << offset_;
  }

  /* virtual */ void Emit(Emitter &emitter, OpndProp *opndprop);

  bool operator<(const StImmOperand &opnd) const {
    return st_ < opnd.st_ || (st_ == opnd.st_ && offset_ < opnd.offset_) ||
           (st_ == opnd.st_ && offset_ == opnd.offset_ && relocs_ < opnd.relocs_);
  }
};

class X86MemOperand : public MemOperand {
 public:
  enum X86AdrMode {
    Addressing_Undef,
    Addressing_BO,    // base, offset
    Addressing_BOIS,  // base ,index, scale, offset
    Addressing_ISO,   // index, scale, offset
    Addressing_O,     // offset only
    Addressing_TEXT   // a name
  };
  X86AdrMode addr_mode_;
 public:
  explicit X86MemOperand(MIRSymbol *symbol, int32 offset, int32 size, MemLayout *memlayout);

  explicit X86MemOperand(X86register reg, int32 offset, int32 size)
    : MemOperand(size, CG::cur_cgfunc_->memPool->New<X86RegOperand>(reg, 64, kRegTyInt), nullptr,
                 CG::cur_cgfunc_->memPool->New<X86OfstOperand>(offset, 32), nullptr, nullptr),
      addr_mode_(Addressing_BO) {}

  explicit X86MemOperand(X86AdrMode mode, int32 size, RegOperand *base, RegOperand *index, Operand *offset,
                         Operand *scale, MIRSymbol *symbol)
    : MemOperand(size, base, index, offset, symbol, scale), addr_mode_(mode) {}

  // copy constructor
  X86MemOperand(const X86MemOperand &xmo) : MemOperand(xmo), addr_mode_(xmo.addr_mode_) {}

  bool operator<(const X86MemOperand &opnd) const {
    return addr_mode_ < opnd.addr_mode_ ||
           (addr_mode_ == opnd.addr_mode_ && GetBaseRegister() < opnd.GetBaseRegister()) ||
           (addr_mode_ == opnd.addr_mode_ && GetBaseRegister() == opnd.GetBaseRegister() &&
            GetIndexRegister() < opnd.GetIndexRegister()) ||
           (addr_mode_ == opnd.addr_mode_ && GetBaseRegister() == opnd.GetBaseRegister() &&
            GetIndexRegister() == opnd.GetIndexRegister() && GetOffsetOperand() < opnd.GetOffsetOperand()) ||
           (addr_mode_ == opnd.addr_mode_ && GetBaseRegister() == opnd.GetBaseRegister() &&
            GetIndexRegister() == opnd.GetIndexRegister() && GetOffsetOperand() == opnd.GetOffsetOperand() &&
            GetScaleOperand() < opnd.GetScaleOperand()) ||
           (addr_mode_ == opnd.addr_mode_ && GetBaseRegister() == opnd.GetBaseRegister() &&
            GetIndexRegister() == opnd.GetIndexRegister() && GetOffsetOperand() == opnd.GetOffsetOperand() &&
            GetScaleOperand() == opnd.GetScaleOperand() && GetSymbol() < opnd.GetSymbol()) ||
           (addr_mode_ == opnd.addr_mode_ && GetBaseRegister() == opnd.GetBaseRegister() &&
            GetIndexRegister() == opnd.GetIndexRegister() && GetOffsetOperand() == opnd.GetOffsetOperand() &&
            GetScaleOperand() == opnd.GetScaleOperand() && GetSymbol() == opnd.GetSymbol() &&
            GetSize() < opnd.GetSize());
  }

  void dump() override;

  void Emit(Emitter &emitter, OpndProp *opndprop) override;
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_X86OPERAND_H_
