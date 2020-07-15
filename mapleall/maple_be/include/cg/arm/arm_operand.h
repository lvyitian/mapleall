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

#ifndef MAPLEBE_INCLUDE_CG_ARMOPERAND_H_
#define MAPLEBE_INCLUDE_CG_ARMOPERAND_H_

#include "armisa.h"
#include "operand.h"
#include "cg.h"
#include "emit.h"
#include "cg_assert.h"

#include <limits>

namespace maplebe {

using namespace maple;

class ArmRegOperand : public RegOperand {
 public:
  uint32 flag_;

 public:
  explicit ArmRegOperand(uint32 reg_no, uint8 size, RegType kind, uint32 flag = 0)
    : RegOperand(reg_no, size, kind, false), flag_(flag) {
    if (kind == kRegTyUndef && reg_no < 100) {
      if (reg_no < static_cast<uint32>(R0)) {
        SetRegisterType(RegTy_cc);
      } else if (reg_no <= static_cast<uint32>(R15)) {
        SetRegisterType(kRegTyInt);
      } else if (reg_no <= static_cast<uint32>(S31)) {
        SetRegisterType(kRegTyFloat);
      }
    }
  }

  // bool IsReturn() {return reg_no_ == R0;}
  bool IsPhysicalRegister() {
    return reg_no_ < 100;
  }

  void SetAsLow32() {
    flag_ |= REGOPNDSETLOW32;
  }

  bool IsAsLow32() {
    return flag_ & REGOPNDSETLOW32;
  }

  void SetAsHigh32() {
    flag_ |= REGOPNDSETHIGH32;
  }

  bool IsAsHigh32() {
    return flag_ & REGOPNDSETHIGH32;
  }

  uint32 GetPhysicalRegister() {
    CG_ASSERT(IsPhysicalRegister(), "not a physical register");
    if (IsAsHigh32()) {
      return reg_no_ + 1;
    }
    return reg_no_;
  }

  static RegType GetRegTypeFromReg(Armregister regno) {
    if (regno < R0) {
      return RegTy_cc;
    }
    if (regno <= R15) {
      return kRegTyInt;
    }
    if (regno <= S31) {
      return kRegTyFloat;
    }
    return kRegTyUndef;
  }

  bool operator<(const ArmRegOperand &opnd) const {
    return reg_no_ < opnd.reg_no_ || (reg_no_ == opnd.reg_no_ && size_ < opnd.size_) ||
           (reg_no_ == opnd.reg_no_ && size_ == opnd.size_ && flag_ < opnd.flag_);
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override;
};

class ArmImmOperand : public ImmOperand {
 public:
  explicit ArmImmOperand(int64 val, uint8 size, bool is_signed = true) : ImmOperand(val, size, is_signed) {}

  uint32 GetLow32() {
    return (0xffffffff & val_);
  }

  uint32 GetHigh32() {
    return (0xffffffff00000000 & val_) >> 32;
  }

  uint32 GetLow16() {
    return (0xffff & val_);
  }

  uint32 GetHigh16() {
    return (0xffff0000UL & val_) >> 16;
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override {
    emitter.emit("#").emit(val_);
  }
};

typedef ArmImmOperand ArmOfstOperand;

class StImmOperand : public Operand  // representing for global variables address
{
 public:
  MIRSymbol *st_;
  int64 offset_;
  int32 relocs_;

 public:
  explicit StImmOperand(MIRSymbol *st, int64 offset, int32 relocs)
    : Operand(Opd_StImmediate, 0), st_(st), offset_(offset), relocs_(relocs) {}

  bool operator<(const StImmOperand &opnd) const {
    return st_ < opnd.st_ || (st_ == opnd.st_ && offset_ < opnd.offset_) ||
           (st_ == opnd.st_ && offset_ == opnd.offset_ && relocs_ < opnd.relocs_);
  }

  void Emit(Emitter &emitter, OpndProp *opndprop);

  void dump() {
    cout << st_->GetName();
    cout << "+offset:" << offset_;
  }
};

class ArmCGFunc;

class ArmMemOperand : public MemOperand {
 public:
  enum ArmAdrMode {
    Addressing_Undef,
    Addressing_BO,    // base, offset. EA = [base] + offset
    Addressing_BOIS,  // base ,index, scale, offset. EA = [base] + [Index]shifted
    Addressing_ISO,   // index, scale, offset. EA = [base] + [Index]shifted
    Addressing_O,     // offset only. EA = [PC] + Offset
    Addressing_TEXT   // a name
  };
  ArmAdrMode addr_mode_;

 public:
  explicit ArmMemOperand(MIRSymbol *symbol, int32 offset, int32 size, ArmCGFunc *cgfunc);

  explicit ArmMemOperand(Armregister reg, int32 offset, int32 size)
    : MemOperand(size, CG::cur_cgfunc_->memPool->New<ArmRegOperand>(reg, 32, kRegTyInt), nullptr,
                 CG::cur_cgfunc_->memPool->New<ArmOfstOperand>(offset, 32), nullptr, nullptr),
      addr_mode_(Addressing_BO) {}

  explicit ArmMemOperand(ArmAdrMode mode, int32 size, RegOperand *base, RegOperand *index, Operand *offset,
                         Operand *scale, MIRSymbol *symbol)
    : MemOperand(size, base, index, offset, symbol, scale), addr_mode_(mode) {}

  /*
     Copy constructor
   */
  ArmMemOperand(const ArmMemOperand &amo) : MemOperand(amo), addr_mode_(amo.addr_mode_) {}

  bool operator<(const ArmMemOperand &opnd) const {
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

  void Emit(Emitter &emitter, OpndProp *opndprop) override;

  void dump() override;
};

class ArmListOperand : public ListOperand {
 public:
  explicit ArmListOperand(MapleAllocator *allocator) : ListOperand(allocator) {}

  ArmRegOperand *SearchRegister(ArmRegOperand *opnd);

  static bool CompareOperand(Operand *opnd0, Operand *opnd1) {
    ArmRegOperand *regopnd0 = static_cast<ArmRegOperand *>(opnd0);
    ArmRegOperand *regopnd1 = static_cast<ArmRegOperand *>(opnd1);
    uint32 reg0 = regopnd0->IsAsHigh32() ? regopnd0->GetRegisterNumber() + 1 : regopnd0->GetRegisterNumber();
    uint32 reg1 = regopnd1->IsAsHigh32() ? regopnd1->GetRegisterNumber() + 1 : regopnd1->GetRegisterNumber();
    return reg0 < reg1;
  }

  void sort();

  void Emit(Emitter &emitter, OpndProp *opndprop) override;
};
}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_ARMOPERAND_H_
