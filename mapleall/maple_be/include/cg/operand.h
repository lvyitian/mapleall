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

#ifndef MAPLEBE_INCLUDE_CG_OPERAND_H_
#define MAPLEBE_INCLUDE_CG_OPERAND_H_

#include "../be/be_common.h"
#include "isa.h"
#include "cg_assert.h"
#include "cg_option.h"

// mapleir
#include "types_def.h"   // need uint8 etc
#include "prim_types.h"  // for PrimType
#include "mir_symbol.h"

// Mempool
#include "mempool_allocator.h"  // MapleList

namespace maplebe {

class Emitter;
class OpndProp;
class SSANode;

class Operand {
 public:
  enum OperandType {
    Opd_Register,
    Opd_Immediate,
    Opd_FPImmediate,
    Opd_FPZeroImmediate,
    Opd_StImmediate,    // use the symbol name as the offset
    Opd_OfstImmediate,  // for the offset operand in MemOperand
    Opd_Mem,
    Opd_BbAddress,
    Opd_List,    //  for arm32 list operand
    Opd_Cond,    //  for AArch64 condition code
    Opd_Shift,   //  for AArch64 shift operand
    Opd_Extend,  //  for AArch64 extend operand
    Opd_String,  //  for comments
    Opd_Undef
  };
  OperandType op_kind_;  // operand type
  uint32 size_;          // size in bits

 public:
  explicit Operand(OperandType type, uint32 size) : op_kind_(type), size_(size) {}

  inline uint32 GetSize() const {
    return size_;
  }

  inline void SetSize(uint32 sz) {
    size_ = sz;
  }

  inline OperandType GetKind() const {
    return op_kind_;
  }

  inline bool IsIntImmediate() const {
    return op_kind_ == Opd_Immediate;
  }

  inline bool IsConstImmediate() const {
    return op_kind_ == Opd_Immediate || op_kind_ == Opd_FPImmediate || op_kind_ == Opd_FPZeroImmediate;
  }

  inline bool IsOfstImmediate() const {
    return op_kind_ == Opd_OfstImmediate;
  }

  inline bool IsStImmediate() const {
    return op_kind_ == Opd_StImmediate;
  }

  inline bool IsImmediate() {
    CG_ASSERT(Opd_OfstImmediate - Opd_Immediate == 4, "");
    return (Opd_Immediate <= op_kind_ && op_kind_ <= Opd_OfstImmediate);
  }

  inline bool IsRegister() const {
    return op_kind_ == Opd_Register;
  }

  inline bool IsList() const {
    return op_kind_ == Opd_List;
  }

  inline bool IsMemoryAccessOperand() const {
    return op_kind_ == Opd_Mem;
  }

  inline bool IsConstant() {
    return IsConstImmediate() || IsConstReg();
  }

  bool IsConstReg() {
    if (!IsRegister()) {
      return false;
    }
    return IsZeroRegister();
  };

  virtual bool IsZeroRegister() {
    return false;
  };

  bool IsLabel() const {
    return op_kind_ == Opd_BbAddress;
  }

  bool IsConditionCode() const {
    return op_kind_ == Opd_Cond;
  }

  virtual bool HasSpill() {
    return false;
  };
  virtual Operand *Clone(MemPool *mp) const = 0;

  /**
   * A simple implementation here.
   * Each subclass can elaborate on demand.
   */
  virtual bool Equals(Operand *op) {
    return BasicEquals(op) && this == op;
  }

  bool BasicEquals(const Operand *op) const {
    return op_kind_ == op->GetKind() && size_ == op->GetSize();
  }

  virtual void Emit(Emitter &, OpndProp *) = 0;

  virtual void dump() = 0;

  virtual bool Less(Operand *right) const = 0;
};

// RegOperand
#define REGOPNDNONE 0
#define REGOPNDSETLOW32 0x1
#define REGOPNDSETHIGH32 0x2


class RegOperand : public Operand {
 protected:
  regno_t reg_no_;
  RegType type_;
  bool is_virtual;
  bool is_bb_local;  // used for EBO(-O1), it can recognize the registers whose use and def are in different BB. It is
                     // true by default. Sometime it should be false such as when handle intrinsiccall for target
                     // aarch64(AArch64CGFunc::SelectIntrinCall).
  uint8 validBitsNum;
  explicit RegOperand(regno_t r, uint8 s, RegType t, bool v)
    : Operand(Opd_Register, s),
      reg_no_(r),
      type_(t),
      is_virtual(v),
      is_bb_local(true),
      validBitsNum(s) {}

  inline void SetRegisterType(RegType t) {
    type_ = t;
  }

 public:
  void SetValidBitsNum(uint8 i) {
    validBitsNum = i;
  }

  uint8 GetValidBitsNum() const {
    return validBitsNum;
  }

  inline bool IsOfIntClass() const {
    return type_ == kRegTyInt;
  }

  inline bool IsOfFloatOrSIMDClass() const {
    return type_ == kRegTyFloat;
  }

  inline bool IsOfCC() const {
    return type_ == kRegTyCc;
  }

  inline bool IsOfVary() const {
    return type_ == kRegTyVary;
  }

  inline RegType GetRegisterType() const {
    return type_;
  }

  inline bool IsVirtualRegister() const {
    return is_virtual;
  }

  virtual bool IsCallArgumentPassingMachineRegister() const = 0;

  virtual bool IsReturnValueMachineRegister() const = 0;

  inline bool IsBBLocalVReg() const {
    return is_virtual && is_bb_local;
  }

  inline void SetRegNotBBLocal() {
    is_bb_local = false;
  }

  regno_t GetRegisterNumber() const {
    return reg_no_;
  }

  void SetRegisterNumber(regno_t r) {
    reg_no_ = r;
  }

  virtual bool IsInvalidRegister() {
    return false;
  }

  virtual bool IsSaveReg(MIRType *ty, BECommon &becommon) {
    return false;
  }

  virtual bool IsPhysicalRegister() {
    return false;
  }

  virtual bool IsSPOrFP() {
    return false;
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override = 0;

  void dump() override {
    const char *p[kRegTyLast] = { "U", "R", "V", "C", "X", "Vra" };
    const char *c[kRegTyLast] = { "[U]", "[I]", "[F]", "[CC]", "[X87]", "[Vra]" };
    CG_ASSERT(type_ < kRegTyLast, "");
    regno_t r = is_virtual ? reg_no_ : (reg_no_ - 1);
    if (CGOptions::doSimplePrint) {
      LogInfo::MapleLogger() << (is_virtual ? "vreg:" : " reg:") << p[type_] << r << c[type_] << "["
                << static_cast<uint32>(validBitsNum) << "]";
    } else {
      LogInfo::MapleLogger() << (is_virtual ? "vreg:" : " reg:") << p[type_] << r << " class: " << c[type_] << " validBitNum: ["
                << static_cast<uint32>(validBitsNum) << "]";
    }
  }

  virtual bool Less(Operand *right) const override {
    if (this == right) {
      return false;
    }

    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    RegOperand *rop = static_cast<RegOperand *>(right);

    // The same type.
    return reg_no_ < rop->reg_no_;
  }

  inline bool Less(const RegOperand *right) const {
    return reg_no_ < right->reg_no_;
  }

  inline bool RegNumEqual(const RegOperand *right) const {
    if (reg_no_ == right->GetRegisterNumber()) {
      return true;
    }
    return false;
  }

  inline int RegCompare(const RegOperand *right) const {
    return (reg_no_ - right->GetRegisterNumber());
  }

  bool Equals(Operand *operand) override {
    if (!dynamic_cast<RegOperand *>(operand)) {
      return false;
    }
    RegOperand *op = static_cast<RegOperand *>(operand);
    if (this == op) {
      return true;
    }
    if (BasicEquals(op) && reg_no_ == op->GetRegisterNumber() && type_ == op->GetRegisterType() &&
        IsBBLocalVReg() == op->IsBBLocalVReg()) {
      return true;
    }
    return false;
  }

};  // class RegOperand

class VRegOperand : public RegOperand {
 public:
  explicit VRegOperand(regno_t r, uint8 s, RegType t) : RegOperand(r, s, t, true) {
    CG_ASSERT(static_cast<int>(s) == 32 || static_cast<int>(s) == 64 || static_cast<int>(s) == 128, "");
  }

  ~VRegOperand() {}

  bool IsCallArgumentPassingMachineRegister() const override {
    return false;
  }

  bool IsReturnValueMachineRegister() const override {
    return false;
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override {
    CHECK_FATAL(0, "must not be invoked");
  }

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<VRegOperand>(*this);
  }
};

class ImmOperand : public Operand {
 protected:
  int64 val_;
  bool is_signed_;
  bool is_vary;

 public:
  explicit ImmOperand(int64 val, uint8 size, bool isSigned, bool isVar = false)
    : Operand(Opd_Immediate, size), val_(val), is_signed_(isSigned), is_vary(isVar) {}

  int64 GetValue() const {
    return val_;
  }

  inline void SetValue(int64 v) {
    val_ = v;
  }

  inline void SetVary(bool flag) {
    is_vary = flag;
  }

  inline bool IsZero() const {
    return val_ == 0;
  }

  inline bool IsVary() const {
    return is_vary;
  }

  inline bool IsOne() const {
    return val_ == 1;
  }

  inline bool IsSignedValue() const {
    return is_signed_;
  }

  bool IsInBitSize(uint8 size, uint8 nLowerZeroBits = 0) const {
    uint64 mask1 = 0xffffffffffffffffULL << size;
    uint64 mask2 = (static_cast<uint64>(1) << static_cast<uint64>(nLowerZeroBits)) - 1;
    return (mask2 & val_) == 0 && (mask1 & ((static_cast<uint64>(val_)) >> nLowerZeroBits)) == 0;
  }

  bool IsInBitSizeRot(uint8 size) const {
    return IsInBitSizeRot(size, val_);
  }

  static bool IsInBitSizeRot(uint8 size, int64 val) {
    // to tell if the val is in a rotate window of size
#if __GNU_C__ || __clang__
    if (val == 0) {
      return true;
    }
    int start = __builtin_ctzll(val);
    int end = sizeof(val) * 8 - __builtin_clzll(val) - 1;
    return (end - start + 1 <= size);
#else
    uint8 start = 0;
    uint8 end = 0;
    bool isfound = false;
    for (uint32 i = 0; i < 64; i++) {
      if (((val >> i) & 0x1) == 0x1) {
        if (!isfound) {
          start = i;
          end = i;
          isfound = true;
        } else {
          end = i;
        }
      }
    }
    return !isfound || (end - start + 1 <= size);
#endif
  }

  static bool IsInValueRange(int32 lowval, int32 highval, int32 val) {
    return val >= lowval && val <= highval;
  }

  bool IsNegative() const {
    return is_signed_ && val_ < 0;
  }

  void Add(int64 delta) {
    val_ += delta;
  }

  void Negate() {
    val_ = -val_;
  }

  void BitwiseNegate() {
    val_ = ~(static_cast<uint64>(val_)) & ((1ULL << size_) - 1);
  }

  void DivideByPow2(int n) {
    val_ >>= n;
  }

  void ModuloByPow2(int n) {
    val_ = (static_cast<uint64>(val_)) & ((1ULL << n) - 1);
  }

  inline bool IsAllOnes() const {
    return val_ == -1;
  }
  inline bool IsAllOnes32bit() const {
    return val_ == 0x0ffffffffLL;
  }

  bool IsInRange(uint8 size, bool isSigned) const {
    if (isSigned) {
      return val_ >= (-(2 << (size - 1))) && (val_ <= (2 << (size - 1)) - 1);
    } else {
      return val_ >= 0 && val_ <= ((2 << size) - 1);
    }
  }

  bool operator<(const ImmOperand &iopnd) const {
    return val_ < iopnd.val_ || (val_ == iopnd.val_ && is_signed_ < iopnd.is_signed_) ||
           (val_ == iopnd.val_ && is_signed_ == iopnd.is_signed_ && size_ < iopnd.size_);
  }

  bool operator==(const ImmOperand &iopnd) const {
    return (val_ == iopnd.val_ && is_signed_ == iopnd.is_signed_ && size_ == iopnd.size_);
  }

  void Emit(Emitter &emitter, OpndProp *prop) override = 0;

  void dump() override;

  virtual bool Less(Operand *right) const override {
    if (this == right) {
      return false;
    }

    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    ImmOperand *rop = static_cast<ImmOperand *>(right);

    // The same type.
    if (is_signed_ != rop->is_signed_) {
      return is_signed_;
    }

    if (is_vary != rop->is_vary) {
      return is_vary;
    }

    return val_ < rop->val_;
  }

  bool Equals(Operand *operand) override {
    if (!dynamic_cast<ImmOperand *>(operand)) {
      return false;
    }
    ImmOperand *op = static_cast<ImmOperand *>(operand);
    if (this == op) {
      return true;
    }
    if (BasicEquals(op) && val_ == op->GetValue() && is_signed_ == op->IsSignedValue()) {
      return true;
    }
    return false;
  }

  bool ValueEquals(const ImmOperand *op) const {
    if (this == op) {
      return true;
    }
    if (val_ == op->GetValue() && is_signed_ == op->IsSignedValue()) {
      return true;
    }
    return false;
  }
};

typedef ImmOperand OfstOperand;

class MemOperand : public Operand {
  RegOperand *base_opnd_;  // base register

  RegOperand *index_opnd_;  // offset register

  Operand *offset_opnd_;  // offset immediate

  Operand *scale_opnd_;

  MIRSymbol *symbol_;  // AddrMode_Literal

  uint32_t memory_ordering_;

 protected:
  MemOperand(int sz, MIRSymbol *sy)
    : Operand(Operand::Opd_Mem, sz),
      base_opnd_(nullptr),
      index_opnd_(nullptr),
      offset_opnd_(nullptr),
      scale_opnd_(nullptr),
      symbol_(sy),
      memory_ordering_(0) {}

  MemOperand(int32 sz, RegOperand *bo, RegOperand *io, Operand *oo, MIRSymbol *sy, Operand *so = nullptr)
    : Operand(Operand::Opd_Mem, sz),
      base_opnd_(bo),
      index_opnd_(io),
      offset_opnd_(oo),
      scale_opnd_(so),
      symbol_(sy),
      memory_ordering_(0) {}

  /*
     Copy constructor
   */
  MemOperand(const MemOperand &mo)
    : Operand(Operand::Opd_Mem, mo.GetSize()),
      base_opnd_(mo.base_opnd_),
      index_opnd_(mo.index_opnd_),
      offset_opnd_(mo.offset_opnd_),
      scale_opnd_(mo.scale_opnd_),
      symbol_(mo.symbol_),
      memory_ordering_(mo.memory_ordering_) {}

  ~MemOperand() {}

 public:
  // Operand *Clone() const override=0;
  MemOperand &operator=(const MemOperand &p) = default;

  RegOperand *GetBaseRegister() const {
    return base_opnd_;
  }

  void SetBaseRegister(RegOperand *r) {
    base_opnd_ = r;
  }

  RegOperand *GetIndexRegister() const {
    return index_opnd_;
  }

  void SetIndexRegister(RegOperand *r) {
    index_opnd_ = r;
  }

  Operand *GetOffsetOperand() const {
    return offset_opnd_;
  }

  void SetOffsetOperand(Operand *oo) {
    offset_opnd_ = oo;
  }

  Operand *GetScaleOperand() const {
    return scale_opnd_;
  }

  MIRSymbol *GetSymbol() const {
    return symbol_;
  }

  bool Equals(Operand *operand) override {
    if (!dynamic_cast<MemOperand *>(operand)) {
      return false;
    }
    MemOperand *op = static_cast<MemOperand *>(operand);
    if (this == op) {
      return true;
    }
    CHECK_FATAL(base_opnd_ != nullptr && index_opnd_ != nullptr, "pointer is null in Equals");
    if (base_opnd_->Equals(op->GetBaseRegister()) && index_opnd_->Equals(op->GetIndexRegister())) {
      return true;
    }
    return false;
  }

  void SetMemoryOrdering(uint32_t mo) {
    memory_ordering_ |= mo;
  }

  bool HasMemoryOrdering(uint32_t mo) const {
    return (memory_ordering_ & mo) != 0;
  }

  virtual Operand *GetOffset() {
    return nullptr;
  }

  virtual bool Less(Operand *right) const override {
    CG_ASSERT(false, "CG internal error.");
    return false;
  }
};

class LabelOperand : public Operand {
 protected:
  const char *parent_func_;

 public:
  LabelIdx labidx_;

 private:
  // this index records the order this label is defined during code emit.
  LabelIDOrder order_id_ = -1u;

 public:
  explicit LabelOperand(const char *parent, LabelIdx labidx)
    : Operand(Opd_BbAddress, 0), parent_func_(parent), labidx_(labidx), order_id_(-1u) {}

  ~LabelOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<LabelOperand>(*this);
  }

  inline LabelIdx GetLabelIndex() const {
    return labidx_;
  }

  inline LabelIDOrder GetLabelOrder() const {
    return order_id_;
  }

  inline LabelIDOrder SetLabelOrder(LabelIDOrder idx) {
    return order_id_ = idx;
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override;

  void dump() override;

  virtual bool Less(Operand *right) const override {
    if (this == right) {
      return false;
    }

    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    LabelOperand *rop = static_cast<LabelOperand *>(right);

    int nRes = strcmp(parent_func_, rop->parent_func_);
    if (nRes == 0) {
      return labidx_ < rop->labidx_;
    } else {
      return nRes < 0;
    }
  }

  bool Equals(Operand *operand) override {
    if (!dynamic_cast<LabelOperand *>(operand)) {
      return false;
    }
    LabelOperand *op = static_cast<LabelOperand *>(operand);
    if (this == op || labidx_ == op->GetLabelIndex()) {
      return true;
    }
    return false;
  }
};

class ListOperand : public Operand {
 protected:
  MapleList<RegOperand *> vec_opnds_;

 public:
  explicit ListOperand(MapleAllocator *allocator) : Operand(Operand::Opd_List, 0), vec_opnds_(allocator->Adapter()) {}

  void PushOpnd(RegOperand *opnd) {
    vec_opnds_.push_back(opnd);
  }

  void PushFront(RegOperand *opnd) {
    vec_opnds_.push_front(opnd);
  }

  void remove_opnd(RegOperand *opnd) {
    vec_opnds_.remove(opnd);
  }

  MapleList<RegOperand *> &GetOperands() {
    return vec_opnds_;
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override = 0;

  void dump() override {
    for (auto regopnd : vec_opnds_) {
      regopnd->dump();
    }
  }

  virtual bool Less(Operand *right) const override {
    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    CG_ASSERT(false, "We don't need to compare list operand.");
    return false;
  }

  bool Equals(Operand *operand) override {
    if (!dynamic_cast<ListOperand *>(operand)) {
      return false;
    }
    ListOperand *op = static_cast<ListOperand *>(operand);
    if (this == op) {
      return true;
    }
    return false;
  }
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_OPERAND_H_
