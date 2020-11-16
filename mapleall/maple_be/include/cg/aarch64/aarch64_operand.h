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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64OPERAND_H_
#define MAPLEBE_INCLUDE_CG_AARCH64OPERAND_H_

#include "aarch64_isa.h"
#include "operand.h"
#include "cg.h"
#include "aarch64_immediate.h"
#include "emit.h"
#include "cg_assert.h"

#include <iostream>
#include <limits>
#include <string>
#include <cmath>
#include <iomanip>

namespace maplebe {

using namespace maple;
using namespace std;

enum VectorType { kVecDouble = 0, kVecSingle = 1, kVecNone };

class AArch64RegOperand : public RegOperand {
 private:
  uint32 flag_;
  VectorType vector_type;  // double(2 * 64bits) or single(4 * 32bits)
  uint8 vector_pos;        // (0,1) for d, (0,3) for s
 public:
  static AArch64RegOperand zero64;
  static AArch64RegOperand zero32;
  bool isreffield_;
  explicit AArch64RegOperand(regno_t regNo, uint8 size, RegType kind, uint32 flag = 0, VectorType type = kVecNone,
                             uint8 position = 0)
    : RegOperand(regNo, size, kind, false), flag_(flag), vector_type(type), vector_pos(position), isreffield_(false) {
    CG_ASSERT(kind != kRegTyUndef, "Reg type must be specified");
  }

  ~AArch64RegOperand() {}

  inline uint32 GetFlags() {
    return flag_;
  }

  VectorType GetVectorType() {
    return vector_type;
  }

  bool IsPhysicalRegister() const {
    CG_ASSERT(GetRegisterNumber() < AArch64reg_t::kMaxRegNum, "");
    return (!IsVirtualRegister());
  }

  inline bool IsInvalidRegister() override {
    return (GetRegisterNumber() == AArch64reg_t::kRinvalid);
  }

  inline bool IsPhysicalRegister() override {
    return AArch64isa::IsPhysicalRegister(GetRegisterNumber());
  }

  bool IsSaveReg(MIRType *ty, BECommon &becommon) override;

  inline bool IsAsHigh32() {
    return flag_ & REGOPNDSETHIGH32;
  }

  inline AArch64reg_t GetPhysicalRegister() {
    CG_ASSERT(AArch64isa::IsPhysicalRegister(GetRegisterNumber()), "not a physical register");
    return (AArch64reg_t)GetRegisterNumber();
  }

  inline static AArch64RegOperand &Get32bitZeroRegister() {
    return zero32;
  }

  inline static AArch64RegOperand &Get64bitZeroRegister() {
    return zero64;
  }

  inline static AArch64RegOperand *GetZeroRegister(uint32 bitleng) {
    /* It is possible to have a bitleng < 32, eg stb.
     * Set it to 32 if it is less than 32.
     */
    if (bitleng < 32) {
      bitleng = 32;
    }
    CG_ASSERT((bitleng == 32 || bitleng == 64), "illegal bit length = %d", bitleng);
    return (bitleng == 32) ? &Get32bitZeroRegister() : &Get64bitZeroRegister();
  }

  bool IsZeroRegister() override {
    return GetRegisterNumber() == RZR;
  }

  bool IsSimdVectorMode() {
    return vector_type != kVecNone;
  }

  VectorType GetSimdVectorType() {
    return vector_type;
  }

  void SetSimdVectorType(VectorType v) {
    vector_type = v;
  }

  int GetSimdVectorPosition() {
    return static_cast<int>(vector_pos);
  }

  void SetSimdVectorPosition(uint8 v) {
    if (vector_type == kVecDouble) {
      CG_ASSERT(v < 2, "Simd FP index for d type out of bound");
    } else if (vector_type == kVecSingle) {
      CG_ASSERT(v < 4, "Simd FP index for s type out of bound");
    }
    vector_pos = v;
  }

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<AArch64RegOperand>(*this);
  }

  bool operator==(const AArch64RegOperand &o) const {
    regno_t myRn = GetRegisterNumber();
    uint32 mySz = GetSize();
    uint32 myFl = flag_;
    regno_t otherRn = o.GetRegisterNumber();
    uint32 otherSz = o.GetSize();
    uint32 otherFl = o.flag_;
    if (myRn != otherRn || mySz != otherSz || myFl != otherFl) {
      return false;
    }
    if (vector_type == kVecNone) {
      if (vector_type == o.vector_type) {
        return true;
      } else {
        return false;
      }
    } else {
      if (vector_type == o.vector_type && vector_pos == o.vector_pos) {
        return true;
      } else {
        return false;
      }
    }
  }

  bool IsCallArgumentPassingMachineRegister() const override {
    return (IsPhysicalRegister() && AArch64isa::IsArgumentPassingRegister(GetRegisterNumber()));
  }

  bool IsReturnValueMachineRegister() const override {
    return (IsPhysicalRegister() && (GetRegisterNumber() == R0 || GetRegisterNumber() == V0));
  }

  bool operator<(const AArch64RegOperand &o) const {
    // if kind_<o.kind_ return true;
    // assert kind_==o.kind_
    regno_t myRn = GetRegisterNumber();
    uint32 mySz = GetSize();
    uint32 myFl = flag_;
    VectorType myTy = vector_type;
    uint8 myPo = vector_pos;
    regno_t otherRn = o.GetRegisterNumber();
    uint32 otherSz = o.GetSize();
    uint32 otherFl = o.flag_;
    VectorType otherTy = o.vector_type;
    uint8 otherPo = o.vector_pos;
    return myRn < otherRn || (myRn == otherRn && mySz < otherSz) ||
           (myRn == otherRn && mySz == otherSz && myFl < otherFl) ||
           (myRn == otherRn && mySz == otherSz && myFl == otherFl && myTy < otherTy) ||
           (myRn == otherRn && mySz == otherSz && myFl == otherFl && myTy == otherTy && myPo < otherPo);
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override;
  bool IsSPOrFP() override;
};

/*
   http://stackoverflow.com/questions/30904718/range-of-immediate-values-in-armv8-a64-assembly

   Unlike A32's "flexible second operand", there is no common
   immediate format in A64. For immediate-operand data-processing
   instructions (ignoring the boring and straightforward ones like shifts),

   1. Arithmetic instructions (add{s}, sub{s}, cmp, cmn) take
     a 12-bit unsigned immediate with an optional 12-bit left shift.
   2. Move instructions (movz, movn, movk) take a 16-bit immediate
     optionally shifted to any 16-bit-aligned position within the register.
   3. Address calculations (adr, adrp) take a 21-bit signed immediate,
     although there's no actual syntax to specify it directly - to do
     so you'd have to resort to assembler expression trickery to generate
     an appropriate "label".
   4. Logical instructions (and{s}, orr, eor, tst) take a "bitmask immediate",
     which I'm not sure I can even explain, so I'll just quote the
     mind-bogglingly complicated definition:
     "Such an immediate is a 32-bit or 64-bit pattern viewed as a vector of
      identical elements of size e = 2, 4, 8, 16, 32, or 64 bits. Each element
      contains the same sub-pattern: a single run of 1 to e-1 non-zero bits,
      rotated by 0 to e-1 bits. This mechanism can generate 5,334 unique
      64-bit patterns (as 2,667 pairs of pattern and their bitwise inverse)."
 */
class AArch64ImmOperand : public ImmOperand {
 public:
  bool is_fmov_;
  explicit AArch64ImmOperand(int64 val, uint8 size, bool isSigned, bool isVary = false, bool isFmov = false)
    : ImmOperand(val, size, isSigned, isVary), is_fmov_(isFmov) {}

  ~AArch64ImmOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<AArch64ImmOperand>(*this);
  }

  inline bool IsBitmaskImmediate() {
    CG_ASSERT((!IsZero() && !IsAllOnes()), "0 and -1 are reserved for bitmask immediate");
    return maplebe::IsBitmaskImmediate(static_cast<uint64>(val_), static_cast<uint32>(size_));
  }

  inline bool IsSingleInstructionMovable() {
    return (maplebe::IsMoveWidableImmediate(static_cast<uint64>(val_), static_cast<uint32>(size_))     /* MOV wide immediate */
            || maplebe::IsMoveWidableImmediate(~static_cast<uint64>(val_), static_cast<uint32>(size_)) /* MOV inverted wide immediate */
            || IsBitmaskImmediate() /* MOV bitmask immediate */);
  }

  void Emit(Emitter &emitter, OpndProp *prop) override {
    if (!is_fmov_) {
      emitter.Emit((prop && static_cast<AArch64OpndProp *>(prop)->IsLoadLiteral()) ? "=" : "#")
        .Emit(size_ == 64 ? val_ : static_cast<int64>(static_cast<int32>(val_)));
    } else {
      int exp = ((((unsigned int)val_ & 0x70) >> 4) ^ 0x4) - 3;
      float mantissa = 1.0 + (static_cast<float>(val_ & 0xf) / 16.0);
      float result = std::pow(2, exp) * mantissa;
      std::stringstream ss;
      ss << std::setprecision(10) << result;
      std::string res;
      ss >> res;
      size_t dot = res.find('.');
      if (dot == std::string::npos) {
        res += ".0";
        dot = res.find('.');
        if (dot == std::string::npos) {
          CG_ASSERT(false, "cannot find in string");
        }
      }
      res.erase(dot, 1);
      std::string integer(res, 0, 1);
      std::string fraction(res, 1);
      while (fraction.size() != 1 && fraction[fraction.size() - 1] == '0') {
        fraction.pop_back();
      }
      std::string sign = static_cast<uint64>(val_) & 0x80 ? "-" : "";
      emitter.Emit(sign).Emit(integer).Emit(".").Emit(fraction).Emit("e+").Emit(dot - 1);
    }
  }
};

class ImmFPZeroOperand : public Operand {
  static ImmFPZeroOperand *instance32;
  static ImmFPZeroOperand *instance64;

 public:
  explicit ImmFPZeroOperand(uint32 sz) : Operand(Opd_FPZeroImmediate, uint8(sz)) {}

  ~ImmFPZeroOperand() {}

  static ImmFPZeroOperand *allocate(uint8 sz) {
    CG_ASSERT((sz == 32 || sz == 64), "half-precession is yet to be supported");
    ImmFPZeroOperand *&inst = (sz == 32) ? instance32 : instance64;
    //    if( !inst ) {
    MemPool *mp = CG::curCgFunc->memPool;
    inst = mp->New<ImmFPZeroOperand>(static_cast<uint32>(sz));
    //    }
    return inst;
  }

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<ImmFPZeroOperand>(*this);
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override {
    emitter.Emit("#0.0");
  }

  bool Less(Operand *right) const override {
    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    return false;
  }

  void dump() override {
    cout << "imm fp" << size_ << ": 0.0";
  }
};

class AArch64OfstOperand : public /*AArch64ImmOperand*/ OfstOperand {
 public:
  explicit AArch64OfstOperand(int32 val, uint8 size, bool isVary = false)
    : OfstOperand(int64(val), size, true, isVary) {}

  ~AArch64OfstOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<AArch64OfstOperand>(*this);
  }

  inline int32 GetOffsetValue() const {
    return GetValue();
  }

  inline void SetOffsetValue(int32 ov) {
    SetValue(static_cast<int64>(ov));
  }

  inline bool operator<(const AArch64OfstOperand &opnd) const {
    return GetValue() < opnd.GetValue();
  }

  inline bool operator==(const AArch64OfstOperand &opnd) const {
    return GetValue() == opnd.GetValue();
  }

  inline int32 operator-(const AArch64OfstOperand &opnd) const {
    return GetValue() - opnd.GetValue();
  }

  inline void AdjustOffset(int32 delta) {
    Add(static_cast<int64>(delta));
  }

  void Emit(Emitter &emitter, OpndProp *prop) override {
    emitter.Emit((prop && static_cast<AArch64OpndProp *>(prop)->IsLoadLiteral()) ? "=" : "#")
      .Emit(size_ == 64 ? val_ : static_cast<int64>(static_cast<int32>(val_)));
  }

  void dump() override {
    cout << "ofst:" << val_;
  }
};

class StImmOperand : public Operand  // representing for global variables address
{
  MIRSymbol *st_;
  int64 offset_;
  int32 relocs_;

 public:
  explicit StImmOperand(MIRSymbol *st, int64 offset, int32 relocs)
    : Operand(Opd_StImmediate, 0), st_(st), offset_(offset), relocs_(relocs) {}

  ~StImmOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<StImmOperand>(*this);
  }

  inline MIRSymbol *GetSymbol() {
    return st_;
  }

  inline const std::string &GetName() {
    return st_->GetName();
  }

  inline int64 GetOffset() {
    return offset_;
  }

  bool operator==(const StImmOperand &opnd) const {
    return (st_ == opnd.st_ && offset_ == opnd.offset_ && relocs_ == opnd.relocs_);
  }

  bool operator<(const StImmOperand &opnd) const {
    return st_ < opnd.st_ || (st_ == opnd.st_ && offset_ < opnd.offset_) ||
           (st_ == opnd.st_ && offset_ == opnd.offset_ && relocs_ < opnd.relocs_);
  }

  virtual bool Less(Operand *right) const override {
    if (this == right) {
      return false;
    }

    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    StImmOperand *rop = static_cast<StImmOperand *>(right);

    if (st_ != rop->st_) {
      return st_ < rop->st_;
    }
    if (offset_ != rop->offset_) {
      return offset_ < rop->offset_;
    }
    return relocs_ < rop->relocs_;
  }

  /* virtual */ void Emit(Emitter &emitter, OpndProp *opndprop) override {
    if (static_cast<AArch64OpndProp *>(opndprop)->IsLiteralLow12()) {
      emitter.Emit("#:lo12:");
    }
    if (CGOptions::doPIC && (st_->GetStorageClass() == kScGlobal || st_->GetStorageClass() == kScExtern)) {
      emitter.Emit(":got:" + GetName());
    } else if (st_->storageClass == kScPstatic && st_->sKind != kStConst && st_->IsLocal()) {
      emitter.Emit(GetName() + to_string(CG::curPuIdx));
    } else {
      emitter.Emit(GetName());
    }
    if (offset_ != 0) {
      emitter.Emit("+" + to_string(offset_));
    }
  }

  /* virtual */ void dump() override {
    cout << GetName();
    cout << "+offset:" << offset_;
  }
};

class FunctionLabelOperand : public LabelOperand {
 public:
  explicit FunctionLabelOperand(const char *func) : LabelOperand(func, 0) {}

  ~FunctionLabelOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<FunctionLabelOperand>(*this);
  }

  /* virtual */ void Emit(Emitter &emitter, OpndProp *opndprop) override {
    emitter.Emit(parent_func_);
  }

  /* virtual */ void dump() override {
    cout << "func :" << parent_func_;
  }
};

// Use StImmOperand instead?
class FuncNameOperand : public Operand {
  MIRSymbol *symbol_;

 public:
  explicit FuncNameOperand(MIRSymbol *fsym) : Operand(Opd_BbAddress, 0), symbol_(fsym) {}

  ~FuncNameOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<FuncNameOperand>(*this);
  }

  inline const std::string &GetName() {
    return symbol_->GetName();
  }

  MIRSymbol *GetFunctionSymbol() {
    return symbol_;
  }

  void SetFunctionSymbol(MIRSymbol *fsym) {
    symbol_ = fsym;
  }

  /* virtual */ void Emit(Emitter &emitter, OpndProp *opndprop) override {
    emitter.Emit(GetName());
  }

  virtual bool Less(Operand *right) const override {
    if (this == right) {
      return false;
    }

    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    FuncNameOperand *rop = static_cast<FuncNameOperand *>(right);

    return static_cast<void *>(symbol_) < static_cast<void *>(rop->symbol_);
  }

  /* virtual */ void dump() override {
    cout << GetName();
  }
};

class AArch64CGFunc;

class AArch64MemOperand : public MemOperand {
 public:
  AArch64MemOperand &operator=(const AArch64MemOperand &p) = default;
  enum AArch64AddressingMode {
    kAddrModeUndef,
    // AddrMode_BO,      // base, offset. EA = [base] + offset;
    kAddrModeBOi,  // INTACT: EA = [base]+immediate
    // PRE: base += immediate, EA = [base]
    // POST: EA = [base], base += immediate
    kAddrModeBOrX,  // EA = [base]+Extend([offreg/idxreg]), OR=Wn/Xn
    kAddrModeLiteral,  // AArch64 insruction LDR takes literal and
    // "calculates an address from the PC value and an immediate offset,
    //  loads a word from memory, and writes it to a register."
    kAddrModeLo12Li  // EA = [base] + #:lo12:Label+immediate. (Example: [x0, #:lo12:__Label300+456]
  };
  /*
     ARMv8-A A64 ISA Overview by Matteo Franchin @ ARM
     (presented at 64-bit ARM. Sep. 2015) p.14

     o Address to load from/store to is a 64-bit base register + an optional offset
       LDR X0, [X1] ; Load from address held in X1
       STR X0, [X1] ; Store to address held in X1

     o Offset can be an immediate or a register
       LDR X0, [X1, #8]  ; Load from address [X1 + 8 bytes]
       LDR X0, [X1, #-8] ; Load with negative offset
       LDR X0, [X1, X2]  ; Load from address [X1 + X2]

     o A Wn register offset needs to be extended to 64 bits
       LDR X0, [X1, W2, SXTW] ; Sign-extend offset in W2
       LDR X0, [X1, W2, UXTW] ; Zero-extend offset in W2

     o Both Xn and Wn register offsets can include an optional left-shift
       LDR X0, [X1, W2, UXTW #2] ; Zero-extend offset in W2 & left-shift by 2
       LDR X0, [X1, X2, LSL #2]  ; Left-shift offset in X2 by 2

     p.15
     Addressing Modes                       Analogous C Code
                                           int *intptr = ... // X1
                                           int out; // W0
     o Simple: X1 is not changed
       LDR W0, [X1]                        out = *intptr;
     o Offset: X1 is not changed
       LDR W0, [X1, #4]                    out = intptr[1];
     o Pre-indexed: X1 changed before load
       LDR W0, [X1, #4]! =|ADD X1,X1,#4    out = *(++intptr);
     |LDR W0,[X1]
     o Post-indexed: X1 changed after load
       LDR W0, [X1], #4  =|LDR W0,[X1]     out = *(intptr++);
     |ADD X1,X1,#4
   */
 public:
  enum ExtendInfo {
    kShiftZero = 0x1,
    kShiftOne = 0x2,
    kShiftTwo = 0x4,
    kShiftThree = 0x8,
    kUnsignedExtend = 0x10,
    kSignExtend = 0x20
  };

  enum IndexingOption {
    kIntact,     // base register stays the same
    kPreIndex,   // base register gets changed before load
    kPostIndex,  // base register gets changed after load
  };

 private:
  static const int32 kLdStSimmLowerBound = -256;
  static const int32 kLdStSimmUpperBound = 255;

  static const int32 kLdpStp32SimmLowerBound = -256;  // multiple of 4
  static const int32 kLdpStp32SimmUpperBound = 252;

  static const int32 kLdpStp64SimmLowerBound = -512;  // multiple of 8
  static const int32 kLdpStp64SimmUpperBound = 504;

  static const int32 kMaxPimm8 = 4095;
  static const int32 kMaxPimm16 = 8190;
  static const int32 kMaxPimm32 = 16380;
  static const int32 kMaxPimm64 = 32760;

  static const int32 kMaxPimms[4];

  AArch64AddressingMode addr_mode_;

  uint32_t extend_;  // used with offset register ; AddrMode_B_OR_X

  IndexingOption idx_opt_;  // used with offset immediate ; AddrMode_B_OI

  bool no_extend;

  bool isStackMem;

 public:
  explicit AArch64MemOperand(AArch64reg_t reg, int32 offset, int32 size, IndexingOption idxOpt = kIntact)
    : MemOperand(size, CG::curCgFunc->memPool->New<AArch64RegOperand>(reg, 64, kRegTyInt), nullptr,
                 CG::curCgFunc->memPool->New<AArch64OfstOperand>(offset, 32), nullptr),
      addr_mode_(kAddrModeBOi),
      extend_(0),
      idx_opt_(idxOpt),
      no_extend(false),
      isStackMem(false) {
    if (reg == RSP || reg == RFP)
      isStackMem = true;
  }

  explicit AArch64MemOperand(AArch64AddressingMode mode, int32 size, RegOperand *base, RegOperand *index,
                             Operand *offset, MIRSymbol *symbol)
    : MemOperand(size, base, index, offset, symbol),
      addr_mode_(mode),
      extend_(0),
      idx_opt_(kIntact),
      no_extend(false) {
    if (base->GetRegisterNumber() == RSP || base->GetRegisterNumber() == RFP)
      isStackMem = true;
  }

  explicit AArch64MemOperand(AArch64AddressingMode mode, int32 size, RegOperand *base, RegOperand *index,
                             Operand *offset, MIRSymbol *symbol, bool noextend)
    : MemOperand(size, base, index, offset, symbol),
      addr_mode_(mode),
      extend_(0),
      idx_opt_(kIntact),
      no_extend(noextend) {
    if (base->GetRegisterNumber() == RSP || base->GetRegisterNumber() == RFP)
      isStackMem = true;
  }

  explicit AArch64MemOperand(AArch64AddressingMode mode, int32 dsize, RegOperand *bo, RegOperand *io, int32 shift,
                             bool isSigned = false)
    : MemOperand(dsize, bo, io, nullptr, nullptr),
      addr_mode_(mode),
      extend_((isSigned ? kSignExtend : kUnsignedExtend) | (1 << shift)),
      idx_opt_(kIntact),
      no_extend(false) {

    if (bo->GetRegisterNumber() == RSP || bo->GetRegisterNumber() == RFP)
      isStackMem = true;
  }

  explicit AArch64MemOperand(AArch64AddressingMode mode, int32 dsize, MIRSymbol *sym)
    : MemOperand(dsize, nullptr, nullptr, nullptr, sym),
      addr_mode_(mode),
      extend_(0),
      idx_opt_(kIntact),
      no_extend(false) {
    CG_ASSERT(mode == kAddrModeLiteral, "This constructor version is supposed to be used with AddrMode_Literal only");
  }

  ~AArch64MemOperand() {}

  /*
     Copy constructor
   */
  AArch64MemOperand(const AArch64MemOperand &mo)
    : MemOperand(mo), addr_mode_(mo.addr_mode_), extend_(mo.extend_), idx_opt_(mo.idx_opt_), no_extend(mo.no_extend) {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<AArch64MemOperand>(*this);
  }

  inline AArch64AddressingMode GetAddrMode() {
    return addr_mode_;
  }

  inline const std::string &GetSymbolName() {
    return GetSymbol()->GetName();
  }

  inline void SetBaseRegister(AArch64RegOperand *br) {
    MemOperand::SetBaseRegister(br);
  }

  inline bool IsStackMem() {
    return isStackMem;
  }

  inline void SetStackMem(bool b) {
    isStackMem = b;
  }

  inline RegOperand *GetOffsetRegister() {
    return MemOperand::GetIndexRegister();
  }

  Operand *GetOffset() override;

  inline void SetOffsetRegister(AArch64RegOperand *osr) {
    MemOperand::SetIndexRegister(osr);
  }

  inline AArch64OfstOperand *GetOffsetImmediate() {
    return static_cast<AArch64OfstOperand *>(GetOffsetOperand());
  }

  inline void SetOffsetImmediate(OfstOperand *oo) {
    MemOperand::SetOffsetOperand(oo);
  }

  // Returns N where alignment == 2^N
  inline static int32 GetImmediateOffsetAlignment(uint32 dsize) {
    CG_ASSERT(8 <= dsize && dsize <= 64 && (dsize & (dsize - 1)) == 0, "");
    /* dsize==8: 0, dsize==16 : 1, dsize==32: 2, dsize==64: 3 */
    return __builtin_ctz(dsize) - 3;
  }

  inline static int32 GetMaxPIMM(uint32 dsize) {
    CG_ASSERT(8 <= dsize && dsize <= 64 && (dsize & (dsize - 1)) == 0, "");
    int alignment = GetImmediateOffsetAlignment(dsize);
    CG_ASSERT(0 <= alignment && alignment <= 3, "");
    return (kMaxPimms[alignment]);
  }

  inline bool IsOffsetMisaligned(uint32 dsize) {
    if (dsize == 128) {
      // hard coded for vector
      return false;
    }
    CG_ASSERT(8 <= dsize && dsize <= 64 && (dsize & (dsize - 1)) == 0, "");
    if (dsize == 8 || addr_mode_ != kAddrModeBOi) {
      return false;
    }
    AArch64OfstOperand *oo = GetOffsetImmediate();
    return ((oo->GetOffsetValue() & ((1 << GetImmediateOffsetAlignment(dsize)) - 1)) != 0);
  }

  static bool IsSIMMOffsetOutOfRange(int32 offset, bool is64bit, bool isLDSTPair) {
    if (isLDSTPair) {
      if (is64bit) {
        return (offset < kLdpStp64SimmLowerBound || offset > kLdpStp64SimmUpperBound);
      } else {
        return (offset < kLdpStp32SimmLowerBound || offset > kLdpStp32SimmUpperBound);
      }
    } else {
      return (offset < kLdStSimmLowerBound || offset > kLdStSimmUpperBound);
    }
  }

  static inline bool IsPIMMOffsetOutOfRange(int32 offset, uint32 dsize) {
    if (dsize == 128) {
      // hard coded for vector
      return false;
    }
    CG_ASSERT(8 <= dsize && dsize <= 64 && (dsize & (dsize - 1)) == 0, "");
    return (!(0 <= offset && offset <= GetMaxPIMM(dsize)));
  }

  inline bool operator<(const AArch64MemOperand &opnd) const {
    return addr_mode_ < opnd.addr_mode_ ||
           (addr_mode_ == opnd.addr_mode_ && GetBaseRegister() < opnd.GetBaseRegister()) ||
           (addr_mode_ == opnd.addr_mode_ && GetBaseRegister() == opnd.GetBaseRegister() &&
            GetIndexRegister() < opnd.GetIndexRegister()) ||
           (addr_mode_ == opnd.addr_mode_ && GetBaseRegister() == opnd.GetBaseRegister() &&
            GetIndexRegister() == opnd.GetIndexRegister() && GetOffsetOperand() < opnd.GetOffsetOperand()) ||
           (addr_mode_ == opnd.addr_mode_ && GetBaseRegister() == opnd.GetBaseRegister() &&
            GetIndexRegister() == opnd.GetIndexRegister() && GetOffsetOperand() == opnd.GetOffsetOperand() &&
            GetSymbol() < opnd.GetSymbol()) ||
           (addr_mode_ == opnd.addr_mode_ && GetBaseRegister() == opnd.GetBaseRegister() &&
            GetIndexRegister() == opnd.GetIndexRegister() && GetOffsetOperand() == opnd.GetOffsetOperand() &&
            GetSymbol() == opnd.GetSymbol() && GetSize() < opnd.GetSize());
  }

  virtual bool Less(Operand *right) const override {
    if (this == right) {
      return false;
    }

    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    AArch64MemOperand *rop = static_cast<AArch64MemOperand *>(right);

    if (addr_mode_ != rop->addr_mode_) {
      return addr_mode_ < rop->addr_mode_;
    }

    switch (addr_mode_) {
      case kAddrModeBOi: {
        CG_ASSERT(idx_opt_ == kIntact, "Should not compare pre/post index addressing.");

        RegOperand *baseReg = GetBaseRegister();
        RegOperand *rbaseReg = rop->GetBaseRegister();
        int nRet = baseReg->RegCompare(rbaseReg);
        if (nRet == 0) {
          Operand *ofstOpnd = GetOffsetOperand();
          Operand *rofstOpnd = rop->GetOffsetOperand();
          return ofstOpnd->Less(rofstOpnd);
        } else {
          return nRet < 0;
        }
      }
      case kAddrModeBOrX: {
        if (no_extend != rop->no_extend) {
          return no_extend;
        }

        if (!no_extend && extend_ != rop->extend_) {
          return extend_ < rop->extend_;
        }

        RegOperand *indexReg = GetIndexRegister();
        RegOperand *rindexReg = rop->GetIndexRegister();
        return (indexReg->Less(rindexReg));
      }
      case kAddrModeLiteral: {
        return static_cast<void *>(GetSymbol()) < static_cast<void *>(rop->GetSymbol());
        break;
      }
      case kAddrModeLo12Li: {
        if (GetSymbol() != rop->GetSymbol()) {
          return static_cast<void *>(GetSymbol()) < static_cast<void *>(rop->GetSymbol());
        }
        Operand *ofstOpnd = GetOffsetOperand();
        Operand *rofstOpnd = rop->GetOffsetOperand();
        return ofstOpnd->Less(rofstOpnd);
      }
      default:
        CG_ASSERT(false, "Internal error.");
        return false;
    }
  }

  bool NoAlias(AArch64MemOperand *rop) {
    if (addr_mode_ == kAddrModeBOi && rop->addr_mode_ == kAddrModeBOi && idx_opt_ == kIntact &&
        rop->idx_opt_ == kIntact) {
      RegOperand *baseReg = GetBaseRegister();
      RegOperand *rbaseReg = rop->GetBaseRegister();

      if (baseReg->GetRegisterNumber() == RFP || rbaseReg->GetRegisterNumber() == RFP) {
        Operand *ofstOpnd = GetOffsetOperand();
        Operand *rofstOpnd = rop->GetOffsetOperand();

        CG_ASSERT(ofstOpnd && rofstOpnd, "offset operand should not be null.");
        OfstOperand *ofst = static_cast<OfstOperand *>(ofstOpnd);
        OfstOperand *rofst = static_cast<OfstOperand *>(rofstOpnd);
        CG_ASSERT(ofst && rofst, "CG internal error, invalid type.");

        return (!ofst->ValueEquals(rofst));
      }
    }

    return false;
  }

  inline bool IsExtendedRegisterMode() {
    return addr_mode_ == kAddrModeBOrX;
  }

  inline bool SignedExtend() {
    return IsExtendedRegisterMode() && (extend_ & kSignExtend);
  }

  inline bool UnsignedExtend() {
    return IsExtendedRegisterMode() && !SignedExtend();
  }

  inline int32 ShiftAmount() {
    int scale = extend_ & 0xF;
    CG_ASSERT(IsExtendedRegisterMode(), "Just checking");
    return scale == 8 ? 3 : (scale == 4 ? 2 : (scale == 2 ? 1 : 0));
  }

  inline bool ShouldEmitExtend() {
    return !no_extend && ((extend_ & 0x3F) != 0);
  }

  inline bool IsIntactIndexed() {
    return idx_opt_ == kIntact;
  }

  inline bool IsPostIndexed() {
    return idx_opt_ == kPostIndex;
  }

  inline bool IsPreIndexed() {
    return idx_opt_ == kPreIndex;
  }

  inline std::string GetExtendAsString() {
    if (GetOffsetRegister()->GetSize() == 64) {
      return std::string("LSL");
    } else {
      return (extend_ & kSignExtend) ? std::string("SXTW") : std::string("UXTW");
    }
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override;

  void dump() override;

  // Return true if given operand has the same base reg and offset with this.
  bool Equals(Operand *operand) override;
  bool Equals(AArch64MemOperand *opnd);
};

class AArch64ListOperand : public ListOperand {
 public:
  explicit AArch64ListOperand(MapleAllocator *allocator) : ListOperand(allocator) {}

  ~AArch64ListOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<AArch64ListOperand>(*this);
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override;
};

class CondOperand : public Operand {
  AArch64CC_t cc;

 public:
  static const char *ccStrs[kCcLast];

 public:
  explicit CondOperand(AArch64CC_t cc) : Operand(Operand::Opd_Cond, 4), cc(cc) {}

  ~CondOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->New<CondOperand>(cc);
  }

  inline AArch64CC_t GetCode() {
    return cc;
  }

  /* virtual */ void Emit(Emitter &emitter, OpndProp *opndprop) override {
    emitter.Emit(ccStrs[cc]);
  }

  virtual bool Less(Operand *right) const override {
    if (this == right) {
      return false;
    }

    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    CondOperand *rop = static_cast<CondOperand *>(right);

    // The same type.
    if (cc == CC_AL || rop->cc == CC_AL) {
      return false;
    }
    return cc < rop->cc;
  }

  /* virtual */ void dump() override {
    cout << "CC: " << ccStrs[cc];
  }
};

// used with MOVK
class LogicalShiftLeftOperand : public Operand {
  static const int kMaxMovkLslEntries = 8;
  static const int kMaxAddsubLslEntries = 2;
  uint32 shift_amount;
  friend class AArch64CGFunc;
  friend class AArch64Peep;
  // Do not make the constructor public unless you are sure you know what you are doing.
  // Only AArch64CGFunc is supposed to create LogicalShiftLeftOperand objects
  // as part of initialization
  explicit LogicalShiftLeftOperand(uint32 amt, int bitlen)
    : Operand(Operand::Opd_Shift, bitlen /*4 or 6*/), shift_amount(amt) {}

  ~LogicalShiftLeftOperand() {}

 public:
  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<LogicalShiftLeftOperand>(*this);
  }

  /* virtual */ void Emit(Emitter &emitter, OpndProp *opndprop) override {
    emitter.Emit(" LSL #").Emit(shift_amount);
  }

  virtual bool Less(Operand *right) const override {
    if (this == right) {
      return false;
    }

    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    LogicalShiftLeftOperand *rop = static_cast<LogicalShiftLeftOperand *>(right);

    // The same type.
    return shift_amount < rop->shift_amount;
  }

  /* virtual */ void dump() override {
    cout << "LSL: " << shift_amount;
  }
};

class ExtendShiftOperand : public Operand {
 public:
  enum ExtendOp {
    SXTW,
  };

 private:
  ExtendOp extend_op;
  uint32 shift_amount;

 public:
  explicit ExtendShiftOperand(ExtendOp op, uint32 amt, int bitlen)
    : Operand(Operand::Opd_Extend, bitlen), extend_op(op), shift_amount(amt) {}

  ~ExtendShiftOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<ExtendShiftOperand>(*this);
  }

  void Emit(Emitter &emitter, OpndProp *prop) override {
    switch (extend_op) {
      case SXTW:
        emitter.Emit("SXTW #").Emit(shift_amount);
        break;
      default:
        CHECK_FATAL(false, "should not be here");
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

    ExtendShiftOperand *rop = static_cast<ExtendShiftOperand *>(right);

    // The same type.
    if (extend_op != rop->extend_op) {
      return extend_op < rop->extend_op;
    }
    return shift_amount < rop->shift_amount;
  }

  void dump() override {
    switch (extend_op) {
      case SXTW:
        cout << "SXTW: ";
        break;
      default:
        CHECK_FATAL(false, "should not be here");
    }
    cout << shift_amount;
  }
};

class BitShiftOperand : public Operand {
 public:
  enum ShiftOp {
    LSL, /* logical shift left */
    LSR, /* logical shift right */
    ASR, /* arithmetic shift right */
  };

 private:
  ShiftOp shift_op;
  uint32 shift_amount;

 public:
  explicit BitShiftOperand(ShiftOp op, uint32 amt, int bitlen)
    : Operand(Operand::Opd_Shift, bitlen /*5 or 6*/), shift_op(op), shift_amount(amt) {}

  ~BitShiftOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<BitShiftOperand>(*this);
  }

  /* virtual */ void Emit(Emitter &emitter, OpndProp *prop) override {
    emitter.Emit((shift_op == LSL) ? "LSL #" : ((shift_op == LSR) ? "LSR #" : "ASR #")).Emit(shift_amount);
  }

  virtual bool Less(Operand *right) const override {
    if (this == right) {
      return false;
    }

    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    BitShiftOperand *rop = static_cast<BitShiftOperand *>(right);

    // The same type.
    if (shift_op != rop->shift_op) {
      return shift_op < rop->shift_op;
    }
    return shift_amount < rop->shift_amount;
  }

  uint32 GetShiftAmount() {
    return shift_amount;
  }

  /* virtual */ void dump() override {
    cout << ((shift_op == LSL) ? "LSL: " : ((shift_op == LSR) ? "LSR: " : "ASR: "));
    cout << shift_amount;
  }
};

class CommentOperand : public Operand {
  const char *comment;

 public:
  explicit CommentOperand(const char *s) : Operand(Operand::Opd_String, 0), comment(s) {}

  explicit CommentOperand(const std::string &s) : Operand(Operand::Opd_String, 0), comment(s.c_str()) {}

  ~CommentOperand() {}

  inline const char *GetComment() {
    return comment;
  }

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<CommentOperand>(*this);
  }

  /* virtual */ void Emit(Emitter &emitter, OpndProp *opndprop) override {
    emitter.Emit(comment);
  }

  virtual bool Less(Operand *right) const override {
    // For different type.
    if (op_kind_ != right->op_kind_) {
      return op_kind_ < right->op_kind_;
    }

    return false;
  }

  /* virtual */ void dump() override {
    cout << "# " << comment << endl;
  }
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_AARCH64OPERAND_H_
