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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64OPERAND_H_
#define MAPLEBE_INCLUDE_CG_AARCH64OPERAND_H_

#include "riscv64_isa.h"
#include "operand.h"
#include "cg.h"
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

class Riscv64RegOperand : public RegOperand {
 private:
  uint32 flag_;
  VectorType vector_type;  // double(2 * 64bits) or single(4 * 32bits)
  uint8 vector_pos;        // (0,1) for d, (0,3) for s
 public:
  static Riscv64RegOperand zero64;
  static Riscv64RegOperand zero32;
  bool isreffield_;
  explicit Riscv64RegOperand(regno_t regNo, uint8 size, RegType kind, uint32 flag = 0, VectorType type = kVecNone,
                             uint8 position = 0)
    : RegOperand(regNo, size, kind, false), flag_(flag), vector_type(type), vector_pos(position), isreffield_(false) {
    CG_ASSERT(kind != kRegTyUndef, "Reg type must be specified");
  }

  ~Riscv64RegOperand() {}

  inline uint32 GetFlags() {
    return flag_;
  }

  VectorType GetVectorType() {
    return vector_type;
  }

  bool IsPhysicalRegister() const {
    CG_ASSERT(GetRegisterNumber() < Riscv64reg_t::kMaxRegNum, "");
    return (!IsVirtualRegister());
  }

  inline bool IsInvalidRegister() override {
    return (GetRegisterNumber() == Riscv64reg_t::kRinvalid);
  }

  inline bool IsPhysicalRegister() override {
    return Riscv64isa::IsPhysicalRegister(GetRegisterNumber());
  }

  bool IsSaveReg(MIRType *ty, BECommon &becommon) override;

  inline bool IsAsHigh32() {
    return flag_ & REGOPNDSETHIGH32;
  }

  inline Riscv64reg_t GetPhysicalRegister() {
    CG_ASSERT(Riscv64isa::IsPhysicalRegister(GetRegisterNumber()), "not a physical register");
    return (Riscv64reg_t)GetRegisterNumber();
  }

  inline static Riscv64RegOperand &Get32bitZeroRegister() {
    return zero32;
  }

  inline static Riscv64RegOperand &Get64bitZeroRegister() {
    return zero64;
  }

  inline static Riscv64RegOperand *GetZeroRegister(uint32 bitleng) {
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
    return GetRegisterNumber() == RZERO;
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
    return mp->Clone<Riscv64RegOperand>(*this);
  }

  bool operator==(const Riscv64RegOperand &o) const {
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
    return (IsPhysicalRegister() && Riscv64isa::IsArgumentPassingRegister(GetRegisterNumber()));
  }

  bool IsReturnValueMachineRegister() const override {
    return (IsPhysicalRegister() && (GetRegisterNumber() == R0 || GetRegisterNumber() == V0));
  }

  bool operator<(const Riscv64RegOperand &o) const {
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

class Riscv64ImmOperand : public ImmOperand {
 public:
  bool is_fmov_;
  bool isUpperBits;
  explicit Riscv64ImmOperand(int64 val, uint8 size, bool isSigned, bool isVary = false, bool isFmov = false, bool isUpperBits = false)
    : ImmOperand(val, size, isSigned, isVary), is_fmov_(isFmov), isUpperBits(isUpperBits) {}

  ~Riscv64ImmOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<Riscv64ImmOperand>(*this);
  }

  bool IsInSimmBitSize(uint8 size) {
    if (IsNegative()) {
      if (IsInBitSize(size)) {
        return true;
      }
    } else {
      if (IsInBitSize(size - 1)) {
        return true;
      }
    }
    return false;
  }

  inline bool IsSingleInstructionMovable() {
    return (static_cast<uint64>(val_) & 0x0fffULL) == static_cast<uint64>(val_);
  }

  void Emit(Emitter &emitter, OpndProp *prop) override {
    if (!is_fmov_) {
      if (prop && static_cast<Riscv64OpndProp *>(prop)->IsLoadLiteral()) {
        emitter.Emit("=");
      }
      if (is_signed_) {
        if (size_ == 64) {
          emitter.Emit(val_);
        } else {
          emitter.Emit(static_cast<int64>(static_cast<int32>(val_)));
        }
      } else {
        if (size_ == 64) {
          emitter.Emit(static_cast<uint64>(val_));
        } else {
          emitter.Emit(static_cast<uint32>(val_));
        }
      }
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
    emitter.Emit("0.0");
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

class Riscv64OfstOperand : public /*Riscv64ImmOperand*/ OfstOperand {
 public:
  explicit Riscv64OfstOperand(int32 val, uint8 size, bool isVary = false)
    : OfstOperand(int64(val), size, true, isVary) {}

  ~Riscv64OfstOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<Riscv64OfstOperand>(*this);
  }

  inline int32 GetOffsetValue() const {
    return GetValue();
  }

  inline void SetOffsetValue(int32 ov) {
    SetValue(static_cast<int64>(ov));
  }

  inline bool operator<(const Riscv64OfstOperand &opnd) const {
    return GetValue() < opnd.GetValue();
  }

  inline bool operator==(const Riscv64OfstOperand &opnd) const {
    return GetValue() == opnd.GetValue();
  }

  inline int32 operator-(const Riscv64OfstOperand &opnd) const {
    return GetValue() - opnd.GetValue();
  }

  inline void AdjustOffset(int32 delta) {
    Add(static_cast<int64>(delta));
  }

  void Emit(Emitter &emitter, OpndProp *prop) override {
    if (prop && static_cast<Riscv64OpndProp *>(prop)->IsLoadLiteral()) {
      emitter.Emit("=");
    }
    emitter.Emit(size_ == 64 ? val_ : static_cast<int64>(static_cast<int32>(val_)));
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
    bool isLower12 = static_cast<Riscv64OpndProp *>(opndprop)->IsLiteralLow12();
    if (isLower12) {
      emitter.Emit("%lo(");
    }
    if (CGOptions::doPIC && (st_->GetStorageClass() == kScGlobal || st_->GetStorageClass() == kScExtern)) {
      emitter.Emit(":got:" + GetName());
    } else {
      if (isLower12 == false) {
        emitter.Emit("%hi(");
      }
      // check for sKind since it might be created by cg. (i.eg. LB_*)
      if (st_->storageClass == kScPstatic && st_->sKind != kStConst && st_->IsLocal()) {
        emitter.Emit(GetName() + to_string(CG::curPuIdx));
      } else {
        emitter.Emit(GetName());
      }
    }
    if (offset_ != 0) {
      emitter.Emit("+" + to_string(offset_));
    }
    emitter.Emit(")");
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

class Riscv64CGFunc;

class Riscv64MemOperand : public MemOperand {
 public:
  Riscv64MemOperand &operator=(const Riscv64MemOperand &p) = default;

 private:
  static const int32 kMaxImm11 = 2047;  // 2048 - 1 -> 0x7ff

  bool isStackMem;

 public:
  explicit Riscv64MemOperand(Riscv64reg_t reg, int32 offset, int32 size)
    : MemOperand(size, CG::curCgFunc->memPool->New<Riscv64RegOperand>(reg, 64, kRegTyInt), nullptr,
                 CG::curCgFunc->memPool->New<Riscv64OfstOperand>(offset, 32), nullptr),
      isStackMem(false) {
    if (reg == RSP || reg == RFP)
      isStackMem = true;
  }

  explicit Riscv64MemOperand(int32 size, RegOperand *base, RegOperand *index,
                             Operand *offset, MIRSymbol *symbol)
    : MemOperand(size, base, index, offset, symbol) {
    if (base->GetRegisterNumber() == RSP || base->GetRegisterNumber() == RFP)
      isStackMem = true;
  }

  ~Riscv64MemOperand() {}

  /*
     Copy constructor
   */
  Riscv64MemOperand(const Riscv64MemOperand &mo)
    : MemOperand(mo) {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<Riscv64MemOperand>(*this);
  }

  inline const std::string &GetSymbolName() {
    return GetSymbol()->GetName();
  }

  inline void SetBaseRegister(Riscv64RegOperand *br) {
    MemOperand::SetBaseRegister(br);
  }

  inline bool IsStackMem() {
    return isStackMem;
  }

  inline void SetStackMem(bool b) {
    isStackMem = b;
  }

  Operand *GetOffset() override;

  inline Riscv64OfstOperand *GetOffsetImmediate() {
    return static_cast<Riscv64OfstOperand *>(GetOffsetOperand());
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
    return kMaxImm11;
  }

  inline bool IsOffsetMisaligned(uint32 dsize) {
    if (dsize == 128) {
      // hard coded for vector
      return false;
    }
    CG_ASSERT(8 <= dsize && dsize <= 64 && (dsize & (dsize - 1)) == 0, "");
    if (dsize == 8) {
      return false;
    }
    Riscv64OfstOperand *oo = GetOffsetImmediate();
    return ((oo->GetOffsetValue() & ((1 << GetImmediateOffsetAlignment(dsize)) - 1)) != 0);
  }

  static inline bool IsPIMMOffsetOutOfRange(int32 offset, uint32 dsize) {
    if (dsize == 128) {
      // hard coded for vector
      return false;
    }
    CG_ASSERT(8 <= dsize && dsize <= 64 && (dsize & (dsize - 1)) == 0, "");
    return (!(0 <= offset && offset <= GetMaxPIMM(dsize)));
  }

  inline bool operator<(const Riscv64MemOperand &opnd) const {
    return (GetBaseRegister() < opnd.GetBaseRegister()) ||
           (GetBaseRegister() == opnd.GetBaseRegister() && GetOffsetOperand() < opnd.GetOffsetOperand()) ||
           (GetBaseRegister() == opnd.GetBaseRegister() && GetOffsetOperand() == opnd.GetOffsetOperand() &&
            GetSymbol() < opnd.GetSymbol()) ||
           (GetBaseRegister() == opnd.GetBaseRegister() && GetOffsetOperand() == opnd.GetOffsetOperand() &&
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

    Riscv64MemOperand *rop = static_cast<Riscv64MemOperand *>(right);

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

  bool NoAlias(Riscv64MemOperand *rop) {
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

    return false;
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override;

  void dump() override;

  // Return true if given operand has the same base reg and offset with this.
  bool Equals(Operand *operand) override;
  bool Equals(Riscv64MemOperand *opnd);
};

class Riscv64ListOperand : public ListOperand {
 public:
  explicit Riscv64ListOperand(MapleAllocator *allocator) : ListOperand(allocator) {}

  ~Riscv64ListOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<Riscv64ListOperand>(*this);
  }

  void Emit(Emitter &emitter, OpndProp *opndprop) override;
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
