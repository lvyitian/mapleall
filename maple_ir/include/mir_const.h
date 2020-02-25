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

#ifndef MAPLE_IR_INCLUDE_MIR_CONST_H
#define MAPLE_IR_INCLUDE_MIR_CONST_H
#include <math.h>
#include "mir_type.h"

namespace maple {
class MIRConst;
using MIRConstPtr = MIRConst*;
#if MIR_FEATURE_FULL
class MIRSymbol;
enum MIRConstKind {
  kConstInvalid,
  kConstInt,
  kConstVecInt,
  kConstAddrof,
  kConstAddrofFunc,
  kConstLblConst,
  kConstStrConst,
  kConstStr16Const,
  kConstFloatConst,
  kConstDoubleConst,
  kConstFloat128Const,
  kConstAggConst,
  kConstStConst
};

class MIRConst {
 public:
  MIRConstKind kind;
  MIRType *type;
  uint32 fieldID;
  explicit MIRConst(MIRType *type, uint32 fieldID = 0) : kind(kConstInvalid), type(type), fieldID(fieldID) {}

  virtual ~MIRConst() {}

  virtual void Dump() const;

  virtual bool IsZero() {
    return false;
  }

  virtual bool IsGeZero() {
    return false;
  }

  virtual bool IsNeg() {
    return false;
  }

  virtual bool IsOne() {
    return false;
  }

  virtual bool IsMagicNum() {
    return false;
  }

  // NO OP
  virtual void Neg() {}

  virtual bool operator==(MIRConst &rhs) const {
    return &rhs == this;
  }

  virtual bool IsAllBitsOne() {
    return false;
  }
};

class MIRIntConst : public MIRConst {
 public:
  using value_type = int64;
  int64 value;
  MIRIntConst(int64 val, MIRType *type, uint32 fieldID = 0) : MIRConst(type, fieldID), value(val) {
    if (!IsPrimitiveDynType(type->GetPrimType())) {
      Trunc(GetPrimTypeBitSize(type->GetPrimType()));
    }
    kind = kConstInt;
  }

  ~MIRIntConst() {}

  uint8 GetBitWidth() const {
    if (value == 0) {
      return 1;
    }
    uint8 width = 0;
    uint64 tmp = value < 0 ? -(value + 1) : value;
    while (tmp != 0) {
      width++;
      tmp = tmp >> 1u;
    }
    return width;
  }

  void Trunc(uint8 width) {
    int32 shiftBitNum = 64u - width;
    if (shiftBitNum < 0) {
      CHECK_FATAL(false, "shiftBitNum should not be less than zero");
    }
    uint32 unsignShiftBitNum = static_cast<uint32>(shiftBitNum);
    if (IsSignedInteger(type->GetPrimType())) {
      value = (value << unsignShiftBitNum) >> unsignShiftBitNum;
    } else {
      value = ((static_cast<uint64>(value)) << unsignShiftBitNum) >> unsignShiftBitNum;
    }
  }

  int64 GetValueUnderType() const {
    uint32 bitSize = GetPrimTypeBitSize(GetNonDynType(type->GetPrimType()));
    int32 shiftBitNum = 64u - bitSize;
    if (shiftBitNum < 0) {
      CHECK_FATAL(false, "shiftBitNum should not be less than zero");
    }
    if (IsSignedInteger(type->GetPrimType())) {
      return static_cast<int64>(((value) << shiftBitNum) >> shiftBitNum);
    } else {
      uint64 unsignedVal = static_cast<uint64>(value);
      return static_cast<int64>((unsignedVal << shiftBitNum) >> shiftBitNum);
    }
  }

  void Dump() const;
  bool IsZero() {
    return value == 0 && IsPrimitiveInteger(type->GetPrimType());
  }

  bool IsGeZero() {
    return value >= 0 && IsPrimitiveInteger(type->GetPrimType());
  }

  bool IsOne() {
    return value == 1 && IsPrimitiveInteger(type->GetPrimType());
  };
  bool IsMagicNum() {
    constexpr int64 kMagicNum = 51;
    return value == kMagicNum && IsPrimitiveInteger(type->GetPrimType());
  };
  bool IsAllBitsOne() {
    return value == -1 && IsPrimitiveInteger(type->GetPrimType());
  };
  void Neg() {
    value = -value;
  }

  bool operator==(MIRConst &rhs) const;
};

class MIRVectorIntConst : public MIRConst {
 static const uint32 kMaxVecSize = 16;
 public:
  int64 vecElems[kMaxVecSize];
  uint8 vecSize;

 public:
  MIRVectorIntConst(uint8 size, MIRType *ty) : MIRConst(ty), vecSize(size){
    kind = kConstVecInt;
  }
  void AddValue(int64 val, uint32 index) {
    CHECK_FATAL(index < vecSize, "wrong index");
    this->vecElems[index] = val;
  }
  void Dump() const;
};

class MIRAddrofConst : public MIRConst {
  StIdx stIdx;
  FieldID fldID;

 public:
  MIRAddrofConst(StIdx sy, FieldID fi, MIRType *ty) : MIRConst(ty), stIdx(sy), fldID(fi) {
    kind = kConstAddrof;
  }

  ~MIRAddrofConst() {}

  const StIdx &GetSymbolIndex() const {
    return stIdx;
  }

  FieldID GetFieldID() const {
    return fldID;
  }

  /* virtual */
  void Dump() const;
  /* virtual */
  bool operator==(MIRConst &rhs) const;
};

class MIRAddroffuncConst : public MIRConst {
  PUIdx puIdx;

 public:
  MIRAddroffuncConst(PUIdx idx, MIRType *ty, uint32 fieldID = 0) : MIRConst(ty, fieldID), puIdx(idx) {
    kind = kConstAddrofFunc;
  }

  ~MIRAddroffuncConst() {}

  PUIdx GetValue() const {
    return puIdx;
  }

  /* virtual */
  void Dump() const;
  /* virtual */
  bool operator==(MIRConst &rhs) const;
};

class MIRLblConst : public MIRConst {
 public:
  LabelIdx value;
  MIRLblConst(LabelIdx val, MIRType *type) : MIRConst(type), value(val) {
    kind = kConstLblConst;
  }

  ~MIRLblConst() {}

  bool operator==(MIRConst &rhs) const;
};

class MIRStrConst : public MIRConst {
 public:
  UStrIdx value;
  static const PrimType kPrimType = PTY_a64;
  MIRStrConst(UStrIdx val, MIRType *type, uint32 fieldID = 0) : MIRConst(type, fieldID), value(val) {
    kind = kConstStrConst;
  }

  MIRStrConst(const std::string &str, MIRType *type);

  ~MIRStrConst() {}

  void Dump() const;
  bool operator==(MIRConst &rhs) const;
};

class MIRStr16Const : public MIRConst {
 public:
  using value_type = const char*;
  static const PrimType kPrimType = PTY_a64;
  U16StrIdx value;
  MIRStr16Const(U16StrIdx val, MIRType *type) : MIRConst(type), value(val) {
    kind = kConstStr16Const;
  }

  MIRStr16Const(const std::u16string &str, MIRType *type);

  ~MIRStr16Const() {}

  void Dump() const;
  bool operator==(MIRConst &rhs) const;
};

class MIRFloatConst : public MIRConst {
 public:
  using value_type = float;
  static const PrimType kPrimType = PTY_f32;
  union {
    value_type floatValue;
    int32 intValue;
  } value;
  MIRFloatConst(float val, MIRType *type) : MIRConst(type) {
    value.floatValue = val;
    kind = kConstFloatConst;
  }

  ~MIRFloatConst() {}

  void SetFloatValue(float fvalue) {
    value.floatValue = fvalue;
  }

  value_type GetFloatValue() const {
    return value.floatValue;
  }

  int32 GetIntValue() const {
    return value.intValue;
  }

  value_type GetValue() const {
    return GetFloatValue();
  }

  void Dump() const;
  bool IsZero() {
    return fabs(value.floatValue) <= 1e-6;
  }

  bool IsGeZero() {
    return value.floatValue >= 0;
  }

  bool IsNeg() {
    return ((value.intValue & 0x80000000) == 0x80000000);
  }

  bool IsOne() {
    return value.floatValue == 1;
  };
  bool IsAllBitsOne() {
    return value.floatValue == -1;
  };
  void Neg() {
    value.floatValue = -value.floatValue;
  }

  bool operator==(MIRConst &rhs) const;
};

class MIRDoubleConst : public MIRConst {
 public:
  using value_type = double;
  static const PrimType kPrimType = PTY_f64;
  union {
    value_type dValue;
    int64 intValue;
  } value;
  MIRDoubleConst(double val, MIRType *type) : MIRConst(type) {
    value.dValue = val;
    kind = kConstDoubleConst;
  }

  ~MIRDoubleConst() {}

  uint32 GetIntLow32() const {
    uint64 unsignVal = static_cast<uint64>(value.intValue);
    return static_cast<uint32>(unsignVal & 0xffffffff);
  }

  uint32 GetIntHigh32() const {
    uint64 unsignVal = static_cast<uint64>(value.intValue);
    return static_cast<uint32>((unsignVal & 0xffffffff00000000) >> 32);
  }

  int64 GetIntValue() const {
    return value.intValue;
  }

  value_type GetValue() const {
    return value.dValue;
  }

  void Dump() const;
  bool IsZero() {
    return fabs(value.dValue) <= 1e-15;
  }

  bool IsGeZero() {
    return value.dValue >= 0;
  }

  bool IsNeg() {
    return ((value.intValue & 0x8000000000000000LL) == 0x8000000000000000LL);
  }

  bool IsOne() {
    return value.dValue == 1;
  };
  bool IsAllBitsOne() {
    return value.dValue == -1;
  };
  void Neg() {
    value.dValue = -value.dValue;
  }

  bool operator==(MIRConst &rhs) const;
};

class MIRFloat128Const : public MIRConst {
 public:
  static const PrimType kPrimType = PTY_f128;
  // value[0]: Low 64 bits; value[1]: High 64 bits.
  const uint64 *value;

  MIRFloat128Const(const uint64 *val, MIRType *type) : MIRConst(type) {
    MIR_ASSERT(val && "val must not nullptr!");
    value = val;
    kind = kConstFloat128Const;
  }

  ~MIRFloat128Const() {}

  bool IsZero() {
    MIR_ASSERT(value && "value must not be nullptr!");
    return value[0] == 0 && value[1] == 0;
  }

  bool IsOne() {
    MIR_ASSERT(value && "value must not be nullptr!");
    return value[0] == 0 && value[1] == 0x3FFF000000000000;
  };
  bool IsAllBitsOne() {
    MIR_ASSERT(value && "value must not be nullptr!");
    return (value[0] == 0xffffffffffffffff && value[1] == 0xffffffffffffffff);
  };
  bool operator==(MIRConst &rhs) const;
  void Dump() const;
};

class MIRAggConst : public MIRConst {
 public:
  MapleAllocator allocator;
  MapleVector<MIRConst*> constVec;
  MIRAggConst(MIRModule *mod, MIRType *type)
      : MIRConst(type), allocator(nullptr), constVec(mod->GetMPAllocator().Adapter()) {
    kind = kConstAggConst;
  }

  MIRAggConst(MIRModule *mod, MIRType *type, MemPool *memPool)
      : MIRConst(type), allocator(memPool), constVec(allocator.Adapter()) {
    kind = kConstAggConst;
  }

  ~MIRAggConst() {}

  MIRConst *GetAggConstElement(unsigned int fieldidx) {
    for (size_t i = 0; i < constVec.size(); ++i) {
      if (constVec[i] == nullptr) {
        CHECK_FATAL(false, "exist nullptr in constVec");
      }
      if (fieldidx == constVec[i]->fieldID) {
        return constVec[i];
      }
    }
    return nullptr;
  }

  void Dump() const;
  bool operator==(MIRConst &rhs) const;
};

// the const has one or more symbols
class MIRStConst : public MIRConst {
 public:
  MapleVector<MIRSymbol*> stVec;    // symbols that in the st const
  MapleVector<uint32> stOffsetVec;  // symbols offset
  MIRStConst(MIRModule *mod, MIRType *type)
      : MIRConst(type), stVec(mod->GetMPAllocator().Adapter()), stOffsetVec(mod->GetMPAllocator().Adapter()) {
    kind = kConstStConst;
  }

  ~MIRStConst() {}
};

#endif  // MIR_FEATURE_FULL
}  // namespace maple
#endif
