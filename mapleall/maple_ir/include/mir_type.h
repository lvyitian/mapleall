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

#ifndef MAPLE_IR_INCLUDE_MIR_TYPE_H
#define MAPLE_IR_INCLUDE_MIR_TYPE_H
#include <algorithm>
#include <array>
#include "prim_types.h"
#include "mir_pragma.h"
#include "mpl_logging.h"
#if MIR_FEATURE_FULL
#include "mempool.h"
#include "mempool_allocator.h"
#endif  // MIR_FEATURE_FULL

#define POINTER_SIZE 8
#define POINTER_P2SIZE 3

namespace maple {

class FieldAttrs;
using TyidxFieldAttrPair =  std::pair<TyIdx, FieldAttrs>;
using FieldPair = std::pair<GStrIdx, TyidxFieldAttrPair>;
using FieldVector = std::vector<FieldPair>;

#define kMaxArrayDim 10
#define JSTRTYPENAME "constStr"
#if MIR_FEATURE_FULL
extern bool VerifyPrimType(PrimType pty1, PrimType pty2);  // verify if pty1 and pty2 match
extern uint32 GetPrimTypeSize(PrimType pty);               // answer in bytes; 0 if unknown
extern uint32 GetPrimTypeP2Size(PrimType pty);             // answer in bytes in power-of-two.
extern const char *GetPrimTypeName(PrimType ty);
extern const char *GetPrimTypeJavaName(PrimType ty);

inline uint32 GetPrimTypeBitSize(PrimType pty) {
  return GetPrimTypeSize(pty) << 3;
}

#endif  // MIR_FEATURE_FULL

// return the same type with size increased to register size
PrimType GetRegPrimType(PrimType pty);
PrimType GetDynType(PrimType primType);
PrimType GetNonDynType(PrimType primType);

inline bool IsAddress(PrimitivType primitiveType) {
  return primitiveType.IsAddress();
}

inline bool IsPossible64BitAddress(PrimType tp) {
  return (tp == PTY_ptr || tp == PTY_ref || tp == PTY_u64 || tp == PTY_a64);
}

inline bool IsPossible32BitAddress(PrimType tp) {
  return (tp == PTY_ptr || tp == PTY_ref || tp == PTY_u32 || tp == PTY_a32);
}

inline bool IsPrimitivePureScalar(PrimitivType primitiveType) {
  return primitiveType.IsInteger() && !primitiveType.IsAddress() && !primitiveType.IsDynamic();
}

inline bool IsUnsignedInteger(PrimitivType primitiveType) {
  return primitiveType.IsInteger() && primitiveType.IsUnsigned() && !primitiveType.IsDynamic();
}

inline bool IsSignedInteger(PrimitivType primitiveType) {
  return primitiveType.IsInteger() && !primitiveType.IsUnsigned() && !primitiveType.IsDynamic();
}

inline bool IsPrimitiveInteger(PrimitivType primitiveType) {
  return primitiveType.IsInteger() && !primitiveType.IsDynamic();
}

inline bool IsPrimitiveDynType(PrimitivType primitiveType) {
  return primitiveType.IsDynamic();
}

inline bool IsPrimitiveDynInteger(PrimitivType primitiveType) {
  return primitiveType.IsDynamic() && primitiveType.IsInteger();
}

inline bool IsPrimitiveDynFloat(PrimitivType primitiveType) {
  return primitiveType.IsDynamic() && primitiveType.IsFloat();
}

inline PrimType GetVectorPrimType (PrimType tp, uint32 vecSize) {
  switch (tp) {
    case PTY_i8: {
      if (vecSize == 16)
        return PTY_v16i8;
      else
        CHECK_FATAL(false, "NYI or error vector size");
    }
    case PTY_i16: {
      if (vecSize == 8)
        return PTY_v8i16;
      else
        CHECK_FATAL(false, "NYI or error vector size");
    }
    case PTY_i32: {
      if (vecSize == 4)
        return PTY_v4i32;
      else
        CHECK_FATAL(false, "NYI or error vector size");
    }
    case PTY_i64: {
      if (vecSize == 2)
        return PTY_v2i64;
      else
        CHECK_FATAL(false, "NYI or error vector size");
    }
    case PTY_f32: {
      if (vecSize == 4)
        return PTY_v4f32;
      else
        CHECK_FATAL(false, "NYI or error vector size");
    }
    case PTY_f64: {
      if (vecSize == 8)
        return PTY_v2f64;
      else
        CHECK_FATAL(false, "NYI or error vector size");
    }
    default:
      CHECK_FATAL(false, "NYI or error type");
  }
}

inline bool IsPrimitiveVectorInt(PrimType tp){
  return (tp == PTY_v4i32 || tp == PTY_v8i16 || tp == PTY_v2i64 ||tp == PTY_v16i8);
}

inline bool IsPrimitiveVectorFloat(PrimType tp) {
  return (tp == PTY_v4f32 || tp == PTY_v2f64);
}

inline bool IsPrimitiveFloat(PrimitivType primitiveType) {
  return primitiveType.IsFloat() && !primitiveType.IsDynamic();
}

inline bool IsPrimitiveScalar(PrimitivType primitiveType) {
  return primitiveType.IsInteger() || primitiveType.IsFloat() ||
         (primitiveType.IsDynamic() && !primitiveType.IsDynamicNone()) ||
         primitiveType.IsSimple();
}

inline bool IsPrimitiveValid(PrimitivType primitiveType) {
  return IsPrimitiveScalar(primitiveType) && !primitiveType.IsDynamicAny();
}

inline bool IsPrimitivePoint(PrimitivType primitiveType) {
  return primitiveType.IsPointer();
}

bool IsNoCvtNeeded(PrimType toType, PrimType fromType);

inline bool IsPrimitiveVector(PrimType tp) {
  return (tp == PTY_v4i32 || tp == PTY_v8i16 || tp == PTY_v2i64 || tp == PTY_v16i8 ||
              tp == PTY_v4f32 || tp == PTY_v2f64);
}

inline bool IsRefOrPtrAssign(PrimType toType, PrimType fromType) {
  return (toType == PTY_ref && fromType == PTY_ptr) || (toType == PTY_ptr && fromType == PTY_ref);
}

enum MIRTypeKind : std::uint8_t {
  kTypeInvalid,
  kTypeUnknown,
  kTypeScalar,
  kTypeBitField,
  kTypeArray,
  kTypeFArray,
  kTypeJArray,
  kTypeStruct,
  kTypeUnion,
  kTypeClass,
  kTypeInterface,
  kTypeStructIncomplete,
  kTypeClassIncomplete,
  kTypeConstString,
  kTypeInterfaceIncomplete,
  kTypePointer,
  kTypeFunction,
  kTypeVoid,
  kTypeByName,          // type definition not yet seen
  kTypeParam,           // to support java generics
  kTypeInstantVector,   // represent a vector of instantiation pairs
  kTypeGenericInstant,  // type to be formed by instantiation of a generic type
};

enum AttrKind : unsigned {
#define TYPE_ATTR
#define ATTR(STR) ATTR_##STR,
#include "all_attributes.def"
#undef ATTR
#undef TYPE_ATTR
};

class TypeAttrs {
 public:
  uint64 attrFlag : 61;
  uint8 attrAlign : 3;  // alignment in bytes is 2 to the power of attrAlign
 public:
  TypeAttrs() : attrFlag(0), attrAlign(0) {}

  TypeAttrs &operator=(const TypeAttrs &t) = default;
  TypeAttrs(const TypeAttrs &ta) : attrFlag(ta.attrFlag), attrAlign(ta.attrAlign) {}

  void SetAttr(AttrKind x) {
    attrFlag |= (1ULL << x);
  }

  void ResetAttr(AttrKind x) {
    attrFlag &= ~(1ULL << x);
  }

  bool GetAttr(AttrKind x) const {
    return (attrFlag & (1ULL << x)) != 0;
  }

  void SetAlign(uint32 x) {
    ASSERT((~(x - 1) & x) == x, "SetAlign called with non-power-of-2");
    attrAlign = 0;
    while (x != 1) {
      x >>= 1;
      attrAlign++;
    }
  }

  uint32 GetAlign(void) const {
    if (attrAlign == 0) {
      return 1;
    }
    uint32 res = 1;
    uint32 exp = attrAlign;
    do {
      exp--;
      res *= 2;
    } while (exp != 0);
    return res;
    // return 1 << attrAlign;
  }

  bool operator==(const TypeAttrs &tA) const {
    return attrFlag == tA.attrFlag && attrAlign == tA.attrAlign;
  }

  bool operator!=(const TypeAttrs &tA) const {
    return !(*this == tA);
  }

  void DumpAttributes() const;
};

enum FldAttrKind {
#define FIELD_ATTR
#define ATTR(STR) FLDATTR_##STR,
#include "all_attributes.def"
#undef ATTR
#undef FIELD_ATTR
};

class FieldAttrs {
 public:
  uint32 attrFlag : 29;
  uint8 attrAlign : 3;  // alignment in bytes is 2 to the power of attrAlign
 public:
  FieldAttrs() : attrFlag(0), attrAlign(0) {}

  ~FieldAttrs() {}

  FieldAttrs &operator=(const FieldAttrs &p) = default;
  FieldAttrs(const FieldAttrs &ta) : attrFlag(ta.attrFlag), attrAlign(ta.attrAlign) {}

  void SetAttr(FldAttrKind x) {
    attrFlag |= (1 << x);
  }

  void ClearAttr(FldAttrKind x) {
    attrFlag &= (~(1 << x));
  }

  bool GetAttr(FldAttrKind x) const {
    return (attrFlag & (1 << x)) != 0;
  }

  void SetAlign(uint32 x) {
    ASSERT((~(x - 1) & x) == x, "SetAlign called with non-power-of-2");
    attrAlign = 0;
    while (x != 1) {
      x >>= 1;
      attrAlign++;
    }
  }

  uint32 GetAlign(void) const {
    if (attrAlign == 0) {
      return 1;
    }
    uint32 res = 1;
    uint32 exp = attrAlign;
    do {
      exp--;
      res *= 2;
    } while (exp != 0);
    return res;
    // return 1 << attrAlign;
  }

  bool operator==(const FieldAttrs &tA) const {
    return attrFlag == tA.attrFlag && attrAlign == tA.attrAlign;
  }

  bool operator!=(const FieldAttrs &tA) const {
    return !(*this == tA);
  }

  void DumpAttributes() const;
  TypeAttrs ConvertToTypeAttrs();
};

enum FuncAttrKind : unsigned {
#define FUNC_ATTR
#define ATTR(STR) FUNCATTR_##STR,
#include "all_attributes.def"
#undef ATTR
#undef FUNC_ATTR
};

class FuncAttrs {
 public:
  uint64 attrFlag;

 public:
  FuncAttrs() : attrFlag(0) {}

  FuncAttrs(const FuncAttrs &ta) : attrFlag(ta.attrFlag) {}

  FuncAttrs &operator=(const FuncAttrs &p) = default;
  void SetAttr(FuncAttrKind x, bool unSet = false) {
    if (unSet == false) {
      attrFlag |= (1ULL << x);
    } else {
      attrFlag &= ~(1ULL << x);
    }
  }

  bool GetAttr(FuncAttrKind x) const {
    return (attrFlag & (1ULL << x)) != 0;
  }

  bool operator==(const FuncAttrs &tA) const {
    return attrFlag == tA.attrFlag;
  }

  bool operator!=(const FuncAttrs &tA) const {
    return !(*this == tA);
  }

  void DumpAttributes() const;
};

// only for internal use, not emitted
enum GenericAttrKind {
#define FUNC_ATTR
#define TYPE_ATTR
#define FIELD_ATTR
#define ATTR(STR) GENATTR_##STR,
#include "all_attributes.def"
#undef ATTR
#undef FUNC_ATTR
#undef TYPE_ATTR
#undef FIELD_ATTR
};

class GenericAttrs {
 public:
  uint64 attrFlag;

 public:
  GenericAttrs &operator=(const GenericAttrs &p) = default;
  GenericAttrs() : attrFlag(0) {}

  GenericAttrs(const GenericAttrs &ta) : attrFlag(ta.attrFlag) {}

  void SetAttr(GenericAttrKind x) {
    attrFlag |= (1ULL << x);
  }

  bool GetAttr(GenericAttrKind x) const {
    return (attrFlag & (1ULL << x)) != 0;
  }

  bool operator==(const GenericAttrs &tA) const {
    return attrFlag == tA.attrFlag;
  }

  bool operator!=(const GenericAttrs &tA) const {
    return !(*this == tA);
  }

  FieldAttrs ConvertToFieldAttrs();
  TypeAttrs ConvertToTypeAttrs();
  FuncAttrs ConvertToFuncAttrs();
};

#if MIR_FEATURE_FULL
constexpr int kShiftNumOfTypeKind = 8;
constexpr int kShiftNumOfNameStrIdx = 6;

class MIRStructType;

class MIRType {
 public:
  MIRTypeKind typeKind : 8;
  PrimType primType : 8;
  bool nameIsLocal : 1;  // needed when printing the type name
  TyIdx tyIdx;
  GStrIdx nameStrIdx;  // name in global string table
  MIRType(MIRTypeKind kind, PrimType primtype)
    : typeKind(kind), primType(primtype), nameIsLocal(false), nameStrIdx(0) {}

  MIRType(MIRTypeKind kind, PrimType primtype, GStrIdx strIdx)
    : typeKind(kind), primType(primtype), nameIsLocal(false), tyIdx(0), nameStrIdx(strIdx) {}

  virtual ~MIRType() {}

  virtual void Dump(int indent, bool dontUseName = false) const;
  virtual void DumpAsCxx(int indent) const;

  virtual bool Equalto(const MIRType &p) const;
  virtual MIRType *CopyMIRTypeNode() const {
    return new MIRType(*this);
  }

  PrimType GetPrimType() const {
    return primType;
  }

  void SetPrimType(PrimType pty) {
    primType = pty;
  }

  inline TyIdx GetTypeIndex() {
    return tyIdx;
  }

  MIRTypeKind GetKind() const {
    return typeKind;
  }

  virtual size_t GetSize() const {
    return GetPrimTypeSize(primType);
  }

  virtual uint8 GetAlign() const {
    return GetPrimTypeSize(primType);
  }

  virtual bool HasVolatileField() {
    return false;
  }

  virtual bool HasTypeParam() const {
    return false;
  }

  virtual bool IsIncomplete() const {
    return typeKind == kTypeStructIncomplete || typeKind == kTypeClassIncomplete ||
           typeKind == kTypeInterfaceIncomplete;
  }

  bool IsVolatile(int fieldID);

  bool ValidateClassOrInterface(const char *classname, bool noWarning = false);
  bool IsOfSameType(MIRType &type);
  const std::string &GetName() const;
  virtual std::string GetMplTypeName() const;
  virtual std::string GetCompactMplTypeName() const;
  virtual bool PointsToConstString() const;
  virtual size_t GetHashIndex() const {
    constexpr uint8 kIdxShift = 2;
    return ((static_cast<uint32>(primType) << kIdxShift) + (typeKind << kShiftNumOfTypeKind));
  }

  virtual bool HasFields() const { return false; }
  virtual size_t NumberOfFieldIDs() { return 0; } // total number of field IDs the type is consisted of, excluding its own field ID
  virtual MIRStructType *EmbeddedStructType() { return nullptr; }  // return any struct type directly embedded in this type
};

class MIRPtrType : public MIRType {
 public:
  TyIdx pointedTyIdx;
  TypeAttrs typeAttrs;
  bool Equalto(const MIRType &p) const override;
  MIRPtrType(TyIdx pointedTyidx) : MIRType(kTypePointer, PTY_ptr), pointedTyIdx(pointedTyidx) {}

  MIRPtrType(TyIdx pointedTyidx, PrimType pty) : MIRType(kTypePointer, pty), pointedTyIdx(pointedTyidx) {}

  MIRPtrType(PrimType pty, GStrIdx strIdx) : MIRType(kTypePointer, pty, strIdx), pointedTyIdx(TyIdx(0)) {}

  MIRType *CopyMIRTypeNode() const override {
    return new MIRPtrType(*this);
  }

  MIRType *GetPointedType() const;

  bool HasTypeParam() const override {
    return GetPointedType()->HasTypeParam();
  }

  void Dump(int indent, bool dontUseName = false) const override;
  size_t GetSize() const override { return POINTER_SIZE; }
  uint8 GetAlign() const override { return POINTER_SIZE; }
  TyidxFieldAttrPair GetPointedTyidxFldAttrPairWithFieldId(FieldID fldid) const;
  TyIdx GetPointedTyidxWithFieldId(FieldID fldid) const;
  bool PointsToConstString() const override;

  std::string GetMplTypeName() const override;

  std::string GetCompactMplTypeName() const override;

  size_t GetHashIndex() const override {
    constexpr uint8 kIdxShift = 4;
    return (pointedTyIdx.GetIdx() << kIdxShift) + (typeKind << kShiftNumOfTypeKind) + (typeAttrs.attrFlag << 3) + typeAttrs.attrAlign;
  }

  bool PointeeVolatile() const { return typeAttrs.GetAttr(ATTR_volatile); }
};

class MIRArrayType : public MIRType {
 public:
  TyIdx eTyIdx;
  uint16 dim;
  uint32 sizeArray[kMaxArrayDim];
  bool Equalto(const MIRType &p) const override;
  MIRArrayType &operator=(const MIRArrayType &p) = default;
  MIRArrayType() : MIRType(kTypeArray, PTY_agg), dim(0) {
    for (int i = 0; i != kMaxArrayDim; i++) {
      sizeArray[i] = 0;
    }
  }

  ~MIRArrayType() {}

  MIRArrayType(const MIRArrayType &pat) : MIRType(kTypeArray, PTY_agg) {
    eTyIdx = pat.eTyIdx;
    dim = pat.dim;
    for (int i = 0; i < dim; i++) {
      sizeArray[i] = pat.sizeArray[i];
    }
  }

  MIRArrayType(const TyIdx &eTyIdx, std::vector<uint32> &sizeArray) : MIRType(kTypeArray, PTY_agg) {
    this->eTyIdx = eTyIdx;
    dim = sizeArray.size();
    for (int i = 0; i < kMaxArrayDim; i++) {
      this->sizeArray[i] = (i < dim) ? sizeArray[i] : 0;
    }
  }

  MIRArrayType(GStrIdx strIdx)
    : MIRType(kTypeArray, PTY_agg, strIdx), eTyIdx(TyIdx()), dim(0), sizeArray{ 0 } {}

  uint16 GetDim() {
    return dim;
  }

  MIRType *GetElemType() const;

  MIRType *CopyMIRTypeNode() const override {
    return new MIRArrayType(*this);
  }

  bool HasTypeParam() const override {
    return GetElemType()->HasTypeParam();
  }

  void Dump(int indent, bool dontUseName = false) const override;
  size_t GetSize() const override {
    size_t elemsize = GetElemType()->GetSize();
    if (elemsize == 0) {
      return 0;
    }
    size_t numelems = sizeArray[0];
    for (int i = 1; i < dim; i++) {
      numelems *= sizeArray[i];
    }
    return elemsize * numelems;
  }
  uint8 GetAlign() const override {
    return GetElemType()->GetAlign();
  }

  size_t GetHashIndex() const override {
    constexpr uint8 kIdxShift = 2;
    size_t hidx = (eTyIdx.GetIdx() << kIdxShift) + (typeKind << kShiftNumOfTypeKind);
    for (size_t i = 0; i < dim; i++) {
      CHECK_FATAL(i < kMaxArrayDim, "array index out of range");
      hidx += sizeArray[i] << i;
    }
    return hidx;
  }

  std::string GetMplTypeName() const override;
  std::string GetCompactMplTypeName() const override;
  bool HasFields() const override;
  size_t NumberOfFieldIDs() override;
  MIRStructType *EmbeddedStructType() override;
};

// flexible array type, must be last field of a top-level struct
class MIRFarrayType : public MIRType {
 public:
  TyIdx elemTyIdx;
  bool Equalto(const MIRType &p) const override;
  MIRFarrayType() : MIRType(kTypeFArray, PTY_agg), elemTyIdx(TyIdx(0)){};
  MIRFarrayType(TyIdx elemtyidx) : MIRType(kTypeFArray, PTY_agg), elemTyIdx(elemtyidx) {}

  MIRFarrayType(GStrIdx strIdx) : MIRType(kTypeFArray, PTY_agg, strIdx), elemTyIdx(TyIdx(0)) {}

  MIRType *CopyMIRTypeNode() const override {
    return new MIRFarrayType(*this);
  }

  MIRType *GetElemType() const;

  bool HasTypeParam() const override {
    return GetElemType()->HasTypeParam();
  }

  void Dump(int indent, bool dontUseName = false) const override;

  size_t GetHashIndex() const override {
    constexpr uint8 kIdxShift = 5;
    return ((elemTyIdx.GetIdx() << kIdxShift) + (typeKind << kShiftNumOfTypeKind));
  }

  std::string GetMplTypeName() const override;
  std::string GetCompactMplTypeName() const override;

  bool HasFields() const override;
  size_t NumberOfFieldIDs() override;
  MIRStructType *EmbeddedStructType() override;
};

using TyidxFuncAttrPair = std::pair<TyIdx, FuncAttrs>;
using MethodPair = std::pair<StIdx, TyidxFuncAttrPair>;
using MethodVector = std::vector<MethodPair>;
using MethodPtrVector = std::vector<MethodPair *>;

#define FIELDVECID2FIELDID(i) ((i) + 1)
#define PARENTFIELDVECID2FIELDID(i) ((-(i)) - 1)

// used by kTypeStruct, kTypeStructIncomplete, kTypeUnion
class MIRStructType : public MIRType {
 public:
  FieldVector fields;
  std::vector<TyIdx> fieldInferredTyidx;
  FieldVector staticFields;
  FieldVector parentFields;        // fields belong to the ancestors not fully defined
  MethodVector methods;            // for the list of member function prototypes
  MethodPtrVector vTableMethods;  // the list of implmentation for all virtual functions for this type
  MethodPtrVector iTableMethods;  // the list of all interface functions for this type; For classes, they are
                                   // implementation functions, For interfaces, they are abstact functions.
                                   // Weak indicates the actual definition is in another module.
  bool isImported;
  bool isUsed;

 private:
  bool hasVolatileField;      // for caching computed value
  bool hasVolatileFieldSet;  // if true, just read hasVolatileField;
                              // otherwise compute to initialize hasVolatileField
 protected:
  virtual FieldPair TraverseToFieldRef(FieldID &fieldID) const;

 public:
  explicit MIRStructType(MIRTypeKind tkind)
      : MIRType(tkind, PTY_agg),
        fields(),
        fieldInferredTyidx(),
        staticFields(),
        parentFields(),
        methods(),
        vTableMethods(),
        iTableMethods(),
        isImported(false),
        isUsed(false),
        hasVolatileField(false),
        hasVolatileFieldSet(false) {}

  MIRStructType(MIRTypeKind tkind, GStrIdx strIdx)
      : MIRType(tkind, PTY_agg, strIdx),
        fields(),
        fieldInferredTyidx(),
        staticFields(),
        parentFields(),
        methods(),
        vTableMethods(),
        iTableMethods(),
        isImported(false),
        isUsed(false),
        hasVolatileField(false),
        hasVolatileFieldSet(false) {}

  ~MIRStructType() {}

  FieldPair TraverseToField(FieldID fieldID) const;
  FieldPair TraverseToField(GStrIdx fieldstridx) const;
  bool IsFieldVolatile(FieldID fieldID) const {
    const FieldPair fldpair = TraverseToField(fieldID);
    return fldpair.second.second.GetAttr(FLDATTR_volatile);
  }

  bool IsFieldFinal(FieldID fieldID) const {
    const FieldPair fldpair = TraverseToField(fieldID);
    return fldpair.second.second.GetAttr(FLDATTR_final);
  }

  bool IsFieldRCUnownedRef(FieldID fieldID) const {
    const FieldPair fldpair = TraverseToField(fieldID);
    return fldpair.second.second.GetAttr(FLDATTR_rcunowned);
  }

  bool IsFieldRCWeak(FieldID fieldID) const {
    const FieldPair fldpair = TraverseToField(fieldID);
    return fldpair.second.second.GetAttr(FLDATTR_rcweak);
  }

  bool IsOwnField(FieldID fieldID) const {
    const FieldPair pair = TraverseToField(fieldID);
    for (auto &iter : fields) {
      if (iter == pair) {
        return true;
      }
    }
    return false;
  }

  bool HasVolatileField() override;
  bool HasTypeParam() const override;
  bool Equalto(const MIRType &ty) const override;
  MIRType *CopyMIRTypeNode() const override {
    return new MIRStructType(*this);
  }

  TyIdx GetElemTyidx(uint32 n) const {
    return fields.at(n).second.first;
  }

  MIRType *GetElemType(uint32 n) const;

  TyIdx GetFieldTyidx(FieldID fieldID) {
    FieldPair fldpair = TraverseToField(fieldID);
    return fldpair.second.first;
  }

  MIRType *GetFieldType(FieldID fieldID);

  void SetElemTyidx(uint32 n, TyIdx tyIdx) {
    fields.at(n).second = TyidxFieldAttrPair(tyIdx, FieldAttrs());
  }

  GStrIdx GetElemStridx(uint32 n) {
    return fields.at(n).first;
  }

  void SetElemStridx(uint32 n, GStrIdx idx) {
    fields.at(n).first = idx;
  }

  void SetElemInferredTyidx(uint32 n, TyIdx tyIdx) {
    for (unsigned i = fieldInferredTyidx.size(); i < n + 1; i++) {
      fieldInferredTyidx.push_back(kInitTyIdx);
    }
    fieldInferredTyidx.at(n) = tyIdx;
  }

  TyIdx GetElemInferredTyidx(uint32 n) {
    for (unsigned i = fieldInferredTyidx.size(); i < n + 1; i++) {
      fieldInferredTyidx.push_back(kInitTyIdx);
    }
    return fieldInferredTyidx.at(n);
  }

  void DumpFieldsAndMethods(int indent, bool hasMethod) const;
  void Dump(int indent, bool dontUseName = false) const override;

  virtual void SetComplete() {
    typeKind = (typeKind == kTypeUnion) ? typeKind : kTypeStruct;
  }

  // only meaningful for MIRClassType and MIRInterface types
  bool IsLocal() const;

  size_t GetSize() const override;
  uint8 GetAlign() const override;

  size_t GetHashIndex() const override {
    return ((nameStrIdx.GetIdx() << kShiftNumOfNameStrIdx) + (typeKind << kShiftNumOfTypeKind));
  }

  virtual void ClearContents() {
    fields.clear();
    staticFields.clear();
    parentFields.clear();
    methods.clear();
    vTableMethods.clear();
    iTableMethods.clear();
    isImported = false;
    isUsed = false;
    hasVolatileField = false;
    hasVolatileFieldSet = false;
  }

  virtual const std::vector<MIRInfoPair> &GetInfo() const {
    CHECK_FATAL(false, "can not use GetInfo");
  }

  virtual const std::vector<bool> &GetInfoIsString() const {
    CHECK_FATAL(false, "can not use GetInfoIsString");
  }

  virtual void PushbackIsString(bool isString) {
    CHECK_FATAL(false, "can not use PushbackIsString");
  }

  bool HasFields() const override { return true; }
  size_t NumberOfFieldIDs() override;
  MIRStructType *EmbeddedStructType() override { return this; }

  std::string GetMplTypeName() const override;
  std::string GetCompactMplTypeName() const override;
  bool IsVolatile(int fieldID);
};

// java array type, must not be nested inside another aggregate
class MIRJarrayType : public MIRFarrayType {
 private:
  TyIdx parentTyIdx;       // since Jarray is also an object, this is java.lang.Object
  GStrIdx javaNameStrIdx;  // for internal java name of Jarray. nameStrIdx is used for other purpose
  bool fromPrimitive;        // the lowest dimension is primitive type
  int dim;                    // the dimension if decidable at compile time. otherwise 0

 public:
  MIRJarrayType() : parentTyIdx(0), javaNameStrIdx(0), fromPrimitive(false), dim(0) {
    typeKind = kTypeJArray;
  };

  ~MIRJarrayType() {}

  MIRJarrayType(TyIdx elemtyidx)
    : MIRFarrayType(elemtyidx), parentTyIdx(0), javaNameStrIdx(0), fromPrimitive(false), dim(0) {
    typeKind = kTypeJArray;
  }

  MIRJarrayType(GStrIdx strIdx)
    : MIRFarrayType(strIdx), parentTyIdx(0), javaNameStrIdx(0), fromPrimitive(false), dim(0) {
    typeKind = kTypeJArray;
  }

  MIRType *CopyMIRTypeNode() const override {
    return new MIRJarrayType(*this);
  }

  MIRStructType *GetParentType();
  const std::string &GetJavaName(void);

  bool IsPrimitiveArray() {
    if (javaNameStrIdx == GStrIdx(0)) {
      DetermineName();
    }
    return fromPrimitive;
  }

  int GetDim() {
    if (javaNameStrIdx == GStrIdx(0)) {
      DetermineName();
    }
    return dim;
  }

  size_t GetHashIndex() const override {
    constexpr uint8 kIdxShift = 5;
    return ((elemTyIdx.GetIdx() << kIdxShift) + (typeKind << kShiftNumOfTypeKind));
  }

 private:
  void DetermineName();  // determine the internal name of this type
};

using MIREncodedArray = std::vector<EncodedValue>;

// used by kTypeClass, kTypeClassIncomplete
class MIRClassType : public MIRStructType {
 public:
  TyIdx parentTyIdx;
  std::vector<TyIdx> interfacesImplemented;  // for the list of interfaces the class implements
  std::vector<MIRInfoPair> info;
  std::vector<bool> infoIsString;
  std::vector<MIRPragma *> pragmaVec;
  MIREncodedArray staticValue;  // DELETE THIS
  virtual FieldPair TraverseToFieldRef(FieldID &fieldID) const override;

 public:
  explicit MIRClassType(MIRTypeKind tkind)
      : MIRStructType(tkind),
        parentTyIdx(0),
        interfacesImplemented(),
        info(),
        infoIsString(),
        pragmaVec(),
        staticValue() {}

  MIRClassType(MIRTypeKind tkind, GStrIdx strIdx)
      : MIRStructType(tkind, strIdx),
        parentTyIdx(0),
        interfacesImplemented(),
        info(),
        infoIsString(),
        pragmaVec(),
        staticValue() {}

  bool Equalto(const MIRType &ty) const override;
  MIRType *CopyMIRTypeNode() const override {
    return new MIRClassType(*this);
  }

  const std::vector<MIRInfoPair> &GetInfo() const override {
    return info;
  }

  const MIRInfoPair &GetInfoElemt(size_t n) const {
    ASSERT(n < info.size(), "array index out of range");
    return info.at(n);
  }

  const std::vector<bool> &GetInfoIsString() const override {
    return infoIsString;
  }
  void PushbackIsString(bool isString) override {
    infoIsString.push_back(isString);
  }
  size_t GetInfoIsStringSize() const {
    return infoIsString.size();
  }
  bool GetInfoIsStringElemt(size_t n) const {
    ASSERT(n < infoIsString.size(), "array index out of range");
    return infoIsString.at(n);
  }

  void Dump(int indent, bool dontUseName = false) const override;
  void DumpAsCxx(int indent) const override;
  uint32 GetInfo(GStrIdx strIdx) const;
  void SetComplete() override {
    typeKind = kTypeClass;
  }

  bool IsFinal() const;
  bool IsAbstract() const;
  bool IsInner() const;

  size_t GetSize() const override;

  FieldID GetLastFieldID() const;
  FieldID GetFirstFieldID() const {
    return GetLastFieldID() - fields.size() + 1;
  }

  // return class id or superclass id accroding to input string
  uint32 GetInfo(const std::string &infoStr) const;

  FieldID GetFirstLocalFieldID() const;
  // return class id or superclass id accroding to input string
  MIRClassType *GetExceptionRootType();
  const MIRClassType *GetExceptionRootType() const;
  bool IsExceptionType() const;
  void AddImplementedInterface(TyIdx interfaceTyIdx) {
    if (std::find(interfacesImplemented.begin(), interfacesImplemented.end(), interfaceTyIdx) ==
        interfacesImplemented.end()) {
      interfacesImplemented.push_back(interfaceTyIdx);
    }
  }

  void ClearContents() override {
    MIRStructType::ClearContents();
    parentTyIdx.SetIdx(0);
    interfacesImplemented.clear();  // for the list of interfaces the class implements
    info.clear();
    infoIsString.clear();
    pragmaVec.clear();
    staticValue.clear();
  }

  size_t GetHashIndex() const override {
    return ((nameStrIdx.GetIdx() << kShiftNumOfNameStrIdx) + (typeKind << kShiftNumOfTypeKind));
  }

  size_t NumberOfFieldIDs() override;
};

// used by kTypeInterface, kTypeInterfaceIncomplete
class MIRInterfaceType : public MIRStructType {
 public:
  std::vector<TyIdx> parentsTyIdx;  // multiple inheritence
  std::vector<MIRInfoPair> info;
  std::vector<bool> infoIsString;
  std::vector<MIRPragma *> pragmaVec;
  MIREncodedArray staticValue;  // DELETE THIS
  virtual FieldPair TraverseToFieldRef(FieldID &fieldID) const override;

 public:
  explicit MIRInterfaceType(MIRTypeKind tkind)
      : MIRStructType(tkind),
        parentsTyIdx(),
        info(),
        infoIsString(),
        pragmaVec(),
        staticValue() {}

  MIRInterfaceType(MIRTypeKind tkind, GStrIdx strIdx)
      : MIRStructType(tkind, strIdx),
        parentsTyIdx(),
        info(),
        infoIsString(),
        pragmaVec(),
        staticValue() {}

  bool Equalto(const MIRType &ty) const override;
  MIRType *CopyMIRTypeNode() const override {
    return new MIRInterfaceType(*this);
  }

  const std::vector<MIRInfoPair> &GetInfo() const override {
    return info;
  }

  const MIRInfoPair &GetInfoElemt(size_t n) const {
    ASSERT(n < info.size(), "array index out of range");
    return info.at(n);
  }

  const std::vector<bool> &GetInfoIsString() const override {
    return infoIsString;
  }
  void PushbackIsString(bool isString) override {
    infoIsString.push_back(isString);
  }
  size_t GetInfoIsStringSize() const {
    return infoIsString.size();
  }
  bool GetInfoIsStringElemt(size_t n) const {
    ASSERT(n < infoIsString.size(), "array index out of range");
    return infoIsString.at(n);
  }

  void Dump(int indent, bool dontUseName = false) const override;
  uint32 GetInfo(GStrIdx strIdx) const;
  uint32 GetInfo(const std::string &infoStr) const;

  void SetComplete() override {
    typeKind = kTypeInterface;
  }

  size_t GetSize() const override;

  void ClearContents() override {
    MIRStructType::ClearContents();
    parentsTyIdx.clear();
    info.clear();
    infoIsString.clear();
    pragmaVec.clear();
    staticValue.clear();
  }

  size_t GetHashIndex() const override {
    return ((nameStrIdx.GetIdx() << kShiftNumOfNameStrIdx) + (typeKind << kShiftNumOfTypeKind));
  }

  bool HasFields() const override { return false; }
  size_t NumberOfFieldIDs() override { return 0; }
  MIRStructType *EmbeddedStructType() override { return nullptr; }
};

class MIRBitfieldType : public MIRType {
 public:
  uint8 fieldSize;
  MIRBitfieldType(uint8 field, PrimType pt) : MIRType(kTypeBitField, pt), fieldSize(field) {}

  MIRBitfieldType(uint8 field, PrimType pt, GStrIdx strIdx) : MIRType(kTypeBitField, pt, strIdx), fieldSize(field) {}

  bool Equalto(const MIRType &ty) const override;

  void Dump(int indent, bool dontUseName = false) const override;

  MIRType *CopyMIRTypeNode() const override {
    return new MIRBitfieldType(*this);
  }

  size_t GetSize() const override {
    if (fieldSize == 0) {
      return 0;
    } else if (fieldSize <= 8) {
      return 1;
    } else {
      return (fieldSize + 7) / 8;
    }
  }

  uint8 GetAlign() const override {
    return 0;
  }  // align not be in bytes

  size_t GetHashIndex() const override {
    return ((static_cast<uint32>(primType) << fieldSize) + (typeKind << kShiftNumOfTypeKind));
  }
};

class MIRFuncType : public MIRType {
 public:
  TyIdx retTyIdx;
  std::vector<TyIdx> paramTypeList;
  std::vector<TypeAttrs> paramAttrsList;
  bool isVarArgs;
  bool Equalto(const MIRType &ty) const override;
  MIRType *CopyMIRTypeNode() const override {
    return new MIRFuncType(*this);
  }

  MIRFuncType()
      : MIRType(kTypeFunction, PTY_ptr),
        paramTypeList(),
        paramAttrsList(),
        isVarArgs(false) {}

  MIRFuncType(TyIdx rettyidx, const std::vector<TyIdx> &vecTy, const std::vector<TypeAttrs> &vecAt)
      : MIRType(kTypeFunction, PTY_ptr),
        retTyIdx(rettyidx),
        paramTypeList(vecTy),
        paramAttrsList(vecAt),
        isVarArgs(false) {}

  explicit MIRFuncType(GStrIdx strIdx)
      : MIRType(kTypeFunction, PTY_ptr, strIdx),
        retTyIdx(TyIdx(0)),
        paramTypeList(),
        paramAttrsList(),
        isVarArgs(false) {}

  ~MIRFuncType() {}

  void Dump(int indent, bool dontUseName = false) const override;
  size_t GetSize() const override {
    return 0;
  }  // size unknown
  uint8 GetAlign() const override {
    return 0;
  }  // align unknown

  size_t GetHashIndex() const override {
    constexpr uint8 kIdxShift = 6;
    size_t hidx = (retTyIdx.GetIdx() << kIdxShift) + (typeKind << kShiftNumOfTypeKind);
    size_t size = paramTypeList.size();
    hidx += ((size) ? (paramTypeList[0].GetIdx() + size) : 0) << 4;
    return hidx;
  }
};

class MIRTypeByName : public MIRType {
  // use nameStrIdx to store the name for both local and global
 public:
  bool Equalto(const MIRType &p) const override;
  MIRTypeByName(GStrIdx sidx) : MIRType(kTypeByName, PTY_void) {
    nameStrIdx = sidx;
  }

  MIRType *CopyMIRTypeNode() const override {
    return new MIRTypeByName(*this);
  }

  void Dump(int indent, bool dontUseName = false) const override;
  size_t GetSize() const override {
    return 0;
  }  // size unknown
  uint8 GetAlign() const override {
    return 0;
  }  // align unknown

  size_t GetHashIndex() const override {
    constexpr uint8 kIdxShift = 2;
    return ((nameStrIdx.GetIdx() << kIdxShift) + nameIsLocal + (typeKind << kShiftNumOfTypeKind));
  }
};

class MIRTypeParam : public MIRType {
  // use nameStrIdx to store the name
 public:
  MIRTypeParam(GStrIdx sidx) : MIRType(kTypeParam, PTY_gen) {
    nameStrIdx = sidx;
  }

  ~MIRTypeParam() {}

  MIRType *CopyMIRTypeNode() const override {
    return new MIRTypeParam(*this);
  }

  bool Equalto(const MIRType &p) const override;
  void Dump(int indent, bool dontUseName = false) const override;
  size_t GetSize() const override {
    return 0;
  }  // size unknown
  uint8 GetAlign() const override {
    return 0;
  }  // align unknown

  bool HasTypeParam() const override {
    return true;
  }

  size_t GetHashIndex() const override {
    constexpr uint8 kIdxShift = 3;
    return ((nameStrIdx.GetIdx() << kIdxShift) + (typeKind << kShiftNumOfTypeKind));
  }
};

typedef std::pair<TyIdx, TyIdx> TypePair;
typedef std::vector<TypePair> GenericInstantVector;

class MIRInstantVectorType : public MIRType {
 public:
  GenericInstantVector instantVec;  // in each pair, first is generic type,
                                     // second is real type
 public:
  MIRInstantVectorType()
      : MIRType(kTypeInstantVector, PTY_agg), instantVec() {}
  explicit MIRInstantVectorType(MIRTypeKind kind)
      : MIRType(kind,PTY_agg), instantVec() {}
  MIRInstantVectorType(MIRTypeKind kind, GStrIdx strIdx)
      : MIRType(kind, PTY_agg, strIdx), instantVec() {}
  MIRType *CopyMIRTypeNode() const override {
    return new MIRInstantVectorType(*this);
  }

  bool Equalto(const MIRType &p) const override;
  void Dump(int indent, bool dontUseName = false) const override;
  size_t GetSize() const override {
    return 0;
  }  // size unknown
  uint8 GetAlign() const override {
    return 0;
  }  // align unknown

  size_t GetHashIndex() const override {
    uint32 hidx = typeKind << kShiftNumOfTypeKind;
    for (TypePair typePair : instantVec) {
      hidx += (typePair.first.GetIdx() + typePair.second.GetIdx()) << 3;
    }
    return hidx;
  }
};

class MIRGenericInstantType : public MIRInstantVectorType {
 public:
  TyIdx genericTyIdx;  // the generic type to be instantiated
 public:
  explicit MIRGenericInstantType(TyIdx gentyidx)
      : MIRInstantVectorType(kTypeGenericInstant), genericTyIdx(gentyidx) {}

  explicit MIRGenericInstantType(GStrIdx strIdx)
      : MIRInstantVectorType(kTypeGenericInstant, strIdx), genericTyIdx(TyIdx(0)) {}

  MIRType *CopyMIRTypeNode() const override {
    return new MIRGenericInstantType(*this);
  }

  bool Equalto(const MIRType &p) const override;
  void Dump(int indent, bool dontUseName = false) const override;
  size_t GetSize() const override {
    return 0;
  }  // size unknown
  uint8 GetAlign() const override {
    return 0;
  }  // align unknown

  size_t GetHashIndex() const override {
    constexpr uint8 kIdxShift = 2;
    uint32 hidx = (genericTyIdx.GetIdx() << kIdxShift) + (typeKind << kShiftNumOfTypeKind);
    for (TypePair typePair : instantVec) {
      hidx += (typePair.first.GetIdx() + typePair.second.GetIdx()) << 3;
    }
    return hidx;
  }
};
#endif  // MIR_FEATURE_FULL
}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_MIR_TYPE_H
