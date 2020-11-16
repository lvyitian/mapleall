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

#include "mir_type.h"
#include <iostream>
#include <cstring>
#include "mir_symbol.h"
#include "printing.h"
#include "name_mangler.h"
#include "global_tables.h"
#include "mir_builder.h"
#include "cfg_primitive_types.h"
#if MIR_FEATURE_FULL

namespace maple {
#define LOAD_PRIMARY_TYPE_PROPERTY
#include "prim_types.def"

#define LOAD_ALGO_PRIMARY_TYPE
const PrimitiveTypeProperty &GetPrimitiveTypeProperty(PrimType pType) {
  switch (pType) {
    case PTY_begin:
      return PTProperty_begin;
#define PRIMTYPE(P) \
    case PTY_##P:   \
      return PTProperty_##P;
#include "prim_types.def"
#undef PRIMTYPE
    case PTY_end:
    default:
      return PTProperty_end;
  }
}

PrimType GetRegPrimType(PrimType primType) {
  switch (primType) {
    case PTY_i8:
    case PTY_i16:
      return PTY_i32;
    case PTY_u1:
    case PTY_u8:
    case PTY_u16:
      return PTY_u32;
    default:
      return primType;
  }
}

bool VerifyPrimType(PrimType pty1, PrimType pty2) {
  switch (pty1) {
    case PTY_u1:
    case PTY_u8:
    case PTY_u16:
    case PTY_u32:
    case PTY_a32:
      return IsUnsignedInteger(pty2);
    case PTY_i8:
    case PTY_i16:
    case PTY_i32:
      return IsSignedInteger(pty2);
    default:
      return pty1 == pty2;
  }
}

PrimType GetDynType(PrimType primType) {
#ifdef DYNAMICLANG
  switch (primType) {
    case PTY_u1:
      return PTY_dynbool;
    case PTY_i32:
      return PTY_dyni32;
    case PTY_simplestr:
      return PTY_dynstr;
    case PTY_simpleobj:
      return PTY_dynobj;
    case PTY_f32:
      return PTY_dynf32;
    case PTY_f64:
      return PTY_dynf64;
    default:
      return primType;
  }
#else
  return primType;
#endif
}

PrimType GetNonDynType(PrimType primType) {
#ifdef DYNAMICLANG
  switch (primType) {
    case PTY_dynbool:
      return PTY_u1;
    case PTY_dyni32:
      return PTY_i32;
    case PTY_dynstr:
      return PTY_simplestr;
    case PTY_dynobj:
      return PTY_simpleobj;
    case PTY_dynf32:
      return PTY_f32;
    case PTY_dynf64:
      return PTY_f64;
    default:
      return primType;
  }
#else
  return primType;
#endif
}

bool IsNoCvtNeeded(PrimType toType, PrimType fromType) {
  switch (toType) {
    case PTY_i32:
      return fromType == PTY_i16 || fromType == PTY_i8;
    case PTY_u32:
      return fromType == PTY_u16 || fromType == PTY_u8;
    case PTY_u1:
    case PTY_u8:
    case PTY_u16:
      return fromType == PTY_u32;
    case PTY_i8:
    case PTY_i16:
      return fromType == PTY_i32;
    case PTY_u64:
      return fromType == PTY_ptr;
    case PTY_ptr:
      return fromType == PTY_u64;
    default:
      return false;
  }
}

// answer in bytes; 0 if unknown
uint32 GetPrimTypeSize(PrimType pty) {
  switch (pty) {
    case PTY_void:
    case PTY_agg:
      return 0;
    case PTY_ptr:
    case PTY_ref:
      return POINTER_SIZE;
    case PTY_u1:
    case PTY_i8:
    case PTY_u8:
      return 1;
    case PTY_i16:
    case PTY_u16:
      return 2;
    case PTY_a32:
    case PTY_f32:
    case PTY_i32:
    case PTY_u32:
    case PTY_simplestr:
    case PTY_simpleobj:
      return 4;
    case PTY_a64:
    case PTY_c64:
    case PTY_f64:
    case PTY_i64:
    case PTY_u64:
      return 8;
    case PTY_v4i32:
    case PTY_v2i64:
    case PTY_v8i16:
    case PTY_v16i8:
    case PTY_v2f64:
    case PTY_v4f32:
      return 16;
    case PTY_c128:
    case PTY_f128:
      return 16;
#ifdef DYNAMICLANG
    case PTY_dynf32:
    case PTY_dyni32:
    case PTY_dynstr:
    case PTY_dynobj:
    case PTY_dynundef:
    case PTY_dynnull:
    case PTY_dynbool:
      return 8;
    case PTY_dynany:
    case PTY_dynf64:
      return 8;
#endif
    default:
      return 0;
  }
}

// answer is n if size in byte is (1<<n) (0: 1B; 1: 2B, 2: 4B, 3: 8B, 4:16B)
uint32 GetPrimTypeP2Size(PrimType pty) {
  switch (pty) {
    case PTY_ptr:
    case PTY_ref:
      return POINTER_P2SIZE;
    case PTY_u1:
    case PTY_i8:
    case PTY_u8:
      return 0;
    case PTY_i16:
    case PTY_u16:
      return 1;
    case PTY_a32:
    case PTY_f32:
    case PTY_i32:
    case PTY_u32:
    case PTY_simplestr:
    case PTY_simpleobj:
      return 2;
    case PTY_a64:
    case PTY_c64:
    case PTY_f64:
    case PTY_i64:
    case PTY_u64:
      return 3;
    case PTY_c128:
    case PTY_f128:
      return 4;
#ifdef DYNAMICLANG
    case PTY_dynf32:
    case PTY_dyni32:
    case PTY_dynstr:
    case PTY_dynobj:
    case PTY_dynundef:
    case PTY_dynnull:
    case PTY_dynbool:
      return 3;
    case PTY_dynany:
    case PTY_dynf64:
      return 3;
#endif
    default:
      CHECK_FATAL(false, "Power-of-2 size only applicable to sizes of 1, 2, 4, 8 or 16 bytes.");
      return 10;
  }
}

const char *GetPrimTypeName(PrimType ty) {
#define LOAD_ALGO_PRIMARY_TYPE
  switch (ty) {
    case kPtyInvalid:
      return "kPtyInvalid";
#define PRIMTYPE(P) \
  case PTY_##P:     \
    return #P;
#include "prim_types.def"
#undef PRIMTYPE
    case kPtyDerived:
      return "derived";  // just for test: no primitive type for derived
    default:
      return "kPtyInvalid";
  }
  // SIMD types to be defined
}

const char *GetPrimTypeJavaName(PrimType ty) {
  switch (ty) {
    default:
    case kPtyInvalid:  // ASSERT(false, "wrong primitive type!");
    case PTY_u1:
      return "Z";
    case PTY_i8:
      return "B";
    case PTY_i16:
      return "S";
    case PTY_u16:
      return "C";
    case PTY_i32:
      return "I";
    case PTY_i64:
      return "J";
    case PTY_f32:
      return "F";
    case PTY_f64:
      return "D";
    case PTY_void:
      return "V";
    case PTY_constStr:
      return JSTRTYPENAME;
  }
}

void TypeAttrs::DumpAttributes() const {
#define TYPE_ATTR
#define STRING(s) #s
#define ATTR(AT)          \
  if (GetAttr(ATTR_##AT)) \
    LogInfo::MapleLogger() << " " << STRING(AT);
#include "all_attributes.def"
#undef ATTR
#undef TYPE_ATTR
  if (attrAlign) {
    LogInfo::MapleLogger() << " align(" << GetAlign() << ")";
  }
}

void FieldAttrs::DumpAttributes() const {
#define FIELD_ATTR
#define STRING(s) #s
#define ATTR(AT)             \
  if (GetAttr(FLDATTR_##AT)) \
    LogInfo::MapleLogger() << " " << STRING(AT);
#include "all_attributes.def"
#undef ATTR
#undef FIELD_ATTR
  if (attrAlign) {
    LogInfo::MapleLogger() << " align(" << GetAlign() << ")";
  }
}

const std::string &MIRType::GetName(void) const {
  return GlobalTables::GetStrTable().GetStringFromStrIdx(nameStrIdx);
}

bool MIRType::ValidateClassOrInterface(const char *classname, bool noWarning) {
  if (primType == maple::PTY_agg && (typeKind == maple::kTypeClass || typeKind == maple::kTypeInterface) &&
      nameStrIdx.GetIdx()) {
    return true;
  } else {
    if (noWarning == false) {
      int len = strlen(classname);
      if (len > 4 && strncmp(classname + len - 3, "_3B", 3) == 0) {
        std::cerr << "error: missing proper mplt file for " << classname << std::endl;
      } else {
        std::cerr << "internal error: type is not java class or interface " << classname << std::endl;
      }
    }
    return false;
  }
}

bool MIRType::PointsToConstString() const {
  if (typeKind == kTypePointer) {
    return static_cast<const MIRPtrType *>(this)->PointsToConstString();
  }
  return false;
}

std::string MIRType::GetMplTypeName() const {
  if (typeKind == kTypeScalar) {
    return GetPrimTypeName(primType);
  }
  return "";
}

std::string MIRType::GetCompactMplTypeName() const {
  if (typeKind == kTypeScalar) {
    return GetPrimTypeJavaName(primType);
  }
  return "";
}

void MIRType::Dump(int indent, bool dontUseName) const {
  LogInfo::MapleLogger() << GetPrimTypeName(primType);
}

void MIRType::DumpAsCxx(int indent) const {
  switch (primType) {
    case PTY_void:
      LogInfo::MapleLogger() << "void";
      break;
    case PTY_i8:
      LogInfo::MapleLogger() << "int8";
      break;
    case PTY_i16:
      LogInfo::MapleLogger() << "int16";
      break;
    case PTY_i32:
      LogInfo::MapleLogger() << "int32";
      break;
    case PTY_i64:
      LogInfo::MapleLogger() << "int64";
      break;
    case PTY_u8:
      LogInfo::MapleLogger() << "uint8";
      break;
    case PTY_u16:
      LogInfo::MapleLogger() << "uint16";
      break;
    case PTY_u32:
      LogInfo::MapleLogger() << "uint32";
      break;
    case PTY_u64:
      LogInfo::MapleLogger() << "uint64";
      break;
    case PTY_u1:
      LogInfo::MapleLogger() << "bool  ";
      break;
    case PTY_ptr:
      LogInfo::MapleLogger() << "void* ";
      break;
    case PTY_ref:
      LogInfo::MapleLogger() << "void* ";
      break;
    case PTY_a32:
      LogInfo::MapleLogger() << "int32";
      break;
    case PTY_a64:
      LogInfo::MapleLogger() << "void* ";
      break;
    case PTY_f32:
      LogInfo::MapleLogger() << "float ";
      break;
    case PTY_f64:
      LogInfo::MapleLogger() << "double";
      break;
    case PTY_c64:
      LogInfo::MapleLogger() << "float complex";
      break;
    case PTY_c128:
      LogInfo::MapleLogger() << "double complex";
      break;
    default:
      ASSERT(false, "NYI");
  }
}

bool MIRType::IsOfSameType(MIRType &type) {
  if (typeKind != type.typeKind) {
    return false;
  }

  if (typeKind == kTypePointer) {
    const auto &ptrType = static_cast<const MIRPtrType&>(*this);
    const auto &ptrTypeIt = static_cast<const MIRPtrType&>(type);
    if (ptrType.pointedTyIdx == ptrTypeIt.pointedTyIdx) {
      return true;
    } else {
      MIRType &mirTypeIt = *GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptrTypeIt.pointedTyIdx);
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptrType.pointedTyIdx)->IsOfSameType(mirTypeIt);
    }
  } else if (typeKind == kTypeJArray) {
    auto &arrType1 = static_cast<MIRJarrayType&>(*this);
    auto &arrType2 = static_cast<MIRJarrayType&>(type);
    if (arrType1.GetDim() != arrType2.GetDim()) {
      return false;
    }
    return arrType1.GetElemType()->IsOfSameType(*arrType2.GetElemType());
  } else {
    return tyIdx == type.tyIdx;
  }
}

inline void DumpTypename(GStrIdx strIdx, bool isLocal) {
  LogInfo::MapleLogger() << ((isLocal) ? "%" : "$") << GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
}

static bool CheckAndDumpTypeName(GStrIdx strIdx, bool isLocal) {
  if (strIdx == GStrIdx(0)) {
    return false;
  }
  LogInfo::MapleLogger() << "<";
  DumpTypename(strIdx, isLocal);
  LogInfo::MapleLogger() << ">";
  return true;
}

void MIRFuncType::Dump(int indent, bool dontUseName) const {
  if (!dontUseName && CheckAndDumpTypeName(nameStrIdx, nameIsLocal)) {
    return;
  }
  LogInfo::MapleLogger() << "<func(";
  int size = paramTypeList.size();
  for (int i = 0; i < size; i++) {
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(paramTypeList[i])->Dump(indent + 1);
    if (size - 1 != i) {
      LogInfo::MapleLogger() << ",";
    }
  }
  if (isVarArgs) {
    LogInfo::MapleLogger() << ", ...";
  }
  LogInfo::MapleLogger() << ") ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(retTyIdx)->Dump(indent + 1);
  LogInfo::MapleLogger() << ">";
}

static constexpr uint64 RoundUpConst(uint64 offset, uint8 align) {
  return (-align) & (offset + align - 1);
}

static inline uint64 RoundUp(uint64 offset, uint8 align) {
  if (align == 0) {
    return offset;
  }
  return RoundUpConst(offset, align);
}

static constexpr uint64 RoundDownConst(uint64 offset, uint8 align) {
  return (-align) & offset;
}

static inline uint64 RoundDown(uint64 offset, uint8 align) {
  if (align == 0) {
    return offset;
  }
  return RoundDownConst(offset, align);
}

size_t MIRArrayType::GetSize() const {
  size_t elemsize = GetElemType()->GetSize();
  if (elemsize == 0) {
    return 0;
  }
  elemsize = RoundUp(elemsize, typeAttrs.GetAlign());
  size_t numelems = sizeArray[0];
  for (int i = 1; i < dim; i++) {
    numelems *= sizeArray[i];
  }
  return elemsize * numelems;
}

uint32 MIRArrayType::GetAlign() const {
  return std::max(GetElemType()->GetAlign(), typeAttrs.GetAlign());
}

void MIRArrayType::Dump(int indent, bool dontUseName) const {
  if (!dontUseName && CheckAndDumpTypeName(nameStrIdx, nameIsLocal)) {
    return;
  }
  LogInfo::MapleLogger() << "<";
  for (int i = 0; i < dim; i++) {
    LogInfo::MapleLogger() << "[" << sizeArray[i] << "]";
  }
  LogInfo::MapleLogger() << " ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(eTyIdx)->Dump(indent + 1);
  typeAttrs.DumpAttributes();
  LogInfo::MapleLogger() << ">";
}

std::string MIRArrayType::GetCompactMplTypeName() const {
  std::stringstream ss;
  ss << "A";
  MIRType *elemType = GetElemType();
  ss << elemType->GetCompactMplTypeName();
  return ss.str();
}

void MIRFarrayType::Dump(int indent, bool dontUseName) const {
  if (!dontUseName && CheckAndDumpTypeName(nameStrIdx, nameIsLocal)) {
    return;
  }
  LogInfo::MapleLogger() << "<[] ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(elemTyIdx)->Dump(indent + 1);
  LogInfo::MapleLogger() << ">";
}

std::string MIRFarrayType::GetCompactMplTypeName() const {
  std::stringstream ss;
  ss << "A";
  MIRType *elemType = GetElemType();
  ss << elemType->GetCompactMplTypeName();
  return ss.str();
}

const std::string &MIRJarrayType::GetJavaName(void) {
  if (javaNameStrIdx == GStrIdx(0)) {
    DetermineName();
  }
  return GlobalTables::GetStrTable().GetStringFromStrIdx(javaNameStrIdx);
}

MIRStructType *MIRJarrayType::GetParentType() {
  if (parentTyIdx == TyIdx(0)) {
    GStrIdx jloStridx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangObjectStr);
    parentTyIdx = GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(jloStridx);
    ASSERT((parentTyIdx != TyIdx(0)), "cannot find type for java.lang.Object");
  }

  return static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyIdx));
}

void MIRJarrayType::DetermineName() {
  if (javaNameStrIdx != GStrIdx(0)) {
    return;  // already determined
  }
  MIRType *elemtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(elemTyIdx);
  dim = 1;
  std::string basename;

  while (1) {
    if (elemtype->typeKind == kTypeScalar) {
      basename = GetPrimTypeJavaName(elemtype->primType);
      fromPrimitive = true;
      break;
    } else if (elemtype->typeKind == kTypePointer) {
      MIRType *ptype = static_cast<MIRPtrType *>(elemtype)->GetPointedType();
      ASSERT(ptype, "ptype is null in MIRJarrayType::DetermineName");
      if (ptype->typeKind == kTypeByName || ptype->typeKind == kTypeClass || ptype->typeKind == kTypeInterface ||
          ptype->typeKind == kTypeClassIncomplete || ptype->typeKind == kTypeInterfaceIncomplete) {
        basename = static_cast<MIRStructType *>(ptype)->GetName();
        fromPrimitive = false;
        break;
      } else if (ptype->typeKind == kTypeArray || ptype->typeKind == kTypeJArray) {
        elemtype = dynamic_cast<MIRJarrayType *>(ptype)->GetElemType();
        ASSERT(elemtype, "elemtype is null in MIRJarrayType::DetermineName");
        dim++;
      } else {
        ASSERT(false, "unexpected type!");
      }
    } else {
      ASSERT(false, "unexpected type!");
    }
  }
  std::string name;
  int i = dim;
  while (i-- > 0) {
    name += JARRAY_PREFIX_STR;
  }

  name += basename;

  javaNameStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(name);
}

MIRType *MIRPtrType::GetPointedType() const {
  return GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointedTyIdx);
}

bool MIRType::IsVolatile(int fieldID) {
  if (fieldID == 0) {
    return HasVolatileField();
  }
  return static_cast<const MIRStructType*>(this)->IsFieldVolatile(fieldID);
}

bool MIRPtrType::PointsToConstString() const {
  GStrIdx typenameIdx = GetPointedType()->nameStrIdx;
  std::string typeName = GlobalTables::GetStrTable().GetStringFromStrIdx(typenameIdx);
  return typeName == NameMangler::kJavaLangStringStr;
}

void MIRPtrType::Dump(int indent, bool dontUseName) const {
  if (!dontUseName && CheckAndDumpTypeName(nameStrIdx, nameIsLocal)) {
    return;
  }
  MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointedTyIdx);
  if (pointedty->typeKind == kTypeFunction) {  // no * for function pointer
    pointedty->Dump(indent);
  } else {
    LogInfo::MapleLogger() << "<* ";
    pointedty->Dump(indent + 1);
    typeAttrs.DumpAttributes();
    LogInfo::MapleLogger() << ">";
  }
}

void MIRBitfieldType::Dump(int indent, bool dontUseName) const {
  LogInfo::MapleLogger() << ":" << static_cast<int>(fieldSize) << " " << GetPrimTypeName(primType);
}

size_t MIRClassType::GetSize() const {
  if (parentTyIdx == TyIdx(0)) {
    return MIRStructType::GetSize();
  }
  MIRClassType *parentType = static_cast<MIRClassType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyIdx));
  uint32 prntSize = parentType->GetSize();
  if (prntSize == 0) {
    return 0;
  }
  uint32 structSize = MIRStructType::GetSize();
  if (structSize == 0) {
    return 0;
  }
  return prntSize + structSize;
}

FieldID MIRClassType::GetFirstLocalFieldID() const {
  if (!IsLocal()) {
    return 0;
  }
  if (parentTyIdx == 0) {
    return 1;
  }

  constexpr uint8 lastFieldIDOffset = 2;
  constexpr uint8 firstLocalFieldIDOffset = 1;
  const auto *parentClassType =
      static_cast<const MIRClassType*>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyIdx));
  return !parentClassType->IsLocal() ? parentClassType->GetLastFieldID() + lastFieldIDOffset
                                     : parentClassType->GetFirstLocalFieldID() + firstLocalFieldIDOffset;
}

const MIRClassType *MIRClassType::GetExceptionRootType() const {
  GStrIdx ehtynameidx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangObjectStr);
  const MIRClassType *subclasstype = this;
  while (subclasstype != nullptr && subclasstype->nameStrIdx != ehtynameidx) {
    subclasstype = static_cast<const MIRClassType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(subclasstype->parentTyIdx));
  }
  return subclasstype;
}

MIRClassType *MIRClassType::GetExceptionRootType() {
  return const_cast<MIRClassType*>(const_cast<const MIRClassType*>(this)->GetExceptionRootType());
}

bool MIRClassType::IsExceptionType() const {
  GStrIdx ehtynameidx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kThrowClassStr);
  const MIRClassType *parentclasstype = this;
  while (parentclasstype != nullptr && parentclasstype->nameStrIdx != ehtynameidx) {
    parentclasstype = static_cast<MIRClassType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentclasstype->parentTyIdx));
  }
  return parentclasstype != nullptr;
}

FieldID MIRClassType::GetLastFieldID() const {
  FieldID fieldID = fields.size();
  if (parentTyIdx != 0) {
    const auto *parentClassType =
        static_cast<const MIRClassType*>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyIdx));
    if (parentClassType != nullptr) {
      fieldID += parentClassType->GetLastFieldID() + 1;
    }
  }
  return fieldID;
}

static void DumpClassOrInterfaceInfo(const MIRStructType &type, int indent) {
  const std::vector<MIRInfoPair> &info = type.GetInfo();
  std::vector<bool> infoIsString = type.GetInfoIsString();
  size_t size = info.size();
  for (size_t i = 0; i < size; ++i) {
    LogInfo::MapleLogger() << '\n';
    PrintIndentation(indent);
    LogInfo::MapleLogger() << "@" << GlobalTables::GetStrTable().GetStringFromStrIdx(info[i].first) << " ";
    if (!infoIsString[i]) {
      LogInfo::MapleLogger() << info[i].second;
    } else {
      LogInfo::MapleLogger() << "\"" << GlobalTables::GetStrTable().GetStringFromStrIdx(GStrIdx(info[i].second))
                             << "\"";
    }
    if (i != size - 1) {
      LogInfo::MapleLogger() << ",";
    }
  }
}

static uint32 GetInfoFromStrIdx(const std::vector<MIRInfoPair> &info, const GStrIdx &strIdx) {
  for (MIRInfoPair infoPair : info) {
    if (infoPair.first == strIdx) {
      return infoPair.second;
    }
  }
  return 0;
}

uint32 MIRInterfaceType::GetInfo(GStrIdx strIdx) const {
  return GetInfoFromStrIdx(info, strIdx);
}

// return class id or superclass id accroding to input string
uint32 MIRInterfaceType::GetInfo(const std::string &infoStr) const {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName(infoStr);
  return GetInfo(strIdx);
}

size_t MIRInterfaceType::GetSize() const {
  if (parentsTyIdx.size() == 0) {
    return MIRStructType::GetSize();
  }
  uint32 size = MIRStructType::GetSize();
  if (size == 0) {
    return 0;
  }
  for (uint32 i = 0; i < parentsTyIdx.size(); i++) {
    MIRInterfaceType *parentType = static_cast<MIRInterfaceType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentsTyIdx[i]));
    uint32 prntSize = parentType->GetSize();
    if (prntSize == 0) {
      return 0;
    }
    size += prntSize;
  }
  return size;
}

static void DumpStaticValue(MIREncodedArray staticValue, int indent) {
  if (staticValue.size() == 0) {
    return;
  }

  LogInfo::MapleLogger() << std::endl;
  PrintIndentation(indent);
  LogInfo::MapleLogger() << "@"
            << "staticValue";
  for (uint32 j = 0; j < staticValue.size(); j++) {
    LogInfo::MapleLogger() << " [";
    uint8 valueArg = static_cast<uint32>(staticValue[j].encodedValue[0]) >> 5;
    uint8 valueType = static_cast<uint32>(staticValue[j].encodedValue[0]) & 0x1f;
    // kValueNull kValueBoolean
    if (valueType == 0x1e || valueType == 0x1f) {
      valueArg = 1;
    } else {
      valueArg += 2;
    }
    for (uint32 k = 0; k < valueArg; k++) {
      LogInfo::MapleLogger() << static_cast<uint32>(staticValue[j].encodedValue[k]);
      if (k != static_cast<uint32>(valueArg - 1)) {
        LogInfo::MapleLogger() << " ";
      }
    }
    LogInfo::MapleLogger() << "]";
  }
}

static void DumpFields(FieldVector fields, int indent, bool otherfields = false) {
  uint32 size = fields.size();
  for (uint32 i = 0; i < size; i++) {
    LogInfo::MapleLogger() << std::endl;
    PrintIndentation(indent);
    if (!otherfields) {
      LogInfo::MapleLogger() << "@";
    } else {
      LogInfo::MapleLogger() << "^";
    }
    LogInfo::MapleLogger() << GlobalTables::GetStrTable().GetStringFromStrIdx(fields[i].first) << " ";
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(fields[i].second.first)->Dump(indent + 1);
    FieldAttrs &fa = fields[i].second.second;
    fa.DumpAttributes();
    if (fa.GetAttr(FLDATTR_static) && fa.GetAttr(FLDATTR_final) &&
        (fa.GetAttr(FLDATTR_public) || fa.GetAttr(FLDATTR_protected))) {
      const char *fieldname = (GlobalTables::GetStrTable().GetStringFromStrIdx(fields[i].first)).c_str();
      MIRSymbol *fieldVar = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(fieldname));
      if (fieldVar && dynamic_cast<MIRStr16Const *>(fieldVar->value.konst)) {
        LogInfo::MapleLogger() << " = ";
        fieldVar->value.konst->Dump();
      }
    }
    if (i != size - 1) {
      LogInfo::MapleLogger() << ",";
    }
  }
  return;
}

static void DumpFieldsAsCxx(FieldVector fields, int indent) {
  for (auto &f : fields) {
    PrintIndentation(indent);

    FieldAttrs &fa = f.second.second;
    if (fa.GetAttr(FLDATTR_static)) {
      LogInfo::MapleLogger() << "// ";
    }

    LogInfo::MapleLogger() << "/* ";
    fa.DumpAttributes();
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(f.second.first)->Dump(indent + 1);
    LogInfo::MapleLogger() << " */ ";

    GlobalTables::GetTypeTable().GetTypeFromTyIdx(f.second.first)->DumpAsCxx(indent + 1);

    LogInfo::MapleLogger() << " " << GlobalTables::GetStrTable().GetStringFromStrIdx(f.first);
    LogInfo::MapleLogger() << ";" << std::endl;
  }
}

static void DumpMethods(MethodVector methods, int indent) {
  int size = methods.size();
  for (int i = 0; i < size; i++) {
    LogInfo::MapleLogger() << std::endl;
    PrintIndentation(indent);
    LogInfo::MapleLogger() << "&" << GlobalTables::GetGsymTable().GetSymbolFromStIdx(methods[i].first.Idx())->GetName();
    methods[i].second.second.DumpAttributes();
    LogInfo::MapleLogger() << " (";
    MIRFuncType *functype = static_cast<MIRFuncType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(methods[i].second.first));
    int parmlistsize = functype->paramTypeList.size();
    for (int j = 0; j < parmlistsize; j++) {
      GlobalTables::GetTypeTable().GetTypeFromTyIdx(functype->paramTypeList[j])->Dump(indent + 1);
      if (parmlistsize - 1 != j) {
        LogInfo::MapleLogger() << ",";
      }
    }
    if (functype->isVarArgs) {
      LogInfo::MapleLogger() << ", ...";
    }
    LogInfo::MapleLogger() << ") ";
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(functype->retTyIdx)->Dump(indent + 1);
    if (i != size - 1) {
      LogInfo::MapleLogger() << ",";
    }
  }
}

static void DumpConstructorsAsCxx(MethodVector methods, int indent) {
  unsigned int i = 0;
  for (auto &m : methods) {
    FuncAttrs &fa = m.second.second;
    if (!fa.GetAttr(FUNCATTR_constructor) || !fa.GetAttr(FUNCATTR_public)) {
      continue;
    }

    MIRFuncType *functype = static_cast<MIRFuncType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(m.second.first));

    PrintIndentation(indent);
    LogInfo::MapleLogger() << "/* ";
    LogInfo::MapleLogger() << "&" << GlobalTables::GetGsymTable().GetSymbolFromStIdx(m.first.Idx())->GetName();
    fa.DumpAttributes();
    LogInfo::MapleLogger() << " (";
    unsigned int j = 0;
    for (auto &p : functype->paramTypeList) {
      GlobalTables::GetTypeTable().GetTypeFromTyIdx(p)->Dump(indent + 1);
      if (functype->paramTypeList.size() - 1 != j++) {
        LogInfo::MapleLogger() << ", ";
      }
    }
    if (functype->isVarArgs) {
      LogInfo::MapleLogger() << ", ...";
    }
    LogInfo::MapleLogger() << ") ";
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(functype->retTyIdx)->Dump(indent + 1);
    LogInfo::MapleLogger() << " */" << std::endl;

    PrintIndentation(indent);
    LogInfo::MapleLogger() << "/* ";
    LogInfo::MapleLogger() << NameMangler::DecodeName(GlobalTables::GetGsymTable().GetSymbolFromStIdx(m.first.Idx())->GetName());
    LogInfo::MapleLogger() << " */" << std::endl;

    PrintIndentation(indent);
    LogInfo::MapleLogger() << "extern \"C\" ";
    // return type
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(functype->retTyIdx)->DumpAsCxx(0);

    LogInfo::MapleLogger() << " " << GlobalTables::GetGsymTable().GetSymbolFromStIdx(m.first.Idx())->GetName() << "( ";

    j = 0;
    for (auto &p : functype->paramTypeList) {
      GlobalTables::GetTypeTable().GetTypeFromTyIdx(p)->DumpAsCxx(indent + 1);
      if (functype->paramTypeList.size() - 1 != j++) {
        LogInfo::MapleLogger() << ", ";
      }
    }
    if (functype->isVarArgs) {
      LogInfo::MapleLogger() << ", ...";
    }
    LogInfo::MapleLogger() << ")";
    if (methods.size() - 1 != i++) {
      LogInfo::MapleLogger() << ";" << std::endl;
    }
  }
}

static void DumpInterfaces(std::vector<TyIdx> interfaces, int indent) {
  uint32 size = interfaces.size();
  for (uint32 i = 0; i < size; i++) {
    LogInfo::MapleLogger() << std::endl;
    PrintIndentation(indent);
    GStrIdx strIdx = GlobalTables::GetTypeTable().GetTypeFromTyIdx(interfaces[i])->nameStrIdx;
    LogInfo::MapleLogger() << "$" << GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
    if (i != size - 1) {
      LogInfo::MapleLogger() << ",";
    }
  }
}

size_t MIRStructType::GetSize() const {
  if (typeKind == kTypeUnion) {
    if (fields.size() == 0) {
      return isCPlusPlus ? 1 : 0;
    }
    size_t maxSize = 0;
    for (size_t i = 0; i < fields.size(); ++i) {
      TyidxFieldAttrPair tfap = GetTyidxFieldAttrPair(i);
      MIRType *fieldType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tfap.first);
      size_t size = RoundUp(fieldType->GetSize(), tfap.second.GetAlign());
      if (maxSize < size) {
        maxSize = size;
      }
    }
    return maxSize;
  }
  // since there may be bitfields, perform a layout process for the fields
  size_t byteOfst = 0;
  size_t bitOfst = 0;
  for (size_t i = 0; i < fields.size(); ++i) {
    TyidxFieldAttrPair tfap = GetTyidxFieldAttrPair(i);
    MIRType *fieldType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tfap.first);
    if (fieldType->typeKind != kTypeBitField) {
      if (byteOfst * 8 < bitOfst) {
        byteOfst = (bitOfst >> 3) + 1;
      }
      byteOfst = RoundUp(byteOfst, std::max(fieldType->GetAlign(), tfap.second.GetAlign()));
      byteOfst += fieldType->GetSize();
      bitOfst = byteOfst * 8;
    } else {
      MIRBitfieldType *bitfType = static_cast<MIRBitfieldType *>(fieldType);
      if (bitfType->fieldSize == 0) {  // special case, for aligning purpose
        bitOfst = RoundUp(bitOfst, GetPrimTypeBitSize(bitfType->primType));
        byteOfst = bitOfst >> 3;
      } else {
        if (RoundDown(bitOfst + bitfType->fieldSize - 1, GetPrimTypeBitSize(bitfType->primType)) !=
            RoundDown(bitOfst, GetPrimTypeBitSize(bitfType->primType))) {
          bitOfst = RoundUp(bitOfst, GetPrimTypeBitSize(bitfType->primType));
          byteOfst = bitOfst >> 3;
        }
        bitOfst += bitfType->fieldSize;
        byteOfst = bitOfst >> 3;
      }
    }
  }
  if (byteOfst * 8 < bitOfst) {
    byteOfst = (bitOfst >> 3) + 1;
  }
  byteOfst = RoundUp(byteOfst, GetAlign());
  if (byteOfst == 0 && isCPlusPlus) {
    return 1;  // empty struct in C++ has size 1
  }
  return byteOfst;
}

uint32 MIRStructType::GetAlign() const {
  if (fields.size() == 0) {
    return 0;
  }
  uint8 maxAlign = 1;
  for (size_t i = 0; i < fields.size(); ++i) {
    TyidxFieldAttrPair tfap = GetTyidxFieldAttrPair(i);
    MIRType *fieldType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tfap.first);
    uint32 algn = fieldType->GetAlign();
    if (fieldType->typeKind == kTypeBitField) {
      algn = GetPrimTypeSize(fieldType->primType);
    } else {
      algn = std::max(algn, tfap.second.GetAlign());
    }
    if (maxAlign < algn) {
      maxAlign = algn;
    }
  }
  return maxAlign;
}

void MIRStructType::DumpFieldsAndMethods(int indent, bool hasMethod) const {
  DumpFields(fields, indent);
  bool hasField = !fields.empty();
  bool hasStaticField = !staticFields.empty();
  if (hasField && hasStaticField) {
    LogInfo::MapleLogger() << ",";
  }
  DumpFields(staticFields, indent, true);
  bool hasFieldOrStaticField = hasField || hasStaticField;
  bool hasParentField = !parentFields.empty();
  if (hasFieldOrStaticField && hasParentField) {
    LogInfo::MapleLogger() << ",";
  }
  DumpFields(parentFields, indent, true);
  if ((hasFieldOrStaticField || hasParentField) && hasMethod) {
    LogInfo::MapleLogger() << ",";
  }
  DumpMethods(methods, indent);
}

void MIRStructType::Dump(int indent, bool dontUseName) const {
  if (!dontUseName && CheckAndDumpTypeName(nameStrIdx, nameIsLocal)) {
    return;
  }
  LogInfo::MapleLogger() << ((typeKind == kTypeStruct) ? "<struct {"
                                                       : ((typeKind == kTypeUnion) ? "<union {"
                                                                                   : "<structincomplete {"));
  DumpFieldsAndMethods(indent, !methods.empty());
  LogInfo::MapleLogger() << "}>";
}

uint32 MIRClassType::GetInfo(GStrIdx strIdx) const {
  return GetInfoFromStrIdx(info, strIdx);
}

// return class id or superclass id accroding to input string
uint32 MIRClassType::GetInfo(const std::string &infoStr) const {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName(infoStr);
  return GetInfo(strIdx);
}

bool MIRClassType::IsFinal() const {
  uint32 attrstridx = GetInfo(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("INFO_attribute_string"));
  CHECK(attrstridx < GlobalTables::GetStrTable().StringTableSize(), "out of range of vector");
  const std::string kAttrstring = GlobalTables::GetStrTable().GetStringFromStrIdx(attrstridx);
  return kAttrstring.find(" final ") != std::string::npos;
}

bool MIRClassType::IsAbstract() const {
  uint32 attrStrIdx = GetInfo(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("INFO_attribute_string"));
  CHECK(attrStrIdx < GlobalTables::GetStrTable().StringTableSize(), "out of range of vector");
  const std::string &kAttrString = GlobalTables::GetStrTable().GetStringFromStrIdx(GStrIdx(attrStrIdx));
  return kAttrString.find(" abstract ") != std::string::npos;
}

bool MIRClassType::IsInner() const {
  const std::string kName = GetName();
  return kName.find("_24") != std::string::npos;
}

static void DumpInfoPragmaStaticValue(const std::vector<MIRInfoPair> &info, const std::vector<MIRPragma*> &pragmaVec,
                                      const MIREncodedArray &staticValue, int indent, bool hasFieldMethodOrInterface) {
  bool hasPragma = pragmaVec.size();
  bool hasStaticValue = staticValue.size();
  if (!info.empty() && (hasPragma || hasStaticValue || hasFieldMethodOrInterface)) {
    LogInfo::MapleLogger() << ",";
  }
  size_t size = pragmaVec.size();
  for (size_t i = 0; i < size; ++i) {
    pragmaVec[i]->Dump(indent);
    if (i != size - 1) {
      LogInfo::MapleLogger() << ",";
    }
  }
  if (hasPragma && (hasStaticValue || hasFieldMethodOrInterface)) {
    LogInfo::MapleLogger() << ",";
  }
  DumpStaticValue(staticValue, indent);
  if (hasStaticValue && hasFieldMethodOrInterface) {
    LogInfo::MapleLogger() << ",";
  }
}

void MIRClassType::Dump(int indent, bool dontUseName) const {
  if (!dontUseName && CheckAndDumpTypeName(nameStrIdx, nameIsLocal)) {
    return;
  }
  LogInfo::MapleLogger() << ((typeKind == kTypeClass) ? "<class " : "<classincomplete ");
  if (parentTyIdx != 0) {
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyIdx)->Dump(indent + 1);
    LogInfo::MapleLogger() << " ";
  }
  LogInfo::MapleLogger() << "{";
  DumpClassOrInterfaceInfo(*this, indent);
  bool hasFieldMethodOrInterface = !(fields.empty() && parentFields.empty() && staticFields.empty() &&
                                     methods.empty() && interfacesImplemented.empty());
  DumpInfoPragmaStaticValue(info, pragmaVec, staticValue, indent, hasFieldMethodOrInterface);

  bool hasMethod = !methods.empty();
  bool hasImplementedInterface = !interfacesImplemented.empty();
  DumpFieldsAndMethods(indent, hasMethod || hasImplementedInterface);
  if (hasMethod && hasImplementedInterface) {
    LogInfo::MapleLogger() << ",";
  }
  DumpInterfaces(interfacesImplemented, indent);
  LogInfo::MapleLogger() << "}>";
}

void MIRClassType::DumpAsCxx(int indent) const {
  LogInfo::MapleLogger() << "{" << std::endl;
  DumpFieldsAsCxx(fields, indent);
  LogInfo::MapleLogger() << "};" << std::endl;
  DumpConstructorsAsCxx(methods, 0);
}

void MIRInterfaceType::Dump(int indent, bool dontUseName) const {
  if (!dontUseName && CheckAndDumpTypeName(nameStrIdx, nameIsLocal)) {
    return;
  }
  LogInfo::MapleLogger() << ((typeKind == kTypeInterface) ? "<interface " : "<interfaceincomplete ");
  for (TyIdx idx : parentsTyIdx) {
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(idx)->Dump(0);
    LogInfo::MapleLogger() << " ";
  }
  LogInfo::MapleLogger() << " {";
  DumpClassOrInterfaceInfo(*this, indent);
  bool hasFieldOrMethod = !(fields.empty() && staticFields.empty() && parentFields.empty() && methods.empty());
  DumpInfoPragmaStaticValue(info, pragmaVec, staticValue, indent, hasFieldOrMethod);
  DumpFieldsAndMethods(indent, !methods.empty());
  LogInfo::MapleLogger() << "}>";
}

void MIRTypeByName::Dump(int indent, bool dontUseName) const {
  const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(nameStrIdx);
  LogInfo::MapleLogger() << (nameIsLocal ? "<%" : "<$") << name << ">";
}

void MIRTypeParam::Dump(int indent, bool dontUseName) const {
  LogInfo::MapleLogger() << "<";
  const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(nameStrIdx);
  LogInfo::MapleLogger() << "!" << name << ">";
}

void MIRInstantVectorType::Dump(int indent, bool dontUseName) const {
  LogInfo::MapleLogger() << "{";
  for (uint32 i = 0; i < instantVec.size(); i++) {
    TypePair tpair = instantVec[i];
    if (i != 0) {
      LogInfo::MapleLogger() << ", ";
    }
    MIRType *typeparmty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tpair.first);
    const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(typeparmty->nameStrIdx);
    LogInfo::MapleLogger() << "!" << name;
    LogInfo::MapleLogger() << "=";
    MIRType *realty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tpair.second);
    realty->Dump(0);
  }
  LogInfo::MapleLogger() << "}";
}

void MIRGenericInstantType::Dump(int indent, bool dontUseName) const {
  LogInfo::MapleLogger() << "<";
  MIRType *genericty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(genericTyIdx);
  DumpTypename(genericty->nameStrIdx, genericty->nameIsLocal);
  MIRInstantVectorType::Dump(indent, dontUseName);
  LogInfo::MapleLogger() << ">";
}

bool MIRType::Equalto(const MIRType &mirType) const {
  return typeKind == mirType.typeKind && primType == mirType.primType;
}

bool MIRPtrType::Equalto(const MIRType &type) const {
  if (typeKind != type.GetKind() || GetPrimType() != type.GetPrimType()) {
    return false;
  }
  const auto &pType = static_cast<const MIRPtrType&>(type);
  return pointedTyIdx == pType.pointedTyIdx && typeAttrs == pType.typeAttrs;
}

bool MIRArrayType::Equalto(const MIRType &type) const {
  if (type.typeKind != typeKind) {
    return false;
  }
  const MIRArrayType *p = dynamic_cast<const MIRArrayType*>(&type);
  CHECK_FATAL(p != nullptr, "null ptr check ");
  if (dim != p->dim || eTyIdx != p->eTyIdx || typeAttrs != p->typeAttrs) {
    return false;
  }

  for (int i = 0; i < dim; i++) {
    if (sizeArray[i] != p->sizeArray[i]) {
      return false;
    }
  }
  return true;
}

MIRType *MIRArrayType::GetElemType() const {
  return GlobalTables::GetTypeTable().GetTypeFromTyIdx(eTyIdx);
}

std::string MIRArrayType::GetMplTypeName() const {
  std::stringstream ss;
  ss << "<[] ";
  MIRType *elemType = GetElemType();
  ss << elemType->GetMplTypeName();
  ss << ">";
  return ss.str();
}

bool MIRArrayType::HasFields() const {
  MIRType *elemType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(eTyIdx);
  return elemType->HasFields();
}

size_t MIRArrayType::NumberOfFieldIDs() {
  MIRType *elemType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(eTyIdx);
  return elemType->NumberOfFieldIDs();
}

MIRStructType *MIRArrayType::EmbeddedStructType() {
  MIRType *elemType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(eTyIdx);
  return elemType->EmbeddedStructType();
}

MIRType *MIRFarrayType::GetElemType()  const {
  return GlobalTables::GetTypeTable().GetTypeFromTyIdx(elemTyIdx);
}

bool MIRFarrayType::Equalto(const MIRType &ty) const {
  if (ty.typeKind != typeKind) {
    return false;
  }
  const MIRFarrayType *pty = dynamic_cast<const MIRFarrayType*>(&ty);
  CHECK_FATAL(pty, "make sure the elemTyIdx is not nullptr");
  return elemTyIdx == pty->elemTyIdx;
}

std::string MIRFarrayType::GetMplTypeName() const {
  std::stringstream ss;
  ss << "<[] ";
  MIRType *elemType = GetElemType();
  ss << elemType->GetMplTypeName();
  ss << ">";
  return ss.str();
}

bool MIRFarrayType::HasFields() const {
  MIRType *elemType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(elemTyIdx);
  return elemType->HasFields();
}

size_t MIRFarrayType::NumberOfFieldIDs() {
  MIRType *elemType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(elemTyIdx);
  return elemType->NumberOfFieldIDs();
}

MIRStructType *MIRFarrayType::EmbeddedStructType() {
  MIRType *elemType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(elemTyIdx);
  return elemType->EmbeddedStructType();
}

bool MIRFuncType::Equalto(const MIRType &type) const {
  if (type.typeKind != typeKind) {
    return false;
  }
  const auto &pType = static_cast<const MIRFuncType&>(type);
  return (pType.retTyIdx == retTyIdx && pType.paramTypeList == paramTypeList &&
          pType.isVarArgs == isVarArgs && pType.paramAttrsList == paramAttrsList);
}

bool MIRBitfieldType::Equalto(const MIRType &ty) const {
  if (ty.typeKind != typeKind) {
    return false;
  }
  const MIRBitfieldType *p = dynamic_cast<const MIRBitfieldType*>(&ty);
  CHECK_FATAL(p != nullptr, " null ptr check ");
  return p->primType == primType &&
         //  p->typeKind == typeKind &&
         p->fieldSize == fieldSize;
}

bool MIRStructType::Equalto(const MIRType &ty) const {
  if (ty.typeKind != typeKind) {
    return false;
  }
  const MIRStructType *p = dynamic_cast<const MIRStructType*>(&ty);
  CHECK_FATAL(p != nullptr, "p is null in MIRStructType::Equalto");
  if (fields != p->fields) {
    return false;
  }
  if (staticFields != p->staticFields) {
    return false;
  }
  if (parentFields != p->parentFields) {
    return false;
  }
  if (methods != p->methods) {
    return false;
  }
  return true;
}

std::string MIRStructType::GetCompactMplTypeName() const {
  return GlobalTables::GetStrTable().GetStringFromStrIdx(nameStrIdx);
}

MIRType *MIRStructType::GetElemType(uint32 n) const {
  return GlobalTables::GetTypeTable().GetTypeFromTyIdx(GetElemTyidx(n));
}

MIRType *MIRStructType::GetFieldType(FieldID fieldID) {
  if (fieldID == 0) {
    return this;
  }
  FieldPair fldpair = TraverseToField(fieldID);
  return GlobalTables::GetTypeTable().GetTypeFromTyIdx(fldpair.second.first);
}

bool MIRStructType::IsLocal() const {
  return GlobalTables::GetGsymTable().GetStIdxFromStrIdx(nameStrIdx).Idx() != 0;
}

std::string MIRStructType::GetMplTypeName() const {
  std::stringstream ss;
  ss << "<$";
  ss << GlobalTables::GetStrTable().GetStringFromStrIdx(nameStrIdx);
  ss << ">";
  return ss.str();
}

bool MIRClassType::Equalto(const MIRType &ty) const {
  if (ty.typeKind != typeKind) {
    return false;
  }
  bool structeq = MIRStructType::Equalto(ty);
  if (!structeq) {
    return false;
  }

  const MIRClassType &classty = static_cast<const MIRClassType &>(ty);
  // classes have parent except empty/thirdparty classes
  if (parentTyIdx != classty.parentTyIdx) {
    return false;
  }

  if (interfacesImplemented != classty.interfacesImplemented) {
    return false;
  }
  if (info != classty.info) {
    return false;
  }
  if (infoIsString != classty.infoIsString) {
    return false;
  }
  return true;
}

bool MIRInterfaceType::Equalto(const MIRType &ty) const {
  if (ty.typeKind != typeKind) {
    return false;
  }
  bool structeq = MIRStructType::Equalto(ty);
  if (!structeq) {
    return false;
  }

  const MIRInterfaceType &interfacety = static_cast<const MIRInterfaceType &>(ty);
  if (parentsTyIdx != interfacety.parentsTyIdx) {
    return false;
  }
  if (info != interfacety.info) {
    return false;
  }
  if (infoIsString != interfacety.infoIsString) {
    return false;
  }
  return true;
}

bool MIRTypeByName::Equalto(const MIRType &ty) const {
  if (ty.typeKind != typeKind) {
    return false;
  }
  const MIRTypeByName *pty = dynamic_cast<const MIRTypeByName*>(&ty);
  CHECK_FATAL(pty != nullptr, "null ptr check ");
  return nameStrIdx == pty->nameStrIdx && nameIsLocal == pty->nameIsLocal;
}

bool MIRTypeParam::Equalto(const MIRType &ty) const {
  if (ty.typeKind != typeKind) {
    return false;
  }
  const MIRTypeParam *pty = dynamic_cast<const MIRTypeParam*>(&ty);
  CHECK_FATAL(pty != nullptr, "null ptr check");
  return nameStrIdx == pty->nameStrIdx;
}

bool MIRInstantVectorType::Equalto(const MIRType &ty) const {
  if (ty.typeKind != typeKind) {
    return false;
  }
  const MIRInstantVectorType *p = dynamic_cast<const MIRInstantVectorType*>(&ty);
  CHECK_FATAL(p, "null ptr check ");
  if (instantVec.size() != p->instantVec.size()) {
    return false;
  }
  for (uint32 i = 0; i < instantVec.size(); i++) {
    if (instantVec[i] != p->instantVec[i]) {
      return false;
    }
  }
  return true;
}

bool MIRGenericInstantType::Equalto(const MIRType &ty) const {
  if (!MIRInstantVectorType::Equalto(ty)) {
    return false;
  }
  const MIRGenericInstantType *p = dynamic_cast<const MIRGenericInstantType*>(&ty);
  CHECK_FATAL(p, "null ptr check ");
  return genericTyIdx == p->genericTyIdx;
}

// in the search, curfieldid is being decremented until it reaches 1
FieldPair MIRStructType::TraverseToFieldRef(FieldID &fieldID) const {
  if (!fields.size()) {
    return FieldPair(GStrIdx(0), TyidxFieldAttrPair(TyIdx(0), FieldAttrs()));
  }

  uint32 fieldidx = 0;
  FieldPair curpair = fields[0];
  while (fieldID > 1) {
    fieldID--;
    MIRType *curfieldtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(curpair.second.first);
    MIRStructType *substructty = curfieldtype->EmbeddedStructType();
    if (substructty != nullptr) {
      curpair = substructty->TraverseToFieldRef(fieldID);
      if (fieldID == 1 && curpair.second.first != TyIdx(0)) {
        return curpair;
      }
    }
    fieldidx++;
    if (fieldidx == fields.size()) {
      return FieldPair(GStrIdx(0), TyidxFieldAttrPair(TyIdx(0), FieldAttrs()));
    }
    curpair = fields[fieldidx];
  }
  return curpair;
}

FieldPair MIRStructType::TraverseToField(FieldID fieldID) const {
  if (fieldID >= 0) {
    return TraverseToFieldRef(fieldID);
  }
  // in parentFields
  uint32 prntfieldidx = -fieldID;
  if (parentFields.size() == 0 || prntfieldidx > parentFields.size()) {
    return FieldPair(GStrIdx(0), TyidxFieldAttrPair(TyIdx(0), FieldAttrs()));
  }
  return parentFields[prntfieldidx - 1];
}

FieldPair MIRStructType::TraverseToField(GStrIdx fieldstridx) const {
  if (fields.size() == 0 && staticFields.size() == 0) {
    return FieldPair(GStrIdx(0), TyidxFieldAttrPair(TyIdx(0), FieldAttrs()));
  }
  if (fields.size() > 0) {
    for (FieldPair field : fields) {
      if (field.first == fieldstridx) {
        return field;
      }
    }
  }
  if (staticFields.size() > 0) {
    for (FieldPair field : staticFields) {
      if (field.first == fieldstridx) {
        return field;
      }
    }
  }
  if (parentFields.size() > 0) {
    for (FieldPair field : parentFields) {
      if (field.first == fieldstridx) {
        return field;
      }
    }
  }
  return FieldPair(GStrIdx(0), TyidxFieldAttrPair(TyIdx(0), FieldAttrs()));
}

// go through all the fields to check if there is volatile attribute set;
bool MIRStructType::HasVolatileField() {
  if (hasVolatileFieldSet) {
    return hasVolatileField;
  }
  hasVolatileFieldSet = true;
  for (uint32 i = 0; i < fields.size(); i++) {
    if (fields[i].second.second.GetAttr(FLDATTR_volatile)) {
      hasVolatileField = true;
      return true;
    }
    MIRType *fldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fields[i].second.first);
    MIRTypeKind fldknd = fldty->GetKind();
    if (fldknd == kTypeStruct || fldknd == kTypeStructIncomplete || fldknd == kTypeClass ||
        fldknd == kTypeClassIncomplete || fldknd == kTypeInterface || fldknd == kTypeInterfaceIncomplete) {
      if (static_cast<MIRStructType *>(fldty)->HasVolatileField()) {
        hasVolatileField = true;
        return true;
      }
    }
  }
  for (uint32 i = 0; i < parentFields.size(); i++) {
    if (parentFields[i].second.second.GetAttr(FLDATTR_volatile)) {
      hasVolatileField = true;
      return true;
    }
    MIRType *fldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentFields[i].second.first);
    MIRTypeKind fldknd = fldty->GetKind();
    if (fldknd == kTypeStruct || fldknd == kTypeStructIncomplete || fldknd == kTypeClass ||
        fldknd == kTypeClassIncomplete || fldknd == kTypeInterface || fldknd == kTypeInterfaceIncomplete)
      if (static_cast<MIRStructType *>(fldty)->HasVolatileField()) {
        hasVolatileField = true;
        return true;
      }
  }
  if (GetKind() == kTypeClass || GetKind() == kTypeClassIncomplete) {
    MIRClassType *thisclassty = static_cast<MIRClassType *>(this);
    if (thisclassty->parentTyIdx != 0) {
      // check if parent class has volatile field
      MIRType *parentty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thisclassty->parentTyIdx);
      if (static_cast<MIRStructType *>(parentty)->HasVolatileField()) {
        hasVolatileField = true;
        return true;
      }
    }
  } else if (GetKind() == kTypeInterface || GetKind() == kTypeInterfaceIncomplete) {
    MIRInterfaceType *thisinterfacety = static_cast<MIRInterfaceType *>(this);
    // check if the parent classes have volatile field
    for (uint32 i = 0; i < thisinterfacety->parentsTyIdx.size(); i++) {
      TyIdx prnttyidx = thisinterfacety->parentsTyIdx[i];
      MIRType *prntty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(prnttyidx);
      if (static_cast<MIRStructType *>(prntty)->HasVolatileField()) {
        hasVolatileField = true;
        return true;
      }
    }
  }
  hasVolatileField = false;
  return false;
}

// go through all the fields to check if there is generic attribute set;
bool MIRStructType::HasTypeParam() const {
  for (uint32 i = 0; i < fields.size(); i++)
    if (fields[i].second.second.GetAttr(FLDATTR_generic)) {
      return true;
    }
  for (uint32 i = 0; i < parentFields.size(); i++)
    if (parentFields[i].second.second.GetAttr(FLDATTR_generic)) {
      return true;
    }
  if (GetKind() == kTypeClass || GetKind() == kTypeClassIncomplete) {
    const MIRClassType *thisclassty = static_cast<const MIRClassType *>(this);
    if (thisclassty->parentTyIdx != 0) {
      // check if parent class has type parameter
      MIRType *parentty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thisclassty->parentTyIdx);
      if (parentty->HasTypeParam()) {
        return true;
      }
    }
  } else if (GetKind() == kTypeInterface || GetKind() == kTypeInterfaceIncomplete) {
    const MIRInterfaceType *thisinterfacety = static_cast<const MIRInterfaceType *>(this);
    // check if the parent classes have type parameter
    for (uint32 i = 0; i < thisinterfacety->parentsTyIdx.size(); i++) {
      TyIdx prnttyidx = thisinterfacety->parentsTyIdx[i];
      MIRType *prntty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(prnttyidx);
      if (prntty->HasTypeParam()) {
        return true;
      }
    }
  }
  return false;
}

size_t MIRClassType::NumberOfFieldIDs() {
  size_t parentFieldIDs = 0;
  if (parentTyIdx != TyIdx(0)) {
    MIRType *parentty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyIdx);
    parentFieldIDs = parentty->NumberOfFieldIDs();
  }
  return parentFieldIDs + MIRStructType::NumberOfFieldIDs();
}

FieldPair MIRClassType::TraverseToFieldRef(FieldID &fieldID) const {
  if (parentTyIdx != TyIdx(0)) {
    MIRClassType *parentclassty = MIR_DYN_CAST(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyIdx), MIRClassType *);
    if (parentclassty) {
      fieldID--;
      FieldPair curpair = parentclassty->TraverseToFieldRef(fieldID);
      if (fieldID == 1 && curpair.second.first != TyIdx(0)) {
        return curpair;
      }
    }
  }
  return MIRStructType::TraverseToFieldRef(fieldID);
}

// fields in interface are all static and are global, won't be accessed through fields
FieldPair MIRInterfaceType::TraverseToFieldRef(FieldID &fieldID) const {
  return FieldPair(GStrIdx(0), TyidxFieldAttrPair(TyIdx(0), FieldAttrs()));
  ;
}

TyidxFieldAttrPair MIRPtrType::GetPointedTyidxFldAttrPairWithFieldId(FieldID fldid) const {
  if (fldid == 0) {
    return TyidxFieldAttrPair(pointedTyIdx, FieldAttrs());
  }
  MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointedTyIdx);
  MIRStructType *structty = ty->EmbeddedStructType();
  CHECK_FATAL(structty,
         "MIRPtrType::GetPointedTyidxWithFieldId(): cannot have non-zero fieldID for something other than a struct");
  return structty->TraverseToField(fldid).second;
}

TyIdx MIRPtrType::GetPointedTyidxWithFieldId(FieldID fldid) const {
  return GetPointedTyidxFldAttrPairWithFieldId(fldid).first;
}

std::string MIRPtrType::GetMplTypeName() const {
  std::stringstream ss;
  ss << "<* ";
  MIRType *pointedType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointedTyIdx);
  CHECK_FATAL(pointedType != nullptr, "invalid ptr type");
  ss << pointedType->GetMplTypeName();
  ss << ">";
  return ss.str();
}

std::string MIRPtrType::GetCompactMplTypeName() const {
  MIRType *pointedType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointedTyIdx);
  CHECK_FATAL(pointedType != nullptr, "invalid ptr type");
  return pointedType->GetCompactMplTypeName();
}

size_t MIRStructType::NumberOfFieldIDs() {
  size_t count = 0;
  for (FieldPair curpair : fields) {
    count++;
    MIRType *curfieldtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(curpair.second.first);
    count += curfieldtype->NumberOfFieldIDs();
  }
  return count;
}

TypeAttrs FieldAttrs::ConvertToTypeAttrs() {
  TypeAttrs attr;
  for (uint32 i = 0; i < 64; i++) {
    if ((attrFlag & (1ULL << i)) == 0) {
      continue;
    }
    FldAttrKind tA = static_cast<FldAttrKind>(i);
    switch (tA) {
#define FIELD_ATTR
#define ATTR(STR)               \
    case FLDATTR_##STR:         \
      attr.SetAttr(ATTR_##STR); \
      break;
#include "all_attributes.def"
#undef ATTR
#undef FIELD_ATTR
      default:
        CHECK_FATAL(false, "unknown TypeAttrs");
        break;
    }
  }
  return attr;
}

TypeAttrs GenericAttrs::ConvertToTypeAttrs() {
  TypeAttrs attr;
  for (uint32 i = 0; i < 64; i++) {
    if ((attrFlag & (1ULL << i)) == 0) {
      continue;
    }
    GenericAttrKind tA = static_cast<GenericAttrKind>(i);
    switch (tA) {
#define TYPE_ATTR
#define ATTR(STR)               \
    case GENATTR_##STR:         \
      attr.SetAttr(ATTR_##STR); \
      break;
#include "all_attributes.def"
#undef ATTR
#undef TYPE_ATTR
      default:
        CHECK_FATAL(false, "unknown TypeAttrs");
        break;
    }
  }
  return attr;
}

FuncAttrs GenericAttrs::ConvertToFuncAttrs() {
  FuncAttrs attr;
  for (uint32 i = 0; i < 64; i++) {
    if ((attrFlag & (1ULL << i)) == 0) {
      continue;
    }
    GenericAttrKind tA = static_cast<GenericAttrKind>(i);
    switch (tA) {
#define FUNC_ATTR
#define ATTR(STR)                   \
    case GENATTR_##STR:             \
      attr.SetAttr(FUNCATTR_##STR); \
      break;
#include "all_attributes.def"
#undef ATTR
#undef FUNC_ATTR
      default:
        CHECK_FATAL(false, "unknown FuncAttrs");
        break;
    }
  }
  return attr;
}

FieldAttrs GenericAttrs::ConvertToFieldAttrs() {
  FieldAttrs attr;
  for (uint32 i = 0; i < 64; i++) {
    if ((attrFlag & (1ULL << i)) == 0) {
      continue;
    }
    GenericAttrKind tA = static_cast<GenericAttrKind>(i);
    switch (tA) {
#define FIELD_ATTR
#define ATTR(STR)                  \
    case GENATTR_##STR:            \
      attr.SetAttr(FLDATTR_##STR); \
      break;
#include "all_attributes.def"
#undef ATTR
#undef FIELD_ATTR
      default:
        CHECK_FATAL(false, "unknown FieldAttrs");
        break;
    }
  }
  return attr;
}

}  // namespace maple
#endif  // MIR_FEATURE_FULL
