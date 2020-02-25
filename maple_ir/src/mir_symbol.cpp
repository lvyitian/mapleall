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

#include <algorithm>
#include <unordered_set>
#include "mir_symbol.h"
#include "mir_function.h"
#include "class_init.h"
#include "vtable_analysis.h"
#include "reflection_analysis.h"
#include "printing.h"
#include "native_stub_func.h"
#include "literal_str_name.h"

namespace maple {

MIRType *MIRSymbol::GetType() const {
  return GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
}

const std::string &MIRSymbol::GetName() const {
  return GlobalTables::GetStrTable().GetStringFromStrIdx(nameStrIdx);
}

bool MIRSymbol::HasAddrOfValues() {
  return (GetName().find(VTAB_PREFIX_STR) == 0 || GetName().find(ITAB_PREFIX_STR) == 0 ||
          GetName().find(NameMangler::kVtabOffsetTabStr) == 0 || IsClassInitBridge() || IsReflectionInfo() ||
          IsReflectionHashTabBucket() || IsReflectionStrtab() || IsItabConflictInfo() || IsRegJNITab() ||
          IsRegJNIFuncTab() || IsLiteral());
}

bool MIRSymbol::IsLiteral() const {
  return (GetName().find(kConstString) == 0);
}

bool MIRSymbol::IsLiteralPtr() const {
  return (GetName().find(kConstStringPtr) == 0);
}

bool MIRSymbol::PointsToConstString() const {
  MIRType *origType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  if (origType->typeKind == kTypePointer) {
    return static_cast<MIRPtrType *>(origType)->PointsToConstString();
  }
  return false;
}

bool MIRSymbol::IsConstString() const {
  return typeAttrs.GetAttr(ATTR_static) && typeAttrs.GetAttr(ATTR_final) && PointsToConstString();
}

bool MIRSymbol::IsReflectionStrtab() {
  return ((GetName().find(NameMangler::kReflectionStrtabPrefixStr) == 0) ||
          (GetName().find(NameMangler::kReflectionStartHotStrtabPrefixStr) == 0) ||
          (GetName().find(NameMangler::kReflectionBothHotStrTabPrefixStr) == 0) ||
          (GetName().find(NameMangler::kReflectionRunHotStrtabPrefixStr) == 0));
}

bool MIRSymbol::IsRegJNITab() {
  return (GetName().find(NameMangler::kRegJNITabPrefixStr) == 0);
}

bool MIRSymbol::IsRegJNIFuncTab() {
  return (GetName().find(NameMangler::kRegJNIFuncTabPrefixStr) == 0);
}

bool MIRSymbol::IsMuidTab() const {
  return (GetName().find(NameMangler::kMuidPrefixStr) == 0);
}

bool MIRSymbol::IsCodeLayoutInfo() const {
  return (GetName().find(NameMangler::kFunctionLayoutStr) == 0);
}

std::string MIRSymbol::GetMuidTabName() {
  if (!IsMuidTab()) {
    return "";
  }

  size_t idx = GetName().find(NameMangler::kFileNameSplitterStr);
  return ((idx != std::string::npos) ? GetName().substr(0, idx) : "");
}

bool MIRSymbol::IsMuidFuncDefTab() const {
  return (GetName().find(NameMangler::kMuidFuncDefTabPrefixStr) == 0);
}

bool MIRSymbol::IsMuidFuncInfTab() const {
  return (GetName().find(NameMangler::kMuidFuncInfTabPrefixStr) == 0);
}

bool MIRSymbol::IsMuidFuncUndefTab() const {
  return (GetName().find(NameMangler::kMuidFuncUndefTabPrefixStr) == 0);
}

bool MIRSymbol::IsMuidDataDefTab() const {
  return (GetName().find(NameMangler::kMuidDataDefTabPrefixStr) == 0);
}

bool MIRSymbol::IsMuidDataUndefTab() const {
  return (GetName().find(NameMangler::kMuidDataUndefTabPrefixStr) == 0);
}

bool MIRSymbol::IsMuidFuncDefMuidTab() const {
  return (GetName().find(NameMangler::kMuidFuncDefMuidTabPrefixStr) == 0);
}

bool MIRSymbol::IsMuidFuncUndefMuidTab() const {
  return (GetName().find(NameMangler::kMuidFuncUndefMuidTabPrefixStr) == 0);
}

bool MIRSymbol::IsMuidDataDefMuidTab() const {
  return (GetName().find(NameMangler::kMuidDataDefMuidTabPrefixStr) == 0);
}

bool MIRSymbol::IsMuidDataUndefMuidTab() const {
  return (GetName().find(NameMangler::kMuidDataUndefMuidTabPrefixStr) == 0);
}

bool MIRSymbol::IsMuidRangeTab() const {
  return (GetName().find(NameMangler::kMuidRangeTabPrefixStr) == 0);
}

bool MIRSymbol::IsForcedGlobalFunc() const {
  return (GetName().find(NameMangler::kJavaLangClassStr) == 0 || GetName().find(NameMangler::kReflectionClassesPrefixStr) == 0);
}

// mrt/maplert/include/mrt_classinfo.h
bool MIRSymbol::IsForcedGlobalClassinfo() const {
  std::unordered_set<string> mrtUse{
#include "mrt_direct_classinfo_list.def"
  };
  return (std::find(mrtUse.begin(), mrtUse.end(), GetName()) != mrtUse.end() ||
          GetName().find("__cinf_Llibcore_2Freflect_2FGenericSignatureParser_3B") == 0);
}

bool MIRSymbol::IsForcedGlobalStaticfield() const { return false; }

bool MIRSymbol::IsClassInitBridge() {
  return (GetName().find(CLASS_INIT_BRIDGE_PREFIX_STR) == 0);
}

bool MIRSymbol::IsReflectionHashTabBucket() {
  return (GetName().compare(0, strlen(NameMangler::kMuidClassMetadataBucketPrefixStr), NameMangler::kMuidClassMetadataBucketPrefixStr) ==
          0);
}

bool MIRSymbol::IsReflectionInfo() {
  return (IsReflectionClassInfo() || IsReflectionClassInfoRO() || IsReflectionFieldInfo() ||
          IsReflectionFieldInfoRO() || IsReflectionFieldInfoCompact() || IsReflectionMethodInfo() ||
          IsReflectionMethodInfoRO() || IsReflectionMethodInfoCompact() || IsReflectionPrimitiveClassInfo() ||
          IsReflectionSuperclassInfo());
}

bool MIRSymbol::IsReflectionFieldInfo() {
  return (GetName().find(NameMangler::kFieldsInfoPrefixStr) == 0);
}

bool MIRSymbol::IsReflectionFieldInfoRO() {
  return (GetName().find(FIELDINFO_RO_PREFIX_STR) == 0);
}

bool MIRSymbol::IsReflectionFieldInfoCompact() const {
  return (GetName().find(NameMangler::kFieldsInfoCompactPrefixStr) == 0);
}

bool MIRSymbol::IsReflectionSuperclassInfo() {
  return (GetName().find(SUPERCLASSINFO_PREFIX_STR) == 0);
}

bool MIRSymbol::IsReflectionClassInfo() {
  return (GetName().find(CLASSINFO_PREFIX_STR) == 0);
}

bool MIRSymbol::IsReflectionArrayClassInfo() const {
  return (GetName().find(NameMangler::kArrayClassInfoPrefixStr) == 0);
}

bool MIRSymbol::IsReflectionClassInfoPtr() const {
  return (GetName().find(NameMangler::kClassINfoPtrPrefixStr) == 0);
}

bool MIRSymbol::IsReflectionClassInfoRO() {
  return (GetName().find(CLASSINFO_RO_PREFIX_STR) == 0);
}

bool MIRSymbol::IsItabConflictInfo() {
  return (GetName().find(ITAB_CONFLICT_PREFIX_STR) == 0);
}

bool MIRSymbol::IsReflectionPrimitiveClassInfo() {
  return (GetName().find(PRIMITIVECLASSINFO_PREFIX_STR) == 0);
}

bool MIRSymbol::IsReflectionMethodInfo() {
  return (GetName().find(NameMangler::kMethodsInfoPrefixStr) == 0);
}

bool MIRSymbol::IsReflectionMethodInfoRO() {
  return (GetName().find(METHODINFO_RO_PREFIX_STR) == 0);
}

bool MIRSymbol::IsReflectionMethodInfoCompact() const {
  return (GetName().find(NameMangler::kMethodsInfoCompactPrefixStr) == 0);
}

bool MIRSymbol::IsPrimordialObject() {
  return IsReflectionClassInfo() || IsReflectionFieldInfo() || IsReflectionMethodInfo() ||
         IsReflectionPrimitiveClassInfo();
}

bool MIRSymbol::IsGctibSym() {
  return (GetName().find(GCTIB_PREFIX_STR) == 0);
}

// [Note]
// Some symbols are ignored by reference counting as they represent objects not managed by us. These include
// string-based exact comparison for "current_vptr", "vtabptr", "itabptr", "funcptr", "env_ptr", "retvar_stubfunc".

GStrIdx MIRSymbol::reflectClassNameIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(NameMangler::kJavaLangClassStr);
GStrIdx MIRSymbol::reflectMethodNameIdx =
  GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(NameMangler::kJavaLangReflectMethod);
GStrIdx MIRSymbol::reflectFieldNameIdx =
  GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(NameMangler::kJavaLangReflectField);

bool MIRSymbol::IgnoreRC() { // is RC needed
  if (isDeleted || GetAttr(ATTR_rcunowned)) {
    return true;
  }

  const std::string &name = GetName();
  // ignore %current_vptr, %vtabptr, %itabptr, %funcptr, %env_ptr
  if (name == "current_vptr" || name == "vtabptr" || name == "itabptr" || name == "funcptr" || name == "env_ptr" ||
      name == "retvar_stubfunc" || name == "_dummy_stub_object_retval") {
    return true;
  }

  if (IsReflectionInfo() || IsRegJNITab() || IsRegJNIFuncTab()) {
    return true;
  }

  MIRType *type = GetType();
  // only consider reference
  if (type == nullptr || type->GetPrimType() != PTY_ref) {
    return true;
  }

  if ((type->GetKind() == kTypeScalar) && (name != "__mapleRC__")) {
    return true;
  }
  // ignore ptr to types Ljava_2Flang_2FClass_3B,
  // Ljava_2Flang_2Freflect_2FMethod_3B, and Ljava_2Flang_2Freflect_2FField_3B
  MIRPtrType *ptype = static_cast<MIRPtrType *>(type);
  GStrIdx strIdx = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptype->pointedTyIdx)->nameStrIdx;
  // LogInfo::MapleLogger() << globaltable.GetStringFromStrIdx(strIdx) << std::endl;
  if (strIdx == reflectClassNameIdx || strIdx == reflectMethodNameIdx || strIdx == reflectFieldNameIdx) {
    return true;
  }

  return false;
}

void MIRSymbol::Dump(bool isLocal, int32 indent, bool suppressinit) const {
  if (storageClass == kScUnused) {
    return;
  }
  if (storageClass == kScFormal) {
    return;
  }
  if (isImported && !appearsincode) {
    return;
  }
  // no need for symbols of extern functions, only need declarations
  if (storageClass == kScExtern && sKind == kStFunc) {
    return;
  }
  if (GetTyIdx().GetIdx() >= GlobalTables::GetTypeTable().typeTable.size()) {
    FATAL(kLncFatal, "valid mapleir with illegal type");
  }
  if (storageClass == kScText && value.mirFunc != nullptr) {
    GetFunction()->Dump(true /*without_body*/);
    return;
  }
  const char *ids = isLocal ? "%" : "$";
  PrintIndentation(indent);
  if (sKind == kStJavaClass) {
    LogInfo::MapleLogger() << "javaclass ";
  } else if (sKind == kStJavaInterface) {
    LogInfo::MapleLogger() << "javainterface ";
  } else if (isTmp) {
    LogInfo::MapleLogger() << "tempvar ";
  } else {
    LogInfo::MapleLogger() << "var ";
  }
  LogInfo::MapleLogger() << ids << GetName() << " ";

  if (storageClass == kScFstatic) {
    LogInfo::MapleLogger() << "fstatic ";
  } else if (storageClass == kScPstatic) {
    LogInfo::MapleLogger() << "pstatic ";
  } else if (storageClass == kScExtern) {
    LogInfo::MapleLogger() << "extern ";
  }

  if (GetTyIdx() != 0) {
    GlobalTables::GetTypeTable().typeTable[GetTyIdx().GetIdx()]->Dump(indent + 1);
  }
  typeAttrs.DumpAttributes();
  if (sKind == kStJavaClass || sKind == kStJavaInterface || storageClass == kScTypeInfoName || storageClass == kScTypeInfo ||
      storageClass == kScTypeCxxAbi) {
    LogInfo::MapleLogger() << std::endl;
    return;
  }
  if (IsConst() && !suppressinit && !(IsLiteral() && storageClass == kScExtern)) {
    LogInfo::MapleLogger() << " = ";
    GetConst()->Dump();
  }
  LogInfo::MapleLogger() << std::endl;
}

void MIRSymbol::DumpAsLiteralVar(int32 indent) {
  if (IsLiteral()) {
    LogInfo::MapleLogger() << GetName();
  }
}

void MIRSymbolTable::Dump(bool isLocal, int32 indent, bool printdeleted) const {
  int32 size = symbolTable.size();
  for (int32 i = 0; i < size; i++) {
    MIRSymbol *symbol = symbolTable[i];
    if (!symbol) {
      continue;
    }
    if (!printdeleted && symbol->IsDeleted()) {
      continue;
    }
    symbol->Dump(isLocal, indent);
  }
  return;
}

LabelIdx MIRLabelTable::CreateLabelWithPrefix(char c) {
  LabelIdx labidx = labelTable.size();
  std::ostringstream lnamestream;
  lnamestream << "@" << c << labidx;
  std::string lname = lnamestream.str();
  GStrIdx nameIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lname);
  labelTable.push_back(nameIdx);
  strIdxToLabIdxMap[nameIdx] = labidx;
  return labidx;
}

const std::string &MIRLabelTable::GetName(LabelIdx labidx) const {
  return GlobalTables::GetStrTable().GetStringFromStrIdx(labelTable.at(labidx));
}

bool MIRLabelTable::AddToStringLabelMap(LabelIdx lidx) {
  if (labelTable[lidx] == GStrIdx(0)) {
    // generate a label name based on lab_idx
    std::ostringstream lnamestream;
    lnamestream << "@" << lidx;
    std::string lname;
    lname = lnamestream.str();
    labelTable[lidx] = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lname);
  }
  GStrIdx strIdx = labelTable.at(lidx);
  strIdxToLabIdxMap[strIdx] = lidx;
  return true;
}

}  // namespace maple
