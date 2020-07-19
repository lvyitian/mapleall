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

#include "vtable_analysis.h"
#include "reflection_analysis.h"
#include "vtable_impl.h"
#include "option.h"
#include "mir_builder.h"
#include "name_mangler.h"
#include "itab_util.h"
#include "name_mangler.h"
#include <unordered_map>
#include <algorithm>
#include <functional>
#include <iomanip>
#include <fstream>

// This file is used to generate classmetadata and classhashmetadata.
//
// Classmetadata is consist of three part :classinfo, fields, methods,
// and we generates these data according to the structure defined in
// reflection_analysis.h && metadata_layout.h then add their address to mirbuilder.

namespace maple {
// +1 is needed here because our field id starts with 0 pointing to the struct itself
#define OBJ_KLASS_FIELDID (CLASS::kShadow + 1)
#define METADATA_KLASS_FIELDID (CLASS::kShadow + 1)

#define TBD_VALUE (0xABCD)
#define MAX_ANNOS_NUM (4000)

#define JAVA_LANG_ANNOTATION_RETENTION_STR (std::string(NameMangler::kJavaLang) + "annotation_2FRetention" + NameMangler::kClassMethodSplitterStr)
#define JAVA_LANG_ENUM_STR (std::string(NameMangler::kJavaLang) + "Enum" + NameMangler::kClassMethodSplitterStr)
#define REFLECTION_METHOD_PREFIX_STR (std::string(NameMangler::kJavaLang) + "reflect_2FMethod" + NameMangler::kClassMethodSplitterStr)
#define REFLECTION_METHOD_241_PREFIX_STR (std::string(NameMangler::kJavaLang) + "reflect_2FMethod_241" + NameMangler::kClassMethodSplitterStr)
#define REFLECTION_FIELD_PREFIX_STR (std::string(NameMangler::kJavaLang) + "reflect_2FField" + NameMangler::kClassMethodSplitterStr)
#define REFLECTION_EXECUTABLE_PREFIX_STR (std::string(NameMangler::kJavaLang) + "reflect_2FExecutable" + NameMangler::kClassMethodSplitterStr)
#define REFLECTION_CONSTRUCTOR_PREFIX_STR (std::string(NameMangler::kJavaLang) + "reflect_2FConstructor" + NameMangler::kClassMethodSplitterStr)
#define REFLECTION_PROXY_PREFIX_STR (std::string(NameMangler::kJavaLang) + "reflect_2FProxy" + NameMangler::kClassMethodSplitterStr)
#define REFLECTION_PROXY_241_PREFIX_STR (std::string(NameMangler::kJavaLang) + "reflect_2FProxy_241" + NameMangler::kClassMethodSplitterStr)
#define REFLECTION_REFERENCE_PREFIX_STR (std::string(NameMangler::kJavaLang) + "ref_2FReference" + NameMangler::kClassMethodSplitterStr)
#ifdef __OPENJDK__
#define JAVA_LANG_CLASS_PREFIX_STR (std::string(NameMangler::kJavaLang)) + "Class_3B"
#endif
#if defined(__x86_64__)
#define SUN_REFLECT_REFLECTION_STR (std::string("Lsun_2Freflect_2FReflection") + NameMangler::kClassMethodSplitterStr)
#endif
#define REFLECTION_ACCESSIBLEOBJECT_PREFIX_STR (std::string(NameMangler::kJavaLang) + "reflect_2FAccessibleObject" + NameMangler::kClassMethodSplitterStr)
#define CLASS_INIT_PROTECT_REGION_STR "__ClassInitProtectRegion__"
#define CLASS_STATE_INITIALIZED_STR "__ClassStateInitialized__"
#define INIT_FUNTION_STR "_3Cinit_3E"
#define INNERCLASS_STR (std::string("annotation_2FInnerClass") + NameMangler::kClassMethodSplitterStr)

static bool raDebug = false;

std::string ReflectionAnalysis::strtab = std::string(1, '\0');
std::unordered_map<std::string, uint32_t> ReflectionAnalysis::str2idxMap;
std::string ReflectionAnalysis::strtabStartHot = std::string(1, '\0');
std::string ReflectionAnalysis::strtabBothHot = std::string(1, '\0');
std::string ReflectionAnalysis::strtabRunHot = std::string(1, '\0');
static bool strtabInited = false;

static uint32_t FirstFindOrInsertRepeatString(const std::string &str, bool isHot, uint32_t hotType) {
  uint32_t index = 0;
  auto it = ReflectionAnalysis::str2idxMap.find(str);
  if (it != ReflectionAnalysis::str2idxMap.end()) {
    index = it->second;
  } else {
    if (isHot) {
      if (hotType == HOT_LAYOUT::kStartUpHot) {
        index =
          ReflectionAnalysis::strtabStartHot.length() | (HOT_LAYOUT::kStartUpHot << 30);  // use the MSB to indicate hotness
        ReflectionAnalysis::strtabStartHot += str;
        ReflectionAnalysis::strtabStartHot += '\0';
      } else if (hotType == HOT_LAYOUT::kBothHot) {
        index =
          ReflectionAnalysis::strtabBothHot.length() | (HOT_LAYOUT::kBothHot << 30);  // use the MSB to indicate hotness
        ReflectionAnalysis::strtabBothHot += str;
        ReflectionAnalysis::strtabBothHot += '\0';
      } else {
        index = ReflectionAnalysis::strtabRunHot.length() | (HOT_LAYOUT::kRunHot << 30);  // use the MSB to indicate hotness
        ReflectionAnalysis::strtabRunHot += str;
        ReflectionAnalysis::strtabRunHot += '\0';
      }
    } else {
      index = ReflectionAnalysis::strtab.length();
      ReflectionAnalysis::strtab += str;
      ReflectionAnalysis::strtab += '\0';
    }
    ReflectionAnalysis::str2idxMap[str] = index;
  }
  return index;
}

uint32_t ReflectionAnalysis::FindOrInsertRepeatString(const std::string &str, bool isHot, uint32_t hotType) {
  if (strtabInited == false) {
    // add default hot strings
    std::string initHot = NameMangler::kJavaLangObjectStrJVersion;
    (void)FirstFindOrInsertRepeatString(initHot, true, HOT_LAYOUT::kStartUpHot);
    strtabInited = true;
  }

  return FirstFindOrInsertRepeatString(str, isHot, hotType);
}

BaseNode *ReflectionAnalysis::GenClassinfoAddr(BaseNode *obj, MIRBuilder *builder) {
  GenMetadataType(builder->mirModule);
  MIRClassType *objectType = static_cast<MIRClassType *>(WKTypes::Util::GetJavaLangObjectType());
  BaseNode *classinfoAddr = nullptr;
  if (objectType && objectType->GetKind() != kTypeClassIncomplete) {
    classinfoAddr = builder->CreateExprIread(GlobalTables::GetTypeTable().GetRef(), GlobalTables::GetTypeTable().GetOrCreatePointerType(objectType),
        OBJ_KLASS_FIELDID, obj);
  } else {
    // If java.lang.Object type is not defined, fall back to use the classinfo struct to retrieve the first field
    MIRStructType *classMetadataType = static_cast<MIRStructType *>(
        GlobalTables::GetTypeTable().GetTypeFromTyIdx(ReflectionAnalysis::classMetadataTyidx));
    classinfoAddr = builder->CreateExprIread(GlobalTables::GetTypeTable().GetRef(),
        GlobalTables::GetTypeTable().GetOrCreatePointerType(classMetadataType), METADATA_KLASS_FIELDID, obj);
  }
  return classinfoAddr;
}

const char *ReflectionAnalysis::klassPtrName = NameMangler::kShadowClassName;
TyIdx ReflectionAnalysis::classMetadataTyidx;
TyIdx ReflectionAnalysis::classMetadataRoTyidx;
TyIdx ReflectionAnalysis::methodMetadataTyidx;
TyIdx ReflectionAnalysis::methodMetadataRoTyidx;
TyIdx ReflectionAnalysis::methodMetadataCompactTyidx;
TyIdx ReflectionAnalysis::fieldMetadataTyidx;
TyIdx ReflectionAnalysis::fieldMetadataRoTyidx;
TyIdx ReflectionAnalysis::fieldMetadataCompactTyidx;
TyIdx ReflectionAnalysis::superclassMetadataTyidx;

#define MOD_PUBLIC 1                  // 0x00000001
#define MOD_PRIVATE 2                 // 0x00000002
#define MOD_PROTECTED 3               // 0x00000004
#define MOD_STATIC 4                  // 0x00000008
#define MOD_FINAL 5                   // 0x00000010
#define MOD_SYNCHRONIZED 6            // 0x00000020
#define MOD_VOLATILE 7                // 0x00000040
#define MOD_TRANSIENT 8               // 0x00000080
#define MOD_NATIVE 9                  // 0x00000100
#define MOD_INTERFACE 10              // 0x00000200
#define MOD_ABSTRACT 11               // 0x00000400
#define MOD_STRICT 12                 // 0x00000800
#define MOD_SYNTHETIC 13              // 0x00001000
#define MOD_CONSTRUCTOR 17            // 0x00010000
#define MOD_DEFAULT 22                // 0x00400000
#define MOD_BRIDGE 7                  // 0x00000040
#define MOD_VARARGS 8                 // 0x00000080
#define MOD_ENUM 15                   // 0x00004000
#define MOD_FINALIZABLE 31            // 0x80000000
#define MOD_DECLARED_SYNCHRONIZED 18  // 0x00020000

#define MODIFIER_RCUNOWNED 24  // 0x00800000
#define MODIFIER_RCWEAK 25     // 0x01000000

uint32 GetMethodModifier(FuncAttrs fa) {
  return (fa.GetAttr(FUNCATTR_public) << (MOD_PUBLIC - 1)) | (fa.GetAttr(FUNCATTR_protected) << (MOD_PROTECTED - 1)) |
         (fa.GetAttr(FUNCATTR_private) << (MOD_PRIVATE - 1)) | (fa.GetAttr(FUNCATTR_abstract) << (MOD_ABSTRACT - 1)) |
         (fa.GetAttr(FUNCATTR_static) << (MOD_STATIC - 1)) | (fa.GetAttr(FUNCATTR_final) << (MOD_FINAL - 1)) |
         (fa.GetAttr(FUNCATTR_declared_synchronized) << (MOD_SYNCHRONIZED - 1)) |
         (fa.GetAttr(FUNCATTR_native) << (MOD_NATIVE - 1)) | (fa.GetAttr(FUNCATTR_strict) << (MOD_STRICT - 1)) |
         (fa.GetAttr(FUNCATTR_synthetic) << (MOD_SYNTHETIC - 1)) | (fa.GetAttr(FUNCATTR_bridge) << (MOD_BRIDGE - 1)) |
         (fa.GetAttr(FUNCATTR_constructor) << (MOD_CONSTRUCTOR - 1)) |
         (fa.GetAttr(FUNCATTR_varargs) << (MOD_VARARGS - 1));
}

uint32 GetFieldModifier(FieldAttrs fa) {
  return (fa.GetAttr(FLDATTR_public) << (MOD_PUBLIC - 1)) | (fa.GetAttr(FLDATTR_protected) << (MOD_PROTECTED - 1)) |
         (fa.GetAttr(FLDATTR_private) << (MOD_PRIVATE - 1)) | (fa.GetAttr(FLDATTR_static) << (MOD_STATIC - 1)) |
         (fa.GetAttr(FLDATTR_final) << (MOD_FINAL - 1)) | (fa.GetAttr(FLDATTR_transient) << (MOD_TRANSIENT - 1)) |
         (fa.GetAttr(FLDATTR_volatile) << (MOD_VOLATILE - 1)) | (fa.GetAttr(FLDATTR_synthetic) << (MOD_SYNTHETIC - 1)) |
         (fa.GetAttr(FLDATTR_enum) << (MOD_ENUM - 1)) | (fa.GetAttr(FLDATTR_rcunowned) << (MODIFIER_RCUNOWNED - 1)) |
         (fa.GetAttr(FLDATTR_rcweak) << (MODIFIER_RCWEAK - 1));
}

uint32 GetClassAccessFlags(MIRClassType *classtype) {
  int32_t accessFlag = 0;
  string flag = "accessFlags";
  for (MIRPragma *prag : classtype->pragmaVec) {
    if (prag->pragmaKind == kPragmaClass) {
      for (MIRPragmaElement *elem : prag->elementVec) {
        string name = GlobalTables::GetStrTable().GetStringFromStrIdx(elem->namestridx_);
        if (name == flag) {
          accessFlag = elem->val_.i;
          return static_cast<uint32>(accessFlag);
        }
      }
    }
  }
  uint32 size = classtype->info.size();
  for (uint32 i = 0; i < size; i++) {
    if (GlobalTables::GetStrTable().GetStringFromStrIdx(classtype->info[i].first) == "INFO_access_flags") {
      return classtype->info[i].second;
    }
  }
  return 0;
}

bool ReflectionAnalysis::IsFinalClass(MIRClassType *classtype) {
  return GetClassAccessFlags(classtype) & 0x00000010;  //#  Modifier_FINAL 0x00000010;
}

void ReflectionAnalysis::CheckPrivateInnerAndNoSubClass(Klass *clazz, const std::string &annoArr) {
  // LMain_24A_3B  `EC!`VL!24!LMain_3B!`IC!`AF!4!2!name!23!A!
  size_t pos = annoArr.find("`EC", 0);
  if (pos == std::string::npos) {
    return;
  }
  if ((GetClassAccessFlags(clazz->GetMIRClassType()) & 0x02) == 0) {
    return;  // check private
  }
  if (clazz->HasSubKlass()) {
    clazz->SetPrivateInnerAndNoSubClass(false);
  } else {
    clazz->SetPrivateInnerAndNoSubClass(true);
  }
}

#define METHOD_FIELD_HASH_SIZE 1022
#define HASH_CONFLICT_FLAG 1023
uint16 GetCharHashIndex(const char *name) {
  unsigned int hashcode = DJBHash(name);
  return static_cast<uint16>(hashcode % METHOD_FIELD_HASH_SIZE);
}

uint16 GenMethodHashIndex(string name, string signature) {
  string fullname = name + signature;
  size_t p = fullname.find(')');
  CHECK_FATAL(p != string::npos, "can not find )");
  string subname = fullname.substr(0, p + 1);
  uint16 h = GetCharHashIndex(subname.c_str());
  return h;
}

void ReflectionAnalysis::ConvertMethodSig(string &signature) {
  size_t sigSize = signature.size();
  for (size_t i = 1; i < sigSize; i++) {
    if (signature[i] == 'L') {
      while (signature[++i] != ';')  // eg. Ljava/io/InputStream; so we do not check the boundary
        ;
    } else if (signature[i] == 'A') {
      signature[i] = '[';
    }
  }
}

void ReflectionAnalysis::GenAllMethodHash(std::vector<std::pair<MethodPair *, int>> &methodinfoVec, bool &isfinalize,
                                          std::unordered_map<uint32, string> &basenameMp,
                                          std::unordered_map<uint32, string> &fullnameMp) {
  std::vector<MIRFunction *> methodVector;
  std::vector<uint16> hashVector;
  int index = 0;
  for (auto &methodinfo : methodinfoVec) {
    MIRSymbol *funcSym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(methodinfo.first->first.Idx());
    MIRFunction *func = funcSym->GetFunction();
    std::string baseName = func->GetBaseFuncName();
    baseName = NameMangler::DecodeName(baseName.c_str());
    basenameMp[func->GetBaseFuncNameStridx().GetIdx()] = baseName;
    string fullname = func->GetBaseFuncNameWithType();
    fullname = NameMangler::DecodeName(fullname);
    fullnameMp[func->GetBaseFuncNameWithTypeStridx().GetIdx()] = fullname;
    CHECK_FATAL(fullname.find("|") != string::npos, "can not find |");
    string signature = fullname.substr(fullname.find("|") + 1);
    ConvertMethodSig(signature);
    uint16 h = GenMethodHashIndex(baseName, signature);
    if (baseName == "finalize" && signature == "()V") {
      h = HASH_CONFLICT_FLAG;
      isfinalize = true;
    }
    func->hashCode = h;
    hashVector.push_back(h);
    index++;
  }
  for (auto &methodinfo : methodinfoVec) {
    MIRSymbol *funcSym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(methodinfo.first->first.Idx());
    MIRFunction *func = funcSym->GetFunction();
    uint16 h = func->hashCode;
    int c = std::count(hashVector.begin(), hashVector.end(), h);
    if (c > 1) {
      func->hashCode = HASH_CONFLICT_FLAG;  // conflict flag
    }
  }
}

void ReflectionAnalysis::GenAllFieldHash(std::vector<std::pair<FieldPair, uint16_t>> &fieldV) {
  std::vector<MIRFunction *> methodVector;
  std::vector<uint16> hashVector;
  for (auto &field : fieldV) {
    uint16_t h = field.second;
    hashVector.push_back(h);
  }
  for (auto &field : fieldV) {
    uint16_t h = field.second;
    int c = std::count(hashVector.begin(), hashVector.end(), h);
    if (c > 1) {
      field.second = HASH_CONFLICT_FLAG;  // conflict flag
    }
  }
}

uint32_t GetFieldHash(std::vector<std::pair<FieldPair, uint16_t>> &fieldV, FieldPair fieldsources) {
  for (auto &field : fieldV) {
    FieldPair f = field.first;
    if (f == fieldsources) {
      return field.second;
    }
  }
  return 0;
}

void delimeterConvert(std::string &str) {
  size_t loc = str.find("!");
  while (loc != string::npos) {
    str.replace(loc, 1, "`!");
    loc = str.find("!", loc + 2);
  }

  loc = str.find("|");
  while (loc != string::npos) {
    str.replace(loc, 1, "`|");
    loc = str.find("|", loc + 2);
  }
}

bool ReflectionAnalysis::RootclassDefined() {
  if (is_libcore < 0) {
    // check whether this module defines root classes including Class/Object/Fields/Methods
    Klass *objectKlass = klassh->GetKlassFromLiteral(NameMangler::kJavaLangObjectStr);
    is_libcore = 0;
    if (objectKlass && objectKlass->GetMIRStructType()->IsLocal()) {
      is_libcore = 1;
    }
  }
  return (is_libcore == 1);
}

MIRSymbol *ReflectionAnalysis::GetOrCreateSymbol(const std::string &name, TyIdx tyIdx, bool needInit = false) {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(name);
  MIRSymbol *st = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
  if (st && !needInit) {
    return st;
  }

  // previous symbol is a forward declaration. create a new symbol for definiton.
  st = mirModule->mirBuilder->CreateGlobalDecl(name, GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx), kScGlobal);
  // Set classinfo symbol as extern if not defined locally
  if (name.find(CLASSINFO_PREFIX_STR) == 0) {
    std::string className = name.substr(strlen(CLASSINFO_PREFIX_STR));
    Klass *klass = klassh->GetKlassFromName(className);
    if (klass && !klass->GetMIRStructType()->IsLocal()) {
      st->SetStorageClass(kScExtern);
    }
  }
  return st;
}

void ReflectionAnalysis::CompressHighFrequencyStr(string &s) {
  if (highFrequencyStrMap.find(s) != highFrequencyStrMap.end()) {
    s = ReflectionAnalysis::highFrequencyStrMap[s];
  }
}

bool ReflectionAnalysis::VtableFunc(const MIRFunction *func) const {
  return (func->GetAttr(FUNCATTR_virtual) && !func->GetAttr(FUNCATTR_private) && !func->GetAttr(FUNCATTR_static));
}

bool RtRetentionPolicyCheck(const MIRSymbol *clInfo) {
  GStrIdx strIdx;
  MIRClassType *annoType = static_cast<MIRClassType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(clInfo->GetTyIdx()));
  for (MIRPragma *p : annoType->pragmaVec) {
    if (GlobalTables::GetStrTable().GetStringFromStrIdx(GlobalTables::GetTypeTable().GetTypeFromTyIdx(p->tyIdx)->nameStrIdx) ==
        (JAVA_LANG_ANNOTATION_RETENTION_STR)) {
      strIdx.SetIdx(p->elementVec[0]->val_.u);
      string retentionType = GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
      if (retentionType != "RUNTIME") {
        return false;
      } else {
        return true;
      }
    }
  }
  return false;
}

MIRSymbol *ReflectionAnalysis::GenMethodsMetaData(const Klass *klass, bool isHot) {
  MIRClassType *ctype = klass->GetMIRClassType();
  if (!ctype || ctype->methods.size() == 0) {
    return nullptr;
  }

  uint32 arraySize;
  arraySize = ctype->methods.size();
  MIRStructType *methodMetadataType = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(methodMetadataTyidx));
  MIRStructType *methodMetadataRoType =
    static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(methodMetadataRoTyidx));
  MIRStructType *methodMetadataCompactType =
    static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(methodMetadataCompactTyidx));
  MIRArrayType *arraytype = nullptr, *arraytypeRO = nullptr, *arraytypeCompact = nullptr;
  MIRAggConst *aggconst = nullptr, *aggconstRO = nullptr, *aggconstCompact = nullptr;
  if (isHot) {
    arraytype = GlobalTables::GetTypeTable().GetOrCreateArrayType(methodMetadataType, arraySize);
    aggconst = mirModule->memPool->New<MIRAggConst>(mirModule, arraytype);
    arraytypeRO = GlobalTables::GetTypeTable().GetOrCreateArrayType(methodMetadataRoType, arraySize);
    aggconstRO = mirModule->memPool->New<MIRAggConst>(mirModule, arraytypeRO);
  } else {
    arraytypeCompact =
        GlobalTables::GetTypeTable().GetOrCreateArrayType(methodMetadataCompactType, arraySize);
    aggconstCompact = mirModule->memPool->New<MIRAggConst>(mirModule, arraytypeCompact);
  }

  std::vector<std::pair<MethodPair *, int>> methodinfoVec;
  for (MethodPair &methodpair : ctype->methods) {
    methodinfoVec.push_back(std::make_pair(&methodpair, -1));
  }

  bool isfinalize = false;
  std::unordered_map<uint32, string> basenameMp, fullnameMp;
  GenAllMethodHash(methodinfoVec, isfinalize, basenameMp, fullnameMp);

  // sort constVec by hashcode
  std::sort(
    methodinfoVec.begin(), methodinfoVec.end(),
    [basenameMp, fullnameMp](std::pair<MethodPair *, int> a, std::pair<MethodPair *, int> b) {
      MIRSymbol *funcSymA = GlobalTables::GetGsymTable().GetSymbolFromStIdx(a.first->first.Idx());
      MIRFunction *funcA = funcSymA->GetFunction();
      auto itB = basenameMp.find(funcA->GetBaseFuncNameStridx().GetIdx());
      std::string funcAName = itB->second;
      auto itF = fullnameMp.find(funcA->GetBaseFuncNameWithTypeStridx().GetIdx());
      string fullnameA = itF->second;
      CHECK_FATAL(fullnameA.find("|") != fullnameA.npos, "not found |");
      string signatureA = fullnameA.substr(fullnameA.find("|") + 1);
      MIRSymbol *funcSymB = GlobalTables::GetGsymTable().GetSymbolFromStIdx(b.first->first.Idx());
      MIRFunction *funcB = funcSymB->GetFunction();
      itB = basenameMp.find(funcB->GetBaseFuncNameStridx().GetIdx());
      std::string funcBName = itB->second;
      itF = fullnameMp.find(funcB->GetBaseFuncNameWithTypeStridx().GetIdx());
      string fullnameB = itF->second;
      CHECK_FATAL(fullnameB.find("|") != string::npos, "not found |");
      string signatureB = fullnameB.substr(fullnameB.find("|") + 1);
      // make bridge the end
      if ((funcA->hashCode == HASH_CONFLICT_FLAG) && (funcB->hashCode == HASH_CONFLICT_FLAG)) {
        // only deal with return type is different
        if ((funcA->GetAttr(FUNCATTR_bridge)) && !(funcB->GetAttr(FUNCATTR_bridge))) {
          return true;
        }
        if (!(funcA->GetAttr(FUNCATTR_bridge)) && (funcB->GetAttr(FUNCATTR_bridge))) {
          return false;
        }
      }

      // as finalize()V is frequency, check it first, we put it at the end method_s
      if ((funcAName == "finalize" && signatureA == "()V") && !(funcBName == "finalize" && signatureB == "()V")) {
        return false;
      }
      if (!(funcAName == "finalize" && signatureA == "()V") && (funcBName == "finalize" && signatureB == "()V")) {
        return true;
      }
      return funcA->hashCode < funcB->hashCode;
    });

  int idx = 0;
  for (auto &methodinfo : methodinfoVec) {
    MIRSymbol *funcSym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(methodinfo.first->first.Idx());
    MIRFunction *func = funcSym->GetFunction();
    MIRAggConst *newconst = nullptr, *newconstRO = nullptr, *newconstCompact = nullptr;
    if (isHot) {
      newconst = mirModule->memPool->New<MIRAggConst>(mirModule, methodMetadataType);
      newconstRO = mirModule->memPool->New<MIRAggConst>(mirModule, methodMetadataRoType);
    } else {
      newconstCompact = mirModule->memPool->New<MIRAggConst>(mirModule, methodMetadataCompactType);
    }
    uint32 fieldID = 1;
    uint32 fieldidRO = 1;
    uint32 fieldidCompact = 1;

    // ==== MethodMetadata ====
    if (isHot) {
      // @shadow
      // It should be set to 0. mpl-linker needs  func->puIdx to generate an extra lable
      mirBuilder.AddIntFieldConst(methodMetadataType, newconst, fieldID++, func->IsAbstract() ? 0 : func->puIdx);
      // @monitor
      mirBuilder.AddIntFieldConst(methodMetadataType, newconst, fieldID++, 0);
    }

    // @modifier
    uint32 mod = GetMethodModifier(func->GetAttrs());
    // Add default attribute
    if (klass->IsInterface() && !func->GetAttr(FUNCATTR_abstract) && !func->GetAttr(FUNCATTR_static)) {
      mod |= (1 << (MOD_DEFAULT));
    }
    if (isHot) {
      mirBuilder.AddIntFieldConst(methodMetadataType, newconst, fieldID++, mod);
    } else {
      mirBuilder.AddIntFieldConst(methodMetadataCompactType, newconstCompact, fieldidCompact++, mod);
    }

    if (isHot) {
      // @method info ro
      mirBuilder.AddIntFieldConst(methodMetadataType, newconst, fieldID++, idx);
    }

    // ==== MethodMetadataRO ====
    // @methodname
    std::string baseName = basenameMp[func->GetBaseFuncNameStridx().GetIdx()];
    uint32 nameIdx = FindOrInsertReflectString(baseName);
    if (isHot) {
      mirBuilder.AddIntFieldConst(methodMetadataRoType, newconstRO, fieldidRO++, nameIdx);
    } else {
      mirBuilder.AddIntFieldConst(methodMetadataCompactType, newconstCompact, fieldidCompact++, nameIdx);
    }

    // @methodsignature
    string fullname = fullnameMp[func->GetBaseFuncNameWithTypeStridx().GetIdx()];
    size_t pos = fullname.find("|") + 1;
    string signature;
    if (pos != std::string::npos) {
      signature = fullname.substr(pos);
    } else {
      FATAL(kLncFatal, "can not find \"|\" in fullname");
    }
    ConvertMethodSig(signature);
    uint32 signatureIdx = FindOrInsertReflectString(signature);
    if (isHot) {
      mirBuilder.AddIntFieldConst(methodMetadataRoType, newconstRO, fieldidRO++, signatureIdx);
    } else {
      mirBuilder.AddIntFieldConst(methodMetadataCompactType, newconstCompact, fieldidCompact++, signatureIdx);
    }

    // @addr : function address
    if (isHot) {
      mirBuilder.AddAddroffuncFieldConst(methodMetadataRoType, newconstRO, fieldidRO++, funcSym);
    } else {
      mirBuilder.AddAddroffuncFieldConst(methodMetadataCompactType, newconstCompact, fieldidCompact++, funcSym);
    }

    // @method_in_vtable_index
    int methodInVtabIndex = 0;
    bool findMethod = false;
    MIRSymbol *vtableSym =
      GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(VTAB_PREFIX_STR + klass->GetKlassName()));
    if (klass->IsClass() && vtableSym) {
      MIRAggConst *vtableConst = dynamic_cast<MIRAggConst *>(vtableSym->GetConst());
      for (MIRConst *&node : vtableConst->constVec) {
        if (node->kind == kConstAddrofFunc) {
          MIRAddroffuncConst *addr = static_cast<MIRAddroffuncConst *>(node);
          MIRFunction *vtabFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(addr->GetValue());
          if (func->GetNameStridx() == vtabFunc->GetNameStridx()) {
            findMethod = true;
            break;
          }
        }
        methodInVtabIndex++;
      }
    } else if (klass->IsInterface()) {
      methodInVtabIndex = 0;
      for (MethodPair &methodpair : ctype->methods) {
        MIRSymbol *currSym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(methodpair.first.Idx());
        MIRFunction *currFunc = currSym->GetFunction();
        if (func->GetNameStridx() == currFunc->GetNameStridx()) {
          findMethod = true;
          break;
        }
        methodInVtabIndex++;
      }
    }
    if (!findMethod) {
      methodInVtabIndex = -10;
    }
    // @annotation
    int itemNum1[MAX_ANNOS_NUM];
    for (int i = 0; i < MAX_ANNOS_NUM; i++) {
      itemNum1[i] = 0;
    }
    string annoArr1;
    int annoNum1 = GeneAnnotation(annoArr1, ctype, kPragmaFunc, func->GetName(), itemNum1);

    // parameter annotation
    int itemNum2[MAX_ANNOS_NUM];
    for (int i = 0; i < MAX_ANNOS_NUM; i++) {
      itemNum2[i] = 0;
    }
    string annoArr2;
    int paramnumArray[MAX_ANNOS_NUM];
    int paramIndex = 0;
    for (int i = 0; i < MAX_ANNOS_NUM; i++) {
      paramnumArray[i] = 0;
    }
    int annoNum2 = GeneAnnotation(annoArr2, ctype, kPragmaParam, func->GetName(), itemNum2, paramnumArray, &paramIndex);
    string subStr = "";
    if (annoNum1 == 0 && annoNum2 == 0) {
      subStr += "0";
    } else {
      subStr += std::to_string(annoNum1);
      subStr += "!";
      for (int z = 0; z < annoNum1; z++) {
        subStr += std::to_string(itemNum1[z]);
        subStr += "!";
      }
      subStr += annoArr1;
      // parameter
      subStr += '|';
      subStr += std::to_string(annoNum2);
      subStr += "!";
      for (int z = 0; z < annoNum2; z++) {
        subStr += std::to_string(itemNum2[z]);
        subStr += "!";
      }
      for (int z = 0; z < paramIndex; z++) {
        subStr += std::to_string(paramnumArray[z]);
        subStr += "!";
      }
      subStr += annoArr2;
    }
    signatureIdx = FindOrInsertReflectString(subStr);
    if (isHot) {
      mirBuilder.AddIntFieldConst(methodMetadataRoType, newconstRO, fieldidRO++, signatureIdx);
    } else {
      mirBuilder.AddIntFieldConst(methodMetadataCompactType, newconstCompact, fieldidCompact++, signatureIdx);
    }

    // @declaringclass
    MIRSymbol *dklassSt = GetOrCreateSymbol(CLASSINFO_PREFIX_STR + func->GetBaseClassName(), classMetadataTyidx);
    if (isHot) {
      mirBuilder.AddAddrofFieldConst(methodMetadataRoType, newconstRO, fieldidRO++, dklassSt);
    } else {
      mirBuilder.AddAddrofFieldConst(methodMetadataCompactType, newconstCompact, fieldidCompact++, dklassSt);
    }

    // @flag
    uint32 flag = 0;
    if (func->GetAttr(FUNCATTR_constructor)) {
      flag |= METHOD_CONSTRUCTOR;
    }
    if (!VtableFunc(func)) {
      flag |= METHOD_NOTVIRTUAL;
    }
    if (isfinalize) {
      flag |= METHOD_FINALIZE;
    }
    uint16 hash = func->hashCode;
    flag |= (hash << 6);  // hash 10 bit
    if (isHot) {
      mirBuilder.AddIntFieldConst(methodMetadataRoType, newconstRO, fieldidRO++, flag);
    } else {
      mirBuilder.AddIntFieldConst(methodMetadataCompactType, newconstCompact, fieldidCompact++, flag);
    }

    // @argsize: number of arguments.
    CHECK_FATAL(func->formalDefVec.size() <= 0xffff, "Error:the argsize is too large");
    if (isHot) {
      mirBuilder.AddIntFieldConst(methodMetadataRoType, newconstRO, fieldidRO++, func->formalDefVec.size());
    } else {
      mirBuilder.AddIntFieldConst(methodMetadataCompactType, newconstCompact, fieldidCompact++,
                                  func->formalDefVec.size());
    }

#ifndef USE_32BIT_REF
    // @padding
    if (isHot) {
      mirBuilder.AddIntFieldConst(methodMetadataRoType, newconstRO, fieldidRO++, 0);
    }
#endif  //! USE_32BIT_REF
        // @method_in_vtab_index
    if (isHot) {
      mirBuilder.AddIntFieldConst(methodMetadataRoType, newconstRO, fieldidRO++, methodInVtabIndex);
    } else {
      mirBuilder.AddIntFieldConst(methodMetadataCompactType, newconstCompact, fieldidCompact++, methodInVtabIndex);
    }

    if (isHot && aggconst && aggconstRO) {
      aggconstRO->constVec.push_back(newconstRO);
      aggconst->constVec.push_back(newconst);
    } else if (!isHot && aggconstCompact) {
      aggconstCompact->constVec.push_back(newconstCompact);
    }

    idx++;
  }

  if (isHot) {
    MIRSymbol *methodsArrayStRo =
      GetOrCreateSymbol(METHODINFO_RO_PREFIX_STR + klass->GetKlassName(), arraytypeRO->tyIdx, true);
    methodsArrayStRo->SetStorageClass(kScFstatic);
    methodsArrayStRo->SetConst(aggconstRO);

    MIRSymbol *methodsArraySt =
      GetOrCreateSymbol(NameMangler::kMethodsInfoPrefixStr + klass->GetKlassName(), arraytype->tyIdx, true);
    if (klass->GetKlassName() != NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangObjectStr)) {
      // Direct access to methodinfo is only possible within a .so, for most classes
      methodsArraySt->SetStorageClass(kScFstatic);
    }
    methodsArraySt->SetConst(aggconst);
    return methodsArraySt;
  } else {
    MIRSymbol *methodsArrayStCompact =
      GetOrCreateSymbol(NameMangler::kMethodsInfoCompactPrefixStr + klass->GetKlassName(), arraytypeCompact->tyIdx, true);
    methodsArrayStCompact->SetStorageClass(kScFstatic);
    methodsArrayStCompact->SetConst(aggconstCompact);
    return methodsArrayStCompact;
  }
}

MIRSymbol *ReflectionAnalysis::GenSuperClassMetaData(const Klass *klass, std::list<Klass *> superclasslist) {
  uint32 size = superclasslist.size();
  uint32 arraySize;
  arraySize = size;
  MIRStructType *superclassMetadataType =
    static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(superclassMetadataTyidx));
  MIRArrayType *arraytype =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(superclassMetadataType, arraySize);
  MIRAggConst *aggconst = mirModule->memPool->New<MIRAggConst>(mirModule, arraytype);

  for (std::list<Klass *>::iterator it = superclasslist.begin(); it != superclasslist.end(); ++it) {
    MIRSymbol *dklassSt = GetOrCreateSymbol(CLASSINFO_PREFIX_STR + (*it)->GetKlassName(), classMetadataTyidx);
    MIRAggConst *newconst = mirModule->memPool->New<MIRAggConst>(mirModule, superclassMetadataType);
    mirBuilder.AddAddrofFieldConst(superclassMetadataType, newconst, 1, dklassSt);
    aggconst->constVec.push_back(newconst);
  }

  MIRSymbol *superclassArraySt =
    GetOrCreateSymbol(SUPERCLASSINFO_PREFIX_STR + klass->GetKlassName(), arraytype->tyIdx, true);
  // Direct access to superclassinfo is only possible within a .so
  superclassArraySt->SetStorageClass(kScFstatic);
  superclassArraySt->SetConst(aggconst);
  return superclassArraySt;
}

void ConvertFieldName(std::string &fieldname, bool staticfield) {
  // convert field name to java define name
  if (staticfield) {
    // remove class name prefix
    std::size_t pos1 = fieldname.find(NameMangler::kClassNameSplitterStr);
    CHECK_FATAL(pos1 != fieldname.npos, "fieldname not found");
    fieldname = fieldname.substr(pos1 + 6);
  }

  fieldname = NameMangler::DecodeName(fieldname);
}

MIRSymbol *ReflectionAnalysis::GenFieldsMetaData(const Klass *klass, bool isHot) {
  MIRClassType *ctype = klass->GetMIRClassType();
  FieldVector fields = ctype->fields;
  FieldVector staticFields = ctype->staticFields;

  uint32 size = fields.size() + staticFields.size();

  if (!size) {
    return nullptr;
  }

  uint32 arraySize;
  arraySize = size;

  MIRStructType *fieldMetadataType = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldMetadataTyidx));
  MIRStructType *fieldMetadataRoType = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldMetadataRoTyidx));
  MIRStructType *fieldMetadataCompactType =
    static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldMetadataCompactTyidx));
  MIRArrayType *arraytype = nullptr, *arraytypeRO = nullptr, *arraytypeCompact = nullptr;
  MIRAggConst *aggconst = nullptr, *aggconstRO = nullptr, *aggconstCompact = nullptr;
  if (isHot) {
    arraytype = GlobalTables::GetTypeTable().GetOrCreateArrayType(fieldMetadataType, arraySize);
    arraytypeRO = GlobalTables::GetTypeTable().GetOrCreateArrayType(fieldMetadataRoType, arraySize);
    aggconst = mirModule->memPool->New<MIRAggConst>(mirModule, arraytype);
    aggconstRO = mirModule->memPool->New<MIRAggConst>(mirModule, arraytypeRO);
  } else {
    arraytypeCompact =
        GlobalTables::GetTypeTable().GetOrCreateArrayType(fieldMetadataCompactType, arraySize);
    aggconstCompact = mirModule->memPool->New<MIRAggConst>(mirModule, arraytypeCompact);
  }

  std::vector<std::pair<FieldPair, uint16_t>> fieldHashvec(size);
  unsigned i = 0;
  for (; i < fields.size(); i++) {
    std::string fieldname = GlobalTables::GetStrTable().GetStringFromStrIdx(fields[i].first);
    ConvertFieldName(fieldname, false);
    uint32_t hashcode = GetCharHashIndex(fieldname.c_str());
    fieldHashvec[i] = std::make_pair(fields[i], hashcode);
  }
  for (unsigned j = 0; j < staticFields.size(); j++) {
    std::string fieldname = GlobalTables::GetStrTable().GetStringFromStrIdx(staticFields[j].first);
    ConvertFieldName(fieldname, true);
    uint32_t hashcode = GetCharHashIndex(fieldname.c_str());
    fieldHashvec[i++] = std::make_pair(staticFields[j], hashcode);
  }
  GenAllFieldHash(fieldHashvec);

  // sort field_hashvec by hashcode
  std::sort(fieldHashvec.begin(), fieldHashvec.end(),
            [](std::pair<FieldPair, uint16_t> a, std::pair<FieldPair, uint16_t> b) {
              std::uint16_t fieldHashA = a.second;
              std::uint16_t fieldHashB = b.second;
              return fieldHashA < fieldHashB;
            });

  std::vector<std::pair<FieldPair, int>> fieldinfoVec(size);
  int j = 0, k = 0;
  for (auto it = fieldHashvec.begin(); it != fieldHashvec.end(); ++it) {
    FieldPair f = (*it).first;
    if (f.second.second.GetAttr(FLDATTR_static)) {
      fieldinfoVec[j] = std::make_pair(f, -1);
    } else {
      fieldinfoVec[j] = std::make_pair(f, k++);
    }
    j++;
  }

  ASSERT(i == size, "In class %s: %d fields seen, BUT %d fields declared", klass->GetKlassName().c_str(), i, size);

  int idx = 0;
  for (auto &fieldinfo : fieldinfoVec) {
    FieldPair fieldP = fieldinfo.first;
    MIRAggConst *newconst = nullptr, *newconstRO = nullptr, *newconstCompact = nullptr;
    if (isHot) {
      newconst = mirModule->memPool->New<MIRAggConst>(mirModule, fieldMetadataType);
      newconstRO = mirModule->memPool->New<MIRAggConst>(mirModule, fieldMetadataRoType);
    } else {
      newconstCompact = mirModule->memPool->New<MIRAggConst>(mirModule, fieldMetadataCompactType);
    }
    uint32 fieldID = 1;
    uint32 fieldidRO = 1;
    uint32 fieldidCompact = 1;

    // ==== FieldMetadata ====
    if (isHot) {
      // @shadow
      mirBuilder.AddIntFieldConst(fieldMetadataType, newconst, fieldID++, 0);
      // @monitor
      mirBuilder.AddIntFieldConst(fieldMetadataType, newconst, fieldID++, 0);
    }

    // @modifier
    FieldAttrs fa = fieldP.second.second;
    if (isHot) {
      mirBuilder.AddIntFieldConst(fieldMetadataType, newconst, fieldID++, GetFieldModifier(fa));
    } else {
      mirBuilder.AddIntFieldConst(fieldMetadataCompactType, newconstCompact, fieldidCompact++, GetFieldModifier(fa));
    }

    // @field info ro
    if (isHot) {
      mirBuilder.AddIntFieldConst(fieldMetadataType, newconst, fieldID++, idx++);
    }

    // we'll amend for the fieldname in generated metadata, so we need an original version of fieldname
    std::string originFieldname = GlobalTables::GetStrTable().GetStringFromStrIdx(fieldP.first);
    std::string fieldname = originFieldname;
    TyIdx tyIdx = fieldP.second.first;
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
    uint32 flag = 0;

    bool staticfield = (fieldinfo.second == -1);
    if (staticfield) {
      flag |= FIELD_STATIC;
    }
    ConvertFieldName(fieldname, staticfield);
    uint32 fieldname32Idx = FindOrInsertReflectString(fieldname);

    // @type :  klass for type
    // some type will have the type name in this field instead of klass
    // when stored as compact format, it's always stored as type name
    // if it's stored as type name, it's stored as a MIRIntConst
    // if it's stored as klass, it's stored as a MIRAddrofConst
    switch (ty->typeKind) {
      case kTypeScalar: {
        flag |= FIELD_PRIM;
        std::string name(GetPrimTypeJavaName(ty->primType));

        uint32 nameIdx = FindOrInsertReflectString(name);
        if (isHot) {
          mirBuilder.AddIntFieldConst(fieldMetadataType, newconst, fieldID++, nameIdx);
        } else {
          mirBuilder.AddIntFieldConst(fieldMetadataCompactType, newconstCompact, fieldidCompact++, nameIdx);
        }
        break;
      }
      case kTypePointer: {
        MIRType *ptype = static_cast<MIRPtrType *>(ty)->GetPointedType();
        if (ptype->typeKind == kTypeArray || ptype->typeKind == kTypeJArray) {
          flag |= FIELD_ARRAY;  // array type will be resolved at runtime;
          if (raDebug && 0) {
            ptype->Dump(0, false);
          }
          CHECK_FATAL(dynamic_cast<MIRJarrayType *>(ptype) != nullptr, "");
          std::string javaname = dynamic_cast<MIRJarrayType *>(ptype)->GetJavaName();
          std::string klassJavaDescriptor;
          NameMangler::DecodeMapleNameToJavaDescriptor(javaname, klassJavaDescriptor);
          uint32 nameIdx = FindOrInsertReflectString(klassJavaDescriptor);
          if (isHot) {
            mirBuilder.AddIntFieldConst(fieldMetadataType, newconst, fieldID++, nameIdx);
          } else {
            mirBuilder.AddIntFieldConst(fieldMetadataCompactType, newconstCompact, fieldidCompact++, nameIdx);
          }
        } else if (ptype->typeKind == kTypeByName || ptype->typeKind == kTypeClass || ptype->typeKind == kTypeInterface ||
                   ptype->typeKind == kTypeClassIncomplete || ptype->typeKind == kTypeInterfaceIncomplete ||
                   ptype->typeKind == kTypeConstString) {
          TyIdx tyIdx = ptype->tyIdx;
          if (ptype->typeKind == kTypeConstString) {
            tyIdx = GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangStringStr));
          }
          Klass *tklass = klassh->GetKlassFromTyidx(tyIdx);
          CHECK_FATAL(tklass, "In class %s: field %s 's type is UNKNOWN", klass->GetKlassName().c_str(), fieldname.c_str());
          if (isHot) {
            MIRSymbol *tklassSt = GetOrCreateSymbol(CLASSINFO_PREFIX_STR + tklass->GetKlassName(), classMetadataTyidx);
            if (!tklass->GetMIRStructType()->IsLocal()) {
              tklassSt->SetStorageClass(kScExtern);
            }
            mirBuilder.AddAddrofFieldConst(fieldMetadataType, newconst, fieldID++, tklassSt);
            // set SHADOW const to 1 to indicate it's the klass instead of the name
            (static_cast<MIRIntConst *>(newconst->constVec[FIELD::kShadow]))->value = 0x1;
          } else {
            std::string javaname = tklass->GetKlassName();
            std::string klassJavaDescriptor;
            NameMangler::DecodeMapleNameToJavaDescriptor(javaname, klassJavaDescriptor);
            uint32 nameIdx = FindOrInsertReflectString(klassJavaDescriptor);
            mirBuilder.AddIntFieldConst(fieldMetadataCompactType, newconstCompact, fieldidCompact++, nameIdx);
          }
        } else {
          CHECK_FATAL(false, "In class %s: field %s 's type is UNKNOWN", klass->GetKlassName().c_str(), fieldname.c_str());
        }
        break;
      }
      default: {
        CHECK_FATAL(false, "In class %s: field %s 's type is UNKNOWN", klass->GetKlassName().c_str(), fieldname.c_str());
      }
    }

    // ==== FieldMetadataRO ====
    // @offset :
    if (staticfield) {
      // offset of the static field, we fill the global var address.
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(GlobalTables::GetStrTable().GetStringFromStrIdx(fieldP.first));
      MIRSymbol *gvarSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
      if (gvarSt == nullptr) {
        // if this static field is not used, the symbol will not be generated in java
        // So we just generate a weak one here.
        gvarSt = mirModule->mirBuilder->CreateGlobalDecl(GlobalTables::GetStrTable().GetStringFromStrIdx(fieldP.first),
                                                         GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx), kScGlobal);
        gvarSt->SetAttr(ATTR_weak);
        gvarSt->SetAttr(ATTR_static);
      }
      if (isHot) {
        mirBuilder.AddAddrofFieldConst(fieldMetadataRoType, newconstRO, fieldidRO++, gvarSt);
      } else {
        mirBuilder.AddAddrofFieldConst(fieldMetadataCompactType, newconstCompact, fieldidCompact++, gvarSt);
      }
    } else {
      // offset of the instance field, we fill the index of fields here and let CG to fill in.
      FieldID fldid = mirBuilder.GetStructFieldIdFromNameAndTypeParentFirstFoundInChild(
        klass->GetMIRClassType(), originFieldname, fieldP.second.first);
      if (isHot) {
        mirBuilder.AddIntFieldConst(fieldMetadataRoType, newconstRO, fieldidRO++, fldid);
      } else {
        mirBuilder.AddIntFieldConst(fieldMetadataCompactType, newconstCompact, fieldidCompact++, fldid);
      }
    }

    // @declaringclass :  klass.
    if (isHot) {
      MIRSymbol *dklassSt = GetOrCreateSymbol(CLASSINFO_PREFIX_STR + klass->GetKlassName(), classMetadataTyidx);
      mirBuilder.AddAddrofFieldConst(fieldMetadataRoType, newconstRO, fieldidRO++, dklassSt);
    }

    // @flag
    uint16 hash = GetFieldHash(fieldHashvec, fieldP);
    flag |= (hash << 6);  // hash 10 bit
    if (isHot) {
      mirBuilder.AddIntFieldConst(fieldMetadataRoType, newconstRO, fieldidRO++, flag);
    } else {
      mirBuilder.AddIntFieldConst(fieldMetadataCompactType, newconstCompact, fieldidCompact++, flag);
    }

    // @padding
    if (isHot) {
      mirBuilder.AddIntFieldConst(fieldMetadataRoType, newconstRO, fieldidRO++, TBD_VALUE);
    } else {
      mirBuilder.AddIntFieldConst(fieldMetadataCompactType, newconstCompact, fieldidCompact++, TBD_VALUE);
    }

    // @fieldname : offset of the name in *local* strtab.
    // In CG, we need to fill the offset as (__reflection_strtab___$file + offset of name)
    if (isHot) {
      mirBuilder.AddIntFieldConst(fieldMetadataRoType, newconstRO, fieldidRO++, fieldname32Idx);
    } else {
      mirBuilder.AddIntFieldConst(fieldMetadataCompactType, newconstCompact, fieldidCompact++, fieldname32Idx);
    }

    //  @annotation
    string annoArr;
    int itemNum[MAX_ANNOS_NUM];
    for (int i = 0; i < MAX_ANNOS_NUM; i++) {
      itemNum[i] = 0;
    }
    int annoNum = GeneAnnotation(annoArr, ctype, kPragmaVar, fieldname, itemNum);
    if (isHot) {
      SetAnnoFieldConst(fieldMetadataRoType, newconstRO, fieldidRO++, annoNum, itemNum, annoArr);
    } else {
      SetAnnoFieldConst(fieldMetadataCompactType, newconstCompact, fieldidCompact++, annoNum, itemNum, annoArr);
    }

#ifndef USE_32BIT_REF
    // @padding1
    if (isHot) {
      mirBuilder.AddIntFieldConst(fieldMetadataRoType, newconstRO, fieldidRO++, 0);
    }
#endif  //! USE_32BIT_REF

    if (isHot && aggconst && aggconstRO) {
      aggconstRO->constVec.push_back(newconstRO);
      aggconst->constVec.push_back(newconst);
    } else if (!isHot && aggconstCompact) {
      aggconstCompact->constVec.push_back(newconstCompact);
    }
  }

  if (isHot) {
    MIRSymbol *fieldsArrayStRo =
      GetOrCreateSymbol(FIELDINFO_RO_PREFIX_STR + klass->GetKlassName(), arraytypeRO->tyIdx, true);
    fieldsArrayStRo->SetStorageClass(kScFstatic);
    fieldsArrayStRo->SetConst(aggconstRO);

    MIRSymbol *fieldsArraySt =
      GetOrCreateSymbol(NameMangler::kFieldsInfoPrefixStr + klass->GetKlassName(), arraytype->tyIdx, true);
    if (klass->GetKlassName() != NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangObjectStr)) {
      // Direct access to fieldinfo is only possible within a .so, for most classes
      fieldsArraySt->SetStorageClass(kScFstatic);
    }
    fieldsArraySt->SetConst(aggconst);
    return fieldsArraySt;
  } else {
    MIRSymbol *fieldsArrayStCompact =
      GetOrCreateSymbol(NameMangler::kFieldsInfoCompactPrefixStr + klass->GetKlassName(), arraytypeCompact->tyIdx, true);
    fieldsArrayStCompact->SetStorageClass(kScFstatic);
    fieldsArrayStCompact->SetConst(aggconstCompact);
    return fieldsArrayStCompact;
  }
}

string ReflectionAnalysis::GetAnnotationValue(MapleVector<MIRPragmaElement *> subelemVector, GStrIdx typestridx) {
  string annoArr, tmp;
  GStrIdx strIdx;
  annoArr += '[';
  annoArr += std::to_string(subelemVector.size());
  annoArr += '!';
  annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(typestridx);
  for (MIRPragmaElement *arrayElem : subelemVector) {
    annoArr += '!';
    std::ostringstream oss3;
    annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(arrayElem->namestridx_);
    annoArr += '!';
    annoArr += std::to_string(arrayElem->type_);
    annoArr += '!';
    if (arrayElem->type_ == kValueInt || arrayElem->type_ == kValueByte || arrayElem->type_ == kValueShort) {
      annoArr += std::to_string(arrayElem->val_.i);
    } else if (arrayElem->type_ == kValueLong) {
      annoArr += std::to_string(arrayElem->val_.j);
    } else if (arrayElem->type_ == kValueDouble) {
      oss3 << tmp << setiosflags(ios::scientific) << setprecision(16) << arrayElem->val_.d;
      annoArr += oss3.str();
    } else if (arrayElem->type_ == kValueFloat) {
      oss3 << tmp << setiosflags(ios::scientific) << setprecision(7) << arrayElem->val_.f;
      annoArr += oss3.str();
    } else if (arrayElem->type_ == kValueString || arrayElem->type_ == kValueEnum) {
      strIdx.SetIdx(arrayElem->val_.u);
      string t = GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
      ReflectionAnalysis::CompressHighFrequencyStr(t);
      delimeterConvert(t);
      annoArr += t;
    } else if (arrayElem->type_ == kValueBoolean || arrayElem->type_ == kValueChar) {
      annoArr += std::to_string(arrayElem->val_.u);
    } else if (arrayElem->type_ == kValueType) {
      strIdx.SetIdx(arrayElem->val_.u);
      annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
    } else if (arrayElem->type_ == kValueAnnotation) {
      annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(arrayElem->typestridx_);
    } else if (arrayElem->type_ == kValueArray) {
      annoArr += GetArrayValue(arrayElem->subelemvec_);
    } else {
      annoArr += std::to_string(arrayElem->val_.u);
      annoArr += '!';
      annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(arrayElem->namestridx_);
      strIdx.SetIdx(arrayElem->val_.u);
      annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
      annoArr += '!';
    }
  }
  annoArr += ']';
  return annoArr;
}

string ReflectionAnalysis::GetArrayValue(MapleVector<MIRPragmaElement *> subelemVector) {
  string annoArr;
  GStrIdx strIdx;
  annoArr += '[';
  annoArr += std::to_string(subelemVector.size());
  annoArr += '!';
  if (subelemVector.size() > 0) {
    annoArr += std::to_string(subelemVector[0]->type_);
    annoArr += '!';
  }
  for (MIRPragmaElement *arrayElem : subelemVector) {
    std::ostringstream oss3;
    string t = GlobalTables::GetStrTable().GetStringFromStrIdx(arrayElem->typestridx_);
    ReflectionAnalysis::CompressHighFrequencyStr(t);
    annoArr += t;
    annoArr += '!';
    string type;
    MapleVector<MIRPragmaElement *> &subsubelemVector = arrayElem->subelemvec_;
    if (arrayElem->type_ == kValueAnnotation) {
      std::ostringstream oss4;
      oss4 << type << subsubelemVector.size();
      annoArr += oss4.str();
      annoArr += '!';
      for (MIRPragmaElement *annoElem : subsubelemVector) {
        annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(annoElem->namestridx_);
        annoArr += '!';
        annoArr += std::to_string(annoElem->type_);
        annoArr += '!';
        annoArr += GetAnnoValueWithoutArray(annoElem);
        annoArr += '!';
      }
    } else if (arrayElem->type_ == kValueInt || arrayElem->type_ == kValueByte || arrayElem->type_ == kValueShort) {
      oss3 << type << arrayElem->val_.i;
      annoArr += oss3.str();
    } else if (arrayElem->type_ == kValueLong) {
      oss3 << type << arrayElem->val_.j;
      annoArr += oss3.str();
    } else if (arrayElem->type_ == kValueDouble) {
      oss3 << type << setiosflags(ios::scientific) << setprecision(16) << arrayElem->val_.d;
      annoArr += oss3.str();
    } else if (arrayElem->type_ == kValueFloat) {
      oss3 << type << setiosflags(ios::scientific) << setprecision(7) << arrayElem->val_.f;
      annoArr += oss3.str();
    } else if (arrayElem->type_ == kValueString || arrayElem->type_ == kValueEnum) {
      strIdx.SetIdx(arrayElem->val_.u);
      string t = GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
      ReflectionAnalysis::CompressHighFrequencyStr(t);
      delimeterConvert(t);
      annoArr += t;
    } else if (arrayElem->type_ == kValueBoolean || arrayElem->type_ == kValueChar) {
      oss3 << type << arrayElem->val_.u;
      annoArr += oss3.str();
    } else if (arrayElem->type_ == kValueType) {
      strIdx.SetIdx(arrayElem->val_.u);
      annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
    } else {
      oss3 << type << arrayElem->val_.u;
      annoArr += oss3.str();
      annoArr += '!';
      annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(arrayElem->namestridx_);
      strIdx.SetIdx(arrayElem->val_.u);
      annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
      annoArr += '!';
    }
  }
  annoArr += ']';
  return annoArr;
}

string ReflectionAnalysis::GetAnnoValueWithoutArray(const MIRPragmaElement *annoElem) {
  std::ostringstream oss;
  string tmp, annoArr;
  GStrIdx strIdx;
  if (annoElem->type_ == kValueInt || annoElem->type_ == kValueByte || annoElem->type_ == kValueShort) {
    annoArr += std::to_string(annoElem->val_.i);
  } else if (annoElem->type_ == kValueFloat) {
    annoArr += std::to_string(annoElem->val_.f);
  } else if (annoElem->type_ == kValueDouble) {
    annoArr += std::to_string(annoElem->val_.d);
  } else if (annoElem->type_ == kValueLong) {
    annoArr += std::to_string(annoElem->val_.j);
  } else if (annoElem->type_ == kValueBoolean || annoElem->type_ == kValueChar) {
    annoArr += std::to_string(annoElem->val_.u);
  } else if (annoElem->type_ == kValueArray) {
    annoArr += GetArrayValue(annoElem->subelemvec_);
  } else {
    strIdx.SetIdx(annoElem->val_.u);
    annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
  }
  return annoArr;
}

uint32 ReflectionAnalysis::BKDRHash(std::string strname, uint32 seed) {
  const char *name = strname.c_str();
  uint32 hash = 0;
  while (*name) {
    hash = hash * seed + (*name++);
  }
  return hash;
}

uint32 ReflectionAnalysis::GetHashIndex(std::string strname) {
  return BKDRHash(strname, 211);
}

static inline void GenHotClassNameString(Klass *klass) {
  MIRClassType *classtype = klass->GetMIRClassType();
  if (!classtype->IsLocal()) {
    // external class.
    return;
  }
  if (!klass->HasNativeMethod()) {
    return;  // it's a cold class. doesn't care
  }

  std::string klassName = klass->GetKlassName();
  std::string klassJavaDescriptor;
  NameMangler::DecodeMapleNameToJavaDescriptor(klassName, klassJavaDescriptor);
  (void)ReflectionAnalysis::FindOrInsertRepeatString(klassJavaDescriptor,
                                                 true);  // always used
}

uint32_t ReflectionAnalysis::FindOrInsertReflectString(const std::string &str) {
  bool isHot = false;
  uint32_t hotType = HOT_LAYOUT::kStartUpHot;
  auto item = hotReflectStrMap.find(str);
  if (item != hotReflectStrMap.end()) {
    isHot = true;
    hotType = item->second;
  }

  return ReflectionAnalysis::FindOrInsertRepeatString(str, isHot, hotType);
}

void ReflectionAnalysis::GenClassMetaData(Klass *klass) {
  MIRClassType *classtype = klass->GetMIRClassType();

  if (!classtype->IsLocal()) {
    // external class.
    return;
  }

  std::string klassName = klass->GetKlassName();
  std::string klassJavaDescriptor;
  NameMangler::DecodeMapleNameToJavaDescriptor(klassName, klassJavaDescriptor);

  uint32 hashIndex = GetHashIndex(klassJavaDescriptor);
  if (raDebug) {
    cerr << "========= Gen Class: " << klassJavaDescriptor << " (" << hashIndex << ") ========" << endl;
  }

  MIRStructType *classMetadataRoType = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(classMetadataRoTyidx));
  MIRAggConst *newconst = mirModule->memPool->New<MIRAggConst>(mirModule, classMetadataRoType);
  uint32 fieldID = 1;
  // @classname
  uint32 nameIdx = FindOrInsertReflectString(klassJavaDescriptor);
  mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, nameIdx);

  // @iFields : All instance fields
  int numOfFields = 0;
  if (klass->IsInterface() && !classtype->IsLocal()) {
    mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, 0);
  } else {
    bool hasAdded = false;
    if (klass->GetKlassName() == NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangObjectStr)) {
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(
        NameMangler::kFieldsInfoPrefixStr + NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangObjectStr));
      MIRSymbol *fieldsSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
      if (fieldsSt) {
        mirBuilder.AddAddrofFieldConst(classMetadataRoType, newconst, fieldID++, fieldsSt);
        hasAdded = true;
      }
    }
    if (!hasAdded) {
      bool isHotFields = mirModule->profile.CheckFieldHot(klassName);
      MIRSymbol *fieldsSt = GenFieldsMetaData(klass, isHotFields);
      if (fieldsSt)
        numOfFields = static_cast<MIRAggConst *>(fieldsSt->GetConst())->constVec.size();
      // all meta data will be weak if dummy constructors
      if (fieldsSt != nullptr)
        mirBuilder.AddAddrofFieldConst(classMetadataRoType, newconst, fieldID++, fieldsSt);
      else
        mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, 0);
    }
  }

  // @methods : all methods
  int numOfMethods = 0;
  if (klass->IsInterface() && !classtype->IsLocal()) {
    mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, 0);
  } else {
    MIRSymbol *methodsSt;

    bool isHotMethods = mirModule->profile.CheckMethodHot(klassName);
    methodsSt = GenMethodsMetaData(klass, isHotMethods);
    if (methodsSt) {
      numOfMethods = static_cast<MIRAggConst *>(methodsSt->GetConst())->constVec.size();
      mirBuilder.AddAddrofFieldConst(classMetadataRoType, newconst, fieldID++, methodsSt);
    } else {
      mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, 0);
    }
  }

  // @superclass :  super class + a list of implemented interfaces
  std::list<Klass *> superclasslist;
  bool missingSuper = false;
  for (Klass *superclass : klass->GetSuperKlasses()) {
    superclasslist.push_back(superclass);
  }
  for (TyIdx const kIntftyidx : classtype->interfacesImplemented) {
    Klass *interface = klassh->GetKlassFromTyidx(kIntftyidx);
    if (interface == nullptr) {
      missingSuper = true;
      MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(kIntftyidx);
      LogInfo::MapleLogger() << "Error: Interface " << static_cast<MIRStructType *>(type)->GetName() << " is not found" << endl;
    }
    std::list<Klass *>::iterator it = std::find(superclasslist.begin(), superclasslist.end(), interface);
    if (it == superclasslist.end()) {
      superclasslist.push_back(interface);
    }
  }
  if (missingSuper) {
    cerr << "Error: Missing interface for " << klass->GetKlassName() << endl;
    CHECK_FATAL(0, "Missing interface");
  }
  uint32 superClassSize = superclasslist.size();
  if (superClassSize >= 1) {
    MIRSymbol *superclassSt = GenSuperClassMetaData(klass, superclasslist);
    mirBuilder.AddAddrofFieldConst(classMetadataRoType, newconst, fieldID++, superclassSt);
  } else {
    mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, 0);
  }

  // @numoffields :num of fields (own)
  CHECK_FATAL(numOfFields <= 0xffff, "Error:the num of fields is too large");
  mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, numOfFields);

  // @numofmethods :num of methods in vtable
  CHECK_FATAL(numOfMethods <= 0xffff, "Error:the num of methods is too large");
  mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, numOfMethods);

  // do annotation
  MIRClassType *ctype = klass->GetMIRClassType();
  int itemNum[MAX_ANNOS_NUM];
  for (int i = 0; i < MAX_ANNOS_NUM; i++) {
    itemNum[i] = 0;
  }
  string annoArr;
  int annoNum = GeneAnnotation(annoArr, ctype, kPragmaClass, klass->GetKlassName(), itemNum);
  bool isAnonymous = IsAnonymousClass(annoArr);
  CheckPrivateInnerAndNoSubClass(klass, annoArr);
  bool isColdClass = !(mirModule->profile.CheckClassHot(klassName));

#ifndef USE_32BIT_REF
  // @flag
  // array class and primitive class is not generated by compiler
  uint32 flag = klass->GetFlag(kClassHasFinalizer | CLASS_REFERENCE | kClassFinalizerreferenceSentinel);
  flag = isAnonymous ? (flag | kClassIsanonymousclass) : flag;
  flag = isColdClass ? (flag | kClassIscoldclass) : flag;

  mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, flag);

  // @numofsuperclasses
  CHECK_FATAL(superClassSize <= 0xffff, "Error:the size of superClass is too big");
  mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, superClassSize);

  // @padding
  mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, 0);
#endif  //! USE_32BIT_REF

  // @modifier: for class fill ClassAccessFlags
  uint32 modifier = GetClassAccessFlags(classtype);
  mirBuilder.AddIntFieldConst(classMetadataRoType, newconst, fieldID++, modifier);

  // @annotation: set annotation field
  SetAnnoFieldConst(classMetadataRoType, newconst, fieldID++, annoNum, itemNum, annoArr);
  MIRSymbol *classMetadataRoSt =
    GetOrCreateSymbol(CLASSINFO_RO_PREFIX_STR + klass->GetKlassName(), classMetadataRoTyidx, true);
  classMetadataRoSt->SetStorageClass(kScFstatic);
  classMetadataRoSt->SetConst(newconst);

  // Class Metadata definition start here
  MIRStructType *classMetadataType = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(classMetadataTyidx));
  newconst = mirModule->memPool->New<MIRAggConst>(mirModule, classMetadataType);
  fieldID = 1;

  // @shadow
  mirBuilder.AddIntFieldConst(classMetadataType, newconst, fieldID++, hashIndex);

  // @monitor
  mirBuilder.AddIntFieldConst(classMetadataType, newconst, fieldID++, 0);

  // @class loader
  mirBuilder.AddIntFieldConst(classMetadataType, newconst, fieldID++, 0);

  // @objsize :fill this in the CG.
  mirBuilder.AddIntFieldConst(classMetadataType, newconst, fieldID++, 0);

#ifdef USE_32BIT_REF
  // @flag
  // array class and primitive class is not generated by compiler
  uint32 flag = klass->GetFlag(kClassHasFinalizer | CLASS_REFERENCE | kClassFinalizerreferenceSentinel);
  flag = isAnonymous ? (flag | kClassIsanonymousclass) : flag;
  flag = isColdClass ? (flag | kClassIscoldclass) : flag;

  mirBuilder.AddIntFieldConst(classMetadataType, newconst, fieldID++, flag);

  // @numofsuperclasses
  CHECK_FATAL(superClassSize <= 0xffff, "Error:the size of superClass is too big");
  mirBuilder.AddIntFieldConst(classMetadataType, newconst, fieldID++, superClassSize);
#endif  // USE_32BIT_REF

  // @itab
  GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName(ITAB_PREFIX_STR + klass->GetKlassName());
  if (strIdx != 0) {
    MIRSymbol *itableSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
    mirBuilder.AddAddrofFieldConst(classMetadataType, newconst, fieldID++, itableSt);
  } else {
    mirBuilder.AddIntFieldConst(classMetadataType, newconst, fieldID++, 0);
  }
  // @vtab
  strIdx = GlobalTables::GetStrTable().GetStrIdxFromName(VTAB_PREFIX_STR + klass->GetKlassName());
  if (strIdx != 0) {
    MIRSymbol *vtableSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
    mirBuilder.AddAddrofFieldConst(classMetadataType, newconst, fieldID++, vtableSt);
  } else {
    mirBuilder.AddIntFieldConst(classMetadataType, newconst, fieldID++, 0);
  }

  // @gctib
  MIRSymbol *gctibSt =
      GetOrCreateSymbol(GCTIB_PREFIX_STR + klass->GetKlassName(), GlobalTables::GetTypeTable().GetVoidPtr()->tyIdx, false);
  if (klass->GetKlassName() != NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangObjectStr)) {
    // Direct access to gctib is only possible within a .so, for most classes
    gctibSt->SetStorageClass(kScFstatic);
  }
  mirBuilder.AddAddrofFieldConst(classMetadataType, newconst, fieldID++, gctibSt);

  // @classinfo ro
  mirBuilder.AddAddrofFieldConst(classMetadataType, newconst, fieldID++, classMetadataRoSt);

  // set default value to class initialization state.
  // If this class and its parents do not have clinit, we do not clinit-check for this class.
  if (klassh->NeedClinitCheckRecursively(klass)) {
    MIRType *ptrtype = GlobalTables::GetTypeTable().GetPtr();
    MIRSymbol *classInitProtectRegion = mirModule->mirBuilder->GetOrCreateGlobalDecl(
      CLASS_INIT_PROTECT_REGION_STR, ptrtype, kScExtern);
    mirBuilder.AddAddrofFieldConst(classMetadataType, newconst, fieldID++, classInitProtectRegion);
  } else {
    MIRType *clinitState = GlobalTables::GetTypeTable().GetUInt64();
    // the class initialization state is modified to __ClassStateInitialized__
    MIRSymbol *classinfo = mirModule->mirBuilder->GetOrCreateGlobalDecl(CLASS_STATE_INITIALIZED_STR, clinitState);
    mirBuilder.AddAddrofFieldConst(classMetadataType, newconst, fieldID++, classinfo);
  }

  // finally generate class metadata here
  MIRSymbol *classSt = GetOrCreateSymbol(CLASSINFO_PREFIX_STR + klass->GetKlassName(), classMetadataTyidx, true);
  classSt->SetConst(newconst);
  classTab.push_back(classSt);
}

void ReflectionAnalysis::SetAnnoFieldConst(MIRStructType *metadataRoType, MIRAggConst *newconst, uint32 fieldID,
                                           int annoNum, const int *itemNum, const std::string &annoArr) {
  if (annoNum == 0) {
    string subStr = "0";
    uint32 signatureIdx = FindOrInsertReflectString(subStr);
    mirBuilder.AddIntFieldConst(metadataRoType, newconst, fieldID, signatureIdx);
  } else {
    string subStr = std::to_string(annoNum);
    subStr += "!";
    for (int z = 0; z < annoNum; z++) {
      subStr += std::to_string(itemNum[z]);
      subStr += "!";
    }
    subStr += annoArr;
    uint32 signatureIdx = FindOrInsertReflectString(subStr);
    mirBuilder.AddIntFieldConst(metadataRoType, newconst, fieldID, signatureIdx);
  }
}

int8_t JudgePara(MIRClassType *ctype) {
  return 0;
}

int ReflectionAnalysis::GeneAnnotation(string &annoArr, MIRClassType *ctype, PragmaKind paragKind,
                                       const std::string &paragName, int *itemNum, int *paramnumArray,
                                       int *paramIndex) {
  int annoNum = 0;
  string cmpString = "";
  for (MIRPragma *prag : ctype->pragmaVec) {
    if (paragKind == kPragmaVar)
      cmpString = NameMangler::DecodeName(GlobalTables::GetStrTable().GetStringFromStrIdx(prag->strIdx).c_str());
    else {
      cmpString = GlobalTables::GetStrTable().GetStringFromStrIdx(prag->strIdx);
    }
    if (prag->pragmaKind == paragKind && paragName == cmpString) {
      MapleVector<MIRPragmaElement *> &elemVector = prag->elementVec;
      ASSERT(annoNum <= MAX_ANNOS_NUM, "In class %s: Annotation number %d is larger than the threshold %d",
              ctype->GetName().c_str(), annoNum, MAX_ANNOS_NUM);
      MIRSymbol *clInfo = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetTypeTable().GetTypeFromTyIdx(prag->tyIdx)->nameStrIdx);
      if (clInfo) {
        if (!RtRetentionPolicyCheck(clInfo)) {
          continue;
        }
      }
      annoNum++;
      GStrIdx gindex = GlobalTables::GetTypeTable().GetTypeFromTyIdx(prag->tyIdx)->nameStrIdx;
      string s = GlobalTables::GetStrTable().GetStringFromStrIdx(gindex);
      ReflectionAnalysis::CompressHighFrequencyStr(s);
      annoArr += s;
      annoArr += "!";
      if (paramnumArray) {
        int8_t x = JudgePara(ctype);
        if (x && paragName.find(INIT_FUNTION_STR) != std::string::npos) {
          paramnumArray[(*paramIndex)++] = prag->paramNum + x;
        } else {
          paramnumArray[(*paramIndex)++] = prag->paramNum;
        }
      }
      for (MIRPragmaElement *elem : elemVector) {
        itemNum[annoNum - 1]++;
        string convertTmp = NameMangler::DecodeName(GlobalTables::GetStrTable().GetStringFromStrIdx(elem->namestridx_).c_str());
        ReflectionAnalysis::CompressHighFrequencyStr(convertTmp);
        annoArr += convertTmp;
        GStrIdx strIdx;
        annoArr += "!";
        std::ostringstream oss;
        string tmp;
        annoArr += std::to_string(elem->type_);
        annoArr += "!";
        MapleVector<MIRPragmaElement *> &subelemVector = elem->subelemvec_;
        switch (elem->type_) {
          CaseCondition(annoArr, elem) case kValueAnnotation : annoArr +=
                                                               GetAnnotationValue(subelemVector, elem->typestridx_);
          break;
          case kValueArray:
            annoArr += GetArrayValue(subelemVector);
            break;
          default:
            strIdx.SetIdx(elem->val_.u);
            annoArr += GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);
        }
        annoArr += "!";
      }
    }
  }
  return annoNum;
}

bool ReflectionAnalysis::IsAnonymousClass(std::string annotationString) {
  // eg: `IC!`AF!4!0!name!30!!
  size_t pos = annotationString.find(INNERCLASS_STR, 0);
  if (pos != std::string::npos) {
    int i = 5;
    while (i--) {
      pos = annotationString.find("!", pos + 1);
      CHECK_FATAL(pos != std::string::npos, "Error:annotationString in func: isAnonymousClass()");
    }
    if (annotationString.substr(pos + 1, 2) == "30") {
      return true;
    }
  }
  return false;
}

TyIdx ReflectionAnalysis::GenMetaStructType(MIRModule *mirModule, MIRStructType &metatype, const std::string &str) {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(str);
  TyIdx tyIdx = GlobalTables::GetTypeTable().CreateMIRType(&metatype);
  // Global?
  mirModule->typeNameTab->SetGStrIdxToTyIdx(strIdx, tyIdx);
  mirModule->typeDefOrder.push_back(strIdx);
  const uint32 globalTypeTableSize =  GlobalTables::GetTypeTable().typeTable.size();
  CHECK_FATAL(globalTypeTableSize > tyIdx.GetIdx(), "null ptr check");
  if (GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]->nameStrIdx == GStrIdx(0)) {
    GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->nameStrIdx = strIdx;
  }

  return tyIdx;
}

MIRType *ReflectionAnalysis::GetRefFieldType(MIRBuilder *mirbuilder) {
#ifdef USE_32BIT_REF
  return GlobalTables::GetTypeTable().GetUInt32();
#else
  return GlobalTables::GetTypeTable().GetVoidPtr();
#endif  // USE_32BIT_REF
}

void ReflectionAnalysis::GenMetadataType(MIRModule *mirModule) {
  if (classMetadataTyidx != TyIdx(0))
  // types have been generated
  {
    return;
  }

  MIRBuilder &mirBuilder = *(mirModule->mirBuilder);
  // ClassMetaType
  MIRStructType classMetadataType(kTypeStruct);
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataType, "shadow", GetRefFieldType(&mirBuilder));
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataType, "monitor", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataType, "classloader", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataType, "objsize", GlobalTables::GetTypeTable().GetUInt16());
#ifdef USE_32BIT_REF
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataType, "flag", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataType, "numofsuperclasses", GlobalTables::GetTypeTable().GetUInt16());
#endif  // USE_32BIT_REF
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataType, "itab", GlobalTables::GetTypeTable().GetVoidPtr());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataType, "vtab", GlobalTables::GetTypeTable().GetVoidPtr());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataType, "gctib", GlobalTables::GetTypeTable().GetVoidPtr());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataType, "classinforo", GlobalTables::GetTypeTable().GetVoidPtr());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataType, "clinitbridge", GlobalTables::GetTypeTable().GetVoidPtr());
  classMetadataTyidx = GenMetaStructType(mirModule, classMetadataType, NameMangler::kClassMetadataTypeName);

  MIRStructType classMetadataRoType(kTypeStruct);
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataRoType, "classname", GlobalTables::GetTypeTable().GetVoidPtr());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataRoType, "ifields", GlobalTables::GetTypeTable().GetVoidPtr());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataRoType, "methods", GlobalTables::GetTypeTable().GetVoidPtr());
  // For array, this is component class; For primitive type, this is nullptr;
  // For general class, this is superclass (only one superclass), this is a pointer to a superclass&interface array
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataRoType, "superclass_or_componentclass", GlobalTables::GetTypeTable().GetVoidPtr());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataRoType, "numoffields", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataRoType, "numofmethods", GlobalTables::GetTypeTable().GetUInt16());
#ifndef USE_32BIT_REF
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataRoType, "flag", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataRoType, "numofsuperclasses", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataRoType, "padding", GlobalTables::GetTypeTable().GetUInt32());
#endif  //! USE_32BIT_REF
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataRoType, "mod", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&classMetadataRoType, "annotation", GlobalTables::GetTypeTable().GetVoidPtr());
  classMetadataRoTyidx = GenMetaStructType(mirModule, classMetadataRoType, CLASS_METADATA_RO_TYPE_NAME);

  // MethodMetaType
  MIRStructType methodMetadataType(kTypeStruct);
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataType, "shadow", GetRefFieldType(&mirBuilder));
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataType, "monitor", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataType, "mod", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataType, "methodinforo", GetRefFieldType(&mirBuilder));

  methodMetadataTyidx = GenMetaStructType(mirModule, methodMetadataType, METHOD_METADATA_TYPE_NAME);

  // MethodMetaType RO
  MIRStructType methodMetadataRoType(kTypeStruct);
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataRoType, "methodname", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataRoType, "signaturename", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataRoType, "addr", GlobalTables::GetTypeTable().GetVoidPtr());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataRoType, "annotationvalue", GlobalTables::GetTypeTable().GetVoidPtr());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataRoType, "declaringclass", GetRefFieldType(&mirBuilder));
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataRoType, "flag", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataRoType, "argsize", GlobalTables::GetTypeTable().GetUInt16());
#ifndef USE_32BIT_REF
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataRoType, "padding", GlobalTables::GetTypeTable().GetUInt32());
#endif  //! USE_32BIT_REF
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataRoType, "method_in_vtab_index", GlobalTables::GetTypeTable().GetUInt64());

  methodMetadataRoTyidx = GenMetaStructType(mirModule, methodMetadataRoType, METHOD_METADATA_RO_TYPE_NAME);

  // MethodMetaType Compact
  MIRStructType methodMetadataCompactType(kTypeStruct);
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataCompactType, "mod", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataCompactType, "methodname", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataCompactType, "signaturename", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataCompactType, "addr", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataCompactType, "annotationvalue", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataCompactType, "declaringclass", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataCompactType, "flag", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataCompactType, "argsize", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&methodMetadataCompactType, "method_in_vtab_index", GlobalTables::GetTypeTable().GetUInt32());
  methodMetadataCompactTyidx =
    GenMetaStructType(mirModule, methodMetadataCompactType, METHOD_METADATA_COMPACT_TYPE_NAME);

  // FieldMetaType
  MIRStructType fieldMetadataType(kTypeStruct);
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataType, "shadow", GetRefFieldType(&mirBuilder));
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataType, "monitor", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataType, "mod", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataType, "fieldinforo", GetRefFieldType(&mirBuilder));
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataType, "type", GlobalTables::GetTypeTable().GetVoidPtr());
  fieldMetadataTyidx = GenMetaStructType(mirModule, fieldMetadataType, FIELD_METADATA_TYPE_NAME);

  // FieldMetaType RO
  MIRStructType fieldMetadataRoType(kTypeStruct);
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataRoType, "offset", GlobalTables::GetTypeTable().GetVoidPtr());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataRoType, "declaringclass", GetRefFieldType(&mirBuilder));
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataRoType, "flag", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataRoType, "padding", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataRoType, "fieldname", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataRoType, "annotation", GlobalTables::GetTypeTable().GetInt32());
#ifndef USE_32BIT_REF
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataRoType, "padding1", GlobalTables::GetTypeTable().GetUInt32());
#endif  //! USE_32BIT_REF
  fieldMetadataRoTyidx = GenMetaStructType(mirModule, fieldMetadataRoType, FIELD_METADATA_RO_TYPE_NAME);

  // FieldMetaType Compact
  MIRStructType fieldMetadataCompactType(kTypeStruct);
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataCompactType, "mod", GlobalTables::GetTypeTable().GetUInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataCompactType, "type", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataCompactType, "offset", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataCompactType, "flag", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataCompactType, "padding", GlobalTables::GetTypeTable().GetUInt16());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataCompactType, "fieldname", GlobalTables::GetTypeTable().GetInt32());
  GlobalTables::GetTypeTable().AddFieldToStructType(&fieldMetadataCompactType, "annotation", GlobalTables::GetTypeTable().GetInt32());
  fieldMetadataCompactTyidx = GenMetaStructType(mirModule, fieldMetadataCompactType, FIELD_METADATA_COMPACT_TYPE_NAME);

  /*SuperClassMetaType */
  MIRStructType superclassMetadataType(kTypeStruct);
  GlobalTables::GetTypeTable().AddFieldToStructType(&superclassMetadataType, "superclassinfo", GlobalTables::GetTypeTable().GetVoidPtr());
  superclassMetadataTyidx = GenMetaStructType(mirModule, superclassMetadataType, SUPERCLASS_METADATA_TYPE_NAME);
}

void ReflectionAnalysis::GenClassHashMetaData() {
  MIRType *type = GlobalTables::GetTypeTable().GetVoidPtr();
  CHECK_FATAL(type != nullptr, "type is null in ReflectionAnalysis::GenClassHashMetaData");
  if (raDebug) {
    cerr << "========= HASH TABLE ========" << endl;
  }
  if (classTab.size() == 0) {
    return;
  }
  std::string bucketName = NameMangler::kMuidClassMetadataBucketPrefixStr + mirModule->GetFileNameAsPostfix();

  uint32 bucketArraySize;
  bucketArraySize = classTab.size();
  MIRArrayType *bucketArraytype =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(type, bucketArraySize);
  MIRSymbol *bucketSt = GetOrCreateSymbol(bucketName, bucketArraytype->tyIdx, true);
  MIRAggConst *bucketAggconst = mirModule->memPool->New<MIRAggConst>(mirModule, bucketArraytype);

  for (MIRSymbol *classSt : classTab) {
    if (raDebug && 0) {
      cerr << classSt->GetName() << endl;
    }
    AddrofNode *classExpr = mirBuilder.CreateExprAddrof(0, classSt);
    MIRType *ptrtype = GlobalTables::GetTypeTable().typeTable[PTY_ptr];
    MIRConst *classconst = mirModule->memPool->New<MIRAddrofConst>(classExpr->stIdx, classExpr->fieldID, ptrtype);
    bucketAggconst->constVec.push_back(classconst);
  }
  bucketSt->SetConst(bucketAggconst);
}

static void ReflectionAnalysisGenStrTab(MIRModule *mirModule, const std::string &strtab,
                                        const std::string &strtabName) {
  MIRBuilder *mirbuilder = mirModule->mirBuilder;
  uint32 strtabSize;
  strtabSize = strtab.length();
  MIRArrayType *strtabType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(GlobalTables::GetTypeTable().GetUInt8(), strtabSize);
  MIRSymbol *strtabSt = mirbuilder->CreateGlobalDecl(strtabName, strtabType, kScGlobal);
  MIRAggConst *strtabAggconst = mirModule->memPool->New<MIRAggConst>(mirModule, strtabType);
  strtabSt->SetStorageClass(kScFstatic);
  for (const char &c : strtab) {
    MIRConst *newconst = mirModule->memPool->New<MIRIntConst>(c, GlobalTables::GetTypeTable().GetUInt8());
    strtabAggconst->constVec.push_back(newconst);
  }
  strtabSt->SetConst(strtabAggconst);
}

void ReflectionAnalysis::GenStrTab(MIRModule *mirModule) {
  // hot string tab
  std::string hotStrtabName = NameMangler::kReflectionStartHotStrtabPrefixStr + mirModule->GetFileNameAsPostfix();
  ReflectionAnalysisGenStrTab(mirModule, strtabStartHot, hotStrtabName);

  hotStrtabName = NameMangler::kReflectionBothHotStrTabPrefixStr + mirModule->GetFileNameAsPostfix();
  ReflectionAnalysisGenStrTab(mirModule, strtabBothHot, hotStrtabName);

  hotStrtabName = NameMangler::kReflectionRunHotStrtabPrefixStr + mirModule->GetFileNameAsPostfix();
  ReflectionAnalysisGenStrTab(mirModule, strtabRunHot, hotStrtabName);

  // cold string tab
  std::string strtabName = NameMangler::kReflectionStrtabPrefixStr + mirModule->GetFileNameAsPostfix();
  ReflectionAnalysisGenStrTab(mirModule, strtab, strtabName);
}

void ReflectionAnalysis::MarkWeakMethods() {
  GStrIdx classNames[] = { GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(NameMangler::kJavaLangClassStr),
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(NameMangler::kJavaLangObjectStr),
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(REFLECTION_METHOD_PREFIX_STR),
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(REFLECTION_METHOD_241_PREFIX_STR),
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(REFLECTION_EXECUTABLE_PREFIX_STR),
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(REFLECTION_FIELD_PREFIX_STR),
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(REFLECTION_CONSTRUCTOR_PREFIX_STR),
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(REFLECTION_PROXY_PREFIX_STR),
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(REFLECTION_PROXY_241_PREFIX_STR),
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(REFLECTION_REFERENCE_PREFIX_STR),
#if __OPENJDK__
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(JAVA_LANG_CLASS_PREFIX_STR),
#endif
#if defined(__x86_64__)
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(SUN_REFLECT_REFLECTION_STR),
#endif
                             GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(REFLECTION_ACCESSIBLEOBJECT_PREFIX_STR) };
  for (GStrIdx nameIdx : classNames) {
    Klass *klass = klassh->GetKlassFromStridx(nameIdx);
    if (klass) {
      MIRClassType *classType = klass->GetMIRClassType();
      for (MethodPair const &methodpair : classType->methods) {
        MIRSymbol *funcSym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(methodpair.first.Idx());
        MIRFunction *mirFunc = funcSym->GetFunction();
        if (RootclassDefined()) {
          mirFunc->SetAttr(FUNCATTR_weak);  // it's marked weak since RT-first
        } else {
          mirFunc->SetAttr(FUNCATTR_extern);
        }
      }
    }
  }
}

void ReflectionAnalysis::LoadProfilingData(std::string profileFile, MapleUnorderedSet<std::string> &preloadSet) {
  std::ifstream infile;
  infile.open(profileFile);
  if (infile.fail()) {
    cerr << "Cannot open profile file " << profileFile << "\n";
    return;
  }

  std::string name;
  while (infile >> name) {
    if (name.empty()) {
      continue;
    }
    preloadSet.insert(name);
  }
  infile.close();
}

void ReflectionAnalysis::LoadReflectStrProfile(std::string profileFile,
                                               MapleUnorderedMap<std::string, uint32_t> &hotReflectStrMap) {
  std::ifstream infile;
  infile.open(profileFile);
  if (infile.fail()) {
    cerr << "Cannot open profile file " << profileFile << "\n";
    return;
  }
  uint32_t hotType = HOT_LAYOUT::kStartUpHot;
  std::string line;
  std::string name;
  std::string tag;
  while (infile >> line) {
    if (line.empty()) {
      continue;
    }
    std::size_t pos = line.find_first_of(':');
    name = line.substr(0, pos);
    tag = line.substr(pos + 1);
    if (tag == "boot-only-hot") {
      hotType = HOT_LAYOUT::kStartUpHot;
    } else if (tag == "both-hot") {
      hotType = HOT_LAYOUT::kBothHot;
    } else if (tag == "run-hot") {
      hotType = HOT_LAYOUT::kRunHot;
    }
    hotReflectStrMap.insert(std::make_pair(name, hotType));
  }
  infile.close();
}

void ReflectionAnalysis::Run() {
  uint32 javaNameIdx = mirModule->GetFileinfo(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("INFO_filename"));
  const std::string &javaName = GlobalTables::GetStrTable().GetStringFromStrIdx(GStrIdx(javaNameIdx));
  mirModule->profile.DeCompress(Options::profile, javaName);
  LoadReflectStrProfile(Options::reflectStringProFile, hotReflectStrMap);

  MarkWeakMethods();
  GenMetadataType(mirModule);

  const MapleVector<Klass *> &klasses = klassh->GetTopoSortedKlasses();
  if (raDebug) {
    cerr << "========= Gen Class: Total " << klasses.size() << " ========" << endl;
  }
  // cluster classname together in reflection string table to improve the locality
  for (Klass *klass : klasses) {
    GenHotClassNameString(klass);
  }
  for (Klass *klass : klasses) {
    GenClassMetaData(klass);
  }
  GenClassHashMetaData();
}

AnalysisResult *DoReflectionAnalysis::Run(MIRModule *module, ModuleResultMgr *mrm) {
  MemPool *mp = mempoolctrler.NewMemPool("ReflectionAnalysis mempool");

  KlassHierarchy *kh = static_cast<KlassHierarchy *>(mrm->GetAnalysisResult(MoPhase_CHA, module));

  maple::MIRBuilder mirBuilder(module);
  ReflectionAnalysis *rv = mp->New<ReflectionAnalysis>(module, mp, kh, mirBuilder);
  rv->Run();
  // This is a transform phase, delete mempool
  mempoolctrler.DeleteMemPool(mp);
  return nullptr;
}

}  // namespace maple
