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

#ifndef MPL2MPL_INCLUDE_REFLECTIONANALYSIS_H
#define MPL2MPL_INCLUDE_REFLECTIONANALYSIS_H

#include "class_hierarchy.h"

namespace maple {

#define CLASS_METADATA_RO_TYPE_NAME "__class_meta_ro__"
#define METHOD_METADATA_TYPE_NAME "__method_meta__"
#define METHOD_METADATA_RO_TYPE_NAME "__method_meta_ro__"
#define METHOD_METADATA_COMPACT_TYPE_NAME "__method_meta_compact__"
#define FIELD_METADATA_TYPE_NAME "__field_meta__"
#define FIELD_METADATA_RO_TYPE_NAME "__field_meta_ro__"
#define FIELD_METADATA_COMPACT_TYPE_NAME "__field_meta_compact__"
#define SUPERCLASS_METADATA_TYPE_NAME "__superclass_meta__"

#define INFO_NAME_INDEX 0

// maple field index definition
struct CLASS_RO {
  enum : uint32_t {
    kClassname = 0,
    kIfields,
    kMethods,
    kSuperclass,
    kNumoffields,
    kNumofMethods,
#ifndef USE_32BIT_REF
    kFlag,
    kNumofsup,
    kPadding,
#endif  //! USE_32BIT_REF
    kMod,
    kAnnotation
  };
};

struct CLASS {
  enum : uint32_t {
    kShadow = 0,
    kMonitor,
    kClassloader,
    kObjsize,
#ifdef USE_32BIT_REF
    FLAG,
    NUMOFSUP,
#endif  // USE_32BIT_REF
    kItab,
    kVtab,
    kGctib,
    kInfoRo,
    kClint
  };
};

struct METHOD_RO {
  enum : uint32_t { kMethodname = 0, kSigname, kAddr, kAnnotationvalue, kDeclarclass, kFlag, kArgsize };
};

struct METHOD {
  enum : uint32_t { kShadow = 0, kMonitor, kMod, kInfoR0 };
};

struct METHOD_COMPACT {
  enum : uint32_t { kMod = 0, kMethodname, kSigname, kAddr, kAnnotation, kDeclarclass, kFlag, kArgsize };
};

struct FIELD_RO {
  enum : uint32_t { kOffset = 0, kDeclarclass, kFlag, kPadding, kName, kAnnotation };
};

struct FIELD {
  enum : uint32_t { kShadow = 0, kMonitor, kMod, kInfoRo, kType };
};

struct FIELD_COMPACT {
  enum : uint32_t { kMod = 0, kTypeName, kOffset, kFlag, kPadding, kName, kAnnotation };
};

struct HOT_LAYOUT {
  enum : uint32_t { kStartUpHot = 1, kBothHot = 2, kRunHot = 3 };
};

// If needed, we can make field type in two bits.
#define FIELD_PRIM 0x00000001
#define FIELD_ARRAY 0x00000002
#define FIELD_INTERFACE 0x00000004
#define FIELD_STATIC 0x00000008

#define METHOD_CONSTRUCTOR 0x00000001
#define METHOD_NOTVIRTUAL 0x00000002
#define METHOD_FINALIZE 0x00000004

#define CaseCondition(ARRAYNAME, ELEM)                                              \
  case kValueInt:                                                                   \
    ARRAYNAME += std::to_string(ELEM->val_.i);                                      \
    oss.str();                                                                      \
    break;                                                                          \
  case kValueLong:                                                                  \
    ARRAYNAME += std::to_string(ELEM->val_.j);                                      \
    break;                                                                          \
  case kValueDouble:                                                                \
    oss << tmp << setiosflags(ios::scientific) << setprecision(16) << ELEM->val_.d; \
    ARRAYNAME += oss.str();                                                         \
    break;                                                                          \
  case kValueFloat:                                                                 \
    oss << tmp << setiosflags(ios::scientific) << setprecision(7) << ELEM->val_.f;  \
    ARRAYNAME += oss.str();                                                         \
    break;                                                                          \
  case kValueString: {                                                              \
    strIdx.SetIdx(ELEM->val_.u);                                                      \
    string s = GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);               \
    ReflectionAnalysis::CompressHighFrequencyStr(s);                                \
    delimeterConvert(s);                                                            \
    ARRAYNAME += s;                                                                 \
    break;                                                                          \
  }                                                                                 \
  case kValueEnum:                                                                  \
    strIdx.SetIdx(ELEM->val_.u);                                                      \
    ARRAYNAME += GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);             \
    break;                                                                          \
  case kValueBoolean:                                                               \
    ARRAYNAME += std::to_string(ELEM->val_.u);                                      \
    break;                                                                          \
  case kValueByte:                                                                  \
    ARRAYNAME += std::to_string(ELEM->val_.i);                                      \
    break;                                                                          \
  case kValueShort:                                                                 \
    ARRAYNAME += std::to_string(ELEM->val_.i);                                      \
    break;                                                                          \
  case kValueChar:                                                                  \
    ARRAYNAME += std::to_string(ELEM->val_.u);                                      \
    break;

class ReflectionAnalysis : public AnalysisResult {
 public:
  static const char *klassPtrName;
  static TyIdx classMetadataTyidx;
  static TyIdx classMetadataRoTyidx;
  static TyIdx methodMetadataTyidx;
  static TyIdx methodMetadataRoTyidx;
  static TyIdx methodMetadataCompactTyidx;
  static TyIdx fieldMetadataTyidx;
  static TyIdx fieldMetadataRoTyidx;
  static TyIdx fieldMetadataCompactTyidx;
  static TyIdx superclassMetadataTyidx;
  static void GenStrTab(MIRModule *mirModule);

  static std::string strtab;
  static std::unordered_map<std::string, uint32_t> str2idxMap;
  static std::string strtabStartHot;
  static std::string strtabBothHot;
  static std::string strtabRunHot;
  static uint32_t FindOrInsertRepeatString(const std::string &str, bool isHot = false,
                                           uint32_t hotType = HOT_LAYOUT::kStartUpHot);
  static BaseNode *GenClassinfoAddr(BaseNode *obj, MIRBuilder *builder);

 private:
  MIRModule *mirModule;
  MapleAllocator allocator;
  KlassHierarchy *klassh;
  MIRBuilder &mirBuilder;
  MapleVector<MIRSymbol *> classTab;
  int is_libcore;
  std::map<std::string, std::string> highFrequencyStrMap;
  MapleUnorderedSet<std::string> hotClassSet, hotMethodSet, hotFieldSet;
  MapleUnorderedMap<std::string, uint32_t> hotReflectStrMap;

 public:
  ReflectionAnalysis(MIRModule *mod, MemPool *mp, KlassHierarchy *kh, MIRBuilder &builder)
    : AnalysisResult(mp),
      mirModule(mod),
      allocator(mp),
      klassh(kh),
      mirBuilder(builder),
      classTab(allocator.Adapter()),
      hotClassSet(allocator.Adapter()),
      hotMethodSet(allocator.Adapter()),
      hotFieldSet(allocator.Adapter()),
      hotReflectStrMap(allocator.Adapter()) {
    is_libcore = -1;
    highFrequencyStrMap["Ljava_2Flang_2Fannotation_2FInherited_3B"] = "`IH";
    highFrequencyStrMap["Ljava_2Flang_2Fannotation_2FRepeatable_3B"] = "`RP";
    highFrequencyStrMap["Ljava/lang/Class"] = "`Cl";
    highFrequencyStrMap["Ljava/lang/Object;"] = "`Oj";
    highFrequencyStrMap["Ljava/lang/String;"] = "`ST";
    highFrequencyStrMap["accessFlags"] = "`AF";
    highFrequencyStrMap["value"] = "`VL";
  }

  ~ReflectionAnalysis() {}

  MIRSymbol *GetOrCreateSymbol(const std::string &name, TyIdx tyIdx, bool needInit);
  void GenClassMetaData(Klass *klass);
  std::string GetAnnoValueWithoutArray(const MIRPragmaElement *annoElem);
  void CompressHighFrequencyStr(std::string &s);
  std::string GetArrayValue(MapleVector<MIRPragmaElement *> subelemVector);
  std::string GetAnnotationValue(MapleVector<MIRPragmaElement *> subelemVector, GStrIdx typestridx);
  MIRSymbol *GenSuperClassMetaData(const Klass *klass, std::list<Klass *> superclasslist);
  MIRSymbol *GenFieldsMetaData(const Klass *klass, bool isHot);
  MIRSymbol *GenMethodsMetaData(const Klass *klass, bool isHot);
  static void GenMetadataType(MIRModule *mirModule);
  static MIRType *GetRefFieldType(MIRBuilder *mirbuilder);
  static TyIdx GenMetaStructType(MIRModule *mirModule, MIRStructType &metatype, const std::string &str);
  uint32 GetHashIndex(std::string strname);
  uint32_t FindOrInsertReflectString(const std::string &str);
  uint32 BKDRHash(std::string strname, uint32 seed);
  void GenClassHashMetaData();
  void MarkWeakMethods();
  void Run();
  bool VtableFunc(const MIRFunction *func) const;
  void GenPrimitiveClass();
  bool RootclassDefined();  // wether current module defines root classes
  void GenAllMethodHash(std::vector<std::pair<MethodPair *, int>> &methodinfoVec, bool &isfinalize,
                        std::unordered_map<uint32, std::string> &basenameMp,
                        std::unordered_map<uint32, std::string> &fullnameMp);
  void GenAllFieldHash(std::vector<std::pair<FieldPair, uint16_t>> &fieldV);
  int GeneAnnotation(std::string &annoArr, MIRClassType *ctype, PragmaKind paragKind,
                     const std::string &paragName, int *itemNum,
                     int *paramnumArray = nullptr, int *paramIndex = nullptr);
  void SetAnnoFieldConst(MIRStructType *metadataRoType, MIRAggConst *newconst, uint32 fieldID, int annoNum,
                         const int *itemNum, const std::string &annoArr);
  bool IsAnonymousClass(std::string annotationString);
  void LoadProfilingData(std::string profileFile, MapleUnorderedSet<std::string> &preloadSet);
  void LoadReflectStrProfile(std::string profileFile, MapleUnorderedMap<std::string, uint32_t> &preloadMap);
  static bool IsFinalClass(MIRClassType *classtype);
  void CheckPrivateInnerAndNoSubClass(Klass *clazz, const std::string &annoArr);
  static void ConvertMethodSig(std::string &signature);
};

class DoReflectionAnalysis : public ModulePhase {
 public:
  DoReflectionAnalysis(ModulePhaseID id) : ModulePhase(id) {}

  ~DoReflectionAnalysis() {}

  AnalysisResult *Run(MIRModule *module, ModuleResultMgr *m) override;
  std::string PhaseName() const override {
    return "reflectionanalysis";
  }
};
}  // namespace maple

#endif
