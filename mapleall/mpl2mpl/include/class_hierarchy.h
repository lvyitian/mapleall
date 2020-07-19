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

#ifndef MPL2MPL_INCLUDE_CLASSHIERARCHY_H
#define MPL2MPL_INCLUDE_CLASSHIERARCHY_H
#include "mir_function.h"
#include "module_phase.h"
#include "name_mangler.h"

namespace maple {
class KlassHierarchy;

// should be consistent with runtime (in mrt_reflection_modifier.h)
static constexpr uint32 kClassPrim = 0x0001;
static constexpr uint32 kClassArray = 0x0002;
static constexpr uint32 kClassHasFinalizer = 0x0004;
static constexpr uint32 kClassSoftreference = 0x0008;
static constexpr uint32 kClassWeakreference = 0x0010;
static constexpr uint32 kClassPhantomreference = 0x0020;
static constexpr uint32 kClassFinalizereference = 0x0040;
static constexpr uint32 kClassCleaner = 0x0080;
static constexpr uint32 kClassFinalizerreferenceSentinel = 0x0100;
static constexpr uint32 kClassIsExceptionKlass = 0x0200;
static constexpr uint32 kClassIsanonymousclass = 0x0400;
static constexpr uint32 kClassIscoldclass = 0x0800;
static constexpr uint32 kClassHasNativeMethod = 0x1000;
static const std::string kJavaLangNoMethodStr = std::string(NameMangler::kJavaLang) + "NoSuchMethodException_3B";


#define CLASS_REFERENCE \
  (kClassSoftreference | kClassWeakreference | kClassCleaner | kClassFinalizereference | kClassPhantomreference)

// Klass is the basic node for building class hierarchy
class Klass {
 public:
  struct KlassComparator {
    bool operator()(const Klass *lhs, const Klass *rhs) const {
      return lhs->GetKlassName() < rhs->GetKlassName();
    }
  };

 private:
  // structType can be class or interface
  MIRStructType *structType;
  MapleAllocator *alloc;
  // A collection of super classes.
  // superklass is NULL if it is not defined in the module.
  MapleList<Klass *> superklasses;
  // A collection of sub classes
  MapleSet<Klass *, KlassComparator> subklasses;
  // a collection of classes which implement the current interface
  MapleSet<Klass *, KlassComparator> implklasses;
  // a collection of interfaces which is implemented by the current klass
  MapleSet<Klass *, KlassComparator> implInterfaces;
  // A collection of class member methods
  MapleList<MIRFunction *> methods;
  MIRFunction *clinitMethod;
  MIRSymbol *classInitBridge;
  // A mapping to track possible implementations for each virtual function
  MapleMap<GStrIdx, MapleVector<MIRFunction *> *> strIdx2CandidateMap;
  // flags of this class.
  // Now contains whether this class is exception, reference or has finalizer.
  uint32 flags;
  bool isPrivateInnerAndNoSubClassFlag;
  bool needDecoupling;
  void DumpKlassImplinterfaces() const;
  void DumpKlassImplklasses() const;
  void DumpKlassSuperklasses() const;
  void DumpKlassSubklasses() const;
  void DumpKlassMethods(MIRModule *mod) const;
  bool IsVirtualMethod(const MIRFunction *func) const;

 public:
  Klass(MIRStructType *type, MapleAllocator *alc);
  ~Klass() {}

  // Return true if Klass represents an interface
  bool IsInterface() const {
    return (structType->typeKind == kTypeInterface);
  }

  // Return true if Klass represents a normal java class
  bool IsClass() const {
    return (structType->typeKind == kTypeClass);
  }

  // Return true if found in the member methods
  bool IsKlassMethod(const MIRFunction *func) const;
  // Return MIRFunction if has method
  const MIRFunction *HasMethod(const char *funcname);
  const MapleList<MIRFunction *> &GetMethods() {
    return methods;
  }

  GStrIdx GetKlassNameStridx() const {
    return structType->nameStrIdx;
  }

  const std::string &GetKlassName() const {
    return structType->GetName();
  }

  TyIdx GetTypeIdx() const {
    return structType->tyIdx;
  }

  MIRStructType *GetMIRStructType() const {
    return structType;
  }

  MIRClassType *GetMIRClassType() const {
    return static_cast<MIRClassType *>(structType);
  }

  MIRInterfaceType *GetMIRInterfaceType() const {
    return static_cast<MIRInterfaceType *>(structType);
  }

  bool HasSuperKlass() const {
    return (superklasses.size() != 0);
  }

  bool HasSubKlass() const {
    return (subklasses.size() != 0);
  }

  bool HasImplementInterfaces() const {
    return (implInterfaces.size() != 0);
  }

  bool ImplementsKlass() const;

  void SetFlag(uint32 flag) {
    flags |= flag;
  }

  uint32 GetFlag(uint32 flag) const {
    return flags & flag;
  }

  bool HasFlag(uint32 flag) const {
    return static_cast<bool>(GetFlag(flag) != 0);
  }

  bool IsExceptionKlass() const {
    return HasFlag(kClassIsExceptionKlass);
  }

  void SetExceptionKlass() {
    SetFlag(kClassIsExceptionKlass);
  }

  bool HasFinalizer() {
    return HasFlag(kClassHasFinalizer);
  }

  bool HasNativeMethod() {
    return HasFlag(kClassHasNativeMethod);
  }

  void SetHasNativeMethod() {
    SetFlag(kClassHasNativeMethod);
  }

  void SetHasFinalizer() {
    SetFlag(kClassHasFinalizer);
  }

  bool IsReference(uint32 flag) {
    return HasFlag(flag);
  }

  bool IsReference() {
    return HasFlag(CLASS_REFERENCE);
  }

  bool IsArray() const {
    return (structType->GetName().find(JARRAY_PREFIX_STR) == 0);
  }

  bool IsPrivateInnerAndNoSubClass() const {
    return isPrivateInnerAndNoSubClassFlag;
  }

  void SetPrivateInnerAndNoSubClass(bool flag) {
    isPrivateInnerAndNoSubClassFlag = flag;
  }

  bool NeedDecoupling() const {
    return needDecoupling;
  }

  void SetNeedDecoupling(bool flag) {
    needDecoupling = flag;
  }

  MIRFunction *GetClinit() const {
    return clinitMethod;
  }

  void SetClinit(MIRFunction *m) {
    clinitMethod = m;
  }

  MIRSymbol *GetClassInitBridge() const {
    return classInitBridge;
  }

  void SetClassInitBridge(MIRSymbol *s) {
    classInitBridge = s;
  }

  // Return the function defined in the current class, or the inherited
  // function if it is not defined in the current class.
  MIRFunction *GetClosestMethod(GStrIdx) const;

  // This for class only, which only has 0 or 1 super class
  Klass *GetSuperKlass() const;
  const MapleList<Klass *> &GetSuperKlasses() const {
    return superklasses;
  }

  const MapleSet<Klass *, KlassComparator> &GetSubKlasses() const {
    return subklasses;
  }

  const MapleSet<Klass *, KlassComparator> &GetImplKlasses() const {
    return implklasses;
  }

  const MapleSet<Klass *, KlassComparator> &GetImplInterfaces() const {
    return implInterfaces;
  }

  // Return a vector of possible functions
  MapleVector<MIRFunction *> *GetCandidates(GStrIdx mnameNoklassStridx) const;
  // Return the unique method if there is only one target virtual function.
  // Return NULL if there are multiple targets.
  MIRFunction *GetUniqueMethod(GStrIdx mnameNoklassStridx) const;

  void AddSuperKlass(Klass *superclass) {
    superklasses.push_back(superclass);
  }

  void AddSubKlass(Klass *subclass) {
    subklasses.insert(subclass);
  }

  void AddImplKlass(Klass *implclass) {
    implklasses.insert(implclass);
  }

  void AddImplInterface(Klass *interfaceKlass) {
    implInterfaces.insert(interfaceKlass);
  }

  void AddMethod(MIRFunction *func) {
    methods.push_front(func);
  }

  void DelMethod(const MIRFunction *func);
  // Collect the virtual methods from parent class and interfaces
  void CountVirtMethTopDown(const KlassHierarchy *kh);
  // Count the virtual methods for subclasses and merge with itself
  void CountVirtMethBottomUp();
  void Dump(MIRModule *mod) const;
};

/* Some well known types like java.lang.Object. They may be commonly referenced. */
class WKTypes {
 public:
  static MIRType *jlObject;
  static MIRType *jlString;
  static MIRType *jIoSerializable;
  static MIRType *jlComparable;
  static MIRType *jlCharSequence;
  static MIRType *jlClass;
  static MIRType *jlrGenericDeclaration;
  static MIRType *jlrAnnotatedElement;
  static MIRType *jlrType;
  static MIRType *jlrMethod;
  static MIRType *jlrExecutable;
  static MIRType *jlrAccessibleObject;
  static MIRType *jlrMember;
  static MIRType *jlrField;
  static MIRType *jlrConstructor;

 public:
  static void Init();
  class Util {
   public:
    static bool MayRefString(const base_node_t *n, MIRType *type);
    static bool MayRefMeta(const base_node_t *n, MIRType *type);
    static bool MayNotRefCyclicly(const base_node_t *n, MIRType *type);
    static MIRType *GetJavaLangObjectType() {
      return jlObject;
    }

   private:
    static bool NotCyclicType(MIRType *type, std::set<MIRType *> &worklist);
  };
};

/* data structure to represent class information defined in the module */
class KlassHierarchy : public AnalysisResult {
 private:
  MapleAllocator alloc;
  MIRModule *mirModule;
  // Map from class name to klass. Use name as the key because the type
  // information from Java is incomplete, e.g.
  // method to class link, e.g.
  //    class A { void foo(); void bar(); }
  //    class B extends A { void foo(); }
  //    In this case, there is no link from B.bar to B in the maple file.
  MapleMap<GStrIdx, Klass *> strIdx2KlassMap;
  // Map from a virtual method name to its corresponding real method name
  // This is used for devirtualization and has to be built with a closed-world view
  MapleMap<GStrIdx, GStrIdx> vfunc2rfuncMap;
  MapleVector<Klass *> topoWorklist;
  // New all klass
  void AddKlasses();
  // Add superklass/subclass edge and class methods for each class
  void AddKlassRelationAndMethods();

  void TagThrowableKlasses();
  // Connect all class<->interface edges based on Depth-First Search
  void UpdateImplementedInterfaces();
  // Get a vector of parent class and implementing interface
  void GetParentKlasses(const Klass *klass, std::vector<Klass *> &parentKlasses) const;
  // Get a vector of child class and implemented class
  void GetChildKlasses(const Klass *klass, std::vector<Klass *> &childKlasses) const;
  void ExceptionFlagProp(Klass *klass);
  Klass *AddClassFlag(const std::string name, uint32 flag);

 public:
  static bool traceFlag;

  KlassHierarchy(MIRModule *mirModule, MemPool *mp);
  virtual ~KlassHierarchy() {}

  // Get a class. Return NULL it does not exist.
  Klass *GetKlassFromStridx(GStrIdx strIdx) const;
  Klass *GetKlassFromTyidx(TyIdx tyIdx) const;
  Klass *GetKlassFromFunc(const MIRFunction *func) const;
  Klass *GetKlassFromName(const std::string &name) const;
  Klass *GetKlassFromLiteral(const char *name) const;
  const MapleMap<GStrIdx, Klass *> &GetKlasses() const {
    return strIdx2KlassMap;
  }

  // Get lowest common ancestor for two classes
  Klass *GetLCA(Klass *k1, Klass *k2) const;
  TyIdx GetLCA(TyIdx ty1, TyIdx ty2) const;
  GStrIdx GetLCA(GStrIdx str1, GStrIdx str2) const;
  const std::string &GetLCA(const std::string &name1, const std::string &name2) const;

  // 1/0/-1: true/false/unknown
  int IsSuperKlass(TyIdx supertid, TyIdx basetid) const;
  bool IsSuperKlass(const Klass *super, Klass *base) const;
  bool IsSuperKlassForInterface(const Klass *super, Klass *base) const;
  bool IsInterfaceImplemented(Klass *interface, const Klass *base) const;

  // return true if class, its super or interfaces have at least one clinit function
  bool NeedClinitCheckRecursively(Klass *kl);

  void TopologicalSortKlasses();
  void MarkClassFlags();
  void CountVirtualMethods();
  void BuildHierarchy();
  void Dump() const;

  // Return the unique method if there is only one target virtual function.
  // Return 0 if there are multiple targets or the targets are unclear.
  GStrIdx GetUniqueMethod(GStrIdx) const;

  bool IsDevirtualListEmpty() const;
  void DumpDevirtualList(const std::string &outputFileName) const;
  void ReadDevirtualList(const std::string &inputFileName);

  const MapleVector<Klass *> &GetTopoSortedKlasses() const {
    return topoWorklist;
  }
};

class DoKlassHierarchy : public ModulePhase {
 public:
  explicit DoKlassHierarchy(ModulePhaseID id) : ModulePhase(id) {}

  AnalysisResult *Run(MIRModule *module, ModuleResultMgr *m) override;
  std::string PhaseName() const override {
    return "classhierarchy";
  }

  virtual ~DoKlassHierarchy(){};
};
}  // namespace maple

#endif /*MPL2MPL_INCLUDE_CLASSHIERARCHY_H */
