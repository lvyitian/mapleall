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

#include <iostream>
#include <fstream>
#include "class_hierarchy.h"
#include "option.h"
#include "name_mangler.h"
namespace maple {
/*
 *                   Class Hierarchy Anlysis
 * This phase is a foundation phase of compilation. This phase build
 * the class hierarchy for both this module and all modules it depends
 * on. So many phases rely on this phase's analysis result, such as
 * call graph, ssa and so on.
 * The main procedure shows as following.
 * A. Based on the infomation read from mplts, it collect all class that
 *    declared in modules. And create a Klass for each class.
 * B. Fill class method info. Connect superclass<->subclass and
 *    interface->implementation edges.
 * C. Tag All Throwable class and its child class.
 * D. In the case of "class C implements B; interface B extends A;",
 *    we need to add a link between C and A. So we recursively traverse
 *    Klass and collect all interfaces it implements.
 * E. Topological Sort
 * F. Based on Topological Sort Order, for each virtual method in a class,
 *    we collect all its potential implementation. If the number of
 *    potential implementations is 1, it means all virtual call to this
 *    method can be easily devirtualized.
 */
bool KlassHierarchy::traceFlag = false;

Klass::Klass(MIRStructType *type, MapleAllocator *alc)
  : structType(type),
    alloc(alc),
    superklasses(alloc->Adapter()),
    subklasses(alloc->Adapter()),
    implklasses(alloc->Adapter()),
    implInterfaces(alloc->Adapter()),
    methods(alloc->Adapter()),
    clinitMethod(nullptr),
    classInitBridge(nullptr),
    strIdx2CandidateMap(alloc->Adapter()),
    flags(0),
    isPrivateInnerAndNoSubClassFlag(false),
    needDecoupling(true) {
  ASSERT((type != nullptr) && (type->typeKind == kTypeClass || type->typeKind == kTypeInterface), "");
}

void Klass::DumpKlassMethods(MIRModule *mod) const {
  if (methods.size() == 0) {
    return;
  }

  LogInfo::MapleLogger() << "   class member methods:\n";
  for (MIRFunction *const &method : methods) {
    LogInfo::MapleLogger() << "   \t" << method->GetName() << " , ";
    method->GetFuncSymbol()->GetAttrs().DumpAttributes();
    LogInfo::MapleLogger() << std::endl;
  }
  for (auto const &m2cPair : strIdx2CandidateMap) {
    LogInfo::MapleLogger() << "   \t" << GlobalTables::GetStrTable().GetStringFromStrIdx(m2cPair.first) << "   , # of target:" << m2cPair.second->size()
         << std::endl;
  }
}

void Klass::DumpKlassImplklasses() const {
  if (implklasses.size() == 0) {
    return;
  }

  LogInfo::MapleLogger() << "  implemented by:\n";
  for (Klass *const &implklass : implklasses) {
    LogInfo::MapleLogger() << "   \t@implbyclass_idx " << implklass->structType->tyIdx.GetIdx() << "\n";
  }
}

void Klass::DumpKlassImplinterfaces() const {
  if (implInterfaces.size() == 0) {
    return;
  }

  LogInfo::MapleLogger() << "  implements:\n";
  for (Klass *const &interface : implInterfaces) {
    LogInfo::MapleLogger() << "   \t@implinterface_idx " << interface->structType->tyIdx.GetIdx() << "\n";
  }
}

void Klass::DumpKlassSuperklasses() const {
  if (superklasses.size() == 0) {
    return;
  }

  LogInfo::MapleLogger() << "   superclasses:\n";
  for (Klass *const &superklass : superklasses) {
    LogInfo::MapleLogger() << "   \t@superclass_idx " << superklass->structType->tyIdx.GetIdx() << "\n";
  }
}

void Klass::DumpKlassSubklasses() const {
  if (subklasses.size() == 0) {
    return;
  }

  LogInfo::MapleLogger() << "   subclasses:\n";
  for (Klass *const &subklass : subklasses) {
    LogInfo::MapleLogger() << "   \t@subclass_idx " << subklass->structType->tyIdx.GetIdx() << "\n";
  }
}

void Klass::Dump(MIRModule *mod) const {
  // Dump detailed class info
  LogInfo::MapleLogger() << "class \" " << GetKlassName() << " \" @class_id " << structType->tyIdx.GetIdx() << std::endl;
  DumpKlassSuperklasses();
  DumpKlassSubklasses();
  DumpKlassImplinterfaces();
  DumpKlassImplklasses();
  DumpKlassMethods(mod);
}

MIRFunction *Klass::GetClosestMethod(GStrIdx funcnamewithtype) const {
  MapleVector<MIRFunction *> *cands = GetCandidates(funcnamewithtype);
  if (cands && cands->size() != 0) {
    return cands->at(0);
  } else {
    return nullptr;
  }
}

void Klass::DelMethod(const MIRFunction *func) {
  for (auto it = methods.begin(); it != methods.end(); it++) {
    if (*it == func) {
      methods.erase(it);
      return;
    }
  }
}

// This for class only, which only has 0 or 1 super class
Klass *Klass::GetSuperKlass() const {
  switch (superklasses.size()) {
    case 0:
      return nullptr;
    case 1:
      return *superklasses.begin();
    default:
      LogInfo::MapleLogger() << GetKlassName() << std::endl;
      ASSERT(false, "GetSuperKlass expects less than one super class");
      return nullptr;
  }
}

bool Klass::IsKlassMethod(const MIRFunction *func) const {
  if (func == nullptr) {
    return false;
  }
  for (MIRFunction *const &method : methods) {
    if (method == func) {
      return true;
    }
  }
  return false;
}

bool Klass::ImplementsKlass() const {
  if (IsInterface()) {
    return false;
  }
  MIRClassType *ctype = GetMIRClassType();
  ASSERT(ctype, "");
  return (ctype->interfacesImplemented.size() > 0);
}

MapleVector<MIRFunction *> *Klass::GetCandidates(GStrIdx mnameNoklassStridx) const {
  MapleMap<GStrIdx, MapleVector<MIRFunction *> *>::const_iterator it;
  it = strIdx2CandidateMap.find(mnameNoklassStridx);
  return ((it != strIdx2CandidateMap.end()) ? (it->second) : nullptr);
}

MIRFunction *Klass::GetUniqueMethod(GStrIdx mnameNoklassStridx) const {
  if (structType->IsIncomplete()) {
    return nullptr;
  }

  MapleMap<GStrIdx, MapleVector<MIRFunction *> *>::const_iterator it;
  it = strIdx2CandidateMap.find(mnameNoklassStridx);
  if (it != strIdx2CandidateMap.end()) {
    MapleVector<MIRFunction *> *fv = it->second;
    if (fv && fv->size() == 1) {
      return fv->at(0);
    }
  }
  return nullptr;
}

bool Klass::IsVirtualMethod(const MIRFunction *func) const {
  // May add other checking conditions in future
  return (func->GetAttr(FUNCATTR_virtual) && !func->GetAttr(FUNCATTR_private) && !func->GetAttr(FUNCATTR_abstract));
}

void Klass::CountVirtMethTopDown(const KlassHierarchy *kh) {
  MapleVector<MIRFunction *> *vec, *pvec;
  GStrIdx strIdx;
  MapleVector<Klass *> *superAndImplClasses = alloc->GetMemPool()->New<MapleVector<Klass *>>(alloc->Adapter());

  // Add default methods of interface. Add them first because they have lowest
  // priorities compared with methods defined in classes
  for (TyIdx const &tyIdx : GetMIRClassType()->interfacesImplemented) {
    Klass *interface = kh->GetKlassFromTyidx(tyIdx);
    if (interface) {
      superAndImplClasses->push_back(interface);
    }
  }

  // Then add methods from superclasses
  for (Klass *const &superklass : superklasses) {
    superAndImplClasses->push_back(superklass);
  }

  // Initialize strIdx2CandidateMap based on the superclass methods
  for (Klass *const &superAndImplClasse : *superAndImplClasses) {
    ASSERT(superAndImplClasse, "Not a valid super class of interface");
    for (auto const &pair : superAndImplClasse->strIdx2CandidateMap) {
      strIdx = pair.first;
      pvec = pair.second;
      ASSERT(pvec->size() == 1, "Expect exactly one method definition from parent class");
      if (strIdx2CandidateMap.find(strIdx) == strIdx2CandidateMap.end()) {
        vec = alloc->GetMemPool()->New<MapleVector<MIRFunction *>>(alloc->Adapter());
        vec->push_back(pvec->at(0));
        strIdx2CandidateMap[strIdx] = vec;
      } else {
        // Override the method coming from previous klass (must be an interface)
        ASSERT(strIdx2CandidateMap[strIdx]->size() == 1, "Expect exactly one method definition");
        ASSERT(kh->GetKlassFromStridx(strIdx2CandidateMap[strIdx]->at(0)->GetBaseClassNameStridx())->IsInterface(),
                "Override interface default methods");
        // Interfaces implemented methods override, need to determine the inherit relation.
        // class method can override interface method, interface method can override its parent's methods
        vec = strIdx2CandidateMap[strIdx];
        Klass *parentklass = kh->GetKlassFromFunc((*vec)[0]);
        Klass *childklass = kh->GetKlassFromFunc((*pvec)[0]);
        CHECK_FATAL(childklass != nullptr, "childklass is null in Klass::CountVirtMethTopDown");
        if (childklass->IsInterface() && !kh->IsSuperKlassForInterface(parentklass, childklass)) {
          continue;
        }

        ASSERT(vec && vec->size() == 1, "Expect exactly one method definition from parent class");
        (*vec)[0] = (*pvec)[0];
      }
    }
  }

  // Initialize mstridx2count_map based on the current class methods
  for (MIRFunction *const &method : methods) {
    if (IsVirtualMethod(method)) {
      strIdx = method->GetBaseFuncNameWithTypeStridx();
      if (strIdx2CandidateMap.find(strIdx) != strIdx2CandidateMap.end()) {
        // Override the method coming from parent
        vec = strIdx2CandidateMap[strIdx];
        ASSERT(vec && vec->size() == 1, "Expect exactly one method definition from parent class");
        (*vec)[0] = method;
      } else {
        // Newly declared and defined
        vec = alloc->GetMemPool()->New<MapleVector<MIRFunction *>>(alloc->Adapter());
        vec->push_back(method);
        strIdx2CandidateMap[strIdx] = vec;
      }
    }
  }
}

void Klass::CountVirtMethBottomUp() {
  MapleVector<MIRFunction *> *vec;
  GStrIdx strIdx;
  for (Klass *const &subklass : subklasses) {
    ASSERT(subklass, "");
    for (auto const &pair : subklass->strIdx2CandidateMap) {
      strIdx = pair.first;
      if (strIdx2CandidateMap.find(strIdx) != strIdx2CandidateMap.end()) {
        vec = strIdx2CandidateMap[strIdx];
        MapleVector<MIRFunction *> *subv = pair.second;
        if (vec->size() != 0 && subv->size() != 0 && vec->at(0) == subv->at(0)) {
          // If this class and subclass share the same default implementation,
          // then we have to avoid duplicated counting.
          vec->insert(vec->end(), subv->begin() + 1, subv->end());
        } else {
          vec->insert(vec->end(), subv->begin(), subv->end());
        }
      }
    }
  }
}

const MIRFunction *Klass::HasMethod(const char *funcname) {
  for (auto method : methods) {
    if (method->GetBaseFuncNameWithType().find(funcname) != std::string::npos) {
      return method;
    }
  }
  return nullptr;
}

Klass *KlassHierarchy::GetKlassFromStridx(GStrIdx strIdx) const {
  MapleMap<GStrIdx, Klass *>::const_iterator it = strIdx2KlassMap.find(strIdx);
  return ((it != strIdx2KlassMap.end()) ? (it->second) : nullptr);
}

Klass *KlassHierarchy::GetKlassFromTyidx(TyIdx tyIdx) const {
  MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  return (type ? GetKlassFromStridx(type->nameStrIdx) : nullptr);
}

Klass *KlassHierarchy::GetKlassFromFunc(const MIRFunction *func) const {
  return (func ? GetKlassFromStridx(func->GetBaseClassNameStridx()) : nullptr);
}

Klass *KlassHierarchy::GetKlassFromName(const std::string &name) const {
  return GetKlassFromStridx(GlobalTables::GetStrTable().GetStrIdxFromName(name));
}

Klass *KlassHierarchy::GetKlassFromLiteral(const char *name) const {
  return GetKlassFromStridx(GlobalTables::GetStrTable().GetStrIdxFromName(name));
}

// check if super is a superclass of base
// 1/0/-1: true/false/unknown
int KlassHierarchy::IsSuperKlass(TyIdx supertid, TyIdx basetid) const {
  if (supertid == TyIdx(0) || basetid == TyIdx(0)) {
    return -1;
  }
  if (supertid == basetid) {
    return 1;
  }
  Klass *super = GetKlassFromTyidx(supertid);
  Klass *base = GetKlassFromTyidx(basetid);
  if (super == nullptr || base == nullptr) {
    return -1;
  }
  while (base) {
    if (base == super) {
      return 1;
    }
    base = base->GetSuperKlass();
  }
  return 0;
}

bool KlassHierarchy::IsSuperKlass(const Klass *super, Klass *base) const {
  if (super == nullptr || base == nullptr) {
    return false;
  }
  while (base) {
    if (base == super) {
      return true;
    }
    base = base->GetSuperKlass();
  }
  return false;
}

// Interface
bool KlassHierarchy::IsSuperKlassForInterface(const Klass *super, Klass *base) const {
  if (super == nullptr || base == nullptr) {
    return false;
  }
  if (!super->IsInterface() || !base->IsInterface()) {
    return false;
  }

  std::vector<Klass *> tmpVector;
  tmpVector.push_back(base);
  for (unsigned int idx = 0; idx < tmpVector.size(); idx++) {
    if (tmpVector[idx] == super) {
      return true;
    }
    for (Klass *superklass : tmpVector[idx]->GetSuperKlasses()) {
      tmpVector.push_back(superklass);
    }
  }
  return false;
}

bool KlassHierarchy::IsInterfaceImplemented(Klass *interface, const Klass *base) const {
  if (interface == nullptr || base == nullptr) {
    return false;
  }
  if (!interface->IsInterface() || !base->IsClass()) {
    return false;
  }
  // All the implemented interfaces and their parent interfaces
  // are directly stored in this set, so no need to look up super
  return (base->GetImplInterfaces().find(interface) != base->GetImplInterfaces().end());
}

bool KlassHierarchy::NeedClinitCheckRecursively(Klass *kl) {
  Klass *klass = kl;
  if (klass->IsClass()) {
    while (klass) {
      if (klass->GetClinit()) {
        return true;
      }
      klass = klass->GetSuperKlass();
    }

    for (Klass *implInterface : kl->GetImplInterfaces()) {
      if (implInterface->GetClinit()) {
        for (auto &func : implInterface->GetMethods()) {
          if (!func->GetAttr(FUNCATTR_abstract) && !func->GetAttr(FUNCATTR_static)) {
            return true;
          }
        }
      }
    }
    return false;
  } else if (klass->IsInterface()) {
    if (klass->GetClinit()) {
      return true;
    }
    return false;
  } else {
    return true;
  }
}

// Get lowest common ancestor for two classes
Klass *KlassHierarchy::GetLCA(Klass *k1, Klass *k2) const {
  std::vector<Klass *> v1, v2;
  while (k1) {
    v1.push_back(k1);
    k1 = k1->GetSuperKlass();
  }
  while (k2) {
    v2.push_back(k2);
    k2 = k2->GetSuperKlass();
  }

  Klass *result = nullptr;
  unsigned int size1 = v1.size();
  unsigned int size2 = v2.size();
  unsigned int min = (size1 < size2) ? size1 : size2;
  for (unsigned int i = 1; i <= min; i++) {
    if (v1[size1 - i] != v2[size2 - i]) {
      break;
    }
    result = v1[size1 - i];
  }
  return result;
}

TyIdx KlassHierarchy::GetLCA(TyIdx ty1, TyIdx ty2) const {
  Klass *result = GetLCA(GetKlassFromTyidx(ty1), GetKlassFromTyidx(ty2));
  return (result ? result->GetTypeIdx() : TyIdx(0));
}

GStrIdx KlassHierarchy::GetLCA(GStrIdx str1, GStrIdx str2) const {
  Klass *result = GetLCA(GetKlassFromStridx(str1), GetKlassFromStridx(str2));
  return (result ? result->GetKlassNameStridx() : GStrIdx(0));
}

const std::string &KlassHierarchy::GetLCA(const std::string &name1, const std::string &name2) const {
  Klass *result = GetLCA(GetKlassFromName(name1), GetKlassFromName(name2));
  return (result ? result->GetKlassName() : GlobalTables::GetStrTable().GetStringFromStrIdx(0));
}

void KlassHierarchy::AddKlasses() {
  for (MIRType *type : GlobalTables::GetTypeTable().typeTable) {
#if DEBUG
    if (type) {
      MIRTypeKind kd = type->GetKind();
      if (kd == kTypeStructIncomplete || kd == kTypeClassIncomplete || kd == kTypeInterfaceIncomplete)
        LogInfo::MapleLogger() << "Warining: KlassHierarchy::AddKlasses " << GlobalTables::GetStrTable().GetStringFromStrIdx(type->nameStrIdx)
                  << " INCOMPLETE " << std::endl;
    }
#endif
    if (!type || (type->typeKind != kTypeClass && type->typeKind != kTypeInterface)) {
      continue;
    }

    GStrIdx strIdx = type->nameStrIdx;
    Klass *klass = GetKlassFromStridx(strIdx);
    if (klass) {
      if (klass->GetKlassName().compare(NameMangler::kThrowClassStr) == 0) {
        klass->SetExceptionKlass();
      }
      continue;
    }
    MIRStructType *structType = static_cast<MIRStructType *>(type);
    klass = GetMempool()->New<Klass>(structType, &alloc);
    strIdx2KlassMap[strIdx] = klass;
  }
}

void KlassHierarchy::ExceptionFlagProp(Klass *klass) {
  if (klass == nullptr) {
    return;
  }
  klass->SetExceptionKlass();
  for (Klass *subClass : klass->GetSubKlasses()) {
    subClass->SetExceptionKlass();
    ExceptionFlagProp(subClass);
  }
}

void KlassHierarchy::AddKlassRelationAndMethods() {
  for (auto const &pair : strIdx2KlassMap) {
    Klass *klass = pair.second;
    ASSERT(klass, "");
    Klass *superklass = nullptr;
    if (klass->IsInterface()) {
      MIRInterfaceType *itype = klass->GetMIRInterfaceType();
      ASSERT(itype, "");
      // Java interface supports multiple inheritance
      for (TyIdx const &supertyidx : itype->parentsTyIdx) {
        superklass = GetKlassFromTyidx(supertyidx);
        if (superklass) {
          klass->AddSuperKlass(superklass);
          superklass->AddSubKlass(klass);
        }
      }
    } else {
      // Class
      MIRClassType *ctype = klass->GetMIRClassType();
      ASSERT(ctype, "");
      // Add interface relationship
      for (TyIdx const &intftyidx : ctype->interfacesImplemented) {
        Klass *intfklass = GetKlassFromTyidx(intftyidx);
        if (intfklass) {
          intfklass->AddImplKlass(klass);
          klass->AddImplInterface(intfklass);
        }
      }
      superklass = GetKlassFromTyidx(ctype->parentTyIdx);
      // Add superclass/subclass for each class.
      if (superklass) {
        klass->AddSuperKlass(superklass);
        superklass->AddSubKlass(klass);
        // klass implements same interfaces inherited from its parent
        for (Klass *intfklass : superklass->GetImplInterfaces()) {
          intfklass->AddImplKlass(klass);
          klass->AddImplInterface(intfklass);
        }
      }
    }

    // Add method info
    for (auto const &mpair : klass->GetMIRStructType()->methods) {
      MIRSymbol *funcSym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(mpair.first.Idx());
      MIRFunction *method = funcSym->GetFunction();
      klass->AddMethod(method);

      // <clinit> name is classname + kClinitSuffix
      if (method->GetName().compare(klass->GetKlassName() + NameMangler::kClinitSuffix) == 0) {
        klass->SetClinit(method);
      }
    }
  }
  // Propagate isExceptionKlass flag
  ExceptionFlagProp(GetKlassFromLiteral(NameMangler::kThrowClassStr));
  if (GetKlassFromLiteral(NameMangler::kThrowClassStr)) {
    ASSERT(GetKlassFromLiteral(NameMangler::kThrowClassStr)->IsExceptionKlass(), "must be exception class");
  }
  if (GetKlassFromLiteral(kJavaLangNoMethodStr.c_str())) {
    ASSERT(GetKlassFromLiteral(kJavaLangNoMethodStr.c_str())->IsExceptionKlass(),
            "must be exception class");
  }
}

void KlassHierarchy::TagThrowableKlasses() {
  Klass *throwable = GetKlassFromName(NameMangler::kThrowClassStr);
  if (throwable == nullptr) {
    return;
  }

  ASSERT(throwable, "unexpeced null klass for java.lang.Throwable");
  for (auto const &pair : strIdx2KlassMap) {
    Klass *klass = pair.second;
    ASSERT(klass, "unexpeced null klass");
    if (!klass->IsInterface() && IsSuperKlass(throwable, klass)) {
      klass->SetExceptionKlass();
    }
  }
}

static void CollectImplInterfaces(const Klass *klass, std::set<Klass *> &implInterfaceSet) {
  for (Klass *superklass : klass->GetSuperKlasses()) {
    if (implInterfaceSet.find(superklass) == implInterfaceSet.end()) {
      if (superklass->IsInterface()) {
        implInterfaceSet.insert(superklass);
      }
      CollectImplInterfaces(superklass, implInterfaceSet);
    }
  }
  for (Klass *interfaceklass : klass->GetImplInterfaces()) {
    if (implInterfaceSet.find(interfaceklass) == implInterfaceSet.end()) {
      implInterfaceSet.insert(interfaceklass);
      CollectImplInterfaces(interfaceklass, implInterfaceSet);
    }
  }
}

void KlassHierarchy::UpdateImplementedInterfaces() {
  for (auto const &pair : strIdx2KlassMap) {
    Klass *klass = pair.second;
    ASSERT(klass, "");
    if (!klass->IsInterface()) {
      std::set<Klass *> implInterfaceSet;
      CollectImplInterfaces(klass, implInterfaceSet);
      for (auto interface : implInterfaceSet) {
        // Add missing parent interface to class link
        interface->AddImplKlass(klass);
        klass->AddImplInterface(interface);
      }
    }
  }
}

void KlassHierarchy::GetParentKlasses(const Klass *klass, std::vector<Klass *> &parentKlasses) const {
  for (Klass *superklass : klass->GetSuperKlasses()) {
    parentKlasses.push_back(superklass);
  }

  if (!klass->IsInterface()) {
    for (Klass *iklass : klass->GetImplInterfaces()) {
      parentKlasses.push_back(iklass);
    }
  }
}

void KlassHierarchy::GetChildKlasses(const Klass *klass, std::vector<Klass *> &childKlasses) const {
  for (Klass *subklass : klass->GetSubKlasses()) {
    childKlasses.push_back(subklass);
  }

  if (klass->IsInterface()) {
    for (Klass *implklass : klass->GetImplKlasses()) {
      childKlasses.push_back(implklass);
    }
  }
}

void KlassHierarchy::TopologicalSortKlasses() {
  std::set<Klass *> inQueue;  // Local variable, no need to use MapleSet

  for (auto const &pair : strIdx2KlassMap) {
    Klass *klass = pair.second;
    ASSERT(klass, "klass can not be nullptr");
    if (!klass->HasSuperKlass() && !klass->ImplementsKlass()) {
      topoWorklist.push_back(klass);
      inQueue.insert(klass);
    }
  }

  // Top-down iterates all nodes
  for (unsigned i = 0; i < topoWorklist.size(); i++) {
    Klass *klass = topoWorklist[i];
    std::vector<Klass *> childklasses;
    GetChildKlasses(klass, childklasses);
    for (Klass *childklass : childklasses) {
      if (inQueue.find(childklass) == inQueue.end()) {
        // callee has not been visited
        bool parentklassAllVisited = true;
        std::vector<Klass *> parentklasses;
        GetParentKlasses(childklass, parentklasses);
        // Check whether all callers of the current callee have been visited
        for (Klass *parentklass : parentklasses) {
          if (inQueue.find(parentklass) == inQueue.end()) {
            parentklassAllVisited = false;
            break;
          }
        }
        if (parentklassAllVisited) {
          topoWorklist.push_back(childklass);
          inQueue.insert(childklass);
        }
      }
    }
  }
}

void KlassHierarchy::CountVirtualMethods() {
  // Top-down iterates all klass nodes
  for (unsigned i = 0; i < topoWorklist.size(); i++) {
    topoWorklist[i]->CountVirtMethTopDown(this);
  }
  // Bottom-up iterates all klass nodes
  for (int i = topoWorklist.size() - 1; i >= 0; i--) {
    topoWorklist[i]->CountVirtMethBottomUp();
  }
}

Klass *KlassHierarchy::AddClassFlag(const std::string name, uint32 flag) {
  Klass *refKlass = GetKlassFromLiteral(name.c_str());
  if (refKlass) {
    refKlass->SetFlag(flag);
  }

  return refKlass;
}

// Mark klasses those implement the finalize method, or inherit
// from super klass (except java.lang.Object).
// Mark klasses those or superclasses are references.
void KlassHierarchy::MarkClassFlags() {
  Klass *cleanerKlass = AddClassFlag(std::string("Lsun_2Fmisc_2FCleaner") + NameMangler::kClassMethodSplitterStr, kClassCleaner);
  AddClassFlag(NameMangler::kJavaLang + std::string("ref_2FSoftReference") + NameMangler::kClassMethodSplitterStr, kClassSoftreference);
  AddClassFlag(NameMangler::kJavaLang + std::string("ref_2FWeakReference") + NameMangler::kClassMethodSplitterStr, kClassWeakreference);
  AddClassFlag(NameMangler::kJavaLang + std::string("ref_2FPhantomReference") + NameMangler::kClassMethodSplitterStr, kClassPhantomreference);
  AddClassFlag(NameMangler::kJavaLang + std::string("ref_2FFinalizerReference") + NameMangler::kClassMethodSplitterStr, kClassFinalizereference);
  AddClassFlag(NameMangler::kJavaLang + std::string("ref_2FFinalizerReference_24Sentinel") + NameMangler::kClassMethodSplitterStr, kClassFinalizerreferenceSentinel);

  Klass *klassObject = GetKlassFromLiteral(NameMangler::kJavaLangObjectStr);
  GStrIdx finalize = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("finalize_7C_28_29V");

  for (Klass *klass : topoWorklist) {
    if (klass == klassObject) {
      continue;
    }
    if (klass->IsInterface()) {
      continue;
    }

    Klass *super = klass->GetSuperKlass();

    // Mark Reference
    // sun.misc.Cleaner's superclass is PhantomReference.
    // Break this chain to process Cleaner correctly.
    if (super && super->IsReference() && klass != cleanerKlass) {
      klass->SetFlag(super->GetFlag(CLASS_REFERENCE));
    }

    // Mark Finalizer
    if (super && super->HasFinalizer()) {
      klass->SetHasFinalizer();
    } else {
      for (auto &method : klass->GetMethods()) {
        if (method->GetBaseFuncNameWithTypeStridx() == finalize) {
          klass->SetHasFinalizer();
          break;
        }
      }
    }

    // Mark native function info
    for (auto &method : klass->GetMethods()) {
      if (method->GetAttr(FUNCATTR_native)) {
        klass->SetHasNativeMethod();
        break;
      }
    }
  }
}

void KlassHierarchy::Dump() const {
  for (Klass *klass : topoWorklist) {
    klass->Dump(mirModule);
  }
}

GStrIdx KlassHierarchy::GetUniqueMethod(GStrIdx vfuncNameStridx) const {
  MapleMap<GStrIdx, GStrIdx>::const_iterator it = vfunc2rfuncMap.find(vfuncNameStridx);
  return (it != vfunc2rfuncMap.end() ? it->second : GStrIdx(0));
}

bool KlassHierarchy::IsDevirtualListEmpty() const {
  return vfunc2rfuncMap.empty();
}

void KlassHierarchy::DumpDevirtualList(const std::string &outputFileName) const {
  std::unordered_map<std::string, std::string> funcMap;
  for (Klass *klass : topoWorklist) {
    for (MIRFunction *func : klass->GetMethods()) {
      MIRFunction *uniqCand = klass->GetUniqueMethod(func->GetBaseFuncNameWithTypeStridx());
      if (uniqCand) {
        funcMap[func->GetName()] = uniqCand->GetName();
      }
    }
  }

  std::ofstream outputFile;
  outputFile.open(outputFileName);
  for (auto it : funcMap) {
    outputFile << it.first << "\t" << it.second << "\n";
  }
  outputFile.close();
}

void KlassHierarchy::ReadDevirtualList(const std::string &inputFileName) {
  std::ifstream inputFile;
  inputFile.open(inputFileName);
  std::string vfuncName, rfuncName;
  while (inputFile >> vfuncName >> rfuncName) {
    vfunc2rfuncMap[GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(vfuncName)] =
      GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(rfuncName);
  }
  inputFile.close();
}

void KlassHierarchy::BuildHierarchy() {
  // Scan class list and generate Klass without method information
  AddKlasses();
  // Fill class method info. Connect superclass<->subclass and
  // interface->implementation edges.
  AddKlassRelationAndMethods();

  TagThrowableKlasses();

  // In the case of "class C implements B; interface B extends A;",
  // we need to add a link between C and A.
  UpdateImplementedInterfaces();
  TopologicalSortKlasses();
  MarkClassFlags();

  if (!strIdx2KlassMap.empty()) {
    WKTypes::Init();
  }
  // Use --dump-devirtual-list=... to dump a closed-wolrd analysis result into a file
  if (!Options::dumpDevirtualList.empty()) {
    DumpDevirtualList(Options::dumpDevirtualList);
  }
  // Use --read-devirtual-list=... to read in a closed-world analysis result
  if (!Options::readDevirtualList.empty()) {
    ReadDevirtualList(Options::readDevirtualList);
  }
}

KlassHierarchy::KlassHierarchy(MIRModule *mirModule, MemPool *mp)
  : AnalysisResult(mp),
    alloc(mp),
    mirModule(mirModule),
    strIdx2KlassMap(alloc.Adapter()),
    vfunc2rfuncMap(alloc.Adapter()),
    topoWorklist(alloc.Adapter()) {}

MIRType *WKTypes::jlObject;
MIRType *WKTypes::jlString;
MIRType *WKTypes::jIoSerializable;
MIRType *WKTypes::jlComparable;
MIRType *WKTypes::jlCharSequence;
MIRType *WKTypes::jlClass;
MIRType *WKTypes::jlrGenericDeclaration;
MIRType *WKTypes::jlrAnnotatedElement;
MIRType *WKTypes::jlrType;
MIRType *WKTypes::jlrMethod;
MIRType *WKTypes::jlrExecutable;
MIRType *WKTypes::jlrAccessibleObject;
MIRType *WKTypes::jlrMember;
MIRType *WKTypes::jlrField;
MIRType *WKTypes::jlrConstructor;

void WKTypes::Init() {
  GStrIdx gStrIdx;
  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangObjectStr);
  jlObject = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangStringStr);
  jlString = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaIoSerializable);
  jIoSerializable = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangComparable);
  jlComparable = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangCharSequence);
  jlCharSequence = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangClassStr);
  jlClass = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangReflectGenericDeclaration);
  jlrGenericDeclaration = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangReflectAnnotateElement);
  jlrAnnotatedElement = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangReflectType);
  jlrType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangReflectMethod);
  jlrMethod = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangReflectExecutable);
  jlrExecutable = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangReflectAccessibleObject);
  jlrAccessibleObject = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangReflectMember);
  jlrMember = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangReflectField);
  jlrField = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));

  gStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangReflectConstructor);
  jlrConstructor = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(gStrIdx));
}

// Return true if node n may point to a String object.
// String is declared as:
//   public final class String implements java.io.Serializable, Comparable<String>, CharSequence
// So n can point to String only if n's type is a ref to String or Object, or is an interface
// type of java.io.Serializable, Comparable or CharSequence
bool WKTypes::Util::MayRefString(const base_node_t *n, MIRType *type) {
  if ((n->primType == PTY_ref || n->primType == PTY_ptr) && type->typeKind == kTypePointer) {
    MIRPtrType *pointtype = static_cast<MIRPtrType *>(type);
    MIRType *pointedType = pointtype->GetPointedType();
    if (pointedType == jlString || pointedType == jIoSerializable || pointedType == jlComparable ||
        pointedType == jlCharSequence || pointedType == jlObject) {
      return true;
    }
  }
  return false;
}

// Return true if node n may point to Meta object, i.e, n is a reference
// of java.lang.Class, java.lang.reflect.Method, java.lang.reflect.Field,
// java.lang.reflect.Constructor
bool WKTypes::Util::MayRefMeta(const base_node_t *n, MIRType *type) {
  if ((n->primType == PTY_ref || n->primType == PTY_ptr) && type->typeKind == kTypePointer) {
    MIRPtrType *pointtype = static_cast<MIRPtrType *>(type);
    MIRType *pointedType = pointtype->GetPointedType();
    /*
       Definition of java.lang.Class:
       public final class Class<T> implements java.io.Serializable,
       GenericDeclaration,
       Type,
       AnnotatedElement {...}
       public interface Serializable {}
       public interface GenericDeclaration extends AnnotatedElement {...}
       public interface AnnotatedElement {...}
       public interface Type {...}
       public interface AnnotatedElement {...}
     */
    if (pointedType == jlClass || pointedType == jIoSerializable || pointedType == jlrGenericDeclaration ||
        pointedType == jlrAnnotatedElement || pointedType == jlrType || pointedType == jlObject) {
      return true;
    }
    /*
       Definition of java.lang.reflect.Method:
       public final class Method extends Executable {...}

       public abstract class Executable extends AccessibleObject
       implements Member, GenericDeclaration {...}
       public class AccessibleObject implements AnnotatedElement {...}
       public interface AnnotatedElement {...}
       public interface Member {...}
       public interface GenericDeclaration extends AnnotatedElement {...}
       public interface AnnotatedElement {...}
     */
    if (pointedType == jlrMethod || pointedType == jlrExecutable || pointedType == jlrAccessibleObject ||
        pointedType == jlrAnnotatedElement || pointedType == jlrMember || pointedType == jlrGenericDeclaration ||
        pointedType == jlrAnnotatedElement || pointedType == jlObject) {
      return true;
    }
    /*
       Definition of java.lang.reflect.Field:
       public final class Field extends AccessibleObject implements Member {...}
     */
    if (pointedType == jlrField) {  // super classes/interfaces are checked by jlrMethod
      return true;
    }

    /*
       Definition of java.lang.reflect.Constructor:
       public final class Constructor<T> extends Executable {...}
     */
    if (pointedType == jlrConstructor) {  // super classes are checked by jlrMethod
      return true;
    }
  }
  return false;
}

// Return true if node 'n', whose type is 'type', can not form a reference cycle.
// E.g, all fields are primitive types, String, array of primitive, etc.
bool WKTypes::Util::MayNotRefCyclicly(const base_node_t *n, MIRType *type) {
  CHECK_FATAL((n->primType == PTY_ref || n->primType == PTY_ptr) && type->typeKind == kTypePointer, "n must be a reference");
  std::set<MIRType *> worklist;
  worklist.insert(type);
  return NotCyclicType(type, worklist);
}

// Helper function of WKTypes::Util::MayNotRefCyclicly.
bool WKTypes::Util::NotCyclicType(MIRType *type, std::set<MIRType *> &worklist) {

  // Fast path for special cases: String, Class
  if (type == jlString || type == jlClass) {
    return true;
  }
  if (type->typeKind == kTypeScalar) {  // primitive type
    return true;
  } else if (type->typeKind == kTypePointer) {
    MIRType *pointedType = static_cast<MIRPtrType *>(type)->GetPointedType();
    return NotCyclicType(pointedType, worklist);
  } else if (type->typeKind == kTypeJArray) {
    MIRType *elemtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRJarrayType *>(type)->elemTyIdx);
    return NotCyclicType(elemtype, worklist);
  } else if (type->typeKind == kTypeClass) {
    MIRClassType *classtype = static_cast<MIRClassType *>(type);
    if (!classtype->IsFinal()) {  // Not sure what actual class it refers to
      return false;
    }
    std::vector<FieldPair> allfields;
    allfields.insert(allfields.end(), classtype->fields.begin(), classtype->fields.end());
    // ignore static fields for now, they are not recycled anyway
    MIRType *parenttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(classtype->parentTyIdx);
    while (parenttype) {
      CHECK_FATAL(parenttype->typeKind == kTypeClass, "parent class");
      MIRClassType *parentclass = static_cast<MIRClassType *>(parenttype);
      allfields.insert(allfields.end(), parentclass->fields.begin(), parentclass->fields.end());
      // ignore static fields for now, they are not recycled anyway
      parenttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentclass->parentTyIdx);
    }
    // Ignore interface fields, as they are all static final (constant) variables
    for (FieldPair &fieldpair : allfields) {
      MIRType *fieldtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldpair.second.first);
      if (fieldtype->typeKind == kTypePointer) {
        // Strictly, we should check whether fieldtype is 'assignable' from any type
        // recorded in worklist, which means a cycle may be formed. However, this
        // function returns false for any non-final calss (see above), so we only
        // check if fieldtype is 'equal' to any type recorded in the worklist.
        // An example:
        // class B extends A {
        //    A field_a;
        // }
        // field_a may point to an instance of B and henceforce a possible cycle.
        // Since A is not a final class, we can recognize it as a cycle candidate.
        if (worklist.find(fieldtype) != worklist.end()) {
          return false;
        }
        worklist.insert(fieldtype);
      }
      if (!NotCyclicType(fieldtype, worklist)) {
        return false;
      }
    }
    return true;
  } else if (type->typeKind == kTypeInterface) {
    // Perhaps something more can do.
    return false;
  }
  return false;
}

}  // namespace maple
