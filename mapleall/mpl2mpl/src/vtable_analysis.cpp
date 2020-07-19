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
#include "itab_util.h"

namespace maple {
// +1 is needed here because our field id starts with 0 pointing to the struct itself
#define KLASS_ITAB_FIELDID (CLASS::kItab + 1)
#define KLASS_VTAB_FIELDID (CLASS::kVtab + 1)

// Vtableanalysis
// This phase is mainly to generate the virtual table && iterface table.
// The virtual table just store the virtual method address. And the interface
// table structure is a litter more complicated.we stored the hash number of methodname
// and function address.And we also move the hot function to the front iterface
// table.If the hash number is conflicted,we stored the whole completed methodname at the
// end of interface table.

VtableAnalysis::VtableAnalysis(MIRModule *mod, KlassHierarchy *kh, bool dump) : FuncOptimizeImpl(mod, kh, dump) {
  voidPtrType = GlobalTables::GetTypeTable().GetVoidPtr();
  // zeroConst and oneConst are shared amony itab entries. It is safe to share them because
  // they are never removed by anybody.
  zeroConst = module->memPool->New<MIRIntConst>(0, voidPtrType);
  oneConst = module->memPool->New<MIRIntConst>(1, voidPtrType);

  for (Klass *klass : klassHierarchy->GetTopoSortedKlasses()) {
    GenVtableList(klass);
    if (klass->IsClass() && klass->GetMIRStructType()->IsLocal()) {
      // We generate itable/vtable definition in the same place where the class is defined
      GenVtableDefinition(klass);
      GenItableDefinition(klass);
    }
    if (trace) {
      DumpVtableList(klass);
    }
  }
}

bool VtableAnalysis::IsVtableCandidate(const MIRFunction *func) const {
  return (func->GetAttr(FUNCATTR_virtual) && !func->GetAttr(FUNCATTR_private) && !func->GetAttr(FUNCATTR_static));
}

// Return true if virtual functions can be set override relationship cross package
bool VtableAnalysis::CheckOverrideForCrossPackage(const MIRFunction *baseMethod, const MIRFunction *currMethod) const {
  const std::string &baseClassName = baseMethod->GetBaseClassName();
  size_t basePos = baseClassName.rfind(NameMangler::kPackageNameSplitterStr);
  std::string basePackageName = (basePos != std::string::npos) ? baseClassName.substr(0, basePos) : "";

  const std::string &currClassName = currMethod->GetBaseClassName();
  size_t currPos = currClassName.rfind(NameMangler::kPackageNameSplitterStr);
  std::string currPackageName = (currPos != std::string::npos) ? currClassName.substr(0, currPos) : "";

  // For the corss package inheritance, only if the base func is declared
  // as either 'public' or 'protected', we shall set override relationship.
  return ((currPackageName == basePackageName) || baseMethod->GetAttr(FUNCATTR_public) ||
          baseMethod->GetAttr(FUNCATTR_protected));
}

// If the method is not in method_table yet, add it in, otherwise update it.
// Note: the method to add should already pass VtableCandidate test
void VtableAnalysis::AddMethodToTable(MethodPtrVector &methodTable, MethodPair *methodpair) {
  MIRFunction *method = builder->GetFunctionFromStidx(methodpair->first);
  GStrIdx strIdx = method->GetBaseFuncNameWithTypeStridx();

  for (unsigned int i = 0; i < methodTable.size(); i++) {
    MIRFunction *currFunc = builder->GetFunctionFromStidx(methodTable[i]->first);
    GStrIdx currStridx = currFunc->GetBaseFuncNameWithTypeStridx();
    if (strIdx == currStridx) {
      if (CheckOverrideForCrossPackage(currFunc, method)) {
        // only update when it's not an abstract method
        if (!method->IsAbstract()) {
          methodTable[i] = methodpair;
        }
        return;
      }
    }
  }

  methodTable.push_back(methodpair);
}

void VtableAnalysis::GenVtableList(Klass *klass) {
  if (klass->IsInterface()) {
    MIRInterfaceType *itype = klass->GetMIRInterfaceType();
    // add in methods from parent interfaces, note interfaces can declare/define same methods
    for (const Klass *parentKlass : klass->GetSuperKlasses()) {
      MIRInterfaceType *parentItype = parentKlass->GetMIRInterfaceType();
      for (MethodPair *methodpair : parentItype->vTableMethods) {
        AddMethodToTable(itype->vTableMethods, methodpair);
      }
    }
    // add in methods from this interface, note it can override methods of parents
    for (MethodPair &imethodpair : itype->methods) {
      AddMethodToTable(itype->vTableMethods, &imethodpair);
    }
  } else {  // it's a class
    MIRClassType *curtype = klass->GetMIRClassType();
    Klass *superklass = klass->GetSuperKlass();
    // prepare vTableMethods
    // first is vtable from parents. since it's single inheritance, we copy it directly
    if (superklass) {
      MIRStructType *partentype = superklass->GetMIRStructType();
      curtype->vTableMethods = partentype->vTableMethods;
    }
    // vtable from implemented interfaces, need to merge in. both default or none-default
    // Note, all interface methods are also virtual methods, need to be in vtable too.
    for (TyIdx const &tyIdx : curtype->interfacesImplemented) {
      MIRInterfaceType *itype = static_cast<MIRInterfaceType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx));
      for (MethodPair *methodpair : itype->vTableMethods) {
        MIRFunction *method = builder->GetFunctionFromStidx(methodpair->first);
        GStrIdx strIdx = method->GetBaseFuncNameWithTypeStridx();
        Klass *iklass = klassHierarchy->GetKlassFromFunc(method);
        bool found = false;
        for (unsigned int i = 0; i < curtype->vTableMethods.size(); i++) {
          MIRFunction *curMethod = builder->GetFunctionFromStidx(curtype->vTableMethods[i]->first);
          GStrIdx currStridx = curMethod->GetBaseFuncNameWithTypeStridx();
          Klass *currKlass = klassHierarchy->GetKlassFromFunc(curMethod);
          // Interfaces implemented methods can't override methods from parent,
          // except the methods comes from another interface which is a parent of current interface
          if (strIdx == currStridx) {
            if (klassHierarchy->IsSuperKlassForInterface(currKlass, iklass)) {
              curtype->vTableMethods[i] = methodpair;
            }
            found = true;
            break;
          }
        }
        if (!found) {
          curtype->vTableMethods.push_back(methodpair);
        }
      }
    }
    // then methods defined in this class
    for (MethodPair &methodpair : curtype->methods) {
      MIRFunction *curMethod = builder->GetFunctionFromStidx(methodpair.first);
      if (IsVtableCandidate(curMethod)) {
        AddMethodToTable(curtype->vTableMethods, &methodpair);
      }
      // Optimization: mark private methods as local
      if (curtype->IsLocal() && curMethod->IsPrivate() && !curMethod->IsConstructor()) {
        curMethod->SetAttr(FUNCATTR_local);
      }
    }
    // Create initial cached vtable mapping
    for (uint32 i = 0; i < curtype->vTableMethods.size(); i++) {
      MIRFunction *curMethod = builder->GetFunctionFromStidx(curtype->vTableMethods[i]->first);
      puidxToVtabIndex[curMethod->puIdx] = i;
    }
  }
}

void VtableAnalysis::GenVtableDefinition(Klass *klass) {
  MIRStructType *curtype = klass->GetMIRStructType();
  MIRAggConst *newconst = module->memPool->New<MIRAggConst>(module, voidPtrType);
  for (MethodPair *methodpair : curtype->vTableMethods) {
    MIRFunction *vtabMethod = builder->GetFunctionFromStidx(methodpair->first);
    AddroffuncNode *addroffuncNode = builder->CreateExprAddroffunc(vtabMethod->puIdx, module->memPool);
    MIRConst *constNode = module->memPool->New<MIRAddroffuncConst>(addroffuncNode->puIdx, voidPtrType);
    newconst->constVec.push_back(constNode);
  }
  // We also need to generate vtable and itable even if the class does not
  // have any virtual method, or does not implement any interface.  Such a
  // class needs a TRIVIAL vtable, or a TRIVIAL itable, because we need to
  // inform the OBJECT ALLOCATOR that such a class does not need any
  // (non-trivial) vtable or itable.  One way (and the most uniform way) to
  // convey this information is having a trivial table.  Alternatively, we can
  // postpone this step to mplcg, where mplcg discovers all classes that does
  // not have any vtable or itable.
  if (newconst->constVec.empty()) {
    newconst->constVec.push_back(zeroConst);
  }
  // The following assignment is needed due to a C++ compiler bug
  std::string klassname = klass->GetKlassName();
  GenTableSymbol(VTAB_PREFIX_STR, klassname, newconst);
}

void VtableAnalysis::GenItableDefinition(Klass *klass) {
  MIRStructType *curtype = klass->GetMIRStructType();
  std::set<GStrIdx> signatureVisited;
  std::vector<MIRFunction *> firstItabVec(kItabFirstHashSize, nullptr);
  std::vector<bool> firstConflictFlag(kItabFirstHashSize, false);
  std::vector<MIRFunction *> firstConflictList;
  bool itabContainsMethod = false;

  for (Klass *implInterface : klass->GetImplInterfaces()) {
    CHECK_FATAL(implInterface->IsInterface(), "implInterface must be interface");
    MIRInterfaceType *interfaceType = implInterface->GetMIRInterfaceType();
    for (MethodPair &imethodpair : interfaceType->methods) {
      MIRFunction *interfaceMethod = builder->GetFunctionFromStidx(imethodpair.first);
      uint32 hashCode = GetHashIndex(interfaceMethod->GetBaseFuncNameWithType().c_str());
      GStrIdx interfaceMethodStridx = interfaceMethod->GetBaseFuncNameWithTypeStridx();
      if (signatureVisited.find(interfaceMethodStridx) == signatureVisited.end()) {
        signatureVisited.insert(interfaceMethodStridx);
      } else {
        // some interfaces own methods with same signature, cache them in order to
        // prevent processing these methods multiple times
        continue;
      }
      // Search in vtable
      MIRFunction *vtabMethod = nullptr;
      for (MethodPair *methodpair : curtype->vTableMethods) {
        MIRFunction *method = builder->GetFunctionFromStidx(methodpair->first);
        if (interfaceMethodStridx == method->GetBaseFuncNameWithTypeStridx()) {
          vtabMethod = method;
          break;
        }
      }
      CHECK_FATAL(vtabMethod, "Interface method %s is not implemented in class %s",
             interfaceMethod->GetName().c_str(), klass->GetKlassName().c_str());
      if (!vtabMethod->IsAbstract()) {
        itabContainsMethod = true;
        if (!firstItabVec[hashCode] && !firstConflictFlag[hashCode]) {
          firstItabVec[hashCode] = vtabMethod;
        } else {  // a conflict found
          if (!firstConflictFlag[hashCode]) {
            // move itab element to conflict table when first conflict occurs
            firstConflictList.push_back(firstItabVec[hashCode]);
            firstItabVec[hashCode] = nullptr;
            firstConflictFlag[hashCode] = true;
          }
          firstConflictList.push_back(vtabMethod);
        }
      }
    }
  }
  if (!itabContainsMethod) {
    // No need to generate itable in this case
    return;
  }

  std::vector<MIRFunction *> secondItabVec(kItabSecondHashSize, nullptr);
  std::vector<bool> secondConflictFlag(kItabSecondHashSize, false);
  std::vector<MIRFunction *> secondConflictList;
  uint32_t count = 0;
  for (MIRFunction *func : firstConflictList) {
    const std::string &signaturename = func->GetBaseFuncNameWithType();
    uint32_t secondHashCode = GetSecondHashIndex(signaturename.c_str());
    if (!secondItabVec[secondHashCode] && !secondConflictFlag[secondHashCode]) {
      secondItabVec[secondHashCode] = func;
      count++;
    } else {
      if (secondItabVec[secondHashCode]) {
        secondConflictList.push_back(secondItabVec[secondHashCode]);
        secondItabVec[secondHashCode] = nullptr;
        secondConflictFlag[secondHashCode] = true;
      }
      secondConflictList.push_back(func);
    }
  }

  if (count == 0) {
    // If no conflict exists, reduce the unnecessary zero element at the end
    for (int i = kItabFirstHashSize - 1; i >= 0; i--) {
      if (!firstItabVec[i] && !firstConflictFlag[i]) {
        firstItabVec.pop_back();
      } else {
        break;
      }
    }
  }
  // Create the first-level itable, which is directly accessed as an array
  MIRAggConst *firstItabEmitArray = module->memPool->New<MIRAggConst>(module, voidPtrType);
  for (MIRFunction *func : firstItabVec) {
    if (func) {
      firstItabEmitArray->constVec.push_back(module->memPool->New<MIRAddroffuncConst>(func->puIdx, voidPtrType));
    } else {
      firstItabEmitArray->constVec.push_back(zeroConst);
    }
  }

  // initialize conflict solution array
  if (count != 0) {
    MIRAggConst *secondItabEmitArray = module->memPool->New<MIRAggConst>(module, voidPtrType);
    // remember count in secondItabVec
    secondItabEmitArray->constVec.push_back(module->memPool->New<MIRIntConst>(count, voidPtrType));
    secondItabEmitArray->constVec.push_back(oneConst); //padding
    for (uint32_t i = 0; i < kItabSecondHashSize; i++) {
      if (!secondItabVec[i] && !secondConflictFlag[i]) {
        continue;
      } else {
        secondItabEmitArray->constVec.push_back(module->memPool->New<MIRIntConst>(i, voidPtrType));
        if (secondItabVec[i]) {
          secondItabEmitArray->constVec.push_back(
              module->memPool->New<MIRAddroffuncConst>(secondItabVec[i]->puIdx, voidPtrType));
        } else {
          // it measn it was conflict again in the second hash
          secondItabEmitArray->constVec.push_back(oneConst);
        }
      }
    }
    for (MIRFunction *func : secondConflictList) {
      const std::string &signaturename = func->GetBaseFuncNameWithType();
      uint32_t nameIdx = ReflectionAnalysis::FindOrInsertRepeatString(signaturename);
      secondItabEmitArray->constVec.push_back(module->memPool->New<MIRIntConst>(nameIdx, voidPtrType));
      secondItabEmitArray->constVec.push_back(
          module->memPool->New<MIRAddroffuncConst>(func->puIdx, voidPtrType));
    }
    // Create the second-level itable, in which hashcode is looked up by binary searching
    GenTableSymbol(ITAB_CONFLICT_PREFIX_STR, klass->GetKlassName(), secondItabEmitArray);

    // push the conflict symbol to the first-level itable
    StIdx symIdx = GlobalTables::GetGsymTable().GetStIdxFromStrIdx(
        GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(ITAB_CONFLICT_PREFIX_STR + klass->GetKlassName()));
    firstItabEmitArray->constVec.push_back(module->memPool->New<MIRAddrofConst>(symIdx, 0, voidPtrType));
  }

  GenTableSymbol(ITAB_PREFIX_STR, klass->GetKlassName(), firstItabEmitArray);
}

void VtableAnalysis::GenTableSymbol(const std::string &prefix, const std::string &klassName, MIRAggConst *newconst) {
  uint32 arraySize = {uint32(newconst->constVec.size())};
  MIRArrayType *arrayType = GlobalTables::GetTypeTable().GetOrCreateArrayType(voidPtrType, arraySize);
  MIRSymbol *vtabSt = builder->CreateGlobalDecl(prefix + klassName, arrayType, kScGlobal);
  if (klassName == NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangObjectStr)) {
    vtabSt->storageClass = kScGlobal;
  } else {
    vtabSt->storageClass = kScFstatic;
  }
  vtabSt->SetConst(newconst);
}

void VtableAnalysis ::DumpVtableList(const Klass *klass) const{
  LogInfo::MapleLogger() << "=========" << klass->GetKlassName() << "========" << std::endl;
  for (MethodPair *vtableMethod : klass->GetMIRStructType()->vTableMethods) {
    MIRFunction *method = builder->GetFunctionFromStidx(vtableMethod->first);
    LogInfo::MapleLogger() << method->GetName() << std::endl;
  }
}

void VtableAnalysis::ProcessFunc(MIRFunction *func) {
  if (func->IsEmpty()) {
    return;
  }
  SetCurrentFunction(func);

  StmtNode *stmt = func->body->GetFirst();
  StmtNode *next = nullptr;
  while (stmt) {
    next = stmt->GetNext();
    switch (stmt->op) {
      case OP_virtualcallassigned: {
        ReplaceVirtualInvoke(static_cast<CallNode *>(stmt));
        break;
      }
      case OP_interfacecallassigned: {
        ReplaceInterfaceInvoke(static_cast<CallNode *>(stmt));
        break;
      }
      case OP_superclasscallassigned: {
        ReplaceSuperclassInvoke(static_cast<CallNode *>(stmt));
        break;
      }
      case OP_polymorphiccallassigned: {
        ReplacePolymorphicInvoke(static_cast<CallNode *>(stmt));
        break;
      }
      default:
        break;
    }
    stmt = next;
  }
}

void VtableAnalysis::ReplaceSuperclassInvoke(CallNode *stmt) {
  // Because the virtual method may be inherited from its parent, we need to find
  // the actual method target.
  MIRFunction *callee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(stmt->puIdx);
  Klass *klass = klassHierarchy->GetKlassFromFunc(callee);
  ASSERT(klass != nullptr, "Klass not found");

  MapleVector<MIRFunction *> *cands = klass->GetCandidates(callee->GetBaseFuncNameWithTypeStridx());
  // continue to search its implinterfaces
  if (!cands) {
    CHECK_FATAL(false, "Candidates not found for function %s", callee->GetName().c_str());
    for (Klass *implinterface : klass->GetImplInterfaces()) {
      cands = implinterface->GetCandidates(callee->GetBaseFuncNameWithTypeStridx());
      if (cands && cands->size() != 0) {
        break;
      }
    }
  }
  CHECK_FATAL(cands && cands->size() != 0,
         "Dependency Error: function %s cannot be found in %s or any of its superclasses/interfaces",
         callee->GetBaseFuncNameWithType().c_str(), klass->GetKlassName().c_str());
  MIRFunction *actualMirfunc = cands->at(0);
  CHECK_FATAL(actualMirfunc, "Can not find the implementation of %s", callee->GetName().c_str());
  stmt->op = OP_callassigned;
  stmt->puIdx = actualMirfunc->puIdx;
}

void VtableAnalysis::ReplacePolymorphicInvoke(CallNode *stmt) {
  IntrinsiccallNode *intrinCall =
    module->CurFuncCodeMemPool()->New<IntrinsiccallNode>(module, OP_xintrinsiccallassigned);
  intrinCall->intrinsic = INTRN_JAVA_POLYMORPHIC_CALL;
  intrinCall->numOpnds = stmt->numOpnds;
  intrinCall->returnValues = stmt->returnValues;
  intrinCall->nOpnd = stmt->nOpnd;
  currFunc->body->ReplaceStmt1WithStmt2(stmt, intrinCall);
}

BaseNode *VtableAnalysis::GenVtabItabBaseAddr(BaseNode *obj, bool isVirtual) {
  BaseNode *classinfoAddr = ReflectionAnalysis::GenClassinfoAddr(obj, builder);
  MIRStructType *classMetadataType = static_cast<MIRStructType *>(
      GlobalTables::GetTypeTable().GetTypeFromTyIdx(ReflectionAnalysis::classMetadataTyidx));
  return builder->CreateExprIread(voidPtrType, GlobalTables::GetTypeTable().GetOrCreatePointerType(classMetadataType),
      (isVirtual ? KLASS_VTAB_FIELDID : KLASS_ITAB_FIELDID), classinfoAddr);
}

int VtableAnalysis::SearchWithoutRettype(const MIRFunction *callee, const MIRStructType *structType) const {
  // Initilization must be < 0
  int entryoffset = -1;
  GStrIdx calleeSigStridx = callee->GetBaseFuncSigStridx();
  Klass *targetKlass = klassHierarchy->GetKlassFromTyidx(callee->GetReturnTyIdx());
  Klass *curKlass = nullptr;
  for (uint32 id1 = 0; id1 < structType->vTableMethods.size(); id1++) {
    MIRFunction *vtableMethod = builder->GetFunctionFromStidx(structType->vTableMethods[id1]->first);
    if (calleeSigStridx == vtableMethod->GetBaseFuncSigStridx()) {
      Klass *tmpKlass = klassHierarchy->GetKlassFromTyidx(vtableMethod->GetReturnTyIdx());
      if (targetKlass->IsClass() && klassHierarchy->IsSuperKlass(tmpKlass, targetKlass) &&
          (!curKlass || klassHierarchy->IsSuperKlass(curKlass, tmpKlass))) {
        curKlass = tmpKlass;
        entryoffset = id1;
      }
      if (!targetKlass->IsClass()) {
        if (tmpKlass->IsClass() && klassHierarchy->IsInterfaceImplemented(targetKlass, tmpKlass) &&
            (!curKlass || klassHierarchy->IsSuperKlass(curKlass, tmpKlass))) {
          curKlass = tmpKlass;
          entryoffset = id1;
        }
        if (!tmpKlass->IsClass() && klassHierarchy->IsSuperKlassForInterface(tmpKlass, targetKlass) &&
            (!curKlass || klassHierarchy->IsSuperKlass(curKlass, tmpKlass))) {
          curKlass = tmpKlass;
          entryoffset = id1;
        }
      }
    }
  }
  return entryoffset;
}

void VtableAnalysis::ReplaceVirtualInvoke(CallNode *stmt) {
  MIRFunction *callee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(stmt->puIdx);
  MIRPtrType *firstFormalArgType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(callee->formalDefVec[0].formalTyIdx));
  MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(firstFormalArgType->pointedTyIdx);
  MIRStructType *structType = NULL;
  if (pointedty->GetKind() == kTypeJArray) {
    structType = static_cast<MIRJarrayType *>(pointedty)->GetParentType();
  } else {
    CHECK_FATAL(klassHierarchy->GetKlassFromFunc(callee), "Error: missing class %s",
           GlobalTables::GetStrTable().GetStringFromStrIdx(pointedty->nameStrIdx).c_str());
    structType = klassHierarchy->GetKlassFromFunc(callee)->GetMIRStructType();
  }

  int entryoffset = -1;
  if (puidxToVtabIndex.find(stmt->puIdx) != puidxToVtabIndex.end() && puidxToVtabIndex[stmt->puIdx] >= 0) {
    entryoffset = puidxToVtabIndex[stmt->puIdx];
  } else {
    GStrIdx calleeStridx = callee->GetBaseFuncNameWithTypeStridx();
    for (uint32 id1 = 0; id1 < structType->vTableMethods.size(); id1++) {
      MIRFunction *vtableMethod = builder->GetFunctionFromStidx(structType->vTableMethods[id1]->first);
      if (calleeStridx == vtableMethod->GetBaseFuncNameWithTypeStridx()) {
        entryoffset = id1;
        puidxToVtabIndex[callee->puIdx] = id1;
        break;
      }
    }
    // If full match function can't be found, try to ignore type of return value
    if (entryoffset == -1) {
      entryoffset = SearchWithoutRettype(callee, structType);
    }
    CHECK_FATAL(entryoffset != -1,
        "Error: method for virtual call cannot be found in all included mplt files. Call to %s in %s",
        callee->GetName().c_str(), currFunc->GetName().c_str());
  }

  BaseNode *offsetNode = builder->CreateIntConst(entryoffset * TAB_ENTRY_SIZE, PTY_u32);
  BaseNode *tabBaseAddr = GenVtabItabBaseAddr(stmt->nOpnd[0], true);
  BaseNode *addrNode = builder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetPtr(), tabBaseAddr, offsetNode);
  BaseNode *readFuncPtr = builder->CreateExprIread(GlobalTables::GetTypeTable().GetCompactPtr(),
      GlobalTables::GetTypeTable().GetOrCreatePointerType(GlobalTables::GetTypeTable().GetCompactPtr()), 0, addrNode);

  stmt->op = OP_virtualicallassigned;
  stmt->nOpnd.insert(stmt->nOpnd.begin(), readFuncPtr);
  stmt->numOpnds++;
}

void VtableAnalysis::ReplaceInterfaceInvoke(CallNode *stmt) {
  BaseNode *tabBaseAddr = GenVtabItabBaseAddr(stmt->nOpnd[0], false);
  ResolveFuncNode *resolvenode = builder->GetCurrentFuncCodeMp()->New<ResolveFuncNode>(
      OP_resolveinterfacefunc, GlobalTables::GetTypeTable().GetCompactPtr()->GetPrimType(),
      stmt->puIdx, tabBaseAddr, builder->GetConstUInt32(0));
  stmt->op = OP_interfaceicallassigned;
  stmt->nOpnd.insert(stmt->nOpnd.begin(), resolvenode);
  stmt->numOpnds++;
}
}  // namespace maple
