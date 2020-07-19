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

#include <fstream>
#include "muid_replacement.h"
#include "vtable_analysis.h"
#include "reflection_analysis.h"
#include "name_mangler.h"

namespace maple {

// MUIDReplacement
// This phase is mainly to enable the maple linker about the text and data structure.
// It will do the following things:
// A) It collect the methods, classinfo, vtable, itable , and etc.And then it will generate the
// basic data structures like func_def, func_undef, data_def, data_undef using these symbols.
//
// B) It will replace the relevant reference about the methods and static variable with def or undef
// table.And then we can close these symbols to reduce the code size.
//
//
#define MUID_FUNC_PTR_STR "__muid_funcptr"
#define MUID_SYM_PTR_STR "__muid_symptr"

MUIDReplacement::MUIDReplacement(MIRModule *mod, KlassHierarchy *kh, bool dump) :
    FuncOptimizeImpl(mod, kh, dump),
    funcDefMap(less<MUID_t>()),
    dataDefMap(less<MUID_t>()),
    funcUndefMap(less<MUID_t>()),
    dataUndefMap(less<MUID_t>()),
    defMuidIdxMap(less<MUID_t>()) {
  isLibcore =
      (GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangObjectStr))) != nullptr);
  GenTables();
}

void MUIDReplacement::DumpMUIDFile(bool isFunc) {
  ofstream outfile;
  const string &mplName = module->GetFileName();
  string prefix;
  if (mplName.rfind(".mpl") != string::npos) {
    prefix = mplName.substr(0, mplName.rfind(".mpl"));
  } else if (mplName.rfind(".bpl") != string::npos) {
    prefix = mplName.substr(0, mplName.rfind(".bpl"));
  } else {
    CHECK_FATAL(false, "can not find .mpl or .bpl");
  }
  string outfileName;
  if (isFunc) {
    outfileName = prefix + ".func.muid";
  } else {
    outfileName = prefix + ".data.muid";
  }
  outfile.open(outfileName);
  if (outfile.fail()) {
    return;
  }
  size_t begin = mplName.find("libmaple");
  size_t end = mplName.find("_", begin);
  std::string outname;
  if (begin != string::npos && end != string::npos && end > begin) {
    outname = mplName.substr(begin, end - begin);
  } else {
    outname = mplName;
  }
  if (isFunc) {
    for (auto keyVal : funcDefMap) {
      outfile << outname << " ";
      MIRSymbol *mirFunc = keyVal.second.first;
      outfile << mirFunc->GetName() << " ";
      outfile << keyVal.first.ToStr() << std::endl;
    }
  } else {
    for (auto keyVal : dataDefMap) {
      outfile << outname << " ";
      MIRSymbol *mirsym = keyVal.second.first;
      outfile << mirsym->GetName() << " ";
      outfile << keyVal.first.ToStr() << std::endl;
    }
  }
  outfile.close();
}

void MUIDReplacement::CollectFuncAndData() {
  // Iterate klasses
  for (Klass *klass : klassHierarchy->GetTopoSortedKlasses()) {
    MIRStructType *structType = klass->GetMIRStructType();
    // DefTable and UndefTable are placed where a class is defined
    if (!structType || !structType->IsLocal()) {
      continue;
    }
    // Collect FuncDefSet
    for (MethodPair &methodpair : structType->methods) {
      MIRSymbol *funcSym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(methodpair.first.Idx());
      MIRFunction *mirFunc = funcSym->GetFunction();
      CHECK_FATAL(mirFunc, "Invalid function symbol for Class %s", structType->GetName().c_str());
      if (mirFunc && mirFunc->body) {
        AddDefFunc(mirFunc);
      }
    }
    // Cases where an external method can be referred:
    // 1. vtable entry (what we are dealing with here)
    // 2. direct call (collected later when iterating function bodies)
    if (!klass->IsInterface()) {
      for (MethodPair *vmethodpair : structType->vTableMethods) {
        CHECK_FATAL(vmethodpair, "Invalid vTableMethods entry for Class %s", structType->GetName().c_str());
        if (vmethodpair) {
          MIRSymbol *funcSym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(vmethodpair->first.Idx());
          MIRFunction *mirFunc = funcSym->GetFunction();
          if (mirFunc && !mirFunc->body && !mirFunc->IsAbstract()) {
            AddUndefFunc(mirFunc);
          }
        }
      }
    }
  }

  // Iterate global symbols
  for (size_t i = 1; i < GlobalTables::GetGsymTable().GetSymbolTableSize(); i++) {
    // entry 0 is reserved as nullptr
    MIRSymbol *mirsym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(i);
    CHECK_FATAL(mirsym, "Invalid global data symbol at index %d", i);
    if (mirsym->GetStorageClass() == kScGlobal) {
      if (mirsym->IsReflectionClassInfo()) {
        if (!mirsym->IsForcedGlobalClassinfo() &&
            kPreloadedClassinfo.find(mirsym->GetName()) == kPreloadedClassinfo.end()) {
          // With maple linker, global data can be declared as local
          mirsym->SetStorageClass(kScFstatic);
        }
        if (mirsym->GetConst()) {
          // Use this to exclude forward-declared classinfo symbol
          AddDefData(mirsym);
        }
      } else if (mirsym->IsStatic()) {
        if (!mirsym->IsForcedGlobalStaticfield()) {
          mirsym->SetStorageClass(kScFstatic);
        }
        AddDefData(mirsym);
      }
    } else if (mirsym->GetStorageClass() == kScExtern && (mirsym->IsReflectionClassInfo() || mirsym->IsStatic())) {
      AddUndefData(mirsym);
    }
  }

  // Iterate function bodies
  for (MIRFunction *mirFunc : module->functionList) {
    if (!mirFunc->body) {
      continue;
    }
    StmtNode *stmt = mirFunc->body->GetFirst();
    while (stmt) {
      PUIdx puIdx = 0;
      switch (stmt->op) {
        case OP_call:
        case OP_callassigned: {
          puIdx = static_cast<CallNode *>(stmt)->puIdx;
          break;
        }
        case OP_dassign: {
          // epre in ME may have splitted a direct call into addroffunc and an indirect call
          BaseNode *rhs = static_cast<DassignNode *>(stmt)->GetRhs();
          if (rhs && rhs->op == OP_addroffunc) {
            puIdx = static_cast<AddroffuncNode *>(rhs)->puIdx;
          }
          break;
        }
        case OP_regassign: {
          BaseNode *rhs = static_cast<RegassignNode *>(stmt)->uOpnd;
          if (rhs && rhs->op == OP_addroffunc) {
            puIdx = static_cast<AddroffuncNode *>(rhs)->puIdx;
          }
          break;
        }
        default:
          break;
      }
      if (puIdx != 0) {
        MIRFunction *mirFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
        CHECK_FATAL(mirFunc, "Invalid function symbol");
        if (mirFunc && !mirFunc->body && (mirFunc->IsJava() || !mirFunc->GetBaseClassName().empty())) {
          AddUndefFunc(mirFunc);
        }
      }
      // Some stmt requires classinfo but is lowered in CG. Handle them here.
      CollectImplicitUndefClassinfo(stmt);
      stmt = stmt->GetNext();
    }
  }
}

void MUIDReplacement::CollectImplicitUndefClassinfo(StmtNode *stmt) {
  BaseNode *rhs = nullptr;
  std::vector<MIRStructType *> classTyVec;
  if (stmt->op == OP_dassign) {
    DassignNode *dnode = static_cast<DassignNode *>(stmt);
    rhs = dnode->GetRhs();
  } else if (stmt->op == OP_regassign) {
    RegassignNode *rnode = static_cast<RegassignNode *>(stmt);
    rhs = rnode->uOpnd;
  } else if (stmt->op == OP_javacatch || stmt->op == OP_catch) {
    CatchNode *jnode = static_cast<CatchNode *>(stmt);
    for (TyIdx tyIdx : jnode->exceptionTyIdxVec) {
      MIRPtrType *ptrType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx));
      MIRType *ptype = ptrType->GetPointedType();
      if (ptype) {
        if (ptype->typeKind == kTypeClass || ptype->typeKind == kTypeInterface) {
          classTyVec.push_back(static_cast<MIRStructType *>(ptype));
        } else if (ptype == GlobalTables::GetTypeTable().GetVoid()) {
          Klass *objectKlass = klassHierarchy->GetKlassFromLiteral(NameMangler::kJavaLangObjectStr);
          if (objectKlass) {
            classTyVec.push_back(objectKlass->GetMIRStructType());
          }
        }
      }
    }
  }

  if (rhs && rhs->op == OP_gcmalloc) {
    // GCMalloc may require more classinfo than what we have seen so far
    GCMallocNode *gcmalloc = static_cast<GCMallocNode *>(rhs);
    classTyVec.push_back(static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(gcmalloc->tyIdx)));
  } else if (rhs && rhs->op == OP_intrinsicopwithtype) {
    IntrinsicopNode *intrinNode = static_cast<IntrinsicopNode *>(rhs);
    if (intrinNode->intrinsic == INTRN_JAVA_CONST_CLASS || intrinNode->intrinsic == INTRN_JAVA_INSTANCE_OF) {
      MIRPtrType *ptrType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(intrinNode->tyIdx));
      MIRType *ptype = ptrType->GetPointedType();
      if (ptype && (ptype->typeKind == kTypeClass || ptype->typeKind == kTypeInterface)) {
        classTyVec.push_back(static_cast<MIRStructType *>(ptype));
      }
    }
  }
  for (MIRStructType *classType : classTyVec) {
    if (!classType) {
      continue;
    }
    std::string classinfoName = CLASSINFO_PREFIX_STR + classType->GetName();
    MIRSymbol *classSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(classinfoName));
    if (!classSym) {
      classSym = builder->CreateGlobalDecl(classinfoName, GlobalTables::GetTypeTable().GetPtr(), kScGlobal);
      classSym->storageClass = kScExtern;
      AddUndefData(classSym);
    }
  }
}

void MUIDReplacement::InsertFunctionProfile(MIRFunction *curFunc, int64 index) {
  // insert profile code  __profile_func_tab[idx] = __profile_func_tab[idx] + 1
  AddrofNode *baseExpr = nullptr;
  MIRArrayType *arrayType = nullptr;
  baseExpr = builder->CreateExprAddrof(0, funcProfileTabSym, module->memPool);
  arrayType = static_cast<MIRArrayType *>(funcProfileTabSym->GetType());
  ConstvalNode *offsetExpr = builder->CreateIntConst(index, PTY_i64);
  ConstvalNode *incExpr = builder->CreateIntConst(1, PTY_i64);
  MapleVector<BaseNode *> opnds(builder->GetCurrentFuncCodeMpAllocator()->Adapter());
  opnds.push_back(baseExpr);
  opnds.push_back(offsetExpr);
  ArrayNode *arrayExpr = builder->CreateExprArray(arrayType, opnds);
  arrayExpr->boundsCheck = false;
  MIRType *elemType = arrayType->GetElemType();
  BaseNode *ireadPtrExpr =
      builder->CreateExprIread(GlobalTables::GetTypeTable().GetVoidPtr(), GlobalTables::GetTypeTable().GetOrCreatePointerType(elemType), 0, arrayExpr);
  BaseNode *addExpr = builder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetUInt32(), ireadPtrExpr, incExpr);
  MIRType *destPtrType = GlobalTables::GetTypeTable().GetOrCreatePointerType(TyIdx(PTY_u32));
  StmtNode *funcProfileIAssign = builder->CreateStmtIassign(destPtrType, 0, arrayExpr, addExpr);
  curFunc->body->InsertFirst(funcProfileIAssign);
}

void MUIDReplacement::GenFuncDefTable() {
  // Use funcDefMap to make sure funcDefTab is sorted by an increasing order of MUID
  for (MIRFunction *mirFunc : funcDefSet) {
    MUID_t muid = GetMUID(mirFunc->GetName());
    CHECK_FATAL(funcDefMap.find(muid) == funcDefMap.end(), "MUID has been used before, possible collision");
    // Use 0 as the index for now. It will be back-filled once we have the whole map.
    funcDefMap[muid] = SymIdxPair(mirFunc->GetFuncSymbol(), 0);
    if (Options::maplelinkerTransformLocal && !mirFunc->GetFuncSymbol()->IsForcedGlobalFunc()) {
      // Turning this function into file-local is not safe for QEMU build due to static binding
      mirFunc->SetAttr(FUNCATTR_local);
    }
  }
  uint32 idx = 0;
  uint32 arraySize;
  arraySize = funcDefMap.size();

  MIRArrayType *muidIdxArrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(GlobalTables::GetTypeTable().GetUInt32(), arraySize);
  MIRAggConst *muidIdxTabConst = module->memPool->New<MIRAggConst>(module, muidIdxArrayType);
  for (auto &keyVal : funcDefMap) {
    // Fill in the real index
    keyVal.second.second = idx++;
    // Use the muid index for now. It will be back-filled once we have the whole vector.
    MIRIntConst *indexConst = module->memPool->New<MIRIntConst>(keyVal.second.second, GlobalTables::GetTypeTable().GetUInt32());
    muidIdxTabConst->constVec.push_back(indexConst);
  }

  FieldVector parentFields;
  FieldVector fields;
  GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "funcUnifiedAddr", GlobalTables::GetTypeTable().GetCompactPtr());
  MIRStructType *funcDefTabEntryType = static_cast<MIRStructType *>(
      GlobalTables::GetTypeTable().GetOrCreateStructType("MUIDFuncDefTabEntry", fields, parentFields, module));

  FieldVector funcinffields;
  GlobalTables::GetTypeTable().PushIntoFieldVector(&funcinffields, "funcSize", GlobalTables::GetTypeTable().GetUInt32());
  GlobalTables::GetTypeTable().PushIntoFieldVector(&funcinffields, "funcName", GlobalTables::GetTypeTable().GetUInt32());
  MIRStructType *funcInfTabEntryType = static_cast<MIRStructType *>(
      GlobalTables::GetTypeTable().GetOrCreateStructType("MUIDFuncInfTabEntry", funcinffields, parentFields, module));

  FieldVector muidfields;
#ifdef USE_64BIT_MUID
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidLow", GlobalTables::GetTypeTable().GetUInt32());
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidHigh", GlobalTables::GetTypeTable().GetUInt32());
#else
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidLow", GlobalTables::GetTypeTable().GetUInt64());
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidHigh", GlobalTables::GetTypeTable().GetUInt64());
#endif  // USE_64BIT_MUID

  MIRStructType *funcDefMuidTabEntryType = static_cast<MIRStructType *>(
      GlobalTables::GetTypeTable().GetOrCreateStructType("MUIDFuncDefMuidTabEntry", muidfields, parentFields, module));

  MIRArrayType *arrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(funcDefTabEntryType, arraySize);
  MIRAggConst *funcDefTabConst = module->memPool->New<MIRAggConst>(module, arrayType);
  MIRArrayType *funcInfArrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(funcInfTabEntryType, arraySize);
  MIRAggConst *funcInfTabConst = module->memPool->New<MIRAggConst>(module, funcInfArrayType);
  MIRArrayType *muidArrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(funcDefMuidTabEntryType, arraySize);
  MIRAggConst *funcDefMuidTabConst = module->memPool->New<MIRAggConst>(module, muidArrayType);

  // Create funcDefSet to store functions sorted by address
  std::vector<std::pair<MIRSymbol*, MUID_t>> funcDefArray;
  idx = 0;
  for (MIRFunction *mirFunc : module->functionList) {
    MUID_t muid = GetMUID(mirFunc->GetName());
    std::map<MUID_t, SymIdxPair>::iterator iter = funcDefMap.find(muid);
    if (mirFunc->body == NULL || iter == funcDefMap.end()) {
      continue;
    }
    funcDefArray.push_back(make_pair(mirFunc->GetFuncSymbol(), muid));
    // Create muidIdxTab to store the index in funcDefTab and funcDefMuidTab
    // With muidIdxTab, we can use index sorted by muid to find the index in funcDefTab and funcDefMuidTab
    // Use the left 1 bit of muidIdx to mark wether the function is weak or not. 1 is for weak
    MIRIntConst *indexConst;
    if (kReflectionlist.find(mirFunc->GetName()) != kReflectionlist.end()) {
      indexConst = module->memPool->New<MIRIntConst>((1 << 31) | idx, GlobalTables::GetTypeTable().GetUInt32());
    } else {
      indexConst = module->memPool->New<MIRIntConst>(idx, GlobalTables::GetTypeTable().GetUInt32());
    }
    uint32 muidIdx = iter->second.second;
    muidIdxTabConst->constVec[muidIdx] = indexConst;
    if(Options::profileFunc) {
      InsertFunctionProfile(mirFunc, idx);
    }
    // Store the real idx of funcdefTab, for ReplaceAddroffuncConst->FindIndexFromDefTable
    defMuidIdxMap[muid] = idx;
    idx++;
    if (trace) {
      LogInfo::MapleLogger() << "funcDefMap, MUID: " << muid.ToStr() << ", Function Name: " << iter->second.first->GetName()
                << ", Offset in addr order: " << idx - 1
                << ", Offset in muid order: "<< iter->second.second << std::endl;
    }
  }

  // Create funcDefTab, funcInfoTab and funcMuidTab sorted by address, funcMuidIdxTab sorted by muid
  for (auto keyVal : funcDefArray) {
    MIRSymbol *funcSym = keyVal.first;
    MUID_t muid = keyVal.second;
    MIRAggConst *entryConst = module->memPool->New<MIRAggConst>(module, funcDefTabEntryType);
    uint32 fieldID = 1;
    MIRAggConst *funcInfEntryConst = module->memPool->New<MIRAggConst>(module, funcInfTabEntryType);
    uint32 funcinffieldid = 1;
    MIRAggConst *muidEntryConst = module->memPool->New<MIRAggConst>(module, funcDefMuidTabEntryType);
    uint32 muidFieldid = 1;

    // To be processed by runtime
    builder->AddAddroffuncFieldConst(funcDefTabEntryType, entryConst, fieldID++, funcSym);
    funcDefTabConst->constVec.push_back(entryConst);

    // To be emitted as method size by CG
    builder->AddAddroffuncFieldConst(funcInfTabEntryType, funcInfEntryConst, funcinffieldid++, funcSym);
    // To be emitted as method name by CG
    builder->AddAddroffuncFieldConst(funcInfTabEntryType, funcInfEntryConst, funcinffieldid++, funcSym);
    funcInfTabConst->constVec.push_back(funcInfEntryConst);

    builder->AddIntFieldConst(funcDefMuidTabEntryType, muidEntryConst, muidFieldid++, muid.data.words[0]);
    builder->AddIntFieldConst(funcDefMuidTabEntryType, muidEntryConst, muidFieldid++, muid.data.words[1]);
    funcDefMuidTabConst->constVec.push_back(muidEntryConst);
    mplMuidStr += muid.ToStr();
  }
  if (funcDefTabConst->constVec.size() != 0) {
    std::string funcDefTabName = NameMangler::kMuidFuncDefTabPrefixStr + module->GetFileNameAsPostfix();
    funcDefTabSym = builder->CreateGlobalDecl(funcDefTabName, arrayType, kScGlobal);
    funcDefTabSym->SetConst(funcDefTabConst);
    funcDefTabSym->SetStorageClass(kScFstatic);
  }
  if (funcInfTabConst->constVec.size() != 0) {
    std::string funcInfTabName = NameMangler::kMuidFuncInfTabPrefixStr + module->GetFileNameAsPostfix();
    funcInfTabSym = builder->CreateGlobalDecl(funcInfTabName, funcInfArrayType, kScGlobal);
    funcInfTabSym->SetConst(funcInfTabConst);
    funcInfTabSym->SetStorageClass(kScFstatic);
  }
  if (funcDefMuidTabConst->constVec.size() != 0) {
    std::string funcDefMuidTabName = NameMangler::kMuidFuncDefMuidTabPrefixStr + module->GetFileNameAsPostfix();
    funcDefMuidTabSym = builder->CreateGlobalDecl(funcDefMuidTabName, muidArrayType, kScGlobal);
    funcDefMuidTabSym->SetConst(funcDefMuidTabConst);
    funcDefMuidTabSym->SetStorageClass(kScFstatic);
  }
  if (muidIdxTabConst->constVec.size() != 0) {
    std::string muidIdxTabName = NameMangler::kMuidFuncMuidIdxTabPrefixStr + module->GetFileNameAsPostfix();
    funcMuidIdxTabSym = builder->CreateGlobalDecl(muidIdxTabName, muidIdxArrayType, kScGlobal);
    funcMuidIdxTabSym->SetConst(muidIdxTabConst);
    funcMuidIdxTabSym->SetStorageClass(kScFstatic);
  }
  if (Options::dumpMuidFile) {
    DumpMUIDFile(true);
  }

  if (Options::profileFunc) {
    uint32 funcIdx = 0;
    // create a table each field is 4 byte record the call times
    MIRAggConst *funcCallTimesConst = module->memPool->New<MIRAggConst>(module, muidIdxArrayType);
    uint32 start = 0;
    for (start = 0; start < arraySize; start++) {
      MIRIntConst *indexConst = module->memPool->New<MIRIntConst>(0, GlobalTables::GetTypeTable().GetUInt32());
      funcCallTimesConst->constVec.push_back(indexConst);
    }
    if (arraySize) {
      std::string funcProfileName = NameMangler::kFunctionProfileTabPrefixStr + module->GetFileNameAsPostfix();
      funcProfileTabSym = builder->CreateGlobalDecl(funcProfileName, muidIdxArrayType, kScGlobal);
      funcProfileTabSym->SetConst(funcCallTimesConst);
      funcProfileTabSym->SetStorageClass(kScFstatic);
    }
  }
}

void MUIDReplacement::GenDataDefTable() {
  // Use dataDefMap to make sure dataDefTab is sorted by an increasing order of MUID
  for (MIRSymbol *mirsym : dataDefSet) {
    MUID_t muid = GetMUID(mirsym->GetName());
    CHECK_FATAL(dataDefMap.find(muid) == dataDefMap.end(), "MUID has been used before, possible collision");
    // Use 0 as the index for now. It will be back-filled once we have the whole map.
    dataDefMap[muid] = SymIdxPair(mirsym, 0);
  }
  uint32_t idx = 0;
  for (auto &keyVal : dataDefMap) {
    // Fill in the real index
    keyVal.second.second = idx++;
  }

  FieldVector parentFields;
  FieldVector fields;
  GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "dataUnifiedAddr", GlobalTables::GetTypeTable().GetCompactPtr());
  MIRStructType *dataDefTabEntryType = static_cast<MIRStructType *>(
      GlobalTables::GetTypeTable().GetOrCreateStructType("MUIDDataDefTabEntry", fields, parentFields, module));
  FieldVector muidfields;
#ifdef USE_64BIT_MUID  // USE_64BIT_MUID
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidLow", GlobalTables::GetTypeTable().GetUInt32());
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidHigh", GlobalTables::GetTypeTable().GetUInt32());
#else  // USE_128BIT_MUID
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidLow", GlobalTables::GetTypeTable().GetUInt64());
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidHigh", GlobalTables::GetTypeTable().GetUInt64());
#endif
  MIRStructType *dataDefMuidTabEntryType = static_cast<MIRStructType *>(
      GlobalTables::GetTypeTable().GetOrCreateStructType("MUIDDataDefMuidTabEntry", muidfields, parentFields, module));
  uint32 arraySize;
  arraySize = dataDefMap.size();
  MIRArrayType *arrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(dataDefTabEntryType, arraySize);
  MIRAggConst *dataDefTabConst = module->memPool->New<MIRAggConst>(module, arrayType);
  MIRArrayType *muidArrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(dataDefMuidTabEntryType, arraySize);
  MIRAggConst *dataDefMuidTabConst = module->memPool->New<MIRAggConst>(module, muidArrayType);

  for (auto keyVal : dataDefMap) {
    MIRSymbol *mirsym = keyVal.second.first;
    MIRAggConst *entryConst = module->memPool->New<MIRAggConst>(module, dataDefTabEntryType);
    uint32 fieldID = 1;
    MUID_t muid = keyVal.first;
    MIRAggConst *muidEntryConst = module->memPool->New<MIRAggConst>(module, dataDefMuidTabEntryType);
    uint32 muidfieldid = 1;
    // Will be emitted as 0 and processed by runtime
    builder->AddAddrofFieldConst(dataDefTabEntryType, entryConst, fieldID++, mirsym);
    dataDefTabConst->constVec.push_back(entryConst);
    builder->AddIntFieldConst(dataDefMuidTabEntryType, muidEntryConst, muidfieldid++, muid.data.words[0]);
    builder->AddIntFieldConst(dataDefMuidTabEntryType, muidEntryConst, muidfieldid++, muid.data.words[1]);
    dataDefMuidTabConst->constVec.push_back(muidEntryConst);
    mplMuidStr += muid.ToStr();
    if (trace) {
      LogInfo::MapleLogger() << "dataDefMap, MUID: " << muid.ToStr() << ", Variable Name: " << mirsym->GetName()
                << ", Offset: " << keyVal.second.second << std::endl;
    }
  }
  if (Options::dumpMuidFile) {
    DumpMUIDFile(false);
  }
  if (dataDefTabConst->constVec.size() != 0) {
    std::string dataDefTabName = NameMangler::kMuidDataDefTabPrefixStr + module->GetFileNameAsPostfix();
    dataDefTabSym = builder->CreateGlobalDecl(dataDefTabName, arrayType, kScGlobal);
    dataDefTabSym->SetConst(dataDefTabConst);
    dataDefTabSym->SetStorageClass(kScFstatic);
  }
  if (dataDefMuidTabConst->constVec.size() != 0) {
    std::string dataDefMuidTabName = NameMangler::kMuidDataDefMuidTabPrefixStr + module->GetFileNameAsPostfix();
    dataDefMuidTabSym = builder->CreateGlobalDecl(dataDefMuidTabName, muidArrayType, kScGlobal);
    dataDefMuidTabSym->SetConst(dataDefMuidTabConst);
    dataDefMuidTabSym->SetStorageClass(kScFstatic);
  }
}

void MUIDReplacement::GenUnifiedUndefTable() {
  for (MIRFunction *mirFunc : funcUndefSet) {
    MUID_t muid = GetMUID(mirFunc->GetName());
    CHECK_FATAL(funcUndefMap.find(muid) == funcUndefMap.end(), "MUID has been used before, possible collision");
    // Use 0 as the index for now. It will be back-filled once we have the whole map.
    funcUndefMap[muid] = SymIdxPair(mirFunc->GetFuncSymbol(), 0);
  }
  for (MIRSymbol *mirsym : dataUndefSet) {
    MUID_t muid = GetMUID(mirsym->GetName());
    CHECK_FATAL(dataUndefMap.find(muid) == dataUndefMap.end(), "MUID has been used before, possible collision");
    // Use 0 as the index for now. It will be back-filled once we have the whole map.
    dataUndefMap[muid] = SymIdxPair(mirsym, 0);
  }

  // Fill in the real index
  uint32_t idx = 0;
  for (auto &keyVal : funcUndefMap) {
    keyVal.second.second = idx++;
  }
  idx = 0;
  for (auto &keyVal : dataUndefMap) {
    keyVal.second.second = idx++;
  }

  FieldVector parentFields;
  FieldVector fields;
  GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "globalAddress", GlobalTables::GetTypeTable().GetCompactPtr());
  MIRStructType *unifiedUndefTabEntryType = static_cast<MIRStructType *>(
      GlobalTables::GetTypeTable().GetOrCreateStructType("MUIDUnifiedUndefTabEntry", fields, parentFields, module));
  FieldVector muidfields;
#ifdef USE_64BIT_MUID
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidLow", GlobalTables::GetTypeTable().GetUInt32());
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidHigh", GlobalTables::GetTypeTable().GetUInt32());
#else
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidLow", GlobalTables::GetTypeTable().GetUInt64());
  GlobalTables::GetTypeTable().PushIntoFieldVector(&muidfields, "muidHigh", GlobalTables::GetTypeTable().GetUInt64());
#endif
  MIRStructType *unifiedUndefMuidTabEntryType = static_cast<MIRStructType *>(
      GlobalTables::GetTypeTable().GetOrCreateStructType("MUIDUnifiedUndefMuidTabEntry", muidfields, parentFields, module));
  uint32_t arraySize;
  arraySize = funcUndefMap.size();
  MIRArrayType *funcArrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(unifiedUndefTabEntryType, arraySize);
  MIRAggConst *funcUndefTabConst = module->memPool->New<MIRAggConst>(module, funcArrayType);
  MIRArrayType *funcMuidArrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(unifiedUndefMuidTabEntryType, arraySize);
  MIRAggConst *funcUndefMuidTabConst = module->memPool->New<MIRAggConst>(module, funcMuidArrayType);
  for (auto keyVal : funcUndefMap) {
    MUID_t muid = keyVal.first;
    mplMuidStr += muid.ToStr();
    if (trace) {
      LogInfo::MapleLogger() << "funcUndefMap, MUID: " << muid.ToStr() << ", Function Name: " << keyVal.second.first->GetName()
                << ", Offset: " << keyVal.second.second << std::endl;
    }

    MIRAggConst *entryConst = module->memPool->New<MIRAggConst>(module, unifiedUndefTabEntryType);
    uint32 fieldID = 1;
    MIRAggConst *muidEntryConst = module->memPool->New<MIRAggConst>(module, unifiedUndefMuidTabEntryType);
    uint32 muidfieldid = 1;
    // to be filled by runtime
    builder->AddIntFieldConst(unifiedUndefTabEntryType, entryConst, fieldID++, 0);
    funcUndefTabConst->constVec.push_back(entryConst);
    builder->AddIntFieldConst(unifiedUndefMuidTabEntryType, muidEntryConst, muidfieldid++, muid.data.words[0]);
    builder->AddIntFieldConst(unifiedUndefMuidTabEntryType, muidEntryConst, muidfieldid++, muid.data.words[1]);
    funcUndefMuidTabConst->constVec.push_back(muidEntryConst);
  }
  if (funcUndefTabConst->constVec.size() != 0) {
    std::string funcUndefTabName = NameMangler::kMuidFuncUndefTabPrefixStr + module->GetFileNameAsPostfix();
    funcUndefTabSym = builder->CreateGlobalDecl(funcUndefTabName, funcArrayType, kScGlobal);
    funcUndefTabSym->SetConst(funcUndefTabConst);
    funcUndefTabSym->SetStorageClass(kScFstatic);
  }
  if (funcUndefMuidTabConst->constVec.size() != 0) {
    std::string funcUndefMuidTabName = NameMangler::kMuidFuncUndefMuidTabPrefixStr + module->GetFileNameAsPostfix();
    funcUndefMuidTabSym = builder->CreateGlobalDecl(funcUndefMuidTabName, funcMuidArrayType, kScGlobal);
    funcUndefMuidTabSym->SetConst(funcUndefMuidTabConst);
    funcUndefMuidTabSym->SetStorageClass(kScFstatic);
  }
  // Continue to generate dataUndefTab
  arraySize = dataUndefMap.size();
  MIRArrayType *dataArrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(unifiedUndefTabEntryType, arraySize);
  MIRAggConst *dataUndefTabConst = module->memPool->New<MIRAggConst>(module, dataArrayType);
  MIRArrayType *dataMuidArrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(unifiedUndefMuidTabEntryType, arraySize);
  MIRAggConst *dataUndefMuidTabConst = module->memPool->New<MIRAggConst>(module, dataMuidArrayType);
  for (auto keyVal : dataUndefMap) {
    MIRAggConst *entryConst = module->memPool->New<MIRAggConst>(module, unifiedUndefTabEntryType);
    uint32 fieldID = 1;
    MIRSymbol *mirsym = keyVal.second.first;
    MUID_t muid = keyVal.first;
    MIRAggConst *muidEntryConst = module->memPool->New<MIRAggConst>(module, unifiedUndefMuidTabEntryType);
    uint32 muidfieldid = 1;
    // Will be emitted as 0 and filled by runtime
    builder->AddAddrofFieldConst(unifiedUndefTabEntryType, entryConst, fieldID++, mirsym);
    dataUndefTabConst->constVec.push_back(entryConst);
    builder->AddIntFieldConst(unifiedUndefMuidTabEntryType, muidEntryConst, muidfieldid++, muid.data.words[0]);
    builder->AddIntFieldConst(unifiedUndefMuidTabEntryType, muidEntryConst, muidfieldid++, muid.data.words[1]);
    dataUndefMuidTabConst->constVec.push_back(muidEntryConst);
    mplMuidStr += muid.ToStr();
    if (trace) {
      LogInfo::MapleLogger() << "dataUndefMap, MUID: " << muid.ToStr() << ", Variable Name: " << mirsym->GetName()
                << ", Offset: " << keyVal.second.second << std::endl;
    }
  }
  if (dataUndefTabConst->constVec.size() != 0) {
    std::string dataUndefTabName = NameMangler::kMuidDataUndefTabPrefixStr + module->GetFileNameAsPostfix();
    dataUndefTabSym = builder->CreateGlobalDecl(dataUndefTabName, dataArrayType, kScGlobal);
    dataUndefTabSym->SetConst(dataUndefTabConst);
    dataUndefTabSym->SetStorageClass(kScFstatic);
  }
  if (dataUndefMuidTabConst->constVec.size() != 0) {
    std::string dataUndefMuidTabName = NameMangler::kMuidDataUndefMuidTabPrefixStr + module->GetFileNameAsPostfix();
    dataUndefMuidTabSym = builder->CreateGlobalDecl(dataUndefMuidTabName, dataMuidArrayType, kScGlobal);
    dataUndefMuidTabSym->SetConst(dataUndefMuidTabConst);
    dataUndefMuidTabSym->SetStorageClass(kScFstatic);
  }
}

// RangeTable stores begin and end of all MUID tables
void MUIDReplacement::GenRangeTable() {
  FieldVector parentFields;
  FieldVector fields;
  GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "tabBegin", GlobalTables::GetTypeTable().GetVoidPtr());
  GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "tabEnd", GlobalTables::GetTypeTable().GetVoidPtr());
  MIRStructType *rangeTabEntryType = static_cast<MIRStructType *>(
      GlobalTables::GetTypeTable().GetOrCreateStructType("MUIDRangeTabEntry", fields, parentFields, module));
  uint32 arraySize = 0;
  MIRArrayType *rangeArrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(rangeTabEntryType, arraySize);
  MIRAggConst *rangeTabConst = module->memPool->New<MIRAggConst>(module, rangeArrayType);
  {
    // First entry is reserved for a compile-time-stamp
    MIRAggConst *entryConst = module->memPool->New<MIRAggConst>(module, rangeTabEntryType);
    uint32 fieldID = 1;
    MUID_t mplMd5 = GetMUID(mplMuidStr);
    builder->AddIntFieldConst(rangeTabEntryType, entryConst, fieldID++, mplMd5.data.words[0]);
    builder->AddIntFieldConst(rangeTabEntryType, entryConst, fieldID++, mplMd5.data.words[1]);
    rangeTabConst->constVec.push_back(entryConst);
  }
  for (uint32_t i = RangeIdx::kVtab; i < RangeIdx::kMaxNum; i++) {
    // Use an integer to mark which entry is for which table
    MIRAggConst *entryConst = module->memPool->New<MIRAggConst>(module, rangeTabEntryType);
    uint32 fieldID = 1;
    if (i == RangeIdx::kGlobalRootlist) {
      MIRSymbol *st = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kGcRootList));
      if (!st) {
        builder->AddIntFieldConst(rangeTabEntryType, entryConst, fieldID++, 0);
        builder->AddIntFieldConst(rangeTabEntryType, entryConst, fieldID++, 0);
        rangeTabConst->constVec.push_back(entryConst);
        continue;
      }
    }
    builder->AddIntFieldConst(rangeTabEntryType, entryConst, fieldID++, i);
    builder->AddIntFieldConst(rangeTabEntryType, entryConst, fieldID++, i);
    rangeTabConst->constVec.push_back(entryConst);
  }
  // Please refer to runtime/mpl_linker.h for the layout
  std::vector<MIRSymbol *> worklist = { funcDefTabSym,       funcInfTabSym,     funcUndefTabSym,     dataDefTabSym,
                                        dataUndefTabSym,     funcDefMuidTabSym, funcUndefMuidTabSym, dataDefMuidTabSym,
                                        dataUndefMuidTabSym, funcMuidIdxTabSym,     funcProfileTabSym };
  for (MIRSymbol *mirsym : worklist) {
    MIRAggConst *entryConst = module->memPool->New<MIRAggConst>(module, rangeTabEntryType);
    uint32 fieldID = 1;
    if (mirsym) {
      builder->AddAddrofFieldConst(rangeTabEntryType, entryConst, fieldID++, mirsym);
      builder->AddAddrofFieldConst(rangeTabEntryType, entryConst, fieldID++, mirsym);
    } else {
      builder->AddIntFieldConst(rangeTabEntryType, entryConst, fieldID++, 0);
      builder->AddIntFieldConst(rangeTabEntryType, entryConst, fieldID++, 0);
    }
    rangeTabConst->constVec.push_back(entryConst);
  }

  if (rangeTabConst->constVec.size() != 0) {
    rangeArrayType->sizeArray[0] = rangeTabConst->constVec.size();
    std::string rangeTabName = NameMangler::kMuidRangeTabPrefixStr + module->GetFileNameAsPostfix();
    rangeTabSym = builder->CreateGlobalDecl(rangeTabName, rangeArrayType, kScGlobal);
    rangeTabSym->SetConst(rangeTabConst);
    rangeTabSym->SetStorageClass(kScFstatic);
  }
}

uint32_t MUIDReplacement::FindIndexFromDefTable(const MIRSymbol *mirsym, bool isFunc) {
  ASSERT(mirsym, "Invalid MIRSymbol");
  MUID_t muid = GetMUID(mirsym->GetName());
  if (isFunc) {
    CHECK_FATAL(defMuidIdxMap.find(muid) != defMuidIdxMap.end(), "Local function %s not found in funcDefMap",
           mirsym->GetName().c_str());
    return defMuidIdxMap[muid];
  } else {
    CHECK_FATAL(dataDefMap.find(muid) != dataDefMap.end(), "Local variable %s not found in dataDefMap",
           mirsym->GetName().c_str());
    return dataDefMap[muid].second;
  }
}

uint32_t MUIDReplacement::FindIndexFromUndefTable(const MIRSymbol *mirsym, bool isFunc) {
  ASSERT(mirsym, "Invalid MIRSymbol");
  MUID_t muid = GetMUID(mirsym->GetName());

  if (isFunc) {
    CHECK_FATAL(funcUndefMap.find(muid) != funcUndefMap.end(), "Extern function %s not found in funcUndefMap",
           mirsym->GetName().c_str());
    return funcUndefMap[muid].second;
  } else {
    CHECK_FATAL(dataUndefMap.find(muid) != dataUndefMap.end(), "Extern variable %s not found in dataUndefMap",
           mirsym->GetName().c_str());
    return dataUndefMap[muid].second;
  }
}

void MUIDReplacement::ReplaceFuncTable(const std::string &name) {
  MIRSymbol *tabSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(name));
  if (!tabSym) {
    return;
  }
  MIRAggConst *oldconst = dynamic_cast<MIRAggConst *>(tabSym->GetConst());
  if (!oldconst) {
    return;
  }

  bool isVtab = false;
  if (tabSym->GetName().find(VTAB_PREFIX_STR) == 0) {
    isVtab = true;
  }
  for (MIRConst *&oldTabEntry : oldconst->constVec) {
    if (oldTabEntry->kind == kConstAggConst) {
      MIRAggConst *aggrC = static_cast<MIRAggConst *>(oldTabEntry);
      for (uint32_t i = 0; i < aggrC->constVec.size(); i++) {
        ReplaceAddroffuncConst(aggrC->constVec[i], i + 1, isVtab);
      }
    } else if (oldTabEntry->kind == kConstAddrofFunc) {
      ReplaceAddroffuncConst(oldTabEntry, 0xffffffff, isVtab);
    }
  }
}

void MUIDReplacement::ReplaceAddroffuncConst(MIRConst *&entry, uint32_t fieldID, bool isVtab = false) {
  if (entry->kind != kConstAddrofFunc) {
    return;
  }

  MIRType *voidType = GlobalTables::GetTypeTable().GetVoidPtr();
  MIRAddroffuncConst *funcaddr = static_cast<MIRAddroffuncConst *>(entry);
  MIRFunction *func = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(funcaddr->GetValue());
  ASSERT(func, "Invalid MIRFunction");

  uint64_t offset = 0;
  MIRIntConst *constNode = nullptr;
  if (func->body) {
    offset = FindIndexFromDefTable(func->GetFuncSymbol(), true);
    // Left shifting is needed because in itable 0 and 1 are reserved.
    // 0 marks no entry and 1 marks a conflict.
    // The second least significant bit is set to 1, indicating
    // this is an index into the funcDefTab
    constNode = module->memPool->New<MIRIntConst>(((offset + 1) << 2) + 2, voidType);
  } else if (isVtab && func->IsAbstract()) {
    MIRType *type = GlobalTables::GetTypeTable().GetVoidPtr();
    constNode = module->memPool->New<MIRIntConst>(0, type);
  } else {
    offset = FindIndexFromUndefTable(func->GetFuncSymbol(), true);
    // The second least significant bit is set to 0, indicating
    // this is an index into the funcUndefTab
    constNode = module->memPool->New<MIRIntConst>((offset + 1) << 2, voidType);
  }
  if (fieldID != 0xffffffff) {
    constNode->fieldID = fieldID;
  }
  entry = constNode;
}

void MUIDReplacement::ReplaceDataTable(const std::string &name) {
  bool isFieldInfo = (name.find(NameMangler::kFieldsInfoPrefixStr) == 0);
  MIRSymbol *tabSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(name));
  if (!tabSym) {
    return;
  }
  MIRAggConst *oldconst = static_cast<MIRAggConst *>(tabSym->GetConst());
  if (!oldconst) {
    return;
  }

  for (MIRConst *&oldTabEntry : oldconst->constVec) {
    if (oldTabEntry->kind == kConstAggConst) {
      MIRAggConst *aggrC = static_cast<MIRAggConst *>(oldTabEntry);
      for (uint32_t i = 0; i < aggrC->constVec.size(); i++) {
        // Fow fieldinfo, we only need to replace TYPE which
        // could be an extern class
        if (!isFieldInfo || i == FIELD::kType) {
          ReplaceAddrofConst(aggrC->constVec[i]);
          aggrC->constVec[i]->fieldID = i + 1;
        }
      }
    } else if (oldTabEntry->kind == kConstAddrof) {
      ReplaceAddrofConst(oldTabEntry);
    }
  }
}

#define FROM_UNDEF_INDEX_MASK 0x4000000000000000
#define FROM_DEF_INDEX_MASK 0x2000000000000000
void MUIDReplacement::ReplaceAddrofConst(MIRConst *&entry) {
  if (entry->kind != kConstAddrof) {
    return;
  }
  MIRType *voidType = GlobalTables::GetTypeTable().GetVoidPtr();
  MIRAddrofConst *addr = static_cast<MIRAddrofConst *>(entry);
  MIRSymbol *addrSym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(addr->GetSymbolIndex().Idx());
  ASSERT(addrSym, "Invalid MIRSymbol");
  if (!addrSym->IsReflectionClassInfo() && !addrSym->IsStatic()) {
    return;
  }

  uint64_t offset = 0;
  MIRIntConst *constNode = nullptr;
  if (addrSym->GetStorageClass() != kScExtern) {
    offset = FindIndexFromDefTable(addrSym, false);
    constNode = module->memPool->New<MIRIntConst>(offset | FROM_DEF_INDEX_MASK, voidType);
  } else {
    offset = FindIndexFromUndefTable(addrSym, false);
    constNode = module->memPool->New<MIRIntConst>(offset | FROM_UNDEF_INDEX_MASK, voidType);
  }
  entry = constNode;
}

void MUIDReplacement::ReplaceDirectInvokeOrAddroffunc(MIRFunction *curFunc, StmtNode *stmt) {
  PUIdx puIdx;
  CallNode *callNode = nullptr;
  DassignNode *dassignNode = nullptr;
  RegassignNode *regassignNode = nullptr;

  if (stmt->op == OP_callassigned || stmt->op == OP_call) {
    callNode = static_cast<CallNode *>(stmt);
    puIdx = callNode->puIdx;
  } else if (stmt->op == OP_dassign) {
    dassignNode = static_cast<DassignNode *>(stmt);
    if (dassignNode->GetRhs()->op != OP_addroffunc) {
      return;
    }
    puIdx = static_cast<AddroffuncNode *>(dassignNode->GetRhs())->puIdx;
  } else if (stmt->op == OP_regassign) {
    regassignNode = static_cast<RegassignNode *>(stmt);
    if (regassignNode->uOpnd->op != OP_addroffunc) {
      return;
    }
    puIdx = static_cast<AddroffuncNode *>(regassignNode->uOpnd)->puIdx;
  } else {
    CHECK_FATAL(false, "unexpected stmt type in ReplaceDirectInvokeOrAddroffunc");
  }

  MIRFunction *calleeFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
  if (!calleeFunc || (!calleeFunc->IsJava() && calleeFunc->GetBaseClassName().empty())) {
    return;
  }

  // Add a comment to store the original function name
  std::string commentLable = NameMangler::kMarkMuidFuncUndefStr + calleeFunc->GetName();
  curFunc->body->InsertBefore(stmt, builder->CreateStmtComment(commentLable));

  // Load the function pointer
  AddrofNode *baseExpr = nullptr;
  uint32_t index = 0;
  MIRArrayType *arrayType = nullptr;
  if (calleeFunc->body) {
    // Local function is accessed through funcDefTab
    baseExpr = builder->CreateExprAddrof(0, funcDefTabSym, module->memPool);
    index = FindIndexFromDefTable(calleeFunc->GetFuncSymbol(), true);
    arrayType = static_cast<MIRArrayType *>(funcDefTabSym->GetType());
  } else {
    // External function is accessed through funcUndefTab
    baseExpr = builder->CreateExprAddrof(0, funcUndefTabSym, module->memPool);
    index = FindIndexFromUndefTable(calleeFunc->GetFuncSymbol(), true);
    arrayType = static_cast<MIRArrayType *>(funcUndefTabSym->GetType());
  }

  ConstvalNode *offsetExpr = builder->CreateIntConst(index, PTY_i64);
  MapleVector<BaseNode *> opnds(builder->GetCurrentFuncCodeMpAllocator()->Adapter());
  opnds.push_back(baseExpr);
  opnds.push_back(offsetExpr);
  ArrayNode *arrayExpr = builder->CreateExprArray(arrayType, opnds);
  arrayExpr->boundsCheck = false;
  MIRType *elemType = arrayType->GetElemType();
  BaseNode *ireadPtrExpr =
      builder->CreateExprIread(GlobalTables::GetTypeTable().GetVoidPtr(), GlobalTables::GetTypeTable().GetOrCreatePointerType(elemType), 1, arrayExpr);
  PregIdx funcPtrPreg = 0;
  MIRSymbol *funcPtrSym = nullptr;
  if (Options::usepreg) {
    funcPtrPreg = curFunc->pregTab->CreatePreg(PTY_ptr);
    RegassignNode *funcPtrPregAssign = builder->CreateStmtRegassign(PTY_ptr, funcPtrPreg, ireadPtrExpr);
    curFunc->body->InsertBefore(stmt, funcPtrPregAssign);
  } else {
    funcPtrSym = builder->GetOrCreateLocalDecl(MUID_SYM_PTR_STR, GlobalTables::GetTypeTable().GetVoidPtr());
    DassignNode *addrNode = builder->CreateStmtDassign(funcPtrSym, 0, ireadPtrExpr);
    curFunc->body->InsertBefore(stmt, addrNode);
  }

  BaseNode *readFuncPtr = nullptr;
  if (Options::usepreg) {
    readFuncPtr = builder->CreateExprRegread(PTY_ptr, funcPtrPreg);
  } else {
    readFuncPtr = builder->CreateExprDread(funcPtrSym);
  }

  if (callNode) {
    // Create icallNode to replace callNode
    IcallNode *icallNode = (callNode->op == OP_call)
                               ? module->CurFuncCodeMemPool()->New<IcallNode>(module, OP_icall)
                               : module->CurFuncCodeMemPool()->New<IcallNode>(module, OP_icallassigned);
    icallNode->numOpnds = callNode->numOpnds + 1;
    icallNode->nOpnd.resize(icallNode->numOpnds);
    icallNode->nOpnd[0] = readFuncPtr;
    for (uint32 i = 1; i < icallNode->nOpnd.size(); i++) {
      icallNode->nOpnd[i] = callNode->nOpnd[i - 1]->CloneTree(module);
    }
    icallNode->retTyIdx = calleeFunc->GetReturnTyIdx();
    if (callNode->op == OP_callassigned) {
      static_cast<IcallNode *>(icallNode)->returnValues = static_cast<CallNode *>(callNode)->returnValues;
    }
    curFunc->body->ReplaceStmt1WithStmt2(callNode, icallNode);
  } else if (dassignNode) {
    dassignNode->SetRhs(readFuncPtr);
  } else if (regassignNode) {
    regassignNode->uOpnd = readFuncPtr;
  }
}

void MUIDReplacement::ReplaceDassign(MIRFunction *curFunc, DassignNode *dassignNode) {
  MIRSymbol *mirsym = curFunc->GetLocalOrGlobalSymbol(dassignNode->stIdx);
  if (!mirsym->IsStatic()) {
    return;
  }

  // Add a comment to store the original symbol name
  std::string cmt = "Assign to: " + mirsym->GetName();
  curFunc->body->InsertBefore(dassignNode, builder->CreateStmtComment(cmt));
  // Load the symbol pointer
  AddrofNode *baseExpr = nullptr;
  uint32_t index = 0;
  MIRArrayType *arrayType = nullptr;
  if (mirsym->GetStorageClass() != kScExtern) {
    // Local static member is accessed through dataDefTab
    baseExpr = builder->CreateExprAddrof(0, dataDefTabSym);
    index = FindIndexFromDefTable(mirsym, false);
    arrayType = static_cast<MIRArrayType *>(dataDefTabSym->GetType());
  } else {
    // External static member is accessed through dataUndefTab
    baseExpr = builder->CreateExprAddrof(0, dataUndefTabSym);
    index = FindIndexFromUndefTable(mirsym, false);
    arrayType = static_cast<MIRArrayType *>(dataUndefTabSym->GetType());
  }
  ConstvalNode *offsetExpr = builder->CreateIntConst(index, PTY_i64);
  MapleVector<BaseNode *> opnds(builder->GetCurrentFuncCodeMpAllocator()->Adapter());
  opnds.push_back(baseExpr);
  opnds.push_back(offsetExpr);
  ArrayNode *arrayExpr = builder->CreateExprArray(arrayType, opnds);
  arrayExpr->boundsCheck = false;
  MIRType *elemType = arrayType->GetElemType();
  BaseNode *ireadPtrExpr =
      builder->CreateExprIread(GlobalTables::GetTypeTable().GetVoidPtr(), GlobalTables::GetTypeTable().GetOrCreatePointerType(elemType), 1, arrayExpr);

  PregIdx symPtrPreg = 0;
  MIRSymbol *symPtrSym = nullptr;
  if (Options::usepreg) {
    symPtrPreg = curFunc->pregTab->CreatePreg(PTY_ptr);
    RegassignNode *symPtrPregAssign = builder->CreateStmtRegassign(PTY_ptr, symPtrPreg, ireadPtrExpr);
    curFunc->body->InsertBefore(dassignNode, symPtrPregAssign);
  } else {
    MIRType *mVoidPtr = GlobalTables::GetTypeTable().GetVoidPtr();
    CHECK_FATAL(mVoidPtr != nullptr, "null ptr check");
    symPtrSym = builder->GetOrCreateLocalDecl(MUID_FUNC_PTR_STR, mVoidPtr);
    DassignNode *addrNode = builder->CreateStmtDassign(symPtrSym, 0, ireadPtrExpr);
    curFunc->body->InsertBefore(dassignNode, addrNode);
  }

  // Replace dassignNode with iassignNode
  BaseNode *destExpr = nullptr;
  if (Options::usepreg) {
    destExpr = builder->CreateExprRegread(PTY_ptr, symPtrPreg);
  } else {
    destExpr = builder->CreateExprDread(symPtrSym);
  }
  MIRType *destPtrType = GlobalTables::GetTypeTable().GetOrCreatePointerType(mirsym->GetType());
  StmtNode *iassignNode = builder->CreateStmtIassign(destPtrType, 0, destExpr, dassignNode->Opnd(0));
  curFunc->body->ReplaceStmt1WithStmt2(dassignNode, iassignNode);
}

void MUIDReplacement::CollectFieldCallSite() {
  for (MIRFunction *mirFunc : module->functionList) {
    if (!mirFunc->body) {
      continue;
    }
    SetCurrentFunction(mirFunc);
    StmtNode *stmt = mirFunc->body->GetFirst();
    StmtNode *next = nullptr;
    while (stmt) {
      next = stmt->GetNext();
      CollectDreadStmt(mirFunc, stmt);
      stmt = next;
    }
  }
}

void MUIDReplacement::CollectDreadStmt(MIRFunction *curFunc, StmtNode *stmt) {
  if (!curFunc || !stmt) {
    return;
  }
  switch (stmt->op) {
    case OP_if: {
      IfStmtNode *inode = static_cast<IfStmtNode *>(stmt);
      inode->SetOpnd(CollectDreadExpr(curFunc, stmt, inode->Opnd(0)), 0);
      CollectDreadStmt(curFunc, inode->thenPart);
      CollectDreadStmt(curFunc, inode->elsePart);
      break;
    }
    case OP_while: {
      WhileStmtNode *wnode = static_cast<WhileStmtNode *>(stmt);
      wnode->SetOpnd(CollectDreadExpr(curFunc, stmt, wnode->Opnd(0)), 0);
      CollectDreadStmt(curFunc, wnode->body);
      break;
    }
    case OP_block: {
      BlockNode *bnode = static_cast<BlockNode *>(stmt);
      for (StmtNode *s = bnode->GetFirst(); s; s = s->GetNext()) {
        CollectDreadStmt(curFunc, s);
      }
      break;
    }
    default:
      for (int i = 0; i < stmt->NumOpnds(); i++) {
        stmt->SetOpnd(CollectDreadExpr(curFunc, stmt, stmt->Opnd(i)), i);
      }
  }
}

void MUIDReplacement::ReplaceDreadStmt(MIRFunction *curFunc, StmtNode *stmt) {
  if (!curFunc || !stmt) {
    return;
  }

  switch (stmt->op) {
    case OP_if: {
      IfStmtNode *inode = static_cast<IfStmtNode *>(stmt);
      inode->SetOpnd(ReplaceDreadExpr(curFunc, stmt, inode->Opnd(0)), 0);
      ReplaceDreadStmt(curFunc, inode->thenPart);
      ReplaceDreadStmt(curFunc, inode->elsePart);
      break;
    }
    case OP_while: {
      WhileStmtNode *wnode = static_cast<WhileStmtNode *>(stmt);
      wnode->SetOpnd(ReplaceDreadExpr(curFunc, stmt, wnode->Opnd(0)), 0);
      ReplaceDreadStmt(curFunc, wnode->body);
      break;
    }
    case OP_block: {
      BlockNode *bnode = static_cast<BlockNode *>(stmt);
      for (StmtNode *s = bnode->GetFirst(); s; s = s->GetNext()) {
        ReplaceDreadStmt(curFunc, s);
      }
      break;
    }
    default: {
      for (int i = 0; i < stmt->NumOpnds(); i++) {
        stmt->SetOpnd(ReplaceDreadExpr(curFunc, stmt, stmt->Opnd(i)), i);
      }
      break;
    }
  }
}

BaseNode *MUIDReplacement::CollectDreadExpr(MIRFunction *curFunc, StmtNode *stmt, BaseNode *expr) {
  if (!curFunc || !stmt || !expr) {
    return nullptr;
  }

  if (expr->op == OP_intrinsicop) {
    IntrinsicopNode *intrinsicsNode = static_cast<IntrinsicopNode *>(expr);
    if (intrinsicsNode->intrinsic == INTRN_MPL_READ_OVTABLE_ENTRY) {
      std::string valueSymName = NameMangler::kOffsetTabStr + module->GetFileNameAsPostfix();
      MIRSymbol *offsetTableSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(valueSymName));
      MIRArrayType *arrayType = static_cast<MIRArrayType *>(offsetTableSym->GetType());
      MIRType *elemType = arrayType->GetElemType();
      const int intrinsicsNodeNopndSize = intrinsicsNode->nOpnd.size();
      CHECK_FATAL(intrinsicsNodeNopndSize > 0, "null ptr check");
      return builder->CreateExprIread(GlobalTables::GetTypeTable().GetUInt32(), GlobalTables::GetTypeTable().GetOrCreatePointerType(elemType), 1,
                                       intrinsicsNode->nOpnd[0]);
    }
  }
  for (int i = 0; i < expr->NumOpnds(); i++) {
    expr->SetOpnd(CollectDreadExpr(curFunc, stmt, expr->Opnd(i)), i);
  }
  return expr;
}

// Turn dread into iread
BaseNode *MUIDReplacement::ReplaceDreadExpr(MIRFunction *curFunc, StmtNode *stmt, BaseNode *expr) {
  if (!curFunc || !stmt || !expr) {
    return nullptr;
  }

  int i = 0;
  UnaryNode *uOpnd = nullptr;
  BinaryNode *bopnds = nullptr;
  TernaryNode *topnds = nullptr;

  switch (expr->op) {
    case OP_dread:
    case OP_addrof: {
      return ReplaceDread(curFunc, stmt, expr);
    }
    case OP_select: {
      topnds = static_cast<TernaryNode *>(expr);
      for (i = 0; i < topnds->NumOpnds(); i++) {
        topnds->SetOpnd(ReplaceDreadExpr(curFunc, stmt, topnds->topnd[i]), i);
      }
      break;
    }
    default: {
      if (expr->IsUnaryNode()) {
        uOpnd = static_cast<UnaryNode *>(expr);
        uOpnd->SetOpnd(ReplaceDreadExpr(curFunc, stmt, uOpnd->uOpnd), i);
      } else if (expr->IsBinaryNode()) {
        bopnds = static_cast<BinaryNode *>(expr);
        for (i = 0; i < bopnds->NumOpnds(); i++) {
          bopnds->SetOpnd(ReplaceDreadExpr(curFunc, stmt, bopnds->bOpnd[i]), i);
        }
      } else {
        for (i = 0; i < expr->NumOpnds(); i++) {
          expr->SetOpnd(ReplaceDreadExpr(curFunc, stmt, expr->Opnd(i)), i);
        }
        break;
      }
    }
  }
  return expr;
}

BaseNode *MUIDReplacement::ReplaceDread(MIRFunction *curFunc, StmtNode *stmt, BaseNode *opnd) {
  if (!opnd || (opnd->op != OP_dread && opnd->op != OP_addrof)) {
    return opnd;
  }

  DreadNode *dreadNode = static_cast<DreadNode *>(opnd);
  MIRSymbol *mirsym = curFunc->GetLocalOrGlobalSymbol(dreadNode->stIdx);
  if (!mirsym->IsStatic()) {
    return opnd;
  }

  // Add a comment to store the original symbol name
  std::string cmt = "Read from: " + mirsym->GetName();
  curFunc->body->InsertBefore(stmt, builder->CreateStmtComment(cmt));
  // Load the symbol pointer
  AddrofNode *baseExpr = nullptr;
  uint32_t index = 0;
  MIRArrayType *arrayType = nullptr;
  if (mirsym->GetStorageClass() != kScExtern) {
    // Local static member is accessed through dataDefTab
    baseExpr = builder->CreateExprAddrof(0, dataDefTabSym);
    index = FindIndexFromDefTable(mirsym, false);
    arrayType = static_cast<MIRArrayType *>(dataDefTabSym->GetType());
  } else {
    // External static member is accessed through dataUndefTab
    baseExpr = builder->CreateExprAddrof(0, dataUndefTabSym);
    index = FindIndexFromUndefTable(mirsym, false);
    arrayType = static_cast<MIRArrayType *>(dataUndefTabSym->GetType());
  }
  ConstvalNode *offsetExpr = builder->CreateIntConst(index, PTY_i64);
  MapleVector<BaseNode *> opnds(builder->GetCurrentFuncCodeMpAllocator()->Adapter());
  opnds.push_back(baseExpr);
  opnds.push_back(offsetExpr);
  ArrayNode *arrayExpr = builder->CreateExprArray(arrayType, opnds);
  arrayExpr->boundsCheck = false;
  MIRType *elemType = arrayType->GetElemType();
  BaseNode *ireadPtrExpr =
      builder->CreateExprIread(GlobalTables::GetTypeTable().GetVoidPtr(), GlobalTables::GetTypeTable().GetOrCreatePointerType(elemType), 1, arrayExpr);
  if (opnd->op == OP_addrof) {
    return ireadPtrExpr;
  }
  MIRType *destType = mirsym->GetType();
  MIRType *destPtrType = GlobalTables::GetTypeTable().GetOrCreatePointerType(destType);
  return builder->CreateExprIread(destType, destPtrType, 0, ireadPtrExpr);
}

void MUIDReplacement::ProcessFunc(MIRFunction *func) {
  // Libcore-all module is self-contained, so no need to do all these replacement
  if (isLibcore || func->IsEmpty()) {
    return;
  }
  SetCurrentFunction(func);
  StmtNode *stmt = func->body->GetFirst();
  StmtNode *next = nullptr;
  while (stmt) {
    next = stmt->GetNext();
    ReplaceDreadStmt(func, stmt);
    // Replace direct func invoke
    if (stmt->op == OP_callassigned || stmt->op == OP_call) {
      ReplaceDirectInvokeOrAddroffunc(func, stmt);
    } else if (stmt->op == OP_dassign) {
      ReplaceDirectInvokeOrAddroffunc(func, stmt);
      ReplaceDassign(func, static_cast<DassignNode *>(stmt));
    } else if (stmt->op == OP_regassign) {
      ReplaceDirectInvokeOrAddroffunc(func, stmt);
    }
    stmt = next;
  }
}

void MUIDReplacement::GenGlobalRootList() {
  uint32 arraySize;
  arraySize = 0;
  MIRType *type = GlobalTables::GetTypeTable().GetVoidPtr();
  MIRArrayType *arraytype = GlobalTables::GetTypeTable().GetOrCreateArrayType(type, arraySize);
  MIRAggConst *newconst = module->memPool->New<MIRAggConst>(module, arraytype);

  for (StIdx stIdx : module->symbolSet) {
    MIRSymbol *symbol = GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx());
    MIRSymKind st = symbol->sKind;
    MIRStorageClass sc = symbol->storageClass;

    if (!(st == kStVar && sc == kScGlobal)) {
      continue;
    }
    TyIdx tyIdx = symbol->GetTyIdx();
    MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
    PrimType pty = type->GetPrimType();
    if (!(pty == PTY_ptr || pty == PTY_ref)) {
      continue;
    }
    // It is a pointer/ref type.  Check its pointed type.
    MIRPtrType *pointType = dynamic_cast<MIRPtrType *>(type);
    if (pty == PTY_ptr) {
      if (pointType == nullptr) {
        continue;
      }
      MIRType *pointedType = pointType->GetPointedType();
      if (!(pointedType->GetKind() == kTypeClass)) {
        continue;
      }
    }
    // Now it is a pointer/ref to a class.  Record it for GC scanning.
    MIRType *ptrtype = GlobalTables::GetTypeTable().typeTable[PTY_ptr];
    MIRConst *constnode = module->memPool->New<MIRAddrofConst>(symbol->GetStIdx(), 0, ptrtype);
    newconst->constVec.push_back(constnode);
  }
  std::string gcRootsName = NameMangler::kGcRootList;
  if (newconst->constVec.size() != 0) {
    MIRSymbol *gcRootsSt = builder->CreateGlobalDecl(gcRootsName, newconst->type, kScGlobal);
    arraytype->sizeArray[0] = newconst->constVec.size();
    gcRootsSt->SetConst(newconst);
  }
}

void MUIDReplacement::GenTables() {
  GenGlobalRootList();
  if (Options::buildApp) {
    CollectFieldCallSite();
  }
  CollectFuncAndData();
  GenFuncDefTable();
  GenDataDefTable();
  GenUnifiedUndefTable();
  GenRangeTable();

  // When MapleLinker is enabled, MUIDReplacement becomes the last
  // phase that updates the reflection string table, thus the table
  // is emitted here.
  ReflectionAnalysis::GenStrTab(module);
  // Replace undef entries in vtab/itab/reflectionMetaData
  for (Klass *klass : klassHierarchy->GetTopoSortedKlasses()) {
    ReplaceFuncTable(VTAB_PREFIX_STR + klass->GetKlassName());
    ReplaceFuncTable(ITAB_PREFIX_STR + klass->GetKlassName());
    ReplaceFuncTable(ITAB_CONFLICT_PREFIX_STR + klass->GetKlassName());
    ReplaceDataTable(SUPERCLASSINFO_PREFIX_STR + klass->GetKlassName());
    ReplaceDataTable(NameMangler::kFieldsInfoPrefixStr + klass->GetKlassName());
  }
  // ReplaceGlobalRootList(GCROOTLIST);
  ReplaceDataTable(NameMangler::kGcRootList);

  if (Options::buildApp) {
    // ReplaceVtableOffsetTable
    ReplaceDataTable(NameMangler::kVtabOffsetTabStr + module->GetFileNameAsPostfix());

    // ReplaceFieldOffsetTable
    ReplaceDataTable(NameMangler::kFieldOffsetTabStr + module->GetFileNameAsPostfix());
  }
}

}  // namespace maple
