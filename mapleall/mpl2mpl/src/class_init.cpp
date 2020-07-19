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
#include "class_init.h"
#include "name_mangler.h"

// This phase does two things.
// 1. insert clinit(class initialization) check, a intrinsic call INTRN_MPL_CLINIT_CHECK
//   for place where needed.
//   Insert clinit check for static native methods which are not private.
// 2. lower JAVA_CLINIT_CHECK to MPL_CLINIT_CHECK.
//   Before insert or tranform the clinit check, we used a optimise based on
//   white list.When the javaname is core-all and the class is in the white list
//   we dont't insert clinit check.Because the class in the white list is intialized
//   in the system bootup.
namespace maple {
ClassInit::ClassInit(MIRModule *mod, KlassHierarchy *kh, bool dump) :
    FuncOptimizeImpl(mod, kh, dump) {
  if (Options::usePreloadedClass) {
    BuildPreloadedClass();
  }
}

void ClassInit::BuildPreloadedClass() {
  preloadedClass = {
#define CLASS_PREFIX(classname) classname,
#include "global_symbols.def"
#undef CLASS_PREFIX
  };
  if (trace) {
    LogInfo::MapleLogger() << "ClassInit read in preloaded class set with " << preloadedClass.size() << " classes" << std::endl;
  }
}

bool ClassInit::CanRemoveClinitCheck(const std::string &clinitclassname) {
  if (!Options::usePreloadedClass) {
    return false;
  }
  if (clinitclassname.empty()) {
    return false;
  }
  if (clinitclassname == std::string(NameMangler::kClassInfoPrefix) + NameMangler::kJavaLangStringStr) {
    return true;
  }

  uint32 javaNameIdx = module->GetFileinfo(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("INFO_filename"));
  const std::string &javaName = GlobalTables::GetStrTable().GetStringFromStrIdx(GStrIdx(javaNameIdx));
  if (javaName.find("core-all") != std::string::npos) {
    return false;
  }

  return (preloadedClass.find(clinitclassname) != preloadedClass.end());
}

#undef CLINIT_CHECK
void ClassInit::GenClassInitCheckProfile(MIRFunction *func, MIRSymbol *classinfo, StmtNode *clinit) const {
#ifdef CLINIT_CHECK
  GenPreClassInitCheck(func, classinfo, clinit);
  GenPostClassInitCheck(func, classinfo, clinit);
#endif  // CLINIT_CHECK
}

void ClassInit::GenPreClassInitCheck(MIRFunction *func, MIRSymbol *classinfo, StmtNode *clinit) {
  MIRFunction *preclinit = builder->GetOrCreateFunction("MCC_PreClinitCheck", (TyIdx)(PTY_void));
  BaseNode *classinfoNode = builder->CreateExprAddrof(0, classinfo);
  MapleVector<BaseNode *> callargs(currFunc->codeMemPoolAllocator.Adapter());
  callargs.push_back(classinfoNode);
  CallNode *callPreclinit = builder->CreateStmtCall(preclinit->puIdx, callargs);
  func->body->InsertBefore(clinit, callPreclinit);
}

void ClassInit::GenPostClassInitCheck(MIRFunction *func, MIRSymbol *classinfo, StmtNode *clinit) {
  MIRFunction *postclinit = builder->GetOrCreateFunction("MCC_PostClinitCheck", (TyIdx)(PTY_void));
  BaseNode *classinfoNode = builder->CreateExprAddrof(0, classinfo);
  MapleVector<BaseNode *> callargs(currFunc->codeMemPoolAllocator.Adapter());
  callargs.push_back(classinfoNode);
  CallNode *callPostclinit = builder->CreateStmtCall(postclinit->puIdx, callargs);
  func->body->InsertAfter(clinit, callPostclinit);
}

void ClassInit::ProcessFunc(MIRFunction *func) {
  // no field will be involved in critical native funcs.
  if (func->IsEmpty() || func->GetAttr(FUNCATTR_critical_native)) {
    return;
  }

  currFunc = func;
  builder->SetCurrentFunction(func);

  // insert clinit check for static methods
  MIRType *selfclassType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->classTyIdx);
  std::string selfclassname;
  if (selfclassType) {
    selfclassname = GlobalTables::GetStrTable().GetStringFromStrIdx(selfclassType->nameStrIdx);
  } else {
    const std::string &funcname = func->GetName();
    size_t pos = funcname.find(NameMangler::kNameSplitterStr);
    while (pos != std::string::npos && (funcname[pos - 1] == '_' && funcname[pos - 2] != '_')) {
      pos = funcname.find(NameMangler::kNameSplitterStr, pos + 3);
    }
    selfclassname = funcname.substr(0, pos);
  }

  // insert clinit check for static native methods which are not private.
  // We have to do this here because native methods are generated as empty by java2mpl,
  // If we simply insert clinit-check (which does not have return value), there will
  // be not return statement for native methods which do hava a return value.
  // clinit check for static java (non-native) methods which are not private is
  // already inserted by java2mpl.
  if (func->IsStatic() && !func->IsPrivate() && !func->IsClinit() && func->IsNative()) {
    MIRType *classType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->classTyIdx);
    CHECK_FATAL(classType != nullptr, "class type is nullptr");
    if (classType) {
      const std::string &classname = GlobalTables::GetStrTable().GetStringFromStrIdx(classType->nameStrIdx);
      if (!CanRemoveClinitCheck(classname)) {
        Klass *klass = klassHierarchy->GetKlassFromName(classname);
        CHECK_FATAL(klass != nullptr, "klass is nullptr in ClassInit::ProcessFunc");
        if (klass->GetClinit() && func != klass->GetClinit()) {
          MIRSymbol *classinfo = GetClassInfo(classname);
          BaseNode *classinfoNode = builder->CreateExprAddrof(0, classinfo);
          if (trace) {
            LogInfo::MapleLogger() << "\t- low-cost clinit - insert check in static method " << func->GetName() << "clasname "
                      << classname << std::endl;
          }
          //StmtNode *intrinsiccall = builder->xCreateStmtIntrinsicCall1(INTRN_MPL_CLINIT_CHECK, classinfoNode);
          MapleVector<BaseNode*> ops(builder->mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
          ops.push_back(classinfoNode);
          StmtNode *intrinsiccall = builder->CreateStmtIntrinsicCall(INTRN_MPL_CLINIT_CHECK, ops);
          func->body->InsertFirst(intrinsiccall);
          GenClassInitCheckProfile(func, classinfo, intrinsiccall);
        }
      }
    }
  }
  // lower JAVA_CLINIT_CHECK to MPL_CLINIT_CHECK
  StmtNode *stmt = func->body->GetFirst();
  while (stmt) {
    if (stmt->op == OP_intrinsiccallwithtype) {
      IntrinsiccallNode *intrinsiccall = static_cast<IntrinsiccallNode *>(stmt);
      if (intrinsiccall->intrinsic == INTRN_JAVA_CLINIT_CHECK) {
        // intrinsiccallwithtype <$LTest_3B> JAVA_CLINIT_CHECK ()        -->
        // intrinsiccall MPL_CLINIT_CHECK (addrof ptr $__cinf_LTest_3B)
        CHECK_FATAL(intrinsiccall->nOpnd.size() == 0, "wrong arg vectors");
        MIRType *classtype = GlobalTables::GetTypeTable().typeTable[intrinsiccall->tyIdx.GetIdx()];
        CHECK_FATAL (classtype && classtype->nameStrIdx != 0,
                "symbol name is null for type index %d", intrinsiccall->tyIdx.GetIdx());

        const std::string &classname = GlobalTables::GetStrTable().GetStringFromStrIdx(classtype->nameStrIdx);
        Klass *klass = klassHierarchy->GetKlassFromName(classname);
        CHECK_FATAL(klass, "klass is null in ClassInit::ProcessFunc");
        bool doClinitCheck = !CanRemoveClinitCheck(classname) &&
                             (func->IsStatic() || (!func->IsStatic() && classname != selfclassname)) &&
                             klassHierarchy->NeedClinitCheckRecursively(klass);

        if (doClinitCheck) {
          MIRSymbol *classinfo = GetClassInfo(classname);
          AddrofNode *classinfoNode = builder->CreateExprAddrof(0, classinfo);
          //StmtNode *intrinsiccall = builder->xCreateStmtIntrinsicCall1(INTRN_MPL_CLINIT_CHECK, classinfoNode);
          MapleVector<BaseNode*> ops(builder->mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
          ops.push_back(classinfoNode);
          StmtNode *intrinsiccall = builder->CreateStmtIntrinsicCall(INTRN_MPL_CLINIT_CHECK, ops);
          func->body->ReplaceStmt1WithStmt2(stmt, intrinsiccall);
          if (trace) {
            LogInfo::MapleLogger() << "\t- low-cost clinit - lower JAVA_CLINIT_CHECK " << classname << " in " << func->GetName()
                      << "()" << std::endl;
          }
          GenClassInitCheckProfile(func, classinfo, intrinsiccall);
        } else {
          func->body->RemoveStmt(stmt);
        }
      }
    }

    stmt = stmt->GetNext();
  }
}

MIRSymbol *ClassInit::GetClassInfo(const std::string &classname) {
  const std::string &classinfoname = CLASSINFO_PREFIX_STR + classname;
  MIRType *classinfoType = GlobalTables::GetTypeTable().GetClassType(NameMangler::kClassMetadataTypeName, module);
  MIRSymbol *classInfo = builder->GetOrCreateGlobalDecl(classinfoname, classinfoType);
  Klass *klass = klassHierarchy->GetKlassFromName(classname);
  if (!klass || !klass->GetMIRStructType()->IsLocal()) {
    classInfo->SetStorageClass(kScExtern);
  }
  return classInfo;
}

}
