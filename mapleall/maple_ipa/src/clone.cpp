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

#include "clone.h"
#include <iostream>
#include <algorithm>
#include <mir_symbol.h>

using namespace std;

// For some funcs, when we can ignore their return-values, we clone a new func of
// them without return-values. We configure a list to save these funcs and clone
// at the very beginning so that clones can also enjoy the optimizations after.
// This mainly contains the clone of funcbody(include labels, symbols, arguments,
// etc.) and the update of the new func infomation.
namespace maple {

ReplaceRetIgnored::ReplaceRetIgnored(maple::MemPool *mp) : allocator(mp) {}

bool ReplaceRetIgnored::ShouldReplaceWithVoidFunc(const CallMeStmt *stmt, const MIRFunction *calleeFunc) const {
  Opcode op = stmt->op;
  size_t nretSize = stmt->mustDefList.size();
  MIRType *returnType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(calleeFunc->GetReturnTyIdx());
  return nretSize == 0 && (op == OP_virtualcallassigned || op == OP_callassigned || op == OP_superclasscallassigned) &&
         !calleeFunc->IsNative() && returnType->GetKind() == kTypePointer && returnType->GetPrimType() == PTY_ref;
}

std::string ReplaceRetIgnored::GenerateNewBaseName(const MIRFunction *originalFunc) {
  return string(originalFunc->GetBaseFuncName()).append(kVoidRetSuffix);
}

std::string ReplaceRetIgnored::GenerateNewFullName(MIRFunction *originalFunc) {
  string oldSignature = originalFunc->GetSignature();
  auto retPos = oldSignature.find("_29");
  return string(originalFunc->GetBaseClassName())
    .append(NameMangler::kNameSplitterStr)
    .append(GenerateNewBaseName(originalFunc))
    .append(NameMangler::kNameSplitterStr)
    .append(oldSignature.substr(0, retPos + 3))
    .append("V");
}

MIRSymbol *Clone::CloneLocalSymbol(const MIRSymbol *oldSym, MIRFunction *newFunc) {
  MemPool *newMP = newFunc->dataMemPool;
  MIRSymbol *newSym = newMP->New<MIRSymbol>(*oldSym);
  if (oldSym->sKind == kStConst) {
    newSym->value.konst = newMP->New<MIRConst>(*oldSym->value.konst);
  } else if (oldSym->sKind == kStPreg) {
    newSym->value.preg = newMP->New<MIRPreg>(*oldSym->value.preg);
  } else if (oldSym->sKind == kStFunc) {
    CHECK_FATAL(false, "%s has unexpected local func symbol", oldSym->GetName().c_str());
  }
  return newSym;
}

void Clone::CloneSymbols(MIRFunction *newfunc, const MIRFunction *oldfunc) {
  uint32 symtabsize = oldfunc->symTab->GetSymbolTableSize();
  for (uint32 i = oldfunc->formalDefVec.size() + 1; i < symtabsize; i++) {
    MIRSymbol *sym = oldfunc->symTab->GetSymbolFromStIdx(i);
    if (!sym) {
      continue;
    }
    MIRSymbol *newSym = CloneLocalSymbol(sym, newfunc);
    if (!newfunc->symTab->AddStOutside(newSym)) {
      CHECK_FATAL(false, "%s already existed in func %s", sym->GetName().c_str(), newfunc->GetName().c_str());
    }
  }
}

void Clone::CloneLabels(MIRFunction *newfunc, const MIRFunction *oldfunc) {
  uint32 labeltabsize = oldfunc->labelTab->GetLabelTableSize();
  for (uint32 i = 1; i < labeltabsize; i++) {
    std::string labelName = oldfunc->labelTab->GetName(i);
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(labelName);
    newfunc->labelTab->AddLabel(strIdx);
  }
}

// Clone a function
MIRFunction *Clone::CloneFunction(MIRFunction *const originalFunction, const std::string &newBaseFuncName,
                                  MIRType *returnType) {
  MapleAllocator cgalloc(originalFunction->codeMemPool);
  ArgVector argument(cgalloc.Adapter());
  CloneArgument(originalFunction, argument);
  MIRType *retType = returnType;
  if (retType == nullptr) {
    retType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(originalFunction->GetReturnTyIdx());
  }
  std::string fullName = originalFunction->GetBaseClassName();
  string signature = originalFunction->GetSignature();

  fullName = fullName.append(NameMangler::kNameSplitterStr).append(newBaseFuncName).append(NameMangler::kNameSplitterStr).append(signature);

  MIRFunction *newFunc =
    mirBuilder.CreateFunction(fullName, retType, argument, false, originalFunction->body != nullptr);

  CHECK_FATAL(newFunc != nullptr, "create cloned function failed");
  mirBuilder.mirModule->AddFunction(newFunc);
  Klass *klass = kh->GetKlassFromName(originalFunction->GetBaseClassName());
  CHECK_FATAL(klass != nullptr, "getklass failed");
  klass->AddMethod(newFunc);
  newFunc->classTyIdx = originalFunction->classTyIdx;

  MIRClassType *classType = klass->GetMIRClassType();
  classType->methods.push_back(
    MethodPair(newFunc->stIdx, TyidxFuncAttrPair(newFunc->GetFuncSymbol()->GetTyIdx(), originalFunction->funcAttrs)));
  newFunc->flag = originalFunction->flag;
  newFunc->srcPosition = originalFunction->srcPosition;
  newFunc->SetAttrs(originalFunction->GetAttrs());
  newFunc->SetBaseClassFuncNames(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(fullName));
  newFunc->easummary = originalFunction->easummary;
  if (originalFunction->body) {
    CopyFuncInfo(originalFunction, newFunc);
    MIRFunction *originalCurrFunction = mirBuilder.GetCurrentFunction();

    mirBuilder.SetCurrentFunction(newFunc);
    newFunc->body = originalFunction->body->CloneTree(originalFunction->module);
    CloneSymbols(newFunc, originalFunction);
    CloneLabels(newFunc, originalFunction);

    mirBuilder.SetCurrentFunction(originalCurrFunction);
  }
  return newFunc;
}

void Clone::CloneArgument(const MIRFunction *originalFunction, ArgVector &argument) const {
  for (uint32 i = 0; i < originalFunction->formalDefVec.size(); i++) {
    argument.push_back(ArgPair(GlobalTables::GetStrTable().GetStringFromStrIdx(originalFunction->formalDefVec[i].formalStrIdx),
                               GlobalTables::GetTypeTable().GetTypeFromTyIdx(originalFunction->formalDefVec[i].formalTyIdx)));
  }
}

void Clone::CopyFuncInfo(const MIRFunction *originalFunction, MIRFunction *newFunc) const {
  auto funcnameIdx = newFunc->GetBaseFuncNameStridx();
  auto fullnameIdx = newFunc->GetNameStridx();
  auto classnameIdx = newFunc->GetBaseClassNameStridx();
  auto metaFullnameIdx = mirBuilder.GetOrCreateStringIndex(FULLNAME_STR);
  auto metaClassNameIdx = mirBuilder.GetOrCreateStringIndex(CLASSNAME_STR);
  auto metaFuncnameIdx = mirBuilder.GetOrCreateStringIndex(FUNCNAME_STR);

  const MIRInfoVector &fnInfo = originalFunction->info;
  const MapleVector<bool> &kFnInfoIsstring = originalFunction->infoIsString;
  uint32 size = fnInfo.size();
  for (uint32 i = 0; i < size; i++) {
    if (fnInfo[i].first == metaFullnameIdx) {
      newFunc->info.push_back(pair<GStrIdx, uint32>(fnInfo[i].first, fullnameIdx.GetIdx()));
    } else if (fnInfo[i].first == metaFuncnameIdx) {
      newFunc->info.push_back(pair<GStrIdx, uint32>(fnInfo[i].first, funcnameIdx.GetIdx()));
    } else if (fnInfo[i].first == metaClassNameIdx) {
      newFunc->info.push_back(pair<GStrIdx, uint32>(fnInfo[i].first, classnameIdx.GetIdx()));
    } else {
      newFunc->info.push_back(pair<GStrIdx, uint32>(fnInfo[i].first, fnInfo[i].second));
    }
    newFunc->infoIsString.push_back(kFnInfoIsstring[i]);
  }
}

void Clone::UpdateFuncInfo(MIRFunction *newFunc) {
  auto fullnameIdx = newFunc->GetNameStridx();
  auto metaFullnameIdx = mirBuilder.GetOrCreateStringIndex(FULLNAME_STR);
  uint32 size = newFunc->info.size();
  for (uint32 i = 0; i < size; i++) {
    if (newFunc->info[i].first == metaFullnameIdx) {
      newFunc->info[i].second = fullnameIdx.GetIdx();
      break;
    }
  }
}

/**
 * Clone all functions that would be invoked with their return value ignored
 * @param original_function The original function to be cloned
 * @param mirBuilder A helper object
 * @return Pointer to the newly cloned function
 */
MIRFunction *Clone::CloneFunctionNoReturn(MIRFunction *const originalFunction) {
  string oldSignature = originalFunction->GetSignature();

  const string kNewMethodBaseName = replaceRetIgnored->GenerateNewBaseName(originalFunction);

  MIRFunction *originalCurrFunction = mirBuilder.mirModule->CurFunction();
  MIRFunction *newFunction = CloneFunction(originalFunction, kNewMethodBaseName, GlobalTables::GetTypeTable().typeTable.at(1));
  /**
   * new stmt should be located in the newFunction->codemp, mirBuilder.CreateStmtReturn will use CurFunction().codemp
   * to assign space for the new stmt. So we set it correctly here.
   */
  mirBuilder.mirModule->SetCurFunction(newFunction);
  if (originalFunction->body) {
    for (StmtNode *stmt = newFunction->body->GetFirst(); stmt; stmt = stmt->GetNext()) {
      if (stmt->op == OP_return) {
        newFunction->body->ReplaceStmt1WithStmt2(stmt, mirBuilder.CreateStmtReturn(nullptr));
      }
    }
  }

  // setup new names for the newly cloned function
  string newFuncFullName = replaceRetIgnored->GenerateNewFullName(originalFunction);
  auto fullNameStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(newFuncFullName);
  newFunction->OverrideBaseClassFuncNames(fullNameStrIdx);
  MIRSymbol *funcst = newFunction->GetFuncSymbol();
  GlobalTables::GetGsymTable().RemoveFromStringSymbolMap(funcst);

  funcst->SetNameStridx(fullNameStrIdx);
  GlobalTables::GetGsymTable().AddToStringSymbolMap(funcst);

  UpdateFuncInfo(newFunction);

  mirBuilder.mirModule->SetCurFunction(originalCurrFunction);
  return newFunction;
}

void Clone::UpdateReturnVoidIfPossible(CallMeStmt *callmestmt, MIRFunction *targetFunc) {
  if (replaceRetIgnored->ShouldReplaceWithVoidFunc(callmestmt, targetFunc)) {
    if (replaceRetIgnored->IsInCloneList(targetFunc->GetName())) {
      string funcNameReturnVoid = replaceRetIgnored->GenerateNewFullName(targetFunc);
      MIRFunction *funcReturnVoid = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(funcNameReturnVoid))->value.mirFunc;
      CHECK_FATAL(nullptr != funcReturnVoid, "target function not found at ssadevirtual");
      callmestmt->puIdx = funcReturnVoid->puIdx;
    }
  }
}

void Clone::DoClone() {
  std::set<string> *clonedNewfuncMap = new std::set<string>();
  for (const string &funcname : *(replaceRetIgnored->GetTobeClonedFuncNames())) {
    MIRSymbol *symbol = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(funcname));
    if (nullptr != symbol) {
      MIRFunction *oriFunc = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(funcname))->value.mirFunc;
      mirModule->SetCurFunction(oriFunc);
      clonedNewfuncMap->insert(CloneFunctionNoReturn(oriFunc)->GetName());
    }
  }
  delete clonedNewfuncMap;
  clonedNewfuncMap = nullptr;
}

AnalysisResult *DoClone::Run(MIRModule *module, ModuleResultMgr *mrm) {
  MemPool *mp = mempoolctrler.NewMemPool(PhaseName().c_str());
  maple::MIRBuilder mirBuilder(module);
  KlassHierarchy *kh = static_cast<KlassHierarchy *>(mrm->GetAnalysisResult(MoPhase_CHA, module));
  Clone *clone = mp->New<Clone>(module, mp, mirBuilder, kh);
  clone->DoClone();
  mrm->AddResult(GetPhaseId(), module, clone);
  return clone;
}

}  // namespace maple
