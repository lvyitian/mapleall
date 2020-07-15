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

#include <cstdio>
#include <fstream>
#include <iostream>
#include "mir_nodes.h"
#include "mir_function.h"
#include "string_utils.h"

namespace maple {

void FuncAttrs::DumpAttributes() const {
#define STRING(s) #s
#define FUNC_ATTR
#define ATTR(AT)              \
  if (GetAttr(FUNCATTR_##AT)) \
    LogInfo::MapleLogger() << " " << STRING(AT);
#include "all_attributes.def"
#undef ATTR
#undef FUNC_ATTR
}

void MIRFunction::Dump(bool withoutBody) {
#if 0 // this would cause omission of func prototype when input file is .bpl
  // skip the functions that are added during process methods in
  // class and interface decls.  these has nothing in formals
  // they do have argumentsTyIdx. this can not skip ones without args
  // but for them at least the func decls are valid
  if (argumentsTyIdx.size() != formals.size()) {
    return;
  }
#endif
  if (GetAttr(FUNCATTR_optimized)) {
    return;
  }

  // save the module's curFunction and set it to the one currently Dump()ing
  mir_func_t *savedFunc = module->CurFunction();
  module->SetCurFunction(this);

  MIRSymbol *fnSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx());
  LogInfo::MapleLogger() << "func "
               << "&" << fnSt->GetName();
  funcAttrs.DumpAttributes();
  if (module->flavor < kMmpl) {
    LogInfo::MapleLogger() << " (";
    // Dump arguments
    bool hasPrintedFormal = false;
    for (int i = 0; i < formalDefVec.size(); i++) {
      MIRSymbol *symbol = formalDefVec[i].formalSym;
      if (symbol == nullptr && (formalDefVec[i].formalStrIdx.GetIdx() == 0 ||
                                GlobalTables::GetStrTable().GetStringFromStrIdx(formalDefVec[i].formalStrIdx).empty())) {
        break;
      }
      hasPrintedFormal = true;
      if (symbol == nullptr) {
        LogInfo::MapleLogger() << "var %" << GlobalTables::GetStrTable().GetStringFromStrIdx(formalDefVec[i].formalStrIdx) << " ";
      } else {
        if (symbol->sKind != kStPreg) {
          LogInfo::MapleLogger() << "var %" << symbol->GetName() << " ";
        } else {
          LogInfo::MapleLogger() << "reg %" << symbol->value.preg->pregNo << " ";
        }
      }
      MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(formalDefVec[i].formalTyIdx);
      ty->Dump(2);
      TypeAttrs tA = formalDefVec[i].formalAttrs;
      tA.DumpAttributes();
      if (i != (formalDefVec.size() - 1)) {
        LogInfo::MapleLogger() << ", ";
      }
    }
    if (IsVarargs()) {
      if (!hasPrintedFormal) {
        LogInfo::MapleLogger() << "...";
      } else {
        LogInfo::MapleLogger() << ", ...";
      }
    }
    LogInfo::MapleLogger() << ") ";
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(GetReturnTyIdx())->Dump(1);
  }

  // codeMemPool is nullptr, means mapleir has been released for memory's sake
  if (codeMemPool == nullptr) {
    LogInfo::MapleLogger() << std::endl;
    LogInfo::MapleLogger() << "# [WARNING] skipped dumping because codeMemPool is nullptr " << std::endl;
  } else if (body && !withoutBody && fnSt->GetStorageClass() != kScExtern) {
    ResetInfoPrinted();  // this ensures funcinfo will be printed
    body->Dump(module, 0, module->flavor < kMmpl ? symTab : nullptr, module->flavor < kMmpl ? pregTab : nullptr,
                false, true);  // Dump body
  } else {
    LogInfo::MapleLogger() << std::endl;
  }
  // restore the curFunction
  module->SetCurFunction(savedFunc);
}

bool MIRFunction::IsEmpty() const {
  return (body == nullptr || body->IsEmpty());
}

bool MIRFunction::IsClinit() const {
  const std::string &funcName = this->GetName();
  // this does not work for smali files like art/test/511-clinit-interface/smali/BogusInterface.smali,
  // which is decorated without "constructor".
  return StringUtils::EndsWith(funcName, NameMangler::kClinitSuffix);
}

void MIRFunction::Emit(const std::string &outFileName, bool isFirstFunction) {
  std::ofstream file;
  // Change LogInfo::MapleLogger()'s buffer to file.
  std::streambuf *backup = LogInfo::MapleLogger().rdbuf();
  LogInfo::MapleLogger().rdbuf(file.rdbuf());
  // If emit the first function, create a new file and Dump globals.
  if (isFirstFunction) {
    file.open(outFileName.c_str(), std::ios::trunc);
    module->DumpGlobals();
  } else {
    file.open(outFileName.c_str(), std::ios::app);
  }
  Dump();
  // Restore LogInfo::MapleLogger()'s buffer.
  LogInfo::MapleLogger().rdbuf(backup);
  file.close();
}

uint32 MIRFunction::GetInfo(GStrIdx strIdx) const {
  uint32 size = info.size();
  for (uint32 i = 0; i < size; i++) {
    if (info[i].first == strIdx) {
      return info[i].second;
    }
  }
  ASSERT(false, "");
  return 0;
}

uint32 MIRFunction::GetInfo(const std::string &string) const {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName(string);
  return GetInfo(strIdx);
}

void MIRFunction::OverrideBaseClassFuncNames(GStrIdx strIdx) {
  baseClassStrIdx.SetIdx(0);
  baseFuncStrIdx.SetIdx(0);
  SetBaseClassFuncNames(strIdx);
}

// there are two ways to represent the delimiter: '|' or "_7C"
// where 7C is the ascii value of char '|' in hex
void MIRFunction::SetBaseClassFuncNames(GStrIdx strIdx) {
  if (baseClassStrIdx != 0 || baseFuncStrIdx != 0) {
    return;
  }

  std::string name = GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx);

  std::string delimiter = "|";
  uint32 width = 1;  // delimiter width
  size_t pos = name.find(delimiter);
  if (pos == std::string::npos) {
    delimiter = NameMangler::kNameSplitterStr;
    width = 3;
    pos = name.find(delimiter);
    // make sure it is not __7C, but ___7C ok
    while (pos != std::string::npos && (name[pos - 1] == '_' && name[pos - 2] != '_')) {
      pos = name.find(delimiter, pos + width);
    }
  }

  if (pos != std::string::npos && pos > 0) {
    std::string classname = name.substr(0, pos);
    baseClassStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(classname);
    std::string funcnameWithtype = name.substr(pos + width, name.length() - pos - width);
    baseFuncWithTypeStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcnameWithtype);
    size_t posEnd = name.find(NameMangler::kRightBracketStr) + (std::string(NameMangler::kRightBracketStr)).length();
    funcnameWithtype = name.substr(pos + width, posEnd - pos - width);
    baseFuncSigStridx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcnameWithtype);
    size_t pos1 = name.find(delimiter, pos + width);
    while (pos1 != std::string::npos && (name[pos1 - 1] == '_' && name[pos1 - 2] != '_')) {
      pos1 = name.find(delimiter, pos1 + width);
    }
    if (pos1) {
      std::string funcname = name.substr(pos + width, pos1 - pos - width);
      baseFuncStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname);

      std::string signature = name.substr(pos1 + width, name.length() - pos1 - width);
      signatureStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(signature);
    }
    return;
  }

  baseFuncStrIdx = strIdx;
}

MIRSymbol *MIRFunction::GetLocalOrGlobalSymbol(const StIdx &idx, bool checkfirst) const {
  return idx.Islocal() ? symTab->GetSymbolFromStIdx(idx.Idx(), checkfirst)
                       : GlobalTables::GetGsymTable().GetSymbolFromStIdx(idx.Idx(), checkfirst);
}

MIRType *MIRFunction::GetNodeType(base_node_t *node) {
  if (node->op == OP_dread) {
    MIRSymbol *sym = GetLocalOrGlobalSymbol(static_cast<DreadNode *>(node)->stIdx);
    return GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
  } else if (node->op == OP_regread) {
    RegreadNode *nodereg = static_cast<RegreadNode *>(node);
    MIRPreg *preg = pregTab->PregFromPregIdx(nodereg->regIdx);
    if (preg->primType == PTY_ref) {
      return preg->mirType;
    }
  }
  return nullptr;
}

void MIRFunction::NewBody() {
  body = codeMemPool->New<BlockNode>();
  // If mir_function.has been seen as a declaration, its symTab has to be moved
  // from module mempool to function mempool.
  MIRSymbolTable *oldSymtab = symTab;
  MIRPregTable *oldPregtab = pregTab;
  MIRTypeNameTable *oldTypenametab = typeNameTab;
  MIRLabelTable *oldLabeltab = labelTab;

  symTab = dataMemPool->New<MIRSymbolTable>(&dataMPAllocator);
  pregTab = dataMemPool->New<MIRPregTable>(&dataMPAllocator);
  typeNameTab = dataMemPool->New<MIRTypeNameTable>(&dataMPAllocator);
  labelTab = dataMemPool->New<MIRLabelTable>(&dataMPAllocator);

  if (oldSymtab == nullptr) {
    // formals not yet entered into symTab; enter them now
    for (size_t i = 0; i < formalDefVec.size(); i++) {
      FormalDef &formalDef = formalDefVec[i];
      formalDef.formalSym = symTab->CreateSymbol(kScopeLocal);
      formalDef.formalSym->storageClass = kScFormal;
      formalDef.formalSym->SetNameStridx(formalDef.formalStrIdx);
      formalDef.formalSym->SetTyIdx(formalDef.formalTyIdx);
      formalDef.formalSym->SetAttrs(formalDef.formalAttrs);
      const std::string formalName = GlobalTables::GetStrTable().GetStringFromStrIdx(formalDef.formalStrIdx);
      if (!isdigit(formalName.front())) {
        formalDef.formalSym->sKind = kStVar;
        symTab->AddToStringSymbolMap(formalDef.formalSym);
      } else {
        formalDef.formalSym->sKind = kStPreg;
        uint32 thepregno = std::stoi(formalName);
        MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(formalDef.formalTyIdx);
        PregIdx pregIdx = pregTab->EnterPregNo(thepregno, mirType->primType, mirType);
        MIRPreg *preg = pregTab->PregFromPregIdx(pregIdx);
        formalDef.formalSym->value.preg = preg;
      }
    }
  }
  else {
    for (size_t i = 1; i < oldSymtab->GetSymbolTableSize(); i++) {
      symTab->AddStOutside(oldSymtab->GetSymbolFromStIdx(i));
    }
  }
  if (oldPregtab) {
    for (size_t i = 1; i < oldPregtab->Size(); i++) {
      pregTab->AddPreg(oldPregtab->PregFromPregIdx(i));
    }
  }
  if (oldTypenametab) {
    CHECK_FATAL(oldTypenametab->Size() == typeNameTab->Size(),
           "Does not expect to process typeNameTab in MIRFunction::NewBody");
  }
  if (oldLabeltab) {
    CHECK_FATAL(oldLabeltab->Size() == labelTab->Size(), "Does not expect to process labelTab in MIRFunction::NewBody");
  }
}

#ifdef DEBUGME
void MIRFunction::SetUpGDBEnv() {
  if (codeMemPool != nullptr) {
    mempoolctrler.DeleteMemPool(codeMemPool);
  }
  codeMemPool = mempoolctrler.NewMemPool("tmp debug");
  codeMemPoolAllocator.SetMemPool(codeMemPool);
}

void MIRFunction::ResetGDBEnv() {
  mempoolctrler.DeleteMemPool(codeMemPool);
  codeMemPool = nullptr;
}

#endif

}  // namespace maple
