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

#include "mir_const.h"
#include "mir_module.h"
#include "mir_preg.h"
#include "mir_function.h"
#include "mir_builder.h"
#include "intrinsics.h"
#include "bin_mplt.h"
#define DEBUG_SYMBOL 1

#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <unordered_set>
#include <cctype>

namespace maple {

#if MIR_FEATURE_FULL  // to avoid compilation error when MIR_FEATURE_FULL=0
MIRModule::MIRModule(const char *fn)
  : memPool(mempoolctrler.NewMemPool("mapleir mempool")),
    memPoolAllocator(memPool),
    functionList(memPoolAllocator.Adapter()),
    compilationList(memPoolAllocator.Adapter()),
    importedMplt(memPoolAllocator.Adapter()),
    typeDefOrder(memPoolAllocator.Adapter()),
    externStructTypeSet(std::less<TyIdx>(), memPoolAllocator.Adapter()),
    symbolSet(std::less<StIdx>(), memPoolAllocator.Adapter()),
    symbolDefOrder(memPoolAllocator.Adapter()),
    someSymbolNeedForwDecl(false),
    hints(0),
    out(LogInfo::MapleLogger()),
    entryFuncName(""),
    fileName(fn),
    throwableTyidx(0),
    entryFunc(nullptr),
    floatNum(0),
    fileInfo(memPoolAllocator.Adapter()),
    fileInfoIsString(memPoolAllocator.Adapter()),
    fileData(memPoolAllocator.Adapter()),
    srcFileInfo(memPoolAllocator.Adapter()),
    importFiles(memPoolAllocator.Adapter()),
    importPaths(memPoolAllocator.Adapter()),
    classList(memPoolAllocator.Adapter()),
    optimizedFuncs(memPoolAllocator.Adapter()),
    rcNotNeedingLock(memPoolAllocator.Adapter()),
    puIdxFieldInitializedMap(std::less<PUIdx>(),
                                memPoolAllocator.Adapter()),
    inliningGlobals(memPoolAllocator.Adapter()) {
  flavor = kFlavorUnknown;
  srcLang = kSrcLangUnknown;
  id = 0xffff;
  globalMemSize = 0;
  globalBlkMap = nullptr;
  globalWordsTypeTagged = nullptr;
  globalWordsRefCounted = nullptr;
  mainFuncID = 0;
  numFuncs = 0;
  withProfileInfo = false;
  GlobalTables::GetGsymTable().module = this;
  typeNameTab = memPool->New<MIRTypeNameTable>(&memPoolAllocator);
  mirBuilder = memPool->New<MIRBuilder>(this);
  IntrinDesc::InitMIRModule(this);
  binMplt = nullptr;
  useFuncCodeMpTmp = false;
}

MIRFunction *MIRModule::CurFunction(void) const {
  return static_cast<MIRFunction *>(GetCurFunction());
}

MemPool *MIRModule::CurFuncCodeMemPool(void) const {
  if (useFuncCodeMpTmp) {
    return static_cast<MIRFunction *>(GetCurFunction())->codeMemPoolTmp;
  } else {
    return static_cast<MIRFunction *>(GetCurFunction())->codeMemPool;
  }
}

MapleAllocator *MIRModule::CurFuncCodeMemPoolAllocator(void) const {
  return &(static_cast<MIRFunction *>(GetCurFunction())->codeMemPoolAllocator);
}

void MIRModule::AddExternStructType(TyIdx tyIdx) {
  externStructTypeSet.insert(tyIdx);
}

void MIRModule::AddExternStructType(const MIRType *ty) {
  externStructTypeSet.insert(ty->tyIdx);
}

void MIRModule::AddSymbol(StIdx stIdx) {
  auto it = symbolSet.find(stIdx);
  if (it == symbolSet.end()) {
    symbolDefOrder.push_back(stIdx);
  }
  symbolSet.insert(stIdx);
}

void MIRModule::AddSymbol(const MIRSymbol *s) {
  AddSymbol(s->stIdx);
}

void MIRModule::DumpGlobals (bool emitStructureType) const {
  if (flavor != kFlavorUnknown) {
    LogInfo::MapleLogger() << "flavor " << flavor << std::endl;
  }
  if (srcLang != kSrcLangUnknown) {
    LogInfo::MapleLogger() << "srclang " << srcLang << std::endl;
  }
  LogInfo::MapleLogger() << "id " << id << std::endl;
  if (globalMemSize != 0) {
    LogInfo::MapleLogger() << "globalmemsize " << globalMemSize << std::endl;
  }
  if (globalBlkMap != nullptr) {
    LogInfo::MapleLogger() << "globalmemmap = [ ";
    uint32 *p = reinterpret_cast<uint32 *>(globalBlkMap);
    LogInfo::MapleLogger() << std::hex;
    while (p < reinterpret_cast<uint32 *>(globalBlkMap + globalMemSize)) {
      LogInfo::MapleLogger() << std::hex << "0x" << *p << " ";
      p++;
    }
    LogInfo::MapleLogger() << std::dec << "]\n";
  }
  if (globalWordsTypeTagged != nullptr) {
    LogInfo::MapleLogger() << "globalwordstypetagged = [ ";
    uint32 *p = reinterpret_cast<uint32 *>(globalWordsTypeTagged);
    LogInfo::MapleLogger() << std::hex;
    while (p < reinterpret_cast<uint32 *>(globalWordsTypeTagged + BlkSize2BitvectorSize(globalMemSize))) {
      LogInfo::MapleLogger() << std::hex << "0x" << *p << " ";
      p++;
    }
    LogInfo::MapleLogger() << std::dec << "]\n";
  }
  if (globalWordsRefCounted != nullptr) {
    LogInfo::MapleLogger() << "globalwordsrefcounted = [ ";
    uint32 *p = reinterpret_cast<uint32 *>(globalWordsRefCounted);
    LogInfo::MapleLogger() << std::hex;
    while (p < reinterpret_cast<uint32 *>(globalWordsRefCounted + BlkSize2BitvectorSize(globalMemSize))) {
      LogInfo::MapleLogger() << std::hex << "0x" << *p << " ";
      p++;
    }
    LogInfo::MapleLogger() << std::dec << "]\n";
  }
  LogInfo::MapleLogger() << "numfuncs " << numFuncs << std::endl;
  if (importFiles.size() != 0) {
    uint32 size = importFiles.size();
    for (uint32 i = 0; i < size; i++) {
      LogInfo::MapleLogger() << "import \"" << GlobalTables::GetStrTable().GetStringFromStrIdx(importFiles[i]) << "\"" << std::endl;
    }
  }
  if (importPaths.size() != 0) {
    uint32 size = importPaths.size();
    for (uint32 i = 0; i < size; i++) {
      LogInfo::MapleLogger() << "importpath \"" << GlobalTables::GetStrTable().GetStringFromStrIdx(importPaths[i]) << "\"" << std::endl;
    }
  }
  if (entryFuncName.length()) {
    LogInfo::MapleLogger() << "entryfunc &" << entryFuncName << std::endl;
  }
  if (fileInfo.size() != 0) {
    LogInfo::MapleLogger() << "fileinfo {\n";
    uint32 size = fileInfo.size();
    for (uint32 i = 0; i < size; i++) {
      LogInfo::MapleLogger() << "  @" << GlobalTables::GetStrTable().GetStringFromStrIdx(fileInfo[i].first) << " ";
      if (!fileInfoIsString[i]) {
        LogInfo::MapleLogger() << "0x" << std::hex << fileInfo[i].second;
      } else {
        LogInfo::MapleLogger() << "\"" << GlobalTables::GetStrTable().GetStringFromStrIdx(fileInfo[i].second) << "\"";
      }
      if (i < size - 1) {
        LogInfo::MapleLogger() << ",\n";
      } else {
        LogInfo::MapleLogger() << "}\n";
      }
    }
    LogInfo::MapleLogger() << std::dec;
  }
  if (srcFileInfo.size() != 0) {
    LogInfo::MapleLogger() << "srcfileinfo {\n";
    uint32 size = srcFileInfo.size();
    uint32 i = 0;
    for (auto it : srcFileInfo) {
      LogInfo::MapleLogger() << "  " << it.second;
      LogInfo::MapleLogger() << " \"" << GlobalTables::GetStrTable().GetStringFromStrIdx(it.first) << "\"";
      if (i++ < size - 1) {
        LogInfo::MapleLogger() << ",\n";
      } else {
        LogInfo::MapleLogger() << "}\n";
      }
    }
  }
  if (fileData.size() != 0) {
    LogInfo::MapleLogger() << "filedata {\n";
    uint32 size = fileData.size();
    for (uint32 i = 0; i < size; i++) {
      LogInfo::MapleLogger() << "  @" << GlobalTables::GetStrTable().GetStringFromStrIdx(fileData[i].first) << " ";
      uint32 datasize = fileData[i].second.size();
      for (uint32 j = 0; j < datasize; j++) {
        uint8 data = fileData[i].second[j];
        LogInfo::MapleLogger() << "0x" << std::hex << static_cast<uint32>(data);
        if (j < datasize - 1) {
          LogInfo::MapleLogger() << ' ';
        }
      }
      if (i < size - 1) {
        LogInfo::MapleLogger() << ",\n";
      } else {
        LogInfo::MapleLogger() << "}\n";
      }
    }
    LogInfo::MapleLogger() << std::dec;
  }
  if (flavor < kMmpl) {
    for (MapleVector<GStrIdx>::const_iterator it = typeDefOrder.begin(); it != typeDefOrder.end(); it++) {
      TyIdx tyIdx = typeNameTab->gStrIdxToTyIdxMap[*it];
      const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(*it);
      MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
      MIRStructType *stype = dynamic_cast<MIRStructType *>(type);
      if (stype && !emitStructureType) {
        // still emit what in externStructTypeSet
        if (externStructTypeSet.find(stype->tyIdx) == externStructTypeSet.end()) {
          continue;
        }
      }
      if (stype && stype->isImported) {
        continue;
      }
      LogInfo::MapleLogger() << "type $" << name << " ";
      if (type->GetKind() == kTypeByName) {
        LogInfo::MapleLogger() << "void";
      } else if (type->nameStrIdx == *it) {
        type->Dump(1, true);
      } else {
        type->Dump(1);
      }
      LogInfo::MapleLogger() << std::endl;
    }

    if (someSymbolNeedForwDecl) {
      // an extra pass thru the global symbol table to print forward decl
      for (MapleSet<StIdx>::iterator sit = symbolSet.begin(); sit != symbolSet.end(); sit++) {
        MIRSymbol *s = GlobalTables::GetGsymTable().GetSymbolFromStIdx((*sit).Idx());
        if (s->needForwDecl) {
          s->Dump(false, 0, true);
        }
      }
    }
    // dump javaclass and javainterface first
    for (MapleVector<StIdx>::const_iterator sit = symbolDefOrder.begin(); sit != symbolDefOrder.end(); sit++) {
      MIRSymbol *s = GlobalTables::GetGsymTable().GetSymbolFromStIdx((*sit).Idx());
      if (!s->IsJavaClassInterface()) {
        continue;
      }
      // Verify: all wpofake variables should have been deleted from globaltable
      ASSERT(!(s->wpofakeParm || s->wpofakeRet) || s->IsDeleted(), "wpofake var not deleted");
      if (!s->IsDeleted()) {
        s->Dump(false, 0);
      }
    }
    for (MapleVector<StIdx>::const_iterator sit = symbolDefOrder.begin(); sit != symbolDefOrder.end(); sit++) {
      MIRSymbol *s = GlobalTables::GetGsymTable().GetSymbolFromStIdx((*sit).Idx());
      if (s->IsJavaClassInterface()) {
        continue;
      }
      // Verify: all wpofake variables should have been deleted from globaltable
      ASSERT(!(s->wpofakeParm || s->wpofakeRet) || s->IsDeleted(), "wpofake var not deleted");
      if (!s->IsDeleted()) {
        s->Dump(false, 0);
      }
    }
  }
}

void MIRModule::Dump(bool emitStructureType,
                     std::unordered_set<std::string> *dumpFuncSet) {
  DumpGlobals(emitStructureType);
  DumpFunctionList(dumpFuncSet);
}

void MIRModule::DumpGlobalArraySymbol() {
  MapleSet<StIdx>::iterator sit = symbolSet.begin();
  for (; sit != symbolSet.end(); sit++) {
    MIRSymbol *s = GlobalTables::GetGsymTable().GetSymbolFromStIdx((*sit).Idx());
    MIRType *stype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(s->GetTyIdx());
    if (stype == nullptr || stype->typeKind != kTypeArray) {
      continue;
    }
    s->Dump(false, 0);
  }
}

void MIRModule::Emit(const std::string &outFileName) const {
  std::ofstream file;
  // Change cout's buffer to file.
  std::streambuf *backup = LogInfo::MapleLogger().rdbuf();
  LogInfo::MapleLogger().rdbuf(file.rdbuf());
  file.open(outFileName, std::ios::trunc);
  DumpGlobals();
  for (MIRFunction *mirFunc : functionList) {
    mirFunc->Dump();
  }
  // Restore cout's buffer.
  LogInfo::MapleLogger().rdbuf(backup);
}

void MIRModule::DumpFunctionList(std::unordered_set<std::string> *dumpFuncSet) {
  for (MapleVector<MIRFunction *>::iterator it = functionList.begin(); it != functionList.end(); it++) {
    if (dumpFuncSet == nullptr || dumpFuncSet->empty()) {
      (*it)->Dump();
    } else {  // dump only if this func matches any name in *dumpFuncSet
      const std::string &name = (*it)->GetName();
      bool matched = false;
      for (std::string elem : *dumpFuncSet) {
        if (name.find(elem.c_str()) != string::npos) {
          matched = true;
          break;
        }
      }
      if (matched) {
        (*it)->Dump();
      }
    }
  }
}

void MIRModule::OutputFunctionListAsciiMpl(const char *phaseName) {
  std::string filestem;
  std::string::size_type lastdot = fileName.find_last_of(".");
  if (lastdot == std::string::npos) {
    filestem = fileName.append(phaseName);
  } else {
    filestem = fileName.substr(0, lastdot).append(phaseName);
  }
  std::string outfilename;
  if (flavor >= kMmpl) {
    outfilename = filestem.append(".mmpl");
  } else {
    outfilename = filestem.append(".mpl");
  }
  std::ofstream mplfile;
  mplfile.open(outfilename, std::ios::app);
  std::streambuf *backup = LogInfo::MapleLogger().rdbuf();
  LogInfo::MapleLogger().rdbuf(mplfile.rdbuf());  // change LogInfo::MapleLogger()'s buffer to that of file
  DumpGlobalArraySymbol();
  DumpFunctionList(nullptr);
  LogInfo::MapleLogger().rdbuf(backup);  // restore LogInfo::MapleLogger()'s buffer
  mplfile.close();
  return;
}

void MIRModule::DumpToFile(const std::string &fileName, bool emitStructureType) {
  std::ofstream file;
  // Change LogInfo::MapleLogger()'s buffer to file.
  std::streambuf *backup = LogInfo::MapleLogger().rdbuf();
  LogInfo::MapleLogger().rdbuf(file.rdbuf());
  file.open(fileName.c_str(), std::ios::trunc);
  Dump(emitStructureType);
  // Restore LogInfo::MapleLogger()'s buffer.
  LogInfo::MapleLogger().rdbuf(backup);
  file.close();
}

void MIRModule::DumpInlineCandidateToFile(const std::string &fileName) {
  if (!optimizedFuncs.size()) {
    return;
  }
  std::ofstream file;
  // Change LogInfo::MapleLogger()'s buffer to file.
  std::streambuf *backup = LogInfo::MapleLogger().rdbuf();
  LogInfo::MapleLogger().rdbuf(file.rdbuf());
  file.open(fileName.c_str(), std::ios::trunc);

  // dump global variables needed for inlining file
  for (auto fit : inliningGlobals) {
    MIRSymbol *s = GlobalTables::GetGsymTable().GetSymbolFromStIdx(fit);
    if (s->storageClass == kScFstatic) {
      if (s->needForwDecl) {
        // const string, including initialization
        s->Dump(false, 0, false);
      }
    }
  }

  for (auto fit : inliningGlobals) {
    MIRSymbol *s = GlobalTables::GetGsymTable().GetSymbolFromStIdx(fit);
    MIRStorageClass sc = s->storageClass;
    if (s->storageClass == kScFstatic) {
      if (!s->needForwDecl) {
        // const string, including initialization
        s->Dump(false, 0, false);
      }
    } else if (s->sKind == kStFunc) {
      s->GetFunction()->Dump(true);
    } else {
      // static fields as extern
      s->storageClass = kScExtern;
      s->Dump(false, 0, true);
    }
    s->storageClass = sc;
  }

  for (MapleVector<MIRFunction *>::iterator it = optimizedFuncs.begin(); it != optimizedFuncs.end(); it++) {
    (*it)->withLocInfo = false;
    (*it)->Dump();
  }

  // Restore LogInfo::MapleLogger()'s buffer.
  LogInfo::MapleLogger().rdbuf(backup);
  file.close();
}

// This is not efficient. Only used in debug mode for now.
const std::string &MIRModule::GetFilenameFromFilenum(uint32 fileNum) {
  GStrIdx nameidx(0);
  for (auto &info : srcFileInfo) {
    if (info.second == fileNum) {
      nameidx = info.first;
    }
  }
  return GlobalTables::GetStrTable().GetStringFromStrIdx(nameidx);
}

void MIRModule::DumpToHeaderFile(bool binaryMplt, std::string outputName) {
  std::string outfilename;
  std::string fileName;

  if (outputName.empty()) {
    fileName = this->fileName;
  } else {
    fileName = outputName;
  }
  std::string::size_type lastdot = fileName.find_last_of(".");
  if (lastdot == std::string::npos) {
    outfilename = fileName.append(".mplt");
  } else {
    outfilename = fileName.substr(0, lastdot).append(".mplt");
  }

  if (binaryMplt) {
    BinaryMplt binMplt(*this);
    binMplt.Export(outfilename);
  } else {
    std::ofstream mpltfile;
    mpltfile.open(outfilename, std::ios::trunc);
    std::streambuf *backup = LogInfo::MapleLogger().rdbuf();
    LogInfo::MapleLogger().rdbuf(mpltfile.rdbuf());  // change LogInfo::MapleLogger()'s buffer to that of file

    for (std::pair<std::u16string, MIRSymbol *> entity : GlobalTables::GetConstPool().constU16StringPool) {
      LogInfo::MapleLogger() << "var $";
      entity.second->DumpAsLiteralVar(1);
      LogInfo::MapleLogger() << std::endl;
    }

    for (auto it = classList.begin(); it != classList.end(); it++) {
      TyIdx curTyidx(*it);
      MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(curTyidx);
      const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(type->nameStrIdx);
      if (type->typeKind == kTypeClass || type->typeKind == kTypeInterface) {
        MIRStructType *structtype = static_cast<MIRStructType *>(type);
        // skip imported class/interface and incomplete types
        // if ((!structtype->isImported ||(structtype->isImported && structtype->isUsed) )&&
        // !structtype->IsIncomplete()) {
        if (!structtype->isImported && !structtype->IsIncomplete()) {
          LogInfo::MapleLogger() << "type $" << name << " ";
          type->Dump(1, true);
          LogInfo::MapleLogger() << std::endl;
        }
      }
    }
    /* restore LogInfo::MapleLogger() */
    LogInfo::MapleLogger().rdbuf(backup);
    mpltfile.close();
  }
}

/*
    We use MIRStructType (kTypeStruct) to represent C/C++ structs
    as well as C++ classes.

    We use MIRClassType (kTypeClass) to represent Java classes, specifically.
    MIRClassType has parents which encode Java class's parent (exploiting
    the fact Java classes have at most one parent class.
 */
void MIRModule::DumpTypeTreeToCxxHeaderFile(MIRType *ty, std::unordered_set<MIRType *> &dumpedClasses) {
  if (dumpedClasses.find(ty) != dumpedClasses.end()) {
    return;
  }

  // first, insert ty to the dumped_classes to prevent infinite recursion
  dumpedClasses.insert(ty);

  ASSERT(ty->typeKind == kTypeClass || ty->typeKind == kTypeStruct || ty->typeKind == kTypeUnion || ty->typeKind == kTypeInterface,
          "");
  /* No need to emit interfaces; because "interface variables are
     final and static by default and methods are public and abstract"
   */
  if (ty->typeKind == kTypeInterface) {
    return;
  }

  // dump all of its parents
  if (srcLang == kSrcLangJava) {
    ASSERT(ty->typeKind != kTypeStruct && ty->typeKind != kTypeUnion && ty->typeKind != kTypeInterface,
            "type is not supposed to be struct/union/interface");
  } else if (srcLang == kSrcLangC || srcLang == kSrcLangCPlusPlus) {
    ASSERT((ty->typeKind == kTypeStruct || ty->typeKind == kTypeUnion), "type should be either struct or union");
  } else {
    ASSERT(false, "source languages other than Java/C/C++ are not supported yet");
  }

  const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(ty->nameStrIdx);
  if (srcLang == kSrcLangJava) {
    // Java class has at most one parent
    MIRClassType *clty = static_cast<MIRClassType *>(ty);
    MIRClassType *prty = nullptr;
    // find parent and generate its type as well as those of its ancestors
    if (clty->parentTyIdx != TyIdx(0) /*invalid type idx*/) {
      prty = static_cast<MIRClassType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(clty->parentTyIdx));
      DumpTypeTreeToCxxHeaderFile(prty, dumpedClasses);
    }

    LogInfo::MapleLogger() << "struct " << name << " ";
    if (prty) {
      LogInfo::MapleLogger() << ": " << prty->GetName() << " ";
    }

    if (!clty->IsIncomplete()) {
      /* dump class type; it will dump as '{ ... }' */
      clty->DumpAsCxx(1);
      LogInfo::MapleLogger() << ";" << std::endl;
    } else {
      LogInfo::MapleLogger() << "  /* incomplete type */" << std::endl;
    }
  } else if (srcLang == kSrcLangC || srcLang == kSrcLangCPlusPlus) {
    // how to access parent fields????
    ASSERT(false, "NYI");
  }
}

void MIRModule::DumpToCxxHeaderFile(std::set<MapleString> &leafClasses, const char *pathToOutf) {
  std::string outfilename;
  outfilename = pathToOutf;
  std::ofstream mpltfile;
  mpltfile.open(outfilename, std::ios::trunc);
  std::streambuf *backup = LogInfo::MapleLogger().rdbuf();
  LogInfo::MapleLogger().rdbuf(mpltfile.rdbuf());  // change LogInfo::MapleLogger()'s buffer to that of file

  char *headerGuard = strdup(outfilename.c_str());
  if (headerGuard == nullptr){
    CHECK_FATAL(false, "strdup failed");
  }
  for (char *p = headerGuard; *p; ++p) {
    if (!isalnum(*p)) {
      *p = '_';
    } else if (isalpha(*p) && islower(*p)) {
      *p = toupper(*p);
    }
  }

  // define a hash table
  std::unordered_set<MIRType *> dumpedClasses;

  const char *prefix = "__SRCLANG_UNKNOWN_";
  if (srcLang == kSrcLangJava) {
    prefix = "__SRCLANG_JAVA_";
  } else if (srcLang == kSrcLangC || srcLang == kSrcLangCPlusPlus) {
    prefix = "__SRCLANG_CXX_";
  }

  LogInfo::MapleLogger() << "#ifndef " << prefix << headerGuard << "__" << std::endl;
  LogInfo::MapleLogger() << "#define " << prefix << headerGuard << "__" << std::endl;
  LogInfo::MapleLogger() << "/* this file is compiler-generated; do not edit */" << std::endl;
  LogInfo::MapleLogger() << std::endl;
  LogInfo::MapleLogger() << "#include <stdint.h>" << std::endl;
  LogInfo::MapleLogger() << "#include <complex.h>" << std::endl;

  for (auto &s : leafClasses) {
    CHECK_FATAL(s.c_str(), "string is null ");
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(std::string(s.c_str()));
    TyIdx tyIdx = typeNameTab->gStrIdxToTyIdxMap[strIdx];
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
    if (!ty) {
      continue;
    }

    ASSERT(
      ty->typeKind == kTypeClass || ty->typeKind == kTypeStruct || ty->typeKind == kTypeUnion || ty->typeKind == kTypeInterface,
      "");
    DumpTypeTreeToCxxHeaderFile(ty, dumpedClasses);
  }

  LogInfo::MapleLogger() << "#endif /* " << prefix << headerGuard << "__ */" << std::endl;

  /* restore LogInfo::MapleLogger() */
  LogInfo::MapleLogger().rdbuf(backup);
  mpltfile.close();
  free(headerGuard);
  headerGuard = nullptr;
}

void MIRModule::DumpClassToFile(const char *path) {
  ASSERT(path != nullptr, "");
  std::string spath(path);
  spath.append("/");
  for (auto it : typeNameTab->gStrIdxToTyIdxMap) {
    const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(it.first);
    MIRType *type = GlobalTables::GetTypeTable().typeTable[it.second.GetIdx()];
    std::string outClassfile = name.c_str();
    /* replace class name / with - */
    std::replace(outClassfile.begin(), outClassfile.end(), '/', '-');
    outClassfile.insert(0, spath);
    outClassfile.append(".mpl");
    std::ofstream mplfile;
    mplfile.open(outClassfile.c_str(), std::ios::trunc);
    std::streambuf *backup = LogInfo::MapleLogger().rdbuf();
    LogInfo::MapleLogger().rdbuf(mplfile.rdbuf());

    /* dump class type */
    LogInfo::MapleLogger() << "type $" << name << " ";
    if (type->nameStrIdx == it.first && type->GetKind() != kTypeByName) {
      type->Dump(1, true);
    } else {
      type->Dump(1);
    }
    LogInfo::MapleLogger() << std::endl;
    /* restore LogInfo::MapleLogger() */
    LogInfo::MapleLogger().rdbuf(backup);
    mplfile.close();
  }
}

MIRFunction *MIRModule::FindEntryFunction() {
  for (uint32 i = 0; i < functionList.size(); i++) {
    MIRFunction *currFunc = functionList[i];
    if (currFunc->GetName() == entryFuncName) {
      entryFunc = currFunc;
      return currFunc;
    }
  }
  return nullptr;
}

// given the phase name (including '.' at beginning), output the program in the
// module to the file with given file suffix, and file stem from
// this->fileName appended with phaseName
void MIRModule::OutputAsciiMpl(const char *phaseName, const char *suffix,
                               std::unordered_set<std::string> *dumpFuncSet,
                               bool emitStructureType, bool binaryform) {
  CHECK_FATAL(!(emitStructureType && binaryform), "Cannot emit type info in .bpl");
  std::string filestem;
  std::string::size_type lastdot = fileName.find_last_of(".");
  if (lastdot == std::string::npos) {
    filestem = fileName.append(phaseName);
  } else {
    filestem = fileName.substr(0, lastdot).append(phaseName);
  }
  std::string outfilename;
  outfilename = filestem + suffix;
  if (!binaryform) {
    std::ofstream mplfile;
    mplfile.open(outfilename, std::ios::trunc);
    std::streambuf *backup = LogInfo::MapleLogger().rdbuf();
    LogInfo::MapleLogger().rdbuf(mplfile.rdbuf());  // change LogInfo::MapleLogger()'s buffer to that of file
    Dump(emitStructureType, dumpFuncSet);
    LogInfo::MapleLogger().rdbuf(backup);  // restore LogInfo::MapleLogger()'s buffer
    mplfile.close();
  } else {
    BinaryMplt binMplt(*this);
    binMplt.GetBinExport().not2mplt = true;
    binMplt.Export(outfilename);
  }
  return;
}

uint32 MIRModule::GetFileinfo(GStrIdx strIdx) const {
  uint32 size = fileInfo.size();
  for (uint32 i = 0; i < size; i++) {
    if (fileInfo[i].first == strIdx) {
      return fileInfo[i].second;
    }
  }
  ASSERT(0, "");
  return 0;
}

std::string MIRModule::GetFileNameAsPostfix() const {
  std::string fileName = NameMangler::kFileNameSplitterStr;
  if (fileInfo.size() != 0) {
    // option 1: file name in INFO
    uint32 fileNameIdx = GetFileinfo(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("INFO_filename"));
    fileName += GlobalTables::GetStrTable().GetStringFromStrIdx(GStrIdx(fileNameIdx));
  } else {
    // option 2: src file name removing ext name.
    CHECK_FATAL(fileName.find_last_of(".") != fileName.npos, "not found .");
    fileName += fileName.substr(0, fileName.find_last_of("."));
  }

  for (uint32 i = 0; i < fileName.length(); ++i) {
    char c = fileName[i];
    if (!isalpha(c) && !isdigit(c) && c != '_' && c != '$') {
      fileName[i] = '_';
    }
  }
  return fileName;
}

void MIRModule::AddClass(TyIdx t) {
  classList.insert(t.GetIdx());
  return;
}

void MIRModule::RemoveClass(TyIdx t) {
  classList.erase(t.GetIdx());
  return;
}

#endif  // MIR_FEATURE_FULL

void MIRModule::ReleaseCurFuncMpTmp() {
  CurFunction()->ReleaseMemory();
}

}  // namespace maple
