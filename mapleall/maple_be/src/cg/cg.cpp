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

#include "cg.h"
#include "cg_lowerer.h"
#include "emit.h"
#include "mir_builder.h"
#include "mir_lower.h"
#include "constant_fold.h"
#include "ebo.h"
#include "cfg_optimizer.h"
#include "ico.h"

#include <iostream>   // LogInfo::MapleLogger(), etc
#include <cinttypes>  // PRIu64, etc
#include <set>


namespace maplebe {

using namespace maple;
using namespace std;
#define JAVALANG (mirModule->IsJavaModule())

const char *BB::bbNames[BB::kBBLast] = { "BB_ft",  "BB_if",        "BB_goto",      "BB_igoto", "BB_call",
                                         "BB_ret", "BB_intrinsic", "BB_rangegoto", "BB_throw" };

CGFunc *CG::curCgFunc = nullptr;
PUIdx CG::curPuIdx = 0;

const char *CG::kCMacroDefSuffix = ".macros.def";
const char *CG::kGctibSuffix = ".gctib.s";
const char *CG::kGrootListSuffix = ".groots.txt";
const char *CG::kPrimorListSuffix = ".primordials.txt";

void CG::GenFieldOffsetMap(const std::string &classlistfile) {
  BECommon &becommon = *g->becommon;

  std::ifstream infile(classlistfile);
  std::string str;
  bool haveunknown = false;

  // For Java check each class name first and expose all unknown classes
  if (JAVALANG) {
    while (infile >> str) {
      MIRType *type = GlobalTables::GetTypeTable().GetClassType(str.c_str(), mirModule);
      MIRClassType *classtype = static_cast<MIRClassType *>(type);
      if (!classtype) {
        LogInfo::MapleLogger() << " >>>>>>>> unknown class: " << str.c_str() << endl;
        haveunknown = true;
      }
    }
  }

  if (haveunknown) {
    return;
  }

  // rewind
  infile.clear();
  infile.seekg(0);
  while (infile >> str) {
    becommon.GenFieldOffsetMap(str);
  }

  return;
}

/**
 * This function intends to be a more general form of GenFieldOffsetmap.
 */
void CG::GenExtraTypeMetadata(const std::string &classListFileName, const std::string &outputBasename) {
  BECommon &becommon = *g->becommon;

  std::vector<MIRClassType *> classesToGenerate;

  if (classListFileName.empty()) {
    // Class list not specified.  Visit all classes.
    std::set<std::string> visited;

    for (auto tyid : mirModule->classList) {
      TyIdx tyIdx(tyid);
      MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);

      MIRClassType *classtype = dynamic_cast<MIRClassType *>(ty);

      if (classtype == nullptr) {
        continue;  // Skip non-class. Too paranoid.  We just enumerated classList!
      }

      const std::string &name = classtype->GetName();

      if (visited.find(name) != visited.end()) {
        continue;  // Skip duplicated class definitions. Workaround issue12
      }

      visited.insert(name);
      classesToGenerate.push_back(classtype);
    }
  } else {
    // Visit listed classes.
    std::ifstream infile(classListFileName);
    std::string str;
    bool haveunknown = false;

    // check each class name first and expose all unknown classes
    while (infile >> str) {
      MIRType *type = GlobalTables::GetTypeTable().GetClassType(str.c_str(), mirModule);
      MIRClassType *classtype = static_cast<MIRClassType *>(type);
      if (!classtype) {
        LogInfo::MapleLogger() << " >>>>>>>> unknown class: " << str.c_str() << endl;
        haveunknown = true;
      }

      classesToGenerate.push_back(classtype);
    }
    if (haveunknown) {
      return;
    }
  }

  if (cgopt_.GenDef()) {
    std::string outputFilename = outputBasename + kCMacroDefSuffix;
    FILE *outputFile = fopen(outputFilename.c_str(), "w");
    CHECK_FATAL(outputFile != nullptr, "open file failed in CG::GenExtraTypeMetadata");
    for (auto classtype : classesToGenerate) {
      becommon.GenObjSize(classtype, outputFile);
      becommon.GenFieldOffsetMap(classtype, outputFile);
    }
    fclose(outputFile);
  }

  if (cgopt_.GenGctib()) {
    fprintf(stderr, "--gen-gctib-file option not implemented");
  }

  return;
}

void CG::GenGlobalRootList(const std::string &outputBasename) {
  static const bool kDebugGenGlobalRootList = false;

  if (!cgopt_.GenGrootList()) {
    return;
  }

  std::string outputFilename = outputBasename + kGrootListSuffix;
  FILE *outputFile = fopen(outputFilename.c_str(), "w");
  CHECK_FATAL(outputFile != nullptr, "open file failed in CG::GenGlobalRootList");

  for (StIdx stIdx : mirModule->symbolSet) {
    MIRSymbol *symbol = GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx());
    const std::string &name = symbol->GetName();
    MIRSymKind st = symbol->sKind;
    MIRStorageClass sc = symbol->storageClass;

    if (kDebugGenGlobalRootList) {
      fprintf(stderr, "Considering symbol %s: %d %d\n", name.c_str(), st, sc);
    }

    if (!(st == kStVar && sc == kScGlobal)) {
      if (kDebugGenGlobalRootList) {
        fprintf(stderr, "  Skipped. Not var or not global.\n");
      }
      continue;
    }

    TyIdx tyIdx = symbol->GetTyIdx();

    MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
    PrimType pty = type->GetPrimType();

    if (!(pty == PTY_ptr || pty == PTY_ref)) {
      if (kDebugGenGlobalRootList) {
        fprintf(stderr, "  Skipped. Not ptr or ref. pty: %d\n", pty);
      }
      continue;
    }

    // It is a pointer/ref type.  Check its pointed type.

    MIRPtrType *pointType = dynamic_cast<MIRPtrType *>(type);
    if (pty == PTY_ptr) {
      if (pointType == nullptr) {
        if (kDebugGenGlobalRootList) {
          fprintf(stderr, "  Skipped. Is a primitive type ptr, may be used by __NativeFunc_xxxx__.\n");
        }
        continue;
      }

      MIRType *pointedType = pointType->GetPointedType();

      if (!(pointedType->GetKind() == kTypeClass)) {
        if (kDebugGenGlobalRootList) {
          fprintf(stderr, "  Skipped. Not pointing to class.\n");
        }
        continue;
      }
    }

    // Now it is a pointer/ref to a class.  Record it for GC scanning.
  }

  fclose(outputFile);
}

void CG::GenPrimordialObjectList(const std::string &outputBasename) {
  if (!cgopt_.GenPrimorList()) {
    return;
  }

  std::string outputFilename = outputBasename + kPrimorListSuffix;
  FILE *outputFile = fopen(outputFilename.c_str(), "w");
  CHECK_FATAL(outputFile != nullptr, "open file failed in CG::GenPrimordialObjectList");

  for (StIdx stIdx : mirModule->symbolSet) {
    MIRSymbol *symbol = GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx());

    if (symbol->IsPrimordialObject()) {
      const std::string &name = symbol->GetName();
      fprintf(outputFile, "%s\n", name.c_str());
    }
  }

  fclose(outputFile);
}


#if DEBUG
MapleMap<MIRFunction *, MapleMap<PregIdx, StIdx> *> *funcs_to_pregs_to_vars_map;
#endif

void CG::AddStackGuardvar() {
  MIRSymbol *chkguard = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  chkguard->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(std::string("__stack_chk_guard")));
  chkguard->storageClass = kScExtern;
  chkguard->sKind = kStVar;
  CHECK_FATAL(GlobalTables::GetTypeTable().typeTable.size() > PTY_u64, "out of vector range");
  chkguard->SetTyIdx(GlobalTables::GetTypeTable().typeTable[PTY_u64]->tyIdx);
  GlobalTables::GetGsymTable().AddToStringSymbolMap(chkguard);

  MIRSymbol *chkfunc = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  chkfunc->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(std::string("__stack_chk_fail")));
  chkfunc->storageClass = kScText;
  chkfunc->sKind = kStFunc;
  GlobalTables::GetGsymTable().AddToStringSymbolMap(chkfunc);
}

void CG::LowerIR() {
  BECommon &becommon = *g->becommon;
  becommon.optim_level = cgopt_.optim_level;

  MIRLower mirlowerer(*mirModule, nullptr);
  CGLowerer thelowerer(*mirModule, becommon, cgopt_.GenerateExceptionHandlingCode(), cgopt_.GenerateVerboseAsm(),
#if DEBUG
                       ConvertLocalsToPRegs(),
#endif
                       cgopt_.optim_level);
  thelowerer.RegisterBuiltIns();
  thelowerer.RegisterExternalLibraryFunctions();
  thelowerer.checkloadstore = CGOptions::checkarraystore;

  if (AddStackGuard()) {
    AddStackGuardvar();
  }

  maple::ConstantFold cf(mirModule);
  for (MIRFunction *mirFunc : mirModule->functionList) {
    if (mirFunc->body == nullptr) {
      continue;
    }

    mirModule->SetCurFunction(mirFunc);

    if (cgopt_.DoConstFold()) {
      cf.Simplify(mirFunc->body);
    }

    // if mapleme not run, needs extra lowering
    if (mirModule->flavor <= kFeProduced) {
      mirlowerer.SetLowerCG();
      mirlowerer.LowerFunc(mirFunc);
    }

    if (CGOptions::printLowerIR) {
      printf("************* before CGLowerer **************\n");
      mirFunc->Dump();
    }

    thelowerer.LowerFunc(mirFunc);
    thelowerer.InsertExit(mirFunc);

    if (CGOptions::printLowerIR) {
      printf("************* after  CGLowerer **************\n");
      mirFunc->Dump();
      printf("************* end    CGLowerer **************\n");
    }
  }

#if DEBUG
  if (ConvertLocalsToPRegs()) {
    funcs_to_pregs_to_vars_map = thelowerer.GetPregsToLocalVarsMap();
  }
#endif
}

void CG::GenerateObjectMaps(BECommon &becommon) {
  cerr << "ERROR: Object maps are not supported on this platform!" << endl;
}

std::string CG::FindGCTIBPatternName(const std::string &name) {
  cerr << "ERROR: GCTIB pattern merging are not supported on this platform!" << endl;
  return std::string();
}

void CG::SetInstrumentationFunction(const std::string name) {
  instru_func_ = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  instru_func_->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(std::string("__").append(name).append("__")));
  instru_func_->storageClass = kScText;
  instru_func_->sKind = kStFunc;
}

#define DBG_TRACE_ENTER mpl_dt_enter
#define DBG_TRACE_EXIT mpl_dt_exit
#define XSTR(s) str(s)
#define str(s) #s

void CG::DefineDebugTraceFunctions() {
  dbg_trace_enter_ = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  dbg_trace_enter_->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(std::string("__" XSTR(DBG_TRACE_ENTER) "__")));
  dbg_trace_enter_->storageClass = kScText;
  dbg_trace_enter_->sKind = kStFunc;

  dbg_trace_exit_ = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  dbg_trace_exit_->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(std::string("__" XSTR(DBG_TRACE_EXIT) "__")));
  dbg_trace_exit_->storageClass = kScText;
  dbg_trace_exit_->sKind = kStFunc;

  dbg_func_profile_ = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  dbg_func_profile_->SetNameStridx(
    GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(std::string("__" XSTR(mpl_func_profile) "__")));
  dbg_func_profile_->storageClass = kScText;
  dbg_func_profile_->sKind = kStFunc;
}

// Add the fields of cur_structty to the result. Used to handle recursive
// structures.
static void AppendReferenceOffsets64(BECommon &beCommon, MIRStructType *curStructty, int64_t &curOffset,
                                     vector<int64_t> &result) {
  /* We are going to reimplement BECommon::GetFieldOffset so that we can do
   * this in one pass through all fields.
   *
   * The tricky part is to make sure the object layout described here is
   * compatible with the rest of the system.  This implies that we need
   * something like a "Maple ABI" documented for each platform.
   */
  if (curStructty->typeKind == kTypeClass) {
    auto curClassty = static_cast<MIRClassType *>(curStructty);
    auto maybeParent = GlobalTables::GetTypeTable().GetTypeFromTyIdx(curClassty->parentTyIdx);

    if (maybeParent != nullptr) {
      auto parentClassty = dynamic_cast<MIRClassType *>(maybeParent);
      // CG_ASSERT(parent_classty != nullptr, "Error: class's parent is not class");
      if (parentClassty != nullptr) {
        AppendReferenceOffsets64(beCommon, parentClassty, curOffset, result);
      } else {
        printf("WARNING:: generating objmap for incomplete class\n");
      }
    }
  }

  for (auto &fieldPair : curStructty->fields) {
    auto fieldNameIdx = fieldPair.first;
    auto fieldTypeIdx = fieldPair.second.first;

    auto &fieldName = GlobalTables::GetStrTable().GetStringFromStrIdx(fieldNameIdx);
    auto fieldType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldTypeIdx);
    auto &fieldTypeName = GlobalTables::GetStrTable().GetStringFromStrIdx(fieldType->nameStrIdx);
    auto fieldTypeKind = fieldType->GetKind();

    auto fieldSize = beCommon.type_size_table[fieldTypeIdx.GetIdx()];
    auto fieldAlign = beCommon.type_align_table[fieldTypeIdx.GetIdx()];
    int64_t myOffset = RoundUp(curOffset, fieldAlign);
    int64_t nextOffset = myOffset + fieldSize;

    if (!CGOptions::quiet) {
      printf("    field: %s\n", fieldName.c_str());
      printf("      type: %" PRIu32 ": %s\n", fieldTypeIdx.GetIdx(), fieldTypeName.c_str());
      printf("      type kind: %d\n", fieldTypeKind);
      printf("      size: %" PRIu64 "\n", fieldSize);
      printf("      align: %" PRIu8 "\n", fieldAlign);
      printf("      field offset: %" PRIu64 "\n", myOffset);
    }

    if (fieldTypeKind == kTypePointer) {
      if (!CGOptions::quiet) {
        printf("      ** Is a pointer field.\n");
      }
      result.push_back(myOffset);
    }

    if (fieldTypeKind == kTypeArray || fieldTypeKind == kTypeStruct || fieldTypeKind == kTypeClass ||
        fieldTypeKind == kTypeInterface) {
      if (!CGOptions::quiet)
        printf(
          "    ** ERROR: We are not expecting nested aggregate type. "
          "All Java classes are flat -- no nested structs. "
          "Please extend me if we are going to work with non-java languages.\n");
    }

    curOffset = nextOffset;
  }
}

// Return a list of offsets of reference fields.
vector<int64_t> CG::GetReferenceOffsets64(BECommon &beCommon, MIRStructType *structty) {
  vector<int64_t> result;
  // java class layout has already been done in previous phase.
  if (structty->typeKind == kTypeClass) {
    for (auto fieldInfo : beCommon.GetJClassLayout(static_cast<MIRClassType *>(structty))) {
      if (fieldInfo.is_ref) {
        result.push_back((int64_t)fieldInfo.offset);
      }
    }
  } else if (structty->typeKind != kTypeInterface) {  // interface doesn't have reference fields
    int64_t curOffset = 0;
    AppendReferenceOffsets64(beCommon, structty, curOffset, result);
  }

  return result;
}

}  // namespace maplebe
