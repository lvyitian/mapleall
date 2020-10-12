/*
 * Copyright (c) [2019-2020] Huawei Technologies Co., Ltd. All rights reserved.
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
#include "driver_runner.h"
#include <iostream>
#include <typeinfo>
#include <sys/stat.h>
#include "mpl_timer.h"
#include "mir_function.h"
#include "mir_parser.h"
#include "bin_mplt.h"
#include "inline.h"

#if TARGX86
#include "x86/x86_cg.h"
#elif TARGX86_64
#error "X86_64 codegen is yet to be supported"
#elif TARGARM
#include "arm/arm_cg.h"
#elif TARGAARCH64
#include "aarch64/aarch64_cg.h"
#elif TARGARK
#include "ark/ark_mir_emit.h"
#include "ark/ark_cg.h"
#include "emit.h"
#else
#error "Unsupported target"
#endif

#include "cg.h"
#include "cg_option.h"
#include "cg_phase_manager.h"

#define JAVALANG (theModule->IsJavaModule())

#define CHECK_MODULE(errorCode...)                                              \
  do {                                                                          \
    if (theModule == nullptr) {                                                 \
      LogInfo::MapleLogger() << "Fatal error: the module is null" << '\n';      \
      return errorCode;                                                         \
    }                                                                           \
  } while (0)

#define RELEASE(pointer)      \
  do {                        \
    if (pointer != nullptr) { \
      delete pointer;         \
      pointer = nullptr;      \
    }                         \
  } while (0)

#define ADD_MODPREPHASE(name, condition)       \
  if ((condition) && Options::skipPhase.compare(name) != 0) { \
    modprephases.push_back(std::string(name)); \
  }

#define ADD_MEPHASE(name, condition)       \
  if ((condition) && meOptions->GetSkipPhases().find(name) == meOptions->GetSkipPhases().end()) { \
    mephases.push_back(std::string(name)); \
  }

#define ADD_MODPOSTPHASE(name, condition)       \
  if ((condition) && Options::skipPhase.compare(name) != 0) { \
    modpostphases.push_back(std::string(name)); \
  }

#define ADD_EXTRA_PHASE(name, timephases, timeStart)                                                    \
  if (timephases) {                                                                                     \
    auto duration = std::chrono::system_clock::now() - (timeStart);                                       \
    extraPhasesTime.push_back(std::chrono::duration_cast<std::chrono::microseconds>(duration).count()); \
    extraPhasesName.push_back(name);                                                                    \
  }

namespace maplebe { // Required global structures
Globals *g;
std::vector<std::string> ehExclusiveFunctionName;  // we don't do exception handling in this list
}  // namespace maplebe

using namespace maplebe;

namespace maple {
const std::string kMpl2mpl = "mpl2mpl";
const std::string kMplMe = "me";
const std::string kMplCg = "mplcg";

enum OptLevel {
  kLevelO0,
  kLevelO1,
  kLevelO2
};

ErrorCode DriverRunner::Run() {
  CHECK_MODULE(kErrorExit);

  if (exeNames.empty()) {
    LogInfo::MapleLogger() << "Fatal error: no exe specified" << '\n';
    return kErrorExit;
  }

  printOutExe = exeNames[exeNames.size() - 1];

  // Prepare output file
  auto lastDot = actualInput.find_last_of(".");
  std::string baseName = (lastDot == std::string::npos) ? actualInput : actualInput.substr(0, lastDot);
  std::string originBaseName = baseName;
  std::string outputFile = baseName.append(GetPostfix());

  ErrorCode ret = ParseInput(outputFile, originBaseName);
  if (ret != kErrorNoError) {
    return kErrorExit;
  }
  if (mpl2mplOptions != nullptr || meOptions != nullptr || cgOptions != nullptr) {
    if (HasThisPhase(kMpl2mpl)) {
      originBaseName.append(".VtableImpl");
    }
    std::string vtableImplFile = originBaseName;
    if (!HasThisPhase(kMpl2mpl) && HasThisPhase(kMplMe)) {
      vtableImplFile.append(".me");
    }
    vtableImplFile.append(".mpl");
    std::string dotSFile = originBaseName;
    dotSFile.append(".s");
    ProcessMpl2mplAndMeAndMplCgPhases(vtableImplFile, dotSFile, originBaseName);
  }
  return kErrorNoError;
}

bool DriverRunner::FuncOrderLessThan(const MIRFunction *left, const MIRFunction *right) {
  return left->GetLayoutType() < right->GetLayoutType();
}

bool DriverRunner::IsFramework() const {
  return false;
}

std::string DriverRunner::GetPostfix() const {
  if (printOutExe == kMplMe) {
    return ".me.mpl";
  }
  if (printOutExe == kMpl2mpl) {
    return ".VtableImpl.mpl";
  }
  return "";
}

ErrorCode DriverRunner::ParseInput(const std::string &outputFile, const std::string &originBaseName) const {
  CHECK_MODULE(kErrorExit);

  LogInfo::MapleLogger() << "Starting parse input" << '\n';
  MPLTimer timer;
  timer.Start();

  ErrorCode ret = kErrorNoError;
  if (inputFileType == kMpl) {
    MIRParser parser(*theModule);
    bool parsed = parser.ParseMIR(0, 0, false, true);
    if (!parsed) {
      ret = kErrorExit;
      parser.EmitError(outputFile);
    }
  } else {
    BinaryMplImport binMplt(*theModule);
    binMplt.imported = false;
    std::string modid = theModule->fileName;
    if (!binMplt.Import(modid, true)) {
      ret = kErrorExit;
      LogInfo::MapleLogger() << "Cannot open .bpl file: %s" << modid << '\n';
    }
  }
  timer.Stop();
  LogInfo::MapleLogger() << "Input consumed " << timer.Elapsed() << "s" << '\n';

  return ret;
}

bool DriverRunner::HasThisPhase(std::string phaseName) const {
  for (std::string exeName : exeNames) {
    if (exeName == phaseName) {
      return true;
    }
  }
  return false;
}

void DriverRunner::ProcessMpl2mplAndMeAndMplCgPhases(const std::string &interimOutputFile, const std::string &outputFile, const std::string &origBaseName) const {
  CHECK_MODULE();

  LogInfo::MapleLogger() << "Processing mpl2mpl&mplme" << '\n';
  MPLTimer timer;
  timer.Start();

  InterleavedManager mgr(optMp, theModule, meInput, timePhases);
  std::vector<std::string> modprephases;
  std::vector<std::string> mephases;
  std::vector<std::string> modpostphases;
#include "phases.def"
  MInline::level = Options::inlineLev;
  MInline::inlineFuncList = MeOption::inlinefunclist;
//InitPhases(mgr, phases);
  mgr.AddPhases(modprephases, true /* isModulePhase */, timePhases);
  if (meOptions) {
    mgr.AddPhases(mephases, false, timePhases, genMeMpl);
  }
  mgr.AddPhases(modpostphases, true, timePhases);
  mgr.Run();

  // emit after module phase
  if (printOutExe == kMpl2mpl || printOutExe == kMplMe || genVtableImpl || Options::emitVtableImpl) {
    theModule->Emit(interimOutputFile);
  }

  timer.Stop();
  LogInfo::MapleLogger() << "Mpl2mpl&mplme consumed " << timer.Elapsed() << "s" << '\n';
  if (!HasThisPhase(kMplCg)) {
    return;
  }

  LogInfo::MapleLogger() << "Processing mplcg" << '\n';
  timer.Start();

  if (cgOptions) {
    g = new Globals(&maple::kOpcodeInfo, &GlobalTables::GetGlobalTables());
    g->optim_level = cgOptions->optim_level;
    cgOptions->SetDefaultOptions(theModule);
    /* new functional phase manager */
    CgFuncPhaseManager cgfpm(optMp, theModule);
    cgfpm.RegisterFuncPhases(); /* scan cg_phases.def and create phase object */
    cgfpm.SetCgPhase(kCgPhaseMainopt);
    if (timePhases) {
      CGOptions::timePhases = true;
    }
#if TARGX86
    X86CG thecg(*cgOptions, cgOptions->run_cg_flag, outputFile.c_str());
#elif TARGARM
    ArmCG thecg(*cgOptions, cgOptions->run_cg_flag, outputFile.c_str());
#elif TARGAARCH64
    AArch64CG thecg(theModule, *cgOptions, cgOptions->run_cg_flag, outputFile.c_str(), ehExclusiveFunctionName,
                    CGOptions::cyclePatternMap);
#elif TARGARK
    ArkCG thecg(theModule, *cgOptions, cgOptions->run_cg_flag, outputFile.c_str(), ehExclusiveFunctionName,
                    CGOptions::cyclePatternMap);
#endif

#ifdef MPLAD_CORTEX_A55
    g->mad = new A55MAD();
#endif // MPLAD_CORTEX_A55

    // Must be done before creating any BECommon instances.
    //
    // BECommon, when constructed, will calculate the type, size and align of
    // all types.  As a side effect, it will also lower ptr and ref types into
    // a64.  That will drop the information of what a ptr or ref points to.
    //
    // All metadata generation passes which depend on the pointed-to type must
    // be done here.
    {
      thecg.GenGlobalRootList(origBaseName);
      thecg.GenPrimordialObjectList(origBaseName);
    }

    // We initialize a couple of BECommon's tables using
    // the size information of GlobalTables.typeTable.
    // So, BECommon must be allocated after all the parsing is done
    // and user-defined types are all acounted.

    g->becommon = new BECommon(*theModule);

    // If a metadata generatoin pass depends on object layout it must be done
    // after creating BECommon.
    { thecg.GenExtraTypeMetadata(cgOptions->class_list_file, origBaseName); }

    if (thecg.NeedInsertInstrumentationFunction()) {
      CG_ASSERT(cgOptions->insert_call, "handling of --insert_call is not correct");
      thecg.SetInstrumentationFunction(cgOptions->instru_func);
    }

    if (cgOptions->run_cg_flag) {
      // 1. LowerIR.
      thecg.LowerIR();
      // 2. Generate the output file
      BECommon &becommon = *g->becommon;
      thecg.emitter_ = theModule->memPool->New<Emitter>(&thecg, outputFile);
      CG_ASSERT(thecg.emitter_, "");
      if (!cgOptions->SuppressFileInfo()) {
        thecg.emitter_->EmitFileInfo(actualInput);
      }

#if TARGARK
      MirGenerator *mirGen = new MirGenerator(*theModule, thecg.emitter_->out, *g->becommon);
      mirGen->OutputMIR(cgOptions->genMirMpl);
      thecg.emitter_->mirg_ = mirGen;

      // skip entry 0 (kOpUndef) and handle duplicate name (OP_dassign, OP_maydassign)
      thecg.emitter_->Emit("\nOP_dassign = 1\n");
      thecg.emitter_->Emit("OP_maydassign = 2\n");
      for (int i = 3; i < kREOpLast; ++i) {
        thecg.emitter_->Emit(string("OP_")+RE_OpName[i]+" = "+to_string(i)+"\n");
      }
#endif

      // 3. generate phase pipeline based on function.
      unsigned long rangeNum = 0;
      if (!CGOptions::quiet) {
        cout << "---Functions' range [0, " << theModule->functionList.size() - 1 << "]" << endl;
      }
      cgfpm.AddPhases(cgOptions->GetSequence());
      if (!cgOptions->profileData.empty()) {
        std::size_t pos;
        if ((pos = cgOptions->profileData.find(':')) != std::string::npos) {
          cgOptions->profileFuncData = cgOptions->profileData.substr(0, pos);
          cgOptions->profileClassData = cgOptions->profileData.substr(pos + 1);
        }
        cgOptions->profileFuncData = cgOptions->profileData;
        cout << "func profile " << cgOptions->profileFuncData << " class profile " << cgOptions->profileClassData
             << endl;
      }
      if (!cgOptions->profileFuncData.empty()) {
        std::ifstream funcProfileFile(cgOptions->profileFuncData);
        if (!funcProfileFile.is_open()) {
          ERR(kLncErr, " %s open failed!", cgOptions->profileFuncData.c_str());
        } else {
          std::string funcName;
          while (getline(funcProfileFile, funcName)) {
            if (!CGOptions::quiet) {
              std::cout << "sortString " << funcName << std::endl;
            }
            MIRFunction *sortFunction = theModule->mirBuilder->GetFunctionFromName(funcName);
            if (sortFunction) {
            }
          }
          std::sort(theModule->functionList.begin(), theModule->functionList.end(), FuncOrderLessThan);
        }
      }
      for (MapleVector<MIRFunction *>::iterator it = theModule->functionList.begin();
           it != theModule->functionList.end(); it++) {
        MIRFunction *mirFunc = *it;
        if (mirFunc->body == nullptr) {
          continue;
        }
        MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(mirFunc->stIdx.Idx());
        MemPool *funcMp = mempoolctrler.NewMemPool(funcSt->GetName().c_str());
        MapleAllocator funcscopeAllocator(funcMp);
        // 4, Create CGFunc
        CGFunc *cgfunc = thecg.CreateCGFunc(theModule, mirFunc, becommon, funcMp, &funcscopeAllocator);
        CG::curCgFunc = cgfunc;
        CG::curPuIdx = cgfunc->mirModule.CurFunction()->puIdx;
        // 5. Run the cg optimizations phases.
        if (CGOptions::useRange && (rangeNum >= CGOptions::range[0] && rangeNum <= CGOptions::range[1])) {
          CGOptions::inRange = true;
        }
        cgfpm.Run(cgfunc);
        cgfpm.Emit(cgfunc);

        thecg.emitter_->EmitLocalVariable(cgfunc);
        // 6. Invalid all analysis result.
        cgfpm.GetAnalysisResultManager()->InvalidIRbaseAnalysisResult(cgfunc);
        // 7, delete mempool.
        mempoolctrler.DeleteMemPool(funcMp);
        rangeNum++;
        CGOptions::inRange = false;
      }

#if TARGAARCH64
      // Emit duplicated asm func to delete plt call
      if (!cgOptions->duplicateAsmFile.empty()) {
        struct stat buffer;
        if (stat(cgOptions->duplicateAsmFile.c_str(), &buffer) == 0) {
          std::ifstream duplicateAsmFileFD(cgOptions->duplicateAsmFile);
          if (!duplicateAsmFileFD.is_open()) {
            ERR(kLncErr, " %s open failed!", cgOptions->duplicateAsmFile.c_str());
          } else {
            std::string contend;
            bool onlyForFramework = false;
            while (getline(duplicateAsmFileFD, contend)) {
              if (!strcmp(contend.c_str(), "#Libframework_start")) {
                onlyForFramework = true;
              }
              if (!strcmp(contend.c_str(), "#Libframework_end")) {
                onlyForFramework = false;
              }
              if (onlyForFramework) {
                continue;
              }
              thecg.emitter_->Emit(contend + "\n");
            }
          }
        }
      }
#endif
      // 8. generate object maps.
      if (cgOptions->gen_objmap) {
        thecg.GenerateObjectMaps(becommon);
      }
      // 9. emit global var
      thecg.emitter_->EmitGlobalVariable();
      if (JAVALANG) {
        thecg.emitter_->EmitMplPersonalityV0();
      } else if (theModule->srcLang == kSrcLangCPlusPlus) {
        thecg.emitter_->EmitGxxPersonalityV0();
        thecg.emitter_->EmitInitArraySection();
      }
      thecg.emitter_->CloseOutput();
    } else {
      cerr << "Skipped generating .s because -no-cg is given" << endl;
    }

    // release
    delete g->becommon;
    g->becommon = nullptr;
    delete g;
    g = nullptr;
  }

  timer.Stop();
  LogInfo::MapleLogger() << "MplCg consumed " << timer.Elapsed() << "s" << '\n';
}

}  // namespace maple
