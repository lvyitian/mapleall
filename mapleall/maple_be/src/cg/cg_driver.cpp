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
#if TARGX86
#include "x86_cg.h"
#elif TARGX86_64
#error "X86_64 codegen is yet to be supported"
#elif TARGARM
#include "arm_cg.h"
#elif TARGAARCH64
#include "aarch64_cg.h"
#elif TARGARK
#include "ark_mir_emit.h"
#include "ark_cg.h"
#include "emit.h"
#else
#error "Unsupported target"
#endif
#include "mpl_logging.h"
#include "mir_parser.h"
#include "bin_mpl_import.h"
#include "option_parser.h"
#include <sys/stat.h>

#include "mir_builder.h"
#include <algorithm>
#include <cstdio>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fstream>
#include <string>
#include "cg.h"
#include "cg_option.h"
#include "cg_phase_manager.h"
#include "special_func.h"
#ifdef MPLAD_CORTEX_A55
#include "a55mad.h"
#endif  // MPLAD_CORTEX_A55

using namespace maple;
using namespace maplebe;
using namespace std;
using namespace mapleOption;

namespace maplebe {
#define JAVALANG (themodule->IsJavaModule())

// Required global structures
Globals *g;
std::vector<std::string> ehExclusiveFunctionName;  // we don't do exception handling in this list
}  // namespace maplebe

static bool FuncOrderLessThan(const MIRFunction *left, const MIRFunction *right) {
  return left->layoutType < right->layoutType;
}

int main(int argc, char **argv) {
  std::string fileName;
  /* new me global mempool */
  MemPool *optmp = mempoolctrler.NewMemPool("maplecg mempool");
  CGOptions cgoption(*optmp);
  cgoption.ParseOptions(argc, argv, fileName);

  g = new Globals(&maple::kOpcodeInfo, &GlobalTables::GetGlobalTables());
  g->optim_level = cgoption.optim_level;
  MIRModule *themodule = new MIRModule(fileName.c_str());
  std::string::size_type lastdot = fileName.find_last_of(".");
  bool isbpl = fileName.compare(lastdot, 5, ".bpl\0") == 0;

  int nErr = 0;
  MIRParser *parser = new MIRParser(*themodule);
  if (!isbpl) {
    if (!parser->ParseMIR(0, cgoption.parserOpt)) {
      parser->EmitError(fileName);
      nErr = 1;
    }
  } else {
    BinaryMplImport binMplt(*themodule);
    binMplt.imported = false;
    if (!binMplt.Import(fileName, true)) {
      nErr = 1;
    }
  }

  if (nErr == 0) {
    if (cgoption.parserOpt & kCheckCompleteType) {
      parser->EmitIncompleteTypes();
    }
    if (!cgoption.DoItQuietly() && parser->GetWarning().size()) {
      parser->EmitWarning(fileName);
    }

    cgoption.SetDefaultOptions(themodule);
    /*new functional phase manager*/
    CgFuncPhaseManager fpm(optmp, themodule);
    fpm.RegisterFuncPhases(); /*scan cg_phases.def and create phase object */
    fpm.SetCgPhase(kCgPhaseMainopt);

    std::string::size_type lastdot = fileName.find_last_of(".");
    std::string output;
    std::string basename;
    if (lastdot == std::string::npos) {
      basename = fileName;
    } else {
      basename = fileName.substr(0, lastdot);
    }

    output = basename + ".s";

#if TARGX86
    X86CG thecg(themodule, cgoption, cgoption.run_cg_flag, output.c_str());
#elif TARGARM
    ArmCG thecg(themodule, cgoption, cgoption.run_cg_flag, output.c_str());
#elif TARGAARCH64
    AArch64CG thecg(themodule, cgoption, cgoption.run_cg_flag, output.c_str(), ehExclusiveFunctionName,
                    CGOptions::cyclePatternMap);
#elif TARGARK
    ArkCG thecg(themodule, cgoption, cgoption.run_cg_flag, output.c_str(), ehExclusiveFunctionName,
                    CGOptions::cyclePatternMap);
#endif

#ifdef MPLAD_CORTEX_A55
    g->mad = new A55MAD();
#endif  // MPLAD_CORTEX_A55

    // Must be done before creating any BECommon instances.
    //
    // BECommon, when constructed, will calculate the type, size and align of
    // all types.  As a side effect, it will also lower ptr and ref types into
    // a64.  That will drop the information of what a ptr or ref points to.
    //
    // All metadata generation passes which depend on the pointed-to type must
    // be done here.
    {
      thecg.GenGlobalRootList(basename);
      thecg.GenPrimordialObjectList(basename);
    }

    // We initialize a couple of BECommon's tables using
    // the size information of GlobalTables.typeTable.
    // So, BECommon must be allocated after all the parsing is done
    // and user-defined types are all acounted.
    g->becommon = new BECommon(*themodule);

    // If a metadata generatoin pass depends on object layout it must be done
    // after creating BECommon.
    { thecg.GenExtraTypeMetadata(cgoption.class_list_file, basename); }

    if (thecg.NeedInsertInstrumentationFunction()) {
      CG_ASSERT(cgoption.insert_call, "handling of --insert_call is not correct");
      thecg.SetInstrumentationFunction(cgoption.instru_func);
    }

    if (cgoption.run_cg_flag) {

      // 1. LowerIR.
      thecg.LowerIR();

      // 2. Generate the output file
      BECommon &becommon = *g->becommon;
      thecg.emitter_ = themodule->memPool->New<Emitter>(&thecg, output);
      CG_ASSERT(thecg.emitter_, "");
      if (!cgoption.SuppressFileInfo()) {
        thecg.emitter_->EmitFileInfo(fileName);
      }

#if TARGARK
      MirGenerator *mirGen = new MirGenerator(*themodule, thecg.emitter_->out, *g->becommon);
      mirGen->OutputMIR(cgoption.genMirMpl);
      thecg.emitter_->mirg_ = mirGen;

      // gen opcodes - skip entry 0 (kOpUndef) and handle duplicate name (OP_dassign, OP_maydassign)
      thecg.emitter_->Emit("\nOP_dassign = 1\n");
      thecg.emitter_->Emit("OP_maydassign = 2\n");
      for (int i = 3; i < kREOpLast; ++i) {
        thecg.emitter_->Emit(string("OP_")+RE_OpName[i]+" = "+to_string(i)+"\n");
      }

      // load profile info for class meta data - uses same binary metadata profile (meta.list) as mpl2mpl
      uint32 javaNameIdx = themodule->GetFileinfo(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("INFO_filename"));
      const std::string &javaName = GlobalTables::GetStrTable().GetStringFromStrIdx(GStrIdx(javaNameIdx));
      themodule->profile.DeCompress(CGOptions::classMetaProFile, javaName);
#endif

      // 3. generate phase pipeline based on function.
      unsigned long rangeNum = 0;
      if (!CGOptions::quiet) {
        LogInfo::MapleLogger() << "---Functions' range [0, " << themodule->functionList.size() - 1 << "]"
             << " in file " << fileName << endl;
      }
      fpm.AddPhases(cgoption.GetSequence());
      if (!cgoption.profileData.empty()) {
        std::size_t pos;
        if ((pos = cgoption.profileData.find(':')) != std::string::npos) {
          cgoption.profileFuncData = cgoption.profileData.substr(0, pos);
          cgoption.profileClassData = cgoption.profileData.substr(pos + 1);
        }
        cgoption.profileFuncData = cgoption.profileData;
        LogInfo::MapleLogger() << "func profile " << cgoption.profileFuncData << " class profile " << cgoption.profileClassData << endl;
      }

      if (!cgoption.profileFuncData.empty()) {
        std::ifstream funcProfileFile(cgoption.profileFuncData);
        if (!funcProfileFile.is_open()) {
          ERR(kLncErr, " %s open failed!", cgoption.profileFuncData.c_str());
        } else {
          std::string funcName;
          while (getline(funcProfileFile, funcName)) {
            if (!CGOptions::quiet) {
              LogInfo::MapleLogger() << "sortString " << funcName << std::endl;
            }
            MIRFunction *sortFunction = themodule->mirBuilder->GetFunctionFromName(funcName);
            if (sortFunction) {
            }
          }
          std::sort(themodule->functionList.begin(), themodule->functionList.end(), FuncOrderLessThan);
        }
      }
      for (MapleVector<MIRFunction *>::iterator it = themodule->functionList.begin();
           it != themodule->functionList.end(); it++) {
        MIRFunction *mirFunc = *it;
        if (mirFunc->body == nullptr) {
          continue;
        }
        MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(mirFunc->stIdx.Idx());
        MemPool *funcMp = mempoolctrler.NewMemPool(funcSt->GetName().c_str());
        MapleAllocator funcscopeAllocator(funcMp);
        // 4, Create CGFunc
        CGFunc *cgfunc = thecg.CreateCGFunc(themodule, mirFunc, becommon, funcMp, &funcscopeAllocator);
        CG::curCgFunc = cgfunc;
        CG::curPuIdx = cgfunc->mirModule.CurFunction()->puIdx;
        // 5. Run the cg optimizations phases.
        if (CGOptions::useRange && (rangeNum >= CGOptions::range[0] && rangeNum <= CGOptions::range[1])) {
          CGOptions::inRange = true;
        }
        fpm.Run(cgfunc);
        fpm.Emit(cgfunc);

        thecg.emitter_->EmitLocalVariable(cgfunc);
        // 6. Invalid all analysis result.
        fpm.GetAnalysisResultManager()->InvalidAllResults();

        // 7, delete mempool.
        mempoolctrler.DeleteMemPool(funcMp);
        rangeNum++;
        CGOptions::inRange = false;
      }

#if TARGAARCH64
      // Emit duplicated asm func to delete plt call
      if (!cgoption.duplicateAsmFile.empty()) {
        if (JAVALANG) {
          struct stat buffer;
          if (stat(cgoption.duplicateAsmFile.c_str(), &buffer) == 0) {
            std::ifstream duplicateAsmFileFD(cgoption.duplicateAsmFile);
            if (!duplicateAsmFileFD.is_open()) {
              ERR(kLncErr, " %s open failed!", cgoption.duplicateAsmFile.c_str());
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
      }
#endif

      // 8. generate object maps.
      if (cgoption.gen_objmap) {
        thecg.GenerateObjectMaps(becommon);
      }
      // 9. emit global var
      thecg.emitter_->EmitGlobalVariable();
      if (JAVALANG) {
        thecg.emitter_->EmitMplPersonalityV0();
      } else if (themodule->srcLang == kSrcLangCPlusPlus) {
        thecg.emitter_->EmitGxxPersonalityV0();
      }
      thecg.emitter_->CloseOutput();
    } else {
      cerr << "Skipped generating .s because -no-cg is given" << endl;
    }
  }

  // release
  delete g->becommon;
  delete parser;
  delete themodule;
  delete g;
  g = nullptr;
  return nErr;
}
