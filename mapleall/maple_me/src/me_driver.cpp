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

#include <cstdlib>
#include "mir_parser.h"
#include "bin_mpl_import.h"
#include "opcode_info.h"
#include "me_option.h"
#include "mir_function.h"
#include "me_function.h"
#include "me_phase_manager.h"
#include "opcode_info.h"
#include "module_phase_manager.h"
#include "mpl_timer.h"

using namespace maple;

int main(int argc, char **argv) {
  MPLTimer timer;
  timer.Start();
  // new me global mempool
  MemPool *optmp = mempoolctrler.NewMemPool("mapleme mempool");
  MemPool *modulemp = mempoolctrler.NewMemPool("imodulephase mempool");
  // parse command line
  std::string filename;
  MeOption meoption(*optmp);
  meoption.ParseOptions(argc, argv, filename);
  MIRModule themodule(filename.c_str());

  // invalid command line
  if (filename.size() == 0) {
    return -1;
  }

  // Do nothing unless opt level is non-zero.
  if (MeOption::optLevel == 0) {
    return 0;
  }

  std::string fileName(filename);
  std::string::size_type lastdot = fileName.find_last_of(".");
  bool isbpl = fileName.compare(lastdot, 5, ".bpl\0") == 0;

  // new functional phase manager
  MeFuncPhaseManager fpm(optmp, &themodule);
  fpm.RegisterFuncPhases();  // scan me_phases.def and create phase object

  MIRParser theparser(themodule);
  if (!isbpl) {
    if (!theparser.ParseMIR(0, MeOption::parserOpt)) {
      theparser.EmitError(fileName);
      return 1;
    }
  } else {
    BinaryMplImport binMplt(themodule);
    binMplt.imported = false;
    if (!binMplt.Import(fileName, true)) {
      FATAL(kLncFatal, "cannot open .bpl file: %s", fileName.c_str());
      return 1;
    }
  }

  ModulePhaseManager mpm(modulemp, &themodule);
  mpm.RegisterModulePhases();
  vector<string> modphases;
  modphases.push_back(string("classhierarchy"));
  mpm.AddModulePhases(modphases);
  mpm.Run();
  fpm.SetModResMgr(mpm.GetModResultMgr());
  if (!isbpl && MeOption::parserOpt & kCheckCompleteType) {
    theparser.EmitIncompleteTypes();
  }
  unsigned long rangeNum = 0;
  if (!MeOption::quiet)
    LogInfo::MapleLogger() << "---Functions range [0, " << themodule.functionList.size() - 1 << "]"
         << " in file " << fileName << endl;

  // generate phase pipeline
  fpm.AddPhases(meoption.GetSkipPhases());
  for (MapleVector<MIRFunction *>::iterator it = themodule.functionList.begin();
       it != themodule.functionList.end(); it++, rangeNum++) {
    MIRFunction *func = *it;

    if (MeOption::useRange && (rangeNum < MeOption::range[0] || rangeNum > MeOption::range[1])) {
      continue;
    }
    if (func->body == nullptr) {
      continue;
    }
    themodule.SetCurFunction(func);
    // lower, create BB and build cfg
    fpm.Run(func, rangeNum, fileName);
    fpm.GetAnalysisResultManager()->InvalidAllResults();
  }
  themodule.OutputAsciiMpl(".me", ".mpl");

  // delete mempool
  mempoolctrler.DeleteMemPool(optmp);
  mempoolctrler.DeleteMemPool(modulemp);
  LogInfo::MapleLogger() << "me consumes " << timer.Elapsed() << " seconds" << endl;
  return 0;
}
