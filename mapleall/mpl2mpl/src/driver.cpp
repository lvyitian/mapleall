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

/* This driver only run ipa phases */
#include <cstdlib>
#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include "mir_parser.h"
#include "option.h"
#include "opcode_info.h"
#include "mpl2mplexe.h"

#include "opcode_info.h"
#include "mir_function.h"
#include "constant_fold.h"
#include "module_phase_manager.h"
#include "inline.h"
#include "interleaved_manager.h"
#include "muid.h"
#include "bin_mpl_import.h"

#if 1
#define ADD_IF_NOT_SKIPPED(vec, str)        \
  if (Options::skipPhase.compare(str) != 0) \
    vec.push_back(std::string(str));
#endif

#if 1
int main(int argc, char **argv) {
  MemPoolCtrler mpc;
  // new me global mempool
  MemPool *modulemp = mpc.NewMemPool("mpl2mpl mempool");
  std::string filename;
  Options option(*modulemp);
  option.ParseOptions(argc, argv, filename);
  std::string::size_type lastdot = filename.find_last_of(".");
  bool isbpl = filename.compare(lastdot, 5, ".bpl\0") == 0;
  MIRModule mirModule(filename.c_str());
  // generate the md5
  {
    ifstream infile;
    std::string ret;
    infile.open(filename.c_str());
    if (infile.fail()) {
      return 0;
    }
    infile.seekg(0, std::ios::end);
    ret.reserve(infile.tellg());
    infile.seekg(0, std::ios::beg);
    ret.assign((std::istreambuf_iterator<char>(infile)), (std::istreambuf_iterator<char>()));
    MUID_t mplMd5 = GetMUID(ret);
    mirModule.SetMplMd5(mplMd5);
    infile.close();
  }
  if (!isbpl) {
    maple::MIRParser theparser(mirModule);
    if (!theparser.ParseMIR(0, option.parserOpt)) {
      theparser.EmitError(mirModule.GetFileName());
      mpc.DeleteMemPool(modulemp);
      return 1;
    }
    if (option.parserOpt & kCheckCompleteType) {
      theparser.EmitIncompleteTypes();
    }
    if (!Options::quiet && theparser.GetWarning().size()) {
      theparser.EmitWarning(mirModule.GetFileName());
    }
  } else {
    BinaryMplImport binMplt(mirModule);
    binMplt.imported = false;
    if (!binMplt.Import(filename, true)) {
      FATAL(kLncFatal, "cannot open .bpl file: %s", filename.c_str());
      return 1;
    }
  }

  // InterleavedManger can call either module or function level phases
  InterleavedManager mgr(modulemp, &mirModule);
  std::vector<std::string> modphases;
  ADD_IF_NOT_SKIPPED(modphases, "classhierarchy");
  ADD_IF_NOT_SKIPPED(modphases, "ipodevirtulize");
  ADD_IF_NOT_SKIPPED(modphases, "callgraph");
  ADD_IF_NOT_SKIPPED(modphases, "vtableanalysis");
  ADD_IF_NOT_SKIPPED(modphases, "reflectionanalysis");
  // GenNativeStubFunc and clinit need classinfo in reflectionanalysis
  ADD_IF_NOT_SKIPPED(modphases, "GenNativeStubFunc");
  // clinit has to be done after GenNativeStubFunc generates stub function body
  ADD_IF_NOT_SKIPPED(modphases, "clinit");
  if (Options::inlineLev >= 0) {
    if (Options::inlineLev == 3) {
      ADD_IF_NOT_SKIPPED(modphases, "greedyinline");
    } else {
      MInline::level = Options::inlineLev;
      ADD_IF_NOT_SKIPPED(modphases, "inline");
    }
  }
  ADD_IF_NOT_SKIPPED(modphases, "VtableImpl");
  ADD_IF_NOT_SKIPPED(modphases, "javaehlower");
  ADD_IF_NOT_SKIPPED(modphases, "JavaLowering");
  if (Options::deferralBarrier) {
    ADD_IF_NOT_SKIPPED(modphases, "deferralBarrier");
  } else if (Options::barrier) {
    ADD_IF_NOT_SKIPPED(modphases, "barrierinsertion");
  }
  if (Options::mapleLinker) {
    ADD_IF_NOT_SKIPPED(modphases, "MUIDReplacement");
  }
  mgr.AddPhases(modphases, true /* isModulePhase */ );
  mgr.Run();

  // emit after module phase
  maple::ConstantFold cf(&mirModule);
  std::string outputfile;
  if (lastdot == std::string::npos) {
    outputfile = filename.append(".");
  } else {
    outputfile = filename.substr(0, lastdot).append(".");
  }
  outputfile.append("VtableImpl.mpl");
  bool isFirstFunc = true;
  for (MIRFunction *mirFunc : mirModule.functionList) {
    if (mirFunc->body != nullptr) {
      mirModule.SetCurFunction(mirFunc);
      cf.Simplify(mirFunc->body);
    }
    mirFunc->Emit(outputfile, isFirstFunc);
    isFirstFunc = false;
  }

  // delete mempool
  mpc.DeleteMemPool(modulemp);
  return 0;
}

#endif
