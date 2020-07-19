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

#include "mpl2mplexe.h"
#include "module_phase_manager.h"
#include "inline.h"
#include "interleaved_manager.h"
#include "constant_fold.h"
#include "mir_function.h"

#define ADD_IF_NOT_SKIPPED(vec, str)        \
  if (Options::skipPhase.compare(str) != 0) \
    vec.push_back(std::string(str));

int Mpl2MplExecutable::Run(Options meoption, MemPool *optmp) {
  // InterleavedManger can call either module or function level phases
  InterleavedManager mgr(optmp, &module);
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
  maple::ConstantFold cf(&module);
  for (MIRFunction *mirFunc : module.functionList) {
    if (mirFunc->body != nullptr) {
      module.SetCurFunction(mirFunc);
      cf.Simplify(mirFunc->body);
    }
  }
  return 0;
}

void Mpl2MplExecutable::EmitParseWarning() {
  if (!Options::quiet && parser->GetWarning().size()) {
    parser->EmitWarning(module.GetFileName());
  }
}

void Mpl2MplExecutable::EmitOutput() {
  std::string outputfile;
  std::string::size_type lastdot = filename.find_last_of(".");
  if (lastdot == std::string::npos) {
    outputfile = filename.append(".");
  } else {
    outputfile = filename.substr(0, lastdot).append(".");
  }
  outputfile.append("VtableImpl.mpl");
  bool isFirstFunc = true;
  for (MIRFunction *mirFunc : module.functionList) {
    mirFunc->Emit(outputfile, isFirstFunc);
    isFirstFunc = false;
  }
}
