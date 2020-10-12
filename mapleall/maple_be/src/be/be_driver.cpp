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

#include "be_common.h"
#include "mir_lower.h"
#include "constant_fold.h"
#if TARGVM
#include "mmpl_lowerer.h"
#include "mmpl_mem_layout.h"
#ifndef OUTPUTMMPL
#include "cmpl_generator.h"
#endif
#else
#include "cg_lowerer.h"
#endif
#include "mir_parser.h"
#include <stdlib.h>

using namespace maplebe;


int main(int argc, char **argv) {
  maple::MIRModule themodule(argv[1]);
  if (argc != 2) {
    fprintf(stderr, "usage ./mplbe <mpl file>\n");
    exit(1);
  }
  std::string fileName(argv[1]);

  maple::MIRParser theparser(themodule);

  if (theparser.ParseMIR()) {
    if (theparser.GetWarning().size()) {
      theparser.EmitWarning(fileName);
    }

    BECommon becommon(themodule);
#if TARGVM
    GlobalMemLayout *globmemlayout = themodule.memPool->New<GlobalMemLayout>(becommon, &themodule.memPoolAllocator);
#endif
    maple::ConstantFold cf(&themodule);
    for (MapleVector<MIRFunction *>::iterator it = themodule.functionList.begin();
         it != themodule.functionList.end(); it++) {
      MIRFunction *mirFunc = *it;
      if (mirFunc->body == nullptr) {
        continue;
      }
      themodule.SetCurFunction(mirFunc);
      cf.Simplify(mirFunc->body);

      // if mapleme not run, needs extra lowering
      if (themodule.flavor == kFeProduced) {
        MIRLower mirlowerer(themodule, mirFunc);
        mirlowerer.SetLowerBE();
        mirlowerer.LowerFunc(mirFunc);
      }

#if TARGVM
      MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(mirFunc->stIdx.Idx());
      MemPool *funcMp = mempoolctrler.NewMemPool(funcSt->GetName().c_str());
      MapleAllocator funcscopeAllocator(funcMp);
      MmplMemLayout *thememlayout = funcMp->New<MmplMemLayout>(becommon, mirFunc, &funcscopeAllocator);
      thememlayout->LayoutStackFrame();
      MmplLowerer lowerer2(themodule, becommon, mirFunc, globmemlayout, thememlayout);
      lowerer2.LowerFunc(mirFunc);
      mirFunc->frameSize = thememlayout->StackFrameSize();
      mirFunc->upFormalSize = thememlayout->UpformalSize();
      mempoolctrler.DeleteMemPool(funcMp);
#else
      CGLowerer thelowerer(themodule, becommon);
      if (mirFunc->body == nullptr) {
        continue;
      }
      thelowerer.LowerFunc(mirFunc);
#endif
    }
    themodule.flavor = kMmpl;
#if TARGVM
#ifdef OUTPUTMMPL
    themodule.OutputAsciiMpl(".mmpl", "");
#else
    std::string::size_type lastdot = fileName.find_last_of(".");
    std::string filestem;
    if (lastdot == std::string::npos) {
      filestem = fileName;
    } else {
      filestem = fileName.substr(0, lastdot);
    }
    // OutputCmpl(themodule, filestem.c_str());
#endif
#else
    themodule.Dump();
#endif
    return 0;
  } else {
    theparser.EmitError(fileName);
    return 1;
  }
}
