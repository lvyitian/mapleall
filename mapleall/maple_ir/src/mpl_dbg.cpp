/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
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

#include "mir_parser.h"
#include "debug_info.h"
#include "bin_mplt.h"
#include "opcode_info.h"
#include <cstdlib>
#include "mir_function.h"
#include "constant_fold.h"
#include "mir_type.h"
#include <iostream>
#include <fstream>

using namespace maple;

std::unordered_set<std::string> dumpFuncSet = {};

bool ConstantFoldModule(maple::MIRModule *module) {
  maple::ConstantFold cf(module);
  MapleVector<maple::MIRFunction *> &funcList = module->functionList;
  for (MapleVector<maple::MIRFunction *>::iterator it = funcList.begin(); it != funcList.end(); it++) {
    maple::MIRFunction *curfun = *it;
    maple::BlockNode *block = curfun->body;
    module->SetCurFunction(curfun);
    if (!block) {
      continue;
    }
    cf.Simplify(block);
  }
  return true;
}

int main(int argc, char **argv) {
  if (argc < 2) {
    MIR_PRINTF("usage: mpldbg foo.mpl\n");
    exit(1);
  }
  std::vector<maple::MIRModule *> themodule(argc, nullptr);
  bool useBinary = false;
  MIRSrcLang srcLang = kSrcLangUnknown;
  // process the options which must come first
  maple::int32 i = 1;
  while (argv[i][0] == '-' ) {
    if (argv[i][1] == 'b' && argv[i][2] == '\0') {
      useBinary = true;
    } else if (strncmp(argv[i], "-dumpfunc=", 10) == 0 && strlen(argv[i]) > 10) {
      std::string funcName(&argv[i][10]);
      dumpFuncSet.insert(funcName);
    } else if (strcmp(argv[i], "-srclang=java") == 0 ) {
      srcLang = kSrcLangJava;
    } else if (strcmp(argv[i], "-srclang=c") == 0 ) {
      srcLang = kSrcLangC;
    } else if (strcmp(argv[i], "-srclang=c++") == 0 ) {
      srcLang = kSrcLangCPlusPlus;
    } else {
      ERR(kLncErr, "mpldbg: unrecognized command line option");
      return 1;
    }
    i++;
  }
  // process the input files
  while (i < argc) {
    themodule[i] = new maple::MIRModule(argv[i]);
    themodule[i]->srcLang = srcLang;
    std::string::size_type lastdot = themodule[i]->fileName.find_last_of(".");
    bool ismplt = themodule[i]->fileName.compare(lastdot, 5, ".mplt") == 0;
    bool istmpl = themodule[i]->fileName.compare(lastdot, 5, ".tmpl") == 0;
    bool ismpl = themodule[i]->fileName.compare(lastdot, 5, ".mpl\0") == 0;
    bool isbpl = themodule[i]->fileName.compare(lastdot, 5, ".bpl\0") == 0;
    if (!ismplt && !istmpl && !ismpl && !isbpl) {
      ERR(kLncErr, "mpldbg: input must be .mplt or .mpl or .bpl or .tmpl file");
      return 1;
    }
    // input the file
    if (ismpl || istmpl) {
      maple::MIRParser theparser(*themodule[i]);
      if (!theparser.ParseMIR()) {
        theparser.EmitError(themodule[i]->fileName.c_str());
        return 1;
      }
    } else {
      BinaryMplImport binMplt(*themodule[i]);
      binMplt.imported = false;
      std::string modid = themodule[i]->fileName;
      if (!binMplt.Import(modid, true)) {
        ERR(kLncErr, "mpldbg: cannot open .mplt or .bpl file: %s", modid.c_str());
        return 1;
      }
    }

    themodule[i]->dbgInfo->BuildDebugInfo();

    // output the file
    if (!useBinary) {
      themodule[i]->OutputAsciiMpl(".dbg", (ismpl || isbpl) ? ".mpl" : ".tmpl", &dumpFuncSet, true, false);
    } else {
      BinaryMplt binMplt(*themodule[i]);
      std::string modid = themodule[i]->fileName;
      binMplt.GetBinExport().not2mplt = ismpl || isbpl;
      std::string filestem = modid.substr(0, lastdot);
      binMplt.Export(filestem + ((ismpl || isbpl) ? ".dbg.bpl" : ".dbg.mplt"), &dumpFuncSet);
    }
    i++;
  }
  return 0;
}
