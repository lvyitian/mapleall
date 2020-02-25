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

#include "mir_parser.h"
#include "opcode_info.h"
#include <cstdlib>
#include "mir_function.h"
#include "constant_fold.h"
#include "mir_type.h"
#include <iostream>
#include <fstream>

int main(int argc, char **argv) {
  if (argc != 2) {
    MIR_PRINTF("usage: mpldbg foo.mpl\n");
    exit(1);
  }
  maple::MIRModule *themodule = new maple::MIRModule(argv[1]);
  maple::MIRParser theparser(*themodule);
  if (theparser.ParseMIR(0, kWithDbgInfo | kCheckCompleteType)) {
    theparser.EmitWarning(themodule->fileName.c_str());
    themodule->OutputAsciiMpl(".dbg", ".mpl");
  } else {
    theparser.EmitError(themodule->fileName.c_str());
    delete themodule;
    return 1;
  }
  delete themodule;
  return 0;
}
