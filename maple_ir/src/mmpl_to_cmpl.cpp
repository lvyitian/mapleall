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

#include "cmpl_generator.h"
#include "mir_type.h"
#include "securec.h"

int main(int argc, char **argv) {
  MIRModule themodule(argv[1]);

  if (argc < 2) {
    MIR_FATAL("using: ./gencmpl <mmpl file>\n");
  }

  MIR_INFO("genCMPL: translate kMmpl to kCmpl(v2)\n");
  CmplGenerator cmplGenerator;
  std::string mmplFile(argv[1]);
  MIRParser parser(themodule);
  if (parser.ParseMIR()) {
    ASSERT(themodule.flavor >= kMmpl, "");

    /* Prepare result file name.
       It changes the original file postfix (from .mmpl to .cmpl),
       and still with the same path (the same directory)
     */
    size_t mirNameLength = strlen(argv[1]);
    CHECK_FATAL(mirNameLength > 5, "malloc size is wrong");
    char *binmirName = static_cast<char *>(malloc(mirNameLength + 2));
    if (binmirName == nullptr){
      CHECK_FATAL(false, "binmirName malloc failed");
    }
    errno_t eNum = strcpy_s(binmirName, mirNameLength + 2, argv[1]);
    if (eNum) {
      CHECK_FATAL(false, "strcpy_s failed");
    }
    ASSERT(!strncmp((binmirName + (mirNameLength - 5)), ".mmpl", 5), "");
    binmirName[mirNameLength - 4] = 'c';  // from ".mmpl" to ".cmpl"
    CHECK_FATAL(mirNameLength - 4 > 0, "malloc size is wrong");
    char *idName = static_cast<char *>(malloc(mirNameLength - 4));
    if (idName == nullptr){
      CHECK_FATAL(false, "idName malloc failed");
    }
    eNum = memcpy_s(idName, (mirNameLength - 5) * sizeof(char), binmirName, (mirNameLength - 5) * sizeof(char));
    if (eNum) {
      CHECK_FATAL(false, "memcpy_s failed");
    }
    idName[mirNameLength - 5] = '\0';

    OutputCmpl(themodule, idName);
    free(static_cast<void *>(binmirName));
    free(static_cast<void *>(idName));
  } else {
    parser.EmitError(mmplFile);
    return 1;
  }
  return 0;
}
