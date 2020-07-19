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

#include "mir_module.h"
#include "mir_parser.h"
#include <cstdlib>
#include "mir_function.h"
#include "constant_fold.h"
#include "bin_mpl_import.h"
#include <climits>
#include "name_mangler.h"

namespace maple {
enum TargetIdEnum { kArm32, kAarch64, kX86, kAmd64, kVm, kLastTargetId };

typedef struct {
  const char *name;
  int len;
  TargetIdEnum id;
} target_descr_t;

target_descr_t targetDescrs[kLastTargetId] = {
  { "aarch64", 7, kAarch64 }, { "arm", 3, kArm32 }, { "vm", 2, kVm }, { "x86_64", 6, kAmd64 }, { "x86", 3, kX86 },
};

TargetIdEnum target;

extern MIRModule *theModule;
bool dumpit;

bool VerifyModule(MIRModule *module) {
  bool res = true;
  MapleVector<MIRFunction *> &funcList = module->functionList;
  for (MapleVector<MIRFunction *>::iterator it = funcList.begin(); it != funcList.end(); it++) {
    MIRFunction *curfun = *it;
    BlockNode *block = curfun->body;
    module->SetCurFunction(curfun);
    if (dumpit) {
      curfun->Dump(false);
    }
    if (!block) {
      continue;
    }
    if (!block->Verify()) {
      res = false;
    }
  }
  return res;
}

}  // namespace maple

// verify a given java signature is consistent after encode-decode
int VerifyJavaNameEncoding(const char *sign) {
  std::string origSign(sign);
  std::string encodedSign = NameMangler::EncodeName(origSign.c_str());
  std::string decodedSign = NameMangler::DecodeName(encodedSign);

  if (decodedSign == origSign) {
    INFO(kLncInfo, "Test passed.");
    return 0;
  } else {
    INFO(kLncInfo, "Test failed:\n\tinput       : %s", sign);
    INFO(kLncInfo, "\n\tconvert-back: %s\n", decodedSign.c_str());
    return 1;
  }
}

enum VerfMode { kVerifyMir = 0, kVerifyJavaNames };

static void usage(const char *pgm) {
  INFO(kLncInfo, "usage: %s <mir name> [--dump] [--javaname] [--target=<target>]\n", pgm);
  exit(1);
}

// dummy declaration needed to avoid segfault during runtime startup
void ConstantFoldModule(maple::MIRModule *module) {
  maple::ConstantFold cf(module);
  cf.Simplify(nullptr);
}

int main(int argc, const char *argv[]) {
  if (argc < 2) {
    usage(argv[0]);
  }

  target = kAarch64;  // default

  const char *mirInfile = nullptr;
  VerfMode mode = kVerifyMir;
  for (int i = 1; i < argc; ++i) {
    if (!strncmp(argv[i], "--dump", 6)) {
      dumpit = true;
    } else if (!strncmp(argv[i], "--target=", 9)) {
      target = kLastTargetId;
      for (int j = 0; j < kLastTargetId; ++j) {
        target_descr_t &jt = targetDescrs[j];
        if (!strncmp(argv[i] + 9, jt.name, jt.len)) {
          target = jt.id;
          break;
        }
      }
      if (target == kLastTargetId) {
        usage(argv[0]);
      }
    } else if (!strncmp(argv[i], "--javaname", 10)) {
      mode = kVerifyJavaNames;
    } else if (argv[i][0] != '-') {
      mirInfile = argv[i];
    }
  }

  if (!mirInfile) {
    usage(argv[0]);
  }
  if (strlen(mirInfile) > PATH_MAX) {
    CHECK_FATAL(false, "invalid arg ");
  }
  if (mode == kVerifyMir) {
    std::string filename(mirInfile);
    std::string::size_type lastdot = filename.find_last_of(".");
    bool isbpl = filename.compare(lastdot, 5, ".bpl\0") == 0;

    theModule = new MIRModule(mirInfile);
    if (!isbpl) {
      maple::MIRParser theparser(*theModule);
      if (!theparser.ParseMIR()) {
        theparser.EmitError(filename);
        return 1;
      }
    }
    else {
      BinaryMplImport binMplt(*theModule);
      binMplt.imported = false;
      if (!binMplt.Import(filename, true)) {
        FATAL(kLncFatal, "cannot open .bpl file: %s", mirInfile);
        return 1;
      }
    }
    VerifyModule(theModule);
    return 0;
  } else if (mode == kVerifyJavaNames) {
    return VerifyJavaNameEncoding(mirInfile);
  } else {
    usage(argv[0]);
  }
  return 0;
}
