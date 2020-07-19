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

#ifndef MAPLE_IR_INCLUDE_BIN_MPLT_H
#define MAPLE_IR_INCLUDE_BIN_MPLT_H
#include "mir_module.h"
#include "mir_nodes.h"
#include "mir_preg.h"
#include "parser_opt.h"
#include "bin_mpl_export.h"
#include "bin_mpl_import.h"

namespace maple {
class BinaryMplt {
 public:
  std::vector<std::string> se_usage = {
    "libcore-all.mpl"
  };

  explicit BinaryMplt(MIRModule &md) : mirModule(md), binImport(md), binExport(md) {}

  virtual ~BinaryMplt() {}

  void Export(const std::string &suffix, std::unordered_set<std::string> *dumpFuncSet = nullptr) {
    binExport.Export(suffix, dumpFuncSet);
  }

  bool Import(const std::string &modID, bool readCG = false, bool readSE = false) {
    bool found = false;
    for (unsigned int i = 0; i < se_usage.size(); i++) {
      if (se_usage[i] == mirModule.fileName) {
        found = true;
        break;
      }
    }
    readSE = readSE && found;
    importFileName = modID;
    return binImport.Import(modID, readCG, readSE);
  }

  MIRModule &GetMod() {
    return mirModule;
  }

  BinaryMplImport &GetBinImport() {
    return binImport;
  }

  BinaryMplExport &GetBinExport() {
    return binExport;
  }

  std::string &GetImportFileName() {
    return importFileName;
  }

  void SetImportFileName(std::string fileName) {
    importFileName = fileName;
  }

 private:
  MIRModule &mirModule;
  BinaryMplImport binImport;
  BinaryMplExport binExport;
  std::string importFileName;
};

}  // namespace maple
#endif
