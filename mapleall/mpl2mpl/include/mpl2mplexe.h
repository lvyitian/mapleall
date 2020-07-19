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

#ifndef MPL2MPL_INCLUDE_MPL2MPLEXE_H
#define MPL2MPL_INCLUDE_MPL2MPLEXE_H

#include "option.h"
#include "mir_module.h"
#include "mir_parser.h"

class Executable {
 public:
  Executable(MIRModule &mod, std::string &fileName) : module(mod), filename(fileName) {
    parser = nullptr;
  }

  void SetFilename(const std::string &fileName) {
    filename = fileName;
  }

  MIRModule GetModule() {
    return module;
  }

  bool ParseMIR(uint32 fileidx = 0, uint32 options = 0) {
    parser = module.memPool->New<MIRParser>(module);
    if (parser->ParseMIR(fileidx, options)) {
      return true;
    } else {
      parser->EmitError(filename);
      return false;
    }
  }

  virtual void EmitOutput() = 0;
  virtual void EmitParseWarning() = 0;
  void EmitError(const std::string &fileName) {
    parser->EmitError(fileName);
  };
  virtual ~Executable(){};

 protected:
  MIRModule &module;
  std::string &filename;
  MIRParser *parser;
};

class Mpl2MplExecutable : public Executable {
 public:
  Mpl2MplExecutable(MIRModule &mod, std::string &fileName) : Executable(mod, fileName) {}

  int Run(Options option, MemPool *optmp);
  void EmitParseWarning() override;
  void EmitOutput() override;
  virtual ~Mpl2MplExecutable(){};
};

#endif /* MPL2MPL_INCLUDE_MPL2MPLEXE_H */
