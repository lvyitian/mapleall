/*
 * Copyright (c) [2019-2020] Huawei Technologies Co., Ltd. All rights reserved.
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
#ifndef MAPLE_DRIVER_INCLUDE_COMPILER_FACTORY_H
#define MAPLE_DRIVER_INCLUDE_COMPILER_FACTORY_H
#include <unordered_set>
#include "compiler.h"
#include "compiler_selector.h"
#include "error_code.h"
#include "mir_module.h"
#include "mir_parser.h"

namespace maple {
class CompilerFactory {
 public:
  static CompilerFactory &GetInstance();
  CompilerFactory(const CompilerFactory&) = delete;
  CompilerFactory(CompilerFactory&&) = delete;
  CompilerFactory &operator=(const CompilerFactory&) = delete;
  CompilerFactory &operator=(CompilerFactory&&) = delete;
  ~CompilerFactory();
  ErrorCode Compile(MplOptions &mplOptions);

 private:
  CompilerFactory();
  void Insert(const std::string &name, Compiler *value);
  ErrorCode DeleteTmpFiles(const MplOptions &mplOptions, const std::vector<std::string> &tempFiles,
                           const std::unordered_set<std::string> &finalOutputs) const;
  SupportedCompilers supportedCompilers;
  CompilerSelector *compilerSelector;
  MIRModule *theModule = nullptr;
};
}  // namespace maple
#endif  // MAPLE_DRIVER_INCLUDE_COMPILER_FACTORY_H
