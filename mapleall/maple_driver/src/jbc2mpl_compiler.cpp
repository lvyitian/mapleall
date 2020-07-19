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
#include <cstdlib>
#include "compiler.h"
#include "default_options.def"

namespace maple {
const std::string &Jbc2MplCompiler::GetBinName() const {
  return kBinNameJbc2mpl;
}

DefaultOption Jbc2MplCompiler::GetDefaultOptions(const MplOptions &options) const {
  DefaultOption defaultOptions = { nullptr, 0 };
  return defaultOptions;
}

void Jbc2MplCompiler::GetTmpFilesToDelete(const MplOptions &mplOptions, std::vector<std::string> &tempFiles) const {
  tempFiles.push_back(mplOptions.GetOutputFolder() + mplOptions.GetOutputName() + ".mpl");
}

std::unordered_set<std::string> Jbc2MplCompiler::GetFinalOutputs(const MplOptions &mplOptions) const {
  std::unordered_set<std::string> finalOutputs;
  finalOutputs.insert(mplOptions.GetOutputFolder() + mplOptions.GetOutputName() + ".mpl");
  return finalOutputs;
}
}  // namespace maple
