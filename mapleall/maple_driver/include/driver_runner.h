/*
 * Copyright (c) [2019] Huawei Technologies Co.,Ltd.All rights reserved.
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
#ifndef MAPLE_DRIVER_INCLUDE_DRIVER_RUNNER_H
#define MAPLE_DRIVER_INCLUDE_DRIVER_RUNNER_H

#include <vector>
#include <string>
#include "me_option.h"
#include "interleaved_manager.h"
#include "error_code.h"
#include "mpl_options.h"
namespace maple {
extern const std::string mpl2Mpl;
extern const std::string mplME;

class DriverRunner final {
 public:
  DriverRunner(MIRModule *theModule, const std::vector<std::string> &exeNames, InputFileType inpFileType, Options *mpl2mplOptions,
               std::string mpl2mplInput, MeOption *meOptions, const std::string &meInput, std::string actualInput,
               MemPool *optMp, bool timePhases = false,
               bool genVtableImpl = false, bool genMeMpl = false)
      : theModule(theModule),
        exeNames(exeNames),
        mpl2mplOptions(mpl2mplOptions),
        mpl2mplInput(mpl2mplInput),
        meOptions(meOptions),
        meInput(meInput),
        actualInput(actualInput),
        optMp(optMp),
        timePhases(timePhases),
        genVtableImpl(genVtableImpl),
        genMeMpl(genMeMpl),
        inputFileType(inpFileType) {}

  DriverRunner(MIRModule *theModule, const std::vector<std::string> &exeNames, InputFileType inpFileType,
               MplOptions &mplOptions, std::string theInput, MemPool *optMp)
      : theModule(theModule),
        exeNames(exeNames),
        mpl2mplOptions(&mplOptions.mpl2mplOptions),
        mpl2mplInput(theInput),
        meOptions(&mplOptions.meOptions),
        meInput(theInput),
        actualInput(theInput),
        cgOptions(&mplOptions.cgOptions),
        optMp(optMp),
        timePhases(mplOptions.HasSetTimePhases()),
        genVtableImpl(mplOptions.HasSetGenVtableImpl()),
        genMeMpl(mplOptions.HasSetGenMeMpl()),
        inputFileType(inpFileType) {}

  DriverRunner(MIRModule *theModule, const std::vector<std::string> &exeNames, InputFileType inpFileType, std::string actualInput, MemPool *optMp,
               bool timePhases = false, bool genVtableImpl = false, bool genMeMpl = false)
      : DriverRunner(theModule, exeNames, inpFileType, nullptr, "", nullptr, "", actualInput, optMp, timePhases, genVtableImpl,
                     genMeMpl) {}

  ~DriverRunner() = default;

  ErrorCode Run();
 private:
  static bool FuncOrderLessThan(const MIRFunction *left, const MIRFunction *right);

  bool IsFramework() const;
  ErrorCode ParseInput(const std::string &outputFile, const std::string &oriBasename) const;
  std::string GetPostfix() const;
  void InitPhases(InterleavedManager &mgr, const std::vector<std::string> &phases) const;
  void AddPhases(InterleavedManager &mgr, const std::vector<std::string> &phases,
                 const PhaseManager &phaseManager) const;
  void AddPhase(std::vector<std::string> &phases, const std::string phase, const PhaseManager &phaseManager) const;
  void ProcessMpl2mplAndMeAndMplCgPhases(const std::string &interimOutputFile, const std::string &outputFile, const std::string &origBaseName, bool runMplCg) const;
  MIRModule *theModule;
  std::vector<std::string> exeNames;
  Options *mpl2mplOptions = nullptr;
  std::string mpl2mplInput;
  MeOption *meOptions = nullptr;
  std::string meInput;
  std::string actualInput;
  CGOptions *cgOptions = nullptr;
  MemPool *optMp;
  bool timePhases = false;
  bool genVtableImpl = false;
  bool genMeMpl = false;
  InputFileType inputFileType;
  std::string printOutExe;
};
}  // namespace maple

#endif  // MAPLE_DRIVER_INCLUDE_DRIVER_RUNNER_H
