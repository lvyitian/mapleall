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

#ifndef MAPLEIR_INCLUDE_ME_OPTION_H
#define MAPLEIR_INCLUDE_ME_OPTION_H
#include <string>
#include <vector>
#include "mempool.h"
#include "mempool_allocator.h"

namespace maple {
class MeOption {
 public:
  enum Level {
    kLevelZero = 0,
    kLevelOne = 1,
    kLevelTwo = 2,
    kLevelThree = 3
  };

  explicit MeOption(MemPool &memPool) : optionAlloc(&memPool) {}

  ~MeOption() = default;

  bool ParseOptions(int argc, char **argv, std::string &fileName);

  void SplitPhases(const char *str, std::unordered_set<std::string> &set);
  void SplitSkipPhases(const std::string &str) {
    SplitPhases(str.c_str(), skip_phases);
  }

  void GetRange(const std::string &str) const;

  const std::unordered_set<std::string> &GetSkipPhases() const {
    return skip_phases;
  }

  void DumpUsage();
  static bool DumpPhase(std::string phase);
  static std::unordered_set<std::string> dumpPhases;
  static bool dumpBefore;
  static bool dumpAfter;
  static unsigned long range[2];
  static bool useRange;
  static std::string dumpFunc;
  static std::string skipFrom;
  static std::string skipAfter;
  static bool quiet;
  static bool setCalleeHasSideEffect;
  static bool noSteensgaard;
  static bool noTBAA;
  static uint8_t aliasAnalysisLevel;
  static bool noDot;
  static bool stmtNum;
  static uint8_t optLevel;
  static bool noRC;
  static bool strictNaiverc;
  static bool rcLowering;
  static bool noGCBar;
  static bool noReflection;
  static bool realCheckCast;
  static bool regNativeFunc;
  static bool warnNativeFunc;
  static uint32_t parserOpt;
  static uint32_t epreLimit;
  static uint32_t eprePULimit;
  static uint32_t stmtprePULimit;
  static uint32_t lpreLimit;
  static uint32_t lprePULimit;
  static uint32_t pregrenameLimit;
  static uint32_t delrcPULimit;
  static bool ignoreIPA;
  static bool epreIncludeRef;
  static bool eprelocalrefvar;
  static bool eprelhsivar;
  static bool dsekeepref;
  static bool propbase;
  static bool propiloadref;
  static bool propglobalref;
  static bool propfinaliloadref;
  static bool propiloadrefnonparm;
  static bool lessThrowAlias;
  static bool enableEA;
  static bool eaoptrc;
  static bool nodelegaterc;
  static bool nocondbasedrc;
  static bool parmtoptr;
  static bool nullcheckpre;
  static bool clinitpre;
  static bool dassignpre;
  static bool assign2finalpre;
  static bool finalFieldAlias;
  static bool regreadAtReturn;
  static bool propatphi;
  static bool nativeopt;
  static bool optdirectcall;
  static bool lprespeculate;
  static bool spillatcatch;
  static bool earlydecref;
  static bool placementrc;
  static bool strengthreduction;
  static bool rcfre;
  static std::string inlinefunclist;
#if MIR_JAVA
  static std::string acquireFuncName;
  static std::string releaseFuncName;
  static unsigned int warningLevel;
  static bool mpltoolonly;
  static bool mpltoolstrict;
  static bool skipvirtualMethod;
#endif
 private:
  std::unordered_set<std::string> skip_phases;
  maple::MapleAllocator optionAlloc;
};

#ifndef DEBUGFUNC
#define DEBUGFUNC(f)                    \
  (MeOption::DumpPhase(PhaseName().c_str()) && \
   (MeOption::dumpFunc.compare("*") == 0 || f->GetName().find(MeOption::dumpFunc.c_str()) != std::string::npos))
#endif

#ifndef TRACE_PHASE
#define TRACE_PHASE (MeOption::DumpPhase(PhaseName()))
#endif
}  // namespace maple
#endif  // MAPLEIR_INCLUDE_ME_OPTION_H
