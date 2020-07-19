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

#ifndef MAPLE_IR_INCLUDE_OPTION_H
#define MAPLE_IR_INCLUDE_OPTION_H
#include <string>
#include <vector>
#include "mempool.h"
#include "mempool_allocator.h"
#include "parser_opt.h"
#include "types_def.h"

namespace maple {
constexpr uint32 kNoDecouple = 0;
constexpr uint32 kConservativeDecouple = 1;
constexpr uint32 kRadicalDecouple = 2;
class Options {
 public:
  enum Level {
    kMpl2MplLevelZero = 0,
    kMpl2MplLevelOne = 1,
    kMpl2MplLevelTwo = 2
  };
  explicit Options(MemPool &memPool) : optionAlloc(&memPool) {}

  bool ParseOptions(int argc, char **argv, std::string &fileName) const;
  ~Options() = default;

  void DumpOptions() const;
  const std::vector<std::string> &GetSequence() const {
    return phaseSeq;
  }

  std::string LastPhaseName() const {
    return phaseSeq.empty() ? "noopt" : phaseSeq[phaseSeq.size() - 1];
  }

  static bool dumpBefore;
  static bool dumpAfter;
  static std::string dumpPhase;
  static std::string skipPhase;
  static std::string skipFrom;
  static std::string skipAfter;
  static std::string dumpFunc;
  static bool quiet;
  static bool noDot;
  static bool noRC;
  static bool barrier;
  static bool deferralBarrier;
  static bool analyzeCtor;
  static bool strictNaiverc;
  static bool regNativeFunc;
  static bool regNativeDynamicOnly;
  static bool nativeWrapper;
  static bool inlineWithProfile;
  static bool useInline;
  static std::string noInlineFuncList;
  static bool useCrossModuleInline;
  static uint32 inlineSmallFunctionThreshold;
  static uint32 inlineHotFunctionThreshold;
  static uint32 inlineModuleGrowth;
  static uint32 inlineColdFunctionThreshold;
  static uint32 profileHotCount;
  static uint32 profileColdCount;
  static bool profileHotCountSeted;
  static bool profileColdCountSeted;
  static uint32 profileHotRate;
  static uint32 profileColdRate;
  static std::string staticBindingList;
  static std::string nativeFuncPropertyFile;

  static bool O1;
  static bool O2;
  static int inlineLev;
  static bool usepreg;
  static bool profileStaticfields;
  static bool maplelinkerTransformLocal;
  static bool usePreg;
  static bool mapleLinker;
  static bool dumpMuidFile;
  static bool wholeReplace;
  static bool buildApp;
  static bool profileFunc;
  static std::string readMuidFile;
  static uint32_t parserOpt;
  static std::string dumpDevirtualList;
  static std::string readDevirtualList;
  static bool usePreloadedClass;
  static bool rcOpt1;
  static std::string profile;
  static std::string profileData;
  static std::string profileClassData;
  static std::string profileFuncData;
  static std::string classMetaProFile;
  static std::string methodMetaProFile;
  static std::string fieldMetaProFile;
  static std::string reflectStringProFile;
  static std::string classloaderInvocationList;
  static bool dumpClassloaderInvocation;
  static bool nativeopt;
  static std::string criticalNativeFile;
  static std::string fastNativeFile;
  static bool emitVtableImpl;
#if MIR_JAVA
  static std::string acquireFuncName;
  static std::string releaseFuncName;
  static unsigned int warningLevel;
  static bool mpltoolonly;
  static bool mpltoolstrict;
  static bool skipVirtualMethod;
#endif
 private:
  MapleAllocator optionAlloc;
  std::vector<std::string> phaseSeq;
};
}  // namespace maple
#ifndef TRACE_PHASE
#define TRACE_PHASE (Options::dumpPhase.compare(PhaseName()) == 0)
#endif
#endif  // MAPLE_IR_INCLUDE_OPTION_H
