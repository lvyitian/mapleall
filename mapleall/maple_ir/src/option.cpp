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

#include "option.h"
#include <iostream>
#include <cstring>
#include <cctype>
#include "mpl_logging.h"
#include "option_parser.h"

namespace maple {
using namespace mapleOption;

bool Options::dumpBefore = false;
bool Options::dumpAfter = false;
std::string Options::dumpPhase = "*";
std::string Options::dumpFunc = "*";
std::string Options::skipPhase;
std::string Options::skipFrom;
std::string Options::skipAfter;
bool Options::quiet = false;
bool Options::noDot = false;
bool Options::noRC = false;
bool Options::barrier = false;
bool Options::deferralBarrier = false;
bool Options::analyzeCtor = true;
bool Options::strictNaiverc = false;
bool Options::regNativeFunc = false;
bool Options::nativeWrapper = true;         // Enabled by default
bool Options::regNativeDynamicOnly = false;
bool Options::profileFunc = false;
std::string Options::staticBindingList = "";
std::string Options::nativeFuncPropertyFile = "";
bool Options::O1 = false;
bool Options::O2 = false;
bool Options::usePreloadedClass = false;
int Options::inlineLev = 2;
bool Options::usepreg = false;
bool Options::rcOpt1 = true;
bool Options::profileStaticfields = false;
bool Options::maplelinkerTransformLocal = true;
bool Options::inlineWithProfile = false;
bool Options::useInline = true;             // Enabled by default
bool Options::useCrossModuleInline = true;  // Enabled by default
std::string Options::noInlineFuncList = "";
uint32 Options::inlineSmallFunctionThreshold = 15;
uint32 Options::inlineHotFunctionThreshold = 30;
uint32 Options::inlineModuleGrowth = 10;
uint32 Options::inlineColdFunctionThreshold = 3;
uint32 Options::profileHotCount = 1000;
uint32 Options::profileColdCount = 10;
bool Options::profileHotCountSeted = false;
bool Options::profileColdCountSeted = false;
uint32 Options::profileHotRate = 500000;
uint32 Options::profileColdRate = 900000;
bool Options::usePreg = false;
bool Options::mapleLinker = false;
bool Options::dumpMuidFile = false;
bool Options::wholeReplace = false;
bool Options::buildApp = false;
std::string Options::readMuidFile = "";
uint32_t Options::parserOpt = 0;
std::string Options::dumpDevirtualList = "";
std::string Options::readDevirtualList = "";
std::string Options::profileData = "";
std::string Options::profileFuncData = "";
std::string Options::profileClassData = "";
std::string Options::profile = "";
std::string Options::methodMetaProFile = "";
std::string Options::fieldMetaProFile = "";
std::string Options::reflectStringProFile = "";
std::string Options::classloaderInvocationList = "";
bool Options::dumpClassloaderInvocation = false;
bool Options::nativeopt = true;
std::string Options::criticalNativeFile = "";
std::string Options::fastNativeFile = "";
bool Options::emitVtableImpl = false;
#if MIR_JAVA
std::string Options::acquireFuncName = "";
std::string Options::releaseFuncName = "";
unsigned int Options::warningLevel = 0;
bool Options::mpltoolonly = false;
bool Options::mpltoolstrict = false;
bool Options::skipVirtualMethod = false;
#endif
enum OptionIndex {
  kUnknown,
  kHelp,
  kDumpPhase,
  kSkipPhase,
  kSkipFrom,
  kSkipAfter,
  kDumpFunc,
  kQuiet,
  kNoDot,
  kNoRc,
  kUseRc,
  kNoBarrier,
  kBarrier,
  kNoReflec,
  kStubJniFunc,
  kInlineWithProfile,
  kInlineWithoutProfile,
  kUseInline,
  kNoInline,
  kNoInlineFuncList,
  kUseCrossModuleInline,
  kNoCrossModuleInline,
  kInlineSmallFunctionThreshold,
  kInlineHotFunctionThreshold,
  kInlineModuleGrowth,
  kInlineColdFunctionThreshold,
  kProfileHotCount,
  kProfileColdCount,
  kProfileHotRate,
  kProfileColdRate,
  kRegNativeDynamicOnly,
  kRegNativeStaticBindingList,
  kNativeFuncPropertyFile,
  kStubFunc,
  kNativeWrapper,
  kDeferralBarrier,
  kStrictNaiveRc,
  kNoNativeOpt,
  kDumpBefore,
  kDumpAfter,
  kAcquireFunc,
  kReleaseFunc,
  kCheckComplete,
  kWarnLevel,
  kToolOnly,
  kToolStrict,
  kSkipVirtual,
  kOptL1,
  kOptL2,
  kDumpDevirtual,
  kReadDevirtual,
  kInlineLev,
  kMapleLinker,
  kPreloadedClass,
  kMplnkDumpMuid,
  kMplnkWholeReplace,
  kBuildApp,
  kMplnkReadMuid,
  kMplnkNoLocal,
  kRcOpt1,
  kProfile,
  kProfileData,
  kProfileStaticFields,
  kClassmetaProfile,
  kMethodmetaProfile,
  kFieldmetaProfile,
  kCheckClInvocation,
  kDumpClInvocation,
  kCriticalnative,
  kFastnative,
  kEmitVtableImpl,
  kProfileFunc
};

const Descriptor kUsage[] = {
  { kUnknown, 0, "", "", kBuildTypeAll, kArgCheckPolicyUnknown,
    "USAGE:  mplxxx [options] *.mpl\n\n"
    "OPTIONS:" },
  { kHelp, 0, "h", "help", kBuildTypeAll, kArgCheckPolicyNone,
    "  -h, --help                        Print usage and exit" },
  { kDumpPhase, 0, "", "dump-phase", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --dump-phase=PHASENAME            Enable debug trace for specified phase (can only specify once)" },
  { kSkipPhase, 0, "", "skip-phase", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --skip-phase=PHASENAME            Skip the phase when adding it to phase manager" },
  { kSkipFrom, 0, "", "skip-from", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --skip-from=PHASENAME             Skip all remaining phases including PHASENAME" },
  { kSkipAfter, 0, "", "skip-after", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --skip-after=PHASENAME            Skip all remaining phases after PHASENAME" },
  { kDumpFunc, 0, "", "dump-func", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --dump-func=FUNCNAME              Dump/trace only for functions whose names contain FUNCNAME as substring\n"
    "                                    (can only specify once)" },
  { kQuiet, 0, "", "quiet", kBuildTypeAll, kArgCheckPolicyNone,
    "  --quiet                           Disable brief trace messages with phase/function names" },
  { kNoDot, 0, "", "nodot", kBuildTypeAll, kArgCheckPolicyNone, "  --nodot                           Disable dot file generation from cfg" },
  { kNoRc, 0, "", "norc", kBuildTypeAll, kArgCheckPolicyNone, "  --norc                            Disable reference counting" },
  { kUseRc, 0, "", "userc", kBuildTypeAll, kArgCheckPolicyNone, "  --userc                           Enable reference counting [default]" },
  { kNoBarrier, 0, "", "nobarrier", kBuildTypeAll, kArgCheckPolicyNone,
    "  --nobarrier                       Disable barrier insertion [default]" },
  { kBarrier, 0, "", "barrier", kBuildTypeAll, kArgCheckPolicyNone,
    "  --barrier                         Enable barrier insertion instead of RC insertion" },
  { kDeferralBarrier, 0, "", "deferral-barrier", kBuildTypeAll, kArgCheckPolicyNone,
    "  --deferral-barrier                Enable write barrier insertion for deferral RC" },
  { kStrictNaiveRc, 0, "", "strict-naiverc", kBuildTypeAll, kArgCheckPolicyNone,
    "  --strict-naiverc                  Strict Naive RC mode, assume no unsafe multi-thread read/write racing" },
  { kStubJniFunc, 0, "", "regnativefunc", kBuildTypeAll, kArgCheckPolicyNone,
    "  --regnativefunc                   Generate native stub function to support JNI registration and calling" },
  { kInlineWithProfile, 0, "", "inline-with-profile", kBuildTypeAll, kArgCheckPolicyNone,
    "  --inline-with-profile             Enable profile-based inlining" },
  { kInlineWithoutProfile, 0, "", "inline-without-profile", kBuildTypeAll, kArgCheckPolicyNone,
    "  --inline-without-profile          Disable profile-based inlining" },
  { kUseInline, 0, "", "inline", kBuildTypeAll, kArgCheckPolicyNone,
    "  --inline                          Enable function inlining" },
  { kNoInline, 0, "", "no-inline", kBuildTypeAll, kArgCheckPolicyNone,
    "  --no-inline                       Disable function inlining" },
  { kNoInlineFuncList, 0, "", "no-inlinefunclist", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --no-inlinefunclist=list          Do not inline function in this list" },
  { kUseCrossModuleInline, 0, "", "cross-module-inline", kBuildTypeAll, kArgCheckPolicyNone,
    "  --cross-module-inline             Enable cross-module inlining" },
  { kNoCrossModuleInline, 0, "", "no-cross-module-inline", kBuildTypeAll, kArgCheckPolicyNone,
    "  --no-cross-module-inline          Disable cross-module inlining" },
  { kInlineSmallFunctionThreshold, 0, "", "inline-small-function-threshold", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --inline-small-function-threshold=15        Threshold for inlining small function" },
  { kInlineHotFunctionThreshold, 0, "", "inline-hot-function-threshold", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --inline-hot-function-threshold=30          Threshold for inlining hot function" },
  { kInlineModuleGrowth, 0, "", "inline-module-growth", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --inline-module-growth=100000               Threshold for maxmium code size growth rate. (10%)" },
  { kInlineColdFunctionThreshold, 0, "", "inline-cold-function-threshold", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --inline-cold-function-threshold=3          Threshold for inlining cold function" },
  { kProfileHotCount, 0, "", "profile-hot-count", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --profile-hot-count=1000          A count is regarded as hot if it exceeds this number" },
  { kProfileColdCount, 0, "", "profile-cold-count", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --profile-cold-count=10           A count is regarded as cold if it is below this number" },
  { kProfileHotRate, 0, "", "profile-hot-rate", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --profile-hot-rate=500000         A count is regarded as hot if it is in the largest 50%" },
  { kProfileColdRate, 0, "", "profile-cold-rate", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --profile-cold-rate=900000        A count is regarded as cold if it is in the smallest 10%" },
  { kNativeWrapper, 1, "", "nativewrapper", kBuildTypeAll, kArgCheckPolicyNone,
    "  --nativewrapper                   Generate native wrappers [default]" },
  { kNativeWrapper, 0, "", "no-nativewrapper", kBuildTypeAll, kArgCheckPolicyNone,
    "  --no-nativewrapper                Do not generate native wrappers" },
  { kRegNativeDynamicOnly, 0, "", "regnative-dynamic-only", kBuildTypeAll, kArgCheckPolicyNone,
    "  --regnative-dynamic-only          Only Generate dynamic register code, Report Fatal Msg if no implemented" },
  { kRegNativeStaticBindingList, 0, "", "static-binding-list", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --static-bindig-list=file         Only Generate static binding function in file configure list" },
  { kNativeFuncPropertyFile, 0, "", "nativefunc-property-list", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --nativefunc-property-list=file   Generate native binding function stub" },
  { kDumpBefore, 0, "", "dump-before", kBuildTypeAll, kArgCheckPolicyNone,
    "  --dump-before                     Do extra IR dump before the specified phase" },
  { kDumpAfter, 0, "", "dump-after", kBuildTypeAll, kArgCheckPolicyNone,
    "  --dump-after                      Do extra IR dump after the specified phase" },
  { kOptL1, 0, "", "O1", kBuildTypeAll, kArgCheckPolicyNone, "  --O1                              Optimization level 1" },
  { kOptL2, 0, "", "O2", kBuildTypeAll, kArgCheckPolicyNone, "  --O2                              Optimization level 2" },
  { kInlineLev, 0, "", "inline-lev", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --inline-lev=0-9                  Inlining level, one digit number" },
  { kCheckComplete, 0, "", "check-complete", kBuildTypeAll, kArgCheckPolicyNone, "  --check-complete                  Check incomplete types" },

  { kMapleLinker, 0, "", "maplelinker", kBuildTypeAll, kArgCheckPolicyNone,
    "  --maplelinker                     Generate MUID symbol tables and references" },
  { kProfileStaticFields, 0, "", "profile-static-fields", kBuildTypeAll, kArgCheckPolicyNone,
    "  --profile-static-fields                     Generate profile static fields info" },
  { kMplnkNoLocal, 0, "", "maplelinker-nolocal", kBuildTypeAll, kArgCheckPolicyNone,
    "  --maplelinker-nolocal             Do not turn functions into local when maple linker is on" },
  { kMplnkDumpMuid, 0, "", "dump-muid", kBuildTypeAll, kArgCheckPolicyNone,
    "  --dump-muid                       Dump MUID def information into a .muid file" },
  { kProfileFunc, 0, "", "profile-func", kBuildTypeAll, kArgCheckPolicyNone, "  --profile-func                   Profile function usage" },
  { kMplnkWholeReplace, 0, "", "whole-replace", kBuildTypeAll, kArgCheckPolicyNone,
    "  --whole-replace                   Replace the whole function call" },
  { kBuildApp, 0, "", "build-app", kBuildTypeAll, kArgCheckPolicyNone, "  --build-app                       build the app file" },
  { kMplnkReadMuid, 0, "", "read-muid", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --read-muid                       Read MUID def information from a .muid file" },
  { kDumpDevirtual, 0, "", "dump-devirtual-list", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --dump-devirtual-list             Dump candidates of devirtualization into a specified file" },
  { kReadDevirtual, 0, "", "read-devirtual-list", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --read-devirtual-list             Read in candidates of devirtualization from a specified file and perform devirtualization" },
  { kRcOpt1, 1, "", "rc-opt1", kBuildTypeAll, kArgCheckPolicyNone, "  --rc-opt1                         Enable RC optimization1 [default]" },
  { kRcOpt1, 0, "", "no-rc-opt1", kBuildTypeAll, kArgCheckPolicyNone, "  --no-rc-opt1                      Disable RC optimization1" },
  { kPreloadedClass, 0, "", "usewhiteclass", kBuildTypeAll, kArgCheckPolicyNone,
    "  --usewhiteclass                      Enable use preloaded class list to reducing clinit check" },
  { kProfileData, 0, "", "profile_data", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --profile_data=FUNCINFOFILE[:]CLASSINFOFILE       use profile info to sort funcbody and class static field" },
  { kProfile, 0, "", "profile", kBuildTypeAll, kArgCheckPolicyRequired,"  -- profile=list_file       For PGO optimization" },
  { kClassmetaProfile, 0, "", "classmeta_profile", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --classmeta_profile=list_file       For classmeta layout optimization" },
  { kMethodmetaProfile, 0, "", "methodmeta_profile", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --methodmeta_profile=list_file       For methodmeta layout optimization" },
  { kFieldmetaProfile, 0, "", "fieldmeta_profile", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --fieldmeta_profile=list_file       For fieldmeta layout optimization" },
  { kCheckClInvocation, 0, "", "check_cl_invocation", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --check_cl_invocation=list_file       For classloader invocation checking" },
  { kDumpClInvocation, 0, "", "dump_cl_invocation", kBuildTypeAll, kArgCheckPolicyNone,
    "  --dump_cl_invocation       For classloader invocation dumping. Work only if already set --check_cl_invocation" },
  { kNoNativeOpt, 0, "", "no-nativeopt", kBuildTypeAll, kArgCheckPolicyNone, "  --no-nativeopt              Disable native opt" },
  { kCriticalnative, 0, "", "CriticalNative", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --CriticalNative=list_file       For CriticalNative optimization" },
  { kFastnative, 0, "", "FastNative", kBuildTypeAll, kArgCheckPolicyRequired, "  --FastNative=list_file       For FastNative optimization" },
  { kEmitVtableImpl, 0, "", "emitVtableImpl", kBuildTypeAll, kArgCheckPolicyNone, " --emitVtableImpl         generate VtableImpl file" },
#if MIR_JAVA
  { kAcquireFunc, 0, "", "acquire-func", kBuildTypeAll, kArgCheckPolicyRequired, "  --acquire-func=FUNCNAME" },
  { kReleaseFunc, 0, "", "release-func", kBuildTypeAll, kArgCheckPolicyRequired, "  --release-func=FUNCNAME" },
  { kToolOnly, 0, "", "toolonly", kBuildTypeAll, kArgCheckPolicyNone, "  --toolonly" },
  { kToolStrict, 0, "", "toolstrict", kBuildTypeAll, kArgCheckPolicyNone, "  --toolstrict" },
  { kEmitVtableImpl, 0, "", "emitVtableImpl", kBuildTypeAll, kArgCheckPolicyNone,
    "  --emitVtableImpl                  Generate VtableImpl file" },
  { kSkipVirtual, 0, "", "skipvirtual", kBuildTypeAll, kArgCheckPolicyNone, "  --skipvirtual" },
  { kWarnLevel, 0, "", "warning", kBuildTypeAll, kArgCheckPolicyNumeric, "  --warning=level" },
#endif
  { 0, 0, nullptr, nullptr, kBuildTypeAll, kArgCheckPolicyNone, nullptr }
};

bool Options::ParseOptions(int argc, char **argv, std::string &fileName) const {
  bool result = true;
  OptionParser optionParser(kUsage);
  int ret = optionParser.Parse(argc, argv);
  CHECK_FATAL(ret == kErrorNoError, "option parser error");
  for (auto opt : optionParser.GetOptions()) {
    switch (opt.Index()) {
      case kHelp: {
        if (opt.Args().empty()) {
          optionParser.PrintUsage();
          result = false;
        }
        break;
      }
      case kDumpBefore:
        Options::dumpBefore = true;
        break;
      case kDumpAfter:
        Options::dumpAfter = true;
        break;
      case kDumpFunc:
        Options::dumpFunc = opt.Args();
        break;
      case kDumpPhase:
        Options::dumpPhase = opt.Args();
        break;
      case kSkipPhase:
        Options::skipPhase = opt.Args();
        break;
      case kSkipFrom:
        Options::skipFrom = opt.Args();
        break;
      case kSkipAfter:
        Options::skipAfter = opt.Args();
        break;
      case kQuiet:
        Options::quiet = true;
        break;
      case kNoRc:
        Options::noRC = true;
        break;
      case kUseRc:
        Options::noRC = false;
        break;
      case kNoBarrier:
        Options::barrier = false;
        break;
      case kBarrier:
        Options::barrier = true;
        break;
      case kStrictNaiveRc:
        Options::strictNaiverc = true;
        break;
      case kDeferralBarrier:
        Options::deferralBarrier = true;
        break;
      case kNoDot:
        Options::noDot = true;
        break;
      case kRegNativeDynamicOnly:
        Options::regNativeDynamicOnly = true;
        break;
      case kRegNativeStaticBindingList:
        Options::staticBindingList = opt.Args();
        break;
      case kNativeFuncPropertyFile:
        Options::nativeFuncPropertyFile = opt.Args();
        break;
      case kStubJniFunc:
        Options::regNativeFunc = true;
        break;
      case kInlineWithProfile:
        Options::inlineWithProfile = true;
        break;
      case kInlineWithoutProfile:
        Options::inlineWithProfile = false;
        break;
      case kOptL1:
        Options::O1 = true;
        Options::O2 = true;
        Options::usepreg = true;
        break;
      case kOptL2:
        Options::O2 = true;
        Options::usepreg = true;
        break;
      case kUseInline:
        Options::useInline = true;
        break;
      case kNoInline:
        Options::useInline = false;
        break;
      case kRcOpt1:
        Options::rcOpt1 = opt.Type();
        break;
      case kInlineLev:
        if (opt.Args().empty() || opt.Args().length() != 1 || !isdigit(opt.Args().at(0))) {
          fprintf(stderr, "expecting one digit number for --inline-lev\n");
          exit(1);
        }
        Options::inlineLev = opt.Args().at(0) - '0';
        break;
      case kNoInlineFuncList:
        Options::noInlineFuncList = opt.Args();
        break;
      case kCheckComplete:
        Options::parserOpt |= kCheckCompleteType;
        break;
      case kDumpDevirtual:
        Options::dumpDevirtualList = opt.Args();
        break;
      case kReadDevirtual:
        Options::readDevirtualList = opt.Args();
        break;
      case kPreloadedClass:
        Options::usePreloadedClass = true;
        break;
      case kProfileStaticFields:
        Options::profileStaticfields = true;
        break;
      case kMplnkNoLocal:
        Options::maplelinkerTransformLocal = false;
        break;
      case kProfile:
        Options::profile = opt.Args();
        break;
      case kProfileFunc:
        Options::profileFunc = true;
        break;
      case kMplnkWholeReplace:
        Options::wholeReplace = true;
        break;
      case kBuildApp:
        Options::buildApp = true;
        break;
      case kMplnkReadMuid:
        Options::readMuidFile = opt.Args();
        break;
      case kProfileData:
        Options::profileData = opt.Args();
        break;
      case kMethodmetaProfile:
        Options::methodMetaProFile = opt.Args();
        break;
      case kFieldmetaProfile:
        Options::fieldMetaProFile = opt.Args();
        break;
      case kCheckClInvocation:
        Options::classloaderInvocationList = opt.Args();
        break;
      case kDumpClInvocation:
        Options::dumpClassloaderInvocation = true;
        break;
      case kNoNativeOpt:
        Options::nativeopt = false;
        break;
      case kCriticalnative:
        Options::criticalNativeFile = opt.Args();
        break;
      case kFastnative:
        Options::fastNativeFile = opt.Args();
        break;
      case kUseCrossModuleInline:
        Options::useCrossModuleInline = true;
        break;
      case kNoCrossModuleInline:
        Options::useCrossModuleInline = false;
        break;
      case kInlineSmallFunctionThreshold:
        Options::inlineSmallFunctionThreshold = std::stoul(opt.Args());
        break;
      case kInlineHotFunctionThreshold:
        Options::inlineHotFunctionThreshold = std::stoul(opt.Args());
        break;
      case kInlineModuleGrowth:
        Options::inlineModuleGrowth = std::stoul(opt.Args());
        break;
      case kInlineColdFunctionThreshold:
        Options::inlineColdFunctionThreshold = std::stoul(opt.Args());
        break;
      case kProfileHotCount:
        Options::profileHotCount = std::stoul(opt.Args());
        Options::profileHotCountSeted = true;
        break;
      case kProfileColdCount:
        Options::profileColdCount = std::stoul(opt.Args());
        Options::profileColdCountSeted = true;
        break;
      case kProfileHotRate:
        Options::profileHotRate = std::stoul(opt.Args());
        break;
      case kProfileColdRate:
        Options::profileColdRate = std::stoul(opt.Args());
        break;
      case kNativeWrapper:
        Options::nativeWrapper = opt.Type();
        break;
      case kMapleLinker:
        Options::mapleLinker = true;
        Options::dumpMuidFile = false;
        break;
      case kMplnkDumpMuid:
        Options::dumpMuidFile = true;
        break;
      case kEmitVtableImpl:
        Options::emitVtableImpl = true;
        break;
#if MIR_JAVA
      case kAcquireFunc:
        Options::acquireFuncName = opt.Args();
        break;
      case kReleaseFunc:
        Options::releaseFuncName = opt.Args();
        break;
      case kWarnLevel:
        Options::warningLevel = std::stoi(opt.Args());
        break;
      case kToolOnly:
        Options::mpltoolonly = true;
        break;
      case kToolStrict:
        Options::mpltoolstrict = true;
        break;
      case kSkipVirtual:
        Options::skipVirtualMethod = true;
        break;
#endif
      default:
        result = false;
        ASSERT(false, "unhandled case in Options");
    }
  }

  if (result) {
    if (optionParser.GetNonOptionsCount() != 1) {
      LogInfo::MapleLogger(kLlErr) << "expecting one .mpl file as last argument, found: ";
      for (const auto &optionArg : optionParser.GetNonOptions()) {
        LogInfo::MapleLogger(kLlErr) << optionArg << " ";
      }
      LogInfo::MapleLogger(kLlErr) << "\n";
      result = false;
    }

    if (result) {
      fileName = optionParser.GetNonOptions().front();
    }
  }
  return result;
}

void Options::DumpOptions() const {
  LogInfo::MapleLogger() << "phase sequence : \t";
  if (phaseSeq.empty()) {
    LogInfo::MapleLogger() << "default phase sequence\n";
  } else {
    for (size_t i = 0; i < phaseSeq.size(); ++i) {
      LogInfo::MapleLogger() << " " << phaseSeq[i];
    }
  }
  LogInfo::MapleLogger() << "\n";
}
};  // namespace maple
