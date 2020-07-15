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

#ifndef MAPLECG_INCLUDE_OPTION_H
#define MAPLECG_INCLUDE_OPTION_H
#include <vector>
#include "mempool.h"
#include "mempool_allocator.h"

namespace maple {
class MIRModule;
}

class CGOptions {
 public:
  enum OptionEnum : uint64_t {
    kUndefined = 0ULL,
    kDoCg = 1ULL << 0,
    kUseFp = 1ULL << 1,
    kDoLinearScanRegAlloc = 1ULL << 2,
    kDoColorRegAlloc = 1ULL << 3,
    kConstFold = 1ULL << 4,
    kGenPic = 1ULL << 5,
    kGenPie = 1ULL << 6,
    kVerboseAsm = 1ULL << 7,
    kInsertCall = 1ULL << 8,
    kAddDebugTrace = 1ULL << 9,
    kGenYieldPoint = 1ULL << 10,
    kGenLocalRc = 1ULL << 11,
    kGenTestInfo = 1ULL << 12,
    kProEpilogueOpt = 1ULL << 13,
    kZeroExtend = 1ULL << 14,
    kDebugFriendly = 1ULL << 20,
    kWithLoc = 1ULL << 21,
    kWithDwarf = 1ULL << 22,
    kWithMpl = 1ULL << 23,
    kWithSrc = 1ULL << 24,
    kWithAsm = 1ULL << 25,
    kWithProfileCode = 1ULL << 30,
    kUseFastunwind = 1ULL << 31,
    kUseStackGuard = 1ULL << 32,
    kSoeCheckInsert = 1ULL << 33,
    kAddFuncProfile = 1ULL << 34,
    // undocumented
    kConvertLocalsToPregs = 1ULL << 59,
    kDumpSsaDef = 1ULL << 60,
    kDumpCfg = 1ULL << 61,
    kDumpCgir = 1ULL << 62,
    kSuppressFileinfo = 1ULL << 63,
  };

  typedef uint64_t option_flag_t;

  enum GenerateEnum : uint64_t {
    kCMacroDef = 1ULL << 0,
    kGctib = 1ULL << 1,
    kGrootList = 1ULL << 2,
    kPrimorList = 1ULL << 3,
  };

  typedef uint64_t generate_flag_t;
  /*
     The default CG option values are:
      Don't BE_QUITE; verbose,
      DO CG and generate .s file as output,
      Generate EH,
      Use frame pointer,
      Generate CFI directives,
      DO peephole optimization,
      Generate position-independent executable,
      Don't insert debug comments in .s file,
      Don't insert a call to the named (instrumentation)
      function at each function entry.
   */
  static const option_flag_t kDefaultOptions = option_flag_t(
#if TARGAARCH64
    kDoCg | kUseFp | kGenPie | kDoColorRegAlloc
#else
    kDoCg | kUseFp
#endif
  );

  /*
     The default metadata generation flags values are:
      Generate .macros.def for C preprocessors.
      Generate .groots.txt for GC.
      Generate .primordials.txt for GC.
      Generate yieldpoints for GC.
      Do not generate separate GCTIB file.
   */
  // kCMacroDef | kGrootList | kPrimorList
  static const generate_flag_t kDefaultGflags = generate_flag_t(0);

  generate_flag_t gflags_;
  option_flag_t options_;
  std::string instru_func;

 private:
  maple::MapleAllocator optionAlloc;
  std::vector<std::string> phaseSeq;

 public:
  explicit CGOptions(maple::MemPool &mp) : optionAlloc(&mp) {}
  virtual ~CGOptions() {}
  bool ParseOptions(int argc, char **argv, std::string &fileName);
  void DumpUsage();
  void DumpOptions();
  std::vector<std::string> &GetSequence() {
    return phaseSeq;
  }

  const char *LastPhaseName() {
    return phaseSeq.size() > 0 ? phaseSeq[phaseSeq.size() - 1].c_str() : "noopt";
  }

  bool insert_call = false;
  bool run_cg_flag = true;
  bool do_with_fp = true;
  bool gen_objmap = true;
  bool generate_gdb_friendly_code = true;  // default for O0/O1
  uint32_t parserOpt = 0;
  int optim_level = 0;
  bool genMirMpl = false;

  static bool quiet;
  static std::unordered_set<std::string> dumpPhases;
  static std::unordered_set<std::string> skipPhases;
  static std::unordered_map<std::string, std::vector<std::string>> cyclePatternMap;
  static std::string skipFrom;
  static std::string skipAfter;
  static std::string dumpFunc;
  static std::string profileData;
  static std::string profileFuncData;
  static std::string profileClassData;
  static std::string duplicateAsmFile;
  static unsigned long range[2];
  static unsigned long spillRanges[2];
  static unsigned long lsraBbOptSize;
  static unsigned long lsraInsnOptSize;
  static unsigned long overlapNum;
  static unsigned long fastAllocMode;
  static unsigned long lsraSimdMode;
  static bool useBarriersForVolatile;
  static bool fastAlloc;
  static bool dumpBefore;
  static bool dumpAfter;
  static bool timePhases;
  static bool spillRange;
  static bool useRange;
  static bool inRange;
  static bool genEH;
  static bool doZeroExtend;
  static bool doConstFold;
  static bool doLiveAnalysisEh;
  static bool doEbo;
  static bool doCfgo;
  static bool doIco;
  static bool doStoreLoadOpt;
  static bool doGlobalOpt;
  static bool doPreLsraOpt;
  static bool doPostLsraOpt;
  static bool doLocalRefSpill;
  static bool doLvarPathOpt;
  static bool doCalleeToSpill;
  static bool dumpOLog;
  static bool doPrePeephole;
  static bool checkarraystore;
  static bool doPeephole;
  static bool doPreSchedule;
  static bool doSchedule;
  static bool genCfi;
  static bool exclusiveEh;
  static bool doPIC;
  static bool noDupBB;
  static bool emitCyclePattern;
  static bool insertYieldpoint;
  static bool maplelinker;
  static bool replaceasm;
  static bool printFunction;
  static bool doSimplePrint;
  static unsigned int maplelinkerSuffix;
  std::string class_list_file;
  std::string ehexclusivefile;
  std::string cyclepatternfile;
  static std::string literalProFile;
  static std::string classMetaProFile;
  static std::string methodMetaProFile;
  static std::string fieldMetaProFile;
  static std::string globalVarProFile;
  static bool emitBlockMarker;
  static bool nativeopt;
  static bool withDwarf;

  static inline void SetOption(option_flag_t &flg, OptionEnum o) {
    flg |= option_flag_t(o);
  }

  static inline void ClearOption(option_flag_t &flg, OptionEnum o) {
    flg &= ~option_flag_t(o);
  }

  static bool CheckOptions(option_flag_t flags) {
    if (genCfi && !(flags & kUseFp)) {
      fprintf(stderr, "E: CFI directives may be generated correctly when FP is used\n");
      return false;
    }
    if ((flags & (kDoLinearScanRegAlloc | kDoColorRegAlloc)) == (kDoLinearScanRegAlloc | kDoColorRegAlloc)) {
      fprintf(stderr, "W: both linear scan and graph-color are specified; using color\n");
      ClearOption(flags, kDoLinearScanRegAlloc);
    }
    return true;
  }

  inline bool GenDef() const {
    return gflags_ & kCMacroDef;
  }

  inline bool GenGctib() const {
    return gflags_ & kGctib;
  }

  inline bool GenGrootList() const {
    return gflags_ & kGrootList;
  }

  inline bool GenPrimorList() const {
    return gflags_ & kPrimorList;
  }

  inline bool GenYieldpoint() const {
    return gflags_ & kGenYieldPoint;
  }

  inline bool GenLocalRC() const {
    return gflags_ & kGenLocalRc;
  }

  inline bool DoItQuietly() const {
    return CGOptions::quiet;
  }

  inline bool DoZeroExtend() const {
    return options_ & kZeroExtend;
  }

  inline bool DoConstFold() const {
    return options_ & kConstFold;
  }

  inline bool DoEmitCode() const {
    return (options_ & kDoCg) != 0;
  }

  inline bool GenerateExceptionHandlingCode() const {
    return CGOptions::genEH;
  }

  inline bool UseFP() const {
    return true;
  }

  inline bool DoLinearScanRegisterAllocation() const {
    return (options_ & kDoLinearScanRegAlloc) != 0;
  }
  inline bool DoColoringBasedRegisterAllocation() const {
    return (options_ & kDoColorRegAlloc) != 0;
  }

  inline bool GeneratePositionIndependentExecutable() const {
    return (options_ & kGenPie) != 0;
  }

  inline bool GenerateVerboseAsm() const {
    return (options_ & kVerboseAsm) != 0;
  }

  inline bool GenerateTestInfo() const {
    return (options_ & kGenTestInfo) != 0;
  }

  inline bool GenerateDebugFriendlyCode() const {
    return true;
  }

  inline bool DoPrologueEpilogue() const {
    return (options_ & kProEpilogueOpt) != 0;
  }

  inline bool UseFastunwind() const {
    return (options_ & kUseFastunwind) != 0;
  }

  inline bool AddStackGuard() const {
    return (options_ & kUseStackGuard) != 0;
  }

  inline bool WithLoc() const {
    return (options_ & kWithLoc) != 0;
  }

  inline bool WithDwarf() const {
    return (options_ & kWithDwarf) != 0;
  }

  inline bool WithSrc() const {
    return (options_ & kWithSrc) != 0;
  }

  inline bool WithMpl() const {
    return (options_ & kWithMpl) != 0;
  }

  inline bool WithAsm() const {
    return (options_ & kWithAsm) != 0;
  }

  inline bool NeedInsertInstrumentationFunction() const {
    return (options_ & kInsertCall) != 0;
  }

  inline bool InstrumentWithDebugTraceCall() const {
    return (options_ & kAddDebugTrace) != 0;
  }

  inline bool InstrumentWithProfile() const {
    return (options_ & kAddFuncProfile) != 0;
  }

  inline bool DoCheckSOE() const {
    return (options_ & kSoeCheckInsert) != 0;
  }

  inline bool SuppressFileInfo() const {
    return (options_ & kSuppressFileinfo) != 0;
  }

  inline bool ConvertLocalsToPRegs() const {
    return (options_ & kConvertLocalsToPregs) != 0;
  }

  inline bool DoDumpCFG() const {
    return (options_ & kDumpCfg) != 0;
  }

  inline bool DoDumpSSADefs() const {
    return (options_ & kDumpSsaDef) != 0;
  }

  void SetDefaultOptions(const maple::MIRModule *mod);
  static bool DumpPhase(const char *phase);

 private:
  void SplitPhases(const char *str, std::unordered_set<std::string> &set);
  void GetRange(const char *str, unsigned long *, const char *);
};

#define IS_STR_IN_SET(SET, NAME) (CGOptions::SET.find(NAME) != CGOptions::SET.end())

#define CGDEBUGFUNC(f)                                                 \
  (IS_STR_IN_SET(dumpPhases, PhaseName()) && \
   (CGOptions::dumpFunc.compare("*") == 0 || f->GetName().find(CGOptions::dumpFunc) != string::npos))

#ifndef TRACE_PHASE
#define TRACE_PHASE (IS_STR_IN_SET(CGOptions::dumpPhases, PhaseName().c_str()))
#endif

#endif
