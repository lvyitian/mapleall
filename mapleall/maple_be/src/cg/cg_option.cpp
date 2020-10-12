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

#include <iostream>
#include <fstream>
#include <cstring>
#include <unordered_map>
#include "mpl_logging.h"
#include "cg_option.h"
#include "parser_opt.h"
#include "mir_parser.h"
#include "option_parser.h"

using namespace std;
using namespace mapleOption;

#define MPLCG_VERSION "mplcg  (CBG Programming Platform TD V100R001C00B824SP01)  1.1.824.1 20181213"

bool CGOptions::dumpBefore = false;
bool CGOptions::dumpAfter = false;
bool CGOptions::timePhases = false;
std::unordered_set<std::string> CGOptions::dumpPhases = {};
std::unordered_set<std::string> CGOptions::skipPhases = {};
std::unordered_map<string, std::vector<string>> CGOptions::cyclePatternMap = {};
string CGOptions::skipFrom = "";
string CGOptions::skipAfter = "";
string CGOptions::dumpFunc = "*";
string CGOptions::profileData = "";
string CGOptions::profileFuncData = "";
string CGOptions::profileClassData = "";
string CGOptions::duplicateAsmFile = "";
string CGOptions::literalProFile = "";
string CGOptions::classMetaProFile = "";
string CGOptions::methodMetaProFile = "";
string CGOptions::fieldMetaProFile = "";
string CGOptions::globalVarProFile = "";
unsigned long CGOptions::spillRanges[2] = { 0, 0 };
unsigned long CGOptions::fastAllocMode = 0;  // 0: fast, 1: spill all
unsigned long CGOptions::lsraSimdMode = 2;   // 0: 64bit only, 1: 64/32 both, 2: off
bool CGOptions::fastAlloc = false;
bool CGOptions::spillRange = false;
unsigned long CGOptions::range[2] = { 0, 0 };
unsigned long CGOptions::lsraBbOptSize = 150000;
unsigned long CGOptions::lsraInsnOptSize = 200000;
unsigned long CGOptions::overlapNum = 28;
bool CGOptions::useBarriersForVolatile = false;
bool CGOptions::useRange = false;
bool CGOptions::inRange = false;
bool CGOptions::quiet = false;
bool CGOptions::doZeroExtend = false;
bool CGOptions::doConstFold = true;
bool CGOptions::exclusiveEh = false;
bool CGOptions::doLiveAnalysisEh = true;
bool CGOptions::doEbo = false;
bool CGOptions::doCfgo = false;
bool CGOptions::doIco = false;
bool CGOptions::doStoreLoadOpt = false;
bool CGOptions::doPreLsraOpt = false;
bool CGOptions::doGlobalOpt = false;
bool CGOptions::doPostLsraOpt = false;
bool CGOptions::doLocalRefSpill = false;
bool CGOptions::doLvarPathOpt = false;
bool CGOptions::checkarraystore = false;
bool CGOptions::doCalleeToSpill = false;
bool CGOptions::doStructFpInInt = true;
bool CGOptions::dumpOLog = false;
bool CGOptions::doPrePeephole = false;
bool CGOptions::doPeephole = false;
bool CGOptions::doPreSchedule = false;
bool CGOptions::doSchedule = false;
bool CGOptions::genEH = true;
bool CGOptions::genCfi = true;
bool CGOptions::doPIC = false;
bool CGOptions::noDupBB = false;
bool CGOptions::emitCyclePattern = false;
bool CGOptions::insertYieldpoint = false;
bool CGOptions::maplelinker = false;
bool CGOptions::replaceasm = false;
bool CGOptions::emitBlockMarker = true;
bool CGOptions::printLowerIR = false;
bool CGOptions::printFunction = false;
bool CGOptions::doSimplePrint = false;
bool CGOptions::nativeopt = false;
bool CGOptions::withDwarf = false;

std::vector<std::string> ehExclusiveFunctionName;  // we don't do exception handling in this list
static void ParseExclusiveFunc(const string &filename) {
  ifstream file(filename.c_str());
  string content;
  while (file >> content) {
    ehExclusiveFunctionName.push_back(content);
  }
}

static void ParseCyclePattern(const string &filename) {
  ifstream file(filename.c_str());
  string content;
  string classStr("class: ");
  while (getline(file, content)) {
    if (content.compare(0, classStr.length(), classStr) == 0) {
      vector<string> classPatternContent;
      string patternContent;
      while (getline(file, patternContent)) {
        if (patternContent.length() == 0) {
          break;
        }
        classPatternContent.push_back(patternContent);
      }
      string className = content.substr(classStr.length());
      CGOptions::cyclePatternMap[className] = move(classPatternContent);
    }
  }
}

template <class T, class U>
inline static void SetOrClear(T &dest, U flag, bool truth) {
  if (truth) {
    dest |= flag;
  } else {
    dest &= ~flag;
  }
}

void CGOptions::GetRange(const char *str, unsigned long myrange[], const char *cmd) {
  std::string s = str;
  size_t comma = s.find_first_of(",", 0);
  if (comma != string::npos) {
    myrange[0] = std::stoul(s.substr(0, comma), nullptr);
    myrange[1] = std::stoul(s.substr(comma + 1, string::npos - (comma + 1)), nullptr);
  }
  if (myrange[0] > myrange[1]) {
    fprintf(stderr, "invalid values for %s=%lu,%lu\n", cmd, myrange[0], myrange[1]);
    exit(1);
  }
}

enum OptionIndex {
  kUnknown,
  kHelp,
  kVersion,
  kCgen,
  kQuiet,
  kEh,
  FP,
  kCfi,
  kLiveAnalysisEh,
  kEbo,
  kCfgo,
  kIco,
  kPrepeep,
  kSlo,
  kGo,
  kPeep,
  kPreSchedule,
  kSchedule,
  kPrelsraopt,
  kPostlsraopt,
  kNativeOpt,
  kLocalrefSpill,
  kLocalrefvarPathOpt,
  kOptcallee,
  kStructFpInInt,
  kPie,
  PIC,
  VERBOSE,
  kTestInfo,
  kInsertCall,
  kTrace,
  kClassList,
  kGenDef,
  kGenGctib,
  kBarrier,
  kGenGrootList,
  kGenPrimorList,
  kRaLinear,
  kRaColor,
  kZeroextend,
  kConstfold,
  kSuppressFinfo,
  kEhList,
  kObjMap,
  kDumpcfg,
  kDumpBefore,
  kDumpAfter,
  kTimePhases,
  kDumpFunc,
  kDebuggingInfo,
  kGdbfriendly,
  kFastunwind,
  kStackguard,
  kDebugGenDwarf,
  kDebugUseSrc,
  kDebugUseMix,
  kDebugAsmMix,
  kProfilingInfo,
  kProfile,
  O0,
  O1,
  O2,
  kLsraBb,
  kLsraInsn,
  kLsraOverlap,
  kDumpssadef,
  kLocalvarsToPregs,
  kDumpOlog,
  kProepilogue,
  kYieldPoing,
  kLocalRc,
  kRange,
  kFastAlloc,
  kSimdMode,
  kSpillRange,
  kCheckComplete,
  kDuplicateBb,
  kProfileData,
  kCyclePatternList,
  kDuplicateToDelPlt,
  kMapleLinker,
  kReplaceAsm,
  kLiteralProfile,
  kStaticFieldsProfile,
  kEmitBlockMarker,
  kEmitLocalBlockMarker,
  kClassMetaProfile,
  kMethodMetaProfile,
  kInsertSoe,
  kCheckArraystore,
  kFieldMetaProfile,
  kPrintLowerIR,
  kPrintFunction,
  kPrintSimple,
  kDumpPhases,
  kSkipPhases,
  kSkipFrom,
  kSkipAfter,
  kGenMirMpl,
};

enum typeIndex { OTI_DISABLED = 0, OTI_ENABLED = 1 };

const Descriptor kUsage[] = {
  { kUnknown, 0, "", "", kBuildTypeAll, kArgCheckPolicyUnknown,
    "USAGE:  mplcg [options] *.mpl\n\n"
    "OPTIONS:" },
  { kHelp, 0, "h", "help", kBuildTypeAll, kArgCheckPolicyNone, "  -h, --help                          Print usage and exit" },

  { kVersion, 0, "v", "version", kBuildTypeAll, kArgCheckPolicyNone, "  -v, --version                       Print mplcg vesion and exit" },

  { kCgen, OTI_ENABLED, "", "cg", kBuildTypeAll, kArgCheckPolicyNone, "  --cg                                Generate the output .s file" },
  { kCgen, OTI_DISABLED, "", "no-cg", kBuildTypeAll, kArgCheckPolicyNone, "  --no-cg\n" },

  { kMapleLinker, OTI_ENABLED, "", "maplelinker", kBuildTypeAll, kArgCheckPolicyNone,
    "  --maplelinker                       Generate the MapleLinker .s format" },
  { kReplaceAsm, OTI_ENABLED, "", "replaceasm", kBuildTypeAll, kArgCheckPolicyNone,
    "  --replaceasm                        Replace the the assembly code" },

  { kQuiet, OTI_ENABLED, "", "quiet", kBuildTypeAll, kArgCheckPolicyNone,
    "  --quiet                             Be quiet (don't output debug messages)" },
  { kQuiet, OTI_DISABLED, "", "no-quiet", kBuildTypeAll, kArgCheckPolicyNone, "  --no-quiet\n" },

  { kEh, OTI_ENABLED, "", "eh", kBuildTypeAll, kArgCheckPolicyNone,
    "  --eh                                Generate exception handling related code" },
  { kEh, OTI_DISABLED, "", "no-eh", kBuildTypeAll, kArgCheckPolicyNone, "  --no-eh\n" },

  { FP, OTI_ENABLED, "", "fp", kBuildTypeAll, kArgCheckPolicyNone,
    "  --fp                                Use use a separate frame pointer; use stack pointer as frame pointer" },
  { FP, OTI_DISABLED, "", "no-fp", kBuildTypeAll, kArgCheckPolicyNone, "  --no-fp\n" },

  { kCfi, OTI_ENABLED, "", "cfi", kBuildTypeAll, kArgCheckPolicyNone, "  --cfi                               Generate .cfi directives" },
  { kCfi, OTI_DISABLED, "", "no-cfi", kBuildTypeAll, kArgCheckPolicyNone, "  --no-cfi\n" },

  { kLiveAnalysisEh, OTI_ENABLED, "", "live-eh", kBuildTypeAll, kArgCheckPolicyNone,
    "  --live-eh                           Perform Live analysis inlcuding EH blocks" },
  { kLiveAnalysisEh, OTI_DISABLED, "", "no-live-eh", kBuildTypeAll, kArgCheckPolicyNone, "  --no-live-eh\n" },

  { kEbo, OTI_ENABLED, "", "ebo", kBuildTypeAll, kArgCheckPolicyNone,
    "  --ebo                               Perform Extend block optimization" },
  { kEbo, OTI_DISABLED, "", "no-ebo", kBuildTypeAll, kArgCheckPolicyNone, "  --no-ebo\n" },

  { kCfgo, OTI_ENABLED, "", "cfgo", kBuildTypeAll, kArgCheckPolicyNone,
    "  --cfgo                              Perform control flow optimization" },
  { kCfgo, OTI_DISABLED, "", "no-cfgo", kBuildTypeAll, kArgCheckPolicyNone, "  --no-cfgo\n" },

  { kIco, OTI_ENABLED, "", "ico", kBuildTypeAll, kArgCheckPolicyNone,
    "  --ico                              Perform if-conversion optimization" },
  { kIco, OTI_DISABLED, "", "no-ico", kBuildTypeAll, kArgCheckPolicyNone, "  --no-ico\n" },

  { kSlo, OTI_ENABLED, "", "storeloadopt", kBuildTypeAll, kArgCheckPolicyNone,
    "  --storeloadopt                              Perform global store-load optimization" },
  { kSlo, OTI_DISABLED, "", "no-storeloadopt", kBuildTypeAll, kArgCheckPolicyNone, "  --no-storeloadopt\n" },

  { kGo, OTI_ENABLED, "", "globalopt", kBuildTypeAll, kArgCheckPolicyNone,
    "  --globalopt                              Perform global optimization" },
  { kGo, OTI_DISABLED, "", "no-globalopt", kBuildTypeAll, kArgCheckPolicyNone, "  --no-globalopt\n" },

  { kDumpOlog, OTI_ENABLED, "", "dump-olog", kBuildTypeAll, kArgCheckPolicyNone,
    "  --dump-olog                          Dump CFGO and ICO debug information" },
  { kDumpOlog, OTI_DISABLED, "", "no-dump-olog", kBuildTypeAll, kArgCheckPolicyNone, "  --no-dump-olog\n" },

  { kPeep, OTI_ENABLED, "", "peep", kBuildTypeAll, kArgCheckPolicyNone,
    "  --peep                              Perform peephole optimization after RA" },
  { kPeep, OTI_DISABLED, "", "no-peep", kBuildTypeAll, kArgCheckPolicyNone, "  --no-peep\n" },

  { kPrepeep, OTI_ENABLED, "", "prepeep", kBuildTypeAll, kArgCheckPolicyNone,
    "  --prepeep                              Perform peephole optimization before RA" },
  { kPrepeep, OTI_DISABLED, "", "no-prepeep", kBuildTypeAll, kArgCheckPolicyNone, "  --no-prepeep\n" },

  { kPreSchedule, OTI_ENABLED, "", "preschedule", kBuildTypeAll, kArgCheckPolicyNone,
    "  --preschedule                              Perform prescheduling" },
  { kPreSchedule, OTI_DISABLED, "", "no-preschedule", kBuildTypeAll, kArgCheckPolicyNone, "  --no-preschedule\n" },

  { kSchedule, OTI_ENABLED, "", "schedule", kBuildTypeAll, kArgCheckPolicyNone,
    "  --schedule                              Perform scheduling" },
  { kSchedule, OTI_DISABLED, "", "no-schedule", kBuildTypeAll, kArgCheckPolicyNone, "  --no-schedule\n" },

  { kPrelsraopt, OTI_ENABLED, "", "prelsra", kBuildTypeAll, kArgCheckPolicyNone,
    "  --prelsra                           Perform live interval simplification in LSRA\n" },
  { kPrelsraopt, OTI_DISABLED, "", "no-prelsra", kBuildTypeAll, kArgCheckPolicyNone, "  --no-prelsra\n" },

  { kPostlsraopt, OTI_ENABLED, "", "postlsra", kBuildTypeAll, kArgCheckPolicyNone,
    "  --postlsra                           Perform pattern matching opt right after LSRA\n" },
  { kPostlsraopt, OTI_DISABLED, "", "no-postlsra", kBuildTypeAll, kArgCheckPolicyNone, "  --no-postlsra\n" },

  { kNativeOpt, OTI_ENABLED, "", "nativeopt", kBuildTypeAll, kArgCheckPolicyNone,
    "  --nativeopt                              Enable native opt" },
  { kNativeOpt, OTI_DISABLED, "", "no-nativeopt", kBuildTypeAll, kArgCheckPolicyNone,
    "  --no-nativeopt                        Disable native opt\n" },

  { kLocalrefSpill, OTI_ENABLED, "", "lsra-lvarspill", kBuildTypeAll, kArgCheckPolicyNone,
    "  --lsra-lvarspill                           Perform LSRA spill using local ref var stack locations\n" },
  { kLocalrefSpill, OTI_DISABLED, "", "no-lsra-lvarspill", kBuildTypeAll, kArgCheckPolicyNone, "  --no-lsra-lvarspill\n" },

  { kLocalrefvarPathOpt, OTI_ENABLED, "", "lvaropt", kBuildTypeAll, kArgCheckPolicyNone,
    "  --lvaropt                           Perform path based local ref var stack location optimization\n" },
  { kLocalrefvarPathOpt, OTI_DISABLED, "", "no-lvaropt", kBuildTypeAll, kArgCheckPolicyNone, "  --no-lvaropt\n" },

  { kOptcallee, OTI_ENABLED, "", "lsra-optcallee", kBuildTypeAll, kArgCheckPolicyNone,
    "  --lsra-optcallee                           Spill callee if only one def to use\n" },
  { kOptcallee, OTI_DISABLED, "", "no-lsra-optcallee", kBuildTypeAll, kArgCheckPolicyNone, "  --no-lsra-optcallee\n" },
  { kStructFpInInt, OTI_ENABLED, "", "allow-struct-parm-in-fp", kBuildTypeAll, kArgCheckPolicyNone,
    "  --allow-struct-parm-in-fp                   Pass struct in int regs instead of fp regs\n" },
  { kStructFpInInt, OTI_DISABLED, "", "no-allow-struct-parm-in-fp", kBuildTypeAll, kArgCheckPolicyNone, "  --no-allow-struct-parm-in-fp" },
  { kPie, OTI_ENABLED, "", "pie", kBuildTypeAll, kArgCheckPolicyNone,
    "  --pie                               Generate position-independent executable" },
  { kPie, OTI_DISABLED, "", "no-pie", kBuildTypeAll, kArgCheckPolicyNone, "  --no-pie\n" },

  { PIC, OTI_ENABLED, "", "fpic", kBuildTypeAll, kArgCheckPolicyNone,
    "  --fpic                              Generate position-independent shared library" },
  { PIC, OTI_DISABLED, "", "no-fpic", kBuildTypeAll, kArgCheckPolicyNone, "  --no-fpic\n" },

  { VERBOSE, OTI_ENABLED, "", "verbose-asm", kBuildTypeAll, kArgCheckPolicyNone,
    "  --verbose-asm                       Add comments to asm output" },
  { VERBOSE, OTI_DISABLED, "", "no-verbose-asm", kBuildTypeAll, kArgCheckPolicyNone, "  --no-verbose-asm\n" },

  { kTestInfo, OTI_ENABLED, "", "testinfo", kBuildTypeAll, kArgCheckPolicyNone,
    "  --testinfo                           Add test comments to asm output for test framework" },
  { kTestInfo, OTI_DISABLED, "", "no-testinfo", kBuildTypeAll, kArgCheckPolicyNone, "  --no-testinfo\n" },

  { kObjMap, OTI_ENABLED, "", "objmap", kBuildTypeAll, kArgCheckPolicyNone,
    "  --objmap                            Create object maps (GCTIBs) inside the main output (.s) file" },
  { kObjMap, OTI_DISABLED, "", "no-objmap", kBuildTypeAll, kArgCheckPolicyNone, "  --no-objmap\n" },

  { kYieldPoing, OTI_ENABLED, "", "yieldpoint", kBuildTypeAll, kArgCheckPolicyNone,
    "  --yieldpoint                        Generate yieldpoints [default]" },
  { kYieldPoing, OTI_DISABLED, "", "no-yieldpoint", kBuildTypeAll, kArgCheckPolicyNone, "  --no-yieldpoint\n" },

  { kProepilogue, OTI_ENABLED, "", "proepilogue", kBuildTypeAll, kArgCheckPolicyNone,
    "  --proepilogue                     Do tail call optimization and eliminate unnecessary prologue and epilogue." },
  { kProepilogue, OTI_DISABLED, "", "no-proepilogue", kBuildTypeAll, kArgCheckPolicyNone, "  --no-proepilogue\n" },

  { kLocalRc, OTI_ENABLED, "", "local-rc", kBuildTypeAll, kArgCheckPolicyNone,
    "  --local-rc                          Handle Local Stack RC [default]" },
  { kLocalRc, OTI_DISABLED, "", "no-local-rc", kBuildTypeAll, kArgCheckPolicyNone, "  --no-local-rc\n" },

  { kInsertCall, 0, "", "insert-call", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --insert-call=name                  Insert a call to the named function" },
  { kTrace, 0, "", "add-debug-trace", kBuildTypeAll, kArgCheckPolicyNone,
    "  --add-debug-trace                   Instrument the output .s file to print call traces at runtime" },
  { kProfile, 0, "", "add-func-profile", kBuildTypeAll, kArgCheckPolicyNone,
    "  --add-func-profile                   Instrument the output .s file to record func at runtime" },
  { kClassList, 0, "", "class-list-file", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --class-list-file=class_list_file   Set the class list file for the following generation options,\n"
    "                                      if not given, generate for all visible classes\n" },
  { kGenDef, OTI_ENABLED, "", "gen-c-macro-def", kBuildTypeAll, kArgCheckPolicyNone,
    "  --gen-c-macro-def                   Generate a .def file that contains extra type metadata, including the\n"
    "                                      class instance sizes and field offsets (default)" },
  { kGenDef, OTI_DISABLED, "", "no-gen-c-macro-def", kBuildTypeAll, kArgCheckPolicyNone, "  --no-gen-c-macro-def\n" },
  { kGenGctib, OTI_ENABLED, "", "gen-gctib-file", kBuildTypeAll, kArgCheckPolicyNone,
    "  --gen-gctib-file                    Generate a separate .s file for GCTIBs. Usually used together with\n"
    "                                      --no-objmap (not implemented yet)" },
  { kGenGctib, OTI_DISABLED, "", "gen-gctib-file", kBuildTypeAll, kArgCheckPolicyNone, "  --no-gen-gctib-file\n" },
  { kGenGrootList, OTI_ENABLED, "", "gen-groot-list", kBuildTypeAll, kArgCheckPolicyNone,
    "  --gen-groot-list                    Generate a list of global variables of ptr to class (and ref) type.\n"
    "                                      For GC root enumeration.  Output has suffix '.groots.txt'. (default)" },
  { kGenGrootList, OTI_DISABLED, "", "no-gen-groot-list", kBuildTypeAll, kArgCheckPolicyNone, "  --no-gen-groot-list\n" },
  { kGdbfriendly, OTI_ENABLED, "", "gdb-friendly", kBuildTypeAll, kArgCheckPolicyNone,
    "  --gdb-friendly                      Generate GDB friendly code (the default for -O0/-O1; it may insert extra instructions" },
  { kGdbfriendly, OTI_DISABLED, "", "no-gdb-friendly", kBuildTypeAll, kArgCheckPolicyNone,
    "  --no-gdb-friendly                   Do not generate GDB friendly code" },
  { kFastunwind, OTI_ENABLED, "", "use-fastunwind", kBuildTypeAll, kArgCheckPolicyNone,
    "  --use-fastunwind                    use fast unwind for EH" },
  { kStackguard, OTI_ENABLED, "", "stackguard", kBuildTypeAll, kArgCheckPolicyNone, "  -stackguard                         add stack guard" },
  { kDebuggingInfo, 0, "g", "", kBuildTypeAll, kArgCheckPolicyNone, "  -g                                  Generate debug infomation" },
  { kDebugGenDwarf, 0, "", "gdwarf", kBuildTypeAll, kArgCheckPolicyNone, "  --gdwarf                            Generate dwarf infomation" },
  { kDebugUseSrc, 0, "", "gsrc", kBuildTypeAll, kArgCheckPolicyNone,
    "  --gsrc                              Use original source file instead of mpl file for debugging" },
  { kDebugUseMix, 0, "", "gmixedsrc", kBuildTypeAll, kArgCheckPolicyNone,
    "  --gmixedsrc                         Use both original source file and mpl file for debugging" },
  { kDebugAsmMix, 0, "", "gmixedasm", kBuildTypeAll, kArgCheckPolicyNone,
    "  --gmixedasm                         Comment out both original source file and mpl file for debugging" },
  { kProfilingInfo, 0, "p", "", kBuildTypeAll, kArgCheckPolicyNone, "  -p                                  Generate profiling infomation" },

  { kRaLinear, 0, "", "with-ra-linear-scan", kBuildTypeAll, kArgCheckPolicyNone,
    "  --with-ra-linear-scan               Do linear-scan register allocation" },
  { kRaColor, 0, "", "with-ra-graph-color", kBuildTypeAll, kArgCheckPolicyNone,
    "  --with-ra-graph-color    Do coloring-based register allocation" },
  { kZeroextend, OTI_ENABLED, "", "zero-extend", kBuildTypeAll, kArgCheckPolicyNone, "  --zero-extend                     Enable zero extension" },
  { kZeroextend, OTI_DISABLED, "","no-zero-extend", kBuildTypeAll, kArgCheckPolicyNone, "  --no-zero-extend                  Disable zero extension" },
  { kConstfold, OTI_ENABLED, "", "const-fold", kBuildTypeAll, kArgCheckPolicyNone, "  --const-fold                        Enable constant folding" },
  { kConstfold, OTI_DISABLED, "", "no-const-fold", kBuildTypeAll, kArgCheckPolicyNone, "  --no-const-fold                     Disable constant folding" },
  { kEhList, 0, "", "eh-exclusive-list", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --eh-exclusive-list=list_file       For generating gold files in unit testing" },
  { O0, 0, "O0", "", kBuildTypeAll, kArgCheckPolicyNone,
    "  -O0                                 make debugging produce the expected results. This is the default." },
  { O1, 0, "O1", "", kBuildTypeAll, kArgCheckPolicyNone, "  -O1                                 do some optimization" },
  { O2, 0, "O2", "", kBuildTypeAll, kArgCheckPolicyNone,
    "  -O2                                 do more optimization including linear scan register allocation" },
  { kLsraBb, 0, "", "lsra-bb", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --lsra-bb=NUM             Switch to spill mode if number of bb in function exceeds NUM" },
  { kLsraInsn, 0, "", "lsra-insn", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --lsra-insn=NUM             Switch to spill mode if number of instructons in function exceeds NUM" },
  { kLsraOverlap, 0, "", "lsra-overlap", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --lsra-overlap=NUM             overlap NUM to decide pre spill in lsra" },
  { kSuppressFinfo, 0, "", "suppress-fileInfo", kBuildTypeAll, kArgCheckPolicyNone,
    "  --suppress-fileInfo                 For generating gold files in unit testing" },
  { kDumpcfg, 0, "", "dump-cfg", kBuildTypeAll, kArgCheckPolicyNone, "  --dump-cfg" },
  { kDumpPhases, 0, "", "dump-phases", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --dump-phases=PHASENAME,...      Enable debug trace for specified phases in the comma separated list" },
  { kSkipPhases, 0, "", "skip-phases", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --skip-phases=PHASENAME,...       Skip the phases specified in the comma separated list" },
  { kSkipFrom, 0, "", "skip-from", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --skip-from=PHASENAME             Skip the rest phases from PHASENAME(included)" },
  { kSkipAfter, 0, "", "skip-after", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --skip-after=PHASENAME            Skip the rest phases after PHASENAME(excluded)" },
  { kDumpFunc, 0, "", "dump-func", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --dump-func=FUNCNAME  Dump/trace only for functions whose names contain FUNCNAME as substring\n"
    "(can only specify once)" },
  { kDumpBefore, 0, "", "dump-before", kBuildTypeAll, kArgCheckPolicyNone,
    "  --dump-before                     Do extra IR dump before the specified phase" },
  { kDumpAfter, 0, "", "dump-after", kBuildTypeAll, kArgCheckPolicyNone,
    "  --dump-after                      Do extra IR dump after the specified phase" },
  { kTimePhases, 0, "", "time-phases", kBuildTypeAll, kArgCheckPolicyNone,
    "  --time-phases                     Collect compilation time stats for each phase" },
  { kBarrier, 0, "", "use-barriers-for-volatile", kBuildTypeAll, kArgCheckPolicyNone,
    "  --use-barriers-for-volatile  Optimize volatile load/str" },
  { kRange, 0, "", "range", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --range=NUM0,NUM1                 Optimize only functions in the range [NUM0, NUM1]" },
  { kFastAlloc, 0, "", "fast-alloc", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --fast-alloc=[0/1] O2 RA fast mode, set to 1 to spill all registers" },
  { kSimdMode, 0, "", "lsra-simd", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --lsra-simd=[0/1/2] O2 RA simd spill mode, 0: 64bit 1: 32/64bit 2: off" },
  { kSpillRange, 0, "", "spill_range", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --spill_range=NUM0,NUM1                 O2 RA spill registers in the range [NUM0, NUM1]" },
  { kDuplicateBb, 0, "", "no-dup-bb", kBuildTypeAll, kArgCheckPolicyNone,
    "  --no-dup-bb                      Allow cfg optimizer to duplicate bb" },
  { kDumpssadef, 0, "", "dump-ssadef", kBuildTypeAll, kArgCheckPolicyNone, "  --dump-ssadef" },
  { kPrintLowerIR, 0, "", "print-ir", kBuildTypeAll, kArgCheckPolicyNone, "  --print-ir" },
  { kPrintFunction, 0, "", "print-func", kBuildTypeAll, kArgCheckPolicyNone, "  --print-func" },
  { kPrintSimple, 0, "", "print-simple", kBuildTypeAll, kArgCheckPolicyNone,                        "  --print-simple" },
  { kCheckComplete, 0, "", "check-complete", kBuildTypeAll, kArgCheckPolicyNone, "  --check-complete                  Check incomplete types" },
#if DEBUG
  { kLocalvarsToPregs, 0, "", "convert-java-localvars-to-pregs", kBuildTypeAll, kArgCheckPolicyNone, "  --convert-java-localvars-to-pregs" },
#endif
  { kProfileData, 0, "", "profile_data", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --profile_data=FUNCINFOFILE[:]CLASSINFOFILE       use profile info to sort funcbody and class static field" },
  { kCyclePatternList, 0, "", "cycle-pattern-list", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --cycle-pattern-list=list_file       For generating cycle pattern meta" },
  { kDuplicateToDelPlt, 0, "", "duplicate_asm_list", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --duplicate_asm_list=list_file       Duplicate asm functions to delete plt call" },
  { kLiteralProfile, 0, "", "literal_profile", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --literal_profile=list_file       For literal layout optimization" },
  { kClassMetaProfile, 0, "", "classmeta_profile", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --classmeta_profile=list_file       For classmeta layout optimization" },
  { kMethodMetaProfile, 0, "", "methodmeta_profile", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --methods_profile=list_file       For methodmeta layout optimization" },
  { kFieldMetaProfile, 0, "", "fieldmeta_profile", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --fields_profile=list_file       For fieldmeta layout optimization" },
  { kStaticFieldsProfile, 0, "", "staticfields_profile", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --staticfields_profile=list_file       For staticFields layout optimization" },
  { kEmitBlockMarker, 0, "", "block-marker", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --block-marker     Emit block marker symbols in emitted assembly files" },
  { kEmitLocalBlockMarker, 0, "", "local-marker", kBuildTypeAll, kArgCheckPolicyNone,
    "  --local-bmarker     Emit block marker symbols as local in emitted assembly files" },
  { kInsertSoe, 0, "", "soe-check", kBuildTypeAll, kArgCheckPolicyNone,
    "  --soe-check                  Insert a soe check instruction[default off]" },
  { kCheckArraystore, OTI_ENABLED, "", "check-arraystore", kBuildTypeAll, kArgCheckPolicyNone,
    "  --check-arraystore                check arraystore exception[default off]" },
  { kGenMirMpl, 0, "", "gen-mir-mpl", kBuildTypeAll, kArgCheckPolicyNone,
    "  --gen-mir-mpl                     generate corresponding ASCII .mpl file along with .mir generation" },
  { 0, 0, nullptr, nullptr, kBuildTypeAll, kArgCheckPolicyUnknown, nullptr }
};

static void PrintCGVersionAndExit() {
  LogInfo::MapleLogger() << MPLCG_VERSION << endl;
  exit(1);
}

// Set default options according to different languages.
void CGOptions::SetDefaultOptions(const maple::MIRModule *mod) {
  if (mod->IsJavaModule()) {
    gflags_ = gflags_ | kGenYieldPoint | kGenLocalRc | kGrootList | kPrimorList;
  }
  insertYieldpoint = GenYieldpoint();
}

bool CGOptions::ParseOptions(int argc, char **argv, string &fileName) {
  bool result = true;
  OptionParser optionParser(kUsage);
  int ret = optionParser.Parse(argc, argv);
  CHECK_FATAL(ret == ErrorCode::kErrorNoError, "option parser error");
  options_ = kDefaultOptions;
  SetOption(options_, kWithMpl);
  gflags_ = kDefaultGflags;
  // mplcg checks completeness by default
  parserOpt |= kCheckCompleteType;
  for (auto opt : optionParser.GetOptions()) {
    switch (opt.Index()) {
      case kHelp:
        if (opt.Args().empty()) {
          optionParser.PrintUsage();
          result = false;
        }
        break;
      case kVersion:
        PrintCGVersionAndExit();
        break;
      case kFastAlloc:
        fastAlloc = true;
        fastAllocMode = std::stoul(opt.Args(), nullptr);
        break;
      case kSimdMode:
        lsraSimdMode = std::stoul(opt.Args(), nullptr);
        break;
      case kBarrier:
        useBarriersForVolatile = true;
        break;
      case kSpillRange:
        spillRange = true;
        GetRange(opt.Args().c_str(), spillRanges, "--pill_range");
        break;
      case kRange:
        useRange = true;
        GetRange(opt.Args().c_str(), range, "--range");
        break;
      case kDumpBefore:
        dumpBefore = true;
        break;
      case kDumpAfter:
        dumpAfter = true;
        break;
      case kTimePhases:
        timePhases = true;
        break;
      case kDumpFunc:
        dumpFunc = opt.Args();
        break;
      case kProfileData:
        profileData = opt.Args();
        break;
      case kDuplicateToDelPlt:
        duplicateAsmFile = opt.Args();
        break;
      case kInsertCall:
        instru_func = opt.Args();
        insert_call = true;
        break;
      case kGdbfriendly:
        (generate_gdb_friendly_code = (opt.Type() == OTI_ENABLED)) ? SetOption(options_, kDebugFriendly)
                                                                   : ClearOption(options_, kDebugFriendly);
        break;
      case kFastunwind:
        SetOption(options_, kUseFastunwind);
        break;
      case kStackguard:
        SetOption(options_, kUseStackGuard);
        break;
      case kDebuggingInfo:
        SetOption(options_, kDebugFriendly);
        SetOption(options_, kWithLoc);
        ClearOption(options_, kSuppressFileinfo);
        break;
      case kDebugGenDwarf:
        SetOption(options_, kDebugFriendly);
        SetOption(options_, kWithLoc);
        SetOption(options_, kWithDwarf);
        parserOpt |= kWithDbgInfo;
        ClearOption(options_, kSuppressFileinfo);
        withDwarf = true;
        break;
      case kDebugUseSrc:
        SetOption(options_, kDebugFriendly);
        SetOption(options_, kWithLoc);
        SetOption(options_, kWithSrc);
        ClearOption(options_, kWithMpl);
        break;
      case kDebugUseMix:
        SetOption(options_, kDebugFriendly);
        SetOption(options_, kWithLoc);
        SetOption(options_, kWithSrc);
        SetOption(options_, kWithMpl);
        break;
      case kDebugAsmMix:
        SetOption(options_, kDebugFriendly);
        SetOption(options_, kWithLoc);
        SetOption(options_, kWithSrc);
        SetOption(options_, kWithMpl);
        SetOption(options_, kWithAsm);
        break;
      case kProfilingInfo:
        SetOption(options_, kWithProfileCode);
        parserOpt |= kWithProfileInfo;
        break;
      case kRaLinear:
        SetOption(options_, kDoLinearScanRegAlloc);
        ClearOption(options_, kDoColorRegAlloc);
        break;
      case kRaColor:
        SetOption(options_, kDoColorRegAlloc);
        ClearOption(options_, kDoLinearScanRegAlloc);
        break;
      case kDumpssadef:
        SetOption(options_, kDumpSsaDef);
        break;
      case kPrintLowerIR:
        printLowerIR = true;
        break;
      case kPrintFunction:
        printFunction = true;
        break;
      case kPrintSimple:
        doSimplePrint = true;
        break;
      case kTrace:
        SetOption(options_, kAddDebugTrace);
        break;
      case kProfile:
        SetOption(options_, kAddFuncProfile);
        break;
      case kSuppressFinfo:
        SetOption(options_, kSuppressFileinfo);
        break;
      case kZeroextend:
        doZeroExtend = (opt.Type() == OTI_ENABLED);
        if (doZeroExtend) {
          SetOption(options_, kZeroExtend);
        } else {
          ClearOption(options_, kZeroExtend);
        }
        break;
      case kConstfold:
        doConstFold = (opt.Type() == OTI_ENABLED);
        if (doConstFold) {
          SetOption(options_, kConstFold);
        } else {
          ClearOption(options_, kConstFold);
        }
        break;
      case kDumpcfg:
        SetOption(options_, kDumpCfg);
        break;
      case kClassList:
        class_list_file = opt.Args();
        break;
      case kGenDef:
        SetOrClear(gflags_, kCMacroDef, opt.Type());
        break;
      case kGenGctib:
        SetOrClear(gflags_, kGctib, opt.Type());
        break;
      case kGenGrootList:
        SetOrClear(gflags_, kGrootList, opt.Type());
        break;
      case kGenPrimorList:
        SetOrClear(gflags_, kPrimorList, opt.Type());
        break;
      case kYieldPoing:
        SetOrClear(gflags_, kGenYieldPoint, opt.Type());
        break;
      case kLocalRc:
        SetOrClear(gflags_, kGenLocalRc, opt.Type());
        break;
      case kEhList:
        ehexclusivefile = string(opt.Args());
        exclusiveEh = true;
        ParseExclusiveFunc(ehexclusivefile);
        break;
      case kCyclePatternList:
        cyclepatternfile = string(opt.Args());
        emitCyclePattern = true;
        ParseCyclePattern(cyclepatternfile);
        break;
      case kCgen:
        (run_cg_flag = (opt.Type() == OTI_ENABLED)) ? SetOption(options_, kDoCg) : ClearOption(options_, kDoCg);
        break;
      case kObjMap:
        gen_objmap = (opt.Type() == OTI_ENABLED);
        break;
      case kQuiet:
        quiet = (opt.Type() == OTI_ENABLED);
        break;
      case kMapleLinker:
        maplelinker = (opt.Type() == OTI_ENABLED);
        break;
      case kReplaceAsm:
        replaceasm = (opt.Type() == OTI_ENABLED);
        break;
      case kEh:
        genEH = (opt.Type() == OTI_ENABLED);
        break;
      case FP:
        (do_with_fp = (opt.Type() == OTI_ENABLED)) ? SetOption(options_, kUseFp) : ClearOption(options_, kUseFp);
        break;
      case kEmitLocalBlockMarker:
        emitBlockMarker = false;
        break;
      case kInsertSoe:
        SetOption(options_, kSoeCheckInsert);
        break;
      case kCheckArraystore:
        checkarraystore = (opt.Type() == OTI_ENABLED);
        break;
      case kCfi:
        genCfi = (opt.Type() == OTI_ENABLED);
        break;
      case kPrepeep:
        doPrePeephole = (opt.Type() == OTI_ENABLED);
        break;
      case kPeep:
        doPeephole = (opt.Type() == OTI_ENABLED);
        break;
      case kLiveAnalysisEh:
        doLiveAnalysisEh = (opt.Type() == OTI_ENABLED);
        break;
      case kEbo:
        doEbo = (opt.Type() == OTI_ENABLED);
        break;
      case kCfgo:
        doCfgo = (opt.Type() == OTI_ENABLED);
        break;
      case kIco:
        doIco = (opt.Type() == OTI_ENABLED);
        break;
      case kSlo:
        doStoreLoadOpt = (opt.Type() == OTI_ENABLED);
        break;
      case kGo:
        doGlobalOpt = (opt.Type() == OTI_ENABLED);
        break;
      case kPreSchedule:
        doPreSchedule = (opt.Type() == OTI_ENABLED);
        break;
      case kSchedule:
        doSchedule = (opt.Type() == OTI_ENABLED);
        break;
      case kPrelsraopt:
        doPreLsraOpt = (opt.Type() == OTI_ENABLED);
        break;
      case kPostlsraopt:
        doPostLsraOpt = (opt.Type() == OTI_ENABLED);
        break;
      case kNativeOpt:
        nativeopt = false;
        break;
      case kLocalrefSpill:
        doLocalRefSpill = (opt.Type() == OTI_ENABLED);
        break;
      case kLocalrefvarPathOpt:
        doLvarPathOpt = (opt.Type() == OTI_ENABLED);
        break;
      case kOptcallee:
        doCalleeToSpill = (opt.Type() == OTI_ENABLED);
        break;
      case kStructFpInInt:
        doStructFpInInt = (opt.Type() == OTI_ENABLED);
        break;
      case kDumpOlog:
        dumpOLog = (opt.Type() == OTI_ENABLED);
        break;
      case kPie:
        (opt.Type() == OTI_ENABLED) ? SetOption(options_, kGenPie) : ClearOption(options_, kGenPie);
        break;
      case PIC:
        (doPIC = (opt.Type() == OTI_ENABLED)) ? SetOption(options_, kGenPic) : ClearOption(options_, kGenPic);
        break;
      case kDuplicateBb:
        noDupBB = true;
        break;
      case VERBOSE:
        (opt.Type() == OTI_ENABLED) ? SetOption(options_, kVerboseAsm) : ClearOption(options_, kVerboseAsm);
        break;
      case kTestInfo:
        (opt.Type() == OTI_ENABLED) ? SetOption(options_, kGenTestInfo) : ClearOption(options_, kGenTestInfo);
        break;
      case kProepilogue:
        (opt.Type() == OTI_ENABLED) ? SetOption(options_, kProEpilogueOpt) : ClearOption(options_, kProEpilogueOpt);
        break;
      case kLsraBb:
        lsraBbOptSize = std::stoul(opt.Args(), nullptr);
        break;
      case kLsraInsn:
        lsraInsnOptSize = std::stoul(opt.Args(), nullptr);
        break;
      case kLsraOverlap:
        overlapNum = std::stoul(opt.Args(), nullptr);
        break;
      case O0:
        optim_level = 0;
        SetOption(options_, kUseStackGuard);
        // const fold is not turned on even if doConstFold is initialized to true.
        break;
      case O1:
        optim_level = 1;
        doEbo = true;
        doCfgo = true;
        doIco = true;
        if (doConstFold) {
          SetOption(options_, kConstFold);
        } else {
          ClearOption(options_, kConstFold);
        }
        ClearOption(options_, kUseStackGuard);
        break;
      case O2:
        optim_level = 2;
        doEbo = true;
        doCfgo = true;
        doIco = true;
        doPrePeephole = true;
        doPeephole = true;
        doStoreLoadOpt = true;
        doGlobalOpt = true;
        doPreLsraOpt = true;
        doPostLsraOpt = true;
        doLocalRefSpill = true;
        doLvarPathOpt = true;
        doCalleeToSpill = true;
        doPreSchedule = false;
        doSchedule = false;
        if (doConstFold) {
          SetOption(options_, kConstFold);
        } else {
          ClearOption(options_, kConstFold);
        }
        SetOption(options_, kProEpilogueOpt);
        ClearOption(options_, kUseStackGuard);
        break;
      case kLocalvarsToPregs:
        SetOption(options_, kConvertLocalsToPregs);
        break;
      case kCheckComplete:
        parserOpt |= kCheckCompleteType;
        break;
      case kLiteralProfile:
        literalProFile = string(opt.Args());
        break;
      case kClassMetaProfile:
        classMetaProFile = string(opt.Args());
        break;
      case kMethodMetaProfile:
        methodMetaProFile = string(opt.Args());
        break;
      case kFieldMetaProfile:
        fieldMetaProFile = string(opt.Args());
        break;
      case kStaticFieldsProfile:
        globalVarProFile = string(opt.Args());
        break;
      case kDumpPhases:
        SplitPhases(opt.Args().c_str(), CGOptions::dumpPhases);
        break;
      case kSkipPhases:
        SplitPhases(opt.Args().c_str(), skipPhases);
        break;
      case kSkipFrom:
        skipFrom = opt.Args();
        break;
      case kSkipAfter:
        skipAfter = opt.Args();
        break;
      case kGenMirMpl:
        genMirMpl = true;
        break;
      default:
        result = false;
        ASSERT(false, "unhandled case in cgdriver");
        break;
    }
  }

  if (generate_gdb_friendly_code == true) {
    SetOption(options_, kDebugFriendly);
#if !TARGARK
    SetOption(options_, kWithLoc);
    SetOption(options_, kWithSrc);
#endif
  } else {
    ClearOption(options_, kDebugFriendly);
  }

  if (result) {
    if (optionParser.GetNonOptionsCount() != 1) {
      LogInfo::MapleLogger(kLlErr) << "expecting one .mpl file as last argument, found: ";
      for (const auto &optionArg : optionParser.GetNonOptions()) {
        LogInfo::MapleLogger(kLlErr) << optionArg << " ";
      }
      LogInfo::MapleLogger(kLlErr) << std::endl;
      result = false;
    }

    if (result) {
      fileName = optionParser.GetNonOptions().front();

      if (!CheckOptions(options_)) {
        optionParser.PrintUsage();
        result = false;
      }
    }
  }
  return result;
}

void CGOptions::SplitPhases(const char *str, std::unordered_set<std::string> &set) {
  std::string s = str;
  size_t current = 0;
  size_t next = 0;
  if (s.compare("*") == 0 || s.compare("cgir") == 0) {
    set.insert(s);
    return;
  }
  do {
    next = s.find_first_of(",", current);
    set.insert(s.substr(current, next - current));
    current = next + 1;
  } while (next != string::npos);
}

bool CGOptions::DumpPhase(const char *phase) {
  CHECK_FATAL(phase, "null cg phase ptr check");
  return (IS_STR_IN_SET(dumpPhases, "*") || IS_STR_IN_SET(dumpPhases, "cgir") || IS_STR_IN_SET(dumpPhases, phase));
}
