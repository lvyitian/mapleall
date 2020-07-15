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

#include <iostream>
#include <cstring>
#include "mpl_logging.h"
#include "me_option.h"
#include "option_parser.h"
#include "parser_opt.h"

using namespace std;
using namespace mapleOption;
using namespace maple;

std::unordered_set<std::string> MeOption::dumpPhases = {};

bool MeOption::dumpBefore = false;
bool MeOption::dumpAfter = false;
string MeOption::dumpFunc = "*";
string MeOption::skipFrom = "";
string MeOption::skipAfter = "";
unsigned long MeOption::range[2] = { 0, 0 };

bool MeOption::useRange = false;
bool MeOption::quiet = false;
bool MeOption::setCalleeHasSideEffect = false;
bool MeOption::noSteensgaard = false;
bool MeOption::noTBAA = false;
uint8_t MeOption::aliasAnalysisLevel = 3;
bool MeOption::noDot = false;
bool MeOption::stmtNum = false;
bool MeOption::noRC = false;
bool MeOption::strictNaiverc = false;
bool MeOption::rcLowering = true;
bool MeOption::noGCBar = true;
bool MeOption::realCheckCast = false;
bool MeOption::regNativeFunc = false;
bool MeOption::warnNativeFunc = false;
uint8_t MeOption::optLevel = 0;
uint32_t MeOption::parserOpt = 0;
uint32_t MeOption::epreLimit = UINT32_MAX;
uint32_t MeOption::eprePULimit = UINT32_MAX;
uint32_t MeOption::stmtprePULimit = UINT32_MAX;
uint32_t MeOption::lpreLimit = UINT32_MAX;
uint32_t MeOption::lprePULimit = UINT32_MAX;
uint32_t MeOption::pregrenameLimit = UINT32_MAX;
uint32_t MeOption::delrcPULimit = UINT32_MAX;
bool MeOption::ignoreIPA = true;
bool MeOption::epreIncludeRef = true;
bool MeOption::eprelocalrefvar = true;
bool MeOption::eprelhsivar = true;
bool MeOption::dsekeepref = false;
bool MeOption::propbase = true;
bool MeOption::propiloadref = false;
bool MeOption::propglobalref = false;
bool MeOption::propfinaliloadref = true;
bool MeOption::propiloadrefnonparm = false;
bool MeOption::lessThrowAlias = true;
bool MeOption::eaoptrc = false;
bool MeOption::nodelegaterc = false;
bool MeOption::nocondbasedrc = false;
bool MeOption::parmtoptr = false;
bool MeOption::nullcheckpre = false;
bool MeOption::clinitpre = true;
bool MeOption::dassignpre = true;
bool MeOption::assign2finalpre = false;
bool MeOption::finalFieldAlias = false;
bool MeOption::regreadAtReturn = true;
bool MeOption::propatphi = true;
bool MeOption::nativeopt = true;
bool MeOption::optdirectcall = false;
bool MeOption::enableEA = false;
bool MeOption::lprespeculate = false;
bool MeOption::spillatcatch = false;
bool MeOption::earlydecref = false;
bool MeOption::placementrc = false;
bool MeOption::strengthreduction = true;
bool MeOption::rcfre = true;
string MeOption::inlinefunclist = "";
#if MIR_JAVA
string MeOption::acquireFuncName = "";
string MeOption::releaseFuncName = "";
unsigned int MeOption::warningLevel = 0;
bool MeOption::mpltoolonly = false;
bool MeOption::mpltoolstrict = false;
bool MeOption::skipvirtualMethod = false;
#endif

enum OptionIndex {
  kUnknown,
  kHelp,
  kDumpPhases,
  kSkipPhases,
  kDumpFunc,
  kSkipFrom,
  kSkipAfter,
  kQuiet,
  kSetCalleeHasSideEffect,
  kNoSteensgaard,
  kNoTBAA,
  kAliasAnalysisLevel,
  kNoDot,
  kStmtNum,
  kRcLower,
  kNoRcLower,
  kNoRc,
  kUseRc,
  kStrictNaiveRc,
  kNoGcbar,
  kUseGcbar,
  kNoReflec,
  kStubJniFunc,
  kStubFunc,
  kWarnNativefunc,
  kDumpBefore,
  kDumpAfter,
  kRealCheckcast,
  kAcquireFunc,
  kReleaseFunc,
  kCheckComplete,
  kWarnLevel,
  kToolOnly,
  kToolStrict,
  kSkipVirtual,
  kOptL0,
  kOptL1,
  kOptL2,
  kRange,
  kEaOptRc,
  kEpreLimit,
  kEprepuLimit,
  kStmtPrepuLimit,
  kLpreLimit,
  kLprepuLimit,
  kPregrenameLimit,
  kDelrcpuLimit,
  kIgnoreIpa,
  kNoIgnoreIpa,
  kEpreIncludeRef,
  kNoEpreIncludeRef,
  kEpreLocalRefVar,
  kNoEpreLocalRefVar,
  kEprelhSivar,
  kDseKeepRef,
  kNoDseKeepRef,
  kPropBase,
  kNopropiLoadRef,
  kPropiLoadRef,
  kPropGloablRef,
  kPropfinalIloadRef,
  kPropIloadRefnonparm,
  kLessThrowAlias,
  kNodeLegateRc,
  kNocondBasedRc,
  kParmToptr,
  kNullcheckPre,
  kClinitPre,
  kDassignPre,
  kAssign2finalPre,
  kFinalFieldAlias,
  kRegReadAtReturn,
  kProPatphi,
  kNoProPatphi,
  kNativeOpt,
  kNoNativeOpt,
  kOptInterfaceCall,
  kNoOptInterfaceCall,
  kOptVirtualCall,
  kOptDirectCall,
  kEnableEa,
  kLpreSpeculate,
  kNoLpreSpeculate,
  kSpillatCatch,
  kEarlyDecref,
  kPlacementRC,
  kStrengthReduction,
  kNoStrengthReduction,
  kRCFullRedundancyElim,
  kNoRCFullRedundancyElim,
  kEaTransRef,
  kEaTransAlloc,
  kInlineHint
};

const Descriptor kUsage[] = {
  { kUnknown, 0, "", "", kBuildTypeAll, kArgCheckPolicyUnknown,
    "USAGE:  mplxxx [options] *.mpl\n\n"
    "OPTIONS:" },
  { kHelp, 0, "h", "help", kBuildTypeAll, kArgCheckPolicyNone, "  -h, --help                        Print usage and exit" },
  { kRange, 0, "", "range", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --range=NUM0,NUM1                 Optimize only functions in the range [NUM0, NUM1]" },
  { kDumpPhases, 0, "", "dump-phases", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --dump-phases=PHASENAME,...      Enable debug trace for specified phases in the comma separated list" },
  { kSkipPhases, 0, "", "skip-phases", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --skip-phases=PHASENAME,...       Skip the phases specified in the comma separated list" },
  { kDumpFunc, 0, "", "dump-func", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --dump-func=FUNCNAME              Dump/trace only for functions whose names contain FUNCNAME as substring\n"
    "                                    (can only specify once)" },
  { kSkipFrom, 0, "", "skip-from", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --skip-from=PHASENAME             Skip the rest phases from PHASENAME(included)" },
  { kSkipAfter, 0, "", "skip-after", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --skip-after=PHASENAME            Skip the rest phases after PHASENAME(excluded)" },
  { kQuiet, 0, "", "quiet", kBuildTypeAll, kArgCheckPolicyNone,
    "  --quiet                           Disable brief trace messages with phase/function names" },
  { kSetCalleeHasSideEffect, 0, "", "setCalleeHasSideEffect", kBuildTypeAll, kArgCheckPolicyNone,
    "  --setCalleeHasSideEffect          Set all the callees have side effect" },
  { kNoSteensgaard, 0, "", "noSteensgaard", kBuildTypeAll, kArgCheckPolicyNone,
    "  --noSteensgaard                   Disable Steensgaard-style alias analysis" },
  { kNoTBAA, 0, "", "noTBAA", kBuildTypeAll, kArgCheckPolicyNone, "  --noTBAA                          Disable type-based alias analysis" },
  { kAliasAnalysisLevel, 0, "", "aliasAnalysisLevel", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --aliasAnalysisLevel=NUM          Set level of alias analysis. 0: most conservative; "
    "1: Steensgaard-style alias analysis; 2: type-based alias analysis; "
    "3: Steensgaard-style and type-based alias analysis" },
  { kNoDot, 0, "", "nodot", kBuildTypeAll, kArgCheckPolicyNone,
   "  --nodot                           Disable dot file generation from cfg" },
  { kStmtNum, 0, "", "stmtNum", kBuildTypeAll, kArgCheckPolicyNone,
   "  --stmtNum                         Print MeStmt index number in IR dump" },
  { kRcLower, 0, "", "rclower", kBuildTypeAll, kArgCheckPolicyNone, "  --rclower                         Enable rc lowering" },
  { kNoRcLower, 0, "", "norclower", kBuildTypeAll, kArgCheckPolicyNone, "  --norclower                       Disable rc lowering" },
  { kNoRc, 0, "", "norc", kBuildTypeAll, kArgCheckPolicyNone, "  --norc                            Disable reference counting" },
  { kUseRc, 0, "", "userc", kBuildTypeAll, kArgCheckPolicyNone, "  --userc                           Enable reference counting [default]" },
  { kStrictNaiveRc, 0, "", "strict-naiverc", kBuildTypeAll, kArgCheckPolicyNone,
    "  --strict-naiverc                  Strict Naive RC mode, assume no unsafe multi-thread read/write racing" },
  { kNoGcbar, 0, "", "nogcbar", kBuildTypeAll, kArgCheckPolicyNone, "  --nogcbar                         Disable GC barriers [default]" },
  { kUseGcbar, 0, "", "usegcbar", kBuildTypeAll, kArgCheckPolicyNone, "  --usegcbar                        Enable GC barriers" },
  { kStubJniFunc, 0, "", "regNativeFunc", kBuildTypeAll, kArgCheckPolicyNone,
    "  --regNativeFunc                   Generate native stub function to support JNI registration and calling" },
  { kWarnNativefunc, 0, "", "warnemptynative", kBuildTypeAll, kArgCheckPolicyNone,
    "  --warnemptynative                 Generate warning and abort unimplemented native function" },
  { kDumpBefore, 0, "", "dump-before", kBuildTypeAll, kArgCheckPolicyNone,
    "  --dump-before                     Do extra IR dump before the specified phase" },
  { kDumpAfter, 0, "", "dump-after", kBuildTypeAll, kArgCheckPolicyNone,
    "  --dump-after                      Do extra IR dump after the specified phase" },
  { kRealCheckcast, 0, "", "realCheckCast", kBuildTypeAll, kArgCheckPolicyNone, "  --realCheckCast" },
  { kOptL0, 0, "", "O0", kBuildTypeAll, kArgCheckPolicyNone, "  --O0                              Disable most optimizations" },
  { kOptL1, 0, "", "O1", kBuildTypeAll, kArgCheckPolicyNone, "  --O1                              Enable only fast optimizations" },
  { kOptL2, 0, "", "O2", kBuildTypeAll, kArgCheckPolicyNone, "  --O2                              Enable all optimizations" },
  { kEpreLimit, 0, "", "eprelimit", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --eprelimit=NUM                   Apply EPRE optimization only for the first NUM expressions" },
  { kEprepuLimit, 0, "", "eprepulimit", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --eprepulimit=NUM                 Apply EPRE optimization only for the first NUM PUs" },
  { kStmtPrepuLimit, 0, "", "stmtprepulimit", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --stmtprepulimit=NUM              Apply STMTPRE optimization only for the first NUM PUs" },
  { kLpreLimit, 0, "", "lprelimit", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --lprelimit=NUM                   Apply LPRE optimization only for the first NUM variables" },
  { kLprepuLimit, 0, "", "lprepulimit", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --lprepulimit=NUM                 Apply LPRE optimization only for the first NUM PUs" },
  { kPregrenameLimit, 0, "", "pregrenamelimit", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --pregrenamelimit=NUM             Apply Preg Renaming optimization only up to NUM times" },
  { kDelrcpuLimit, 0, "", "delrcpulimit", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --delrcpulimit=NUM                Apply DELEGATERC optimization only for the first NUM PUs" },
  { kIgnoreIpa, 0, "", "ignoreipa", kBuildTypeAll, kArgCheckPolicyNone,
    "  --ignoreipa                       Ignore information provided by interprocedural analysis" },
  { kNoIgnoreIpa, 0, "", "noignoreipa", kBuildTypeAll, kArgCheckPolicyNone,
    "  --noignoreipa                     Do not ignore information provided by interprocedural analysis" },
  { kEpreIncludeRef, 0, "", "epreincluderef", kBuildTypeAll, kArgCheckPolicyNone,
    "  --epreincluderef                  Include ref-type expressions when performing epre optimization" },
  { kNoEpreIncludeRef, 0, "", "noepreincluderef", kBuildTypeAll, kArgCheckPolicyNone,
    "  --noepreincluderef                Exclude ref-type expressions when performing epre optimization" },
  { kEpreLocalRefVar, 0, "", "eprelocalrefvar", kBuildTypeAll, kArgCheckPolicyNone,
    "  --eprelocalrefvar                 The EPRE phase will create new localrefvars when appropriate" },
  { kNoEpreLocalRefVar, 0, "", "noeprelocalrefvar", kBuildTypeAll, kArgCheckPolicyNone,
    "  --noeprelocalrefvar               The EPRE phase will not create new localrefvars" },
  { kEprelhSivar, 0, "", "eprelhsivar", kBuildTypeAll, kArgCheckPolicyNone,
    "  --eprelhsivar                     The EPRE phase will consider iassigns when optimizing ireads" },
  { kDseKeepRef, 0, "", "dsekeepref", kBuildTypeAll, kArgCheckPolicyNone,
    "  --dsekeepref                    Preverse dassign of local var that are of ref type to anywhere" },
  { kNoDseKeepRef, 0, "", "nodsekeepref", kBuildTypeAll, kArgCheckPolicyNone, "  --nodsekeepref                    Disable dsekeepref" },
  { kPropBase, 0, "", "propbase", kBuildTypeAll, kArgCheckPolicyNone,
    "  --propbase                        Apply copy propagation that can change the base of indirect memory accesses" },
  { kPropiLoadRef, 0, "", "propiloadref", kBuildTypeAll, kArgCheckPolicyNone,
    "  --propiloadref                    Allow copy propagating iloads that are of ref type to anywhere" },
  { kNopropiLoadRef, 0, "", "nopropiloadref", kBuildTypeAll, kArgCheckPolicyNone,
    "  --nopropiloadref                  Don't allow copy propagating iloads that are of ref type to anywhere" },
  { kPropGloablRef, 0, "", "propglobalref", kBuildTypeAll, kArgCheckPolicyNone,
    "  --propglobalref                   Allow copy propagating global that are of ref type to anywhere" },
  { kPropfinalIloadRef, 0, "", "propfinaliloadref", kBuildTypeAll, kArgCheckPolicyNone,
    "  --propfinaliloadref     Allow copy propagating iloads of final fields that are of ref type to anywhere" },
  { kPropIloadRefnonparm, 0, "", "propiloadrefnonparm", kBuildTypeAll, kArgCheckPolicyNone,
    "  --propiloadref                    Allow copy propagating iloads that are of ref type to anywhere except actual parameters" },
  { kLessThrowAlias, 0, "", "lessThrowAlias", kBuildTypeAll, kArgCheckPolicyNone,
    "  --lessThrowAlias                  Handle aliases at java throw statements more accurately" },
  { kNodeLegateRc, 0, "", "nodelegaterc", kBuildTypeAll, kArgCheckPolicyNone,
    "  --nodelegateerc                   Do not apply RC delegation to local object reference pointers" },
  { kNocondBasedRc, 0, "", "nocondbasedrc", kBuildTypeAll, kArgCheckPolicyNone,
    "  --nocondbasedrc                   Do not apply condition-based RC optimization to local object reference pointers" },
  { kParmToptr, 0, "", "parmtoptr", kBuildTypeAll, kArgCheckPolicyNone,
    "  --parmtoptr                       Allow rcoptlocals to change actual parameters from ref to ptr type" },
  { kNullcheckPre, 0, "", "nullcheckpre", kBuildTypeAll, kArgCheckPolicyNone,
    "  --nullcheckpre              Turn on partial redundancy elimination of null pointer checks" },
  { kClinitPre, 0, "", "clinitpre", kBuildTypeAll, kArgCheckPolicyNone,
    "  --clinitpre                       Turn on partial redundancy elimination of class initialization checks" },
  { kDassignPre, 0, "", "dassignpre", kBuildTypeAll, kArgCheckPolicyNone,
    "  --dassignpre                      Turn on partial redundancy elimination of assignments to scalar variables" },
  { kAssign2finalPre, 0, "", "assign2finalpre", kBuildTypeAll, kArgCheckPolicyNone,
    "  --assign2finalpre                 Turn on partial redundancy elimination of assignments to final variables" },
  { kFinalFieldAlias, 0, "", "finalFieldAlias", kBuildTypeAll, kArgCheckPolicyNone,
    "  --finalFieldAlias                 Regard final fields as having alias like other fields" },
  { kRegReadAtReturn, 0, "", "regreadAtReturn", kBuildTypeAll, kArgCheckPolicyNone,
    "  --regreadAtReturn                 Allow register promotion to promote the operand of return statements" },
  { kProPatphi, 0, "", "propatphi", kBuildTypeAll, kArgCheckPolicyNone,
    "  --propatphi                       Enable copy propagation across phi nodes" },
  { kNoProPatphi, 0, "", "nopropatphi", kBuildTypeAll, kArgCheckPolicyNone,
    "  --nopropatphi                     Disable copy propagation across phi nodes" },
  { kNativeOpt, 0, "", "nativeopt", kBuildTypeAll, kArgCheckPolicyNone, "  --nativeopt                      Enable native opt" },
  { kNoNativeOpt, 0, "", "no-nativeopt", kBuildTypeAll, kArgCheckPolicyNone, "  --no-nativeopt                   Disable native opt" },
  { kOptDirectCall, 0, "", "optdirectcall", kBuildTypeAll, kArgCheckPolicyNone,
    "  --optdirectcall                   Enable redundancy elimination of directcalls" },
  { kEnableEa, 0, "", "enable-ea", kBuildTypeAll, kArgCheckPolicyNone, "  --enable-ea                       Enable escape analysis" },
  { kLpreSpeculate, 0, "", "lprespeculate", kBuildTypeAll, kArgCheckPolicyNone,
    "  --lprespeculate                   Enable speculative code motion in LPRE phase" },
  { kNoLpreSpeculate, 0, "", "nolprespeculate", kBuildTypeAll, kArgCheckPolicyNone,
    "  --nolprespeculate                 Disable speculative code motion in LPRE phase" },
  { kSpillatCatch, 0, "", "spillatcatch", kBuildTypeAll, kArgCheckPolicyNone,
    "  --spillatcatch                    Minimize upward exposed preg usage in catch blocks" },
  { kEarlyDecref, 0, "", "earlydecref", kBuildTypeAll, kArgCheckPolicyNone,
    "  --earlydecref                     Insert cleanup decrefs for localrefvars as early as possible" },
  { kPlacementRC, 0, "", "placementrc", kBuildTypeAll, kArgCheckPolicyNone,
    "  --placementrc                     Insert RC decrements for localrefvars using the placement optimization approach" },
  { kStrengthReduction, 0, "", "strengthreduction", kBuildTypeAll, kArgCheckPolicyNone,
    "  --strengthreduction               Perform strength reduction optimization" },
  { kNoStrengthReduction, 0, "", "nostrengthreduction", kBuildTypeAll, kArgCheckPolicyNone,
    "  --nostrengthreduction             Do not perform strength reduction optimization" },
  { kRCFullRedundancyElim, 0, "", "rcfre", kBuildTypeAll, kArgCheckPolicyNone,
    "  --rcfre                           Perform full redundancy elimination for reference counting" },
  { kNoRCFullRedundancyElim, 0, "", "norcfre", kBuildTypeAll, kArgCheckPolicyNone,
    "  --norcfre                         Do not perform full redundancy elimination for reference counting" },
  { kEaTransRef, 0, "", "EAOPT-ref2ptr", kBuildTypeAll, kArgCheckPolicyNumeric,
    "  --EAOPT-ref2ptr=1/0               Enable/Disable ref2ptr transformation in escape analysis "
    "(analysis is still done)" },
  { kEaTransAlloc, 0, "", "EAOPT-alloc", kBuildTypeAll, kArgCheckPolicyNumeric,
    "  --EAOPT-alloc=1/0                 Enable/Disable allocation transformation in escape analysis "
    "(analysis is still done)" },
  { kEaOptRc, 0, "", "EAOPT-rc", kBuildTypeAll, kArgCheckPolicyNone,
    "  --EAOPT-rc                        Enable/Disable RC lock optimization based on escape analysis " },
  { kDumpPhases, 0, "", "dump-phases", kBuildTypeAll, kArgCheckPolicyRequired,
    "  --dump-phases=PHASENAME,...      Enable debug trace for specified phases in the comma separated list" },
  { kInlineHint, 0, "", "inlinefunclist", kBuildTypeAll, kArgCheckPolicyRequired,
    "--inlinefunclist=                   Inlining related configuration" },
#if MIR_JAVA
  { kAcquireFunc, 0, "", "acquire-func", kBuildTypeAll, kArgCheckPolicyRequired, "  --acquire-func=FUNCNAME" },
  { kReleaseFunc, 0, "", "release-func", kBuildTypeAll, kArgCheckPolicyRequired, "  --release-func=FUNCNAME" },
  { kToolOnly, 0, "", "toolonly", kBuildTypeAll, kArgCheckPolicyNone, "  --toolonly" },
  { kToolStrict, 0, "", "toolstrict", kBuildTypeAll, kArgCheckPolicyNone, "  --toolstrict" },
  { kSkipVirtual, 0, "", "skipvirtual", kBuildTypeAll, kArgCheckPolicyNone, "  --skipvirtual" },
  { kWarnLevel, 0, "", "warning", kBuildTypeAll, kArgCheckPolicyNumeric, "  --warning=level" },
  { kCheckComplete, 0, "", "check-complete", kBuildTypeAll, kArgCheckPolicyNone, "  --check-complete                  Check incomplete types" },
#endif
  { kUnknown, 0, "", "", kBuildTypeAll, kArgCheckPolicyNone,
    "\nPHASENAME can be:\n"
    "    loopcanon         Loop canonicaliation\n"
    "    splitcriticaledge Insert empty BB on critical edges\n"
    "    dom               Compute dominators\n"
    "    pdom              Compute post-dominators\n"
    "    dominance         Compute dominance and post-dominance\n"
    "    ssa               Build SSA formation\n"
    "    fsaa              Perform flow sensitive alias analysis\n"
    "    maydefuse         Insert mayDef and mayUse nodes\n"
    "    dse               Do dead store elimination based on SSA\n"
    "    irMap             Build HSSA form\n"
    "    hprop             Do copy propagation\n"
    "    hdse              Do dead store elimination based on hashed SSA\n"
    "    ti                Perform type inferencing\n"
    "    ssapre            Perform partial redundancy elimination\n" },
  { 0, 0, nullptr, nullptr, kBuildTypeAll, kArgCheckPolicyUnknown, nullptr }
};

bool MeOption::ParseOptions(int argc, char **argv, string &fileName) {
  bool result = true;
  OptionParser optionParser(kUsage);
  int ret = optionParser.Parse(argc, argv);
  CHECK_FATAL(ret == ErrorCode::kErrorNoError, "option parser error");
  for (auto opt : optionParser.GetOptions()) {
    switch (opt.Index()) {
      case kHelp:
        if (opt.Args().empty()) {
          optionParser.PrintUsage();
          result = false;
        }
        break;
      case kSkipPhases:
        SplitPhases(opt.Args().c_str(), skip_phases);
        break;
      case kRange:
        MeOption::useRange = true;
        GetRange(opt.Args());
        break;
      case kDumpBefore:
        MeOption::dumpBefore = true;
        break;
      case kDumpAfter:
        MeOption::dumpAfter = true;
        break;
      case kDumpFunc:
        MeOption::dumpFunc = opt.Args();
        break;
      case kSkipFrom:
        MeOption::skipFrom = opt.Args();
        break;
      case kSkipAfter:
        MeOption::skipAfter = opt.Args();
        break;
      case kDumpPhases:
        SplitPhases(opt.Args().c_str(), MeOption::dumpPhases);
        break;
      case kQuiet:
        MeOption::quiet = true;
        break;
      case kSetCalleeHasSideEffect:
        MeOption::setCalleeHasSideEffect = true;
        break;
      case kNoSteensgaard:
        MeOption::noSteensgaard = true;
        break;
      case kNoTBAA:
        MeOption::noTBAA = true;
        break;
      case kAliasAnalysisLevel:
        MeOption::aliasAnalysisLevel = std::stoul(opt.Args(), nullptr);
        if (MeOption::aliasAnalysisLevel > 3) {
          MeOption::aliasAnalysisLevel = 3;
        }
        switch (MeOption::aliasAnalysisLevel) {
          case 3:
            MeOption::setCalleeHasSideEffect = false;
            MeOption::noSteensgaard = false;
            MeOption::noTBAA = false;
            break;
          case 0:
            MeOption::setCalleeHasSideEffect = true;
            MeOption::noSteensgaard = true;
            MeOption::noTBAA = true;
            break;
          case 1:
            MeOption::setCalleeHasSideEffect = false;
            MeOption::noSteensgaard = false;
            MeOption::noTBAA = true;
            break;
          case 2:
            MeOption::setCalleeHasSideEffect = false;
            MeOption::noSteensgaard = true;
            MeOption::noTBAA = false;
            break;
          default:
            break;
        }
      case kRcLower:
        MeOption::rcLowering = true;
        break;
      case kNoRcLower:
        MeOption::rcLowering = false;
        break;
      case kNoRc:
        MeOption::noRC = true;
        break;
      case kUseRc:
        MeOption::noRC = false;
        break;
      case kStrictNaiveRc:
        MeOption::strictNaiverc = true;
        break;
      case kNoGcbar:
        MeOption::noGCBar = true;
        break;
      case kUseGcbar:
        MeOption::noGCBar = false;
        break;
      case kRealCheckcast:
        MeOption::realCheckCast = true;
        break;
      case kNoDot:
        MeOption::noDot = true;
        break;
      case kStmtNum:
        MeOption::stmtNum = true;
        break;
      case kStubJniFunc:
        MeOption::regNativeFunc = true;
        break;
      case kWarnNativefunc:
        MeOption::warnNativeFunc = true;
        break;
      case kOptL0:
        MeOption::optLevel = 0;
        break;
      case kOptL1:
        MeOption::optLevel = 1;
        break;
      case kOptL2:
        MeOption::optLevel = 2;
        // Turn the followings ON only at O2
        MeOption::optdirectcall = true;
        break;
      case kEpreLimit:
        MeOption::epreLimit = std::stoul(opt.Args(), nullptr);
        break;
      case kEprepuLimit:
        MeOption::eprePULimit = std::stoul(opt.Args(), nullptr);
        break;
      case kStmtPrepuLimit:
        MeOption::stmtprePULimit = std::stoul(opt.Args(), nullptr);
        break;
      case kLpreLimit:
        MeOption::lpreLimit = std::stoul(opt.Args(), nullptr);
        break;
      case kLprepuLimit:
        MeOption::lprePULimit = std::stoul(opt.Args(), nullptr);
        break;
      case kPregrenameLimit:
        MeOption::pregrenameLimit = std::stoul(opt.Args(), nullptr);
        break;
      case kDelrcpuLimit:
        MeOption::delrcPULimit = std::stoul(opt.Args(), nullptr);
        break;
      case kIgnoreIpa:
        MeOption::ignoreIPA = true;
        break;
      case kNoIgnoreIpa:
        MeOption::ignoreIPA = false;
        break;
      case kEpreIncludeRef:
        MeOption::epreIncludeRef = true;
        break;
      case kNoEpreIncludeRef:
        MeOption::epreIncludeRef = false;
        break;
      case kEpreLocalRefVar:
        MeOption::eprelocalrefvar = true;
        break;
      case kNoEpreLocalRefVar:
        MeOption::eprelocalrefvar = false;
        break;
      case kEprelhSivar:
        MeOption::eprelhsivar = true;
        break;
      case kDseKeepRef:
        MeOption::dsekeepref = true;
        break;
      case kNoDseKeepRef:
        MeOption::dsekeepref = false;
        break;
      case kPropBase:
        MeOption::propbase = true;
        break;
      case kPropiLoadRef:
        MeOption::propiloadref = true;
        MeOption::propiloadrefnonparm = false;  // to override previous -propiloadrefnonparm
        break;
      case kNopropiLoadRef:
        MeOption::propiloadref = false;
        break;
      case kPropGloablRef:
        MeOption::propglobalref = true;
        break;
      case kPropfinalIloadRef:
        MeOption::propfinaliloadref = true;
        break;
      case kPropIloadRefnonparm:
        MeOption::propiloadref = true;
        MeOption::propiloadrefnonparm = true;
        break;
      case kLessThrowAlias:
        MeOption::lessThrowAlias = true;
        break;
      case kNodeLegateRc:
        MeOption::nodelegaterc = true;
        break;
      case kNocondBasedRc:
        MeOption::nocondbasedrc = true;
        break;
      case kParmToptr:
        MeOption::parmtoptr = true;
        break;
      case kEaOptRc:
        MeOption::eaoptrc = true;
        break;
      case kNullcheckPre:
        MeOption::nullcheckpre = true;
        break;
      case kClinitPre:
        MeOption::clinitpre = true;
        break;
      case kDassignPre:
        MeOption::dassignpre = true;
        break;
      case kAssign2finalPre:
        MeOption::assign2finalpre = true;
        break;
      case kFinalFieldAlias:
        MeOption::finalFieldAlias = true;
        break;
      case kRegReadAtReturn:
        MeOption::regreadAtReturn = true;
        break;
      case kProPatphi:
        MeOption::propatphi = true;
        break;
      case kNoProPatphi:
        MeOption::propatphi = false;
        break;
      case kNativeOpt:
        MeOption::nativeopt = true;
        break;
      case kNoNativeOpt:
        MeOption::nativeopt = false;
        break;
      case kOptDirectCall:
        MeOption::optdirectcall = true;
        break;
      case kEnableEa:
        MeOption::enableEA = true;
        break;
      case kLpreSpeculate:
        MeOption::lprespeculate = true;
        break;
      case kNoLpreSpeculate:
        MeOption::lprespeculate = false;
        break;
      case kSpillatCatch:
        MeOption::spillatcatch = true;
        break;
      case kEarlyDecref:
        MeOption::earlydecref = true;
        break;
      case kPlacementRC:
        MeOption::placementrc = true;
        break;
      case kStrengthReduction:
        MeOption::strengthreduction = true;
        break;
      case kNoStrengthReduction:
        MeOption::strengthreduction = false;
        break;
      case kRCFullRedundancyElim:
        MeOption::rcfre = true;
        break;
      case kNoRCFullRedundancyElim:
        MeOption::rcfre = false;
        break;
      case kInlineHint:
        MeOption::inlinefunclist = opt.Args();
        break;
#if MIR_JAVA
      case kAcquireFunc:
        MeOption::acquireFuncName = opt.Args();
        break;
      case kReleaseFunc:
        MeOption::releaseFuncName = opt.Args();
        break;
      case kWarnLevel:
        MeOption::warningLevel = std::stoi(opt.Args());
        break;
      case kToolOnly:
        MeOption::mpltoolonly = true;
        break;
      case kToolStrict:
        MeOption::mpltoolstrict = true;
        break;
      case kSkipVirtual:
        MeOption::skipvirtualMethod = true;
        break;
      case kCheckComplete:
        MeOption::parserOpt |= kCheckCompleteType;
        break;
#endif
      default:
        result = false;
        ASSERT(false, "unhandled case in MeOption");
    }
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
    }
  }
  return result;
}

void MeOption::SplitPhases(const char *str, std::unordered_set<std::string> &set) {
  std::string s = str;
  size_t current = 0;
  size_t next = 0;
  do {
    next = s.find_first_of(",", current);
    set.insert(s.substr(current, next - current));
    current = next + 1;
  } while (next != string::npos);
}

void MeOption::GetRange(const std::string &str) const {
  std::string s{ str };
  size_t comma = s.find_first_of(",", 0);
  if (comma != std::string::npos) {
    range[0] = std::stoul(s.substr(0, comma), nullptr);
    range[1] = std::stoul(s.substr(comma + 1, std::string::npos - (comma + 1)), nullptr);
  }
  if (range[0] > range[1]) {
    LogInfo::MapleLogger(kLlErr) << "invalid values for --range=" << range[0] << "," << range[1] << '\n';
    ASSERT(false, "GetRange exit");
  }
}

bool MeOption::DumpPhase(std::string phase) {
  return dumpPhases.find(phase) != dumpPhases.end();
}
