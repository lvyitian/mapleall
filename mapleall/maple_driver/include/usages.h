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
#ifndef MAPLE_DRIVER_INCLUDE_USAGES_H
#define MAPLE_DRIVER_INCLUDE_USAGES_H

namespace maple {
enum OptionIndex : uint64 {
  kUnknown,
  kHelp,
  kVersion,
  kInFile,
  kInMplt,
  kOptimization0,
  kMeOpt,
  kMpl2MplOpt,
  kMplCgOpt,
  kSaveTemps,
  kRun,
  kExe,
  kOption,
  kAllDebug,
  // ----------jbc2mpl begin-------
  kUseStringFactory,
  kJbc2mplOutMpl,
  kTimePhases,
  kGenMeMpl,
  kGenVtableImpl,
  kVerify,
  // ----------me begin-----------
  kMeHelp,
  kMeDumpPhases,
  kMeSkipPhases,
  kMeDumpFunc,
  kMeQuiet,
  kMeNoDot,
  kSetCalleeHasSideEffect,
  kNoSteensgaard,
  kNoTBAA,
  kAliasAnalysisLevel,
  kStmtNum,
  kMeDumpAfter,
  kMeRange,
  kLessThrowAlias,
  kRegReadAtReturn,
  // ----------mpl2mpl begin---------
  kMpl2MplHelp,
  kMpl2MplDumpPhase,
  kMpl2MplSkipPhase,
  kMpl2MplSkipFrom,
  kMpl2MplSkipAfter,
  kMpl2MplDumpFunc,
  kMpl2MplQuiet,
  kMpl2MplStubJniFunc,
  kMpl2MplSkipVirtual,
  kRegNativeDynamicOnly,
  kRegNativeStaticBindingList,
  kNativeWrapper,
  kMpl2MplDumpBefore,
  kMpl2MplDumpAfter,
  kMpl2MplMapleLinker,
  kMplnkDumpMuid,
  kEmitVtableImpl,
  kInlineWithProfile,
  kInlineWithoutProfile,
  kMpl2MplUseInline,
  kMpl2MplNoInlineFuncList,
  kMpl2MplUseCrossModuleInline,
  kInlineSmallFunctionThreshold,
  kInlineHotFunctionThreshold,
  kInlineModuleGrowth,
  kInlineColdFunctionThreshold,
  kProfileHotCount,
  kProfileColdCount,
  kProfileHotRate,
  kProfileColdRate,
  // ----------mplcg begin---------
  kCGQuiet,
  kPie,
  kPic,
  kVerbose,
  kCGMapleLinker,
  kDuplicateToDelPlt,
// ----------mplcg end-----------
};

enum EnabledIndex {
  kDisable,
  kEnable
};
}  // namespace maple
#endif  // MAPLE_DRIVER_INCLUDE_USAGES_H
