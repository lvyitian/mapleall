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

#ifndef MPLCG_INCLUDE_SPECIALFUNC_H
#define MPLCG_INCLUDE_SPECIALFUNC_H

#include <string>
#include <unordered_map>
#include <vector>
#include "intrinsic_op.h"

namespace maplebe {

using namespace std;
using namespace maple;

extern unordered_map<std::string, MIRIntrinsicID> IntrinsicList;
extern vector<pair<string, uint64_t>> wellKnownFrameWorksClass;

bool IsEhTerminatingFunction(const string &name);
bool IsThrowOrNonReturnFunc(const string &name);
bool CanFuncThrow(const string &name);
bool IsFuncSyncEnterOrExit(const string &name);
bool IsRtCleanupFunc(const string &name);
bool IsRtLockCall(const string &name);
bool IsRtNativeCall(const string &name);
bool IsRtCleanupLocalStackCall(const string &name);
bool IsFuncNeedFrame(const string &funcName);

}  // namespace maplebe

#endif  // MPLCG_INCLUDE_SPECIALFUNC_H
