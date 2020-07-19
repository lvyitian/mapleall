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
#include "mir_nodes.h"
#include "special_func.h"

namespace maplebe {

std::vector<std::pair<string, uint64_t>> wellKnownFrameWorksClass{
};

bool IsEhTerminatingFunction(const string &name) {
  if (!name.compare("_Unwind_Resume") ||
      !name.compare("__cxa_throw")) {
    return true;
  }
  return false;
}

bool IsThrowOrNonReturnFunc(const string &name) {
  if (name == GetIntrinsicFuncName(INTRN_MCCThrowException) ||
      name == GetIntrinsicFuncName(INTRN_MCCRethrowException) ||
      name == GetIntrinsicFuncName(INTRN_JAVA_THROW_ARITHMETIC) ||
      name == GetIntrinsicFuncName(INTRN_MCCThrowArrayIndexOutOfBoundException) ||
      name == GetIntrinsicFuncName(INTRN_MCCThrowNullPointerException) ||
      name == GetIntrinsicFuncName(INTRN_MCCThrowStringIndexOutOfBoundsException) ||
      name == "abort" ||
      name == "exit") {
    return true;
  }
  return false;
}

bool CanFuncThrow(const string &name) {
  /*
    There are a list of runtime function that can throw,
    but it is a lot of work to go through each, so the reverse is done,
    assuming it will throw unless marked otherwise.
      MCC_ThrowException
      MCC_ThrowNullPointerException
      MCC_RethrowException
      MCC_ThrowNullArrayNullPointerException
      MCC_Array_Boundary_Check
      MCC_JavaArrayLength
      MCC_JavaPolymorphicCall
   */
  if (name.compare(0, 4, "MCC_")) {
    return true;
  }
  if (name == GetIntrinsicFuncName(INTRN_MCCInitializeLocalStackRef) ||
      name == GetIntrinsicFuncName(INTRN_MCCIncRef) ||
      name == GetIntrinsicFuncName(INTRN_MCCDecRef) ||
      name == GetIntrinsicFuncName(INTRN_MCCIncDecRef) ||
      name == GetIntrinsicFuncName(INTRN_MCCCleanupLocalStackRefNaiveRCFast) ||
      name == GetIntrinsicFuncName(INTRN_MCCLoadRef) ||
      name == GetIntrinsicFuncName(INTRN_MCCGetOrInsertLiteral) ||
      name == GetIntrinsicFuncName(INTRN_MCCNewObjFixedClass) ||
      name == GetIntrinsicFuncName(INTRN_JAVA_BEGIN_CATCH)) {
    return false;
  }
  return true;
}

bool IsFuncSyncEnterOrExit(const string &name) {
  if (name == GetIntrinsicFuncName(INTRN_MCCSyncEnterFast0) ||
      name == GetIntrinsicFuncName(INTRN_MCCSyncEnterFast1) ||
      name == GetIntrinsicFuncName(INTRN_MCCSyncEnterFast2) ||
      name == GetIntrinsicFuncName(INTRN_MCCSyncEnterFast3) ||
      name == GetIntrinsicFuncName(INTRN_MCCSyncExitFast)) {
    return true;
  }
  return false;
}

bool IsRtCleanupFunc(const string &name) {
  if (name == GetIntrinsicFuncName(INTRN_MCCDecRef) ||
      name == GetIntrinsicFuncName(INTRN_MCCIncRef) ||
      name == GetIntrinsicFuncName(INTRN_MCCIncDecRef) ||
      name == GetIntrinsicFuncName(INTRN_MCCLoadRefS) ||
      name == GetIntrinsicFuncName(INTRN_MCCLoadRefField) ||
      name == GetIntrinsicFuncName(INTRN_MCCLoadReferentField) ||
      name == GetIntrinsicFuncName(INTRN_MCCLoadRef) ||
      name == GetIntrinsicFuncName(INTRN_MCCLoadRefVol) ||
      name == GetIntrinsicFuncName(INTRN_MCCLoadRefSVol) ||
      name == GetIntrinsicFuncName(INTRN_MCCLoadWeak) ||
      name == GetIntrinsicFuncName(INTRN_MCCCheckObjMem)) {
    return true;
  }
  return false;
}

bool IsRtLockCall(const string &name) {
  if (name == "MCC_SyncExitFast" ||
      name == "MCC_SyncExitFast0" ||
      name == "MCC_SyncExitFast1" ||
      name == "MCC_SyncExitFast2" ||
      name == "MCC_SyncExitFast3") {
    return true;
  }
  return false;
}

bool IsRtNativeCall(const string &name) {
  if (name == "MCC_CallFastNative" ||
      name == "MCC_CallFastNativeExt" ||
      name == "MCC_CallSlowNative0" ||
      name == "MCC_CallSlowNative1" ||
      name == "MCC_CallSlowNative2" ||
      name == "MCC_CallSlowNative3" ||
      name == "MCC_CallSlowNative4" ||
      name == "MCC_CallSlowNative5" ||
      name == "MCC_CallSlowNative6" ||
      name == "MCC_CallSlowNative7" ||
      name == "MCC_CallSlowNative8" ||
      name == "MCC_CallSlowNativeExt") {
    return true;
  }
  return false;
}

bool IsRtCleanupLocalStackCall(const string &name) {
  if (name == GetIntrinsicFuncName(INTRN_MCCCleanupLocalStackRefNaiveRCFast) ||
      name == GetIntrinsicFuncName(INTRN_MCCCleanupLocalStackRefSkipNaiveRCFast) ||
      name == GetIntrinsicFuncName(INTRN_MCCCleanupLocalStackRefSkip)) {
    return true;
  }
  return false;
}

bool IsFuncNeedFrame(const string &funcName) {
  return false;
}

}  // namespace maplebe
