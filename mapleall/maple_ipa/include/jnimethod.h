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

#ifndef MAPLE_IPA_INCLUDE_JNIMETHOD_H
#define MAPLE_IPA_INCLUDE_JNIMETHOD_H

#include <string>
#include <vector>
#include "../../maple_ir/include/types_def.h"

using namespace maple;

struct SideEffect {
  uint32 has_use : 1;          // Has a use of a field of any pre-existing object
  uint32 has_def : 1;          // Has a definition of a field of any pre-existing object
  uint32 ret_fresh_obj : 1;    // Returns freshly allocated object
  uint32 throw_ex : 1;         // Throws an exception
  uint32 has_private_use : 1;  // Has a use of a private field
  uint32 has_private_def : 1;  // Has a definition of a private field

  uint32 GetHasUse() const {
    return has_use;
  }

  uint32 GetHasDef() const {
    return has_def;
  }

  uint32 GetRetFreshObj() const {
    return ret_fresh_obj;
  }

  uint32 GetThrowEx() const {
    return throw_ex;
  }

  uint32 GetHasPrivateUse() const {
    return has_private_use;
  }

  uint32 GetHasPrivateDef() const {
    return has_private_def;
  }

  void SetHasUse(uint32 hasUse) {
    has_use = hasUse;
  }

  void SetHasDef(uint32 hasDef) {
    has_def = hasDef;
  }

  void SetRetFreshObj(uint32 retFreshObj) {
    ret_fresh_obj = retFreshObj;
  }

  void SetThrowEx(uint32 throwEx) {
    throw_ex = throwEx;
  }

  void SetHasPrivateUse(uint32 hasPrivateUse) {
    has_private_use = hasPrivateUse;
  }

  void SetHasPrivateDef(uint32 hasPrivateDef) {
    has_private_def = hasPrivateDef;
  }
};

struct JavaInvokeInfo {
  std::string classname;
  std::string funcname;
  std::string signature;
};

struct JNIFuncInfo {
  std::string mplname;                         // maple name of the jni function
  bool is_pure;                                // true if the jni function is pure
  SideEffect sideeffect;                       // side effect information
  std::vector<JavaInvokeInfo> javainvokeinfo;  // java method invoke information of the jni function
};

#endif
