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

#ifndef HAVE_OPENSSL
#include "muid.h"

MUID_t GetMUID(const std::string &symbolName, bool forSystem) {
  MUID_t ret;
  MD5_CTX c;

  MD5_Init(&c);
  MD5_Update(&c, symbolName.c_str(), symbolName.length());
#ifdef USE_64BIT_MUID
  // MuidFinal((unsigned char *)&ret.data, &c);
  assert(false);
#else
  MD5_Final((unsigned char *)&ret.data, &c);
#endif // USE_64BIT_MUID
  if (forSystem) {
    ret.SetSystemNameSpace();
  } else {
    ret.SetApkNameSpace();
  }
  return ret;
}

#endif

