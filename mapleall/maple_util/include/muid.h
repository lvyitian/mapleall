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

#ifndef MUID_H
#define MUID_H

// This is shared between maple compiler and runtime.

#include <stdint.h>
#include <string>
#include <sstream>
#include <iomanip>

#include <openssl/md5.h>

#ifdef USE_32BIT_REF
#define USE_64BIT_MUID
#endif // USE_32BIT_REF

#define SYSTEM_NAMESPACE 0xc0
#define APK_NAMESPACE    0x80
#define BITMASK          0x3f

#ifdef USE_64BIT_MUID
#undef MUID_LENGTH
#define MUID_LENGTH 8
#else
#undef MUID_LENGTH
#define MUID_LENGTH 16
#endif // USE_64BIT_MUID

// muid-related files are shared between maple compiler and runtime, thus not in
// namespace maplert
struct MUID_t{
  union {
#ifdef USE_64BIT_MUID
    uint32_t words[2];
    uint8_t bytes[MUID_LENGTH];
#else
    //unsigned __int128 dword;
    uint64_t words[2];
    uint8_t bytes[MUID_LENGTH];
#endif // USE_64BIT_MUID
  } data;

  MUID_t() {
    data.words[0] = 0;
    data.words[1] = 0;
  }
  inline bool IsSystemNameSpace() {
    return (data.bytes[MUID_LENGTH-1] & ~BITMASK) == SYSTEM_NAMESPACE;
  }
  inline bool IsApkNameSpace() {
    return (data.bytes[MUID_LENGTH-1] & ~BITMASK) == APK_NAMESPACE;
  }
  inline void SetSystemNameSpace() {
    data.bytes[MUID_LENGTH-1] &= BITMASK;
    data.bytes[MUID_LENGTH-1] |= SYSTEM_NAMESPACE;
  }
  inline void SetApkNameSpace() {
    data.bytes[MUID_LENGTH-1] &= BITMASK;
    data.bytes[MUID_LENGTH-1] |= APK_NAMESPACE;
  }
  bool operator < (const MUID_t &muid) const {
    return (data.words[1] < muid.data.words[1] ||
            (data.words[1] == muid.data.words[1] && data.words[0] < muid.data.words[0]));
  }
  bool operator > (const MUID_t &muid) const {
    return (data.words[1] > muid.data.words[1] ||
            (data.words[1] == muid.data.words[1] && data.words[0] > muid.data.words[0]));
  }
  bool operator == (const MUID_t &muid) const {
    return data.words[1] == muid.data.words[1] && data.words[0] == muid.data.words[0];
  }
  bool operator != (const MUID_t &muid) const {
    return data.words[1] != muid.data.words[1] || data.words[0] != muid.data.words[0];
  }
  std::string ToStr() const {
    std::stringstream sbuf;
#ifdef USE_64BIT_MUID
    sbuf << std::setfill('0') << std::setw(8) << std::hex << data.words[1]
         << std::setfill('0') << std::setw(8) << std::hex << data.words[0];
#else
    sbuf << std::setfill('0') << std::setw(16) << std::hex << data.words[1]
         << std::setfill('0') << std::setw(16) << std::hex << data.words[0];
#endif // USE_64BIT_MUID
    return sbuf.str();
  }
};

// void MuidFinal(unsigned char *result, MD5_CTX *ctx);
MUID_t GetMUID(const std::string &symbolName, bool forSystem = true);

#endif
