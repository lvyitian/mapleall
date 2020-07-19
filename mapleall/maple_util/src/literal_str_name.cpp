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

#include "literal_str_name.h"

// literal string name is shared between maple compiler and runtime, thus not in namespace maplert
// note there is a macor kConstString "_C_STR_" in literal_str_name.h
// which need to match
static std::string mpl_const_str("_C_STR_00000000000000000000000000000000");
static const char *mpl_digits = "0123456789abcdef";

std::string LiteralStrName::GetHexStr(const uint8_t *bytes, uint32_t len) {
  std::string str(mpl_const_str, 0, (len<<1)+kConstStringLen);
  for (unsigned i=0; i<len; i++) {
    str[2*i+kConstStringLen] = mpl_digits[(bytes[i]&0xf0)>>4];
    str[2*i+kConstStringLen+1] = mpl_digits[ bytes[i]&0x0f];
  }
  return str;
}

int32_t LiteralStrName::CalculateHashSwapByte(const char16_t *data, uint32_t len) {
  int hash = 0;
  const char16_t *end = data + len;
  while(data < end){
    hash = (hash << 5) - hash;
    char16_t val = *data++;
    hash += (((val<<8)&0xff00)|((val>>8)&0xff));
  }
  return hash;
}


std::string LiteralStrName::GetLiteralStrName(const uint8_t *bytes, uint32_t len) {
  if (len <= 15)
    return GetHexStr(bytes, len);

  return ComputeMuid(bytes, len);
}

std::string LiteralStrName::ComputeMuid(const uint8_t *bytes, uint32_t len) {
  MD5_CTX digestContext;
  DigestHash digestHash;
  MD5_Init(&digestContext);
  digestHash.d.first = digestHash.d.second = 0;
  MD5_Update(&digestContext, bytes, len);
  MD5_Final(digestHash.bytes, &digestContext);
  return GetHexStr(digestHash.bytes, MUID_LENGTH);
}
