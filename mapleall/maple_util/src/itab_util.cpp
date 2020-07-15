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

#include "itab_util.h"
#include <map>
#include <mutex>
#include <cstring>

namespace maple {

unsigned int DJBHash(const char *str) {
  unsigned int hash = 5381;
  while (*str) {
    hash += (hash << 5) + (unsigned char)(*str++);
  }
  return (hash & 0x7FFFFFFF);
}

unsigned int GetHashIndex(const char *name) {
  unsigned int hashcode = DJBHash(name);
  return (hashcode % kHashSize);
}

struct cmp_str {
  bool operator()(char const *a, char const *b) const  {
    return std::strcmp(a, b) < 0;
  }
};

// check risk incurred by multi-threads.
std::map<const char*, unsigned int, cmp_str>::iterator it;
std::mutex mapLock;

unsigned int GetSecondHashIndex(const char *name) {
  std::lock_guard<std::mutex> guard(mapLock);
  unsigned int hashcode = DJBHash(name);
  return (hashcode % kItabSecondHashSize);
}

}  // namespace maple
