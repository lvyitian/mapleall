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

#ifndef MAPLEBE_INCLUDE_COMMON_UTILS_H
#define MAPLEBE_INCLUDE_COMMON_UTILS_H

#include <cstdint>
#include <cassert>

namespace maplebe {

// The constexpr implementations, without assertions.  Suitable for using in
// constants.

constexpr bool IsPowerOf2Const(uint64_t i) {
  return (i & (i - 1)) == 0;
}

constexpr uint64_t RoundUpConst(uint64_t offset, uint64_t align) {
  return (-align) & (offset + align - 1);
}

constexpr int64_t RoundDownConst(int64_t offset, int64_t align) {
  return (-align) & offset;
}

inline bool IsPowerOf2(uint64_t i) {
  return IsPowerOf2Const(i);
}

// align must be a power of 2
inline uint64_t RoundUp(uint64_t offset, uint64_t align) {
  if (align == 0) {
    return offset;
  }
  assert(IsPowerOf2(align));
  return RoundUpConst(offset, align);
}

inline bool IsAlignedTo(uint64_t offset, uint64_t align) {
  assert(IsPowerOf2(align));
  return (offset & (align - 1)) == 0;
}

// align must be a power of 2
inline int64_t RoundDown(int64_t offset, int64_t align) {
  if (align == 0) {
    return offset;
  }
  assert(IsPowerOf2(align));
  return RoundDownConst(offset, align);
}

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_COMMON_UTILS_H
