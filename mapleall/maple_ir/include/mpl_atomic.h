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

#ifndef MAPLE_IR_INCLUDE_MPLATOMIC_H
#define MAPLE_IR_INCLUDE_MPLATOMIC_H

#include <cstdint>

#include "mpl_logging.h"

namespace maple {

enum class MemOrd : uint32_t {
  kNotAtomic = 0,
#define ATTR(STR) STR,
#include "memory_order_attrs.def"
#undef ATTR
};

inline MemOrd MemOrdFromU32(uint32_t val) {
  CHECK_FATAL(val <= 6 && val != 2, "Illegal number for MemOrd: %u", val);
  static MemOrd tab[] = {
    MemOrd::kNotAtomic,
    MemOrd::memory_order_relaxed,
    // memory_order_consume Disabled.  Its semantics is debatable.
    // We don't support it now, but reserve the number.  Use memory_order_acquire instead.
    MemOrd::memory_order_acquire,  // padding entry
    MemOrd::memory_order_acquire,
    MemOrd::memory_order_release,
    MemOrd::memory_order_acq_rel,
    MemOrd::memory_order_seq_cst,
  };
  return tab[val];
}

inline bool MemOrdIsAcquire(MemOrd ord) {
  static bool tab[] = {
    false,  // kNotAtomic
    false,  // memory_order_relaxed
    true,   // memory_order_consume
    true,   // memory_order_acquire
    false,  // memory_order_release
    true,   // memory_order_acq_rel
    true,   // memory_order_seq_cst
  };
  return tab[static_cast<uint32_t>(ord)];
}

inline bool MemOrdIsRelease(MemOrd ord) {
  static bool tab[] = {
    false,  // kNotAtomic
    false,  // memory_order_relaxed
    false,  // memory_order_consume
    false,  // memory_order_acquire
    true,   // memory_order_release
    true,   // memory_order_acq_rel
    true,   // memory_order_seq_cst
  };
  return tab[static_cast<uint32_t>(ord)];
}

}  // namespace maple

#endif  // MAPLE_IR_INCLUDE_MPLATOMIC_H
