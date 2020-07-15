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

#include "aarch64_immediate.h"
#include "cg_assert.h"

#if DEBUG
#include <stdio.h>
#include <set>

namespace maplebe {
static std::set<maple::uint64> ValidBitmaskImmSet{
#include "valid_bitmask_imm.txt"
};
}  // namespace maplebe
#endif

namespace maplebe {
using namespace maple;
static uint64 BitmaskImmMultTable[] = {
  0x0000000100000001ULL, 0x0001000100010001ULL, 0x0101010101010101ULL, 0x1111111111111111ULL, 0x5555555555555555ULL,
};

bool IsBitmaskImmediate(uint64 val, uint32 bitlen) {
  CG_ASSERT((val != 0 && static_cast<int64>(val) != -1), "IsBitmaskImmediate() don's accept 0 or -1");
#if DEBUG
  uint64 val2 = val;
  if (bitlen == 32) {
    val2 = (val2 << 32) | (val2 & ((1ULL << 32) - 1));
  }
  bool expectedOutcome = ValidBitmaskImmSet.find(val2) != ValidBitmaskImmSet.end();
#endif

  if ((val & 0x1) != 0) {
    // we want to work with
    // 0000000000000000000000000000000000000000000001100000000000000000
    // instead of
    // 1111111111111111111111111111111111111111111110011111111111111111
    val = ~val;
  }

  if (bitlen == 32) {
    val = (val << 32) | (val & ((1ULL << 32) - 1));
  }

  // get the least significant bit set and add it to 'val'
  uint64 tval = val + (val & (-val));

  // now check if tmp is a power of 2 or tval==0.
  tval = tval & (tval - 1);
  if (tval == 0) {
#if DEBUG
    if (!expectedOutcome) {
      printf("0x%016llx\n", (unsigned long long)val);
    }
    CG_ASSERT(expectedOutcome == true, "incorrect implementation: not valid value but returning true");
#endif
    // power of two or zero ; return true
    return true;
  }

  int p0 = __builtin_ctzll(val);
  int p1 = __builtin_ctzll(tval);
  int64 diff = p1 - p0;

  // check if diff is a power of two; return false if not.
  if ((diff & (diff - 1)) != 0) {
#if DEBUG
    CG_ASSERT(expectedOutcome == false, "incorrect implementation: valid value but returning false");
#endif
    return false;
  }

  int logDiff = __builtin_ctzll(diff);
  int64 pattern = val & ((1ULL << diff) - 1);
#if DEBUG
  bool ret = val == pattern * BitmaskImmMultTable[5 - logDiff];
  CG_ASSERT(expectedOutcome == ret, "incorrect implementation: return value does not match expected outcome");
  return ret;
#else
  return val == pattern * BitmaskImmMultTable[5 - logDiff];
#endif
}

bool IsMoveWidableImmediate(uint64 val, uint32 bitlen) {
  if (bitlen == 64) {
    // 0xHHHH000000000000 or 0x0000HHHH00000000, return true
    if ((val & ((static_cast<uint64>(0xffff)) << 48)) == val ||
        (val & ((static_cast<uint64>(0xffff)) << 32)) == val) {
      return true;
    }
  } else {
    val &= static_cast<uint64>(0xffffffff);
  }
  return ((val & ((static_cast<uint64>(0xffff)) << 16)) == val ||
          (val & ((static_cast<uint64>(0xffff)) << 0)) == val);
}

bool BetterUseMOVZ(uint64 val) {
  int n16zerosChunks = 0;
  int n16onesChunks = 0;

  for (uint64 i = 0, sa = 0; i < 4; ++i, sa += 16) {
    uint64 chunkVal = (val >> (static_cast<uint64>(sa))) & 0x0000FFFFULL;
    if (chunkVal == 0) {
      ++n16zerosChunks;
    } else if (chunkVal == 0xFFFFULL) {
      ++n16onesChunks;
    }
  }
  // note that since we already check if the value
  // can be movable with as a single mov instruction,
  // we should not exepct either n_16zeros_chunks>=3 or n_16ones_chunks>=3
  CG_ASSERT(n16zerosChunks <= 2 && n16onesChunks <= 2, "");
  return (n16zerosChunks >= n16onesChunks);
}

}  // namespace maplebe
