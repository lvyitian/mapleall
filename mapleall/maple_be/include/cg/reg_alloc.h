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

#ifndef MAPLEBE_INCLUDE_CG_REGALLOC_H_
#define MAPLEBE_INCLUDE_CG_REGALLOC_H_

#include "isa.h"
#include "cg_phase.h"
#include "cg_assert.h"

#include <map>
#include <stack>
#include <limits.h>  // CHAR_BIT

#include <iostream>

namespace maplebe {
// forward declaration
class CGFunc;

class VirtualRegNode {
  friend class CGFunc;
  RegType regtype;
  uint8_t size;   // size in bytes
  regno_t regno;  // physical register assigned by register allocation

 public:
  VirtualRegNode() : regtype(kRegTyUndef), size(0), regno(INVALID_REGNO) {}

  VirtualRegNode(RegType t, uint8_t s) : regtype(t), size(s), regno(INVALID_REGNO) {}

  virtual ~VirtualRegNode() {}

  void AssignPhysicalRegister(regno_t prn) {
    regno = prn;
  }

  RegType GetType() const {
    return regtype;
  }

  uint32_t GetSize() const {
    return (uint32_t)size;
  }
};

class RegAllocator {
 protected:
  CGFunc *cgfunc_;

 public:
  RegAllocator(CGFunc *cgfunc) : cgfunc_(cgfunc) {}

  virtual ~RegAllocator() {}

  virtual bool AllocateRegisters() = 0;
};

CGFUNCPHASE(CgDoRegAlloc, "regalloc")

class Insn;

/*
   We callect information that helps us generate instructions
   to spill and reload caller-saved registers.

   store_insertion_point: an instruction before which we insert the store
    instructions of caller-saved registers.
   load_insertion_point:  an instruction after which we insert the load
    instructions of caller-saved registers.

   When we generate debug-friendly code, we allocate space before the call
   and we release/free the space after the call returns.

   The two fields point to those instructions, respectively --
    store_insertion_point -> stack allocation instruction
    load_insertion_point  -> stack free instruction

   When we don't, and the call have stack-passed arguments, the space is
   allocated as part of the call frame at the function entry.

   If the call does not have stack-passed operands, no instructions
   are needed to allocate/free the stack space.

   In either case, we will have
    store_insertion_point -> first instruction to move a call argument
                             to an argument-passing register
    load_insertion_point  -> the call instruction

   If the call does not pass any argument
    store_insertion_point -> the call instruction
    load_insertion_point  -> the call instruction

   Finally, regs_used contains registers used to pass call arguments as well
    as the register that contains the return value.
 */
struct CSR_call_info_t {
  Insn *store_insertion_point;
  Insn *load_insertion_point;
  CSR_BITSET regs_used;
  CSR_call_info_t() : store_insertion_point(nullptr), load_insertion_point(nullptr), regs_used(0) {}
};

class CallerSavedRegHandler {
 protected:
  CGFunc *cgfunc;
  RegType return_reg_type;

 private:
  bool ReturnsInt() const {
    return return_reg_type == kRegTyInt;
  }

  bool ReturnsFP() const {
    return return_reg_type == kRegTyFloat;
  }

 public:
  CallerSavedRegHandler(CGFunc *f);

  static const int kCsrBitsetDone = (CHAR_BIT * sizeof(CSR_BITSET));

  inline static void CsrBitsetSet(CSR_BITSET &bset, int pos) {
    CG_ASSERT(0 <= pos && pos <= 63, "");
    bset |= (1ULL << pos);
  }

  inline static void CsrBitsetUnset(CSR_BITSET &bset, int pos) {
    CG_ASSERT(0 <= pos && pos <= 63, "");
    bset &= ~(1ULL << pos);
  }

  inline static bool CsrBitsetIsEqual(const CSR_BITSET bset0, const CSR_BITSET bset1) {
    return bset0 == bset1;
  }

  inline static void CsrBitsetOr(CSR_BITSET &res, const CSR_BITSET bset0, const CSR_BITSET bset1) {
    res = bset0 | bset1;
  }

  inline static void CsrBitsetCopy(CSR_BITSET &res, const CSR_BITSET bset0) {
    res = bset0;
  }

  inline static void CsrBitsetSubtract(CSR_BITSET &res, const CSR_BITSET bset0, const CSR_BITSET bset1) {
    res = bset0 & ~bset1;  // A - B == A & ~B
  }

#define FOR_EACH_OF_BITS_SET(bit, set)                                    \
  for ((bit) = CsrBitsetFindFirstBitSet((set)); (bit) < (kCsrBitsetDone); \
       (bit) = CsrBitsetFindNextBitSet((set), (bit) + 1))

  static int CsrBitsetFindFirstBitSet(const CSR_BITSET s) {
    // assumes CSR_BITSET is 64-bit integer
    // __builtin_ffs*() : Returns one plus the index of the least
    //                    significant 1-bit of x, or if x is zero, returns zero.
    int n = __builtin_ffsll(s);
    return (n == 0 ? kCsrBitsetDone : n - 1);
  }

  static int CsrBitsetFindNextBitSet(const CSR_BITSET s, int startingAt) {
    if (startingAt >= kCsrBitsetDone) {
      return kCsrBitsetDone;
    }

    int64_t t = s & ~((1ULL << startingAt) - 1);
    int64_t n = __builtin_ffsll(t);
    return (n == 0 ? kCsrBitsetDone : n - 1);
  }
};

}  // namespace maplebe

#endif /* MAPLEBE_INCLUDE_CG_REGALLOC_H_ */
