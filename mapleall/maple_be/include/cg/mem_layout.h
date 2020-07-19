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

#ifndef MAPLEBE_INCLUDE_BE_MEMLAYOUT_H_
#define MAPLEBE_INCLUDE_BE_MEMLAYOUT_H_

#include "be_common.h"
#include "mir_function.h"
#include "mir_nodes.h"  // StmtNode
#include "cg_assert.h"

/// C++ headers.
#include <cstddef>
#include <utility>
#include <cstdio>

namespace maplebe {

typedef uint32_t regno_t;
typedef enum {
  kMsUnknown,
  /*
     Function arguments that are not passed through registers
     are passed to the callee through stack.
   */
  kMsArgsStkpassed,
  /*
     In between MS_args_stackpassed and kMsArgsRegpassed,
     we store call-saved registers if any.
   */
  /*
     Args passed via registers according to the architecture-specific ABI
     may need be stored in stack.
     1) In the unoptimized version, we implement a model (similar to GCC -O0)
        where all the values are initially stored in the memory and
        loaded into registers when needed, and stored back to the memory when
        their uses are done.
     2) In an optimized version, some register-passed values may need to be
       spilled into memory. We allocate the space in this Memory segment.
       (or we may allocate them in caller-saved; may be this is better...)
   */
  kMsArgsRegpassed,
  /*
     GR/VR Save areas for unnamed arguments under vararg functions
   */
  kMsGRSavearea,
  kMsVRSavearea,
  /*
     local (auto) variables
   */
  kMsReflocals,
  kMsLocals,
  kMsSpillReg,
  kMsLockslot,
  /*
     In between kMsLocals and MS_args_to_stackpass, we allocate
     a register-spill area and space for caller-saved registers
   */
  /*
     When a function calls another which takes some arguments
     that cannot be passed through registers, it is the caller's
     responsibility to allocate space for those arguments in memory.
   */
  kMsArgsToStkpass,
  kMsFPbased, /* ? */
  kMsSPbased, /* ? */
} MemSegmentKind;

class CGFunc;
class MemSegment;

// describes where a symbol is allocated
class SymbolAlloc {
 public:
  MemSegment *mem_segment;
  int64 offset;

 public:
  SymbolAlloc() : mem_segment(nullptr), offset(0) {}

  ~SymbolAlloc() {}

};  // class SymbolAlloc

// keeps track of the allocation of a memory segment
class MemSegment {
 public:
  MemSegmentKind kind;
  int32 size;             // size is negative if allocated offsets are negative
  SymbolAlloc how_alloc;  // this segment may be allocated inside another segment

 public:
  MemSegment(MemSegmentKind k) : kind(k), size(0) {}

  ~MemSegment() {}

  inline int32 GetSize() const {
    return size;
  }

};  // class MemSegment

class MemLayout {
 protected:
  MapleAllocator *mem_allocator;
  CGFunc *cgfunc;

 public:
  BECommon &be;
  MIRFunction *func;
  MemSegment seg__args_stkpassed;
  MemSegment seg__args_regpassed;
  MemSegment seg__args_to_stkpass;
  MapleVector<SymbolAlloc *> sym_alloc_table;  // index is stindex from StIdx
  MapleVector<SymbolAlloc *> spill_loc_table;  // index is preg idx
  MapleMap<regno_t, SymbolAlloc *> spillreg_loc_map;

 public:
  explicit MemLayout(BECommon *b, MIRFunction *f, MapleAllocator *mallocator)
    : mem_allocator(mallocator),
      cgfunc(nullptr),
      be(*b),
      func(f),
      seg__args_stkpassed(kMsArgsStkpassed),
      seg__args_regpassed(kMsArgsRegpassed),
      seg__args_to_stkpass(kMsArgsToStkpass),
      sym_alloc_table(mallocator->Adapter()),
      spill_loc_table(mallocator->Adapter()),
      spillreg_loc_map(std::less<regno_t>(), mallocator->Adapter()) {
    sym_alloc_table.resize(f->symTab == nullptr ? 0 : f->symTab->GetSymbolTableSize());
  }

  ~MemLayout() {}

  inline void SetCurrFunction(CGFunc *f) {
    cgfunc = f;
  }

  /*
     Returns stack space required for a call
     which is used to pass arguments that cannot be
     passed through registers
   */
  virtual uint32 ComputeStackSpaceRequirementForCall(StmtNode *stmt, int32 &aggCopySize, bool isIcall) = 0;

  /*
     Go over all outgoing calls in the function body and get the maximum space
     needed for storing the actuals based on the actual parameters and the ABI.
     These are usually those arguments that cannot be passed
     through registers because a call passes more than 8 arguments, or
     they cannot be fit in a pair of registers.
   */
  uint32 FindLargestActualArea(int32 &aggCopySize);

  virtual void LayoutStackFrame(int32 &structCopySize, int32 &maxParmStackSize) = 0;

  /*
     "Pseudo-registers can be regarded as local variables of a
     primitive type whose addresses are never taken"
   */
  virtual void AssignSpillLocationsToPseudoRegisters() = 0;

  virtual SymbolAlloc *AssignLocationToSpillReg(regno_t vrnum) = 0;

  inline SymbolAlloc *FindSymAllocInfo(uint32 stIdx) {
    return sym_alloc_table[stIdx];
  }

  inline SymbolAlloc *GetSpillLocOfPseduoRegister(PregIdx i) {
    return spill_loc_table.at(i);
  }

  inline SymbolAlloc *GetLocOfSpillRegister(regno_t vrnum) {
    SymbolAlloc *loc = nullptr;
    auto p = spillreg_loc_map.find(vrnum);
    if (p == spillreg_loc_map.end()) {
      loc = AssignLocationToSpillReg(vrnum);
    } else {
      loc = p->second;
    }
    return loc;
  }

  inline int32 SizeOfArgsToStackpass() {
    return seg__args_to_stkpass.GetSize();
  }

  inline int32 SizeOfArgsRegisterPassed() {
    return seg__args_regpassed.GetSize();
  }

  inline int32 SizeOfArgsStackPassed() {
    return seg__args_stkpassed.GetSize();
  }
};

}  // namespace maplebe

#endif  //  MAPLEBE_INCLUDE_BE_MEMLAYOUT_H_
