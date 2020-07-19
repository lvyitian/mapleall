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

#ifndef MAPLEBE_INCLUDE_BE_MMPL_MMPLMEMLAYOUT_H_
#define MAPLEBE_INCLUDE_BE_MMPL_MMPLMEMLAYOUT_H_

/// C++ headers.
#include <utility>
#include <cstddef>
#include <cstdio>

#include "be_common.h"

/// MapleIR headers.
#include "tokens.h"
#include "lexer.h"
#include "opcodes.h"
#include "prim_types.h"
#include "mir_type.h"
#include "mir_const.h"
#include "mir_symbol.h"
#include "mir_nodes.h"
#include "mir_module.h"
#include "mir_preg.h"
#include "mir_function.h"
#include "mir_parser.h"

namespace maplebe {

typedef enum {
  MS_unknown,
  MS_upformal,  // for the incoming parameters that are passed on the caller's stack
  MS_formal,    // for the incoming parameters that are passed in registers
  MS_actual,    // for the outgoing parameters
  MS_local,     // for all local variables and temporaries
  MS_FPbased,   // addressed via offset from the frame pointer
  MS_SPbased,   // addressed via offset from the stack pointer
  MS_GPbased,   // addressed via offset from the global pointer
} MemSegmentKind;

class MemSegment;

// describes where a symbol is allocated
class SymbolAlloc {
 public:
  MemSegment *mem_segment;
  int32 offset;

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

  MemSegment(MemSegmentKind k, int32 sz) : kind(k), size(sz) {}

  ~MemSegment() {}

};  // class MemSegment

class MmplMemLayout {
 public:
  BECommon &be;
  MIRFunction *func;
  MemSegment seg_upformal;
  MemSegment seg_formal;
  MemSegment seg_actual;
  MemSegment seg_FPbased;
  MemSegment seg_SPbased;
  MapleVector<SymbolAlloc> sym_alloc_table;  // index is StIdx

 public:
  uint32 FindLargestActualArea(void);
  uint32 FindLargestActualArea(StmtNode *, int &);
  explicit MmplMemLayout(BECommon &b, MIRFunction *f, MapleAllocator *mallocator)
    : be(b),
      func(f),
      seg_upformal(MS_upformal),
      seg_formal(MS_formal),
      seg_actual(MS_actual),
      seg_FPbased(MS_FPbased, -SIZEOFPTR),
      seg_SPbased(MS_SPbased),
      sym_alloc_table(mallocator->Adapter()) {
    sym_alloc_table.resize(f->symtab->GetSymbolTableSize());
  }

  ~MmplMemLayout() {}

  void LayoutStackFrame(void);
  int32 StackFrameSize(void) const {
    return seg_SPbased.size - seg_FPbased.size;
  }

  int32 UpformalSize(void) const {
    return seg_upformal.size;
  }
};

class GlobalMemLayout {
 public:
  MemSegment seg_GPbased;
  MapleVector<SymbolAlloc> sym_alloc_table;  // index is StIdx
 private:
  BECommon &be_;

 public:
  GlobalMemLayout(BECommon &be, MapleAllocator *mallocator);
  ~GlobalMemLayout() {}

 private:
  void FillScalarValueInMap(uint32 startaddress, PrimType pty, MIRConst *c);
  void FillTypeValueInMap(uint32 startaddress, MIRType *ty, MIRConst *c);
  void FillSymbolValueInMap(const MIRSymbol *sym);
  void FillTypetaggedRefcountedBit(const MIRSymbol *sym);
};

// for specifying how a parameter is passed
struct PLocInfo {
  int32 memoffset;
  int32 memsize;
};

// for processing an incoming or outgoing parameter list
class ParmLocator {
 private:
  BECommon &be_;
  int32 parm_num_;  // number of all types of parameters processed so far
  int32 last_memoffset_;

 public:
  ParmLocator(BECommon &b) : be_(b), parm_num_(0), last_memoffset_(0) {}

  ~ParmLocator() {}

  void LocateNextParm(const MIRType *ty, PLocInfo &ploc);
};

// given the type of the return value, determines the return mechanism
class ReturnMechanism {
 public:
  bool fake_first_parm;  // whether returning in memory via fake first parameter
  PrimType ptype0;       // the primitive type stored in retval0

  ReturnMechanism(const MIRType *retty, BECommon &be);
};

}  // namespace maplebe

#endif  //  MAPLEBE_INCLUDE_BE_MMPL_MMPLMEMLAYOUT_H_
