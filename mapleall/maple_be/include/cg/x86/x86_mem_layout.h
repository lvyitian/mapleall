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

#ifndef MAPLEBE_INCLUDE_CG_X86MEMLAYOUT_H_
#define MAPLEBE_INCLUDE_CG_X86MEMLAYOUT_H_

#include "mem_layout.h"
#include "x86_abi.h"

namespace maplebe {

class X86SymbolAlloc : public SymbolAlloc {
 public:
  X86register BaseReg(void) {
    if (mem_segment->kind == MS_args_stkpassed || mem_segment->kind == MS_args_regpassed ||
        mem_segment->kind == MS_FPbased) {
      return RBP;
    } else if (mem_segment->kind == MS_args_to_stkpass || mem_segment->kind == MS_SPbased) {
      return RSP;
    } else {
      return RCC;  // dummy
    }
  }

  int32 Offset(void) {
    int32 ofst = offset;
    MemSegment *memseg = mem_segment;
    while (memseg->how_alloc.mem_segment != nullptr) {
      ofst += memseg->how_alloc.offset;
      memseg = memseg->how_alloc.mem_segment;
    }
    return ofst;
  }
};

class X86MemLayout : public MemLayout {
 public:
  MemSegment seg_FPbased;
  MemSegment seg_SPbased;

 public:
  explicit X86MemLayout(BECommon *b, MIRFunction *f, MapleAllocator *mallocator)
    : MemLayout(b, f, mallocator), seg_FPbased(MS_FPbased), seg_SPbased(MS_SPbased) {}

  /*virtual*/
  uint32 FindLargestActualArea(void);
  /*virtual*/
  void LayoutStackFrame();
  /* virtual */
  int32 StackFrameSize(void) {
    return seg_SPbased.size - seg_FPbased.size;
  }

  void AssignSpillLocationsToPseduRegisters() {}
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_X86MEMLAYOUT_H_
