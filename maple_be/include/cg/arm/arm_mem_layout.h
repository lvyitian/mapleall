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

#ifndef MAPLEBE_INCLUDE_CG_ARMMEMLAYOUT_H_
#define MAPLEBE_INCLUDE_CG_ARMMEMLAYOUT_H_

#include "mem_layout.h"
#include "arm_abi.h"

namespace maplebe {

class ArmSymbolAlloc : public SymbolAlloc {
 public:
  Armregister BaseReg(void) {
    if (mem_segment->kind == MS_args_stkpassed || mem_segment->kind == MS_args_regpassed ||
        mem_segment->kind == MS_FPbased) {
      return RSP;
    } else if (mem_segment->kind == MS_args_to_stkpass || mem_segment->kind == MS_SPbased) {
      return RSP;
    } else {
      return RCC;  // dummy
    }
  }

  int32 Offset(void) {
    int32 ofst = offset;
    return ofst;
  }
};

class ArmMemLayout : public MemLayout {
 public:
  MemSegment seg_locals;

 public:
  explicit ArmMemLayout(BECommon *b, MIRFunction *f, MapleAllocator *mallocator)
    : MemLayout(b, f, mallocator), seg_locals(MS_locals) {}

  /*virtual*/
  uint32 FindLargestActualArea(void);
  /*virtual*/
  void LayoutStackFrame(void);
  /*virtual*/
  int32 StackFrameSize();
  void AssignSpillLocationsToPseduRegisters() {}
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_ARMMEMLAYOUT_H_
