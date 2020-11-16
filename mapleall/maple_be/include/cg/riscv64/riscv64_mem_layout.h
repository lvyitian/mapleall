/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64MEMLAYOUT_H_
#define MAPLEBE_INCLUDE_CG_AARCH64MEMLAYOUT_H_

#include "mem_layout.h"
#include "riscv64_abi.h"

namespace maplebe {

class Riscv64SymbolAlloc : public SymbolAlloc {
  Riscv64reg_t reg0;
  Riscv64reg_t reg1;

 public:
  Riscv64SymbolAlloc() : reg0(kRinvalid), reg1(kRinvalid) {}

  ~Riscv64SymbolAlloc() {}

  void SetRegisters(Riscv64reg_t r0, Riscv64reg_t r1) {
    reg0 = r0;
    reg1 = r1;
  }

  inline bool IsRegister() {
    return reg0 != kRinvalid;
  }

  inline Riscv64reg_t GetFirstRegister() {
    return reg0;
  }

  inline Riscv64reg_t GetSecondRegister() {
    return reg1;
  }
};

/*
   On Riscv64, stack frames are structured as follows:

   The stack grows downward -- full descending (SP points
   to a filled slot).

   Any of the parts of a frame is optional, i.e., it is
   possible to write a caller-callee pair in such a way
   that the particular part is absent in the frame.

   Before a call is made, the frame looks like:
 |                            |
 ||----------------------------|
 | args passed on the stack   | (we call them up-formals)
 ||----------------------------|<- Stack Pointer
 |                            |

   V1.
   Right after a call is made
 |                            |
 ||----------------------------|
 | args passed on the stack   |
 ||----------------------------|<- Stack Pointer
 | PREV_FP, PREV_LR           |
 ||----------------------------|<- Frame Pointer

   After the prologue has run,
 |                            |
 ||----------------------------|
 | args passed on the stack   |
 ||----------------------------|
 | PREV_FP, PREV_LR           |
 ||----------------------------|<- Frame Pointer
 | callee-saved registers     |
 ||----------------------------|
 | empty space. should have   |
 | at least 16-byte alignment |
 ||----------------------------|
 | local variables            |
 ||----------------------------|
 | variable-sized local vars  |
 | (VLAs)                     |
 ||----------------------------|<- Stack Pointer

 * callee-saved registers include
    1. R19-R28
    2. R8 if return value needs to be returned
       thru memory and callee wants to use R8
    3. we don't need to save R19 if it is used
       as base register for PIE.
    4. V8-V15

   V2. (this way, we may be able to save
       on SP modifying instruction)
   Right after a call is made
 |                            |
 ||----------------------------|
 | args passed on the stack   |
 ||----------------------------|<- Stack Pointer
 |                            |
 | empty space                |
 |                            |
 ||----------------------------|
 | PREV_FP, PREV_LR           |
 ||----------------------------|<- Frame Pointer

   After the prologue has run,
 |                            |
 ||----------------------------|
 | args passed on the stack   |
 ||----------------------------|
 | callee-saved registers     |
 | including those used for   |
 | parameter passing          |
 ||----------------------------|
 | empty space. should have   |
 | at least 16-byte alignment |
 ||----------------------------|
 | local variables            |
 ||----------------------------|
 | PREV_FP, PREV_LR           |
 ||----------------------------|<- Frame Pointer
 | variable-sized local vars  |
 | (VLAs)                     |
 ||----------------------------|
 | args to pass through stack |
 ||----------------------------|
 */

class Riscv64MemLayout : public MemLayout {
  /*
     inherited fields from MemLayout
     MemSegment seg_upformal; // this is the segment for parameters passed thru stack
                             // because no available registers for them or they
                             // are too large to be passed thru registers according to
                             // the ABI.
     MemSegment seg_formal;   // this is the segment for parameters passed thru registers.
                             // We may need space for these on stack (not for Java, though)
     MemSegment seg_actual;
   */
  MemSegment seg_reflocals;
  MemSegment seg_GRSavearea;
  MemSegment seg_VRSavearea;

  // callee saved register R19-R28 (10)
  MemSegment seg__spillreg;

 public:
  MemSegment seg_lockobjslot;
  MemSegment seg_locals;  // these are accessed via Frame Pointer
  int32 fixstacksize;
  int32 lockinfosize;
  int32 lockslotsize;
  bool unusedslot;  // true if the frame exist an unused 8 byte slot that is generated by roundup.
 public:
  explicit Riscv64MemLayout(BECommon *b, MIRFunction *f, MapleAllocator *mallocator)
    : MemLayout(b, f, mallocator),
      seg_reflocals(kMsReflocals),
      seg_GRSavearea(kMsGRSavearea),
      seg_VRSavearea(kMsVRSavearea),
      seg__spillreg(kMsSpillReg),
      seg_lockobjslot(kMsLockslot),
      seg_locals(kMsLocals),
      fixstacksize(0),
      lockinfosize(SIZEOFPTR),
      lockslotsize(2 * SIZEOFPTR),
      unusedslot(false) {}

  ~Riscv64MemLayout() {}

  /*
     Returns stack space required for a call
     which is used to pass arguments that cannot be
     passed through registers
   */
  uint32 ComputeStackSpaceRequirementForCall(StmtNode *stmt, int32 &aggCopySize,  bool isIcall) override;

  void LayoutStackFrame(int32 &structCopySize, int32 &maxParmStackSize) override;

  void AssignSpillLocationsToPseudoRegisters() override;

  SymbolAlloc *AssignLocationToSpillReg(regno_t vrnum) override;

  int32 StackFrameSize();

  bool GetUnusedSlot();

  int32 RealStackFrameSize();

  int32 GetadjustforRefloc();

  inline MemSegment &locals() {
    return seg_locals;
  }

  inline MemSegment &Reflocals() {
    return seg_reflocals;
  }

  inline MemSegment &GRSavearea() {
    return seg_GRSavearea;
  }

  inline MemSegment &VRSavearea() {
    return seg_VRSavearea;
  }

  inline int32 GetSizeOfSpillReg() {
    return seg__spillreg.GetSize();
  }

  inline int32 GetSizeOfLocals() {
    return seg_locals.GetSize();
  }

  inline int32 GetSizeOfRefLocals() {
    return seg_reflocals.GetSize();
  }

  inline int32 GetSizeOfGRSavearea() {
    return seg_GRSavearea.GetSize();
  }

  inline int32 GetSizeOfVRSavearea() {
    return seg_VRSavearea.GetSize();
  }

  int32 GetReflocbaseLoc();
  int32 GetGRSaveareaBaseLoc();
  int32 GetVRSaveareaBaseLoc();
  void AdjustOffsetForCalleeSaved(SymbolAlloc &sym);
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_AARCH64MEMLAYOUT_H_
