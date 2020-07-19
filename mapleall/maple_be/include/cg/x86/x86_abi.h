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

#ifndef MAPLEBE_INCLUDE_CG_X86ABI_H_
#define MAPLEBE_INCLUDE_CG_X86ABI_H_

#include "x86_isa.h"
#include "types_def.h"
#include "be_common.h"

namespace maplebe {

using namespace maple;

namespace X86Abi {
constexpr static const X86register int_return_regs[2] = { RAX, RDX };
constexpr static const X86register float_return_regs[2] = { XMM0, XMM1 };
static const int32 num_int_parm_regs = 6;
constexpr static const X86register int_parm_regs[num_int_parm_regs] = { RDI, RSI, RDX, RCX, R8, R9 };
static const int32 num_float_parm_regs = 8;
constexpr static const X86register float_parm_regs[num_float_parm_regs] = { XMM0, XMM1, XMM2, XMM3,
                                                                            XMM4, XMM5, XMM6, XMM7 };
};  // namespace X86Abi

// See X86-64 ABI document
typedef enum {
  X86_64_NoClass,
  X86_64_IntegerClass,
  X86_64_SseClass,
  X86_64_MemoryClass,
  X86_64_X87Class,
  X86_64_X87upClass,
  X86_64_SSEupClass,
} X86_64_ArgumentClass;

// for specifying how a parameter is passed
struct PLocInfo {
  X86register reg0;  // 0 means parameter is stored on the stack
  X86register reg1;
  int32 memoffset;
  int32 memsize;
};

// for processing an incoming or outgoing parameter list
class ParmLocator {
 private:
  BECommon &be_;
  int32 parm_num_;        // number of all types of parameters processed so far
  int32 int_parm_num_;    // number of integer parameters processed so far
  int32 float_parm_num_;  // number of float parameters processed so far
  int32 last_memoffset_;

 public:
  ParmLocator(BECommon &b) : be_(b), parm_num_(0), int_parm_num_(0), float_parm_num_(0), last_memoffset_(0) {}

  ~ParmLocator() {}

  void LocateNextParm(MIRType *ty, PLocInfo &ploc);
};

// given the type of the return value, determines the return mechanism
class ReturnMechanism {
 public:
  uint8 regcount;        // number of registers <= 2 storing the return value
  bool fake_first_parm;  // whether returning in memory via fake first parameter
  X86register reg0;      // first register storing the return value
  X86register reg1;      // second register storing the return value
  PrimType ptype0;       // the primitive type stored in reg0
  PrimType ptype1;       // the primitive type stored in reg1

  ReturnMechanism(MIRType *retty, BECommon &be);
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_X86ABI_H_
