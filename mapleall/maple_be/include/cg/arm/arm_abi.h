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

#ifndef MAPLEBE_INCLUDE_CG_ARMABI_H_
#define MAPLEBE_INCLUDE_CG_ARMABI_H_

#include "arm_isa.h"
#include "types_def.h"
#include "be_common.h"

namespace maplebe {

using namespace maple;

namespace ArmAbi
{
constexpr static const Armregister int_return_regs[2] = { R0, R1 };
constexpr static const Armregister int64_return_regs[2] = { R0, R2 };
constexpr static const Armregister float_return_regs[2] = { S0, S1 };
constexpr static const Armregister double_return_regs[2] = { S0, S2 };
static const int32 num_int_parm_regs = 4;
constexpr static const Armregister int_parm_regs[num_int_parm_regs] = { R0, R1, R2, R3 };
static const int32 num_int64_parm_regs = 2;
constexpr static const Armregister int64_parm_regs[num_int64_parm_regs] = { R0, R2 };
static const int32 num_float_parm_regs = 16;
constexpr static const Armregister float_parm_regs[num_float_parm_regs] = { S0, S1, S2,  S3,  S4,  S5,  S6,  S7,
                                                                            S8, S9, S10, S11, S12, S13, S14, S15 };
static const int32 num_double_parm_regs = 8;  // for f64 size arm use
constexpr static const Armregister double_parm_regs[num_double_parm_regs] = { S0, S2, S4, S6, S8, S10, S12, S14 };
};  // namespace ArmAbi

// See ARM ABI document
typedef enum {
  ARM_NoClass,
  ARM_IntegerClass,
  ARM_Integer64Class,
  ARM_FloatClass,
  ARM_DoubleClass,
  ARM_MemoryClass,
} ARM_ArgumentClass;

// for specifying how a parameter is passed
struct PLocInfo {
  Armregister reg0;  // 0 means parameter is stored on the stack
  Armregister reg1;
  int32 memoffset;
  int32 memsize;
};

// for processing an incoming or outgoing parameter list
class ParmLocator {
 private:
  BECommon &be_;
  int32 parm_num_;        // number of all types of parameters processed so far
  int32 int_parm_num_;    // number of integer parameters processed so far
  int32 int64_parm_num_;  // number of integer 64 parameters
  int32 float_parm_num_;  // number of float parameters processed so far
  int32 double_parm_num_;
  int32 last_memoffset_;

 public:
  ParmLocator(BECommon &b)
    : be_(b),
      parm_num_(0),
      int_parm_num_(0),
      int64_parm_num_(0),
      float_parm_num_(0),
      double_parm_num_(0),
      last_memoffset_(0) {}

  ~ParmLocator() {}

  void LocateNextParm(MIRType *ty, PLocInfo &ploc);
};

// given the type of the return value, determines the return mechanism
class ReturnMechanism {
 public:
  uint8 regcount;        // number of registers <= 2 storing the return value
  bool fake_first_parm;  // whether returning in memory via fake first parameter
  Armregister reg0;      // first register storing the return value
  Armregister reg1;      // second register storing the return value
  PrimType ptype0;       // the primitive type stored in reg0
  PrimType ptype1;       // the primitive type stored in reg1

  ReturnMechanism(MIRType *retty, BECommon &be);
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_ARMABI_H_
