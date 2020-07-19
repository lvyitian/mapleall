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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64ABI_H
#define MAPLEBE_INCLUDE_CG_AARCH64ABI_H

#include "aarch64_isa.h"
#include "types_def.h"
#include "be_common.h"

namespace maplebe {

using namespace maple;

namespace AArch64Abi
{
static const int32 kNumIntParmRegs = 8;
static const int32 kNumFloatParmRegs = 8;

constexpr static const AArch64reg_t int_return_regs[kNumIntParmRegs] = { R0, R1, R2, R3, R4, R5, R6, R7 };
constexpr static const AArch64reg_t float_return_regs[kNumFloatParmRegs] = { V0, V1, V2, V3, V4, V5, V6, V7 };
constexpr static const AArch64reg_t int_parm_regs[kNumIntParmRegs] = { R0, R1, R2, R3, R4, R5, R6, R7 };
constexpr static const AArch64reg_t float_parm_regs[kNumFloatParmRegs] = { V0, V1, V2, V3, V4, V5, V6, V7 };

// Refer to ARM IHI 0055C_beta: Procedure Call Standard  for
// ARM 64-bit Architecture. Section 5.5
static const AArch64reg_t kIntRetReg0 = R0;
static const AArch64reg_t kFpRetReg0 = V0;

inline bool IsCalleeSavedReg(AArch64reg_t reg) {
  return (R19 <= reg && reg <= R30) || (V8 <= reg && reg <= V15);
}

}  // namespace AArch64Abi

// Refer to ARM IHI 0055C_beta: Procedure Call Standard  for
// ARM 64-bit Architecture. Table 1.
enum AArch64_ArgumentClass {
  kAArch64NoClass,
  kAArch64IntegerClass,
  kAArch64FloatClass,
  kAArch64ShortVectorClass,
  kAArch64PointerClass,
  kAArch64CompositeTypeHFAClass,  // Homegeneous Floating-point Aggregates
  kAArch64CompositeTypeHVAClass,  // Homegeneous Short-Vector Aggregates
  kAArch64MemoryClass
};

// for specifying how a parameter is passed
struct PLocInfo {
  AArch64reg_t reg0;  // 0 means parameter is stored on the stack
  AArch64reg_t reg1;
  AArch64reg_t reg2;
  AArch64reg_t reg3;
  int32 memoffset;
  int32 memsize;
  uint32 fpSize;
  uint32 numFpPureRegs;
};

/*
   We use the names used in ARM IHI 0055C_beta. $ 5.4.2.
   NGRN (= _int_parm_num)  : Next General-purpose Register number
   NSRN (= _float_parm_num): Next SIMD and Floating-point Register Number
   NSAA (= _last_memoffset): Next Stacked Argument Address
 */
// for processing an incoming or outgoing parameter list
class ParmLocator {
 private:
  BECommon &_be;
  int32 _parm_num;  // number of all types of parameters processed so far
  int32 NGRN;       // number of integer parameters processed so far
  int32 NSRN;       // number of float parameters processed so far
  int32 NSAA;

 public:
  // IHI 0055C_beta $ 5.4.2 Stage A (NSAA is set to 0, meaning "relative to the current SP")
  ParmLocator(BECommon &b) : _be(b), _parm_num(0), NGRN(0), NSRN(0), NSAA(0) {}

  virtual ~ParmLocator() {}

  // Return size of aggregate structure copy on stack.
  int32 LocateNextParm(MIRType *ty, PLocInfo &ploc, bool isFirst = false);
  void InitPlocInfo(PLocInfo &ploc);

 private:
  inline AArch64reg_t AllocateGPRegister() {
    return (NGRN < AArch64Abi::kNumIntParmRegs) ? AArch64Abi::int_parm_regs[NGRN++] : kRinvalid;
  }

  inline void AllocateTwoGPRegisters(PLocInfo &ploc) {
    if (NGRN + 1 < AArch64Abi::kNumIntParmRegs) {
      ploc.reg0 = AArch64Abi::int_parm_regs[NGRN++];
      ploc.reg1 = AArch64Abi::int_parm_regs[NGRN++];
    } else {
      ploc.reg0 = kRinvalid;
    }
  }

  inline AArch64reg_t AllocateSIMDFPRegister() {
    return (NSRN < AArch64Abi::kNumFloatParmRegs) ? AArch64Abi::float_parm_regs[NSRN++] : kRinvalid;
  }

  inline void AllocateNSIMDFPRegisters(PLocInfo &ploc, uint32 num) {
    if ((NSRN + num - 1) < AArch64Abi::kNumFloatParmRegs) {
      switch (num) {
      case 1:
        ploc.reg0 = AArch64Abi::float_parm_regs[NSRN++];
        break;
      case 2:
        ploc.reg0 = AArch64Abi::float_parm_regs[NSRN++];
        ploc.reg1 = AArch64Abi::float_parm_regs[NSRN++];
        break;
      case 3:
        ploc.reg0 = AArch64Abi::float_parm_regs[NSRN++];
        ploc.reg1 = AArch64Abi::float_parm_regs[NSRN++];
        ploc.reg2 = AArch64Abi::float_parm_regs[NSRN++];
        break;
      case 4:
        ploc.reg0 = AArch64Abi::float_parm_regs[NSRN++];
        ploc.reg1 = AArch64Abi::float_parm_regs[NSRN++];
        ploc.reg2 = AArch64Abi::float_parm_regs[NSRN++];
        ploc.reg3 = AArch64Abi::float_parm_regs[NSRN++];
        break;
      default:
        CHECK_FATAL(0, "AllocateNSIMDFPRegisters: unsupported");
      }
    } else {
      ploc.reg0 = kRinvalid;
    }
  }

  inline void RoundNGRNUpToNextEven() {
    NGRN = static_cast<int32>((NGRN + 1) & ~static_cast<int32>(1));
  }
};

// given the type of the return value, determines the return mechanism
class ReturnMechanism {
 public:
  uint8 regcount;     // number of registers <= 2 storing the return value
                      // bool fake_first_parm; // whether returning in memory via fake first parameter
  AArch64reg_t reg0;  // first register storing the return value
  AArch64reg_t reg1;  // second register storing the return value
  AArch64reg_t reg2;  // third register storing the return value
  AArch64reg_t reg3;  // fourth register storing the return value
  PrimType ptype0;    // the primitive type stored in reg0
  PrimType ptype1;    // the primitive type stored in reg1
                      // There is no need for extra ptype, as reg2 and reg3 should be same as reg0

  ReturnMechanism(MIRType *retty, BECommon &be);

  virtual ~ReturnMechanism() {}

  void SetupToReturnThroughMemory() {
    regcount = 1;
    reg0 = R8;
    ptype0 = PTY_u64;
  }

  void SetupSecondRetReg(MIRType *retty2);
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_AARCH64ABI_H
