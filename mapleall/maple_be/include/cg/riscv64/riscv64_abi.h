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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64ABI_H
#define MAPLEBE_INCLUDE_CG_AARCH64ABI_H

#include "riscv64_isa.h"
#include "types_def.h"
#include "be_common.h"

namespace maplebe {

using namespace maple;

namespace Riscv64Abi
{
static const int32 kNumIntParmRegs = 8;
static const int32 kNumFloatParmRegs = 8;
static const int32 kNumIntReturnRegs = 2;
static const int32 kNumFloatReturnRegs = 2;
static const int32 kMaxInstrForCondBr = 4000; // approximately less than (2^12);

constexpr static const Riscv64reg_t int_return_regs[kNumIntParmRegs] = { R10, R11 };
constexpr static const Riscv64reg_t float_return_regs[kNumFloatParmRegs] = { V10, V11 };
constexpr static const Riscv64reg_t int_parm_regs[kNumIntParmRegs] = { R10, R11, R12, R13, R14, R15, R16, R17 };
constexpr static const Riscv64reg_t float_parm_regs[kNumFloatParmRegs] = { V10, V11, V12, V13, V14, V15, V16, V17 };

// Refer to ARM IHI 0055C_beta: Procedure Call Standard  for
// ARM 64-bit Architecture. Section 5.5
static const Riscv64reg_t kIntRetReg0 = R10;
static const Riscv64reg_t kFpRetReg0 = V10;

// This is a temporary patch for a temporary physical register used after register allocation.
static const Riscv64reg_t kIntSpareReg = R1;
static const Riscv64reg_t kFpSpareReg = V31;

inline bool IsCalleeSavedReg(Riscv64reg_t reg) {
  return (reg >= R1 && reg <= R4) || (reg >= R8 && reg <= R9) || (reg >= R18 && reg <= R27) ||
         (reg >= V8 && reg <= V9) || (reg >= V18 && reg <= V27);
}

}  // namespace Riscv64Abi

// Refer to ARM IHI 0055C_beta: Procedure Call Standard  for
// ARM 64-bit Architecture. Table 1.
enum Riscv64_ArgumentClass {
  kRiscv64NoClass,
  kRiscv64IntegerClass,
  kRiscv64FloatClass,
  kRiscv64ShortVectorClass,
  kRiscv64PointerClass,
  kRiscv64CompositeTypeHFAClass,  // Homegeneous Floating-point Aggregates
  kRiscv64CompositeTypeHVAClass,  // Homegeneous Short-Vector Aggregates
  kRiscv64MemoryClass
};

// for specifying how a parameter is passed
struct PLocInfo {
  Riscv64reg_t reg0;  // 0 means parameter is stored on the stack
  Riscv64reg_t reg1;
  uint8 rsize0;       // regsize occupied, 4 or 8 for agg's 1st/2nd reg
  uint8 rsize1;
  int32 memoffset;
  int32 memsize;
};

/*
   We use the names used in ARM IHI 0055C_beta. $ 5.4.2.
   NGRN (= _int_parm_num)  : Next General-purpose Register number
   NSRN (= _float_parm_num): Next SIMD and Floating-point Register Number
   NSAA (= _last_memoffset): Next Stacked Argument Address
   NRGN                    : Next Return General-purpose Register number
   NRFN                    : Next Return Floating-point Register Number
 */
// for processing an incoming or outgoing parameter list
class ParmLocator {
 private:
  BECommon &_be;
  int32 _parm_num;  // number of all types of parameters processed so far
  int32 NGRN;       // number of integer parameters processed so far
  int32 NSRN;       // number of float parameters processed so far
  int32 NSAA;
  int32 NRGN;       // number of integer return processed so far
  int32 NRFN;       // number of float return processed so far

 public:
  // IHI 0055C_beta $ 5.4.2 Stage A (NSAA is set to 0, meaning "relative to the current SP")
  ParmLocator(BECommon &b) : _be(b), _parm_num(0), NGRN(0), NSRN(0), NSAA(0), NRGN(0), NRFN(0) {}

  virtual ~ParmLocator() {}

  // Return size of aggregate structure copy on stack.
  int32 LocateNextParm(MIRType *ty, PLocInfo &ploc, bool isFirst = false, bool isvararg = false);
  int32 LocateRetValue(MIRType *ty, PLocInfo &ploc);
  void InitPlocInfo(PLocInfo &ploc);

 private:
  inline Riscv64reg_t AllocateGPRegister() {
    return (NGRN < Riscv64Abi::kNumIntParmRegs) ? Riscv64Abi::int_parm_regs[NGRN++] : kRinvalid;
  }

  inline void AllocateTwoGPRegisters(PLocInfo &ploc) {
    if (NGRN + 1 < Riscv64Abi::kNumIntParmRegs) {
      ploc.reg0 = Riscv64Abi::int_parm_regs[NGRN++];
      ploc.reg1 = Riscv64Abi::int_parm_regs[NGRN++];
    } else {
      ploc.reg0 = kRinvalid;
    }
  }

  inline Riscv64reg_t AllocateSIMDFPRegister() {
    return (NSRN < Riscv64Abi::kNumFloatParmRegs) ? Riscv64Abi::float_parm_regs[NSRN++] : kRinvalid;
  }

  inline void AllocateNSIMDFPRegisters(PLocInfo &ploc, uint32 num) {
    if ((NSRN + num - 1) < Riscv64Abi::kNumFloatParmRegs) {
      switch (num) {
      case 1:
        ploc.reg0 = Riscv64Abi::float_parm_regs[NSRN++];
        break;
      case 2:
        ploc.reg0 = Riscv64Abi::float_parm_regs[NSRN++];
        ploc.reg1 = Riscv64Abi::float_parm_regs[NSRN++];
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

  inline Riscv64reg_t AllocateReturnGPRegister() {
    return (NGRN < Riscv64Abi::kNumIntReturnRegs) ? Riscv64Abi::int_return_regs[NRGN++] : kRinvalid;
  }

  inline Riscv64reg_t AllocateReturnFPRegister() {
    return (NSRN < Riscv64Abi::kNumFloatReturnRegs) ? Riscv64Abi::float_return_regs[NRFN++] : kRinvalid;
  }
};

// given the type of the return value, determines the return mechanism
class ReturnMechanism {
 public:
  uint8 regcount;     // number of registers <= 2 storing the return value
                      // bool fake_first_parm; // whether returning in memory via fake first parameter
  Riscv64reg_t reg0;  // first register storing the return value
  Riscv64reg_t reg1;  // second register storing the return value
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
