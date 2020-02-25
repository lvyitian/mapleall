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

#include "aarch64_abi.h"
#include "aarch64_cg_func.h"
#include "be_common.h"
#include "cg_assert.h"

namespace maplebe {

using namespace maple;

/*
   Analyze the given aggregate using the rules given by the ARM 64-bit ABI and
   return the number of doublewords to be passed in registers; the classes of
   the doublewords are returned in parameter "classes"; if 0 is returned, it
   means the whole aggregate is passed in memory.
 */
static int32 ClassifyAggregate(BECommon &be, MIRType *ty, AArch64_ArgumentClass classes[2], size_t classesLength) {
  CHECK_FATAL(classesLength > 0, "invalid index");
  uint32 sizeofty = be.type_size_table.at(ty->tyIdx.GetIdx());
  // Rule B.3.
  // If the argument type is a Composite Type that is larger than 16 bytes
  // then the argument is copied to memory allocated by the caller and
  // the argument is replaced by a pointer to the copy.
  if (sizeofty > 16 || sizeofty == 0) {
    return 0;
  }

  // An argument of any Integer class takes up an integer register
  // which is a single double-word.
  // Rule B.4. The size of an argument of composite type is rounded up to the nearest
  // multiple of 8 bytes.
  int32 sizeoftyInDwords = RoundUp(sizeofty, 8) >> 3;

  CG_ASSERT(sizeoftyInDwords == 1 || sizeoftyInDwords == 2, "");
  int32 i;
  for (i = 0; i < sizeoftyInDwords; i++) {
    classes[i] = kAArch64NoClass;
  }
  if (ty->typeKind != kTypeStruct && ty->typeKind != kTypeArray) {
    // scalar type
    switch (ty->GetPrimType()) {
      case PTY_u1:
      case PTY_u8:
      case PTY_i8:
      case PTY_u16:
      case PTY_i16:
      case PTY_a32:
      case PTY_u32:
      case PTY_i32:
      case PTY_a64:
      case PTY_ptr:
      case PTY_ref:
      case PTY_u64:
      case PTY_i64:
        classes[0] = kAArch64IntegerClass;
        return 1;
      case PTY_f32:
      case PTY_f64:
      case PTY_c64:
      case PTY_c128:
        classes[0] = kAArch64FloatClass;
        return 1;
      default:
        CG_ASSERT(false, "");
    }
    // should not reach to this point
    return 0;
  } else {
    classes[0] = kAArch64IntegerClass;
    if (sizeoftyInDwords == 2) {
      classes[1] = kAArch64IntegerClass;
    }
    return sizeoftyInDwords;
  }
}

/*
   Refer to ARM IHI 0055C_beta: Procedure Call Standard for
   the ARM 64-bit Architecture. $5.4.2
 */
// LocateNextParm should be called with each parameter in the parameter list
// starting from the beginning, one call per parameter in sequence; it returns
// the information on how each parameter is passed in ploc
int32 ParmLocator::LocateNextParm(MIRType *ty, PLocInfo &ploc) {
  int typesize = _be.type_size_table.at(ty->tyIdx.GetIdx());
  int typealign = _be.type_align_table[ty->tyIdx.GetIdx()];
  ploc.reg0 = kRinvalid;
  ploc.reg1 = kRinvalid;
  ploc.memoffset = NSAA;
  // Rule C.12 states that we do round NSAA up before we use its value
  // according to the alignment requirement of the argument being processed.
  // We do the rounding up at the end of LocateNextParm(),
  // so we want to make sure our rounding up is correct.
  CG_ASSERT((NSAA & (std::max(typealign, 8) - 1)) == 0, "C.12 alignment requirement is violated");
  ploc.memsize = typesize;
  _parm_num++;

  int32 aggCopySize = 0;
  switch (ty->GetPrimType()) {
    case PTY_u1:
    case PTY_u8:
    case PTY_i8:
    case PTY_u16:
    case PTY_i16:
    case PTY_a32:
    case PTY_u32:
    case PTY_i32:
    case PTY_ptr:
    case PTY_ref:
    case PTY_a64:
    case PTY_u64:
    case PTY_i64:
      // Rule C.7
      CG_ASSERT(GetPrimTypeSize(PTY_i64) == 8, "");
      typesize = 8;  // 8 bytes
      ploc.reg0 = AllocateGPRegister();
      CG_ASSERT(NGRN <= AArch64Abi::kNumIntParmRegs, "");
      break;
    /*
       for c64 complex numbers, we assume
        - callers marshall the two f32 numbers into one f64 register
        - callees de-marshall one f64 value into the real and the imaginery part
     */
    case PTY_f32:
    case PTY_f64:
    case PTY_c64:
      // Rule C.1
      CG_ASSERT(GetPrimTypeSize(PTY_i64) == 8, "");
      CG_ASSERT(GetPrimTypeSize(PTY_f64) == 8, "");
      typesize = 8;  // 8 bytes
      ploc.reg0 = AllocateSIMDFPRegister();
      CG_ASSERT(NSRN <= AArch64Abi::kNumFloatParmRegs, "");
      break;

    /*
       for c128 complex numbers, we assume
        - callers marshall the two f64 numbers into one f128 register
        - callees de-marshall one f128 value into the real and the imaginery part
     */
    case PTY_c128:
      // SIMD-FP registers have 128-bits.
      ploc.reg0 = AllocateSIMDFPRegister();
      CG_ASSERT(NSRN <= AArch64Abi::kNumFloatParmRegs, "");
      CG_ASSERT(typesize == 16, "");
      break;

    /*
       case of quad-word integer:
       we don't support it yet.
       if (has-16-byte-alignment-requirement)
        NGRN = (NGRN+1) & ~1; // C.8 round it up to the next even number
       try allocate two consecutive registers at once.
     */
    case PTY_agg: {
      // In AArch64, integer-float or float-integer
      // argument passing is not allowed. All should go through
      // integer-integer.
      AArch64_ArgumentClass classes[2];
      ploc.memsize = typesize;
      if (typesize > 16) {
        aggCopySize = RoundUp(typesize, SIZEOFPTR);
      }
      /* alignment requirement */
      // Note. This is one of a few things iOS diverges from
      // the ARM 64-bit standard. They don't observe the round-up requirement.
      if (typealign == 16) {
        RoundNGRNUpToNextEven();
      }

      int32 numregs = ClassifyAggregate(_be, ty, classes, sizeof(classes));
      if (numregs == 1) {  // passing in registers
        typesize = 8;
        if (classes[0] == kAArch64FloatClass) {
          CHECK_FATAL(0, "PTY_agg: param passing in FP reg not allowed.");
          //ploc.reg0 = AllocateSIMDFPRegister();
        } else {
          ploc.reg0 = AllocateGPRegister();
          // Rule C.11
          CG_ASSERT(ploc.reg0 != kRinvalid || (NGRN == AArch64Abi::kNumIntParmRegs), "");
        }
      } else if (numregs == 2) {
        CG_ASSERT(classes[0] == kAArch64IntegerClass && classes[1] == kAArch64IntegerClass, "");
        // typesize = ploc.memsize; ?
        AllocateTwoGPRegisters(ploc);
        // Rule C.11
        if (ploc.reg0 == kRinvalid) {
          NGRN = 8;
        }
        CG_ASSERT(AArch64Abi::kNumIntParmRegs == 8, "AArch64Abi::kNumIntParmRegs should be set to 8");
      } else {
        /* 0 returned from ClassifyAggregate(). This means the whole data
         * is passed thru memory.
         * Rule B.3.
         *  If the argument type is a Composite Type that is larger than 16
         *  bytes then the argument is copied to memory allocated by the
         *  caller and the argument is replaced by a pointer to the copy.
         */
        // Try to allocate an integer register
        typesize = 8;  // 8 bytes
        ploc.reg0 = AllocateGPRegister();
        ploc.memsize = 8;  // byte size of a pointer in AArch64
        if (ploc.reg0 != kRinvalid) {
          numregs = 1;
        }
      }
      // compute rightpad
      if (numregs == 0 || ploc.reg0 == kRinvalid) {  // passed in memory
        int32 paddedSize = RoundUp(ploc.memsize, 8);
        typesize = paddedSize;
      }
      break;
    }  // case PTY_agg

    default:
      CG_ASSERT(false, "");
      break;
  }

  // Rule C.12
  if (ploc.reg0 == kRinvalid) {  // being passed in memory
    NSAA = ploc.memoffset + typesize /*+ rightpad*/;
  }
  return aggCopySize;
}

// instantiated with the type of the function return value, it describes how
// the return value is to be passed back to the caller
/*
   Refer to ARM IHI 0055C_beta: Procedure Call Standard for
   the ARM 64-bit Architecture. $5.5
   "If the type, T, of the result of a function is such that
      void func(T arg)
    would require that 'arg' be passed as a value in a register
    (or set of registers) according to the rules in $5.4 Parameter
    Passing, then the result is returned in the same registers
    as would be used for such an argument.
 */
ReturnMechanism::ReturnMechanism(MIRType *retty, BECommon &be)
  : regcount(0), reg0(kRinvalid), reg1(kRinvalid), ptype0(kPtyInvalid), ptype1(kPtyInvalid) {
  PrimType primType = retty->GetPrimType();
  switch (primType) {
    case PTY_void:
      break;
    case PTY_u1:
    case PTY_u8:
    case PTY_i8:
    case PTY_u16:
    case PTY_i16:
    case PTY_a32:
    case PTY_u32:
    case PTY_i32:
      regcount = 1;
      reg0 = AArch64Abi::int_return_regs[0];
      ptype0 = IsSignedInteger(primType) ? PTY_i32 : PTY_u32;  // promote the type
      return;

    case PTY_ptr:
    case PTY_ref:
      CHECK_FATAL(false, "PTY_ptr should have been lowered", "");

    case PTY_a64:
    case PTY_u64:
    case PTY_i64:
      regcount = 1;
      reg0 = AArch64Abi::int_return_regs[0];
      ptype0 = IsSignedInteger(primType) ? PTY_i64 : PTY_u64;  // promote the type
      return;

    /*
       for c64 complex numbers, we assume
        - callers marshall the two f32 numbers into one f64 register
        - callees de-marshall one f64 value into the real and the imaginery part
     */
    case PTY_f32:
    case PTY_f64:
    case PTY_c64:
      regcount = 1;
      reg0 = AArch64Abi::float_return_regs[0];
      ptype0 = primType;
      return;

    /*
       for c128 complex numbers, we assume
        - callers marshall the two f64 numbers into one f128 register
        - callees de-marshall one f128 value into the real and the imaginery part
     */
    case PTY_c128:
      regcount = 1;
      reg0 = AArch64Abi::float_return_regs[0];
      ptype0 = primType;
      return;

    /*
       Refer to ARM IHI 0055C_beta: Procedure Call Standard for
       the ARM 64-bit Architecture. $5.5
        "Otherwise, the caller shall reserve a block of memory of
         sufficient size and alignment to hold the result. The
         address of the memory block shall be passed as an additional
         argument to the function in x8. The callee may modify the
         result memory block at any point during the execution of the
         subroutine (there is no requirement for the callee to preserve
         the value stored in x8)."
     */
    case PTY_agg: {
      uint64 size = be.type_size_table.at(retty->tyIdx.GetIdx());
      if (size > 16 || size == 0) {
        // The return value is returned via memory.
        // The address is in X8 and passed by the caller.
        SetupToReturnThroughMemory();
        return;
      }
      AArch64_ArgumentClass classes[2];
      regcount = ClassifyAggregate(be, retty, classes, sizeof(classes));
      if (regcount == 0) {
        SetupToReturnThroughMemory();
        return;
      } else {
        if (regcount == 1) {  // passing in registers
          if (classes[0] == kAArch64FloatClass) {
            reg0 = AArch64Abi::float_return_regs[0];
            ptype0 = PTY_f64;
          } else {
            reg0 = AArch64Abi::int_return_regs[0];
            ptype0 = PTY_i64;
          }
        } else {
          CG_ASSERT(regcount == 2, "reg count from ClassifyAggregate() should be 0, 1, or 2");
          CG_ASSERT(classes[0] == kAArch64IntegerClass && classes[1] == kAArch64IntegerClass, "");
          reg0 = AArch64Abi::int_return_regs[0];
          ptype0 = PTY_i64;
          reg1 = AArch64Abi::int_return_regs[1];
          ptype1 = PTY_i64;
        }
        return;
      }
    }

    default:
      CHECK_FATAL(0, "NYI");
      return;
  }
}

void ReturnMechanism::SetupSecondRetReg(MIRType *retty2) {
  CG_ASSERT(reg1 == kRinvalid, "");
  PrimType primType = retty2->GetPrimType();
  switch (primType) {
    case PTY_void:
      break;
    case PTY_u1:
    case PTY_u8:
    case PTY_i8:
    case PTY_u16:
    case PTY_i16:
    case PTY_a32:
    case PTY_u32:
    case PTY_i32:
    case PTY_ptr:
    case PTY_ref:
    case PTY_a64:
    case PTY_u64:
    case PTY_i64:
      reg1 = AArch64Abi::int_return_regs[1];
      ptype1 = IsSignedInteger(primType) ? PTY_i64 : PTY_u64;  // promote the type
      break;
    default:
      CG_ASSERT(false, "NYI");
  }
}

/*
   From "ARM Procedure Call Standard for ARM 64-bit Architecture"
       ARM IHI 0055C_beta, 6th November 2013
   $ 5.1 machine Registers
   $ 5.1.1 General-Purpose Registers
    <Table 2>                Note
    SP       Stack Pointer
    R30/LR   Link register   Stores the return address.
                             We push it into stack along with FP on function
                             entry using STP and restore it on function exit
                             using LDP even if the function is a leaf (i.e.,
                             it does not call any other function) because it
                             is free (we have to store FP anyway).  So, if a
                             function is a leaf, we may use it as a temporary
                             register.
    R29/FP   Frame Pointer
    R19-R28  Callee-saved
             registers
    R18      Platform reg    Can we use it as a temporary register?
    R16,R17  IP0,IP1         Maybe used as temporary registers. Should be
                             given lower priorities. (i.e., we push them
                             into the free register stack before the others)
    R9-R15                   Temporary registers, caller-saved



    Note:
    R16 and R17 may be used by a linker as a scratch register between
    a routine and any subroutine it calls. They can also be used within a
    routine to hold intermediate values between subroutine calls.

    The role of R18 is platform specific. If a platform ABI has need of
    a dedicated general purpose register to carry inter-procedural state
    (for example, the thread context) then it should use this register for
    that purpose. If the platform ABI has no such requirements, then it should
    use R18 as an additional temporary register. The platform ABI specification
    must document the usage for this register.

    A subroutine invocation must preserve the contents of the registers R19-R29
    and SP. All 64 bits of each value stored in R19-R29 must be preserved, even
    when using the ILP32 data model.

    $ 5.1.2 SIMD and Floating-Point Registers

    The first eight registers, V0-V7, are used to pass argument values into
    a subroutine and to return result values from a function. They may also
    be used to hold intermediate values within a routine.

    V8-V15 must be preserved by a callee across subroutine calls; the
    remaining registers do not need to be preserved( or caller-saved).
    Additionally, only the bottom 64 bits of each value stored in V8-
    V15 need to be preserved.

 */
/*
   Temporary registers: basically, caller-saved.
   if one is alive after a call, the caller should spill it and reload it after the control returns from the call.

   parameter passing : they are basically caller-saved, temporary except their values are defined on entry
   and not available initially.

   then, callee-saved. If a function wants to use it, unless the function is the start of function execution (i.e.,
   main) it has to stash the old value, and restore it before the control leaves the function.

   so, there are two kinds. caller-saved, temporary and callee-saved
   We allocate registers from caller-saved first, then calee-saved if the former runs out.
   We also need to have float-type register files as well.
 */

}  // namespace maplebe
