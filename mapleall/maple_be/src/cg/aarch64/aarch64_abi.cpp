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

#include "aarch64_abi.h"
#include "aarch64_cg_func.h"
#include "be_common.h"
#include "cg_assert.h"

namespace maplebe {

using namespace maple;

void ParmLocator::InitPlocInfo(PLocInfo &ploc) {
  ploc.reg0 = kRinvalid;
  ploc.reg1 = kRinvalid;
  ploc.reg2 = kRinvalid;
  ploc.reg3 = kRinvalid;
  ploc.memoffset = NSAA;
  ploc.fpSize = 0;
  ploc.numFpPureRegs = 0;
}

static PrimType TraverseStructFieldsForFp(MIRType *ty, uint32 &numregs) {
  if (ty->typeKind == kTypeArray) {
    MIRArrayType *arrtype = static_cast<MIRArrayType *>(ty);
    MIRType *pty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(arrtype->eTyIdx);
    if (pty->typeKind == kTypeArray || pty->typeKind == kTypeStruct) {
      return TraverseStructFieldsForFp(pty, numregs);
    }
    for (uint32 i = 0; i < arrtype->dim; ++i) {
      numregs += arrtype->sizeArray[i];
    }
    return pty->primType;
  } else if (ty->typeKind == kTypeStruct) {
    MIRStructType *sttype = static_cast<MIRStructType *>(ty);
    FieldVector fields = sttype->fields;
    PrimType oldtype = PTY_void;
    for (uint32 fcnt = 0; fcnt < fields.size(); ++fcnt) {
      TyIdx fieldtyidx = fields[fcnt].second.first;
      MIRType *fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);
      PrimType ptype = TraverseStructFieldsForFp(fieldty, numregs);
      if (oldtype != PTY_void && oldtype != ptype) {
        return PTY_void;
      } else {
        oldtype = ptype;
      }
    }
    return oldtype;
  } else {
    numregs++;
    return ty->primType;
  }
}

/*
   Analyze the given aggregate using the rules given by the ARM 64-bit ABI and
   return the number of doublewords to be passed in registers; the classes of
   the doublewords are returned in parameter "classes"; if 0 is returned, it
   means the whole aggregate is passed in memory.
 */
static int32 ClassifyAggregate(BECommon &be, MIRType *ty, AArch64_ArgumentClass classes[4], size_t classesLength, uint32 &fpSize) {
  CHECK_FATAL(classesLength > 0, "invalid index");
  uint32 sizeofty = be.type_size_table.at(ty->tyIdx.GetIdx());
  // Rule B.3.
  // If the argument type is a Composite Type that is larger than 16 bytes
  // then the argument is copied to memory allocated by the caller and
  // the argument is replaced by a pointer to the copy.
  if (sizeofty > 16 || sizeofty == 0) {
    classes[0] = kAArch64NoClass;
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
  if (ty->typeKind != kTypeStruct && ty->typeKind != kTypeUnion && ty->typeKind != kTypeArray) {
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
    // If small struct is composed of all float or all double, cannot mix with each
    // other type including the float or double, then the fp fields are passed in
    // fp param registers exclusively.  However, it is all or nothing, so if there
    // are not enough fp registers to satisfy the demand, then it will be passed in
    // memory. Type float, 4 bytes, is the smallest type supported.
    if (CGOptions::doStructFpInInt) {
    if (ty->typeKind == kTypeStruct) {
      MIRStructType *sty = static_cast<MIRStructType *>(ty);
      FieldVector fields = sty->fields;
      bool isF32 = false;
      bool isF64 = false;
      uint32 numregs = 0;
      for (uint32 fcnt = 0; fcnt < fields.size(); ++fcnt) {
        TyIdx fieldtyidx = fields[fcnt].second.first;
        MIRType *fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);
        PrimType ptype = TraverseStructFieldsForFp(fieldty, numregs);
        if (ptype == PTY_f32) {
          if (isF64) {
            isF32 = isF64 = false;
            break;
          }
          isF32 = true;
        } else if (ptype == PTY_f64) {
          if (isF32) {
            isF32 = isF64 = false;
            break;
          }
          isF64 = true;
        } else {
          isF32 = isF64 = false;
          break;
        }
      }
      if (isF32 || isF64) {
        for (uint32 i = 0; i < fields.size(); ++i) {
          classes[i] = kAArch64FloatClass;
        }
        fpSize = isF32 ? 4 : 8;
        return numregs;
      }
    }
    }

    classes[0] = kAArch64IntegerClass;
    if (sizeoftyInDwords == 2) {
      classes[1] = kAArch64IntegerClass;
    }
    return sizeoftyInDwords;
  }
}

int32 ParmLocator::LocateRetVal(MIRType *retty, PLocInfo &ploc) {
  InitPlocInfo(ploc);
  int retsz = _be.type_size_table.at(retty->tyIdx.GetIdx());
  if (retsz == 0) {
    return 0;    // size 0 ret val
  }
  if (retsz <= 16) {
    // For return struct size less or equal to 16 bytes, the values
    // are returned in register pairs.
    AArch64_ArgumentClass classes[4]; // Max of four floats.
    uint32 fpsize;
    int32 numregs = ClassifyAggregate(_be, retty, classes, sizeof(classes), fpsize);
    if (classes[0] == kAArch64FloatClass) {
      CHECK_FATAL(numregs <= 4, "LocateNextParm: illegal number of regs");
      AllocateNSIMDFPRegisters(ploc, numregs);
      ploc.numFpPureRegs = numregs;
      ploc.fpSize = fpsize;
      return 0;
    } else {
      CHECK_FATAL(numregs <= 2, "LocateNextParm: illegal number of regs");
      if (numregs == 1) {
        ploc.reg0 = AllocateGPRegister();
      } else {
        AllocateTwoGPRegisters(ploc);
      }
      return 0;
    }
  } else {
    // For return struct size > 16 bytes the pointer returns in x8.
    ploc.reg0 = R8;
    return SIZEOFPTR;
  }
}

/*
   Refer to ARM IHI 0055C_beta: Procedure Call Standard for
   the ARM 64-bit Architecture. $5.4.2
 */
// LocateNextParm should be called with each parameter in the parameter list
// starting from the beginning, one call per parameter in sequence; it returns
// the information on how each parameter is passed in ploc
int32 ParmLocator::LocateNextParm(MIRType *ty, PLocInfo &ploc, bool isFirst) {
  InitPlocInfo(ploc);
  if (isFirst) {
    MIRFunction *func = _be.mirModule.CurFunction();
    auto funcIt = _be.funcReturnType.find(func);
    if (funcIt != _be.funcReturnType.end()) {
      TyIdx retidx = funcIt->second;
      uint32 retsz = _be.type_size_table[retidx.GetIdx()];
      if (retsz == 0) {
        // For return struct size 0 there is no return value.
        return 0;
      } else if (retsz <= 16) {
        // For return struct size less or equal to 16 bytes, the values
        // are returned in register pairs.
        AArch64_ArgumentClass classes[4]; // Max of four floats.
        uint32 fpsize;
        MIRType *retty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(retidx);
        int32 numregs = ClassifyAggregate(_be, retty, classes, sizeof(classes), fpsize);
        if (classes[0] == kAArch64FloatClass) {
          CHECK_FATAL(numregs <= 4, "LocateNextParm: illegal number of regs");
          AllocateNSIMDFPRegisters(ploc, numregs);
          ploc.numFpPureRegs = numregs;
          ploc.fpSize = fpsize;
          return 0;
        } else {
          CHECK_FATAL(numregs <= 2, "LocateNextParm: illegal number of regs");
          if (numregs == 1) {
            ploc.reg0 = AllocateGPRegister();
          } else {
            AllocateTwoGPRegisters(ploc);
          }
          return 0;
        }
      } else {
        // For return struct size > 16 bytes the pointer returns in x8.
        ploc.reg0 = R8;
        return SIZEOFPTR;
      }
    }
  }
  int typesize = _be.type_size_table.at(ty->tyIdx.GetIdx());
  if (typesize == 0) {
    return 0;
  }
  int typealign = _be.type_align_table[ty->tyIdx.GetIdx()];
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
      // In AArch64, all should go through integer-integer, except in the case
      // where a struct is homogeneous composed of one of the fp types.
      // Then it can be passed through float-float.
      AArch64_ArgumentClass classes[4]; // Max of four floats.
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

      uint32 fpsize;
      int32 numregs = ClassifyAggregate(_be, ty, classes, sizeof(classes), fpsize);
      if (classes[0] == kAArch64FloatClass) {
        CHECK_FATAL(numregs <= 4, "LocateNextParm: illegal number of regs");
        typesize = 8;
        AllocateNSIMDFPRegisters(ploc, numregs);
        ploc.numFpPureRegs = numregs;
        ploc.fpSize = fpsize;
      } else if (numregs == 1) {  // passing in registers
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
      uint32 fpsize;
      AArch64_ArgumentClass classes[4];
      regcount = ClassifyAggregate(be, retty, classes, sizeof(classes), fpsize);
      if (classes[0] == kAArch64FloatClass) {
        switch (regcount) {
        case 4:
          reg3 = AArch64Abi::float_return_regs[3];
        case 3:
          reg2 = AArch64Abi::float_return_regs[2];
        case 2:
          reg1 = AArch64Abi::float_return_regs[1];
        case 1:
          reg0 = AArch64Abi::float_return_regs[0];
          break;
        default:
          CHECK_FATAL(0, "ReturnMechanism: unsupported");
        }
        if (fpsize == 4) {
          ptype0 = ptype1 = PTY_f32;
        } else {
          ptype0 = ptype1 = PTY_f64;
        }
        return;
      } else if (regcount == 0) {
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
