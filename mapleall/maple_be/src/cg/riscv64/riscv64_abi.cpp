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

#include "riscv64_abi.h"
#include "riscv64_cg_func.h"
#include "be_common.h"
#include "cg_assert.h"

namespace maplebe {

using namespace maple;

void ParmLocator::InitPlocInfo(PLocInfo &ploc) {
  ploc.reg0 = kRinvalid;
  ploc.reg1 = kRinvalid;
  ploc.rsize0 = 0;
  ploc.rsize1 = 0;
  ploc.memoffset = NSAA;
}

static Riscv64_ArgumentClass ClassifyIntFpScalar(PrimType ty) {
  switch (ty) {
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
      return kRiscv64IntegerClass;
    case PTY_f32:
    case PTY_f64:
      return kRiscv64FloatClass;
    default:
      return kRiscv64NoClass;
  }
}

// This function ONLY handles FP Calling Convention, under which a riscv64
// small struct can have at most 2 fields, it can reside in 2 fpregs or intreg+fpreg,
// as indicated by the referenced classes. Otherwise PTY_void is returned indicating
// the struct is to follow the subsequent Integer Calling Convention.
static PrimType TraverseStructFieldsForFp(MIRType *ty, uint32 &numregs, Riscv64_ArgumentClass classes[2], uint32 sizes[2]) {
  if (numregs >= 2) {
    return PTY_void;      // only 2 registers max
  }

  if (ty->typeKind == kTypeArray) {
    MIRArrayType *arrtype = static_cast<MIRArrayType *>(ty);
    MIRType *pty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(arrtype->eTyIdx);
    if (pty->typeKind == kTypeArray || pty->typeKind == kTypeStruct) {
      return TraverseStructFieldsForFp(pty, numregs, classes, sizes);
    }
    uint32 n = 0;         // real size of array
    for (uint32 i = 0; i < arrtype->dim; ++i) {
      n += arrtype->sizeArray[i];
    }
    if (n == 0 || n > 2) {
      return PTY_void;
    }
    Riscv64_ArgumentClass ptyp = ClassifyIntFpScalar(pty->primType);
    uint32 psz = GetPrimTypeSize(pty->primType);
    if (ptyp != kRiscv64FloatClass) {
      return PTY_void;
    }
    for (uint32 i = 0; i < n; i++) {
      classes[i] = ptyp;
      sizes[i] = psz;
    }
    numregs = n;     // number of regs used
    return pty->primType;
  } else if (ty->typeKind == kTypeStruct) {
    MIRStructType *sttype = static_cast<MIRStructType *>(ty);
    FieldVector fields = sttype->fields;
    PrimType ptype = PTY_void;
    for (uint32 fcnt = 0; fcnt < fields.size(); ++fcnt) {
      TyIdx fieldtyidx = fields[fcnt].second.first;
      MIRType *fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);
      ptype = TraverseStructFieldsForFp(fieldty, numregs, classes, sizes);
      if (ptype == PTY_void) {
        return PTY_void;
      }
    }
    return ptype;  // doesn't matter which one, as long as it's not PTY_void
  } else if (ty->typeKind == kTypeUnion) {
    return PTY_void;
  } else {
    classes[numregs] = ClassifyIntFpScalar(ty->primType);
    sizes[numregs] = GetPrimTypeSize(ty->primType);
    numregs++;
    return ty->primType;
  }
}

static int32 ClassifyFpCCAggregate(BECommon &be, MIRType *ty, Riscv64_ArgumentClass classes[2], uint32 sizes[2]) {
  uint32 sizeofty = be.type_size_table.at(ty->tyIdx.GetIdx());
  // to be handled by intCC
  if (sizeofty > 16 || sizeofty == 0) {
    return 0;
  }
  if (ty->typeKind == kTypeStruct) {
    MIRStructType *sty = static_cast<MIRStructType *>(ty);
    FieldVector fields = sty->fields;
    uint32 numregs = 0;
    if (fields.size() > 2) {
      return 0;
    }
    for (uint32 fcnt = 0; fcnt < fields.size(); ++fcnt) {
      TyIdx fieldtyidx = fields[fcnt].second.first;
      MIRType *fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);
      PrimType ptype = TraverseStructFieldsForFp(fieldty, numregs, classes, sizes);
      if (ptype == PTY_void) {
        return 0;
      }
    }
    if (numregs == 1 && classes[0] == kRiscv64FloatClass) {
      return 1;
    } else if (numregs == 2 &&
      ((classes[0] == kRiscv64FloatClass && classes[0] == classes[1]) ||
       (classes[0] == kRiscv64FloatClass && classes[1] == kRiscv64IntegerClass) ||
       (classes[0] == kRiscv64IntegerClass && classes[1] == kRiscv64FloatClass))) {
      return 2;
    } else {
      classes[0] = kRiscv64NoClass;
      classes[1] = kRiscv64NoClass;
      return 0;
    }
  }
  return 0;
}

// return 0 -- to be passed on stack
// return 1 -- one intreg
// return 2 -- either 2 intregs or
//             1 intreg (kRiscv64IntegerClass) + 1 stack (kRiscv64MemoryClass)
static int32 ClassifyIntCCAggregate(BECommon &be, MIRType *ty, Riscv64_ArgumentClass classes[2]) {
  uint32 sizeofty = be.type_size_table.at(ty->tyIdx.GetIdx());
  if (sizeofty > 16 || sizeofty == 0) {
    return 0;
  }

  // An argument of any Integer class takes up an integer register
  // which is a single double-word.
  int32 sizeoftyInDwords = RoundUp(sizeofty, 8) >> 3;
  CG_ASSERT(sizeoftyInDwords == 1 || sizeoftyInDwords == 2, "");
  classes[0] = kRiscv64NoClass;
  classes[1] = kRiscv64NoClass;

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
        classes[0] = kRiscv64IntegerClass;
        return 1;
      case PTY_f32:
      case PTY_f64:
        CG_ASSERT(0, "FP argument not possible here");
      default:
        CG_ASSERT(false, "");
    }
    // should not reach to this point
    return 0;
  } else {
    // struct, union and array
    classes[0] = kRiscv64IntegerClass;
    if (sizeoftyInDwords == 2) {
      classes[1] = kRiscv64IntegerClass;
    }
    return sizeoftyInDwords;
  }
  return 0;
}

int32 ParmLocator::LocateRetValue(MIRType *retty, PLocInfo &ploc) {
  InitPlocInfo(ploc);
  uint32 retsz = _be.type_size_table[retty->tyIdx.GetIdx()];
  if (retsz == 0) {
    // For return struct size 0 there is no return value.
    return 0;
  } else if (retsz <= 16) {
    // For return struct size less or equal to 16 bytes, the values
    // are returned in register pairs.
    Riscv64_ArgumentClass classes[2]; // Max of four floats.
    uint32 sizes[2];
    int32 numregs = ClassifyFpCCAggregate(_be, retty, classes, sizes);
    ploc.memsize = retsz;
    if (numregs) {
      if (classes[0] == kRiscv64FloatClass) {
        ploc.reg0 = AllocateReturnFPRegister();
      } else {
        ploc.reg0 = AllocateReturnGPRegister();
      }
      ploc.rsize0 = sizes[0];
      if (numregs == 2) {
        if (classes[1] == kRiscv64FloatClass) {
          ploc.reg1 = AllocateReturnFPRegister();
        } else {
          ploc.reg1 = AllocateReturnGPRegister();
        }
        ploc.rsize1 = sizes[1];
      }
    } else { // use IntCC
      numregs = ClassifyIntCCAggregate(_be, retty, classes);
      CG_ASSERT(numregs, "Invalid numregs from IntCC");
      ploc.reg0 = AllocateReturnGPRegister();
      ploc.rsize0 = 8;
      if (numregs == 2) {
        ploc.reg1 = AllocateReturnGPRegister();
        ploc.rsize1 = (retsz > 8 && retsz <= 12) ? 4 : 8;
      }
    }
  } else {
    // For return struct size > 16 bytes the pointer returns in a0
    ploc.reg0 = AllocateGPRegister();
    ploc.rsize0 = 8;
    ploc.memsize = retsz;
  }
  return 0;
}

// LocateNextParm should be called with each parameter in the parameter list
// starting from the beginning, one call per parameter in sequence; it returns
// the information on how each parameter is passed in ploc
int32 ParmLocator::LocateNextParm(MIRType *ty, PLocInfo &ploc, bool isFirst, bool isVararg) {
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
        Riscv64_ArgumentClass classes[2]; // Max of four floats.
        MIRType *retty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(retidx);

        uint32 sizes[2];
        int32 numregs = ClassifyFpCCAggregate(_be, ty, classes, sizes);
        ploc.memsize = retsz;
        if (numregs) {
          if (classes[0] == kRiscv64FloatClass) {
            ploc.reg0 = AllocateReturnFPRegister();
          } else {
            ploc.reg0 = AllocateReturnGPRegister();
          }
          ploc.rsize0 = sizes[0];
          if (numregs == 2) {
            if (classes[1] == kRiscv64FloatClass) {
              ploc.reg1 = AllocateReturnFPRegister();
            } else {
              ploc.reg1 = AllocateReturnGPRegister();
            }
            ploc.rsize1 = sizes[1];
          }
        } else { // use IntCC
          numregs = ClassifyIntCCAggregate(_be, retty, classes);
          CG_ASSERT(numregs, "Invalid numregs from IntCC");
          ploc.reg0 = AllocateReturnGPRegister();
          ploc.rsize0 = 8;
          if (numregs == 2) {
            ploc.reg1 = AllocateReturnGPRegister();
            ploc.rsize1 = (retsz > 8 && retsz <= 12) ? 4 : 8;
          }
        }
      } else {
        // For return struct size > 16 bytes the pointer returns in a0
        ploc.reg0 = AllocateGPRegister();
        ploc.rsize0 = 8;
        ploc.memsize = retsz;
      }
      return 0;
    }
  }
  int typesize = _be.type_size_table.at(ty->tyIdx.GetIdx());
  if (typesize == 0) {
    return 0;
  }
  int typealign = _be.type_align_table[ty->tyIdx.GetIdx()];
  CG_ASSERT((NSAA & (std::max(typealign, 8) - 1)) == 0, "alignment requirement is violated");
  ploc.memsize = typesize;
  _parm_num++;

  // Do Floating Point Calling Convention first
  int32 aggCopySize = 0;
  PrimType pt = isVararg ? PTY_void : ty->GetPrimType(); // intCC only for vararg
  switch (pt) {
    case PTY_f32:
    case PTY_f64:
      CG_ASSERT(GetPrimTypeSize(PTY_f64) == 8, "");
      typesize = 8;  // 8 bytes
      ploc.reg0 = AllocateSIMDFPRegister();
      if (ploc.reg0 == kRinvalid) {
        break;       // no register, continue with IntCC
      }
      ploc.rsize0 = GetPrimTypeSize(ty->GetPrimType());
      return 0;
      break;

    case PTY_c64:
      CG_ASSERT(0, "Complex type not supported");
      break;

    case PTY_agg: {
      Riscv64_ArgumentClass classes[2]; // Max of 2 values, can be int, fp
                                        // or (invalid) redo it in intCC below

      if (ty->typeKind == kTypeUnion)
        break;    // move to intCC below

      ploc.memsize = typesize;
      if (typesize > 16) {
        aggCopySize = RoundUp(typesize, SIZEOFPTR);
      }
      if (typealign == 16) {
        RoundNGRNUpToNextEven();
      }

      uint32 sizes[2];
      int32 numregs = ClassifyFpCCAggregate(_be, ty, classes, sizes);
      CHECK_FATAL(numregs <= 2, "LocateNextParm: illegal number of regs");
      if (numregs == 1 ) {
        if (classes[0] == kRiscv64FloatClass) {
          AllocateNSIMDFPRegisters(ploc, numregs);
        } else if (classes[0] == kRiscv64IntegerClass) {
          ploc.reg0 = AllocateGPRegister();
        } else {
          CHECK_FATAL(0, "LocateNextParam: illegal class returned");
        }
        if (ploc.reg0 == kRinvalid) {
          break;    // no fpreg, continue with IntCC
        }
        ploc.rsize0 = sizes[0];
        return 0;   // 0 aggCopySize, in reg
      } else if (numregs == 2) {
        CHECK_FATAL(!(classes[0] == kRiscv64IntegerClass && classes[0] == classes[1]),
                    "FpCC must not have 2 intregs");
        if (classes[0] == kRiscv64FloatClass) {
          ploc.reg0 = AllocateSIMDFPRegister();
        } else if (classes[0] == kRiscv64IntegerClass) {
          ploc.reg0 = AllocateGPRegister();
        }
        if (ploc.reg0 == kRinvalid) {
          break;    // no 1st int/fpreg, continue with IntCC
        }
        if (classes[1] == kRiscv64FloatClass) {
          ploc.reg1 = AllocateSIMDFPRegister();
        } else if (classes[1] == kRiscv64IntegerClass) {
          ploc.reg1 = AllocateGPRegister();
        }
        if (ploc.reg1 == kRinvalid) {
          break;    // no 2nd int/fpreg, continue with IntCC
        }
        ploc.rsize0 = sizes[0];
        ploc.rsize1 = sizes[1];
        return 0;  // 0 aggCopySize, in regs
      }
    } // case PTY_agg

    default:
      break;    // continue in Integer Calling Convention
  }

  // Start Integer Calling Convention
  int32 numregs = 0;
  Riscv64reg_t tReg;
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
    case PTY_f32:
    case PTY_f64:
      CG_ASSERT(GetPrimTypeSize(PTY_i64) == 8, "");
      typesize = 8;  // 8 bytes
      ploc.reg0 = AllocateGPRegister();
      if (ploc.reg0 != kRinvalid) {
        ploc.rsize0 = GetPrimTypeSize(ty->GetPrimType());
      }
      CG_ASSERT(NGRN <= Riscv64Abi::kNumIntParmRegs, "");
      break;

    case PTY_agg: {
      Riscv64_ArgumentClass classes[2];
      ploc.memsize = typesize;
      if (typesize > 16) {
        aggCopySize = RoundUp(typesize, SIZEOFPTR);
      }

      numregs = ClassifyIntCCAggregate(_be, ty, classes);
      if (numregs == 1) {  // passing in registers
        typesize = 8;
        if (classes[0] == kRiscv64FloatClass) {
          CHECK_FATAL(0, "PTY_agg: param passing in FP reg not allowed.");
        } else {
          ploc.reg0 = AllocateGPRegister();
          if (ploc.reg0 != kRinvalid) {
            ploc.rsize0 = (typesize > 4) ? 8 : 4;
          }
          CG_ASSERT(ploc.reg0 != kRinvalid || NGRN <= Riscv64Abi::kNumIntParmRegs, "");
        }
      } else if (numregs == 2) {
        ploc.reg0 = AllocateGPRegister();
        if (ploc.reg0 != kRinvalid) {
          ploc.rsize0 = 8;
        }
        ploc.reg1 = AllocateGPRegister();
        if (ploc.reg1 != kRinvalid) {
          if (typesize > 12) { // 1st: 8, 2nd: 8
            ploc.rsize1 = 8;
          } else if (typesize > 8 && typesize <= 12) { // 1st: 8, 2nd: 4
            ploc.rsize1 = 4;
          }
        } else if (ploc.reg0 != kRinvalid) {
          ploc.memsize -= 8;   // 1st half in reg, 2nd half on stack
        }
      } else {
        // 0 returned from ClassifyIntCCAggregate(). This means the whole data
        // is passed thru memory, typesize > 16
        //
        // Try to allocate an intreg for addr
        typesize = 8;
        ploc.reg0 = AllocateGPRegister();
        ploc.memsize = 8;
        if (ploc.reg0 != kRinvalid) {
          ploc.rsize0 = 8;
          numregs = 1;
        }
      }
      // compute rightpad
      if (numregs == 0 || ploc.reg0 == kRinvalid ||
          (numregs == 2 && ploc.reg0 != kRinvalid && ploc.reg1 == kRinvalid)) {
        int32 paddSize = RoundUp(ploc.memsize, 8);
        typesize = paddSize;
      }
      break;
    } // case PTY_agg

    default:
      CG_ASSERT(false, "");
      break;
  } // intCC

  if (ploc.reg0 == kRinvalid ||
      (numregs == 2 && ploc.reg0 != kRinvalid && ploc.reg1 == kRinvalid)) {
    // being passed in memory
    NSAA = ploc.memoffset + typesize;
  }
  return aggCopySize;
}

void ParmLocator::LocateCallNodeParm(MIRType *ty, PLocInfo &ploc, uint32 retSize, bool isFirst, bool isVararg) {
  if (isFirst && retSize > 16) {
    InitPlocInfo(ploc);
    ploc.reg0 = AllocateGPRegister();
    return;
  } else {
    LocateNextParm(ty, ploc, isFirst && retSize > 16, isVararg);
    return;
  }
}

// instantiated with the type of the function return value, it describes how
// the return value is to be passed back to the caller
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
      reg0 = Riscv64Abi::int_return_regs[0];
      ptype0 = IsSignedInteger(primType) ? PTY_i32 : PTY_u32;  // promote the type
      return;

    case PTY_ptr:
    case PTY_ref:
      CHECK_FATAL(false, "PTY_ptr should have been lowered", "");

    case PTY_a64:
    case PTY_u64:
    case PTY_i64:
      regcount = 1;
      reg0 = Riscv64Abi::int_return_regs[0];
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
      reg0 = Riscv64Abi::float_return_regs[0];
      ptype0 = primType;
      return;

    /*
       for c128 complex numbers, we assume
        - callers marshall the two f64 numbers into one f128 register
        - callees de-marshall one f128 value into the real and the imaginery part
     */
    case PTY_c128:
      regcount = 1;
      reg0 = Riscv64Abi::float_return_regs[0];
      ptype0 = primType;
      return;

    // Otherwise, the caller shall reserve a block of memory of
    // sufficient size and alignment to hold the result. The
    // address of the memory block shall be passed as an additional
    // argument to the function in a0. The callee may modify the
    // result memory block at any point during the execution of the
    // subroutine (there is no requirement for the callee to preserve
    // the value stored in a0)."
    case PTY_agg: {
      uint64 size = be.type_size_table.at(retty->tyIdx.GetIdx());
      if (size > 16 || size == 0) {
        // The return value is returned via memory.
        // The address is in a0 and passed by the caller.
        SetupToReturnThroughMemory();
        return;
      }
      Riscv64_ArgumentClass classes[2];
      uint32 sizes[2];
      uint32 intRegCnt = 0;
      uint32 fpRegCnt = 0;
      regcount = ClassifyFpCCAggregate(be, retty, classes, sizes);
      ptype0 = ptype1 = PTY_i64;
      if (regcount) {
        if (classes[0] == kRiscv64FloatClass) {
          reg0 = Riscv64Abi::float_return_regs[fpRegCnt++];
          ptype0 = sizes[0] <= 4 ? PTY_f32 : PTY_f64;
        } else {
          reg0 = Riscv64Abi::int_return_regs[intRegCnt++];
        }
        if (regcount == 2) {
          if (classes[1] == kRiscv64FloatClass) {
            reg1 = Riscv64Abi::float_return_regs[fpRegCnt++];
            ptype1 = sizes[1] <= 4 ? PTY_f32 : PTY_f64;
          } else {
            reg1 = Riscv64Abi::int_return_regs[intRegCnt++];
          }
        }
      } else { // use IntCC
        regcount = ClassifyIntCCAggregate(be, retty, classes);
        if (regcount) {
          reg0 = Riscv64Abi::int_return_regs[0];
          if (regcount == 2) {
            reg1 = Riscv64Abi::int_return_regs[1];
          }
        } else {
          SetupToReturnThroughMemory();  // should never happen?
        }
      }
      return;
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
      reg1 = Riscv64Abi::int_return_regs[1];
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
