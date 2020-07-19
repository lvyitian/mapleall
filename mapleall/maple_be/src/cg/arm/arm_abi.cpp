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

#include "be_common.h"
#include "arm_abi.h"
#include "cg_assert.h"

namespace maplebe {

#define SIZEOFREGS 8

using namespace maple;
// return the class resulted from merging the two classes, based on rules
// described by the ARM ABI
static ARM_ArgumentClass MergeClasses(ARM_ArgumentClass class0, ARM_ArgumentClass class1) {
  if (class0 == class1) {
    return class0;
  }
  if (class0 == ARM_NoClass) {
    return class1;
  }
  if (class1 == ARM_NoClass) {
    return class0;
  }
  if (class0 == ARM_MemoryClass || class1 == ARM_MemoryClass) {
    return ARM_MemoryClass;
  }
  if (class0 == ARM_IntegerClass || class1 == ARM_IntegerClass) {
    return ARM_IntegerClass;
  }
  CG_ASSERT(false, "nyi");
  return ARM_NoClass;
}

// analyze the given aggregate using the rules given by the X86-64 ABI and
// return the number of doublewords to be passed in registers; the classes of
// the doublewords are returned in parameter "classes"; if 0 is returned, it
// means the whole aggregate is passed in memory
static int32 ClassifyAggregate(BECommon &be, MIRType *ty, ARM_ArgumentClass classes[2], size_t classesLength) {
  ASSERT(classesLength > 0,"invalid index");
  uint32 sizeofty = be.type_size_table[ty->_ty_idx.idx];
  if (sizeofty > 16 || sizeofty == 0) {
    return 0;
  }
  int32 sizeofty_in_dwords = RoundUp(sizeofty, 8) >> 3;
  int32 i;
  for (i = 0; i < sizeofty_in_dwords; i++) {
    classes[i] = ARM_NoClass;
  }
  if (ty->typeKind != kTypeStruct && ty->typeKind != kTypeArray) {
    switch (ty->GetPrimType()) {
      case PTY_u1:
      case PTY_u8:
      case PTY_i8:
      case PTY_u16:
      case PTY_i16:
      case PTY_a32:
      case PTY_ptr:
      case PTY_ref:
      case PTY_u32:
      case PTY_i32:
      case PTY_a64:
      case PTY_u64:
      case PTY_i64:
        classes[0] = ARM_IntegerClass;
        return 1;
      case PTY_f32:
      case PTY_c64:
      case PTY_f64:
        classes[0] = ARM_FloatClass;
        return 1;
      case PTY_c128:
        classes[0] = ARM_FloatClass;
        classes[1] = ARM_FloatClass;
        return 2;
      default:
        CG_ASSERT(false, "");
    }
    return 0;
  } else {
    ARM_ArgumentClass subclasses[2];
    int32 subnumregs;
    if (ty->typeKind == kTypeStruct) {
      MIRStructType *sty = static_cast<MIRStructType *>(ty);
      uint32 fld_bofst = 0;  // offset of field in bits within immediate struct
      uint64 alloced_size = 0;
      uint64 alloced_size_in_bits = 0;
      for (uint32 f = 0; f < sty->fields.size(); f++) {
        TyIdx fieldtyidx = sty->fields[f].second.first;
        MIRType *fieldty = globaltable.GetTypeFromTyIdx(fieldtyidx);
        subnumregs = ClassifyAggregate(be, fieldty, subclasses, sizeof(subclasses));
        if (subnumregs == 0) {
          return 0;
        }
        // determine fld_bofst for this field
        uint32 fieldsize = be.type_size_table[fieldty->_ty_idx.idx];
        CG_ASSERT(fieldsize != 0, "");
        uint8 fieldalign = be.type_align_table[fieldty->_ty_idx.idx];
        CG_ASSERT(fieldalign != 0, "");
        if (sty->typeKind != kTypeUnion) {
          if (fieldty->typeKind == kTypeBitField) {
            uint32 fldsize = static_cast<MIRBitfieldType *>(fieldty)->fieldSize;
            // is this field is crossing the align boundary of its base type?
            if (alloced_size_in_bits / (fieldalign * 8) != (alloced_size_in_bits + fldsize - 1) / (fieldalign * 8)) {
              // the field is crossing the align boundary of its base type;
              // align alloced_size_in_bits to fieldalign
              alloced_size_in_bits = RoundUp(alloced_size_in_bits, fieldalign * 8);
            }
            // allocate the bitfield
            fld_bofst = alloced_size_in_bits;
            alloced_size_in_bits += fldsize;
            alloced_size = std::max(alloced_size, RoundUp(alloced_size_in_bits, fieldalign * 8) / 8);
          } else {
            // pad alloced_size according to the field alignment
            alloced_size = RoundUp(alloced_size, fieldalign);
            fld_bofst = alloced_size * 8;
            alloced_size += fieldsize;
            alloced_size_in_bits = alloced_size * 8;
          }
        } else {  // for unions, bitfields are treated as non-bitfields
          fld_bofst = alloced_size * 8;
          alloced_size = std::max(alloced_size, static_cast<uint64>(fieldsize));
        }
        // merge subclasses into classes
        int32 indx = fld_bofst >> 6;  // index into the struct in doublewords
        for (i = 0; i < subnumregs; i++) {
          classes[i + indx] = MergeClasses(classes[i + indx], subclasses[i]);
        }
      }
      if (subnumregs < sizeofty_in_dwords) {
        for (i = 1; i < sizeofty_in_dwords; i++)
          if (classes[i] == ARM_NoClass) {
            classes[i] = classes[i - 1];
          }
      }
    } else {
      MIRArrayType *aty = static_cast<MIRArrayType *>(ty);
      subnumregs = ClassifyAggregate(be, globaltable.GetTypeFromTyIdx(aty->eTyIdx), subclasses, sizeof(subclasses));
      if (subnumregs == 0) {
        return 0;
      }
      for (i = 0; i < sizeofty_in_dwords; i++) {
        classes[i] = subclasses[i % subnumregs];
      }
    }
    // post merger clean-up
    for (i = 0; i < sizeofty_in_dwords; i++) {
      if (classes[i] == ARM_MemoryClass) {
        return 0;
      }
    }
    return sizeofty_in_dwords;
  }
}

// LocateNextParm should be called with each parameter in the parameter list
// starting from the beginning, one call per parameter in sequence; it returns
// the information on how each parameter is passed in ploc
void ParmLocator::LocateNextParm(MIRType *ty, PLocInfo &ploc) {
  ploc.reg0 = RCC;
  ploc.reg1 = RCC;
  ploc.memoffset = last_memoffset_;
  ploc.memsize = GetPrimTypeSize(ty->GetPrimType());

  uint32 rightpad = 0;
  parm_num_++;

  switch (ty->GetPrimType()) {
    case PTY_u1:
    case PTY_u8:
    case PTY_i8:
    case PTY_u16:
    case PTY_i16:
    case PTY_a32:
    case PTY_ptr:
    case PTY_ref:
    case PTY_u32:
    case PTY_i32:
      rightpad = GetPrimTypeSize(PTY_i64) - ploc.memsize;
      int_parm_num_++;
      if (int_parm_num_ % 2 != 0) {
        int64_parm_num_++;
      }
      if (int_parm_num_ <= ArmAbi::num_int_parm_regs) {
        ploc.reg0 = ArmAbi::int_parm_regs[int_parm_num_ - 1];
      }
      break;
    // fall through
    case PTY_a64:
    case PTY_u64:
    case PTY_i64:
      rightpad = GetPrimTypeSize(PTY_i64) - ploc.memsize;
      if (int_parm_num_ % 2 == 0) {
        int_parm_num_ += 2;
      } else {
        int_parm_num_ += 3;
      }
      int64_parm_num_++;
      if (int64_parm_num_ <= ArmAbi::num_int64_parm_regs) {
        ploc.reg0 = ArmAbi::int64_parm_regs[int64_parm_num_ - 1];
      }
      break;

    case PTY_f32:
      rightpad = GetPrimTypeSize(PTY_f64) - ploc.memsize;
      float_parm_num_++;
      if (float_parm_num_ % 2 != 0) {
        double_parm_num_++;  // overlap with double
      }
      if (float_parm_num_ <= ArmAbi::num_float_parm_regs) {
        ploc.reg0 = ArmAbi::float_parm_regs[float_parm_num_ - 1];
      }
      break;
    // fall through
    case PTY_c64:
    case PTY_f64:
      rightpad = GetPrimTypeSize(PTY_f64) - ploc.memsize;
      if (float_parm_num_ % 2 == 0) {
        float_parm_num_ += 2;
      } else {
        float_parm_num_ += 3;
      }
      double_parm_num_++;
      if (double_parm_num_ <= ArmAbi::num_float_parm_regs) {
        ploc.reg0 = ArmAbi::double_parm_regs[double_parm_num_ - 1];
      }
      break;

    case PTY_c128:
      float_parm_num_++;
      if (float_parm_num_ <= ArmAbi::num_float_parm_regs) {
        ploc.reg0 = ArmAbi::float_parm_regs[float_parm_num_ - 1];
        float_parm_num_++;
        if (float_parm_num_ <= ArmAbi::num_float_parm_regs) {
          ploc.reg1 = ArmAbi::float_parm_regs[float_parm_num_ - 1];
        } else {  // whole c128passed in memory
          float_parm_num_ -= 2;
          ploc.reg0 = RCC;
        }
      } else {
        float_parm_num_--;
      }
      break;

    case PTY_agg: {
      ARM_ArgumentClass classes[2];
      int32 saveint_parm_num_ = int_parm_num_;
      int32 savefloat_parm_num_ = float_parm_num_;
      ploc.memsize = be_.type_size_table[ty->_ty_idx.idx];
      int32 numregs = ClassifyAggregate(be_, ty, classes, sizeof(classes));
      if (numregs > 0) {  // passing in registers
        if (classes[0] == ARM_FloatClass) {
          float_parm_num_++;
          if (float_parm_num_ <= ArmAbi::num_float_parm_regs) {
            ploc.reg0 = ArmAbi::float_parm_regs[float_parm_num_ - 1];
          } else {  // pass in memory
            float_parm_num_ = savefloat_parm_num_;
            numregs = 0;
          }
        } else {
          int_parm_num_++;
          if (int_parm_num_ <= ArmAbi::num_int_parm_regs) {
            ploc.reg0 = ArmAbi::int_parm_regs[int_parm_num_ - 1];
          } else {  // pass in memory
            int_parm_num_ = saveint_parm_num_;
            numregs = 0;
          }
        }
        if (numregs > 1) {
          if (classes[1] == ARM_FloatClass) {
            float_parm_num_++;
            if (float_parm_num_ <= ArmAbi::num_float_parm_regs) {
              ploc.reg1 = ArmAbi::float_parm_regs[float_parm_num_ - 1];
            } else {  // pass in memory
              float_parm_num_ = savefloat_parm_num_;
              int_parm_num_ = saveint_parm_num_;
              ploc.reg0 = RCC;
              numregs = 0;
            }
          } else if (classes[1] == ARM_IntegerClass) {
            int_parm_num_++;
            if (int_parm_num_ <= ArmAbi::num_int_parm_regs) {
              ploc.reg1 = ArmAbi::int_parm_regs[int_parm_num_ - 1];
            } else {  // pass in memory
              float_parm_num_ = savefloat_parm_num_;
              int_parm_num_ = saveint_parm_num_;
              ploc.reg0 = RCC;
              numregs = 0;
            }
          }
        }
      }
      // compute rightpad
      if (numregs == 0 || ploc.reg0 == 0) {  // passed in memory
        int32 padded_size = RoundUp(ploc.memsize, 8);
        rightpad = padded_size - ploc.memsize;
      }
      break;
    }
    default:
      CG_ASSERT(false, "");
  }

  if (ploc.reg0 == 0) {  // being passed in memory
    last_memoffset_ = ploc.memoffset + ploc.memsize + rightpad;
  }
  return;
}

// instantiated with the type of the function return value, it describes how
// the return value is to be passed back to the caller
ReturnMechanism::ReturnMechanism(MIRType *retty, BECommon &be) : regcount(0), fake_first_parm(false) {
  PrimType primType = retty->GetPrimType();
  switch (primType) {
    case PTY_u1:
    case PTY_u8:
    case PTY_i8:
    case PTY_u16:
    case PTY_i16:
    case PTY_a32:
    case PTY_ptr:
    case PTY_ref:
    case PTY_u32:
    case PTY_i32:
      regcount = 1;
      ptype0 = IsSignedInteger(primType) ? PTY_i32 : PTY_u32;  // promote the type
      reg0 = ArmAbi::int_return_regs[0];
      reg1 = ArmAbi::int_return_regs[1];
      return;
    case PTY_a64:
    case PTY_u64:
    case PTY_i64:
      regcount = 1;
      ptype0 = retty->GetPrimType();
      reg0 = ArmAbi::int64_return_regs[0];
      reg1 = ArmAbi::int64_return_regs[1];
      return;

    case PTY_f32:
      regcount = 1;
      ptype0 = retty->GetPrimType();
      reg0 = ArmAbi::float_return_regs[0];
      reg1 = ArmAbi::float_return_regs[1];
      return;
    case PTY_f64:
      regcount = 1;
      ptype0 = retty->GetPrimType();
      reg0 = ArmAbi::float_return_regs[0];
      reg1 = ArmAbi::float_return_regs[1];
      return;

    case PTY_c64:
      regcount = 2;
      ptype0 = PTY_f32;
      ptype1 = PTY_f32;
      reg0 = ArmAbi::float_return_regs[0];
      reg1 = ArmAbi::float_return_regs[1];
      return;

    case PTY_c128:
      regcount = 2;
      ptype0 = PTY_f64;
      ptype1 = PTY_f64;
      reg0 = ArmAbi::float_return_regs[0];
      reg1 = ArmAbi::float_return_regs[1];
      return;

    case PTY_agg: {
      uint64 size = be.type_size_table[retty->_ty_idx.idx];
      if (size == 0) {
        return;
      }
      if (size > SIZEOFREGS * 2) {
        fake_first_parm = true;
        return;
      }
      ARM_ArgumentClass classes[2];
      regcount = ClassifyAggregate(be, retty, classes, sizeof(classes));
      if (regcount == 0) {
        fake_first_parm = true;
        return;
      } else {
        Armregister next_int_return_reg = ArmAbi::int_return_regs[0];
        Armregister next_float_return_reg = ArmAbi::float_return_regs[0];
        if (classes[0] == ARM_IntegerClass) {
          reg0 = ArmAbi::int_return_regs[0];
          ptype0 = PTY_i64;
          next_int_return_reg = ArmAbi::int_return_regs[0];
        } else {
          reg0 = ArmAbi::float_return_regs[0];
          ptype0 = PTY_f64;
          next_float_return_reg = ArmAbi::float_return_regs[1];
        }
        if (regcount > 1) {
          if (classes[0] == ARM_IntegerClass) {
            reg1 = next_int_return_reg;
            ptype0 = PTY_i64;
          } else {
            reg1 = next_float_return_reg;
            ptype0 = PTY_f64;
          }
        }
        return;
      }
    }

    default:
      return;
  }
}

}  // namespace maplebe
