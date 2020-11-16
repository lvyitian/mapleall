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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64ISA_H
#define MAPLEBE_INCLUDE_CG_AARCH64ISA_H

#include "isa.h"
#include "insn.h"
#include "operand.h"
#include "mad.h"

#define DEFINE_MOP(op,a2,a3,a4,a5,a6,a7,a8,a9,aa,ab,ac) op,
enum Riscv64MOP_t {
#include "riscv64_md.def"
  kMopLast
};
#undef DEFINE_MOP

namespace maplebe {

/*
   ARM Architecture Reference Manual (for ARMv8)
   D1.8.2
 */
#define RISCV64_STACK_PTR_ALIGNMENT 16

static const int kIntregBytelen = 8;   /* 64-bit */
static const int kFpregBytelen = 8;    /* only lower 64 bits are used */
static const int kSimdregBytelen = 16; /* 128-bit */

#define STP_LDP_IMM64_LOWER_BOUND -2048
#define STP_LDP_IMM64_UPPER_BOUND 2048
#define STP_LDP_IMM32_LOWER_BOUND -2048
#define STP_LDP_IMM32_UPPER_BOUND 2048

#define STR_LDR_PRE_POST_LOWER_BOUND -256
#define STR_LDR_PRE_POST_UPPER_BOUND 255
#define STRALL_LDRALL_IMM_LOWER_BOUND 0
#define STR_LDR_IMM32_UPPER_BOUND 2040 /* must be a multiple of 4 */
#define STR_LDR_IMM64_UPPER_BOUND 2040 /* must be a multiple of 8 */
#define STRB_LDRB_IMM_UPPER_BOUND 4095
#define STRH_LDRH_IMM_UPPER_BOUND 8190

/*
   Riscv64 Condition Code Suffixes
 */
enum Riscv64CC_t {
#define CONDCODE(a) CC_##a,
#include "riscv64_cc.def"
#undef CONDCODE
  kCcLast
};

/*
   ARM Compiler armasm User Guide version 6.6.
   http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0473j/deb1353594352617.html
   (retrieved on 3/24/2017)

   $ 4.1 Registers in Riscv64 state
   ...When you use the 32-bit form of an instruction, the upper
   32 bits of the source registers are ignored and
   the upper 32 bits of the destination register are set to zero.
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   There is no register named W31 or X31.
   Depending on the instruction, register 31 is either the stack
   pointer or the zero register. When used as the stack pointer,
   you refer to it as "SP". When used as the zero register, you refer
   to it as WZR in a 32-bit context or XZR in a 64-bit context.
   The zero register returns 0 when read and discards data when
   written (e.g., when setting the status register for testing).
 */
enum Riscv64reg_t {
  kRinvalid = INVALID_REGNO,
// integer registers
#define INT_REG(ID, PREF32, PREF64) R##ID,
#define INT_REG_ALIAS(ALIAS, ID, PREF32, PREF64)
#include "riscv64_int_regs.def"
#undef INT_REG
#undef INT_REG_ALIAS
// fp-simd registers
#define FP_SIMD_REG(ID, PV, P8, P16, P32, P64, P128) V##ID,
#define FP_SIMD_EXT_BASE(ID) VB##ID,
#define FP_SIMD_EXT(ID, PV, P8, P16, P32, P64, P128) V##ID,
#define FP_SIMD_REG_ALIAS(ID)
#include "riscv64_fp_simd_regs.def"
#undef FP_SIMD_REG
#undef FP_SIMD_EXT_BASE
#undef FP_SIMD_EXT
#undef FP_SIMD_REG_ALIAS
  kMaxRegNum,
// alias
#define INT_REG(ID, PREF32, PREF64)
#define INT_REG_ALIAS(ALIAS, ID, PREF32, PREF64) R##ALIAS = R##ID,
#include "riscv64_int_regs.def"
#undef INT_REG
#undef INT_REG_ALIAS
#define FP_SIMD_REG(ID, PV, P8, P16, P32, P64, P128)
#define FP_SIMD_EXT_BASE(ID)
#define FP_SIMD_EXT(ID, PV, P8, P16, P32, P64, P128)
#define FP_SIMD_REG_ALIAS(ID) S##ID = V##ID,
#include "riscv64_fp_simd_regs.def"
#undef FP_SIMD_REG
#undef FP_SIMD_EXT_BASE
#undef FP_SIMD_EXT
#undef FP_SIMD_REG_ALIAS
#define FP_SIMD_REG(ID, PV, P8, P16, P32, P64, P128)
#define FP_SIMD_EXT_BASE(ID)
#define FP_SIMD_EXT(ID, PV, P8, P16, P32, P64, P128)
#define FP_SIMD_REG_ALIAS(ID) D##ID = V##ID,
#include "riscv64_fp_simd_regs.def"
#undef FP_SIMD_REG
#undef FP_SIMD_EXT_BASE
#undef FP_SIMD_EXT
#undef FP_SIMD_REG_ALIAS
};

namespace Riscv64isa {

static inline bool IsGPRegister(Riscv64reg_t r) {
  return (R0 <= r && r <= R31);
}

static inline bool IsFPSIMDRegister(Riscv64reg_t r) {
  return (V0 <= r && r <= V31);
}

static inline bool IsPhysicalRegister(regno_t r) {
  return r < kMaxRegNum;
}

static inline bool IsArgumentPassingRegister(regno_t r) {
  return (R0 <= r && r <= R7) || (V0 <= r && r <= V7);
}

static inline RegType GetRegType(Riscv64reg_t r) {
  if (IsGPRegister(r)) {
    return kRegTyInt;
  } else if (IsFPSIMDRegister(r)) {
    return kRegTyFloat;
  } else {
    CG_ASSERT(0, "No suitable register type to return?");
    return kRegTyUndef;
  }
}

enum memory_ordering_t : uint32_t {
  kMoNone = 0,
  kMoAcquire = (1UL << 0),      // ARMv8
  kMoAcquireRcpc = (1UL << 1),  // ARMv8.3
  kMoLoacquire = (1UL << 2),    // ARMv8.1
  kMoRelease = (1UL << 3),      // ARMv8
  kMoLorelease = (1UL << 4)     // ARMv8.1
};

}  // namespace Riscv64isa

#define MAXREG kMaxRegNum

#define REGPROPUNDEF 0
#define REGPROPDEF 0x1
#define REGPROPUSE 0x2
#define REGHIGH 0x4
#define REGLOW 0x8
#define MEMLOW12 0x10
#define LITERAL_LOW12 MEMLOW12
#define PREINC 0x20
#define POSTINC 0x40
#define LOADLITERAL 0x80

enum SubRegType{
 kSubRegTyUndef,
 kSubRegTyInt32,
 kSubRegTyInt64,
 kSubRegTyFloat32,
 kSubRegTyFloat64,
};

struct RegProp {
  RegType regtype_ : 8;
  SubRegType subRegType : 8;
  Riscv64reg_t physical_reg_;
  uint32 def_use_;  // used for register use/define and other properties of other operand
  RegProp(RegType t, SubRegType subt, Riscv64reg_t r, uint32 d) : regtype_(t),
      subRegType(subt), physical_reg_(r), def_use_(d) {}
};

struct Riscv64OpndProp : public OpndProp {
  Operand::OperandType opnd_ty_;
  RegProp regprop_;
  uint8 size_;

  Riscv64OpndProp(Operand::OperandType t, RegProp p, uint8 s) : opnd_ty_(t), regprop_(p), size_(s) {}

  bool IsPhysicalRegister() {
    return opnd_ty_ == Operand::Opd_Register && regprop_.physical_reg_ < kMaxRegNum;
  }

  bool IsRegister() {
    return opnd_ty_ == Operand::Opd_Register;
  }

  bool IsRegDef() {
    return opnd_ty_ == Operand::Opd_Register && (regprop_.def_use_ & REGPROPDEF);
  }

  bool IsRegUse() {
    return opnd_ty_ == Operand::Opd_Register && (regprop_.def_use_ & REGPROPUSE);
  }

  bool IsRegHigh() {
    return opnd_ty_ == Operand::Opd_Register && (regprop_.def_use_ & REGHIGH);
  }

  bool IsRegLow() {
    return opnd_ty_ == Operand::Opd_Register && (regprop_.def_use_ & REGLOW);
  }

  bool IsMemLow12() {
    return opnd_ty_ == Operand::Opd_Mem && (regprop_.def_use_ & MEMLOW12);
  }

  bool IsLiteralLow12() {
    return opnd_ty_ == Operand::Opd_StImmediate && (regprop_.def_use_ & LITERAL_LOW12);
  }

  bool IsMemPreIndex() {
    return opnd_ty_ == Operand::Opd_Mem && (regprop_.def_use_ & PREINC);
  }

  bool IsMemPostIndex() {
    return opnd_ty_ == Operand::Opd_Mem && (regprop_.def_use_ & POSTINC);
  }

  bool IsDef() {
    return regprop_.def_use_ & REGPROPDEF;
  }

  bool IsUse() {
    return regprop_.def_use_ & REGPROPUSE;
  }

  bool IsLoadLiteral() {
    return (regprop_.def_use_ & LOADLITERAL);
  }

  uint32 GetOperandSize() {
    return static_cast<uint32>(size_);
  }
};

struct Riscv64MD {
  MOperator opc_;
  OpndProp *operand_[5];
  uint64 properties_;
  LatencyType latencyType;
  const char *name_;
  const char *format_;
  int32_t resnum;
  int32_t opndnum;

  bool UseSpecReg() const {
    return properties_ & USESPECREG;
  }

  bool IsCall() const {
    return properties_ & ISCALL;
  }

  bool CanThrow() const {
    return properties_ & CANTHROW;
  }

  inline Riscv64OpndProp *GetOperand(int nth) const {
    CG_ASSERT(nth < 5, "");
    return static_cast<Riscv64OpndProp *>(operand_[nth]);
  }

  inline uint32 GetOperandSize() const {
    if (properties_ & (ISLOAD | ISSTORE))
    // use memory operand
    {
      return (GetOperand(1)->GetOperandSize());
    } else
    // use dest operand
    {
      return (GetOperand(0)->GetOperandSize());
    }
  }

  inline bool Is64Bit() const {
    return (GetOperandSize() == 64);
  }

  inline bool IsVector() const {
    return (GetOperandSize() == 128);
  }

  inline bool IsVolatile() const {
    return (((properties_ & HASRELEASE) != 0) || ((properties_ & HASACQUIRE) != 0));
  }

  inline bool IsMemAccessBar() const {
    return ((properties_ & (HASRELEASE | HASACQUIRE | HASACQUIRERCPC | HASLOACQUIRE | HASLORELEASE)) != 0);
  }

  inline bool IsMemAccess() const {
    return ((properties_ & (ISLOAD | ISSTORE | ISLOADPAIR | ISSTOREPAIR)) != 0);
  }

  inline bool IsBranch() const {
    return ((properties_ & (ISBRANCH)) != 0);
  }

  inline bool IsMove() const {
    return ((properties_ & (ISMOVE)) != 0);
  }

  inline bool IsDMB() const {
    return ((properties_ & (ISDMB)) != 0);
  }

  inline bool IsLoad() const {
    return ((properties_ & (ISLOAD)) != 0);
  }

  inline bool IsStore() const {
    return ((properties_ & (ISSTORE)) != 0);
  }

  inline bool IsLoadPair() const {
    return ((properties_ & (ISLOADPAIR)) != 0);
  }

  inline bool IsStorePair() const {
    return ((properties_ & (ISSTOREPAIR)) != 0);
  }

  inline bool IsLoadStorePair() const {
    return ((properties_ & (ISLOADPAIR | ISSTOREPAIR)) != 0);
  }

  inline bool IsLoadAddress() const {
    return ((properties_ & (ISLOADADDR)) != 0);
  }

  inline bool IsAtomic() const {
    return ((properties_ & ISATOMIC) != 0);
  }

  inline bool IsCondDef() const {
    return (properties_ & ISCONDDEF);
  }

  inline bool IsPartDef() const {
    return (properties_ & ISPARTDEF);
  }

  inline int32_t GetResultNum() {
    return resnum;
  }

  inline int32_t GetOpndsNum() {
    return opndnum;
  }

  LatencyType GetLatencyType() const {
    return latencyType;
  }
};

/*
   We save callee-saved registers from lower stack area to upper stack area.
   If possible, we store a pair of registers (int/int and fp/fp) in the stack.
   The Stack Pointer has to be aligned at 16-byte boundary.
   On Riscv64, kIntregBytelen == 8 (see the above)
 */
static inline void GetNextOffsetCalleeSavedPair(int &offset) {
  offset += kIntregBytelen * 2;
}

static inline void GetNextOffsetCalleeSaved(int &offset) {
  offset += kIntregBytelen;
}

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_AARCH64ISA_H
