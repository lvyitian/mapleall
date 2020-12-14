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

#include "riscv64_cg_func.h"
#include "riscv64_cg.h"
#include "cg_assert.h"
#include "mir_builder.h"

#include <iostream>

namespace maplebe {
using namespace maple;
using namespace std;

extern bool doItQuietly;

static MOperator ldis[2][4] = {
  /* unsigned == 0 */
  { MOP_wldrb, MOP_wldrh, MOP_wldr, MOP_xldr },
  /* signed == 1 */
  { MOP_wldrsb, MOP_wldrsh, MOP_wldrsw, MOP_xldr }
};
static MOperator stis[2][4] = {
  /* unsigned == 0 */
  { MOP_wstrb, MOP_wstrh, MOP_wstr, MOP_xstr },
  /* signed == 1 */
  { MOP_wstrb, MOP_wstrh, MOP_wstr, MOP_xstr }
};

static MOperator stIsRel[2][4] = {
  /* unsigned == 0 */
  { MOP_undef, MOP_undef, MOP_wstlr, MOP_xstlr },
  /* signed == 1 */
  { MOP_undef, MOP_undef, MOP_wstlr, MOP_xstlr }
};

static MOperator ldfs[2] = { MOP_sldr, MOP_dldr };
static MOperator stfs[2] = { MOP_sstr, MOP_dstr };

static MOperator ldFsAcq[2] = { MOP_undef, MOP_undef };
static MOperator stFsRel[2] = { MOP_undef, MOP_undef };

template <bool is_load>
MOperator PickLdStInsn(uint32 bitsize, PrimType rtype, Riscv64isa::memory_ordering_t mo) {
  CG_ASSERT(__builtin_popcount((unsigned int)mo) <= 1, "");
  CG_ASSERT(rtype != PTY_ptr && rtype != PTY_ref, "should have been lowered");
  CG_ASSERT(bitsize >= 8 && __builtin_popcount(bitsize) == 1, "PTY_u1 should have been lowered?");
  CG_ASSERT(
    (is_load && (mo == Riscv64isa::kMoNone || mo == Riscv64isa::kMoAcquire || mo == Riscv64isa::kMoAcquireRcpc ||
                 mo == Riscv64isa::kMoLoacquire)) ||
      (!is_load && (mo == Riscv64isa::kMoNone || mo == Riscv64isa::kMoRelease || mo == Riscv64isa::kMoLorelease)),
    "");

  /* __builtin_ffs(x) returns: 0 -> 0, 1 -> 1, 2 -> 2, 4 -> 3, 8 -> 4 */
  // int mo_idx = __builtin_ffs( (int)mo );

  if (IsPrimitiveInteger(rtype)) {
    MOperator(*table)[4];
    if (is_load) {
      table = mo == Riscv64isa::kMoAcquire ? ldis : ldis;  // need to support acquire
    } else {
      table = mo == Riscv64isa::kMoRelease ? stIsRel : stis;
    }

    int signedUnsigned = IsUnsignedInteger(rtype) ? 0 : 1;
    /* __builtin_ffs(x) returns: 8 -> 4, 16 -> 5, 32 -> 6, 64 -> 7 */
    unsigned int size = static_cast<unsigned int>(__builtin_ffs(static_cast<int>(bitsize))) - 4;
    CG_ASSERT(size <= 3, "");
    return table[signedUnsigned][size];
  } else if (IsPrimitiveVector(rtype)) {
    return is_load ? MOP_vldr : MOP_vstr;
  } else {
    MOperator *table = nullptr;
    if (is_load) {
      table = mo == Riscv64isa::kMoAcquire ? ldFsAcq : ldfs;
    } else {
      table = mo == Riscv64isa::kMoRelease ? stFsRel : stfs;
    }

    /* __builtin_ffs(x) returns: 32 -> 6, 64 -> 7 */
    unsigned int size = static_cast<unsigned int>(__builtin_ffs(static_cast<int>(bitsize))) - 6;
    CG_ASSERT(size <= 1, "");
    return table[size];
  }
}

MOperator Riscv64CGFunc::PickVecDup(PrimType sPtyp) {
  switch (sPtyp) {
    case PTY_i32:
      return MOP_vdupi32;
    case PTY_i64:
      return MOP_vdupi64;
    case PTY_f32:
      return MOP_vdupf32;
    case PTY_f64:
      return MOP_vdupf64;
    default:
      CG_ASSERT(false, "NYI");
      return MOP_undef;
  }
}

MOperator Riscv64CGFunc::PickLdInsn(uint32 bitsize, PrimType rtype, Riscv64isa::memory_ordering_t mo) {
  return PickLdStInsn<true>(bitsize, rtype, mo);
}

MOperator Riscv64CGFunc::PickStInsn(uint32 bitsize, PrimType rtype, Riscv64isa::memory_ordering_t mo) {
  return PickLdStInsn<false>(bitsize, rtype, mo);
}

MOperator Riscv64CGFunc::PickMovInsn(PrimType primtype) {
  switch (primtype) {
    case PTY_u8:
    case PTY_u16:
    case PTY_u32:
    case PTY_i8:
    case PTY_i16:
    case PTY_i32:
      return MOP_wmovrr;
    case PTY_a32:
      CG_ASSERT(0, "Invalid primitive type for Riscv64");
      return MOP_undef;
    case PTY_ptr:
    case PTY_ref:
      CG_ASSERT(0, "PTY_ref and PTY_ptr should have been lowered");
      return MOP_undef;
    case PTY_a64:
    case PTY_u64:
    case PTY_i64:
      return MOP_xmovrr;
    case PTY_f32:
      return MOP_xvmovs;
    case PTY_f64:
      return MOP_xvmovd;
    case PTY_v2i64:
    case PTY_v4i32:
    case PTY_v8i16:
    case PTY_v16i8:
    case PTY_v2f64:
    case PTY_v4f32:
      return MOP_vmovrr;
    default:
      CG_ASSERT(0, "NYI PickMovInsn");
      return MOP_undef;
  }
}

MOperator Riscv64CGFunc::PickMovInsn(RegOperand *lhs, RegOperand *rhs) {
  //CG_ASSERT(lhs->GetSize() == rhs->GetSize(), "PickMovInsn: unequal size NYI");
  if (lhs->GetRegisterType() == kRegTyInt) {
    if (rhs->GetRegisterType() == kRegTyInt) {
      return ((lhs->GetSize() == 64) ? MOP_xmovrr : MOP_xmovrr);
    } else {
      return (rhs->GetSize() == 64) ? MOP_xvmovrd : MOP_xvmovrs;
    }
  } else if (lhs->GetRegisterType() == kRegTyFloat) {
    if (rhs->GetRegisterType() == kRegTyFloat) {
      return (lhs->GetSize() == 64) ? MOP_xvmovd : MOP_xvmovs;
    } else {
      return (lhs->GetSize() == 64) ? MOP_xvmovdr : MOP_xvmovsr;
    }
  }
  CG_ASSERT(false, "PickMovInsn: kind NYI");
  return 0;
}

MOperator Riscv64CGFunc::PickMovInsn(uint32_t bitlen, RegType rtype) {
  CG_ASSERT(bitlen == 32 || bitlen == 64, "");
  CG_ASSERT(rtype == kRegTyInt || rtype == kRegTyFloat, "");
  if (rtype == kRegTyInt) {
    return ((bitlen == 32) ? static_cast<uint32>(MOP_wmovrr) : static_cast<uint32>(MOP_xmovrr));
  } else {
    return ((bitlen == 32) ? static_cast<uint32>(MOP_xvmovs) : static_cast<uint32>(MOP_xvmovd));
  }
}

void Riscv64CGFunc::SelectLoadAcquire(Operand *dest, PrimType dtype, Operand *src, PrimType stype,
                                      Riscv64isa::memory_ordering_t mo, bool isDirect) {
  CG_ASSERT(src->op_kind_ == Operand::Opd_Mem, "Just checking");
  CG_ASSERT(mo != Riscv64isa::kMoNone, "Just checking");

  Insn *insn = nullptr;
  uint32 ssize = isDirect ? src->GetSize() : GetPrimTypeBitSize(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  MOperator mop = PickLdInsn(ssize, stype, mo);

  Operand *newSrc = src;
  Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(src);
  Riscv64OfstOperand *immopnd = memopnd->GetOffsetImmediate();
  int32 offset = immopnd->GetOffsetValue();
  RegOperand *origBasereg = memopnd->GetBaseRegister();
  if (offset != 0) {
    RegOperand *resopnd = CreateRegisterOperandOfType(PTY_i64);
    SelectAdd(resopnd, origBasereg, immopnd, PTY_i64);
    newSrc = CreateReplacementMemOperand(memopnd, ssize, resopnd, 0);
  }

  std::string key;
  if (isDirect && cg->GenerateVerboseAsm()) {
    MIRSymbol *sym = static_cast<Riscv64MemOperand *>(src)->GetSymbol();
    if (sym) {
      MIRStorageClass sc = sym->GetStorageClass();
      if (sc == kScFormal) {
        key = "param: ";
      } else if (sc == kScAuto) {
        key = "local var: ";
      } else {
        key = "global: ";
      }
      key.append(sym->GetName());
    }
  }

  /* Check if the right load-acquire instruction is available. */
  if (mop != MOP_undef) {
    insn = cg->BuildInstruction<Riscv64Insn>(mop, dest, newSrc);
    if (isDirect && cg->GenerateVerboseAsm()) {
      insn->AddComment(key);
    }
    curbb->AppendInsn(insn);
  } else {
    if (IsPrimitiveFloat(stype)) {
      /* Uses signed integer version ldar followed by a floating-point move(fmov).  */
      CG_ASSERT(stype == dtype, "Just checking");
      PrimType itype = stype == PTY_f32 ? PTY_i32 : PTY_i64;
      RegOperand *regopnd = CreateRegisterOperandOfType(itype);
      insn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(ssize, itype, mo), regopnd, newSrc);
      if (isDirect && cg->GenerateVerboseAsm()) {
        insn->AddComment(key);
      }
      curbb->AppendInsn(insn);
      mop = stype == PTY_f32 ? MOP_xvmovsr : MOP_xvmovdr;
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, dest, regopnd));
    } else if (IsPrimitiveInteger(stype)){
      /* Use unsigned version ldarb/ldarh followed by a sign-extension instruction(sxtb/sxth).  */
      CG_ASSERT(ssize == 8 || ssize == 16, "Just checking");
      PrimType utype = ssize == 8 ? PTY_u8 : PTY_u16;
      insn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(ssize, utype, mo), dest, newSrc);
      if (isDirect && cg->GenerateVerboseAsm()) {
        insn->AddComment(key);
      }
      curbb->AppendInsn(insn);
      GenerateSext(dest, dest, dtype, ssize);
    } else {
      CG_ASSERT(false, "vector NYI");
    }
  }
}

void Riscv64CGFunc::SelectStoreRelease(Operand *dest, PrimType dtype, Operand *src, PrimType stype,
                                       Riscv64isa::memory_ordering_t mo, bool isDirect) {
  CG_ASSERT(dest->op_kind_ == Operand::Opd_Mem, "Just checking");
  CG_ASSERT(mo != Riscv64isa::kMoNone, "Just checking");

  Insn *insn = nullptr;
  uint32 dsize = isDirect ? dest->GetSize() : GetPrimTypeBitSize(stype);
  MOperator mop = PickStInsn(dsize, stype, mo);

  Operand *newDest = dest;
  Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(dest);
  Riscv64OfstOperand *immopnd = memopnd->GetOffsetImmediate();
  int32 offset = immopnd->GetOffsetValue();
  RegOperand *origBasereg = memopnd->GetBaseRegister();
  if (offset != 0) {
    RegOperand *resopnd = CreateRegisterOperandOfType(PTY_i64);
    SelectAdd(resopnd, origBasereg, immopnd, PTY_i64);
    newDest = CreateReplacementMemOperand(memopnd, dsize, resopnd, 0);
  }

  std::string key;
  if (isDirect && cg->GenerateVerboseAsm()) {
    MIRSymbol *sym = static_cast<Riscv64MemOperand *>(dest)->GetSymbol();
    if (sym) {
      MIRStorageClass sc = sym->GetStorageClass();
      if (sc == kScFormal) {
        key = "param: ";
      } else if (sc == kScAuto) {
        key = "local var: ";
      } else {
        key = "global: ";
      }
      key.append(sym->GetName());
    }
  }

  /* Check if the right store-release instruction is available. */
  if (mop != MOP_undef) {
    insn = cg->BuildInstruction<Riscv64Insn>(mop, src, newDest);
    if (isDirect && cg->GenerateVerboseAsm()) {
      insn->AddComment(key);
    }
    curbb->AppendInsn(insn);
  } else {
    /* Use a floating-point move(fmov) followed by a stlr.  */
    CG_ASSERT(IsPrimitiveFloat(stype) && stype == dtype, "Just checking");
    PrimType itype = stype == PTY_f32 ? PTY_i32 : PTY_i64;
    RegOperand *regopnd = CreateRegisterOperandOfType(itype);
    mop = stype == PTY_f32 ? MOP_xvmovrs : MOP_xvmovrd;
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, regopnd, src));
    insn = cg->BuildInstruction<Riscv64Insn>(PickStInsn(dsize, itype, mo), regopnd, newDest);
    if (isDirect && cg->GenerateVerboseAsm()) {
      insn->AddComment(key);
    }
    curbb->AppendInsn(insn);
  }
}

RegOperand *Riscv64CGFunc::MoveImm16Bits(uint32 val) {
  RegOperand *dest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 4));
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, dest, CreateImmOperand(val, 4, false)));
  return dest;
}

RegOperand *Riscv64CGFunc::CombineImmBits(Operand *dest, Operand *upper, Operand *lower, uint32 shift) {
  RegOperand *sllDest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 4));
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, sllDest, upper, CreateImmOperand(shift, 4, false)));
  if (dest == nullptr) {
    dest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 4));
  }
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xiorrrr, dest, sllDest, lower));
  return static_cast<RegOperand *>(dest);
}

void Riscv64CGFunc::SelectCopyImm(Operand *dest, Riscv64ImmOperand *src, PrimType dtype) {
  uint32 dsize = GetPrimTypeBitSize(dtype);
  CG_ASSERT(IsPrimitiveInteger(dtype), "The type of destination operand must be Integer");
  CG_ASSERT((dsize == 8 || dsize == 16 || dsize == 32 || dsize == 64), "The destination operand must be >= 8-bit");
  if (static_cast<Riscv64ImmOperand *>(src)->IsInBitSize(11)) {
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, dest, src));
  } else {
    uint64 srcval = static_cast<uint64>(src->GetValue());
    // using mov/movk to load the immediate value
    if (dsize == 8) {
      if (dtype == PTY_u8) {
        // zero extend
        srcval = (srcval << 56) >> 56;
        dtype = PTY_u16;
      } else {
        // sign extend
        srcval = ((static_cast<int64>(srcval)) << 56) >> 56;
        dtype = PTY_i16;
      }
      dsize = 16;
    }
    if (dsize == 16) {
      if (dtype == PTY_u16) {
        // create an imm opereand which represents lower 16 bits of the immediate
        ImmOperand *srcLower = CreateImmOperand((srcval & 0x0000FFFFULL), 16, false);
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, dest, srcLower));
        return;
      } else {
        // sign extend and let `dsize == 32` case take care of it
        srcval = (((int64)srcval) << 48) >> 48;
        dsize = 32;
      }
    }
    if (dsize == 32) {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, dest, CreateImmOperand(srcval, 32, src->IsSignedValue())));
    } else {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, dest, CreateImmOperand(srcval, 64, src->IsSignedValue())));
    }
  }
}

void Riscv64CGFunc::SelectCopy(Operand *dest, PrimType dtype, Operand *src, PrimType stype) {
  uint32 ssize = src->GetSize();
  CG_ASSERT(dest->IsRegister() || dest->IsMemoryAccessOperand(), "");
  uint32 dsize = GetPrimTypeBitSize(dtype);
  if (dest->IsRegister()) {
    dsize = dest->GetSize();
  }
  Operand::OperandType opnd0ty = dest->op_kind_;
  Operand::OperandType opnd1ty = src->op_kind_;
  //CG_ASSERT((dsize >= ssize || opnd0ty == Operand::Opd_Mem), "NYI");
  CG_ASSERT((opnd0ty == Operand::Opd_Register || opnd1ty == Operand::Opd_Register),
            "either src or dest should be register");

  switch (src->op_kind_) {
    case Operand::Opd_Mem: {
      Riscv64isa::memory_ordering_t mo = Riscv64isa::kMoNone;
      MIRSymbol *sym = static_cast<Riscv64MemOperand *>(src)->GetSymbol();
      if (sym && sym->GetStorageClass() == kScGlobal && sym->GetAttr(ATTR_memory_order_acquire)) {
        mo = Riscv64isa::kMoAcquire;
      }

      Insn *insn = nullptr;
      if (mo == Riscv64isa::kMoNone) {
        if (IsPrimitiveFloat(stype)) {
          CG_ASSERT(dsize == ssize, "");
          insn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(ssize, stype), dest, src);
          curbb->AppendInsn(insn);
        } else {
          PrimType symty = stype;
#if 0
          if (sym) {
            PrimType tmpty = sym->GetType()->primType;
            if (tmpty == PTY_u8 || tmpty == PTY_u16 || tmpty == PTY_u32) {
              symty = tmpty;
            }
          }
#endif

#if 0
          if (sym && dsize > ssize) {
            RegOperand *ldDst = CreateRegisterOperandOfType(dtype);
            insn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(ssize, symty), ldDst, src);
            curbb->AppendInsn(insn);
            switch (sym->GetType()->primType) {
            case PTY_i8:
            case PTY_i16:
            case PTY_i32:
            case PTY_i64:
              GenerateSext(dest, ldDst, dtype, ssize);
              break;
            default:
              GenerateZext(dest, ldDst, dtype, ssize);
            }
          } else {
#endif
          {
            insn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(ssize, symty), dest, src);
            curbb->AppendInsn(insn);
          }
        }

        if (cg->GenerateVerboseAsm()) {
          MIRSymbol *sym = static_cast<Riscv64MemOperand *>(src)->GetSymbol();
          if (sym) {
            std::string key;
            MIRStorageClass sc = sym->GetStorageClass();
            if (sc == kScFormal) {
              key = "param: ";
            } else if (sc == kScAuto) {
              key = "local var: ";
            } else {
              key = "global: ";
            }
            insn->AddComment(key.append(sym->GetName()));
          }
        }
      } else {
        Riscv64CGFunc::SelectLoadAcquire(dest, dtype, src, stype, mo, true);
      }
    } break;

    case Operand::Opd_Immediate:
      SelectCopyImm(dest, static_cast<Riscv64ImmOperand *>(src), stype);
      break;

    case Operand::Opd_FPZeroImmediate:
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(dsize == 32 ? MOP_xvmovsr : MOP_xvmovdr, dest,
                                                          Riscv64RegOperand::GetZeroRegister(dsize)));
      break;

    case Operand::Opd_Register: {
      if (opnd0ty == Operand::Opd_Mem) {
        Riscv64isa::memory_ordering_t mo = Riscv64isa::kMoNone;
        MIRSymbol *sym = static_cast<Riscv64MemOperand *>(dest)->GetSymbol();
        if (sym && sym->GetStorageClass() == kScGlobal && sym->GetAttr(ATTR_memory_order_release)) {
          mo = Riscv64isa::kMoRelease;
        }

        if (mo == Riscv64isa::kMoNone) {
          MOperator strmop = PickStInsn(dsize, stype);
          if (dest->IsMemoryAccessOperand()) {
            Riscv64MemOperand *mopnd = static_cast<Riscv64MemOperand *>(dest);
            CG_ASSERT(mopnd, "mopnd should not be nullptr");
            if (mopnd->GetOffsetOperand() == nullptr) {
              curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(strmop, src, dest));
              return;
            }
            ImmOperand *imopnd = static_cast<ImmOperand *>(mopnd->GetOffsetOperand());
            CG_ASSERT(imopnd, "imopnd should not be nullptr");
            int64 immVal = imopnd->GetValue();
            bool isInRange = false;
            bool isMopStr = false;
            switch (strmop) {
              case MOP_xstr:
              case MOP_wstr: {
                bool is64bits = (dest->GetSize() == 64) ? true : false;
                isInRange =
                   ((!is64bits && immVal >= STRALL_LDRALL_IMM_LOWER_BOUND && immVal <= STR_LDR_IMM32_UPPER_BOUND) ||
                    (is64bits && immVal >= STRALL_LDRALL_IMM_LOWER_BOUND && immVal <= STR_LDR_IMM64_UPPER_BOUND));
                isMopStr = true;
                break;
              }
              case MOP_wstrb:
                isInRange =
                  (immVal >= STRALL_LDRALL_IMM_LOWER_BOUND && immVal <= STRB_LDRB_IMM_UPPER_BOUND);
                isMopStr = true;
                break;
              case MOP_wstrh:
                isInRange =
                  (immVal >= STRALL_LDRALL_IMM_LOWER_BOUND && immVal <= STRH_LDRH_IMM_UPPER_BOUND);
                isMopStr = true;
                break;
              case MOP_vstr:
                isInRange =
                  (immVal >= STRALL_LDRALL_IMM_LOWER_BOUND && immVal <= STRH_LDRH_IMM_UPPER_BOUND);
                isMopStr = true;
                break;
              default:
                isMopStr = false;
                break;
            }
            if (isInRange || !isMopStr) {
              curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(strmop, src, dest));
            } else {
                RegOperand *immreg = CreateRegisterOperandOfType(PTY_i64);
                curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, immreg, imopnd));
                RegOperand *basereg = CreateRegisterOperandOfType(PTY_i64);
                curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, basereg, mopnd->GetBaseRegister(), immreg));
                MemOperand *newDest = GetOrCreateMemOpnd(GetPrimTypeBitSize(dtype),
                                                         basereg, nullptr, GetOrCreateOfstOpnd(0, 32),
                                                         static_cast<MIRSymbol *>(nullptr));
                curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(strmop, src, newDest));
            }
          } else {
            curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(strmop, src, dest));
          }
        } else {
          Riscv64CGFunc::SelectStoreRelease(dest, dtype, src, stype, mo, true);
        }
      } else {
        CG_ASSERT(stype != PTY_a32, "");
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickMovInsn(static_cast<RegOperand*>(dest), static_cast<RegOperand*>(src)), dest, src));
      }
    } break;

    default:
      CG_ASSERT(false, "NYI");
  }
}

// This function copies src to a register, the src can be an imm, mem or a label
RegOperand *Riscv64CGFunc::SelectCopy(Operand *src, PrimType stype, PrimType dtype) {
  RegOperand *dest = CreateRegisterOperandOfType(dtype);
  SelectCopy(dest, dtype, src, stype);
  return dest;
}

Operand *Riscv64CGFunc::SelectIgoto(Operand *opnd0) {
  if (opnd0->GetKind() == Operand::Opd_Mem) {
    regno_t vRegNo = New_V_Reg(kRegTyInt, 8);
    Operand *dst = CreateVirtualRegisterOperand(vRegNo);
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xldr, dst, opnd0));
    return dst;
  }
  return opnd0;
}

/*
   We need to adjust the offset of a stack allocated local variable
   if we store FP/SP before any other local variables to save an instruction.
   See Riscv64CGFunc::OffsetAdjustmentForFPLR() in riscv64_cg_func.cpp

   That is when we ShouldSaveFPLR() and !UsedStpSubPairForCallFrameAllocation().

   Because we need to use the STP/SUB instruction pair to store FP/SP 'after'
   local variables when the call frame size is greater that the max offset
   value allowed for the STP instruction (we cannot use STP w/ prefix, LDP w/
   postfix), if UsedStpSubPairForCallFrameAllocation(), we don't need to
   adjust the offsets.
 */
bool Riscv64CGFunc::IsImmediateOffsetOutOfRange(Riscv64MemOperand *mo, uint32 bitlen) {
  CG_ASSERT(8 <= bitlen && bitlen <= 64 && (bitlen & (bitlen - 1)) == 0, "");
  int32 offsetValue = mo->GetOffsetImmediate()->GetOffsetValue();
  if (mo->GetOffsetImmediate()->IsVary()) {
    offsetValue += static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize() + 0xff;
  }
  offsetValue += 2 * kIntregBytelen;  // Refer to the above comment
  return Riscv64MemOperand::IsPIMMOffsetOutOfRange(offsetValue, bitlen);
}

Riscv64MemOperand *Riscv64CGFunc::CreateReplacementMemOperand(Riscv64MemOperand *mo, uint32 bitlen, RegOperand *basereg,
                                                              int32 offset) {
  return static_cast<Riscv64MemOperand *>(CreateMemOpnd(basereg, offset, bitlen));
}

Riscv64MemOperand *Riscv64CGFunc::SplitOffsetWithAddInstruction(Riscv64MemOperand *mo, uint32 bitlen,
                                                                Riscv64reg_t baseregNum, Insn *insn, bool isDest) {
  Riscv64OfstOperand *oo = mo->GetOffsetImmediate();
  int32 ov = oo->GetOffsetValue();

  // li reg = offsetValue
  // add reg = sp + reg
  // load/store val -> [reg]
  RegOperand *resopnd;
  if (isAfterRegAlloc) {
    resopnd = GetOrCreatePhysicalRegisterOperand(Riscv64Abi::kIntSpareReg, (bitlen <= 32) ? 4 : 8, kRegTyInt);
  } else {
    resopnd = CreateRegisterOperandOfType(PTY_a64);
  }
  Riscv64ImmOperand *immOpnd = CreateImmOperand(ov, 8, false);
  if (mo->GetOffsetImmediate()->IsVary()) {
    immOpnd->SetVary(true);
  }
  Insn *immInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, resopnd, immOpnd);
  RegOperand *baseOpnd = mo->GetBaseRegister();
  Insn *addInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, resopnd, baseOpnd, resopnd);
  if (insn) {
    if (isDest) {
      insn->bb->InsertInsnAfter(insn, addInsn);
      insn->bb->InsertInsnAfter(insn, immInsn);
    } else {
      insn->bb->InsertInsnBefore(insn, immInsn);
      insn->bb->InsertInsnBefore(insn, addInsn);
    }
  } else {
    curbb->AppendInsn(immInsn);
    curbb->AppendInsn(addInsn);
  }

  MemOperand *newMemOpnd = CreateMemOpnd(resopnd, 0, bitlen);
  Riscv64MemOperand *retMopnd = dynamic_cast<Riscv64MemOperand *>(newMemOpnd);
  retMopnd->SetStackMem(mo->IsStackMem());
  return retMopnd;
}

Riscv64MemOperand *Riscv64CGFunc::SplitStpLdpOffsetForCalleeSavedWithAddInstruction(Riscv64MemOperand *mo,
                                                                                    uint32 bitlen,
                                                                                    Riscv64reg_t baseregNum) {
  Riscv64OfstOperand *oo = mo->GetOffsetImmediate();
  int32 ov = oo->GetOffsetValue();
  CG_ASSERT(ov > 0 && ((ov & 0x7) == 0), "");
  // if( ShouldSaveFPLR() )
  //  ov += 2 * kIntregBytelen; // we need to adjust the offset by length(FP/SP)
  // Offset adjustment due to FP/SP has already been done
  // in Riscv64CGFunc::GeneratePushRegs() and Riscv64CGFunc::GeneratePopRegs()
  Riscv64RegOperand *br = GetOrCreatePhysicalRegisterOperand(baseregNum, bitlen, kRegTyInt);
  if (split_stpldp_base_offset == 0) {
    split_stpldp_base_offset = ov;  // remember the offset; don't forget to clear it
    ImmOperand *immAddend = CreateImmOperand(ov, 64, true);
    RegOperand *origBasereg = mo->GetBaseRegister();
    SelectAdd(br, origBasereg, immAddend, PTY_i64);
  }
  ov = ov - split_stpldp_base_offset;
  return CreateReplacementMemOperand(mo, bitlen, br, ov);
}

void Riscv64CGFunc::SelectDassign(DassignNode *stmt, Operand *opnd0) {
  SelectDassign(stmt->stIdx, stmt->fieldID, stmt->GetRhs()->primType, opnd0);
}

// NOTE: I divided SelectDassign so that we can create "virtual" assignments
// when selecting other complex Maple IR instructions.  For example, the atomic
// exchange and other intrinsics will need to assign its results to local
// variables.  Such Maple IR instructions are pltform-specific (e.g.
// atomic_exchange can be implemented as one single machine intruction on x86_64
// and ARMv8.1, but ARMv8.0 needs an LL/SC loop), therefore they cannot (in
// principle) be lowered at BELowerer or CGLowerer.
void Riscv64CGFunc::SelectDassign(StIdx stIdx, FieldID fieldID, PrimType rhsPtyp, Operand *opnd0) {
  MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(stIdx);
  int32 offset = 0;
  bool parmCopy = false;
  if (fieldID != 0) {
    MIRStructType *structty = static_cast<MIRStructType *>(symbol->GetType());
    CG_ASSERT(structty, "SelectDassign: non-zero fieldID for non-structure");
    offset = becommon.GetFieldOffset(structty, fieldID).first;
    parmCopy = IsParamStructCopy(symbol);
  }
  int regsize = GetPrimTypeBitSize(rhsPtyp);
  MIRType *type = symbol->GetType();
  Operand *stOpnd = LoadIntoRegister(opnd0, IsPrimitiveInteger(rhsPtyp), regsize, IsSignedInteger(type->GetPrimType()));
  MOperator mop = MOP_undef;
  if (type->typeKind == kTypeStruct || type->typeKind == kTypeUnion) {
    MIRStructType *structType = static_cast<MIRStructType *>(type);
    TyIdx ftyidx = structType->TraverseToField(fieldID).second.first;
    type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx);
  } else if (type->typeKind == kTypeClass) {
    MIRClassType *classType = static_cast<MIRClassType *>(type);
    TyIdx ftyidx = classType->TraverseToField(fieldID).second.first;
    type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx);
  }

  int datasize = GetPrimTypeBitSize(type->GetPrimType());
  if (type->GetPrimType() == PTY_agg) {
    datasize = GetPrimTypeBitSize(PTY_a64);
  }
  MemOperand *memopnd;
  if (parmCopy) {
    memopnd = LoadStructCopyBase(symbol, offset, datasize);
  } else {
    memopnd = GetOrCreateMemOpnd(symbol, offset, datasize);
  }
  CHECK_FATAL(memopnd != nullptr && static_cast<Riscv64MemOperand *>(memopnd) != nullptr,
         "pointer is null in Riscv64CGFunc::SelectDassign");
  if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(memopnd), datasize)) {
    memopnd = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(memopnd), datasize);
  }

  MIRTypeKind tyKind = type->typeKind;
  CG_ASSERT((tyKind == kTypeScalar || tyKind == kTypePointer || tyKind == kTypeStruct || tyKind == kTypeArray),
            "NYI dassign type");
  PrimType primType = type->GetPrimType();
  if (primType == PTY_agg) {
    if (static_cast<RegOperand*>(stOpnd)->IsOfIntClass()) {
      primType = PTY_a64;
    } else {
      primType = PTY_f64;
    }
  }

  Riscv64isa::memory_ordering_t mo = Riscv64isa::kMoNone;
  MIRSymbol *sym = static_cast<Riscv64MemOperand *>(memopnd)->GetSymbol();
  if (sym && sym->GetStorageClass() == kScGlobal && isVolStore) {
    mo = Riscv64isa::kMoRelease;
    isVolStore = false;
  }
  Insn *insn = nullptr;
  if (mo == Riscv64isa::kMoNone) {
    mop = PickStInsn(GetPrimTypeBitSize(primType), primType);
    insn = cg->BuildInstruction<Riscv64Insn>(mop, stOpnd, memopnd);

    if (cg->GenerateVerboseAsm()) {
      MIRSymbol *sym = static_cast<Riscv64MemOperand *>(memopnd)->GetSymbol();
      if (sym) {
        std::string key;
        MIRStorageClass sc = sym->GetStorageClass();
        if (sc == kScFormal) {
          key = "param: ";
        } else if (sc == kScAuto) {
          key = "local var: ";
        } else {
          key = "global: ";
        }
        insn->AddComment(key.append(sym->GetName()));
      }
    }

    curbb->AppendInsn(insn);
  } else {
    Riscv64CGFunc::SelectStoreRelease(memopnd, primType, stOpnd, primType, mo, true);
  }
}

void Riscv64CGFunc::SelectAssertnull(UnaryStmtNode *stmt) {
  Operand *opnd0 = HandleExpr(stmt, stmt->uOpnd);
  RegOperand *basereg = LoadIntoRegister(opnd0, PTY_a64);
  auto &zwr = Riscv64RegOperand::Get32bitZeroRegister();
  auto *mem = CreateMemOpnd(basereg, 0, 32);
  Insn *loadref = cg->BuildInstruction<Riscv64Insn>(MOP_wldr, &zwr, mem);
  loadref->do_not_remove = true;
  if (cg->GenerateVerboseAsm()) {
    loadref->AddComment("null pointer check");
  }
  curbb->AppendInsn(loadref);
}

void Riscv64CGFunc::SelectRegassign(RegassignNode *stmt, Operand *opnd0) {
  RegOperand *regopnd = nullptr;
  PregIdx pregidx = stmt->regIdx;

  if (IsSpecialPseudoRegister(pregidx)) {
    // if it is one of special registers
    CG_ASSERT(-pregidx != kSregRetval0, "");
    regopnd = GetOrCreateSpecialRegisterOperand(-pregidx);
  } else {
    regopnd = GetOrCreateVirtualRegisterOperand(GetVirtualRegNoFromPseudoRegIdx(pregidx));
  }
  // look at rhs
  PrimType rhstype = stmt->uOpnd->primType;
  PrimType dtype = rhstype;
  if (GetPrimTypeBitSize(dtype) < 32) {
    CG_ASSERT(IsPrimitiveInteger(dtype), "");
    dtype = IsSignedInteger(dtype) ? PTY_i32 : PTY_u32;
  }
  SelectCopy(regopnd, dtype, opnd0, rhstype);

  if (g->optim_level == 0 && pregidx >= 0) {
    MemOperand *dest = GetPseudoRegisterSpillMemoryOperand(pregidx);
    PrimType stype = GetTypeFromPseudoRegIdx(pregidx);
    MIRPreg *preg = func->pregTab->PregFromPregIdx(pregidx);
    uint32_t srcbitlen = GetPrimTypeSize(preg->primType) * BITS_PER_BYTE;
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickStInsn(srcbitlen, stype), regopnd, dest));
  }
  if (mirModule.IsCModule() && g->optim_level > 0 && pregidx >= 0) {
    // Here it is assumed both lhs and rhs are of the same signed/unsigned type.
    // Otherwise, IR is missing a conversion.
    PrimType lhstype = stmt->primType;
    uint32 rhssize = GetPrimTypeBitSize(rhstype);
    uint32 lhssize = GetPrimTypeBitSize(lhstype);
    MOperator mop = MOP_undef;
    if (rhssize > lhssize) {
      switch (lhstype) {
      case PTY_i8:
        GenerateSext(regopnd, regopnd, lhstype, 8);
        return;
      case PTY_i16:
        GenerateSext(regopnd, regopnd, lhstype, 16);
        return;
      case PTY_i32:
        mop = MOP_xsxtw64;
        break;
      case PTY_u8:
        GenerateZext(regopnd, regopnd, lhstype, 8);
        return;
      case PTY_u16:
        GenerateZext(regopnd, regopnd, lhstype, 16);
        return;
      case PTY_u32:
        GenerateZext(regopnd, regopnd, lhstype, 32);
        break;
      default:
        CHECK_FATAL(0,"Unsupported primtype");
      }
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, regopnd, regopnd));
    } if (lhssize > rhssize) {
      switch (rhstype) {
      case PTY_i8:
        //mop = MOP_xsxtb64;
        GenerateSext(regopnd, regopnd, lhstype, 8);
        return;
      case PTY_i16:
        //mop = MOP_xsxth64;
        GenerateSext(regopnd, regopnd, lhstype, 16);
        return;
      case PTY_i32:
        mop = MOP_xsxtw64;
        break;
      case PTY_u8:
        //mop = MOP_xuxtb32;
        GenerateZext(regopnd, regopnd, lhstype, 8);
        return;
      case PTY_u16:
        //mop = MOP_xuxth32;
        GenerateZext(regopnd, regopnd, lhstype, 16);
        return;
      case PTY_u32:
        GenerateZext(regopnd, regopnd, lhstype, 32);
        break;
      default:
        CHECK_FATAL(0,"Unsupported primtype");
      }
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, regopnd, regopnd));
    }
  }
}

void Riscv64CGFunc::SelectAggDassign(DassignNode *stmt) {
  MIRSymbol *lhssymbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(stmt->stIdx);
  int32 lhsoffset = 0;
  MIRType *lhsty = lhssymbol->GetType();
  if (stmt->fieldID != 0) {
    MIRStructType *structty = static_cast<MIRStructType *>(lhsty);
    CG_ASSERT(structty, "SelectDassign: non-zero fieldID for non-structure");
    FieldPair thepair = structty->TraverseToField(stmt->fieldID);
    lhsty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
    lhsoffset = becommon.GetFieldOffset(structty, stmt->fieldID).first;
  }
  uint32 lhsalign = becommon.type_align_table[lhsty->tyIdx.GetIdx()];
  uint32 stAlign = becommon.type_natural_align_table[lhsty->tyIdx.GetIdx()];
  if (stAlign && stAlign < lhsalign) {
    lhsalign = stAlign;
  }
  uint32 lhssize = becommon.type_size_table.at(lhsty->tyIdx.GetIdx());

  uint32 rhsalign;
  uint32 alignused;
  int32 rhsoffset = 0;
  if (stmt->GetRhs()->op == OP_dread) {
    AddrofNode *rhsdread = static_cast<AddrofNode *>(stmt->GetRhs());
    MIRSymbol *rhssymbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(rhsdread->stIdx);
    MIRType *rhsty = rhssymbol->GetType();
    if (rhsdread->fieldID != 0) {
      MIRStructType *structty = static_cast<MIRStructType *>(rhsty);
      CG_ASSERT(structty, "SelectDassign: non-zero fieldID for non-structure");
      FieldPair thepair = structty->TraverseToField(rhsdread->fieldID);
      rhsty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(structty, rhsdread->fieldID).first;
    }
    rhsalign = becommon.type_align_table[rhsty->tyIdx.GetIdx()];
    uint32 stAlign = becommon.type_natural_align_table[rhsty->tyIdx.GetIdx()];
    if (stAlign && stAlign < rhsalign) {
      rhsalign = stAlign;
    }

    // arm64 can handle unaligned memory access, so there is
    //   is no need to split it into smaller accesses.
    bool parmCopy = IsParamStructCopy(rhssymbol);
    alignused = std::min(lhsalign, rhsalign);
    Operand *rhsmemopnd = nullptr;
    Operand *lhsmemopnd = nullptr;
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      if (parmCopy) {
        rhsmemopnd = LoadStructCopyBase(rhssymbol, rhsoffset + i * alignused, alignused * BITS_PER_BYTE);
      } else {
        rhsmemopnd = GetOrCreateMemOpnd(rhssymbol, rhsoffset + i * alignused, alignused * BITS_PER_BYTE);
      }
      regno_t vRegNo = New_V_Reg(kRegTyInt, std::max(4u, alignused));
      RegOperand *result = CreateVirtualRegisterOperand(vRegNo);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickLdInsn(alignused * 8, PTY_u32), result, rhsmemopnd));
      if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(rhsmemopnd), alignused * 8)) {
        curbb->lastinsn->opnds[1] =
            SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(rhsmemopnd), alignused * 8, kRinvalid, curbb->lastinsn);
      }
      // generate the store
      if (lhssymbol->storageClass == kScFormal && becommon.type_size_table[lhssymbol->tyIdx.GetIdx()] > 16) {
        // formal of size of greater than 16 is copied by the caller and the pointer to it is passed.
        // otherwise it is passed in register and is accessed directly.
        RegOperand *vreg;
        lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, 0, alignused * BITS_PER_BYTE);
        vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        Insn *ldInsn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(64, PTY_i64), vreg, lhsmemopnd);
        curbb->AppendInsn(ldInsn);
        lhsmemopnd = GetOrCreateMemOpnd(64, vreg, nullptr,
                    GetOrCreateOfstOpnd(lhsoffset + i * alignused, 32), static_cast<MIRSymbol *>(nullptr));
      } else {
        lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + i * alignused, alignused * 8);
      }
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickStInsn(alignused * 8, PTY_u32), result, lhsmemopnd));
      if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(lhsmemopnd), alignused * 8)) {
        curbb->lastinsn->opnds[1] =
            SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(lhsmemopnd), alignused * 8, kRinvalid, curbb->lastinsn);
      }
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssizeCovered = (lhssize / alignused) * alignused;
    uint32 newalignused = alignused;
    while (lhssizeCovered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssizeCovered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      rhsmemopnd = GetOrCreateMemOpnd(rhssymbol, rhsoffset + lhssizeCovered, newalignused * 8);
      regno_t vRegNo = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
      RegOperand *result = CreateVirtualRegisterOperand(vRegNo);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickLdInsn(newalignused * 8, PTY_u32), result, rhsmemopnd));
      if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(rhsmemopnd), newalignused * 8)) {
        curbb->lastinsn->opnds[1] =
            SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(rhsmemopnd), newalignused * 8, kRinvalid, curbb->lastinsn);
      }
      // generate the store
      if (lhssymbol->storageClass == kScFormal && becommon.type_size_table[lhssymbol->tyIdx.GetIdx()] > 16) {
        RegOperand *vreg;
        lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, 0, newalignused * BITS_PER_BYTE);
        vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        Insn *ldInsn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(64, PTY_i64), vreg, lhsmemopnd);
        curbb->AppendInsn(ldInsn);
        lhsmemopnd = GetOrCreateMemOpnd(64, vreg, nullptr,
                    GetOrCreateOfstOpnd(lhsoffset + lhssizeCovered, 32), static_cast<MIRSymbol *>(nullptr));
      } else {
        lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + lhssizeCovered, newalignused * 8);
      }
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickStInsn(newalignused * 8, PTY_u32), result, lhsmemopnd));
      lhssizeCovered += newalignused;
      if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(lhsmemopnd), newalignused * 8)) {
        curbb->lastinsn->opnds[1] =
            SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(lhsmemopnd), newalignused * 8, kRinvalid, curbb->lastinsn);
      }
    }
  } else if (stmt->GetRhs()->op == OP_iread) {  // rhs is iread
    IreadNode *rhsiread = static_cast<IreadNode *>(stmt->GetRhs());
    RegOperand *addropnd = static_cast<RegOperand *>(HandleExpr(rhsiread, rhsiread->Opnd(0)));
    addropnd = LoadIntoRegister(addropnd, rhsiread->Opnd(0)->primType);
    MIRPtrType *rhspointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(rhsiread->tyIdx));
    MIRType *rhsty = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(rhspointerty->pointedTyIdx));
    bool isRefField = false;
    if (rhsiread->fieldID != 0) {
      MIRStructType *rhsstructty = static_cast<MIRStructType *>(rhsty);
      CG_ASSERT(rhsstructty, "SelectAggDassign: non-zero fieldID for non-structure");
      FieldPair thepair = rhsstructty->TraverseToField(rhsiread->fieldID);
      rhsty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(rhsstructty, rhsiread->fieldID).first;
      isRefField = becommon.IsRefField(rhsstructty, rhsiread->fieldID);
    }

    rhsalign = becommon.type_align_table[rhsty->tyIdx.GetIdx()];
    uint32 stAlign = becommon.type_natural_align_table[rhsty->tyIdx.GetIdx()];
    if (stAlign && stAlign < rhsalign) {
      rhsalign = stAlign;
    }

    if (lhsalign == 0 || rhsalign == 0) {
      // workaround for IR where rhs alignment cannot be determined.
      alignused = lhsalign == 0 ? rhsalign : (rhsalign == 0 ? lhsalign :0);
      CG_ASSERT(alignused, "");    // both cannot be 0?
    } else {
      alignused = std::min(lhsalign, rhsalign);
    }
    Operand *rhsmemopnd = nullptr;
    Operand *lhsmemopnd = nullptr;
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      Riscv64OfstOperand *offopnd = GetOrCreateOfstOpnd(rhsoffset + i * alignused, 32);
      rhsmemopnd = GetOrCreateMemOpnd(alignused * 8, addropnd, nullptr, offopnd,
                                      static_cast<MIRSymbol *>(nullptr));
      regno_t vRegNo = New_V_Reg(kRegTyInt, std::max(4u, alignused));
      RegOperand *result = CreateVirtualRegisterOperand(vRegNo);
      Insn *insn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(alignused * 8, PTY_u32), result, rhsmemopnd);
      insn->MarkAsAccessRefField(isRefField);
      curbb->AppendInsn(insn);
      if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(rhsmemopnd), alignused * 8)) {
        curbb->lastinsn->opnds[1] =
            SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(rhsmemopnd), alignused * 8, kRinvalid, curbb->lastinsn);
      }
      // generate the store
      if (lhssymbol->storageClass == kScFormal && becommon.type_size_table[lhssymbol->tyIdx.GetIdx()] > 16) {
        RegOperand *vreg;
        lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, 0, alignused * BITS_PER_BYTE);
        vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        Insn *ldInsn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(64, PTY_i64), vreg, lhsmemopnd);
        curbb->AppendInsn(ldInsn);
        lhsmemopnd = GetOrCreateMemOpnd(64, vreg, nullptr,
                    GetOrCreateOfstOpnd(lhsoffset + i * alignused, 32), static_cast<MIRSymbol *>(nullptr));
      } else {
        lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + i * alignused, alignused * 8);
      }
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickStInsn(alignused * 8, PTY_u32), result, lhsmemopnd));
      if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(lhsmemopnd), alignused * 8)) {
        curbb->lastinsn->opnds[1] =
            SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(lhsmemopnd), alignused * 8, kRinvalid, curbb->lastinsn);
      }
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssizeCovered = (lhssize / alignused) * alignused;
    uint32 newalignused = alignused;
    while (lhssizeCovered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssizeCovered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      Riscv64OfstOperand *offopnd = GetOrCreateOfstOpnd(rhsoffset + lhssizeCovered, 32);
      rhsmemopnd = GetOrCreateMemOpnd(newalignused * 8, addropnd, nullptr, offopnd,
                                      static_cast<MIRSymbol *>(nullptr));
      regno_t vRegNo = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
      RegOperand *result = CreateVirtualRegisterOperand(vRegNo);
      Insn *insn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(newalignused * 8, PTY_u32), result, rhsmemopnd);
      insn->MarkAsAccessRefField(isRefField);
      curbb->AppendInsn(insn);
      if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(rhsmemopnd), newalignused * 8)) {
        curbb->lastinsn->opnds[1] =
            SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(rhsmemopnd), newalignused * 8, kRinvalid, curbb->lastinsn);
      }
      // generate the store
      if (lhssymbol->storageClass == kScFormal && becommon.type_size_table[lhssymbol->tyIdx.GetIdx()] > 16) {
        RegOperand *vreg;
        lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, 0, newalignused * BITS_PER_BYTE);
        vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        Insn *ldInsn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(64, PTY_i64), vreg, lhsmemopnd);
        curbb->AppendInsn(ldInsn);
        lhsmemopnd = GetOrCreateMemOpnd(64, vreg, nullptr,
                    GetOrCreateOfstOpnd(lhsoffset + lhssizeCovered, 32), static_cast<MIRSymbol *>(nullptr));
      } else {
        lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + lhssizeCovered, newalignused * 8);
      }
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickStInsn(newalignused * 8, PTY_u32), result, lhsmemopnd));
      lhssizeCovered += newalignused;
      if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(lhsmemopnd), newalignused * 8)) {
        curbb->lastinsn->opnds[1] =
            SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(lhsmemopnd), newalignused * 8, kRinvalid, curbb->lastinsn);
      }
    }
  } else {
    CG_ASSERT(stmt->GetRhs()->op == OP_regread, "SelectAggDassign: NYI");
    bool isRet = false;
    if (lhsty->typeKind == kTypeStruct || lhsty->typeKind == kTypeUnion) {
      RegreadNode *rhsregread = static_cast<RegreadNode *>(stmt->GetRhs());
      PregIdx pregidx = rhsregread->regIdx;
      if (IsSpecialPseudoRegister(pregidx)) {
        pregidx = GetSpecialPseudoRegisterIndex(pregidx);
        if (pregidx == kSregRetval0) {
          ParmLocator parmlocator(becommon);
          PLocInfo ploc;
          PrimType retpty;
          RegType regtype;
          uint32 memsize;
          uint32 regsize;
          parmlocator.LocateRetValue(lhsty, ploc);
          Riscv64reg_t r[4];
          r[0] = ploc.reg0;
          r[1] = ploc.reg1;
          uint32 sizes[2];
          sizes[0] = ploc.rsize0;
          sizes[1] = ploc.rsize1;
          for (uint32 i = 0; i < 2; ++i) {
            if (r[i] == kRinvalid) {
              break;
            }
            if (r[i] >= V10) {
              regsize = (sizes[i] == 4) ? 32 : 64;
              memsize = sizes[i];
              retpty = (sizes[i] == 4) ? PTY_f32 : PTY_f64;
              regtype = kRegTyFloat;
            } else {
              regsize = 64;
              memsize = 8;
              retpty = PTY_u64;
              regtype = kRegTyInt;
            }
            RegOperand *parm = GetOrCreatePhysicalRegisterOperand(r[i], regsize, regtype);
            Operand *memopnd = GetOrCreateMemOpnd(lhssymbol, memsize * i, regsize);
            curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickStInsn(regsize, retpty), parm, memopnd));
            if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(memopnd), regsize)) {
              curbb->lastinsn->opnds[1] =
                  SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(memopnd), regsize * 8, kRinvalid, curbb->lastinsn);
            }
          }
          isRet = true;
        }
      }
    }
    CHECK_FATAL(isRet, "SelectAggDassign: NYI");
  }
}

static MIRType *GetPointedToType(MIRPtrType *pointerty) {
  MIRType *atype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
  if (atype->GetKind() == kTypeArray) {
    MIRArrayType *arraytype = static_cast<MIRArrayType *>(atype);
    return GlobalTables::GetTypeTable().GetTypeFromTyIdx(arraytype->eTyIdx);
  }
  if (atype->GetKind() == kTypeFArray || atype->GetKind() == kTypeJArray) {
    MIRFarrayType *farraytype = static_cast<MIRFarrayType *>(atype);
    return GlobalTables::GetTypeTable().GetTypeFromTyIdx(farraytype->elemTyIdx);
  }
  return atype;
}

void Riscv64CGFunc::SelectIassign(IassignNode *stmt) {
  int32 offset = 0;
  MIRPtrType *pointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(stmt->tyIdx));
  CG_ASSERT(pointerty, "expect a pointer type at iassign node");
  MIRType *pointedType = nullptr;
  bool isRefField = false;
  Riscv64isa::memory_ordering_t mo = Riscv64isa::kMoNone;

  if (stmt->fieldID != 0) {
    MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
    MIRStructType *structty = nullptr;
    if (pointedty->GetKind() != kTypeJArray) {
      structty = static_cast<MIRStructType *>(pointedty);
      CG_ASSERT(structty, "SelectIassign: non-zero fieldID for non-structure");
    } else {
      // it's a Jarray type. using it's parent's field info: java.lang.Object
      structty = static_cast<MIRJarrayType *>(pointedty)->GetParentType();
    }
    CG_ASSERT(structty, "SelectIassign: non-zero fieldID for non-structure");
    FieldPair thepair = structty->TraverseToField(stmt->fieldID);
    pointedType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
    offset = becommon.GetFieldOffset(structty, stmt->fieldID).first;
    isRefField = becommon.IsRefField(structty, stmt->fieldID);
  } else {
    pointedType = GetPointedToType(pointerty);
    if (func->IsJava() && (pointedType->GetKind() == kTypePointer)) {
      MIRType *nextPointedType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(pointedType)->pointedTyIdx);
      if (nextPointedType->GetKind() != kTypeScalar) {
        isRefField = true;  // write into an object array or a high-dimensional array
      }
    }
  }

  PrimType styp = stmt->rhs->primType;
  Operand *valopnd = HandleExpr(stmt, stmt->rhs);
  PrimType destType = pointedType->GetPrimType();
  if (IsPrimitiveVector(styp)) {
    CG_ASSERT(stmt->fieldID == 0, "NYI");
    MemOperand *memopnd = CreateMemOpnd(styp, stmt, stmt->addrExpr, offset);
    SelectCopy(memopnd, styp, valopnd, styp);
    return;
  }
  Operand *srcOpnd = LoadIntoRegister(valopnd, IsPrimitiveInteger(styp), GetPrimTypeBitSize(styp));

  if (destType == PTY_agg) {
    destType = PTY_a64;
  }

  MemOperand *memopnd = CreateMemOpnd(destType, stmt, stmt->addrExpr, offset);
  if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(memopnd), GetPrimTypeBitSize(destType))) {
    memopnd = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(memopnd), GetPrimTypeBitSize(destType));
  }
  if (isVolStore) {
    mo = Riscv64isa::kMoRelease;
    isVolStore = false;
  }

  if (mo == Riscv64isa::kMoNone) {
    SelectCopy(memopnd, destType, srcOpnd, destType);
  } else {
    Riscv64CGFunc::SelectStoreRelease(memopnd, destType, srcOpnd, destType, mo, false);
  }
  curbb->lastinsn->MarkAsAccessRefField(isRefField);
}

void Riscv64CGFunc::SelectAggIassign(IassignNode *stmt, Operand *lhsaddropnd) {
  MIRType *stmtty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(stmt->tyIdx);
  MIRPtrType *lhspointerty;
  MIRSymbol *addrsym;
  if (stmtty->primType == PTY_agg) {
    // Move into regs.
    AddrofNode *addrofnode = dynamic_cast<AddrofNode *>(stmt->addrExpr);
    addrsym = mirModule.CurFunction()->GetLocalOrGlobalSymbol(addrofnode->stIdx);
    MIRType *addrty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(addrsym->tyIdx);
    lhspointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(addrty->tyIdx));
  } else {
    lhsaddropnd = LoadIntoRegister(lhsaddropnd, stmt->addrExpr->primType);
    lhspointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(stmt->tyIdx));
  }
  int32 lhsoffset = 0;
  MIRType *lhsty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(lhspointerty->pointedTyIdx);
  if (stmt->fieldID != 0) {
    MIRStructType *structty = static_cast<MIRStructType *>(lhsty);
    CG_ASSERT(structty, "SelectAggIassign: non-zero fieldID for non-structure");
    FieldPair thepair = structty->TraverseToField(stmt->fieldID);
    lhsty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
    lhsoffset = becommon.GetFieldOffset(structty, stmt->fieldID).first;
  } else if (MIRArrayType *arraylhsty = dynamic_cast<MIRArrayType *>(lhsty)) {
    // access an array element
    lhsty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(arraylhsty->eTyIdx);
    MIRTypeKind tykind = lhsty->typeKind;
    CG_ASSERT((tykind == kTypeScalar || tykind == kTypeStruct || tykind == kTypeClass || tykind == kTypePointer),
              "unexpected array element type in iassign");
  } else if (MIRFarrayType *farraylhsty = dynamic_cast<MIRFarrayType *>(lhsty)) {
    // access an array element
    lhsty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(farraylhsty->elemTyIdx);
    MIRTypeKind tykind = lhsty->typeKind;
    CG_ASSERT((tykind == kTypeScalar || tykind == kTypeStruct || tykind == kTypeClass || tykind == kTypePointer),
              "unexpected array element type in iassign");
  }

  uint32 lhsalign = becommon.type_align_table[lhsty->tyIdx.GetIdx()];
  uint32 stAlign = becommon.type_natural_align_table[lhsty->tyIdx.GetIdx()];
  if (stAlign && stAlign < lhsalign) {
    lhsalign = stAlign;
  }
  uint32 lhssize = becommon.type_size_table.at(lhsty->tyIdx.GetIdx());

  uint32 rhsalign;
  uint32 alignused;
  int32 rhsoffset = 0;
  if (stmt->rhs->op == OP_dread) {
    AddrofNode *rhsdread = static_cast<AddrofNode *>(stmt->rhs);
    MIRSymbol *rhssymbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(rhsdread->stIdx);
    MIRType *rhsty = rhssymbol->GetType();
    if (rhsdread->fieldID != 0) {
      MIRStructType *structty = static_cast<MIRStructType *>(rhssymbol->GetType());
      CG_ASSERT(structty, "SelectDassign: non-zero fieldID for non-structure");
      FieldPair thepair = structty->TraverseToField(rhsdread->fieldID);
      rhsty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(structty, rhsdread->fieldID).first;
    }
    if (stmtty->primType == PTY_agg) {
      // generate move to regs.
      CHECK_FATAL(lhssize <= 16, "SelectAggIassign: illegal struct size");
      ParmLocator parmlocator(becommon);
      PLocInfo ploc;
      MIRSymbol *retst = becommon.mirModule.CurFunction()->formalDefVec[0].formalSym;
      if (retst == addrsym) {
        // return value
        parmlocator.LocateNextParm(lhsty, ploc, true);
      } else {
        parmlocator.InitPlocInfo(ploc);
      }

      // aggregates are 8 byte aligned.
      Operand *rhsmemopnd = nullptr;
      RegOperand *result[2];
      uint32 loadSize;
      RegType regtype;
      PrimType retpty;
      bool fpParm = false;
      bool parmCopy = IsParamStructCopy(rhssymbol);
      Riscv64reg_t regs[2];
      regs[0] = ploc.reg0;
      regs[1] = ploc.reg1;
      uint32 num = 0;
      if (regs[0] != kRinvalid) {
        num = regs[1] != kRinvalid ? 2 : 1;
      }
      CG_ASSERT(num <= 2, "");
      uint32 rsizes[2];
      rsizes[0] = ploc.rsize0;
      rsizes[1] = ploc.rsize1;
      for (uint32 i = 0; i < num; i++) {
        loadSize = rsizes[i];
        if (regs[i] >= V10) {
          fpParm = true;
          regtype = kRegTyFloat;
          retpty = (loadSize == 4) ? PTY_f32 : PTY_f64;
        } else {
          regtype = kRegTyInt;
          retpty = (loadSize == 4) ? PTY_u32 : PTY_u64;
        }
        if (parmCopy) {
          if (fpParm) {
            rhsmemopnd = LoadStructCopyBase(rhssymbol, rhsoffset + i * loadSize, loadSize * BITS_PER_BYTE);
          } else {
            rhsmemopnd = LoadStructCopyBase(rhssymbol, rhsoffset + i * 8, loadSize * BITS_PER_BYTE);
          }
        } else if (fpParm) {
          rhsmemopnd = GetOrCreateMemOpnd(rhssymbol, rhsoffset + i * loadSize, loadSize * BITS_PER_BYTE);
        } else {
          rhsmemopnd = GetOrCreateMemOpnd(rhssymbol, rhsoffset + i * 8, loadSize * BITS_PER_BYTE);
        }
        regno_t vRegNo = New_V_Reg(regtype, loadSize);
        result[i] = CreateVirtualRegisterOperand(vRegNo);
        Insn *ld = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(loadSize * BITS_PER_BYTE, retpty), result[i], rhsmemopnd);
        curbb->AppendInsn(ld);
        if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(rhsmemopnd), loadSize * 8)) {
          curbb->lastinsn->opnds[1] =
              SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(rhsmemopnd), loadSize * 8, kRinvalid, curbb->lastinsn);
        }
      }
      for (uint32 i = 0; i < num; i++) {
        Riscv64reg_t preg;
        MOperator mop;
        preg = regs[i];
        if (regs[i] >= V10) {
          mop = (loadSize == 4) ? MOP_xvmovs : MOP_xvmovd;
        } else {
          mop = (loadSize == 4) ? MOP_wmovrr: MOP_xmovrr;
        }
        RegOperand *dest = GetOrCreatePhysicalRegisterOperand(preg, loadSize * BITS_PER_BYTE, regtype);
        Insn *mov = cg->BuildInstruction<Riscv64Insn>(mop, dest, result[i]);
        curbb->AppendInsn(mov);
      }
      for (uint32 i = 0; i < num; i++) {
        Riscv64reg_t preg;
        MOperator mop;
        preg = regs[i];
        if (regs[i] >= V10) {
          mop = MOP_pseudo_ret_float;
        } else {
          mop = MOP_pseudo_ret_int;
        }
        RegOperand *dest = GetOrCreatePhysicalRegisterOperand(preg, loadSize * BITS_PER_BYTE, regtype);
        Insn *pseudo = cg->BuildInstruction<Riscv64Insn>(mop, dest);
        curbb->AppendInsn(pseudo);
      }
    } else {
      rhsalign = becommon.type_align_table[rhsty->tyIdx.GetIdx()];
      if (GetPrimTypeSize(stmtty->primType) < rhsalign) {
        rhsalign = GetPrimTypeSize(stmtty->primType);
      }

      alignused = std::min(lhsalign, rhsalign);
      MemOperand *rhsmemopnd = nullptr;
      MemOperand *lhsmemopnd = nullptr;
      bool parmCopy = IsParamStructCopy(rhssymbol);
      uint32 numElems = lhssize / alignused;
      if (numElems > 4) {
        vector<Operand *> opndvec;
        RegOperand *vreg;
        vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        opndvec.push_back(vreg);  // result
        Riscv64OfstOperand *offopnd = GetOrCreateOfstOpnd(lhsoffset, 32);
        lhsmemopnd = GetOrCreateMemOpnd(alignused * 8,
                         static_cast<Riscv64RegOperand *>(lhsaddropnd), nullptr, offopnd,
                         static_cast<MIRSymbol *>(nullptr));
        vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        ImmOperand *offOpnd = static_cast<ImmOperand *>(lhsmemopnd->GetOffsetOperand());
        int64 val = offOpnd->GetValue();
        Operand *base = lhsmemopnd->GetBaseRegister();
        if (val > 0x7ff) {
          Insn *insn;
          RegOperand *tmpreg;
          tmpreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
          ImmOperand *newImmOpnd = CreateImmOperand(val, 64, false);
          insn = cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, tmpreg, newImmOpnd);
          curbb->AppendInsn(insn);
          insn = cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, vreg, base, tmpreg);
          curbb->AppendInsn(insn);
        } else {
          if (val == 0) {
            vreg = static_cast<RegOperand *>(base);
          } else {
            Insn *insn = cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, vreg, base, offOpnd);
            curbb->AppendInsn(insn);
          }
        }
        opndvec.push_back(vreg);  // param 0
        if (parmCopy) {
          rhsmemopnd = LoadStructCopyBase(rhssymbol, rhsoffset, alignused * 8);
        } else {
          rhsmemopnd = GetOrCreateMemOpnd(rhssymbol, rhsoffset, alignused * 8);
        }
        vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        offOpnd = static_cast<ImmOperand *>(rhsmemopnd->GetOffsetOperand());
        val = offOpnd->GetValue();
        base = rhsmemopnd->GetBaseRegister();
        if (val > 0x7ff) {
          Insn *insn;
          RegOperand *tmpreg;
          tmpreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
          ImmOperand *newImmOpnd = CreateImmOperand(val, 64, false);
          insn = cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, tmpreg, newImmOpnd);
          curbb->AppendInsn(insn);
          insn = cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, vreg, base, tmpreg);
          curbb->AppendInsn(insn);
        } else {
          if (val == 0) {
            vreg = static_cast<RegOperand *>(base);
          } else {
            Insn *insn = cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, vreg, base, offOpnd);
            curbb->AppendInsn(insn);
          }
        }
        opndvec.push_back(vreg);  // param 1
        vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        Riscv64ImmOperand *sizeOpnd = CreateImmOperand(numElems * alignused, 64, false);
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, vreg, sizeOpnd));
        opndvec.push_back(vreg);  // param 2
        SelectLibCall("memcpy", opndvec, PTY_a64, PTY_a64);
        return;
      }
      for (uint32 i = 0; i < numElems; i++) {
        // generate the load
        if (parmCopy) {
          rhsmemopnd = LoadStructCopyBase(rhssymbol, rhsoffset + i * alignused, alignused * 8);
        } else {
          rhsmemopnd = GetOrCreateMemOpnd(rhssymbol, rhsoffset + i * alignused, alignused * 8);
        }
        regno_t vRegNo = New_V_Reg(kRegTyInt, std::max(4u, alignused));
        RegOperand *result = CreateVirtualRegisterOperand(vRegNo);
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickLdInsn(alignused * 8, PTY_u32), result, rhsmemopnd));
        if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(rhsmemopnd), alignused * 8)) {
          curbb->lastinsn->opnds[1] =
              SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(rhsmemopnd), alignused * 8, kRinvalid, curbb->lastinsn);
        }
        // generate the store
        Riscv64OfstOperand *offopnd = GetOrCreateOfstOpnd(lhsoffset + i * alignused, 32);
        lhsmemopnd =
          GetOrCreateMemOpnd(alignused * 8,
                             static_cast<Riscv64RegOperand *>(lhsaddropnd), nullptr, offopnd,
                             static_cast<MIRSymbol *>(nullptr));
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickStInsn(alignused * 8, PTY_u32), result, lhsmemopnd));
        if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(lhsmemopnd), alignused * 8)) {
          curbb->lastinsn->opnds[1] =
              SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(lhsmemopnd), alignused * 8, kRinvalid, curbb->lastinsn);
        }
      }
      // take care of extra content at the end less than the unit of alignused
      uint32 lhssizeCovered = (lhssize / alignused) * alignused;
      uint32 newalignused = alignused;
      while (lhssizeCovered < lhssize) {
        newalignused = newalignused >> 1;
        if (lhssizeCovered + newalignused > lhssize) {
          continue;
        }
        // generate the load
        rhsmemopnd = GetOrCreateMemOpnd(rhssymbol, rhsoffset + lhssizeCovered, newalignused * 8);
        regno_t vRegNo = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
        Operand *result = CreateVirtualRegisterOperand(vRegNo);
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickLdInsn(newalignused * 8, PTY_u32), result, rhsmemopnd));
        if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(rhsmemopnd), newalignused * 8)) {
          curbb->lastinsn->opnds[1] =
              SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(rhsmemopnd), newalignused * 8, kRinvalid, curbb->lastinsn);
        }
        // generate the store
        Riscv64OfstOperand *offopnd = GetOrCreateOfstOpnd(lhsoffset + lhssizeCovered, 32);
        lhsmemopnd =
          GetOrCreateMemOpnd(newalignused * 8,
                             static_cast<Riscv64RegOperand *>(lhsaddropnd), nullptr, offopnd,
                             static_cast<MIRSymbol *>(nullptr));
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickStInsn(newalignused * 8, PTY_u32), result, lhsmemopnd));
        lhssizeCovered += newalignused;
        if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(lhsmemopnd), newalignused * 8)) {
          curbb->lastinsn->opnds[1] =
              SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(lhsmemopnd), newalignused * 8, kRinvalid, curbb->lastinsn);
        }
      }
    }
  } else {  // rhs is iread
    CG_ASSERT(stmt->rhs->op == OP_iread, "SelectAggDassign: NYI");
    IreadNode *rhsiread = static_cast<IreadNode *>(stmt->rhs);
    RegOperand *rhsaddropnd = static_cast<RegOperand *>(HandleExpr(rhsiread, rhsiread->Opnd(0)));
    rhsaddropnd = LoadIntoRegister(rhsaddropnd, rhsiread->Opnd(0)->primType);
    MIRPtrType *rhspointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(rhsiread->tyIdx));
    MIRType *rhsty = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(rhspointerty->pointedTyIdx));
    bool isRefField = false;
    if (rhsiread->fieldID != 0) {
      MIRStructType *rhsstructty = static_cast<MIRStructType *>(rhsty);
      CG_ASSERT(rhsstructty, "SelectAggDassign: non-zero fieldID for non-structure");
      FieldPair thepair = rhsstructty->TraverseToField(rhsiread->fieldID);
      rhsty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(rhsstructty, rhsiread->fieldID).first;
      isRefField = becommon.IsRefField(rhsstructty, rhsiread->fieldID);
    }
    if (stmtty->primType == PTY_agg) {
      // generate move to regs.
      CHECK_FATAL(lhssize <= 16, "SelectAggIassign: illegal struct size");
      RegOperand *result[2];
      uint32 loadSize = (lhssize <= 4) ? 4 : 8;
      uint32 num = (lhssize <= 8) ? 1 : 2;
      for (uint32 i = 0; i < num; i++) {
        Riscv64OfstOperand *rhsoffopnd = GetOrCreateOfstOpnd(rhsoffset + i * loadSize, loadSize * BITS_PER_BYTE);
        Operand *rhsmemopnd =
             GetOrCreateMemOpnd(loadSize * BITS_PER_BYTE,
                           static_cast<Riscv64RegOperand *>(rhsaddropnd), nullptr, rhsoffopnd,
                           static_cast<MIRSymbol *>(nullptr));
        regno_t vRegNo = New_V_Reg(kRegTyInt, loadSize);
        result[i] = CreateVirtualRegisterOperand(vRegNo);
        Insn *ld = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(loadSize * BITS_PER_BYTE, PTY_u32), result[i], rhsmemopnd);
        ld->MarkAsAccessRefField(isRefField);
        curbb->AppendInsn(ld);
        if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(rhsmemopnd), loadSize * 8)) {
          curbb->lastinsn->opnds[1] =
              SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(rhsmemopnd), loadSize * 8, kRinvalid, curbb->lastinsn);
        }
      }
      for (uint32 i = 0; i < num; i++) {
        Riscv64reg_t preg = (i == 0 ? R10 : R11);
        RegOperand *dest = GetOrCreatePhysicalRegisterOperand(preg, loadSize * BITS_PER_BYTE, kRegTyInt);
        Insn *mov = cg->BuildInstruction<Riscv64Insn>(MOP_xmovrr, dest, result[i]);
        curbb->AppendInsn(mov);
      }
      for (uint32 i = 0; i < num; i++) {
        Riscv64reg_t preg = (i == 0 ? R10 : R11);
        RegOperand *dest = GetOrCreatePhysicalRegisterOperand(preg, loadSize * BITS_PER_BYTE, kRegTyInt);
        Insn *pseudo = cg->BuildInstruction<Riscv64Insn>(MOP_pseudo_ret_int, dest);
        curbb->AppendInsn(pseudo);
      }
    } else {
      rhsalign = becommon.type_align_table[rhsty->tyIdx.GetIdx()];
      if (GetPrimTypeSize(stmtty->primType) < rhsalign) {
        rhsalign = GetPrimTypeSize(stmtty->primType);
      }

      alignused = std::min(lhsalign, rhsalign);
      Operand *rhsmemopnd = nullptr;
      Operand *lhsmemopnd = nullptr;
      for (uint32 i = 0; i < (lhssize / alignused); i++) {
        // generate the load
        Riscv64OfstOperand *rhsoffopnd = GetOrCreateOfstOpnd(rhsoffset + i * alignused, 32);
        rhsmemopnd =
          GetOrCreateMemOpnd(alignused * 8,
                             static_cast<Riscv64RegOperand *>(rhsaddropnd), nullptr, rhsoffopnd,
                             static_cast<MIRSymbol *>(nullptr));
        regno_t vRegNo = New_V_Reg(kRegTyInt, std::max(4u, alignused));
        RegOperand *result = CreateVirtualRegisterOperand(vRegNo);
        Insn *insn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(alignused * 8, PTY_u32), result, rhsmemopnd);
        insn->MarkAsAccessRefField(isRefField);
        curbb->AppendInsn(insn);
        if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(rhsmemopnd), alignused * 8)) {
          curbb->lastinsn->opnds[1] =
              SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(rhsmemopnd), alignused * 8, kRinvalid, curbb->lastinsn);
        }
        // generate the store
        Riscv64OfstOperand *lhsoffopnd = GetOrCreateOfstOpnd(lhsoffset + i * alignused, 32);
        lhsmemopnd =
          GetOrCreateMemOpnd(alignused * 8,
                             static_cast<Riscv64RegOperand *>(lhsaddropnd), nullptr, lhsoffopnd,
                             static_cast<MIRSymbol *>(nullptr));
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickStInsn(alignused * 8, PTY_u32), result, lhsmemopnd));
        if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(lhsmemopnd), alignused * 8)) {
          curbb->lastinsn->opnds[1] =
              SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(lhsmemopnd), alignused * 8, kRinvalid, curbb->lastinsn);
        }
      }
      // take care of extra content at the end less than the unit of alignused
      uint32 lhssizeCovered = (lhssize / alignused) * alignused;
      uint32 newalignused = alignused;
      while (lhssizeCovered < lhssize) {
        newalignused = newalignused >> 1;
        if (lhssizeCovered + newalignused > lhssize) {
          continue;
        }
        // generate the load
        Riscv64OfstOperand *rhsoffopnd = GetOrCreateOfstOpnd(rhsoffset + lhssizeCovered, 32);
        rhsmemopnd =
          GetOrCreateMemOpnd(newalignused * 8,
                             static_cast<Riscv64RegOperand *>(rhsaddropnd), nullptr, rhsoffopnd,
                             static_cast<MIRSymbol *>(nullptr));
        regno_t vRegNo = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
        RegOperand *result = CreateVirtualRegisterOperand(vRegNo);
        Insn *insn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(newalignused * 8, PTY_u32), result, rhsmemopnd);
        insn->MarkAsAccessRefField(isRefField);
        curbb->AppendInsn(insn);
        if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(rhsmemopnd), newalignused * 8)) {
          curbb->lastinsn->opnds[1] =
              SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(rhsmemopnd), newalignused * 8, (Riscv64reg_t)kRinvalid, curbb->lastinsn);
        }
        // generate the store
        Riscv64OfstOperand *lhsoffopnd = GetOrCreateOfstOpnd(lhsoffset + lhssizeCovered, 32);
        lhsmemopnd =
          GetOrCreateMemOpnd(newalignused * 8,
                             static_cast<Riscv64RegOperand *>(lhsaddropnd), nullptr, lhsoffopnd,
                             static_cast<MIRSymbol *>(nullptr));
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickStInsn(newalignused * 8, PTY_u32), result, lhsmemopnd));
        lhssizeCovered += newalignused;
        if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(lhsmemopnd), newalignused * 8)) {
          curbb->lastinsn->opnds[1] =
              SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(lhsmemopnd), newalignused * 8, kRinvalid, curbb->lastinsn);
        }
      }
    }
  }
}

Operand *Riscv64CGFunc::SelectCopyToVecRegister(Operand *srcOpnd, PrimType dPtyp, PrimType sPtyp) {
  CG_ASSERT(GetPrimTypeBitSize(sPtyp) == srcOpnd->GetSize(), "");
  Operand *dRegopnd = srcOpnd;
  if (srcOpnd->op_kind_ != Operand::Opd_Register) {
    dRegopnd = CreateRegisterOperandOfType(sPtyp);
    SelectCopy(dRegopnd, sPtyp, srcOpnd, sPtyp);
  }
  RegOperand *dVecopnd = CreateRegisterOperandOfType(dPtyp);
  CG_ASSERT(GetPrimTypeBitSize(dPtyp) == 128, "NYI");
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickVecDup(sPtyp), dVecopnd, dRegopnd));
  return dVecopnd;
}

Operand *Riscv64CGFunc::SelectDread(BaseNode *parent, DreadNode *expr) {
  MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(expr->stIdx);
  bool unionTy = false;
  if (symbol->IsEhIndex()) {
    // use the second register return by __builtin_eh_return().
    ReturnMechanism retmech(GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_i32), becommon);
    retmech.SetupSecondRetReg(GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_i32));
    return GetOrCreatePhysicalRegisterOperand(retmech.reg1, 64, kRegTyInt);
    ;
  }

  PrimType symty = symbol->GetType()->primType;
  int32 offset = 0;
  bool parmCopy = false;
  if (expr->fieldID != 0) {
    MIRStructType *structty = static_cast<MIRStructType *>(symbol->GetType());
    if (structty->typeKind == kTypeUnion) {
      unionTy = true;
    }
    CG_ASSERT(structty, "SelectDread: non-zero fieldID for non-structure");
    FieldPair thepair = structty->TraverseToField(expr->fieldID);
    symty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first)->primType;
    offset = becommon.GetFieldOffset(structty, expr->fieldID).first;
    parmCopy = IsParamStructCopy(symbol);
  }
  int datasize = GetPrimTypeSize(symty) * BITS_PER_BYTE;
  uint32 aggsize = 0;
  if (symty == PTY_agg) {
    if (expr->primType == PTY_agg) {
      aggsize = becommon.type_size_table.at(symbol->GetType()->tyIdx.GetIdx());
      datasize = SIZEOFPTR;
    } else {
      datasize = GetPrimTypeSize(expr->primType) * BITS_PER_BYTE;
    }
  }
  MemOperand *memopnd;
  if (aggsize > 8) {
    if (parent->op == OP_eval) {
      if (symbol->typeAttrs.GetAttr(ATTR_volatile)) {
        // Need to generate loads for the upper parts of the struct.
        Operand *dest = Riscv64RegOperand::GetZeroRegister(SIZEOFPTR * BITS_PER_BYTE);
        uint32 num = aggsize >> 3;
        for (uint32 o = 0; o < num; ++o) {
          if (parmCopy) {
            memopnd = LoadStructCopyBase(symbol, offset + o * SIZEOFPTR , SIZEOFPTR);
          } else {
            memopnd = GetOrCreateMemOpnd(symbol, offset + o * SIZEOFPTR, SIZEOFPTR);
          }
          if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(memopnd), SIZEOFPTR)) {
            memopnd = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(memopnd), SIZEOFPTR);
          }
          SelectCopy(dest, PTY_u64, memopnd, PTY_u64);
        }
      } else {
        // No need to generate anything for eval.
      }
    } else {
      CHECK_FATAL(0,"SelectDread: Illegal agg size");
    }
  }
  if (parmCopy) {
    memopnd = LoadStructCopyBase(symbol, offset, datasize);
  } else {
    memopnd = GetOrCreateMemOpnd(symbol, offset, datasize);
  }
  CHECK_FATAL(static_cast<Riscv64MemOperand *>(memopnd), "null ptr check");
  if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(memopnd), datasize)) {
    memopnd = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(memopnd), datasize);
  }
  if (IsPrimitiveVector(expr->primType)) {
    return SelectCopyToVecRegister(memopnd, expr->primType, symty);
  }
  if (symty == PTY_agg) {
    return memopnd;
  }

  Operand *desopnd = LoadIntoRegister(memopnd, symty);
  PrimType dtype = expr->primType;
  if (dtype != symty) {
    uint32 loadSz = GetPrimTypeSize(symty);
    uint32 destSz = GetPrimTypeSize(dtype);
    if (loadSz > destSz) {
      return desopnd;
    }
    bool signExt = false;
    uint32 bitSz = 0;
    switch (symty) {
    case PTY_u8:
      if (dtype == PTY_i8 || dtype == PTY_i16 || dtype == PTY_i32 || dtype == PTY_i64) {
        signExt = true;
        bitSz = 8;
      }
      break;
    case PTY_u16:
      if (dtype == PTY_i16 || dtype == PTY_i32 || dtype == PTY_i64) {
        signExt = true;
        bitSz = 16;
      }
      break;
    case PTY_u32:
      if (dtype == PTY_i32 || dtype == PTY_i64) {
        signExt = true;
        bitSz = 32;
      }
      break;
    case PTY_i8:
      if (dtype == PTY_u8 || dtype == PTY_u16 || dtype == PTY_u32 || dtype == PTY_u64) {
        bitSz = 8;
      } else if (unionTy && (dtype == PTY_i16 || dtype == PTY_i32 || dtype == PTY_i64)) {
        bitSz = 8;
        signExt = true;
      }
      break;
    case PTY_i16:
      if (dtype == PTY_u16 || dtype == PTY_u32 || dtype == PTY_u64) {
        bitSz = 16;
      } else if (unionTy && (dtype == PTY_i32 || dtype == PTY_i64)) {
        bitSz = 16;
        signExt = true;
      }
      break;
    case PTY_i32:
      if (dtype == PTY_u32 || dtype == PTY_u64) {
        bitSz = 32;
      } else if (unionTy && (dtype == PTY_i64)) {
        bitSz = 32;
        signExt = true;
      }
      break;
    default:
      break;
    }
    if (bitSz != 0) {
      Operand *extopnd = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, GetPrimTypeSize(dtype)));
      if (signExt) {
        return GenerateSext(extopnd, desopnd, dtype, bitSz);
      } else {
        return GenerateZext(extopnd, desopnd, dtype, bitSz);
      }
    }
  }

  return desopnd;
}

RegOperand *Riscv64CGFunc::SelectRegread(BaseNode *parent, RegreadNode *expr) {
  PregIdx pregidx = expr->regIdx;
  if (IsSpecialPseudoRegister(pregidx)) {
    // if it is one of special registers
    pregidx = GetSpecialPseudoRegisterIndex(pregidx);
    RegOperand *reg = GetOrCreateSpecialRegisterOperand(pregidx, expr->primType);
    if (pregidx == kSregRetval0) {
      CG_ASSERT(curbb->lastinsn->IsCall(), "Dangling regread(SREG_retreg0)?");
      CG_ASSERT(call_info_map.find(curbb->lastinsn) != call_info_map.end(), "Dangling regread(SREG_retreg0)?");
      CSR_call_info_t &ci = call_info_map[curbb->lastinsn];
      CG_ASSERT(reg->GetRegisterType() == kRegTyInt || reg->GetRegisterType() == kRegTyFloat, "");
      Riscv64reg_t rn = reg->GetRegisterType() == kRegTyInt ? Riscv64Abi::kIntRetReg0 : Riscv64Abi::kFpRetReg0;
      CallerSavedRegHandler::CsrBitsetSet(ci.regs_used, Riscv64CallerSavedRegHandler::Reg2BitPos(rn));
    }
    return reg;
  } else {
    RegOperand *reg = GetOrCreateVirtualRegisterOperand(GetVirtualRegNoFromPseudoRegIdx(pregidx));
    if (g->optim_level == 0) {
      MemOperand *src = GetPseudoRegisterSpillMemoryOperand(pregidx);
      PrimType stype = expr->primType;//GetTypeFromPseudoRegIdx(pregidx);
      MIRPreg *preg = func->pregTab->PregFromPregIdx(pregidx);
      int32 bytelen = GetPrimTypeSize(stype);
      uint32_t srcbitlen = bytelen * BITS_PER_BYTE;
      RegType regty = GetRegTyFromPrimTyRiscv64(stype);
      RegOperand *vreg =  CreateVirtualRegisterOperand(New_V_Reg(regty, bytelen));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickLdInsn(srcbitlen, stype), vreg, src));
      return vreg;
    }
    return reg;
  }
}

void Riscv64CGFunc::SelectAddrof(Operand *result, StImmOperand *stimm) {
  MIRSymbol *symbol = stimm->GetSymbol();
  if (symbol->storageClass == kScAuto || symbol->storageClass == kScFormal) {
    if (!cg->DoItQuietly()) {
      fprintf(stderr, "Warning: we expect AddrOf with StImmOperand is not used for local variables");
    }
    Riscv64SymbolAlloc *symloc = static_cast<Riscv64SymbolAlloc *>(memlayout->sym_alloc_table.at(symbol->GetStIndex()));
    Riscv64ImmOperand *offset = nullptr;
    if (symloc->mem_segment->kind == kMsArgsStkpassed) {
      offset = CreateImmOperand(GetBaseOffset(symloc) + stimm->GetOffset(), 64, false, true);
    } else if (symloc->mem_segment->kind == kMsReflocals) {
      auto it = immopnds_requiring_offset_adjustment_for_refloc_.find(symloc);
      if (it != immopnds_requiring_offset_adjustment_for_refloc_.end()) {
        offset = (*it).second;
      } else {
        offset = CreateImmOperand(GetBaseOffset(symloc) + stimm->GetOffset(), 64, false);
        immopnds_requiring_offset_adjustment_for_refloc_[symloc] = offset;
      }
    } else if (mirModule.IsJavaModule()) {
      auto it = immopnds_requiring_offset_adjustment_.find(symloc);
      if (it != immopnds_requiring_offset_adjustment_.end()) {
        offset = (*it).second;
      } else {
        offset = CreateImmOperand(GetBaseOffset(symloc) + stimm->GetOffset(), 64, false);
        if (symbol->GetType()->typeKind != kTypeClass) {
          immopnds_requiring_offset_adjustment_[symloc] = offset;
        }
      }
    } else {
      // Do not cache modified symbol location
      offset = CreateImmOperand(GetBaseOffset(symloc) + stimm->GetOffset(), 64, false);
    }

    SelectAdd(result, GetBaseReg(symloc), offset, PTY_u64);
    if (cg->GenerateVerboseAsm()) {
      // Add a comment
      Insn *insn = curbb->lastinsn;
      std::string comm = "local/formal var: ";
      comm.append(symbol->GetName());
      insn->AddComment(comm);
    }
  } else {
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_adrp, result, stimm));
    if (CGOptions::doPIC && (symbol->GetStorageClass() == kScGlobal || symbol->GetStorageClass() == kScExtern)) {
      // ldr     x0, [x0, #:got_lo12:Ljava_2Flang_2FSystem_3B_7Cout]
      Riscv64MemOperand *mo = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE,
                                                 static_cast<Riscv64RegOperand *>(result), nullptr, stimm, nullptr);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xldr, result, mo));
    } else {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_adrpl12, result, result, stimm));
    }
  }
}

void Riscv64CGFunc::SelectAddrof(Operand *result, Riscv64MemOperand *memopnd) {
  MIRSymbol *symbol = memopnd->GetSymbol();
  if (symbol->storageClass == kScAuto) {
    SelectAdd(result, memopnd->GetBaseRegister(),
              CreateImmOperand(static_cast<Riscv64OfstOperand *>(memopnd->GetOffsetImmediate())->GetOffsetValue(),
                               PTY_u32, false),
              PTY_u32);
  } else {
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_adrp, result, memopnd));
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_adrpl12, result, result, memopnd));
  }
}

Operand *Riscv64CGFunc::SelectAddrof(AddrofNode *expr) {
  MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(expr->stIdx);
  int32 offset = 0;
  if (expr->fieldID != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(symbol->GetType());
    if (structty) {
      CG_ASSERT(structty, "SelectAddrof: non-zero fieldID for non-structure");
      offset = becommon.GetFieldOffset(structty, expr->fieldID).first;
    }
  }
  if ((symbol->storageClass == kScFormal) && (symbol->sKind == kStVar) &&
      ((expr->fieldID != 0) ||
       (becommon.type_size_table.at(symbol->GetType()->tyIdx.GetIdx()) > 16))) {
    // Struct param is copied on the stack by caller if struct size > 16.
    // Else if size < 16 then struct param is copied into one or two registers.
    regno_t vregno;
    vregno = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
    Operand *stackAddr = CreateVirtualRegisterOperand(vregno);
    // load the base address of the struct copy from stack.
    SelectAddrof(stackAddr, CreateStImmOperand(symbol, 0, 0));
    Operand *structAddr;
    if (becommon.type_size_table.at(symbol->GetType()->tyIdx.GetIdx()) <= 16) {
      isAggParamInReg = true;
      structAddr = stackAddr;
    } else {
      Riscv64OfstOperand *offopnd = CreateOfstOpnd(0, 32);
      Riscv64MemOperand *
        mo = GetOrCreateMemOpnd( SIZEOFPTR * BITS_PER_BYTE,
                                 static_cast<RegOperand *>(stackAddr),
                                 nullptr, offopnd, nullptr);
      vregno = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
      structAddr = CreateVirtualRegisterOperand(vregno);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xldr, structAddr, mo));
    }
    if (offset == 0) {
      return structAddr;
    } else {
      // add the struct offset to the base address
      vregno = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
      Operand *result = CreateVirtualRegisterOperand(vregno);
      ImmOperand *imm = CreateImmOperand(PTY_a64, offset);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, result,
                               structAddr, imm));
      return result;
    }
  }
  PrimType primType = expr->primType;
  regno_t vRegNo = New_V_Reg(kRegTyInt, GetPrimTypeSize(primType));
  Operand *result = CreateVirtualRegisterOperand(vRegNo);
  if (symbol->IsReflectionClassInfo() && !symbol->IsReflectionArrayClassInfo() && !isLibcore) {
    // Turn addrof __cinf_X  into a load of _PTR__cinf_X
    //  adrp    x1, _PTR__cinf_Ljava_2Flang_2FSystem_3B
    //  ldr     x1, [x1, #:lo12:_PTR__cinf_Ljava_2Flang_2FSystem_3B]
    std::string ptrName = NameMangler::kPtrPrefixStr + symbol->GetName();
    MIRType *ptrType = GlobalTables::GetTypeTable().GetPtr();
    symbol = mirModule.mirBuilder->GetOrCreateGlobalDecl(ptrName, ptrType);
    symbol->storageClass = kScFstatic;

    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_adrp_ldr, result, CreateStImmOperand(symbol, 0, 0)));
    return result;
  }

  SelectAddrof(result, CreateStImmOperand(symbol, offset, 0));
  return result;
}

Operand *Riscv64CGFunc::SelectAddroffunc(AddroffuncNode *expr) {
  regno_t vRegNo = New_V_Reg(kRegTyInt, expr->SizeOfInstr());
  Operand *operand = CreateVirtualRegisterOperand(vRegNo);
  MIRFunction *func = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(expr->puIdx);
  CG_ASSERT(func, "function not found for 'addroffunc'");
  SelectAddrof(operand, CreateStImmOperand(func->GetFuncSymbol(), 0, 0));
  return operand;
}

Operand *Riscv64CGFunc::SelectIread(BaseNode *parent, IreadNode *expr) {
  int32 offset = 0;
  MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(expr->tyIdx);
  MIRPtrType *pointerty = static_cast<MIRPtrType *>(type);
  CG_ASSERT(pointerty, "expect a pointer type at iread node");
  MIRType *pointedType = nullptr;
  bool isRefField = false;
  Riscv64isa::memory_ordering_t mo = Riscv64isa::kMoNone;

  if (expr->fieldID != 0) {
    MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
    MIRStructType *structty = nullptr;
    if (pointedty->GetKind() != kTypeJArray) {
      structty = static_cast<MIRStructType *>(pointedty);
      CG_ASSERT(structty != nullptr, "structty is null in Riscv64CGFunc::SelectIread");
    } else {
      // it's a Jarray type. using it's parent's field info: java.lang.Object
      structty = static_cast<MIRJarrayType *>(pointedty)->GetParentType();
    }

    CG_ASSERT(structty, "SelectIread: non-zero fieldID for non-structure");
    FieldPair thepair = structty->TraverseToField(expr->fieldID);
    pointedType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
    offset = becommon.GetFieldOffset(structty, expr->fieldID).first;
    isRefField = becommon.IsRefField(structty, expr->fieldID);
  } else {
    pointedType = GetPointedToType(pointerty);
    if (func->IsJava() && (pointedType->GetKind() == kTypePointer)) {
      MIRType *nextPointedType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(pointedType)->pointedTyIdx);
      if (nextPointedType->GetKind() != kTypeScalar) {
        isRefField = true;  // read from an object array, or an high-dimentional array
      }
    }
  }

  RegType regty;
  if (expr->primType == PTY_agg) {
    regty = kRegTyInt;
  } else {
    regty = GetRegTyFromPrimTyRiscv64(expr->primType);
  }
  uint32 regsize = GetPrimTypeSize(expr->primType);
  if (expr->fieldID == 0 && pointedType->primType == PTY_agg) {
    if (regty == kRegTyFloat) {
      // regsize is correct
    } else if (becommon.type_size_table.at(pointedType->tyIdx.GetIdx()) <= 4) {
      regsize = 4;
    } else {
      regsize = 8;
    }
  } else {
    if (regsize < 4) {
      regsize = 4;  // 32-bit
    }
  }

  PrimType destType = pointedType->GetPrimType();

  uint32 bitsize = 0;
  uint32 numLoads = 1;
  if (pointedType->typeKind == kTypeStructIncomplete || pointedType->typeKind == kTypeClassIncomplete ||
      pointedType->typeKind == kTypeInterfaceIncomplete) {
    bitsize = GetPrimTypeBitSize(expr->primType);
    fprintf(stderr, "Warning: objsize is zero! \n");
  } else {
    MIRStructType *structtype = dynamic_cast<MIRStructType *>(pointedType);
    if (structtype) {
      bitsize = structtype->GetSize() << 3;
    } else {
      bitsize = GetPrimTypeBitSize(destType);
    }
    if (regty == kRegTyFloat) {
      destType = expr->primType;
      bitsize = GetPrimTypeBitSize(destType);
    } else if (expr->fieldID == 0 && destType == PTY_agg) {  // entire struct
      switch (bitsize) {
      case 8:
        destType = PTY_u8;
        break;
      case 16:
        destType = PTY_u16;
        break;
      case 32:
        destType = PTY_u32;
        break;
      case 64:
        destType = PTY_u64;
        break;
      default:
        destType = PTY_u64;
        numLoads = RoundUp(bitsize, 64) / 64;
        bitsize = 64;
        break;
      }
    }
  }

  Operand *result;
#if 0
  if (parent->op == OP_eval) {
    result = Riscv64RegOperand::GetZeroRegister(regsize << 3);
  } else {
    result = CreateVirtualRegisterOperand(New_V_Reg(regty, regsize));
  }
#else
  result = CreateVirtualRegisterOperand(New_V_Reg(regty, regsize));
#endif

  for (uint32 i = 0; i < numLoads; ++i) {
    MemOperand *memopnd = CreateMemOpnd(destType, expr, expr->Opnd(0), offset, mo);
    if (aggParamReg) {
      isAggParamInReg = false;
      return aggParamReg;
    }
    if (isVolLoad) {
      mo = Riscv64isa::kMoAcquire;
      isVolLoad = false;
    }

    if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(memopnd), bitsize)) {
      memopnd = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(memopnd), bitsize);
    }

    if (mo == Riscv64isa::kMoNone) {
      MOperator mop = PickLdInsn(bitsize, destType);
      Insn *insn = cg->BuildInstruction<Riscv64Insn>(mop, result, memopnd);
      if (parent->op == OP_eval && result->IsRegister() && static_cast<Riscv64RegOperand *>(result)->IsZeroRegister()) {
        insn->AddComment("null-check");
      }
      curbb->AppendInsn(insn);
    } else {
      Riscv64CGFunc::SelectLoadAcquire(result, destType, memopnd, destType, mo, false);
    }
    curbb->lastinsn->MarkAsAccessRefField(isRefField);
    offset += 8;
  }
  return result;
}

Operand *Riscv64CGFunc::SelectIntconst(MIRIntConst *intconst, PrimType parentPtype) {
  PrimType pty = intconst->type->primType;
  if (pty != parentPtype) {
    // Maple IR allows silent type conversion between sign/unsigned if size is the same.
    if (GetPrimTypeSize(pty) == GetPrimTypeSize(parentPtype)) {
      pty = parentPtype;
    }
  }
  bool isSigned;
  if (pty == PTY_i8 || pty == PTY_i16 || pty == PTY_i32 || pty == PTY_i64) {
    isSigned = true;
  } else {
    isSigned = false;
  }
  int64 val = intconst->value;
  if (isSigned == false && GetPrimTypeSize(pty) == 4) {
    val = static_cast<uint64>(val) & 0x0ffffffffULL;
  }
  return CreateImmOperand(val, GetPrimTypeSize(intconst->type->GetPrimType()) * 8, isSigned);
}

template <typename T>
Operand *SelectLiteral(T *c, MIRFunction *func, uint32 labelIdx, Riscv64CGFunc *cgfunc) {
  MIRSymbol *st = func->symTab->CreateSymbol(kScopeLocal);
  std::string lblstr(".LB_");
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  std::string funcname = funcSt->GetName();
  lblstr.append(funcname).append(to_string(labelIdx));
  st->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lblstr));
  st->storageClass = kScPstatic;
  st->sKind = kStConst;
  st->SetConst(c);
  PrimType primty = c->type->GetPrimType();
  st->SetTyIdx(c->type->tyIdx);
  uint32 typeBitsize = GetPrimTypeBitSize(primty);

  switch (c->kind) {
    case kConstFloatConst:
    case kConstDoubleConst: {
      // Handling of -0.0.  Use zero reg only for +0.0, not -0.0.
      if (cgfunc->mirModule.IsCModule()) {
        return static_cast<Operand *>(cgfunc->GetOrCreateMemOpnd(st, 0, typeBitsize));
      }
      return (c->IsZero() && !(c->IsNeg())) ? static_cast<Operand *>(cgfunc->GetOrCreateFpZeroOperand(typeBitsize))
                       : static_cast<Operand *>(cgfunc->GetOrCreateMemOpnd(st, 0, typeBitsize));
    }
    case kConstVecInt: {
      return static_cast<Operand *>(cgfunc->GetOrCreateMemOpnd(st, 0, typeBitsize));
    }
    default: {
      ASSERT(0, "Unsupported const type");
      return nullptr;
    }
  }
}

Operand *Riscv64CGFunc::SelectFloatconst(MIRFloatConst *floatconst) {
  return SelectLiteral(floatconst, func, labelIdx++, this);
}

Operand *Riscv64CGFunc::SelectDoubleconst(MIRDoubleConst *doubleconst) {
  return SelectLiteral(doubleconst, func, labelIdx++, this);
}

Operand *Riscv64CGFunc::SelectVectorIntconst(MIRVectorIntConst *vecIntconst) {
  return SelectLiteral(vecIntconst, func, labelIdx++, this);
}

template <typename T>
Operand *SelectStrLiteral(T *c, Riscv64CGFunc *cgfunc) {
  std::string labelStr;
  if (c->kind == kConstStrConst) {
    labelStr.append("__Ustr_");
  } else if (c->kind == kConstStr16Const) {
    labelStr.append("__Ustr16_");
  } else {
    ASSERT(0, "Unsupported literal type");
  }
  labelStr.append(std::to_string(c->value.GetIdx()));

  MIRSymbol *labelSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(labelStr));
  if (!labelSym) {
    labelSym = cgfunc->mirModule.mirBuilder->CreateGlobalDecl(labelStr, c->type, kScGlobal);
    labelSym->storageClass = kScFstatic;
    labelSym->sKind = kStConst;
    // c may be local, we need a global node here
    labelSym->SetConst(cgfunc->mirModule.memPool->New<T>(c->value, c->type));
  }

  if (c->kPrimType == PTY_a64) {
    StImmOperand *stopnd = cgfunc->CreateStImmOperand(labelSym, 0, 0);
    RegOperand *addropnd = cgfunc->CreateRegisterOperandOfType(PTY_a64);
    cgfunc->SelectAddrof(addropnd, stopnd);
    return addropnd;
  } else {
    ASSERT(0, "Unsupported const string type");
    return nullptr;
  }
}

Operand *Riscv64CGFunc::SelectStrconst(MIRStrConst *strconst) {
  return SelectStrLiteral(strconst, this);
}

Operand *Riscv64CGFunc::SelectStr16const(MIRStr16Const *str16const) {
  return SelectStrLiteral(str16const, this);
}

}  // namespace maplebe
