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

#include "aarch64_operand.h"
#include "aarch64_abi.h"
#include "aarch64_cg_func.h"
#include "aarch64_cg.h"
#include "cg_assert.h"
#include <iostream>
#include <string>

namespace maplebe {

AArch64RegOperand AArch64RegOperand::zero64(RZR, 64, kRegTyInt);
AArch64RegOperand AArch64RegOperand::zero32(RZR, 32, kRegTyInt);

const char *CondOperand::ccStrs[kCcLast] = {
#define CONDCODE(a) #a,
#include "aarch64_cc.def"
#undef CONDCODE
};

ImmFPZeroOperand *ImmFPZeroOperand::instance32 = nullptr;
ImmFPZeroOperand *ImmFPZeroOperand::instance64 = nullptr;

bool AArch64RegOperand::IsSaveReg(MIRType *type, BECommon &becommon) {
  ReturnMechanism retmech(type, becommon);
  if (retmech.regcount > 0) {
    return GetRegisterNumber() == retmech.reg0 || GetRegisterNumber() == retmech.reg1 ||
           GetRegisterNumber() == retmech.reg2 || GetRegisterNumber() == retmech.reg3;
  }
  return false;
}

void AArch64RegOperand::Emit(Emitter &emitter, OpndProp *prop) {
  CG_ASSERT((!prop || (static_cast<AArch64OpndProp *>(prop)->opnd_ty_ == Operand::Opd_Register)),
            "operand type doesn't match");
  // prop null means a sub emit, i.e from MemOperand
  uint8 size = prop ? static_cast<AArch64OpndProp *>(prop)->size_ : size_;
  switch (GetRegisterType()) {
    case kRegTyInt: {
      CG_ASSERT((size == 32 || size == 64), "illegal register size");
#ifdef USE_32BIT_REF
      bool r32 = (size == 32) || isreffield_;
#else
      bool r32 = (size == 32);
#endif  // USE_32BIT_REF
      emitter.Emit(AArch64CG::intRegNames[(r32 ? AArch64CG::kR32List : AArch64CG::kR64List)][reg_no_]);
      break;
    }
    case kRegTyFloat: {
      bool isSimdvec = (size == 128);
      CG_ASSERT((size == 8 || size == 16 || size == 32 || size == 64 || isSimdvec),
      "illegal register size");
      if (IsSimdVectorMode()) {
        // see if it is slot 0 or 1 of simd
        if (reg_no_ >= VB64) {
          reg_no_ = reg_no_ - VB64 + V0;
        } else if (reg_no_ >= VB32) {
          reg_no_ = reg_no_ - VB32 + V0;
        }
        if (isSimdvec) {
          const AArch64MD *md = &AArch64CG::kMd[emitter.GetCurrentMOP()];
          if (md->IsMemAccess()) {
            // emit things like   str q0, [x1] ldr q1, .LB_testx2
            emitter.Emit(AArch64CG::intRegNames[AArch64CG::kV128List][reg_no_]);
          } else {
            emitter.Emit(AArch64CG::intRegNames[(AArch64CG::kV64List)][reg_no_]);
            // emit things like dup v0.4s, w1; add v0.4s, v0.4s, v1.4s
            emitter.Emit(GetSimdVectorType() == kVecDouble ? ".2d" : ".4s");
          }
        } else { // not simd vector
          emitter.Emit(AArch64CG::intRegNames[(AArch64CG::kV64List)][reg_no_]);
          if (GetSimdVectorType() == kVecDouble) {
            // 2 * 64bits
            emitter.Emit(".d[");
            CG_ASSERT(0, "AArch64RegOperand::Emit - unsupported 128 bit simd");
          } else {
            emitter.Emit(".s[");
          }
          int pos = GetSimdVectorPosition();
          switch (pos) {
            case 0:
              emitter.Emit("0]");
              break;
            case 1:
              emitter.Emit("1]");
              break;
            case 2:
              emitter.Emit("2]");
              break;
            case 3:
              emitter.Emit("3]");
              break;
          }
        }
      } else {
        /*
         * FP reg cannot be reffield_.
         */
        //#ifdef USE_32BIT_REF
        //      bool r32 = (size == 32) || isreffield_;
        //      uint32_t regset = r32 ? AArch64CG::kR32List : (__builtin_ctz(size) - 3);
        //#else
        uint32_t regset = __builtin_ctz(size) - 3;
        //#endif //USE_32BIT_REF
        emitter.Emit(AArch64CG::intRegNames[regset][reg_no_]);
      }
      break;
    }
    default:
      CG_ASSERT(false, "NYI");
      break;
  }
}

const int32 AArch64MemOperand::kMaxPimms[4] = { AArch64MemOperand::kMaxPimm8, AArch64MemOperand::kMaxPimm16,
                                                AArch64MemOperand::kMaxPimm32, AArch64MemOperand::kMaxPimm64 };

bool AArch64RegOperand::IsSPOrFP() {
  if (!is_virtual && (reg_no_ == 30 || reg_no_ == 32)) {
    return true;
  } else {
    return false;
  }
}

Operand *AArch64MemOperand::GetOffset() {
  switch (addr_mode_) {
    case kAddrModeBOi:
      return GetOffsetOperand();
      break;
    case kAddrModeBOrX:
      return GetOffsetRegister();
      break;
    case kAddrModeLiteral:
      break;
    case kAddrModeLo12Li:
      break;
    default:
      CG_ASSERT(false, "error memoperand dump");
  }
  return nullptr;
}

void AArch64MemOperand::Emit(Emitter &emitter, OpndProp *prop) {
  AArch64OpndProp *opndprop = static_cast<AArch64OpndProp *>(prop);
  AArch64MemOperand::AArch64AddressingMode addrMode = GetAddrMode();
  const AArch64MD *md = &AArch64CG::kMd[emitter.GetCurrentMOP()];
  bool is64bit = md->Is64Bit();
  bool isVector = md->IsVector();
  bool isLDSTpair = md->IsLoadStorePair();
#if DEBUG
  CG_ASSERT(isVector || is64bit || md->GetOperandSize() <= 32, "");
#endif
  if (addrMode == AArch64MemOperand::kAddrModeBOi) {
    emitter.Emit("[");
    AArch64RegOperand *baseReg = static_cast<AArch64RegOperand *>(GetBaseRegister());
    CG_ASSERT(baseReg, "expect an AArch64RegOperand here");
    if (CGOptions::doPIC && (baseReg->GetSize() != 64)) {
      baseReg->SetSize(64);
    }
    baseReg->Emit(emitter, nullptr);
    AArch64OfstOperand *offset = GetOffsetImmediate();
    if (offset) {
#ifndef USE_32BIT_REF  // can be load a ref here
      CG_ASSERT((baseReg->GetRegisterNumber() != RSP) || !IsOffsetMisaligned(md->GetOperandSize()), "");
#endif  // USE_32BIT_REF
      if (IsPostIndexed()) {
        CG_ASSERT(!IsSIMMOffsetOutOfRange(offset->GetOffsetValue(), is64bit, isLDSTpair), "");
        emitter.Emit("]");
        if (!offset->IsZero()) {
          emitter.Emit(", ");
          offset->Emit(emitter, nullptr);
        }
      } else if (IsPreIndexed()) {
        CG_ASSERT(!IsSIMMOffsetOutOfRange(offset->GetOffsetValue(), is64bit, isLDSTpair), "");
        if (!offset->IsZero()) {
          emitter.Emit(",");
          offset->Emit(emitter, nullptr);
        }
        emitter.Emit("]!");
      } else {
        if (CGOptions::doPIC && offset->op_kind_ == Opd_StImmediate &&
            (((StImmOperand *)offset)->GetSymbol()->GetStorageClass() == kScGlobal ||
             ((StImmOperand *)offset)->GetSymbol()->GetStorageClass() == kScExtern)) {
          emitter.Emit(",");
          emitter.Emit("#:got_lo12:");
          emitter.Emit(((StImmOperand *)offset)->GetName());
        } else {
          CG_ASSERT(!IsPIMMOffsetOutOfRange(offset->GetOffsetValue(), size_), "");
          if (!offset->IsZero()) {
            emitter.Emit(",");
            offset->Emit(emitter, nullptr);
          }
        }
        emitter.Emit("]");
      }
    } else {
      emitter.Emit("]");
    }
  } else if (addrMode == AArch64MemOperand::kAddrModeBOrX) {
    // Base plus offset   | [base{, #imm}]  [base, Xm{, LSL #imm}]   [base, Wm, (S|U)XTW {#imm}]
    //                      offset_opnds=nullptr
    //                                      offset_opnds=64          offset_opnds=32
    //                                      imm=0 or 3               imm=0 or 2, s/u
    emitter.Emit("[");
    GetBaseRegister()->Emit(emitter, nullptr);
    emitter.Emit(",");
    GetOffsetRegister()->Emit(emitter, nullptr);
    if (ShouldEmitExtend()) {
      emitter.Emit(",");
      // extend, #0, of #3/#2
      emitter.Emit(GetExtendAsString());
      emitter.Emit(" #");
      emitter.Emit(ShiftAmount());
    }
    emitter.Emit("]");
  } else if (addrMode == AArch64MemOperand::kAddrModeLiteral) {
    if (opndprop->IsMemLow12()) {
      emitter.Emit("#:lo12:");
    }
    emitter.Emit(GetSymbol()->GetName());
  } else if (addrMode == AArch64MemOperand::kAddrModeLo12Li) {
    emitter.Emit("[");
    GetBaseRegister()->Emit(emitter, nullptr);

    AArch64OfstOperand *offset = GetOffsetImmediate();
    CG_ASSERT(offset, "");

    emitter.Emit(", #:lo12:");
    if (GetSymbol()->storageClass == kScPstatic) {
      emitter.Emit(GetSymbolName() + to_string(CG::curPuIdx));
    } else {
      emitter.Emit(GetSymbolName());
    }
    if (!offset->IsZero()) {
      emitter.Emit("+");
      emitter.Emit(to_string(offset->GetOffsetValue()));
    }
    emitter.Emit("]");
  } else {
    CG_ASSERT(false, "nyi");
  }
}

void AArch64MemOperand::dump() {
  LogInfo::MapleLogger() << "Mem:";
  switch (addr_mode_) {
    case kAddrModeBOi: {
      LogInfo::MapleLogger() << "base:";
      GetBaseRegister()->dump();
      LogInfo::MapleLogger() << "offset:";
      GetOffsetOperand()->dump();
      switch (idx_opt_) {
        case kIntact:
          LogInfo::MapleLogger() << "  intact";
          break;
        case kPreIndex:
          LogInfo::MapleLogger() << "  pre-index";
          break;
        case kPostIndex:
          LogInfo::MapleLogger() << "  post-index";
          break;
      }
      break;
    }
    case kAddrModeBOrX: {
      LogInfo::MapleLogger() << "base:";
      GetBaseRegister()->dump();
      LogInfo::MapleLogger() << "offset:";
      GetOffsetRegister()->dump();
      LogInfo::MapleLogger() << " " << GetExtendAsString();
      LogInfo::MapleLogger() << " shift: " << ShiftAmount();
      break;
    }
    case kAddrModeLiteral:
      LogInfo::MapleLogger() << "literal: " << GetSymbolName();
      break;
    case kAddrModeLo12Li: {
      LogInfo::MapleLogger() << "base:";
      GetBaseRegister()->dump();
      LogInfo::MapleLogger() << "offset:";
      AArch64OfstOperand *offopnd = GetOffsetImmediate();
      LogInfo::MapleLogger() << "#:lo12:";
      if (GetSymbol()->storageClass == kScPstatic) {
        LogInfo::MapleLogger() << GetSymbolName() << to_string(CG::curPuIdx);
      } else {
        LogInfo::MapleLogger() << GetSymbolName();
      }
      LogInfo::MapleLogger() << "+" << to_string(offopnd->GetOffsetValue());
      break;
    }
    default:
      CG_ASSERT(false, "error memoperand dump");
  }
}

bool AArch64MemOperand::Equals(Operand *operand) {
  if (!dynamic_cast<AArch64MemOperand *>(operand)) {
    return false;
  }
  AArch64MemOperand *op = static_cast<AArch64MemOperand *>(operand);
  return Equals(op);
}

bool AArch64MemOperand::Equals(AArch64MemOperand *op) {
  if (this == op) {
    return true;
  }

  if (addr_mode_ == op->GetAddrMode() &&
      GetBaseRegister()->GetRegisterNumber() == op->GetBaseRegister()->GetRegisterNumber() &&
      ((GetOffsetRegister() && op->GetOffsetRegister() &&
        GetOffsetRegister()->GetRegisterNumber() == op->GetOffsetRegister()->GetRegisterNumber()) ||
       (GetOffsetImmediate() && op->GetOffsetImmediate() &&
        GetOffsetImmediate()->GetValue() == op->GetOffsetImmediate()->GetValue()))) {
    return true;
  } else {
    return false;
  }
}

// sort the register operand according to their number
void AArch64ListOperand::Emit(Emitter &emitter, OpndProp *prop) {
  uint32 nLeft = vec_opnds_.size();
  if (nLeft == 0) {
    return;
  }

  auto it = vec_opnds_.begin();
  for (; it != vec_opnds_.end(); ++it) {
    (*it)->Emit(emitter, nullptr);
    if (--nLeft >= 1) {
      emitter.Emit(", ");
    }
  }
}

}  // namespace maplebe
