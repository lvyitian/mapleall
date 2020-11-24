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

#include "riscv64_operand.h"
#include "riscv64_abi.h"
#include "riscv64_cg_func.h"
#include "riscv64_cg.h"
#include "cg_assert.h"
#include <iostream>
#include <string>

namespace maplebe {

Riscv64RegOperand Riscv64RegOperand::zero64(RZERO, 64, kRegTyInt);
Riscv64RegOperand Riscv64RegOperand::zero32(RZERO, 32, kRegTyInt);

ImmFPZeroOperand *ImmFPZeroOperand::instance32 = nullptr;
ImmFPZeroOperand *ImmFPZeroOperand::instance64 = nullptr;

bool Riscv64RegOperand::IsSaveReg(MIRType *type, BECommon &becommon) {
  ReturnMechanism retmech(type, becommon);
  if (retmech.regcount > 0) {
    return GetRegisterNumber() == retmech.reg0 || GetRegisterNumber() == retmech.reg1;
  }
  return false;
}

void Riscv64RegOperand::Emit(Emitter &emitter, OpndProp *prop) {
  CG_ASSERT((!prop || (static_cast<Riscv64OpndProp *>(prop)->opnd_ty_ == Operand::Opd_Register)),
            "operand type doesn't match");
  // prop null means a sub emit, i.e from MemOperand
  uint8 size = prop ? static_cast<Riscv64OpndProp *>(prop)->size_ : size_;
  switch (GetRegisterType()) {
    case kRegTyInt: {
      CG_ASSERT((size == 32 || size == 64), "illegal register size");
#ifdef USE_32BIT_REF
      bool r32 = (size == 32) || isreffield_;
#else
      bool r32 = (size == 32);
#endif  // USE_32BIT_REF
      emitter.Emit(Riscv64CG::intRegNames[(r32 ? Riscv64CG::kR32List : Riscv64CG::kR64List)][reg_no_]);
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
          const Riscv64MD *md = &Riscv64CG::kMd[emitter.GetCurrentMOP()];
          if (md->IsMemAccess()) {
            // emit things like   str q0, [x1] ldr q1, .LB_testx2
            emitter.Emit(Riscv64CG::intRegNames[Riscv64CG::kV128List][reg_no_]);
          } else {
            emitter.Emit(Riscv64CG::intRegNames[(Riscv64CG::kV64List)][reg_no_]);
            // emit things like dup v0.4s, w1; add v0.4s, v0.4s, v1.4s
            emitter.Emit(GetSimdVectorType() == kVecDouble ? ".2d" : ".4s");
          }
        } else { // not simd vector
          emitter.Emit(Riscv64CG::intRegNames[(Riscv64CG::kV64List)][reg_no_]);
          if (GetSimdVectorType() == kVecDouble) {
            // 2 * 64bits
            emitter.Emit(".d[");
            CG_ASSERT(0, "Riscv64RegOperand::Emit - unsupported 128 bit simd");
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
        //      uint32_t regset = r32 ? Riscv64CG::kR32List : (__builtin_ctz(size) - 3);
        //#else
        uint32_t regset = __builtin_ctz(size) - 3;
        //#endif //USE_32BIT_REF
        emitter.Emit(Riscv64CG::intRegNames[regset][reg_no_]);
      }
      break;
    }
    default:
      CG_ASSERT(false, "NYI");
      break;
  }
}

bool Riscv64RegOperand::IsSPOrFP() {
  if (!is_virtual && (reg_no_ == 30 || reg_no_ == 32)) {
    return true;
  } else {
    return false;
  }
}

Operand *Riscv64MemOperand::GetOffset() {
  return GetOffsetOperand();
}

void Riscv64MemOperand::Emit(Emitter &emitter, OpndProp *prop) {
  Riscv64OpndProp *opndprop = static_cast<Riscv64OpndProp *>(prop);
  const Riscv64MD *md = &Riscv64CG::kMd[emitter.GetCurrentMOP()];
  bool is64bit = md->Is64Bit();
  bool isVector = md->IsVector();
  bool isLDSTpair = md->IsLoadStorePair();
#if DEBUG
  CG_ASSERT(isVector || is64bit || md->GetOperandSize() <= 32, "");
#endif
  MIRSymbol *symbol = GetSymbol();
  Riscv64OfstOperand *offset = GetOffsetImmediate();
  if (symbol && (symbol->IsGlobal() || symbol->storageClass == kScPstatic ||
                 symbol->storageClass == kScFstatic)) {
    emitter.Emit("%lo(");
    // check for sKind since it might be created by cg. (i.eg. LB_*)
    if (symbol->storageClass == kScPstatic && symbol->sKind != kStConst &&
        symbol->IsLocal()) {
      emitter.Emit(symbol->GetName() + to_string(CG::curPuIdx));
    } else {
      emitter.Emit(symbol->GetName());
    }
    if (offset && !offset->IsZero()) {
      emitter.Emit("+");
      offset->Emit(emitter, nullptr);
    }
    emitter.Emit(")");
  } else if (offset) {
    if (CGOptions::doPIC && offset->op_kind_ == Opd_StImmediate &&
        (((StImmOperand *)offset)->GetSymbol()->GetStorageClass() ==
             kScGlobal ||
         ((StImmOperand *)offset)->GetSymbol()->GetStorageClass() ==
             kScExtern)) {
      emitter.Emit("#:got_lo12:");
      emitter.Emit(((StImmOperand *)offset)->GetName());
    } else {
      CG_ASSERT(!IsPIMMOffsetOutOfRange(offset->GetOffsetValue(), size_), "");
      if (!offset->IsZero()) {
        offset->Emit(emitter, nullptr);
      }
    }
  }
  emitter.Emit("(");
  Riscv64RegOperand *baseReg = static_cast<Riscv64RegOperand *>(GetBaseRegister());
  CG_ASSERT(baseReg, "expect an Riscv64RegOperand here");
  if (CGOptions::doPIC && (baseReg->GetSize() != 64)) {
    baseReg->SetSize(64);
  }
  baseReg->Emit(emitter, nullptr);
  emitter.Emit(")");
}

void Riscv64MemOperand::dump() {
  LogInfo::MapleLogger() << "Mem:";
  LogInfo::MapleLogger() << "base:";
  GetBaseRegister()->dump();
  LogInfo::MapleLogger() << "offset:";
  GetOffsetOperand()->dump();
}

bool Riscv64MemOperand::Equals(Operand *operand) {
  if (!dynamic_cast<Riscv64MemOperand *>(operand)) {
    return false;
  }
  Riscv64MemOperand *op = static_cast<Riscv64MemOperand *>(operand);
  return Equals(op);
}

bool Riscv64MemOperand::Equals(Riscv64MemOperand *op) {
  if (this == op) {
    return true;
  }

  if (GetBaseRegister()->GetRegisterNumber() == op->GetBaseRegister()->GetRegisterNumber() &&
      ((GetOffsetImmediate() && op->GetOffsetImmediate() &&
        GetOffsetImmediate()->GetValue() == op->GetOffsetImmediate()->GetValue()))) {
    return true;
  } else {
    return false;
  }
}

// sort the register operand according to their number
void Riscv64ListOperand::Emit(Emitter &emitter, OpndProp *prop) {
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
