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

#include "aarch64_reg_alloc.h"
#include "aarch64_insn.h"
#include "aarch64_cg.h"
#include "aarch64_ra_opt.h"
#include "aarch64_operand.h"
#include "mir_lower.h"
#include "cg_assert.h"
#include "special_func.h"

#include <iostream>
#include <iomanip>
#include <queue>
#include "securec.h"

namespace maplebe {

/*
   NB. As an optimization we can use X8 as a scratch (temporary)
      register if the return value is not returned through memory.
 */

Operand *AArch64RegAllocator::AllocSrcOpnd(Operand *opnd, OpndProp *prop, Insn *insn, BB *bb) {
  AArch64OpndProp *opndprop = static_cast<AArch64OpndProp *>(prop);
  if (opndprop && (opndprop->regprop_.regtype_ == kRegTyCc || opndprop->regprop_.regtype_ == kRegTyVary)) {
    return opnd;
  }
  if (opnd->IsRegister()) {
    RegOperand *regopnd = static_cast<RegOperand *>(opnd);
    AArch64CGFunc *aarch64Cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
    VectorType vctType = kVecNone;
    if (opndprop)
      vctType = aarch64Cgfunc->PickVectorType(opndprop->regprop_.subRegType);
    if (regopnd->IsOfCC() || regopnd->IsOfVary()) {
      return opnd;
    }
    if (!regopnd->IsVirtualRegister()) {
      avail_reg_set_[regopnd->GetRegisterNumber()] = false;
      live_reg_.insert(regopnd->GetRegisterNumber());
      return static_cast<AArch64RegOperand *>(regopnd);
    }
    auto regMapIt = reg_map_.find(regopnd->GetRegisterNumber());
    if (regMapIt != reg_map_.end()) {  // already allocated this register
      CG_ASSERT(AArch64isa::IsPhysicalRegister(regMapIt->second), "");
      AArch64reg_t newRegno = regMapIt->second;
      avail_reg_set_[newRegno] = false;  // make sure the real register can not be allocated and live
      live_reg_.insert(newRegno);
      allocated_set_.insert(opnd);
      return aarch64Cgfunc->GetOrCreatePhysicalRegisterOperand(newRegno, regopnd->GetSize(),
                                                                                       regopnd->GetRegisterType(), 0, vctType);
    }
    if (opndprop && opndprop->IsPhysicalRegister()) {
      AArch64reg_t newRegno = opndprop->regprop_.physical_reg_;
      allocated_set_.insert(opnd);
      avail_reg_set_[newRegno] = false;
      live_reg_.insert(newRegno);
      return aarch64Cgfunc->GetOrCreatePhysicalRegisterOperand(newRegno, regopnd->GetSize(),
                                                                                       regopnd->GetRegisterType(), 0, vctType);
    }
    if (AllocatePhysicalRegister(regopnd, opndprop)) {
      allocated_set_.insert(opnd);
      auto regMapIt = reg_map_.find(regopnd->GetRegisterNumber());
      CG_ASSERT(regMapIt != reg_map_.end(), " ERROR !! ");
      return aarch64Cgfunc->GetOrCreatePhysicalRegisterOperand(
        regMapIt->second, regopnd->GetSize(), regopnd->GetRegisterType(),
        0, vctType);
    }

    regno_t regNo = DoRegisterSpill(regopnd, insn, false, bb);
    CHECK_FATAL(regNo, "Cannot spill with O0 regalloc");
    return aarch64Cgfunc->GetOrCreatePhysicalRegisterOperand(
      (AArch64reg_t)regNo, regopnd->GetSize(), regopnd->GetRegisterType(), 0, vctType);
  } else if (opnd->IsMemoryAccessOperand()) {
    AArch64MemOperand *memopnd = static_cast<AArch64MemOperand *>(opnd);
    Operand *res = nullptr;
    switch (memopnd->GetAddrMode()) {
      case AArch64MemOperand::kAddrModeBOi:
        res = AllocSrcOpnd(memopnd->GetBaseRegister());
        //CG_ASSERT(res->IsRegister() && !static_cast<RegOperand *>(res)->IsVirtualRegister(), "");
        memopnd->SetBaseRegister(static_cast<AArch64RegOperand *>(res));
        break;
      case AArch64MemOperand::kAddrModeBOrX:
        res = AllocSrcOpnd(memopnd->GetBaseRegister());
        CG_ASSERT(res->IsRegister() && !static_cast<RegOperand *>(res)->IsVirtualRegister(), "");
        memopnd->SetBaseRegister(static_cast<AArch64RegOperand *>(res));
        res = AllocSrcOpnd(memopnd->GetOffsetRegister());
        CG_ASSERT(res->IsRegister() && !static_cast<RegOperand *>(res)->IsVirtualRegister(), "");
        memopnd->SetOffsetRegister(static_cast<AArch64RegOperand *>(res));
        break;
      case AArch64MemOperand::kAddrModeLiteral:
        break;
      case AArch64MemOperand::kAddrModeLo12Li:
        res = AllocSrcOpnd(memopnd->GetBaseRegister());
        CG_ASSERT(res->IsRegister() && !static_cast<RegOperand *>(res)->IsVirtualRegister(), "");
        memopnd->SetBaseRegister(static_cast<AArch64RegOperand *>(res));
        break;
      default:
        CG_ASSERT(false, "ERROR");
    }
    allocated_set_.insert(opnd);
    return memopnd;
  }
  CG_ASSERT(0, "NYI");
  return nullptr;
}

Operand *AArch64RegAllocator::AllocDestOpnd(Operand *opnd, Insn *insn, uint32 index, BB *bb) {
  if (opnd->IsRegister()) {
    RegOperand *regopnd = static_cast<RegOperand *>(opnd);
    RegType regty = regopnd->GetRegisterType();
    const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
    AArch64OpndProp *opndprop = static_cast<AArch64OpndProp *>(md->operand_[index]);
    if (!regopnd->IsVirtualRegister()) {
      AArch64reg_t rn = (AArch64reg_t)regopnd->GetRegisterNumber();
      avail_reg_set_[rn] = true;
      auto it = regLiveness.find(regopnd);
      if (it != regLiveness.end()) {
        if (it->second <= insn->id) {
          ReleaseReg(opndprop->regprop_.regtype_, rn);
        }
      }
      return opnd;
    }
    AArch64CGFunc *aarch64Cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
    VectorType vctType = kVecNone;
    if (opndprop)
      vctType = aarch64Cgfunc->PickVectorType(opndprop->regprop_.subRegType);

    if (opndprop->IsPhysicalRegister()) {  // physical register
      AArch64reg_t physicalReg = opndprop->regprop_.physical_reg_;
      CG_ASSERT(live_reg_.find(physicalReg) == live_reg_.end(), "physical register spill NYI");
      auto it = regLiveness.find(regopnd);
      if (it != regLiveness.end()) {
        if (it->second <= insn->id) {
          ReleaseReg(opndprop->regprop_.regtype_, physicalReg);
        }
      }
      return aarch64Cgfunc->GetOrCreatePhysicalRegisterOperand(physicalReg, regopnd->GetSize(),
                                                              regopnd->GetRegisterType(), 0, vctType);
    }

    auto regMapIt = reg_map_.find(regopnd->GetRegisterNumber());
    if (regMapIt != reg_map_.end()) {
      AArch64reg_t reg = regMapIt->second;
      if (!insn->IsCondDef()) {
        auto it = regLiveness.find(regopnd);
        if (it != regLiveness.end()) {
          if (it->second <= insn->id) {
            ReleaseReg(regty, reg);
          }
        }
      }
    } else {
      // AllocatePhysicalRegister insert a mapping from vreg no to phy reg no into reg_map_
      if (AllocatePhysicalRegister(regopnd, opndprop)) {
        regMapIt = reg_map_.find(regopnd->GetRegisterNumber());
        // Add opt by store it's value to the spill location if it has spilled before.
        if (regopnd->IsVirtualRegister()) {
          StorePseudoRegister(regopnd, regMapIt->second, insn, bb);
        }
        if (!insn->IsCondDef()) {
          auto it = regLiveness.find(regopnd);
          if (it != regLiveness.end()) {
            if (it->second <= insn->id) {
              ReleaseReg(regopnd->GetRegisterType(), regMapIt->second);
            }
          }
        }
      } else {
        // For register spill.
        regno_t regNo = DoRegisterSpill(regopnd, insn, true, bb);
        return aarch64Cgfunc->GetOrCreatePhysicalRegisterOperand(
          (AArch64reg_t)regNo, regopnd->GetSize(), regopnd->GetRegisterType(), 0, vctType);
      }
    }
    allocated_set_.insert(opnd);
    return aarch64Cgfunc->GetOrCreatePhysicalRegisterOperand(
      regMapIt->second, regopnd->GetSize(), regopnd->GetRegisterType(), 0, vctType);
  } else {
    CG_ASSERT(0, "result operand must be of type register");
  }
  return nullptr;
}

void AArch64RegAllocator::PreAllocate() {
  FOR_ALL_BB(bb, cgfunc_) {
    if (bb->IsEmpty()) {
      continue;
    }
    FOR_BB_INSNS_SAFE(insn, bb, next_insn) {
      const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
      if (!md->UseSpecReg()) {
        continue;
      }
      for (int i = 0; i < Insn::kMaxOperandNum; i++) {
        Operand *opnd = insn->opnds[i];
        if (!opnd) {
          break;
        }
        AArch64OpndProp *opndprop = static_cast<AArch64OpndProp *>(md->operand_[i]);
        if (opndprop->IsPhysicalRegister()) {
          AArch64CGFunc *a64func = static_cast<AArch64CGFunc *>(cgfunc_);
          RegOperand *regopnd = static_cast<RegOperand *>(opnd);
          AArch64RegOperand *phyreg =
            a64func->GetOrCreatePhysicalRegisterOperand(opndprop->regprop_.physical_reg_, opnd->size_, kRegTyInt);
          if (opndprop->IsRegDef()) {
            Insn *newInsn =
              a64func->cg->BuildInstruction<AArch64Insn>(a64func->PickMovInsn(regopnd, phyreg), regopnd, phyreg);
            bb->InsertInsnAfter(insn, newInsn);
          } else {
            Insn *newInsn =
              a64func->cg->BuildInstruction<AArch64Insn>(a64func->PickMovInsn(phyreg, regopnd), phyreg, regopnd);
            bb->InsertInsnBefore(insn, newInsn);
          }
          insn->opnds[i] = phyreg;
        }
      }
    }
  }
}

void AArch64RegAllocator::AllocHandleCallee(Insn *insn, const AArch64MD *md) {
  AArch64CGFunc *acgf = static_cast<AArch64CGFunc *>(cgfunc_);
  Operand *opnd1 = insn->opnds[1];
  if (opnd1->IsList()) {
    AArch64ListOperand *srcopnds = static_cast<AArch64ListOperand *>(insn->opnds[1]);
    AArch64ListOperand *srcopndsNew = acgf->memPool->New<AArch64ListOperand>(acgf->funcscope_allocator_);
    for (auto regopnd : srcopnds->GetOperands()) {
      CG_ASSERT(!regopnd->IsVirtualRegister(), "");
      AArch64RegOperand *aarch64Regopnd = static_cast<AArch64RegOperand *> (regopnd);
      AArch64reg_t physicalReg = (AArch64reg_t)regopnd->GetRegisterNumber();
      avail_reg_set_[physicalReg] = false;
      live_reg_.insert(physicalReg);
      srcopndsNew->PushOpnd(
        acgf->GetOrCreatePhysicalRegisterOperand(physicalReg, regopnd->GetSize(), regopnd->GetRegisterType(),
                                                 0, aarch64Regopnd->GetVectorType()));
    }
    insn->opnds[1] = srcopndsNew;
  }

  Operand *opnd = insn->opnds[0];
  AArch64OpndProp *opndProp0 = static_cast<AArch64OpndProp *>(md->operand_[0]);
  if (opnd && opnd->IsRegister() && opndProp0->IsRegUse()) {
    if (allocated_set_.find(opnd) != allocated_set_.end()) {
      RegOperand *regopnd = static_cast<RegOperand *>(opnd);
      AArch64reg_t physicalReg = reg_map_[regopnd->GetRegisterNumber()];
      insn->opnds[0] = acgf->GetOrCreatePhysicalRegisterOperand(
        physicalReg, regopnd->GetSize(), regopnd->GetRegisterType(),
        0, acgf->PickVectorType(opndProp0->regprop_.subRegType));
    } else {
      insn->opnds[0] = AllocSrcOpnd(opnd, opndProp0);
    }
  }
}

void AArch64RegAllocator::GetPhysicalRegisterBank(RegType regty, uint8 &begin, uint8 &end) {
  switch (regty) {
    case kRegTyVary:
    case kRegTyCc:
      begin = kRinvalid;
      end = kRinvalid;
      break;
    case kRegTyInt:
      begin = R0;
      end = R28;
      break;
    case kRegTyFloat:
      begin = V0;
      end = V31;
      break;
    default:
      CG_ASSERT(false, "NYI");
      break;
  }
}

void AArch64RegAllocator::InitAvailReg() {
  errno_t eNum = memset_s(avail_reg_set_, MAXREG, 1, sizeof(avail_reg_set_));
  if (eNum) {
    CHECK_FATAL(false, "memset_s failed");
  }
  avail_reg_set_[R29] = false;  // FP
  avail_reg_set_[RLR] = false;
  avail_reg_set_[RSP] = false;
  avail_reg_set_[RZR] = false;

  // when yieldpoint is enabled,
  // the dedicated register is not available.
  if (cgfunc_->cg->GenYieldpoint()) {
    avail_reg_set_[RYP] = false;
  }

  if (g->optim_level > 1) {
    AArch64CGFunc *cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
    for (auto &reg : cgfunc->formal_reg_list_) {
      avail_reg_set_[reg] = false;
    }
  }
}

bool AArch64RegAllocator::IsSpecialReg(AArch64reg_t reg)  // these registers can not be allocated
{
  if (reg == RLR || reg == RSP) {
    return true;
  }

  // when yieldpoint is enabled, the dedicated register can not be allocated.
  if (cgfunc_->cg->GenYieldpoint() && reg == RYP) {
    return true;
  }

  AArch64CGFunc *cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
  for (MapleVector<AArch64reg_t>::iterator it = cgfunc->formal_reg_list_.begin(); it != cgfunc->formal_reg_list_.end();
       it++) {
    if (*it == reg) {
      return true;
    }
  }
  return false;
}

// Those registers can not be overwrite.
bool AArch64RegAllocator::IsUntouchableReg(uint32 regno) {
  if (regno == RSP || regno == RFP) {
    return true;
  }

  // when yieldpoint is enabled, the RYP(x19) can not be used.
  if (cgfunc_->cg->GenYieldpoint() && regno == RYP) {
    return true;
  }

  return false;
}

void AArch64RegAllocator::ReleaseReg(RegOperand *regopnd, OpndProp *prop) {
  ReleaseReg(regopnd->GetRegisterType(), reg_map_[regopnd->GetRegisterNumber()]);
}

void AArch64RegAllocator::ReleaseReg(RegType regty, AArch64reg_t reg) {
  CG_ASSERT(reg < 100, "can't release virtual register");
  live_reg_.erase(reg);
  if (!IsSpecialReg((AArch64reg_t)reg)) {
    avail_reg_set_[reg] = true;
  }
}

// trying to allocate a physical register to opnd. return true if success
bool AArch64RegAllocator::AllocatePhysicalRegister(RegOperand *opnd, OpndProp *prop) {
  RegType regtype = opnd->GetRegisterType();
  uint8 regStart = 0;
  uint8 regEnd = 0;
  GetPhysicalRegisterBank(regtype, regStart, regEnd);

  for (uint8 reg = regStart; reg <= regEnd; reg++) {
    if (!avail_reg_set_[reg]) {
      continue;
    }

    reg_map_[opnd->GetRegisterNumber()] = AArch64reg_t(reg);
    avail_reg_set_[reg] = false;
    live_reg_.insert(reg);  // this register is live now
    return true;
  }
  return false;
}

// If opnd is a callee saved register, save it in the prolog and restore it in the epilog
void AArch64RegAllocator::SaveCalleeSavedReg(RegOperand *regopnd) {
  regno_t regno = regopnd->GetRegisterNumber();
  AArch64reg_t a64reg = (AArch64reg_t)(regopnd->IsVirtualRegister() ? reg_map_[regno] : regno);

  // when yieldpoint is enabled, skip the reserved register for yieldpoint.
  if (cgfunc_->cg->GenYieldpoint() && a64reg == RYP) {
    return;
  }

  if (AArch64Abi::IsCalleeSavedReg(a64reg)) {
    static_cast<AArch64CGFunc *>(cgfunc_)->AddtoCalleeSaved(a64reg);
  }
}

static void InsertPRegStoreInstruction(Insn *insn, BB *bb) {
  switch (bb->GetKind()) {
    case BB::kBBIf: {
      Insn *cmpInsn = bb->lastinsn->prev;
      bb->InsertInsnBefore(cmpInsn, insn);
      break;
    }
    case BB::kBBThrow:
    case BB::kBBCall: {
      const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(bb->lastinsn)->mop_];
      CG_ASSERT(md->IsCall(), "");
      Insn *endInsn = bb->firstinsn->prev;
      Insn *insertAfterInsn = bb->lastinsn->prev;
      CG_ASSERT(insertAfterInsn, "This block must have regassign; lastinsn->prev must not be nullptr");
      while (insertAfterInsn != endInsn) {
        md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insertAfterInsn)->mop_];
        if (!md->IsMove() || static_cast<RegOperand *>(insertAfterInsn->GetOperand(0))->IsVirtualRegister() == true) {
          break;
        }
        insertAfterInsn = insertAfterInsn->prev;
      }
      CG_ASSERT(insertAfterInsn != endInsn, "This block must have regassign; insert_after_insn must not be end_insn");
      bb->InsertInsnAfter(insertAfterInsn, insn);
      break;
    }
    case BB::kBBGoto:
      bb->InsertInsnBefore(bb->lastinsn, insn);
      break;
    default:
      bb->AppendInsn(insn);
      break;
  }
}

bool DefaultO0RegAllocator::AllocateRegisters() {
  InitAvailReg();
  PreAllocate();
  cgfunc_->SetIsAfterRegAlloc();

  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
  // we store both FP/LR if using FP or if not using FP, but func has a call
  if (a64cgfunc->ShouldSaveFPLR()) {
    // Using FP, record it for saving
    a64cgfunc->AddtoCalleeSaved(RFP);
    a64cgfunc->AddtoCalleeSaved(RLR);
    a64cgfunc->NoteFPLRAddedToCalleeSavedList();
  }

  FOR_ALL_BB_REV(bb, a64cgfunc) {
    if (bb->IsEmpty()) {
      continue;
    }
    regLiveness.clear();

    bool isIntrinsicBb = bb->GetKind() == BB::kBBIntrinsic;

    uint32 id = 1;
    FOR_BB_INSNS_REV(insn, bb) {
      if (!insn->IsMachineInstruction()) continue;
      insn->id = id;
      id++;
      const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
      for (int i = 0; i < Insn::kMaxOperandNum; i++) {
        Operand *opnd = insn->opnds[i];
        AArch64OpndProp *aarch64Opndprop = static_cast<AArch64OpndProp *>(md->operand_[i]);
        if (!opnd || !aarch64Opndprop->IsRegDef()) {
          continue;
        }
        if (opnd->IsRegister()) {
          regLiveness[opnd] = insn->id;
        }
      }
    }
    FOR_BB_INSNS_REV(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }

      const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];

      if (md->IsCall() && insn->mop_ != MOP_clinit) {
        AllocHandleCallee(insn, md);
        continue;
      }

      for (int i = 0; i < Insn::kMaxOperandNum; i++) {  // the dest registers
        Operand *opnd = insn->opnds[i];
        AArch64OpndProp *aarch64Opndprop = static_cast<AArch64OpndProp *>(md->operand_[i]);
        if (!opnd || !aarch64Opndprop->IsRegDef()) {
          continue;
        }
        if (allocated_set_.find(opnd) != allocated_set_.end()) {
          // free the live range of this register
          RegOperand *regopnd = static_cast<RegOperand *>(opnd);
          CG_ASSERT(regopnd, "only register can be a result");
          SaveCalleeSavedReg(regopnd);
          if (isIntrinsicBb && insn->IsAtomicStore()) {
            // remember the physical machine register assigned
            CG_ASSERT(atomic_store_result_reg == kRinvalid, "");
            regno_t regno = regopnd->GetRegisterNumber();
            AArch64reg_t a64reg = (AArch64reg_t)(regopnd->IsVirtualRegister() ? reg_map_[regno] : regno);
            atomic_store_result_reg = a64reg;
          } else if (!insn->IsCondDef()) {
            auto it = regLiveness.find(regopnd);
            if (it != regLiveness.end()) {
              if (it->second <= insn->id) {
                ReleaseReg(regopnd, md->operand_[i]);
              }
            }
          }

          insn->opnds[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand(
            reg_map_[regopnd->GetRegisterNumber()], regopnd->GetSize(), regopnd->GetRegisterType(),
            0, a64cgfunc->PickVectorType(aarch64Opndprop->regprop_.subRegType));
          continue;  // already allocated
        }

        if (opnd->IsRegister()) {
          insn->opnds[i] = AllocDestOpnd(opnd, insn, i);
          SaveCalleeSavedReg(static_cast<RegOperand *>(opnd));
        }
      }

      for (int i = 0; i < Insn::kMaxOperandNum; i++) {  // the src registers
        Operand *opnd = insn->opnds[i];
        if (!opnd ||
            !(static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegUse() || opnd->op_kind_ == Operand::Opd_Mem)) {
          continue;
        }
        if (allocated_set_.find(opnd) != allocated_set_.end() && opnd->IsRegister()) {
          RegOperand *regopnd = static_cast<RegOperand *>(opnd);
          AArch64reg_t reg = reg_map_[regopnd->GetRegisterNumber()];
          avail_reg_set_[reg] = false;
          live_reg_.insert(reg);  // this register is live now
          CG_ASSERT( regopnd->GetSize() != 128 || !regopnd->IsVirtualRegister(), "nyi");
          insn->opnds[i] =
            a64cgfunc->GetOrCreatePhysicalRegisterOperand(reg, regopnd->GetSize(), regopnd->GetRegisterType());
        } else {
          insn->opnds[i] = AllocSrcOpnd(opnd, md->operand_[i]);
        }
      }
    }

    // hack. a better way to handle intrinsics?
    if (atomic_store_result_reg != kRinvalid) {
      ReleaseReg(kRegTyInt, atomic_store_result_reg);
      atomic_store_result_reg = kRinvalid;
    }
  }
  return true;
}

void O1RegAllocator::CollectPRegUsesInExpr(BaseNode *expr, BB *bb) {
  Opcode opr = expr->op;
  switch (opr) {
    case OP_regread:
      if (!cgfunc_->IsSpecialPseudoRegister(static_cast<RegreadNode *>(expr)->regIdx)) {
        bb->AddPseudoRegisterToUseList(static_cast<RegreadNode *>(expr)->regIdx);
      }
      break;
    /* binary expressions */
    case OP_add:
    case OP_ashr:
    case OP_lshr:
    case OP_shl:
    case OP_mul:
    case OP_div:
    case OP_rem:
    case OP_sub:
    case OP_band:
    case OP_bior:
    case OP_bxor:
    case OP_depositbits:
    case OP_land:
    case OP_lior:
    case OP_min:
    case OP_max:
    case OP_le:
    case OP_ge:
    case OP_gt:
    case OP_lt:
    case OP_ne:
    case OP_eq:
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg:
      CollectPRegUsesInExpr(expr->Opnd(0), bb);
      CollectPRegUsesInExpr(expr->Opnd(1), bb);
      break;
    /* unary expressions */
    case OP_abs:
    case OP_bnot:
    case OP_sext:
    case OP_zext:
    case OP_extractbits:
    case OP_lnot:
    case OP_neg:
    case OP_recip:
    case OP_sqrt:
    case OP_ceil:
    case OP_floor:
    case OP_retype:
    case OP_cvt:
    case OP_round:
    case OP_trunc:
    case OP_malloc:
    case OP_gcmallocjarray:
    case OP_gcpermallocjarray:
    case OP_iread:
      CollectPRegUsesInExpr(expr->Opnd(0), bb);
      break;
    /* tertiary expressions */
    case OP_select:
      CollectPRegUsesInExpr(expr->Opnd(0), bb);
      CollectPRegUsesInExpr(expr->Opnd(1), bb);
      CollectPRegUsesInExpr(expr->Opnd(2), bb);
      break;
    default:
      /*
         case OP_dread:
         case OP_constval:
         case OP_conststr:
         case OP_conststr16:
         case OP_addrof:
         case OP_gcmalloc:
         case OP_gcpermalloc:
       */
      break;
  }
}

void O1RegAllocator::CollectPRegUses(Opcode c, StmtNode *s, BB *bb) {
  switch (c) {
    /* unary statements */
    case OP_brfalse:
    case OP_brtrue:
    case OP_rangegoto: {
      UnaryStmtNode *usn = static_cast<UnaryStmtNode *>(s);
      CollectPRegUsesInExpr(usn->Opnd(0), bb);
      break;
    }
    /* n-ary statements */
    case OP_return:
    case OP_call:
    case OP_icall:
    case OP_intrinsiccall:
    case OP_intrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned: {
      NaryStmtNode *nsn = static_cast<NaryStmtNode *>(s);
      for (auto o : nsn->nOpnd) {
        CollectPRegUsesInExpr(o, bb);
      }
      break;
    }
    /* with rhs */
    case OP_dassign: {
      DassignNode *dan = static_cast<DassignNode *>(s);
      CollectPRegUsesInExpr(dan->GetRhs(), bb);
      break;
    }
    case OP_regassign:
      CG_ASSERT(0, "Should have already handled?");
      break;
    /* with addrexpr and rhs */
    case OP_iassign: {
      IassignNode *ian = static_cast<IassignNode *>(s);
      CollectPRegUsesInExpr(ian->addrExpr, bb);
      CollectPRegUsesInExpr(ian->rhs, bb);
      break;
    }
    case OP_eval:
      CG_ASSERT(0, "Not supported");
      break;
    case OP_label:
    case OP_goto:
    case OP_comment:
    case OP_javacatch:
    case OP_javatry:
    case OP_cppcatch:
    case OP_cpptry:
    case OP_catch:
    case OP_try:
    case OP_endtry:
      break;
    case OP_syncenter:
    case OP_syncexit:
      CG_ASSERT(0, "should have been lowered to a call or inlined");
      break;
    default:
      CG_ASSERT(0, "NYI");
      break;
  }
}

void O1RegAllocator::CollectPRegDefsUses(BB *bb) {
  if (bb->laststmt && !bb->firststmt) {
    LogInfo::MapleLogger() << "BB " << hex << bb << dec << " ";
    bb->laststmt->Dump(&(cgfunc_->mirModule), 0);
  }

  StmtNode *stmt = bb->laststmt;
  StmtNode *endStmt = bb->firststmt->GetPrev();
  CHECK_FATAL(stmt != nullptr, "null ptr check");
  for (; stmt != endStmt; stmt = stmt->GetPrev()) {
    Opcode opcode = stmt->op;
    switch (opcode) {
      case OP_regassign: {
        PregIdx pregidx = static_cast<RegassignNode *>(stmt)->regIdx;
        if (pregidx >=
            0) {  // Special registers should not be stored since they don't have corresponding virtual register.
          bb->AddPseudoRegisterWritten(pregidx);
        }
        bb->RemovePseudoRegisterFromUseList(pregidx);
        CollectPRegUsesInExpr(static_cast<RegassignNode *>(stmt)->uOpnd, bb);
        break;
      }

      default:
        CollectPRegUses(opcode, stmt, bb);
        break;
    }
  }
}

regno_t O1RegAllocator::GetVirtualRegFromPhysicalReg(AArch64reg_t regT) {
  for (auto it : reg_map_) {
    if (it.second == regT) {
      return (it.first);
    }
  }
  CG_ASSERT(0, "There must be a map betwwen virtual register and physical register.");
  return INVALID_REGNO;
}

Operand *O1RegAllocator::GetOperandFromAllocatedSet(regno_t regNo) {
  for (auto it : allocated_set_) {
    if (static_cast<RegOperand *>(it)->GetRegisterNumber() == regNo) {
      return (it);
    }
  }

  CG_ASSERT(0, "There must be an operand with reg_no in allocated_set_.");
  return nullptr;
}

void O1RegAllocator::StorePseudoRegister(RegOperand *regopnd, AArch64reg_t regNo, Insn *insn, BB *bb) {
  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
  CG *cg = a64cgfunc->cg;
  if (!a64cgfunc->IsVRegNoForPseudoRegister(regopnd->GetRegisterNumber())) {
    return;
  }
  PregIdx pregIdx = a64cgfunc->GetPseudoRegIdxFromVirtualRegNo(regopnd->GetRegisterNumber());
  MemOperand *memOperand = a64cgfunc->GetPseudoRegisterSpillMemoryOperand(pregIdx);
  CG_ASSERT( regopnd->GetSize() != 128 || !regopnd->IsVirtualRegister(), "nyi");
  RegOperand *physicalRegOperand =
    a64cgfunc->GetOrCreatePhysicalRegisterOperand(regNo, regopnd->GetSize(), regopnd->GetRegisterType());
  PrimType stype = a64cgfunc->GetTypeFromPseudoRegIdx(pregIdx);
  CHECK_FATAL(memOperand != nullptr, "memOperand is null in O1RegAllocator::StorePseudoRegister");
  Insn *stInsn = cg->BuildInstruction<AArch64Insn>(a64cgfunc->PickStInsn(memOperand->GetSize(), stype),
                                                   physicalRegOperand, memOperand);
  bb->InsertInsnAfter(insn, stInsn);
}

bool O1RegAllocator::AllocateRegisters() {
  InitAvailReg();
  PreAllocate();
  cgfunc_->SetIsAfterRegAlloc();

  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
  // we store both FP/LR if using FP or if not using FP, but func has a call
  if (a64cgfunc->ShouldSaveFPLR()) {
    // Using FP, record it for saving
    a64cgfunc->AddtoCalleeSaved(RFP);
    a64cgfunc->AddtoCalleeSaved(RLR);
    a64cgfunc->NoteFPLRAddedToCalleeSavedList();
  }

  CG_ASSERT(g->optim_level == 1, "");

  FOR_ALL_BB_REV(bb, a64cgfunc) {
    if (bb->IsEmpty()) {
      continue;
    }

    bool isIntrinsicBb = bb->GetKind() == BB::kBBIntrinsic;

    CollectPRegDefsUses(bb);

    if (bb->GetKind() != BB::kBBReturn) {
      CG *cg = a64cgfunc->cg;
      RegOperand *src = nullptr;
      for (PregIdx pr : bb->written_pseudo_regs) {
        if (a64cgfunc->IsSpecialPseudoRegister(pr)) {
          src = a64cgfunc->GetOrCreateSpecialRegisterOperand(-pr);
        } else {
          src = a64cgfunc->CreateVirtualRegisterOperand(a64cgfunc->GetVirtualRegNoFromPseudoRegIdx(pr));
        }
        MemOperand *dest = a64cgfunc->GetPseudoRegisterSpillMemoryOperand(pr);
        PrimType stype = a64cgfunc->GetTypeFromPseudoRegIdx(pr);
        Insn *stInsn = cg->BuildInstruction<AArch64Insn>(a64cgfunc->PickStInsn(src->GetSize(), stype), src, dest);
        InsertPRegStoreInstruction(stInsn, bb);
      }
    }

    FOR_BB_INSNS_REV(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }

      const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];

      if (md->IsCall() && insn->mop_ != MOP_clinit) {
        AllocHandleCallee(insn, md);
        continue;
      }

      InitValidSpillRegIndex(insn);

      for (int i = 0; i < Insn::kMaxOperandNum; i++) {  // the dest registers
        Operand *opnd = insn->opnds[i];

        if (!opnd || !static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegDef()) {
          continue;
        }

        if (allocated_set_.find(opnd) != allocated_set_.end()) {
          // free the live range of this register
          RegOperand *regopnd = static_cast<RegOperand *>(opnd);
          // CG_ASSERT( regopnd, "only register can be a result" );
          SaveCalleeSavedReg(regopnd);
          if (isIntrinsicBb && insn->IsAtomicStore()) {
            // remember the physical machine register assigned
            CG_ASSERT(atomic_store_result_reg == kRinvalid, "");
            regno_t regno = regopnd->GetRegisterNumber();
            AArch64reg_t a64reg = (AArch64reg_t)(regopnd->IsVirtualRegister() ? reg_map_[regno] : regno);
            atomic_store_result_reg = a64reg;
          } else if (!insn->IsCondDef()) {
            ReleaseReg(regopnd, md->operand_[i]);
            allocated_set_.erase(opnd);
          }
          CG_ASSERT( regopnd->GetSize() != 128 || !regopnd->IsVirtualRegister(), "nyi");
          insn->opnds[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand(
            reg_map_[regopnd->GetRegisterNumber()], regopnd->GetSize(), regopnd->GetRegisterType());
          continue;  // already allocated
        }

        if (opnd->IsRegister()) {
          CG_ASSERT(!(isIntrinsicBb && insn->IsAtomicStore()), "");
          insn->opnds[i] = AllocDestOpnd(opnd, insn, i, bb);
          SaveCalleeSavedReg(static_cast<RegOperand *>(opnd));
        }
      }

      for (int i = 0; i < Insn::kMaxOperandNum; i++) {  // the src registers
        Operand *opnd = insn->opnds[i];
        if (!opnd ||
            !(static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegUse() || opnd->op_kind_ == Operand::Opd_Mem)) {
          continue;
        }
        if (allocated_set_.find(opnd) != allocated_set_.end() && opnd->IsRegister()) {
          RegOperand *regopnd = static_cast<RegOperand *>(opnd);
          AArch64reg_t reg = reg_map_[regopnd->GetRegisterNumber()];
          avail_reg_set_[reg] = false;
          live_reg_.insert(reg);  // this register is live now
          CG_ASSERT( regopnd->GetSize() != 128 || !regopnd->IsVirtualRegister(), "nyi");
          insn->opnds[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand(
            reg_map_[regopnd->GetRegisterNumber()], regopnd->GetSize(), regopnd->GetRegisterType());
        } else {
          insn->opnds[i] = AllocSrcOpnd(opnd, md->operand_[i], insn, bb);
        }
      }
    }

    // hack. a better way to handle intrinsics?
    if (atomic_store_result_reg != kRinvalid) {
      ReleaseReg(kRegTyInt, atomic_store_result_reg);
      atomic_store_result_reg = kRinvalid;
    }

    {
      // collect still assigned physical registers...
      CG *cg = a64cgfunc->cg;
      for (PregIdx pr : bb->GetUsePRegs()) {
        regno_t vregNo = a64cgfunc->GetVirtualRegNoFromPseudoRegIdx(pr);
        auto regMapIt = reg_map_.find(vregNo);
        CG_ASSERT(regMapIt != reg_map_.end(), "The preg is not assigned physical register?");
        CG_ASSERT(AArch64isa::IsPhysicalRegister(regMapIt->second), "");
        AArch64reg_t assignedMachReg = regMapIt->second;

        MemOperand *src = a64cgfunc->GetPseudoRegisterSpillMemoryOperand(pr);
        CG_ASSERT(src != nullptr, "src is null in O1RegAllocator::AllocateRegisters");
        PrimType stype = a64cgfunc->GetTypeFromPseudoRegIdx(pr);
        RegType rtyp = IsPrimitiveInteger(stype) ? kRegTyInt : kRegTyFloat;
        int rbitlen = (src->GetSize() < 32) ? 32 : src->GetSize();
        CG_ASSERT(rbitlen != 128, "NYI");
        AArch64RegOperand *dest = a64cgfunc->GetOrCreatePhysicalRegisterOperand(assignedMachReg, rbitlen, rtyp);
        // if the first statement is dassign from regread of retval0
        Insn *ldInsn = cg->BuildInstruction<AArch64Insn>(a64cgfunc->PickLdInsn(src->GetSize(), stype), dest, src);
        if (bb->firstinsn->IsSaveRetValToLocal()) {
          CG_ASSERT(!bb->firstinsn->next->IsSaveRetValToLocal(), "");
          bb->InsertInsnAfter(bb->firstinsn, ldInsn);
        } else {
          bb->InsertInsnBegin(ldInsn);
        }

        reg_map_.erase(vregNo);
      }
    }
  }
  CG_ASSERT(atomic_store_result_reg == kRinvalid, "");

  return true;
}

/*
 * Check whether the reserve spill registers are valid.
 * If an operand of a instruction maps a physical register, and the physical register is a reserve spill register,
 * then set the corresponding bit of m_validIndex as 0.
 */
void O1RegAllocator::InitValidSpillRegIndex(Insn *insn) {
  m_validIndex = 0xFFFF;

  ClearRegIndex();

  for (int i = 0; i < Insn::kMaxOperandNum; i++) {
    Operand *opnd = insn->opnds[i];

    if (!opnd || !opnd->IsRegister()) {
      continue;
    }

    if (allocated_set_.find(opnd) != allocated_set_.end()) {
      RegOperand *regopnd = static_cast<RegOperand *>(opnd);
      CHECK_FATAL(regopnd != nullptr, "null ptr check");
      regno_t regno = regopnd->GetRegisterNumber();
      AArch64reg_t a64reg = (AArch64reg_t)(regopnd->IsVirtualRegister() ? reg_map_[regno] : regno);
      if (a64reg <= O1_INT_REG_FOR_SPILL && (O1_INT_REG_FOR_SPILL - a64reg) < Insn::kMaxOperandNum) {
        m_validIndex &= (~(0x1 << (O1_INT_REG_FOR_SPILL - a64reg)));
      }
    }
  }
}

regno_t O1RegAllocator::DoRegisterSpill(RegOperand *regopnd, Insn *insn, bool isDstRegister, BB *bb) {
  RegOperand *regOperand = nullptr;
  regno_t regNo;
  PregIdx pregIdx, oldPregIdx;
  Insn *ldInsn = nullptr;
  CG_ASSERT(insn != nullptr, "insn should not be nullptr for function DoSrcRegisterSpill.");
  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
  CG *cg = a64cgfunc->cg;

  pregIdx = a64cgfunc->GetPseudoRegIdxFromVirtualRegNo(regopnd->GetRegisterNumber());
  PrimType stype = a64cgfunc->GetTypeFromPseudoRegIdx(pregIdx);

  if (IsPrimitiveInteger(stype)) {
    regNo = O1_INT_REG_FOR_SPILL - GetNextIntRegIndex();
  } else {
    regNo = O1_FLT_REG_FOR_SPILL - GetNextFloatRegIndex();
  }
  CG_ASSERT( regopnd->GetSize() != 128 || !regopnd->IsVirtualRegister(), "nyi");

  regOperand =
    a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)regNo, regopnd->GetSize(), regopnd->GetRegisterType());
  regno_t oldRegNo = GetVirtualRegFromPhysicalReg((AArch64reg_t)regNo);

  oldPregIdx = a64cgfunc->GetPseudoRegIdxFromVirtualRegNo(oldRegNo);
  // GetOrCreateRegisterSpillMemoryOperand(..).
  MemOperand *oldMemOperand = a64cgfunc->GetOrCreatSpillMem(oldPregIdx);

  PrimType oldStype = a64cgfunc->GetTypeFromPseudoRegIdx(oldPregIdx);
  ldInsn = cg->BuildInstruction<AArch64Insn>(a64cgfunc->PickLdInsn(oldMemOperand->GetSize(), oldStype), regOperand,
                                             oldMemOperand);
  bb->InsertInsnAfter(insn, ldInsn);
  reg_map_.erase(oldRegNo);
  allocated_set_.erase(GetOperandFromAllocatedSet(oldRegNo));

  if (isDstRegister) {
    // Store value after current instruction.
    MemOperand *memOperand = a64cgfunc->GetOrCreatSpillMem(pregIdx);
    Insn *stInsn =
      cg->BuildInstruction<AArch64Insn>(a64cgfunc->PickStInsn(regopnd->GetSize(), stype), regOperand, memOperand);
    bb->InsertInsnAfter(insn, stInsn);
  } else {
    allocated_set_.insert(regopnd);
    reg_map_[regopnd->GetRegisterNumber()] = AArch64reg_t(regNo);
  }

  return regNo;
}

//==================
//= Linear Scan RA
//==================

#define LSRA_DEBUG

#ifdef LSRA_DEBUG
#define LSRA_DUMP CGDEBUGFUNC(cgfunc_)
#else
#define LSRA_DUMP 0
#endif

#define SPILLED 1

#define IN_SPILL_RANGE                                                                          \
  (cgfunc_->GetName().find(CGOptions::dumpFunc.c_str()) != string::npos && ++debug_spill_cnt && \
   (CGOptions::spillRanges[0] < debug_spill_cnt) && (debug_spill_cnt < CGOptions::spillRanges[1]))

#undef LSRA_GRAPH

// This LSRA implementation is an interpretation of the [Poletto97] paper.
// BFS BB ordering is used to order the instructions.  The live intervals are vased on
// this instruction order.  All vreg defines should come before an use, else a warning is
// given.
// Live interval is traversed in order from lower instruction order to higher order.
// When encountering a live interval for the first time, it is assumed to be live and placed
// inside the 'active' structure until the vreg's last access.  During the time a vreg
// is in 'active', the vreg occupies a physical register allocation and no other vreg can
// be allocated the same physical register.

void LSRALinearScanRegAllocator::PrintRegSet(MapleSet<uint32> set, string str) {
  LogInfo::MapleLogger() << str;
  MapleSet<uint32>::iterator it;
  for (it = set.begin(); it != set.end(); it++) {
    LogInfo::MapleLogger() << " " << *it;
  }
  LogInfo::MapleLogger() << "\n";
}

// This is a support routine to compute the overlapping live intervals in graph form.
// The output file can be viewed by gnuplot.
// Despite the function name of LiveRanges, it is using live intervals.
void LSRALinearScanRegAllocator::PrintLiveRanges() {
  // ================= Output to plot.pg ===============
  std::ofstream out("plot.pg");
  std::streambuf *coutbuf = LogInfo::MapleLogger().rdbuf();  // old buf
  LogInfo::MapleLogger().rdbuf(out.rdbuf());                 // new buf

  LogInfo::MapleLogger() << "#!/usr/bin/gnuplot\n";
  LogInfo::MapleLogger() << "#max_insn_num " << max_insn_num << "\n";
  LogInfo::MapleLogger() << "#min_vreg_num " << min_vreg_num << "\n";
  LogInfo::MapleLogger() << "#max_vreg_num " << max_vreg_num << "\n";
  LogInfo::MapleLogger() << "reset\nset terminal png\n";
  LogInfo::MapleLogger() << "set xrange [1:" << max_insn_num << "]\n";
  LogInfo::MapleLogger() << "set grid\nset style data linespoints\n";
  LogInfo::MapleLogger() << "set datafile missing '0'\n";
  std::vector<std::vector<uint32>> graph;
  graph.resize(max_vreg_num);
  for (uint32 i = 0; i < max_vreg_num; i++) {
    graph[i].resize(max_insn_num);
  }
  uint32 minY = 0xFFFFFFFF;
  uint32 maxY = 0;
  for (uint32 i = 0; i < LI_.size(); i++) {
    if (!LI_[i] || LI_[i]->regno == 0) {
      continue;
    }
    LiveInterval *li = LI_[i];
    uint32 regno = li->regno;
    if ((li->last_use - li->first_def) < 20) {
      continue;
    }
    if (regno < minY) {
      minY = regno;
    }
    if (regno > maxY) {
      maxY = regno;
    }
    uint32 n;
    for (n = 0; n <= (li->first_def - 1); n++) {
      graph[regno - min_vreg_num][n] = 0;
    }
    if (li->last_use >= n) {
      for (; n <= (li->last_use - 1); n++) {
        graph[regno - min_vreg_num][n] = regno;
      }
    }
    for (; n < max_insn_num; n++) {
      graph[regno - min_vreg_num][n] = 0;
    }

#define CHECK_FOR_REG(opnd, regno, isdef)                                                            \
  {                                                                                                  \
    if (!opnd->IsRegister()) {                                                                       \
      continue;                                                                                      \
    }                                                                                                \
    RegOperand *RegOpnd = static_cast<RegOperand *>(opnd);                                           \
    if (RegOpnd->GetRegisterType() == kRegTyCc || RegOpnd->GetRegisterType() == kRegTyVary) {        \
      continue;                                                                                      \
    }                                                                                                \
    if (RegOpnd->GetRegisterNumber() == regno) {                                                     \
      LogInfo::MapleLogger() << "set object circle at " << insn->id << "," << li->regno << " size 5 fillcolor rgb \""; \
      if (isdef)                                                                                     \
        LogInfo::MapleLogger() << "black\"\n";                                                                         \
      else                                                                                           \
        LogInfo::MapleLogger() << "orange\"\n";                                                                        \
    }                                                                                                \
  }

    for (uint32_t bbIdx = 0; bbIdx < sorted_bbs.size(); bbIdx++) {
      BB *bb = sorted_bbs[bbIdx];
      FOR_BB_INSNS(insn, bb) {
        const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
        for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
          Operand *opnd = insn->GetOperand(i);
          if (!opnd) {
            continue;
          }
          if (opnd->IsList()) {
          } else if (opnd->IsMemoryAccessOperand()) {
            MemOperand *memopnd = static_cast<MemOperand *>(opnd);
            Operand *base = memopnd->GetBaseRegister();
            Operand *offset = memopnd->GetIndexRegister();
            if (base) {
              CHECK_FOR_REG(base, regno, false);
            }
            if (offset) {
              CHECK_FOR_REG(offset, regno, false);
            }
          } else {
            bool isdef = static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegDef();
            CHECK_FOR_REG(opnd, regno, isdef);
          }
        }
      }
    }
#undef CHECK_FOR_REG
  }
  LogInfo::MapleLogger() << "set yrange [" << minY - 1 << ":" << maxY + 1 << "]\n";

  LogInfo::MapleLogger() << "plot \"plot.dat\" using 1:2 title \"R" << min_vreg_num << "\"";
  for (uint32 i = 1; i < (max_vreg_num - min_vreg_num + 1); i++) {
    LogInfo::MapleLogger() << ", \\\n\t\"\" using 1:" << i + 2 << " title \"R" << min_vreg_num + i << "\"";
  }
  LogInfo::MapleLogger() << ";\n";

  // ================= Output to plot.dat ===============
  std::ofstream out2("plot.dat");
  LogInfo::MapleLogger().rdbuf(out2.rdbuf());  // new buf
  LogInfo::MapleLogger() << "##reg";
  for (uint32 i = min_vreg_num; i <= max_vreg_num; i++) {
    LogInfo::MapleLogger() << " R" << i;
  }
  LogInfo::MapleLogger() << "\n";
  for (uint32 n = 0; n < max_insn_num; n++) {
    LogInfo::MapleLogger() << n + 1;
    for (uint32 i = min_vreg_num; i <= max_vreg_num; i++) {
      LogInfo::MapleLogger() << " " << graph[i - min_vreg_num][n];
    }
    LogInfo::MapleLogger() << "\n";
  }
  LogInfo::MapleLogger().rdbuf(coutbuf);
}

void LSRALinearScanRegAllocator::PrintLiveInterval(LiveInterval *li, string str) {
  LogInfo::MapleLogger() << str << "\n";
  if (li->is_call != nullptr) {
    LogInfo::MapleLogger() << " first_def " << li->first_def;
    LogInfo::MapleLogger() << " is_call";
  } else if (li->phys_use) {
    LogInfo::MapleLogger() << "\tregno " << li->regno;
    LogInfo::MapleLogger() << " first_def " << li->first_def;
    LogInfo::MapleLogger() << " phys_use " << li->phys_use;
    LogInfo::MapleLogger() << " end_by_call " << li->end_by_call;
  } else {
    LogInfo::MapleLogger() << "\tregno " << setw(5) << li->regno;
    LogInfo::MapleLogger() << " first_def " << setw(8) << li->first_def;
    LogInfo::MapleLogger() << " last_use " << setw(8) << li->last_use;
    LogInfo::MapleLogger() << " assigned " << li->assigned_reg;
    LogInfo::MapleLogger() << " ref_count " << li->ref_count;
    LogInfo::MapleLogger() << " priority " << li->priority;
  }
  LogInfo::MapleLogger() << " object_address 0x" << std::hex << li << std::dec << "\n";
}

void LSRALinearScanRegAllocator::PrintParamQueue(string str) {
  LogInfo::MapleLogger() << str << "\n";
  for (uint32 i = 0; i < int_param_queue.size(); i++) {
    if (int_param_queue[i].empty()) {
      continue;
    }
    LiveInterval *li = int_param_queue[i].front();
    LiveInterval *last = int_param_queue[i].back();
    PrintLiveInterval(li, "");
    while (li != last) {
      int_param_queue[i].pop_front();
      int_param_queue[i].push_back(li);
      li = int_param_queue[i].front();
      PrintLiveInterval(li, "");
    }
    int_param_queue[i].pop_front();
    int_param_queue[i].push_back(li);
  }
}

void LSRALinearScanRegAllocator::PrintCallQueue(string str) {
  LogInfo::MapleLogger() << str << "\n";
  for (MapleList<LiveInterval *>::iterator it = call_list.begin(); it != call_list.end(); it++) {
    LiveInterval *li = static_cast<LiveInterval *>(*it);
    PrintLiveInterval(li, "");
  }
}

void LSRALinearScanRegAllocator::PrintActiveList(string str, uint32 len) {
  uint32 count = 0;
  MapleSet<LiveInterval *, ActiveCmp>::iterator it;
  LogInfo::MapleLogger() << str << " " << active.size() << "\n";
  for (it = active.begin(); it != active.end(); it++) {
    PrintLiveInterval(*it, "");
    count++;
    if ((len != 0) && (count == len)) {
      break;
    }
  }
}

void LSRALinearScanRegAllocator::PrintActiveListSimple() {
  MapleSet<LiveInterval *, ActiveCmp>::iterator it;
  for (it = active.begin(); it != active.end(); it++) {
    LiveInterval *li = *it;
    uint32 assignedReg = li->assigned_reg;
    if (li->stk_slot == SPILLED) {
      assignedReg = 16;
    }
    LogInfo::MapleLogger() << li->regno << "(" << assignedReg << ", ";
    if (li->phys_use) {
      LogInfo::MapleLogger() << "p) ";
    } else {
      LogInfo::MapleLogger() << li->first_acrossed_call;
    }
    LogInfo::MapleLogger() << "<" << li->first_def << "," << li->last_use << ">) ";
  }
  LogInfo::MapleLogger() << "\n";
}

void LSRALinearScanRegAllocator::PrintLiveIntervals() {
  for (uint32_t i = 0; i < LI_.size(); i++) {
    if (!LI_[i] || LI_[i]->regno == 0) {
      continue;
    }
    PrintLiveInterval(LI_[i], "");
  }
  LogInfo::MapleLogger() << "\n";
}

void LSRALinearScanRegAllocator::DebugCheckActiveList() {
  LiveInterval *prev = nullptr;
  MapleSet<LiveInterval *, ActiveCmp>::iterator it;
  for (it = active.begin(); it != active.end(); it++) {
    LiveInterval *li = *it;
    if (prev != nullptr) {
      if ((li->regno <= V7) && (prev->regno > V7)) {
        if (li->first_def < prev->first_def) {
          LogInfo::MapleLogger() << "ERRer: active list with out of order phys + vreg\n";
          PrintLiveInterval(prev, "prev");
          PrintLiveInterval(li, "current");
          PrintActiveList("Active", 10);
        }
      }
      if ((li->regno <= V7) && (prev->regno <= V7)) {
        if (li->first_def < prev->first_def) {
          LogInfo::MapleLogger() << "ERRer: active list with out of order phys reg use\n";
          PrintLiveInterval(prev, "prev");
          PrintLiveInterval(li, "current");
          PrintActiveList("Active", 10);
        }
      }
    } else {
      prev = li;
    }
  }
}

// Need to initialize the registers that can be allocated.
//
// regty : register type, int or float
// begin : starting physical register number
// end   : ending physical register number
// isCaller : true for caller register, false for callee
void LSRALinearScanRegAllocator::GetAvailablePRregs(RegType regty, uint32 &begin, uint32 &end, bool isCaller) {
  // ==== int regs ====
  // FP 29, LR 30, SP 31, 0 to 7 parameters
  // MapleCG defines 32 as ZR (zero register)
  switch (regty) {
    case kRegTyVary:
    case kRegTyCc:
      begin = kRinvalid;
      end = kRinvalid;
      break;
    case kRegTyInt:
      if (isCaller) {
        begin = 9;
        end = 17;
      } else {
        // when yieldpoint is enabled, x19 is reserved.
        if (cgfunc_->cg->GenYieldpoint()) {
          begin = 20;
        } else {
          begin = 19;
        }
        end = 28;
      }
      break;
    case kRegTyFloat:
      if (isCaller) {
        begin = 16;
        end = 31;
      } else {
        begin = 8;
        end = 15;
      }
      break;
    default:
      CG_ASSERT(false, "NYI");
      break;
  }
}

// Prepare the free physical register pool for allocation.
// When a physical register is allocated, it is removed from the pool.
// The physical register is re-inserted into the pool when the associated live
// interval has ended.
void LSRALinearScanRegAllocator::InitFreeRegPool() {
  // 16 and 17 are intra-procedure call temp, can be caller saved
  uint32 bgn;
  uint32 end;
  GetAvailablePRregs(kRegTyInt, bgn, end, true);
  for (uint32 i = bgn; i <= end; i++) {
    int_caller_reg_set.insert(i);
    int_caller_mask |= (1 << i);
  }
  GetAvailablePRregs(kRegTyInt, bgn, end, false);
  for (uint32 i = bgn; i <= end; i++) {
    int_callee_reg_set.insert(i);
    int_callee_mask |= (1 << i);
  }
  // ==== fp regs ====
  GetAvailablePRregs(kRegTyFloat, bgn, end, true);
  for (uint32 i = bgn; i <= end; i++) {
    fp_caller_reg_set.insert(i);
    fp_caller_mask |= (1 << i);
  }
  GetAvailablePRregs(kRegTyFloat, bgn, end, false);
  for (uint32 i = bgn; i <= end; i++) {
    fp_callee_reg_set.insert(i);
    fp_callee_mask |= (1 << i);
  }
  // Add any physical registers that are not used.
  // R0 to R8,R18 for integer, V0-V7 for float
  int_caller_reg_set.insert(8);
  int_caller_mask |= (1 << 8);
  for (uint32 i = 0; i <= 7; i++) {
    int_param_reg_set.insert(i);
    int_param_mask |= (1 << i);
  }
  int_caller_reg_set.insert(18);
  int_caller_mask |= (1 << 18);
  for (uint32 i = 0; i <= 7; i++) {
    fp_param_reg_set.insert(i);
    fp_param_mask |= (1 << i);
  }
  // The number of registers set aside for spill should equal to the max
  // number of operands in one instruction.  Assume two for now.
  CG_ASSERT(MAX_INT_SPILL == 3, "LinearScanRegAllocator::InitFreeRegPool wrong # of spill regs");
  int_spill_reg_set[0] = 16;
  int_spill_reg_set[1] = 17;
  if (has_3reg_opnd == true) {
    int_spill_reg_set[2] = 15;
    // Remove the corresponding register from caller saved
    int_caller_reg_set.erase(15);
    int_caller_mask &= ~(1 << 15);
  } else {
    max_int_spill--;
  }
  int_caller_reg_set.erase(16);
  int_caller_mask &= ~(1 << 16);
  int_caller_reg_set.erase(17);
  int_caller_mask &= ~(1 << 17);
  CG_ASSERT(MAX_FP_SPILL == 2, "LinearScanRegAllocator::InitFreeRegPool wrong # of spill regs");
  fp_spill_reg_set[0] = 30;
  fp_spill_reg_set[1] = 31;
  fp_caller_reg_set.erase(30);
  fp_caller_reg_set.erase(31);

  if (LSRA_DUMP) {
    PrintRegSet(int_caller_reg_set, "ALLOCATABLE_INT_CALLER");
    PrintRegSet(int_callee_reg_set, "ALLOCATABLE_INT_CALLEE");
    PrintRegSet(int_param_reg_set, "ALLOCATABLE_INT_PARAM");
    PrintRegSet(fp_caller_reg_set, "ALLOCATABLE_FP_CALLER");
    PrintRegSet(fp_callee_reg_set, "ALLOCATABLE_FP_CALLEE");
    PrintRegSet(fp_param_reg_set, "ALLOCATABLE_FP_PARAM");
    LogInfo::MapleLogger() << "INT_SPILL_REGS";
    for (uint i = 0; i < max_int_spill; i++) {
      LogInfo::MapleLogger() << " " << int_spill_reg_set[i];
    }
    LogInfo::MapleLogger() << "\n";
    LogInfo::MapleLogger() << "FP_SPILL_REGS";
    for (uint i = 0; i < MAX_FP_SPILL; i++) {
      LogInfo::MapleLogger() << " " << fp_spill_reg_set[i];
    }
    LogInfo::MapleLogger() << "\n";
    LogInfo::MapleLogger() << std::hex;
    LogInfo::MapleLogger() << "INT_CALLER_MASK " << int_caller_mask << "\n";
    LogInfo::MapleLogger() << "INT_CALLEE_MASK " << int_callee_mask << "\n";
    LogInfo::MapleLogger() << "INT_PARAM_MASK " << int_param_mask << "\n";
    LogInfo::MapleLogger() << "FP_CALLER_FP_MASK " << fp_caller_mask << "\n";
    LogInfo::MapleLogger() << "FP_CALLEE_FP_MASK " << fp_callee_mask << "\n";
    LogInfo::MapleLogger() << "FP_PARAM_FP_MASK " << fp_param_mask << "\n";
    LogInfo::MapleLogger() << std::dec;
  }
}

bool LSRALinearScanRegAllocator::AllPredBBVisited(BB *bb) {
  bool allPredVisited = true;
  for (MapleList<BB *>::iterator predIt = bb->preds.begin(); predIt != bb->preds.end(); ++predIt) {
    BB *predBb = *predIt;
    // See if pred bb is a loop back edge
    bool isBackEdge = false;
    for (MapleList<BB *>::iterator loopIt = predBb->loop_succs.begin(); loopIt != predBb->loop_succs.end(); ++loopIt) {
      BB *loopBb = *loopIt;
      if (loopBb == bb) {
        isBackEdge = true;
        break;
      }
    }
    if ((isBackEdge == false) && (visited_bbs[predBb->id] == false)) {
      allPredVisited = false;
      break;
    }
  }
  for (MapleList<BB *>::iterator predIt = bb->eh_preds.begin(); predIt != bb->eh_preds.end(); ++predIt) {
    BB *predBb = *predIt;
    bool isBackEdge = false;
    for (MapleList<BB *>::iterator loopIt = predBb->loop_succs.begin(); loopIt != predBb->loop_succs.end(); ++loopIt) {
      BB *loopBb = *loopIt;
      if (loopBb == bb) {
        isBackEdge = true;
        break;
      }
    }
    if ((isBackEdge == false) && (visited_bbs[predBb->id] == false)) {
      allPredVisited = false;
      break;
    }
  }
  return allPredVisited;
}

// During live interval construction, bb has only one predecessor and/or one
// successor are stright line bb.  It can be considered to be a single large bb
// for the purpose of finding live interval.  This is to prevent extending live
// interval of registers unnecessarily when interleaving bb from other paths.
BB *LSRALinearScanRegAllocator::MarkStraightLineBBInBFS(BB *bb) {
  while (1) {
    if (bb->succs.size() == 1 && bb->eh_succs.size() == 0) {
      BB *sbb = bb->succs.front();
      if (visited_bbs[sbb->id] == true) {
        break;
      }
      if (sbb->preds.size() == 1 && sbb->eh_preds.size() == 0) {
        sorted_bbs.push_back(sbb);
        visited_bbs[sbb->id] = true;
        bb = sbb;
      } else {
        break;
      }
    } else {
      break;
    }
  }
  return bb;
}

BB *LSRALinearScanRegAllocator::SearchForStraightLineBBs(BB *bb) {
  /* Special case for issue #1863.
   *   Switch cases containing a simple if(){bbs} break;
   * Try to group if and bbs together.
   */
  if (bb->succs.size() != 2 || bb->eh_succs.size() != 0) {
    return bb;
  }
  BB *sbb1 = bb->succs.front();
  BB *sbb2 = bb->succs.back();
  uint32 predSz1 = sbb1->preds.size();
  uint32 predSz2 = sbb2->preds.size();
  BB *candidateBb = nullptr;
  if (predSz1 == 1 && predSz2 > 5) {
    candidateBb = sbb1;
  } else if (predSz2 == 1 && predSz1 > 5) {
    candidateBb = sbb2;
  } else {
    return bb;
  }
  CG_ASSERT(candidateBb->id < visited_bbs.size(),
            "index out of range in LSRALinearScanRegAllocator::SearchForStraightLineBBs");
  if (visited_bbs[candidateBb->id] == true) {
    return bb;
  }
  if (candidateBb->eh_preds.size() != 0) {
    return bb;
  }
  if (candidateBb->succs.size() != 1) {
    return bb;
  }

  sorted_bbs.push_back(candidateBb);
  visited_bbs[candidateBb->id] = true;
  return MarkStraightLineBBInBFS(candidateBb);
}

// breadth first search of bb for live interval computation.
void LSRALinearScanRegAllocator::BFS(BB *curbb) {
  std::queue<BB *> worklist;
  worklist.push(curbb);
  CG_ASSERT(curbb->id < cgfunc_->NumBBs(), "LinearScanRegAllocator::BFS visited_bbs overflow");
  CG_ASSERT(curbb->id < visited_bbs.size(), "index out of range in LSRALinearScanRegAllocator::BFS");
  visited_bbs[curbb->id] = true;
  do {
    BB *bb = worklist.front();
    sorted_bbs.push_back(bb);
    CG_ASSERT(bb->id < cgfunc_->NumBBs(), "LinearScanRegAllocator::BFS visited_bbs overflow");
    visited_bbs[bb->id] = true;
    worklist.pop();
    // Look for straight line bb
    bb = MarkStraightLineBBInBFS(bb);
    // Look for an 'if' followed by some straight-line bb
    bb = SearchForStraightLineBBs(bb);
    for (MapleList<BB *>::iterator it = bb->succs.begin(); it != bb->succs.end(); ++it) {
      BB *ibb = *it;
      // See if there are unvisited predecessor
      if (visited_bbs[ibb->id] == false) {
        if (AllPredBBVisited(ibb) == true) {
          worklist.push(ibb);
          CG_ASSERT(ibb->id < cgfunc_->NumBBs(), "LinearScanRegAllocator::BFS visited_bbs overflow");
          visited_bbs[ibb->id] = true;
        }
      }
    }
  } while (!worklist.empty());
  return;
}

void LSRALinearScanRegAllocator::ComputeBlockOrder() {
  visited_bbs.clear();
  sorted_bbs.clear();
  visited_bbs.resize(cgfunc_->NumBBs());
  for (uint32_t i = 0; i < cgfunc_->NumBBs(); i++) {
    visited_bbs[i] = false;
  }

  bool changed;
  uint32 sortedCnt = 0;
  bool done = false;
  do {
    if (LSRA_DUMP) {
      LogInfo::MapleLogger() << "BFS iteration " << sorted_bbs.size() << " " << cgfunc_->NumBBs() << "\n";
    }
    changed = false;
    FOR_ALL_BB(bb, cgfunc_) {
      if (bb->internal_flag1 == 1) {
        continue;
      }
      if (visited_bbs[bb->id] == false) {
        changed = true;
        if (AllPredBBVisited(bb) == true) {
          BFS(bb);
        }
      }
    }
    // Make sure there is no infinite loop.
    if (sortedCnt == sorted_bbs.size()) {
      if (done == false) {
        done = true;
      } else {
        LogInfo::MapleLogger() << "Error: LSRA BFS loop " << sortedCnt << "in func " << cgfunc_->GetName() << "\n";
        exit(0);
      }
    }
    sortedCnt = sorted_bbs.size();
  } while (changed == true);

  BB *cleanupBb = nullptr;
  FOR_ALL_BB(bb, cgfunc_) {
    if (bb->firststmt == cgfunc_->cleanup_label) {
      cleanupBb = bb;
      break;
    }
  }
  for (BB *bb = cleanupBb; bb; bb = bb->next) {
    sorted_bbs.push_back(bb);
  }

  if (LSRA_DUMP) {
    for (uint32_t i = 0; i < sorted_bbs.size(); i++) {
      LogInfo::MapleLogger() << "\n< === > ";
      LogInfo::MapleLogger() << sorted_bbs[i]->id;
      LogInfo::MapleLogger() << " succs:";
      MapleList<BB *> succs = sorted_bbs[i]->succs;
      for (MapleList<BB *>::iterator it = succs.begin(); it != succs.end(); it++) {
        BB *succBb = static_cast<BB *>(*it);
        LogInfo::MapleLogger() << " " << succBb->id;
      }
      LogInfo::MapleLogger() << " eh_succs:";
      succs = sorted_bbs[i]->eh_succs;
      for (MapleList<BB *>::iterator it = succs.begin(); it != succs.end(); it++) {
        BB *succBb = static_cast<BB *>(*it);
        LogInfo::MapleLogger() << " " << succBb->id;
      }
    }
    LogInfo::MapleLogger() << "\n";
  }
}

// Remember calls for caller/callee allocation.
void LSRALinearScanRegAllocator::RecordCall(Insn *insn) {
  // Maintain call at the beginning of active list
  LiveInterval *li = cgfunc_->memPool->New<LiveInterval>(allocator);
  li->first_def = insn->id;
  li->is_call = insn;
  call_list.push_back(li);
}

// Handle parameter registers for live interval.
void LSRALinearScanRegAllocator::RecordPhysRegs(Insn *insn, RegOperand *regOpnd, uint32 insnNum, bool isDef,
                                                bool isCall) {
  RegType regtype = regOpnd->GetRegisterType();
  uint32 regno = regOpnd->GetRegisterNumber();
  if (regtype == kRegTyCc || regtype == kRegTyVary) {
    return;
  }

  if (IsUntouchableReg(regno) || regOpnd->IsConstReg()) {
    return;
  }

  if (regno == R30) {
    return;
  }

  if (regno >= V0 && regno <= V31) {
    simd_spill_regs.erase(regno);
    simd_spill_0.erase(regno);
    simd_spill_1.erase(regno);
  }

  bool maybeParam = (regtype == kRegTyInt && int_param_queue[regno - R0].size() == 0) ||
                    (regtype == kRegTyFloat && fp_param_queue[regno - V0].size() == 0);

  if (isDef) {
    // parameter register def is assumed to be live until a call.
    LiveInterval *li = cgfunc_->memPool->New<LiveInterval>(allocator);
    li->regno = regno;
    li->regtype = regtype;
    li->stk_slot = -1;
    li->first_def = insnNum;
    li->phys_use = insnNum;
    li->assigned_reg = regno;
    if (regtype == kRegTyInt) {
      int_param_queue[regno - R0].push_back(li);
    } else {
      fp_param_queue[regno - V0].push_back(li);
    }
  } else if (maybeParam) {
    CHECK_FATAL(false, "impossible");
  } else {
    if (regtype == kRegTyInt) {
      CHECK_FATAL((regno - R0) < int_param_queue.size(), "index out of range in LSRALinearScanRegAllocator::RecordPhysRegs");
      LiveInterval *li = int_param_queue[regno - R0].back();
      li->phys_use = insnNum;
    } else {
      LiveInterval *li = fp_param_queue[regno - V0].back();
      li->phys_use = insnNum;
    }
  }
}

// main entry function for live interval computation.
void LSRALinearScanRegAllocator::SetupLiveInterval(Operand *opnd, Insn *insn, bool isDef, uint32 &nUses) {
  if (!opnd->IsRegister()) {
    return;
  }
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 regno = regOpnd->GetRegisterNumber();
  uint32 insnNum = insn->id;
  if (regOpnd->IsPhysicalRegister()) {
    RecordPhysRegs(insn, regOpnd, insnNum, isDef);
    return;
  }
  RegType regtype = regOpnd->GetRegisterType();
  if (regtype == kRegTyCc || regtype == kRegTyVary) {
    return;
  }

  LiveInterval *li = nullptr;
  if (!LI_[regno]) {
    li = cgfunc_->memPool->New<LiveInterval>(allocator);
    li->regno = regno;
    li->regtype = regtype;
    li->stk_slot = -1;
    LI_[regno] = li;
  } else {
    li = LI_[regno];
  }

  if (isDef) {
    if (li->first_def == 0) {
      li->first_def = insnNum;
      li->last_use = insnNum + 1;
    } else if (li->last_use < insnNum) {
      if (insn->bb->unreachable == false) {
        li->last_use = insnNum + 1;
      }
    }
    // try-catch related
    //   Not set when extending live interval with bb's livein in ComputeLiveInterval.
    li->regtype = regtype;
    if (li->use_before_def == true) {
      if (insn->bb->unreachable == false) {
        li->last_use = insnNum + 1;
      }
    }
    li->result_count++;
  } else {
    if (li->first_def == 0) {
      CG_ASSERT(0, "SetupLiveInterval: use before def");
    }
    /*
     * In ComputeLiveInterval when extending live interval using
     * live-out information, li created does not have a type.
     */
    li->regtype = regtype;
    if (insn->bb->unreachable == false) {
      li->last_use = insnNum;
    }
    nUses++;
  }

  if (insn->bb->is_catch) {
    li->SetInCatchState();
  } else {
    li->SetNotInCatchState();
  }

  if (insn->bb->internal_flag1) {
    li->SetInCleanupState();
  } else {
    if (insn->bb->id != 1) {
      li->SetNotInCleanupState(false);
    } else {
      li->SetNotInCleanupState(true);
    }
  }

  li->ref_count++;

  uint32 index = regno / (sizeof(uint64) * 8);
  uint64 bit = regno % (sizeof(uint64) * 8);
  if (reg_used_in_bb[index] & (1 << bit)) {
    li->multi_use_in_bb = true;
  }
  reg_used_in_bb[index] |= (1 << bit);

  if (min_vreg_num > regno) {
    min_vreg_num = regno;
  }
  if (max_vreg_num < regno) {
    max_vreg_num = regno;
  }

  // setup the def/use point for it
  CG_ASSERT(regno < LI_.size(), "out of range of vector LI_");

  return;
}

// Support 'hole' in LSRA.
// For a live interval, there might be multiple segments of live ranges,
// and between these segments a 'hole'.
// Some other short lived vreg can go into these 'holes'.
//
// from : starting instruction sequence id
// to   : ending instruction sequence id
void LSRALinearScanRegAllocator::LiveInterval::AddRange(uint32 from, uint32 to) {
  if (ranges.size() == 0) {
    ranges.push_back(pair<uint32, uint32>(from, to));
  } else {
    if (to < ranges.front().first) {
      ranges.insert(ranges.begin(), pair<uint32, uint32>(from, to));
    } else if (to >= ranges.front().second && from < ranges.front().first) {
      ranges.front().first = from;
      ranges.front().second = to;
    } else if (to >= ranges.front().first && from < ranges.front().first) {
      ranges.front().first = from;
    } else if (from > ranges.front().second) {
      CG_ASSERT(0, "No possible on reverse traverse.");
    } else {
      // Do nothing.
    }
  }
  return;
}

void LSRALinearScanRegAllocator::LiveInterval::AddUsePos(uint32 pos) {
  use_positions.insert(pos);
}

// See if a vreg can fit in one of the holes of a longer live interval.
uint32 LSRALinearScanRegAllocator::FillInHole(LiveInterval *li) {
  MapleSet<LiveInterval *, ActiveCmp>::iterator it;
  for (it = active.begin(); it != active.end(); it++) {
    LiveInterval *ili = static_cast<LiveInterval *>(*it);

    /* If ili is part in cleanup, the hole info will be not correct,
       since cleanup bb do not have edge to normal func bb, and the
       live-out info will not correct.*/
    if (!ili->IsAllOutCleanup() || ili->IsInCatch()) {
      continue;
    }

    if (ili->regtype == li->regtype && ili->stk_slot == -1 && ili->li_child == nullptr && ili->assigned_reg != 0) {
      MapleVector<std::pair<uint32, uint32>>::iterator it;
      for (it = ili->holes.begin(); it != ili->holes.end(); it++) {
        if ((*it).first <= li->first_def && (*it).second >= li->last_use) {
          ili->li_child = li;
          li->li_parent = ili;
          li->assigned_reg = ili->assigned_reg;
          /* No need to modify simd_spill_regs since ili already updated it. */
          // If assigned physical register is callee save register, set should_save false;
          uint32 begin = 0;
          uint32 end = 0;
          uint32 phyRegIndx = 0;
          GetAvailablePRregs(li->regtype, begin, end, false);
          if (li->regtype == kRegTyInt) {
            phyRegIndx = li->assigned_reg - R0;
          } else if (li->regtype == kRegTyFloat) {
            phyRegIndx = li->assigned_reg - V0;
          } else {
            CG_ASSERT(false, "FillInHole, Invalid register type");
          }

          if (phyRegIndx >= begin && phyRegIndx <= end) {
            li->should_save = false;
          }
          return ili->assigned_reg;
        } else if ((*it).first > li->last_use) {
          break;
        }
      }
    }
  }
  return 0;
}

// Support finding holes by searching for ranges where holes exist.
void LSRALinearScanRegAllocator::BuildIntervalRanges() {
  uint32_t bbIdx = sorted_bbs.size();
  uint32 regno;
  if (bbIdx == 0) {
    return;
  }

  do {
    bbIdx--;
    BB *bb = sorted_bbs[bbIdx];
    if (bb->firstinsn == nullptr || bb->lastinsn == nullptr) {
      continue;
    }
    uint32 blockFrom = bb->firstinsn->id;
    uint32 blockTo = bb->lastinsn->id + 1;

    for (auto it = bb->liveout_regno.begin(); it != bb->liveout_regno.end(); it++) {
      regno = static_cast<regno_t>(*it);
      if (regno < kMaxRegNum) {
        // Do not consider physical regs.
        continue;
      }
      LI_[regno]->AddRange(blockFrom, blockTo);
    }

    FOR_BB_INSNS_REV(insn, bb) {
      const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
      for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
        Operand *opnd = insn->GetOperand(i);
        if (!opnd) {
          continue;
        }

        if (opnd->IsMemoryAccessOperand()) {
          MemOperand *memopnd = static_cast<MemOperand *>(opnd);
          Operand *base = memopnd->GetBaseRegister();
          Operand *offset = memopnd->GetIndexRegister();
          if (base != nullptr && base->IsRegister()) {
            RegOperand *regOpnd = static_cast<RegOperand *>(base);
            RegType regtype = regOpnd->GetRegisterType();
            if (regtype != kRegTyCc && regtype != kRegTyVary) {
              regno = regOpnd->GetRegisterNumber();
              if (regno > kMaxRegNum) {
                LI_[regno]->AddRange(blockFrom, insn->id);
                LI_[regno]->use_positions.insert(insn->id);
              }
            }
          }
          if (offset != nullptr && offset->IsRegister()) {
            RegOperand *regOpnd = static_cast<RegOperand *>(offset);
            RegType regtype = regOpnd->GetRegisterType();
            if (regtype != kRegTyCc && regtype != kRegTyVary) {
              regno = regOpnd->GetRegisterNumber();
              if (regno > kMaxRegNum) {
                LI_[regno]->AddRange(blockFrom, insn->id);
                LI_[regno]->use_positions.insert(insn->id);
              }
            }
          }
        } else {
          if (opnd->IsRegister()) {
            RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
            RegType regtype = regOpnd->GetRegisterType();
            if (regtype != kRegTyCc && regtype != kRegTyVary) {
              bool isdef = static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegDef();
              bool isuse = static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegUse();
              regno = regOpnd->GetRegisterNumber();
              if (regno > kMaxRegNum) {
                if (isdef) {
                  if (!LI_[regno]->ranges.empty()) {
                    LI_[regno]->ranges.front().first = insn->id;
                    LI_[regno]->use_positions.insert(insn->id);
                  }
                }
                if (isuse) {
                  LI_[regno]->AddRange(blockFrom, insn->id);
                  LI_[regno]->use_positions.insert(insn->id);
                }
              }
            }
          }
        }
      }
    }
  } while (bbIdx != 0);

  // Build holes.
  for (int i = 0; i < cgfunc_->GetMaxVReg(); i++) {
    LiveInterval *li = LI_[i];
    if (li == nullptr) {
      continue;
    }
    if (li->ranges.size() < 2) {
      continue;
    }

    MapleVector<std::pair<uint32, uint32>>::iterator it;
    MapleVector<std::pair<uint32, uint32>>::iterator itPrev = li->ranges.end();

    for (it = li->ranges.begin(); it != li->ranges.end(); it++) {
      if (itPrev == li->ranges.end()) {
        itPrev = it;
        continue;
      }
      if (((*it).first - (*itPrev).second) > 2) {
        li->holes.push_back(std::pair<uint32, uint32>((*itPrev).second, (*it).first));
      }
      itPrev = it;
    }
  }
}

void LSRALinearScanRegAllocator::ComputeLiveInterval()
// Preference is to put bracket as 1st char of a newline
{
  uint32_t insnNum;
  // initialize block ordering
  // Can be either breadth first or depth first.
  // To avoid use before set, we prefer breadth first
  ComputeBlockOrder();

  callee_use_cnt.resize(kMaxRegNum);
  LI_.resize(cgfunc_->GetMaxVReg());
  // LiveInterval queue for each param register
  last_int_param_li.resize(8);
  last_fp_param_li.resize(8);

  uint32 bgn;
  uint32 end;
  GetAvailablePRregs(kRegTyFloat, bgn, end, false);
  for (uint32 i = bgn; i <= end; i++) {
    simd_spill_regs.insert(i + V0);
    simd_spill_0.insert(i + V0);
    simd_spill_1.insert(i + V0);
  }
  insnNum = 1;

  for (uint32_t bbIdx = 0; bbIdx < sorted_bbs.size(); bbIdx++) {
    BB *bb = sorted_bbs[bbIdx];
    // Extend live interval with live-in info
    for (auto it = bb->livein_regno.begin(); it != bb->livein_regno.end(); it++) {
      regno_t regno = static_cast<regno_t>(*it);
      if (regno < kMaxRegNum) {
        // Do not consider physical regs.
        continue;
      }
      LiveInterval *li = LI_[regno];
      if (!li && (!bb->IsEmpty() || bb->id == 1)) {
        // try-catch related
        //   Since it is livein but not seen before, its a use before def
        LiveInterval *li = cgfunc_->memPool->New<LiveInterval>(allocator);
        li->regno = regno;
        // li->regtype  = regtype; // Set in SetupLiveInterval
        li->stk_slot = -1;
        LI_[regno] = li;
        li->first_def = insnNum;
        li->use_before_def = true;
        if (bb->unreachable == false && static_cast<AArch64CGFunc *>(cgfunc_)->GetJavaCatchRegno() != regno) {
          if (bb->id != 1) {
            LogInfo::MapleLogger() << "ERROR: " << regno << " use before def in bb " << bb->id << " : " << cgfunc_->GetName() << "\n";
            CHECK_FATAL(0, "There should only be [use before def in bb 1], temporarily.");
          }
          LogInfo::MapleLogger() << "WARNING: " << regno << " use before def in bb " << bb->id << " : " << cgfunc_->GetName() << "\n";
        }
        // Need to insert to active list now, as live interval is
        // conservatively to start at instruction 1
        active.insert(li);

        if (bb->is_catch) {
          li->SetInCatchState();
        } else {
          li->SetNotInCatchState();
        }

        if (bb->internal_flag1) {
          li->SetInCleanupState();
        } else {
          if (bb->id != 1) {
            li->SetNotInCleanupState(false);
          } else {
            li->SetNotInCleanupState(true);
          }
        }
      }
    }

    if (LSRA_DUMP) {
      LogInfo::MapleLogger() << "bb(" << bb->id << ")LIVEOUT:";
      MapleSet<regno_t>::iterator it;
      for (auto it = bb->liveout_regno.begin(); it != bb->liveout_regno.end();
           it++) {
        regno_t regno = static_cast<regno_t>(*it);
        LogInfo::MapleLogger() << " " << regno;
      }
      LogInfo::MapleLogger() << ".\n";
      LogInfo::MapleLogger() << "bb(" << bb->id << ")LIVEIN:";
      for (auto it = bb->livein_regno.begin(); it != bb->livein_regno.end(); it++) {
        regno_t regno = static_cast<regno_t>(*it);
        LogInfo::MapleLogger() << " " << regno;
      }
      LogInfo::MapleLogger() << ".\n";
    }

    reg_used_in_bb_sz = (cgfunc_->GetMaxVReg() / (sizeof(uint64) * 8) + 1);
    reg_used_in_bb = new uint64[reg_used_in_bb_sz];
    errno_t rc = memset_s(reg_used_in_bb, reg_used_in_bb_sz * sizeof(uint64), 0, reg_used_in_bb_sz * sizeof(uint64));
    CG_ASSERT(rc == EOK, "call memset_s failed in LSRALinearScanRegAllocator::ComputeLiveInterval()");
    if (bb->firstinsn != nullptr) {
      if (!bb->eh_preds.empty()) {
        bb->livein_regno.insert(R0);
        bb->livein_regno.insert(R1);
      }
      //  traverse live in regno, for each live in regno create a new liveinterval
      for (auto it = bb->livein_regno.begin(); it != bb->livein_regno.end(); it++) {
        regno_t regno = static_cast<regno_t>(*it);
        if ((regno >= R0 && regno <= R7) || (regno >= V0 && regno <= V7)) {
          LiveInterval *li = cgfunc_->memPool->New<LiveInterval>(allocator);
          li->regno = regno;
          li->stk_slot = -1;
          li->first_def = insnNum;
          li->phys_use = insnNum;
          li->assigned_reg = regno;
          if (regno >= R0 && regno <= R7) {
            li->regtype = kRegTyInt;
            int_param_queue[regno - R0].push_back(li);
          } else {
            li->regtype = kRegTyFloat;
            fp_param_queue[regno - V0].push_back(li);
          }
        }
      }
      if (!bb->eh_preds.empty()) {
        bb->livein_regno.erase(R0);
        bb->livein_regno.erase(R1);
      }
    }

    FOR_BB_INSNS(insn, bb) {
      uint32 numUses;
      insn->id = insnNum;

      // skip comment and debug insn
      if (insn->IsImmaterialInsn()) {
        continue;
      }
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      if (insn->mop_ == MOP_clinit) {
        RegOperand *dst = static_cast<RegOperand *>(insn->GetOperand(0));
        RegOperand *phyOpnd = static_cast<AArch64CGFunc *>(cgfunc_)->GetOrCreatePhysicalRegisterOperand(
          (AArch64reg_t)(R30), dst->GetSize(), dst->GetRegisterType());
        insn->SetOperand(0, phyOpnd);
      }

      if (insn->IsCall()) {
        bool skipCall = false;
        if (!insn->IsIndirectCall()) {
          FuncNameOperand *target = dynamic_cast<FuncNameOperand *>(insn->GetCallTargetOperand());
          if (target) {
            MIRSymbol *funcst = target->GetFunctionSymbol();
            CG_ASSERT(funcst->sKind == kStFunc, "");
            if (funcst->GetName() == "exit") {
              skipCall = true;
            } else if ((localrefvar_max_stack_loc == 0) && (funcst->GetName() == GetIntrinsicFuncName(INTRN_MCCInitializeLocalStackRef))) {
              // Detecting local ref var init size and start location.
              localrefvar_min_stack_loc = FindLocalRefVarStackLoc(insn, R0);
              uint32 slots = FindLocalRefVarStackLoc(insn, R1);
              localrefvar_max_stack_loc = localrefvar_min_stack_loc + ((slots - 1) << 3);
              lvar_offset_regno_map.resize(slots);
              if (LSRA_DUMP) {
                LogInfo::MapleLogger() << "localrefvar stack loc " << localrefvar_min_stack_loc << " to " << localrefvar_max_stack_loc
                     << " slots " << slots << "\n";
              }
            }
          }
        }

        if (!skipCall) {
          if (!insn->is_throw || !bb->eh_succs.empty()) {
            RecordCall(insn);
          }
        }
      }

      numUses = 0;
      const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];

      // we need to process src opnd first just in case the src/dest vreg are the same and the src vreg belongs to the
      // last interval.
      bool isLocalrefvar = false;
      uint32 immOffset = 0;
      for (int32_t i = Insn::kMaxOperandNum - 1; i >= 0; i--) {
        Operand *opnd = insn->GetOperand(i);
        if (!opnd) {
          continue;
        }

        bool isdef = static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegDef();
        if (opnd->IsList()) {
          ListOperand *listopnd = static_cast<ListOperand *>(opnd);
          for (auto op : listopnd->GetOperands()) {
            SetupLiveInterval(op, insn, isdef, numUses);
          }
        } else if (opnd->IsMemoryAccessOperand()) {
          MemOperand *memopnd = static_cast<MemOperand *>(opnd);
          Operand *base = memopnd->GetBaseRegister();
          Operand *offset = memopnd->GetIndexRegister();
          isdef = false;
          // ldr(156) (opnd0:  reg:V34 class: [F]) (opnd1: Mem:literal:
          // .LB_Ljava_2Fnio_2FByteBuffer_3B_7CgetDouble_7C_28_29D2)
          if (base != nullptr) {
            SetupLiveInterval(base, insn, isdef, numUses);
          }
          if (offset != nullptr) {
            SetupLiveInterval(offset, insn, isdef, numUses);
          }

          // Find a local ref var reference
          if (base && localrefvar_max_stack_loc != 0 && base->IsRegister()) {
            RegOperand *regOpnd = static_cast<RegOperand *>(base);
            if (regOpnd->GetRegisterNumber() == R29) {
              AArch64MemOperand *mem = static_cast<AArch64MemOperand *>(opnd);
              AArch64OfstOperand *imm = mem->GetOffsetImmediate();
              if (imm != nullptr) {
                immOffset = static_cast<uint32>(imm->GetOffsetValue());
                isLocalrefvar = DetectLocalRefVarInfo(insn, immOffset);
              }
            }
          }
        } else {
          SetupLiveInterval(opnd, insn, isdef, numUses);
        }
      }
      if (isLocalrefvar) {
        MarkLocalRefVarForLiveInterval(insn, immOffset);
      }
      if (numUses > 2) {
        CG_ASSERT((numUses < 4), "Num of instruction use operands larger than 3");
        has_3reg_opnd = true;
      }
      insnNum++;
    }
    //  traverse live out regno
    //  for each live out regno if the last corresponding live interval is created within this bb
    //  update this li->last_use to the end of BB
    for (auto it = bb->liveout_regno.begin(); it != bb->liveout_regno.end(); it++) {
      regno_t regno = static_cast<regno_t>(*it);
      if ((regno >= R0 && regno <= R7) || (regno >= V0 && regno <= V7)) {
        LiveInterval *li = nullptr;
        if (regno >= R0 && regno <= R7) {
          if (int_param_queue[regno - R0].size() == 0) {
            continue;
          }
          li = int_param_queue[regno - R0].back();
          if (bb->firstinsn && li->first_def >= bb->firstinsn->id) {
            li->phys_use = insnNum;
          }
        } else {
          if (fp_param_queue[regno - V0].size() == 0) {
            continue;
          }
          li = fp_param_queue[regno - V0].back();
          if (bb->firstinsn && li->first_def >= bb->firstinsn->id) {
            li->phys_use = insnNum;
          }
        }
      }
    }

    delete[] reg_used_in_bb;
    reg_used_in_bb = nullptr;
    max_insn_num = insnNum - 1;  // insn_num started from 1

    // Extend live interval with live-out info
    for (auto it = bb->liveout_regno.begin(); it != bb->liveout_regno.end(); it++) {
      regno_t regno = static_cast<regno_t>(*it);
      LiveInterval *li = LI_[regno];
      if (li != nullptr && !bb->IsEmpty()) {
        li->last_use = bb->lastinsn->id;

        if (bb->is_catch) {
          li->SetInCatchState();
        } else {
          li->SetNotInCatchState();
        }

        if (bb->internal_flag1) {
          li->SetInCleanupState();
        } else {
          if (bb->id != 1) {
            li->SetNotInCleanupState(false);
          } else {
            li->SetNotInCleanupState(true);
          }
        }
      }
    }
  }

  for (uint32_t i = 0; i < LI_.size(); i++) {
    LiveInterval *li = LI_[i];
    if (!li || li->regno == 0) {
      continue;
    }
    if (li->is_call != nullptr || li->phys_use) {
      continue;
    }
    if (li->last_use > li->first_def) {
      li->priority = static_cast<float>(li->ref_count) / static_cast<float>(li->last_use - li->first_def);
    } else {
      li->priority = static_cast<float>(li->ref_count) / static_cast<float>(li->first_def - li->last_use);
    }
  }

  if (LSRA_DUMP) {
    PrintLiveIntervals();
  }
  return;
}

// A physical register is freed at the end of the live interval.  Return to pool.
void LSRALinearScanRegAllocator::ReturnPregToSet(LiveInterval *li, uint32 preg) {
  if (preg == 0) {
    return;
  }
  if (li->regtype == kRegTyInt) {
    preg -= R0;
  } else if (li->regtype == kRegTyFloat) {
    preg -= V0;
  } else {
    CG_ASSERT(0, "ReturnPregToSet: Invalid reg type");
  }
  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "\trestoring preg " << preg << " as allocatable\n";
  }
  uint32 mask = 1 << preg;
  if (preg == 16 && li->stk_slot == -1) {
    // this reg is temporary used for liveinterval which last_use-first_def == 1
  } else if (li->regtype == kRegTyInt) {
    if (int_caller_mask & mask) {
      int_caller_reg_set.insert(preg);
    } else if (int_callee_mask & mask) {
      int_callee_reg_set.insert(preg);
    } else if (int_param_mask & mask) {
      int_param_reg_set.insert(preg);
    } else {
      CG_ASSERT(0, "ReturnPregToSet: Unknown caller/callee type");
    }
  } else if (fp_caller_mask & mask) {
    fp_caller_reg_set.insert(preg);
  } else if (fp_callee_mask & mask) {
    fp_callee_reg_set.insert(preg);
  } else if (fp_param_mask & mask) {
    fp_param_reg_set.insert(preg);
  } else {
    CG_ASSERT(0, "ReturnPregToSet invalid physical register");
  }
  return;
}

// A physical register is removed from allocation as it is assigned.
void LSRALinearScanRegAllocator::ReleasePregToset(LiveInterval *li, uint32 preg) {
  if (preg == 0) {
    return;
  }
  if (li->regtype == kRegTyInt) {
    preg -= R0;
  } else if (li->regtype == kRegTyFloat) {
    preg -= V0;
  } else {
    CG_ASSERT(0, "ReleasePregToset: Invalid reg type");
  }
  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "\treleasing preg " << preg << " as allocatable\n";
  }
  uint32 mask = 1 << preg;
  if (preg == 16 && li->stk_slot == -1) {
    // this reg is temporary used for liveinterval which last_use-first_def == 1
  } else if (li->regtype == kRegTyInt) {
    if (int_caller_mask & mask) {
      int_caller_reg_set.erase(preg);
    } else if (int_callee_mask & mask) {
      int_callee_reg_set.erase(preg);
    } else if (int_param_mask & mask) {
      int_param_reg_set.erase(preg);
    } else {
      CG_ASSERT(0, "ReleasePregToset: Unknown caller/callee type");
    }
  } else if (fp_caller_mask & mask) {
    fp_caller_reg_set.erase(preg);
  } else if (fp_callee_mask & mask) {
    fp_callee_reg_set.erase(preg);
  } else if (fp_param_mask & mask) {
    fp_param_reg_set.erase(preg);
  } else {
    CG_ASSERT(0, "ReleasePregToset invalid physical register");
  }
  return;
}

// Remove a live interval from 'active' list.
void LSRALinearScanRegAllocator::RetireFromActive(const Insn *insn) {
  if ((insn->mop_ == MOP_adrp_ldr && insn->next && insn->next->mop_ == MOP_clinit_tail) ||
      (insn->mop_ == MOP_clinit_tail)) {
    // Cannot spill for clinit pair
  } else if (spill_all) {
    return;
  }
  int instrNum = insn->id;
  // active list is sorted based on increasing last_use
  // any operand whose use is greater than current
  // instruction number is still in use.
  // If the use is less than or equal to instruction number
  // then it is possible to retire this live interval and
  // reclaim the physical register associated with it.
  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "RetireFromActive instr_num " << instrNum << "\n";
  }
  // Retire call from call queue
  for (auto it = call_list.begin(); it != call_list.end();) {
    LiveInterval *li = static_cast<LiveInterval *>(*it);
    if (li->first_def <= instrNum) {
      call_list.pop_front();
      // at here, it is invalidated
      it = call_list.begin();
    } else {
      break;
    }
  }

  for (uint32 i = 0; i < int_param_queue.size(); i++) {
    // push back the param not yet use  <-  as only one is popped, just push it back again
    if (last_int_param_li[i]) {
      int_param_queue[i].push_front(last_int_param_li[i]);
      int_param_reg_set.insert(i);
      last_int_param_li[i] = nullptr;
    }
    if (last_fp_param_li[i]) {
      fp_param_queue[i].push_front(last_fp_param_li[i]);
      fp_param_reg_set.insert(i);
      last_fp_param_li[i] = nullptr;
    }
  }

  // Retire live intervals from active list
  MapleSet<LiveInterval *, ActiveCmp>::iterator it;
  for (it = active.begin(); it != active.end(); /* erase will update */) {
    LiveInterval *li = static_cast<LiveInterval *>(*it);
    if (li->last_use > instrNum) {
      break;
    }
    // Is it phys reg?
    if ((li->regno >= R0) && (li->regno <= R7)) {
      if (li->phys_use != 0 && li->phys_use <= instrNum) {
        it = active.erase(it);
        if (li->phys_use != 0) {
          ReturnPregToSet(li, li->regno);
        }
        if (LSRA_DUMP) {
          PrintLiveInterval(li, "\tRemoving phys_reg li\n");
        }
      } else {
        it++;
      }
      continue;
    } else if ((li->regno >= V0) && (li->regno <= V7)) {
      if (li->phys_use != 0 && li->phys_use <= instrNum) {
        it = active.erase(it);
        if (li->phys_use != 0) {
          ReturnPregToSet(li, li->regno);
        }
        if (LSRA_DUMP) {
          PrintLiveInterval(li, "\tRemoving phys_reg li\n");
        }

      } else {
        it++;
      }
      continue;
    }
    // live interval ended for this reg in active
    // release physical reg assigned to free reg pool
    if (li->li_parent) {
      li->li_parent->li_child = nullptr;
      li->li_parent = nullptr;
    } else {
      ReturnPregToSet(li, li->assigned_reg);
    }
    if (LSRA_DUMP) {
      LogInfo::MapleLogger() << "Removing "
           << "(" << li->assigned_reg << ")"
           << "from regset\n";
      PrintLiveInterval(li, "\tRemoving virt_reg li\n");
    }
    it = active.erase(it);
  }
}

// the return value is a physical reg
uint32 LSRALinearScanRegAllocator::GetRegFromSet(MapleSet<uint32> &set, regno_t offset, LiveInterval *li,
                                                 regno_t forcedReg) {
  uint32 regno;
  if (forcedReg) {
    // forced_reg is a caller save reg
    regno = forcedReg;
  } else {
    CHECK(set.size() > 0, "set is null in LSRALinearScanRegAllocator::GetRegFromSet");
    regno = *(set.begin());
  }
  set.erase(regno);
  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "\tAssign " << regno << "\n";
  }
  regno += offset;  // Mapping into Maplecg reg
  li->assigned_reg = regno;
  if (regno >= V0 && regno <= V31) {
    simd_spill_regs.erase(regno);
    simd_spill_0.erase(regno);
    simd_spill_1.erase(regno);
  }
  if (LSRA_DUMP) {
    PrintRegSet(set, "Reg Set AFTER");
    PrintLiveInterval(li, "LiveInterval after assignment");
  }
  return regno;
}

// Handle adrp register assignment. Use the same register for the next
// instruction.
uint32 LSRALinearScanRegAllocator::AssignSpecialPhysRegPattern(Insn *insn, LiveInterval *li) {
  MOperator opcode = insn->GetMachineOpcode();
  if (opcode != MOP_xadrp) {
    return 0;
  }
  Insn *ninsn = insn->next;
  if (ninsn == nullptr || !ninsn->IsMachineInstruction() || ninsn->IsDMBInsn()) {
    return 0;
  }

  const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(ninsn)->mop_];
  bool isdef = static_cast<AArch64OpndProp *>(md->operand_[0])->IsRegDef();
  if (!isdef) {
    return 0;
  }
  Operand *opnd = ninsn->GetOperand(0);
  if (!opnd->IsRegister()) {
    return 0;
  }
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  if (!regOpnd->IsPhysicalRegister()) {
    return 0;
  }
  uint32 regno = regOpnd->GetRegisterNumber();
  if (!(regno >= R0 && regno <= R7)) {
    return 0;
  }

  // next insn's dest is a physical param reg 'regno'
  bool match = false;
  for (int32 i = 1; i < Insn::kMaxOperandNum; i++) {
    Operand *src = ninsn->GetOperand(i);
    if (!src) {
      continue;
    }
    if (src->IsMemoryAccessOperand()) {
      MemOperand *memopnd = static_cast<MemOperand *>(src);
      Operand *base = memopnd->GetBaseRegister();
      if (base) {
        RegOperand *regSrc = static_cast<RegOperand *>(base);
        uint32 srcRegno = regSrc->GetRegisterNumber();
        if (li->regno == srcRegno) {
          match = true;
          break;
        }
      }
      Operand *offset = memopnd->GetIndexRegister();
      if (offset) {
        RegOperand *regSrc = static_cast<RegOperand *>(offset);
        uint32 srcRegno = regSrc->GetRegisterNumber();
        if (li->regno == srcRegno) {
          match = true;
          break;
        }
      }
    } else if (src->IsRegister()) {
      RegOperand *regSrc = static_cast<RegOperand *>(src);
      uint32 srcRegno = regSrc->GetRegisterNumber();
      if (li->regno == srcRegno) {
        bool srcIsdef = static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegDef();
        if (srcIsdef) {
          break;
        }
        match = true;
        break;
      }
    }
  }
  if (match && li->last_use > ninsn->id) {
    return 0;
  }
  // dest of adrp is src of next insn
  if (match) {
    return GetRegFromSet(int_param_reg_set, R0, li, regno - R0);
  }
  return 0;
}

// Return a phys register number for the live interval.
uint32 LSRALinearScanRegAllocator::FindAvailablePhyReg(LiveInterval *li, Insn *insn) {
  uint32 regno = 0;
  if (fast_alloc) {
    if (li->regtype == kRegTyInt) {
      if (int_callee_reg_set.size() != 0) {
        regno = GetRegFromSet(int_callee_reg_set, R0, li);
        li->should_save = false;
      } else if (int_caller_reg_set.size() != 0) {
        regno = GetRegFromSet(int_caller_reg_set, R0, li);
        li->should_save = true;
      } else {
        li->should_save = false;
      }
    } else if (li->regtype == kRegTyFloat) {
      if (fp_callee_reg_set.size() != 0) {
        regno = GetRegFromSet(fp_callee_reg_set, V0, li);
        li->should_save = false;
      } else if (fp_caller_reg_set.size() != 0) {
        regno = GetRegFromSet(fp_caller_reg_set, V0, li);
        li->should_save = true;
      } else {
        li->should_save = false;
      }
    }
    return regno;
  }

  uint32 vregno = li->regno;
  bool saveAcrossCall = false;
  // See if register is live accross a call
  for (MapleList<LiveInterval *>::iterator it = call_list.begin(); it != call_list.end(); it++) {
    LiveInterval *cli = static_cast<LiveInterval *>(*it);
    if (cli->first_def > li->last_use) {
      break;
    }
    bool isLiveout = false;
    Insn *call = cli->is_call;
    // Determine if vreg is live out from bb
    if (g->optim_level >= 2 && call->bb->liveout_regno.find(vregno) != call->bb->liveout_regno.end()) {
      isLiveout = true;
    }
    // Determine if live interval crosses the call
    if (isLiveout && (cli->first_def > li->first_def) && (cli->first_def < li->last_use)) {
      li->should_save = true;

      // Need to spill/fill around this call
      if (!saveAcrossCall) {
        li->first_acrossed_call = call->id;
        saveAcrossCall = true;
      }
    }
  }

  if (li->regtype == kRegTyInt) {
    if (saveAcrossCall) {
      if (LSRA_DUMP) {
        LogInfo::MapleLogger() << "\t\tlive interval crosses a call\n";
      }
      if (regno == 0) {
        if (li->IsInCatch() == false && li->IsAllInCleanupOrFirstBB() == false && int_callee_reg_set.size() != 0) {
          // call in live interval, use callee if available
          regno = GetRegFromSet(int_callee_reg_set, R0, li);
          // Since it is callee saved, no need to continue search
          li->should_save = false;
        } else if (li->multi_use_in_bb) {
          // allocate caller save if there are multiple uses in one bb
          // else it is no different from spilling
          if (int_caller_reg_set.size() != 0) {
            regno = GetRegFromSet(int_caller_reg_set, R0, li);
          } else if (int_param_reg_set.size() != 0) {
            regno = GetRegFromSet(int_param_reg_set, R0, li);
          }
        }
      }
      if (regno == 0) {
        // No register left for allocation
        regno = FillInHole(li);
        if (regno == 0) {
          li->should_save = false;
        }
      }
      return regno;
    } else {
      if (LSRA_DUMP) {
        LogInfo::MapleLogger() << "\t\tlive interval does not cross a call\n";
      }
      regno = AssignSpecialPhysRegPattern(insn, li);
      if (regno) {
        return regno;
      }
      if (int_param_reg_set.size() != 0) {
        regno = GetRegFromSet(int_param_reg_set, R0, li);
      }
      if (regno == 0) {
        if (int_caller_reg_set.size() != 0) {
          regno = GetRegFromSet(int_caller_reg_set, R0, li);
        } else if (int_callee_reg_set.size() != 0) {
          regno = GetRegFromSet(int_callee_reg_set, R0, li);
        } else {
          regno = FillInHole(li);
        }
      }
      return regno;
    }
  } else if (li->regtype == kRegTyFloat) {
    if (saveAcrossCall) {
      if (LSRA_DUMP) {
        LogInfo::MapleLogger() << "\t\tlive interval crosses a call\n";
      }
      if (regno == 0) {
        if (li->IsInCatch() == false && li->IsAllInCleanupOrFirstBB() == false && fp_callee_reg_set.size() != 0) {
          // call in live interval, use callee if available
          regno = GetRegFromSet(fp_callee_reg_set, V0, li);
          // Since it is callee saved, no need to continue search
          li->should_save = false;
        } else if (li->multi_use_in_bb) {
          // allocate caller save if there are multiple uses in one bb
          // else it is no different from spilling
          if (fp_caller_reg_set.size() != 0 && li->multi_use_in_bb == true) {
            regno = GetRegFromSet(fp_caller_reg_set, V0, li);
          } else if (fp_param_reg_set.size() != 0) {
            regno = GetRegFromSet(fp_param_reg_set, V0, li);
          }
        }
      }
      if (regno == 0) {
        // No register left for allocation
        regno = FillInHole(li);
        if (regno == 0) {
          li->should_save = false;
        }
      }
      return regno;
    } else {
      if (LSRA_DUMP) {
        LogInfo::MapleLogger() << "\t\tlive interval does not cross a call\n";
      }
      if (fp_param_reg_set.size() != 0) {
        regno = GetRegFromSet(fp_param_reg_set, V0, li);
      }
      if (regno == 0) {
        if (fp_caller_reg_set.size() != 0) {
          regno = GetRegFromSet(fp_caller_reg_set, V0, li);
        } else if (fp_callee_reg_set.size() != 0) {
          regno = GetRegFromSet(fp_callee_reg_set, V0, li);
        } else {
          regno = FillInHole(li);
        }
      }
      return regno;
    }
  }

  return regno;
}

// Spill and reload for caller saved registers.
void LSRALinearScanRegAllocator::InsertCallerSave(Insn *insn, Operand *opnd, bool isDef) {
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 vregno = regOpnd->GetRegisterNumber();
  CHECK_FATAL(vregno < LI_.size(), "index out of range in LSRALinearScanRegAllocator::InsertCallerSave");
  LiveInterval *rli = LI_[vregno];
  RegType regtype = regOpnd->GetRegisterType();

  is_spill_zero = false;
  if (isDef == false) {
    uint32 mask;
    uint32 regbase;
    if (regtype == kRegTyInt) {
      mask = int_bb_def_mask;
      regbase = R0;
    } else {
      mask = fp_bb_def_mask;
      regbase = V0;
    }
    if (mask & (1 << (rli->assigned_reg - regbase))) {
      if (LSRA_DUMP) {
        LogInfo::MapleLogger() << "InsertCallerSave " << rli->assigned_reg << " skipping due to local def\n";
      }
      return;
    }
  }

  if (!rli->should_save) {
    return;
  }

  uint32 regsize = regOpnd->GetSize();
  PrimType stype;

  if (regtype == kRegTyInt) {
    stype = (regsize <= 32) ? PTY_i32 : PTY_i64;
    int_bb_def_mask |= (1 << (rli->assigned_reg - R0));
  } else {
    CG_ASSERT(regsize != 128, "NYI");
    stype = (regsize <= 32) ? PTY_f32 : PTY_f64;
    fp_bb_def_mask |= (1 << (rli->assigned_reg - V0));
  }

  if (use_localvar_spill == false && UseSimdForSpill(insn, opnd, isDef, 0)) {
    return;
  }

  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "InsertCallerSave " << vregno << "\n";
  }

  if (isDef == false && rli->is_caller_spilled == false) {
    LogInfo::MapleLogger() << "WARNING: " << vregno << " caller restore without spill in bb " << insn->bb->id << " : "
         << cgfunc_->GetName() << "\n";
  }
  rli->is_caller_spilled = true;

  if (isDef) {
    MOperator opcode = insn->GetMachineOpcode();
    if (opcode == MOP_xmovri64 || opcode == MOP_xmovri32) {
      Operand *opnd1 = insn->GetOperand(1);
      AArch64ImmOperand *imm = static_cast<AArch64ImmOperand *>(opnd1);
      if (imm->IsZero()) {
        is_spill_zero = true;
      }
    } else if (opcode == MOP_wmovrr || opcode == MOP_xmovrr) {
      RegOperand *opnd1 = static_cast<RegOperand *>(insn->GetOperand(1));
      if (opnd1 && opnd1->IsZeroRegister()) {
        is_spill_zero = true;
      }
    }
    if (is_spill_zero) {
      // This has to be a caller register
      int_bb_def_mask &= ~(1 << (rli->assigned_reg - R0));
    }
  }

  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
  CG *cg = a64cgfunc->cg;
  MemOperand *memopnd = nullptr;
  RegOperand *phyopnd = nullptr;

  if (is_spill_zero) {
    phyopnd = AArch64RegOperand::GetZeroRegister(regsize);
  } else {
    phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)rli->assigned_reg, regsize, regtype);
  }

  std::string comment;
  uint8 isOutOfRange = 0;
  if (isDef) {
    memopnd = GetSpillMem(vregno, 1, insn, (AArch64reg_t)(int_spill_reg_set[0] + R0), isOutOfRange);
    Insn *stInsn = cg->BuildInstruction<AArch64Insn>(a64cgfunc->PickStInsn(regsize, stype), phyopnd, memopnd);
    stInsn->SetSpillOp();
    comment = " SPILL for caller_save " + std::to_string(vregno);
    caller_save_spill_count++;
    if (rli->last_use == insn->id) {
      if (LI_[vregno]->IsLocalRefVar() == false) {
        a64cgfunc->FreeSpillRegMem(vregno);
      }
      comment += " end";
    }
    if (use_localvar_spill) {
      comment += " LocalVarSpill";
    }
    stInsn->AddComment(comment);
    if (isOutOfRange) {
      insn->bb->InsertInsnAfter(insn->next, stInsn);
    } else {
      insn->bb->InsertInsnAfter(insn, stInsn);
    }
  } else {
    memopnd = GetSpillMem(vregno, 0, insn, (AArch64reg_t)(int_spill_reg_set[0] + R0), isOutOfRange);
    Insn *ldInsn = cg->BuildInstruction<AArch64Insn>(a64cgfunc->PickLdInsn(regsize, stype), phyopnd, memopnd);
    ldInsn->SetSpillOp();
    comment = " RELOAD for caller_save " + std::to_string(vregno);
    caller_save_reload_count++;
    if (rli->last_use == insn->id) {
      if (LI_[vregno]->IsLocalRefVar() == false) {
        a64cgfunc->FreeSpillRegMem(vregno);
      }
      comment += " end";
    }
    if (use_localvar_spill) {
      comment += " LocalVarReload";
    }
    ldInsn->AddComment(comment);
    insn->bb->InsertInsnBefore(insn, ldInsn);
  }
  return;
}

// Shell function to find a physical register for an operand.
RegOperand *LSRALinearScanRegAllocator::AssignPhysRegs(Operand *opnd, Insn *insn) {
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 vregno = regOpnd->GetRegisterNumber();
  RegType regtype = regOpnd->GetRegisterType();
  CHECK_FATAL(vregno < LI_.size(), "index out of range in LSRALinearScanRegAllocator::AssignPhysRegs");
  LiveInterval *li = LI_[vregno];

  bool doNotSpill = false;
  if (li->must_allocate || (insn->mop_ == MOP_adrp_ldr && insn->next && insn->next->mop_ == MOP_clinit_tail) ||
      (insn->mop_ == MOP_clinit_tail)) {
    // Cannot spill for clinit pair
    doNotSpill = true;
  } else if (spill_all) {
    return nullptr;
  } else if (IN_SPILL_RANGE) {
    return nullptr;
  }

  if (doNotSpill) {
    li->must_allocate = true;
  }

  // if only def, no use, then should assign a new phyreg,
  // otherwise, there may be conflict
  if (li->assigned_reg != 0 && (li->last_use != 0 || li->phys_use != 0)) {
    if (AArch64Abi::IsCalleeSavedReg((AArch64reg_t)li->assigned_reg)) {
      callee_use_cnt[li->assigned_reg]++;
    }
    if (li->stk_slot == -1) {
      CG_ASSERT(opnd->GetSize() != 128, "NYI");
      return static_cast<AArch64CGFunc *>(cgfunc_)->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)(li->assigned_reg),
                                                                                       opnd->GetSize(), regtype);
    } else {
      // need to reload
      return nullptr;
    }
  }

  // pre spilled:
  if (li->stk_slot != -1) {
    return nullptr;
  }

  if (LSRA_DUMP) {
    uint32 activeSz = active.size();
    LogInfo::MapleLogger() << "\tAssignPhysRegs-active_sz " << activeSz << "\n";
  }

  uint32 regno = FindAvailablePhyReg(li, insn);
  if (regno != 0) {
    if (AArch64Abi::IsCalleeSavedReg((AArch64reg_t)regno)) {
      if (!CGOptions::doCalleeToSpill) {
        if (LSRA_DUMP) {
          LogInfo::MapleLogger() << "\tCallee-save register for save/restore in prologue/epilogue: " << regno << "\n";
        }
        static_cast<AArch64CGFunc *>(cgfunc_)->AddtoCalleeSaved((AArch64reg_t)regno);
      }
      callee_use_cnt[regno]++;
    }
    CG_ASSERT(opnd->GetSize() != 128, "NYI");
    return static_cast<AArch64CGFunc *>(cgfunc_)->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)(li->assigned_reg),
                                                                                     opnd->GetSize(), regtype);
  }

  return nullptr;
}

MemOperand *LSRALinearScanRegAllocator::GetSpillMem(uint32 vregno, uint8 isDest, Insn *insn, AArch64reg_t regno,
                                                    uint8 &isOutOfRange) {
  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
  MemOperand *memopnd = nullptr;
  if (use_localvar_spill) {
    memopnd = a64cgfunc->GetOrCreatSpillMem(vregno, LI_[vregno]->localrefvar_offset);
  } else {
    memopnd = a64cgfunc->GetOrCreatSpillMem(vregno);
  }
  return (a64cgfunc->AdjustMemOperandIfOffsetOutOfRange(memopnd, vregno, isDest, insn, regno, isOutOfRange));
}

// Set a vreg in live interval as being marked for spill.
void LSRALinearScanRegAllocator::SetOperandSpill(Operand *opnd) {
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 regno = regOpnd->GetRegisterNumber();
  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "SetOperandSpill " << regno;
    LogInfo::MapleLogger() << "(" << LI_[regno]->first_acrossed_call;
    LogInfo::MapleLogger() << ", ref_count " << LI_[regno]->ref_count << ")\n";
  }

  CG_ASSERT(regno < LI_.size(), "index out of vector size in LSRALinearScanRegAllocator::SetOperandSpill");
  LiveInterval *li = LI_[regno];
  li->stk_slot = SPILLED;
  return;
}

// See if there are available fp(simd) register so instead of a spill that
// a move to/from a simd register is generated.
bool LSRALinearScanRegAllocator::UseSimdForSpill(Insn *insn, Operand *opnd, bool isDef, uint32 spillIdx) {
  is_spill_zero = false;

  if (CGOptions::lsraSimdMode == 2) {
    return false;
  }

  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  RegType regtype = regOpnd->GetRegisterType();
  if (regtype != kRegTyInt) {
    return false;
  }

  // See if a simd has already been allocated for spill
  uint32 regno = regOpnd->GetRegisterNumber();
  LiveInterval *li = LI_[regno];

  /* If it is a catch bb or cleanup bb, then do not use simd
   * for spill, since that might incur overhead in prolog/epilog
   * for save/restore.
   */
  if (li->IsInCatch() || li->IsAllInCleanupOrFirstBB()) {
    return false;
  }

  if (li->assigned_reg && li->is_simd_spilled == false) {
    // This reg has been spilled, but by whom?
    if (li->should_save == false) {
      return false;
    } else if (li->is_caller_spilled) {
      return false;
    }
  }

  regno_t simd;
  SimdSlot slot;
  uint32 regsize;
  if (li->simd_spill_reg) {
    // Is spilled by simd
    simd = li->simd_spill_reg;
    slot = li->simd_slot;
    if (slot == kRegSlotNone) {
      regsize = 64;
    } else {
      regsize = 32;
    }
  } else {
#undef SIMD_DEBUG
#ifdef SIMD_DEBUG
    static int count = 1;
    int limit = 5;  // 4 pass  5 fail  20 total
#define SIMD_DEBUG_MACRO                                                                                          \
  {                                                                                                               \
    if (cgfunc_->GetName().find("Lsun_2Fmisc_2FFloatingDecimal_24BinaryToASCIIBuffer_3B_7Cdtoa_7C_28IJIZ_29V") != \
        string::npos) {                                                                                           \
      LogInfo::MapleLogger() << "Do " << count << " " << cgfunc_->GetName() << "\n";                                                \
      if (count > limit)                                                                                          \
        return false;                                                                                             \
      count++;                                                                                                    \
    }                                                                                                             \
  }

#else  // SIMD_DEBUG
#define SIMD_DEBUG_MACRO ;
#endif  // SIMD_DEBUG
    regsize = regOpnd->GetSize();
    if (CGOptions::lsraSimdMode == 0 || regsize == 64) {
      if (simd_spill_regs.empty()) {
        return false;
      }
      SIMD_DEBUG_MACRO;
      // Look for available 64bit simd reg to spill int reg
      MapleSet<regno_t>::iterator it = simd_spill_regs.begin();
      simd = static_cast<regno_t>(*it);
      simd_spill_regs.erase(it);
      simd_spill_0.erase(simd);
      simd_spill_1.erase(simd);
      // For 64 bit spill, do not set vector mode for operand
      slot = kRegSlotNone;
      regsize = 64;
    } else {
      bool useSlot0 = false;
      if ((insn->id - last_simd_insn_num) > 5) {
        if (simd_spill_0.size() >= simd_spill_1.size()) {
          useSlot0 = true;
        }
      } else {
        // There are some penalty associated with using the same simd
        // reg but different slot.  Try to separate them.
        if (last_simd_slot == 1) {
          useSlot0 = true;
        }
      }
      if (useSlot0) {
        // look for 32bit from slot 0
        if (simd_spill_0.empty()) {
          return false;
        }
        SIMD_DEBUG_MACRO;
        MapleSet<regno_t>::iterator it = simd_spill_0.begin();
        simd = static_cast<regno_t>(*it);
        simd_spill_0.erase(simd);
        simd_spill_regs.erase(simd);
        slot = kRegSlot0;
      } else {
        if (simd_spill_1.empty()) {
          return false;
        }
        SIMD_DEBUG_MACRO;
        // look for 32bit from slot 1
        MapleSet<regno_t>::iterator it = simd_spill_1.begin();
        simd = static_cast<regno_t>(*it);
        simd_spill_1.erase(simd);
        simd_spill_regs.erase(simd);
        slot = kRegSlot1;
      }
    }
  }

  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "InsertSpillReload using simd " << regno << " with simd reg " << simd << "\n";
  }

  static_cast<AArch64CGFunc *>(cgfunc_)->AddtoCalleeSaved((AArch64reg_t)simd);

  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
  CG *cg = a64cgfunc->cg;

  regno_t spreg;
  li->is_simd_spilled = true;
  if (li->should_save) {
    // caller save
    spreg = li->assigned_reg;
  } else {
    CG_ASSERT((spillIdx < max_int_spill), "UseSimdForSpill: ran out int spill reg");
    spreg = int_spill_reg_set[spillIdx] + R0;
    li->assigned_reg = spreg;
  }

  if (isDef) {
    MOperator opcode = insn->GetMachineOpcode();
    if (opcode == MOP_xmovri64 || opcode == MOP_xmovri32) {
      Operand *opnd1 = insn->GetOperand(1);
      AArch64ImmOperand *imm = static_cast<AArch64ImmOperand *>(opnd1);
      if (imm->IsZero()) {
        spreg = RZR;
        is_spill_zero = true;
      }
    } else if (opcode == MOP_wmovrr || opcode == MOP_xmovrr) {
      RegOperand *opnd1 = static_cast<RegOperand *>(insn->GetOperand(1));
      if (opnd1 && opnd1->IsZeroRegister()) {
        spreg = RZR;
        is_spill_zero = true;
      }
    }
    if (is_spill_zero && li->assigned_reg >= R0 && li->assigned_reg < R16) {
      /*  The zero register opt is removing the definition of the assigned reg
       *  so the caller register must be regenerated in the same bb.
       */
      int_bb_def_mask &= ~(1 << (li->assigned_reg - R0));
    }
  }

  AArch64RegOperand *fopnd = nullptr;
  RegOperand *iopnd;
  CG_ASSERT(regsize != 128, "NYI");
  iopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)spreg, regsize, kRegTyInt);
  if (slot == kRegSlotNone) {
    fopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)simd, regsize, kRegTyFloat);
  } else {
    regno_t renameSimd = simd - V0;
    // To distinguish upper/lower part and regular fp, use an alias
    if (slot == kRegSlot0) {
      renameSimd += VB32;  // add v# base, see aarch64_fp_simd_regs.def
    } else if (slot == kRegSlot1) {
      renameSimd += VB64;  // add v# base, see aarch64_fp_simd_regs.def
    }
    // Can only be single here
    fopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)renameSimd, regsize, kRegTyFloat, 0 /*flag*/,
                                                          kVecSingle, static_cast<int>(slot));
    fopnd->SetSimdVectorType(kVecSingle);
    fopnd->SetSimdVectorPosition(slot);
  }

  if (isDef) {
    is_mov_dst_simd_spilled = true;
  } else {
    is_mov_src_simd_spilled = true;
  }

  Insn *mov = nullptr;
  if (isDef) {
    li->stk_slot = SPILLED;
    simd_spill_count++;
    if (regsize <= 32) {
      mov = cg->BuildInstruction<AArch64Insn>(MOP_xvmovvr, fopnd, iopnd);
    } else {
      mov = cg->BuildInstruction<AArch64Insn>(MOP_xvmovdr, fopnd, iopnd);
    }
    std::string comment = " FMOV to simd " + std::to_string(regno);
    if (li->should_save) {
      comment += " caller";
    }
    if (li->last_use == insn->id) {
      simd_reg_reclaim[simd_reg_reclaim_idx] = simd;
      simd_reg_reclaim_slot[simd_reg_reclaim_idx] = slot;
      simd_reg_reclaim_idx++;
      comment += " end";
    }
    li->simd_spill_reg = simd;
    li->simd_slot = slot;
    mov->AddComment(comment);
    insn->bb->InsertInsnAfter(insn, mov);
    return true;
  } else {
    if (li->stk_slot == -1) {
      LogInfo::MapleLogger() << "WARNING: " << regno << " simd restore without spill in" << cgfunc_->GetName() << "\n";
    }
    simd_reload_count++;
    if (regsize <= 32) {
      mov = cg->BuildInstruction<AArch64Insn>(MOP_xvmovrv, iopnd, fopnd);
    } else {
      mov = cg->BuildInstruction<AArch64Insn>(MOP_xvmovrd, iopnd, fopnd);
    }
    std::string comment = " FMOV from simd " + std::to_string(regno);
    if (li->should_save) {
      comment += " caller";
    }
    if (li->last_use == insn->id) {
      simd_reg_reclaim[simd_reg_reclaim_idx] = simd;
      simd_reg_reclaim_slot[simd_reg_reclaim_idx] = slot;
      simd_reg_reclaim_idx++;
      comment += " end";
    }
    li->simd_spill_reg = simd;
    li->simd_slot = slot;
    mov->AddComment(comment);
    insn->bb->InsertInsnBefore(insn, mov);
    return true;
  }
}

// Generate spill/reload for an operand.
// spill_idx : one of 3 phys regs set aside for the purpose of spills.
void LSRALinearScanRegAllocator::SpillOperand(Insn *insn, Operand *opnd, bool isDef, uint32 spillIdx) {
  if (use_localvar_spill == false && UseSimdForSpill(insn, opnd, isDef, spillIdx)) {
    return;
  }

  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 regno = regOpnd->GetRegisterNumber();
  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "InsertSpillReload " << regno << "\n";
  }

  is_spill_zero = false;

  regno_t spreg;
  PrimType stype;
  CHECK_FATAL(regno < LI_.size(), "index out of range in LSRALinearScanRegAllocator::SpillOperand");
  LiveInterval *li = LI_[regno];
  CG_ASSERT(li->should_save == false, "SpillOperand: Should not be caller");
  uint32 regsize = regOpnd->GetSize();
  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
  CG *cg = a64cgfunc->cg;
  RegType regtype = regOpnd->GetRegisterType();

  if (isDef) {
    MOperator opcode = insn->GetMachineOpcode();
    if (opcode == MOP_xmovri64 || opcode == MOP_xmovri32) {
      Operand *opnd1 = insn->GetOperand(1);
      AArch64ImmOperand *imm = static_cast<AArch64ImmOperand *>(opnd1);
      if (imm->IsZero()) {
        is_spill_zero = true;
      }
    } else if (opcode == MOP_wmovrr || opcode == MOP_xmovrr) {
      RegOperand *opnd1 = static_cast<RegOperand *>(insn->GetOperand(1));
      if (opnd1 && opnd1->IsZeroRegister()) {
        is_spill_zero = true;
      }
    }
  }

  if (li->regtype == kRegTyInt) {
    CG_ASSERT((spillIdx < max_int_spill), "SpillOperand: ran out int spill reg");
    spreg = int_spill_reg_set[spillIdx] + R0;
    stype = (regsize <= 32) ? PTY_i32 : PTY_i64;
  } else {
    CG_ASSERT((li->regtype == kRegTyFloat), "SpillOperand: Should be float type");
    CG_ASSERT((spillIdx < MAX_FP_SPILL), "SpillOperand: ran out fp spill reg");
    spreg = fp_spill_reg_set[spillIdx] + V0;
    stype = (regsize <= 32) ? PTY_f32 : PTY_f64;
  }

  uint8 isOutOfRange = 0;
  RegOperand *phyopnd = nullptr;
  if (is_spill_zero) {
    phyopnd = AArch64RegOperand::GetZeroRegister(regsize);
  } else {
    CG_ASSERT(regsize != 128, "NYI");
    phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)spreg, regsize, regtype);
  }
  li->assigned_reg = phyopnd->GetRegisterNumber();

  MemOperand *memopnd = nullptr;
  if (isDef) {
    // Need to assign spreg (one of the two spill reg) to the destination of the insn.
    //    spill_vreg <- opn1 op opn2
    // to
    //    spreg <- opn1 op opn2
    //    store spreg -> spillmem
    li->stk_slot = SPILLED;

    spill_count++;
    memopnd = GetSpillMem(regno, 1, insn, (AArch64reg_t)(int_spill_reg_set[spillIdx + 1] + R0), isOutOfRange);
    Insn *stInsn = cg->BuildInstruction<AArch64Insn>(a64cgfunc->PickStInsn(regsize, stype), phyopnd, memopnd);
    stInsn->SetSpillOp();
    std::string comment = " SPILL vreg:" + std::to_string(regno);
    if (li->last_use == insn->id) {
      if (LI_[regno]->IsLocalRefVar() == false) {
        a64cgfunc->FreeSpillRegMem(regno);
      }
      comment += " end";
    }
    if (use_localvar_spill) {
      comment += " LocalVarSpill";
    }
    stInsn->AddComment(comment);
    if (isOutOfRange) {
      insn->bb->InsertInsnAfter(insn->next, stInsn);
    } else {
      insn->bb->InsertInsnAfter(insn, stInsn);
    }
    return;
  } else {
    // Here, reverse of isDef, change either opn1 or opn2 to the spreg.
    if (li->stk_slot == -1) {
      LogInfo::MapleLogger() << "WARNING: " << regno << " assigned " << li->assigned_reg << " restore without spill in bb "
           << insn->bb->id << " : " << cgfunc_->GetName() << "\n";
    }
    reload_count++;
    memopnd = GetSpillMem(regno, 0, insn, (AArch64reg_t)(int_spill_reg_set[spillIdx] + R0), isOutOfRange);
    Insn *ldInsn = cg->BuildInstruction<AArch64Insn>(a64cgfunc->PickLdInsn(regsize, stype), phyopnd, memopnd);
    ldInsn->SetSpillOp();
    std::string comment = " RELOAD vreg" + std::to_string(regno);
    if (li->last_use == insn->id) {
      if (LI_[regno]->IsLocalRefVar() == false) {
        a64cgfunc->FreeSpillRegMem(regno);
      }
      comment += " end";
    }
    if (use_localvar_spill) {
      comment += " LocalVarReload";
    }
    ldInsn->AddComment(comment);
    insn->bb->InsertInsnBefore(insn, ldInsn);
    return;
  }
}

RegOperand *LSRALinearScanRegAllocator::HandleSpillForInsn(Insn *insn, Operand *opnd) {
  // choose the lowest priority li to spill
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 regno = regOpnd->GetRegisterNumber();
  CG_ASSERT(regno < LI_.size(), "index out of range of MapleVector in LSRALinearScanRegAllocator::HandleSpillForInsn");
  LiveInterval *li = LI_[regno];
  RegType regtype = regOpnd->GetRegisterType();
  LiveInterval *spillLi = nullptr;
  FindLowestPrioInActive(&spillLi, regtype, true);

  // compare spill_li with current li
  // spill_li is null and li->stk_slot == Spilled when the li is spilled due to LiveIntervalAnalysis
  if (!spillLi || spillLi->li_parent || spillLi->li_child || li->stk_slot == SPILLED || li->first_def != insn->id ||
      li->priority < spillLi->priority || li->ref_count < spillLi->ref_count ||
      !(AArch64Abi::IsCalleeSavedReg((AArch64reg_t)(spillLi->assigned_reg)))) {
    // spill current li
    if (LSRA_DUMP) {
      LogInfo::MapleLogger() << "Flexible Spill: still spill " << li->regno << ".\n";
    }
    SetOperandSpill(opnd);
    return nullptr;
  }

  ReturnPregToSet(spillLi, spillLi->assigned_reg);
  RegOperand *newOpnd = AssignPhysRegs(opnd, insn);
  if (!newOpnd) {
    ReleasePregToset(spillLi, spillLi->assigned_reg);
    SetOperandSpill(opnd);
    return nullptr;
  }

  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "Flexible Spill: " << spillLi->regno << " instead of " << li->regno << ".\n";
    PrintLiveInterval(spillLi, "TO spill: ");
    PrintLiveInterval(li, "Instead of: ");
  }

  // spill this live interval
  active.erase(it_finded);
  spillLi->stk_slot = SPILLED;

  return newOpnd;
}

bool LSRALinearScanRegAllocator::OpndNeedAllocation(Insn *insn, Operand *opnd, bool isdef, uint32 insnNum) {
  if (!opnd->IsRegister()) {
    return false;
  }
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  RegType regtype = regOpnd->GetRegisterType();
  uint32 regno = regOpnd->GetRegisterNumber();
  if (regtype == kRegTyCc || regtype == kRegTyVary) {
    return false;
  }
  if (IsUntouchableReg(regno) || regOpnd->IsConstReg()) {
    return false;
  }
  if (regOpnd->IsPhysicalRegister()) {
    if ((insn->mop_ == MOP_adrp_ldr && insn->next && insn->next->mop_ == MOP_clinit_tail) ||
        (insn->mop_ == MOP_clinit_tail)) {
      // Cannot spill for clinit pair
    } else if (spill_all) {
      return false;
    }
    if (isdef) {
      if (regtype == kRegTyInt) {
        if (regno > R7) {
          return false;
        }
        if (int_param_queue[regno - R0].empty() == true) {
          return false;
        }
        LiveInterval *li = int_param_queue[regno - R0].front();
        // li may have been inserted by InsertParamToActive
        if (li->first_def == insnNum) {
          int_param_reg_set.erase(regno - R0);
          active.insert(li);
          CG_ASSERT((regno - R0) < int_param_queue.size(),
                    "index out of range in LSRALinearScanRegAllocator::OpndNeedAllocation");
          int_param_queue[regno - R0].pop_front();
        }
      } else {
        if (regno > V7) {
          return false;
        }
        if (fp_param_queue[regno - V0].empty() == true) {
          return false;
        }
        LiveInterval *li = fp_param_queue[regno - V0].front();
        // li may have been inserted by InsertParamToActive
        if (li->first_def == insnNum) {
          fp_param_reg_set.erase(regno - V0);
          active.insert(li);
          fp_param_queue[regno - V0].pop_front();
        }
      }
    }
    return false;
  }
  // This is a virtual register
  return true;
}

void LSRALinearScanRegAllocator::InsertParamToActive(Operand *opnd, uint32 insnNum) {
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 regno = regOpnd->GetRegisterNumber();
  CHECK_FATAL(regno < LI_.size(), "index out of range in LSRALinearScanRegAllocator::InsertParamToActive");
  LiveInterval *li = LI_[regno];
  // Search for parameter registers that is in the live range to insert into queue
  if (li->regtype == kRegTyInt) {
    for (uint32 i = 0; i < int_param_queue.size(); i++) {
      if (int_param_queue[i].empty()) {
        continue;
      }
      LiveInterval *pli = int_param_queue[i].front();
      do {
        if ((pli->first_def <= li->first_def) && (pli->phys_use <= li->first_def)) {
          // just discard it
          int_param_queue[i].pop_front();
          if (int_param_queue[i].empty()) {
            break;
          }
          pli = int_param_queue[i].front();
        } else {
          break;
        }
      } while (true);
      if ((pli->first_def < li->last_use) && (pli->phys_use > li->first_def)) {
        if (int_param_reg_set.find(i) != int_param_reg_set.end()) {
          // reserve this param register and active the its first use
          last_int_param_li[i] = pli;
          int_param_reg_set.erase(i);
          int_param_queue[i].pop_front();
        }
      }
    }
  } else {
    CG_ASSERT((li->regtype == kRegTyFloat), "InsertParamToActive: Incorrect register type");
    for (uint32 i = 0; i < fp_param_queue.size(); i++) {
      if (fp_param_queue[i].empty()) {
        continue;
      }
      LiveInterval *pli = fp_param_queue[i].front();
      do {
        if ((pli->first_def <= li->first_def) && (pli->phys_use <= li->first_def)) {
          // just discard it
          fp_param_queue[i].pop_front();
          if (fp_param_queue[i].empty()) {
            break;
          }
          pli = fp_param_queue[i].front();
        } else {
          break;
        }
      } while (true);
      if ((pli->first_def < li->last_use) && (pli->phys_use > li->first_def)) {
        if (fp_param_reg_set.find(i) != fp_param_reg_set.end()) {
          last_fp_param_li[i] = pli;
          fp_param_reg_set.erase(i);
          fp_param_queue[i].pop_front();
        }
      }
    }
  }
}

// Insert a live interval into the 'active' list.
void LSRALinearScanRegAllocator::InsertToActive(Operand *opnd, uint32 insnNum) {
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 regno = regOpnd->GetRegisterNumber();
  CHECK_FATAL(regno < LI_.size(), "index out of range in LSRALinearScanRegAllocator::InsertToActive");
  LiveInterval *li = LI_[regno];
  if (li->last_use <= insnNum) {
    // insert first, and retire later, then the assigned reg can be released
    active.insert(li);
    if (LSRA_DUMP) {
      PrintLiveInterval(li, "LiveInterval is skip due to past insn num --- opt to remove redunant insn");
    }
    return;
  }
  active.insert(li);
}

// find the lowest one and erase it from active
void LSRALinearScanRegAllocator::FindLowestPrioInActive(LiveInterval **targetLi, RegType regtype, bool startRa) {
  float lowestPrio = 100.0;
  bool found = false;
  MapleSet<LiveInterval *, ActiveCmp>::iterator it;
  MapleSet<LiveInterval *, ActiveCmp>::iterator lowestIt;
  for (it = active.begin(); it != active.end(); it++) {
    LiveInterval *li = static_cast<LiveInterval *>(*it);
    if (startRa && li->phys_use != 0) {
      continue;
    }
    if (li->priority < lowestPrio && li->regtype == regtype) {
      lowestPrio = li->priority;
      lowestIt = it;
      found = true;
    }
  }
  if (found) {
    *targetLi = *lowestIt;
    it_finded = lowestIt;
  }
  return;
}

// Calculate the weight of a live interval for pre-spill and flexible spill
void LSRALinearScanRegAllocator::LiveIntervalAnalysis() {
  for (uint32_t bbIdx = 0; bbIdx < sorted_bbs.size(); bbIdx++) {
    BB *bb = sorted_bbs[bbIdx];

    // 1. calculate live interfere
    FOR_BB_INSNS(insn, bb) {
      if (insn->IsImmaterialInsn()) {
        continue;
      }
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      if (insn->id == 0) {
        // New instruction inserted by reg alloc (ie spill)
        continue;
      }

      // simple retire from active
      MapleSet<LiveInterval *, ActiveCmp>::iterator it;
      for (it = active.begin(); it != active.end(); /* erase will update */) {
        LiveInterval *li = static_cast<LiveInterval *>(*it);
        if (li->last_use > insn->id) {
          break;
        }
        it = active.erase(it);
      }

      // simple insert to active
      const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
      for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
        Operand *opnd = insn->GetOperand(i);
        if (!opnd) {
          continue;
        }

        bool isdef = static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegDef();
        if (isdef) {
          RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
          if (regOpnd->IsVirtualRegister() && regOpnd->GetRegisterType() != kRegTyCc) {
            uint32 regno = regOpnd->GetRegisterNumber();
            LiveInterval *li = LI_[regno];
            if (li->first_def == insn->id) {
              active.insert(li);
            }
          }
        }
      }

      // get interfere info
      uint32 interNum = active.size();
      if (LSRA_DUMP) {
        LogInfo::MapleLogger() << "In insn " << insn->id << ", " << interNum << " overlap live intervals.\n";
      }

      // 2. analysis which to spill
      while (interNum > CGOptions::overlapNum) {
        LiveInterval *lowestLi = nullptr;
        FindLowestPrioInActive(&lowestLi);
        if (lowestLi) {
          if (LSRA_DUMP) {
            PrintLiveInterval(lowestLi, "Pre spilled: ");
          }
          lowestLi->stk_slot = SPILLED;
          active.erase(it_finded);
          interNum = active.size();
        } else {
          break;
        }
      }
    }
  }

  active.clear();
}

// Iterate through the operands of an instruction for allocation.
void LSRALinearScanRegAllocator::AssignPhysRegsForInsn(Insn *insn) {
  const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];

  // At the beginning of the landing pad, we handle the x1, x2 as if they are implicitly defined.
  if (insn->bb->eh_preds.size() != 0 && insn == insn->bb->firstinsn) {
    if (!int_param_queue[0].empty()) {
      LiveInterval *li = int_param_queue[0].front();
      if (li->first_def == insn->id) {
        int_param_reg_set.erase(li->assigned_reg - R0);
        active.insert(li);
        int_param_queue[0].pop_front();
      }
    }

    if (!int_param_queue[1].empty()) {
      LiveInterval *li = int_param_queue[1].front();
      if (li->first_def == insn->id) {
        int_param_reg_set.erase(li->assigned_reg - R0);
        active.insert(li);
        int_param_queue[1].pop_front();
      }
    }
  }

  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "active in " << insn->id << " :";
    PrintActiveListSimple();
  }
  for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
    Operand *opnd = insn->GetOperand(i);
    if (!opnd) {
      continue;
    }
    bool isdef = static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegDef();
    RegOperand *newOpnd = nullptr;
    if (opnd->IsList()) {
      // For arm32, not arm64
    } else if (opnd->IsMemoryAccessOperand()) {
      MemOperand *memopnd = static_cast<MemOperand *>(opnd);
      Operand *base = memopnd->GetBaseRegister();
      Operand *offset = memopnd->GetIndexRegister();
      isdef = false;
      if (base != nullptr) {
        if (OpndNeedAllocation(insn, base, isdef, insn->id)) {
          newOpnd = AssignPhysRegs(base, insn);
          if (!newOpnd) {
            SetOperandSpill(base);
          }
          // add CG_ASSERT here.
          // memopnd->SetBaseRegister(NewOpnd);
        }
      }
      if (offset != nullptr) {
        if (OpndNeedAllocation(insn, offset, isdef, insn->id) == false) {
          continue;
        }
        newOpnd = AssignPhysRegs(offset, insn);
        if (!newOpnd) {
          SetOperandSpill(offset);
        }
        // memopnd->SetIndexRegister(NewOpnd);
      }
    } else {
      if (OpndNeedAllocation(insn, opnd, isdef, insn->id) == false) {
        continue;
      }
      if (isdef && (fast_alloc == false)) {
        InsertParamToActive(opnd, insn->id);
      }
      newOpnd = AssignPhysRegs(opnd, insn);
      if (isdef != true && 0 /* check if active size change?*/) {
        // Might want to warn here if use reg is not assigned a physical.
        continue;
      }

      if (newOpnd) {
        if (isdef) {
          InsertToActive(opnd, insn->id);
        }
      } else {
        // If dest and both src are spilled, src will use both of the
        // spill registers.
        // dest can use any spill reg, choose 0
        if (isdef) {
          newOpnd = HandleSpillForInsn(insn, opnd);
          if (newOpnd) {
            InsertToActive(opnd, insn->id);
          }
        } else {
          SetOperandSpill(opnd);
        }
      }
    }
  }
}

// Pattern matcher for a MCC_InitializeLocalStackRef to get the call parameter.
uint32 LSRALinearScanRegAllocator::FindLocalRefVarStackLoc(Insn *insn, uint32 param) {
  // The caller already knows this insn is a call to "MCC_InitializeLocalStackRef".
  // Assume parameters are in the same bb.
  for (Insn *prev = insn->prev; prev; prev = prev->prev) {
    Operand *dst = prev->GetOperand(0);
    RegOperand *regopnd = static_cast<RegOperand *>(dst);
    uint32 dstRegno = regopnd->GetRegisterNumber();
    if (dstRegno == param) {
      Operand *src = nullptr;
      if (prev->GetOpndNum() == 1) {
        // move insn
        src = prev->GetOpnd(0);
      } else {
        // add insn
        src = prev->GetOpnd(1);
      }
      CG_ASSERT(src != nullptr, "src is null in LSRALinearScanRegAllocator::FindLocalRefVarStackLoc");
      if (src->IsRegister()) {
        RegOperand *regsrc = static_cast<RegOperand *>(src);
        return FindLocalRefVarStackLoc(prev, regsrc->GetRegisterNumber());
      } else {
        CG_ASSERT(src->IsIntImmediate(), "FindLocalRefVarStackLoc not imm const");
        ImmOperand *imm = static_cast<ImmOperand *>(src);
        return static_cast<uint32>(imm->GetValue());
      }
    }
  }
  return 0;
}

bool LSRALinearScanRegAllocator::DetectLocalRefVarInfo(Insn *insn, uint32 imm) {
  if (imm < localrefvar_min_stack_loc || imm > localrefvar_max_stack_loc) {
    return false;
  }
  if (!insn->IsStore()) {
    return false;
  }

  return true;
}

/* To match a vreg to a localref var has several patterns.
 * There might be more, but currently these are supported.
 * case 1
 *  mov Ry <- Rx    // Match Ry
 *  str Rx -> []    // local ref var
 *
 * case 2
 *  mov Ry <- 0     // Match Ry
 *  str R32 -> []
 *
 * case 3
 *  Rx <- R0        // Match Rx
 *  str Rx -> []
 *
 *  In each case, the matched reg is a candidate to be associated
 *  with the local ref var.
 */
regno_t LSRALinearScanRegAllocator::FindLocalRefVarReg(Insn *insn) {
  CG_ASSERT((insn->IsStore() && (!insn->IsStorePair()) && (!insn->IsAtomic())),
            "FindLocalRefVarReg: insn has to be a store");
  Insn *pinsn = insn->prev;
  if (!pinsn) {
    return 0;
  }
  // Check for matching size
  Operand *iopnd = insn->GetOperand(0);
  uint8 regsize = iopnd->GetSize();
  MOperator opcode = pinsn->GetMachineOpcode();
  if (regsize <= 32) {
    if (opcode != MOP_wmovrr && opcode != MOP_xmovri32) {
      return 0;
    }
  } else {
    if (opcode != MOP_xmovrr && opcode != MOP_xmovri64) {
      return 0;
    }
  }

  RegOperand *regOpnd = static_cast<RegOperand *>(iopnd);
  Operand *psrc = pinsn->GetOperand(1);
  if (opcode == MOP_xmovri32 || opcode == MOP_xmovri64) {
    ImmOperand *imm = static_cast<ImmOperand *>(psrc);
    if (imm == nullptr) {
      return 0;
    }
    uint32 val = imm->GetValue();
    if (regOpnd->IsZeroRegister() && val == 0) {
      Operand *dst = pinsn->GetOperand(0);
      RegOperand *rdst = static_cast<RegOperand *>(dst);
      // case 2
      if (!rdst->IsPhysicalRegister()) {
        return rdst->GetRegisterNumber();
      }
    }
  } else {
    // wmovrr or xmovrr
    RegOperand *psrcOpnd = static_cast<RegOperand *>(psrc);
    if (psrcOpnd->GetRegisterNumber() == R0) {
      if (psrcOpnd->GetRegisterNumber() != regOpnd->GetRegisterNumber()) {
        return 0;
      }
      // case 3
      RegOperand *pdstOpnd = static_cast<RegOperand *>(pinsn->GetOperand(0));
      if (!pdstOpnd->IsPhysicalRegister()) {
        return pdstOpnd->GetRegisterNumber();
      }
    } else if (psrcOpnd->GetRegisterNumber() == regOpnd->GetRegisterNumber()) {
      // case 1
      RegOperand *pdst = static_cast<RegOperand *>(pinsn->GetOperand(0));
      if (!pdst->IsPhysicalRegister()) {
        return pdst->GetRegisterNumber();
      }
    }
  }
  return 0;
}

// If a memory instruction is a localrefvar, mark it so it can be used
// for a spill/reload of operands accessing the same stack location.
void LSRALinearScanRegAllocator::MarkLocalRefVarForLiveInterval(Insn *insn, uint32 offset) {
  if (!insn->IsStore()) {
    return;
  }
  if (insn->IsStorePair()) {
    CG_ASSERT(insn->mop_ == MOP_xstp, "InvalidateLocalRefVarOffset: wrong store pair insn");
    // localrefvar should be a normal store, not the release version.
    RegOperand *regOpnd;
    regno_t regno;
    // Cannot match store pair with a single spill.  mark these registers
    // invalid for spill use.
    regOpnd = static_cast<RegOperand *>(insn->GetOperand(0));
    uint32 idx = (offset - localrefvar_min_stack_loc) >> 3;
    if (!regOpnd->IsPhysicalRegister()) {
      regno = regOpnd->GetRegisterNumber();
      LI_[regno]->SetLocalRefVarStateInvalid();
      if (lvar_offset_regno_map[idx]) {
        regno = lvar_offset_regno_map[idx];
        LI_[regno]->SetLocalRefVarStateInvalid();
      } else {
        lvar_offset_regno_map[idx] = regno;
      }
    }
    regOpnd = static_cast<RegOperand *>(insn->GetOperand(1));
    if (!regOpnd->IsPhysicalRegister()) {
      regno = regOpnd->GetRegisterNumber();
      LI_[regno]->SetLocalRefVarStateInvalid();
      if (lvar_offset_regno_map[idx + 1]) {
        regno = lvar_offset_regno_map[idx + 1];
        LI_[regno]->SetLocalRefVarStateInvalid();
      } else {
        lvar_offset_regno_map[idx + 1] = regno;
      }
    }
    return;
  }
  CG_ASSERT(insn->mop_ == MOP_xstr, "InvalidateLocalRefVarOffset: wrong store insn");

  RegOperand *regOpnd = static_cast<RegOperand *>(insn->GetOperand(0));
  regno_t regno = FindLocalRefVarReg(insn);
  if (regno == 0) {
    if (!regOpnd->IsPhysicalRegister()) {
      regno = regOpnd->GetRegisterNumber();
      LI_[regno]->SetLocalRefVarStateInvalid();
      uint32 idx = (offset - localrefvar_min_stack_loc) >> 3;
      if (lvar_offset_regno_map[idx]) {
        regno = lvar_offset_regno_map[idx];
        LI_[regno]->SetLocalRefVarStateInvalid();
      } else {
        lvar_offset_regno_map[idx] = regno;
      }
    }
    return;
  }

  LiveInterval *li = LI_[regno];
  li->localrefvar_count++;
  if (li->localrefvar_offset == 0) {
    uint32 idx = (offset - localrefvar_min_stack_loc) >> 3;
    if (lvar_offset_regno_map[idx]) {
      // Another register mapped to this location, invalidate both registers
      li->SetLocalRefVarStateInvalid();
      LI_[lvar_offset_regno_map[idx]]->SetLocalRefVarStateInvalid();
      return;
    }

    lvar_offset_regno_map[idx] = regno;
    li->SetLocalRefVarStateValid();
    li->localrefvar_offset = offset;
    if (LSRA_DUMP) {
      LogInfo::MapleLogger() << "\t\tLocalVarRef set vreg " << regno << " imm " << offset << "\n";
    }
  } else if (li->localrefvar_offset != offset) {
    li->SetLocalRefVarStateInvalid();
    uint32 idx = (offset - localrefvar_min_stack_loc) >> 3;
    if (lvar_offset_regno_map[idx]) {
      LI_[lvar_offset_regno_map[idx]]->SetLocalRefVarStateInvalid();
    } else {
      lvar_offset_regno_map[idx] = regno;
    }
    idx = (li->localrefvar_offset - localrefvar_min_stack_loc) >> 3;
    if (lvar_offset_regno_map[idx]) {
      LI_[lvar_offset_regno_map[idx]]->SetLocalRefVarStateInvalid();
    } else {
      lvar_offset_regno_map[idx] = regno;
    }
    if (LSRA_DUMP) {
      LogInfo::MapleLogger() << "\t\tLocalVarRef unset vreg " << regno << " imm " << offset << "\n";
    }
  }
}

// Create an operand with physical register assigned, or a spill register
// in the case where a physical register cannot be assigned.
RegOperand *LSRALinearScanRegAllocator::GetReplaceOpnd(Insn *insn, Operand *opnd, uint32 &spillIdx, bool isdef) {
  if (!opnd->IsRegister()) {
    return nullptr;
  }
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);

  uint32 vregno = regOpnd->GetRegisterNumber();
  RegType regtype = regOpnd->GetRegisterType();
  if (regtype == kRegTyCc || regtype == kRegTyVary) {
    return nullptr;
  }
  if (IsUntouchableReg(vregno) || regOpnd->IsConstReg()) {
    return nullptr;
  }
  if (regOpnd->IsPhysicalRegister()) {
    return nullptr;
  }

  CG_ASSERT(vregno < LI_.size(), "index out of range of MapleVector in LSRALinearScanRegAllocator::GetReplaceOpnd");
  LiveInterval *li = LI_[vregno];

  bool addCalleeToSaved = true;
  regno_t regno = li->assigned_reg;
  bool isCalleeReg = AArch64Abi::IsCalleeSavedReg((AArch64reg_t)regno);
  if (CGOptions::doCalleeToSpill &&
      // prolog can use stp, so try to estimate if spill callee should be done.
      ((should_opt_int_callee && li->regtype == kRegTyInt) || (should_opt_fp_callee && li->regtype == kRegTyFloat))) {
    if (isCalleeReg) {
      // Determine if it is worth keeping callee
      if (callee_use_cnt[regno] == 2 && li->result_count == 1 && li->ref_count == 2) {
        // This callee is allocated for one def and one use
        li->stk_slot = SPILLED;
        li->assigned_reg = 0;
        addCalleeToSaved = false;
      }
    }
  }
  if (isCalleeReg && addCalleeToSaved) {
    static_cast<AArch64CGFunc *>(cgfunc_)->AddtoCalleeSaved((AArch64reg_t)regno);
  }

  bool noSpill = false;
  bool spillInserted = false;
  use_localvar_spill = false;
  if (li->should_save) {
    // Determine if spill can reside in localref space
    if (li->IsLocalRefVar() && CGOptions::doLocalRefSpill) {
      // Make sure vreg writes and localref store match.
      if (li->result_count == li->localrefvar_count) {
        use_localvar_spill = true;
        spillInserted = true;
        if (LSRA_DUMP) {
          LogInfo::MapleLogger() << "\t\tcaller_use_localrefvar_spill " << li->regno << " imm " << li->localrefvar_offset << " ";
          opnd->dump();
          LogInfo::MapleLogger() << "\n";
        }
      }
    }
    if (insn->mop_ == MOP_xadrp && insn->next && insn->next->mop_ == MOP_xadrpl12 && li->assigned_reg != 0) {
      RegOperand *ndst = static_cast<RegOperand *>(insn->next->GetOperand(0));
      RegOperand *nsrc = static_cast<RegOperand *>(insn->next->GetOperand(1));
      RegOperand *curdst = static_cast<RegOperand *>(insn->GetOperand(0));
      if (curdst->GetRegisterNumber() == ndst->GetRegisterNumber() &&
          curdst->GetRegisterNumber() == nsrc->GetRegisterNumber()) {
        noSpill = true;
      }
    }
    if (insn->mop_ == MOP_adrp_ldr && insn->next && insn->next->mop_ == MOP_clinit_tail) {
      // clinit pair
      li->assigned_reg = R16;
    } else if (insn->mop_ == MOP_clinit_tail && insn->prev && insn->prev->mop_ == MOP_adrp_ldr) {
      isdef = true;
      InsertCallerSave(insn, opnd, isdef);
      spillInserted = true;
    } else {
      InsertCallerSave(insn, opnd, isdef);
      spillInserted = true;
    }
  } else if (li->stk_slot == SPILLED) {
    // Determine if spill can reside in localref space
    if (li->IsLocalRefVar() && CGOptions::doLocalRefSpill) {
      // Make sure vreg writes and localref store match.
      if (li->result_count == li->localrefvar_count) {
        use_localvar_spill = true;
        if (LSRA_DUMP) {
          LogInfo::MapleLogger() << "\t\tcallee_use_localrefvar_spill " << li->regno << " imm " << li->localrefvar_offset << " ";
          opnd->dump();
          LogInfo::MapleLogger() << "\n";
        }
      }
    }
    bool noSpill = false;
    if (insn->mop_ == MOP_xadrp && insn->next && insn->next->mop_ == MOP_xadrpl12 && li->assigned_reg != 0) {
      RegOperand *ndst = static_cast<RegOperand *>(insn->next->GetOperand(0));
      RegOperand *nsrc = static_cast<RegOperand *>(insn->next->GetOperand(1));
      RegOperand *curdst = static_cast<RegOperand *>(insn->GetOperand(0));
      if (curdst->GetRegisterNumber() == ndst->GetRegisterNumber() &&
          curdst->GetRegisterNumber() == nsrc->GetRegisterNumber()) {
        noSpill = true;
      }
    }
    if ((insn->mop_ == MOP_adrp_ldr && insn->next && insn->next->mop_ == MOP_clinit_tail)) {
      // clinit pair
      li->assigned_reg = R16;
    } else if (insn->mop_ == MOP_clinit_tail && insn->prev && insn->prev->mop_ == MOP_adrp_ldr) {
      isdef = true;
      spillIdx = 0;
      SpillOperand(insn, opnd, isdef, spillIdx);
      spillInserted = true;
    } else {
      if (isdef) {
        spillIdx = 0;
      }
      SpillOperand(insn, opnd, isdef, spillIdx);
      spillInserted = true;
      if (!isdef) {
        spillIdx++;
      }
    }
  }

  if (spillInserted && noSpill) {
    insn->bb->RemoveInsn(insn->next);
  }

  CG_ASSERT(opnd->GetSize() != 128, "NYI");
  RegOperand *phyOpnd = static_cast<AArch64CGFunc *>(cgfunc_)->GetOrCreatePhysicalRegisterOperand(
    (AArch64reg_t)(li->assigned_reg), opnd->GetSize(), regtype);

  return phyOpnd;
}

// Iterate through all instructions and change the vreg to preg.
void LSRALinearScanRegAllocator::FinalizeRegisters() {
  // Try to estimate if spill callee should be done based on even/odd for stp in prolog.
  if (CGOptions::doCalleeToSpill) {
    uint32 pairCnt = 0;
    uint32 begin;
    uint32 end;
    GetAvailablePRregs(kRegTyInt, begin, end, false);
    for (uint32 idx = begin; idx < end; idx++) {
      if (callee_use_cnt[idx] != 0) {
        pairCnt++;
      }
    }
    if (pairCnt & 0x01) {
      should_opt_int_callee = true;
    }

    GetAvailablePRregs(kRegTyFloat, begin, end, false);
    for (uint32 idx = begin; idx < end; idx++) {
      if (callee_use_cnt[idx] != 0) {
        pairCnt++;
      }
    }
    if (pairCnt & 0x01) {
      should_opt_fp_callee = true;
    }
  }
  for (uint32_t bbIdx = 0; bbIdx < sorted_bbs.size(); bbIdx++) {
    BB *bb = sorted_bbs[bbIdx];
    int_bb_def_mask = 0;
    fp_bb_def_mask = 0;

    FOR_BB_INSNS(insn, bb) {
      if (insn->IsImmaterialInsn()) {
        continue;
      }
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      if (insn->id == 0) {
        continue;
      }

      bool isMove = false;
      bool isMove32 = false;
      MOperator opcode = insn->GetMachineOpcode();
      if (opcode == MOP_wmovrr || opcode == MOP_xmovrr) {
        isMove = true;
        isMove32 = (opcode == MOP_wmovrr) ? true : false;
        is_mov_src_simd_spilled = false;
        is_mov_dst_simd_spilled = false;
      }

      uint32 spillIdx = 0;
      const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];

      // Handle source opernads first
      simd_reg_reclaim_idx = 0;
      for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
        Operand *opnd = insn->GetOperand(i);
        if (!opnd) {
          continue;
        }

        CG_ASSERT((md->operand_[i]) != nullptr, "pointer is null in LSRALinearScanRegAllocator::FinalizeRegisters");
        bool isdef = static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegDef();
        if (isdef == true) {
          continue;
        }
        RegOperand *phyOpnd = nullptr;
        if (opnd->IsList()) {
          // For arm32, not arm64
        } else if (opnd->IsMemoryAccessOperand()) {
          MemOperand *memopnd = static_cast<MemOperand *>(static_cast<MemOperand *>(opnd)->Clone(cgfunc_->memPool));
          CG_ASSERT(memopnd != nullptr, "memopnd is null in LSRALinearScanRegAllocator::FinalizeRegisters");
          insn->SetOperand(i, memopnd);
          Operand *base = memopnd->GetBaseRegister();
          Operand *offset = memopnd->GetIndexRegister();
          if (base != nullptr) {
            phyOpnd = GetReplaceOpnd(insn, base, spillIdx, false);
            if (phyOpnd) {
              memopnd->SetBaseRegister(phyOpnd);
            }
          }
          if (offset != nullptr) {
            phyOpnd = GetReplaceOpnd(insn, offset, spillIdx, false);
            if (phyOpnd) {
              memopnd->SetIndexRegister(phyOpnd);
            }
          }
        } else {
          phyOpnd = GetReplaceOpnd(insn, opnd, spillIdx, false);
          if (phyOpnd) {
            insn->SetOperand(i, phyOpnd);
          }
        }
      }
      // Handle dest opernads last
      for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
        Operand *opnd = insn->GetOperand(i);
        if (!opnd) {
          continue;
        }

        bool isdef = static_cast<AArch64OpndProp *>(md->operand_[i])->IsRegDef();
        if (isdef == false) {
          continue;
        }
        is_spill_zero = false;
        RegOperand *phyOpnd = nullptr;
        phyOpnd = GetReplaceOpnd(insn, opnd, spillIdx, true);
        if (phyOpnd) {
          insn->SetOperand(i, phyOpnd);
          if (is_spill_zero) {
            insn->bb->RemoveInsn(insn);
          }
        }
      }

      if (isMove && is_mov_src_simd_spilled && is_mov_dst_simd_spilled) {
        /* mov reg1, reg2 -> both src & dest spill
         *
         *    fmov reg2 <- fregx        fmov reg2 <- fregx
         *    mov  reg1 <- reg2   ===>  fmov fregy <- fregx
         *    fmov fregy <- reg1
         *
         * Cannot remove the first fmov due to caller save local use.
         */
        CG_ASSERT(insn->prev, "LSRA spill has no prev insn");
        CG_ASSERT(insn->next, "LSRA spill has no next insn");

        // Since reg1 is not defined, need to reload it for caller if
        // it is necessary in the same bb.
        RegOperand *dstRegopnd = static_cast<RegOperand *>(insn->GetOperand(0));
        RegOperand *srcRegopnd = static_cast<RegOperand *>(insn->GetOperand(1));
        if (dstRegopnd->GetSize() == srcRegopnd->GetSize()) {
          regno_t regno = dstRegopnd->GetRegisterNumber();
          if (cgfunc_->IsCallerSaved(regno)) {
            int_bb_def_mask &= ~(1 << (regno - R0));
          }

          Insn *ninsn = insn->next;
          Operand *dst = ninsn->GetOperand(0);
          Insn *pinsn = insn->prev;
          Operand *src = pinsn->GetOperand(1);

          insn->SetOperand(0, dst);
          insn->SetOperand(1, src);
          if (isMove32) {
            insn->SetMOP(MOP_xvmovvv);
          } else {
            insn->SetMOP(MOP_xvmovd);
          }
          ninsn->bb->RemoveInsn(ninsn);
        }
      }

      for (int i = 0; i < simd_reg_reclaim_idx; i++) {
        regno_t simd = simd_reg_reclaim[i];
        if (simd_reg_reclaim_slot[i] == kRegSlot0) {
          simd_spill_0.insert(simd);
          // if both [0,1] are available, then 64bit total is avail
          for (MapleSet<regno_t>::iterator it = simd_spill_1.begin(); it != simd_spill_1.end(); it++) {
            regno_t sreg = static_cast<regno_t>(*it);
            if (sreg == simd) {
              simd_spill_regs.insert(simd);
              break;
            }
          }
        } else if (simd_reg_reclaim_slot[i] == kRegSlot1) {
          simd_spill_1.insert(simd);
          // if both [0,1] are available, then 64bit total is avail
          for (MapleSet<regno_t>::iterator it = simd_spill_0.begin(); it != simd_spill_0.end(); it++) {
            regno_t sreg = static_cast<regno_t>(*it);
            if (sreg == simd) {
              simd_spill_regs.insert(simd);
              break;
            }
          }
        } else {
          simd_spill_regs.insert(simd_reg_reclaim[i]);
        }
      }
    }
  }
}

// Peep optimization.  Turned off due to +/- 0.0 issue.
void LSRALinearScanRegAllocator::SimdSpillToFcmpOpt() {
  FOR_ALL_BB(bb, cgfunc_) {
    Insn *insn = bb->lastinsn;

    if (!insn) {
      continue;
    }

    MOperator thisMop = insn->GetMachineOpcode();
    switch (thisMop) {
      case MOP_wcbnz:
      case MOP_xcbnz:
      case MOP_wcbz:
      case MOP_xcbz: {
        /*  Issue #1876
         * mov x16 <- freg         fcmp freq, #0.0
         * cbnz x16, label   ==>   bne  label
         *
         * or cbz ==> beq
         */
        if (insn == bb->firstinsn) {
          break;
        }
        Operand *brOpnd = insn->GetOperand(0);
        RegOperand *brRegopnd = static_cast<RegOperand *>(brOpnd);
        if (brRegopnd->GetRegisterNumber() != R16) {
          break;
        }
        Insn *previnsn = insn->prev;
        while (previnsn != bb->firstinsn->prev && !previnsn->GetMachineOpcode()) {
          previnsn = previnsn->prev;
        }
        MOperator prevMop = previnsn->GetMachineOpcode();
        if (prevMop != MOP_xvmovrv && prevMop != MOP_xvmovrd) {
          break;
        }
        Operand *mvOpnd1 = previnsn->GetOperand(1);
        uint8 regsize;
        if (mvOpnd1->GetSize() <= 32) {
          break;
          // break;
          // regsize = 32;
        } else {
          // Has to be double
          regsize = 64;
        }
        Operand *mvOpnd0 = previnsn->GetOperand(0);
        RegOperand *mvRegopnd0 = static_cast<RegOperand *>(mvOpnd0);
        if (mvRegopnd0->GetRegisterNumber() != R16) {
          break;
        }

        // Make sure rflag is not live-out
        Operand *rflag = cgfunc_->GetOrCreateRflag();
        CG_ASSERT(static_cast<RegOperand *>(rflag) != nullptr,
                  "nullptr in LSRALinearScanRegAllocator::SimdSpillToFcmpOpt");
        uint32 ccReg = static_cast<RegOperand *>(rflag)->GetRegisterNumber();
        if (bb->liveout_regno.find(ccReg) != bb->liveout_regno.end()) {
          break;
        }

        // Insn in place, optimize
        previnsn->SetOperand(0, rflag);
        ImmFPZeroOperand *immOpnd = ImmFPZeroOperand::allocate(regsize);
        previnsn->SetOperand(2, immOpnd);
        previnsn->SetMOP(MOP_dcmpqri);
        insn->SetOperand(0, rflag);
        if (thisMop == MOP_wcbnz || thisMop == MOP_xcbnz) {
          insn->SetMOP(MOP_bne);
        } else if (thisMop == MOP_wcbz || thisMop == MOP_xcbz) {
          insn->SetMOP(MOP_beq);
        }
        break;
      }
      default:
        break;
    }  // switch
  }
}

// Main entrance for the LSRA register allocator.
bool LSRALinearScanRegAllocator::AllocateRegisters() {
  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc_);
  // we store both FP/LR if using FP or if not using FP, but func has a call
  if (a64cgfunc->ShouldSaveFPLR()) {
    // Using FP, record it for saving
    a64cgfunc->AddtoCalleeSaved(RFP);
    a64cgfunc->AddtoCalleeSaved(RLR);
    a64cgfunc->NoteFPLRAddedToCalleeSavedList();
  }

  if (LSRA_DUMP) {
    MIRModule &mirModule = cgfunc_->mirModule;
    DotGenerator::GenerateDot("RA", cgfunc_, &mirModule);
    DotGenerator::GenerateDot("RAe", cgfunc_, &mirModule, true);
  }

  if (CGOptions::doPreLsraOpt) {
    RaX0Opt x0Opt;
    x0Opt.PropagateX0(cgfunc_);
    if (LSRA_DUMP) {
      LogInfo::MapleLogger() << "******** CG IR After PreLSRA: *********" << endl;
      cgfunc_->DumpCGIR();
    }
  }

  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "Entering LinearScanRegAllocator\n";
  }

  {
    BB *cleanupBb = nullptr;
    FOR_ALL_BB(bb, cgfunc_) {
      bb->internal_flag1 = 0;  // Use to mark cleanup bb
      if (bb->firststmt == cgfunc_->cleanup_label) {
        cleanupBb = bb;
      }
    }
    for (BB *bb = cleanupBb; bb; bb = bb->next) {
      bb->internal_flag1 = 1;
    }
  }

  ComputeLiveInterval();

#ifdef LSRA_GRAPH
  PrintLiveRanges();
#endif
  LiveIntervalAnalysis();
  InitFreeRegPool();

  BuildIntervalRanges();

  if (CGOptions::fastAlloc == true) {
    if (CGOptions::fastAllocMode == 0) {
      fast_alloc = true;
    } else {
      spill_all = true;
    }
    // In-Range spill range can still be specified (only works with --dump-func=).
  } else if (cgfunc_->NumBBs() > CGOptions::lsraBbOptSize) {
    // instruction size is checked in ComputeLieveInterval()
    fast_alloc = true;
  }

  if (LSRA_DUMP) {
    if (fast_alloc) {
      LogInfo::MapleLogger() << "fast_alloc mode on\n";
    }
    if (spill_all) {
      LogInfo::MapleLogger() << "spill_all mode on\n";
    }
    PrintParamQueue("Initial param queue");
    PrintCallQueue("Initial call queue");
  }
  // handle param register
  for (uint32 i = 0; i < int_param_queue.size(); i++) {
    if (int_param_queue[i].size() != 0 && int_param_queue[i].front()->first_def == 0) {
      LiveInterval *li = int_param_queue[i].front();
      int_param_reg_set.erase(li->assigned_reg - R0);
      active.insert(li);
      int_param_queue[i].pop_front();
    }
  }
  for (uint32 i = 0; i < fp_param_queue.size(); i++) {
    if (fp_param_queue[i].size() != 0 && fp_param_queue[i].front()->first_def == 0) {
      LiveInterval *li = fp_param_queue[i].front();
      fp_param_reg_set.erase(li->assigned_reg - V0);
      active.insert(li);
      fp_param_queue[i].pop_front();
    }
  }

  for (uint32_t bbIdx = 0; bbIdx < sorted_bbs.size(); bbIdx++) {
    BB *bb = sorted_bbs[bbIdx];
    if (LSRA_DUMP) {
      LogInfo::MapleLogger() << "======New BB=====" << bb->id << " " << std::hex << bb << std::dec << "\n";
    }
    FOR_BB_INSNS(insn, bb) {
      if (insn->IsImmaterialInsn()) {
        continue;
      }
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      if (insn->id == 0) {
        // New instruction inserted by reg alloc (ie spill)
        continue;
      }
      if (LSRA_DUMP) {
        LogInfo::MapleLogger() << "======New Insn=====" << insn->id << " " << insn->bb->id << "\n";
        insn->dump();
      }
      RetireFromActive(insn);
#ifdef LSRA_DEBUG
      DebugCheckActiveList();
#endif
      AssignPhysRegsForInsn(insn);
      if (LSRA_DUMP) {
        LogInfo::MapleLogger() << "======After Alloc=====" << insn->id << " " << insn->bb->id << "\n";
        insn->dump();
      }
    }
  }
  FinalizeRegisters();

  if (LSRA_DUMP) {
    LogInfo::MapleLogger() << "Total " << spill_count << " spill_count in " << cgfunc_->GetName() << " \n";
    LogInfo::MapleLogger() << "Total " << reload_count << " reload_count\n";
    LogInfo::MapleLogger() << "Total "
         << "(" << spill_count << "+ " << caller_save_spill_count << ") = " << spill_count + caller_save_spill_count
         << " SPILL\n";
    LogInfo::MapleLogger() << "Total "
         << "(" << reload_count << "+ " << caller_save_reload_count << ") = " << reload_count + caller_save_reload_count
         << " RELOAD\n";

    LogInfo::MapleLogger() << "Total " << simd_spill_count << " simd d <- x\n";
    LogInfo::MapleLogger() << "Total " << simd_reload_count << " simd x <- d\n";
  }

  return true;
}

AnalysisResult *CgDoRegAlloc::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  MemPool *phaseMp = mempoolctrler.NewMemPool("CG phase-wise pool");
  MapleAllocator phaseAllocator(phaseMp);
  // It doesn't need live range information when -O1, because the register will not live out of bb.
  if (g->optim_level >= 2) {
    if (CGOptions::doLiveAnalysisEh) {
      LiveAnalysis *live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVEEH, cgfunc));
    } else {
      LiveAnalysis *live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));
    }
  }
  RegAllocator *regallocator = cgfunc->NewRegAllocator(cgfunc, phaseMp, &phaseAllocator);
  CHECK_FATAL(regallocator != nullptr, "regallocator is null in CgDoRegAlloc::Run");
  m->GetAnalysisResult(CgFuncPhase_LOOP, cgfunc);
  cgfunc->SetIsAfterRegAlloc();
  regallocator->AllocateRegisters();
  // the live range info may changed, so invalid the info.
  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  m->InvalidAnalysisResult(CgFuncPhase_LOOP, cgfunc);
  mempoolctrler.DeleteMemPool(phaseMp);

  return nullptr;
}

}  // namespace maplebe
