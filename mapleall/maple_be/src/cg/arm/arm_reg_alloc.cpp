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

#include <iostream>
#include "mir_lower.h"
#include "arm_cg.h"
#include "arm_reg_alloc.h"
#include "cg_assert.h"

namespace maplebe {

void ArmRegAllocator::AllocSrcOpnd(Operand *opnd, OpndProp *prop) {
  ArmOpndProp *opndprop = static_cast<ArmOpndProp *>(prop);
  if (opnd->op_kind_ == Operand::Opd_Register) {
    ArmRegOperand *regopnd = static_cast<ArmRegOperand *>(opnd);
    if (regopnd->IsPhysicalRegister()) {
      allocated_set_.insert(opnd);
      avail_reg_set_[regopnd->GetRegisterNumber()] = false;
      live_reg_.insert(regopnd->GetRegisterNumber());
      if (opndprop && opndprop->size_ == 64) {  // 64bits register occupy 2 consecutive registers
        CG_ASSERT(((regopnd->GetRegisterNumber() % 2) != 0), "CG_ASSERT wrong register to 64 bits register");
        avail_reg_set_[regopnd->GetRegisterNumber() + 1] = false;
        live_reg_.insert(regopnd->GetRegisterNumber() + 1);
      }
      return;
    }
    auto reg_map_it = reg_map_.find(regopnd->GetRegisterNumber());
    if (reg_map_it != reg_map_.end()) {  // already allocated this register
      regopnd->SetRegisterNumber(reg_map_it->second);
      avail_reg_set_[regopnd->GetPhysicalRegister()] =
        false;  // make sure the real register can not be allocated and live
      live_reg_.insert(regopnd->GetPhysicalRegister());
      allocated_set_.insert(opnd);
      return;
    }
    if (opndprop && opndprop->IsPhysicalRegister()) {
      regopnd->SetRegisterNumber(opndprop->regprop_.physical_reg_);
      allocated_set_.insert(opnd);
      avail_reg_set_[regopnd->GetRegisterNumber()] = false;
      live_reg_.insert(opndprop->regprop_.physical_reg_);
      if (opndprop->size_ == 64) {
        CG_ASSERT(((regopnd->GetRegisterNumber() % 2) != 0), "CG_ASSERT wrong register to 64 bits register");
        avail_reg_set_[regopnd->GetRegisterNumber() + 1] = false;
        live_reg_.insert(regopnd->GetRegisterNumber() + 1);
      }
      return;
    }
    if (AllocateReg(regopnd, opndprop)) {
      allocated_set_.insert(opnd);
      return;
    }
    // can not assign any register here, need to insert spill code
    CG_ASSERT(false, "spill register here");
  } else if (opnd->op_kind_ == Operand::Opd_Mem) {
    ArmMemOperand *memopnd = static_cast<ArmMemOperand *>(opnd);
    switch (memopnd->addr_mode_) {
      case ArmMemOperand::Addressing_BO:
        AllocSrcOpnd(memopnd->GetBaseRegister());
        break;
      case ArmMemOperand::Addressing_BOIS:
        AllocSrcOpnd(memopnd->GetBaseRegister());
        AllocSrcOpnd(memopnd->GetIndexRegister());
        break;
      case ArmMemOperand::Addressing_ISO:
        AllocSrcOpnd(memopnd->GetIndexRegister());
        break;
      case ArmMemOperand::Addressing_O:
      case ArmMemOperand::Addressing_TEXT:
        break;
      default:
        CG_ASSERT(false, "ERROR");
    }
    allocated_set_.insert(opnd);
    return;
  } else {
    return;
  }
}

void ArmRegAllocator::AllocDestOpnd(Operand *opnd, Insn *insn, uint32 index) {
  if (opnd->op_kind_ == Operand::Opd_Register) {
    ArmRegOperand *resopnd = static_cast<ArmRegOperand *>(opnd);
    RegType regty = resopnd->GetRegisterType();
    const ARMMD *insn_md = &ArmCG::thearmmachine[static_cast<ArmInsn *>(insn)->mop_];
    ArmOpndProp *opndprop = static_cast<ArmOpndProp *>(insn_md->operand_[index]);
    if (resopnd->IsPhysicalRegister()) {
      if (live_reg_.find(resopnd->GetPhysicalRegister()) == live_reg_.end()) {
        uint8 reg = resopnd->GetPhysicalRegister();
        ReleaseReg(opndprop->regprop_.regtype_, reg);
        if (opndprop->size_ == 64) {
          ReleaseReg(opndprop->regprop_.regtype_, reg + 1);
        }
      }
      return;
    }

    if (opndprop->IsPhysicalRegister()) {  // physical register
      Armregister physical_reg = opndprop->regprop_.physical_reg_;
      CG_ASSERT(live_reg_.find(physical_reg) == live_reg_.end(), "physical register spill NYI");
      resopnd->SetRegisterNumber(opndprop->regprop_.physical_reg_);
      reg_map_[resopnd->GetRegisterNumber()] = opndprop->regprop_.physical_reg_;
      ReleaseReg(opndprop->regprop_.regtype_, resopnd->GetRegisterNumber());
      if (opndprop->size_ == 64) {
        ReleaseReg(opndprop->regprop_.regtype_, resopnd->GetRegisterNumber() + 1);
      }
      return;
    }
    auto reg_map_it = reg_map_.find(resopnd->GetRegisterNumber());
    if (reg_map_it != reg_map_.end()) {
      uint8 reg = reg_map_it->second;
      resopnd->SetRegisterNumber(reg);
      //  the living register is over here, add the register to availabe again
      if (!insn->IsCondDef()) {
        if (resopnd->IsAsHigh32()) {
          ReleaseReg(regty, reg + 1);
        } else {
          ReleaseReg(regty, reg);
          if (opndprop->size_ == 64) {
            ReleaseReg(regty, reg + 1);
          }
        }
      }
    } else {
      // this register defined with no use
      if (!insn->IsCondDef())
        ;
      AllocateReg(resopnd, opndprop);
      if (!insn->IsCondDef()) {
        if (resopnd->IsAsHigh32()) {
          ReleaseReg(resopnd->GetRegisterType(), resopnd->GetRegisterNumber() + 1);
        } else {
          ReleaseReg(resopnd->GetRegisterType(), resopnd->GetRegisterNumber());
          if (opndprop->size_ == 64) {
            ReleaseReg(resopnd->GetRegisterType(), resopnd->GetRegisterNumber() + 1);
          }
        }
      }
    }
  }
}

void ArmRegAllocator::PreAllocate() {
  for (BB *bb = cgfunc_->firstbb; bb; bb = bb->next) {
    if (bb->IsEmpty()) {
      continue;
    }
    Insn *next_insn = nullptr;
    Insn *lastinsn_next = bb->lastinsn->next;
    for (Insn *insn = bb->firstinsn; insn && insn != lastinsn_next; insn = next_insn) {
      next_insn = insn->next;
      const ARMMD *insn_md = &ArmCG::thearmmachine[static_cast<ArmInsn *>(insn)->mop_];
      if (!insn_md->UseSpecReg()) {
        continue;
      }
      for (int i = 0; i < Insn::MAX_OPERAND_NUM; i++) {
        Operand *opnd = insn->opnds[i];
        if (!opnd) {
          break;
        }
        ArmOpndProp *opndprop = static_cast<ArmOpndProp *>(insn_md->operand_[i]);
        if (opndprop->IsPhysicalRegister()) {
          Operand *phyreg = static_cast<ArmCGFunc *>(cgfunc_)->GetOrCreatePhysicalRegisterOperand(
            opndprop->regprop_.physical_reg_, opnd->size_, kRegTyInt);
          if (opndprop->IsRegDef()) {
            Insn *armmovinsn = cgfunc_->cg->BuildInstruction<ArmInsn>(
              static_cast<ArmCGFunc *>(cgfunc_)->PickMovInsn(static_cast<RegOperand *>(opnd),
                                                             static_cast<RegOperand *>(phyreg)),
              static_cast<RegOperand *>(opnd), static_cast<RegOperand *>(phyreg));
            bb->InsertInsnAfter(insn, armmovinsn);
          } else {
            Insn *armmovinsn = cgfunc_->cg->BuildInstruction<ArmInsn>(
              static_cast<ArmCGFunc *>(cgfunc_)->PickMovInsn(static_cast<RegOperand *>(phyreg),
                                                             static_cast<RegOperand *>(opnd)),
              static_cast<RegOperand *>(phyreg), static_cast<RegOperand *>(opnd));
            bb->InsertInsnBefore(insn, armmovinsn);
          }
          insn->opnds[i] = phyreg;
        }
      }
    }
  }
}

void ArmRegAllocator::AllocHandleCallee(Insn *insn) {
  ListOperand *srcopnds = static_cast<ListOperand *>(insn->opnds[1]);
  for (auto regopnd : srcopnds->GetOperands()) {
    avail_reg_set_[regopnd->GetRegisterNumber()] = false;
    live_reg_.insert(regopnd->GetRegisterNumber());
    if (regopnd->size_ == 64) {
      CG_ASSERT(((regopnd->GetRegisterNumber() % 2) != 0), "CG_ASSERT wrong register to 64 bits register");
      avail_reg_set_[regopnd->GetRegisterNumber() + 1] = false;
      live_reg_.insert(regopnd->GetRegisterNumber() + 1);
    }
  }
}

bool ArmRegAllocator::AllocateRegisters() {
  InitAvailReg();
  PreAllocate();

  for (BB *bb = cgfunc_->lastbb; bb; bb = bb->prev) {
    if (bb->IsEmpty()) {
      continue;
    }
    Insn *firstinsn_prev = bb->firstinsn->prev;
    Insn *prev_insn = nullptr;
    for (Insn *insn = bb->lastinsn; insn && insn != firstinsn_prev; insn = prev_insn) {
      prev_insn = insn->prev;
      const ARMMD *insn_md = &ArmCG::thearmmachine[static_cast<ArmInsn *>(insn)->mop_];
      if (insn_md->IsCall()) {
        AllocHandleCallee(insn);
        continue;
      }
      for (int i = 0; i < Insn::MAX_OPERAND_NUM; i++) {  // the dest registers
        Operand *opnd = insn->opnds[i];
        if (!opnd || !static_cast<ArmOpndProp *>(insn_md->operand_[i])->IsRegDef()) {
          continue;
        }
        if (allocated_set_.find(opnd) != allocated_set_.end()) {
          // free the live range of this register
          ArmRegOperand *regopnd = dynamic_cast<ArmRegOperand *>(opnd);
          CG_ASSERT(regopnd, "only register can be a result");
          SaveCalleeRegs(regopnd);
          if (!insn->IsCondDef()) {
            ReleaseReg(regopnd, insn_md->operand_[i]);
          }
          continue;  // already allocated
        }

        if (opnd->op_kind_ == Operand::Opd_Register) {
          AllocDestOpnd(opnd, insn, i);
          SaveCalleeRegs(static_cast<ArmRegOperand *>(opnd));
        }
      }
      for (int i = 0; i < Insn::MAX_OPERAND_NUM; i++) {  // the src registers
        Operand *opnd = insn->opnds[i];
        if (!opnd ||
            !(static_cast<ArmOpndProp *>(insn_md->operand_[i])->IsRegUse() || opnd->op_kind_ == Operand::Opd_Mem)) {
          continue;
        }
        AllocSrcOpnd(opnd, insn_md->operand_[i]);
      }
    }
  }
  return true;
}

void ArmRegAllocator::GetPhysicalRegisterBank(RegType regty, uint8 &begin, uint8 &end) {
  switch (regty) {
    case RegTy_cc:
      begin = RCC;
      end = RCC;
      break;
    case kRegTyInt:
      begin = R0;
      end = R15;
      break;
    case kRegTyFloat:
      begin = S16;
      end = S31;
      break;
    default:
      CG_ASSERT(false, "NYI");
      break;
  }
}

void ArmRegAllocator::InitAvailReg() {
  memset_s(avail_reg_set_, sizeof(avail_reg_set_), 0, sizeof(avail_reg_set_));
  for (int j = 0; j < MAXREG; j++) {
    avail_reg_set_[j] = true;
  }
  avail_reg_set_[RSP] = false;
  avail_reg_set_[RLR] = false;
  avail_reg_set_[RIP] = false;
  avail_reg_set_[RPC] = false;
  ArmCGFunc *cgfunc = static_cast<ArmCGFunc *>(cgfunc_);
  for (MapleVector<Armregister>::iterator it = cgfunc->formal_reg_list_.begin(); it != cgfunc->formal_reg_list_.end();
       it++) {
    avail_reg_set_[*it] = false;
  }
}

bool ArmRegAllocator::IsSpecialReg(Armregister reg)  // these registers can not be allocated
{
  if (reg == RLR || reg == RIP || reg == RSP || reg == RPC) {
    return true;
  }
  ArmCGFunc *cgfunc = static_cast<ArmCGFunc *>(cgfunc_);
  for (MapleVector<Armregister>::iterator it = cgfunc->formal_reg_list_.begin(); it != cgfunc->formal_reg_list_.end();
       it++) {
    if (*it == reg) {
      return true;
    }
  }
  return false;
}

void ArmRegAllocator::ReleaseReg(ArmRegOperand *regopnd, OpndProp *prop) {
  if (regopnd->IsAsHigh32()) {
    ReleaseReg(regopnd->GetRegisterType(), regopnd->GetRegisterNumber() + 1);
  } else {
    ReleaseReg(regopnd->GetRegisterType(), regopnd->GetRegisterNumber());
    if (static_cast<ArmOpndProp *>(prop)->size_ == 64) {
      ReleaseReg(regopnd->GetRegisterType(), regopnd->GetRegisterNumber() + 1);
    }
  }
}

void ArmRegAllocator::ReleaseReg(RegType regty, uint8 reg) {
  CG_ASSERT((reg >= 0 && reg < 100), "can't release virtual register");
  live_reg_.erase(reg);
  if (!IsSpecialReg((Armregister)reg)) {
    avail_reg_set_[reg] = true;
  }
}

// tring to allocate a physical register to opnd. return true if success
bool ArmRegAllocator::AllocateReg(ArmRegOperand *opnd, OpndProp *prop) {
  RegType regtype = opnd->GetRegisterType();
  uint8 reg_start, reg_end;
  GetPhysicalRegisterBank(regtype, reg_start, reg_end);

  for (uint8 reg = reg_start; reg <= reg_end; reg++) {
    if (!avail_reg_set_[reg]) {
      continue;
    }
    if (((prop && static_cast<ArmOpndProp *>(prop)->size_ == 64) || opnd->IsAsHigh32() || opnd->IsAsLow32()) &&
        ((reg % 2 == 0) || !avail_reg_set_[reg + 1])) {  // 64 bits register always start at r0/d0, r2/d2.....
      continue;
    }

    reg_map_[opnd->GetRegisterNumber()] = reg;
    opnd->SetRegisterNumber(reg);  // assign register
    avail_reg_set_[reg] = false;
    if (!opnd->IsAsHigh32()) {
      live_reg_.insert(reg);  // this register is living now
    }
    if ((prop && static_cast<ArmOpndProp *>(prop)->size_ == 64) || opnd->IsAsHigh32()) {
      avail_reg_set_[reg + 1] = false;
      live_reg_.insert(reg + 1);
    }
    return true;
  }
  return false;
}

// if opnd is a callee saved register, save it in the prolog and restore it in the epilog
// we defined a listopnd in a function(saved_opnds_), just push them to the list
void ArmRegAllocator::SaveCalleeRegs(ArmRegOperand *opnd) {
  ArmCGFunc *armcgfunc = static_cast<ArmCGFunc *>(cgfunc_);
  ArmListOperand *saved_opnds = armcgfunc->GetOrCreateSavedOpnds();
  if (opnd->size_ == 64) {
    ArmRegOperand *loopnd = static_cast<ArmRegOperand *>(armcgfunc->GetLow32bitsOpnd(opnd));
    ArmRegOperand *hiopnd = static_cast<ArmRegOperand *>(armcgfunc->GetHigh32bitsOpnd(opnd));
    SaveCalleeRegs(loopnd);
    SaveCalleeRegs(hiopnd);
    return;
  }
  regno_t reg = opnd->IsAsHigh32() ? opnd->GetRegisterNumber() + 1 : opnd->GetRegisterNumber();
  if (IsCalleeSaveReg((Armregister)reg)) {
    ArmRegOperand *searchopnd = saved_opnds->SearchRegister(opnd);
    if (!searchopnd) {
      saved_opnds->push_front(opnd);
    }
  }
}

}  // namespace maplebe
