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
#include "x86_cg.h"
#include "x86reg_alloc.h"
#include "cg_assert.h"

namespace maplebe {

void X86RegAllocator::AllocSrcOpnd(Operand *opnd, OpndProp *prop) {
  X86OpndProp *opndprop = static_cast<X86OpndProp *>(prop);
  if (opnd->op_kind_ == Operand::Opd_Register) {
    X86RegOperand *regopnd = static_cast<X86RegOperand *>(opnd);
    if (regopnd->IsPhysicalRegister()) {
      allocated_set_.insert(opnd);
      avail_reg_set_[regopnd->GetRegisterNumber()] = false;
      live_reg_.insert(regopnd->GetRegisterNumber());
      return;
    }
    auto reg_map_it = reg_map_.find(regopnd->GetRegisterNumber());
    if (reg_map_it != reg_map_.end()) {  // already allocated this register
      regopnd->SetRegisterNumber(reg_map_it->second);
      allocated_set_.insert(opnd);
      return;
    }
    if (opndprop && opndprop->IsPhysicalRegister()) {
      regopnd->SetRegisterNumber(opndprop->regprop_.physical_reg_);
      allocated_set_.insert(opnd);
      live_reg_.insert(opndprop->regprop_.physical_reg_);
      return;
    }
    if (AllocateReg(regopnd)) {
      allocated_set_.insert(opnd);
      return;
    }
    // can not assign any register here, need to insert spill code
    CG_ASSERT(false, "spill register here");
  } else if (opnd->op_kind_ == Operand::Opd_Mem) {
    X86MemOperand *memopnd = static_cast<X86MemOperand *>(opnd);
    switch (memopnd->addr_mode_) {
      case X86MemOperand::Addressing_BO:
        AllocSrcOpnd(memopnd->GetBaseRegister());
        break;
      case X86MemOperand::Addressing_BOIS:
        AllocSrcOpnd(memopnd->GetBaseRegister());
        AllocSrcOpnd(memopnd->GetIndexRegister());
        break;
      case X86MemOperand::Addressing_ISO:
        AllocSrcOpnd(memopnd->GetIndexRegister());
        break;
      case X86MemOperand::Addressing_O:
      case X86MemOperand::Addressing_TEXT:
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

void X86RegAllocator::AllocDestOpnd(Operand *opnd, Insn *insn, uint32 index) {
  if (opnd->op_kind_ == Operand::Opd_Register) {
    X86RegOperand *resopnd = static_cast<X86RegOperand *>(opnd);
    RegType regty = resopnd->GetRegisterType();
    if (resopnd->IsPhysicalRegister()) {
      if (live_reg_.find(resopnd->GetRegisterNumber()) == live_reg_.end()) {
        return;
      }
      // CG_ASSERT spill code for this living physic register here
      CG_ASSERT(false, "result physic register spill here");
    }
    const X86MD *insn_md = &X86CG::thex86machine[static_cast<X86Insn *>(insn)->mop_];
    X86OpndProp *opndprop = static_cast<X86OpndProp *>(insn_md->operand_[index]);
    if (opndprop->IsPhysicalRegister()) {  // physic register
      X86register physic_reg = opndprop->regprop_.physical_reg_;
      CG_ASSERT(live_reg_.find(physic_reg) == live_reg_.end(), "physic register spill NYI");
      resopnd->SetRegisterNumber(opndprop->regprop_.physical_reg_);
      reg_map_[resopnd->GetRegisterNumber()] = opndprop->regprop_.physical_reg_;
      return;
    }
    auto reg_map_it = reg_map_.find(resopnd->GetRegisterNumber());
    if (reg_map_it != reg_map_.end()) {
      uint8 reg = reg_map_it->second;
      resopnd->SetRegisterNumber(reg);
      //  the living register is over here, add the register to availabe again
      if (!insn->IsCondDef()) {
        ReleaseReg(regty, reg);
      }
    } else {
      // this register defined with no use
      if (!insn->IsCondDef()) {
        std::cerr << "Warning: "
                  << "register:" << resopnd->GetRegisterNumber() << " defined without a use\n";
      }
      AllocateReg(resopnd);
      if (!insn->IsCondDef()) {
        ReleaseReg(resopnd->GetRegisterType(), resopnd->GetRegisterNumber());
      }
    }
  }
}

void X86RegAllocator::PreAllocate() {
  for (BB *bb = cgfunc_->firstbb; bb; bb = bb->next) {
    if (bb->IsEmpty()) {
      continue;
    }
    Insn *next_insn = nullptr;
    Insn *lastinsn_next = bb->lastinsn->next;
    for (Insn *insn = bb->firstinsn; insn && insn != lastinsn_next; insn = next_insn) {
      next_insn = insn->next;
      const X86MD *insn_md = &X86CG::thex86machine[static_cast<X86Insn *>(insn)->mop_];
      if (!insn_md->UseSpecReg()) {
        continue;
      }
      for (int i = 0; i < Insn::MAX_OPERAND_NUM; i++) {
        Operand *opnd = insn->opnds[i];
        if (!opnd) {
          break;
        }
        X86OpndProp *opndprop = static_cast<X86OpndProp *>(insn_md->operand_[i]);
        if (opndprop->IsPhysicalRegister()) {
          Operand *phyreg = static_cast<X86CGFunc *>(cgfunc_)->GetOrCreatePhysicalRegisterOperand(
            opndprop->regprop_.physical_reg_, opnd->size_, kRegTyInt);
          if (opndprop->is_res_) {
            Insn *x86movinsn = cgfunc_->cg->BuildInstruction<X86Insn>(
              static_cast<X86CGFunc *>(cgfunc_)->PickMovInsn(static_cast<X86RegOperand *>(opnd),
                                                             static_cast<X86RegOperand *>(phyreg), false),
              static_cast<RegOperand *>(opnd), static_cast<RegOperand *>(phyreg));
            bb->InsertInsnAfter(insn, x86movinsn);
          } else {
            Insn *x86movinsn = cgfunc_->cg->BuildInstruction<X86Insn>(
              static_cast<X86CGFunc *>(cgfunc_)->PickMovInsn(static_cast<X86RegOperand *>(phyreg),
                                                             static_cast<X86RegOperand *>(opnd), false),
              static_cast<RegOperand *>(phyreg), static_cast<RegOperand *>(opnd));
            bb->InsertInsnBefore(insn, x86movinsn);
          }
          insn->opnds[i] = phyreg;
        }
      }
    }
  }
}

bool X86RegAllocator::AllocateRegisters() {
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
      const X86MD *insn_md = &X86CG::thex86machine[static_cast<X86Insn *>(insn)->mop_];
      bool isx86style = insn_md->IsX86Style();
      Insn *x86movinsn = nullptr;
      if (isx86style) {
        /* for x86 style, we will do:
         * r1 = r2 op r3 expand to
            r1 = r2;
            r1 = r1 op r3
         */
        X86RegOperand *rsh = dynamic_cast<X86RegOperand *>(insn->opnds[0]);
        X86RegOperand *lsh = dynamic_cast<X86RegOperand *>(insn->opnds[1]);
        if (rsh != lsh) {
          x86movinsn = cgfunc_->cg->BuildInstruction<X86Insn>(
            static_cast<X86CGFunc *>(cgfunc_)->PickMovInsn(lsh, rsh, false), rsh, lsh);
          insn->opnds[1] = rsh;
        }
      }
      for (int i = 0; i < Insn::MAX_OPERAND_NUM; i++) {  // the dest registers
        Operand *opnd = insn->opnds[i];
        if (!opnd) {
          continue;
        }
        if (!static_cast<X86OpndProp *>(insn_md->operand_[i])->is_res_) {
          break;
        }
        if (allocated_set_.find(opnd) != allocated_set_.end()) {
          // free the live range of this register
          X86RegOperand *regopnd = dynamic_cast<X86RegOperand *>(opnd);
          CG_ASSERT(regopnd, "only register can be a result");
          if (!insn->IsCondDef() &&
              !x86movinsn) {  // when insert x86 style mov, the kill of the dest live range must delay
            ReleaseReg(regopnd);
          }
          continue;  // already allocated
        }

        if (opnd->op_kind_ == Operand::Opd_Register) {
          AllocDestOpnd(opnd, insn, i);
        }
      }
      for (int i = 0; i < Insn::MAX_OPERAND_NUM; i++) {  // the src registers
        Operand *opnd = insn->opnds[i];
        if (!opnd || static_cast<X86OpndProp *>(insn_md->operand_[i])->is_res_) {
          continue;
        }
        AllocSrcOpnd(opnd, insn_md->operand_[i]);
      }
      if (x86movinsn) {  // allocate the x86 style mov instruction
        ReleaseReg(static_cast<X86RegOperand *>(x86movinsn->opnds[0]));
        AllocSrcOpnd(x86movinsn->opnds[1]);
        RegOperand *destopnd = dynamic_cast<RegOperand *>(x86movinsn->opnds[0]);
        RegOperand *srcopnd = dynamic_cast<RegOperand *>(x86movinsn->opnds[1]);
        if (destopnd->GetRegisterNumber() != srcopnd->GetRegisterNumber()) {
          bb->InsertInsnBefore(insn, x86movinsn);
        }
      }
    }
  }
  return true;
}

void X86RegAllocator::GetPhysicalRegisterBank(RegType regty, uint8 &begin, uint8 &end) {
  switch (regty) {
    case RegTy_cc:
      begin = RCC;
      end = RCC;
      break;
    case kRegTyInt:
      begin = RAX;
      end = R15;
      break;
    case kRegTyFloat:
      begin = XMM0;
      end = XMM15;
      break;
    default:
      CG_ASSERT(false, "NYI");
      break;
  }
}

void X86RegAllocator::InitAvailReg() {
  memset_s(avail_reg_set_, sizeof(avail_reg_set_), 0, sizeof(avail_reg_set_));
  for (int j = 0; j < MAXREG; j++) {
    avail_reg_set_[j] = true;
  }
  avail_reg_set_[RSP] = false;
  avail_reg_set_[RBP] = false;
  avail_reg_set_[RAX] = false;
}

void X86RegAllocator::ReleaseReg(X86RegOperand *regopnd) {
  ReleaseReg(regopnd->GetRegisterType(), regopnd->GetRegisterNumber());
}

void X86RegAllocator::ReleaseReg(RegType regty, uint8 reg) {
  CG_ASSERT((reg >= 0 && reg < 100), "can't release virtual register");
  live_reg_.erase(reg);
  avail_reg_set_[reg] = true;
}

// tring to allocate a physic register to opnd. return true if success
bool X86RegAllocator::AllocateReg(X86RegOperand *opnd) {
  RegType regtype = opnd->GetRegisterType();
  uint8 reg_start, reg_end;
  GetPhysicalRegisterBank(regtype, reg_start, reg_end);

  for (uint8 reg = reg_start; reg <= reg_end; reg++) {
    if (!avail_reg_set_[reg]) {
      continue;
    }

    reg_map_[opnd->GetRegisterNumber()] = reg;
    opnd->SetRegisterNumber(reg);  // assign register
    avail_reg_set_[reg] = false;
    live_reg_.insert(reg);  // this register is living now
    return true;
  }
  return false;
}

}  // namespace maplebe
