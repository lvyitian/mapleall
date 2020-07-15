/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan PSL v1.
 * You can use this software according to the terms and conditions of the Mulan PSL v1.
 * You may obtain a copy of Mulan PSL v1 at:
 *
 *     http://license.coscl.org.cn/MulanPSL
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v1 for more details.
 */

#include <iostream>
#include "cg.h"
#include "arm_isa.h"
#include "arm_cg.h"
#include "arm_operand.h"
#include "cg_assert.h"

namespace maplebe {

void ArmRegOperand::Emit(Emitter &emitter, OpndProp *opndprop) {
  ArmOpndProp *armopndprop = static_cast<ArmOpndProp *>(opndprop);
  CG_ASSERT((!opndprop || (armopndprop->opnd_ty_ == Operand::Opd_Register)), "operand type doesn't match");
  // opndprop null means a sub emit, i.e from MemOperand
  uint8 size = opndprop ? armopndprop->size_ : size_;
  switch (GetRegisterType()) {
    case kRegTyInt:
    case kRegTyFloat: {
      switch (size) {
        case 8:
          emitter.emit(ArmCG::int_reg_name1[reg_no_]);
          break;
        case 16:
          emitter.emit(ArmCG::int_reg_name2[reg_no_]);
          break;
        case 32:
          emitter.emit(ArmCG::int_reg_name4[IsAsHigh32() ? (reg_no_ + 1) : reg_no_]);
          break;
        case 64: {
          if (opndprop && armopndprop->IsRegHigh()) {
            emitter.emit(ArmCG::int_reg_name4[reg_no_ + 1]);
          } else {
            emitter.emit(ArmCG::int_reg_name8[reg_no_]);
          }
          break;
        }
      }
      break;
    }
    default:
      CG_ASSERT(false, "NYI");
      break;
  }
}

ArmMemOperand::ArmMemOperand(MIRSymbol *symbol, int32 offset, int32 size, ArmCGFunc *cgfunc)
  : MemOperand(size, symbol) {
  MIRStorageClass storageClass = symbol->storageClass;
  if (storageClass == kScAuto || storageClass == kScFormal) {
    addr_mode_ = Addressing_BO;
    SymbolAlloc &symalloc = *cgfunc->memlayout->sym_alloc_table[symbol->GetStIndex()];
    ArmSymbolAlloc *psymalloc = static_cast<ArmSymbolAlloc *>(&symalloc);
    MemPool *mp = CG::cur_cgfunc_->memPool;
    MemOperand::SetBaseRegister(static_cast<RegOperand *>(cgfunc->GetBaseReg(psymalloc)));
    uint32 stoffset = cgfunc->GetBaseOffset(psymalloc);
    MemOperand::SetOffsetOperand(mp->New<ArmOfstOperand>(stoffset + offset, 32));
  } else if (storageClass == kScText) {
    addr_mode_ = Addressing_TEXT;
  } else if (storageClass == kScGlobal || storageClass == kScExtern || storageClass == kScFstatic) {
    //  global variabls generate symbol immediate
    addr_mode_ = Addressing_O;
    MemOperand::SetOffsetOperand(CG::cur_cgfunc_->memPool->New<StImmOperand>(symbol, offset, 0));
  } else {
    CG_ASSERT(false, "NYI");
  }
}

void ArmMemOperand::Emit(Emitter &emitter, OpndProp *prop) {
  ArmOpndProp *opndprop = static_cast<ArmOpndProp *>(prop);
  if (addr_mode_ == ArmMemOperand::Addressing_TEXT) {
    if (opndprop->IsMemLow16()) {
      emitter.emit("#:lower16:");
    } else if (opndprop->IsMemHigh16()) {
      emitter.emit("#:upper16:");
    }
    emitter.emit(GetSymbol()->GetName());
  } else if (addr_mode_ == ArmMemOperand::Addressing_BO) {
    emitter.emit("[");
    GetBaseRegister()->Emit(emitter, nullptr);
    Operand *oo = GetOffsetOperand();
    CG_ASSERT((oo->IsRegister() || oo->IsIntImmediate()), "offset can only be either register or int immediate");
    if (oo->IsIntImmediate()) {
      if (!static_cast<ArmImmOperand *>(oo)->IsZero()) {
        // if needed, we can allocate prop, and pass it offset_opnd_
        emitter.emit(",");
        oo->Emit(emitter, nullptr);
      }
    } else {
      emitter.emit(",");
      oo->Emit(emitter, nullptr);
    }
  } else if (addr_mode_ == ArmMemOperand::Addressing_O) {
    StImmOperand *stopnd = dynamic_cast<StImmOperand *>(GetOffsetOperand());
    CG_ASSERT(stopnd, "MemOperand with Addressing_O has nullptr stimm_opnd_");
    int64 offset = stopnd->offset_;
    if (opndprop->IsMemLow16()) {
      CG_ASSERT(offset == 0, "NYI");
      emitter.emit("#:lower16:");
      stopnd->Emit(emitter, nullptr);
    } else if (opndprop->IsMemHigh16()) {
      CG_ASSERT(offset == 0, "NYI");
      emitter.emit("#:upper16:");
      stopnd->Emit(emitter, nullptr);
    } else {
      stopnd->Emit(emitter, nullptr);
    }
  } else if (addr_mode_ == ArmMemOperand::Addressing_ISO) {
    StImmOperand *stopnd = dynamic_cast<StImmOperand *>(GetOffsetOperand());
    if (stopnd) {
      CG_ASSERT(stopnd->offset_ == 0, "NYI");
      stopnd->Emit(emitter, nullptr);
      emitter.emit("(,");  // skip base
      GetIndexRegister()->Emit(emitter, nullptr);
      emitter.emit(",");
      GetScaleOperand()->Emit(emitter, nullptr);
      emitter.emit(")");
    } else {
      CG_ASSERT(false, "offset is not a StImmOperand NYI");
    }
  } else {
    CG_ASSERT(false, "nyi");
  }
}

void ArmMemOperand::dump() {
  cout << "Mem:";
  switch (addr_mode_) {
    case Addressing_BO: {
      cout << "base:";
      GetBaseRegister()->dump();
      cout << "offset:";
      GetOffsetOperand()->dump();
      break;
    }
    case Addressing_BOIS: {
      cout << "base:";
      GetBaseRegister()->dump();
      cout << "index:";
      GetIndexRegister()->dump();
      cout << "offset:";
      GetOffsetOperand()->dump();
      cout << "scale:";
      GetScaleOperand()->dump();
      break;
    }
    case Addressing_ISO: {
      cout << "index:";
      GetIndexRegister()->dump();
      cout << "offset:";
      GetOffsetOperand()->dump();
      cout << "scale:";
      GetScaleOperand()->dump();
      break;
    }
    case Addressing_O: {
      cout << "offset:";
      GetOffsetOperand()->dump();
      break;
    }
    case Addressing_TEXT: {
      cout << GetSymbol()->GetName();
      break;
    }
    default:
      CG_ASSERT(false, "error memoperand dump");
  }
}

void StImmOperand::Emit(Emitter &emitter, OpndProp *opndprop) {
  if (offset_ != 0) {
    emitter.emit("[");
  }

  emitter.emit(st_->GetName());

  if (offset_ > 0) {
    emitter.emit("+").emit(offset_);
  } else if (offset_ < 0) {
    emitter.emit(offset_);
  }

  if (offset_ != 0) {
    emitter.emit("]");
  }
}

ArmRegOperand *ArmListOperand::SearchRegister(ArmRegOperand *opnd) {
  CG_ASSERT(opnd->IsPhysicalRegister(), "only physical register uses this function");
  regno_t reg = opnd->IsAsHigh32() ? opnd->GetRegisterNumber() + 1 : opnd->GetRegisterNumber();

  for (auto regopnd : vec_opnds_) {
    ArmRegOperand *aro = static_cast<ArmRegOperand *>(regopnd);
    regno_t treg = aro->IsAsHigh32() ? aro->GetRegisterNumber() + 1 : aro->GetRegisterNumber();
    if (treg == reg) {
      return aro;
    }
  }
  return nullptr;
}

// sort the register operand according to their number
void ArmListOperand::sort() {
  MapleList<RegOperand *> &opnds_vec = GetOperands();
  uint32 size = opnds_vec.size();
  uint32 i = 0;
  vector<RegOperand *> tmpvec;
  while (i < size) {
    auto it = opnds_vec.begin();
    ArmRegOperand *aro = static_cast<ArmRegOperand *>(*it);
    if (it != opnds_vec.end()) {
      it++;
    }
    for (; it != opnds_vec.end(); it++) {
      ArmRegOperand *nxt = static_cast<ArmRegOperand *>(*it);
      if (!CompareOperand(aro, nxt)) {
        aro = nxt;
      }
    }
    tmpvec.push_back(aro);
    opnds_vec.remove(aro);
    i++;
  }
  for (i = 0; i < size; i++) {
    opnds_vec.push_back(tmpvec[i]);
  }
}

void ArmListOperand::Emit(Emitter &emitter, OpndProp *opndprop) {
  uint32 n_left = vec_opnds_.size();
  if (n_left == 0) {
    return;
  }

  sort();

  emitter.emit("{");

  auto it = vec_opnds_.begin();

  for (; it != vec_opnds_.end(); ++it) {
    (*it)->Emit(emitter, nullptr);
    if (--n_left >= 1) {
      emitter.emit(",");
    }
  }
  CG_ASSERT(n_left == 0, "some are missed...");

  emitter.emit("}");
}

}  // namespace maplebe
