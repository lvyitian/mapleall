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

#include "x86_operand.h"
#include "x86_cg.h"

namespace maplebe {

void X86RegOperand::Emit(Emitter &emitter, OpndProp *opndprop) {
  X86OpndProp *x86opndprop = static_cast<X86OpndProp *>(opndprop);
  CG_ASSERT((!x86opndprop || x86opndprop->opnd_ty_ == Operand::Opd_Register), "operand type doesn't match");
  // opndprop null means a sub emit, i.e from MemOperand
  uint8 size = x86opndprop ? x86opndprop->size_ : size_;
  switch (GetRegisterType()) {
    case kRegTyInt:
    case kRegTyFloat: {
      switch (size) {
        case 8:
          emitter.emit(X86CG::int_reg_name1[reg_no_]);
          break;
        case 16:
          emitter.emit(X86CG::int_reg_name2[reg_no_]);
          break;
        case 32:
          emitter.emit(X86CG::int_reg_name4[reg_no_]);
          break;
        case 64:
          emitter.emit(X86CG::int_reg_name8[reg_no_]);
          break;
      }
      break;
    }
    default:
      CG_ASSERT(false, "NYI");
      break;
  }
}

void X86OfstOperand::Emit(Emitter &emitter, OpndProp *opndprop) {
  emitter.emit(val_);
}

void X86OfstOperand::dump() {
  cout << "ofst:" << val_;
}

void X86MemOperand::Emit(Emitter &emitter, OpndProp *opndprop) {
  if (addr_mode_ == X86MemOperand::Addressing_TEXT) {
    emitter.emit(GetSymbol()->GetName());
  } else if (addr_mode_ == X86MemOperand::Addressing_BO) {
    GetOffsetOperand()->Emit(emitter, nullptr);
    emitter.emit("(");
    GetBaseRegister()->Emit(emitter, nullptr);
    emitter.emit(")");
  } else if (addr_mode_ == X86MemOperand::Addressing_O) {
    GetOffsetOperand()->Emit(emitter, nullptr);
  } else if (addr_mode_ == X86MemOperand::Addressing_ISO) {
    Operand *oo = GetOffsetOperand();
    if (oo) {
      StImmOperand *stopnd = dynamic_cast<StImmOperand *>(oo);
      CG_ASSERT(stopnd->offset_ == 0, "incorrect StImmOperand; offset_ must be 0");
      oo->Emit(emitter, nullptr);
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

void StImmOperand::Emit(Emitter &emitter, OpndProp *opndprop) {
  int offset = offset_;
  if (offset != 0) {
    emitter.emit("(");
  }

  emitter.emit(st_->GetName());

  if (offset > 0)
    emitter.emit("+").emit(offset).emit(")");
  else if (offset < 0)
    emitter.emit(offset).emit(")");
}

X86MemOperand::X86MemOperand(MIRSymbol *symbol, int32 offset, int32 size, MemLayout *memlayout)
  : MemOperand(size, symbol) {
  MIRStorageClass storageClass = symbol->storageClass;
  if (storageClass == kScAuto || storageClass == kScFormal) {
    addr_mode_ = Addressing_BO;
    SymbolAlloc symalloc = *memlayout->sym_alloc_table[symbol->GetStIndex()];
    X86SymbolAlloc *psymalloc = static_cast<X86SymbolAlloc *>(&symalloc);
    MemPool *mp = CG::cur_cgfunc_->memPool;
    MemOperand::SetBaseRegister(mp->New<X86RegOperand>(psymalloc->BaseReg(), 64, kRegTyInt));
    MemOperand::SetOffsetOperand(mp->New<X86OfstOperand>(psymalloc->Offset() + offset, 32));
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

void X86MemOperand::dump() {
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

}  // namespace maplebe
