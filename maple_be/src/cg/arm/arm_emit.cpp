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

#include "emit.h"
#include "cg.h"
#include "arm_isa.h"
#include "arm_cg.h"
#include "cg_assert.h"
#include <string>

namespace maplebe {

using namespace maple;

const char *ArmCG::int_reg_name1[R15 + 1] = {
  "null", "null", "null", "null", "null", "null", "null", "null", "null",
  "null", "null", "null", "null", "null", "null", "null", "null",
};
const char *ArmCG::int_reg_name2[R15 + 1] = {
  "null", "null", "null", "null", "null", "null", "null", "null", "null",
  "null", "null", "null", "null", "null", "null", "null", "null",
};
const char *ArmCG::int_reg_name4[MAXREG] = { "null", "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",  "r8",
                                             "r9",   "r10", "r11", "ip",  "sp",  "lr",  "pc",  "s0",  "s1",  "s2",
                                             "s3",   "s4",  "s5",  "s6",  "s7",  "s8",  "s9",  "s10", "s11", "s12",
                                             "s13",  "s14", "s15", "s16", "s17", "s18", "s19", "s20", "s21", "s22",
                                             "s23",  "s24", "s25", "s26", "s27", "s28", "s29", "s30", "s31" };
const char *ArmCG::int_reg_name8[MAXREG] = { "null", "r0",  "null", "r2",  "null", "r4",  "null", "r6",  "null", "r8",
                                             "null", "r10", "null", "r12", "null", "r14", "null", "d0",  "null", "d1",
                                             "null", "d2",  "null", "d3",  "null", "d4",  "null", "d5",  "null", "d6",
                                             "null", "d7",  "null", "d8",  "null", "d9",  "null", "d10", "null", "d11",
                                             "null", "d12", "null", "d13", "null", "d14", "null", "d15", "null" };

void ArmInsn::Emit(CG &cg, Emitter &emitter) {
  MOperator mop = GetMachineOpcode();
  const ARMMD *md = &ArmCG::thearmmachine[mop];

  if (mop == MOP_Tbpush ||
      mop == MOP_Tbpop) {  // for instructions using list operand, if the operand's size is zero, then, it's a nop
    ListOperand *listopnd = dynamic_cast<ListOperand *>(opnds[1]);
    if (!listopnd || listopnd->GetOperands().size() == 0) {
      return;
    }
  }

  std::string format(md->format_);
  emitter.emit("\t").emit(md->name_).emit("  ");
  int seq[5];
  std::string prefix[5];  // used for print prefix like "*" in icall *rax
  int index = 0;
  memset_s(seq, sizeof(seq), -1, sizeof(seq));
  int comma_num = 0;
  for (uint32 i = 0; i < format.length(); i++) {
    char c = format[i];
    if (c >= '0' && c <= '5') {
      seq[index++] = c - '0';
      comma_num++;
    } else if (c != ',') {
      prefix[index].push_back(c);
    }
  }
  for (int i = 0; i < comma_num; i++) {
    if (seq[i] == -1) {
      continue;
    }
    if (prefix[i].length() > 0) {
      emitter.emit(prefix[i]);
    }
    opnds[seq[i]]->Emit(emitter, md->operand_[seq[i]]);
    if (i != (comma_num - 1)) {
      emitter.emit(",");
    }
  }
  emitter.emit("\n");
}

void ArmCGFunc::Emit() {
  Emitter &emitter = *cg->emitter_;
  // emit header of this function
  emitter.emit("\t.text\n");  // emit .text section
  MIRSymbol *func_st = globaltable.GetSymbolFromStidx(func->stidx.Idx());
  emitter.emit("\t.globl  ").emit(func_st->GetName()).emit("\n");
  emitter.emit("\t.type  ").emit(func_st->GetName()).emit(", %function\n");
  emitter.emit(func_st->GetName()).emit(":\n");

  // emit instructions
  for (BB *bb = firstbb; bb; bb = bb->next) {
    // emit bb headers
    if (bb->labidx != 0) {
      emitter.emit(".label.").emit(func_st->GetName()).emit(bb->labidx).emit(":\n");
    }
    for (Insn *insn = bb->firstinsn; insn && insn != bb->lastinsn->next; insn = insn->next) {
      insn->Emit(*cg, emitter);
    }
  }

  uint32 size = func->symtab->GetSymbolTableSize();
  for (size_t i = 0; i < size; i++) {
    MIRSymbol *st = func->symtab->GetSymbolFromStidx(i);
    if (st == nullptr) {
      continue;
    }
    MIRStorageClass storageClass = st->storageClass;
    MIRSymKind sKind = st->sKind;
    if (storageClass == kScPstatic && sKind == kStConst) {
      emitter.emit("\t.align 2\n");
      emitter.emit(st->GetName().c_str());
      emitter.emit(":\n");
      switch (st->GetConst()->type_->GetPrimType()) {
        case PTY_u32: {
          MIRIntConst *intconst = static_cast<MIRIntConst *>(st->GetConst());
          emitter.emit("\t.long ");
          emitter.emit(static_cast<uint32>(intconst->value));
          emitter.emit("\n");
          break;
        }
        case PTY_f32: {
          MIRFloatConst *floatconst = static_cast<MIRFloatConst *>(st->GetConst());
          emitter.emit("\t.word ");
          emitter.emit(static_cast<uint32>(floatconst->GetIntValue()));
          emitter.emit("\n");
          break;
        }
        case PTY_f64: {
          MIRDoubleConst *doubleconst = static_cast<MIRDoubleConst *>(st->GetConst());
          emitter.emit("\t.word ");
          emitter.emit(doubleconst->GetIntLow32());
          emitter.emit("\n");
          emitter.emit("\t.word ");
          emitter.emit(doubleconst->GetIntHigh32());
          emitter.emit("\n");
          break;
        }
        default:
          CG_ASSERT(false, "NYI");
          break;
      }
    }
  }
  for (MapleVector<MIRSymbol *>::iterator stit = emitstvec_.begin(); stit != emitstvec_.end();
       stit++) {  // emit switch table only here
    MIRSymbol *st = *stit;
    CG_ASSERT(st->isReadOnly(), "NYI");
    emitter.emit("\n");
    emitter.emit(".align 2\n");
    emitter.emit(st->GetName().c_str());
    emitter.emit(":\n");
    MIRAggConst *array_const = dynamic_cast<MIRAggConst *>(st->GetConst());
    for (uint32 i = 0; i < array_const->const_vec.size(); i++) {
      MIRLblConst *lblconst = dynamic_cast<MIRLblConst *>(array_const->const_vec[i]);
      emitter.emit("\t.word ");
      emitter.emit(".label.");
      emitter.emit(func_st->GetName().c_str());
      emitter.emit(lblconst->value);
      emitter.emit("+1\n");
    }
  }
}

void ArmCGFunc::EmitOperand(Operand *opnd, OpndProp *prop) {
  ofstream &outf = cg->emitter_->out;
  ArmOpndProp *opndprop = static_cast<ArmOpndProp *>(prop);
  if (ArmRegOperand *regopnd = dynamic_cast<ArmRegOperand *>(opnd)) {
    RegType regtype = regopnd->GetRegisterType();
    CG_ASSERT((!opndprop || (opndprop->opnd_ty_ == Operand::Opd_Register)), "operand type doesn't match");
    uint8 size = opndprop ? opndprop->size_ : regopnd->size_;  // opndprop null means a sub emit, i.e from ArmMemOperand
    regno_t reg_no = regopnd->GetRegisterNumber();
    switch (regtype) {
      case kRegTyInt:
      case kRegTyFloat: {
        switch (size) {
          case 8:
            outf << ArmCG::int_reg_name1[reg_no];
            break;
          case 16:
            outf << ArmCG::int_reg_name2[reg_no];
            break;
          case 32:
            outf << ArmCG::int_reg_name4[regopnd->IsAsHigh32() ? (reg_no + 1) : reg_no];
            break;
          case 64: {
            if (opndprop && opndprop->IsRegHigh()) {
              outf << ArmCG::int_reg_name4[reg_no + 1];
            } else {
              outf << ArmCG::int_reg_name8[reg_no];
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
  } else if (ImmOperand *immopnd = dynamic_cast<ImmOperand *>(opnd)) {
    outf << "#" << immopnd->GetValue();
  } else if (OfstOperand *ofstopnd = dynamic_cast<OfstOperand *>(opnd)) {
    outf << "#" << ofstopnd->GetValue();
  } else if (ArmMemOperand *memopnd = dynamic_cast<ArmMemOperand *>(opnd)) {
    ArmMemOperand::ArmAdrMode addr_mode = memopnd->addr_mode_;
    if (addr_mode == ArmMemOperand::Addressing_TEXT) {
      if (opndprop->IsMemLow16()) {
        outf << "#:lower16:";
        outf << memopnd->GetSymbol()->GetName();
        return;
      }
      if (opndprop->IsMemHigh16()) {
        outf << "#:upper16:";
        outf << memopnd->GetSymbol()->GetName();
        return;
      }
      outf << memopnd->GetSymbol()->GetName();
    } else if (addr_mode == ArmMemOperand::Addressing_BO) {
      outf << "[";
      EmitOperand(memopnd->GetBaseRegister(), nullptr);
      if (OfstOperand *offset_opnd = dynamic_cast<OfstOperand *>(memopnd->GetOffsetOperand())) {
        if (offset_opnd->GetValue() != 0) {
          outf << ",";
          EmitOperand(offset_opnd, nullptr);
        }
      } else if (RegOperand *reg_opnd = dynamic_cast<RegOperand *>(memopnd->GetOffsetOperand())) {
        outf << ",";
        EmitOperand(reg_opnd, nullptr);
      }
      outf << "]";
    } else if (addr_mode == ArmMemOperand::Addressing_O) {
      StImmOperand *stopnd = dynamic_cast<StImmOperand *>(memopnd->GetOffsetOperand());
      CG_ASSERT(stopnd, "ArmMemOperand with Addressing_O has nullptr stimm_opnd_");
      int64 offset = stopnd->offset_;
      if (opndprop->IsMemLow16()) {
        CG_ASSERT(offset == 0, "NYI");
        outf << "#:lower16:";
        outf << stopnd->st_->GetName();
        return;
      }
      if (opndprop->IsMemHigh16()) {
        CG_ASSERT(offset == 0, "NYI");
        outf << "#:upper16:";
        outf << stopnd->st_->GetName();
        return;
      }

      if (offset != 0) {
        outf << "[";
      }
      outf << stopnd->st_->GetName();
      if (offset > 0) {
        outf << "+" << offset;
      } else if (offset < 0) {
        outf << offset;
      }
      if (offset != 0) {
        outf << "]";
      }
    } else if (addr_mode == ArmMemOperand::Addressing_ISO) {
      StImmOperand *stopnd = dynamic_cast<StImmOperand *>(memopnd->GetOffsetOperand());
      if (stopnd) {
        outf << stopnd->st_->GetName();
        outf << "(,";  // skip base
        EmitOperand(memopnd->GetIndexRegister(), nullptr);
        outf << ",";
        ImmOperand *immopnd = dynamic_cast<ImmOperand *>(memopnd->GetScaleOperand());
        outf << immopnd->GetValue();
        outf << ")";
      } else {
        CG_ASSERT(false, "offset is not a StImmOperand NYI");
      }
    } else {
      CG_ASSERT(false, "nyi");
    }
  } else if (LabelOperand *lblopnd = dynamic_cast<LabelOperand *>(opnd)) {
    MIRSymbol *func_st = globaltable.GetSymbolFromStidx(func->stidx.Idx());
    outf << ".label." << func_st->GetName() << lblopnd->GetLabelIndex();
  } else if (ListOperand *listopnd = dynamic_cast<ListOperand *>(opnd)) {
    ArmListOperand *alo = static_cast<ArmListOperand *>(listopnd);
    outf << "{";
    alo->sort();
    uint32 size = alo->GetOperands().size();
    uint32 i = 0;

    for (auto opnd : alo->GetOperands()) {
      EmitOperand(opnd, nullptr);
      if (i != (size - 1)) {
        outf << ",";
      }
      i++;
    }

    outf << "}";
  } else {
    CG_ASSERT(false, "NYI");
  }
}

}  // namespace maplebe
