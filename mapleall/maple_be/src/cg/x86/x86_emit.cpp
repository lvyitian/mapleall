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

#include "emit.h"
#include "cg.h"
#include "x86_isa.h"
#include "x86_cg.h"
#include "cg_assert.h"
#include <string>

namespace maplebe {

using namespace maple;

const char *X86CG::int_reg_name1[R15 + 1] = { "null",  "%al",   "%bl",   "%bpl",  "%spl", "%dil",
                                              "%sil",  "%dl",   "%cl",   "%r8b",  "%r9b", "%r10b",
                                              "%r11b", "%r12b", "%r13b", "%r14b", "%r15b" };
const char *X86CG::int_reg_name2[R15 + 1] = { "null",  "%ax",   "%bx",   "%bp",   "%sp",  "%di",
                                              "%si",   "%dx",   "%cx",   "%r8w",  "%r9w", "%r10w",
                                              "%r11w", "%r12w", "%r13w", "%r14w", "%r15w" };
const char *X86CG::int_reg_name4[MAXREG] = { "null",   "%eax",   "%ebx",   "%ebp",   "%esp",  "%edi",  "%esi",
                                             "%edx",   "%ecx",   "%r8d",   "%r9d",   "%r10d", "%r11d", "%r12d",
                                             "%r13d",  "%r14d",  "%r15d",  "%xmm0",  "%xmm1", "%xmm2", "%xmm3",
                                             "%xmm4",  "%xmm5",  "%xmm6",  "%xmm7",  "%xmm8", "%xmm9", "%xmm10",
                                             "%xmm11", "%xmm12", "%xmm13", "%xmm14", "%xmm15" };
const char *X86CG::int_reg_name8[MAXREG] = { "null",   "%rax",   "%rbx",   "%rbp",   "%rsp",  "%rdi",  "%rsi",
                                             "%rdx",   "%rcx",   "%r8",    "%r9",    "%r10",  "%r11",  "%r12",
                                             "%r13",   "%r14",   "%r15",   "%xmm0",  "%xmm1", "%xmm2", "%xmm3",
                                             "%xmm4",  "%xmm5",  "%xmm6",  "%xmm7",  "%xmm8", "%xmm9", "%xmm10",
                                             "%xmm11", "%xmm12", "%xmm13", "%xmm14", "%xmm15" };

void X86Insn::Emit(CG &cg, Emitter &emitter) {
  MOperator mop = GetMachineOpcode();
  const X86MD *md = &X86CG::thex86machine[mop];

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

void X86CGFunc::Emit() {
  // emit header of this function
  Emitter &emitter = *cg->emitter_;
  emitter.emit("\t.text\n");  // emit .text section
  MIRSymbol *func_st = globaltable.GetSymbolFromStidx(func->stidx.Idx());
  emitter.emit("\t.globl  ").emit(func_st->GetName()).emit("\n");
  emitter.emit("\t.type  ").emit(func_st->GetName()).emit(", @function\n");
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

  for (MapleVector<MIRSymbol *>::iterator stit = emitstvec_.begin(); stit != emitstvec_.end();
       stit++) {  // emit switch table only here
    MIRSymbol *st = *stit;
    CG_ASSERT(st->isReadOnly(), "NYI");
    emitter.emit(emitter.asminfo_->asm_section);
    emitter.emit(emitter.asminfo_->asm_rodata);
    emitter.emit("\n");
    emitter.emit(".align 8\n");
    emitter.emit(st->GetName().c_str());
    emitter.emit(":\n");
    MIRAggConst *array_const = dynamic_cast<MIRAggConst *>(st->GetConst());
    for (uint32 i = 0; i < array_const->const_vec.size(); i++) {
      MIRLblConst *lblconst = dynamic_cast<MIRLblConst *>(array_const->const_vec[i]);
      emitter.emit("\t.quad ");
      emitter.emit(".label.");
      emitter.emit(func_st->GetName().c_str());
      emitter.emit(lblconst->value);
      emitter.emit("\n");
    }
  }
}

void X86CGFunc::EmitOperand(Operand *opnd, OpndProp *prop) {
  ofstream &outf = cg->emitter_->out;
  X86OpndProp *opndprop = static_cast<X86OpndProp *>(prop);
  if (X86RegOperand *regopnd = dynamic_cast<X86RegOperand *>(opnd)) {
    RegType regtype = regopnd->GetRegisterType();
    CG_ASSERT((!opndprop || opndprop->opnd_ty_ == Operand::Opd_Register), "operand type doesn't match");
    uint8 size = opndprop ? opndprop->size_ : regopnd->size_;  // opndprop null means a sub emit, i.e from MemOperand
    regno_t reg_no = regopnd->GetRegisterNumber();
    switch (regtype) {
      case kRegTyInt:
      case kRegTyFloat: {
        switch (size) {
          case 8:
            outf << X86CG::int_reg_name1[reg_no];
            break;
          case 16:
            outf << X86CG::int_reg_name2[reg_no];
            break;
          case 32:
            outf << X86CG::int_reg_name4[reg_no];
            break;
          case 64:
            outf << X86CG::int_reg_name8[reg_no];
            break;
        }
        break;
      }
      default:
        CG_ASSERT(false, "NYI");
        break;
    }
  } else if (ImmOperand *immopnd = dynamic_cast<ImmOperand *>(opnd)) {
    outf << "$" << immopnd->GetValue();
  } else if (OfstOperand *ofstopnd = dynamic_cast<OfstOperand *>(opnd)) {
    outf << ofstopnd->GetValue();
  } else if (X86MemOperand *memopnd = dynamic_cast<X86MemOperand *>(opnd)) {
    X86MemOperand::X86AdrMode addr_mode = memopnd->addr_mode_;
    if (addr_mode == X86MemOperand::Addressing_TEXT) {
      outf << memopnd->GetSymbol()->GetName();
    } else if (addr_mode == X86MemOperand::Addressing_BO) {
      EmitOperand(memopnd->GetOffsetOperand(), nullptr);
      outf << "(";
      EmitOperand(memopnd->GetBaseRegister(), nullptr);
      outf << ")";
    } else if (addr_mode == X86MemOperand::Addressing_O) {
      StImmOperand *stopnd = dynamic_cast<StImmOperand *>(memopnd->GetOffsetOperand());
      CG_ASSERT(stopnd, "MemOperand with Addressing_O has nullptr stimm_opnd_");
      int offset = stopnd->offset_;
      if (offset != 0) {
        outf << "(";
      }
      outf << stopnd->st_->GetName();
      if (offset > 0) {
        outf << "+" << offset;
      } else if (offset < 0) {
        outf << offset;
      }
      if (offset != 0) {
        outf << ")";
      }
    } else if (addr_mode == X86MemOperand::Addressing_ISO) {
      StImmOperand *stopnd = dynamic_cast<StImmOperand *>(memopnd->GetOffsetOperand());
      if (stopnd) {
        outf << stopnd->st_->GetName();
        outf << "(,";  // skip base
        EmitOperand(memopnd->GetBaseRegister(), nullptr);
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
    outf << ".label." << func_st->GetName() << lblopnd->labidx_;
  } else {
    CG_ASSERT(false, "NYI");
  }
}

}  // namespace maplebe
