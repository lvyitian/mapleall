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

#include "cg.h"
#include "x86_isa.h"
#include "x86_cg.h"
#include "cg_assert.h"
#include <iostream>

namespace maplebe {

using namespace maple;

MOperator X86CGFunc::PickAddInsn(PrimType primtype, bool ismem) {
  switch (primtype) {
    case PTY_i8:
    case PTY_u8:
    case PTY_i16:
    case PTY_u16:
    case PTY_i32:
    case PTY_u32:
    case PTY_a32:
      return ismem ? MOP_add32rm : MOP_add32rr;
    case PTY_i64:
    case PTY_u64:
    case PTY_a64:
      return ismem ? MOP_add64rm : MOP_add64rr;
    case PTY_f32:
      return ismem ? MOP_addssrm : MOP_addssrr;
    case PTY_f64:
      return ismem ? MOP_addsdrm : MOP_addsdrr;
    default:
      CG_ASSERT(false, "NYI PickAddInsn");
      return MOP_undef;
  }
}

MOperator X86CGFunc::PickMpyInsn(PrimType primtype, bool ismem) {
  switch (primtype) {
    case PTY_i8:
    case PTY_u8:
    case PTY_i16:
    case PTY_u16:
    case PTY_i32:
    case PTY_u32:
    case PTY_a32:
      return ismem ? MOP_imul32rm : MOP_imul32rr;
    case PTY_i64:
    case PTY_u64:
    case PTY_a64:
      return ismem ? MOP_imul64rm : MOP_imul32rr;
    case PTY_f32:
      return ismem ? MOP_mulssrm : MOP_mulssrr;
    case PTY_f64:
      return ismem ? MOP_mulsdrm : MOP_mulsdrr;
    default:
      CG_ASSERT(false, "NYI PickAddInsn");
      return MOP_undef;
  }
}

MOperator X86CGFunc::PickCmovInsn(bool is64bits, bool issigned, Opcode opcode) {
  MOperator mop;
  switch (opcode) {
    case OP_le:
      mop = is64bits ? (issigned ? MOP_cmovle64 : MOP_cmovbe64) : (issigned ? MOP_cmovle32 : MOP_cmovbe32);
      break;
    case OP_ge:
      mop = is64bits ? (issigned ? MOP_cmovge64 : MOP_cmovae64) : (issigned ? MOP_cmovge32 : MOP_cmovae32);
      break;
    case OP_lt:
      mop = is64bits ? (issigned ? MOP_cmovl64 : MOP_cmovb64) : (issigned ? MOP_cmovl32 : MOP_cmovb32);
      break;
    case OP_gt:
      mop = is64bits ? (issigned ? MOP_cmovg64 : MOP_cmova64) : (issigned ? MOP_cmovg32 : MOP_cmova32);
      break;
    case OP_ne:
      mop = is64bits ? MOP_cmovne64 : MOP_cmovne32;
      break;
    case OP_eq:
      mop = is64bits ? MOP_cmove64 : MOP_cmove32;
      break;
    default:
      CG_ASSERT(false, "unexpected compare");
      mop = 0;
  }
  return mop;
}

// opnd1 is the second operand of this insn, we need to choose different insn according to its type
MOperator X86CGFunc::PickCmpInsn(PrimType primtype, Operand *opnd1) {
  if (opnd1->op_kind_ == Operand::Opd_Immediate && !static_cast<ImmOperand *>(opnd1)->IsInRange(4, true)) {
    opnd1 = SelectCopy(opnd1, IsSignedInteger(primtype) ? PTY_i64 : PTY_u64);
  }
  switch (primtype) {
    case PTY_u1:
    case PTY_i32:
    case PTY_u32:
    case PTY_a32: {
      switch (opnd1->op_kind_) {
        case Operand::Opd_Register:
          return MOP_cmp32rr;
        case Operand::Opd_Immediate:
          return MOP_cmp32ri;
        case Operand::Opd_Mem:
          return MOP_cmp32rm;
        default:
          CG_ASSERT(false, "unexpected operand for compare");
      }
    }
    case PTY_i64:
    case PTY_u64:
    case PTY_a64: {
      switch (opnd1->op_kind_) {
        case Operand::Opd_Register:
          return MOP_cmp64rr;
        case Operand::Opd_Immediate:
          return MOP_cmp64ri;
        case Operand::Opd_Mem:
          return MOP_cmp64rm;
        default:
          CG_ASSERT(false, "unexpected operand for compare");
      }
    }
    case PTY_f32: {
      switch (opnd1->op_kind_) {
        case Operand::Opd_Register:
          return MOP_comissrr;
        case Operand::Opd_Mem:
          return MOP_comissrm;
        default:
          CG_ASSERT(false, "unexpected operand for compare");
      }
    }
    case PTY_f64: {
      switch (opnd1->op_kind_) {
        case Operand::Opd_Register:
          return MOP_comisdrr;
        case Operand::Opd_Mem:
          return MOP_comisdrm;
        default:
          CG_ASSERT(false, "unexpected operand for compare");
      }
    }
    default:
      CG_ASSERT(false, "NYI PickCmpInsn");
      return MOP_undef;
  }
}

MOperator X86CGFunc::PickSubInsn(PrimType primtype, bool ismem) {
  switch (primtype) {
    case PTY_i8:
    case PTY_u8:
    case PTY_i16:
    case PTY_u16:
    case PTY_i32:
    case PTY_u32:
    case PTY_a32:
      return ismem ? MOP_sub32rm : MOP_sub32rr;
    case PTY_i64:
    case PTY_u64:
    case PTY_a64:
      return ismem ? MOP_sub64rm : MOP_sub64rr;
    case PTY_f32:
      return ismem ? MOP_subssrm : MOP_subssrr;
    case PTY_f64:
      return ismem ? MOP_subsdrm : MOP_subsdrr;
    default:
      CG_ASSERT(false, "NYI PickAddInsn");
      return MOP_undef;
  }
}

void X86CGFunc::SelectCondGoto(CondGotoNode *stmt, Operand *opnd0, Operand *opnd1) {
  LabelIdx labelidx = stmt->offset;
  base_node_t *condnode = stmt->Opnd(0);
  Operand *targetopnd = GetOrCreateLabelOperand(labelidx);
  Operand *rflag = CreateRflagOpnd();
  if (opnd0->op_kind_ != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, condnode->primType);
  }
  MOperator mop = PickCmpInsn(condnode->primType, opnd1);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, rflag, opnd0, opnd1));

  PrimType primType = condnode->primType;
  MOperator jmp_op;
  bool issigned = IsSignedInteger(primType);
  switch (condnode->op) {
    default:  // if opt is not a comparison, then handle as != zero
    case OP_ne: {
      jmp_op = (OP_brtrue == stmt->op ? MOP_jne : MOP_je);
      break;
    }
    case OP_eq: {
      jmp_op = (OP_brtrue == stmt->op ? MOP_je : MOP_jne);
      break;
    }
    case OP_lt: {
      jmp_op = (OP_brtrue == stmt->op ? (issigned ? MOP_jl : MOP_jb) : (issigned ? MOP_jge : MOP_jae));
      break;
    }
    case OP_le: {
      jmp_op = (OP_brtrue == stmt->op ? (issigned ? MOP_jle : MOP_jbe) : (issigned ? MOP_jg : MOP_ja));
      break;
    }
    case OP_gt: {
      jmp_op = (OP_brtrue == stmt->op ? (issigned ? MOP_jg : MOP_ja) : (issigned ? MOP_jle : MOP_jbe));
      break;
    }
    case OP_ge: {
      jmp_op = (OP_brtrue == stmt->op ? (issigned ? MOP_jge : MOP_jae) : (issigned ? MOP_jl : MOP_jb));
      break;
    }
  }

  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(jmp_op, rflag, targetopnd));
}

void X86CGFunc::SelectGoto(GotoNode *stmt) {
  Operand *targetopnd = GetOrCreateLabelOperand(stmt->offset);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_jmp, targetopnd));
}

Operand *X86CGFunc::SelectAdd(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectAdd(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void X86CGFunc::SelectAdd(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectAdd(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectAdd(resopnd, opnd1, opnd0, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    uint32 s0size = opnd0->size_;
    if (s0size < GetPrimTypeBitSize(prmtype)) {
      SelectAdd(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
    }
    if (opnd1ty == Operand::Opd_Immediate) {
      ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
      if (immopnd1->IsInRange(4, true)) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>((is64bits ? MOP_add64ri : MOP_add32ri), resopnd, opnd0, opnd1));
      } else if (!is64bits) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_add32ri, resopnd, opnd0, immopnd1));
      } else {
        SelectAdd(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
      }
    } else if (opnd1ty == Operand::Opd_Mem) {
      uint32 s1size = opnd1->size_;
      if (s1size < GetPrimTypeBitSize(prmtype)) {
        SelectAdd(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
      } else {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickAddInsn(prmtype, true), resopnd, opnd0, opnd1));
      }
    } else {
      CG_ASSERT(false, "NYI SelectAdd");
    }
  } else {  // both operands are register
    if (opnd0->size_ < GetPrimTypeBitSize(prmtype)) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    if (opnd1->size_ < GetPrimTypeBitSize(prmtype)) {
      opnd1 = SelectCopy(opnd1, prmtype);
    }
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickAddInsn(prmtype, false), resopnd, opnd0, opnd1));
  }
}

Operand *X86CGFunc::SelectShift(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  Opcode opcode = node->op;
  SHIFTDIRECTION direct = (opcode == OP_lshr) ? ShiftLright : (opcode == OP_ashr ? ShiftAright : ShiftLeft);
  SelectShift(resopnd, opnd0, opnd1, direct, prmtype);
  return resopnd;
}

void X86CGFunc::SelectSub(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, prmtype);
  }
  if (opnd1ty != Operand::Opd_Register) {
    uint32 s0size = opnd0->size_;
    if (s0size < GetPrimTypeBitSize(prmtype)) {
      SelectSub(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
    }
    if (opnd1ty == Operand::Opd_Immediate) {
      ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
      if (immopnd1->IsInRange(4, true)) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>((is64bits ? MOP_sub64ri : MOP_sub32ri), resopnd, opnd0, opnd1));
      } else if (!is64bits) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_sub32ri, resopnd, opnd0, immopnd1));
      } else {
        SelectSub(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
      }
    } else if (opnd1ty == Operand::Opd_Mem) {
      uint32 s1size = opnd1->size_;
      if (s1size < GetPrimTypeBitSize(prmtype)) {
        SelectSub(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
      } else {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickSubInsn(prmtype, true), resopnd, opnd0, opnd1));
      }
    } else {
      CG_ASSERT(false, "NYI SelectSub");
    }
  } else {  // both operands are register
    if (opnd0->size_ < GetPrimTypeBitSize(prmtype)) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    if (opnd1->size_ < GetPrimTypeBitSize(prmtype)) {
      opnd1 = SelectCopy(opnd1, prmtype);
    }
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickSubInsn(prmtype, false), resopnd, opnd0, opnd1));
  }
}

Operand *X86CGFunc::SelectSub(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectSub(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

Operand *X86CGFunc::SelectMpy(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  PrimType prmtype = node->primType;
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    Operand *newopnd0 = SelectCopy(opnd0, prmtype);
    return SelectMpy(node, newopnd0, opnd1);
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    return SelectMpy(node, opnd1, opnd0);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
      if (immopnd1->IsInRange(4, true)) {
        uint32 size = GetPrimTypeSize(node->primType);
        MOperator mop = (size == 8 ? MOP_imul64ri : MOP_imul32ri);
        uint32 v_reg_no = New_V_Reg(kRegTyInt, size);
        RegOperand *resultopd = CreateVirtualRegisterOperand(v_reg_no);
        curbb->AppendInsn(static_cast<X86CG *>(cg)->BuildInstruction<X86Insn>(mop, resultopd, opnd0, opnd1));
        return resultopd;
      } else {
        Operand *newopnd1 = SelectCopy(opnd1, prmtype);
        return SelectMpy(node, opnd0, newopnd1);
      }
    } else if (opnd1ty == Operand::Opd_Mem) {
      uint32 size = GetPrimTypeSize(node->primType);
      MOperator mop = PickMpyInsn(node->primType, true);
      RegType regty = GetRegTyFromPrimTy(node->primType);
      uint32 v_reg_no = New_V_Reg(regty, size);
      RegOperand *resultopd = CreateVirtualRegisterOperand(v_reg_no);
      curbb->AppendInsn(static_cast<X86CG *>(cg)->BuildInstruction<X86Insn>(mop, resultopd, opnd0, opnd1));
      return resultopd;
    } else {
      CG_ASSERT(false, "NYI SelectMpy");
    }
  } else {
    uint32 size = GetPrimTypeSize(node->primType);
    MOperator mop = PickMpyInsn(node->primType, false);
    RegType regty = GetRegTyFromPrimTy(node->primType);
    uint32 v_reg_no = New_V_Reg(regty, size);
    RegOperand *resultopd = CreateVirtualRegisterOperand(v_reg_no);
    curbb->AppendInsn(static_cast<X86CG *>(cg)->BuildInstruction<X86Insn>(mop, resultopd, opnd0, opnd1));
    return resultopd;
  }
  return nullptr;
}

void X86CGFunc::SelectDiv(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  bool is64bits = GetPrimTypeBitSize(prmtype) == 64;
  if (IsPrimitiveFloat(prmtype)) {
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    if (opnd1ty == Operand::Opd_Register) {
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_divsdrr : MOP_divssrr, resopnd, opnd0, opnd1));
    } else if (opnd1ty == Operand::Opd_Mem) {
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_divsdrm : MOP_divssrm, resopnd, opnd0, opnd1));
    } else {
      opnd1 = SelectCopy(opnd1, prmtype);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_divsdrr : MOP_divssrr, resopnd, opnd0, opnd1));
    }
  } else if (IsPrimitiveInteger(prmtype)) {
    Operand *rdx = CreateOpndOfType(prmtype);  // high end of dividend
    Operand *rax = CreateOpndOfType(prmtype);  // low end of dividend
    bool issigned = IsSignedInteger(prmtype);
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    SelectCopy(rax, opnd0, prmtype);
    if (issigned) {
      SelectShift(rdx, opnd0, CreateImmOperand(PTY_u8, is64bits ? 63 : 31), ShiftAright, prmtype);
    } else {
      curbb->AppendInsn(
        cg->BuildInstruction<X86Insn>(is64bits ? MOP_ldc64 : MOP_ldc32, rdx, CreateImmOperand(PTY_u8, 0)));
    }
    MOperator divop = MOP_undef;
    if (opnd1ty == Operand::Opd_Mem) {
      divop = issigned ? (is64bits ? MOP_idiv64m : MOP_idiv32m) : (is64bits ? MOP_div64m : MOP_div32m);
    } else {
      if (opnd1ty != Operand::Opd_Register) {
        opnd1 = SelectCopy(opnd1, prmtype);
      }
      divop = issigned ? (is64bits ? MOP_idiv64r : MOP_idiv32r) : (is64bits ? MOP_div64r : MOP_div32r);
    }
    Operand *res_rdx = CreateOpndOfType(prmtype);
    Operand *res_rax = CreateOpndOfType(prmtype);
    // Operand *res_rax = GetOrCreateRegOpnd(RAX, is64bits ? 64 : 32, kRegTyInt);
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(divop, res_rax, res_rdx, rdx, rax, opnd1));
    SelectCopy(resopnd, res_rax, prmtype);
  } else {
    CG_ASSERT(false, "div NIY");
  }
}

Operand *X86CGFunc::SelectDiv(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectDiv(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void X86CGFunc::SelectRem(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  bool is64bits = GetPrimTypeBitSize(prmtype) == 64;
  Operand *rdx = CreateOpndOfType(prmtype);  // high end of dividend
  Operand *rax = CreateOpndOfType(prmtype);  // low end of dividend
  bool issigned = IsSignedInteger(prmtype);
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, prmtype);
  }
  SelectCopy(rax, opnd0, prmtype);
  if (issigned) {
    SelectShift(rdx, opnd0, CreateImmOperand(PTY_u8, is64bits ? 63 : 31), ShiftAright, prmtype);
  } else {
    curbb->AppendInsn(
      cg->BuildInstruction<X86Insn>(is64bits ? MOP_ldc64 : MOP_ldc32, rdx, CreateImmOperand(PTY_u8, 0)));
  }
  MOperator divop = MOP_undef;
  if (opnd1ty == Operand::Opd_Mem) {
    divop = issigned ? (is64bits ? MOP_idiv64m : MOP_idiv32m) : (is64bits ? MOP_div64m : MOP_div32m);
  } else {
    if (opnd1ty != Operand::Opd_Register) {
      opnd1 = SelectCopy(opnd1, prmtype);
    }
    divop = issigned ? (is64bits ? MOP_idiv64r : MOP_idiv32r) : (is64bits ? MOP_div64r : MOP_div32r);
  }
  Operand *res_rax = CreateOpndOfType(prmtype);
  Operand *res_rdx = CreateOpndOfType(prmtype);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(divop, res_rax, res_rdx, rdx, rax, opnd1));
  SelectCopy(resopnd, res_rdx, prmtype);
}

Operand *X86CGFunc::SelectRem(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  PrimType prmtype = (is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32));  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectRem(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void X86CGFunc::SelectTest(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType dtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  bool is64bits = GetPrimTypeBitSize(dtype) == 64;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, dtype);
  }
  if (opnd1ty == Operand::Opd_Immediate) {
    ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
    int64 val = immopnd1->GetValue();
    if (val == 0 && opnd0 == resopnd) {
      return;
    }
    if (immopnd1->IsInRange(4, true)) {
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_test64ri : MOP_test32ri, resopnd, opnd0, opnd1));
    } else if (!is64bits) {
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_test32ri, resopnd, opnd0, immopnd1));
    } else {
      Operand *newopnd1 = SelectCopy(opnd1, dtype);
      SelectTest(resopnd, opnd0, newopnd1, dtype);
    }
  } else if (opnd1ty == Operand::Opd_Mem) {
    if (opnd1->size_ < GetPrimTypeBitSize(dtype)) {
      SelectTest(resopnd, opnd0, SelectCopy(opnd1, dtype), dtype);
    } else {
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_test64rm : MOP_test32rm, resopnd, opnd0, opnd1));
    }
  } else {
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_test64rr : MOP_test32rr, resopnd, opnd0, opnd1));
  }
}

Operand *X86CGFunc::SelectLand(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  bool is64bits = (GetPrimTypeBitSize(node->primType) == 64);
  PrimType prmtype = is64bits ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                              : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  Operand *ccopnd = CreateRflagOpnd();
  Operand *opndres1 = SelectCopy(CreateImmOperand(prmtype, 1), prmtype);
  SelectCopy(resopnd, CreateImmOperand(prmtype, 0), prmtype);
  SelectTest(ccopnd, opnd0, opnd1, prmtype);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_cmovne64 : MOP_cmovne32, resopnd, opndres1, ccopnd));
  curbb->lastinsn->SetCondDef();
  return resopnd;
}

Operand *X86CGFunc::SelectLor(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  bool is64bits = (GetPrimTypeBitSize(node->primType) == 64);
  PrimType prmtype = is64bits ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                              : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  Operand *ccopnd = CreateRflagOpnd();
  Operand *opndres1 = SelectCopy(CreateImmOperand(prmtype, 1), prmtype);
  Operand *oropnd = CreateOpndOfType(prmtype);
  SelectBior(oropnd, opnd0, opnd1, prmtype);
  SelectCopy(resopnd, CreateImmOperand(prmtype, 0), prmtype);
  SelectTest(ccopnd, oropnd, oropnd, prmtype);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_cmove64 : MOP_cmove32, resopnd, opndres1, ccopnd));
  curbb->lastinsn->SetCondDef();
  return resopnd;
}

void X86CGFunc::SelectCmp(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register) {
    Operand *newopnd0 = SelectCopy(opnd0, prmtype);
    SelectCmp(resopnd, newopnd0, opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd0->size_ < GetPrimTypeBitSize(prmtype)) {
      SelectCmp(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
      return;
    }
    if (opnd1ty == Operand::Opd_Immediate) {
      ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
      if (immopnd1->IsInRange(4, true)) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_cmp64ri : MOP_cmp32ri, resopnd, opnd0, opnd1));
      } else if (!is64bits) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_cmp32ri, resopnd, opnd0, immopnd1));
      } else {
        Operand *newopnd1 = SelectCopy(opnd1, prmtype);
        SelectCmp(resopnd, opnd0, newopnd1, prmtype);
      }
    } else if (opnd1ty == Operand::Opd_Mem) {
      if (opnd1->size_ < GetPrimTypeBitSize(prmtype)) {
        SelectCmp(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
      } else {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_cmp64rm : MOP_cmp32rm, resopnd, opnd0, opnd1));
      }
    } else {
      CG_ASSERT(false, "NYI SelectAdd");
    }
  } else {
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_cmp64rr : MOP_cmp32rr, resopnd, opnd0, opnd1));
  }
}

Operand *X86CGFunc::SelectCmpOp(CompareNode *node, Operand *opnd0, Operand *opnd1) {
  bool is64bits = (GetPrimTypeBitSize(node->primType) == 64);
  bool issigned = IsSignedInteger(node->primType);
  PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  Operand *cmpresopnd = CreateRflagOpnd();
  Operand *v0opnd = CreateImmOperand(prmtype, 0);
  Operand *v1opnd = CreateImmOperand(prmtype, 1);
  Operand *v1resopnd = CreateOpndOfType(prmtype);
  resopnd = SelectCopy(v0opnd, prmtype);
  v1resopnd = SelectCopy(v1opnd, prmtype);
  SelectCmp(cmpresopnd, opnd0, opnd1, prmtype);
  Opcode opcode = node->op;
  MOperator mop = PickCmovInsn(is64bits, issigned, opcode);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, resopnd, v1resopnd, cmpresopnd));
  curbb->lastinsn->SetCondDef();
  return resopnd;
}

Operand *X86CGFunc::SelectBand(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType prmtype = (GetPrimTypeBitSize(node->primType) == 64)
                       ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                       : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectBand(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void X86CGFunc::SelectBand(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    Operand *newopnd0 = SelectCopy(opnd0, prmtype);
    SelectBand(resopnd, newopnd0, opnd1, prmtype);
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBand(resopnd, opnd1, opnd0, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd0->size_ < GetPrimTypeBitSize(prmtype)) {
      SelectBand(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
      return;
    }
    if (opnd1ty == Operand::Opd_Immediate) {
      ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
      int64 val = immopnd1->GetValue();
      if (val == -1 || (!is64bits && (val << 32 >> 32) == -1)) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_ldc64 : MOP_ldc32, resopnd, opnd0));
        return;
      }
      if (val == 0xff) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_movzbq : MOP_movzbl, resopnd, opnd0));
        return;
      }
      if (val == 0xffff) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_movzwq : MOP_movzwl, resopnd, opnd0));
        return;
      }

      if (immopnd1->IsInRange(4, true)) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_and64ri : MOP_and32ri, resopnd, opnd0, opnd1));
      } else if (!is64bits) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_and32ri, resopnd, opnd0, immopnd1));
      } else {
        Operand *newopnd1 = SelectCopy(opnd1, prmtype);
        SelectBand(resopnd, opnd0, newopnd1, prmtype);
      }
    } else if (opnd1ty == Operand::Opd_Mem) {
      if (opnd1->size_ < GetPrimTypeBitSize(prmtype)) {
        SelectBand(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
      } else {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_and64rm : MOP_and32rm, resopnd, opnd0, opnd1));
      }
    } else {
      CG_ASSERT(false, "NYI SelectAdd");
    }

  } else {
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_and64rr : MOP_and32rr, resopnd, opnd0, opnd1));
  }
}

Operand *X86CGFunc::SelectBior(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType prmtype = (GetPrimTypeBitSize(node->primType) == 64)
                       ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                       : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectBxor(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void X86CGFunc::SelectBior(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    Operand *newopnd0 = SelectCopy(opnd0, prmtype);
    SelectBxor(resopnd, newopnd0, opnd1, prmtype);
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBxor(resopnd, opnd1, opnd0, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd0->size_ < GetPrimTypeBitSize(prmtype)) {
      SelectBxor(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
      return;
    }
    if (opnd1ty == Operand::Opd_Immediate) {
      ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
      int64 val = immopnd1->GetValue();
      if (val == 0 && opnd0 == resopnd) {
        return;
      }
      if (immopnd1->IsInRange(4, true)) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_or64ri : MOP_or32ri, resopnd, opnd0, opnd1));
      } else if (!is64bits) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_or32ri, resopnd, opnd0, immopnd1));
      } else {
        Operand *newopnd1 = SelectCopy(opnd1, prmtype);
        SelectBxor(resopnd, opnd0, newopnd1, prmtype);
      }
    } else if (opnd1ty == Operand::Opd_Mem) {
      if (opnd1->size_ < GetPrimTypeBitSize(prmtype)) {
        SelectBxor(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
      } else {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_or64rm : MOP_or32rm, resopnd, opnd0, opnd1));
      }
    } else {
      CG_ASSERT(false, "NYI SelectAdd");
    }
  } else {
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_or64rr : MOP_or32rr, resopnd, opnd0, opnd1));
  }
}

Operand *X86CGFunc::SelectMin(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType prmtype = (GetPrimTypeBitSize(node->primType) == 64)
                       ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                       : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectMin(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void X86CGFunc::SelectMin(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (IsPrimitiveFloat(prmtype)) {
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    if (opnd1ty == Operand::Opd_Register) {
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_minsdrr : MOP_minssrr, resopnd, opnd0, opnd1));
    } else if (opnd1ty == Operand::Opd_Mem) {
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_minsdrm : MOP_minssrm, resopnd, opnd0, opnd1));
    } else {
      opnd1 = SelectCopy(opnd1, prmtype);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_minsdrr : MOP_minssrr, resopnd, opnd0, opnd1));
    }
  } else {  // integer type
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    Operand *rflag = CreateRflagOpnd();
    SelectCmp(rflag, opnd0, opnd1, prmtype);
    SelectCopy(resopnd, opnd1, prmtype);
    bool issigned = IsSignedInteger(prmtype);
    MOperator cmovop = is64bits ? (issigned ? MOP_cmovle64 : MOP_cmovbe64) : (issigned ? MOP_cmovle32 : MOP_cmovbe32);
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(cmovop, resopnd, opnd0, rflag));
    curbb->lastinsn->SetCondDef();
  }
}

Operand *X86CGFunc::SelectMax(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType prmtype = (GetPrimTypeBitSize(node->primType) == 64)
                       ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                       : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectMax(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void X86CGFunc::SelectMax(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (IsPrimitiveFloat(prmtype)) {
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    if (opnd1ty == Operand::Opd_Register) {
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_maxsdrr : MOP_maxssrr, resopnd, opnd0, opnd1));
    } else if (opnd1ty == Operand::Opd_Mem) {
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_maxsdrm : MOP_maxssrm, resopnd, opnd0, opnd1));
    } else {
      opnd1 = SelectCopy(opnd1, prmtype);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_maxsdrr : MOP_maxssrr, resopnd, opnd0, opnd1));
    }
  } else {  // integer type
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    Operand *rflag = CreateRflagOpnd();
    SelectCmp(rflag, opnd0, opnd1, prmtype);
    SelectCopy(resopnd, opnd1, prmtype);
    bool issigned = IsSignedInteger(prmtype);
    MOperator cmovop = is64bits ? (issigned ? MOP_cmovge64 : MOP_cmovae64) : (issigned ? MOP_cmovge32 : MOP_cmovae32);
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(cmovop, resopnd, opnd0, rflag));
    curbb->lastinsn->SetCondDef();
  }
}

Operand *X86CGFunc::SelectBxor(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType prmtype = (GetPrimTypeBitSize(node->primType) == 64)
                       ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                       : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectBxor(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void X86CGFunc::SelectBxor(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    Operand *newopnd0 = SelectCopy(opnd0, prmtype);
    SelectBxor(resopnd, newopnd0, opnd1, prmtype);
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBxor(resopnd, opnd1, opnd0, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd0->size_ < GetPrimTypeBitSize(prmtype)) {
      SelectBxor(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
      return;
    }
    if (opnd1ty == Operand::Opd_Immediate) {
      ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
      int64 val = immopnd1->GetValue();
      if (val == 0 && opnd0 == resopnd) {
        return;
      }
      if (immopnd1->IsInRange(4, true)) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_xor64ri : MOP_xor32ri, resopnd, opnd0, opnd1));
      } else if (!is64bits) {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_xor32ri, resopnd, opnd0, immopnd1));
      } else {
        Operand *newopnd1 = SelectCopy(opnd1, prmtype);
        SelectBxor(resopnd, opnd0, newopnd1, prmtype);
      }
    } else if (opnd1ty == Operand::Opd_Mem) {
      if (opnd1->size_ < GetPrimTypeBitSize(prmtype)) {
        SelectBxor(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
      } else {
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_xor64rm : MOP_xor32rm, resopnd, opnd0, opnd1));
      }
    } else {
      CG_ASSERT(false, "NYI SelectAdd");
    }
  } else {
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_xor64rr : MOP_xor32rr, resopnd, opnd0, opnd1));
  }
}

void X86CGFunc::SelectShift(Operand *resopnd, Operand *opnd0, Operand *opnd1, SHIFTDIRECTION direct, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, prmtype);
  }
  MOperator mop;
  if (opnd1ty == Operand::Opd_Immediate) {
    ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
    const int64 val = immopnd1->GetValue();
    const uint8 shiftamt = is64bits ? 63 : 31;
    CG_ASSERT(shiftamt >= val, "shift error oversize");
    switch (direct) {
      case ShiftLeft:
        if (val == 1) {
          SelectAdd(resopnd, opnd0, opnd0, prmtype);
          return;
        }
        mop = is64bits ? MOP_shli64 : MOP_shli32;
        break;
      case ShiftAright:
        mop = is64bits ? MOP_sari64 : MOP_sari32;
        break;
      case ShiftLright:
        mop = is64bits ? MOP_shri64 : MOP_shri32;
        break;
        opnd1 = GetOrCreateImmOperand(val & shiftamt, 32, false);
    }
  } else if (opnd1ty != Operand::Opd_Register) {
    SelectShift(resopnd, opnd0, SelectCopy(opnd1, prmtype), direct, prmtype);
    return;
  } else {
    switch (direct) {
      case ShiftLeft:
        mop = is64bits ? MOP_shl64 : MOP_shl32;
        break;
      case ShiftAright:
        mop = is64bits ? MOP_sar64 : MOP_sar32;
        break;
      case ShiftLright:
        mop = is64bits ? MOP_shr64 : MOP_shr32;
        break;
    }
  }

  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, resopnd, opnd0, opnd1));
}

//  abs %esi
//      ||
//      ||
//      ||
//      \/
//  movl/q  %edi,%ebx
//  negl/q  %ebx
//  cmovs  %edi,%ebx
Operand *X86CGFunc::SelectAbs(UnaryNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  if (IsPrimitiveFloat(dtyp)) {
    CG_ASSERT(false, "float abs will generate an intrinsic call(NYI)");
  } else {
    Operand::OperandType opnd0ty = opnd0->op_kind_;
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    bool issigned = IsSignedInteger(dtyp);
    PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    Operand *resopd = CreateOpndOfType(prmtype);
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_neg64r : MOP_neg32r, resopd, opnd0));
    curbb->AppendInsn(
      cg->BuildInstruction<X86Insn>(is64bits ? MOP_cmovs64 : MOP_cmovs32, resopd, opnd0, CreateRflagOpnd()));
    curbb->lastinsn->SetCondDef();
    return resopd;
  }
  return nullptr;
}

//  bnot %esi
//      ||
//      ||
//      ||
//      \/
//  notl  %edi
Operand *X86CGFunc::SelectBnot(UnaryNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  if (IsPrimitiveInteger(dtyp)) {
    Operand::OperandType opnd0ty = opnd0->op_kind_;
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    bool issigned = IsSignedInteger(dtyp);
    PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    Operand *resopd = CreateOpndOfType(prmtype);
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_not64r : MOP_not32r, resopd, opnd0));
    return resopd;
  } else {
    CG_ASSERT(false, "bnot expect integer or NYI");
  }
  return nullptr;
}

Operand *X86CGFunc::SelectExtractbits(ExtractbitsNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  Operand *resopnd = CreateOpndOfType(dtyp);
  bool issigned = IsSignedInteger(dtyp);
  uint32 size = GetPrimTypeBitSize(dtyp);
  uint8 boffset = node->boffset;
  uint8 bsize = node->bsize;
  if (boffset == 0 && bsize <= 16 && !issigned) {
    SelectBand(resopnd, opnd0, GetOrCreateImmOperand((static_cast<uint64>(1) << bsize) - 1, 4, false), dtyp);
    return resopnd;
  }
  Operand *tresopnd = CreateOpndOfType(dtyp);
  SelectShift(tresopnd, opnd0, GetOrCreateImmOperand(size - boffset - bsize, 8, false), ShiftLeft, dtyp);
  SelectShift(resopnd, tresopnd, GetOrCreateImmOperand(size - bsize, 8, false), issigned ? ShiftAright : ShiftLright,
              dtyp);
  return resopnd;
}

void X86CGFunc::SelectDepositbits(Operand *resopnd, Operand *opnd0, Operand *opnd1, uint32 boffset, uint32 bsize,
                                  PrimType rtyp, PrimType dtyp) {
  Operand *t1opnd = CreateOpndOfType(rtyp);
  Operand *t2opnd = CreateOpndOfType(rtyp);
  Operand *t3opnd = CreateOpndOfType(rtyp);
  Operand *t4opnd = CreateOpndOfType(rtyp);  // make sure it's SSA format
  uint32 rtypbitsize = GetPrimTypeBitSize(rtyp);
  SelectShift(t1opnd, opnd0, GetOrCreateImmOperand(boffset, 8, false), ShiftLright, rtyp);
  SelectBxor(t2opnd, t1opnd, opnd1, rtyp);
  SelectShift(t3opnd, t2opnd, GetOrCreateImmOperand(rtypbitsize - bsize, 8, false), ShiftLeft, rtyp);
  SelectShift(t4opnd, t3opnd, GetOrCreateImmOperand(rtypbitsize - bsize - boffset, 8, false), ShiftLright, rtyp);
  SelectBxor(resopnd, opnd0, t4opnd, rtyp);
}

Operand *X86CGFunc::SelectDepositbits(DepositbitsNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtyp = node->primType;
  Operand *resopnd = CreateOpndOfType(dtyp);
  SelectDepositbits(resopnd, opnd0, opnd1, node->boffset, node->bsize, dtyp, dtyp);
  return resopnd;
}

//  lnot %ebx
//      ||
//      ||
//      ||
//      \/
//  testl/q  rflag opn0, opn0
//  movl dest, 0
//  sete dest, rflag
Operand *X86CGFunc::SelectLnot(UnaryNode *node, Operand *opnd0) {
  PrimType dtype = node->primType;
  Operand *resopnd = CreateOpndOfType(dtype);
  Operand *rcc = CreateRflagOpnd();
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, dtype);
  }
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(opnd0->size_ == 64 ? MOP_test64rr : MOP_test32rr, rcc, opnd0, opnd0));
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_ldc32, resopnd, GetOrCreateImmOperand(0, 32, false)));
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_sete, resopnd, rcc));
  return resopnd;
}

Operand *X86CGFunc::SelectNeg(UnaryNode *node, Operand *opnd0) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  if (opnd0->op_kind_ != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, prmtype);
  }
  if (isfloat) {
    Operand *maskopnd = nullptr;
    MOperator mop;
    MOperator mop_ld;
    if (is64bits) {
      union {
        int64 ival;
        double fval;
      } fvalue;
      fvalue.ival = 0x8000000000000000ULL;
      maskopnd = CreateDoubleImmOperand(fvalue.fval);
      mop = MOP_xorpd64rr;
      mop_ld = MOP_ldsd;
    } else {
      union {
        int32 ival;
        float fval;
      } fvalue;
      fvalue.ival = 0x80000000;
      maskopnd = CreateFloatImmOperand(fvalue.ival);
      mop = MOP_xorps32rr;
      mop_ld = MOP_ldss;
    }
    Operand *ldres = CreateOpndOfType(prmtype);
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop_ld, ldres, maskopnd));
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, resopnd, opnd0, ldres));
  } else {
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_neg64r : MOP_neg32r, resopnd, opnd0));
  }
  return resopnd;
}

//  recip %esi
//      ||
//      ||
//      ||
//      \/
//  divss/divsd  %edi
Operand *X86CGFunc::SelectRecip(UnaryNode *node, Operand *opnd0) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  PrimType primType = node->primType;
  CG_ASSERT(IsPrimitiveFloat(primType), "recip wrong type should be float");
  bool isdouble = GetPrimTypeBitSize(primType) == 64;
  if (opnd0ty != Operand::Opd_Register && opnd0ty != Operand::Opd_Mem) {
    opnd0 = SelectCopy(opnd0, primType);
  }
  Operand *f1opd = CreateOpndOfType(primType);
  Operand *memopnd = isdouble ? CreateDoubleImmOperand(1.0) : CreateFloatImmOperand(1.0f);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(isdouble ? MOP_ldsd : MOP_ldss, f1opd, memopnd));
  Operand *resopnd = CreateOpndOfType(primType);
  SelectDiv(resopnd, f1opd, opnd0, primType);
  return resopnd;
}

Operand *X86CGFunc::SelectSqrt(UnaryNode *node, Operand *opnd0) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  PrimType primType = node->primType;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, primType);
  }

  Operand *resopnd = CreateOpndOfType(primType);
  curbb->AppendInsn(
    cg->BuildInstruction<X86Insn>(GetPrimTypeBitSize(primType) == 64 ? MOP_sqrtsd : MOP_sqrtss, resopnd, opnd0));
  return resopnd;
}

void X86CGFunc::SelectFloat2Int(Operand *resopnd, Operand *opnd0, PrimType itype, PrimType ftype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  switch (itype) {
    case PTY_i32: {
      bool ismem = (opnd0ty == Operand::Opd_Mem);
      if (opnd0ty != Operand::Opd_Register && !ismem) {
        opnd0 = SelectCopy(opnd0, ftype);
      }
      MOperator mop = is64bits ? (ismem ? MOP_cvttsd2sim : MOP_cvttsd2sir) : (ismem ? MOP_cvttss2sim : MOP_cvttss2sir);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, resopnd, opnd0));
      return;
    }
    case PTY_u32: {
      if (opnd0ty != Operand::Opd_Register) {
        opnd0 = SelectCopy(opnd0, ftype);
      }
      Operand *intreg = CreateOpndOfType(PTY_i64);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_cvttsd2sir : MOP_cvttss2si64r, intreg, opnd0));
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_movzlq, resopnd, intreg));  // movzql actually zero extend,
      return;
    }
    case PTY_i64: {
      bool ismem = (opnd0ty == Operand::Opd_Mem);
      if (opnd0ty != Operand::Opd_Register && opnd0ty != Operand::Opd_Mem) {
        opnd0 = SelectCopy(opnd0, ftype);
      }
      MOperator mop =
        is64bits ? (ismem ? MOP_cvttsd2si64m : MOP_cvttsd2si64r) : (ismem ? MOP_cvttss2si64m : MOP_cvttss2si64r);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, resopnd, opnd0));
      return;
    }
    case PTY_u64: {
      union {
        int32 ival;
        float fval;
      } fvalue;
      union {
        int64 ival;
        double dval;
      } dvalue;
      fvalue.ival = 1593835520;           // # float 9.22337203E+18
      dvalue.ival = 4890909195324358656;  // # double 9.2233720368547758E+18
      Operand *fconstopnd = is64bits ? CreateDoubleImmOperand(dvalue.dval) : CreateFloatImmOperand(fvalue.fval);
      Operand *topnd1 = CreateOpndOfType(is64bits ? PTY_f64 : PTY_f32);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_ldsd : MOP_ldss, topnd1, fconstopnd));
      if (opnd0ty != Operand::Opd_Register) {
        opnd0 = SelectCopy(opnd0, ftype);
      }
      Operand *subresopnd = CreateOpndOfType(is64bits ? PTY_f64 : PTY_f32);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_subsdrr : MOP_subssrr, subresopnd, opnd0, topnd1));
      Operand *ss2siresopnd = CreateOpndOfType(PTY_i64);
      curbb->AppendInsn(
        cg->BuildInstruction<X86Insn>(is64bits ? MOP_cvttsd2si64r : MOP_cvttss2si64r, ss2siresopnd, subresopnd));
      Operand *movresopnd = CreateOpndOfType(PTY_i64);
      curbb->AppendInsn(
        cg->BuildInstruction<X86Insn>(MOP_ldc64abs, movresopnd, GetOrCreateImmOperand(0x8000000000000000, 64, true)));
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_xor64rr, resopnd, ss2siresopnd, movresopnd));
      Operand *ss2siresopnd2 = CreateOpndOfType(PTY_i64);
      curbb->AppendInsn(
        cg->BuildInstruction<X86Insn>(is64bits ? MOP_cvttsd2si64r : MOP_cvttss2si64r, ss2siresopnd2, opnd0));
      Operand *rflag = CreateRflagOpnd();
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_ucomisdrr : MOP_ucomissrr, rflag, opnd0, topnd1));
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_cmovb64, resopnd, ss2siresopnd2, rflag));
      curbb->lastinsn->SetCondDef();
      return;
    }
    default:
      CG_ASSERT(false, "unexpected type");
  }
}

void X86CGFunc::SelectInt2Float(Operand *resopnd, Operand *opnd0, PrimType itype, PrimType ftype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  switch (itype) {
    case PTY_i32: {
      bool ismem = (opnd0ty == Operand::Opd_Mem);
      if (opnd0ty != Operand::Opd_Register && !ismem) {
        opnd0 = SelectCopy(opnd0, itype);
      }
      MOperator mop = is64bits ? (ismem ? MOP_cvtsi2sdm : MOP_cvtsi2sdr) : (ismem ? MOP_cvtsi2ssm : MOP_cvtsi2ssr);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, resopnd, opnd0));
      return;
    }
    case PTY_u32: {
      opnd0 = SelectCopy(opnd0, PTY_u64);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_cvtsi2sdqr : MOP_cvtsi2ssqr, resopnd, opnd0));
      return;
    }
    case PTY_i64: {
      bool ismem = (opnd0ty == Operand::Opd_Mem);
      if (opnd0ty != Operand::Opd_Register && !ismem) {
        opnd0 = SelectCopy(opnd0, itype);
      }
      MOperator mop = is64bits ? (ismem ? MOP_cvtsi2sdqm : MOP_cvtsi2sdqr) : (ismem ? MOP_cvtsi2ssqm : MOP_cvtsi2ssqr);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, resopnd, opnd0));
      return;
    }
    case PTY_u64: {
      if (ftype == PTY_f32 || ftype == PTY_f64) {
        bool isdouble = ftype == PTY_f64;
        if (opnd0ty != Operand::Opd_Register) {
          opnd0 = SelectCopy(opnd0, itype);
        }
        Operand *opnda = CreateOpndOfType(PTY_u64);
        Operand *opndc = CreateOpndOfType(PTY_u64);
        Operand *opndor = CreateOpndOfType(PTY_u64);
        Operand *opndf1 = CreateOpndOfType(isdouble ? PTY_f64 : PTY_f32);
        Operand *opndcvtf1 = CreateOpndOfType(isdouble ? PTY_f64 : PTY_f32);
        Operand *opndimm1 = CreateImmOperand(PTY_u8, 1);
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_shri64, opnda, opnd0, opndimm1));
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_and64ri, opndc, opnd0, opndimm1));
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_or64rr, opndor, opnda, opndc));
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(isdouble ? MOP_cvtsi2sdqr : MOP_cvtsi2ssqr, opndcvtf1, opndor));
        curbb->AppendInsn(
          cg->BuildInstruction<X86Insn>(isdouble ? MOP_addsdrr : MOP_addssrr, resopnd, opndcvtf1, opndcvtf1));
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(isdouble ? MOP_cvtsi2sdqr : MOP_cvtsi2ssqr, opndf1, opnd0));
        Operand *rflag = CreateRflagOpnd();
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_test64rr, rflag, opnd0, opnd0));
        // start 2 bbs
        BB *thenbb = CreateNewBB();
        curbb->InsertAfter(thenbb);
        BB *elsebb = CreateNewBB();
        // if a BB has a valid label idx, the emitter emits a label.
        elsebb->AddLabel(CreateLabel());
        thenbb->InsertAfter(elsebb);
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_js, rflag, GetOrCreateLabelOperand(elsebb->labidx)));
        Insn *movssinsn = cg->BuildInstruction<X86Insn>(isdouble ? MOP_movsd : MOP_movss, resopnd, opndf1);
        movssinsn->SetCondDef();
        thenbb->AppendInsn(movssinsn);
        curbb = elsebb;
        //   MOP_js ... label_a
        //   MOP_movsd/ss ...    # then
        // label_a:              # else
      } else {
        CG_ASSERT(false, "uint64 to non float/double");
      }
      break;
    }
    default:
      CG_ASSERT(false, "unexpected integer type i2f");
  }
}

Operand *X86CGFunc::SelectCeil(TypeCvtNode *node, Operand *opnd0) {
  PrimType ftype = node->fromptyp;
  bool isdouble = GetPrimTypeBitSize(ftype) == 64;
  bool is64bits = (GetPrimTypeBitSize(node->primType) == 64);
  PrimType itype = (is64bits) ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                              : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, ftype);
  }
  Operand *resopnd = CreateOpndOfType(itype);
  SelectFloat2Int(resopnd, opnd0, itype, ftype);
  Operand *addresopnd = CreateOpndOfType(itype);
  SelectAdd(addresopnd, resopnd, GetOrCreateImmOperand(1, 32, false), itype);
  Operand *zeroopnd = CreateOpndOfType(ftype);
  Operand *topnd = CreateOpndOfType(ftype);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(isdouble ? MOP_xorpd64rr : MOP_xorps32rr, zeroopnd, topnd, topnd));
  Operand *rflag = CreateRflagOpnd();
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(isdouble ? MOP_comisdrr : MOP_comissrr, rflag, opnd0, zeroopnd));
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_cmovbe64 : MOP_cmovbe32, addresopnd, resopnd,
                                                  rflag));  // Condition define
  curbb->lastinsn->SetCondDef();
  Operand *i2fresopnd = CreateOpndOfType(ftype);
  SelectInt2Float(i2fresopnd, resopnd, itype, ftype);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(isdouble ? MOP_comisdrr : MOP_comissrr, rflag, opnd0, i2fresopnd));
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_cmovne64 : MOP_cmovne32, resopnd, addresopnd, rflag));
  curbb->lastinsn->SetCondDef();
  return resopnd;
}

// float to int floor
Operand *X86CGFunc::SelectFloor(TypeCvtNode *node, Operand *opnd0) {
  PrimType ftype = node->fromptyp;
  bool isdouble = GetPrimTypeBitSize(ftype) == 64;
  bool is64bits = (GetPrimTypeBitSize(node->primType) == 64);
  PrimType itype = (is64bits) ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                              : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, ftype);
  }
  Operand *resopnd = CreateOpndOfType(itype);
  Operand *subopnd = CreateOpndOfType(itype);
  Operand *xoropnd = CreateOpndOfType(ftype);
  Operand *i2fopnd = CreateOpndOfType(ftype);
  Operand *rflag = CreateRflagOpnd();
  SelectFloat2Int(resopnd, opnd0, itype, ftype);
  SelectSub(subopnd, resopnd, CreateImmOperand(PTY_u8, 1), itype);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(isdouble ? MOP_xorpd64rr : MOP_xorps32rr, xoropnd, opnd0, opnd0));
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(isdouble ? MOP_comisdrr : MOP_comissrr, rflag, opnd0, xoropnd));
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(isdouble ? MOP_cmova64 : MOP_cmova32, subopnd, resopnd, rflag));
  curbb->lastinsn->SetCondDef();
  SelectInt2Float(i2fopnd, resopnd, itype, ftype);
  Operand *rflag2 = CreateRflagOpnd();
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(isdouble ? MOP_comisdrr : MOP_comissrr, rflag2, opnd0, i2fopnd));
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(isdouble ? MOP_cmovne64 : MOP_cmovne32, resopnd, subopnd, rflag2));
  curbb->lastinsn->SetCondDef();
  return resopnd;
}

Operand *X86CGFunc::SelectRetype(TypeCvtNode *node, Operand *opnd0) {
  PrimType fromtype = node->fromptyp;
  PrimType totype = node->primType;
  CG_ASSERT(GetPrimTypeSize(fromtype) == GetPrimTypeSize(totype), "retype bit widith doesn' match");
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand *resopnd = CreateOpndOfType(totype);
  if (IsPrimitiveInteger(fromtype) || IsPrimitiveFloat(fromtype)) {
    bool isfromint = IsPrimitiveInteger(fromtype);
    bool is64bits = GetPrimTypeBitSize(fromtype) == 64;
    PrimType itype = isfromint
                       ? ((GetPrimTypeBitSize(fromtype) == 64) ? (IsSignedInteger(fromtype) ? PTY_i64 : PTY_u64)
                                                               : (IsSignedInteger(fromtype) ? PTY_i32 : PTY_u32))
                       : (is64bits ? PTY_f64 : PTY_f32);
    if (!isfromint && opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, itype);
      opnd0ty = Operand::Opd_Register;
    }
    if (opnd0ty == Operand::Opd_Mem) {
      MOperator mop = isfromint ? (is64bits ? MOP_ldi2fq : MOP_ldi2fd) : (is64bits ? MOP_ldf2iq : MOP_ldf2id);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, resopnd, opnd0));
    } else {
      if (opnd0ty != Operand::Opd_Register) {
        opnd0 = SelectCopy(opnd0, itype);
      }
      MOperator mop = isfromint ? (is64bits ? MOP_movi2fq : MOP_movi2fd) : (is64bits ? MOP_movf2iq : MOP_movf2id);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, resopnd, opnd0));
    }
    return resopnd;
  } else {
    CG_ASSERT(false, "NYI retype");
  }
  return nullptr;
}

void X86CGFunc::SelectCvtF2F(Operand *resopnd, Operand *opnd0, PrimType fromty, PrimType toty) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  bool ismem = opnd0ty == Operand::Opd_Mem;
  if (!ismem && opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, fromty);
  }
  MOperator mop = 0;
  switch (toty) {
    case PTY_f32: {
      switch (fromty) {
        case PTY_f64:
          mop = ismem ? MOP_cvtsd2ssm : MOP_cvtsd2ssr;
          break;
        default:
          CG_ASSERT(false, "unexpected cvt from type");
      }
      break;
    }
    case PTY_f64: {
      switch (fromty) {
        case PTY_f32:
          mop = ismem ? MOP_cvtss2sdm : MOP_cvtss2sdr;
          break;
        default:
          CG_ASSERT(false, "unexpected cvt from type");
      }
      break;
    }
    default:
      CG_ASSERT(false, "unexpected cvt to type");
  }
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, resopnd, opnd0));
}

void X86CGFunc::SelectCvtI2I(Operand *resopnd, Operand *opnd0, PrimType fromty, PrimType toty) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (opnd0ty == Operand::Opd_Mem && GetPrimTypeSize(fromty) <= GetPrimTypeSize(toty)) {
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(fromty, toty), resopnd, opnd0));
    return;
  }
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, fromty);
  }
  MOperator mop = 0;
  switch (toty) {
    case PTY_u32:
    case PTY_a32:
    case PTY_i32: {
      switch (fromty) {
        case PTY_i8:
          mop = MOP_movsbl;
          break;
        case PTY_i16:
          mop = MOP_movswl;
          break;
        case PTY_a32:
        case PTY_u32:
        case PTY_i32:
          mop = MOP_mov32;  // noop
          break;
        case PTY_a64:
        case PTY_u64:
        case PTY_i64:
          mop = MOP_movzql;
          break;
        case PTY_u8:
          mop = MOP_movzbl;
          break;
        case PTY_u16:
          mop = MOP_movzwl;
          break;
        default:
          CG_ASSERT(false, "unexpected cvt from type");
      }
      break;
    }
    case PTY_a64:
    case PTY_u64:
    case PTY_i64: {
      switch (fromty) {
        case PTY_i8:
          mop = MOP_movsbq;
          break;
        case PTY_i16:
          mop = MOP_movswq;
          break;
        case PTY_i32:
          mop = MOP_movslq;
          break;
        case PTY_u32:
        case PTY_a32:
          mop = MOP_movzlq;
          break;
        case PTY_a64:
        case PTY_u64:
        case PTY_i64:
          mop = MOP_mov64;  // noop
          break;
        case PTY_u8:
          mop = MOP_movzbq;
          break;
        case PTY_u16:
          mop = MOP_movzwq;
          break;
        default:
          CG_ASSERT(false, "unexpected cvt from type");
      }
      break;
    }
    default:
      CG_ASSERT(false, "unexpected cvt to type");
  }
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, resopnd, opnd0));
}

Operand *X86CGFunc::SelectCvt(TypeCvtNode *node, Operand *opnd0) {
  PrimType fromtype = node->fromptyp;
  PrimType totype = node->primType;
  if (fromtype == totype) {
    return opnd0;  // noop
  }
  Operand *resopnd = CreateOpndOfType(totype);
  if (IsPrimitiveFloat(totype) && IsPrimitiveInteger(fromtype)) {
    PrimType itype = (GetPrimTypeBitSize(fromtype) == 64) ? (IsSignedInteger(fromtype) ? PTY_i64 : PTY_u64)
                                                          : (IsSignedInteger(fromtype) ? PTY_i32 : PTY_u32);
    SelectInt2Float(resopnd, opnd0, itype, totype);
    return resopnd;
  } else if (IsPrimitiveFloat(fromtype) && IsPrimitiveInteger(totype)) {
    SelectFloat2Int(resopnd, opnd0, totype, fromtype);
    return resopnd;
  } else if (IsPrimitiveInteger(fromtype) && IsPrimitiveInteger(totype)) {
    SelectCvtI2I(resopnd, opnd0, fromtype, totype);
    return resopnd;
  } else {  // both are float type
    SelectCvtF2F(resopnd, opnd0, fromtype, totype);
    return resopnd;
  }
}

Operand *X86CGFunc::SelectRound(TypeCvtNode *node, Operand *opnd0) {
  PrimType ftype = node->fromptyp;
  bool is64bits = (GetPrimTypeBitSize(node->primType) == 64);
  PrimType itype = (is64bits) ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                              : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(itype);
  SelectFloat2Int(resopnd, opnd0, itype, ftype);
  return resopnd;
}

Operand *X86CGFunc::SelectTrunc(TypeCvtNode *node, Operand *opnd0) {
  PrimType ftype = node->fromptyp;
  bool is64bits = (GetPrimTypeBitSize(node->primType) == 64);
  PrimType itype = (is64bits) ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                              : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(itype);
  SelectFloat2Int(resopnd, opnd0, itype, ftype);
  return resopnd;
}

void X86CGFunc::SelectSelect(Operand *resopnd, Operand *condopnd, Operand *trueopnd, Operand *falseopnd, PrimType dtyp,
                             PrimType ctyp) {
  Operand::OperandType condty = condopnd->op_kind_;
  Operand::OperandType truety = trueopnd->op_kind_;
  Operand::OperandType falsety = falseopnd->op_kind_;
  if (resopnd == condopnd || condty != Operand::Opd_Register) {
    condopnd = SelectCopy(condopnd, ctyp);
  }

  if (falsety != Operand::Opd_Register) {
    falseopnd = SelectCopy(falseopnd, dtyp);
  }

  if (IsPrimitiveInteger(dtyp)) {
    bool is64bits = GetPrimTypeBitSize(dtyp) == 64;
    if (resopnd == falseopnd) {
      falseopnd = SelectCopy(falseopnd, dtyp);
    }
    if (resopnd != trueopnd) {
      SelectCopy(resopnd, trueopnd, dtyp);
    }
    Operand *rflag = CreateRflagOpnd();
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_test64rr : MOP_test32rr, rflag, condopnd, condopnd));
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_cmove64 : MOP_cmove32, resopnd, falseopnd, rflag));
    curbb->lastinsn->SetCondDef();
  } else if (IsPrimitiveFloat(dtyp)) {
    if (truety != Operand::Opd_Register) {
      trueopnd = SelectCopy(trueopnd, dtyp);
    }
    bool is64bits = GetPrimTypeBitSize(dtyp) == 64;
    bool iscond64bits = GetPrimTypeBitSize(ctyp) == 64;
    Operand *topnd3 = CreateOpndOfType(dtyp);
    Operand *topnd4 = CreateOpndOfType(dtyp);
    Operand *topnd5 = CreateOpndOfType(dtyp);
    PrimType itype = is64bits ? PTY_i64 : PTY_i32;
    Operand *topnd1 = CreateOpndOfType(itype);
    Operand *topnd2 = CreateOpndOfType(itype);
    Operand *immopnd = CreateImmOperand(PTY_u8, is64bits ? 63 : 31);
    // test condition
    Operand *rflag = CreateRflagOpnd();
    Operand *newcondopnd = CreateOpndOfType(ctyp);
    Operand *zeroopnd = CreateZeroOperand(ctyp);
    curbb->AppendInsn(
      cg->BuildInstruction<X86Insn>(iscond64bits ? MOP_ldc64 : MOP_ldc32, newcondopnd, CreateImmOperand(PTY_u8, 1)));
    curbb->AppendInsn(
      cg->BuildInstruction<X86Insn>(iscond64bits ? MOP_test64rr : MOP_test32rr, rflag, condopnd, condopnd));
    curbb->AppendInsn(
      cg->BuildInstruction<X86Insn>(iscond64bits ? MOP_cmove64 : MOP_cmove32, newcondopnd, zeroopnd, rflag));
    curbb->lastinsn->SetCondDef();
    SelectShift(topnd1, newcondopnd, immopnd, ShiftLeft, itype);
    SelectShift(topnd2, topnd1, immopnd, ShiftAright, itype);
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_movi2fq : MOP_movi2fd, topnd3, topnd2));
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_andpdrr : MOP_andpsrr, topnd4, trueopnd, topnd3));
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_andnpdrr : MOP_andnpsrr, topnd5, topnd3, falseopnd));
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(is64bits ? MOP_orpdrr : MOP_orpsrr, resopnd, topnd5, topnd4));
  } else {
    CG_ASSERT(false, "unknown type for select");
  }
}

Operand *X86CGFunc::SelectSelect(TernaryNode *node, Operand *opnd0, Operand *opnd1, Operand *opnd2) {
  PrimType dtyp = node->primType;
  PrimType ctyp = node->Opnd(0)->primType;
  Operand *resopnd = CreateOpndOfType(dtyp);
  SelectSelect(resopnd, opnd0, opnd1, opnd2, dtyp, ctyp);
  return resopnd;
}

void X86CGFunc::SelectRangegoto(RangegotoNode *rangegotonode, Operand *opnd0) {
  SmallCaseVector &switchtable = rangegotonode->rangegototable;
  MIRType *etype = globaltable.GetTypeFromTyIdx((TyIdx)PTY_a64);
  MapleVector<uint32> size_array(funcscope_allocator_->Adapter());
  size_array.push_back(switchtable.size());
  MIRArrayType *arraytype = memPool->New<MIRArrayType>(etype->_ty_idx, size_array);
  MIRAggConst *arrayconst = memPool->New<MIRAggConst>(g->mirModule, arraytype);
  for (uint32 i = 0; i < switchtable.size(); i++) {
    LabelIdx lidx = switchtable[i].second;
    MIRConst *mirconst = memPool->New<MIRLblConst>(lidx, etype);
    arrayconst->const_vec.push_back(mirconst);
  }
  MIRSymbol *lblst = func->symtab->CreateSymbol(SCOPE_LOCAL);
  lblst->storageClass = kScFstatic;
  lblst->sKind = kStConst;
  lblst->SetTyIdx(arraytype->_ty_idx);
  lblst->SetConst(arrayconst);
  std::string lblstr(".LB_");
  MIRSymbol *func_st = globaltable.GetSymbolFromStidx(func->stidx.Idx());
  lblstr.append(func_st->GetName()).append(to_string(labelidx++));
  lblst->SetNameStridx(globaltable.GetOrCreateGstridxFromName(lblstr));
  emitstvec_.push_back(lblst);

  PrimType ityp = rangegotonode->uopnd->primType;
  bool is64bits = GetPrimTypeBitSize(ityp) == 64;
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, ityp);
  }
  Operand *addopnd = CreateOpndOfType(ityp);
  int32 minidx = switchtable[0].first;
  SelectAdd(addopnd, opnd0, CreateImmOperand(PTY_i32, -minidx - rangegotonode->tagoffset), ityp);
  if (!is64bits) {
    addopnd = SelectCopy(addopnd, PTY_u64);
  }
  Operand *jmpopnd = CreateOpndOfType(PTY_a64);
  X86MemOperand *memopnd =
    GetOrCreateMemOpnd(X86MemOperand::Addressing_ISO, GetPrimTypeBitSize(ityp), nullptr,
                       static_cast<RegOperand *>(addopnd),
                       static_cast<Operand *>(GetOrCreateStImmOperand(lblst, 0, 0)),
                       CreateImmOperand(PTY_u64, 8), lblst);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_ld64, jmpopnd, memopnd));
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_ijmpr, jmpopnd));
}

}  // namespace maplebe
