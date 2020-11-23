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

#include "riscv64_cg_func.h"
#include "riscv64_cg.h"
#include "riscv64_rt_support.h"
#include "opcode_info.h"  // mapleir/include/opcode_info.h
#include "cg_assert.h"
#include "special_func.h"
#include <iostream>
#include <typeinfo>

namespace maplebe {

using namespace maple;

#define CLANG  (mirModule.IsCModule())

MOperator Riscv64CGFunc::PickAddInsn(PrimType primtype, bool ismem) {
  CG_ASSERT(false, "NYI PickAddInsn");
  return MOP_undef;
}

MOperator Riscv64CGFunc::PickMpyInsn(PrimType primtype, bool ismem) {
  CG_ASSERT(false, "NYI PickAddInsn");
  return MOP_undef;
}

MOperator Riscv64CGFunc::PickJmpInsn(Opcode brop, Opcode cmpop, bool isfloat, bool issigned, bool isZero) {
  switch (cmpop) {
    default:
      CG_ASSERT(false, "PickJmpInsn error");
    case OP_ne:
      if (isZero) {
        return static_cast<uint32>(brop == OP_brtrue ? MOP_bnez : MOP_beqz);
      } else {
        return static_cast<uint32>(brop == OP_brtrue ? MOP_bne : MOP_beq);
      }
    case OP_eq:
      if (isZero) {
        return static_cast<uint32>(brop == OP_brtrue ? MOP_beqz : MOP_bnez);
      } else {
        return static_cast<uint32>(brop == OP_brtrue ? MOP_beq : MOP_bne);
      }
    case OP_lt:
      if (isZero) {
        return static_cast<uint32>(brop == OP_brtrue ? MOP_bltz
                                        : (isfloat ? MOP_bpl : MOP_bgez));
      } else {
        return static_cast<uint32>(brop == OP_brtrue ? (issigned ? MOP_blt : MOP_blo)
                                        : (isfloat ? MOP_bpl : (issigned ? MOP_bge : MOP_bhs)));
      }
    case OP_le:
      if (isZero) {
        return static_cast<uint32>(brop == OP_brtrue ? MOP_blez
                                        : (isfloat ? MOP_bhi : MOP_bgtz));
      } else {
        return static_cast<uint32>(brop == OP_brtrue ? (issigned ? MOP_ble : MOP_bls)
                                        : (isfloat ? MOP_bhi : (issigned ? MOP_bgt : MOP_bhi)));
      }
    case OP_gt:
      if (isZero) {
        return static_cast<uint32>(brop == OP_brtrue ? (isfloat ? MOP_bhi : MOP_bgtz) : MOP_blez);
      } else {
        return static_cast<uint32>(brop == OP_brtrue ? (isfloat ? MOP_bhi : (issigned ? MOP_bgt : MOP_bhi))
                                        : (issigned ? MOP_ble : MOP_bls));
      }
    case OP_ge:
      if (isZero) {
        return static_cast<uint32>(brop == OP_brtrue ? (isfloat ? MOP_bpl : MOP_bgez) : MOP_bltz);
      } else {
        return static_cast<uint32>(brop == OP_brtrue ? (isfloat ? MOP_bpl : (issigned ? MOP_bge : MOP_bhs))
                                        : (issigned ? MOP_blt : MOP_blo));
      }
  }
}

void Riscv64CGFunc::SelectCondGoto(LabelOperand *targetopnd, Opcode jmpop, Opcode cmpop, Operand *opnd0, Operand *opnd1,
                                   PrimType primType, bool signedCond) {
  CG_ASSERT(targetopnd != nullptr, "no branch target");

  bool isMem = (opnd0->op_kind_ == Operand::Opd_Mem);
  if (opnd0->IsRegister()) {
    opnd0 = SelectZeroSignExt(opnd0, primType);
  } else {
    opnd0 = LoadIntoRegister(opnd0, primType);
  }
#if 0
  if (isMem && !(primType == PTY_u8 || primType == PTY_u16 || primType == PTY_u32 || primType == PTY_u64)) {
    // Riscv operations need to be sign extended.
    MOperator mop = MOP_undef;
    switch (primType) {
    case PTY_u8:
      mop = MOP_xsxtb64;
      break;
    case PTY_u16:
      mop = MOP_xsxth64;
      break;
    case PTY_u32:
      mop = MOP_xsxtw64;
      break;
    default:
      break;
    }
    if (mop != MOP_undef) {
      Operand *dest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, (opnd0->size_ == 32) ? 4 : 8));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, dest, opnd0));
      opnd0 = dest;
    }
  }
#endif

  bool isfloat = IsPrimitiveFloat(primType);
  MOperator branchOp;
  if (isfloat) {
    opnd1 = LoadIntoRegister(opnd1, primType);

    MOperator op;
    RegOperand *fcmpResult;
    branchOp = MOP_beq;
    switch (cmpop) {
    case OP_eq:
      op = (primType == PTY_f32) ? MOP_scmpeqrr : MOP_dcmpeqrr;
      if (jmpop == OP_brtrue) {
        branchOp = MOP_bne;
      }
      break;
    case OP_ne:
      op = (primType == PTY_f32) ? MOP_scmpeqrr : MOP_dcmpeqrr;
      if (jmpop == OP_brfalse) {
        branchOp = MOP_bne;
      }
      break;
    case OP_gt:
      op = (primType == PTY_f32) ? MOP_scmpgtrr : MOP_dcmpgtrr;
      if (jmpop == OP_brtrue) {
        branchOp = MOP_bne;
      }
      break;
    case OP_lt:
      op = (primType == PTY_f32) ? MOP_scmpltrr : MOP_dcmpltrr;
      if (jmpop == OP_brtrue) {
        branchOp = MOP_bne;
      }
      break;
    case OP_ge:
      op = (primType == PTY_f32) ? MOP_scmpgerr : MOP_dcmpgerr;
      if (jmpop == OP_brtrue) {
        branchOp = MOP_bne;
      }
      break;
    case OP_le:
      op = (primType == PTY_f32) ? MOP_scmplerr : MOP_dcmplerr;
      if (jmpop == OP_brtrue) {
        branchOp = MOP_bne;
      }
      break;
    default:
      op = MOP_undef;
      CG_ASSERT(false, "illegal logical operator");
    }
    fcmpResult = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8u));
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(op, fcmpResult, opnd0, opnd1));
    Riscv64RegOperand *xzr = Riscv64RegOperand::GetZeroRegister(64);
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(branchOp, fcmpResult, xzr, targetopnd));
  } else {
    bool isimm = opnd1->op_kind_ == Operand::Opd_Immediate;
    isMem = (opnd1->op_kind_ == Operand::Opd_Mem);
    bool isReg = (opnd1->op_kind_ == Operand::Opd_Register);
    if (!isReg && !isimm) {
      opnd1 = SelectCopy(opnd1, primType, primType);
    } else {
      opnd0 = LoadIntoRegister(opnd0, primType);
      if (isReg && (opnd1->size_ < 64)) {
        opnd1 = SelectZeroSignExt(opnd1, primType);
      }
    }

    bool isZero = false;
    if (isimm) {
      RegOperand *dest;
      if (static_cast<Riscv64ImmOperand *>(opnd1)->GetValue() == 0) {
        isZero = true;
      } else {
        regno_t vRegNo = New_V_Reg(kRegTyInt, 8);
        dest = CreateVirtualRegisterOperand(vRegNo);
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, dest, opnd1));
        opnd1 = dest;
      }
    }
    bool issigned = IsPrimitiveInteger(primType) ? IsSignedInteger(primType) : (signedCond ? true : false);
    branchOp = PickJmpInsn(jmpop, cmpop, isfloat, issigned, isZero);
    if (isZero) {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(branchOp, opnd0, targetopnd));
    } else {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(branchOp, opnd0, opnd1, targetopnd));
    }
  }
  curbb->SetKind(BB::kBBIf);
}

/*  brtrue @label0 (ge u8 i32 (
    cmp i32 i64 (dread i64 %Reg2_J, dread i64 %Reg4_J),
    constVal i32 0))
   ===>
    cmp r1, r2
    bge Cond, label0
 */
void Riscv64CGFunc::SelectCondSpecial(CondGotoNode *stmt, BaseNode *expr) {
  CG_ASSERT((expr->op == OP_cmp && "unexpect opcode"), "");
  Operand *opnd0 = HandleExpr(expr, expr->Opnd(0));
  Operand *opnd1 = HandleExpr(expr, expr->Opnd(1));
  CompareNode *node = static_cast<CompareNode *>(expr);
  bool isfloat = IsPrimitiveFloat(node->opndType);
  opnd0 = LoadIntoRegister(opnd0, node->opndType);
  // most of FP constants are passed as Riscv64MemOperand
  // except 0.0 which is passed as Opd_FPZeroImmediate
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  if (opnd1ty != Operand::Opd_Immediate && opnd1ty != Operand::Opd_FPZeroImmediate) {
    opnd1 = LoadIntoRegister(opnd1, node->opndType);
  }
  SelectRiscv64Cmp(opnd0, opnd1, !isfloat, GetPrimTypeBitSize(node->opndType));
  // handle condgoto now.
  LabelIdx labelIdx = stmt->offset;
  BaseNode *condnode = stmt->Opnd(0);
  LabelOperand *targetopnd = GetOrCreateLabelOperand(labelIdx);
  Opcode cmpop = condnode->op;
  PrimType primType = static_cast<CompareNode *>(condnode)->opndType;
  isfloat = IsPrimitiveFloat(primType);
  bool issigned = IsPrimitiveInteger(primType) ? IsSignedInteger(primType) : (IsSignedInteger(condnode->primType) ? true : false);
  MOperator jmpOp = PickJmpInsn(stmt->op, cmpop, isfloat, issigned);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(jmpOp, targetopnd));
}

void Riscv64CGFunc::SelectCondGoto(CondGotoNode *stmt, Operand *opnd0, Operand *opnd1) {
  // handle brfalse/brtrue op, opnd0 can be a compare node or non-compare node
  // such as a dread for example
  LabelIdx labelIdx = stmt->offset;
  BaseNode *condnode = stmt->Opnd(0);
  LabelOperand *targetopnd = GetOrCreateLabelOperand(labelIdx);
  Opcode cmpop;

  PrimType primType;
  if (kOpcodeInfo.IsCompare(condnode->op)) {
    cmpop = condnode->op;
    primType = static_cast<CompareNode *>(condnode)->opndType;
  } else {
    // not a compare node; dread for example, take its ptype
    cmpop = OP_ne;
    primType = condnode->primType;
  }

  SelectCondGoto(targetopnd, stmt->op, cmpop, opnd0, opnd1, primType, IsSignedInteger(condnode->primType));
}

void Riscv64CGFunc::SelectGoto(GotoNode *stmt) {
  Operand *targetopnd = GetOrCreateLabelOperand(stmt->offset);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xuncond, targetopnd));
}

Operand *Riscv64CGFunc::SelectAdd(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  RegOperand *resopnd = CreateRegisterOperandOfType(dtype);
  PrimType prmtype;
  if (!IsPrimitiveVector(dtype)) {
    bool issigned = IsSignedInteger(dtype);
    uint32 dsize = GetPrimTypeBitSize(dtype);
    bool is64bits = (dsize == 64);
    bool isfloat = IsPrimitiveFloat(dtype);
    PrimType prmtype =
      isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
    bool doExtend = false;
    if (cg->cgopt_.DoZeroExtend()) {
      doExtend = true;
    }
    // Need to zero extend a smaller sized unsigned type
    uint32 size = (dtype == PTY_u8) ? 8 : ((dtype == PTY_u16) ? 16 : (dtype == PTY_u32 ? 32 : 0));
    if (doExtend && size > 0) {
      RegOperand *tmpopnd = CreateRegisterOperandOfType(dtype);
      SelectAdd(tmpopnd, opnd0, opnd1, prmtype);
      GenerateZext(resopnd, tmpopnd, dtype, size);
    } else {
      SelectAdd(resopnd, opnd0, opnd1, prmtype);
    }
  } else {
    prmtype = dtype;
    SelectVecAdd(resopnd, opnd0, opnd1, prmtype);
  }
  return resopnd;
}

void Riscv64CGFunc::SelectAdd(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectAdd(resopnd, SelectCopy(opnd0, prmtype, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      Riscv64ImmOperand *immopnd = static_cast<Riscv64ImmOperand *>(opnd1);
      // Add can have immediate 2nd source but Sub cannot
        if (immopnd->IsInBitSize(11)) {
          curbb->AppendInsn(
            cg->BuildInstruction<Riscv64Insn>(is64bits ? MOP_xaddrri12 : MOP_waddrri12, resopnd, opnd0, immopnd));
        } else {
          // load into register
          RegOperand *regopnd;
          if (isAfterRegAlloc) {
            CHECK_FATAL(curbb->isProEpilog, "Not prolog/epilog");

            RegType regty = GetRegTyFromPrimTyRiscv64(prmtype);
            uint8 bytelen = GetPrimTypeSize(prmtype);
            // Use one of the temp reg, R31, since this is at the end of epilog
            // where sp is being adjusted.  No more instruction should be after this.
            regopnd = GetOrCreatePhysicalRegisterOperand(R31, bytelen, regty);
          } else {
            regopnd = CreateRegisterOperandOfType(prmtype);
          }
          curbb->AppendInsn(
            cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, regopnd, immopnd));
          curbb->AppendInsn(
            cg->BuildInstruction<Riscv64Insn>(is64bits ? MOP_xaddrrr : MOP_waddrrr, resopnd, opnd0, regopnd));
        }
    } else {
      SelectAdd(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectAdd(resopnd, opnd1, opnd0, prmtype);  // commutative
  } else {
    if (IsPrimitiveFloat(prmtype)) {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(is64bits ? MOP_dadd : MOP_sadd, resopnd, opnd0, opnd1));
    } else if (IsPrimitiveInteger(prmtype)) {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(is64bits ? MOP_xaddrrr : MOP_waddrrr, resopnd, opnd0, opnd1));
    } else {
      CG_ASSERT(false, "NYI add");
    }
  }
}

Operand *Riscv64CGFunc::SelectAddroflabel(AddroflabelNode *expr) {
  // adrp reg, label-id
  regno_t vRegNo = New_V_Reg(kRegTyInt, expr->SizeOfInstr());
  Operand *dst = CreateVirtualRegisterOperand(vRegNo);
  Operand *immOpnd = CreateImmOperand(expr->offset, 64, false);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_adrp_label, dst, immOpnd));
  return dst;
}

Operand *Riscv64CGFunc::SelectCGArrayElemAdd(BinaryNode *node) {
  BaseNode *opnd0 = node->Opnd(0);
  BaseNode *opnd1 = node->Opnd(1);
  CG_ASSERT(opnd1->op == OP_constval, "Internal error, opnd1->op should be OP_constval.");

  switch (opnd0->op) {
  case OP_regread: {
    RegreadNode *regreadnode = static_cast<RegreadNode *>(opnd0);
    return SelectRegread(nullptr, regreadnode);
  }
  case OP_addrof: {
    AddrofNode *addrofnode = static_cast<AddrofNode *>(opnd0);
    MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(addrofnode->stIdx);
    CG_ASSERT(addrofnode->fieldID == 0, "For debug SelectCGArrayElemAdd.");

    PrimType primType = addrofnode->primType;
    regno_t vRegNo = New_V_Reg(kRegTyInt, GetPrimTypeSize(primType));
    Operand *result = CreateVirtualRegisterOperand(vRegNo);

    // OP_constval
    ConstvalNode *constvalnode = static_cast<ConstvalNode *>(opnd1);
    MIRConst *mirconst = constvalnode->constVal;
    MIRIntConst *mirintconst = dynamic_cast<MIRIntConst *>(mirconst);
    CHECK_FATAL(mirintconst != nullptr, "dynamic_cast failed in SelectCGArrayElemAdd");
    SelectAddrof(result, CreateStImmOperand(symbol, mirintconst->value, 0));

    return result;
  }
  default:
    CHECK_FATAL(0, "Internal error, cannot handle opnd0.");
  }
}

void Riscv64CGFunc::SelectSub(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(prmtype);
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  if (opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      Riscv64ImmOperand *immopnd = static_cast<Riscv64ImmOperand *>(opnd1);
/*      if (immopnd->IsNegative()) {
        immopnd->Negate();
        SelectAdd(resopnd, opnd0, immopnd, prmtype);
      } else */if ((immopnd->IsInBitSize(11) || immopnd->IsInBitSize(12, 12))) {
        // imm is positive, riscv has no subi.  negate and use add
        immopnd->Negate();
        SelectAdd(resopnd, opnd0, immopnd, prmtype);
      } else {
        RegOperand *regopnd;
        if (isAfterRegAlloc) {
          CHECK_FATAL(curbb->isProEpilog, "Not prolog/epilog");

          RegType regty = GetRegTyFromPrimTyRiscv64(prmtype);
          uint8 bytelen = GetPrimTypeSize(prmtype);
          // Use one of the temp reg, R31, since this is at the start of prolog
          // where sp is being adjusted.  No more instruction should be before this.
          regopnd = GetOrCreatePhysicalRegisterOperand(R31, bytelen, regty);
        } else {
          regopnd = CreateRegisterOperandOfType(prmtype);
        }
        curbb->AppendInsn(
          cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, regopnd, immopnd));
        curbb->AppendInsn(
          cg->BuildInstruction<Riscv64Insn>(is64bits ? MOP_xsubrrr : MOP_wsubrrr, resopnd, opnd0, regopnd));
      }
    } else {
      SelectSub(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
    }
  } else {
    if (isfloat) {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>((is64bits ? MOP_dsub : MOP_ssub), resopnd, opnd0, opnd1));
    } else {
      curbb->AppendInsn(
        cg->BuildInstruction<Riscv64Insn>((is64bits ? MOP_xsubrrr : MOP_wsubrrr), resopnd, opnd0, opnd1));
    }
  }
}

Operand *Riscv64CGFunc::SelectSub(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  CG_ASSERT(!IsPrimitiveVector(dtype), "NYI");
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  bool doExtend = false;
  if (cg->cgopt_.DoZeroExtend()) {
    doExtend = true;
  }
  // Need to zero extend a smaller sized unsigned type
  uint32 size = (dtype == PTY_u8) ? 8 : ((dtype == PTY_u16) ? 16 : (dtype == PTY_u32 ? 32 : 0));
  if (doExtend && size > 0) {
    RegOperand *tmpopnd = CreateRegisterOperandOfType(dtype);
    SelectSub(tmpopnd, opnd0, opnd1, prmtype);
    GenerateZext(resopnd, tmpopnd, dtype, size);
  } else {
    SelectSub(resopnd, opnd0, opnd1, prmtype);
  }
  return resopnd;
}

Operand *Riscv64CGFunc::SelectMpy(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  CG_ASSERT(!IsPrimitiveVector(dtype), "NYI");
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  bool doExtend = false;
  if (cg->cgopt_.DoZeroExtend()) {
    doExtend = true;
  }
  // Need to zero extend a smaller sized unsigned type
  uint32 size = (dtype == PTY_u8) ? 8 : ((dtype == PTY_u16) ? 16 : (dtype == PTY_u32 ? 32 : 0));
  if (doExtend && size > 0) {
    RegOperand *tmpopnd = CreateRegisterOperandOfType(dtype);
    SelectMpy(tmpopnd, opnd0, opnd1, prmtype);
    GenerateZext(resopnd, tmpopnd, dtype, size);
  } else {
    SelectMpy(resopnd, opnd0, opnd1, prmtype);
  }
  return resopnd;
}

void Riscv64CGFunc::SelectMpy(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);

  if ((opnd0ty == Operand::Opd_Immediate || opnd1ty == Operand::Opd_Immediate) && IsPrimitiveInteger(prmtype)) {
    ImmOperand *imm =
      opnd0ty == Operand::Opd_Immediate ? static_cast<ImmOperand *>(opnd0) : static_cast<ImmOperand *>(opnd1);
    Operand *otherop = opnd0ty == Operand::Opd_Immediate ? opnd1 : opnd0;
    int64 immValue = llabs(imm->GetValue());
    if (immValue != 0 && (immValue & (immValue - 1)) == 0) {
      if (otherop->op_kind_ != Operand::Opd_Register) {
        if (otherop->op_kind_ == Operand::Opd_Immediate) {
          // both operands are immediates
          ImmOperand *otherImm = static_cast<ImmOperand *>(otherop);
          int64 otherImmVal = llabs(otherImm->GetValue());
          otherImm->SetValue(otherImmVal * immValue);
          SelectCopy(otherop, prmtype, prmtype);
          curbb->lastinsn->SetOperand(0, resopnd);
          return;
        }
        otherop = SelectCopy(otherop, prmtype, prmtype);
      }
      Riscv64ImmOperand *shiftnum = CreateImmOperand(__builtin_ffsll(immValue) - 1, dsize, false);
      SelectShift(resopnd, otherop, shiftnum, kShiftLeft, prmtype);
      if (imm->GetValue() < 0) {
        SelectNeg(resopnd, resopnd, prmtype);
      }

      return;
    } else if (immValue > 2) {
      uint32 zeronum = __builtin_ffsll(immValue) - 1;
      int64 headval = static_cast<uint64>(immValue) >> zeronum;
      if (((headval + 1) & headval) == 0) {
        if (otherop->op_kind_ != Operand::Opd_Register) {
          otherop = SelectCopy(otherop, prmtype, prmtype);
        }
        Riscv64ImmOperand *shiftnum = CreateImmOperand(__builtin_ffsll(headval + 1) - 1, dsize, false);
        RegOperand *tmpopnd = CreateRegisterOperandOfType(prmtype);
        SelectShift(tmpopnd, otherop, shiftnum, kShiftLeft, prmtype);
        SelectSub(resopnd, tmpopnd, otherop, prmtype);
        shiftnum = CreateImmOperand(zeronum, dsize, false);
        SelectShift(resopnd, resopnd, shiftnum, kShiftLeft, prmtype);
        if (imm->GetValue() < 0) {
          SelectNeg(resopnd, resopnd, prmtype);
        }

        return;
      }
      if (((headval - 1) & (headval - 2)) == 0) {
        if (otherop->op_kind_ != Operand::Opd_Register) {
          otherop = SelectCopy(otherop, prmtype, prmtype);
        }
        Riscv64ImmOperand *shiftnum = CreateImmOperand(__builtin_ffsll(headval - 1) - 1, dsize, false);
        RegOperand *tmpopnd = CreateRegisterOperandOfType(prmtype);
        SelectShift(tmpopnd, otherop, shiftnum, kShiftLeft, prmtype);
        SelectAdd(resopnd, tmpopnd, otherop, prmtype);
        shiftnum = CreateImmOperand(zeronum, dsize, false);
        SelectShift(resopnd, resopnd, shiftnum, kShiftLeft, prmtype);
        if (imm->GetValue() < 0) {
          SelectNeg(resopnd, resopnd, prmtype);
        }

        return;
      }
    }
  }

  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectMpy(resopnd, SelectCopy(opnd0, prmtype, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectMpy(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectMpy(resopnd, opnd1, opnd0, prmtype);
  } else {
    if (IsPrimitiveFloat(prmtype)) {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(is64bits ? MOP_xvmuld : MOP_xvmuls, resopnd, opnd0, opnd1));
    } else if (IsPrimitiveInteger(prmtype)) {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmulrrr, resopnd, opnd0, opnd1));
    }
  }
}

void Riscv64CGFunc::SelectDiv(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);

#if 0  // Need to revisit this optimization for RISCV
  if (g->optim_level >= 2) {
    if (opnd1ty == Operand::Opd_Immediate && IsSignedInteger(prmtype)) {
      ImmOperand *imm = static_cast<ImmOperand *>(opnd1);
      int64 immValue = llabs(imm->GetValue());
      if (immValue != 0 && (immValue & (immValue - 1)) == 0) {
        if (immValue == 1) {
          if (imm->GetValue() > 0) {
            uint32 mop = is64bits ? MOP_xmovrr : MOP_wmovrr;
            curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, resopnd, opnd0));
          } else {
            SelectNeg(resopnd, opnd0, prmtype);
          }

          return;
        }
        int shiftnumber = __builtin_ffsll(immValue) - 1;
        Riscv64ImmOperand *shiftnum = CreateImmOperand(shiftnumber, dsize, false);
        SelectShift(resopnd, opnd0, CreateImmOperand(dsize - 1, dsize, false), kShiftAright, prmtype);
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, resopnd, resopnd, CreateImmOperand(dsize - shiftnumber, is64bits ? 64 : 32, false)));
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, resopnd, resopnd, opnd0));
        SelectShift(resopnd, resopnd, shiftnum, kShiftAright, prmtype);
        if (imm->GetValue() < 0) {
          SelectNeg(resopnd, resopnd, prmtype);
        }

        return;
      }
    } else if (opnd1ty == Operand::Opd_Immediate && IsUnsignedInteger(prmtype)) {
      ImmOperand *imm = static_cast<ImmOperand *>(opnd1);
      if (imm->GetValue() != 0) {
        if (imm->GetValue() > 0 && (imm->GetValue() & (imm->GetValue() - 1)) == 0) {
          Riscv64ImmOperand *shiftnum = CreateImmOperand(__builtin_ffsll(imm->GetValue()) - 1, dsize, false);
          SelectShift(resopnd, opnd0, shiftnum, kShiftLright, prmtype);

          return;
        } else if (imm->GetValue() < 0) {
          SelectRiscv64Cmp(opnd0, imm, true, dsize);
          SelectRiscv64CSet(resopnd, GetCondOperand(CC_CS), is64bits);

          return;
        }
      }
    }
  }
#endif

  if (opnd0ty != Operand::Opd_Register) {
    SelectDiv(resopnd, SelectCopy(opnd0, prmtype, prmtype), opnd1, prmtype);
  } else if (opnd1ty != Operand::Opd_Register) {
    SelectDiv(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
  } else {
    if (IsPrimitiveFloat(prmtype)) {
      // CG_ASSERT( (dsize == 32 || dsize == 64), "we don't support half-precision yet" );
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(is64bits ? MOP_ddivrrr : MOP_sdivrrr, resopnd, opnd0, opnd1));
    } else if (IsPrimitiveInteger(prmtype)) {
      bool issigned = IsSignedInteger(prmtype);
      uint32 mopDiv = is64bits ? (issigned ? MOP_xsdivrrr : MOP_xudivrrr) : (issigned ? MOP_wsdivrrr : MOP_wudivrrr);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopDiv, resopnd, opnd0, opnd1));
    }
  }
}

Operand *Riscv64CGFunc::SelectDiv(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  CG_ASSERT(!IsPrimitiveVector(dtype), "NYI");
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  SelectDiv(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void Riscv64CGFunc::SelectRem(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype, bool issigned,
                              bool is64bits) {
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  opnd1 = LoadIntoRegister(opnd1, prmtype);
  CG_ASSERT(IsPrimitiveInteger(prmtype), "Wrong type for REM");
  uint32 mopRem = is64bits ? (issigned ? MOP_xsremrrr : MOP_xuremrrr) : (issigned ? MOP_wsremrrr : MOP_wuremrrr);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopRem, resopnd, opnd0, opnd1));
}

Operand *Riscv64CGFunc::SelectRem(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  CG_ASSERT(IsPrimitiveInteger(dtype), "wrong type for rem");
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);

  PrimType prmtype = ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  SelectRem(resopnd, opnd0, opnd1, prmtype, issigned, is64bits);
  return resopnd;
}

void Riscv64CGFunc::SelectTest(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType dtype) {
  CG_ASSERT(false, "nyi");
}

Operand *Riscv64CGFunc::SelectLand(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType prmtype = node->primType;
  CG_ASSERT(IsPrimitiveInteger(prmtype), "Land should be integer type");
  bool is64bits = GetPrimTypeBitSize(prmtype) == 64;
  RegOperand *resopnd = CreateRegisterOperandOfType(is64bits ? PTY_u64 : PTY_u32);
  /*
   *   beq opnd0, 0, elselabel   # curbb
   *   beq opnd1, 0, elselabel   # bbcmp1
   *   resopnd = 1               # bbtrue
   *   goto done
   * elselabel:
   *   resopnd = 0               # bbfalse
   * done:
   *                             # newbb == new curbb
   */
  BB *bbcmp1 = CreateNewBB();
  BB *bbtrue = CreateNewBB();
  BB *bbfalse = CreateNewBB();
  BB *newbb = CreateNewBB();
  curbb->AppendBB(bbcmp1);
  bbcmp1->AppendBB(bbtrue);
  bbtrue->AppendBB(bbfalse);
  bbfalse->AppendBB(newbb);

  LabelIdx labidx1 = CreateLabel();
  bbfalse->AddLabel(labidx1);
  lab2bbmap[labidx1] = bbfalse;
  LabelOperand *bbfalseTargOpnd = GetOrCreateLabelOperand(labidx1);

  LabelIdx labidx2 = CreateLabel();
  newbb->AddLabel(labidx2);
  lab2bbmap[labidx2] = newbb;
  LabelOperand *newbbTargOpnd = GetOrCreateLabelOperand(labidx2);

  ImmOperand *fopnd = CreateImmOperand(0, is64bits ? 64 : 32, false);
  RegOperand *zeroOpnd = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8u));
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, zeroOpnd, fopnd));
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  curbb->SetKind(BB::kBBIf);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_beq, opnd0, zeroOpnd, bbfalseTargOpnd));

  curbb = bbcmp1;
  opnd1 = LoadIntoRegister(opnd1, prmtype);
  bbcmp1->SetKind(BB::kBBIf);
  bbcmp1->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_beq, opnd1, zeroOpnd, bbfalseTargOpnd));

  ImmOperand *topnd = CreateImmOperand(1, is64bits ? 64 : 32, false);
  bbtrue->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, resopnd, topnd));
  bbtrue->SetKind(BB::kBBGoto);
  bbtrue->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xuncond, newbbTargOpnd));

  bbfalse->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, resopnd, fopnd));

  curbb = newbb;

  return resopnd;
}

Operand *Riscv64CGFunc::SelectLor(BinaryNode *node, Operand *opnd0, Operand *opnd1, bool parentIsBr) {
  PrimType prmtype = node->primType;
  CG_ASSERT(IsPrimitiveInteger(prmtype), "Lior should be integer type");
  bool is64bits = GetPrimTypeBitSize(prmtype) == 64;
  RegOperand *resopnd = CreateRegisterOperandOfType(is64bits ? PTY_u64 : PTY_u32);
  /*
   *   bne opnd0, 0, iflabel     # curbb
   *   beq opnd1, 0, elselabel   # bbcmp1
   * iflabel:
   *   resopnd = 1               # bbtrue
   *   goto done
   * elselabel:
   *   resopnd = 0               # bbfalse
   * done:
   *                             # newbb == new curbb
   *
   */
  BB *bbcmp1 = CreateNewBB();
  BB *bbtrue = CreateNewBB();
  BB *bbfalse = CreateNewBB();
  BB *newbb = CreateNewBB();
  curbb->AppendBB(bbcmp1);
  bbcmp1->AppendBB(bbtrue);
  bbtrue->AppendBB(bbfalse);
  bbfalse->AppendBB(newbb);

  LabelIdx labidx0 = CreateLabel();
  bbtrue->AddLabel(labidx0);
  lab2bbmap[labidx0] = bbtrue;
  LabelOperand *bbtrueTargOpnd = GetOrCreateLabelOperand(labidx0);

  LabelIdx labidx1 = CreateLabel();
  bbfalse->AddLabel(labidx1);
  lab2bbmap[labidx1] = bbfalse;
  LabelOperand *bbfalseTargOpnd = GetOrCreateLabelOperand(labidx1);

  LabelIdx labidx2 = CreateLabel();
  newbb->AddLabel(labidx2);
  lab2bbmap[labidx2] = newbb;
  LabelOperand *newbbTargOpnd = GetOrCreateLabelOperand(labidx2);

  ImmOperand *fopnd = CreateImmOperand(0, is64bits ? 64 : 32, false);
  RegOperand *zeroOpnd = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8u));
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, zeroOpnd, fopnd));
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  curbb->SetKind(BB::kBBIf);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_bne, opnd0, zeroOpnd, bbtrueTargOpnd));

  curbb = bbcmp1;
  opnd1 = LoadIntoRegister(opnd1, prmtype);
  bbcmp1->SetKind(BB::kBBIf);
  bbcmp1->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_beq, opnd1, zeroOpnd, bbfalseTargOpnd));

  ImmOperand *topnd = CreateImmOperand(1, is64bits ? 64 : 32, false);
  bbtrue->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, resopnd, topnd));
  bbtrue->SetKind(BB::kBBGoto);
  bbtrue->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xuncond, newbbTargOpnd));

  bbfalse->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, resopnd, fopnd));

  curbb = newbb;

  return resopnd;
}

void Riscv64CGFunc::SelectCmpOp(Operand *resopnd, Operand *opnd0, Operand *opnd1, Opcode opcode, PrimType operandType) {
  uint32 dsize = resopnd->size_;
  bool isfloat = IsPrimitiveFloat(operandType);
  bool doZext0 = false;
  bool doZext1 = false;

  if (!IsSignedInteger(operandType) && dsize < 64) {
    if (opnd0->IsRegister()) { // only for unsigned
      opnd0 = SelectZeroSignExt(opnd0, operandType);
    }
    if (opnd1->IsRegister()) {
      opnd1 = SelectZeroSignExt(opnd1, operandType);
    }
  }
  opnd0 = LoadIntoRegister(opnd0, operandType);

  // most of FP constants are passed as Riscv64MemOperand
  // except 0.0 which is passed as Opd_FPZeroImmediate
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  if (opnd1ty != Operand::Opd_Immediate && opnd1ty != Operand::Opd_FPZeroImmediate) {
    opnd1 = LoadIntoRegister(opnd1, operandType);
  }

  bool unsignedIntegerComparison = !isfloat && !IsSignedInteger(operandType);
  /* OP_cmp, OP_cmpl, OP_cmpg
   * <cmp> OP0, OP1  ; fcmp for OP_cmpl/OP_cmpg, cmp/fcmpe for OP_cmp
   * CSINV RES, WZR, WZR, GE
   * CSINC RES, RES, WZR, LE
   * if OP_cmpl, CSINV RES, RES, WZR, VC (no overflow)
   * if OP_cmpg, CSINC RES, RES, WZR, VC (no overflow)
   */
  Riscv64RegOperand *xzr = Riscv64RegOperand::GetZeroRegister(dsize);
    static_cast<RegOperand *>(resopnd)->SetValidBitsNum(1);
    if (IsPrimitiveFloat(operandType)) {
      ImmOperand *topnd = CreateImmOperand(1, dsize, false);
      ImmOperand *fopnd = CreateImmOperand(0, dsize, false);
      if (opnd0->size_ != opnd1->size_) {
        LogInfo::MapleLogger() << "WARNING: SelectCmpOp: Inconsistent size" << endl;
      }
      uint32 sz = (opnd1->size_ <=32) ? 32 : 64;
      SelectRiscv64SelectOp(resopnd, opnd0, opnd1, topnd, fopnd, opcode, sz, unsignedIntegerComparison);
      return;
    }

    MOperator mop;
    regno_t vRegNo = New_V_Reg(kRegTyInt, GetPrimTypeSize(operandType));
    if (opnd1ty == Operand::Opd_Immediate) {
      int64 value = static_cast<ImmOperand *>(opnd1)->GetValue();
      if (value == 0) {
        switch (opcode) {
          case OP_eq:
          case OP_ne:
            mop = opcode == OP_eq ? MOP_xseqz : MOP_xsnez;
            curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, resopnd, opnd0));
            break;
          case OP_gt:
            if (!unsignedIntegerComparison) {
              curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xsgt,
                resopnd, opnd0, xzr));
            } else {
              curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xsnez, resopnd, opnd0));
            }
            break;
          case OP_le: {
            if (!unsignedIntegerComparison) {
              ImmOperand *iopnd = CreateImmOperand(1, dsize, false);
              curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xslti,
                resopnd, opnd0, iopnd));
            } else {
              curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xseqz, resopnd, opnd0));
            }
            break;
          }
          case OP_lt:
          case OP_ge: {
            if (unsignedIntegerComparison && opcode) {
              if (opcode == OP_lt) {
                SelectCopy(resopnd, operandType, xzr, operandType);
              } else {
                RegOperand *rtmp = CreateVirtualRegisterOperand(vRegNo);
                ImmOperand *iopnd = CreateImmOperand(1, dsize, false);
                SelectCopy(resopnd, operandType, iopnd, operandType);
              }
              break;
            }
            RegOperand *rtmp = CreateVirtualRegisterOperand(vRegNo);
            if (opcode == OP_ge) {
              curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xnotrr, rtmp, opnd0));
            }
            ImmOperand *shiftNum =
              CreateImmOperand(opnd0->GetSize() == 64 ? 63 : 31, opnd0->GetSize() == 64 ? 6 : 5, false);
              curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(
                opnd0->GetSize() == 64 ? MOP_xlsrrri6 : MOP_wlsrrri5, resopnd,
                opcode == OP_ge ? rtmp : opnd0, shiftNum));
            break;
          }
          default:
            CHECK_FATAL(0, "Invalid set instruction");
        }
        return;
      } else {
        // special case
        if (opcode == OP_ge && value == 1) {
          curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xsgt,
            resopnd, opnd0, xzr));
          return;
        }
        // load imm int register, flow through to below
        opnd1 = LoadIntoRegister(opnd1, operandType);
      }
    }
    RegOperand *rtmp = CreateVirtualRegisterOperand(vRegNo);
    switch (opcode) {
      case OP_eq:
      case OP_ne: {
        SelectSub(rtmp, opnd0, opnd1, PTY_i64);
        mop = (opcode == OP_eq) ? MOP_xseqz : MOP_xsnez;
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, resopnd, rtmp));
        break;
      }
      case OP_lt:
      case OP_le:
      case OP_gt:
      case OP_ge: {
        if (unsignedIntegerComparison) {
          mop = (opcode == OP_lt || opcode == OP_ge) ? MOP_xsltu : MOP_xsgtu;
        } else {
          mop = (opcode == OP_lt || opcode == OP_ge) ? MOP_xslt : MOP_xsgt;
        }
        if (opcode == OP_lt || opcode == OP_gt) {
          curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, resopnd, opnd0, opnd1));
        } else {
          curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, rtmp, opnd0, opnd1));
          ImmOperand *iopnd = CreateImmOperand(1, dsize, false);
          SelectBxor(resopnd, rtmp, iopnd, operandType);
        }
        break;
      }
      default:
        CHECK_FATAL(0, "Invalid set instruction");
    }
}

Operand *Riscv64CGFunc::SelectCmpOp(CompareNode *node, Operand *opnd0, Operand *opnd1) {
  RegOperand *resopnd = CreateRegisterOperandOfType(node->primType);
  SelectCmpOp(resopnd, opnd0, opnd1, node->op, node->opndType);
  return resopnd;
}

void Riscv64CGFunc::SelectFPCmpQuiet(Operand *o0, Operand *o1, uint32 dsize) {
  CHECK_FATAL((dsize == 32 || dsize == 64), "");
  MOperator mopcode = 0;

  if (o1->op_kind_ == Operand::Opd_FPZeroImmediate) {
    mopcode = (dsize == 64) ? MOP_dcmpqri : MOP_scmpqri;
  } else if (o1->op_kind_ == Operand::Opd_Register) {
    mopcode = (dsize == 64) ? MOP_dcmpqrr : MOP_scmpqrr;
  } else {
    CG_ASSERT(false, "unsupported operand type");
  }
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopcode, o0, o1));
}

void Riscv64CGFunc::SelectRiscv64Cmp(Operand *o0, Operand *o1, bool isIntty, uint32 dsize) {
  MOperator mopcode = 0;
  if (isIntty) {
    if (o1->op_kind_ == Operand::Opd_Immediate) {
      ImmOperand *immopnd = static_cast<ImmOperand *>(o1);
      // imm : 0 ~ 4095, shift: none, LSL #0, or LSL #12
      // riscv64 assembly takes up to 24-bits, if the lower 12 bits is all 0
      if (immopnd->IsInBitSize(11)) {
        mopcode = (dsize == 64) ? MOP_xcmpri : MOP_wcmpri;
      } else {
        // load into register
        PrimType primType = (dsize == 64) ? PTY_i64 : PTY_i32;
        o1 = SelectCopy(o1, primType, primType);
        mopcode = (dsize == 64) ? MOP_xcmprr : MOP_wcmprr;
      }
    } else if (o1->op_kind_ == Operand::Opd_Register) {
      mopcode = (dsize == 64) ? MOP_xcmprr : MOP_wcmprr;
    } else {
      CG_ASSERT(false, "unsupported operand type");
    }
  } else { /* float */
    CHECK_FATAL((dsize == 32 || dsize == 64), "");
    if (o1->op_kind_ == Operand::Opd_FPZeroImmediate) {
      mopcode = (dsize == 64) ? MOP_dcmperi : MOP_scmperi;
    } else if (o1->op_kind_ == Operand::Opd_Register) {
      mopcode = (dsize == 64) ? MOP_dcmperr : MOP_scmperr;
    } else {
      CG_ASSERT(false, "unsupported operand type");
    }
  }
  CG_ASSERT(mopcode != 0, "mopcode undefined");
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopcode, o0, o1));
}

Operand *Riscv64CGFunc::SelectBand(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  SelectBand(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void Riscv64CGFunc::SelectBand(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectBand(resopnd, SelectCopy(opnd0, prmtype, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      Riscv64ImmOperand *immopnd = static_cast<Riscv64ImmOperand *>(opnd1);
      if (immopnd->IsZero()) {
        uint32 mopMv = is64bits ? MOP_xmovrr : MOP_wmovrr;
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopMv, resopnd, Riscv64RegOperand::GetZeroRegister(dsize)));
      } else if ((is64bits && immopnd->IsAllOnes()) || (!is64bits && immopnd->IsAllOnes32bit())) {
        SelectCopy(resopnd, prmtype, opnd0, prmtype);
      } else if (immopnd->IsInSimmBitSize(12)) {
        uint32 mopBand = /*is64bits ? */MOP_xandrri13/* : MOP_wandrri12*/;
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopBand, resopnd, opnd0, opnd1));
      } else {
        int64 immVal = immopnd->GetValue();
        RegOperand *regopnd = CreateRegisterOperandOfType(prmtype);
        SelectCopyImm(regopnd, immopnd, prmtype);
        uint32 mopBand = /*is64bits ? */MOP_xandrrr/* : MOP_wandrrr*/;
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopBand, resopnd, opnd0, regopnd));
      }
    } else {
      SelectBand(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBand(resopnd, opnd1, opnd0, prmtype);
  } else {
    CG_ASSERT(IsPrimitiveInteger(prmtype), "NYI band");
    uint32 mopBand = /*is64bits ? */MOP_xandrrr/* : MOP_wandrrr*/;
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopBand, resopnd, opnd0, opnd1));
  }
}

Operand *Riscv64CGFunc::SelectBior(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  SelectBior(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void Riscv64CGFunc::SelectBior(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectBior(resopnd, SelectCopy(opnd0, prmtype, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      Riscv64ImmOperand *immopnd = static_cast<Riscv64ImmOperand *>(opnd1);

      if (immopnd->IsZero()) {
        SelectCopy(resopnd, prmtype, opnd0, prmtype);
      } else if (immopnd->IsAllOnes()) {
        curbb->AppendInsn(
          cg->BuildInstruction<Riscv64Insn>(MOP_xinegrr, resopnd, Riscv64RegOperand::GetZeroRegister(dsize)));
      } else if (immopnd->IsInSimmBitSize(12)) {
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xiorrri13, resopnd, opnd0, opnd1));
      } else {
        int64 immVal = immopnd->GetValue();
        RegOperand *regopnd = CreateRegisterOperandOfType(prmtype);
        SelectCopyImm(regopnd, immopnd, prmtype);
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xiorrrr, resopnd, opnd0, regopnd));
      }
    } else {
      SelectBior(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBior(resopnd, opnd1, opnd0, prmtype);
  } else {
    CG_ASSERT(IsPrimitiveInteger(prmtype), "NYI band");
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xiorrrr, resopnd, opnd0, opnd1));
  }
}

Operand *Riscv64CGFunc::SelectMin(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  SelectMin(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void Riscv64CGFunc::SelectMin(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (IsPrimitiveInteger(prmtype)) {
    opnd0 = LoadIntoRegister(opnd0, prmtype);
    opnd1 = LoadIntoRegister(opnd1, prmtype);
    resopnd = LoadIntoRegister(resopnd, prmtype);
    SelectRiscv64SelectOp(resopnd, opnd0, opnd1, opnd0, opnd1, OP_le, dsize, !IsSignedInteger(prmtype));
  } else if (IsPrimitiveFloat(prmtype)) {
    opnd0 = LoadIntoRegister(opnd0, prmtype);
    opnd1 = LoadIntoRegister(opnd1, prmtype);
    SelectFMinFMax(resopnd, opnd0, opnd1, is64bits, true /*min*/);
  }
}

Operand *Riscv64CGFunc::SelectMax(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  SelectMax(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void Riscv64CGFunc::SelectMax(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (IsPrimitiveInteger(prmtype)) {
    opnd0 = LoadIntoRegister(opnd0, prmtype);
    opnd1 = LoadIntoRegister(opnd1, prmtype);
    resopnd = LoadIntoRegister(resopnd, prmtype);
    SelectRiscv64SelectOp(resopnd, opnd0, opnd1, opnd0, opnd1, OP_ge, dsize, !IsSignedInteger(prmtype));
  } else if (IsPrimitiveFloat(prmtype)) {
    opnd0 = LoadIntoRegister(opnd0, prmtype);
    opnd1 = LoadIntoRegister(opnd1, prmtype);
    SelectFMinFMax(resopnd, opnd0, opnd1, is64bits, false /*min*/);
  } else {
    CG_ASSERT(false, "NIY type max");
  }
}

void Riscv64CGFunc::SelectFMinFMax(Operand *resopnd, Operand *opnd0, Operand *opnd1, bool is64bits, bool isMin) {
  uint32 mopcode = isMin ? (is64bits ? MOP_xfminrrr : MOP_wfminrrr) : (is64bits ? MOP_xfmaxrrr : MOP_wfmaxrrr);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopcode, resopnd, opnd0, opnd1));
}

Operand *Riscv64CGFunc::SelectBxor(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  SelectBxor(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void Riscv64CGFunc::SelectBxor(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectBxor(resopnd, SelectCopy(opnd0, prmtype, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      Riscv64ImmOperand *immopnd = static_cast<Riscv64ImmOperand *>(opnd1);
      if (immopnd->IsZero()) {
        SelectCopy(resopnd, prmtype, opnd0, prmtype);
      } else if (immopnd->IsAllOnes()) {
        SelectMvn(resopnd, opnd0, prmtype);
      } else if (immopnd->IsInSimmBitSize(12)) {
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xeorrri13, resopnd, opnd0, opnd1));
      } else {
        int64 immVal = immopnd->GetValue();
        RegOperand *regopnd = CreateRegisterOperandOfType(prmtype);
        SelectCopyImm(regopnd, immopnd, prmtype);
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xeorrrr, resopnd, opnd0, regopnd));
      }
    } else {
      SelectBxor(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBxor(resopnd, opnd1, opnd0, prmtype);
  } else {
    CG_ASSERT(IsPrimitiveInteger(prmtype), "NYI bxor");
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xeorrrr, resopnd, opnd0, opnd1));
  }
}

Operand *Riscv64CGFunc::SelectShift(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  Opcode opcode = node->op;
  SHIFTDIRECTION direct = (opcode == OP_lshr) ? kShiftLright : (opcode == OP_ashr ? kShiftAright : kShiftLeft);
  SelectShift(resopnd, opnd0, opnd1, direct, prmtype);
  return resopnd;
}

void Riscv64CGFunc::SelectShift(Operand *resopnd, Operand *opnd0, Operand *opnd1, SHIFTDIRECTION direct,
                                PrimType prmtype) {
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  opnd0 = LoadIntoRegister(opnd0, prmtype);

  MOperator mopShift;
  if (opnd1ty == Operand::Opd_Immediate) {
    Riscv64ImmOperand *immopnd1 = static_cast<Riscv64ImmOperand *>(opnd1);
    const int64 kVal = immopnd1->GetValue();
    const uint32 kShiftamt = is64bits ? 63 : 31;
    if (kVal == 0) {
      SelectCopy(resopnd, prmtype, opnd0, prmtype);
      return;
    }
    // CG_ASSERT(shiftamt >= val, "shift error oversize");
    // e.g. a >> -1
    if (kVal < 0 || kVal > kShiftamt) {
      SelectShift(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), direct, prmtype);
      return;
    }
    switch (direct) {
      case kShiftLeft:
        if (kVal == 1) {
          SelectAdd(resopnd, opnd0, opnd0, prmtype);
          return;
        }
        mopShift = is64bits ? MOP_xlslrri6 : MOP_wlslrri5;
        break;
      case kShiftAright:
        mopShift = is64bits ? MOP_xasrrri6 : MOP_wasrrri5;
        break;
      case kShiftLright:
        mopShift = is64bits ? MOP_xlsrrri6 : MOP_wlsrrri5;
        break;
    }
  } else if (opnd1ty != Operand::Opd_Register) {
    SelectShift(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), direct, prmtype);
    return;
  } else {
    switch (direct) {
      case kShiftLeft:
        mopShift = is64bits ? MOP_xlslrrr : MOP_wlslrrr;
        break;
      case kShiftAright:
        mopShift = is64bits ? MOP_xasrrrr : MOP_wasrrrr;
        break;
      case kShiftLright:
        mopShift = is64bits ? MOP_xlsrrrr : MOP_wlsrrrr;
        break;
    }
  }

  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopShift, resopnd, opnd0, opnd1));
}

Operand *Riscv64CGFunc::SelectAbs(UnaryNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  if (IsPrimitiveFloat(dtyp)) {
    CG_ASSERT(GetPrimTypeBitSize(dtyp) >= 32, "We don't support hanf-word FP operands yet");
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    opnd0 = LoadIntoRegister(opnd0, dtyp);
    RegOperand *resopnd = CreateRegisterOperandOfType(dtyp);
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(is64bits ? MOP_dabsrr : MOP_sabsrr, resopnd, opnd0));
    return resopnd;
  } else {
    // srai/w x <- val >> 63|31
    // xor y <- val, x
    // sub/w result <- y, x
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    PrimType prmtype = is64bits ? (PTY_i64) : (PTY_i32);  // promoted type
    RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
    opnd0 = LoadIntoRegister(opnd0, prmtype);
    MOperator mop = is64bits ? MOP_xasrrri6 : MOP_wasrrri5;
    RegOperand *tmp1Dest = CreateRegisterOperandOfType(prmtype);
    int32 shiftVal = opnd0->size_ - 1;
    ImmOperand *shift = CreateImmOperand(shiftVal, 4, false);
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, tmp1Dest, opnd0, shift));
    RegOperand *tmp2Dest = CreateRegisterOperandOfType(prmtype);
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xeorrrr, tmp2Dest, opnd0, tmp1Dest));
    mop = is64bits ? MOP_xsubrrr : MOP_wsubrrr;
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, resopnd, tmp2Dest, tmp1Dest));
    return resopnd;
  }
}

Operand *Riscv64CGFunc::SelectBnot(UnaryNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  RegOperand *resopnd = nullptr;
  if (IsPrimitiveInteger(dtyp)) {
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    bool issigned = IsSignedInteger(dtyp);
    PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
    resopnd = CreateRegisterOperandOfType(prmtype);

    opnd0 = LoadIntoRegister(opnd0, prmtype);

    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xnotrr, resopnd, opnd0));
  } else {
    CG_ASSERT(false, "bnot expect integer or NYI");
  }
  return resopnd;
}

Operand *Riscv64CGFunc::SelectZeroSignExt(Operand *opnd, PrimType primType) {
  if (opnd->IsRegister()) {
    PrimType dtype = PTY_i64;
    bool isSigned = false;
    uint32 bitSz = 0;
    switch (primType) {
    case PTY_u8:
      bitSz = 8;
      dtype = PTY_u64;
      break;
    case PTY_u16:
      bitSz = 16;
      dtype = PTY_u64;
      break;
    case PTY_u32:
      bitSz = 32;
      dtype = PTY_u64;
      break;
    case PTY_i8:
      isSigned = true;
      bitSz = 8;
      break;
    case PTY_i16:
      isSigned = true;
      bitSz = 16;
      break;
    case PTY_i32:
      isSigned = true;
      bitSz = 32;
      break;
    default:
      break;
    }
    if (bitSz > 0) {
      RegOperand *dest = CreateRegisterOperandOfType(dtype);
      opnd = isSigned ? GenerateSext(dest, opnd, dtype, bitSz) : GenerateZext(dest, opnd, dtype, bitSz);
    }
  }
  return opnd;
}

Operand *Riscv64CGFunc::GenerateSext(Operand *resopnd, Operand *opnd0, PrimType ptype, uint32 bitSize) {
  opnd0 = LoadIntoRegister(opnd0, ptype);
  if (bitSize == 32) {
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xsxtw64, resopnd, opnd0));
    return resopnd;
  }
  if (g->optim_level == 0) {
    RegOperand *tmp = CreateRegisterOperandOfType(ptype);
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, tmp, opnd0, CreateImmOperand(64 - bitSize, 32, false)));
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xasrrri6, resopnd, tmp, CreateImmOperand(64 - bitSize, 32, false)));
  } else {
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xsxtv64, resopnd, opnd0, CreateImmOperand(64 - bitSize, 32, false)));
  }
  return resopnd;
}

Operand *Riscv64CGFunc::SelectSext(ExtractbitsNode *node, Operand *opnd0) {
  //uint32 bitSz = node->bitsSize;
  PrimType dtyp = node->primType;
  RegOperand *resopnd = CreateRegisterOperandOfType(dtyp);
  return GenerateSext(resopnd, opnd0, dtyp, node->bitsSize);
}

Operand *Riscv64CGFunc::GenerateZext(Operand *resopnd, Operand *opnd0, PrimType ptype, uint32 bitSize) {
#if 0
  // slli val << (64 - bitSize)
  // srli val >> (64 - bitSize)
  opnd0 = LoadIntoRegister(opnd0, ptype);
  PrimType pty = GetPrimTypeBitSize(ptype) <= 32 ? PTY_u32 : PTY_u64;
  RegOperand *sllDest = CreateRegisterOperandOfType(pty);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, sllDest, opnd0, CreateImmOperand(64 - bitSize, 64, false)));
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlsrrri6, resopnd, sllDest, CreateImmOperand(64 - bitSize, 64, false)));
#else
  opnd0 = LoadIntoRegister(opnd0, ptype);
  PrimType pty = GetPrimTypeBitSize(ptype) <= 32 ? PTY_u32 : PTY_u64;
  RegOperand *sllDest = CreateRegisterOperandOfType(pty);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xuxtv64, resopnd, opnd0, CreateImmOperand(64 - bitSize, 64, false)));
#endif
  return resopnd;
}

Operand *Riscv64CGFunc::SelectZext(ExtractbitsNode *node, Operand *opnd0) {
  RegOperand *resopnd = CreateRegisterOperandOfType((GetPrimTypeSize(node->primType) <= 32) ? PTY_u32 : PTY_u64);
  return GenerateZext(resopnd, opnd0, node->primType, node->bitsSize);
}

Operand *Riscv64CGFunc::SelectExtractbits(ExtractbitsNode *node, Operand *opnd0) {
  if (node->op == OP_sext) {
    return SelectSext(node, opnd0);
  } else if (node->op == OP_zext) {
    return SelectZext(node, opnd0);
  }
  PrimType dtyp = node->primType;
  RegOperand *resopnd = CreateRegisterOperandOfType(dtyp);
  bool issigned = IsSignedInteger(dtyp);
  uint8 bitsOffset = node->bitsOffset;
  uint8 bitsSize = node->bitsSize;
  bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
  uint32 immWidth = 12;
  opnd0 = LoadIntoRegister(opnd0, dtyp);
  if (bitsOffset == 0 && !issigned && (bitsSize < immWidth)) {
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xandrri13, resopnd, opnd0, CreateImmOperand((static_cast<uint64>(1) << bitsSize) - 1, 4, false)));
    return resopnd;
  }
  if (issigned) {
    MOperator sllmop = (is64bits) ? MOP_xlslrri6 : MOP_wlslrri5;
    MOperator sramop = (is64bits) ? MOP_xasrrri6 : MOP_wasrrri5;
    RegOperand *sllDst = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
    if (is64bits) {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(sllmop, sllDst, opnd0, CreateImmOperand(64 - bitsOffset - bitsSize, 4, false)));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(sramop, resopnd, sllDst, CreateImmOperand(64 - bitsSize, 4, false)));
    } else {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(sllmop, sllDst, opnd0, CreateImmOperand(32 - bitsOffset - bitsSize, 4, false)));
      RegOperand *sraDst = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(sramop, sraDst, sllDst, CreateImmOperand(32 - bitsSize, 4, false)));
      RegOperand *sll64Dst = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, sll64Dst, sraDst, CreateImmOperand(32, 4, false)));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlsrrri6, resopnd, sll64Dst, CreateImmOperand(32, 4, false)));
      GenerateSext(resopnd, resopnd, dtyp, 32);
    }
  } else {
    RegOperand *srl1Dst = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlsrrri6, srl1Dst, opnd0, CreateImmOperand(bitsOffset, 4, false)));
    RegOperand *srl2Dst = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
    if (is64bits) {
      // li -1 -> one
      // srli ones, 64 - bitsSize -> val2
      RegOperand *oneDst = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, oneDst, CreateImmOperand(-1, 8, true)));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlsrrri6, srl2Dst, oneDst, CreateImmOperand(64 - bitsSize, 4, false)));
    } else {
      // li 1 << bitsSize  -> mask
      // addi mask - 1 -> val2
      RegOperand *maskDst = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, maskDst, CreateImmOperand(1 << bitsSize, 8, false)));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, srl2Dst, maskDst, CreateImmOperand(-1, 8, true)));
    }
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xandrrr, resopnd, srl1Dst, srl2Dst));
  }

  return resopnd;
}

/*
   operand fits in MOVK if
   boffst == 0, 16, 32, 48 && bitsSize == 16
 */
inline bool IsMoveWideKeepable(uint32 bitsOffset, uint32 bitsSize, bool is64bits) {
  CG_ASSERT(is64bits || bitsOffset < 32, "");
  return (bitsSize == 16 && ((bitsOffset >> 4) & ~static_cast<uint32>(is64bits ? 0x3 : 0x1)) == 0);
}

// we use the fact that A ^ B ^ A == B, A ^ 0 = A
void Riscv64CGFunc::SelectDepositbits(Operand *resopnd, Operand *opnd0, Operand *opnd1, uint32 bitsOffset, uint32 bitsSize,
                                      PrimType rtyp, PrimType dtyp) {
  RegOperand *t1opnd = CreateRegisterOperandOfType(rtyp);
  bool is64bits = GetPrimTypeBitSize(rtyp) == 64;
  RegOperand *dest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
  if (is64bits) {
    if (bitsSize < 32) {
      // lui bottom 'bitsSize' 0
      // addiw 1
      // slli bitsOffset
      // addi -1
      uint32 mask = -1;
      for (uint i = 0; i < bitsSize; ++i) {
        mask &= ~(1ULL << i);
      }
      uint64 mask_upper = mask >> 12;
      RegOperand *luiDest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      Riscv64ImmOperand *imm = CreateImmOperand(mask_upper, 32, false);
      imm->isUpperBits = true;
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmov20up, luiDest, imm));
      RegOperand *addiwDest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_waddrri12, addiwDest, luiDest, CreateImmOperand(1, 32, false)));
      RegOperand *sllDest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, sllDest, addiwDest, CreateImmOperand(bitsOffset, 32, false)));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, dest, sllDest, CreateImmOperand(-1, 32, true)));
    } else {
      // addi zero, -1
      // slli bitsSize
      // addi 1
      // slli bitsOffset
      // addi -1
      RegOperand *add1Dest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      Operand *zero = GetOrCreatePhysicalRegisterOperand(RZERO, 64, kRegTyInt);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, add1Dest, zero, CreateImmOperand(-1, 32, true)));
      RegOperand *sll1Dest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, sll1Dest, add1Dest, CreateImmOperand(bitsSize, 32, false)));
      RegOperand *add2Dest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, add2Dest, sll1Dest, CreateImmOperand(1, 32, false)));
      RegOperand *sll2Dest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, sll2Dest, add2Dest, CreateImmOperand(bitsOffset, 32, false)));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, dest, sll2Dest, CreateImmOperand(-1, 32, true)));
    }
  } else {
    uint32 mask = -1;
    for (uint i = bitsOffset; i < (bitsSize + bitsOffset); ++i) {
      mask &= ~(1ULL << i);
    }
    uint32 mask_upper = mask >> 12;
    RegOperand *luiDest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 4));
    Riscv64ImmOperand *imm = CreateImmOperand(mask_upper, 32, false);
    imm->isUpperBits = true;
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmov20up, luiDest, imm));
    uint32 mask_lower = mask & 0x0fff;
    if (mask_lower & 0x800) {
      // negative
      RegOperand *constDest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 4));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, constDest, CreateImmOperand(mask_lower, 32, false)));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, dest, luiDest, constDest));
    } else {
      // mask fits in imm with 11 bits
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xiorrri13, dest, luiDest, CreateImmOperand(mask_lower, 32, false)));
    }
  }

  // Generate the bit value excluding target bitfield
  RegOperand *andDest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xandrrr, andDest, opnd0, dest));

  if (opnd1->IsIntImmediate()) {
    opnd1 = SelectCopy(opnd1, rtyp, rtyp);
  }
  // Mask away the target bits upper part beyond its bit size limit
  RegOperand *maskDest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
  uint64 fieldMask = 0;
  for (uint i = 0; i < bitsSize; ++i) {
    fieldMask |= (1ULL << i);
  }
  if (bitsSize <= 11) {
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xandrri13, maskDest, opnd1, CreateImmOperand(fieldMask, 32, false)));
  } else {
    RegOperand *maskImmDest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, maskImmDest, CreateImmOperand(fieldMask, 64, false)));
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xandrrr, maskDest, opnd1, maskImmDest));
  }
  opnd1 = maskDest;
  if (bitsOffset > 0) {
    // shift the target bit field into the right position
    RegOperand *sllDest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 4));
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, sllDest, opnd1, CreateImmOperand(bitsOffset, 32, false)));
    opnd1 = sllDest;
  }
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xiorrrr, resopnd, andDest, opnd1));
}

Operand *Riscv64CGFunc::SelectDepositbits(DepositbitsNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtyp = node->primType;
  RegOperand *resopnd = CreateRegisterOperandOfType(dtyp);
  SelectDepositbits(resopnd, opnd0, opnd1, node->bitsOffset, node->bitsSize, dtyp, dtyp);
  return resopnd;
}

Operand *Riscv64CGFunc::SelectLnot(UnaryNode *node, Operand *opnd0) {
  PrimType dtype = node->primType;
  RegOperand *resopnd = CreateRegisterOperandOfType(dtype);
  bool is64bits = (GetPrimTypeBitSize(dtype) == 64);
  opnd0 = LoadIntoRegister(opnd0, dtype);
  Operand *zero = GetOrCreatePhysicalRegisterOperand(RZERO, GetPrimTypeBitSize(dtype), kRegTyInt);
  RegOperand *sltuDest = CreateRegisterOperandOfType(dtype);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xsltu, sltuDest, zero, opnd0));
  ImmOperand *neg = CreateImmOperand(1, 32, false);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xeorrri13, resopnd, opnd0, neg));
  return resopnd;
}

Operand *Riscv64CGFunc::SelectNeg(UnaryNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
  PrimType prmtype;
  if (IsPrimitiveFloat(dtyp)) {
    prmtype = dtyp;
  } else {
    prmtype = is64bits ? (PTY_i64) : (PTY_i32);  // promoted type
  }
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  SelectNeg(resopnd, opnd0, prmtype);
  return resopnd;
}

void Riscv64CGFunc::SelectNeg(Operand *dest, Operand *opnd0, PrimType prmtype) {
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  bool is64bits = (GetPrimTypeBitSize(prmtype) == 64);
  MOperator mop;
  if (IsPrimitiveFloat(prmtype)) {
    mop = is64bits ? MOP_xfnegrr : MOP_wfnegrr;
  } else {
    mop = is64bits ? MOP_xinegrr : MOP_winegrr;
  }
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, dest, opnd0));
}

void Riscv64CGFunc::SelectMvn(Operand *dest, Operand *opnd0, PrimType prmtype) {
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  bool is64bits = (GetPrimTypeBitSize(prmtype) == 64);
  MOperator mop;
  CG_ASSERT(!IsPrimitiveFloat(prmtype), "Instruction 'mvn' do not have float version.");
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xnotrr, dest, opnd0));
}

Operand *Riscv64CGFunc::SelectRecip(UnaryNode *node, Operand *opnd0) {
  // fconsts s15, #112
  // fdivs s0, s15, s0
  PrimType dtyp = node->primType;
  if (IsPrimitiveFloat(dtyp)) {
    opnd0 = LoadIntoRegister(opnd0, dtyp);
    RegOperand *resopnd = CreateRegisterOperandOfType(dtyp);
    Operand *one = nullptr;
    if (GetPrimTypeBitSize(dtyp) == 64) {
      MIRDoubleConst *c = memPool->New<MIRDoubleConst>(1.0, GlobalTables::GetTypeTable().typeTable.at(PTY_f64));
      one = SelectDoubleconst(c);
    } else if (GetPrimTypeBitSize(dtyp) == 32) {
      MIRFloatConst *c = memPool->New<MIRFloatConst>(1.0f, GlobalTables::GetTypeTable().typeTable.at(PTY_f32));
      one = SelectFloatconst(c);
    } else {
      CG_ASSERT(false, "we don't support half-precision fp operations yet");
    }
    SelectDiv(resopnd, one, opnd0, dtyp);
    return resopnd;
  } else {
    CG_ASSERT(false, "should be float type");
  }
  return nullptr;
}

Operand *Riscv64CGFunc::SelectSqrt(UnaryNode *node, Operand *opnd0) {
  /* gcc generates code like below for better accurate
     fsqrts  s15, s0
     fcmps s15, s15
     fmstat
     beq .L4
     push  {r3, lr}
     bl  sqrtf
     pop {r3, pc}
     .L4:
     fcpys s0, s15
     bx  lr */
  PrimType dtyp = node->primType;
  if (IsPrimitiveFloat(dtyp)) {
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    opnd0 = LoadIntoRegister(opnd0, dtyp);
    RegOperand *resopnd = CreateRegisterOperandOfType(dtyp);
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(is64bits ? MOP_vsqrtd : MOP_vsqrts, resopnd, opnd0));
    return resopnd;
  } else {
    CG_ASSERT(false, "should be float type");
  }
  return nullptr;
}

void Riscv64CGFunc::SelectCvtFloat2Int(Operand *resopnd, Operand *opnd0, PrimType itype, PrimType ftype) {
  bool is64bitsFloat = (ftype == PTY_f64);
  MOperator mop = 0;

  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong from type");

  opnd0 = LoadIntoRegister(opnd0, ftype);
  switch (itype) {
    case PTY_i32:
      mop = !is64bitsFloat ? MOP_vcvtrf : MOP_vcvtrd;
      break;
    case PTY_u32:
      mop = !is64bitsFloat ? MOP_vcvturf : MOP_vcvturd;
      break;
    case PTY_i64:
      mop = !is64bitsFloat ? MOP_xvcvtrf : MOP_xvcvtrd;
      break;
    case PTY_u64:
      mop = !is64bitsFloat ? MOP_xvcvturf : MOP_xvcvturd;
      break;
    default:
      CG_ASSERT(false, "unexpected type");
  }
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, resopnd, opnd0));
}

void Riscv64CGFunc::SelectCvtInt2Float(Operand *resopnd, Operand *opnd0, PrimType totype, PrimType fromtype) {
  bool is64bitsFloat = (totype == PTY_f64);
  MOperator mop = 0;

  PrimType itype = (GetPrimTypeBitSize(fromtype) == 64) ? (IsSignedInteger(fromtype) ? PTY_i64 : PTY_u64)
                                                        : (IsSignedInteger(fromtype) ? PTY_i32 : PTY_u32);

  opnd0 = LoadIntoRegister(opnd0, itype);
  switch (itype) {
    case PTY_i32:
      mop = !is64bitsFloat ? MOP_vcvtfr : MOP_vcvtdr;
      break;
    case PTY_u32:
      mop = !is64bitsFloat ? MOP_vcvtufr : MOP_vcvtudr;
      break;
    case PTY_i64:
      mop = !is64bitsFloat ? MOP_xvcvtfr : MOP_xvcvtdr;
      break;
    case PTY_u64:
      mop = !is64bitsFloat ? MOP_xvcvtufr : MOP_xvcvtudr;
      break;
    default:
      CG_ASSERT(false, "unexpected type");
  }
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, resopnd, opnd0));
}

Operand *Riscv64CGFunc::SelectCeil(TypeCvtNode *node, Operand *opnd0) {
  if (CLANG) {
    // Generate C style ceilf() and ceil() calls
    PrimType ftype = node->fromPrimType;
    PrimType rtype = node->primType;
    vector<Operand *> opndvec;
    Riscv64RegOperand *physOpnd;
    if (ftype == PTY_f64) {
      physOpnd = GetOrCreatePhysicalRegisterOperand(D0, 64, kRegTyFloat);
    } else {
      physOpnd = GetOrCreatePhysicalRegisterOperand(S0, 32, kRegTyFloat);
    }
    Operand *resopnd = static_cast<Operand *>(physOpnd);
    opndvec.push_back(resopnd);
    opnd0 = LoadIntoRegister(opnd0, ftype);
    opndvec.push_back(opnd0);
    if (ftype == PTY_f32) {
      SelectLibCall("ceilf", opndvec, ftype, rtype);
    } else {
      SelectLibCall("ceil", opndvec, ftype, rtype);
    }
    return resopnd;
  }
  PrimType ftype = node->fromPrimType;
  PrimType itype = node->primType;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  RegOperand *resopnd = CreateRegisterOperandOfType(itype);
  opnd0 = LoadIntoRegister(opnd0, ftype);
  //curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(is64bits ? : , resopnd, opnd0));
  CHECK_FATAL(0, "NYI none C path");
  return resopnd;
}

// float to int floor
Operand *Riscv64CGFunc::SelectFloor(TypeCvtNode *node, Operand *opnd0) {
  if (CLANG) {
    // Generate C style floorf() and floor() calls
    PrimType ftype = node->fromPrimType;
    PrimType rtype = node->primType;
    vector<Operand *> opndvec;
    Riscv64RegOperand *physOpnd;
    if (ftype == PTY_f64) {
      physOpnd = GetOrCreatePhysicalRegisterOperand(D0, 64, kRegTyFloat);
    } else {
      physOpnd = GetOrCreatePhysicalRegisterOperand(S0, 32, kRegTyFloat);
    }
    Operand *resopnd = static_cast<Operand *>(physOpnd);
    opndvec.push_back(resopnd);
    opnd0 = LoadIntoRegister(opnd0, ftype);
    opndvec.push_back(opnd0);
    if (ftype == PTY_f32) {
      SelectLibCall("floorf", opndvec, ftype, rtype);
    } else {
      SelectLibCall("floor", opndvec, ftype, rtype);
    }
    return resopnd;
  }
  PrimType ftype = node->fromPrimType;
  PrimType itype = node->primType;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  RegOperand *resopnd = CreateRegisterOperandOfType(itype);
  opnd0 = LoadIntoRegister(opnd0, ftype);
  //curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(is64bits ? : , resopnd, opnd0));
  CHECK_FATAL(0, "NYI none C path");
  return resopnd;
}

static bool LIsPrimitivePointer(PrimType primType) {
  return (PTY_ptr <= primType && primType <= PTY_a64);
}

Operand *Riscv64CGFunc::SelectRetype(TypeCvtNode *node, Operand *opnd0) {
  PrimType fromtype = node->Opnd(0)->primType;
  PrimType totype = node->primType;
  CG_ASSERT(GetPrimTypeSize(fromtype) == GetPrimTypeSize(totype), "retype bit widith doesn' match");
  if (LIsPrimitivePointer(fromtype) && LIsPrimitivePointer(totype)) {
    return LoadIntoRegister(opnd0, totype);
  }
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  RegOperand *resopnd = CreateRegisterOperandOfType(totype);
  if (IsPrimitiveInteger(fromtype) || IsPrimitiveFloat(fromtype)) {
    bool isfromint = IsPrimitiveInteger(fromtype);
    bool is64bits = GetPrimTypeBitSize(fromtype) == 64;
    PrimType itype = isfromint
                       ? ((GetPrimTypeBitSize(fromtype) == 64) ? (IsSignedInteger(fromtype) ? PTY_i64 : PTY_u64)
                                                               : (IsSignedInteger(fromtype) ? PTY_i32 : PTY_u32))
                       : (is64bits ? PTY_f64 : PTY_f32);

    // if source operand is in memory,
    // simply read it as a value of 'totype 'into the dest operand
    // and return
    if (opnd0ty == Operand::Opd_Mem) {
      resopnd = SelectCopy(opnd0, totype, totype);
      return resopnd;
    }
    opnd0 = LoadIntoRegister(opnd0, itype);
    if ((IsPrimitiveFloat(fromtype) && IsPrimitiveInteger(totype)) ||
        (IsPrimitiveFloat(totype) && IsPrimitiveInteger(fromtype))) {
      uint32 mopFmov =
              isfromint ? (is64bits ? MOP_xvmovdr : MOP_xvmovsr) : (is64bits ? MOP_xvmovrd : MOP_xvmovrs);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopFmov, resopnd, opnd0));
      return resopnd;
    } else {
      return opnd0;
    }
  } else {
    CG_ASSERT(false, "NYI retype");
  }
  return nullptr;
}

void Riscv64CGFunc::SelectCvtFloat2Float(Operand *resopnd, Operand *opnd0, PrimType fromty, PrimType toty) {
  opnd0 = LoadIntoRegister(opnd0, fromty);
  MOperator mop = 0;
  switch (toty) {
    case PTY_f32: {
      switch (fromty) {
        case PTY_f64:
          mop = MOP_xvcvtfd;
          break;
        default:
          CG_ASSERT(false, "unexpected cvt from type");
      }
      break;
    }
    case PTY_f64: {
      switch (fromty) {
        case PTY_f32:
          mop = MOP_xvcvtdf;
          break;
        default:
          CG_ASSERT(false, "unexpected cvt from type");
      }
      break;
    }
    default:
      CG_ASSERT(false, "unexpected cvt to type");
  }
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, resopnd, opnd0));
}

bool Riscv64CGFunc::IfParamVRegOperand(Operand *opnd) {
  //ParmLocator parmlocator(becommon);

  CG_ASSERT(opnd->IsRegister(), "Operand should be a register operand.");

  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  regno_t regno = regOpnd->GetRegisterNumber();

  for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->formalDefVec[i].formalTyIdx);

    MIRSymbol *sym = func->formalDefVec[i].formalSym;
    if (!sym->IsPreg()) {
      continue;
    }

    PregIdx pregIdx = func->pregTab->GetPregIdxFromPregNo(sym->GetPreg()->pregNo);

    if (GetVirtualRegNoFromPseudoRegIdx(pregIdx) == regno) {
      return true;
    }
  }

  return false;
}

/*
   This should be regarded only as a reference.

   C11 specification.
   6.3.1.3 Signed and unsigned integers
   1 When a value with integer type is converted to another integer
    type other than _Bool, if the value can be represented by the
    new type, it is unchanged.
   2 Otherwise, if the new type is unsigned, the value is converted
    by repeatedly adding or subtracting one more than the maximum
    value that can be represented in the new type until the value
    is in the range of the new type.60)
   3 Otherwise, the new type is signed and the value cannot be
    represented in it; either the result is implementation-defined
    or an implementation-defined signal is raised.
 */
void Riscv64CGFunc::SelectCvtInt2Int(BaseNode *parent, Operand *&resopnd, Operand *opnd0, PrimType fromty,
                                     PrimType toty) {
  int fsize = GetPrimTypeBitSize(fromty);
  int tsize = GetPrimTypeBitSize(toty);
  if (fsize == 64 && fsize == tsize && opnd0->GetSize() < 64) {
    fsize = opnd0->GetSize();
    fromty = (fromty == PTY_u64) ? PTY_u32 : PTY_i32;
  }
  bool isexpand = tsize > fsize;
  bool is64bit = tsize == 64;
  if (parent && opnd0->IsIntImmediate() &&
      ((parent->op == OP_band) || (parent->op == OP_bior) || (parent->op == OP_bxor) || (parent->op == OP_ashr) ||
       (parent->op == OP_lshr) || (parent->op == OP_shl))) {
    ImmOperand *simm = static_cast<ImmOperand *>(opnd0);
    CG_ASSERT(simm != nullptr, "simm is nullptr in Riscv64CGFunc::SelectCvtInt2Int");
    bool isSign = false;
    int64 origValue = simm->GetValue();
    int64 newValue = origValue;
    int64 signValue = 0;
    if (isexpand) {
      // LogInfo::MapleLogger()<<"expand "<<simm->GetSize()<<std::endl;
    } else {
      // 64--->32
      if (fsize > tsize) {
        // LogInfo::MapleLogger()<<"before trunc "<<origValue<<"size "<<simm->GetSize()<<std::endl;
        if (IsSignedInteger(toty)) {
          if (origValue < 0) {
            signValue = 0xFFFFFFFFFFFFFFFF & ((1 << tsize));
          }
          newValue = origValue & (static_cast<int64>(1 << tsize) - 1) & signValue;
        } else {
          newValue = origValue & (static_cast<int64>(1 << tsize) - 1);
        }
        // LogInfo::MapleLogger()<<"after trunc "<<newValue<<std::endl;
      }
    }
    if (IsSignedInteger(toty)) {
      isSign = true;
    }
    resopnd = static_cast<Operand *>(CreateImmOperand(newValue, GetPrimTypeSize(toty) * 8, isSign));
    return;
  }
  if (isexpand) {  // Expansion
    /*
       int8_t i8;
       uint8_t ui8;
       short s ;
       unsigned short us ;
       i8 = 0xFF;
       ui8 = 0xFF;
       s = (short)i8;
       us = (unsigned short)i8;
       printf( "signed to signed %d -> %d\n", i8, s );
       printf( "signed to unsigned %d -> %d\n", i8, us );
       s = (short)ui8;
       us = (unsigned short)ui8;
       printf( "unsigned to signed %u -> %d\n", ui8, s );
       printf( "unsigned to unsigned %u -> %d\n", ui8, us );

       Output:
       signed to signed -1 -> -1
       signed to unsigned -1 -> 65535
       unsigned to signed 255 -> 255
       unsigned to unsigned 255 -> 255
     */
    // if cvt expr's parent is add,and,xor and some other,we can use the imm version
    PrimType prmty =
      (fsize == 64 ? (IsSignedInteger(fromty) ? PTY_i64 : PTY_u64) : (IsSignedInteger(fromty) ? PTY_i32 : PTY_u32));
    opnd0 = LoadIntoRegister(opnd0, prmty);

    if (IsSignedInteger(fromty)) {
      CG_ASSERT((is64bit || (fsize == 8 || fsize == 16)), "incorrect from size");
      GenerateSext(resopnd, opnd0, toty, fsize);
    } else {
      // Unsigned
      if (is64bit) {
        if (fsize == 8) {
          ImmOperand *immopnd = CreateImmOperand(0xff, 64, false);
          curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xandrri13, resopnd, opnd0, immopnd));
        } else if (fsize == 16) {
          ImmOperand *immopnd = CreateImmOperand(0xffff, 64, false);
          curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xandrri13, resopnd, opnd0, immopnd));
        } else {
          GenerateZext(resopnd, opnd0, toty, fsize);
        }
      } else {
        CG_ASSERT((fsize == 8 || fsize == 16), "incorrect from size");
        if (fsize == 8) {
          static_cast<RegOperand *>(opnd0)->SetValidBitsNum(8);
          static_cast<RegOperand *>(resopnd)->SetValidBitsNum(8);
        }
        if (fromty == PTY_u1) {
          static_cast<RegOperand *>(opnd0)->SetValidBitsNum(1);
          static_cast<RegOperand *>(resopnd)->SetValidBitsNum(1);
        }
        GenerateZext(resopnd, opnd0, toty, fsize);
      }
    }
  } else {  // Same size or truncate
#ifdef CNV_OPTIMIZE
    // No code needed for riscv64 with same reg.
    // Just update regno.
    RegOperand *reg = static_cast<RegOperand *>(resopnd);
    reg->reg_no_ = static_cast<RegOperand *>(opnd0)->reg_no_;
#else
    /*
       With GCC 5.4.1
       short s ;
       unsigned short us ;
       int   w = 0x00FFFFFF;
       s = (short) w;
       us = (unsigned short) w;
       printf( "signed %d -> %d\n", w, s );
       printf( "unsigned %d -> %d\n", w, us );

       Output:
       signed 16777215 -> -1
       unsigned 16777215 -> 65535
     */
    // This is not really needed if opnd0 is result from a load.
    // Hopefully the FE will get rid of the redundant conversions for loads.
    PrimType prmty =
      (fsize == 64 ? (IsSignedInteger(fromty) ? PTY_i64 : PTY_u64) : (IsSignedInteger(fromty) ? PTY_i32 : PTY_u32));
    opnd0 = LoadIntoRegister(opnd0, prmty);

    if (fsize > tsize) {
      IsSignedInteger(toty) ? GenerateSext(resopnd, opnd0, toty, tsize)
                            : GenerateZext(resopnd, opnd0, toty, tsize);
    } else {
      // same size, so resopnd can be set
      if (IsSignedInteger(fromty) == IsSignedInteger(toty) ||
          GetPrimTypeSize(toty) > 4) {
        Riscv64RegOperand *reg = static_cast<Riscv64RegOperand *>(resopnd);
        reg->SetRegisterNumber(static_cast<Riscv64RegOperand *>(opnd0)->GetRegisterNumber());
      } else if (IsUnsignedInteger(toty)) {
        GenerateZext(resopnd, opnd0, toty, GetPrimTypeBitSize(toty));
      } else {
        GenerateSext(resopnd, opnd0, toty, GetPrimTypeBitSize(toty));
      }
    }
#endif
  }
}

Operand *Riscv64CGFunc::SelectCvt(BaseNode *parent, TypeCvtNode *node, Operand *opnd0) {
  PrimType fromtype = node->fromPrimType;
  PrimType totype = node->primType;
  if (fromtype == totype) {
    return opnd0;  // noop
  }
  if (IsPrimitiveInteger(fromtype) && IsPrimitiveInteger(totype) &&
      opnd0->op_kind_ == Operand::Opd_Immediate &&
      GetPrimTypeBitSize(fromtype) < GetPrimTypeBitSize(totype)) {
    ImmOperand *otherImm = static_cast<ImmOperand *>(opnd0);
    int64 imm = otherImm->GetValue();
    switch (GetPrimTypeBitSize(fromtype)) {
    case 8: {
      int64 val;
      if (IsSignedInteger(totype)) {
        val = 0x7f;
      } else {
        val = 0xff;
      }
      if ((imm & val) == imm) {
        return LoadIntoRegister(opnd0, fromtype);
      }
      break;
    }
    case 16: {
      int64 val;
      if (IsSignedInteger(totype)) {
        val = 0x7fff;
      } else {
        val = 0xffff;
      }
      if ((imm & val) == imm) {
        return LoadIntoRegister(opnd0, fromtype);
      }
      break;
    }
    case 32: {
      int64 val;
      if (IsSignedInteger(totype)) {
        val = 0x7fffffff;
      } else {
        val = 0xffffffff;
      }
      if ((imm & val) == imm) {
        return LoadIntoRegister(opnd0, fromtype);
      }
      break;
    }
    default:
      break;
    }
  }
  Operand *resopnd = static_cast<Operand *>(CreateRegisterOperandOfType(totype));
  if (IsPrimitiveFloat(totype) && IsPrimitiveInteger(fromtype)) {
    SelectCvtInt2Float(resopnd, opnd0, totype, fromtype);
    return resopnd;
  } else if (IsPrimitiveFloat(fromtype) && IsPrimitiveInteger(totype)) {
    SelectCvtFloat2Int(resopnd, opnd0, totype, fromtype);
    return resopnd;
  } else if (IsPrimitiveInteger(fromtype) && IsPrimitiveInteger(totype)) {
#if 0
    if (GetPrimTypeSize(fromtype) == 8) {
      if (GetPrimTypeSize(totype) < 8) {
        if (fromtype == PTY_i64) {
          GenerateSext(resopnd, opnd0, totype, GetPrimTypeSize(totype) * BITS_PER_BYTE);
        } else {
          GenerateZext(resopnd, opnd0, totype, GetPrimTypeSize(totype) * BITS_PER_BYTE);
        }
        return resopnd;
      }
      return opnd0;
    }
    bool signExtend = false;
    switch (totype) {
    case PTY_u8:
    case PTY_u16:
    case PTY_u32:
    case PTY_u64:
    case PTY_a64:
      break;
    case PTY_i8:
    case PTY_i16:
    case PTY_i32:
    case PTY_i64:
      signExtend = true;
      break;
    default:
      CHECK_FATAL(0, "SelectCvt: unknown pty");
      break;
    }
    uint32 bitSize = GetPrimTypeSize(fromtype) * BITS_PER_BYTE;
    opnd0 = LoadIntoRegister(opnd0, fromtype);
    if (signExtend) {
      GenerateSext(resopnd, opnd0, totype, bitSize);
    } else {
      GenerateZext(resopnd, opnd0, totype, bitSize);
    }
#else
    // u64 <- i32  sign extension
    // i32 <- u64  sign extension
    // i64 <- u32  zero extension
    // u32 <- i64  sign extension
    // u64 <- u32  zero extension
    // u32 <- u64  sign extension
    // i64 <- i32  sign extension
    // i32 <- i64  sign extension
    uint32 bitSize;
    if (GetPrimTypeSize(fromtype) > GetPrimTypeSize(totype)) {
      bitSize = GetPrimTypeSize(totype) * BITS_PER_BYTE;
    } else {
      bitSize = GetPrimTypeSize(fromtype) * BITS_PER_BYTE;
    }
    bool isFromUnsigned =
        (fromtype == PTY_u8 || fromtype == PTY_u16 || fromtype == PTY_u32 || fromtype == PTY_u64)
            ? true : false;
    bool isToUnsigned =
        (totype == PTY_u8 || totype == PTY_u16 || totype == PTY_u32 || totype == PTY_u64)
            ? true : false;
    opnd0 = LoadIntoRegister(opnd0, fromtype);
    if (isFromUnsigned &&
        (isFromUnsigned == isToUnsigned || GetPrimTypeSize(fromtype) < GetPrimTypeSize(totype))) {
        GenerateZext(resopnd, opnd0, totype, bitSize);
    } else {
      GenerateSext(resopnd, opnd0, totype, bitSize);
    }
#endif
    return resopnd;
  } else {  // both are float type
    SelectCvtFloat2Float(resopnd, opnd0, fromtype, totype);
    return resopnd;
  }
}

Operand *Riscv64CGFunc::SelectRound(TypeCvtNode *node, Operand *opnd0) {
  if (CLANG) {
    // Generate C style roundf() and round() calls
    PrimType ftype = node->fromPrimType;
    PrimType rtype = node->primType;
    vector<Operand *> opndvec;
    Riscv64RegOperand *physOpnd;
    if (ftype == PTY_f64) {
      physOpnd = GetOrCreatePhysicalRegisterOperand(D0, 64, kRegTyFloat);
    } else {
      physOpnd = GetOrCreatePhysicalRegisterOperand(S0, 32, kRegTyFloat);
    }
    Operand *resopnd = static_cast<Operand *>(physOpnd);
    opndvec.push_back(resopnd);
    opnd0 = LoadIntoRegister(opnd0, ftype);
    opndvec.push_back(opnd0);
    if (ftype == PTY_f32) {
      SelectLibCall("roundf", opndvec, ftype, rtype);
    } else {
      SelectLibCall("round", opndvec, ftype, rtype);
    }
    return resopnd;
  }
  PrimType ftype = node->fromPrimType;
  PrimType itype = node->primType;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  RegOperand *resopnd = CreateRegisterOperandOfType(itype);
  opnd0 = LoadIntoRegister(opnd0, ftype);
  //curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(is64bits ? : , resopnd, opnd0));
  CHECK_FATAL(0, "NYI none C path");
  return resopnd;
}

Operand *Riscv64CGFunc::SelectTrunc(TypeCvtNode *node, Operand *opnd0) {
  PrimType ftype = node->fromPrimType;
  bool is64bits = (GetPrimTypeBitSize(node->primType) == 64);
  PrimType itype = (is64bits) ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                              : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(itype);
  SelectCvtFloat2Int(resopnd, opnd0, itype, ftype);
  return resopnd;
}

void Riscv64CGFunc::SelectSelect(Operand *resopnd, Operand *condopnd, Operand *trueopnd, Operand *falseopnd,
                                 PrimType dtyp, PrimType ctyp) {
  CG_ASSERT(resopnd != condopnd, "resopnd cannot be the same as condopnd");
  condopnd = LoadIntoRegister(condopnd, ctyp);
  trueopnd = LoadIntoRegister(trueopnd, dtyp);
  falseopnd = LoadIntoRegister(falseopnd, dtyp);

  bool isIntty = IsPrimitiveInteger(dtyp);
  uint32 dsize = GetPrimTypeBitSize(dtyp);
  bool isUnsignedInt = isIntty && !IsSignedInteger(dtyp);

  Riscv64RegOperand *condopnd2 = Riscv64RegOperand::GetZeroRegister(GetPrimTypeBitSize(ctyp));
  SelectRiscv64SelectOp(resopnd, condopnd, condopnd2, falseopnd, trueopnd, OP_eq, dsize, isUnsignedInt);
}

Operand *Riscv64CGFunc::SelectSelect(TernaryNode *node, Operand *opnd0, Operand *opnd1, Operand *opnd2) {
  PrimType dtyp = node->primType;
  PrimType ctyp = node->Opnd(0)->primType;
  RegOperand *resopnd = CreateRegisterOperandOfType(dtyp);
  SelectSelect(resopnd, opnd0, opnd1, opnd2, dtyp, ctyp);
  return resopnd;
}

void Riscv64CGFunc::SelectRiscv64SelectOp(Operand *dest, Operand *opnd0, Operand *opnd1, Operand *trueOpnd, Operand *falseOpnd,
                                          Opcode opcode, uint32 dsize, bool unsignedInt) {
    /*
     *   br (false cond) -> else
     *     dest = 1
     *     jump to done
     * else:
     *     dest = 0
     * done:
     */
    MOperator mopcode = 0;
    RegOperand *fcmpResult = nullptr;
    if (static_cast<RegOperand*>(opnd0)->IsOfIntClass()) {
      if (opnd0->op_kind_ == Operand::Opd_Register) {
        if (dsize == 32) {
          unsignedInt ?
              GenerateZext(opnd0, opnd0, PTY_u64, 32)
            : GenerateSext(opnd0, opnd0, PTY_i64, 32);
        }
      }
      if (opnd1->op_kind_ == Operand::Opd_Immediate) {
        // load into register
        PrimType primType = (dsize == 64) ? PTY_i64 : PTY_i32;
        opnd1 = SelectCopy(opnd1, primType, primType);
      } else if (opnd1->op_kind_ == Operand::Opd_Register) {
        if (dsize == 32) {
          unsignedInt ?
              GenerateZext(opnd1, opnd1, PTY_u64, 32)
            : GenerateSext(opnd1, opnd1, PTY_i64, 32);
        }
      }
      switch (opcode) {
      case OP_eq:
        mopcode = MOP_bne;
        break;
      case OP_ne:
        mopcode = MOP_beq;
        break;
      case OP_gt:
        mopcode = unsignedInt ? MOP_bls : MOP_ble;
        break;
      case OP_lt:
        mopcode = unsignedInt ? MOP_bhs : MOP_bge;
        break;
      case OP_ge:
        mopcode = unsignedInt ? MOP_blo : MOP_blt;
        break;
      case OP_le:
        mopcode = unsignedInt ? MOP_bhi : MOP_bgt;
        break;
      default:
        CG_ASSERT(false, "illegal logical operator");
      }
    } else {
      if (opnd1->op_kind_ == Operand::Opd_FPZeroImmediate) {
        PrimType primType = (dsize == 64) ? PTY_f64 : PTY_f32;
        opnd1 = SelectCopy(opnd1, primType, primType);
      }
      MOperator cmpop = 0;
      mopcode = MOP_beq;
      switch (opcode) {
      case OP_eq:
        cmpop = (dsize == 32) ? MOP_scmpeqrr : MOP_dcmpeqrr;
        break;
      case OP_ne:
        cmpop = (dsize == 32) ? MOP_scmpeqrr : MOP_dcmpeqrr;
        mopcode = MOP_bne;
        break;
      case OP_gt:
        cmpop = (dsize == 32) ? MOP_scmpgtrr : MOP_dcmpgtrr;
        break;
      case OP_lt:
        cmpop = (dsize == 32) ? MOP_scmpltrr : MOP_dcmpltrr;
        break;
      case OP_ge:
        cmpop = (dsize == 32) ? MOP_scmpgerr : MOP_dcmpgerr;
        break;
      case OP_le:
        cmpop = (dsize == 32) ? MOP_scmplerr : MOP_dcmplerr;
        break;
      default:
        CG_ASSERT(false, "illegal logical operator");
      }
      fcmpResult = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8u));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(cmpop, fcmpResult, opnd0, opnd1));
    }

    BB *ifbb = CreateNewBB();
    curbb->AppendBB(ifbb);
    BB *elsebb = CreateNewBB();
    ifbb->AppendBB(elsebb);
    BB *donebb = CreateNewBB();
    elsebb->AppendBB(donebb);

    LabelIdx elselabidx = CreateLabel();
    elsebb->AddLabel(elselabidx);
    lab2bbmap[elselabidx] = elsebb;
    LabelOperand *elseTargOpnd = GetOrCreateLabelOperand(elselabidx);

    LabelIdx donelabidx = CreateLabel();
    lab2bbmap[donelabidx] = donebb;
    donebb->AddLabel(donelabidx);
    LabelOperand *doneTargOpnd = GetOrCreateLabelOperand(donelabidx);

    curbb->SetKind(BB::kBBIf);
    ifbb->SetKind(BB::kBBGoto);

    //SelectRiscv64CSet(dest, GetCondOperand(cc), (dsize == 64));
    if (fcmpResult) {
      Operand *zero = GetOrCreatePhysicalRegisterOperand(RZERO, 64, kRegTyInt);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopcode, fcmpResult, zero, elseTargOpnd));
    } else {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mopcode, opnd0, opnd1, elseTargOpnd));
    }
    MOperator mop;
    if (trueOpnd->op_kind_ == Operand::Opd_Immediate) {
      ifbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, dest, trueOpnd));
    } else {
      mop = (static_cast<RegOperand*>(trueOpnd)->IsOfIntClass()) ?
                      ((dsize == 32) ? MOP_wmovrrr : MOP_xmovrrr) :
                      ((dsize == 32) ? MOP_xvmovss : MOP_xvmovdd);
      ifbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, dest, trueOpnd, falseOpnd));
    }
    if (falseOpnd->op_kind_ == Operand::Opd_Immediate) {
      elsebb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, dest, falseOpnd));
    } else {
      mop = (static_cast<RegOperand*>(falseOpnd)->IsOfIntClass()) ?
                      ((dsize == 32) ? MOP_wmovrrr : MOP_xmovrrr) :
                      ((dsize == 32) ? MOP_xvmovss : MOP_xvmovdd);
      elsebb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, dest, falseOpnd, trueOpnd));
    }
    ifbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xuncond, doneTargOpnd));

    curbb = donebb;
}

void Riscv64CGFunc::SelectRangegoto(RangegotoNode *rangegotonode, Operand *opnd0) {
  SmallCaseVector &switchtable = rangegotonode->rangegotoTable;
  MIRType *etype = GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_a64);
  /*
   * we store 8-byte displacement ( jump_label - offset_table_address )
   * in the table. Refer to Riscv64Emit::Emit() in riscv64_emit.cpp
   */
  vector<uint32> sizeArray;
  sizeArray.push_back(switchtable.size());
  MIRArrayType *arraytype = memPool->New<MIRArrayType>(etype->tyIdx, sizeArray);
  MIRAggConst *arrayconst = memPool->New<MIRAggConst>(&mirModule, arraytype);
  for (uint32 i = 0; i < switchtable.size(); i++) {
    LabelIdx lidx = switchtable[i].second;
    MIRConst *mirconst = memPool->New<MIRLblConst>(lidx, func->puIdx, etype);
    arrayconst->constVec.push_back(mirconst);
  }

  MIRSymbol *lblst = func->symTab->CreateSymbol(kScopeLocal);
  lblst->storageClass = kScFstatic;
  lblst->sKind = kStConst;
  lblst->SetTyIdx(arraytype->tyIdx);
  lblst->SetConst(arrayconst);
  std::string lblstr(".LB_");
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  lblstr.append(funcSt->GetName()).append(to_string(labelIdx++));
  lblst->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lblstr));
  emitstvec_.push_back(lblst);

  PrimType ityp = rangegotonode->uOpnd->primType;
  opnd0 = LoadIntoRegister(opnd0, ityp);

  regno_t vRegNo = New_V_Reg(kRegTyInt, 8u);
  RegOperand *addopnd = CreateVirtualRegisterOperand(vRegNo);

  int32 minidx = switchtable[0].first;
  SelectAdd(addopnd, opnd0, CreateImmOperand(-minidx - rangegotonode->tagOffset, GetPrimTypeBitSize(ityp), true), ityp);

  // contains the index
  if (addopnd->GetSize() != GetPrimTypeBitSize(PTY_u64)) {
    addopnd = static_cast<Riscv64RegOperand *>(SelectCopy(addopnd, PTY_u64, PTY_u64));
  }

  RegOperand *baseopnd = CreateRegisterOperandOfType(PTY_u64);
  StImmOperand *switchTable = CreateStImmOperand(lblst, 0, 0);

  // load the address of the switch table
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_adrp, baseopnd, switchTable));
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_adrpl12, baseopnd, baseopnd, switchTable));

  // Compute the address of the load
  RegOperand *sllDst = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, sllDst, addopnd, CreateImmOperand(3, 4, false)));
  RegOperand *addDst = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, addDst, baseopnd, sllDst));
  Riscv64OfstOperand *immOffset = CreateOfstOpnd(0, 32);
  Operand *disp = memPool->New<Riscv64MemOperand>(64, addDst, nullptr, immOffset, nullptr);
  RegOperand *tgt = CreateRegisterOperandOfType(PTY_a64);
  SelectAdd(tgt, baseopnd, disp, PTY_u64);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xbr, tgt));
}

Operand *Riscv64CGFunc::SelectAlloca(UnaryNode *node, Operand *opnd0) {
  hasAlloca = true;
  PrimType rettype = node->primType;
  CG_ASSERT((rettype == PTY_a64), "wrong type");
  PrimType stype = node->Opnd(0)->primType;
  Operand *resopnd = opnd0;
  if (GetPrimTypeBitSize(stype) < GetPrimTypeBitSize(PTY_u64)) {
    resopnd = CreateRegisterOperandOfType(PTY_u64);
    SelectCvtInt2Int(nullptr, resopnd, opnd0, stype, PTY_u64);
  }

  RegOperand *aliop = CreateRegisterOperandOfType(PTY_u64);

  SelectAdd(aliop, resopnd, CreateImmOperand(RISCV64_STACK_PTR_ALIGNMENT - 1, 64, true), PTY_u64);
  Operand *shifopnd = CreateImmOperand(__builtin_ctz(RISCV64_STACK_PTR_ALIGNMENT), 64, true);
  SelectShift(aliop, aliop, shifopnd, kShiftLright, PTY_u64);
  SelectShift(aliop, aliop, shifopnd, kShiftLeft, PTY_u64);
  Operand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
  SelectSub(spOpnd, spOpnd, aliop, PTY_u64);
  int64 argsToStkpassSize = memlayout->SizeOfArgsToStackpass();
  if (argsToStkpassSize > 0) {
    RegOperand *resallo = CreateRegisterOperandOfType(PTY_u64);
    SelectAdd(resallo, spOpnd, CreateImmOperand(argsToStkpassSize, 64, true), PTY_u64);
    return resallo;
  } else {
    return SelectCopy(spOpnd, PTY_u64, PTY_u64);
  }
}

Operand *Riscv64CGFunc::SelectMalloc(UnaryNode *node, Operand *opnd0) {
  PrimType rettype = node->primType;
  CG_ASSERT((rettype == PTY_a64), "wrong type");

  vector<Operand *> opndvec;
  RegOperand *resopnd = CreateRegisterOperandOfType(rettype);
  opndvec.push_back(resopnd);
  opndvec.push_back(opnd0);
  // Use calloc to make sure allocated memory is zero-initialized
  const char *funcName;
  funcName = "calloc";
  Operand *opnd1;
  bool size32 = (opnd0->GetSize() <= 32);
  if (size32) {
    opnd1 = CreateImmOperand(1, PTY_u32, false);
  } else {
    opnd1 = CreateImmOperand(1, PTY_u64, false);
  }
  opndvec.push_back(opnd1);
  if (size32) {
    SelectLibCall(funcName, opndvec, PTY_u32, rettype);
  } else {
    SelectLibCall(funcName, opndvec, PTY_u64, rettype);
  }
  return resopnd;
}

Operand *Riscv64CGFunc::SelectGCMalloc(GCMallocNode *node) {
  PrimType rettype = node->primType;
  CG_ASSERT((rettype == PTY_a64), "wrong type");

  // Get the size and alignment of the type.
  TyIdx tyIdx = node->tyIdx;
  uint64_t size = becommon.type_size_table.at(tyIdx.GetIdx());
  uint8_t align = Riscv64RTSupport::kObjectAlignment;

  // Generate the call to MCC_NewObj
  Operand *opndSize = CreateImmOperand(size, 64, false);
  Operand *opndAlign = CreateImmOperand(align, 64, false);

  RegOperand *resopnd = CreateRegisterOperandOfType(rettype);

  vector<Operand *> opndvec{ resopnd, opndSize, opndAlign };

  const char *funcName = strdup(GetIntrinsicFuncName(INTRN_MCCNewObj).c_str());
  SelectLibCall(funcName, opndvec, PTY_u64, rettype);

  return resopnd;
}

Operand *Riscv64CGFunc::SelectJarrayMalloc(JarrayMallocNode *node, Operand *opnd0) {
  PrimType rettype = node->primType;
  CG_ASSERT((rettype == PTY_a64), "wrong type");

  // Extract jarray type
  TyIdx tyIdx = node->tyIdx;
  MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  auto jarytype = dynamic_cast<MIRJarrayType *>(type);
  if (jarytype == nullptr) {
    FATAL(kLncFatal, "dynamic cast is nullptr");
  }
  CG_ASSERT((jarytype != nullptr), "Type param of gcmallocjarray is not a MIRJarrayType");

  uint64_t fixedSize = Riscv64RTSupport::kArrayContentOffset;
  uint8_t align = Riscv64RTSupport::kObjectAlignment;

  MIRType *elemType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(jarytype->elemTyIdx);
  PrimType elemPrimtype = elemType->GetPrimType();
  uint64_t elemSize = GetPrimTypeSize(elemPrimtype);

  // Generate the cal to MCC_NewObj_flexible
  Operand *opndFixedSize = CreateImmOperand(PTY_u64, fixedSize);
  Operand *opndElemSize = CreateImmOperand(PTY_u64, elemSize);

  Operand *opndNElems = opnd0;

  Operand *opndNElems64 = static_cast<Operand *>(CreateRegisterOperandOfType(PTY_u64));
  SelectCvtInt2Int(nullptr, opndNElems64, opndNElems, PTY_u32, PTY_u64);

  Operand *opndAlign = CreateImmOperand(PTY_u64, align);

  RegOperand *resopnd = CreateRegisterOperandOfType(rettype);

  vector<Operand *> opndvec{ resopnd, opndFixedSize, opndElemSize, opndNElems64, opndAlign };

  const char *funcName = strdup(GetIntrinsicFuncName(INTRN_MCCNewObjFlexibleCname).c_str());
  SelectLibCall(funcName, opndvec, PTY_u64, rettype);

  // Generate the store of the object length field
  MemOperand *opndArrayLengthField = CreateMemOpnd(resopnd, Riscv64RTSupport::kArrayLengthOffset, 4);
  RegOperand *regopndNElems = SelectCopy(opndNElems, PTY_u32, PTY_u32);
  SelectCopy(opndArrayLengthField, PTY_u32, regopndNElems, PTY_u32);

  return resopnd;
}

}  // namespace maplebe
