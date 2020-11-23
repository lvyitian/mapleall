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

#include "aarch64_cg_func.h"
#include "aarch64_cg.h"
#include "aarch64_rt_support.h"
#include "opcode_info.h"  // mapleir/include/opcode_info.h
#include "cg_assert.h"
#include "special_func.h"
#include <iostream>
#include <typeinfo>

namespace maplebe {

using namespace maple;

#define CLANG  (mirModule.IsCModule())

// Returns the number of leading 0-bits in x, starting at the most significant bit position. If x is 0, the result is
// -1.
static int GetHead0BitNum(int64 val) {
  int bitNum = 0;
  for (; bitNum < 64; bitNum++) {
    if ((0x8000000000000000 >> bitNum) & val) {
      break;
    }
  }
  if (bitNum == 64) {
    bitNum = -1;
  }
  return bitNum;
}

// Returns the number of trailing 0-bits in x, starting at the least significant bit position. If x is 0, the result is
// -1.
static int GetTail0BitNum(int64 val) {
  int bitNum = 0;
  for (; bitNum < 64; bitNum++) {
    if ((static_cast<uint64>(1) << bitNum) & val) {
      break;
    }
  }
  if (bitNum == 64) {
    bitNum = -1;
  }
  return bitNum;
}

MOperator AArch64CGFunc::PickAddInsn(PrimType primtype, bool ismem) {
  CG_ASSERT(false, "NYI PickAddInsn");
  return MOP_undef;
}

MOperator AArch64CGFunc::PickMpyInsn(PrimType primtype, bool ismem) {
  CG_ASSERT(false, "NYI PickAddInsn");
  return MOP_undef;
}

MOperator AArch64CGFunc::PickJmpInsn(Opcode brop, Opcode cmpop, bool isfloat, bool issigned) {
  switch (cmpop) {
    default:
      CG_ASSERT(false, "PickJmpInsn error");
    case OP_ne:
      return static_cast<uint32>(brop == OP_brtrue ? MOP_bne : MOP_beq);
    case OP_eq:
      return static_cast<uint32>(brop == OP_brtrue ? MOP_beq : MOP_bne);
    case OP_lt:
      return static_cast<uint32>(brop == OP_brtrue ? (issigned ? MOP_blt : MOP_blo)
                                        : (isfloat ? MOP_bpl : (issigned ? MOP_bge : MOP_bhs)));
    case OP_le:
      return static_cast<uint32>(brop == OP_brtrue ? (issigned ? MOP_ble : MOP_bls)
                                        : (isfloat ? MOP_bhi : (issigned ? MOP_bgt : MOP_bhi)));
    case OP_gt:
      return static_cast<uint32>(brop == OP_brtrue ? (isfloat ? MOP_bhi : (issigned ? MOP_bgt : MOP_bhi))
                                        : (issigned ? MOP_ble : MOP_bls));
    case OP_ge:
      return static_cast<uint32>(brop == OP_brtrue ? (isfloat ? MOP_bpl : (issigned ? MOP_bge : MOP_bhs))
                                        : (issigned ? MOP_blt : MOP_blo));
  }
}

void AArch64CGFunc::SelectCondGoto(LabelOperand *targetopnd, Opcode jmpop, Opcode cmpop, Operand *opnd0, Operand *opnd1,
                                   PrimType primType, bool signedCond) {
  CG_ASSERT(targetopnd != nullptr, "no branch target");

  //if (opnd0->IsIntImmediate() && static_cast<AArch64ImmOperand *>(opnd0)->IsZero() &&
  //    (!opnd1->IsIntImmediate() || (opnd1->IsIntImmediate() && static_cast<AArch64ImmOperand *>(opnd1)->IsZero()))) {
  //  if (opnd0->GetSize() <= 32) {
  //    opnd0 = &AArch64RegOperand::Get32bitZeroRegister();
  //  } else {
  //    opnd0 = &AArch64RegOperand::Get64bitZeroRegister();
  //  }
  //} else {
    opnd0 = LoadIntoRegister(opnd0, primType);
  //}

  bool is64bits = GetPrimTypeBitSize(primType) == 64;
  bool isfloat = IsPrimitiveFloat(primType);
  Operand *rflag = GetOrCreateRflag();
  if (isfloat) {
    opnd1 = LoadIntoRegister(opnd1, primType);
    MOperator mop = is64bits ? MOP_dcmperr : (GetPrimTypeBitSize(primType) == 32 ? MOP_scmperr : MOP_hcmperr);
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, rflag, opnd0, opnd1));
  } else {
    bool isimm = opnd1->op_kind_ == Operand::Opd_Immediate;
    if (opnd1->op_kind_ != Operand::Opd_Register && !isimm) {
      opnd1 = SelectCopy(opnd1, primType, primType);
    }
    MOperator mop = is64bits ? MOP_xcmprr : MOP_wcmprr;

    if (isimm) {
      // Special cases, i.e., comparing with zero
      // Do not perform optimization for C, unlike Java which has no unsigned int.
      if (static_cast<AArch64ImmOperand *>(opnd1)->IsZero() && (g->optim_level >= 2) &&
          ((mirModule.IsJavaModule()) ||
           ((primType != PTY_u64) && (primType != PTY_u32)))) {
        ImmOperand *signBit = nullptr;
        switch (cmpop) {
          case OP_ne:
            if (jmpop == OP_brtrue) {
              curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xcbnz : MOP_wcbnz, opnd0, targetopnd));
            } else {
              curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xcbz : MOP_wcbz, opnd0, targetopnd));
            }
            return;
          case OP_eq:
            if (jmpop == OP_brtrue) {
              curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xcbz : MOP_wcbz, opnd0, targetopnd));
            } else {
              curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xcbnz : MOP_wcbnz, opnd0, targetopnd));
            }
            return;
          // TBZ/TBNZ instruction have a range of +/-32KB, need to check if the jump target is reachable in a later
          // phase. If the branch target is not reachable, then we change tbz/tbnz into combination of ubfx and
          // cbz/cbnz, which will clobber one extra register. With LSRA under O2, we can use of the reserved registers
          // for that purpose.
          case OP_lt:
            signBit = CreateImmOperand(is64bits ? 63 : 31, 8, false);
            if (jmpop == OP_brtrue) {
              curbb->AppendInsn(
                cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xtbnz : MOP_wtbnz, opnd0, signBit, targetopnd));
            } else {
              curbb->AppendInsn(
                cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xtbz : MOP_wtbz, opnd0, signBit, targetopnd));
            }
            return;
          case OP_ge:
            signBit = CreateImmOperand(is64bits ? 63 : 31, 8, false);
            if (jmpop == OP_brtrue) {
              curbb->AppendInsn(
                cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xtbz : MOP_wtbz, opnd0, signBit, targetopnd));
            } else {
              curbb->AppendInsn(
                cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xtbnz : MOP_wtbnz, opnd0, signBit, targetopnd));
            }
            return;

          default:
            break;
        }
      }

      // aarch64 assembly takes up to 24-bits immediate, generating
      // either cmp or cmp with shift 12 encoding
      ImmOperand *immopnd = static_cast<ImmOperand *>(opnd1);
      if (immopnd->IsInBitSize(12) || immopnd->IsInBitSize(12, 12)) {
        mop = is64bits ? MOP_xcmpri : MOP_wcmpri;
      } else {
        opnd1 = SelectCopy(opnd1, primType, primType);
      }
    }
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, rflag, opnd0, opnd1));
  }

  bool issigned = IsPrimitiveInteger(primType) ? IsSignedInteger(primType) : (signedCond ? true : false);
  MOperator jmpOp = PickJmpInsn(jmpop, cmpop, isfloat, issigned);
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(jmpOp, rflag, targetopnd));
}

/*  brtrue @label0 (ge u8 i32 (
    cmp i32 i64 (dread i64 %Reg2_J, dread i64 %Reg4_J),
    constVal i32 0))
   ===>
    cmp r1, r2
    bge Cond, label0
 */
void AArch64CGFunc::SelectCondSpecial(CondGotoNode *stmt, BaseNode *expr) {
  CG_ASSERT((expr->op == OP_cmp && "unexpect opcode"), "");
  Operand *opnd0 = HandleExpr(expr, expr->Opnd(0));
  Operand *opnd1 = HandleExpr(expr, expr->Opnd(1));
  CompareNode *node = static_cast<CompareNode *>(expr);
  bool isfloat = IsPrimitiveFloat(node->opndType);
  opnd0 = LoadIntoRegister(opnd0, node->opndType);
  // most of FP constants are passed as AArch64MemOperand
  // except 0.0 which is passed as Opd_FPZeroImmediate
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  if (opnd1ty != Operand::Opd_Immediate && opnd1ty != Operand::Opd_FPZeroImmediate) {
    opnd1 = LoadIntoRegister(opnd1, node->opndType);
  }
  SelectAArch64Cmp(opnd0, opnd1, !isfloat, GetPrimTypeBitSize(node->opndType));
  // handle condgoto now.
  LabelIdx labelIdx = stmt->offset;
  BaseNode *condnode = stmt->Opnd(0);
  LabelOperand *targetopnd = GetOrCreateLabelOperand(labelIdx);
  Opcode cmpop = condnode->op;
  PrimType primType = static_cast<CompareNode *>(condnode)->opndType;
  isfloat = IsPrimitiveFloat(primType);
  Operand *rflag = GetOrCreateRflag();
  bool issigned = IsPrimitiveInteger(primType) ? IsSignedInteger(primType) : (IsSignedInteger(condnode->primType) ? true : false);
  MOperator jmpOp = PickJmpInsn(stmt->op, cmpop, isfloat, issigned);
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(jmpOp, rflag, targetopnd));
}

void AArch64CGFunc::SelectCondGoto(CondGotoNode *stmt, Operand *opnd0, Operand *opnd1) {
  // handle brfalse/brtrue op, opnd0 can be a compare node or non-compare node
  // such as a dread for example
  LabelIdx labelIdx = stmt->offset;
  BaseNode *condnode = stmt->Opnd(0);
  LabelOperand *targetopnd = GetOrCreateLabelOperand(labelIdx);
  Opcode cmpop;

  if (opnd0->IsRegister() && static_cast<RegOperand *>(opnd0)->GetValidBitsNum() == 1 && condnode->op == OP_lior) {
    ImmOperand *condBit = CreateImmOperand(0, 8, false);
    if (stmt->op == OP_brtrue) {
      curbb->AppendInsn(
        cg->BuildInstruction<AArch64Insn>(MOP_wtbnz, static_cast<RegOperand *>(opnd0), condBit, targetopnd));
    } else {
      curbb->AppendInsn(
        cg->BuildInstruction<AArch64Insn>(MOP_wtbz, static_cast<RegOperand *>(opnd0), condBit, targetopnd));
    }
    return;
  }

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

void AArch64CGFunc::SelectGoto(GotoNode *stmt) {
  Operand *targetopnd = GetOrCreateLabelOperand(stmt->offset);
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_xuncond, targetopnd));
}

Operand *AArch64CGFunc::SelectAdd(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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
    MOperator mop = MOP_undef;
    if (IsUnsignedInteger(dtype)) {
      switch (dtype) {
      case PTY_u8:
        mop = MOP_xuxtb32;
        break;
      case PTY_u16:
        mop = MOP_xuxth32;
        break;
      case PTY_u32:
        mop = MOP_xuxtw64;
        break;
      default:
        // no extend.
        break;
      }
    }
    if (doExtend && mop != MOP_undef) {
      RegOperand *tmpopnd = CreateRegisterOperandOfType(dtype);
      SelectAdd(tmpopnd, opnd0, opnd1, prmtype);
      curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, tmpopnd));
    } else {
      SelectAdd(resopnd, opnd0, opnd1, prmtype);
    }
  } else {
    prmtype = dtype;
    SelectVecAdd(resopnd, opnd0, opnd1, prmtype);
  }
  return resopnd;
}

void AArch64CGFunc::SelectAdd(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectAdd(resopnd, SelectCopy(opnd0, prmtype, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      AArch64ImmOperand *immopnd = static_cast<AArch64ImmOperand *>(opnd1);
      if (immopnd->IsNegative()) {
        immopnd->Negate();
        SelectSub(resopnd, opnd0, immopnd, prmtype);
      } else {
        if (immopnd->IsInBitSize(24)) {
          // ADD Wd|WSP, Wn|WSP, #imm{, shift} ; 32-bit general registers
          // ADD Xd|SP,  Xn|SP,  #imm{, shift} ; 64-bit general registers
          // imm : 0 ~ 4095, shift: none, LSL #0, or LSL #12
          // aarch64 assembly takes up to 24-bits, if the lower 12 bits is all 0
          if (!(immopnd->IsInBitSize(12) || immopnd->IsInBitSize(12, 12))) {
            ImmOperand *immopnd2 =
              CreateImmOperand(immopnd->GetValue() >> 12, immopnd->size_, immopnd->IsSignedValue());
            curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xaddrri24 : MOP_waddrri24, resopnd,
                                                                opnd0, immopnd2, &addSubLslOperand));
            immopnd->ModuloByPow2(12);
            opnd0 = resopnd;
          }
          curbb->AppendInsn(
            cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xaddrri12 : MOP_waddrri12, resopnd, opnd0, immopnd));
        } else {
          // load into register
          int64 immVal = immopnd->GetValue();
          int tail0bitNum = GetTail0BitNum(immVal);
          int head0bitNum = GetHead0BitNum(immVal);
          int bitnum = 64 - head0bitNum - tail0bitNum;
          RegOperand *regopnd;
          if (isAfterRegAlloc) {
            CHECK_FATAL(curbb->isProEpilog, "Not prolog/epilog");

            RegType regty = GetRegTyFromPrimTyAarch64(prmtype);
            uint8 bytelen = GetPrimTypeSize(prmtype);
            regopnd = GetOrCreatePhysicalRegisterOperand((AArch64reg_t)(R16), bytelen, regty);
          } else {
            regopnd = CreateRegisterOperandOfType(prmtype);
          }

          if (bitnum <= 16) {
            int64 newImm = (static_cast<uint64>(immVal) >> static_cast<unsigned int>(tail0bitNum)) & 0xFFFF;
            AArch64ImmOperand *immOpnd1 = CreateImmOperand(newImm, 16, false);
            SelectCopyImm(regopnd, immOpnd1, prmtype);
            uint32 mopBadd = is64bits ? MOP_xaddrrrs : MOP_waddrrrs;
            curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(
              mopBadd, resopnd, opnd0, regopnd,
              CreateBitShiftOperand(BitShiftOperand::LSL, tail0bitNum, is64bits ? 6 : 5)));
          } else {
            SelectCopyImm(regopnd, immopnd, prmtype);
            curbb->AppendInsn(
              cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xaddrrr : MOP_waddrrr, resopnd, opnd0, regopnd));
          }
        }
      }
    } else {
      SelectAdd(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectAdd(resopnd, opnd1, opnd0, prmtype);  // commutative
  } else {
    if (IsPrimitiveFloat(prmtype)) {
      curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_dadd : MOP_sadd, resopnd, opnd0, opnd1));
    } else if (IsPrimitiveInteger(prmtype)) {
      curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xaddrrr : MOP_waddrrr, resopnd, opnd0, opnd1));
    } else {
      CG_ASSERT(false, "NYI add");
    }
  }
}

Operand *AArch64CGFunc::SelectAddroflabel(AddroflabelNode *expr) {
  // adrp reg, label-id
  regno_t vRegNo = New_V_Reg(kRegTyInt, expr->SizeOfInstr());
  Operand *dst = CreateVirtualRegisterOperand(vRegNo);
  Operand *immOpnd = CreateImmOperand(expr->offset, 64, false);
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_adrp_label, dst, immOpnd));
  return dst;
}

Operand *AArch64CGFunc::SelectCGArrayElemAdd(BinaryNode *node) {
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

void AArch64CGFunc::SelectSub(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(prmtype);
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  if (opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      AArch64ImmOperand *immopnd = static_cast<AArch64ImmOperand *>(opnd1);
      if (immopnd->IsNegative()) {
        immopnd->Negate();
        SelectAdd(resopnd, opnd0, immopnd, prmtype);
      } else {
        if (immopnd->IsInBitSize(24)) {
          // SUB Wd|WSP, Wn|WSP, #imm{, shift} ; 32-bit general registers
          // SUB Xd|SP,  Xn|SP,  #imm{, shift} ; 64-bit general registers
          // imm : 0 ~ 4095, shift: none, LSL #0, or LSL #12
          // aarch64 assembly takes up to 24-bits, if the lower 12 bits is all 0
          if (!(immopnd->IsInBitSize(12) || immopnd->IsInBitSize(12, 12))) {
            ImmOperand *immopnd2 =
              CreateImmOperand(immopnd->GetValue() >> 12, immopnd->size_, immopnd->IsSignedValue());
            curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xsubrri24 : MOP_wsubrri24, resopnd,
                                                                opnd0, immopnd2, &addSubLslOperand));
            immopnd->ModuloByPow2(12);
            opnd0 = resopnd;
          }
          curbb->AppendInsn(
            cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xsubrri12 : MOP_wsubrri12, resopnd, opnd0, immopnd));
        } else {
          // load into register
          int64 immVal = immopnd->GetValue();
          int tail0bitNum = GetTail0BitNum(immVal);
          int head0bitNum = GetHead0BitNum(immVal);
          int bitnum = 64 - head0bitNum - tail0bitNum;
          RegOperand *regopnd;
          if (isAfterRegAlloc) {
            CHECK_FATAL(curbb->isProEpilog, "Not prolog/epilog");

            RegType regty = GetRegTyFromPrimTyAarch64(prmtype);
            uint8 bytelen = GetPrimTypeSize(prmtype);
            regopnd = GetOrCreatePhysicalRegisterOperand((AArch64reg_t)(R16), bytelen, regty);
          } else {
            regopnd = CreateRegisterOperandOfType(prmtype);
          }

          if (bitnum <= 16) {
            int64 newImm = (static_cast<uint64>(immVal) >> static_cast<unsigned int>(tail0bitNum)) & 0xFFFF;
            AArch64ImmOperand *immOpnd1 = CreateImmOperand(newImm, 16, false);
            SelectCopyImm(regopnd, immOpnd1, prmtype);
            uint32 mopBsub = is64bits ? MOP_xsubrrrs : MOP_wsubrrrs;
            curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(
              mopBsub, resopnd, opnd0, regopnd,
              CreateBitShiftOperand(BitShiftOperand::LSL, tail0bitNum, is64bits ? 6 : 5)));
          } else {
            SelectCopyImm(regopnd, immopnd, prmtype);
            curbb->AppendInsn(
              cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xsubrrr : MOP_wsubrrr, resopnd, opnd0, regopnd));
          }
        }
      }
    } else {
      SelectSub(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
    }
  } else {
    if (isfloat) {
      curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>((is64bits ? MOP_dsub : MOP_ssub), resopnd, opnd0, opnd1));
    } else {
      curbb->AppendInsn(
        cg->BuildInstruction<AArch64Insn>((is64bits ? MOP_xsubrrr : MOP_wsubrrr), resopnd, opnd0, opnd1));
    }
  }
}

Operand *AArch64CGFunc::SelectSub(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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
  MOperator mop = MOP_undef;
  if (IsUnsignedInteger(dtype)) {
    switch (dtype) {
    case PTY_u8:
      mop = MOP_xuxtb32;
      break;
    case PTY_u16:
      mop = MOP_xuxth32;
      break;
    case PTY_u32:
      mop = MOP_xuxtw64;
      break;
    default:
      // no extend.
      break;
    }
  }
  if (doExtend && mop != MOP_undef) {
    RegOperand *tmpopnd = CreateRegisterOperandOfType(dtype);
    SelectSub(tmpopnd, opnd0, opnd1, prmtype);
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, tmpopnd));
  } else {
    SelectSub(resopnd, opnd0, opnd1, prmtype);
  }
  return resopnd;
}

Operand *AArch64CGFunc::SelectMpy(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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
  MOperator mop = MOP_undef;
  if (IsUnsignedInteger(dtype)) {
    switch (dtype) {
    case PTY_u8:
      mop = MOP_xuxtb32;
      break;
    case PTY_u16:
      mop = MOP_xuxth32;
      break;
    case PTY_u32:
      mop = MOP_xuxtw64;
      break;
    default:
      // no extend.
      break;
    }
  }
  if (doExtend && mop != MOP_undef) {
    RegOperand *tmpopnd = CreateRegisterOperandOfType(dtype);
    SelectMpy(tmpopnd, opnd0, opnd1, prmtype);
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, tmpopnd));
  } else {
    SelectMpy(resopnd, opnd0, opnd1, prmtype);
  }
  return resopnd;
}

void AArch64CGFunc::SelectMpy(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
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
        otherop = SelectCopy(otherop, prmtype, prmtype);
      }
      AArch64ImmOperand *shiftnum = CreateImmOperand(__builtin_ffsll(immValue) - 1, dsize, false);
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
        AArch64ImmOperand *shiftnum = CreateImmOperand(__builtin_ffsll(headval + 1) - 1, dsize, false);
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
        AArch64ImmOperand *shiftnum = CreateImmOperand(__builtin_ffsll(headval - 1) - 1, dsize, false);
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
      curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xvmuld : MOP_xvmuls, resopnd, opnd0, opnd1));
    } else if (IsPrimitiveInteger(prmtype)) {
      if (is64bits) {
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_xmulrrr, resopnd, opnd0, opnd1));
      } else {
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_wmulrrr, resopnd, opnd0, opnd1));
      }
    }
  }
}

void AArch64CGFunc::SelectDiv(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);

  if (g->optim_level >= 2) {
    if (opnd1ty == Operand::Opd_Immediate && IsSignedInteger(prmtype)) {
      ImmOperand *imm = static_cast<ImmOperand *>(opnd1);
      int64 immValue = llabs(imm->GetValue());
      if (immValue != 0 && (immValue & (immValue - 1)) == 0) {
        if (immValue == 1) {
          if (imm->GetValue() > 0) {
            uint32 mop = is64bits ? MOP_xmovrr : MOP_wmovrr;
            curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, opnd0));
          } else {
            SelectNeg(resopnd, opnd0, prmtype);
          }

          return;
        }
        int shiftnumber = __builtin_ffsll(immValue) - 1;
        AArch64ImmOperand *shiftnum = CreateImmOperand(shiftnumber, dsize, false);
        SelectShift(resopnd, opnd0, CreateImmOperand(dsize - 1, dsize, false), kShiftAright, prmtype);
        uint32 mopBadd = is64bits ? MOP_xaddrrrs : MOP_waddrrrs;
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(
          mopBadd, resopnd, opnd0, resopnd,
          CreateBitShiftOperand(BitShiftOperand::LSR, dsize - shiftnumber, is64bits ? 6 : 5)));
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
          AArch64ImmOperand *shiftnum = CreateImmOperand(__builtin_ffsll(imm->GetValue()) - 1, dsize, false);
          SelectShift(resopnd, opnd0, shiftnum, kShiftLright, prmtype);

          return;
        } else if (imm->GetValue() < 0) {
          SelectAArch64Cmp(opnd0, imm, true, dsize);
          SelectAArch64CSet(resopnd, GetCondOperand(CC_CS), is64bits);

          return;
        }
      }
    }
  }

  if (opnd0ty != Operand::Opd_Register) {
    SelectDiv(resopnd, SelectCopy(opnd0, prmtype, prmtype), opnd1, prmtype);
  } else if (opnd1ty != Operand::Opd_Register) {
    SelectDiv(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
  } else {
    if (IsPrimitiveFloat(prmtype)) {
      curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_ddivrrr : MOP_sdivrrr, resopnd, opnd0, opnd1));
    } else if (IsPrimitiveInteger(prmtype)) {
      bool issigned = IsSignedInteger(prmtype);
      uint32 mopDiv = is64bits ? (issigned ? MOP_xsdivrrr : MOP_xudivrrr) : (issigned ? MOP_wsdivrrr : MOP_wudivrrr);
      curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopDiv, resopnd, opnd0, opnd1));
    }
  }
}

Operand *AArch64CGFunc::SelectDiv(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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

void AArch64CGFunc::SelectRem(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype, bool issigned,
                              bool is64bits) {
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  opnd1 = LoadIntoRegister(opnd1, prmtype);

  CG_ASSERT(IsPrimitiveInteger(prmtype), "Wrong type for REM");
 /* printf("%d \n", 29 % 7 );
  * -> 1
  * printf("%u %d \n", (unsigned)-7, (unsigned)(-7) % 7 );
  * -> 4294967289 4
  * printf("%d \n", (-7) % 7 );
  * -> 0
  * printf("%d \n", 237 % -7 );
  * 6->
  * printf("implicit i->u conversion %d \n", ((unsigned)237) % -7 );
  * implicit conversion 237

  * http://stackoverflow.com/questions/35351470/obtaining-remainder-using-single-aarch64-instruction
  * input: x0=dividend, x1=divisor
  * udiv|sdiv x2, x0, x1
  * msub x3, x2, x1, x0  -- multply-sub : x3 <- x0 - x2*x1
  * result: x2=quotient, x3=remainder
  */
  //allocate temporary register
  RegOperand *temp = CreateRegisterOperandOfType(prmtype);
  uint32 mopDiv = is64bits ? (issigned ? MOP_xsdivrrr : MOP_xudivrrr) : (issigned ? MOP_wsdivrrr : MOP_wudivrrr);
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopDiv, temp, opnd0, opnd1));

  uint32 mopSub = is64bits ? MOP_xmsubrrrr : MOP_wmsubrrrr;
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopSub, resopnd, temp, opnd1, opnd0));
}

Operand *AArch64CGFunc::SelectRem(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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

void AArch64CGFunc::SelectTest(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType dtype) {
  CG_ASSERT(false, "nyi");
}

Operand *AArch64CGFunc::SelectLand(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType prmtype = node->primType;
  CG_ASSERT(IsPrimitiveInteger(prmtype), "Land should be integer type");
  bool is64bits = GetPrimTypeBitSize(prmtype) == 64;
  RegOperand *resopnd = CreateRegisterOperandOfType(is64bits ? PTY_u64 : PTY_u32);
  // OP0 band Op1
  // cmp  OP0, 0     # compare X0 with 0, sets Z bit
  // ccmp OP1, 0, 4/*==0100b*/, ne     # if(OP0!=0) cmp Op1 and 0, else NZCV <- 0100 makes OP0==0
  // cset RES, ne     # if Z==1(i.e., OP0==0||OP1==0) RES<-0, RES<-1
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  SelectAArch64Cmp(opnd0, CreateImmOperand(0, prmtype, false), true, GetPrimTypeBitSize(prmtype));
  opnd1 = LoadIntoRegister(opnd1, prmtype);
  SelectAArch64CCmp(opnd1, CreateImmOperand(0, prmtype, false), CreateImmOperand(4, PTY_u8, false),
                    GetCondOperand(CC_NE), is64bits);
  SelectAArch64CSet(resopnd, GetCondOperand(CC_NE), is64bits);
  return resopnd;
}

Operand *AArch64CGFunc::SelectLor(BinaryNode *node, Operand *opnd0, Operand *opnd1, bool parentIsBr) {
  PrimType prmtype = node->primType;
  CG_ASSERT(IsPrimitiveInteger(prmtype), "Lior should be integer type");
  bool is64bits = GetPrimTypeBitSize(prmtype) == 64;
  RegOperand *resopnd = CreateRegisterOperandOfType(is64bits ? PTY_u64 : PTY_u32);
  // OP0 band Op1
  // cmp  OP0, 0     # compare X0 with 0, sets Z bit
  // ccmp OP1, 0, 0/*==0100b*/, eq     # if(OP0==0,eq) cmp Op1 and 0, else NZCV <- 0000 makes OP0!=0
  // cset RES, ne     # if Z==1(i.e., OP0==0&&OP1==0) RES<-0, RES<-1
  if (parentIsBr && !is64bits && opnd0->IsRegister() && static_cast<RegOperand *>(opnd0)->GetValidBitsNum() == 1 &&
      opnd1->IsRegister() && static_cast<RegOperand *>(opnd1)->GetValidBitsNum() == 1) {
    uint32 mop = MOP_wiorrrr;
    static_cast<RegOperand *>(resopnd)->SetValidBitsNum(1);
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, opnd0, opnd1));
  } else {
    SelectBior(resopnd, opnd0, opnd1, prmtype);
    SelectAArch64Cmp(resopnd, CreateImmOperand(0, prmtype, false), true, GetPrimTypeBitSize(prmtype));
    SelectAArch64CSet(resopnd, GetCondOperand(CC_NE), is64bits);
  }
  return resopnd;
}

void AArch64CGFunc::SelectCmpOp(Operand *resopnd, Operand *opnd0, Operand *opnd1, Opcode opcode, PrimType operandType) {
  uint32 dsize = resopnd->size_;
  bool isfloat = IsPrimitiveFloat(operandType);
  opnd0 = LoadIntoRegister(opnd0, operandType);

  // most of FP constants are passed as AArch64MemOperand
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
  AArch64RegOperand *xzr = AArch64RegOperand::GetZeroRegister(dsize);
  if (opcode == OP_cmpl || opcode == OP_cmpg) {
    CG_ASSERT(isfloat, "incorrect operand types");
    SelectFPCmpQuiet(opnd0, opnd1, GetPrimTypeBitSize(operandType));
    SelectAArch64CSINV(resopnd, xzr, xzr, GetCondOperand(CC_GE), (dsize == 64));
    SelectAArch64CSINC(resopnd, resopnd, xzr, GetCondOperand(CC_LE), (dsize == 64));
    if (opcode == OP_cmpl) {
      SelectAArch64CSINV(resopnd, resopnd, xzr, GetCondOperand(CC_VC), (dsize == 64));
    } else {
      SelectAArch64CSINC(resopnd, resopnd, xzr, GetCondOperand(CC_VC), (dsize == 64));
    }
  } else if (opcode == OP_cmp) {
    SelectAArch64Cmp(opnd0, opnd1, !isfloat, GetPrimTypeBitSize(operandType));
    if (unsignedIntegerComparison) {
      SelectAArch64CSINV(resopnd, xzr, xzr, GetCondOperand(CC_HS), (dsize == 64));
      SelectAArch64CSINC(resopnd, resopnd, xzr, GetCondOperand(CC_LS), (dsize == 64));
    } else {
      SelectAArch64CSINV(resopnd, xzr, xzr, GetCondOperand(CC_GE), (dsize == 64));
      SelectAArch64CSINC(resopnd, resopnd, xzr, GetCondOperand(CC_LE), (dsize == 64));
    }
  } else {
    static_cast<RegOperand *>(resopnd)->SetValidBitsNum(1);
    if (opcode == OP_lt && opnd0->IsRegister() && opnd1->IsImmediate() &&
        static_cast<ImmOperand *>(opnd1)->GetValue() == 0) {
      if (!unsignedIntegerComparison) {
        ImmOperand *shiftNum =
          CreateImmOperand(opnd0->GetSize() == 64 ? 63 : 31, opnd0->GetSize() == 64 ? 6 : 5, false);
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(opnd0->GetSize() == 64 ? MOP_xlsrrri6 : MOP_wlsrrri5,
                                                            resopnd, opnd0, shiftNum));
        return;
      } else {
        ImmOperand *constNum = CreateImmOperand(0, opnd0->GetSize() == 64 ? 64 : 32, false);
        curbb->AppendInsn(
          cg->BuildInstruction<AArch64Insn>(opnd0->GetSize() == 64 ? MOP_xmovri64 : MOP_xmovri32, resopnd, constNum));
        return;
      }
    }
    SelectAArch64Cmp(opnd0, opnd1, !isfloat, GetPrimTypeBitSize(operandType));

    AArch64CC_t cc = CC_EQ;
    switch (opcode) {
      case OP_eq:
        cc = CC_EQ;
        break;
      case OP_ne:
        cc = CC_NE;
        break;
      case OP_le:
        cc = unsignedIntegerComparison ? CC_LS : CC_LE;
        break;
      case OP_ge:
        cc = unsignedIntegerComparison ? CC_HS : CC_GE;
        break;
      case OP_gt:
        cc = unsignedIntegerComparison ? CC_HI : CC_GT;
        break;
      case OP_lt:
        cc = unsignedIntegerComparison ? CC_LO : CC_LT;
        break;
      default:
        CG_ASSERT(false, "illegal logical operator");
    }
    SelectAArch64CSet(resopnd, GetCondOperand(cc), (dsize == 64));
  }
}

Operand *AArch64CGFunc::SelectCmpOp(CompareNode *node, Operand *opnd0, Operand *opnd1) {
  RegOperand *resopnd = CreateRegisterOperandOfType(node->primType);
  SelectCmpOp(resopnd, opnd0, opnd1, node->op, node->opndType);
  return resopnd;
}

void AArch64CGFunc::SelectFPCmpQuiet(Operand *o0, Operand *o1, uint32 dsize) {
  MOperator mopcode = 0;
  if (o1->op_kind_ == Operand::Opd_FPZeroImmediate) {
    mopcode = (dsize == 64) ? MOP_dcmpqri : (dsize == 32) ? MOP_scmpqri : MOP_hcmpqri;
  } else if (o1->op_kind_ == Operand::Opd_Register) {
    mopcode = (dsize == 64) ? MOP_dcmpqrr : (dsize == 32) ? MOP_scmpqrr : MOP_hcmpqrr;
  } else {
    CG_ASSERT(false, "unsupported operand type");
  }
  Operand *rflag = GetOrCreateRflag();
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopcode, rflag, o0, o1));
}

void AArch64CGFunc::SelectAArch64Cmp(Operand *o0, Operand *o1, bool isIntty, uint32 dsize) {
  MOperator mopcode = 0;
  if (isIntty) {
    if (o1->op_kind_ == Operand::Opd_Immediate) {
      ImmOperand *immopnd = static_cast<ImmOperand *>(o1);
      // imm : 0 ~ 4095, shift: none, LSL #0, or LSL #12
      // aarch64 assembly takes up to 24-bits, if the lower 12 bits is all 0
      if (immopnd->IsInBitSize(12) || immopnd->IsInBitSize(12, 12)) {
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
    if (o1->op_kind_ == Operand::Opd_FPZeroImmediate) {
      mopcode = (dsize == 64) ? MOP_dcmperi : (dsize == 32) ? MOP_scmperi : MOP_hcmperi;
    } else if (o1->op_kind_ == Operand::Opd_Register) {
      mopcode = (dsize == 64) ? MOP_dcmperr : (dsize == 32) ? MOP_scmperr : MOP_hcmperr;
    } else {
      CG_ASSERT(false, "unsupported operand type");
    }
  }
  CG_ASSERT(mopcode != 0, "mopcode undefined");
  Operand *rflag = GetOrCreateRflag();
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopcode, rflag, o0, o1));
}

void AArch64CGFunc::SelectAArch64CCmp(Operand *o, Operand *i, Operand *nzcv, CondOperand *cond, bool is64bits) {
  uint32 mopcode = is64bits ? MOP_xccmpriic : MOP_wccmpriic;
  Operand *rflag = GetOrCreateRflag();
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopcode, rflag, o, i, nzcv, cond));
}

void AArch64CGFunc::SelectAArch64CSet(Operand *r, CondOperand *cond, bool is64bits) {
  MOperator mopcode = is64bits ? MOP_xcsetrc : MOP_wcsetrc;
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopcode, r, cond));
}

void AArch64CGFunc::SelectAArch64CSINV(Operand *res, Operand *o0, Operand *o1, CondOperand *cond, bool is64bits) {
  MOperator mopcode = is64bits ? MOP_xcsinvrrrc : MOP_wcsinvrrrc;
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopcode, res, o0, o1, cond));
}

void AArch64CGFunc::SelectAArch64CSINC(Operand *res, Operand *o0, Operand *o1, CondOperand *cond, bool is64bits) {
  MOperator mopcode = is64bits ? MOP_xcsincrrrc : MOP_wcsincrrrc;
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopcode, res, o0, o1, cond));
}

Operand *AArch64CGFunc::SelectBand(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  SelectBand(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void AArch64CGFunc::SelectBand(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectBand(resopnd, SelectCopy(opnd0, prmtype, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      AArch64ImmOperand *immopnd = static_cast<AArch64ImmOperand *>(opnd1);
      if (immopnd->IsZero()) {
        uint32 mopMv = is64bits ? MOP_xmovrr : MOP_wmovrr;
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopMv, resopnd, AArch64RegOperand::GetZeroRegister(dsize)));
      } else if ((is64bits && immopnd->IsAllOnes()) || (!is64bits && immopnd->IsAllOnes32bit())) {
        SelectCopy(resopnd, prmtype, opnd0, prmtype);
      } else if (immopnd->IsBitmaskImmediate()) {
        uint32 mopBand = is64bits ? MOP_xandrri13 : MOP_wandrri12;
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBand, resopnd, opnd0, opnd1));
      } else {
        int64 immVal = immopnd->GetValue();
        int tail0bitNum = GetTail0BitNum(immVal);
        int head0bitNum = GetHead0BitNum(immVal);
        int bitnum = 64 - head0bitNum - tail0bitNum;
        RegOperand *regopnd = CreateRegisterOperandOfType(prmtype);

        if (bitnum <= 16) {
          int64 newImm = ((uint64)immVal >> (unsigned int)tail0bitNum) & 0xFFFF;
          AArch64ImmOperand *immOpnd1 = CreateImmOperand(newImm, 16, false);
          SelectCopyImm(regopnd, immOpnd1, prmtype);
          uint32 mopBand = is64bits ? MOP_xandrrrs : MOP_wandrrrs;
          curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(
            mopBand, resopnd, opnd0, regopnd,
            CreateBitShiftOperand(BitShiftOperand::LSL, tail0bitNum, is64bits ? 6 : 5)));
        } else {
          SelectCopyImm(regopnd, immopnd, prmtype);
          uint32 mopBand = is64bits ? MOP_xandrrr : MOP_wandrrr;
          curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBand, resopnd, opnd0, regopnd));
        }
      }
    } else {
      SelectBand(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBand(resopnd, opnd1, opnd0, prmtype);
  } else {
    CG_ASSERT(IsPrimitiveInteger(prmtype), "NYI band");
    uint32 mopBand = is64bits ? MOP_xandrrr : MOP_wandrrr;
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBand, resopnd, opnd0, opnd1));
  }
}

Operand *AArch64CGFunc::SelectBior(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  SelectBior(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void AArch64CGFunc::SelectBior(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectBior(resopnd, SelectCopy(opnd0, prmtype, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      AArch64ImmOperand *immopnd = static_cast<AArch64ImmOperand *>(opnd1);

      if (immopnd->IsZero()) {
        SelectCopy(resopnd, prmtype, opnd0, prmtype);
      } else if (immopnd->IsAllOnes()) {
        uint32 mopMovn = is64bits ? MOP_xmovnri16 : MOP_wmovnri16;
        curbb->AppendInsn(
          cg->BuildInstruction<AArch64Insn>(mopMovn, resopnd, AArch64RegOperand::GetZeroRegister(dsize)));
      } else if (immopnd->IsBitmaskImmediate()) {
        uint32 mopBior = is64bits ? MOP_xiorrri13 : MOP_wiorrri12;
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBior, resopnd, opnd0, opnd1));
      } else {
        int64 immVal = immopnd->GetValue();
        int tail0bitNum = GetTail0BitNum(immVal);
        int head0bitNum = GetHead0BitNum(immVal);
        int bitnum = 64 - head0bitNum - tail0bitNum;
        RegOperand *regopnd = CreateRegisterOperandOfType(prmtype);

        if (bitnum <= 16) {
          int64 newImm = ((uint64)immVal >> (unsigned int)tail0bitNum) & 0xFFFF;
          AArch64ImmOperand *immOpnd1 = CreateImmOperand(newImm, 16, false);
          SelectCopyImm(regopnd, immOpnd1, prmtype);
          uint32 mopBior = is64bits ? MOP_xiorrrrs : MOP_wiorrrrs;
          curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(
            mopBior, resopnd, opnd0, regopnd,
            CreateBitShiftOperand(BitShiftOperand::LSL, tail0bitNum, is64bits ? 6 : 5)));
        } else {
          SelectCopyImm(regopnd, immopnd, prmtype);
          uint32 mopBior = is64bits ? MOP_xiorrrr : MOP_wiorrrr;
          curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBior, resopnd, opnd0, regopnd));
        }
      }
    } else {
      SelectBior(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBior(resopnd, opnd1, opnd0, prmtype);
  } else {
    CG_ASSERT(IsPrimitiveInteger(prmtype), "NYI band");
    uint32 mopBior = is64bits ? MOP_xiorrrr : MOP_wiorrrr;
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBior, resopnd, opnd0, opnd1));
  }
}

Operand *AArch64CGFunc::SelectMin(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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

void AArch64CGFunc::SelectMin(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (IsPrimitiveInteger(prmtype)) {
    opnd0 = LoadIntoRegister(opnd0, prmtype);
    if (opnd1->op_kind_ != Operand::Opd_Immediate) {
      opnd1 = LoadIntoRegister(opnd1, prmtype);
    }
    SelectAArch64Cmp(opnd0, opnd1, true, dsize);
    resopnd = LoadIntoRegister(resopnd, prmtype);
    CondOperand *cc = IsSignedInteger(prmtype) ? GetCondOperand(CC_LT) : GetCondOperand(CC_LO);
    SelectAArch64Select(resopnd, opnd0, opnd1, cc, true, dsize);
  } else if (IsPrimitiveFloat(prmtype)) {
    opnd0 = LoadIntoRegister(opnd0, prmtype);
    opnd1 = LoadIntoRegister(opnd1, prmtype);
    SelectFMinFMax(resopnd, opnd0, opnd1, is64bits, true /*min*/);
  }
}

Operand *AArch64CGFunc::SelectMax(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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

void AArch64CGFunc::SelectMax(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (IsPrimitiveInteger(prmtype)) {
    opnd0 = LoadIntoRegister(opnd0, prmtype);
    opnd1 = LoadIntoRegister(opnd1, prmtype);
    SelectAArch64Cmp(opnd0, opnd1, true, dsize);
    resopnd = LoadIntoRegister(resopnd, prmtype);
    CondOperand *cc = IsSignedInteger(prmtype) ? GetCondOperand(CC_GT) : GetCondOperand(CC_HI);
    SelectAArch64Select(resopnd, opnd0, opnd1, cc, true, dsize);
  } else if (IsPrimitiveFloat(prmtype)) {
    opnd0 = LoadIntoRegister(opnd0, prmtype);
    opnd1 = LoadIntoRegister(opnd1, prmtype);
    SelectFMinFMax(resopnd, opnd0, opnd1, is64bits, false /*min*/);
  } else {
    CG_ASSERT(false, "NIY type max");
  }
}

void AArch64CGFunc::SelectFMinFMax(Operand *resopnd, Operand *opnd0, Operand *opnd1, bool is64bits, bool isMin) {
  uint32 mopcode = isMin ? (is64bits ? MOP_xfminrrr : MOP_wfminrrr) : (is64bits ? MOP_xfmaxrrr : MOP_wfmaxrrr);
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopcode, resopnd, opnd0, opnd1));
}

Operand *AArch64CGFunc::SelectBxor(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
  SelectBxor(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void AArch64CGFunc::SelectBxor(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectBxor(resopnd, SelectCopy(opnd0, prmtype, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      AArch64ImmOperand *immopnd = static_cast<AArch64ImmOperand *>(opnd1);
      if (immopnd->IsZero()) {
        SelectCopy(resopnd, prmtype, opnd0, prmtype);
      } else if (immopnd->IsAllOnes()) {
        SelectMvn(resopnd, opnd0, prmtype);
      } else if (immopnd->IsBitmaskImmediate()) {
        uint32 mopBxor = is64bits ? MOP_xeorrri13 : MOP_weorrri12;
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBxor, resopnd, opnd0, opnd1));
      } else {
        int64 immVal = immopnd->GetValue();
        int tail0bitNum = GetTail0BitNum(immVal);
        int head0bitNum = GetHead0BitNum(immVal);
        int bitnum = 64 - head0bitNum - tail0bitNum;
        RegOperand *regopnd = CreateRegisterOperandOfType(prmtype);

        if (bitnum <= 16) {
          int64 newImm = (static_cast<uint64>(immVal) >> static_cast<unsigned int>(tail0bitNum)) & 0xFFFF;
          AArch64ImmOperand *immOpnd1 = CreateImmOperand(newImm, 16, false);
          SelectCopyImm(regopnd, immOpnd1, prmtype);
          uint32 mopBxor = is64bits ? MOP_xeorrrrs : MOP_weorrrrs;
          curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(
            mopBxor, resopnd, opnd0, regopnd,
            CreateBitShiftOperand(BitShiftOperand::LSL, tail0bitNum, is64bits ? 6 : 5)));
        } else {
          SelectCopyImm(regopnd, immopnd, prmtype);
          uint32 mopBxor = is64bits ? MOP_xeorrrr : MOP_weorrrr;
          curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBxor, resopnd, opnd0, regopnd));
        }
      }
    } else {
      SelectBxor(resopnd, opnd0, SelectCopy(opnd1, prmtype, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBxor(resopnd, opnd1, opnd0, prmtype);
  } else {
    CG_ASSERT(IsPrimitiveInteger(prmtype), "NYI bxor");
    uint32 mopBxor = is64bits ? MOP_xeorrrr : MOP_weorrrr;
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBxor, resopnd, opnd0, opnd1));
  }
}

Operand *AArch64CGFunc::SelectShift(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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

void AArch64CGFunc::SelectBxorShift(Operand *resopnd, Operand *opnd0, Operand *opnd1, Operand *opnd2,
                                    PrimType prmtype) {
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  opnd1 = LoadIntoRegister(opnd1, prmtype);
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  MOperator mopBxor = is64bits ? MOP_xeorrrrs : MOP_weorrrrs;
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBxor, resopnd, opnd0, opnd1, opnd2));
}

void AArch64CGFunc::SelectShift(Operand *resopnd, Operand *opnd0, Operand *opnd1, SHIFTDIRECTION direct,
                                PrimType prmtype) {
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  opnd0 = LoadIntoRegister(opnd0, prmtype);

  MOperator mopShift;
  if (opnd1ty == Operand::Opd_Immediate) {
    AArch64ImmOperand *immopnd1 = static_cast<AArch64ImmOperand *>(opnd1);
    const int64 kVal = immopnd1->GetValue();
    const uint32 kShiftamt = is64bits ? 63 : 31;
    if (kVal == 0) {
      SelectCopy(resopnd, prmtype, opnd0, prmtype);
      return;
    }
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

  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopShift, resopnd, opnd0, opnd1));
}

Operand *AArch64CGFunc::SelectAbs(UnaryNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  if (IsPrimitiveFloat(dtyp)) {
    CG_ASSERT(GetPrimTypeBitSize(dtyp) >= 32, "We don't support hanf-word FP operands yet");
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    opnd0 = LoadIntoRegister(opnd0, dtyp);
    RegOperand *resopnd = CreateRegisterOperandOfType(dtyp);
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_dabsrr : MOP_sabsrr, resopnd, opnd0));
    return resopnd;
  } else {
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    PrimType prmtype = is64bits ? (PTY_i64) : (PTY_i32);  // promoted type
    RegOperand *resopnd = CreateRegisterOperandOfType(prmtype);
    opnd0 = LoadIntoRegister(opnd0, prmtype);
    SelectAArch64Cmp(opnd0, CreateImmOperand(0, is64bits ? PTY_u64 : PTY_u32, false), true, GetPrimTypeBitSize(dtyp));
    uint32 mopCsneg = is64bits ? MOP_xcsnegrrrc : MOP_wcsnegrrrc;
    // ABS requires the operand be interpreted as a signed integer
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopCsneg, resopnd, opnd0, opnd0, GetCondOperand(CC_GE)));
    return resopnd;
  }
}

Operand *AArch64CGFunc::SelectBnot(UnaryNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  RegOperand *resopnd = nullptr;
  if (IsPrimitiveInteger(dtyp)) {
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    bool issigned = IsSignedInteger(dtyp);
    PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
    resopnd = CreateRegisterOperandOfType(prmtype);

    opnd0 = LoadIntoRegister(opnd0, prmtype);

    uint32 mopBnot = is64bits ? MOP_xnotrr : MOP_wnotrr;
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBnot, resopnd, opnd0));
  } else {
    CG_ASSERT(false, "bnot expect integer or NYI");
  }
  return resopnd;
}

Operand *AArch64CGFunc::SelectExtractbits(ExtractbitsNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  RegOperand *resopnd = CreateRegisterOperandOfType(dtyp);
  bool issigned = IsSignedInteger(dtyp);
  uint8 bitsOffset = node->bitsOffset;
  uint8 bitsSize = node->bitsSize;
  bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
  uint32 immWidth = is64bits ? 13 : 12;
  opnd0 = LoadIntoRegister(opnd0, dtyp);
  if (bitsOffset == 0 && !issigned && (bitsSize < immWidth)) {
    SelectBand(resopnd, opnd0, CreateImmOperand((static_cast<uint64>(1) << bitsSize) - 1, immWidth, false), dtyp);
    return resopnd;
  }
  uint32 mopBfx =
    is64bits ? (issigned ? MOP_xsbfxrri6i6 : MOP_xubfxrri6i6) : (issigned ? MOP_wsbfxrri5i5 : MOP_wubfxrri5i5);
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopBfx, resopnd, opnd0, CreateImmOperand(bitsOffset, 8, false),
                                                      CreateImmOperand(bitsSize, 8, false)));
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
void AArch64CGFunc::SelectDepositbits(Operand *resopnd, Operand *opnd0, Operand *opnd1, uint32 bitsOffset, uint32 bitsSize,
                                      PrimType rtyp, PrimType dtyp) {
  RegOperand *t1opnd = CreateRegisterOperandOfType(rtyp);
  bool is64bits = GetPrimTypeBitSize(rtyp) == 64;
  // if operand 1 is immediate and fits in MOVK, use it
  // MOVK Wd, #imm{, LSL #shift} ; 32-bit general registers
  // MOVK Xd, #imm{, LSL #shift} ; 64-bit general registers
  if (opnd1->IsIntImmediate() && IsMoveWideKeepable(bitsOffset, bitsSize, is64bits)) {
    SelectCopy(resopnd, rtyp, opnd0, rtyp);
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>((is64bits ? MOP_xmovkri16 : MOP_wmovkri16), resopnd, opnd1,
                                                        GetLogicalShiftLeftOperand(bitsOffset, is64bits)));
  } else {
    // Merge-form of Itanium deposit
    // 1. (opnd0>>bitsOffset) ^ opnd1
    Operand *shiftOpnd = CreateBitShiftOperand(BitShiftOperand::LSR, bitsOffset, is64bits ? 6 : 5);
    // bit-shift the first operand to the right by offset and XOR with the second operand
    SelectBxorShift(t1opnd, opnd1, opnd0, shiftOpnd, rtyp);
    // bit-shift the result to the left by offset, retain size bits from offset, clear the rest.
    // ubfiz t1opnd, bitsOffset, size
    uint32 mopUbfiz = is64bits ? MOP_xubfizrri6i6 : MOP_wubfizrri5i5;
    // XOR the result with the first operand
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopUbfiz, t1opnd, t1opnd, CreateImmOperand(bitsOffset, 8, false),
                                                        CreateImmOperand(bitsSize, 8, false)));
    // opnd0 ^ t1opnd
    SelectBxor(resopnd, opnd0, t1opnd, rtyp);
  }
}

Operand *AArch64CGFunc::SelectDepositbits(DepositbitsNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtyp = node->primType;
  RegOperand *resopnd = CreateRegisterOperandOfType(dtyp);
  SelectDepositbits(resopnd, opnd0, opnd1, node->bitsOffset, node->bitsSize, dtyp, dtyp);
  return resopnd;
}

Operand *AArch64CGFunc::SelectLnot(UnaryNode *node, Operand *opnd0) {
  PrimType dtype = node->primType;
  RegOperand *resopnd = CreateRegisterOperandOfType(dtype);
  bool is64bits = (GetPrimTypeBitSize(dtype) == 64);
  opnd0 = LoadIntoRegister(opnd0, dtype);
  SelectAArch64Cmp(opnd0, CreateImmOperand(0, is64bits ? PTY_u64 : PTY_u32, false), true, GetPrimTypeBitSize(dtype));
  SelectAArch64CSet(resopnd, GetCondOperand(CC_EQ), is64bits);
  return resopnd;
}

Operand *AArch64CGFunc::SelectNeg(UnaryNode *node, Operand *opnd0) {
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

void AArch64CGFunc::SelectNeg(Operand *dest, Operand *opnd0, PrimType prmtype) {
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  bool is64bits = (GetPrimTypeBitSize(prmtype) == 64);
  MOperator mop;
  if (IsPrimitiveFloat(prmtype)) {
    mop = is64bits ? MOP_xfnegrr : MOP_wfnegrr;
  } else {
    mop = is64bits ? MOP_xinegrr : MOP_winegrr;
  }
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, dest, opnd0));
}

void AArch64CGFunc::SelectMvn(Operand *dest, Operand *opnd0, PrimType prmtype) {
  opnd0 = LoadIntoRegister(opnd0, prmtype);
  bool is64bits = (GetPrimTypeBitSize(prmtype) == 64);
  MOperator mop;
  CG_ASSERT(!IsPrimitiveFloat(prmtype), "Instruction 'mvn' do not have float version.");
  mop = is64bits ? MOP_xnotrr : MOP_wnotrr;
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, dest, opnd0));
}

Operand *AArch64CGFunc::SelectRecip(UnaryNode *node, Operand *opnd0) {
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

Operand *AArch64CGFunc::SelectSqrt(UnaryNode *node, Operand *opnd0) {
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
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_vsqrtd : MOP_vsqrts, resopnd, opnd0));
    return resopnd;
  } else {
    CG_ASSERT(false, "should be float type");
  }
  return nullptr;
}

void AArch64CGFunc::SelectCvtFloat2Int(Operand *resopnd, Operand *opnd0, PrimType itype, PrimType ftype) {
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
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, opnd0));
}

void AArch64CGFunc::SelectCvtInt2Float(Operand *resopnd, Operand *opnd0, PrimType totype, PrimType fromtype) {
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
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, opnd0));
}

Operand *AArch64CGFunc::SelectCeil(TypeCvtNode *node, Operand *opnd0) {
  PrimType ftype = node->fromPrimType;
  PrimType itype = node->primType;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  RegOperand *resopnd = CreateRegisterOperandOfType(itype);
  opnd0 = LoadIntoRegister(opnd0, ftype);
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xvcvtps : MOP_vcvtps, resopnd, opnd0));
  return resopnd;
}

// float to int floor
Operand *AArch64CGFunc::SelectFloor(TypeCvtNode *node, Operand *opnd0) {
  if (CLANG) {
    // Generate C style floorf() and floor() calls
    PrimType ftype = node->fromPrimType;
    PrimType rtype = node->primType;
    vector<Operand *> opndvec;
    AArch64RegOperand *physOpnd;
    if (ftype == PTY_f64) {
      physOpnd = GetOrCreatePhysicalRegisterOperand(D0, 64, kRegTyFloat);
    } else {
      physOpnd = GetOrCreatePhysicalRegisterOperand(S0, 32, kRegTyFloat);
    }
    Operand *resopnd = static_cast<Operand *>(physOpnd);
    opndvec.push_back(resopnd);
    opnd0 = LoadIntoRegister(opnd0, ftype);
    opndvec.push_back(opnd0);
    if (ftype == PTY_f64) {
      SelectLibCall("floor", opndvec, ftype, rtype);
    } else {
      SelectLibCall("floorf", opndvec, ftype, rtype);
    }
    return resopnd;
  }
  PrimType ftype = node->fromPrimType;
  PrimType itype = node->primType;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  RegOperand *resopnd = CreateRegisterOperandOfType(itype);
  opnd0 = LoadIntoRegister(opnd0, ftype);
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xvcvtms : MOP_vcvtms, resopnd, opnd0));
  return resopnd;
}

static bool LIsPrimitivePointer(PrimType primType) {
  return (PTY_ptr <= primType && primType <= PTY_a64);
}

Operand *AArch64CGFunc::SelectRetype(TypeCvtNode *node, Operand *opnd0) {
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
    bool isimm = false;
    ImmOperand *imm = static_cast<ImmOperand *>(opnd0);
    uint64 val = static_cast<uint64>(imm->GetValue());
    uint64 canRepreset = is64bits ? (val & 0xffffffffffff) : (val & 0x7ffff);
    uint32 bb = is64bits ? (val >> 61) & 0x3 : (val >> 29) & 0x3;
    uint32 b = is64bits ? (val >> 54) & 0xff : (val >> 25) & 0x1f;
    bool bSame = is64bits ? (b == 0 || b == 0xff) : (b == 0 || b == 0x1f);
    canRepreset = canRepreset == 0 && ((bb & 0x1) ^ ((bb & 0x2) >> 1)) && bSame;
    if (IsPrimitiveInteger(fromtype) && IsPrimitiveFloat(totype) && canRepreset) {
      uint64 a = is64bits ? (val >> 63) << 7 : (val >> 31) << 7;
      uint64 tmp = is64bits ? val >> 48 : val >> 19;
      int64 imm8 = (tmp & 0x7f) | a;
      opnd0 = CreateImmOperand(imm8, 8, false, false, true);
      isimm = true;
    } else {
      opnd0 = LoadIntoRegister(opnd0, itype);
    }
    if ((IsPrimitiveFloat(fromtype) && IsPrimitiveInteger(totype)) ||
        (IsPrimitiveFloat(totype) && IsPrimitiveInteger(fromtype))) {
      uint32 mopFmov =
        isimm ? is64bits ? MOP_xdfmovri : MOP_wsfmovri
              : isfromint ? (is64bits ? MOP_xdfmovrr : MOP_wsfmovrr) : (is64bits ? MOP_dxfmovrr : MOP_swfmovrr);
      curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopFmov, resopnd, opnd0));
      return resopnd;
    } else {
      return opnd0;
    }
  } else {
    CG_ASSERT(false, "NYI retype");
  }
  return nullptr;
}

void AArch64CGFunc::SelectCvtFloat2Float(Operand *resopnd, Operand *opnd0, PrimType fromty, PrimType toty) {
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
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, opnd0));
}

bool AArch64CGFunc::IfParamVRegOperand(Operand *opnd) {
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
void AArch64CGFunc::SelectCvtInt2Int(BaseNode *parent, Operand *&resopnd, Operand *opnd0, PrimType fromty,
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
    CG_ASSERT(simm != nullptr, "simm is nullptr in AArch64CGFunc::SelectCvtInt2Int");
    bool isSign = false;
    int64 origValue = simm->GetValue();
    int64 newValue = origValue;
    int64 signValue = 0;
    if (isexpand) {
      //
    } else {
      // 64--->32
      if (fsize > tsize) {
        if (IsSignedInteger(toty)) {
          if (origValue < 0) {
            signValue = 0xFFFFFFFFFFFFFFFFLL & (static_cast<int64>(1ULL << tsize));
          }
          newValue = (origValue & (static_cast<int64>(1ULL << tsize) - 1)) | signValue;
        } else {
          newValue = origValue & (static_cast<int64>(1ULL << tsize) - 1);
        }
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

      MOperator mop = (is64bit ? (fsize == 8 ? MOP_xsxtb64 : (fsize == 16 ? MOP_xsxth64 : MOP_xsxtw64))
                               : (fsize == 8 ? MOP_xsxtb32 : MOP_xsxth32));
      curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, opnd0));
    } else {
      // Unsigned
      if (is64bit) {
        if (fsize == 8) {
          ImmOperand *immopnd = CreateImmOperand(0xff, 64, false);
          curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_xandrri13, resopnd, opnd0, immopnd));
        } else if (fsize == 16) {
          ImmOperand *immopnd = CreateImmOperand(0xffff, 64, false);
          curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_xandrri13, resopnd, opnd0, immopnd));
        } else {
          curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_xuxtw64, resopnd, opnd0));
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
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(fsize == 8 ? MOP_xuxtb32 : MOP_xuxth32, resopnd, opnd0));
      }
    }
  } else {  // Same size or truncate
#ifdef CNV_OPTIMIZE
    // No code needed for aarch64 with same reg.
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
      if (fsize == 64) {
        MOperator mop = IsSignedInteger(toty) ? MOP_xsbfxrri6i6 : MOP_xubfxrri6i6;
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, opnd0, CreateImmOperand(0, 8, false),
                                                            CreateImmOperand(tsize, 8, false)));
      } else {
        MOperator mop = IsSignedInteger(toty) ? MOP_wsbfxrri5i5 : MOP_wubfxrri5i5;
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, opnd0, CreateImmOperand(0, 8, false),
                                                            CreateImmOperand(tsize, 8, false)));
      }
    } else {
      // same size, so resopnd can be set
      if (IsSignedInteger(fromty) == IsSignedInteger(toty) ||
          GetPrimTypeSize(toty) > 4) {
        AArch64RegOperand *reg = static_cast<AArch64RegOperand *>(resopnd);
        reg->SetRegisterNumber(static_cast<AArch64RegOperand *>(opnd0)->GetRegisterNumber());
      } else if (IsUnsignedInteger(toty)) {
        MOperator mop;
        switch (toty) {
        case PTY_u8:
          mop = MOP_xuxtb32;
          break;
        case PTY_u16:
          mop = MOP_xuxth32;
          break;
        case PTY_u32:
          mop = MOP_xuxtw64;
          break;
        default:
          CHECK_FATAL(0,"Unhandled unsigned convert");
        }
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, opnd0));
      } else {
        uint32 size = GetPrimTypeSize(toty);
        // signed target
        MOperator mop;
        switch (toty) {
        case PTY_i8:
          mop = (size > 4) ? MOP_xsxtb64 : MOP_xsxtb32;
          break;
        case PTY_i16:
          mop = (size > 4) ? MOP_xsxth64 : MOP_xsxth32;
          break;
        case PTY_i32:
          mop = MOP_xsxtw64;
          break;
        default:
          CHECK_FATAL(0,"Unhandled unsigned convert");
        }
        curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mop, resopnd, opnd0));
      }
    }
#endif
  }
}

Operand *AArch64CGFunc::SelectCvt(BaseNode *parent, TypeCvtNode *node, Operand *opnd0) {
  PrimType fromtype = node->fromPrimType;
  PrimType totype = node->primType;
  if (fromtype == totype) {
    return opnd0;  // noop
  }
  Operand *resopnd = static_cast<Operand *>(CreateRegisterOperandOfType(totype));
  if (IsPrimitiveFloat(totype) && IsPrimitiveInteger(fromtype)) {
    SelectCvtInt2Float(resopnd, opnd0, totype, fromtype);
    return resopnd;
  } else if (IsPrimitiveFloat(fromtype) && IsPrimitiveInteger(totype)) {
    SelectCvtFloat2Int(resopnd, opnd0, totype, fromtype);
    return resopnd;
  } else if (IsPrimitiveInteger(fromtype) && IsPrimitiveInteger(totype)) {
    SelectCvtInt2Int(parent, resopnd, opnd0, fromtype, totype);
    return resopnd;
  } else {  // both are float type
    SelectCvtFloat2Float(resopnd, opnd0, fromtype, totype);
    return resopnd;
  }
}

Operand *AArch64CGFunc::SelectRound(TypeCvtNode *node, Operand *opnd0) {
  PrimType ftype = node->fromPrimType;
  PrimType itype = node->primType;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  RegOperand *resopnd = CreateRegisterOperandOfType(itype);
  opnd0 = LoadIntoRegister(opnd0, ftype);
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xvcvtas : MOP_vcvtas, resopnd, opnd0));
  return resopnd;
}

Operand *AArch64CGFunc::SelectTrunc(TypeCvtNode *node, Operand *opnd0) {
  PrimType ftype = node->fromPrimType;
  bool is64bits = (GetPrimTypeBitSize(node->primType) == 64);
  PrimType itype = (is64bits) ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                              : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  RegOperand *resopnd = CreateRegisterOperandOfType(itype);
  SelectCvtFloat2Int(resopnd, opnd0, itype, ftype);
  return resopnd;
}

void AArch64CGFunc::SelectSelect(Operand *resopnd, Operand *condopnd, Operand *trueopnd, Operand *falseopnd,
                                 PrimType dtyp, PrimType ctyp) {
  CG_ASSERT(resopnd != condopnd, "resopnd cannot be the same as condopnd");
  condopnd = LoadIntoRegister(condopnd, ctyp);
  trueopnd = LoadIntoRegister(trueopnd, dtyp);
  falseopnd = LoadIntoRegister(falseopnd, dtyp);

  bool isIntty = IsPrimitiveInteger(dtyp);

  SelectAArch64Cmp(condopnd, CreateImmOperand(0, ctyp, false), true, GetPrimTypeBitSize(ctyp));
  CG_ASSERT((IsPrimitiveInteger(dtyp) || IsPrimitiveFloat(dtyp)), "unknown type for select");
  resopnd = LoadIntoRegister(resopnd, dtyp);
  SelectAArch64Select(resopnd, trueopnd, falseopnd, GetCondOperand(CC_NE), isIntty, GetPrimTypeBitSize(dtyp));
}

Operand *AArch64CGFunc::SelectSelect(TernaryNode *node, Operand *opnd0, Operand *opnd1, Operand *opnd2) {
  PrimType dtyp = node->primType;
  PrimType ctyp = node->Opnd(0)->primType;
  RegOperand *resopnd = CreateRegisterOperandOfType(dtyp);
  SelectSelect(resopnd, opnd0, opnd1, opnd2, dtyp, ctyp);
  return resopnd;
}

/*
   syntax: select <prim-type> (<opnd0>, <opnd1>, <opnd2>)
   <opnd0> must be of integer type.
   <opnd1> and <opnd2> must be of the type given by <prim-type>.
   If <opnd0> is not 0, return <opnd1>.  Otherwise, return <opnd2>.
 */
void AArch64CGFunc::SelectAArch64Select(Operand *dest, Operand *o0, Operand *o1, CondOperand *cond, bool isIntty,
                                        uint32 dsize) {
  uint32 mopcode = isIntty ? (dsize == 64 ? MOP_xcselrrrc : MOP_wcselrrrc)
                           : (dsize == 64 ? MOP_dcselrrrc : (dsize == 32 ? MOP_scselrrrc : MOP_hcselrrrc));
  if (o1->IsImmediate()) {
    // int only
    uint32 movop = (dsize == 64 ? MOP_xmovri64 : MOP_xmovri32);
    RegOperand *movdest = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, (dsize == 64) ? 8 : 4));
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(movop, movdest, o1));
    curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopcode, dest, o0, movdest, cond));
    return;
  }
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(mopcode, dest, o0, o1, cond));
}

void AArch64CGFunc::SelectRangegoto(RangegotoNode *rangegotonode, Operand *opnd0) {
  SmallCaseVector &switchtable = rangegotonode->rangegotoTable;
  MIRType *etype = GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_a64);
  /*
   * we store 8-byte displacement ( jump_label - offset_table_address )
   * in the table. Refer to AArch64Emit::Emit() in aarch64_emit.cpp
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
    addopnd = static_cast<AArch64RegOperand *>(SelectCopy(addopnd, PTY_u64, PTY_u64));
  }

  RegOperand *baseopnd = CreateRegisterOperandOfType(PTY_u64);
  StImmOperand *switchTable = CreateStImmOperand(lblst, 0, 0);

  // load the address of the switch table
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_xadrp, baseopnd, switchTable));
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_xadrpl12, baseopnd, baseopnd, switchTable));

  // load the displacement into a register by accessing memory at base + index*8
  Operand *disp = memPool->New<AArch64MemOperand>(AArch64MemOperand::kAddrModeBOrX, 64, baseopnd, addopnd, 3);
  RegOperand *tgt = CreateRegisterOperandOfType(PTY_a64);
  SelectAdd(tgt, baseopnd, disp, PTY_u64);
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_xbr, tgt));
}

Operand *AArch64CGFunc::SelectAlloca(UnaryNode *node, Operand *opnd0) {
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

  SelectAdd(aliop, resopnd, CreateImmOperand(AARCH64_STACK_PTR_ALIGNMENT - 1, 64, true), PTY_u64);
  Operand *shifopnd = CreateImmOperand(__builtin_ctz(AARCH64_STACK_PTR_ALIGNMENT), 64, true);
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

Operand *AArch64CGFunc::SelectMalloc(UnaryNode *node, Operand *opnd0) {
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

Operand *AArch64CGFunc::SelectGCMalloc(GCMallocNode *node) {
  PrimType rettype = node->primType;
  CG_ASSERT((rettype == PTY_a64), "wrong type");

  // Get the size and alignment of the type.
  TyIdx tyIdx = node->tyIdx;
  uint64_t size = becommon.type_size_table.at(tyIdx.GetIdx());
  uint8_t align = AArch64RTSupport::kObjectAlignment;

  // Generate the call to MCC_NewObj
  Operand *opndSize = CreateImmOperand(size, 64, false);
  Operand *opndAlign = CreateImmOperand(align, 64, false);

  RegOperand *resopnd = CreateRegisterOperandOfType(rettype);

  vector<Operand *> opndvec{ resopnd, opndSize, opndAlign };

  const char *funcName = strdup(GetIntrinsicFuncName(INTRN_MCCNewObj).c_str());
  SelectLibCall(funcName, opndvec, PTY_u64, rettype);

  return resopnd;
}

Operand *AArch64CGFunc::SelectJarrayMalloc(JarrayMallocNode *node, Operand *opnd0) {
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

  uint64_t fixedSize = AArch64RTSupport::kArrayContentOffset;
  uint8_t align = AArch64RTSupport::kObjectAlignment;

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
  MemOperand *opndArrayLengthField = CreateMemOpnd(resopnd, AArch64RTSupport::kArrayLengthOffset, 4);
  RegOperand *regopndNElems = SelectCopy(opndNElems, PTY_u32, PTY_u32);
  SelectCopy(opndArrayLengthField, PTY_u32, regopndNElems, PTY_u32);

  return resopnd;
}

}  // namespace maplebe
