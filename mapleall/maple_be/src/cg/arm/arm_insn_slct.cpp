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
#include "cg.h"
#include "arm_isa.h"
#include "arm_cg.h"
#include "opcode_info.h"  // mapleir/include/opcode_info.h
#include "cg_assert.h"

namespace maplebe {

using namespace maple;

MOperator ArmCGFunc::PickAddInsn(PrimType primtype, bool ismem) {
  CG_ASSERT(false, "NYI PickAddInsn");
  return MOP_undef;
}

MOperator ArmCGFunc::PickMpyInsn(PrimType primtype, bool ismem) {
  CG_ASSERT(false, "NYI PickAddInsn");
  return MOP_undef;
}

// opnd1 is the second operand of this insn, we need to choose different insn according to its type
MOperator ArmCGFunc::PickCmpInsn(PrimType primtype, Operand *opnd1) {
  switch (primtype) {
    case PTY_i32:
    case PTY_u32:
    case PTY_a32:
    case PTY_ptr:
    case PTY_ref: {
      switch (opnd1->op_kind_) {
        case Operand::Opd_Register:
          return MOP_Tbcmprr;
        case Operand::Opd_Immediate:
          return MOP_Tbcmpri8;
        default:
          CG_ASSERT(false, "unexpected operand for compare");
      }
    }
    case PTY_f32: {
      switch (opnd1->op_kind_) {
        case Operand::Opd_Register:
          return MOP_Tb2vcmps;
        default:
          CG_ASSERT(false, "unexpected operand for compare");
      }
    }
    case PTY_f64: {
      switch (opnd1->op_kind_) {
        case Operand::Opd_Register:
          return MOP_Tb2vcmpd;
        default:
          CG_ASSERT(false, "unexpected operand for compare");
      }
    }
    default:
      CG_ASSERT(false, "PickCmpInsn NYI or shoudn't be here");
  }
  return 0;
}

MOperator ArmCGFunc::PickJmpInsn(Opcode brop, Opcode cmpop, bool isfloat, bool issigned) {
  switch (cmpop) {
    default:
      CG_ASSERT(false, "PickJmpInsn error");
    case OP_ne:
      return brop == OP_brtrue ? MOP_bne : MOP_beq;
    case OP_eq:
      return brop == OP_brtrue ? MOP_beq : MOP_bne;
    case OP_lt:
      return brop == OP_brtrue ? (issigned ? MOP_blt : MOP_blo) : (isfloat ? MOP_bpl : (issigned ? MOP_bge : MOP_bhs));
    case OP_le:
      return brop == OP_brtrue ? (issigned ? MOP_ble : MOP_bls) : (isfloat ? MOP_bhi : (issigned ? MOP_bgt : MOP_bhi));
    case OP_gt:
      return brop == OP_brtrue ? (isfloat ? MOP_bhi : (issigned ? MOP_bgt : MOP_bhi)) : (issigned ? MOP_ble : MOP_bls);
    case OP_ge:
      return brop == OP_brtrue ? (isfloat ? MOP_bpl : (issigned ? MOP_bge : MOP_bhs)) : (issigned ? MOP_blt : MOP_blo);
  }
}

void ArmCGFunc::PickCmovInsn(Opcode cmpop, PrimType prmtype, MOperator &itop, MOperator &movop, bool iselse) {
  bool isfloat = IsPrimitiveFloat(prmtype);
  bool issigned = IsSignedInteger(prmtype);
  switch (cmpop) {
    default:
      CG_ASSERT(false, "PickCmovInsn error");
    case OP_ne: {
      itop = iselse ? MOP_Tbitene : MOP_Tbitne;
      movop = MOP_Tb2movneimm12;
      break;
    }
    case OP_eq: {
      itop = iselse ? MOP_Tbiteeq : MOP_Tbiteq;
      movop = MOP_Tb2moveqimm12;
      break;
    }
    case OP_lt: {
      itop = isfloat ? (iselse ? MOP_Tbitelt : MOP_Tbitlt)
                     : (issigned ? (iselse ? MOP_Tbitelt : MOP_Tbitlt) : (iselse ? MOP_Tbitecc : MOP_Tbitcc));
      movop = isfloat ? MOP_Tb2movltimm12 : (issigned ? MOP_Tb2movltimm12 : MOP_Tb2movccimm12);
      break;
    }
    case OP_le: {
      itop = isfloat ? (iselse ? MOP_Tbitele : MOP_Tbitle)
                     : (issigned ? (iselse ? MOP_Tbitele : MOP_Tbitle) : (iselse ? MOP_Tbitels : MOP_Tbitls));
      movop = isfloat ? MOP_Tb2movleimm12 : (issigned ? MOP_Tb2movleimm12 : MOP_Tb2movlsimm12);
      break;
    }
    case OP_gt: {
      itop = isfloat ? (iselse ? MOP_Tbitehi : MOP_Tbithi)
                     : (issigned ? (iselse ? MOP_Tbitegt : MOP_Tbitgt) : (iselse ? MOP_Tbitehi : MOP_Tbithi));
      movop = isfloat ? MOP_Tb2movhiimm12 : (issigned ? MOP_Tb2movgtimm12 : MOP_Tb2movhiimm12);
      break;
    }
    case OP_ge: {
      itop = isfloat ? (iselse ? MOP_Tbitepl : MOP_Tbitpl)
                     : (issigned ? (iselse ? MOP_Tbitege : MOP_Tbitge) : (issigned ? MOP_Tbitecs : MOP_Tbitcs));
      movop = isfloat ? MOP_Tb2movplimm12 : (issigned ? MOP_Tb2movgeimm12 : MOP_Tb2movcsimm12);
      break;
    }
  }
}

void ArmCGFunc::SelectCondGoto(LabelOperand *targetopnd, Opcode jmpop, Opcode cmpop, Operand *opnd0, Operand *opnd1,
                               PrimType primType) {
  Operand *rflag = GetOrCreateRflag();
  if (opnd0->op_kind_ != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, primType);
  }
  bool is64bits = GetPrimTypeBitSize(primType) == 64;
  bool isfloat = IsPrimitiveFloat(primType);
  if (isfloat) {
    if (opnd1->op_kind_ != Operand::Opd_Register) {
      opnd1 = SelectCopy(opnd1, primType);
    }
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2vcmpd : MOP_Tb2vcmps, rflag, opnd0, opnd1));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2fmstat, rflag));
  } else {
    bool isimm = opnd1->op_kind_ == Operand::Opd_Immediate;
    if (opnd1->op_kind_ != Operand::Opd_Register && !isimm) {
      opnd1 = SelectCopy(opnd1, primType);
    }
    if (is64bits) {
      CG_ASSERT(false, "NYI 64bits integer cmp");
    } else {
      MOperator mop = MOP_Tbcmprr;
      if (isimm) {
        ImmOperand *immopnd = static_cast<ImmOperand *>(opnd1);
        if (!immopnd->IsInBitSizeRot(8)) {
          opnd1 = SelectCopy(opnd1, primType);
        } else {
          mop = MOP_Tbcmpri8;
        }
      }
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(mop, rflag, opnd0, opnd1));
    }
  }
  MOperator jmp_op = PickJmpInsn(jmpop, cmpop, isfloat, IsSignedInteger(primType));
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(jmp_op, rflag, targetopnd));
}

void ArmCGFunc::SelectCondGoto(CondGotoNode *stmt, Operand *opnd0, Operand *opnd1) {
  LabelIdx labelidx = stmt->offset;
  base_node_t *condnode = stmt->Opnd(0);
  LabelOperand *targetopnd = GetOrCreateLabelOperand(labelidx);

  PrimType primType = condnode->primType;
  Opcode cmpop = kOpcodeInfo.IsCompare(condnode->op) ? condnode->op : OP_ne;
  SelectCondGoto(targetopnd, stmt->op, cmpop, opnd0, opnd1, primType);
}

void ArmCGFunc::SelectGoto(GotoNode *stmt) {
  Operand *targetopnd = GetOrCreateLabelOperand(stmt->offset);
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbbuncond, targetopnd));
}

Operand *ArmCGFunc::SelectAdd(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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

void ArmCGFunc::SelectAdd(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectAdd(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      ArmImmOperand *immopnd = static_cast<ArmImmOperand *>(opnd1);
      if (is64bits) {
        ImmOperand *lowopnd = GetOrCreateImmOperand(immopnd->GetLow32(), 8, false);
        ImmOperand *highopnd = GetOrCreateImmOperand(immopnd->GetHigh32(), 8, false);
        Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
        Operand *loresopnd = GetLow32bitsOpnd(resopnd);
        Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
        Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
        Operand *rflag = GetOrCreateRflag();
        if (lowopnd->IsInBitSizeRot(8)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbaddrri3s, rflag, loresopnd, loopnd0, lowopnd));
        } else {
          curbb->AppendInsn(
            cg->BuildInstruction<ArmInsn>(MOP_Tb2addrrr, rflag, loresopnd, loopnd0, SelectCopy(lowopnd, PTY_u32)));
        }
        if (highopnd->IsInBitSizeRot(8)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2adcrri8m, hiresopnd, hiopnd0, highopnd, rflag));
        } else {
          curbb->AppendInsn(
            cg->BuildInstruction<ArmInsn>(MOP_Tbadcrrr, hiresopnd, hiopnd0, SelectCopy(highopnd, PTY_u32), rflag));
        }
      } else {
        if (immopnd->IsInBitSize(8)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbaddrri8, resopnd, opnd0, opnd1));
        } else if (immopnd->IsInBitSize(12)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2addrri12, resopnd, opnd0, opnd1));
        } else {
          Operand *resopndt = CreateOpndOfType(prmtype);
          SelectCopyImm(resopndt, immopnd, prmtype);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbaddrrr, resopnd, opnd0, resopndt));
        }
      }
    } else {
      SelectAdd(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectAdd(resopnd, opnd1, opnd0, prmtype);
  } else {
    if (IsPrimitiveFloat(prmtype)) {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2vaddd : MOP_Tb2vadds, resopnd, opnd0, opnd1));
    } else if (IsPrimitiveInteger(prmtype)) {
      if (is64bits) {
        // emulate 64-bits add
        Operand *rflag = GetOrCreateRflag();
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2addrrr, rflag, GetLow32bitsOpnd(resopnd),
                                                        GetLow32bitsOpnd(opnd0), GetLow32bitsOpnd(opnd1)));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbadcrrr, GetHigh32bitsOpnd(resopnd),
                                                        GetHigh32bitsOpnd(opnd0), GetHigh32bitsOpnd(opnd1), rflag));
      } else {
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbaddrrr, resopnd, opnd0, opnd1));
      }
    } else {
      CG_ASSERT(false, "NYI add");
    }
  }
}

Operand *ArmCGFunc::SelectShift(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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

void ArmCGFunc::SelectSubI64(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  CG_ASSERT(GetPrimTypeBitSize(prmtype) == 64 && IsPrimitiveInteger(prmtype), "wrong type");
  if (opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      ArmImmOperand *immopnd = static_cast<ArmImmOperand *>(opnd1);
      ImmOperand *lowopnd = GetOrCreateImmOperand(immopnd->GetLow32(), 8, false);
      ImmOperand *highopnd = GetOrCreateImmOperand(immopnd->GetHigh32(), 8, false);
      Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
      Operand *loresopnd = GetLow32bitsOpnd(resopnd);
      Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
      Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
      Operand *rflag = GetOrCreateRflag();
      if (lowopnd->IsInBitSizeRot(8)) {
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2subsrri8, rflag, loresopnd, loopnd0, lowopnd));
      } else {
        curbb->AppendInsn(
          cg->BuildInstruction<ArmInsn>(MOP_Tb2subrrr, rflag, loresopnd, loopnd0, SelectCopy(lowopnd, PTY_u32)));
      }
      if (highopnd->IsInBitSizeRot(8)) {
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2sbcrri8m, hiresopnd, hiopnd0, highopnd, rflag));
      } else {
        curbb->AppendInsn(
          cg->BuildInstruction<ArmInsn>(MOP_Tb2sbcrrr, hiresopnd, hiopnd0, SelectCopy(highopnd, PTY_u32), rflag));
      }
    } else {
      SelectSubI64(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
    }
  } else {
    Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
    Operand *loresopnd = GetLow32bitsOpnd(resopnd);
    Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
    Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
    Operand *hiopnd1 = GetHigh32bitsOpnd(opnd1);
    Operand *loopnd1 = GetLow32bitsOpnd(opnd1);
    Operand *rflag = GetOrCreateRflag();
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2subrrr, rflag, loresopnd, loopnd0, loopnd1));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2sbcrrr, hiresopnd, hiopnd0, hiopnd1, rflag));
  }
}

void ArmCGFunc::SelectSub(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(prmtype);
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, prmtype);
  }
  if (is64bits && IsPrimitiveInteger(prmtype)) {
    SelectSubI64(resopnd, opnd0, opnd1, prmtype);
    return;
  }
  if (opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
      if (immopnd1->IsInRange(12, true)) {
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2subrri12, resopnd, opnd0, opnd1));
        return;
      }
    }
    SelectSub(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
  } else {
    if (isfloat) {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2vsubd : MOP_Tb2vsubs, resopnd, opnd0, opnd1));
    } else {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2subrrr, GetOrCreateRflag(), resopnd, opnd0, opnd1));
    }
  }
}

Operand *ArmCGFunc::SelectSub(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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

Operand *ArmCGFunc::SelectMpy(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectMpy(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void ArmCGFunc::SelectMpy(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectMpy(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectMpy(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectMpy(resopnd, opnd1, opnd0, prmtype);
  } else {
    if (IsPrimitiveFloat(prmtype)) {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2vmuld : MOP_Tb2vmuls, resopnd, opnd0, opnd1));
    } else if (IsPrimitiveInteger(prmtype)) {
      if (is64bits) {
        // emulation 64-bits mul
        // r0d = r0d * r2d =>
        // mul r3, r0, r3
        // mla r3, r2, r1, r3
        // umull r0, r1, r0, r2
        // add r1, r1, r3
        Operand *r3 = CreateOpndOfType(PTY_u32);
        Operand *r31 = CreateOpndOfType(PTY_u32);
        Operand *r1 = CreateOpndOfType(PTY_u32);
        Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
        Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
        Operand *hiopnd1 = GetHigh32bitsOpnd(opnd1);
        Operand *loopnd1 = GetLow32bitsOpnd(opnd1);
        Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
        Operand *loresopnd = GetLow32bitsOpnd(resopnd);
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2mulrrr, r3, loopnd0, hiopnd1));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2mla, r31, loopnd1, hiopnd0, r3));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2umull, loresopnd, r1, loopnd0, loopnd1));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbaddrrr, hiresopnd, r1, r31));
      } else {
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2mulrrr, resopnd, opnd0, opnd1));
      }
    }
  }
}

void ArmCGFunc::SelectDiv(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, prmtype);
  }
  if (opnd1ty != Operand::Opd_Register) {
    opnd1 = SelectCopy(opnd1, prmtype);
  }

  if (IsPrimitiveFloat(prmtype)) {
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2vdivd : MOP_Tb2vdivs, resopnd, opnd0, opnd1));
  } else if (IsPrimitiveInteger(prmtype)) {
    // generate lib call
    const char *func_name = nullptr;
    switch (prmtype) {
      case PTY_i32:
        func_name = "__aeabi_idiv";
        break;
      case PTY_u32:
        func_name = "__aeabi_uidiv";
        break;
      case PTY_i64:
        func_name = "__aeabi_ldivmod";
        break;
      case PTY_u64:
        func_name = "__aeabi_uldivmod";
        break;
      default:
        CG_ASSERT(false, "not supposed to be here");
    }
    vector<Operand *> opndvec;
    opndvec.push_back(resopnd);
    opndvec.push_back(opnd0);
    opndvec.push_back(opnd1);
    SelectLibCall(func_name, opndvec, prmtype, prmtype);
  }
}

Operand *ArmCGFunc::SelectDiv(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
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

void ArmCGFunc::SelectRem(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, prmtype);
  }
  if (opnd1ty != Operand::Opd_Register) {
    opnd1 = SelectCopy(opnd1, prmtype);
  }

  if (IsPrimitiveInteger(prmtype)) {
    // generate lib call
    const char *func_name = nullptr;
    switch (prmtype) {
      case PTY_i32:
        func_name = "__aeabi_idivmod";
        break;
      case PTY_u32:
        func_name = "__aeabi_uidivmod";
        break;
      case PTY_i64:
        func_name = "__aeabi_ldivmod";
        break;
      case PTY_u64:
        func_name = "__aeabi_uldivmod";
        break;
      default:
        CG_ASSERT(false, "not supposed to be here");
    }
    vector<Operand *> opndvec;
    opndvec.push_back(resopnd);
    opndvec.push_back(opnd0);
    opndvec.push_back(opnd1);
    SelectLibCall(func_name, opndvec, prmtype, prmtype, true);
  } else {
    CG_ASSERT(false, "wrong type for rem");
  }
}

Operand *ArmCGFunc::SelectRem(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectRem(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void ArmCGFunc::SelectTest(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType dtype) {
  CG_ASSERT(false, "nyi");
}

Operand *ArmCGFunc::SelectLand(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType prmtype = node->primType;
  CG_ASSERT(IsPrimitiveInteger(prmtype), "Land should be integer type");
  bool is64bits = GetPrimTypeBitSize(prmtype) == 64;
  Operand *resopnd = CreateOpndOfType(is64bits ? PTY_u64 : PTY_u32);
  Operand *andres = CreateOpndOfType(prmtype);
  SelectBand(andres, opnd0, opnd1, prmtype);
  SelectCmpOp(is64bits ? GetLow32bitsOpnd(resopnd) : resopnd, andres, GetOrCreateImmOperand(0, prmtype, false), OP_ne,
              prmtype);
  if (is64bits) {
    SelectCopy(GetHigh32bitsOpnd(resopnd), GetOrCreateImmOperand(0, 32, false), PTY_u32);
  }
  return resopnd;
}

Operand *ArmCGFunc::SelectLor(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType prmtype = node->primType;
  CG_ASSERT(IsPrimitiveInteger(prmtype), "Lior should be integer type");
  bool is64bits = GetPrimTypeBitSize(prmtype) == 64;
  Operand *resopnd = CreateOpndOfType(is64bits ? PTY_u64 : PTY_u32);
  Operand *orres = CreateOpndOfType(prmtype);
  SelectBior(orres, opnd0, opnd1, prmtype);
  SelectCmpOp(is64bits ? GetLow32bitsOpnd(resopnd) : resopnd, orres, GetOrCreateImmOperand(0, prmtype, false), OP_ne,
              prmtype);
  if (is64bits) {
    SelectCopy(GetHigh32bitsOpnd(resopnd), GetOrCreateImmOperand(0, 32, false), PTY_u32);
  }
  return resopnd;
}

void ArmCGFunc::SelectCmpOpi64(Operand *resopnd, Operand *opnd0, Operand *opnd1, Opcode opcode, PrimType prmtype) {
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(prmtype);
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  CG_ASSERT(is64bits && !isfloat, "this is a 64-bits emulation of compare");
  Operand *rflag = GetOrCreateRflag();
  if (opcode == OP_ne || opcode == OP_eq) {
    // for ne
    // resopnd = opnd0 ne opnd1
    // eor r1, hiopnd0, hiopnd1
    // eor r0, loopnd0, loopnd1
    // orrs  resopnd, r0, r1
    // it ne
    // movne resopnd, #1
    // for eq
    // eor r1, r1, r3
    // eor r0, r0, r2
    // orrs  r0v1, r0, r1
    // mov resopnd, #0
    // moveq resopnd, #1
    Operand *r01 = CreateOpndOfType(prmtype);
    SelectBxor(r01, opnd0, opnd1, prmtype);
    Operand *hir01 = GetHigh32bitsOpnd(r01);
    Operand *lor01 = GetLow32bitsOpnd(r01);
    if (opcode == OP_ne) {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrsrrr, rflag, resopnd, hir01, lor01));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbitne));
      curbb->AppendInsn(
        cg->BuildInstruction<ArmInsn>(MOP_Tb2movneimm12, resopnd, GetOrCreateImmOperand(1, PTY_u32, false), rflag));
      curbb->lastinsn->SetCondDef();
    } else {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrsrrr, rflag, CreateOpndOfType(PTY_u32), hir01, lor01));
      curbb->AppendInsn(
        cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm12, resopnd, GetOrCreateImmOperand(0, PTY_u32, false), rflag));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbiteq));
      curbb->AppendInsn(
        cg->BuildInstruction<ArmInsn>(MOP_Tb2moveqimm12, resopnd, GetOrCreateImmOperand(1, PTY_u32, false), rflag));
      curbb->lastinsn->SetCondDef();
    }
  } else {
    // emulation 64-bits
    // resopnd =  opnd0 > opnd1 ? 1 : 0;
    // mov r0v0, 0
    //  mov r12, 0
    //  cmp loopnd0, loopnd1
    //  it hi
    //  movhi r0v0, 1
    //  cmp hiopnd0, hiopnd1
    //  it gt
    //  movgt r12, #1
    //  it eq
    //  moveq r12, r0v0
    //  mov resopnd,r12
    Operand *imm0 = GetOrCreateImmOperand(0, PTY_u32, false);
    Operand *opndr0v0 = SelectCopy(imm0, PTY_u32);
    Operand *opndr12 = SelectCopy(imm0, PTY_u32);
    Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
    Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
    Operand *hiopnd1 = nullptr;
    Operand *loopnd1 = nullptr;
    Operand *hiopnd1imm = nullptr;
    Operand *loopnd1imm = nullptr;
    Insn *cmpinsn1 = nullptr;
    if (opnd1ty == Operand::Opd_Immediate) {
      ArmImmOperand *immopnd = static_cast<ArmImmOperand *>(opnd1);
      ImmOperand *lowopnd = GetOrCreateImmOperand(immopnd->GetLow32(), 8, false);
      ImmOperand *highopnd = GetOrCreateImmOperand(immopnd->GetHigh32(), 8, false);
      if (lowopnd->IsInBitSizeRot(8)) {
        loopnd1imm = lowopnd;
      } else {
        loopnd1 = SelectCopy(lowopnd, PTY_u32);
      }
      if (highopnd->IsInBitSizeRot(8)) {
        hiopnd1imm = highopnd;
      } else {
        hiopnd1 = SelectCopy(highopnd, PTY_u32);
      }
    } else if (opnd1ty != Operand::Opd_Register) {
      opnd1 = SelectCopy(opnd1, prmtype);
      hiopnd1 = GetHigh32bitsOpnd(opnd1);
      loopnd1 = GetLow32bitsOpnd(opnd1);
    } else {
      hiopnd1 = GetHigh32bitsOpnd(opnd1);
      loopnd1 = GetLow32bitsOpnd(opnd1);
    }
    cmpinsn1 = cg->BuildInstruction<ArmInsn>(loopnd1imm ? MOP_Tbcmpri8 : MOP_Tbcmprr, rflag, loopnd0,
                                             loopnd1imm ? loopnd1imm : loopnd1);
    curbb->AppendInsn(cmpinsn1);
    MOperator movop;
    MOperator itop;
    PickCmovInsn(opcode, PTY_u32, itop, movop);
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(itop));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(movop, opndr0v0, GetOrCreateImmOperand(1, PTY_u32, false), rflag));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(hiopnd1imm ? MOP_Tbcmpri8 : MOP_Tbcmprr, rflag, hiopnd0,
                                                    hiopnd1imm ? hiopnd1imm : hiopnd1));
    PickCmovInsn(opcode, prmtype, itop, movop);
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(itop));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(movop, opndr12, GetOrCreateImmOperand(1, PTY_u32, false), rflag));
    curbb->lastinsn->SetCondDef();
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbiteq));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2moveqrr, opndr12, opndr0v0, rflag));
    curbb->lastinsn->SetCondDef();
    SelectCopy(resopnd, opndr12, PTY_u32);
  }
}

void ArmCGFunc::SelectCmpOp(Operand *resopnd, Operand *opnd0, Operand *opnd1, Opcode opcode, PrimType prmtype) {
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(prmtype);
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, prmtype);
  }
  if (isfloat) {
    MOperator movop;
    MOperator itop;
    PickCmovInsn(opcode, prmtype, itop, movop);
    if (opnd1ty != Operand::Opd_Register) {
      opnd1 = SelectCopy(opnd1, prmtype);
    }
    Operand *rflag = GetOrCreateRflag();
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2vcmpd : MOP_Tb2vcmps, rflag, opnd0, opnd1));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(itop));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(movop, resopnd, GetOrCreateImmOperand(1, PTY_u32, false), rflag));
  } else {
    if (is64bits) {
      SelectCmpOpi64(resopnd, opnd0, opnd1, opcode, prmtype);
    } else {
      SelectCopy(resopnd, GetOrCreateImmOperand(0, PTY_u32, false), PTY_u32);
      MOperator movop;
      MOperator itop;
      PickCmovInsn(opcode, prmtype, itop, movop);
      Operand *rflag = GetOrCreateRflag();
      if (opnd1ty == Operand::Opd_Immediate) {
        ImmOperand *immopnd = static_cast<ImmOperand *>(opnd1);
        if (immopnd->IsInBitSizeRot(8)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbcmpri8, rflag, opnd0, opnd1));
        } else {
          opnd1 = SelectCopy(opnd1, prmtype);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbcmprr, rflag, opnd0, opnd1));
        }
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(itop));
        curbb->AppendInsn(
          cg->BuildInstruction<ArmInsn>(movop, resopnd, GetOrCreateImmOperand(1, PTY_u32, false), rflag));
        return;
      } else if (opnd1ty != Operand::Opd_Register) {
        opnd1 = SelectCopy(opnd1, prmtype);
      }
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbcmprr, rflag, opnd0, opnd1));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(itop));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(movop, resopnd, GetOrCreateImmOperand(1, PTY_u32, false), rflag));
    }
  }
}

Operand *ArmCGFunc::SelectCmpOp(CompareNode *node, Operand *opnd0, Operand *opnd1) {
  Operand *resopnd = CreateOpndOfType(PTY_u32);
  SelectCmpOp(resopnd, opnd0, opnd1, node->op, node->primType);
  return resopnd;
}

Operand *ArmCGFunc::SelectBand(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectBand(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void ArmCGFunc::SelectBand(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectBand(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      ArmImmOperand *immopnd = static_cast<ArmImmOperand *>(opnd1);
      if (is64bits) {
        uint32 low_val = immopnd->GetLow32();
        uint32 high_val = immopnd->GetHigh32();
        if (ImmOperand::IsInBitSizeRot(8, low_val) && ImmOperand::IsInBitSizeRot(8, high_val)) {
          curbb->AppendInsn(
            cg->BuildInstruction<ArmInsn>(MOP_Tbandrri8l, resopnd, opnd0, GetOrCreateImmOperand(low_val, 8, false)));
          curbb->AppendInsn(
            cg->BuildInstruction<ArmInsn>(MOP_Tbandrri8h, resopnd, opnd0, GetOrCreateImmOperand(high_val, 8, false)));
        } else {
          SelectBand(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
        }
      } else {
        if (immopnd->IsInBitSizeRot(8)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbandrri8, resopnd, opnd0, opnd1));
        } else {
          Operand *resopndt = CreateOpndOfType(prmtype);
          SelectCopyImm(resopndt, immopnd, prmtype);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2andrrr, resopnd, opnd0, resopndt));
        }
      }
    } else {
      SelectBand(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBand(resopnd, opnd1, opnd0, prmtype);
  } else {
    CG_ASSERT(IsPrimitiveInteger(prmtype), "NYI band");
    if (is64bits) {
      Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
      Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
      Operand *hiopnd1 = GetHigh32bitsOpnd(opnd1);
      Operand *loopnd1 = GetLow32bitsOpnd(opnd1);
      Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
      Operand *loresopnd = GetLow32bitsOpnd(resopnd);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2andrrr, loresopnd, loopnd0, loopnd1));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2andrrr, hiresopnd, hiopnd0, hiopnd1));
    } else {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2andrrr, resopnd, opnd0, opnd1));
    }
  }
}

Operand *ArmCGFunc::SelectBior(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectBior(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void ArmCGFunc::SelectBior(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectBior(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      ArmImmOperand *immopnd = static_cast<ArmImmOperand *>(opnd1);
      if (is64bits) {
        ImmOperand *lowopnd = GetOrCreateImmOperand(immopnd->GetLow32(), 8, false);
        ImmOperand *highopnd = GetOrCreateImmOperand(immopnd->GetHigh32(), 8, false);
        Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
        Operand *loresopnd = GetLow32bitsOpnd(resopnd);
        Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
        Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
        if (lowopnd->IsInBitSizeRot(8)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tborrri8, loresopnd, loopnd0, lowopnd));
        } else {
          curbb->AppendInsn(
            cg->BuildInstruction<ArmInsn>(MOP_Tb2orrrrr, loresopnd, loopnd0, SelectCopy(lowopnd, PTY_u32)));
        }

        if (highopnd->IsInBitSizeRot(8)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tborrri8, hiresopnd, hiopnd0, highopnd));
        } else {
          curbb->AppendInsn(
            cg->BuildInstruction<ArmInsn>(MOP_Tb2orrrrr, hiresopnd, hiopnd0, SelectCopy(highopnd, PTY_u32)));
        }
      } else {
        if (immopnd->IsInBitSizeRot(8)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tborrri8, resopnd, opnd0, opnd1));
        } else {
          Operand *resopndt = CreateOpndOfType(prmtype);
          SelectCopyImm(resopndt, immopnd, prmtype);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrrrr, resopnd, opnd0, resopndt));
        }
      }
    } else {
      SelectBior(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBior(resopnd, opnd1, opnd0, prmtype);
  } else {
    CG_ASSERT(IsPrimitiveInteger(prmtype), "NYI band");
    if (is64bits) {
      Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
      Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
      Operand *hiopnd1 = GetHigh32bitsOpnd(opnd1);
      Operand *loopnd1 = GetLow32bitsOpnd(opnd1);
      Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
      Operand *loresopnd = GetLow32bitsOpnd(resopnd);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrrrr, loresopnd, loopnd0, loopnd1));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrrrr, hiresopnd, hiopnd0, hiopnd1));
    } else {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrrrr, resopnd, opnd0, opnd1));
    }
  }
}

Operand *ArmCGFunc::SelectMin(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectMin(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void ArmCGFunc::SelectMin(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  if (IsPrimitiveInteger(prmtype)) {
    Operand *cmpres = CreateOpndOfType(PTY_u32);
    SelectCmpOp(cmpres, opnd0, opnd1, OP_lt, prmtype);
    SelectSelect(resopnd, cmpres, opnd0, opnd1, prmtype, PTY_u32);
    return;
  } else if (IsPrimitiveFloat(prmtype)) {
    // generate lib call
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    if (opnd1ty != Operand::Opd_Register) {
      opnd1 = SelectCopy(opnd1, prmtype);
    }
    const char *func_name = nullptr;
    switch (prmtype) {
      case PTY_f32:
        func_name = "fminf";
        break;
      case PTY_f64:
        func_name = "fmin";
        break;
      default:
        CG_ASSERT(false, "not supposed to be here");
    }
    vector<Operand *> opndvec;
    opndvec.push_back(resopnd);
    opndvec.push_back(opnd0);
    opndvec.push_back(opnd1);
    SelectLibCall(func_name, opndvec, prmtype, prmtype);
  }
}

Operand *ArmCGFunc::SelectMax(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  bool isfloat = IsPrimitiveFloat(dtype);
  PrimType prmtype =
    isfloat ? dtype : ((is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32)));  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectMax(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void ArmCGFunc::SelectMax(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  if (IsPrimitiveInteger(prmtype)) {
    Operand *cmpres = CreateOpndOfType(PTY_u32);
    SelectCmpOp(cmpres, opnd0, opnd1, OP_gt, prmtype);
    SelectSelect(resopnd, cmpres, opnd0, opnd1, prmtype, PTY_u32);
    return;
  } else if (IsPrimitiveFloat(prmtype)) {
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    if (opnd1ty != Operand::Opd_Register) {
      opnd1 = SelectCopy(opnd1, prmtype);
    }
    // generate lib call
    const char *func_name = nullptr;
    switch (prmtype) {
      case PTY_f32:
        func_name = "fmaxf";
        break;
      case PTY_f64:
        func_name = "fmax";
        break;
      default:
        CG_ASSERT(false, "not supposed to be here");
    }
    vector<Operand *> opndvec;
    opndvec.push_back(resopnd);
    opndvec.push_back(opnd0);
    opndvec.push_back(opnd1);
    SelectLibCall(func_name, opndvec, prmtype, prmtype);
  } else {
    CG_ASSERT(false, "NIY type max");
  }
}

Operand *ArmCGFunc::SelectBxor(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtype = node->primType;
  bool issigned = IsSignedInteger(dtype);
  uint32 dsize = GetPrimTypeBitSize(dtype);
  bool is64bits = (dsize == 64);
  PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(prmtype);
  SelectBxor(resopnd, opnd0, opnd1, prmtype);
  return resopnd;
}

void ArmCGFunc::SelectBxor(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    SelectBxor(resopnd, SelectCopy(opnd0, prmtype), opnd1, prmtype);
  } else if (opnd0ty == Operand::Opd_Register && opnd1ty != Operand::Opd_Register) {
    if (opnd1ty == Operand::Opd_Immediate) {
      ArmImmOperand *immopnd = static_cast<ArmImmOperand *>(opnd1);
      if (is64bits) {
        ImmOperand *lowopnd = GetOrCreateImmOperand(immopnd->GetLow32(), 8, false);
        ImmOperand *highopnd = GetOrCreateImmOperand(immopnd->GetHigh32(), 8, false);
        Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
        Operand *loresopnd = GetLow32bitsOpnd(resopnd);
        Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
        Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
        if (lowopnd->IsInBitSizeRot(8)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2eorrri8m, loresopnd, loopnd0, lowopnd));
        } else {
          curbb->AppendInsn(
            cg->BuildInstruction<ArmInsn>(MOP_Tb2eorrrr, loresopnd, loopnd0, SelectCopy(lowopnd, PTY_u32)));
        }

        if (highopnd->IsInBitSizeRot(8)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2eorrri8m, hiresopnd, hiopnd0, highopnd));
        } else {
          curbb->AppendInsn(
            cg->BuildInstruction<ArmInsn>(MOP_Tb2eorrrr, hiresopnd, hiopnd0, SelectCopy(highopnd, PTY_u32)));
        }
      } else {
        if (immopnd->IsInBitSizeRot(8)) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2eorrri8m, resopnd, opnd0, opnd1));
        } else {
          Operand *resopndt = CreateOpndOfType(prmtype);
          SelectCopyImm(resopndt, immopnd, prmtype);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2eorrrr, resopnd, opnd0, resopndt));
        }
      }
    } else {
      SelectBxor(resopnd, opnd0, SelectCopy(opnd1, prmtype), prmtype);
    }
  } else if (opnd0ty != Operand::Opd_Register && opnd1ty == Operand::Opd_Register) {
    SelectBxor(resopnd, opnd1, opnd0, prmtype);
  } else {
    CG_ASSERT(IsPrimitiveInteger(prmtype), "NYI bxor");
    if (is64bits) {
      Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
      Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
      Operand *hiopnd1 = GetHigh32bitsOpnd(opnd1);
      Operand *loopnd1 = GetLow32bitsOpnd(opnd1);
      Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
      Operand *loresopnd = GetLow32bitsOpnd(resopnd);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2eorrrr, loresopnd, loopnd0, loopnd1));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2eorrrr, hiresopnd, hiopnd0, hiopnd1));
    } else {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2eorrrr, resopnd, opnd0, opnd1));
    }
  }
}

void ArmCGFunc::SelectShift64(Operand *resopnd, Operand *opnd0, Operand *opnd1, SHIFTDIRECTION direct,
                              PrimType prmtype) {
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  if (opnd1ty == Operand::Opd_Immediate) {
    ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
    const int64 val = immopnd1->GetValue();
    if (val == 0) {
      SelectCopy(resopnd, opnd0, prmtype);
      return;
    }
    const uint8 shiftamt = 63;
    CG_ASSERT(shiftamt >= val, "shift error oversize");
    switch (direct) {
      case ShiftLeft: {
        if (val == 1) {
          SelectAdd(resopnd, opnd0, opnd0, prmtype);
          return;
        }
        Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
        Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
        Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
        Operand *loresopnd = GetLow32bitsOpnd(resopnd);
        if (val == 32) {
          SelectCopy(loresopnd, GetOrCreateImmOperand(0, 32, false), PTY_u32);
          SelectCopy(hiresopnd, loopnd0, PTY_u32);
          return;
        }
        if (val < 32) {
          // opnd0 << 30 =>
          // mov r3v0, r0(loopnd0)
          // lsls  r1v0, r1(hiopnd0), #30
          // lsls  loresopnd, r0(loopnd0), #30
          // orr hiresopnd, r1v0, r3v0, lsr #2
          Operand *r1v0 = CreateOpndOfType(PTY_u32);
          Operand *immopnd = GetOrCreateImmOperand(val, 32, false);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lslrri5, r1v0, hiopnd0, immopnd));
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lslrri5, loresopnd, loopnd0, immopnd));
          Operand *r3v0 = CreateOpndOfType(PTY_u32);
          SelectShift(r3v0, loopnd0, GetOrCreateImmOperand(32 - val, 32, false), ShiftLright, PTY_u32);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrrrr, hiresopnd, r1v0, r3v0));
          return;
        } else {
          // opnd0 << 55 =>
          // lsls  hiresopnd, loopnd0, #23;
          // movs  loresopnd, #0
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lslrri5, hiresopnd, loopnd0,
                                                          GetOrCreateImmOperand(val - 32, PTY_u32, false)));
          curbb->AppendInsn(
            cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm12, loresopnd, GetOrCreateImmOperand(0, PTY_u32, false)));
        }
      } break;
      case ShiftAright: {
        Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
        Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
        Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
        Operand *loresopnd = GetLow32bitsOpnd(resopnd);
        if (val == 32) {
          SelectCopy(loresopnd, hiopnd0, PTY_u32);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2asrrri5, hiresopnd, hiopnd0,
                                                          GetOrCreateImmOperand(31, PTY_u32, false)));
        } else if (val > 32) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2asrrri5, loresopnd, hiopnd0,
                                                          GetOrCreateImmOperand(val - 32, 32, false)));
          curbb->AppendInsn(
            cg->BuildInstruction<ArmInsn>(MOP_Tb2asrrri5, hiresopnd, hiopnd0, GetOrCreateImmOperand(31, 32, false)));
        } else {
          // resopnd = opnd0 ashr 30 =>
          // lsrs  r0v0, r0(loopnd0), #30;
          // orr r0(loresopnd), r0v0, r1(hiopnd0), lsl #2;
          // asrs  r1(hiresopnd), r1(hiopnd0), #30
          Operand *r0v0 = CreateOpndOfType(PTY_u32);
          Operand *rlsl = CreateOpndOfType(PTY_u32);
          Operand *valopnd = GetOrCreateImmOperand(val, 32, false);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lsrrri5, r0v0, loopnd0, valopnd));
          SelectShift(rlsl, hiopnd0, GetOrCreateImmOperand(32 - val, 32, false), ShiftLeft, PTY_u32);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrrrr, loresopnd, r0v0, rlsl));
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2asrrri5, hiresopnd, hiopnd0, valopnd));
        }
      } break;
      case ShiftLright: {
        Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
        Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
        Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
        Operand *loresopnd = GetLow32bitsOpnd(resopnd);
        if (val == 32) {
          SelectCopy(loresopnd, hiopnd0, PTY_u32);
          SelectCopy(hiresopnd, GetOrCreateImmOperand(0, 32, false), PTY_u32);
        } else if (val > 32) {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lsrrri5, loresopnd, hiopnd0,
                                                          GetOrCreateImmOperand(val - 32, 32, false)));
          SelectCopy(hiresopnd, GetOrCreateImmOperand(0, 32, false), PTY_u32);
        } else {
          Operand *r0v0 = CreateOpndOfType(PTY_u32);
          Operand *rlsl = CreateOpndOfType(PTY_u32);
          Operand *valopnd = GetOrCreateImmOperand(val, 32, false);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lsrrri5, r0v0, loopnd0, valopnd));
          SelectShift(rlsl, hiopnd0, GetOrCreateImmOperand(32 - val, 32, false), ShiftLeft, PTY_u32);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrrrr, loresopnd, r0v0, rlsl));
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lsrrri5, hiresopnd, hiopnd0, valopnd));
        }
      } break;
      default:
        CG_ASSERT(false, "can not go there");
    }
  } else {
    if (opnd1ty != Operand::Opd_Register) {
      opnd1 = SelectCopy(opnd1, prmtype);
    }
    switch (direct) {
      case ShiftLeft: {
        // algorithm in SSA format
        // sub r4v0, r2(opnd1_lo), #32
        // rsb r3v0, r2(opnd1_lo), #32
        // lsls  r1v0, r1(opnd0_hi), r2(opnd1_lo)
        // lsl r4v1, r0(opnd0_lo), r4v0
        // orrs  r1v1, r1v0, r4v1
        // lsr r3v1, r0(opnd0_lo), r3v0
        // lsls  res_lo, r0(opnd0_lo), r2(opnd1_lo)
        // orrs  res_hi, r1v1, r3v1
        Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
        Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
        Operand *loopnd1 = GetLow32bitsOpnd(opnd1);
        Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
        Operand *loresopnd = GetLow32bitsOpnd(resopnd);
        Operand *r4v0 = CreateOpndOfType(PTY_u32);
        Operand *r3v0 = CreateOpndOfType(PTY_u32);
        Operand *r1v0 = CreateOpndOfType(PTY_u32);
        Operand *r4v1 = CreateOpndOfType(PTY_u32);
        Operand *r1v1 = CreateOpndOfType(PTY_u32);
        Operand *r3v1 = CreateOpndOfType(PTY_u32);
        Operand *imm32 = GetOrCreateImmOperand(32, PTY_i32, false);
        Operand *rflag = GetOrCreateRflag();
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2subrri12, r4v0, loopnd1, imm32));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2rsubrri8, r3v0, loopnd1, imm32));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tblslsrrr, rflag, r1v0, hiopnd0, loopnd1));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lslrrr, r4v1, loopnd0, r4v0));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrsrrr, rflag, r1v1, r1v0, r4v1));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lsrrrr, r3v1, loopnd0, r3v0));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tblslsrrr, rflag, loresopnd, loopnd0, loopnd1));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrsrrr, rflag, hiresopnd, r1v1, r3v1));
        break;
      }
      case ShiftAright: {
        // algorithm in SSA format
        // rsb r3v0, r2(loopnd1), #32
        // subs  r4v0, r2(loopnd1), #32
        // lsr r0v0, r0(loopnd0), r2(loopnd1)
        // lsl r3v1, r1(hiopnd0), r3v0
        // orr loresopnd, r0v0, r3v1
        // itt pl
        // asrpl r4v1, r1(hiopnd0), r4v0
        // orrpl loresopnd, loresopnd, r4v1
        // asrs  hiresopnd, r1(hiopnd0), r2(loopnd1)
        Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
        Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
        Operand *loopnd1 = GetLow32bitsOpnd(opnd1);
        Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
        Operand *loresopnd = GetLow32bitsOpnd(resopnd);
        Operand *r4v0 = CreateOpndOfType(PTY_u32);
        Operand *r3v0 = CreateOpndOfType(PTY_u32);
        Operand *r0v0 = CreateOpndOfType(PTY_u32);
        Operand *r4v1 = CreateOpndOfType(PTY_u32);
        Operand *r3v1 = CreateOpndOfType(PTY_u32);
        Operand *imm32 = GetOrCreateImmOperand(32, PTY_i32, false);
        Operand *rflag = GetOrCreateRflag();
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2rsubrri8, r3v0, loopnd1, imm32));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2subsrri8, rflag, r4v0, loopnd1, imm32));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lsrrrr, r0v0, loopnd0, loopnd1));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lslrrr, r3v1, hiopnd0, r3v0));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrrrr, loresopnd, r0v0, r3v1));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbittpl, rflag));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2asrplrrr, r4v1, hiopnd0, r4v0, rflag));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrplrrr, loresopnd, loresopnd, r4v1, rflag));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbasrsrrr, rflag, hiresopnd, hiopnd0, loopnd1));
        break;
      }
      case ShiftLright: {
        // rsb r4v0, r2(loopnd1), #32
        // sub r3v0, r2(loopnd1), #32
        // lsrs  r0v0, r0(loopnd0), r2(loopnd1)
        // lsl r4v1, r1(hiopnd0), r4v0
        // orrs  r0v1, r0v0, r4v1
        // lsr r3v1, r1(hiopnd0), r3v0
        // orrs  loresopnd, r0v1, r3v1
        // lsrs  hiresopnd, r1(hiopnd0), r2(loopnd1)
        Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
        Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
        Operand *loopnd1 = GetLow32bitsOpnd(opnd1);
        Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
        Operand *loresopnd = GetLow32bitsOpnd(resopnd);
        Operand *r4v0 = CreateOpndOfType(PTY_u32);
        Operand *r3v0 = CreateOpndOfType(PTY_u32);
        Operand *r0v0 = CreateOpndOfType(PTY_u32);
        Operand *r0v1 = CreateOpndOfType(PTY_u32);
        Operand *r4v1 = CreateOpndOfType(PTY_u32);
        Operand *r3v1 = CreateOpndOfType(PTY_u32);
        Operand *imm32 = GetOrCreateImmOperand(32, PTY_i32, false);
        Operand *rflag = GetOrCreateRflag();
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2rsubrri8, r4v0, loopnd1, imm32));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2subsrri8, rflag, r3v0, loopnd1, imm32));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tblsrsrrr, rflag, r0v0, loopnd0, loopnd1));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lslrrr, r4v1, hiopnd0, r4v0));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrsrrr, rflag, r0v1, r0v0, r4v1));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2lsrrrr, r3v1, hiopnd0, r3v0));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrsrrr, rflag, loresopnd, r0v1, r3v1));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tblsrsrrr, rflag, hiresopnd, hiopnd0, loopnd1));
        break;
      }
      default:
        CG_ASSERT(false, "shouldn't be here shift64 register");
    }
  }
}

void ArmCGFunc::SelectShift(Operand *resopnd, Operand *opnd0, Operand *opnd1, SHIFTDIRECTION direct, PrimType prmtype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  Operand::OperandType opnd1ty = opnd1->op_kind_;
  uint32 dsize = GetPrimTypeBitSize(prmtype);
  bool is64bits = (dsize == 64);
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, prmtype);
  }
  if (is64bits) {
    SelectShift64(resopnd, opnd0, opnd1, direct, prmtype);
    return;
  }
  MOperator mop = 0;
  if (opnd1ty == Operand::Opd_Immediate) {
    ImmOperand *immopnd1 = static_cast<ImmOperand *>(opnd1);
    const int64 val = immopnd1->GetValue();
    const uint8 shiftamt = 31;
    if (val == 0) {
      SelectCopy(resopnd, opnd0, prmtype);
      return;
    }
    CG_ASSERT(shiftamt >= val, "shift error oversize");
    switch (direct) {
      case ShiftLeft:
        if (val == 1) {
          SelectAdd(resopnd, opnd0, opnd0, prmtype);
          return;
        }
        mop = MOP_Tb2lslrri5;
        break;
      case ShiftAright:
        mop = MOP_Tb2asrrri5;
        break;
      case ShiftLright:
        mop = MOP_Tb2lsrrri5;
        break;
        opnd1 = GetOrCreateImmOperand(val & shiftamt, 32, false);
    }
  } else if (opnd1ty != Operand::Opd_Register) {
    SelectShift(resopnd, opnd0, SelectCopy(opnd1, prmtype), direct, prmtype);
    return;
  } else {
    switch (direct) {
      case ShiftLeft:
        mop = MOP_Tb2lslrrr;
        break;
      case ShiftAright:
        mop = MOP_Tb2asrrrr;
        break;
      case ShiftLright:
        mop = MOP_Tb2lsrrrr;
        break;
    }
  }
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(mop, resopnd, opnd0, opnd1));
}

Operand *ArmCGFunc::SelectAbs(UnaryNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (IsPrimitiveFloat(dtyp)) {
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, dtyp);
    }
    Operand *resopnd = CreateOpndOfType(dtyp);
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2vabsd : MOP_Tb2vabss, resopnd, opnd0));
    return resopnd;
  } else {
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    PrimType prmtype = is64bits ? (PTY_i64) : (PTY_i32);  // promoted type
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    Operand *rflag = GetOrCreateRflag();
    Operand *resopnd = CreateOpndOfType(prmtype);
    if (is64bits) {
      // adds  r0, r0, r1, asr #31
      // adc r2, r1, r1, asr #31
      // eor r0, r0, r1, asr #31
      // eor r1, r2, r1, asr #31
      Operand *r0 = CreateOpndOfType(PTY_i32);
      Operand *r2 = CreateOpndOfType(PTY_i32);
      Operand *r1s = GetHigh32bitsOpnd(opnd0);
      Operand *r0s = GetLow32bitsOpnd(opnd0);
      Operand *rshift = CreateOpndOfType(PTY_i32);
      SelectShift(rshift, r1s, GetOrCreateImmOperand(31, 8, false), ShiftAright, PTY_i32);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2addrrr, rflag, r0, r0s, rshift));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbadcrrr, r2, r1s, rshift, rflag));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2eorrrr, GetLow32bitsOpnd(resopnd), r0, rshift));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2eorrrr, GetHigh32bitsOpnd(resopnd), r2, rshift));
    } else {
      // cmp r0, #0
      // rsbmi r0, r0, #0
      SelectCopy(resopnd, opnd0, prmtype);
      Operand *zeroopnd = GetOrCreateImmOperand(0, 32, false);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbcmpri8, rflag, opnd0, zeroopnd));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbitmi));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbrsbmiri, resopnd, opnd0, zeroopnd, rflag));
      curbb->lastinsn->SetCondDef();
    }
    return resopnd;
  }
}

Operand *ArmCGFunc::SelectBnot(UnaryNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  Operand *resopnd = nullptr;
  if (IsPrimitiveInteger(dtyp)) {
    Operand::OperandType opnd0ty = opnd0->op_kind_;
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    bool issigned = IsSignedInteger(dtyp);
    PrimType prmtype = is64bits ? (issigned ? PTY_i64 : PTY_u64) : (issigned ? PTY_i32 : PTY_u32);  // promoted type
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    resopnd = CreateOpndOfType(prmtype);
    if (is64bits) {
      Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
      Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
      Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
      Operand *loresopnd = GetLow32bitsOpnd(resopnd);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2mnvrr, loresopnd, loopnd0));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2mnvrr, hiresopnd, hiopnd0));
    } else {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2mnvrr, resopnd, opnd0));
    }
  } else {
    CG_ASSERT(false, "bnot expect integer or NYI");
  }
  return resopnd;
}

Operand *ArmCGFunc::SelectExtractbits(ExtractbitsNode *node, Operand *opnd0) {
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

void ArmCGFunc::SelectDepositbits(Operand *resopnd, Operand *opnd0, Operand *opnd1, uint32 boffset, uint32 bsize,
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

Operand *ArmCGFunc::SelectDepositbits(DepositbitsNode *node, Operand *opnd0, Operand *opnd1) {
  PrimType dtyp = node->primType;
  Operand *resopnd = CreateOpndOfType(dtyp);
  SelectDepositbits(resopnd, opnd0, opnd1, node->boffset, node->bsize, dtyp, dtyp);
  return resopnd;
}

Operand *ArmCGFunc::SelectLnot(UnaryNode *node, Operand *opnd0) {
  PrimType dtype = node->primType;
  Operand *resopnd = CreateOpndOfType(dtype);
  Operand *rcc = GetOrCreateRflag();
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, dtype);
  }
  bool is64bits = (GetPrimTypeBitSize(dtype) == 64);
  if (is64bits) {
    // orr r0v0, hiopnd0, loopnd0
    // resopnd = 0
    // cmp r0v0, #0
    // moveq loresopnd, 1
    Operand *r0v0 = CreateOpndOfType(dtype);
    Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
    Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
    Operand *loresopnd = GetLow32bitsOpnd(resopnd);
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2orrrrr, r0v0, hiopnd0, loopnd0));
    SelectCopy(resopnd, GetOrCreateImmOperand(0, 64, false), PTY_u64);
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbcmpri8, rcc, r0v0, GetOrCreateImmOperand(0, 32, false)));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbiteq));
    curbb->AppendInsn(
      cg->BuildInstruction<ArmInsn>(MOP_Tb2moveqimm12, loresopnd, GetOrCreateImmOperand(1, 32, false), rcc));
    curbb->lastinsn->SetCondDef();
  } else {
    // resopnd = 0;
    // cmp opnd0, #0
    // moveq resopnd, 1
    SelectCopy(resopnd, GetOrCreateImmOperand(0, 32, false), PTY_u32);
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbcmpri8, rcc, opnd0, GetOrCreateImmOperand(0, 32, false)));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbiteq));
    curbb->AppendInsn(
      cg->BuildInstruction<ArmInsn>(MOP_Tb2moveqimm12, resopnd, GetOrCreateImmOperand(1, 32, false), rcc));
    curbb->lastinsn->SetCondDef();
  }
  return resopnd;
}

Operand *ArmCGFunc::SelectNeg(UnaryNode *node, Operand *opnd0) {
  PrimType dtyp = node->primType;
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (IsPrimitiveFloat(dtyp)) {
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, dtyp);
    }
    Operand *resopnd = CreateOpndOfType(dtyp);
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2vnegd : MOP_Tb2vnegs, resopnd, opnd0));
    return resopnd;
  } else {
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    PrimType prmtype = is64bits ? (PTY_i64) : (PTY_i32);  // promoted type
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, prmtype);
    }
    Operand *rflag = GetOrCreateRflag();
    Operand *resopnd = CreateOpndOfType(prmtype);
    if (is64bits) {
      // rsbs  loresopnd, loopnd0, #0
      // sbc   hiresopnd, hiopnd0, hiopnd0, lsl #1
      Operand *r0v0 = CreateOpndOfType(prmtype);
      Operand *hiopnd0 = GetHigh32bitsOpnd(opnd0);
      Operand *loopnd0 = GetLow32bitsOpnd(opnd0);
      Operand *loresopnd = GetLow32bitsOpnd(resopnd);
      Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2rsubsrri8, rflag, loresopnd, loopnd0,
                                                      GetOrCreateImmOperand(0, 32, false)));
      SelectShift(r0v0, hiopnd0, GetOrCreateImmOperand(1, 32, false), ShiftLeft, PTY_u32);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2sbcrrr, hiresopnd, hiopnd0, r0v0, rflag));
    } else {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2negrr, resopnd, opnd0));
    }
    return resopnd;
  }
}

//  recip %esi
//      ||
//      ||
//      ||
//      \/
//  divss/divsd  %edi
Operand *ArmCGFunc::SelectRecip(UnaryNode *node, Operand *opnd0) {
  // fconsts s15, #112
  // fdivs s0, s15, s0
  PrimType dtyp = node->primType;
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (IsPrimitiveFloat(dtyp)) {
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, dtyp);
    }
    Operand *resopnd = CreateOpndOfType(dtyp);
    Operand *s15 = CreateOpndOfType(dtyp);
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2fconstd : MOP_Tb2fconsts, s15,
                                                    GetOrCreateImmOperand(112, 8, false)));
    SelectDiv(resopnd, s15, opnd0, dtyp);
    return resopnd;
  } else {
    CG_ASSERT(false, "should be float type");
  }
  return nullptr;
}

Operand *ArmCGFunc::SelectSqrt(UnaryNode *node, Operand *opnd0) {
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
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (IsPrimitiveFloat(dtyp)) {
    bool is64bits = (GetPrimTypeBitSize(dtyp) == 64);
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, dtyp);
    }
    Operand *resopnd = CreateOpndOfType(dtyp);
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2vsqrtd : MOP_Tb2vsqrts, resopnd, opnd0));
    return resopnd;
  } else {
    CG_ASSERT(false, "should be float type");
  }
  return nullptr;
}

void ArmCGFunc::SelectFloat2Int(Operand *resopnd, Operand *opnd0, PrimType itype, PrimType ftype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  switch (itype) {
    case PTY_i32:
    case PTY_u32: {
      // f64toi32
      // ftosizd s13, opnd0
      // fmrs  r3, s13 @ int
      // mov resopnd, r3
      //  f32toi32
      // ftosizs s15, opnd0
      //   fmrs  r3, s15 @ int
      //   mov resopnd, r3
      // f32tou32
      // ftouizs s15, opnd0
      // fmrs  r3, s15 @ int
      // mov resopnd, r3
      // f64tou32
      // ftouizd s13, opnd0
      // fmrs  r3, s13 @ int
      // mov resopnd, r3
      if (opnd0ty != Operand::Opd_Register) {
        opnd0 = SelectCopy(opnd0, ftype);
      }
      bool issigned = (itype == PTY_i32);
      MOperator mop =
        is64bits ? (issigned ? MOP_Tb2vcvtid : MOP_Tb2vcvtud) : (issigned ? MOP_Tb2vcvtif : MOP_Tb2vcvtuf);
      Operand *s13 = CreateOpndOfType(PTY_f32);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(mop, s13, opnd0));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2fmrs, resopnd, s13));
      break;
    }
    case PTY_i64:
    case PTY_u64: {
      // f32toi64
      // ldr r0, [r7, #4]  @ float
      // bl  __aeabi_f2lz
      // mov r2, r0
      // mov r3, r1
      // f64toi64
      // ldrd  r0, [r7]
      // bl  __aeabi_d2lz
      // mov r2, r0
      // mov r3, r1
      // mov r0, r2
      // mov r1, r3
      // f32tou64
      // ldr r0, [r7, #4]  @ float
      // bl  __aeabi_f2ulz
      // mov r2, r0
      // mov r3, r1
      // mov r0, r2
      // mov r1, r3
      // f64tou64
      // ldrd  r0, [r7]
      // bl  __aeabi_d2ulz
      // mov r2, r0
      // mov r3, r1
      // mov r0, r2
      // mov r1, r3
      bool issigned = itype == PTY_i64;
      vector<Operand *> opndvec;
      if (opnd0ty != Operand::Opd_Register) {
        opnd0 = SelectCopy(opnd0, is64bits ? PTY_i64 : PTY_i32);
      } else {
        Operand *tmpopnd = CreateOpndOfType(is64bits ? PTY_i64 : PTY_i32);
        if (is64bits) {
          Operand *lotmpopnd = GetLow32bitsOpnd(tmpopnd);
          Operand *hitmpopnd = GetHigh32bitsOpnd(tmpopnd);
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2fmrrd, lotmpopnd, hitmpopnd, opnd0));
        } else {
          curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2fmrs, tmpopnd, opnd0));
        }
        opnd0 = tmpopnd;
      }
      opndvec.push_back(resopnd);
      opndvec.push_back(opnd0);
      const char *func_name =
        is64bits ? (issigned ? "__aeabi_d2lz" : "__aeabi_d2ulz") : (issigned ? "__aeabi_f2lz" : "__aeabi_f2ulz");
      SelectLibCall(func_name, opndvec, is64bits ? PTY_u64 : PTY_u32, itype);
      break;
    }
    default:
      CG_ASSERT(false, "unexpected type");
  }
}

void ArmCGFunc::SelectInt2Float(Operand *resopnd, Operand *opnd0, PrimType itype, PrimType ftype) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  bool issigned = IsSignedInteger(itype);
  switch (itype) {
    case PTY_u32:
    // u32tof32
    // ldr r3, [r7, #4]
    //  fmsr  s14, r3 @ int
    //      fuitos  s15, s14
    //        fcpys s0, s15
    // u32tof64
    // ldr r3, [r7, #4]
    //  fmsr  s13, r3 @ int
    //      fuitod  d7, s13
    //        fcpyd d0, d7
    case PTY_i32: {
      // i32tof32
      // fmsr  s14, r3 @ int
      //  fsitos  s15, s14
      //      fcpys s0, s15
      // i32tof64
      // ldr r3, [r7, #4]
      //  fmsr  s13, r3 @ int
      //      fsitod  d7, s13
      //        fcpyd d0, d7
      if (opnd0ty != Operand::Opd_Register) {
        opnd0 = SelectCopy(opnd0, PTY_i32);
      }
      Operand *s14 = CreateOpndOfType(PTY_f32);
      Operand *d7 = CreateOpndOfType(is64bits ? PTY_f64 : PTY_f32);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2fmsr, s14, opnd0));
      MOperator cvtop =
        is64bits ? (issigned ? MOP_Tb2vcvtdi : MOP_Tb2vcvtdu) : (issigned ? MOP_Tb2vcvtfi : MOP_Tb2vcvtfu);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(cvtop, d7, s14));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2vmovd : MOP_Tb2vmovs, resopnd, d7));
      break;
    }
    case PTY_u64:
    // u64tof32
    // ldrd  r0, [r7]
    //    bl  __aeabi_ul2f
    //      mov r3, r0  @ float
    //        fmsr  s15, r3
    //          fcpys s0, s15
    // i64tof32
    // ldrd  r0, [r7]
    //  bl  __aeabi_ul2d
    //    mov r2, r0
    //      mov r3, r1
    //        fmdrr d7, r2, r3
    //          fcpyd d0, d7
    case PTY_i64: {
      // i64tof32
      // ldrd  r0, [r7]
      //    bl  __aeabi_l2f
      //      mov r3, r0  @ float
      //        fmsr  s15, r3
      //           fcpys s0, s15
      // i64tof64
      // ldrd  r0, [r7]
      //  bl  __aeabi_l2d
      //      mov r2, r0
      //        mov r3, r1
      //          fmdrr d7, r2, r3
      //              fcpyd d0, d7
      vector<Operand *> opndvec;
      Operand *tmpres = CreateOpndOfType(is64bits ? PTY_u64 : PTY_u32);
      opndvec.push_back(tmpres);
      // if (opnd0ty != Operand::Opd_Register)
      //  opnd0 = SelectCopy(opnd0, itype);
      opndvec.push_back(opnd0);
      const char *func_name =
        is64bits ? (issigned ? "__aeabi_l2d" : "__aeabi_ul2d") : (issigned ? "__aeabi_l2f" : "__aeabi_ul2f");
      SelectLibCall(func_name, opndvec, itype, is64bits ? PTY_u64 : PTY_u32);
      if (is64bits) {
        Operand *lotmpres = GetLow32bitsOpnd(tmpres);
        Operand *hitmpres = GetHigh32bitsOpnd(tmpres);
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2fmdrr, resopnd, lotmpres, hitmpres));
      } else {
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2fmsr, resopnd, tmpres));
      }
      break;
    }
    default:
      CG_ASSERT(false, "unexpected type");
  }
}

Operand *ArmCGFunc::SelectCeil(TypeCvtNode *node, Operand *opnd0) {
  // Operand::OperandType opnd0ty = opnd0->op_kind_;
  PrimType ftype = node->fromptyp;
  PrimType itype = node->primType;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  vector<Operand *> opndvec;
  Operand *tmpresopnd = CreateOpndOfType(ftype);
  opndvec.push_back(tmpresopnd);
  opndvec.push_back(opnd0);
  const char *func_name = is64bits ? "ceil" : "ceilf";
  SelectLibCall(func_name, opndvec, ftype, ftype);
  Operand *resopnd = CreateOpndOfType(itype);
  SelectFloat2Int(resopnd, tmpresopnd, itype, ftype);
  return resopnd;
}

// float to int floor
Operand *ArmCGFunc::SelectFloor(TypeCvtNode *node, Operand *opnd0) {
  PrimType ftype = node->fromptyp;
  PrimType itype = node->primType;
  CG_ASSERT((ftype == PTY_f64 || ftype == PTY_f32), "wrong float type");
  bool is64bits = (ftype == PTY_f64);
  vector<Operand *> opndvec;
  Operand *tmpresopnd = CreateOpndOfType(ftype);
  opndvec.push_back(tmpresopnd);
  opndvec.push_back(opnd0);
  const char *func_name = is64bits ? "floor" : "floorf";
  SelectLibCall(func_name, opndvec, ftype, ftype);
  Operand *resopnd = CreateOpndOfType(itype);
  SelectFloat2Int(resopnd, tmpresopnd, itype, ftype);
  return resopnd;
}

Operand *ArmCGFunc::SelectRetype(TypeCvtNode *node, Operand *opnd0) {
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
    if (opnd0ty == Operand::Opd_Mem) {
      resopnd = SelectCopy(opnd0, totype);
      return resopnd;
    }
    if (opnd0ty != Operand::Opd_Register) {
      opnd0 = SelectCopy(opnd0, itype);
    }
    if (isfromint) {
      if (is64bits) {
        curbb->AppendInsn(
          cg->BuildInstruction<ArmInsn>(MOP_Tb2fmdrr, resopnd, GetLow32bitsOpnd(opnd0), GetHigh32bitsOpnd(opnd0)));
      } else {
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2fmrs, resopnd, opnd0));
      }
    } else {
      if (is64bits) {
        curbb->AppendInsn(
          cg->BuildInstruction<ArmInsn>(MOP_Tb2fmdrr, GetLow32bitsOpnd(resopnd), GetHigh32bitsOpnd(resopnd), opnd0));
      } else {
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2fmsr, resopnd, opnd0));
      }
    }
    return resopnd;
  } else {
    CG_ASSERT(false, "NYI retype");
  }
  return nullptr;
}

void ArmCGFunc::SelectCvtF2F(Operand *resopnd, Operand *opnd0, PrimType fromty, PrimType toty) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, fromty);
  }
  MOperator mop = 0;
  switch (toty) {
    case PTY_f32: {
      switch (fromty) {
        case PTY_f64:
          mop = MOP_Tb2vcvtfd;
          break;
        default:
          CG_ASSERT(false, "unexpected cvt from type");
      }
      break;
    }
    case PTY_f64: {
      switch (fromty) {
        case PTY_f32:
          mop = MOP_Tb2vcvtdf;
          break;
        default:
          CG_ASSERT(false, "unexpected cvt from type");
      }
      break;
    }
    default:
      CG_ASSERT(false, "unexpected cvt to type");
  }
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(mop, resopnd, opnd0));
}

void ArmCGFunc::SelectCvtI2I(Operand *resopnd, Operand *opnd0, PrimType fromty, PrimType toty) {
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, fromty);
  }
  fromty = (GetPrimTypeBitSize(fromty) == 64) ? (IsSignedInteger(fromty) ? PTY_i64 : PTY_u64)
                                              : (IsSignedInteger(fromty) ? PTY_i32 : PTY_u32);
  bool is64bits = GetPrimTypeBitSize(toty) == 64;
  if (is64bits) {
    Operand *lowresopnd = GetLow32bitsOpnd(resopnd);
    Operand *highresopnd = GetHigh32bitsOpnd(resopnd);
    if (fromty == PTY_u32) {
      SelectCopy(lowresopnd, opnd0, PTY_u32);
      SelectCopy(highresopnd, GetOrCreateImmOperand(0, 32, false), PTY_u32);
    } else if (fromty == PTY_i32) {
      SelectCopy(lowresopnd, opnd0, PTY_u32);
      SelectShift(highresopnd, opnd0, GetOrCreateImmOperand(31, 32, false), ShiftAright, PTY_u32);
    } else {
      CG_ASSERT(false, "");
    }
  } else {  // trunc
    SelectCopy(resopnd, GetPrimTypeBitSize(fromty) == 64 ? GetLow32bitsOpnd(opnd0) : opnd0, PTY_u32);
  }
}

Operand *ArmCGFunc::SelectCvt(TypeCvtNode *node, Operand *opnd0) {
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

Operand *ArmCGFunc::SelectRound(TypeCvtNode *node, Operand *opnd0) {
  PrimType ftype = node->fromptyp;
  bool is64bits = (GetPrimTypeBitSize(node->primType) == 64);
  PrimType itype = (is64bits) ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                              : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(itype);
  SelectFloat2Int(resopnd, opnd0, itype, ftype);
  return resopnd;
}

Operand *ArmCGFunc::SelectTrunc(TypeCvtNode *node, Operand *opnd0) {
  PrimType ftype = node->fromptyp;
  bool is64bits = (GetPrimTypeBitSize(node->primType) == 64);
  PrimType itype = (is64bits) ? (IsSignedInteger(node->primType) ? PTY_i64 : PTY_u64)
                              : (IsSignedInteger(node->primType) ? PTY_i32 : PTY_u32);  // promoted type
  Operand *resopnd = CreateOpndOfType(itype);
  SelectFloat2Int(resopnd, opnd0, itype, ftype);
  return resopnd;
}

void ArmCGFunc::SelectSelect(Operand *resopnd, Operand *condopnd, Operand *trueopnd, Operand *falseopnd, PrimType dtyp,
                             PrimType ctyp) {
  Operand::OperandType condty = condopnd->op_kind_;
  Operand::OperandType falsety = falseopnd->op_kind_;
  if (resopnd == condopnd || condty != Operand::Opd_Register) {
    condopnd = SelectCopy(condopnd, ctyp);
  }

  if (IsPrimitiveInteger(dtyp)) {
    bool is64bits = GetPrimTypeBitSize(dtyp) == 64;
    if (!is64bits) {
      if (falsety != Operand::Opd_Register) {
        falseopnd = SelectCopy(falseopnd, dtyp);
      }
      SelectCopy(resopnd, trueopnd, dtyp);
      Operand *rflag = GetOrCreateRflag();
      curbb->AppendInsn(
        cg->BuildInstruction<ArmInsn>(MOP_Tbcmpri8, rflag, condopnd, GetOrCreateImmOperand(0, 8, false)));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbiteq));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2moveqrr, resopnd, falseopnd,
                                                      rflag));
      curbb->lastinsn->SetCondDef();
    } else {
      if (falsety != Operand::Opd_Register) {
        falseopnd = SelectCopy(falseopnd, dtyp);
      }
      SelectCopy(resopnd, trueopnd, dtyp);
      Operand *rflag = GetOrCreateRflag();
      Operand *loresopnd = GetLow32bitsOpnd(resopnd);
      Operand *hiresopnd = GetHigh32bitsOpnd(resopnd);
      Operand *lofalseopnd = GetLow32bitsOpnd(falseopnd);
      Operand *hifalseopnd = GetHigh32bitsOpnd(falseopnd);
      curbb->AppendInsn(
        cg->BuildInstruction<ArmInsn>(MOP_Tbcmpri8, rflag, condopnd, GetOrCreateImmOperand(0, 8, false)));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbiteq));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2moveqrr, loresopnd, lofalseopnd, rflag));
      curbb->lastinsn->SetCondDef();
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbiteq));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2moveqrr, hiresopnd, hifalseopnd, rflag));
      curbb->lastinsn->SetCondDef();
    }
  } else if (IsPrimitiveFloat(dtyp)) {
    if (falsety != Operand::Opd_Register) {
      falseopnd = SelectCopy(falseopnd, dtyp);
    }
    SelectCopy(resopnd, trueopnd, dtyp);
    bool is64bits = GetPrimTypeBitSize(dtyp) == 64;
    Operand *rflag = GetOrCreateRflag();
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbcmpri8, rflag, condopnd, GetOrCreateImmOperand(0, 8, false)));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbiteq));
    curbb->AppendInsn(
      cg->BuildInstruction<ArmInsn>(is64bits ? MOP_Tb2vmovdc : MOP_Tb2vmovsc, resopnd, falseopnd, rflag));
    curbb->lastinsn->SetCondDef();
  } else {
    CG_ASSERT(false, "unknown type for select");
  }
}

Operand *ArmCGFunc::SelectSelect(TernaryNode *node, Operand *opnd0, Operand *opnd1, Operand *opnd2) {
  PrimType dtyp = node->primType;
  PrimType ctyp = node->Opnd(0)->primType;
  Operand *resopnd = CreateOpndOfType(dtyp);
  SelectSelect(resopnd, opnd0, opnd1, opnd2, dtyp, ctyp);
  return resopnd;
}

void ArmCGFunc::SelectRangegoto(RangegotoNode *rangegotonode, Operand *opnd0) {
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
  Operand::OperandType opnd0ty = opnd0->op_kind_;
  if (opnd0ty != Operand::Opd_Register) {
    opnd0 = SelectCopy(opnd0, ityp);
  }
  Operand *addopnd = CreateOpndOfType(ityp);
  int32 minidx = switchtable[0].first;
  SelectAdd(addopnd, opnd0, GetOrCreateImmOperand(-minidx - rangegotonode->tagoffset, 32, true), ityp);
  MemOperand *lbmemopnd = GetOrCreateMemOpnd(lblst, 0, 32);
  Operand *baseopnd = CreateOpndOfType(PTY_u32);
  Operand *shiftres = CreateOpndOfType(PTY_u32);
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2adr, baseopnd, lbmemopnd));
  SelectShift(shiftres, addopnd, GetOrCreateImmOperand(2, 32, false), ShiftLeft, PTY_u32);
  Operand *ldaddr = GetOrCreateMemOpnd(ArmMemOperand::Addressing_BO, 32, static_cast<RegOperand *>(baseopnd), nullptr,
                                       shiftres, nullptr, nullptr);
  Operand *pc_opnd = GetOrCreatePhysicalRegisterOperand(RPC, 32, kRegTyInt);
  SelectCopy(pc_opnd, ldaddr, PTY_u32);
}

}  // namespace maplebe
