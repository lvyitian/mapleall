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
#include "cg_assert.h"

namespace maplebe {

using namespace maple;
using namespace std;

MOperator ArmCGFunc::PickLdInsn(PrimType ptype, PrimType rtype) {
  switch (ptype) {
    case PTY_i8:
      return MOP_Tbldrb;
    case PTY_i16:
      return MOP_Tbldrsh;
    case PTY_i32:
      return MOP_Tbld;
    case PTY_u64:
    case PTY_a64:
    case PTY_i64:
      CG_ASSERT(false, "64 bits integer load needs emulation");
    case PTY_u1:
    case PTY_u8:
      return MOP_Tbldrb;
    case PTY_u16:
      return MOP_Tbldrh;
    case PTY_a32:
    case PTY_ptr:
    case PTY_ref:
    case PTY_u32:
      return MOP_Tbld;
    case PTY_f32:
      return MOP_Tb2vldrs;
    case PTY_f64:
      return MOP_Tb2vldrd;
    default:
      CG_ASSERT(false, "NYI PickLdInsn");
  }
  return 0;
}

MOperator ArmCGFunc::PickLdInsn(uint32 bitsize, PrimType rtype, bool exactsize) {
  if (!exactsize) {
    bitsize = RoundUp(bitsize, 32);
  }
  if (IsUnsignedInteger(rtype)) {
    switch (bitsize) {
      case 1:
      case 8:
        return MOP_Tbldrb;
      case 16:
        return MOP_Tbldrh;
      case 32:
        return MOP_Tbld;
      case 64:
        return MOP_Tb2ldrd;
      default:
        CG_ASSERT(false, "NYI PickLdInsn");
        return MOP_undef;
    }
  } else if (IsSignedInteger(rtype)) {
    switch (bitsize) {
      case 8:
        return MOP_Tbldrb;
      case 16:
        return MOP_Tbldrsh;
      case 32:
        return MOP_Tbld;
      case 64:
        return MOP_Tb2ldrd;
      default:
        CG_ASSERT(false, "NYI PickLdInsn");
        return MOP_undef;
    }
  } else {
    switch (rtype) {
      case PTY_f32:
        return MOP_Tb2vldrs;
      case PTY_f64:
        return MOP_Tb2vldrd;
      default:
        CG_ASSERT(false, "NYI PickLdInsn");
        return MOP_undef;
    }
  }
}

MOperator ArmCGFunc::PickStInsn(PrimType primtype) {
  switch (primtype) {
    case PTY_u1:
    case PTY_i8:
    case PTY_u8:
      return MOP_Tbstrb;
    case PTY_i16:
    case PTY_u16:
      return MOP_Tbstrh;
    case PTY_a32:
    case PTY_ptr:
    case PTY_ref:
    case PTY_i32:
    case PTY_u32:
      return MOP_Tbstr;
    case PTY_i64:
    case PTY_u64:
    case PTY_a64:
      return MOP_Tb2strd;
    case PTY_f32:
      return MOP_Tb2vstrs;
    case PTY_f64:
      return MOP_Tb2vstrd;
    default:
      CG_ASSERT(false, "NYI PickStInsn");
      return MOP_undef;
  }
}

MOperator ArmCGFunc::PickStInsn(uint32 bitsize, PrimType rtype, bool exactsize) {
  if (!exactsize) {
    bitsize = RoundUp(bitsize, 32);
  }
  if (IsPrimitiveInteger(rtype) || rtype == PTY_agg) {
    switch (bitsize) {
      case 1:
      case 8:
        return MOP_Tbstrb;
      case 16:
        return MOP_Tbstrh;
      case 32:
        return MOP_Tbstr;
      case 64:
        return MOP_Tb2strd;
      default:
        CG_ASSERT(false, "NYI PickLdInsn");
        return MOP_undef;
    }
  } else {
    switch (rtype) {
      case PTY_f32:
        return MOP_Tb2vstrs;
      case PTY_f64:
        return MOP_Tb2vstrd;
      default:
        CG_ASSERT(false, "NYI PickLdInsn");
        return MOP_undef;
    }
  }
  return MOP_undef;
}

MOperator ArmCGFunc::PickMovInsn(PrimType primtype) {
  switch (primtype) {
    case PTY_u8:
    case PTY_u16:
    case PTY_u32:
    case PTY_i8:
    case PTY_i16:
    case PTY_i32:
      return MOP_Tb2movrr;
    case PTY_f32:
      return MOP_Tb2vmovs;
    case PTY_f64:
      return MOP_Tb2vmovd;
    default:
      CG_ASSERT(false, "NYI PickStInsn");
      return MOP_undef;
  }
}

MOperator ArmCGFunc::PickMovInsn(RegOperand *lhs, RegOperand *rhs) {
  CG_ASSERT(lhs->GetRegisterType() == rhs->GetRegisterType(), "PickMovInsn: register types don't match");
  CG_ASSERT(lhs->GetSize() == rhs->GetSize(), "PickMovInsn: sizes don't match");
  CG_ASSERT((lhs->GetSize() < 64 || lhs->GetRegisterType() == kRegTyFloat), "should split the 64 bits or more mov");
  if (lhs->GetRegisterType() == kRegTyInt) {
    return MOP_Tb2movrr;
  }
  if (lhs->GetRegisterType() == kRegTyFloat) {
    return lhs->GetSize() <= 32 ? MOP_Tb2vmovs : MOP_Tb2vmovd;
  }
  CG_ASSERT(false, "PickMovInsn: kind NYI");
  return 0;
}

void ArmCGFunc::SelectCopyImm(Operand *dest, ArmImmOperand *src, PrimType dtype) {
  uint32 dsize = GetPrimTypeBitSize(dtype);
  if (IsPrimitiveInteger(dtype) && dsize <= 32) {
    // return 0x12345678;  // movw  r3, #22136(0x5678) movt  r3, #4660(0x1234)
    // return 0x5678;  // movw  r3, #22136(0x5678)
    // return 0x78;  // movs  r3, #120(0x78)
    // return 0x678;  // mov r3, #1656(0x678)
    // return 0xff;  // movs  r3, #255(0xff)
    // return 0x100;  // mov r3, #256(0x100)
    // return -1;  // mov r3, #-1
    // return -0xff;  // mvn r3, #254
    // return -2;  // mvn r3, #1 = ~1
    // return ~(0x5678);  // movw  r3, #43399(0xa987) movt  r3, 65535(0xffff) ~(0xffffa987) = 0x5678
    // return ~(0x678);  // mvn r3, #1656(0x678)
    ArmImmOperand *immopnd = src;
    if (immopnd->IsInBitSize(8)) {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbmovimm8, GetOrCreateRflag(), dest, src));
    } else if (immopnd->IsInBitSizeRot(8) || static_cast<int32>(immopnd->GetValue()) == -1) {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm12, dest, src));
    } else if (immopnd->IsInBitSize(16)) {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm16l, dest, src));
    } else if (static_cast<int32>(immopnd->GetValue()) > 0) {
      curbb->AppendInsn(
        cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm16l, dest, GetOrCreateImmOperand(immopnd->GetLow16(), 16, false)));
      curbb->AppendInsn(
        cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm16h, dest, GetOrCreateImmOperand(immopnd->GetHigh16(), 16, false)));
    } else {  // must be < 0
      int32 val = immopnd->GetValue();
      ImmOperand *reverse_opnd = GetOrCreateImmOperand(~val, 32, true);
      if (reverse_opnd->IsInBitSizeRot(8)) {
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2mvni12, dest, reverse_opnd));
      } else {
        curbb->AppendInsn(
          cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm16l, dest, GetOrCreateImmOperand(immopnd->GetLow16(), 16, false)));
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm16h, dest,
                                                        GetOrCreateImmOperand(immopnd->GetHigh16(), 16, false)));
      }
    }
  } else if (IsPrimitiveInteger(dtype) && dsize == 64) {
    ArmImmOperand *immopnd = src;
    uint32 low_val = immopnd->GetLow32();
    uint32 high_val = immopnd->GetHigh32();
    if (ImmOperand::IsInBitSizeRot(8, low_val)) {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm12, GetLow32bitsOpnd(dest),
                                                      GetOrCreateImmOperand(immopnd->GetLow32(), 8, false)));
    } else if (ImmOperand::IsInBitSizeRot(8, ~low_val)) {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2mvni12, GetLow32bitsOpnd(dest),
                                                      GetOrCreateImmOperand(~low_val, 8, false)));
    } else {
      MIRSymbol *st = func->symtab->CreateSymbol(SCOPE_LOCAL);
      std::string lblstr(".LB_");
      MIRSymbol *func_st = globaltable.GetSymbolFromStidx(func->stidx.Idx());
      std::string funcname = func_st->GetName();
      lblstr.append(funcname).append(to_string(labelidx++));
      st->SetNameStridx(globaltable.GetOrCreateGstridxFromName(lblstr));
      st->storageClass = kScPstatic;
      st->sKind = kStConst;
      st->SetConst(memPool->New<MIRIntConst>(low_val, globaltable.GetTypeFromTyIdx(PTY_u32)));
      st->SetTyIdx(PTY_u32);
      MemOperand *srcopnd = GetOrCreateMemOpnd(st, 0, 32);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbld, GetLow32bitsOpnd(dest), srcopnd));
    }
    if (ImmOperand::IsInBitSizeRot(8, high_val)) {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm12, GetHigh32bitsOpnd(dest),
                                                      GetOrCreateImmOperand(immopnd->GetHigh32(), 8, false)));
    } else if (ImmOperand::IsInBitSizeRot(8, ~high_val)) {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2mvni12, GetHigh32bitsOpnd(dest),
                                                      GetOrCreateImmOperand(~high_val, 8, false)));
    } else {
      MIRSymbol *st = func->symtab->CreateSymbol(SCOPE_LOCAL);
      std::string lblstr(".LB_");
      MIRSymbol *func_st = globaltable.GetSymbolFromStidx(func->stidx.Idx());
      std::string funcname = func_st->GetName();
      lblstr.append(funcname).append(to_string(labelidx++));
      st->SetNameStridx(globaltable.GetOrCreateGstridxFromName(lblstr));
      st->storageClass = kScPstatic;
      st->sKind = kStConst;
      st->SetConst(memPool->New<MIRIntConst>(high_val, globaltable.GetTypeFromTyIdx(PTY_u32)));
      st->SetTyIdx(PTY_u32);
      MemOperand *srcopnd = GetOrCreateMemOpnd(st, 0, 32);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbld, GetHigh32bitsOpnd(dest), srcopnd));
    }
  } else {
    CG_ASSERT(false, "NYI");
  }
}

void ArmCGFunc::SelectCopy(Operand *dest, Operand *src, PrimType dtype) {
  uint32 ssize = src->size_;
  uint32 dsize = GetPrimTypeBitSize(dtype);
  Operand::OperandType opnd0ty = dest->op_kind_;
  Operand::OperandType opnd1ty = src->op_kind_;
  CG_ASSERT((dsize >= ssize || opnd0ty == Operand::Opd_Mem), "NYI");
  CG_ASSERT((opnd0ty == Operand::Opd_Register || opnd1ty == Operand::Opd_Register),
            "either src or dest should be register");
  switch (src->op_kind_) {
    case Operand::Opd_Mem: {
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickLdInsn(ssize, dtype, true), dest, src));
    } break;
    case Operand::Opd_Immediate: {
      SelectCopyImm(dest, static_cast<ArmImmOperand *>(src), dtype);
    } break;
    case Operand::Opd_Register: {
      if (opnd0ty == Operand::Opd_Mem) {
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickStInsn(dsize, dtype, true), src, dest));
      } else {
        switch (dtype) {
          case PTY_u8:
          case PTY_i8:
          case PTY_i16:
          case PTY_u16:
          case PTY_i32:
          case PTY_a32:
          case PTY_ptr:
          case PTY_ref:
          case PTY_u32:
            curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2movrr, dest, src));
            break;
          case PTY_a64:
          case PTY_u64:
          case PTY_i64:
            curbb->AppendInsn(
              cg->BuildInstruction<ArmInsn>(MOP_Tb2movrr, GetLow32bitsOpnd(dest), GetLow32bitsOpnd(src)));
            curbb->AppendInsn(
              cg->BuildInstruction<ArmInsn>(MOP_Tb2movrr, GetHigh32bitsOpnd(dest), GetHigh32bitsOpnd(src)));
            break;
          case PTY_f32:
            curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2vmovs, dest, src));
            break;
          case PTY_f64:
            curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2vmovd, dest, src));
            break;
          default:
            CG_ASSERT(false, "NYI type of move");
        }
      }
    } break;
    default:
      CG_ASSERT(false, "NYI");
  }
}

// this function copy src to a register, the src can be an imm, mem or a lebel
Operand *ArmCGFunc::SelectCopy(Operand *src, PrimType dtype) {
  Operand *dest = CreateOpndOfType(dtype);
  SelectCopy(dest, src, dtype);
  return dest;
}

void ArmCGFunc::SelectDassign(DassignNode *stmt, Operand *opnd0) {
  MIRModule &mirModule = *g->mirModule;
  MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(stmt->stidx);
  int32 offset = 0;
  if (stmt->fieldid != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(symbol->GetType());
    CG_ASSERT(structty, "SelectDassign: non-zero fieldid for non-structure");
    offset = becommon.GetFieldOffset(structty, stmt->fieldid).first;
  }
  Operand *st_opnd = opnd0;
  if (opnd0->op_kind_ != Operand::Opd_Register) {
    st_opnd = SelectCopy(opnd0, stmt->rhs->primType);
  }
  MIRType *type = symbol->GetType();
  MOperator mop = MOP_undef;
  if (type->typeKind == kTypeStruct) {
    MIRStructType *struct_type = static_cast<MIRStructType *>(type);
    TyIdx ftyidx = struct_type->TraverseToField(stmt->fieldid).second.first;
    type = globaltable.GetTypeFromTyIdx(ftyidx);
  } else if (type->typeKind == kTypeClass) {
    MIRClassType *class_type = static_cast<MIRClassType *>(type);
    TyIdx ftyidx = class_type->TraverseToField(stmt->fieldid).second.first;
    type = globaltable.GetTypeFromTyIdx(ftyidx);
  }

  MemOperand *memopnd = GetOrCreateMemOpnd(symbol, offset, GetPrimTypeBitSize(type->GetPrimType()) /*fill later*/);
  MIRTypeKind ty_kind = type->typeKind;
  CG_ASSERT((ty_kind == kTypeScalar || ty_kind == kTypePointer), "NYI dassign type");
  mop = PickStInsn(type->GetPrimType());
  st_opnd->size_ = memopnd->size_;  // for store, register size must match mem size
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(mop, st_opnd, memopnd));
}

void ArmCGFunc::SelectRegassign(RegassignNode *stmt, Operand *opnd0) {
  PregIdx pregidx = stmt->regidx;
  uint32 v_reg_no = pregidx + first_mapleir_v_reg_no;
  RegOperand *regopnd = CreateVirtualRegisterOperand(v_reg_no);
  // look at rhs
  SelectCopy(regopnd, opnd0, stmt->uopnd->primType);
}

void ArmCGFunc::SelectAggDassign(DassignNode *stmt) {
  MIRModule &mirModule = *g->mirModule;
  MIRSymbol *lhssymbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(stmt->stidx);
  int32 lhsoffset = 0;
  MIRType *lhsty = lhssymbol->GetType();
  if (stmt->fieldid != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(lhssymbol->GetType());
    CG_ASSERT(structty, "SelectDassign: non-zero fieldid for non-structure");
    FieldPair thepair = structty->TraverseToField(stmt->fieldid);
    lhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
    lhsoffset = becommon.GetFieldOffset(structty, stmt->fieldid).first;
  }
  uint32 lhsalign = becommon.type_align_table[lhsty->_ty_idx.idx];
  uint32 lhssize = becommon.type_size_table[lhsty->_ty_idx.idx];

  uint32 rhsalign;
  uint32 alignused;
  int32 rhsoffset = 0;
  if (stmt->rhs->op == OP_dread) {
    AddrofNode *rhsdread = static_cast<AddrofNode *>(stmt->rhs);
    MIRSymbol *rhssymbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(rhsdread->stidx);
    MIRType *rhsty = rhssymbol->GetType();
    if (rhsdread->fieldid != 0) {
      MIRStructType *structty = dynamic_cast<MIRStructType *>(rhssymbol->GetType());
      CG_ASSERT(structty, "SelectDassign: non-zero fieldid for non-structure");
      FieldPair thepair = structty->TraverseToField(rhsdread->fieldid);
      rhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(structty, rhsdread->fieldid).first;
    }
    rhsalign = becommon.type_align_table[rhsty->_ty_idx.idx];

    alignused = std::min(lhsalign, rhsalign);
    MemOperand *rhsmemopnd = nullptr;
    MemOperand *lhsmemopnd = nullptr;
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      rhsmemopnd = GetOrCreateMemOpnd(rhssymbol, rhsoffset + i * alignused, alignused * 8);
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, alignused));
      RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickLdInsn(alignused * 8, PTY_u32), result, rhsmemopnd));
      // generate the store
      lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + i * alignused, alignused * 8);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickStInsn(alignused * 8, PTY_u32, true), result, lhsmemopnd));
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssize_covered = (lhssize / alignused) * alignused;
    uint32 newalignused = alignused;
    while (lhssize_covered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssize_covered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      rhsmemopnd = GetOrCreateMemOpnd(rhssymbol, rhsoffset + lhssize_covered, newalignused * 8);
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
      RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickLdInsn(newalignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + lhssize_covered, newalignused * 8);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickStInsn(newalignused * 8, PTY_u32, true), result, lhsmemopnd));
      lhssize_covered += newalignused;
    }
  } else {  // rhs is iread
    CG_ASSERT(stmt->rhs->op == OP_iread, "SelectAggDassign: NYI");
    IreadNode *rhsiread = static_cast<IreadNode *>(stmt->rhs);
    RegOperand *addropnd = static_cast<RegOperand *>(HandleExpr(rhsiread, rhsiread->Opnd(0)));
    if (addropnd->op_kind_ != Operand::Opd_Register) {
      addropnd = static_cast<RegOperand *>(SelectCopy(addropnd, rhsiread->Opnd(0)->primType));
    }
    MIRPtrType *rhspointerty = static_cast<MIRPtrType *>(globaltable.GetTypeFromTyIdx(rhsiread->tyidx));
    MIRType *rhsty = static_cast<MIRStructType *>(globaltable.GetTypeFromTyIdx(rhspointerty->pointedTyIdx));
    if (rhsiread->fieldid != 0) {
      MIRStructType *rhsstructty = dynamic_cast<MIRStructType *>(rhsty);
      CG_ASSERT(rhsstructty, "SelectAggDassign: non-zero fieldid for non-structure");
      FieldPair thepair = rhsstructty->TraverseToField(rhsiread->fieldid);
      rhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(rhsstructty, rhsiread->fieldid).first;
    }
    rhsalign = becommon.type_align_table[rhsty->_ty_idx.idx];

    alignused = std::min(lhsalign, rhsalign);
    MemOperand *rhsmemopnd = nullptr;
    MemOperand *lhsmemopnd = nullptr;
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      OfstOperand *offopnd = GetOrCreateOfstOperand(rhsoffset + i * alignused, 32);
      rhsmemopnd = GetOrCreateMemOpnd(ArmMemOperand::Addressing_BO, alignused * 8, addropnd, nullptr, offopnd, nullptr,
                                      static_cast<MIRSymbol *>(nullptr));
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, alignused));
      RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickLdInsn(alignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + i * alignused, alignused * 8);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickStInsn(alignused * 8, PTY_u32, true), result, lhsmemopnd));
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssize_covered = (lhssize / alignused) * alignused;
    uint32 newalignused = alignused;
    while (lhssize_covered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssize_covered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      OfstOperand *offopnd = GetOrCreateOfstOperand(rhsoffset + lhssize_covered, 32);
      rhsmemopnd = GetOrCreateMemOpnd(ArmMemOperand::Addressing_BO, newalignused * 8, addropnd, nullptr, offopnd,
                                      nullptr, static_cast<MIRSymbol *>(nullptr));
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
      RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickLdInsn(newalignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + lhssize_covered, newalignused * 8);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickStInsn(newalignused * 8, PTY_u32, true), result, lhsmemopnd));
      lhssize_covered += newalignused;
    }
  }
}

static MIRType *GetPointedToType(MIRPtrType *pointerty) {
  MIRType *atype = globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx);
  if (atype->GetKind() == kTypeArray) {
    MIRArrayType *arraytype = static_cast<MIRArrayType *>(atype);
    return globaltable.GetTypeFromTyIdx(arraytype->eTyIdx);
  }
  if (atype->GetKind() == kTypeFArray || atype->GetKind() == kTypeJArray) {
    MIRFarrayType *farraytype = static_cast<MIRFarrayType *>(atype);
    return globaltable.GetTypeFromTyIdx(farraytype->elemTyIdx);
  }
  return globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx);
}

void ArmCGFunc::SelectIassign(IassignNode *stmt) {
  Operand *valopnd = HandleExpr(stmt, stmt->rhs);
  Operand *addropnd = HandleExpr(stmt, stmt->addrexp);

  if (addropnd->op_kind_ != Operand::Opd_Register) {
    addropnd = static_cast<RegOperand *>(SelectCopy(addropnd, stmt->addrexp->primType));
  }

  int32 offset = 0;
  MIRType *type = globaltable.GetTypeFromTyIdx(stmt->tyidx);
  MIRPtrType *pointerty = dynamic_cast<MIRPtrType *>(type);
  CG_ASSERT(pointerty, "expect a pointer type at iassign node");
  if (stmt->fieldid != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx));
    CG_ASSERT(structty, "SelectIassign: non-zero fieldid for non-structure");
    FieldPair thepair = structty->TraverseToField(stmt->fieldid);
    type = globaltable.GetTypeFromTyIdx(thepair.second.first);
    offset = becommon.GetFieldOffset(structty, stmt->fieldid).first;
  } else {
    type = GetPointedToType(pointerty);
  }
  Operand *src_opnd = valopnd;
  if (valopnd->op_kind_ != Operand::Opd_Register) {
    src_opnd = SelectCopy(valopnd, stmt->rhs->primType);
  }

  OfstOperand *offopnd = GetOrCreateOfstOperand(offset, 32);
  PrimType dest_type = type->GetPrimType();
  Operand *memopnd = GetOrCreateMemOpnd(ArmMemOperand::Addressing_BO, GetPrimTypeSize(dest_type) * 8,
                                        static_cast<RegOperand *>(addropnd), nullptr, offopnd, nullptr, nullptr);
  SelectCopy(memopnd, src_opnd, dest_type);
}

void ArmCGFunc::SelectAggIassign(IassignNode *stmt, Operand *lhsaddropnd) {
  MIRModule &mirModule = *g->mirModule;
  if (lhsaddropnd->op_kind_ != Operand::Opd_Register) {
    lhsaddropnd = SelectCopy(lhsaddropnd, stmt->addrexp->primType);
  }
  int32 lhsoffset = 0;
  MIRPtrType *lhspointerty = static_cast<MIRPtrType *>(globaltable.GetTypeFromTyIdx(stmt->tyidx));
  MIRType *lhsty = globaltable.GetTypeFromTyIdx(lhspointerty->pointedTyIdx);
  if (stmt->fieldid != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(lhsty);
    CG_ASSERT(structty, "SelectAggIassign: non-zero fieldid for non-structure");
    FieldPair thepair = structty->TraverseToField(stmt->fieldid);
    lhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
    lhsoffset = becommon.GetFieldOffset(structty, stmt->fieldid).first;
  } else if (MIRArrayType *arraylhsty = dynamic_cast<MIRArrayType *>(lhsty)) {
    // access an array element
    lhsty = globaltable.GetTypeFromTyIdx(arraylhsty->eTyIdx);
    MIRTypeKind tykind = lhsty->typeKind;
    CG_ASSERT((tykind == kTypeScalar || tykind == kTypeStruct || tykind == kTypeClass || tykind == kTypePointer),
              "unexpected array element type in iassign");
  } else if (MIRFarrayType *farraylhsty = dynamic_cast<MIRFarrayType *>(lhsty)) {
    // access an farray element
    lhsty = globaltable.GetTypeFromTyIdx(farraylhsty->elemTyIdx);
    MIRTypeKind tykind = lhsty->typeKind;
    CG_ASSERT((tykind == kTypeScalar || tykind == kTypeStruct || tykind == kTypeClass || tykind == kTypePointer),
              "unexpected array element type in iassign");
  }
  uint32 lhsalign = becommon.type_align_table[lhsty->_ty_idx.idx];
  uint32 lhssize = becommon.type_size_table[lhsty->_ty_idx.idx];

  uint32 rhsalign;
  uint32 alignused;
  int32 rhsoffset = 0;
  if (stmt->rhs->op == OP_dread) {
    AddrofNode *rhsdread = static_cast<AddrofNode *>(stmt->rhs);
    MIRSymbol *rhssymbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(rhsdread->stidx);
    MIRType *rhsty = rhssymbol->GetType();
    if (rhsdread->fieldid != 0) {
      MIRStructType *structty = dynamic_cast<MIRStructType *>(rhssymbol->GetType());
      CG_ASSERT(structty, "SelectDassign: non-zero fieldid for non-structure");
      FieldPair thepair = structty->TraverseToField(rhsdread->fieldid);
      rhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(structty, rhsdread->fieldid).first;
    }
    rhsalign = becommon.type_align_table[rhsty->_ty_idx.idx];

    alignused = std::min(lhsalign, rhsalign);
    MemOperand *rhsmemopnd = nullptr;
    MemOperand *lhsmemopnd = nullptr;
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      rhsmemopnd = GetOrCreateMemOpnd(rhssymbol, rhsoffset + i * alignused, alignused * 8);
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, alignused));
      RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickLdInsn(alignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      OfstOperand *offopnd = GetOrCreateOfstOperand(lhsoffset + i * alignused, 32);
      lhsmemopnd =
        GetOrCreateMemOpnd(ArmMemOperand::Addressing_BO, alignused * 8, static_cast<RegOperand *>(lhsaddropnd), nullptr,
                           offopnd, nullptr, static_cast<MIRSymbol *>(nullptr));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickStInsn(alignused * 8, PTY_u32, true), result, lhsmemopnd));
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssize_covered = (lhssize / alignused) * alignused;
    uint32 newalignused = alignused;
    while (lhssize_covered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssize_covered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      rhsmemopnd = GetOrCreateMemOpnd(rhssymbol, rhsoffset + lhssize_covered, newalignused * 8);
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
      RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickLdInsn(newalignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      OfstOperand *offopnd = GetOrCreateOfstOperand(lhsoffset + lhssize_covered, 32);
      lhsmemopnd =
        GetOrCreateMemOpnd(ArmMemOperand::Addressing_BO, newalignused * 8, static_cast<RegOperand *>(lhsaddropnd),
                           nullptr, offopnd, nullptr, static_cast<MIRSymbol *>(nullptr));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickStInsn(newalignused * 8, PTY_u32, true), result, lhsmemopnd));
      lhssize_covered += newalignused;
    }
  } else {  // rhs is iread
    CG_ASSERT(stmt->rhs->op == OP_iread, "SelectAggDassign: NYI");
    IreadNode *rhsiread = static_cast<IreadNode *>(stmt->rhs);
    RegOperand *rhsaddropnd = static_cast<RegOperand *>(HandleExpr(rhsiread, rhsiread->Opnd(0)));
    if (rhsaddropnd->op_kind_ != Operand::Opd_Register) {
      rhsaddropnd = static_cast<RegOperand *>(SelectCopy(rhsaddropnd, rhsiread->Opnd(0)->primType));
    }
    MIRPtrType *rhspointerty = static_cast<MIRPtrType *>(globaltable.GetTypeFromTyIdx(rhsiread->tyidx));
    MIRType *rhsty = static_cast<MIRStructType *>(globaltable.GetTypeFromTyIdx(rhspointerty->pointedTyIdx));
    if (rhsiread->fieldid != 0) {
      MIRStructType *rhsstructty = dynamic_cast<MIRStructType *>(rhsty);
      CG_ASSERT(rhsstructty, "SelectAggDassign: non-zero fieldid for non-structure");
      FieldPair thepair = rhsstructty->TraverseToField(rhsiread->fieldid);
      rhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(rhsstructty, rhsiread->fieldid).first;
    }
    rhsalign = becommon.type_align_table[rhsty->_ty_idx.idx];

    alignused = std::min(lhsalign, rhsalign);
    Operand *rhsmemopnd =nullptr;
    Operand *lhsmemopnd = nullptr;
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      OfstOperand *rhsoffopnd = GetOrCreateOfstOperand(rhsoffset + i * alignused, 32);
      rhsmemopnd = GetOrCreateMemOpnd(ArmMemOperand::Addressing_BO, alignused * 8, rhsaddropnd, nullptr, rhsoffopnd,
                                      nullptr, static_cast<MIRSymbol *>(nullptr));
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, alignused));
      ArmRegOperand *result = memPool->New<ArmRegOperand>(v_reg_no, v_reg_table[v_reg_no].size * 8, kRegTyInt);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickLdInsn(alignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      OfstOperand *lhsoffopnd = GetOrCreateOfstOperand(lhsoffset + i * alignused, 32);
      lhsmemopnd =
        GetOrCreateMemOpnd(ArmMemOperand::Addressing_BO, alignused * 8, static_cast<RegOperand *>(lhsaddropnd), nullptr,
                           lhsoffopnd, nullptr, static_cast<MIRSymbol *>(nullptr));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickStInsn(alignused * 8, PTY_u32, true), result, lhsmemopnd));
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssize_covered = (lhssize / alignused) * alignused;
    uint32 newalignused = alignused;
    while (lhssize_covered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssize_covered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      OfstOperand *rhsoffopnd = GetOrCreateOfstOperand(rhsoffset + lhssize_covered, 32);
      rhsmemopnd = GetOrCreateMemOpnd(ArmMemOperand::Addressing_BO, newalignused * 8, rhsaddropnd, nullptr, rhsoffopnd,
                                      nullptr, static_cast<MIRSymbol *>(nullptr));
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
      ArmRegOperand *result = memPool->New<ArmRegOperand>(v_reg_no, v_reg_table[v_reg_no].size * 8, kRegTyInt);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickLdInsn(newalignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      OfstOperand *lhsoffopnd = GetOrCreateOfstOperand(lhsoffset + lhssize_covered, 32);
      lhsmemopnd =
        GetOrCreateMemOpnd(ArmMemOperand::Addressing_BO, newalignused * 8, static_cast<ArmRegOperand *>(lhsaddropnd),
                           nullptr, lhsoffopnd, nullptr, static_cast<MIRSymbol *>(nullptr));
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickStInsn(newalignused * 8, PTY_u32, true), result, lhsmemopnd));
      lhssize_covered += newalignused;
    }
  }
}

Operand *ArmCGFunc::SelectDread(base_node_t *parent, AddrofNode *expr) {
  MIRModule &mirModule = *g->mirModule;
  MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(expr->stidx);
  PrimType symty = symbol->GetType()->primType;
  int32 offset = 0;
  if (expr->fieldid != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(symbol->GetType());
    CG_ASSERT(structty, "SelectDread: non-zero fieldid for non-structure");
    FieldPair thepair = structty->TraverseToField(expr->fieldid);
    symty = globaltable.GetTypeFromTyIdx(thepair.second.first)->primType;
    offset = becommon.GetFieldOffset(structty, expr->fieldid).first;
  }
  CG_ASSERT(symty != PTY_agg, "dread type error");
  MemOperand *memopnd = GetOrCreateMemOpnd(symbol, offset, GetPrimTypeSize(symty) * 8);
  return memopnd;
}

RegOperand *ArmCGFunc::SelectRegread(base_node_t *parent, RegreadNode *expr) {
  PregIdx pregidx = expr->regidx;
  uint32 v_reg_no = pregidx + first_mapleir_v_reg_no;
  return CreateVirtualRegisterOperand(v_reg_no);
}

void ArmCGFunc::SelectAddrof(Operand *result, ArmMemOperand *memopnd) {
  MIRSymbol *symbol = memopnd->GetSymbol();
  if (symbol->storageClass == kScAuto) {
    SelectAdd(
      result, memopnd->GetBaseRegister(),
      GetOrCreateImmOperand(static_cast<OfstOperand *>(memopnd->GetOffsetOperand())->GetValue(), PTY_u32, false),
      PTY_u32);
  } else {
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm16lst, result, memopnd));
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tb2movimm16hst, result, memopnd));
  }
}

Operand *ArmCGFunc::SelectAddrof(AddrofNode *expr) {
  MIRSymbol *symbol = g->mirModule->CurFunction()->GetLocalOrGlobalSymbol(expr->stidx);
  int32 offset = 0;
  if (expr->fieldid != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(symbol->GetType());
    CG_ASSERT(structty, "SelectAddrof: non-zero fieldid for non-structure");
    offset = becommon.GetFieldOffset(structty, expr->fieldid).first;
  }
  PrimType primType = expr->primType;
  uint32 v_reg_no = New_V_Reg(kRegTyInt, GetPrimTypeSize(primType));
  RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);
  MemOperand *memopnd = GetOrCreateMemOpnd(symbol, offset, GetPrimTypeSize(primType) * 8);
  if (symbol->storageClass == kScGlobal || symbol->storageClass == kScExtern) {
    CG_ASSERT(static_cast<ArmMemOperand *>(memopnd)->addr_mode_ == ArmMemOperand::Addressing_BO, "");
    OfstOperand *offopnd = static_cast<OfstOperand *>(memopnd->GetOffsetOperand());
    SelectAdd(result, memopnd->GetBaseRegister(), offopnd, PTY_u32);
  } else {
    SelectAddrof(result, static_cast<ArmMemOperand *>(memopnd));
  }
  return result;
}

Operand *ArmCGFunc::SelectAddroffunc(AddroffuncNode *expr) {
  CG_ASSERT(false, "ArmCGFunc::SelectAddroffunc not implemented.");
  return nullptr;
}

Operand *ArmCGFunc::SelectIread(base_node_t *parent, IreadNode *expr) {
  Operand *addropnd = HandleExpr(expr, expr->Opnd(0));
  if (addropnd->op_kind_ != Operand::Opd_Register) {
    addropnd = static_cast<ArmRegOperand *>(SelectCopy(addropnd, expr->Opnd(0)->primType));
  }

  int32 offset = 0;
  MIRType *type = globaltable.GetTypeFromTyIdx(expr->tyidx);
  MIRPtrType *pointerty = static_cast<MIRPtrType *>(type);
  CG_ASSERT(pointerty, "expect a pointer type at iread node");
  if (expr->fieldid != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx));
    CG_ASSERT(structty, "SelectIread: non-zero fieldid for non-structure");
    FieldPair thepair = structty->TraverseToField(expr->fieldid);
    type = globaltable.GetTypeFromTyIdx(thepair.second.first);
    offset = becommon.GetFieldOffset(structty, expr->fieldid).first;
  } else {
    type = GetPointedToType(pointerty);
  }

  OfstOperand *offopnd = GetOrCreateOfstOperand(offset, 32);
  Operand *memopnd = GetOrCreateMemOpnd(ArmMemOperand::Addressing_BO, GetPrimTypeSize(expr->primType) * 8,
                                        static_cast<RegOperand *>(addropnd), nullptr, offopnd, nullptr, nullptr);

  RegType regty = GetRegTyFromPrimTy(expr->primType);
  uint32 v_reg_no = New_V_Reg(regty, GetPrimTypeSize(expr->primType));
  RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);

  PrimType dest_type = type->GetPrimType();
  MOperator mop = PickLdInsn(dest_type, expr->primType);
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(mop, result, memopnd));
  return result;
}

Operand *ArmCGFunc::SelectIntconst(MIRIntConst *intconst, PrimType pty) {
  return GetOrCreateImmOperand(intconst->value, GetPrimTypeSize(intconst->type_->GetPrimType()) * 8, false);
}

Operand *ArmCGFunc::SelectFloatconst(MIRFloatConst *floatconst) {
  MIRSymbol *st = func->symtab->CreateSymbol(SCOPE_LOCAL);
  std::string lblstr(".LB_");
  MIRSymbol *func_st = globaltable.GetSymbolFromStidx(func->stidx.Idx());
  std::string funcname = func_st->GetName();
  lblstr.append(funcname).append(to_string(labelidx++));
  st->SetNameStridx(globaltable.GetOrCreateGstridxFromName(lblstr));
  st->storageClass = kScPstatic;
  st->sKind = kStConst;
  st->SetConst(floatconst);
  st->SetTyIdx(PTY_f32);
  return GetOrCreateMemOpnd(st, 0, 32);
}

Operand *ArmCGFunc::SelectDoubleconst(MIRDoubleConst *doubleconst) {
  MIRSymbol *st = func->symtab->CreateSymbol(SCOPE_LOCAL);
  std::string lblstr(".LB_");
  MIRSymbol *func_st = globaltable.GetSymbolFromStidx(func->stidx.Idx());
  lblstr.append(func_st->GetName()).append(to_string(labelidx++));
  st->SetNameStridx(globaltable.GetOrCreateGstridxFromName(lblstr));
  st->storageClass = kScPstatic;
  st->sKind = kStConst;
  st->SetConst(doubleconst);
  st->SetTyIdx(PTY_f64);
  return GetOrCreateMemOpnd(st, 0, 64);
}

}  // namespace maplebe
