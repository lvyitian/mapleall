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
#include "x86_isa.h"
#include "x86_cg.h"
#include "cg_assert.h"

namespace maplebe {

using namespace maple;
using namespace std;

MOperator X86CGFunc::PickLdInsn(PrimType ptype, PrimType rtype) {
  bool is64bits = GetPrimTypeSize(rtype) == 8;
  switch (ptype) {
    case PTY_i8:
      return is64bits ? MOP_ldsbq : MOP_ldsbl;
    case PTY_i16:
      return is64bits ? MOP_ldswq : MOP_ldswl;
    case PTY_i32:
      return is64bits ? MOP_ldslq : MOP_ld32;
    case PTY_i64:
      return MOP_ld64;
    case PTY_u1:
    case PTY_u8:
      return is64bits ? MOP_ldzbq : MOP_ldzbl;
    case PTY_u16:
      return is64bits ? MOP_ldzwq : MOP_ldzwl;
    case PTY_a32:
    case PTY_u32:
      return is64bits ? MOP_ldzlq : MOP_ld32;
    case PTY_a64:
    case PTY_u64:
      return MOP_ld64;
    case PTY_f32:
      return MOP_ldss;
    case PTY_f64:
      return MOP_ldsd;
    default:
      CG_ASSERT(false, "NYI PickLdInsn");
      return MOP_undef;
  }
}

MOperator X86CGFunc::PickLdInsn(uint32 bitsize, PrimType rtype, bool exactsize) {
  bool is64bits = GetPrimTypeSize(rtype) == 8;
  if (!exactsize) {
    bitsize = RoundUp(bitsize, 32);
  }
  if (IsUnsignedInteger(rtype)) {
    switch (bitsize) {
      case 1:
      case 8:
        return is64bits ? MOP_ldzbq : MOP_ldzbl;
      case 16:
        return is64bits ? MOP_ldzwq : MOP_ldzwl;
      case 32:
        return is64bits ? MOP_ldzlq : MOP_ld32;
      case 64:
        return MOP_ld64;
      default:
        CG_ASSERT(false, "NYI PickLdInsn");
        return MOP_undef;
    }
  } else if (IsSignedInteger(rtype)) {
    switch (bitsize) {
      case 8:
        return is64bits ? MOP_ldsbq : MOP_ldsbl;
      case 16:
        return is64bits ? MOP_ldswq : MOP_ldswl;
      case 32:
        return is64bits ? MOP_ldslq : MOP_ld32;
      case 64:
        return MOP_ld64;
      default:
        CG_ASSERT(false, "NYI PickLdInsn");
        return MOP_undef;
    }
  } else {
    switch (rtype) {
      case PTY_f32:
        return MOP_ldss;
      case PTY_f64:
        return MOP_ldsd;
      default:
        CG_ASSERT(false, "NYI PickLdInsn");
        return MOP_undef;
    }
  }
  return MOP_undef;
}

MOperator X86CGFunc::PickStInsn(PrimType primtype) {
  switch (primtype) {
    case PTY_u1:
    case PTY_i8:
    case PTY_u8:
      return MOP_st8;
    case PTY_i16:
    case PTY_u16:
      return MOP_st16;
    case PTY_a32:
    case PTY_i32:
    case PTY_u32:
      return MOP_st32;
    case PTY_i64:
    case PTY_u64:
    case PTY_a64:
      return MOP_st64;
    case PTY_f32:
      return MOP_stss;
    case PTY_f64:
      return MOP_stsd;
    default:
      CG_ASSERT(false, "NYI PickStInsn");
      return MOP_undef;
  }
}

MOperator X86CGFunc::PickStInsn(uint32 bitsize, PrimType rtype, bool exactsize) {
  if (!exactsize) {
    bitsize = RoundUp(bitsize, 32);
  }
  if (IsPrimitiveInteger(rtype) || rtype == PTY_agg) {
    switch (bitsize) {
      case 1:
      case 8:
        return MOP_st8;
      case 16:
        return MOP_st16;
      case 32:
        return MOP_st32;
      case 64:
        return MOP_st64;
      default:
        CG_ASSERT(false, "NYI PickLdInsn");
        return MOP_undef;
    }
  } else {
    switch (rtype) {
      case PTY_f32:
        return MOP_stss;
      case PTY_f64:
        return MOP_stsd;
      default:
        CG_ASSERT(false, "NYI PickLdInsn");
        return MOP_undef;
    }
  }
  return MOP_undef;
}

MOperator X86CGFunc::PickMovInsn(PrimType primtype, bool ismovimm) {
  switch (primtype) {
    case PTY_u1:
    case PTY_i8:
    case PTY_u8:
    case PTY_i16:
    case PTY_u16:
    case PTY_a32:
    case PTY_i32:
    case PTY_u32:
      return ismovimm ? MOP_ldc32 : MOP_mov32;
    case PTY_i64:
    case PTY_u64:
    case PTY_a64:
      return ismovimm ? MOP_ldc64 : MOP_mov64;
    case PTY_f32:
      return MOP_movss;
    case PTY_f64:
      return MOP_movsd;
    default:
      CG_ASSERT(false, "NYI PickMovInsn");
  }
  return 0;
}

MOperator X86CGFunc::PickMovInsn(RegOperand *lhs, RegOperand *rhs, bool ismovimm) {
  CG_ASSERT(lhs->GetRegisterType() == rhs->GetRegisterType(), "PickMovInsn: register types don't match");
  CG_ASSERT(lhs->GetSize() == rhs->GetSize(), "PickMovInsn: sizes don't match");
  if (lhs->GetRegisterType() == kRegTyInt) {
    if (ismovimm) {
      return lhs->GetSize() <= 32 ? MOP_ldc32 : MOP_ldc64;
    } else {
      return lhs->GetSize() <= 32 ? MOP_mov32 : MOP_mov64;
    }
  }
  if (lhs->GetRegisterType() == kRegTyFloat) {
    return lhs->GetSize() <= 32 ? MOP_movss : MOP_movsd;
  }
  CG_ASSERT(false, "PickMovInsn: kind NYI");
  return 0;
}

void X86CGFunc::SelectCopy(Operand *dest, Operand *src, PrimType dtype) {
  uint32 ssize = src->size_;
  uint32 dsize = GetPrimTypeBitSize(dtype);
  CG_ASSERT(dsize >= ssize, "NYI");
  switch (src->op_kind_) {
    case Operand::Opd_Immediate: {
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(ssize > 32 ? MOP_ldc64 : MOP_ldc32, dest, src));
    } break;
    case Operand::Opd_Mem: {
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(ssize, dtype, true), dest, src));
    } break;
    case Operand::Opd_Register: {
      MOperator mop = 0;
      if (IsPrimitiveFloat(dtype)) {
        mop = PickMovInsn(dtype, false);
      } else {
        switch (ssize) {
          case 32: {
            switch (dtype) {
              case PTY_i32:
              case PTY_u32:
              case PTY_a32:
                mop = MOP_mov32;
                break;
              case PTY_i64:
                mop = MOP_movslq;
                break;
              case PTY_u64:
              case PTY_a64:
                mop = MOP_movzlq;
                break;
              default:
                CG_ASSERT(false, "NYI");
                mop = 0;
                break;
            }
            break;
          }
          case 64: {
            switch (dtype) {
              case PTY_i64:
              case PTY_u64:
              case PTY_a64:
                mop = MOP_mov64;
                break;
              default:
                CG_ASSERT(false, "NYI");
                break;
            }
            break;
          }
          default:
            CG_ASSERT(false, "NYI");
            break;
        }
      }
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, dest, src));
    } break;
    default:
      CG_ASSERT(false, "NYI");
      break;
  }
}

// this function copy src to a register, the src can be an imm, mem or a lebel
Operand *X86CGFunc::SelectCopy(Operand *src, PrimType dtype) {
  Operand *dest = CreateOpndOfType(dtype);
  SelectCopy(dest, src, dtype);
  return dest;
}

void X86CGFunc::SelectDassign(DassignNode *stmt, Operand *opnd0) {
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
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, st_opnd, memopnd));
}

void X86CGFunc::SelectRegassign(RegassignNode *stmt, Operand *opnd0) {
  PregIdx pregidx = stmt->regidx;
  uint32 v_reg_no = pregidx + first_mapleir_v_reg_no;
  RegOperand *regopnd = CreateVirtualRegisterOperand(v_reg_no);
  // look at rhs
  if (opnd0->op_kind_ == Operand::Opd_Mem) {
    X86MemOperand *rhsmemopnd = static_cast<X86MemOperand *>(opnd0);
    curbb->AppendInsn(
      cg->BuildInstruction<X86Insn>(PickLdInsn(rhsmemopnd->size_, stmt->uopnd->primType), regopnd, rhsmemopnd));
  } else if (opnd0->op_kind_ == Operand::Opd_Register) {
    X86RegOperand *rhsregopnd = static_cast<X86RegOperand *>(opnd0);
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickMovInsn(regopnd, rhsregopnd), regopnd, rhsregopnd));
  } else if (opnd0->op_kind_ == Operand::Opd_Immediate) {
    X86RegOperand *rhsregopnd = static_cast<X86RegOperand *>(SelectCopy(opnd0, stmt->uopnd->primType));
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickMovInsn(regopnd, rhsregopnd), regopnd, rhsregopnd));
  } else {
    CG_ASSERT(false, "SelectDassign ispreg NYI");
  }
}

void X86CGFunc::SelectAggDassign(DassignNode *stmt) {
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
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(alignused * 8, PTY_u32), result, rhsmemopnd));
      // generate the store
      lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + i * alignused, alignused * 8);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickStInsn(alignused * 8, PTY_u32, true), result, lhsmemopnd));
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
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(newalignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + lhssize_covered, newalignused * 8);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickStInsn(newalignused * 8, PTY_u32, true), result, lhsmemopnd));
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
      OfstOperand *offopnd = GetOrCreateOfstOpnd(rhsoffset + i * alignused, 32);
      rhsmemopnd = GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, alignused * 8, addropnd, nullptr, offopnd, nullptr,
                                      static_cast<MIRSymbol *>(nullptr));
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, alignused));
      RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(alignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + i * alignused, alignused * 8);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickStInsn(alignused * 8, PTY_u32, true), result, lhsmemopnd));
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
      OfstOperand *offopnd = GetOrCreateOfstOpnd(rhsoffset + lhssize_covered, 32);
      rhsmemopnd = GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, newalignused * 8, addropnd, nullptr, offopnd,
                                      nullptr, static_cast<MIRSymbol *>(nullptr));
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
      RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(newalignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      lhsmemopnd = GetOrCreateMemOpnd(lhssymbol, lhsoffset + lhssize_covered, newalignused * 8);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickStInsn(newalignused * 8, PTY_u32, true), result, lhsmemopnd));
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

void X86CGFunc::SelectIassign(IassignNode *stmt) {
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

  OfstOperand *offopnd = GetOrCreateOfstOpnd(offset, 32);
  PrimType dest_type = type->GetPrimType();
  Operand *memopnd = GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, GetPrimTypeSize(dest_type) * 8,
                                        static_cast<RegOperand *>(addropnd), nullptr, offopnd, nullptr, nullptr);
  MOperator mop = PickStInsn(dest_type);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, src_opnd, memopnd));
}

void X86CGFunc::SelectAggIassign(IassignNode *stmt, Operand *lhsaddropnd) {
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
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(alignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      OfstOperand *offopnd = GetOrCreateOfstOpnd(lhsoffset + i * alignused, 32);
      lhsmemopnd =
        GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, alignused * 8, static_cast<RegOperand *>(lhsaddropnd), nullptr,
                           offopnd, nullptr, static_cast<MIRSymbol *>(nullptr));
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickStInsn(alignused * 8, PTY_u32, true), result, lhsmemopnd));
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
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(newalignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      OfstOperand *offopnd = GetOrCreateOfstOpnd(lhsoffset + lhssize_covered, 32);
      lhsmemopnd =
        GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, newalignused * 8, static_cast<RegOperand *>(lhsaddropnd),
                           nullptr, offopnd, nullptr, static_cast<MIRSymbol *>(nullptr));
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickStInsn(newalignused * 8, PTY_u32, true), result, lhsmemopnd));
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
    Operand *rhsmemopnd = nullptr;
    Operand *lhsmemopnd = nullptr;
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      OfstOperand *rhsoffopnd = GetOrCreateOfstOpnd(rhsoffset + i * alignused, 32);
      rhsmemopnd = GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, alignused * 8, rhsaddropnd, nullptr, rhsoffopnd,
                                      nullptr, static_cast<MIRSymbol *>(nullptr));
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, alignused));
      X86RegOperand *result = memPool->New<X86RegOperand>(v_reg_no, v_reg_table[v_reg_no].size * 8, kRegTyInt);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(alignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      OfstOperand *lhsoffopnd = GetOrCreateOfstOpnd(lhsoffset + i * alignused, 32);
      lhsmemopnd =
        GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, alignused * 8, static_cast<RegOperand *>(lhsaddropnd), nullptr,
                           lhsoffopnd, nullptr, static_cast<MIRSymbol *>(nullptr));
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickStInsn(alignused * 8, PTY_u32, true), result, lhsmemopnd));
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
      OfstOperand *rhsoffopnd = GetOrCreateOfstOpnd(rhsoffset + lhssize_covered, 32);
      rhsmemopnd = GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, newalignused * 8, rhsaddropnd, nullptr, rhsoffopnd,
                                      nullptr, static_cast<MIRSymbol *>(nullptr));
      uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
      X86RegOperand *result = memPool->New<X86RegOperand>(v_reg_no, v_reg_table[v_reg_no].size * 8, kRegTyInt);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(newalignused * 8, PTY_u32, true), result, rhsmemopnd));
      // generate the store
      OfstOperand *lhsoffopnd = GetOrCreateOfstOpnd(lhsoffset + lhssize_covered, 32);
      lhsmemopnd =
        GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, newalignused * 8, static_cast<RegOperand *>(lhsaddropnd),
                           nullptr, lhsoffopnd, nullptr, static_cast<MIRSymbol *>(nullptr));
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickStInsn(newalignused * 8, PTY_u32, true), result, lhsmemopnd));
      lhssize_covered += newalignused;
    }
  }
}

Operand *X86CGFunc::SelectDread(base_node_t *parent, AddrofNode *expr) {
  MIRModule &mirModule = *g->mirModule;
  PrimType ptype = expr->primType;
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

  if (GetPrimTypeSize(symty) != GetPrimTypeSize(ptype)) {
    return SelectCopy(memopnd, ptype);
  } else {
    return memopnd;
  }
}

RegOperand *X86CGFunc::SelectRegread(base_node_t *parent, RegreadNode *expr) {
  PregIdx pregidx = expr->regidx;
  uint32 v_reg_no = pregidx + first_mapleir_v_reg_no;
  return CreateVirtualRegisterOperand(v_reg_no);
}

Operand *X86CGFunc::SelectAddrof(AddrofNode *expr) {
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
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(GetPrimTypeSize(primType) == 8 ? MOP_lea64 : MOP_lea32, result, memopnd));

  return result;
}

Operand *X86CGFunc::SelectAddroffunc(AddroffuncNode *expr) {
  CG_ASSERT(false, "X86CGFunc::SelectAddroffunc not implemented.");
  return nullptr;
}

Operand *X86CGFunc::SelectIread(base_node_t *parent, IreadNode *expr) {
  Operand *addropnd = HandleExpr(expr, expr->Opnd(0));
  if (addropnd->op_kind_ != Operand::Opd_Register) {
    addropnd = static_cast<RegOperand *>(SelectCopy(addropnd, expr->Opnd(0)->primType));
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

  OfstOperand *offopnd = GetOrCreateOfstOpnd(offset, 32);
  Operand *memopnd = GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, GetPrimTypeSize(expr->primType) * 8,
                                        static_cast<RegOperand *>(addropnd), nullptr, offopnd, nullptr, nullptr);

  RegType regty = GetRegTyFromPrimTy(expr->primType);
  uint32 v_reg_no = New_V_Reg(regty, GetPrimTypeSize(expr->primType));
  RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);

  PrimType dest_type = type->GetPrimType();
  MOperator mop = PickLdInsn(dest_type, expr->primType);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, result, memopnd));
  return result;
}

Operand *X86CGFunc::SelectIntconst(MIRIntConst *intconst) {
  return GetOrCreateImmOperand(intconst->value, GetPrimTypeSize(intconst->type_->GetPrimType()) * 8, false);
}

Operand *X86CGFunc::SelectFloatconst(MIRFloatConst *floatconst) {
  if (!floatconst->st_) {
    MIRSymbol *st = globaltable.CreateSymbol(SCOPE_GLOBAL);
    std::string cur_num_str;
    cur_num_str.append(".FC").append(to_string(g->mirModule->GetAndIncFloatNum()));
    st->SetNameStridx(globaltable.GetOrCreateGstridxFromName(cur_num_str));
    if (!globaltable.AddToStringSymbolMap(st)) {
      CG_ASSERT(false, "duplicated string met");
    }
    st->storageClass = kScFstatic;
    st->sKind = kStConst;
    st->SetConst(floatconst);
    st->SetTyIdx(PTY_f32);
    floatconst->st_ = st;
  }
  return GetOrCreateMemOpnd(floatconst->st_, 0, 32);
}

Operand *X86CGFunc::SelectDoubleconst(MIRDoubleConst *doubleconst) {
  if (!doubleconst->st_) {
    MIRSymbol *st = globaltable.CreateSymbol(SCOPE_GLOBAL);
    std::string cur_num_str;
    cur_num_str.append(".FC").append(to_string(g->mirModule->GetAndIncFloatNum()));
    st->SetNameStridx(globaltable.GetOrCreateGstridxFromName(cur_num_str));
    if (!globaltable.AddToStringSymbolMap(st)) {
      CG_ASSERT(false, "duplicated string met");
    }
    st->storageClass = kScFstatic;
    st->sKind = kStConst;
    st->SetConst(doubleconst);
    st->SetTyIdx(PTY_f64);
    doubleconst->st_ = st;
  }
  return GetOrCreateMemOpnd(doubleconst->st_, 0, 64);
}

}  // namespace maplebe
