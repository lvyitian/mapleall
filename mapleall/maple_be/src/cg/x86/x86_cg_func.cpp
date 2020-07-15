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

void X86CGFunc::GenerateProlog(BB *bb) {
  BB *formal_curbb = curbb;
  dummybb->ClearInsns();
  curbb = dummybb;
  Operand *sp_opnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
  Operand *fp_opnd = GetOrCreatePhysicalRegisterOperand(RBP, 64, kRegTyInt);
  Operand *sp_opnd2 = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_pushq, sp_opnd, fp_opnd, sp_opnd2));
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_mov64, fp_opnd, sp_opnd));

  // decrement the stack pointer
  Operand *immopnd = GetOrCreateImmOperand(-static_cast<X86MemLayout *>(memlayout)->StackFrameSize(), 32, true);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_add64ri, sp_opnd, sp_opnd2, immopnd));

  // fetch parameters passed in registers and store to their home locations
  ParmLocator parmlocator(becommon);
  PLocInfo ploc;
  for (uint32 i = 0; i < func->formals.size(); i++) {
    MIRType *ty = globaltable.GetTypeFromTyIdx(func->argumentsTyidx[i]);
    parmlocator.LocateNextParm(ty, ploc);
    if (ploc.reg0 == 0) {  // passed in memory, so do nothing
      continue;
    }
    MIRSymbol *sym = func->formals[i];
    int32 symsize = becommon.type_size_table[ty->_ty_idx.idx];
    RegType regtype = ploc.reg0 <= R15 ? kRegTyInt : kRegTyFloat;
    RegOperand *regopnd = GetOrCreatePhysicalRegisterOperand(ploc.reg0, RoundUp(std::min(8, symsize), 4) * 8, regtype);
    MemOperand *memopnd = GetOrCreateMemOpnd(sym, 0, std::min(8, symsize) * 8);
    MOperator mop = PickStInsn(std::min(8, symsize) * 8, regtype == kRegTyInt ? PTY_i32 : PTY_f32);
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, regopnd, memopnd));
    if (ploc.reg1 == 0) {
      continue;
    }
    regtype = ploc.reg1 <= R15 ? kRegTyInt : kRegTyFloat;
    regopnd = GetOrCreatePhysicalRegisterOperand(ploc.reg1, (symsize - 8) * 8, regtype);
    memopnd = GetOrCreateMemOpnd(sym, 8, (symsize - 8) * 8);
    mop = (regtype == kRegTyInt) ? MOP_st64 : MOP_stsd;
    curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, regopnd, memopnd));
  }
  bb->InsertAtBeginning(dummybb);
  curbb = formal_curbb;
}

void X86CGFunc::GenerateEpilog(BB *bb) {
  bb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_leave));
  bb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_ret));
}

Insn *X86CGFunc::GenerateCfiRestores(BB *lastbb) {
  Insn *ipoint = nullptr;

  Insn *def_cfa_inst = cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RSP, 64));
  if (lastbb->firstinsn) {
    ipoint = lastbb->InsertInsnAfter(lastbb->lastinsn, def_cfa_inst);
  } else {
    ipoint = lastbb->firstinsn = lastbb->lastinsn = def_cfa_inst;
  }
  return ipoint;
}

RegOperand *X86CGFunc::GetTargetRetOperand(PrimType ptype) {
  return GetOrCreatePhysicalRegisterOperand(IsPrimitiveFloat(ptype) ? XMM0 : RAX, GetPrimTypeSize(ptype) * 8,
                                            GetRegTyFromPrimTy(ptype));
}

Operand *X86CGFunc::CreateOpndOfReg(X86RegOperand *opnd) {
  uint32 size = opnd->GetSize();
  regno_t v_reg_no = New_V_Reg(opnd->GetRegisterType(), size);
  return memPool->New<X86RegOperand>(v_reg_no, size * 8, opnd->GetRegisterType());
}

Operand *X86CGFunc::CreateOpndOfType(PrimType primtype) {
  RegType regty = GetRegTyFromPrimTy(primtype);
  uint32 size = GetPrimTypeSize(primtype);
  regno_t v_reg_no = New_V_Reg(regty, size);
  return memPool->New<X86RegOperand>(v_reg_no, size * 8, regty);
}

Operand *X86CGFunc::CreateRflagOpnd() {
  regno_t v_reg_no = New_V_Reg(RegTy_cc, 8);
  return memPool->New<X86RegOperand>(v_reg_no, 64, RegTy_cc);
}

// create an immediate operand from primType
Operand *X86CGFunc::CreateImmOperand(PrimType primType, int64 val) {
  CG_ASSERT(IsPrimitiveInteger(primType), "must be integer");
  return GetOrCreateImmOperand(val, GetPrimTypeSize(primType) * 8, IsSignedInteger(primType));
}

// create an float immediate operand.
Operand *X86CGFunc::CreateFloatImmOperand(float val) {
  MIRFloatConst *fconst = globaltable.GetOrCreateFloatConst(val);
  MIRSymbol *st = globaltable.CreateSymbol(SCOPE_GLOBAL);
  // string cur_num_str = ".FC"+ to_string(cg->mirModule.GetAndIncFloatNum());
  std::string cur_num_str;
  cur_num_str.append(".FC");
  cur_num_str.append(to_string(g->mirModule->GetAndIncFloatNum()));

  st->SetNameStridx(globaltable.GetOrCreateGstridxFromName(cur_num_str));
  if (!globaltable.AddToStringSymbolMap(st)) {
    CG_ASSERT(false, "duplicated string met");
  }
  st->storageClass = kScFstatic;
  st->sKind = kStConst;
  st->SetConst(fconst);
  st->SetTyIdx(PTY_f32);
  fconst->st_ = st;
  return GetOrCreateMemOpnd(st, 0, GetPrimTypeSize(PTY_f32));
}

// create an double immediate operand.
Operand *X86CGFunc::CreateDoubleImmOperand(double val) {
  MIRDoubleConst *dconst = globaltable.GetOrCreateDoubleConst(val);
  MIRSymbol *st = globaltable.CreateSymbol(SCOPE_GLOBAL);
  std::string cur_num_str;
  MapleString temp(to_string(g->mirModule->GetAndIncFloatNum()), memPool);
  cur_num_str.append(".FC").append(temp);
  st->SetNameStridx(globaltable.GetOrCreateGstridxFromName(cur_num_str));
  if (!globaltable.AddToStringSymbolMap(st)) {
    CG_ASSERT(false, "duplicated string met");
  }
  st->storageClass = kScFstatic;
  st->sKind = kStConst;
  st->SetConst(dconst);
  st->SetTyIdx(PTY_f64);
  dconst->st_ = st;
  return GetOrCreateMemOpnd(st, 0, GetPrimTypeSize(PTY_f64));
}

// create an insn that zero operand
Operand *X86CGFunc::CreateZeroOperand(PrimType primType) {
  Operand *result = CreateOpndOfType(primType);
  MOperator mop = MOP_undef;
  switch (primType) {
    case PTY_i8:
    case PTY_u8:
    case PTY_i16:
    case PTY_u16:
    case PTY_i32:
    case PTY_u32:
    case PTY_a32:
      mop = MOP_zero32i;
      break;
    case PTY_a64:
    case PTY_f32:
      mop = MOP_zero32f;
      break;
      break;
    case PTY_f64:
      mop = MOP_zero64f;
      break;
    default:
      CG_ASSERT(false, "don't need a zero opnd or NYI");
      break;
  }
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, result));
  return result;
}

void X86CGFunc::SelectParmList(StmtNode *narynode) {
  MIRModule &mirModule = *g->mirModule;
  ParmLocator parmlocator(becommon);  // instantiate a parm locator
  PLocInfo ploc;
  int32 i = 0;
  if (narynode->op == OP_icall) {
    i++;
  }
  for (; i < narynode->NumOpnds(); i++) {
    MIRType *ty = nullptr;
    base_node_t *argexpr = GenericOpnd(narynode, i);
    PrimType ptype = argexpr->primType;
    CG_ASSERT(ptype != PTY_void, "");
    if (ptype != PTY_agg) {
      ty = globaltable.typeTable[static_cast<uint32>(ptype)];
      Operand *opnd = HandleExpr(narynode, argexpr);
      Operand *st_opnd = opnd;
      if (opnd->op_kind_ != Operand::Opd_Register) {
        st_opnd = SelectCopy(opnd, ptype);
      }
      RegOperand *expregopnd = static_cast<RegOperand *>(st_opnd);
      parmlocator.LocateNextParm(ty, ploc);
      if (ploc.reg0 != 0) {  // load to the register
        RegOperand *parmregopnd =
          GetOrCreatePhysicalRegisterOperand(ploc.reg0, expregopnd->size_, GetRegTyFromPrimTy(ptype));
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickMovInsn(ptype), parmregopnd, expregopnd));
      } else {  // store to actual memory segment
        MemOperand *actmemopnd = CreateMemOpnd(RSP, ploc.memoffset, GetPrimTypeSize(ptype) * 8);
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickStInsn(ptype), expregopnd, actmemopnd));
      }
      CG_ASSERT(ploc.reg1 == 0, "SelectCall NYI");
    } else {
      Opcode opnd_opcode = argexpr->op;
      CG_ASSERT(opnd_opcode == OP_dread || opnd_opcode == OP_iread, "");
      if (opnd_opcode == OP_dread) {
        AddrofNode *dread = static_cast<AddrofNode *>(argexpr);
        MIRSymbol *sym = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dread->stidx);

        ty = globaltable.GetTypeFromTyIdx(sym->GetTyIdx());
        int32 offset = 0;
        if (dread->fieldid != 0) {
          MIRStructType *structty = dynamic_cast<MIRStructType *>(ty);
          CG_ASSERT(structty, "SelectParmList: non-zero fieldid for non-structure");
          FieldPair thepair = structty->TraverseToField(dread->fieldid);
          ty = globaltable.GetTypeFromTyIdx(thepair.second.first);
          offset = becommon.GetFieldOffset(structty, dread->fieldid).first;
        }
        parmlocator.LocateNextParm(ty, ploc);
        uint64 sizeused;
        MemOperand *memopnd = nullptr;
        RegOperand *result = nullptr;
        if (ploc.reg0 != 0) {
          sizeused = std::min(static_cast<uint64>(8), becommon.type_size_table[ty->_ty_idx.idx]);
          memopnd = GetOrCreateMemOpnd(sym, offset, sizeused * 8);
          RegType regtype = X86RegOperand::GetRegTypeFromReg(ploc.reg0);
          result = GetOrCreatePhysicalRegisterOperand(ploc.reg0, sizeused * 8, regtype);
          if (regtype == kRegTyInt) {
            ptype = sizeused <= 4 ? PTY_u32 : PTY_u64;
          } else {
            ptype = sizeused <= 4 ? PTY_f32 : PTY_f64;
          }
          curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(ptype, ptype), result, memopnd));
          if (ploc.reg1 != 0) {
            offset = offset + sizeused;
            sizeused = becommon.type_size_table[ty->_ty_idx.idx] - sizeused;
            memopnd = GetOrCreateMemOpnd(sym, offset, sizeused * 8);
            result = GetOrCreatePhysicalRegisterOperand(ploc.reg1, sizeused * 8, regtype);
            if (regtype == kRegTyInt) {
              ptype = sizeused <= 4 ? PTY_u32 : PTY_u64;
            } else {
              ptype = sizeused <= 4 ? PTY_f32 : PTY_f64;
            }
            curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(ptype, ptype), result, memopnd));
          }
        } else {  // ploc.reg0 == 0, store to actual memory segment
          Operand *actmemopnd = nullptr;
          uint32 alignused = becommon.type_align_table[ty->_ty_idx.idx];
          sizeused = becommon.type_size_table[ty->_ty_idx.idx];
          for (uint32 i = 0; i < (sizeused / alignused); i++) {
            // generate the load
            memopnd = GetOrCreateMemOpnd(sym, offset + i * alignused, alignused * 8);
            uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, alignused));
            result = CreateVirtualRegisterOperand(v_reg_no);
            curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(alignused * 8, PTY_u32), result, memopnd));
            // generate the store
            actmemopnd = CreateMemOpnd(RSP, ploc.memoffset + i * alignused, alignused * 8);
            curbb->AppendInsn(
              cg->BuildInstruction<X86Insn>(PickStInsn(alignused * 8, PTY_u32, true), result, actmemopnd));
          }
          // extra content at the end less than the unit of alignused
          uint32 size_covered = (sizeused / alignused) * alignused;
          uint32 newalignused = alignused;
          while (size_covered < sizeused) {
            newalignused = newalignused >> 1;
            if (size_covered + newalignused > sizeused) {
              continue;
            }
            // generate the load
            memopnd = GetOrCreateMemOpnd(sym, offset + size_covered, newalignused * 8);
            uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
            RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);
            curbb->AppendInsn(
              cg->BuildInstruction<X86Insn>(PickLdInsn(newalignused * 8, PTY_u32, true), result, memopnd));
            // generate the store
            actmemopnd = CreateMemOpnd(RSP, ploc.memoffset + size_covered, newalignused * 8);
            curbb->AppendInsn(
              cg->BuildInstruction<X86Insn>(PickStInsn(newalignused * 8, PTY_u32, true), result, actmemopnd));
            size_covered += newalignused;
          }
        }
      } else {  // OP_iread
        IreadNode *iread = static_cast<IreadNode *>(argexpr);
        RegOperand *addropnd = static_cast<RegOperand *>(HandleExpr(iread, iread->Opnd(0)));
        if (addropnd->op_kind_ != Operand::Opd_Register) {
          addropnd = static_cast<RegOperand *>(SelectCopy(addropnd, iread->Opnd(0)->primType));
        }
        ty = globaltable.GetTypeFromTyIdx(iread->tyidx);
        CG_ASSERT(ty->typeKind == kTypePointer, "");
        ty = globaltable.GetTypeFromTyIdx(static_cast<MIRPtrType *>(ty)->pointedTyIdx);
        int32 offset = 0;
        if (iread->fieldid != 0) {
          MIRStructType *structty = dynamic_cast<MIRStructType *>(ty);
          CG_ASSERT(structty, "SelectParmList: non-zero fieldid for non-structure");
          FieldPair thepair = structty->TraverseToField(iread->fieldid);
          ty = globaltable.GetTypeFromTyIdx(thepair.second.first);
          offset = becommon.GetFieldOffset(structty, iread->fieldid).first;
        }
        parmlocator.LocateNextParm(ty, ploc);
        uint64 sizeused;
        Operand *memopnd = nullptr;
        Operand *result = nullptr;
        if (ploc.reg0 != 0) {
          sizeused = std::min(static_cast<uint64>(8), becommon.type_size_table[ty->_ty_idx.idx]);
          OfstOperand *offopnd = GetOrCreateOfstOpnd(offset, 32);
          memopnd =
            GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, sizeused, addropnd, nullptr, offopnd, nullptr, nullptr);
          RegType regtype = X86RegOperand::GetRegTypeFromReg(ploc.reg0);
          result = GetOrCreatePhysicalRegisterOperand(ploc.reg0, sizeused * 8, regtype);
          if (regtype == kRegTyInt) {
            ptype = sizeused <= 4 ? PTY_u32 : PTY_u64;
          } else {
            ptype = sizeused <= 4 ? PTY_f32 : PTY_f64;
          }
          curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(ptype, ptype), result, memopnd));
          if (ploc.reg1 != 0) {
            offset = offset + sizeused;
            sizeused = becommon.type_size_table[ty->_ty_idx.idx] - sizeused;
            offopnd = GetOrCreateOfstOpnd(offset, 32);
            memopnd =
              GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, sizeused, addropnd, nullptr, offopnd, nullptr, nullptr);
            regtype = X86RegOperand::GetRegTypeFromReg(ploc.reg0);
            result = GetOrCreatePhysicalRegisterOperand(ploc.reg1, sizeused * 8, regtype);
            if (regtype == kRegTyInt) {
              ptype = sizeused <= 4 ? PTY_u32 : PTY_u64;
            } else {
              ptype = sizeused <= 4 ? PTY_f32 : PTY_f64;
            }
            curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(ptype, ptype), result, memopnd));
          }
        } else {  // ploc.reg0 == 0, store to actual memory segment
          Operand *actmemopnd = nullptr;
          OfstOperand *offopnd;
          uint32 alignused = becommon.type_align_table[ty->_ty_idx.idx];
          sizeused = becommon.type_size_table[ty->_ty_idx.idx];
          for (uint32 i = 0; i < (sizeused / alignused); i++) {
            // generate the load
            offopnd = GetOrCreateOfstOpnd(offset + i * alignused, 32);
            memopnd =
              GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, alignused, addropnd, nullptr, offopnd, nullptr, nullptr);
            uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, alignused));
            result = CreateVirtualRegisterOperand(v_reg_no);
            curbb->AppendInsn(cg->BuildInstruction<X86Insn>(PickLdInsn(alignused * 8, PTY_u32), result, memopnd));
            // generate the store
            actmemopnd = CreateMemOpnd(RSP, ploc.memoffset + i * alignused, alignused * 8);
            curbb->AppendInsn(
              cg->BuildInstruction<X86Insn>(PickStInsn(alignused * 8, PTY_u32, true), result, actmemopnd));
          }
          // extra content at the end less than the unit of alignused
          uint32 size_covered = (sizeused / alignused) * alignused;
          uint32 newalignused = alignused;
          while (size_covered < sizeused) {
            newalignused = newalignused >> 1;
            if (size_covered + newalignused > sizeused) {
              continue;
            }
            // generate the load
            offopnd = GetOrCreateOfstOpnd(offset + size_covered, 32);
            memopnd = GetOrCreateMemOpnd(X86MemOperand::Addressing_BO, newalignused, addropnd, nullptr, offopnd,
                                         nullptr, nullptr);
            uint32 v_reg_no = New_V_Reg(kRegTyInt, std::max(4u, newalignused));
            RegOperand *result = CreateVirtualRegisterOperand(v_reg_no);
            curbb->AppendInsn(
              cg->BuildInstruction<X86Insn>(PickLdInsn(newalignused * 8, PTY_u32, true), result, memopnd));
            // generate the store
            actmemopnd = CreateMemOpnd(RSP, ploc.memoffset + size_covered, newalignused * 8);
            curbb->AppendInsn(
              cg->BuildInstruction<X86Insn>(PickStInsn(newalignused * 8, PTY_u32, true), result, actmemopnd));
            size_covered += newalignused;
          }
        }
      }
    }
  }
}

void X86CGFunc::SelectCall(CallNode *callnode) {
  SelectParmList(callnode);
  MIRFunction *fn = globaltable.GetFunctionFromPuidx(callnode->puidx);
  MemOperand *targetopnd =
    GetOrCreateMemOpnd(g->mirModule->CurFunction()->GetLocalOrGlobalSymbol(fn->stidx, false), 0, 0);
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_call, targetopnd));
}

void X86CGFunc::SelectIcall(IcallNode *icallnode, Operand *fptropnd) {
  SelectParmList(icallnode);
  if (fptropnd->op_kind_ != Operand::Opd_Register) {
    fptropnd = SelectCopy(fptropnd, icallnode->Opnd(0)->primType);
  }
  RegOperand *regopnd = dynamic_cast<RegOperand *>(fptropnd);
  CG_ASSERT(regopnd, "SelectIcall: function pointer not RegOperand");
  curbb->AppendInsn(cg->BuildInstruction<X86Insn>(MOP_icallr, regopnd));
}

void X86CGFunc::SelectReturn(NaryStmtNode *stmt, Operand *opnd0) {
  ReturnMechanism retmech(globaltable.GetTypeFromTyIdx(func->returnTyidx), becommon);
  X86RegOperand *retopnd = nullptr;
  if (retmech.regcount > 0) {
    if (X86RegOperand *regopnd = dynamic_cast<X86RegOperand *>(opnd0)) {
      if (regopnd->GetRegisterNumber() != retmech.reg0) {
        retopnd = GetOrCreatePhysicalRegisterOperand(retmech.reg0, regopnd->size_, GetRegTyFromPrimTy(retmech.ptype0));
        MOperator mop = PickMovInsn(retopnd, regopnd, false);
        curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, retopnd, regopnd));
      }
    } else if (X86ImmOperand *immopnd = dynamic_cast<X86ImmOperand *>(opnd0)) {
      retopnd = GetOrCreatePhysicalRegisterOperand(retmech.reg0, GetPrimTypeSize(retmech.ptype0) * 8, kRegTyInt);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(immopnd->size_ > 32 ? MOP_ldc64 : MOP_ldc32, retopnd, opnd0));
    } else if (X86MemOperand *memopnd = dynamic_cast<X86MemOperand *>(opnd0)) {
      retopnd = GetOrCreatePhysicalRegisterOperand(retmech.reg0, GetPrimTypeSize(retmech.ptype0) * 8,
                                                   GetRegTyFromPrimTy(retmech.ptype0));
      MOperator mop = PickLdInsn(memopnd->size_, stmt->nopnd[0]->primType, false);
      curbb->AppendInsn(cg->BuildInstruction<X86Insn>(mop, retopnd, memopnd));
    } else {
      CG_ASSERT(false, "SelectReturn: NYI");
    }
  } else if (opnd0 != nullptr) {  // pass in memory
    CG_ASSERT(false, "SelectReturn: return in memory NYI");
    if (retmech.fake_first_parm) {
    }
  }
  exitbbsvec.push_back(curbb);
}

X86RegOperand *X86CGFunc::GetOrCreatePhysicalRegisterOperand(regno_t reg_no, uint8 size, RegType type) {
  X86RegOperand tregopnd(reg_no, size, type);
  MapleMap<X86RegOperand, X86RegOperand *>::iterator it = hash_regopnd_tb_.find(tregopnd);
  if (it != hash_regopnd_tb_.end()) {
    return it->second;
  }
  X86RegOperand *res = memPool->New<X86RegOperand>(reg_no, size, type);
  hash_regopnd_tb_[tregopnd] = res;
  return res;
}

ImmOperand *X86CGFunc::GetOrCreateImmOperand(int64 val, uint8 size, bool is_signed) {
  X86ImmOperand timmopnd(val, size, is_signed);
  auto it = hash_immopnd_tb_.find(timmopnd);
  if (it != hash_immopnd_tb_.end()) {
    return it->second;
  }
  X86ImmOperand *res = memPool->New<X86ImmOperand>(val, size, is_signed);
  hash_immopnd_tb_[timmopnd] = res;
  return res;
}

LabelOperand *X86CGFunc::GetOrCreateLabelOperand(LabelIdx labidx) {
  MapleMap<LabelIdx, LabelOperand *>::iterator it = hash_lblopnd_tb_.find(labidx);
  if (it != hash_lblopnd_tb_.end()) {
    return it->second;
  }
  MIRSymbol *func_st = globaltable.GetSymbolFromStidx(func->stidx.Idx());
  LabelOperand *res = memPool->New<LabelOperand>(func_st->GetName().c_str(), labidx);
  hash_lblopnd_tb_[labidx] = res;
  return res;
}

X86OfstOperand *X86CGFunc::GetOrCreateOfstOpnd(uint32 offset, uint32 size) {
  X86OfstOperand tofstopnd(offset, size);
  auto it = hash_ofstopnd_tb_.find(tofstopnd);
  if (it != hash_ofstopnd_tb_.end()) {
    return it->second;
  }
  X86OfstOperand *res = memPool->New<X86OfstOperand>(offset, size);
  hash_ofstopnd_tb_[tofstopnd] = res;
  return res;
}

StImmOperand *X86CGFunc::GetOrCreateStImmOperand(MIRSymbol *st, int64 offset, int32 relocs) {
  StImmOperand tstimmopnd(st, offset, relocs);
  MapleMap<StImmOperand, StImmOperand *>::iterator it = hash_stimmopnd_tb_.find(tstimmopnd);
  if (it != hash_stimmopnd_tb_.end()) {
    return it->second;
  }
  StImmOperand *res = memPool->New<StImmOperand>(st, offset, relocs);
  hash_stimmopnd_tb_[tstimmopnd] = res;
  return res;
}

MemOperand *X86CGFunc::GetOrCreateMemOpnd(MIRSymbol *symbol, int32 offset, int32 size) {
  MIRStorageClass storageClass = symbol->storageClass;
  if (storageClass == kScAuto || storageClass == kScFormal) {
    SymbolAlloc symalloc = *memlayout->sym_alloc_table[symbol->GetStIndex()];
    X86SymbolAlloc *psymalloc = static_cast<X86SymbolAlloc *>(&symalloc);
    RegOperand *base_opnd = CreateVirtualRegisterOperand(psymalloc->BaseReg());
    OfstOperand *offset_opnd = GetOrCreateOfstOpnd(psymalloc->Offset() + offset, 32);
    X86MemOperand tmemopnd(X86MemOperand::Addressing_BO, size, base_opnd, nullptr, offset_opnd, nullptr, symbol);
    auto it = hash_memopnd_tb_.find(tmemopnd);
    if (it != hash_memopnd_tb_.end()) {
      return it->second;
    }
    X86MemOperand *res = memPool->New<X86MemOperand>(tmemopnd);
    hash_memopnd_tb_[tmemopnd] = res;
    return res;
  } else if (storageClass == kScText) {
    X86MemOperand tmemopnd(X86MemOperand::Addressing_TEXT, size, nullptr, nullptr, nullptr, nullptr, symbol);
    auto it = hash_memopnd_tb_.find(tmemopnd);
    if (it != hash_memopnd_tb_.end()) {
      return it->second;
    }
    X86MemOperand *res = memPool->New<X86MemOperand>(tmemopnd);
    hash_memopnd_tb_[tmemopnd] = res;
    return res;
  } else if (storageClass == kScGlobal || storageClass == kScExtern || storageClass == kScFstatic) {
    Operand *offset_opnd = GetOrCreateStImmOperand(symbol, offset, 0);
    X86MemOperand tmemopnd(X86MemOperand::Addressing_O, size, nullptr, nullptr, offset_opnd, nullptr, symbol);
    auto it = hash_memopnd_tb_.find(tmemopnd);
    if (it != hash_memopnd_tb_.end()) {
      return it->second;
    }
    X86MemOperand *res = memPool->New<X86MemOperand>(tmemopnd);
    hash_memopnd_tb_[tmemopnd] = res;
    return res;
  } else {
    CG_ASSERT(false, "NYI");
    return nullptr;
  }
}

X86MemOperand *X86CGFunc::GetOrCreateMemOpnd(X86MemOperand::X86AdrMode mode, int32 size, RegOperand *base,
                                             RegOperand *index, Operand *offset, Operand *scale, MIRSymbol *st) {
  X86MemOperand tmemopnd(mode, size, base, index, offset, scale, st);
  auto it = hash_memopnd_tb_.find(tmemopnd);
  if (it != hash_memopnd_tb_.end()) {
    return it->second;
  }
  X86MemOperand *res = memPool->New<X86MemOperand>(tmemopnd);
  hash_memopnd_tb_[tmemopnd] = res;
  return res;
}

MemOperand *X86CGFunc::CreateMemOpnd(RegOperand *base_opnd, int32 offset, int32 size) {
  OfstOperand *offset_opnd = GetOrCreateOfstOpnd(offset, 32);
  X86MemOperand tmemopnd(X86MemOperand::Addressing_BO, size, base_opnd, nullptr, offset_opnd, nullptr, nullptr);
  auto it = hash_memopnd_tb_.find(tmemopnd);
  if (it != hash_memopnd_tb_.end()) {
    return it->second;
  }
  X86MemOperand *res = memPool->New<X86MemOperand>(tmemopnd);
  hash_memopnd_tb_[tmemopnd] = res;
  return res;
}

}  // namespace maplebe
