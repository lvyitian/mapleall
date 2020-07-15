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
#include "arm_isa.h"
#include "arm_operand.h"
#include "arm_cg.h"
#include "cg_assert.h"
#include <iostream>

namespace maplebe {

using namespace maple;

void ArmCGFunc::GenerateProlog(BB *bb) {
  // generate push {LR}  // Can optimize by push LR when the function has a call
  BB *formal_curbb = curbb;
  dummybb->ClearInsns();
  curbb = dummybb;
  vector<Insn *> parmsinsnvec;
  ParmLocator parmlocator(becommon);
  Operand *sp_opnd = GetOrCreatePhysicalRegisterOperand(RSP, 32, kRegTyInt);
  if (fsp_) {
    GetOrCreateSavedOpnds()->push_opnd(fsp_);
  }
  if (func->HasCall()) {
    ArmRegOperand *lr_opnd = GetOrCreatePhysicalRegisterOperand(RLR, 32, kRegTyInt);
    GetOrCreateSavedOpnds()->push_opnd(lr_opnd);
  }
  if (upfsp_) {
    SelectCopy(upfsp_, sp_opnd, PTY_u32);
  }
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbpush, sp_opnd, GetOrCreateSavedOpnds()));
  Operand *immopnd = GetOrCreateImmOperand(static_cast<ArmMemLayout *>(memlayout)->StackFrameSize(), 32, true);
  SelectSub(sp_opnd, sp_opnd, immopnd, PTY_u32);
  if (fsp_) {
    SelectCopy(fsp_, sp_opnd, PTY_u32);
  }
  PLocInfo ploc;
  for (uint32 i = 0; i < func->formals.size(); i++) {
    MIRType *ty = globaltable.GetTypeFromTyIdx(func->argumentsTyidx[i]);
    parmlocator.LocateNextParm(ty, ploc);
    if (ploc.reg0 == 0) {
      continue;
    }
    MIRSymbol *sym = func->formals[i];
    int32 symsize = becommon.type_size_table[ty->_ty_idx.idx];
    RegType regtype = ploc.reg0 <= R15 ? kRegTyInt : kRegTyFloat;
    RegOperand *regopnd = GetOrCreatePhysicalRegisterOperand(ploc.reg0, RoundUp(std::min(8, symsize), 4) * 8, regtype);
    MemOperand *memopnd = GetOrCreateMemOpnd(sym, 0, std::min(8, symsize) * 8);
    MOperator mop =
      PickStInsn(std::min(8, symsize) * 8, regtype == kRegTyInt ? PTY_i32 : (symsize == 8 ? PTY_f64 : PTY_f32));
    parmsinsnvec.push_back(cg->BuildInstruction<ArmInsn>(mop, regopnd, memopnd));
    formal_reg_list_.push_back(ploc.reg0);
    if (symsize == 8) {
      formal_reg_list_.push_back((Armregister)(static_cast<uint32>(ploc.reg0) + 1));
    }
    if (ploc.reg1 == 0) {
      continue;
    }
    CG_ASSERT(false, "NYI");
  }
  for (vector<Insn *>::iterator it = parmsinsnvec.begin(); it != parmsinsnvec.end(); it++) {
    curbb->AppendInsn(*it);
  }

  bb->InsertAtBeginning(dummybb);
  curbb = formal_curbb;
  return;
}

void ArmCGFunc::GenerateEpilog(BB *bb) {
  BB *formal_curbb = curbb;
  dummybb->ClearInsns();
  curbb = dummybb;
  Operand *sp_opnd = GetOrCreatePhysicalRegisterOperand(RSP, 32, kRegTyInt);
  Operand *immopnd = GetOrCreateImmOperand(static_cast<ArmMemLayout *>(memlayout)->StackFrameSize(), 32, true);
  SelectAdd(sp_opnd, sp_opnd, immopnd, PTY_u32);
  if (saved_opnds_) {
    curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbpop, sp_opnd, saved_opnds_));
  }
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbbx, GetOrCreatePhysicalRegisterOperand(RLR, 32, kRegTyInt)));
  bb->AppendBBInsns(dummybb);
  curbb = formal_curbb;
}

Insn *ArmCGFunc::GenerateCfiRestores(BB *lastbb) {
  Insn *ipoint = nullptr;
  Insn *restore_rlr_inst = cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RLR, 64));
  if (lastbb->firstinsn) {
    ipoint = lastbb->InsertInsnAfter(lastbb->lastinsn, restore_rlr_inst);
  } else {
    ipoint = lastbb->firstinsn = lastbb->lastinsn = restore_rlr_inst;
  }
  return lastbb->InsertInsnAfter(
    ipoint,
    cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_def_cfa, CreateCfiRegOperand(RSP, 64), CreateCfiImmOperand(0, 64)));
}

Operand *ArmCGFunc::GetTargetRetOperand(PrimType ptype) {
  return GetOrCreatePhysicalRegisterOperand(IsPrimitiveFloat(ptype) ? S0 : R0, GetPrimTypeBitSize(ptype),
                                            GetRegTyFromPrimTy(ptype));
}

Operand *ArmCGFunc::CreateOpndOfReg(RegOperand *opnd) {
  CG_ASSERT(false, "NYI");
  return nullptr;
}

Operand *ArmCGFunc::CreateOpndOfType(PrimType primtype) {
  RegType regty = GetRegTyFromPrimTy(primtype);
  uint32 size = GetPrimTypeBitSize(primtype);
  regno_t v_reg_no = New_V_Reg(regty, size);
  return memPool->New<ArmRegOperand>(v_reg_no, size, regty);
}

Operand *ArmCGFunc::CreateRflagOpnd() {
  regno_t v_reg_no = New_V_Reg(RegTy_cc, 8);
  return memPool->New<ArmRegOperand>(v_reg_no, 32, RegTy_cc);
}

// create an immediate operand from primType
Operand *ArmCGFunc::CreateImmOperand(PrimType primType, int64 val) {
  return GetOrCreateImmOperand(val, GetPrimTypeBitSize(primType), IsSignedInteger(primType));
}

// create an float immediate operand.
Operand *ArmCGFunc::CreateFloatImmOperand(float val) {
  CG_ASSERT(false, "NYI");
  return nullptr;
}

// create an double immediate operand.
Operand *ArmCGFunc::CreateDoubleImmOperand(double val) {
  CG_ASSERT(false, "NYI");
  return nullptr;
}

// create an insn that zero operand
Operand *ArmCGFunc::CreateZeroOperand(PrimType primType) {
  CG_ASSERT(false, "NYI");
  return nullptr;
}

void ArmCGFunc::SelectParmList(StmtNode *narynode, ArmListOperand *srcopnds) {
  ParmLocator parmlocator(becommon);
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
        ArmRegOperand *parmregopnd =
          GetOrCreatePhysicalRegisterOperand(ploc.reg0, expregopnd->size_, GetRegTyFromPrimTy(ptype));
        SelectCopy(parmregopnd, expregopnd, ptype);
        srcopnds->push_opnd(parmregopnd);
      } else {  // store to actual memory segment
        Operand *actmemopnd = CreateMemOpnd(RSP, ploc.memoffset, GetPrimTypeSize(ptype) * 8);
        curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(PickStInsn(ptype), expregopnd, actmemopnd));
      }
      CG_ASSERT(ploc.reg1 == 0, "SelectCall NYI");
    } else {
      CG_ASSERT(false, "NYI");
    }
  }
}

void ArmCGFunc::SelectCall(CallNode *callnode) {
  ArmListOperand *src_opnds = memPool->New<ArmListOperand>(funcscope_allocator_);
  SelectParmList(callnode, src_opnds);
  MIRFunction *fn = globaltable.GetFunctionFromPuidx(callnode->puidx);
  MemOperand *targetopnd =
    GetOrCreateMemOpnd(g->mirModule->CurFunction()->GetLocalOrGlobalSymbol(fn->stidx, false), 0, 0);
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbbl, targetopnd, src_opnds));
  func->SetHasCall();
}

void ArmCGFunc::SelectIcall(IcallNode *icallnode, Operand *fptropnd) {
  ArmListOperand *src_opnds = memPool->New<ArmListOperand>(funcscope_allocator_);
  SelectParmList(icallnode, src_opnds);
  if (fptropnd->op_kind_ != Operand::Opd_Register) {
    fptropnd = SelectCopy(fptropnd, icallnode->Opnd(0)->primType);
  }
  RegOperand *regopnd = dynamic_cast<RegOperand *>(fptropnd);
  CG_ASSERT(regopnd, "SelectIcall: function pointer not RegOperand");
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbblxr, regopnd, src_opnds));
  func->SetHasCall();
}

void ArmCGFunc::SelectReturn(NaryStmtNode *stmt, Operand *opnd0) {
  ReturnMechanism retmech(globaltable.GetTypeFromTyIdx(func->returnTyidx), becommon);
  RegOperand *retopnd = nullptr;
  if (retmech.regcount > 0) {
    if (RegOperand *regopnd = dynamic_cast<RegOperand *>(opnd0)) {
      if (regopnd->GetRegisterNumber() != retmech.reg0) {
        retopnd = GetOrCreatePhysicalRegisterOperand(retmech.reg0, regopnd->size_, GetRegTyFromPrimTy(retmech.ptype0));
        SelectCopy(retopnd, regopnd, retmech.ptype0);
      }
    } else if (ArmMemOperand *memopnd = dynamic_cast<ArmMemOperand *>(opnd0)) {
      retopnd = GetOrCreatePhysicalRegisterOperand(retmech.reg0, GetPrimTypeSize(retmech.ptype0) * 8,
                                                   GetRegTyFromPrimTy(retmech.ptype0));
      MOperator mop = PickLdInsn(memopnd->size_, retmech.ptype0, false);
      curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(mop, retopnd, memopnd));
    } else if (ImmOperand *immopnd = dynamic_cast<ImmOperand *>(opnd0)) {
      retopnd = GetOrCreatePhysicalRegisterOperand(retmech.reg0, GetPrimTypeSize(retmech.ptype0) * 8, kRegTyInt);
      SelectCopy(retopnd, immopnd, retmech.ptype0);
    } else {
      CG_ASSERT(false, "nyi");
    }
  } else if (opnd0 != nullptr) {  // pass in memory
    CG_ASSERT(false, "SelectReturn: return in memory NYI");
    if (retmech.fake_first_parm) {
    }
  }
  exitbbsvec.push_back(curbb);
}

ArmRegOperand *ArmCGFunc::GetOrCreatePhysicalRegisterOperand(regno_t reg_no, uint8 size, RegType kind, uint32 flag) {
  ArmRegOperand tregopnd(reg_no, size, kind, flag);
  auto it = hash_regopnd_tb_.find(tregopnd);
  if (it != hash_regopnd_tb_.end()) {
    return it->second;
  }
  ArmRegOperand *res = memPool->New<ArmRegOperand>(reg_no, size, kind, flag);
  hash_regopnd_tb_[tregopnd] = res;
  return res;
}

ImmOperand *ArmCGFunc::GetOrCreateImmOperand(int64 val, uint8 size, bool is_signed) {
  ArmImmOperand timmopnd(val, size, is_signed);
  auto it = hash_immopnd_tb_.find(timmopnd);
  if (it != hash_immopnd_tb_.end()) {
    return it->second;
  }
  ArmImmOperand *res = memPool->New<ArmImmOperand>(val, size, is_signed);
  hash_immopnd_tb_[timmopnd] = res;
  return res;
}

LabelOperand *ArmCGFunc::GetOrCreateLabelOperand(LabelIdx labidx) {
  MapleMap<LabelIdx, LabelOperand *>::iterator it = hash_lblopnd_tb_.find(labidx);
  if (it != hash_lblopnd_tb_.end()) {
    return it->second;
  }
  MIRSymbol *func_st = globaltable.GetSymbolFromStidx(func->stidx.Idx());
  LabelOperand *res = memPool->New<LabelOperand>(func_st->GetName().c_str(), labidx);
  hash_lblopnd_tb_[labidx] = res;
  return res;
}

ArmOfstOperand *ArmCGFunc::GetOrCreateOfstOperand(uint32 offset, uint32 size) {
  ArmOfstOperand tofstopnd(offset, size);
  auto it = hash_ofstopnd_tb_.find(tofstopnd);
  if (it != hash_ofstopnd_tb_.end()) {
    return it->second;
  }
  ArmOfstOperand *res = memPool->New<ArmOfstOperand>(offset, size);
  hash_ofstopnd_tb_[tofstopnd] = res;
  return res;
}

StImmOperand *ArmCGFunc::GetOrCreateStImmOperand(MIRSymbol *st, int64 offset, int32 relocs) {
  StImmOperand tstimmopnd(st, offset, relocs);
  MapleMap<StImmOperand, StImmOperand *>::iterator it = hash_stimmopnd_tb_.find(tstimmopnd);
  if (it != hash_stimmopnd_tb_.end()) {
    return it->second;
  }
  StImmOperand *res = memPool->New<StImmOperand>(st, offset, relocs);
  hash_stimmopnd_tb_[tstimmopnd] = res;
  return res;
}

MemOperand *ArmCGFunc::GetOrCreateMemOpnd(MIRSymbol *symbol, int32 offset, int32 size) {
  MIRStorageClass storageClass = symbol->storageClass;
  if (storageClass == kScAuto || storageClass == kScFormal) {
    SymbolAlloc symalloc = *memlayout->sym_alloc_table[symbol->GetStIndex()];
    ArmSymbolAlloc *psymalloc = static_cast<ArmSymbolAlloc *>(&symalloc);
    RegOperand *base_opnd = static_cast<RegOperand *>(GetBaseReg(psymalloc));
    int32 stoffset = GetBaseOffset(psymalloc);
    int32 total_offset = stoffset + offset;
    if (total_offset < -255 || total_offset > 1020) {
      RegOperand *resimmopnd = static_cast<RegOperand *>(CreateOpndOfType(PTY_i32));
      SelectAdd(resimmopnd, base_opnd, GetOrCreateImmOperand(total_offset, 32, true), PTY_i32);
      return memPool->New<ArmMemOperand>(ArmMemOperand::Addressing_BO, size, resimmopnd, nullptr,
                                     GetOrCreateImmOperand(0, 32, false), nullptr, symbol);
    }
    OfstOperand *offset_opnd = GetOrCreateOfstOperand(stoffset + offset, 32);
    ArmMemOperand tmemopnd(ArmMemOperand::Addressing_BO, size, base_opnd, nullptr, offset_opnd, nullptr, symbol);
    auto it = hash_memopnd_tb_.find(tmemopnd);
    if (it != hash_memopnd_tb_.end()) {
      return it->second;
    }
    ArmMemOperand *res = memPool->New<ArmMemOperand>(tmemopnd);
    hash_memopnd_tb_[tmemopnd] = res;
    return res;
  } else if (storageClass == kScGlobal || storageClass == kScExtern) {
    Operand *offset_opnd = GetOrCreateStImmOperand(symbol, 0, 0);
    ArmMemOperand tmemopnd(ArmMemOperand::Addressing_O, size, nullptr, nullptr, offset_opnd, nullptr, symbol);
    auto it = hash_memopnd_tb_.find(tmemopnd);
    ArmMemOperand *stmemopnd = nullptr;
    if (it != hash_memopnd_tb_.end()) {
      stmemopnd = it->second;
    } else {
      stmemopnd = memPool->New<ArmMemOperand>(tmemopnd);
      hash_memopnd_tb_[tmemopnd] = stmemopnd;
    }
    RegOperand *staddropnd = static_cast<RegOperand *>(CreateOpndOfType(PTY_u32));
    SelectAddrof(staddropnd, stmemopnd);
    return memPool->New<ArmMemOperand>(ArmMemOperand::Addressing_BO, size, staddropnd, nullptr,
                                   GetOrCreateOfstOperand(offset, 32), nullptr, symbol);
  } else if (storageClass == kScPstatic || storageClass == kScFstatic) {
    Operand *offset_opnd = GetOrCreateStImmOperand(symbol, offset, 0);
    ArmMemOperand tmemopnd(ArmMemOperand::Addressing_O, size, nullptr, nullptr, offset_opnd, nullptr, symbol);
    auto it = hash_memopnd_tb_.find(tmemopnd);
    if (it != hash_memopnd_tb_.end()) {
      return it->second;
    }
    ArmMemOperand *res = memPool->New<ArmMemOperand>(tmemopnd);
    hash_memopnd_tb_[tmemopnd] = res;
    return res;
  } else if (storageClass == kScText) {
    ArmMemOperand tmemopnd(ArmMemOperand::Addressing_TEXT, size, nullptr, nullptr, nullptr, nullptr, symbol);
    auto it = hash_memopnd_tb_.find(tmemopnd);
    if (it != hash_memopnd_tb_.end()) {
      return it->second;
    }
    ArmMemOperand *res = memPool->New<ArmMemOperand>(tmemopnd);
    hash_memopnd_tb_[tmemopnd] = res;
    return res;
  } else {
    CG_ASSERT(false, "NYI");
  }
  return nullptr;
}

ArmMemOperand *ArmCGFunc::GetOrCreateMemOpnd(ArmMemOperand::ArmAdrMode mode, int32 size, RegOperand *base,
                                             RegOperand *index, Operand *offset, Operand *scale, MIRSymbol *st) {
  ArmMemOperand tmemopnd(mode, size, base, index, offset, scale, st);
  auto it = hash_memopnd_tb_.find(tmemopnd);
  if (it != hash_memopnd_tb_.end()) {
    return it->second;
  }
  ArmMemOperand *res = memPool->New<ArmMemOperand>(tmemopnd);
  hash_memopnd_tb_[tmemopnd] = res;
  return res;
}

MemOperand *ArmCGFunc::CreateMemOpnd(RegOperand *base_opnd, int32 offset, int32 size) {
  OfstOperand *offset_opnd = GetOrCreateOfstOperand(offset, 32);
  if (!ImmOperand::IsInBitSizeRot(8, offset)) {
    Operand *resimmopnd = SelectCopy(GetOrCreateImmOperand(offset, 32, true), PTY_i32);
    return memPool->New<ArmMemOperand>(ArmMemOperand::Addressing_BO, size, base_opnd, nullptr, resimmopnd, nullptr,
                                   nullptr);
  }
  ArmMemOperand tmemopnd(ArmMemOperand::Addressing_BO, size, base_opnd, nullptr, offset_opnd, nullptr, nullptr);
  auto it = hash_memopnd_tb_.find(tmemopnd);
  if (it != hash_memopnd_tb_.end()) {
    return it->second;
  }
  ArmMemOperand *res = memPool->New<ArmMemOperand>(tmemopnd);
  hash_memopnd_tb_[tmemopnd] = res;
  return res;
}

Operand *ArmCGFunc::GetOrCreateRflag() {
  if (rcc_ == nullptr) {
    rcc_ = CreateRflagOpnd();
  }
  return rcc_;
}

Operand *ArmCGFunc::GetHigh32bitsOpnd(Operand *opnd) {
  ArmRegOperand *regopnd = dynamic_cast<ArmRegOperand *>(opnd);
  CG_ASSERT(regopnd->size_ == 64, "only 64 bits register can do that");
  return GetOrCreatePhysicalRegisterOperand(regopnd->GetRegisterNumber(), 32, regopnd->GetRegisterType(),
                                            regopnd->flag_ | REGOPNDSETHIGH32);
}

Operand *ArmCGFunc::GetLow32bitsOpnd(Operand *opnd) {
  ArmRegOperand *regopnd = dynamic_cast<ArmRegOperand *>(opnd);
  CG_ASSERT(regopnd->size_ == 64, "only 64 bits register can do that");
  return GetOrCreatePhysicalRegisterOperand(regopnd->GetRegisterNumber(), 32, regopnd->GetRegisterType(),
                                            regopnd->flag_ | REGOPNDSETLOW32);
}

// the first operand in opndvec is return opnd
void ArmCGFunc::SelectLibCall(const char *name, vector<Operand *> &opndvec, PrimType primtype, PrimType retprmtype,
                              bool is2ndret) {
  MIRModule &mirModule = *g->mirModule;
  MIRSymbol *st = globaltable.CreateSymbol(SCOPE_GLOBAL);
  std::string funcname(name);
  st->SetNameStridx(globaltable.GetOrCreateGstridxFromName(funcname));
  st->storageClass = kScText;
  st->sKind = kStFunc;
  // setup the type of the callee function
  MapleVector<TyIdx> vec(mirModule.memPoolAllocator.Adapter());
  MapleVector<TypeAttrs> vec_at(mirModule.memPoolAllocator.Adapter());
  for (uint32 i = 1; i < opndvec.size(); i++) {
    vec.push_back(globaltable.typeTable[static_cast<int>(primtype)]->_ty_idx);
    vec_at.push_back(TypeAttrs());
  }
  MIRType *rettype = globaltable.typeTable[static_cast<int>(primtype)];
  MIRFuncType functype(rettype->_ty_idx, vec, vec_at);
  st->SetTyIdx(globaltable.GetOrCreateMIRType(&functype));

  ParmLocator parmlocator(becommon);
  PLocInfo ploc;
  CG_ASSERT(primtype != PTY_void, "");
  // setup actual parameters
  ArmListOperand *src_opnds = memPool->New<ArmListOperand>(funcscope_allocator_);
  for (uint32 i = 1; i < opndvec.size(); i++) {
    MIRType *ty;
    ty = globaltable.typeTable[static_cast<uint32>(primtype)];
    Operand *st_opnd = opndvec[i];
    if (st_opnd->op_kind_ != Operand::Opd_Register) {
      st_opnd = SelectCopy(st_opnd, primtype);
    }
    RegOperand *expregopnd = static_cast<RegOperand *>(st_opnd);
    parmlocator.LocateNextParm(ty, ploc);
    if (ploc.reg0 != 0) {  // load to the register
      ArmRegOperand *parmregopnd =
        GetOrCreatePhysicalRegisterOperand(ploc.reg0, expregopnd->size_, GetRegTyFromPrimTy(primtype));
      SelectCopy(parmregopnd, expregopnd, primtype);
      src_opnds->push_opnd(parmregopnd);
    }
    CG_ASSERT(ploc.reg1 == 0, "SelectCall NYI");
  }
  MemOperand *targetopnd =
    GetOrCreateMemOpnd(mirModule.CurFunction()->GetLocalOrGlobalSymbol(st->GetStIdx(), false), 0, 0);
  curbb->AppendInsn(cg->BuildInstruction<ArmInsn>(MOP_Tbbl, targetopnd, src_opnds));
  func->SetHasCall();
  // get return value
  Operand *opnd0 = opndvec[0];
  ReturnMechanism retmech(globaltable.typeTable[retprmtype], becommon);
  RegOperand *retopnd = nullptr;
  if (retmech.regcount > 0) {
    if (RegOperand *regopnd = dynamic_cast<RegOperand *>(opnd0)) {
      uint32 regnum = is2ndret ? retmech.reg1 : retmech.reg0;
      if (regopnd->GetRegisterNumber() != regnum) {
        retopnd = GetOrCreatePhysicalRegisterOperand(regnum, regopnd->size_, GetRegTyFromPrimTy(retprmtype));
        SelectCopy(opnd0, retopnd, retprmtype);
      }
    } else {
      CG_ASSERT(false, "nyi");
    }
  } else {
    CG_ASSERT(false, "should return from register");
  }
}

Operand *ArmCGFunc::GetBaseReg(ArmSymbolAlloc *symballoc) {
  MemSegmentKind sgkind = symballoc->mem_segment->kind;
  if (sgkind == MS_args_regpassed || sgkind == MS_locals || sgkind == MS_args_to_stkpass) {
    if (!fsp_) {
      fsp_ = GetOrCreatePhysicalRegisterOperand(R7, 32, kRegTyInt);
    }
    return fsp_;
  } else if (sgkind == MS_args_stkpassed) {
    if (!upfsp_) {
      upfsp_ = CreateOpndOfType(PTY_u32);
    }
    return upfsp_;
  } else {
    CG_ASSERT(false, "");
  }
  return nullptr;
}

uint32 ArmCGFunc::GetBaseOffset(ArmSymbolAlloc *symalloc) {
  /* stack layout of arm
     up_formal
     some registers pushed
     formals
     local variables
     actual formals
   */
  MemSegmentKind sgkind = symalloc->mem_segment->kind;
  ArmMemLayout *memlayout = static_cast<ArmMemLayout *>(this->memlayout);
  if (sgkind == MS_args_stkpassed) {
    return symalloc->offset;
  } else if (sgkind == MS_args_regpassed) {
    return memlayout->seg__args_to_stkpass.size + memlayout->seg_locals.size + symalloc->offset;
  } else if (sgkind == MS_locals) {
    return memlayout->seg__args_to_stkpass.size + symalloc->offset;
  } else if (sgkind == MS_args_to_stkpass) {
    return symalloc->offset;
  } else {
    CG_ASSERT(false, "");
  }
  return 0;
}

ArmListOperand *ArmCGFunc::GetOrCreateSavedOpnds() {
  saved_opnds_ = saved_opnds_ ? saved_opnds_ : memPool->New<ArmListOperand>(funcscope_allocator_);
  return saved_opnds_;
}

}  // namespace maplebe
