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

#include <vector>
#include <cstdint>

#include "riscv64_cg_func.h"
#include "riscv64_cg.h"
#include "riscv64_rt_support.h"
#include "opcode_info.h"  // mapleir/include/opcode_info.h
#include "cg_assert.h"
#include "mir_builder.h"
#include "mpl_atomic.h"

#include "mpl_logging.h"

namespace maplebe {

using namespace maple;

void Riscv64CGFunc::SelectMPLClinitCheck(IntrinsiccallNode *intrnnode) {
  CG_ASSERT(intrnnode->NumOpnds() == 1, "must be 1 operand");
  BaseNode *arg = intrnnode->Opnd(0);
  Operand *stopnd = nullptr;
  bool bClinitSeperate = false;
  CG_ASSERT(CGOptions::doPIC, "must be doPIC");
  if (arg->op == OP_addrof) {
    AddrofNode *addrof = static_cast<AddrofNode *>(arg);
    MIRSymbol *symbol = func->GetLocalOrGlobalSymbol(addrof->stIdx);
    CG_ASSERT(symbol->GetName().find(CLASSINFO_PREFIX_STR) == 0, "must be a symbol with __classinfo__");

    if (!symbol->IsMuidDataUndefTab()) {
      std::string ptrName = NameMangler::kPtrPrefixStr + symbol->GetName();
      MIRType *ptrType = GlobalTables::GetTypeTable().GetPtr();
      symbol = mirModule.mirBuilder->GetOrCreateGlobalDecl(ptrName, ptrType);
      bClinitSeperate = true;
      symbol->storageClass = kScFstatic;
    }
    stopnd = CreateStImmOperand(symbol, 0, 0);
  } else {
    arg = arg->Opnd(0);
    BaseNode *arg0 = arg->Opnd(0);
    BaseNode *arg1 = arg->Opnd(1);
    CG_ASSERT(arg0->op == OP_addrof, "expect the operand to be addrof");
    AddrofNode *addrof = static_cast<AddrofNode *>(arg0);
    CHECK_FATAL(addrof, "static_cast failed");
    MIRSymbol *symbol = func->GetLocalOrGlobalSymbol(addrof->stIdx);
    CG_ASSERT(addrof->fieldID == 0, "For debug SelectCGArrayElemAdd.");
    // CG_ASSERT(symbol->GetName().find (CLASSINFO_PREFIX_STR) == 0, "must be a symbol with __classinfo__");
    ConstvalNode *constvalnode = static_cast<ConstvalNode *>(arg1);
    CHECK_FATAL(constvalnode, "static_cast failed");
    MIRConst *mirconst = constvalnode->constVal;
    MIRIntConst *mirintconst = dynamic_cast<MIRIntConst *>(mirconst);
    CHECK_FATAL(mirintconst, "dynamic_cast failed");
    stopnd = CreateStImmOperand(symbol, mirintconst->value, 0);
  }

  //{
  //  Riscv64ListOperand* src_opnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
  //  //src_opnds->push_opnd(stopnd);
  //  MIRSymbol *st = globaltable.CreateSymbol(kScopeGlobal);
  //  std::string funcname("__mpl_pre_clinit");
  //  st->SetNameStridx(globaltable.GetOrCreateGstridxFromName(funcname));
  //  st->storageClass = kScText;
  //  st->sKind = kStFunc;
  //  Operand *targetopnd = GetOrCreateFuncNameOpnd(func->GetLocalOrGlobalSymbol(st->GetStIdx(), false));
  //  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xbl, targetopnd, src_opnds));
  //}

  regno_t vreg2no = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
  RegOperand *vreg2 = CreateVirtualRegisterOperand(vreg2no);
  vreg2->SetRegNotBBLocal();
  if (bClinitSeperate) {
    // Seperate MOP_clinit to MOP_adrp_ldr + MOP_clinit_tail.
    Insn *newinsn = cg->BuildInstruction<Riscv64Insn>(MOP_adrp_ldr, vreg2, stopnd);
    curbb->AppendInsn(newinsn);
    newinsn->do_not_remove = true;

    newinsn = cg->BuildInstruction<Riscv64Insn>(MOP_clinit_tail, vreg2);
    newinsn->do_not_remove = true;
    curbb->AppendInsn(newinsn);
  } else {
    Insn *newinsn = cg->BuildInstruction<Riscv64Insn>(MOP_clinit, vreg2, stopnd);
    curbb->AppendInsn(newinsn);
  }

  //{
  //  Riscv64ListOperand* src_opnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
  //  //src_opnds->push_opnd(stopnd);
  //  MIRSymbol *st = globaltable.CreateSymbol(kScopeGlobal);
  //  std::string funcname("__mpl_post_clinit");
  //  st->SetNameStridx(globaltable.GetOrCreateGstridxFromName(funcname));
  //  st->storageClass = kScText;
  //  st->sKind = kStFunc;
  //  Operand *targetopnd = GetOrCreateFuncNameOpnd(func->GetLocalOrGlobalSymbol(st->GetStIdx(), false));
  //  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xbl, targetopnd, src_opnds));
  //}
}


void Riscv64CGFunc::SelectCVaStart(IntrinsiccallNode *intrnnode) {
  CG_ASSERT(intrnnode->NumOpnds() == 2, "must be 2 operands");
  // 2 operands, but only 1 needed. Don't need to emit code for second operand

  RegOperand *opnd0;                // first argument of intrinisc
  BaseNode *argexpr = intrnnode->Opnd(0);
  AddrofNode *ad = static_cast<AddrofNode *>(argexpr);
  MIRSymbol *sym = mirModule.CurFunction()->GetLocalOrGlobalSymbol(ad->stIdx, false);
  int32 offs = 0;
  Riscv64ImmOperand *offsOpnd;
  Riscv64ImmOperand *offsOpnd2;
  RegOperand *tmpreg;            // offs value to be assigned (rhs)
  MemOperand *strOpnd;           // mem operand in va_list struct (lhs)
  regno_t vregno = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
  RegOperand *vreg = CreateVirtualRegisterOperand(vregno);
  Operand *stkOpnd;
  Insn *insn;
  Riscv64MemLayout *memlayout = static_cast<Riscv64MemLayout *>(this->memlayout);
  int32 grAreaSize = memlayout->GetSizeOfGRSavearea();
  bool isSym = sym && (argexpr->op == OP_addrof);

  // if first argument of va_start is not a symbol, load its value
  if (!isSym) {
    Operand *opnd = HandleExpr(intrnnode, argexpr);
    opnd0 = LoadIntoRegister(opnd, PTY_a64);
  }

  // FPLR only pushed in regalloc() after intrin function
  if (UseFP() || UsedStpSubPairForCallFrameAllocation()) {
    stkOpnd = GetOrCreatePhysicalRegisterOperand(RFP, 64, kRegTyInt);
  } else {
    stkOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
  }

  // Find beginning of unnamed arg on stack.
  // Maybe in the save area or on stack
  // Ex. void foo(int i1, int i2, ... int i8, S s, int x);
  //     where struct S has size 32, s are on stack but they are named, to
  //     reach the unnamed x, we need to skip over the named ones on the stack
  ParmLocator parmlocator(becommon);
  PLocInfo ploc;
  uint32 stksize = 0;
  for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->formalDefVec[i].formalTyIdx);
    uint32_t ptyIdx = ty->tyIdx.GetIdx();
    parmlocator.LocateNextParm(ty, ploc);
    if (ploc.reg0 == kRinvalid) {  // on stack
      stksize = ploc.memoffset + ploc.memsize;
    }
  }
  stksize = RoundUp(stksize, SIZEOFPTR);

  offsOpnd = CreateImmOperand(0, 64, true, true); // isvary reset StackFrameSize
  offsOpnd2 = CreateImmOperand(grAreaSize, 64, false);
  SelectSub(vreg, offsOpnd, offsOpnd2, PTY_a64);  // if 1st opnd is register => sub
  SelectAdd(vreg, stkOpnd, vreg, PTY_a64);
  if (stksize) {
    offsOpnd = CreateImmOperand(stksize, 64, false);
    SelectAdd(vreg, offsOpnd, vreg, PTY_a64);
  }
  if (!isSym) {
    Riscv64OfstOperand *offopnd = GetOrCreateOfstOpnd(0, 64);
    strOpnd = GetOrCreateMemOpnd(64, opnd0, nullptr, offopnd, static_cast<MIRSymbol *>(nullptr));
  } else {
    strOpnd = GetOrCreateMemOpnd(sym, 0, 64);
  }
  insn = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, vreg, strOpnd);
  curbb->AppendInsn(insn);

  return;
}

Operand *Riscv64CGFunc::SelectCclz(IntrinsicopNode *intrnnode) {
  vector<Operand *> opndvec;

  CG_ASSERT(intrnnode->NumOpnds() == 1, "must be 1 operand");
  BaseNode *argexpr = intrnnode->Opnd(0);
  PrimType ptype = argexpr->primType;
  Operand *opnd = HandleExpr(intrnnode, argexpr);

  RegOperand *ret = CreateRegisterOperandOfType(ptype);
  opndvec.push_back(ret); // ret val
  opndvec.push_back(opnd); // param 0
  SelectLibCall("__clzdi2", opndvec, ptype, ptype);

  if (GetPrimTypeSize(ptype) == 4) {
    RegOperand *dst = CreateRegisterOperandOfType(ptype);
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, dst, ret, CreateImmOperand(-32, 8, true)));
    return dst;
  }
  return ret;
}

Operand *Riscv64CGFunc::SelectCctz(IntrinsicopNode *intrnnode) {
  vector<Operand *> opndvec;

  CG_ASSERT(intrnnode->NumOpnds() == 1, "must be 1 operand");
  BaseNode *argexpr = intrnnode->Opnd(0);
  PrimType ptype = argexpr->primType;
  Operand *opnd = HandleExpr(intrnnode, argexpr);

  RegOperand *ret = CreateRegisterOperandOfType(ptype);
  opndvec.push_back(ret); // ret val
  opndvec.push_back(opnd); // param 0
  SelectLibCall("__ctzdi2", opndvec, ptype, ptype);

  return ret;
}

Operand *Riscv64CGFunc::SelectIntrinOp(IntrinsicopNode *intrinsicopnode) {
  MIRIntrinsicID intrinsic = intrinsicopnode->intrinsic;
  if (intrinsic == INTRN_C_clz32 || intrinsic == INTRN_C_clz64) {
    return SelectCclz(intrinsicopnode);
  }
  if (intrinsic == INTRN_C_ctz32 || intrinsic == INTRN_C_ctz64) {
    return SelectCctz(intrinsicopnode);
  }
  CHECK_FATAL(0, "NYI");
}

void Riscv64CGFunc::SelectIntrinCall(IntrinsiccallNode *intrinsiccallnode) {
  MIRIntrinsicID intrinsic = intrinsiccallnode->intrinsic;

  if (cg->GenerateVerboseAsm()) {
    string *comment = new string(GetIntrinsicName(intrinsic));
    const char *str = comment->c_str();
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand(str)));
  }

  // At this moment, we eagerly evaluates all argument expressions.  In theory,
  // there could be intrinsics that extract meta-information of variables, such
  // as their locations, rather than computing their values.  Applications
  // include building stack maps that help runtime libraries to find the values
  // of local variables (See @stackmap in LLVM), in which case knowing their
  // locations will suffice.

  if (intrinsic == INTRN_MPL_CLINIT_CHECK) {  // special case
    SelectMPLClinitCheck(intrinsiccallnode);
    return;
  }
  if (intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS || intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS_SKIP ||
      intrinsic == INTRN_MPL_STACK) {
    return;
  }
  if (intrinsic == INTRN_C_va_start) {
    SelectCVaStart(intrinsiccallnode);
    return;
  }
  std::vector<Operand *> operands;  // Temporary.  Deallocated on return.
  for (int32_t i = 0; i < intrinsiccallnode->NumOpnds(); i++) {
    BaseNode *argexpr = intrinsiccallnode->Opnd(i);
    Operand *opnd = HandleExpr(intrinsiccallnode, argexpr);
    operands.push_back(opnd);
  }

  CallReturnVector *retvals = nullptr;

  auto icanode = dynamic_cast<IntrinsiccallNode *>(intrinsiccallnode);
  if (icanode) {
    retvals = &icanode->returnValues;
  }

  switch (intrinsic) {
    case INTRN_MPL_ATOMIC_EXCHANGE_PTR: {
      CG_ASSERT(curbb->GetKind() == BB::kBBCall, "");
      BB *origFtBb = curbb->next;
      Operand *loc = operands[0];
      Operand *newVal = operands[1];
      Operand *memOrd = operands[2];

      MemOrd ord = OperandToMemOrd(memOrd);
      bool isAcquire = MemOrdIsAcquire(ord);
      bool isRelease = MemOrdIsRelease(ord);

      const PrimType kValPty = PTY_a64;

      RegOperand *locReg = LoadIntoRegister(loc, PTY_a64);
      // Because there is no live analysis when -O1
      if (g->optim_level < 2) {
        locReg->SetRegNotBBLocal();
      }
      Riscv64MemOperand *locMem =
        GetOrCreateMemOpnd(64, locReg, nullptr, GetOrCreateOfstOpnd(0, 32), nullptr);

      RegOperand *newValReg = LoadIntoRegister(newVal, PTY_a64);
      if (g->optim_level < 2) {
        newValReg->SetRegNotBBLocal();
      }
      curbb->SetKind(BB::kBBFallthru);

      LabelIdx retryLabidx = CreateLabeledBB(intrinsiccallnode);

      RegOperand *oldVal = SelectLoadExcl(kValPty, locMem, isAcquire);
      if (g->optim_level < 2) {
        oldVal->SetRegNotBBLocal();
      }
      RegOperand *succ = SelectStoreExcl(kValPty, locMem, newValReg, isRelease);
      if (g->optim_level < 2) {
        succ->SetRegNotBBLocal();
      }

      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_bnez, succ, GetOrCreateLabelOperand(retryLabidx)));
      curbb->SetKind(BB::kBBIntrinsic);
      curbb->next = origFtBb;

      SaveReturnValueInLocal(retvals, 0, kValPty, oldVal, intrinsiccallnode);
      break;
    }
    default:
      CHECK_FATAL(false, "Intrinsic %d: %s not implemented by the Riscv64 CG.", intrinsic, GetIntrinsicName(intrinsic));
      break;
  }
}

/* NOTE: consider moving the following things into riscv64_cg.cpp  They may
 * serve not only inrinsics, but other MapleIR instructions as well.

 * Do it as if we are adding a label in straight-line assembly code.
 */
LabelIdx Riscv64CGFunc::CreateLabeledBB(StmtNode *stmt) {
  LabelIdx labidx = CreateLabel();
  BB *newbb = StartNewBBImpl<false>(stmt);
  newbb->AddLabel(labidx);
  CG_ASSERT(newbb, "");
  lab2bbmap[labidx] = newbb;
  curbb = newbb;
  return labidx;
}

// Save value into the local variable for the index-th return value;
void Riscv64CGFunc::SaveReturnValueInLocal(CallReturnVector *retvals, size_t index, PrimType pty, Operand *value,
                                           StmtNode *parentStmt) {
  CHECK_FATAL(retvals, "The current intrinsic call does not have return values");
  CallReturnPair &pair = (*retvals).at(index);
  BB tempBb((uint32_t)-1, funcscope_allocator_);
  BB *realCurbb = curbb;
  curbb = &tempBb;
  if (!pair.second.IsReg()) {
    SelectDassign(pair.first, pair.second.GetFieldid(), pty, value);
  } else {
    CHECK_FATAL(false, "NYI");
  }
  if (!realCurbb->next) {
    realCurbb->laststmt = parentStmt;
    realCurbb->next = StartNewBBImpl<true>(parentStmt);
    realCurbb->next->SetKind(BB::kBBFallthru);
    realCurbb->next->prev = realCurbb;
  } else {
    CG_ASSERT(0, "");
  }
  realCurbb->next->InsertAtBeginning(curbb);
  // restore it
  curbb = realCurbb->next;
}

//// The following are translation of LL/SC and atomic RMW operations

MemOrd Riscv64CGFunc::OperandToMemOrd(Operand *opnd) {
  auto immOpnd = dynamic_cast<ImmOperand *>(opnd);
  CHECK_FATAL(immOpnd, "Memory order must be an int constant.");
  uint32_t val = immOpnd->GetValue();
  return MemOrdFromU32(val);
}

// Generate ldxr or ldaxr instruction.
//
// byte_p2x: power-of-2 size of operand in bytes (0: 1B, 1: 2B, 2: 4B, 3: 8B).
MOperator Riscv64CGFunc::PickLoadStoreExclInsn(int byteP2size, bool store, bool acqRel) {
  CHECK_FATAL(0 <= byteP2size && byteP2size < 4, "Illegal argument p2size: %d", byteP2size);

  // Riscv64 has only word and double word atomic load/store
  CHECK_FATAL(byteP2size > 2, "Illegal size for atomic");
  static MOperator operators[4][2][2] = { { { MOP_undef, MOP_undef }, { MOP_undef, MOP_undef } },
                                          { { MOP_undef, MOP_undef }, { MOP_undef, MOP_undef } },
                                          { { MOP_wldxr, MOP_wldaxr }, { MOP_wstxr, MOP_wstlxr } },
                                          { { MOP_xldxr, MOP_xldaxr }, { MOP_xstxr, MOP_xstlxr } } };

  MOperator optr = operators[byteP2size][store][acqRel];

  CHECK_FATAL(optr != MOP_undef, "Unsupported type p2size: %d", byteP2size);

  return optr;
}

RegOperand *Riscv64CGFunc::SelectLoadExcl(PrimType valPty, Riscv64MemOperand *loc, bool acquire) {
  int p2size = GetPrimTypeP2Size(valPty);

  RegOperand *result = CreateRegisterOperandOfType(valPty);

  MOperator mop = PickLoadStoreExclInsn(p2size, false, acquire);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, result, loc));

  return result;
}

RegOperand *Riscv64CGFunc::SelectStoreExcl(PrimType valPty, Riscv64MemOperand *loc, RegOperand *newVal, bool release) {
  int p2size = GetPrimTypeP2Size(valPty);

  // the result (success/fail) is to be stored in a 32-bit register
  RegOperand *result = CreateRegisterOperandOfType(PTY_u32);

  MOperator mop = PickLoadStoreExclInsn(p2size, true, release);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, result, newVal, loc));

  return result;
}

}  // namespace maplebe
