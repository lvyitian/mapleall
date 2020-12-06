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
#include "cfi.h"
#include "cg_assert.h"
#include <iostream>
#include "reflection_analysis.h"
#include "special_func.h"
#include "riscv64_schedule.h"

#define SOE_CHCK_OFFSET 8192
namespace maplebe {

using namespace maple;
const int kFreqBase = 10000;
enum RegsPushPop {
  kRegsPushOp,
  kRegsPopOp,
};

#define PUSH_POP_SINGLE 0
#define PUSH_POP_PAIR 1
#define PUSH_POP_VECTOR 2

#define CLANG  (mirModule.IsCModule())

static MOperator pushPopOps[2][3][3] /*[2]*/ = { {
                                                   /* push */
                                                   { /*undef*/ 0 },
                                                   {
                                                     /*kRegTyInt*/
                                                     /* single */ MOP_xstr,
                                                     /* pair   */ MOP_undef,
                                                   },
                                                   {
                                                     /*kRegTyFloat*/
                                                     /* single */ MOP_dstr,
                                                     /* pair   */ MOP_undef,
                                                     /* vector */MOP_undef,
                                                   },
                                                 },
                                                 {
                                                   /* pop  */
                                                   { /*undef*/ 0 },
                                                   {
                                                     /*kRegTyInt*/
                                                     /* single */ MOP_xldr,
                                                     /* pair   */ MOP_undef,
                                                   },
                                                   {
                                                     /*kRegTyFloat*/
                                                     /* single */ MOP_dldr,
                                                     /* pair   */ MOP_undef,
                                                     /* vector */MOP_undef,
                                                   },
                                                 } };

static inline void AppendInstructionTo(Insn *i, CGFunc *f) {
  f->curbb->AppendInsn(i);
}

Operand *Riscv64CGFunc::GetZeroOpnd(uint32_t size) {
  return Riscv64RegOperand::GetZeroRegister(size <= 32 ? 32 : 64);
}

bool Riscv64CGFunc::IsFrameReg(RegOperand *opnd) {
  uint32_t fpno = UseFP() ? 30 : 32;
  if (opnd->GetRegisterNumber() == fpno) {
    return true;
  } else {
    return false;
  }
}

bool Riscv64CGFunc::NeedCleanup() {
  Riscv64MemLayout *layout = static_cast<Riscv64MemLayout *>(memlayout);
  if (layout->GetSizeOfRefLocals() > 0) {
    return true;
  }
  for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
    TypeAttrs tA = func->formalDefVec[i].formalAttrs;
    if (tA.GetAttr(ATTR_localrefvar)) {
      return true;
    }
  }
  // On-stack non-escaped objects always need cleanup
  if (hasNonescapedVar) {
    return true;
  }

  return false;
}

Schedule *Riscv64CGFunc::NewSchedule(CGFunc *cgfunc, MemPool* mp, LiveAnalysis *live, const char *phaseName) {
  return mp->New<Riscv64Schedule>(cgfunc, mp, live, phaseName);
}

void Riscv64CGFunc::HandleParamRCDec() {
  if (!cg->GenLocalRC()) {
    // handle local rc is disabled.
    return;
  }

  MIRSymbol *sym = nullptr;
  for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
    sym = func->formalDefVec[i].formalSym;
    TypeAttrs tA = func->formalDefVec[i].formalAttrs;
    if (!tA.GetAttr(ATTR_localrefvar)) {
      continue;
    }

    RegOperand *phyopnd = GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
    if (sym->IsPreg()) {
      PregIdx pregIdx = func->pregTab->GetPregIdxFromPregNo(sym->GetPreg()->pregNo);
      RegOperand *dstRegopnd = GetOrCreateVirtualRegisterOperand(GetVirtualRegNoFromPseudoRegIdx(pregIdx));
      int datasize = GetPrimTypeSize(sym->GetPreg()->primType) * BITS_PER_BYTE;
      MOperator mop = PickMovInsn(datasize, kRegTyInt);

      Insn *ins = cg->BuildInstruction<Riscv64Insn>(mop, phyopnd, dstRegopnd);
      curbb->AppendInsn(ins);

    } else {
      PrimType symty = sym->GetType()->primType;
      int datasize = GetPrimTypeSize(symty) * BITS_PER_BYTE;
      MemOperand *memopnd = GetOrCreateMemOpnd(sym, 0, datasize);
      CHECK_FATAL(static_cast<Riscv64MemOperand *>(memopnd),
             "static_cast<Riscv64MemOperand*>(memopnd) is null in Riscv64CGFunc::HandleParamRCDec");
      if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(memopnd), datasize)) {
        memopnd = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(memopnd), datasize);
      }

      Insn *ldInsn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(64, symty), phyopnd, memopnd);
      curbb->AppendInsn(ldInsn);
    }
    Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
    srcOpnds->PushOpnd(phyopnd);
    MIRSymbol *callsym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
    std::string funcname(GetIntrinsicFuncName(INTRN_MCCDecRef));
    callsym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));
    callsym->storageClass = kScText;
    callsym->sKind = kStFunc;

    Operand *targetopnd = GetOrCreateFuncNameOpnd(callsym);
    Insn *callInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xbl, targetopnd, srcOpnds);
    curbb->AppendInsn(callInsn);

    curbb->SetKind(BB::kBBCall);
    BB *newbb = CreateNewBB();
    curbb->AppendBB(newbb);
    // now all stmts has been handled
    newbb->frequency = curbb->frequency;
    curbb = newbb;
  }
}

// bb must be the cleanup bb.
// this function must be invoked before register allocation.
// extended epilogue is specific for fast exception handling and is made up of
// clean up code and epilogue.
// clean up code is generated here while epilogue is generated in GeneratePrologEpilog()
void Riscv64CGFunc::GenerateCleanupCodeForExtEpilog(BB *bb) {
  CG_ASSERT(lastbb->prev->firststmt == cleanup_label, "must be");

  if (NeedCleanup()) {
    // this is necessary for code insertion.
    curbb = bb;

    Riscv64RegOperand *regr0 =
      GetOrCreatePhysicalRegisterOperand(R0, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
    Riscv64RegOperand *regr1 =
      GetOrCreatePhysicalRegisterOperand(R1, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
    Riscv64MemOperand *frameAlloc = CreateCallFrameOperand(-16, SIZEOFPTR * BITS_PER_BYTE);
    Insn *allocInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, regr0, frameAlloc);
    allocInsn->do_not_remove = true;
    AppendInstructionTo(allocInsn, this);
    frameAlloc = CreateCallFrameOperand(-8, SIZEOFPTR * BITS_PER_BYTE);
    allocInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, regr1, frameAlloc);
    allocInsn->do_not_remove = true;
    AppendInstructionTo(allocInsn, this);

    // invoke MCC_CleanupLocalStackRef().
    HandleRCCall(false);
    // handle special ref param
    HandleParamRCDec();

    Riscv64MemOperand *frameDealloc = CreateCallFrameOperand(16, SIZEOFPTR * BITS_PER_BYTE);
    Insn *deallocInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xldr, regr0, frameDealloc);
    deallocInsn->do_not_remove = true;
    AppendInstructionTo(deallocInsn, this);
    frameDealloc = CreateCallFrameOperand(24, SIZEOFPTR * BITS_PER_BYTE);
    deallocInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xldr, regr1, frameDealloc);
    deallocInsn->do_not_remove = true;
    AppendInstructionTo(deallocInsn, this);
    // Update cleanupbb since bb may have been splitted
    cleanupbb = curbb;
  }
}

// bb must be the cleanup bb.
// this function must be invoked before register allocation.
void Riscv64CGFunc::GenerateCleanupCode(BB *bb) {
  CG_ASSERT(lastbb->prev->firststmt == cleanup_label, "must be");

  if (!NeedCleanup()) {
    return;
  }

  // this is necessary for code insertion.
  curbb = bb;

  // R0 is lived-in for clean-up code, save R0 before invocation
  Riscv64RegOperand *livein = GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
  RegOperand *backupRegop = nullptr;

  if (!cg->GenLocalRC()) {
    // by pass local RC operations.
  } else if (g->optim_level > 0) {
    regno_t vreg = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
    backupRegop = CreateVirtualRegisterOperand(vreg);
    backupRegop->SetRegNotBBLocal();
    SelectCopy(backupRegop, PTY_a64, livein, PTY_a64);

    // invoke MCC_CleanupLocalStackRef().
    HandleRCCall(false);
    // handle special ref param
    HandleParamRCDec();
    SelectCopy(livein, PTY_a64, backupRegop, PTY_a64);
  } else {
    // Register Allocation for O0 can not handle this case, so use a callee saved register directly.
    // If yieldpoint is enabled, we use R20 instead R19.
    Riscv64reg_t backupRegno = cg->GenYieldpoint() ? R20 : R19;
    backupRegop = GetOrCreatePhysicalRegisterOperand(backupRegno, 64, kRegTyInt);
    SelectCopy(backupRegop, PTY_a64, livein, PTY_a64);
    // invoke MCC_CleanupLocalStackRef().
    HandleRCCall(false);
    // handle special ref param
    HandleParamRCDec();
    SelectCopy(livein, PTY_a64, backupRegop, PTY_a64);
  }

  // invoke _Unwind_Resume
  std::string funcname("_Unwind_Resume");
  MIRSymbol *sym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  sym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));
  sym->storageClass = kScText;
  sym->sKind = kStFunc;
  Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
  srcOpnds->PushOpnd(livein);
  Operand *targetopnd = GetOrCreateFuncNameOpnd(sym);
  Insn *callInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xbl, targetopnd, srcOpnds);
  curbb->AppendInsn(callInsn);

  curbb->SetKind(BB::kBBCall);
  BB *newbb = CreateNewBB();
  if (firstbb->frequency != 0) {
    newbb->frequency = kFreqBase / 100;
  }
  curbb->AppendBB(newbb);
  curbb = newbb;

  // this instruction is unreachable, but we need it as the return address of previous
  // "bl _Unwind_Resume" for stack unwinding.
  Insn *nop = cg->BuildInstruction<Riscv64Insn>(MOP_xblr, livein, srcOpnds);
  curbb->AppendInsn(nop);

  curbb->SetKind(BB::kBBCall);
  newbb = CreateNewBB();
  if (firstbb->frequency != 0) {
    newbb->frequency = kFreqBase / 100;
  }
  curbb->AppendBB(newbb);
  curbb = newbb;
  // Update cleanupbb since bb may have been splitted
  cleanupbb = curbb;
}

void Riscv64CGFunc::AppendInstructionPushSingle(Riscv64reg_t reg, RegType rty, int offset) {
  MOperator mop = pushPopOps[kRegsPushOp][rty][PUSH_POP_SINGLE];
  Operand *o0 = GetOrCreatePhysicalRegisterOperand(reg, SIZEOFPTR * BITS_PER_BYTE, rty);
  Operand *o1 = CreateStkTopOpnd(offset, SIZEOFPTR * BITS_PER_BYTE);

  uint32 datasize = SIZEOFPTR * BITS_PER_BYTE;
  if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(o1), datasize)) {
    o1 = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(o1), datasize, R9);
  }

  Insn *pushInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1);
  string comment = "SAVE";
  pushInsn->AddComment(comment);
  AppendInstructionTo(pushInsn, this);

  // Append CFI code
  if (cg->GenerateCfiDirectives()) {
    int64 stackFrameSize = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize();
    if (cg->UseFP() || HasVLAOrAlloca()) {
      stackFrameSize -= memlayout->SizeOfArgsToStackpass();
    }
    int cfiOffset = stackFrameSize - offset;
    curbb->InsertInsnAfter(pushInsn,
                           cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset, CreateCfiRegOperand(reg, 64),
                                                              CreateCfiImmOperand(-cfiOffset, 64)));
  }
}

void Riscv64CGFunc::AppendInstructionPushRZRPair(Riscv64reg_t reg0, Riscv64reg_t reg1, int offset) {
  Operand *o0 = nullptr;
  Operand *o1 = nullptr;
  if (reg0 == RZERO) {
    o0 = GetZeroOpnd(64);
  } else {
    o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
  }

  if (reg1 == RZERO) {
    o1 = GetZeroOpnd(64);
  } else {
    o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
  }

  Operand *o2 = CreateStkTopOpnd(offset, SIZEOFPTR * BITS_PER_BYTE);

  uint32 datasize = SIZEOFPTR * BITS_PER_BYTE;
  CG_ASSERT(offset >= 0, "");
  if (offset > STP_LDP_IMM64_UPPER_BOUND) {
    o2 = SplitStpLdpOffsetForCalleeSavedWithAddInstruction(static_cast<Riscv64MemOperand *>(o2), datasize, Riscv64Abi::kIntSpareReg);
  }
  Insn *push_insn = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, o0, o2);
  AppendInstructionTo(push_insn, this);
  o2 = CreateStkTopOpnd(offset + 8, SIZEOFPTR * BITS_PER_BYTE);
  if (offset > STP_LDP_IMM64_UPPER_BOUND) {
    o2 = SplitStpLdpOffsetForCalleeSavedWithAddInstruction(static_cast<Riscv64MemOperand *>(o2), datasize, Riscv64Abi::kIntSpareReg);
  }
  push_insn = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, o1, o2);
  AppendInstructionTo(push_insn, this);

  // Append CFi code
  if (cg->GenerateCfiDirectives()) {
    int64 stackFrameSize = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize();
    if (cg->UseFP() || HasVLAOrAlloca()) {
      stackFrameSize -= memlayout->SizeOfArgsToStackpass();
    }
    int cfi_offset = stackFrameSize - offset;
    if (reg0 != RZERO) {
      push_insn = curbb->InsertInsnAfter(
        push_insn, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset, CreateCfiRegOperand(reg0, 64),
                                                      CreateCfiImmOperand(-cfi_offset, 64)));
    }
    if (reg1 != RZERO) {
      curbb->InsertInsnAfter(push_insn,
                             cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset, CreateCfiRegOperand(reg1, 64),
                                                                CreateCfiImmOperand(-cfi_offset + 8, 64)));
    }
  }
}

// if offset < 0, allocation; otherwise, deallocation
Riscv64MemOperand *Riscv64CGFunc::CreateCallFrameOperand(int32 offset, int32 size) {
  CHECK_FATAL(0, "NYI");
}

void Riscv64CGFunc::AppendInstructionAllocateCallFrame(Riscv64reg_t reg0, Riscv64reg_t reg1, RegType rty) {
  if (cg->GenerateVerboseAsm()) {
    curbb->AppendInsn(
      cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand("allocate activation frame")));
  }

  Insn *ipoint = nullptr;
  // stack_frame_size includes the size of args to stack-pass
  // if a function has neither VLA nor alloca.
  int64 stackFrameSize = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize();
  int64 argsToStkpassSize = memlayout->SizeOfArgsToStackpass();
  // ldp/stp's imm should be within -512 and 504;
  // if stp's imm > 512, we fall back to the stp-sub version
  bool useStpSub = false;
  int64 offset = 0;
  int cfiOffset = 0;
  if (!HasVLAOrAlloca() && argsToStkpassSize > 0) {
    // stack_frame_size == size of formal parameters + callee-saved (including FP/RL)
    //                     + size of local vars
    //                     + size of actuals
    // (when passing more than 8 args, its caller's responsibility to
    //  allocate space for it. size of actuals represent largest such size in the function.
    Operand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
    Operand *immopnd = CreateImmOperand(stackFrameSize, 32, true);
    SelectSub(spOpnd, spOpnd, immopnd, PTY_u64);
    ipoint = curbb->lastinsn;
    cfiOffset = stackFrameSize;
  } else {
    if (stackFrameSize > STP_LDP_IMM64_UPPER_BOUND) {
      useStpSub = true;
      offset = 16;
      stackFrameSize -= offset;
    } else {
      offset = stackFrameSize;
    }
    MOperator mop = pushPopOps[kRegsPushOp][rty][PUSH_POP_SINGLE];
    Riscv64RegOperand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
    Riscv64MemOperand *o1 = CreateCallFrameOperand(-offset, SIZEOFPTR * BITS_PER_BYTE);
    Insn *allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1);
    AppendInstructionTo(allocInsn, this);
    o0 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
    Riscv64OfstOperand *ofopnd = GetOrCreateOfstOpnd(SIZEOFPTR, 32);
    Riscv64RegOperand *rsp = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * 8, kRegTyInt);
    o1 = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, rsp, nullptr, ofopnd, 0);
    allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1);
    AppendInstructionTo(allocInsn, this);
    ipoint = allocInsn;
    cfiOffset = offset;
    if (cg->NeedInsertInstrumentationFunction()) {
      AppendCall(cg->GetInstrumentationFunction());
    } else if (cg->InstrumentWithDebugTraceCall()) {
      AppendCall(cg->GetDebugTraceEnterFunction());
    } else if (cg->InstrumentWithProfile()) {
      AppendCall(cg->GetProfileFunction());
    }
  }

  if (cg->GenerateCfiDirectives()) {
    ipoint = InsertCFIDefCfaOffset(cfiOffset, ipoint);
  }

  if (!HasVLAOrAlloca() && argsToStkpassSize > 0) {
    CG_ASSERT(!useStpSub, "Invalid assumption");
    MOperator mop = pushPopOps[kRegsPushOp][rty][PUSH_POP_PAIR];
    Insn *allocInsn = nullptr;
    if (argsToStkpassSize > STP_LDP_IMM64_UPPER_BOUND) {
      if (argsToStkpassSize <= STR_LDR_IMM64_UPPER_BOUND - 8) {
        mop = pushPopOps[kRegsPushOp][rty][PUSH_POP_SINGLE];
        Riscv64RegOperand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
        Riscv64MemOperand *o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize, SIZEOFPTR * BITS_PER_BYTE);
        allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o2);
        AppendInstructionTo(allocInsn, this);
        Riscv64RegOperand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
        o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize + SIZEOFPTR, SIZEOFPTR * BITS_PER_BYTE);
        allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o1, o2);
        AppendInstructionTo(allocInsn, this);
      } else {
        Riscv64RegOperand *oo = GetOrCreatePhysicalRegisterOperand(R9, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
        Riscv64ImmOperand *io = CreateImmOperand(argsToStkpassSize, 64, true);
        SelectCopyImm(oo, io, PTY_i64);

        Riscv64RegOperand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
        Riscv64RegOperand *rsp = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * 8, kRegTyInt);
        AppendInstructionTo(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, oo, rsp, oo), this);
        Riscv64OfstOperand *ofopnd = GetOrCreateOfstOpnd(0, 32);
        Riscv64MemOperand *mo = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, oo, nullptr, ofopnd, 0);
        allocInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, o0, mo);
        AppendInstructionTo(allocInsn, this);

        io = CreateImmOperand(SIZEOFPTR, 64, true);
        SelectAdd(oo, oo, io, PTY_i64);

        Riscv64RegOperand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
        ofopnd = GetOrCreateOfstOpnd(0, 32);
        mo = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, oo, nullptr, ofopnd, 0);
        allocInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, o1, mo);
        AppendInstructionTo(allocInsn, this);
      }
    } else {
      Operand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
      Operand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
      Operand *o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize, SIZEOFPTR * BITS_PER_BYTE);
      Operand *o3 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize+SIZEOFPTR, SIZEOFPTR * BITS_PER_BYTE);
      //allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1, o2);
      allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o2);
      AppendInstructionTo(allocInsn, this);
      allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o1, o3);
      AppendInstructionTo(allocInsn, this);
    }
    ipoint = allocInsn;
    if (cg->NeedInsertInstrumentationFunction()) {
      AppendCall(cg->GetInstrumentationFunction());
    } else if (cg->InstrumentWithDebugTraceCall()) {
      AppendCall(cg->GetDebugTraceEnterFunction());
    } else if (cg->InstrumentWithProfile()) {
      AppendCall(cg->GetProfileFunction());
    }
  }

  if (useStpSub) {
    Operand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
    Operand *immopnd = CreateImmOperand(stackFrameSize, 32, true);
    SelectSub(spOpnd, spOpnd, immopnd, PTY_u64);
    ipoint = curbb->lastinsn;
    this->used_stp_sub_pair_to_allocate_call_frame = true;
  }

  if (cg->GenerateCfiDirectives()) {
    CG_ASSERT(ipoint, "ipoint should not be nullptr at this point");
    cfiOffset = 0;
    if (useStpSub) {
      cfiOffset = stackFrameSize;
      ipoint = InsertCFIDefCfaOffset(cfiOffset, ipoint);
    }
    cfiOffset = GetOffsetFromCFA();
    if (!HasVLAOrAlloca()) {
      cfiOffset -= argsToStkpassSize;
    }
    ipoint = curbb->InsertInsnAfter(ipoint,
                                    cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset, CreateCfiRegOperand(RFP, 64),
                                                                       CreateCfiImmOperand(-cfiOffset, 64)));
    (void)curbb->InsertInsnAfter(ipoint,
                                 cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset, CreateCfiRegOperand(RRA, 64),
                                                                    CreateCfiImmOperand(-cfiOffset + 8, 64)));
  }
}

void Riscv64CGFunc::AppendInstructionAllocateCallFrameDebug(Riscv64reg_t reg0, Riscv64reg_t reg1, RegType rty) {
  if (cg->GenerateVerboseAsm()) {
    curbb->AppendInsn(
      cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand("allocate activation frame for debugging")));
  }

  int64 stackFrameSize = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize();
  int64 argsToStkpassSize = memlayout->SizeOfArgsToStackpass();

  Insn *ipoint = nullptr;
  int cfiOffset = 0;

  if (argsToStkpassSize > 0) {
    Operand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
    Operand *immopnd = CreateImmOperand(stackFrameSize, 32, true);
    SelectSub(spOpnd, spOpnd, immopnd, PTY_u64);
    ipoint = curbb->lastinsn;
    cfiOffset = stackFrameSize;
    if (cg->GenerateCfiDirectives()) {
      ipoint = InsertCFIDefCfaOffset(cfiOffset, ipoint);
    }

    MOperator mop = pushPopOps[kRegsPushOp][rty][PUSH_POP_SINGLE];
    Insn *allocInsn = nullptr;
    if (argsToStkpassSize > STP_LDP_IMM64_UPPER_BOUND) {
      if (argsToStkpassSize <= STR_LDR_IMM64_UPPER_BOUND - 8) {
        mop = pushPopOps[kRegsPushOp][rty][PUSH_POP_SINGLE];
        Riscv64RegOperand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
        Riscv64MemOperand *o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize, SIZEOFPTR * BITS_PER_BYTE);
        allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o2);
        AppendInstructionTo(allocInsn, this);
        if (cg->GenerateCfiDirectives()) {
          ipoint = allocInsn;
          cfiOffset = GetOffsetFromCFA() - argsToStkpassSize;
          ipoint = curbb->InsertInsnAfter(
              ipoint, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset,
                  CreateCfiRegOperand(RFP, 64), CreateCfiImmOperand(-cfiOffset, 64)));
        }

        Riscv64RegOperand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
        o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize + SIZEOFPTR, SIZEOFPTR * BITS_PER_BYTE);
        allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o1, o2);
        AppendInstructionTo(allocInsn, this);
        if (cg->GenerateCfiDirectives()) {
          ipoint = allocInsn;
          curbb->InsertInsnAfter(
              ipoint, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset,
                  CreateCfiRegOperand(RRA, 64), CreateCfiImmOperand(-cfiOffset + 8, 64)));
        }
      } else {
        Riscv64RegOperand *oo = GetOrCreatePhysicalRegisterOperand(R9, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
        Riscv64ImmOperand *io = CreateImmOperand(argsToStkpassSize, 64, true);
        SelectCopyImm(oo, io, PTY_i64);

        Riscv64RegOperand *rsp = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * 8, kRegTyInt);
        AppendInstructionTo(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, oo, rsp, oo), this);

        Riscv64RegOperand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
        Riscv64OfstOperand *ofopnd = GetOrCreateOfstOpnd(0, 32);
        Riscv64MemOperand *mo = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, oo, nullptr, ofopnd, 0);
        allocInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, o0, mo);
        AppendInstructionTo(allocInsn, this);
        if (cg->GenerateCfiDirectives()) {
          ipoint = allocInsn;
          cfiOffset = GetOffsetFromCFA() - argsToStkpassSize;
          ipoint = curbb->InsertInsnAfter(
              ipoint, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset,
                  CreateCfiRegOperand(RFP, 64), CreateCfiImmOperand(-cfiOffset, 64)));
        }

        io = CreateImmOperand(SIZEOFPTR, 64, true);
        SelectAdd(oo, oo, io, PTY_i64);

        Riscv64RegOperand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
        ofopnd = GetOrCreateOfstOpnd(0, 32);
        mo = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, oo, nullptr, ofopnd, 0);
        allocInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, o1, mo);
        AppendInstructionTo(allocInsn, this);
        if (cg->GenerateCfiDirectives()) {
          ipoint = allocInsn;
          curbb->InsertInsnAfter(
              ipoint, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset,
                  CreateCfiRegOperand(RRA, 64), CreateCfiImmOperand(-cfiOffset + 8, 64)));
        }
      }
    } else {
      Operand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
      Operand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
      Operand *o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize, SIZEOFPTR * BITS_PER_BYTE);
      Operand *o3 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize+SIZEOFPTR, SIZEOFPTR * BITS_PER_BYTE);
      //allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1, o2);
      allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o2);
      AppendInstructionTo(allocInsn, this);
      if (cg->GenerateCfiDirectives()) {
        ipoint = allocInsn;
        cfiOffset = GetOffsetFromCFA() - argsToStkpassSize;
        ipoint = curbb->InsertInsnAfter(
            ipoint, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset,
                CreateCfiRegOperand(RFP, 64), CreateCfiImmOperand(-cfiOffset, 64)));
      }
      allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o1, o3);
      AppendInstructionTo(allocInsn, this);
      if (cg->GenerateCfiDirectives()) {
        ipoint = allocInsn;
        curbb->InsertInsnAfter(
            ipoint, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset,
                CreateCfiRegOperand(RRA, 64), CreateCfiImmOperand(-cfiOffset + 8, 64)));
      }
    }
    ipoint = allocInsn;
    if (cg->NeedInsertInstrumentationFunction()) {
      AppendCall(cg->GetInstrumentationFunction());
    } else if (cg->InstrumentWithDebugTraceCall()) {
      AppendCall(cg->GetDebugTraceEnterFunction());
    } else if (cg->InstrumentWithProfile()) {
      AppendCall(cg->GetProfileFunction());
    }
  } else {
    bool useStpSub = false;

    if (stackFrameSize > STP_LDP_IMM64_UPPER_BOUND) {
      useStpSub = true;
      Riscv64RegOperand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
      ImmOperand *immopnd = CreateImmOperand(stackFrameSize, 32, true);
      SelectSub(spOpnd, spOpnd, immopnd, PTY_u64);
      ipoint = curbb->lastinsn;
      cfiOffset = stackFrameSize;
      if (cg->GenerateCfiDirectives()) {
        ipoint = InsertCFIDefCfaOffset(cfiOffset, ipoint);
      }
    } else {
      // Generate new sp
      RegOperand *rsp = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * 8, kRegTyInt);
      ImmOperand *iopnd = CreateImmOperand(-stackFrameSize, 32, true);
      Insn *spAdd = cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, rsp, rsp, iopnd);
      AppendInstructionTo(spAdd, this);
      ipoint = spAdd;
      cfiOffset = stackFrameSize;
      if (cg->GenerateCfiDirectives()) {
        ipoint = InsertCFIDefCfaOffset(cfiOffset, ipoint);
      }
      // Save regs on new stack
      MOperator mop = pushPopOps[kRegsPushOp][rty][PUSH_POP_SINGLE];
      Riscv64RegOperand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
      Riscv64OfstOperand *ofopnd = GetOrCreateOfstOpnd(0, 32);
      Riscv64MemOperand *o1 = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, rsp, nullptr, ofopnd, 0);
      Insn *allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1);
      AppendInstructionTo(allocInsn, this);
      if (cg->GenerateCfiDirectives()) {
        ipoint = allocInsn;
        cfiOffset = GetOffsetFromCFA() - argsToStkpassSize;
        ipoint = curbb->InsertInsnAfter(
            ipoint, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset,
                CreateCfiRegOperand(RFP, 64), CreateCfiImmOperand(-cfiOffset, 64)));
      }
      o0 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
      ofopnd = GetOrCreateOfstOpnd(SIZEOFPTR, 32);
      o1 = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, rsp, nullptr, ofopnd, 0);
      allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1);
      AppendInstructionTo(allocInsn, this);
      if (cg->GenerateCfiDirectives()) {
        ipoint = allocInsn;
        curbb->InsertInsnAfter(
            ipoint, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset,
                CreateCfiRegOperand(RRA, 64), CreateCfiImmOperand(-cfiOffset + 8, 64)));
      }
    }

    if (useStpSub) {
      MOperator mop = pushPopOps[kRegsPushOp][rty][PUSH_POP_SINGLE];
      Riscv64RegOperand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
      Riscv64MemOperand *o2 = memPool->New<Riscv64MemOperand>(RSP, 0, SIZEOFPTR * BITS_PER_BYTE);
      Insn *allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o2);
      AppendInstructionTo(allocInsn, this);
      if (cg->GenerateCfiDirectives()) {
        ipoint = allocInsn;
        cfiOffset = GetOffsetFromCFA() - argsToStkpassSize;
        ipoint = curbb->InsertInsnAfter(
            ipoint, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset,
                CreateCfiRegOperand(RFP, 64), CreateCfiImmOperand(-cfiOffset, 64)));
      }

      Riscv64RegOperand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
      o2 = memPool->New<Riscv64MemOperand>(RSP, SIZEOFPTR, SIZEOFPTR * BITS_PER_BYTE);
      allocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o1, o2);
      AppendInstructionTo(allocInsn, this);
      if (cg->GenerateCfiDirectives()) {
        ipoint = allocInsn;
        curbb->InsertInsnAfter(
            ipoint, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_offset,
                CreateCfiRegOperand(RRA, 64), CreateCfiImmOperand(-cfiOffset + 8, 64)));
      }
    }

    if (cg->NeedInsertInstrumentationFunction()) {
      AppendCall(cg->GetInstrumentationFunction());
    } else if (cg->InstrumentWithDebugTraceCall()) {
      AppendCall(cg->GetDebugTraceEnterFunction());
    } else if (cg->InstrumentWithProfile()) {
      AppendCall(cg->GetProfileFunction());
    }
  }
}

void Riscv64CGFunc::AppendInstructionPopSingle(Riscv64reg_t reg, RegType rty, int offset) {
  MOperator mop = pushPopOps[kRegsPopOp][rty][PUSH_POP_SINGLE];
  Operand *o0 = GetOrCreatePhysicalRegisterOperand(reg, SIZEOFPTR * BITS_PER_BYTE, rty);
  Operand *o1 = CreateStkTopOpnd(offset, SIZEOFPTR * BITS_PER_BYTE);

  uint32 datasize = SIZEOFPTR * BITS_PER_BYTE;
  if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(o1), datasize)) {
    o1 = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(o1), datasize, Riscv64Abi::kIntSpareReg);
  }

  Insn *popInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1);
  string comment = "RESTORE";
  popInsn->AddComment(comment);
  curbb->AppendInsn(popInsn);

  // Append CFI code.
  if (cg->GenerateCfiDirectives()) {
    Insn *restoreCfiInsn = cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(reg, 64));
    curbb->AppendInsn(restoreCfiInsn);
  }
}

void Riscv64CGFunc::AppendInstructionPopPair(Riscv64reg_t reg0, Riscv64reg_t reg1, RegType rty, int offset) {
  MOperator mop = pushPopOps[kRegsPopOp][rty][PUSH_POP_PAIR];
  Operand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
  Operand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
  Operand *o2 = CreateStkTopOpnd(offset, SIZEOFPTR * BITS_PER_BYTE);

  uint32 datasize = SIZEOFPTR * BITS_PER_BYTE;
  CG_ASSERT(offset >= 0, "");
  if (offset > STP_LDP_IMM64_UPPER_BOUND) {
    o2 = SplitStpLdpOffsetForCalleeSavedWithAddInstruction(static_cast<Riscv64MemOperand *>(o2), datasize, R16);
  }
  Insn *popInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1, o2);
  string comment = "RESTORE RESTORE";
  popInsn->AddComment(comment);
  curbb->AppendInsn(popInsn);

  // Append CFI code
  Insn *restoreCfiInsn = cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(reg0, 64));
  curbb->AppendInsn(restoreCfiInsn);
  restoreCfiInsn = cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(reg1, 64));
  curbb->AppendInsn(restoreCfiInsn);
}

void Riscv64CGFunc::AppendInstructionDeallocateCallFrame(Riscv64reg_t reg0, Riscv64reg_t reg1, RegType rty) {
  MOperator mop = pushPopOps[kRegsPopOp][rty][PUSH_POP_SINGLE];
  Operand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
  Operand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
  int64 stackFrameSize = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize();
  int64 argsToStkpassSize = memlayout->SizeOfArgsToStackpass();
  // ldp/stp's imm should be within -512 and 504;
  // if ldp's imm > 504, we fall back to the ldp-add version
  bool useLdpAdd = false;
  int64 offset = 0;

  Operand *o2 = nullptr;
  Operand *o3 = nullptr;
  if (!HasVLAOrAlloca() && argsToStkpassSize > 0) {
    o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize, SIZEOFPTR * BITS_PER_BYTE);
    o3 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize+SIZEOFPTR, SIZEOFPTR * BITS_PER_BYTE);
  } else {
    if (stackFrameSize > STP_LDP_IMM64_UPPER_BOUND) {
      useLdpAdd = true;
      offset = 16;
      stackFrameSize -= offset;
    } else {
      offset = stackFrameSize;
    }
    o2 = CreateCallFrameOperand(offset, SIZEOFPTR * BITS_PER_BYTE);
    o3 = CreateCallFrameOperand(offset+SIZEOFPTR, SIZEOFPTR * BITS_PER_BYTE);
  }

  if (useLdpAdd) {
    Operand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
    Operand *immopnd = CreateImmOperand(stackFrameSize, 32, true);
    SelectAdd(spOpnd, spOpnd, immopnd, PTY_u64);
    if (cg->GenerateCfiDirectives()) {
      int cfiOffset = GetOffsetFromCFA();
      (void)curbb->InsertInsnAfter(
        curbb->lastinsn, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_def_cfa, CreateCfiRegOperand(RSP, 64),
                                                            CreateCfiImmOperand(cfiOffset - stackFrameSize, 64)));
    }
  }

  if (!HasVLAOrAlloca() && argsToStkpassSize > 0) {
    CG_ASSERT(!useLdpAdd, "Invalid assumption");
    if (argsToStkpassSize > STP_LDP_IMM64_UPPER_BOUND) {
      if (argsToStkpassSize <= STR_LDR_IMM64_UPPER_BOUND - 8) {
        mop = pushPopOps[kRegsPopOp][rty][PUSH_POP_SINGLE];
        o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
        o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize, SIZEOFPTR * BITS_PER_BYTE);
        Insn *popInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o2);
        AppendInstructionTo(popInsn, this);
        o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
        o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize + SIZEOFPTR, SIZEOFPTR * BITS_PER_BYTE);
        popInsn = cg->BuildInstruction<Riscv64Insn>(mop, o1, o2);
        AppendInstructionTo(popInsn, this);
      } else {
        Riscv64RegOperand *oo = GetOrCreatePhysicalRegisterOperand(R9, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
        Riscv64ImmOperand *io = CreateImmOperand(argsToStkpassSize, 64, true);
        SelectCopyImm(oo, io, PTY_i64);

        o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
        Riscv64RegOperand *rsp = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * 8, kRegTyInt);
        AppendInstructionTo(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, oo, rsp, oo), this);
        Riscv64OfstOperand *ofopnd = GetOrCreateOfstOpnd(0, 32);
        Riscv64MemOperand *mo = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, oo, nullptr, ofopnd, 0);
        Insn *popInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xldr, o0, mo);
        AppendInstructionTo(popInsn, this);

        io = CreateImmOperand(SIZEOFPTR, 64, true);
        SelectAdd(oo, oo, io, PTY_i64);

        o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
        ofopnd = GetOrCreateOfstOpnd(0, 32);
        mo = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, oo, nullptr, ofopnd, 0);
        popInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xldr, o1, mo);
        AppendInstructionTo(popInsn, this);
      }
    } else {
      //Insn *deallocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1, o2);
      Insn *deallocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o2);
      curbb->AppendInsn(deallocInsn);
      deallocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o1, o3);
      curbb->AppendInsn(deallocInsn);
    }
    Operand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
    Operand *immopnd = CreateImmOperand(stackFrameSize, 32, true);
    Operand *offset = FixLargeStackOffset(static_cast<ImmOperand *>(immopnd));
    if (offset == nullptr) {
      offset = immopnd;
    }
    SelectAdd(spOpnd, spOpnd, offset, PTY_u64);
  } else {
    Insn *deallocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1, o2);
    curbb->AppendInsn(deallocInsn);
  }

  // Append CFI restore
  curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RFP, 64)));
  curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RRA, 64)));
}

void Riscv64CGFunc::AppendInstructionDeallocateCallFrameDebug(Riscv64reg_t reg0, Riscv64reg_t reg1, RegType rty) {
  MOperator mop = pushPopOps[kRegsPopOp][rty][PUSH_POP_SINGLE];
  Operand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
  Operand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
  int64 stackFrameSize = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize();
  int64 argsToStkpassSize = memlayout->SizeOfArgsToStackpass();
  /* ldp/stp's imm should be within -512 and 504;
   * if ldp's imm > 504, we fall back to the ldp-add version
   */
  if (HasVLAOrAlloca() || argsToStkpassSize == 0) {
    stackFrameSize -= argsToStkpassSize;
    if (stackFrameSize > STP_LDP_IMM64_UPPER_BOUND) {
      Operand *o2;
      o2 = memPool->New<Riscv64MemOperand>(RSP, 0, SIZEOFPTR * BITS_PER_BYTE);
      Insn *deallocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o2);
      curbb->AppendInsn(deallocInsn);
      curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RFP, 64)));
      o2 = memPool->New<Riscv64MemOperand>(RSP, SIZEOFPTR, SIZEOFPTR * BITS_PER_BYTE);
      deallocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o1, o2);
      curbb->AppendInsn(deallocInsn);
      // Append CFI restore
      curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RRA, 64)));
      Operand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
      Operand *immopnd = CreateImmOperand(stackFrameSize, 32, true);
      Operand *offset = FixLargeStackOffset(static_cast<ImmOperand *>(immopnd));
      if (offset == nullptr) {
        offset = immopnd;
      }
      SelectAdd(spOpnd, spOpnd, offset, PTY_u64);
    } else {
      RegOperand *rsp = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * 8, kRegTyInt);
      Riscv64OfstOperand *ofopnd = GetOrCreateOfstOpnd(0, 32);
      Riscv64MemOperand *o2 = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, rsp, nullptr, ofopnd, 0);
      Insn *deallocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o2);
      curbb->AppendInsn(deallocInsn);
      curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RFP, 64)));
      ofopnd = GetOrCreateOfstOpnd(SIZEOFPTR, 32);
      o2 = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, rsp, nullptr, ofopnd, 0);
      deallocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o1, o2);
      curbb->AppendInsn(deallocInsn);
      curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RRA, 64)));
      // Restore old sp
      MOperator mop = MOP_xaddrri12;
      ImmOperand *iopnd = CreateImmOperand(stackFrameSize, 32, true);
      Operand *offset = FixLargeStackOffset(iopnd);
      if (offset) {
        mop = MOP_xaddrrr;
      } else {
        offset = iopnd;
      }
      Insn *spAdd = cg->BuildInstruction<Riscv64Insn>(mop, rsp, rsp, offset);
      curbb->AppendInsn(spAdd);
    }

  } else {
    Operand *o2;
    o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize, SIZEOFPTR * BITS_PER_BYTE);
    if (argsToStkpassSize > STP_LDP_IMM64_UPPER_BOUND) {
      if (argsToStkpassSize <= STR_LDR_IMM64_UPPER_BOUND - 8) {
        mop = pushPopOps[kRegsPopOp][rty][PUSH_POP_SINGLE];
        Riscv64RegOperand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
        Riscv64MemOperand *o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize, SIZEOFPTR * BITS_PER_BYTE);
        Insn *popInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o2);
        AppendInstructionTo(popInsn, this);
        curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RFP, 64)));

        Riscv64RegOperand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
        o2 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize + SIZEOFPTR, SIZEOFPTR * BITS_PER_BYTE);
        popInsn = cg->BuildInstruction<Riscv64Insn>(mop, o1, o2);
        AppendInstructionTo(popInsn, this);
        curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RRA, 64)));
      } else {
        Riscv64RegOperand *oo = GetOrCreatePhysicalRegisterOperand(R9, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
        Riscv64ImmOperand *io = CreateImmOperand(argsToStkpassSize, 64, true);
        SelectCopyImm(oo, io, PTY_i64);

        Riscv64RegOperand *o0 = GetOrCreatePhysicalRegisterOperand(reg0, SIZEOFPTR * BITS_PER_BYTE, rty);
        Riscv64RegOperand *rsp = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * 8, kRegTyInt);
        AppendInstructionTo(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, oo, rsp, oo), this);
        Riscv64OfstOperand *ofopnd = GetOrCreateOfstOpnd(0, 32);
        Riscv64MemOperand *mo = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, oo, nullptr, ofopnd, 0);
        Insn *popInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xldr, o0, mo);
        AppendInstructionTo(popInsn, this);
        curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RFP, 64)));

        io = CreateImmOperand(SIZEOFPTR, 64, true);
        SelectAdd(oo, oo, io, PTY_i64);

        Riscv64RegOperand *o1 = GetOrCreatePhysicalRegisterOperand(reg1, SIZEOFPTR * BITS_PER_BYTE, rty);
        ofopnd = GetOrCreateOfstOpnd(0, 32);
        mo = GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE, oo, nullptr, ofopnd, 0);
        popInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xldr, o1, mo);
        AppendInstructionTo(popInsn, this);
        curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RRA, 64)));
      }
    } else {
      Operand *o3 = memPool->New<Riscv64MemOperand>(RSP, argsToStkpassSize+SIZEOFPTR, SIZEOFPTR * BITS_PER_BYTE);
      //Insn *deallocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o1, o2);
      Insn *deallocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o0, o2);
      curbb->AppendInsn(deallocInsn);
      curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RFP, 64)));
      deallocInsn = cg->BuildInstruction<Riscv64Insn>(mop, o1, o3);
      curbb->AppendInsn(deallocInsn);
      curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RRA, 64)));
    }

    Operand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
    Operand *immopnd = CreateImmOperand(stackFrameSize, 32, true);
    Operand *offset = FixLargeStackOffset(static_cast<ImmOperand *>(immopnd));
    if (offset == nullptr) {
      offset = immopnd;
    }
    SelectAdd(spOpnd, spOpnd, offset, PTY_u64);
  }
}

Insn *Riscv64CGFunc::GenerateCfiRestores(BB *lastbb) {
  Insn *ipoint = nullptr;
  Insn *restoreRlrInst = cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RRA, 64));
  if (lastbb->firstinsn) {
    ipoint = lastbb->InsertInsnAfter(lastbb->lastinsn, restoreRlrInst);
  } else {
    ipoint = lastbb->firstinsn = lastbb->lastinsn = restoreRlrInst;
  }
  ipoint = lastbb->InsertInsnAfter(
    ipoint, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore, CreateCfiRegOperand(RFP, 64)));
  return lastbb->InsertInsnAfter(
    ipoint,
    cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_def_cfa, CreateCfiRegOperand(RSP, 64), CreateCfiImmOperand(0, 64)));
}

Riscv64MemOperand *Riscv64CGFunc::CreateStkTopOpnd(int32 offset, int32 size) {
  if (cg->UseFP() || HasVLAOrAlloca()) {
    return memPool->New<Riscv64MemOperand>(RFP, offset, size);
  } else {
    return memPool->New<Riscv64MemOperand>(RSP, offset, size);
  }
}

/*
   From Riscv64 Reference Manual
   C1.3.3 Load/Store Addressing Mode
   ...
   When stack alignment checking is enabled by system software and
   the base register is the SP, the current stack pointer must be
   initially quadword aligned, that is aligned to 16 bytes. Misalignment
   generates a Stack Alignment fault.  The offset does not have to
   be a multiple of 16 bytes unless the specific Load/Store instruction
   requires this. SP cannot be used as a register offset.
 */
void Riscv64CGFunc::GeneratePushRegs() {
  MapleVector<Riscv64reg_t> &regsToSave = GetCalleeSavedRegs();

  CG_ASSERT(regsToSave.size() > 0, "FP/LR not added to callee-saved list?");

  if (cg->GenerateVerboseAsm()) {
    curbb->AppendInsn(
      cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand("save callee-saved registers")));
  }

  // Even if we don't use RFP, since we push a pair of registers in one instruction
  // and the stack needs be aligned on a 16-byte boundary, push RFP as well if function has a call
  // Make sure this is reflected when computing callee_saved_regs.size()
  if (cg->GenerateDebugFriendlyCode()) {
    AppendInstructionAllocateCallFrameDebug(RFP, RRA, kRegTyInt);
  } else {
    CHECK_FATAL(0, "Should not reach here");
    AppendInstructionAllocateCallFrame(RFP, RRA, kRegTyInt);
  }

  if (cg->UseFP() || HasVLAOrAlloca()) {
    if (cg->GenerateVerboseAsm()) {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand("copy SP to FP")));
    }
    Operand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
    Operand *fpOpnd = GetOrCreatePhysicalRegisterOperand(RFP, 64, kRegTyInt);
    int64 argsToStkpassSize = memlayout->SizeOfArgsToStackpass();
    if (argsToStkpassSize > 0) {
      Operand *immopnd = CreateImmOperand(argsToStkpassSize, 32, true);
      SelectAdd(fpOpnd, spOpnd, immopnd, PTY_u64);
      if (cg->GenerateCfiDirectives()) {
        cfi::ImmOperand *imm = memPool->New<cfi::ImmOperand>(
          static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize() - argsToStkpassSize, 64);
        curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_def_cfa, CreateCfiRegOperand(RFP, 64), imm));
      }

    } else {
      SelectCopy(fpOpnd, PTY_u64, spOpnd, PTY_u64);
      curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_def_cfa_register, CreateCfiRegOperand(RFP, 64)));
    }
  }

  MapleVector<Riscv64reg_t>::iterator it = regsToSave.begin();
  // skip the first two registers
  CG_ASSERT(*it == RFP, "The first two callee saved regs are expected to be RFP and RRA");
  ++it;
  CG_ASSERT(*it == RRA, "The first two callee saved regs are expected to be RFP and RRA");
  ++it;

  int offsetCalleeSingle = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize() -
                           (SizeOfCalleeSaved() - 2 * kIntregBytelen /*for FP/LR*/);

  if (cg->UseFP() || HasVLAOrAlloca()) {
    offsetCalleeSingle -= memlayout->SizeOfArgsToStackpass();
  }

  if (CLANG && func->GetAttr(FUNCATTR_varargs)) {
    // GR/VR save areas are above the callee save area
    Riscv64MemLayout *ml = static_cast<Riscv64MemLayout *>(memlayout);
    int saveareasize = RoundUp(ml->GetSizeOfGRSavearea(), SIZEOFPTR*2);
    offsetCalleeSingle -= saveareasize;
  }

  for (; it != regsToSave.end(); ++it) {
    Riscv64reg_t reg = *it;
    CG_ASSERT(reg != RFP && reg != RRA, "stray RFP/RRA in callee_saved_list?");

    RegType rty = Riscv64isa::IsGPRegister(reg) ? kRegTyInt : kRegTyFloat;
    AppendInstructionPushSingle(reg, rty, offsetCalleeSingle);
    GetNextOffsetCalleeSaved(offsetCalleeSingle);
  }

  // in case we split stp/ldp instructions,
  // so that we generate a load-into-base-register instruction
  // for pop pairs as well.
  split_stpldp_base_offset = 0;
}

void Riscv64CGFunc::GeneratePopRegs() {
  MapleVector<Riscv64reg_t> &regsToRestore = GetCalleeSavedRegs();

  CG_ASSERT(regsToRestore.size() > 0, "FP/LR not added to callee-saved list?");

  if (cg->GenerateVerboseAsm()) {
    curbb->AppendInsn(
      cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand("restore callee-saved registers")));
  }

  MapleVector<Riscv64reg_t>::iterator it = regsToRestore.begin();
  // Even if we don't use FP, since we push a pair of registers
  // in a single instruction (i.e., stp) and the stack needs be aligned
  // on a 16-byte boundary, push FP as well if the function has a call.
  // Make sure this is reflected when computing callee_saved_regs.size()
  // skip the first two registers
  CG_ASSERT(*it == RFP, "The first two callee saved regs are expected to be RFP and RRA");
  ++it;
  CG_ASSERT(*it == RRA, "The first two callee saved regs are expected to be RFP and RRA");
  ++it;

  int offsetCalleeSingle = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize() -
                           (SizeOfCalleeSaved() - 2 * kIntregBytelen /*for FP/LR*/);

  if (cg->UseFP() || HasVLAOrAlloca()) {
    offsetCalleeSingle -= memlayout->SizeOfArgsToStackpass();
  }

  if (CLANG && func->GetAttr(FUNCATTR_varargs)) {
    // GR/VR save areas are above the callee save area
    Riscv64MemLayout *ml = static_cast<Riscv64MemLayout *>(memlayout);
    int saveareasize = RoundUp(ml->GetSizeOfGRSavearea(), SIZEOFPTR*2);
    offsetCalleeSingle -= saveareasize;
  }

  // We are using a cleared dummy block; so insertPoint cannot be ret;
  // see GenerateEpilog()
  for (; it != regsToRestore.end(); ++it) {
    Riscv64reg_t reg = *it;
    CG_ASSERT(reg != RFP && reg != RRA, "stray RFP/RRA in callee_saved_list?");

    RegType rty = Riscv64isa::IsGPRegister(reg) ? kRegTyInt : kRegTyFloat;
    AppendInstructionPopSingle(reg, rty, offsetCalleeSingle);
    GetNextOffsetCalleeSaved(offsetCalleeSingle);
  }

  if (cg->GenerateDebugFriendlyCode()) {
    AppendInstructionDeallocateCallFrameDebug(RFP, RRA, kRegTyInt);
  } else {
    AppendInstructionDeallocateCallFrame(RFP, RRA, kRegTyInt);
  }

  if (cg->GenerateCfiDirectives()) {
    curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_def_cfa, CreateCfiRegOperand(RSP, 64),
                                                         CreateCfiImmOperand(0, 64)));
  }
  // in case we split stp/ldp instructions,
  // so that we generate a load-into-base-register instruction
  // for the next function, maybe? (seems not necessary, but...)
  split_stpldp_base_offset = 0;
}

Operand *Riscv64CGFunc::FixLargeStackOffset(ImmOperand *iopnd) {
  if (static_cast<Riscv64ImmOperand *>(iopnd)->IsInSimmBitSize(11) == false) {
    // Cannot use kIntSpareReg here since all registers have been restored in epilog
    Riscv64RegOperand *dst = GetOrCreatePhysicalRegisterOperand(R31, 64, kRegTyInt);
    Insn *li = cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, dst, CreateImmOperand(iopnd->GetValue(), 64, false));
    curbb->AppendInsn(li);
    return dst;
  }
  return nullptr;
}

void Riscv64CGFunc::Genstackguard(BB *bb) {
  if (cg->AddStackGuard()) {
    BB *formerCurbb = curbb;
    dummybb->ClearInsns();
    curbb = dummybb;

    MIRSymbol *stkguardsym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName("__stack_chk_guard"));
    StImmOperand *stopnd = CreateStImmOperand(stkguardsym, 0, 0);
    Riscv64RegOperand *staddropnd =
      static_cast<Riscv64RegOperand *>(GetOrCreatePhysicalRegisterOperand(R9, SIZEOFPTR * 8, kRegTyInt));
    SelectAddrof(staddropnd, stopnd);
    Riscv64MemOperand *guardmemopn =
      memPool->New<Riscv64MemOperand>(SIZEOFPTR * 8, staddropnd,
                                  static_cast<Riscv64RegOperand *>(nullptr), GetOrCreateOfstOpnd(0, 32), stkguardsym);
    MOperator mop = PickLdInsn(64, PTY_u64);
    Insn *ins = cg->BuildInstruction<Riscv64Insn>(mop, staddropnd, guardmemopn);
    ins->do_not_remove = true;
    curbb->AppendInsn(ins);

    int varea = 0;
    if (CLANG && func->GetAttr(FUNCATTR_varargs)) {
      Riscv64MemLayout *ml = static_cast<Riscv64MemLayout *>(memlayout);
      int vsize = ml->GetSizeOfGRSavearea();
      if (vsize > 0) {
        varea += RoundUp(vsize, RISCV64_STACK_PTR_ALIGNMENT);
      }
    }

    Riscv64MemOperand *downstk = nullptr;
    if (cg->UseFP() || HasVLAOrAlloca()) {
      int32 stksize = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize() -
                      static_cast<Riscv64MemLayout *>(memlayout)->SizeOfArgsToStackpass() -
                      varea;
      downstk = memPool->New<Riscv64MemOperand>(RFP, stksize - 8, SIZEOFPTR * BITS_PER_BYTE);
    } else {
      downstk = memPool->New<Riscv64MemOperand>(RSP, static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize() - 8 - varea,
                                            SIZEOFPTR * BITS_PER_BYTE);
    }
    if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(downstk), 64)) {
      downstk = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(downstk), 64, R10);
    }
    mop = PickStInsn(SIZEOFPTR * BITS_PER_BYTE, PTY_u64);
    ins = cg->BuildInstruction<Riscv64Insn>(mop, staddropnd, downstk);
    ins->do_not_remove = true;
    curbb->AppendInsn(ins);

    bb->InsertAtBeginning(dummybb);
    curbb = formerCurbb;
  }
}

void Riscv64CGFunc::MoveRegargs(BB *bb) {
  BB *formerCurbb = curbb;
  dummybb->ClearInsns();
  curbb = dummybb;
  ParmLocator parmlocator(becommon);
  PLocInfo ploc;

  for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
    if (i == 0) {
      MIRFunction *func = becommon.mirModule.CurFunction();
      if (func->IsReturnStruct()) {
        auto funcIt = becommon.funcReturnType.find(func);
        if (funcIt != becommon.funcReturnType.end() && becommon.type_size_table[funcIt->second.GetIdx()] <= 16) {
          continue;
        }
      }
    }

    MIRType *typ = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->formalDefVec[i].formalTyIdx);
    parmlocator.LocateNextParm(typ, ploc, i == 0);

    MIRSymbol *sym = func->formalDefVec[i].formalSym;
    if (sym->IsPreg()) {
      continue;
    }
    Riscv64SymbolAlloc *symloc =
      static_cast<Riscv64SymbolAlloc *>(memlayout->sym_alloc_table[sym->GetStIndex()]);
    Riscv64RegOperand *baseOpnd = static_cast<Riscv64RegOperand *>(GetBaseReg(symloc));
    int32 stoffset = GetBaseOffset(symloc);

    Riscv64ImmOperand *zeroOffset = CreateImmOperand(0, 64, false);
    PrimType ty = typ->GetPrimType();
    uint32 sz = 0;
    MemOperand *memOpnd;

    // Save param or 1st half of agg param
    if (ploc.reg0 != kRinvalid) {
      RegType firstRegType = ploc.reg0 < V0 ? kRegTyInt : kRegTyFloat;
      // Size of parameter is based on its primType. As the stack space is computed
      // based on the alignment and size required, the store generated here must
      // follow that.
      if (ty == PTY_agg || (ploc.reg0 < V0 && IsPrimitiveInteger(ty) == false)) {
        // Riscv calling convention uses int parm regs to pass after
        // using all of fp parm regs for fp parms.
        ty = ploc.reg0 < V0 ? (ploc.rsize0 > 4 ? PTY_i64 : PTY_i32) : (ploc.rsize0 < 8 ? PTY_f32 : PTY_f64);
      }
      sz = GetPrimTypeSize(ty);
      if (Riscv64MemOperand::IsPIMMOffsetOutOfRange(stoffset, sz * BITS_PER_BYTE)) {
        RegOperand *baseReg = GetOrCreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        Riscv64ImmOperand *offsetOpnd = CreateImmOperand(stoffset, 64, false);
        SelectAdd(baseReg, baseOpnd, offsetOpnd, PTY_a64);
        memOpnd = memPool->New<Riscv64MemOperand>(sz * BITS_PER_BYTE,
                                                  baseReg, (Riscv64RegOperand *)nullptr, zeroOffset, sym);
      } else {
        Riscv64OfstOperand *offsetOpnd = CreateOfstOpnd(stoffset, 32);
        if (symloc->mem_segment->kind == kMsArgsStkpassed) {
          offsetOpnd->SetVary(true);
        }
        memOpnd =
          memPool->New<Riscv64MemOperand>(sz * BITS_PER_BYTE,
                                          baseOpnd, (Riscv64RegOperand *)nullptr, offsetOpnd, sym);
      }
      MOperator mop = PickStInsn(GetPrimTypeSize(ty) * BITS_PER_BYTE, ty);
      RegOperand *regOpnd = GetOrCreatePhysicalRegisterOperand(ploc.reg0, sz * BITS_PER_BYTE, firstRegType);
      Insn *ins = cg->BuildInstruction<Riscv64Insn>(mop, regOpnd, memOpnd);
      if (cg->GenerateVerboseAsm()) {
        ins->AddComment(std::string("store param: ").append(sym->GetName()));
      }
      curbb->AppendInsn(ins);
    }

    // Restore the primType overwritten by ploc.reg0.
    ty = typ->GetPrimType();
    // Save 2nd half of agg param
    if (ploc.reg1 != kRinvalid) {
      RegType secondRegType = ploc.reg1 < V0 ? kRegTyInt : kRegTyFloat;
      if (ploc.rsize1 != 0) { // 2nd valid reg may exist for agg
        if (ploc.rsize0 <= 4 && ploc.rsize1 <= 4) {
          stoffset += 4;
        } else {
          stoffset += SIZEOFPTR;
        }
        if (ty == PTY_agg || (ploc.reg0 < V0 && IsPrimitiveInteger(ty) == false)) {
          ty = ploc.reg1 < V0 ? (ploc.rsize1 > 4 ? PTY_i64 : PTY_i32) : (ploc.rsize1 < 8 ? PTY_f32 : PTY_f64);
        }
        sz = GetPrimTypeSize(ty);
        if (Riscv64MemOperand::IsPIMMOffsetOutOfRange(stoffset, sz * BITS_PER_BYTE)) {
          RegOperand *baseReg = GetOrCreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
          Riscv64ImmOperand *offsetOpnd = CreateImmOperand(stoffset, 64, false);
          SelectAdd(baseReg, baseOpnd, offsetOpnd, PTY_a64);
          memOpnd = memPool->New<Riscv64MemOperand>(sz * BITS_PER_BYTE,
                                                    baseReg, (Riscv64RegOperand *)nullptr, zeroOffset, sym);
        } else {
          Riscv64OfstOperand *offsetOpnd = CreateOfstOpnd(stoffset, 32);
          if (symloc->mem_segment->kind == kMsArgsStkpassed) {
            offsetOpnd->SetVary(true);
          }
          memOpnd = memPool->New<Riscv64MemOperand>(sz * BITS_PER_BYTE,
                                                    baseOpnd, (Riscv64RegOperand *)nullptr, offsetOpnd, sym);
        }
        MOperator mop = PickStInsn(GetPrimTypeSize(ty) * BITS_PER_BYTE, ty);
        RegOperand *regOpnd = GetOrCreatePhysicalRegisterOperand(ploc.reg1, sz * BITS_PER_BYTE, secondRegType);
        Insn *ins = cg->BuildInstruction<Riscv64Insn>(mop, regOpnd, memOpnd);
        if (cg->GenerateVerboseAsm()) {
          ins->AddComment(std::string("store param: ").append(sym->GetName()));
        }
        curbb->AppendInsn(ins);
      }
    }

    // Half in register, half on stack, store the half on stack into its local location
    uint32 aggSize = becommon.type_size_table.at(typ->tyIdx.GetIdx());
    Riscv64RegOperand *baseOpnd2;
    if (ploc.reg0 != kRinvalid && ploc.reg1 == kRinvalid &&
        aggSize <= 16 && aggSize > ploc.memsize && aggSize > ploc.reg0) {
      // load from stack
      if (UseFP() || HasVLAOrAlloca()) {
        baseOpnd2 = GetOrCreatePhysicalRegisterOperand(RFP, 64, kRegTyInt);
      } else {
        baseOpnd2 = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
      }
      RegOperand *baseReg = GetOrCreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      // StackFrameSize is not fixed yet, use isvary
      Riscv64ImmOperand *offsetOpnd =
        CreateImmOperand(0, 64, true, true); // isvary reset StackFrameSize
      if (ploc.memoffset) {
        Riscv64ImmOperand *offsetOpnd2 = CreateImmOperand(ploc.memoffset, 64, false);
        SelectAdd(baseReg, offsetOpnd, offsetOpnd2, PTY_a64);
        SelectAdd(baseReg, baseOpnd2, baseReg, PTY_a64);
      } else {
        SelectAdd(baseReg, baseOpnd2, offsetOpnd, PTY_a64);
      }
      memOpnd = GetOrCreateMemOpnd(64, baseReg, nullptr,
                zeroOffset, static_cast<MIRSymbol *>(nullptr));
      RegOperand *regOpnd = GetOrCreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      Insn *insn0 = cg->BuildInstruction<Riscv64Insn>(MOP_xldr, regOpnd, memOpnd);
      curbb->AppendInsn(insn0);

      // store to local location
      if (Riscv64MemOperand::IsPIMMOffsetOutOfRange(stoffset, SIZEOFPTR * BITS_PER_BYTE)) {
        RegOperand *baseReg = GetOrCreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        Riscv64ImmOperand *offsetOpnd = CreateImmOperand(stoffset+8, 64, false);
        SelectAdd(baseReg, baseOpnd2, offsetOpnd, PTY_a64);
        memOpnd = memPool->New<Riscv64MemOperand>(64,
                                                  baseReg, (Riscv64RegOperand *)nullptr, zeroOffset, sym);
      } else {
        Riscv64OfstOperand *offsetOpnd = CreateOfstOpnd(stoffset+8, 32);
        if (symloc->mem_segment->kind == kMsArgsStkpassed) {
          offsetOpnd->SetVary(true);
        }
        memOpnd = memPool->New<Riscv64MemOperand>(64,
                                                  baseOpnd, (Riscv64RegOperand *)nullptr, offsetOpnd, sym);
      }
      MOperator mop = PickStInsn(GetPrimTypeSize(PTY_i64) * BITS_PER_BYTE, ty);
      Insn *ins = cg->BuildInstruction<Riscv64Insn>(mop, regOpnd, memOpnd);
      if (cg->GenerateVerboseAsm()) {
        ins->AddComment(std::string("store param: ").append(sym->GetName()));
      }
      curbb->AppendInsn(ins);
    }
  }

  bb->InsertAtBeginning(dummybb);
  curbb = formerCurbb;
}

void Riscv64CGFunc::MoveVRegargs(BB *bb) {
  BB *formerCurbb = curbb;
  dummybb->ClearInsns();
  curbb = dummybb;
  ParmLocator parmlocator(becommon);
  PLocInfo ploc;

  for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
    if (i == 0) {
      MIRFunction *func = becommon.mirModule.CurFunction();
      if (func->IsReturnStruct()) {
        auto funcIt = becommon.funcReturnType.find(func);
        if (funcIt != becommon.funcReturnType.end() && becommon.type_size_table[funcIt->second.GetIdx()] <= 16) {
          continue;
        }
      }
    }
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->formalDefVec[i].formalTyIdx);
    parmlocator.LocateNextParm(ty, ploc, i == 0);

    MIRSymbol *sym = func->formalDefVec[i].formalSym;
    if (!sym->IsPreg()) {
      continue;
    }

    PrimType stype = sym->GetType()->primType;
    int bytesize = GetPrimTypeSize(stype);

    PregIdx pregIdx = func->pregTab->GetPregIdxFromPregNo(sym->GetPreg()->pregNo);
    RegOperand *dstRegopnd = GetOrCreateVirtualRegisterOperand(GetVirtualRegNoFromPseudoRegIdx(pregIdx));

    if (ploc.reg0 == 0) {
      // load stack parameters to the vreg.
      int bitsize = bytesize * BITS_PER_BYTE;
      MemOperand *memopnd = GetOrCreateMemOpnd(sym, 0, bitsize);

      Insn *insn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(GetPrimTypeBitSize(stype), stype), dstRegopnd, memopnd);

      if (cg->GenerateVerboseAsm()) {
        std::string key = "param: %%";

        key += std::to_string(sym->GetPreg()->pregNo);
        CG_ASSERT(sym->GetStorageClass() == kScFormal, "vreg parameters should be kScFormal type.");

        insn->AddComment(key);
      }

      curbb->InsertInsnBegin(insn);
    } else {
      RegType regtype = ploc.reg0 < V0 ? kRegTyInt : kRegTyFloat;

      uint8 srcBitsize = uint8((bytesize < 4 ? 4 : bytesize) * BITS_PER_BYTE);
      dstRegopnd->size_ = srcBitsize;

      RegOperand *srcRegopnd = GetOrCreatePhysicalRegisterOperand(ploc.reg0, srcBitsize, regtype);

      CG_ASSERT(sym->storageClass == kScFormal, "should be args");

      MOperator mop = PickMovInsn(srcBitsize, regtype);
      Insn *ins = cg->BuildInstruction<Riscv64Insn>(mop, dstRegopnd, srcRegopnd);
      if (cg->GenerateVerboseAsm()) {
        std::string key = "param: %%";
        key += std::to_string(sym->GetPreg()->pregNo);
        ins->AddComment(key);
      }
      curbb->InsertInsnBegin(ins);
    }

    if (ploc.reg1 == 0) {
      continue;
    }
  }

  bb->InsertAtBeginning(dummybb);
  curbb = formerCurbb;
}

/*
 * The following functions are for pattern matching for fast path
 * where function has early return of spedified pattern load/test/return
 */
void Riscv64CGFunc::ReplaceMachedOperand(Insn *orig, Insn *target, RegOperand *matchedOpnd, bool isFirstDst) {
  for (int s = 0; s < target->GetOpndNum(); s++) {
    Operand *src = target->GetOpnd(s);
    CHECK_FATAL(src, "src is null in ReplaceMachedOperand");
    if (src->IsRegister()) {
      RegOperand *reg = static_cast<RegOperand *>(src);
      if (reg != matchedOpnd) {
        continue;
      }
      RegOperand *phys = nullptr;
      if (isFirstDst) {
        Operand *origSrc = orig->GetOpnd(0);
        RegOperand *origPhys = static_cast<RegOperand *>(origSrc);
        CHECK_FATAL(origPhys, "pointer is null");
        regno_t regno = origPhys->GetRegisterNumber();
        phys = GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)(regno), matchedOpnd->GetSize(), kRegTyInt);
      } else {
        /* Replace the operand with the src of inst */
        phys = GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)(R16), reg->GetSize(), kRegTyInt);
      }
      target->SetOpnd(s, phys);
      return;
    } else if (src->IsMemoryAccessOperand()) {
      MemOperand *memopnd = static_cast<MemOperand *>(src);
      Operand *base = memopnd->GetBaseRegister();
      Operand *offset = memopnd->GetIndexRegister();
      if (base && base->IsRegister()) {
        RegOperand *reg = static_cast<RegOperand *>(base);
        if (reg != matchedOpnd) {
          continue;
        }
        RegOperand *phys = nullptr;
        if (isFirstDst) {
          Operand *origSrc = orig->GetOpnd(0);
          RegOperand *origPhys = static_cast<RegOperand *>(origSrc);
          CHECK_FATAL(origPhys, "orig_phys cast failed");
          regno_t regno = origPhys->GetRegisterNumber();
          phys = GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)(regno), matchedOpnd->GetSize(), kRegTyInt);
        } else {
          /* Replace the operand with the src of inst */
          phys = GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)(R16), base->GetSize(), kRegTyInt);
        }
        memopnd->SetBaseRegister(phys);
        return;
      }
      if (offset && offset->IsRegister()) {
        RegOperand *reg = static_cast<RegOperand *>(offset);
        if (reg != matchedOpnd) {
          continue;
        }
        RegOperand *phys = nullptr;
        if (isFirstDst) {
          Operand *origSrc = orig->GetOpnd(0);
          RegOperand *origPhys = static_cast<RegOperand *>(origSrc);
          CHECK_FATAL(origPhys, "orig_phys is nullptr in ReplaceMachedOperand");
          regno_t regno = origPhys->GetRegisterNumber();
          phys = GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)(regno), matchedOpnd->GetSize(), kRegTyInt);
        } else {
          /* Replace the operand with the src of inst */
          phys = GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)(R16), offset->GetSize(), kRegTyInt);
        }
        memopnd->SetIndexRegister(phys);
        return;
      }
    }
  }
  if (isFirstDst == false) {
    RegOperand *phys;
    phys = GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)(R16), matchedOpnd->GetSize(), kRegTyInt);
    orig->SetResult(0, phys);
  }
  return;
}

void Riscv64CGFunc::ForwardPropagateAndRename(Insn *mov, Insn *load, BB *terminateBb) {
  // This is specialized function to work with IsolateFastPath().
  // The control flow and instruction pattern must be recognized.
  Insn *insn = mov;
  bool isFirstDst = true;
  for (int i = 0; i < 2; i++) {
    /* Finish the bb the mov is in */
    for (Insn *target = insn->next; target; target = target->next) {
      if (target->IsImmaterialInsn()) {
        continue;
      }
      if (!target->IsMachineInstruction()) {
        continue;
      }
      Operand *dst = insn->GetResult(0);
      RegOperand *rdst = static_cast<RegOperand *>(dst);
      CHECK_FATAL(rdst, "rdst is nullptr in ForwardPropagateAndRename");
      ReplaceMachedOperand(insn, target, rdst, isFirstDst);
    }
    BB *bb = insn->bb->succs.front();
    while (1) {
      FOR_BB_INSNS(target, bb) {
        if (target->IsImmaterialInsn()) {
          continue;
        }
        if (!target->IsMachineInstruction()) {
          continue;
        }
        Operand *dst = insn->GetResult(0);
        RegOperand *rdst = static_cast<RegOperand *>(dst);
        CHECK_FATAL(rdst, "rdst is nullptr in ForwardPropagateAndRename");
        ReplaceMachedOperand(insn, target, rdst, isFirstDst);
      }
      if (bb == terminateBb) {
        break;
      }
      bb = bb->succs.front();
    }
    insn = load;
    isFirstDst = false;
  }
}

bool Riscv64CGFunc::BackwardFindDependency(BB *ifbb, BB *returnbb, RegOperand *targetOpnd, Insn *&load, Insn *&mov,
                                           Insn *&depMov, std::list<Insn *> &list) {
  load = nullptr;
  mov = nullptr;
  depMov = nullptr;
  BB *pred = ifbb;
  /* Pattern match,  (*) instruction are moved down below branch.
   *   mov reg1, R0                         mov  reg1, R0                        *
   *   ld  Rx  , [reg1, const]              ld   R16 , [R0, const]
   *   mov reg2, Rx                   =>    mov  reg2, R16    <- this may exist  *
   *                                        mov  Rx  , R16    <- replicate       *
   *   cbr       Rx, label                  cbr        R16, label
   *
   *   R16 is used because it is used as spill register.
   *   Need to modify if different register allcoation mechanism is used.
   */
  do {
    FOR_BB_INSNS_REV(insn, pred) {
      if (insn == ifbb->lastinsn) {
        continue;
      }
      if (insn->IsImmaterialInsn()) {
        continue;
      }
      if (!insn->IsMachineInstruction()) {
        continue;
      }

      bool found = false;  // allowing for only one src to be register
      for (int r = 0; r < insn->GetResultNum(); r++) {
        Operand *dst = insn->GetResult(r);
        CHECK_FATAL(dst, "pointer is null");
        if (!dst->IsRegister()) {
          continue;
        }
        RegOperand *regopnd = static_cast<RegOperand *>(dst);
        if (regopnd != targetOpnd) {
          continue;
        }
        if (load != nullptr) {
          if (mov != nullptr) {
            return false;
          }
          MOperator opcode = insn->GetMachineOpcode();
          if (opcode == MOP_xmovrr) {
            Operand *mvSrc = insn->GetOpnd(0);
            RegOperand *mvRegsrc = static_cast<RegOperand *>(mvSrc);
            CHECK_FATAL(mvRegsrc, "mv_regsrc cast failed");
            regno_t mvReg = mvRegsrc->GetRegisterNumber();
            // make it very specific for now
            if (mvReg != R0) {
              return false;
            }
            Operand *mvDst = insn->GetResult(0);
            RegOperand *mvRegdst = static_cast<RegOperand *>(mvDst);
            CHECK_FATAL(mvRegdst, "mv_regdst cast failed");
            mvReg = mvRegdst->GetRegisterNumber();
            if (mvReg != R20) {
              return false;
            }
            mov = insn;
          } else {
            return false;
          }
        }
        /* Found def, continue dep chain with src */
        for (int s = 0; s < insn->GetOpndNum(); s++) {
          Operand *src = insn->GetOpnd(s);
          CHECK_FATAL(src != nullptr, "src is nullptr in BackwardFindDependency");
          if (src->IsRegister()) {
            if (found) {
              return false;
            }
            RegOperand *preg = static_cast<RegOperand *>(src);
            targetOpnd = preg;
            if (preg->IsPhysicalRegister() && insn->GetMachineOpcode() == MOP_xmovrr) {
              /* Skipping the start of the dependency chain because
               * the the parameter reg will be propagated leaving
               * the mov instruction alone to be relocated down
               * to the cold path.
               */
              found = false;
            } else {
              return false;
            }
          } else if (src->IsMemoryAccessOperand()) {
            MemOperand *memopnd = static_cast<MemOperand *>(src);
            Operand *base = memopnd->GetBaseRegister();
            Operand *offset = memopnd->GetIndexRegister();
            if (base && base->IsRegister()) {
              if (found) {
                return false;
              }
              load = insn;
              targetOpnd = static_cast<RegOperand *>(base);
              found = true;
              Operand *ldDst = insn->GetResult(0);
              RegOperand *ldRdst = static_cast<RegOperand *>(ldDst);
              CHECK_FATAL(ldRdst != nullptr, "ld_rdst is nullptr in BackwardFindDependency");
              if (ldRdst->GetRegisterNumber() != R1) {
                return false;  // hard code for now.
              }
              // Make sure instruction depending on load is mov and cond br
              for (Insn *ni = insn->next; ni; ni = ni->next) {
                if (ni->GetMachineOpcode() == MOP_xmovrr || ni->GetMachineOpcode() == MOP_wmovrr) {
                  Operand *dep = ni->GetOpnd(0);
                  RegOperand *rdep = static_cast<RegOperand *>(dep);
                  if (rdep == ldRdst) {
                    if (depMov == nullptr) {
                      depMov = ni;
                    } else {
                      return false;
                    }
                  }
                }
              }
            }
            if (offset && offset->IsRegister()) {
              return false;
            }
          }
        }
      }
      if (found == false) {
        list.push_back(insn);
      }
    }
    if (pred->preds.empty()) {
      break;
    }
    pred = pred->preds.front();
  } while (pred);

  return true;
}

BB *Riscv64CGFunc::IsolateFastPath(BB *bb) {
  // Issue #1103
  /* Detect "if (cond) return" fast path, and move extra instructions
   * to the slow path.
   * Must match the following block structure.  BB1 can be a series of
   * single-pred/single-succ blocks.
   *     BB1 ops1 cmp-br to BB3        BB1 cmp-br to BB3
   *     BB2 ops2 br to retBB    ==>   BB2 ret
   *     BB3 slow path                 BB3 ops1 ops2
   * BB3 will be used to generate prolog stuff.
   */
  if (bb->prev) {
    return nullptr;
  }
  BB *ifbb = nullptr;
  BB *returnbb = nullptr;
  BB *coldbb = nullptr;
  {
    BB *curbb = bb;
    /* Look for straight line code */
    while (1) {
      if (curbb->eh_succs.size() != 0) {
        return nullptr;
      }
      if (curbb->succs.size() == 1) {
        if (curbb->GetKind() == BB::kBBCall) {
          return nullptr;
        }
        BB *succ = curbb->succs.front();
        if (succ->preds.size() != 1 || succ->eh_preds.size() != 0) {
          return nullptr;
        }
        curbb = succ;
      } else if (curbb->GetKind() == BB::kBBIf) {
        ifbb = curbb;
        break;
      } else {
        return nullptr;
      }
    }
  }
  /* targets of if bb can only be reached by if bb */
  {
    BB *first = ifbb->succs.front();
    BB *second = ifbb->succs.back();
    if (first->preds.size() != 1 || first->eh_preds.size() != 0) {
      return nullptr;
    }
    if (second->preds.size() != 1 || second->eh_preds.size() != 0) {
      return nullptr;
    }

    /* One target of the if bb jumps to a return bb */
    if (first->GetKind() == BB::kBBGoto && first->succs.front()->GetKind() == BB::kBBReturn) {
      if (second->succs.size() == 0) {
        return nullptr;
      }
      returnbb = first;
      coldbb = second;
    } else {
      return nullptr;
    }
  }
  /* The control flow matches at this point.
   * Make sure the hot bb contains atmost a
   * 'mov x0, value' and 'b'.
   */
  {
    if (returnbb->NumInsn() > 2) {
      return nullptr;
    }
    Insn *first = returnbb->firstinsn;
    while (first->IsImmaterialInsn()) {
      first = first->next;
    }
    if (first == returnbb->lastinsn) {
      if (!first->IsBranch()) {
        return nullptr;
      }
    } else {
      MOperator opcode = first->GetMachineOpcode();
      /* only allow mov constant */
      if (opcode != MOP_xmovri64) {
        return nullptr;
      }
      Insn *last = returnbb->lastinsn;
      if (!last->IsBranch()) {
        return nullptr;
      }
    }
  }
  /*
   * Resolve any register usage issues.
   * 1) Any use of parameter registes must be renamed
   * 2) Any usage of callee saved register that needs saving in prolog
   *    must be able to move down into the cold path.
   */
  RegOperand *targetOpnd = nullptr;
  {
    /* Find the branch's src register for backward propagation. */
    Insn *condbr = ifbb->lastinsn;
    Operand *srcopnd1 = condbr->opnds[0];
    if (srcopnd1 == nullptr) {
      return nullptr;
    }
    targetOpnd = static_cast<RegOperand *>(srcopnd1);
    if (targetOpnd->GetRegisterType() != kRegTyInt) {
      return nullptr;
    }
  }
  /* Search backward looking for dependencies for the cond branch */
  std::list<Insn *> insnlist;  // instructions to be moved to coldbb
  Insn *ld = nullptr;
  Insn *mv = nullptr;
  Insn *depmv = nullptr;
  // The mv is the 1st move using the parameter register leading to the branch
  // The ld is the load using the parameter register indirectly for the branch
  // The depmv is the move which preserves the result of the load but might
  //    destroy a parameter register which will be moved below the branch.
  if (BackwardFindDependency(ifbb, returnbb, targetOpnd, ld, mv, depmv, insnlist) == false) {
    return nullptr;
  }
  if (ld == nullptr || mv == nullptr) {
    return nullptr;
  }
  /* depmv can be live out, so duplicate it
   * and set dest to output of ld and src R16
   */
  if (depmv) {
    CG_ASSERT(depmv->GetMachineOpcode(), "return check");
    Insn *newMv = cg->BuildInstruction<Riscv64Insn>(
      depmv->GetMachineOpcode(), ld->opnds[0],
      GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)(R16), depmv->opnds[1]->GetSize(), kRegTyInt));
    insnlist.push_front(newMv);
    /* temporary put it some where */
    coldbb->InsertInsnBegin(newMv);
  } else {
    uint32 regsize = ld->opnds[0]->GetSize();
    Insn *newMv =
      cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, ld->opnds[0],
                                        GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)(R16), regsize, kRegTyInt));
    insnlist.push_front(newMv);
    /* temporary put it some where */
    coldbb->InsertInsnBegin(newMv);
  }

  ForwardPropagateAndRename(mv, ld, returnbb);

  std::list<Insn *>::iterator it;
  for (it = insnlist.begin(); it != insnlist.end(); it++) {
    Insn *in = *it;
    in->bb->RemoveInsn(in);
    coldbb->InsertInsnBegin(in);
  }

  // All instructions are in the right place, replace branch to ret bb to just ret.
  returnbb->RemoveInsn(returnbb->lastinsn);
  returnbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xret));
  // bb is now a retbb and has no succ.
  returnbb->SetKind(BB::kBBReturn);
  BB *tgtBB = returnbb->succs.front();
  auto predIt = std::find(tgtBB->preds.begin(), tgtBB->preds.end(), returnbb);
  tgtBB->preds.erase(predIt);
  returnbb->succs.clear();

  return coldbb;
}

void Riscv64CGFunc::Savelockinfo(BB * bb) {
  BB *formerCurbb = curbb;
  dummybb->ClearInsns();
  curbb = dummybb;
  RegOperand *baseOpnd = GetOrCreateStackBaseRegOperand();
  RegOperand *tmpreg = CreateRegisterOperandOfType(PTY_u64);
  Riscv64ImmOperand *locknum = CreateImmOperand(func->lockslotnum, 64, false);

  Riscv64SymbolAlloc symalloc;
  Riscv64MemLayout *memlayout = static_cast<Riscv64MemLayout *>(this->memlayout);
  symalloc.mem_segment = &memlayout->seg_lockobjslot;
  symalloc.offset = 0;

  int32 slotbase = GetBaseOffset(&symalloc);

  MemOperand *stackloc = CreateMemOpnd(baseOpnd, slotbase, 64);

  if (func->lockslotnum == 0) {
    tmpreg = &Riscv64RegOperand::Get64bitZeroRegister();
  } else {
    SelectCopy(tmpreg, PTY_u64, locknum, PTY_u64);
  }

  auto allocInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, tmpreg, stackloc);
  curbb->AppendInsn(allocInsn);

  bb->InsertAtBeginning(dummybb);
  curbb = formerCurbb;
}

void Riscv64CGFunc::GenSavemethodinfoCode(BB * bb) {
  if (cg->UseFastUnwind()) {
    BB *formerCurbb = curbb;
    dummybb->ClearInsns();
    curbb = dummybb;
    // FUNCATTR_bridge for function: Ljava_2Flang_2FString_3B_7CcompareTo_7C_28Ljava_2Flang_2FObject_3B_29I, to
    // exclude this funciton this function is a bridge function generated for Java Genetic
    if ((func->GetAttr(FUNCATTR_native) || func->GetAttr(FUNCATTR_fast_native)) &&
        !func->GetAttr(FUNCATTR_critical_native) && !func->GetAttr(FUNCATTR_bridge)) {
      RegOperand *fpreg = GetOrCreatePhysicalRegisterOperand(RFP, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);

      Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
      Riscv64RegOperand *parmregopnd = GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
      srcOpnds->PushOpnd(parmregopnd);
      Operand *immopnd = CreateImmOperand(0, 64, false);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xadri64, parmregopnd, immopnd));
      parmregopnd = GetOrCreatePhysicalRegisterOperand(R1, 64, kRegTyInt);
      srcOpnds->PushOpnd(parmregopnd);
      SelectCopy(parmregopnd, PTY_a64, fpreg, PTY_a64);

      MIRSymbol *sym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
      std::string funcname(GetIntrinsicFuncName(INTRN_MCCSetRiskyUnwindContext));
      sym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));

      sym->storageClass = kScText;
      sym->sKind = kStFunc;

      Operand *targetopnd = GetOrCreateFuncNameOpnd(sym);
      Insn *callInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xbl, targetopnd, srcOpnds);
      curbb->AppendInsn(callInsn);
      bb->SetKind(BB::kBBCall);
    }

    bb->InsertAtBeginning(dummybb);
    curbb = formerCurbb;
  }
}

/* Insert stp/str xzr, [mem] for each spill slot. */
void Riscv64CGFunc::InitialSpillSlot(BB * bb) {
  Riscv64MemLayout *riscv64MemLayout = static_cast<Riscv64MemLayout *>(memlayout);
  int32 offset = riscv64MemLayout->RealStackFrameSize() - (SizeOfCalleeSaved() - 2 * kIntregBytelen /*for FP/LR*/);

  if (riscv64MemLayout->GetUnusedSlot()) {
    offset -= kIntregBytelen;
  }

  int32 spillSize = RoundUp(riscv64MemLayout->GetSizeOfSpillReg(), SIZEOFPTR);
  offset -= spillSize;

  if (cg->UseFP() || HasVLAOrAlloca()) {
    offset -= memlayout->SizeOfArgsToStackpass();
  }

  int32 i = 0;
  for (; i <= spillSize - 2 * kIntregBytelen; i += (2 * kIntregBytelen)) {
    if ((offset + i) > STP_LDP_IMM64_UPPER_BOUND) {
      break;
    }
    Insn *pushInsn0 = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, GetZeroOpnd(64),
                                                       CreateStkTopOpnd(offset + i, SIZEOFPTR * BITS_PER_BYTE));
    Insn *pushInsn1 = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, GetZeroOpnd(64),
                                                       CreateStkTopOpnd(offset + i + 8, SIZEOFPTR * BITS_PER_BYTE));
    if (cg->GenerateVerboseAsm()) {
      std::string key = "initial spill slot";
      pushInsn0->AddComment(key);
      pushInsn1->AddComment(key);
    }
    bb->AppendInsn(pushInsn0);
    bb->AppendInsn(pushInsn1);
  }

  for (; i <= spillSize - kIntregBytelen; i += kIntregBytelen) {
    uint32 datasize = SIZEOFPTR * BITS_PER_BYTE;
    Operand* memopnd = CreateStkTopOpnd(offset + i, datasize);
    if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand*>(memopnd), datasize)) {
      memopnd = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand*>(memopnd), datasize, R9);
    }

    Insn* pushInsn = cg->BuildInstruction<Riscv64Insn>( MOP_xstr, GetZeroOpnd(64), memopnd);
    if (cg->GenerateVerboseAsm()) {
      std::string key = "initial spill slot";
      pushInsn->AddComment(key);
    }
    bb->AppendInsn(pushInsn);
  }
}

void Riscv64CGFunc::AppendInstructionStackCheck(Riscv64reg_t reg, RegType rty, int offset) {
  // sub x16, sp, #0x2000
  auto x16Opnd = GetOrCreatePhysicalRegisterOperand(reg, 64, rty);
  auto spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, rty);
  auto imm1 = CreateImmOperand(offset, 64, true);
  SelectSub(x16Opnd, spOpnd, imm1, PTY_u64);

  // ldr wzr, [x16]
  auto &wzr = Riscv64RegOperand::Get32bitZeroRegister();
  auto refX16 = CreateMemOpnd(reg, 0, 64);
  auto soeInstr = cg->BuildInstruction<Riscv64Insn>(MOP_wldr, &wzr, refX16);
  if (cg->GenerateVerboseAsm()) {
    soeInstr->AddComment("soerror");
  }
  soeInstr->do_not_remove = true;
  AppendInstructionTo(soeInstr, this);
}

void Riscv64CGFunc::GenerateProlog(BB * bb) {
  BB *formerCurbb = curbb;
  dummybb->ClearInsns();
  dummybb->isProEpilog = true;
  curbb = dummybb;
  ParmLocator parmlocator(becommon);
  Operand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
  Operand *fpOpnd = GetOrCreatePhysicalRegisterOperand(RFP, 64, kRegTyInt);
  if (!hasProEpilogue) {
    return;
  }

  if (func->IsJava()) {
    // modify return-address to tag this frame as Java frame
    Operand *lrOpnd = GetOrCreatePhysicalRegisterOperand(RRA, 64, kRegTyInt);
    Operand *imm1 = CreateImmOperand(1, 64, true);
    SelectAdd(lrOpnd, lrOpnd, imm1, PTY_u64);
  }

  // insert .loc for function
  if (cg->cgopt_.WithLoc()) {
    if (cg->cgopt_.WithSrc()) {
      uint32 tempmaxsize = mirModule.srcFileInfo.size();
      uint32 endfilenum = mirModule.srcFileInfo[tempmaxsize - 1].second;
      if (func->srcPosition.Filenum() != 0 && func->srcPosition.Filenum() <= endfilenum) {
        Operand *o0 = CreateDbgImmOperand(func->srcPosition.Filenum());
        // Operand *o1 = CreateDbgImmOperand((func->srcPosition->Linenum() ? func->srcPosition->Linenum() : 1));
        int64_t lineNum = func->srcPosition.Linenum();
        if (lineNum == 0) {
          if (func->funcAttrs.GetAttr(FUNCATTR_native)) {
            lineNum = 0xffffe;
          } else {
            lineNum = 0xffffd;
          }
        }
        Operand *o1 = CreateDbgImmOperand(lineNum);
        Insn *loc = cg->BuildInstruction<mpldbg::DbgInsn>(mpldbg::OP_DBG_loc, o0, o1);
        curbb->AppendInsn(loc);
      }
    } else {
      Operand *o0 = CreateDbgImmOperand(1);
      // line number might not be available.
      //CG_ASSERT(func->srcPosition.MplLinenum(), "return check");
      Operand *o1 = CreateDbgImmOperand(func->srcPosition.MplLinenum());
      Insn *loc = cg->BuildInstruction<mpldbg::DbgInsn>(mpldbg::OP_DBG_loc, o0, o1);
      curbb->AppendInsn(loc);
    }
  }

  MapleVector<Riscv64reg_t> &regsToSave = GetCalleeSavedRegs();
  if (regsToSave.size() > 0) {
    // Among other things, push the FP & LR pair.
    // FP/LR are added to the callee-saved list in AllocateRegisters()
    // We add them to the callee-saved list regardless of UseFP() being true/false.
    // Activation Frame is allocated as part of pushing FP/LR pair
    GeneratePushRegs();
  } else {
    int64 stackFrameSize = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize();
    if (stackFrameSize > 0) {
      if (cg->GenerateVerboseAsm()) {
        curbb->AppendInsn(
          cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand("allocate activation frame")));
      }
      Operand *immopnd = CreateImmOperand(stackFrameSize, 32, true);
      SelectSub(spOpnd, spOpnd, immopnd, PTY_u64);

      if (cg->GenerateCfiDirectives()) {
        int offset = stackFrameSize;
        (void)InsertCFIDefCfaOffset(offset, curbb->lastinsn);
      }
    }
    if (cg->UseFP() || HasVLAOrAlloca()) {
      if (cg->GenerateVerboseAsm()) {
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand("copy SP to FP")));
      }
      int64 argsToStkpassSize = memlayout->SizeOfArgsToStackpass();
      if (argsToStkpassSize > 0) {
        Operand *immopnd = CreateImmOperand(argsToStkpassSize, 32, true);
        SelectAdd(fpOpnd, spOpnd, immopnd, PTY_u64);
        if (cg->GenerateCfiDirectives()) {
          // curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>( cfi::OP_CFI_def_cfa_register,
          // CreateCfiRegOperand(RFP,64)));
          cfi::ImmOperand *imm = memPool->New<cfi::ImmOperand>(
            static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize() - argsToStkpassSize, 64);
          curbb->AppendInsn(
            cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_def_cfa, CreateCfiRegOperand(RFP, 64), imm));
        }
      } else {
        SelectCopy(fpOpnd, PTY_u64, spOpnd, PTY_u64);
        if (cg->GenerateCfiDirectives()) {
          curbb->AppendInsn(
            cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_def_cfa_register, CreateCfiRegOperand(RFP, 64)));
        }
      }
    }
  }
  if (CLANG && func->GetAttr(FUNCATTR_varargs)) {
    Riscv64MemLayout *memlayout = static_cast<Riscv64MemLayout *>(this->memlayout);
    uint32 dataSizeBits = SIZEOFPTR * BITS_PER_BYTE;
    int32 offset = memlayout->GetGRSaveareaBaseLoc();
    if (memlayout->GetSizeOfGRSavearea() % RISCV64_STACK_PTR_ALIGNMENT) {
      offset += SIZEOFPTR;  // End of area should be aligned. Hole between VR and GR area
    }
    int32 start_regno = 8 - (memlayout->GetSizeOfGRSavearea() / SIZEOFPTR);
    CG_ASSERT(start_regno <= 8, "Incorrect starting GR regno for GR Save Area");
    for (int32 i = start_regno + (int)R10; i <= (int)R17; i++) {
      Operand *stackloc = CreateStkTopOpnd(offset, dataSizeBits);
      RegOperand *reg= GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)i, 64, kRegTyInt);
      Insn *inst = cg->BuildInstruction<Riscv64Insn>(
                     PickStInsn(dataSizeBits, PTY_i64), reg, stackloc);
      curbb->AppendInsn(inst);
      offset += SIZEOFPTR;
    }
  }
  if (cg->DoCheckSOE()) {
    AppendInstructionStackCheck(R16, kRegTyInt, SOE_CHCK_OFFSET);
  }
  bb->InsertAtBeginning(dummybb);
  curbb = formerCurbb;
  dummybb->isProEpilog = false;
}

void Riscv64CGFunc::GenerateRet(BB * bb) {
  MOperator mop;
  Riscv64reg_t preg;
  if (retRegType == kRegTyInt) {
    mop = MOP_pseudo_ret_int;
    preg = R10;
  } else {
    mop = MOP_pseudo_ret_float;
    preg = V10;
  }
  bb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, GetOrCreatePhysicalRegisterOperand(preg, 64, kRegTyInt)));
  bb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xret));
}

bool Riscv64CGFunc::HasStackLoadStore() {
  FOR_ALL_BB(bb, this) {
    FOR_BB_INSNS(insn, bb) {
      for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
        Operand *opnd = insn->GetOperand(i);
        if (!opnd) {
          continue;
        }

        if (opnd->IsMemoryAccessOperand()) {
          MemOperand *memopnd = static_cast<MemOperand *>(opnd);
          Operand *base = memopnd->GetBaseRegister();

          if (base != nullptr && base->IsRegister()) {
            RegOperand *regOpnd = static_cast<RegOperand *>(base);
            RegType regtype = regOpnd->GetRegisterType();
            uint32 regno = regOpnd->GetRegisterNumber();
            if ((regtype != kRegTyCc && (regno == RFP || regno == RSP)) || regtype == kRegTyVary) {
              return true;
            }
          }
        }
      }
    }
  }
  return false;
}

bool Riscv64CGFunc::HasClinit() {
  FOR_ALL_BB(bb, this) {
    FOR_BB_INSNS(insn, bb) {
      if (insn->mop_ == MOP_clinit || insn->mop_ == MOP_clinit_tail) {
        return true;
      }
    }
  }
  return false;
}

bool Riscv64CGFunc::HasCall() {
  FOR_ALL_BB(bb, this) {
    FOR_BB_INSNS(insn, bb) {
      if (insn->IsCall()) {
        return true;
      }
    }
  }
  return false;
}

bool Riscv64CGFunc::HasLoop() {
  FOR_ALL_BB(bb, this) {
    if (bb->IsBackEdgeDest()) {
      return true;
    }
  }
  return false;
}

/*
   Remove redundant mov and mark optimizable bl/blr insn in the BB.
   Return value: true if is empty bb, otherwise false.
 */
bool Riscv64CGFunc::OptimizeTailBB(BB * bb, set<Insn *> & callInsns) {
  FOR_BB_INSNS_REV_SAFE(insn, bb, prev_insn) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }
    MOperator insnMop = insn->GetMachineOpcode();
    switch (insnMop) {
      case MOP_wmovrr:
      case MOP_xmovrr: {
        CG_ASSERT(insn->GetOperand(0)->IsRegister() && insn->GetOperand(1)->IsRegister(), "expects registers");
        Riscv64RegOperand *reg1 = static_cast<Riscv64RegOperand *>(insn->GetOperand(0));
        Riscv64RegOperand *reg2 = static_cast<Riscv64RegOperand *>(insn->GetOperand(1));
        CG_ASSERT((reg1 && reg2), "invalid insn");

        if (reg1->GetRegisterNumber() != R10 || reg2->GetRegisterNumber() != R10) {
          return false;
        }

        bb->RemoveInsn(insn);
        break;
      }
      case MOP_xbl: {
        callInsns.insert(insn);
        return false;
      }
      case MOP_xblr: {
        callInsns.insert(insn);
        return false;
      }
      default:
        return false;
    }
  }

  return true;
}

/* Recursively invoke this function until exitBB's precursor not exist.*/
void Riscv64CGFunc::TailCallBBOpt(BB * exitBB, set<Insn *> & callInsns) {
  for (auto tmpBB : exitBB->preds) {
    if (tmpBB->succs.size() != 1 || tmpBB->eh_succs.size() != 0 ||
        (tmpBB->GetKind() != BB::kBBFallthru && tmpBB->GetKind() != BB::kBBCall)) {
      continue;
    }

    if (OptimizeTailBB(tmpBB, callInsns)) {
      TailCallBBOpt(tmpBB, callInsns);
    }
  }
}

/*
   If a function without callee-saved register, and end with a function call,
   then transfer bl/blr to b/br.
   Return value: true if function do not need Prologue/Epilogue. false otherwise.
 */
bool Riscv64CGFunc::TailCallOpt() {
  BB *exitBB = nullptr;
  MapleVector<Riscv64reg_t> &regsToRestore = GetCalleeSavedRegs();

  CG_ASSERT(regsToRestore.size() >= 2, "Forgot FP and LR ?");

  if ((mirModule.IsCModule()) &&
      static_cast<Riscv64MemLayout *>(memlayout)->locals().size > 0) {
    return false;
  }

  if (CLANG && func->GetAttr(FUNCATTR_varargs)) {
    return false;
  }

  if (regsToRestore.size() > 2 || HasStackLoadStore() || HasLoop() || func->GetAttr(FUNCATTR_callersensitive) ||
      (mirModule.IsJavaModule() && IsFuncNeedFrame(GetName()))) {
    return false;
  }

  uint32 exitbbsize = exitbbsvec.size();

  CG_ASSERT(exitbbsize == 1, "Should not be exist multiple exits.");

  if (exitbbsize == 0) {
    if (lastbb->prev->firststmt == cleanup_label && lastbb->prev->prev) {
      exitBB = lastbb->prev->prev;
    } else {
      exitBB = lastbb->prev;
    }
  } else {
    exitBB = exitbbsvec.front();
  }

  CG_ASSERT(exitBB->firstinsn == nullptr, "exit bb should be empty.");

  // Count how many call insns in the whole function.
  int nCount = 0;
  bool hasGetStackClass = false;

  FOR_ALL_BB(bb, this) {
    FOR_BB_INSNS(insn, bb) {
      if (insn->IsCall()) {
        if (insn->GetMachineOpcode() == MOP_xbl) {
          FuncNameOperand *target = static_cast<FuncNameOperand *>(insn->opnds[0]);
          if (mirModule.IsJavaModule() && IsFuncNeedFrame(target->GetName())) {
            hasGetStackClass = true;
          }
        }
        nCount++;
      }
    }
  }

  if (nCount > 0 && func->GetAttr(FUNCATTR_interface)) {
    return false;
  }

  if (hasGetStackClass) {
    return false;
  }

  set<Insn *> callInsns;

  TailCallBBOpt(exitBB, callInsns);

  if (nCount == callInsns.size()) {
    // Replace all of the call insns.
    for (auto callInsn : callInsns) {
      MOperator insnMop = callInsn->GetMachineOpcode();
      switch (insnMop) {
        case MOP_xbl: {
          callInsn->SetMOP(MOP_tail_call_opt_xbl);
          break;
        }
        case MOP_xblr: {
          callInsn->SetMOP(MOP_tail_call_opt_xblr);
          break;
        }
        default:
          CG_ASSERT(false, "Internal error.");
      }
    }
  } else {
    return false;
  }

  return true;
}

void Riscv64CGFunc::GenerateEpilogForCleanup(BB * bb) {
  CG_ASSERT(exitbbsvec.size() > 0, "exit bb size is zero!");
  if (exitbbsvec[0]->unreachable) {  // if exitbb is unreachable then exitbb can not be generated
    GenerateEpilog(bb);
  } else if (NeedCleanup()) {  // bl to the exit epilogue
    LabelOperand *targetopnd = GetOrCreateLabelOperand(exitbbsvec[0]->labidx);
    bb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xuncond, targetopnd));
  }
}

bool Riscv64CGFunc::IsCommentBB(BB * bb) {
  FOR_BB_INSNS(insn, bb) {
    if (!insn->IsMachineInstruction()) {
      continue;
    } else {
      return false;
    }
  }
  return true;
}

/* If all the preds of exitBB made the TailcallOpt(replace blr/bl with br/b), return true, we don't create ret insn.
   Otherwise, return false, create the ret insn.
 */
bool Riscv64CGFunc::TestPredsOfRetBB(BB * exitBB) {
  for (auto tmpBB : exitBB->preds) {
    Insn *firstInsn = tmpBB->firstinsn;
    if ((firstInsn == nullptr || IsCommentBB(tmpBB)) && (!tmpBB->preds.empty())) {
      if (TestPredsOfRetBB(tmpBB)) {
        continue;
      } else {
        return false;
      }
    } else {
      Insn *lastInsn = tmpBB->lastinsn;
      if (lastInsn != nullptr) {
        MOperator insnMop = lastInsn->GetMachineOpcode();
        if (MOP_tail_call_opt_xbl == insnMop || MOP_tail_call_opt_xblr == insnMop) {
          continue;
        } else {
          return false;
        }
      } else {
        return false;
      }
    }
  }
  return true;
}

void Riscv64CGFunc::GenerateEpilog(BB * bb) {
  BB *formerCurbb = curbb;
  dummybb->ClearInsns();
  dummybb->isProEpilog = true;
  curbb = dummybb;

  if (!hasProEpilogue) {
    if (bb->preds.empty()) {
      GenerateRet(curbb);
      bb->AppendBBInsns(curbb);
      curbb = formerCurbb;
    } else {
      if (!TestPredsOfRetBB(bb)) {
        GenerateRet(curbb);
        bb->AppendBBInsns(curbb);
        curbb = formerCurbb;
      }
    }
    return;
  }

  if (cg->AddStackGuard()) {  // && CGOptions::inRange)
    MIRSymbol *stkguardsym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName("__stack_chk_guard"));
    StImmOperand *stopnd = CreateStImmOperand(stkguardsym, 0, 0);
    Riscv64RegOperand *staddropnd =
      static_cast<Riscv64RegOperand *>(GetOrCreatePhysicalRegisterOperand(R9, SIZEOFPTR * 8, kRegTyInt));
    SelectAddrof(staddropnd, stopnd);
    Riscv64MemOperand *guardmemopn =
      memPool->New<Riscv64MemOperand>(SIZEOFPTR * 8, staddropnd,
                                  static_cast<Riscv64RegOperand *>(nullptr), GetOrCreateOfstOpnd(0, 32), stkguardsym);
    MOperator mop = PickLdInsn(64, PTY_u64);
    Insn *ins = cg->BuildInstruction<Riscv64Insn>(mop, staddropnd, guardmemopn);
    ins->do_not_remove = true;
    curbb->AppendInsn(ins);

    int varea = 0;
    if (CLANG && func->GetAttr(FUNCATTR_varargs)) {
      Riscv64MemLayout *ml = static_cast<Riscv64MemLayout *>(memlayout);
      int vsize = ml->GetSizeOfGRSavearea();
      if (vsize > 0) {
        varea += RoundUp(vsize, RISCV64_STACK_PTR_ALIGNMENT);
      }
    }

    Riscv64RegOperand *checkopn =
      static_cast<Riscv64RegOperand *>(GetOrCreatePhysicalRegisterOperand(R12, SIZEOFPTR * 8, kRegTyInt));
    Riscv64MemOperand *downstk = nullptr;
    if (cg->UseFP() || HasVLAOrAlloca()) {
      int32 stksize = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize() -
                      static_cast<Riscv64MemLayout *>(memlayout)->SizeOfArgsToStackpass() -
                      varea;
      downstk = memPool->New<Riscv64MemOperand>(RFP, stksize - 8, SIZEOFPTR * BITS_PER_BYTE);
    } else {
      downstk = memPool->New<Riscv64MemOperand>(RSP, static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize() - 8 - varea,
                                            SIZEOFPTR * BITS_PER_BYTE);
    }

    if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(downstk), 64)) {
      downstk = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(downstk), 64, R12);
    }

    mop = PickLdInsn(SIZEOFPTR * BITS_PER_BYTE, PTY_u64);
    ins = cg->BuildInstruction<Riscv64Insn>(mop, checkopn, downstk);
    ins->do_not_remove = true;
    curbb->AppendInsn(ins);
    SelectBxor(staddropnd, staddropnd, checkopn, PTY_u64);
    LabelIdx faillable = CreateLabel();
    RegOperand *zero = GetOrCreatePhysicalRegisterOperand(R0, SIZEOFPTR * 8, kRegTyInt);
    SelectCondGoto(GetOrCreateLabelOperand(faillable), OP_brtrue, OP_eq, staddropnd, zero, PTY_u64, false);
    MIRSymbol *failfunc = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName("__stack_chk_fail"));
    Operand *targetopnd = GetOrCreateFuncNameOpnd(failfunc);
    Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
    Insn *callInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xbl, targetopnd, srcOpnds);
    callInsn->do_not_remove = true;
    curbb->AppendInsn(callInsn);

    bb->AppendBBInsns(curbb);

    BB *newbb = CreateNewBB();
    newbb->AddLabel(faillable);
    lab2bbmap[newbb->labidx] = newbb;
    bb->AppendBB(newbb);
    if (lastbb == bb) {
      lastbb = newbb;
    }
    newbb->unreachable = bb->unreachable;
    curbb = newbb;
    dummybb->isProEpilog = false;
    bb = newbb;
  }

  Operand *spOpnd = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
  Operand *fpOpnd = GetOrCreatePhysicalRegisterOperand(RFP, 64, kRegTyInt);

  if (HasVLAOrAlloca()) {
    SelectCopy(spOpnd, PTY_u64, fpOpnd, PTY_u64);
  }

  // exit bb should always be reachable, since we need its existance for ".cfi_remember_state"
  if (cg->GenerateCfiDirectives() && bb != lastbb && bb->next) {
    BB *nextBB = bb->next;
    do {
      if (nextBB == lastbb || !nextBB->IsEmpty()) {
        break;
      } else {
        nextBB = nextBB->next;
      }
    } while (nextBB);
    if (!(nextBB == nullptr || nextBB->IsEmpty())) {
      curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_remember_state));
      nextBB->InsertInsnBefore(nextBB->firstinsn, cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_restore_state));
    }
  }

  MapleVector<Riscv64reg_t> &regsToSave = GetCalleeSavedRegs();
  if (regsToSave.size() > 0) {
    GeneratePopRegs();
  } else {
    int64 stackFrameSize = static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize();
    if (stackFrameSize > 0) {
      if (cg->GenerateVerboseAsm()) {
        curbb->AppendInsn(
          cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand("pop up activation frame")));
      }

      if (HasVLAOrAlloca()) {
        stackFrameSize -= static_cast<Riscv64MemLayout *>(memlayout)->seg__args_to_stkpass.size;
      }

      if (stackFrameSize > 0) {
        Operand *immopnd = CreateImmOperand(stackFrameSize, 32, true);
        SelectAdd(spOpnd, spOpnd, immopnd, PTY_u64);
        if (cg->GenerateCfiDirectives()) {
          curbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_def_cfa, CreateCfiRegOperand(RSP, 64),
                                                               CreateCfiImmOperand(0, 64)));
        }
      }
    }
  }

  if (func->IsJava()) {
    // restore return-address before leaving a Java frame
    Operand *lrOpnd = GetOrCreatePhysicalRegisterOperand(RRA, 64, kRegTyInt);
    Operand *imm1 = CreateImmOperand(1, 64, true);
    SelectSub(lrOpnd, lrOpnd, imm1, PTY_u64);
  }

  if (cg->InstrumentWithDebugTraceCall()) {
    AppendJump(cg->GetDebugTraceExitFunction());
  }

  GenerateRet(curbb);
  if (!(cg->AddStackGuard())) {  // && CGOptions::inRange))
    bb->AppendBBInsns(curbb);
  }
  curbb = formerCurbb;
  dummybb->isProEpilog = false;
}

void Riscv64CGFunc::GenerateYieldpoint(BB * bb) {
  // ldr wzr, [RYP]  # RYP hold address of the polling page.
  auto &wzr = Riscv64RegOperand::Get32bitZeroRegister();
  auto pollingPage = CreateMemOpnd(RYP, 0, 32);
  auto yieldPoint = cg->BuildInstruction<Riscv64Insn>(MOP_wldr, &wzr, pollingPage);
  if (cg->GenerateVerboseAsm()) {
    yieldPoint->AddComment("yieldpoint");
  }
  bb->AppendInsn(yieldPoint);
}

Operand *Riscv64CGFunc::GetTargetRetOperand(PrimType ptype, int32 sreg) {
  uint8 bitsize = GetPrimTypeBitSize(ptype) < 32 ? 32 : GetPrimTypeBitSize(ptype);
  Riscv64reg_t preg;
  switch (sreg) {
  case kSregRetval0:
    preg = IsPrimitiveFloat(ptype) ? V10 : R10;
    break;
  case kSregRetval1:
    preg = R11;
    break;
  default:
    preg = RLAST_INT_REG;
    CG_ASSERT(0, "GetTargetRetOperand: NYI");
  }
  return GetOrCreatePhysicalRegisterOperand(preg, bitsize, GetRegTyFromPrimTyRiscv64(ptype));
}

RegOperand *Riscv64CGFunc::CreateRegisterOperandOfType(PrimType primtype) {
  RegType regty = GetRegTyFromPrimTyRiscv64(primtype);
  uint32 bytelen = GetPrimTypeSize(primtype);
  return CreateRegisterOperandOfType(regty, bytelen);
}

RegOperand *Riscv64CGFunc::CreateRegisterOperandOfType(RegType regty, uint32 bytelen) {
  // Enhance when half-precision floating point operations are supported.
  if (bytelen < 4) {
    bytelen = 4;  // Riscv64 has 32-bit and 64-bit registers only
  }
  regno_t vRegNo = New_V_Reg(regty, bytelen);
  return CreateVirtualRegisterOperand(vRegNo);
}

void Riscv64CGFunc::MergeReturn() {
  CG_ASSERT(((mirModule.IsCModule())
             || (curbb->prev->firststmt == cleanup_label)), "must be");

  BB *tmpBb = nullptr;
  uint32 exitbbsize = exitbbsvec.size();
  if (exitbbsize == 0) {
    return;
  }
  if (exitbbsize == 1 && exitbbsvec[0] == curbb) {
    return;
  }
  if (1 == exitbbsize) {
    BB *onlyExitBB = exitbbsvec[0];
    BB *onlyExitBbNext = onlyExitBB->next;
    StmtNode *stmt = onlyExitBbNext->firststmt;
    // only deal with the return_BB in the middle
    if (stmt != cleanup_label) {
      BB *retbb = CreateNewBB();
      retbb->SetKind(BB::kBBReturn);
      retbb->frequency = onlyExitBB->frequency;
      onlyExitBB->AppendBB(retbb);
      LabelIdx labidx = CreateLabel();
      retbb->AddLabel(labidx);
      lab2bbmap[labidx] = retbb;
      // modify the original return BB.
      BB::BBKind k = onlyExitBB->kind;
      CG_ASSERT(k == BB::kBBReturn, "Error: suppose to merge multi return bb");
      onlyExitBB->SetKind(BB::kBBFallthru);

      exitbbsvec.pop_back();
      exitbbsvec.push_back(retbb);
      return;
    }
  }
  BB *retbb = CreateNewBB();
  cleanupbb->PrependBB(retbb);
  retbb->SetKind(BB::kBBReturn);
  LabelIdx labidx = CreateLabel();
  retbb->AddLabel(labidx);
  lab2bbmap[labidx] = retbb;
  LabelOperand *targetopnd = GetOrCreateLabelOperand(labidx);

  uint32_t freq = 0;
  for (uint32 i = 0; i < exitbbsize; i++) {
    tmpBb = exitbbsvec[i];
    BB::BBKind k = tmpBb->kind;
    CG_ASSERT(k == BB::kBBReturn, "Error: suppose to merge multi return bb");
    tmpBb->SetKind(BB::kBBGoto);
    tmpBb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xuncond, targetopnd));
    freq += tmpBb->frequency;
  }
  for (uint32 i = 0; i < exitbbsize; i++) {
    exitbbsvec.pop_back();
  }
  retbb->frequency = freq;
  exitbbsvec.push_back(retbb);
  return;
}

void Riscv64CGFunc::HandleRetCleanup(NaryStmtNode * retnode) {
  if (!cg->GenLocalRC()) {
    // handle local rc is disabled.
    return;
  }

  Opcode ops[15] = { OP_label, OP_goto,      OP_brfalse,   OP_brtrue,  OP_return, OP_call,
                     OP_icall, OP_rangegoto, OP_javacatch, OP_javatry, OP_catch, OP_try,
                     OP_cpptry, OP_cppcatch, OP_endtry };
  set<Opcode> branchop(ops, ops + 11);

  // get cleanup intrinsic
  bool found = false;
  StmtNode *cleanupnode = retnode->GetPrev();
  while (cleanupnode) {
    if (branchop.find(cleanupnode->op) != branchop.end()) {
      if (cleanupnode->op == OP_call) {
        CallNode *callnode = static_cast<CallNode *>(cleanupnode);
        MIRFunction *fn = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callnode->puIdx);
        MIRSymbol *fsym = mirModule.CurFunction()->GetLocalOrGlobalSymbol(fn->stIdx, false);
        if (IsRtCleanupFunc(fsym->GetName())) {
          cleanupnode = cleanupnode->GetPrev();
          continue;
        } else {
          break;
        }
      } else {
        break;
      }
    }

    if (OP_intrinsiccall == cleanupnode->op) {
      IntrinsiccallNode *tempnode = static_cast<IntrinsiccallNode *>(cleanupnode);
      if (tempnode->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS ||
          tempnode->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS_SKIP) {
        if (GenRetCleanup(tempnode) && hasNonescapedVar) {
          GenNonescapedobjcleanup();
        }
        found = true;
        break;
      }
    }
    cleanupnode = cleanupnode->GetPrev();
  }

  if (!found) {
    MIRSymbol *retRef = nullptr;
    if (retnode->NumOpnds() != 0) {
      retRef = GetRetRefSymbol(static_cast<NaryStmtNode *>(retnode)->Opnd(0));
    }
    HandleRCCall(false, retRef);
  }
}

bool Riscv64CGFunc::GenRetCleanup(IntrinsiccallNode * cleanupnode) {
#undef CC_DEBUG_INFO

#ifdef CC_DEBUG_INFO
  LogInfo::MapleLogger() << "==============" << func->GetName() << "==============" << endl;
#endif

  int32 minbyteoffset = INT_MAX;
  int32 maxbyteoffset = 0;

  int skipindex = -1;
  MIRSymbol *skipsym = nullptr;
  int32 refsymnum = 0;
  if (cleanupnode->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS) {
    refsymnum = cleanupnode->NumOpnds();
    if (refsymnum < 1) {
      return true;
    }
  } else if (cleanupnode->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS_SKIP) {
    refsymnum = cleanupnode->NumOpnds();
    if (refsymnum < 2) {
      return true;
    }
    BaseNode *skipexpr = cleanupnode->Opnd(refsymnum - 1);

    CHECK_FATAL(skipexpr->op == OP_dread, "should be dread");
    DreadNode *refnode = static_cast<DreadNode *>(skipexpr);
    skipsym = mirModule.CurFunction()->GetLocalOrGlobalSymbol(refnode->stIdx);

    refsymnum -= 1;
  }

  // now compute the offset range
  Riscv64MemLayout *memlayout = static_cast<Riscv64MemLayout *>(this->memlayout);
  for (int i = 0; i < refsymnum; i++) {
    BaseNode *argexpr = cleanupnode->Opnd(i);
    CHECK_FATAL(argexpr->op == OP_dread, "should be dread");
    DreadNode *refnode = static_cast<DreadNode *>(argexpr);
    MIRSymbol *refsymbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(refnode->stIdx);
    CHECK_FATAL(memlayout->sym_alloc_table.size() > refsymbol->GetStIndex(), "access memlayout->sym_alloc_table failed");
    Riscv64SymbolAlloc *symloc =
      static_cast<Riscv64SymbolAlloc *>(memlayout->sym_alloc_table[refsymbol->GetStIndex()]);
    int32 tempoff = GetBaseOffset(symloc);
#ifdef CC_DEBUG_INFO
    LogInfo::MapleLogger() << "refsym " << refsymbol->GetName() << " offset " << tempoff << endl;
#endif
    minbyteoffset = minbyteoffset > tempoff ? tempoff : minbyteoffset;
    maxbyteoffset = maxbyteoffset < tempoff ? tempoff : maxbyteoffset;
  }

  // get the skip offset
  int32 skipoffset = -1;
  if (skipsym) {
    Riscv64SymbolAlloc *symloc = static_cast<Riscv64SymbolAlloc *>(memlayout->sym_alloc_table[skipsym->GetStIndex()]);
    skipoffset = GetBaseOffset(symloc);

#ifdef CC_DEBUG_INFO
    LogInfo::MapleLogger() << "skip " << skipsym->GetName() << " offset " << skipoffset << endl;
#endif

    skipindex = symloc->offset / kIntregBytelen;
  }

  // call runtime cleanup

  if (minbyteoffset < INT_MAX) {
    int32 reflocbase = memlayout->GetReflocbaseLoc();
    uint32 refNum = memlayout->GetSizeOfRefLocals() / kIntregBytelen;
    int32 reflocend = reflocbase + (refNum - 1) * kIntregBytelen;
    int32 realmin = minbyteoffset < reflocbase ? reflocbase : minbyteoffset;
    int32 realmax = maxbyteoffset > reflocend ? reflocend : maxbyteoffset;
#ifdef CC_DEBUG_INFO
    LogInfo::MapleLogger() << " realmin " << realmin << " realmax " << realmax << endl;
#endif
    if (realmax < realmin) {
      HandleRCCall(false, skipsym);
      return false;
    }

    // optimization for little slot cleanup
    if (realmax == realmin) {
      RegOperand *phyopnd = GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
      Operand *stackloc = CreateStkTopOpnd(realmin, SIZEOFPTR * BITS_PER_BYTE);
      Insn *ldInsn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(64, PTY_a64), phyopnd, stackloc);
      curbb->AppendInsn(ldInsn);

      Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
      srcOpnds->PushOpnd(phyopnd);
      MIRSymbol *callsym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
      std::string funcname(GetIntrinsicFuncName(INTRN_MCCDecRef));
      callsym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));
      callsym->storageClass = kScText;
      callsym->sKind = kStFunc;

      Operand *targetopnd = GetOrCreateFuncNameOpnd(callsym);
      Insn *callInsn = cg->BuildInstruction<Riscv64cleancallInsn>(MOP_xbl, targetopnd, srcOpnds);
      static_cast<Riscv64cleancallInsn *>(callInsn)->ref_skipindex = skipindex;
      curbb->AppendInsn(callInsn);

      curbb->SetKind(BB::kBBCall);
      // because of return stmt is often the last stmt
      curbb->frequency = frequency;
      BB *newbb = CreateNewBB();
      curbb->AppendBB(newbb);
      curbb = newbb;

      return true;
    }
    Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);

    Riscv64ImmOperand *beginopnd = CreateImmOperand(realmin, 64, true);

    regno_t vreg0no = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
    RegOperand *vreg0 = CreateVirtualRegisterOperand(vreg0no);
    RegOperand *fpopnd = GetOrCreateStackBaseRegOperand();
    SelectAdd(vreg0, fpopnd, beginopnd, PTY_i64);

    Riscv64RegOperand *parmregopnd = GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
    srcOpnds->PushOpnd(parmregopnd);
    SelectCopy(parmregopnd, PTY_a64, vreg0, PTY_a64);

    uint32 realRefNum = (realmax - realmin) / kIntregBytelen + 1;

    Riscv64ImmOperand *countopnd = CreateImmOperand(realRefNum, 64, true);

    parmregopnd = GetOrCreatePhysicalRegisterOperand(R1, 64, kRegTyInt);
    srcOpnds->PushOpnd(parmregopnd);
    SelectCopyImm(parmregopnd, countopnd, PTY_i64);

    MIRSymbol *funcsym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
    if (skipsym && skipoffset >= realmin && skipoffset <= realmax) {
      // call cleanupskip
      uint32 stoffset = (skipoffset - realmin) / kIntregBytelen;
      Riscv64ImmOperand *retLoc = CreateImmOperand(stoffset, 64, true);

      parmregopnd = GetOrCreatePhysicalRegisterOperand(R2, 64, kRegTyInt);
      srcOpnds->PushOpnd(parmregopnd);
      SelectCopyImm(parmregopnd, retLoc, PTY_i64);

      std::string funcname(GetIntrinsicFuncName(INTRN_MCCCleanupLocalStackRefSkipNaiveRCFast));
      funcsym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));
#ifdef CC_DEBUG_INFO
      LogInfo::MapleLogger() << "num " << real_ref_num << " skip loc " << stoffset << endl;
#endif
    } else {
      // call cleanup
      std::string funcname(GetIntrinsicFuncName(INTRN_MCCCleanupLocalStackRefNaiveRCFast));
      funcsym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));
#ifdef CC_DEBUG_INFO
      LogInfo::MapleLogger() << "num " << real_ref_num << endl;
#endif
    }

    funcsym->storageClass = kScText;
    funcsym->sKind = kStFunc;

    Operand *targetopnd = GetOrCreateFuncNameOpnd(funcsym);
    Insn *callInsn = cg->BuildInstruction<Riscv64cleancallInsn>(MOP_xbl, targetopnd, srcOpnds);
    static_cast<Riscv64cleancallInsn *>(callInsn)->ref_skipindex = skipindex;
    curbb->AppendInsn(callInsn);

    curbb->SetKind(BB::kBBCall);
    curbb->frequency = frequency;
    BB *newbb = CreateNewBB();
    curbb->AppendBB(newbb);
    curbb = newbb;
  }
  return true;
}

void Riscv64CGFunc::GenNonescapedobjcleanup() {
  // Cleanup non-escaped objects that have been allocated on stack
  for (size_t i = 1; i < func->symTab->GetSymbolTableSize(); i++) {
    MIRSymbol *sym = func->symTab->GetSymbolFromStIdx(i);
    if (!sym || !sym->GetType() || sym->GetType()->typeKind != kTypeClass) {
      continue;
    }

    Riscv64SymbolAlloc *symloc = static_cast<Riscv64SymbolAlloc *>(memlayout->sym_alloc_table[sym->GetStIndex()]);
    Riscv64ImmOperand *offsetopnd = CreateImmOperand(GetBaseOffset(symloc), 64, true);
    regno_t vreg0no = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
    RegOperand *vreg0 = CreateVirtualRegisterOperand(vreg0no);
    RegOperand *fpopnd = GetOrCreateStackBaseRegOperand();
    SelectAdd(vreg0, fpopnd, offsetopnd, PTY_i64);

    Riscv64RegOperand *parmregopnd = GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
    Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
    srcOpnds->PushOpnd(parmregopnd);
    SelectCopy(parmregopnd, PTY_a64, vreg0, PTY_a64);

    MIRSymbol *callsym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
    callsym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(GetIntrinsicFuncName(INTRN_MCCCleanupNonEscapedVar)));
    callsym->storageClass = kScText;
    callsym->sKind = kStFunc;
    Operand *targetopnd = GetOrCreateFuncNameOpnd(callsym);
    Insn *callInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xbl, targetopnd, srcOpnds);
    curbb->AppendInsn(callInsn);
    curbb->SetKind(BB::kBBCall);
    // Stmts are not all handled yet.
    if (frequency != 0) {
      curbb->frequency = frequency;
    }
    BB *newbb = CreateNewBB();
    newbb->frequency = curbb->frequency;
    curbb->AppendBB(newbb);
    curbb = newbb;
  }
}

void Riscv64CGFunc::HandleRCCall(bool begin, MIRSymbol *retRef) {
  if (!cg->GenLocalRC()) {
    // handle local rc is disabled.
    return;
  }

  if (!begin) {
    GenNonescapedobjcleanup();
  }
  Riscv64MemLayout *memlayout = static_cast<Riscv64MemLayout *>(this->memlayout);

  uint32 refNum = memlayout->GetSizeOfRefLocals() / kIntregBytelen;
  if (!refNum) {
    if (begin) {
      GenerateYieldpoint(curbb);
      yieldPointInsn = curbb->lastinsn;
    }
    return;
  }

  // no MCC_CleanupLocalStackRefSkip when ret_ref is the only ref symbol
  if (refNum == 1 && retRef != nullptr) {
    if (begin) {
      GenerateYieldpoint(curbb);
      yieldPointInsn = curbb->lastinsn;
    }
    return;
  }
  CHECK_FATAL(refNum < 0xFFFF, "not enough room for size.");
  int32 reflocbase = memlayout->GetReflocbaseLoc();
  CHECK_FATAL(reflocbase >= 0 && reflocbase < 0xFFFF, "not enough room for offset.");
  if (begin && refNum <= 6 && (reflocbase + 8 * (refNum - 1)) < STP_LDP_IMM64_UPPER_BOUND) {
    int ind = 0;
    while (ind < refNum) {
      int offset = memlayout->GetReflocbaseLoc() + 8 * ind;
      Operand *zeroop = GetZeroOpnd(64);
      Operand *stackloc = CreateStkTopOpnd(offset, SIZEOFPTR * BITS_PER_BYTE);
      Insn *setinc = cg->BuildInstruction<Riscv64Insn>(MOP_xstr, zeroop, stackloc);
      curbb->AppendInsn(setinc);
      ind++;
    }
    // Insert Yield Point just after localrefvar are initialized.
    GenerateYieldpoint(curbb);
    yieldPointInsn = curbb->lastinsn;
    return;
  }

  if (refNum == 1 && !begin && !retRef) {
    RegOperand *phyopnd = GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
    Operand *stackloc = CreateStkTopOpnd(memlayout->GetReflocbaseLoc(), SIZEOFPTR * BITS_PER_BYTE);
    Insn *ldInsn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(64, PTY_a64), phyopnd, stackloc);
    curbb->AppendInsn(ldInsn);

    Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
    srcOpnds->PushOpnd(phyopnd);
    MIRSymbol *callsym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
    std::string funcname(GetIntrinsicFuncName(INTRN_MCCDecRef));
    callsym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));
    callsym->storageClass = kScText;
    callsym->sKind = kStFunc;

    Operand *targetopnd = GetOrCreateFuncNameOpnd(callsym);
    Insn *callInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xbl, targetopnd, srcOpnds);
    curbb->AppendInsn(callInsn);

    curbb->SetKind(BB::kBBCall);
    if (frequency != 0) {
      curbb->frequency = frequency;
    }
    BB *newbb = CreateNewBB();
    // After handle all stmts.
    if (frequency == 0) {
      newbb->frequency = curbb->frequency;
    }
    curbb->AppendBB(newbb);
    curbb = newbb;
    return;
  }

  if (refNum == 2 && !begin && retRef) {
    Riscv64SymbolAlloc *symloc = static_cast<Riscv64SymbolAlloc *>(memlayout->sym_alloc_table[retRef->GetStIndex()]);
    int32 stoffset = symloc->offset / kIntregBytelen;
    RegOperand *phyopnd = GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
    Operand *stackloc = nullptr;
    if (stoffset == 0) {
      // just have to Dec the next one
      stackloc = CreateStkTopOpnd(memlayout->GetReflocbaseLoc() + kIntregBytelen, SIZEOFPTR * BITS_PER_BYTE);
    } else {
      // just have to Dec the current one
      stackloc = CreateStkTopOpnd(memlayout->GetReflocbaseLoc(), SIZEOFPTR * BITS_PER_BYTE);
    }
    Insn *ldInsn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(64, PTY_a64), phyopnd, stackloc);
    curbb->AppendInsn(ldInsn);

    Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
    srcOpnds->PushOpnd(phyopnd);
    MIRSymbol *callsym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
    std::string funcname(GetIntrinsicFuncName(INTRN_MCCDecRef));
    callsym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));
    callsym->storageClass = kScText;
    callsym->sKind = kStFunc;

    Operand *targetopnd = GetOrCreateFuncNameOpnd(callsym);
    Insn *callInsn = cg->BuildInstruction<Riscv64cleancallInsn>(MOP_xbl, targetopnd, srcOpnds);
    static_cast<Riscv64cleancallInsn *>(callInsn)->ref_skipindex = stoffset;
    curbb->AppendInsn(callInsn);

    curbb->SetKind(BB::kBBCall);
    if (frequency != 0) {
      curbb->frequency = frequency;
    }
    BB *newbb = CreateNewBB();
    if (frequency == 0) {
      newbb->frequency = curbb->frequency;
    }
    curbb->AppendBB(newbb);
    curbb = newbb;
    return;
  }

  bool needSkip = false;
  Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);

  Riscv64ImmOperand *beginopnd = CreateImmOperand(memlayout->GetReflocbaseLoc(), 64, true);
  Riscv64ImmOperand *countopnd = CreateImmOperand(refNum, 64, true);
  int refskipindex = -1;
  if (!begin && retRef) {
    Riscv64SymbolAlloc *symloc = static_cast<Riscv64SymbolAlloc *>(memlayout->sym_alloc_table[retRef->GetStIndex()]);
    int32 stoffset = symloc->offset / kIntregBytelen;
    refskipindex = stoffset;
    if (stoffset == 0) {
      // ret_ref at begin
      beginopnd = CreateImmOperand(memlayout->GetReflocbaseLoc() + kIntregBytelen, 64, true);
      countopnd = CreateImmOperand(refNum - 1, 64, true);
    } else if (stoffset == (refNum - 1)) {
      // ret_ref at end
      countopnd = CreateImmOperand(refNum - 1, 64, true);
    } else {
      needSkip = true;
    }
  }

  regno_t vreg0no = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
  RegOperand *vreg0 = CreateVirtualRegisterOperand(vreg0no);
  RegOperand *fpopnd = GetOrCreateStackBaseRegOperand();
  SelectAdd(vreg0, fpopnd, beginopnd, PTY_i64);

  Riscv64RegOperand *parmregopnd = GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
  srcOpnds->PushOpnd(parmregopnd);
  SelectCopy(parmregopnd, PTY_a64, vreg0, PTY_a64);

  regno_t vreg1no = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
  RegOperand *vreg1 = CreateVirtualRegisterOperand(vreg1no);
  SelectCopyImm(vreg1, countopnd, PTY_i64);

  parmregopnd = GetOrCreatePhysicalRegisterOperand(R1, 64, kRegTyInt);
  srcOpnds->PushOpnd(parmregopnd);
  SelectCopy(parmregopnd, PTY_a64, vreg1, PTY_a64);

  MIRSymbol *sym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  if (begin) {
    std::string funcname(GetIntrinsicFuncName(INTRN_MCCInitializeLocalStackRef));
    sym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));
    CG_ASSERT(countopnd->GetValue() > 0, "refCount should be greater than 0.");
    refCount = countopnd->GetValue();
    beginOffset = beginopnd->GetValue();
  } else if (!needSkip) {
    std::string funcname(GetIntrinsicFuncName(INTRN_MCCCleanupLocalStackRefNaiveRCFast));
    sym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));
  } else {
    CHECK_FATAL(retRef->GetStIndex() < memlayout->sym_alloc_table.size(),
           "index out of range in Riscv64CGFunc::HandleRCCall");
    Riscv64SymbolAlloc *symloc = static_cast<Riscv64SymbolAlloc *>(memlayout->sym_alloc_table[retRef->GetStIndex()]);
    int32 stoffset = symloc->offset / kIntregBytelen;
    Riscv64ImmOperand *retLoc = CreateImmOperand(stoffset, 64, true);

    regno_t vreg2no = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
    RegOperand *vreg2 = CreateVirtualRegisterOperand(vreg2no);
    SelectCopyImm(vreg2, retLoc, PTY_i64);

    parmregopnd = GetOrCreatePhysicalRegisterOperand(R2, 64, kRegTyInt);
    srcOpnds->PushOpnd(parmregopnd);
    SelectCopy(parmregopnd, PTY_a64, vreg2, PTY_a64);

    std::string funcname(GetIntrinsicFuncName(INTRN_MCCCleanupLocalStackRefSkipNaiveRCFast));
    sym->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));
  }
  sym->storageClass = kScText;
  sym->sKind = kStFunc;

  Operand *targetopnd = GetOrCreateFuncNameOpnd(sym);
  Insn *callInsn = cg->BuildInstruction<Riscv64cleancallInsn>(MOP_xbl, targetopnd, srcOpnds);
  static_cast<Riscv64cleancallInsn *>(callInsn)->ref_skipindex = refskipindex;
  curbb->AppendInsn(callInsn);
  if (frequency != 0) {
    curbb->frequency = frequency;
  }
  curbb->SetKind(BB::kBBCall);
  BB *newbb = CreateNewBB();
  if (frequency == 0) {
    newbb->frequency = curbb->frequency;
  }
  curbb->AppendBB(newbb);
  curbb = newbb;
  if (begin) {
    // Insert Yield Point just after localrefvar are initialized.
    GenerateYieldpoint(curbb);
    yieldPointInsn = curbb->lastinsn;
  }
  return;
}

void Riscv64CGFunc::CreateCallStructParamFieldPassByStack(int32 symSize, MIRSymbol *sym, uint32 symOffset,
                                                     RegOperand *addropnd, int32 baseOffset) {
  MemOperand *ldmopnd, *stmopnd;
  regno_t vreg;
  RegOperand *vreg2;

  if (symSize == 0) {
    return;
  }

  uint32 dataSize = GetPrimTypeSize(PTY_a64);
  uint32 dataSizeBits = dataSize * BITS_PER_BYTE;

  CG_ASSERT(symSize <= 8, "ParamField size should not be > 8");
  if (sym) {
    ldmopnd = GetOrCreateMemOpnd(sym, symOffset, dataSizeBits);
  } else {
    ldmopnd = GetOrCreateMemOpnd(64, addropnd, nullptr,
                  GetOrCreateOfstOpnd(symOffset, 32), static_cast<MIRSymbol *>(nullptr));
  }

  vreg = New_V_Reg(kRegTyInt, dataSize);
  vreg2 = CreateVirtualRegisterOperand(vreg);
  curbb->AppendInsn(
      cg->BuildInstruction<Riscv64Insn>(PickLdInsn(dataSizeBits, PTY_i64), vreg2, ldmopnd)
  );

  stmopnd = CreateMemOpnd(RSP, (baseOffset + 0), dataSizeBits);
  curbb->AppendInsn(
      cg->BuildInstruction<Riscv64Insn>(PickStInsn(dataSizeBits, PTY_i64), vreg2, stmopnd)
  );
}

void Riscv64CGFunc::CreateCallStructParamPassByStack(int32 symSize, MIRSymbol *sym,
                                                     RegOperand *addropnd, int32 baseOffset) {
  MemOperand *ldmopnd, *stmopnd;
  regno_t vreg;
  RegOperand *vreg2;

  if (symSize == 0) {
    return;
  }

  uint32 dataSize = GetPrimTypeSize(PTY_a64);
  uint32 dataSizeBits = dataSize * BITS_PER_BYTE;

  uint32 numRegNeeded = (symSize <= 8) ? 1 : 2;
  for (int j = 0; j < numRegNeeded; j++) {
    if (sym) {
      ldmopnd = GetOrCreateMemOpnd(sym, (j * SIZEOFPTR), dataSizeBits);
    } else {
      ldmopnd = GetOrCreateMemOpnd(64, addropnd, nullptr,
                    GetOrCreateOfstOpnd(j * SIZEOFPTR, 32), static_cast<MIRSymbol *>(nullptr));
    }

    vreg = New_V_Reg(kRegTyInt, dataSize);
    vreg2 = CreateVirtualRegisterOperand(vreg);
    curbb->AppendInsn(
        cg->BuildInstruction<Riscv64Insn>(PickLdInsn(dataSizeBits, PTY_i64), vreg2, ldmopnd)
    );

    stmopnd = CreateMemOpnd(RSP, (baseOffset + (j * SIZEOFPTR)), dataSizeBits);
    curbb->AppendInsn(
        cg->BuildInstruction<Riscv64Insn>(PickStInsn(dataSizeBits, PTY_i64), vreg2, stmopnd)
    );
  }
}

Riscv64RegOperand *Riscv64CGFunc::GenUnalignedSymCallStructParam(Riscv64reg_t reg, MIRSymbol *sym, uint32 memOffset, PrimType pty, RegOperand *addropnd) {
  int32 ldOffStart, ldOffDec, shftStart, ldSize;
  if (pty == PTY_u8) {
    ldOffStart = 7;
    ldOffDec = 1;
    shftStart = 56;
    ldSize = 8;
  } else if (pty == PTY_u16) {
    ldOffStart = 6;
    ldOffDec = 2;
    shftStart = 48;
    ldSize = 16;
  } else if (pty == PTY_u32) {
    ldOffStart = 4;
    ldOffDec = 4;
    shftStart = 32;
    ldSize = 32;
  } else {
    CHECK_FATAL(0, "Unknown primtype");
  }

  Riscv64RegOperand *parmOpnd;
  MemOperand *mopnd;
  RegOperand *ropnd, *rdst;
  parmOpnd = GetOrCreatePhysicalRegisterOperand(reg, 32, kRegTyInt);
  for (int32 ldOff = ldOffStart, shft = shftStart; ldOff >= 0;
       ldOff -= ldOffDec, shft -= ldSize) {
    ropnd = GetOrCreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 4));
    if (sym) {
      mopnd = GetOrCreateMemOpnd(sym, memOffset + ldOff, 32);
    } else {
      Riscv64OfstOperand *offopnd = GetOrCreateOfstOpnd(ldOff, 32);
      mopnd = GetOrCreateMemOpnd(32, addropnd,
                                 nullptr, offopnd, static_cast<MIRSymbol *>(nullptr));
    }
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickLdInsn(ldSize, PTY_u8), ropnd, mopnd));
    if (ldOff == 0) {
      rdst = ropnd;
    } else {
      Riscv64ImmOperand *shiftImm;
      shiftImm = CreateImmOperand(shft, 64, false);
      if (ldOff == ldOffStart) {
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, parmOpnd, ropnd, shiftImm));
        rdst = parmOpnd;
      } else {
        rdst = GetOrCreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 4));
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, rdst, ropnd, shiftImm));
      }
    }
    if (ldOff != ldOffStart) {
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xiorrrr, parmOpnd, parmOpnd, rdst));
    }
  }

  return parmOpnd;
}

void Riscv64CGFunc::CreateCallStructParamPassByReg(Riscv64reg_t reg, MemOperand *mopnd,
                                    Riscv64ListOperand *srcopnds, CSR_call_info_t &ci,
                                    MIRSymbol *sym, int32 memOffset, fpParamState state, RegOperand *addropnd) {
  Riscv64RegOperand *parmOpnd;
  OfstOperand *offsetOpnd = static_cast<OfstOperand *>(mopnd->GetOffsetOperand());
  int64 val = offsetOpnd->GetValue();
  char handleUnaligned = 0;
  if (val > 256) { // mem offset is limited if access is unaligned
    if (val & 0x1) {
      handleUnaligned = 1;
    } else if (val & 0x3) {
      handleUnaligned = 2;
    } else if (val & 0x7) {
      handleUnaligned = 4;
    }
  }
  if (handleUnaligned == 1) {
    /*    ldb  v1, [offset + 7]
     *    lsl  parmreg, v1, 56
     *    ldh  v2, [offset + 6]
     *    lsl  v3, v2, 48
     *    orr  parmreg, parmreg, v3
     *    ...
     *    ldh  v4, [offset + 1]
     *    lsl  v5, v4, 8
     *    orr  parmreg, parmreg, v5
     *    ldh  v6, [offset + 0]
     *    orr  parmreg, parmreg, v6
     */
    parmOpnd = GenUnalignedSymCallStructParam(reg, sym, memOffset, PTY_u8, addropnd);
  } else if (handleUnaligned == 2) {
    /*    ldh  v1, [offset + 6]
     *    lsl  parmreg, v1, 48
     *    ldh  v2, [offset + 4]
     *    lsl  v3, v2, 32
     *    orr  parmreg, parmreg, v3
     *    ldh  v4, [offset + 2]
     *    lsl  v5, v4, 16
     *    orr  parmreg, parmreg, v5
     *    ldh  v6, [offset + 0]
     *    orr  parmreg, parmreg, v6
     */
    parmOpnd = GenUnalignedSymCallStructParam(reg, sym, memOffset, PTY_u16, addropnd);
  } else if (handleUnaligned == 4) {
    /*    ldw  v1, [offset + 4]
     *    lsl  parmreg, v1, 32
     *    ldw  v2, [offset + 0]
     *    orr  parmreg, parmreg, v2
     */
    parmOpnd = GenUnalignedSymCallStructParam(reg, sym, memOffset, PTY_u32, addropnd);
  } else {
    uint32 dataSizeBits = 0;
    PrimType pt = PTY_void;
    parmOpnd = nullptr;
    if (state == kNotFp) {
      parmOpnd = GetOrCreatePhysicalRegisterOperand(reg, 64, kRegTyInt);
      dataSizeBits = GetPrimTypeSize(PTY_i64) * BITS_PER_BYTE;
      pt = PTY_i64;
    } else if (state == kIsFp32bit) {
      parmOpnd = GetOrCreatePhysicalRegisterOperand(reg, 32, kRegTyFloat);
      dataSizeBits = GetPrimTypeSize(PTY_f32) * BITS_PER_BYTE;
      pt = PTY_f32;
    } else if (state == kIsFp64bit) {
      parmOpnd = GetOrCreatePhysicalRegisterOperand(reg, 64, kRegTyFloat);
      dataSizeBits = GetPrimTypeSize(PTY_f64) * BITS_PER_BYTE;
      pt = PTY_f64;
    } else {
      CG_ASSERT(0, "CreateCallStructParamPassByReg: Unknown state");
    }
    if (sym && sym->storageClass == kScFormal && memOffset >= 0) {
      RegOperand *base = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickLdInsn(64, PTY_i64), base, mopnd));
      uint32 msize = (state == kIsFp32bit) ? 32 : 64;
      MemOperand *dataopnd = CreateMemOpnd(base, memOffset, msize);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickLdInsn(dataSizeBits, pt), parmOpnd, dataopnd));
    } else {
      curbb->AppendInsn(
        cg->BuildInstruction<Riscv64Insn>(
            PickLdInsn(dataSizeBits, pt),
            parmOpnd, mopnd)
        );
    }
  }

  if (ci.store_insertion_point == nullptr) {
    ci.store_insertion_point = curbb->lastinsn;
  }
  srcopnds->PushOpnd(parmOpnd);
  CallerSavedRegHandler::CsrBitsetSet(ci.regs_used,
      Riscv64CallerSavedRegHandler::Reg2BitPos(reg));
}

RegOperand *Riscv64CGFunc::CreateCallStructParamMemcpy(MIRSymbol *sym, RegOperand *addropnd, uint32 structSize, int32 copyOffset, int32 fromOffset) {
  vector<Operand *> opndvec;
  RegOperand *vreg;

  vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
  opndvec.push_back(vreg);  // result

  RegOperand *parmOpnd = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
  RegOperand *spReg = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
  Riscv64ImmOperand *offsetOpnd = CreateImmOperand(copyOffset, 64, false);
  SelectAdd(parmOpnd, spReg, offsetOpnd, PTY_a64);
  opndvec.push_back(parmOpnd);  // param 0

  if (sym) {
    vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
    if (sym->storageClass == kScGlobal || sym->storageClass == kScExtern) {
      StImmOperand *stopnd = CreateStImmOperand(sym, 0, 0);
      Riscv64RegOperand *staddropnd = static_cast<Riscv64RegOperand *>(CreateRegisterOperandOfType(PTY_u64));
      SelectAddrof(staddropnd, stopnd);
      opndvec.push_back(staddropnd);  // param 1
    } else if (sym->storageClass == kScAuto || sym->storageClass == kScFormal) {
      Riscv64SymbolAlloc *symloc = static_cast<Riscv64SymbolAlloc *>(memlayout->sym_alloc_table[sym->GetStIndex()]);
      Riscv64RegOperand *baseOpnd = static_cast<Riscv64RegOperand *>(GetBaseReg(symloc));
      int32 stoffset = GetBaseOffset(symloc);
      Riscv64ImmOperand *offsetOpnd = CreateImmOperand(stoffset, 64, false);
      curbb->AppendInsn(
          cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, vreg, baseOpnd, offsetOpnd));
      if (sym->storageClass == kScFormal) {
        MemOperand *ldmopnd = GetOrCreateMemOpnd(64, vreg, nullptr,
                    GetOrCreateOfstOpnd(0, 32), static_cast<MIRSymbol *>(nullptr));
        RegOperand *tmpreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickLdInsn(64, PTY_a64), tmpreg, ldmopnd));
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, vreg, tmpreg, CreateImmOperand(fromOffset, 64, false)));
      }
      opndvec.push_back(vreg);  // param 1
    } else if (sym->storageClass == kScPstatic || sym->storageClass == kScFstatic) {
      if (sym->sKind == kStConst) {
        CHECK_FATAL(0,"Unsupported sym const for struct param");
      }
      StImmOperand *stopnd = CreateStImmOperand(sym, 0, 0);
      Riscv64RegOperand *staddropnd = static_cast<Riscv64RegOperand *>(CreateRegisterOperandOfType(PTY_u64));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_adrp, staddropnd, stopnd));
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_adrpl12, staddropnd, staddropnd, stopnd));
      opndvec.push_back(staddropnd);  // param 1
    } else {
      CHECK_FATAL(0,"Unsupported sym for struct param");
    }
  } else {
    opndvec.push_back(addropnd);  // param 1
  }

  vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
  Riscv64ImmOperand *sizeOpnd = CreateImmOperand(structSize, 64, false);
  curbb->AppendInsn(
        cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, vreg, sizeOpnd));
  opndvec.push_back(vreg);  // param 2

  SelectLibCall("memcpy", opndvec, PTY_a64, PTY_a64);

  return parmOpnd;
}

Riscv64RegOperand *Riscv64CGFunc::CreateCallStructParamCopyToStack(uint32 numMemOp, MIRSymbol *sym,
                                      RegOperand *addropnd, int32 copyOffset, int32 fromOffset, PLocInfo *ploc) {
  Riscv64reg_t reg = ploc->reg0;
  uint32 dataSize = GetPrimTypeSize(PTY_a64);
  uint32 dataSizeBits = dataSize * BITS_PER_BYTE;
  // Create the struct copies.
  MemOperand *ldmopnd, *stmopnd;
  RegOperand *vreg;
  for (int j = 0; j < numMemOp; j++) {
    if (sym) {
      if (sym->storageClass == kScFormal) {
        ldmopnd = GetOrCreateMemOpnd(sym, 0, dataSizeBits);
        vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, dataSize));
        Insn *ldInsn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(dataSizeBits, PTY_i64), vreg, ldmopnd);
        curbb->AppendInsn(ldInsn);
        ldmopnd = GetOrCreateMemOpnd(64, vreg, nullptr,
                    GetOrCreateOfstOpnd(j * SIZEOFPTR + fromOffset, 32), static_cast<MIRSymbol *>(nullptr));
      } else {
        ldmopnd = GetOrCreateMemOpnd(sym, (j * SIZEOFPTR) + fromOffset, dataSizeBits);
      }
    } else {
      ldmopnd = GetOrCreateMemOpnd(64, addropnd, nullptr,
                    GetOrCreateOfstOpnd(j * SIZEOFPTR + fromOffset, 32), static_cast<MIRSymbol *>(nullptr));
    }
    vreg = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, dataSize));
    Insn *ldInsn = cg->BuildInstruction<Riscv64Insn>(PickLdInsn(dataSizeBits, PTY_i64), vreg, ldmopnd);
    curbb->AppendInsn(ldInsn);
    if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(ldmopnd), 64)) {
      ldmopnd = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(ldmopnd), 64, Riscv64reg_t::kRinvalid, ldInsn);
      ldInsn->opnds[1] = ldmopnd;
    }

    stmopnd = CreateMemOpnd(RSP, (copyOffset + (j * SIZEOFPTR)), dataSizeBits);
    curbb->AppendInsn(
        cg->BuildInstruction<Riscv64Insn>(PickStInsn(dataSizeBits, PTY_i64), vreg, stmopnd)
    );
  }
  // Create the copy address parameter for the struct
  Riscv64RegOperand *parmOpnd;
  Riscv64ImmOperand *offset = CreateImmOperand(copyOffset, 64, false);
  if (reg != kRinvalid) {
    // if a parameter register still available
    parmOpnd = GetOrCreatePhysicalRegisterOperand(reg, 64, kRegTyInt);
    RegOperand *fpopnd = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
    SelectAdd(parmOpnd, fpopnd, offset, PTY_a64);
  } else {
    // no more parameter register available, push address to stack
    RegOperand *fpopnd = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
    RegOperand *res = CreateRegisterOperandOfType(PTY_u64);
    SelectAdd(res, fpopnd, offset, PTY_u64);
    stmopnd = CreateMemOpnd(RSP, ploc->memoffset, dataSizeBits);
    curbb->AppendInsn(
        cg->BuildInstruction<Riscv64Insn>(PickStInsn(dataSizeBits, PTY_i64), res, stmopnd));
    return nullptr;
  }

  return parmOpnd;
}

// Determine if the caller is a vararg call or not
bool Riscv64CGFunc::CallIsVararg(StmtNode *narynode, uint32 &namedFormals, BB *bb) {
  bool varargFunc = false;

  if (CallNode *callNode = dynamic_cast<CallNode *>(narynode)) {
    MIRFunction *func = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callNode->puIdx);
    varargFunc = CLANG && func->GetAttr(FUNCATTR_varargs) ? true : false;
    namedFormals = func->formalDefVec.size();
  } else if (IcallNode *icallNode = dynamic_cast<IcallNode *>(narynode)) {
    BaseNode *fNode = icallNode->Opnd(0);
    MIRFuncType *fType = nullptr;
    MIRPtrType *pType = nullptr;
    if (fNode->op == OP_dread) {
      DreadNode *dNode = static_cast<DreadNode*>(fNode);
      MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dNode->stIdx);
      pType = static_cast<MIRPtrType*>(symbol->GetType());
      MIRType *ty = pType;
      if (dNode->fieldID != 0) {
        CG_ASSERT(ty->typeKind == kTypeStruct || ty->typeKind == kTypeClass, "");
        FieldPair thepair;
        if (ty->typeKind == kTypeStruct) {
          thepair = static_cast<MIRStructType *>(ty)->TraverseToField(dNode->fieldID);
        } else {
          thepair = static_cast<MIRClassType *>(ty)->TraverseToField(dNode->fieldID);
        }
        pType = static_cast<MIRPtrType*>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first));
      }
      fType = static_cast<MIRFuncType*>(pType->GetPointedType());
      varargFunc = fType->isVarArgs;
      namedFormals = fType->paramTypeList.size();
    } else if (fNode->op == OP_iread) {
      IreadNode *iNode = static_cast<IreadNode *>(fNode);
      MIRPtrType *pointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iNode->tyIdx));
      MIRType *pointedType = pointerty->GetPointedType();
      if (iNode->fieldID != 0) {
        pointedType = static_cast<MIRStructType *>(pointedType)->GetFieldType(iNode->fieldID);
      }
      if (pointedType->typeKind == kTypeFunction) {
        fType = static_cast<MIRFuncType *>(pointedType);
      } else if (pointedType->typeKind == kTypePointer) {
        fType = dynamic_cast<MIRFuncType *>(static_cast<MIRPtrType *>(pointedType)->GetPointedType());
        CHECK_FATAL(fType != nullptr, "Cannot determine if icall is variadic");
      }
      varargFunc = fType->isVarArgs;
      namedFormals = fType->paramTypeList.size();
    } else if (fNode->op == OP_select) {
      TernaryNode *sNode = static_cast<TernaryNode *>(fNode);
      BaseNode *expr = sNode->Opnd(1);
      // both function ptrs under select should have the same signature, chk op1 only
      AddroffuncNode *afNode = static_cast<AddroffuncNode*>(expr);
      MIRFunction *func = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(afNode->puIdx);
      varargFunc = CLANG && func->GetAttr(FUNCATTR_varargs);
      namedFormals = func->formalDefVec.size();
      if (bb) {
        bb->SetKind(BB::kBBCall);
      }
    } else if (fNode->op == OP_regread) {
      RegreadNode *rNode = static_cast<RegreadNode *>(fNode);
      PregIdx pregidx = rNode->regIdx;
      MIRPreg *preg = func->pregTab->PregFromPregIdx(pregidx);
      MIRPtrType *pType = dynamic_cast<MIRPtrType*>(preg->mirType);
      if (!pType) {
        return false;
      }
      MIRFuncType *fType = dynamic_cast<MIRFuncType*>(pType->GetPointedType());
      if (!fType) {
        CG_ASSERT(0, "cannot process this icall");
      };
      varargFunc = fType->isVarArgs;
      namedFormals = fType->paramTypeList.size();
    } else {
      CG_ASSERT(0, "cannot process this icall");
    }
  }
  return varargFunc;
}

/*
   SelectParmList generates an instrunction for each of the parameters
   to load the parameter value into the corresponding register.
   We return a list of registers to the call instruction because
   they may be needed in the register allocation phase.
 */
void Riscv64CGFunc::SelectParmList(StmtNode * narynode, Riscv64ListOperand * srcopnds, CSR_call_info_t & ci,
                                   bool iscallnative) {
  ParmLocator parmlocator(becommon);
  PLocInfo ploc;
  int32 i = 0;
  if (narynode->op == OP_icall || iscallnative) {
    i++;
  }
  MIRSymbol *fsym = nullptr;
  CallNode *callnode = dynamic_cast<CallNode *>(narynode);
  bool islockcall = false;
  if (callnode) {
    MIRFunction *fn = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callnode->puIdx);
    fsym = mirModule.CurFunction()->GetLocalOrGlobalSymbol(fn->stIdx, false);
  }

  if (fsym && IsRtLockCall(fsym->GetName())) {
    islockcall = true;
  }

  uint32 retSize = 0;
  int32 structCopyOffset = GetMaxParamStackSize() - GetStructCopySize();
  RegOperand *bigParm = nullptr;
  vector<int32> offsetList;
  MIRFunction *callfunc = nullptr;
  bool varargFunc = false;
  uint32 namedFormals = 0;
  if (becommon.mirModule.IsCModule()) {
    // Preprocess for converting big struct param to memcpy.
    int32 pi = i;
    for (; pi < narynode->NumOpnds(); pi++) {
      bigParm = nullptr;
      BaseNode *argexpr = narynode->Opnd(pi);
      PrimType ptype = argexpr->primType;
      if (ptype == PTY_agg) {
        int32 symSize;
        RegOperand *addropnd = nullptr;
        MIRSymbol *sym = nullptr;
        int32 rhsoffset = 0;
        if (argexpr->op == OP_iread) {
          IreadNode *iread = static_cast<IreadNode *>(argexpr);
          MIRPtrType *pointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iread->tyIdx));
          MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
          if (iread->fieldID != 0) {
            MIRStructType *rhsstructty = static_cast<MIRStructType *>(ty);
            FieldPair thepair = rhsstructty->TraverseToField(iread->fieldID);
            ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
            rhsoffset = becommon.GetFieldOffset(rhsstructty, iread->fieldID).first;
          }
          symSize = becommon.type_size_table.at(ty->tyIdx.GetIdx());
          if (symSize > kParmMemcpySize) {
            addropnd = static_cast<RegOperand *>(HandleExpr(iread, iread->Opnd(0)));
            addropnd = LoadIntoRegister(addropnd, iread->Opnd(0)->primType);
            curbb->AppendInsn(
                cg->BuildInstruction<Riscv64Insn>(MOP_xaddrri12, addropnd, addropnd, CreateImmOperand(rhsoffset, 64, false)));
          }
        } else if (argexpr->op == OP_dread) {
          DreadNode *dread = static_cast<DreadNode *>(argexpr);
          sym = becommon.mirModule.CurFunction()->GetLocalOrGlobalSymbol(dread->stIdx);
          MIRType *ty = sym->GetType();
          if (dread->fieldID != 0) {
            MIRStructType *structty = static_cast<MIRStructType *>(ty);
            FieldPair thepair = structty->TraverseToField(dread->fieldID);
            ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
            rhsoffset = becommon.GetFieldOffset(structty, dread->fieldID).first;
          }
          symSize = becommon.type_size_table.at(ty->tyIdx.GetIdx());
        } else {
          CHECK_FATAL(0, "call param is agg but not dread or iread");
        }
        if (symSize > kParmMemcpySize) {
          bigParm = CreateCallStructParamMemcpy(sym, addropnd, symSize, structCopyOffset, rhsoffset);
          offsetList.push_back(structCopyOffset);
          structCopyOffset += RoundUp(symSize, SIZEOFPTR);

          if (ci.store_insertion_point == nullptr) {
            ci.store_insertion_point = curbb->lastinsn;
          }
          CallerSavedRegHandler::CsrBitsetSet(ci.regs_used, Riscv64CallerSavedRegHandler::Reg2BitPos(R0));
          CallerSavedRegHandler::CsrBitsetSet(ci.regs_used, Riscv64CallerSavedRegHandler::Reg2BitPos(R1));
          CallerSavedRegHandler::CsrBitsetSet(ci.regs_used, Riscv64CallerSavedRegHandler::Reg2BitPos(R2));
          BB *oldBB = curbb;
          curbb = StartNewBB(narynode);
          SplitCallBB(oldBB);
          curbb->SetKind(BB::kBBCall);
        }
      }
    }
    // Determine if call is variadic
    varargFunc = CallIsVararg(narynode, namedFormals, curbb);
    if (callnode) {
      callfunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callnode->puIdx);
      TyIdx retIdx = callfunc->GetReturnTyIdx();
      retSize = becommon.type_size_table[retIdx.GetIdx()];
      if (retSize == 0) {
        auto funcIt = becommon.funcReturnType.find(callfunc);
        if (funcIt != becommon.funcReturnType.end()) {
          retSize = becommon.type_size_table[funcIt->second.GetIdx()];
        }
      }
    } else {
      IcallNode *icallnode = dynamic_cast<IcallNode *>(narynode);
      if (icallnode) {
        CallReturnVector *p2nrets = &icallnode->returnValues;
        if (p2nrets->size() == 1) {
          StIdx stIdx = (*p2nrets)[0].first;
          MIRSymbol *sym = becommon.mirModule.CurFunction()->symTab->GetSymbolFromStIdx(stIdx.Idx());
          if (sym) {
            MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
            retSize = becommon.type_size_table[sym->tyIdx.GetIdx()];
          }
        }
      }
    }
  }
  string fname = fsym ? fsym->GetName() : "icall";
  //LogInfo::MapleLogger() << fname << " varargFunc: " << varargFunc << endl;
  for (int32 pidx = 0, pnum = 0; i < narynode->NumOpnds(); i++, pnum++) {
    MIRType *ty = nullptr;
    BaseNode *argexpr = narynode->Opnd(i);
    PrimType ptype = argexpr->primType;
    CG_ASSERT(ptype != PTY_void, "");
    bool variadArg = varargFunc && (pnum+1 > namedFormals);
    if (ptype != PTY_agg) {
      ty = GlobalTables::GetTypeTable().typeTable[static_cast<uint32>(ptype)];
      RegOperand *expregopnd = nullptr;
      if (!(i == 1 && islockcall)) {
        Operand *opnd = HandleExpr(narynode, argexpr);
        if (!opnd->IsRegister()) {
          opnd = LoadIntoRegister(opnd, ptype);
        }
        expregopnd = static_cast<RegOperand *>(opnd);
      }

      parmlocator.LocateNextParm(ty, ploc, pnum == 0 && retSize > 16, variadArg);
      if (ploc.reg0 != 0) {  // load to the register
        Riscv64RegOperand *parmregopnd = nullptr;
        if (islockcall && i == 1) {
          ConstvalNode *soff = static_cast<ConstvalNode *>(argexpr);
          int32 stackoff = static_cast<MIRIntConst *>(soff->constVal)->GetValueUnderType();

          parmregopnd = GetOrCreatePhysicalRegisterOperand(ploc.reg0, 64, kRegTyInt);
          Riscv64SymbolAlloc symalloc;
          Riscv64MemLayout *memlayout = static_cast<Riscv64MemLayout *>(this->memlayout);
          symalloc.mem_segment = &memlayout->seg_lockobjslot;
          symalloc.offset = 0;

          uint32 slotbase = GetBaseOffset(&symalloc);
          uint32 slotoffset = slotbase + memlayout->lockinfosize + (stackoff - 1) * memlayout->lockslotsize;
          Riscv64ImmOperand *offoper = CreateImmOperand(slotoffset, 64, true);
          RegOperand *fpopnd = GetOrCreateStackBaseRegOperand();
          SelectAdd(parmregopnd, fpopnd, offoper, PTY_a64);
        } else {
          CHECK_FATAL(expregopnd, "null ptr check");
          PrimType ptype2 = ptype;
          if (ploc.reg0 <= R17) {  // fp passed in intreg
            ptype2 = (ptype == PTY_f32) ? PTY_i32 : ((ptype == PTY_f64) ? PTY_i64 : ptype);
          }
          parmregopnd = GetOrCreatePhysicalRegisterOperand(ploc.reg0, expregopnd->size_, GetRegTyFromPrimTyRiscv64(ptype2));
          SelectCopy(parmregopnd, ptype2, expregopnd, ptype);
        }

        if (ci.store_insertion_point == nullptr) {
          ci.store_insertion_point = curbb->lastinsn;
        }
        srcopnds->PushOpnd(parmregopnd);
        CallerSavedRegHandler::CsrBitsetSet(ci.regs_used, Riscv64CallerSavedRegHandler::Reg2BitPos(ploc.reg0));
      } else {  // store to the memory segment for stack-passsed arguments
        Operand *actmemopnd = CreateMemOpnd(RSP, ploc.memoffset, GetPrimTypeBitSize(ptype));
        curbb->AppendInsn(
          cg->BuildInstruction<Riscv64Insn>(PickStInsn(GetPrimTypeBitSize(ptype), ptype), expregopnd, actmemopnd));
      }
      CG_ASSERT(ploc.reg1 == 0, "SelectCall NYI");
    } else {
      // The param passing stack layout:
      //    ------------------------------  previous call frame
      //       incoming call param stk loc
      //    ------------------------------  previous call frame - #param_regs_size
      //       locals, spills, ...
      //    ------------------------------  %fp + 8
      //       %fp and %lr
      //    ------------------------------  %fp = %sp + max call params + struct copy offset
      //      struct copy area
      //    ------------------------------  %sp + maximum call parameter offset
      //      out flow params beyong %x7
      //    ------------------------------  %sp + 0
      uint32 dataSize = GetPrimTypeSize(PTY_a64);
      uint32 dataSizeBits = dataSize * BITS_PER_BYTE;
      if (argexpr->op == OP_iread) {
        IreadNode *iread = static_cast<IreadNode *>(argexpr);
        MIRPtrType *pointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iread->tyIdx));
        ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
        int32 rhsoffset = 0;
        if (iread->fieldID != 0) {
          MIRStructType *rhsstructty = static_cast<MIRStructType *>(ty);
          FieldPair thepair = rhsstructty->TraverseToField(iread->fieldID);
          ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
          rhsoffset = becommon.GetFieldOffset(rhsstructty, iread->fieldID).first;
        }
        int32 symSize = becommon.type_size_table.at(ty->tyIdx.GetIdx());
        parmlocator.LocateNextParm(ty, ploc, false, variadArg);
        if (symSize <= 16) {
          RegOperand *addropnd = static_cast<RegOperand *>(HandleExpr(iread, iread->Opnd(0)));
          addropnd = LoadIntoRegister(addropnd, iread->Opnd(0)->primType);
          if (ploc.reg0 == 0) {
            // No param regs available, pass on stack.
            CreateCallStructParamPassByStack(symSize, nullptr, addropnd, ploc.memoffset);
          } else {
            if (symSize > 8 && ploc.reg1 == kRinvalid) {
              // half of the agg is to be passed in reg below, the 2nd half on stack
              CreateCallStructParamFieldPassByStack(symSize-8, nullptr, 8, addropnd, ploc.memoffset);
            }
            // pass by param regs.
            fpParamState state = kStateUnknown;
            uint32 msize = 0;
            if (ploc.reg0 < V0) {
              state = kNotFp;
              msize = 64;
            } else {
              if (ploc.rsize0 <= 4) {
                state = kIsFp32bit;
                msize = 32;
              } else {
                state = kIsFp64bit;
                msize = 64;
              }
            }
            Riscv64OfstOperand *offopnd = GetOrCreateOfstOpnd(rhsoffset, 32);
            MemOperand *mopnd;
            mopnd = GetOrCreateMemOpnd(msize, addropnd,
                                       nullptr, offopnd, static_cast<MIRSymbol *>(nullptr));
            CreateCallStructParamPassByReg(ploc.reg0, mopnd, srcopnds, ci, (MIRSymbol*)nullptr, 0, state, addropnd);
            if (ploc.reg1) {
              if (ploc.reg0 < V0) {
                state = kNotFp;
                msize = 64;
              } else {
                if (ploc.rsize0 <= 4) {
                  state = kIsFp32bit;
                  msize = 32;
                } else {
                  state = kIsFp64bit;
                  msize = 64;
                }
              }
              offopnd = GetOrCreateOfstOpnd(ploc.rsize0 + rhsoffset, 32);
              mopnd = GetOrCreateMemOpnd(msize, addropnd,
                                         nullptr, offopnd, static_cast<MIRSymbol *>(nullptr));
              CreateCallStructParamPassByReg(ploc.reg1, mopnd, srcopnds, ci, (MIRSymbol*)nullptr, 0, state, addropnd);
            }
          }
        } else if (symSize > kParmMemcpySize) {
          // This has been dealt with at the beginning of this func with memcpy
          RegOperand *spReg = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
          Riscv64ImmOperand *offsetOpnd = CreateImmOperand(offsetList[pidx], 64, false);
          pidx++;
          if (ploc.reg0) {
            RegOperand *res = GetOrCreatePhysicalRegisterOperand(ploc.reg0, 64, kRegTyInt);
            SelectAdd(res, spReg, offsetOpnd, PTY_a64);
            srcopnds->PushOpnd(res);
          } else {
            regno_t vregno = New_V_Reg(kRegTyInt, 8);
            RegOperand *parmOpnd = CreateVirtualRegisterOperand(vregno);
            SelectAdd(parmOpnd, spReg, offsetOpnd, PTY_a64);
            MemOperand *stmopnd = CreateMemOpnd(RSP, ploc.memoffset, dataSizeBits);
            curbb->AppendInsn(
                cg->BuildInstruction<Riscv64Insn>(PickStInsn(dataSizeBits, PTY_i64), parmOpnd, stmopnd));
          }
        } else {
          // Pass larger sized struct on stack.
          RegOperand *addropnd = static_cast<RegOperand *>(HandleExpr(iread, iread->Opnd(0)));
          addropnd = LoadIntoRegister(addropnd, iread->Opnd(0)->primType);
          Riscv64RegOperand *parmOpnd;
          uint32 numMemOp = RoundUp(symSize, SIZEOFPTR) / SIZEOFPTR; // round up
          parmOpnd = CreateCallStructParamCopyToStack(numMemOp, nullptr, addropnd, structCopyOffset, rhsoffset, &ploc);
          structCopyOffset += (numMemOp * SIZEOFPTR);
          if (ci.store_insertion_point == nullptr) {
            ci.store_insertion_point = curbb->lastinsn;
          }
          if (ploc.reg0) {
            srcopnds->PushOpnd(parmOpnd);
            CallerSavedRegHandler::CsrBitsetSet(ci.regs_used, Riscv64CallerSavedRegHandler::Reg2BitPos(ploc.reg0));
          }
        }
        continue;
      }
      CHECK_FATAL((argexpr->op == OP_dread), "call param is agg but not dread or iread");
      DreadNode *dread = static_cast<DreadNode *>(argexpr);
      MIRSymbol *sym = becommon.mirModule.CurFunction()->GetLocalOrGlobalSymbol(dread->stIdx);
      ty = sym->GetType();
      int32 rhsoffset = 0;
      if (dread->fieldID != 0) {
        MIRStructType *structty = static_cast<MIRStructType *>(ty);
        FieldPair thepair = structty->TraverseToField(dread->fieldID);
        ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
        rhsoffset = becommon.GetFieldOffset(structty, dread->fieldID).first;
      }
      int32 symSize = becommon.type_size_table.at(ty->tyIdx.GetIdx());
      parmlocator.LocateNextParm(ty, ploc, false, variadArg);
      if (symSize <= 16) {
        // in two param regs if possible
        // If struct is <= 8 bytes, then it fits into one param reg.
        // If struct is <= 16 bytes, then it fits into two param regs.
        // Otherwise, it goes onto the stack.
        // If the number of available param reg is less than what is
        // needed to fit the entire struct into them, then the param
        // reg is skipped and the struct goes onto the stack.
        // Example 1.
        //  struct size == 8 bytes.
        //  param regs a0 to a6 are used.
        //  struct is passed in a7.
        // Example 2.
        //  struct is 16 bytes.
        //  param regs a0 to a5 are used.
        //  struct is passed in a6 and a7.
        // Example 3.
        //  struct is 16 bytes.
        //  param regs a0 to a6 are used.  a7 alone is not enough to pass the struct.
        //  Other other half is passed on the stack.
        if (ploc.reg0 == 0) {
          // No param regs available, pass on stack.
          CreateCallStructParamPassByStack(symSize, sym, nullptr, ploc.memoffset);
        } else {
          if (symSize > 8 && ploc.reg1 == kRinvalid) {
            // half of the agg is to be passed in reg below, the 2nd half on stack
            CreateCallStructParamFieldPassByStack(symSize-8, sym, 8, nullptr, ploc.memoffset);
          }
          // pass by param regs.
          fpParamState state = kStateUnknown;
          if (ploc.reg0 < V0) {
            state = kNotFp;
            dataSizeBits = 64;
          } else {
            if (ploc.rsize0 <= 4) {
              state = kIsFp32bit;
              dataSizeBits = 32;
            } else {
              state = kIsFp64bit;
              dataSizeBits = 64;
            }
          }
          MemOperand *mopnd;
          int32 dataOffset;
          if (sym->storageClass == kScFormal && dread->fieldID > 0) {
            mopnd = GetOrCreateMemOpnd(sym, 0, dataSizeBits);
            dataOffset = rhsoffset;
          } else {
            mopnd = GetOrCreateMemOpnd(sym, rhsoffset, dataSizeBits);
            dataOffset = -1;
          }
          CreateCallStructParamPassByReg(ploc.reg0, mopnd, srcopnds, ci, sym, dataOffset, state);
          if (ploc.reg1) {
            if (ploc.reg1 < V0) {
              state = kNotFp;
              dataSizeBits = 64;
            } else {
              if (ploc.rsize1 <= 4) {
                state = kIsFp32bit;
                dataSizeBits = 32;
              } else {
                state = kIsFp64bit;
                dataSizeBits = 64;
              }
            }
            int32 symOffset;
            if (sym->storageClass == kScFormal && dread->fieldID > 0) {
              symOffset = 0;
              dataOffset = SIZEOFPTR + rhsoffset;
            } else if (ploc.reg1 >= V0) {
              if (ploc.rsize1 <= 4) {
                symOffset = 4 + rhsoffset;
              } else {
                symOffset = 8 + rhsoffset;
              }
              dataOffset = -1;
            } else {
              symOffset = SIZEOFPTR + rhsoffset;
              dataOffset = -1;
            }
            mopnd = GetOrCreateMemOpnd(sym, symOffset, dataSizeBits);
            CreateCallStructParamPassByReg(ploc.reg1, mopnd, srcopnds, ci, sym, dataOffset, state);
          }
        }
      } else if (symSize > kParmMemcpySize) {
        // This has been dealt with at the beginning of this func with memcpy
        RegOperand *spReg = GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
        Riscv64ImmOperand *offsetOpnd = CreateImmOperand(offsetList[pidx], 64, false);
        pidx++;
        if (ploc.reg0) {
          RegOperand *res = GetOrCreatePhysicalRegisterOperand(ploc.reg0, 64, kRegTyInt);
          SelectAdd(res, spReg, offsetOpnd, PTY_a64);
          srcopnds->PushOpnd(res);
        } else {
          regno_t vregno = New_V_Reg(kRegTyInt, 8);
          RegOperand *parmOpnd = CreateVirtualRegisterOperand(vregno);
          SelectAdd(parmOpnd, spReg, offsetOpnd, PTY_a64);
          MemOperand *stmopnd = CreateMemOpnd(RSP, ploc.memoffset, dataSizeBits);
          curbb->AppendInsn(
              cg->BuildInstruction<Riscv64Insn>(PickStInsn(dataSizeBits, PTY_i64), parmOpnd, stmopnd));
        }
      } else {
        // Pass larger sized struct on stack.
        // Need to copy the entire structure onto the stack.
        // The pointer to the starting address of the copied struct is then
        // used as the parameter for the struct.
        // This pointer is passed as the next parameter.
        // Example 1:
        //  struct is 23 bytes.
        //  param regs x0 to x5 are used.
        //  First around up 23 to 24, so 3 of 8-byte slots.
        //  Copy struct to a created space on the stack.
        //  Pointer of copied struct is passed in x6.
        // Example 2:
        //  struct is 25 bytes.
        //  param regs x0 to x7 are used.
        //  First around up 25 to 32, so 4 of 8-byte slots.
        //  Copy struct to a created space on the stack.
        //  Pointer of copied struct is passed on stack as the 9th parameter.

        Riscv64RegOperand *parmOpnd;
        uint32 numMemOp = RoundUp(symSize, SIZEOFPTR) / SIZEOFPTR; // round up
        parmOpnd = CreateCallStructParamCopyToStack(numMemOp, sym, nullptr, structCopyOffset, rhsoffset, &ploc);
        structCopyOffset += (numMemOp * SIZEOFPTR);
        if (ci.store_insertion_point == nullptr) {
          ci.store_insertion_point = curbb->lastinsn;
        }
        if (ploc.reg0) {
          srcopnds->PushOpnd(parmOpnd);
          CallerSavedRegHandler::CsrBitsetSet(ci.regs_used, Riscv64CallerSavedRegHandler::Reg2BitPos(ploc.reg0));
        }
      }
    }
  }
}

void Riscv64CGFunc::SelectCall(CallNode * callnode) {
  MIRFunction *fn = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callnode->puIdx);
  MIRSymbol *fsym = mirModule.CurFunction()->GetLocalOrGlobalSymbol(fn->stIdx, false);
  MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fn->GetReturnTyIdx());

  CSR_call_info_t ci;
  CG_ASSERT(ci.store_insertion_point == nullptr && ci.load_insertion_point == nullptr, "");

  if (cg->GenerateVerboseAsm()) {
    string *comment = new string(fsym->GetName());
    const char *str = comment->c_str();
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand(str)));
  }

  Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);

  bool callnative = false;
  if (IsRtNativeCall(fsym->GetName())) {
    callnative = true;
  }

  SelectParmList(callnode, srcOpnds, ci, callnative);
  if (callnative) {
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand("call native func")));

    BaseNode *funcargexpr = callnode->Opnd(0);
    PrimType ptype = funcargexpr->primType;
    Operand *funcopnd = HandleExpr(callnode, funcargexpr);
    Riscv64RegOperand *livein = GetOrCreatePhysicalRegisterOperand(RSP, 64, kRegTyInt);
    SelectCopy(livein, ptype, funcopnd, ptype);
    callnativemap[curbb->lastinsn] = nullptr;
  }

  Operand *targetopnd = GetOrCreateFuncNameOpnd(fsym);
  Insn *callInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xbl, targetopnd, srcOpnds);
  if (rettype) {
    callInsn->SetRetSize(rettype->GetSize());
    callInsn->SetRetSignType(IsUnsignedInteger(rettype->GetPrimType()));
  }
  curbb->AppendInsn(callInsn);

  func->SetHasCall();
  if (IsThrowOrNonReturnFunc(fsym->GetName())) {
    callInsn->is_throw = true;
    curbb->SetKind(BB::kBBThrow);
  }

  if (ci.store_insertion_point == nullptr) {
    ci.store_insertion_point = callInsn;
  }
  if (ci.load_insertion_point == nullptr) {
    ci.load_insertion_point = callInsn;
  }

  // We collect the return value info in SelectRegread() where we process kSregRetval0

  call_info_map.insert(pair<Insn *, CSR_call_info_t>(callInsn, ci));
}

void Riscv64CGFunc::SelectIcall(IcallNode * icallnode, Operand * fptropnd) {
  CSR_call_info_t ci;
  CG_ASSERT(ci.store_insertion_point == nullptr && ci.load_insertion_point == nullptr, "");

  Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
  SelectParmList(icallnode, srcOpnds, ci);

  if (fptropnd->op_kind_ != Operand::Opd_Register) {
    PrimType ty = icallnode->Opnd(0)->primType;
    fptropnd = SelectCopy(fptropnd, ty, ty);
  }

  RegOperand *regopnd = dynamic_cast<RegOperand *>(fptropnd);
  CG_ASSERT(regopnd, "SelectIcall: function pointer not RegOperand");
  Insn *callInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xblr, regopnd, srcOpnds);

  MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(icallnode->retTyIdx);
  if (rettype) {
    callInsn->SetRetSize(rettype->GetSize());
    callInsn->SetRetSignType(IsUnsignedInteger(rettype->GetPrimType()));
  }

  curbb->AppendInsn(callInsn);
  CG_ASSERT(curbb->GetKind() == BB::kBBCall && curbb->lastinsn->IsCall(), "");
  func->SetHasCall();

  if (ci.store_insertion_point == nullptr) {
    ci.store_insertion_point = callInsn;
  }
  if (ci.load_insertion_point == nullptr) {
    ci.load_insertion_point = callInsn;
  }

  // We collect the return value info in SelectRegread() where we process kSregRetval0

  call_info_map.insert(pair<Insn *, CSR_call_info_t>(callInsn, ci));
}

void Riscv64CGFunc::HandleJavaCatch() {
  if (g->optim_level == 2) {
    regno_t regno = ujavaCatch.regno_javaCatch;
    RegOperand *vregOpnd = GetOrCreateVirtualRegisterOperand(regno);
    curbb->AppendInsn(
      cg->BuildInstruction<Riscv64Insn>(MOP_xmovrr, vregOpnd, GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt)));
  } else
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickStInsn(ujavaCatch.opnd_javaCatch->GetSize(), PTY_a64),
                                                        GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt),
                                                        ujavaCatch.opnd_javaCatch));

  return;
}

bool Riscv64CGFunc::CanBBThrow(BB * bb) {
  FOR_BB_INSNS(insn, bb) {
    if (insn->CanThrow()) {
      if (insn->GetMachineOpcode() == MOP_xbl) {
        FuncNameOperand *target = dynamic_cast<FuncNameOperand *>(insn->GetCallTargetOperand());
        if (target) {
          MIRSymbol *funcst = target->GetFunctionSymbol();
          if (CanFuncThrow(funcst->GetName())) {
            return true;
          }
          continue;
        }
      } else if (insn->IsMemAccess()) {
        Operand *opnd = insn->GetMemOpnd();
        if (opnd->IsMemoryAccessOperand()) {
          MemOperand *memopnd = static_cast<MemOperand *>(opnd);
          Operand *base = memopnd->GetBaseRegister();
          if (base) {
            RegOperand *ropnd = static_cast<RegOperand *>(base);
            if (ropnd->IsPhysicalRegister() &&
                (ropnd->GetRegisterNumber() == RFP || ropnd->GetRegisterNumber() == RSP)) {
              continue;
            }
          }
        }
      }
      return true;
    }
  }
  return false;
}

void Riscv64CGFunc::SelectMembar(StmtNode * membar) {
  switch (membar->op) {
    case OP_membaracquire:
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_dmb_ishld));
      break;
    case OP_membarrelease:
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_dmb_ishst));
      break;
    case OP_membarstoreload:
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_dmb_ish));
      break;
    case OP_membarstorestore:
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_dmb_ishst));
      break;

    default:
      CG_ASSERT(false, "NYI");
      break;
  }
}

void Riscv64CGFunc::SelectComment(CommentNode * comment) {
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand(comment->comment)));
}

void Riscv64CGFunc::SelectReturn(NaryStmtNode * stmt, Operand * opnd0) {
  ReturnMechanism retmech(GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->GetReturnTyIdx()), becommon);
  Riscv64RegOperand *retopnd = nullptr;
  if (retmech.regcount > 0) {
    if (RegOperand *regopnd = dynamic_cast<RegOperand *>(opnd0)) {
      if (regopnd->GetRegisterNumber() != retmech.reg0) {
        if (Riscv64isa::IsGPRegister(retmech.reg0)) {
          retRegType = kRegTyInt;
        } else {
          retRegType = kRegTyFloat;
        }
        retopnd =
          GetOrCreatePhysicalRegisterOperand(retmech.reg0, regopnd->size_, GetRegTyFromPrimTyRiscv64(retmech.ptype0));
        SelectCopy(retopnd, retmech.ptype0, regopnd, retmech.ptype0);
      }
    } else if (Riscv64MemOperand *memopnd = dynamic_cast<Riscv64MemOperand *>(opnd0)) {
      if (IsPrimitivePureScalar(retmech.ptype0)) {
        retRegType = kRegTyInt;
      } else {
        retRegType = kRegTyFloat;
      }
      retopnd = GetOrCreatePhysicalRegisterOperand(retmech.reg0, GetPrimTypeBitSize(retmech.ptype0),
                                                   GetRegTyFromPrimTyRiscv64(retmech.ptype0));
      MOperator mop = PickLdInsn(memopnd->size_, retmech.ptype0);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(mop, retopnd, memopnd));
    } else if (opnd0->IsConstImmediate()) {
      if (IsPrimitivePureScalar(retmech.ptype0)) {
        retRegType = kRegTyInt;
      } else {
        retRegType = kRegTyFloat;
      }
      ImmOperand *immopnd = static_cast<ImmOperand *>(opnd0);
      retopnd = GetOrCreatePhysicalRegisterOperand(retmech.reg0, GetPrimTypeBitSize(retmech.ptype0),
                                                   GetRegTyFromPrimTyRiscv64(retmech.ptype0));
      SelectCopy(retopnd, retmech.ptype0, immopnd, retmech.ptype0);
    } else {
      CG_ASSERT(false, "nyi");
    }
  } else if (opnd0 != nullptr) {  // pass in memory
    CG_ASSERT(false, "SelectReturn: return in memory NYI");
  }
  exitbbsvec.push_back(curbb);
}

RegOperand *Riscv64CGFunc::GetOrCreateSpecialRegisterOperand(PregIdx sregidx, PrimType primType) {
  Riscv64reg_t reg = R0;
  switch (sregidx) {
    case kSregSp:
      reg = RSP;
      break;
    case kSregFp:
      reg = RFP;
      break;
    case kSregThrownval: { /* uses x0 == R0 */
      CG_ASSERT(ujavaCatch.regno_javaCatch > 0, "regno_javaCatch should greater than 0.");

      RegOperand *regOpnd = nullptr;
      if (g->optim_level < 2) {
        regOpnd = GetOrCreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
        curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(PickLdInsn(ujavaCatch.opnd_javaCatch->GetSize(), PTY_a64),
                                                            regOpnd, ujavaCatch.opnd_javaCatch));
      } else {
        regOpnd = GetOrCreateVirtualRegisterOperand(ujavaCatch.regno_javaCatch);
      }
      return regOpnd;
    }
    case kSregRetval0:
      if (!IsPrimitiveInteger(primType)) {
        reg = V0;
      }
      /* else (if-integer) uses x0 == R0 */
      break;
    case kSregMethodhdl:
      if (method_handle_vreg == regno_t(-1)) {
        method_handle_vreg = New_V_Reg(kRegTyInt, 8);
      }
      return GetOrCreateVirtualRegisterOperand(method_handle_vreg);
    default:
      CG_ASSERT(0, "Special pseudo registers NYI");
      break;
  }
  return GetOrCreatePhysicalRegisterOperand(reg, 64, kRegTyInt);
}

Riscv64RegOperand *Riscv64CGFunc::GetOrCreatePhysicalRegisterOperand(Riscv64reg_t regNo, uint8 size, RegType kind,
                                                                     uint32 flag, VectorType vecType, int vecPos) {
  if (size <= 32) {
    size = 32;
  } else if (size <= 64){
    size = 64;
  } else if (size <= 128) {
    size = 128;
  } else {
    CG_ASSERT(false, "NYI");
  }

  auto it = phy_reg_operand_table.find(Riscv64RegOperand(regNo, size, kind, flag, vecType, vecPos));
  if (it != phy_reg_operand_table.end()) {
    return it->second;
  }

  Riscv64RegOperand *o = memPool->New<Riscv64RegOperand>(regNo, size, kind, flag, vecType, vecPos);
  phy_reg_operand_table[*o] = o;
  return o;
}

LabelOperand *Riscv64CGFunc::GetOrCreateLabelOperand(LabelIdx labidx) {
  MapleMap<LabelIdx, LabelOperand *>::iterator it = hash_lblopnd_tb_.find(labidx);
  if (it != hash_lblopnd_tb_.end()) {
    return it->second;
  }
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  const char *funcName = MapleString(funcSt->GetName(), memPool).c_str();
  LabelOperand *res = memPool->New<LabelOperand>(funcName, labidx);
  hash_lblopnd_tb_[labidx] = res;
  return res;
}

LabelOperand *Riscv64CGFunc::CreateFuncLabelOperand(MIRSymbol * func) {
  const char *funcName = memPool->New<string>(func->GetName())->c_str();
  return memPool->New<FunctionLabelOperand>(funcName);
}

Riscv64OfstOperand *Riscv64CGFunc::GetOrCreateOfstOpnd(uint32 offset, uint32 size) {
  Riscv64OfstOperand tofstopnd(offset, size);
  auto it = hash_ofstopnd_tb_.find(tofstopnd);
  if (it != hash_ofstopnd_tb_.end()) {
    return it->second;
  }
  Riscv64OfstOperand *res = memPool->New<Riscv64OfstOperand>(offset, size);
  hash_ofstopnd_tb_[tofstopnd] = res;
  return res;
}

MemOperand *Riscv64CGFunc::GetOrCreateArgMemOpnd(MIRSymbol * symbol, int32 offset, int32 size) {
  MIRStorageClass storageClass = symbol->storageClass;

  Riscv64SymbolAlloc *symloc = static_cast<Riscv64SymbolAlloc *>(memlayout->sym_alloc_table[symbol->GetStIndex()]);
  CG_ASSERT(symloc, "sym loc should have been defined");
  // At this point, we don't know which registers the callee needs to save.
  CG_ASSERT((IsFPLRAddedToCalleeSavedList() || SizeOfCalleeSaved() == 0),
            "CalleeSaved won't be known until after Register Allocation");
  StIdx idx = symbol->GetStIdx();
  auto it = memopnds_requiring_offset_adjustment_.find(idx);
  CG_ASSERT((!IsFPLRAddedToCalleeSavedList() ||
             (it != memopnds_requiring_offset_adjustment_.end() ||
              storageClass == kScFormal)),
            "Memory operand of this symbol should have been added to the hash table");
  if (it != memopnds_requiring_offset_adjustment_.end()) {
    return it->second;
  }
  it = memopnds_for_stkpassed_arguments.find(idx);
  if (it != memopnds_for_stkpassed_arguments.end()) {
    return it->second;
  }

  RegOperand *tempreg = GetOrCreatePhysicalRegisterOperand(R9, SIZEOFPTR * 8, kRegTyInt);

  Riscv64RegOperand *baseOpnd = static_cast<Riscv64RegOperand *>(GetBaseReg(symloc));
  int32 stoffset = GetBaseOffset(symloc);
  int32 totalOffset = stoffset + offset;
  // needs a fresh copy of OfstOperand as we may adjust its offset at a later stage
  Riscv64MemOperand *res = nullptr;
  if (symloc->mem_segment->kind == kMsArgsStkpassed && Riscv64MemOperand::IsPIMMOffsetOutOfRange(totalOffset, size)) {
    Riscv64ImmOperand *offsetopran;
    offsetopran = CreateImmOperand(totalOffset, 64, true, true);
    SelectCopy(tempreg, PTY_i64, offsetopran, PTY_i64);
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, tempreg, tempreg, baseOpnd));
    Riscv64OfstOperand *offsetOpnd = memPool->New<Riscv64OfstOperand>(0, 64);
    res = memPool->New<Riscv64MemOperand>(size, tempreg,
                                      nullptr, offsetOpnd, symbol);
  } else if (symloc->mem_segment->kind == kMsReflocals && false) {
    Riscv64ImmOperand *offsetopran;
    auto iOpndIt = immopnds_requiring_offset_adjustment_for_refloc_.find(symloc);
    if (iOpndIt != immopnds_requiring_offset_adjustment_for_refloc_.end()) {
      offsetopran = (*iOpndIt).second;
    } else {
      offsetopran = CreateImmOperand(totalOffset, 64, true);
      immopnds_requiring_offset_adjustment_for_refloc_[symloc] = offsetopran;
    }

    SelectCopy(tempreg, PTY_i64, offsetopran, PTY_i64);
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, tempreg, tempreg, baseOpnd));
    Riscv64OfstOperand *offsetOpnd = memPool->New<Riscv64OfstOperand>(0, 64);
    res = memPool->New<Riscv64MemOperand>(size, tempreg,
                                      nullptr, offsetOpnd, symbol);

  } else {
    Riscv64OfstOperand *offsetOpnd = memPool->New<Riscv64OfstOperand>(totalOffset, 64);
    if (symloc->mem_segment->kind == kMsArgsStkpassed) {
      offsetOpnd->SetVary(true);
    }
    res = memPool->New<Riscv64MemOperand>(size, baseOpnd, nullptr,
                                      offsetOpnd, symbol);
    memopnds_requiring_offset_adjustment_[idx] = res;
  }
  return res;
}

MemOperand *Riscv64CGFunc::GetOrCreateMemOpnd(MIRSymbol * symbol, int32 offset, int32 size) {
  MIRStorageClass storageClass = symbol->storageClass;
  if (storageClass == kScAuto || storageClass == kScFormal) {
    Riscv64SymbolAlloc *symloc =
      static_cast<Riscv64SymbolAlloc *>(memlayout->sym_alloc_table.at(symbol->GetStIndex()));
    CG_ASSERT(symloc, "sym loc should have been defined");
    // At this point, we don't know which registers the callee needs to save.
    CG_ASSERT((IsFPLRAddedToCalleeSavedList() || SizeOfCalleeSaved() == 0),
              "CalleeSaved won't be known until after Register Allocation");
    StIdx idx = symbol->GetStIdx();
    auto it = memopnds_requiring_offset_adjustment_.find(idx);
    CG_ASSERT(
      (!IsFPLRAddedToCalleeSavedList() ||
       (it != memopnds_requiring_offset_adjustment_.end() ||
        storageClass == kScFormal)),
      "Memory operand of this symbol should have been added to the hash table");
    int32 stoffset = GetBaseOffset(symloc);
    if (it != memopnds_requiring_offset_adjustment_.end()) {
      if (mirModule.IsJavaModule()) {
        return it->second;
      } else {
        Operand *offopnd = (it->second)->GetOffset();
        if (((static_cast<Riscv64OfstOperand*>(offopnd))->GetOffsetValue() == (stoffset + offset)) &&
            (it->second->GetSize() == size)) {
          return it->second;
        }
      }
    }
    it = memopnds_for_stkpassed_arguments.find(idx);
    if (it != memopnds_for_stkpassed_arguments.end()) {
      return it->second;
    }

    Riscv64RegOperand *baseOpnd = static_cast<Riscv64RegOperand *>(GetBaseReg(symloc));
    int32 totalOffset = stoffset + offset;
    // needs a fresh copy of OfstOperand as we may adjust its offset at a later stage
    Riscv64MemOperand *res = nullptr;
    if (symloc->mem_segment->kind == kMsArgsStkpassed &&
        Riscv64MemOperand::IsPIMMOffsetOutOfRange(totalOffset, size)) {
      Riscv64ImmOperand *offsetopran;
      offsetopran = CreateImmOperand(totalOffset, 64, true, true);
      RegOperand *tempreg = SelectCopy(offsetopran, PTY_i64, PTY_i64);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, tempreg, tempreg, baseOpnd));
      Riscv64OfstOperand *offsetOpnd = memPool->New<Riscv64OfstOperand>(0, 64);
      res = memPool->New<Riscv64MemOperand>(size, tempreg,
                                        nullptr, offsetOpnd, symbol);
    } else if (symloc->mem_segment->kind == kMsReflocals && false) {
      Riscv64ImmOperand *offsetopran;
      auto iOpndIt = immopnds_requiring_offset_adjustment_for_refloc_.find(symloc);
      if (iOpndIt != immopnds_requiring_offset_adjustment_for_refloc_.end()) {
        offsetopran = (*iOpndIt).second;
      } else {
        offsetopran = CreateImmOperand(totalOffset, 64, true);
        immopnds_requiring_offset_adjustment_for_refloc_[symloc] = offsetopran;
      }
      RegOperand *tempreg = SelectCopy(offsetopran, PTY_i64, PTY_i64);
      curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, tempreg, tempreg, baseOpnd));
      Riscv64OfstOperand *offsetOpnd = memPool->New<Riscv64OfstOperand>(0, 64);
      res = memPool->New<Riscv64MemOperand>(size, tempreg,
                                        nullptr, offsetOpnd, symbol);
    } else {
      Riscv64OfstOperand *offsetOpnd = memPool->New<Riscv64OfstOperand>(totalOffset, 64);
      if (symloc->mem_segment->kind == kMsArgsStkpassed) {
        offsetOpnd->SetVary(true);
      }
      res = memPool->New<Riscv64MemOperand>(size, baseOpnd,
                                        nullptr, offsetOpnd, symbol);
      if (symbol->GetType()->typeKind != kTypeClass) {
        memopnds_requiring_offset_adjustment_[idx] = res;
      }
    }
    return res;
  } else if (storageClass == kScGlobal || storageClass == kScExtern ||
             storageClass == kScPstatic || storageClass == kScFstatic) {
    StImmOperand *stopnd = CreateStImmOperand(symbol, offset, 0);
    Riscv64RegOperand *staddropnd = static_cast<Riscv64RegOperand *>(CreateRegisterOperandOfType(PTY_u64));
    SelectAddrof(staddropnd, stopnd);
    /* Riscv64MemOperand::AddrMode_B_OI */
    /* Since adrp of the symbol already took care of the offset from the symbol base,
       there is no need to repeat the offset in the actual mem opnd.
     */
    return memPool->New<Riscv64MemOperand>(size, staddropnd,
                                       static_cast<Riscv64RegOperand *>(nullptr), GetOrCreateOfstOpnd(0, 32), nullptr);
  } else {
    CG_ASSERT(false, "NYI");
  }
  return nullptr;
}

Riscv64MemOperand *Riscv64CGFunc::GetOrCreateMemOpnd(int32 size, RegOperand * base, RegOperand * index, Operand * offset,
                                                     MIRSymbol * st) {
  Riscv64MemOperand tmemopnd(size, base, index, offset, st);
  auto it = hash_memopnd_tb_.find(tmemopnd);
  if (it != hash_memopnd_tb_.end()) {
    return it->second;
  }
  Riscv64MemOperand *res = memPool->New<Riscv64MemOperand>(tmemopnd);
  hash_memopnd_tb_[tmemopnd] = res;
  return res;
}

// offset: base offset from FP or SP
MemOperand *Riscv64CGFunc::CreateMemOpnd(RegOperand * baseOpnd, int32 offset, int32 datasize) {
  if (Riscv64MemOperand::IsPIMMOffsetOutOfRange(offset, datasize)) {
    Riscv64ImmOperand *offsetopran = CreateImmOperand(offset, SIZEOFPTR * 8, true, true);
    regno_t vreg = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
    RegOperand *tempreg = CreateVirtualRegisterOperand(vreg);
    SelectCopy(tempreg, PTY_i64, offsetopran, PTY_i64);
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, tempreg, tempreg, baseOpnd));
    Riscv64OfstOperand *offsetOpnd = memPool->New<Riscv64OfstOperand>(0, 64);
    return memPool->New<Riscv64MemOperand>(datasize, tempreg,
                                       nullptr, offsetOpnd, (MIRSymbol *)nullptr);
  } else if (!ImmOperand::IsInBitSizeRot(12, offset)) {
    Operand *resimmopnd = SelectCopy(CreateImmOperand(offset, 32, true), PTY_i32, PTY_i32);
    return memPool->New<Riscv64MemOperand>(datasize, baseOpnd,
                                       static_cast<Riscv64RegOperand *>(resimmopnd), nullptr, nullptr);
  } else {
    Riscv64OfstOperand *offsetOpnd = CreateOfstOpnd(offset, 32);
    return memPool->New<Riscv64MemOperand>(datasize, baseOpnd,
                                       nullptr, offsetOpnd, (MIRSymbol *)nullptr);
  }
}

// offset: base offset + #:lo12:Label+immediate
MemOperand *Riscv64CGFunc::CreateMemOpnd(RegOperand * baseOpnd, int32 offset, int32 datasize, MIRSymbol * sym) {
  Riscv64OfstOperand *offsetOpnd = CreateOfstOpnd(offset, 32);
  CG_ASSERT(ImmOperand::IsInBitSizeRot(12, offset), "");
  return memPool->New<Riscv64MemOperand>(datasize, baseOpnd,
                                     static_cast<Riscv64RegOperand *>(nullptr), offsetOpnd, sym);
}

RegOperand *Riscv64CGFunc::GenStructParamIndex(RegOperand *base, BaseNode *indexExpr, int shift) {
  RegOperand *index = static_cast<RegOperand *>(LoadIntoRegister(HandleExpr(indexExpr, indexExpr->Opnd(0)), PTY_a64));
  RegOperand *tmpopnd = CreateRegisterOperandOfType(PTY_a64);
  ImmOperand *imm = CreateImmOperand(PTY_a64, shift);
  SelectShift(tmpopnd, index, imm, kShiftLeft, PTY_a64);
  RegOperand *result = CreateRegisterOperandOfType(PTY_a64);
  SelectAdd(result, base, tmpopnd, PTY_a64);

  Riscv64OfstOperand *offopnd = CreateOfstOpnd(0, 32);
  Riscv64MemOperand *mo =
      GetOrCreateMemOpnd(SIZEOFPTR * BITS_PER_BYTE,
                         static_cast<RegOperand *>(result),
                         nullptr, offopnd, nullptr);
  regno_t vregno = New_V_Reg(kRegTyInt, GetPrimTypeSize(PTY_a64));
  RegOperand *structAddr = CreateVirtualRegisterOperand(vregno);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xldr, structAddr, mo));

  return structAddr;
}

// Create a memory operand with specified data type and memory ordering, making
// use of riscv64 extend register addressing mode when possible.
MemOperand *Riscv64CGFunc::CreateMemOpnd(PrimType ptype, BaseNode * parent, BaseNode * addrExpr, int offset,
                                         Riscv64isa::memory_ordering_t mo) {
  aggParamReg = nullptr;
  MemOperand *memopnd = nullptr;

#if 0
  if (mo == Riscv64isa::kMoNone && IsPrimitiveInteger(ptype) &&
      addrExpr->op == OP_add && offset == 0) {
    BaseNode *baseExpr = addrExpr->Opnd(0);
    BaseNode *addendExpr = addrExpr->Opnd(1);

    if (addendExpr->op == OP_mul) {
      BaseNode *indexExpr, *scaleExpr;
      indexExpr = addendExpr->Opnd(0);
      scaleExpr = addendExpr->Opnd(1);

      if (scaleExpr->op == OP_constval) {
        ConstvalNode *constvalnode = static_cast<ConstvalNode *>(scaleExpr);
        MIRIntConst *mirintconst = dynamic_cast<MIRIntConst *>(constvalnode->constVal);
        CHECK_FATAL(mirintconst != nullptr, "just checking");

        int scale = mirintconst->value;
        if (scale == GetPrimTypeSize(ptype)) {
          if (indexExpr->op == OP_cvt) {
            int shift = scale == 8 ? 3 : (scale == 4 ? 2 : (scale == 2 ? 1 : 0));
            RegOperand *base = static_cast<RegOperand *>(LoadIntoRegister(HandleExpr(addrExpr, baseExpr), PTY_a64));
            TypeCvtNode *typecvtnode = static_cast<TypeCvtNode *>(indexExpr);
            PrimType fromtype = typecvtnode->fromPrimType;
            PrimType totype = typecvtnode->primType;

            if (isAggParamInReg) {
              aggParamReg = GenStructParamIndex(base, indexExpr, shift);
              return nullptr;
            }

            if (fromtype == PTY_i32 && totype == PTY_a64) {
              RegOperand *index =
                static_cast<RegOperand *>(LoadIntoRegister(HandleExpr(indexExpr, indexExpr->Opnd(0)), PTY_i32));
              memopnd = GetOrCreateMemOpnd(Riscv64MemOperand::kAddrModeBOrX, GetPrimTypeBitSize(ptype), base, index,
                                           shift, true);
            } else if (fromtype == PTY_u32 && totype == PTY_a64) {
              RegOperand *index =
                static_cast<RegOperand *>(LoadIntoRegister(HandleExpr(indexExpr, indexExpr->Opnd(0)), PTY_u32));
              memopnd = GetOrCreateMemOpnd(Riscv64MemOperand::kAddrModeBOrX, GetPrimTypeBitSize(ptype), base, index,
                                           shift, false);
            }
          }
        }
      }
    }
  }
#endif

  if (memopnd == nullptr) {
    Operand *addropnd = HandleExpr(parent, addrExpr);
    addropnd = static_cast<RegOperand *>(LoadIntoRegister(addropnd, PTY_a64));
    Riscv64OfstOperand *offopnd = GetOrCreateOfstOpnd(offset, 32);
    memopnd = GetOrCreateMemOpnd(GetPrimTypeBitSize(ptype),
                                 static_cast<Riscv64RegOperand *>(addropnd), nullptr, offopnd, nullptr);
  }

  return memopnd;
}

Operand *Riscv64CGFunc::GetOrCreateFuncNameOpnd(MIRSymbol * symbol) {
  return memPool->New<FuncNameOperand>(symbol);
}

Operand *Riscv64CGFunc::GetOrCreatevaryreg() {
  // TODO : To use SP or FP in OffsetAdjustmentForFPLR() for the future, this register type
  // is not needed as IsVary() accomplish the same.
  if (vary_ == nullptr) {
    regno_t vRegNo = New_V_Reg(kRegTyVary, 8);
    vary_ = CreateVirtualRegisterOperand(vRegNo);
  }
  return vary_;
}

// the first operand in opndvec is return opnd
void Riscv64CGFunc::SelectLibCall(const char *name, vector<Operand *> &opndvec, PrimType primtype,
                                  PrimType retprmtype, bool is2ndret) {
  MIRSymbol *st = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  std::string funcname(name);
  st->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcname));
  st->storageClass = kScText;
  st->sKind = kStFunc;
  // setup the type of the callee function
  std::vector<TyIdx> vec;
  std::vector<TypeAttrs> vecAt;
  for (uint32 i = 1; i < opndvec.size(); i++) {
    vec.push_back(GlobalTables::GetTypeTable().typeTable[static_cast<int>(primtype)]->tyIdx);
    vecAt.push_back(TypeAttrs());
  }

  MIRType *rettype = GlobalTables::GetTypeTable().typeTable[static_cast<int>(primtype)];
  st->SetTyIdx(becommon.BeGetOrCreateFunctionType(rettype->tyIdx, vec, vecAt)->tyIdx);

  if (cg->GenerateVerboseAsm()) {
    string comment = "lib call : " + funcname;
    const char *str = comment.c_str();
    curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_comment, CreateCommentOperand(str)));
  }

  ParmLocator parmlocator(becommon);
  PLocInfo ploc;
  CG_ASSERT(primtype != PTY_void, "");
  // setup actual parameters
  Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
  for (uint32 i = 1; i < opndvec.size(); i++) {
    MIRType *ty;
    ty = GlobalTables::GetTypeTable().typeTable[static_cast<uint32>(primtype)];
    Operand *stOpnd = opndvec[i];
    if (stOpnd->op_kind_ != Operand::Opd_Register) {
      stOpnd = SelectCopy(stOpnd, primtype, primtype);
    }
    RegOperand *expregopnd = static_cast<RegOperand *>(stOpnd);
    parmlocator.LocateNextParm(ty, ploc);
    if (ploc.reg0 != 0) {  // load to the register
      Riscv64RegOperand *parmregopnd =
        GetOrCreatePhysicalRegisterOperand(ploc.reg0, expregopnd->size_, GetRegTyFromPrimTyRiscv64(primtype));
      SelectCopy(parmregopnd, primtype, expregopnd, primtype);
      srcOpnds->PushOpnd(parmregopnd);
    }
    CG_ASSERT(ploc.reg1 == 0, "SelectCall NYI");
  }

  Operand *targetopnd =
    GetOrCreateFuncNameOpnd(mirModule.CurFunction()->GetLocalOrGlobalSymbol(st->GetStIdx(), false));
  Insn *callInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xbl, targetopnd, srcOpnds);
  MIRType *callRettype = GlobalTables::GetTypeTable().typeTable[static_cast<int>(retprmtype)];
  if (callRettype) {
    callInsn->SetRetSize(callRettype->GetSize());
    callInsn->SetRetSignType(IsUnsignedInteger(rettype->GetPrimType()));
  }
  curbb->AppendInsn(callInsn);
  func->SetHasCall();
  // get return value
  Operand *opnd0 = opndvec[0];
  ReturnMechanism retmech(GlobalTables::GetTypeTable().typeTable.at(retprmtype), becommon);
  Riscv64RegOperand *retopnd = nullptr;
  if (retmech.regcount > 0) {
    if (RegOperand *regopnd = dynamic_cast<RegOperand *>(opnd0)) {
      Riscv64reg_t regnum = is2ndret ? retmech.reg1 : retmech.reg0;
      if (regopnd->GetRegisterNumber() != regnum) {
        retopnd = GetOrCreatePhysicalRegisterOperand(regnum, regopnd->GetSize(), GetRegTyFromPrimTyRiscv64(retprmtype));
        SelectCopy(opnd0, retprmtype, retopnd, retprmtype);
      }
    } else {
      CG_ASSERT(false, "nyi");
    }
  } else {
    CG_ASSERT(false, "should return from register");
  }
}

Operand *Riscv64CGFunc::GetBaseReg(Riscv64SymbolAlloc * symballoc) {
  MemSegmentKind sgkind = symballoc->mem_segment->kind;
  CG_ASSERT((sgkind == kMsArgsRegpassed || sgkind == kMsLocals || sgkind == kMsReflocals ||
             sgkind == kMsArgsToStkpass || sgkind == kMsArgsStkpassed),
            "NYI");

  if (sgkind == kMsArgsStkpassed) {
    return GetOrCreatevaryreg();
  }

  if (!fsp_) {
    fsp_ = GetOrCreatePhysicalRegisterOperand(UseFP() || HasVLAOrAlloca() ? RFP : RSP, SIZEOFPTR * 8, kRegTyInt);
  }
  return fsp_;
}

uint32 Riscv64CGFunc::GetBaseOffset(SymbolAlloc * sa) {
  Riscv64SymbolAlloc *symalloc = static_cast<Riscv64SymbolAlloc *>(sa);
  /* Call Frame layout of Riscv64
     Refer to V2 in riscv64_mem_layout.h.
     Do Not change this unless you know what you do
   */
  int sizeofFplr = 2 * kIntregBytelen;
  MemSegmentKind sgkind = symalloc->mem_segment->kind;
  Riscv64MemLayout *memlayout = static_cast<Riscv64MemLayout *>(this->memlayout);
  int64 argsToStkpassSize = memlayout->SizeOfArgsToStackpass();
  if (sgkind == kMsArgsStkpassed) {  // for callees
    uint32 offset = symalloc->offset;
    return offset;
  } else if (sgkind == kMsArgsRegpassed) {
    int baseOffset = memlayout->GetSizeOfLocals() + symalloc->offset + memlayout->GetSizeOfRefLocals() +
                     memlayout->seg_lockobjslot.size;
    if (!UseFP() && !HasVLAOrAlloca() && argsToStkpassSize > 0) {
      baseOffset += argsToStkpassSize;
    }
    return baseOffset + sizeofFplr;
  } else if (sgkind == kMsReflocals) {
    int baseOffset = symalloc->offset + memlayout->GetSizeOfLocals() + memlayout->seg_lockobjslot.size;
    if (!UseFP() && !HasVLAOrAlloca() && argsToStkpassSize > 0) {
      baseOffset += argsToStkpassSize;
    }
    return baseOffset + sizeofFplr;
  } else if (sgkind == kMsLocals) {
    int baseOffset = symalloc->offset + memlayout->seg_lockobjslot.size;
    if (!UseFP() && !HasVLAOrAlloca() && argsToStkpassSize > 0) {
      baseOffset += argsToStkpassSize;
    }
    return baseOffset + sizeofFplr;
  } else if (sgkind == kMsLockslot) {
    int baseOffset = symalloc->offset;
    if (!UseFP() && !HasVLAOrAlloca() && argsToStkpassSize > 0) {
      baseOffset += argsToStkpassSize;
    }
    return baseOffset + sizeofFplr;
  } else if (sgkind == kMsSpillReg) {
    int baseOffset = symalloc->offset + memlayout->SizeOfArgsRegisterPassed() + memlayout->GetSizeOfLocals() +
                     memlayout->GetSizeOfRefLocals() + memlayout->seg_lockobjslot.size;
    if (!UseFP() && !HasVLAOrAlloca() && argsToStkpassSize > 0) {
      baseOffset += argsToStkpassSize;
    }
    return baseOffset + sizeofFplr;
  } else if (sgkind == kMsArgsToStkpass) {  // this is for callers
    return static_cast<uint32>(symalloc->offset);
  } else {
    CG_ASSERT(false, "");
  }
  return 0;
}

void Riscv64CGFunc::AppendCall(MIRSymbol * func) {
  Riscv64ListOperand *srcOpnds = memPool->New<Riscv64ListOperand>(funcscope_allocator_);
  Operand *targetopnd = GetOrCreateFuncNameOpnd(func);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xbl, targetopnd, srcOpnds));
}

void Riscv64CGFunc::AppendJump(MIRSymbol * func) {
  Operand *targetopnd = CreateFuncLabelOperand(func);
  curbb->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xuncond, targetopnd));
}

/* For long reach branch, insert a branch in between.
 * convert
 *     condbr target_label
 *     fallthruBB
 * to
 *     condbr pad_label         bb
 *     uncondbr bypass_label    brBB
 * pad_label                   padBB
 *     uncondbr target_label
 * bypass_label                bypassBB
 *     ...                     fallthruBB
 */
void Riscv64CGFunc::InsertJumpPad(Insn *insn) {
  BB *bb = insn->bb;
  CG_ASSERT(bb, "instruction has no bb");
  CG_ASSERT(bb->GetKind() == BB::kBBIf, "instruction is not in a if bb");

  LabelIdx labidx = static_cast<LabelOperand *>(insn->GetOperand(insn->GetJumpTargetIdx()))->labidx_;
  LabelIdx padLabel = CreateLabel();
  BB *brBB = CreateNewBB();
  BB *padBB = CreateNewBB();
  lab2bbmap[padLabel] = padBB;
  padBB->AddLabel(padLabel);

  BB *targetBB;
  BB *fallthruBB = bb->next;
  CG_ASSERT(bb->succs.size() == 2, "if bb should have 2 successors");
  if (bb->succs.front() == fallthruBB) {
    targetBB = bb->succs.back();
  } else {
    targetBB = bb->succs.front();
  }
  // Regardless targetBB as is or an non-empty  successor, it needs to be removed
  bb->succs.remove(targetBB);
  targetBB->preds.remove(bb);
  while (targetBB->GetKind() == BB::kBBFallthru && targetBB->NumInsn() == 0) {
    targetBB = targetBB->next;
  }
  bb->next = brBB;
  brBB->next = padBB;
  padBB->next = fallthruBB;
  brBB->prev = bb;
  padBB->prev = brBB;
  fallthruBB->prev = padBB;
  // adjust bb branch preds succs for jump to padBB
  LabelOperand *padLabelOpnd = GetOrCreateLabelOperand(padLabel);
  uint32 idx = insn->GetJumpTargetIdx();
  insn->SetOperand(idx, padLabelOpnd);
  bb->succs.remove(fallthruBB);
  bb->succs.push_back(brBB);   // new fallthru
  bb->succs.push_back(padBB);  // new target
  //
  targetBB->preds.remove(bb);
  targetBB->preds.push_back(padBB);
  //
  LabelIdx bypassLabel = fallthruBB->labidx;
  if (bypassLabel == 0) {
    bypassLabel = CreateLabel();
    lab2bbmap[bypassLabel] = fallthruBB;
    fallthruBB->AddLabel(bypassLabel);
  }
  LabelOperand *bypassLabelOpnd = GetOrCreateLabelOperand(bypassLabel);
  brBB->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xuncond, bypassLabelOpnd));
  brBB->SetKind(BB::kBBGoto);
  brBB->preds.push_back(bb);
  brBB->succs.push_back(fallthruBB);
  //
  RegOperand *targetAddr = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
  LabelIdx targetLabel = targetBB->labidx;
  if (targetLabel == 0) {
    targetLabel = CreateLabel();
    lab2bbmap[targetLabel] = targetBB;
    targetBB->AddLabel(targetLabel);
  }
  LabelOperand *targetLabelOpnd = GetOrCreateLabelOperand(targetLabel);
  padBB->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_laddr, targetAddr, targetLabelOpnd));
  padBB->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xbr, targetAddr, targetLabelOpnd));
  padBB->SetKind(BB::kBBIgoto);
  padBB->preds.push_back(bb);
  padBB->succs.push_back(targetBB);
  //
  fallthruBB->preds.remove(bb);
  fallthruBB->preds.push_back(brBB);
}

void Riscv64CGFunc::ConvertJumpToRegJump(Insn *insn) {
  BB *bb = insn->bb;
  CG_ASSERT(bb, "instruction has no bb");
  CG_ASSERT(bb->GetKind() == BB::kBBGoto, "instruction is not in a goto bb");

  LabelIdx labidx = static_cast<LabelOperand *>(insn->GetOperand(insn->GetJumpTargetIdx()))->labidx_;
  RegOperand *targetAddr = CreateVirtualRegisterOperand(New_V_Reg(kRegTyInt, 8));
  LabelOperand *targetLabelOpnd = GetOrCreateLabelOperand(labidx);
  Insn *loadLabelInsn = cg->BuildInstruction<Riscv64Insn>(MOP_laddr, targetAddr, targetLabelOpnd);
  bb->InsertInsnBefore(insn, loadLabelInsn);

  Insn *regJumpInsn = cg->BuildInstruction<Riscv64Insn>(MOP_xbr, targetAddr, targetLabelOpnd);
  bb->ReplaceInsn(insn, regJumpInsn);
}

void Riscv64CGFunc::DBGFixCallFrameLocationOffsets() {
  // LogInfo::MapleLogger() << "DBGFixCallFrameLocationOffsets " << " DBG callframe offset = " << dbg_callframe_offset << endl;
  for (DBGExprLoc *el : dbg_callframe_locations) {
    if (el->simploc_->dwop_ == DW_OP_fbreg) {
      SymbolAlloc *symloc = static_cast<SymbolAlloc *>(el->symloc_);
      int64_t offset = GetBaseOffset(symloc) - dbg_callframe_offset;

      el->SetFboffset(offset);
    }
  }
}

void Riscv64CGFunc::OffsetAdjustmentForFPLR() {
  int64 argsToStkpassSize = memlayout->SizeOfArgsToStackpass();
  FOR_ALL_BB(bb, this) {
    FOR_BB_INSNS_SAFE(insn, bb, ninsn) {
      for (int i = 0; i < Insn::kMaxOperandNum; i++) {
        Operand *oper = insn->opnds[i];
        if (oper) {
          if (oper->IsRegister()) {
            RegOperand *regop = dynamic_cast<RegOperand *>(oper);
            if (regop && regop->IsOfVary()) {
              insn->SetOperand(i, GetOrCreateStackBaseRegOperand());
            }
          } else if (oper->IsMemoryAccessOperand()) {
            Riscv64MemOperand *memoper = dynamic_cast<Riscv64MemOperand *>(oper);
            if (memoper &&
                memoper->GetBaseRegister() && memoper->GetBaseRegister()->IsOfVary()) {
              memoper->SetBaseRegister(static_cast<Riscv64RegOperand *>(GetOrCreateStackBaseRegOperand()));
            }

            if (memoper) {
              Riscv64OfstOperand *oo = memoper->GetOffsetImmediate();
              if (oo->IsVary()) {
                int32 offset;
                Riscv64MemLayout *layout = static_cast<Riscv64MemLayout *>(memlayout);
                if (!UseFP() && !HasVLAOrAlloca() && argsToStkpassSize > 0) {
                  offset = layout->RealStackFrameSize();
                } else {
                  offset = layout->RealStackFrameSize() - argsToStkpassSize;
                }
                oo->AdjustOffset(offset);
                if (oo->IsInBitSize(11) == false) {
                  Riscv64RegOperand *dst = GetOrCreatePhysicalRegisterOperand(Riscv64Abi::kIntSpareReg, 64, kRegTyInt);
                  Insn *li = cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, dst, CreateImmOperand(oo->GetOffsetValue(), 64, false));
                  insn->bb->InsertInsnBefore(insn, li);
                  Insn *add = cg->BuildInstruction<Riscv64Insn>(MOP_xaddrrr, dst, memoper->GetBaseRegister(), dst);
                  insn->bb->InsertInsnBefore(insn, add);
                  memoper->SetBaseRegister(dst);
                  oo->SetOffsetValue(0);
                }
                oo->SetVary(false);
              }
            }

          } else if (oper->IsIntImmediate()) {
            ImmOperand *imo = dynamic_cast<ImmOperand *>(oper);
            OfstOperand *imo1 = dynamic_cast<OfstOperand *>(oper);
            if (imo || imo1) {
              if (imo->IsVary()) {
                if (!UseFP() && !HasVLAOrAlloca() && argsToStkpassSize > 0) {
                  imo->Add(static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize());
                } else {
                  imo->Add(static_cast<Riscv64MemLayout *>(memlayout)->RealStackFrameSize() - argsToStkpassSize);
                  if (insn->GetMachineOpcode() != MOP_xmovri64 &&
                      !static_cast<Riscv64ImmOperand*>(imo)->IsInBitSize(11)) {
                    if (ReplaceLargeStackOffsetImm(insn)) {
                      bb->RemoveInsn(insn);
                    }
                  }
                }
              }
              imo->SetVary(false);
            }
          }
        } else {
          break;
        }
      }
    }
  }

#undef STKLAY_DBUG
#ifdef STKLAY_DBUG
  // int32 total =
  //            seg__args_regpassed.size
  //          + static_cast<Riscv64CGFunc*>(cgfunc)->SizeOfCalleeSaved()
  //          + GetSizeOfRefLocals() + locals().size + GetSizeOfSpillReg() + seg_lockobjslot.size;

  // if the function does not have VLA nor alloca,
  // we allocate space for arguments to stack-pass
  // in the call frame; otherwise, it has to be allocated for each call and reclaimed afterward.
  //    total += seg__args_to_stkpass.size;
  Riscv64MemLayout *riscv64memlayout = static_cast<Riscv64MemLayout *>(this->memlayout);
  LogInfo::MapleLogger() << "stkpass: " << riscv64memlayout->seg__args_to_stkpass.size << endl;
  LogInfo::MapleLogger() << "lockslot: " << riscv64memlayout->seg_lockobjslot.size << endl;
  LogInfo::MapleLogger() << "local: " << riscv64memlayout->GetSizeOfLocals() << endl;
  LogInfo::MapleLogger() << "ref local: " << riscv64memlayout->GetSizeOfRefLocals() << endl;
  LogInfo::MapleLogger() << "regpass: " << riscv64memlayout->seg__args_regpassed.size << endl;
  LogInfo::MapleLogger() << "regspill: " << riscv64memlayout->GetSizeOfSpillReg() << endl;
  LogInfo::MapleLogger() << "calleesave: " << SizeOfCalleeSaved() << endl;

#endif
}

bool Riscv64CGFunc::SizeIsRight(Insn * insn) {
  CG_ASSERT(insn->IsMachineInstruction(), "insn should be instruction");
  MOperator mop = insn->GetMachineOpcode();
  switch (mop) {
    case MOP_xaddrri12:
    case MOP_waddrri12:
    case MOP_xsubrri12:
    case MOP_wsubrri12: {
      Riscv64ImmOperand *immopnd = static_cast<Riscv64ImmOperand *>(insn->GetOperand(2));
      return immopnd->IsInSignedBitSize(12);
    }
    case MOP_xandrri13:
    case MOP_xiorrri13:
    case MOP_xeorrri13: {
      Riscv64ImmOperand *immopnd = static_cast<Riscv64ImmOperand *>(insn->GetOperand(2));
      return immopnd->IsInBitSize(12);
    }
    case MOP_wlslrri5:
    case MOP_wasrrri5:
    case MOP_wlsrrri5: {
      Riscv64ImmOperand *immopnd = static_cast<Riscv64ImmOperand *>(insn->GetOperand(2));
      return immopnd->IsInBitSize(8);
    }
    case MOP_xlslrri6:
    case MOP_xasrrri6:
    case MOP_xlsrrri6: {
      Riscv64ImmOperand *immopnd = static_cast<Riscv64ImmOperand *>(insn->GetOperand(2));
      return immopnd->IsInBitSize(6);
    }
    case MOP_wldrb:
    case MOP_wstrb:
    case MOP_wldrsb: {
      Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(insn->GetOperand(1));
      return memopnd->GetSize() == 8;
    }
    case MOP_wldrh:
    case MOP_wstrh:
    case MOP_wldrsh: {
      Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(insn->GetOperand(1));
      return memopnd->GetSize() == 16;
    }
    case MOP_sldr:
    case MOP_wldaxr:
    case MOP_wstr:
    case MOP_sstr:
    case MOP_wstlr:
    case MOP_wstllr:
    case MOP_wstadd:
    case MOP_wstaddl:
    case MOP_wstclr:
    case MOP_wstclrl:
    case MOP_wsteor:
    case MOP_wsteorl:
    case MOP_wldr: {
      Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(insn->GetOperand(1));
      return memopnd->GetSize() == 32;
    }
    case MOP_dldr:
    case MOP_xldxr:
    case MOP_xldaxr:
    case MOP_xstr:
    case MOP_dstr:
    case MOP_xstlr:
    case MOP_xstllr:
    case MOP_xstadd:
    case MOP_xstaddl:
    case MOP_xstclr:
    case MOP_xstclrl:
    case MOP_xsteor:
    case MOP_xsteorl:
    case MOP_xldr: {
      Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(insn->GetOperand(1));
      return memopnd->GetSize() == 64;
    }
    case MOP_wldaxp:
    case MOP_wstxr:
    case MOP_wstlxr: {
      Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(insn->GetOperand(2));
      return memopnd->GetSize() == 32;
    }
    case MOP_xldaxp:
    case MOP_xstxr:
    case MOP_xstlxr: {
      Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(insn->GetOperand(2));
      return memopnd->GetSize() == 64;
    }
    case MOP_xstlxp:
    case MOP_wstlxp: {
      Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(insn->GetOperand(3));
      return memopnd->GetSize() == 64;
    }
    default:
      return true;
  }
}

void Riscv64CGFunc::CheckImmMemSize() {
  FOR_ALL_BB(bb, this) {
    FOR_BB_INSNS(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      if (!SizeIsRight(insn)) {
        LogInfo::MapleLogger() << "Warning: ImmOperand or MemOperand size is unreasonable in function " << GetName() << std::endl;
        insn->dump();
      }
    }
  }
}

MemOperand *Riscv64CGFunc::AdjustMemOperandIfOffsetOutOfRange(
  MemOperand * memopnd, regno_t vrnum, Insn * insn, Riscv64reg_t regnum, uint8 & isOutOfRange, bool isDest) {
  CHECK_FATAL(vrnum < v_reg_table.size(), "index out of range in Riscv64CGFunc::AdjustMemOperandIfOffsetOutOfRange");
  int32 datasize = v_reg_table[vrnum].GetSize() * BITS_PER_BYTE;
  if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(memopnd), datasize)) {
    isOutOfRange = 1;
    memopnd =
      SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(memopnd), datasize, regnum, insn, isDest);
  } else {
    isOutOfRange = 0;
  }
  return memopnd;
}

void Riscv64CGFunc::TestInterface() {
  regno_t vn1 = New_V_Reg(kRegTyInt, 4);
  regno_t vn2 = New_V_Reg(kRegTyInt, 8);
  MemOperand *rm1 = GetOrCreatSpillMem(vn1);
  MemOperand *rm2 = GetOrCreatSpillMem(vn2);
  FreeSpillRegMem(vn1);
  FreeSpillRegMem(vn2);

  regno_t vn3 = New_V_Reg(kRegTyInt, 4);
  regno_t vn4 = New_V_Reg(kRegTyInt, 8);
  MemOperand *rm3 = GetOrCreatSpillMem(vn3);
  MemOperand *rm4 = GetOrCreatSpillMem(vn4);

  LogInfo::MapleLogger() << "rm1 addr: " << static_cast<Riscv64OfstOperand *>(rm1->GetOffsetOperand())->GetOffsetValue() << endl;
  LogInfo::MapleLogger() << "rm3 addr: " << static_cast<Riscv64OfstOperand *>(rm3->GetOffsetOperand())->GetOffsetValue() << endl;
  LogInfo::MapleLogger() << "rm2 addr: " << static_cast<Riscv64OfstOperand *>(rm2->GetOffsetOperand())->GetOffsetValue() << endl;
  LogInfo::MapleLogger() << "rm4 addr: " << static_cast<Riscv64OfstOperand *>(rm4->GetOffsetOperand())->GetOffsetValue() << endl;
}

void Riscv64CGFunc::FreeSpillRegMem(regno_t vrnum) {
  MemOperand *memop = nullptr;

  auto vrIt = spillreg_mem_operands.find(vrnum);
  if (vrIt != spillreg_mem_operands.end()) {
    memop = vrIt->second;
  }

  if (!memop && IsVRegNoForPseudoRegister(vrnum)) {
    auto p = preg_spill_mem_operands.find(GetPseudoRegIdxFromVirtualRegNo(vrnum));
    if (p != preg_spill_mem_operands.end()) {
      memop = p->second;
    }
  }

  if (!memop) {
    CG_ASSERT(false, " free spillreg have no mem");
    return;
  }

  uint32 size = memop->GetSize();
  MapleMap<uint32, SpillmemoperandSet *>::iterator iter;
  if ((iter = reusespillloc_mem.find(size)) != reusespillloc_mem.end()) {
    iter->second->Add(memop);
  } else {
    reusespillloc_mem[size] = memPool->New<SpillmemoperandSet>(funcscope_allocator_);
    reusespillloc_mem[size]->Add(memop);
  }
}

MemOperand *Riscv64CGFunc::GetOrCreatSpillMem(regno_t vrnum, int32 spillOffset) {
  // NOTES: must used in RA, not used in other place
  if (spillOffset == 0 && IsVRegNoForPseudoRegister(vrnum)) {
    auto p = preg_spill_mem_operands.find(GetPseudoRegIdxFromVirtualRegNo(vrnum));
    if (p != preg_spill_mem_operands.end()) {
      return p->second;
    }
  }

  auto p = spillreg_mem_operands.find(vrnum);
  if (p == spillreg_mem_operands.end()) {
    CHECK_FATAL(vrnum < v_reg_table.size(), "index out of range in Riscv64CGFunc::FreeSpillRegMem");
    uint32 datasize = v_reg_table[vrnum].GetSize() * BITS_PER_BYTE;
    auto it = reusespillloc_mem.find(datasize);
    if (spillOffset == 0 && it != reusespillloc_mem.end()) {
      MemOperand *memopnd = it->second->GetOne();
      if (memopnd) {
        spillreg_mem_operands.insert(pair<regno_t, MemOperand *>(vrnum, memopnd));
        return memopnd;
      }
    }

    RegOperand *baseOpnd = GetOrCreateStackBaseRegOperand();
    int32 offset;
    if (spillOffset) {
      offset = spillOffset;
    } else {
      offset = GetOrCreatSpillRegLocation(vrnum);
    }
    Riscv64OfstOperand *offsetOpnd = memPool->New<Riscv64OfstOperand>(offset, 64);
    MemOperand *memopnd = memPool->New<Riscv64MemOperand>(datasize, baseOpnd,
                                                      static_cast<Riscv64RegOperand *>(nullptr), offsetOpnd, nullptr);
    spillreg_mem_operands.insert(pair<regno_t, MemOperand *>(vrnum, memopnd));
    spillreg_mem_operands_adj.insert(memopnd);
    return memopnd;
  } else {
    return p->second;
  }
}

MemOperand *Riscv64CGFunc::GetPseudoRegisterSpillMemoryOperand(PregIdx i) {
  MemOperand *memopnd = nullptr;
  MapleMap<PregIdx, MemOperand *>::iterator p;
  if (g->optim_level == 0) {
    p = preg_spill_mem_operands.end();
  } else {
    p = preg_spill_mem_operands.find(i);
  }
  if (p == preg_spill_mem_operands.end()) {
    int64 offset = GetPseudoRegisterSpillLocation(i);
    MIRPreg *preg = func->pregTab->PregFromPregIdx(i);
    uint32_t bitlen = GetPrimTypeSize(preg->primType) * BITS_PER_BYTE;
    RegOperand *base = GetOrCreateFramePointerRegOperand();

    Riscv64OfstOperand *offopnd = GetOrCreateOfstOpnd(offset, 32);
    memopnd = GetOrCreateMemOpnd(bitlen, base, nullptr, offopnd, nullptr);
    if (IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(memopnd), bitlen)) {
      memopnd = SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(memopnd), bitlen);
    }
    preg_spill_mem_operands.insert(pair<PregIdx, MemOperand *>(i, memopnd));
  } else {
    memopnd = p->second;
  }
  return memopnd;
}

/* Get the number of return register of current function.
 */
Riscv64reg_t Riscv64CGFunc::GetReturnRegisterNumber() {
  ReturnMechanism retmech(GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->GetReturnTyIdx()), becommon);
  if (retmech.regcount > 0) {
    return retmech.reg0;
  } else {
    return kRinvalid;
  }
}

void Riscv64CGFunc::InsertYieldpoint() {
  string str = string(NameMangler::kReflectionClassesPrefixStr) + string(NameMangler::kReference) + string(NameMangler::kCinitStr) +
               string(NameMangler::kJavaLangObjectStr) + string(NameMangler::kJavaLangRef) + string(NameMangler::kReferenceQueue);
  if (strncmp((func->GetName()).c_str(), str.c_str(), strlen(str.c_str())) == 0) {
    // skip insert yieldpoint in reference constructor, avoid rc verify issue
    CG_ASSERT (yieldPointInsn != nullptr, "the entry yield point has been inserted");
    yieldPointInsn->bb->RemoveInsn(yieldPointInsn);
    return;
  }
  // Converting x31 to x9 for alternate calling convention used in special functions.
  for (auto &p : callnativemap) {
    Insn *in = p.first;
    Riscv64RegOperand *funcreg = GetOrCreatePhysicalRegisterOperand(R9, 64, kRegTyInt);
    in->SetOperand(0, funcreg);
  }
  // do not insert yieldpoint in function that not saved X30 into stack,
  // because X30 will be changed after yieldpoint is taken.
  if (!hasProEpilogue) {
    CG_ASSERT (yieldPointInsn != nullptr, "the entry yield point has been inserted");
    yieldPointInsn->bb->RemoveInsn(yieldPointInsn);
    return;
  }
  // skip if no firstbb.
  if (firstbb == nullptr) {
    return;
  }
  // The yield point in the entry of the func is inserted just after the initialization
  // of localrefvars in HandleRCCall.
  // for BBs after firstbb.
  for (BB *bb = firstbb->next; bb != nullptr; bb = bb->next) {
    if (g->optim_level >= 2) {
      // insert a yieldpoint before the last jump instruction of a goto BB.
      if (bb->IsBackEdgeDest()) {
        dummybb->ClearInsns();
        GenerateYieldpoint(dummybb);
        bb->InsertAtBeginning(dummybb);
        continue;
      }
    } else {
      // when -O0, there is no backedge info available.
      if (bb->GetKind() == BB::kBBGoto && bb->lastinsn != nullptr && bb->lastinsn->IsBranch()) {
        dummybb->ClearInsns();
        GenerateYieldpoint(dummybb);
        bb->InsertAtBeginning(dummybb);
        continue;
      }
      // insert yieldpoint for empty loop (CondGoto backward),
      // aka. last instruction jump to the top of the BB.
      if (bb->lastinsn && bb->lastinsn->IsBranch()) {
        // the jump instruction.
        Insn *jump = bb->lastinsn;
        // skip if no jump target.
        if (jump->GetOpndNum() == 0) {
          continue;
        }
        // get the jump target operand.
        Operand *op = jump->GetOpnd(jump->GetOpndNum() - 1);
        // last operand not found or not a label operand.
        if (op == nullptr || !op->IsLabel()) {
          continue;
        }
        // the label operand of the jump instruction.
        LabelOperand *label = dynamic_cast<LabelOperand *>(op);
        if (label == nullptr || label->GetLabelIndex() != bb->labidx) {
          continue;
        }
        // insert yieldpoint before jump instruction.
        dummybb->ClearInsns();
        GenerateYieldpoint(dummybb);
        bb->InsertAtBeginning(dummybb);
      }
    }
  }
}

MemOperand *Riscv64CGFunc::LoadStructCopyBase(MIRSymbol *symbol, int32 offset, int datasize) {
  // For struct formals > 16 bytes, this is the pointer to the struct copy.
  // Load the base pointer first.
  uint32 dataSize = GetPrimTypeSize(PTY_a64);
  uint32 dataSizeBits = dataSize * BITS_PER_BYTE;
  regno_t vreg = New_V_Reg(kRegTyInt, dataSize);
  RegOperand *vreg2 = CreateVirtualRegisterOperand(vreg);
  MemOperand *memopnd = GetOrCreateMemOpnd(symbol, 0, dataSizeBits);
  curbb->AppendInsn(
      cg->BuildInstruction<Riscv64Insn>(
          PickLdInsn(dataSizeBits, PTY_i64), vreg2, memopnd)
  );
  // Create the indirect load mem opnd from the base pointer.
  return CreateMemOpnd(vreg2, offset, datasize);
}

// Return true to remove insn
bool Riscv64CGFunc::ReplaceLargeStackOffsetImm(Insn *insn) {
  MOperator opc = insn->GetMachineOpcode();
  switch (opc) {
  case MOP_xaddrri12: {
    Operand *dst = insn->opnds[0];
    Insn *li = cg->BuildInstruction<Riscv64Insn>(MOP_xmovri64, dst, insn->opnds[2]);
    insn->bb->InsertInsnBefore(insn, li);
    insn->SetOperand(2, dst);
    insn->SetMOP(MOP_xaddrrr);
    return false;
  }
  default:
    CHECK_FATAL(0, "FIX");
  }
  return true;
}

}  // namespace maplebe
