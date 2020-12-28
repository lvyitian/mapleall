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

#include "riscv64_ebo.h"
#include "riscv64_cg.h"
#include "cg_assert.h"
namespace maplebe {

using namespace maple;
#define EBODUMP CGDEBUGFUNC(cgfunc)

// return the regno of the low 32bit. e.g opnd= d10, return v10.s[0]
regno_t Riscv64Ebo::GetLowVec(Operand *opnd) {
  CG_ASSERT(opnd && opnd->IsRegister(), "");
  regno_t regno = static_cast<RegOperand *>(opnd)->GetRegisterNumber();
  return regno - V0 + VB32;
}

// return the regno of the high 32bit. e.g opnd= d10, return v10.s[1]
regno_t Riscv64Ebo::GetHighVec(Operand *opnd) {
  CG_ASSERT(opnd && opnd->IsRegister(), "");
  regno_t regno = static_cast<RegOperand *>(opnd)->GetRegisterNumber();
  return regno - V0 + VB64;
}

bool Riscv64Ebo::IsFloatReg(RegOperand *regopnd) {
  regno_t regno = regopnd->GetRegisterNumber();
  return (regno >= V0 && regno <= V31);
}

bool Riscv64Ebo::IsFmov(Insn *insn) {
  return (insn->mop_ >= MOP_xvmovsr && insn->mop_ <= MOP_xvmovrd);
}

void Riscv64Ebo::SetOpnd(Insn *insn, int i, Operand *opnd) {
  if (insn->IsDestRegAlsoSrcReg()) {
    insn->SetOperand(i, opnd);
    return;
  }
  const Riscv64MD *md = &Riscv64CG::kMd[insn->mop_];
  if (md->IsStore()) {
    insn->SetOperand(i, opnd);
  } else {
    insn->SetOperand(insn->GetResultNum() + i, opnd);
  }
}

bool Riscv64Ebo::IsAdd(Insn *insn) {
  return (insn->mop_ >= MOP_xaddrrr && insn->mop_ <= MOP_ssub);
}

bool Riscv64Ebo::IsVecReg(Operand *opnd) {
  if (!opnd->IsRegister()) {
    return false;
  }
  Riscv64RegOperand *a64reg = static_cast<Riscv64RegOperand *>(opnd);
  return a64reg->IsSimdVectorMode();
}

bool Riscv64Ebo::IsZeroRegister(Operand *opnd) {
  if (!opnd->IsRegister()) {
    return false;
  }
  Riscv64RegOperand *aarreg = static_cast<Riscv64RegOperand *>(opnd);
  return aarreg->IsZeroRegister();
}

bool Riscv64Ebo::IsImplicit(Insn *insn) {
  const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->mop_];
  if (md->IsCondDef()) {
    return true;
  }
  for (uint32_t i = 0; i < Riscv64Insn::kMaxOperandNum; i++) {
    Operand *op = insn->GetOperand(i);
    if (op && op->IsConditionCode()) {
      return true;
    }
  }
  return false;
}

bool Riscv64Ebo::IsClinitCheck(Insn *insn) {
  if (insn == nullptr) {
    return false;
  }
  MOperator mop = insn->GetMachineOpcode();
  return mop == MOP_clinit || mop == MOP_clinit_tail;
}

void Riscv64Ebo::InitCallerSaveRegisters() {
  if (initCallerSaveRegs) {
    return;
  }
  initCallerSaveRegs = true;
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  for (int i = R1; i <= R31; i++) {
    if (Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)i)) {
      continue;
    }
    regOperand[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)i, 64, kRegTyInt);
  }
  for (int i = V0; i <= V31; i++) {
    if (Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)i)) {
      continue;
    }
    regOperand[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)i, 64, kRegTyFloat);
  }
}

void Riscv64Ebo::InitCallAndReturnUseRegisters() {
  if (initCallAndReturnRegs) {
    return;
  }
  initCallAndReturnRegs = true;
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  for (int i = R1; i <= R31; i++) {
    if (Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)i) == false) {
      continue;
    }
    regOperand[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)i, 64, kRegTyInt);
  }
  for (int i = V0; i <= V31; i++) {
    if (Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)i) == false) {
      continue;
    }
    regOperand[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)i, 64, kRegTyFloat);
  }
  regOperand[RSP] = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)RSP, 64, kRegTyInt);
}

void Riscv64Ebo::DefineClinitSpecialRegisters(InsnInfo *insninfo) {
  Insn *insn = insninfo->insn;
  RegOperand *phyopnd;
  InitCallerSaveRegisters();
  phyopnd = regOperand[R16];
  OpndInfo *opndinfo = OperandInfoDef(insn->bb, insn, phyopnd);
  opndinfo->insninfo = insninfo;

  phyopnd = regOperand[R17];
  opndinfo = OperandInfoDef(insn->bb, insn, phyopnd);
  opndinfo->insninfo = insninfo;
}

void Riscv64Ebo::DefineCallerSaveRegisters(InsnInfo *insninfo) {
  RegOperand *phyopnd = nullptr;
  Insn *insn = insninfo->insn;

  CG_ASSERT(insn->IsCall(), "insn should be a call insn.");

  // Define return register.
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  InitCallerSaveRegisters();
  phyopnd = regOperand[Riscv64Abi::kIntRetReg0];
  OpndInfo *opndinfo = OperandInfoDef(insn->bb, insn, phyopnd);
  opndinfo->insninfo = insninfo;

  phyopnd = regOperand[Riscv64Abi::kFpRetReg0];
  opndinfo = OperandInfoDef(insn->bb, insn, phyopnd);
  opndinfo->insninfo = insninfo;

  // Define scalar caller save register.
  for (int i = R1; i <= R31; i++) {
    if (Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)i)) {
      continue;
    }
    phyopnd = regOperand[i];
    opndinfo = OperandInfoDef(insn->bb, insn, phyopnd);
    opndinfo->insninfo = insninfo;
  }

  // Define FP caller save registers.
  for (int i = V0; i <= V31; i++) {
    if (Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)i)) {
      continue;
    }
    phyopnd = regOperand[i];
    opndinfo = OperandInfoDef(insn->bb, insn, phyopnd);
    opndinfo->insninfo = insninfo;
  }
}

void Riscv64Ebo::DefineReturnUseRegister(Insn *insn) {
  RegOperand *phyopnd = nullptr;

  // Define return register.
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  InitCallAndReturnUseRegisters();

  // Define scalar callee save register and FP, LR.
  for (int i = R1; i <= R31; i++) {
    if (Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)i) == false) {
      continue;
    }
    phyopnd = regOperand[i];
    OperandInfoUse(insn->bb, insn, phyopnd);
  }

  // Define SP
  phyopnd = regOperand[RSP];
  OperandInfoUse(insn->bb, insn, phyopnd);

  // Define FP caller save registers.
  for (int i = V0; i <= V31; i++) {
    if (Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)i) == false) {
      continue;
    }
    phyopnd = regOperand[i];
    OperandInfoUse(insn->bb, insn, phyopnd);
  }
}

void Riscv64Ebo::DefineCallUseSpecialRegister(Insn *insn) {
  RegOperand *phyopnd = nullptr;

  // Define return register.
  InitCallAndReturnUseRegisters();

  // Define FP, LR.
  phyopnd = regOperand[R1];
  OperandInfoUse(insn->bb, insn, phyopnd);
  phyopnd = regOperand[R8];
  OperandInfoUse(insn->bb, insn, phyopnd);

  // Define SP
  phyopnd = regOperand[RSP];
  OperandInfoUse(insn->bb, insn, phyopnd);
}

/*return true if op1 == op2*/
bool Riscv64Ebo::OperandEqSpecial(Operand *op1, Operand *op2) {
  switch (op1->GetKind()) {
    case Operand::Opd_Register: {
      Riscv64RegOperand *reg1 = static_cast<Riscv64RegOperand *>(op1);
      Riscv64RegOperand *reg2 = static_cast<Riscv64RegOperand *>(op2);
      return (*reg1) == (*reg2);
    }
    case Operand::Opd_Immediate: {
      ImmOperand *imm1 = static_cast<ImmOperand *>(op1);
      ImmOperand *imm2 = static_cast<ImmOperand *>(op2);
      return (*imm1) == (*imm2);
    }
    case Operand::Opd_OfstImmediate: {
      Riscv64OfstOperand *ofst1 = static_cast<Riscv64OfstOperand *>(op1);
      Riscv64OfstOperand *ofst2 = static_cast<Riscv64OfstOperand *>(op2);
      return (*ofst1) == (*ofst2);
    }
    case Operand::Opd_StImmediate: {
      StImmOperand *stimm1 = static_cast<StImmOperand *>(op1);
      StImmOperand *stimm2 = static_cast<StImmOperand *>(op2);
      return (*stimm1) == (*stimm2);
    }
    case Operand::Opd_Mem: {
      Riscv64MemOperand *mem1 = static_cast<Riscv64MemOperand *>(op1);
      Riscv64MemOperand *mem2 = static_cast<Riscv64MemOperand *>(op2);
      return (OperandEqual(mem1->GetBaseRegister(), mem2->GetBaseRegister()) &&
              OperandEqual(mem1->GetIndexRegister(), mem2->GetIndexRegister()) &&
              OperandEqual(mem1->GetOffsetOperand(), mem2->GetOffsetOperand()) &&
              (mem1->GetSymbol() == mem2->GetSymbol()) && (mem1->GetSize() == mem2->GetSize()));
    }
    default: {
      return false;
    }
  }
  return false;
}

int32_t Riscv64Ebo::GetOffsetVal(MemOperand *mem) {
  Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(mem);
  Riscv64OfstOperand *offset = memopnd->GetOffsetImmediate();
  int32_t val = 0;
  if (offset) {
    val += offset->GetOffsetValue();

    if (offset->op_kind_ == Operand::Opd_StImmediate) {
      val += ((StImmOperand *)offset)->GetSymbol()->GetStIdx().Idx();
    }
  }
  return val;
}

/*
 *move vreg, 0
 *store vreg, mem
 *===>
 *store wzr, mem
 *return true if do simplify successfully.
 */
bool Riscv64Ebo::DoConstProp(Insn *insn, int idx, Operand *opnd) {
  Riscv64ImmOperand *src = static_cast<Riscv64ImmOperand *>(opnd);
  // move vreg, 0
  // store vreg, mem
  // ===>
  // store wzr, mem
  CG_ASSERT(insn->GetOpnd(idx), "");
  const Riscv64MD *md = &Riscv64CG::kMd[(insn->mop_)];
  // avoid the invalid case "cmp wzr, #0"/"add w1, wzr, #100"
  if (src->IsZero() && insn->GetOpnd(idx)->IsRegister() && (insn->IsStore() || insn->IsMove() || md->IsCondDef())) {
    SetOpnd(insn, idx, GetZeroOpnd(src->GetSize()));
    return true;
  }
  MOperator mopcode = insn->GetMachineOpcode();
  switch (mopcode) {
    case MOP_xiorrrr: {
      if (src->GetValue() == 0) {
        Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
        RegOperand *zero = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)R0, 64, kRegTyInt);
        insn->SetOperand(idx + 1, zero);
        return true;
      }
      break;
    }
    case MOP_xmovrr:
    case MOP_wmovrr: {
      if (idx != 0) {
        return false;
      }
      uint32_t targetSize = insn->GetOpnd(idx)->GetSize();
      if (src->GetSize() != targetSize) {
        src = static_cast<Riscv64ImmOperand *>(src->Clone(cgfunc->memPool));
        CG_ASSERT(src != nullptr, "");
        src->size_ = targetSize;
      }
      if (static_cast<Riscv64ImmOperand *>(src)->IsInBitSize(11)) {
        if (EBODUMP) {
          LogInfo::MapleLogger() << " Do constprop:Prop constVal " << src->GetValue() << "into insn:\n";
          insn->dump();
        }
        insn->SetOperand(1, src);
        MOperator mop;
        if (src->isUpperBits) {
          mop = (mopcode == MOP_wmovrr) ? MOP_xmov20up : MOP_xmov52up;
        } else {
          mop = MOP_xmovri64;
        }
        insn->mop_ = mop;
        if (EBODUMP) {
          LogInfo::MapleLogger() << " after constprop the insn is:\n";
          insn->dump();
        }
        return true;
      }
      break;
    }
    case MOP_xaddrrr:
    case MOP_waddrrr:
    case MOP_xsubrrr:
    case MOP_wsubrrr: {
      if (idx != 1 || !(src->IsInBitSize(11))) {
        return false;
      }
      if (insn->GetResult(0) == nullptr) {
        return false;
      }
      const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->mop_];
      bool is64bits = md->Is64Bit();
      if (EBODUMP) {
        LogInfo::MapleLogger() << " Do constprop:Prop constVal " << src->GetValue() << "into insn:\n";
        insn->dump();
      }
      if (src->IsZero() && src->IsVary() == false) {
        insn->SetOperand(2, nullptr);
        insn->mop_ = MOP_xmovrr;
        if (EBODUMP) {
          LogInfo::MapleLogger() << " after constprop the insn is:\n";
          insn->dump();
        }
        return true;
      }
      insn->SetOperand(2, src);
      if (mopcode == MOP_xaddrrr || mopcode == MOP_waddrrr) {
        insn->mop_ = is64bits ? MOP_xaddrri12 : MOP_waddrri12;
      } else if (mopcode == MOP_xsubrrr || mopcode == MOP_wsubrrr) {
        insn->mop_ = is64bits ? MOP_xsubrri12 : MOP_wsubrri12;
      }
      if (EBODUMP) {
        LogInfo::MapleLogger() << " after constprop the insn is:\n";
        insn->dump();
      }
      return true;
      break;
    }
    default:
      break;
  }
  return false;
}

/*
 * Look at an exression that has a constant second operand and attempt to
 * simplify the computations.
 */
bool Riscv64Ebo::ConstantOperand(Insn *insn, Operand **opnds, OpndInfo **opndInfo) {
  BB *bb = insn->bb;
  bool retval = false;
  if (insn->GetOpndNum() < 1) {
    return false;
  }
  Operand *op0 = opnds[0];
  Operand *op1 = opnds[1];
  Operand *res = insn->GetResult(0);
  CHECK_FATAL(res, "null ptr check");
  const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->mop_];
  uint32_t dsize = md->GetOperandSize();
  bool first = op0->IsConstant() && !op1->IsConstant();
  CG_ASSERT((op1->IsConstant() && !op0->IsConstant()) || (op0->IsConstant() && !op1->IsConstant()), "");
  Riscv64ImmOperand *imm = nullptr;
  Operand *op = nullptr;
  int32_t idx0 = 0;
  if (first) {
    imm = static_cast<Riscv64ImmOperand *>(op0);
    op = op1;
    if (op->IsMemoryAccessOperand()) {
      op = insn->GetOpnd(1);
    }
    idx0 = 1;
  } else {
    imm = static_cast<Riscv64ImmOperand *>(op1);
    op = op0;
    if (op->IsMemoryAccessOperand()) {
      op = insn->GetOpnd(0);
    }
  }
  // For orr insn and one of the opnd is zero
  if (insn->GetMachineOpcode() == MOP_xiorrri13 && imm->IsZero()) {
    bb->ReplaceInsn(insn, cgfunc->cg->BuildInstruction<Riscv64Insn>(dsize == 64 ? MOP_xmovrr : MOP_wmovrr, res, op));
    return true;
  }
  // For the imm is 0. Then replace the insn by a move insn.
  if ((insn->GetMachineOpcode() >= MOP_xaddrrr && insn->GetMachineOpcode() <= MOP_sadd && imm->IsZero()) ||
      (!first && insn->GetMachineOpcode() >= MOP_xsubrrr && insn->GetMachineOpcode() <= MOP_ssub && imm->IsZero())) {
    bb->ReplaceInsn(insn, cgfunc->cg->BuildInstruction<Riscv64Insn>(dsize == 64 ? MOP_xmovrr : MOP_wmovrr, res, op));
    return true;
  }

  if (insn->GetMachineOpcode() == MOP_xaddrrr || insn->GetMachineOpcode() == MOP_waddrrr) {
//    if (imm->IsInBitSize(24)) {
      // ADD Wd|WSP, Wn|WSP, #imm{, shift} ; 32-bit general registers
      // ADD Xd|SP,  Xn|SP,  #imm{, shift} ; 64-bit general registers
      // imm : 0 ~ 4095, shift: none, LSL #0, or LSL #12
      // riscv64 assembly takes up to 24-bits, if the lower 12 bits is all 0
      if (imm->IsInBitSize(11)) {
        bb->ReplaceInsn(
          insn, cgfunc->cg->BuildInstruction<Riscv64Insn>(dsize == 64 ? MOP_xaddrri12 : MOP_waddrri12, res, op, imm));
        retval = true;
      }
//    }
  }
  // Look for the sequence which can be simpified.
  if (retval == true || insn->GetMachineOpcode() == MOP_xaddrri12 || insn->GetMachineOpcode() == MOP_waddrri12) {
    Insn *prev = opndInfo[idx0]->insn;
    if (prev != nullptr && (prev->GetMachineOpcode() == MOP_xaddrri12 || prev->GetMachineOpcode() == MOP_waddrri12)) {
      OpndInfo *previnfo0 = opndInfo[idx0]->insninfo->orig_opnd[0];
      // if prevop0 has been redefined. skip this optimiztation.
      if (previnfo0->redefined) {
        return retval;
      }
      Operand *prevop0 = prev->GetOpnd(0);
      Riscv64ImmOperand *imm0 = static_cast<Riscv64ImmOperand *>(prev->GetOpnd(1));
      CHECK_FATAL(imm0 != nullptr, "imm0 is null in Riscv64Ebo::ConstantOperand ");
      int64_t val = imm0->GetValue() + imm->GetValue();
      Riscv64CGFunc *aarchfunc = static_cast<Riscv64CGFunc *>(cgfunc);
      Riscv64ImmOperand *imm1 = aarchfunc->CreateImmOperand(val, dsize, imm0->IsSignedValue());
      if (imm0->IsVary()) {
        imm1->SetVary(true);
      }
      if (imm1->IsInBitSize(11)) {
        bb->ReplaceInsn(insn, cgfunc->cg->BuildInstruction<Riscv64Insn>(dsize == 64 ? MOP_xaddrri12 : MOP_waddrri12,
                                                                        res, prevop0, imm1));
        retval = true;
      }
    }
  }
  return retval;
}

/*
 * Try to evaluate the condition of a branch when it's const.
 * For CGIR conditonal branch has two insns.
 */
bool Riscv64Ebo::ResoveCondBranch(Insn *insn, Operand **opnds) {
  BB *bb = insn->bb;
  // Only concern with conditional branch.
  if (bb == nullptr || bb->succs.size() != 2) {
    return false;
  }
  return false;
}

/* Do some special pattern*/
bool Riscv64Ebo::SpecialSequence(Insn *insn, Operand **opnds, OpndInfo **origInfo) {
  if (insn == nullptr) {
    return false;
  }
  MOperator opc = insn->GetMachineOpcode();
  Riscv64CGFunc *aarchfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  switch (opc) {
    // mov R503, R0
    // mov R0, R503
    //  ==> mov R0, R0
    case MOP_wmovrr:
    case MOP_xmovrr: {
      OpndInfo *opndinfo = origInfo[0];
      if (opndinfo == nullptr) {
        return false;
      }
      Insn *prevInsn = origInfo[0]->insn;
      if (prevInsn && (prevInsn->GetMachineOpcode() == opc) && (prevInsn == insn->GetPreviousMachineInsn()) &&
          !RegistersIdentical(prevInsn->GetOperand(0), prevInsn->GetOperand(1)) &&
          !RegistersIdentical(insn->GetOperand(0), insn->GetOperand(1))) {
        Operand *reg1 = insn->GetResult(0);
        Operand *reg2 = prevInsn->GetOperand(1);
        Insn *newInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(insn->GetMachineOpcode(), reg1, reg2);
        insn->bb->ReplaceInsn(insn, newInsn);
        return true;
      }
      break;
    }
    case MOP_xsxtw64: {
      OpndInfo *opndinfo = origInfo[0];
      if (opndinfo == nullptr) {
        return false;
      }
      Insn *prevInsn = origInfo[0]->insn;
      if (prevInsn && (prevInsn->GetMachineOpcode() == opc)) {
        insn->SetMOP(MOP_xmovrr);
      }
      break;
    }
    case MOP_wstr:
    case MOP_xstr:
    case MOP_wldr:
    case MOP_xldr: {
      // add x2, x1, imm
      // ldr x3, [x2]
      // -> ldr x3, [x1, imm]
      // ---------------------
      // add x2, x1, imm
      // str x3, [x2]
      // -> str x3, [x1, imm]
      CG_ASSERT(insn->GetResult(0), "");
      const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->mop_];
      bool is64bits = md->Is64Bit();
      int64 size = md->GetOperandSize();
      OpndInfo *opndinfo = origInfo[0];
      if (insn->IsLoad() && !opndinfo) {
        return false;
      }

      OpndInfo *baseinfo = nullptr;
      MemOperand *memopnd = nullptr;
      if (insn->IsLoad()) {
        MemOpndInfo *meminfo = static_cast<MemOpndInfo *>(opndinfo);
        baseinfo = meminfo->GetBaseInfo();
        memopnd = static_cast<MemOperand *>(meminfo->opnd);
      } else {
        Operand *res = insn->GetResult(0);
        CG_ASSERT(res != nullptr && res->IsMemoryAccessOperand(), "");
        memopnd = static_cast<MemOperand *>(res);
        Operand *base = memopnd->GetBaseRegister();
        CG_ASSERT(base->IsRegister(), "");
        baseinfo = GetOpndInfo(base, -1);
      }

      if (baseinfo && baseinfo->insn) {
        Insn *insn1 = baseinfo->insn;
        InsnInfo *insninfo1 = baseinfo->insninfo;
        MOperator opc1 = insn1->GetMachineOpcode();
        if (opc1 == MOP_xaddrri12 || opc1 == MOP_waddrri12) {
          if (!memopnd->GetOffset()) {
            return false;
          }
          Riscv64ImmOperand *imm0 = static_cast<Riscv64ImmOperand *>(memopnd->GetOffset());
          if (!imm0) {
            return false;
          }
          int64 imm0Val = imm0->GetValue();
          Operand *res = insn->GetOperand(0);
          RegOperand *op1 = static_cast<RegOperand *>(insn1->GetOperand(1));
          Riscv64ImmOperand *imm1 = static_cast<Riscv64ImmOperand *>(insn1->GetOperand(2));
          CG_ASSERT(imm1 != nullptr, "");
          int64 immVal;
          // don't use register if it was redefined.
          OpndInfo *opndinfo1 = insninfo1->orig_opnd[0];
          if (opndinfo1 && opndinfo1->redefined) {
            return false;
          } else {
            immVal = imm0Val + imm1->GetValue();
          }
          if ((!is64bits && immVal < STR_LDR_IMM32_UPPER_BOUND && immVal % 4 == 0) ||
              (is64bits && immVal < STR_LDR_IMM64_UPPER_BOUND && immVal % 8 == 0)) {
            MemOperand *mo = aarchfunc->CreateMemOpnd(op1, immVal, size);
            if (imm0->IsVary() || imm1->IsVary()) {
              ImmOperand *ofst = static_cast<ImmOperand *>(mo->GetOffsetOperand());
              ofst->SetVary(true);
            }
            Insn *ldInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(opc, res, mo);
            insn->bb->ReplaceInsn(insn, ldInsn);
            return true;
          }
        }
      }
      break;
    }  // end case MOP_xldr
    default:
      break;
  }
  return false;
}

/*
   *iii. mov w16, v10.s[1]   //  FMOV from simd 105   ---> replace_insn
       mov w1, w16     ----->insn
       ==>
       mov w1, v10.s[1]
 */
bool Riscv64Ebo::IsMovToSIMDVmov(Insn *insn, Insn *replaceInsn) {
  if (insn->GetMachineOpcode() == MOP_wmovrr && replaceInsn->GetMachineOpcode() == MOP_xvmovrs) {
    insn->mop_ = replaceInsn->GetMachineOpcode();
    return true;
  }
  return false;
}

/*
 * i.   fmov x10, d8             ----> replace_insn
 *      mov  x1, x10             ----> insn
 *      ==>
 *      fmov x1, d8
 * ii.  fmov w10, s8             ----> replace_insn
 *      mov  w1,  w10            ----> insn
 *      ==>
 *      fmov w1,  s8
 * iii. mov w16, v10.s[1]        ----> replace_insn
 *      mov w1, w16              ----> insn
 *      ==>
 *      mov w1, v10.s[1]
 */
bool Riscv64Ebo::ReplaceMovToVmov(Insn *insn, Insn *replaceInsn) {
  if ((insn->GetMachineOpcode() == MOP_xmovrr && replaceInsn->GetMachineOpcode() == MOP_xvmovrd) ||
      (insn->GetMachineOpcode() == MOP_wmovrr && replaceInsn->GetMachineOpcode() == MOP_xvmovrs)) {
    // Change opcode
    insn->mop_ = replaceInsn->GetMachineOpcode();
    return true;
  }
  return false;
}

bool Riscv64Ebo::ChangeLdrMop(Insn *insn, Operand *opnd) {
  CG_ASSERT(insn->IsLoad(), "");
  CG_ASSERT(opnd->IsRegister(), "");

  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  CG_ASSERT(static_cast<RegOperand *>(insn->GetOperand(0))->GetRegisterType() != regOpnd->GetRegisterType(), "");
  CHECK_FATAL(insn && insn->GetOpnd(0) && static_cast<MemOperand *>(insn->GetOpnd(0)), "null ptr check");
  if (static_cast<MemOperand *>(insn->GetOpnd(0))->GetIndexRegister()) {
    return false;
  }

  bool bRet = true;
  if (regOpnd->GetRegisterType() == kRegTyFloat) {
    switch (insn->mop_) {
      case MOP_wldr:
        insn->mop_ = MOP_sldr;
        break;
      case MOP_xldr:
        insn->mop_ = MOP_dldr;
        break;
      case MOP_wldrsb:
      case MOP_wldrsh:
      default:
        bRet = false;
        break;
    }
  } else if (regOpnd->GetRegisterType() == kRegTyInt) {
    switch (insn->mop_) {
      case MOP_sldr:
        insn->mop_ = MOP_wldr;
        break;
      case MOP_dldr:
        insn->mop_ = MOP_xldr;
        break;
      default:
        bRet = false;
        break;
    }
  } else {
    CG_ASSERT(false, "Internal error.");
    bRet = false;
  }

  return bRet;
}

/*
 * opnd_info[0]->insn
 * eor(89) (opnd0: vreg:R107 class: [I]) (opnd1: vreg:R108 class: [I]) (opnd2: vreg:R109 class: [I])
 * orig_info[0]->insn
 * str(213) (opnd0: vreg:R107 class: [I]) (opnd1: Mem:base: reg:R30 class: [I]offset:ofst:8)
 * insn
 * ldr(154) (opnd0: vreg:R113 class: [I]) (opnd1: Mem:base: reg:R30 class: [I]offset:ofst:8)
 *
 * store Rxx, base, offset
 * load  Rxx, base, offset
 *
 * load Rxx, base, offset
 * load Rxx, base, offset
 */
bool Riscv64Ebo::RemoveRedundantLoad(BB *bb, Insn *insn, Operand **opnds, OpndInfo **opndInfo, OpndInfo **origInfo) {
  if (insn == bb->firstinsn) {
    return false;
  }
  RegOperand *base = static_cast<Riscv64RegOperand *>(GetBase(insn));
  Operand *offset = GetOffset(insn);
  RegOperand *reg = GetRegOpnd(insn);
  if (!base || !offset || !reg || !IsFrameReg(base) || IsZeroRegister(reg)) {
    return false;
  }

  int32_t opndnum = insn->GetOpndNum();
  int32_t resnum = insn->GetResultNum();
  if (opndnum != 1 || resnum != 1) {
    return false;
  }

  MemOpndInfo *meminfo = static_cast<MemOpndInfo *>(origInfo[0]);
  if (meminfo == nullptr) {
    return false;
  }
  OpndInfo *baseinfo = meminfo->GetBaseInfo();
  OpndInfo *offinfo = meminfo->GetOffsetInfo();

  Insn *prevInsn = origInfo[0]->insn;
  if (!prevInsn || !prevInsn->AccessMem()) {
    return false;
  }
  InsnInfo *prevInsninfo = origInfo[0]->insninfo;
  MemOpndInfo *prevMeminfo = GetMemInfo(prevInsninfo);
  CHECK_FATAL(prevMeminfo, "null ptr check");
  OpndInfo *prevBaseinfo = prevMeminfo->GetBaseInfo();
  OpndInfo *prevOffinfo = prevMeminfo->GetOffsetInfo();

  RegOperand *prevBase = static_cast<Riscv64RegOperand *>(GetBase(prevInsn));
  Operand *prevOffset = GetOffset(prevInsn);
  RegOperand *prevReg = GetRegOpnd(prevInsn);
  OpndInfo *prevReginfo = prevInsn->IsStore() ? prevInsninfo->orig_opnd[0] : prevInsninfo->result[0];
  // prev_insn maybe str wzr mem.
  if (baseinfo != prevBaseinfo || offinfo != prevOffinfo || offset != prevOffset ||
      (prevReginfo && prevReginfo->redefined)) {
    return false;
  }

  // limited to the same bb until O2 RA is enabled
  if (origInfo[0]->bb != bb) {
    return false;
  }

  if (!reg->IsOfIntClass() || !prevReg->IsOfIntClass() || reg->GetSize() != prevReg->GetSize() ||
      (prevBase != nullptr && base->GetSize() != prevBase->GetSize()) || IsPhysicalReg(reg) || IsPhysicalReg(prevReg)) {
    return false;
  }

  // gen mov reg, prev_reg
  if ((cgfunc->mirModule.IsCModule()) &&
      ((insn->GetMachineOpcode() == MOP_wldrb) ||
       (insn->GetMachineOpcode() == MOP_wldrh))) {
    // C has unsigned value which can overflow.  To be safe, replace with a zero ext.
    // FIXME, replace insn with a zero extension
    return true;
  }
  bb->ReplaceInsn(insn, cgfunc->cg->BuildInstruction<Riscv64Insn>(SelectMovMop(reg, prevReg), reg, prevReg));
  return true;
}

MOperator Riscv64Ebo::SelectMovMop(RegOperand *dstOpnd, RegOperand *srcOpnd) {
  MOperator mop;
  uint32_t size = dstOpnd->GetSize();

  if (Riscv64isa::IsFPSIMDRegister((Riscv64reg_t)dstOpnd->GetRegisterNumber())) {
    if (Riscv64isa::IsFPSIMDRegister((Riscv64reg_t)srcOpnd->GetRegisterNumber())) {
      mop = size == 64 ? MOP_xvmovd : MOP_xvmovs;
    } else {
      mop = size == 64 ? MOP_xvmovdr : MOP_xvmovsr;
    }
  } else {
    if (Riscv64isa::IsFPSIMDRegister((Riscv64reg_t)srcOpnd->GetRegisterNumber())) {
      mop = size == 64 ? MOP_xvmovrd : MOP_xvmovrs;
    } else {
      mop = size == 64 ? MOP_xmovrr : MOP_wmovrr;
    }
  }
  return mop;
}

bool Riscv64Ebo::DeleteDupMemInsn(Insn *insn, OpndInfo **opndInfo, InsnInfo *insninfo, InsnInfo *predInfo) {
  if (cgfunc->mirModule.IsCModule()) {
    // C has memory aliasing issue. For both load and store removal, there can
    // have no intermediate load or store in between.  Major overhaul required.
    // Alias information is nice to have.
    return false;
  }
  Insn *prevInsn = insninfo->insn;
  if (!insn->AccessMem() || !prevInsn->AccessMem()) {
    return false;
  }

  int32_t resnum = insn->GetResultNum();
  if (resnum != 1) {
    return false;
  }

  resnum = prevInsn->GetResultNum();
  if (resnum != 1) {
    return false;
  }

  if (insn->bb != prevInsn->bb) {
    return false;
  }

  // Avoid load/store pairs now.
  if (GetRegNumForMem(prevInsn) != 1 || GetRegNumForMem(insn) != 1) {
    return false;
  }

  CHECK_FATAL(GetMemOpnd(insn) != nullptr, "GetMemOpnd(insn) is null in Riscv64Ebo::DeleteDupMemInsn");
  const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->mop_];
  uint32_t currSize = md->GetOperandSize();
  CHECK_FATAL(GetMemOpnd(prevInsn) != nullptr, "GetMemOpnd(prev_insn) is null in Riscv64Ebo::DeleteDupMemInsn");
  const Riscv64MD *prevMd = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(prevInsn)->mop_];
  uint32_t prevSize = prevMd->GetOperandSize();
  if (currSize != prevSize) {
    return false;
  }

  RegOperand *reg = GetRegOpnd(insn);
  RegOperand *prevReg = GetRegOpnd(prevInsn);
  if (before_regalloc && (IsPhysicalReg(reg) || IsPhysicalReg(prevReg))) {
    return false;
  }

  if (insn->IsLoad() && prevInsn->IsLoad()) {
    /* try to remove the second load */
    if (reg->GetRegisterType() != prevReg->GetRegisterType()) {
      return false;
    }
    if (insn->GetMachineOpcode() != prevInsn->GetMachineOpcode()) {
      return false;
    }
    if (!reg->IsOfIntClass() || IsZeroRegister(reg) || IsZeroRegister(prevReg)) {
      return false;
    }

    // gen mov reg, prev_reg
    Insn *newinsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(SelectMovMop(reg, prevReg), reg, prevReg);

    if (EBODUMP) {
      LogInfo::MapleLogger() << "< ==== Replace Insn :\n";
      insn->dump();
      LogInfo::MapleLogger() << " To be : =====>\n";
      newinsn->dump();
    }
    insn->bb->ReplaceInsn(insn, newinsn);
    return true;
  } else if (insn->IsLoad() && prevInsn->IsStore()) {
    /* try to use mov instead of load */
    if (reg->GetRegisterType() != prevReg->GetRegisterType() || IsZeroRegister(reg)) {
      return false;
    }
    if (!reg->IsOfIntClass()) {
      return false;
    }

    // gen mov reg, prev_reg
    Insn *newinsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(SelectMovMop(reg, prevReg), reg, prevReg);
    if (EBODUMP) {
      LogInfo::MapleLogger() << "< ==== Replace Insn :\n";
      insn->dump();
      LogInfo::MapleLogger() << " To be : =====>\n";
      newinsn->dump();
    }
    insn->bb->ReplaceInsn(insn, newinsn);
    return true;
  } else if (insn->IsStore() && prevInsn->IsStore()) {
    /* try to remove the first store */
    /* Skip the case:
        < -1 : 0x65ba07f0 > < 0 > str(248) (opnd0:  reg:R32 class: [I]) (opnd1: Mem:base: reg:R29 class:
       [I]offset:ofst:40) < -1 : 0x65b9fb08 > < 0 > stp(252) (opnd0:  reg:R32 class: [I]) (opnd1:  reg:R32 class: [I])
       (opnd2: Mem:base: reg:R29 class: [I]offset:ofst:40  intact) */
    if (insn->GetMachineOpcode() != prevInsn->GetMachineOpcode()) {
      return false;
    }
    if (reg->GetRegisterType() != prevReg->GetRegisterType()) {
      return false;
    }

    if (insninfo->mustnot_be_removed || prevInsn->do_not_remove) {
      return false;
    }

    // gen mov reg, prev_reg
    if (EBODUMP) {
      LogInfo::MapleLogger() << " Remove Duplicate store insn \n";
      prevInsn->dump();
    }
    prevInsn->bb->RemoveInsn(prevInsn);
    // remove the insninfo.
    InsnInfo *prevInfo = insninfo->prev;
    if (prevInfo) {
      prevInfo->next = insninfo->next;
    }
    if (insninfo->next) {
      insninfo->next->prev = prevInfo;
    }
    if (predInfo == nullptr) {
      SetInsnInfo(insninfo->hash_index, insninfo->same);
    } else {
      predInfo->same = insninfo->same;
    }
    return false;
  } else {
    /* not opt chance for load - store */
    // ldr(161) (opnd0: vreg:R680 class: [I] validBitNum: 64) (opnd1: Mem:base: reg:R29 class: [I] validBitNum:
    // 64offset:ofst:192) str(250) (opnd0: vreg:R680 class: [I] validBitNum: 64) (opnd1: Mem:base: reg:R29 class: [I]
    // validBitNum: 64offset:ofst:192)
    Riscv64RegOperand *reg1 = static_cast<Riscv64RegOperand *>(reg);
    Riscv64RegOperand *reg2 = static_cast<Riscv64RegOperand *>(prevReg);
    if ((*reg1) == (*reg2) && !insninfo->result[0]->redefined) {
      Insn *newinsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(SelectMovMop(reg, prevReg), reg, prevReg);
      if (EBODUMP) {
        LogInfo::MapleLogger() << "< ==== Remove insn who store the reg to the orig place. Replace Insn :\n";
        insn->dump();
        LogInfo::MapleLogger() << " with : =====>\n";
        newinsn->dump();
      }
      insn->bb->ReplaceInsn(insn, newinsn);
      return true;
    }
    return false;
  }
  return false;
}

bool Riscv64Ebo::DeleteDupInsn(Insn *insn, OpndInfo **opndInfo, InsnInfo *insninfo) {
  Insn *prevInsn = insninfo->insn;
  int32_t resnum = insn->GetResultNum();
  if (resnum != 1) {
    return false;
  }

  resnum = prevInsn->GetResultNum();
  if (resnum != 1) {
    return false;
  }

  if (insn->bb != prevInsn->bb) {
    return false;
  }

  if (insn->AccessMem() || prevInsn->AccessMem()) {
    return false;
  }

  MOperator insnMop = insn->GetMachineOpcode();
  if (insnMop == MOP_xuxtv64) {
    ImmOperand *imm0 = static_cast<ImmOperand *>(insn->GetOperand(2));
    ImmOperand *imm1 = static_cast<ImmOperand *>(prevInsn->GetOperand(2));
    if (imm0->GetValue() != imm1->GetValue()) {
      return false;
    }
  }

  // Return false if insn is clinit.
  if (insnMop == MOP_adrp_ldr && insn->do_not_remove) {
    return false;
  }

  RegOperand *reg = static_cast<RegOperand *>(insn->GetResult(0));
  RegOperand *prevReg = static_cast<RegOperand *>(prevInsn->GetResult(0));
  if (!reg || !prevReg || !reg->IsOfIntClass() || !prevReg->IsOfIntClass()) {
    return false;
  }

  if (EBODUMP) {
    LogInfo::MapleLogger() << "< === Dup insn was found, origin insn is ===> \n";
    insn->dump();
    insninfo->insn->dump();
  }
  Insn *newInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(SelectMovMop(reg, prevReg), reg, prevReg);
  insn->bb->ReplaceInsn(insn, newInsn);
  if (EBODUMP) {
    LogInfo::MapleLogger() << "< === replaced insn is ===> \n";
    newInsn->dump();
  }
  return true;
}

}  // namespace maplebe
