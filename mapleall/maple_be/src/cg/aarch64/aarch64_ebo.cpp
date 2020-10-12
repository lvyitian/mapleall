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

#include "aarch64_ebo.h"
#include "aarch64_cg.h"
#include "cg_assert.h"
namespace maplebe {

using namespace maple;
#define EBODUMP CGDEBUGFUNC(cgfunc)
#define CLANG (cgfunc->func->module->IsCModule())

// return the regno of the low 32bit. e.g opnd= d10, return v10.s[0]
regno_t AArch64Ebo::GetLowVec(Operand *opnd) {
  CG_ASSERT(opnd && opnd->IsRegister(), "");
  regno_t regno = static_cast<RegOperand *>(opnd)->GetRegisterNumber();
  return regno - V0 + VB32;
}

// return the regno of the high 32bit. e.g opnd= d10, return v10.s[1]
regno_t AArch64Ebo::GetHighVec(Operand *opnd) {
  CG_ASSERT(opnd && opnd->IsRegister(), "");
  regno_t regno = static_cast<RegOperand *>(opnd)->GetRegisterNumber();
  return regno - V0 + VB64;
}

bool AArch64Ebo::IsFloatReg(RegOperand *regopnd) {
  regno_t regno = regopnd->GetRegisterNumber();
  return (regno >= V0 && regno <= V31);
}

bool AArch64Ebo::IsFmov(Insn *insn) {
  return (insn->mop_ >= MOP_xvmovsr && insn->mop_ <= MOP_xvmovrd);
}

void AArch64Ebo::SetOpnd(Insn *insn, int i, Operand *opnd) {
  if (insn->IsDestRegAlsoSrcReg()) {
    insn->SetOperand(i, opnd);
    return;
  }
  const AArch64MD *md = &AArch64CG::kMd[insn->mop_];
  if (md->IsStore()) {
    insn->SetOperand(i, opnd);
  } else {
    insn->SetOperand(insn->GetResultNum() + i, opnd);
  }
}

bool AArch64Ebo::IsAdd(Insn *insn) {
  return (insn->mop_ >= MOP_xaddrrr && insn->mop_ <= MOP_ssub);
}

bool AArch64Ebo::IsCmp(Insn *insn) {
  return (insn->mop_ >= MOP_wcmpri && insn->mop_ <= MOP_xcmprr);
}

bool AArch64Ebo::IsVecReg(Operand *opnd) {
  if (!opnd->IsRegister()) {
    return false;
  }
  AArch64RegOperand *a64reg = static_cast<AArch64RegOperand *>(opnd);
  return a64reg->IsSimdVectorMode();
}

bool AArch64Ebo::IsZeroRegister(Operand *opnd) {
  if (!opnd->IsRegister()) {
    return false;
  }
  AArch64RegOperand *aarreg = static_cast<AArch64RegOperand *>(opnd);
  return aarreg->IsZeroRegister();
}

bool AArch64Ebo::IsBranchCmpSpecial(Insn *insn) {
  return ((insn->GetMachineOpcode() >= MOP_wcmpri) && (insn->GetMachineOpcode() <= MOP_xcmprr));
}

bool AArch64Ebo::IsImplicit(Insn *insn) {
  const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
  if (md->IsCondDef()) {
    return true;
  }
  for (uint32_t i = 0; i < AArch64Insn::kMaxOperandNum; i++) {
    Operand *op = insn->GetOperand(i);
    if (op && op->IsConditionCode()) {
      return true;
    }
  }
  return false;
}

bool AArch64Ebo::IsClinitCheck(Insn *insn) {
  if (insn == nullptr) {
    return false;
  }
  MOperator mop = insn->GetMachineOpcode();
  return mop == MOP_clinit || mop == MOP_clinit_tail;
}

void AArch64Ebo::InitCallerSaveRegisters() {
  if (initCallerSaveRegs) {
    return;
  }
  initCallerSaveRegs = true;
  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  for (int i = R0; i <= R18; i++) {
    regOperand[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)i, 64, kRegTyInt);
  }
  for (int i = V0; i <= V7; i++) {
    regOperand[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)i, 64, kRegTyFloat);
  }
  for (int i = V16; i <= V31; i++) {
    regOperand[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)i, 64, kRegTyFloat);
  }
}

void AArch64Ebo::InitCallAndReturnUseRegisters() {
  if (initCallAndReturnRegs) {
    return;
  }
  initCallAndReturnRegs = true;
  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  for (int i = R19; i <= R30; i++) {
    regOperand[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)i, 64, kRegTyInt);
  }
  for (int i = V8; i <= V15; i++) {
    regOperand[i] = a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)i, 64, kRegTyFloat);
  }
  regOperand[RSP] = a64cgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)RSP, 64, kRegTyInt);
}

void AArch64Ebo::DefineClinitSpecialRegisters(InsnInfo *insninfo) {
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

void AArch64Ebo::DefineCallerSaveRegisters(InsnInfo *insninfo) {
  RegOperand *phyopnd = nullptr;
  Insn *insn = insninfo->insn;

  CG_ASSERT(insn->IsCall(), "insn should be a call insn.");

  // Define return register.
  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  InitCallerSaveRegisters();
  phyopnd = regOperand[R0];
  OpndInfo *opndinfo = OperandInfoDef(insn->bb, insn, phyopnd);
  opndinfo->insninfo = insninfo;

  phyopnd = regOperand[V0];
  opndinfo = OperandInfoDef(insn->bb, insn, phyopnd);
  opndinfo->insninfo = insninfo;

  // Define scalar caller save register.
  for (int i = R1; i <= R18; i++) {
    phyopnd = regOperand[i];
    opndinfo = OperandInfoDef(insn->bb, insn, phyopnd);
    opndinfo->insninfo = insninfo;
  }

  // Define FP caller save registers.
  for (int i = V1; i <= V7; i++) {
    phyopnd = regOperand[i];
    opndinfo = OperandInfoDef(insn->bb, insn, phyopnd);
    opndinfo->insninfo = insninfo;
  }
  for (int i = V16; i <= V31; i++) {
    phyopnd = regOperand[i];
    opndinfo = OperandInfoDef(insn->bb, insn, phyopnd);
    opndinfo->insninfo = insninfo;
  }
}

void AArch64Ebo::DefineReturnUseRegister(Insn *insn) {
  RegOperand *phyopnd = nullptr;

  // Define return register.
  AArch64CGFunc *a64cgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  InitCallAndReturnUseRegisters();

  // Define scalar callee save register and FP, LR.
  for (int i = R19; i <= R30; i++) {
    phyopnd = regOperand[i];
    OperandInfoUse(insn->bb, insn, phyopnd);
  }

  // Define SP
  phyopnd = regOperand[RSP];
  OperandInfoUse(insn->bb, insn, phyopnd);

  // Define FP caller save registers.
  for (int i = V8; i <= V15; i++) {
    phyopnd = regOperand[i];
    OperandInfoUse(insn->bb, insn, phyopnd);
  }
}

void AArch64Ebo::DefineCallUseSpecialRegister(Insn *insn) {
  RegOperand *phyopnd = nullptr;

  // Define return register.
  InitCallAndReturnUseRegisters();

  // Define FP, LR.
  for (int i = R29; i <= R30; i++) {
    phyopnd = regOperand[i];
    OperandInfoUse(insn->bb, insn, phyopnd);
  }

  // Define SP
  phyopnd = regOperand[RSP];
  OperandInfoUse(insn->bb, insn, phyopnd);
}

/*return true if op1 == op2*/
bool AArch64Ebo::OperandEqSpecial(Operand *op1, Operand *op2) {
  switch (op1->GetKind()) {
    case Operand::Opd_Register: {
      AArch64RegOperand *reg1 = static_cast<AArch64RegOperand *>(op1);
      AArch64RegOperand *reg2 = static_cast<AArch64RegOperand *>(op2);
      return (*reg1) == (*reg2);
    }
    case Operand::Opd_Immediate: {
      ImmOperand *imm1 = static_cast<ImmOperand *>(op1);
      ImmOperand *imm2 = static_cast<ImmOperand *>(op2);
      return (*imm1) == (*imm2);
    }
    case Operand::Opd_OfstImmediate: {
      AArch64OfstOperand *ofst1 = static_cast<AArch64OfstOperand *>(op1);
      AArch64OfstOperand *ofst2 = static_cast<AArch64OfstOperand *>(op2);
      return (*ofst1) == (*ofst2);
    }
    case Operand::Opd_StImmediate: {
      StImmOperand *stimm1 = static_cast<StImmOperand *>(op1);
      StImmOperand *stimm2 = static_cast<StImmOperand *>(op2);
      return (*stimm1) == (*stimm2);
    }
    case Operand::Opd_Mem: {
      AArch64MemOperand *mem1 = static_cast<AArch64MemOperand *>(op1);
      AArch64MemOperand *mem2 = static_cast<AArch64MemOperand *>(op2);
      return ((mem1->GetAddrMode() == mem2->GetAddrMode()) &&
              OperandEqual(mem1->GetBaseRegister(), mem2->GetBaseRegister()) &&
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

int32_t AArch64Ebo::GetOffsetVal(MemOperand *mem) {
  AArch64MemOperand *memopnd = static_cast<AArch64MemOperand *>(mem);
  AArch64OfstOperand *offset = memopnd->GetOffsetImmediate();
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
bool AArch64Ebo::DoConstProp(Insn *insn, int idx, Operand *opnd) {
  AArch64ImmOperand *src = static_cast<AArch64ImmOperand *>(opnd);
  // move vreg, 0
  // store vreg, mem
  // ===>
  // store wzr, mem
  CG_ASSERT(insn->GetOpnd(idx), "");
  const AArch64MD *md = &AArch64CG::kMd[(insn->mop_)];
  // avoid the invalid case "cmp wzr, #0"/"add w1, wzr, #100"
  if (src->IsZero() && insn->GetOpnd(idx)->IsRegister() && (insn->IsStore() || insn->IsMove() || md->IsCondDef())) {
    SetOpnd(insn, idx, GetZeroOpnd(src->GetSize()));
    return true;
  }
  MOperator mopcode = insn->GetMachineOpcode();
  switch (mopcode) {
    case MOP_wiorrrr:
    case MOP_xiorrrr: {
      if (src->GetValue() == 0) {
        MOperator mop = idx == 0 ? (mopcode == MOP_wiorrrr) ? MOP_wiorri12r : MOP_xiorri13r
                                 : (mopcode == MOP_wiorrrr) ? MOP_wiorrri12 : MOP_xiorrri13;
        insn->mop_ = mop;
        uint32 tSize = (mopcode == MOP_wiorrrr) ? 12 : 13;
        src->SetSize(tSize);
        insn->SetOperand(idx + 1, src);
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
        src = static_cast<AArch64ImmOperand *>(src->Clone(cgfunc->memPool));
        CG_ASSERT(src != nullptr, "");
        src->size_ = targetSize;
      }
      if (src->IsSingleInstructionMovable()) {
        if (EBODUMP) {
          LogInfo::MapleLogger() << " Do constprop:Prop constVal " << src->GetValue() << "into insn:\n";
          insn->dump();
        }
        insn->SetOperand(1, src);
        MOperator mop = (mopcode == MOP_wmovrr) ? MOP_xmovri32 : MOP_xmovri64;
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
      if (idx != 1 || !src->IsInBitSize(24) || !(src->IsInBitSize(12) || src->IsInBitSize(12, 12))) {
        return false;
      }
      if (insn->GetResult(0) == nullptr) {
        return false;
      }
      bool is64bits = (insn->GetResult(0)->GetSize() == 64);
      if (EBODUMP) {
        LogInfo::MapleLogger() << " Do constprop:Prop constVal " << src->GetValue() << "into insn:\n";
        insn->dump();
      }
      if (src->IsZero()) {
        insn->SetOperand(2, nullptr);
        insn->mop_ = is64bits ? MOP_xmovrr : MOP_wmovrr;
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

/*Constant folding*/
bool AArch64Ebo::DoConstantFold(Insn *insn, Operand **opnds, OpndInfo **opndInfo) {
  MOperator opc = insn->GetMachineOpcode();

  if (insn->GetOpndNum() == 0) {
    return false;
  }

  Operand *res = insn->GetResult(0);

  CG_ASSERT(res && res->IsRegister(), "");
  // only do integers
  RegOperand *reg = static_cast<RegOperand *>(res);
  if (!reg->IsOfIntClass()) {
    return false;
  }
  // csel ->cset
  if (opc == MOP_wcselrrrc || opc == MOP_xcselrrrc) {
    Operand *op0 = opnds[0];
    Operand *op1 = opnds[1];
    AArch64ImmOperand *imm0 = nullptr;
    AArch64ImmOperand *imm1 = nullptr;
    if (op0->IsImmediate()) {
      imm0 = static_cast<AArch64ImmOperand *>(op0);
    }
    if (op1->IsImmediate()) {
      imm1 = static_cast<AArch64ImmOperand *>(op1);
    }

    bool reverse = imm1 && imm1->IsOne() && ((imm0 && imm0->IsZero()) || op0->IsZeroRegister());
    if ((imm0 && imm0->IsOne() && ((imm1 && imm1->IsZero()) || op1->IsZeroRegister())) || reverse) {
      if (EBODUMP) {
        LogInfo::MapleLogger() << "change csel insn :\n";
        insn->dump();
      }
      Operand *result = insn->GetResult(0);
      Operand *condOperand = insn->GetOperand(3);
      Insn *newinsn = nullptr;
      if (!reverse) {
        newinsn = cgfunc->cg->BuildInstruction<AArch64Insn>(opc == MOP_xcselrrrc ? MOP_xcsetrc : MOP_wcsetrc, result,
                                                            condOperand);
      } else {
        CondOperand *cond = static_cast<CondOperand *>(condOperand);
        if (!CheckCondCode(cond)) {
          return false;
        }
        AArch64CGFunc *aarfunc = static_cast<AArch64CGFunc *>(cgfunc);
        CondOperand *reverseCond = aarfunc->GetCondOperand(GetReverseCond(cond));
        newinsn = cgfunc->cg->BuildInstruction<AArch64Insn>(opc == MOP_xcselrrrc ? MOP_xcsetrc : MOP_wcsetrc, result,
                                                            reverseCond);
      }
      if (EBODUMP) {
        LogInfo::MapleLogger() << "to cset insn ====>\n";
        newinsn->dump();
      }
      insn->bb->ReplaceInsn(insn, newinsn);
      return true;
    }

    return false;
  }
  //  int64_t resval, val0, val1;
  return false;
}

/*
 * Look at an exression that has a constant second operand and attempt to
 * simplify the computations.
 */
bool AArch64Ebo::ConstantOperand(Insn *insn, Operand **opnds, OpndInfo **opndInfo) {
  BB *bb = insn->bb;
  bool retval = false;
  if (insn->GetOpndNum() < 1) {
    return false;
  }
  Operand *op0 = opnds[0];
  Operand *op1 = opnds[1];
  Operand *res = insn->GetResult(0);
  CHECK_FATAL(res, "null ptr check");
  const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
  uint32_t dsize = md->GetOperandSize();
  bool first = op0->IsConstant() && !op1->IsConstant();
  CG_ASSERT((op1->IsConstant() && !op0->IsConstant()) || (op0->IsConstant() && !op1->IsConstant()), "");
  AArch64ImmOperand *imm = nullptr;
  Operand *op = nullptr;
  int32_t idx0 = 0;
  if (first) {
    imm = static_cast<AArch64ImmOperand *>(op0);
    op = op1;
    if (op->IsMemoryAccessOperand()) {
      op = insn->GetOpnd(1);
    }
    idx0 = 1;
  } else {
    imm = static_cast<AArch64ImmOperand *>(op1);
    op = op0;
    if (op->IsMemoryAccessOperand()) {
      op = insn->GetOpnd(0);
    }
  }
  // For orr insn and one of the opnd is zero
  if ((insn->GetMachineOpcode() == MOP_wiorrri12 || insn->GetMachineOpcode() == MOP_xiorrri13 ||
       insn->GetMachineOpcode() == MOP_xiorri13r || insn->GetMachineOpcode() == MOP_wiorri12r) &&
      imm->IsZero()) {
    bb->ReplaceInsn(insn, cgfunc->cg->BuildInstruction<AArch64Insn>(dsize == 64 ? MOP_xmovrr : MOP_wmovrr, res, op));
    return true;
  }
  // For the imm is 0. Then replace the insn by a move insn.
  if ((insn->GetMachineOpcode() >= MOP_xaddrrr && insn->GetMachineOpcode() <= MOP_sadd && imm->IsZero()) ||
      (!first && insn->GetMachineOpcode() >= MOP_xsubrrr && insn->GetMachineOpcode() <= MOP_ssub && imm->IsZero())) {
    bb->ReplaceInsn(insn, cgfunc->cg->BuildInstruction<AArch64Insn>(dsize == 64 ? MOP_xmovrr : MOP_wmovrr, res, op));
    return true;
  }

  if (insn->GetMachineOpcode() == MOP_xaddrrr || insn->GetMachineOpcode() == MOP_waddrrr) {
    if (imm->IsInBitSize(24)) {
      // ADD Wd|WSP, Wn|WSP, #imm{, shift} ; 32-bit general registers
      // ADD Xd|SP,  Xn|SP,  #imm{, shift} ; 64-bit general registers
      // imm : 0 ~ 4095, shift: none, LSL #0, or LSL #12
      // aarch64 assembly takes up to 24-bits, if the lower 12 bits is all 0
      if ((imm->IsInBitSize(12) || imm->IsInBitSize(12, 12))) {
        bb->ReplaceInsn(
          insn, cgfunc->cg->BuildInstruction<AArch64Insn>(dsize == 64 ? MOP_xaddrri12 : MOP_waddrri12, res, op, imm));
        retval = true;
      }
    }
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
      AArch64ImmOperand *imm0 = static_cast<AArch64ImmOperand *>(prev->GetOpnd(1));
      CHECK_FATAL(imm0 != nullptr, "imm0 is null in AArch64Ebo::ConstantOperand ");
      int64_t val = imm0->GetValue() + imm->GetValue();
      AArch64CGFunc *aarchfunc = static_cast<AArch64CGFunc *>(cgfunc);
      AArch64ImmOperand *imm1 = aarchfunc->CreateImmOperand(val, dsize, imm0->IsSignedValue());
      if (imm1->IsInBitSize(24) && (imm1->IsInBitSize(12) || imm1->IsInBitSize(12, 12))) {
        bb->ReplaceInsn(insn, cgfunc->cg->BuildInstruction<AArch64Insn>(dsize == 64 ? MOP_xaddrri12 : MOP_waddrri12,
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
bool AArch64Ebo::ResoveCondBranch(Insn *insn, Operand **opnds) {
  BB *bb = insn->bb;
  // Only concern with conditional branch.
  if (bb == nullptr || bb->succs.size() != 2) {
    return false;
  }
  return false;
}

AArch64CC_t AArch64Ebo::GetReverseCond(CondOperand *cond) {
  AArch64CC_t ccCode = kCcLast;
  switch (cond->GetCode()) {
    case CC_NE:
      ccCode = CC_EQ;
      break;
    case CC_EQ:
      ccCode = CC_NE;
      break;
    case CC_LT:
      ccCode = CC_GE;
      break;
    case CC_GE:
      ccCode = CC_LT;
      break;
    case CC_GT:
      ccCode = CC_LE;
      break;
    case CC_LE:
      ccCode = CC_GT;
      break;
    default:
      CG_ASSERT(0, "Not support yet.");
      break;
  }
  return ccCode;
}

/*return true if cond == CC_LE*/
bool AArch64Ebo::CheckCondCode(CondOperand *cond) {
  switch (cond->GetCode()) {
    case CC_NE:
    case CC_EQ:
    case CC_LT:
    case CC_GE:
    case CC_GT:
    case CC_LE:
      return true;
    default:
      return false;
  }
  return false;
}

/* Do some special pattern*/
bool AArch64Ebo::SpecialSequence(Insn *insn, Operand **opnds, OpndInfo **origInfo) {
  if (insn == nullptr) {
    return false;
  }
  MOperator opc = insn->GetMachineOpcode();
  AArch64CGFunc *aarchfunc = static_cast<AArch64CGFunc *>(cgfunc);
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
        Insn *newInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(insn->GetMachineOpcode(), reg1, reg2);
        insn->bb->ReplaceInsn(insn, newInsn);
        return true;
      }
      break;
    }
    //  lsl     x1, x1, #3
    //  add     x0, x0, x1
    // ===> add x0, x0, x1, 3({MOP_xaddrrrs,
    // {MOPD_Reg64ID,MOPD_Reg64IS,MOPD_Reg64IS,MOPD_BitShift64,MOPD_Undef},0,"add","0,1,2,3", 1, 3})
    case MOP_xaddrrr:
    case MOP_waddrrr: {
      if (insn->GetResult(0) == nullptr) {
        return false;
      }
      bool is64bits = (insn->GetResult(0)->GetSize() == 64);
      Operand *op0 = opnds[0];
      OpndInfo *opndinfo = origInfo[1];
      if (opndinfo && opndinfo->insn) {
        Insn *insn1 = opndinfo->insn;
        InsnInfo *insninfo1 = opndinfo->insninfo;
        MOperator opc1 = insn1->GetMachineOpcode();
        if (opc1 == MOP_xlslrri6 || opc1 == MOP_wlslrri5) {
          // don't use register if it was redefined.
          OpndInfo *opndinfo1 = insninfo1->orig_opnd[0];
          if (opndinfo1 && opndinfo1->redefined) {
            return false;
          }
          Operand *res = insn->GetOperand(0);
          Operand *op1 = insn1->GetOperand(1);
          AArch64ImmOperand *imm = static_cast<AArch64ImmOperand *>(insn1->GetOperand(2));
          CG_ASSERT(imm != nullptr, "");
          Operand *shiftOpnd =
            aarchfunc->CreateBitShiftOperand(BitShiftOperand::LSL, imm->GetValue(), opc1 == MOP_xlslrri6 ? 6 : 5);
          insn->bb->ReplaceInsn(insn, cgfunc->cg->BuildInstruction<AArch64Insn>(is64bits ? MOP_xaddrrrs : MOP_waddrrrs,
                                                                                res, op0, op1, shiftOpnd));
          return true;
        }
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
      const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
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
          AArch64ImmOperand *imm0 = static_cast<AArch64ImmOperand *>(memopnd->GetOffset());
          if (!imm0) {
            return false;
          }
          int64 imm0Val = imm0->GetValue();
          Operand *res = insn->GetOperand(0);
          RegOperand *op1 = static_cast<RegOperand *>(insn1->GetOperand(1));
          AArch64ImmOperand *imm1 = static_cast<AArch64ImmOperand *>(insn1->GetOperand(2));
          CG_ASSERT(imm1 != nullptr, "");
          int64 immVal;
          // don't use register if it was redefined.
          OpndInfo *opndinfo1 = insninfo1->orig_opnd[0];
          if (opndinfo1 && opndinfo1->redefined) {
            // return false;
            // add x2, x1, imm0, LSL imm1
            // add x2, x2, imm2
            // ldr x3, [x2]
            // -> ldr x3, [x1, imm]
            // ----------------------------
            // add x2, x1, imm0, LSL imm1
            // add x2, x2, imm2
            // str x3, [x2]
            // -> str x3, [x1, imm]
            Insn *insn2 = opndinfo1->insn;
            if (!insn2) {
              return false;
            }
            MOperator opc2 = insn2->GetMachineOpcode();
            if (opc2 != MOP_xaddrri24 && opc2 != MOP_waddrri24) {
              return false;
            }
            RegOperand *res2 = static_cast<RegOperand *>(insn2->GetOperand(0));
            RegOperand *base2 = static_cast<RegOperand *>(insn2->GetOperand(1));
            AArch64ImmOperand *imm2 = static_cast<AArch64ImmOperand *>(insn2->GetOperand(2));
            RegOperand *res1 = static_cast<RegOperand *>(insn1->GetOperand(0));
            if (RegistersIdentical(res1, op1) && RegistersIdentical(res1, res2) && GetOpndInfo(base2, -1) != nullptr &&
                GetOpndInfo(base2, -1)->redefined == false) {
              immVal = imm0Val + imm1->GetValue() + ((imm2->GetValue()) << 12);
              op1 = base2;
            } else {
              return false;
            }
          } else {
            immVal = imm0Val + imm1->GetValue();
          }
          if ((!is64bits && immVal < STR_LDR_IMM32_UPPER_BOUND && immVal % 4 == 0) ||
              (is64bits && immVal < STR_LDR_IMM64_UPPER_BOUND && immVal % 8 == 0)) {
            MemOperand *mo = aarchfunc->CreateMemOpnd(op1, immVal, size);
            Insn *ldInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(opc, res, mo);
            insn->bb->ReplaceInsn(insn, ldInsn);
            return true;
          }
        }
      }
      break;
    }  // end case MOP_xldr
    case MOP_xcsetrc:
    case MOP_wcsetrc: {
      /* i.   cmp     x0, x1
              cset    w0, EQ     ===> cmp x0, x1
              cmp     w0, #0          cset w0, EQ
              cset    w0, NE

         ii.  cmp     x0, x1
              cset    w0, EQ     ===> cmp x0, x1
              cmp     w0, #0          cset w0, NE
              cset    w0, EQ

         a.< -1 : 0x20ff25e0 > < 0 > cmp(226) (opnd0: vreg:C105 class: [CC]) (opnd1: vreg:R104 class: [I]) (opnd2:
         vreg:R106 class: [I]) b.< -1 : 0x20ff60a0 > < 0 > cset(72) (opnd0: vreg:R101 class: [I]) (opnd1: CC: EQ) c.< -1
         : 0x20ff3870 > < 0 > cmp(223) (opnd0: vreg:C105 class: [CC]) (opnd1: vreg:R101 class: [I]) (opnd2: imm:0) d.<
         -1 : 0x20ff3908 > < 0 > cset(72) (opnd0: vreg:R107 class: [I]) (opnd1: CC: NE) d1.< -1 : 0x20ff3908 > < 0 >
         cset(72) (opnd0: vreg:R107 class: [I]) (opnd1: CC: EQ) i, d ===> mov R107 R101 ii, a,b,c,d1 ===> a,b,cset Rxx
         NE, c, mov R107 Rxx  */
      CondOperand *cond = static_cast<CondOperand *>(insn->GetOperand(1));
      if (cond->GetCode() != CC_NE && cond->GetCode() != CC_EQ) {
        return false;
      }
      bool reverse = (cond->GetCode() == CC_EQ);
      OpndInfo *condinfo = origInfo[0];
      if (condinfo && condinfo->insn) {
        Insn *cmp1 = condinfo->insn;
        if (cmp1->GetMachineOpcode() == MOP_xcmpri || cmp1->GetMachineOpcode() == MOP_wcmpri) {
          InsnInfo *cmpinfo1 = condinfo->insninfo;
          OpndInfo *info0 = cmpinfo1->orig_opnd[0];
          // if R101 was not redefined.
          if (info0 && info0->insninfo && info0->insn && (reverse || !info0->redefined) &&
              cmp1->GetOperand(2)->IsImmediate()) {
            Insn *csetInsn = info0->insn;
            MOperator opc1 = csetInsn->GetMachineOpcode();
            if ((opc1 == MOP_xcsetrc || opc1 == MOP_wcsetrc) &&
                static_cast<ImmOperand *>(cmp1->GetOperand(2))->IsZero()) {
              CondOperand *cond1 = static_cast<CondOperand *>(csetInsn->GetOperand(1));
              if (!CheckCondCode(cond1)) {
                return false;
              }
              if (EBODUMP) {
                LogInfo::MapleLogger() << "< === do specical condition optimization, replace insn  ===> \n";
                insn->dump();
              }
              Operand *result = insn->GetResult(0);
              CHECK_FATAL(result, "pointor result is null");
              uint32_t size = result->GetSize();
              Insn *newInsn = nullptr;
              if (reverse) {
                // After regalloction, we can't create a new register.
                if (!before_regalloc) {
                  return false;
                }
                AArch64CGFunc *aarfunc = static_cast<AArch64CGFunc *>(cgfunc);
                Operand *r =
                  aarfunc->CreateRegisterOperandOfType(static_cast<RegOperand *>(result)->GetRegisterType(), size / 8);
                CondOperand *cond2 = aarfunc->GetCondOperand(GetReverseCond(cond1));
                Insn *newCset = cgfunc->cg->BuildInstruction<AArch64Insn>(
                  result->GetSize() == 64 ? MOP_xcsetrc : MOP_wcsetrc, r, cond2);
                // new_cset use the same cond as cset_insn.
                IncRef(info0->insninfo->orig_opnd[0]);
                csetInsn->bb->InsertInsnAfter(csetInsn, newCset);
                newInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(result->GetSize() == 64 ? MOP_xmovrr : MOP_wmovrr,
                                                                    result, r);
                insn->bb->ReplaceInsn(insn, newInsn);
              } else {
                Operand *result1 = csetInsn->GetResult(0);
                newInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(result->GetSize() == 64 ? MOP_xmovrr : MOP_wmovrr,
                                                                    result, result1);
                insn->bb->ReplaceInsn(insn, newInsn);
              }
              if (EBODUMP) {
                LogInfo::MapleLogger() << "< === with new insn ===> \n";
                newInsn->dump();
              }
              return true;
            }
          }
        }
      }
    }  // end case MOP_wcsetrc
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
bool AArch64Ebo::IsMovToSIMDVmov(Insn *insn, Insn *replaceInsn) {
  if (insn->GetMachineOpcode() == MOP_wmovrr && replaceInsn->GetMachineOpcode() == MOP_xvmovrv) {
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
bool AArch64Ebo::ReplaceMovToVmov(Insn *insn, Insn *replaceInsn) {
  if ((insn->GetMachineOpcode() == MOP_xmovrr && replaceInsn->GetMachineOpcode() == MOP_xvmovrd) ||
      (insn->GetMachineOpcode() == MOP_wmovrr && replaceInsn->GetMachineOpcode() == MOP_xvmovrs) ||
      (insn->GetMachineOpcode() == MOP_wmovrr && replaceInsn->GetMachineOpcode() == MOP_xvmovrv)) {
    // Change opcode
    insn->mop_ = replaceInsn->GetMachineOpcode();
    return true;
  }
  return false;
}

bool AArch64Ebo::ChangeLdrMop(Insn *insn, Operand *opnd) {
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
      case MOP_wldrb:
        insn->mop_ = MOP_bldr;
        break;
      case MOP_wldrh:
        insn->mop_ = MOP_hldr;
        break;
      case MOP_wldr:
        insn->mop_ = MOP_sldr;
        break;
      case MOP_xldr:
        insn->mop_ = MOP_dldr;
        break;
      case MOP_wldli:
        insn->mop_ = MOP_sldli;
        break;
      case MOP_xldli:
        insn->mop_ = MOP_dldli;
        break;
      case MOP_wldrsb:
      case MOP_wldrsh:
      default:
        bRet = false;
        break;
    }
  } else if (regOpnd->GetRegisterType() == kRegTyInt) {
    switch (insn->mop_) {
      case MOP_bldr:
        insn->mop_ = MOP_wldrb;
        break;
      case MOP_hldr:
        insn->mop_ = MOP_wldrh;
        break;
      case MOP_sldr:
        insn->mop_ = MOP_wldr;
        break;
      case MOP_dldr:
        insn->mop_ = MOP_xldr;
        break;
      case MOP_sldli:
        insn->mop_ = MOP_wldli;
        break;
      case MOP_dldli:
        insn->mop_ = MOP_xldli;
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
bool AArch64Ebo::RemoveRedundantLoad(BB *bb, Insn *insn, Operand **opnds, OpndInfo **opndInfo, OpndInfo **origInfo) {
  if (CLANG) {
    return false;
  }
  if (insn == bb->firstinsn) {
    return false;
  }
  RegOperand *base = static_cast<AArch64RegOperand *>(GetBase(insn));
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

  RegOperand *prevBase = static_cast<AArch64RegOperand *>(GetBase(prevInsn));
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
    MOperator mop = (insn->GetMachineOpcode() == MOP_wldrb) ? MOP_xuxtb32 : MOP_xuxth32;
    bb->ReplaceInsn(insn, cgfunc->cg->BuildInstruction<AArch64Insn>(mop, reg, prevReg));
    return true;
  }
  bb->ReplaceInsn(insn, cgfunc->cg->BuildInstruction<AArch64Insn>(SelectMovMop(reg, prevReg), reg, prevReg));
  return true;
}

MOperator AArch64Ebo::SelectMovMop(RegOperand *dstOpnd, RegOperand *srcOpnd) {
  MOperator mop;
  uint32_t size = dstOpnd->GetSize();

  if (AArch64isa::IsFPSIMDRegister((AArch64reg_t)dstOpnd->GetRegisterNumber())) {
    if (AArch64isa::IsFPSIMDRegister((AArch64reg_t)srcOpnd->GetRegisterNumber())) {
      mop = size == 64 ? MOP_xvmovd : MOP_xvmovs;
    } else {
      mop = size == 64 ? MOP_xdfmovrr : MOP_wsfmovrr;
    }
  } else {
    if (AArch64isa::IsFPSIMDRegister((AArch64reg_t)srcOpnd->GetRegisterNumber())) {
      mop = size == 64 ? MOP_dxfmovrr : MOP_swfmovrr;
    } else {
      mop = size == 64 ? MOP_xmovrr : MOP_wmovrr;
    }
  }
  return mop;
}

bool AArch64Ebo::DeleteDupMemInsn(Insn *insn, OpndInfo **opndInfo, InsnInfo *insninfo, InsnInfo *predInfo) {
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

  CHECK_FATAL(GetMemOpnd(insn) != nullptr, "GetMemOpnd(insn) is null in AArch64Ebo::DeleteDupMemInsn");
  const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
  uint32_t currSize = md->GetOperandSize();
  CHECK_FATAL(GetMemOpnd(prevInsn) != nullptr, "GetMemOpnd(prev_insn) is null in AArch64Ebo::DeleteDupMemInsn");
  const AArch64MD *prevMd = &AArch64CG::kMd[static_cast<AArch64Insn *>(prevInsn)->mop_];
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
    Insn *newinsn = cgfunc->cg->BuildInstruction<AArch64Insn>(SelectMovMop(reg, prevReg), reg, prevReg);

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
    Insn *newinsn = cgfunc->cg->BuildInstruction<AArch64Insn>(SelectMovMop(reg, prevReg), reg, prevReg);
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
    AArch64RegOperand *reg1 = static_cast<AArch64RegOperand *>(reg);
    AArch64RegOperand *reg2 = static_cast<AArch64RegOperand *>(prevReg);
    if ((*reg1) == (*reg2) && !insninfo->result[0]->redefined) {
      Insn *newinsn = cgfunc->cg->BuildInstruction<AArch64Insn>(SelectMovMop(reg, prevReg), reg, prevReg);
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

bool AArch64Ebo::DeleteDupInsn(Insn *insn, OpndInfo **opndInfo, InsnInfo *insninfo) {
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
  if (insnMop == MOP_xcsetrc || insnMop == MOP_wcsetrc) {
    return false;
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
  Insn *newInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(SelectMovMop(reg, prevReg), reg, prevReg);
  insn->bb->ReplaceInsn(insn, newInsn);
  if (EBODUMP) {
    LogInfo::MapleLogger() << "< === replaced insn is ===> \n";
    newInsn->dump();
  }
  return true;
}

}  // namespace maplebe
