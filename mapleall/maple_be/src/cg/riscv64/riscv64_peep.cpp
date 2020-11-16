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

#include "riscv64_peep.h"
#include "cg.h"
#include "special_func.h"

namespace maplebe {
#define JAVALANG (cgfunc->mirModule.IsJavaModule())
/*
 * Match identical operands for memory access of type [base + offset] only.
 * Caller must check for the opcode.
 * Make sure the base address is not modified.
 */
// function will need to consider load and store doubles.
bool Riscv64Peep::IsMemOperandsIdentical(Insn *insn, Insn *ninsn) {
  regno_t regno1st = static_cast<RegOperand *>(insn->GetOperand(0))->GetRegisterNumber();
  regno_t regno2nd = static_cast<RegOperand *>(ninsn->GetOperand(0))->GetRegisterNumber();
  if (regno1st != regno2nd) {
    return false;
  }
  // Match only [base + offset]
  MemOperand *mopnd1st = static_cast<MemOperand *>(insn->GetOperand(1));
  MemOperand *mopnd2nd = static_cast<MemOperand *>(ninsn->GetOperand(1));

  Operand *base1st = mopnd1st->GetBaseRegister();
  Operand *base2nd = mopnd2nd->GetBaseRegister();
  if (!(base1st && base1st->IsRegister())) {
    return false;
  }
  if (!(base2nd && base2nd->IsRegister())) {
    return false;
  }

  regno_t baseRegno1st = static_cast<RegOperand *>(base1st)->GetRegisterNumber();
  // first insn re-write base addr   reg1 <- [ reg1 + offset ]
  if (baseRegno1st == regno1st) {
    return false;
  }

  regno_t baseRegno2nd = static_cast<RegOperand *>(base2nd)->GetRegisterNumber();
  if (baseRegno1st != baseRegno2nd) {
    return false;
  }

  if (static_cast<Riscv64MemOperand *>(mopnd1st)->GetOffsetImmediate()->GetOffsetValue() !=
      static_cast<Riscv64MemOperand *>(mopnd2nd)->GetOffsetImmediate()->GetOffsetValue()) {
    return false;
  }
  return true;
}

/* Looking for identical mem insn to eliminate.
 * if two back-to-back is:
 * 1. str + str
 * 2. str + ldr
 * And the [MEM] is pattern of [base + offset]
 * 1. the [MEM] operand is exactly same then first
      str can be eliminate.
 * 2. the [MEM] operand is exactly same and src opnd
      of str is same as the dest opnd of ldr then
      ldr can be eliminate
 */

void Riscv64Peep::RemoveIdenticalLoadAndStore() {
  FOR_ALL_BB(bb, cgfunc) {
    FOR_BB_INSNS_SAFE(insn, bb, ninsn) {
      if (!ninsn) {
        break;
      }

      MOperator mop1st = insn->GetMachineOpcode();
      MOperator mop2nd = ninsn->GetMachineOpcode();
      if ((mop1st == MOP_wstr && mop2nd == MOP_wstr) || (mop1st == MOP_xstr && mop2nd == MOP_xstr)) {
        if (IsMemOperandsIdentical(insn, ninsn)) {
          bb->RemoveInsn(insn);
        }
      } else if ((mop1st == MOP_wstr && mop2nd == MOP_wldr) || (mop1st == MOP_xstr && mop2nd == MOP_xldr)) {
        if (IsMemOperandsIdentical(insn, ninsn)) {
          bb->RemoveInsn(ninsn);
          ninsn = insn;
        }
      }
    }
  }
}

void Riscv64Peep::Peephole0() {
  RemoveIdenticalLoadAndStore();
  DeleteMovAfterCbzOrCbnz();
}

/* We optimize the following pattern in this function:
 * A: Remove redundant mov which src and dest opnd is exactly same
 * B: Combining 2 STRs into 1 stp or 2 LDRs into 1 ldp, when they are
      back to back and the [MEM] they access is conjointed.
   C: Eliminate the sxt[b|h|w] w0, w0;, when w0 is satisify following:
      i)  mov w0, #imm (#imm is not out of range)
      ii) ldrs[b|h] w0, [MEM]
 * D: Eliminate the uxt[b|h|w] w0, w0;when w0 is satisify following:
      i)  mov w0, #imm (#imm is not out of range)
      ii) mov w0, R0(Is return value of call and return size is not of range)
      iii)w0 is defined and used by special load insn and uxt[] pattern
 * E: fmov ireg1 <- freg1   previous insn
      fmov ireg2 <- freg1   current insn
      use  ireg2            may or may not be present
      =>
      fmov ireg1 <- freg1   previous insn
      mov  ireg2 <- ireg1   current insn
      use  ireg1            may or may not be present
 * F: cbnz x0, labelA
      mov x0, 0
      b  return-bb
      labelA:
      =>
      cbz x0, return-bb
      labelA:
 * G: When exist load after load or load after store, and [MEM] is
      totally same. Then optimize them.
 */
void Riscv64Peep::Peephole() {
  RemoveIdenticalLoadAndStore();
  Riscv64CGFunc *acgfunc = static_cast<Riscv64CGFunc *>(cgfunc);

  // Delete insn moving the same register, this will simplify the following two passes.
  FOR_ALL_BB(bb, acgfunc) {
    FOR_BB_INSNS_SAFE(insn, bb, ninsn) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }

      MOperator thisMop = insn->GetMachineOpcode();
      switch (thisMop) {
        case MOP_wmovrr:
        case MOP_xmovrr:
        case MOP_xvmovs:
        case MOP_xvmovd: {
          CG_ASSERT(insn->GetOperand(0)->IsRegister() && insn->GetOperand(1)->IsRegister(), "expects registers");
          Riscv64RegOperand *reg1 = static_cast<Riscv64RegOperand *>(insn->GetOperand(0));
          Riscv64RegOperand *reg2 = static_cast<Riscv64RegOperand *>(insn->GetOperand(1));
          CG_ASSERT((reg1 && reg2), "invalid insn");

          if ((reg1->GetRegisterNumber() == reg2->GetRegisterNumber()) && (reg1->GetSize() == reg2->GetSize())) {
            bb->RemoveInsn(insn);
          }
        } break;
      }
    }
  }

  // To save compilation performance, we will have at most 2
  // iterations through the basic blocks, one pass forward and
  // one pass backward

  FOR_ALL_BB(bb, cgfunc) {
    Insn *prevInsn = nullptr;
    // Forward scan
    FOR_BB_INSNS_SAFE(insn, bb, ninsn) {
      MOperator thisMop = insn->GetMachineOpcode();

      if (!insn->IsMachineInstruction()) {
        continue;
      }

      switch (thisMop) {
        case MOP_xldr:
        case MOP_xstr:
        case MOP_wldr:
        case MOP_wstr:
        case MOP_dldr:
        case MOP_dstr:
        case MOP_sldr:
        case MOP_sstr:
          {
          }
          break;
        case MOP_xvmovrs:
        case MOP_xvmovrd:
          {
          }
          break;
        case MOP_bnez: {
          /*         bnez x0, labelA
           *         mov x0, 0
           *         b  return-bb
           *   labelA:
           *
           * into
           *         beqz x0, return-bb
           *   labelA:
           */
          CG_ASSERT(ninsn == nullptr, "peep: next inst is not null");
          // reg has to be R0, since return value is in R0
          RegOperand *regOpnd0 = static_cast<RegOperand *>(insn->GetOperand(0));
          if (regOpnd0->GetRegisterNumber() != R0) {
            break;
          }
          BB *nbb = bb->next;
          // Make sure nbb can only be reached by bb
          if (nbb->preds.size() > 1 || nbb->eh_preds.size() != 0) {
            break;
          }

          BB *tbb = nullptr;
          auto it = bb->succs.begin();
          if (*it == nbb) {
            ++it;
            tbb = *it;
          } else {
            tbb = *it;
          }
          // Make sure when nbb is empty, tbb is fallthru of bb.
          if (tbb != nbb->next) {
            break;
          }
          // Is nbb branch to the return-bb?
          if (nbb->succs.size() != 1) {
            break;
          }
          BB *nbbTarget = *(nbb->succs.begin());
          if (nbbTarget->kind != BB::kBBReturn) {
            break;
          }

          // Next insn should be a mov R0 = 0
          Insn *nextinsn = nbb->firstinsn;
          while (nextinsn && !nextinsn->IsMachineInstruction()) {
            nextinsn = nextinsn->next;
          }
          if (!nextinsn) {
            break;
          }
          MOperator nextinsnMop = nextinsn->GetMachineOpcode();
          if (nextinsnMop != MOP_xmovri64) {
            break;
          }
          RegOperand *movDest = static_cast<RegOperand *>(nextinsn->GetOperand(0));
          if (movDest->GetRegisterNumber() != R0) {
            break;
          }
          ImmOperand *movImm = static_cast<ImmOperand *>(nextinsn->GetOperand(1));
          if (movImm->GetValue() != 0) {
            break;
          }
          Insn *nninsn = nextinsn->next;
          while (nninsn && !nninsn->IsMachineInstruction()) {
            nninsn = nninsn->next;
          }
          if (!nninsn) {
            break;
          }
          if (nninsn->GetMachineOpcode() != MOP_xuncond) {
            break;
          }
          // Control flow looks nice, instruction looks nice
          Operand *brTarget = nninsn->GetOperand(0);
          insn->SetOperand(1, brTarget);
          insn->SetMOP(MOP_beqz);
          nbb->RemoveInsn(nextinsn);
          nbb->RemoveInsn(nninsn);
          // nbb is now a fallthru bb, not a goto bb
          nbb->SetKind(BB::kBBFallthru);

          // fix control flow, we have bb, nbb, tbb, nbb_target
          // connect bb -> nbb_target erase tbb
          it = bb->succs.begin();
          if (*it == tbb) {
            bb->succs.erase(it);
            bb->succs.push_front(nbbTarget);
          } else {
            it++;
            bb->succs.erase(it);
            bb->succs.push_back(nbbTarget);
          }
          for (it = tbb->preds.begin(); it != tbb->preds.end(); it++) {
            if (*it == bb) {
              tbb->preds.erase(it);
              break;
            }
          }
          for (it = nbbTarget->preds.begin(); it != nbbTarget->preds.end(); it++) {
            if (*it == nbb) {
              nbbTarget->preds.erase(it);
              break;
            }
          }
          nbbTarget->preds.push_back(bb);

          // nbb has no target, originally just branch target
          nbb->succs.erase(nbb->succs.begin());
          CG_ASSERT(nbb->succs.size() == 0, "peep: branch target incorrect");
          // Now make nbb fallthru to tbb
          nbb->succs.push_front(tbb);
          tbb->preds.push_back(nbb);
          break;
        }
        default:
          break;
      }  // switch
    }    // Insn

    // Backward scan
    for (Insn *insn = bb->lastinsn; insn && insn != bb->firstinsn; insn = prevInsn) {
      prevInsn = insn->prev;
      while (prevInsn && !prevInsn->GetMachineOpcode() && prevInsn != bb->firstinsn) {
        prevInsn = prevInsn->prev;
      }

      if (!insn->IsMachineInstruction() || !prevInsn) {
        continue;
      }

      bool loadafterstore = false;
      bool loadafterload = false;
      MOperator thisMop = insn->GetMachineOpcode();
      MOperator prevMop = prevInsn->GetMachineOpcode();

      // store regB, RegC, offset
      // load regA, RegC, offset
      if ((thisMop == MOP_xldr && prevMop == MOP_xstr) || (thisMop == MOP_wldr && prevMop == MOP_wstr) ||
          (thisMop == MOP_dldr && prevMop == MOP_dstr) || (thisMop == MOP_sldr && prevMop == MOP_sstr)) {
        loadafterstore = true;
      }

      // load regA, RegC, offset
      // load regB, RegC, offset
      if ((thisMop == MOP_xldr || thisMop == MOP_wldr || thisMop == MOP_dldr || thisMop == MOP_sldr) &&
          prevMop == thisMop) {
        loadafterload = true;
      }

      if (loadafterstore || loadafterload) {
        CG_ASSERT(insn->GetOperand(1)->IsMemoryAccessOperand() && prevInsn->GetOperand(1)->IsMemoryAccessOperand(),
                  "expects mem operands");

        Riscv64RegOperand *reg1 = static_cast<Riscv64RegOperand *>(insn->opnds[0]);
        Riscv64MemOperand *mopnd1 = static_cast<Riscv64MemOperand *>(insn->GetOperand(1));

        Riscv64RegOperand *base1 = static_cast<Riscv64RegOperand *>(mopnd1->GetBaseRegister());
        CG_ASSERT(base1 == nullptr || !base1->IsVirtualRegister(), "physical register has not been allocated?");
        Riscv64OfstOperand *offset1 = mopnd1->GetOffsetImmediate();

        Riscv64RegOperand *reg2 = static_cast<Riscv64RegOperand *>(prevInsn->opnds[0]);
        Riscv64MemOperand *mopnd2 = static_cast<Riscv64MemOperand *>(prevInsn->GetOperand(1));

        Riscv64RegOperand *base2 = static_cast<Riscv64RegOperand *>(mopnd2->GetBaseRegister());
        CG_ASSERT(base2 == nullptr || !base2->IsVirtualRegister(), "physical register has not been allocated?");
        Riscv64OfstOperand *offset2 = mopnd2->GetOffsetImmediate();

        if (base1 == nullptr || base2 == nullptr || offset1 == nullptr || offset2 == nullptr) {
          continue;
        }

        int offsetVal1 = offset1->GetOffsetValue();
        int offsetVal2 = offset2->GetOffsetValue();

        if (base1->GetRegisterNumber() == base2->GetRegisterNumber() &&
            reg1->GetRegisterType() == reg2->GetRegisterType() && reg1->GetSize() == reg2->GetSize() &&
            offsetVal1 == offsetVal2) {
          if (loadafterstore && reg1->GetRegisterNumber() != reg2->GetRegisterNumber()) {
            // replace it with mov
            MOperator newOp = MOP_wmovrr;
            if (reg1->GetRegisterType() == kRegTyInt) {
              newOp = reg1->GetSize() <= 32 ? MOP_wmovrr : MOP_xmovrr;
            } else if (reg1->GetRegisterType() == kRegTyFloat) {
              newOp = reg1->GetSize() <= 32 ? MOP_xvmovs : MOP_xvmovd;
            }

            bb->InsertInsnAfter(prevInsn, cg->BuildInstruction<Riscv64Insn>(newOp, reg1, reg2));
            bb->RemoveInsn(insn);
          } else if (reg1->GetRegisterNumber() == reg2->GetRegisterNumber() &&
                     base1->GetRegisterNumber() != reg2->GetRegisterNumber()) {
            bb->RemoveInsn(insn);
          }
        }
      }
    }  // Insn

  }  // BB

  return;
}

void Riscv64Peep::PostRemoveSext() {
  Riscv64CGFunc *acgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    if (bb->firstinsn && bb->firstinsn->GetMachineOpcode() == MOP_wmovrr) {
      RegOperand *reg1 = static_cast<RegOperand *>(bb->firstinsn->opnds[1]);
      if (reg1->GetRegisterNumber() == R10 &&
          bb->preds.size() == 1 && bb->preds.front() == bb->prev &&
          (bb->prev->lastinsn->GetMachineOpcode() == MOP_xbl ||
           bb->prev->lastinsn->GetMachineOpcode() == MOP_xblr)) {
        Insn *nextinsn = bb->firstinsn->GetNextMachineInsn();
        if (nextinsn) {
          RegOperand *reg0 = static_cast<RegOperand *>(nextinsn->opnds[0]);
          if (reg0->GetRegisterNumber() != R10) {
            if (nextinsn->GetMachineOpcode() == MOP_xsxtw64) {
              nextinsn->SetMOP(MOP_xmovrr);
            } else if (nextinsn->GetMachineOpcode() == MOP_xsxtv64) {
              nextinsn->SetMOP(MOP_xmovrr);
              nextinsn->SetOperand(2, nullptr);
            }
          }
        }
      }
    }
  }
}

void Riscv64Peep::RemoveSext() {
  Riscv64CGFunc *acgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    FOR_BB_INSNS_REV_SAFE(insn, bb, ninsn) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      Insn *previnsn = insn->GetPreviousMachineInsn();
      if (previnsn == nullptr) {
        continue;
      }
      if (insn->GetMachineOpcode() == MOP_xsxtw64) {
        switch (previnsn->GetMachineOpcode()) {
        case MOP_xmovri64:
        case MOP_waddrri12:
        case MOP_waddrrr:
        case MOP_xsxtv64:
        case MOP_xslt:
        case MOP_xsltu:
        case MOP_xslti:
        case MOP_xsltiu:
        case MOP_xsgt:
        case MOP_xsgtu:
        case MOP_xseqz:
        case MOP_xsnez:
        case MOP_wldrsw: {
          RegOperand *reg1 = static_cast<RegOperand *>(insn->opnds[1]);
          RegOperand *reg2 = static_cast<RegOperand *>(previnsn->opnds[0]);
          if (IsSameReg(reg1, reg2)) {
            insn->SetMOP(MOP_xmovrr);
          }
          break;
          }
        }
        Insn *prevprevinsn = previnsn->GetPreviousMachineInsn();
        if (prevprevinsn == nullptr) {
          continue;
        }
        switch (prevprevinsn->GetMachineOpcode()) {
        case MOP_xmovri64:
        case MOP_waddrri12:
        case MOP_waddrrr:
        case MOP_xsxtv64:
        case MOP_xseqz:
        case MOP_xsnez:
        case MOP_wldrsw: {
          RegOperand *reg1 = static_cast<RegOperand *>(insn->opnds[1]);
          RegOperand *reg2 = static_cast<RegOperand *>(prevprevinsn->opnds[0]);
          if (IsSameReg(reg1, reg2)) {
            insn->SetMOP(MOP_xmovrr);
          }
          break;
          }
        }
      } else if (insn->GetMachineOpcode() == MOP_xsxtv64) {
        switch (previnsn->GetMachineOpcode()) {
        case MOP_wldrsb:
          if (static_cast<ImmOperand *>(insn->opnds[2])->GetValue() == 56) {
            insn->SetMOP(MOP_xmovrr);
            insn->SetOperand(2, nullptr);
          }
          break;
        case MOP_wldrsh:
          if (static_cast<ImmOperand *>(insn->opnds[2])->GetValue() == 48) {
            insn->SetMOP(MOP_xmovrr);
            insn->SetOperand(2, nullptr);
          }
          break;
        }
      }
    }
  }
}

void Riscv64Peep::ConvertPseudoOps() {
  Riscv64CGFunc *acgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    FOR_BB_INSNS(insn, bb) {
      switch(insn->GetMachineOpcode()) {
      case MOP_xsxtv64: {
        RegOperand *resopnd = static_cast<RegOperand *>(insn->opnds[0]);
        RegOperand *opnd0 = static_cast<RegOperand *>(insn->opnds[1]);
        ImmOperand *imm = static_cast<ImmOperand *>(insn->opnds[2]);
        bb->InsertInsnBefore(insn, cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, resopnd, opnd0, imm));
        bb->InsertInsnBefore(insn, cg->BuildInstruction<Riscv64Insn>(MOP_xasrrri6, resopnd, resopnd, imm));
        bb->RemoveInsn(insn);
        break;
        }
      case MOP_xuxtv64: {
        RegOperand *resopnd = static_cast<RegOperand *>(insn->opnds[0]);
        RegOperand *opnd0 = static_cast<RegOperand *>(insn->opnds[1]);
        ImmOperand *imm = static_cast<ImmOperand *>(insn->opnds[2]);
        bb->InsertInsnBefore(insn, cg->BuildInstruction<Riscv64Insn>(MOP_xlslrri6, resopnd, opnd0, imm));
        bb->InsertInsnBefore(insn, cg->BuildInstruction<Riscv64Insn>(MOP_xlsrrri6, resopnd, resopnd, imm));
        bb->RemoveInsn(insn);
        break;
        }
      case MOP_xsubrri12: {
        int64 val = static_cast<ImmOperand *>(insn->opnds[2])->GetValue();
        ImmOperand *imm = acgfunc->CreateImmOperand(-val, 8, true);
        insn->SetOperand(2, imm);
        insn->SetMOP(MOP_xaddrri12);
        break;
        }
      case MOP_wsubrri12: {
        int64 val = static_cast<ImmOperand *>(insn->opnds[2])->GetValue();
        ImmOperand *imm = acgfunc->CreateImmOperand(-val, 8, true);
        insn->SetOperand(2, imm);
        insn->SetMOP(MOP_waddrri12);
        break;
        }
      }
    }
  }
}

void Riscv64Peep::PeepholeOpt() {
  PostRemoveSext();
  Peephole();
}

bool Riscv64Peep::IsSameReg(Operand *firstOpnd, Operand *secondOpnd) {
  CG_ASSERT(firstOpnd->IsRegister() && secondOpnd->IsRegister(),
            "first_opnd and second_opnd should be Register Operand");
  RegOperand *firstReg = static_cast<RegOperand *>(firstOpnd);
  RegOperand *secondReg = static_cast<RegOperand *>(secondOpnd);
  return firstReg->RegNumEqual(secondReg);
}

bool Riscv64Peep::PredBBCheck(BB *bb, bool checkcbz, Operand *opnd) {
  if (bb->GetKind() != BB::kBBIf) {
    return false;
  }

  Insn *condbr = cgfunc->theCFG->FindLastCondBrInsn(bb);
  CG_ASSERT(condbr, "condbr must be found");
  if (!cgfunc->theCFG->IsCompareAndBranchInsn(condbr)) {
    return false;
  }
  MOperator mop = condbr->GetMachineOpcode();
  if (checkcbz && mop != MOP_beqz) {
    return false;
  }
  if (!checkcbz && mop != MOP_bnez) {
    return false;
  }
  return IsSameReg(condbr->GetOperand(0), opnd);
}

/* help function for DeleteMovAfterCbzOrCbnz
 * input:
 *        bb: the bb to be checked out
 *        checkcbz: to check out BB end with cbz or cbnz, if cbz, input true
 *        opnd: for MOV reg, #0, opnd indicate reg
 **return:
 *        according to cbz, return true if insn is cbz or cbnz and the first operand of cbz(cbnz) is same as input
 *operand
 */
bool Riscv64Peep::OpndDefByMovZero(Insn *insn) {
  MOperator defMop = insn->GetMachineOpcode();
  switch (defMop) {
    case MOP_xmovri64: {
      Operand *defOpnd = insn->GetOperand(1);
      CG_ASSERT(defOpnd->IsIntImmediate(), "expects ImmOperand");
      ImmOperand *defConst = static_cast<ImmOperand *>(defOpnd);
      int64 defConstValue = defConst->GetValue();
      if (defConstValue == 0) {
        return true;
      }
      return false;
    }
    case MOP_xmovrr:
    case MOP_wmovrr: {
      Operand *secondOpnd = insn->GetOperand(1);
      CG_ASSERT(secondOpnd && secondOpnd->IsRegister(), "expects RegOperand here");
      Riscv64RegOperand *regOpnd = static_cast<Riscv64RegOperand *>(secondOpnd);
      return regOpnd->IsZeroRegister();
    }
    default:
      return false;
  }
}

/*
 *check wether predefine insn of first operand of test_insn is exist in current BB
 * */
bool Riscv64Peep::NoPreDefine(Insn *testInsn) {
  Insn *ninsn = nullptr;
  for (Insn *insn = testInsn->bb->firstinsn; insn && insn != testInsn; insn = ninsn) {
    ninsn = insn->GetNextMachineInsn();
    if (!insn->IsMachineInstruction()) {
      continue;
    }
    CG_ASSERT(!insn->IsCall(), "CG internal error, call insn should not be at the middle of the BB.");
    const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->mop_];
    for (int i = 0; i < Insn::kMaxOperandNum; i++) {
      Operand *opnd = insn->opnds[i];
      if (opnd == nullptr) {
        continue;
      }
      Riscv64OpndProp *regprop = static_cast<Riscv64OpndProp *>(md->operand_[i]);
      if (regprop->IsDef()) {
        if (opnd->IsMemoryAccessOperand() == false) {
          CG_ASSERT(opnd->IsRegister(), "expects RegOperand");
          if (IsSameReg(testInsn->GetOperand(0), opnd)) {
            return false;
          }
        }
      }
    }
  }
  return true;
}

/* cbnz w0, @label
 * ....
 * mov  w0, #0 (elseBB)        -->this instruction can be deleted
 *
 * cbz  w0, @label
 * ....
 * mov  w0, #0 (ifBB)          -->this instruction can be deleted
 *
 * condition:
 *  1.there is not predefine points of w0 in elseBB(ifBB)
 *  2.the first opearnd of cbnz insn is same as the first Operand of mov insn
 *  3.w0 is defined by move 0
 *  4.all preds of elseBB(ifBB) end with cbnz or cbz
 *
 *  NOTE: if there are multiple preds and there is not define point of w0 in one pred,
 *        (mov w0, 0) can't be deleted, avoiding use before def.
 */
void Riscv64Peep::DeleteMovAfterCbzOrCbnz() {
  cgfunc->theCFG->InitInsnVisitor(cgfunc, cgfunc->memPool);
  FOR_ALL_BB(bb, cgfunc) {
    if (bb->GetKind() != BB::kBBIf) {
      continue;
    }

    Insn *condbr = cgfunc->theCFG->FindLastCondBrInsn(bb);
    CG_ASSERT(condbr, "condbr must be found");
    if (!cgfunc->theCFG->IsCompareAndBranchInsn(condbr)) {
      continue;
    }
    BB *processBb = nullptr;
    MOperator condbrMop = condbr->GetMachineOpcode();
    if (condbrMop == MOP_bnez) {
      processBb = bb->next;
    } else {
      processBb = cgfunc->theCFG->GetTargetSuc(bb);
    }

    CG_ASSERT(processBb != nullptr, "process_bb is null in Riscv64Peep::DeleteMovAfterCbzOrCbnz");
    Insn *ninsn = nullptr;
    for (Insn *insn = processBb->firstinsn; insn; insn = ninsn) {
      ninsn = insn->GetNextMachineInsn();
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      if (OpndDefByMovZero(insn) && NoPreDefine(insn) && IsSameReg(insn->GetOperand(0), condbr->GetOperand(0))) {
        bool toDoOpt = true;
        // process elseBB, other preds must be cbz
        if (condbrMop == MOP_bnez) {
          // check out all preds of process_bb
          for (auto processBbPred : processBb->preds) {
            if (processBbPred == bb) {
              continue;
            }
            if (!PredBBCheck(processBbPred, true, insn->GetOperand(0))) {
              toDoOpt = false;
              break;
            }
          }
        } else {
          if (processBb->preds.size() == 1) {
            // conditional-bb target and fallthru is the same bb
            toDoOpt = false;
          }
          // process ifBB, other preds can be cbz or cbnz(one at most)
          for (auto processBbPred : processBb->preds) {
            if (processBbPred == bb) {
              continue;
            }
            // for cbnz pred, there is one at most
            if (processBbPred == processBb->prev) {
              if (!PredBBCheck(processBbPred, false, insn->GetOperand(0))) {
                toDoOpt = false;
                break;
              }
            } else {
              // for cbz pred
              if (!PredBBCheck(processBbPred, true, insn->GetOperand(0))) {
                toDoOpt = false;
                break;
              }
            }
          }
        }
        if (!toDoOpt) {
          continue;
        }
        processBb->RemoveInsn(insn);
      }
    }
  }
}

bool Riscv64Peep::OpndDefByOneValidBit(Insn *defInsn) {
  CG_ASSERT(defInsn, "def_insn must not be null");
  MOperator defMop = defInsn->GetMachineOpcode();
  switch (defMop) {
    case MOP_xslt:
    case MOP_xsltu:
    case MOP_xslti:
    case MOP_xsltiu:
    case MOP_xsgt:
    case MOP_xsgtu:
    case MOP_xseqz:
    case MOP_xsnez:
      return true;
    case MOP_xmovri64: {
      Operand *defOpnd = defInsn->GetOperand(1);
      CG_ASSERT(defOpnd->IsIntImmediate(), "expects ImmOperand");
      ImmOperand *defConst = static_cast<ImmOperand *>(defOpnd);
      int64 defConstValue = defConst->GetValue();
      if (defConstValue != 0 && defConstValue != 1) {
        return false;
      } else {
        return true;
      }
    }
    case MOP_xmovrr:
    case MOP_wmovrr:
      CG_ASSERT(defInsn && defInsn->GetOperand(1), "Operand must not be null");
      return defInsn->GetOperand(1)->IsZeroRegister();
    case MOP_wlsrrri5:
    case MOP_xlsrrri6: {
      Operand *opnd2 = defInsn->GetOperand(2);
      CG_ASSERT(opnd2 && opnd2->IsIntImmediate(), "expects ImmOperand");
      ImmOperand *opndimm = static_cast<ImmOperand *>(opnd2);
      int64 shiftbits = opndimm->GetValue();
      if ((defMop == MOP_wlsrrri5 && shiftbits == 31) || (defMop == MOP_xlsrrri6 && shiftbits == 63)) {
        return true;
      } else {
        return false;
      }
    }
    default:
      return false;
  }
}

// if there is define point of checkinsn->GetOperand(opndidx) between startinsn and  bb->firstinsn
// return define insn. else return nullptr
Insn *Riscv64Peep::DefInsnOfOperandInBB(BB *bb, Insn *startinsn, Insn *checkinsn, int opndidx) {
  Insn *previnsn = nullptr;
  for (Insn *insn = startinsn; insn && insn != bb->firstinsn->prev; insn = previnsn) {
    previnsn = insn->GetPreviousMachineInsn();
    if (!insn->IsMachineInstruction()) {
      continue;
    }
    // checkinsn->GetOperand(opndidx) is thought modified conservatively
    if (insn->IsCall()) {
      return insn;
    }
    const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->mop_];
    for (int i = 0; i < Insn::kMaxOperandNum; i++) {
      Operand *opnd = insn->opnds[i];
      if (opnd == nullptr) {
        continue;
      }
      Riscv64OpndProp *regprop = static_cast<Riscv64OpndProp *>(md->operand_[i]);
      if (regprop->IsDef()) {
        // Operand is base reg of Memory, defined by str
        if (opnd->IsMemoryAccessOperand() == false) {
          CG_ASSERT(opnd->IsRegister(), "expects RegOperand");
          if (IsSameReg(checkinsn->GetOperand(opndidx), opnd)) {
            return insn;
          }
        }
      }
    }
  }
  return nullptr;
}

void Riscv64Peep::PrePeepholeOpt() {
  RemoveSext();
  ReplaceInstruction();
}

void Riscv64Peep::PrePeepholeOpt1() {
}

/* orr  w21, w0, #0  ====> mov  w21, w0
   orr  w21, #0, w0  ====> mov  w21, w0
 */
void Riscv64Peep::ReplaceInstruction() {
  Riscv64CGFunc *acgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    FOR_BB_INSNS(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      Operand *opndOfOrr = nullptr;
      Operand *opnd1OfMov = nullptr;
      Operand *opnd2OfMov = nullptr;
      ImmOperand *immOpnd = nullptr;
      Riscv64RegOperand *reg1 = nullptr;
      Riscv64RegOperand *reg2 = nullptr;
      MOperator thisMop = insn->GetMachineOpcode();
      switch (thisMop) {
        case MOP_xiorrri13: {  // opnd1 is reg64 and opnd3 is immediate.
          opndOfOrr = insn->opnds[2];
          CG_ASSERT(opndOfOrr->IsIntImmediate(), "expects immediate operand");
          immOpnd = static_cast<ImmOperand *>(opndOfOrr);
          if (0 == immOpnd->GetValue()) {
            reg1 = static_cast<Riscv64RegOperand *>(insn->opnds[0]);
            reg2 = static_cast<Riscv64RegOperand *>(insn->opnds[1]);
            bb->ReplaceInsn(insn, cg->BuildInstruction<Riscv64Insn>(MOperator(MOP_xmovrr), reg1, reg2));
          }
          break;
        }
        default:
          break;
      }
    }
  }
}

AnalysisResult *CgDoPrePeepHole::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  LiveAnalysis *live = nullptr;
  // It doesn't need live range information when -O1, because the register will not live out of bb.
  if (g->optim_level >= 2) {
    live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));
  }
  Riscv64Peep *peep = new Riscv64Peep(cgfunc);
  peep->PrePeepholeOpt();
  delete peep;
  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  return nullptr;
}

AnalysisResult *CgDoPrePeepHole1::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (cgfunc->GetRDStatus()) {
    cgfunc->GetRD()->ClearDefUseInfo();
    m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
    m->InvalidAnalysisResult(CgFuncPhase_REACHDEF, cgfunc);
  }
  Riscv64Peep *peep = new Riscv64Peep(cgfunc);
  peep->PrePeepholeOpt1();
  delete peep;
  return nullptr;
}

AnalysisResult *CgDoPeepHole0::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  LiveAnalysis *live = nullptr;
  if (g->optim_level >= 2) {
    live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));
  }
  Riscv64Peep *peep = new Riscv64Peep(cgfunc);
  peep->Peephole0();
  delete peep;
  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  return nullptr;
}

AnalysisResult *CgDoPeepHole::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  LiveAnalysis *live = nullptr;
  if (g->optim_level >= 2) {
    live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));
  }
  Riscv64Peep *peep = new Riscv64Peep(cgfunc);
  peep->PeepholeOpt();
  delete peep;
  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  return nullptr;
}

AnalysisResult *CgFixShortBranch::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  Riscv64Peep *peep = new Riscv64Peep(cgfunc);
  peep->ConvertPseudoOps();
  delete peep;
  return nullptr;
}

}  // namespace maplebe
