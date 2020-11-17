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

#include "aarch64_peep.h"
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
bool AArch64Peep::IsMemOperandsIdentical(Insn *insn, Insn *ninsn) {
  regno_t regno1st = static_cast<RegOperand *>(insn->GetOperand(0))->GetRegisterNumber();
  regno_t regno2nd = static_cast<RegOperand *>(ninsn->GetOperand(0))->GetRegisterNumber();
  if (regno1st != regno2nd) {
    return false;
  }
  // Match only [base + offset]
  MemOperand *mopnd1st = static_cast<MemOperand *>(insn->GetOperand(1));
  if (static_cast<AArch64MemOperand *>(mopnd1st)->GetAddrMode() != AArch64MemOperand::kAddrModeBOi) {
    return false;
  }
  MemOperand *mopnd2nd = static_cast<MemOperand *>(ninsn->GetOperand(1));
  if (static_cast<AArch64MemOperand *>(mopnd2nd)->GetAddrMode() != AArch64MemOperand::kAddrModeBOi) {
    return false;
  }

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

  if (static_cast<AArch64MemOperand *>(mopnd1st)->GetOffsetImmediate()->GetOffsetValue() !=
      static_cast<AArch64MemOperand *>(mopnd2nd)->GetOffsetImmediate()->GetOffsetValue()) {
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

void AArch64Peep::RemoveIdenticalLoadAndStore() {
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

void AArch64Peep::Peephole0() {
  RemoveIdenticalLoadAndStore();
  CmpCsetOptimize();
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
void AArch64Peep::Peephole() {
  RemoveIdenticalLoadAndStore();
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);

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
          AArch64RegOperand *reg1 = static_cast<AArch64RegOperand *>(insn->GetOperand(0));
          AArch64RegOperand *reg2 = static_cast<AArch64RegOperand *>(insn->GetOperand(1));
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
            // Combining 2 STRs into 1 stp or 2 LDRs into 1 ldp
            if (ninsn && ninsn->GetMachineOpcode() == thisMop) {
              CG_ASSERT(insn->GetOperand(1)->IsMemoryAccessOperand() && ninsn->GetOperand(1)->IsMemoryAccessOperand(),
                        "expects mem operands");

              AArch64RegOperand *reg1 = static_cast<AArch64RegOperand *>(insn->opnds[0]);
              CG_ASSERT(reg1 != nullptr, "static_cast can't return nullptr");
              AArch64MemOperand *mopnd1 = static_cast<AArch64MemOperand *>(insn->GetOperand(1));

              AArch64MemOperand::AArch64AddressingMode addrMode1 = mopnd1->GetAddrMode();
              if (addrMode1 != AArch64MemOperand::kAddrModeBOi || (!mopnd1->IsIntactIndexed())) {
                continue;
              }

              AArch64RegOperand *base1 = static_cast<AArch64RegOperand *>(mopnd1->GetBaseRegister());
              CG_ASSERT(base1 == nullptr || !base1->IsVirtualRegister(), "physical register has not been allocated?");
              AArch64OfstOperand *offset1 = mopnd1->GetOffsetImmediate();

              AArch64RegOperand *reg2 = static_cast<AArch64RegOperand *>(ninsn->opnds[0]);
              AArch64MemOperand *mopnd2 = static_cast<AArch64MemOperand *>(ninsn->GetOperand(1));

              AArch64MemOperand::AArch64AddressingMode addrMode2 = mopnd2->GetAddrMode();
              if (addrMode2 != AArch64MemOperand::kAddrModeBOi || (!mopnd2->IsIntactIndexed())) {
                continue;
              }

              AArch64RegOperand *base2 = static_cast<AArch64RegOperand *>(mopnd2->GetBaseRegister());
              CG_ASSERT(base2 == nullptr || !base2->IsVirtualRegister(), "physical register has not been allocated?");
              AArch64OfstOperand *offset2 = mopnd2->GetOffsetImmediate();

              if (base1 == nullptr || base2 == nullptr || offset1 == nullptr || offset2 == nullptr) {
                continue;
              }

              // In ARM Architecture Reference Manual ARMv8, for ARMv8-A architecture profile
              // LDP on page K1-6125 delcare that ldp can't use same reg
              if ((thisMop == MOP_xldr || thisMop == MOP_sldr || thisMop == MOP_dldr || thisMop == MOP_wldr) &&
                  reg1->GetRegisterNumber() == reg2->GetRegisterNumber()) {
                continue;
              }

              if (reg1->GetSize() != mopnd1->GetSize() || reg2->GetSize() != mopnd2->GetSize()) {
                continue;
              }

              uint32 size = reg1->GetSize() >> LOG2_BITS_PER_BYTE;
              int offsetVal1 = offset1->GetOffsetValue();
              int offsetVal2 = offset2->GetOffsetValue();
              if ((base1->GetRegisterNumber() == RFP || base1->GetRegisterNumber() == RSP) &&
                  base1->GetRegisterNumber() == base2->GetRegisterNumber() &&
                  reg1->GetRegisterType() == reg2->GetRegisterType() && reg1->GetSize() == reg2->GetSize() &&
                  abs(offsetVal1 - offsetVal2) == static_cast<int>(size)) {
                // pair instr for 8/4 byte registers must have multiple of 8/4 for imm
                if ((offsetVal1 % size) != 0) {
                  continue;
                }
                // For stp/ldp, the imm should be within -512 and 504.
                if (size == kIntregBytelen /*==8*/)
                  if (offsetVal1 <= STP_LDP_IMM64_LOWER_BOUND || offsetVal1 >= STP_LDP_IMM64_UPPER_BOUND) {
                    continue;
                  }
                if (size == (kIntregBytelen >> 1) /*==4*/)
                  if (offsetVal1 <= STP_LDP_IMM32_LOWER_BOUND || offsetVal1 >= STP_LDP_IMM32_UPPER_BOUND) {
                    continue;
                  }

                MOperator mopPair = 0;
                switch (thisMop) {
                  case MOP_xldr:
                    mopPair = MOP_xldp;
                    break;
                  case MOP_wldr:
                    mopPair = MOP_wldp;
                    break;
                  case MOP_xstr:
                    mopPair = MOP_xstp;
                    break;
                  case MOP_wstr:
                    mopPair = MOP_wstp;
                    break;
                  case MOP_dldr:
                    mopPair = MOP_dldp;
                    break;
                  case MOP_sldr:
                    mopPair = MOP_sldp;
                    break;
                  case MOP_dstr:
                    mopPair = MOP_dstp;
                    break;
                  case MOP_sstr:
                    mopPair = MOP_sstp;
                    break;
                  default:
                    CG_ASSERT(false, "NYI");
                }

                if (offsetVal1 < offsetVal2) {
                  bb->InsertInsnAfter(ninsn, cg->BuildInstruction<AArch64Insn>(mopPair, reg1, reg2, mopnd1));
                } else {
                  bb->InsertInsnAfter(ninsn, cg->BuildInstruction<AArch64Insn>(mopPair, reg2, reg1, mopnd2));
                }

                Insn *nn = ninsn->next;
                std::string newComment = "";
                std::string comment = insn->GetComment();
                if (comment.c_str() && strlen(comment.c_str()) > 0) {
                  newComment += comment.c_str();
                }
                comment = ninsn->GetComment();
                if (newComment.c_str() && strlen(newComment.c_str()) > 0) {
                  newComment += "  ";
                }
                if (comment.c_str() && strlen(comment.c_str()) > 0) {
                  newComment += comment.c_str();
                }
                if (newComment.c_str() && strlen(newComment.c_str()) > 0) {
                  nn->AddComment(newComment);
                }
                bb->RemoveInsnPair(insn, ninsn);
                ninsn = nn;
              }  // pattern found
            }
          }
          break;
        case MOP_xsxtb32:
        case MOP_xsxth32:
        case MOP_xsxtb64:
        case MOP_xsxth64:
        case MOP_xsxtw64: {
          RegOperand *regopnd0 = static_cast<RegOperand *>(insn->opnds[0]);
          RegOperand *regopnd1 = static_cast<RegOperand *>(insn->opnds[1]);
          Insn *previnsn = insn->prev;
          while (previnsn != bb->firstinsn->prev && !previnsn->GetMachineOpcode()) {
            previnsn = previnsn->prev;
          }
          if (previnsn == bb->firstinsn->prev) {
            break;
          }
          if (insn != bb->firstinsn && regopnd0->GetRegisterNumber() == regopnd1->GetRegisterNumber() &&
              previnsn->IsMachineInstruction()) {
            if (previnsn->GetMachineOpcode() == MOP_xmovri32 || previnsn->GetMachineOpcode() == MOP_xmovri64) {
              RegOperand *dstmovopnd = static_cast<RegOperand *>(previnsn->opnds[0]);
              if (dstmovopnd->GetRegisterNumber() != regopnd1->GetRegisterNumber()) {
                break;
              }
              Operand *opnd = previnsn->opnds[1];
              if (opnd->IsIntImmediate()) {
                ImmOperand *immOpnd = static_cast<ImmOperand *>(opnd);
                int64 value = immOpnd->GetValue();
                if (thisMop == MOP_xsxtb32) {
                  if (value >= 0xFFFFFFFFFFFFFF80 && value <= 0x7F) {
                    bb->RemoveInsn(insn);
                  }
                } else if (thisMop == MOP_xsxth32) {
                  if (value >= 0xFFFFFFFFFFFF8000 && value <= 0x7FFF) {
                    bb->RemoveInsn(insn);
                  }
                } else {
                  int64 flag = 0xFFFFFFFFFFFFFF80;
                  if (thisMop == MOP_xsxth64) {
                    flag = 0xFFFFFFFFFFFF8000;
                  } else if (thisMop == MOP_xsxtw64) {
                    flag = 0xFFFFFFFF80000000;
                  }
                  if (!(value & flag)) {
                    RegOperand *dstOpnd = acgfunc->GetOrCreatePhysicalRegisterOperand(
                      (AArch64reg_t)(dstmovopnd->GetRegisterNumber()), 64, dstmovopnd->GetRegisterType());
                    previnsn->opnds[0] = dstOpnd;
                    previnsn->mop_ = MOP_xmovri64;
                    bb->RemoveInsn(insn);
                  }
                }
              }
            } else if (previnsn->GetMachineOpcode() == MOP_wldrsb) {
              RegOperand *dstmovopnd = static_cast<RegOperand *>(previnsn->opnds[0]);
              if (dstmovopnd->GetRegisterNumber() != regopnd1->GetRegisterNumber()) {
                break;
              }
              if (thisMop == MOP_xsxtb32) {
                bb->RemoveInsn(insn);
              }
            } else if (previnsn->GetMachineOpcode() == MOP_wldrsh) {
              RegOperand *dstmovopnd = static_cast<RegOperand *>(previnsn->opnds[0]);
              if (dstmovopnd->GetRegisterNumber() != regopnd1->GetRegisterNumber()) {
                break;
              }
              if (thisMop == MOP_xsxth32) {
                bb->RemoveInsn(insn);
              }
            }
          }
          break;
        }
        case MOP_xuxtb32:
        case MOP_xuxth32:
        case MOP_xuxtw64: {
          RegOperand *regopnd0 = static_cast<RegOperand *>(insn->opnds[0]);
          RegOperand *regopnd1 = static_cast<RegOperand *>(insn->opnds[1]);
          Insn *previnsn = insn->prev;
          while (previnsn != bb->firstinsn->prev && !previnsn->GetMachineOpcode()) {
            previnsn = previnsn->prev;
          }
          if (previnsn == bb->firstinsn->prev) {
            // insn is first insn, check if it read return registers (x0/v0).
            if (bb->preds.size() == 1 && bb->preds.front()->GetKind() == BB::kBBCall) {
              CG_ASSERT(bb->preds.front()->lastinsn->IsCall(),
                        "The last insn of the BB_call block should be a call insn.");
              CG_ASSERT(insn->GetOperand(0)->IsRegister(),
                        "The first operand of insn MOP_xuxtb32/MOP_xuxth32/MOP_xuxtw64 should be register operand.");
              CG_ASSERT(insn->GetOperand(1)->IsRegister(),
                        "The second operand of insn MOP_xuxtb32/MOP_xuxth32/MOP_xuxtw64 should be register operand.");

              Insn *callinsn = bb->preds.front()->lastinsn;
              if (callinsn->IsCallRetUnsigned() == false) {
                // call return value is signed, so zero extend is necessary.
                break;
              }
              RegOperand *regOpnd0 = static_cast<RegOperand *>(insn->GetOperand(0));
              RegOperand *regOpnd1 = static_cast<RegOperand *>(insn->GetOperand(1));
              if (regOpnd0->GetRegisterNumber() == regOpnd1->GetRegisterNumber() &&
                  (regOpnd1->GetRegisterNumber() == R0 || regOpnd1->GetRegisterNumber() == V0)) {
                uint32 retsize = callinsn->GetRetSize();
                if (retsize > 0 &&
                    ((thisMop == MOP_xuxtb32 && retsize <= 1) || (thisMop == MOP_xuxth32 && retsize <= 2) ||
                     (thisMop == MOP_xuxtw64 && retsize <= 4))) {
                  bb->RemoveInsn(insn);
                }
              }
            }
            break;
          }
          if (insn != bb->firstinsn && regopnd0->GetRegisterNumber() == regopnd1->GetRegisterNumber() &&
              previnsn->IsMachineInstruction()) {
            if (thisMop == MOP_xuxtb32) {
              if (previnsn->GetMachineOpcode() == MOP_xmovri32 || previnsn->GetMachineOpcode() == MOP_xmovri64) {
                RegOperand *dstmovopnd = static_cast<RegOperand *>(previnsn->opnds[0]);
                if (dstmovopnd->GetRegisterNumber() != regopnd1->GetRegisterNumber()) {
                  break;
                }
                Operand *opnd = previnsn->opnds[1];
                if (opnd->IsIntImmediate()) {
                  ImmOperand *immOpnd = static_cast<ImmOperand *>(opnd);
                  int64 value = immOpnd->GetValue();
                  if (!(value & 0xFFFFFFFFFFFFFF00)) {
                    bb->RemoveInsn(insn);
                  }
                }
              } else if (previnsn->GetMachineOpcode() == MOP_wldrb) {
                RegOperand *dstldopnd = static_cast<RegOperand *>(previnsn->opnds[0]);
                if (dstldopnd->GetRegisterNumber() != regopnd1->GetRegisterNumber()) {
                  break;
                }
                bb->RemoveInsn(insn);
              }
            } else if (thisMop == MOP_xuxth32) {
              if (previnsn->GetMachineOpcode() == MOP_xmovri32 || previnsn->GetMachineOpcode() == MOP_xmovri64) {
                Operand *opnd = previnsn->opnds[1];
                if (opnd->IsIntImmediate()) {
                  ImmOperand *immOpnd = static_cast<ImmOperand *>(opnd);
                  int64 value = immOpnd->GetValue();
                  if (!(value & 0xFFFFFFFFFFFF0000)) {
                    bb->RemoveInsn(insn);
                  }
                }
              } else if (previnsn->GetMachineOpcode() == MOP_wldrh) {
                RegOperand *dstldopnd = static_cast<RegOperand *>(previnsn->opnds[0]);
                if (dstldopnd->GetRegisterNumber() != regopnd1->GetRegisterNumber()) {
                  break;
                }
                bb->RemoveInsn(insn);
              }
            } else {
              // this_mop == MOP_xuxtw64
              if (previnsn->GetMachineOpcode() == MOP_xmovri32 || previnsn->GetMachineOpcode() == MOP_wldrsb ||
                  previnsn->GetMachineOpcode() == MOP_wldrb || previnsn->GetMachineOpcode() == MOP_wldrsh ||
                  previnsn->GetMachineOpcode() == MOP_wldrh || previnsn->GetMachineOpcode() == MOP_wldr) {
                RegOperand *dstopnd = static_cast<RegOperand *>(previnsn->opnds[0]);
                if (dstopnd->GetRegisterNumber() != regopnd1->GetRegisterNumber()) {
                  break;
                }
                // 32-bit ldr does zero-extension by default, so this conversion can be skipped
                bb->RemoveInsn(insn);
              }
            }
          }
          break;
        }
        case MOP_xvmovrv:
        case MOP_xvmovrd: {
          /*   fmov ireg1 <- freg1   previous insn
           *   fmov ireg2 <- freg1   current insn
           *   use  ireg2            may or may not be present
           *
           * to
           *   fmov ireg1 <- freg1   previous insn
           *   mov  ireg2 <- ireg1   current insn
           *   use  ireg1            may or may not be present
           *
           */
          if (insn == bb->firstinsn) {
            break;
          }
          Insn *previnsn = insn->prev;
          MOperator prevMop = previnsn->GetMachineOpcode();
          MOperator newMop;
          uint8 doOpt = 0;
          if (prevMop == MOP_xvmovrv && thisMop == MOP_xvmovrv) {
            doOpt = 32;
            newMop = MOP_wmovrr;
          } else if (prevMop == MOP_xvmovrd && thisMop == MOP_xvmovrd) {
            doOpt = 64;
            newMop = MOP_xmovrr;
          }
          if (doOpt == 0) {
            break;
          }
          RegOperand *curSrcRegopnd = static_cast<RegOperand *>(insn->opnds[1]);
          RegOperand *prevSrcRegopnd = static_cast<RegOperand *>(previnsn->opnds[1]);
          // same src freg
          if (curSrcRegopnd->GetRegisterNumber() != prevSrcRegopnd->GetRegisterNumber()) {
            break;
          }
          RegOperand *curDstRegopnd = static_cast<RegOperand *>(insn->opnds[0]);
          regno_t curDstReg = curDstRegopnd->GetRegisterNumber();
          // optimize case 1
          RegOperand *prevDstRegopnd = static_cast<RegOperand *>(previnsn->opnds[0]);
          regno_t prevDstReg = prevDstRegopnd->GetRegisterNumber();
          RegOperand *dst = acgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)curDstReg, doOpt, kRegTyInt);
          RegOperand *src = acgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)prevDstReg, doOpt, kRegTyInt);
          Insn *newinsn = cg->BuildInstruction<AArch64Insn>(newMop, dst, src);
          bb->InsertInsnBefore(insn, newinsn);
          bb->RemoveInsn(insn);
          if (!ninsn) {
            break;
          }
          RegOperand *newOpnd = acgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)prevDstReg, doOpt, kRegTyInt);
          for (int opndIdx = 0; opndIdx < Insn::kMaxOperandNum; opndIdx++) {
            Operand *opnd = ninsn->opnds[opndIdx];
            if (!opnd) {
              break;
            }
            if (opnd->IsMemoryAccessOperand()) {
              MemOperand *memopnd = static_cast<MemOperand *>(opnd);
              Operand *base = memopnd->GetBaseRegister();
              if (base != nullptr) {
                if (base->IsRegister()) {
                  RegOperand *reg = static_cast<RegOperand *>(base);
                  if (reg->GetRegisterNumber() == curDstReg) {
                    memopnd->SetBaseRegister(newOpnd);
                  }
                }
              }
              Operand *offset = memopnd->GetIndexRegister();
              if (offset != nullptr) {
                if (offset->IsRegister()) {
                  RegOperand *reg = static_cast<RegOperand *>(offset);
                  if (reg->GetRegisterNumber() == curDstReg) {
                    memopnd->SetIndexRegister(newOpnd);
                  }
                }
              }
            } else if (opnd->IsRegister()) {
              // Check if it is a source operand.
              const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(ninsn)->mop_];
              AArch64OpndProp *regprop = static_cast<AArch64OpndProp *>(md->operand_[opndIdx]);
              if (regprop->IsUse()) {
                RegOperand *reg = static_cast<RegOperand *>(opnd);
                if (reg->GetRegisterNumber() == curDstReg) {
                  ninsn->SetOperand(opndIdx, newOpnd);
                }
              }
            }
          }
          break;
        }
        case MOP_wcbnz:
        case MOP_xcbnz: {
          /*         cbnz x0, labelA
           *         mov x0, 0
           *         b  return-bb
           *   labelA:
           *
           * into
           *         cbz x0, return-bb
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
          if (nextinsnMop != MOP_xmovri32 && nextinsnMop != MOP_xmovri64) {
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
          if (thisMop == MOP_wcbnz) {
            insn->SetMOP(MOP_wcbz);
          } else {
            insn->SetMOP(MOP_xcbz);
          }
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

        AArch64RegOperand *reg1 = static_cast<AArch64RegOperand *>(insn->opnds[0]);
        AArch64MemOperand *mopnd1 = static_cast<AArch64MemOperand *>(insn->GetOperand(1));

        AArch64MemOperand::AArch64AddressingMode addrMode1 = mopnd1->GetAddrMode();
        if (addrMode1 != AArch64MemOperand::kAddrModeBOi || (!mopnd1->IsIntactIndexed())) {
          continue;
        }

        AArch64RegOperand *base1 = static_cast<AArch64RegOperand *>(mopnd1->GetBaseRegister());
        CG_ASSERT(base1 == nullptr || !base1->IsVirtualRegister(), "physical register has not been allocated?");
        AArch64OfstOperand *offset1 = mopnd1->GetOffsetImmediate();

        AArch64RegOperand *reg2 = static_cast<AArch64RegOperand *>(prevInsn->opnds[0]);
        AArch64MemOperand *mopnd2 = static_cast<AArch64MemOperand *>(prevInsn->GetOperand(1));

        AArch64MemOperand::AArch64AddressingMode addrMode2 = mopnd2->GetAddrMode();
        if (addrMode2 != AArch64MemOperand::kAddrModeBOi || (!mopnd2->IsIntactIndexed())) {
          continue;
        }

        AArch64RegOperand *base2 = static_cast<AArch64RegOperand *>(mopnd2->GetBaseRegister());
        CG_ASSERT(base2 == nullptr || !base2->IsVirtualRegister(), "physical register has not been allocated?");
        AArch64OfstOperand *offset2 = mopnd2->GetOffsetImmediate();

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

            bb->InsertInsnAfter(prevInsn, cg->BuildInstruction<AArch64Insn>(newOp, reg1, reg2));
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

/*
 * Check the distance between the first insn of BB with the lable(targ_labidx)
 * and the insn with targ_id. If the distance greater than SHORT_BR_DISTANCE
 * return false.
 */
bool AArch64Peep::DistanceCheck(BB *bb, LabelIdx targLabidx, uint32 targId) {
  for (MapleList<BB *>::iterator it = bb->succs.begin(); it != bb->succs.end(); it++) {
    if ((*it)->labidx == targLabidx) {
      BB *tBb = (*it);
      Insn *tInsn = tBb->firstinsn;
      while (tInsn == nullptr || !tInsn->IsMachineInstruction()) {
        if (tInsn == nullptr) {
          tBb = tBb->next;
          tInsn = tBb->firstinsn;
          continue;
        }
        if (!tInsn->IsMachineInstruction()) {
          tInsn = tInsn->next;
        }
      }
      uint32 tmp = tInsn->id > targId ? tInsn->id - targId : targId - tInsn->id;
      return tmp < SHORT_BR_DISTANCE ? true : false;
    }
  }
  CHECK_FATAL(false, "CFG error");
}

/*
 * TBZ/TBNZ instruction is generated under -O2, these branch instructions only have a range of +/-32KB.
 * If the branch target is not reachable, we split tbz/tbnz into combination of ubfx and cbz/cbnz, which
 * will clobber one extra register. With LSRA under -O2, we can use one of the reserved registers R16 for
 * that purpose. To save compile time, we do this change when there are more than 32KB / 4 instructions
 * in the function.
 */
void AArch64Peep::FixShortBranches() {
  uint32 i = 0;
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    FOR_BB_INSNS(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      insn->id = i++;
      if (insn->GetMachineOpcode() == MOP_clinit) {
        i = i + 3;
      }
      if (insn->GetMachineOpcode() == MOP_adrp_ldr || insn->GetMachineOpcode() == MOP_clinit_tail) {
        i = i + 1;
      }
    }
  }

  FOR_ALL_BB(bb, acgfunc) {
    // Do a backward scan searching for short branches
    FOR_BB_INSNS_REV(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }

      MOperator thisMop = insn->GetMachineOpcode();

      if (thisMop == MOP_wtbz || thisMop == MOP_wtbnz || thisMop == MOP_xtbz || thisMop == MOP_xtbnz) {
        LabelOperand *label = static_cast<LabelOperand *>(insn->opnds[2]);
        if (DistanceCheck(bb, label->GetLabelIndex(), insn->id)) {
          continue;
        }
        AArch64RegOperand *reg = static_cast<AArch64RegOperand *>(insn->opnds[0]);
        ImmOperand *bitsize = acgfunc->CreateImmOperand(1, 8, false);
        ImmOperand *bitpos = static_cast<ImmOperand *>(insn->opnds[1]);
        MOperator ubfxOp = MOP_undef, cbOp = MOP_undef;
        switch (thisMop) {
          case MOP_wtbz:
            ubfxOp = MOP_wubfxrri5i5;
            cbOp = MOP_wcbz;
            break;
          case MOP_wtbnz:
            ubfxOp = MOP_wubfxrri5i5;
            cbOp = MOP_wcbnz;
            break;
          case MOP_xtbz:
            ubfxOp = MOP_xubfxrri6i6;
            cbOp = MOP_xcbz;
            break;
          case MOP_xtbnz:
            ubfxOp = MOP_xubfxrri6i6;
            cbOp = MOP_xcbnz;
            break;
        }

        AArch64RegOperand *tmp =
          acgfunc->GetOrCreatePhysicalRegisterOperand(R16, ubfxOp == MOP_wubfxrri5i5 ? 32 : 64, kRegTyInt);
        bb->InsertInsnAfter(insn, cg->BuildInstruction<AArch64Insn>(cbOp, tmp, label));
        bb->InsertInsnAfter(insn, cg->BuildInstruction<AArch64Insn>(ubfxOp, tmp, reg, bitpos, bitsize));
        bb->RemoveInsn(insn);
        break;
      }
    }
  }
}

int AArch64Peep::IsPowerOf2(int64 val) {
  uint32 lsbVal = val & 0xFFFFFFFF;
  uint32 msbVal = (val >> 32) & 0xFFFFFFFF;
  if (__builtin_popcount(lsbVal) + __builtin_popcount(msbVal) == 1) {
    return __builtin_ffs(msbVal) ? 31 + __builtin_ffs(msbVal) : __builtin_ffs(lsbVal) - 1;
  }
  return -1;
}

/* We optimize the following pattern in this function:
 * and x1, x1, #imm (is n power of 2)
 * cbz/cbnz x1, .label
 * =>
 * tbnz/tbz x1, #n, .label
 */
void AArch64Peep::OptOneHoleBranches() {
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    if (bb->lastinsn == nullptr) {
      continue;
    }
    Insn *insnNext = bb->lastinsn;
    MOperator nextMop = insnNext->GetMachineOpcode();
    Insn *insn = nullptr;
    if (nextMop != MOP_wcbz && nextMop != MOP_wcbnz && nextMop != MOP_xcbz && nextMop != MOP_xcbnz) {
      continue;
    } else {
      LabelOperand *label = static_cast<LabelOperand *>(insnNext->opnds[1]);
      MOperator newOp = MOP_undef;
      switch (nextMop) {
        case MOP_wcbz:
          newOp = MOP_wtbz;
          break;
        case MOP_wcbnz:
          newOp = MOP_wtbnz;
          break;
        case MOP_xcbz:
          newOp = MOP_xtbz;
          break;
        case MOP_xcbnz:
          newOp = MOP_xtbnz;
          break;
        default:
          CHECK_FATAL(false, "can not touch here");
      }
      for (insn = insnNext->prev; insn != nullptr && !insn->IsMachineInstruction(); insn = insn->prev)
        ;
      if (insn != nullptr && (insn->GetMachineOpcode() == MOP_wandrri12 || insn->GetMachineOpcode() == MOP_xandrri13)) {
        if (insn->opnds[0] != insnNext->opnds[0]) {
          continue;
        }
        ImmOperand *imm = static_cast<ImmOperand *>(insn->opnds[2]);
        int n = IsPowerOf2(imm->GetValue());
        if (n < 0) {
          continue;
        }
        ImmOperand *oneHoleP = acgfunc->CreateImmOperand(n, 8, false);
        bb->InsertInsnAfter(insnNext, cg->BuildInstruction<AArch64Insn>(
                                        newOp, static_cast<AArch64RegOperand *>(insn->opnds[1]), oneHoleP, label));
        bb->RemoveInsn(insnNext);
        continue;
      }
    }
  }
}

/*
 * Find 5 insn with certain OP code
 * 1 : MOP_xaddrri12
 * 2 : MOP_waddrrr
 * 3 : MOP_waddrri12
 * 4 : MOP_xsxtw64
 * 5 : MOP_xaddrrrs
 */
bool AArch64Peep::FindComputationTree(std::vector<Insn *> &optInsn, Insn *insn) {
  MOperator thisMop = insn->GetMachineOpcode();
  /// first
  if (optInsn.size() == 0 && thisMop == MOP_xaddrri12) {
    optInsn.push_back(insn);
    return false;
  } else if (optInsn.size() == 0) {
    return false;
  }
  /// second
  if (optInsn.size() == 1 && thisMop == MOP_waddrrr) {
    optInsn.push_back(insn);
    return false;
  } else if (optInsn.size() == 1) {
    optInsn.clear();
    return false;
  }
  /// third
  if (optInsn.size() == 2 && thisMop == MOP_waddrri12) {
    optInsn.push_back(insn);
    return false;
  } else if (optInsn.size() == 2) {
    optInsn.clear();
    return false;
  }
  /// forth
  if (optInsn.size() == 3 && thisMop == MOP_xsxtw64) {
    optInsn.push_back(insn);
    return false;
  } else if (optInsn.size() == 3) {
    optInsn.clear();
    return false;
  }
  /// fifth
  if (optInsn.size() == 4 && thisMop == MOP_xaddrrrs) {
    optInsn.push_back(insn);
    return true;
  } else if (optInsn.size() == 4) {
    optInsn.clear();
    return false;
  }
  return false;
}

/*
 * Make sure the insn in opt_insn match the pattern as following:
 * add x1, x1, #16
 * add w2, w10, w10
 * add w2, w2, #1
 * sxtw x2, w2
 * add x1, x1, x2, LSL #3
 * bl MCC_LoadRefField_NaiveRCFast
 */
bool AArch64Peep::PatternIsMatch(std::vector<Insn *> &optInsn) {
  CHECK_FATAL(optInsn.size() > 4, "access opt_insn failed");
  Insn *insn1 = optInsn[0];
  Insn *insn2 = optInsn[1];
  Insn *insn3 = optInsn[2];
  Insn *insn4 = optInsn[3];
  Insn *insn5 = optInsn[4];
  Insn *insn6 = insn5->next;
  if (insn1->opnds[0] == insn5->opnds[1] && insn2->opnds[1] == insn2->opnds[2] && insn2->opnds[0] == insn3->opnds[1] &&
      insn3->opnds[0] == insn4->opnds[1] && insn4->opnds[0] == insn5->opnds[2] && insn6 && insn6->IsCall() &&
      dynamic_cast<FuncNameOperand *>(insn6->opnds[0]) &&
      dynamic_cast<FuncNameOperand *>(insn6->opnds[0])->GetName() == GetIntrinsicFuncName(INTRN_MCCLoadRef) &&
      static_cast<ImmOperand *>(insn1->opnds[2])->GetValue() == 16 &&
      static_cast<ImmOperand *>(insn3->opnds[2])->GetValue() == 1) {
    return true;
  } else {
    return false;
  }
}

/* We optimize the following pattern in this function:
 * add x1, x1, #16
 * add w2, w10, w10
 * add w2, w2, #1
 * sxtw x2, w2
 * add x1, x1, x2, LSL #3
 * =>
 * add x1, x1, w10, SXTW #4
 * add x1, x1, #24
 */
void AArch64Peep::OptComputationTree() {
  std::vector<Insn *> optInsn;
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    optInsn.clear();
    FOR_BB_INSNS(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      /// found pattern
      if (FindComputationTree(optInsn, insn) && PatternIsMatch(optInsn)) {
        BitShiftOperand *lsl = dynamic_cast<BitShiftOperand *>(optInsn[4]->opnds[3]);
        CHECK_FATAL(lsl, "should not happened");
        Operand *sxtw = nullptr;
        Operand *imm = nullptr;
        if (lsl->GetShiftAmount() == 3) {
          sxtw = acgfunc->CreateExtendShiftOperand(ExtendShiftOperand::SXTW, 4, 3);
          imm = acgfunc->CreateImmOperand(24, 12, true);
        } else if (lsl->GetShiftAmount() == 2) {
          sxtw = acgfunc->CreateExtendShiftOperand(ExtendShiftOperand::SXTW, 3, 3);
          imm = acgfunc->CreateImmOperand(20, 12, true);
        }
        Insn *newInsn = cg->BuildInstruction<AArch64Insn>(MOP_xxwaddrrre, optInsn[4]->opnds[0], optInsn[0]->opnds[1],
                                                          optInsn[1]->opnds[1], sxtw);
        bb->ReplaceInsn(optInsn[4], newInsn);
        Insn *newAdd =
          cg->BuildInstruction<AArch64Insn>(MOP_xaddrri12, optInsn[4]->opnds[0], optInsn[4]->opnds[0], imm);
        bb->InsertInsnAfter(newInsn, newAdd);
        optInsn.clear();
      }
    }
  }
}

/*We optimize the following pattern in this function:
   cmp w[0-9]*, wzr  ====> tbz w[0-9]*, #31, .label
   bge .label

   cmp wzr, w[0-9]*  ====> tbz w[0-9]*, #31, .label
   ble .label

   cmp w[0-9]*,wzr   ====> tbnz w[0-9]*, #31, .label
   blt .label

   cmp wzr, w[0-9]*  ====> tbnz w[0-9]*, #31, .label
   bgt .label

   cmp w[0-9]*, #0   ====> tbz w[0-9]*, #31, .label
   bge .label

   cmp w[0-9]*, #0   ====> tbnz w[0-9]*, #31, .label
   blt .label*/
void AArch64Peep::OptZeroCmpBranches() {
  Insn *last = nullptr;
  Insn *prev = nullptr;
  LabelOperand *label = nullptr;
  RegOperand *regopnd = nullptr;
  RegOperand *reg0 = nullptr;
  RegOperand *reg1 = nullptr;
  MOperator newOp = MOP_undef;
  ImmOperand *imm = nullptr;
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    if (bb->lastinsn == nullptr) {
      continue;
    }
    last = bb->lastinsn;
    for (prev = last->prev; prev != nullptr && !prev->IsMachineInstruction(); prev = prev->prev)
      ;
    if (prev == nullptr) {
      continue;
    }
    if (!last->IsBranch()) {
      continue;
    }
    if (last->GetOperand(1) && !last->GetOperand(1)->IsLabel()) {
      continue;
    }
    label = static_cast<LabelOperand *>(last->GetOperand(1));
    switch (prev->GetMachineOpcode()) {
      case MOP_wcmpri:
      case MOP_xcmpri:
        regopnd = static_cast<RegOperand *>(prev->GetOperand(1));
        imm = static_cast<ImmOperand *>(prev->GetOperand(2));
        if (imm->GetValue() != 0) {
          continue;
        }
        if (last->GetMachineOpcode() != MOP_bge && last->GetMachineOpcode() != MOP_blt) {
          continue;
        }
        if (last->GetMachineOpcode() == MOP_bge) {
          newOp = (regopnd->GetSize() <= 32) ? MOP_wtbz : MOP_xtbz;
        } else if (last->GetMachineOpcode() == MOP_blt) {
          newOp = (regopnd->GetSize() <= 32) ? MOP_wtbnz : MOP_xtbnz;
        } else {
          continue;
        }
        break;

      case MOP_wcmprr:
      case MOP_xcmprr: {
        reg0 = static_cast<RegOperand *>(prev->GetOperand(1));
        reg1 = static_cast<RegOperand *>(prev->GetOperand(2));
        if (!reg0->IsZeroRegister() && !reg1->IsZeroRegister()) {
          continue;
        }
        switch (last->GetMachineOpcode()) {
          case MOP_bge:
            if (reg1->IsZeroRegister()) {
              regopnd = static_cast<RegOperand *>(prev->GetOperand(1));
              newOp = (regopnd->GetSize() <= 32) ? MOP_wtbz : MOP_xtbz;
            } else {
              continue;
            }
            break;

          case MOP_ble:
            if (reg0->IsZeroRegister()) {
              regopnd = static_cast<RegOperand *>(prev->GetOperand(2));
              newOp = (regopnd->GetSize() <= 32) ? MOP_wtbz : MOP_xtbz;
            } else {
              continue;
            }
            break;

          case MOP_blt:
            if (reg1->IsZeroRegister()) {
              regopnd = static_cast<RegOperand *>(prev->GetOperand(1));
              newOp = (regopnd->GetSize() <= 32) ? MOP_wtbnz : MOP_xtbnz;
            } else {
              continue;
            }
            break;

          case MOP_bgt:
            if (reg0->IsZeroRegister()) {
              regopnd = static_cast<RegOperand *>(prev->GetOperand(2));
              newOp = (regopnd->GetSize() <= 32) ? MOP_wtbnz : MOP_xtbnz;
            } else {
              continue;
            }
            break;

          default:
            continue;
        }
        break;
      }
      default:
        continue;
    }
    ImmOperand *bitp = acgfunc->CreateImmOperand(regopnd->GetSize() <= 32 ? 31 : 63, 8, false);
    bb->InsertInsnAfter(
      last, cg->BuildInstruction<AArch64Insn>(newOp, static_cast<AArch64RegOperand *>(regopnd), bitp, label));
    bb->RemoveInsn(last);
    bb->RemoveInsn(prev);
  }
}

/* Remove following patterns:
   mov     x1, x0
   bl      MCC_IncDecRef_NaiveRCFast
 */
void AArch64Peep::RemoveIncDecRef1Param() {
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    for (Insn *insnLast : bb->callInsns) {
      if (insnLast->GetMachineOpcode() != MOP_xbl) {
        continue;
      }
      FuncNameOperand *target = static_cast<FuncNameOperand *>(insnLast->opnds[0]);
      Insn *insnMov = insnLast->GetPreviousMachineInsn();
      if (insnMov == nullptr) {
        continue;
      }
      MOperator mopMov = insnMov->GetMachineOpcode();
      if (target->GetName() == GetIntrinsicFuncName(INTRN_MCCIncDecRef) && mopMov == MOP_xmovrr &&
          static_cast<RegOperand *>(insnMov->opnds[0])->GetRegisterNumber() == R1 &&
          static_cast<RegOperand *>(insnMov->opnds[1])->GetRegisterNumber() == R0) {
        bb->RemoveInsn(insnMov);
        bb->RemoveInsn(insnLast);
        bb->SetKind(BB::kBBFallthru);
      }
    }
  }
}

void AArch64Peep::PeepholeOpt() {
  Peephole();
  OptZeroCmpBranches();
  OptAndCmpBranchesToCset();
  ReplaceDivisionToMultiplication();
  RemoveIncDecRef1Param();
}

/* We optimize the following pattern in this function:
 * if w0's valid bits is one
 * uxtb w0, w0
 * eor w0, w0, #1
 * cbz w0, .label
 * =>
 * tbnz w0, .label
 * &&
 * if there exists uxtb w0, w0 and w0's valid bits is
 * less than 8, eliminate it.
 */
void AArch64Peep::PreOptOneHoleBranches() {
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    if (bb->lastinsn == nullptr) {
      continue;
    }
    Insn *insnNext = bb->lastinsn;
    MOperator nextMop = insnNext->GetMachineOpcode();
    Insn *insn = nullptr;
    if (nextMop != MOP_wcbz && nextMop != MOP_wcbnz && nextMop != MOP_xcbz && nextMop != MOP_xcbnz) {
      continue;
    } else {
      LabelOperand *label = static_cast<LabelOperand *>(insnNext->opnds[1]);
      MOperator newOp = MOP_undef;
      switch (nextMop) {
        case MOP_wcbz:
          newOp = MOP_wtbnz;
          break;
        case MOP_wcbnz:
          newOp = MOP_wtbz;
          break;
        case MOP_xcbz:
          newOp = MOP_xtbnz;
          break;
        case MOP_xcbnz:
          newOp = MOP_xtbz;
          break;
        default:
          CHECK_FATAL(false, "can not touch here");
      }
      for (insn = insnNext->prev; insn != nullptr && !insn->IsMachineInstruction(); insn = insn->prev)
        ;
      if (insn != nullptr && insn->GetMachineOpcode() == MOP_xuxtb32 &&
          (static_cast<RegOperand *>(insn->opnds[1])->GetValidBitsNum() <= 8 ||
           static_cast<RegOperand *>(insn->opnds[0])->GetValidBitsNum() <= 8)) {
        if (insn->opnds[0] != insnNext->opnds[0]) {
          continue;
        }
        insnNext->opnds[0] = insn->opnds[1];
        bb->RemoveInsn(insn);
      }
      if (insn != nullptr && (insn->GetMachineOpcode() == MOP_xeorrri13 || insn->GetMachineOpcode() == MOP_weorrri12) &&
          static_cast<ImmOperand *>(insn->opnds[2])->GetValue() == 1) {
        if (insn->opnds[0] != insnNext->opnds[0]) {
          continue;
        }
        Insn *insnPrev = nullptr;
        for (insnPrev = insn->prev; insnPrev != nullptr && !insnPrev->IsMachineInstruction(); insnPrev = insnPrev->prev)
          ;
        if (insnPrev == nullptr) {
          continue;
        }
        if (insnPrev->GetMachineOpcode() == MOP_xuxtb32 &&
            static_cast<RegOperand *>(insnPrev->opnds[1])->GetValidBitsNum() == 1) {
          if (insnPrev->opnds[0] != insn->opnds[1]) {
            continue;
          }
          ImmOperand *oneHoleP = acgfunc->CreateImmOperand(0, 8, false);
          bb->InsertInsnAfter(
            insnNext, cg->BuildInstruction<AArch64Insn>(newOp, static_cast<AArch64RegOperand *>(insnPrev->opnds[1]),
                                                        oneHoleP, label));
          bb->RemoveInsn(insnNext);
          bb->RemoveInsn(insn);
          bb->RemoveInsn(insnPrev);
        }
      }
    }
  }
}

bool AArch64Peep::IsSameReg(Operand *firstOpnd, Operand *secondOpnd) {
  CG_ASSERT(firstOpnd->IsRegister() && secondOpnd->IsRegister(),
            "first_opnd and second_opnd should be Register Operand");
  RegOperand *firstReg = static_cast<RegOperand *>(firstOpnd);
  RegOperand *secondReg = static_cast<RegOperand *>(secondOpnd);
  return firstReg->RegNumEqual(secondReg);
}

bool AArch64Peep::IsSameRegNumAndRegSize(Operand *firstOpnd, Operand *secondOpnd) {
  CG_ASSERT(firstOpnd->IsRegister() && secondOpnd->IsRegister(),
            "first_opnd and second_opnd should be Register Operand");
  RegOperand *firstReg = static_cast<RegOperand *>(firstOpnd);
  RegOperand *secondReg = static_cast<RegOperand *>(secondOpnd);
  return firstReg->RegNumEqual(secondReg) && (firstReg->GetSize() == secondReg->GetSize());
}

bool AArch64Peep::PredBBCheck(BB *bb, bool checkcbz, Operand *opnd) {
  if (bb->GetKind() != BB::kBBIf) {
    return false;
  }

  Insn *condbr = cgfunc->theCFG->FindLastCondBrInsn(bb);
  CG_ASSERT(condbr, "condbr must be found");
  if (!cgfunc->theCFG->IsCompareAndBranchInsn(condbr)) {
    return false;
  }
  MOperator mop = condbr->GetMachineOpcode();
  if (checkcbz && mop != MOP_wcbz && mop != MOP_xcbz) {
    return false;
  }
  if (!checkcbz && mop != MOP_xcbnz && mop != MOP_wcbnz) {
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
bool AArch64Peep::OpndDefByMovZero(Insn *insn) {
  MOperator defMop = insn->GetMachineOpcode();
  switch (defMop) {
    case MOP_xmovri32:
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
      AArch64RegOperand *regOpnd = static_cast<AArch64RegOperand *>(secondOpnd);
      return regOpnd->IsZeroRegister();
    }
    default:
      return false;
  }
}

/*
 *check wether predefine insn of first operand of test_insn is exist in current BB
 * */
bool AArch64Peep::NoPreDefine(Insn *testInsn) {
  Insn *ninsn = nullptr;
  for (Insn *insn = testInsn->bb->firstinsn; insn && insn != testInsn; insn = ninsn) {
    ninsn = insn->GetNextMachineInsn();
    if (!insn->IsMachineInstruction()) {
      continue;
    }
    CG_ASSERT(!insn->IsCall(), "CG internal error, call insn should not be at the middle of the BB.");
    const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
    for (int i = 0; i < Insn::kMaxOperandNum; i++) {
      Operand *opnd = insn->opnds[i];
      if (opnd == nullptr) {
        continue;
      }
      AArch64OpndProp *regprop = static_cast<AArch64OpndProp *>(md->operand_[i]);
      if (regprop->IsDef()) {
        if (opnd->IsMemoryAccessOperand()) {
          AArch64MemOperand *memopnd = static_cast<AArch64MemOperand *>(opnd);
          RegOperand *base = memopnd->GetBaseRegister();
          CG_ASSERT(base && base->IsRegister(), "expects RegOperand");
          if (IsSameReg(base, testInsn->GetOperand(0)) && memopnd->GetAddrMode() == AArch64MemOperand::kAddrModeBOi &&
              (memopnd->IsPostIndexed() || memopnd->IsPreIndexed())) {
            return false;
          }

        } else {
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
void AArch64Peep::DeleteMovAfterCbzOrCbnz() {
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
    if (condbrMop == MOP_wcbnz || condbrMop == MOP_xcbnz) {
      processBb = bb->next;
    } else {
      processBb = cgfunc->theCFG->GetTargetSuc(bb);
    }

    CG_ASSERT(processBb != nullptr, "process_bb is null in AArch64Peep::DeleteMovAfterCbzOrCbnz");
    Insn *ninsn = nullptr;
    for (Insn *insn = processBb->firstinsn; insn; insn = ninsn) {
      ninsn = insn->GetNextMachineInsn();
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      if (OpndDefByMovZero(insn) && NoPreDefine(insn) && IsSameReg(insn->GetOperand(0), condbr->GetOperand(0))) {
        bool toDoOpt = true;
        // process elseBB, other preds must be cbz
        if (condbrMop == MOP_wcbnz || condbrMop == MOP_xcbnz) {
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

bool AArch64Peep::OpndDefByOneValidBit(Insn *defInsn) {
  CG_ASSERT(defInsn, "def_insn must not be null");
  MOperator defMop = defInsn->GetMachineOpcode();
  switch (defMop) {
    case MOP_wcsetrc:
    case MOP_xcsetrc:
      return true;
    case MOP_xmovri32:
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
Insn *AArch64Peep::DefInsnOfOperandInBB(BB *bb, Insn *startinsn, Insn *checkinsn, int opndidx) {
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
    const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
    for (int i = 0; i < Insn::kMaxOperandNum; i++) {
      Operand *opnd = insn->opnds[i];
      if (opnd == nullptr) {
        continue;
      }
      AArch64OpndProp *regprop = static_cast<AArch64OpndProp *>(md->operand_[i]);
      if (regprop->IsDef()) {
        // Operand is base reg of Memory, defined by str
        if (opnd->IsMemoryAccessOperand()) {
          AArch64MemOperand *memopnd = static_cast<AArch64MemOperand *>(opnd);
          RegOperand *base = memopnd->GetBaseRegister();
          CG_ASSERT(base && base->IsRegister(), "expects RegOperand");
          if (IsSameReg(base, checkinsn->GetOperand(opndidx)) &&
              memopnd->GetAddrMode() == AArch64MemOperand::kAddrModeBOi &&
              (memopnd->IsPostIndexed() || memopnd->IsPreIndexed())) {
            return insn;
          }

        } else {
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

// help function for cmpcset optimize
// if all define points of used opnd in insn has only one valid bit,return true.
// for cmp reg,#0(#1), that is checking for reg
bool AArch64Peep::CheckOpndDefPoints(Insn *checkinsn, int opndidx) {
  // check current BB
  Insn *defInsn = DefInsnOfOperandInBB(checkinsn->bb, checkinsn, checkinsn, opndidx);
  if (defInsn) {
    if (OpndDefByOneValidBit(defInsn)) {
      return true;
    } else {
      return false;
    }
  }
  // check pred
  for (auto predbb : checkinsn->bb->preds) {
    Insn *tempInsn = DefInsnOfOperandInBB(predbb, predbb->lastinsn, checkinsn, opndidx);
    if (!tempInsn || !OpndDefByOneValidBit(tempInsn)) {
      return false;
    }
  }
  return true;
}

bool AArch64Peep::FlagNotUsedLaterInCurBB(BB *bb, Insn *startinsn) {
  if (startinsn == nullptr || bb != startinsn->bb) {
    return true;
  }
  Insn *ninsn = nullptr;
  for (Insn *insn = startinsn; insn != bb->lastinsn->next; insn = ninsn) {
    ninsn = insn->GetNextMachineInsn();
    for (int i = 0; i < Insn::kMaxOperandNum; i++) {
      Operand *opnd = insn->opnds[i];
      if (opnd == nullptr) {
        continue;
      }
      if (opnd->IsConditionCode()) {
        return false;
      }
    }
  }
  return true;
}

/*  cmp  w0, #0
 *  cset w1, NE --> mov w1, w0
 *
 *  cmp  w0, #0
 *  cset w1, EQ --> eor w1, w0, 1
 *
 *  cmp  w0, #1
 *  cset w1, NE --> eor w1, w0, 1
 *
 *  cmp  w0, #1
 *  cset w1, EQ --> mov w1, w0
 *
 *  cmp w0,  #0
 *  cset w0, NE -->null
 *
 *  cmp w0, #1
 *  cset w0, EQ -->null
 *
 *  condition:
 *    1. the first operand of cmp instruction must has only one valid bit
 *    2. the second operand of cmp instruction must be 0 or 1
 *    3. flag register of cmp isntruction must not be used later
 */

void AArch64Peep::CmpCsetOptimize() {
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    Insn *ninsn = nullptr;
    for (Insn *insn = bb->firstinsn; insn; insn = ninsn) {
      ninsn = insn->GetNextMachineInsn();
      if (!ninsn) {
        break;
      }
      MOperator firstMop = insn->GetMachineOpcode();
      MOperator secondMop = ninsn->GetMachineOpcode();
      if ((firstMop == MOP_wcmpri || firstMop == MOP_xcmpri) &&
          (secondMop == MOP_wcsetrc || secondMop == MOP_xcsetrc)) {
        Operand *cmpFirstOpnd = insn->GetOperand(1);
        // get ImmOperand, must be 0 or 1
        Operand *cmpSecondOpnd = insn->GetOperand(2);
        Operand *cmpFlag = insn->GetOperand(0);
        RegOperand *cmpFlagReg = static_cast<RegOperand *>(cmpFlag);
        CG_ASSERT(cmpFlagReg, "flag must be exist");
        CG_ASSERT(cmpSecondOpnd->IsIntImmediate(), "expects ImmOperand");
        ImmOperand *cmpConst = static_cast<ImmOperand *>(cmpSecondOpnd);
        int64 cmpConstVal = cmpConst->GetValue();
        Operand *csetFirstOpnd = ninsn->GetOperand(0);
        if ((cmpConstVal != 0 && cmpConstVal != 1) || !CheckOpndDefPoints(insn, 1) ||
            !FlagNotUsedLaterInCurBB(bb, ninsn->GetNextMachineInsn()) ||
            bb->liveout_regno.find(cmpFlagReg->GetRegisterNumber()) != bb->liveout_regno.end()) {
          continue;
        }

        Insn *csetInsn = ninsn;
        ninsn = ninsn->GetNextMachineInsn();
        CondOperand *cond = static_cast<CondOperand *>(csetInsn->GetOperand(1));
        if ((cmpConstVal == 0 && cond->GetCode() == CC_NE) || (cmpConstVal == 1 && cond->GetCode() == CC_EQ)) {
          if (IsSameReg(cmpFirstOpnd, csetFirstOpnd)) {
            bb->RemoveInsn(insn);
            bb->RemoveInsn(csetInsn);
          } else {
            if (cmpFirstOpnd->GetSize() != csetFirstOpnd->GetSize()) {
              continue;
            }
            MOperator mopCode = (cmpFirstOpnd->GetSize() == 64) ? MOP_xmovrr : MOP_wmovrr;
            Insn *newInsn = cg->BuildInstruction<AArch64Insn>(mopCode, csetFirstOpnd, cmpFirstOpnd);
            bb->ReplaceInsn(insn, newInsn);
            bb->RemoveInsn(csetInsn);
          }
        } else if ((cmpConstVal == 1 && cond->GetCode() == CC_NE) || (cmpConstVal == 0 && cond->GetCode() == CC_EQ)) {
          MOperator mopCode = (cmpFirstOpnd->GetSize() == 64) ? MOP_xeorrri13 : MOP_weorrri12;
          ImmOperand *one = acgfunc->CreateImmOperand(1, 8, false);
          Insn *newInsn = cg->BuildInstruction<AArch64Insn>(mopCode, csetFirstOpnd, cmpFirstOpnd, one);
          bb->ReplaceInsn(insn, newInsn);
          bb->RemoveInsn(csetInsn);
        }
      }
    }
  }
}

/* Remove following patterns:
     mov x0, XX
     mov x1, XX
     bl  MCC_IncDecRef_NaiveRCFast
 */
void AArch64Peep::RemoveIncDecRef2Param() {
  FOR_ALL_BB(bb, cgfunc) {
    for (Insn *insnLast : bb->callInsns) {
      MOperator mop = insnLast->GetMachineOpcode();
      if (mop != MOP_xbl) {
        continue;
      }
      FuncNameOperand *target = static_cast<FuncNameOperand *>(insnLast->opnds[0]);
      if (target->GetName() != GetIntrinsicFuncName(INTRN_MCCIncDecRef)) {
        continue;
      }
      Insn *insnMov2 = insnLast->GetPreviousMachineInsn();
      if (insnMov2 == nullptr) {
        continue;
      }
      MOperator mopMov2 = insnMov2->GetMachineOpcode();
      if (mopMov2 != MOP_xmovrr) {
        continue;
      }
      Insn *insnMov1 = insnMov2->GetPreviousMachineInsn();
      if (insnMov1 == nullptr) {
        continue;
      }
      MOperator mopMov1 = insnMov1->GetMachineOpcode();
      if (mopMov1 != MOP_xmovrr) {
        continue;
      }
      if (static_cast<RegOperand *>(insnMov1->opnds[1])->GetRegisterNumber() !=
          static_cast<RegOperand *>(insnMov2->opnds[1])->GetRegisterNumber()) {
        continue;
      }
      RegOperand *mov2Dest = static_cast<RegOperand *>(insnMov2->opnds[0]);
      RegOperand *mov1Dest = static_cast<RegOperand *>(insnMov1->opnds[0]);
      if (mov1Dest->IsVirtualRegister() || mov2Dest->IsVirtualRegister() || mov1Dest->GetRegisterNumber() != R0 ||
          mov2Dest->GetRegisterNumber() != R1) {
        continue;
      }
      bb->RemoveInsn(insnLast);
      bb->RemoveInsn(insnMov2);
      bb->RemoveInsn(insnMov1);
      bb->SetKind(BB::kBBFallthru);
    }
  }
}

/* Remove following patterns:
      mov     x0, xzr/#0
      bl      MCC_DecRef_NaiveRCFast
 */
void AArch64Peep::RemoveDecRef() {
  FOR_ALL_BB(bb, cgfunc) {
    for (Insn *insnLast : bb->callInsns) {
      if (insnLast->GetMachineOpcode() != MOP_xbl) {
        continue;
      }
      FuncNameOperand *target = static_cast<FuncNameOperand *>(insnLast->opnds[0]);
      if (target->GetName() != GetIntrinsicFuncName(INTRN_MCCDecRef)) {
        continue;
      }
      Insn *insnMov = insnLast->GetPreviousMachineInsn();
      if (insnMov == nullptr) {
        continue;
      }
      MOperator mopMov = insnMov->GetMachineOpcode();
      if (mopMov != MOP_xmovrr || static_cast<RegOperand *>(insnMov->opnds[0])->GetRegisterNumber() != R0) {
        continue;
      }
      if (!insnMov->opnds[1]->IsZeroRegister() &&
          !(insnMov->opnds[1]->IsImmediate() && static_cast<ImmOperand *>(insnMov->opnds[1])->GetValue() == 0)) {
        continue;
      }
      bb->RemoveInsn(insnMov);
      bb->RemoveInsn(insnLast);
      bb->SetKind(BB::kBBFallthru);
    }
  }
}

/*
 * Replace following pattern:
 * mov x1, xzr
 * bl MCC_IncDecRef_NaiveRCFast
 * =>
 * bl MCC_IncRef_NaiveRCFast
 */
void AArch64Peep::ReplaceIncDecWithInc() {
  FOR_ALL_BB(bb, cgfunc) {
    for (Insn *insnLast : bb->callInsns) {
      if (insnLast->GetMachineOpcode() != MOP_xbl) {
        continue;
      }
      FuncNameOperand *target = static_cast<FuncNameOperand *>(insnLast->opnds[0]);
      if (target->GetName() != GetIntrinsicFuncName(INTRN_MCCIncDecRef)) {
        continue;
      }
      Insn *insnMov = insnLast->GetPreviousMachineInsn();
      if (insnMov == nullptr) {
        continue;
      }
      MOperator mopMov = insnMov->GetMachineOpcode();
      if (mopMov != MOP_xmovrr) {
        continue;
      }
      if (static_cast<RegOperand *>(insnMov->opnds[0])->GetRegisterNumber() != R1 ||
          !insnMov->opnds[1]->IsZeroRegister()) {
        continue;
      }
      std::string funcName = GetIntrinsicFuncName(INTRN_MCCIncRef);
      GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName(funcName);
      MIRSymbol *st = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx, true);
      if (!st) {
        LogInfo::MapleLogger() << "WARNING: Replace IncDec With Inc fail due to no MCC_IncRef_NaiveRCFast func" << std::endl;
        continue;
      }
      bb->RemoveInsn(insnMov);
      target->SetFunctionSymbol(st);
    }
  }
}

bool AArch64Peep::FindLondIntCmpWithZ(std::vector<Insn *> &optInsn, Insn *insn) {
  MOperator thisMop = insn->GetMachineOpcode();
  /// first
  if (optInsn.size() == 0 && thisMop == MOP_xcmpri) {
    optInsn.push_back(insn);
    return false;
  } else if (optInsn.size() == 0) {
    return false;
  }
  /// second
  if (optInsn.size() == 1 && thisMop == MOP_wcsinvrrrc) {
    optInsn.push_back(insn);
    return false;
  } else if (optInsn.size() == 1) {
    optInsn.clear();
    return false;
  }
  /// third
  if (optInsn.size() == 2 && thisMop == MOP_wcsincrrrc) {
    optInsn.push_back(insn);
    return false;
  } else if (optInsn.size() == 2) {
    optInsn.clear();
    return false;
  }
  /// forth
  if (optInsn.size() == 3 && thisMop == MOP_wcmpri) {
    optInsn.push_back(insn);
    return true;
  } else if (optInsn.size() == 3) {
    optInsn.clear();
    return false;
  }
  return false;
}

bool AArch64Peep::PatternMatch(std::vector<Insn *> &optInsn) {
  CG_ASSERT(optInsn.size() == 4, "Must be size 4");
  Insn *insn1 = optInsn[0];
  Insn *insn2 = optInsn[1];
  Insn *insn3 = optInsn[2];
  Insn *insn4 = optInsn[3];
  if (insn2->opnds[1]->IsZeroRegister() && insn2->opnds[2]->IsZeroRegister() && insn3->opnds[2]->IsZeroRegister() &&
      insn3->opnds[0] == insn3->opnds[1] && static_cast<CondOperand *>(insn2->opnds[3])->GetCode() == CC_GE &&
      static_cast<CondOperand *>(insn3->opnds[3])->GetCode() == CC_LE &&
      static_cast<ImmOperand *>(insn1->opnds[2])->GetValue() == 0 &&
      static_cast<ImmOperand *>(insn4->opnds[2])->GetValue() == 0) {
    return true;
  } else {
    return false;
  }
}

/*opt long int compare with 0
   *cmp x0, #0
   csinv w0, wzr, wzr, GE
   csinc w0, w0, wzr, LE
   cmp w0, #0
   =>
   cmp x0, #0
 */
void AArch64Peep::LongIntCompareWithZ() {
  std::vector<Insn *> optInsn;
  FOR_ALL_BB(bb, cgfunc) {
    optInsn.clear();
    FOR_BB_INSNS(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      /// found pattern
      if (FindLondIntCmpWithZ(optInsn, insn) && PatternMatch(optInsn)) {
        Insn *newInsn = cg->BuildInstruction<AArch64Insn>(optInsn[0]->GetMachineOpcode(), optInsn[0]->opnds[0],
                                                          optInsn[0]->opnds[1], optInsn[0]->opnds[2]);
        bb->ReplaceInsn(optInsn[3], newInsn);
        optInsn.clear();
      }
    }
  }
}

/* Check if a regOpnd is live after insn. True if live, otherwise false.
 */
bool AArch64Peep::IfOperandIsLiveAfterInsn(RegOperand *regOpnd, Insn *insn) {
  CG_ASSERT(insn, "insn should not be nullptr.");

  for (Insn *nextInsn = insn->next; nextInsn && nextInsn != insn->bb->lastinsn->next; nextInsn = nextInsn->next) {
    if (!nextInsn->IsMachineInstruction()) {
      continue;
    }

    for (int i = Insn::kMaxOperandNum - 1; i >= 0; i--) {
      Operand *opnd = nextInsn->opnds[i];
      if (opnd == nullptr) {
        continue;
      }

      if (opnd->IsMemoryAccessOperand()) {
        MemOperand *mem = static_cast<MemOperand *>(opnd);
        Operand *base = mem->GetBaseRegister();
        Operand *offset = mem->GetOffset();

        if (base && base->IsRegister()) {
          RegOperand *tmpRegOpnd = static_cast<RegOperand *>(base);
          if (tmpRegOpnd->GetRegisterNumber() == regOpnd->GetRegisterNumber()) {
            return true;
          }
        }
        if (offset && offset->IsRegister()) {
          RegOperand *tmpRegOpnd = static_cast<RegOperand *>(offset);
          if (tmpRegOpnd->GetRegisterNumber() == regOpnd->GetRegisterNumber()) {
            return true;
          }
        }
      }

      if (opnd->IsRegister()) {
        RegOperand *tmpRegOpnd = static_cast<RegOperand *>(opnd);
        if (tmpRegOpnd->GetRegisterNumber() == regOpnd->GetRegisterNumber()) {
          const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(nextInsn)->mop_];
          AArch64OpndProp *regprop = static_cast<AArch64OpndProp *>(md->operand_[i]);
          bool isUse = regprop->IsUse();
          if (isUse) {
            return true;
          } else {
            // Redefined, no need to check live-out.
            return false;
          }
        }
      }
    }
  }

  // Check if it is live-out.
  if (insn->bb->liveout_regno.find(regOpnd->GetRegisterNumber()) != insn->bb->liveout_regno.end()) {
    return true;
  }

  return false;
}

/* add     x0, x1, #:lo12:Ljava_2Futil_2FLocale_241_3B_7C_24SwitchMap_24java_24util_24Locale_24Category
   ldr     x2, [x0]
   ==>
   ldr     x2, [x1, #:lo12:Ljava_2Futil_2FLocale_241_3B_7C_24SwitchMap_24java_24util_24Locale_24Category]
 */
void AArch64Peep::ComplexMemOperandOpt() {
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    Insn *nextInsn = nullptr;

    for (Insn *insn = bb->firstinsn; insn; insn = nextInsn) {
      nextInsn = insn->GetNextMachineInsn();
      if (nextInsn == nullptr) {
        continue;
      }
      MOperator thisMop = insn->GetMachineOpcode();
      if (thisMop == MOP_xadrpl12) {
        MOperator nextMop = nextInsn->GetMachineOpcode();
        if (nextMop &&
            ((nextMop >= MOP_wldrsb && nextMop <= MOP_dldp) || (nextMop >= MOP_wstrb && nextMop <= MOP_dstp))) {
          // Check if base register of nextInsn and the dest operand of insn are identical.
          AArch64MemOperand *memOpnd = static_cast<AArch64MemOperand *>(nextInsn->GetMemOpnd());
          CG_ASSERT(memOpnd != nullptr, "memOpnd is null in AArch64Peep::ComplexMemOperandOpt");

          // Only for AddrMode_B_OI addressing mode.
          if (memOpnd->GetAddrMode() != AArch64MemOperand::kAddrModeBOi) {
            continue;
          }

          // Only for intact memory addressing.
          if (!memOpnd->IsIntactIndexed()) {
            continue;
          }

          RegOperand *regOpnd = static_cast<RegOperand *>(insn->GetOperand(0));

          // Check if dest operand of insn is idential with base register of nextInsn.
          if (memOpnd->GetBaseRegister() != regOpnd) {
            continue;
          }

          // Check if x0 is used after ldr insn, and if it is in live-out.
          if (IfOperandIsLiveAfterInsn(regOpnd, nextInsn)) {
            continue;
          }

          StImmOperand *stImmOpnd = static_cast<StImmOperand *>(insn->GetOperand(2));
          AArch64OfstOperand *offopnd =
            acgfunc->GetOrCreateOfstOpnd(stImmOpnd->GetOffset() + memOpnd->GetOffsetImmediate()->GetOffsetValue(), 32);
          RegOperand *newBaseOpnd = static_cast<RegOperand *>(insn->GetOperand(1));
          AArch64MemOperand *newMemOpnd =
            acgfunc->GetOrCreateMemOpnd(AArch64MemOperand::kAddrModeLo12Li, memOpnd->GetSize(), newBaseOpnd, nullptr,
                                        offopnd, stImmOpnd->GetSymbol());

          nextInsn->SetOperand(1, newMemOpnd);
          bb->RemoveInsn(insn);
        }
      } else if (thisMop == MOP_xmovri32 || thisMop == MOP_xmovri64) {
        // mov x0, #1  (small val)    =>  mov x0, #1
        // ldr x1, [x2, x0, SXTW #2]      ldr x1, [x2, #4]
        MOperator nextMop = nextInsn->GetMachineOpcode();
        if (nextMop &&
            ((nextMop >= MOP_wldrsb && nextMop <= MOP_dldp) || (nextMop >= MOP_wstrb && nextMop <= MOP_dstp))) {
          AArch64MemOperand *memOpnd = static_cast<AArch64MemOperand *>(nextInsn->GetMemOpnd());
          if (memOpnd->GetAddrMode() == AArch64MemOperand::kAddrModeBOrX) {
            regno_t movReg = static_cast<RegOperand *>(insn->opnds[0])->GetRegisterNumber();
            regno_t offReg = memOpnd->GetOffsetRegister()->GetRegisterNumber();
            if (movReg == offReg) {
              int32 mul = static_cast<ImmOperand *>(insn->opnds[1])->GetValue() << memOpnd->ShiftAmount();
              if (mul < 256) {
                AArch64MemOperand *newMemOpnd =
                  acgfunc->GetOrCreateMemOpnd(AArch64MemOperand::kAddrModeBOi, memOpnd->GetSize(),
                    memOpnd->GetBaseRegister(), nullptr, acgfunc->GetOrCreateOfstOpnd(mul, 32), memOpnd->GetSymbol());
                nextInsn->SetOperand(1, newMemOpnd);
              }
            }
          }
        }
      }
    }
  }
}

/*
   add     x0, x0, x1, LSL #2
   ldr     x2, [x0]
   ==>
   ldr     x2, [x0,x1,LSL #2]
 */
void AArch64Peep::ComplexMemOperandOptLSL() {
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    Insn *nextInsn = nullptr;
    for (Insn *insn = bb->firstinsn; insn; insn = nextInsn) {
      nextInsn = insn->GetNextMachineInsn();
      if (nextInsn == nullptr) {
        continue;
      }
      MOperator thisMop = insn->GetMachineOpcode();
      if (thisMop == MOP_xaddrrrs) {
        MOperator nextMop = nextInsn->GetMachineOpcode();
        // load/store byte not eligible.
        if (nextMop &&
            ((nextMop >= MOP_wldrsh && nextMop <= MOP_dldr) || (nextMop >= MOP_wstrh && nextMop <= MOP_dstr))) {
          // Check if base register of nextInsn and the dest operand of insn are identical.
          AArch64MemOperand *memOpnd = static_cast<AArch64MemOperand *>(nextInsn->GetMemOpnd());
          CG_ASSERT(memOpnd != nullptr, "null ptr check");

          // Only for AddrMode_B_OI addressing mode.
          if (memOpnd->GetAddrMode() != AArch64MemOperand::kAddrModeBOi) {
            continue;
          }

          // Only for immediate is  0.
          if (memOpnd->GetOffsetImmediate()->GetOffsetValue() != 0) {
            continue;
          }

          // Only for intact memory addressing.
          if (!memOpnd->IsIntactIndexed()) {
            continue;
          }

          RegOperand *regOpnd = static_cast<RegOperand *>(insn->GetOperand(0));

          // Check if dest operand of insn is idential with base register of nextInsn.
          if (memOpnd->GetBaseRegister() != regOpnd) {
            continue;
          }

#ifdef USE_32BIT_REF
          if (nextInsn->IsAccessRefField() && nextInsn->GetOperand(0)->GetSize() > 32) {
            continue;
          }
#endif

          // Check if x0 is used after ldr insn, and if it is in live-out.
          if (IfOperandIsLiveAfterInsn(regOpnd, nextInsn)) {
            continue;
          }
          BitShiftOperand *lsl = static_cast<BitShiftOperand *>(insn->GetOperand(3));
          uint32 shftsize = lsl->GetShiftAmount();
          if ((nextMop == MOP_wldrsh || nextMop == MOP_wldrh || nextMop == MOP_wstrh) && shftsize > 1) {
            // half word only allows shift value of 1 or less
            continue;
          }
          if ((memOpnd->GetSize() == 32 && (shftsize != 0 && shftsize != 2)) ||
              (memOpnd->GetSize() == 64 && (shftsize != 0 && shftsize != 3))) {
            continue;
          }
          uint32 destsize = nextInsn->GetOperand(0)->GetSize();
          if ((destsize == 32 && (shftsize != 0 && shftsize != 2)) ||
              (destsize == 64 && (shftsize != 0 && shftsize != 3))) {
            continue;
          }

          RegOperand *newBaseOpnd = static_cast<RegOperand *>(insn->GetOperand(1));
          RegOperand *newIndexOpnd = static_cast<RegOperand *>(insn->GetOperand(2));
          AArch64MemOperand *newMemOpnd =
            acgfunc->GetOrCreateMemOpnd(AArch64MemOperand::kAddrModeBOrX, memOpnd->GetSize(), newBaseOpnd, newIndexOpnd,
                                        lsl->GetShiftAmount(), false);
          nextInsn->SetOperand(1, newMemOpnd);
          bb->RemoveInsn(insn);
        }
      }
    }
  }
}

/*
     mov     w1, #34464
     movk    w1, #1,  LSL #16
     sdiv    w2, w0, w1
   ========>
     mov     w1, #0x588f
     movk    w1, #0x4f8b, LSL #16
     smull   x2, w0, w1
     lsr     x2, x2, #32
     add     x2, x2, w0, SXTW
     lsr     x2, x2, #17
     add     x2, x2, x0, LSR #31
 */
void AArch64Peep::ReplaceDivisionToMultiplication() {
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  for (BB *bb = acgfunc->firstbb; bb; bb = bb->next) {
    Insn *prevInsn = nullptr;
    Insn *preprevInsn = nullptr;
    for (Insn *insn = bb->lastinsn; insn; insn = prevInsn) {
      prevInsn = insn->GetPreviousMachineInsn();
      if (prevInsn == nullptr) {
        continue;
      }
      MOperator thisMop = insn->GetMachineOpcode();
      if (thisMop == MOP_wsdivrrr) {
        preprevInsn = prevInsn->GetPreviousMachineInsn();
        if (preprevInsn == nullptr) {
          continue;
        }
        MOperator prevMop = prevInsn->GetMachineOpcode();
        MOperator preprevMop = preprevInsn->GetMachineOpcode();
        if (prevMop && (prevMop == MOP_wmovkri16) && preprevMop && (preprevMop == MOP_xmovri32)) {
          RegOperand *divisorOpnd = static_cast<RegOperand *>(insn->GetOperand(2));
          CG_ASSERT(divisorOpnd, "divisorOpnd should not be nullptr.");
          // Check if dest operand of insn is idential with  register of prevInsn and preprevInsn.
          if ((prevInsn->GetOperand(0) != divisorOpnd) || (preprevInsn->GetOperand(0) != divisorOpnd)) {
            continue;
          }

          LogicalShiftLeftOperand *prevLsl = static_cast<LogicalShiftLeftOperand *>(prevInsn->GetOperand(2));
          CG_ASSERT(prevLsl, "prevLsl should not be nullptr.");
          if (prevLsl->shift_amount != 16) {
            continue;
          }

          ImmOperand *prevImmOpnd = static_cast<ImmOperand *>(prevInsn->GetOperand(1));
          CG_ASSERT(prevImmOpnd, "prevImmOpnd should not be nullptr.");
          ImmOperand *preprevImmOpnd = static_cast<ImmOperand *>(preprevInsn->GetOperand(1));
          CG_ASSERT(preprevImmOpnd, "preprevImmOpnd should not be nullptr.");
          if ((prevImmOpnd->GetValue() != 1) || (preprevImmOpnd->GetValue() != 34464)) {
            continue;
          }

          // Check if x1 is used after sdiv insn, and if it is in live-out.
          if (divisorOpnd != insn->GetOperand(0)) {
            if (IfOperandIsLiveAfterInsn(divisorOpnd, insn)) {
              continue;
            }
          }

          // mov     w1, #0x588f
          ImmOperand *multiplierLow = acgfunc->CreateImmOperand(0x588f, 32, false);
          Insn *multiplierLowInsn = cg->BuildInstruction<AArch64Insn>(MOP_xmovri32, divisorOpnd, multiplierLow);
          bb->InsertInsnBefore(preprevInsn, multiplierLowInsn);

          // movk    w1, #0x4f8b, LSL #16
          ImmOperand *multiplierHigh = acgfunc->CreateImmOperand(0x4f8b, 32, false);
          LogicalShiftLeftOperand *multiplierHighLsl = acgfunc->GetLogicalShiftLeftOperand(16, true);
          Insn *multiplierHighInsn =
            cg->BuildInstruction<AArch64Insn>(MOP_wmovkri16, divisorOpnd, multiplierHigh, multiplierHighLsl);
          bb->InsertInsnBefore(preprevInsn, multiplierHighInsn);

          // smull   x2, w0, w1
          RegOperand *smullMultiplier = static_cast<RegOperand *>(insn->GetOperand(1));
          RegOperand *wSmullOpnd = static_cast<RegOperand *>(insn->GetOperand(0));
          regno_t extandSmullReg = wSmullOpnd->GetRegisterNumber();
          RegOperand *dstSmullOpnd =
            acgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)extandSmullReg, 64, kRegTyInt);
          Insn *newSmullInsn =
            cg->BuildInstruction<AArch64Insn>(MOP_xsmullrrr, dstSmullOpnd, smullMultiplier, divisorOpnd);
          bb->InsertInsnBefore(preprevInsn, newSmullInsn);

          // lsr     x2, x2, #32
          ImmOperand *dstLsrImmHigh = acgfunc->CreateImmOperand(32, 32, false);
          Insn *dstLsrInsnHigh =
            cg->BuildInstruction<AArch64Insn>(MOP_xlsrrri6, dstSmullOpnd, dstSmullOpnd, dstLsrImmHigh);
          bb->InsertInsnBefore(preprevInsn, dstLsrInsnHigh);

          // add     x2, x2, w0, SXTW
          Operand *sxtw = acgfunc->CreateExtendShiftOperand(ExtendShiftOperand::SXTW, 0, 3);
          Insn *addInsn =
            cg->BuildInstruction<AArch64Insn>(MOP_xxwaddrrre, dstSmullOpnd, dstSmullOpnd, smullMultiplier, sxtw);
          bb->InsertInsnBefore(preprevInsn, addInsn);

          // lsr     x2, x2, #17
          ImmOperand *dstLsrImmChange = acgfunc->CreateImmOperand(17, 32, false);
          Insn *dstLsrInsnChange =
            cg->BuildInstruction<AArch64Insn>(MOP_xlsrrri6, dstSmullOpnd, dstSmullOpnd, dstLsrImmChange);
          bb->InsertInsnBefore(preprevInsn, dstLsrInsnChange);

          // add     x2, x2, x0, LSR #31
          regno_t addOpndRegNum = smullMultiplier->GetRegisterNumber();
          RegOperand *extandSmullMultiplier =
            acgfunc->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)addOpndRegNum, 64, kRegTyInt);
          BitShiftOperand *addLsrOpnd = acgfunc->CreateBitShiftOperand(BitShiftOperand::LSR, 31, 6);
          Insn *addLsrInsn = cg->BuildInstruction<AArch64Insn>(MOP_xaddrrrs, dstSmullOpnd, dstSmullOpnd,
                                                               extandSmullMultiplier, addLsrOpnd);
          bb->InsertInsnBefore(preprevInsn, addLsrInsn);

          // remove insns
          bb->RemoveInsn(preprevInsn);
          bb->RemoveInsn(prevInsn);
          bb->RemoveInsn(insn);
        }
      }
    }
  }
}

void AArch64Peep::PrePeepholeOpt() {
  PreOptOneHoleBranches();
  ReplaceInstruction();
  RemoveIncDecRef2Param();
  LongIntCompareWithZ();
  ComplexMemOperandOpt();
  ComplexMemOperandOptLSL();
}

void AArch64Peep::PrePeepholeOpt1() {
  if (JAVALANG) {
    RemoveDecRef();
  }
  OptComputationTree();
  OptOneHoleBranches();
  if (JAVALANG) {
    ReplaceIncDecWithInc();
  }
  OptAndCmpBranchesToTbz();
}

/*Optimize the following patterns:
   1)
   orr  w21, w0, #0  ====> mov  w21, w0
   orr  w21, #0, w0  ====> mov  w21, w0

   2)
   ldr  w0, [x21,#68]        ldr  w0, [x21,#68]
   mov  w1, #-1              mov  w1, #-1
   cmp  w0, w1     ====>     cmn  w0, #-1
 */
void AArch64Peep::ReplaceInstruction() {
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    FOR_BB_INSNS(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      Operand *opndOfOrr = nullptr;
      Operand *opnd1OfMov = nullptr;
      Operand *opnd2OfMov = nullptr;
      ImmOperand *immOpnd = nullptr;
      AArch64RegOperand *reg1 = nullptr;
      AArch64RegOperand *reg2 = nullptr;
      MOperator thisMop = insn->GetMachineOpcode();
      switch (thisMop) {
        case MOP_wiorri12r: {  // opnd1 is Reg32 and opnd2 is immediate.
          opndOfOrr = insn->opnds[1];
          CG_ASSERT(opndOfOrr->IsIntImmediate(), "expects immediate operand");
          immOpnd = static_cast<ImmOperand *>(opndOfOrr);
          if (0 == immOpnd->GetValue()) {
            reg1 = static_cast<AArch64RegOperand *>(insn->opnds[0]);
            reg2 = static_cast<AArch64RegOperand *>(insn->opnds[2]);
            bb->ReplaceInsn(insn, cg->BuildInstruction<AArch64Insn>(MOperator(MOP_wmovrr), reg1, reg2));
          }
          break;
        }
        case MOP_wiorrri12: {  // opnd1 is reg32 and opnd3 is immediate.
          opndOfOrr = insn->opnds[2];
          CG_ASSERT(opndOfOrr->IsIntImmediate(), "expects immediate operand");
          immOpnd = static_cast<ImmOperand *>(opndOfOrr);
          if (0 == immOpnd->GetValue()) {
            reg1 = static_cast<AArch64RegOperand *>(insn->opnds[0]);
            reg2 = static_cast<AArch64RegOperand *>(insn->opnds[1]);
            bb->ReplaceInsn(insn, cg->BuildInstruction<AArch64Insn>(MOperator(MOP_wmovrr), reg1, reg2));
          }
          break;
        }
        case MOP_xiorri13r: {  // opnd1 is Reg64 and opnd2 is immediate.
          opndOfOrr = insn->opnds[1];
          CG_ASSERT(opndOfOrr->IsIntImmediate(), "expects immediate operand");
          immOpnd = static_cast<ImmOperand *>(opndOfOrr);
          if (0 == immOpnd->GetValue()) {
            reg1 = static_cast<AArch64RegOperand *>(insn->opnds[0]);
            reg2 = static_cast<AArch64RegOperand *>(insn->opnds[2]);
            bb->ReplaceInsn(insn, cg->BuildInstruction<AArch64Insn>(MOperator(MOP_xmovrr), reg1, reg2));
          }
          break;
        }
        case MOP_xiorrri13: {  // opnd1 is reg64 and opnd3 is immediate.
          opndOfOrr = insn->opnds[2];
          CG_ASSERT(opndOfOrr->IsIntImmediate(), "expects immediate operand");
          immOpnd = static_cast<ImmOperand *>(opndOfOrr);
          if (0 == immOpnd->GetValue()) {
            reg1 = static_cast<AArch64RegOperand *>(insn->opnds[0]);
            reg2 = static_cast<AArch64RegOperand *>(insn->opnds[1]);
            bb->ReplaceInsn(insn, cg->BuildInstruction<AArch64Insn>(MOperator(MOP_xmovrr), reg1, reg2));
          }
          break;
        }
        case MOP_xmovri32: {
          opnd1OfMov = insn->opnds[0];
          opnd2OfMov = insn->opnds[1];
          if (opnd2OfMov->IsIntImmediate()) {
            immOpnd = static_cast<ImmOperand *>(opnd2OfMov);
            int64 iVal = immOpnd->GetValue();
            if (NEGATIVE_IMM_LOWER_LIMIT <= iVal && 0 > iVal) {
              Insn *nextInsn = insn->GetNextMachineInsn();  // get the next insn to judge if it is a cmp instruction.
              if (nextInsn != nullptr) {
                Operand *opndCmp2 = nextInsn->opnds[1];
                Operand *opndCmp3 = nullptr;
                if (MOP_wcmprr == nextInsn->GetMachineOpcode()) {
                  opndCmp3 = nextInsn->opnds[2];  // get the third operand of cmp
                  if (opnd1OfMov ==
                      opndCmp3) {  // if the first operand of mov equals the third operand of cmp, match the pattern.
                    ImmOperand *newOpnd = acgfunc->CreateImmOperand(iVal * (-1), immOpnd->size_, false);
                    Operand *regFlag = nextInsn->GetOperand(0);
                    bb->ReplaceInsn(
                      nextInsn, cg->BuildInstruction<AArch64Insn>(MOperator(MOP_wcmnri), regFlag, opndCmp2, newOpnd));
                  }
                }
              }
            }
          }
          break;
        }
        case MOP_xmovri64: {
          opnd1OfMov = insn->opnds[0];
          opnd2OfMov = insn->opnds[1];
          if (opnd2OfMov->IsIntImmediate()) {
            immOpnd = static_cast<ImmOperand *>(opnd2OfMov);
            int64 iVal = immOpnd->GetValue();
            if (NEGATIVE_IMM_LOWER_LIMIT <= iVal && 0 > iVal) {
              Insn *nextInsn = insn->GetNextMachineInsn();  // get the next insn to judge if it is a cmp instruction.
              if (nextInsn != nullptr) {
                Operand *opndCmp2 = nextInsn->opnds[1];
                Operand *opndCmp3 = nullptr;
                if (MOP_xcmprr == nextInsn->GetMachineOpcode()) {
                  opndCmp3 = nextInsn->opnds[2];  // get the third operand of cmp
                  if (opnd1OfMov ==
                      opndCmp3) {  // if the first operand of mov equals the third operand of cmp, match the pattern.
                    ImmOperand *newOpnd = acgfunc->CreateImmOperand(iVal * (-1), immOpnd->size_, false);
                    Operand *regFlag = nextInsn->GetOperand(0);
                    bb->ReplaceInsn(
                      nextInsn, cg->BuildInstruction<AArch64Insn>(MOperator(MOP_xcmnri), regFlag, opndCmp2, newOpnd));
                  }
                }
              }
            }
          }
          break;
        }
        default:
          break;
      }
    }
  }
}

/*Optimize the following patterns of issue#1783:
   and  w0, w6, #1  ====> tbz  w6, 0, .label
   cmp  w0, #1
   bne  .label

   and  w0, w6, #16  ====> tbz  w6, 4, .label
   cmp  w0, #16
   bne  .label

   and  w0, w6, #32  ====> tbnz  w6, 5, .label
   cmp  w0, #32
   beq  .label

   and  x0, x6, #32  ====> tbz  x6, 5, .label
   cmp  x0, #0
   beq  .label

   and  x0, x6, #32  ====> tbnz  x6, 5, .label
   cmp  x0, #0
   bne  .label
 */
void AArch64Peep::OptAndCmpBranchesToTbz() {
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    // get the last insn of BB and judge if its type is bne or beq.
    if (bb->lastinsn == nullptr) {
      continue;
    }
    Insn *insnNext = bb->lastinsn;
    MOperator mopB = insnNext->GetMachineOpcode();
    if (mopB != MOP_beq && mopB != MOP_bne) {
      continue;
    }
    LabelOperand *label = static_cast<LabelOperand *>(insnNext->opnds[1]);
    // get the instruction before bne/beq, expects its type is cmp.
    Insn *insn = nullptr;
    insn = insnNext->GetPreviousMachineInsn();
    if (insn != nullptr && (insn->GetMachineOpcode() == MOP_wcmpri || insn->GetMachineOpcode() == MOP_xcmpri)) {
      Insn *insnPrev;
      // get the instruction before "cmp", expect its type is "and".
      insnPrev = insn->GetPreviousMachineInsn();
      if (insnPrev == nullptr) {
        continue;
      }
      MOperator mopAnd = insnPrev->GetMachineOpcode();
      if (mopAnd == MOP_wandrri12 || mopAnd == MOP_xandrri13) {
        if (insn->opnds[1] != insnPrev->opnds[0]) {  // the real register of "cmp" and "and" must be the same.
          continue;
        }
        if (!insnPrev->opnds[2]->IsIntImmediate() || !insn->opnds[2]->IsIntImmediate()) {
          continue;
        }
        ImmOperand *immAnd = static_cast<ImmOperand *>(insnPrev->opnds[2]);
        ImmOperand *immCmp = static_cast<ImmOperand *>(insn->opnds[2]);
        if (0 == immCmp->GetValue()) {
          int n = IsPowerOf2(immAnd->GetValue());
          if (n < 0) {
            continue;
          }
          // judge whether the flag_reg and "w0" is live later.
          RegOperand *flagReg = static_cast<RegOperand *>(insn->GetOperand(0));
          regno_t flagRegno = flagReg->GetRegisterNumber();
          RegOperand *cmpReg = static_cast<RegOperand *>(insn->GetOperand(1));
          regno_t cmpRegno = cmpReg->GetRegisterNumber();
          if (bb->liveout_regno.find(flagRegno) != bb->liveout_regno.end() ||
              bb->liveout_regno.find(cmpRegno) != bb->liveout_regno.end()) {
            continue;
          }
          MOperator mopNew = MOP_undef;
          switch (mopB) {
            case MOP_beq:
              if (mopAnd == MOP_wandrri12) {
                mopNew = MOP_wtbz;
              } else if (mopAnd == MOP_xandrri13) {
                mopNew = MOP_xtbz;
              }
              break;
            case MOP_bne:
              if (mopAnd == MOP_wandrri12) {
                mopNew = MOP_wtbnz;
              } else if (mopAnd == MOP_xandrri13) {
                mopNew = MOP_xtbnz;
              }
              break;
            default:
              CG_ASSERT(false, "expects beq or bne insn");
          }
          ImmOperand *newImm = acgfunc->CreateImmOperand(n, 8, false);
          bb->InsertInsnAfter(insnNext, cg->BuildInstruction<AArch64Insn>(
                                          mopNew, static_cast<AArch64RegOperand *>(insnPrev->opnds[1]), newImm, label));
          bb->RemoveInsn(insnNext);
          bb->RemoveInsn(insn);
          bb->RemoveInsn(insnPrev);
        } else {
          int n = IsPowerOf2(immAnd->GetValue());
          int m = IsPowerOf2(immCmp->GetValue());
          if (n < 0 || m < 0 || n != m) {
            continue;
          }
          // judge whether the flag_reg and "w0" is live later.
          RegOperand *flagReg = static_cast<RegOperand *>(insn->GetOperand(0));
          regno_t flagRegno = flagReg->GetRegisterNumber();
          RegOperand *cmpReg = static_cast<RegOperand *>(insn->GetOperand(1));
          regno_t cmpRegno = cmpReg->GetRegisterNumber();
          if (bb->liveout_regno.find(flagRegno) != bb->liveout_regno.end() ||
              bb->liveout_regno.find(cmpRegno) != bb->liveout_regno.end()) {
            continue;
          }
          MOperator mopNew = MOP_undef;
          switch (mopB) {
            case MOP_beq:
              if (mopAnd == MOP_wandrri12) {
                mopNew = MOP_wtbnz;
              } else if (mopAnd == MOP_xandrri13) {
                mopNew = MOP_xtbnz;
              }
              break;
            case MOP_bne:
              if (mopAnd == MOP_wandrri12) {
                mopNew = MOP_wtbz;
              } else if (mopAnd == MOP_xandrri13) {
                mopNew = MOP_xtbz;
              }
              break;
            default:
              CG_ASSERT(false, "expects beq or bne insn");
          }
          ImmOperand *newImm = acgfunc->CreateImmOperand(n, 8, false);
          bb->InsertInsnAfter(insnNext, cg->BuildInstruction<AArch64Insn>(
                                          mopNew, static_cast<AArch64RegOperand *>(insnPrev->opnds[1]), newImm, label));
          bb->RemoveInsn(insnNext);
          bb->RemoveInsn(insn);
          bb->RemoveInsn(insnPrev);
        }
      }
    }
  }
}

/*Optimize the following patterns of issue#1813&issue2488:
   and  w0, w0, #1  ====> and  w0, w0, #1
   cmp  w0, #1
   cset w0, EQ

   and  w0, w0, #1  ====> and  w0, w0, #1
   cmp  w0, #0
   cset w0, NE
   ---------------------------------------------------
   and  w0, w0, #imm  ====> ubfx  w0, w0, pos, size
   cmp  w0, #imm
   cset w0, EQ

   and  w0, w0, #imm  ====> ubfx  w0, w0, pos, size
   cmp  w0, #0
   cset w0, NE
   conditions:
   imm is pos power of 2
 */
void AArch64Peep::OptAndCmpBranchesToCset() {
  AArch64CGFunc *acgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, acgfunc) {
    FOR_BB_INSNS_REV(insn, bb) {
      if (!insn->IsMachineInstruction() ||
          (insn->GetMachineOpcode() != MOP_wcsetrc && insn->GetMachineOpcode() != MOP_xcsetrc)) {
        continue;
      }
      // prevInsn must be "cmp" insn
      Insn *prevInsn = insn->GetPreviousMachineInsn();
      if (prevInsn == nullptr ||
          (prevInsn->GetMachineOpcode() != MOP_wcmpri && prevInsn->GetMachineOpcode() != MOP_xcmpri)) {
        continue;
      }
      // prevPrevInsn must be "and" insn
      Insn *prevPrevInsn = prevInsn->GetPreviousMachineInsn();
      if (prevPrevInsn == nullptr ||
          (prevPrevInsn->GetMachineOpcode() != MOP_wandrri12 && prevPrevInsn->GetMachineOpcode() != MOP_xandrri13)) {
        continue;
      }

      CondOperand *csetCond = static_cast<CondOperand *>(insn->GetOperand(1));
      ImmOperand *cmpImm = static_cast<ImmOperand *>(prevInsn->GetOperand(2));
      int64 cmpImmVal = cmpImm->GetValue();
      ImmOperand *andImm = static_cast<ImmOperand *>(prevPrevInsn->GetOperand(2));
      int64 andImmVal = andImm->GetValue();
      if ((csetCond->GetCode() == CC_EQ && cmpImmVal == andImmVal) ||
          (csetCond->GetCode() == CC_NE && cmpImmVal == 0)) {
        // if flag_reg of "cmp" is live later, we can't remove cmp insn.
        RegOperand *flagReg = static_cast<RegOperand *>(prevInsn->GetOperand(0));
        regno_t flagRegno = flagReg->GetRegisterNumber();
        if (bb->liveout_regno.find(flagRegno) != bb->liveout_regno.end()) {
          continue;
        }

        RegOperand *csetReg = static_cast<RegOperand *>(insn->GetOperand(0));
        if (andImmVal == 1) {
          if (!IsSameReg(csetReg, prevInsn->GetOperand(1)) || !IsSameReg(csetReg, prevPrevInsn->GetOperand(0))) {
            continue;
          }
          // save the "and" insn only.
          bb->RemoveInsn(insn);
          bb->RemoveInsn(prevInsn);
          insn = prevPrevInsn;
        } else {
          if (!IsSameRegNumAndRegSize(csetReg, prevInsn->GetOperand(1)) ||
              !IsSameRegNumAndRegSize(csetReg, prevPrevInsn->GetOperand(0)) ||
              !IsSameRegNumAndRegSize(csetReg, prevPrevInsn->GetOperand(1))) {
            continue;
          }

          // andImmVal is n power of 2
          int n = IsPowerOf2(andImmVal);
          if (n < 0) {
            continue;
          }

          // create ubfx insn
          MOperator ubfxOp = (csetReg->GetSize() <= 32) ? MOP_wubfxrri5i5 : MOP_xubfxrri6i6;
          AArch64RegOperand *reg = static_cast<AArch64RegOperand *>(csetReg);
          ImmOperand *bitPos = acgfunc->CreateImmOperand(n, 8, false);
          ImmOperand *bitSize = acgfunc->CreateImmOperand(1, 8, false);
          Insn *ubfxInsn = cg->BuildInstruction<AArch64Insn>(ubfxOp, reg, reg, bitPos, bitSize);
          bb->InsertInsnBefore(prevPrevInsn, ubfxInsn);
          bb->RemoveInsn(insn);
          bb->RemoveInsn(prevInsn);
          bb->RemoveInsn(prevPrevInsn);
          insn = ubfxInsn;
        }
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
  AArch64Peep *peep = new AArch64Peep(cgfunc);
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
  AArch64Peep *peep = new AArch64Peep(cgfunc);
  peep->PrePeepholeOpt1();
  delete peep;
  return nullptr;
}

AnalysisResult *CgDoPeepHole0::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  LiveAnalysis *live = nullptr;
  if (g->optim_level >= 2) {
    live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));
  }
  AArch64Peep *peep = new AArch64Peep(cgfunc);
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
  AArch64Peep *peep = new AArch64Peep(cgfunc);
  peep->PeepholeOpt();
  delete peep;
  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  return nullptr;
}

AnalysisResult *CgFixShortBranch::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  AArch64Peep *peep = new AArch64Peep(cgfunc);
  peep->FixShortBranches();
  delete peep;
  return nullptr;
}

}  // namespace maplebe
