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

#include "riscv64_reaching_definition.h"
#include "riscv64_cg.h"
#include "riscv64_operand.h"
namespace maplebe {

/*check wether exception may be thrown between headInsn and tailInsn
   return true if exception may be thrown. Otherwise return false.
 */
bool Riscv64ReachingDefinition::MayHasThrow(const Insn *headInsn, const Insn *tailInsn) {
  CG_ASSERT(headInsn && tailInsn && headInsn->bb == tailInsn->bb, "");

  if (headInsn->id >= tailInsn->id) {
    return false;
  }

  for (Insn *insn = tailInsn->prev; insn != headInsn; insn = insn->prev) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }
    if (insn->CanThrow()) {
      // If it is a memory operation and base register is FP, then continue.
      if (insn->IsLoad() || insn->IsStore()) {
        for (int i = 0; i < Insn::kMaxOperandNum; i++) {
          Operand *opnd = insn->opnds[i];
          if (opnd == nullptr || !opnd->IsMemoryAccessOperand()) {
            continue;
          }
          MemOperand *memopnd = static_cast<MemOperand *>(opnd);
          Operand *base = memopnd->GetBaseRegister();
          if (base != nullptr && IsFrameReg(base)) {
            break;
          } else {
            return true;
          }
        }
      } else {
        return true;
      }
    }
  }
  return false;
}

/*insert pseudoInsn for parameters definition*/
void Riscv64ReachingDefinition::InitStartGen() {
  BB *bb = cgfunc->firstbb;

  // Parameters should be define first.
  ParmLocator parmlocator(cgfunc->becommon);
  PLocInfo ploc;
  for (uint32 i = 0; i < cgfunc->func->formalDefVec.size(); i++) {
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(cgfunc->func->formalDefVec[i].formalTyIdx);
    parmlocator.LocateNextParm(ty, ploc);
    if (ploc.reg0 == 0) {
      // If is a large frame, parameter addressing mode is based vreg:Vra.
      continue;
    }

    int32 symsize = cgfunc->becommon.type_size_table[ty->tyIdx.GetIdx()];
    if (symsize > 8) {
      // For C structure passing in one or two registers.
      symsize = 8;
    }
    RegType regtype = ploc.reg0 < V0 ? kRegTyInt : kRegTyFloat;
    uint8 srcBitsize = uint8((symsize < 4 ? 4 : symsize) * BITS_PER_BYTE);

    Riscv64CGFunc *aarchcgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
    RegOperand *regopnd = aarchcgfunc->GetOrCreatePhysicalRegisterOperand(ploc.reg0, srcBitsize, regtype);

    MOperator mop;
    if (regtype == kRegTyInt) {
      if (srcBitsize <= 32) {
        mop = MOP_pseudo_param_def_w;
      } else {
        mop = MOP_pseudo_param_def_x;
      }
    } else {
      if (srcBitsize <= 32) {
        mop = MOP_pseudo_param_def_s;
      } else {
        mop = MOP_pseudo_param_def_d;
      }
    }

    Insn *pseudoInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(mop, regopnd);
    bb->InsertInsnBegin(pseudoInsn);
    pseudoInsns.insert(pseudoInsn);

    if (ploc.reg1) {
      regopnd = aarchcgfunc->GetOrCreatePhysicalRegisterOperand(ploc.reg1, srcBitsize, regtype);
      pseudoInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(mop, regopnd);
      bb->InsertInsnBegin(pseudoInsn);
      pseudoInsns.insert(pseudoInsn);
    }

    {
      /* Define memory address since store param may be transfered to stp and which with the short offset range.
         We can not get the correct definition before RA.
         Example:
          add  x8, sp, #712
          stp  x0, x1, [x8]    // store param: _this Reg40_R313644
          stp  x2, x3, [x8,#16]    // store param: Reg41_R333743 Reg42_R333622
          stp  x4, x5, [x8,#32]    // store param: Reg43_R401297 Reg44_R313834
          str  x7, [x8,#48]    // store param: Reg46_R401297

         Fix me: Remove the following code.
                 Need to support base register = FP + constant value addressing mode.
       */
      MIRSymbol *sym = cgfunc->func->formalDefVec[i].formalSym;
      if (!sym->IsPreg()) {
        MIRSymbol *firstsym = cgfunc->func->formalDefVec[i].formalSym;
        Riscv64SymbolAlloc *firstsymloc =
          static_cast<Riscv64SymbolAlloc *>(cgfunc->memlayout->sym_alloc_table[firstsym->GetStIndex()]);
        int32 stoffset = cgfunc->GetBaseOffset(firstsymloc);
        MIRType *firstty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(cgfunc->func->formalDefVec[i].formalTyIdx);
        int32 firstsymsize = cgfunc->becommon.type_size_table[firstty->tyIdx.GetIdx()];
        int8 firststksize = firstsymsize < 4 ? 4 : firstsymsize;

        Riscv64MemOperand *memOpnd = cgfunc->memPool->New<Riscv64MemOperand>(RFP, stoffset, firststksize * 8);
        MOperator mop = firststksize <= 4 ? MOP_pseudo_param_store_w : MOP_pseudo_param_store_x;
        Insn *pseudoInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(mop, memOpnd);
        bb->InsertInsnBegin(pseudoInsn);
        pseudoInsns.insert(pseudoInsn);
      }
    }
  }

  // If function has "bl  MCC_InitializeLocalStackRef", should define corresponding memory.
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);

  for (int i = 0; i < a64cgfunc->refCount; i++) {
    Riscv64MemOperand *memOpnd = cgfunc->memPool->New<Riscv64MemOperand>(RFP, a64cgfunc->beginOffset + i * 8, 64);
    Insn *pseudoInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_pseudo_ref_init_x, memOpnd);

    bb->InsertInsnBegin(pseudoInsn);
    pseudoInsns.insert(pseudoInsn);
  }
}

/*insert pseudoInsns for ehBB, R0 and R1 are defined in pseudoInsns*/
void Riscv64ReachingDefinition::InitEhDefine(BB *bb) {
  Riscv64CGFunc *aarchcgfunc = static_cast<Riscv64CGFunc *>(cgfunc);

  // Insert MOP_pseudo_eh_def_x R1.
  RegOperand *regopnd = aarchcgfunc->GetOrCreatePhysicalRegisterOperand(R1, 64, kRegTyInt);
  Insn *pseudoInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_pseudo_eh_def_x, regopnd);
  bb->InsertInsnBegin(pseudoInsn);
  pseudoInsns.insert(pseudoInsn);

  // Insert MOP_pseudo_eh_def_x R0.
  regopnd = aarchcgfunc->GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
  pseudoInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_pseudo_eh_def_x, regopnd);
  bb->InsertInsnBegin(pseudoInsn);
  pseudoInsns.insert(pseudoInsn);
}

/*insert pseudoInsns for return value R0/V0*/
void Riscv64ReachingDefinition::AddRetPseudoInsn(BB *bb) {
  Riscv64reg_t regno = static_cast<Riscv64CGFunc *>(cgfunc)->GetReturnRegisterNumber();
  if (regno == INVALID_REGNO) {
    return;
  }

  Insn *retInsn = nullptr;
  if (regno == R10) {
    RegOperand *regopnd =
      static_cast<Riscv64CGFunc *>(cgfunc)->GetOrCreatePhysicalRegisterOperand(regno, 64, kRegTyInt);
    retInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_pseudo_ret_int, regopnd);
  } else {
    CG_ASSERT(regno == V10, "CG internal error. Return value should be R0 or V0.");
    RegOperand *regopnd =
      static_cast<Riscv64CGFunc *>(cgfunc)->GetOrCreatePhysicalRegisterOperand(regno, 64, kRegTyFloat);
    retInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_pseudo_ret_float, regopnd);
  }

  bb->AppendInsn(retInsn);
  pseudoInsns.insert(retInsn);
}

void Riscv64ReachingDefinition::AddRetPseudoInsns() {
  uint32 exitbbsize = cgfunc->exitbbsvec.size();

  if (exitbbsize == 0) {
    if (cgfunc->lastbb->prev->firststmt == cgfunc->cleanup_label && cgfunc->lastbb->prev->prev) {
      AddRetPseudoInsn(cgfunc->lastbb->prev->prev);
    } else {
      AddRetPseudoInsn(cgfunc->lastbb->prev);
    }
  } else {
    for (uint32 i = 0; i < exitbbsize; i++) {
      AddRetPseudoInsn(cgfunc->exitbbsvec[i]);
    }
  }
}

/* We should add caller-saved regs to kill list, but it may significantly worse compile time.
   Before RA: we just erase the corresponding gen eliment and add R0/V0 to kill list since only
              R0/V0 may be defined more than once before RA.
   Note: 1): Optimization: After merge call BB, add kill R0/V0 once each BB.
         2): Add caller-saved regs to kill list if analysis after RA.
 */
void Riscv64ReachingDefinition::KillAllCallerSavedRegs(BB *bb, Insn *callInsn) {
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  RegOperand *phyopnd;
  int i;

  phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
  bb->kill.insert(DataAnalysisInfo(kInfoReg, phyopnd));
  bb->gen.erase(DataAnalysisInfo(kInfoReg, phyopnd));

  phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand(V0, 64, kRegTyInt);
  bb->kill.insert(DataAnalysisInfo(kInfoReg, phyopnd));
  bb->gen.erase(DataAnalysisInfo(kInfoReg, phyopnd));

  for (i = R1; i <= R18; i++) {
    phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)i, 64, kRegTyInt);
    bb->gen.erase(DataAnalysisInfo(kInfoReg, phyopnd));
  }

  for (i = V0; i <= V7; i++) {
    phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)i, 64, kRegTyInt);
    bb->gen.erase(DataAnalysisInfo(kInfoReg, phyopnd));
  }

  for (i = V16; i <= V31; i++) {
    phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)i, 64, kRegTyFloat);
    bb->gen.erase(DataAnalysisInfo(kInfoReg, phyopnd));
  }
}

void Riscv64ReachingDefinition::DirtyAllNonStackMem(BB *bb) {
  return;
}

/* We should add caller-saved regs to kill list, but it may significantly worse compile time.
   So we just erase the corresponding gen eliment if before RA.
   Note: Add caller-saved regs to kill list if analysis after RA.
 */
void Riscv64ReachingDefinition::KillAllCallerSavedRegsOnBBIn(BB *bb, Insn *callInsn) {
  map<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>, DataAnalysisInfoCmp>::iterator itIn;
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  RegOperand *phyopnd = nullptr;
  int i;

  for (i = R0; i <= R18; i++) {
    phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)i, 64, kRegTyInt);

    itIn = bb->in.find(DataAnalysisInfo(kInfoReg, phyopnd));
    if (itIn != bb->in.end()) {
      // Save redefine.
      for (auto insnInfoElem : itIn->second) {
        Insn *oldDefInsn = insnInfoElem.GetInsn();
        int idx = insnInfoElem.GetProperty() & DataInsnInfo::kIndexQuickcalc;
        if (!oldDefInsn->redefine[idx]) {
          oldDefInsn->redefine[idx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
        }

        oldDefInsn->redefine[idx]->insert(DataInsnInfo(callInsn, -1));
      }

      bb->in.erase(itIn);
    }
  }

  for (i = V0; i <= V7; i++) {
    phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)i, 64, kRegTyInt);

    itIn = bb->in.find(DataAnalysisInfo(kInfoReg, phyopnd));
    if (itIn != bb->in.end()) {
      // Save redefine.
      for (auto insnInfoElem : itIn->second) {
        Insn *oldDefInsn = insnInfoElem.GetInsn();
        int idx = insnInfoElem.GetProperty() & DataInsnInfo::kIndexQuickcalc;
        if (!oldDefInsn->redefine[idx]) {
          oldDefInsn->redefine[idx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
        }

        oldDefInsn->redefine[idx]->insert(DataInsnInfo(callInsn, -1));
      }

      bb->in.erase(itIn);
    }
  }

  for (i = V16; i <= V31; i++) {
    phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)i, 64, kRegTyInt);

    itIn = bb->in.find(DataAnalysisInfo(kInfoReg, phyopnd));
    if (itIn != bb->in.end()) {
      // Save redefine.
      for (auto insnInfoElem : itIn->second) {
        Insn *oldDefInsn = insnInfoElem.GetInsn();
        int idx = insnInfoElem.GetProperty() & DataInsnInfo::kIndexQuickcalc;
        if (!oldDefInsn->redefine[idx]) {
          oldDefInsn->redefine[idx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
        }

        oldDefInsn->redefine[idx]->insert(DataInsnInfo(callInsn, -1));
      }

      bb->in.erase(itIn);
    }
  }
}

/* For riscv64, R0/V0 as the return register.
 */
void Riscv64ReachingDefinition::GenReturnValue(BB *bb, Insn *insn) {
  map<DataAnalysisInfo, DataInsnInfo, DataAnalysisInfoCmp>::iterator itGen;

  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  RegOperand *phyopnd = nullptr;
  if (insn->ret_type == Insn::kRegInt) {
    phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)R0, 64, kRegTyInt);
    bb->gen.insert(pair<DataAnalysisInfo, DataInsnInfo>(DataAnalysisInfo(kInfoReg, phyopnd), DataInsnInfo(insn, -1)));
  } else if (insn->ret_type == Insn::kRegFloat) {
    phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)V0, 64, kRegTyFloat);
    bb->gen.insert(pair<DataAnalysisInfo, DataInsnInfo>(DataAnalysisInfo(kInfoReg, phyopnd), DataInsnInfo(insn, -1)));
  }
}

/* For riscv64, R0/V0 as the return register.
 */
void Riscv64ReachingDefinition::GenReturnValueOnBBIn(BB *bb, Insn *insn) {
  map<DataAnalysisInfo, MapleSet<DataInsnInfo, DataInsnInfoCmp>, DataAnalysisInfoCmp>::iterator itIn;

  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
  RegOperand *phyopnd = nullptr;
  set<DataInsnInfo, DataInsnInfoCmp> s;
  s.insert(DataInsnInfo(insn, -1));
  if (insn->ret_type == Insn::kRegInt) {
    RegOperand *phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)R0, 64, kRegTyInt);
    bb->in.insert(pair<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>>(DataAnalysisInfo(kInfoReg, phyopnd), s));
  } else if (insn->ret_type == Insn::kRegFloat) {
    phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)V0, 64, kRegTyFloat);
    bb->in.insert(pair<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>>(DataAnalysisInfo(kInfoReg, phyopnd), s));
  }
}

/*initialize kill and gen for bb*/
void Riscv64ReachingDefinition::InitKillGen(BB *bb, int mode) {
  if (bb->IsEmpty()) {
    return;
  }

  FOR_BB_INSNS(insn, bb) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }

    const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->mop_];

    if (insn->IsCall()) {
      if (mode & RD_REGANALYSIS) {
        // Kill all the caller-saved registers.
        KillAllCallerSavedRegs(bb, insn);

        // Gen return value R0.
        GenReturnValue(bb, insn);
      }
      continue;
    }

    for (int i = 0; i < Insn::kMaxOperandNum; i++) {
      Operand *opnd = insn->opnds[i];
      if (opnd == nullptr) {
        continue;
      }
      Riscv64OpndProp *regprop = static_cast<Riscv64OpndProp *>(md->operand_[i]);
      bool isDef = regprop->IsDef();
      if (!isDef && !opnd->IsMemoryAccessOperand()) {
        continue;
      }

      if (opnd->IsList()) {
        CG_ASSERT(false, "Internal error, list operand should not be defined.");
      } else if (opnd->IsMemoryAccessOperand()) {
        // Need to consider pre/post index addressing.
        // Current not exist before RA.
        Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(opnd);
        RegOperand *base = memopnd->GetBaseRegister();
        RegOperand *index = memopnd->GetIndexRegister();

        if (base == nullptr) {
          // Do nothing.
        } else {
          // Check if it is a pre/post index memory operand.
          if (!isDef || !(mode & RD_MEMANALYSIS)) {
            continue;
          };

          if (IsFrameReg(base)) {
            if (index) {
              CG_ASSERT(false, "Exist [x29 + index register addressing ].");
              // Currently dirty all of the memory definition.
              // Can do analysis of index register for optimization.
              DirtyAllStackMemGen(bb);

              // For Reaching definition, internal_flag1 indicates this BB
              // has an instruction that will dirty all the memory definition.
              bb->internal_flag1 = true;
            } else {
              DefineMem(bb, memopnd, insn, i);
            }
          } else {
            // Can build non-stack memory info here.
          }
        }
      } else if (opnd->IsConditionCode()) {
        // Can add RFlag mapping.
        if (mode & RD_REGANALYSIS) {
          DefineRegister(bb, static_cast<RegOperand *>(opnd), insn, i);
        }
      } else if (opnd->IsRegister()) {
        // Register.
        if (mode & RD_REGANALYSIS) {
          if (insn->GetMachineOpcode() == MOP_pseudo_eh_def_x) {
            DefineRegister(bb, static_cast<RegOperand *>(opnd), insn, i, DataInsnInfo::kMaydef);
          } else if (insn->IsPartDef()) {
            DefineRegister(bb, static_cast<RegOperand *>(opnd), insn, i, DataInsnInfo::kPartdef);
          } else {
            DefineRegister(bb, static_cast<RegOperand *>(opnd), insn, i,
                           i ? DataInsnInfo::kSecondMemAddr : DataInsnInfo::kNormal);
          }
        }
      }
    }
  }
}

/* This function will change bb->in
   add definition DataInsnInfo for used Operand.
 */
void Riscv64ReachingDefinition::GenerateUseDef(BB *bb, int mode) {
  map<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>, DataAnalysisInfoCmp>::iterator itMap;

  for (Insn *insn = bb->firstinsn; insn; insn = insn->next) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }

    const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->mop_];

    for (int i = 0; i < Insn::kMaxOperandNum; i++) {
      Operand *opnd = insn->opnds[i];
      if (opnd == nullptr) {
        continue;
      }

      Riscv64OpndProp *regprop = static_cast<Riscv64OpndProp *>(md->operand_[i]);

      if (opnd->IsMemoryAccessOperand()) {
        // Build use->def from base/index register to it's define.
        Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(opnd);
        RegOperand *baseRegister = memopnd->GetBaseRegister();
        if (baseRegister) {
          itMap = bb->in.find(DataAnalysisInfo(kInfoReg, baseRegister));
          if (itMap != bb->in.end()) {
            insn->defs[i + 2] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
            for (auto s : itMap->second) {
              insn->defs[i + 2]->insert(s);
            }
          }
        }
        RegOperand *indexRegister = memopnd->GetIndexRegister();
        if (indexRegister) {
          itMap = bb->in.find(DataAnalysisInfo(kInfoReg, indexRegister));
          if (itMap != bb->in.end()) {
            insn->defs[i + 3] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
            for (auto s : itMap->second) {
              insn->defs[i + 3]->insert(s);
            }
          }
        }
      }

      if (regprop->IsUse()) {
        if (opnd->IsRegister() || opnd->IsMemoryAccessOperand()) {
          itMap = bb->in.find(DataAnalysisInfo(opnd->IsRegister() ? kInfoReg : kInfoMem, opnd));
          if (itMap != bb->in.end()) {
            insn->defs[i] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
            for (auto s : itMap->second) {
              insn->defs[i]->insert(s);
            }
          } else {
            // if (insn->IsLoad() && IsFrameReg(static_cast<Riscv64MemOperand *>(opnd)->GetBaseRegister() ) )
            //  CG_ASSERT(false, "Should not exist non-define use.");
          }
        } else if (opnd->IsList()) {
          if (!(mode & RD_REGANALYSIS)) {
            continue;
          }
          ListOperand *listOpnd = static_cast<ListOperand *>(opnd);
          int j = 0;
          for (auto lst : listOpnd->GetOperands()) {
            itMap = bb->in.find(DataAnalysisInfo(kInfoReg, lst));
            if (itMap != bb->in.end()) {
              CG_ASSERT((i + j) < Insn::kMaxUseOperandNum, "Error, out of range.");
              insn->defs[i + j] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
              for (auto s : itMap->second) {
                insn->defs[i + j]->insert(s);
              }
            } else {
              // if (insn->IsLoad() && IsFrameReg(static_cast<Riscv64MemOperand *>(opnd)->GetBaseRegister() ) )
              //  CG_ASSERT(false, "Should not exist non-define use.");
            }
            j++;
          }
        }
      }
    }

    if (insn->IsCall()) {
      if (mode & RD_REGANALYSIS) {
        // Kill all the caller-saved registers.
        KillAllCallerSavedRegsOnBBIn(bb, insn);

        // Gen return value R0.
        GenReturnValueOnBBIn(bb, insn);
      }
      continue;
    }

    for (int i = 0; i < Insn::kMaxOperandNum; i++) {
      Operand *opnd = insn->opnds[i];
      if (opnd == nullptr) {
        continue;
      }

      Riscv64OpndProp *regprop = static_cast<Riscv64OpndProp *>(md->operand_[i]);

      if (regprop->IsDef()) {
        if (opnd->IsMemoryAccessOperand()) {
          if (!(mode & RD_MEMANALYSIS)) {
            continue;
          }
          Riscv64MemOperand *memopnd = static_cast<Riscv64MemOperand *>(opnd);
          RegOperand *base = memopnd->GetBaseRegister();
          RegOperand *index = memopnd->GetIndexRegister();

          if (base == nullptr) {
            // Do nothing.
          } else {
            if (!(mode & RD_MEMANALYSIS)) {
              continue;
            }

            if (IsFrameReg(base)) {
              if (index) {
                // Currently dirty all of the memory definition.
                // Can do analysis of index register for optimization.
                DirtyAllStackMemOnBBIn(bb);
              } else {
                DefineMemOnBBIn(bb, memopnd, insn, i);
              }
            }
          }
        } else if (opnd->IsConditionCode()) {
          if (mode & RD_REGANALYSIS) {
            DefineRegisterOnBBIn(bb, opnd, insn, i);
          }
        } else if (opnd->IsRegister()) {
          // Register.
          if (mode & RD_REGANALYSIS) {
            if (insn->GetMachineOpcode() == MOP_pseudo_eh_def_x) {
              DefineRegisterOnBBIn(bb, opnd, insn, i, DataInsnInfo::kMaydef);
            } else if (insn->IsPartDef()) {
              DefineRegisterOnBBIn(bb, opnd, insn, i, DataInsnInfo::kPartdef);
            } else {
              DefineRegisterOnBBIn(bb, opnd, insn, i, i ? DataInsnInfo::kSecondMemAddr : DataInsnInfo::kNormal);
            }
          }
        }
      }
    }
  }
}

void Riscv64ReachingDefinition::InsertUseToDef(DataInsnInfo &insnInfo, Insn *useInsn, short index, short userProperty) {
  Insn *defInsn = insnInfo.GetInsn();
  unsigned short defProperty = insnInfo.GetProperty();
  int duIdx = defProperty & DataInsnInfo::kIndexQuickcalc;

  userProperty |= (defProperty & DataInsnInfo::kDataProp);

  if (useInsn->IsCall() && index > 0) {
    CG_ASSERT(index > 0, "Internal error.");

    index--;
    userProperty |= DataInsnInfo::kCallParam;
  }

  if (!defInsn->uses[duIdx]) {
    defInsn->uses[duIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp2>>(rdalloc.Adapter());
  }

  defInsn->uses[duIdx]->insert(DataInsnInfo(useInsn, index, userProperty));

  if (!insnInfo.IsMulGen()) {
    return;
  }

  DataInsnInfo *prevMultiGenInsnInfo = insnInfo.GetPrevMultiGenInsn();
  while (prevMultiGenInsnInfo) {
    defInsn = prevMultiGenInsnInfo->GetInsn();
    duIdx = prevMultiGenInsnInfo->GetProperty() & DataInsnInfo::kIndexQuickcalc;

    if (!defInsn->uses[duIdx]) {
      defInsn->uses[duIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp2>>(rdalloc.Adapter());
    }

    defInsn->uses[duIdx]->insert(DataInsnInfo(useInsn, index, userProperty));

    prevMultiGenInsnInfo = prevMultiGenInsnInfo->GetPrevMultiGenInsn();
  }
}

/*add using DataInsnInfo for defined Operand.*/
void Riscv64ReachingDefinition::GenerateDefUse(BB *bb) {
  FOR_BB_INSNS(insn, bb) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }

    short userProperty = DataInsnInfo::kNormal;
    int memOpndIdx = Insn::kMaxUseOperandNum;

    if (insn->IsLoad()) {
      memOpndIdx = insn->GetResultNum();
    } else if (insn->IsStore()) {
      memOpndIdx = insn->GetOpndNum();
    }

    for (int i = 0; i < Insn::kMaxUseOperandNum; i++) {
      if (insn->defs[i]) {
        int index = i;

        if (i > memOpndIdx) {
          CG_ASSERT((i - memOpndIdx) <= 3, "Internal error.");

          index = memOpndIdx;

          // Set property SECONDMEMADDR or BASEREG or INDEXREG.
          userProperty |= (i - memOpndIdx);
        }

        for (auto defElem : *(insn->defs[i])) {
          InsertUseToDef(defElem, insn, index, userProperty);
        }
      }
    }
  }
}

void Riscv64ReachingDefinition::DumpUDChain(BB *bb) {
  LogInfo::MapleLogger() << "\n    Start to dump U-D chain: " << endl;

  FOR_BB_INSNS(insn, bb) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }

    LogInfo::MapleLogger() << "      U-D chain for insn : ";
    insn->dump();

    MapleList<RegOperand *>::iterator it;
    for (int i = 0; i < Insn::kMaxUseOperandNum; i++) {
      if (!insn->defs[i] || insn->defs[i]->size() == 0) {
        continue;
      }

      if (insn->IsCall() && i > 0) {
        LogInfo::MapleLogger() << "        For operand[" << i << "] : ";
        CG_ASSERT(insn->opnds[1]->IsList(), "Internal error.");
        ListOperand *listOpnd = static_cast<ListOperand *>(insn->opnds[1]);
        if (i == 1) {
          it = listOpnd->GetOperands().begin();
        }
        CG_ASSERT(it != listOpnd->GetOperands().end(), "Internal error.");
        if (it != listOpnd->GetOperands().end()) {
          (*it++)->dump();
        }
      } else {
        if (insn->IsLoad() || insn->IsStore()) {
          int memOpndIdx = Insn::kMaxUseOperandNum;
          if (insn->IsLoad()) {
            memOpndIdx = insn->GetResultNum();
          } else if (insn->IsStore()) {
            memOpndIdx = insn->GetOpndNum();
          }

          if (i <= memOpndIdx) {
            CG_ASSERT(i < Insn::kMaxOperandNum, "Internal error.");
            LogInfo::MapleLogger() << "        For operand[" << i << "] : ";
            insn->opnds[i]->dump();
          } else {
            CG_ASSERT((i - memOpndIdx) <= 3, "Internal error.");
            short property = i - memOpndIdx;

            switch (property) {
              case DataInsnInfo::kNormal: {
                LogInfo::MapleLogger() << "        For first memory address operand : ";
                insn->opnds[memOpndIdx]->dump();
                break;
              }
              case DataInsnInfo::kSecondMemAddr: {
                LogInfo::MapleLogger() << "        For second memory address operand : ";
                insn->opnds[memOpndIdx]->dump();
                break;
              }
              case DataInsnInfo::kBaseReg: {
                CG_ASSERT(insn->opnds[memOpndIdx]->IsMemoryAccessOperand(),
                          "CG internal error, should be memory operand.");
                LogInfo::MapleLogger() << "        For base register of the memory operand : ";
                Riscv64MemOperand *memOpnd = static_cast<Riscv64MemOperand *>(insn->opnds[memOpndIdx]);
                memOpnd->GetBaseRegister()->dump();
                break;
              }
              case DataInsnInfo::kIndexReg: {
                CG_ASSERT(insn->opnds[memOpndIdx]->IsMemoryAccessOperand(),
                          "CG internal error, should be memory operand.");
                LogInfo::MapleLogger() << "        For index register of the memory operand : ";
                Riscv64MemOperand *memOpnd = static_cast<Riscv64MemOperand *>(insn->opnds[memOpndIdx]);
                memOpnd->GetIndexRegister()->dump();
                break;
              }
              default:
                CG_ASSERT(false, "CG internal error.");
            }
          }
        } else {
          CG_ASSERT(i < Insn::kMaxOperandNum, "Internal error.");
          LogInfo::MapleLogger() << "        For operand[" << i << "] : ";
          insn->opnds[i]->dump();
        }
      }

      for (auto insnInfo : *(insn->defs[i])) {
        LogInfo::MapleLogger() << "\n        Define insn: ";
        insnInfo.GetInsn()->dump();
        if (insnInfo.IsMulGen()) {
          DataInsnInfo *tempinfo = insnInfo.GetPrevMultiGenInsn();
          if (!tempinfo) {
            LogInfo::MapleLogger() << "\n-----mulgen but no prevmulgeninsn-----\n";
          } else {
            LogInfo::MapleLogger() << "\n------prev multigen insn:------\n";
            tempinfo->GetInsn()->dump();
          }
        }

        LogInfo::MapleLogger() << "          SecondMemAddr[" << (insnInfo.IsSecondMemAddr() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "BaseReg[" << (insnInfo.IsBaseRegister() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "IndexReg[" << (insnInfo.IsIndexRegister() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "CallParam[" << (insnInfo.IsCallParam() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "Dirty[" << (insnInfo.IsDirty() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "MulGen[" << (insnInfo.IsMulGen() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "MayDef[" << (insnInfo.IsMayDef() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "PartDef[" << (insnInfo.IsPartDef() ? "true" : "false") << "]";
        LogInfo::MapleLogger() << endl;
      }
    }
  }
}

void Riscv64ReachingDefinition::DumpDUChain(BB *bb) {
  LogInfo::MapleLogger() << "\n    Start to dump D-U chain: " << endl;

  FOR_BB_INSNS(insn, bb) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }

    LogInfo::MapleLogger() << "      D-U chain for insn : ";
    insn->dump();

    MapleList<RegOperand *>::iterator it;
    for (int i = 0; i < Insn::kMaxDefineOperandNum; i++) {
      if (!insn->uses[i] || insn->uses[i]->size() == 0) {
        continue;
      }

      if (insn->IsCall()) {
        LogInfo::MapleLogger() << "        For return value R0 : ";

        Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);
        RegOperand *phyopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand((Riscv64reg_t)R0, 64, kRegTyInt);
        phyopnd->dump();
      } else {
        LogInfo::MapleLogger() << "        For " << i << "th operand : ";
        insn->opnds[i]->dump();
      }

      for (auto insnInfo : *(insn->uses[i])) {
        LogInfo::MapleLogger() << "\n        Use insn: ";
        insnInfo.GetInsn()->dump();
        LogInfo::MapleLogger() << "          SecondMemAddr[" << (insnInfo.IsSecondMemAddr() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "BaseReg[" << (insnInfo.IsBaseRegister() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "IndexReg[" << (insnInfo.IsIndexRegister() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "CallParam[" << (insnInfo.IsCallParam() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "Dirty[" << (insnInfo.IsDirty() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "MulGen[" << (insnInfo.IsMulGen() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "MayDef[" << (insnInfo.IsMayDef() ? "true" : "false") << "], ";
        LogInfo::MapleLogger() << "PartDef[" << (insnInfo.IsPartDef() ? "true" : "false") << "]";
        LogInfo::MapleLogger() << endl;
      }
    }
  }
}

void Riscv64ReachingDefinition::DumpWAWDependence(BB *bb) {
  LogInfo::MapleLogger() << "\n    WAW dependence: " << endl;

  FOR_BB_INSNS(insn, bb) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }

    LogInfo::MapleLogger() << "\n      WAW dependence for insn : ";
    insn->dump();

    MapleList<RegOperand *>::iterator it;
    for (int i = 0; i < Insn::kMaxDefineOperandNum; i++) {
      LogInfo::MapleLogger() << "        For " << i << "th destination operand : ";
      if (insn->redefine[i] && insn->redefine[i]->size()) {
        for (auto redefInsn : *(insn->redefine[i])) {
          LogInfo::MapleLogger() << "\n        Redefine insn: \n          ";
          redefInsn.GetInsn()->dump();
        }
      }

      if (insn->predefine[i] && insn->predefine[i]->size()) {
        for (auto predefInsn : *(insn->predefine[i])) {
          LogInfo::MapleLogger() << "\n        Previous define insn: \n          ";
          predefInsn.GetInsn()->dump();
        }
      }
    }
  }
}

/* Remove insn and maintain data flow information.
   Need to handle property changes. such as dirty, multidef, mayDef.
 */
void Riscv64ReachingDefinition::RemoveDUUDForInsn(Insn *insn) {
  CG_ASSERT(insn, "CG internal error, insn should not be nullptr.");

  // For each uses of insn, remove D-U.
  for (int i = 0; i < Insn::kMaxUseOperandNum; i++) {
    if (insn->defs[i] == nullptr) {
      continue;
    }

    for (auto defInsnInfo : *(insn->defs[i])) {
      Insn *defInsn = defInsnInfo.GetInsn();
      int idx = defInsnInfo.GetDUIndex();

      CG_ASSERT(defInsn->uses[idx], "CG internal error.");
      // property  was used in the compare functor of set<DataInsnInfo>, so property can't be default
      if (insn->IsCall() && i > 0) {
        defInsn->uses[idx]->erase(DataInsnInfo(insn, i - 1, DataInsnInfo::kCallParam));
      } else {
        defInsn->uses[idx]->erase(DataInsnInfo(insn, i));
      }
    }

    insn->defs[i] = nullptr;
  }

  // For each definition operand of insn, rebuild links between it's uses and it's predefine operands.
  for (int i = 0; i < Insn::kMaxDefineOperandNum; i++) {
    if (insn->uses[i] != nullptr) {
      // If insn has predefine insn, then build duud between predefine insn and use insn.
      if (insn->predefine[i] != nullptr) {
        for (auto predefInsnInfo : *(insn->predefine[i])) {
          Insn *predefInsn = predefInsnInfo.GetInsn();
          int predefIdx = predefInsnInfo.GetDUIndex();

          CG_ASSERT(predefInsn->redefine[predefIdx], "CG internal error.");

          for (auto useInsnInfo : *(insn->uses[i])) {
            Insn *useInsn = useInsnInfo.GetInsn();
            int useInsnIdx = useInsnInfo.GetUDIndex();

            // Add def->use between predefInsn and useInsn.
            if (predefInsn->uses[predefIdx] == nullptr) {
              predefInsn->uses[predefIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp2>>(rdalloc.Adapter());
            }

            predefInsn->uses[predefIdx]->insert(useInsnInfo);

            // Add use-def between useInsn and predefInsn
            CG_ASSERT(useInsn->defs[useInsnIdx], "CG internal error.");
            useInsn->defs[useInsnIdx]->insert(predefInsnInfo);
          }
        }
      }

      // Remove use->define between useInsn and insn.
      for (auto useInsnInfo : *(insn->uses[i])) {
        Insn *useInsn = useInsnInfo.GetInsn();
        int useInsnIdx = useInsnInfo.GetUDIndex();

        CG_ASSERT(useInsn->defs[useInsnIdx], "CG internal error.");
        useInsn->defs[useInsnIdx]->erase(DataInsnInfo(insn, 0));
      }

      // Remove def->use from insn to it's uses.
      insn->uses[i]->clear();
      insn->uses[i] = nullptr;
    }
  }

  // Rebuild redefine.
  for (int i = 0; i < Insn::kMaxDefineOperandNum; i++) {
    if (insn->predefine[i] != nullptr) {
      for (auto predefInsnInfo : *(insn->predefine[i])) {
        Insn *predefInsn = predefInsnInfo.GetInsn();
        int idx = predefInsnInfo.GetDUIndex();

        CG_ASSERT(predefInsn->redefine[idx], "CG internal error.");

        predefInsn->redefine[idx]->erase(DataInsnInfo(insn, 0));

        if (insn->redefine[i]) {
          for (auto redefInsnInfo : *(insn->redefine[i])) {
            // if redefine insn is same as insn, don't need to build relation between predefine insn and define insn
            if (redefInsnInfo.GetInsn() == insn) {
              continue;
            }

            predefInsn->redefine[idx]->insert(redefInsnInfo);
            Insn *redefInsn = redefInsnInfo.GetInsn();

            // Call insn do not have predefine.
            if (!redefInsn->IsCall()) {
              int redefIdx = redefInsnInfo.GetDUIndex();

              CG_ASSERT(redefInsn->predefine[redefIdx], "CG internal error.");

              redefInsn->predefine[redefIdx]->insert(predefInsnInfo);
            }
          }
        }
      }
    }

    if (insn->redefine[i]) {
      for (auto redefInsnInfo : *(insn->redefine[i])) {
        Insn *redefInsn = redefInsnInfo.GetInsn();

        // Call insn do not have predefine.
        if (!redefInsn->IsCall()) {
          int redefIdx = redefInsnInfo.GetDUIndex();

          CG_ASSERT(redefInsn->predefine[redefIdx], "CG internal error.");

          redefInsn->predefine[redefIdx]->erase(DataInsnInfo(insn, 0));
        }
      }
    }
  }
}

/* Uses: After insert an instruction, build data flow insns for one source operand.
   Params: 1) new_insn: The insn inserted.
           2) new_index: The index of the destination operand.
           3) new_prop: Indicate the kind of the destination operand. (NORMAL / SECONDDEF / BASEREG / INDEXREG /
   CALLPARAM) 4) ref_insn: The reference instruction with the same destination operand. nullptr if the operand is a new
   operand without predefine/redefine/uses. 5) ref_index: The index of the destination operand. 6) ref_prop: Indicate
   the kind of the destination operand.  (NORMAL / SECONDDEF / BASEREG / INDEXREG / CALLPARAM)
 */
void Riscv64ReachingDefinition::InsertDUUDForSrcOperand(Insn *newInsn, short newIndex, unsigned short newProp,
                                                        Insn *refInsn, short refIndex, unsigned short refProp) {
  CG_ASSERT(newInsn, "CG internal error, new_insn should not be nullptr.");

  if (refInsn == nullptr) {
    LogInfo::MapleLogger() << "CG Warning, use without def.  " << cgfunc->GetName() << std::endl;
    return;
  }

  int refInsnUDIdx = refProp & DataInsnInfo::kIndexQuickcalc;

  CG_ASSERT(refInsnUDIdx != DataInsnInfo::kIndexQuickcalc, "CG internal error.");
  CG_ASSERT(!(refInsnUDIdx && (refProp & DataInsnInfo::kCallParam)),
            "CG internal error, can not with both kIndexQuickcalc and CALLPARAM property.");

  refInsnUDIdx = refIndex + refInsnUDIdx;
  // If is useInsn with CALLPARAM property, index indicates the ith parameter.
  refInsnUDIdx = (refProp & DataInsnInfo::kCallParam) ? refInsnUDIdx + 1 : refInsnUDIdx;

  int newInsnUDIdx = newProp & DataInsnInfo::kIndexQuickcalc;

  CG_ASSERT(newInsnUDIdx != DataInsnInfo::kIndexQuickcalc, "CG internal error.");
  CG_ASSERT(!(newInsnUDIdx && (newProp & DataInsnInfo::kCallParam)),
            "CG internal error, can not with both kIndexQuickcalc and CALLPARAM property.");

  newInsnUDIdx = newIndex + newInsnUDIdx;
  // If is useInsn with CALLPARAM property, index indicates the ith parameter.
  newInsnUDIdx = (newProp & DataInsnInfo::kCallParam) ? newInsnUDIdx + 1 : newInsnUDIdx;

  if (refInsn->defs[refInsnUDIdx]) {
    if (newInsn->defs[newInsnUDIdx] == nullptr) {
      newInsn->defs[newInsnUDIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
    }
    for (auto defInsnInfo : (*refInsn->defs[refInsnUDIdx])) {
      // Build use->def from new_insn to desfInsn.
      newInsn->defs[newInsnUDIdx]->insert(defInsnInfo);

      // Build def->use from defInsn to new_insn.
      Insn *defInsn = defInsnInfo.GetInsn();
      int defInsnDUIdx = defInsnInfo.GetDUIndex();

      CG_ASSERT(defInsn->uses[defInsnDUIdx] != nullptr, "CG internal error, defInsn should have uses.");
      unsigned short newInsnProp = newProp | (defInsnInfo.GetProperty() & DataInsnInfo::kDataProp);
      defInsn->uses[defInsnDUIdx]->insert(DataInsnInfo(newInsn, newIndex, newInsnProp));
    }
  } else {
    LogInfo::MapleLogger() << "CG Warning, use without def.  " << cgfunc->GetName() << std::endl;
    return;
  }
}

/* Uses: After insert an instruction, build data flow insns for one destination operand.
   Params: 1) new_insn: The insn inserted.
           2) new_index: The index of the destination operand.
           3) new_prop: Indicate the kind of the destination operand. (NORMAL / SECONDDEF / BASEREG)
           4) ref_insn: The reference instruction with the same destination operand. ref_insn should not be a call insn.
                        nullptr if the operand is a new operand without predefine/redefine/uses.
           5) ref_index: The index of the destination operand.
           6) ref_prop: Indicate the kind of the destination operand.  (NORMAL / SECONDDEF / BASEREG)
           7) isRefInsnBeforeNewInsn: true if new_insn is inserted just after ref_insn, otherwise false.
 */
void Riscv64ReachingDefinition::InsertDUUDForDestOperand(Insn *newInsn, short newIndex, unsigned short newProp,
                                                         Insn *refInsn, short refIndex, unsigned short refProp,
                                                         bool isRefInsnBeforeNewInsn) {
  MapleSet<DataInsnInfo, DataInsnInfoCmp>::iterator itSet;
  MapleSet<DataInsnInfo, DataInsnInfoCmp2>::iterator itSet2;

  CG_ASSERT(newInsn, "CG internal error, new_insn should not be nullptr.");

  if (refInsn == nullptr) {
    return;
  }

  int refDuIdx = refIndex + (refProp & DataInsnInfo::kIndexQuickcalc);
  int newDuIdx = newIndex + (newProp & DataInsnInfo::kIndexQuickcalc);
  if (isRefInsnBeforeNewInsn) {
    // new_insn is just after ref_insn.
    if (refInsn->uses[refDuIdx] != nullptr) {
      if (newInsn->uses[newDuIdx] == nullptr) {
        newInsn->uses[newDuIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp2>>(rdalloc.Adapter());
      }
      // Add Def->Use and Use->Def between new_insn and ref_insn's uses.
      // Remove Def->Use and Use->Def between ref_insn and it's uses.
      for (itSet2 = refInsn->uses[refDuIdx]->begin(); itSet2 != refInsn->uses[refDuIdx]->end();) {
        const DataInsnInfo &useInsnInfo = (*itSet2);
        Insn *useInsn = useInsnInfo.GetInsn();
        int useInsnIdx = useInsnInfo.GetUDIndex();

        CG_ASSERT(useInsn->defs[useInsnIdx], "CG internal error.");
        // find ref_insn in useInsn's defs and change ref_insn to new insn.
        itSet = useInsn->defs[useInsnIdx]->find(DataInsnInfo(refInsn, DataInsnInfo::kAnyIndex));
        CG_ASSERT(itSet != useInsn->defs[useInsnIdx]->end(), "CG internal error, Use should have Def.");
        const DataInsnInfo &refInsnInfo = (*itSet);
        DataInsnInfo newInsnInfo(newInsn, newIndex, newProp);
        newInsnInfo.SetProperty(refInsnInfo.GetProperty() &
                                (DataInsnInfo::kDirty | DataInsnInfo::kMultigen | DataInsnInfo::kMaydef));
        newInsnInfo.SetPrevMultiGenInsn(refInsnInfo.GetPrevMultiGenInsn());

        if (useInsn != newInsn) {
          // Remove use->def between useInsn and ref_insn, and build use->def between useInsn and new_insn.
          useInsn->defs[useInsnIdx]->erase(itSet);
          useInsn->defs[useInsnIdx]->insert(newInsnInfo);

          // Build def->use between new_insn and useInsn.
          newInsn->uses[newDuIdx]->insert(useInsnInfo);

          // Remove ref_insn's def->use.
          refInsn->uses[refDuIdx]->erase(itSet2++);
        } else {
          itSet2++;
        }
      }

      if (refInsn->uses[refDuIdx]->empty()) {
        refInsn->uses[refDuIdx] = nullptr;
      }
    }

    if (refInsn->redefine[refDuIdx]) {
      if (newInsn->redefine[newDuIdx] == nullptr) {
        newInsn->redefine[newDuIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
      }

      for (auto redefInsnInfo : *(refInsn->redefine[refDuIdx])) {
        // 1) Build redefine between new_insn and redefineInsns of ref_insn.
        newInsn->redefine[newDuIdx]->insert(redefInsnInfo);

        // 2) Build predefine between redefineInsns of ref_insn and new_insn.
        Insn *redefInsn = redefInsnInfo.GetInsn();
        int redefInsnIdx = redefInsnInfo.GetDUIndex();

        // Call insn do not have predefine.
        if (!redefInsn->IsCall()) {
          CG_ASSERT(redefInsn->predefine[redefInsnIdx], "CG internal error.");
          // find ref_insn in redefInsn's predefine and change ref_insn to new insn.
          itSet = redefInsn->predefine[redefInsnIdx]->find(DataInsnInfo(refInsn, DataInsnInfo::kAnyIndex));
          CG_ASSERT(itSet != redefInsn->predefine[redefInsnIdx]->end(),
                    "CG internal error, redefInsn should have predef.");

          redefInsn->predefine[redefInsnIdx]->insert(DataInsnInfo(newInsn, newIndex, newProp));

          // 3) Remove predefine between redefineInsns of ref_insn and ref_insn.
          redefInsn->predefine[redefInsnIdx]->erase(itSet);
        }
      }

      // 4) Remove redefine between ref_insn and redefineInsns of ref_insn.
      refInsn->redefine[refDuIdx]->clear();
    } else {
      refInsn->redefine[refDuIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
    }

    // Build ref_insn's redefine between ref_insn and new_insn.
    refInsn->redefine[refDuIdx]->insert(DataInsnInfo(newInsn, newIndex, newProp));

    // Build new_insn's predefine between new_insn and ref_insn.
    if (newInsn->predefine[newDuIdx] == nullptr) {
      newInsn->predefine[newDuIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
    }
    newInsn->predefine[newDuIdx]->insert(DataInsnInfo(refInsn, refIndex, refProp));
  } else {
    // new_insn is just before ref_insn.
    if (refInsn->predefine[refDuIdx]) {
      // new_insn should have predefine too.
      if (newInsn->predefine[newDuIdx] == nullptr) {
        newInsn->predefine[newDuIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
      }
      for (auto predefInsnInfo : *(refInsn->predefine[refDuIdx])) {
        Insn *predefInsn = predefInsnInfo.GetInsn();
        int predefInsnIdx = predefInsnInfo.GetDUIndex();
        // Check if there is a new_insn's def->use is ref_insn's source operands.
        if (predefInsn->uses[predefInsnIdx]) {
          bool find = false;
          do {
            // ref_insn may have multiple same use operands.
            itSet2 = predefInsn->uses[predefInsnIdx]->find(DataInsnInfo(refInsn, DataInsnInfo::kAnyIndex));
            if (itSet2 != predefInsn->uses[predefInsnIdx]->end()) {
              find = true;
              // Find it. Then:
              // 1) Build a new def->use between new_insn and ref_insn.
              if (newInsn->uses[newDuIdx] == nullptr) {
                newInsn->uses[newDuIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp2>>(rdalloc.Adapter());
              }
              DataInsnInfo useInsnInfo = (*itSet2);
              useInsnInfo.ClearProperty(DataInsnInfo::kDirty | DataInsnInfo::kMultigen);
              newInsn->uses[newDuIdx]->insert(useInsnInfo);

              // 2) Build a new use->def between ref_insn and new_insn.
              Insn *useInsn = useInsnInfo.GetInsn();
              int useInsnIdx = useInsnInfo.GetUDIndex();

              CG_ASSERT(useInsn->defs[useInsnIdx], "CG internal error.");

              useInsn->defs[useInsnIdx]->insert(DataInsnInfo(newInsn, newIndex, newProp));

              // 3) Remove use->def between ref_insn and predefInsn.
              // find ref_insn in predefInsn's uses and change ref_insn to new insn.
              itSet = useInsn->defs[useInsnIdx]->find(DataInsnInfo(predefInsn, DataInsnInfo::kAnyIndex));
              CG_ASSERT(itSet != useInsn->defs[useInsnIdx]->end(), "CG internal error, Use should have Def.");
              useInsn->defs[useInsnIdx]->erase(itSet);

              // 4) Remove def->use between predefInsn and ref_insn.
              predefInsn->uses[predefInsnIdx]->erase(itSet2);
            }
          } while (find);

          if (predefInsn->uses[predefInsnIdx]->empty()) {
            predefInsn->uses[predefInsnIdx] = nullptr;
          }
        }

        // Build predefine from new_insn to predefInsn.
        newInsn->predefine[newDuIdx]->insert(predefInsnInfo);

        CG_ASSERT(predefInsn->redefine[predefInsnIdx], "CG internal error. predefInsn should have redefine data.");

        // Build redefine from predefInsn to new_insn.
        predefInsn->redefine[predefInsnIdx]->insert(DataInsnInfo(newInsn, newIndex, newProp));

        // Remove redefine from predefInsn to ref_insn.
        itSet = predefInsn->redefine[predefInsnIdx]->find(DataInsnInfo(refInsn, DataInsnInfo::kAnyIndex));
        CG_ASSERT(itSet != predefInsn->redefine[predefInsnIdx]->end(),
                  "CG internal error. predefInsn should find ref_insn.");

        if (itSet != predefInsn->redefine[predefInsnIdx]->end()) {
          predefInsn->redefine[predefInsnIdx]->erase(itSet);
        }
      }

      // Remove all predefine of ref_insn.
      refInsn->predefine[refDuIdx]->clear();
    } else {
      // ref_insn do not have predefine.
      refInsn->predefine[refDuIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
    }

    // Call insn do not have predefine.
    if (refInsn->IsCall()) {
      CG_ASSERT(false, "CG internal error, ref_insn should not be a call insn here.");
    }

    // Build predefine frome ref_insn to new_insn.
    refInsn->predefine[refDuIdx]->insert(DataInsnInfo(newInsn, newIndex, newProp));

    // Build redefine from new_insn to ref_insn.
    if (!newInsn->redefine[newDuIdx]) {
      newInsn->redefine[newDuIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
    }
    newInsn->redefine[newDuIdx]->insert(DataInsnInfo(refInsn, refIndex, refProp));
  }
}

/* Uses: Replace one of the insn operand, with maintaining data flow info.
   Params: 1) insn: The replace insn.
           2) index: The index of the src operand that will replaced.
           3) prop: Indicate the kind of the replaced operand. (NORMAL / BASEREG / INDEXREG)
                    No need to replace CALLPARAM since they are physical registers.
           4) new_opnd: The new src operand of the insn.
           4) ref_insn: The reference instruction with the same new_opnd.
                        nullptr if the new src operand without defs.
           5) ref_index: The operand index of the ref_insn that with the same data flow info.
           6) ref_prop: Indicate the kind of the destination operand.  (NORMAL / SECONDDEF / BASEREG / INDEXREG /
   CALLPARAM)
 */
void Riscv64ReachingDefinition::ReplaceInsnSrcOperand(Insn *insn, short index, unsigned short prop, Operand *newOpnd,
                                                      Insn *refInsn, short refIndex, unsigned short refProp) {
  MapleSet<DataInsnInfo, DataInsnInfoCmp2>::iterator itSet2;

  CG_ASSERT(insn, "CG internal error, insn should not be nullptr.");

  int udIdx = index + (prop & DataInsnInfo::kIndexQuickcalc);
  int refUdIdx = refIndex + (refProp & DataInsnInfo::kIndexQuickcalc) + ((refProp & DataInsnInfo::kCallParam) ? 1 : 0);

  if (insn->defs[udIdx]) {
    // Remove old def->use.
    for (auto udInsnInfo : *(insn->defs[udIdx])) {
      Insn *defInsn = udInsnInfo.GetInsn();
      int duIdx = udInsnInfo.GetDUIndex();

      CG_ASSERT(defInsn->uses[duIdx], "CG internal error, defInsn should have uses.");

      itSet2 = defInsn->uses[duIdx]->find(DataInsnInfo(insn, index, prop & DataInsnInfo::kCallParam));
      CG_ASSERT(itSet2 != defInsn->uses[duIdx]->end(), "CG internal error, defInsn should have insn use.");
      if (itSet2 != defInsn->uses[duIdx]->end()) {
        defInsn->uses[duIdx]->erase(itSet2);
        if (defInsn->uses[duIdx]->empty()) {
          defInsn->uses[duIdx] = nullptr;
        }
      }
    }

    // Remove all use->def.
    insn->defs[udIdx]->clear();
  }

  if (refInsn != nullptr && refInsn->defs[refUdIdx] != nullptr) {
    if (insn->defs[udIdx] == nullptr) {
      insn->defs[udIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
    }

    for (auto udInsnInfo : *(refInsn->defs[refUdIdx])) {
      Insn *defInsn = udInsnInfo.GetInsn();
      int duIdx = udInsnInfo.GetDUIndex();

      CG_ASSERT(defInsn->uses[duIdx], "CG internal error, defInsn should have uses.");

      itSet2 = defInsn->uses[duIdx]->find(DataInsnInfo(refInsn, refIndex, refProp & DataInsnInfo::kCallParam));
      CG_ASSERT(itSet2 != defInsn->uses[duIdx]->end(), "CG internal error, defInsn should have insn use.");
      if (itSet2 != defInsn->uses[duIdx]->end()) {
        // Build def->use from defInsn to insn.
        unsigned short dataProp = itSet2->GetProperty() & DataInsnInfo::kDataProp;
        defInsn->uses[duIdx]->insert(DataInsnInfo(insn, index, prop | dataProp));

        // Build use->def from insn to defInsn.
        insn->defs[udIdx]->insert(udInsnInfo);
      }
    }
  }

  if (insn->defs[udIdx] && insn->defs[udIdx]->empty()) {
    insn->defs[udIdx] = nullptr;
  }

  switch (prop & DataInsnInfo::kAnyIndex) {
    case DataInsnInfo::kNormal:
      insn->opnds[index] = newOpnd;
      break;
    case DataInsnInfo::kSecondMemAddr:
      CG_ASSERT(false, "CG internal error, Could not replace the second memory address.");
      break;
    case DataInsnInfo::kBaseReg: {
      CG_ASSERT(newOpnd->IsRegister(), "CG Internal error, new_opnd should be a register operand.");
      CG_ASSERT(insn->opnds[index]->IsMemoryAccessOperand(),
                "CG Internal error, insn->opnds[index] should be a memory operand.");

      MemOperand *memOpnd =
        static_cast<MemOperand *>(static_cast<MemOperand *>(insn->opnds[index])->Clone(cgfunc->memPool));
      insn->SetOperand(index, memOpnd);
      memOpnd->SetBaseRegister(static_cast<RegOperand *>(newOpnd));
      break;
    }
    case DataInsnInfo::kIndexReg: {
      CG_ASSERT(newOpnd->IsRegister(), "CG Internal error, new_opnd should be a register operand.");
      CG_ASSERT(insn->opnds[index]->IsMemoryAccessOperand(),
                "CG Internal error, insn->opnds[index] should be a memory operand.");

      MemOperand *memOpnd =
        static_cast<MemOperand *>(static_cast<MemOperand *>(insn->opnds[index])->Clone(cgfunc->memPool));
      CG_ASSERT(memOpnd != nullptr, "memOpnd is null in Riscv64CGFunc::ReplaceInsnSrcOperand");
      insn->SetOperand(index, memOpnd);
      memOpnd->SetIndexRegister(static_cast<RegOperand *>(newOpnd));
      break;
    }
    case DataInsnInfo::kCallParam:
      CG_ASSERT(false, "CG internal error, we don't need to replace call params.");
      break;
    default:
      CG_ASSERT(false, "CG invalid property.");
      break;
  }

  if (insn->defs[udIdx]->empty()) {
    insn->defs[udIdx] = nullptr;
  }
}

/* Uses: Replace one of the insn operand, with maintaining data flow info.
   Params: 1) insn: The replace insn.
           2) index: The index of the dest operand that will replaced.
           3) prop: Indicate the kind of the replaced operand. (NORMAL / BASEREG)
                    No need to replace CALLPARAM / INDEXREG since they won't be dest operands.
           4) new_opnd: The new src operand of the insn.
           4) ref_insn: The reference instruction with the same new_opnd.
                        nullptr if the new dest operand without predefine/redefine/uses.
           5) ref_index: The operand index of the ref_insn that with the same data flow info.
           6) ref_prop: Indicate the kind of the destination operand.  (NORMAL / SECONDMEMADDR / BASEREG)
           7) isRefInsnBeforeInsn: true if ref_insn is before insn. otherwise false.
 */
void Riscv64ReachingDefinition::ReplaceInsnDestOperand(Insn *insn, short index, unsigned short prop, Operand *newOpnd,
                                                       Insn *refInsn, short refIndex, unsigned short refProp,
                                                       bool isRefInsnBeforeInsn) {
  MapleSet<DataInsnInfo, DataInsnInfoCmp>::iterator itSet;
  MapleSet<DataInsnInfo, DataInsnInfoCmp2>::iterator itSet2;

  CG_ASSERT(insn, "CG internal error, insn should not be nullptr.");

  int duIdx = index + (prop & DataInsnInfo::kIndexQuickcalc);

  // For the definition operand of insn, rebuild links between it's uses and it's predefine operands.
  if (insn->uses[duIdx] != nullptr) {
    // If insn has predefine insn, then build duud between insn->predefine and insn->uses.
    if (insn->predefine[duIdx] != nullptr) {
      for (auto predefInsnInfo : *(insn->predefine[duIdx])) {
        Insn *predefInsn = predefInsnInfo.GetInsn();
        int predefIdx = predefInsnInfo.GetDUIndex();

        CG_ASSERT(predefInsn->redefine[predefIdx], "CG internal error.");

        for (auto useInsnInfo : *(insn->uses[duIdx])) {
          Insn *useInsn = useInsnInfo.GetInsn();
          int useInsnIdx = useInsnInfo.GetUDIndex();

          // Add def->use between predefInsn and useInsn.
          if (predefInsn->uses[predefIdx] == nullptr) {
            predefInsn->uses[predefIdx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp2>>(rdalloc.Adapter());
          }

          predefInsn->uses[predefIdx]->insert(useInsnInfo);

          // Add use-def between useInsn and predefInsn
          CG_ASSERT(useInsn->defs[useInsnIdx], "CG internal error.");
          useInsn->defs[useInsnIdx]->insert(predefInsnInfo);
        }
      }
    }

    // Remove use->define between useInsn and insn.
    for (auto useInsnInfo : *(insn->uses[duIdx])) {
      Insn *useInsn = useInsnInfo.GetInsn();
      int useInsnIdx = useInsnInfo.GetUDIndex();

      CG_ASSERT(useInsn->defs[useInsnIdx], "CG internal error.");
      useInsn->defs[useInsnIdx]->erase(DataInsnInfo(insn, 0));
    }

    // Remove def->use from insn to it's uses.
    insn->uses[duIdx]->clear();
    insn->uses[duIdx] = nullptr;
  }

  // Rebuild redefine.
  if (insn->predefine[duIdx] != nullptr) {
    for (auto predefInsnInfo : *(insn->predefine[duIdx])) {
      Insn *predefInsn = predefInsnInfo.GetInsn();
      int idx = predefInsnInfo.GetDUIndex();

      CG_ASSERT(predefInsn->redefine[idx], "CG internal error.");

      predefInsn->redefine[idx]->erase(DataInsnInfo(insn, 0));

      if (insn->redefine[duIdx]) {
        for (auto redefInsnInfo : *(insn->redefine[duIdx])) {
          predefInsn->redefine[idx]->insert(redefInsnInfo);
          Insn *redefInsn = redefInsnInfo.GetInsn();

          // Call insn do not have predefine.
          if (!redefInsn->IsCall()) {
            int redefIdx = redefInsnInfo.GetDUIndex();

            CG_ASSERT(redefInsn->predefine[redefIdx], "CG internal error.");

            redefInsn->predefine[redefIdx]->insert(predefInsnInfo);
          }
        }
      }
    }
  }

  // Remove predefine from redefInsn to insn.
  if (insn->redefine[duIdx]) {
    for (auto redefInsnInfo : *(insn->redefine[duIdx])) {
      Insn *redefInsn = redefInsnInfo.GetInsn();

      // Call insn do not have predefine.
      if (!redefInsn->IsCall()) {
        int redefIdx = redefInsnInfo.GetDUIndex();

        CG_ASSERT(redefInsn->predefine[redefIdx], "CG internal error.");

        redefInsn->predefine[redefIdx]->erase(DataInsnInfo(insn, 0));
      }
    }
  }

  // Insert destination operand data flow info.
  InsertDUUDForDestOperand(insn, index, prop, refInsn, refIndex, refProp, isRefInsnBeforeInsn);

  // Replace operand.
  switch (prop & DataInsnInfo::kAnyIndex) {
    case DataInsnInfo::kNormal:
      insn->opnds[index] = newOpnd;
      break;
    case DataInsnInfo::kSecondMemAddr:
      CG_ASSERT(false, "CG internal error, Could not replace the second memory address.");
      break;
    case DataInsnInfo::kBaseReg: {
      CG_ASSERT(newOpnd->IsRegister(), "CG Internal error, new_opnd should be a register operand.");
      CG_ASSERT(insn->opnds[index]->IsMemoryAccessOperand(),
                "CG Internal error, insn->opnds[index] should be a memory operand.");

      MemOperand *memOpnd =
        static_cast<MemOperand *>(static_cast<MemOperand *>(insn->opnds[index])->Clone(cgfunc->memPool));
      CG_ASSERT(memOpnd != nullptr, "memOpnd is null in Riscv64CGFunc::ReplaceInsnDestOperand");
      insn->SetOperand(index, memOpnd);
      if (prop & DataInsnInfo::kBaseReg) {
        memOpnd->SetBaseRegister(static_cast<RegOperand *>(newOpnd));
      } else {
        memOpnd->SetIndexRegister(static_cast<RegOperand *>(newOpnd));
      }

      break;
    }
    case DataInsnInfo::kIndexReg:
      CG_ASSERT(false, "CG internal error, index register of memory operand should not be a dest operand.");
      break;
    case DataInsnInfo::kCallParam:
      CG_ASSERT(false, "CG internal error, we don't need to replace call params.");
      break;
    default:
      CG_ASSERT(false, "CG invalid property.");
      break;
  }
}

}  // namespace maplebe
