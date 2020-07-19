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

#include "aarch64_live_analysis.h"
#include "aarch64_cg.h"
#include "cg_assert.h"

namespace maplebe {

void AArch64LiveAnalysis::InitEhDefine(BB *bb) {
  AArch64CGFunc *aarchcgfunc = static_cast<AArch64CGFunc *>(cgfunc_);

  // Insert MOP_pseudo_eh_def_x R1.
  RegOperand *regopnd = aarchcgfunc->GetOrCreatePhysicalRegisterOperand(R1, 64, kRegTyInt);
  Insn *pseudoInsn = cgfunc_->cg->BuildInstruction<AArch64Insn>(MOP_pseudo_eh_def_x, regopnd);
  bb->InsertInsnBegin(pseudoInsn);

  // Insert MOP_pseudo_eh_def_x R0.
  regopnd = aarchcgfunc->GetOrCreatePhysicalRegisterOperand(R0, 64, kRegTyInt);
  pseudoInsn = cgfunc_->cg->BuildInstruction<AArch64Insn>(MOP_pseudo_eh_def_x, regopnd);
  bb->InsertInsnBegin(pseudoInsn);
}

regno_t AArch64LiveAnalysis::UpdateRegNum(RegOperand *opnd) {
  // upper 8 bits are encoded bit vector bit position
  // See cg_bb.h for comparison function for std::set
  regno_t rno = opnd->GetRegisterNumber();
  if (CGOptions::lsraSimdMode == 2) {
    return rno;
  }
  AArch64RegOperand *aopnd = dynamic_cast<AArch64RegOperand *>(opnd);
  if (aopnd && aopnd->IsSimdVectorMode()) {
    VectorType type = aopnd->GetSimdVectorType();
    if (type == kVecSingle) {
      switch (aopnd->GetSimdVectorPosition()) {
        case 0:
          rno |= LIVE_POS_0;
          break;
        case 1:
          rno |= LIVE_POS_1;
          break;
        case 2:
          rno |= LIVE_POS_2;
          break;
        case 3:
          rno |= LIVE_POS_3;
          break;
        default:
          CG_ASSERT(0, "AArch64LiveAnalysis::UpdateRegNum Error");
      }
    } else {
      // VEC_DOUBLE
      switch (aopnd->GetSimdVectorPosition()) {
        case 0:
          rno |= LIVE_POS_0_1;
          break;
        case 1:
          rno |= LIVE_POS_2_3;
          break;
        default:
          CG_ASSERT(0, "AArch64LiveAnalysis::UpdateRegNum Error");
      }
    }
  }
  return rno;
}

/* build use and def sets of each BB according to the type of regopnd. */
void AArch64LiveAnalysis::CollectLiveInfo(BB *bb, Operand *opnd, bool isDef, bool isUse) {
  if (!opnd || !opnd->IsRegister()) {
    return;
  }
  RegOperand *regopnd = static_cast<RegOperand *>(opnd);
  regno_t rno = UpdateRegNum(regopnd);
  RegType regtype = regopnd->GetRegisterType();
  if (regtype == kRegTyVary) {
    return;
  }
  if (isDef) {
    bb->def_regno.push_back(rno);
    if (!isUse) {
      bb->EraseUseRegnoFromVector(rno);
    }
  }
  if (isUse) {
    bb->use_regno.push_back(rno);
    bb->EraseDefRegnoFromVector(rno);
  }
}

BB* AArch64LiveAnalysis::CreateSubBB(BB *bb) {
  subBBId++;
  BB *newSubBB = memPool->New<BB>(subBBId, bb->mallocator);
  (*subBB)[bb->id].push_back(newSubBB);
  return newSubBB;
}

bool AArch64LiveAnalysis::CanInsnThrow(Insn *insn) {
  if (insn->CanThrow()) {
    if (insn->IsMemAccess()) {
      Operand *opnd = insn->GetMemOpnd();
      if (opnd->IsMemoryAccessOperand()) {
        MemOperand *memopnd = static_cast<MemOperand *>(opnd);
        Operand *base = memopnd->GetBaseRegister();
        if (base) {
          RegOperand *ropnd = static_cast<RegOperand *>(base);
          if (!ropnd->IsPhysicalRegister() ||
              ((ropnd->GetRegisterNumber() != RFP && ropnd->GetRegisterNumber() != RSP))) {
            return true;
          }
        }
      }
    } else {
      return true;
    }
  }
  return false;
}

void AArch64LiveAnalysis::GetInsnDefUse(BB *bb, Insn *insn) {
  if (insn->IsCall()) {
    for (uint32 i = 0; i < 8; i++) {
      Operand *phyopnd = static_cast<AArch64CGFunc *>(cgfunc_)->GetOrCreatePhysicalRegisterOperand(
        (AArch64reg_t)(R0 + i), 64, kRegTyInt);
      CollectLiveInfo(bb, phyopnd, true, false);

      phyopnd = static_cast<AArch64CGFunc *>(cgfunc_)->GetOrCreatePhysicalRegisterOperand((AArch64reg_t)(V0 + i), 64,
                                                                                          kRegTyFloat);
      CollectLiveInfo(bb, phyopnd, true, false);
    }
  }
  const AArch64MD *md = &AArch64CG::kMd[static_cast<AArch64Insn *>(insn)->mop_];
  for (int i = 0; i < Insn::kMaxOperandNum; i++) {
    Operand *opnd = insn->opnds[i];
    if (opnd == nullptr) {
      continue;
    }
    AArch64OpndProp *regprop = static_cast<AArch64OpndProp *>(md->operand_[i]);
    bool isDef = regprop->IsRegDef();
    bool isUse = regprop->IsRegUse();
    if (opnd->IsList()) {
      ListOperand *listopnd = static_cast<ListOperand *>(opnd);
      for (auto op : listopnd->GetOperands()) {
        CollectLiveInfo(bb, op, false, true);
      }
    } else if (opnd->IsMemoryAccessOperand()) {
      MemOperand *memopnd = static_cast<MemOperand *>(opnd);
      Operand *base = memopnd->GetBaseRegister();
      Operand *offset = memopnd->GetIndexRegister();
      if (base != nullptr) {
        CollectLiveInfo(bb, base, false, true);
      }
      if (offset != nullptr) {
        CollectLiveInfo(bb, offset, false, true);
      }
    } else if (opnd->IsConditionCode()) {
      Operand *rflag = cgfunc_->GetOrCreateRflag();
      CollectLiveInfo(bb, rflag, false, true);
    } else {
      CollectLiveInfo(bb, opnd, isDef, isUse);
    }
  }
}

void AArch64LiveAnalysis::BreakBBIntoSubBB(BB *bb) {
  /* For a B that can throw, at least two subBBs will be created.
     The insn that throws will be the first insn of the subBB.
   */
  BB *newSubBB = nullptr;
  Insn *prev = nullptr;
  FOR_BB_INSNS(insn, bb) {
    if (CanInsnThrow(insn)) {
      if (newSubBB) {
        newSubBB->lastinsn = prev;
        newSubBB = CreateSubBB(bb);
        newSubBB->firstinsn = insn;
      } else {
        // first insn of bb can throw
        newSubBB = CreateSubBB(bb);
        newSubBB->firstinsn = insn;
      }
    } else if (newSubBB == nullptr) {
      // first insn of bb did not throw
      newSubBB = CreateSubBB(bb);
      newSubBB->firstinsn = insn;
    }
    prev = insn;
  }
  newSubBB->lastinsn = bb->lastinsn;

  for (auto sbb: (*subBB)[bb->id]) {
    for (auto ehsucc: bb->eh_succs) {
      sbb->eh_succs.push_back(ehsucc);
    }
  }
  uint32 numSubBBs = (*subBB)[bb->id].size();
  for (uint32 idx = 0; idx < (numSubBBs - 1); idx++) {
    ((*subBB)[bb->id])[idx]->succs.push_back( ((*subBB)[bb->id])[idx+1] );
  }
  for (auto succ: bb->succs) {
    ((*subBB)[bb->id])[numSubBBs-1]->succs.push_back(succ);
  }
}

/* entry of get def/use of bb.
   getting the def or use info of each regopnd as parameters of CollectLiveInfo().
   build the subBB structure for Eh aware live analysis.
 */
void AArch64LiveAnalysis::GetBBDefUse(BB *bb) {
  if (bb->IsEmpty()) {
    return;
  }
  if (GetDoEhLiveAnalysis() && bb->eh_succs.size() > 0) {
    BreakBBIntoSubBB(bb);

    for (auto sbb = (*subBB)[bb->id].rbegin(); sbb != (*subBB)[bb->id].rend(); sbb++) {
      FOR_SUBBB_INSNS_REV(insn, (*sbb)) {
        if (!insn->IsMachineInstruction()) {
          continue;
        }
        GetInsnDefUse((*sbb), insn);
      }
    }
    // Live analysis does not need this info once it is split into sub-BBs.
    // However some optimization phase uses this info.
    FOR_BB_INSNS_REV(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      GetInsnDefUse(bb, insn);
    }
  } else {
    FOR_BB_INSNS_REV(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      GetInsnDefUse(bb, insn);
    }
  }
}

bool AArch64LiveAnalysis::CleanupBBIgnoreReg(uint32 reg) {
  if (reg < 8 || (reg >= 29 && reg <= 32)) {
    return true;
  }
  return false;
}

void AArch64LiveAnalysis::FinalizeLiveAnalysis() {
  FOR_ALL_BB(bb, cgfunc_) {
    auto it = bb->livein_regno.begin();
    while (it != bb->livein_regno.end()) {
      regno_t rno = *it;
      if (rno & LIVE_SLOT_MASK) {
        it = bb->livein_regno.erase(it);
        bb->livein_regno.insert(rno & LIVE_REG_MASK);
      } else {
        it++;
      }
    }

    it = bb->liveout_regno.begin();
    while (it != bb->liveout_regno.end()) {
      regno_t rno = *it;
      if (rno & LIVE_SLOT_MASK) {
        it = bb->liveout_regno.erase(it);
        bb->liveout_regno.insert(rno & LIVE_REG_MASK);
      } else {
        it++;
      }
    }
  }
}

}  // namespace maplebe
