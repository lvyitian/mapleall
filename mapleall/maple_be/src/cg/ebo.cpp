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

/*
 * The Optimizations include forward propagation, common expression elimination, constant folding,
 * dead code elimination and some target optimizations. The main entry of the optimization is run.
 * When the Optimization level is less than O2, it can only perform in single block. and in O2 it
 * can perform it a sequence of blocks.
 */

#include "ebo.h"
#include "cg.h"
#include "optimize_common.h"
#include "cg_assert.h"
#include <iostream>
#include "securec.h"

namespace maplebe {

using namespace maple;

int eboNumOpndinfoEntries = 0;

#define EBODUMP CGDEBUGFUNC(cgfunc)
#define TRUE_OPND cgfunc->GetTrueOpnd()
#define EBO_OPND_HASH_LENGTH (EBO_MAX_OPND_HASH - 1)
#define EBO_MAX_BBNUMS 200

/* Return the number of regs for load/store insn.*/
int32_t Ebo::GetRegNumForMem(Insn *insn) {
  CG_ASSERT(insn->AccessMem(), "");
  if (insn->IsStore())

  {
    return insn->GetOpndNum();
  } else {
    return insn->GetResultNum();
  }
  return 0;
}

/*get insn's MemOperand*/
MemOperand *Ebo::GetMemOpnd(Insn *insn) {
  CG_ASSERT(insn->AccessMem(), "");
  Operand *mem = nullptr;
  if (insn->IsStore()) {
    mem = insn->GetResult(0);
  } else {
    mem = insn->GetOpnd(0);
  }

  // special case: marked as ISLOAD
  // special rx, label + offset0
  if (mem && mem->IsMemoryAccessOperand()) {
    return static_cast<MemOperand *>(mem);
  }
  return nullptr;
}

MemOpndInfo *Ebo::GetMemInfo(InsnInfo *insninfo) {
  Insn *insn = insninfo->insn;
  CG_ASSERT(insn && insn->AccessMem(), "");
  OpndInfo *meminfo = nullptr;
  if (insn->IsStore()) {
    meminfo = insninfo->result[0];
  } else {
    meminfo = insninfo->orig_opnd[0];
  }

  return (meminfo != nullptr) ? static_cast<MemOpndInfo *>(meminfo) : nullptr;
}

Operand *Ebo::GetBase(Insn *insn) {
  CG_ASSERT(insn->AccessMem(), "");
  MemOperand *mem = GetMemOpnd(insn);
  if (mem) {
    return mem->GetBaseRegister();
  }

  return nullptr;
}

Operand *Ebo::GetOffset(Insn *insn) {
  CG_ASSERT(insn->AccessMem(), "");
  MemOperand *mem = GetMemOpnd(insn);
  if (mem) {
    return mem->GetOffset();
  }

  return nullptr;
}

RegOperand *Ebo::GetRegOpnd(Insn *insn) {
  CG_ASSERT(insn->AccessMem(), "");
  if (insn->IsStore()) {
    return static_cast<RegOperand *>(insn->GetOpnd(0));
  } else {
    return static_cast<RegOperand *>(insn->GetResult(0));
  }
}

bool Ebo::IsFrameReg(Operand *opnd) {
  if (!opnd->IsRegister()) {
    return false;
  }
  RegOperand *reg = static_cast<RegOperand *>(opnd);
  return cgfunc->IsFrameReg(reg);
}

Operand *Ebo::GetZeroOpnd(uint32_t size) {
  return cgfunc->GetZeroOpnd(size);
}

bool Ebo::IsSaveReg(Operand *opnd) {
  if (!opnd->IsRegister()) {
    return false;
  }
  RegOperand *reg = static_cast<RegOperand *>(opnd);
  return reg->IsSaveReg(GlobalTables::GetTypeTable().GetTypeFromTyIdx(cgfunc->func->GetReturnTyIdx()), cgfunc->becommon);
}

bool Ebo::IsPhysicalReg(Operand *opnd) {
  if (!opnd->IsRegister()) {
    return false;
  }
  RegOperand *reg = static_cast<RegOperand *>(opnd);
  return reg->IsPhysicalRegister();
}

bool Ebo::HasAssignedReg(Operand *opnd) {
  if (!opnd->IsRegister()) {
    return false;
  }
  RegOperand *reg = static_cast<RegOperand *>(opnd);
  return reg->IsVirtualRegister() ? (!reg->IsInvalidRegister()) : true;
}

bool Ebo::IsOfSameClass(Operand *op0, Operand *op1) const {
  if (!op0->IsRegister() || !op1->IsRegister()) {
    return false;
  }
  RegOperand *reg0 = static_cast<RegOperand *>(op0);
  RegOperand *reg1 = static_cast<RegOperand *>(op1);
  return reg0->GetRegisterType() == reg1->GetRegisterType();
}

/*return true if opnd of bb is available.*/
bool Ebo::OpndAvailableInBB(const BB *bb, OpndInfo *info) {
  if (info == nullptr || info->opnd == nullptr) {
    return false;
  }
  Operand *op = info->opnd;
  int32_t hashval = op->IsRegister() ? -1 : info->hashval;
  if (op->IsConstant()) {
    return true;
  }
  if (GetOpndInfo(op, hashval) != info) {
    return false;
  }
  // global operands aren't supported at low levels of optimization.
  if ((g->optim_level < 2) && (bb != info->bb)) {
    return false;
  }
  if (before_regalloc && IsPhysicalReg(op)) {
    return false;
  }

  return true;
}

/* For Memory Operand, its info was stored in a hash table, this function is to compute
   its hash value.  */
int32_t Ebo::ComputeOpndHash(Operand *opnd) {
  // using shift 4 to make the hash more evenly distributed.
  uint64 aval = reinterpret_cast<uint64>(opnd) >> 4;
  return (int32_t)(aval & EBO_OPND_HASH_LENGTH);
}

/* Store the operand information. Store it to the vreginfo if is register. otherwise put it to
   the hash table. */
void Ebo::SetOpndInfo(Operand *opnd, OpndInfo *opndinfo, int32_t hashval) {
  if (opnd->IsRegister()) {
    RegOperand *reg = static_cast<RegOperand *>(opnd);
    vreginfo[reg->GetRegisterNumber()] = opndinfo;
  } else {
    if (opndinfo != nullptr) {
      opndinfo->hashval = hashval;
      opndinfo->hash_next = exprinfo_table[hashval];
      exprinfo_table[hashval] = opndinfo;
    } else {
      OpndInfo *prev = exprinfo_table[hashval];
      if (prev) {
        exprinfo_table[hashval] = prev->hash_next;
      }
    }
  }
}

/* Used to change the info of opnd from opndinfo to newinfo. */
void Ebo::UpdateOpndInfo(Operand *opnd, OpndInfo *opndinfo, OpndInfo *newinfo, int32_t hashval) {
  if (opnd->IsRegister()) {
    RegOperand *reg = static_cast<RegOperand *>(opnd);
    vreginfo[reg->GetRegisterNumber()] = newinfo;
  } else {
    OpndInfo *info = exprinfo_table[hashval];
    if (newinfo != nullptr) {
      newinfo->hash_next = opndinfo->hash_next;
      if (info == opndinfo) {
        exprinfo_table[hashval] = newinfo;
      } else {
        while (info) {
          if (info->hash_next == opndinfo) {
            info->hash_next = newinfo;
            opndinfo->hash_next = nullptr;
            return;
          }
          info = info->hash_next;
        }
      }
    } else {
      if (exprinfo_table[hashval] == opndinfo) {
        exprinfo_table[hashval] = opndinfo->hash_next;
      } else {
        info = exprinfo_table[hashval];
        while (info) {
          if (info->hash_next == opndinfo) {
            info->hash_next = opndinfo->next;
            opndinfo->hash_next = nullptr;
            return;
          }
          info = info->hash_next;
        }
      }
    }
  }
}

/*return true if op1 op2 is equal*/
bool Ebo::OperandEqual(Operand *op1, Operand *op2) {
  if (op1 == op2) {
    return true;
  }
  if ((op1 == nullptr && op2 != nullptr) || (op1 != nullptr && op2 == nullptr)) {
    return false;
  }
  CHECK_FATAL(op2 != nullptr && op1 != nullptr, "null ptr check ");
  if (op1->GetKind() != op2->GetKind()) {
    return false;
  }
  return OperandEqSpecial(op1, op2);
}

OpndInfo *Ebo::GetOpndInfo(Operand *opnd, int32_t hashval) {
  if (hashval < 0) {
    RegOperand *reg = static_cast<RegOperand *>(opnd);
    auto it = vreginfo.find(reg->GetRegisterNumber());
    return it != vreginfo.end() ? it->second : nullptr;
  } else {
    OpndInfo *info = exprinfo_table[hashval];
    while (info) {
      if (OperandEqual(opnd, info->opnd)) {
        // if base/offset if redefined, then the mem if redefined.
        if (opnd->IsMemoryAccessOperand()) {
          MemOpndInfo *meminfo = static_cast<MemOpndInfo *>(info);
          if (meminfo != nullptr && ((meminfo->GetBaseInfo() && meminfo->GetBaseInfo()->redefined) ||
                                     (meminfo->GetOffsetInfo() && meminfo->GetOffsetInfo()->redefined))) {
            info = info->hash_next;
            continue;
          }
        }
        return info;
      }
      info = info->hash_next;
    }
  }
  return nullptr;
}

/* Create a opndinfo for opnd. */
OpndInfo *Ebo::GetNewOpndInfo(BB *bb, Insn *insn, Operand *opnd, int32_t hashval) {
  OpndInfo *opndinfo = nullptr;
  if (opnd->IsMemoryAccessOperand()) {
    opndinfo = ebomp->New<MemOpndInfo>(opnd);
  } else {
    opndinfo = ebomp->New<OpndInfo>(opnd);
  }
  eboNumOpndinfoEntries++;
  // Initialize the entry.
  opndinfo->hashval = hashval;
  opndinfo->opnd = opnd;
  opndinfo->replacement_opnd = nullptr;
  opndinfo->replacement_info = nullptr;
  opndinfo->bb = bb;
  opndinfo->insn = insn;
  opndinfo->insninfo = nullptr;
  opndinfo->redefined_inbb = false;
  opndinfo->redefined = false;
  opndinfo->same = nullptr;
  opndinfo->refcount = 0;

  opndinfo->prev = last_opndinfo;
  if (first_opndinfo == nullptr) {
    first_opndinfo = opndinfo;
  } else {
    last_opndinfo->next = opndinfo;
  }
  last_opndinfo = opndinfo;
  opndinfo->next = nullptr;

  return opndinfo;
}

bool Ebo::NoEhinsnBetweenRedef(OpndInfo *opndinfo, OpndInfo *opndinfoPrev) {
  if (opndinfo == nullptr || opndinfoPrev == nullptr) {
    return false;
  }
  InsnInfo *info = LocateInsnInfo(opndinfo);
  InsnInfo *infoPrev = LocateInsnInfo(opndinfoPrev);
  CHECK_FATAL(info->insn->bb == infoPrev->insn->bb,"bb shuold be equal");
  if (info->insn->bb->eh_succs.empty()) {
    return true;
  }
  Insn *insnfrom = info->insn;
  Insn *insnto = infoPrev->insn;
  for (Insn *insn = insnfrom; insn != insnto; insn = insn->next) {
    if (insn->CanThrow()) {
      return false;
    } else {
      continue;
    }
  }
  return true;
}

/* Create infomation for local_opnd from its def insn current_insn. */
OpndInfo *Ebo::OperandInfoDef(BB *currentBb, Insn *currentInsn, Operand *localOpnd) {
  int32_t hashval = localOpnd->IsRegister() ? -1 : ComputeOpndHash(localOpnd);
  OpndInfo *opndinfoPrev = GetOpndInfo(localOpnd, hashval);
  OpndInfo *opndinfo = GetNewOpndInfo(currentBb, currentInsn, localOpnd, hashval);
  if (localOpnd->IsMemoryAccessOperand()) {
    MemOpndInfo *meminfo = static_cast<MemOpndInfo *>(opndinfo);
    MemOperand *mem = static_cast<MemOperand *>(localOpnd);
    Operand *base = mem->GetBaseRegister();
    Operand *offset = mem->GetOffset();
    if (base && base->IsRegister()) {
      meminfo->SetBaseInfo(OperandInfoUse(currentBb, currentInsn, base));
    }
    if (offset && offset->IsRegister()) {
      meminfo->SetOffsetInfo(OperandInfoUse(currentBb, currentInsn, offset));
    }
  }
  opndinfo->same = opndinfoPrev;
  if ((opndinfoPrev != nullptr)) {
    opndinfoPrev->redefined = TRUE;
    if (opndinfoPrev->bb == currentBb /*&& NoEhinsnBetweenRedef(opndinfo, opndinfoPrev)*/) {
      opndinfoPrev->redefined_inbb = TRUE;
    }
  }
  // For vectore registers
  if (!before_regalloc && hashval < 0 && IsFloatReg(static_cast<RegOperand *>(localOpnd))) {
    regno_t regno0 = GetLowVec(localOpnd);
    regno_t regno1 = GetHighVec(localOpnd);
    auto it = vreginfo.find(regno0);
    OpndInfo *tempInfo = (it != vreginfo.end() ? it->second : nullptr);
    if ((tempInfo != nullptr)) {
      tempInfo->redefined = TRUE;
      if (tempInfo->bb == currentBb) {
        tempInfo->redefined_inbb = TRUE;
      }
    }

    it = vreginfo.find(regno1);
    tempInfo = (it != vreginfo.end() ? it->second : nullptr);
    if ((tempInfo != nullptr)) {
      tempInfo->redefined = TRUE;
      if (tempInfo->bb == currentBb) {
        tempInfo->redefined_inbb = TRUE;
      }
    }
  }

  if (opndinfoPrev != nullptr) {
    UpdateOpndInfo(localOpnd, opndinfoPrev, opndinfo, hashval);
  } else {
    SetOpndInfo(localOpnd, opndinfo, hashval);
  }
  return opndinfo;
}

/* Update the use infomation for local_opnd because of its use insn current_insn. */
OpndInfo *Ebo::OperandInfoUse(BB *currentBb, Insn *currentInsn, Operand *localOpnd) {
  if (!localOpnd->IsRegister() && !localOpnd->IsMemoryAccessOperand()) {
    return nullptr;
  }
  int32_t hashval = localOpnd->IsRegister() ? -1 : ComputeOpndHash(localOpnd);
  OpndInfo *opndinfo = GetOpndInfo(localOpnd, hashval);

  if (opndinfo == nullptr) {
    opndinfo = GetNewOpndInfo(currentBb, nullptr, localOpnd, hashval);
    SetOpndInfo(localOpnd, opndinfo, hashval);
  }
  IncRef(opndinfo);
  return opndinfo;
}

/*return true if op0 is identical with op1*/
bool Ebo::RegistersIdentical(Operand *op0, Operand *op1) {
  if (op0 == op1) {
    return true;
  }
  if (!(op0->IsRegister() && op1->IsRegister())) {
    return false;
  }
  RegOperand *reg0 = static_cast<RegOperand *>(op0);
  RegOperand *reg1 = static_cast<RegOperand *>(op1);
  return ((reg0->IsPhysicalRegister() || !reg0->IsInvalidRegister()) &&
          (reg1->IsPhysicalRegister() || !reg1->IsInvalidRegister()) &&
          (reg0->GetRegisterType() == reg1->GetRegisterType()) &&
          (reg0->GetRegisterNumber() == reg1->GetRegisterNumber()));
}

/*
 *For a given memory insn, look for a preceeding memory insn to
 *the same location and attempt to replace one of them.
 *Return TRUE if this memory insn is no longer needed.
 */
bool Ebo::FindDupMemInsn(BB *bb, Insn *insn, Operand **opnds, OpndInfo **opndInfo, OpndInfo **origInfo) {
  if (insn == nullptr || (!insn->IsLoad() && !insn->IsStore()) || insn->do_not_remove || insn->IsYieldpoint()) {
    return false;
  }

  if (EBODUMP) {
    LogInfo::MapleLogger() << "===Enter FindDupMemInsn of bb:" << bb->id << "===\n";
  }

  MemOperand *memopnd = GetMemOpnd(insn);
  if (memopnd == nullptr || memopnd->GetIndexRegister() || memopnd->GetScaleOperand()) {
    return false;
  }

  OpndInfo *baseinfo = nullptr;
  OpndInfo *offsetinfo = nullptr;
  // For load, the memopnd has been analyzed yet.
  if (insn->IsLoad()) {
    MemOpndInfo *meminfo = static_cast<MemOpndInfo *>(origInfo[0]);
    if (meminfo != nullptr) {
      baseinfo = meminfo->GetBaseInfo();
      offsetinfo = meminfo->GetOffsetInfo();
    }
  } else {
    CHECK_FATAL(GetBase(insn), "null ptr check");
    baseinfo = GetOpndInfo(GetBase(insn), -1);
    Operand *off = GetOffset(insn);
    if (off && off->IsRegister()) {
      offsetinfo = GetOpndInfo(off, -1);
    }
  }

  if (baseinfo == nullptr) {
    return false;
  }
  // find the index of the address compoments of the insn.
  Operand *base = baseinfo->opnd;
  Operand *offset = offsetinfo ? offsetinfo->opnd : memopnd->GetOffset();
  bool insnMatch = false;
  bool insnReplaced = false;
  int32_t hashval = 0;
  hashval = ComputeHashVal(insn, opndInfo);
  InsnInfo *insninfo = insninfo_table[hashval];
  // record the insninfo whose same is insninfo.
  InsnInfo *prevInfo = nullptr;

  if (!base || !offset) {
    return false;
  }

  // handle stack related memory first
  if (hashval != EBO_SPILL_MEM_HASH && hashval != EBO_DEFAULT_MEM_HASH) {
    return false;
  }

  while (insninfo) {
    Insn *prev = insninfo->insn;
    if (prev == nullptr || !prev->AccessMem()) {
      prevInfo = insninfo;
      insninfo = insninfo->same;
      continue;
    }
    // non-stack memory maybe modified by call insn.
    if (prev->bb != bb && hashval != EBO_SPILL_MEM_HASH) {
      break;
    }

    MemOpndInfo *prevMeminfo = GetMemInfo(insninfo);
    CG_ASSERT(prevMeminfo != nullptr, "");
    OpndInfo *prevBaseinfo = prevMeminfo->GetBaseInfo();
    OpndInfo *prevOffsetinfo = prevMeminfo->GetOffsetInfo();
    MemOperand *prevMem = static_cast<MemOperand *>(prevMeminfo->opnd);
    Operand *prevOffset = prevOffsetinfo ? prevOffsetinfo->opnd : prevMem->GetOffset();
    insnMatch = (prevOffset && baseinfo && prevBaseinfo && baseinfo == prevBaseinfo && offsetinfo == prevOffsetinfo);

    bool insnIsSubset = false;
    bool offsetMayOverlap = true;

    if (insnMatch && offset != prevOffset) {
      // check the real offset
      // i. operand equal.
      CHECK_FATAL(prevOffset, "null ptr check ");
      if (offset->GetKind() == prevOffset->GetKind() && OperandEqSpecial(offset, prevOffset)) {
        break;
      }
      // ii. imm offset.
      if (!((offset->IsConstImmediate() || offset->IsOfstImmediate()) &&
            (prevOffset->IsConstImmediate() || prevOffset->IsOfstImmediate()))) {
        insnMatch = false;
        prevInfo = insninfo;
        insninfo = insninfo->same;
        continue;
      }
      int32_t currOff = 0;
      int32_t prevOff = 0;
      if (offset->IsConstImmediate() || offset->IsOfstImmediate()) {
        ImmOperand *imm = static_cast<ImmOperand *>(offset);
        currOff = imm->GetValue();
      }
      if (prevOffset->IsConstImmediate() || prevOffset->IsOfstImmediate()) {
        ImmOperand *imm = static_cast<ImmOperand *>(prevOffset);
        prevOff = imm->GetValue();
      }

      if (insnMatch) {
        // check size
        uint32_t currSize = memopnd->GetSize() / BYTE_PER_SIZE * GetRegNumForMem(insn);
        uint32_t prevSize = prevMem->GetSize() / BYTE_PER_SIZE * GetRegNumForMem(insn);
        if (prevOff == currOff && prevSize == currSize) {
          // perfect match
        } else if (prevOff <= currOff && prevOff + prevSize >= currOff + currSize) {
          insnIsSubset = true;
        } else if (insn->IsLoad() && prev->IsLoad() && prevSize == currSize &&
                   (int32_t(prevOff + prevSize) == currOff || int32_t(currOff + currSize) == prevOff)) {
          offsetMayOverlap = false;
        } else if (int32_t(prevOff + prevSize) <= currOff || int32_t(currOff + currSize) <= prevOff) {
          offsetMayOverlap = false;
        } else {
          insnMatch = false;
        }
      }
    }

    if (memopnd->GetIndexRegister() || memopnd->GetScaleOperand()) {
      insn->dump();
      offsetMayOverlap = true;
    }

    if (insnMatch && !offsetMayOverlap) {
      prevInfo = insninfo;
      insninfo = insninfo->same;
      continue;
    }

    if (prev != nullptr && hashval == EBO_DEFAULT_MEM_HASH && prev->IsStore() != insn->IsStore()) {
      /* handle alias issues */
      if (insnMatch && insnIsSubset && insn->IsStore() && prev->IsStore()) {
        insninfo->mustnot_be_removed = true;
        break;
      }
    }

    if (insnMatch && prev != nullptr) {
      OpndInfo *resregInfo = prev->IsStore() ? insninfo->orig_opnd[0] : insninfo->result[0];
      if (resregInfo && resregInfo->redefined) {
        if (EBODUMP) {
          resregInfo->opnd->dump();
          LogInfo::MapleLogger() << " prev reg was redefined, can't be used correctly. \n";
        }
        break;
      }
      if (prev->IsLoad() || (prev->IsStore() && insn->IsLoad())) {
        if (!OpndAvailableInBB(bb, resregInfo)) {
          if (EBODUMP && resregInfo) {
            resregInfo->opnd->dump();
            LogInfo::MapleLogger() << " prev reg unavailable \n";
          }
          break;
        }
      }
    }

    if (insnMatch) {
      /* If we match a volatile memory insn, this
          one should have been volatile, too. */
      if (prev->IsVolatile()) {
        break;
      }
      if (insnIsSubset) {
        /* delete_subset_mem_insn */
      } else {
        if (EBODUMP) {
          LogInfo::MapleLogger() << "< === > \n";
          insn->dump();
          insninfo->insn->dump();
        }
        insnReplaced = DeleteDupMemInsn(insn, opndInfo, insninfo, prevInfo);
      }
    }

    if (insnReplaced) {
      return true;
    } else {
      /* If insn is matched and not deleted, both insns are kept. */
      if (insn->IsStore()) {
        insninfo->mustnot_be_removed = true;
      }
      CHECK_FATAL(prev, "null ptr check");
      if (insn->IsStore() && prev->IsLoad()) {
        break;
      }
      if (insnIsSubset || (hashval == EBO_DEFAULT_MEM_HASH)) {
        if (prev->IsStore()) {
          insninfo->mustnot_be_removed = true;
        }
        break;
      }
    }

    prevInfo = insninfo;
    insninfo = insninfo->same;
  }

  return false;
}

/*
 *find Common subexpression (dupinsn)
 *and attemp to delete it.
 *return true if deleted dupinsn successfully.
 */
bool Ebo::FindDupInsn(BB *bb, Insn *insn, Operand **opnds, OpndInfo **opndInfo, OpndInfo **origInfo) {
  if (insn == nullptr) {
    return false;
  }
  int32_t opndnum = insn->GetOpndNum();
  int32_t resnum = insn->GetResultNum();
  int32_t hasval = 0;
  bool insnMatch = false;

  if (insn->AccessMem()) {
    return false;
  }
  if (insn->GetResultNum() == 0 || insn->GetOpndNum() == 0) {
    return false;
  }
  // Condition def use the result of the insns before which can't be seen in the operands of the current insn.
  // skip these insns now.
  if (insn->IsPseudoInstruction() || IsImplicit(insn)) {
    return false;
  }

  hasval = ComputeHashVal(insn, opndInfo);
  InsnInfo *insninfo = insninfo_table[hasval];
  while (insninfo) {
    Insn *prev = insninfo->insn;
    insnMatch = ((prev != nullptr) && insn->GetResultNum() == prev->GetResultNum() &&
                 insn->GetOpndNum() == prev->GetOpndNum() && (insn->GetMachineOpcode() == prev->GetMachineOpcode()));

    if (insnMatch) {
      for (int32_t i = 0; i < opndnum; i++) {
        if (insninfo->optimal_opnd[i] == opndInfo[i]) {
          if (!opnds[i]->IsConstant() && opndInfo[i] != nullptr)
          /* If operands are not constants and the insns match,
               then the values represented must also be identical. */
          {
            continue;
          }
          /* Items that are constant (i.e. have a nullptr opnd pointer)
             must be checked to verify that the constants are the same.
             Note that there are several "reasonable" combinations that
             can come up:
               1. The constants have identical opnds.
               2. The Insns have identical opnds and the predecessor hasn't changed.
               3. The opnds have been resolved to the same constant, but the
                  operand of the insn (for some reason) could not be changed to
                  reference a constant.
           */
          if (OperandEqual(opnds[i], prev->GetOpnd(i)) ||
              ((insn->GetOpnd(i) == prev->GetOpnd(i)) && OpndAvailableInBB(bb, insninfo->orig_opnd[i])) ||
              ((insninfo->orig_opnd[i] != nullptr) &&
               OperandEqual(opnds[i], insninfo->orig_opnd[i]->replacement_opnd))) {
            continue;
          }
        }
        insnMatch = false;
        break;
      }
    }

    if (insnMatch && prev != nullptr) {
      for (int32_t j = 0; j < resnum; j++) {
        CHECK_FATAL(insn->GetResult(j), "null ptr check");
        if (!insn->GetResult(j)->IsConstReg() && !OpndAvailableInBB(bb, insninfo->result[j])) {
          insnMatch = false;
          break;
        }
        if (insninfo->result[j] && insninfo->result[j]->redefined) {
          if (EBODUMP) {
            insninfo->result[j]->opnd->dump();
            LogInfo::MapleLogger() << "prev reg was redefined, can't be used correctly. \n";
          }
          insnMatch = false;
          break;
        }
      }
    }

    if(insn->IsEffectiveCopy() && insnMatch) {
      RegOperand *currOp = static_cast<RegOperand *>(insn->GetResult(0));
      RegOperand *prevOp = static_cast<RegOperand *>(prev->GetResult(0));
      if(currOp == nullptr || prevOp == nullptr) {
        return false;
      }
      if (currOp->Equals(prevOp) && insn->bb == prev->bb) {
        bb->RemoveInsn(insn);
        return true;
      } else {
        return false;
      }
    }

    if (insnMatch) {
      if (DeleteDupInsn(insn, opndInfo, insninfo)) {
        return true;
      } else {
        break;
      }
    }

    insninfo = insninfo->same;
  }

  return false;
}

/*maybe this function does not needed anymore.*/
bool Ebo::FindPrevConst(maplebe::Insn *, maplebe::OpndInfo **) const {
  return false;
}

InsnInfo *Ebo::GetNewInsnInfo(Insn *insn) {
  InsnInfo *insninfo = ebomp->New<InsnInfo>(insn);
  int32_t nopnds = insn->GetOpndNum();
  ;
  OpndInfo **opndInfo = &insninfo->opnd_info[0];
  insninfo->optimal_opnd = &opndInfo[0];
  insninfo->orig_opnd = &opndInfo[nopnds];
  insninfo->result = &opndInfo[nopnds + nopnds];

  insninfo->prev = last_insninfo;
  if (first_insninfo == nullptr) {
    first_insninfo = insninfo;
  } else {
    last_insninfo->next = insninfo;
  }
  last_insninfo = insninfo;
  insninfo->next = nullptr;

  return insninfo;
}

int32_t Ebo::ComputeHashVal(Insn *insn, OpndInfo **opndInfo) {
  int32_t hashVal = 0;
  if (insn->AccessMem()) {
    Operand *op = nullptr;
    hashVal = EBO_DEFAULT_MEM_HASH;
    if (insn->NoAlias()) {
      hashVal = EBO_NO_ALIAS_MEM_HASH;
    }
    if (insn->IsLoad()) {
      op = insn->GetResult(0);
    } else if (insn->IsStore()) {
      op = insn->GetOpnd(0);
    }
    if (op && op->HasSpill()) {
      hashVal = EBO_SPILL_MEM_HASH;
    }
    Operand *baseReg = GetBase(insn);
    if (baseReg && IsFrameReg(baseReg)) {
      hashVal = EBO_SPILL_MEM_HASH;
    }
  } else if (insn->IsEffectiveCopy()) {
    hashVal = EBO_COPY_INSN_HASH;
  } else {
    int32_t opndnum = insn->GetOpndNum();
    hashVal = (int32_t)(insn->GetMachineOpcode());
    for (int32_t i = 0; i < opndnum; i++) {
      hashVal += (int32_t)(intptr_t)opndInfo[i];
    }
    hashVal = EBO_RESERVED_INSN_HASH + EBO_EXP_INSN_HASH(hashVal);
  }
  return hashVal;
}

/*computeHashVal of insn*/
void Ebo::HashInsn(Insn *insn, OpndInfo **origInfo, OpndInfo **opndInfo) {
  int32_t hashVal = ComputeHashVal(insn, opndInfo);
  int32_t i = 0;
  /* Create a new insninfo entry and add the new insn to the hash table. */
  InsnInfo *insninfo = GetNewInsnInfo(insn);
  insninfo->bb = insn->bb;
  insninfo->insn = insn;
  insninfo->hash_index = hashVal;
  insninfo->same = insninfo_table[hashVal];

  if (!before_regalloc) {
    if (insn->IsCall() && !insn->is_throw) {
      DefineCallerSaveRegisters(insninfo);
    } else if (IsClinitCheck(insn)) {
      DefineClinitSpecialRegisters(insninfo);
    }
  }

  for (i = 0; i < insn->GetResultNum(); i++) {
    OpndInfo *opndinfo = nullptr;
    Operand *op = insn->GetResult(i);
    if ((op != nullptr) && (op != TRUE_OPND) &&
        ((op->IsRegister() && op != GetZeroOpnd(op->GetSize())) ||
         (op->IsMemoryAccessOperand() && (static_cast<MemOperand *>(op))->GetBaseRegister() != nullptr))) {
      opndinfo = OperandInfoDef(insn->bb, insn, op);
      opndinfo->insninfo = insninfo;
    }
    insninfo->result[i] = opndinfo;
  }

  /* Copy all the opndinfo entries for the operands. */
  for (i = 0; i < insn->GetOpndNum(); i++) {
    insninfo->orig_opnd[i] = origInfo[i];
    insninfo->optimal_opnd[i] = opndInfo[i];
  }

  insninfo_table[hashVal] = insninfo;
}

/*
 *do decref of orig_info
 *refcount will be set to 0
 */
void Ebo::RemoveUses(int32_t opndnum, OpndInfo **origInfo) {
  int32_t i = 0;
  OpndInfo *info = nullptr;
  for (i = 0; i < opndnum; i++) {
    info = origInfo[i];
    if (info != nullptr) {
      DecRef(info);
      if (info->opnd->IsMemoryAccessOperand()) {
        MemOpndInfo *meminfo = static_cast<MemOpndInfo *>(info);
        OpndInfo *baseinfo = meminfo->GetBaseInfo();
        OpndInfo *offsetinfo = meminfo->GetOffsetInfo();
        if (baseinfo != nullptr) {
          DecRef(baseinfo);
        }
        if (offsetinfo != nullptr) {
          DecRef(offsetinfo);
        }
      }
    }
  }
}

/*
 *this func do :
 *1.build opereand info of bb;
 *2.do Forward propagation after regalloc;
 *3.simplify the insn,include Constant folding,redundant insns elimination.
 */
bool Ebo::BuildOperandInfo(BB *bb) {
  Insn *insn = bb->firstinsn;
  bool noBarriers = true;
  bool frameNoclobber = true;
  bool hasMemBarrier = false;
  if (EBODUMP) {
    LogInfo::MapleLogger() << "===Enter BuildOperandinfo of bb:" << bb->id << "===\n";
  }

  int maxOpnds = Insn::kMaxOperandNum;
  Operand **opnds = ebomp->NewArray<Operand *>(maxOpnds);
  OpndInfo **opndInfo = ebomp->NewArray<OpndInfo *>(maxOpnds);
  OpndInfo **origInfo = ebomp->NewArray<OpndInfo *>(maxOpnds);

  while (insn && insn != bb->lastinsn->next) {
    Insn *prev = insn->prev;
    int32_t opndnum = insn->GetOpndNum();
    int32_t resnum = insn->GetResultNum();
    Operand *opndReplace = nullptr;
    OpndInfo *opndinfo = nullptr;
    bool insnReplaced = false;
    Operand *opnd = nullptr;
    bool opndsConstant = true;

    if (insn->IsClinit()) {
      hasMemBarrier = true;
    }
    if (insn->IsBarrier() || insn->AccessRegBank()) {
      if (SpecialSequence(insn, nullptr, nullptr)) {
        if (EBODUMP) {
          LogInfo::MapleLogger() << "< ===We were able to restrict propagation of the specific registers in BB" << bb->id << "===\n";
        }
      } else {
        if (EBODUMP) {
          LogInfo::MapleLogger() << "< === BuildOperandInfo Barrier insn encountered in bb" << bb->id << "> ===\n";
        }
        noBarriers = false;
      }
    }

    if (insn->IsReturn()) {
      DefineReturnUseRegister(insn);
    }

    if (insn->IsCall() || insn->IsClinit()) {
      DefineCallUseSpecialRegister(insn);
    }

    if (opndnum == 0 && resnum == 0) {
      insn = insn->next;
      continue;
    }

    // start : Process all the operands.
    for (int32_t i = 0; i < opndnum; i++) {
      opnd = insn->GetOpnd(i);
      opndinfo = nullptr;
      opnds[i] = opnd;
      opndInfo[i] = nullptr;
      origInfo[i] = nullptr;
      if (opnd == nullptr || opnd->IsConstant()) {
        continue;
      }
      if (opnd->IsList()) {
        ListOperand *listopnd = static_cast<ListOperand *>(opnd);
        for (auto op : listopnd->GetOperands()) {
          OperandInfoUse(bb, insn, op);
        }
        continue;
      } else if (opnd->IsConditionCode()) {
        Operand *rflag = cgfunc->GetOrCreateRflag();
        CHECK_FATAL(rflag, "null ptr check");
        OperandInfoUse(bb, insn, rflag);
        // if operand is Opnd_cond, the orig_info store the info of rflag.
        OpndInfo *opndinfo = GetOpndInfo(rflag, -1);
        origInfo[i] = opndinfo;
        continue;
      }

      if (!opnd->IsRegister() && !opnd->IsMemoryAccessOperand()) {
        continue;
      }

      if (opnd->IsMemoryAccessOperand()) {
        MemOperand *memopnd = static_cast<MemOperand *>(opnd);
        Operand *base = memopnd->GetBaseRegister();
        Operand *offset = memopnd->GetOffset();
        OpndInfo *baseinfo = nullptr;
        OpndInfo *offsetinfo = nullptr;
        if (base != nullptr) {
          baseinfo = OperandInfoUse(bb, insn, base);
          // forward prop for base register.
          if (baseinfo && base->IsRegister()) {
            RegOperand *basereg = static_cast<RegOperand *>(base);
            Operand *replace = baseinfo->replacement_opnd;
            OpndInfo *replinfo = baseinfo->replacement_info;
            if (replinfo && replace && !basereg->IsSPOrFP() &&
                (!before_regalloc || (!IsPhysicalReg(replace) && !IsPhysicalReg(base))) &&
                IsOfSameClass(base, replace) &&
                base->GetSize() <= replace->GetSize()
                // In case that replace opnd was redefined.
                && !replinfo->redefined) {
              MemOperand *newmem = static_cast<MemOperand *>(memopnd->Clone(cgfunc->memPool));
              CG_ASSERT(newmem != nullptr, "newmem is null in Ebo::BuildOperandInfo(BB *bb)");
              newmem->SetBaseRegister(static_cast<RegOperand *>(replace));
              insn->SetOpnd(i, newmem);
              DecRef(baseinfo);
              IncRef(replinfo);
              baseinfo = replinfo;
            }
          }
        }
        if (offset != nullptr && offset->IsRegister()) {
          offsetinfo = OperandInfoUse(bb, insn, offset);
        }
        CG_ASSERT(insn->GetOpnd(i), "");
        opndinfo = OperandInfoUse(bb, insn, insn->GetOpnd(i));
        MemOpndInfo *meminfo = static_cast<MemOpndInfo *>(opndinfo);
        CG_ASSERT(meminfo, "");
        meminfo->SetBaseInfo(baseinfo);
        meminfo->SetOffsetInfo(offsetinfo);
        origInfo[i] = meminfo;
        continue;
      }
      opndinfo = OperandInfoUse(bb, insn, opnd);
      origInfo[i] = opndinfo;
      opndReplace = opndinfo->replacement_opnd;

      // Don't propagate physical registers before register allocation.
      if (before_regalloc && opndReplace != nullptr && (IsPhysicalReg(opndReplace) || IsPhysicalReg(opnd))) {
        opndReplace = nullptr;
      }

      if (!insn->AccessMem() && opnd->IsRegister()) {
        if (IsFrameReg(opnd)) {
          frameNoclobber = false;
        }
      }

      // forward propagation of constants
      if (opndReplace != nullptr && !(opndinfo->replacement_info != nullptr && opndinfo->replacement_info->redefined) &&
          (opndReplace->IsConstant() || OpndAvailableInBB(bb, opndinfo->replacement_info) ||
           RegistersIdentical(opnd, opndReplace)) &&
          (opndReplace->IsConstant() || (HasAssignedReg(opnd) == HasAssignedReg(opndReplace))) &&
          (!before_regalloc || (!IsPhysicalReg(opndReplace)))) {
        Operand *old = opnd;
        opnd = opndinfo->replacement_opnd;
        opndinfo = opndinfo->replacement_info;

        // constant prop.
        if (opnd->IsIntImmediate() && old->IsRegister()) {
          if (DoConstProp(insn, i, opnd)) {
            DecRef(origInfo[i]);
            // Update the actual expression info.
            origInfo[i] = opndinfo;
          }
        }
        // forward prop for registers.
        if (!opnd->IsConstant() &&
            (!before_regalloc || (HasAssignedReg(old) == HasAssignedReg(opndReplace)) || opnd->IsConstReg() ||
             !insn->IsMove()) &&
            opndinfo != nullptr && (!before_regalloc || opndinfo->bb == bb || !GRAHomeable(opndReplace)) &&
            ((insn->GetResultNum() == 0) ||
             ((insn->GetResult(0) != nullptr && !RegistersIdentical(opnd, insn->GetResult(0))) || !before_regalloc)) &&
            (before_regalloc || !IsFrameReg(old)) && !insn->IsDestRegAlsoSrcReg() &&
            ((IsOfSameClass(old, opndReplace) && old->GetSize() <= opndReplace->GetSize()) ||
             IsMovToSIMDVmov(insn, origInfo[i]->insn))) {
          // Copies to and from the same register are not needed.
          if (!before_regalloc && insn->IsEffectiveCopy() && insn->CopyOperands() == i &&
              RegistersIdentical(opnd, insn->GetResult(0))) {
            if (EBODUMP) {
              LogInfo::MapleLogger() << "===replace operand " << i << " of insn: \n";
              insn->dump();
              LogInfo::MapleLogger() << "===Remove the new insn because Copies to and from the same register. \n";
            }
            insnReplaced = true;
            continue;
          }

          if (EBODUMP) {
            LogInfo::MapleLogger() << "===replace operand " << i << " of insn: \n";
            insn->dump();
            LogInfo::MapleLogger() << "the new insn is:\n";
          }
          DecRef(origInfo[i]);
          insn->SetOpnd(i, opnd);

          if (EBODUMP) {
            insn->dump();
          }
          if (opndinfo != nullptr) {
            IncRef(opndinfo);
          }

          // Update the actual expression info.
          origInfo[i] = opndinfo;
          // extend the live range of the replacement operand.
          if (opndinfo != nullptr && opndinfo->bb != bb && opnd->IsRegister()) {
            MarkOpndLiveIntoBB(opnd, bb, opndinfo->bb);
          }
        }
      }

      opnds[i] = opnd;
      opndInfo[i] = opndinfo;

      if (!opnd->IsConstant()) {
        opndsConstant = false;
      }
    }  // End : Process all the operands.

    if (insnReplaced) {
      RemoveUses(opndnum, origInfo);
      Insn *temp = insn->next;
      bb->RemoveInsn(insn);
      insn = temp;
      continue;
    }

    // simplify the insn.
    if (insn->AccessMem()) {
      if (!insnReplaced) {
        insnReplaced = SpecialSequence(insn, opnds, origInfo);
      }
      // disable this optimization for superbb.
      if (!cgfunc->GetSBB()) {
        if (!insnReplaced && frameNoclobber && noBarriers && !hasMemBarrier) {
          insnReplaced = FindDupMemInsn(bb, insn, opnds, opndInfo, origInfo);
        }
      }
      if (!insnReplaced && frameNoclobber && noBarriers && !hasMemBarrier && insn->IsLoad()) {
        insnReplaced = RemoveRedundantLoad(bb, insn, opnds, opndInfo, origInfo);
      }
    } else if (insn->IsEffectiveCopy()) {
      if (!insnReplaced && opndsConstant) {
        insnReplaced = FindPrevConst(insn, opndInfo);
      }
      if (!insnReplaced) {
        insnReplaced = SpecialSequence(insn, opnds, opndInfo);
      }
    } else if (!insnReplaced && !insn->HasSideEffects() && !insn->AccessRegBank()) {
      if (opndsConstant && (opndnum > 1)) {
        if (IsBranchCmpSpecial(insn)) {
          insnReplaced = ResoveCondBranch(insn, opnds);
        } else if (insn->GetResultNum() >= 1) {
          insnReplaced = DoConstantFold(insn, opnds, opndInfo);
        }
      } else if (opndnum >= 1) {
        // special case
        if (insn->GetResultNum() > 0) {
          // Here mainly to do is optimizatinn of "add" when only one oprand is constant
          if (!insnReplaced && opndnum == 2 && insn->GetResultNum() == 1 &&
              ((opnds[1] != nullptr && opnds[1]->IsConstant()) || (opnds[0] != nullptr && opnds[0]->IsConstant()))) {
            insnReplaced = ConstantOperand(insn, opnds, opndInfo);
          }
        }
        if (!insnReplaced) {
          insnReplaced = SpecialSequence(insn, opnds, origInfo);
        }
      }
    }
    // Look for redundant insns.
    if (noBarriers && !insnReplaced && !insn->HasSideEffects() && !insn->AccessRegBank()) {
      insnReplaced = FindDupInsn(bb, insn, opnds, opndInfo, origInfo);
    }

    if (!insnReplaced) {
      HashInsn(insn, origInfo, opndInfo);
      int32_t resnum = insn->GetResultNum();
      // Processing the result of the insn.
      if (insn->IsEffectiveCopy() || (resnum && !insn->AccessMem())) {
        Operand *res = insn->GetResult(0);
        int32_t idx = insn->CopyOperands();
        if (res != nullptr && res != TRUE_OPND && res != GetZeroOpnd(res->GetSize())) {
          opndinfo = last_insninfo->result[0];
          // Don't propagate for fmov insns.
          if (idx >= 0 && opndinfo != nullptr && !IsFmov(insn)) {
            opndinfo->replacement_opnd = opnds[idx];
            opndinfo->replacement_info = opndInfo[idx];
          } else if (insn->IsDestRegAlsoSrcReg() && opndinfo != nullptr) {
            opndinfo->replacement_opnd = nullptr;
            opndinfo->replacement_info = nullptr;
          }
        }
      }
      insn = insn->next;
    } else {
      RemoveUses(opndnum, origInfo);
      // If insn is replaced, reanalyze the new insn to have more opportunities.
      insn = ((prev == nullptr) ? bb->firstinsn : prev->next);
    }
  }
  return noBarriers;
}

/*Decrement the use counts for the actual operands of an insninfo.*/
void Ebo::RemoveInsn(InsnInfo *info) {
  Insn *insn = info->insn;
  int32_t opndnum = insn->GetOpndNum();
  OpndInfo *opndinfo = nullptr;
  for (int32_t i = 0; i < opndnum; i++) {
    opndinfo = info->orig_opnd[i];
    if (opndinfo != nullptr) {
      DecRef(opndinfo);
      Operand *opndtemp = opndinfo->opnd;
      if (opndtemp == nullptr) {
        continue;
      }
      if (opndtemp->IsMemoryAccessOperand()) {
        MemOpndInfo *meminfo = static_cast<MemOpndInfo *>(opndinfo);
        OpndInfo *baseinfo = meminfo->GetBaseInfo();
        OpndInfo *offinfo = meminfo->GetOffsetInfo();
        if (baseinfo != nullptr) {
          DecRef(baseinfo);
        }
        if (offinfo != nullptr) {
          DecRef(offinfo);
        }
      }
    }
  }
}

/*retrun true if insn is globalneeded*/
bool Ebo::IsGlobalNeeded(Insn *insn) {
  // Calls may have side effects.
  if (insn->IsCall()) {
    //MarkCalleeSavedRegs(insn);
    return true;
  }

  // Clinit should not be removed.
  if (insn->IsFixedInsn()) {
    return true;
  }

  // Yieldpoints should not be removed by optimizer.
  if (cgfunc->cg->GenYieldpoint() && insn->IsYieldpoint()) {
    return true;
  }

  Operand *opnd = insn->GetResult(0);
  if (opnd != nullptr && (opnd->IsConstReg() || (opnd->IsRegister() && static_cast<RegOperand *>(opnd)->IsSPOrFP()))) {
    return true;
  }
  return false;
}

/* Mark opnd is live between def bb and into bb.*/
void Ebo::MarkOpndLiveIntoBB(Operand *opnd, BB *into, BB *def) {
  if (live == nullptr) {
    return;
  }
  if (into == def) {
    return;
  }
  CG_ASSERT(opnd->IsRegister(), "expect register here.");
  RegOperand *reg = static_cast<RegOperand *>(opnd);
  into->livein_regno.insert(reg->GetRegisterNumber());
  def->liveout_regno.insert(reg->GetRegisterNumber());
}

/*return insn information if has insninfo,else,return last_insninfo*/
InsnInfo *Ebo::LocateInsnInfo(OpndInfo *info) {
  if (info != nullptr && info->insn != nullptr) {
    if (info->insninfo != nullptr) {
      return info->insninfo;
    } else {
      InsnInfo *insninfo = last_insninfo;
      int32_t limit = 50;
      for (; (insninfo != nullptr && limit != 0); insninfo = insninfo->prev, limit--) {
        if (insninfo->insn == info->insn) {
          return insninfo;
        }
      }
    }
  }
  return nullptr;
}

/* Return true if opnd live out of bb.*/
bool Ebo::LiveOutOfBB(Operand *opnd, BB *bb) {
  CG_ASSERT(opnd->IsRegister(), "expect register here.");
  // when optimize_level < 2, there is need to anlyze live range.
  if (live == nullptr) {
    return false;
  }
  regno_t lout = static_cast<RegOperand *>(opnd)->GetRegisterNumber();
  return (bb->liveout_regno.find(lout) != bb->liveout_regno.end());
}

/*redundant insns elimination*/
void Ebo::RemoveUnusedInsns(BB *bb, bool normal) {
  InsnInfo *insninfo = nullptr;
  OpndInfo *opndinfo = nullptr;
  Operand *opnd = nullptr;
  int32_t resnum = 0;

  if (first_insninfo == nullptr) {
    return;
  }

  for (insninfo = last_insninfo; insninfo != nullptr; insninfo = insninfo->prev) {
    Insn *insn = insninfo->insn;
    if (insn == nullptr || insn->bb == nullptr) {
      continue;
    }
    // stop looking for insn when it goes out of bb.
    if (insn->bb != bb) {
      break;
    }

    resnum = insn->GetResultNum();
    if (bb->lastinsn == insn && insn->IsBranch()) {
      goto insn_is_needed;
    }

    if (resnum == 0 || IsGlobalNeeded(insn) || insn->IsStore()) {
      goto insn_is_needed;
    }

    // Check all the result can be removed.
    for (int32_t i = 0; i < resnum; i++) {
      opndinfo = insninfo->result[i];
      // A couple of checks.
      if (opndinfo == nullptr) {
        continue;
      }
      if (opndinfo->bb != bb || opndinfo->insn == nullptr) {
        goto insn_is_needed;
      }
      opnd = opndinfo->opnd;
      if (opnd == GetZeroOpnd(opnd->GetSize())) {
        continue;
      }
      if (insn->IsEffectiveCopy()) {
        int32_t idx = insn->CopyOperands();
        OpndInfo *opinfo = insninfo->orig_opnd[idx];
        InsnInfo *previnfo = insninfo->prev;
        if (previnfo && previnfo->insn && opinfo && opinfo->opnd->IsRegister() && IsVecReg(opinfo->opnd)) {
          Insn *prev = previnfo->insn;
          if (prev->IsEffectiveCopy() && previnfo->orig_opnd[prev->CopyOperands()] == opinfo) {
            OpndInfo *resinfo = previnfo->result[0];
            InsnInfo *nextinfo = insninfo->next;
            CG_ASSERT(insn->GetResult(0) != nullptr, "insn->GetResult(0) is null in Ebo::RemoveUnusedInsns");
            if (nextinfo && nextinfo->insn && nextinfo->orig_opnd[0] == resinfo && resinfo->refcount == 1 &&
                IsOfSameClass(resinfo->opnd, insn->GetResult(0)) && IsCmp(nextinfo->insn) &&
                !LiveOutOfBB(resinfo->opnd, opinfo->bb)) {
              nextinfo->insn->SetOpnd(0, insn->GetResult(0));
              nextinfo->orig_opnd[0] = insninfo->result[0];
              resinfo->refcount--;
              insninfo->result[0]->refcount++;
            }
          }
        }
        previnfo = LocateInsnInfo(opinfo);
        if (opinfo != nullptr && previnfo && opinfo->insn && opinfo->opnd->IsRegister() && !IsVecReg(opndinfo->opnd)) {
          Insn *prev = opinfo->insn;
          RegOperand *reg = static_cast<RegOperand *>(opinfo->opnd);
          Operand *res1 = insn->GetResult(0);
          CG_ASSERT(res1, "");
          // i. change fmov + mov  ==> fmov
          // make sure that the fmov can be removed.
          if (opinfo->refcount == 1 && prev->bb == bb && previnfo->orig_opnd[0] &&
              (previnfo == insninfo->prev || !previnfo->orig_opnd[0]->redefined_inbb) &&
              !LiveOutOfBB(reg, opinfo->bb) && ReplaceMovToVmov(insn, prev)) {
            OpndInfo *replaceInfo = previnfo->orig_opnd[0];
            if (EBODUMP) {
              LogInfo::MapleLogger() << "change mov to vmov;  ===replace operand " << i << " of insn: \n";
              insn->dump();
              LogInfo::MapleLogger() << "the new insn is:\n";
            }
            insn->SetOperand(1, replaceInfo->opnd);
            if (EBODUMP) {
              insn->dump();
            }
            DecRef(opinfo);
            IncRef(replaceInfo);
            insninfo->orig_opnd[0] = replaceInfo;
          } else if (!reg->IsSPOrFP() && (prev->IsLoad() || IsAdd(prev)) &&
                     (!LiveOutOfBB(reg, bb) || opinfo->redefined_inbb)) {
            /* pattern 1:
               ldr Rx, R29, offset
               mov R0, Rx
               ==> ldr R0, R29, offset

               pattern 2:
               ldr Rx, R29, offset
               mov R0, Rx
               add Ry, Rx, const
               ==> ldr R0, R29, 96
                   add Ry, R0, 16

               pattern 3:
               validBitNum: [32] )
               add Rx, Ry, const
               mov Ry, Rx
               ===> add Ry, Ry, const

               pattern 4:
               ldr Rx, Ry, offset
               mov Rz, Rx
               ldr Rw, Rx, offset
               ===> ldr Rz, Ry, offset
                    ldr Rw, Rz, offset
             */
            // prev->next == insn to make sure that the operand used in prev has not been redefined between prev and
            // insn.
            if (prev->IsDefinition() && insn->IsDefinition() && prev->bb == bb && prev->next == insn) {
              OpndInfo *resinfo = insninfo->result[0];
              CG_ASSERT(res1, "");
              CG_ASSERT(resinfo, "");
              if (res1->IsRegister()) {
                InsnInfo *nextinfo = insninfo->next;
                Insn *next = nullptr;
                bool pattern1 = false;
                bool pattern2 = false;
                bool pattern3 = false;
                bool pattern4 = false;
                if (opinfo->refcount == 1) {
                  if (IsOfSameClass(reg, res1)) {
                    pattern3 = true;
                    pattern1 = true;
                  } else if (prev->IsLoad() && ChangeLdrMop(prev, res1)) {
                    pattern1 = true;
                  }
                }
                if (nextinfo != nullptr && nextinfo->insn != nullptr) {
                  next = nextinfo->insn;
                  // ldr     x8, [x29,#280]          // param: _this
                  // fmov    d8, x8          //  FMOV to simd 117 caller
                  // add     x0, x8, #88   ==/=> can't be changed to  "add     x0, d8, #88"
                  if (next && prev->IsLoad() && IsOfSameClass(reg, res1) && opinfo->refcount == 2) {
                    if (nextinfo->orig_opnd[0] == opinfo && IsAdd(next)) {
                      pattern2 = true;
                    }
                    if (next->IsLoad()) {
                      MemOpndInfo *meminfo = static_cast<MemOpndInfo *>(nextinfo->orig_opnd[0]);
                      if (meminfo && meminfo->GetBaseInfo() == opinfo) {
                        pattern4 = true;
                        meminfo->SetBaseInfo(resinfo);
                        resinfo->refcount++;
                      }
                    }
                  }
                }
                if (pattern1 || pattern2 || pattern3 || pattern4) {
                  prev->SetOperand(0, res1);
                  resinfo->insn = prev;
                  CG_ASSERT(previnfo, "");
                  previnfo->result[0] = resinfo;
                  if (insninfo->mustnot_be_removed && resinfo != nullptr) {
                    previnfo->mustnot_be_removed = true;
                  }
                  if (pattern2) {
                    next->SetOperand(1, res1);
                    nextinfo->orig_opnd[0] = resinfo;
                    resinfo->refcount++;
                  } else if (pattern4) {
                    CG_ASSERT(next->GetOpnd(0) != nullptr, "next->GetOpnd(0) is null in Ebo::RemoveUnusedInsns");
                    MemOperand *memopnd = static_cast<MemOperand *>(next->GetOpnd(0)->Clone(cgfunc->memPool));
                    CG_ASSERT(memopnd != nullptr, "memopnd is null in Ebo::RemoveUnusedInsns");
                    memopnd->SetBaseRegister(static_cast<RegOperand *>(res1));
                    next->SetOperand(1, memopnd);
                  }
                  goto can_be_removed;
                }
              }
            }
          }
        }
      }
      if ((before_regalloc && IsPhysicalReg(opnd)) || (IsSaveReg(opnd) && !opndinfo->redefined_inbb)) {
        goto insn_is_needed;
      }
      // Copies to and from the same register are not needed.
      if (insn->IsEffectiveCopy()) {
        int32_t idx = insn->CopyOperands();
        CHECK_FATAL(insn->GetOpnd(idx), "null ptr check");
        if (HasAssignedReg(opnd) && HasAssignedReg(insn->GetOpnd(idx)) &&
            RegistersIdentical(opnd, insn->GetOpnd(idx))) {
          // We may be able to get rid of the copy, but be sure that the operand is marked live into this block.
          if ((insninfo->orig_opnd[idx] != nullptr) && bb != insninfo->orig_opnd[idx]->bb) {
            MarkOpndLiveIntoBB(opnd, bb, insninfo->orig_opnd[idx]->bb);
          }
          // propagate use count for this opnd to it's input operand.
          if (opndinfo->same != nullptr) {
            opndinfo->same->refcount += opndinfo->refcount;
          }

          // remove the copy causes the previous def to reach the end of the block.
          if (!opndinfo->redefined && opndinfo->same != nullptr) {
            opndinfo->same->redefined = false;
            opndinfo->same->redefined_inbb = false;
          }
          goto can_be_removed;
        }
      }
      // there must bo no direct references to the operand.
      if (!normal || opndinfo->refcount != 0) {
        goto insn_is_needed;
      }
      // When O1, the vreg who live out of bb should be recognized.
      // The regs for clinit is also be marked to recognize it can't be deleted. so extend it to O2.
      if (opnd->IsRegister()) {
        RegOperand *reg = static_cast<RegOperand *>(opnd);
        if (before_regalloc && !reg->IsBBLocalVReg()) {
          goto insn_is_needed;
        }
      }
      // Volatile || sideeffect
      if (opndinfo->insn->IsVolatile() || opndinfo->insn->HasSideEffects()) {
        goto insn_is_needed;
      }

      if (!opndinfo->redefined_inbb && LiveOutOfBB(opnd, opndinfo->bb))
      {
        goto insn_is_needed;
      }
    }
    if (!normal || insninfo->mustnot_be_removed || insn->do_not_remove) {
      goto insn_is_needed;
    }
can_be_removed:
    if (EBODUMP) {
      LogInfo::MapleLogger() << "< ==== Remove Unused insn in bb:" << bb->id << "====\n";
      insn->dump();
    }
    RemoveInsn(insninfo);
    bb->RemoveInsn(insn);
    insninfo->insn = nullptr;
    insninfo->bb = nullptr;
    for (int32_t i = 0; i < resnum; i++) {
      opndinfo = insninfo->result[i];
      CG_ASSERT(opndinfo, "");
      if (opndinfo->redefined && (opndinfo->same != nullptr)) {
        OpndInfo *next = opndinfo->same;
        next->redefined = true;
        if (opndinfo->redefined_inbb && (opndinfo->same->bb == bb)) {
          next->redefined_inbb = true;
        }
      }
      CG_ASSERT(opndinfo, "");
      if (!opndinfo->redefined_inbb && opndinfo->same != nullptr && opndinfo->same->bb == bb) {
        opndinfo->same->redefined_inbb = false;
      }
      if (!opndinfo->redefined && opndinfo->same != nullptr) {
        opndinfo->same->redefined = false;
        opndinfo->same->redefined_inbb = false;
      }
    }
    continue;
insn_is_needed:
    if (!bb->eh_succs.empty()) {
      for (int32_t i = 0; i < resnum; i++) {
        opndinfo = insninfo->result[i];
        if (opndinfo != nullptr && opndinfo->opnd != nullptr && opndinfo->same != nullptr) {
          OpndInfo *nextinfo = opndinfo->same;
          while (nextinfo) {
            if (nextinfo->insn != nullptr) {
              InsnInfo *info = LocateInsnInfo(nextinfo);
              if (info != nullptr) {
                  info->mustnot_be_removed = true;
              } else {
                /* Couldn't find the insninfo entry.  Make sure that the operand has
                  a use count so that the defining insn will not be deleted. */
                nextinfo->refcount += opndinfo->refcount;
              }
            }
            nextinfo = nextinfo->same;
          }
        }
      }
    }

    if(!bb->eh_preds.empty()) {
      for (int32_t i = 0; i < insn->GetOpndNum(); i++) {
        opndinfo = insninfo->orig_opnd[i];
        if (opndinfo != nullptr && opndinfo->opnd != nullptr && opndinfo->same != nullptr) {
          OpndInfo *nextinfo = opndinfo->same;
          while (nextinfo) {
            if (nextinfo->insn != nullptr) {
              InsnInfo *info = LocateInsnInfo(nextinfo);
              if (info != nullptr) {
                info->mustnot_be_removed = true;
              } else {
                /* Couldn't find the insninfo entry.  Make sure that the operand has
                 a use count so that the defining insn will not be deleted. */
              nextinfo->refcount += opndinfo->refcount;
              }
            }
            nextinfo = nextinfo->same;
          }
        }

        if (opndinfo != nullptr && opndinfo->opnd && bb != opndinfo->bb && opndinfo->opnd->IsRegister()) {
          MarkOpndLiveIntoBB(opndinfo->opnd, bb, opndinfo->bb);
        }
      }
    }
    continue;
  }
  return;
}

/*back up to last saved OpndInfo*/
void Ebo::BackupOpndInfoList(OpndInfo *saveLast) {
  OpndInfo *opndinfo = last_opndinfo;
  if (last_opndinfo != saveLast) {
    while (opndinfo != saveLast) {
      int32_t hashval = opndinfo->opnd->IsRegister() ? -1 : opndinfo->hashval;
      UpdateOpndInfo(opndinfo->opnd, opndinfo, opndinfo->same, hashval);
      opndinfo = opndinfo->prev;
    }
    if (saveLast != nullptr) {
      saveLast->next = nullptr;
      last_opndinfo = saveLast;
    } else {
      first_opndinfo = nullptr;
      last_opndinfo = nullptr;
    }
  }
}

/*back up to last saved insn*/
void Ebo::BackupInsnInfoList(InsnInfo *saveLast) {
  InsnInfo *insninfo = last_insninfo;
  if (last_insninfo != saveLast) {
    while (insninfo != saveLast) {
      insninfo_table[insninfo->hash_index] = insninfo->same;
      insninfo = insninfo->prev;
    }
    if (saveLast != nullptr) {
      saveLast->next = nullptr;
      last_insninfo = saveLast;
    } else {
      first_insninfo = nullptr;
      last_insninfo = nullptr;
    }
  }
}

/*add bb to eb ,and build operandinfo of bb*/
void Ebo::AddBB2EB(BB *bb) {
  OpndInfo *saveLastOpndinfo = last_opndinfo;
  InsnInfo *saveLastInsninfo = last_insninfo;
  SetBBVisited(bb);
  bbnum++;
  bool normal = BuildOperandInfo(bb);
  // Stop adding BB to EB if the bbs in the current EB exceeds EBO_MAX_BBNUMS
  if (normal && bbnum < EBO_MAX_BBNUMS) {
    for (MapleList<BB *>::iterator it = bb->succs.begin(); it != bb->succs.end(); ++it) {
      if ((*it)->preds.size() == 1 && IsNotVisited(*it)) {
        AddBB2EB(*it);
      }
    }
  }

  RemoveUnusedInsns(bb, normal);
  /* Remove information about Operand's and Insn's in this block. */
  BackupOpndInfoList(saveLastOpndinfo);
  BackupInsnInfoList(saveLastInsninfo);
  bbnum--;
  return;
}

/* Perform EBO */
void Ebo::EboProcess() {
  FOR_ALL_BB(bb, cgfunc)
  if (IsNotVisited(bb)) {
    bbnum = 0;
    AddBB2EB(bb);
  }
}

/* Perform EBO on O1 which the optimization can only be in a single block. */
void Ebo::EboProcessSingleBB() {
  FOR_ALL_BB(bb, cgfunc) {
    OpndInfo *saveLastOpndinfo = last_opndinfo;
    InsnInfo *saveLastInsninfo = last_insninfo;
    bool normal = BuildOperandInfo(bb);
    RemoveUnusedInsns(bb, normal);
    /* Remove information about Operand's and Insn's in this block. */
    BackupOpndInfoList(saveLastOpndinfo);
    BackupInsnInfoList(saveLastInsninfo);
  }
  return;
}

void Ebo::EboInit() {
  visited_bbs = ebomp->NewArray<bool>(cgfunc->NumBBs());
  for (uint32_t i = 0; i < cgfunc->NumBBs(); i++) {
    visited_bbs[i] = false;
  }
  errno_t eNum =
    memset_s(insninfo_table, EBO_MAX_INSN_HASH * sizeof(InsnInfo *), 0, EBO_MAX_INSN_HASH * sizeof(InsnInfo *));
  if (eNum) {
    FATAL(kLncFatal, "memset_s failed");
  }
  eNum = memset_s(exprinfo_table, EBO_MAX_OPND_HASH * sizeof(OpndInfo *), 0, EBO_MAX_OPND_HASH * sizeof(OpndInfo *));
  if (eNum) {
    FATAL(kLncFatal, "memset_s failed");
  }
}

/* perform EB optimizations right after instruction selection.*/
void Ebo::Run() {
  EboInit();
  if (g->optim_level >= 2) {
    EboProcess();
  } else {
    EboProcessSingleBB(); /* Perform SingleBB Optimization when -O1.  */
  }
}

/*dump ebo*/
AnalysisResult *CgDoEbo::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  LiveAnalysis *live = nullptr;
  if (EBODUMP) {
    DotGenerator::GenerateDot("ebo", cgfunc, &(cgfunc->mirModule));
  }
  // It doesn't need live range information when -O1, because the register will not live out of bb.
  if (g->optim_level >= 2) {
    live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));
  }

  MemPool *ebomp = mempoolctrler.NewMemPool("ebo");
  Ebo *ebo = cgfunc->NewEbo(cgfunc, ebomp, live, true, PhaseName().c_str());
  CG_ASSERT(ebo, "");
  ebo->Run();
  // the live range info may changed, so invalid the info.
  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  mempoolctrler.DeleteMemPool(ebomp);
  return nullptr;
}

/*dump ebo1*/
AnalysisResult *CgDoEbo1::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  LiveAnalysis *live = nullptr;
  if (EBODUMP) {
    DotGenerator::GenerateDot("ebo1", cgfunc, &(cgfunc->mirModule));
  }
  // It doesn't need live range information when -O1, because the register will not live out of bb.
  if (g->optim_level >= 2) {
    live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));
  }

  MemPool *ebomp = mempoolctrler.NewMemPool("ebo1");
  Ebo *ebo = cgfunc->NewEbo(cgfunc, ebomp, live, true, PhaseName().c_str());
  CG_ASSERT(ebo, "");
  ebo->Run();
  // the live range info may changed, so invalid the info.
  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  mempoolctrler.DeleteMemPool(ebomp);
  return nullptr;
}

/*dump postebo*/
AnalysisResult *CgDoPostEbo::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  LiveAnalysis *live = nullptr;
  if (EBODUMP) {
    DotGenerator::GenerateDot("postebo", cgfunc, &(cgfunc->mirModule), true);
  }
  // It doesn't need live range information when -O1, because the register will not live out of bb.
  if (g->optim_level >= 2) {
    live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));
  }

  MemPool *ebomp = mempoolctrler.NewMemPool("postebo");
  Ebo *ebo = cgfunc->NewEbo(cgfunc, ebomp, live, false, PhaseName().c_str());
  CHECK_FATAL(ebo, "null ptr check");
  ebo->Run();
  // the live range info may changed, so invalid the info.
  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  mempoolctrler.DeleteMemPool(ebomp);
  return nullptr;
}

}  // namespace maplebe
