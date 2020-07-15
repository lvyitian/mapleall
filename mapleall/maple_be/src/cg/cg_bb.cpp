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

#include "cg_bb.h"
#include "dbg.h"
#include <iostream>

namespace maplebe {

using namespace std;

Insn *BB::InsertInsnBefore(Insn *existing, Insn *newinsn) {
  Insn *pre = existing->prev;
  if (pre == nullptr) {
    newinsn->prev = nullptr;
    newinsn->next = existing;
    existing->prev = newinsn;
  } else {
    newinsn->prev = pre;
    newinsn->next = existing;
    existing->prev = newinsn;
    pre->next = newinsn;
  }
  if (existing == firstinsn) {
    firstinsn = newinsn;
  }
  newinsn->bb = this;
  return newinsn;
}

Insn *BB::InsertInsnAfter(Insn *existing, Insn *newinsn) {
  newinsn->prev = existing;
  newinsn->next = existing->next;
  existing->next = newinsn;
  if (existing == lastinsn) {
    lastinsn = newinsn;
  } else if (newinsn->next) {
    newinsn->next->prev = newinsn;
  }
  newinsn->bb = this;
  return newinsn;
}

void BB::ReplaceInsn(Insn *insn, Insn *newinsn) {
  if (insn->IsAccessRefField()) {
    newinsn->MarkAsAccessRefField(true);
  }
  if (insn->do_not_remove) {
    newinsn->do_not_remove = true;
  }
  newinsn->prev = insn->prev;
  newinsn->next = insn->next;
  if (insn == lastinsn) {
    lastinsn = newinsn;
  } else if (newinsn->next) {
    newinsn->next->prev = newinsn;
  }

  if (firstinsn == insn) {
    firstinsn = newinsn;
  } else if (newinsn->prev) {
    newinsn->prev->next = newinsn;
  }
  newinsn->bb = this;
}

void BB::InsertBackBeforeControlTransfer(Insn *insn) {
  CG_ASSERT(kind != kBBIntrinsic, "NYI");
  CG_ASSERT(kind != kBBReturn, "NYI");
  switch (kind) {
    case kBBIf:
    case kBBRangegoto:
    case kBBGoto: {
      Insn *bi = lastinsn;
      while (!bi->IsBranch()) {
        bi = bi->prev;
      }
      InsertInsnBefore(bi, insn);
    } break;
    default:
      CG_ASSERT(kind == kBBCall || kind == kBBThrow || kind == kBBFallthru, "");
      AppendInsn(insn);
      break;
  }
}

void BB::InsertBackBeforeControlTransferAndCall(Insn *insn) {
  CG_ASSERT(kind != kBBIntrinsic, "NYI");
  CG_ASSERT(kind != kBBReturn, "NYI");
  switch (kind) {
    case kBBIf:
    case kBBRangegoto:
    case kBBGoto: {
      Insn *bi = lastinsn;
      while (!bi->IsBranch()) {
        bi = bi->prev;
      }
      InsertInsnBefore(bi, insn);
    } break;
    case kBBThrow:
    case kBBCall: {
      Insn *bi = lastinsn;
      while (!bi->IsCall()) {
        bi = bi->prev;
      }
      InsertInsnBefore(bi, insn);
    } break;
    case kBBReturn: {
      Insn *bi = lastinsn;
      while (!bi->IsReturn()) {
        bi = bi->prev;
      }
      InsertInsnBefore(bi, insn);
    } break;
    default:
      CG_ASSERT(kind == kBBFallthru, "");
      AppendInsn(insn);
      break;
  }
}

void BB::RemoveInsn(Insn *insn) {
  if (firstinsn == insn && lastinsn == insn) {
    firstinsn = lastinsn = nullptr;
  } else if (firstinsn == insn) {
    firstinsn = insn->next;
  } else if (lastinsn == insn) {
    lastinsn = insn->prev;
  }
  // remove insn from lir list
  Insn *prev = insn->prev;
  Insn *next = insn->next;
  if (prev) {
    prev->next = next;
  }
  if (next) {
    next->prev = prev;
  }
}

void BB::RemoveInsnPair(Insn *insn, Insn *nextInsn) {
  CG_ASSERT(insn->next == nextInsn && nextInsn->prev == insn, "next_insn is supposed to follow insn");
  if (this->firstinsn == insn && lastinsn == nextInsn) {
    this->firstinsn = lastinsn = nullptr;
  } else if (this->firstinsn == insn) {
    this->firstinsn = nextInsn->next;
  } else if (this->lastinsn == nextInsn) {
    this->lastinsn = insn->prev;
  }
  if (insn->prev) {
    insn->prev->next = nextInsn->next;
  }
  if (nextInsn->next) {
    nextInsn->next->prev = insn->prev;
  }
}

// Remove insns in this bb from insn1 to insn2.
void BB::RemoveInsnSequence(Insn *insn1, Insn *insn2) {
  CG_ASSERT(insn1 && insn2 && insn1->bb == this && insn2->bb == this, "remove insn sequence in one bb");
  if (firstinsn == insn1 && lastinsn == insn2) {
    firstinsn = lastinsn = nullptr;
  } else if (firstinsn == insn1) {
    firstinsn = insn2->next;
  } else if (lastinsn == insn2) {
    lastinsn = insn1->prev;
  }

  if (insn1->prev) {
    insn1->prev->next = insn2->next;
  }
  if (insn2->next) {
    insn2->next->prev = insn1->prev;
  }
}

void BB::AppendBBInsns(BB *bb)  // append all insns from bb into this bb
{
  if (!firstinsn) {
    firstinsn = bb->firstinsn;
    lastinsn = bb->lastinsn;
    if (firstinsn) {
      FOR_BB_INSNS(i, bb) {
        i->bb = this;
      }
    }
    return;
  }
  if (bb->firstinsn == nullptr || bb->lastinsn == nullptr) {
    return;
  }
  FOR_BB_INSNS_SAFE(insn, bb, nextinsn) {
    AppendInsn(insn);
  }
}

void BB::InsertAtBeginning(BB *bb)  // append all insns from bb into this bb
{
  if (!bb->firstinsn) { /* nothing to add */
    return;
  }

  FOR_BB_INSNS(insn, bb) {
    insn->bb = this;
  }

  if (!firstinsn) {
    firstinsn = bb->firstinsn;
    lastinsn = bb->lastinsn;
  } else {
    bb->lastinsn->next = firstinsn;
    firstinsn->prev = bb->lastinsn;
    firstinsn = bb->firstinsn;
  }
  bb->firstinsn = bb->lastinsn = nullptr;
}

// Number of instructions excluding DbgInsn and comments
int BB::NumInsn() {
  int bbSize = 0;

  FOR_BB_INSNS(i, this) {
    if (i->IsImmaterialInsn()) {
      continue;
    }
    bbSize++;
  }
  return bbSize;
}

void BB::Dump() {
  LogInfo::MapleLogger() << "=== BB " << " <" << GetKindName();
  if (labidx) {
    LogInfo::MapleLogger() << "[labeled with " << labidx << "]";
  }
  LogInfo::MapleLogger() << "> <" << id << "> ";
  if (is_cleanup) {
    LogInfo::MapleLogger() << "[is_cleanup] ";
  }
  if (unreachable) {
    LogInfo::MapleLogger() << "[unreachable] ";
  }
  if (succs.size()) {
    LogInfo::MapleLogger() << "succs: ";
    MapleList<BB *>::iterator it;
    for (it = succs.begin(); it != succs.end(); ++it) {
      BB *sbb = *it;
      LogInfo::MapleLogger() << sbb->id << " ";
    }
  }
  if (eh_succs.size()) {
    LogInfo::MapleLogger() << "eh_succs: ";
    MapleList<BB *>::iterator it;
    for (it = eh_succs.begin(); it != eh_succs.end(); ++it) {
      BB *sbb = *it;
      LogInfo::MapleLogger() << sbb->id << " ";
    }
  }
  if (preds.size()) {
    LogInfo::MapleLogger() << "preds: ";
    MapleList<BB *>::iterator it;
    for (it = preds.begin(); it != preds.end(); ++it) {
      BB *pbb = *it;
      LogInfo::MapleLogger() << pbb->id << " ";
    }
  }
  if (eh_preds.size()) {
    LogInfo::MapleLogger() << "eh_preds: ";
    MapleList<BB *>::iterator it;
    for (it = eh_preds.begin(); it != eh_preds.end(); ++it) {
      BB *pbb = *it;
      LogInfo::MapleLogger() << pbb->id << " ";
    }
  }

  LogInfo::MapleLogger() << "===\n";
  LogInfo::MapleLogger() << "frequency:" << frequency << endl;

  Insn *insn = firstinsn;
  while (insn) {
    insn->dump();
    insn = insn->next;
  }
}

}  // namespace maplebe
