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

#include "super_bb.h"
#include "cg.h"
#include "aarch64_isa.h"

// Provide merge interface, that can merge into superBB by traversing all BBs.
// Each superBB contains multiple function call instructions.
// The corresponding split function also provided.

namespace maplebe {

using namespace maple;

Insn *SuperBBBuilder::FindLastMachineInsn(BB *bb) const {
  Insn *result = nullptr;
  CG_ASSERT(bb->GetKind() == BB::kBBCall, "expected kind of BB");
  for (Insn *insn = bb->lastinsn; insn && insn != bb->firstinsn->prev; insn = insn->prev) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }
    result = insn;
    break;
  }
  return result;
}

// if bb has no other precursor, return true, otherwise return false.
bool SuperBBBuilder::HasOtherPreds(const BB *mergee) const {
  if (mergee->eh_preds.empty() && mergee->preds.size() == 1) {
    return false;
  }
  return true;
}

// if merger and mergee BB have the same eh_succs, return true, otherwise return false.
bool SuperBBBuilder::IsEhsuccsSame(const BB *merger, const BB *mergee) const {
  CG_ASSERT(mergee != nullptr, "mergee BB exsit");
  if (merger != nullptr) {
    if (merger->eh_succs.empty() && mergee->eh_succs.empty()) {
      return true;
    } else {
      return (merger->eh_succs == mergee->eh_succs ? true : false);
    }
  }
  return true;
}

// return the last but not empty bb in superbb.
BB *SuperBBBuilder::GetLastBBInSuperBB(SuperBB *sbb) const {
  CG_ASSERT(sbb->GetEnclosedBBs().size() >= 1, "enclosed_BB size");
  BB *foundBB = nullptr;
  BB *tempBB = nullptr;
  for (MapleVector<BB *>::iterator it = sbb->GetEnclosedBBs().end(); it != sbb->GetEnclosedBBs().begin();) {
    tempBB = *(--it);
    if (!tempBB->IsEmpty()) {
      foundBB = tempBB;
      break;
    }
  }
  return foundBB;
}

// return the first but not empty bb in superbb.
BB *SuperBBBuilder::GetFirstBBInSuperBB(SuperBB *sbb) const {
  CG_ASSERT(sbb->GetEnclosedBBs().size() >= 1, "enclosed_BB size");
  BB *foundBB = nullptr;
  BB *tempBB = nullptr;
  for (MapleVector<BB *>::iterator it = sbb->GetEnclosedBBs().begin(); it != sbb->GetEnclosedBBs().end(); it++) {
    tempBB = *(it);
    if (!tempBB->IsEmpty()) {
      foundBB = tempBB;
      break;
    }
  }
  return foundBB;
}

// judge if mergee_bb is in LSDA.
bool SuperBBBuilder::IsInLSDA(const BB *bb, SuperBB *sbb) const{
  if (cgFunc->mirModule.IsJavaModule() == false) {
    return false;
  }
  EHFunc *ehfunc = cgFunc->ehfunc;
  if (!ehfunc->NeedFullLSDA()) {
    return true;
  }
  bool tryAtFront = false;
  bool mergeeIsTry = false;
  BB *bbFront = sbb->GetEnclosedBBs().front();
  MapleVector<LSDACallSite *>& callsite_table = ehfunc->lsda_callsite_table->callsite_table;
  for (uint32 i = 0; i < callsite_table.size(); i++) {
    LSDACallSite *lsdacallsite = callsite_table[i];
    if (lsdacallsite->cs_length.start_offset->labelIdx == bbFront->labidx) {
      tryAtFront = true;
      break;
    }
  }
  if (tryAtFront) {
    // current bb is try_block and with the same eh_succs of bb_front
    for (uint32 i = 0; i < callsite_table.size(); i++) {
      LSDACallSite *lsdacallsite = callsite_table[i];
      if (lsdacallsite->cs_length.start_offset->labelIdx == bb->labidx) {
        mergeeIsTry = true;
        return (bbFront->eh_succs == bb->eh_succs ? true : false);
      }
    }
    // other type is not allowed
    if (!mergeeIsTry) {
      return false;
    }
  } else {
    // superbb doesn't start with try_block, but current bb is in LSDA.
    if (cgFunc->theCFG->InLSDA(bb->labidx, ehfunc)) {
      return false;
    }
  }
  return true;
}

// update bb_ptr of each insn: pointer to superbb after merge and pointer to oringial bb after split.
void SuperBBBuilder::UpdateInsnBB() {
  FOR_ALL_BB(bb, cgFunc) {
    FOR_BB_INSNS(insn, bb) {
      insn->bb = bb;
    }
  }
}

// update informattion of superbb, including kind, label, first and last insn.
void SuperBBBuilder::UpdateSuperBBInfo(SuperBB *sbb) {
  // update kind and cleanup attribute
  BB *lastBB = sbb->GetEnclosedBBs().back();
  sbb->SetKind(lastBB->GetKind());
  if (lastBB->is_cleanup) {
    sbb->is_cleanup = true;
  }
  // set label
  BB *firstBB = sbb->GetEnclosedBBs().front();
  CG_ASSERT(!firstBB->unreachable, "unreachable BB should have been deleted.");
  sbb->AddLabel(firstBB->labidx);
  // set firststmt
  sbb->firststmt = firstBB->firststmt;
  // set first and last insn
  BB *firstBBNotEmpty = GetFirstBBInSuperBB(sbb);
  if (firstBBNotEmpty != nullptr) {
    sbb->firstinsn= firstBBNotEmpty->firstinsn;
  }
  BB *lastBBNotEmpty = GetLastBBInSuperBB(sbb);
  if (lastBBNotEmpty != nullptr) {
    sbb->lastinsn = lastBBNotEmpty->lastinsn;
  }
  sbb->SetUpdatedFlag(true);
}

// update information of cgFunc. param: merge use state:1, split use state:0
void SuperBBBuilder::UpdateCGFunc(bool state) {
  if (state) {
    // set first and last bb of cgfunc.
    SuperBB *first = sBBVect.front();
    cgFunc->firstbb = first;
    SuperBB *last = sBBVect.back();
    cgFunc->lastbb = last;
    // set pred/succs/eh_preds/eh_succs of each superbb.
    for (MapleVector<SuperBB *>::iterator itSBB = sBBVect.begin(); itSBB != sBBVect.end(); ++itSBB) {
      SuperBB *sBB = (*itSBB);
      SetPredsAndSuccs(sBB);
    }
  } else {
    // reset first and last bb
    SuperBB *firstSuper = sBBVect.front();
    BB *firstBB = firstSuper->GetEnclosedBBs().front();
    cgFunc->firstbb = firstBB;
    SuperBB *endSuper = sBBVect.back();
    BB *lastBB = endSuper->GetEnclosedBBs().back();
    cgFunc->lastbb = lastBB;
  }
}

// reset preds/succs and eh_preds/eh_succs for each superbb.
void SuperBBBuilder::SetPredsAndSuccs(SuperBB *superBB) {
  BB *lastBB = superBB->GetEnclosedBBs().back();
  for (BB *succ : lastBB->succs) {
    for (MapleVector<SuperBB *>::iterator itSBB = sBBVect.begin(); itSBB != sBBVect.end(); ++itSBB) {
      SuperBB *sBB = *itSBB;
      BB *firstBB = sBB->GetEnclosedBBs().front();
      CG_ASSERT(!firstBB->unreachable, "unreachable BB should have been deleted.");
      if (succ->id == firstBB->id) {
        superBB->succs.push_back(sBB);
        break;
      }
    }
  }
  for (BB *ehs : lastBB->eh_succs) {
    for (MapleVector<SuperBB *>::iterator itSBB = sBBVect.begin(); itSBB != sBBVect.end(); ++itSBB) {
      SuperBB *sBB = *itSBB;
      BB *firstBB = sBB->GetEnclosedBBs().front();
      CG_ASSERT(!firstBB->unreachable, "unreachable BB should have been deleted.");
      if (ehs->id == firstBB->id) {
        superBB->eh_succs.push_back(sBB);
        break;
      }
    }
  }
  BB *firstBB = superBB->GetEnclosedBBs().front();
  for (BB *pred : firstBB->preds) {
    for (MapleVector<SuperBB *>::iterator itSBB = sBBVect.begin(); itSBB != sBBVect.end(); ++itSBB) {
      SuperBB *sBB = *itSBB;
      BB *lastBB = sBB->GetEnclosedBBs().back();
      CG_ASSERT(!lastBB->unreachable, "unreachable BB should have been deleted.");
      if (pred->id == lastBB->id) {
        superBB->preds.push_back(sBB);
        break;
      }
    }
  }
  for (BB *ehp : firstBB->eh_preds) {
    for (MapleVector<SuperBB *>::iterator itSBB = sBBVect.begin(); itSBB != sBBVect.end(); ++itSBB) {
      SuperBB *sBB = *itSBB;
      BB *lastBB = sBB->GetEnclosedBBs().back();
      CG_ASSERT(!lastBB->unreachable, "unreachable BB should have been deleted.");
      if (ehp->id == lastBB->id) {
        superBB->eh_preds.push_back(sBB);
        break;
      }
    }
  }
}

/*entry function for merge bb.
  the following conditions are need to be satisfied:
  1) prev_bb is kind of fallthru or call;
  2) no other BB can jump into the current BB except the sequential precursor;
  3) if exist eh_succs which must be consistent with the previous BB;
  4) try and catch BB cannot appear in the middle of superbb.
*/
void SuperBBBuilder::MergeProcess() {
  BB *bb = cgFunc->firstbb;
  if (!bb) {
    return;
  }
  // init data vecotor
  sBBVect.clear();
  cgFunc->exitbbsvec.clear();
  // start merge process
  SuperBB *sBB = nullptr;
  for (; bb; bb = bb->next) {
    // firstbb or return_bb does not merge with other bb.
    if (bb == cgFunc->firstbb || bb->GetKind() == BB::kBBReturn) {
      sBB = CreateNewSuperBB();
      sBB->GetEnclosedBBs().push_back(bb);
      if (bb->GetKind() == BB::kBBReturn && !sBBVect.empty()) {
        SuperBB *lastSBB = sBBVect.back();
        if (!lastSBB->GetUpdatedFlag()) {
          UpdateSuperBBInfo(lastSBB);
        }
        lastSBB->next = sBB;
        sBB->prev = lastSBB;
        cgFunc->exitbbsvec.push_back(sBB);
      }
      sBBVect.push_back(sBB);
      UpdateSuperBBInfo(sBB);
      continue;
    }
    CG_ASSERT(!sBBVect.empty(), "sBBVect should not be empty!");
    sBB = sBBVect.back();
    // create new superBB from second bb.
    if (bb == cgFunc->firstbb->next) {
      SuperBB *newSuperBB = nullptr;
      newSuperBB = CreateNewSuperBB();
      newSuperBB->GetEnclosedBBs().push_back(bb);
      sBBVect.push_back(newSuperBB);
      sBB->next = newSuperBB;
      newSuperBB->prev = sBB;
      continue;
    }
    // try to merge next bb
    CG_ASSERT(bb->prev, "bb->prev should be exist.");
    if ((bb->prev->GetKind() == BB::kBBFallthru || bb->prev->GetKind() == BB::kBBCall) &&
        !HasOtherPreds(bb) &&
        IsEhsuccsSame(bb->prev, bb) &&
        IsInLSDA(bb, sBB)) {
      if (bb->prev->GetKind() == BB::kBBCall) {
        Insn *insnlast = FindLastMachineInsn(bb->prev);
        if (insnlast && insnlast->IsCall()) {
          sBB->callInsns.push_back(insnlast);
        }
      }
      sBBVect.pop_back();
      BB *prevNonEmptyBB = GetLastBBInSuperBB(sBB);
      if (!bb->IsEmpty() && prevNonEmptyBB) {
        prevNonEmptyBB->lastinsn->next = bb->firstinsn;
        bb->firstinsn->prev = prevNonEmptyBB->lastinsn;
      }
      sBB->GetEnclosedBBs().push_back(bb);
      sBBVect.push_back(sBB);
    } else {
      UpdateSuperBBInfo(sBB);
      SuperBB *newSuperBB = nullptr;
      newSuperBB = CreateNewSuperBB();
      newSuperBB->GetEnclosedBBs().push_back(bb);
      sBBVect.push_back(newSuperBB);
      // link the current superBB and the last one
      sBB->next = newSuperBB;
      newSuperBB->prev = sBB;
    }
  }
  if (!sBB->GetUpdatedFlag()) {
    UpdateSuperBBInfo(sBB);
  }
  UpdateCGFunc(1);
  UpdateInsnBB();
}

// check if catch_BB is in the front of superbb, and try_BB only can be merged after try_BB.
bool SuperBBBuilder::CheckTryCatchStatus() const {
  EHFunc *ehfunc = cgFunc->ehfunc;
  if (!ehfunc->NeedFullLSDA()) {
    return true;
  }
  FOR_ALL_BB(bb, cgFunc) {
    bool tryAtBegin = false;
    SuperBB *sBB = static_cast<SuperBB*>(bb);
    if (sBB->GetEnclosedBBs().size() == 1) {
      continue;
    } else {
      for (MapleVector<BB*>::iterator it = sBB->GetEnclosedBBs().begin(); it != sBB->GetEnclosedBBs().end(); ++it) {
        BB *curBB = *it;
        MapleVector<LSDACallSite *>& callsite_table = ehfunc->lsda_callsite_table->callsite_table;
        for (uint32 i = 0; i < callsite_table.size(); i++) {
          LSDACallSite *lsdacallsite = callsite_table[i];
          if (lsdacallsite->cs_length.start_offset->labelIdx == curBB->labidx) {
            if (curBB == sBB->GetEnclosedBBs().front()) {
              tryAtBegin = true;
            } else {
              if (!tryAtBegin) {
                return false;
              }
            }
            break;
          }
          if (lsdacallsite->cs_landing_pad.end_offset &&
              lsdacallsite->cs_landing_pad.end_offset->labelIdx == curBB->labidx) {
            if (curBB != sBB->GetEnclosedBBs().front()) {
              return false;
            }
            break;
          }
        }
      }
    }
  }
  return true;
}

// old interface functions for split.
void SuperBBBuilder::SplitProcessOld() {
  // step1, split the encloed_bbs in superbb: break the link between lastinsn and firstinsn of next bb.
  for (MapleVector<SuperBB*>::iterator itSBB = sBBVect.begin(); itSBB != sBBVect.end(); ++itSBB) {
    SuperBB *sBB = *itSBB;
    if (sBB->GetEnclosedBBs().size() > 1) {
      MapleVector<BB*>::iterator itBB = sBB->GetEnclosedBBs().begin();
      for (; itBB != sBB->GetEnclosedBBs().end(); ++itBB) {
        BB *curBB = *itBB;
        Insn *last = curBB->lastinsn;
        if (last == nullptr) {
          continue;
        }
        if (last->next) {
          if (last->next->prev == last) {
            last->next->prev = nullptr;
            last->next = nullptr;
          } else {
            // lastinsn has been modified.
            if (last->next->prev) {
              Insn *newInsn = last->next->prev;
              if (FindInsnLink(newInsn, curBB)) {
                // original lastinsn has been replaced with new insn that links to other BB.
                newInsn->next = nullptr;
                last->next->prev = nullptr;
              } else {
                // may insert not only one new insns behind lastinsn, so we need to split according to its nextBB.
                BB *nextBB = curBB->next;
                FOR_BB_INSNS(first, nextBB) {
                  if (first && first->prev) {
                    if (first->prev->next == first) {
                      first->prev->next = nullptr;
                      first->prev = nullptr;
                      break;
                    }
                  }
                }
              }
              last->next = nullptr;
            } else {
              // "last->next->prev == nullptr" means curBB is empty and linked with nextBB
              curBB->lastinsn = nullptr;
            }
          }
        }
      }
    }
  }
  // step2, change the firstbb of cgFunc and deal with the modify of firstinsn or lastinsn by traversing all bbs.
  UpdateCGFunc(0);
  FOR_ALL_BB(bb, cgFunc) {
    bool haveSetFirst = false;
    Insn *insn = nullptr;
    for (insn=bb->firstinsn; insn&&insn!=bb->lastinsn; insn=insn->next) {
      if (!insn->next) {
        break;
      }
      if (!bb->lastinsn) {
        // bb is empty after step 1
        insn = nullptr;
        break;
      }
      // use loop to collect all insns that link its prev and next normaly.
      if (insn->next->prev == insn) {
        if (!haveSetFirst) {
          bb->firstinsn = insn;
          haveSetFirst = true;
        }
        insn->bb = bb;
      } else {
        if (insn->next->prev == nullptr) {
          // firstinsn has been deleted.
          continue;
        } else if (insn->next->prev->bb != bb) {
          if (insn->next->prev->bb != bb->prev) {
            // firstinsn has been replaced with new insn.
            if (!haveSetFirst) {
              bb->firstinsn = insn->next->prev;
              haveSetFirst = true;
            }
            insn->next->prev->bb = bb;
          } else {
            // firstinsn and prev_BB's lastinsn have been deleted.
            continue;
          }
        }
      }
    }
    // judge whether bb become empty after other opt.
    if ((insn && insn->prev && insn->prev->next != insn)) {
      insn = nullptr;
    }
    // set lastinsn pointer
    bb->lastinsn = insn;
    if (insn) {
      insn->bb = bb;
    }
    if (!haveSetFirst) {
      bb->firstinsn = insn;
      haveSetFirst = true;
    }
  }
}

// find the target insn with which current insn linked.(used in old split function)
bool SuperBBBuilder::FindInsnLink(const Insn *insn, const BB *bb) const {
  SuperBB *sbb = static_cast<SuperBB*>(insn->bb);
  if (sbb->GetEnclosedBBs().size() > 1) {
    for (MapleVector<BB*>::iterator it = sbb->GetEnclosedBBs().begin(); it != sbb->GetEnclosedBBs().end(); ++it) {
      BB *curBB = *it;
      if (curBB->id <= bb->id) {
        continue;
      } else {
        FOR_BB_INSNS(insnNextBB, curBB) {
          if (insn->next == insnNextBB && insnNextBB->prev == insn) {
            return true;
          }
        }
      }
    }
    return false;
  }
  return false;
}

// split superbb into original bb.
// insns of fallthru_BB will palce to its non-fallthru next bb.
void SuperBBBuilder::SplitProcess() {
  cgFunc->exitbbsvec.clear();
  for (MapleVector<SuperBB*>::iterator itS = sBBVect.begin(); itS != sBBVect.end(); ++itS) {
    SuperBB *sBB = *itS;
    BB::BBKind kindS = sBB->GetKind();
    Insn *insnF = sBB->firstinsn;
    Insn *insnL = sBB->lastinsn;
    if (sBB->GetEnclosedBBs().size() > 1) {
      bool extendBB = false;
      BB *curBB = nullptr;
      for (MapleVector<BB*>::iterator it = sBB->GetEnclosedBBs().begin(); it != sBB->GetEnclosedBBs().end(); ++it) {
        curBB = *it;
        curBB->callInsns.clear();
        BB::BBKind kindO = curBB->GetKind();
        bool haveSetFirst = false;
        if (curBB != sBB->GetEnclosedBBs().back() && (curBB->IsEmpty() || kindO == BB::kBBFallthru) &&
            curBB->next->GetKind() != BB::kBBReturn && curBB != cgFunc->cleanupbb) {
          curBB->firstinsn = nullptr;
          curBB->lastinsn = nullptr;
          continue;
        } else { // insert insns to original bb
          Insn *last = curBB->lastinsn;
          bool lastBB = false;
          if (curBB == sBB->GetEnclosedBBs().back()) {
            lastBB = true;
          }
          Insn *insnFlag = nullptr;
          if (last) {
            for (; insnF&&insnF!=last->next; insnF=insnF->next) {
              if (!haveSetFirst) {
                curBB->firstinsn = insnF;
                haveSetFirst = true;
              }
              insnF->bb = curBB;
              if (insnF->IsCall()) {
                curBB->callInsns.push_back(insnF);
              }
              insnFlag = insnF;
            }
          }
          if (lastBB && insnF) {
            extendBB = true;
          }
          curBB->lastinsn = insnFlag;
          if (curBB->lastinsn) {
            ResetKind(curBB);
            if (curBB->lastinsn->next && curBB->lastinsn->next->prev == curBB->lastinsn) {
              curBB->lastinsn->next->prev = nullptr;
            }
            curBB->lastinsn->next = nullptr;
          }
          if (!haveSetFirst) {
            curBB->firstinsn = curBB->lastinsn;
            haveSetFirst = true;
          }
        }
      }
      if (extendBB) {
        BB *newBB = CreateNewFallThroughBB(curBB);
        newBB->firstinsn = insnF;
        insnF->bb = newBB;
        Insn *lastInsn = insnF;
        insnF = insnF->next;
        for (; insnF ; insnF = insnF->next) {
          insnF->bb = newBB;
          lastInsn = insnF;
        }
        newBB->lastinsn = lastInsn;
      }
    } else {
      BB *onlyBB = sBB->GetEnclosedBBs().front();
      onlyBB->callInsns.clear();
      if (kindS == BB::kBBReturn) {
        CG_ASSERT(onlyBB->GetKind() == BB::kBBReturn, "return bb does not merge with others.");
        if (cgFunc->exitbbsvec.size() > 0) {
          CG_ASSERT(cgFunc->exitbbsvec.size() == 1, "BB_ret should not be more than one.");
          cgFunc->exitbbsvec.pop_back();
          cgFunc->exitbbsvec.push_back(onlyBB);
        } else {
          cgFunc->exitbbsvec.push_back(onlyBB);
        }
      }
      bool haveSetFirst = false;
      BB *oldBB = nullptr;
      FOR_BB_INSNS(insn, sBB) {
        if (!haveSetFirst) {
          onlyBB->firstinsn = insn;
          haveSetFirst = true;
        }
        insn->bb = onlyBB;
        if (insn->IsCall()) {
          onlyBB->callInsns.push_back(insn);
          onlyBB->lastinsn = insn;
          ResetKind(onlyBB);
          if (!haveSetFirst) {
            onlyBB->firstinsn = onlyBB->lastinsn;
          }
          if (insn->next) {
            oldBB = onlyBB;
            onlyBB = CreateNewFallThroughBB(onlyBB);
            oldBB->SetKind(BB::kBBCall);
            haveSetFirst = false;
          }
        }
        onlyBB->lastinsn = insn;
      }
      if (oldBB) {
        oldBB->lastinsn->next->prev = nullptr;
        oldBB->lastinsn->next = nullptr;
      }
    }
  }
  UpdateCGFunc(0);
  DeleteEmptyBB();
}

void SuperBBBuilder::ResetKind(BB* bb) {
  CG_ASSERT(bb->lastinsn, "");
  MOperator mop = bb->lastinsn->GetMachineOpcode();
  switch (mop) {
    case MOP_xret: {
      bb->SetKind(BB::kBBReturn);
      break;
    }
    case MOP_xuncond: {
      bb->SetKind(BB::kBBGoto);
      break;
    }
    case MOP_xbr: {
      bb->SetKind(BB::kBBRangegoto);
      break;
    }
    case MOP_bmi:
    case MOP_bvc:
    case MOP_bls:
    case MOP_blt:
    case MOP_ble:
    case MOP_blo:
    case MOP_beq:
    case MOP_bpl:
    case MOP_bhs:
    case MOP_bvs:
    case MOP_bhi:
    case MOP_bgt:
    case MOP_bge:
    case MOP_bal:
    case MOP_bne:
    case MOP_wcbz:
    case MOP_xcbz:
    case MOP_wcbnz:
    case MOP_xcbnz:
    case MOP_wtbz:
    case MOP_xtbz:
    case MOP_wtbnz:
    case MOP_xtbnz: {
      bb->SetKind(BB::kBBIf);
      break;
    }
  }
}

void SuperBBBuilder::DeleteEmptyBB() {
  BB *bb = cgFunc->firstbb;
  CG_ASSERT(!bb->unreachable, "unreachable BB should have been deleted.");
  while (bb) {
    // Empty bb but do not have cleanup label.
    if (bb->prev != nullptr && bb->firststmt != cgFunc->cleanup_label &&
        bb != cgFunc->lastbb && bb->GetKind() != BB::kBBReturn &&
        bb->firstinsn == nullptr && bb->lastinsn == nullptr &&
        !cgFunc->theCFG->InLSDA(bb->labidx, cgFunc->ehfunc) &&
        !cgFunc->theCFG->InSwitchTable(bb->labidx, cgFunc)) {
      if ((bb->GetKind() == BB::kBBFallthru || bb->GetKind() == BB::kBBCall) &&
          bb->next && bb->next->firststmt == cgFunc->cleanup_label) {
        return;
      }
      bool jumpFlag = false;
      if(bb->GetKind() == BB::kBBFallthru || bb->GetKind() == BB::kBBCall) {
        jumpFlag = false;
      } else if ((bb->GetKind() == BB::kBBGoto && bb->prev->GetKind() == BB::kBBGoto) ||
                 (bb->GetKind() == BB::kBBIf && bb->prev->GetKind() == BB::kBBIf)) {
        jumpFlag = true;
      }
      cgFunc->theCFG->InitInsnVisitor(cgFunc, cgFunc->memPool);
      cgFunc->theCFG->RemoveBB(bb, jumpFlag);
    }
    bb = bb->next;
  }
}

BB *SuperBBBuilder::CreateNewFallThroughBB(BB *prevBB) {
  BB *newBB = cgFunc->CreateNewBBFromBB(prevBB);
  uint32 succSize = prevBB->succs.size();
  CG_ASSERT((succSize <= 2), "Error: SplitProcess prevBB succ > 2");

  // preds/succs chain of prevBB -> newBB -> nextBB
  if (succSize == 1) {
    BB *nextBB = prevBB->succs.front();
    newBB->succs.push_back(nextBB);
    auto itPrev = find(nextBB->preds.begin(), nextBB->preds.end(), prevBB);
    CG_ASSERT(itPrev!=nextBB->preds.end(),"Error: SplitProcess nextBB pred not found");
    nextBB->preds.erase(itPrev);
    nextBB->preds.push_back(newBB);
    prevBB->succs.erase(prevBB->succs.begin());
    prevBB->SetKind(BB::kBBFallthru);
  } else if (succSize == 2) {
    for (auto nextBB : prevBB->succs) {
      newBB->succs.push_back(nextBB);
      auto itPrev = find(nextBB->preds.begin(), nextBB->preds.end(), prevBB);
      CG_ASSERT(itPrev!=nextBB->preds.end(),"Error: SplitProcess nextBB pred not found");
      nextBB->preds.erase(itPrev);
      nextBB->preds.push_back(newBB);
#if BUGFIX_REMOVED
      nextBB->SetKind(prevBB->GetKind());
      prevBB->SetKind(BB::kBBFallthru);
#endif
    }
    prevBB->succs.clear();
  }
  newBB->SetKind(prevBB->GetKind());

  newBB->preds.push_back(prevBB);
  prevBB->succs.push_back(newBB);

  if (prevBB->eh_succs.size() && cgFunc->CanBBThrow(newBB)) {
    for (auto ehBB: prevBB->eh_succs) {
      newBB->eh_succs.push_back(ehBB);
      ehBB->eh_preds.push_back(newBB);
    }
  }
  return newBB;
}

// check integrity of bb and insns in bbs.
bool SuperBBBuilder::CheckBBInsnStatus() const {
  FOR_ALL_BB(bb, cgFunc) {
    if (bb->next) {
      if (bb->next->prev != bb) {
        LogInfo::MapleLogger()<<"bb->next->prev != bb"<<endl;
        return false;
      }
    }
    if (bb->prev) {
      if (bb->prev->next != bb) {
        LogInfo::MapleLogger()<<"bb->prev->next != bb"<<endl;
        return false;
      }
    }
    if (!bb->IsEmpty()) {
      Insn *insn = bb->firstinsn;
      if (!insn) {
        return false;
      }
      CG_ASSERT((nullptr == bb->firstinsn->prev && nullptr == bb->lastinsn->next), "insn link error.");
      while (insn) {
        if (insn->next) {
          if (insn->next->prev != insn) {
            LogInfo::MapleLogger()<<"insn->next->prev != insn"<<endl;
            return false;
          }
        }
        if (insn->prev) {
          if (insn->prev->next != insn) {
            LogInfo::MapleLogger()<<"insn->prev->next != insn"<<endl;
            return false;
          }
        }
        insn = insn->next;
      }
    }
  }
  return true;
}

int SuperBBBuilder::GetCallBBCount(SuperBB *sbb) const {
  int nCount = 0;
  for (MapleVector<BB*>::iterator itBB = sbb->GetEnclosedBBs().begin(); itBB != sbb->GetEnclosedBBs().end(); ++itBB) {
    BB *curBB = *itBB;
    if (curBB->GetKind() == BB::kBBCall) {
      nCount++;
    }
  }
  return nCount;
}

}  // namespace maplebe
