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

#include "riscv64/riscv64_color_ra.h"
#include "riscv64/riscv64_insn.h"
#include "riscv64/riscv64_cg.h"
#include "riscv64/riscv64_ra_opt.h"
#include "mir_lower.h"
#include "cg_assert.h"
#include "securec.h"

#include <iostream>
#include <iomanip>
#include <queue>
#include <algorithm>
#include "securec.h"

/*
 * Based on concepts from Chow and Hennessey.
 * Phases are as follows:
 *   Prepass to collect local BB information.
 *     Compute local register allocation demands for global RA.
 *   Compute live ranges.
 *     Live ranges LR represented by a vector of size #BBs.
 *     for each cross bb vreg, a bit is set in the vector.
 *   Build interference graph with basic block as granularity.
 *     When intersection of two LRs is not null, they interfere.
 *   Separate unconstrained and constrained LRs.
 *     unconstrained - LR with connect edges less than available colors.
 *                     These LR can always be colored.
 *     constrained - not uncontrained.
 *   Split LR based on priority cost
 *     Repetitive adding BB from original LR to new LR until constrained.
 *     Update all LR the new LR interferes with.
 *   Color the new LR
 *     Each LR has a forbidden list, the registers cannot be assigned
 *     Coalesce move using preferred color first.
 *   Mark the remaining uncolorable LR after split as spill.
 *   Local register allocate.
 *   Emit and insert spills.
 */

namespace maplebe {

// for physical regopnd phyOpnd,
// R0->GetRegisterNumber() == 1
// V0->GetRegisterNumber() == 33
const uint32 k32 = sizeof(int) * CHAR_BIT;
const uint32 k64 = sizeof(long long) * CHAR_BIT;
const uint32 kCondBrNum = 2;
const uint32 kSwitchCaseNum = 5;
const uint32 kLoopWeight = 10;
const uint32 kMaxParamNum = 8;
const uint32 kNumPregBits = V31 + 1;
const uint32 kNumInsnThreashold = 200000;

#define GCRA_DUMP CGDEBUGFUNC(cgfunc_)

void LiveUnit::PrintLiveUnit() const {
  LogInfo::MapleLogger() << "[" << bgn << "," << end << "]";
  LogInfo::MapleLogger() << "<D" << defNum << "U" << useNum << ">";
  if (hasCall == false) {
    // Too many calls, so only print when there is no call.
    LogInfo::MapleLogger() << " nc";
  }
  if (needRlod) {
    LogInfo::MapleLogger() << " rlod";
  }
  if (needRstr) {
    LogInfo::MapleLogger() << " rstr";
  }
}

void GraphColorRegAllocator::PrintLiveUnitMap(const LiveRange *lr) const {
  LogInfo::MapleLogger() << "\n\tlu:";
  for (uint32 i = 0; i < cgfunc_->NumBBs(); i++) {
    if (IS_BIT_ARR_ELEM_SET(lr->bmember, i)) {
      auto lu = lr->luMap.find(i);
      if (lu != lr->luMap.end()) {
        LogInfo::MapleLogger() << "(" << i << " ";
        lu->second->PrintLiveUnit();
        LogInfo::MapleLogger() << ")";
      }
    }
  }
  LogInfo::MapleLogger() << endl;
}

void GraphColorRegAllocator::PrintLiveRangeConflicts(const LiveRange *lr) const {
  LogInfo::MapleLogger() << "\n\tinterfere(" << lr->numBconflicts << "): ";
  for (uint32 i = 0; i < regBuckets; i++) {
    uint64 chunk = lr->bconflict[i];
    for (uint64 bit = 0; bit < (sizeof(uint64) * CHAR_BIT); bit++) {
      if (chunk & (1LL << bit)) {
        regno_t newno = i * sizeof(uint64) * CHAR_BIT + bit;
        LogInfo::MapleLogger() << newno << ",";
      }
    }
  }
  LogInfo::MapleLogger() << "\n";
}

void GraphColorRegAllocator::PrintLiveBbBit(const LiveRange *lr) const {
  LogInfo::MapleLogger() << "live_bb(" << lr->numBmembers << "): ";
  for (uint32 i = 0; i < cgfunc_->NumBBs(); i++) {
    if (IS_BIT_ARR_ELEM_SET(lr->bmember, i)) {
      LogInfo::MapleLogger() << i << " ";
    }
  }
  LogInfo::MapleLogger() << "\n";
}

void GraphColorRegAllocator::PrintLiveRange(const LiveRange *lr, const string str) const {
  LogInfo::MapleLogger() << str << "\n";

  LogInfo::MapleLogger() << "R" << lr->regno;
  if (lr->regtype == kRegTyInt) {
    LogInfo::MapleLogger() << "(I)";
  } else if (lr->regtype == kRegTyFloat) {
    LogInfo::MapleLogger() << "(F)";
  } else {
    LogInfo::MapleLogger() << "(U)";
  }
  LogInfo::MapleLogger() << "\tnumCall " << lr->numCall;
  LogInfo::MapleLogger() << "\tpriority " << lr->priority;
  LogInfo::MapleLogger() << "\tforbidden: ";
  for (regno_t preg = 0; preg < kNumPregBits; preg++) {
    if (lr->forbidden[preg]) {
      LogInfo::MapleLogger() << preg << ",";
    }
  }
  LogInfo::MapleLogger() << "\tpregveto: ";
  for (regno_t preg = 0; preg < kNumPregBits; preg++) {
    if (lr->pregveto[preg]) {
      LogInfo::MapleLogger() << preg << ",";
    }
  }
  if (lr->spilled) {
    LogInfo::MapleLogger() << " spilled";
  }
  if (lr->splitLr) {
    LogInfo::MapleLogger() << " split";
  }
  LogInfo::MapleLogger() << "\n";
  PrintLiveBbBit(lr);
  PrintLiveRangeConflicts(lr);
  PrintLiveUnitMap(lr);
  if (lr->splitLr) {
    PrintLiveRange(lr->splitLr, "===>Split LR");
  }
}

void GraphColorRegAllocator::PrintLiveRanges() const {
  cout << "PrintLiveRanges: size = " << lrVec.size() << endl;
  for (uint32_t i = 0; i < lrVec.size(); i++) {
    if (lrVec[i] == nullptr || lrVec[i]->regno == 0) {
      continue;
    }
    PrintLiveRange(lrVec[i], "");
  }
  LogInfo::MapleLogger() << "\n";
}

void GraphColorRegAllocator::PrintLocalRaInfo(const string str) const {
  LogInfo::MapleLogger() << str << endl;
  for (uint32 id = 0; id < cgfunc_->NumBBs(); id++) {
    LocalRaInfo *lraInfo = localRegVec[id];
    if (lraInfo == nullptr) {
      continue;
    }
    LogInfo::MapleLogger() << "bb " << id << " def ";
    for (auto it : lraInfo->defCnt) {
      LogInfo::MapleLogger() << "[" << it.first << ":" << it.second << "],";
    }
    LogInfo::MapleLogger() << "\n";
    LogInfo::MapleLogger() << "use ";
    for (auto it : lraInfo->useCnt) {
      LogInfo::MapleLogger() << "[" << it.first << ":" << it.second << "],";
    }
    LogInfo::MapleLogger() << "\n";
  }
}

void GraphColorRegAllocator::PrintBbAssignInfo() const {
  for (uint32 id = 0; id < sortedBBs.size(); id++) {
    uint32 bbid = sortedBBs[id]->id;
    BbAssignInfo *bbInfo = bbRegInfo[bbid];
    if (bbInfo == nullptr) {
      continue;
    }
    LogInfo::MapleLogger() << "BBinfo(" << id << ")";
    LogInfo::MapleLogger() << " lra-needed int " << bbInfo->intLocalRegsNeeded;
    LogInfo::MapleLogger() << " fp " << bbInfo->fpLocalRegsNeeded;
    LogInfo::MapleLogger() << " greg-used ";
    for (regno_t regno = 0; regno < kNumPregBits; regno++) {
      if (bbInfo->globalsAssigned[regno]) {
        LogInfo::MapleLogger() << regno << ",";
      }
    }
    LogInfo::MapleLogger() << "\n";
  }
}

void GraphColorRegAllocator::CalculatePriority(LiveRange *lr) {
#ifdef RANDOM_PRIORITY
  lr->priority = 1 / rand();
  return;
#endif  // RANDOM_PRIORITY
  float pri = 0.0;
  uint32 bbNum = 0;
  FOREACH_BB_ARR_ELEM(lr->bmember, bbid)
    auto lu = lr->luMap.find(bbid);
    BB *bb = bbVec[bbid];
    bbNum++;
    uint32 useCnt = lu->second->defNum + lu->second->useNum;
    uint32 mult;
#ifdef USE_BB_FREQUENCY
    mult = bb->frequency;
#else   // USE_BB_FREQUENCY
    if (bb->loop) {
      mult = (uint32)pow(kLoopWeight, bb->loop->loopLevel);
    } else {
      mult = 1;
    }
#endif  // USE_BB_FREQUENCY
    pri += useCnt * mult;
  END_FOREACH_BB_ARR_ELEM
  if (bbNum) {
    lr->priority = log(pri) / bbNum;
  } else {
    lr->priority = 0.0;
  }
  return;
}

bool GraphColorRegAllocator::AllPredBBVisited(BB *bb) {
  bool isAllPredsVisited = true;
  for (MapleList<BB *>::iterator predIt = bb->preds.begin(); predIt != bb->preds.end(); ++predIt) {
    BB *predBb = *predIt;
    // See if pred bb is a loop back edge
    bool isBackEdge = false;
    for (MapleList<BB *>::iterator loopIt = predBb->loop_succs.begin(); loopIt != predBb->loop_succs.end(); ++loopIt) {
      BB *loopBb = *loopIt;
      if (loopBb == bb) {
        isBackEdge = true;
        break;
      }
    }
    if ((isBackEdge == false) && (visitedBBs[predBb->id] == false)) {
      isAllPredsVisited = false;
      break;
    }
  }
  for (MapleList<BB *>::iterator predIt = bb->eh_preds.begin(); predIt != bb->eh_preds.end(); ++predIt) {
    BB *predBb = *predIt;
    bool isBackEdge = false;
    for (MapleList<BB *>::iterator loopIt = predBb->loop_succs.begin(); loopIt != predBb->loop_succs.end(); ++loopIt) {
      BB *loopBb = *loopIt;
      if (loopBb == bb) {
        isBackEdge = true;
        break;
      }
    }
    if ((isBackEdge == false) && (visitedBBs[predBb->id] == false)) {
      isAllPredsVisited = false;
      break;
    }
  }
  return isAllPredsVisited;
}

// During live interval construction, bb has only one predecessor and/or one
// successor are stright line bb.  It can be considered to be a single large bb
// for the purpose of finding live interval.  This is to prevent extending live
// interval of registers unnecessarily when interleaving bb from other paths.
BB *GraphColorRegAllocator::MarkStraightLineBBInBFS(BB *bb) {
  while (1) {
    if (bb->succs.size() == 1 && bb->eh_succs.size() == 0) {
      BB *sbb = bb->succs.front();
      if (visitedBBs[sbb->id] == true) {
        break;
      }
      if (sbb->preds.size() == 1 && sbb->eh_preds.size() == 0) {
        sortedBBs.push_back(sbb);
        visitedBBs[sbb->id] = true;
        bb = sbb;
      } else {
        break;
      }
    } else {
      break;
    }
  }
  return bb;
}

BB *GraphColorRegAllocator::SearchForStraightLineBBs(BB *bb) {
  /* Special case for issue #1863.
   *   Switch cases containing a simple if(){bbs} break;
   * Try to group if and bbs together.
   */
  if (bb->succs.size() != kCondBrNum || bb->eh_succs.size() != 0) {
    return bb;
  }
  BB *sbb1 = bb->succs.front();
  BB *sbb2 = bb->succs.back();
  uint32 predSz1 = sbb1->preds.size();
  uint32 predSz2 = sbb2->preds.size();
  BB *candidateBb;
  if (predSz1 == 1 && predSz2 > kSwitchCaseNum) {
    candidateBb = sbb1;
  } else if (predSz2 == 1 && predSz1 > kSwitchCaseNum) {
    candidateBb = sbb2;
  } else {
    return bb;
  }
  CG_ASSERT(candidateBb->id < visitedBBs.size(), "index out of range in GCRA::SearchForStraightLineBBs");
  if (visitedBBs[candidateBb->id] == true) {
    return bb;
  }
  if (candidateBb->eh_preds.size() != 0) {
    return bb;
  }
  if (candidateBb->succs.size() != 1) {
    return bb;
  }

  sortedBBs.push_back(candidateBb);
  visitedBBs[candidateBb->id] = true;
  return MarkStraightLineBBInBFS(candidateBb);
}

void GraphColorRegAllocator::BFS(BB *curbb) {
  std::queue<BB *> worklist;
  worklist.push(curbb);
  CG_ASSERT(curbb->id < cgfunc_->NumBBs(), "GCRA::BFS visitedBBs overflow");
  CG_ASSERT(curbb->id < visitedBBs.size(), "index out of range in GCRA::BFS");
  visitedBBs[curbb->id] = true;
  do {
    BB *bb = worklist.front();
    sortedBBs.push_back(bb);
    CG_ASSERT(bb->id < cgfunc_->NumBBs(), "GCRA::BFS visitedBBs overflow");
    visitedBBs[bb->id] = true;
    worklist.pop();
    // Look for straight line bb
    bb = MarkStraightLineBBInBFS(bb);
    // Look for an 'if' followed by some straight-line bb
    bb = SearchForStraightLineBBs(bb);
    for (MapleList<BB *>::iterator it = bb->succs.begin(); it != bb->succs.end(); ++it) {
      BB *ibb = *it;
      // See if there are unvisited predecessor
      if (visitedBBs[ibb->id] == false) {
        if (AllPredBBVisited(ibb) == true) {
          worklist.push(ibb);
          CG_ASSERT(ibb->id < cgfunc_->NumBBs(), "GCRA::BFS visitedBBs overflow");
          visitedBBs[ibb->id] = true;
        }
      }
    }
  } while (!worklist.empty());
  return;
}

void GraphColorRegAllocator::PrintBBs() const {
  for (uint32_t i = 0; i < sortedBBs.size(); i++) {
    LogInfo::MapleLogger() << "\n< === > ";
    LogInfo::MapleLogger() << sortedBBs[i]->id;
    LogInfo::MapleLogger() << " succs:";
    MapleList<BB *> succs = sortedBBs[i]->succs;
    for (MapleList<BB *>::iterator it = succs.begin(); it != succs.end(); it++) {
      BB *succBb = static_cast<BB *>(*it);
      LogInfo::MapleLogger() << " " << succBb->id;
    }
    LogInfo::MapleLogger() << " eh_succs:";
    succs = sortedBBs[i]->eh_succs;
    for (MapleList<BB *>::iterator it = succs.begin(); it != succs.end(); it++) {
      BB *succBb = static_cast<BB *>(*it);
      LogInfo::MapleLogger() << " " << succBb->id;
    }
  }
  LogInfo::MapleLogger() << "\n";
  return;
}

void GraphColorRegAllocator::ComputeBlockOrder() {
  visitedBBs.clear();
  sortedBBs.clear();
  visitedBBs.resize(cgfunc_->NumBBs());
  for (uint32_t i = 0; i < cgfunc_->NumBBs(); i++) {
    visitedBBs[i] = false;
  }
  BB *cleanupBb = nullptr;
  FOR_ALL_BB(bb, cgfunc_) {
    bb->internal_flag1 = 0;
    if (bb->firststmt == cgfunc_->cleanup_label) {
      cleanupBb = bb;
    }
  }
  for (BB *bb = cleanupBb; bb; bb = bb->next) {
    bb->internal_flag1 = 1;
  }

  bool changed = false;
  uint32 sortedCnt = 0;
  bool done = false;
  do {
    if (GCRA_DUMP) {
      LogInfo::MapleLogger() << "BFS iteration " << sortedBBs.size() << " " << cgfunc_->NumBBs() << "\n";
    }
    changed = false;
    FOR_ALL_BB(bb, cgfunc_) {
      if (bb->internal_flag1 == 1) {
        continue;
      }
      if (visitedBBs[bb->id] == false) {
        changed = true;
        if (AllPredBBVisited(bb) == true) {
          BFS(bb);
        }
      }
    }
    // Make sure there is no infinite loop.
    if (sortedCnt == sortedBBs.size()) {
      if (done == false) {
        done = true;
      } else {
        LogInfo::MapleLogger() << "Error: GCRA BFS loop " << sortedCnt << " in func " << cgfunc_->GetName() << "\n";
      }
    }
    sortedCnt = sortedBBs.size();
  } while (changed == true);

  for (BB *bb = cleanupBb; bb; bb = bb->next) {
    sortedBBs.push_back(bb);
  }

  if (GCRA_DUMP) {
    PrintBBs();
  }
}

uint32 GraphColorRegAllocator::MaxIntPhysRegNum() {
  return (R31 - R0);
}

uint32 GraphColorRegAllocator::MaxFloatPhysRegNum() {
  return (V30 - V0);
}

// ==== int regs ====
// 0 ZR (zero register)
// 1 RA
// 2 SP
// 3 GP
// 4 TP
// 5 to 7                caller
// 8 FP
// 9                     callee
// 10 to 17 parameters   caller (10, 11 return values)
// 18 to 27              callee
// 28 to 31              caller
// ==== fp regs ====
// 0 to 7                caller
// 8 to 9                callee
// 10 to 17              caller (10, 11 return values)
// 18 to 27              callee
// 28 to 31              caller

void GraphColorRegAllocator::InitFreeRegPool() {
  uint32 intNum = 0;
  uint32 fpNum = 0;
  // int caller
  for (uint32 i = R5 - R0; i <= R7 - R0; i++) {
    intCallerRegSet.insert(i);
  }
  intNum += 3;
  for (uint32 i = R28 - R0; i <= R31 - R0; i++) {
    intCallerRegSet.insert(i);
  }
  intNum += 4;
  // int callee
  intCalleeRegSet.insert(R9 - R0);
  for (uint32 i = R18 - R0; i <= R27 - R0; i++) {
    intCalleeRegSet.insert(i);
  }
  intNum += 11;
  // fp caller
  for (uint32 i = V0 - V0; i <= V7 - V0; i++) {
    fpCallerRegSet.insert(i);
  }
  fpNum += 8;
#ifdef RESERVED_REGS
  regno_t lastCaller = V30;
  fpNum += 3;
#else
  regno_t lastCaller = V31;
  fpNum += 4;
#endif
  for (uint32 i = V28 - V0; i <= lastCaller - V0; i++) {
    fpCallerRegSet.insert(i);
  }
  // fp callee
  for (uint32 i = V8 - V0; i <= V9 - V0; i++) {
    fpCalleeRegSet.insert(i);
  }
  for (uint32 i = V18 - V0; i <= V27 - V0; i++) {
    fpCalleeRegSet.insert(i);
  }
  fpNum += 12;

  // Add any physical registers that are not used.
  // R10 to R17 for integer, V10-V17 for float
  for (uint32 i = R10 - R0; i < (kMaxParamNum + R10 - R0); i++) {
    intCallerRegSet.insert(i);
  }
  intNum += kMaxParamNum;
  for (uint32 i = V10 - V0; i <= (kMaxParamNum + V10 - V0); i++) {
    fpCallerRegSet.insert(i);
  }
  fpNum += kMaxParamNum;

  intRegNum = intNum;
  fpRegNum = fpNum;
}

bool GraphColorRegAllocator::IsUnconcernedReg(regno_t regno) {
  switch (regno) {
  case R0:  // ZERO
  case R1:  // RA
  case R2:  // SP
  case R3:  // GP
  case R4:  // TP
  case R8:  // FP
    return true;
  default:
    break;
  }

  // when yieldpoint is enabled, the RYP(x19) can not be used.
  if (cgfunc_->cg->GenYieldpoint() && regno == RYP) {
    return true;
  }

  return false;
}

bool GraphColorRegAllocator::IsUnconcernedReg(RegOperand *regOpnd) {
  uint32 regno = regOpnd->GetRegisterNumber();
  RegType regtype = regOpnd->GetRegisterType();
  if (regtype == kRegTyCc || regtype == kRegTyVary) {
    return true;
  }
  if (regOpnd->IsConstReg()) {
    return true;
  }
  return IsUnconcernedReg(regno);
}

//  Based on live analysis, the live-in and live-out set determines
//  the bit to be set in the LR vector, which is of size #BBs.
//  If a vreg is in the live-in and live-out set, it is live in the BB.
//
//  Also keep track if a LR crosses a call.  If a LR crosses a call, it
//  interferes with all caller saved registers.  Add all caller registers
//  to the LR's forbidden list.
//
//  Return true if LR created, else return false.
//
//  maybe need extra info:
//  Add info for setjmp.
//  Add info for defBB, useBB, index in BB for def and use
//  Add info for startingBB and endingBB
LiveRange *GraphColorRegAllocator::NewLiveRange() {
  LiveRange *lr = cgfunc_->memPool->New<LiveRange>(allocator);

  if (bbBuckets == 0) {
    bbBuckets = (cgfunc_->NumBBs() / (sizeof(uint64) * CHAR_BIT)) + 1;
  }
  lr->bmember = cgfunc_->memPool->NewArray<uint64>(bbBuckets);
  memset_s(lr->bmember, bbBuckets * sizeof(uint64), 0, bbBuckets * sizeof(uint64));
  if (regBuckets == 0) {
    regBuckets = (cgfunc_->GetMaxRegNum() / (sizeof(uint64) * CHAR_BIT)) + 1;
  }
  lr->bconflict = cgfunc_->memPool->NewArray<uint64>(regBuckets);
  memset_s(lr->bconflict, regBuckets * sizeof(uint64), 0, regBuckets * sizeof(uint64));

  lr->pregveto.clear();
  lr->pregveto.resize(kNumPregBits);
  lr->forbidden.clear();
  lr->forbidden.resize(kNumPregBits);

  return lr;
}

// Create local info for LR.  return true if reg is not local.
bool GraphColorRegAllocator::CreateLiveRangeHandleLocal(regno_t regno, BB *bb, bool isDef) {
  if (FIND_NOT_IN(bb->liveout_regno, regno) && FIND_NOT_IN(bb->livein_regno, regno)) {
    // register not in globals for the bb, so it is local.
    // Compute local RA info.
    LocalRaInfo *lraInfo = localRegVec[bb->id];
    if (lraInfo == nullptr) {
      lraInfo = cgfunc_->memPool->New<LocalRaInfo>(allocator);
      localRegVec[bb->id] = lraInfo;
    }
    if (isDef) {
      // movk is handled by different id for use/def in the same insn.
      lraInfo->defCnt[regno]++;
      //lraInfo->localPregMask |= (1LL << regno);
    } else {
      lraInfo->useCnt[regno]++;
      //lraInfo->localPregMask |= (1LL << regno);
    }
    // lr info is useful for lra, so continue lr info
  } else if (regno < kMaxRegNum) {
    // This is a cross bb physical reg
    LocalRaInfo *lraInfo = localRegVec[bb->id];
    if (lraInfo == nullptr) {
      lraInfo = cgfunc_->memPool->New<LocalRaInfo>(allocator);
      localRegVec[bb->id] = lraInfo;
    }
    //lraInfo->globalPreg.insert(regno);
    //lraInfo->globalPregMask |= (1LL << regno);
  } else {
    return true;
  }
  return false;
}

LiveRange *GraphColorRegAllocator::CreateLiveRangeAllocateAndUpdate(regno_t regno, BB *bb, bool isDef, uint32 currId) {
  LiveRange *lr;
  if (lrVec[regno] == nullptr) {
    lr = NewLiveRange();
    lr->regno = regno;
    lr->id = currId;

    LiveUnit *lu = cgfunc_->memPool->New<LiveUnit>();
    lr->luMap[bb->id] = lu;
    lu->bgn = lu->end = currId;
    if (isDef) {
      LogInfo::MapleLogger() << "no use after def for regno:" << regno << endl;
      for (auto pregNo : pregLive) {
        lr->insertPregveto(pregNo);
      }
    }
  } else {
    lr = lrVec[regno];

    LiveUnit *lu = lr->luMap[bb->id];
    if (lu == nullptr) {
      lu = cgfunc_->memPool->New<LiveUnit>();
      lr->luMap[bb->id] = lu;
      lu->bgn = lu->end = currId;
    }
    if (lu->bgn > currId) {
      lu->bgn = currId;
    }
    if (lu->end < currId) {
      lu->end = currId;
    }
  }

  return lr;
}

bool GraphColorRegAllocator::CreateLiveRange(regno_t regno, BB *bb, bool isDef, bool isMovk, uint32 currId,
                                             bool updateCount) {
  bool isNonLocal = CreateLiveRangeHandleLocal(regno, bb, isDef);

  if (!isDef) {
    currId--;
  }

  LiveRange *lr = CreateLiveRangeAllocateAndUpdate(regno, bb, isDef, currId);
  lr->isNonLocal = isNonLocal;
  if (isDef) {
    if (isMovk == false) {
      vregLive.erase(regno);
    }
#ifdef OPTIMIZE_FOR_PROLOG
    if (doOptProlog && updateCount) {
      if (lr->numDefs == 0) {
        lr->frequency += bb->frequency;
      }
      lr->numDefs++;
    }
#endif  // OPTIMIZE_FOR_PROLOG
  } else {
    vregLive.insert(regno);
#ifdef OPTIMIZE_FOR_PROLOG
    if (doOptProlog && updateCount) {
      if (lr->numUses == 0) {
        lr->frequency += bb->frequency;
      }
      lr->numUses++;
    }
#endif  // OPTIMIZE_FOR_PROLOG
  }
  for (auto pregNo : pregLive) {
    lr->insertPregveto(pregNo);
  }

  // only handle it in live_in and def point?
  SET_MEMBER_BIT_ARR_ELEM(lr, bb->id);

  lrVec[regno] = lr;

  return true;
}

bool GraphColorRegAllocator::SetupLiveRangeByOpHandlePhysicalReg(RegOperand *regOpnd, Insn *insn, regno_t regno,
                                                                 bool isDef) {
  if (regOpnd->IsPhysicalRegister()) {
    LocalRaInfo *lraInfo = nullptr;
    lraInfo = localRegVec[insn->bb->id];
    if (lraInfo == nullptr) {
      lraInfo = cgfunc_->memPool->New<LocalRaInfo>(allocator);
      localRegVec[insn->bb->id] = lraInfo;
    }

    if (isDef) {
      pregLive.erase(regno);

      if (lraInfo) {
        lraInfo->defCnt[regno] = lraInfo->defCnt[regno] + 1;
      }
    } else {
      pregLive.insert(regno);
      for (auto vregno : vregLive) {
        if (IsUnconcernedReg(vregno)) {
          continue;
        }
        LiveRange *lr = lrVec[vregno];
        lr->insertPregveto(regno);
      }

      if (lraInfo) {
        lraInfo->useCnt[regno]++;
      }
    }
    return true;
  }
  return false;
}

// add pregs to forbidden list of lr. If preg is in
// the live list, then it is forbidden for other vreg on the list.
// Return kImmediateDelete or kDelayedDelete if insn involved should be deleted.
//   if return value is kImmediateDelete, then deletion can be immediate
//   if return value is kDelayedDelete, then deletion needs to be done after
//   finish processing the instruction.
uint8 GraphColorRegAllocator::SetupLiveRangeByOp(Operand *op, Insn *insn, bool isDef) {
  if (!op->IsRegister()) {
    return kNoDelete;
  }
  RegOperand *regOpnd = static_cast<RegOperand *>(op);
  uint32 regno = regOpnd->GetRegisterNumber();
  if (IsUnconcernedReg(regOpnd)) {
    if (lrVec[regno]) {
      CG_ASSERT(0, "Unconcerned reg");
      lrVec[regno] = nullptr;
    }
    return kNoDelete;
  }
  if (SetupLiveRangeByOpHandlePhysicalReg(regOpnd, insn, regno, isDef)) {
    return kNoDelete;
  }
#ifdef REMOVE_INSN
  if (isDef) {
    if (lrVec[regno] == nullptr) {
      return kImmediateDelete;
    } else if (CGOptions::doLiveAnalysisEh && FIND_NOT_IN(vregLive,regno)) {
      return kDelayedDelete;
    }
  }
#endif  // REMOVE_INSN
  if (CreateLiveRange(regno, insn->bb, isDef, false, insn->id, true)) {
    LiveRange *lr = lrVec[regno];
    if (lr->regtype == kRegTyUndef) {
      lr->regtype = regOpnd->GetRegisterType();
    }
    if (isDef) {
      lr->luMap[insn->bb->id]->defNum++;
    } else {
      lr->luMap[insn->bb->id]->useNum++;
    }
#ifdef MOVE_COALESCE
    if (insn->GetMachineOpcode() == MOP_xmovrr || insn->GetMachineOpcode() == MOP_wmovrr) {
      RegOperand *opnd = static_cast<RegOperand *>(insn->GetOperand(1));
      if (opnd->GetRegisterNumber() < kMaxRegNum) {
        lr->prefs.insert(opnd->GetRegisterNumber() - R0);
      }
    }
#endif  //  MOVE_COALESCE
  }
  return kNoDelete;
}

// handle live range for bb->live_out
void GraphColorRegAllocator::SetupLiveRangeByRegno(regno_t lout, BB *bb, uint32 currPoint) {
  if (IsUnconcernedReg(lout)) {
    return;
  }
  if (lout < kMaxRegNum) {
    pregLive.insert(lout);
    for (auto vregno : vregLive) {
      LiveRange *lr = lrVec[vregno];
      lr->insertPregveto(lout);

      // See if phys reg is livein also. Then assume it span the entire bb.
      if (FIND_IN(bb->livein_regno, lout)) {
        LocalRaInfo *lraInfo = localRegVec[bb->id];
        if (lraInfo == nullptr) {
          lraInfo = cgfunc_->memPool->New<LocalRaInfo>(allocator);
          localRegVec[bb->id] = lraInfo;
        }
        // Make it a large enough so no locals can be allocated.
        lraInfo->useCnt[lout] = kLargeUint16;
      }
    }
    return;
  }
  vregLive.insert(lout);
  CreateLiveRange(lout, bb, false, false, currPoint, false);
  return;
}

void GraphColorRegAllocator::ComputeLiveRangesForEachOperand(Insn *insn) {
  int32 delInsn = -1;
  const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->GetMachineOpcode()];
  for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
    Operand *opnd = insn->GetOperand(i);
    if (opnd == nullptr) {
      continue;
    }
    bool isDef = static_cast<Riscv64OpndProp *>(md->operand_[i])->IsRegDef();
    if (opnd->IsList()) {
      ListOperand *listopnd = static_cast<ListOperand *>(opnd);
      for (auto op : listopnd->GetOperands()) {
        SetupLiveRangeByOp(op, insn, isDef);
      }
    } else if (opnd->IsMemoryAccessOperand()) {
      MemOperand *memopnd = static_cast<MemOperand *>(opnd);
      Operand *base = memopnd->GetBaseRegister();
      Operand *offset = memopnd->GetIndexRegister();
      isDef = false;
      if (base != nullptr) {
        SetupLiveRangeByOp(base, insn, isDef);
      }
      if (offset != nullptr) {
        SetupLiveRangeByOp(offset, insn, isDef);
      }
    } else {
      uint8 del = SetupLiveRangeByOp(opnd, insn, isDef);
      /* -1: unknown.
          0: def liveout, cannot delete
          1: possible delete, unless other def is used
       */
      if (isDef) {
        if (delInsn != 0) {
          if (del == kNoDelete) {
            delInsn = 0;
          } else if (del == kDelayedDelete) {
            delInsn = 1;
          } else if (del == kImmediateDelete) {
            delInsn = 1;
            //break;
          }
        }
      } else if (delInsn == 1 && opnd->IsRegister()) {
        regno_t vreg = (static_cast<RegOperand *>(opnd))->GetRegisterNumber();
        if (vreg > kMaxRegNum) {
          lrVec[vreg]->luMap[insn->bb->id]->useNum--;
        }
        LocalRaInfo *lraInfo = localRegVec[insn->bb->id];
        if (lraInfo) {
          if (lraInfo->useCnt.find(vreg) != lraInfo->useCnt.end()) {
            lraInfo->useCnt[vreg]--;
          }
        }
      }
    }
  }
  if (delInsn == 1) {
    if (GCRA_DUMP) {
      LogInfo::MapleLogger() << "Remove insn: ";
      insn->dump();
    }
    insn->bb->RemoveInsn(insn);
  }
}

void GraphColorRegAllocator::ComputeLiveRangesUpdateIfInsnIsCall(Insn *insn) {
  if (insn->IsCall()) {
    for (auto vregno : vregLive) {
      LiveRange *lr = lrVec[vregno];
      lr->numCall++;

      auto lu = lr->luMap.find(insn->bb->id);
      if (lu != lr->luMap.end()) {
        lu->second->hasCall = true;
      }
    }
    // def the return value
    pregLive.erase(R10);
    pregLive.erase(V10);

    // active the parametes
    Operand *opnd1 = insn->opnds[1];
    if (opnd1->IsList()) {
      Riscv64ListOperand *srcopnds = static_cast<Riscv64ListOperand *>(opnd1);
      for (auto regopnd : srcopnds->GetOperands()) {
        CG_ASSERT(!regopnd->IsVirtualRegister(), "");
        Riscv64reg_t physicalReg = (Riscv64reg_t)regopnd->GetRegisterNumber();
        pregLive.insert(physicalReg);
      }
    }
  }
}

void GraphColorRegAllocator::ComputeLiveRangesUpdateLiveUnitInsnRange(BB *bb, uint32 currPoint) {
  for (auto lin : bb->livein_regno) {
    if (lin < kMaxRegNum) {
      continue;
    }
    LiveRange *lr = lrVec[lin];
    if (lr == nullptr) {
      continue;
    }
    auto lu = lr->luMap.find(bb->id);
    if (bb->firstinsn) {
      lu->second->bgn = bb->firstinsn->id;
    } else {
      // since bb is empty, then use pointer as is
      lu->second->bgn = currPoint;
    }
    lu->second->bgn = lu->second->bgn - 1;
  }
}

void GraphColorRegAllocator::ComputeLiveRangesBmemberSize() {
  for (uint32_t i = 0; i < lrVec.size(); i++) {
    LiveRange *lr = lrVec[i];
    if (lr == nullptr || lr->regno == 0) {
      continue;
    }
  }
}

void GraphColorRegAllocator::ComputeLiveOut(BB *bb) {
  vregLive.clear();
  pregLive.clear();
  // No loop backedge
  for (auto succ : bb->succs) {
    if (FIND_IN(bb->loop_succs, succ)) {
      continue;
    }
    for (auto regno : succ->livein_regno) {
      if (IsUnconcernedReg(regno)) {
        continue;
      }
      if (regno < kMaxRegNum) {
        pregLive.insert(regno);
        continue;
      }
      vregLive.insert(regno);
    }
  }
  for (auto succ : bb->eh_succs) {
    if (FIND_IN(bb->loop_succs, succ)) {
      continue;
    }
    for (auto regno : succ->livein_regno) {
      if (IsUnconcernedReg(regno)) {
        continue;
      }
      if (regno < kMaxRegNum) {
        pregLive.insert(regno);
        continue;
      }
      vregLive.insert(regno);
    }
  }
  // With instruction scheduling, other instructions might be placed between
  // definition of return register and return block.  Need to preserve return reg.
  if (bb->HasCall() == false && bb->succs.size() == 1) {
    Riscv64reg_t regno = static_cast<Riscv64CGFunc *>(cgfunc_)->GetReturnRegisterNumber();
    if (regno != INVALID_REGNO) {
      BB *succBB = bb->succs.front();
      if (succBB->HasReturn()) {
        // Prevent local reg allocator using this return register.
        SetBbInfoGlobalAssigned(bb->id, regno);
        // Create proper live range for global reg allocator.
        pregLive.insert(regno);
      }
    }
  }
}

#define UPDATE_INSN_CNT_AND_SKIP_USELESS(insn) \
  insn->id = currPoint;                        \
  if (insn->IsImmaterialInsn()) {              \
    currPoint--;                               \
    continue;                                  \
  }                                            \
  if (!insn->IsMachineInstruction()) {         \
    currPoint--;                               \
    continue;                                  \
  }

// For each succ bb->succs, if bb->liveout - succ->livein is not empty, the vreg(s) is
// dead on this path (but alive on the other path as there is some use of it on the
// other path).  This might be useful for optimization of reload placement later for
// splits (lr split into lr1 & lr2 and lr2 will need to reload.)
// Not for now though.
void GraphColorRegAllocator::ComputeLiveRanges() {
  bbVec.clear();
  bbVec.resize(cgfunc_->NumBBs());

  uint32 currPoint = cgfunc_->GetTotalNumberOfInstructions() + sortedBBs.size();
  currPoint *= 2;
  for (uint32_t bbIdx = sortedBBs.size(); bbIdx > 0; bbIdx--) {
    BB *bb = sortedBBs[bbIdx - 1];
    bbVec[bb->id] = bb;
    bb->level = bbIdx - 1;

    ComputeLiveOut(bb);
    for (auto lout : bb->liveout_regno) {
      SetupLiveRangeByRegno(lout, bb, currPoint);
    }
    currPoint--;

    FOR_BB_INSNS_REV_SAFE(insn, bb, ninsn) {
      UPDATE_INSN_CNT_AND_SKIP_USELESS(insn);

      ComputeLiveRangesForEachOperand(insn);

      ComputeLiveRangesUpdateIfInsnIsCall(insn);
      // distinguish use/def
      currPoint -= 2;
    }
    ComputeLiveRangesUpdateLiveUnitInsnRange(bb, currPoint);
    // move one more step for each BB
    currPoint--;
  }

  ComputeLiveRangesBmemberSize();

  if (GCRA_DUMP) {
    LogInfo::MapleLogger() << "After ComputeLiveRanges\n";
    PrintLiveRanges();
#ifdef USE_LRA
    if (doLRA) {
      PrintLocalRaInfo("After ComputeLiveRanges");
    }
#endif  // USE_LRA
  }
  return;
}

#undef UPDATE_INSN_CNT_AND_SKIP_USELESS

MemOperand *GraphColorRegAllocator::CreateSpillMem(uint32 spillIdx) {
  // Create a common stack space for spilling with need_spill
  if (spillIdx == 0) {
    if (spillMemopnd0 == nullptr) {
      regno_t reg = cgfunc_->New_V_Reg(kRegTyInt, sizeof(long long));
      Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc_);
      spillMemopnd0 = a64cgfunc->GetOrCreatSpillMem(reg);
    }
    return spillMemopnd0;
  } else if (spillIdx == 1) {
    if (spillMemopnd1 == nullptr) {
      regno_t reg = cgfunc_->New_V_Reg(kRegTyInt, sizeof(long long));
      Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc_);
      spillMemopnd1 = a64cgfunc->GetOrCreatSpillMem(reg);
    }
    return spillMemopnd1;
  } else if (spillIdx == 2) {
    if (spillMemopnd2 == nullptr) {
      regno_t reg = cgfunc_->New_V_Reg(kRegTyInt, sizeof(long long));
      Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc_);
      spillMemopnd2 = a64cgfunc->GetOrCreatSpillMem(reg);
    }
    return spillMemopnd2;
  }
  return nullptr;
}

bool GraphColorRegAllocator::IsLocalReg(regno_t regno) {
  LiveRange *lr = lrVec[regno];
  if (lr == nullptr) {
    return true;
  }
  if (lr->splitLr) {
    return false;
  } else {
    return (lr->numBmembers == 1) && (!lr->isNonLocal);
  }
}

bool GraphColorRegAllocator::IsLocalReg(LiveRange *lr) {
  if (lr->splitLr) {
    return false;
  } else {
    return (lr->numBmembers == 1) && (!lr->isNonLocal);
  }
}

void GraphColorRegAllocator::CheckInterference(LiveRange *lr1, LiveRange *lr2) {
//  uint64 bitArr[bbBuckets];
//  BIT_ARR_AND(lr1->bmember, lr2->bmember, bitArr, bbBuckets);
  uint32 lastBitSet;
  uint32 overlapNum = 0;
  bool stop = false;
  for (uint32 i = 0; i < bbBuckets; i++) {
    uint64 val = lr1->bmember[i] & lr2->bmember[i];
    if (val) {
      for (uint32 x = 0; x < (sizeof(uint64) * CHAR_BIT); x++) {
        if (val & (1LL << x)) {
          overlapNum++;
          lastBitSet = i * sizeof(uint64) * CHAR_BIT + x;
          if (overlapNum > 1) {
            stop = true;
            break;
          }
        }
      }
      if (stop) {
        break;
      }
    }
  }
  if (overlapNum == 0) {
    // no interference
  } else if (overlapNum == 1) {
    // bgn and end should be in the bb info (LU)
    // Need to rethink this if.
    // Under some circumstance, lr->bgn can occur after lr->end.
    auto lu1 = lr1->luMap.find(lastBitSet);
    auto lu2 = lr2->luMap.find(lastBitSet);
    if (lu1 != lr1->luMap.end() && lu2 != lr2->luMap.end() &&
        !((lu1->second->bgn < lu2->second->bgn && lu1->second->end < lu2->second->bgn) ||
          (lu2->second->bgn < lu1->second->end && lu2->second->end < lu1->second->bgn))) {
      SET_CONFLICT_BIT_ARR_ELEM(lr1, lr2->regno);
      SET_CONFLICT_BIT_ARR_ELEM(lr2, lr1->regno);
    }
  } else {
    // interfere
    SET_CONFLICT_BIT_ARR_ELEM(lr1, lr2->regno);
    SET_CONFLICT_BIT_ARR_ELEM(lr2, lr1->regno);
  }

  return;
}

void GraphColorRegAllocator::BuildInterferenceGraphSeparateIntFp(std::vector<LiveRange *> &intLrVec,
                                                                 std::vector<LiveRange *> &fpLrVec) {
  for (uint32_t i = 0; i < lrVec.size(); i++) {
    if (lrVec[i] == nullptr || lrVec[i]->regno == 0) {
      continue;
    }
#ifdef USE_LRA
    if (doLRA && lrVec[i]->numBmembers == 1)
    {
      continue;
    }
#endif  // USE_LRA
    if (lrVec[i]->regtype == kRegTyInt) {
      intLrVec.push_back(lrVec[i]);
    } else if (lrVec[i]->regtype == kRegTyFloat) {
      fpLrVec.push_back(lrVec[i]);
    } else {
      CG_ASSERT(0, "Illegal regtype in BuildInterferenceGraph");
      LogInfo::MapleLogger() << "error: Illegal regtype in BuildInterferenceGraph\n";
    }
  }
}

//  Based on intersection of LRs.  When two LRs interfere, add to each other's
//  interference list.
void GraphColorRegAllocator::BuildInterferenceGraph() {
  std::vector<LiveRange *> intLrVec;
  std::vector<LiveRange *> fpLrVec;
  BuildInterferenceGraphSeparateIntFp(intLrVec, fpLrVec);

  int sz = intLrVec.size();

  // Checking interferences among LVs consumes significant amount of time.
  // Take advantage of the fact that a large portion of LVs are short-lived
  // Delay to do the same to FP
  std::vector<int32> idxLastBucket(sz);
  for (int i = 0; i < sz; i++) {
    uint32 count = 0;
    uint32 lastPos;
    LiveRange *lr =  intLrVec[i];
    for (int j = 0; j < bbBuckets; j++) {
      if (lr->bmember[j]) {
        count++;
        lastPos = j;
      }
    }
    if (count == 1) {
      idxLastBucket[i] = lastPos;
    } else {
      idxLastBucket[i] = -1;
    }
  }

  for (int i = 0; i < sz; i++) {
    LiveRange *lr1 = intLrVec[i];
    CalculatePriority(lr1);
    int32 iLastBucketIdx = idxLastBucket[i];

    for (int j = i + 1; j < sz; j++) {
      int32 jLastBucketIdx = idxLastBucket[j];
      LiveRange *lr2 = intLrVec[j];
      if (lr1->regno < lr2->regno ) {
        if (iLastBucketIdx == -1 && jLastBucketIdx == -1) {
          CheckInterference(lr1, lr2);
        } else if ((iLastBucketIdx >= 0 && lr1->bmember[iLastBucketIdx] & lr2->bmember[iLastBucketIdx]) ||
                   (jLastBucketIdx >= 0 && lr1->bmember[jLastBucketIdx] & lr2->bmember[jLastBucketIdx])) {
          CheckInterference(lr1, lr2);
        }
      }
    }
  }

  sz = fpLrVec.size();
  for (int i = 0; i < sz; i++) {
    LiveRange *lr1 = fpLrVec[i];
    CalculatePriority(lr1);
    for (int j = i + 1; j < sz; j++) {
      LiveRange *lr2 = fpLrVec[j];
      if (lr1->regno < lr2->regno) {
        CheckInterference(lr1, lr2);
      }
    }
  }

  if (GCRA_DUMP) {
    LogInfo::MapleLogger() << "After BuildInterferenceGraph\n";
    PrintLiveRanges();
  }

  return;
}

void GraphColorRegAllocator::SetBbInfoGlobalAssigned(uint32 bbid, regno_t regno) {
  BbAssignInfo *bbInfo = bbRegInfo[bbid];
  if (bbInfo == nullptr) {
    bbInfo = cgfunc_->memPool->New<BbAssignInfo>(allocator);
    bbRegInfo[bbid] = bbInfo;
    bbInfo->globalsAssigned.clear();
    bbInfo->globalsAssigned.resize(kNumPregBits);
  }
  bbInfo->insertGlobalsAssigned(regno);
}

bool GraphColorRegAllocator::HaveAvailableColor(LiveRange *lr, uint32 num) {
  return ((lr->regtype == kRegTyInt && num < intRegNum) || (lr->regtype == kRegTyFloat && num < fpRegNum));
}

//  If the members on the interference list is less than #colors, then
//  it can be trivially assigned a register.  Otherwise it is constrained.
//  Separate the LR based on if it is contrained or not.
//
//  The unconstrained LRs are colored last.
//
//  Compute a sorted list of constrained LRs based on priority cost.
void GraphColorRegAllocator::Separate() {
  for (auto lr : lrVec) {
    if (lr == nullptr) {
      continue;
    }
#ifdef USE_LRA
    if (doLRA && IsLocalReg(lr)) {
      continue;
    }
#endif  // USE_LRA
#ifdef OPTIMIZE_FOR_PROLOG
    if (doOptProlog &&
        (lr->numDefs <= 1 && lr->numUses <= 1 && lr->numCall > 0) &&
        (lr->frequency <= (cgfunc_->firstbb->frequency << 1))) {
      if (lr->regtype == kRegTyInt) {
        intDelayed.push_back(lr);
      } else {
        fpDelayed.push_back(lr);
      }
      continue;
    }
#endif  // OPTIMIZE_FOR_PROLOG
    if (HaveAvailableColor(lr, lr->numBconflicts + lr->numPregveto + lr->numForbidden))
    {
      unconstrained.push_back(lr);
    } else {
      constrained.push_back(lr);
    }
  }
  if (GCRA_DUMP) {
    LogInfo::MapleLogger() << "Unconstrained : ";
    for (auto lr : unconstrained) {
      LogInfo::MapleLogger() << lr->regno << " ";
    }
    LogInfo::MapleLogger() << "\n";
    LogInfo::MapleLogger() << "Constrained : ";
    for (auto lr : constrained) {
      LogInfo::MapleLogger() << lr->regno << " ";
    }
    LogInfo::MapleLogger() << "\n";
  }
}

MapleVector<LiveRange *>::iterator GraphColorRegAllocator::GetHighPriorityLr(MapleVector<LiveRange *> &lrSet) {
  MapleVector<LiveRange *>::iterator it = lrSet.begin();
  MapleVector<LiveRange *>::iterator highestIt = it;
  LiveRange *startLr = *it;
  float maxPrio = startLr->priority;
  it++;
  for (; it != lrSet.end(); it++) {
    LiveRange *lr = *it;
    if (lr->priority > maxPrio) {
      maxPrio = lr->priority;
      highestIt = it;
    }
  }
  return highestIt;
}

void GraphColorRegAllocator::UpdateForbiddenForNeighbors(LiveRange *lr) {
  FOREACH_REG_ARR_ELEM(lr->bconflict, regno)
    LiveRange *newLr = lrVec[regno];
    if (newLr->pregveto[lr->assigned] == false) {
      newLr->insertForbidden(lr->assigned);
    }
  END_FOREACH_REG_ARR_ELEM
  return;
}

void GraphColorRegAllocator::UpdatePregvetoForNeighbors(LiveRange *lr) {
  FOREACH_REG_ARR_ELEM(lr->bconflict, regno)
    LiveRange *newLr = lrVec[regno];
    newLr->insertPregveto(lr->assigned);
  END_FOREACH_REG_ARR_ELEM
  return;
}

// For cases with only one def/use and crosses a call.
// It might be more beneficial to spill vs save/restore in prolog/epilog.
// But if the callee register is already used, then it is ok to reuse it again.
// Or in certain cases, just use the callee.
bool GraphColorRegAllocator::ShouldUseCallee(LiveRange *lr, MapleSet<regno_t> &calleeUsed,
                                             MapleVector<LiveRange *> &delayed) {
  if (FIND_IN(calleeUsed, lr->assigned)) {
    return true;
  } else if (Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)(lr->assigned)) && calleeUsed.size() % 2 != 0) {
    return true;
  } else if (delayed.size() > 1 && calleeUsed.size() == 0) {
    // If there are more than 1 vreg that can benefit from callee, use callee
    return true;
  } else {
    lr->assigned = 0;
    return false;
  }
}

regno_t GraphColorRegAllocator::FindColorForLr(LiveRange *lr) {
  regno_t base;
  regno_t reg = 0;
  RegType regtype = lr->regtype;
  MapleSet<uint32> *currRegSet;
  MapleSet<uint32> *nextRegSet;
  if (regtype == kRegTyInt) {
    if (lr->numCall != 0) {
      currRegSet = intCalleeRegSet.size() ? &intCalleeRegSet : &intCallerRegSet;
      nextRegSet = intCallerRegSet.size() ? &intCallerRegSet : &intCalleeRegSet;
    } else {
      currRegSet = intCallerRegSet.size() ? &intCallerRegSet : &intCalleeRegSet;
      nextRegSet = intCalleeRegSet.size() ? &intCalleeRegSet : &intCallerRegSet;
    }
    base = R0;
  } else {
    if (lr->numCall != 0) {
      currRegSet = fpCalleeRegSet.size() ? &fpCalleeRegSet : &fpCallerRegSet;
      nextRegSet = fpCallerRegSet.size() ? &fpCallerRegSet : &fpCalleeRegSet;
    } else {
      currRegSet = fpCallerRegSet.size() ? &fpCallerRegSet : &fpCalleeRegSet;
      nextRegSet = fpCalleeRegSet.size() ? &fpCalleeRegSet : &fpCallerRegSet;
    }
    base = V0;
  }
#ifdef MOVE_COALESCE
  for (auto it : lr->prefs) {
    reg = it + base;
    if ((FIND_IN(*currRegSet, reg) || FIND_IN(*nextRegSet, reg)) && (lr->forbidden[reg] == false) &&
        lr->pregveto[reg] == false) {
      return reg;
    }
  }
#endif  //  MOVE_COALESCE

  for (auto it = currRegSet->begin(); it != currRegSet->end(); it++) {
    reg = *it + base;
    if ((lr->forbidden[reg] == false) && lr->pregveto[reg] == false) {
      return reg;
    }
  }
  // Failed to allocate in first choice. Try 2nd choice.
  for (auto it = nextRegSet->begin(); it != nextRegSet->end(); it++) {
    reg = *it + base;
    if ((lr->forbidden[reg] == false) && lr->pregveto[reg] == false) {
      return reg;
    }
  }
  //CG_ASSERT(0, "Failed to find a register");

  return 0;
}

// If forbidden list has more registers than max of all BB's local reg
// requirement, then LR can be colored.
// Update LR's color if success, return true, else return false.
bool GraphColorRegAllocator::AssignColorToLr(LiveRange *lr, bool isDelayed) {
  if (lr->assigned > 0) {
    // Already assigned.
    return true;
  }
  if (HaveAvailableColor(lr, lr->numForbidden + lr->numPregveto)) {
    lr->assigned = FindColorForLr(lr);
    if (lr->assigned == 0) {
      return false;
    }
#ifdef OPTIMIZE_FOR_PROLOG
    if (doOptProlog && isDelayed) {
      if (lr->regtype == kRegTyInt) {
        if (!ShouldUseCallee(lr, intCalleeUsed, intDelayed)) {
          return false;
        }
      } else {
        if (!ShouldUseCallee(lr, fpCalleeUsed, fpDelayed)) {
          return false;
        }
      }
    }
#endif  // OPTIMIZE_FOR_PROLOG

    bool isCalleeReg = Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)(lr->assigned));
    if (isCalleeReg) {
      if (lr->regtype == kRegTyInt) {
        intCalleeUsed.insert((lr->assigned));
      } else {
        fpCalleeUsed.insert((lr->assigned));
      }
    }

    UpdateForbiddenForNeighbors(lr);

    FOREACH_BB_ARR_ELEM(lr->bmember, bbid)
      SetBbInfoGlobalAssigned(bbid, lr->assigned);
    END_FOREACH_BB_ARR_ELEM
    if (GCRA_DUMP) {
      LogInfo::MapleLogger() << "assigned " << lr->assigned << " to R" << lr->regno << endl;
    }
    return true;
  }
  return false;
}

void GraphColorRegAllocator::PruneLrForSplit(LiveRange *lr, BB *bb, bool remove, set<CgfuncLoops *> &candidateInLoop,
                                             set<CgfuncLoops *> &defInLoop) {
  if (bb->internal_flag1) {
    // already visited
    return;
  }

  bb->internal_flag1 = true;
  auto lu = lr->luMap.find(bb->id);
  uint32 defNum = 0;
  uint32 useNum = 0;
  if (lu != lr->luMap.end()) {
    defNum = lu->second->defNum;
    useNum = lu->second->useNum;
  }

  if (remove) {
    // In removal mode, has not encountered a ref yet.
    if (defNum == 0 && useNum == 0) {
      if (bb->loop && FIND_IN(candidateInLoop, bb->loop)) {
        // Upward search has found a loop.  Regardless of def/use
        // The loop members must be included in the new LR.
        remove = false;
      } else {
        // No ref in this bb. mark as potential remove.
        bb->internal_flag2 = true;
      }
    } else {
      // found a ref, no more removal of bb and preds.
      remove = false;
    }
  }

  // With a def in loop, cannot prune that loop
  if (bb->loop && defNum > 0) {
    defInLoop.insert(bb->loop);
  }

  if (remove == false) {
    if (bb->loop) {
      // bb in loop, need to make sure of loop carried dependency
      candidateInLoop.insert(bb->loop);
    }
    for (auto pred : bb->preds) {
      if (FIND_NOT_IN(bb->loop_preds, pred)) {
        PruneLrForSplit(lr, pred, remove, candidateInLoop, defInLoop);
      }
    }
    for (auto pred : bb->eh_preds) {
      if (FIND_NOT_IN(bb->loop_preds, pred)) {
        PruneLrForSplit(lr, pred, remove, candidateInLoop, defInLoop);
      }
    }
  }
}

void GraphColorRegAllocator::FindBbSharedInSplit(LiveRange *lr, set<CgfuncLoops *> &candidateInLoop,
                                                 set<CgfuncLoops *> &defInLoop) {
  // A loop might be split into two.  Need to see over the entire LR if there is a def in the loop.
  FOREACH_BB_ARR_ELEM(lr->bmember, bbid)
  BB *bb = bbVec[bbid];
    if (bb->loop && FIND_IN(candidateInLoop, bb->loop)) {
      auto lu = lr->luMap.find(bb->id);
      if (lu != lr->luMap.end() && lu->second->defNum > 0) {
        defInLoop.insert(bb->loop);
      }
    }
  END_FOREACH_BB_ARR_ELEM
}

// Backward traversal of the top part of the split LR.
// Prune the part of the LR that has no downward exposing references.
// Take into account of loops and loop carried dependencies.
// The candidate bb to be removed, if in a loop, store that info.
// If a LR crosses a loop, even if the loop has no def/use, it must
// be included in the new LR.
void GraphColorRegAllocator::ComputeBbForNewSplit(LiveRange *newLr, LiveRange *origLr) {
  // The candidate bb to be removed, if in a loop, store that info.
  // If a LR crosses a loop, even if the loop has no def/use, it must
  // be included in the new LR.
  set<CgfuncLoops *> candidateInLoop;
  // If a bb has a def and is in a loop, store that info.
  set<CgfuncLoops *> defInLoop;
  set<BB *, SortedBBCmpFunc> smember;
  FOREACH_BB_ARR_ELEM(newLr->bmember, bbid)
  smember.insert(bbVec[bbid]);
  END_FOREACH_BB_ARR_ELEM
  for (auto bbit = smember.rbegin(); bbit != smember.rend(); bbit++)
  {
    BB *bb = *bbit;
    if (bb->internal_flag1) {
      continue;
    }
    PruneLrForSplit(newLr, bb, true, candidateInLoop, defInLoop);
  }
  FindBbSharedInSplit(origLr, candidateInLoop, defInLoop);
  FOREACH_BB_ARR_ELEM(newLr->bmember, bbid)  // prune the top LR.
  BB *bb = bbVec[bbid];
  if (bb->internal_flag2) {
    if (FIND_IN(candidateInLoop, bb->loop)) {
      continue;
    }
    if (FIND_NOT_IN(defInLoop, bb->loop)) {  // defInLoop should be a subset of candidateInLoop.  remove.
      UNSET_MEMBER_BIT_ARR_ELEM(newLr, bbid);
    }
  }
  END_FOREACH_BB_ARR_ELEM
}

bool GraphColorRegAllocator::UseIsUncovered(BB *bb, BB *startBb) {
  for (auto pred : bb->preds) {
    if (pred->level <= startBb->level) {
      return true;
    }
    if (UseIsUncovered(pred, startBb)) {
      return true;
    }
  }
  for (auto pred : bb->eh_preds) {
    if (pred->level <= startBb->level) {
      return true;
    }
    if (UseIsUncovered(pred, startBb)) {
      return true;
    }
  }
  return false;
}

void GraphColorRegAllocator::FindUseForSplit(LiveRange *lr, SplitBbInfo &bbInfo, bool &remove,
                                             set<CgfuncLoops *> &candidateInLoop, set<CgfuncLoops *> &defInLoop) {
  BB *bb = bbInfo.GetCandidateBb();
  BB *startBb = bbInfo.GetStartBb();
  if (bb->internal_flag1) {
    // already visited
    return;
  }
  for (auto pred : bb->preds) {
    if (pred->internal_flag1 == false) {
      return;
    }
  }
  for (auto pred : bb->eh_preds) {
    if (pred->internal_flag1 == false) {
      return;
    }
  }

  bb->internal_flag1 = true;
  auto lu = lr->luMap.find(bb->id);
  uint32 defNum = 0;
  uint32 useNum = 0;
  if (lu != lr->luMap.end()) {
    defNum = lu->second->defNum;
    useNum = lu->second->useNum;
  }

  if (remove) {
    // In removal mode, has not encountered a ref yet.
    if (defNum == 0 && useNum == 0) {
      // No ref in this bb. mark as potential remove.
      bb->internal_flag2 = true;
      if (bb->loop) {
        // bb in loop, need to make sure of loop carried dependency
        candidateInLoop.insert(bb->loop);
      }
    } else {
      // found a ref, no more removal of bb and preds.
      remove = false;
      // A potential point for a upward exposing use. (might be a def).
      lu->second->needRlod = true;
    }
  } else if ((defNum > 0 || useNum > 0) && UseIsUncovered(bb, startBb)) {
    lu->second->needRlod = true;
  }

  // With a def in loop, cannot prune that loop
  if (bb->loop && defNum > 0) {
    defInLoop.insert(bb->loop);
  }

  for (auto succ : bb->succs) {
    if (FIND_NOT_IN(bb->loop_succs, succ)) {
      bbInfo.SetCandidateBb(succ);
      FindUseForSplit(lr, bbInfo, remove, candidateInLoop, defInLoop);
    }
  }
  for (auto succ : bb->eh_succs) {
    if (FIND_NOT_IN(bb->loop_succs, succ)) {
      bbInfo.SetCandidateBb(succ);
      FindUseForSplit(lr, bbInfo, remove, candidateInLoop, defInLoop);
    }
  }
}

void GraphColorRegAllocator::ClearLrBbFlags(set<BB *, SortedBBCmpFunc> &member) {
  for (auto bb : member) {
    bb->internal_flag1 = false;
    bb->internal_flag2 = false;
    for (auto pred : bb->preds) {
      pred->internal_flag1 = false;
      pred->internal_flag2 = false;
    }
    for (auto pred : bb->eh_preds) {
      pred->internal_flag1 = false;
      pred->internal_flag2 = false;
    }
  }
}

// Downward traversal of the bottom part of the split LR.
// Prune the part of the LR that has no upward exposing references.
// Take into account of loops and loop carried dependencies.
void GraphColorRegAllocator::ComputeBbForOldSplit(LiveRange *newLr, LiveRange *origLr) {
  // The candidate bb to be removed, if in a loop, store that info.
  set<CgfuncLoops *> candidateInLoop;
  // If a bb has a def and is in a loop, store that info.
  set<CgfuncLoops *> defInLoop;
  SplitBbInfo bbInfo;
  bool remove = true;

  set<BB *, SortedBBCmpFunc> smember;
  FOREACH_BB_ARR_ELEM(origLr->bmember, bbid)
  smember.insert(bbVec[bbid]);
  END_FOREACH_BB_ARR_ELEM
  ClearLrBbFlags(smember);
  for (auto bb : smember)
  {
    if (bb->internal_flag1) {
      continue;
    }
    for (auto pred : bb->preds) {
      pred->internal_flag1 = true;
    }
    for (auto pred : bb->eh_preds) {
      pred->internal_flag1 = true;
    }
    bbInfo.SetCandidateBb(bb);
    bbInfo.SetStartBb(bb);
    FindUseForSplit(origLr, bbInfo, remove, candidateInLoop, defInLoop);
  }
  FindBbSharedInSplit(newLr, candidateInLoop, defInLoop);
  FOREACH_BB_ARR_ELEM(origLr->bmember, bbid)
  BB *bb = bbVec[bbid];
  if (bb->internal_flag2) {
    if (FIND_NOT_IN(defInLoop, bb->loop)) {
      UNSET_MEMBER_BIT_ARR_ELEM(origLr, bbid);
    }
  }
  END_FOREACH_BB_ARR_ELEM
}

// There is at least one available color for this BB from the neighbors
// minus the ones reserved for local allocation.
// bbAdded : The new BB to be added into the split LR if color is available.
// conflictRegs : Reprent the LR before adding the bbAdded.  These are the
//                forbidden regs before adding the new BBs.
// Side effect : Adding the new forbidden regs from bbAdded into
//               conflictRegs if the LR can still be colored.
bool GraphColorRegAllocator::LrCanBeColored(LiveRange *lr, BB *bbAdded, set<regno_t> &conflictRegs) {
  RegType type = lr->regtype;

  set<regno_t> newConflict;
  FOREACH_REG_ARR_ELEM(lr->bconflict, regno)
    // check the real conflict in current bb
    LiveRange *conflictLr = lrVec[regno];
    // If the bb to be added to the new LR has an actual
    // conflict with another LR, and if that LR has already
    // assigned a color that is not in the conflictRegs,
    // then add it as a newConflict.
    if (IS_BIT_ARR_ELEM_SET(conflictLr->bmember, bbAdded->id)) {
      regno_t confReg = conflictLr->assigned;
      if ((confReg > 0) && FIND_NOT_IN(conflictRegs, confReg) && lr->pregveto[confReg] == false) {
        newConflict.insert(confReg);
      }
    } else if (conflictLr->splitLr && IS_BIT_ARR_ELEM_SET(conflictLr->splitLr->bmember, bbAdded->id)) {
      // The after split LR is split into pieces, and this ensures
      // the after split color is taken into consideration.
      regno_t confReg = conflictLr->splitLr->assigned;
      if ((confReg > 0) && FIND_NOT_IN(conflictRegs, confReg) && lr->pregveto[confReg] == false) {
        newConflict.insert(confReg);
      }
    }
  END_FOREACH_REG_ARR_ELEM

  uint32 numRegs;
  numRegs = newConflict.size() + lr->numPregveto + conflictRegs.size();

  bool canColor = false;
  if (type == kRegTyInt) {
    if (numRegs < intRegNum) {
      canColor = true;
    }
  } else if (numRegs < fpRegNum) {
    canColor = true;
  }

  if (canColor) {
    for (auto regno : newConflict) {
      conflictRegs.insert(regno);
    }
  }

  // Update all the registers conflicting when adding thew new bb.
  return canColor;
}

// Support function for LR split.  Move one BB from LR1 to LR2.
void GraphColorRegAllocator::MoveLrBbInfo(LiveRange *oldLr, LiveRange *newLr, BB *bb) {
  // initialize backward traversal flag for the bb pruning phase
  bb->internal_flag1 = false;
  // initialize bb removal marker
  bb->internal_flag2 = false;
  // Insert BB into new LR
  SET_MEMBER_BIT_ARR_ELEM(newLr, bb->id);

  // Move LU from old LR to new LR
  auto luIt = oldLr->luMap.find(bb->id);
  if (luIt != oldLr->luMap.end()) {
    newLr->luMap[luIt->first] = luIt->second;
    oldLr->luMap.erase(luIt);
  }

  // Remove BB from old LR
  UNSET_MEMBER_BIT_ARR_ELEM(oldLr, bb->id);
}

// Is the set of loops inside the loop?
bool GraphColorRegAllocator::ContainsLoop(CgfuncLoops *loop, set<CgfuncLoops *> &loops) {
  for (auto lp : loops) {
    while (lp) {
      if (lp == loop) {
        return true;
      }
      lp = lp->outer_loop;
    }
  }
  return false;
}

void GraphColorRegAllocator::GetAllLrMemberLoops(LiveRange *lr, set<CgfuncLoops *> &loops) {
  FOREACH_BB_ARR_ELEM(lr->bmember, bbid)
  BB *bb = bbVec[bbid];
    CgfuncLoops *loop = bb->loop;
    if (loop) {
      loops.insert(loop);
    }
  END_FOREACH_BB_ARR_ELEM
}

bool GraphColorRegAllocator::SplitLrShouldSplit(LiveRange *lr) {
  if (lr->splitLr || lr->numBmembers == 1)
  {
    return false;
  }

  // Need to split within the same hierarchy
  int32 loopId = -1;
  FOREACH_BB_ARR_ELEM(lr->bmember, bbid)
  BB *bb = bbVec[bbid];
    if (loopId == -1) {
      if (bb->loop) {
        loopId = bb->loop->header->id;
      } else {
        loopId = 0;
      }
    } else {
      if ((bb->loop && bb->loop->header->id != loopId) || (bb->loop == nullptr && loopId != 0)) {
        return false;
      }
    }
  END_FOREACH_BB_ARR_ELEM
  return true;
}

bool GraphColorRegAllocator::SplitLrIsProfitable(LiveRange *newLr) {
  if (newLr->numBmembers == 0)
  {
    // split fail
    return false;
  }
  bool hasUse = false;
  FOREACH_BB_ARR_ELEM(newLr->bmember, bbid) {
    auto lu = newLr->luMap.find(bbid);
    if (lu->second->useNum > 0) {
      hasUse = true;
      break;
    }
  }
  END_FOREACH_BB_ARR_ELEM
  return hasUse;
}

void GraphColorRegAllocator::ResetSplitLr(LiveRange *origLr, LiveRange *newLr) {
  FOREACH_BB_ARR_ELEM(newLr->bmember, bbid) {
    UNSET_MEMBER_BIT_ARR_ELEM(newLr, bbid);
    SET_MEMBER_BIT_ARR_ELEM(origLr, bbid);

    auto luIt = newLr->luMap.find(bbid);
    if (luIt != newLr->luMap.end()) {
      origLr->luMap[luIt->first] = luIt->second;
      newLr->luMap.erase(luIt);
    }
  }
  END_FOREACH_BB_ARR_ELEM
}

// When a BB in the LR has no def or use in it, then potentially
// there is no conflict within these BB for the new LR, since
// the new LR will need to spill the defs which terminates the
// new LR unless there is a use later which extends the new LR.
// There is no need to compute conflicting register set unless
// there is a def or use.
// It is assumed that the new LR is extended to the def or use.
// Initially newLr is empty, then add bb if can be colored.
// Return true if there is a split.
bool GraphColorRegAllocator::SplitLrFindCandidateLr(LiveRange *lr, LiveRange *newLr, set<regno_t> &conflictRegs) {
  if (GCRA_DUMP) {
    LogInfo::MapleLogger() << "start split lr for vreg " << lr->regno << endl;
  }
  set<BB *, SortedBBCmpFunc> smember;
  FOREACH_BB_ARR_ELEM(lr->bmember, bbid)
  smember.insert(bbVec[bbid]);
  END_FOREACH_BB_ARR_ELEM
  for (auto bb : smember)
  {
    if (LrCanBeColored(lr, bb, conflictRegs)) {
      MoveLrBbInfo(lr, newLr, bb);
    } else {
      break;
    }
  }

  if (SplitLrIsProfitable(newLr)) {
    if (GCRA_DUMP) {
      LogInfo::MapleLogger() << "Split:" << lr->regno << endl;
    }
    return true;
  } else {
    ResetSplitLr(lr, newLr);
    if (GCRA_DUMP) {
      LogInfo::MapleLogger() << "ResetSplit:" << lr->regno << endl;
    }
    return false;
  }
}

void GraphColorRegAllocator::SplitLrHandleLoops(LiveRange *lr, LiveRange *newLr, set<CgfuncLoops *> &origLoops,
                                                set<CgfuncLoops *> &newLoops) {
  // bb in loops might need a reload due to loop carried dependency.
  // Compute this before pruning the LRs.
  // if there is no re-definition, then reload is not necessary.

  // Part of the new LR region after the last reference is
  // no longer in the LR.  Remove those bb.
  ComputeBbForNewSplit(newLr, lr);
  // With new LR, recompute conflict.
  FOREACH_BB_ARR_ELEM(newLr->bmember, bbid)
  FOREACH_REG_ARR_ELEM(lr->bconflict, regno)
  LiveRange *conf_lr = lrVec[regno];
  if (IS_BIT_ARR_ELEM_SET(conf_lr->bmember, bbid)) {
    // New LR getting the interference does not mean the
    // old LR can remove the interference.
    // Old LR's interference will be handled at the end of split.
    SET_CONFLICT_BIT_ARR_ELEM(newLr, regno);
  } else if (conf_lr->splitLr && IS_BIT_ARR_ELEM_SET(conf_lr->splitLr->bmember, bbid)) {
    SET_CONFLICT_BIT_ARR_ELEM(newLr, regno);
  }
  END_FOREACH_REG_ARR_ELEM
  END_FOREACH_BB_ARR_ELEM
  // update bb/loop same as for new LR.
  ComputeBbForOldSplit(newLr, lr);
  // Update the conflict interference for the original LR later.

  for (auto loop : newLoops) {
    if (ContainsLoop(loop, origLoops)) {
      for (auto bb : loop->loop_members) {
        if (IS_BIT_ARR_ELEM_SET(newLr->bmember, bb->id))
        {
          LiveUnit *lu = newLr->luMap[bb->id];
          if (lu->useNum != 0) {
            lu->needRlod = true;
          }
        }
      }
    }
  }
}

void GraphColorRegAllocator::SplitLrFixNewLrCallsAndRlod(LiveRange *newLr, set<CgfuncLoops *> &origLoops) {
  // If a 2nd split loop is before the bb in 1st split bb.
  newLr->numCall = 0;
  FOREACH_BB_ARR_ELEM(newLr->bmember, bbid)
  BB *bb = bbVec[bbid];
    for (auto loop : origLoops) {
      if (loop->header->level < bb->level) {
        LiveUnit *lu = newLr->luMap[bbid];
        if (lu->useNum != 0) {
          lu->needRlod = true;
        }
      }
    }
    LiveUnit *lu = newLr->luMap[bbid];
    if (newLr->numBmembers > 1 && lu->hasCall) {
      newLr->numCall++;
    }
  END_FOREACH_BB_ARR_ELEM
}

void GraphColorRegAllocator::SplitLrFixOrigLrCalls(LiveRange *lr) {
  lr->numCall = 0;
  if (lr->numBmembers > 1) {
    FOREACH_BB_ARR_ELEM(lr->bmember, bbid)
      LiveUnit *lu = lr->luMap[bbid];
      if (lu->hasCall) {
        lr->numCall++;
      }
    END_FOREACH_BB_ARR_ELEM
  }
}

void GraphColorRegAllocator::SplitLrUpdateInterference(LiveRange *lr) {
  // newLr is now a separate LR from the original lr.
  // Update the interference info.
  // Also recompute the forbidden info
  lr->forbidden.clear();
  FOREACH_REG_ARR_ELEM(lr->bconflict, regno)
    LiveRange *conf_lr = lrVec[regno];
    if (SET_BB_OVERLAP(lr->bmember, conf_lr->bmember)) {
      // interfere
      if (conf_lr->assigned && lr->pregveto[conf_lr->assigned] == false) {
        lr->insertForbidden(conf_lr->assigned);
      }
    } else {
      // no interference
      UNSET_CONFLICT_BIT_ARR_ELEM(lr, regno);
    }
  END_FOREACH_REG_ARR_ELEM
}

void GraphColorRegAllocator::SplitLrUpdateRegInfo(LiveRange *origLr, LiveRange *newLr, set<regno_t> &conflictRegs) {
  for (regno_t regno = 0; regno < kNumPregBits; regno++) {
    if (origLr->pregveto[regno]) {
      newLr->insertPregveto(regno);
    }
  }
  for (auto regno : conflictRegs) {
    if (newLr->pregveto[regno] == false) {
      newLr->insertForbidden(regno);
    }
  }
}

void GraphColorRegAllocator::SplitLrErrorCheckAndDebug(LiveRange *origLr, LiveRange *newLr) {
  if (origLr->numBmembers == 0) {
    CG_ASSERT(origLr->numBconflicts == 0, "Error: member and conflict not match");
  }

  if (GCRA_DUMP) {
    LogInfo::MapleLogger() << "----->newLr->assigned " << newLr->assigned << endl;
    PrintLiveRange(origLr, "LRs after split");
  }
}

// Pick a starting BB, then expand to maximize the new LR.
// Return the new LR.
LiveRange *GraphColorRegAllocator::SplitLr(LiveRange *lr) {
  if (SplitLrShouldSplit(lr) == false) {
    return nullptr;
  }
  LiveRange *newLr = NewLiveRange();
  // For the new LR, whenever a BB with either a def or
  // use is added, then add the registers that the neighbor
  // is using to the conflict register set indicating that these
  // registers cannot be used for the new LR's color.
  set<regno_t> conflictRegs;
  if (SplitLrFindCandidateLr(lr, newLr, conflictRegs) == false) {
    return nullptr;
  }
#ifdef REUSE_SPILLMEM
  // Copy the original conflict vector for spill reuse optimization
  lr->oldConflict = cgfunc_->memPool->NewArray<uint64>(regBuckets);
  COPY_BIT_ARR_ELEM(lr->oldConflict, lr->bconflict, regBuckets);
#endif  // REUSE_SPILLMEM

  set<CgfuncLoops *> newLoops;
  set<CgfuncLoops *> origLoops;
  GetAllLrMemberLoops(newLr, newLoops);
  GetAllLrMemberLoops(lr, origLoops);
  SplitLrHandleLoops(lr, newLr, origLoops, newLoops);
  SplitLrFixNewLrCallsAndRlod(newLr, origLoops);
  SplitLrFixOrigLrCalls(lr);

  SplitLrUpdateRegInfo(lr, newLr, conflictRegs);

  CalculatePriority(lr);
  // At this point, newLr should be unconstrained.

  lr->splitLr = newLr;

  newLr->regno = lr->regno;
  newLr->regtype = lr->regtype;
  newLr->id = lr->id;
  CalculatePriority(newLr);
  SplitLrUpdateInterference(lr);
  newLr->assigned = FindColorForLr(newLr);

  // For the new LR, update assignment for local RA
  FOREACH_BB_ARR_ELEM(newLr->bmember, bbid)
    SetBbInfoGlobalAssigned(bbid, newLr->assigned);
  END_FOREACH_BB_ARR_ELEM

  UpdatePregvetoForNeighbors(newLr);

  SplitLrErrorCheckAndDebug(lr, newLr);
  return newLr;
}

void GraphColorRegAllocator::ColorForOptPrologEpilog() {
#ifdef OPTIMIZE_FOR_PROLOG
  if (doOptProlog) {
    for (auto lr : intDelayed) {
      if (AssignColorToLr(lr, true)) {
        continue;
      } else {
        lr->spilled = true;
      }
    }
    for (auto lr : fpDelayed) {
      if (AssignColorToLr(lr, true)) {
        continue;
      } else {
        lr->spilled = true;
      }
    }
  }
#endif
  return;
}

//  From the sorted list of constrained LRs, pick the most profitable LR.
//  Split the LR into LRnew1 LRnew2 where LRnew1 has the maximum number of
//  BB and is colorable.
//  The starting BB for traversal must have a color available.
//
//  Assign a color, update neighbor's forbidden list.
//
//  Update the conflict graph by change the interference list.
//  In the case of both LRnew1 and LRnew2 conflicts with a BB, this Bb's
//  #neightbors increased.  If this BB was unconstrained, must check if
//  it is still unconstrained.  Move to constrained if necessary.
//
//  Color the unconstrained LRs.
void GraphColorRegAllocator::SplitAndColor() {
  // handle constrained
  while (constrained.size()) {
    MapleVector<LiveRange *>::iterator highestIt = GetHighPriorityLr(constrained);
    LiveRange *lr = *highestIt;
    if (AssignColorToLr(lr)) {
      if (highestIt != constrained.end()) {
        constrained.erase(highestIt);
      } else {
        CG_ASSERT(0, "Error: not in constrained");
      }
    } else {
#ifdef USE_SPLIT
      LiveRange *newLr = nullptr;
      newLr = SplitLr(lr);
      // check those lrs in lr->sconflict which is in unconstrained whether it turns to constrined
#endif  // USE_SPLIT
      if (highestIt != constrained.end()) {
        constrained.erase(highestIt);
        // When LR is spilled, it potentially has no conflicts as
        // each def/use is spilled/reloaded.
#ifdef COLOR_SPLIT
        if (AssignColorToLr(lr) == false) {
#endif  // COLOR_SPLIT
          lr->spilled = true;
          hasSpill = true;
#ifdef COLOR_SPLIT
        }
#endif  // COLOR_SPLIT
      } else {
        CG_ASSERT(0, "Error: not in constrained");
      }
    }
  }

  // assign color for unconstained
  while (unconstrained.size() > 0) {
    MapleVector<LiveRange *>::iterator highestIt = GetHighPriorityLr(unconstrained);
    LiveRange *lr = *highestIt;
    if (AssignColorToLr(lr)) {
      if (highestIt != unconstrained.end()) {
        unconstrained.erase(highestIt);
      } else {
        CG_ASSERT(0, "Error: not in unconstrained");
        LogInfo::MapleLogger() << "Error: not in unconstrained\n";
        // with error, iterator not erased
        break;
      }
    } else {
      CG_ASSERT(0, "LR should be colorable");
      LogInfo::MapleLogger() << "error: LR should be colorable " << lr->regno << "\n";
      // with error, iterator not erased
      break;
    }
  }

#ifdef OPTIMIZE_FOR_PROLOG
  if (doOptProlog) {
    ColorForOptPrologEpilog();
  }
#endif  // OPTIMIZE_FOR_PROLOG

  return;
}

void GraphColorRegAllocator::HandleLocalRegAssignment(regno_t regno, LocalRegAllocator *localRa, bool isInt) {
  MapleMap<regno_t, regno_t> *regAssignmentMap = localRa->GetRegAssignmentMap(isInt);

  // vreg, get a reg for it if not assigned already.
  if (localRa->isInRegAssigned(regno, isInt) == false) {
    // find an available phys reg
    if (localRa->isInRegSpilled(regno, isInt) == false) {
      bool founded = false;
      uint32 preg;
      FOREACH_LRA_REGS(isInt, preg, localRa) {
        if (lrVec[regno]->pregveto[preg]) {
          continue;
        }
        regno_t assignedReg = preg;
        localRa->ClearRegs(assignedReg, isInt);
        localRa->SetRegUsed(assignedReg, isInt);
        localRa->SetRegAssigned(regno, isInt);
        (*regAssignmentMap)[regno] = assignedReg;
        founded = true;
        break;
      }
      END_FOREACH_LRA_REGS;
      if (!founded) {
        localRa->SetRegSpilled(regno, isInt);
      }
    }
  } else {
    if (localRa->useInfo[regno] == 0) {
      // last use of vreg, release assignment
      localRa->SetRegs((*regAssignmentMap)[regno], isInt);
      if (GCRA_DUMP) {
        LogInfo::MapleLogger() << "\t\tlast use release reg " << (*regAssignmentMap)[regno] << " for " << regno << endl;
      }
    }
  }
}

bool GraphColorRegAllocator::HandleLocalRegDefWithNoUse(regno_t regno, LocalRegAllocator *localRa, bool isInt,
                                                        bool isDef) {
  MapleMap<regno_t, regno_t> *regAssignmentMap = localRa->GetRegAssignmentMap(isInt);

  auto usedIt = localRa->useInfo.find(regno);
  if (usedIt == localRa->useInfo.end()) {
    // local reg has no use, but obviously seen.
    // If it is a vreg, then just assign it an unused preg
    // no action needed since its live is dead right after.
    if (regno >= kMaxRegNum) {
      bool founded = false;
      uint32 preg;
      FOREACH_LRA_REGS(isInt, preg, localRa) {
        if (lrVec[regno]->pregveto[preg]) {
          continue;
        }
        founded = true;
        regno_t assignedReg = preg;
        localRa->SetRegAssigned(regno, isInt);
        (*regAssignmentMap)[regno] = assignedReg;
        break;
      }
      END_FOREACH_LRA_REGS;
    }
    // founded may be false
    return true;
  }
  if (!isDef) {
    // reg use, decrement count
    CG_ASSERT(usedIt->second > 0, "Incorrect local ra info");
    localRa->useInfo[regno] = usedIt->second - 1;
    if (GCRA_DUMP) {
      LogInfo::MapleLogger() << "\t\treg " << regno << " update #use to " << localRa->useInfo[regno] << endl;
    }
  }
  return false;
}

void GraphColorRegAllocator::HandleLocalRaDebug(regno_t regno, LocalRegAllocator *localRa, bool isInt) {
  LogInfo::MapleLogger() << "HandleLocalReg " << regno << endl;
  LogInfo::MapleLogger() << "\tregUsed:";
  uint64 regUsed;
  regUsed = localRa->GetRegUsed(isInt);
  regno_t base = 0;
  if (isInt) {
    base = R0;
  } else {
    base = V0;
  }
  for (uint32 i = 0; i < R31; i++) {
    if (regUsed & (1LL << i)) {
      LogInfo::MapleLogger() << " " << (i + base);
    }
  }
  LogInfo::MapleLogger() << endl;
  LogInfo::MapleLogger() << "\tregs:";
  uint64 regs = localRa->GetRegs(isInt);
  for (uint32 regno = 0; regno < R31; regno++) {
    if (regs & (1LL << regno)) {
      LogInfo::MapleLogger() << " " << (regno + base);
    }
  }
  LogInfo::MapleLogger() << endl;
}

void GraphColorRegAllocator::HandleLocalReg(Operand *op, LocalRegAllocator *localRa, BbAssignInfo *bbInfo, bool isDef,
                                            bool isInt) {
  if (!op->IsRegister()) {
    return;
  }
  RegOperand *regOpnd = static_cast<RegOperand *>(op);
  regno_t regno = regOpnd->GetRegisterNumber();

  if (Riscv64isa::IsPhysicalRegister(regno)) {
    return;
  }

  if (IsUnconcernedReg(regOpnd)) {
    return;
  }

  // is this a local register ?
  if (regno >= kMaxRegNum && !IsLocalReg(regno)) {
    return;
  }

  MapleMap<regno_t, regno_t> *regAssignmentMap = localRa->GetRegAssignmentMap(isInt);

  if (GCRA_DUMP) {
    HandleLocalRaDebug(regno, localRa, isInt);
  }

  if (HandleLocalRegDefWithNoUse(regno, localRa, isInt, isDef)) {
    return;
  }
  if (regOpnd->IsPhysicalRegister()) {
    if (localRa->useInfo[regno] == 0) {
      // See if it is needed by global RA
      if (bbInfo && bbInfo->globalsAssigned[regno] == false) {
        // This phys reg is now available for assignment for a vreg
        localRa->SetRegs(regno, isInt);
        if (GCRA_DUMP) {
          LogInfo::MapleLogger() << "\t\tphys-reg " << regno << " now available\n";
        }
      }
    }
  } else {
    HandleLocalRegAssignment(regno, localRa, isInt);
  }
}

void GraphColorRegAllocator::LocalRaRegSetEraseReg(LocalRegAllocator *localRa, regno_t regno) {
  bool isInt;
  if (Riscv64isa::IsGPRegister(static_cast<Riscv64reg_t>(regno))) {
    isInt = true;
  } else {
    isInt = false;
  }
  if (localRa->IsInRegs(regno, isInt)) {
    localRa->ClearRegs(regno, isInt);
  }
}

bool GraphColorRegAllocator::LocalRaInitRegSet(LocalRegAllocator *localRa, uint32 bbid, bool doAllocate) {
  bool needLocalRa = false;
  // Note physical regs start from R0, V0.
  localRa->InitRegs(MaxIntPhysRegNum(), MaxFloatPhysRegNum(), cgfunc_->cg->GenYieldpoint());

  localRa->useInfo.clear();
  LocalRaInfo *lraInfo = localRegVec[bbid];
  for (auto it : lraInfo->useCnt) {
    regno_t regno = it.first;
    if (regno >= kMaxRegNum) {
      needLocalRa = true;
#ifdef DO_PRE_LRA
    } else if (doLRA) {
      if (!doAllocate) {
        // ? no need when have forbidden info
        LocalRaRegSetEraseReg(localRa, regno);
      }
#endif
    }
    localRa->useInfo[it.first] = it.second;
  }
  return needLocalRa;
}

void GraphColorRegAllocator::LocalRaInitAllocatableRegs(LocalRegAllocator *localRa, uint32 bbid, bool doAllocate) {
  BbAssignInfo *bbInfo = bbRegInfo[bbid];
  if (bbInfo) {
    for (regno_t regno = 0; regno < kNumPregBits; regno++) {
      if (bbInfo->globalsAssigned[regno]) {
        LocalRaRegSetEraseReg(localRa, regno);
      }
    }
  }
}

void GraphColorRegAllocator::LocalRaPrepareBb(BB *bb, LocalRegAllocator *localRa) {
  BbAssignInfo *bbInfo = bbRegInfo[bb->id];
  FOR_BB_INSNS(insn, bb) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }

    // Use reverse operand order, assuming use first then def for allocation.
    // need to free the use resource so it can be reused for def.
    const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->GetMachineOpcode()];
    for (int32 i = (Insn::kMaxOperandNum - 1); i >= 0; i--) {
      Operand *opnd = insn->GetOperand(i);
      if (opnd == nullptr) {
        continue;
      }
      if (opnd->IsList()) {
      } else if (opnd->IsMemoryAccessOperand()) {
        MemOperand *memopnd = static_cast<MemOperand *>(opnd);
        Operand *base = memopnd->GetBaseRegister();
        if (base != nullptr) {
          HandleLocalReg(base, localRa, bbInfo, false, true);
        }
        Operand *offset = memopnd->GetIndexRegister();
        if (offset != nullptr) {
          HandleLocalReg(offset, localRa, bbInfo, false, true);
        }
      } else {
        bool isDef = static_cast<Riscv64OpndProp *>(md->operand_[i])->IsRegDef();
        RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
        if (regOpnd->GetRegisterType() == kRegTyInt) {
          HandleLocalReg(opnd, localRa, bbInfo, isDef, true);
        } else {
          HandleLocalReg(opnd, localRa, bbInfo, isDef, false);
        }
      }
    }
  }
}

void GraphColorRegAllocator::LocalRaFinalAssignment(LocalRegAllocator *localRa, BbAssignInfo *bbInfo) {
  for (auto it : localRa->intRegAssignmentMap) {
    regno_t regno = it.second;
    if (GCRA_DUMP) {
      LogInfo::MapleLogger() << "[" << it.first << "," << regno << "],";
    }
    // Might need to get rid of this copy.
    bbInfo->regMap[it.first] = regno;
    if (Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)regno)) {
      intCalleeUsed.insert(regno);
    }
  }
  for (auto it : localRa->fpRegAssignmentMap) {
    regno_t regno = it.second;
    if (GCRA_DUMP) {
      LogInfo::MapleLogger() << "[" << it.first << "," << regno << "],";
    }
    // Might need to get rid of this copy.
    bbInfo->regMap[it.first] = regno;
    if (Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)regno)) {
      fpCalleeUsed.insert(regno);
    }
  }
}

void GraphColorRegAllocator::LocalRaDebug(BB *bb, LocalRegAllocator *localRa) {
  LogInfo::MapleLogger() << "bb " << bb->id << " local ra INT need " << localRa->numIntRegUsed << " regs\n";
  LogInfo::MapleLogger() << "bb " << bb->id << " local ra FP need " << localRa->numFpRegUsed << " regs\n";
  LogInfo::MapleLogger() << "\tpotential assignments:";
  for (auto it : localRa->intRegAssignmentMap) {
    LogInfo::MapleLogger() << "[" << it.first << "," << it.second << "],";
  }
  for (auto it : localRa->fpRegAssignmentMap) {
    LogInfo::MapleLogger() << "[" << it.first << "," << it.second << "],";
  }
  LogInfo::MapleLogger() << "\n";
}

// When do_allocate is false, it is prepass:
// Traverse each BB, keep track of the number of registers required
// for local registers in the BB.  Communicate this to global RA.
//
// When do_allocate is true:
// Allocate local registers for each BB based on unused registers
// from global RA.  Spill if no register available.
void GraphColorRegAllocator::LocalRegisterAllocator(bool doAllocate) {
  if (GCRA_DUMP) {
    if (doAllocate) {
      LogInfo::MapleLogger() << "LRA allocation start\n";
      PrintBbAssignInfo();
    } else {
      LogInfo::MapleLogger() << "LRA preprocessing start\n";
    }
  }
  LocalRegAllocator *localRa = cgfunc_->memPool->New<LocalRegAllocator>(cgfunc_, allocator);
  for (uint32 idx = 0; idx < sortedBBs.size(); idx++) {
    BB *bb = sortedBBs[idx];
    uint32 bbid = bb->id;

    LocalRaInfo *lraInfo = localRegVec[bb->id];
    if (lraInfo == nullptr) {
      // No locals to allocate
      continue;
    }

    localRa->ClearLocalRaInfo();
    bool needLocalRa = LocalRaInitRegSet(localRa, bbid, doAllocate);
    for (auto it : lraInfo->defCnt) {
      if (it.first >= kMaxRegNum) {
        needLocalRa = true;
        break;
      }
    }
    if (needLocalRa == false) {
      // Only physical regs in bb, no local ra needed.
      continue;
    }

    if (doAllocate) {
      LocalRaInitAllocatableRegs(localRa, bbid, doAllocate);
    }

    LocalRaPrepareBb(bb, localRa);

    BbAssignInfo *bbInfo = bbRegInfo[bb->id];
    if (bbInfo == nullptr) {
      bbInfo = cgfunc_->memPool->New<BbAssignInfo>(allocator);
      bbRegInfo[bbid] = bbInfo;
      bbInfo->globalsAssigned.clear();
      bbInfo->globalsAssigned.resize(kNumPregBits);
    }
    bbInfo->intLocalRegsNeeded = localRa->numIntRegUsed;
    bbInfo->fpLocalRegsNeeded = localRa->numFpRegUsed;

    if (doAllocate) {
      if (GCRA_DUMP) {
        LogInfo::MapleLogger() << "\tbb(" << bb->id << ")final local ra assignments:";
      }
      LocalRaFinalAssignment(localRa, bbInfo);
      if (GCRA_DUMP) {
        LogInfo::MapleLogger() << "\n";
      }
    } else if (GCRA_DUMP) {
      LocalRaDebug(bb, localRa);
    }
  }
}

MemOperand *GraphColorRegAllocator::GetConsistentReuseMem(uint32 vregno, uint32 size, RegType regtype) {
  LiveRange *lr = lrVec[vregno];
  uint64 *conflict;
  if (lr->splitLr) {
    // For split LR, the vreg liveness is optimized, but for spill location
    // the stack location needs to be maintained for the entire LR.
    conflict = lr->oldConflict;
  } else {
    conflict = lr->bconflict;
  }
  set<LiveRange *, SetLiveRangeCmpFunc> sconflict;
  FOREACH_REG_ARR_ELEM_NOT_SET(conflict, regno) {
    if (regno >= numVregs) {
      break;
    }
    if (lrVec[regno]) {
      sconflict.insert(lrVec[regno]);
    }
  }
  END_FOREACH_BB_ARR_ELEM

  set<MemOperand *> usedMemopnd;
  FOREACH_REG_ARR_ELEM(conflict, regno) {
    LiveRange *lr = lrVec[regno];
    if (lr->spillMem) {
      usedMemopnd.insert(lr->spillMem);
    }
  }
  END_FOREACH_REG_ARR_ELEM

  for (auto it : sconflict) {
    LiveRange *noConflictLr = it;
    if (noConflictLr && noConflictLr->regtype == regtype && noConflictLr->spillSize == size) {
      if (usedMemopnd.find(noConflictLr->spillMem) == usedMemopnd.end()) {
        return noConflictLr->spillMem;
      }
    }
  }
  return nullptr;
}

MemOperand *GraphColorRegAllocator::GetCommonReuseMem(uint32 vregno, uint32 size, RegType regtype) {
  LiveRange *lr = lrVec[vregno];
  uint64 *conflict;
  if (lr->splitLr) {
    // For split LR, the vreg liveness is optimized, but for spill location
    // the stack location needs to be maintained for the entire LR.
    conflict = lr->oldConflict;
  } else {
    conflict = lr->bconflict;
  }

  set<MemOperand *> usedMemopnd;
  FOREACH_REG_ARR_ELEM(conflict, regno) {
    LiveRange *lr = lrVec[regno];
    if (lr->spillMem) {
      usedMemopnd.insert(lr->spillMem);
    }
  }
  END_FOREACH_REG_ARR_ELEM

  FOREACH_REG_ARR_ELEM_NOT_SET(conflict, regno) {
    if (regno >= numVregs || IsLocalReg(regno)) {
      // conflict is for globals.  Cannot factor into locals.
      break;
    }
    LiveRange *noConflictLr = lrVec[regno];
    if (noConflictLr && noConflictLr->regtype == regtype && noConflictLr->spillSize == size) {
      if (usedMemopnd.find(noConflictLr->spillMem) == usedMemopnd.end()) {
        return noConflictLr->spillMem;
      }
    }
  }

  END_FOREACH_REG_ARR_ELEM_NOT_SET
  return nullptr;
}

// See if any of the non-conflict LR is spilled and use its memopnd.
MemOperand *GraphColorRegAllocator::GetReuseMem(uint32 vregno, uint32 size, RegType regtype) {
  if (IsLocalReg(vregno)) {
    return nullptr;
  }

  size = (size <= k32) ? k32 : k64;

  // This is to order the search so memopnd given out is consistent.
  // When vreg#s do not change going through VtableImpl.mpl file
  // then this can be simplified.
#ifdef CONSISTENT_MEMOPND
  return GetConsistentReuseMem(vregno, size, regtype);
#else   // CONSISTENT_MEMOPND
  return GetCommonReuseMem(vregno, size, regtype);
#endif  // CONSISTENT_MEMOPND

  //return nullptr;
}

MemOperand *GraphColorRegAllocator::GetSpillMem(uint32 vregno, Insn *insn, Riscv64reg_t regno,
                                                uint8 &isOutOfRange, bool isDef) {
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc_);
  MemOperand *memopnd;
  memopnd = a64cgfunc->GetOrCreatSpillMem(vregno);
  return (a64cgfunc->AdjustMemOperandIfOffsetOutOfRange(memopnd, vregno, insn, regno, isOutOfRange, isDef));
}

void GraphColorRegAllocator::SpillOperandForSpillPre(Insn *insn, Operand *opnd, RegOperand *phyopnd, uint32 spillIdx,
                                                     bool needSpill) {
  if (needSpill == false) {
    return;
  }
  MemOperand *spillMem;
  spillMem = CreateSpillMem(spillIdx);
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc_);
  CG *cg = a64cgfunc->cg;

  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 regno = regOpnd->GetRegisterNumber();
  uint32 regsize = regOpnd->GetSize();
  PrimType stype = (phyopnd->IsOfIntClass()) ?
                       ((regsize <= k32) ? PTY_i32 : PTY_i64) :
                       ((regsize <= k32) ? PTY_f32 : PTY_f64);

  if (a64cgfunc->IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(spillMem), k64)) {
    //CHECK_FATAL(0, "NYI");
    regno_t pregNo = Riscv64Abi::kIntSpareReg;
    spillMem = a64cgfunc->SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(spillMem), k64,
                                                        (Riscv64reg_t)pregNo, insn);
  }
  Insn *stInsn =
      cg->BuildInstruction<Riscv64Insn>(a64cgfunc->PickStInsn(spillMem->GetSize(), stype), phyopnd, spillMem);
  stInsn->SetSpillOp();
  std::string comment = " SPILL for spill vreg: " + std::to_string(regno);
  stInsn->AddComment(comment);
  insn->bb->InsertInsnBefore(insn, stInsn);
}

void GraphColorRegAllocator::SpillOperandForSpillPost(Insn *insn, Operand *opnd, RegOperand *phyopnd, uint32 spillIdx,
                                                      bool needSpill) {
  if (needSpill == false) {
    return;
  }
  if (insn == nullptr) {
    return;
  }

  MemOperand *spillMem;
  spillMem = CreateSpillMem(spillIdx);
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc_);
  CG *cg = a64cgfunc->cg;
  bool lastInsn = false;
  if (insn->bb->kind == BB::kBBIf && insn->bb->lastinsn == insn) {
    lastInsn = true;
  }

  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 regno = regOpnd->GetRegisterNumber();
  uint32 regsize = regOpnd->GetSize();
  PrimType stype = (phyopnd->IsOfIntClass()) ?
                       ((regsize <= k32) ? PTY_i32 : PTY_i64) :
                       ((regsize <= k32) ? PTY_f32 : PTY_f64);

  bool isOutOfRange = false;
  if (a64cgfunc->IsImmediateOffsetOutOfRange(static_cast<Riscv64MemOperand *>(spillMem), k64)) {
    regno_t pregNo = Riscv64Abi::kIntSpareReg;
    // This split call will prepend extra instructions.  So try to prepend it to the next insn.
    // insn is either the spill of the def or the actual insn whose use opnd being spilled.
    spillMem = a64cgfunc->SplitOffsetWithAddInstruction(static_cast<Riscv64MemOperand *>(spillMem), k64,
                                                        (Riscv64reg_t)pregNo,
                                                        (insn->next) ? insn->next : insn);
    isOutOfRange = true;
  }
  Insn *ldInsn =
      cg->BuildInstruction<Riscv64Insn>(a64cgfunc->PickLdInsn(spillMem->GetSize(), stype), phyopnd, spillMem);
  ldInsn->SetSpillOp();
  std::string comment = " RELOAD for spill vreg: " + std::to_string(regno);
  ldInsn->AddComment(comment);
  if (lastInsn) {
    BB *newBB = nullptr;
    BB *tgtBB = nullptr;
    for (auto tbb : insn->bb->succs) {
      Insn *newLd = cg->BuildInstruction<Riscv64Insn>(a64cgfunc->PickLdInsn(spillMem->GetSize(), stype), phyopnd, spillMem);
      newLd->SetSpillOp();
      newLd->AddComment(comment);
      if (insn->bb == tbb || insn->bb->level > tbb->level) {
        // If it is a backward branch to itself or a block that is a predecessor.
        CHECK_FATAL(newBB == nullptr, "targetBB already created");
        tgtBB = tbb;
        newBB = cgfunc_->CreateNewBB();
        LabelIdx tgtLabel = cgfunc_->CreateLabel();
        newBB->AddLabel(tgtLabel);
        newBB->level = insn->bb->level + 1;
        cgfunc_->lab2bbmap[newBB->labidx] = newBB;
        cgfunc_->lastbb->AppendBB(newBB);
        cgfunc_->lastbb = newBB;
        int tgtIdx = insn->GetJumpTargetIdx();
        LabelOperand *origTarget = static_cast<LabelOperand*>(insn->opnds[tgtIdx]);
        LabelOperand *newTarget = cgfunc_->GetOrCreateLabelOperand(tgtLabel);
        insn->opnds[tgtIdx] = newTarget;
        newBB->AppendInsn(newLd);
        newBB->AppendInsn(cg->BuildInstruction<Riscv64Insn>(MOP_xuncond, origTarget));
      } else {
        tbb->InsertInsnBegin(newLd);
      }
    }
    if (newBB) {
      insn->bb->RemoveFromSuccessorList(tgtBB);
      insn->bb->succs.push_back(newBB);
      newBB->succs.push_back(tgtBB);

      tgtBB->RemoveFromPredecessorList(insn->bb);
      tgtBB->preds.push_back(newBB);
      newBB->preds.push_back(insn->bb);
    }
  } else {
    if (isOutOfRange && insn->next) {
      insn->bb->InsertInsnAfter(insn->next->next, ldInsn);
    } else {
      insn->bb->InsertInsnAfter(insn, ldInsn);
    }
  }
}

MemOperand *GraphColorRegAllocator::GetSpillOrReuseMem(LiveRange *lr, uint32 regsize, uint8 &isOutOfRange, Insn *insn,
                                                       bool isDef) {
  MemOperand *memopnd;
  if (lr->spillMem) {
    // the saved memopnd cannot be out-of-range
    memopnd = lr->spillMem;
  } else {
#ifdef REUSE_SPILLMEM
    memopnd = GetReuseMem(lr->regno, regsize, lr->regtype);
    if (memopnd) {
      lr->spillMem = memopnd;
      lr->spillSize = (regsize <= k32) ? k32 : k64;
    } else {
#endif  // REUSE_SPILLMEM
      regno_t baseRegNO;
      if (!isDef) {
        /* src will use its' spill reg as baseRegister when offset out-of-range
         * add x16, x29, #max-offset  //out-of-range
         * ldr x16, [x16, #offset]    //reload
         * mov xd, x16
         */
        baseRegNO = lr->spillReg;
        if ( baseRegNO > RLAST_INT_REG ) {
          baseRegNO = Riscv64Abi::kFpSpareReg;
        }
      } else {
        /* dest will use R17 as baseRegister when offset out-of-range
         * mov x16, xs
         * add x17, x29, #max-offset  //out-of-range
         * str x16, [x17, #offset]    //spill
         */
        baseRegNO = Riscv64Abi::kIntSpareReg;
      }
      ASSERT(baseRegNO != kRinvalid, "invalid base register number");
      memopnd = GetSpillMem(lr->regno, insn, (Riscv64reg_t)(baseRegNO), isOutOfRange, isDef);
#ifdef REUSE_SPILLMEM
      if (isOutOfRange == 0) {
        lr->spillMem = memopnd;
        lr->spillSize = (regsize <= k32) ? k32 : k64;
      }
    }
#endif  // REUSE_SPILLMEM
  }
  return memopnd;
}

// Create spill insn for the operand.
// When need_spill is true, need to spill the spill operand register first
// then use it for the current spill, then reload it again.
Insn *GraphColorRegAllocator::SpillOperand(Insn *insn, Operand *opnd, bool isDef, RegOperand *phyopnd,
                                           uint32 &spillIdx) {
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 regno = regOpnd->GetRegisterNumber();
  uint32 pregNo = phyopnd->GetRegisterNumber();
  if (GCRA_DUMP) {
    LogInfo::MapleLogger() << "InsertSpillReload " << regno << "\n";
  }

  uint32 regsize = regOpnd->GetSize();
  uint8 isOutOfRange = 0;
  PrimType stype;
  RegType regtype = regOpnd->GetRegisterType();
  if (regtype == kRegTyInt) {
    stype = (regsize <= k32) ? PTY_i32 : PTY_i64;
  } else {
    stype = (regsize <= k32) ? PTY_f32 : PTY_f64;
  }
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc_);
  CG *cg = a64cgfunc->cg;

  Insn *spillDefInsn = nullptr;
  MemOperand *memopnd;
  if (isDef) {
    LiveRange *lr = lrVec[regno];
    lr->spillReg = pregNo;
    memopnd = GetSpillOrReuseMem(lr, regsize, isOutOfRange, insn, true);
    spillDefInsn = cg->BuildInstruction<Riscv64Insn>(a64cgfunc->PickStInsn(regsize, stype), phyopnd, memopnd);
    spillDefInsn->SetSpillOp();
    std::string comment = " SPILL vreg: " + std::to_string(regno);
    spillDefInsn->AddComment(comment);
    if (insn->next && insn->next->GetMachineOpcode() == MOP_clinit_tail) {
      insn->bb->InsertInsnAfter(insn->next, spillDefInsn);
    } else {
      if (isOutOfRange) {
        insn->bb->InsertInsnAfter(insn->next->next, spillDefInsn);
      } else {
        insn->bb->InsertInsnAfter(insn, spillDefInsn);
      }
    }
  } else {
    if (insn->GetMachineOpcode() == MOP_clinit_tail) {
      return nullptr;
    }
    LiveRange *lr = lrVec[regno];
    Insn *spillUseInsn = nullptr;
    lr->spillReg = pregNo;
    memopnd = GetSpillOrReuseMem(lr, regsize, isOutOfRange, insn, false);
    spillUseInsn = cg->BuildInstruction<Riscv64Insn>(a64cgfunc->PickLdInsn(regsize, stype), phyopnd, memopnd);
    spillUseInsn->SetSpillOp();
    std::string comment = " RELOAD vreg: " + std::to_string(regno);
    spillUseInsn->AddComment(comment);
    insn->bb->InsertInsnBefore(insn, spillUseInsn);
  }

  if (spillDefInsn) {
    return spillDefInsn;
  } else {
    return insn;
  }
  return spillDefInsn;
}

// Try to find available reg for spill.
bool GraphColorRegAllocator::GetAvailableSpillReg(set<regno_t> &cannotUseReg, LiveRange *lr, uint64 &usedRegMask) {
  regno_t spillReg;
  RegType regtype = lr->regtype;
  if (regtype == kRegTyInt) {
    for (auto it : intCallerRegSet) {
      spillReg = it + R0;
      if (cannotUseReg.find(spillReg) == cannotUseReg.end() && (usedRegMask & (1LL << (spillReg))) == 0) {
        lr->assigned = spillReg;
        usedRegMask |= 1LL << (spillReg);
        return true;
      }
    }
    for (auto it : intCalleeRegSet) {
      spillReg = it + R0;
      if (cannotUseReg.find(spillReg) == cannotUseReg.end() && (usedRegMask & (1LL << (spillReg))) == 0) {
        lr->assigned = spillReg;
        usedRegMask |= 1LL << (spillReg);
        return true;
      }
    }

  } else if (regtype == kRegTyFloat) {
    for (auto it : fpCallerRegSet) {
      spillReg = it + V0;
      if (cannotUseReg.find(spillReg) == cannotUseReg.end() && (usedRegMask & (1LL << (spillReg))) == 0) {
        lr->assigned = spillReg;
        usedRegMask |= 1LL << (spillReg);
        return true;
      }
    }
    for (auto it : fpCalleeRegSet) {
      spillReg = it + V0;
      if (cannotUseReg.find(spillReg) == cannotUseReg.end() && (usedRegMask & (1LL << (spillReg))) == 0) {
        lr->assigned = spillReg;
        usedRegMask |= 1LL << (spillReg);
        return true;
      }
    }
  } else {
    CG_ASSERT(0, "Illegal regtype in GetAvailableSpillReg");
  }
  return false;
}

void GraphColorRegAllocator::CollectCannotUseReg(set<regno_t> &cannotUseReg, LiveRange *lr, Insn *insn) {
  // Find the bb in the conflict LR that actually conflicts with the current bb.
  for (regno_t regno = 0; regno < kNumPregBits; regno++) {
    if (lr->pregveto[regno]) {
      cannotUseReg.insert(regno);
    }
  }
  FOREACH_REG_ARR_ELEM(lr->bconflict, regno)
    LiveRange *conflictLr = lrVec[regno];
    // conflictLr->assigned might be zero
    // caller save will be inserted so the assigned reg can be released actually
    if ((conflictLr->assigned > 0) && IS_BIT_ARR_ELEM_SET(conflictLr->bmember, insn->bb->id)) {
      if (!Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)conflictLr->assigned) && conflictLr->numCall) {
        continue;
      }
      cannotUseReg.insert(conflictLr->assigned);
    }
  END_FOREACH_REG_ARR_ELEM
#ifdef USE_LRA
  if (doLRA) {
    BbAssignInfo *bbInfo = bbRegInfo[insn->bb->id];
    if (bbInfo) {
      for (auto it : bbInfo->regMap) {
        cannotUseReg.insert(it.second);
      }
    }
  }
#endif  // USE_LRA
}

// return true if need extra spill
bool GraphColorRegAllocator::PickRegForSpill(LiveRange *lr, Insn *insn, uint64 &usedRegMask, bool isDef) {
  bool needSpillLr = false;
  set<regno_t> cannotUseReg;

  // SPILL COALESCE
  if (isDef == false && (insn->GetMachineOpcode() == MOP_xmovrr || insn->GetMachineOpcode() == MOP_wmovrr)) {
    RegOperand *ropnd = static_cast<RegOperand *>(insn->GetOperand(0));
    if (ropnd->IsPhysicalRegister()) {
      lr->assigned = ropnd->GetRegisterNumber();
      return false;
    }
  }

  CollectCannotUseReg(cannotUseReg, lr, insn);

  if (GetAvailableSpillReg(cannotUseReg, lr, usedRegMask)) {
    return false;
  }

  if (!lr->assigned) {
    // All regs are assigned and none are free.
    // Pick a reg to spill and reuse for this spill.
    // Need to make sure the reg picked is not assigned to this insn,
    // else there will be conflict.
    regno_t base;
    regno_t spillReg;
    if (lr->regtype == kRegTyInt) {
      base = R0;
    } else {
      base = V0;
    }
    for (spillReg = (MaxIntPhysRegNum() + base); spillReg > base; spillReg--) {
      if ((usedRegMask & (1LL << (spillReg))) == 0) {
        lr->assigned = spillReg;
        usedRegMask |= 1LL << (spillReg);
        needSpillLr = true;
        break;
      }
    }
  }
  return needSpillLr;
}

regno_t GraphColorRegAllocator::PickRegForLocalSpill(regno_t regno, RegType regtype, uint64 &usedRegMask) {
  // use the reg that exclude livein/liveout/bbInfo->regMap

  // Need to make sure the reg picked is not assigned to this insn,
  // else there will be conflict.
  regno_t base;
  regno_t spillReg = 0;
  if (regtype == kRegTyInt) {
    base = R0;
  } else {
    base = V0;
  }
  for (spillReg = (MaxIntPhysRegNum() + base); spillReg > base; spillReg--) {
    if ((usedRegMask & (1LL << (spillReg))) == 0) {
      usedRegMask |= 1LL << (spillReg);
      break;
    }
  }
  if (GCRA_DUMP) {
    LogInfo::MapleLogger() << "\tassigning lra spill reg " << spillReg << endl;
  }
  return spillReg;
}

RegOperand *GraphColorRegAllocator::GetReplaceOpndForLRA(Insn *insn, Operand *opnd, uint32 &spillIdx,
                                                         uint64 &usedRegMask, bool isDef) {
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 vregno = regOpnd->GetRegisterNumber();
  RegType regtype = regOpnd->GetRegisterType();
  BbAssignInfo *bbInfo = bbRegInfo[insn->bb->id];
  if (bbInfo) {
    auto regIt = bbInfo->regMap.find(vregno);
    if (regIt != bbInfo->regMap.end()) {
      RegOperand *phyOpnd = static_cast<Riscv64CGFunc *>(cgfunc_)->GetOrCreatePhysicalRegisterOperand(
          (Riscv64reg_t)(regIt->second), regOpnd->GetSize(), regtype);
      return phyOpnd;
    } else {
      if (GCRA_DUMP) {
        LogInfo::MapleLogger() << "spill vreg " << vregno << endl;
      }
      regno_t spillReg;
      if ((insn->GetMachineOpcode() == MOP_clinit_tail) ||
          (insn->next && isDef && insn->next->GetMachineOpcode() == MOP_clinit_tail)) {
        spillReg = Riscv64Abi::kIntSpareReg;
      } else {
        spillReg = PickRegForLocalSpill(vregno, regtype, usedRegMask);
        bool isCalleeReg = Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)(spillReg));
        if (isCalleeReg) {
          if (regtype == kRegTyInt) {
            intCalleeUsed.insert((spillReg));
          } else {
            fpCalleeUsed.insert((spillReg));
          }
        }
      }
      RegOperand *phyOpnd = static_cast<Riscv64CGFunc *>(cgfunc_)->GetOrCreatePhysicalRegisterOperand(
          (Riscv64reg_t)(spillReg), regOpnd->GetSize(), regtype);
      SpillOperandForSpillPre(insn, regOpnd, phyOpnd, spillIdx, true);
      Insn *spill = SpillOperand(insn, regOpnd, isDef, phyOpnd, spillIdx);
      SpillOperandForSpillPost(spill, regOpnd, phyOpnd, spillIdx, true);
      spillIdx++;
      return phyOpnd;
    }
  }
  return nullptr;
}

// get spill reg and check if need extra spill
bool GraphColorRegAllocator::GetSpillReg(Insn *insn, LiveRange *lr, uint64 &usedRegMask, bool isDef) {
  bool needSpillLr = false;
  // Find a spill reg for the BB among interfereing LR.
  // Without LRA, this info is very inaccurate.  It will falsely interfere
  // with all locals which the spill might not be interfering.
  // For now, every instance of the spill requires a brand new reg assignment.
  if (GCRA_DUMP) {
    LogInfo::MapleLogger() << "LR-regno " << lr->regno << " spilled, finding a spill reg\n";
  }
  if (insn->IsBranch() || insn->IsCall() || (insn->GetMachineOpcode() == MOP_clinit_tail) ||
      (insn->next && isDef && insn->next->GetMachineOpcode() == MOP_clinit_tail)) {
    // When a cond branch reg is spilled, it cannot
    // restore the value after the branch since it can be the target from other br.
    // Todo it properly, it will require creating a intermediate bb for the reload.
    // Use x16, it is taken out from available since it is used as a global in the system.
    lr->assigned = Riscv64Abi::kIntSpareReg;
  } else {
    lr->assigned = 0;
    needSpillLr = PickRegForSpill(lr, insn, usedRegMask, isDef);
    bool isCalleeReg = Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)(lr->assigned));
    if (isCalleeReg) {
      if (lr->regtype == kRegTyInt) {
        intCalleeUsed.insert((lr->assigned));
      } else {
        fpCalleeUsed.insert((lr->assigned));
      }
    }
  }
  return needSpillLr;
}

RegOperand *GraphColorRegAllocator::GetReplaceOpnd(Insn *insn, Operand *opnd, uint32 &spillIdx, uint64 &usedRegMask,
                                                   bool isDef) {
  if (!opnd->IsRegister()) {
    return nullptr;
  }
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);

  uint32 vregno = regOpnd->GetRegisterNumber();
  RegType regtype = regOpnd->GetRegisterType();
  if (vregno < kMaxRegNum) {
    return nullptr;
  }
  if (IsUnconcernedReg(regOpnd)) {
    return nullptr;
  }

#ifdef USE_LRA
  if (doLRA && IsLocalReg(vregno)) {
    return GetReplaceOpndForLRA(insn, opnd, spillIdx, usedRegMask, isDef);
  }
#endif  // USE_LRA

  CG_ASSERT(vregno < lrVec.size(), "index out of range of MapleVector in GraphColorRegAllocator::GetReplaceOpnd");
  LiveRange *lr = lrVec[vregno];

  bool isSplitPart = false;
  bool needSpillLr = false;
  if (lr->splitLr && IS_BIT_ARR_ELEM_SET(lr->splitLr->bmember, insn->bb->id)) {
    isSplitPart = true;
  }

  if (lr->spilled && !isSplitPart) {
    needSpillLr = GetSpillReg(insn, lr, usedRegMask, isDef);
  }

  regno_t regno;
  if (isSplitPart) {
    regno = lr->splitLr->assigned;
  } else {
    regno = lr->assigned;
  }
  bool isCalleeReg = Riscv64Abi::IsCalleeSavedReg((Riscv64reg_t)regno);
  RegOperand *phyOpnd = static_cast<Riscv64CGFunc *>(cgfunc_)->GetOrCreatePhysicalRegisterOperand(
      (Riscv64reg_t)(regno), opnd->GetSize(), regtype);
  if (GCRA_DUMP) {
    LogInfo::MapleLogger() << "replace R" << vregno << " with R" << regno << endl;
  }

  std::string comment = insn->GetComment();
  comment += " [R" + std::to_string(vregno) + "] ";
  insn->AddComment(comment);

  if (isSplitPart && (isCalleeReg || lr->splitLr->numCall == 0)) {
    if (isDef) {
      SpillOperand(insn, opnd, isDef, phyOpnd, spillIdx);
      spillIdx++;
    } else {
      if (lr->splitLr->luMap[insn->bb->id]->needRlod) {
        SpillOperand(insn, opnd, isDef, phyOpnd, spillIdx);
        spillIdx++;
      }
    }
    return phyOpnd;
  }

  if (lr->spilled || (isSplitPart && lr->splitLr->numCall) || (lr->numCall && !isCalleeReg) ||
      (!isSplitPart && !(lr->spilled) && lr->luMap[insn->bb->id]->needRlod)) {
    SpillOperandForSpillPre(insn, regOpnd, phyOpnd, spillIdx, needSpillLr);
    Insn *spill = SpillOperand(insn, opnd, isDef, phyOpnd, spillIdx);
    SpillOperandForSpillPost(spill, regOpnd, phyOpnd, spillIdx, needSpillLr);
    spillIdx++;
  }

  return phyOpnd;
}

void GraphColorRegAllocator::MarkUsedRegs(Operand *opnd, uint64 &usedRegMask) {
  RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
  uint32 vregno = regOpnd->GetRegisterNumber();
  LiveRange *lr = lrVec[vregno];
  if (lr && lr->assigned) {
    usedRegMask |= (1LL << (lr->assigned));
  }
}

uint64 GraphColorRegAllocator::FinalizeRegisterPreprocess(FinalizeRegisterInfo *fInfo, Insn *insn) {
  uint64 usedRegMask = 0;
  const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->GetMachineOpcode()];
  for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
    Operand *opnd = insn->GetOperand(i);
    if (opnd == nullptr) {
      continue;
    }
    CG_ASSERT((md->operand_[i]) != nullptr, "pointer is null in GraphColorRegAllocator::FinalizeRegisters");

    if (opnd->IsList()) {
      // For arm32, not arm64
    } else if (opnd->IsMemoryAccessOperand()) {
      MemOperand *memopnd = static_cast<MemOperand *>(opnd);
      CG_ASSERT(memopnd != nullptr, "memopnd is null in colorRA FinalizeRegisters");
      Operand *base = memopnd->GetBaseRegister();
      if (base != nullptr) {
        fInfo->SetBaseOperand(opnd, i);
        MarkUsedRegs(base, usedRegMask);
      }
      Operand *offset = memopnd->GetIndexRegister();
      if (offset != nullptr) {
        fInfo->SetOffsetOperand(opnd);
        MarkUsedRegs(offset, usedRegMask);
      }
    } else {
      bool isDef = static_cast<Riscv64OpndProp *>(md->operand_[i])->IsRegDef();
      if (isDef) {
        fInfo->SetDefOperand(opnd, i);

        // Need to exclude def also, since it will clobber the result when the
        // original value is reloaded.
        MarkUsedRegs(opnd, usedRegMask);
      } else {
        fInfo->SetUseOperand(opnd, i);
        if (opnd->IsRegister()) {
          MarkUsedRegs(opnd, usedRegMask);
        }
      }
    }
  }  // operand
  return usedRegMask;
}

void GraphColorRegAllocator::GenerateSpillFillRegs(Insn *insn) {
  static regno_t intregs[3] = { R29, R30, R31 }; // R9 is used for large stack offset temp
  static regno_t fpregs[3] = { V29, V30, V31 };
  uint32 intUseCnt = 0;
  uint32 fpUseCnt = 0;
  for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
    Operand *opnd = insn->GetOperand(i);
    if (!opnd) {
      continue;
    }
    if (opnd->IsList()) {
      // call parameters
    } else if (opnd->IsMemoryAccessOperand()) {
      MemOperand *memopnd = static_cast<MemOperand *>(opnd);
      Riscv64RegOperand *base = static_cast<Riscv64RegOperand *>(memopnd->GetBaseRegister());
      if (base != nullptr && base->IsPhysicalRegister()) {
        regno_t regno = base->GetRegisterNumber();
        if (regno == intregs[0] || regno == intregs[1] || regno == intregs[2]) {
          intUseCnt++;
        }
      }
    } else {
      RegOperand *ropnd = static_cast<RegOperand *>(opnd);
      if (ropnd && ropnd->IsVirtualRegister() == false) {
        regno_t regno = ropnd->GetRegisterNumber();
        if (regno >= V0) {
          if (regno == fpregs[0] || regno == fpregs[1] || regno == fpregs[2]) {
            fpUseCnt++;
          }
        } else {
          if (regno == intregs[0] || regno == intregs[1] || regno == intregs[2]) {
            intUseCnt++;
          }
        }
      }
    }
  }
  intSpillFillRegs[0] = intregs[0] + intUseCnt;
  intSpillFillRegs[1] = intregs[1] + intUseCnt;
  intSpillFillRegs[2] = intregs[2] + intUseCnt;
  fpSpillFillRegs[0] = fpregs[0] + fpUseCnt;
  fpSpillFillRegs[1] = fpregs[1] + fpUseCnt;
  fpSpillFillRegs[2] = fpregs[2] + fpUseCnt;
}

RegOperand *GraphColorRegAllocator::CreateSpillFillCode(RegOperand *opnd, Insn *insn, uint32 spillCnt, bool isdef) {
  regno_t vregno = opnd->GetRegisterNumber();
  LiveRange *lr = lrVec[vregno];
  if (lr && lr->spilled) {
    Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc_);
    CG *cg = a64cgfunc->cg;
    uint32 bits = opnd->GetValidBitsNum();
    if (bits < 32) {
      bits = 32;
    }
    MemOperand *loadmem = a64cgfunc->GetOrCreatSpillMem(vregno);
    uint8 isOutOfRange;
    loadmem = a64cgfunc->AdjustMemOperandIfOffsetOutOfRange(loadmem, vregno, insn, R28, isOutOfRange, isdef);
    PrimType pty = (lr->regtype == kRegTyInt) ? ((bits > 32) ? PTY_i64 : PTY_i32)
                                              : ((bits > 32) ? PTY_f64 : PTY_f32);
    regno_t spreg = 0;
    RegType rtype = lr->regtype;
    if (spillCnt == 0) {
      // pregveto will take care of assignment, so pick a caller reg for temp
      GenerateSpillFillRegs(insn);
    }
    CHECK_FATAL(spillCnt < 3, "spill count exceeded");
    spreg = (rtype == kRegTyInt) ? intSpillFillRegs[spillCnt] : fpSpillFillRegs[spillCnt];
    RegOperand *regopnd = a64cgfunc->GetOrCreatePhysicalRegisterOperand(static_cast<Riscv64reg_t>(spreg), opnd->GetSize(), rtype);

    Insn *memInsn;
    if (isdef) {
      memInsn = cg->BuildInstruction<Riscv64Insn>(a64cgfunc->PickStInsn(bits, pty), regopnd, loadmem);
      memInsn->SetSpillOp();
      std::string comment = " SPILLcolor vreg: " + std::to_string(vregno);
      memInsn->AddComment(comment);
      if (isOutOfRange) {
        insn->bb->InsertInsnAfter(insn->next->next, memInsn);
      } else {
        insn->bb->InsertInsnAfter(insn, memInsn);
      }
    } else {
      memInsn = cg->BuildInstruction<Riscv64Insn>(a64cgfunc->PickLdInsn(bits, pty), regopnd, loadmem);
      memInsn->SetSpillOp();
      std::string comment = " RELOADcolor vreg: " + std::to_string(vregno);
      memInsn->AddComment(comment);
      insn->bb->InsertInsnBefore(insn, memInsn);
    }
    return regopnd;
  }
  return nullptr;
}

void GraphColorRegAllocator::SpillLiveRangeForSpills() {
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc_);
  CG *cg = a64cgfunc->cg;
  for (uint32_t bbIdx = 0; bbIdx < sortedBBs.size(); bbIdx++) {
    BB *bb = sortedBBs[bbIdx];
    FOR_BB_INSNS(insn, bb) {
      uint32 spillCnt;
      if (insn->IsImmaterialInsn() || !insn->IsMachineInstruction() || insn->id == 0) {
        continue;
      }
      spillCnt = 0;
      const Riscv64MD *md = &Riscv64CG::kMd[static_cast<Riscv64Insn *>(insn)->mop_];
      for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
        Operand *opnd = insn->GetOperand(i);
        if (!opnd) {
          continue;
        }
        if (opnd->IsList()) {
          // call parameters
        } else if (opnd->IsMemoryAccessOperand()) {
          MemOperand *newmemopnd = nullptr;
          MemOperand *memopnd = static_cast<MemOperand *>(opnd);
          RegOperand *base = static_cast<RegOperand *>(memopnd->GetBaseRegister());
          if (base != nullptr && base->IsVirtualRegister()) {
            RegOperand *replace = CreateSpillFillCode(base, insn, spillCnt);
            if (replace) {
              spillCnt++;
              newmemopnd = static_cast<MemOperand *>(static_cast<MemOperand *>(opnd)->Clone(cgfunc_->memPool));
              newmemopnd->SetBaseRegister(replace);
              insn->SetOperand(i, newmemopnd);
            }
          }
          RegOperand *offset = static_cast<RegOperand *>(memopnd->GetIndexRegister());
          if (offset != nullptr && offset->IsVirtualRegister()) {
            RegOperand *replace = CreateSpillFillCode(offset, insn, spillCnt);
            if (replace) {
              spillCnt++;
              if (newmemopnd == nullptr) {
                newmemopnd = static_cast<MemOperand *>(static_cast<MemOperand *>(opnd)->Clone(cgfunc_->memPool));
              }
              newmemopnd->SetIndexRegister(replace);
              insn->SetOperand(i, newmemopnd);
            }
          }
        } else if (opnd->IsRegister()) {
          bool isdef = static_cast<Riscv64OpndProp *>(md->operand_[i])->IsRegDef();
          RegOperand *replace = CreateSpillFillCode(static_cast<RegOperand *>(opnd), insn, spillCnt, isdef);
          if (replace) {
            if (isdef == false) {
              spillCnt++;
            }
            insn->SetOperand(i, replace);
          }
        }
      }
    }
  }
}

// Iterate through all instructions and change the vreg to preg.
void GraphColorRegAllocator::FinalizeRegisters() {
  if (doMultiPass && hasSpill) {
    SpillLiveRangeForSpills();
    return;
  }
  for (uint32_t bbIdx = 0; bbIdx < sortedBBs.size(); bbIdx++) {
    BB *bb = sortedBBs[bbIdx];
    FOR_BB_INSNS(insn, bb) {
      if (insn->IsImmaterialInsn()) {
        continue;
      }
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      if (insn->id == 0) {
        continue;
      }

      FinalizeRegisterInfo *fInfo = cgfunc_->memPool->New<FinalizeRegisterInfo>(allocator);
      uint64 usedRegMask = FinalizeRegisterPreprocess(fInfo, insn);

      uint32 spillIdx = 0;
      MemOperand *memopnd = nullptr;
      if (fInfo->baseOperand) {
        memopnd = static_cast<MemOperand *>(static_cast<MemOperand *>(fInfo->baseOperand)->Clone(cgfunc_->memPool));
        insn->SetOperand(fInfo->memOperandIdx, memopnd);
        Operand *base = memopnd->GetBaseRegister();
        RegOperand *phyOpnd = GetReplaceOpnd(insn, base, spillIdx, usedRegMask, false);
        if (phyOpnd) {
          memopnd->SetBaseRegister(phyOpnd);
        }
      }
      if (fInfo->offsetOperand) {
        CG_ASSERT(memopnd != nullptr, "mem operand cannot be null");
        Operand *offset = memopnd->GetIndexRegister();
        RegOperand *phyOpnd = GetReplaceOpnd(insn, offset, spillIdx, usedRegMask, false);
        if (phyOpnd) {
          memopnd->SetIndexRegister(phyOpnd);
        }
      }
      for (int32_t i = 0; i < fInfo->useOperands.size(); i++) {
        Operand *opnd = fInfo->useOperands[i];
        RegOperand *phyOpnd = GetReplaceOpnd(insn, opnd, spillIdx, usedRegMask, false);
        if (phyOpnd) {
          insn->SetOperand(fInfo->useIdx[i], phyOpnd);
        }
      }
      for (int32_t i = 0; i < fInfo->defOperands.size(); i++) {
        Operand *opnd = fInfo->defOperands[i];
        RegOperand *phyOpnd = GetReplaceOpnd(insn, opnd, spillIdx, usedRegMask, true);
        if (phyOpnd) {
          insn->SetOperand(fInfo->defIdx[i], phyOpnd);
        }
      }
    }  // insn
  }    // BB
}

void GraphColorRegAllocator::MarkCalleeSaveRegs() {
  for (auto regno : intCalleeUsed) {
    static_cast<Riscv64CGFunc *>(cgfunc_)->AddtoCalleeSaved((Riscv64reg_t)regno);
  }
  for (auto regno : fpCalleeUsed) {
    static_cast<Riscv64CGFunc *>(cgfunc_)->AddtoCalleeSaved((Riscv64reg_t)regno);
  }
}

bool GraphColorRegAllocator::AllocateRegisters() {
#ifdef RANDOM_PRIORITY
  // Change this seed for different random numbers
  srand(0);
#endif  // RANDOM_PRIORITY
  Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc_);

  // we store both FP/LR if using FP or if not using FP, but func has a call
  if (a64cgfunc->ShouldSaveFPLR()) {
    // Using FP, record it for saving
    a64cgfunc->AddtoCalleeSaved(RFP);
    a64cgfunc->AddtoCalleeSaved(RRA);
    a64cgfunc->NoteFPLRAddedToCalleeSavedList();
  }

  uint32 cnt = 0;
  FOR_ALL_BB(bb, cgfunc_) {
    FOR_BB_INSNS(insn, bb) {
      cnt++;
    }
  }
  CG_ASSERT(cnt <= cgfunc_->GetTotalNumberOfInstructions(), "Incorrect insn count");
  if ((cnt < kNumInsnThreashold) && CGOptions::doMultiPassColorRA) {
    doMultiPass = true;
  }

#ifdef PROPAGATE_REG
  if (cgfunc_->IsAfterRegAlloc() == false) {
    RaX0Opt x0Opt;
    x0Opt.PropagateX0(cgfunc_);
  }
  if (GCRA_DUMP) {
    LogInfo::MapleLogger() << "******** CG IR After PreColorRA: *********" << endl;
    cgfunc_->DumpCGIR();
    MIRModule &mirModule = cgfunc_->mirModule;
    DotGenerator::GenerateDot("RAe", cgfunc_, &mirModule, true);
  }
#endif

  if (doMultiPass) {
    doLRA = false;
    doOptProlog = false;
  }

  cgfunc_->SetIsAfterRegAlloc();
  // EBO propgation extent the live range and might need to be turned off.
  InitFreeRegPool();

  ComputeBlockOrder();

  ComputeLiveRanges();

#ifdef DO_PRE_LRA
  if (doLRA) {
    LocalRegisterAllocator(false);
  }
#endif  // DO_PRE_LRA

  BuildInterferenceGraph();

  Separate();

  SplitAndColor();

#ifdef USE_LRA
  if (doLRA) {
    LocalRegisterAllocator(true);
  }
#endif  // USE_LRA

  FinalizeRegisters();

  MarkCalleeSaveRegs();

  if (GCRA_DUMP) {
    cgfunc_->DumpCGIR();
  }

  if (doMultiPass && hasSpill) {
    return false;
  } else {
    return true;
  }
}

}  // namespace maplebe
