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

#include "schedule.h"
#include "cg.h"

namespace maplebe {
using namespace std;

/* ---- RegPressureSchedule function ----*/
void RegPressureSchedule::InitBBInfo(const BB *b, MemPool *mp, const std::vector<DepNode*> &nodes) {
  bb = b;
  liveReg.clear();
  scheduledNode.clear();
  readyList.clear();
  maxpriority = 0;
  maxPressure = mp->NewArray<int>(RegPressure::GetMaxRegClassNum());
  curPressure = mp->NewArray<int>(RegPressure::GetMaxRegClassNum());
  physicalRegNum = mp->NewArray<int>(RegPressure::GetMaxRegClassNum());
  for (auto node : nodes) {
    node->SetState(kNormal);
  }
}

// Get amount of every physical register
void RegPressureSchedule::BuildPhyRegInfo(const int *regNum, size_t regNumLength) {
  FOR_ALL_REGCLASS(i) {
    physicalRegNum[i] = regNum[i];
  }
}

void RegPressureSchedule::InitReadyList(std::vector<DepNode*> &nodes) {
  for (auto node : nodes) {
    // If a node is not been scheduled and there is no pred node of the node, add the node reaylist.
    if (node->GetState()  == kNormal && node->GetPreds().empty()) {
      readyList.push_back(node);
      node->SetState(kReady);
    }
  }
  return;
}

// initialize register pressure information according to bb's live-in data.
void RegPressureSchedule::InitPressure() {
  FOR_ALL_REGCLASS(i) {
    curPressure[i] = 0;
  }
  // add all bb's live-in register to liveReg.
  for (auto reg : bb->livein_regno) {
    RegType rty = cgfunc->GetRegisterType(reg);
    if (liveReg.find(reg) == liveReg.end()) {
      liveReg.insert(reg);
      // increase the pressure of the register type.
      curPressure[rty]++;
    }
  }

  FOR_ALL_REGCLASS(i) {
    maxPressure[i] = curPressure[i];
  }
  return;
}

void RegPressureSchedule::SortReadyList() {
  std::sort(readyList.begin(), readyList.end(), DepNodePriorityCmp);
}

// return true if nodes1 first.
bool RegPressureSchedule::DepNodePriorityCmp(const DepNode *node1, const DepNode *node2) {
  if (node1->GetPriority() != node2->GetPriority()) {
    return node1->GetPriority() > node2->GetPriority();
  }

  int depths1 = node1->GetMaxDepth() + node1->GetNear();
  int depths2 = node2->GetMaxDepth() + node2->GetNear();
  if (depths1 != depths2) {
    return depths1 > depths2;
  }

  if (node1->GetNear() != node2->GetNear()) {
    return node1->GetNear() > node2->GetNear();
  }

  //return node1 < node2;
  return (node1->GetInsn()->id < node1->GetInsn()->id);
}

// calculate a node register pressure base on current scheduling
void RegPressureSchedule::ReCalculateDepNodePressure(DepNode *node) {
  // initialize
  node->InitPressure();
  // calculate the node uses'register pressure
  for (auto reg : node->GetUses()) {
    CalculatePressure(node, reg, false);
  }
  // calculate the node defs'register pressure
  for (auto reg : node->GetDefs()) {
    CalculatePressure(node, reg, true);
  }

  // if there is a type of register pressure increases, set incPressure as true.
  const int *pressure = node->GetPressure();
  FOR_ALL_REGCLASS(i) {
    if (pressure[i] > 0) {
      node->SetIncPressure(true);
      break;
    }
  }
  return;
}

// calculate the maxDepth of every node in nodes.
void RegPressureSchedule::CalculateMaxDepth(std::vector<DepNode*> &nodes) {
  // from the last node to first node.
  for (auto rit = nodes.rbegin(); rit != nodes.rend(); ++rit) {
    // traversing each successor of rit.
    for (auto succ : (*rit)->GetSuccs()) {
      DepNode *to = succ->GetTo();
      if ((*rit)->GetMaxDepth() < to->GetMaxDepth()+1) {
        (*rit)->SetMaxDepth(to->GetMaxDepth()+1);
      }
    }
  }
}

// calculate the near of every successor of the node.
void RegPressureSchedule::CalculateNear(DepNode *node) {
  for (auto succ : node->GetSuccs()) {
    DepNode *to = succ->GetTo();
    if (to->GetNear() < node->GetNear()+1) {
      to->SetNear(node->GetNear()+1);
    }
  }
}

// return true if it is last time using the regno.
bool RegPressureSchedule::IsLastUse(const DepNode *node, regno_t regno) const {
  auto it = node->GetRegUses().find(regno);
  CG_ASSERT(it != node->GetRegUses().end(), "");
  RegList *regList = it->second;
  if (bb->liveout_regno.find(regno) != bb->liveout_regno.end()) {
    return false;
  }

  // except the node, if there are insn that has no scheduled in regno'sregList,
  // then it is not the last time using the regno, return false.
  while (regList) {
    DepNode *useNode = regList->insn->depNode;
    if (regList->insn != node->GetInsn()
        && useNode->GetState() != kScheduled) {
      return false;
    }
    regList = regList->next;
  }
  return true;
}

void RegPressureSchedule::CalculatePressure(DepNode *node,
    regno_t reg, bool def) {
  RegType rty = cgfunc->GetRegisterType(reg);
  // if def a register, register pressure increase.
  if (def) {
    if (liveReg.find(reg) == liveReg.end()) {
      node->IncPressureByIndex(rty);
    }
  } else {
    // if it is the last time using the reg, register pressure decrease.
    if (IsLastUse(node, reg)) {
      if (liveReg.find(reg) != liveReg.end()) {
        node->DecPressureByIndex(rty);
      }
    }
  }
  return;
}

// update live reg information.
void RegPressureSchedule::UpdateLiveReg(DepNode *node, regno_t reg, bool def) {
  RegType rty = cgfunc->GetRegisterType(reg);
  if (def) {
    if (liveReg.find(reg) == liveReg.end()) {
      liveReg.insert(reg);
    }
  } else {
    if (IsLastUse(node, reg)) {
      if (liveReg.find(reg) != liveReg.end()) {
        liveReg.erase(reg);
      }
    }
  }
}

// update register pressure information.
void RegPressureSchedule::UpdateBBPressure(DepNode *node) {
  const std::set<regno_t> uses = node->GetUses();
  const std::set<regno_t> defs = node->GetDefs();

  for (auto reg : uses) {
    UpdateLiveReg(node, reg, false);
  }
  for (auto reg : defs) {
    UpdateLiveReg(node, reg, true);
  }
  const int *pressure = node->GetPressure();
  FOR_ALL_REGCLASS(i) {
    curPressure[i] += pressure[i];
    if (curPressure[i] > maxPressure[i]) {
      maxPressure[i] = curPressure[i];
    }
  }
}

// update node priority and try to update the priority of all node's ancestor.
void RegPressureSchedule::UpdatePriority(DepNode *node) {
  std::queue<DepNode*> workQueue;
  workQueue.push(node);
  node->SetPriority(maxpriority++);
  do {
    DepNode *nowNode = workQueue.front();
    workQueue.pop();
    for (auto pred : nowNode->GetPreds()) {
      DepNode *from = pred->GetFrom();
      if (from->GetState() != kScheduled) {
        from->SetPriority(maxpriority);
        workQueue.push(from);
      }
    }
  } while (!workQueue.empty());

}

// return true if all node's pred has been scheduled.
bool RegPressureSchedule::CanSchedule(const DepNode *node) const {
  for (auto pred : node->GetPreds()) {
    DepNode *from = pred->GetFrom();
    if (from->GetState() != kScheduled) {
      return false;
    }
  }
  return true;
}

/* add the successor of node to readyList when
 *  1. successor has no been scheduled;
 *  2. successor's has been scheduled or the dependence between node and successor is true-dependence.
 */
void RegPressureSchedule::UpdateReadyList(DepNode *node) {
  for (auto succ : node->GetSuccs()) {
    DepNode *succNode = succ->GetTo();
    if ((/*succ->GetDepType() == kDependenceTypeTrue || */
        CanSchedule(succNode)) &&
        succNode->GetState() == kNormal){
      readyList.push_back(succNode);
      succNode->SetState(kReady);
    }
  }
  return;
}

// choose a node to schedule
DepNode *RegPressureSchedule::ChooseNode() {
  DepNode *node = nullptr;
  DepNode *intNode = nullptr;
  for (auto it = readyList.begin(); it != readyList.end(); it++) {
    if ((*it)->GetIncPressure() == false) {
      if (CanSchedule(*it) == true) {
        return *it;
      } else if (node == nullptr) {
        node = *it;
      }
    }
  }
  if (node == nullptr) {
    node = readyList.front();
  }
  return node;
}

void RegPressureSchedule::DoScheduling(std::vector<DepNode*> &nodes) {
  // initialize register pressure information and readylist.
  InitPressure();
  InitReadyList(nodes);
  CalculateMaxDepth(nodes);
  SortReadyList();
  while(!readyList.empty()) {
    // calculate register pressure
    for (auto it = readyList.begin(); it != readyList.end(); it++) {
      ReCalculateDepNodePressure(*it);
    }

    // choose a node can be scheduled currently.
    DepNode *node = ChooseNode();
    while (!CanSchedule(node)) {
      UpdatePriority(node);
      SortReadyList();
      node = readyList.front();
    }

    scheduledNode.push_back(node);
    // mark node has scheduled
    node->SetState(kScheduled);
    UpdateBBPressure(node);
    CalculateNear(node);
    // delete node from readylist
    for (auto it = readyList.begin(); it != readyList.end(); it++) {
      if (*it == node) {
        readyList.erase(it);
        break;
      }
    }
    UpdateReadyList(node);
    SortReadyList();
  }

  // update nodes according to scheduledNode.
  nodes.clear();
  for (auto node : scheduledNode) {
    nodes.push_back(node);
  }
}

int RegPressureSchedule::RegPressureCost(const DepNode *node) const {
  int cost = 0;
  const int *pressure = node->GetPressure();
  FOR_ALL_REGCLASS(i) {
    int maxRegNum = physicalRegNum[i] > maxPressure[i] ?
                     physicalRegNum[i] : maxPressure[i];
    int curP = curPressure[i] + pressure[i];

    if (pressure[i] > 0 && curP > maxRegNum) {
      cost += ((curP-maxRegNum) < pressure[i] ? (curP-maxRegNum) : pressure[i]);
    } else if (pressure[i] < 0 && curPressure[i] > physicalRegNum[i]) {
      cost += (pressure[i] > (physicalRegNum[i]-curPressure[i]) ?
        pressure[i] : (physicalRegNum[i]-curPressure[i]));
    }
  }
  return cost;
}

bool RegPressureSchedule::CompareRegPressureCost(const DepNode *node1, const DepNode *node2) const {
  int cost1 = RegPressureCost(node1);
  int cost2 = RegPressureCost(node2);

  return cost1 < cost2;
}


/* ------------- Schedule function ----------*/
void Schedule::MarkInsnId() {
  uint32 id = 0;

  FOR_ALL_BB(bb, cgfunc) {
    FOR_BB_INSNS(insn, bb) {
      insn->id = id++;
    }
  }
}

AnalysisResult* CgDoPreScheduling::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (LISTSCHEDDUMP) {
    LogInfo::MapleLogger() << "Before CgDoPreScheduling : " << cgfunc->GetName() << endl;
    DotGenerator::GenerateDot("preschedule", cgfunc, &(cgfunc->mirModule), true);
  }

  LiveAnalysis *live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));

  MemPool *scheduleMp = mempoolctrler.NewMemPool(PhaseName().c_str());
  CHECK_FATAL(scheduleMp != nullptr, "schedule_mp is null in CgDoPreScheduling::Run");

  Schedule *schd = cgfunc->NewSchedule(cgfunc, scheduleMp, live, PhaseName().c_str());
  CHECK_FATAL(schd != nullptr, "scheduling is null in CgDoPreScheduling::Run");

  schd->ListScheduling(true);

  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  mempoolctrler.DeleteMemPool(scheduleMp);

  return nullptr;
}

AnalysisResult* CgDoScheduling::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (LISTSCHEDDUMP) {
    LogInfo::MapleLogger() << "Before CgDoScheduling : " << cgfunc->GetName() << endl;
    DotGenerator::GenerateDot("scheduling", cgfunc, &(cgfunc->mirModule), true);
  }

  LiveAnalysis *live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));

  MemPool *scheduleMp = mempoolctrler.NewMemPool(PhaseName().c_str());
  CHECK_FATAL(scheduleMp != nullptr, "schedule_mp is null in CgDoScheduling::Run");

  Schedule *schd = cgfunc->NewSchedule(cgfunc, scheduleMp, live, PhaseName().c_str());
  CHECK_FATAL(schd != nullptr, "scheduling is null in CgDoScheduling::Run");

  schd->ListScheduling(false);

  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  mempoolctrler.DeleteMemPool(scheduleMp);

  return nullptr;
}

}  // namespace maplebe
