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


#include "riscv64_schedule.h"
#include "riscv64_cg.h"
#include "riscv64_operand.h"
#include "riscv64_dep_analysis.h"
#include "reg_pressure.h"

#include <time.h>

namespace maplebe {

void Riscv64Schedule::Init() {
  readyList.clear();
  nodeSize = nodes.size();
  lastSeparatorIndex = 0;
  DepNode *node = nodes[0];

  CG_ASSERT(node->GetType() == kNodeTypeSeparator, "CG internal error, the first node should be a separator node.");

  readyList.push_back(node);
  node->SetState(kReady);

  // Init validPredsSize and validSuccsSize.
  for (auto node : nodes) {
    node->SetValidPredsSize(node->GetPreds().size());
    node->SetValidSuccsSize(node->GetSuccs().size());
  }
}

void Riscv64Schedule::MemoryAccessPairOpt(BB *bb) {
}

/* combine clinit pairs.
 */
void Riscv64Schedule::ClinitPairOpt() {
  uint32 i = 0;
  for (auto it = nodes.begin(); it != nodes.end(); it++, i++) {
    auto nextIt = std::next(it, 1);
    if (nextIt == nodes.end()) {
      return;
    }

    if ((*it)->GetInsn()->GetMachineOpcode() == MOP_adrp_ldr) {
      if ((*nextIt)->GetInsn()->GetMachineOpcode() == MOP_clinit_tail) {
        depAnalysis->CombineClinit(*it, *(nextIt), false);
      } else if ((*nextIt)->GetType() == kNodeTypeSeparator){
        nextIt = std::next(nextIt, 1);
        if (nextIt == nodes.end()) {
          return;
        }

        if ((*nextIt)->GetInsn()->GetMachineOpcode() == MOP_clinit_tail) {
          // Do something.
          depAnalysis->CombineClinit(*it, *(nextIt), true);
        }
      }
    }
  }
}

uint32 Riscv64Schedule::GetNextSepIndex() const {
  uint32 nextSepIndex = (lastSeparatorIndex + kMaxDependenceNum) < nodeSize
                        ? (lastSeparatorIndex + kMaxDependenceNum)
                        : nodes.size() - 1;
  return nextSepIndex;
}

void Riscv64Schedule::RegPressureScheduling(const BB *bb, std::vector<DepNode*> &nodes) {
  RegPressureSchedule* regSchedule = mp->New<RegPressureSchedule>(cgfunc, mp);
  // Get physical register amount currently
  // undef, Int Reg, Floag Reg, Flag Reg
  const int kregNum[] = {0/*, RFLAG*/, kMaxRegNum-V0+1, 1};
  regSchedule->InitBBInfo(bb, mp, nodes);
  regSchedule->BuildPhyRegInfo(kregNum, sizeof(kregNum));
  regSchedule->DoScheduling(nodes);
}

/* Compute earliest start of the node.
   return value : the maximum estart.
 */
uint32 Riscv64Schedule::ComputeEstart(uint32 cycle) {

  std::vector<DepNode*> readyNodes;
  uint32 maxIndex = GetNextSepIndex();

  // Check validPredsSize.
  for (uint32 i = lastSeparatorIndex; i <= maxIndex; i++) {
    DepNode *node = nodes[i];
    int schedNum = 0;
    for (auto predLink : node->GetPreds()) {
      if (predLink->GetFrom()->GetState() == kScheduled)
        schedNum ++;
    }
    CG_ASSERT((node->GetPreds().size() - schedNum) == node->GetValidPredsSize(), "validPredsSize error.");
  }

  CG_ASSERT(nodes[maxIndex]->GetType() == kNodeTypeSeparator,
            "CG internal error, nodes[maxIndex] should be a separator node.");

  readyNodes.reserve(maxIndex - lastSeparatorIndex + 1);
  readyNodes = readyList;

  uint32 maxEstart = cycle;

  for (uint32 i = lastSeparatorIndex; i <= maxIndex; i++) {
    DepNode *node = nodes[i];
    node->SetVisit(0);
  }

  for (auto node : readyNodes) {
    CG_ASSERT(node->GetState() == kReady, "CG internal error, all nodes in ready list should be ready.");
    node->SetEStart(cycle);
  }

  for (auto it = readyNodes.begin(); it != readyNodes.end(); it++) {
    DepNode *node = *it;
    for (auto succLink : node->GetSuccs()) {
      DepNode *succNode = succLink->GetTo();
      if (succNode->GetType() == kNodeTypeSeparator)
        continue;

      if (succNode->GetEStart() < node->GetEStart() + succLink->GetLatency()) {
        succNode->SetEStart(node->GetEStart() + succLink->GetLatency());
        maxEstart = (maxEstart < succNode->GetEStart() ? succNode->GetEStart() : maxEstart);
      }
      succNode->IncreaseVisit();
      if (succNode->GetVisit() >= succNode->GetValidPredsSize() && succNode->GetType() != kNodeTypeSeparator) {
        readyNodes.push_back(succNode);
      }
      CG_ASSERT(succNode->GetVisit() <= succNode->GetValidPredsSize(), "CG internal error.");
    }
  }
  return maxEstart;
}

/* Compute latest start of the node.
 */
void Riscv64Schedule::ComputeLstart(uint32 maxEstart) {
  std::vector<DepNode*> readyNodes;
  uint32 maxIndex = GetNextSepIndex();

  CG_ASSERT(nodes[maxIndex]->GetType() == kNodeTypeSeparator,
            "CG internal error, nodes[maxIndex] should be a separator node.");

  readyNodes.reserve(maxIndex - lastSeparatorIndex + 1);

  for (uint32 i = lastSeparatorIndex; i <= maxIndex; i++) {
    DepNode *node = nodes[i];
    node->SetLStart(maxEstart);
    node->SetVisit(0);
  }

  readyNodes.push_back(nodes[maxIndex]);

  for (auto it = readyNodes.begin(); it != readyNodes.end(); it++) {
    DepNode *node = *it;
    for (auto predLink : node->GetPreds()) {
      DepNode *predNode = predLink->GetFrom();
      if (predNode->GetState() == kScheduled) {
        continue;
      }

      if (predNode->GetLStart() > node->GetLStart() - predLink->GetLatency()) {
        predNode->SetLStart(node->GetLStart() - predLink->GetLatency());
      }
      predNode->IncreaseVisit();
      if (predNode->GetVisit() >= predNode->GetValidSuccsSize() && predNode->GetType() != kNodeTypeSeparator) {
        readyNodes.push_back(predNode);
      }

      CG_ASSERT(predNode->GetVisit() <= predNode->GetValidSuccsSize(), "CG internal error.");
    }
  }
}

void Riscv64Schedule::UpdateELStartsOnCycle(uint32 cycle) {
  ComputeLstart(ComputeEstart(cycle));
}

bool DepNode::CanBeScheduled() const {
  for (int i = 0; i < unitNum; i++) {
    Unit *unit = units[i];
    if (unit) {
      if (!unit->IsFree(i)) {
        return false;
      }
    }
  }

  return true;
}

void DepNode::OccupyUnits() {
  for (int i = 0; i < unitNum; i++) {
    Unit *unit = units[i];
    if (unit) {
      unit->Occupy(insn, i);
    }
  }
}

void Riscv64Schedule::RandomTest() {
  Init();
  nodes.clear();

  while (!readyList.empty()) {
    DepNode *currNode = readyList.back();
    currNode->SetState(kScheduled);
    readyList.pop_back();
    nodes.push_back(currNode);

    for (auto succLink : currNode->GetSuccs()) {
      DepNode *succNode = succLink->GetTo();
      bool ready = true;
      for (auto predLink : succNode->GetPreds()) {
        DepNode *predNode = predLink->GetFrom();
        if (predNode->GetState() != kScheduled) {
          ready = false;
          break;
        }
      }

      if (ready) {
        CG_ASSERT(succNode->GetState() == kNormal, "");
        readyList.push_back(succNode);
        succNode->SetState(kReady);
      }
    }
  }
}

void Riscv64Schedule::EraseNodeFromReadyList(const DepNode *target) {
  for (auto it = readyList.begin(); it != readyList.end(); it++) {
    if ((*it) == target) {
      readyList.erase(it);
      return;
    }
  }

  CG_ASSERT(false, "CG internal error, erase node fail.");
}

/* After building dependence graph, schedule insns.
 */
void Riscv64Schedule::DoSchedule() {
  vector<DepNode*> availableReadyList;
  vector<DepNode*> tempReadyList;
  vector<DepNode*> scheduledNodes;

  uint32 lastUpdateCycle = 0;
  uint32 currCycle = 0;
  bool advanceCycle = false;
  bool isFirstSeparator = true;

  Init();

  availableReadyList.reserve(nodeSize);
  tempReadyList.reserve(nodeSize);

  UpdateELStartsOnCycle(currCycle);

  while (!readyList.empty()) {
    if (advanceCycle) {
      currCycle++;
      mad->AdvanceCycle();
      advanceCycle = false;
    }
    tempReadyList.clear();
    availableReadyList.clear();

    // Check if schedulable
    for (auto node : readyList) {
      if (node->CanBeScheduled()) {
        availableReadyList.push_back(node);
      }
    }

    if (availableReadyList.empty()) {
      // Advance cycle.
      advanceCycle = true;
      continue;
    }

    if (lastUpdateCycle < currCycle) {
      UpdateELStartsOnCycle(currCycle);
      lastUpdateCycle = currCycle;
    }

    // Check EStart.
    for (auto node : availableReadyList) {
      if (node->GetEStart() <= currCycle) {
        tempReadyList.push_back(node);
      }
    }

    if (tempReadyList.empty()) {
      // Advance cycle.
      advanceCycle = true;
      continue;
    } else {
      availableReadyList = tempReadyList;
      tempReadyList.clear();
    }

    // Select minimum LStart.
    if (availableReadyList.size() > 1) {
      uint32 minlStart = -1;
      for (auto node : availableReadyList) {
        if (minlStart > node->GetLStart()) {
          minlStart = node->GetLStart();
        }
      }

      for (auto node : availableReadyList) {
        if (minlStart == node->GetLStart()) {
          tempReadyList.push_back(node);
        }
      }

      CG_ASSERT(tempReadyList.size() >= 1, "CG internal error.");
      availableReadyList = tempReadyList;
      tempReadyList.clear();
    }

    // Select slot0 first.
    if (mad->IsSlot0Free() && availableReadyList.size() > 1) {
      for (auto node : availableReadyList) {
        enum SlotType slotType = node->GetReservation()->GetSlot();
        if (slotType == kSlot0 || slotType == kSlots) {
          tempReadyList.push_back(node);
        }
      }

      if (!tempReadyList.empty()) {
        availableReadyList = tempReadyList;
        tempReadyList.clear();
      }
    }

    // Select first node.
    DepNode *targetNode = availableReadyList.front();
    targetNode->SetState(kScheduled);
    scheduledNodes.push_back(targetNode);

    EraseNodeFromReadyList(targetNode);
    targetNode->OccupyUnits();

    // Update readyList.
    for (auto succLink : targetNode->GetSuccs()) {
      DepNode *succNode = succLink->GetTo();
      succNode->DescreaseValidPredsSize();
      if (succNode->GetValidPredsSize() == 0) {
        readyList.push_back(succNode);
        succNode->SetState(kReady);
      }
    }

    if (targetNode->GetType() == kNodeTypeSeparator) {
      // If target node is separator node, update lastSeparatorIndex.
      if (!isFirstSeparator) {
        lastSeparatorIndex += kMaxDependenceNum;
      } else {
        isFirstSeparator = false;
      }
    }

    if (mad->IsFullIssued()) {
      advanceCycle = true;
    }
  }

  CG_ASSERT(scheduledNodes.size() == nodes.size(), "CG internal error, Not all nodes scheduled.");

  nodes = scheduledNodes;
}

/* Restore dependence graph to normal CGIR.
 */
void Riscv64Schedule::FinalizeScheduling(BB *bb, const DepAnalysis *depAnalysis) {
  bb->ClearInsns();

  for (auto node : nodes) {
    // Append comments first.
    for (auto comment : node->GetComments()) {
      bb->AppendInsn(comment);
    }

    // Append insn.
    if (!node->GetClinitInsns().empty()) {
      for (auto clinit : node->GetClinitInsns()) {
        bb->AppendInsn(clinit);
      }
    } else if (node->GetType() == kNodeTypeNormal) {
      bb->AppendInsn(node->GetInsn());
    }

    // Append cfi instructions.
    for (auto cfi : node->GetCfiInsns()) {
      bb->AppendInsn(cfi);
    }
  }

  for (auto lastComment : depAnalysis->GetLastComments()) {
    bb->AppendInsn(lastComment);
  }
}

void Riscv64Schedule::DumpDepGraph(const std::vector<DepNode*> &nodes) const {
  for (auto node : nodes) {
    depAnalysis->dumpDepNode(node);
    LogInfo::MapleLogger() << "---------- preds ----------" << endl;
    for (auto pred : node->GetPreds()) {
      depAnalysis->dumpDepLink(pred, pred->GetFrom());
    }
    LogInfo::MapleLogger() << "---------- succs ----------" << endl;
    for (auto succ : node->GetSuccs()) {
      depAnalysis->dumpDepLink(succ, succ->GetTo());
    }
    LogInfo::MapleLogger() << "---------------------------" << endl;
  }
}

void Riscv64Schedule::GenerateDot(const BB *bb, const std::vector<DepNode*> &nodes) const {
  streambuf *coutbuf = LogInfo::MapleLogger().rdbuf();  /* keep original LogInfo::MapleLogger() buffer */
  ofstream dgFile;
  streambuf *buf = dgFile.rdbuf();
  LogInfo::MapleLogger().rdbuf(buf);

  // construct the file name
  std::string fileName;
  fileName.append(phaseName);
  fileName.append("_");
  fileName.append(cgfunc->GetName().c_str());
  fileName.append("_BB");
  auto str = std::to_string(bb->id);
  fileName.append(str.c_str());
  fileName.append("_dep_graph.dot");

  dgFile.open(fileName.c_str(), ios::trunc);
  dgFile << "digraph {\n";
  for (auto node : nodes) {
    for (auto succ : node->GetSuccs()) {
      dgFile << "insn" << node->GetInsn() << " -> " << "insn" << succ->GetTo()->GetInsn();
      dgFile << " [";
      if (succ->GetDepType() == kDependenceTypeTrue) {
        dgFile << "color=red,";
      }
      dgFile << "label= \"" << succ->GetLatency() << "\"";
      dgFile << "];\n";
    }
  }

  for (auto node : nodes) {
    MOperator mop = node->GetInsn()->GetMachineOpcode();
    const Riscv64MD *md = &Riscv64CG::kMd[mop];
    dgFile << "insn" << node->GetInsn() << "[";
    dgFile << "shape=box,label= \" " << node->GetInsn()->id << ":\n";
    dgFile << "{ ";
    dgFile << md->name_ << "\n";
    dgFile << "}\"];\n";
  }
  dgFile << "}\n";
  dgFile.flush();
  dgFile.close();
  LogInfo::MapleLogger().rdbuf(coutbuf);
}

//#define TimeCalculate

/* A local list scheduling.
   Schedule insns in basic blocks.
 */
void Riscv64Schedule::ListScheduling(bool beforeRA) {
  MarkInsnId();

  mad = g->mad;
  depAnalysis = mp->New<Riscv64DepAnalysis>(cgfunc, mp, beforeRA);
  RegPressure::SetMaxRegClassNum(kRegisterLast);

  FOR_ALL_BB(bb, cgfunc) {
#if TimeCalculate
    clock_t startTime;
    clock_t endTime;
    startTime = clock();
#endif //TimeCalculate
    depAnalysis->Run(bb, nodes);
#if TimeCalculate
    endTime = clock();
    uint32 bbsize;
    if (bb->firstinsn == nullptr) {
      bbsize = 0;
    } else {
      bbsize = bb->lastinsn->id - bb->firstinsn->id + 1;
    }
    LogInfo::MapleLogger() << "BB id = " << bb->id << "; BB size = " << bbsize << "; ";
    LogInfo::MapleLogger() << "Dependence analysis Time : " << (endTime - startTime) << "ms" << std::endl;
#endif //TimeCalculate
    ClinitPairOpt();
    if (LISTSCHEDDUMP) {
      GenerateDot(bb, nodes);
    }
    if (beforeRA) {
      MemoryAccessPairOpt(bb);
      RegPressureScheduling(bb, nodes);
    } else {
#if TimeCalculate
      startTime = clock();
#endif //if
      DoSchedule();
#if TimeCalculate
      endTime = clock();
      LogInfo::MapleLogger() << "Schedule Time : " << (endTime - startTime) << "ms" << std::endl;
#endif //if
    }

    FinalizeScheduling(bb, depAnalysis);
  }
}

}  // namespace maplebe
