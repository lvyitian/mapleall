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


#include "aarch64_cg.h"
#include "aarch64_operand.h"
#include "aarch64_dep_analysis.h"
#include "reg_pressure.h"
#include "special_func.h"

namespace maplebe {

/* constructor */
AArch64DepAnalysis::AArch64DepAnalysis(CGFunc *func, MemPool *p, bool beforeRA)
    : DepAnalysis(func, p, beforeRA), regDefs(nullptr), regUses(nullptr),
      memBarInsn(nullptr), hasAmbiRegs(false), lastCallInsn(nullptr), separatorIndex(0),
      useRegnos(), defRegnos(), stackUses(), stackDefs(), heapUses(), heapDefs(),
      mayThrows(), ambiInsns(), ehInRegs() {
  uint32 maxRegNum;
  if (beforeRA) {
    maxRegNum = cgfunc->GetMaxVReg();
  } else {
    maxRegNum = kMaxRegNum;
  }

  regDefs = mp->NewArray<Insn *>(maxRegNum);
  regUses = mp->NewArray<RegList *>(maxRegNum);
  useRegnos.reserve(Insn::kMaxOperandNum);
  defRegnos.reserve(Insn::kMaxOperandNum);
  mad = g->mad;
}

// print dep node information
void AArch64DepAnalysis::dumpDepNode(DepNode *node) {
  CG_ASSERT(node, "");
  node->GetInsn()->dump();
  int num = node->GetUnitNum();
  LogInfo::MapleLogger() << "unit num : " << num << ", ";
  for (int i = 0; i < num; i++) {
    PRINT_VAL(node->GetUnitByIndex(i)->GetUnitId());
  }
  LogInfo::MapleLogger() << endl;
  node->DumpRegPressure();
}

// print dep link information
void AArch64DepAnalysis::dumpDepLink(DepLink *link, DepNode *node) {
  CG_ASSERT(link, "");
  PRINT_VAL(GetDepTypeName(link->GetDepType()));
  PRINT_STR_VAL("Latency: ", link->GetLatency());
  if (node) {
    node->GetInsn()->dump();
  } else {
    LogInfo::MapleLogger() << "from : ";
    link->GetFrom()->GetInsn()->dump();
    LogInfo::MapleLogger() << "to : ";
    link->GetTo()->GetInsn()->dump();
  }
}

/* Append use register to the list. */
void AArch64DepAnalysis::AppendRegUseList(Insn *insn, regno_t regno) {
  RegList *regList = mp->New<RegList>();
  regList->insn = insn;
  regList->next = nullptr;
  if (!regUses[regno]) {
    regUses[regno] = regList;
  } else {
    RegList *lastRegList = regUses[regno];
    while (lastRegList->next) {
      lastRegList = lastRegList->next;
    }
    lastRegList->next = regList;
  }
}

/* Add dependence edge.
   Two dependence node has a unique edge.
   True dependence overwirtes other dependences.
 */
void AArch64DepAnalysis::AddDependence(DepNode *fromNode, DepNode *toNode, DepType tpy) {
  // Can not build a self loop dependence.
  if (fromNode == toNode)
    return;

  // Check if exist edge.
  if (!fromNode->GetSuccs().empty()) {
    DepLink *depLink = fromNode->GetSuccs().back();
    if (depLink->GetTo() == toNode) {
      if (depLink->GetDepType() != kDependenceTypeTrue) {
        if (tpy == kDependenceTypeTrue) {
          // Has exist edge, replace it.
          depLink->SetDepType(kDependenceTypeTrue);
          depLink->SetLatency(mad->GetLatency(fromNode->GetInsn(), toNode->GetInsn()));
          return;
        }
      }
      return;
    }
  }

  DepLink *depLink = mp->New<DepLink>(fromNode, toNode, tpy);
  if (tpy == kDependenceTypeTrue) {
    depLink->SetLatency(mad->GetLatency(fromNode->GetInsn(), toNode->GetInsn()));
  }
  fromNode->AddSucc(depLink);
  toNode->AddPred(depLink);
}

// Remove self dependence (self loop) in dependence graph.
void AArch64DepAnalysis::RemoveSelfDeps(Insn *insn) {
  DepNode *node = insn->depNode;
  CG_ASSERT(node->GetSuccs().back()->GetTo()->GetInsn() == insn, "Is not a self dependence.");
  CG_ASSERT(node->GetPreds().back()->GetFrom()->GetInsn() == insn, "Is not a self dependence.");
  node->RemoveSucc();
  node->RemovePred();
}

/* Build dependences of source register operand.
 */
void AArch64DepAnalysis::BuildDepsUseReg(Insn *insn, regno_t regno) {
  if (!beforeRA) {
    regno = cgfunc->AdjustRegno(regno);
  }
  useRegnos.push_back(regno);
  if (regDefs[regno]) {
    // Build true dependences.
    AddDependence(regDefs[regno]->depNode, insn->depNode, kDependenceTypeTrue);
  }
}

/* Build dependences of destination register operand.
 */
void AArch64DepAnalysis::BuildDepsDefReg(Insn *insn, regno_t regno) {
  if (!beforeRA) {
    regno = cgfunc->AdjustRegno(regno);
  }
  defRegnos.push_back(regno);
  // Build anti dependences.
  RegList *regList = regUses[regno];
  while (regList) {
    AddDependence(regList->insn->depNode, insn->depNode, kDependenceTypeAnti);
    regList = regList->next;
  }
  // Build output depnedence.
  if (regDefs[regno]) {
    AddDependence(regDefs[regno]->depNode, insn->depNode, kDependenceTypeOutput);
  }
}

/* Combine adrpldr&clinit_tail to clinit.
 */
void AArch64DepAnalysis::CombineClinit(DepNode *firstNode, DepNode *secondNode, bool isAcrossSeparator) {
  CG_ASSERT(firstNode->GetInsn()->GetMachineOpcode() == MOP_adrp_ldr, "first insn should be adrpldr");
  CG_ASSERT(secondNode->GetInsn()->GetMachineOpcode() == MOP_clinit_tail, "second insn should be clinit_tail");
  CG_ASSERT(firstNode->GetCfiInsns().empty(), "There should not be any comment/cfi instructions between clinit.");
  CG_ASSERT(secondNode->GetComments().empty(), "There should not be any comment/cfi instructions between clinit.");

  Insn *newInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(MOP_clinit,
      firstNode->GetInsn()->opnds[0], firstNode->GetInsn()->opnds[1]);
  newInsn->id = firstNode->GetInsn()->id;

  // Replace first node with new insn.
  firstNode->AddClinitInsn(firstNode->GetInsn());
  firstNode->AddClinitInsn(secondNode->GetInsn());
  firstNode->SetInsn(newInsn);
  Reservation *rev = mad->FindReservation(newInsn);
  CG_ASSERT(rev, "reservation is nullptr.");
  firstNode->SetReservation(rev);
  firstNode->SetUnits(firstNode->GetReservation()->GetUnit());
  firstNode->SetUnitNum(firstNode->GetReservation()->GetUnitNum());
  newInsn->depNode = firstNode;
  firstNode->SetCfiInsns(secondNode->GetCfiInsns());

  // Clear second node information.
  newInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(MOP_pseudo_none);
  newInsn->depNode = secondNode;
  secondNode->SetInsn(newInsn);
  secondNode->SetType(kNodeTypeEmpty);
  secondNode->SetReservation(mad->FindReservation(newInsn));
  secondNode->SetUnitNum(0);
  secondNode->ClearCfiInsns();
  secondNode->SetUnits(nullptr);

  CombineDependence(firstNode, secondNode, isAcrossSeparator);
}

/* Combine two dependence nodes to one*/
void AArch64DepAnalysis::CombineDependence(DepNode *firstNode, DepNode *secondNode, bool isAcrossSeparator) {
  if (isAcrossSeparator) {
    // Clear all latency of the second node.
    for (auto predLink : secondNode->GetPreds()) {
      predLink->SetLatency(0);
    }
    for (auto succLink : secondNode->GetSuccs()) {
      succLink->SetLatency(0);
    }
  } else {
    std::set<DepNode*> uniqueNodes;

    for (auto predLink : firstNode->GetPreds()) {
      if (predLink->GetDepType() == kDependenceTypeTrue) {
        predLink->SetLatency(mad->GetLatency(predLink->GetFrom()->GetInsn(), firstNode->GetInsn()));
      }
      uniqueNodes.insert(predLink->GetFrom());
    }

    for (auto predLink : secondNode->GetPreds()) {
      if (predLink->GetFrom() != firstNode) {
        if (uniqueNodes.insert(predLink->GetFrom()).second) {
          AddDependence(predLink->GetFrom(), firstNode, predLink->GetDepType());
        }
      }
      predLink->SetLatency(0);
    }

    uniqueNodes.clear();

    for (auto succLink : firstNode->GetSuccs()) {
      if (succLink->GetDepType() == kDependenceTypeTrue) {
        succLink->SetLatency(mad->GetLatency(succLink->GetFrom()->GetInsn(), firstNode->GetInsn()));
      }
      uniqueNodes.insert(succLink->GetTo());
    }

    for (auto succLink : secondNode->GetSuccs()) {
      if (uniqueNodes.insert(succLink->GetTo()).second) {
        AddDependence(firstNode, succLink->GetTo(), succLink->GetDepType());
      }
      succLink->SetLatency(0);
    }
  }
}

/* Build dependences of ambiguous instruction.
   ambiguous instruction : instructions that can not across may throw instructions.
 */
void AArch64DepAnalysis::BuildDepsAmbiInsn(Insn *insn) {
  for (auto throwInsn : mayThrows) {
    AddDependence(throwInsn->depNode, insn->depNode, kDependenceTypeThrow);
  }
  ambiInsns.push_back(insn);
}

/* Build dependences of may throw instructions.
 */
void AArch64DepAnalysis::BuildDepsMayThrowInsn(Insn *insn) {
  for (auto ambiInsn : ambiInsns) {
    AddDependence(ambiInsn->depNode, insn->depNode, kDependenceTypeThrow);
  }
}

bool AArch64DepAnalysis::IsFrameReg(const RegOperand *opnd) const {
  return opnd->GetRegisterNumber() == RFP || opnd->GetRegisterNumber() == RSP;
}

void AArch64DepAnalysis::AllocateRegPressure(DepNode *node) {
  RegPressure *regPressure = mp->New<RegPressure>(mp);
  node->SetRegPressure(regPressure);
  if (regPressure) {
    int *p = mp->NewArray<int>(RegPressure::GetMaxRegClassNum());
    node->SetPressure(p);
  }
}

/* Get the second memory access operand of stp/ldp instructions.
 */
AArch64MemOperand * AArch64DepAnalysis::GetNextMemOperand(Insn *insn, AArch64MemOperand * aarchMemOpnd) {
  AArch64MemOperand * nextMemOpnd = nullptr;
  switch (insn->GetMachineOpcode()) {
    case MOP_wldp:
    case MOP_sldp:
    case MOP_xldpsw:
    case MOP_wstp:
    case MOP_sstp: {
      Operand *nextOpnd = aarchMemOpnd->Clone(mp);
      nextMemOpnd = static_cast<AArch64MemOperand *>(nextOpnd);
      Operand *nextOfstOpnd = nextMemOpnd->GetOffsetImmediate()->Clone(mp);
      AArch64OfstOperand *aarchNextOfstOpnd = static_cast<AArch64OfstOperand *>(nextOfstOpnd);

      CG_ASSERT(aarchNextOfstOpnd, "ofstOpnd should not be nullptr.");

      int32 offsetVal = aarchNextOfstOpnd->GetOffsetValue();
      aarchNextOfstOpnd->SetOffsetValue(offsetVal+4);
      nextMemOpnd->SetOffsetImmediate(aarchNextOfstOpnd);
      break;
    }
    case MOP_xldp:
    case MOP_dldp:
    case MOP_xstp:
    case MOP_dstp: {
      Operand *nextOpnd = aarchMemOpnd->Clone(mp);
      nextMemOpnd = static_cast<AArch64MemOperand *>(nextOpnd);
      Operand *nextOfstOpnd = nextMemOpnd->GetOffsetImmediate()->Clone(mp);
      AArch64OfstOperand *aarchNextOfstOpnd = static_cast<AArch64OfstOperand *>(nextOfstOpnd);

      CG_ASSERT(aarchNextOfstOpnd, "ofstOpnd should not be nullptr.");

      int32 offsetVal = aarchNextOfstOpnd->GetOffsetValue();
      aarchNextOfstOpnd->SetOffsetValue(offsetVal+8);
      nextMemOpnd->SetOffsetImmediate(aarchNextOfstOpnd);
      break;
    }
    default:
      break;
  }

  return nextMemOpnd;
}

/* Build dependences of symbol memory access.
   Memory access with symbol must be a heap memory access.
 */
void AArch64DepAnalysis::BuildDepsAccessStImmMem(Insn *insn, bool isDest) {
  if (isDest) {
    // Heap memory
    // Build anti dependences.
    for (auto useInsn : heapUses) {
      AddDependence(useInsn->depNode, insn->depNode, kDependenceTypeAnti);
    }
    // Build output depnedence.
    for (auto defInsn : heapDefs) {
      AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeOutput);
    }
    heapDefs.push_back(insn);
  } else {
    // Heap memory
    for (auto defInsn : heapDefs) {
      AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeTrue);
    }

    heapUses.push_back(insn);
  }

  if (memBarInsn) {
    AddDependence(memBarInsn->depNode, insn->depNode, kDependenceTypeMembar);
  }

}

/* Build dependences of stack memory and heap memory uses.
 */
void AArch64DepAnalysis::BuildDepsUseMem(Insn *insn, MemOperand * memOpnd) {
  RegOperand * baseRegister = memOpnd->GetBaseRegister();
  AArch64MemOperand * aarchMemOpnd = static_cast<AArch64MemOperand *>(memOpnd);
  AArch64MemOperand * nextMemOpnd = GetNextMemOperand(insn, aarchMemOpnd);

  if ((baseRegister && IsFrameReg(baseRegister)) || aarchMemOpnd->IsStackMem()) {
    // Stack memory address
    for (auto defInsn : stackDefs) {
      if (defInsn->IsCall()) {
        AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeTrue);
        continue;
      }
      Operand *defOpnd = defInsn->GetMemOpnd();
      AArch64MemOperand *defMemOpnd = static_cast<AArch64MemOperand *>(defOpnd);
      if (!aarchMemOpnd->NoAlias(defMemOpnd)) {
        AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeTrue);
        continue;
      } else if (nextMemOpnd) {
        if (!nextMemOpnd->NoAlias(defMemOpnd)) {
          AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeTrue);
          continue;
        }
      }

      AArch64MemOperand *defNextMemOpnd = GetNextMemOperand(defInsn, defMemOpnd);
      if (defNextMemOpnd) {
        if (!aarchMemOpnd->NoAlias(defNextMemOpnd)) {
          AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeTrue);
          continue;
        } else if (nextMemOpnd) {
          if (!nextMemOpnd->NoAlias(defNextMemOpnd)) {
            AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeTrue);
            continue;
          }
        }
      }
    }

    stackUses.push_back(insn);
  } else {
    // Heap memory
    for (auto defInsn : heapDefs) {
      AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeTrue);
    }

    heapUses.push_back(insn);
  }

  if (memBarInsn) {
    AddDependence(memBarInsn->depNode, insn->depNode, kDependenceTypeMembar);
  }
}

/* Build dependences of stack memory and heap memory definitions.
 */
void AArch64DepAnalysis::BuildDepsDefMem(Insn *insn, MemOperand * memOpnd) {
  RegOperand * baseRegister = memOpnd->GetBaseRegister();
  AArch64MemOperand * aarchMemOpnd = static_cast<AArch64MemOperand *>(memOpnd);
  AArch64MemOperand * nextMemOpnd = GetNextMemOperand(insn, aarchMemOpnd);

  if ((baseRegister && IsFrameReg(baseRegister)) || aarchMemOpnd->IsStackMem()) {
    // Stack memory address
    // Build anti dependences.
    for (auto useInsn : stackUses) {
      Operand *useOpnd = useInsn->GetMemOpnd();
      AArch64MemOperand *useMemOpnd = static_cast<AArch64MemOperand *>(useOpnd);
      if (!aarchMemOpnd->NoAlias(useMemOpnd)) {
        AddDependence(useInsn->depNode, insn->depNode, kDependenceTypeAnti);
        continue;
      } else if (nextMemOpnd) {
        if (!nextMemOpnd->NoAlias(useMemOpnd)) {
          AddDependence(useInsn->depNode, insn->depNode, kDependenceTypeAnti);
          continue;
        }
      }

      AArch64MemOperand *useNextMemOpnd = GetNextMemOperand(useInsn, useMemOpnd);
      if (useNextMemOpnd) {
        if (!aarchMemOpnd->NoAlias(useNextMemOpnd)) {
          AddDependence(useInsn->depNode, insn->depNode, kDependenceTypeAnti);
          continue;
        } else if (nextMemOpnd) {
          if (!nextMemOpnd->NoAlias(useNextMemOpnd)) {
            AddDependence(useInsn->depNode, insn->depNode, kDependenceTypeAnti);
            continue;
          }
        }
      }
    }

    // Build output depnedence.
    for (auto defInsn : stackDefs) {
      if (defInsn->IsCall()) {
        AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeOutput);
        continue;
      }
      Operand *defOpnd = defInsn->GetMemOpnd();
      AArch64MemOperand *defMemOpnd = static_cast<AArch64MemOperand *>(defOpnd);
      if (!aarchMemOpnd->NoAlias(defMemOpnd)) {
        AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeOutput);
        continue;
      } else if (nextMemOpnd) {
        if (!nextMemOpnd->NoAlias(defMemOpnd)) {
          AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeOutput);
          continue;
        }
      }

      AArch64MemOperand *defNextMemOpnd = GetNextMemOperand(defInsn, defMemOpnd);
      if (defNextMemOpnd) {
        if (!aarchMemOpnd->NoAlias(defNextMemOpnd)) {
          AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeOutput);
          continue;
        } else if (nextMemOpnd) {
          if (!nextMemOpnd->NoAlias(defNextMemOpnd)) {
            AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeOutput);
            continue;
          }
        }
      }
    }

    if (lastCallInsn) {
      // Build a dependence between stack passed arguments and call.
      if (baseRegister->GetRegisterNumber() == RSP)
        AddDependence(lastCallInsn->depNode, insn->depNode, kDependenceTypeControl);
    }
    stackDefs.push_back(insn);
  } else {
    // Heap memory
    // Build anti dependences.
    for (auto useInsn : heapUses) {
      AddDependence(useInsn->depNode, insn->depNode, kDependenceTypeAnti);
    }
    // Build output depnedence.
    for (auto defInsn : heapDefs) {
      AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeOutput);
    }

    heapDefs.push_back(insn);
  }

  if (memBarInsn) {
    AddDependence(memBarInsn->depNode, insn->depNode, kDependenceTypeMembar);
  }

  // Memory definition can not across may-throw insns.
  for (auto mayThrowInsn : mayThrows) {
    AddDependence(mayThrowInsn->depNode, insn->depNode, kDependenceTypeThrow);
  }
}

/* Build dependences of memory barrior instructions.
 */
void AArch64DepAnalysis::BuildDepsMemBar(Insn *insn) {
  for (auto i : stackUses) {
    if (insn != i)
      AddDependence(i->depNode, insn->depNode, kDependenceTypeMembar);
  }
  for (auto i : heapUses) {
    if (insn != i)
      AddDependence(i->depNode, insn->depNode, kDependenceTypeMembar);
  }
  for (auto i : stackDefs) {
    if (insn != i)
      AddDependence(i->depNode, insn->depNode, kDependenceTypeMembar);
  }
  for (auto i : heapDefs) {
    if (insn != i)
      AddDependence(i->depNode, insn->depNode, kDependenceTypeMembar);
  }
  memBarInsn = insn;
}

/* A pseudo separator node depends all the other nodes. */
void AArch64DepAnalysis::BuildDepsSeparator(DepNode *newSepNode, vector<DepNode*> &nodes) {
  uint32 nextSepIndex = (separatorIndex + kMaxDependenceNum) < nodes.size()
                        ? (separatorIndex + kMaxDependenceNum)
                        : nodes.size() - 1;
  for (int i = separatorIndex; i < nextSepIndex; i++) {
    AddDependence(nodes[i], newSepNode, kDependenceTypeSeparator);
  }
}

/* Build control dependence for branch/ret instructions.
 */
void AArch64DepAnalysis::BuildDepsControlAll(DepNode *depNode, vector<DepNode *>& nodes) {
  for (int i = separatorIndex; i < depNode->GetIndex(); i++) {
    AddDependence(nodes[i], depNode, kDependenceTypeControl);
  }
}

/* Build dependences of call instructions.
   Caller-saved physical registers will defined by a call instruction.
   Also a conditional register may modified by a call.
 */
void AArch64DepAnalysis::BuildCallerSavedDeps(Insn *insn) {
  // Build anti dependence and output dependence.
  for (int i = R0; i <= R7; i++) {
    BuildDepsDefReg(insn, i);
  }

  for (int i = V0; i <= V7; i++) {
    BuildDepsDefReg(insn, i);
  }

  if (beforeRA) {
    BuildDepsDefReg(insn, RSP);
  } else {
    for (int i = R8; i <= R18; i++) {
      BuildDepsDefReg(insn, i);
    }
    for (int i = R29; i <= RSP; i++) {
      BuildDepsUseReg(insn, i);
    }
    for (int i = V16; i <= V31; i++) {
      BuildDepsDefReg(insn, i);
    }
  }

  // For condition operand, such as NE, EQ, and so on.
  Operand *rflagOpnd = cgfunc->GetRflag();
  if (rflagOpnd) {
    BuildDepsDefReg(insn, RFLAG);
  }
}

/* Build dependence between control register and last call instruction.
   insn : instruction that with control register operand.
   isDest : if the control register operand is a destination operand.
 */
void AArch64DepAnalysis::BuildDepsBetweenControlRegAndCall(Insn *insn, bool isDest) {
  if (lastCallInsn) {
    if (isDest) {
      AddDependence(lastCallInsn->depNode, insn->depNode, kDependenceTypeOutput);
    } else {
      AddDependence(lastCallInsn->depNode, insn->depNode, kDependenceTypeAnti);
    }
  }
}

struct OperandCmp{
  bool operator() (Operand * lhs, Operand * rhs) const {
    return (lhs->Less(rhs));
  }
};

void AArch64DepAnalysis::BuildStackPassArgsDeps(Insn * insn) {
  for (auto stackDefInsn : stackDefs) {
    Operand *opnd = stackDefInsn->GetMemOpnd();
    CG_ASSERT(opnd->IsMemoryAccessOperand(), "");

    MemOperand *memOpnd = static_cast<MemOperand *>(opnd);
    RegOperand *baseReg = memOpnd->GetBaseRegister();
    if (baseReg && (baseReg->GetRegisterNumber() == RSP))
      AddDependence(stackDefInsn->depNode, insn->depNode, kDependenceTypeControl);
  }
}

// Some insns may dirty all stack memory, such as "bl MCC_InitializeLocalStackRef".
void AArch64DepAnalysis::BuildDepsDirtyStack(Insn * insn) {
  // Build anti dependences.
  for (auto useInsn : stackUses) {
    AddDependence(useInsn->depNode, insn->depNode, kDependenceTypeAnti);
  }
  // Build output depnedence.
  for (auto defInsn : stackDefs) {
    AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeOutput);
  }

  stackDefs.push_back(insn);
}

// Some call insns may use all stack memory, such as "bl MCC_CleanupLocalStackRef_NaiveRCFast".
void AArch64DepAnalysis::BuildDepsUseStack(Insn * insn) {
  // Build true dependences.
  for (auto defInsn : stackDefs) {
    AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeTrue);
  }
}

// Some insns may dirty all heap memory, such as a call insn.
void AArch64DepAnalysis::BuildDepsDirtyHeap(Insn * insn) {
  // Build anti dependences.
  for (auto useInsn : heapUses) {
    AddDependence(useInsn->depNode, insn->depNode, kDependenceTypeAnti);
  }
  // Build output depnedence.
  for (auto defInsn : heapDefs) {
    AddDependence(defInsn->depNode, insn->depNode, kDependenceTypeOutput);
  }

  heapDefs.push_back(insn);
}

// Build a pseudo node to seperate dependence graph.
DepNode *AArch64DepAnalysis::BuildSeparatorNode() {
  Insn *pseudoSepInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(MOP_pseudo_dependence_seperator);
  DepNode *separatorNode= mp->New<DepNode>(pseudoSepInsn);
  separatorNode->SetType(kNodeTypeSeparator);
  return separatorNode;
}

void AArch64DepAnalysis::Init(BB *bb, vector<DepNode*> &nodes) {
  uint32 maxRegNum;
  if (beforeRA) {
    maxRegNum = cgfunc->GetMaxVReg();
  } else {
    maxRegNum = kMaxRegNum;
  }

  memset_s(regDefs, sizeof(Insn *) * maxRegNum, 0, sizeof(Insn *) * maxRegNum);
  memset_s(regUses, sizeof(RegList *) * maxRegNum, 0, sizeof(RegList *) * maxRegNum);
  memBarInsn = nullptr;
  lastCallInsn = nullptr;

  stackUses.clear();
  stackDefs.clear();
  heapUses.clear();
  heapDefs.clear();
  mayThrows.clear();
  ambiInsns.clear();
  lastComments.clear();

  // Analysis live-in registers in catch BB.
  AnalysisAmbiInsns(bb);

  // Clear all dependence nodes and push the first separator node.
  nodes.clear();
  DepNode *separatorNode = BuildSeparatorNode();
  AllocateRegPressure(separatorNode);
  nodes.push_back(separatorNode);

  separatorIndex = 0;
}

/* When a separator build, it is the same as a new basic block. */
void AArch64DepAnalysis::ClearAllDepData() {
  uint32 maxRegNum;
  if (beforeRA) {
    maxRegNum = cgfunc->GetMaxVReg();
  } else {
    maxRegNum = kMaxRegNum;
  }

  memset_s(regDefs, sizeof(Insn *) * maxRegNum, 0, sizeof(Insn *) * maxRegNum);
  memset_s(regUses, sizeof(RegList *) * maxRegNum, 0, sizeof(RegList *) * maxRegNum);
  memBarInsn = nullptr;
  lastCallInsn = nullptr;

  stackUses.clear();
  stackDefs.clear();
  heapUses.clear();
  heapDefs.clear();
  mayThrows.clear();
  ambiInsns.clear();
}

/* Analysis live-in registers in catch bb and cleanup bb.*/
void AArch64DepAnalysis::AnalysisAmbiInsns(BB *bb) {
  hasAmbiRegs = false;

  if (bb->eh_succs.empty()) {
    return;
  }

  // Union all catch bb
  set<regno_t> ordered;
  for (auto succBb : bb->eh_succs) {
    ordered.insert(succBb->livein_regno.begin(), succBb->livein_regno.end());
    set_union(ordered.begin(), ordered.end(),
              ehInRegs.begin(), ehInRegs.end(),
              inserter(ehInRegs, ehInRegs.begin()));
    ordered.clear();
  }

  // Union cleanup entry bb.
  ordered.insert(cgfunc->cleanupEntrybb->livein_regno.begin(), cgfunc->cleanupEntrybb->livein_regno.end());
  set_union(ordered.begin(), ordered.end(),
            ehInRegs.begin(), ehInRegs.end(),
            inserter(ehInRegs, ehInRegs.begin()));

  // Subtract R0 and R1, that is defined by eh runtime.
  ehInRegs.erase(R0);
  ehInRegs.erase(R1);

  if (ehInRegs.empty()) {
    return;
  }

  hasAmbiRegs = true;
  return;
}

/* Check if regno is in ehInRegs. */
bool AArch64DepAnalysis::IfInAmbiRegs(regno_t regno) const {
  if (!hasAmbiRegs)
    return false;

  if (ehInRegs.find(regno) != ehInRegs.end()) {
    return true;
  }

  return false;
}

/* Build dependence graph.
   1: Build dependence nodes.
   2: Build edges between dependence nodes. Edges are:
     2.1) True dependences
     2.2) Anti dependences
     2.3) Output dependences
     2.4) Barrier dependences
 */
void AArch64DepAnalysis::Run(BB *bb, vector<DepNode *>& nodes) {
  // Initial internal datas.
  Init(bb, nodes);

  uint32 nodeSum = 1;
  std::vector<Insn *>  comments;
  // Loads that cannot move across calls, unless alias info tells otherwise.
  std::vector<Insn *>  loads;

  FOR_BB_INSNS(insn, bb) {
    if( !insn->IsMachineInstruction()) {
      if (insn->IsComment()) {
        comments.push_back(insn);
      } else if (insn->IsCfiInsn()) {
        nodes.back()->AddCfiInsn(insn);
      }
      continue;
    }

    if (nodeSum > 0 && nodeSum % kMaxDependenceNum == 0) {
      CG_ASSERT(nodeSum == nodes.size(), "CG internal error, nodeSum should equal to nodes.size.");
      // Add a pseudo node to seperate dependence graph.
      DepNode *separatorNode = BuildSeparatorNode();
      AllocateRegPressure(separatorNode);
      separatorNode->SetIndex(nodeSum);
      nodes.push_back(separatorNode);
      BuildDepsSeparator(separatorNode, nodes);
      ClearAllDepData();
      separatorIndex = nodeSum++;
      // Only preventing loads crossing call from the same separattor group
      loads.clear();
    }

    DepNode * depNode = nullptr;
    Reservation *rev = mad->FindReservation(insn);
    CG_ASSERT(rev, "rev is nullptr");
    depNode= mp->New<DepNode>(insn, rev->GetUnit(), rev->GetUnitNum(), rev);
    AllocateRegPressure(depNode);
    /*
    RegPressure *regPressure = mp->New<RegPressure>(mp);
    depNode->SetRegPressure(regPressure);
    if (regPressure) {
      int *p = mp->NewArray<int>(RegPressure::GetMaxRegClassNum());
      depNode->SetPressure(p);
      //RegPressure *p = mp->New<RegPressure>();
      //depNode->SetRegPressure(p);
    }
    */
    depNode->SetIndex(nodeSum);
    nodes.push_back(depNode);
    nodeSum++;
    insn->depNode = depNode;

    if (!comments.empty()) {
      depNode->SetComments(comments);
      comments.clear();
    }

    bool isMayThrowInsn = insn->MayThrow();
    if (isMayThrowInsn) {
      BuildDepsMayThrowInsn(insn);
    }
    const AArch64MD* md = &AArch64CG::kMd[static_cast<AArch64Insn*>(insn)->mop_];
    MOperator mop = insn->GetMachineOpcode();

    useRegnos.clear();
    defRegnos.clear();

    for (int i = 0; i < Insn::kMaxOperandNum; i++) {
      Operand* opnd = insn->opnds[i];
      if (opnd == nullptr)
        continue;

      AArch64OpndProp* regprop = static_cast<AArch64OpndProp*>(md->operand_[i]);

      if (opnd->IsMemoryAccessOperand()) {
        AArch64MemOperand *memopnd = static_cast<AArch64MemOperand *>(opnd);
        RegOperand * baseRegister = memopnd->GetBaseRegister();
        if (baseRegister) {
          regno_t regno = baseRegister->GetRegisterNumber();
          BuildDepsUseReg(insn, regno);

          if (memopnd->GetAddrMode() == AArch64MemOperand::kAddrModeBOi &&
            (memopnd->IsPostIndexed() || memopnd->IsPreIndexed())) {
            // Base operand has changed.
            BuildDepsDefReg(insn, regno);
          }
        }
        RegOperand * indexRegister = memopnd->GetIndexRegister();
        if (indexRegister) {
          regno_t regno = indexRegister->GetRegisterNumber();
          BuildDepsUseReg(insn, regno);
        }

        if (regprop->IsUse()) {
          BuildDepsUseMem(insn, memopnd);
        } else {
          BuildDepsDefMem(insn, memopnd);
          BuildDepsAmbiInsn(insn);
        }

        if (insn->IsMemAccessBar()) {
          BuildDepsMemBar(insn);
        }
      } else if (opnd->IsStImmediate()) {
        if (mop != MOP_xadrpl12) {
          BuildDepsAccessStImmMem(insn, false);
        }
      } else if (opnd->IsRegister()) {
        RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
        regno_t regno = regOpnd->GetRegisterNumber();

        if (regprop->IsUse()) {
          BuildDepsUseReg(insn, regno);
        }

        if (regprop->IsDef()) {
          BuildDepsDefReg(insn, regno);
        }
      } else if (opnd->IsConditionCode()) {
        // For condition operand, such as NE, EQ, and so on.
        if (regprop->IsUse()) {
          BuildDepsUseReg(insn, RFLAG);
          BuildDepsBetweenControlRegAndCall(insn, false);
        }

        if (regprop->IsDef()) {
          BuildDepsDefReg(insn, RFLAG);
          BuildDepsBetweenControlRegAndCall(insn, true);
        }
      } else if (opnd->IsList()) {
        ListOperand *listOpnd = static_cast<ListOperand *>(opnd);
        // Build true dependences
        for (auto lst : listOpnd->GetOperands()) {
          regno_t regno = lst->GetRegisterNumber();
          BuildDepsUseReg(insn, regno);
        }
      }
    }

    if (insn->IsCall() || insn->IsTailCall()) {
      // Caller saved registers.
      BuildCallerSavedDeps(insn);
      BuildStackPassArgsDeps(insn);

      int dirtyHeap = true;
      if (mop == MOP_xbl) {
        FuncNameOperand *target = static_cast<FuncNameOperand *>(insn->opnds[0]);
        if (target->GetName() == GetIntrinsicFuncName(INTRN_MCCInitializeLocalStackRef)) {
          // Write stack memory.
          BuildDepsDirtyStack(insn);
          dirtyHeap = false;
        } else if (IsRtCleanupLocalStackCall(target->GetName())) {
          // UseStackMemory.
          BuildDepsUseStack(insn);
        }
      }

      if (dirtyHeap) {
        BuildDepsDirtyHeap(insn);
      }

      if (lastCallInsn) {
        AddDependence(lastCallInsn->depNode, insn->depNode, kDependenceTypeControl);
      }
      for (auto ldInsn: loads) {
        AddDependence(ldInsn->depNode, insn->depNode, kDependenceTypeAnti);
      }
      loads.clear();
      lastCallInsn = insn;
    } else if (mop == MOP_clinit_tail || mop == MOP_clinit) {
      BuildDepsDirtyHeap(insn);
      BuildDepsDefReg(insn, RFLAG);
    } else if (mop == MOP_xret || md->IsBranch()) {
      BuildDepsControlAll(depNode, nodes);
    }

    for (auto regno : defRegnos) {
      if (IfInAmbiRegs(regno)) {
        BuildDepsAmbiInsn(insn);
        break;
      }
    }

    if (isMayThrowInsn) {
      mayThrows.push_back(insn);
      if (insn->IsLoad()) {
        loads.push_back(insn);
      }

      for (auto stackDefInsn : stackDefs) {
        AddDependence(stackDefInsn->depNode, insn->depNode, kDependenceTypeThrow);
      }

      for (auto heapDefInsn : heapDefs) {
        AddDependence(heapDefInsn->depNode, insn->depNode, kDependenceTypeThrow);
      }
    }

    // Seperator exists.
    AddDependence(nodes[separatorIndex], insn->depNode, kDependenceTypeSeparator);

    for (auto regno : useRegnos) {
      AppendRegUseList(insn, regno);
      depNode->AddUseReg(regno);
      depNode->SetRegUses(regno, regUses[regno]);
    }
    for (auto regno : defRegnos) {
      regDefs[regno] = insn;
      regUses[regno] = nullptr;
      depNode->AddDefReg(regno);
    }
  }

  DepNode * separatorNode = BuildSeparatorNode();
  AllocateRegPressure(separatorNode);
  nodes.push_back(separatorNode);
  BuildDepsSeparator(separatorNode, nodes);


  if (!comments.empty()) {
    lastComments = comments;
  }

  comments.clear();

}

// return dependence type name
std::string AArch64DepAnalysis::GetDepTypeName(DepType depType) {
  CG_ASSERT(depType <= kDependenceTypeNone, "");
  switch (depType) {
    case kDependenceTypeTrue:
      return "true-dep";
    case kDependenceTypeOutput:
      return "output-dep";
    case kDependenceTypeAnti:
      return "anti-dep";
    case kDependenceTypeControl:
      return "control-dep";
    case kDependenceTypeMembar:
      return "membar-dep";
    case kDependenceTypeThrow:
      return "throw-dep";
    case kDependenceTypeNone:
      return "none-dep";
    default:
      return "none";
  }
}

}  // namespace maplebe


