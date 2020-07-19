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

#ifndef SCHEDULE_H
#define SCHEDULE_H

#include "insn.h"
#include "cg_func.h"
#include "mad.h"
#include "dep_analysis.h"

#ifdef MPLAD_CORTEX_A55
#include "cortex_a55/a55mad.h"
#endif  // MPLAD_CORTEX_A55

namespace maplebe {
#define LISTSCHEDDUMP CGDEBUGFUNC(cgfunc)

class RegPressureSchedule {
 public:
  RegPressureSchedule (CGFunc *cg, MemPool *mp)
      : cgfunc(cg), bb(nullptr), maxPressure(nullptr), curPressure(nullptr), liveReg(), scheduledNode(),
        readyList(), physicalRegNum(nullptr), maxpriority(0) {}
  virtual ~RegPressureSchedule() {}
  void InitBBInfo(const BB *b, MemPool *mp, const std::vector<DepNode*> &nodes);
  void BuildPhyRegInfo(const int *regNum, size_t regNumLength);
  void InitReadyList(std::vector<DepNode*> &nodes);
  void InitPressure();
  void UpdatePressure(regno_t reg, bool def);
  void UpdateBBPressure(DepNode *node);
  void CalculatePressure(DepNode *node, regno_t reg, bool def);
  void SortReadyList();
  bool IsLastUse(const DepNode *node, regno_t reg) const;
  void ReCalculateDepNodePressure(DepNode *node);
  void UpdateLiveReg(DepNode *node, regno_t reg, bool def);
  bool CanSchedule(const DepNode *node) const;
  void UpdateReadyList(DepNode *node);
  void UpdatePriority(DepNode *node);
  void CalculateMaxDepth(std::vector<DepNode*> &nodes);
  void CalculateNear(DepNode *node);
  static bool DepNodePriorityCmp(const DepNode *node1, const DepNode *node2);
  DepNode *ChooseNode();
  int RegPressureCost(const DepNode *node) const;
  bool CompareRegPressureCost(const DepNode *node1, const DepNode *node2) const;

  void DoScheduling(std::vector<DepNode*> &nodes);

 private:
  CGFunc *cgfunc;
  const BB *bb;
  int *maxPressure;
  int *curPressure;
  std::set<regno_t> liveReg;
  // save node that has been scheduled.
  std::vector<DepNode*> scheduledNode;
  std::vector<DepNode*> readyList;
  // save the amount of every type register.
  int *physicalRegNum;
  int maxpriority;
};


class Schedule {
 public:
  Schedule(CGFunc *func, MemPool *p, LiveAnalysis *l, const char *phase)
      : phaseName(phase),
        cgfunc(func),
        mp(p),
        alloc(p),
        live(l),
        depAnalysis(nullptr),
        mad(nullptr),
        lastSeparatorIndex(0),
        nodeSize(0),
        nodes(),
        readyList() {}

  virtual ~Schedule() {}

  virtual void MemoryAccessPairOpt(BB *bb) {}
  virtual void ClinitPairOpt() {}

  virtual void RegPressureScheduling(const BB *bb, std::vector<DepNode*> &nd) {}

  virtual void DoSchedule() {}

  virtual void ListScheduling(bool beforeRA) {}
  virtual void FinalizeScheduling(BB *bb, const DepAnalysis *depAnalysis) {}

 protected:
  const char *phaseName;
  CGFunc *cgfunc;
  MemPool *mp;
  MapleAllocator alloc;
  LiveAnalysis *live;
  DepAnalysis *depAnalysis;
  MAD *mad;
  uint32 lastSeparatorIndex;
  uint32 nodeSize;
  std::vector<DepNode*> nodes;      // Dependence graph
  std::vector<DepNode*> readyList;  // Ready list.

  virtual void Init() {}
  virtual void BuildDepsUseMem(Insn *insn, MemOperand *memOpnd) {}
  virtual uint32 ComputeEstart(uint32 cycle) {
    return 0;
  }
  virtual void ComputeLstart(uint32 maxEstart) {}
  virtual inline void UpdateELStartsOnCycle(uint32 cycle) {}
  virtual void RandomTest() {}
  virtual void EraseNodeFromReadyList(const DepNode *target) {}
  inline virtual uint32 GetNextSepIndex() const {
    return 0;
  }
  void MarkInsnId();
  const char *PhaseName() const {
    return phaseName;
  }
};

CGFUNCPHASE_CANSKIP(CgDoPreScheduling, "prescheduling")
CGFUNCPHASE_CANSKIP(CgDoScheduling, "scheduling")

}  // namespace maplebe
#endif  // SCHEDULE_H
