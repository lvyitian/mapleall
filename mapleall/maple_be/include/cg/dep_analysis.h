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


#ifndef DEPANALYSIS_H
#define DEPANALYSIS_H

#include "deps.h"
#include "cg_func.h"

const uint32 kMaxDependenceNum = 100;

namespace maplebe {

using namespace std;

#define PRINT_STR_VAL(STR, VAL) \
  LogInfo::MapleLogger() << left << setw(12) << STR << VAL << " | ";
#define PRINT_VAL(VAL) \
  LogInfo::MapleLogger() << left << setw(12) << VAL << " | ";

class DepAnalysis {
 public:
  DepAnalysis(CGFunc *func, MemPool *p, bool bra)
      : cgfunc(func), mp(p), alloc(p), beforeRA(bra), mad(nullptr), lastComments() {}

  virtual ~DepAnalysis() {}

  virtual void Run(BB *bb, std::vector<DepNode*> &nodes) {}

  const std::vector<Insn*> &GetLastComments() const {
    return lastComments;
  }
  virtual void CombineClinit(DepNode *firstNode, DepNode *secondNode, bool isAcrossSeparator) {}
  virtual void CombineDependence(DepNode *firstNode, DepNode *secondNode, bool isAcrossSeparator) {}

  virtual std::string GetDepTypeName(DepType depType) {
    return "none";
  };
  virtual void dumpDepNode(DepNode *node) {};
  virtual void dumpDepLink(DepLink *link, DepNode *node) {};

 protected:
  CGFunc *cgfunc;
  MemPool *mp;
  MapleAllocator alloc;
  bool beforeRA;
  MAD *mad;
  std::vector<Insn*> lastComments;

  virtual void Init(BB *bb, vector<DepNode*> &nodes) {}
  virtual void ClearAllDepData() {}
  virtual void AnalysisAmbiInsns(BB *bb) {}
  virtual void AppendRegUseList(Insn *insn, regno_t regno) {}
  inline virtual void AddDependence(DepNode *fromNode, DepNode *toNode, DepType tpy) {}
  inline virtual void RemoveSelfDeps(Insn *insn) {}
  inline virtual void BuildDepsUseReg(Insn *insn, regno_t regno) {}
  inline virtual void BuildDepsDefReg(Insn *insn, regno_t regno) {}
  inline virtual void BuildDepsAmbiInsn(Insn *insn) {}
  inline virtual void BuildDepsMayThrowInsn(Insn *insn) {}
  virtual void BuildDepsUseMem(Insn *insn, MemOperand *memOpnd) {}
  virtual void BuildDepsDefMem(Insn *insn, MemOperand *memOpnd) {}
  virtual void BuildDepsMemBar(Insn *insn) {}
  virtual void BuildDepsSeparator(DepNode *newSepNode, vector<DepNode*> &nodes) {}
  virtual void BuildDepsControlAll(DepNode *depNode, vector<DepNode*> &nodes) {}
  virtual void BuildDepsAccessStImmMem(Insn *insn, bool isDest) {}
  virtual void BuildCallerSavedDeps(Insn *insn) {}
  inline virtual void BuildDepsBetweenControlRegAndCall(Insn *insn, bool isDest) {}
  virtual void BuildStackPassArgsDeps(Insn *insn) {}
  virtual void BuildDepsDirtyStack(Insn *insn) {}
  virtual void BuildDepsUseStack(Insn *insn) {}
  virtual void BuildDepsDirtyHeap(Insn *insn) {}
  virtual DepNode *BuildSeparatorNode() {
    return nullptr;
  }
  virtual bool IfInAmbiRegs(regno_t regno) const {
    return false;
  }
  virtual inline bool IsFrameReg(const RegOperand *) const {
    return false;
  }
  virtual void AllocateRegPressure(DepNode *depNode) {}
};

}
#endif //DEPANALYSIS_H
