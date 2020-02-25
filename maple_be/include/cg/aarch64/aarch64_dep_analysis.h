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

#ifndef AARCH64DEPANALYSIS_H
#define AARCH64DEPANALYSIS_H

#include "dep_analysis.h"
#include "cg_func.h"

namespace maplebe {


class AArch64DepAnalysis : public DepAnalysis {
 public:
  AArch64DepAnalysis(CGFunc *func, MemPool *p, bool beforeRA);

  ~AArch64DepAnalysis() {}

  void Run(BB *bb, vector<DepNode*> &nodes) override;
  std::string GetDepTypeName(DepType depType) override;
  void dumpDepNode(DepNode *node) override;
  void dumpDepLink(DepLink *link, DepNode *node) override;

 protected:
  void Init(BB *bb, vector<DepNode*> &nodes) override;
  void ClearAllDepData() override;
  void AnalysisAmbiInsns(BB *bb) override;
  void AppendRegUseList(Insn *insn, regno_t regno) override;
  inline void AddDependence(DepNode* fromNode, DepNode *toNode, DepType tpy) override;
  inline void RemoveSelfDeps(Insn *insn) override;
  void CombineClinit(DepNode *firstNode, DepNode *secondNode, bool isAcrossSeparator) override;
  void CombineDependence(DepNode *firstNode, DepNode *secondNode, bool isAcrossSeparator) override;
  inline void BuildDepsUseReg(Insn *insn, regno_t regno) override;
  inline void BuildDepsDefReg(Insn *insn, regno_t regno) override;
  inline void BuildDepsAmbiInsn(Insn *insn) override;
  inline void BuildDepsMayThrowInsn(Insn *insn) override;
  void BuildDepsUseMem(Insn *insn, MemOperand * memOpnd) override;
  void BuildDepsDefMem(Insn *insn, MemOperand * memOpnd) override;
  void BuildDepsMemBar(Insn *insn) override;
  void BuildDepsSeparator(DepNode *newSepNode, vector<DepNode*> &nodes) override;
  void BuildDepsControlAll(DepNode *depNode, vector<DepNode*> &nodes) override;
  void BuildDepsAccessStImmMem(Insn *insn, bool isDest) override;
  void BuildCallerSavedDeps(Insn *insn) override;
  inline void BuildDepsBetweenControlRegAndCall(Insn *insn, bool isDest) override;
  void BuildStackPassArgsDeps(Insn *insn) override;
  void BuildDepsDirtyStack(Insn *insn) override;
  void BuildDepsUseStack(Insn *insn) override;
  void BuildDepsDirtyHeap(Insn *insn) override;
  DepNode *BuildSeparatorNode() override;
  bool IfInAmbiRegs(regno_t regno) const override;
  inline bool IsFrameReg(const RegOperand *) const override;
  void AllocateRegPressure(DepNode *depNode) override;

 private:
  Insn **regDefs;
  RegList **regUses;
  Insn *memBarInsn;
  bool hasAmbiRegs;
  Insn *lastCallInsn;
  uint32 separatorIndex;
  std::vector<regno_t> useRegnos;
  std::vector<regno_t> defRegnos;
  std::vector<Insn*> stackUses;
  std::vector<Insn*> stackDefs;
  std::vector<Insn*> heapUses;
  std::vector<Insn*> heapDefs;
  std::vector<Insn*> mayThrows;

  // instructions that can not across may throw instructions.
  std::vector<Insn*> ambiInsns;

  // register number that catch bb and cleanup bb uses.
  std::set<regno_t> ehInRegs;

  AArch64MemOperand *GetNextMemOperand(Insn *insn, AArch64MemOperand *aarchMemOpnd);
};

}

#endif //AARCH64DEPANALYSIS_H
