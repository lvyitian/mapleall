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

#ifndef AARCH64SCHEDULE_H
#define AARCH64SCHEDULE_H

#include "schedule.h"
#include "aarch64_operand.h"

namespace maplebe {

enum RegisterType {
  kRegisterUndef,
  kRegisterInt,
  kRegisterFloat,
  kRegisterCc,
  kRegisterLast,
};


class AArch64Schedule : public Schedule {
 public:
  AArch64Schedule(CGFunc *func, MemPool *mp, LiveAnalysis *live, const char *phaseName)
      : Schedule(func, mp, live, phaseName) {}
  ~AArch64Schedule() {}
 protected:
  void DumpDepGraph(const std::vector<DepNode*> &nodes) const;
  void GenerateDot(const BB *bb, const std::vector<DepNode*> &nodes) const;

 private:
  void Init() override;
  void MemoryAccessPairOpt(BB *bb) override;
  void ClinitPairOpt() override;
  void RegPressureScheduling(const BB *bb, std::vector<DepNode*> &nd) override;
  void DoSchedule() override;
  void ListScheduling(bool beforeRA) override;
  void FinalizeScheduling(BB *bb, const DepAnalysis *depAnalysis) override;
  uint32 ComputeEstart(uint32 cycle) override;
  void ComputeLstart(uint32 maxEstart) override;
  inline void UpdateELStartsOnCycle(uint32 cycle) override;
  void RandomTest() override;
  void EraseNodeFromReadyList(const DepNode *target) override;
  inline uint32 GetNextSepIndex() const override;
};

}  // namespace maplebe

#endif  // AARCH64SCHEDULE_H
