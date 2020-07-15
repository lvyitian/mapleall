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

#ifndef MAPLE_IPA_INCLUDE_FORMREGIONS_H
#define MAPLE_IPA_INCLUDE_FORMREGIONS_H
#include "module_phase.h"
#include "callgraph.h"
#include "wpo_option.h"

namespace maple {

class Region {
 public:
  static uint32 regionLimit;      // size in number of statements
  static uint32 regidNext;        // for assigning regid, initialized to 1; 0 is reserved
  static uint32 compileRegionId;  // compile only the region given by id for debug purpose.
                                  // -1U indicates all regions will be compiled
  static bool singleFuncMode;     // Region contains a single function in this mode
  uint32 regid;                   // id of the region starting from 1
  MapleVector<PUIdx> funcmembers;
  uint32 regstmtcount;

 public:
  Region(MapleAllocator *alloc) : regid(regidNext), funcmembers(alloc->Adapter()), regstmtcount(0) {
    regidNext++;
  }

  virtual ~Region() {}

  bool HasRoom(const CGNode *cgnode) const {
    return (regstmtcount + cgnode->GetStmtCount() < regionLimit);
  }
};

class FormRegions : public AnalysisResult {
 private:
  MapleAllocator alloc;
  CallGraph *callgraph;

 public:
  MapleVector<uint32> assigned_region;    // index is puIdx; 0 if unsigned
  MapleVector<bool> called_from_outside;  // index is puIdx; if called from outside its region
  MapleVector<Region *> theregions;

 public:
  FormRegions(MemPool *mp, CallGraph *cg)
    : AnalysisResult(mp),
      alloc(mp),
      callgraph(cg),
      assigned_region(GlobalTables::GetFunctionTable().funcTable.size(), 0, alloc.Adapter()),
      called_from_outside(GlobalTables::GetFunctionTable().funcTable.size(), false, alloc.Adapter()),
      theregions(alloc.Adapter()) {}

  ~FormRegions() {}

  void AddFuncToRegion(const CGNode *cgnode, Region *reg, const char *neighborkind);
  void AddNeighborsInCallGraph(CGNode *cgnode);
  bool FormOneRegion();
  void MergeAllRegionsToOne(uint32 totalFunctions);
  void DetermineCalledFromOutside();
  void FormSingleFunRegions();
};

class DoFormRegions : public ModulePhase {
 public:
  DoFormRegions(ModulePhaseID id) : ModulePhase(id) {}

  AnalysisResult *Run(MIRModule *module, ModuleResultMgr *m) override;
  std::string PhaseName() const override {
    return "form regions";
  }

  virtual ~DoFormRegions(){};
};
}  // namespace maple
#endif  // MAPLE_IPA_INCLUDE_FORMREGIONS_H
