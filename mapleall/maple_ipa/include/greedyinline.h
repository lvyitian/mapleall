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

#ifndef MAPLE_IPA_INCLUDE_GREEDYINLINE_H
#define MAPLE_IPA_INCLUDE_GREEDYINLINE_H
#include "inline.h"
#include "callgraph.h"
#include "module_phase.h"

namespace maple {

class CallSiteData {
 public:
  CallInfo *ci;
  CGNode *callee;
  CGNode *caller;
  double weight;
  CallSiteData &operator=(const CallSiteData &p) = default;
  CallSiteData(CallInfo *ci, CGNode *callee, CGNode *caller) : ci(ci), callee(callee), caller(caller), weight(0.0) {}

  CallSiteData(const CallSiteData &x) {
    ci = x.ci;
    callee = x.callee;
    caller = x.caller;
    weight = x.weight;
  }

  virtual ~CallSiteData() {}

  void Print() const {
    printf("# CallSiteData:\n");
    printf("Caller: %s\nCallee: %s\n", caller->GetMIRFuncName().c_str(), callee->GetMIRFuncName().c_str());
    printf("loop depth: %u\n", ci->GetLoopDepth());
    printf("weight: %f\n", weight);
    printf("----------------\n");
  }
};

class GreedyInline : public MInline {
 private:
  // Inline any call site with its weight larger than this threshold
  double InlineWeightThreshold;
  uint32 max_nodes_threshold;
  double growth_rate_threshold;

  // Benefit is used in computing a weight of a callsite. if you want to
  // process a call site first over other callsites, you need give it
  // larger benefit.
  // Benefit if an argument is constant.
  uint32 BenefitConstantArgument;
  // Benefit if an argument is not constant.
  uint32 BenefitVariableArgument;

  // Factor is the scale used in weight computation. It is used for
  // increasing or decreasing the important of an element in the
  // equation.
  // Factor for LoopDepthBonus.
  uint32 LoopDepthBonusFactor;
  // Factor for the use count of this callee if there is only one use.
  // uint32 OneUseBonusFactor;
  // Factor for the use count of this callee if there is only two uses.
  // uint32 TwoUseBonusFactor;
  // Factor for the whole weight that makes weight threshold more effective.
  uint32 WeightScaleFactor;

 public:
  GreedyInline(MIRModule &mod, MemPool *mp, CallGraph *cg) : MInline(mod, mp, nullptr, cg) {
    // todo change case for all fields
    max_nodes_threshold = 80;
    growth_rate_threshold = 0.10;  // max 10% bigger
    InlineWeightThreshold = 0;
    BenefitConstantArgument = 2;
    BenefitVariableArgument = 0;
    LoopDepthBonusFactor = 10;
    WeightScaleFactor = 400;
  }

  ~GreedyInline() {}

  void Inline();
  void RunGreedyInline(MIRFunction *func);

 private:
  CallSiteData GetBestCallSite(std::vector<CallSiteData> &callSites);
  bool HasAllowedOps(BlockNode *blk);
  bool CanInline(CGNode *node, std::unordered_map<MIRFunction *, bool> &canInline);
  uint32 ComputeCallSiteWeightBenefit(const CallSiteData &cs);
  double ComputeCallSiteWeight(CallSiteData &cs);
  void ComputeCallSitesWeight(std::vector<CallSiteData> &callSites);
  void CollectFunctionCallSites(CGNode *caller, std::vector<CallSiteData> &callSites,
                                std::unordered_map<MIRFunction *, bool> &canInline);
  void UpdateCallSites(CallSiteData &cs, std::vector<CallSiteData> &callSites,
                       std::unordered_map<MIRFunction *, bool> &canInline);
  void UpdateCallGraph(CallSiteData &cs);
  BlockNode *FindEnclosingBlk(BlockNode *blk, CallNode *callStmt);
};

class DoGreedyInline : public ModulePhase {
 public:
  DoGreedyInline(ModulePhaseID id) : ModulePhase(id) {}

  ~DoGreedyInline() {}

  AnalysisResult *Run(MIRModule *module, ModuleResultMgr *m) override;
  std::string PhaseName() const override {
    return "greedyinline";
  }
};
}  // namespace maple
#endif  // MAPLE_IPA_INCLUDE_GREEDYINLINE_H
