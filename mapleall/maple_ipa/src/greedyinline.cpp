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

#include "greedyinline.h"
#include "constant_fold.h"
#include "mpl_logging.h"
#include <cmath>
#include <algorithm>

using namespace std;
namespace maple {

#define DEBUG_INLINE 0

static void PrintCallSites(vector<CallSiteData> &callSites) {
  for (auto it : callSites) {
    it.Print();
  }
}

uint32 GreedyInline::ComputeCallSiteWeightBenefit(const CallSiteData &cs) {
  uint32 benefit = 1;
  CallNode *call = static_cast<CallNode *>(cs.ci->GetCallStmt());
  for (uint32 i = 0; i < call->nOpnd.size(); i++) {
    base_node_t *arg = call->nOpnd[i];
    if (arg != nullptr && arg->op == OP_constval) {
      benefit += BenefitConstantArgument;
    } else {
      benefit += BenefitVariableArgument;
    }
  }
  return benefit;
}

/*
   ComputeCallSiteWeight: Compute weight from the given call site.

   benefit: a value added to favor a certain feature of a call site
   loop_depth_bonus: if a callsite  is inside a loop, it is preferred over
                    a call site not in a loop.
   callee_size: we should consider smaller function at first. Because code size
               10 and 15 don't make much difference, sqrt is used to make
               callee size denser.
 */
double GreedyInline::ComputeCallSiteWeight(CallSiteData &cs) {
  CGNode *callee = cs.callee;
  uint32 calleeSize = callee->GetNodeCount();

  uint32 loopDepthBonus = 1;
  loopDepthBonus += cs.ci->GetLoopDepth() * LoopDepthBonusFactor;

  double benefit = static_cast<double>(ComputeCallSiteWeightBenefit(cs));

  /*
     Enable these two once we have a way to know if we are analyzing the whole
     program (and so be able to delete functions whose call sites are all inlined).
     In that case the following factors are useful.

     use_bonus: if a callee only has one or two uses, we like to consider
               it at first because it has many benefit if it is processed at
               first, like reducing code size and the number of functions in
               the queue to speed up inliner.
     use_count: if a callee has a lot of callers in the module, we may leave
               it to be processed after those with smaller use_count.
  */
 double weight = benefit * loopDepthBonus /* * use_bonus */ * WeightScaleFactor / ( /* use_count * */ sqrt(calleeSize));

  if (DEBUG_INLINE) {
    printf("Call site weight for %s call in caller %s = %f\n", callee->GetMIRFuncName().c_str(),
           cs.caller->GetMIRFuncName().c_str(), weight);

    printf("Where:\n");
    printf("  benefit: %f\n", benefit);
    printf("  loop_depth_bonus: %u\n", loopDepthBonus);
    printf("  WeightScaleFactor: %u\n", WeightScaleFactor);
    printf("  sqrt(callee_size): %f\n", sqrt(calleeSize));
    printf("----------------\n");
  }
  CHECK_FATAL(calleeSize != 0, "callee_size is 0!");
  return weight;
}

bool GreedyInline::HasAllowedOps(BlockNode *blk) {
  CHECK_FATAL(blk, "expecting a BlockNode, found nullptr");
  for (StmtNode *stmt = blk->GetFirst(); stmt != nullptr; stmt = static_cast<StmtNode *>(stmt)->GetNext()) {
    switch (stmt->op) {
      case OP_foreachelem: {
        ForeachelemNode *n = static_cast<ForeachelemNode *>(stmt);
        if (!HasAllowedOps(n->loopBody)) {
          return false;
        }
        break;
      }
      case OP_doloop: {
        DoloopNode *n = static_cast<DoloopNode *>(stmt);
        if (!HasAllowedOps(n->doBody)) {
          return false;
        }
        break;
      }
      case OP_dowhile:
      case OP_while: {
        WhileStmtNode *n = static_cast<WhileStmtNode *>(stmt);
        if (!HasAllowedOps(n->body)) {
          return false;
        }
        break;
      }
      case OP_if: {
        IfStmtNode *n = static_cast<IfStmtNode *>(stmt);
        if (!HasAllowedOps(n->thenPart) || (n->elsePart && !HasAllowedOps(n->elsePart))) {
          return false;
        }
        break;
      }
      case OP_virtualcall:
      case OP_superclasscall:
      case OP_interfacecall:
      case OP_icall:
      case OP_virtualcallassigned:
      case OP_superclasscallassigned:
      case OP_interfacecallassigned:
      case OP_icallassigned:
      case OP_customcallassigned:
      case OP_polymorphiccallassigned:
      case OP_customcall:
      case OP_polymorphiccall:
      case OP_jstry:
      case OP_javatry:
      case OP_cpptry:
      case OP_try:
      case OP_throw:
      case OP_jscatch:
      case OP_javacatch:
      case OP_catch:
      case OP_cppcatch:
      case OP_finally:
      case OP_cleanuptry:
      case OP_endtry:
      case OP_gosub:
      case OP_retsub:
      // With sync block
      case OP_syncenter:
      case OP_syncexit:
        return false;
      case OP_block:
        ASSERT(false, "Unexpected BlockNode");
        break;
      default:
        break;
    }
  }
  return true;
}

bool GreedyInline::CanInline(CGNode *node, unordered_map<MIRFunction *, bool> &canInline) {
  MIRFunction *func = node->GetMIRFunction();
  if (func == nullptr) {
    return false;
  }
  auto it = canInline.find(func);
  if (it != canInline.end()) {
    return it->second;
  }

  bool ret = true;
  if (  // Func has no body or is mutually recursive
    func->body == nullptr || node->GetSCCNode()->HasRecursion() ||
    // Func is too big and has more than one callee
    (node->GetNodeCount() > max_nodes_threshold && node->NumberOfUses() != 1) ||
    // Func has a disallowed attribute
    // Keep in sync with mapleall/maple_ir/include/func_attrs.def
    // Allow FUNCATTR: abstract, bridge, const, final, local, private, public,
    //                 static, constructor (if it has an empty body)
    ((func->GetAttr(FUNCATTR_constructor) && !func->body->IsEmpty()) || func->GetAttr(FUNCATTR_declared_synchronized) ||
     func->GetAttr(FUNCATTR_extern) || func->GetAttr(FUNCATTR_generic) || func->GetAttr(FUNCATTR_implicit) ||
     func->GetAttr(FUNCATTR_interface) || func->GetAttr(FUNCATTR_native) || func->GetAttr(FUNCATTR_protected) ||
     func->GetAttr(FUNCATTR_strict) || func->GetAttr(FUNCATTR_synchronized) || func->GetAttr(FUNCATTR_synthetic) ||
     func->GetAttr(FUNCATTR_varargs) || func->GetAttr(FUNCATTR_virtual) || func->GetAttr(FUNCATTR_weak)) ||
    !HasAllowedOps(func->body)) {
    ret = false;
  }
  canInline.insert(pair<MIRFunction *, bool>(func, ret));
  return ret;
}

void GreedyInline::CollectFunctionCallSites(CGNode *caller, vector<CallSiteData> &callSites,
                                            unordered_map<MIRFunction *, bool> &canInline) {
  for (auto it = caller->CalleeBegin(); it != caller->CalleeEnd(); it++) {
    CallInfo *ciCallee = it->first;
    CGNode *callee = it->second;
    if (ciCallee->GetCallType() == kCallTypeCall && CanInline(callee, canInline)) {
      callSites.push_back(CallSiteData(ciCallee, callee, caller));
    } else if (DEBUG_INLINE) {
      printf("Skipping '%s': cannot inline\n", callee->GetMIRFuncName().c_str());
    }
  }
}

void GreedyInline::ComputeCallSitesWeight(vector<CallSiteData> &callSites) {
  auto it = callSites.begin();
  while (it != callSites.end()) {
    double weight = ComputeCallSiteWeight(*it);

    if (weight <= InlineWeightThreshold) {
      if (DEBUG_INLINE) {
        printf("Skip this CallSite (weight <= InlineWeightThreshold):\n");
        it->Print();
      }
      it = callSites.erase(it);
    } else {
      it->weight = weight;
      ++it;
    }
  }
}

CallSiteData GreedyInline::GetBestCallSite(vector<CallSiteData> &callSites) {
  vector<CallSiteData>::iterator bestIdx = callSites.end();
  double maxWeight = std::numeric_limits<double>::min();
  for (auto it = callSites.begin(); it != callSites.end(); it++) {
    double weight = it->weight;
    if (weight > maxWeight) {
      bestIdx = it;
      maxWeight = weight;
    }
  }

  if (bestIdx == callSites.end()) {
    FATAL(kLncFatal, "Can not find max in vector ");
  }
  CallSiteData res = *bestIdx;
  callSites.erase(bestIdx);
  return res;
}

void GreedyInline::UpdateCallGraph(CallSiteData &cs) {
  cs.callee->DelCaller(cs.caller);
  cg_->UpdateCallGraphNode(cs.caller);
  cg_->RecomputeSCC();
}

void GreedyInline::UpdateCallSites(CallSiteData &cs, vector<CallSiteData> &callSites,
                                   unordered_map<MIRFunction *, bool> &canInline) {
  if (cs.callee->NumberOfCallSites() == 0) {
    // The inlined cs.callee does not contribute to new call sites to cs.caller
    return;
  }
  if (DEBUG_INLINE) {
    PrintCallSites(callSites);
    printf("\nUpdateCallSites--------------\n");
  }
  // We inlined cs.callee into cs.caller, delete the old call sites of
  // cs.caller
  auto itCs = callSites.begin();
  while (itCs != callSites.end()) {
    if (itCs->caller == cs.caller) {
      itCs = callSites.erase(itCs);
    } else {
      itCs++;
    }
  }
  if (DEBUG_INLINE) {
    PrintCallSites(callSites);
    printf("\nUpdateCallSites--------------\n");
  }
  // Add all new call sites in cs.caller
  vector<CallSiteData> newCallSites;
  for (auto it = cs.caller->CalleeBegin(); it != cs.caller->CalleeEnd(); it++) {
    CallInfo *ciCallee = it->first;
    CGNode *callee = it->second;
    if (ciCallee->GetCallType() == kCallTypeCall && CanInline(callee, canInline)) {
      newCallSites.push_back(CallSiteData(ciCallee, callee, cs.caller));
    }
  }
  ComputeCallSitesWeight(newCallSites);
  for (auto it : newCallSites) {
    callSites.push_back(it);
  }
  if (DEBUG_INLINE) {
    PrintCallSites(callSites);
    printf("\nUpdateCallSites---------END\n");
  }
}

BlockNode *GreedyInline::FindEnclosingBlk(BlockNode *blk, CallNode *callStmt) {
  CHECK_FATAL(blk, "expecting a BlockNode, found nullptr");
  BlockNode *res = nullptr;
  for (StmtNode *stmt = blk->GetFirst(); stmt != nullptr; stmt = static_cast<StmtNode *>(stmt)->GetNext()) {
    switch (stmt->op) {
      case OP_foreachelem: {
        ForeachelemNode *n = static_cast<ForeachelemNode *>(stmt);
        res = FindEnclosingBlk(n->loopBody, callStmt);
        break;
      }
      case OP_doloop: {
        DoloopNode *n = static_cast<DoloopNode *>(stmt);
        res = FindEnclosingBlk(n->doBody, callStmt);
        break;
      }
      case OP_dowhile:
      case OP_while: {
        WhileStmtNode *n = static_cast<WhileStmtNode *>(stmt);
        res = FindEnclosingBlk(n->body, callStmt);
        break;
      }
      case OP_if: {
        IfStmtNode *n = static_cast<IfStmtNode *>(stmt);
        res = FindEnclosingBlk(n->thenPart, callStmt);
        if (res == nullptr && n->elsePart) {
          res = FindEnclosingBlk(n->elsePart, callStmt);
        }
        break;
      }
      case OP_call:
      case OP_callassigned:
        if (stmt == callStmt) {
          res = blk;
        }
        break;
      case OP_block:
        ASSERT(false, "Unexpected BlockNode");
        break;
      default:;  // skip
    }
    if (res != nullptr) {
      break;
    }
  }
  return res;
}

void GreedyInline::Inline() {
  vector<CallSiteData> callSites;
  unordered_map<MIRFunction *, bool> canInline;
  uint64 totalSize = 0;
  for (CGNode *caller : cg_->GetNodesMap()) {
    if (caller == nullptr) {
      continue;
    }
    if (DEBUG_INLINE) {
      printf("processing %s...\n", caller->GetMIRFuncName().c_str());
    }
    CollectFunctionCallSites(caller, callSites, canInline);
    totalSize += caller->GetNodeCount();
  }
  ComputeCallSitesWeight(callSites);
  if (DEBUG_INLINE) {
    PrintCallSites(callSites);
    printf("TotalSize %u\n", static_cast<uint32>(totalSize));
  }

  vector<CallSiteData> toInline;
  const MapleVector<CGNode *> &roots = cg_->GetRootNodes();
  uint64 oldSize = totalSize;
  while (callSites.size() > 0) {
    CallSiteData cs = GetBestCallSite(callSites);
    // NumCallSiteProcessed++;
    CGNode *caller = cs.caller;
    CGNode *callee = cs.callee;

    if (caller->NumberOfUses() == 0 && find(roots.begin(), roots.end(), caller) == roots.end()) {
      // The call site belong to a function never called
      continue;
    }
    totalSize += callee->GetNodeCount();
    if (oldSize == 0) {
      FATAL(kLncFatal, "divided by zero ");
    }
    double growthRate = static_cast<double>(totalSize - oldSize) / static_cast<double>(oldSize);
    if (growthRate > growth_rate_threshold) {
      if (DEBUG_INLINE) {
        printf("Inliner stopped due to growth rate > %f%%\n", growth_rate_threshold * 100.0);
      }
      break;
    }

    if (DEBUG_INLINE) {
      printf("-> INLINING '%s' (Weight %f) to '%s'...\n", callee->GetMIRFuncName().c_str(), cs.weight,
             caller->GetMIRFuncName().c_str());
    }
    MIRFunction *callerFunc = caller->GetMIRFunction();
    CallNode *callStmt = static_cast<CallNode *>(cs.ci->GetCallStmt());
    BlockNode *enclosingBlk = FindEnclosingBlk(callerFunc->body, callStmt);
    CHECK_FATAL(enclosingBlk, "null ptr check");
    PerformInline(callerFunc, enclosingBlk, callStmt, callee->GetMIRFunction());

    UpdateCallGraph(cs);
    UpdateCallSites(cs, callSites, canInline);
  }
}

/* Unified interface to run inline module phase. */
AnalysisResult *DoGreedyInline::Run(MIRModule *module, ModuleResultMgr *m) {
  MemPool *mp = mempoolctrler.NewMemPool("inline mempool");
  CallGraph *cg = static_cast<CallGraph *>(m->GetAnalysisResult(MoPhase_CALLGRAPH_ANALYSIS, module));
  CHECK_FATAL(cg != nullptr, "Expecting a valid CallGraph, found nullptr");
  GreedyInline ginline(*module, mp, cg);
  cg->DumpToFile(false);
  ginline.Inline();
  cg->DumpToFile(true);
  maple::ConstantFold cf(module);
  for (MapleVector<MIRFunction *>::iterator it = module->functionList.begin(); it != module->functionList.end();
       it++) {
    MIRFunction *func = *it;
    module->SetCurFunction(func);
    if (func->body == nullptr) {
      continue;
    }
    cf.Simplify(func->body);
  }
  return nullptr;
}

}  // namespace maple
