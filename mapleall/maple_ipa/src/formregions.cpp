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

#include "callgraph.h"
#include "formregions.h"
#include "option.h"
#include <iostream>

namespace maple {

// If it's singleFuncMode, form regions in reverse toplogical order of
// callgraph and each region consists of a single function.
// If not, each call creates one region out of the remaining functions in
// the module. If total size of all regions is under regionLimit, merge
// into one region.
// At last, mark if it is called from outside.

uint32 Region::regionLimit = 30000;    // size in number of statements
uint32 Region::regidNext = 1;          // 0 is reserved
uint32 Region::compileRegionId = 0xffffffffU;  // -1u(0xffffffff) indicates all regions will be compiled
bool Region::singleFuncMode = false;

void FormRegions::AddFuncToRegion(const CGNode *cgnode, Region *reg, const char *neighborkind) {
  MIRFunction *func = cgnode->GetMIRFunction();
  reg->funcmembers.push_back(func->puIdx);
  reg->regstmtcount += cgnode->GetStmtCount();
  assigned_region.at(func->puIdx) = reg->regid;
  if (!Options::quiet)
    INFO(kLncInfo, "function %s(%d) added to Region %d as %s", func->GetName().c_str(), func->puIdx, reg->regid,
         neighborkind);
}

void FormRegions::AddNeighborsInCallGraph(CGNode *cgnode) {
  Region *region = nullptr;
  if (theregions.size()) {
    region = theregions.back();
  } else {
    FATAL(kLncFatal, "maple vector is empty but use back()");
  }
  // if part of SCC, add the SCC nodes
  SCCNode *sccnode = cgnode->GetSCCNode();
  if (sccnode != nullptr) {
    for (MapleVector<CGNode *>::iterator itSccnd = sccnode->cgNodes.begin(); itSccnd != sccnode->cgNodes.end();
         itSccnd++) {
      MIRFunction *func = (*itSccnd)->GetMIRFunction();
      if (func == nullptr || func->body == nullptr || assigned_region[func->puIdx] != 0) {
        continue;
      }
      if (region->HasRoom(*itSccnd)) {
        AddFuncToRegion(*itSccnd, region, "scc member");
      }
    }
  }
  // look through the callers
  for (MapleSet<CGNode *>::iterator itCaller = cgnode->CallerBegin(); itCaller != cgnode->CallerEnd(); itCaller++) {
    CGNode *caller = *itCaller;
    MIRFunction *func = caller->GetMIRFunction();
    if (func == nullptr || func->body == nullptr || assigned_region[func->puIdx] != 0) {
      continue;
    }
    if (region->HasRoom(caller)) {
      AddFuncToRegion(caller, region, "caller");
    }
  }
  // look through the direct callees
  for (MapleVector<Callsite>::iterator itCallee = cgnode->CalleeBegin(); itCallee != cgnode->CalleeEnd(); itCallee++) {
    CGNode *callee = (*itCallee).second;
    MIRFunction *func = callee->GetMIRFunction();
    if (func == nullptr || func->body == nullptr || assigned_region[func->puIdx] != 0) {
      continue;
    }
    if (region->HasRoom(callee)) {
      AddFuncToRegion(callee, region, "callee");
    }
  }
}

// each call creates one region out of the remaining functions in the module;
// return false if no region formed
bool FormRegions::FormOneRegion() {
  MIRFunction *func = nullptr;
  CGNode *firstmem = callgraph->GetEntryNode();
  if (firstmem != nullptr) {
    func = firstmem->GetMIRFunction();
    if (func->body == nullptr) {
      firstmem = nullptr;
    }
  }
  // CHECK_FATAL(func, "func is null in FormRegions::FormOneRegion");
  bool found = firstmem != nullptr && assigned_region.at(func->puIdx) == 0;
  if (!found) {
    // find a root node
    for (CGNode *root : callgraph->GetRootNodes()) {
      func = root->GetMIRFunction();
      if (func->body != nullptr && assigned_region[func->puIdx] == 0) {
        firstmem = root;
        found = true;
        break;
      }
    }
  }
  if (!found) {
    // find any node not assigned to any region
    for (CGNode *node : callgraph->GetNodesMap()) {
      if (node == nullptr) {
        continue;
      }
      func = node->GetMIRFunction();
      if (func->body != nullptr && assigned_region[func->puIdx] == 0) {
        firstmem = node;
        found = true;
        break;
      }
    }
  }
  if (!found) {
    return false;
  }
  Region *region = alloc.GetMemPool()->New<Region>(&alloc);
  theregions.push_back(region);
  AddFuncToRegion(firstmem, region, "first node");
  uint32 i = 0;
  do {
    CGNode *nodeinreg = callgraph->GetCGNode(static_cast<int32>(region->funcmembers[i]));
    CHECK_FATAL(nodeinreg, "null ptr check");
    AddNeighborsInCallGraph(nodeinreg);
    i++;
  } while (i < region->funcmembers.size());
  return true;
}

void FormRegions::MergeAllRegionsToOne(uint32 totalFunctions) {
  Region *firstreg = theregions.at(0);
  uint32 firstregfuncmembersSize = firstreg->funcmembers.size();
  firstreg->funcmembers.resize(totalFunctions);
  // merge to the first region start from the last region
  do {
    Region *reg = theregions.back();
    firstreg->regstmtcount += reg->regstmtcount;
    for (PUIdx pidx : reg->funcmembers) {
      firstreg->funcmembers[firstregfuncmembersSize++] = pidx;
    }
    theregions.pop_back();
  } while (theregions.size() > 1);
  ASSERT(firstreg->funcmembers.size() == firstregfuncmembersSize && theregions.size() == 1,
          "DoFormRegions: error in merging regions to the first region");
  // update called_from_outside vector
  for (PUIdx pidx = 0; pidx < called_from_outside.size(); pidx++) {
    if (!called_from_outside[pidx]) {
      continue;
    }
    CGNode *cgnode = callgraph->GetCGNode(pidx);
    CHECK_FATAL(cgnode, "null ptr check");
    if (cgnode->HasCaller()) {
      called_from_outside[pidx] = false;
    }
  }
  if (!Options::quiet) {
    INFO(kLncInfo, "all regions merged to Region 1");
  }
}

// determine called_from_outside for each function in each region
void FormRegions::DetermineCalledFromOutside() {
  for (Region *reg : theregions) {
    for (uint32 i = 0; i < reg->funcmembers.size(); i++) {
      PUIdx pidx = reg->funcmembers[i];
      CGNode *cgnode = callgraph->GetCGNode(pidx);
      CHECK_FATAL(cgnode != nullptr, "cgnode is null in FormRegions::DetermineCalledFromOutside");
      if (!cgnode->HasCaller()) {
        called_from_outside[pidx] = true;
        continue;
      }
      bool hasCallerFromOutside = false;
      for (MapleSet<CGNode *>::iterator itCaller = cgnode->CallerBegin(); itCaller != cgnode->CallerEnd(); itCaller++) {
        CGNode *caller = *itCaller;
        MIRFunction *callerfunc = caller->GetMIRFunction();
        if (assigned_region[callerfunc->puIdx] != reg->regid) {
          hasCallerFromOutside = true;
          break;
        }
        // if caller is same with itself, mark it called from outside.
        // otherwise the wcfg might end with no entry func.
        if (caller == cgnode) {
          hasCallerFromOutside = true;
          break;
        }
      }
      called_from_outside[pidx] = hasCallerFromOutside;
    }
  }
}

// Form regions in reverse toplogical order of callgraph. Each region
// consists of a single function.
void FormRegions::FormSingleFunRegions() {
  const MapleVector<SCCNode *> &sccTopVec = callgraph->GetSCCTopVec();
  for (unsigned i = sccTopVec.size() - 1; i != 0; i--) {
    SCCNode *sccNode = sccTopVec[i];
    for (auto const kIt : sccNode->cgNodes) {
      CGNode *node = kIt;
      MIRFunction *func = node->GetMIRFunction();
      if (func && func->body) {
        Region *region = alloc.GetMemPool()->New<Region>(&alloc);
        theregions.push_back(region);
        AddFuncToRegion(node, region, "single node");
      }
    }
  }
}

/* Interface to run fully inline module phase. */
AnalysisResult *DoFormRegions::Run(MIRModule *module, ModuleResultMgr *m) {
  MemPool *mp = mempoolctrler.NewMemPool("formregions mempool");
  CallGraph *cg = static_cast<CallGraph *>(m->GetAnalysisResult(MoPhase_CALLGRAPH_ANALYSIS, module));
  ASSERT(cg != nullptr, "DoFormRegions: call graph cannot be nullptr");
  FormRegions *formreg = mp->New<FormRegions>(mp, cg);

  if (Region::singleFuncMode) {
    formreg->FormSingleFunRegions();
  } else {
    while (formreg->FormOneRegion()) {
    }

    if (formreg->theregions.size() > 1) {
      // if total size of all regions is under regionLimit, merge into 1 region
      uint32 totalRegstmtcount = 0;
      uint32 totalRegfunctions = 0;  // total number of functions in all regions
      for (Region *reg : formreg->theregions) {
        totalRegstmtcount += reg->regstmtcount;
        totalRegfunctions += reg->funcmembers.size();
      }
      if (totalRegstmtcount <= Region::regionLimit) {
        formreg->MergeAllRegionsToOne(totalRegfunctions);
      }
    }
  }

  formreg->DetermineCalledFromOutside();

  m->AddResult(GetPhaseId(), module, formreg);
  return formreg;
}

}  // namespace maple
