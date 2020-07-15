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

#ifndef MAPLEBE_INCLUDE_CG_LOOP_H
#define MAPLEBE_INCLUDE_CG_LOOP_H

#include <cg_phase.h>
#include <cg_func.h>
#include <cg_bb.h>
#include <insn.h>
#include <iostream>

namespace maplebe {

using namespace std;

class loop_hierarchy;

class LoopFinder : public AnalysisResult {
 public:
  CGFunc *cgfunc;
  MemPool *mp;
  MapleAllocator loopmp;

  MapleStack<BB *> stack;
  MapleList<BB *> candidate;  // loop candidate
  MapleVector<bool> visited_bbs;
  MapleVector<BB *> sorted_bbs;
  MapleStack<BB *> dfs_bbs;
  MapleVector<bool> recurseVisited;
  loop_hierarchy *loops;

  bool withEhBb;

  LoopFinder(CGFunc *func, MemPool *mem)
    : AnalysisResult(mem),
      cgfunc(func),
      mp(mem),
      loopmp(mp),
      stack(loopmp.Adapter()),
      candidate(loopmp.Adapter()),
      visited_bbs(loopmp.Adapter()),
      sorted_bbs(loopmp.Adapter()),
      dfs_bbs(loopmp.Adapter()),
      recurseVisited(loopmp.Adapter()),
      loops(nullptr),
      withEhBb(false) {}

  ~LoopFinder() {}

  void Insert(BB *bb, BB *header, set<BB *> &extraHeader);
  void DetectLoop(BB *header, BB *back);
  bool DetectLoopSub(BB *header, BB *back);
  void FindBackEdge();
  void MergeLoops();
  void SortLoops();
  void CreateInnerLoop(loop_hierarchy *inner, loop_hierarchy *outer);
  void DetectInnerLoop();
  void UpdateCgfunc();
  void FormLoopHierarchy();
};

class loop_hierarchy {
 public:
  struct BbIdCmp {
    bool operator()(const BB *bb1, const BB *bb2) const {
      return (bb1->id < bb2->id);
    }
  };

  MapleAllocator loopmp;
  BB *header;
  MapleSet<BB *, BbIdCmp> otherLoopEntries;
  MapleSet<BB *, BbIdCmp> loop_members;
  MapleSet<BB *, BbIdCmp> backedge;
  MapleSet<loop_hierarchy *> inner_loops;
  loop_hierarchy *outer_loop;
  loop_hierarchy *prev;
  loop_hierarchy *next;

  loop_hierarchy(MemPool *mp)
    : loopmp(mp),
      header(nullptr),
      otherLoopEntries(loopmp.Adapter()),
      loop_members(loopmp.Adapter()),
      backedge(loopmp.Adapter()),
      inner_loops(loopmp.Adapter()),
      outer_loop(nullptr),
      prev(nullptr),
      next(nullptr) {}

  virtual ~loop_hierarchy() {}

  void PrintLoops(string name);
};

class CgfuncLoops {
 public:
  BB *header;
  vector<BB *> multiEntries;
  vector<BB *> loop_members;
  vector<BB *> backedge;
  vector<CgfuncLoops *> inner_loops;
  CgfuncLoops *outer_loop;
  uint32 loopLevel;

  CgfuncLoops() : header(nullptr), outer_loop(nullptr), loopLevel(0) {}

  virtual ~CgfuncLoops() {}

  void CheckLoops();
  void PrintLoops();
};

CGFUNCPHASE(CgDoLoopAnalysis, "loopanalysis")
CGFUNCPHASE(CgDoLoopAnalysisNoEh, "loopanalysisnoeh")
}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_LOOP_H
