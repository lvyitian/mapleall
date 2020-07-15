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


#include <loop.h>
#include "cg.h"

namespace maplebe {

#define LOOPANALYSISDUMP CGDEBUGFUNC(cgfunc)

static void PrintLoopInfo(const loop_hierarchy *l) {
  LogInfo::MapleLogger() << "header " << l->header->id;
  MapleSet<BB *, loop_hierarchy::BbIdCmp>::iterator it;
  if (l->otherLoopEntries.size()) {
    LogInfo::MapleLogger() << " multi-header ";
    for (it = l->otherLoopEntries.begin(); it != l->otherLoopEntries.end(); ++it) {
      BB *en = *it;
      LogInfo::MapleLogger() << en->id << " ";
    }
  }
  if (l->outer_loop) {
    LogInfo::MapleLogger() << " parent " << l->outer_loop->header->id;
  }
  LogInfo::MapleLogger() << " backedge ";
  for (it = l->backedge.begin(); it != l->backedge.end(); ++it) {
    BB *be = *it;
    LogInfo::MapleLogger() << be->id << " ";
  }
  LogInfo::MapleLogger() << "\n members ";
  for (it = l->loop_members.begin(); it != l->loop_members.end(); ++it) {
    BB *ibb = *it;
    LogInfo::MapleLogger() << ibb->id << " ";
  }
  if (l->inner_loops.empty() == false) {
    LogInfo::MapleLogger() << "\n inner_loop_headers ";
    for (MapleSet<loop_hierarchy *>::iterator it = l->inner_loops.begin(); it != l->inner_loops.end(); ++it) {
      loop_hierarchy *inner = *it;
      LogInfo::MapleLogger() << inner->header->id << " ";
    }
  }
  LogInfo::MapleLogger() << "\n";
}

static void PrintInner(const loop_hierarchy *l, uint32 level) {
  MapleSet<loop_hierarchy *>::iterator it;
  for (it = l->inner_loops.begin(); it != l->inner_loops.end(); ++it) {
    loop_hierarchy *inner = *it;
    LogInfo::MapleLogger() << "loop-level-" << level << "\n";
    PrintLoopInfo(inner);
    PrintInner(inner, level + 1);
  }
}

void loop_hierarchy::PrintLoops(string name) {
  LogInfo::MapleLogger() << name << "\n";
  for (loop_hierarchy *l = this; l; l = l->next) {
    PrintLoopInfo(l);
  }
  for (loop_hierarchy *l = this; l; l = l->next) {
    PrintInner(l, 1);
  }
}

void CgfuncLoops::CheckLoops() {
  // Make sure backedge -> header relationship holds
  for (auto bEdge: backedge) {
    if (find(bEdge->succs.begin(), bEdge->succs.end(), header) == bEdge->succs.end()) {
      bool inOtherEntry = false;
      for (auto entry: multiEntries) {
        if (find(bEdge->succs.begin(), bEdge->succs.end(), entry) != bEdge->succs.end()) {
          inOtherEntry = true;
          break;
        }
      }
      if (inOtherEntry == false) {
        LogInfo::MapleLogger() << "Error: inconsistent loop backedge";
        CG_ASSERT(0, "loop backedge does not go to loop header");
      }
    }
    if (find(header->preds.begin(), header->preds.end(), bEdge) == header->preds.end()) {
      bool inOtherEntry = false;
      for (auto entry: multiEntries) {
        if (find(entry->preds.begin(), entry->preds.end(), bEdge) == entry->preds.end()) {
          inOtherEntry = true;
          break;
        }
      }
      if (inOtherEntry == false) {
        LogInfo::MapleLogger() << "Error: inconsistent loop header";
        CG_ASSERT(0, "loop header does not have a backedge");
      }
    }
  }

  // Make sure immediate containing loop members do not overlap
  for (auto iloop: inner_loops) {
    for (auto bb: iloop->loop_members) {
      if (find(loop_members.begin(), loop_members.end(), bb) != loop_members.end()) {
        LogInfo::MapleLogger() << "Error: inconsistent loop member";
        CG_ASSERT(0, "loop member overlap with inner loop");
      }
    }
  }

  if (inner_loops.empty() == false) {
    vector<CgfuncLoops *>::iterator lit;
    for (lit = inner_loops.begin(); lit != inner_loops.end(); ++lit) {
      CgfuncLoops *il = *lit;
      il->CheckLoops();
    }
  }
}

void CgfuncLoops::PrintLoops() {
  LogInfo::MapleLogger() << "loop_level(" << loopLevel << ") ";
  LogInfo::MapleLogger() << "header " << header->id << " ";
  vector<BB *>::iterator bit;
  if (multiEntries.size()) {
    LogInfo::MapleLogger() << "other-header ";
    for (bit = multiEntries.begin(); bit != multiEntries.end(); ++bit) {
      BB *bb = *bit;
      LogInfo::MapleLogger() << bb->id << " ";
    }
  }
  if (outer_loop) {
    LogInfo::MapleLogger() << "parent " << outer_loop->header->id << " ";
  }
  LogInfo::MapleLogger() << "backedge ";
  for (bit = backedge.begin(); bit != backedge.end(); ++bit) {
    BB *bb = *bit;
    LogInfo::MapleLogger() << bb->id << " ";
  }
  LogInfo::MapleLogger() << "\n members ";
  for (bit = loop_members.begin(); bit != loop_members.end(); ++bit) {
    BB *bb = *bit;
    LogInfo::MapleLogger() << bb->id << " ";
  }
  LogInfo::MapleLogger() << "\n";
  if (inner_loops.empty() == false) {
    vector<CgfuncLoops *>::iterator lit;
    LogInfo::MapleLogger() << " inner_loop_headers ";
    for (lit = inner_loops.begin(); lit != inner_loops.end(); ++lit) {
      CgfuncLoops *il = *lit;
      LogInfo::MapleLogger() << il->header->id << " ";
    }
    LogInfo::MapleLogger() << "\n";
    for (lit = inner_loops.begin(); lit != inner_loops.end(); ++lit) {
      CgfuncLoops *il = *lit;
      il->PrintLoops();
    }
  }
}

bool LoopFinder::DetectLoopSub(BB *header, BB *back) {
  if (recurseVisited[header->id]) {
    return false;
  }
  recurseVisited[header->id] = true;
  for (auto succ : header->succs) {
    if (succ == back) {
      return true;
    }
  }
  // not reachable yet, continue searching
  for (auto succ : header->succs) {
    if (DetectLoopSub(succ, back)) {
      return true;
    }
  }
  if (withEhBb) {
    for (auto ehSucc : header->eh_succs) {
      if (ehSucc == back) {
        return true;
      }
    }
    for (auto ehSucc : header->eh_succs) {
      if (DetectLoopSub(ehSucc, back)) {
        return true;
      }
    }
  }
  return false;
}

void LoopFinder::Insert(BB *bb, BB *header, set<BB *> &extraHeader) {
  if (find(candidate.begin(), candidate.end(), bb) == candidate.end()) {
    recurseVisited.clear();
    recurseVisited.resize(cgfunc->NumBBs());
    if (DetectLoopSub(header, bb)) {
      candidate.push_back(bb);
      stack.push(bb);
    } else {
      // since loop member predecessor is not reachable from loop header,
      // it is another entry to the loop.
      // If a backedge is to some bb not the header, then it should be
      // considered a different loop.
      for (auto succ : bb->succs) {
        if (find(candidate.begin(), candidate.end(), succ) != candidate.end()) {
          extraHeader.insert(succ);
          break;
        }
      }
      if (withEhBb) {
        for (auto ehsucc : bb->eh_succs) {
          if (find(candidate.begin(), candidate.end(), ehsucc) != candidate.end()) {
            extraHeader.insert(ehsucc);
            break;
          }
        }
      }
    }
  }
}

void LoopFinder::DetectLoop(
  BB *header,   // backege -> header
  BB *back
)
{
  set<BB *> moreHeaders;
  candidate.push_back(header);
  candidate.push_back(back);
  stack.push(back);
  while (stack.empty() == false) {
    BB *bb = stack.top();
    stack.pop();
    if (bb == header) {
      if (find(candidate.begin(), candidate.end(), bb) == candidate.end()) {
        candidate.push_back(bb);
      }
    } else {
      for (MapleList<BB *>::iterator it = bb->preds.begin(); it != bb->preds.end(); ++it){
        BB *ibb = *it;
        Insert(ibb, header, moreHeaders);
      }
    }
    if (withEhBb && bb != header) {
      for (MapleList<BB *>::iterator it = bb->eh_preds.begin(); it != bb->eh_preds.end(); ++it){
        BB *ibb = *it;
        Insert(ibb, header, moreHeaders);
      }
    }
  }
  loop_hierarchy *simple_loop = mp->New<loop_hierarchy>(mp);
  for (MapleList<BB *>::iterator it = candidate.begin(); it != candidate.end(); ++it) {
    BB *ibb = *it;
    simple_loop->loop_members.insert(ibb);
  }
  candidate.clear();
  simple_loop->header = header;
  simple_loop->backedge.insert(back);
  simple_loop->otherLoopEntries.insert(moreHeaders.begin(), moreHeaders.end());

  if (loops) {
    loops->prev = simple_loop;
  }
  simple_loop->next = loops;
  loops = simple_loop;
}

void LoopFinder::FindBackEdge() {
  BB *bb = nullptr;
  bool childPushed;
  while (dfs_bbs.empty() == false) {
    childPushed = false;
    bb = dfs_bbs.top();
    dfs_bbs.pop();
    CHECK_FATAL(bb != nullptr, "bb is null in LoopFinder::FindBackEdge");
    visited_bbs[bb->id] = true;
    if (bb->level == 0) {
      bb->level = 1;
    }
    std::stack<BB *> succs;
    // Mimic more of the recursive DFS by reversing the order of the succs.
    for (MapleList<BB *>::iterator it = bb->succs.begin(); it != bb->succs.end(); ++it) {
      BB *ibb = *it;
      succs.push(ibb);
    }
    while (succs.empty() == false) {
      BB *ibb = succs.top();
      succs.pop();
      if (visited_bbs[ibb->id] == false) {
        childPushed = true;
        ibb->level = bb->level + 1;
        sorted_bbs[ibb->id] = bb;  // tracking parent of traversed child
        dfs_bbs.push(ibb);
      } else if ((ibb->level != 0) && (bb->level >= ibb->level)) {
        // Backedge bb -> ibb
        DetectLoop(ibb, bb);
        bb->loop_succs.push_back(ibb);
        ibb->loop_preds.push_back(bb);
      }
    }
    if (withEhBb)
    for (MapleList<BB *>::iterator it = bb->eh_succs.begin(); it != bb->eh_succs.end(); ++it) {
      BB *ibb = *it;
      succs.push(ibb);
    }
    while (succs.empty() == false) {
      BB *ibb = succs.top();
      succs.pop();
      if (visited_bbs[ibb->id] == false) {
        childPushed = true;
        ibb->level = bb->level + 1;
        sorted_bbs[ibb->id] = bb;  // tracking parent of traversed child
        dfs_bbs.push(ibb);
      } else if ((ibb->level != 0) && (bb->level >= ibb->level)) {
        // Backedge
        DetectLoop(ibb, bb);
        bb->loop_succs.push_back(ibb);
        ibb->loop_preds.push_back(bb);
      }
    }
    // Remove duplicate bb that are visited from top of stack
    if (dfs_bbs.empty() == false) {
      BB *nbb = dfs_bbs.top();
      while (nbb) {
        if (visited_bbs[nbb->id] == true) {
          dfs_bbs.pop();
        } else {
          break;
        }
        if (dfs_bbs.empty() == false) {
          nbb = dfs_bbs.top();
        } else {
          break;
        }
      }
    }
    if (childPushed == false && dfs_bbs.empty() == false) {
      // reached the bottom of visited chain, reset level to the next visit bb
      bb->level = 0;
      BB *nbb = dfs_bbs.top();
      if (sorted_bbs[nbb->id]) {
        nbb = sorted_bbs[nbb->id];
        // All bb up to the top of stack bb's parent's child (its sibling)
        while (1) {
          // get parent bb
          BB *pbb = sorted_bbs[bb->id];
          if (pbb == nullptr || pbb == nbb) {
            break;
          }
          pbb->level = 0;
          bb = pbb;
        }
      }
    }
  }
}

void LoopFinder::MergeLoops() {
  for (loop_hierarchy *l1 = loops; l1; l1 = l1->next) {
    for (loop_hierarchy *l2 = l1->next; l2; l2 = l2->next) {
      if (l1->header != l2->header) {
        continue;
      }
      MapleSet<BB *, loop_hierarchy::BbIdCmp>::iterator it;
      for (it = l2->loop_members.begin(); it != l2->loop_members.end(); ++it) {
        BB *bb = *it;
        l1->loop_members.insert(bb);
      }
      for (it = l2->otherLoopEntries.begin(); it != l2->otherLoopEntries.end(); ++it) {
        BB *bb = *it;
        l1->otherLoopEntries.insert(bb);
      }
      for (it = l2->backedge.begin(); it != l2->backedge.end(); ++it) {
        BB *bb = *it;
        l1->backedge.insert(bb);
      }
      l2->prev->next = l2->next;
      if (l2->next) {
        l2->next->prev = l2->prev;
      }
    }
  }
}

void LoopFinder::SortLoops() {
  loop_hierarchy *head = nullptr;
  loop_hierarchy *next1 = nullptr;
  loop_hierarchy *next2 = nullptr;
  bool swapped;
  do {
    swapped = false;
    for (loop_hierarchy *l1 = loops; l1; ) {
      // remember l1's prev in case if l1 moved
      head = l1;
      next1 = l1->next;
      for (loop_hierarchy *l2 = l1->next; l2;) {
        next2 = l2->next;

        if (l1->loop_members.size() > l2->loop_members.size()) {
          if (head->prev == nullptr) {
            /* remove l2 from list */
            l2->prev->next = l2->next;
            if (l2->next) {
              l2->next->prev = l2->prev;
            }
            // link l2 as head
            loops = l2;
            l2->prev = nullptr;
            l2->next = head;
            head->prev = l2;
          } else {
            l2->prev->next = l2->next;
            if (l2->next) {
              l2->next->prev = l2->prev;
            }
            head->prev->next = l2;
            l2->prev = head->prev;
            l2->next = head;
            head->prev = l2;
          }
          head = l2;
          swapped = true;
        }
        l2 = next2;
      }
      l1 = next1;
    }
  } while (swapped);
}

void LoopFinder::CreateInnerLoop(loop_hierarchy *inner, loop_hierarchy *outer) {
  outer->inner_loops.insert(inner);
  inner->outer_loop = outer;
  MapleSet<BB *, loop_hierarchy::BbIdCmp>::iterator iti;
  MapleSet<BB *, loop_hierarchy::BbIdCmp>::iterator ito;
  for (iti = inner->loop_members.begin(); iti != inner->loop_members.end(); ++iti) {
    BB *bb = *iti;
    for (ito = outer->loop_members.begin(); ito != outer->loop_members.end();) {
      BB *rm = *ito;
      if (rm == bb) {
        ito = outer->loop_members.erase(ito);
      } else {
        ito++;
      }
    }
  }
  if (loops == inner) {
    loops = inner->next;
  } else {
    loop_hierarchy *prev = loops;
    for (loop_hierarchy *l1 = loops->next; l1; l1 = l1->next) {
      if (l1 == inner) {
        prev->next = prev->next->next;
      }
      prev = l1;
    }
  }
}

void LoopFinder::DetectInnerLoop() {
  bool innerCreated;
  do {
    innerCreated = false;
    for (loop_hierarchy *l1 = loops; l1; l1 = l1->next) {
      for (loop_hierarchy *l2 = l1->next; l2; l2 = l2->next) {
        if (l1->header != l2->header) {
          MapleSet<BB *, loop_hierarchy::BbIdCmp>::iterator it;
          for (it = l2->loop_members.begin(); it != l2->loop_members.end(); ++it) {
            BB *bb = *it;
            if (l1->header != bb) {
              continue;
            }
            CreateInnerLoop(l1, l2);
            innerCreated = true;
            break;
          }
          if (innerCreated) {
            break;
          }
        }
      }
      if (innerCreated) {
        break;
      }
    }
  } while (innerCreated);
}

static void CopyLoopInfo(loop_hierarchy *from, CgfuncLoops *to, CgfuncLoops *parent) {
  to->header = from->header;
  MapleSet<BB *, loop_hierarchy::BbIdCmp>::iterator it;
  for (it = from->otherLoopEntries.begin(); it != from->otherLoopEntries.end(); ++it) {
    BB *bb = *it;
    to->multiEntries.push_back(bb);
  }
  for (it = from->loop_members.begin(); it != from->loop_members.end(); ++it) {
    BB *bb = *it;
    to->loop_members.push_back(bb);
    bb->loop = to;
  }
  for (it = from->backedge.begin(); it != from->backedge.end(); ++it) {
    BB *bb = *it;
    to->backedge.push_back(bb);
  }
  if (from->inner_loops.empty() == false) {
    MapleSet<loop_hierarchy *>::iterator lit;
    for (lit = from->inner_loops.begin(); lit != from->inner_loops.end(); ++lit) {
      loop_hierarchy *lh = *lit;
      CgfuncLoops *floop = new CgfuncLoops;
      to->inner_loops.push_back(floop);
      floop->loopLevel = to->loopLevel + 1;
      CopyLoopInfo(lh, floop, to);
    }
  }
  if (parent) {
    to->outer_loop = parent;
  }
}

void LoopFinder::UpdateCgfunc() {
  for (loop_hierarchy *l = loops; l; l = l->next) {
    CgfuncLoops *floop = new CgfuncLoops;
    cgfunc->loops.push_back(floop);
    floop->loopLevel = 1;    // top level
    CopyLoopInfo(l, floop, nullptr);
  }
}

void LoopFinder::FormLoopHierarchy() {
  visited_bbs.resize(cgfunc->NumBBs());
  for (uint32 i = 0; i < cgfunc->NumBBs(); i++) {
    visited_bbs[i] = false;
    sorted_bbs.push_back(nullptr);
  }
  FOR_ALL_BB(bb, cgfunc) {
    bb->level = 0;
  }
  bool changed;
  do {
    changed = false;
    FOR_ALL_BB(bb, cgfunc) {
      if (visited_bbs[bb->id] == false) {
        dfs_bbs.push(bb);
        FindBackEdge();
        changed = true;
      }
      // For connected graph, one iteration should have touched all blocks.
      // However for EH bb, it might not be touched, and can be ignored
      // if EH edges are not considered.
      if (withEhBb == false) {
        changed = false;
        break;
      }
    }
  } while (changed == true);
  /* Start merging the loops with the same header */
  MergeLoops();
  /* order loops from least number of members */
  SortLoops();
  DetectInnerLoop();
  UpdateCgfunc();
}

AnalysisResult *CgDoLoopAnalysis::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (LOOPANALYSISDUMP) {
    DotGenerator::GenerateDot("buildloop", cgfunc, &(cgfunc->mirModule));
  }

  cgfunc->ClearLoopInfo();
  MemPool *loopmp = mempoolctrler.NewMemPool("loopmp");
  LoopFinder *loopFinder = cgfunc->NewLoopFinder(cgfunc, loopmp);
  loopFinder->withEhBb = true;
  loopFinder->FormLoopHierarchy();

  return loopFinder;
}

AnalysisResult *CgDoLoopAnalysisNoEh::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (LOOPANALYSISDUMP) {
    DotGenerator::GenerateDot("buildloopnoeh", cgfunc, &(cgfunc->mirModule));
  }

  cgfunc->ClearLoopInfo();
  MemPool *loopmp = mempoolctrler.NewMemPool("loopmp");
  LoopFinder *loopFinder = cgfunc->NewLoopFinder(cgfunc, loopmp);
  loopFinder->withEhBb = false;
  loopFinder->FormLoopHierarchy();

  if (LOOPANALYSISDUMP) {
    cgfunc->CheckLoop();
  }
  return loopFinder;
}

}  // namespace maplebe
