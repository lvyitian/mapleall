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

// This module analyzes the tag distribution in a switch statement and decides
// the best strategy in terms of runtime performance to generate code for it.
// The generated code makes use of 3 code generation techniques:
//
// 1. cascade of if-then-else based on equality test
// 2. rangegoto
// 3. binary search
//
// 1 is applied only if the number of possibilities is <= 6.
// 2 corresponds to indexed jump, but it requires allocating an array
// initialized with the jump targets.  Since it causes memory usage overhead,
// rangegoto is used only if the density is higher than 0.7.
// If neither 1 nor 2 is applicable, 3 is applied in the form of a decision
// tree.  In this case, each test would split the tags into 2 halves.  For
// each half, the above algorithm is then applied recursively until the
// algorithm terminates.
//
// But we don't want to apply 3 right from the beginning if both 1 and 2 do not
// apply, because there may be regions that have density > 0.7.  Thus, the
// switch lowerer begins by finding clusters.  A cluster is defined to be a
// maximal range of tags whose density is > 0.7.
//
// In finding clusters, the original switch table is sorted and then each dense
// region is condensed into 1 switch item; in the switch_items table, each item // either corresponds to an original
// entry in the original switch table (pair's // second is 0), or to a dense region (pair's second gives the upper limit
// of the dense range).  The output code is generated based on the switch_items. See BuildCodeForSwitchItems() which is
// recursive.

#include "mir_nodes.h"
#include "mir_builder.h"
#include "switch_lowerer.h"
#include "be_lowerer.h"
#include "mir_lower.h"  // "../../../maple_ir/include/mir_lower.h"
#include <cmath>
#include <algorithm>
#include <climits>

namespace maplebe {

using namespace maple;

static bool CasePairKeyLessThan(const CasePair &left, const CasePair &right) {
  return left.first < right.first;
}

void SwitchLowerer::FindClusters(MapleVector<cluster_t> &clusters) {
  int32 length = static_cast<int>(stmt->switchTable.size());
  int32 i = 0;
  while (i < length - kClusterSwitchCutoff) {
    for (int32 j = length - 1; j > i; j--) {
      float tmp1 = static_cast<float>(j - i);
      float tmp2 = static_cast<float>(stmt->switchTable[j].first - stmt->switchTable[i].first);
      if ((j - i) >= kClusterSwitchCutoff &&
          (stmt->switchTable[j].first - stmt->switchTable[i].first) < kMaxRangegotoTableSize &&
           tmp1 / tmp2 >= kClusterSwitchDensity) {
        clusters.push_back(cluster_t(i, j));
        i = j;
        break;
      }
    }
    i++;
  }
}

void SwitchLowerer::InitSwitchItems(MapleVector<cluster_t> &clusters) {
  if (clusters.empty()) {
    for (int32 i = 0; i < static_cast<int>(stmt->switchTable.size()); ++i) {
      switch_items.push_back(switch_item_t(i, 0));
    }
  } else {
    int32 j = 0;
    cluster_t front = clusters[j];
    for (int32 i = 0; i < static_cast<int>(stmt->switchTable.size()); ++i) {
      if (i == front.first) {
        switch_items.push_back(switch_item_t(i, front.second));

        i = front.second;
        j++;
        if (static_cast<int>(clusters.size()) > j) {
          front = clusters[j];
        }
      } else {
        switch_items.push_back(switch_item_t(i, 0));
      }
    }
  }
}

RangegotoNode *SwitchLowerer::BuildRangegotoNode(int32 startIdx, int32 endIdx) {
  RangegotoNode *node = mirModule.CurFuncCodeMemPool()->New<RangegotoNode>(&mirModule);
  node->SetOpnd(stmt->switchOpnd, 0);

  node->rangegotoTable = SmallCaseVector(mirModule.CurFuncCodeMemPoolAllocator()->Adapter());
  node->tagOffset = stmt->switchTable[startIdx].first;
  uint32 curtag = 0;
  node->rangegotoTable.push_back(SmallCasePair(curtag, stmt->switchTable[startIdx].second));
  int64 lastcasetag = stmt->switchTable.at(startIdx).first;
  for (int32 i = startIdx + 1; i <= endIdx; ++i) {
    while (stmt->switchTable[i].first != (lastcasetag + 1)) {
      // fill in a gap in the case tags
      curtag = (++lastcasetag) - node->tagOffset;
      node->rangegotoTable.push_back(SmallCasePair(curtag, stmt->defaultLabel));
    }
    curtag = stmt->switchTable[i].first - node->tagOffset;
    node->rangegotoTable.push_back(SmallCasePair(curtag, stmt->switchTable[i].second));
    lastcasetag = stmt->switchTable[i].first;
  }
  ASSERT(static_cast<int32>(node->rangegotoTable.size()) <= kMaxRangegotoTableSize,
          "rangegoto table exceeds allowed number of entries");
  ASSERT(node->numOpnds == 1, "RangegotoNode is a UnaryOpnd and numOpnds must be 1");
  return node;
}

CompareNode *SwitchLowerer::BuildCmpNode(Opcode opcode, uint32 idx) {
  CompareNode *binaryExpr = mirModule.CurFuncCodeMemPool()->New<CompareNode>(opcode);
  binaryExpr->primType = PTY_u32;
  binaryExpr->opndType = stmt->switchOpnd->primType;

  MIRConst *constVal = mirModule.memPool->New<MIRIntConst>(stmt->switchTable.at(idx).first,
                                                       GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(stmt->switchOpnd->primType)));
  ConstvalNode *exprConst = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>();
  exprConst->primType = stmt->switchOpnd->primType;
  exprConst->constVal = constVal;

  binaryExpr->bOpnd[0] = stmt->switchOpnd;
  binaryExpr->bOpnd[1] = exprConst;
  return binaryExpr;
}

GotoNode *SwitchLowerer::BuildGotoNode(int32 idx) {
  GotoNode *gotostmt = mirModule.CurFuncCodeMemPool()->New<GotoNode>(OP_goto);
  if (idx == -1) {
    gotostmt->offset = stmt->defaultLabel;
  } else {
    gotostmt->offset = stmt->switchTable.at(idx).second;
  }
  return gotostmt;
}

CondGotoNode *SwitchLowerer::BuildCondGotoNode(int32 idx, Opcode opcode, BaseNode *cond) {
  CondGotoNode *cgotostmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(opcode);
  cgotostmt->uOpnd = cond;
  if (idx == -1) {
    cgotostmt->offset = stmt->defaultLabel;
  } else {
    cgotostmt->offset = stmt->switchTable.at(idx).second;
  }
  return cgotostmt;
}

// start and end is with respect to switch_items[]
BlockNode *SwitchLowerer::BuildCodeForSwitchItems(int32 start, int32 end, bool lowbndchecked, bool highbndchecked) {
  BlockNode *localBlk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  if (start > end) {
    return localBlk;
  }
  CondGotoNode *cgoto = nullptr;
  RangegotoNode *rangegoto = nullptr;
  IfStmtNode *ifstmt = nullptr;
  CompareNode *cmpnode = nullptr;
  MIRLower mirlowerer(mirModule, lowerer->GetCurrentFunc());
  // if low side starts with a dense item, handle it first
  while (start <= end && switch_items[start].second != 0) {
    if (!lowbndchecked) {
      cgoto = BuildCondGotoNode(-1, OP_brtrue, BuildCmpNode(OP_lt, switch_items[start].first));
      localBlk->AddStatement(cgoto);
      lowbndchecked = true;
    }
    rangegoto = BuildRangegotoNode(switch_items[start].first, switch_items[start].second);
    cmpnode = BuildCmpNode(OP_le, switch_items[start].second);
    ifstmt = static_cast<IfStmtNode *>(mirModule.mirBuilder->CreateStmtIf(cmpnode));
    ifstmt->thenPart->AddStatement(rangegoto);
    localBlk->AppendStatementsFromBlock(mirlowerer.LowerIfStmt(ifstmt, false));
    if (start < end) {
      lowbndchecked = (stmt->switchTable[switch_items[start].second].first + 1 ==
                       stmt->switchTable[switch_items[start + 1].first].first);
    }
    start++;
  }
  // if high side starts with a dense item, handle it also
  while (start <= end && switch_items[end].second != 0) {
    if (!highbndchecked) {
      cgoto = BuildCondGotoNode(-1, OP_brtrue, BuildCmpNode(OP_gt, switch_items[end].second));
      localBlk->AddStatement(cgoto);
      highbndchecked = true;
    }
    rangegoto = BuildRangegotoNode(switch_items[end].first, switch_items[end].second);
    cmpnode = BuildCmpNode(OP_ge, switch_items[end].first);
    ifstmt = static_cast<IfStmtNode *>(mirModule.mirBuilder->CreateStmtIf(cmpnode));
    ifstmt->thenPart->AddStatement(rangegoto);
    localBlk->AppendStatementsFromBlock(mirlowerer.LowerIfStmt(ifstmt, false));
    if (start < end)
      highbndchecked =
        (stmt->switchTable[switch_items[end].first].first - 1 ==
         stmt->switchTable[switch_items[end - 1].first].first) ||
        (stmt->switchTable[switch_items[end].first].first - 1 == stmt->switchTable[switch_items[end - 1].second].first);
    end--;
  }
  if (start > end) {
    if (!lowbndchecked || !highbndchecked) {
      GotoNode *gotodft = BuildGotoNode(-1);
      localBlk->AddStatement(gotodft);
      jumpToDefaultBlockGenerated = true;
    }
    return localBlk;
  }
  if (start == end && lowbndchecked && highbndchecked) {
    // only 1 case with 1 tag remains
    localBlk->AddStatement(BuildGotoNode(switch_items[start].first));
    return localBlk;
  }
  if (end < start + kClusterSwitchCutoff) {
    // generate equality checks for what remains
    while (start <= end && switch_items[start].second == 0) {
      if (start == end && lowbndchecked && highbndchecked) {
        cgoto = (CondGotoNode *)BuildGotoNode(switch_items[start].first);  // can omit the condition
      } else {
        cgoto = BuildCondGotoNode(switch_items[start].first, OP_brtrue, BuildCmpNode(OP_eq, switch_items[start].first));
      }
      localBlk->AddStatement(cgoto);
      if (lowbndchecked && start < end) {
        lowbndchecked = (stmt->switchTable[switch_items[start].first].first + 1 ==
                         stmt->switchTable[switch_items[start + 1].first].first);
      }
      start++;
    }
    if (start <= end) {  // recursive call
      BlockNode *tmp = BuildCodeForSwitchItems(start, end, lowbndchecked, highbndchecked);
      CHECK_FATAL(tmp, "null ptr check ");
      localBlk->AppendStatementsFromBlock(tmp);
    } else if (!lowbndchecked || !highbndchecked) {
      GotoNode *gotodft = BuildGotoNode(-1);
      localBlk->AddStatement(gotodft);
      jumpToDefaultBlockGenerated = true;
    }
    return localBlk;
  }

  int64 lowesttag = stmt->switchTable[switch_items[start].first].first;
  int64 highesttag = stmt->switchTable[switch_items[end].first].first;

  // if lowesttag and higesttag have the same sign, use difference
  // if lowesttag and higesttag have the diefferent sign, use sum
  int64 middletag = ((((static_cast<uint64>(lowesttag)) ^ (static_cast<uint64>(lowesttag))) & (1LL << 63)) == 0)
                      ? /*same sign*/ (highesttag - lowesttag) / 2 + lowesttag
                      : /*opposite sign*/ (highesttag + lowesttag) / 2;
  // find the mid-point in switch_items between start and end
  int32 mid = start;
  while (stmt->switchTable[switch_items[mid].first].first < middletag) {
    mid++;
  }
  ASSERT(mid >= start, "switch lowering mid must be > start");
  ASSERT(mid <= end, "switch lowering mid must be <= end");
  // generate test for binary search
  cmpnode = BuildCmpNode(OP_ge, switch_items[mid].first);
  BaseNode *expnode =
      static_cast<BaseNode *>(mirModule.mirBuilder->CreateExprUnary(OP_lnot, GlobalTables::GetTypeTable().GetUInt1(), cmpnode));
  ifstmt = static_cast<IfStmtNode *>(mirModule.mirBuilder->CreateStmtIf(expnode));
  bool lefthighbndchecked = (stmt->switchTable[switch_items.at(mid - 1).first].first + 1 ==
                             stmt->switchTable[switch_items.at(mid).first].first) ||
                            (stmt->switchTable[switch_items.at(mid - 1).second].first + 1 ==
                             stmt->switchTable[switch_items.at(mid).first].first);
  ifstmt->thenPart = BuildCodeForSwitchItems(start, mid - 1, lowbndchecked, lefthighbndchecked);
  ifstmt->elsePart = BuildCodeForSwitchItems(mid, end, true, highbndchecked);
  if (ifstmt->elsePart) {
    ifstmt->numOpnds = 3;
  }
  localBlk->AppendStatementsFromBlock(mirlowerer.LowerIfStmt(ifstmt, false));
  return localBlk;
}

BlockNode *SwitchLowerer::LowerSwitch() {
  if (stmt->switchTable.size() == 0) {  // change to goto
    BlockNode *localBlk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
    GotoNode *gotodft = BuildGotoNode(-1);
    localBlk->AddStatement(gotodft);
    return localBlk;
  }
  MapleVector<cluster_t> clusters(ownallocator->Adapter());
  std::sort(stmt->switchTable.begin(), stmt->switchTable.end(), CasePairKeyLessThan);
  FindClusters(clusters);
  InitSwitchItems(clusters);
  BlockNode *blk = BuildCodeForSwitchItems(0, static_cast<int>(switch_items.size()) - 1, false, false);
  if (!jumpToDefaultBlockGenerated) {
    GotoNode *gotodft = BuildGotoNode(-1);
    blk->AddStatement(gotodft);
  }
  return blk;
}

}  // namespace maplebe
