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

#ifndef MAPLEBE_INCLUDE_BE_SWITCHLOWERER_H
#define MAPLEBE_INCLUDE_BE_SWITCHLOWERER_H
#include "mir_nodes.h"
#include "mir_module.h"

namespace maplebe {

class BELowerer;

class SwitchLowerer {
 private:
  typedef std::pair<int32, int32> cluster_t;
  typedef std::pair<int32, int32> switch_item_t;

  MIRModule &mirModule;
  SwitchNode *stmt;
  BELowerer *lowerer;
  // the original switch table is sorted and then each dense (in terms of the
  // case tags) region is condensed into 1 switch item; in the switch_items
  // table, each item either corresponds to an original entry in the original
  // switch table (pair's second is 0), or to a dense region (pair's second
  // gives the upper limit of the dense range)
  MapleVector<switch_item_t> switch_items;  // uint32 is index in switchTable
  MapleAllocator *ownallocator;
  const int32 kClusterSwitchCutoff;
  const float kClusterSwitchDensity;
  const int32 kMaxRangegotoTableSize;
  bool jumpToDefaultBlockGenerated;

 public:
  explicit SwitchLowerer(MIRModule &mod, SwitchNode *stmt, BELowerer *lowerer, MapleAllocator *allocator)
    : mirModule(mod),
      stmt(stmt),
      lowerer(lowerer),
      switch_items(allocator->Adapter()),
      ownallocator(allocator),
      kClusterSwitchCutoff(6),
      kClusterSwitchDensity(0.7),
      kMaxRangegotoTableSize(127),
      jumpToDefaultBlockGenerated(false) {}

  ~SwitchLowerer() {}

  BlockNode *LowerSwitch();

 private:
  void FindClusters(MapleVector<cluster_t> &);
  void InitSwitchItems(MapleVector<cluster_t> &clusters);
  RangegotoNode *BuildRangegotoNode(int32 startIdx, int32 endIdx);
  CompareNode *BuildCmpNode(Opcode opcode, uint32 idx);
  GotoNode *BuildGotoNode(int32 idx);
  CondGotoNode *BuildCondGotoNode(int32 idx, Opcode opcode, BaseNode *cond);
  BlockNode *BuildCodeForSwitchItems(int32 start, int32 end, bool lowbndchecked, bool highbndchecked);
};

}  // namespace maplebe

#endif /* MAPLEBE_INCLUDE_BE_SWITCHLOWERER_H */
