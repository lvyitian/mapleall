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

#ifndef MAPLEBE_INCLUDE_CG_LSDA_H
#define MAPLEBE_INCLUDE_CG_LSDA_H
#include "types_def.h"
#include "mir_nodes.h"

namespace maplebe {

struct LabelPair {
  LabelNode *start_offset;
  LabelNode *end_offset;
};

struct LSDAHeader {
  LabelNode *lsda_label;
  uint8 lpstart_encoding;
  uint8 ttype_encoding;
  LabelPair ttype_offset;
  uint8 callsite_encoding;
};

struct LSDACallSite {
  LabelPair cs_start;
  LabelPair cs_length;
  LabelPair cs_landing_pad;
  uint32 cs_action;

 public:
  void Init(LabelNode *soffset, LabelNode *eoffset, LabelNode *lsoffset, LabelNode *leoffset, LabelNode *ldpsoffset,
            LabelNode *ldpeoffset, uint32 csaction) {
    cs_start.start_offset = soffset;
    cs_start.end_offset = eoffset;
    cs_length.start_offset = lsoffset;
    cs_length.end_offset = leoffset;
    cs_landing_pad.start_offset = ldpsoffset;
    cs_landing_pad.end_offset = ldpeoffset;
    cs_action = csaction;
  }
};

struct LSDAAction {
  uint8 action_index;
  uint8 action_filter;
  LSDAAction(uint8 ai, uint8 af) : action_index(ai), action_filter(af) {}
};

struct LSDAType {};

class LSDACallSiteTable {
 public:
  MapleVector<LSDACallSite *> callsite_table;
  LabelPair cs_table;
  LSDACallSiteTable(MapleAllocator *alloc) : callsite_table(alloc->Adapter()) {
    cs_table.start_offset = nullptr;
    cs_table.end_offset = nullptr;
  }
  virtual ~LSDACallSiteTable() {}
};

class LSDAActionTable {
 public:
  MapleVector<LSDAAction *> action_table;
  LSDAActionTable(MapleAllocator *alloc) : action_table(alloc->Adapter()) {}
  virtual ~LSDAActionTable() {}
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_LSDA_H
