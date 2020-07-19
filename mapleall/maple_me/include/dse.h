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

#ifndef MAPLE_ME_INCLUDE_DSE_H
#define MAPLE_ME_INCLUDE_DSE_H

#include "bb.h"
#include "dominance.h"

namespace maple {
class DSE {
 public:
  MIRModule *mirModule;
  StmtsSSAPart *stmtsSSAPart;
  Dominance *pdom;
  MapleAllocator dse_allocator;
  MapleVector<bool> bb_required;
  MapleUnorderedSet<uint32> stmt_required;  // key is stmtID; required if in set
  std::forward_list<VersionSt *> worklist;
  explicit DSE(MIRModule *mod, StmtsSSAPart *ssapart, Dominance *dom, MemPool *mp)
    : mirModule(mod),
      stmtsSSAPart(ssapart),
      pdom(dom),
      dse_allocator(mp),
      bb_required(dse_allocator.Adapter()),
      stmt_required(dse_allocator.Adapter()) {}

  void MarkLive(BaseNode *mirnode);
  void MarkVst(VersionSt *vst);
  void MarkBBLive(BB *bb);
  void MarkStmt(StmtNode *stmt, BB *bb);
  bool ExprNonDeletable(BaseNode *expr);
  virtual BB *GetBB(BBId id) {
    return nullptr;
  }

  bool StmtRequired(const StmtNode *s) {
    return stmt_required.find(s->stmtID) != stmt_required.end();
  }

  void SetStmtRequired(const StmtNode *s) {
    stmt_required.insert(s->stmtID);
  }
};
}  // namespace maple
#endif
