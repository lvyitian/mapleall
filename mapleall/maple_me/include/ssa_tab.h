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

#ifndef MAPLE_ME_INCLUDE_SSA_TAB_H
#define MAPLE_ME_INCLUDE_SSA_TAB_H
#include "mempool.h"
#include "mempool_allocator.h"
#include "phase.h"
#include "ver_symbol.h"
#include "ssa_mir_nodes.h"

namespace maple {

class VersionStTable;

class SSATab : public AnalysisResult {
  // represent the SSA table
 public:
  MIRModule &mirModule;
  MemPool *vers_mp;
  MapleAllocator vers_alloc;
  VersionStTable versionStTable;  // this uses special versmp because it will be freed earlier
  OriginalStTable originalStTable;
  StmtsSSAPart stmtsSSAPart;  // this uses special versmp because it will be freed earlier
  MapleVector<MapleSet<BBId> *> defBBs4Ost; // gives the set of BBs that has def for each original symbol
  bool wholeProgramScope;

  SSATab(MemPool *mp, MemPool *versmp, MIRModule *mod, uint32 bbcnt)
    : AnalysisResult(mp),
      mirModule(*mod),
      vers_mp(versmp),
      vers_alloc(versmp),
      versionStTable(versmp),
      originalStTable(mp, mod),
      stmtsSSAPart(versmp),
      defBBs4Ost(16, nullptr, vers_alloc.Adapter()),
      wholeProgramScope(false) {}

  ~SSATab() {}

  BaseNode *CreateSSAExpr(BaseNode *expr);
  void CreateSSAStmt(StmtNode *stmt, const BB *curbb, bool ignoreCallassignedDefs = false);
  bool HasDefBB(OStIdx oidx) {
    return oidx.idx < defBBs4Ost.size() && defBBs4Ost[oidx.idx] && !defBBs4Ost[oidx.idx]->empty();
  }
  void AddDefBB4Ost(OStIdx oidx, BBId bbid) {
    if (oidx.idx >= defBBs4Ost.size()) {
      defBBs4Ost.resize(oidx.idx + 16, nullptr);
    }
    if (defBBs4Ost[oidx.idx] == nullptr) {
      defBBs4Ost[oidx.idx] = vers_mp->New<MapleSet<BBId>>(vers_alloc.Adapter());
    }
    defBBs4Ost[oidx.idx]->insert(bbid);
  }

  // following are handles to methods in originalStTable
  OriginalSt *CreateSymbolOriginalSt(MIRSymbol *mirst, PUIdx pidx, FieldID fld) {
    return originalStTable.CreateSymbolOriginalSt(mirst, pidx, fld);
  }
  OriginalSt *FindOrCreateSymbolOriginalSt(MIRSymbol *mirst, PUIdx pidx, FieldID fld) {
    return originalStTable.FindOrCreateSymbolOriginalSt(mirst, pidx, fld);
  }
  OriginalSt *FindSymbolOriginalSt(MIRSymbol *mirst) {
    return originalStTable.FindSymbolOriginalSt(mirst);
  }
  OriginalSt *GetOriginalStFromid(OStIdx id) {
    return originalStTable.GetOriginalStFromid(id);
  }
  MIRSymbol *GetMIRSymbolFromid(OStIdx id) {
    return originalStTable.GetMIRSymbolFromid(id);
  }
};
}  // namespace maple
#endif  // end MAPLE_ME_INCLUDE_SSA_TAB_H
