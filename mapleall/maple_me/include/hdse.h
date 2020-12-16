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


#ifndef MAPLE_ME_INCLUDE_HDSE_H
#define MAPLE_ME_INCLUDE_HDSE_H
#include "bb.h"
#include "irmap.h"
#include "dominance.h"

namespace maple {

class MeIRMap;

class HDSE {
 protected:
  MIRModule *mirModule;
  SSATab *ssaTab;
  Dominance *pdom;
  IRMap *irMap;
  MapleAllocator dse_allocator;
  MapleVector<BB *> &bbVec;
  MapleVector<bool> bb_required;
  MapleVector<bool> expr_live;
  MapleForwardList<MeExpr *> worklist;

 public:
  bool hdsedebug;
  bool dsekeepref;

 protected:
  explicit HDSE(MIRModule *mod, SSATab *stab, Dominance *pdom, IRMap *map, MemPool *mp, MapleVector<BB *> &bbvec)
    : mirModule(mod),
      ssaTab(stab),
      pdom(pdom),
      irMap(map),
      dse_allocator(mp),
      bbVec(bbvec),
      bb_required(dse_allocator.Adapter()),
      expr_live(dse_allocator.Adapter()),
      worklist(dse_allocator.Adapter()),
      hdsedebug(false),
      dsekeepref(false) {}

 protected:
  virtual void UpdateStmt(BB *) = 0;
  void MarkExprNeeded(MeExpr *);
  void MarkPhiNeeded(MePhiNode *);
  void MarkMuListNeeded(MapleMap<OStIdx, ScalarMeExpr *> &);
  void MarkChiNodeNeeded(ChiMeNode *);
  void MarkBBNeeded(BB *);
  void MarkStmtNeeded(MeStmt *);
  bool ExprNonDeletable(MeExpr *x);
  void DseProcessBB(BB *);
  virtual bool IsLfo() { return false; }
  virtual void ProcessWhileInfos() {}
 public:
  void DseProcess();
  void Update();
 protected:
  bool IsExprNeeded(const MeExpr *meexpr) {
    return expr_live.at(meexpr->exprID);
  }

  void SetExprNeeded(const MeExpr *meexpr) {
    expr_live.at(meexpr->exprID) = true;
  }
};
}  // namespace maple
#endif
