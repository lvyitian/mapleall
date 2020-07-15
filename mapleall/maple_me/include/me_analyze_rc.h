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

#ifndef MAPLE_ME_INCLUDE_ME_ANALYZE_RC_H
#define MAPLE_ME_INCLUDE_ME_ANALYZE_RC_H
#include "me_function.h"
#include "me_phase.h"
#include "me_alias_class.h"
#include "me_irmap.h"

namespace maple {
class RCItem {
 public:
  OriginalSt *ost;
  bool noalias = false;
  bool nonlocal = false;    // need to do placement optimization if (!nonlocal)
  bool isformal = false;    // is an incoming formal parameter
  bool needsomeRC = false;  // true if any definition has rhs that needs RC
  MapleStack<MeExpr *> versionStack;
  MapleSet<BBId> occurBBs;  // set of BBs where the pointer occurs; only for local ref pointers

  RCItem(OriginalSt *o, MapleAllocator *alloc)
    : ost(o),
      versionStack(alloc->Adapter()),
      occurBBs(std::less<BBId>(), alloc->Adapter()) {}

  void Dump();
};

class AnalyzeRC {
 private:
  MeFunction *func;
  IRMap *irMap;
  SSATab *ssaTab;
  Dominance *dominance;
  AliasClass *aliasclass;
  MemPool *analyzercMp;
  MapleAllocator analyzercAllocator;
  bool globalWrite;

 public:
  MapleMap<OStIdx, RCItem *> rcitemsmap;
  bool earliest_decref_cleanup;
  bool skip_localrefvars;

  AnalyzeRC(MeFunction *f, Dominance *dom, AliasClass *ac, MemPool *mp)
    : func(f),
      irMap(f->irMap),
      ssaTab(f->meSSATab),
      dominance(dom),
      aliasclass(ac),
      analyzercMp(mp),
      analyzercAllocator(mp),
      globalWrite(false),
      rcitemsmap(std::less<OStIdx>(), analyzercAllocator.Adapter()),
      earliest_decref_cleanup(false),
      skip_localrefvars(false) {}

 private:
  RCItem *FindOrCreateRCItem(OriginalSt *ost);
  OriginalSt *GetOriginalSt(MeExpr *refLhs);
  bool NeedIncref(MeStmt *stmt);
  UnaryMeStmt *CreateIncrefZeroVersion(OriginalSt *ost);
  DassignMeStmt *CreateDassignInit(OStIdx ostidx);
  void InsertInitAtPUEntry(RCItem *rci);
  void CollectLocalRefPointerUses(MeExpr *x, BBId bbid);
  UnaryMeStmt *GenerateDecrefBeforeDead(OStIdx ostidx, BB *bb);

 public:
  void IdentifyRCStmts();
  void CreateCleanupIntrinsics();
  void RenameRefPtrs(BB *bb);
  void RenameDecrefsBeforeExit(BB *bb);
  void OptimizeRC();
  void InsertEntryIncrefs4Formals();
  void RemoveUnneededCleanups();
  std::string PhaseName() const {
    return "analyzerc";
  }
};

class MeDoAnalyzeRC : public MeFuncPhase {
 public:
  MeDoAnalyzeRC(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "analyzerc";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_ANALYZE_RC_H
