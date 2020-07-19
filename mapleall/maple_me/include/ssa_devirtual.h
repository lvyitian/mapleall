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

#ifndef MAPLE_ME_INCLUDE_ME_SSA_DEVIRTUAL_H
#define MAPLE_ME_INCLUDE_ME_SSA_DEVIRTUAL_H
#include "me_ir.h"
#include "me_phase.h"
#include "me_irmap.h"
#include "dominance.h"
#include "class_hierarchy.h"
#include "clone.h"

namespace maple {
class SSADevirtual {
 public:
  static bool debug;

  SSADevirtual(MemPool *mp, MIRModule *mod, IRMap *irMap, KlassHierarchy *kh, Dominance *dom, uint32 bbvecsize,
               Clone *clone)
    : devirtual_alloc(mp),
      mod_(mod),
      irmap_(irMap),
      kh_(kh),
      dom_(dom),
      bb_visited(bbvecsize, false, devirtual_alloc.Adapter()),
      clone(clone),
      retty_(kNotSeen),
      inferred_rettyidx_(0),
      total_virtualcalls(0),
      opted_virtualcalls(0),
      total_interfacecalls(0),
      opted_interfacecalls(0),
      nullcheck_count(0) {}

  virtual ~SSADevirtual() {}

  void Perform(BB *entryBb);
  std::string PhaseName() const {
    return "ssadevirt";
  }

 protected:
  virtual MIRFunction *GetMIRFunction() {
    return nullptr;
  }

  virtual BB *GetBB(BBId id) = 0;
  void TraversalBB(BB *);
  void TraversalMeStmt(MeStmt *stmt);
  void VisitVarPhiNode(MePhiNode *) const;
  void VisitMeExpr(MeExpr *);
  void PropVarInferredType(VarMeExpr *);
  void PropIvarInferredType(IvarMeExpr *);
  void ReturnTyidxInferring(RetMeStmt *);
  bool NeedNullCheck(MeExpr *) const;
  void InsertNullCheck(CallMeStmt *, MeExpr *);
  bool DevirtualizeCall(CallMeStmt *);
  void SSADevirtualize(CallNode *stmt);
  void ReplaceCall(CallMeStmt *, MIRFunction *);
  TyIdx GetInferredTyidx(MeExpr *expr);

 private:
  MapleAllocator devirtual_alloc;
  MIRModule *mod_;
  IRMap *irmap_;
  KlassHierarchy *kh_;
  Dominance *dom_;
  MapleVector<bool> bb_visited;  // needed because dominator tree is a DAG in wpo
  Clone *clone;
  enum TagRetTyidx { kNotSeen, kSeen, FAILED } retty_;
  TyIdx inferred_rettyidx_;
  unsigned int total_virtualcalls;
  unsigned int opted_virtualcalls;
  unsigned int total_interfacecalls;
  unsigned int opted_interfacecalls;
  unsigned int nullcheck_count;
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_MESSA_DEVIRTUAL_H
