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

#ifndef MAPLE_ME_INCLUDE_ME_DELEGATE_RC_H
#define MAPLE_ME_INCLUDE_ME_DELEGATE_RC_H
#include "me_function.h"
#include "me_phase.h"
#include "me_irmap.h"

namespace maple {
class DelegateRC {
 private:
  MeFunction *func;
  IRMap *irMap;
  SSATab *ssaTab;
  Dominance *dominance;
  MapleAllocator delegaterc_allocator;
  MapleVector<bool> verst_cantdelegate;               // true if it has appearance as phi opnd
  MapleVector<uint> verst_usecounts;                  // use counts of each SSA version
                                                      // or passed as parameter
  MapleMap<VarMeExpr *, RegMeExpr *> refvar2reg_map;  // map to the replacement preg
 public:
  MapleVector<bool> verst_derefedcopied;    // true if it is dereferenced or copied
  MapleVector<bool> verst_cantdecrefearly;  // true if it is unsafe to insert early decref in form B1 delegation

  DelegateRC(MeFunction *f, Dominance *dom, MemPool *tempmp)
    : func(f),
      irMap(func->irMap),
      ssaTab(f->meSSATab),
      dominance(dom),
      delegaterc_allocator(tempmp),
      verst_cantdelegate(irMap->verst2MeExprTable.size(), false, delegaterc_allocator.Adapter()),
      verst_usecounts(irMap->verst2MeExprTable.size(), 0, delegaterc_allocator.Adapter()),
      refvar2reg_map(delegaterc_allocator.Adapter()),
      verst_derefedcopied(irMap->verst2MeExprTable.size(), false, delegaterc_allocator.Adapter()),
      verst_cantdecrefearly(irMap->verst2MeExprTable.size(), false, delegaterc_allocator.Adapter()) {}

 private:
  void FindAndDecrUseCount(VarMeExpr *rhsvar, MeExpr *x, int32 &remainingUses);
  bool ContainAllTheUses(VarMeExpr *rhsvar, MeStmt *fromstmt, const MeStmt *tostmt);
  RegMeExpr *RHSTempDelegated(MeExpr *rhs, MeStmt *usestmt);
  bool FinalRefNoRC(MeExpr *x);

 public:
  void SetCantDelegate(MapleMap<OStIdx, MePhiNode *> &mevarphilist);
  void CollectUsesInfo(MeExpr *x);
  void DelegateRCTemp(MeStmt *stmt);
  bool CanOmitRC4LHSVar(MeStmt *stmt, bool &onlyWithDecref);
  void DelegateHandleNoRCStmt(MeStmt *stmt, bool addDecref);
  void RenameDelegatedRefVarUses(MeStmt *mestmt, MeExpr *meexpr);
  std::string PhaseName() const {
    return "delegaterc";
  }
};

class MeDoDelegateRC : public MeFuncPhase {
 public:
  MeDoDelegateRC(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "delegaterc";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_DELEGATE_RC_H
