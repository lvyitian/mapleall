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

#ifndef MAPLE_ME_INCLUDE_SSA_EPRE_H
#define MAPLE_ME_INCLUDE_SSA_EPRE_H
#include "ssa_pre.h"

namespace maple {
class SSAEPre : public SSAPre {
 public:
  explicit SSAEPre(IRMap *map, Dominance *dom, MemPool *mp, MemPool *mp2, PreKind kind, uint32 limit, bool includeref,
                   bool lhsivar)
    : SSAPre(map, dom, mp, mp2, kind, limit), epre_include_ref(includeref), enable_lhs_ivar(lhsivar) {}

 private:
  bool epre_include_ref;
  bool enable_lhs_ivar;

  void GenerateSaveLhsRealocc(MeRealOcc *realocc, ScalarMeExpr *regorvar);
  void GenerateSaveRealocc(MeRealOcc *realocc);
  void GenerateReloadRealocc(MeRealOcc *realocc);
  MeExpr *PhiOpndFromRes(MeRealOcc *realz, uint32 j);
  void ComputeVarAndDfPhis();
  void BuildWorkListExpr(MeStmt *mestmt, int32 seqstmt, MeExpr *, bool isrebuild, MeExpr *tempvar, bool isRootExpr);
  void BuildWorkListIvarLHSOcc(MeStmt *mestmt, int32 seqstmt, bool isrebuild, MeExpr *tempvar);
  void CollectVarForMeExpr(MeExpr *meexpr, std::vector<MeExpr *> &varvec);
  void CollectVarForCand(MeRealOcc *realocc, std::vector<MeExpr *> &varvec);

  bool LeafIsVolatile(MeExpr *x) {
    VarMeExpr *v = dynamic_cast<VarMeExpr *>(x);
    return v != nullptr && v->IsVolatile(irMap->ssaTab);
  }

  virtual bool CfgHasDoWhile() {
    return false;
  }
  bool AllVarsSameVersion(MeRealOcc *realocc1, MeRealOcc *realocc2);
  VarMeExpr *ResolveAllInjuringDefs(VarMeExpr *varx);
  RegMeExpr *ResolveAllInjuringDefs(RegMeExpr *regx);
  MeExpr *ResolveAllInjuringDefs(MeExpr *x) {
    if (!work_cand->isSRCand) {
      return x;
    }
    return (x->meOp == kMeOpVar) ?
        static_cast<MeExpr *>(ResolveAllInjuringDefs(static_cast<VarMeExpr *>(x))) :
        static_cast<MeExpr *>(ResolveAllInjuringDefs(static_cast<RegMeExpr *>(x)));
  }
  void SubstituteOpnd(MeExpr *x, MeExpr *oldopnd, MeExpr *newopnd);
  bool OpndInDefOcc(MeExpr *opnd, MeOccur *defocc, uint32 i);
  void SRSetNeedRepair(MeOccur *useocc, std::set<MeStmt *> *needRepairInjuringDefs);
  MeExpr *InsertRepairStmt(MeExpr *temp, int64 increAmt, MeStmt *injuringDef);
  MeExpr *SRRepairOpndInjuries(MeExpr *curopnd, MeOccur *defocc, int32 i,
      MeExpr *tempAtDef, std::set<MeStmt *> *needRepairInjuringDefs,
      std::set<MeStmt *> *repairedInjuringDefs);
  MeExpr *SRRepairInjuries(MeOccur *useocc,
      std::set<MeStmt *> *needRepairInjuringDefs,
      std::set<MeStmt *> *repairedInjuringDefs);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_SSA_EPRE_H
