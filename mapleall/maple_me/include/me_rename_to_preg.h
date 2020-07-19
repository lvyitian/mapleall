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

#ifndef MAPLE_ME_INCLUDE_ME_RENAME_TO_PREG_H
#define MAPLE_ME_INCLUDE_ME_RENAME_TO_PREG_H
#include "alias_class.h"
#include "me_function.h"

namespace maple {

class SSARename2Preg {
 public:
  SSARename2Preg(MemPool *mp, MeFunction *f, MeIRMap *hmap, AliasClass *alias)
    : alloc(mp),
      func(f),
      meirmap(hmap),
      ssaTab(f->meSSATab),
      mirModule(&f->mirModule),
      aliasclass(alias),  // ostidx2reg_map(alloc.Adapter()),

      sym2reg_map(std::less<OStIdx>(), alloc.Adapter()),
      vstidx2reg_map(alloc.Adapter()),
      parm_used_vec(alloc.Adapter()),
      reg_formal_vec(alloc.Adapter()) {}

  void RunSelf();
  void PromoteEmptyFunction();

 private:
  MapleAllocator alloc;
  MeFunction *func;
  MeIRMap *meirmap;
  SSATab *ssaTab;
  MIRModule *mirModule;
  AliasClass *aliasclass;
  MapleMap<OStIdx, OriginalSt *> sym2reg_map;      // map var to reg in original symbol
  MapleUnorderedMap<int32, RegMeExpr *> vstidx2reg_map;  // maps the VarMeExpr's exprID to RegMeExpr
  MapleVector<bool> parm_used_vec;                       // if parameter is not used, it's false, otherwise true
  // if the parameter got promoted, the nth of func->mirFunc->_formal is the nth of reg_formal_vec, otherwise nullptr;
  MapleVector<RegMeExpr *> reg_formal_vec;

  AliasElem *GetAliasElem(const OriginalSt *ost) {
    if (ost->index.idx >= aliasclass->osym2Elem.size()) {
      return nullptr;
    }
    return aliasclass->FindAliasElem(ost);
  }

  void Rename2PregStmt(MeStmt *);
  void Rename2PregExpr(MeStmt *, MeExpr *);
  void Rename2PregLeafRHS(MeStmt *, VarMeExpr *);
  void Rename2PregLeafLHS(MeStmt *, VarMeExpr *);
  void Rename2PregPhi(BB *, MePhiNode *, MapleMap<OStIdx, MePhiNode *> &);
  void UpdateRegPhi(MePhiNode *, MePhiNode *, const RegMeExpr *, const VarMeExpr *);
  void Rename2PregCallReturn(MapleVector<MustDefMeNode> &);
  RegMeExpr *RenameVar(VarMeExpr *);
  void UpdateMirFunctionFormal();
  void SetupParmUsed(const VarMeExpr *);
  void Init();
  std::string PhaseName() const {
    return "rename2preg";
  }
};

class MeDoSSARename2Preg : public MeFuncPhase {
 public:
  MeDoSSARename2Preg(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "rename2preg";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_RENAME_TO_PREG_H
