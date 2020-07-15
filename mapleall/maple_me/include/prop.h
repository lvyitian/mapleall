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

#ifndef MAPLE_ME_INCLUDE_PROP_H
#define MAPLE_ME_INCLUDE_PROP_H
#include "me_ir.h"
#include "ver_symbol.h"
#include "bb.h"
#include "me_function.h"

namespace maple {

class IRMap;
class Dominance;

class Prop {
 public:
  IRMap *irMap;
  SSATab *ssaTab;
  MIRModule *mirModule;

  Prop(IRMap *, Dominance *, MemPool *, uint32 bbvecsize, bool propbase, bool propiloadref, bool propglobalref,
       bool propfinaliloadref, bool propiloadrefNonparm, bool propatphi);

  void TraversalBB(BB *);

 protected:
  Dominance *dominance;

  virtual BB *GetBB(BBId id) {
    return nullptr;
  }

  virtual MeFunction *GetFunc() {
    return nullptr;
  }

  virtual void UpdateCurFunction(BB *bb) {}

  virtual bool LocalToDifferentPU(StIdx stIdx, BB *bb) {
    return false;
  }

  virtual bool InConstructorFunc() {
    return false;
  }

 private:
  MapleAllocator prop_map_alloc;
  MapleVector<MapleStack<MeExpr *> *> vst_live_stack_vec;
  MapleVector<bool> bb_visited;  // needed because dominator tree is a DAG in wpo
  BB *curbb;                     // gives the bb of the traversal
  bool propagate_base;
  bool propagate_iload_ref;
  bool propagate_global_ref;
  bool propagate_final_iload_ref;
  bool propagate_iload_ref_nonparm;
  bool propagate_at_phi;

  void CollectSubVarMeExpr(MeExpr *, std::vector<MeExpr *> &);
  bool IsVersionConsistent(const std::vector<MeExpr *> &vervec,
                           const MapleVector<MapleStack<MeExpr *> *> &vstLiveStack) const;
  bool IvarIsFinalField(const IvarMeExpr *ivarmeexpr);
  bool Propagatable(MeExpr *x, BB *frombb, bool atParm);
  MeExpr *CheckTruncation(MeExpr *lhs, MeExpr *rhs);
  MeExpr *PropReg(RegMeExpr *regmeexpr, bool atParm);
  void TraversalMeStmt(MeStmt *);

 public:
  MeExpr *SimplifyMeExpr(OpMeExpr *);
  MeExpr *PropMeExpr(MeExpr *meexpr, bool &isproped, bool atParm);
  MeExpr *PropVar(VarMeExpr *varmeexpr, bool atParm, bool checkPhi);
  MeExpr *PropIvar(IvarMeExpr *ivarmeexpr);
  void PropUpdateDef(MeExpr *);
  void PropUpdateChiListDef(const MapleMap<OStIdx, ChiMeNode *> &);
  void PropUpdateMustDefList(MeStmt *);
  uint32 GetVstLiveStackVecSize() const {
    return vst_live_stack_vec.size();
  }
  MapleStack<MeExpr *> * GetVstLiveStackVec(uint32 i) {
    return vst_live_stack_vec[i];
  }
  void SetCurBB(BB *bb) {
    curbb = bb;
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_PROP_H
