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

#ifndef MAPLE_ME_INCLUDE_ME_STORE_PRE_H
#define MAPLE_ME_INCLUDE_ME_STORE_PRE_H
#include "me_ssu_pre.h"
#include "me_alias_class.h"

namespace maple {

class MeStorePre : public MeSSUPre {
 public:
  explicit MeStorePre(MeFunction *f, Dominance *dom, AliasClass *ac, MemPool *mp)
    : MeSSUPre(f, dom, mp, kStorePre),
      aliasclass(ac),
      cur_temp(nullptr),
      bb_cur_temp_map(ssuPreAlloc.Adapter())
  {}

 private:
  AliasClass *aliasclass;
  // step 6 codemotion
  RegMeExpr *cur_temp;                    // the preg for the RHS of inserted stores
  MapleUnorderedMap<BB *, RegMeExpr *> bb_cur_temp_map;  // map bb to cur_temp version

  // step 6 methods
  void CheckCreateCurTemp();
  RegMeExpr *EnsureRhsInCurTemp(BB *bb);
  void CodeMotion();
  // step 0 methods
  void CreateRealOcc(OStIdx oidx, MeStmt *mestmt);
  void CreateKillOcc(OStIdx oidx, BB *bb);
  void CreateSpreKillOccsThruAliasing(const OriginalSt *muost, BB *bb);
  void FindAndCreateSpreKillOccs(MeExpr *x, BB *bb);
  void CreateSpreKillOccsForAll(BB *bb);
  void BuildWorkListBB(BB *bb);
  void PerCandInit() {
    cur_temp = nullptr;
    bb_cur_temp_map.clear();
  }
  std::string PhaseName() const { return "storepre"; }
};

class MeDoStorePre : public MeFuncPhase {
 public:
  MeDoStorePre(MePhaseID id) : MeFuncPhase(id) {}
  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m) override;
  std::string PhaseName() const override { return "storepre"; }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_STORE_PRE_H
