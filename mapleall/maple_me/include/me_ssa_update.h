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

#ifndef MAPLE_ME_INCLUDE_ME_SSA_UPDATE_H
#define MAPLE_ME_INCLUDE_ME_SSA_UPDATE_H

#include "me_function.h"
#include "me_dominance.h"
#include "me_irmap.h"

namespace maple {

class SSAUpdate {
 public:
  SSAUpdate &operator=(const SSAUpdate &p) = default;
  SSAUpdate(const SSAUpdate &p) = default;
  SSAUpdate(MeFunction *f, SSATab *stab, Dominance *d, MapleMap<OStIdx, MapleSet<BBId> *> *cands)
    : func(f),
      irMap(f->irMap),
      ssaTab(stab),
      dom(d),
      ssaUpdateMp(mempoolctrler.NewMemPool("SSAUpdate")),
      ssaUpdateAlloc(ssaUpdateMp),
      updateCands(cands),
      renameStacks(std::less<OStIdx>(), ssaUpdateAlloc.Adapter()) {}

  ~SSAUpdate() {
    mempoolctrler.DeleteMemPool(ssaUpdateMp);
  }

  void Run();

 private:
  MeFunction *func;
  IRMap *irMap;
  SSATab *ssaTab;
  Dominance *dom;
  MemPool *ssaUpdateMp;
  MapleAllocator ssaUpdateAlloc;
  MapleMap<OStIdx, MapleSet<BBId> *> *updateCands;
  MapleMap<OStIdx, MapleStack<ScalarMeExpr *> *> renameStacks;

  void InsertPhis();
  void RenamePhi(BB *bb);
  MeExpr *RenameExpr(MeExpr *meexpr, bool &changed);
  void RenameStmts(BB *bb);
  void RenamePhiOpndsInSucc(BB *bb);
  void RenameBB(BB *bb);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_SSA_UPDATE_H
