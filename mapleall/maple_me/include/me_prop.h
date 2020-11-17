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

#ifndef MAPLE_ME_INCLUDE_ME_PROP_H
#define MAPLE_ME_INCLUDE_ME_PROP_H
#include "me_irmap.h"
#include "bb.h"
#include "me_phase.h"
#include "prop.h"

namespace maple {

class MeProp : public Prop {
 public:
  MeProp(MeIRMap *hmap, Dominance *dom, MemPool *mp, bool propbase, bool propiloadref, bool propglobalref,
         bool propfinaliloadref, bool propiloadrefNonparm, bool nopropatphi)
    : Prop(hmap, dom, mp, hmap->mirFunc->theCFG->bbVec.size(), propbase, propiloadref, propglobalref, propfinaliloadref,
           propiloadrefNonparm, nopropatphi),
      func(hmap->mirFunc) {}

 private:
  MeFunction *func;

  BB *GetBB(BBId id) {
    return func->theCFG->bbVec.at(id.idx);
  }

  bool InConstructorFunc() {
    return func->mirFunc->IsConstructor();
  }

  bool IsUservar(const MIRSymbol *st) {
    if (!st->IsLocal()) {
      return true;
    }
    MapleMap<GStrIdx, MIRAliasVars> &aliasVarMap = func->mirFunc->aliasVarMap;
    for (auto &als : aliasVarMap) {
      if(als.second.memPoolStrIdx == st->nameStrIdx) {
        return true;
      }
    }
    return false;
  }
};

class MeDoMeProp : public MeFuncPhase {
 public:
  explicit MeDoMeProp(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "hprop";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_PROP_H
