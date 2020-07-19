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

#include "me_option.h"
#include "me_prop.h"
#include "me_dominance.h"

// This phase perform copy propagation optimization based on SSA representation.
// The propagation will not apply to ivar's of ref type unless the option
// --propiloadref is enabled.
//
// Copy propagation works by conducting a traversal over the program.  When it
// encounters a variable reference, it uses its SSA representation to look up
// its assigned value and try to do the substitution.

namespace maple {
AnalysisResult *MeDoMeProp::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
  ASSERT(dom != nullptr, "dominance phase has problem");

  MeIRMap *hmap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func));
  ASSERT(hmap != nullptr, "hssamap has problem");

  MemPool *propmp = mempoolctrler.NewMemPool(PhaseName().c_str());
  MeProp meprop(hmap, dom, propmp, MeOption::propbase, MeOption::propiloadref,
                MeOption::propglobalref, MeOption::propfinaliloadref, MeOption::propiloadrefnonparm,
                MeOption::propatphi);
  MIRFunction *mirfunction = func->mirFunc;
  meprop.TraversalBB(func->commonEntryBB);
  mempoolctrler.DeleteMemPool(propmp);
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============== After Copy Propagation  =============" << std::endl;
    hmap->Dump();
  }
  return nullptr;
}

}  // namespace maple
