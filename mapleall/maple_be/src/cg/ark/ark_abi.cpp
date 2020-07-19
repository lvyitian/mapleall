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

#include "ark_abi.h"
#include "ark_cg_func.h"
#include "be_common.h"
#include "cg_assert.h"

namespace maplebe {

using namespace maple;

// LocateNextParm should be called with each parameter in the parameter list
// starting from the beginning, one call per parameter in sequence; it returns
// the information on how each parameter is passed in ploc
void ParmLocator::LocateNextParm(MIRType *ty, PLocInfo &ploc) {
  int typesize = _be.type_size_table.at(ty->tyIdx.GetIdx());
  int typealign = _be.type_align_table[ty->tyIdx.GetIdx()];
  ploc.memoffset = NSAA;
  // Rule C.12 states that we do round NSAA up before we use its value
  // according to the alignment requirement of the argument being processed.
  // We do the rounding up at the end of LocateNextParm(),
  // so we want to make sure our rounding up is correct.
  CG_ASSERT((NSAA & (std::max(typealign, 8) - 1)) == 0, "C.12 alignment requirement is violated");
  ploc.memsize = typesize;
  _parm_num++;

  // being passed in memory
  NSAA = ploc.memoffset + typesize /*+ rightpad*/;
  return;
}

}  // namespace maplebe
