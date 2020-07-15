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

#include "cg_func.h"
#include "mem_layout.h"
#include <iostream>

namespace maplebe {

using namespace maple;
using namespace std;

/*
   Go over all outgoing calls in the function body and get the maximum space
   needed for storing the actuals based on the actual parameters and the ABI.
   These are usually those arguments that cannot be passed
   through registers because a call passes more than 8 arguments, or
   they cannot be fit in a pair of registers.

   This assumes that all nesting of statements has been removed,
   so that all the statements are at only one block level.

   For passing of structures, the struct copy area is allocated at the top of
   the
 */
uint32 MemLayout::FindLargestActualArea(int32 &aggCopySize) {
  StmtNode *stmt = func->body->GetFirst();
  if (!stmt) {
    return 0;
  }
  uint32 maxActualSize = 0;
  uint32 maxParamStackSize = 0;  // Size of parameter stack requirement
  uint32 maxCopyStackSize = 0;   // Size of aggregate param stack copy requirement
  for (; stmt; stmt = stmt->GetNext()) {
    Opcode opcode = stmt->op;
    if (opcode < OP_call || opcode > OP_xintrinsiccallassigned) {
      continue;
    }
    if (opcode == OP_intrinsiccallwithtypeassigned || opcode == OP_intrinsiccallwithtype ||
        opcode == OP_intrinsiccallassigned || opcode == OP_intrinsiccall)
    // Some intrinsics, such as MPL_ATOMIC_EXCHANGE_PTR, are handled by CG,
    // and map to machine code sequences.  We ignore them because they are not
    // function calls.
    {
      continue;
    }
    // if the following check fails, most likely java has invoke-custom etc
    // that is not supported yet
    DCHECK((opcode == OP_call || opcode == OP_icall), "Not lowered to call or icall?");
    int32 copySize;
    uint32 n = ComputeStackSpaceRequirementForCall(stmt, copySize, opcode == OP_icall);
    if (n > maxParamStackSize) {
      maxParamStackSize = n;
    }
    if (copySize > maxCopyStackSize) {
      maxCopyStackSize = copySize;
    }
    if ((maxParamStackSize + maxCopyStackSize) > maxActualSize) {
      maxActualSize = maxParamStackSize + maxCopyStackSize;
    }
  }
  aggCopySize = maxCopyStackSize;
  maxActualSize = RoundUp(maxActualSize, SIZEOFPTR * 2);
  return maxActualSize;
}

AnalysisResult *CgDoLayoutSF::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (CGOptions::printFunction) {
    LogInfo::MapleLogger() << cgfunc->GetName() << "\n";
  }

  cgfunc->LayoutStackFrame();

  return nullptr;
}

}  // namespace maplebe
