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

#include <iostream>
#include <algorithm>
#include "retype.h"

using namespace std;

namespace maple {
void Retype::ReplaceRetypeExpr(BaseNode *expr) {
  if (expr->NumOpnds() == 0)

  {
    return;
  }

  for (int32 i = 0; i < expr->NumOpnds(); i++) {
    BaseNode *opnd = expr->Opnd(i);

    if (opnd->op == OP_retype) {
      opnd->SetOpnd(opnd->Opnd(0), i);

      continue;
    }

    ReplaceRetypeExpr(opnd);
  }
}

void Retype::Retypestmt(MIRFunction *func) {
  if (func->IsEmpty())

  {
    return;
  }

  for (StmtNode *stmt = func->body->GetFirst(); stmt; stmt = stmt->GetNext()) {
    if (stmt->op == OP_comment)

    {
      continue;
    }

    for (int32 i = 0; i < stmt->NumOpnds(); i++) {
      BaseNode *opnd = stmt->Opnd(i);

      if (opnd->op == OP_retype) {
        stmt->SetOpnd(opnd->Opnd(0), i);

        continue;

      }

      else {
        ReplaceRetypeExpr(opnd);
      }
    }
  }
}

void Retype::DoRetype() {
  for (MIRFunction *func : mirModule->functionList) {
    if (func->IsEmpty())

    {
      continue;
    }

    Retypestmt(func);
  }
}

}  // namespace maple
