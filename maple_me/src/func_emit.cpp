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

#include "func_emit.h"
#include "mir_function.h"
namespace maple {
void FuncEmit::EmitLabelForBB(MIRFunction *func, BB *bb) {
  ASSERT(bb->bbLabel != 0, "Should have a label");
  // create labelnode
  LabelNode *label = func->codeMemPool->New<LabelNode>();
  label->labelIdx = bb->bbLabel;

  if (bb->IsEmpty()) {
    bb->SetFirst(label);
    bb->SetLast(label);
  } else {
    // Insert label before the first non-comment statement of bb
    StmtNode *first = bb->stmtNodeList.first;
    StmtNode *firstPrev = nullptr;
    while (first && first->op == OP_comment) {
      firstPrev = first;
      first = first->GetNext();
    }
    // "first" points to the first non-comment statement, or nullptr
    if (first) {
      label->InsertBefore(first);
      if (first == bb->stmtNodeList.first) {
        bb->SetFirst(label);
      }
    } else {
      label->InsertAfter(firstPrev);
      if (firstPrev == bb->stmtNodeList.last) {
        bb->SetLast(label);
      }
    }
  }
}

static void ConvertMaydassign(BB *bb) {
  for (auto stmt : bb->stmtNodeList) {
    if (stmt->op == OP_maydassign) {
      stmt->op = OP_dassign;
    }
  }
}

// Inserting BBs in bblist into func's body.
void FuncEmit::EmitBeforeHSSA(MIRFunction *func, const MapleVector<BB *> *bblist) {
  StmtNode *lastStmt = nullptr; /* last stmt of previous bb */
  func->body->ResetBlock();  /* reset body first stmt */
  for (BB *bb : *bblist) {
    if (bb == nullptr) {
      continue;
    }
    ConvertMaydassign(bb);
    if (bb->bbLabel != 0) {
      EmitLabelForBB(func, bb);
    }
    if (!bb->IsEmpty()) {
      if (func->body->GetFirst() == nullptr) {
        func->body->SetFirst(bb->stmtNodeList.first);
      }
      if (lastStmt) {
        bb->stmtNodeList.first->InsertAfter(lastStmt);
      }
      lastStmt = bb->stmtNodeList.last;
    }
    if (bb->AddBackEndTry()) {
      /* generate op_endtry andd added to next, it could be in an empty bb. */
      StmtNode *endtry = func->codeMemPool->New<StmtNode>(OP_endtry);
      endtry->InsertAfter(lastStmt);
      lastStmt = endtry;
    }
  }
  func->body->SetLast(lastStmt);
  return;
}

}  // namespace maple
