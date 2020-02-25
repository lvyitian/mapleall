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

#include "bb.h"
#include "me_option.h"
#include "me_ssa.h"
#include <iostream>
#include "ssa_mir_nodes.h"
#include "ver_symbol.h"
#include "dominance.h"
#include "me_function.h"

/*
   This phase builds the SSA form of a function. Before this we have got the dominator tree
   and each bb's dominance frontiers. Then the algorithm follows this outline:

   Step 1: Inserting phi-node.
   With dominance frontiers, we can determine more
   precisely where phi-node might be needed. The basic idea is simple.
   A definition of x in block b forces a phi-node at every node in b's
   dominance frontiers. Since that phi-node is a new definition of x,
   it may, in turn, force the insertion of additional phi-node.

   Step 2: Renaming.
   Renames both definitions and uses of each symbol in
   a preorder walk over the dominator tree. In each block, we first
   rename the values defined by phi-node at the head of the block, then
   visit each stmt in the block: we rewrite each uses of a symbol with current
   SSA names(top of the stack which holds the current SSA version of the corresponding symbol),
   then it creates a new SSA name for the result of the stmt.
   This latter act makes the new name current. After all the stmts in the
   block have been rewritten, we rewrite the appropriate phi-node's
   parameters in each cfg successor of the block, using the current SSA names.
   Finally, it recurs on any children of the block in the dominator tree. When it
   returns from those recursive calls, we restores the stack of current SSA names to
   the state that existed before the current block was visited.
 */
using namespace std;

namespace maple {

void MeSSA::BuildSSA() {
  InsertPhiNode();

  InitRenameStack(&mirFunc->meSSATab->originalStTable, mirFunc->bbVec.size(), mirFunc->meSSATab->versionStTable);

  // recurse down dominator tree in pre-order traversal
  MapleSet<BBId> *children = &dom_->domChildren[mirFunc->commonEntryBB->id.idx];
  for (BBId child : *children) {
    RenameBB(mirFunc->bbVec[child.idx]);
  }
}

void MeSSA::InsertPhiNode() {
  for (uint32 i = 1; i < ssaTab->originalStTable.Size(); i++) {
    OriginalSt *ost = ssaTab->GetOriginalStFromid(OStIdx(i));
    if (ost->indirectLev < 0) {
      continue;
    }
    if (!ssaTab->HasDefBB(ost->index)) {
      continue;
    }
    if (ost->IsVolatile()) { // volatile variables will not have ssa form.
      continue;
    }
    std::set<BBId> phibbs;
    for (BBId bbid : *ssaTab->defBBs4Ost[ost->index.idx]) {
      phibbs.insert(dom_->iterDomFrontier[bbid.idx].begin(), dom_->iterDomFrontier[bbid.idx].end());
    }
    VersionSt *vst = ssaTab->versionStTable.GetZeroVersionSt(ost);
    for (BBId bbid : phibbs) {
      BB *phiBB = bbVec[bbid.idx];
      CHECK_FATAL(phiBB != nullptr, "MeSSA::InsertPhiNode: non-existent BB for definition");
      phiBB->InsertPhi(&mirFunc->alloc, vst);
    }
  }
}

bool MeSSA::VerifySSAOpnd(BaseNode *node) {
  Opcode op = node->op;
  uint32 vtableSize = mirFunc->meSSATab->versionStTable.Size();
  if (op == OP_dread || op == OP_addrof) {
    AddrofSSANode *addrofssanode = static_cast<AddrofSSANode *>(node);
    VersionSt *verSt = addrofssanode->ssaVar;
    ASSERT(verSt->index < vtableSize, "");
    return true;
  } else if (op == OP_regread) {
    RegreadSSANode *regnode = static_cast<RegreadSSANode *>(node);
    VersionSt *verSt = regnode->ssaVar;
    ASSERT(verSt->index < vtableSize, "");
    return true;
  }
  for (int32 i = 0; i < node->NumOpnds(); i++) {
    VerifySSAOpnd(node->Opnd(i));
  }
  return true;
}

bool MeSSA::VerifySSA() {
  VersionStTable *versionsttable = &mirFunc->meSSATab->versionStTable;
  uint32 vtableSize = mirFunc->meSSATab->versionStTable.Size();
  for (BB *bb : mirFunc->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    Opcode opcode;
    for (auto stmt : bb->stmtNodeList) {
      opcode = stmt->op;
      if (opcode == OP_dassign) {
        MayDefPartWithVersionSt *thessapart =
          static_cast<MayDefPartWithVersionSt *>(mirFunc->meSSATab->stmtsSSAPart.SsapartOf(stmt));
        VersionSt *verSt = thessapart->ssaVar;
        ASSERT(verSt->index < vtableSize, "");
      } else if (opcode == OP_regassign) {
        VersionSt *verSt = static_cast<VersionSt *>(mirFunc->meSSATab->stmtsSSAPart.SsapartOf(stmt));
        ASSERT(verSt->index < vtableSize, "");
      }
      for (int32 i = 0; i < stmt->NumOpnds(); i++) {
        ASSERT(VerifySSAOpnd(stmt->Opnd(i)), "");
      }
    }
  }
  return true;
}

AnalysisResult *MeDoSSA::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
  ASSERT(dom != nullptr, "dominance phase has problem");

  SSATab *ssaTab = static_cast<SSATab *>(m->GetAnalysisResult(MeFuncPhase_SSATAB, func));
  ASSERT(ssaTab != nullptr, "ssaTab phase has problem");

  MemPool *ssamp = mempoolctrler.NewMemPool(PhaseName().c_str());

  MeSSA ssa(func, func->meSSATab, dom, ssamp);
  ssa.BuildSSA();

  mempoolctrler.DeleteMemPool(ssamp);
  ssa.VerifySSA();

  if (DEBUGFUNC(func)) {
    ssaTab->versionStTable.Dump(&ssaTab->mirModule);
  }

  if (DEBUGFUNC(func)) {
    func->DumpFunction();
  }

  return nullptr;
}

}  // namespace maple
