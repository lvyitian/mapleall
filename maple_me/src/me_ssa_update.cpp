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

#include "me_ssa_update.h"

// Create or update HSSA representation for variables given by *updateCands;
// for each variable, the mapped bb set gives the bbs that have newly isnerted
// dassign's to the variable.
// If some assignments have been deleted, the current implementation does not
// delete useless phi's, and these useless phi's may end up hving identical
// phi operands.

namespace maple {

void SSAUpdate::InsertPhis() {
  MapleMap<OStIdx, MapleSet<BBId> *>::iterator it = updateCands->begin();
  MapleSet<BBId> dfSet(std::less<BBId>(), ssaUpdateAlloc.Adapter());
  for (; it != updateCands->end(); it++) {
    dfSet.clear();
    for (BBId bbid : *it->second) {
      dfSet.insert(dom->iterDomFrontier[bbid.idx].begin(), dom->iterDomFrontier[bbid.idx].end());
    }
    for (BBId bbid : dfSet) {
      // insert a phi node
      BB *bb = func->bbVec[bbid.idx];
      MapleMap<OStIdx, MePhiNode *>::iterator philistit = bb->mePhiList.find(it->first);
      if (philistit != bb->mePhiList.end()) {
        philistit->second->isLive = true;
        continue;
      }
      MePhiNode *phimenode = irMap->NewInPool<MePhiNode>();
      phimenode->defBB = bb;
      phimenode->opnds.resize(bb->pred.size());
      bb->mePhiList.insert(std::make_pair(it->first, phimenode));
    }
    // initialize its rename stack
    renameStacks[it->first] = ssaUpdateMp->New<MapleStack<VarMeExpr *>>(ssaUpdateAlloc.Adapter());
  }
}

void SSAUpdate::RenamePhi(BB *bb) {
  MapleMap<OStIdx, MapleStack<VarMeExpr *> *>::iterator it1 = renameStacks.begin();
  for (; it1 != renameStacks.end(); it1++) {
    MapleMap<OStIdx, MePhiNode *>::iterator it2 = bb->mePhiList.find(it1->first);
    if (it2 != bb->mePhiList.end()) {
      // if there is existing phi result node
      MePhiNode *phi = it2->second;
      phi->isLive = true;  // always make it live, for correctness
      if (phi->lhs == nullptr) {
        // create a new VarMeExpr defined by this phi
        VarMeExpr *newvar =
          irMap->New<VarMeExpr>(irMap->exprID++, ssaTab->GetOriginalStFromid(it2->first), irMap->verst2MeExprTable.size(), PTY_ref);
        newvar->ost->fieldID = 0;
        irMap->verst2MeExprTable.push_back(newvar);
        ASSERT(newvar->vstIdx == irMap->verst2MeExprTable.size() - 1, "RenamePhi: vstIdx wrongly initialized");
        phi->UpdateLhs(newvar);
        it1->second->push(newvar);  // push the stack
      } else {
        it1->second->push(static_cast<VarMeExpr*>(phi->lhs));  // push the stack
      }
    }
  }
}

// changed has been initialized to false by caller
MeExpr *SSAUpdate::RenameExpr(MeExpr *meexpr, bool &changed) {
  bool needrehash = false;
  switch (meexpr->meOp) {
    case kMeOpVar: {
      VarMeExpr *varexpr = static_cast<VarMeExpr *>(meexpr);
      MapleMap<OStIdx, MapleStack<VarMeExpr *> *>::iterator it1 = renameStacks.find(varexpr->ost->index);
      if (it1 == renameStacks.end()) {
        return meexpr;
      }
      MapleStack<VarMeExpr *> *renamestack = it1->second;
      VarMeExpr *curvar = renamestack->top();
      if (varexpr == curvar) {
        return meexpr;
      }
      changed = true;
      return curvar;
    }
    case kMeOpIvar: {
      IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(meexpr);
      MeExpr *newbase = RenameExpr(ivarmeexpr->base, needrehash);
      if (needrehash) {
        changed = true;
        IvarMeExpr newmeexpr(-1, ivarmeexpr->primType, ivarmeexpr->tyIdx, ivarmeexpr->fieldID);
        newmeexpr.base = newbase;
        newmeexpr.mu = ivarmeexpr->mu;
        return irMap->HashMeExpr(&newmeexpr);
      }
      return meexpr;
    }
    case kMeOpOp: {
      OpMeExpr *meopexpr = static_cast<OpMeExpr *>(meexpr);
      OpMeExpr newmeexpr(-1, meopexpr->op, meopexpr->primType, meopexpr->numOpnds);
      newmeexpr.SetOpnd(RenameExpr(meopexpr->GetOpnd(0), needrehash), 0);
      if (meopexpr->GetOpnd(1)) {
        newmeexpr.SetOpnd(RenameExpr(meopexpr->GetOpnd(1), needrehash), 1);
        if (meopexpr->GetOpnd(2)) {
          newmeexpr.SetOpnd(RenameExpr(meopexpr->GetOpnd(2), needrehash), 2);
        }
      }
      if (needrehash) {
        changed = true;
        newmeexpr.opndType = meopexpr->opndType;
        newmeexpr.bitsOffset = meopexpr->bitsOffset;
        newmeexpr.bitsSize = meopexpr->bitsSize;
        newmeexpr.tyIdx = meopexpr->tyIdx;
        newmeexpr.fieldID = meopexpr->fieldID;
        return irMap->HashMeExpr(&newmeexpr);
      }
      return meexpr;
    }
    case kMeOpNary: {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(meexpr);
      NaryMeExpr newmeexpr(&irMap->irMapAlloc, -1, meexpr->op, meexpr->primType, meexpr->numOpnds,
          narymeexpr->tyIdx, narymeexpr->intrinsic, narymeexpr->boundCheck);
      for (uint32 i = 0; i < narymeexpr->numOpnds; i++) {
        newmeexpr.PushOpnd(RenameExpr(narymeexpr->GetOpnd(i), needrehash));
      }
      if (needrehash) {
        changed = true;
        return irMap->HashMeExpr(&newmeexpr);
      }
      return meexpr;
    }
    default:
      return meexpr;
  }
}

void SSAUpdate::RenameStmts(BB *bb) {
  for (auto stmt : bb->meStmtList) {
    // rename the expressions
    bool changed = false;
    for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++)
      stmt->SetMeStmtOpnd(i, RenameExpr(stmt->GetMeStmtOpnd(i), changed /* dummy */));

    // process the LHS
    VarMeExpr *lhsVar;
    if (stmt->op == OP_dassign || stmt->op == OP_maydassign) {
      lhsVar = stmt->GetVarLhs();
    } else if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
      MapleVector<MustDefMeNode> *mustdeflist = stmt->GetMustDefList();
      if (mustdeflist->size() == 0) {
        continue;
      }
      if (mustdeflist->front().lhs->meOp != kMeOpVar) {
        continue;
      }
      lhsVar = static_cast<VarMeExpr *>(mustdeflist->front().lhs);
    } else {
      continue;
    }
    MapleMap<OStIdx, MapleStack<VarMeExpr *> *>::iterator it = renameStacks.find(lhsVar->ost->index);
    if (it == renameStacks.end()) {
      continue;
    }
    MapleStack<VarMeExpr *> *renamestack = it->second;
    renamestack->push(lhsVar);
  }
}

void SSAUpdate::RenamePhiOpndsInSucc(BB *bb) {
  for (BB *succ : bb->succ) {
    // find index of bb in succ_bb->pred[]
    uint32 index = 0;
    while (index < succ->pred.size()) {
      if (succ->pred[index] == bb) {
        break;
      }
      index++;
    }
    ASSERT(index < succ->pred.size(), "RenamePhiOpndsinSucc: cannot find corresponding pred");
    MapleMap<OStIdx, MapleStack<VarMeExpr *> *>::iterator it1 = renameStacks.begin();
    for (; it1 != renameStacks.end(); it1++) {
      MapleMap<OStIdx, MePhiNode *>::iterator it2 = succ->mePhiList.find(it1->first);
      if (it2 == succ->mePhiList.end()) {
        continue;
      }
      MePhiNode *phi = it2->second;
      MapleStack<VarMeExpr *> *renamestack = it1->second;
      VarMeExpr *curvar = renamestack->top();
      if (phi->opnds[index] != curvar) {
        phi->opnds[index] = curvar;
      }
    }
  }
}

void SSAUpdate::RenameBB(BB *bb) {
  // for recording stack height on entering this BB, to pop back to same height
  // when backing up the dominator tree
  std::map<OStIdx, uint32> origStackSize((std::less<OStIdx>()));
  MapleMap<OStIdx, MapleStack<VarMeExpr *> *>::iterator it = renameStacks.begin();
  for (; it != renameStacks.end(); it++) {
    origStackSize[it->first] = it->second->size();
  }

  RenamePhi(bb);

  RenameStmts(bb);

  RenamePhiOpndsInSucc(bb);

  // recurse down dominator tree in pre-order traversal
  MapleSet<BBId> *children = &dom->domChildren[bb->id.idx];
  for (BBId child : *children) {
    RenameBB(func->bbVec[child.idx]);
  }

  // pop stacks back to where they were at entry to this BB
  it = renameStacks.begin();
  for (; it != renameStacks.end(); it++) {
    while (it->second->size() > origStackSize[it->first]) {
      it->second->pop();
    }
  }
}

void SSAUpdate::Run() {
  InsertPhis();

  // push zero-version varmeexpr nodes to rename stacks
  MapleMap<OStIdx, MapleStack<VarMeExpr *> *>::iterator it = renameStacks.begin();
  for (; it != renameStacks.end(); it++) {
    OriginalSt *ost = ssaTab->GetSymbolOriginalStFromid(it->first);
    VarMeExpr *zeroversvar = irMap->GetOrCreateZeroVersionVarMeExpr(ost);
    MapleStack<VarMeExpr *> *renamestack = it->second;
    renamestack->push(zeroversvar);
  }

  // recurse down dominator tree in pre-order traversal
  MapleSet<BBId> *children = &dom->domChildren[func->commonEntryBB->id.idx];
  for (BBId child : *children) {
    RenameBB(func->bbVec[child.idx]);
  }
}

}  // namespace maple
