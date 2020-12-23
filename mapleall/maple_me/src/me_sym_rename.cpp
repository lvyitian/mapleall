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

#include "me_sym_rename.h"
#include "union_find.h"

// The symrename phase analyzes the live range of each local symbol, excluding
// virtual symbols.  If its live range has disconnected components, it will
// separate each disconnected live range component by assigning it a new
// OriginalSt.  The new OriginalSt still points to the same MIRSymbol, so that
// after re-emitting Maple IR, the number of local symbols is not increased.
// The symrename phase must be performed after copy propagation because copy
// propagation can form identity assignments which affect symrename's operation.

using namespace std;

namespace maple {

void SymRename::PerformSymRename() {
  UnionFind unionFind(mp, irMap->verst2MeExprTable.size());
  // conduct a pass over the program code to perform union-find
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr || bb == func->theCFG->commonEntryBB || bb == func->theCFG->commonExitBB) {
      continue;
    }
    // go thru all the phi's in the program to perform union-find
    for (std::pair<OStIdx, MePhiNode *> varphipair : bb->mePhiList) {
      ScalarMeExpr *lhs = varphipair.second->lhs;
      OriginalSt *ost = lhs->ost;
      if (!ost->IsSymbol() || !IsSymRenameCand(ost)) {
        continue;
      }
      uint32 lhsVstIdx = lhs->vstIdx;
      for (ScalarMeExpr *phiopnd : varphipair.second->opnds) {
        unionFind.Union(lhsVstIdx, phiopnd->vstIdx);
      }
    }
    // go thru the BB statements to perform union-find
    for (auto stmt : bb->meStmtList) {
      // union-find for identity and isIncDecStmt dassigns
      if (stmt->op == OP_dassign) {
        DassignMeStmt *dass = static_cast<DassignMeStmt *>(stmt);
        OriginalSt *ost = dass->lhs->ost;
        if (IsSymRenameCand(ost)) {
          if (dass->lhs->IsUseSameSymbol(dass->rhs)) {
            unionFind.Union(dass->lhs->vstIdx, static_cast<VarMeExpr *>(dass->rhs)->vstIdx);
          } else if (dass->isIncDecStmt) {
            OpMeExpr *oprhs = static_cast<OpMeExpr *>(dass->rhs);
            CHECK_FATAL(oprhs->op == OP_add || oprhs->op == OP_sub,
                   "PerformSymRename: dassigned marked isIncDecStmt has unexpected form");
            VarMeExpr *rhsvar = nullptr;
            if (oprhs->GetOpnd(0)->meOp == kMeOpVar) {
              rhsvar = static_cast<VarMeExpr *>(oprhs->GetOpnd(0));
            } else {
              rhsvar = static_cast<VarMeExpr *>(oprhs->GetOpnd(1));
            }
            CHECK_FATAL(rhsvar->ost == dass->lhs->ost,
                   "PerformSymRename: dassigned marked isIncDecStmt has unexpected rhs var");
            unionFind.Union(dass->lhs->vstIdx, rhsvar->vstIdx);
          }
        }
      }

      // union-find for chi nodes
      MapleMap<OStIdx, ChiMeNode *> *chilist = stmt->GetChiList();
      if (chilist == nullptr) {
        continue;
      }
      MapleMap<OStIdx, ChiMeNode *>::iterator chiit = chilist->begin();
      for (; chiit != chilist->end(); chiit++) {
        OriginalSt *ost = ssaTab->GetOriginalStFromid(chiit->first);
        if (!ost->IsSymbol() || !IsSymRenameCand(ost)) {
          continue;
        }
        ChiMeNode *chi = chiit->second;
        unionFind.Union(chi->lhs->vstIdx, chi->rhs->vstIdx);
      }
    }
  }
  // maps each root resulting from the union-find to a vector of its members;
  // the vector contains all the versions in each connected live range
  MapleMap<uint32, MapleVector<uint32>> root2MembersMap(alloc.Adapter());
  for (uint32 i = 0; i < irMap->verst2MeExprTable.size(); i++) {
    MeExpr *meexpr = irMap->verst2MeExprTable[i];
    if (!meexpr || meexpr->meOp != kMeOpVar)
      continue;
    VarMeExpr *varx = static_cast<VarMeExpr *>(meexpr);
    OriginalSt *ost = varx->ost;
    if (!IsSymRenameCand(ost)) {
      continue;
    }
    uint32 rootVstidx = unionFind.Root(i);
    MapleMap<uint32, MapleVector<uint32>>::iterator mpit = root2MembersMap.find(rootVstidx);
    if (mpit == root2MembersMap.end()) {
      root2MembersMap.insert(make_pair(rootVstidx, MapleVector<uint32>(1, i, alloc.Adapter())));
    } else {
      mpit->second.push_back(i);
    }
  }
  // origStClaimed monitors if the original OriginalSt has been used
  std::vector<bool> origStClaimed(ssaTab->originalStTable.Size(), false);
  // create new OriginalSt's
  for (MapleMap<uint32, MapleVector<uint32>>::iterator it = root2MembersMap.begin();
       it != root2MembersMap.end(); it++) {
    MapleVector<uint32> membersVec = it->second;
    // check if there is any member version that is zero version or defined in try block
    bool intryOrZeroVersion = false;
    bool isFormal = false;
    uint32 i = 0;
    do {
      uint32 vstIdx = membersVec[i];
      VarMeExpr *varx = static_cast<VarMeExpr *>(irMap->verst2MeExprTable[vstIdx]);
      if (varx->ost->isFormal) {
        isFormal = true;
        break;
      }
      if (varx->defBy == kDefByNo || varx->DefByBB()->InTryBlock()) {
        intryOrZeroVersion = true;
        origStClaimed[varx->ost->index.idx] = true;
        break;
      }
      i++;
    } while (i < membersVec.size());

    if (intryOrZeroVersion || isFormal) {
      continue;
    }

    OStIdx origOstIdx = static_cast<VarMeExpr *>(irMap->verst2MeExprTable[it->first])->ost->index;
    CHECK_FATAL(origOstIdx.idx < origStClaimed.size(), "origOstIdx out of range");
    if (!origStClaimed[origOstIdx.idx]) { // use the original OriginalSt
      origStClaimed[origOstIdx.idx] = true;
      continue;
    }

    // allocate a new OriginalSt for versions belonging to this live range
    OriginalSt *origOst = ssaTab->GetOriginalStFromid(origOstIdx);
    origOst->symRenamed = true;
    OriginalSt *newOst = ssaTab->CreateSymbolOriginalSt(origOst->GetMIRSymbol(), origOst->puIdx, origOst->fieldID);
    newOst->ignoreRC = origOst->ignoreRC;
    newOst->indexRenamedFrom = origOst->index;
    numNewOriginalSts++;
    // create the zero version for newOst
    newOst->zeroVersionIndex = irMap->verst2MeExprTable.size();
    irMap->verst2MeExprTable.push_back(nullptr);
    newOst->versionsIndex.push_back(newOst->zeroVersionIndex);

    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "symRenamed ";
      origOst->Dump();
      LogInfo::MapleLogger() << " to ";
      newOst->Dump();
      LogInfo::MapleLogger() << endl;
    }

    // rename its member versions
    for (i = 0; i < membersVec.size(); i++) {
      uint32 vstIdx = membersVec[i];
      VarMeExpr *varx = static_cast<VarMeExpr *>(irMap->verst2MeExprTable[vstIdx]);
      varx->ost = newOst;
      // if it is defined by phi or chi, use the map key to use the new ostidx
      if (varx->defBy == kDefByPhi) {
        MePhiNode *varphi = static_cast<MePhiNode *>(varx->def.defPhi);
        BB *phibb = varphi->defBB;
        phibb->mePhiList.erase(origOstIdx);  // delete old entry
        phibb->mePhiList[newOst->index] = varphi;  // insert new entry
      } else if (varx->defBy == kDefByChi) {
        ChiMeNode *chi = varx->def.defChi;
        MapleMap<OStIdx, ChiMeNode *> *chilist = chi->base->GetChiList();
        chilist->erase(origOstIdx);
        (*chilist)[newOst->index] = chi;
      }
    }
  }
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "symrename phase created " << numNewOriginalSts << " new OriginalSts\n";
  }
}

AnalysisResult *MeDoSymRename::Run(MeFunction *func, MeFuncResultMgr *m) {
  SSATab *ssaTab = static_cast<SSATab *>(m->GetAnalysisResult(MeFuncPhase_SSATAB, func, !MeOption::quiet));
  ASSERT(ssaTab != nullptr, "ssaTab phase has problem");
  MeIRMap *irMap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func, !MeOption::quiet));
  ASSERT(irMap != nullptr, "irmapbuild phase has problem");

  SymRename symrename(func);
  symrename.PerformSymRename();

  return nullptr;
}

}  // namespace maple
