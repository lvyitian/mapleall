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

#include "mempool_allocator.h"
#include "bb.h"
#include "ver_symbol.h"
#include "me_ssa.h"
#include "me_ir.h"

namespace maple {

std::string BB::StrAttribute() const {
  switch (kind) {
    case kBBUnknown:
      return "unknown";
    case kBBCondGoto:
      return "condgoto";
    case kBBGoto:
      return "goto";
    case kBBFallthru:
      return "fallthru";
    case kBBReturn:
      return "return";
    case kBBAfterGosub:
      return "aftergosub";
    case kBBSwitch:
      return "switch";
    case kBBInvalid:
      return "invalid";
    default:
      ASSERT(false, "NYI");
  }
  return "NYI";
}

void BB::DumpBBAttribute(MIRModule *mod) {
  if (isEntry) {
    LogInfo::MapleLogger() << " Entry ";
  }
  if (isExit) {
    LogInfo::MapleLogger() << " Exit ";
  }
  if (isTry) {
    LogInfo::MapleLogger() << " Try ";
  }
  if (isTryEnd) {
    LogInfo::MapleLogger() << " Tryend ";
  }
  if (isJSCatch) {
    LogInfo::MapleLogger() << " JSCatch ";
  }
  if (isJSFinally) {
    LogInfo::MapleLogger() << " JSFinally ";
  }
  if (isJavaFinally) {
    LogInfo::MapleLogger() << " Catch::Finally ";
  } else if (isCatch) {
    LogInfo::MapleLogger() << " Catch ";
  }
  if (isInLoop) {
    LogInfo::MapleLogger() << " InLoop ";
  }
}

void BB::DumpHeader(MIRModule *mod) {
  LogInfo::MapleLogger() << "============BB id:" << id.idx << " " << StrAttribute() << " [";
  DumpBBAttribute(mod);
  LogInfo::MapleLogger() << "]===============\n";
  LogInfo::MapleLogger() << "preds: ";
  for (MapleVector<BB *>::iterator it = pred.begin(); it != pred.end(); it++) {
    LogInfo::MapleLogger() << (*it)->id.idx << " ";
  }
  LogInfo::MapleLogger() << "\nsuccs: ";
  for (MapleVector<BB *>::iterator it = succ.begin(); it != succ.end(); it++) {
    LogInfo::MapleLogger() << (*it)->id.idx << " ";
  }
  LogInfo::MapleLogger() << "\n";
  if (bbLabel != 0) {
    static LabelNode lblnode;
    lblnode.labelIdx = bbLabel;
    lblnode.Dump(mod, 0);
    LogInfo::MapleLogger() << "\n";
  }
}

void BB::Dump(MIRModule *mod) {
  DumpHeader(mod);
  DumpPhi(mod);
  for (auto stmt : stmtNodeList) {
    stmt->Dump(mod, 1);
  }
}

MeStmt *BB::GetCondBrStmt() {
  CHECK_FATAL(kind == kBBCondGoto, "must be cond goto");
  MeStmt *mestmt = meStmtList.last;
  if (mestmt->IsCondBr()) {
    return mestmt;
  }
  CHECK_FATAL(mestmt->op == OP_comment, "NYI");
  return nullptr;
}

// if the bb contains only one stmt other than comment, return that stmt
// otherwise return nullptr
MeStmt *BB::GetTheOnlyMeStmt() {
  MeStmt *mestmt = meStmtList.first;
  MeStmt *lastMeStmt = meStmtList.last;
  while (mestmt != lastMeStmt && mestmt->op == OP_comment) {
    mestmt = mestmt->next;
  }
  if (mestmt == lastMeStmt) {
    return mestmt;
  }
  MeStmt *nextmestmt = mestmt->next;
  while (nextmestmt != lastMeStmt && nextmestmt->op == OP_comment) {
    nextmestmt = nextmestmt->next;
  }
  if (nextmestmt == lastMeStmt && nextmestmt->op == OP_comment) {
    return mestmt;
  }
  return nullptr;
}

// if the bb contains only one stmt other than comment and goto, return that stmt
// otherwise return nullptr
MeStmt *BB::GetTheOnlyMeStmtWithGoto() {
  MeStmt *arraystmt[2];
  uint32 index = 0;
  MeStmt *mestmt = meStmtList.first;
  while (mestmt != meStmtList.last) {
    if (mestmt->op != OP_comment) {
      arraystmt[index++] = mestmt;
    }
    if (index >= 2) {
      return nullptr;
    }
    mestmt = mestmt->next;
  }
  if (mestmt->op != OP_comment) {
    arraystmt[index++] = mestmt;
  }
  if (index == 2 && arraystmt[0]->op != OP_comment && arraystmt[1]->op == OP_goto) {
    return arraystmt[0];
  }
  return nullptr;
}

void BB::DumpPhi(const MIRModule *mod) {
  MapleMap<OriginalSt *, PhiNode>::iterator phiIt;
  for (phiIt = phiList.begin(); phiIt != phiList.end(); phiIt++) {
    (*phiIt).second.Dump(mod);
  }
}

PhiNode *BB::PhiofVerstInserted(VersionSt *vsym) {
  MapleMap<OriginalSt *, PhiNode>::iterator phiit = phiList.find(vsym->ost);
  return (phiit != phiList.end()) ? &(*phiit).second : nullptr;
}

void BB::InsertPhi(MapleAllocator *alloc, VersionSt *vsym) {
  MapleVector<BB *>::iterator prevIt;
  PhiNode phinode(alloc, vsym);

  for (prevIt = pred.begin(); prevIt != pred.end(); prevIt++) {
    phinode.phiOpnds.push_back(vsym);
  }

  phiList.insert(std::make_pair(vsym->ost, phinode));
}

bool BB::IsInList(MapleVector<BB *> &bblist) const {
  for (MapleVector<BB *>::iterator it = bblist.begin(); it != bblist.end(); it++) {
    if (*it == this) {
      return true;
    }
  }
  return false;
}

// remove the given bb from the BB vector bvec; nothing done if not found
int BB::RemoveBBFromVector(MapleVector<BB *> &bvec) {
  uint32 i = 0;
  while (i < bvec.size()) {
    if (bvec[i] == this) {
      break;
    }
    i++;
  }
  if (i == bvec.size()) {
    // bb not in the vector
    return -1;
  }
  int retval = i;
  // shift remaining elements down
  while (i < bvec.size() - 1) {
    bvec[i] = bvec[i + 1];
    i++;
  }
  bvec.pop_back();
  return retval;
}

void BB::RemoveBBFromPred(BB *bb) {
  int index = bb->RemoveBBFromVector(pred);
  for (auto phiIt = phiList.begin(); phiIt != phiList.end(); phiIt++) {
    MapleVector<VersionSt *> *opnds = &(*phiIt).second.phiOpnds;
    opnds->erase(opnds->begin() + index);
  }
  for (auto phiIt = mePhiList.begin(); phiIt != mePhiList.end(); phiIt++) {
    MapleVector<ScalarMeExpr *> *opnds = &(*phiIt).second->opnds;
    opnds->erase(opnds->begin() + index);
  }
}

void BB::RemoveBBFromSucc(BB *bb) {
  bb->RemoveBBFromVector(succ);
}

/* add stmtnode to bb and update first_stmt_ and last_stmt_ */
void BB::AddStmtNode(StmtNode *stmt) {
  ASSERT(stmt != nullptr, "");
  stmtNodeList.push_back(stmt);
}

/* add stmtnode at beginning of bb and update first_stmt_ and last_stmt_ */
void BB::PrependStmtNode(StmtNode *stmt) {
  stmtNodeList.push_front(stmt);
}

void BB::PrependMeStmt(MeStmt *mestmt) {
  meStmtList.push_front(mestmt);
  mestmt->bb = this;
}

// warning: if stmt is not in the bb, this will cause undefined behavior
void BB::RemoveStmtNode(StmtNode *stmt) {
  ASSERT(stmt != nullptr, "");
  stmtNodeList.erase(StmtNodes::iterator(stmt));
}

void BB::InsertStmtBefore(StmtNode *stmt, StmtNode *newStmt) {
  CHECK_FATAL(newStmt != nullptr, "null ptr check");
  CHECK_FATAL(stmt != nullptr, "null ptr check");
  stmtNodeList.insert(StmtNodes::iterator(stmt), newStmt);
}

void BB::ReplaceStmt(StmtNode *stmt, StmtNode *newStmt) {
  InsertStmtBefore(stmt, newStmt);
  RemoveStmtNode(stmt);
}

/* delete last_stmt_ and update */
void BB::RemoveLastStmt() {
  stmtNodeList.pop_back();
}

/* replace pred with newbb to keep position and
 * add this as succ of newPred */
void BB::ReplacePred(const BB *old, BB *newPred) {
  for (uint32 i = 0; i < pred.size(); i++) {
    if (pred[i] == old) {
      pred[i] = newPred;
      newPred->succ.push_back(this);
      break;
    }
  }
  return;
}

/* replace succ in current position with newSucc
 * and add this as pred of newSucc */
void BB::ReplaceSucc(const BB *old, BB *newSucc) {
  for (uint32 i = 0; i < succ.size(); i++) {
    if (succ[i] == old) {
      succ[i] = newSucc;
      newSucc->pred.push_back(this);
      break;
    }
  }
  return;
}

/* this is commonEntryBB, so newSucc's pred is left as is */
void BB::ReplaceSuccOfCommonEntryBB(const BB *old, BB *newSucc) {
  for (uint32 i = 0; i < succ.size(); i++) {
    if (succ[i] == old) {
      succ[i] = newSucc;
      break;
    }
  }
  return;
}

void BB::FindReachableBBs(std::vector<bool> &visitedBBs) {
  if (visitedBBs[id.idx]) {
    return;
  }
  visitedBBs[id.idx] = true;
  for (MapleVector<BB *>::iterator it = succ.begin(); it != succ.end(); ++it) {
    (*it)->FindReachableBBs(visitedBBs);
  }
}

void BB::FindWillExitBBs(std::vector<bool> &visitedBBs) {
  if (visitedBBs[id.idx]) {
    return;
  }
  visitedBBs[id.idx] = true;
  for (MapleVector<BB *>::iterator it = pred.begin(); it != pred.end(); ++it) {
    (*it)->FindWillExitBBs(visitedBBs);
  }
}

void BB::SetFirstMe(MeStmt *stmt) {
  meStmtList.update_front(stmt);
}

void BB::SetLastMe(MeStmt *stmt) {
  meStmtList.update_back(stmt);
}

void BB::RemoveMeStmt(MeStmt *meStmt) {
  CHECK_FATAL(meStmt != nullptr, "null ptr check");
  meStmtList.erase(meStmt);
}

void BB::AddMeStmtFirst(MeStmt *meStmt) {
  CHECK_FATAL(meStmt != nullptr, "null ptr check");
  meStmtList.push_front(meStmt);
  meStmt->SetBB(this);
}

void BB::AddMeStmtLast(MeStmt *meStmt) {
  CHECK_FATAL(meStmt != nullptr, "null ptr check");
  meStmtList.push_back(meStmt);
  meStmt->SetBB(this);
}

void BB::InsertMeStmtBefore(MeStmt *meStmt, MeStmt *inStmt) {
  CHECK_FATAL(meStmt != nullptr, "null ptr check");
  CHECK_FATAL(inStmt != nullptr, "null ptr check");
  meStmtList.insert(meStmt, inStmt);
  inStmt->SetBB(this);
}

void BB::InsertMeStmtAfter(MeStmt *meStmt, MeStmt *inStmt) {
  meStmtList.insertAfter(meStmt, inStmt);
  inStmt->SetBB(this);
}

// insert instmt before br to the last of bb
void BB::InsertMeStmtLastBr(MeStmt *instmt) {
  if (IsMeStmtEmpty()) {
    AddMeStmtLast(instmt);
  } else {
    MeStmt *brstmt = meStmtList.last;
    Opcode op = brstmt->op;
    if (brstmt->IsCondBr() || op == OP_goto || op == OP_switch || op == OP_throw || op == OP_return || op == OP_gosub ||
        op == OP_retsub) {
      InsertMeStmtBefore(brstmt, instmt);
    } else {
      AddMeStmtLast(instmt);
    }
  }
}

void BB::ReplaceMeStmt(MeStmt *stmt, MeStmt *newstmt) {
  InsertMeStmtBefore(stmt, newstmt);
  RemoveMeStmt(stmt);
}

void BB::DumpMePhiList(IRMap *irMap) {
  for (std::pair<OStIdx, MePhiNode *> phiEntry : mePhiList) {
    phiEntry.second->Dump(irMap);
  }
}

}  // namespace maple
