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
#include "me_analyze_rc.h"
#include "me_placement_opt.h"
#include "me_dominance.h"
#include "me_delegate_rc.h"
#include "me_condbased_opt.h"

// This phase analyzes the defs and uses of ref pointers in the function and
// performs the following modifications to the code:
//
// A. Insert a decref for the ref pointer before each of its definition.  If the
// ref poitner is local (a localrefvar) and the analysis shows that it is the
// first definition since entering the function, the decref will be omitted.
//
// B. At each statement that assigns a new value to a ref pointer, insert an
// incref after the assignment.  In cases where the incref has already been
// performed when the assigned value is computed, it will not insert the incref.
//
// C. A localrefvar need to be cleaned up before function exit.  This clean-up
// corresponds to performing a decref.  Instead of inserting a decref to clean up
// each localrefvar, the cleanup for the localrefvars in the function is
// represented aggregately via a call to the intrinsic CLEANUP_LOCALREFVARS
// inserted before each return statement.  The localrefvars to be cleaned up
// for each return statement are indicated as actual parameters in the
// intrinsiccall statement.  If a localrefvar's definition cannot reach a return
// statement, it will not be included in the actual parameters.  If the number
// of localrefvar parameters in the intrinsiccall is more than 200, the
// intrinsiccall will be omitted, in which case the code generator will clean up
// all localrefvars.
//
// For C, analyzerc can try to do more optimization by inserting decrefs for
// individual localrefvars earlier than the return statements.  This is done
// when the earliest_decref_cleanup flag is set to true.  This optimization is
// performed by calling PlacementOpt::ComputePlacement() for each localrefvar,
// and it will return the BBs in which to insert decref's for that localrefvar.
// Under earliest_decref_cleanup, the CLEANUP_LOCALREFVARS instrinciccall will
// still be inserted for the decrefs being inserted before the return statement.
//
// When a formal parameter of ref type is ever assigned inside the function,
// an incref for it is inserted at function entry.  Such a formal parameter also
// has a decref to clean it up inserted before each return statement.
// The above placement optimization is not applicable for formals.
//
// If the return statement returns a ref pointer variable, an incref for it
// needs to be effected. This is not handled by analyzerc, but is taken care of
// by the rcLowering phase.
//
// This phase needs to be run before register promotion, because the needed
// processing associated with a localrefvar would no longer be apparent after it
// is promoted to register.  Because it is run after EPRE phase, analyzerc can
// encounter regassign's and regread's of ref type, which are not supposed to
// trigger decref or incref insertion.

#define NODEC_IN_CTOR 1

// if number of live localrefvars is more than this limit at a return,
// will not insert the intrinsiccall to MMPL_CLEANUP_LOCALREFVARS
const int kCleanupLocalRefVarsLimit = 200;

namespace maple {
void RCItem::Dump() {
  ost->Dump();
  if (!noalias) {
    LogInfo::MapleLogger() << " aliased";
  }
  if (nonlocal) {
    LogInfo::MapleLogger() << " nonlocal";
  }
  if (isformal) {
    LogInfo::MapleLogger() << " isformal";
  }
  if (!occurBBs.empty()) {
    for (BBId bbid : occurBBs) {
      LogInfo::MapleLogger() << " " << bbid.idx;
    }
  }
  LogInfo::MapleLogger() << std::endl;
}

RCItem *AnalyzeRC::FindOrCreateRCItem(OriginalSt *ost) {
  MapleMap<OStIdx, RCItem *>::iterator mapit = rcitemsmap.find(ost->index);
  if (mapit != rcitemsmap.end()) {
    return mapit->second;
  }
  RCItem *rci = analyzercMp->New<RCItem>(ost, &analyzercAllocator);
  rcitemsmap[ost->index] = rci;
  if (ost->indexRenamedFrom.idx != 0) {  // change to use the original ost
    ost = ssaTab->GetOriginalStFromid(ost->indexRenamedFrom);
  }
  if (ost->index.idx >= aliasclass->osym2Elem.size()) {
    rci->noalias = true;
  } else {
    AliasElem *ae = aliasclass->osym2Elem[ost->index.idx];
    rci->noalias = ae->classSet == nullptr;
  }
  rci->nonlocal = ost->indirectLev > 0 || !ost->isLocal;
  if (!rci->nonlocal) {
    rci->isformal = ost->isFormal;
  }
  return rci;
}

OriginalSt *AnalyzeRC::GetOriginalSt(MeExpr *refLhs) {
  OriginalSt *ost = nullptr;
  if (refLhs->meOp == kMeOpVar) {
    VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(refLhs);
    return varmeexpr->ost;
  }
  ASSERT(refLhs->meOp == kMeOpIvar, "GetOriginalSt: unexpected node type");
  IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(refLhs);
  if (ivarmeexpr->mu != nullptr) {
    return ivarmeexpr->mu->ost;
  }
  ASSERT(ivarmeexpr->defStmt != nullptr, "GetOriginalSt: ivar with mu==nullptr has no defStmt");
  IassignMeStmt *iass = ivarmeexpr->defStmt;
  ASSERT(!iass->chiList.empty(), "GetOriginalSt: ivar with mu==nullptr has empty chiList at its def");
  return iass->chiList.begin()->second->lhs->ost;
}

// check if incref needs to be inserted after this ref pointer assignment;
// if it is callassigned, the incref has already been done in the callee;
// if rhs is gcmalloc/gcmallocjarray, the refcount is already 1;
// if rhs is neither dread or iread, it cannot be a pointer, so incref not needed
bool AnalyzeRC::NeedIncref(MeStmt *stmt) {
  if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
    return false;
  }
  return stmt->GetRhs()->PointsToSomethingThatNeedsIncref();
}

// identify assignments to ref pointers and insert decref before it and incref
// after it
void AnalyzeRC::IdentifyRCStmts() {
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->meStmtList) {
      MeExpr *lhsRef = stmt->GetLhsRef(ssaTab, skip_localrefvars);
      if (lhsRef != nullptr) {
        OriginalSt *ost = GetOriginalSt(lhsRef);
        ASSERT(ost != nullptr, "IdentifyRCStmts: cannot get SymbolOriginalSt");
        FindOrCreateRCItem(ost);
        // this part for inserting decref
        if (lhsRef->meOp == kMeOpVar) {
          // insert a decref statement
          UnaryMeStmt *decrefstmt = irMap->New<UnaryMeStmt>(OP_decref);
          decrefstmt->bb = bb;
          decrefstmt->srcPos = stmt->srcPos;
          decrefstmt->opnd = irMap->GetOrCreateZeroVersionVarMeExpr(ost);
          if (MeOption::eaoptrc && static_cast<VarMeExpr *>(lhsRef)->escStatus == NoEsc) {
            decrefstmt->not_need_lock = true;
          }
          // insertion position is before stmt
          bb->InsertMeStmtBefore(stmt, decrefstmt);
          if (!ost->isLocal) {
            globalWrite = true;
          }
        } else {
          IvarMeExpr *lhsIvar = static_cast<IvarMeExpr *>(lhsRef);
          {
            // insert a decref statement
            UnaryMeStmt *decrefstmt = irMap->New<UnaryMeStmt>(OP_decref);
            decrefstmt->bb = bb;
            decrefstmt->srcPos = stmt->srcPos;
            IvarMeExpr ivarmeexpr(-1, lhsIvar->primType, lhsIvar->tyIdx, lhsIvar->fieldID);
            ivarmeexpr.base = lhsIvar->base;
            // form mu from chilist
            IassignMeStmt *iass = static_cast<IassignMeStmt *>(stmt);
            MapleMap<OStIdx, ChiMeNode *>::iterator xit = iass->chiList.begin();
            for (; xit != iass->chiList.end(); xit++) {
              ChiMeNode *chi = xit->second;
              if (chi->rhs->ost == ost) {
                ivarmeexpr.mu = static_cast<VarMeExpr *>(chi->rhs);
                break;
              }
            }
            ASSERT(xit != iass->chiList.end(), "IdentifyRCStmts: failed to find corresponding chi node");
            decrefstmt->opnd = irMap->HashMeExpr(&ivarmeexpr);
            // insertion position is before stmt
            bb->InsertMeStmtBefore(stmt, decrefstmt);

            ost = GetOriginalSt(decrefstmt->opnd);
            ASSERT(ost != nullptr, "IdentifyRCStmts: cannot get SymbolOriginalSt");
            FindOrCreateRCItem(ost);
          }
        }
        // this part for inserting incref
        if (NeedIncref(stmt)) {
          stmt->SetNeedIncref();
        }
      }
    }  // end of stmt iteration
  }
}

// if globalWrite is true, entry increfs are inserted for all formals; otherwise
// they are inserted only for formals that are defined in the function
void AnalyzeRC::InsertEntryIncrefs4Formals() {
  std::vector<OriginalSt *> formalOstVec;
  if (globalWrite) {
    MIRFunction *mirFunc = func->mirFunc;
    for (size_t i = (mirFunc->IsStatic() ? 0 : 1); i < mirFunc->formalDefVec.size(); ++i) {
      MIRSymbol *sym = mirFunc->formalDefVec[i].formalSym;
      if (sym->IgnoreRC()) {
        continue;
      }
      OriginalSt *ost = ssaTab->FindSymbolOriginalSt(sym);
      if (ost == NULL) {
        continue;
      }
      formalOstVec.push_back(ost);
      FindOrCreateRCItem(ost);
      MeStmt *dass = CreateIncrefZeroVersion(ost);
      dass->bb = func->theCFG->first_bb;
      dass->bb->AddMeStmtFirst(dass);
    }
  }
  else {
    for (std::pair<OStIdx, RCItem *> mapitem : rcitemsmap) {
      RCItem *rcitem = mapitem.second;
      if (rcitem->isformal) {
        formalOstVec.push_back(rcitem->ost);
        InsertInitAtPUEntry(rcitem);
      }
    }
  }
  if (!skip_localrefvars || formalOstVec.empty()) {
    return;
  }
  // insert decref for the formals at each return
  for (BB *bb : func->theCFG->commonExitBB->pred) {
    MeStmt *lastMeStmt = bb->meStmtList.last;
    if (lastMeStmt == nullptr || lastMeStmt->op != OP_return) {
      continue;
    }
    for (OriginalSt *ost : formalOstVec) {
      UnaryMeStmt *decref = GenerateDecrefBeforeDead(ost->index, bb);
      bb->InsertMeStmtBefore(bb->meStmtList.last, decref);
    }
  }
}

void AnalyzeRC::CreateCleanupIntrinsics() {
  for (BB *bb : func->theCFG->commonExitBB->pred) {
    MeStmt *lastMeStmt = bb->meStmtList.last;
    if (lastMeStmt && lastMeStmt->op == OP_return) {
      IntrinsiccallMeStmt *intrn = irMap->NewInPool<IntrinsiccallMeStmt>(OP_intrinsiccall, INTRN_MPL_CLEANUP_LOCALREFVARS);
      for (std::pair<OStIdx, RCItem *> mapitem : rcitemsmap) {
        RCItem *rcitem = mapitem.second;
        if (rcitem->nonlocal) {
          continue;
        }
        intrn->opnds.push_back(irMap->GetOrCreateZeroVersionVarMeExpr(rcitem->ost));
      }
      bb->InsertMeStmtBefore(lastMeStmt, intrn);
    }
  }
}

void AnalyzeRC::RenameRefPtrs(BB *bb) {
  if (bb == nullptr) {
    return;
  }
  if (skip_localrefvars) {
    return;
  }
  std::map<RCItem *, uint32> savedStacksize;  // to record stack size in each RCItem for stack pop-ups
  for (std::pair<OStIdx, RCItem *> mapitem : rcitemsmap) {
    RCItem *rcitem = mapitem.second;
    if (rcitem->nonlocal) {
      continue;
    }
    // record stack size
    savedStacksize[rcitem] = rcitem->versionStack.size();
    // if there is a phi, push stack
    MapleMap<OStIdx, MePhiNode *>::iterator phiit = bb->mePhiList.find(mapitem.second->ost->index);
    if (phiit != bb->mePhiList.end()) {
      rcitem->versionStack.push((*phiit).second->lhs);
    }
  }

  // traverse the BB stmts
  if (!bb->meStmtList.empty()) {
    for (auto mestmt : bb->meStmtList) {
      if (mestmt->op == OP_decref) {
        // rename this node
        UnaryMeStmt *decrefstmt = static_cast<UnaryMeStmt *>(mestmt);
        OriginalSt *ost = GetOriginalSt(decrefstmt->opnd);
        RCItem *rcitem = rcitemsmap[ost->index];
        if (!rcitem->nonlocal && !rcitem->versionStack.empty()) {
          decrefstmt->opnd = rcitem->versionStack.top();
        }
      } else if (mestmt->op == OP_intrinsiccall &&
                 static_cast<IntrinsiccallMeStmt *>(mestmt)->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS) {
        IntrinsiccallMeStmt *intrn = static_cast<IntrinsiccallMeStmt *>(mestmt);
        for (uint32 i = 0; i < intrn->opnds.size(); i++) {
          VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(intrn->opnds[i]);
          OriginalSt *ost = varmeexpr->ost;
          RCItem *rcitem = rcitemsmap[ost->index];
          ASSERT(!rcitem->nonlocal, "cleanup_localrefvars only takes locals");
          if (!rcitem->versionStack.empty()) {
            intrn->opnds[i] = rcitem->versionStack.top();
          }
        }
      } else {
        MeExpr *lhsRef = mestmt->GetLhsRef(ssaTab, skip_localrefvars);
        if (lhsRef != nullptr) {
          OriginalSt *ost = GetOriginalSt(lhsRef);
          RCItem *rcitem = rcitemsmap[ost->index];
          if (!rcitem->nonlocal) {
            rcitem->versionStack.push(lhsRef);
          }
        }
      }
    }
  }

  // recursive call in preorder traversal of dominator tree
  CHECK(bb->id.idx < dominance->domChildren.size(), "index out of range in AnalyzeRC::RenameRefPtrs");
  MapleSet<BBId> *domChildren = &dominance->domChildren[bb->id.idx];
  for (MapleSet<BBId>::iterator bbit = domChildren->begin(); bbit != domChildren->end(); bbit++) {
    BBId childbbid = *bbit;
    RenameRefPtrs(func->theCFG->bbVec[childbbid.idx]);
  }

  // restore the stacks to their size at entry to this function invocation
  for (std::pair<OStIdx, RCItem *> mapitem : rcitemsmap) {
    RCItem *rcitem = mapitem.second;
    if (rcitem->nonlocal) {
      continue;
    }
    uint32 lastSize = savedStacksize[rcitem];
    while (rcitem->versionStack.size() > lastSize) {
      rcitem->versionStack.pop();
    }
  }
}

DassignMeStmt *AnalyzeRC::CreateDassignInit(OStIdx ostidx) {
  OriginalSt *ost = ssaTab->GetOriginalStFromid(ostidx);
  VarMeExpr *newvar = irMap->New<VarMeExpr>(-1, ost, -1, PTY_ref);
  newvar->ost->fieldID = ost->fieldID;
  DassignMeStmt *dass = irMap->NewInPool<DassignMeStmt>(newvar, nullptr);
  MIRIntConst *intConst = func->mirModule.memPool->New<MIRIntConst>(0, GlobalTables::GetTypeTable().typeTable[PTY_ptr]);
  ConstMeExpr constmeexpr(-1, intConst, PTY_ptr);
  dass->rhs = irMap->HashMeExpr(&constmeexpr);
  return dass;
}

UnaryMeStmt *AnalyzeRC::CreateIncrefZeroVersion(OriginalSt *ost) {
  UnaryMeStmt *increfstmt = irMap->New<UnaryMeStmt>(OP_incref);
  increfstmt->opnd = irMap->GetOrCreateZeroVersionVarMeExpr(ost);
  return increfstmt;
}

// insert initialization (incref for formal) at end of entry BB
void AnalyzeRC::InsertInitAtPUEntry(RCItem *rci) {
  MeStmt *dass = nullptr;
  if (rci->isformal) {
    dass = CreateIncrefZeroVersion(rci->ost);
  } else {
    dass = CreateDassignInit(rci->ost->index);
  }
  dass->bb = func->theCFG->first_bb;
  dass->bb->AddMeStmtFirst(dass);
  rci->occurBBs.insert(func->theCFG->first_bb->id);
}

void AnalyzeRC::CollectLocalRefPointerUses(MeExpr *x, BBId bbid) {
  if (x == nullptr) {
    return;
  }
  if (x->meOp == kMeOpVar) {
    VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(x);
    if (varmeexpr->primType != PTY_ref) {
      return;
    }
    OriginalSt *ost = GetOriginalSt(varmeexpr);
    MapleMap<OStIdx, RCItem *>::iterator mapit = rcitemsmap.find(ost->index);
    if (mapit == rcitemsmap.end()) {
      return;
    }
    RCItem *rcitem = mapit->second;
    if (!rcitem->nonlocal) {
      rcitem->occurBBs.insert(bbid);
    }
    return;
  }
  for (int32 i = 0; i < x->NumMeExprOpnds(); i++) {
    CollectLocalRefPointerUses(x->GetOpnd(i), bbid);
  }
  return;
}

UnaryMeStmt *AnalyzeRC::GenerateDecrefBeforeDead(OStIdx ostidx, BB *bb) {
  OriginalSt *ost = ssaTab->GetOriginalStFromid(ostidx);
  UnaryMeStmt *decrefstmt = irMap->New<UnaryMeStmt>(OP_decref);
  decrefstmt->bb = bb;
  decrefstmt->opnd = irMap->GetOrCreateZeroVersionVarMeExpr(ost);
  decrefstmt->decref_before_exit = true;
  return decrefstmt;
}

void AnalyzeRC::RenameDecrefsBeforeExit(BB *bb) {
  if (bb == nullptr) {
    return;
  }
  std::map<RCItem *, uint32> savedStacksize;  // to record stack size
  // in each RCItem for stack pop-ups
  for (std::pair<OStIdx, RCItem *> mapitem : rcitemsmap) {
    RCItem *rcitem = mapitem.second;
    if (rcitem->nonlocal) {
      continue;
    }
    // record stack size
    savedStacksize[rcitem] = rcitem->versionStack.size();
    // if there is a phi, push stack
    MapleMap<OStIdx, MePhiNode *>::iterator phiit = bb->mePhiList.find(mapitem.second->ost->index);
    if (phiit != bb->mePhiList.end()) {
      rcitem->versionStack.push((*phiit).second->lhs);
    }
  }

  // traverse the BB stmts
  if (!bb->meStmtList.empty()) {
    for (auto mestmt : bb->meStmtList) {
      if (mestmt->op == OP_decref) {
        UnaryMeStmt *decrefstmt = static_cast<UnaryMeStmt *>(mestmt);
        if (!decrefstmt->decref_before_exit) {
          continue;
        }
        // rename this decref operand
        OriginalSt *ost = GetOriginalSt(decrefstmt->opnd);
        RCItem *rcitem = rcitemsmap[ost->index];
        if (!rcitem->nonlocal && !rcitem->versionStack.empty()) {
          decrefstmt->opnd = rcitem->versionStack.top();
        }
      } else {
        MeExpr *lhsRef = mestmt->GetLhsRef(ssaTab, skip_localrefvars);
        if (lhsRef != nullptr) {
          OriginalSt *ost = GetOriginalSt(lhsRef);
          RCItem *rcitem = rcitemsmap[ost->index];
          if (!rcitem->nonlocal) {
            rcitem->versionStack.push(lhsRef);
          }
        }
      }
    }
  }

  // recursive call in preorder traversal of dominator tree
  CHECK(bb->id.idx < dominance->domChildren.size(), "index out of range in AnalyzeRC::RenameDecrefsBeforeExit");
  MapleSet<BBId> *domChildren = &dominance->domChildren[bb->id.idx];
  for (MapleSet<BBId>::iterator bbit = domChildren->begin(); bbit != domChildren->end(); bbit++) {
    BBId childbbid = *bbit;
    RenameDecrefsBeforeExit(func->theCFG->bbVec[childbbid.idx]);
  }

  // restore the stacks to their size at entry to this function invocation
  for (std::pair<OStIdx, RCItem *> mapitem : rcitemsmap) {
    RCItem *rcitem = mapitem.second;
    if (rcitem->nonlocal) {
      continue;
    }
    uint32 lastSize = savedStacksize[rcitem];
    while (rcitem->versionStack.size() > lastSize) {
      rcitem->versionStack.pop();
    }
  }
}

// find the next statement which is not a comment statement
static MeStmt *NextNonCommentStmt(MeStmt *stmt) {
  MeStmt *nxtstmt = stmt->next;
  while (nxtstmt != nullptr && nxtstmt->op == OP_comment) {
    nxtstmt = nxtstmt->next;
  }
  return nxtstmt;
}

// starting from bb's last stmt, search backward to find the stmt containing the
// last use of the symbol; return nullptr if not found
static MeStmt *FindLastUseStmt(OStIdx oidx, BB *bb) {
  for (MeStmt *stmt = bb->meStmtList.last; stmt; stmt = stmt->prev) {
    for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
      if (stmt->GetMeStmtOpnd(i)->SymAppears(oidx)) {
        // if it is the return stmt, return the stmt for the cleanup intrinsic
        if (stmt->op == OP_return) {
          do {
            stmt = stmt->prev;
            if (stmt->op == OP_intrinsiccall &&
                static_cast<IntrinsiccallMeStmt *>(stmt)->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS) {
              return stmt;
            }
          } while (true);
        }
        if (bb->meStmtList.last->op != OP_return) {
          return stmt;
        }
        // if just before the cleanup intrinsic, return the cleanup intrinsic stmt
        MeStmt *nxtstmt = NextNonCommentStmt(stmt);
        if (nxtstmt->op == OP_intrinsiccall &&
            static_cast<IntrinsiccallMeStmt *>(nxtstmt)->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS) {
          return nxtstmt;
        }
        return stmt;
      }
    }
  }
  return nullptr;
}

// check if the bb contains any call
static bool BBHasCall(BB *bb) {
  for (auto stmt : bb->meStmtList) {
    if (kOpcodeInfo.IsCall(stmt->op) && stmt->op != OP_intrinsiccall && stmt->op != OP_intrinsiccallassigned &&
        stmt->op != OP_intrinsiccallwithtype && stmt->op != OP_intrinsiccallwithtypeassigned) {
      return true;
    }
  }
  return false;
}

void AnalyzeRC::OptimizeRC() {
  for (BB *bb : func->theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->meStmtList) {
      if (stmt->op != OP_decref) {
        if (earliest_decref_cleanup) {
          // look for uses and update occurBBs
          for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
            CollectLocalRefPointerUses(stmt->GetMeStmtOpnd(i), bb->id);
          }
        }
        continue;
      }
      UnaryMeStmt *decref = static_cast<UnaryMeStmt *>(stmt);
      MeStmt *refassign = stmt->next;
      OriginalSt *ost = GetOriginalSt(decref->opnd);
      if (decref->opnd->meOp == kMeOpVar && ost->isLocal && skip_localrefvars) {
        continue;
      }
      CHECK_FATAL(rcitemsmap.find(ost->index) != rcitemsmap.end(),
             " AnalyzeRC::OptimizeRC:: rcitem has not been created");
      RCItem *rcitem = rcitemsmap[ost->index];
      bool decrefIsNeeded = true;
      // see if the decref can be optimized away
      if (rcitem->nonlocal) {
#if NODEC_IN_CTOR
        // the decref can be avoided for iassign thru this in constructor funcs
        if (ost->indirectLev == 1 && func->mirFunc->IsConstructor()) {
          IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(decref->opnd);
          if (ivarmeexpr->base->meOp == kMeOpVar) {
            VarMeExpr *basevar = static_cast<VarMeExpr *>(ivarmeexpr->base);
            MIRSymbol *sym = basevar->ost->GetMIRSymbol();
            if (sym->storageClass == kScFormal && sym == func->mirFunc->formalDefVec[0].formalSym) {
              MIRType *basetype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ivarmeexpr->tyIdx);
              MIRType *type = static_cast<MIRPtrType *>(basetype)->GetPointedType();
              if (dynamic_cast<MIRClassType *>(type)) {
                MIRClassType *classType = static_cast<MIRClassType *>(type);
                // check ivarmeexpr->fieldID is not from base classes
                if (classType->IsOwnField(ivarmeexpr->fieldID)) {
                  // check the ivar's mu is zero version
                  if (ivarmeexpr->mu->vstIdx == ost->zeroVersionIndex || ivarmeexpr->mu->defBy == kDefByNo) {
                    decrefIsNeeded = false;
                  }
                }
              }
            }
          }
        }
#endif
      } else if (!rcitem->nonlocal && !rcitem->noalias) {
        ASSERT(false, "OptimizeRC: local pointers cannot have alias");
      } else if (decref->opnd->meOp == kMeOpVar) {
        // !nonlocal && noalias
        rcitem->occurBBs.insert(bb->id);
        rcitem->needsomeRC = true;
        VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(decref->opnd);
        if (!rcitem->isformal && (varmeexpr->vstIdx == ost->zeroVersionIndex || varmeexpr->defBy == kDefByNo)) {
          decrefIsNeeded = false;
        }
      }
      if (decrefIsNeeded) {
        refassign->SetNeedDecref();
      } else {
        bb->RemoveMeStmt(stmt);  // delete the decref
      }
      stmt = refassign;  // next iteration will process the stmt after refassign
      if (earliest_decref_cleanup) {
        // look for uses and update occurBBs
        for (int32 i = 0; i < stmt->NumMeStmtOpnds(); i++) {
          CollectLocalRefPointerUses(stmt->GetMeStmtOpnd(i), bb->id);
        }
      }
    }
  }
  if (skip_localrefvars) {
    return;
  }
  if (!MeOption::rcLowering) {
    return;
  }
  // determine placement of last decref for each local ref pointer
  PlacementOpt placeopt(func, dominance);
  if (DEBUGFUNC(func)) {
    placeopt.placementoptdebug = true;
  }
  if (!earliest_decref_cleanup) {
    return;
  }
  for (std::pair<OStIdx, RCItem *> mapitem : rcitemsmap) {
    RCItem *rci = mapitem.second;
    if (rci->nonlocal || rci->isformal) {
      continue;
    }
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "optimizing placement for final decref of ";
      rci->ost->Dump();
      LogInfo::MapleLogger() << std::endl;
    }
    placeopt.ComputePlacement(&rci->occurBBs);
    for (BBId bbid : placeopt.inserted_bbs) {
      BB *insertedbb = func->theCFG->bbVec[bbid.idx];
      MeStmt *insertedLast = insertedbb->meStmtList.last;
      if (insertedbb->succ.size() == 0 && insertedLast && insertedLast->op == OP_return &&
          !BBHasCall(insertedbb)) {
        // add to arguments of the INTRN_CLEANUP_LOCALREFVARS intrinsiccall
        MeStmt *mestmt = insertedLast->prev;
        CHECK_FATAL(mestmt->op == OP_intrinsiccall, "OptimizeRC: cannot find cleanup intrinsic stmt");
        IntrinsiccallMeStmt *intrn = static_cast<IntrinsiccallMeStmt *>(mestmt);
        CHECK_FATAL(intrn->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS, "OptimizeRC: cannot find cleanup intrinsic stmt");
        intrn->opnds.push_back(irMap->GetOrCreateZeroVersionVarMeExpr(rci->ost));
      } else {
        UnaryMeStmt *decref = GenerateDecrefBeforeDead(mapitem.first, insertedbb);
        if (insertedbb->IsMeStmtEmpty()) {
          insertedbb->AddMeStmtLast(decref);
        } else {
          // skip statements required to be first statement in BB
          MeStmt *curstmt = insertedbb->meStmtList.first;
          while (curstmt && (curstmt->op == OP_javacatch || curstmt->op == OP_javatry ||
                             curstmt->op == OP_catch || curstmt->op == OP_try || curstmt->op == OP_comment)) {
            curstmt = curstmt->next;
          }
          if (curstmt == nullptr) {
            insertedbb->AddMeStmtLast(decref);
          } else {
            insertedbb->InsertMeStmtBefore(curstmt, decref);
          }
        }
      }
    }
    for (BBId bbid : placeopt.lastuse_bbs) {
      BB *lastusebb = func->theCFG->bbVec[bbid.idx];
      MeStmt *lastStmt = lastusebb->meStmtList.last;
      if (lastusebb->succ.empty() && lastStmt && lastStmt->op == OP_throw) {
        continue;  // decref before exit  not needed if exitting via throw
      }
      if (lastusebb->succ.size() == 0 && lastStmt && lastStmt->op == OP_return &&
          !BBHasCall(lastusebb)) {
        // add to arguments of the INTRN_CLEANUP_LOCALREFVARS intrinsiccall
        MeStmt *mestmt = lastStmt->prev;
        CHECK_FATAL(mestmt->op == OP_intrinsiccall, "OptimizeRC: cannot find cleanup intrinsic stmt");
        IntrinsiccallMeStmt *intrn = static_cast<IntrinsiccallMeStmt *>(mestmt);
        CHECK_FATAL(intrn->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS, "OptimizeRC: cannot find cleanup intrinsic stmt");
        intrn->opnds.push_back(irMap->GetOrCreateZeroVersionVarMeExpr(rci->ost));
      } else {
        MeStmt *lastusestmt = FindLastUseStmt(mapitem.first, lastusebb);
        if (lastusestmt != nullptr && lastusestmt->op == OP_intrinsiccall &&
            static_cast<IntrinsiccallMeStmt *>(lastusestmt)->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS) {
          // add to arguments of the INTRN_CLEANUP_LOCALREFVARS intrinsiccall
          IntrinsiccallMeStmt *intrn = static_cast<IntrinsiccallMeStmt *>(lastusestmt);
          intrn->opnds.push_back(irMap->GetOrCreateZeroVersionVarMeExpr(rci->ost));
        } else {
          UnaryMeStmt *decref = GenerateDecrefBeforeDead(mapitem.first, lastusebb);
          if (lastusestmt == nullptr) {  // insert at BB entry
            // skip statements required to be first statement in BB
            MeStmt *curstmt = lastusebb->meStmtList.first;
            while (curstmt && (curstmt->op == OP_javacatch || curstmt->op == OP_javatry ||
                               curstmt->op == OP_catch || curstmt->op == OP_try || curstmt->op == OP_comment)) {
              curstmt = curstmt->next;
            }
            if (curstmt == nullptr) {
              lastusebb->AddMeStmtLast(decref);
            } else {
              lastusebb->InsertMeStmtBefore(curstmt, decref);
            }
          } else if (lastusestmt == lastusebb->meStmtList.last && lastusebb->kind != kBBFallthru) {
            lastusebb->InsertMeStmtBefore(lastusebb->meStmtList.last, decref);
          } else {
            lastusebb->InsertMeStmtAfter(lastusestmt, decref);
          }
        }
      }
    }
  }
  RenameDecrefsBeforeExit(func->theCFG->commonEntryBB);
}

// among the arguments in the intrinsiccall to INTRN_CLEANUP_LOCALREFVARS, those
// that are zero version are not live, and can be deleted; if the number of
// arguments left are > 200, delete the intrinsiccall; for any formal among the
// arguments, delete it and create a separate decref for it, regardless of
// whether it is zero version or not, because it always has incref inserted
// at function entry.
void AnalyzeRC::RemoveUnneededCleanups() {
  for (BB *bb : func->theCFG->commonExitBB->pred) {
    MeStmt *lastMeStmt = bb->meStmtList.last;
    if (lastMeStmt == nullptr || lastMeStmt->op != OP_return) {
      continue;
    }
    MeStmt *mestmt = lastMeStmt->prev;
    CHECK_FATAL(mestmt->op == OP_intrinsiccall, "RemoveUnneededCleanups: cannot find cleanup intrinsic stmt");
    IntrinsiccallMeStmt *intrn = static_cast<IntrinsiccallMeStmt *>(mestmt);
    CHECK_FATAL(intrn->intrinsic == INTRN_MPL_CLEANUP_LOCALREFVARS,
           "RemoveUnneededCleanups: cannot find cleanup intrinsic stmt");
    // delete the zero version operands
    if (earliest_decref_cleanup) {
      intrn->opnds.clear();
      continue;
    }
    uint32 nextpos = 0;
    uint32 i = 0;
    for (; i < intrn->opnds.size(); i++) {
      VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(intrn->opnds[i]);
      OriginalSt *ost = varmeexpr->ost;
      if (ost->isFormal) {  // omit as argument and insert separate decref
        UnaryMeStmt *decref = GenerateDecrefBeforeDead(ost->index, bb);
        decref->opnd = varmeexpr;
        bb->InsertMeStmtBefore(bb->meStmtList.last, decref);
        continue;
      }
      if (varmeexpr->IsZeroVersion()) {
        continue;
      }
      if (nextpos != i) {
        intrn->opnds[nextpos] = varmeexpr;
      }
      nextpos++;
    }
    while (nextpos < i) {
      intrn->opnds.pop_back();
      i--;
    }
    if (intrn->opnds.size() > 200) {
      bb->RemoveMeStmt(intrn);  // delete the intrinsiccall stmt
    }
  }
}

AnalysisResult *MeDoAnalyzeRC::Run(MeFunction *func, MeFuncResultMgr *m) {
  MeIRMap *hmap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func, !MeOption::quiet));
  ASSERT(hmap != nullptr, "irmapbuild phase has problem");

  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func, !MeOption::quiet));
  ASSERT(dom != nullptr, "dominance phase has problem");

  AliasClass *aliasclass = static_cast<AliasClass *>(m->GetAnalysisResult(MeFuncPhase_ALIASCLASS, func, !MeOption::quiet));
  ASSERT(aliasclass != nullptr, "aliasclass phase has problem");

  MIRFunction *mirfunction = func->mirFunc;

  func->mirModule.hints = kRcAnalyzed;

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << " Processing " << mirfunction->GetName() << std::endl;
  }

  {
    // add extra scope so destructor for analyzerc will be invoked earlier
    MemPool *analyzercmp = mempoolctrler.NewMemPool(PhaseName().c_str());
    AnalyzeRC analyzerc(func, dom, aliasclass, analyzercmp);

    if (MeOption::earlydecref && MeOption::optLevel > 0) {
      analyzerc.earliest_decref_cleanup = true;
    }
    if (func->placementRCOn) {
      analyzerc.earliest_decref_cleanup = false;
      analyzerc.skip_localrefvars = true;
    }

    analyzerc.IdentifyRCStmts();
    analyzerc.InsertEntryIncrefs4Formals();
    if (!analyzerc.skip_localrefvars) {
      analyzerc.CreateCleanupIntrinsics();
    }
    analyzerc.RenameRefPtrs(func->theCFG->commonEntryBB);
    if (MeOption::optLevel > 0 && !analyzerc.skip_localrefvars) {
      analyzerc.RemoveUnneededCleanups();
    }
    analyzerc.OptimizeRC();
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "\n============== After ANALYZE RC =============" << std::endl;
      func->irMap->Dump();
    }

    mempoolctrler.DeleteMemPool(analyzercmp);

  }  // end of analyzerc's scope

  if (!MeOption::nodelegaterc && !func->placementRCOn && MeOption::rcLowering && MeOption::optLevel > 0) {
    MeDoDelegateRC dodelegaterc(MeFuncPhase_DELEGATERC);
    if (!MeOption::quiet) {
      LogInfo::MapleLogger() << "  == " << PhaseName() << " invokes [ " << dodelegaterc.PhaseName() << " ] ==\n";
    }
    dodelegaterc.Run(func, m);
  }

  if (!MeOption::nocondbasedrc && MeOption::rcLowering && MeOption::optLevel > 0) {
    MeDoCondBasedRC docondbasedrc(MeFuncPhase_CONDBASEDRC);
    if (!MeOption::quiet) {
      LogInfo::MapleLogger() << "  == " << PhaseName() << " invokes [ " << docondbasedrc.PhaseName() << " ] ==\n";
    }
    docondbasedrc.Run(func, m);
  }

  return nullptr;
}

}  // namespace maple
