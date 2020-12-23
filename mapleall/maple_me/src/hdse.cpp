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


#include "hdse.h"
#include <iostream>
#include "ssa_mir_nodes.h"
#include "ver_symbol.h"
#include "dominance.h"
#include "irmap.h"
#include "me_ssa.h"
#include "opcode_info.h"
#include "mir_preg.h"

#define JAVALANG (irMap->ssaTab->mirModule.IsJavaModule())

namespace maple {

void HDSE::MarkExprNeeded(MeExpr *meexpr) {
  if (IsExprNeeded(meexpr)) {
    return;
  }
  SetExprNeeded(meexpr);
  MeExprOp meOp = meexpr->meOp;
  switch (meOp) {
    case kMeOpVar: {
      VarMeExpr *varexpr = static_cast<VarMeExpr *>(meexpr);

      switch (varexpr->defBy) {
        case kDefByNo:
          break;
        case kDefByStmt:
          CHECK_FATAL(varexpr->def.defStmt, "");
          MarkStmtNeeded(varexpr->def.defStmt);
          break;
        case kDefByPhi:
          MarkPhiNeeded(varexpr->def.defPhi);
          break;
        case kDefByChi:
          MarkChiNodeNeeded(varexpr->def.defChi);
          break;
        case kDefByMustdef: {
          MustDefMeNode *mustDef = varexpr->def.defMustDef;
          if (mustDef->isLive) {
            break;
          }
          mustDef->isLive = true;
          MarkStmtNeeded(mustDef->base);
          break;
        }
        default:
          ASSERT(false, "var defined wrong");
      }
      return;
    }
    case kMeOpReg: {
      RegMeExpr *regread = static_cast<RegMeExpr *>(meexpr);
      PregIdx regIdx = regread->GetPregIdx();
      if (regIdx == -kSregRetval0) {
        if (regread->def.defStmt) {
          MarkStmtNeeded(regread->def.defStmt);
        }
        return;
      }
      switch (regread->defBy) {
        case kDefByNo:
          break;
        case kDefByStmt:
          CHECK_FATAL(regread->def.defStmt, "");
          MarkStmtNeeded(regread->def.defStmt);
          break;
        case kDefByPhi:
          MarkPhiNeeded(regread->def.defPhi);
          break;
        case kDefByChi:
          ASSERT(regread->ost->indirectLev > 0, "MarkExprNeeded: preg cannot be defined by chi");
          MarkChiNodeNeeded(regread->def.defChi);
          break;
        case kDefByMustdef: {
          MustDefMeNode *mustDef = regread->def.defMustDef;
          if (mustDef->isLive) {
            break;
          }
          mustDef->isLive = true;
          MarkStmtNeeded(mustDef->base);
          break;
        }
        default:
          ASSERT(false, "MarkExprNeeded: unexpected defBy value");
      }
      return;
    }
    case kMeOpOp: {
      OpMeExpr *meopexpr = static_cast<OpMeExpr *>(meexpr);
      MeExpr *opnd1 = meopexpr->GetOpnd(1);
      MeExpr *opnd2 = meopexpr->GetOpnd(2);

      worklist.push_front(meopexpr->GetOpnd(0));
      if (opnd1) {
        worklist.push_front(opnd1);
        if (opnd2) {
          worklist.push_front(opnd2);
        }
      }
      return;
    }
    case kMeOpNary: {
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(meexpr);
      for (size_t i = 0; i < narymeexpr->numOpnds; i++) {
        worklist.push_front(narymeexpr->GetOpnd(i));
      }
      return;
    }
    case kMeOpIvar: {
      IvarMeExpr *ivarmeexpr = static_cast<IvarMeExpr *>(meexpr);
      worklist.push_front(ivarmeexpr->base);
      ScalarMeExpr *mu = ivarmeexpr->mu;
      if (mu != nullptr) {
        worklist.push_front(mu);
        if (mu->defBy != kDefByNo) {
          MapleMap<OStIdx, ChiMeNode *> *chiList = GenericGetChiListFromVarMeExpr(mu);
          if (chiList != nullptr) {
            MapleMap<OStIdx, ChiMeNode *>::iterator it = chiList->begin();
            for (; it != chiList->end(); it++) {
              MarkChiNodeNeeded(it->second);
            }
          }
        }
      }
      return;
    }
    default:
      ASSERT((meOp == kMeOpAddrof || meOp == kMeOpAddroffunc || meOp == kMeOpAddroflabel || meOp == kMeOpGcmalloc ||
              meOp == kMeOpConst || meOp == kMeOpConststr || meOp == kMeOpConststr16 || meOp == kMeOpSizeoftype),
              "MeOP NIY");
      return;
  }
}

void HDSE::MarkPhiNeeded(MePhiNode *phime) {
  phime->isLive = true;
  MarkBBNeeded(phime->defBB);
  for (auto meexpr : phime->opnds) {
    if (meexpr) {
      worklist.push_front(meexpr);
    }
  }
}

void HDSE::MarkMuListNeeded(MapleMap<OStIdx, ScalarMeExpr *> &mulist) {
  for (MapleMap<OStIdx, ScalarMeExpr *>::iterator it = mulist.begin(); it != mulist.end(); it++) {
    worklist.push_front(it->second);
  }
}

void HDSE::MarkChiNodeNeeded(ChiMeNode *chinode) {
  if (chinode->isLive) {
    return;
  }
  chinode->isLive = true;
  worklist.push_front(chinode->rhs);
  MeStmt *mestmt = chinode->base;
  MarkStmtNeeded(mestmt);
}

void HDSE::MarkBBNeeded(BB *bb) {
  if (bb_required[bb->id.idx]) {
    return;
  }
  bb_required[bb->id.idx] = true;
  MeStmt *laststmt = bb->meStmtList.last;
  if (laststmt != nullptr) {
    // if bb's last stmt is a branch instruction, it is also needed
    Opcode op = laststmt->op;
    if ((laststmt->IsCondBr() || op == OP_goto || op == OP_switch || op == OP_igoto) && !laststmt->isLive) {
      laststmt->isLive = true;
      if (op != OP_goto) {
        UnaryMeStmt *unarystmt = static_cast<UnaryMeStmt *>(laststmt);
        worklist.push_front(unarystmt->opnd);
      }
    }
  }
  // make last stmt in control-dependent BBs live
  CHECK(bb->id.idx < pdom->pdomFrontier.size(), "index out of range in HDSE::MarkBBNeeded");
  for (BBId cdBbid : pdom->pdomFrontier[bb->id.idx]) {
    BB *cdBb = bbVec[cdBbid.idx];
    CHECK_FATAL(cdBb != nullptr, "cd_bb is null in HDSE::MarkChiNodeNeeded");
    if (cdBb == bb || cdBb->IsMeStmtEmpty() ||  // stmts in cd_bb may be deleted in dse phase
        cdBb->IsFuncEntry()) {
      continue;
    }
    laststmt = cdBb->meStmtList.last;
    Opcode op = laststmt->op;
    CHECK_FATAL((laststmt->IsCondBr() || op == OP_switch || op == OP_igoto || op == OP_retsub || op == OP_throw || cdBb->InTryBlock() ||
            cdBb->WontExit()),
           "HDSE::MarkBBNeeded: control dependent on unexpected statement");
    if ((laststmt->IsCondBr() || op == OP_goto || op == OP_switch || op == OP_igoto || op == OP_retsub || op == OP_throw) &&
        !laststmt->isLive) {
      laststmt->isLive = true;
      UnaryMeStmt *unarystmt = dynamic_cast<UnaryMeStmt *>(laststmt);
      if (unarystmt) {
        worklist.push_front(unarystmt->opnd);
      } else if (op == OP_throw) {
        ThrowMeStmt *thrmestmt = static_cast<ThrowMeStmt *>(laststmt);
        worklist.push_front(thrmestmt->opnd);
        MarkMuListNeeded(thrmestmt->muList);
      }
      MarkBBNeeded(cdBb);
    }
  }
  // make unconditional goto's in bb's predecessors live
  for (MapleVector<BB *>::iterator predIt = bb->pred.begin(); predIt != bb->pred.end(); predIt++) {
    BB *predBb = *predIt;
    if (predBb == bb) {
      continue;
    }
    laststmt = predBb->meStmtList.last;
    if (laststmt != nullptr && !laststmt->isLive && laststmt->op == OP_goto) {
      laststmt->isLive = true;
      MarkBBNeeded(predBb);
    }
  }
}

void HDSE::MarkStmtNeeded(MeStmt *mestmt) {
  if (mestmt->isLive) {
    return;
  }
  mestmt->isLive = true;
  Opcode op = mestmt->op;
  switch (op) {
    case OP_iassign: {
      IassignMeStmt *ivarstmt = static_cast<IassignMeStmt *>(mestmt);
      worklist.push_front(ivarstmt->lhsVar->base);
    }
    case OP_dassign:
    case OP_regassign:
    case OP_maydassign: {
      worklist.push_front(mestmt->GetRhs());
      break;
    }
    case OP_call:
    case OP_icall:
    case OP_virtualcall:
    case OP_virtualicall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_interfaceicall:
    case OP_customcall:
    case OP_polymorphiccall:
    case OP_callassigned:
    case OP_icallassigned:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_intrinsiccallwithtype:
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_intrinsiccallwithtypeassigned:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_syncenter:
    case OP_syncexit: {
      NaryMeStmt *narymestmt = static_cast<NaryMeStmt *>(mestmt);
      MapleVector<MeExpr *> &opnds = narymestmt->opnds;
      for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
        worklist.push_front(*it);
      }
      MarkMuListNeeded(*(narymestmt->GetMuList()));
      break;
    }
    default:
      CHECK_FATAL(false, "unexpected stmt in HDSE MarkStmtNeeded");
  }
  MarkBBNeeded(mestmt->bb);
}

bool HDSE::ExprNonDeletable(MeExpr *x) {
  if (JAVALANG && kOpcodeInfo.HasSideEffect(x->op)) {
    return true;
  }
  switch (x->meOp) {
    case kMeOpAddrof:
    case kMeOpAddroffunc:
    case kMeOpAddroflabel:
    case kMeOpGcmalloc:
    case kMeOpConst:
    case kMeOpConststr:
    case kMeOpConststr16:
    case kMeOpSizeoftype:
      return false;
    case kMeOpReg: {
      RegMeExpr *r = static_cast<RegMeExpr *>(x);
      return (r->GetPregIdx() == -kSregThrownval);
    }
    case kMeOpVar: {
      VarMeExpr *v = static_cast<VarMeExpr *>(x);
      return v->IsVolatile(ssaTab);
    }
    case kMeOpIvar: {
      IvarMeExpr *iv = static_cast<IvarMeExpr *>(x);
      return iv->IsVolatile() || ExprNonDeletable(iv->base);
    }
    case kMeOpOp: {
      if (x->op == OP_gcmallocjarray) {
        return true;
      }
      OpMeExpr *o = static_cast<OpMeExpr *>(x);
      return ExprNonDeletable(o->GetOpnd(0)) ||
             (o->GetOpnd(1) && (ExprNonDeletable(o->GetOpnd(1)) || (o->GetOpnd(2) && ExprNonDeletable(o->GetOpnd(2)))));
    }
    case kMeOpNary: {
      NaryMeExpr *a = static_cast<NaryMeExpr *>(x);
      if (x->op == OP_intrinsicop) {
        IntrinDesc *intrindesc = &IntrinDesc::intrintable[a->intrinsic];
        if (!intrindesc->HasNoSideEffect()) {
          return true;
        }
      }
      for (uint32 i = 0; i < a->numOpnds; i++)
        if (ExprNonDeletable(a->GetOpnd(i))) {
          return true;
        }
      return false;
    }
    default:
      CHECK_FATAL(false, "unexpected stmt in HDSE ExprNonDeletable()");
  }
  return false;
}

void HDSE::DseProcessBB(BB *bb) {
  if (bb == nullptr) {
    return;
  }
  for (MeStmt *stmt = bb->meStmtList.last; stmt; stmt = stmt->prev) {
    Opcode op = stmt->op;
    if (stmt->isLive) {
      continue;
    }
    switch (op) {
      case OP_dassign: {
        DassignMeStmt *dasgn = static_cast<DassignMeStmt *>(stmt);
        VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(dasgn->GetVarLhs());
        if ((varmeexpr && varmeexpr->IsVolatile(ssaTab)) || ExprNonDeletable(dasgn->rhs) ||
            (dsekeepref && dasgn->Propagated()) || dasgn->wasMayDassign) {
          MarkStmtNeeded(stmt);
        }
        break;
      }
      case OP_regassign: {
        AssignMeStmt *rasgn = static_cast<AssignMeStmt *>(stmt);
        if (ExprNonDeletable(rasgn->rhs)) {
          MarkStmtNeeded(stmt);
        }
        break;
      }
      case OP_maydassign:
        MarkStmtNeeded(stmt);
        break;
      case OP_iassign: {
        IassignMeStmt *iasgn = static_cast<IassignMeStmt *>(stmt);
        IvarMeExpr *ivarmeexpr = dynamic_cast<IvarMeExpr *>(iasgn->lhsVar);
        CHECK_FATAL(ivarmeexpr != nullptr, "unexpected lhs in IassignMeStmt");
        MIRPtrType *ty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iasgn->tyIdx));
        MIRType *pointedclassty = ty->GetPointedType();
        const std::string &tname = GlobalTables::GetStrTable().GetStringFromStrIdx(pointedclassty->nameStrIdx);
        if (ivarmeexpr->IsVolatile() || ivarmeexpr->IsFinal() || ExprNonDeletable(iasgn->lhsVar->base) ||
            ExprNonDeletable(iasgn->rhs)) {
          MarkStmtNeeded(stmt);
        }
        break;
      }
      case OP_goto: {
        if (bb->WontExit() || bb->IsGoto()) {
          // control flow in an infinite loop cannot be changed
          stmt->isLive = true;
          MarkBBNeeded(stmt->bb);
        }
        break;
      }
      case OP_switch:
      case OP_igoto:
      case OP_brtrue:
      case OP_brfalse: {
        UnaryMeStmt *unarystmt = static_cast<UnaryMeStmt *>(stmt);
        if (bb->WontExit() || ExprNonDeletable(unarystmt->opnd)) {
          // control flow in an infinite loop cannot be changed
          stmt->isLive = true;
          worklist.push_front(unarystmt->opnd);
          MarkBBNeeded(stmt->bb);
        }
        break;
      }
      case OP_comment: {
        stmt->isLive = true;
        break;
      }
      case OP_jstry:
      case OP_jscatch:
      case OP_finally:
      case OP_endtry:
      case OP_cleanuptry:
      case OP_try:
      case OP_javatry:
      case OP_cpptry:
      case OP_catch:
      case OP_javacatch:
      case OP_cppcatch:
      case OP_membaracquire:
      case OP_membarrelease:
      case OP_membarstoreload:
      case OP_membarstorestore: {
        // these statements are needed anyway
        stmt->isLive = true;
        MarkBBNeeded(stmt->bb);
        break;
      }
      case OP_gosub:
      case OP_retsub: {
        stmt->isLive = true;
        WithMuMeStmt *mumestmt = static_cast<WithMuMeStmt *>(stmt);
        MarkMuListNeeded(mumestmt->muList);
        MarkBBNeeded(stmt->bb);
        break;
      }
      case OP_throw: {
        stmt->isLive = true;
        ThrowMeStmt *thrmestmt = static_cast<ThrowMeStmt *>(stmt);
        worklist.push_front(thrmestmt->opnd);
        MarkMuListNeeded(thrmestmt->muList);
        MarkBBNeeded(stmt->bb);
        break;
      }
      case OP_incref:
      case OP_decref:
      case OP_decrefreset:
      case OP_assertnonnull:
      case OP_eval:
      case OP_free: {
        stmt->isLive = true;
        // mark opnds as needed;
        worklist.push_front(static_cast<UnaryMeStmt *>(stmt)->opnd);
        MarkBBNeeded(stmt->bb);
        break;
      }
      case OP_syncenter:
      case OP_syncexit:
      case OP_return: {
        stmt->isLive = true;
        NaryMeStmt *retmestmt = static_cast<NaryMeStmt *>(stmt);
        MapleVector<MeExpr *> &opnds = retmestmt->opnds;
        for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
          worklist.push_front(*it);
        }
        MarkMuListNeeded(*(retmestmt->GetMuList()));
        MarkBBNeeded(stmt->bb);
        break;
      }
      case OP_call:
      case OP_virtualcall:
      case OP_virtualicall:
      case OP_superclasscall:
      case OP_interfacecall:
      case OP_interfaceicall:
      case OP_customcall:
      case OP_polymorphiccall:
      case OP_icall:
      case OP_callassigned:
      case OP_virtualcallassigned:
      case OP_virtualicallassigned:
      case OP_superclasscallassigned:
      case OP_interfacecallassigned:
      case OP_interfaceicallassigned:
      case OP_customcallassigned:
      case OP_polymorphiccallassigned:
      case OP_icallassigned: {
        MarkStmtNeeded(stmt);
        break;
      }
      case OP_intrinsiccall:
      case OP_xintrinsiccall:
      case OP_intrinsiccallwithtype:
      case OP_intrinsiccallassigned:
      case OP_xintrinsiccallassigned:
      case OP_intrinsiccallwithtypeassigned: {
        IntrinsiccallMeStmt *intrinmestmt = static_cast<IntrinsiccallMeStmt *>(stmt);
        MIRIntrinsicID intrinsic = intrinmestmt->intrinsic;
        IntrinDesc *intrindesc = &IntrinDesc::intrintable[intrinsic];
        if (!intrindesc->IsJsOp()) {
          MarkStmtNeeded(stmt);
        }
        break;
      }
      default:
        CHECK_FATAL(false, "NYI HDSE stmt");
        break;
    }
  }
}

void HDSE::DseProcess() {
  for (int32 i = bbVec.size() - 1; i >= 0; i--) {
    BB *bb = bbVec[i];
    DseProcessBB(bb);
  }
  if (IsLfo()) {
    ProcessWhileInfos();
  }
  while (!worklist.empty()) {
    MeExpr *meexpr = worklist.front();
    worklist.pop_front();
    MarkExprNeeded(meexpr);
  }
}

void HDSE::Update() {
  for (BB *bb : bbVec) {
    if (bb == nullptr) {
      continue;
    }
    UpdateStmt(bb);
  }
}

}  // namespace maple
