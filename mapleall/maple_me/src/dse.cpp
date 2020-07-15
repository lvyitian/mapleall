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

#include "ssa_mir_nodes.h"
#include "ver_symbol.h"
#include "ssa.h"
#include "dse.h"
#include "opcode_info.h"
#include "mir_function.h"

#ifdef DEBUG
#define RETAIN_COMMENT true
#else
#define RETAIN_COMMENT true
#endif

namespace maple {
void DSE::MarkLive(BaseNode *mirnode) {
  Opcode op = mirnode->op;
  switch (op) {
    case OP_dread: {
      AddrofSSANode *addofnode = static_cast<AddrofSSANode *>(mirnode);
      VersionSt *vsym = addofnode->ssaVar;
      worklist.push_front(vsym);
      break;
    }
    case OP_regread: {
      RegreadSSANode *regnode = static_cast<RegreadSSANode *>(mirnode);
      VersionSt *vsym = regnode->ssaVar;
      worklist.push_front(vsym);
      break;
    }
    case OP_iread: {
      IreadSSANode *ireadnode = static_cast<IreadSSANode *>(mirnode);
      VersionSt *vsym = ireadnode->mayUse.opnd;
      CHECK_FATAL(vsym != nullptr, "DSE::MarkLive: iread has no mayUse opnd");
      worklist.push_front(vsym);
      if (!vsym->IsInitVersion()) {
        auto maydeflist = SSAGenericGetMayDefsFromVersionSt(vsym, stmtsSSAPart);
        if (maydeflist != nullptr) {
          auto it = maydeflist->begin();
          for (; it != maydeflist->end(); it++) {
            worklist.push_front(it->second.result);
          }
        }
      }
      MarkLive(mirnode->Opnd(0));
      break;
    }
    default: {
      for (int32_t i = 0; i < mirnode->NumOpnds(); i++) {
        MarkLive(mirnode->Opnd(i));
      }
      break;
    }
  }
}

void DSE::MarkVst(VersionSt *vst) {
  if (vst->IsLive() || vst->version == INIT_VERSION) {
    return;
  }
  BB *dfBb = vst->GetDefBB();
  if (vst->defType == VersionSt::kDassign) {
    vst->MarkLive();
    DassignNode *dassign = vst->defStmt.dassign;
    if (StmtRequired(dassign)) {
      return;
    }
    MarkStmt(dassign, dfBb);
    for (int32_t i = 0; i < dassign->NumOpnds(); i++) {
      MarkLive(dassign->Opnd(i));
    }
  } else if (vst->defType == VersionSt::kRegassign) {
    vst->MarkLive();
    RegassignNode *regassign = vst->defStmt.regassign;
    if (StmtRequired(regassign)) {
      return;
    }
    MarkStmt(regassign, dfBb);
    MarkLive(regassign->Opnd(0));
  } else if (vst->defType == VersionSt::kPhi) {
    PhiNode *phi = vst->defStmt.phi;
    ASSERT(phi->result == vst, "MarkVst: wrong corresponding version st in phi");
    phi->result->MarkLive();
    MarkBBLive(dfBb);
    for (uint32_t i = 0; i < phi->phiOpnds.size(); i++) {
      VersionSt *vsym = phi->phiOpnds[i];
      worklist.push_front(vsym);
    }
  } else if (vst->defType == VersionSt::kMayDef) {
    MayDefNode *mdef = vst->defStmt.mayDef;
    ASSERT(mdef->result == vst, "MarkVst: wrong corresponding version st in mayDef");
    mdef->MarkRequired();
    VersionSt *vsym = mdef->opnd;
    MarkStmt(mdef->stmt, dfBb);
    worklist.push_front(vsym);
  } else {
    // vst->defType == VersionSt::MustDef
    MustDefNode *mustDef = vst->defStmt.mustDef;
    ASSERT(mustDef->result == vst, "MarkVst: wrong corresponding version st in mustDef");
    mustDef->MarkRequired();
    MarkStmt(mustDef->stmt, dfBb);
  }
}

void DSE::MarkBBLive(BB *bb) {
  if (!bb_required[bb->id.idx]) {
    bb_required[bb->id.idx] = true;
    StmtNode *lastStmt = bb->stmtNodeList.last;
    if (lastStmt != nullptr) {
      // if bb's last stmt is a branch instruction, it is also needed
      Opcode op = lastStmt->op;
      if (lastStmt->IsCondBr() || op == OP_goto || op == OP_switch) {
        MarkStmt(lastStmt, bb);
      }
    }
    // make last stmt in control-dependent BBs live
    CHECK(bb->id.idx < pdom->pdomFrontier.size(), "index out of range in DSE::MarkBBLive ");
    MapleSet<BBId> *pdomFrontier = &pdom->pdomFrontier[bb->id.idx];
    for (MapleSet<BBId>::iterator bbit = pdomFrontier->begin(); bbit != pdomFrontier->end(); bbit++) {
      BB *cdBb = GetBB((*bbit));
      CHECK_FATAL(cdBb != nullptr, "cd_bb is null in DSE::MarkBBLive");
      if (cdBb == bb) {
        continue;
      }
      StmtNode *lastStmt = cdBb->stmtNodeList.last;
      if (lastStmt != nullptr) {
        Opcode op = lastStmt->op;
        if (lastStmt->IsCondBr() || op == OP_goto || op == OP_switch) {
          MarkStmt(lastStmt, cdBb);
        }
      }
    }
    // make unconditional goto's in bb's predecessors live
    for (MapleVector<BB *>::iterator predIt = bb->pred.begin(); predIt != bb->pred.end(); predIt++) {
      BB *predBb = *predIt;
      if (predBb == bb || predBb->IsEmpty()) {
        continue;
      }
      if (predBb->stmtNodeList.last->op == OP_goto) {
        MarkStmt(predBb->stmtNodeList.last, predBb);
      }
    }
  }
}

void DSE::MarkStmt(StmtNode *stmt, BB *bb) {
  if (StmtRequired(stmt)) {
    return;
  }

  SetStmtRequired(stmt);
  if (RETAIN_COMMENT) {
    // mark comment preceding it as required also
    StmtNode *prev = stmt->GetPrev();
    if (prev && prev->op == OP_comment) {
      SetStmtRequired(prev);
    }
  }
  for (int32_t i = 0; i < stmt->NumOpnds(); i++) {
    MarkLive(stmt->Opnd(i));
  }
  Opcode op = stmt->op;
  if (kOpcodeInfo.HasSSAUse(op)) {
    MapleMap<OStIdx, MayUseNode> *mayUseNodes = SSAGenericGetMayUseNode(stmt, stmtsSSAPart);
    for (std::pair<OStIdx, MayUseNode> mapitem : *mayUseNodes) {
      MayUseNode &mayUse = mapitem.second;
      worklist.push_front(mayUse.GetOpnd());
    }
  }
  MarkBBLive(bb);
}

bool DSE::ExprNonDeletable(BaseNode *expr) {
  if (kOpcodeInfo.HasSideEffect(expr->op)) {
    return true;
  }

  switch (expr->op) {
    case OP_dread: {
      AddrofNode *dread = static_cast<AddrofNode *>(expr);
      MIRSymbol *sym = mirModule->CurFunction()->GetLocalOrGlobalSymbol(dread->stIdx);
      if (sym->IsVolatile()) {
        return true;
      }
      MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
      if (dread->fieldID == 0) {
        return ty->HasVolatileField();
      }
      return static_cast<MIRStructType *>(ty)->IsFieldVolatile(dread->fieldID);
    }
    case OP_iread: {
      IreadNode *iread = static_cast<IreadNode *>(expr);
      MIRPtrType *ty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iread->tyIdx));
      MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ty->pointedTyIdx);
      bool typevolatile = false;
      if (iread->fieldID == 0) {
        typevolatile = pointedty->HasVolatileField();
      } else {
        typevolatile = static_cast<MIRStructType *>(pointedty)->IsFieldVolatile(iread->fieldID);
      }
      return typevolatile || ExprNonDeletable(iread->uOpnd);
    }
    case OP_regread: {
      RegreadNode *regread = static_cast<RegreadNode *>(expr);
      if (regread->regIdx == -kSregThrownval) {
        return true;
      }
      break;
    }
    case OP_intrinsicop: {
      IntrinsicopNode *innode = static_cast<IntrinsicopNode *>(expr);
      IntrinDesc *intrindesc = &IntrinDesc::intrintable[innode->intrinsic];
      if (!intrindesc->HasNoSideEffect()) {
        return true;
      }
      break;
    }
    case OP_gcmallocjarray:
    case OP_gcpermallocjarray: {
      // may throw exception
      return true;
    }
    default: {
      for (int32 i = 0; i < expr->NumOpnds(); i++) {
        if (ExprNonDeletable(expr->Opnd(i))) {
          return true;
        }
      }
      break;
    }
  }
  return false;
}

}  // namespace maple
