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

/// Copyright [year] <Copyright Owner>
#include "mir_lower.h"
#include "vtable_impl.h"

#define DO_LT_0_CHECK 1

namespace maple {

BlockNode *MIRLower::LowerIfStmt(IfStmtNode *ifstmt, bool recursive) {
  bool thenempty = ifstmt->thenPart == nullptr || ifstmt->thenPart->GetFirst() == nullptr;
  bool elseempty = ifstmt->elsePart == nullptr || ifstmt->elsePart->GetFirst() == nullptr;
  if (recursive) {
    if (!thenempty) {
      ifstmt->thenPart = LowerBlock(ifstmt->thenPart);
    }
    if (!elseempty) {
      ifstmt->elsePart = LowerBlock(ifstmt->elsePart);
    }
  }

  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  if (thenempty && elseempty) {
    // generate EVAL <cond> statement
    UnaryStmtNode *evalstmt = mirModule.CurFuncCodeMemPool()->New<UnaryStmtNode>(OP_eval);
    evalstmt->uOpnd = ifstmt->uOpnd;
    evalstmt->srcPosition = ifstmt->srcPosition;
    blk->AddStatement(evalstmt);
  } else if (elseempty) {
    // brfalse <cond> <endlabel>
    // <thenPart>
    // label <endlabel>
    CondGotoNode *brfalsestmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(OP_brfalse);
    brfalsestmt->uOpnd = ifstmt->uOpnd;
    brfalsestmt->srcPosition = ifstmt->srcPosition;
    LabelIdx lidx = mirModule.CurFunction()->labelTab->CreateLabel();
    mirModule.CurFunction()->labelTab->AddToStringLabelMap(lidx);
    brfalsestmt->offset = lidx;
    blk->AddStatement(brfalsestmt);

    blk->AppendStatementsFromBlock(ifstmt->thenPart);

    LabelNode *labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
    labstmt->labelIdx = lidx;
    blk->AddStatement(labstmt);
  } else if (thenempty) {
    // brtrue <cond> <endlabel>
    // <elsePart>
    // label <endlabel>
    CondGotoNode *brtruestmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(OP_brtrue);
    brtruestmt->uOpnd = ifstmt->uOpnd;
    brtruestmt->srcPosition = ifstmt->srcPosition;
    LabelIdx lidx = mirModule.CurFunction()->labelTab->CreateLabel();
    mirModule.CurFunction()->labelTab->AddToStringLabelMap(lidx);
    brtruestmt->offset = lidx;
    blk->AddStatement(brtruestmt);

    blk->AppendStatementsFromBlock(ifstmt->elsePart);

    LabelNode *labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
    labstmt->labelIdx = lidx;
    blk->AddStatement(labstmt);
  } else {
    // brfalse <cond> <elselabel>
    // <thenPart>
    // goto <endlabel>
    // label <elselabel>
    // <elsePart>
    // label <endlabel>
    CondGotoNode *brfalsestmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(OP_brfalse);
    brfalsestmt->uOpnd = ifstmt->uOpnd;
    brfalsestmt->srcPosition = ifstmt->srcPosition;
    LabelIdx lidx = mirModule.CurFunction()->labelTab->CreateLabel();
    mirModule.CurFunction()->labelTab->AddToStringLabelMap(lidx);
    brfalsestmt->offset = lidx;
    blk->AddStatement(brfalsestmt);

    blk->AppendStatementsFromBlock(ifstmt->thenPart);
    bool fallthruFromThen = !BlockNoFallThru(ifstmt->thenPart);
    LabelIdx gotolidx = 0;

    if (fallthruFromThen) {
      GotoNode *gotostmt = mirModule.CurFuncCodeMemPool()->New<GotoNode>(OP_goto);
      gotolidx = mirModule.CurFunction()->labelTab->CreateLabel();
      mirModule.CurFunction()->labelTab->AddToStringLabelMap(gotolidx);
      gotostmt->offset = gotolidx;
      blk->AddStatement(gotostmt);
    }

    LabelNode *labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
    labstmt->labelIdx = lidx;
    blk->AddStatement(labstmt);

    blk->AppendStatementsFromBlock(ifstmt->elsePart);

    if (fallthruFromThen) {
      labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
      labstmt->labelIdx = gotolidx;
      blk->AddStatement(labstmt);
    }
  }
  return blk;
}

//     while <cond> <body>
// is lowered to:
//     brfalse <cond> <endlabel>
//   label <bodylabel>
//     <body>
//     brtrue <cond> <bodylabel>
//   label <endlabel>
BlockNode *MIRLower::LowerWhileStmt(WhileStmtNode *whilestmt) {
  whilestmt->body = LowerBlock(whilestmt->body);
  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();

  CondGotoNode *brfalsestmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(OP_brfalse);
  brfalsestmt->uOpnd = whilestmt->uOpnd;
  brfalsestmt->srcPosition = whilestmt->srcPosition;
  LabelIdx lidx = mirModule.CurFunction()->labelTab->CreateLabel();
  mirModule.CurFunction()->labelTab->AddToStringLabelMap(lidx);
  brfalsestmt->offset = lidx;
  blk->AddStatement(brfalsestmt);

  LabelIdx bodylidx = mirModule.CurFunction()->labelTab->CreateLabel();
  mirModule.CurFunction()->labelTab->AddToStringLabelMap(bodylidx);
  LabelNode *labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
  labstmt->labelIdx = bodylidx;
  blk->AddStatement(labstmt);
  CHECK_FATAL(whilestmt->body, "null ptr check");
  blk->AppendStatementsFromBlock(whilestmt->body);

  CondGotoNode *brtruestmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(OP_brtrue);
  brtruestmt->uOpnd = whilestmt->uOpnd->CloneTree(&mirModule);
  brtruestmt->offset = bodylidx;
  blk->AddStatement(brtruestmt);

  labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
  labstmt->labelIdx = lidx;
  blk->AddStatement(labstmt);
  return blk;
}

//    doloop <do-var>(<start-expr>,<cont-expr>,<incr-amt>) {<body-stmts>}
// is lowered to:
//     dassign <do-var> (<start-expr>)
//     brfalse <cond-expr> <endlabel>
//   label <bodylabel>
//     <body-stmts>
//     dassign <do-var> (<incr-amt>)
//     brtrue <cond-expr>  <bodylabel>
//   label <endlabel>
BlockNode *MIRLower::LowerDoloopStmt(DoloopNode *doloop) {
  doloop->doBody = LowerBlock(doloop->doBody);
  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  if (doloop->isPreg) {
    PregIdx regIdx = (PregIdx)doloop->doVarStIdx.FullIdx();
    MIRPreg *mpreg = mirModule.CurFunction()->pregTab->PregFromPregIdx(regIdx);
    PrimType primType = mpreg->primType;
    ASSERT(primType != kPtyInvalid, "");

    RegassignNode *startRegassign = mirModule.CurFuncCodeMemPool()->New<RegassignNode>();
    startRegassign->regIdx = regIdx;
    startRegassign->primType = primType;
    startRegassign->uOpnd = doloop->startExpr;
    startRegassign->srcPosition = doloop->srcPosition;
    blk->AddStatement(startRegassign);
  } else {
    DassignNode *startDassign = mirModule.CurFuncCodeMemPool()->New<DassignNode>();
    startDassign->stIdx = doloop->doVarStIdx;

    startDassign->SetRhs(doloop->startExpr);
    startDassign->srcPosition = doloop->srcPosition;
    blk->AddStatement(startDassign);
  }

  CondGotoNode *brfalsestmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(OP_brfalse);
  brfalsestmt->uOpnd = doloop->condExpr;
  LabelIdx lidx = mirModule.CurFunction()->labelTab->CreateLabel();
  mirModule.CurFunction()->labelTab->AddToStringLabelMap(lidx);
  brfalsestmt->offset = lidx;
  blk->AddStatement(brfalsestmt);

  LabelIdx bodylidx = mirModule.CurFunction()->labelTab->CreateLabel();
  mirModule.CurFunction()->labelTab->AddToStringLabelMap(bodylidx);
  LabelNode *labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();

  labstmt->labelIdx = bodylidx;
  blk->AddStatement(labstmt);
  CHECK_FATAL(doloop->doBody, "null ptr check ");
  blk->AppendStatementsFromBlock(doloop->doBody);

  if (doloop->isPreg) {
    PregIdx regIdx = (PregIdx)doloop->doVarStIdx.FullIdx();
    MIRPreg *mpreg = mirModule.CurFunction()->pregTab->PregFromPregIdx(regIdx);
    PrimType dovarPtyp = mpreg->primType;
    ASSERT(dovarPtyp != kPtyInvalid, "");

    RegreadNode *readDovar = mirModule.CurFuncCodeMemPool()->New<RegreadNode>();
    readDovar->regIdx = regIdx;
    readDovar->primType = dovarPtyp;

    BinaryNode *add = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_add, dovarPtyp, doloop->incrExpr, readDovar);
    RegassignNode *endRegassign = mirModule.CurFuncCodeMemPool()->New<RegassignNode>();
    endRegassign->regIdx = regIdx;
    endRegassign->primType = dovarPtyp;
    endRegassign->uOpnd = add;
    blk->AddStatement(endRegassign);
  } else {
    MIRSymbol *dovarSym = mirModule.CurFunction()->GetLocalOrGlobalSymbol(doloop->doVarStIdx);
    PrimType dovarPtyp = dovarSym->GetType()->GetPrimType();
    DreadNode *readDovar = mirModule.CurFuncCodeMemPool()->New<DreadNode>(OP_dread, dovarPtyp, doloop->doVarStIdx, 0);
    BinaryNode *add = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_add, dovarPtyp, doloop->incrExpr, readDovar);
    DassignNode *endDassign = mirModule.CurFuncCodeMemPool()->New<DassignNode>();
    endDassign->stIdx = doloop->doVarStIdx;
    endDassign->SetRhs(add);

    blk->AddStatement(endDassign);
  }

  CondGotoNode *brtruestmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(OP_brtrue);
  brtruestmt->uOpnd = doloop->condExpr->CloneTree(&mirModule);
  brtruestmt->offset = bodylidx;
  blk->AddStatement(brtruestmt);

  labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
  labstmt->labelIdx = lidx;
  blk->AddStatement(labstmt);
  return blk;
}

//     dowhile <body> <cond>
// is lowered to:
//   label <bodylabel>
//     <body>
//     brtrue <cond> <bodylabel>
BlockNode *MIRLower::LowerDowhileStmt(WhileStmtNode *dowhilestmt) {
  dowhilestmt->body = LowerBlock(dowhilestmt->body);
  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();

  LabelIdx lidx = mirModule.CurFunction()->labelTab->CreateLabel();
  mirModule.CurFunction()->labelTab->AddToStringLabelMap(lidx);
  LabelNode *labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
  labstmt->labelIdx = lidx;
  blk->AddStatement(labstmt);
  CHECK_FATAL(dowhilestmt->body, "null ptr check ");
  blk->AppendStatementsFromBlock(dowhilestmt->body);

  CondGotoNode *brtruestmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(OP_brtrue);
  brtruestmt->uOpnd = dowhilestmt->uOpnd;
  brtruestmt->offset = lidx;
  blk->AddStatement(brtruestmt);

  return blk;
}

BlockNode *MIRLower::LowerBlock(BlockNode *block) {
  BlockNode *newblk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  BlockNode *tmp = nullptr;
  if (!block->GetFirst()) {
    return newblk;
  }
  StmtNode *nextstmt = block->GetFirst();
  do {
    StmtNode *stmt = nextstmt;
    nextstmt = stmt->GetNext();
    switch (stmt->op) {
      case OP_if:
        tmp = LowerIfStmt(static_cast<IfStmtNode *>(stmt));
        CHECK_FATAL(tmp, "null ptr check");
        newblk->AppendStatementsFromBlock(tmp);
        break;
      case OP_while:
        newblk->AppendStatementsFromBlock(LowerWhileStmt(static_cast<WhileStmtNode *>(stmt)));
        break;
      case OP_dowhile:
        newblk->AppendStatementsFromBlock(LowerDowhileStmt(static_cast<WhileStmtNode *>(stmt)));
        break;
      case OP_doloop:

        newblk->AppendStatementsFromBlock(LowerDoloopStmt(static_cast<DoloopNode *>(stmt)));
        break;
      case OP_block:
        tmp = LowerBlock(static_cast<BlockNode *>(stmt));
        CHECK_FATAL(tmp, "null ptr check ");
        newblk->AppendStatementsFromBlock(tmp);
        break;
      default:
        newblk->AddStatement(stmt);
        break;
    }
  } while (nextstmt);
  return newblk;
}

// for lowering OP_cand and OP_cior that are top level operators in the
// condition operand of OP_brfalse and OP_brtrue
void MIRLower::LowerBrCond(BlockNode *block) {
  if (!block->GetFirst()) {
    return;
  }
  StmtNode *nextstmt = block->GetFirst();
  do {
    StmtNode *stmt = nextstmt;
    nextstmt = stmt->GetNext();
    if (stmt->IsCondBr()) {
      CondGotoNode *condgoto = static_cast<CondGotoNode *>(stmt);
      if (condgoto->uOpnd->op == OP_cand || condgoto->uOpnd->op == OP_cior) {
        BinaryNode *cond = static_cast<BinaryNode *>(condgoto->uOpnd);
        if ((stmt->op == OP_brfalse && cond->op == OP_cand) || (stmt->op == OP_brtrue && cond->op == OP_cior)) {
          // short-circuit target label is same as original condgoto stmt
          condgoto->uOpnd = cond->bOpnd[0];
          CondGotoNode *newcondgoto = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(stmt->op);
          newcondgoto->uOpnd = cond->bOpnd[1];
          newcondgoto->offset = condgoto->offset;
          block->InsertAfter(condgoto, newcondgoto);
          nextstmt = stmt;  // so it will be re-processed if another cand/cior
        } else {            // short-circuit target is next statement
          LabelIdx lidx;
          LabelNode *labstmt = nullptr;
          if (nextstmt->op == OP_label) {
            labstmt = static_cast<LabelNode *>(nextstmt);
            lidx = labstmt->labelIdx;
          } else {
            lidx = mirModule.CurFunction()->labelTab->CreateLabel();
            mirModule.CurFunction()->labelTab->AddToStringLabelMap(lidx);
            labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
            labstmt->labelIdx = lidx;
            block->InsertAfter(condgoto, labstmt);
          }
          CondGotoNode *newcondgoto =
            mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(stmt->op == OP_brfalse ? OP_brtrue : OP_brfalse);
          newcondgoto->uOpnd = cond->bOpnd[0];
          newcondgoto->offset = lidx;
          block->InsertBefore(condgoto, newcondgoto);
          condgoto->uOpnd = cond->bOpnd[1];
          nextstmt = newcondgoto;  // so it will be re-processed if another cand/cior
        }
      }
    }
  } while (nextstmt);
  return;
}

void MIRLower::LowerFunc(MIRFunction *func) {
  mirModule.SetCurFunction(func);
  if (IsLowerExpandArray()) {
    ExpandArraymrt(func);
  }
  BlockNode *origbody = func->body;
  BlockNode *newbody = LowerBlock(origbody);
  LowerBrCond(newbody);
  func->body = newbody;
  return;
}

IfStmtNode *MIRLower::ExpandArraymrtifblock(IfStmtNode *node) {
  if (node->thenPart) {
    node->thenPart = ExpandArraymrtblock(node->thenPart);
  }
  if (node->elsePart) {
    node->elsePart = ExpandArraymrtblock(node->elsePart);
  }
  return node;
}

WhileStmtNode *MIRLower::ExpandArraymrtwhileblock(WhileStmtNode *node) {
  if (node->body) {
    node->body = ExpandArraymrtblock(node->body);
  }
  return node;
}

DoloopNode *MIRLower::ExpandArraymrtdoloopblock(DoloopNode *node) {
  if (node->doBody) {
    node->doBody = ExpandArraymrtblock(node->doBody);
  }
  return node;
}

ForeachelemNode *MIRLower::ExpandArraymrtForeachelemblock(ForeachelemNode *node) {
  if (node->loopBody) {
    node->loopBody = ExpandArraymrtblock(node->loopBody);
  }
  return node;
}

void MIRLower::AddArraymrtmpl(BaseNode *exp, BlockNode *newblk) {
  MIRModule &mod = mirModule;
  MIRBuilder *mirbuilder = mod.mirBuilder;

  for (int32 i = 0; i < exp->NumOpnds(); i++) {
    AddArraymrtmpl(exp->Opnd(i), newblk);
  }

  if (exp->op == OP_array) {
    ArrayNode *arrayNode = static_cast<ArrayNode *>(exp);
    if (arrayNode->boundsCheck) {
      BaseNode *arraddr = arrayNode->Opnd(0);
      BaseNode *index = arrayNode->Opnd(1);
      MIRType *indextyp = GlobalTables::GetTypeTable().GetPrimType(index->primType);
      UnaryStmtNode *nullcheck = mirbuilder->CreateStmtUnary(OP_assertnonnull, arraddr);
      newblk->AddStatement(nullcheck);

#if DO_LT_0_CHECK
      ConstvalNode *indexzero = mod.mirBuilder->GetConstUInt32(0);

      CompareNode *lesszero =
          mirbuilder->CreateExprCompare(OP_lt, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetUInt32(), index, indexzero);
#endif
      MIRType *infolentyp = GlobalTables::GetTypeTable().GetInt32();

      MapleVector<BaseNode*> ops(mod.mirBuilder->mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
      ops.push_back(arraddr);
      BaseNode *arrlen = mod.mirBuilder->CreateExprIntrinsicop(INTRN_JAVA_ARRAY_LENGTH, infolentyp, ops);
      BaseNode *cpmindex = index;
      if (arrlen->primType != index->primType) {
        cpmindex = mirbuilder->CreateExprTypeCvt(OP_cvt, infolentyp, indextyp, index);
      }
      CompareNode *largelen =
          mirbuilder->CreateExprCompare(OP_ge, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetUInt32(), cpmindex, arrlen);
      // maybe should use cior
#if DO_LT_0_CHECK
      BinaryNode *indexcon = mirbuilder->CreateExprBinary(OP_lior, GlobalTables::GetTypeTable().GetUInt1(), lesszero, largelen);
#endif
#if DO_LT_0_CHECK
      ops.clear();
      ops.push_back(indexcon);
      IntrinsiccallNode *bdtrinsiccall = mirbuilder->CreateStmtIntrinsicCall(INTRN_MPL_BOUNDARY_CHECK, ops);
#else
      ops.clear();
      ops.push_back(largelen);
      IntrinsiccallNode *bdtrinsiccall = mirbuilder->CreateStmtIntrinsicCall(INTRN_MPL_BOUNDARY_CHECK, ops);
#endif
      newblk->AddStatement(bdtrinsiccall);
    }

  } else if (0 && exp->op == OP_intrinsicop &&
             static_cast<IntrinsicopNode *>(exp)->intrinsic == INTRN_JAVA_ARRAY_LENGTH) {
    IntrinsicopNode *getlennode = static_cast<IntrinsicopNode *>(exp);
    BaseNode *arraddr = getlennode->Opnd(0);
    MIRType *addrtyp = GlobalTables::GetTypeTable().GetPrimType(arraddr->primType);

    ConstvalNode *exprconst = mod.CurFuncCodeMemPool()->New<ConstvalNode>();
    exprconst->primType = arraddr->primType;
    MIRConst *constVal = mod.memPool->New<MIRIntConst>(0, addrtyp);
    exprconst->constVal = constVal;

    BaseNode *nullcon = mod.mirBuilder->CreateExprBinary(OP_eq, GlobalTables::GetTypeTable().GetUInt1(), arraddr, exprconst);
    static_cast<CompareNode *>(nullcon)->opndType = arraddr->primType;
    IfStmtNode *ifstat = mod.mirBuilder->CreateStmtIf(nullcon);

    MapleVector<BaseNode *> opnds(mod.CurFuncCodeMemPoolAllocator()->Adapter());
    MIRFunction *fn = mod.mirBuilder->GetOrCreateFunction("MCC_ThrowNullPointerException", TyIdx(PTY_void));
    CallNode *checkStmt = mod.mirBuilder->CreateStmtCall(fn->puIdx, opnds);
    ifstat->thenPart->AddStatement(checkStmt);
    newblk->AddStatement(ifstat);
  }
}

BlockNode *MIRLower::ExpandArraymrtblock(BlockNode *block) {
  BlockNode *newblk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  if (!block->GetFirst()) {
    return newblk;
  }
  StmtNode *nextstmt = block->GetFirst();
  do {
    StmtNode *stmt = nextstmt;
    nextstmt = stmt->GetNext();
    switch (stmt->op) {
      case OP_if:
        newblk->AddStatement(ExpandArraymrtifblock(static_cast<IfStmtNode *>(stmt)));
        break;
      case OP_while:
        newblk->AddStatement(ExpandArraymrtwhileblock(static_cast<WhileStmtNode *>(stmt)));
        break;
      case OP_dowhile:
        newblk->AddStatement(ExpandArraymrtwhileblock(static_cast<WhileStmtNode *>(stmt)));
        break;
      case OP_doloop:
        newblk->AddStatement(ExpandArraymrtdoloopblock(static_cast<DoloopNode *>(stmt)));
        break;
      case OP_foreachelem:
        newblk->AddStatement(ExpandArraymrtForeachelemblock(static_cast<ForeachelemNode *>(stmt)));
        break;
      case OP_block:
        newblk->AddStatement(ExpandArraymrtblock(static_cast<BlockNode *>(stmt)));
        break;
      default:
        AddArraymrtmpl(stmt, newblk);
        newblk->AddStatement(stmt);
        break;
    }
  } while (nextstmt);
  return newblk;
}

void MIRLower::ExpandArraymrt(MIRFunction *func) {
  if (ShouldOptArrayMrt(func)) {
    BlockNode *origbody = func->body;
    BlockNode *newbody = ExpandArraymrtblock(origbody);
    func->body = newbody;
  }
}

const std::set<std::string> MIRLower::kSetArrayHotFunc = {
};

bool MIRLower::ShouldOptArrayMrt(const MIRFunction *func) {
  if (MIRLower::kSetArrayHotFunc.find(func->GetName()) != MIRLower::kSetArrayHotFunc.end()) {
    return true;
  }

  return false;
}

}  // namespace maple
