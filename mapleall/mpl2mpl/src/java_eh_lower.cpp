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

#include "java_eh_lower.h"
#include "mir_function.h"
#include "mir_builder.h"
#include "global_tables.h"
#include "option.h"

namespace maple {

// do exception handling runtime insertion of runtime function call
// scan the entire function body once to lookup expression that
// could potentially raise exceptions such as division,
// for example:
// if we have x = a/b
// and we don't know the value of b during compile time
// then we will insert the test for exception:
// if (b == 0)
//  call MCC_ThrowArithmeticException()
// x = a/b

BaseNode *JavaEHLowerer::DoLowerDiv(BinaryNode *expr, BlockNode *blknode) {
  PrimType ptype = expr->primType;
  MIRBuilder *mirbuilder = mirModule->mirBuilder;
  MIRFunction *func = mirModule->CurFunction();
  if (IsPrimitiveInteger(ptype)) {
    // store divopnd to a tmp st if not a leaf node
    BaseNode *divopnd = expr->Opnd(1);
    if (!divopnd->IsLeaf()) {
      std::string opnd1name(strDivOpnd);
      opnd1name.append(std::to_string(divSTIndex));
      if (useRegTmp) {
        PregIdx pregidx = func->pregTab->CreatePreg(ptype);
        RegassignNode *regassdivnode = mirbuilder->CreateStmtRegassign(ptype, pregidx, divopnd);
        blknode->AddStatement(regassdivnode);
        divopnd = mirbuilder->CreateExprRegread(ptype, pregidx);
      } else {
        MIRSymbol *divopndst = mirbuilder->CreateLocalDecl(opnd1name, GlobalTables::GetTypeTable().GetPrimType(ptype));
        DassignNode *dssdivnode = mirbuilder->CreateStmtDassign(divopndst, 0, divopnd);
        blknode->AddStatement(dssdivnode);
        divopnd = mirbuilder->CreateExprDread(divopndst);
      }
      expr->bOpnd[1] = divopnd;
    }
    BaseNode *retexprnode = nullptr;
    if (useRegTmp) {
      PregIdx respregidx = func->pregTab->CreatePreg(ptype);
      RegassignNode *regassnode = mirbuilder->CreateStmtRegassign(ptype, respregidx, expr);
      blknode->AddStatement(regassnode);
      retexprnode = mirModule->mirBuilder->CreateExprRegread(ptype, respregidx);
    } else {
      std::string resname(strDivRes);
      resname.append(std::to_string(divSTIndex++));

      MIRSymbol *divresst = mirbuilder->CreateLocalDecl(resname, GlobalTables::GetTypeTable().GetPrimType(ptype));
      // put expr result to dssnode
      DassignNode *dssnode = mirbuilder->CreateStmtDassign(divresst, 0, expr);
      blknode->AddStatement(dssnode);
      retexprnode = mirModule->mirBuilder->CreateExprDread(divresst, 0);
    }
    // check if the second operand of the div expression is 0
    // inser if statement for high level ir
    CompareNode *cmpnode =
        mirbuilder->CreateExprCompare(OP_eq, GlobalTables::GetTypeTable().GetInt32(), GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)ptype),
                                      divopnd, mirbuilder->CreateIntConst(0, ptype));
    IfStmtNode *ifstmtnode = mirbuilder->CreateStmtIf(cmpnode);
    blknode->AddStatement(ifstmtnode);

    // call the MCC_ThrowArithmeticException() that will never return
    MapleVector<BaseNode*> ops(mirbuilder->mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
    IntrinsiccallNode *intrincallnode = mirbuilder->CreateStmtIntrinsicCall(INTRN_JAVA_THROW_ARITHMETIC, ops);

    ifstmtnode->thenPart->AddStatement(intrincallnode);

    // make dread from the divresst and return it as new expression for this function
    return retexprnode;
  } else {
    return expr;
  }
}

BaseNode *JavaEHLowerer::DoLowerExpr(BaseNode *expr, BlockNode *curblk) {
  for (int32 i = 0; i < expr->NumOpnds(); i++) {
    expr->SetOpnd(DoLowerExpr(expr->Opnd(i), curblk), i);
  }
  switch (expr->op) {
    case OP_div: {
      return DoLowerDiv(static_cast<BinaryNode *>(expr), curblk);
    }
    case OP_rem: {
      return DoLowerRem(static_cast<BinaryNode *>(expr), curblk);
    }
    default:
      return expr;
  }
}

void JavaEHLowerer::DoLowerBoundaryCheck(IntrinsiccallNode *intrincall, BlockNode *newblk) {
  const int intrincallNopndSize = intrincall->nOpnd.size();
  CHECK_FATAL(intrincallNopndSize > 0, "null ptr check");
  BaseNode *opnd0 = intrincall->nOpnd[0];
  CondGotoNode *brfalsestmt = mirModule->CurFuncCodeMemPool()->New<CondGotoNode>(OP_brfalse);
  brfalsestmt->uOpnd = DoLowerExpr(opnd0, newblk);
  brfalsestmt->srcPosition = intrincall->srcPosition;
  LabelIdx lbidx = mirModule->CurFunction()->labelTab->CreateLabel();
  mirModule->CurFunction()->labelTab->AddToStringLabelMap(lbidx);
  brfalsestmt->offset = lbidx;
  newblk->AddStatement(brfalsestmt);
  LabelNode *labstmt = mirModule->CurFuncCodeMemPool()->New<LabelNode>();
  labstmt->labelIdx = lbidx;
  MIRFunction *fn = mirModule->mirBuilder->GetOrCreateFunction(strMCCThrowArrayIndexOutOfBoundsException, TyIdx(PTY_void));
  MapleVector<BaseNode *> nopnds(mirModule->memPoolAllocator.Adapter());
  CallNode *callstmt = static_cast<CallNode *>(mirModule->mirBuilder->CreateStmtCall(fn->puIdx, nopnds));
  newblk->AddStatement(callstmt);
  newblk->AddStatement(labstmt);
}

BlockNode *JavaEHLowerer::DoLowerBlock(BlockNode *block) {
  BlockNode *newblock = mirModule->CurFuncCodeMemPool()->New<BlockNode>();
  if (!block->GetFirst()) {
    return newblock;
  }
  StmtNode *nextstmt = block->GetFirst();
  do {
    StmtNode *stmt = nextstmt;
    nextstmt = stmt->GetNext();
    stmt->SetNext(nullptr);
    switch (stmt->op) {
      case OP_switch: {
        SwitchNode *switchnode = static_cast<SwitchNode *>(stmt);
        switchnode->switchOpnd = DoLowerExpr(switchnode->switchOpnd, newblock);
        newblock->AddStatement(switchnode);
        break;
      }
      case OP_if: {
        IfStmtNode *ifstmtnode = static_cast<IfStmtNode *>(stmt);
        BlockNode *thenPart = ifstmtnode->thenPart;
        BlockNode *elsePart = ifstmtnode->elsePart;
        ifstmtnode->uOpnd = DoLowerExpr(ifstmtnode->uOpnd, newblock);
        ifstmtnode->thenPart = DoLowerBlock(thenPart);
        if (elsePart) {
          ifstmtnode->elsePart = DoLowerBlock(elsePart);
        }
        newblock->AddStatement(ifstmtnode);
        break;
      }
      case OP_while:
      case OP_dowhile: {
        WhileStmtNode *whilestmtnode = static_cast<WhileStmtNode *>(stmt);
        BaseNode *testopnd = whilestmtnode->Opnd(0);
        whilestmtnode->uOpnd = DoLowerExpr(testopnd, newblock);
        whilestmtnode->body = DoLowerBlock(whilestmtnode->body);
        newblock->AddStatement(whilestmtnode);
        break;
      }
      case OP_doloop: {
        DoloopNode *doloopnode = static_cast<DoloopNode *>(stmt);
        doloopnode->startExpr = DoLowerExpr(doloopnode->startExpr, newblock);
        doloopnode->condExpr = DoLowerExpr(doloopnode->condExpr, newblock);
        doloopnode->incrExpr = DoLowerExpr(doloopnode->incrExpr, newblock);
        doloopnode->doBody = DoLowerBlock(doloopnode->doBody);
        newblock->AddStatement(doloopnode);
        break;
      }
      case OP_block: {
        BlockNode *tmp = DoLowerBlock(static_cast<BlockNode *>(stmt));
        CHECK_FATAL(tmp, "null ptr check");
        newblock->AddStatement(tmp);
        break;
      }
      case OP_throw: {
        UnaryStmtNode *tstmt = static_cast<UnaryStmtNode *>(stmt);
        BaseNode *opnd0 = DoLowerExpr(tstmt->Opnd(0), newblock);
        if (opnd0->op == OP_constval) {
          CHECK_FATAL(IsPrimitiveInteger(opnd0->primType), "must be integer or something wrong");
          MIRIntConst *intconst = static_cast<MIRIntConst *>(static_cast<ConstvalNode *>(opnd0)->constVal);
          CHECK_FATAL(intconst->IsZero(), "can only be zero");
          MIRFunction *fn = mirModule->mirBuilder->GetOrCreateFunction(strMCCThrowNullPointerException, TyIdx(PTY_void));
          fn->SetNoReturn();
          MapleVector<BaseNode *> nopnds(mirModule->memPoolAllocator.Adapter());
          CallNode *callstmt = static_cast<CallNode *>(mirModule->mirBuilder->CreateStmtCall(fn->puIdx, nopnds));
          newblock->AddStatement(callstmt);
        } else {
          tstmt->SetOpnd(opnd0, 0);
          newblock->AddStatement(tstmt);
        }
        break;
      }
      case OP_intrinsiccall: {
        IntrinsiccallNode *intrincall = static_cast<IntrinsiccallNode *>(stmt);
        if (intrincall->intrinsic == INTRN_MPL_BOUNDARY_CHECK) {
          DoLowerBoundaryCheck(intrincall, newblock);
          break;
        }
      }
      default: {
        for (int i = 0; i < stmt->NumOpnds(); i++) {
          stmt->SetOpnd(DoLowerExpr(stmt->Opnd(i), newblock), i);
        }
        newblock->AddStatement(stmt);
        break;
      }
    }
  } while (nextstmt);
  return newblock;
}

void JavaEHLowerer::DoLower(MIRFunction *func) {
  if (func->body == nullptr) {
    return;
  }
  divSTIndex = 0;  // init it to 0
  BlockNode *newbody = DoLowerBlock(func->body);
  func->body = newbody;
}

AnalysisResult *JavaEHLowerer::Run(MIRModule *module, ModuleResultMgr *mrm) {
  mirModule = module;
  if (Options::usepreg) {
    useRegTmp = true;
  }
  for (MapleVector<MIRFunction *>::iterator it = module->functionList.begin(); it != module->functionList.end();
       it++) {
    mirModule->SetCurFunction(*it);
    DoLower(*it);
  }
  return nullptr;
}

}  // namespace maple
