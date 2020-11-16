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

#include "mir_parser.h"
#include "mir_function.h"
#include "opcode_info.h"
#include "debug_info.h"

using namespace std;
namespace maple {
std::map<TokenKind, MIRParser::FuncPtrParseStmt> MIRParser::funcPtrMapForParseStmt =
    MIRParser::InitFuncPtrMapForParseStmt();
std::map<TokenKind, MIRParser::FuncPtrParseStmtBlock> MIRParser::funcPtrMapForParseStmtBlock =
    MIRParser::InitFuncPtrMapForParseStmtBlock();

bool MIRParser::ParseStmtDassign(StmtNode *&stmt) {
  if (lexer.GetTokenKind() != TK_dassign) {
    Error("expect dassign but get ");
    return false;
  }
  DassignNode *assignstmt = mod.CurFuncCodeMemPool()->New<DassignNode>();
  // parse %i
  lexer.NextToken();
  StIdx stIdx;
  if (!ParseDeclaredSt(stIdx)) {
    return false;
  }
  if (stIdx.FullIdx() == 0) {
    Error("expect a symbol parsing ParseStmtDassign");
    return false;
  }
  assignstmt->stIdx = stIdx;

  TokenKind ntk = lexer.NextToken();
  // parse field id
  if (ntk == TK_intconst) {  // may be a field id
    assignstmt->fieldID = lexer.GetTheIntVal();
    ntk = lexer.NextToken();
  }

  // parse expression like (constVal i32 0)
  if (ntk != TK_lparen) {
    Error("expect ( parsing Dassign but get ");
    return false;
  }
  // parse expression in ()
  lexer.NextToken();
  BaseNode *expr = nullptr;
  if (!ParseExpression(expr)) {
    Error("ParseStmtDassign failed");
    return false;
  }

  // parse )
  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing Dassign but get ");
    return false;
  }
  assignstmt->SetRhs(expr);
  stmt = assignstmt;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtRegassign(StmtNode *&stmt) {
  RegassignNode *regassign = mod.CurFuncCodeMemPool()->New<RegassignNode>();
  if (!IsPrimitiveType(lexer.NextToken())) {
    Error("expect type parsing binary operator but get ");
    return false;
  }
  regassign->primType = GetPrimitiveType(lexer.GetTokenKind());
  lexer.NextToken();
  if (lexer.GetTokenKind() == TK_specialreg) {
    if (!ParseSpecialReg(regassign->regIdx)) {
      return false;
    }
  } else if (lexer.GetTokenKind() == TK_preg) {
    if (!ParsePseudoreg(regassign->primType, regassign->regIdx)) {
      return false;
    }
  } else {
    Error("expect special or pseudo register but get ");
    return false;
  }

  // parse expression
  if (lexer.GetTokenKind() != TK_lparen) {
    Error("expect ( parsing regassign but get ");
    return false;
  }
  // parse expression in ()
  lexer.NextToken();
  BaseNode *expr = nullptr;
  if (!ParseExpression(expr)) {
    Error("ParseStmtRegassign failed");
    return false;
  }

  // parse )
  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing regassign but get ");
    return false;
  }
  regassign->uOpnd = expr;
  if (regassign->regIdx > 0) {  // check type consistenency for the preg
    MIRPreg *preg = mod.CurFunction()->pregTab->PregFromPregIdx(regassign->regIdx);
    if (preg->primType == kPtyInvalid) {
      preg->primType = expr->primType;
    } else if (preg->primType == PTY_dynany) {
      if (!IsPrimitiveDynType(expr->primType)) {
        Error("inconsistent preg primitive dynamic type at ");
        return false;
      }
    }
  }
  stmt = regassign;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtIassign(StmtNode *&stmt) {
  // iassign <* [10] int> ()
  if (lexer.GetTokenKind() != TK_iassign) {
    Error("expect iassign but get ");
    return false;
  }
  // expect <> derived type
  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParseDerivedType(tyIdx)) {
    Error("ParseStmtIassign failed when parisng derived type");
    return false;
  }
  IassignNode *iassign = mod.CurFuncCodeMemPool()->New<IassignNode>();
  iassign->tyIdx = tyIdx;

  if (lexer.GetTokenKind() == TK_intconst) {
    iassign->fieldID = lexer.GetTheIntVal();
    lexer.NextToken();
  }

  if (lexer.GetTokenKind() != TK_lparen) {
    Error("expect ( during parse iassign but get ");
    return false;
  }
  lexer.NextToken();

  // parse 2 operands then, #1 is address, the other would be value
  BaseNode *node0 = nullptr;
  if (!ParseExpression(node0)) {
    Error("ParseStmtIassign failed when parsing operand 0");
    return false;
  }
  iassign->addrExpr = node0;

  if (lexer.GetTokenKind() != TK_coma) {
    Error("expect , during parse iassign but get ");
    return false;
  }
  lexer.NextToken();
  BaseNode *node1 = nullptr;
  if (!ParseExpression(node1)) {
    Error("ParseStmtIassign failed when parsing operand 1");
    return false;
  }

  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) during parse iassign bug get ");
    return false;
  }
  iassign->rhs = node1;
  lexer.NextToken();
  stmt = iassign;
  return true;
}

bool MIRParser::ParseStmtIassignoff(StmtNode *&stmt) {
  // iassign <prim-type> <offset> ( <addr-expr>, <rhs-expr> )
  IassignoffNode *iassignoff = mod.CurFuncCodeMemPool()->New<IassignoffNode>();
  if (!IsPrimitiveType(lexer.NextToken())) {
    Error("expect type parsing binary operator but get ");
    return false;
  }
  iassignoff->primType = GetPrimitiveType(lexer.GetTokenKind());
  if (lexer.NextToken() != TK_intconst) {
    Error("expect offset but get ");
    return false;
  }
  iassignoff->offset = lexer.GetTheIntVal();
  if (lexer.NextToken() != TK_lparen) {
    Error("expect ( in iassignoff but get ");
    return false;
  }
  lexer.NextToken();

  // parse 2 operands then, #1 is address, the other would be value
  BaseNode *node0 = nullptr;
  if (!ParseExpression(node0)) {
    Error("ParseStmtIassignoff failed when parsing operand 0");
    return false;
  }
  iassignoff->bOpnd[0] = node0;

  if (lexer.GetTokenKind() != TK_coma) {
    Error("expect , in iassignoff but get ");
    return false;
  }
  lexer.NextToken();
  BaseNode *node1 = nullptr;
  if (!ParseExpression(node1)) {
    Error("ParseStmtIassignoff failed when parsing operand 1");
    return false;
  }

  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) during parse iassign bug get ");
    return false;
  }
  iassignoff->bOpnd[1] = node1;
  lexer.NextToken();
  stmt = iassignoff;
  return true;
}

bool MIRParser::ParseStmtIassignFPoff(StmtNode *&stmt) {
  // iassignfpoff <prim-type> <offset> (<rhs-expr> )
  IassignFPoffNode *iassignoff = mod.CurFuncCodeMemPool()->New<IassignFPoffNode>();
  if (!IsPrimitiveType(lexer.NextToken())) {
    Error("expect type parsing binary operator but get ");
    return false;
  }
  iassignoff->primType = GetPrimitiveType(lexer.GetTokenKind());
  if (lexer.NextToken() != TK_intconst) {
    Error("expect offset but get ");
    return false;
  }
  iassignoff->offset = lexer.GetTheIntVal();
  lexer.NextToken();

  if (lexer.GetTokenKind() != TK_lparen) {
    Error("expect ( in iassignoff but get ");
    return false;
  }
  lexer.NextToken();

  // parse 2 operands then, #1 is address, the other would be value
  BaseNode *node1 = nullptr;
  if (!ParseExpression(node1)) {
    Error("ParseStmtIassignFPoff failed when parsing operand 0");
    return false;
  }

  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) during parse iassign bug get ");
    return false;
  }
  iassignoff->uOpnd = node1;
  lexer.NextToken();
  stmt = iassignoff;
  return true;
}

bool MIRParser::ParseStmtDoloop(StmtNode *&stmt) {
  // syntax: doloop <do-var> (<start-expr>, <cont-expr>, <incr-amt>) {
  //              <body-stmts> }
  DoloopNode *doloopnode = mod.CurFuncCodeMemPool()->New<DoloopNode>();
  stmt = doloopnode;
  lexer.NextToken();

  if (lexer.GetTokenKind() == TK_preg) {
    uint32 pregNo = static_cast<uint32>(lexer.GetTheIntVal());
    MIRFunction *curfunc = mod.CurFunction();
    PregIdx pregidx = curfunc->pregTab->EnterPregNo(pregNo, kPtyInvalid);
    doloopnode->isPreg = true;
    doloopnode->doVarStIdx.SetFullIdx(pregidx);
    // let other appearances handle the preg primitive type
  } else {
    StIdx stIdx;

    if (!ParseDeclaredSt(stIdx)) {
      return false;
    }
    if (stIdx.FullIdx() == 0) {
      Error("expect a symbol parsing ParseStmtDoloop");
      return false;
    }
    if (stIdx.IsGlobal()) {
      Error("expect local variable for doloop var but get ");
      return false;
    }
    doloopnode->doVarStIdx = stIdx;
  }

  // parse (
  if (lexer.NextToken() != TK_lparen) {
    Error("expect ( but get ");
    return false;
  }

  // parse start expression
  lexer.NextToken();
  BaseNode *start = nullptr;
  if (!ParseExpression(start)) {
    Error("ParseStmtDoloop when parsing start expression");
    return false;
  }

  if (doloopnode->isPreg) {
    PregIdx regIdx = (PregIdx)doloopnode->doVarStIdx.FullIdx();
    MIRPreg *mpreg = mod.CurFunction()->pregTab->PregFromPregIdx(regIdx);
    if (mpreg->primType == kPtyInvalid) {
      mpreg->primType = start->primType;
    }
  }

  if (lexer.GetTokenKind() != TK_coma) {
    Error("expect , after start expression but get ");
    return false;
  }
  doloopnode->startExpr = start;

  // parse end expression
  lexer.NextToken();
  BaseNode *end = nullptr;
  if (!ParseExpression(end)) {  // here should be a compare expression
    Error("ParseStmtDoloop when parsing end expression");
    return false;
  }
  if (lexer.GetTokenKind() != TK_coma) {
    Error("expect , after condition expression but get ");
    return false;
  }
  doloopnode->condExpr = end;

  // parse renew induction expression
  lexer.NextToken();
  BaseNode *induction = nullptr;
  if (!ParseExpression(induction)) {
    Error("ParseStmtDoloop when parsing induction");
    return false;
  }
  // parse )
  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing doloop but get ");
    return false;
  }
  doloopnode->incrExpr = induction;

  // parse body of the loop
  lexer.NextToken();
  BlockNode *bodystmt = nullptr;
  if (!ParseStmtBlock(bodystmt)) {
    Error("ParseStmtDoloop when parsing body of the loop");
    return false;
  }
  doloopnode->doBody = bodystmt;
  return true;
}

bool MIRParser::ParseStmtForeachelem(StmtNode *&stmt) {
  // syntax: foreachelem <elemvar> <arrayvar> {
  //              <body-stmts> }
  ForeachelemNode *fornode = mod.CurFuncCodeMemPool()->New<ForeachelemNode>();
  stmt = fornode;
  lexer.NextToken();  // skip foreachelem token

  StIdx stIdx;
  if (!ParseDeclaredSt(stIdx)) {
    return false;
  }
  if (stIdx.FullIdx() == 0) {
    Error("error parsing element variable of foreachelem in ");
    return false;
  }
  if (stIdx.IsGlobal()) {
    Error("illegal global scope for element variable for foreachelem in ");
    return false;
  }
  fornode->elemStIdx = stIdx;

  lexer.NextToken();
  if (!ParseDeclaredSt(stIdx)) {
    return false;
  }
  if (stIdx.FullIdx() == 0) {
    Error("error parsing array/collection variable of foreachelem in ");
    return false;
  }
  fornode->arrayStIdx = stIdx;
  lexer.NextToken();

  // parse body of the loop
  BlockNode *bodystmt = nullptr;
  if (!ParseStmtBlock(bodystmt)) {
    Error("error when parsing body of foreachelem loop in ");
    return false;
  }
  fornode->loopBody = bodystmt;
  return true;
}

bool MIRParser::ParseStmtIf(StmtNode *&stmt) {
  if (lexer.GetTokenKind() != TK_if) {
    Error("expect if but get ");
    return false;
  }
  IfStmtNode *ifstmt = mod.CurFuncCodeMemPool()->New<IfStmtNode>();

  if (lexer.NextToken() != TK_lparen) {
    Error("expect return with ( but get ");
    return false;
  }

  lexer.NextToken();
  BaseNode *expr = nullptr;
  if (!ParseExpression(expr)) {
    Error("ParseStmtIf failed when parsing value");
    return false;
  }
  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing return but get ");
    return false;
  }
  ifstmt->uOpnd = expr;

  if (lexer.NextToken() != TK_lbrace) {
    Error("expect { begin if body but get ");
    return false;
  }
  BlockNode *thenblock = nullptr;
  if (!ParseStmtBlock(thenblock)) {
    Error("ParseStmtIf failed when parsing then block");
    return false;
  }
  ifstmt->thenPart = thenblock;

  BlockNode *elseblock = nullptr;
  if (lexer.GetTokenKind() == TK_else) {
    // has else part
    if (lexer.NextToken() != TK_lbrace) {
      Error("expect { begin if body but get ");
      return false;
    }
    if (!ParseStmtBlock(elseblock)) {
      Error("ParseStmtIf failed when parsing else block");
      return false;
    }
    ifstmt->elsePart = elseblock;
    if (elseblock) {
      ifstmt->numOpnds++;
    }
  }

  stmt = ifstmt;
  return true;
}

bool MIRParser::ParseStmtWhile(StmtNode *&stmt) {
  if (lexer.GetTokenKind() != TK_while) {
    Error("expect while but get ");
    return false;
  }
  WhileStmtNode *whilestmt = mod.CurFuncCodeMemPool()->New<WhileStmtNode>(OP_while);

  if (lexer.NextToken() != TK_lparen) {
    Error("expect while with ( but get ");
    return false;
  }

  BaseNode *expr = nullptr;
  lexer.NextToken();
  if (!ParseExpression(expr)) {
    return false;
  }

  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing return but get ");
    return false;
  }
  whilestmt->uOpnd = expr;

  if (lexer.NextToken() != TK_lbrace) {
    Error("expect { begin if body but get ");
    return false;
  }
  BlockNode *whilebody = nullptr;
  if (!ParseStmtBlock(whilebody)) {
    Error("ParseStmtWhile failed when parse while body");
    return false;
  }
  whilestmt->body = whilebody;

  stmt = whilestmt;
  return true;
}

bool MIRParser::ParseStmtDowhile(StmtNode *&stmt) {
  if (lexer.GetTokenKind() != TK_dowhile) {
    Error("expect while but get ");
    return false;
  }
  WhileStmtNode *whilestmt = mod.CurFunction()->codeMemPool->New<WhileStmtNode>(OP_dowhile);

  if (lexer.NextToken() != TK_lbrace) {
    Error("expect { begin if body but get ");
    return false;
  }
  BlockNode *dowhilebody = nullptr;
  if (!ParseStmtBlock(dowhilebody)) {
    Error("ParseStmtDowhile failed when trying to parsing do while body");
    return false;
  }
  whilestmt->body = dowhilebody;

  if (lexer.GetTokenKind() != TK_lparen) {
    Error("expect ( but get ");
    return false;
  }

  BaseNode *expr = nullptr;
  lexer.NextToken();
  if (!ParseExpression(expr)) {
    Error("ParseStmtDowhile failed");
    return false;
  }

  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing return but get ");
    return false;
  }
  whilestmt->uOpnd = expr;
  lexer.NextToken();
  stmt = whilestmt;
  return true;
}

bool MIRParser::ParseStmtLabel(StmtNode *&stmt) {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  LabelIdx labidx = mod.CurFunction()->labelTab->strIdxToLabIdxMap[strIdx];
  if (labidx == 0) {
    labidx = mod.CurFunction()->labelTab->CreateLabel();
    mod.CurFunction()->labelTab->labelTable.at(labidx) = strIdx;
    mod.CurFunction()->labelTab->AddToStringLabelMap(labidx);
  } else {
    if (definedLabels.size() > labidx && definedLabels[labidx]) {
      Error("label multiply declared ");
      return false;
    }
  }
  if (definedLabels.size() <= labidx) {
    definedLabels.resize(labidx + 1);
  }
  definedLabels[labidx] = true;
  LabelNode *labnode = mod.CurFuncCodeMemPool()->New<LabelNode>();
  labnode->labelIdx = labidx;

  stmt = labnode;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtGoto(StmtNode *&stmt) {
  if (lexer.GetTokenKind() != TK_goto) {
    Error("expect goto but get ");
    return false;
  }
  if (lexer.NextToken() != TK_label) {
    Error("expect label in goto but get ");
    return false;
  }
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  LabelIdx labidx = mod.CurFunction()->labelTab->strIdxToLabIdxMap[strIdx];
  if (labidx == 0) {
    labidx = mod.CurFunction()->labelTab->CreateLabel();
    mod.CurFunction()->labelTab->labelTable.at(labidx) = strIdx;
    mod.CurFunction()->labelTab->AddToStringLabelMap(labidx);
  }
  GotoNode *gotonode = mod.CurFuncCodeMemPool()->New<GotoNode>(OP_goto);
  gotonode->offset = labidx;

  stmt = gotonode;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtBr(StmtNode *&stmt) {
  TokenKind tk = lexer.GetTokenKind();
  if (tk != TK_brtrue && tk != TK_brfalse) {
    Error("expect brtrue/brfalse but get ");
    return false;
  }
  if (lexer.NextToken() != TK_label) {
    Error("expect label in goto but get ");
    return false;
  }
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  LabelIdx labidx = mod.CurFunction()->labelTab->strIdxToLabIdxMap[strIdx];
  if (labidx == 0) {
    labidx = mod.CurFunction()->labelTab->CreateLabel();
    mod.CurFunction()->labelTab->labelTable.at(labidx) = strIdx;
    mod.CurFunction()->labelTab->AddToStringLabelMap(labidx);
  }
  CondGotoNode *condgoto = mod.CurFuncCodeMemPool()->New<CondGotoNode>(tk == TK_brtrue ? OP_brtrue : OP_brfalse);
  condgoto->offset = labidx;

  // parse (<opnd0>)
  if (lexer.NextToken() != TK_lparen) {
    Error("expect ( parsing expression but get ");
    return false;
  }
  lexer.NextToken();
  BaseNode *expr = nullptr;
  if (!ParseExpression(expr)) {
    Error("expect expression but get ");
    return false;
  }
  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing expression but get ");
    return false;
  }
  condgoto->uOpnd = expr;

  stmt = condgoto;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseSwitchCase(int32 &constVal, LabelIdx &lblidx) {
  // syntax <intconst0>: goto <label0>
  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect intconst in switch but get ");
    return false;
  }
  constVal = lexer.GetTheIntVal();

  if (lexer.NextToken() != TK_colon) {
    Error("expect : in switch but get ");
    return false;
  }
  if (lexer.NextToken() != TK_goto) {
    Error("expect goto in switch case but get ");
    return false;
  }

  if (lexer.NextToken() != TK_label) {
    Error("expect label in switch but get ");
    return false;
  }

  lblidx = mod.CurFunction()->GetOrCreateLablidxFromName(lexer.GetName());
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtSwitch(StmtNode *&stmt) {
  SwitchNode *switchnode = mod.CurFuncCodeMemPool()->New<SwitchNode>(&mod);
  stmt = switchnode;
  if (lexer.NextToken() != TK_lparen) {
    Error("expect ( parsing switch operand ");
    return false;
  }
  BaseNode *opnd0 = nullptr;
  lexer.NextToken();
  if (!ParseExpression(opnd0)) {
    Error("expect expression parsing switch but get ");
    return false;
  }
  if (!IsPrimitiveInteger(opnd0->primType)) {
    Error("expect expression return integer but get ");
    return false;
  }
  switchnode->switchOpnd = opnd0;
  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing switch operand ");
    return false;
  }

  if (lexer.NextToken() == TK_label) {
    switchnode->defaultLabel = mod.CurFunction()->GetOrCreateLablidxFromName(lexer.GetName());
  } else {
    Error("expect label in switch but get ");
    return false;
  }

  if (lexer.NextToken() != TK_lbrace) {
    Error("expect { in switch but get ");
    return false;
  }

  // <intconst0>: goto <label0>
  // <intconst1>: goto <label1>
  // ...
  // <intconstn>: goto <labeln>
  TokenKind tk = lexer.NextToken();
  MapleVector<CasePair> &switchTable = switchnode->switchTable;
  std::set<int32> casesSet;
  while (tk != TK_rbrace) {
    int32 constVal = 0;
    LabelIdx lbl = 0;
    if (!ParseSwitchCase(constVal, lbl)) {
      Error("parse switch case failed ");
      return false;
    }
    if (casesSet.find(constVal) != casesSet.end()) {
      Error("duplicated switch case ");
      return false;
    }
    switchTable.push_back(CasePair(constVal, lbl));
    casesSet.insert(constVal);
    tk = lexer.GetTokenKind();
  }
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtRangegoto(StmtNode *&stmt) {
  RangegotoNode *rangegotonode = mod.CurFuncCodeMemPool()->New<RangegotoNode>(&mod);
  stmt = rangegotonode;
  if (lexer.NextToken() != TK_lparen) {
    Error("expect ( parsing rangegoto operand ");
    return false;
  }
  BaseNode *opnd0 = nullptr;
  lexer.NextToken();
  if (!ParseExpression(opnd0)) {
    Error("expect expression parsing rangegoto but get ");
    return false;
  }
  if (!IsPrimitiveInteger(opnd0->primType)) {
    Error("expect expression return integer but get ");
    return false;
  }
  rangegotonode->uOpnd = opnd0;
  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing rangegoto operand ");
    return false;
  }

  if (lexer.NextToken() == TK_intconst) {
    rangegotonode->tagOffset = lexer.GetTheIntVal();
  } else {
    Error("expect tag offset in rangegoto but get ");
    return false;
  }

  if (lexer.NextToken() != TK_lbrace) {
    Error("expect { in switch but get ");
    return false;
  }

  // <intconst0>: goto <label0>
  // <intconst1>: goto <label1>
  // ...
  // <intconstn>: goto <labeln>
  TokenKind tk = lexer.NextToken();
  SmallCaseVector &rangegotoTable = rangegotonode->rangegotoTable;
  std::set<uint16> casesSet;
  int32 minidx = MAXUINT16;
  int32 maxidx = 0;
  while (tk != TK_rbrace) {
    int32 constVal = 0;
    LabelIdx lbl = 0;
    if (!ParseSwitchCase(constVal, lbl)) {
      Error("parse switch case failed ");
      return false;
    }
    if (constVal > MAXUINT16 || constVal < 0) {
      Error("rangegoto case tag not within unsigned 16 bits range ");
      return false;
    }
    if (casesSet.find(constVal) != casesSet.end()) {
      Error("duplicated switch case ");
      return false;
    }
    if (constVal < minidx) {
      minidx = constVal;
    }
    if (constVal > maxidx) {
      maxidx = constVal;
    }
    rangegotoTable.push_back(std::pair<uint32, uint32>(static_cast<uint32>(constVal), static_cast<uint32>(lbl)));
    casesSet.insert(constVal);
    tk = lexer.GetTokenKind();
  }
  ASSERT(rangegotonode->numOpnds == 1, "Rangegoto is a UnaryOpnd; numOpnds must be 1");
  // check there is no gap
  if (static_cast<uint32>(maxidx - minidx + 1) != casesSet.size()) {
    Error("gap not allowed in rangegoto case tags ");
    return false;
  }
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtMultiway(StmtNode *&stmt) {
  MultiwayNode *multiwaynode = mod.CurFuncCodeMemPool()->New<MultiwayNode>(&mod);
  stmt = multiwaynode;
  if (lexer.NextToken() != TK_lparen) {
    Error("expect ( parsing multiway operand ");
    return false;
  }
  BaseNode *opnd0 = nullptr;
  lexer.NextToken();
  if (!ParseExpression(opnd0)) {
    Error("expect expression parsing multiway but get ");
    return false;
  }
  multiwaynode->multiWayOpnd = static_cast<BaseNode *>(opnd0);
  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing multiway operand ");
    return false;
  }

  if (lexer.NextToken() == TK_label) {
    multiwaynode->defaultLabel = mod.CurFunction()->GetOrCreateLablidxFromName(lexer.GetName());
  } else {
    Error("expect label in multiway but get ");
    return false;
  }

  if (lexer.NextToken() != TK_lbrace) {
    Error("expect { in switch but get ");
    return false;
  }

  // (<expr0>): goto <label0>
  // (<expr1>): goto <label1>
  // ...
  // (<exprn>): goto <labeln>
  TokenKind tk = lexer.NextToken();
  MapleVector<MCasePair> &multiWayTable = multiwaynode->multiWayTable;
  while (tk != TK_rbrace) {
    BaseNode *x = nullptr;
    if (lexer.GetTokenKind() != TK_lparen) {
      Error("expect ( parsing multiway case tag expression but get ");
      return false;
    }
    lexer.NextToken();
    if (!ParseExpression(x)) {
      Error("parsing of multiway case tag expression failed at ");
      return false;
    }
    if (lexer.GetTokenKind() != TK_rparen) {
      Error("expect > parsing multiway case tag expression but get ");
      return false;
    }
    if (lexer.NextToken() != TK_colon) {
      Error("expect : parsing multiway case tag specification but get ");
      return false;
    }
    if (lexer.NextToken() != TK_goto) {
      Error("expect goto in multiway case expression but get ");
      return false;
    }
    if (lexer.NextToken() != TK_label) {
      Error("expect goto label after multiway case expression but get ");
      return false;
    }
    LabelIdx lblidx = mod.CurFunction()->GetOrCreateLablidxFromName(lexer.GetName());
    lexer.NextToken();
    multiWayTable.push_back(MCasePair(static_cast<BaseNode *>(x), lblidx));
    tk = lexer.GetTokenKind();
  }
  multiwaynode->numOpnds = multiWayTable.size();
  lexer.NextToken();
  return true;
}

// used only when parsing mmpl
PUIdx MIRParser::EnterUndeclaredFunction(void) {
  const std::string &funcName = lexer.GetName();
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcName);
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  funcSt->SetNameStridx(strIdx);
  GlobalTables::GetGsymTable().AddToStringSymbolMap(funcSt);
  funcSt->storageClass = kScText;
  funcSt->sKind = kStFunc;
  MIRFunction *fn = mod.memPool->New<MIRFunction>(&mod, funcSt->GetStIdx());
  fn->puIdx = GlobalTables::GetFunctionTable().funcTable.size();
  GlobalTables::GetFunctionTable().funcTable.push_back(fn);
  funcSt->SetFunction(fn);
  return fn->puIdx;
}

PUIdx MIRParser::EnterUndeclaredFunctionMcount(void) {
  const std::string &funcName = "_mcount";
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcName);
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  funcSt->SetNameStridx(strIdx);
  GlobalTables::GetGsymTable().AddToStringSymbolMap(funcSt);
  funcSt->storageClass = kScText;
  funcSt->sKind = kStFunc;
  MIRFunction *fn = mod.memPool->New<MIRFunction>(&mod, funcSt->GetStIdx());
  fn->puIdx = GlobalTables::GetFunctionTable().funcTable.size();

  CHECK(PTY_void < GlobalTables::GetTypeTable().typeTable.size(), "index out of range in MIRParser::EnterUndeclaredFunctionMcount");

  GlobalTables::GetFunctionTable().funcTable.push_back(fn);
  funcSt->SetFunction(fn);
  MIRFuncType *functype = mod.memPool->New<MIRFuncType>();
  fn->funcType = functype;

  return fn->puIdx;
}

bool MIRParser::ParseStmtCallMcount(StmtNode *&stmt) {
  // syntax: call <PU-name> (<opnd0>, ..., <opndn>)
  Opcode o = OP_call;

  PUIdx pidx = EnterUndeclaredFunctionMcount();

  CallNode *callstmt;

  callstmt = mod.CurFuncCodeMemPool()->New<CallNode>(&mod, o);
  callstmt->puIdx = pidx;

  MapleVector<BaseNode *> opndsvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
  callstmt->nOpnd = opndsvec;
  callstmt->numOpnds = opndsvec.size();

  stmt = callstmt;
  return true;
}

bool MIRParser::ParseStmtCall(StmtNode *&stmt) {
  // syntax: call <PU-name> (<opnd0>, ..., <opndn>)
  TokenKind tk = lexer.GetTokenKind();
  Opcode o = GetOpFromToken(tk);
  CHECK_FATAL(kOpcodeInfo.IsCall(o), "ParseStmtCall: not a call opcode");
  bool hasassigned = kOpcodeInfo.IsCallAssigned(o);
  bool hasinstant = false;
  bool withtype = false;
  switch (tk) {
    case TK_polymorphiccall:
    case TK_polymorphiccallassigned:
      withtype = true;
      break;
    case TK_callinstant:
    case TK_virtualcallinstant:
    case TK_superclasscallinstant:
    case TK_interfacecallinstant:
    case TK_callinstantassigned:
    case TK_virtualcallinstantassigned:
    case TK_superclasscallinstantassigned:
    case TK_interfacecallinstantassigned:
      hasinstant = true;
      break;
    default:;
  }

  TyIdx polymophictyidx(0);
  if (o == OP_polymorphiccallassigned || o == OP_polymorphiccall) {
    TokenKind tk = lexer.NextToken();
    if (tk == TK_langle) {
      tk = lexer.NextToken();
      if (tk == TK_func) {
        lexer.NextToken();
        if (!ParseFuncType(polymophictyidx)) {
          Error("error parsing functype in ParseStmtCall for polymorphiccallassigned at ");
          return false;
        }
      } else {
        Error("expect func in functype but get ");
        return false;
      }
    } else {
      Error("expect < in functype but get ");
      return false;
    }
  }

  TokenKind funcTk = lexer.NextToken();
  if (funcTk != TK_fname) {
    Error("expect func name in call but get ");
    return false;
  }
  PUIdx pidx;
  if (!ParseDeclaredFunc(pidx)) {
    if (mod.flavor < kMmpl) {
      Error("expect .mmpl");
      return false;
    }
    pidx = EnterUndeclaredFunction();
  }

  lexer.NextToken();
  CallNode *callstmt = nullptr;
  CallNode *callwithtypestmt = nullptr;
  CallinstantNode *callinstantstmt = nullptr;
  CallNode *callassignedstmt = nullptr;
  CallinstantNode *callinstantassignedstmt = nullptr;
  CallNode *callwithtypeassignedstmt = nullptr;
  if (withtype) {
    if (o == OP_polymorphiccallassigned) {
      callwithtypeassignedstmt = mod.CurFuncCodeMemPool()->New<CallNode>(&mod, o);
      callwithtypeassignedstmt->tyIdx = polymophictyidx;
      callstmt = callwithtypeassignedstmt;
    } else {
      callwithtypestmt = mod.CurFuncCodeMemPool()->New<CallNode>(&mod, o);
      callwithtypestmt->tyIdx = polymophictyidx;
      callstmt = callwithtypestmt;
    }
  } else if (hasinstant) {
    TokenKind langleTk = lexer.GetTokenKind();
    if (langleTk != TK_langle) {
      Error("missing < in generic method instantiation at ");
      return false;
    }
    TokenKind lbraceTk = lexer.NextToken();
    if (lbraceTk != TK_lbrace) {
      Error("missing { in generic method instantiation at ");
      return false;
    }
    MIRInstantVectorType instvecty;
    if (!ParseGenericInstantVector(instvecty.instantVec)) {
      Error("error parsing generic method instantiation at ");
      return false;
    }
    TokenKind rangleTk = lexer.GetTokenKind();
    if (rangleTk != TK_rangle) {
      Error("missing > in generic method instantiation at ");
      return false;
    }
    TyIdx tyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&instvecty);
    if (!hasassigned) {
      callinstantstmt = mod.CurFuncCodeMemPool()->New<CallinstantNode>(&mod, o, tyIdx);
      callstmt = callinstantstmt;
    } else {
      callinstantassignedstmt = mod.CurFuncCodeMemPool()->New<CallinstantNode>(&mod, o, tyIdx);
      callstmt = callinstantassignedstmt;
    }
    lexer.NextToken();  // skip the >
  } else {
    if (!hasassigned) {
      callstmt = mod.CurFuncCodeMemPool()->New<CallNode>(&mod, o);
    } else {
      callassignedstmt = mod.CurFuncCodeMemPool()->New<CallNode>(&mod, o);
      callstmt = callassignedstmt;
    }
  }
  callstmt->puIdx = pidx;
  MIRFunction *callee = GlobalTables::GetFunctionTable().funcTable[pidx];
  if (callee->GetName() == "setjmp") {
    mod.CurFunction()->SetHasSetjmp();
  }

  MapleVector<BaseNode *> opndsvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
  if (!ParseExprNaryOperand(opndsvec)) {
    return false;
  }
  callstmt->nOpnd = opndsvec;
  callstmt->numOpnds = opndsvec.size();

  if (hasassigned) {
    CallReturnVector retsvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
    if (!ParseCallReturns(retsvec)) {
      return false;
    }
    if (!hasinstant) {
      if (withtype) {
        CHECK_FATAL(callwithtypeassignedstmt, "callwithtypeassignedstmt is null in MIRParser::ParseStmtCall");
        callwithtypeassignedstmt->returnValues = retsvec;
      } else {
        CHECK_FATAL(callassignedstmt != nullptr, "callassignedstmt is null in MIRParser::ParseStmtCall");
        callassignedstmt->returnValues = retsvec;
      }
    } else {
      CHECK_FATAL(callinstantassignedstmt != nullptr, "callinstantassignedstmt is null in MIRParser::ParseStmtCall");
      callinstantassignedstmt->returnValues = retsvec;
    }
  }

  lexer.NextToken();
  stmt = callstmt;
  return true;
}

bool MIRParser::ParseStmtIcall(StmtNode* &stmt, bool isAssigned) {
  // syntax: icall (<PU-ptr>, <opnd0>, ..., <opndn>)
  //         icallassigned <PU-ptr> (<opnd0>, ..., <opndn>) {
  //              dassign <var-name0> <field-id0>
  //              dassign <var-name1> <field-id1>
  //               . . .
  //              dassign <var-namen> <field-idn> }
  IcallNode *icallStmt = nullptr;
  if (!isAssigned) {
    icallStmt = mod.CurFuncCodeMemPool()->New<IcallNode>(&mod, OP_icall);
  } else {
    icallStmt = mod.CurFuncCodeMemPool()->New<IcallNode>(&mod, OP_icallassigned);
  }
  lexer.NextToken();
  MapleVector<BaseNode*> opndsvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
  if (!ParseExprNaryOperand(opndsvec)) {
    return false;
  }
  icallStmt->nOpnd = opndsvec;
  icallStmt->numOpnds = opndsvec.size();
  if (isAssigned) {
    CallReturnVector retsvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
    if (!ParseCallReturns(retsvec)) {
      return false;
    }
    static_cast<IcallNode *>(icallStmt)->returnValues = retsvec;
  }
  lexer.NextToken();
  stmt = icallStmt;
  return true;
}

bool MIRParser::ParseStmtIcall(StmtNode *&stmt) {
  return ParseStmtIcall(stmt, false);
}

bool MIRParser::ParseStmtIcallAssigned(StmtNode *&stmt) {
  return ParseStmtIcall(stmt, true);
}

bool MIRParser::ParseStmtIntrinsiccall(StmtNode* &stmt, bool isAssigned) {
  Opcode o = !isAssigned ? (lexer.GetTokenKind() == TK_intrinsiccall ? OP_intrinsiccall : OP_xintrinsiccall)
                         : (lexer.GetTokenKind() == TK_intrinsiccallassigned ? OP_intrinsiccallassigned
                                                                             : OP_xintrinsiccallassigned);
  IntrinsiccallNode *intrncallnode = nullptr;
  if (!isAssigned) {
    intrncallnode = mod.CurFuncCodeMemPool()->New<IntrinsiccallNode>(&mod, o);
  } else {
    intrncallnode = mod.CurFuncCodeMemPool()->New<IntrinsiccallNode>(&mod, o);
  }
  lexer.NextToken();
  if (o == OP_intrinsiccall || o == OP_intrinsiccallassigned) {
    intrncallnode->intrinsic = GetIntrinsicId(lexer.GetTokenKind());
  } else {
    intrncallnode->intrinsic = static_cast<MIRIntrinsicID>(lexer.GetTheIntVal());
  }
  lexer.NextToken();
  MapleVector<BaseNode*> opndsvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
  if (!ParseExprNaryOperand(opndsvec)) {
    return false;
  }
  intrncallnode->nOpnd = opndsvec;
  intrncallnode->numOpnds = opndsvec.size();
  if (isAssigned) {
    CallReturnVector retsvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
    if (!ParseCallReturns(retsvec)) {
      return false;
    }
    // store return type of IntrinsiccallNode
    if (retsvec.size() == 1 && retsvec[0].first.Idx() != 0) {
      MIRSymbol *retsymbol = curFunc->symTab->GetSymbolFromStIdx(retsvec[0].first.Idx());
      MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(retsymbol->tyIdx);
      CHECK_FATAL(rettype != nullptr, "rettype is null in MIRParser::ParseStmtIntrinsiccallAssigned");
      intrncallnode->primType = rettype->GetPrimType();
    }
    static_cast<IntrinsiccallNode *>(intrncallnode)->returnValues = retsvec;
  }
  stmt = intrncallnode;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtIntrinsiccall(StmtNode* &stmt) {
  return ParseStmtIntrinsiccall(stmt, false);
}

bool MIRParser::ParseStmtIntrinsiccallassigned(StmtNode* &stmt) {
  return ParseStmtIntrinsiccall(stmt, true);
}

bool MIRParser::ParseStmtIntrinsiccallwithtype(StmtNode* &stmt, bool isAssigned) {
  Opcode o = !isAssigned ? OP_intrinsiccallwithtype : OP_intrinsiccallwithtypeassigned;
  IntrinsiccallNode *intrncallnode = nullptr;
  if (!isAssigned) {
    intrncallnode = mod.CurFuncCodeMemPool()->New<IntrinsiccallNode>(&mod, o);
  } else {
    intrncallnode = mod.CurFuncCodeMemPool()->New<IntrinsiccallNode>(&mod, o);
  }
  TokenKind tk = lexer.NextToken();
  TyIdx tyIdx(0);
  if (IsPrimitiveType(tk)) {
    if (!ParsePrimType(tyIdx)) {
      Error("expect primitive type in ParseStmtIntrinsiccallwithtype but get ");
      return false;
    }
  } else if (!ParseDerivedType(tyIdx)) {
    Error("error parsing type in ParseStmtIntrinsiccallwithtype at ");
    return false;
  }
  intrncallnode->tyIdx = tyIdx;
  intrncallnode->intrinsic = GetIntrinsicId(lexer.GetTokenKind());
  lexer.NextToken();
  MapleVector<BaseNode*> opndsvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
  if (!ParseExprNaryOperand(opndsvec)) {
    return false;
  }
  intrncallnode->nOpnd = opndsvec;
  intrncallnode->numOpnds = opndsvec.size();
  if (isAssigned) {
    CallReturnVector retsvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
    if (!ParseCallReturns(retsvec)) {
      return false;
    }
    // store return type of IntrinsiccallNode
    if (retsvec.size() == 1 && retsvec[0].first.Idx() != 0) {
      MIRSymbol *retsymbol = curFunc->symTab->GetSymbolFromStIdx(retsvec[0].first.Idx());
      MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(retsymbol->GetTyIdx());
      CHECK_FATAL(rettype != nullptr, "rettype is null in MIRParser::ParseStmtIntrinsiccallwithtypeAssigned");
      intrncallnode->primType = rettype->GetPrimType();
    }
    static_cast<IntrinsiccallNode *>(intrncallnode)->returnValues = retsvec;
  }
  stmt = intrncallnode;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtIntrinsiccallwithtype(StmtNode* &stmt) {
  return ParseStmtIntrinsiccallwithtype(stmt, false);
}

bool MIRParser::ParseStmtIntrinsiccallwithtypeassigned(StmtNode* &stmt) {
  return ParseStmtIntrinsiccallwithtype(stmt, true);
}

bool MIRParser::ParseCallReturns(CallReturnVector &retsvec) {
  //             {
  //              dassign <var-name0> <field-id0>
  //              dassign <var-name1> <field-id1>
  //               . . .
  //              dassign <var-namen> <field-idn> }
  //              OR
  //             {
  //               regassign <type> <reg1>
  //               regassign <type> <reg2>
  //               regassign <type> <reg3>
  //             }
  if (lexer.NextToken() != TK_lbrace) {
    Error("expect { parsing call return values. ");
    return false;
  }
  TokenKind tk = lexer.NextToken();
  while (tk != TK_rbrace) {
    if (lexer.GetTokenKind() != TK_dassign && lexer.GetTokenKind() != TK_regassign) {
      Error("expect dassign/regassign but get ");
      return false;
    }
    bool isst = (lexer.GetTokenKind() == TK_dassign);
    if (isst) {
      // parse %i
      lexer.NextToken();
      StIdx stIdx;
      // How to use isLocal??
      if (!ParseDeclaredSt(stIdx)) {
        return false;
      }
      if (lexer.GetTokenKind() == TK_lname) {
        MIRSymbolTable *lsymtab = mod.CurFunction()->symTab;
        MIRSymbol *lsym = lsymtab->GetSymbolFromStIdx(stIdx.Idx(), 0);
        if (lsym->GetName().find("L_STR") == 0) {
          MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(lsym->tyIdx);
          CHECK_FATAL(ty->GetKind() == kTypePointer, "Pointer type expected for L_STR prefix");
          MIRPtrType tempType(static_cast<MIRPtrType *>(ty)->pointedTyIdx, PTY_ptr);
          TyIdx newTyidx = GlobalTables::GetTypeTable().CreateMIRType(&tempType);
          MIRType *newty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(newTyidx);
          lsym->tyIdx = newTyidx;
        }
      }

      if (stIdx.FullIdx() == 0) {
        Error("expect a symbol parsing ParseCallAssignedStmts. ");
        return false;
      }

      uint16 fieldID = 0;
      TokenKind ntk = lexer.NextToken();
      // parse field id
      if (ntk == TK_intconst) {
        fieldID = lexer.GetTheIntVal();
      } else {
        Error("expect a fieldID parsing ParseCallAssignedStmts. ");
      }
      RegFieldPair regfieldpair;
      regfieldpair.fieldID = fieldID;
      retsvec.push_back(CallReturnPair(stIdx, regfieldpair));
      tk = lexer.NextToken();
    } else {
      // parse type
      lexer.NextToken();
      TyIdx tyIdx(0);
      // RegreadNode regreadexpr;
      bool ret = ParsePrimType(tyIdx);
      if (ret != true) {
        Error("call ParsePrimType failed in ParseCallReturns");
        return false;
      }
      if (tyIdx == TyIdx(0)) {
        Error("expect primitive type but get ");
        return false;
      }
      PrimType ptype = GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]->primType;
      PregIdx pregidx;
      if (lexer.GetTokenKind() == TK_specialreg) {
        if (!ParseSpecialReg(pregidx)) {
          Error("expect specialreg parsing callassign CallReturnVector");
          return false;
        }
      } else if (lexer.GetTokenKind() == TK_preg) {
        if (!ParsePseudoreg(ptype, pregidx)) {
          Error("expect pseudoreg parsing callassign CallReturnVector");
          return false;
        }
      } else {
        Error("expect special or pseudo register but get ");
        return false;
      }
      CHECK_FATAL(pregidx > 0 && pregidx <= 0xffff, "register number is over 16 bits or is zero");
      RegFieldPair regfieldpair;
      regfieldpair.pregIdx = pregidx;
      retsvec.push_back(CallReturnPair(StIdx(), regfieldpair));
      tk = lexer.GetTokenKind();
    }
  }
  return true;
}

bool MIRParser::ParseStmtJsTry(StmtNode *&stmt) {
  JsTryNode *trynode = mod.CurFuncCodeMemPool()->New<JsTryNode>();
  lexer.NextToken();
  // parse handler label
  if (lexer.GetTokenKind() == TK_intconst && lexer.GetTheIntVal() == 0) {
    trynode->catchOffset = 0;
  } else {
    if (lexer.GetTokenKind() != TK_label) {
      Error("expect handler label in try but get ");
      return false;
    }
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
    LabelIdx labidx = mod.CurFunction()->labelTab->strIdxToLabIdxMap[strIdx];
    if (labidx == 0) {
      labidx = mod.CurFunction()->labelTab->CreateLabel();
      mod.CurFunction()->labelTab->labelTable[labidx] = strIdx;
      mod.CurFunction()->labelTab->AddToStringLabelMap(labidx);
    }
    trynode->catchOffset = labidx;
  }
  lexer.NextToken();
  // parse finally label
  if (lexer.GetTokenKind() == TK_intconst && lexer.GetTheIntVal() == 0) {
    trynode->finallyOffset = 0;
  } else {
    if (lexer.GetTokenKind() != TK_label) {
      Error("expect finally label in try but get ");
      return false;
    }
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
    LabelIdx labidx = mod.CurFunction()->labelTab->strIdxToLabIdxMap[strIdx];
    if (labidx == 0) {
      labidx = mod.CurFunction()->labelTab->CreateLabel();
      mod.CurFunction()->labelTab->labelTable.at(labidx) = strIdx;
      mod.CurFunction()->labelTab->AddToStringLabelMap(labidx);
    }
    trynode->finallyOffset = labidx;
  }

  stmt = trynode;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtJavaTry(StmtNode *&stmt) {
  TryNode *javatrynode = mod.CurFuncCodeMemPool()->New<TryNode>(&mod, lexer.GetTokenKind() == TK_cpptry ? OP_cpptry : OP_try);
  lexer.NextToken();
  ASSERT(lexer.GetTokenKind() == TK_lbrace, "expect left brace in try but get ");
  lexer.NextToken();
  // parse handler label
  while (lexer.GetTokenKind() != TK_rbrace) {
    if (lexer.GetTokenKind() != TK_label) {
      Error("expect handler label in try but get ");
      return false;
    }
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
    LabelIdx labidx = mod.CurFunction()->labelTab->strIdxToLabIdxMap[strIdx];
    if (labidx == 0) {
      labidx = mod.CurFunction()->labelTab->CreateLabel();
      mod.CurFunction()->labelTab->labelTable[labidx] = strIdx;
      mod.CurFunction()->labelTab->AddToStringLabelMap(labidx);
    }
    javatrynode->offsets.push_back(labidx);
    lexer.NextToken();
  }

  stmt = javatrynode;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtCppCatch(StmtNode *&stmt) {
  CppCatchNode *catchnode = mod.CurFuncCodeMemPool()->New<CppCatchNode>();
  lexer.NextToken();
  ASSERT(lexer.GetTokenKind() == TK_lbrace, "expect left brace in catch but get ");
  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("expect type parsing cpp catch statement");
    return false;
  }
  catchnode->exceptionTyIdx = tyIdx;
  ASSERT(lexer.GetTokenKind() == TK_rbrace, "expect right brace in catch but get ");

  stmt = catchnode;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtJavaCatch(StmtNode *&stmt) {
  CatchNode *javacatchnode = mod.CurFuncCodeMemPool()->New<CatchNode>(&mod);
  lexer.NextToken();
  ASSERT(lexer.GetTokenKind() == TK_lbrace, "expect left brace in catch but get ");
  lexer.NextToken();
  while (lexer.GetTokenKind() != TK_rbrace) {
    TyIdx tyIdx(0);
    if (!ParseType(tyIdx)) {
      Error("expect type parsing java catch statement");
      return false;
    }
    javacatchnode->exceptionTyIdxVec.push_back(tyIdx);
  }

  javacatchnode->numOpnds = 0;

  stmt = javacatchnode;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseUnaryStmt(Opcode op, StmtNode *&stmt) {
  lexer.NextToken();
  UnaryStmtNode *throwstmt = mod.CurFuncCodeMemPool()->New<UnaryStmtNode>(op);
  stmt = throwstmt;

  BaseNode *expr = nullptr;
  if (!ParseExprOneOperand(expr)) {
    return false;
  }
  throwstmt->uOpnd = expr;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseUnaryStmtThrow(StmtNode* &stmt) {
  return ParseUnaryStmt(OP_throw, stmt);
}

bool MIRParser::ParseUnaryStmtDecRef(StmtNode* &stmt) {
  return ParseUnaryStmt(OP_decref, stmt);
}

bool MIRParser::ParseUnaryStmtIncRef(StmtNode* &stmt) {
  return ParseUnaryStmt(OP_incref, stmt);
}

bool MIRParser::ParseUnaryStmtDecRefReset(StmtNode* &stmt) {
  return ParseUnaryStmt(OP_decrefreset, stmt);
}

bool MIRParser::ParseUnaryStmtIGoto(StmtNode* &stmt) {
  return ParseUnaryStmt(OP_igoto, stmt);
}

bool MIRParser::ParseUnaryStmtEval(StmtNode* &stmt) {
  return ParseUnaryStmt(OP_eval, stmt);
}

bool MIRParser::ParseUnaryStmtFree(StmtNode* &stmt) {
  return ParseUnaryStmt(OP_free, stmt);
}

bool MIRParser::ParseUnaryStmtAssertNonNull(StmtNode* &stmt) {
  return ParseUnaryStmt(OP_assertnonnull, stmt);
}

bool MIRParser::ParseStmtMarker(StmtNode *&stmt) {
  Opcode op;
  switch (paramTokenKindForStmt) {
    case TK_jscatch:
      op = OP_jscatch;
      break;
    case TK_finally:
      op = OP_finally;
      break;
    case TK_cleanuptry:
      op = OP_cleanuptry;
      break;
    case TK_endtry:
      op = OP_endtry;
      break;
    case TK_retsub:
      op = OP_retsub;
      break;
    case TK_membaracquire:
      op = OP_membaracquire;
      break;
    case TK_membarrelease:
      op = OP_membarrelease;
      break;
    case TK_membarstoreload:
      op = OP_membarstoreload;
      break;
    case TK_membarstorestore:
      op = OP_membarstorestore;
      break;
    default:
      return false;
  }

  StmtNode *stmtnode = mod.CurFuncCodeMemPool()->New<StmtNode>(op);
  stmt = stmtnode;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtGosub(StmtNode *&stmt) {
  if (lexer.NextToken() != TK_label) {
    Error("expect finally label in gosub but get ");
    return false;
  }
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  LabelIdx labidx = mod.CurFunction()->labelTab->strIdxToLabIdxMap[strIdx];
  if (labidx == 0) {
    labidx = mod.CurFunction()->labelTab->CreateLabel();
    mod.CurFunction()->labelTab->labelTable.at(labidx) = strIdx;
    mod.CurFunction()->labelTab->AddToStringLabelMap(labidx);
  }
  GotoNode *gosubnode = mod.CurFuncCodeMemPool()->New<GotoNode>(OP_gosub);
  gosubnode->offset = labidx;

  stmt = gosubnode;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseBinaryStmt(StmtNode *&stmt, Opcode op) {
  BinaryStmtNode *assstmt = mod.CurFuncCodeMemPool()->New<BinaryStmtNode>(op);
  lexer.NextToken();
  BaseNode *opnd0 = nullptr;
  BaseNode *opnd1 = nullptr;
  if (!ParseExprTwoOperand(opnd0, opnd1)) {
    return false;
  }
  assstmt->bOpnd[0] = opnd0;
  assstmt->bOpnd[1] = opnd1;
  stmt = assstmt;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseBinaryStmtAssertGE(StmtNode* &stmt) {
  return ParseBinaryStmt(stmt, OP_assertge);
}

bool MIRParser::ParseBinaryStmtAssertLT(StmtNode* &stmt) {
  return ParseBinaryStmt(stmt, OP_assertlt);
}

bool MIRParser::ParseNaryStmt(StmtNode *&stmt, Opcode op) {
  NaryStmtNode *stmtreturn = mod.CurFuncCodeMemPool()->New<NaryStmtNode>(&mod, op);

  if (lexer.NextToken() != TK_lparen) {
    Error("expect return with ( but get ");
    return false;
  }

  TokenKind exprTk = lexer.NextToken();

  if (exprTk == TK_rparen) {  // no operand
    stmt = stmtreturn;
    lexer.NextToken();
    return true;
  }

  BaseNode *expr = nullptr;
  if (!ParseExpression(expr)) {
    Error("ParseStmtReturn failed");
    return false;
  }
  stmtreturn->nOpnd.push_back(expr);

  if (op == OP_syncenter) {
    if (lexer.GetTokenKind() == TK_coma) {
      lexer.NextToken();
      BaseNode *expr = nullptr;
      if (!ParseExpression(expr)) {
        Error("ParseStmtReturn failed");
        return false;
      }
      stmtreturn->nOpnd.push_back(expr);
    }
  }

  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing return but get ");
    return false;
  }

  stmtreturn->numOpnds = stmtreturn->nOpnd.size();
  stmt = stmtreturn;

  lexer.NextToken();
  return true;
}

bool MIRParser::ParseNaryStmtReturn(StmtNode* &stmt) {
  return ParseNaryStmt(stmt, OP_return);
}

bool MIRParser::ParseNaryStmtSyncEnter(StmtNode* &stmt) {
  return ParseNaryStmt(stmt, OP_syncenter);
}

bool MIRParser::ParseNaryStmtSyncExit(StmtNode* &stmt) {
  return ParseNaryStmt(stmt, OP_syncexit);
}

bool MIRParser::ParseLoc(StmtNode *&stmt) {
  if (lexer.NextToken() != TK_intconst) {
    Error("expect intconst in LOC but get ");
    return false;
  }
  lastFileNum = lexer.GetTheIntVal();
  if (lexer.NextToken() != TK_intconst) {
    Error("expect intconst in LOC but get ");
    return false;
  }
  lastLineNum = lexer.GetTheIntVal();
  if (firstLineNum == 0) {
    firstLineNum = lastLineNum;
  }
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStatement(StmtNode *&stmt) {
  paramTokenKindForStmt = lexer.GetTokenKind();
  uint32 mplNum = lexer.GetLineNum();
  uint32 lnum = lastLineNum;
  uint32 fnum = lastFileNum;
  std::map<TokenKind, FuncPtrParseStmt>::iterator itFuncPtr = funcPtrMapForParseStmt.find(paramTokenKindForStmt);
  if (itFuncPtr != funcPtrMapForParseStmt.end()) {
    if (!(this->*(itFuncPtr->second))(stmt)) {
      return false;
    }
  } else {
    return false;
  }
  if (stmt && stmt->srcPosition.MplLinenum() == 0) {
    stmt->srcPosition.SetFilenum(fnum);
    stmt->srcPosition.SetLinenum(lnum);
    stmt->srcPosition.SetMplLinenum(mplNum);
  }
  return true;
}

/* parse the statements enclosed by { and }
 */
bool MIRParser::ParseStmtBlock(BlockNode* &blk) {
  if (lexer.GetTokenKind() != TK_lbrace) {
    Error("expect { for func body but get ");
    return false;
  }
  blk = mod.CurFuncCodeMemPool()->New<BlockNode>();
  MIRFunction *fn = mod.CurFunction();
  paramCurrFuncForParseStmtBlock = fn;
  lexer.NextToken();
  // Insert _mcount for PI.
  if (mod.withProfileInfo) {
    StmtNode *stmtt = nullptr;
    if (!ParseStmtCallMcount(stmtt)) {
      return false;
    }
    blk->AddStatement(stmtt);
  }
  while (true) {
    TokenKind stmtTk = lexer.GetTokenKind();
    // calculate the mpl file line number mplNum here to get accurate result
    uint32 mplNum = lexer.GetLineNum();
    if (IsStatement(stmtTk)) {
      ParseStmtBlockForSeenComment(blk, mplNum);
      StmtNode *stmt = nullptr;
      if (!ParseStatement(stmt)) {
        Error("ParseStmtBlock failed when parsing a statement");
        return false;
      }
      if (stmt != nullptr) {  // stmt is nullptr if it is a LOC
        // SetSrcPos(stmt, mplNum);
        blk->AddStatement(stmt);
      }
    } else {
      std::map<TokenKind, FuncPtrParseStmtBlock>::iterator itFuncPtr = funcPtrMapForParseStmtBlock.find(stmtTk);
      if (itFuncPtr == funcPtrMapForParseStmtBlock.end()) {
        if (stmtTk == TK_rbrace) {
          ParseStmtBlockForSeenComment(blk, mplNum);
          lexer.NextToken();
          return true;
        } else {
          Error("expect } or var or statement for func body but get ");
          return false;
        }
      } else {
        if (!(this->*(itFuncPtr->second))()) {
          return false;
        }
      }
    }
  }
}

void MIRParser::ParseStmtBlockForSeenComment(BlockNode* blk, uint32 mplNum) {
  // collect accumulated comments into comment statement nodes
  if (!lexer.seenComments.empty()) {
    for (size_t i = 0; i < lexer.seenComments.size(); i++) {
      CommentNode *cmnt = mod.CurFuncCodeMemPool()->New<CommentNode>(&mod);
      cmnt->comment = lexer.seenComments[i];
      SetSrcPos(cmnt, mplNum);
      blk->AddStatement(cmnt);
    }
    lexer.GetSeenComments().clear();
  }
}

bool MIRParser::ParseStmtBlockForVar(TokenKind stmtTK) {
  MIRFunction *fn = paramCurrFuncForParseStmtBlock;
  MIRSymbol *st = fn->symTab->CreateSymbol(kScopeLocal);
  st->SetStorageClass(kScAuto);
  st->sKind = kStVar;
  if (stmtTK == TK_tempvar) {
    st->isTmp = true;
  }
  if (!ParseDeclareVar(st)) {
    return false;
  }
  if (!fn->symTab->AddToStringSymbolMap(st)) {
    Error("duplicate declare symbol parse function ");
    return false;
  }
  return true;
}

bool MIRParser::ParseStmtBlockForVar() {
  return ParseStmtBlockForVar(TK_var);
}

bool MIRParser::ParseStmtBlockForTempVar() {
  return ParseStmtBlockForVar(TK_tempvar);
}

bool MIRParser::ParseStmtBlockForReg() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_preg) {
    Error("expect %%preg after reg");
    return false;
  }
  PregIdx pregidx;
  if (!ParsePseudoreg(PTY_ref, pregidx)) {
    return false;
  }
  MIRPreg *preg = mod.CurFunction()->pregTab->PregFromPregIdx(pregidx);
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("ParseDeclareVar failed when parsing the type");
    return false;
  }
  CHECK_FATAL(tyIdx.GetIdx() > 0, "parse declare var failed ");
  MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  preg->mirType = mirType;
  if (lexer.GetTokenKind() == TK_intconst) {
    int64 theintval = lexer.GetTheIntVal();
    if (theintval != 0 && theintval != 1) {
      Error("parseDeclareReg failed");
      return false;
    }
    preg->needRC = theintval == 0 ? false : true;
  } else {
    Error("parseDeclareReg failed");
    return false;
  }
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtBlockForType() {
  paramParseLocalType = true;
  if (!ParseTypeDef()) {
    return false;
  }
  return true;
}

bool MIRParser::ParseStmtBlockForFrameSize() {
  MIRFunction *fn = paramCurrFuncForParseStmtBlock;
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect integer after frameSize but get ");
    return false;
  }
  fn->frameSize = lexer.GetTheIntVal();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtBlockForUpformalSize() {
  MIRFunction *fn = paramCurrFuncForParseStmtBlock;
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect integer after upFormalSize but get ");
    return false;
  }
  fn->upFormalSize = lexer.GetTheIntVal();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtBlockForModuleID() {
  MIRFunction *fn = paramCurrFuncForParseStmtBlock;
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect integer after moduleid but get ");
    return false;
  }
  fn->moduleID = lexer.GetTheIntVal();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtBlockForFuncSize() {
  MIRFunction *fn = paramCurrFuncForParseStmtBlock;
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect integer after funcSize but get ");
    return false;
  }
  fn->funcSize = lexer.GetTheIntVal();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtBlockForFuncID() {
  // funcid is for debugging purpose
  MIRFunction *fn = paramCurrFuncForParseStmtBlock;
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect integer after funcid but get ");
    return false;
  }
  fn->puIdxOrigin = lexer.GetTheIntVal();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseStmtBlockForFormalWordsTypeTagged() {
  MIRFunction *fn = paramCurrFuncForParseStmtBlock;
  uint8 *addr = ParseWordsInfo(fn->upFormalSize);
  if (addr == nullptr) {
    Error("parser error for formalwordstypetagged");
    return false;
  }
  fn->formalWordsTypeTagged = addr;
  return true;
}

bool MIRParser::ParseStmtBlockForLocalWordsTypeTagged() {
  MIRFunction *fn = paramCurrFuncForParseStmtBlock;
  uint8 *addr = ParseWordsInfo(fn->frameSize);
  if (addr == nullptr) {
    Error("parser error for localWordsTypeTagged");
    return false;
  }
  fn->localWordsTypeTagged = addr;
  return true;
}

bool MIRParser::ParseStmtBlockForFormalWordsRefCounted() {
  MIRFunction *fn = paramCurrFuncForParseStmtBlock;
  uint8 *addr = ParseWordsInfo(fn->upFormalSize);
  if (addr == nullptr) {
    Error("parser error for formalwordsrefcounted");
    return false;
  }
  fn->formalWordsRefCounted = addr;
  return true;
}

bool MIRParser::ParseStmtBlockForLocalWordsRefCounted() {
  MIRFunction *fn = paramCurrFuncForParseStmtBlock;
  uint8 *addr = ParseWordsInfo(fn->frameSize);
  if (addr == nullptr) {
    Error("parser error for localwordsrefcounted");
    return false;
  }
  fn->localWordsRefCounted = addr;
  return true;
}

bool MIRParser::ParseStmtBlockForFuncInfo() {
  lexer.NextToken();
  if (!ParseFuncinfo()) {
    return false;
  }
  return true;
}

std::map<TokenKind, MIRParser::FuncPtrParseStmt> MIRParser::InitFuncPtrMapForParseStmt() {
  std::map<TokenKind, MIRParser::FuncPtrParseStmt> funcPtrMap;
  funcPtrMap[TK_dassign] = &MIRParser::ParseStmtDassign;
  funcPtrMap[TK_iassign] = &MIRParser::ParseStmtIassign;
  funcPtrMap[TK_iassignoff] = &MIRParser::ParseStmtIassignoff;
  funcPtrMap[TK_iassignfpoff] = &MIRParser::ParseStmtIassignFPoff;
  funcPtrMap[TK_regassign] = &MIRParser::ParseStmtRegassign;
  funcPtrMap[TK_doloop] = &MIRParser::ParseStmtDoloop;
  funcPtrMap[TK_foreachelem] = &MIRParser::ParseStmtForeachelem;
  funcPtrMap[TK_dowhile] = &MIRParser::ParseStmtDowhile;
  funcPtrMap[TK_if] = &MIRParser::ParseStmtIf;
  funcPtrMap[TK_while] = &MIRParser::ParseStmtWhile;
  funcPtrMap[TK_goto] = &MIRParser::ParseStmtGoto;
  funcPtrMap[TK_brfalse] = &MIRParser::ParseStmtBr;
  funcPtrMap[TK_brtrue] = &MIRParser::ParseStmtBr;
  funcPtrMap[TK_switch] = &MIRParser::ParseStmtSwitch;
  funcPtrMap[TK_rangegoto] = &MIRParser::ParseStmtRangegoto;
  funcPtrMap[TK_multiway] = &MIRParser::ParseStmtMultiway;
  funcPtrMap[TK_call] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_virtualcall] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_virtualicall] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_superclasscall] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_interfacecall] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_interfaceicall] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_customcall] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_polymorphiccall] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_callinstant] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_virtualcallinstant] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_superclasscallinstant] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_interfacecallinstant] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_callassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_virtualcallassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_virtualicallassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_superclasscallassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_interfacecallassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_interfaceicallassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_customcallassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_polymorphiccallassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_callinstantassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_virtualcallinstantassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_superclasscallinstantassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_interfacecallinstantassigned] = &MIRParser::ParseStmtCall;
  funcPtrMap[TK_icall] = &MIRParser::ParseStmtIcall;
  funcPtrMap[TK_icallassigned] = &MIRParser::ParseStmtIcallAssigned;
  funcPtrMap[TK_intrinsiccall] = &MIRParser::ParseStmtIntrinsiccall;
  funcPtrMap[TK_intrinsiccallassigned] = &MIRParser::ParseStmtIntrinsiccallassigned;
  funcPtrMap[TK_xintrinsiccall] = &MIRParser::ParseStmtIntrinsiccall;
  funcPtrMap[TK_xintrinsiccallassigned] = &MIRParser::ParseStmtIntrinsiccallassigned;
  funcPtrMap[TK_intrinsiccallwithtype] = &MIRParser::ParseStmtIntrinsiccallwithtype;
  funcPtrMap[TK_intrinsiccallwithtypeassigned] = &MIRParser::ParseStmtIntrinsiccallwithtypeassigned;
  funcPtrMap[TK_return] = &MIRParser::ParseNaryStmtReturn;
  funcPtrMap[TK_try] = &MIRParser::ParseStmtJavaTry;
  funcPtrMap[TK_jstry] = &MIRParser::ParseStmtJsTry;
  funcPtrMap[TK_javatry] = &MIRParser::ParseStmtJavaTry;
  funcPtrMap[TK_cpptry] = &MIRParser::ParseStmtJavaTry;
  funcPtrMap[TK_catch] = &MIRParser::ParseStmtJavaCatch;
  funcPtrMap[TK_jscatch] = &MIRParser::ParseStmtMarker;
  funcPtrMap[TK_javacatch] = &MIRParser::ParseStmtJavaCatch;
  funcPtrMap[TK_cppcatch] = &MIRParser::ParseStmtCppCatch;
  funcPtrMap[TK_syncenter] = &MIRParser::ParseNaryStmtSyncEnter;
  funcPtrMap[TK_syncexit] = &MIRParser::ParseNaryStmtSyncExit;
  funcPtrMap[TK_throw] = &MIRParser::ParseUnaryStmtThrow;
  funcPtrMap[TK_decref] = &MIRParser::ParseUnaryStmtDecRef;
  funcPtrMap[TK_incref] = &MIRParser::ParseUnaryStmtIncRef;
  funcPtrMap[TK_decrefreset] = &MIRParser::ParseUnaryStmtDecRefReset;
  funcPtrMap[TK_igoto] = &MIRParser::ParseUnaryStmtIGoto;
  funcPtrMap[TK_jscatch] = &MIRParser::ParseStmtMarker;
  funcPtrMap[TK_finally] = &MIRParser::ParseStmtMarker;
  funcPtrMap[TK_cleanuptry] = &MIRParser::ParseStmtMarker;
  funcPtrMap[TK_endtry] = &MIRParser::ParseStmtMarker;
  funcPtrMap[TK_retsub] = &MIRParser::ParseStmtMarker;
  funcPtrMap[TK_membaracquire] = &MIRParser::ParseStmtMarker;
  funcPtrMap[TK_membarrelease] = &MIRParser::ParseStmtMarker;
  funcPtrMap[TK_membarstoreload] = &MIRParser::ParseStmtMarker;
  funcPtrMap[TK_membarstorestore] = &MIRParser::ParseStmtMarker;
  funcPtrMap[TK_gosub] = &MIRParser::ParseStmtGosub;
  funcPtrMap[TK_eval] = &MIRParser::ParseUnaryStmtEval;
  funcPtrMap[TK_free] = &MIRParser::ParseUnaryStmtFree;
  funcPtrMap[TK_assertnonnull] = &MIRParser::ParseUnaryStmtAssertNonNull;
  funcPtrMap[TK_assertge] = &MIRParser::ParseBinaryStmtAssertGE;
  funcPtrMap[TK_assertlt] = &MIRParser::ParseBinaryStmtAssertLT;
  funcPtrMap[TK_label] = &MIRParser::ParseStmtLabel;
  funcPtrMap[TK_LOC] = &MIRParser::ParseLoc;
  funcPtrMap[TK_ALIAS] = &MIRParser::ParseAlias;
  return funcPtrMap;
}

std::map<TokenKind, MIRParser::FuncPtrParseStmtBlock> MIRParser::InitFuncPtrMapForParseStmtBlock() {
  std::map<TokenKind, MIRParser::FuncPtrParseStmtBlock> funcPtrMap;
  funcPtrMap[TK_var] = &MIRParser::ParseStmtBlockForVar;
  funcPtrMap[TK_tempvar] = &MIRParser::ParseStmtBlockForTempVar;
  funcPtrMap[TK_reg] = &MIRParser::ParseStmtBlockForReg;
  funcPtrMap[TK_type] = &MIRParser::ParseStmtBlockForType;
  funcPtrMap[TK_framesize] = &MIRParser::ParseStmtBlockForFrameSize;
  funcPtrMap[TK_upformalsize] = &MIRParser::ParseStmtBlockForUpformalSize;
  funcPtrMap[TK_moduleid] = &MIRParser::ParseStmtBlockForModuleID;
  funcPtrMap[TK_funcsize] = &MIRParser::ParseStmtBlockForFuncSize;
  funcPtrMap[TK_funcid] = &MIRParser::ParseStmtBlockForFuncID;
  funcPtrMap[TK_formalwordstypetagged] = &MIRParser::ParseStmtBlockForFormalWordsTypeTagged;
  funcPtrMap[TK_localwordstypetagged] = &MIRParser::ParseStmtBlockForLocalWordsTypeTagged;
  funcPtrMap[TK_formalwordsrefcounted] = &MIRParser::ParseStmtBlockForFormalWordsRefCounted;
  funcPtrMap[TK_localwordsrefcounted] = &MIRParser::ParseStmtBlockForLocalWordsRefCounted;
  funcPtrMap[TK_funcinfo] = &MIRParser::ParseStmtBlockForFuncInfo;
  return funcPtrMap;
}

void MIRParser::SetSrcPos(StmtNode *stmt, uint32 mplNum) {
  stmt->srcPosition.SetFilenum(lastFileNum);
  stmt->srcPosition.SetLinenum(lastLineNum);
  stmt->srcPosition.SetMplLinenum(mplNum);
}

}  // namespace maple
