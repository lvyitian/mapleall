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

#include "mir_builder.h"
#include "lfo_mir_lower.h"

using namespace maple;

// is lowered to :
// label <whilelabel>
// brfalse <cond> <endlabel>
// <body>
// goto <whilelabel>
// label <endlabel>

BlockNode *LFOMIRLower::LowerWhileStmt(WhileStmtNode *whilestmt) {
  MIRBuilder *mirbuilder = mirModule.mirBuilder;
//DoCondVarProp(whilestmt);
  whilestmt->body = LowerBlock(whilestmt->body);
  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  LabelIdx whilelblidx = func->mirFunc->labelTab->CreateLabel();
  mirModule.CurFunction()->labelTab->AddToStringLabelMap(whilelblidx);
  LabelNode *whilelblstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
  whilelblstmt->labelIdx = whilelblidx;
  LfoWhileInfo *whileInfo = lfoFunc->lfomp->New<LfoWhileInfo>();
  lfoFunc->SetLabelCreatedByLfo(whilelblidx);
  lfoFunc->label2WhileInfo.insert(std::make_pair(whilelblidx, whileInfo));
  blk->AddStatement(whilelblstmt);
  CondGotoNode *brfalsestmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(OP_brfalse);
  brfalsestmt->uOpnd = whilestmt->uOpnd;
  brfalsestmt->srcPosition = whilestmt->srcPosition;
  // add jump label target later
  blk->AddStatement(brfalsestmt);

  // creaate body
  LabelIdx bodylidx = mirModule.CurFunction()->labelTab->CreateLabel();
  CHECK_FATAL(whilestmt->body, "null ptr check");
  blk->AppendStatementsFromBlock(whilestmt->body);
  GotoNode *whilegotonode = mirbuilder->CreateStmtGoto(OP_goto, whilelblidx);
  blk->AddStatement(whilegotonode);
  // create endlabel
  LabelIdx endlblidx = mirModule.CurFunction()->labelTab->CreateLabel();
  lfoFunc->SetLabelCreatedByLfo(endlblidx);
  mirModule.CurFunction()->labelTab->AddToStringLabelMap(endlblidx);
  LabelNode *endlblstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
  endlblstmt->labelIdx = endlblidx;
  brfalsestmt->offset = endlblidx;
  blk->AddStatement(endlblstmt);
  return blk;
}

BlockNode *LFOMIRLower::LowerIfStmt(IfStmtNode *ifstmt, bool recursive) {
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
  MIRFunction *mirFunc = func->mirFunc;
  MIRBuilder *mirbuilder = mirModule.mirBuilder;

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
    LabelIdx endlabelidx = mirFunc->labelTab->CreateLabel();
    mirFunc->labelTab->AddToStringLabelMap(endlabelidx);
    lfoFunc->SetLabelCreatedByLfo(endlabelidx);
    LfoIfInfo *ifInfo = lfoFunc->lfomp->New<LfoIfInfo>();
    brfalsestmt->offset = endlabelidx;
    blk->AddStatement(brfalsestmt);

    blk->AppendStatementsFromBlock(ifstmt->thenPart);

    LabelNode *labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
    labstmt->labelIdx = endlabelidx;
    ifInfo->endLabel = endlabelidx;
    lfoFunc->label2IfInfo.insert(std::make_pair(endlabelidx, ifInfo));
    blk->AddStatement(labstmt);
  } else if (thenempty) {
    // brtrue <cond> <endlabel>
    // <elsePart>
    // label <endlabel>
    CondGotoNode *brtruestmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(OP_brtrue);
    brtruestmt->uOpnd = ifstmt->uOpnd;
    brtruestmt->srcPosition = ifstmt->srcPosition;
    LabelIdx endlabelidx = mirFunc->labelTab->CreateLabel();
    lfoFunc->SetLabelCreatedByLfo(endlabelidx);
    LfoIfInfo *ifInfo = lfoFunc->lfomp->New<LfoIfInfo>();
    mirFunc->labelTab->AddToStringLabelMap(endlabelidx);
    brtruestmt->offset = endlabelidx;
    blk->AddStatement(brtruestmt);

    blk->AppendStatementsFromBlock(ifstmt->elsePart);

    LabelNode *labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
    labstmt->labelIdx = endlabelidx;
    ifInfo->endLabel = endlabelidx;
    lfoFunc->label2IfInfo.insert(std::make_pair(endlabelidx, ifInfo));
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
    LabelIdx elselabelidx = mirFunc->labelTab->CreateLabel();
    mirFunc->labelTab->AddToStringLabelMap(elselabelidx);
    lfoFunc->SetLabelCreatedByLfo(elselabelidx);
    LfoIfInfo *ifInfo = lfoFunc->lfomp->New<LfoIfInfo>();
    brfalsestmt->offset = elselabelidx;
    blk->AddStatement(brfalsestmt);
    ifInfo->elseLabel = elselabelidx;
    lfoFunc->label2IfInfo.insert(std::make_pair(elselabelidx, ifInfo));

    blk->AppendStatementsFromBlock(ifstmt->thenPart);
    bool fallthru_from_then = !BlockNoFallThru(ifstmt->thenPart);
    LabelIdx endlabelidx = 0;

    if (fallthru_from_then) {
      GotoNode *gotostmt = mirModule.CurFuncCodeMemPool()->New<GotoNode>(OP_goto);
      endlabelidx = mirFunc->labelTab->CreateLabel();
      mirFunc->labelTab->AddToStringLabelMap(endlabelidx);
      lfoFunc->SetLabelCreatedByLfo(endlabelidx);
      gotostmt->offset = endlabelidx;
      blk->AddStatement(gotostmt);
    }

    LabelNode *labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
    labstmt->labelIdx = elselabelidx;
    blk->AddStatement(labstmt);

    blk->AppendStatementsFromBlock(ifstmt->elsePart);

    if (fallthru_from_then) {
      labstmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
      labstmt->labelIdx = endlabelidx;
      blk->AddStatement(labstmt);
    }
    if (endlabelidx == 0) {  // create end label
      endlabelidx = mirbuilder->CreateLabidx(mirFunc);
      lfoFunc->SetLabelCreatedByLfo(endlabelidx);
      LabelNode *endlabelnode = mirbuilder->CreateStmtLabel(endlabelidx);
      blk->AddStatement(endlabelnode);
    }
    ifInfo->endLabel = endlabelidx;
  }
  return blk;
}
