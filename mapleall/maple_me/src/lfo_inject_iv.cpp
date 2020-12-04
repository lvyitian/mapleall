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

#include "lfo_inject_iv.h"
#include "dominance.h"
#include "me_ident_loops.h"
#include "me_option.h"
#include "mir_builder.h"
#include <string>

namespace maple {

AnalysisResult *DoLfoInjectIV::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func, !MeOption::quiet));
  CHECK_FATAL(dom, "dominance phase has problem");
  IdentifyLoops *identloops = static_cast<IdentifyLoops *>(m->GetAnalysisResult(MeFuncPhase_IDENTLOOPS, func, !MeOption::quiet));
  CHECK_FATAL(identloops != nullptr, "identloops has problem");

  uint32 ivCount = 0;
  MIRBuilder *mirbuilder = func->mirModule.mirBuilder;
  LfoFunction *lfoFunc = func->lfoFunc;

  for (LoopDesc *aloop : identloops->meloops) {
    BB *headbb = aloop->head;
    // check if the label has associated LfoWhileInfo
    if (headbb->bbLabel == 0) {
      continue;
    }
    if (headbb->pred.size() != 2) {
      continue;  // won't insert IV for loops with > 1 tail bbs
    }
    MapleMap<LabelIdx, LfoWhileInfo*>::iterator it = lfoFunc->label2WhileInfo.find(headbb->bbLabel);
    if (it == lfoFunc->label2WhileInfo.end()) {
      continue;
    }
    LfoWhileInfo *whileInfo = it->second;
    // find the entry BB as the predecessor of headbb that dominates headbb
    MapleVector<BB*>::iterator predit = headbb->pred.begin();
    for ( ; predit != headbb->pred.end(); predit++) {
      if (dom->Dominate(*predit, headbb))
        break;
    }
    if (predit == headbb->pred.end()) {
      continue;  // cannot insert IV for this loop
    }
    BB *entrybb = *predit;

    // create the IV for this loop
    std::string ivName("injected.iv");
    ivName.append(std::to_string(++ivCount));
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(ivName);
    MIRSymbol *st = mirbuilder->CreateSymbol((TyIdx)PTY_i64, strIdx, kStVar, kScAuto, kScopeLocal, func->mirFunc);
    whileInfo->injectedIVSym = st;
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "****** Injected IV " << st->GetName() << " in while loop at label ";
      LogInfo::MapleLogger() << "@" << func->mirFunc->GetLabelName(headbb->bbLabel) << std::endl;
    }

    // initialize IV to 0 at loop entry
    DassignNode *dass = mirbuilder->CreateStmtDassign(st, 0, mirbuilder->CreateIntConst(0, PTY_i64));
    StmtNode *laststmt = entrybb->stmtNodeList.last;
    if (laststmt &&
        (laststmt->op == OP_brfalse || laststmt->op == OP_brtrue || laststmt->op == OP_goto || laststmt->op == OP_igoto || laststmt->op == OP_switch)) {
      entrybb->InsertStmtBefore(laststmt, dass);
    } else {
      entrybb->AddStmtNode(dass);
    }

    // insert IV increment at loop tail BB
    BB *tailbb = aloop->tail;
    AddrofNode *dread = mirbuilder->CreateExprDread(GlobalTables::GetTypeTable().GetInt64(), 0, st);
    BinaryNode *addnode = mirbuilder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetInt64(), dread, mirbuilder->CreateIntConst(1, PTY_i64));
    dass = mirbuilder->CreateStmtDassign(st, 0, addnode);
    laststmt = tailbb->stmtNodeList.last;
    tailbb->InsertStmtBefore(laststmt, dass);
  }
  return nullptr;
}

}  // namespace maple
