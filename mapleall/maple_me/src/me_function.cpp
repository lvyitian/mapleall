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

#include <iostream>
#include <fstream>
#include "me_function.h"
#include "ssa_mir_nodes.h"
#include "me_ssa.h"
#include "me_cfg.h"
#include "mir_lower.h"
#include "mir_builder.h"
#include "constant_fold.h"
#include "me_irmap.h"
#include "me_phase.h"

#define JAVALANG (mirModule.IsJavaModule())

using namespace std;

namespace maple {
#if DEBUG
MIRModule *g_mirmodule = nullptr;
MeFunction *g_func = nullptr;
MeIRMap *g_irmap = nullptr;
SSATab *g_ssatab = nullptr;
#endif

void MeFunction::DumpFunction() {
  for (BB *bb : theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    bb->DumpHeader(&mirModule);

    MapleMap<OriginalSt *, PhiNode>::iterator phiIt;
    for (phiIt = bb->phiList->begin(); phiIt != bb->phiList->end(); phiIt++) {
      (*phiIt).second.Dump(&mirModule);
    }

    for (auto stmt : bb->stmtNodeList) {
      GenericSSAPrint(&mirModule, stmt, 1, &meSSATab->stmtsSSAPart);
    }
  }
}

void MeFunction::DumpFunctionNoSSA() {
  for (BB *bb : theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    bb->DumpHeader(&mirModule);

    MapleMap<OriginalSt *, PhiNode>::iterator phiIt;
    for (phiIt = bb->phiList->begin(); phiIt != bb->phiList->end(); phiIt++) {
      (*phiIt).second.Dump(&mirModule);
    }

    for (auto stmt : bb->stmtNodeList) {
      stmt->Dump(&mirModule, 1);
    }
  }
}

void MeFunction::DumpMeFunc() {
  for (BB *bb : theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }

    bb->DumpHeader(&mirModule);
    bb->DumpMePhiList(irMap);
    for (auto mestmt : bb->meStmtList) {
      mestmt->Dump(irMap);
    }
  }
}

void MeFunction::DumpMayDUFunction() {
  for (BB *bb : theCFG->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    bb->DumpHeader(&mirModule);

    bool skipStmt = false;
    for (auto stmt : bb->stmtNodeList) {
      if (HasMayDef(stmt, meSSATab) || HasMayUseOpnd(stmt, meSSATab) || HasMallocOpnd(stmt)) {
        if (skipStmt) {
          mirModule.out << "......" << endl;
        }
        GenericSSAPrint(&mirModule, stmt, 1, &meSSATab->stmtsSSAPart);
        skipStmt = false;
      } else {
        skipStmt = true;
      }
    }
    if (skipStmt) {
      mirModule.out << "......" << endl;
    }
  }
}

void MeFunction::SetTryBlockInfo(StmtNode *javatryStmt, BB *lastjavatryBb, const StmtNode *nextstmt, BB *curbb,
                                 BB *newbb) {
  ASSERT(javatryStmt != nullptr, "");
  if (nextstmt->op == OP_endtry) {
    curbb->isTryEnd = true;
    ASSERT(lastjavatryBb != nullptr, "");
    theCFG->endTryBB2TryBB[curbb] = lastjavatryBb;
  } else {
    newbb->isTry = true;
    theCFG->bbTryNodeMap[newbb] = javatryStmt;
  }
}

BaseNode *CreateDummyReturnValue(MIRBuilder *mirBuilder, MIRType *retty) {
  if (IsPrimitiveInteger(retty->primType)) {
    return mirBuilder->CreateIntConst(0, retty->primType);
  }
  switch (retty->primType) {
  case PTY_void: return nullptr;
  case PTY_f32: return mirBuilder->CreateFloatConst(0.0f);
  case PTY_f64: return mirBuilder->CreateDoubleConst(0.0);
  case PTY_f128: {
    uint64 constzero = 0;
    return mirBuilder->CreateFloat128Const(&constzero);
  }
  default: {  // create a dummy local var with the return type
    static uint32 tempCount = 0;
    std::string tempstr = string("__DummyRetTemp.").append(to_string(++tempCount));
    MIRSymbol *st = mirBuilder->CreateLocalDecl(tempstr.c_str(), retty);
    return mirBuilder->CreateExprDread(retty, 0, st);
  }
  }
  return nullptr;
}

void MeFunction::CreateBasicBlocks(MirCFG *cfg) {
  if (mirFunc->IsEmpty()) {
    if (!MeOption::quiet) {
      LogInfo::MapleLogger() << "function is empty, cfg is nullptr\n";
    }
    return;
  }
  /* create common_entry/exit bb first as bbVec[0] and bbVec[1] */
  cfg->commonEntryBB = NewBasicBlock();
  cfg->commonEntryBB->isEntry = true;
  cfg->commonExitBB = NewBasicBlock();
  cfg->commonExitBB->isExit = true;

  cfg->first_bb = NewBasicBlock();
  cfg->first_bb->isEntry = true;
  StmtNode *nextstmt = mirFunc->body->GetFirst();
  ASSERT(nextstmt != nullptr, "function has no statement");
  BB *curbb = cfg->first_bb;
  std::stack<StmtNode *> tryStmtStack;
  std::stack<BB *> tryBBStack;      // bb containing javatry_stmt
  do {
    StmtNode *stmt = nextstmt;
    nextstmt = stmt->GetNext();
    switch (stmt->op) {
      case OP_goto: {
        if (curbb->IsEmpty()) {
          curbb->SetFirst(stmt);
        }
        curbb->SetLast(stmt);
        curbb->kind = kBBGoto;
        if (nextstmt) {
          BB *newbb = NewBasicBlock();
          if (JAVALANG && !tryStmtStack.empty()) {
            SetTryBlockInfo(tryStmtStack.top(), tryBBStack.top(), nextstmt, curbb, newbb);
          }
          curbb = newbb;
        }
        break;
      }
      case OP_igoto: {
        if (curbb->IsEmpty()) {
          curbb->SetFirst(stmt);
        }
        curbb->SetLast(stmt);
        curbb->kind = kBBIgoto;
        if (nextstmt) {
          curbb = NewBasicBlock();
        }
        break;
      }
      case OP_dassign: {
        if (curbb->IsEmpty()) {
          curbb->SetFirst(stmt);
        }

        if (JAVALANG && static_cast<DassignNode *>(stmt)->GetRhs()->MayThrowException()) {
          stmt->op = OP_maydassign;
          if (!tryStmtStack.empty()) {
            // breaks new BB only inside try blocks
            curbb->SetLast(stmt);
            curbb->kind = kBBFallthru;
            BB *newbb = NewBasicBlock();
            SetTryBlockInfo(tryStmtStack.top(), tryBBStack.top(), nextstmt, curbb, newbb);
            curbb = newbb;
            break;
          }
        }
        if ((nextstmt == nullptr) && (curbb->stmtNodeList.last == nullptr)) {
          curbb->SetLast(stmt);
        }
        break;
      }
      case OP_brfalse:
      case OP_brtrue: {
        if (curbb->IsEmpty()) {
          curbb->SetFirst(stmt);
        }
        curbb->SetLast(stmt);
        curbb->kind = kBBCondGoto;
        BB *newbb = NewBasicBlock();
        if (JAVALANG && !tryStmtStack.empty()) {
          SetTryBlockInfo(tryStmtStack.top(), tryBBStack.top(), nextstmt, curbb, newbb);
        }
        curbb = newbb;
        break;
      }
      case OP_if:
      case OP_doloop:
      case OP_dowhile:
      case OP_while: {
        CHECK_FATAL(false, "NYI");
        break;
      }
      case OP_throw:
        if (JAVALANG && !tryStmtStack.empty()) {
          // handle as goto
          if (curbb->IsEmpty()) {
            curbb->SetFirst(stmt);
          }
          curbb->SetLast(stmt);
          curbb->kind = kBBGoto;
          if (nextstmt) {
            BB *newbb = NewBasicBlock();
            SetTryBlockInfo(tryStmtStack.top(), tryBBStack.top(), nextstmt, curbb, newbb);
            curbb = newbb;
          }
          break;
        }
      // fall thru to handle as return
      case OP_gosub:
      case OP_retsub:
      case OP_return: {
        if (curbb->IsEmpty()) {
          curbb->SetFirst(stmt);
        }
        curbb->SetLast(stmt);
        curbb->kind = kBBReturn;
        curbb->isExit = true;
        if (nextstmt) {
          BB *newbb = NewBasicBlock();
          if (JAVALANG && !tryStmtStack.empty()) {
            SetTryBlockInfo(tryStmtStack.top(), tryBBStack.top(), nextstmt, curbb, newbb);
          }
          curbb = newbb;
          if (stmt->op == OP_gosub) {
            curbb->isEntry = true;
          }
        }
        break;
      }
      case OP_endtry: {
        ASSERT(!tryStmtStack.empty(), "");
        if (!curbb->IsEmpty()) {
          StmtNode *laststmt = stmt->GetPrev();
          CHECK_FATAL((curbb->stmtNodeList.last == nullptr || curbb->stmtNodeList.last == laststmt), "something wrong building BB");
          curbb->SetLast(laststmt);
          if (curbb->kind == kBBUnknown) {
            curbb->kind = kBBFallthru;
          }
          curbb->isTryEnd = true;
          cfg->endTryBB2TryBB[curbb] = tryBBStack.top();
          curbb = NewBasicBlock();
        } else {
          // endtry has already been processed in SetTryBlockInfo() for java
          if (!JAVALANG || curbb->bbLabel != 0) {
            // create the empty BB
            curbb->kind = kBBFallthru;
            curbb->isTryEnd = true;
            cfg->endTryBB2TryBB[curbb] = tryBBStack.top();
            curbb = NewBasicBlock();
          }
        }
        tryStmtStack.pop();
        tryBBStack.pop();
        break;
      }
      case OP_cpptry:
      case OP_try:
      case OP_javatry: {
        // start a new bb
        if (!curbb->IsEmpty()) {
          // prepare a new bb
          StmtNode *laststmt = stmt->GetPrev();
          CHECK_FATAL((curbb->stmtNodeList.last == nullptr || curbb->stmtNodeList.last == laststmt), "something wrong building BB");
          curbb->SetLast(laststmt);
          if (curbb->kind == kBBUnknown) {
            curbb->kind = kBBFallthru;
          }
          BB *newbb = NewBasicBlock();
          // java has no nested javatry, so no need to call SetTryBlockInfo()
          curbb = newbb;
        }
        curbb->SetFirst(stmt);
        tryStmtStack.push(stmt);
        tryBBStack.push(curbb);
        curbb->isTry = true;
        if (JAVALANG) {
          cfg->bbTryNodeMap[curbb] = tryStmtStack.top();
          // prepare a new bb that contains only a OP_javatry. It is needed
          // to work correctly: assignments in a try block should not affect
          // assignments before the try block as exceptions might occur.
          curbb->SetLast(stmt);
          curbb->kind = kBBFallthru;
          BB *newbb = NewBasicBlock();
          SetTryBlockInfo(tryStmtStack.top(), tryBBStack.top(), nextstmt, curbb, newbb);
          curbb = newbb;
        }
        break;
      }
      case OP_cppcatch: {
        // start a new bb
        if (!curbb->IsEmpty()) {
          // prepare a new bb
          StmtNode *laststmt = stmt->GetPrev();
          StmtNode *curLast = curbb->stmtNodeList.last;
          CHECK_FATAL((curLast == nullptr || curLast == laststmt), "something wrong building BB");
          curbb->SetLast(laststmt);
          if (curbb->kind == kBBUnknown) {
            curbb->kind = kBBFallthru;
          }
          BB *newbb = NewBasicBlock();
          curbb = newbb;
        }
        curbb->SetFirst(stmt);
        curbb->isCatch = true;
        curbb->isEntry = true;
        break;
      }
      case OP_catch:
      case OP_javacatch: {
        // start a new bb
        if (!curbb->IsEmpty()) {
          // prepare a new bb
          StmtNode *laststmt = stmt->GetPrev();
          StmtNode *curLast = curbb->stmtNodeList.last;
          CHECK_FATAL((curLast == nullptr || curLast == laststmt), "something wrong building BB");
          curbb->SetLast(laststmt);
          if (curbb->kind == kBBUnknown) {
            curbb->kind = kBBFallthru;
          }
          BB *newbb = NewBasicBlock();
          if (!tryStmtStack.empty()) {
            SetTryBlockInfo(tryStmtStack.top(), tryBBStack.top(), nextstmt, curbb, newbb);
          }
          curbb = newbb;
        }
        curbb->SetFirst(stmt);
        curbb->isCatch = true;
        CatchNode *catchnode = static_cast<CatchNode *>(stmt);
        // ASSERT(catchnode->exceptionTyIdxVec.size() == 1, "TODO: handle exceptionTyIdxVec");
        const MapleVector<TyIdx> &exceptiontyidxvec = catchnode->exceptionTyIdxVec;
        for (uint32 i = 0; i < exceptiontyidxvec.size(); i++) {
          MIRType *etype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(exceptiontyidxvec[i]);
          ASSERT(
            etype != nullptr && (etype->GetPrimType() == maple::PTY_ptr || etype->GetPrimType() == maple::PTY_ref), "");
          MIRPtrType *eptype = static_cast<MIRPtrType *>(etype);
          MIRType *pointtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(eptype->pointedTyIdx);
          const std::string &ename = GlobalTables::GetStrTable().GetStringFromStrIdx(pointtype->nameStrIdx);
          if ((pointtype->primType == maple::PTY_void) || (ename.compare("Ljava/lang/Throwable;") == 0) ||
              (ename.compare("Ljava/lang/Exception;") == 0)) {
            // "Ljava/lang/Exception;" is risk to set isJavaFinally because it
            // only deal with "throw exception". if throw error,  it's wrong
            curbb->isJavaFinally = true;  // this is a start of finally handler
          }
        }
        break;
      }
      case OP_label: {
        LabelNode *lblnode = static_cast<LabelNode *>(stmt);
        LabelIdx labidx = lblnode->labelIdx;
        if (!curbb->IsEmpty() || curbb->bbLabel != 0) {
          // prepare a new bb
          StmtNode *laststmt = stmt->GetPrev();
          StmtNode *curLast = curbb->stmtNodeList.last;
          CHECK_FATAL((curLast == nullptr || curLast == laststmt), "something wrong building BB");
          if ((curLast == nullptr) && (laststmt->op != OP_label)) {
            if (mirModule.IsJavaModule() && laststmt->op == OP_endtry) {
              if (curbb->stmtNodeList.first == nullptr) {
                curbb->SetLast(nullptr);
              } else {
                // find a valid stmt which is not label or endtry
                StmtNode *p = laststmt->GetPrev();
                ASSERT(p && (p->op != OP_label) && (p->op != OP_endtry), "");
                curbb->SetLast(p);
              }
            } else {
              curbb->SetLast(laststmt);
            }
          }
          if (curbb->kind == kBBUnknown) {
            curbb->kind = kBBFallthru;
          }
          BB *newbb = NewBasicBlock();
          if (!tryStmtStack.empty()) {
            newbb->isTry = true;
            if (JAVALANG) {
              cfg->bbTryNodeMap[newbb] = tryStmtStack.top();
            }
          }
          curbb = newbb;
        }
        cfg->labelBBIdMap[labidx] = curbb;
        curbb->bbLabel = labidx;
        break;
      }
      case OP_jscatch: {
        if (curbb->IsEmpty()) {
          curbb->SetFirst(stmt);
        }
        curbb->isEntry = true;
        curbb->isJSCatch = true;
        break;
      }
      case OP_finally: {
        CHECK_FATAL(curbb->IsEmpty(), "");
        curbb->SetFirst(stmt);
        curbb->isEntry = true;
        curbb->isJSFinally = true;
        break;
      }
      case OP_switch: {
        if (curbb->IsEmpty()) {
          curbb->SetFirst(stmt);
        }
        curbb->SetLast(stmt);
        curbb->kind = kBBSwitch;
        BB *newbb = NewBasicBlock();
        if (JAVALANG && !tryStmtStack.empty()) {
          SetTryBlockInfo(tryStmtStack.top(), tryBBStack.top(), nextstmt, curbb, newbb);
        }
        curbb = newbb;
        break;
      }
      default: {
        if (curbb->IsEmpty()) {
          curbb->SetFirst(stmt);
        }
        if ((nextstmt == nullptr) && (curbb->stmtNodeList.last == nullptr)) {
          curbb->SetLast(stmt);
        }
        break;
      }
    }
  } while (nextstmt);
  ASSERT(tryStmtStack.empty(), "CreateBasicBlcoks: missing endtry");
  ASSERT(tryBBStack.empty(), "CreateBasicBlocks: javatry and endtry should be one-to-one mapping");
  cfg->last_bb = curbb;
  if (cfg->last_bb->IsEmpty() || cfg->last_bb->kind == kBBUnknown) {
    // insert a return statement
    MIRType *retty = mirModule.CurFunction()->GetReturnType();
    BaseNode *retOpnd = CreateDummyReturnValue(mirModule.mirBuilder, retty);
    cfg->last_bb->stmtNodeList.push_back(mirModule.mirBuilder->CreateStmtReturn(retOpnd));
    cfg->last_bb->isExit = true;
    cfg->last_bb->kind = kBBReturn;
  }
  return;
}

void MeFunction::Verify() {
  theCFG->Verify();
  theCFG->VerifyLabels();
}

BB *MeFunction::NewBasicBlock() {
  BB *newbb = theCFG->cfgAlloc.mp->New<BB>(&theCFG->cfgAlloc, BBId(theCFG->nextBBId++));
  theCFG->bbVec.push_back(newbb);
  return newbb;
}

/* clone stmtnode in orig bb to newbb */
void MeFunction::CloneBasicBlock(BB *newbb, BB *orig) {
  if (orig == nullptr || orig->IsEmpty()) {
    return;
  }
  for (auto stmt : orig->stmtNodeList) {
    StmtNode *newStmt = static_cast<StmtNode *>(stmt->CloneTree(&mirModule));
    newStmt->SetNext(nullptr);
    newStmt->SetPrev(nullptr);
    newbb->AddStmtNode(newStmt);
    if (meSSATab != nullptr) {
      meSSATab->CreateSSAStmt(newStmt, newbb);
    }
  }
}

/* Split BB at split_point */
BB *MeFunction::SplitBB(BB *bb, StmtNode *splitPoint) {
  BB *newbb = NewBasicBlock();
  StmtNode *newBbStart = splitPoint->GetNext();

  // Fix Stmt in BB.

  if (newBbStart) {
    newBbStart->SetPrev(nullptr);

    for (StmtNode *stmt = newBbStart; stmt != nullptr;) {
      StmtNode *nextStmt = stmt->GetNext();
      newbb->AddStmtNode(stmt);
      if (meSSATab != nullptr) {
        meSSATab->CreateSSAStmt(stmt, newbb);
      }
      stmt = nextStmt;
    }
  }

  bb->stmtNodeList.update_back(splitPoint);
  splitPoint->SetNext(nullptr);

  // Fix BB in CFG
  newbb->kind = bb->kind;
  bb->kind = kBBFallthru;

  // Special Case: commonExitBB is orig bb's succ
  for (uint32 i = 0; i < theCFG->commonExitBB->pred.size(); i++) {
    if (theCFG->commonExitBB->pred[i] == bb) {
      theCFG->commonExitBB->pred[i] = newbb;
      break;
    }
  }

  for (uint32 i = 0; i < bb->succ.size(); i++) {
    BB *succ = bb->succ[i];
    succ->ReplacePred(bb, newbb);
  }

  bb->succ.clear();
  bb->succ.push_back(newbb);
  newbb->pred.push_back(bb);

  // Setup flags
  newbb->CopyFlagsAfterSplit(bb);
  newbb->isTryEnd = bb->isTryEnd;
  theCFG->endTryBB2TryBB[newbb] = theCFG->endTryBB2TryBB[bb];
  bb->isExit = false;
  bb->isTryEnd = false;
  return newbb;
}

/* create label for bb */
void MeFunction::CreateBBLabel(BB *bb) {
  if (bb->bbLabel != 0) {
    return;
  }

  LabelIdx label = mirFunc->labelTab->CreateLabelWithPrefix('m');
  mirFunc->labelTab->AddToStringLabelMap(label);
  bb->bbLabel = label;
  theCFG->labelBBIdMap.insert(make_pair(label, bb));
}

// Recognize the following kind of simple pattern and remove corresponding EH edges
// @label78044   javacatch { <* void> }
//  dassign %Reg0_R524935 0 (regread ptr %%thrownval)
//  syncexit (dread ref %Reg8_R460958)
//  endtry
//  throw (dread ptr %Reg0_R524935)
void MeFunction::RemoveEhEdgesInSyncRegion() {
  if (theCFG->endTryBB2TryBB.size() != 1) {
    return;
  }

  for (auto iter : theCFG->endTryBB2TryBB) {
    BB *tryBB = iter.second;
    BB *endtryBB = iter.first;
    // Filter out complex cases
    if (!(endtryBB->IsCatch() || endtryBB->isJSCatch) || endtryBB->kind != kBBFallthru || !endtryBB->IsEndTry() ||
        !endtryBB->IsJavaFinally() || endtryBB->stmtNodeList.last->op != OP_syncexit || (tryBB->stmtNodeList.last->op != OP_javatry && tryBB->stmtNodeList.last->op != OP_try)) {
      return;
    }
    for (auto it : theCFG->bbTryNodeMap) {
      BB *bb = it.first;
      if (bb != tryBB && bb != endtryBB) {
        for (auto stmt : bb->stmtNodeList) {
          if (stmt->op == OP_javatry || stmt->op == OP_try ||
              stmt->op == OP_javacatch || stmt->op == OP_catch || stmt->op == OP_throw) {
            return;
          }
        }
      }
    }

    // Unmark unnecessary isTry flags
    for (auto it : theCFG->bbTryNodeMap) {
      BB *bb = it.first;
      if (bb != tryBB && bb != endtryBB) {
        bb->isTry = false;
      }
    }
  }
}

}  // namespace maple
