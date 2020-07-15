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

void MeFunction::PartialInit(bool issecondpass) {
  theCFG = nullptr;
  irMap = nullptr;

  regNum = 0;
  hasEH = false;
  secondPass = issecondpass;
  // if (!issecondpass) { // TODO: this is a hack to avoid cond jump to be eliminated
  maple::ConstantFold cf(&mirModule);
  cf.Simplify(mirModule.CurFunction()->body);
  // }
  if ((mirModule.srcLang == kSrcLangJava) && (mirModule.CurFunction()->info.size() > 0)) {
    std::string string("INFO_registers");
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(string);
    regNum = mirModule.CurFunction()->GetInfo(strIdx);
    std::string trynum("INFO_tries_size");
    strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(trynum);
    uint32 num = mirModule.CurFunction()->GetInfo(strIdx);
    hasEH = (num != 0);
  }
}

void MeFunction::DumpFunction() {
  for (BB *bb : bbVec) {
    if (bb == nullptr) {
      continue;
    }
    bb->DumpHeader(&mirModule);

    MapleMap<OriginalSt *, PhiNode>::iterator phiIt;
    for (phiIt = bb->phiList.begin(); phiIt != bb->phiList.end(); phiIt++) {
      (*phiIt).second.Dump(&mirModule);
    }

    for (auto stmt : bb->stmtNodeList) {
      GenericSSAPrint(&mirModule, stmt, 1, &meSSATab->stmtsSSAPart);
    }
  }
}

void MeFunction::DumpFunctionNoSSA() {
  for (BB *bb : bbVec) {
    if (bb == nullptr) {
      continue;
    }
    bb->DumpHeader(&mirModule);

    MapleMap<OriginalSt *, PhiNode>::iterator phiIt;
    for (phiIt = bb->phiList.begin(); phiIt != bb->phiList.end(); phiIt++) {
      (*phiIt).second.Dump(&mirModule);
    }

    for (auto stmt : bb->stmtNodeList) {
      stmt->Dump(&mirModule, 1);
    }
  }
}

void MeFunction::DumpMeFunc() {
  for (BB *bb : bbVec) {
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
  for (BB *bb : bbVec) {
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
    endTryBB2TryBB[curbb] = lastjavatryBb;
  } else {
    newbb->isTry = true;
    bbTryNodeMap[newbb] = javatryStmt;
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
  default: CHECK_FATAL(false, "CreateDummyReturnValue: NYI for non-scalar return type");
  }
  return nullptr;
}

void MeFunction::CreateBasicBlocks() {
  if (mirModule.CurFunction()->IsEmpty()) {
    if (!MeOption::quiet) {
      LogInfo::MapleLogger() << "function is empty, cfg is nullptr\n";
    }
    return;
  }
  /* create common_entry/exit bb first as bbVec[0] and bbVec[1] */
  commonEntryBB = NewBasicBlock();
  commonEntryBB->isEntry = true;
  commonExitBB = NewBasicBlock();
  commonExitBB->isExit = true;

  first_bb_ = NewBasicBlock();
  first_bb_->isEntry = true;
  StmtNode *nextstmt = mirModule.CurFunction()->body->GetFirst();
  ASSERT(nextstmt != nullptr, "function has no statement");
  BB *curbb = first_bb_;
  StmtNode *javatryStmt = nullptr;  // record current javatry stmt for map<bb, javatry_stmt>
  BB *lastjavatryBb = nullptr;      // bb containing javatry_stmt
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
          if (javatryStmt != nullptr) {
            SetTryBlockInfo(javatryStmt, lastjavatryBb, nextstmt, curbb, newbb);
          }
          curbb = newbb;
        }
        break;
      }
      case OP_dassign: {
        if (curbb->IsEmpty()) {
          curbb->SetFirst(stmt);
        }

        if (JAVALANG && static_cast<DassignNode *>(stmt)->GetRhs()->MayThrowException()) {
          stmt->op = OP_maydassign;
          if (javatryStmt != nullptr) {
            // breaks new BB only inside try blocks
            curbb->SetLast(stmt);
            curbb->kind = kBBFallthru;
            BB *newbb = NewBasicBlock();
            SetTryBlockInfo(javatryStmt, lastjavatryBb, nextstmt, curbb, newbb);
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
        if (javatryStmt != nullptr) {
          SetTryBlockInfo(javatryStmt, lastjavatryBb, nextstmt, curbb, newbb);
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
        if (javatryStmt != nullptr) {
          // handle as goto
          if (curbb->IsEmpty()) {
            curbb->SetFirst(stmt);
          }
          curbb->SetLast(stmt);
          curbb->kind = kBBGoto;
          if (nextstmt) {
            BB *newbb = NewBasicBlock();
            SetTryBlockInfo(javatryStmt, lastjavatryBb, nextstmt, curbb, newbb);
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
          if (javatryStmt != nullptr) {
            SetTryBlockInfo(javatryStmt, lastjavatryBb, nextstmt, curbb, newbb);
          }
          curbb = newbb;
          if (stmt->op == OP_gosub) {
            curbb->isEntry = true;
          }
        }
        break;
      }
      case OP_endtry:
        if (mirModule.srcLang == kSrcLangJava) {
          if (javatryStmt == nullptr) {
            break;
          }
          /* skip OP_entry and generate it in emit phase */
          ASSERT(javatryStmt != nullptr, "");
          ASSERT(lastjavatryBb != nullptr, "");
          javatryStmt = nullptr;  // reset intryblocks
          if (!curbb->IsEmpty()) {
            StmtNode *laststmt = stmt->GetPrev();
            CHECK_FATAL((curbb->stmtNodeList.last == nullptr || curbb->stmtNodeList.last == laststmt), "something wrong building BB");
            curbb->SetLast(laststmt);
            if (curbb->kind == kBBUnknown) {
              curbb->kind = kBBFallthru;
            }
            curbb->isTryEnd = true;
            endTryBB2TryBB[curbb] = lastjavatryBb;
            curbb = NewBasicBlock();
          } else if (curbb->bbLabel != 0) {
            // create the empty BB
            curbb->kind = kBBFallthru;
            curbb->isTryEnd = true;
            endTryBB2TryBB[curbb] = lastjavatryBb;
            curbb = NewBasicBlock();
          } else {
          }  // endtry has already been processed in SetTryBlockInfo()
          lastjavatryBb = nullptr;
        } else {
          /* TODO:: js has OP_endtry and donothing now */
          if (curbb->IsEmpty()) {
            curbb->SetFirst(stmt);
          }
          if ((nextstmt == nullptr) && (curbb->stmtNodeList.last == nullptr)) {
            curbb->SetLast(stmt);
          }
        }
        break;
      case OP_cpptry:
      case OP_try:
      case OP_javatry: {
        // start a new bb or with a label
        if (!curbb->IsEmpty()) {
          // prepare a new bb
          StmtNode *laststmt = stmt->GetPrev();
          CHECK_FATAL((curbb->stmtNodeList.last == nullptr || curbb->stmtNodeList.last == laststmt), "something wrong building BB");
          curbb->SetLast(laststmt);
          if (curbb->kind == kBBUnknown) {
            curbb->kind = kBBFallthru;
          }
          BB *newbb = NewBasicBlock();
          // assume no nested javatry, so no need to call SetTryBlockInfo()
          curbb = newbb;
        }
        curbb->SetFirst(stmt);
        javatryStmt = stmt;
        lastjavatryBb = curbb;
        curbb->isTry = true;
        bbTryNodeMap[curbb] = javatryStmt;
        // prepare a new bb that contains only a OP_javatry. It is needed for
        // dse to work correctly: assignments in a try block should not affect
        // assignments before the try block as exceptions might occur.
        curbb->SetLast(stmt);
        curbb->kind = kBBFallthru;
        BB *newbb = NewBasicBlock();
        SetTryBlockInfo(javatryStmt, lastjavatryBb, nextstmt, curbb, newbb);
        curbb = newbb;
        break;
      }
      case OP_cppcatch: {
        // start a new bb or with a label
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
        break;
      }
      case OP_catch:
      case OP_javacatch: {
        // start a new bb or with a label
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
          if (javatryStmt != nullptr) {
            SetTryBlockInfo(javatryStmt, lastjavatryBb, nextstmt, curbb, newbb);
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
            if (mirModule.srcLang == kSrcLangJava && laststmt->op == OP_endtry) {
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
          if (javatryStmt != nullptr) {
            newbb->isTry = true;
            bbTryNodeMap[newbb] = javatryStmt;
            if (curbb->kind == kBBFallthru && false) {
              // let's create a new javatry and update the predessor of fallthru to this bb.
              StmtNode *newjavatrystmt = javatryStmt->CloneTree(&mirModule);
              javatryStmt = newjavatrystmt;
              curbb->isTryEnd = true;
              endTryBB2TryBB[curbb] = lastjavatryBb;
              lastjavatryBb = newbb;
              mirModule.CurFunction()->body->InsertAfter(stmt, newjavatrystmt);
              newbb->SetFirst(newjavatrystmt);
              bbTryNodeMap[newbb] = newjavatrystmt;
            }
          }
          curbb = newbb;
        }
        labelBBIdMap[labidx] = curbb;
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
        if (javatryStmt != nullptr) {
          SetTryBlockInfo(javatryStmt, lastjavatryBb, nextstmt, curbb, newbb);
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
  ASSERT(javatryStmt == nullptr,
          "");  // tryandendtry should be one-one mapping
  ASSERT(lastjavatryBb == nullptr,
          "");  // tryandendtry should be one-one mapping
  last_bb_ = curbb;
  if (last_bb_->IsEmpty() || last_bb_->kind == kBBUnknown) {
    // insert a return statement
    MIRType *retty = mirModule.CurFunction()->GetReturnType();
    BaseNode *retOpnd = CreateDummyReturnValue(mirModule.mirBuilder, retty);
    last_bb_->stmtNodeList.push_back(mirModule.mirBuilder->CreateStmtReturn(retOpnd));
    last_bb_->isExit = true;
    last_bb_->kind = kBBReturn;
  }
  return;
}

void MeFunction::Prepare(unsigned long rangeNum) {
  if (!MeOption::quiet)
    LogInfo::MapleLogger() << "---Preparing Function  < " << mirModule.CurFunction()->GetName() << " > [" << rangeNum << "] ---\n";
  /* lower first */
  MIRLower mirlowerer(mirModule, mirModule.CurFunction());
  if (!isLno) {
    mirlowerer.SetLowerME();
    mirlowerer.SetLowerExpandArray();
    mirlowerer.LowerFunc(mirModule.CurFunction());
  }
  CreateBasicBlocks();
  if (NumBBs() == 0) {
    /* there's no basicblock generated */
    return;
  }
  RemoveEhEdgesInSyncRegion();

  theCFG = memPool->New<MirCFG>(this);
  theCFG->BuildMirCFG();
  theCFG->VerifyLabels();
  theCFG->UnreachCodeAnalysis();
  theCFG->WontExitAnalysis();
  theCFG->Verify();
}

void MeFunction::Verify() {
  theCFG->Verify();
  theCFG->VerifyLabels();
}

BB *MeFunction::NewBasicBlock() {
  BB *newbb = memPool->New<BB>(&alloc, &versAlloc, BBId(nextBBId++));
  bbVec.push_back(newbb);
  return newbb;
}

void MeFunction::DeleteBasicBlock(const BB *bb) {
  ASSERT(bbVec[bb->id.idx] == bb, "");
  /* update first_bb_ and last_bb if needed */
  if (first_bb_ == bb) {
    first_bb_ = NextBB(bb);
  } else if (last_bb_ == bb) {
    last_bb_ = PrevBB(bb);
  }
  bbVec.at(bb->id.idx) = nullptr;
  return;
}

/* get next bb in bbVec*/
BB *MeFunction::NextBB(const BB *bb) {
  if (bb->id.idx == bbVec.size() - 1) {
    return nullptr;
  }
  uint32 i = bb->id.idx + 1;
  for (; i < bbVec.size(); i++) {
    if (bbVec[i] != nullptr) {
      return bbVec[i];
    }
  }
  return nullptr;
}

/* get prev bb in bbVec*/
BB *MeFunction::PrevBB(const BB *bb) {
  if (bb->id.idx == 0) {
    return nullptr;
  }
  int32 i = bb->id.idx - 1;
  for (; i >= 0; i--) {
    if (bbVec[i] != nullptr) {
      return bbVec[i];
    }
  }
  return nullptr;
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
  for (uint32 i = 0; i < commonExitBB->pred.size(); i++) {
    if (commonExitBB->pred[i] == bb) {
      commonExitBB->pred[i] = newbb;
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
  endTryBB2TryBB[newbb] = endTryBB2TryBB[bb];
  bb->isExit = false;
  bb->isTryEnd = false;
  return newbb;
}

/* create label for bb */
void MeFunction::CreateBBLabel(BB *bb) {
  if (bb->bbLabel != 0) {
    return;
  }

  LabelIdx label = mirModule.CurFunction()->labelTab->CreateLabelWithPrefix('m');
  mirModule.CurFunction()->labelTab->AddToStringLabelMap(label);
  bb->bbLabel = label;
  labelBBIdMap.insert(make_pair(label, bb));
}

// Recognize the following kind of simple pattern and remove corresponding EH edges
// @label78044   javacatch { <* void> }
//  dassign %Reg0_R524935 0 (regread ptr %%thrownval)
//  syncexit (dread ref %Reg8_R460958)
//  endtry
//  throw (dread ptr %Reg0_R524935)
void MeFunction::RemoveEhEdgesInSyncRegion() {
  if (endTryBB2TryBB.size() != 1) {
    return;
  }

  for (auto iter : endTryBB2TryBB) {
    BB *tryBB = iter.second;
    BB *endtryBB = iter.first;
    // Filter out complex cases
    if (!(endtryBB->IsCatch() || endtryBB->isJSCatch) || endtryBB->kind != kBBFallthru || !endtryBB->IsEndTry() ||
        !endtryBB->IsJavaFinally() || endtryBB->stmtNodeList.last->op != OP_syncexit || (tryBB->stmtNodeList.last->op != OP_javatry && tryBB->stmtNodeList.last->op != OP_try)) {
      return;
    }
    for (auto it : bbTryNodeMap) {
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
    for (auto it : bbTryNodeMap) {
      BB *bb = it.first;
      if (bb != tryBB && bb != endtryBB) {
        bb->isTry = false;
      }
    }
  }
}

}  // namespace maple
