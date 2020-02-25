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
#include "bb.h"
#include "me_cfg.h"
#include "ssa_mir_nodes.h"
#include "me_irmap.h"
#include "mir_builder.h"
#include <algorithm>

using namespace std;

namespace maple {

void MirCFG::BuildMirCFG() {
  MapleVector<BB *> entryblocks(func->alloc.Adapter());
  MapleVector<BB *> exitblocks(func->alloc.Adapter());

  // bbVec[0,1] are common entry/exit bb
  for (uint32 i = func->commonExitBB->id.idx + 1; i < func->bbVec.size(); i++) {
    BB *bb = func->bbVec[i];
    if (bb == nullptr) {
      continue;
    }
    if (bb->isEntry) {
      entryblocks.push_back(bb);
    }
    if (bb->isExit) {
      exitblocks.push_back(bb);
    }
    switch (bb->kind) {
      case kBBGoto: {
        StmtNode *lastStmt = bb->stmtNodeList.last;
        if (lastStmt->op == OP_throw) {
          break;
        }
        CHECK_FATAL(lastStmt->op == OP_goto, "");
        GotoNode *gotostmt = static_cast<GotoNode *>(lastStmt);
        LabelIdx lblidx = (LabelIdx)gotostmt->offset;
        BB *meBb = func->labelBBIdMap[lblidx];
        bb->succ.push_back(meBb);
        meBb->pred.push_back(bb);
        break;
      }
      case kBBCondGoto: {
        StmtNode *laststmt = bb->stmtNodeList.last;
        CHECK_FATAL(laststmt->IsCondBr(), "");
        /* succ[0] is fallthru, succ[1] is gotobb */
        BB *rightnextbb = func->NextBB(bb);
        CHECK_FATAL(rightnextbb, "null ptr check ");
        rightnextbb->pred.push_back(bb);
        bb->succ.push_back(rightnextbb);
        /* link goto */
        CondGotoNode *gotostmt = static_cast<CondGotoNode *>(laststmt);
        LabelIdx lblidx = (LabelIdx)gotostmt->offset;
        BB *meBb = func->labelBBIdMap[lblidx];
        bb->succ.push_back(meBb);
        meBb->pred.push_back(bb);
        break;
      }
      case kBBSwitch: {
        StmtNode *laststmt = bb->stmtNodeList.last;
        CHECK_FATAL(laststmt->op == OP_switch, "");
        SwitchNode *switchstmt = static_cast<SwitchNode *>(laststmt);
        LabelIdx lblidx = switchstmt->defaultLabel;
        BB *mirbb = func->labelBBIdMap[lblidx];
        bb->succ.push_back(mirbb);
        mirbb->pred.push_back(bb);
        for (uint32_t j = 0; j < switchstmt->switchTable.size(); j++) {
          lblidx = switchstmt->switchTable[j].second;
          BB *mebb = func->labelBBIdMap[lblidx];
          // Avoid duplicate succs.
          MapleVector<BB *>::iterator it = std::find(bb->succ.begin(), bb->succ.end(), mebb);
          if (it == bb->succ.end()) {
            bb->succ.push_back(mebb);
            mebb->pred.push_back(bb);
          }
        }
        break;
      }
      case kBBReturn:
        break;
      default: {
        // fall through?
        BB *rightnextbb = func->NextBB(bb);
        if (rightnextbb) {
          rightnextbb->pred.push_back(bb);
          bb->succ.push_back(rightnextbb);
        }
        break;
      }
    }
    /* deal try blocks, add catch handler to try's succ */
    if (bb->isTry) {
      ASSERT((func->bbTryNodeMap.find(bb) != func->bbTryNodeMap.end()), "try bb without try");
      StmtNode *stmt = func->bbTryNodeMap[bb];
      TryNode *trynode = static_cast<TryNode *>(stmt);
      bool hasfinallyhandler = false;
      /* add exception handler bb */
      for (uint32 i = 0; i < trynode->offsets.size(); i++) {
        LabelIdx labelIdx = trynode->offsets[i];
        ASSERT(func->labelBBIdMap.find(labelIdx) != func->labelBBIdMap.end(), "");
        BB *meBb = func->labelBBIdMap[labelIdx];
        ASSERT(meBb != nullptr && meBb->IsCatch(), "");
        uint32 si = 0;
        if (meBb->IsJavaFinally() || meBb->IsCatch()) {
          hasfinallyhandler = true;
        }
        /* avoid redundant succ */
        for (; si < bb->succ.size(); si++) {
          if (meBb == bb->succ[si]) {
            break;
          }
        }
        if (si == bb->succ.size()) {
          bb->succ.push_back(meBb);
          meBb->pred.push_back(bb);
        }
      }
      /* if try block don't have finally catch handler, add commonExitBB as its succ */
      if (hasfinallyhandler == false) {
        if (!bb->isExit) {
          bb->isExit = true;  // may exit
          exitblocks.push_back(bb);
        }
      } else if ((func->mirModule.srcLang == kSrcLangJava) && bb->isExit) {
        // deal with throw bb, if throw bb in a tryblock and has finallyhandler
        StmtNode *lastStmt = bb->stmtNodeList.last;
        if (lastStmt && lastStmt->op == OP_throw) {
          bb->isExit = false;
          ASSERT(bb == exitblocks.back(), "");
          exitblocks.pop_back();
        }
      }
    }
  }
  // merge all blocks in entryblocks
  for (MapleVector<BB *>::iterator it = entryblocks.begin(); it != entryblocks.end(); ++it) {
    func->commonEntryBB->succ.push_back(*it);
  }
  // merge all blocks in exitblocks
  for (MapleVector<BB *>::iterator it = exitblocks.begin(); it != exitblocks.end(); ++it) {
    func->commonExitBB->pred.push_back(*it);
  }
}

// used only after DSE because it looks at live field of VersionSt
void MirCFG::ConvertPhis2IdentityAssigns(BB *mebb) {
  MapleMap<OriginalSt *, PhiNode>::iterator phiIt = mebb->phiList.begin();
  while (phiIt != mebb->phiList.end()) {
    if (!(*phiIt).second.result->IsLive()) {
      phiIt++;
      continue;
    }
    // replace phi with identify assignment as it only has 1 opnd
    OriginalSt *ost = (*phiIt).first;
    if (ost->ostType == OriginalSt::kSymbolOst && ost->indirectLev == 0) {
      MIRSymbol *st = ost->GetMIRSymbol();
      MIRType *stype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(st->GetTyIdx());
      AddrofNode *dread = func->mirModule.mirBuilder->CreateDread(st, GetRegPrimType(stype->GetPrimType()));
      AddrofSSANode *dread2 = func->mirFunc->codeMemPool->New<AddrofSSANode>(dread);
      dread2->ssaVar = (*phiIt).second.phiOpnds[0];
      DassignNode *dass = func->mirModule.mirBuilder->CreateStmtDassign(st, 0, dread2);
      func->meSSATab->stmtsSSAPart.SetSsapartOf(dass,
                                                 func->meSSATab->stmtsSSAPart.ssaPartMp->New<MayDefPartWithVersionSt>(
                                                   &func->meSSATab->stmtsSSAPart.ssaPartAlloc));
      MayDefPartWithVersionSt *thessapart =
        static_cast<MayDefPartWithVersionSt *>(func->meSSATab->stmtsSSAPart.SsapartOf(dass));
      thessapart->ssaVar = (*phiIt).second.result;
      mebb->PrependStmtNode(dass);
    }
    phiIt++;
  }
  mebb->phiList.clear();  // delete all the phis

  MapleMap<OStIdx, MePhiNode *>::iterator mePhiIt = mebb->mePhiList.begin();
  while (mePhiIt != mebb->mePhiList.end()) {
    if (!mePhiIt->second->isLive) {
      mePhiIt++;
      continue;
    }
    // replace phi with identify assignment as it only has 1 opnd
    OriginalSt *ost = func->meSSATab->GetOriginalStFromid(mePhiIt->first);
    if (ost->ostType == OriginalSt::kSymbolOst && ost->indirectLev == 0) {
      MePhiNode *phi = mePhiIt->second;
      AssignMeStmt *assign = nullptr;
      if (phi->UseReg()) {
        assign = func->irMap->New<AssignMeStmt>(OP_regassign, phi->lhs, phi->opnds[0]);
      } else {
        assign = func->irMap->NewInPool<DassignMeStmt>(phi->lhs, phi->opnds[0]);
      }
      assign->bb = phi->defBB;
      assign->isLive = phi->isLive;
      mebb->PrependMeStmt(assign);
    }
    mePhiIt++;
  }
  mebb->mePhiList.clear();  // delete all the phis
}

// analyse the CFG to find the BBs that are not reachable from function entries
// and delete them
void MirCFG::UnreachCodeAnalysis(bool updatePhi) {
  std::vector<bool> visitedBBs(func->NumBBs(), false);
  func->commonEntryBB->FindReachableBBs(visitedBBs);

  // delete the unreached bb
  BB *bb = nullptr;
  for (uint32_t i = 0; i < func->bbVec.size(); i++) {
    bb = func->bbVec[i];
    if (!visitedBBs[i] && bb && !bb->isEntry && bb != func->commonExitBB) {
      bb->wontExit = true;
      /* avoid redundant pred before adding to commonExitBB's pred list */
      uint32 pi = 0;
      for (; pi < func->commonExitBB->pred.size(); pi++) {
        if (bb == func->commonExitBB->pred[pi]) {
          break;
        }
      }
      if (pi == func->commonExitBB->pred.size()) {
        func->commonExitBB->pred.push_back(bb);
      }
      if (!MeOption::quiet) {
        LogInfo::MapleLogger() << "#### BB " << bb->id.idx << " deleted because unreachable\n";
      }
      if (bb->isTryEnd) {
        // unreachable bb has try end info
        int j = static_cast<int>(i) - 1;
        for (; j >= 0; j--) {
          if (func->bbVec[j] != nullptr) {
            // move entrytry tag to previous bb with try
            if (func->bbVec[j]->isTry && !func->bbVec[j]->isTryEnd) {
              func->bbVec[j]->isTryEnd = true;
              func->endTryBB2TryBB[func->bbVec[j]] = func->endTryBB2TryBB[bb];
            }
            break;
          }
        }
      }
      func->DeleteBasicBlock(bb);
      // remove the bb from its succ's pred list
      for (MapleVector<BB *>::iterator it = bb->succ.begin(); it != bb->succ.end(); it++) {
        BB *sucbb = *it;
        if (!updatePhi) {
          bb->RemoveBBFromVector(sucbb->pred);
        } else {
          sucbb->RemoveBBFromPred(bb);
          if (sucbb->pred.size() == 1) {
            ConvertPhis2IdentityAssigns(sucbb);
          } else if (sucbb->pred.empty()) {
            sucbb->phiList.clear();
          }
        }
      }
      // remove the bb from commonExitBB's pred list if it is there
      for (MapleVector<BB *>::iterator it = func->commonExitBB->pred.begin();
           it != func->commonExitBB->pred.end(); it++) {
        if (*it == bb) {
          func->commonExitBB->RemoveBBFromPred(bb);
          break;
        }
      }
    }
  }
}

// analyse the CFG to find the BBs that will not reach any function exit; these
// are BBs inside infinite loops; mark their wontExit flag and create
// artificial edges from them to commonExitBB
void MirCFG::WontExitAnalysis() {
  if (func->NumBBs() == 0) {
    return;
  }
  vector<bool> visitedBBs(func->NumBBs(), false);
  func->commonExitBB->FindWillExitBBs(visitedBBs);

  BB *bb = nullptr;
  uint32_t bbvecsize = func->bbVec.size();
  for (uint32_t i = 0; i < bbvecsize; i++) {
    bb = func->bbVec[i];
    if (!visitedBBs[i] && bb && (bb != func->commonEntryBB)) {
      bb->wontExit = true;
      if (!MeOption::quiet) {
        printf("#### BB %d wont exit\n", i);
      }
      if (bb->kind == kBBGoto) {
        // create artificial BB to transition to commonExitBB
        BB *newbb = func->NewBasicBlock();
        newbb->kind = kBBReturn;
        newbb->isExit = true;
        newbb->artificial = true;
        bb->succ.push_back(newbb);
        newbb->pred.push_back(bb);

        func->commonExitBB->pred.push_back(newbb);
      }
    }
  }
}

// CFG Verify
// Check bb-vec and bb-list are strictly consistent.
void MirCFG::Verify() {
  // Check every bb in bb-list.
  for (BB *bb : func->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    ASSERT(bb->id.idx < func->bbVec.size(), "CFG Error!");
    ASSERT(func->bbVec[bb->id.idx] == bb, "CFG Error!");
    if (bb->IsEmpty() || bb == func->commonEntryBB || bb == func->commonExitBB) {
      continue;
    }
    ASSERT(bb->kind != kBBUnknown, "");
    /* verify succ[1] is goto bb */
    StmtNode *lastStmt = bb->stmtNodeList.last;
    if (bb->kind == kBBCondGoto) {
      ASSERT(bb->isTry || bb->wontExit || (lastStmt != nullptr && bb->succ.size() == 2), "");
      CondGotoNode *gotostmt = static_cast<CondGotoNode *>(lastStmt);
      BB *gotobb = bb->succ[1];
      ASSERT(gotobb->bbLabel == gotostmt->offset, "");
    } else if (bb->kind == kBBGoto) {
      if (lastStmt->op == OP_throw) {
        continue;
      }
      ASSERT(bb->isTry || bb->wontExit || (lastStmt != nullptr && bb->succ.size() == 1), "");
      GotoNode *gotostmt = static_cast<GotoNode *>(lastStmt);
      BB *gotobb = bb->succ[0];
      ASSERT(gotobb->bbLabel == gotostmt->offset, "");
    }
  }
}

// check that all the target labels in jump statements are defined
void MirCFG::VerifyLabels(void) {
  for (uint32_t i = 0; i < func->bbVec.size(); i++) {
    BB *mirbb = func->bbVec[i];
    if (mirbb == nullptr) {
      continue;
    }
    if (mirbb->stmtNodeList.last == nullptr) {
      continue;
    }
    if (mirbb->kind == kBBGoto) {
      if (mirbb->stmtNodeList.last->op == OP_throw) {
        continue;
      }
      GotoNode *gotostmt = static_cast<GotoNode *>(mirbb->stmtNodeList.last);
      LabelIdx targetLabidx = (LabelIdx)gotostmt->offset;
      BB *bb = func->labelBBIdMap[targetLabidx];
      CHECK_FATAL(bb->bbLabel == targetLabidx, "undefined label in goto");
    } else if (mirbb->kind == kBBCondGoto) {
      CondGotoNode *cgotostmt = static_cast<CondGotoNode *>(mirbb->stmtNodeList.last);
      LabelIdx targetLabidx = (LabelIdx)cgotostmt->offset;
      BB *bb = func->labelBBIdMap[targetLabidx];
      CHECK_FATAL(bb->bbLabel == targetLabidx, "undefined label in conditional branch");
    } else if (mirbb->kind == kBBSwitch) {
      SwitchNode *switchstmt = static_cast<SwitchNode *>(mirbb->stmtNodeList.last);
      LabelIdx targetLabidx = switchstmt->defaultLabel;
      BB *bb = func->labelBBIdMap[targetLabidx];
      CHECK_FATAL(bb->bbLabel == targetLabidx, "undefined label in switch");
      for (uint32_t j = 0; j < switchstmt->switchTable.size(); j++) {
        targetLabidx = switchstmt->switchTable[j].second;
        bb = func->labelBBIdMap[targetLabidx];
        CHECK_FATAL(bb->bbLabel == targetLabidx, "undefined switch target label");
      }
    }
  }
}

void MirCFG::Dump() {
  // BSF  Dump the cfg
  // map<uint32, uint32> visited_map;
  // set<uint32> visited_set;
  printf("####### CFG Dump: ");
  CHECK_FATAL(func->NumBBs() != 0, "size to be allocated is 0");
  bool *visitedMap = static_cast<bool*>(calloc(func->NumBBs(), sizeof(bool)));
  if (visitedMap != nullptr) {
    queue<BB *> qu;
    qu.push(func->first_bb_);
    while (!qu.empty()) {
      BB *bb = qu.front();
      qu.pop();
      if (bb == nullptr) {
        continue;
      }
      BBId id = bb->id;

      if (visitedMap[id.idx] == true) {
        continue;
      }
      printf("%zu ", id.idx);
      visitedMap[id.idx] = true;
      MapleVector<BB *>::iterator it = bb->succ.begin();
      while (it != bb->succ.end()) {
        BB *kidbb = *it;
        if (!visitedMap[kidbb->id.idx]) {
          qu.push(kidbb);
        }
        it++;
      }
    }
    printf("\n");
    free(visitedMap);
  }
}

/* replace special char in FunctionName for output file */
static void ReplaceFilename(string &filename) {
  for (uint32 i = 0; i < filename.length(); i++) {
    if (filename[i] == ';' || filename[i] == '/' || filename[i] == '|') {
      filename[i] = '_';
    }
  }
}

static bool ContainsConststr(const BaseNode *x) {
  if (x->op == OP_conststr || x->op == OP_conststr16) {
    return true;
  }
  for (uint32 i = 0; i < x->numOpnds; i++)
    if (ContainsConststr(x->Opnd(i))) {
      return true;
    }
  return false;
}

/* generate dot file for cfg */
void MirCFG::DumpToFile(const char *prefix, bool dumpinstrs) {
  if (MeOption::noDot) {
    return;
  }
  ofstream cfgfile;
  streambuf *coutbuf = LogInfo::MapleLogger().rdbuf(); /* keep original LogInfo::MapleLogger() buffer */
  streambuf *buf = cfgfile.rdbuf();
  LogInfo::MapleLogger().rdbuf(buf);
  string filename;
  if (prefix != nullptr) {
    filename.append(prefix);
    filename.append("-");
  }
  // the func name length may exceed OS's file name limit, so truncate after 80 chars
  if (func->GetName().size() <= 80) {
    filename.append(func->GetName());
  } else {
    filename.append(func->GetName().c_str(), 80);
  }
  ReplaceFilename(filename);
  filename.append(".dot");
  cfgfile.open(filename.c_str(), ios::trunc);
  cfgfile << "digraph {\n";
  cfgfile << " # /*" << func->GetName().c_str() << " (red line is exception handler)*/\n";
  /* dump edge */
  for (BB *bb : func->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    if (bb == func->commonExitBB) {
      /* specical case for commonExitBB */
      for (auto it = bb->pred.begin(); it != bb->pred.end(); it++) {
        cfgfile << "BB" << (*it)->id.idx << " -> "
                << "BB" << bb->id.idx << "[style=dotted];\n";
      }
      continue;
    }
    for (auto it = bb->succ.begin(); it != bb->succ.end(); it++) {
      cfgfile << "BB" << bb->id.idx << " -> "
              << "BB" << (*it)->id.idx;
      if (bb == func->commonEntryBB) {
        cfgfile << "[style=dotted];\n";
        continue;
      }
      if ((*it)->IsCatch()) {
        /* succ is exception handler */
        cfgfile << "[color=red];\n";
      } else {
        cfgfile << ";\n";
      }
    }
  }
  /* dump instruction in each BB */
  if (dumpinstrs) {
    for (BB *bb : func->bbVec) {
      if (bb == nullptr || bb->IsEmpty()) {
        continue;
      }
      if (bb->kind == kBBCondGoto) {
        cfgfile << "BB" << bb->id.idx << "[shape=diamond,label= \" BB" << bb->id.idx << ":\n{ ";
      } else {
        cfgfile << "BB" << bb->id.idx << "[shape=box,label= \" BB" << bb->id.idx << ":\n{ ";
      }
      if (bb->bbLabel != 0) {
        cfgfile << "@" << func->mirFunc->GetLabelName(bb->bbLabel) << ":\n";
      }
      MapleMap<OriginalSt *, PhiNode>::iterator phiIt;
      for (phiIt = bb->phiList.begin(); phiIt != bb->phiList.end(); phiIt++) {
        (*phiIt).second.Dump(&(func->mirModule));
      }
      StmtNode *stmt = bb->stmtNodeList.first;
      do {
        // avoid printing content that may contain " as this needs to be quoted
        if (stmt->op != OP_comment && !ContainsConststr(stmt)) {
          stmt->Dump(&(func->mirModule), 1);
        }
        if (stmt == bb->stmtNodeList.last) {
          break;
        } else {
          stmt = stmt->GetNext();
        }
      } while (true);
      cfgfile << "}\"];\n";
    }
  }
  cfgfile << "}\n";
  cfgfile.flush();
  cfgfile.close();
  LogInfo::MapleLogger().rdbuf(coutbuf);
}

}  // namespace maple
