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

#include "me_dse.h"
#include <iostream>
#include "ssa_mir_nodes.h"
#include "ver_symbol.h"
#include "me_ssa.h"
#include "me_cfg.h"
#include "me_fsaa.h"

// This phase do dead store elimination. This optimization is done on SSA
// version basis.
// This optimization consider all stmt are not needed at first. The whole
// algorithm is
// as follow:
// 1. mark all stmt are not needed. init an empty worklist to put live node.
// 2. mark some special stmts which has side effect as needed, such as
//    return/eh/call and some assigned stmts who have volatile fileds and so on.
//    Put all operands and mayUse nodes of the needed stmt into worklist.
// 3. For the nodes in worklist mark the def stmt as needed just as step 2 and
//    pop the node from the worklist.
// 4. Repeat step 3 untile the worklist is empty.

namespace maple {
// step 2 : mark the special stmts.
void MeDSE::DseProcess() {
  for (int32 i = cfg->bbVec.size() - 1; i >= 0; i--) {
    BB *bb = cfg->bbVec[i];
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->stmtNodeList) {
      Opcode op = stmt->op;
      switch (op) {
        case OP_jstry:
        case OP_throw:
        case OP_try:
        case OP_javatry:
        case OP_cpptry:
        case OP_catch:
        case OP_javacatch:
        case OP_cppcatch:
        case OP_jscatch:
        case OP_finally:
        case OP_endtry:
        case OP_cleanuptry:
        case OP_gosub:
        case OP_retsub:
        case OP_return:
        case OP_call:
        case OP_virtualcall:
        case OP_virtualicall:
        case OP_superclasscall:
        case OP_interfacecall:
        case OP_interfaceicall:
        case OP_customcall:
        case OP_polymorphiccall:
        case OP_callassigned:
        case OP_virtualcallassigned:
        case OP_virtualicallassigned:
        case OP_superclasscallassigned:
        case OP_interfacecallassigned:
        case OP_interfaceicallassigned:
        case OP_customcallassigned:
        case OP_polymorphiccallassigned:
        case OP_icall:
        case OP_icallassigned:
        case OP_intrinsiccall:
        case OP_xintrinsiccall:
        case OP_intrinsiccallassigned:
        case OP_xintrinsiccallassigned:
        case OP_intrinsiccallwithtype:
        case OP_intrinsiccallwithtypeassigned:
        case OP_syncenter:
        case OP_syncexit:
        case OP_membaracquire:
        case OP_membarrelease:
        case OP_membarstoreload:
        case OP_membarstorestore:
        case OP_assertnonnull:
        case OP_eval:
        case OP_free: {
          MarkStmt(stmt, bb);
          break;
        }
        case OP_goto: {
          if (bb->wontExit || bb->kind == kBBGoto) {
            MarkStmt(stmt, bb);
          }
          break;
        }
        case OP_brtrue:
        case OP_brfalse:
        case OP_igoto:
        case OP_switch: {
          // control flow in an infinite loop cannot be changed
          if (bb->wontExit) {
            MarkStmt(stmt, bb);
          }
          break;
        }
        case OP_dassign: {
          DassignNode *dass = static_cast<DassignNode *>(stmt);
          // check lhs
          MIRSymbol *sym = mirModule->CurFunction()->GetLocalOrGlobalSymbol(dass->stIdx);
          if (sym->IsVolatile()) {
            MarkStmt(stmt, bb);
            break;
          }
          MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
          bool typevolatile = false;
          if (dass->fieldID == 0) {
            typevolatile = ty->HasVolatileField();
          } else {
            typevolatile = static_cast<MIRStructType *>(ty)->IsFieldVolatile(dass->fieldID);
          }
          if (typevolatile || ExprNonDeletable(dass->GetRhs())) {
            MarkStmt(stmt, bb);
          }
          break;
        }
        case OP_regassign: {
          RegassignNode *rass = static_cast<RegassignNode *>(stmt);
          if (ExprNonDeletable(rass->Opnd(0))) {
            MarkStmt(stmt, bb);
          }
          break;
        }
        case OP_maydassign: {
          MarkStmt(stmt, bb);
          break;
        }
        case OP_iassign: {
          IassignNode *iass = static_cast<IassignNode *>(stmt);
          // check lhs
          MIRPtrType *ty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iass->tyIdx));
          MIRType *pointedclassty = ty->GetPointedType();
          const std::string &tname = GlobalTables::GetStrTable().GetStringFromStrIdx(pointedclassty->nameStrIdx);
          TyidxFieldAttrPair fldpair = ty->GetPointedTyidxFldAttrPairWithFieldId(iass->fieldID);
          MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fldpair.first);
          bool typevolatile = ty->PointeeVolatile();
          bool hasfinalattr = false;
          if (!typevolatile) {
            if (iass->fieldID == 0) {
              typevolatile = pointedty->HasVolatileField();
            } else {
              typevolatile = fldpair.second.GetAttr(FLDATTR_volatile);
            }
          }
          if (mirModule->IsCModule() && !typevolatile) {
            // go thru maydefs to check volatile symbols
            SSATab *ssaTab = func->meSSATab;
            MayDefPart *thessapart = static_cast<MayDefPart *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
            for (std::pair<OStIdx, MayDefNode> maydefit : thessapart->mayDefNodes) {
              OriginalSt *ost = ssaTab->GetOriginalStFromid(maydefit.first);
              if (ost->IsVolatile()) {
                typevolatile = true;
                break;
              }
            }
          }
          hasfinalattr = fldpair.second.GetAttr(FLDATTR_final);
          if (typevolatile || hasfinalattr || ExprNonDeletable(iass->addrExpr) ||
              ExprNonDeletable(iass->rhs)) {
            MarkStmt(stmt, bb);
          }
          break;
        }
        case OP_comment: {
          break;
        }
        default: {
          ASSERT(false, "DSE: unrecognized statement opcode");
          break;
        }
      }
    }
  }
}

void MeDSE::UpdateStmt(BB *bb) {
  StmtNode *first = nullptr;
  StmtNode *tmp = nullptr;
  for (auto stmt : bb->stmtNodeList) {
    if (!StmtRequired(stmt)) {
      if (DEBUGFUNC(func)) {
        LogInfo::MapleLogger() << "**** DSE1 deleting: ";
        stmt->Dump(mirModule);
      }
      if (stmt->IsCondBr() || stmt->op == OP_goto) {
        // update BB pred/succ
        bb->kind = kBBFallthru;
        cfg_updated = true;  // tag cfg is changed
        LabelIdx lidx =
          (stmt->op == OP_goto) ? static_cast<GotoNode *>(stmt)->offset : static_cast<CondGotoNode *>(stmt)->offset;
        for (uint32 i = 0; i < bb->succ.size(); i++) {
          if (bb->succ[i]->bbLabel == lidx) {
            BB *succbb = bb->succ[i];
            bb->RemoveBBFromSucc(succbb);
            succbb->RemoveBBFromPred(bb);
            if (succbb->pred.size() == 1) {
              cfg->ConvertPhis2IdentityAssigns(succbb);
            }
            break;
          }
        }
      }
    } else {
      Opcode op = stmt->op;
      if (op == OP_intrinsiccallwithtypeassigned) {
        IntrinsiccallNode *iwtanode = static_cast<IntrinsiccallNode *>(stmt);
        MIRIntrinsicID intrinsic = iwtanode->intrinsic;
        if (intrinsic == INTRN_JAVA_FILL_NEW_ARRAY) {
          bool isiwtareqired = false;
          MapleVector<MustDefNode> *mustdefs = SSAGenericGetMustDefNode(stmt, &func->meSSATab->stmtsSSAPart);
          MapleVector<MustDefNode>::iterator it = mustdefs->begin();
          for (; it != mustdefs->end(); it++) {
            if ((*it).IsRequired()) {
              isiwtareqired = true;
            };
          }
          if (!isiwtareqired) {
            if (DEBUGFUNC(func)) {
              LogInfo::MapleLogger() << "**** DSE1 deleting intrinsiccallwithtypeassign: ";
              stmt->Dump(mirModule);
            }
            continue;
          }
        }
      }
      if (first == nullptr) {
        first = stmt;
        tmp = stmt;
      } else {
        CHECK_FATAL(tmp != nullptr, "tmp is null in  MeDSE::UpdateStmt");
        tmp->SetNext(stmt);
        stmt->SetPrev(tmp);
        tmp = stmt;
      }

      if (kOpcodeInfo.IsCallAssigned(op)) {
        MapleVector<MustDefNode> *mustdefs = SSAGenericGetMustDefNode(stmt, &func->meSSATab->stmtsSSAPart);
        // cannot delete return value assignment if any return value is larger then register size
        bool hasLargeReturn = false;
        for (MustDefNode mustdef : *mustdefs) {
          OriginalSt *ost = mustdef.result->ost;
          if (!ost->IsSymbol()) {
            continue;
          }
          if (ost->symOrPreg.mirSt->GetType()->GetSize() > 16) {
            hasLargeReturn = true;
            break;
          }
        }
        if (!hasLargeReturn) {
          MapleVector<MustDefNode>::iterator it = mustdefs->begin();
          for (; it != mustdefs->end(); it++) {
            if ((*it).IsRequired()) {
              continue;
            }

            if (DEBUGFUNC(func)) {
              LogInfo::MapleLogger() << "**** DSE1 deleting return value assignment in: ";
              stmt->Dump(mirModule);
            }
            CallReturnVector *returnValues = stmt->GetCallReturnVector();
            CHECK_FATAL(returnValues, "null ptr check  ");
            returnValues->clear();
            mustdefs->clear();
            if (mirModule->srcLang == kSrcLangJs) {
              break;  // no need update opcode for JS
            }
            break;
          }
        }
      }
    }
  }
  bb->SetFirst(first);
  bb->SetLast(tmp);
}

void MeDSE::Update() {
  for (BB *bb : cfg->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    UpdateStmt(bb);
  }
}

void MeDSE::DseInit() {
  // Init bb's required flag
  bb_required.resize(cfg->bbVec.size());
  for (uint32 i = 0; i < bb_required.size(); i++) {
    bb_required[i] = false;
  }
  if (cfg->commonEntryBB != cfg->first_bb) {
    bb_required[cfg->commonEntryBB->id.idx] = true;
  }
  if (cfg->commonExitBB != cfg->last_bb) {
    bb_required[cfg->commonExitBB->id.idx] = true;
  }

  // Init the versionst table;
  VersionStTable *vtable = &func->meSSATab->versionStTable;
  for (uint32_t i = 1; i < vtable->Size(); i++) {
    VersionSt *vst = vtable->versionStVector[i];
    vst->MarkDead();
  }
}

void MeDSE::VerifyPhi() {
  for (auto bb : cfg->bbVec) {
    if (bb == nullptr || bb == cfg->commonExitBB || bb->phiList->empty()) {
      continue;
    }
    uint32 predbbNums = bb->pred.size();
    for (auto phiitem : *bb->phiList) {
      if (phiitem.second.result->IsLive()) {
        if (predbbNums <= 1) {
          OriginalSt *ost = phiitem.first;
          ASSERT(ost->ostType != OriginalSt::kSymbolOst || ost->indirectLev != 0,
                  "phi is live and non-virtual in bb with zero or one pred");
        } else if (phiitem.second.phiOpnds.size() != predbbNums) {
          ASSERT(0, "TODO: phi opnd num is not consistent with pred bb num(need update phi)");
        }
      }
    }
  }
}

void MeDSE::Dse() {
  if (DEBUGFUNC(func)) {
    func->DumpFunction();
  }
  DseInit();
  DseProcess();
  while (!worklist.empty()) {
    VersionSt *vst = worklist.front();
    worklist.pop_front();
    MarkVst(vst);
  }
  Update();
  /* remove unreached BB */
  cfg->UnreachCodeAnalysis(/* update_phi */ true);
  VerifyPhi();
  if (DEBUGFUNC(func)) {
    func->DumpFunction();
  }
}

AnalysisResult *MeDoDSE::Run(MeFunction *func, MeFuncResultMgr *m) {
  MeSSA *ssa = static_cast<MeSSA *>(m->GetAnalysisResult(MeFuncPhase_SSA, func, !MeOption::quiet));
  ASSERT(ssa != nullptr, "ssa phase has problem");
  Dominance *pdom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func, !MeOption::quiet));
  ASSERT(pdom != nullptr, "dominance phase has problem");

  SSATab *ssaTab = static_cast<SSATab *>(m->GetAnalysisResult(MeFuncPhase_SSATAB, func, !MeOption::quiet));
  ASSERT(ssaTab != nullptr, "ssaTab phase has problem");

  MemPool *dsemp = mempoolctrler.NewMemPool(PhaseName().c_str());

  MeDSE *dse = dsemp->New<MeDSE>(func, pdom, dsemp);
  dse->Dse();
  func->Verify();

  /* cfg change , invalid results in MeFuncResultMgr */
  if (dse->UpdatedCfg()) {
    m->InvalidAnalysisResult(MeFuncPhase_DOMINANCE, func);
  }

  if (func->mirModule.IsCModule()) {
    /* invoke FSAA */
    MeDoFSAA doFSAA(MeFuncPhase_FSAA);
    if (!MeOption::quiet) {
      LogInfo::MapleLogger() << "  == " << PhaseName() << " invokes [ " << doFSAA.PhaseName() << " ] ==\n";
    }
    doFSAA.Run(func, m);
  }

  return dse;
}

}  // namespace maple
