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


#include "me_option.h"
#include "me_hti.h"
#include <iostream>
#include "ver_symbol.h"
#include "dominance.h"
#include "me_irmap.h"
#include "intrinsics.h"

/*
   This file implement Javascript Type Inferencing (TI) optimization based on SSA
   representation of the program.  The implementation of TI is separated into two
   parts:tThe Algorithm part completely determines the variables to be promoted
   from dynamic type to fixed type, while the IR Update part transforms the IR to
   reflect the result of TI.

   TI only applies to scalar variables.  TI's granularity is at the SSA version,
   and is rooted at analyzing an SSA variable version's definition.  No account
   is taken on how a variable is used, because the JS language standard allows
   implicit type conversion during execution based on how a variable is used or
   operated on.

   TI needs to go through all the variable SSA versions (variable for short) in
   the program.  Initially, the status of each variable is set at Not_Processed.
   After its definition has been looked at once, its status will never return to
   Not_Processed.  Any variable that is already of static type does not need to be
   worked on, and its status is set at NONEED_Infer.  When TI determines a variable
   is kMustNotInfer or kMustInfer, its status will not change again.  A variable
   at kMaybeInfer may change to either kMustNotInfer or kMustInfer as the
   algorithm progresses.  Thus, after a variable has been looked at once, TI will
   continue to work on it as long as its status remains at kMaybeInfer.

   Each variable has its inferred_type initialized to kPtyInvalid. Inferred_type is
   meaningful only if its status is either kMustInfer or kMaybeInfer.  If the
   status is kMustInfer, the processing is done with the variable, so inferred_type
   will no longer change and it indicates its inferred type, which must be a
   static primitive type.  If the status is kMaybeInfer, inferred_type may be
   kPtyInvalid, which means no inferred type has been found.  If status is
   MAarstvYBE_Infer and inferred_type is a valid static primitive type, it indicates
   its may-be-inferred type that is not 100% certain.

   To perform TI efficiently, the Algorithm part is separated into 5 steps:

   1. Because status propagation is from def to uses, we first build the lists of
   uses foe each variable and represent them in stmts_with_use and phis_with_use.
   It also marks the NONEED_Infer variables.

   2. This steps determine the initial set of kMustNotInfer variables as those with
   maydefs or mayuses.  For maydefs, appearance on either lhs or rhs will cause
   kMustNotInfer.   Idenitifying these early serves to reduce the computation
   overhead of the remaining steps of the algorithm.

   Steps 1 and 2 are performed at the same time by a pass through the program code.

   Steps 3 to 5 are worked on at the highest level by looping through the
   variables. When a variable's status is changed, it may cause change in the
   status of other variables.  This propagation of status is driven by a worklist.

   Step 3 extends the initial set of kMustNotInfer variables via worklist-driven
   propagation.

   In the remaining steps, only variables marked kMaybeInfer are to be put in
   the worklist.

   Step 4 examines the remaining variables (those still marked Not_Processed) in
   detail. The examination will result in status changing to kMustNotInfer,
   kMustInfer or kMaybeInfer.  Each status change will have effect on the variables
   whose definitions contain its use.  Thus, each status change will cause
   insertions of the affected variables to the worklist.

   Step 5 performs the main status propagation.  When the worklist becomes empty,
   the propagation finishes.  At this point, all kMustNotInfer variables have
   been identified, and there cannot be additional kMustNotInfer variable.  Thus,
   each kMaybeInfer variable that has a valid inferred type can be regarded as
   kMustInfer, and its status is updated to MUST_infer.  This update may
   cause insertion into the worklist.  Thus, the main status propagation has to
   be repeated. This process iterates until there is no more kMaybeInfer with valid
   inferred type. */

namespace maple {

void MeTI::DumpStatus(void) {
  if (!DEBUGFUNC(func)) {
    return;
  }
  LogInfo::MapleLogger() << "--------- TypeInference tables ----------" << endl;
  for (uint32 i = 0; i < vst_size; i++) {
    // TODO_LB: handle the case where irMap->verst2MeExprTable[i] returns a RegMeExpr
    MeExpr *expr = irMap->verst2MeExprTable[i];
    if (expr || expr->meOp == kMeOpReg) {
      continue;
    }
    VarMeExpr *var = static_cast<VarMeExpr *>(expr);
    if (infer_status[var->vstIdx] == kNoneedInfer) {
      continue;
    }
    if (infer_status[var->vstIdx] == kNotProcessed) {
      continue;
    }
    var->Dump(func->irMap);
    LogInfo::MapleLogger() << "vx" << var->vstIdx;
    InferStatus infstatus = infer_status[var->vstIdx];
    if (infstatus == kMustNotInfer) {
      LogInfo::MapleLogger() << " mustnot ";
    } else if (infstatus == kMustInfer) {
      LogInfo::MapleLogger() << " must ";
    } else if (infstatus == kMaybeInfer) {
      LogInfo::MapleLogger() << " maybe ";
    }
    if (inferred_type[var->vstIdx] != kPtyInvalid) {
      LogInfo::MapleLogger() << GetPrimTypeName(inferred_type[var->vstIdx]);
    }
    LogInfo::MapleLogger() << std::endl;
  }
}

void MeTI::BuildUseFromExpr(MeExpr *expr, MeStmt *mestmt) {
  switch (expr->meOp) {
    case kMeOpVar: {
      VarMeExpr *var = static_cast<VarMeExpr *>(expr);
      if (infer_status[var->vstIdx] != kNoneedInfer) {
        stmts_with_use.at(var->vstIdx)->push_front(mestmt);
      }
      break;
    }
    case kMeOpOp: {
      OpMeExpr *op = static_cast<OpMeExpr *>(expr);
      if (op->GetOpnd(0)) {
        BuildUseFromExpr(op->GetOpnd(0), mestmt);
      }
      if (op->GetOpnd(1)) {
        BuildUseFromExpr(op->GetOpnd(1), mestmt);
      }
      if (op->GetOpnd(2)) {
        BuildUseFromExpr(op->GetOpnd(2), mestmt);
      }
      break;
    }
    default:
      break;
  }
}

void MeTI::AnalyzeChiList(MapleMap<OStIdx, ChiMeNode *> chilist) {
  for (MapleMap<OStIdx, ChiMeNode *>::iterator it = chilist.begin(); it != chilist.end(); it++) {
    ChiMeNode *chinode = it->second;
    VarMeExpr *var = static_cast<VarMeExpr *>(chinode->rhs);
    if (infer_status[var->vstIdx] != kNoneedInfer) {
      infer_status[var->vstIdx] = kMustNotInfer;
    }
  }
}

void MeTI::AnalyzeMuList(MapleMap<OStIdx, ScalarMeExpr *> mulist) {
  for (MapleMap<OStIdx, ScalarMeExpr *>::iterator it = mulist.begin(); it != mulist.end(); it++) {
    ScalarMeExpr *var = it->second;
    if (infer_status[var->vstIdx] != kNoneedInfer) {
      infer_status[var->vstIdx] = kMustNotInfer;
    }
  }
}

void MeTI::FindMuList(MeExpr *expr) {
  switch (expr->meOp) {
    case kMeOpIvar: {
      IvarMeExpr *ivar = static_cast<IvarMeExpr *>(expr);
      ScalarMeExpr *var = ivar->mu;
      if (var != nullptr && var->vstIdx < infer_status.size() && infer_status[var->vstIdx] != kNoneedInfer) {
        infer_status[var->vstIdx] = kMustNotInfer;
      }
      break;
    }
    case kMeOpOp: {
      OpMeExpr *op = static_cast<OpMeExpr *>(expr);
      if (op->GetOpnd(0)) {
        FindMuList(op->GetOpnd(0));
      }
      if (op->GetOpnd(1)) {
        FindMuList(op->GetOpnd(1));
      }
      if (op->GetOpnd(2)) {
        FindMuList(op->GetOpnd(2));
      }
      break;
    }
    default:
      break;
  }
}

// only uses appearing in dassign/intrinsiccallassigned stmts can have type
// inference effects; in addition, locate all may-uses and must_defs and mark
// kMustNotInfer
void MeTI::BuildUseAndFindMustNotInfersFromStmt(MeStmt *stmt) {
  switch (stmt->op) {
    case OP_dassign: {
      DassignMeStmt *mestmt = static_cast<DassignMeStmt *>(stmt);
      BuildUseFromExpr(mestmt->rhs, stmt);
      FindMuList(mestmt->rhs);
      AnalyzeChiList(mestmt->chiList);
      break;
    }
    case OP_regassign: {
      AssignMeStmt *mestmt = static_cast<AssignMeStmt *>(stmt);
      BuildUseFromExpr(mestmt->rhs, stmt);
      FindMuList(mestmt->rhs);
      break;
    }
    case OP_intrinsiccallassigned: {
      IntrinsiccallMeStmt *mestmt = static_cast<IntrinsiccallMeStmt *>(stmt);
      for (uint32_t i = 0; i < mestmt->opnds.size(); i++) {
        BuildUseFromExpr(mestmt->opnds[i], stmt);
        FindMuList(mestmt->opnds[i]);
      }
      AnalyzeMuList(mestmt->muList);
      AnalyzeChiList(mestmt->chiList);
      break;
    }
    case OP_iassign: {
      IassignMeStmt *mestmt = static_cast<IassignMeStmt *>(stmt);
      FindMuList(mestmt->rhs);
      AnalyzeChiList(mestmt->chiList);
      break;
    }
    case OP_brfalse:
    case OP_brtrue: {
      CondGotoMeStmt *mestmt = static_cast<CondGotoMeStmt *>(stmt);
      FindMuList(mestmt->opnd);
      break;
    }
    case OP_return: {
      RetMeStmt *mestmt = static_cast<RetMeStmt *>(stmt);
      for (uint32 i = 0; i < mestmt->opnds.size(); i++) {
        FindMuList(mestmt->opnds[i]);
      }
      AnalyzeMuList(mestmt->muList);
      break;
    }
    case OP_assertnonnull:
    case OP_eval:
    case OP_free: {
      UnaryMeStmt *mestmt = static_cast<UnaryMeStmt *>(stmt);
      FindMuList(mestmt->opnd);
      break;
    }
    case OP_throw: {
      ThrowMeStmt *mestmt = static_cast<ThrowMeStmt *>(stmt);
      FindMuList(mestmt->opnd);
      AnalyzeMuList(mestmt->muList);
      break;
    }
    case OP_syncenter:
    case OP_syncexit: {
      SyncMeStmt *mestmt = static_cast<SyncMeStmt *>(stmt);
      for (int32 i = 0; i < mestmt->NumMeStmtOpnds(); i++) {
        FindMuList(mestmt->opnds[i]);
      }
      AnalyzeMuList(mestmt->muList);
      AnalyzeChiList(mestmt->chiList);
      break;
    }
    case OP_gosub: {
      GosubMeStmt *mestmt = static_cast<GosubMeStmt *>(stmt);
      AnalyzeMuList(mestmt->muList);
      break;
    }
    case OP_retsub: {
      WithMuMeStmt *mestmt = static_cast<WithMuMeStmt *>(stmt);
      AnalyzeMuList(mestmt->muList);
      break;
    }
    case OP_switch: {
      SwitchMeStmt *mestmt = static_cast<SwitchMeStmt *>(stmt);
      FindMuList(mestmt->opnd);
      break;
    }
    case OP_intrinsiccallwithtype:
    case OP_intrinsiccall: {
      IntrinsiccallMeStmt *mestmt = static_cast<IntrinsiccallMeStmt *>(stmt);
      for (uint32 i = 0; i < mestmt->opnds.size(); i++) {
        FindMuList(mestmt->opnds[i]);
      }
      AnalyzeMuList(mestmt->muList);
      AnalyzeChiList(mestmt->chiList);
      break;
    }
    case OP_intrinsiccallwithtypeassigned: {
      IntrinsiccallMeStmt *mestmt = static_cast<IntrinsiccallMeStmt *>(stmt);
      for (uint32 i = 0; i < mestmt->opnds.size(); i++) {
        FindMuList(mestmt->opnds[i]);
      }
      AnalyzeMuList(mestmt->muList);
      AnalyzeChiList(mestmt->chiList);
      break;
    }
    case OP_call:
    case OP_icall:
    case OP_virtualcall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_customcall:
    case OP_polymorphiccall: {
      CallMeStmt *mestmt = static_cast<CallMeStmt *>(stmt);
      for (uint32 i = 0; i < mestmt->opnds.size(); i++) {
        FindMuList(mestmt->opnds[i]);
      }
      AnalyzeMuList(mestmt->muList);
      AnalyzeChiList(mestmt->chiList);
      break;
    }
    case OP_callassigned:
    case OP_icallassigned:
    case OP_virtualcallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned: {
      CallMeStmt *mestmt = static_cast<CallMeStmt *>(stmt);
      for (uint32 i = 0; i < mestmt->opnds.size(); i++) {
        FindMuList(mestmt->opnds[i]);
      }
      AnalyzeMuList(mestmt->muList);
      AnalyzeChiList(mestmt->chiList);
      // lhs of mustdefs are made kMustNotInfer
      MapleVector<MustDefMeNode>::iterator it = mestmt->mustDefList.begin();
      for (; it != mestmt->mustDefList.end(); it++) {
        MeExpr *melhs = (*it).lhs;
        CHECK_FATAL(melhs->meOp == kMeOpVar, "expected var");
        VarMeExpr *lhsVar = static_cast<VarMeExpr *>(melhs);
        if (infer_status[lhsVar->vstIdx] != kNoneedInfer) {
          infer_status[lhsVar->vstIdx] = kMustNotInfer;
        }
      }
      break;
    }
    case OP_xintrinsiccallassigned: {
      IntrinsiccallMeStmt *mestmt = static_cast<IntrinsiccallMeStmt *>(stmt);
      for (uint32 i = 0; i < mestmt->opnds.size(); i++) {
        FindMuList(mestmt->opnds[i]);
      }
      AnalyzeMuList(mestmt->muList);
      AnalyzeChiList(mestmt->chiList);
      // lhs of mustdefs are made kMustNotInfer
      MapleVector<MustDefMeNode>::iterator it = mestmt->mustDefList.begin();
      for (; it != mestmt->mustDefList.end(); it++) {
        MeExpr *melhs = (*it).lhs;
        CHECK_FATAL(melhs->meOp == kMeOpVar, "expected var");
        VarMeExpr *lhsVar = static_cast<VarMeExpr *>(melhs);
        if (infer_status[lhsVar->vstIdx] != kNoneedInfer) {
          infer_status[lhsVar->vstIdx] = kMustNotInfer;
        }
      }
      break;
    }
    default:
      break;
  }
}

// this is the only time we traverse the program code
void MeTI::BuildUseListsAndFindMustNotInfers() {
  for (BB *bb : func->bbVec) {
    if (bb == nullptr) {
      continue;
    }

    for (auto mestmt : bb->meStmtList) {
      BuildUseAndFindMustNotInfersFromStmt(mestmt);
    }
    for (std::pair<OStIdx, MePhiNode *> phiEntry : bb->mePhiList) {
      MePhiNode *phi = phiEntry.second;
      if (!phi->isLive) {
        // phi->lhs has been set NONEED_Infer in Init.
        //        VarMeExpr *lhs = static_cast<VarMeExpr *>(phi->lhs);
        //        infer_status[lhs->vstIdx] = NONEED_Infer;
        continue;
      }
      for (uint32_t i = 0; i < phi->opnds.size(); i++) {
        ScalarMeExpr *var = phi->opnds[i];
        if (infer_status[var->vstIdx] != kNoneedInfer) {
          phis_with_use[var->vstIdx]->push_front(phi);
        }
      }
    }
  }
}

// steps 1 and 2: build use lists and determine initial kMustNotInfer
void MeTI::Init() {
  for (uint32_t i = 0; i < vst_size; i++) {
    // TODO_LB: handle the case where irMap->verst2MeExprTable[i] returns a RegMeExpr
    MeExpr *expr = irMap->verst2MeExprTable[i];
    if (expr && expr->meOp == kMeOpReg) {
      continue;
    }
    VarMeExpr *var = static_cast<VarMeExpr *>(expr);
    if (!var) {
      infer_status[i] = kNoneedInfer;
      continue;
    }
    OriginalSt *ost = var->ost;
    CHECK_FATAL(var->primType != kPtyInvalid, "");
    if (!IsPrimitiveDynType(var->primType)) {
      infer_status[i] = kNoneedInfer;
      continue;
    }
    if (var->defBy == kDefByStmt) {
      if (!var->def.defStmt->isLive) {
        infer_status[i] = kNoneedInfer;
        continue;
      }
    } else if (var->defBy == kDefByPhi) {
      if (!var->def.defPhi->isLive) {
        infer_status[i] = kNoneedInfer;
        continue;
      }
    } else if (var->defBy == kDefByMustdef) {
      if (var->def.defMustDef == nullptr || !var->def.defMustDef->isLive) {
        infer_status[i] = kNoneedInfer;
        continue;
      }
    } else {
      // var->defBy == kDefByChi || var->defBy == kDefByNo
      // globals and formals are already covered by defby == kDefByNo
      stmts_with_use[i] = ti_allocator.GetMemPool()->New<MapleForwardList<MeStmt *>>(ti_allocator.Adapter());
      phis_with_use[i] = ti_allocator.GetMemPool()->New<MapleForwardList<MePhiNode *>>(ti_allocator.Adapter());
      infer_status[i] = kMustNotInfer;
      continue;
    }
    stmts_with_use[i] = ti_allocator.GetMemPool()->New<MapleForwardList<MeStmt *>>(ti_allocator.Adapter());
    phis_with_use[i] = ti_allocator.GetMemPool()->New<MapleForwardList<MePhiNode *>>(ti_allocator.Adapter());
    orig_type[i] = var->primType;
  }
  BuildUseListsAndFindMustNotInfers();
}

// step 3: propagate kMustNotInfer to additional VarMeExpr nodes based on
//         def-use edges
void MeTI::PropagateMustNotInfer() {
  VarMeExpr *lhsVar = nullptr;
  worklist.clear();
  // first do a complete pass through all variable versions; put MUSTNOT_Infers
  // into worklist
  for (uint32_t i = 0; i < vst_size; i++)
    if (infer_status[i] == kMustNotInfer) {
      // TODO_LB: handle the case where irMap->verst2MeExprTable[i] returns a RegMeExpr
      MeExpr *expr = irMap->verst2MeExprTable[i];
      if (expr && expr->meOp == kMeOpReg) {
        continue;
      }
      worklist.insert(static_cast<VarMeExpr *>(expr));
    }

  // do the recursive propagation based on the worklist which is growing at the
  // same time
  while (worklist.size() > 0) {
    MapleSet<VarMeExpr *>::iterator wit = worklist.begin();
    VarMeExpr *var = *wit;
    // propagate kMustNotInfer via uses in statements
    MapleForwardList<MeStmt *> *stmtlist = stmts_with_use[var->vstIdx];
    MapleForwardList<MeStmt *>::iterator sit = stmtlist->begin();
    for (; sit != stmtlist->end(); sit++) {
      MeStmt *stmt = *sit;
      if (stmt->op == OP_dassign) {
        DassignMeStmt *dstmt = static_cast<DassignMeStmt *>(stmt);
        lhsVar = static_cast<VarMeExpr *>(dstmt->lhs);
        if (infer_status[lhsVar->vstIdx] == kNoneedInfer || infer_status[lhsVar->vstIdx] == kMustNotInfer) {
          continue;
        }
        if (dstmt->rhs == var) {
          // dassign is a copy and rhs is the kMustNotInfer var
          infer_status[lhsVar->vstIdx] = kMustNotInfer;
          worklist.insert(lhsVar);
        }
      } else if (stmt->op == OP_intrinsiccallassigned) {
#if (MIR_JAVA == 0)
        IntrinsiccallMeStmt *inass = static_cast<IntrinsiccallMeStmt *>(stmt);
        MIRIntrinsicID id = inass->intrinsic;
        if (id == INTRN_JSOP_ADD) {
          MeExpr *melhs = inass->mustDefList[0].lhs;
          CHECK_FATAL(melhs->meOp == kMeOpVar, "nyi");
          VarMeExpr *lhsVar = static_cast<VarMeExpr *>(melhs);
          infer_status[lhsVar->vstIdx] = kMustNotInfer;
          worklist.insert(lhsVar);
        }
#endif
      }
    }
    // propagate kMustNotInfer via uses in phis
    MapleForwardList<MePhiNode *> *philist = phis_with_use[var->vstIdx];
    MapleForwardList<MePhiNode *>::iterator pit = philist->begin();
    for (; pit != philist->end(); pit++) {
      MePhiNode *phi = *pit;
      if (!phi->isLive) {
        continue;
      }
      lhsVar = static_cast<VarMeExpr *>(phi->lhs);
      if (infer_status[lhsVar->vstIdx] == kNoneedInfer || infer_status[lhsVar->vstIdx] == kMustNotInfer) {
        continue;
      }
      // phi's lhs is made kMustNotInfer since one phi operand is kMustNotInfer
      infer_status[lhsVar->vstIdx] = kMustNotInfer;
      worklist.insert(lhsVar);
    }

    worklist.erase(wit);
  }
}

// the new status can be kMustNotInfer, kMustInfer or kMaybeInfer
void MeTI::UpdateInferStatus(VarMeExpr *var, PrimType pty, InferStatus status) {
  InferStatus previousStatus = infer_status[var->vstIdx];
  CHECK_FATAL(previousStatus != kMustInfer, "");
  CHECK_FATAL(previousStatus != kMustNotInfer, "");
  CHECK_FATAL(previousStatus != kNoneedInfer, "");

  if (pty == PTY_dyni32) {
    pty = PTY_i32;
  }

  if (inferred_type[var->vstIdx] == pty && previousStatus == status) {
    return;
  }

  // Record Inferred type
  inferred_type[var->vstIdx] = pty;
  if (status == kMustInfer) {
    infer_status[var->vstIdx] = kMustInfer;
  } else if (previousStatus == kMaybeInfer && inferred_type[var->vstIdx] != pty)
  // 2nd time inferred type different from first time, mark it kMustNotInfer
  {
    infer_status[var->vstIdx] = kMustNotInfer;
  } else {
    infer_status[var->vstIdx] = status;
  }
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "~~~~";
    var->Dump(func->irMap);
    LogInfo::MapleLogger() << "vx" << var->vstIdx << " status updated to";
    if (infer_status[var->vstIdx] == kMustNotInfer) {
      LogInfo::MapleLogger() << " mustnot\n";
    } else if (infer_status[var->vstIdx] == kMustInfer) {
      LogInfo::MapleLogger() << " must\n";
    } else if (infer_status[var->vstIdx] == kMaybeInfer) {
      LogInfo::MapleLogger() << " maybe\n";
    }
  }

  // add stmts in its uses to worklist
  MapleForwardList<MeStmt *> *stmtlist = stmts_with_use.at(var->vstIdx);
  MapleForwardList<MeStmt *>::iterator sit = stmtlist->begin();
  for (; sit != stmtlist->end(); sit++) {
    MeStmt *stmt = *sit;
    if (stmt->op == OP_dassign) {
      DassignMeStmt *dstmt = static_cast<DassignMeStmt *>(stmt);
      VarMeExpr *lhsVar = static_cast<VarMeExpr *>(dstmt->lhs);
      if (infer_status[lhsVar->vstIdx] == kMaybeInfer) {
        worklist.insert(lhsVar);
        if (DEBUGFUNC(func)) {
          LogInfo::MapleLogger() << "||||";
          lhsVar->Dump(func->irMap);
          LogInfo::MapleLogger() << "vx" << lhsVar->vstIdx << " added to worklist\n";
        }
      }
    } else if (stmt->op == OP_intrinsiccallassigned) {
      // lhs of mustdefs
      CallMeStmt *mestmt = static_cast<CallMeStmt *>(stmt);
      MapleVector<MustDefMeNode>::iterator it = mestmt->mustDefList.begin();
      for (; it != mestmt->mustDefList.end(); it++) {
        MeExpr *melhs = (*it).lhs;
        CHECK_FATAL(melhs->meOp == kMeOpVar, "expected var");
        VarMeExpr *lhsVar = static_cast<VarMeExpr *>(melhs);
        if (infer_status[lhsVar->vstIdx] == kMaybeInfer) {
          worklist.insert(lhsVar);
          if (DEBUGFUNC(func)) {
            LogInfo::MapleLogger() << "||||";
            lhsVar->Dump(func->irMap);
            LogInfo::MapleLogger() << "vx" << lhsVar->vstIdx << " added to worklist\n";
          }
        }
      }
    }
  }
  // add phis in its uses to worklist
  MapleForwardList<MePhiNode *> *philist = phis_with_use[var->vstIdx];
  MapleForwardList<MePhiNode *>::iterator pit = philist->begin();
  for (; pit != philist->end(); pit++) {
    MePhiNode *phi = *pit;
    VarMeExpr *lhsVar = static_cast<VarMeExpr *>(phi->lhs);
    if (infer_status[lhsVar->vstIdx] == kMaybeInfer) {
      worklist.insert(lhsVar);
      if (DEBUGFUNC(func)) {
        LogInfo::MapleLogger() << "||||";
        lhsVar->Dump(func->irMap);
        LogInfo::MapleLogger() << "vx" << lhsVar->vstIdx << " added to worklist\n";
      }
    }
  }
}

// Examine the phi operands to determine lhs's infer_status
// (1) If any operand is kMustNotInfer, lhs is kMustNotInfer
// (2) If all operands are kMustInfer:
//     (a) if the inferred types are the same, lhs is kMustInfer
//     (b) if the inferred types are not the same, lhs is kMustNotInfer
// (3) For each operand that is kMustInfer or [kMaybeInfer with inferred_type],
//     (a) if the new inferred type conflicts with its existing inferred_type,
//         make lhs kMustNotInfer
//     (b) otherwise, make lhs kMaybeInfer and update lhs's inferred_type;
// If status changes in this examination, insert the lhs of its uses into
// worklist
void MeTI::DetermineMustOrMaybeInferForPhi(MePhiNode *phi) {
  VarMeExpr *lhs = static_cast<VarMeExpr*>(phi->lhs);
  uint32_t i = 0;
  // check condition (1)
  for (; i < phi->opnds.size(); i++) {
    VarMeExpr *opnd = static_cast<VarMeExpr *>(phi->opnds[i]);
    if (infer_status[opnd->vstIdx] == kMustNotInfer) {
      UpdateInferStatus(lhs, kPtyInvalid, kMustNotInfer);
      return;
    }
  }
  // check condition (2)
  VarMeExpr *opnd0 = static_cast<VarMeExpr *>(phi->opnds[0]);
  if (infer_status[opnd0->vstIdx] == kMustInfer) {
    PrimType pty0 = inferred_type.at(opnd0->vstIdx);
    for (i = 1; i < phi->opnds.size(); i++) {
      VarMeExpr *opnd = static_cast<VarMeExpr *>(phi->opnds[i]);
      if (infer_status[opnd->vstIdx] != kMustInfer) {
        break;
      }
      if (inferred_type[opnd->vstIdx] != pty0) {
        // condition (2)(b)
        UpdateInferStatus(lhs, kPtyInvalid, kMustNotInfer);
        return;
      }
      if ((i + 1) == phi->opnds.size()) {
        // condition (2)(a)
        UpdateInferStatus(lhs, pty0, kMustInfer);
        return;
      }
    }
  }
  // check condition (3)
  for (i = 0; i < phi->opnds.size(); i++) {
    VarMeExpr *opnd = static_cast<VarMeExpr *>(phi->opnds[i]);
    if (inferred_type[opnd->vstIdx] != kPtyInvalid) {
      if (inferred_type[lhs->vstIdx] == kPtyInvalid) {
        // condition (3)(b)
        UpdateInferStatus(lhs, inferred_type[opnd->vstIdx], kMaybeInfer);
      } else if (inferred_type[opnd->vstIdx] != inferred_type[lhs->vstIdx]) {
        // condition (3)(a)
        UpdateInferStatus(lhs, kPtyInvalid, kMustNotInfer);
        return;
      }
    }
  }
  infer_status[lhs->vstIdx] = kMaybeInfer;
}

void MeTI::AnalyzeExpr(MeExpr *expr, PrimType *inftype, InferStatus *infstatus) {
  if (IsPrimitiveValid(GetNonDynType(expr->primType))) {
    *inftype = GetNonDynType(expr->primType);
    *infstatus = kMustInfer;
    return;
  }

  switch (expr->meOp) {
    case kMeOpConst:
      if (IsPrimitiveValid(expr->primType)) {
        *inftype = GetNonDynType(expr->primType);
        *infstatus = kMustInfer;
        return;
      }
      break;
    case kMeOpVar: {
      VarMeExpr *var = static_cast<VarMeExpr *>(expr);
      if (infer_status[var->vstIdx] == kMustNotInfer) {
        *inftype = kPtyInvalid;
        *infstatus = kMustNotInfer;
        return;
      }
      if (infer_status[var->vstIdx] == kNoneedInfer) {
        *inftype = var->primType;
        *infstatus = kMustInfer;
        return;
      }
      if (infer_status[var->vstIdx] == kMustInfer || infer_status[var->vstIdx] == kMaybeInfer) {
        *inftype = inferred_type.at(var->vstIdx);
        *infstatus = infer_status[var->vstIdx];
        return;
      }
      break;
    }
    case kMeOpOp: {
      OpMeExpr *xexpr = static_cast<OpMeExpr *>(expr);
      switch (expr->op) {
        case OP_abs:
        case OP_bnot:
        case OP_neg:
        case OP_recip:
        case OP_sqrt:
        case OP_alloca:
        case OP_malloc:
        case OP_gcmallocjarray:
        case OP_ashr:
          *inftype = PTY_i32;
          *infstatus = kMustInfer;
          return;
        case OP_cvt:
          if (IsPrimitiveDynType(xexpr->primType) && !IsPrimitiveDynType(xexpr->opndType)) {
            *inftype = xexpr->opndType;
            *infstatus = kMustInfer;
            return;
          }
          break;
        case OP_land:
        case OP_lior:
        case OP_lt:
        case OP_gt:
        case OP_le:
        case OP_ge:
        case OP_cmp:
        case OP_eq:
        case OP_ne:
        case OP_lnot:
          *inftype = PTY_u1;
          *infstatus = kMustInfer;
          return;
        default:
          break;
      }
    }
    default:
      break;
  }
  *inftype = kPtyInvalid;
  *infstatus = kMaybeInfer;
  return;
}

void MeTI::DetermineMustOrMaybeInferForStmt(MeStmt *mestmt) {
  DassignMeStmt *dassign = static_cast<DassignMeStmt *>(mestmt);
  VarMeExpr *lhs = static_cast<VarMeExpr *>(dassign->lhs);
  PrimType inftype;
  InferStatus infstatus;
  AnalyzeExpr(dassign->rhs, &inftype, &infstatus);
  UpdateInferStatus(lhs, inftype, infstatus);
  return;
}

void MeTI::AnalyzeIntrAssigned(MeStmt *mestmt) {
  CHECK_FATAL(mestmt->op == OP_intrinsiccallassigned, "");
  IntrinsiccallMeStmt *inass = static_cast<IntrinsiccallMeStmt *>(mestmt);
  CHECK_FATAL(!inass->mustDefList.empty(), "inass->mustDefList is empty");
  MeExpr *melhs = inass->mustDefList[0].lhs;
  CHECK_FATAL(melhs->meOp == kMeOpVar, "NYI");
  VarMeExpr *ret = static_cast<VarMeExpr *>(melhs);
#if (MIR_JAVA == 0)
  MIRIntrinsicID id = inass->intrinsic;
  switch (id) {
    case INTRN_JSOP_IN:
      UpdateInferStatus(ret, PTY_u1, kMustInfer);
      return;
    case INTRN_JSOP_LENGTH:
      UpdateInferStatus(ret, PTY_u32, kMustInfer);
      return;
    case INTRN_JSOP_NEXT_ITERATOR:
      UpdateInferStatus(ret, kPtyInvalid, kMustNotInfer);
      return;
    case INTRN_JSOP_ADD: {
      PrimType inftype0, inftype1;
      InferStatus infstatus0, infstatus1;
      AnalyzeExpr(inass->opnds[0], &inftype0, &infstatus0);
      AnalyzeExpr(inass->opnds[1], &inftype1, &infstatus1);
      if (inftype0 == PTY_simpleobj || inftype1 == PTY_simpleobj) {
        UpdateInferStatus(ret, kPtyInvalid, kMustNotInfer);
        return;
      }
      if (infstatus0 == kMustNotInfer || infstatus1 == kMustNotInfer) {
        UpdateInferStatus(ret, kPtyInvalid, kMustNotInfer);
        return;
      }
      if ((infstatus0 == kMustInfer && inftype0 == PTY_simplestr) ||
          (infstatus1 == kMustInfer && inftype1 == PTY_simplestr)) {
        UpdateInferStatus(ret, PTY_simplestr, kMustInfer);
        return;
      }
      if (inftype0 != kPtyInvalid && inftype1 != kPtyInvalid && inftype0 != inftype1) {
        UpdateInferStatus(ret, kPtyInvalid, kMustNotInfer);
        return;
      }
      if ((infstatus0 == kMustInfer || infstatus1 == kMustInfer) && (inftype0 == inftype1)) {
        UpdateInferStatus(ret, inftype0, kMustInfer);
        return;
      }
      if (inftype0 != kPtyInvalid && inftype0 == inftype1) {
        UpdateInferStatus(ret, inftype0, kMaybeInfer);
        return;
      }
      break;
    }
    default:
      break;
  }
#endif
  infer_status.at(ret->vstIdx) = kMaybeInfer;
  return;
}

// step 4
void MeTI::DetermineMustOrMaybeInfer() {
  worklist.clear();
  // do a complete pass through all variable versions and work on Not_Processed
  // VarMeExpr nodes that are not in the worklist
  for (uint32_t i = 0; i < vst_size; i++) {
    if (infer_status[i] != kNotProcessed) {
      continue;
    }
    // TODO_LB: handle the case where irMap->verst2MeExprTable[i] returns a RegMeExpr
    MeExpr *expr = irMap->verst2MeExprTable[i];
    if (expr && expr->meOp == kMeOpReg) {
      continue;
    }
    VarMeExpr *var = static_cast<VarMeExpr *>(expr);
    CHECK_FATAL(var != nullptr, "variable is nullptr ");
    if (var->defBy == kDefByPhi) {
      DetermineMustOrMaybeInferForPhi(var->def.defPhi);
    } else if (var->defBy == kDefByStmt) {
      DetermineMustOrMaybeInferForStmt(var->def.defStmt);
    } else {
      CHECK_FATAL(var->defBy == kDefByMustdef, "");
      MustDefMeNode *must = var->def.defMustDef;
      MeStmt *defStmt = must->base;
      if (defStmt->op != OP_intrinsiccallassigned) {
        continue;
      }
      AnalyzeIntrAssigned(defStmt);
    }
  }
}

// step 5
void MeTI::MainPropagation() {
  // variables inserted into the worklist must be kMaybeInfer
  do {
    // do the recursive propagation based on the worklist
    while (worklist.size() > 0) {
      MapleSet<VarMeExpr *>::iterator wit = worklist.begin();
      VarMeExpr *var = *wit;
      if (DEBUGFUNC(func)) {
        LogInfo::MapleLogger() << "^^^^";
        var->Dump(func->irMap);
        LogInfo::MapleLogger() << "vx" << var->vstIdx << " in worklist being worked on\n";
      }
      if (var->defBy == kDefByPhi) {
        DetermineMustOrMaybeInferForPhi(var->def.defPhi);
      } else if (var->defBy == kDefByStmt) {
        DetermineMustOrMaybeInferForStmt(var->def.defStmt);
      } else {
        CHECK_FATAL(var->defBy == kDefByMustdef, "");
        MustDefMeNode *must = var->def.defMustDef;
        MeStmt *defStmt = must->base;
        if (defStmt->op == OP_intrinsiccallassigned) {
          AnalyzeIntrAssigned(defStmt);
        }
      }
      worklist.erase(wit);
    }

    // change all kMaybeInfer with a inferred type to kMustInfer
    for (uint32_t i = 0; i < vst_size; i++) {
      if (infer_status[i] == kMaybeInfer && inferred_type[i] != kPtyInvalid) {
        // TODO_LB: handle the case where irMap->verst2MeExprTable[i] returns a RegMeExpr
        MeExpr *expr = irMap->verst2MeExprTable[i];
        if (expr && expr->meOp == kMeOpReg) {
          continue;
        }

        VarMeExpr *var = static_cast<VarMeExpr *>(expr);
        CHECK_FATAL(var != nullptr, " var is nullptr  ");
        worklist.erase(var);
        UpdateInferStatus(var, inferred_type[i], kMustInfer);
      }
    }
  } while (worklist.size() > 0);
  return;
}

// ================== IR update starts here ====================

MeExpr *MeTI::CreateConst(int64_t val, PrimType pty) {
  MIRType *type = GlobalTables::GetTypeTable().typeTable.at(pty);
  MIRIntConst *constVal = module->memPool->New<MIRIntConst>(val, type);
  return irMap->CreateConstMeExpr(pty, constVal);
}

MeExpr *MeTI::RebuildConst(MeExpr *opnd, PrimType primType) {
  CHECK_FATAL(opnd->meOp == kMeOpConst, "");
  ConstMeExpr *constExpr = static_cast<ConstMeExpr *>(opnd);
  MIRIntConst *intConst = static_cast<MIRIntConst *>(constExpr->constVal);
  int64 value = intConst->value;
  MIRType *type = GlobalTables::GetTypeTable().typeTable.at(primType);
  MIRIntConst *intconst = module->memPool->New<MIRIntConst>(value, type);
  return irMap->CreateConstMeExpr(primType, intconst);
}

MeExpr *MeTI::CreateExprTypeCvt(Opcode o, MeExpr *opnd, PrimType primType) {
  if (opnd->primType == primType || (IsPrimitiveDynType(opnd->primType) && IsPrimitiveDynType(primType))) {
    return opnd;
  }

  if (opnd->meOp == kMeOpConst && IsPrimitiveInteger(primType)) {
    return RebuildConst(opnd, primType);
  }

  TypeCvtNode *un = module->memPool->New<TypeCvtNode>(o);
  un->primType = primType;
  un->fromPrimType = opnd->primType;
  un->uOpnd = reinterpret_cast<BaseNode*>(opnd);
  OpMeExpr opmeexpr(-1, un->op, un->primType, un->numOpnds);
  opmeexpr.SetOpnd(opnd,0);
  opmeexpr.opndType = opnd->primType;
  MeExpr *retmeexpr = irMap->HashMeExpr(&opmeexpr);
  return retmeexpr;
}

MeExpr *MeTI::CreateBinary(Opcode o, MeExpr *opnd0, MeExpr *opnd1, PrimType primType) {
  BinaryNode *bn = module->memPool->New<BinaryNode>(o);
  bn->primType = primType;
  bn->bOpnd[0] = reinterpret_cast<BaseNode*>(opnd0);
  bn->bOpnd[1] = reinterpret_cast<BaseNode*>(opnd1);
  OpMeExpr opmeexpr(-1, bn->op, bn->primType, bn->numOpnds);
  opmeexpr.SetOpnd(opnd0, 0);
  opmeexpr.SetOpnd(opnd1, 1);
  opmeexpr.opndType = opnd0->primType;
  MeExpr *retmeexpr = irMap->HashMeExpr(&opmeexpr);
  return retmeexpr;
}

// Create Intrinsic op expressions for those only have one oprand, e.g. ToString/ToNumber etc.
MeExpr *MeTI::CreateIntrExpr(MIRIntrinsicID intrinsic, MapleVector<MeExpr *> opnds, PrimType primType) {
  NaryMeExpr narymeexpr(&irMap->irMapAlloc, -1, OP_intrinsicop, primType, opnds.size(), TyIdx(0), intrinsic, false);

  for (uint32 i = 0; i < opnds.size(); i++) {
    narymeexpr.PushOpnd(opnds[i]);
  }
  MeExpr *retmeexpr = irMap->HashMeExpr(&narymeexpr);
  return retmeexpr;
}

void MeTI::ReplaceMeStmt(MeStmt *from, MeStmt *to) {
  BB *bb = from->bb;
  bb->InsertMeStmtBefore(from, to);
  bb->RemoveMeStmt(from);
}

MeStmt *MeTI::CreateDassign(MeExpr *lhs, MeExpr *rhs, uint32_t fieldId, BBId bbId) {
  DassignNode *stmt = module->memPool->New<DassignNode>();
  VarMeExpr *var = static_cast<VarMeExpr *>(lhs);
  MIRSymbol *symbol = func->meSSATab->GetMIRSymbolFromid(var->ost->index);
  stmt->stIdx = symbol->GetStIdx();

  stmt->fieldID = fieldId;
  stmt->SetRhs(reinterpret_cast<BaseNode*>(rhs));
  DassignMeStmt *mestmt = module->memPool->New<DassignMeStmt>(&irMap->irMapAlloc, stmt);
  mestmt->rhs = rhs;
  mestmt->UpdateLhs(var);
  mestmt->bb = func->bbVec.at(bbId.idx);
  return mestmt;
}

// Create a new variable from versionst.
MeExpr *MeTI::CreateNewVar(MeExpr *op, PrimType pty) {
  CHECK_FATAL(op->meOp == kMeOpVar, "");
  VarMeExpr *var = static_cast<VarMeExpr *>(op);
  MIRSymbol *sym = func->meSSATab->GetMIRSymbolFromid(var->ost->index);
  std::stringstream ss;
  ss << GetPrimTypeName(pty);
  std::string name(sym->GetName().c_str());
  name.append("|");
  name.append(ss.str());
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(name);
  // new symbols always created locally
  MIRSymbolTable *symtab = func->mirFunc->symTab;
  // FIXME: don't get a symbol from string except Parser
  MIRSymbol *st = symtab->GetSymbolFromStrIdx(strIdx, true);
  if (st == nullptr) {
    st = symtab->CreateSymbol(kScopeLocal);
    st->storageClass = kScAuto;
    st->sKind = kStVar;
    st->SetNameStridx(strIdx);
    MIRType *type = GlobalTables::GetTypeTable().typeTable.at(pty);
    st->SetTyIdx(type->tyIdx);
    symtab->AddToStringSymbolMap(st);
  }
  OriginalSt *ost = func->meSSATab->FindOrCreateSymbolOriginalSt(st, func->mirFunc->puIdx, 0);
  VarMeExpr *medef = irMap->New<VarMeExpr>(irMap->exprID++, ost, 0, pty);
  uint8 typeflag = 0;
  typeflag |= IsPrimitiveScalar(pty) ? TYPEFLAG_SCALAR_MASK : 0;
  typeflag |= IsPrimitiveFloat(pty) ? TYPEFLAG_FLOAT_MASK : 0;
  typeflag |= IsPrimitiveInteger(pty) ? TYPEFLAG_INTEGER_MASK : 0;
  medef->ost->fieldID = var->ost->fieldID;
  medef->defBy = var->defBy;
  medef->def.defStmt = var->def.defStmt;
  irMap->verst2MeExprTable.push_back(medef);
  medef->vstIdx = var->vstIdx;

  return medef;
}

MeExpr *MeTI::GetOrCreateNewVar(VarMeExpr *v, PrimType pty) {
  if (new_symbol[v->vstIdx]) {
    return new_symbol[v->vstIdx];
  }
  MeExpr *newvar = CreateNewVar(v, pty);
  new_symbol.at(v->vstIdx) = newvar;
  return newvar;
}

MeExpr *MeTI::StripExpr(MeExpr *expr) {
  if (expr->op == OP_cvt) {
    OpMeExpr *opmeexpr = static_cast<OpMeExpr *>(expr);
    return StripExpr(opmeexpr->GetOpnd(0));
  }
  return expr;
}

MeExpr *MeTI::RebuildExpr(MeExpr *meexpr) {
  MeExprOp meOp = meexpr->meOp;
  switch (meOp) {
    case kMeOpVar: {
      VarMeExpr *var = static_cast<VarMeExpr *>(meexpr);
      PrimType pty = inferred_type.at(var->vstIdx);
      if (infer_status[var->vstIdx] == kMustInfer && var->primType != pty) {
        MIRSymbol *sym = var->ost->GetMIRSymbol();
        if (var->ost->versionsIndex.size() == 1) {
          if (sym && sym->GetType()->primType != pty) {
            MIRType *type = GlobalTables::GetTypeTable().typeTable[pty];
            sym->SetTyIdx(type->tyIdx);
          }
          var->primType = pty;
        } else if (sym) {
          PrimType symPty = sym->GetType()->primType;
          if (pty != symPty && !(IsPrimitiveDynType(pty) && IsPrimitiveDynType(symPty))) {
            return GetOrCreateNewVar(var, pty);
          }
        }
      }
      return meexpr;
    }
    case kMeOpOp: {
      OpMeExpr *meopexpr = static_cast<OpMeExpr *>(meexpr);
      if (meopexpr->GetOpnd(0)) {
        meopexpr->SetOpnd(RebuildExpr(meopexpr->GetOpnd(0)), 0);
      }
      if (meopexpr->GetOpnd(1)) {
        meopexpr->SetOpnd(RebuildExpr(meopexpr->GetOpnd(1)), 1);
      }
      if (meopexpr->GetOpnd(2)) {
        meopexpr->SetOpnd(RebuildExpr(meopexpr->GetOpnd(2)), 2);
      }
      switch (meexpr->op) {
        case OP_cvt: {
          PrimType pty = meopexpr->primType;
          if (!IsPrimitiveDynType(meopexpr->GetOpnd(0)->primType) && !IsPrimitiveDynType(pty) &&
              (meopexpr->GetOpnd(0)->primType != pty &&
               !(IsPrimitiveInteger(pty) && IsPrimitiveInteger(meopexpr->GetOpnd(0)->primType)))) {
            meopexpr->SetOpnd(CreateExprTypeCvt(OP_cvt, meopexpr->GetOpnd(0), GetDynType(meopexpr->GetOpnd(0)->primType)), 0);
            MapleVector<MeExpr *> opnds(irMap->irMapAlloc.Adapter());
            opnds.push_back(meopexpr->GetOpnd(0));
#if (MIR_JAVA == 0)
            switch (pty) {
              case PTY_u1:
                return CreateIntrExpr(INTRN_JS_BOOLEAN, opnds, pty);
              case PTY_a32:
              case PTY_i32:
              case PTY_u32:
                return CreateIntrExpr(INTRN_JS_NUMBER, opnds, pty);
              default:
                break;
            }
#endif
          }
          if (meopexpr->GetOpnd(0)->primType != meopexpr->opndType) {
            meopexpr->opndType = meopexpr->GetOpnd(0)->primType;
          }
          if (meopexpr->GetOpnd(0)->primType == meopexpr->primType) {
            return meopexpr->GetOpnd(0);
          }
          break;
        }
        case OP_add:
        case OP_sub:
        case OP_ne:
        case OP_eq:
        case OP_lt:
        case OP_le:
        case OP_gt:
        case OP_div: {
          MeExpr *opnd0 = meopexpr->GetOpnd(0);
          MeExpr *opnd1 = meopexpr->GetOpnd(1);
          if (!(StripExpr(opnd0)->primType == PTY_simplestr) && !(StripExpr(opnd1)->primType == PTY_simplestr)) {
            opnd0 = StripExpr(opnd0);
            opnd1 = StripExpr(opnd1);
          }
          if (opnd0->primType != opnd1->primType) {
            if (IsPrimitiveDynType(opnd0->primType) && !IsPrimitiveDynType(opnd1->primType)) {
              PrimType pty = GetNonDynType(opnd0->primType);
              if (opnd0->meOp == kMeOpConst &&
                  (pty == opnd1->primType || (IsPrimitiveInteger(pty) && IsPrimitiveInteger(opnd1->primType)))) {
                meopexpr->SetOpnd(RebuildConst(opnd0, opnd1->primType), 0);
                meopexpr->SetOpnd(opnd1, 1);
                meopexpr->opndType = meopexpr->GetOpnd(0)->primType;
              } else {
                if (IsPrimitiveDynType(meopexpr->opndType))
                  meopexpr->SetOpnd(CreateExprTypeCvt(OP_cvt, opnd1, GetDynType(opnd1->primType)), 1);
                else
                  meopexpr->SetOpnd(CreateExprTypeCvt(OP_cvt, opnd0, meopexpr->opndType), 0);
              }
            }
            if (IsPrimitiveDynType(opnd1->primType) && !IsPrimitiveDynType(opnd0->primType)) {
              PrimType pty = GetNonDynType(opnd1->primType);
              if (opnd1->meOp == kMeOpConst &&
                  (pty == opnd0->primType || (IsPrimitiveInteger(pty) && IsPrimitiveInteger(opnd0->primType)))) {
                meopexpr->SetOpnd(opnd0, 0);
                meopexpr->SetOpnd(RebuildConst(opnd1, opnd0->primType), 1);
                meopexpr->opndType = meopexpr->GetOpnd(0)->primType;
              } else {
                if (IsPrimitiveDynType(meopexpr->opndType))
                  meopexpr->SetOpnd(CreateExprTypeCvt(OP_cvt, opnd0, GetDynType(opnd0->primType)), 0);
                else
                  meopexpr->SetOpnd(CreateExprTypeCvt(OP_cvt, opnd1, meopexpr->opndType), 1);
              }
            }
          } else if (!IsPrimitiveDynType(opnd1->primType)) {
            meopexpr->SetOpnd(opnd0, 0);
            meopexpr->SetOpnd(opnd1, 1);
            meopexpr->opndType = meopexpr->GetOpnd(0)->primType;
          } else {
            // simplify expr such as (eq u1 dyni32 (constVal dyni32 0x40000000b, constVal dyni32 0x40000000b) to be
            // (eq u1 i32 (constVal i32 0x40000000b, constVal i32 0x40000000b)
            if (opnd0->op == OP_constval && opnd1->op == OP_constval) {
              PrimType pty = GetNonDynType(opnd0->primType);
              if (!IsPrimitiveDynType(pty)) {
                meopexpr->SetOpnd(RebuildConst(opnd0, pty), 0);
                meopexpr->SetOpnd(RebuildConst(opnd1, pty), 1);
                meopexpr->opndType = meopexpr->GetOpnd(0)->primType;
              }
            }
          }

          break;
        }
        default:
          break;
      }
      break;
    }
    case kMeOpNary: {
      if (meexpr->op == OP_intrinsicop) {
        meexpr = RebuildIntrinop(meexpr);
        break;
      }
      NaryMeExpr *narymeexpr = static_cast<NaryMeExpr *>(meexpr);
      for (size_t i = 0; i < narymeexpr->numOpnds; i++) {
        narymeexpr->SetOpnd(RebuildExpr(narymeexpr->GetOpnd(i)), i);
      }
      break;
    }
    default:
      break;
  }
  return meexpr;
}

MeExpr *MeTI::RebuildIntrinop(MeExpr *meexpr) {
  NaryMeExpr *intrinmeexpr = static_cast<NaryMeExpr *>(meexpr);
  MapleVector<MeExpr *> opnds(irMap->irMapAlloc.Adapter());
  for (size_t i = 0; i < intrinmeexpr->numOpnds; i++) {
    opnds.push_back(intrinmeexpr->GetOpnd(i));
  }
#if (MIR_JAVA == 0)
  MIRIntrinsicID intrinsic = intrinmeexpr->intrinsic;
  switch (intrinsic) {
    case INTRN_JS_NUMBER:
    case INTRN_JS_INT32:
    case INTRN_JS_BOOLEAN:
    case INTRN_JS_STRING: {
      MeExpr *opnd = RebuildExpr(opnds[0]);
      if (intrinmeexpr->primType == opnd->primType) {
        return opnd;
      }
      opnd = StripExpr(opnd);
      if (intrinmeexpr->primType == opnd->primType) {
        return opnd;
      } else if (GetNonDynType(opnd->primType) == intrinmeexpr->primType && opnd->op == OP_constval) {
        return RebuildConst(opnd, intrinmeexpr->primType);
      }
      // Optimization for JS_NUMBER/JS_INT32
      if (intrinsic == INTRN_JS_NUMBER || intrinsic == INTRN_JS_INT32) {
        if (opnd->op == OP_constval) {
          if (opnd->primType == PTY_dynundef || opnd->primType == PTY_dynnull) {
            return CreateConst(0, PTY_i32);
          }
          if (IsPrimitiveInteger(GetNonDynType(opnd->primType))) {
            return RebuildConst(opnd, PTY_i32);
          }
        }
      }
      // Optimization for JS_BOOLEAN
      if (intrinmeexpr->intrinsic == INTRN_JS_BOOLEAN) {
        if (opnd->op == OP_constval && IsPrimitiveInteger(GetNonDynType(opnd->primType))) {
          ConstMeExpr *const_expr = static_cast<ConstMeExpr *>(opnd);
          MIRIntConst *int_const = static_cast<MIRIntConst *>(const_expr->constVal);
          int64_t val = (int_const->value == 0) ? 0 : 1;
          return CreateConst(val, intrinmeexpr->primType);
        }
        if (opnd->primType == PTY_simplestr || IsPrimitiveInteger(opnd->primType)) {
          PrimType pty = opnd->primType;
          MeExpr *opnd0 = opnd;
          if (opnd->primType == PTY_simplestr) {
            MapleVector<MeExpr *> opnds(irMap->irMapAlloc.Adapter());
            opnds.push_back(opnd);
            opnd0 = CreateIntrExpr(INTRN_JSSTR_LENGTH, opnds, PTY_u32);
            pty = PTY_u32;
          }
          MeExpr *opnd1 = CreateConst(0, pty);
          return CreateBinary(OP_ne, opnd0, opnd1, PTY_u1);
        }
      }
      break;
    }
    case INTRN_JSOP_STRICTNE:
    case INTRN_JSOP_STRICTEQ: {
      MeExpr *opnd0 = RebuildExpr(opnds[0]);
      MeExpr *opnd1 = RebuildExpr(opnds[1]);
      if (opnd0->primType == opnd1->primType) {
        if (opnd0->primType == PTY_simplestr) {
          MIRIntrinsicID intrinsic =
            (intrinmeexpr->intrinsic == INTRN_JSOP_STRICTNE) ? INTRN_JSSTR_STRICTNE : INTRN_JSSTR_STRICTEQ;
          return CreateIntrExpr(intrinsic, opnds, PTY_u1);
        }
        if (IsPrimitiveInteger(opnd0->primType)) {
          if (intrinmeexpr->intrinsic == INTRN_JSOP_STRICTNE) {
            return CreateBinary(OP_ne, opnd0, opnd1, PTY_u1);
          } else {
            return CreateBinary(OP_eq, opnd0, opnd1, PTY_u1);
          }
        }
      }
      break;
    }
    default:
      break;
  }
#endif
  for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
    *it = RebuildExpr(*it);
    if ((*it)->meOp == kMeOpVar) {
      // if var's type has been inferred, insert cvt
      VarMeExpr *var = static_cast<VarMeExpr *>(*it);
      if (infer_status[var->vstIdx] == kMustInfer) {
        *it = CreateExprTypeCvt(OP_cvt, *it, orig_type[var->vstIdx]);
      }
    }
  }
  return meexpr;
}

void MeTI::RebuildMuList(MapleMap<OStIdx, ScalarMeExpr *> &mulist) {
  for (MapleMap<OStIdx, ScalarMeExpr *>::iterator it = mulist.begin(); it != mulist.end(); it++) {
    mulist[it->first] = static_cast<VarMeExpr *>(RebuildExpr(it->second));
  }
}

void MeTI::RebuildChiList(MapleMap<OStIdx, ChiMeNode *> &chilist) {
  for (MapleMap<OStIdx, ChiMeNode *>::iterator it = chilist.begin(); it != chilist.end(); it++) {
    ChiMeNode *chi = it->second;
    VarMeExpr *lhs = static_cast<VarMeExpr *>(RebuildExpr(chi->lhs));
    chi->rhs = static_cast<VarMeExpr *>(RebuildExpr(chi->rhs));
    if (lhs->exprID != chi->lhs->exprID) {
      lhs->defBy = kDefByChi;
      lhs->def.defChi = chi;
      chi->lhs = lhs;
    }
  }
}

void MeTI::RebuildMustDefList(IntrinsiccallMeStmt *intrin, MapleVector<MustDefMeNode> *mustdefList) {
  if ((*mustdefList).size() == 0) {
    return;
  }
  MeExpr *melhs = (*mustdefList)[0].lhs;
  CHECK_FATAL(melhs->meOp == kMeOpVar, "expected var");
  VarMeExpr *ret = static_cast<VarMeExpr *>(melhs);
  VarMeExpr *newret = static_cast<VarMeExpr *>(RebuildExpr(ret));
  if (newret->exprID != ret->exprID) {
    (*mustdefList)[0].lhs = newret;
  }
}

void MeTI::RebuildStmt(MeStmt *stmt) {
  switch (stmt->op) {
    case OP_dassign: {
      DassignMeStmt *mestmt = static_cast<DassignMeStmt *>(stmt);
      VarMeExpr *lhs = static_cast<VarMeExpr *>(RebuildExpr(mestmt->lhs));
      if (lhs->exprID != mestmt->lhs->exprID) {
        lhs->def.defStmt = mestmt;
        mestmt->lhs = lhs;
      }
      MeExpr *newrhs = RebuildExpr(mestmt->rhs);
      PrimType lpty = mestmt->lhs->primType;
      OpMeExpr *oexpr = static_cast<OpMeExpr *>(newrhs);
      if (newrhs->meOp == kMeOpOp && newrhs->op == OP_cvt && oexpr->opndType == lpty) {
        mestmt->rhs = oexpr->GetOpnd(0);
        break;
      } else {
        mestmt->rhs = newrhs;
      }

      PrimType rpty = mestmt->rhs->primType;
      if (lpty != rpty && lpty != kPtyInvalid) {
        if (!(IsPrimitiveDynType(lpty) && IsPrimitiveDynType(rpty))) {
          mestmt->rhs = CreateExprTypeCvt(OP_cvt, mestmt->rhs, lpty);
        }
      }
      RebuildChiList(mestmt->chiList);
      break;
    }
    case OP_callassigned:
    case OP_icallassigned:
    case OP_virtualcallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_call:
    case OP_icall:
    case OP_virtualcall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_customcall:
    case OP_polymorphiccall: {
      CallMeStmt *callmestmt = static_cast<CallMeStmt *>(stmt);
      MapleVector<MeExpr *> &opnds = callmestmt->opnds;
      for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
        *it = RebuildExpr(*it);
        if ((*it)->meOp == kMeOpVar) {
          // if var's type has been inferred, insert cvt
          VarMeExpr *var = static_cast<VarMeExpr *>(*it);
          if (infer_status[var->vstIdx] == kMustInfer) {
            *it = CreateExprTypeCvt(OP_cvt, *it, orig_type[var->vstIdx]);
          }
        }
      }
      RebuildMuList(callmestmt->muList);
      RebuildChiList(callmestmt->chiList);
      break;
    }
    case OP_intrinsiccallwithtype:
    case OP_intrinsiccall:
    case OP_xintrinsiccall: {
      IntrinsiccallMeStmt *intrincallstmt = static_cast<IntrinsiccallMeStmt *>(stmt);
      MapleVector<MeExpr *> &opnds = intrincallstmt->opnds;
      for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
        if ((*it)->meOp == kMeOpVar) {
          VarMeExpr *var = static_cast<VarMeExpr *>(*it);
          if (infer_status[var->vstIdx] == kMustInfer) {
            *it = RebuildExpr(*it);
            if (!IsPrimitiveDynType((*it)->primType)) {
              *it = CreateExprTypeCvt(OP_cvt, *it, orig_type[var->vstIdx]);
            }
          }
        } else {
          *it = RebuildExpr(*it);
        }
      }
      RebuildMuList(intrincallstmt->muList);
      RebuildChiList(intrincallstmt->chiList);
      break;
    }
    case OP_intrinsiccallwithtypeassigned:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned: {
      IntrinsiccallMeStmt *intrin = static_cast<IntrinsiccallMeStmt *>(stmt);
      RebuildMustDefList(intrin, &(intrin->mustDefList));
      MapleVector<MeExpr *> &opnds = intrin->opnds;
#if (MIR_JAVA == 0)
      if (intrin->intrinsic == INTRN_JSOP_ADD && intrin->mustDefList.size() != 0) {
        MustDefMeNode node = intrin->mustDefList[0];
        MeExpr *melhs = node.lhs;
        CHECK_FATAL(melhs->meOp == kMeOpVar, "NYI");
        VarMeExpr *lhs = static_cast<VarMeExpr *>(melhs);
        // Change to OP_add when both operands's types are interger
        if (infer_status[lhs->vstIdx] == kMustInfer && inferred_type[lhs->vstIdx] == PTY_i32) {
          MeExpr *op0 = StripExpr(RebuildExpr(opnds[0]));
          MeExpr *op1 = StripExpr(RebuildExpr(opnds[1]));
          CHECK_FATAL(op0->primType == PTY_i32 || (op0->primType == PTY_dyni32 && op0->meOp == kMeOpConst), "");
          CHECK_FATAL(op1->primType == PTY_i32 || (op1->primType == PTY_dyni32 && op1->meOp == kMeOpConst), "");
          if (op0->primType == PTY_dyni32) {
            op0 = RebuildConst(op0, PTY_i32);
          }
          if (op1->primType == PTY_dyni32) {
            op1 = RebuildConst(op1, PTY_i32);
          }
          lhs = static_cast<VarMeExpr *>(RebuildExpr(lhs));
          MeExpr *rhs = CreateBinary(OP_add, op0, op1, lhs->primType);
          MeStmt *defStmt = CreateDassign(node.lhs, rhs, 0, stmt->bb->id);
          ReplaceMeStmt(stmt, defStmt);
          return;
        }
        // Change to INTRN_concat when both operands's types are simplestr
        if (infer_status[lhs->vstIdx] == kMustInfer && inferred_type[lhs->vstIdx] == PTY_simplestr) {
          MeExpr *op0 = StripExpr(RebuildExpr((opnds[0])));
          MeExpr *op1 = StripExpr(RebuildExpr((opnds[1])));

          if (op0->primType != PTY_simplestr) {
            if (!IsPrimitiveDynType(op0->primType)) {
              op0 = CreateExprTypeCvt(OP_cvt, op0, GetDynType(op0->primType));
            }

            MapleVector<MeExpr *> opnds(irMap->irMapAlloc.Adapter());
            opnds.push_back(op0);
            op0 = CreateIntrExpr(INTRN_JS_STRING, opnds, PTY_simplestr);
          } else if (op1->primType != PTY_simplestr) {
            if (!IsPrimitiveDynType(op1->primType)) {
              op1 = CreateExprTypeCvt(OP_cvt, op1, GetDynType(op0->primType));
            }
            MapleVector<MeExpr *> opnds(irMap->irMapAlloc.Adapter());
            opnds.push_back(op1);
            op1 = CreateIntrExpr(INTRN_JS_STRING, opnds, PTY_simplestr);
          }
          CHECK_FATAL(op0->primType == PTY_simplestr, "");
          CHECK_FATAL(op1->primType == PTY_simplestr, "");
          intrin->intrinsic = INTRN_JSOP_CONCAT;
          intrin->mustDefList[0].lhs = static_cast<VarMeExpr *>(RebuildExpr(lhs));
          intrin->opnds[0] = op0;
          intrin->opnds[1] = op1;
          return;
        }
      }
#endif
      for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
        if ((*it)->meOp == kMeOpVar) {
          VarMeExpr *var = static_cast<VarMeExpr *>(*it);
          if (infer_status[var->vstIdx] == kMustInfer) {
            *it = RebuildExpr(*it);
            if (!IsPrimitiveDynType((*it)->primType)) {
              *it = CreateExprTypeCvt(OP_cvt, *it, orig_type[var->vstIdx]);
            }
          }
        } else {
          *it = RebuildExpr(*it);
        }
      }
      RebuildMuList(intrin->muList);
      RebuildChiList(intrin->chiList);
      break;
    }
    case OP_iassign: {
      IassignMeStmt *ivarstmt = static_cast<IassignMeStmt *>(stmt);
      ivarstmt->lhsVar->base = RebuildExpr(ivarstmt->lhsVar->base);
      ivarstmt->rhs = RebuildExpr(ivarstmt->rhs);
      // get the type of the field being stored into
      MIRType *typ = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ivarstmt->tyIdx);
      MIRPtrType *pointerty = static_cast<MIRPtrType *>(typ);
      if (ivarstmt->lhsVar->fieldID != 0) {
        MIRStructType *structty =
          dynamic_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx));
        CHECK_FATAL(structty, "RebuildStmt: non-zero fieldID for non-structure");
        TyIdx ftyidx = structty->TraverseToField(ivarstmt->lhsVar->fieldID).second.first;
        typ = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx);
      } else {
        MIRArrayType *arrayty = dynamic_cast<MIRArrayType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx));
        if (arrayty) {
          typ = GlobalTables::GetTypeTable().GetTypeFromTyIdx(arrayty->eTyIdx);
        }
      }
      PrimType lpty = typ->primType;
      PrimType rpty = ivarstmt->rhs->primType;
      if (lpty != rpty) {
        ivarstmt->rhs = CreateExprTypeCvt(OP_cvt, ivarstmt->rhs, lpty);
      }
      RebuildChiList(ivarstmt->chiList);
      break;
    }
    case OP_syncenter:
    case OP_syncexit: {
      SyncMeStmt *syncmestmt = static_cast<SyncMeStmt *>(stmt);
      MapleVector<MeExpr *> &opnds = syncmestmt->opnds;
      for (MapleVector<MeExpr *>::iterator it = opnds.begin(); it != opnds.end(); it++) {
        *it = RebuildExpr(*it);
      }
      RebuildMuList(syncmestmt->muList);
      RebuildChiList(syncmestmt->chiList);
      break;
    }
    case OP_brtrue:
    case OP_brfalse: {
      CondGotoMeStmt *mestmt = static_cast<CondGotoMeStmt *>(stmt);
      mestmt->opnd = RebuildExpr(mestmt->opnd);
      break;
    }
    case OP_throw: {
      ThrowMeStmt *tmestmt = static_cast<ThrowMeStmt *>(stmt);
      tmestmt->opnd = RebuildExpr(tmestmt->opnd);
      if (!IsPrimitiveDynType(tmestmt->opnd->primType)) {
        tmestmt->opnd = CreateExprTypeCvt(OP_cvt, tmestmt->opnd, PTY_dynany);
      }
      RebuildMuList(tmestmt->muList);
      break;
    }
    case OP_gosub:
    case OP_retsub: {
      WithMuMeStmt *mumestmt = static_cast<WithMuMeStmt *>(stmt);
      RebuildMuList(mumestmt->muList);
      break;
    }
    case OP_return: {
      RetMeStmt *retstmt = static_cast<RetMeStmt *>(stmt);
      for (uint32 i = 0; i < retstmt->opnds.size(); i++) {
        if (retstmt->opnds[i]->meOp == kMeOpVar) {
          VarMeExpr *var = static_cast<VarMeExpr *>(retstmt->opnds[i]);
          if (infer_status[var->vstIdx] == kMustInfer) {
            retstmt->opnds[i] = RebuildExpr(var);
            if (!IsPrimitiveDynType(retstmt->opnds[i]->primType))
              retstmt->opnds[i] = CreateExprTypeCvt(OP_cvt, retstmt->opnds[i], orig_type[var->vstIdx]);
          }
        } else {
          retstmt->opnds[i] = RebuildExpr(retstmt->opnds[i]);
        }
      }
      RebuildMuList(retstmt->muList);
      break;
    }
    default:
      break;
  }
}

void MeTI::RebuildBB(BB *bb) {
  if (bb_rebuilt[bb->id.idx]) {
    return;
  } else {
    bb_rebuilt.at(bb->id.idx) = true;
  }

  for (std::pair<OStIdx, MePhiNode *> phiEntry : bb->mePhiList) {
    MePhiNode *phi = phiEntry.second;
    if (!phi->isLive) {
      continue;
    }
    bool lhsmustinfer = (infer_status[static_cast<VarMeExpr *>(phi->lhs)->vstIdx] == kMustInfer);
    VarMeExpr *lhs = static_cast<VarMeExpr *>(RebuildExpr(phi->lhs));

    if (lhsmustinfer) {
      // In case all symbols are changed of the phi;
      // MeStmt *dassign = CreateDassign(phi->lhs, CreateExprTypeCvt(OP_cvt, lhs, phi->lhs->pty), 0, bb);
      // bb->InsertMeStmtLastBr(dassign);
      phi->UpdateLhs(lhs);
    }
    for (uint32_t i = 0; i < phi->opnds.size(); i++) {
      uint32_t id = static_cast<VarMeExpr *>(phi->opnds[i])->vstIdx;
      if (infer_status[id] == kMustInfer && !lhsmustinfer) {
        PrimType lhspty = lhs->primType;
        if (lhspty == kPtyInvalid) {
          lhspty = lhs->ost->GetMIRSymbol()->GetType()->primType;
        }
        if (!(IsPrimitiveDynType(inferred_type[id]) && IsPrimitiveDynType(lhspty))) {
          BB *fromBb = phi->defBB->pred[i];
          RebuildBB(fromBb);
          MeExpr *medef = GetOrCreateNewVar(static_cast<VarMeExpr *>(phi->opnds[i]), inferred_type[id]);
          MeStmt *dassign = CreateDassign(phi->opnds[i], CreateExprTypeCvt(OP_cvt, medef, lhspty), 0, fromBb->id);
          fromBb->InsertMeStmtLastBr(dassign);
        }
      } else if (lhsmustinfer && infer_status[id] != kMustInfer) {
        BB *fromBb = phi->defBB->pred[i];
        RebuildBB(fromBb);
        MeExpr *medef = CreateNewVar(static_cast<VarMeExpr *>(phi->opnds[i]), lhs->primType);
        MeExpr *var = nullptr;
#if (MIR_JAVA == 0)
        MapleVector<MeExpr *> opnds(irMap->irMapAlloc.Adapter());
        opnds.push_back(phi->opnds[i]);
        if (IsPrimitiveInteger(lhs->primType)) {
          var = CreateIntrExpr(INTRN_JS_NUMBER, opnds, lhs->primType);
        } else if (lhs->primType == PTY_simplestr) {
          var = CreateIntrExpr(INTRN_JS_STRING, opnds, lhs->primType);
        } else
#endif
          var = CreateExprTypeCvt(OP_cvt, phi->opnds[i], lhs->primType);
        MeStmt *dassign = CreateDassign(medef, var, 0, fromBb->id);
        fromBb->InsertMeStmtLastBr(dassign);
        phi->opnds[i] = static_cast<VarMeExpr *>(medef);
      } else {
        phi->opnds[i] = static_cast<VarMeExpr *>(RebuildExpr(phi->opnds[i]));
      }
    }
  }
  for (auto mestmt : bb->meStmtList) {
    RebuildStmt(mestmt);
  }
}

void MeTI::TypeInfer() {
  Init();  // steps 1 and 2
  DumpStatus();
  PropagateMustNotInfer();  // step 3
  DumpStatus();
  DetermineMustOrMaybeInfer();  // step 4
  DumpStatus();
  MainPropagation();  // step 5
  DumpStatus();
  // rebuild BBs via preorder traversal of the dominator tree
  for (BBId bbid : dom->dtPreOrder) {
    RebuildBB(func->bbVec[bbid.idx]);
  }
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============== TypeInference =============" << endl;
    irMap->Dump();
  }
}

AnalysisResult *MeDohTI::Run(MeFunction *func, MeFuncResultMgr *m) {
  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));

  MeIRMap *hmap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func));
  ASSERT(hmap != nullptr, "hssamap is nullptr");
  MemPool *timp = mempoolctrler.NewMemPool(PhaseName().c_str());

  MeTI *meti = timp->New<MeTI>(func, dom, hmap, timp);

  MIRFunction *mirfunction = func->mirFunc;

  meti->TypeInfer();

  mempoolctrler.DeleteMemPool(timp);

  return nullptr;
}

}  // namespace maple
