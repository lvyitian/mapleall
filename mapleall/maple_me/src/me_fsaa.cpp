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

#include "me_fsaa.h"
#include "me_ssa.h"
#include "me_option.h"

// The FSAA phase performs flow-sensitive alias analysis.  This is flow
// sensitive because, based on the SSA that has been constructed, it is
// possible to look up the assigned value of pointers.  It focuses only on
// using known assigned pointer values to fine-tune the aliased items at
// iassign statements.  When the base of an iassign statements has an assigned
// value that is unique (i.e. of known value or is pointing to read-only
// memory), it will go through the maydefs attached to the iassign statement to
// trim away any whose base cannot possibly be the same value as the base's
// assigned value.  When any mayDef has bee deleted, the SSA form of the
// function will be updated by re-running only the SSA rename step, so as to
// maintain the correctness of the SSA form.

using namespace std;

namespace maple {

// if the pointer represented by vst is found to have a unique pointer value,
// return the BB of the definition
BB *FSAA::FindUniquePointerValueDefBB(VersionSt *vst) {
  if (vst->IsInitVersion()) {
    return nullptr;
  }
  BaseNode *rhs = nullptr;
  if (vst->defType == VersionSt::kDassign) {
    DassignNode *dass = vst->defStmt.dassign;
    rhs = dass->GetRhs();
  } else if (vst->defType == VersionSt::kRegassign) {
    RegassignNode *rass = vst->defStmt.regassign;
    rhs = rass->GetRhs();
  } else {
    return nullptr;
  }

  if (rhs->op == OP_malloc || rhs->op == OP_gcmalloc || rhs->op == OP_gcpermalloc ||
      rhs->op == OP_gcmallocjarray) {
    return vst->defBB;
  } else if (rhs->op == OP_dread) {
    AddrofSSANode *dread = static_cast<AddrofSSANode *>(rhs);
    OriginalSt *ost = dread->ssaVar->ost;
    if (ost->GetMIRSymbol()->IsLiteralPtr() || (ost->isFinal && !ost->isLocal)) {
      return vst->defBB;
    } else {  // rhs is another pointer; call recursively for its rhs
      return FindUniquePointerValueDefBB(dread->ssaVar);
    }
    return nullptr;
  } else if (rhs->op == OP_iread) {
    if (func->mirFunc->IsConstructor() || func->mirFunc->IsStatic() || func->mirFunc->formalDefVec.empty()) {
      return nullptr;
    }
    // check if rhs is reading a final field thru this
    IreadSSANode *iread = static_cast<IreadSSANode *>(rhs);
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(iread->tyIdx);
    MIRType *pointedty = static_cast<MIRPtrType *>(ty)->GetPointedType();
    if (pointedty->GetKind() == kTypeClass &&
        static_cast<MIRStructType *>(pointedty)->IsFieldFinal(iread->fieldID) &&
        iread->uOpnd->op == OP_dread) {
      AddrofSSANode *basedread = static_cast<AddrofSSANode *>(iread->uOpnd);
      MIRSymbol *mirst = basedread->ssaVar->ost->GetMIRSymbol();
      if (mirst == func->mirFunc->formalDefVec[0].formalSym) {
        return vst->defBB;
      }
    }
    return nullptr;
  }
  return nullptr;
}

void FSAA::ProcessBB(BB *bb) {
  for (auto stmt : bb->stmtNodeList) {
    if (stmt->op != OP_iassign) {
      continue;
    }
    IassignNode *iass = static_cast<IassignNode *>(stmt);
    VersionSt *vst = nullptr;
    if (iass->addrExpr->op == OP_dread) {
      vst = static_cast<AddrofSSANode *>(iass->addrExpr)->ssaVar;
    } else if (iass->addrExpr->op == OP_regread) {
      vst = static_cast<RegreadSSANode *>(iass->addrExpr)->ssaVar;
    } else {
      break;
    }
    BB *defBB = FindUniquePointerValueDefBB(vst);
    if (defBB != nullptr) {
      if (DEBUGFUNC(func)) {
        LogInfo::MapleLogger() << "FSAA finds unique pointer value def\n";
      }
      // delete any maydefnode in the list that is defined before defBB
      MapleMap<OStIdx, MayDefNode> *mayDefNodes = SSAGenericGetMayDefNodes(stmt, &ssaTab->stmtsSSAPart);

      bool hasErase;
      do {
        hasErase = false;
        MapleMap<OStIdx, MayDefNode>::iterator it = mayDefNodes->begin();
        // due to use of iterator, can do at most 1 erasion each iterator usage
        for (; it != mayDefNodes->end(); it++) {
          MayDefNode &mayDef = it->second;
          if (mayDef.base == nullptr) {
          } else {
            BB *aliasedDefBB = mayDef.base->defBB;
            if (aliasedDefBB == nullptr) {
              hasErase = true;
            } else {
              hasErase = defBB != aliasedDefBB && dom->Dominate(aliasedDefBB, defBB);
            }
          }
          if (hasErase) {
            if (DEBUGFUNC(func)) {
              LogInfo::MapleLogger() << "FSAA deletes mayDef of ";
              mayDef.result->Dump(mirModule);
              LogInfo::MapleLogger() << " in BB " << bb->id.idx << " at:" << endl;
              stmt->Dump(mirModule);
            }
            mayDefNodes->erase(it);
            needUpdateSSA = true;
            CHECK_FATAL(!mayDefNodes->empty(), "FSAA::ProcessBB: mayDefNodes of iassign rendered empty");
            break;
          }
        }
      } while (hasErase);
    }
  }
}

AnalysisResult *MeDoFSAA::Run(MeFunction *func, MeFuncResultMgr *m) {
  SSATab *ssaTab = static_cast<SSATab *>(m->GetAnalysisResult(MeFuncPhase_SSATAB, func));
  ASSERT(ssaTab != nullptr, "ssaTab phase has problem");

  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func));
  ASSERT(dom != nullptr, "dominance phase has problem");

  FSAA fsaa(func, dom);

  for (BB *bb : func->bbVec) {
    if (bb != nullptr) {
      fsaa.ProcessBB(bb);
    }
  }

  if (fsaa.needUpdateSSA) {
    MemPool *ssamp = mempoolctrler.NewMemPool(PhaseName().c_str());
    MeSSA ssa(func, ssaTab, dom, ssamp);
    ssa.runRenameOnly = true;

    ssa.InitRenameStack(&ssaTab->originalStTable, func->bbVec.size(), ssaTab->versionStTable);
    // recurse down dominator tree in pre-order traversal
    MapleSet<BBId> *children = &dom->domChildren[func->commonEntryBB->id.idx];
    for (BBId child : *children) {
      ssa.RenameBB(func->bbVec[child.idx]);
    }

    mempoolctrler.DeleteMemPool(ssamp);
    ssa.VerifySSA();

    if (DEBUGFUNC(func)) {
      func->DumpFunction();
    }
  }

  return nullptr;
}

}  // namespace maple
