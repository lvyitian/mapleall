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

#include "me_stmt_pre.h"

namespace maple {

void MeStmtPre::ResetFullyAvail(MePhiOcc *occg) {
  occg->is_canbeavail = false;
  // reset those phiocc nodes that have oc as one of its operands
  for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
    MePhiOcc *phiocc = *it;
    MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
    for (uint32 i = 0; i < phiopnds.size(); i++) {
      MePhiOpndOcc *phiopnd = phiopnds[i];
      if (phiopnd->def && phiopnd->def == occg) {
        // phiopnd is a use of occg
        if (!phiopnd->has_real_use && phiocc->is_canbeavail) {
          ResetFullyAvail(phiocc);
        }
      }
    }
  }
}

// the fullyavail attribute is stored in the is_canbeavail field
void MeStmtPre::ComputeFullyAvail() {
  for (MapleVector<MePhiOcc *>::iterator it = phi_occs.begin(); it != phi_occs.end(); it++) {
    MePhiOcc *phiocc = *it;
    if (phiocc->is_canbeavail) {
      // reset canbeavail if any phi operand is null
      bool existNullDef = false;
      MapleVector<MePhiOpndOcc *> &phiopnds = phiocc->phiopnds;
      for (uint32 i = 0; i < phiopnds.size(); i++) {
        MePhiOpndOcc *phiopnd = phiopnds[i];
        if (phiopnd->def == nullptr) {
          existNullDef = true;
          break;
        }
      }
      if (existNullDef) {
        ResetFullyAvail(phiocc);
      }
    }
  }
}

bool MeStmtPre::AllVarsSameVersionStmtFre(MeRealOcc *topocc, MeRealOcc *curocc) const {
  ASSERT(topocc->mestmt->op == OP_dassign, "AllVarsSameVersionStmtFre: only dassign is handled");
  DassignMeStmt *dass1 = static_cast<DassignMeStmt *>(topocc->mestmt);
  DassignMeStmt *dass2 = static_cast<DassignMeStmt *>(curocc->mestmt);
  if (dass1->rhs != dass2->rhs) {
    return false;
  }
  return dass1->lhs == curocc->meexpr;
}

void MeStmtPre::Rename1StmtFre() {
  std::stack<MeOccur *> occStack;
  rename2_set.clear();
  class_count = 1;
  // iterate the occurrence according to its preorder dominator tree
  for (MeOccur *occ : all_occs) {
    while (!occStack.empty() && !occStack.top()->IsDominate(dominance, occ)) {
      occStack.pop();
    }
    switch (occ->occty) {
      case kOccReal: {
        if (occStack.empty()) {
          // assign new class
          occ->classid = class_count++;
          occStack.push(occ);
          break;
        }
        MeOccur *topoccur = occStack.top();
        if (topoccur->occty == kOccMembar) {
          occ->classid = class_count++;
          occStack.push(occ);
          break;
        }
        MeRealOcc *realocc = static_cast<MeRealOcc *>(occ);
        if (topoccur->occty == kOccReal) {
          MeRealOcc *realtopoccur = static_cast<MeRealOcc *>(topoccur);
          if (AllVarsSameVersionStmtFre(realtopoccur, realocc)) {
            // all corresponding variables are the same
            realocc->classid = realtopoccur->classid;
            realocc->def = realtopoccur;
          } else {
            // assign new class
            occ->classid = class_count++;
          }
          occStack.push(occ);
        } else {
          // top of stack is a PHI occurrence
          std::vector<MeExpr *> varVec;
          CollectVarForCand(realocc, varVec);
          bool isalldom = true;
          for (std::vector<MeExpr *>::iterator varit = varVec.begin(); varit != varVec.end(); varit++) {
            MeExpr *varmeexpr = *varit;
            if (!DefVarDominateOcc(varmeexpr, topoccur)) {
              isalldom = false;
            }
          }
          if (isalldom) {
            realocc->classid = topoccur->classid;
            realocc->def = topoccur;
            rename2_set.insert(realocc->position);
            occStack.push(realocc);
          } else {
            // assign new class
            occ->classid = class_count++;
          }
          occStack.push(occ);
        }
        break;
      }
      case kOccPhiocc: {
        // assign new class
        occ->classid = class_count++;
        occStack.push(occ);
        break;
      }
      case kOccPhiopnd: {
        if (occStack.empty() || occStack.top()->occty == kOccMembar) {
          occ->def = nullptr;
        } else {
          MeOccur *topoccur = occStack.top();
          occ->def = topoccur;
          occ->classid = topoccur->classid;
          if (topoccur->occty == kOccReal) {
            static_cast<MePhiOpndOcc *>(occ)->has_real_use = true;
          }
        }
        break;
      }
      case kOccExit:
        break;
      case kOccMembar:
        if (occStack.empty() || occStack.top()->occty != kOccMembar) {
          occStack.push(occ);
        }
        break;
      default:
        ASSERT(false, "");
    }
  }

  if (ssapredebug) {
    PreWorkCand *curCand = work_cand;
    LogInfo::MapleLogger() << "======== ssafre candidate " << curCand->index << " after rename1StmtFre ===================\n";

    for (MeOccur *occ : all_occs) {
      occ->Dump(irMap);
      LogInfo::MapleLogger() << std::endl;
    }
    LogInfo::MapleLogger() << "\n"
                    << "rename2 set:\n";
    for (uint32_t pos : rename2_set) {
      MeRealOcc *occur = work_cand->real_occs[pos];
      occur->Dump(irMap);
      LogInfo::MapleLogger() << " with def at\n";
      occur->def->Dump(irMap);
      LogInfo::MapleLogger() << "\n";
    }
    LogInfo::MapleLogger() << "\n";
  }
}

void MeStmtPre::DoSSAFRE() {
  if (ssapredebug) {
    LogInfo::MapleLogger() << "{{{{{{{{ start of SSAFRE }}}}}}}}" << std::endl;
  }
  // form new all_occs that has no use_occ and reflect insertions and deletions
  MapleVector<MeOccur *> newAllOccs(percand_allocator.Adapter());
  ;
  int32 realoccCnt = 0;
  bool hasInsertion = false;  // if there is insertion, do not perform SSAFRE
  // because SSA form has not been updated
  for (MeOccur *occ : all_occs) {
    switch (occ->occty) {
      case kOccReal: {
        MeRealOcc *realocc = static_cast<MeRealOcc *>(occ);
        if (!realocc->is_reload) {
          realocc->is_reload = false;
          realocc->is_save = false;
          realocc->classid = 0;
          realocc->def = nullptr;
          newAllOccs.push_back(realocc);
          realoccCnt++;
        }
        break;
      }
      case kOccPhiopnd: {
        MePhiOpndOcc *phiopnd = static_cast<MePhiOpndOcc *>(occ);
        if (phiopnd->def_phiocc->IsWillBeAvail()) {
          MeOccur *defocc = phiopnd->def;
          if (defocc && defocc->occty == kOccInserted && !phiopnd->is_phiopndreload) {
            hasInsertion = true;
            break;
          }
        }
        phiopnd->is_processed = false;
        phiopnd->has_real_use = false;
        phiopnd->is_insertedocc = false;
        phiopnd->is_phiopndreload = false;
        phiopnd->current_expr.mestmt = nullptr;
        phiopnd->classid = 0;
        phiopnd->def = nullptr;
        newAllOccs.push_back(phiopnd);
        break;
      }
      case kOccPhiocc: {
        MePhiOcc *phiocc = static_cast<MePhiOcc *>(occ);
        phiocc->is_downsafety = false;
        phiocc->is_canbeavail = true;
        phiocc->is_later = false;
        phiocc->is_extraneous = false;
        phiocc->is_removed = false;
        phiocc->classid = 0;
        newAllOccs.push_back(phiocc);
        break;
      }
      case kOccExit:
      case kOccMembar:
        newAllOccs.push_back(occ);
        break;
      case kOccUse:
        break;
      default:
        ASSERT(false, "");
    }
    if (hasInsertion) {
      break;
    }
  }
  if (hasInsertion || realoccCnt <= 1) {
    return;
  }
  all_occs = newAllOccs;
  Rename1StmtFre();
  Rename2();
  ComputeFullyAvail();
  Finalize1();
  Finalize2();
  CodeMotion();
}

}  // namespace maple
