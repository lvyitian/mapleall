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

#include "sync_select.h"

namespace maple {

void SyncSelect::AddFunc(uint32_t kind, const char *funcname) {
  MapleString str(funcname, mp);
  syncmap_[str] = kind;
  return;
}

void SyncSelect::CollectFuncs() {
  syncmap_.clear();
  return;
}

uint32_t SyncSelect::GetSyncKind(const StmtNode *stmt) const {
  CHECK_FATAL(stmt->op == OP_syncenter, "expect syncenter in GetSyncKind");
  return 2;
}

void SyncSelect::SetSyncKind(StmtNode *stmt, uint32_t n) const {
  CHECK_FATAL(stmt->op == OP_syncenter, "expect syncenter in SetSyncKind");
  NaryStmtNode *nd = dynamic_cast<NaryStmtNode *>(stmt);
  CHECK_FATAL(nd && nd->nOpnd.size() > 1, "access nd->nOpnd failed");
  ConstvalNode *cst = static_cast<ConstvalNode *>(nd->nOpnd[1]);
  MIRIntConst *intconst = dynamic_cast<MIRIntConst *>(cst->constVal);
  CHECK_FATAL(intconst != nullptr, "intconst is null in SyncSelect::SetSyncKind");
  intconst->value = n;
  return;
}

void SyncSelect::SetAllSyncKind(uint32_t n) {
  for (auto bb : currfunc_->bbVec) {
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->stmtNodeList) {
      if (stmt->op == OP_syncenter) {
        SetSyncKind(stmt, n);
      }
    }
  }
  return;
}

int SyncSelect::GetSyncFuncKind() {
  MapleMap<MapleString, uint32_t>::iterator it;
  MapleString temp(currfunc_->GetName(), mp);
  it = syncmap_.find(temp);
  if (it != syncmap_.end()) {
    return it->second;
  }
  return -1;
}

void SyncSelect::SyncOptimization() {
  // deal with func white list where all syncenter use same version
  int n = GetSyncFuncKind();
  if (n != -1) {
    SetAllSyncKind(n);
  }
  return;
}

AnalysisResult *MeDoSyncSelect::Run(MeFunction *func, MeFuncResultMgr *frm, ModuleResultMgr *mrm) {
  MemPool *syncmp = mempoolctrler.NewMemPool(PhaseName().c_str());
  SyncSelect *sync = syncmp->New<SyncSelect>(func, syncmp);
  sync->SyncOptimization();
  mempoolctrler.DeleteMemPool(syncmp);
  return nullptr;
}

}  // namespace maple
