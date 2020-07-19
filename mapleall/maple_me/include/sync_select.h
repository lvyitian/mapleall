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

#ifndef MAPLE_ME_INCLUDE_SYNC_SELECT_H
#define MAPLE_ME_INCLUDE_SYNC_SELECT_H
#include "me_function.h"
#include "me_cfg.h"
#include "mir_module.h"
#include "mir_nodes.h"
#include "mempool.h"
#include "mempool_allocator.h"

namespace maple {

// sync kinds:
// 0: spinlock
// 1:
// 2: default
// 3: system call

class SyncSelect {
 public:
  MeFunction *currfunc_;
  MemPool *mp;
  MapleAllocator alloc;
  MapleMap<MapleString, uint32_t> syncmap_;

  explicit SyncSelect(MeFunction *func, MemPool *mp) : currfunc_(func), mp(mp), alloc(mp), syncmap_(alloc.Adapter()) {
    CollectFuncs();
  }

  uint32_t GetSyncKind(const StmtNode *s) const;
  void SetSyncKind(StmtNode *s, uint32_t n) const;
  void SetAllSyncKind(uint32_t n);
  void SyncOptimization();
  void AddFunc(uint32_t kind, const char *funcname);
  void CollectFuncs();
  int GetSyncFuncKind();
};

class MeDoSyncSelect : public MeFuncPhase {
 public:
  MeDoSyncSelect(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *frm, ModuleResultMgr *mrm) override;
  std::string PhaseName() const override {
    return "syncselect";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_SYNC_SELECT_H
