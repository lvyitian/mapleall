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

#ifndef MAPLEBE_INCLUDE_CG_SCHEDULING_H
#define MAPLEBE_INCLUDE_CG_SCHEDULING_H

#include "cg_func.h"
#include "cg_phase.h"
#include "cg_option.h"

namespace maplebe {

#ifndef DEBUG_SCHD
//#define DEBUG_SCHD
#endif  // DEBUG_SCHD

#define SCHDDUMP CGDEBUGFUNC(cgfunc)

class ReachingDefinition;

class StoreLoadOpt {
 public:
  enum LiveStatus {
    kFail = 0,
    kSucc,
    kWhite,
    GRAY,
    kBlack,
  };

  enum OptVersion {
    kOptVersionStpLive = 1,
    kOptVersionStpDie = 2,
    kOptVersionStpZero = 4,
    kOptVersionStrLive = 8,
    kOptVersionStrDie = 16,
    kOptVersionStrZero = 32,
  };

 public:
  CGFunc *cgfunc;
  MemPool *schdmp;
  MapleAllocator schdAlloc;
  ReachingDefinition *rd;
  MapleVector<BB *> dfs_bb;

 public:
  StoreLoadOpt(CGFunc *func, MemPool *mp, ReachingDefinition *rd)
    : cgfunc(func), schdmp(mp), schdAlloc(mp), rd(rd), dfs_bb(schdAlloc.Adapter()) {}

  virtual ~StoreLoadOpt() {}

  virtual void run() {}

  virtual void DoStoreLoadOpt() {}

  virtual bool IsLiveAtInsn(Insn *defInsn, int index, Insn *startInsn, Insn *useInsn) {
    return false;
  }

  virtual bool CanReachEndBBFromBB(BB *bb, BB *endBB, set<BB *> &traversedBBSet) {
    return false;
  }

  virtual bool IsLiveInAllPathBB(BB *startBB, BB *endBB, MapleSet<BB *> &setBb, bool isParamPhyRegOpnd,
                                 std::set<BB *> &traversed) {
    return false;
  }

  virtual void RemovDUUD(Insn *defInsn, Insn *useInsn) {}

  virtual void InsertDUUD(const DataInsnInfo &defInsnInfo, const DataInsnInfo &useInsnInfo) {}

  virtual void DoLoadToMoveTransfer(Insn *stInsn, short stIdx, Insn *ldInsn, short ldIdx, enum OptVersion version,
                                    const DataInsnInfo &defInsnInfo) {}

  virtual void DoLoadZeroToMoveTransfer(Insn *stInsn, short stIdx, Insn *ldInsn, short ldIdx, enum OptVersion version) {
  }

 protected:
  void DFS(BB *startBB);
  void FindBackEdge();
  const char *PhaseName() const {
    return "storeloadopt";
  }
};

CGFUNCPHASE_CANSKIP(CgDoStoreLoadOpt, "storeloadopt")

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_SCHEDULING_H
