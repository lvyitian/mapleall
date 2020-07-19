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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64SCHEDULING_H
#define MAPLEBE_INCLUDE_CG_AARCH64SCHEDULING_H

#include "store_load_opt.h"

namespace maplebe {

using namespace maple;
using namespace std;

class AArch64StoreLoadOpt : public StoreLoadOpt {
 private:
 public:
  MapleUnorderedMap<uint64, pair<Insn*, Insn*>> *moveInsn;

  explicit AArch64StoreLoadOpt(CGFunc *func, MemPool *mp, ReachingDefinition *rd)
    : StoreLoadOpt(func, mp, rd),
      moveInsn(nullptr) {}

  ~AArch64StoreLoadOpt() {}

  void run() override;
  void DoStoreLoadOpt() override;
  bool IsLiveAtInsn(Insn *defInsn, int index, Insn *startInsn, Insn *useInsn) override;
  bool CanReachEndBBFromBB(BB *bb, BB *endBB, set<BB *> &traversedBBSet) override;
  bool IsLiveInAllPathBB(BB *startBB, BB *endBB, MapleSet<BB *> &setBb, bool isParamPhyRegOpnd,
                         std::set<BB *> &traversed) override;
  void RemovDUUD(Insn *defInsn, Insn *useInsn) override;
  void InsertDUUD(const DataInsnInfo &defInsnInfo, const DataInsnInfo &useInsnInfo) override;

  void DoLoadToMoveTransfer(Insn *stInsn, short stIdx, Insn *ldInsn, short ldIdx, enum OptVersion version,
                            const DataInsnInfo &defInsnInfo) override;

  void DoLoadZeroToMoveTransfer(Insn *stInsn, short stIdx, Insn *ldInsn, short ldIdx, enum OptVersion version) override;

 private:
};
}  // namespace maplebe
#endif  // MAPLEBE_INCLUDE_CG_AARCH64SCHEDULING_H
