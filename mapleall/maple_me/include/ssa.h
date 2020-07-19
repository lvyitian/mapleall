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

#ifndef MAPLE_ME_INCLUDE_SSA_H
#define MAPLE_ME_INCLUDE_SSA_H
#include <iostream>
#include "mir_module.h"
#include "mir_nodes.h"

namespace maple {

class BB;
class VersionSt;
class OriginalStTable;
class VersionStTable;
class SSATab;
class Dominance;

class PhiNode {
 public:
  VersionSt *result;
  MapleVector<VersionSt *> phiOpnds;

  PhiNode(MapleAllocator *alloc, VersionSt *vsym) : result(vsym), phiOpnds(2, nullptr, alloc->Adapter()) {
    phiOpnds.pop_back();
    phiOpnds.pop_back();
  };
  ~PhiNode() {}

  void Dump(const MIRModule *mod);
};

class SSA {
 public:
  MapleAllocator ssaAlloc;
  MapleVector<MapleStack<VersionSt *> *> vstStacks;  // rename stack for variable versions
  MapleVector<bool> bbRenamed;                       //   indicate bb is renamed or not
  MapleVector<BB *> &bbVec;

  SSATab *ssaTab;
  Dominance *dom_;
  bool runRenameOnly = false;

  SSA(MemPool *mp, SSATab *stab, Dominance *dm, MapleVector<BB *> &bbvec)
    : ssaAlloc(mp),
      vstStacks(ssaAlloc.Adapter()),
      bbRenamed(ssaAlloc.Adapter()),
      bbVec(bbvec),
      ssaTab(stab),
      dom_(dm) {}

  virtual ~SSA() {}

  void InitRenameStack(OriginalStTable *, uint32, VersionStTable &);
  VersionSt *CreateNewVersion(VersionSt *vsym, BB *defBb);
  void RenamePhi(BB *bb);
  void RenameDefs(StmtNode *stmt, BB *defBb);
  void RenameMustDefs(const StmtNode *stmt, BB *defBb);
  void RenameExpr(BaseNode *expr);
  void RenameUses(StmtNode *stmt);
  void RenamePhiUseInSucc(BB *);
  void RenameMayUses(BaseNode *node);
  void RenameBB(BB *);
};
}  // namespace maple
#endif  // define MAPLE_ME_INCLUDE_SSA_H
