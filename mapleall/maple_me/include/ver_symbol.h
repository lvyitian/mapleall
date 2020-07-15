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

#ifndef MAPLE_ME_INCLUDE_VER_SYMBOL_H
#define MAPLE_ME_INCLUDE_VER_SYMBOL_H
#include "mir_module.h"
#include "mir_symbol.h"
#include "orig_symbol.h"
#include <iostream>

// This file defines the data structure VersionSt that represents an SSA version

struct BBId;

namespace maple {
class BB;
class PhiNode;
class MayDefNode;
class MustDefNode;
class VersionStTable;
class OriginalSt;

class VersionSt {
 public:
  uint32 index;     // index number in versionStTable;
  int version;      // starts from 0 for each symbol
  OriginalSt *ost;  // the index of related originalst in originalst_table;

  BB *defBB;
  enum DefType { kUnknown, kDassign, kRegassign, kPhi, kMayDef, kMustDef } defType;
  union DefStmt {
    maple::DassignNode *dassign;
    maple::RegassignNode *regassign;
    maple::PhiNode *phi;
    MayDefNode *mayDef;
    MustDefNode *mustDef;
  } defStmt;    // only valid after SSA
  bool live;      // helper of dse
  bool isReturn;  // the symbol will return in its function

  VersionSt(uint32 index, uint32 version, OriginalSt *ost)
    : index(index),
      version(version),
      ost(ost),
      defBB(nullptr),
      defType(kUnknown),
      defStmt(),
      live(false),
      isReturn(false) {}

  ~VersionSt() {}

  void SetDefBB(BB *defbb) {
    defBB = defbb;
  };
  BB *GetDefBB() {
    return defBB;
  };
  void DumpDefStmt(const MIRModule *mod);
  bool IsLive() const {
    return live;
  }

  bool IsInitVersion() const {
    return version == INIT_VERSION;
  }

  void MarkLive() {
    live = true;
  }

  void MarkDead() {
    live = false;
  }

  OStIdx GetOrigIdx() {
    return ost->index;
  }

  OriginalSt *GetOrigSt() const {
    return ost;
  }

  void Dump(const MIRModule *mod, bool omitname = false) {
    if (!omitname) {
      ost->Dump();
    }
    LogInfo::MapleLogger() << "(" << version << ")";
  };
};

class VersionStTable {
 public:
  MapleAllocator vstAlloc;                    // this stores versionStVector
  MapleVector<VersionSt *> versionStVector;  // the vector that map a versionst's index to its pointer

  VersionStTable(MemPool *vstMp) : vstAlloc(vstMp), versionStVector(vstAlloc.Adapter()) {
    versionStVector.push_back(static_cast<VersionSt*>(nullptr));
  }

  ~VersionStTable() {}

  VersionSt *CreateNextVersionSt(OriginalSt *ost);

  void CreateZeroVersionSt(OriginalSt *ost);

  VersionSt *GetZeroVersionSt(OriginalSt *ost) {
    CHECK_FATAL(ost->versionsIndex.size() != 0, "GetZeroVersionSt:: zero version has not been created");
    return versionStVector[ost->zeroVersionIndex];
  }

  uint32 Size() const {
    return versionStVector.size();
  }

  void Dump(MIRModule *mod);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_VER_SYMBOL_H
