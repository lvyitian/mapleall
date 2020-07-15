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

#include "bb.h"
#include "me_ssa.h"
#include "ssa_mir_nodes.h"
#include "ver_symbol.h"

namespace maple {

void VersionSt::DumpDefStmt(const MIRModule *mod) {
  if (version <= 0) {
    return;
  }
  switch (defType) {
    case kDassign:
      defStmt.dassign->Dump(mod, 0);
      return;
    case kPhi:
      defStmt.phi->Dump(mod);
      return;
    case kMayDef:
      defStmt.mayDef->Dump(mod);
      return;
    case kMustDef:
      defStmt.mustDef->Dump(mod);
      return;
    default:
      CHECK_FATAL(false, "NYI");
  }
}

VersionSt *VersionStTable::CreateNextVersionSt(OriginalSt *ost) {
  CHECK_FATAL(ost->versionsIndex.size() != 0, "CreateNextVersionSt: zero version must have been created first");
  VersionSt *vst = vstAlloc.GetMemPool()->New<VersionSt>(versionStVector.size(), ost->versionsIndex.size(), ost);
  versionStVector.push_back(vst);
  ost->versionsIndex.push_back(vst->index);
  vst->ost = ost;
  return vst;
}

void VersionStTable::CreateZeroVersionSt(OriginalSt *ost) {
  if (ost->zeroVersionIndex != 0) {
    return;  // already created
  }
  CHECK_FATAL(ost->versionsIndex.size() == 0, "ssa version need to be created incrementally!");
  VersionSt *vst = vstAlloc.GetMemPool()->New<VersionSt>(versionStVector.size(), INIT_VERSION, ost);
  versionStVector.push_back(vst);
  ost->versionsIndex.push_back(vst->index);
  ost->zeroVersionIndex = vst->index;
  vst->ost = ost;
  return;
}

void VersionStTable::Dump(MIRModule *mod) {
  LogInfo::MapleLogger() << "=======version st table entries=======\n";
  for (uint32 i = 1; i < versionStVector.size(); i++) {
    VersionSt *vst = versionStVector[i];
    vst->Dump(mod);
    if (vst->version > 0) {
      LogInfo::MapleLogger() << " defined BB" << vst->GetDefBB()->id.idx << ": ";
      vst->DumpDefStmt(mod);
    } else {
      LogInfo::MapleLogger() << std::endl;
    }
  }
  LogInfo::MapleLogger() << "=======end version st table===========\n";
}

}  // namespace maple
