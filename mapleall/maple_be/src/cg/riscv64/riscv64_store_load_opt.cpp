/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
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

#include "riscv64_store_load_opt.h"
#include "riscv64_operand.h"
#include "riscv64_insn.h"
#include "riscv64_cg_func.h"
#include <iostream>

namespace maplebe {

using namespace maple;

static int gTestInfoStoreID = 0;
static int gTestInfoMovID = 0;

bool Riscv64StoreLoadOpt::CanReachEndBBFromBB(BB *bb, BB *endBB, set<BB *> &traversedBBSet) {
  if (bb == endBB) {
    return true;
  }

  for (auto succ : bb->succs) {
    if (traversedBBSet.find(succ) != traversedBBSet.end()) {
      continue;
    } else {
      traversedBBSet.insert(succ);
      if (succ == endBB) {
        return true;
      }
      if (CanReachEndBBFromBB(succ, endBB, traversedBBSet)) {
        return true;
      }
    }
  }

  for (auto ehsucc : bb->eh_succs) {
    if (traversedBBSet.find(ehsucc) != traversedBBSet.end()) {
      continue;
    } else {
      traversedBBSet.insert(ehsucc);
      if (ehsucc == endBB) {
        return true;
      }
      if (CanReachEndBBFromBB(ehsucc, endBB, traversedBBSet)) {
        return true;
      }
    }
  }

  return false;
}

bool Riscv64StoreLoadOpt::IsLiveInAllPathBB(BB *startBB, BB *endBB, MapleSet<BB *> &setBb,
                                            bool isParamPhyRegOpnd, std::set<BB *> &traversed) {
  set<BB *> traversedPathSet;

  for (auto succ : startBB->succs) {
    if (traversed.find(succ) != traversed.end()) {
      continue;
    }

    traversed.insert(succ);
    traversedPathSet.clear();
    if ((succ != endBB || (succ == endBB && endBB->loop)) &&
        ((isParamPhyRegOpnd && succ->HasCall()) || (setBb.find(succ) != setBb.end())) &&
        CanReachEndBBFromBB(succ, endBB, traversedPathSet)) {
      return false;
    }

    bool isLive = IsLiveInAllPathBB(succ, endBB, setBb, isParamPhyRegOpnd, traversed);
    if (!isLive) {
      return false;
    }
  }

  for (auto ehsucc : startBB->eh_succs) {
    if (traversed.find(ehsucc) != traversed.end()) {
      continue;
    }

    traversed.insert(ehsucc);
    traversedPathSet.clear();
    if ((ehsucc != endBB || (ehsucc == endBB && endBB->loop)) &&
        ((isParamPhyRegOpnd && ehsucc->HasCall()) || (setBb.find(ehsucc) != setBb.end())) &&
        CanReachEndBBFromBB(ehsucc, endBB, traversedPathSet)) {
      return false;
    }

    bool isLive = IsLiveInAllPathBB(ehsucc, endBB, setBb, isParamPhyRegOpnd, traversed);
    if (!isLive) {
      return false;
    }
  }

  return true;
}

/* Check if the definition operand of the defInsn is live from startInsn to endInsn point.
   Params:
     defInsn: The instruction of the operand defined.
     index: The operand of the ith destination operand of the defInsn.
     startInsn: The start point.
     endInsn: The end point.
 */
bool Riscv64StoreLoadOpt::IsLiveAtInsn(Insn *defInsn, int index, Insn *startInsn, Insn *endInsn) {
  if (defInsn->IsStore()) {
    // Memory live info should consider dirty property.
    CHECK_FATAL(false, "Currently no need to do this optimization.");
  } else {
    // Currently always return false.
    if (defInsn->redefine[index] == nullptr) {
      return true;
    }

    bool isParamPhyRegOpnd = false;
    if (defInsn->IsCall()) {
      // For return value R0.
      isParamPhyRegOpnd = true;
    } else if (defInsn->GetOperand(index)->IsRegister()) {
      RegOperand *regOpnd = static_cast<RegOperand *>(defInsn->GetOperand(index));
      regno_t regno = regOpnd->GetRegisterNumber();
      if ((regno >= R0 && regno <= R7) || (regno >= V0 && regno <= V7)) {
        isParamPhyRegOpnd = true;
      }
    }

    MapleSet<BB *> setBb(schdAlloc.Adapter());
    for (auto redef : *(defInsn->redefine[index])) {
      Insn *redefInsn = redef.GetInsn();
      if (startInsn->bb != endInsn->bb) {
        if (redefInsn->bb == endInsn->bb) {
          if (redefInsn->id < endInsn->id) {
            return false;
          }
        } else if (redefInsn->bb == startInsn->bb) {
          if (redefInsn->id > startInsn->id) {
            return false;
          }
        } else {
          setBb.insert(redefInsn->bb);
        }
      } else {
        CG_ASSERT(startInsn->id < endInsn->id, "Internal error. No possible.");

        if (redefInsn->bb == endInsn->bb) {
          // All in the same BB.
          if (redefInsn->id < endInsn->id && redefInsn->id > startInsn->id) {
            return false;
          }
        } else {
          // Do nothing.
        }
      }
    }
    // Check if there exist a redefinInsn in the BB that any paths from defInsn->bb to endInsn->bb may traverl through.
    if (startInsn->bb != endInsn->bb) {
      std::set<BB *> traversed;
      BB *startBB = startInsn->bb;
      traversed.insert(startBB);
      return IsLiveInAllPathBB(startBB, endInsn->bb, setBb, isParamPhyRegOpnd, traversed);
    }
  }

  return true;
}

void Riscv64StoreLoadOpt::InsertDUUD(const DataInsnInfo &defInsnInfo, const DataInsnInfo &useInsnInfo) {
  Insn *defInsn = defInsnInfo.GetInsn();
  short defUseIdx = defInsnInfo.GetDUIndex();
  Insn *useInsn = useInsnInfo.GetInsn();
  short useDefIdx = useInsnInfo.GetIndex();
  if (useDefIdx > 0 && (defInsnInfo.GetProperty() & DataInsnInfo::kCallParam)) {
    useDefIdx++;
  }

  if (defInsn->uses[defUseIdx] == nullptr) {
    defInsn->uses[defUseIdx] =
      rd->GetMemPool()->New<MapleSet<DataInsnInfo, DataInsnInfoCmp2>>(rd->GetMemAllocator().Adapter());
  }

  defInsn->uses[defUseIdx]->insert(useInsnInfo);

  if (useInsn->defs[useDefIdx] == nullptr) {
    useInsn->defs[useDefIdx] =
      rd->GetMemPool()->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rd->GetMemAllocator().Adapter());
  }

  useInsn->defs[useDefIdx]->insert(defInsnInfo);
}

void Riscv64StoreLoadOpt::RemovDUUD(Insn *defInsn, Insn *useInsn) {
  MapleSet<DataInsnInfo, DataInsnInfoCmp>::iterator itSet;
  MapleSet<DataInsnInfo, DataInsnInfoCmp2>::iterator itSet2;

  for (int i = 0; i < Insn::kMaxDefineOperandNum; i++) {
    if (defInsn->uses[i] == nullptr) {
      continue;
    }

    for (itSet2 = defInsn->uses[i]->begin(); itSet2 != defInsn->uses[i]->end();) {
      if ((*itSet2).GetInsn() == useInsn) {
        itSet2 = defInsn->uses[i]->erase(itSet2);
        if (defInsn->uses[i]->empty()) {
          break;
        }
        // Do not break here, since one insn may have multiple same use operand.
        /* Such as:
           x10 = ...
               = x10, x10
         */
      } else {
        itSet2++;
      }
    }
  }

  for (int i = 0; i < Insn::kMaxUseOperandNum; i++) {
    if (useInsn->defs[i] == nullptr) {
      continue;
    }

    for (itSet = useInsn->defs[i]->begin(); itSet != useInsn->defs[i]->end();) {
      if ((*itSet).GetInsn() == defInsn) {
        itSet = useInsn->defs[i]->erase(itSet);
        break;
      } else {
        itSet++;
      }
    }
  }
}

/* Transfer: store x100, [MEM]
             ... // May exist branches.
             load  x200, [MEM]
         ==>
         OPT_VERSION_STR_LIVE:
             store x100, [MEM]
             ... // May exist branches. if x100 not dead here.
             mov   x200, x100
         OPT_VERSION_STR_DIE:
             store x100, [MEM]
             mov x9000(new reg), x100
             ... // May exist branches. if x100 dead here.
             mov   x200, x9000
   Params:
     stInsn: indicate store insn.
     stIdx: index of source register operand of store insn. (x100 in this example)
     ldInsn: indicate load insn.
     ldIdx: index of source memory operand of the load insn.
     defInsnInfo: insnInfo for x100.
 */
void Riscv64StoreLoadOpt::DoLoadToMoveTransfer(Insn *stInsn, short stIdx, Insn *ldInsn, short ldIdx,
                                               enum OptVersion version, const DataInsnInfo &defInsnInfo) {
  Operand *resOpnd = ldInsn->GetResult(0);
  Operand *srcOpnd = stInsn->GetOperand(stIdx);

  CG_ASSERT(ldIdx == 1, "Currently only support ldr, not support ldp.");
  CG_ASSERT(resOpnd && srcOpnd, "null ptr check");
  CG_ASSERT(resOpnd->GetSize() == srcOpnd->GetSize(), "For stack location, the size of src and dst should be same.");
  CG_ASSERT(resOpnd->IsRegister() && srcOpnd->IsRegister(), "Operands of mov/fmov should be all register operands.");
  CG_ASSERT(version & (kOptVersionStpLive | kOptVersionStrLive | kOptVersionStpDie | kOptVersionStrDie),
            "version is not invalid.");

  RegOperand *resRegOpnd = static_cast<RegOperand *>(resOpnd);
  RegOperand *srcRegOpnd = static_cast<RegOperand *>(srcOpnd);

  if (resRegOpnd->GetRegisterType() != srcRegOpnd->GetRegisterType()) {
    return;
  }

  uint64 index = (int64)(stInsn);
  auto itMoveInsn = moveInsn->find(index);
  Insn *movInsn = nullptr;
  if (itMoveInsn != moveInsn->end()) {
    if (stIdx == 0) {
      movInsn = itMoveInsn->second.first;
    } else {
      movInsn = itMoveInsn->second.second;
    }
  }

  if (version & (kOptVersionStpLive | kOptVersionStrLive)) {
    if (movInsn) {
      RegOperand *vregOpnd = static_cast<RegOperand *>(movInsn->GetOperand(0));

      ldInsn->SetOperand(ldIdx, vregOpnd);
      if (resRegOpnd->IsOfFloatOrSIMDClass() && srcRegOpnd->IsOfFloatOrSIMDClass()) {
        // fmov
        ldInsn->SetMOP(resOpnd->GetSize() == 64 ? MOP_xvmovd : MOP_xvmovs);
      } else {
        // mov
        ldInsn->SetMOP(resOpnd->GetSize() == 64 ? MOP_xmovrr : MOP_wmovrr);
      }

      // Add comment.
      std::string newComment = ldInsn->GetComment();
      if (version & kOptVersionStpLive) {
        newComment += ";  stp-load live version.";
      } else {
        newComment += ";  str-load live version.";
      }

      ldInsn->AddComment(newComment);

      // Update DU-UD info.
      // 1: remove use of store insn. ([MEM])
      // 2: remove def of load insn. ([MEM])
      RemovDUUD(stInsn, ldInsn);
      // 3: insert use of mov x9000.
      // 4: insert def of the operand "x9000" of the mov insn.
      DataInsnInfo movDefInsnInfo(movInsn, 0, DataInsnInfo::kNormal);
      DataInsnInfo ldrUseInsnInfo(ldInsn, ldIdx, DataInsnInfo::kNormal);
      InsertDUUD(movDefInsnInfo, ldrUseInsnInfo);
    } else {
      ldInsn->SetOperand(ldIdx, srcOpnd);
      if (resRegOpnd->IsOfFloatOrSIMDClass() && srcRegOpnd->IsOfFloatOrSIMDClass()) {
        // fmov
        ldInsn->SetMOP(resOpnd->GetSize() == 64 ? MOP_xvmovd : MOP_xvmovs);
      } else {
        // mov
        ldInsn->SetMOP(resOpnd->GetSize() == 64 ? MOP_xmovrr : MOP_wmovrr);
      }

      // Add comment.
      std::string newComment = ldInsn->GetComment();
      if (version & kOptVersionStpLive) {
        newComment += ";  stp-load live version.";
      } else {
        newComment += ";  str-load live version.";
      }
      ldInsn->AddComment(newComment);

      // Update DU-UD info.
      // 1: remove use of store insn. ([MEM])
      // 2: remove def of load insn. ([MEM])
      RemovDUUD(stInsn, ldInsn);
      // 3: insert use of x100's definition insn.
      // 4: insert def of the operand "x100" of the mov insn.
      DataInsnInfo useInsnInfo(ldInsn, ldIdx, DataInsnInfo::kNormal);
      InsertDUUD(defInsnInfo, useInsnInfo);
    }
  } else if (version & (kOptVersionStpDie | kOptVersionStrDie)) {
    // To do.
    CG_ASSERT(stIdx <= 2, "CG internal error.");

    RegType regty = srcRegOpnd->IsOfFloatOrSIMDClass() ? kRegTyFloat : kRegTyInt;
    RegOperand *vregOpnd = nullptr;
    MOperator newMop;
    Insn *newMovInsn = nullptr;

    if (movInsn == nullptr) {
      regno_t vRegNo = cgfunc->New_V_Reg(regty, srcRegOpnd->GetSize() <= 32 ? 4 : 8);
      vregOpnd = cgfunc->CreateVirtualRegisterOperand(vRegNo);

      if (srcRegOpnd->IsOfFloatOrSIMDClass()) {
        // fmov
        newMop = resOpnd->GetSize() == 64 ? MOP_xvmovd : MOP_xvmovs;
      } else {
        newMop = resOpnd->GetSize() == 64 ? MOP_xmovrr : MOP_wmovrr;
      }

      newMovInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(newMop, vregOpnd, srcRegOpnd);
      if (stInsn->next && stInsn->next->id == stInsn->id + 1) {
        newMovInsn->id = stInsn->id + 1;
        stInsn->next->id = stInsn->id + 2;
      } else {
        newMovInsn->id = stInsn->id + 1;
      }
      stInsn->bb->InsertInsnAfter(stInsn, newMovInsn);

      if (stInsn->bb->loop) {
        // insert pre/redefine for newMovInsn
        newMovInsn->redefine[0] = cgfunc->GetRD()->GetMemPool()->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(
          cgfunc->GetRD()->GetMemAllocator().Adapter());
        newMovInsn->predefine[0] = cgfunc->GetRD()->GetMemPool()->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(
          cgfunc->GetRD()->GetMemAllocator().Adapter());
        newMovInsn->redefine[0]->insert(DataInsnInfo(newMovInsn, 0, DataInsnInfo::kNormal));
        newMovInsn->predefine[0]->insert(DataInsnInfo(newMovInsn, 0, DataInsnInfo::kNormal));
      }

      if (itMoveInsn != moveInsn->end()) {
        if (stIdx == 0) {
          itMoveInsn->second.first = newMovInsn;
        } else {
          itMoveInsn->second.second = newMovInsn;
        }
      } else {
        uint64 index = (uint64)(stInsn);
        if (stIdx == 0) {
          (*moveInsn)[index] = make_pair(newMovInsn, nullptr);
//pair<Insn*, Insn*>(newMovInsn, nullptr);
        } else {
          (*moveInsn)[index] = make_pair(nullptr, newMovInsn);
//pair<Insn*, Insn*>(nullptr, newMovInsn);
        }
      }
    } else {
      newMovInsn = movInsn;

      CG_ASSERT((newMovInsn->GetMachineOpcode() == MOP_xvmovd || newMovInsn->GetMachineOpcode() == MOP_xvmovs ||
                 newMovInsn->GetMachineOpcode() == MOP_xmovrr || newMovInsn->GetMachineOpcode() == MOP_wmovrr),
                "CG internal error.");

      vregOpnd = static_cast<RegOperand *>(newMovInsn->GetOperand(0));
    }

    ldInsn->SetOperand(ldIdx, vregOpnd);
    if (resRegOpnd->IsOfFloatOrSIMDClass() && srcRegOpnd->IsOfFloatOrSIMDClass()) {
      // fmov
      ldInsn->SetMOP(resOpnd->GetSize() == 64 ? MOP_xvmovd : MOP_xvmovs);
    } else {
      // mov
      ldInsn->SetMOP(resOpnd->GetSize() == 64 ? MOP_xmovrr : MOP_wmovrr);
    }

    // Add comment.
    std::string newComment = ldInsn->GetComment();
    if (version & kOptVersionStpDie) {
      newComment += ";  stp-load die version.";
    } else {
      newComment += ";  str-load die version.";
    }
    ldInsn->AddComment(newComment);

    // Update DU-UD info.
    // 1: remove use of store insn. ([MEM])
    // 2: remove def of load insn. ([MEM])
    RemovDUUD(stInsn, ldInsn);
    // 3: insert use of x100's definition insn.
    // 4: insert def of the operand "x100" of the load insn.
    DataInsnInfo movUseInsnInfo(newMovInsn, 1, DataInsnInfo::kNormal);
    InsertDUUD(defInsnInfo, movUseInsnInfo);

    DataInsnInfo movDefInsnInfo(newMovInsn, 0, DataInsnInfo::kNormal);
    DataInsnInfo ldrUseInsnInfo(ldInsn, ldIdx, DataInsnInfo::kNormal);
    InsertDUUD(movDefInsnInfo, ldrUseInsnInfo);
  } else {
    CG_ASSERT(false, "No other optimization version.");
  }

  if (cgfunc->cg->GenerateVerboseAsm() && cgfunc->cg->GenerateTestInfo()) {
    // For test framework.
    Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);

    if (version & (kOptVersionStpLive | kOptVersionStpDie)) {
      std::string comment = "stp_id: ";
      comment += std::to_string(gTestInfoStoreID);
      const char *str1 = comment.c_str();
      Insn *commentInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_comment, a64cgfunc->CreateCommentOperand(str1));
      stInsn->bb->InsertInsnAfter(stInsn, commentInsn);

      comment = "mov_pid: ";
      comment += std::to_string(gTestInfoStoreID);
      comment += "_";
      comment += std::to_string(stIdx);
      comment += "-";
      comment += std::to_string(gTestInfoMovID++);
      const char *str2 = comment.c_str();
      commentInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_comment, a64cgfunc->CreateCommentOperand(str2));
      ldInsn->bb->InsertInsnAfter(ldInsn, commentInsn);
    } else {
      std::string comment = "str_id: ";
      comment += std::to_string(gTestInfoStoreID);
      const char *str1 = comment.c_str();
      Insn *commentInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_comment, a64cgfunc->CreateCommentOperand(str1));
      stInsn->bb->InsertInsnAfter(stInsn, commentInsn);

      comment = "mov_id: ";
      comment += std::to_string(gTestInfoStoreID);
      comment += "-";
      comment += std::to_string(gTestInfoMovID++);
      const char *str2 = comment.c_str();
      commentInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_comment, a64cgfunc->CreateCommentOperand(str2));

      ldInsn->bb->InsertInsnAfter(ldInsn, commentInsn);
    }
  }
}

/* Transfer: store wzr, [MEM]
             ... // May exist branches.
             load  x200, [MEM]
         ==>
         OPT_VERSION_STP_ZERO / OPT_VERSION_STR_ZERO:
             store wzr, [MEM]
             ... // May exist branches. if x100 not dead here.
             mov   x200, wzr

   Params:
     stInsn: indicate store insn.
     stIdx: index of source register operand of store insn. (wzr in this example)
     ldInsn: indicate load insn.
     ldIdx: index of source memory operand of the load insn.
 */
void Riscv64StoreLoadOpt::DoLoadZeroToMoveTransfer(Insn *stInsn, short stIdx, Insn *ldInsn, short ldIdx,
                                                   enum OptVersion version) {
  CG_ASSERT(stInsn && ldInsn, "null ptr check");
  Operand *resOpnd = ldInsn->GetResult(0);
  RegOperand *resRegOpnd = static_cast<RegOperand *>(resOpnd);
  Operand *srcOpnd = stInsn->GetOperand(stIdx);

  CG_ASSERT(resOpnd && srcOpnd, "null ptr check");
  CG_ASSERT(resOpnd->GetSize() == srcOpnd->GetSize(), "For stack location, the size of src and dst should be same.");
  CG_ASSERT(version & (kOptVersionStpZero | kOptVersionStrZero), "version is invalid.");

  ldInsn->SetOperand(ldIdx, srcOpnd);
  if (resRegOpnd->IsOfFloatOrSIMDClass()) {
    // fmov
    ldInsn->SetMOP(resOpnd->GetSize() == 64 ? MOP_xvmovd : MOP_xvmovs);
  } else {
    ldInsn->SetMOP(resOpnd->GetSize() == 64 ? MOP_xmovrr : MOP_wmovrr);
  }

  // Update DU-UD info.
  // 1: remove use of store insn. ([MEM])
  // 2: remove def of load insn. ([MEM])
  RemovDUUD(stInsn, ldInsn);

  // Add comment.
  std::string newComment = ldInsn->GetComment();
  if (version & kOptVersionStpZero) {
    newComment += ";  stp-load zero version.";
  } else {
    newComment += ";  str-load zero version.";
  }
  ldInsn->AddComment(newComment);

  if (cgfunc->cg->GenerateVerboseAsm() && cgfunc->cg->GenerateTestInfo()) {
    // For test framework.
    Riscv64CGFunc *a64cgfunc = static_cast<Riscv64CGFunc *>(cgfunc);

    if (version == kOptVersionStpZero) {
      std::string comment = "stp_id: ";
      comment += std::to_string(gTestInfoStoreID);

      const char *str1 = comment.c_str();
      Insn *commentInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_comment, a64cgfunc->CreateCommentOperand(str1));
      stInsn->bb->InsertInsnAfter(stInsn, commentInsn);

      comment = "mov_pid: ";
      comment += std::to_string(gTestInfoStoreID);
      comment += "_";
      comment += std::to_string(stIdx);
      comment += "-";
      comment += std::to_string(gTestInfoMovID++);

      const char *str2 = comment.c_str();
      commentInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_comment, a64cgfunc->CreateCommentOperand(str2));
      ldInsn->bb->InsertInsnAfter(ldInsn, commentInsn);
    } else {
      std::string comment = "str_id: ";
      comment += std::to_string(gTestInfoStoreID);

      const char *str1 = comment.c_str();
      Insn *commentInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_comment, a64cgfunc->CreateCommentOperand(str1));
      stInsn->bb->InsertInsnAfter(stInsn, commentInsn);

      comment = "mov_id: ";
      comment += std::to_string(gTestInfoStoreID);
      comment += "-";
      comment += std::to_string(gTestInfoMovID++);

      const char *str2 = comment.c_str();
      commentInsn = cgfunc->cg->BuildInstruction<Riscv64Insn>(MOP_comment, a64cgfunc->CreateCommentOperand(str2));
      ldInsn->bb->InsertInsnAfter(ldInsn, commentInsn);
    }
  }
}

/* Optimize: store x100, [MEM]
             ... // May exist branches.
             load  x200, [MEM]
         ==>
         OPT_VERSION_STP_LIVE / OPT_VERSION_STR_LIVE:
             store x100, [MEM]
             ... // May exist branches. if x100 not dead here.
             mov   x200, x100
         OPT_VERSION_STP_DIE / OPT_VERSION_STR_DIE:
             store x100, [MEM]
             mov x9000(new reg), x100
             ... // May exist branches. if x100 dead here.
             mov   x200, x9000

   Note: x100 may be wzr/xzr registers.
 */
void Riscv64StoreLoadOpt::DoStoreLoadOpt() {
  MapleSet<DataInsnInfo, DataInsnInfoCmp2>::iterator itSet2;
  MapleSet<DataInsnInfo, DataInsnInfoCmp2>::iterator itSetNext2;

  FOR_ALL_BB(bb, cgfunc) {
    FOR_BB_INSNS(insn, bb) {
      if (!insn->IsMachineInstruction()) {
        continue;
      }

      if (insn->IsStore()) {
        gTestInfoStoreID++;
        gTestInfoMovID = 0;
        if (insn->uses[0]) {
          CG_ASSERT(insn->GetOpnd(0) != nullptr, "Internal error.");

          RegOperand *regOpnd = static_cast<RegOperand *>(insn->GetOpnd(0));
          if (regOpnd && regOpnd->IsZeroRegister()) {
            MapleSet<DataInsnInfo, DataInsnInfoCmp2> &setUses = *(insn->uses[0]);
            for (itSet2 = setUses.begin(); itSet2 != setUses.end(); itSet2 = itSetNext2) {
              itSetNext2 = itSet2;
              itSetNext2++;

              const DataInsnInfo &ldrInfo = *itSet2;
              if (ldrInfo.IsDirty()) {
                continue;
              }

              Insn *ldrInsn = ldrInfo.GetInsn();

              // Currently we don't support useInsn is ldp insn.
              if (ldrInsn->GetResultNum() > 1) {
                continue;
              }

              // If load has multiple definition, continue.
              CG_ASSERT(ldrInsn->defs[ldrInfo.GetIndex()], "load insn should have definitions.");
              CG_ASSERT(ldrInfo.GetIndex() == 1, "memory address should be 1.");
              if (ldrInsn->defs[1]->size() > 1 || ldrInsn->bb->is_cleanup) {
                continue;
              }

              // If load has multigen property, continue.
              if (ldrInsn->defs[1]->begin()->IsMulGen()) {
                continue;
              }

              if (SCHDDUMP) {
                LogInfo::MapleLogger() << "Do store-load optimization, str -- wzr/xzr version: ";
                LogInfo::MapleLogger() << cgfunc->GetName() << std::endl;
                LogInfo::MapleLogger() << "Store insn: ";
                insn->dump();
                LogInfo::MapleLogger() << "Load insn: ";
                ldrInsn->dump();
              }
              DoLoadZeroToMoveTransfer(insn, 0, ldrInsn, ldrInfo.GetIndex(), kOptVersionStrZero);
            }

            continue;
          }

          if (insn->defs[0] == nullptr) {
            LogInfo::MapleLogger() << "Warning: use without def." << std::endl;
            continue;
          }

          // Find x100's definition insn.
          if (insn->defs[0]->size() != 1 || insn->bb->is_cleanup) {
            continue;
          }

          DataInsnInfo insnInfo = *(insn->defs[0]->begin());
          Insn *defInsn = insnInfo.GetInsn();
          int index = insnInfo.GetProperty() & DataInsnInfo::kIndexQuickcalc;

          MapleSet<DataInsnInfo, DataInsnInfoCmp2> &setUses = *(insn->uses[0]);
          for (itSet2 = setUses.begin(); itSet2 != setUses.end(); itSet2 = itSetNext2) {
            itSetNext2 = itSet2;
            itSetNext2++;

            const DataInsnInfo &ldrInfo = *itSet2;
            if (ldrInfo.IsDirty()) {
              continue;
            }

            Insn *ldrInsn = ldrInfo.GetInsn();

            // Currently we don't support useInsn is ldp insn.
            if (ldrInsn->GetResultNum() > 1) {
              continue;
            }

            // If load has multiple definition, continue.
            CG_ASSERT(ldrInsn->defs[ldrInfo.GetIndex()], "load insn should have definitions.");
            CG_ASSERT(ldrInfo.GetIndex() == 1, "memory address should be 1.");
            if (ldrInsn->defs[1]->size() > 1 || ldrInsn->bb->is_cleanup) {
              continue;
            }

            // If load has multigen property, continue.
            if (ldrInsn->defs[1]->begin()->IsMulGen()) {
              continue;
            }

            // Check if use operand of store is live at load insn.
            if (IsLiveAtInsn(defInsn, index, insn, ldrInsn)) {
              // Do the optimization.
              /* Optimize: store x100, [MEM]
                           ... // May exist branches.
                           load  x200, [MEM]
                       ==>
                           store x100, [MEM]
                           ... // May exist branches
                           mov   x200, x100
               */
              if (SCHDDUMP) {
                LogInfo::MapleLogger() << "Do store-load optimization 1: str version";
                LogInfo::MapleLogger() << cgfunc->GetName() << std::endl;
                LogInfo::MapleLogger() << "Store insn: ";
                insn->dump();
                LogInfo::MapleLogger() << "Load insn: ";
                ldrInsn->dump();
              }
              DoLoadToMoveTransfer(insn, 0, ldrInsn, ldrInfo.GetIndex(), kOptVersionStrLive, insnInfo);
              continue;
            } else {
              // Do the optimization.
              /* Optimize: store x100, [MEM]
                         ... // May exist branches. x100 dead here.
                           load  x200, [MEM]
                       ==>
                           store x100, [MEM]
                           mov x9000(new reg), x100
                           ... // May exist branches. x100 dead here.
                           mov   x200, x9000
             */
              if (SCHDDUMP) {
                LogInfo::MapleLogger() << "Do store-load optimization 2: str version";
                LogInfo::MapleLogger() << cgfunc->GetName() << std::endl;
                LogInfo::MapleLogger() << "Store insn: ";
                insn->dump();
                LogInfo::MapleLogger() << "Load insn: ";
                ldrInsn->dump();
              }

              DoLoadToMoveTransfer(insn, 0, ldrInsn, ldrInfo.GetIndex(), kOptVersionStrDie, insnInfo);
              continue;
            }
          }

          if (setUses.empty()) {
            insn->uses[0] = nullptr;
            break;
          }
        }
      }
    }
  }
}

/* Optimize: store x100, [MEM]
             ... // May exist branches.
             load  x200, [MEM]
         ==>
         OPT_VERSION_STP_LIVE / OPT_VERSION_STR_LIVE:
             store x100, [MEM]
             ... // May exist branches. if x100 not dead here.
             mov   x200, x100
         OPT_VERSION_STP_DIE / OPT_VERSION_STR_DIE:
             store x100, [MEM]
             mov x9000(new reg), x100
             ... // May exist branches. if x100 dead here.
             mov   x200, x9000

   Note: x100 may be wzr/xzr registers.
 */

void Riscv64StoreLoadOpt::run() {
  // Give up this optimization if a non-escaped object has been allocated on stack,
  // because the DU information collected at this CGIR level may not be accurate.
  if (cgfunc->HasNonescapedVar()) {
    return;
  }

  MemPool *phaseMp = mempoolctrler.NewMemPool("CG StoreLoadOpt pool");
  MapleAllocator phaseAllocator(phaseMp);
  MapleUnorderedMap<uint64, pair<Insn*, Insn*>> tmpInsns(phaseAllocator.Adapter());
  moveInsn = &tmpInsns;

  FindBackEdge();
  DoStoreLoadOpt();

  if (SCHDDUMP) {
    LogInfo::MapleLogger() << "$$$$$$$$$$$$$$$$$    End store-load optimization.    $$$$$$$$$$$$$$$$$$$$" << std::endl;

    FOR_ALL_BB(bb, cgfunc) {
      rd->DumpUDChain(bb);
      rd->DumpDUChain(bb);
      rd->DumpWAWDependence(bb);
    }
  }

  mempoolctrler.DeleteMemPool(phaseMp);
}

}  // namespace maplebe
