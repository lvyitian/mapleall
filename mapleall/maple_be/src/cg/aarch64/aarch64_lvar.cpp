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

#include "aarch64_lvar.h"
#include "aarch64_insn.h"
#include "aarch64_operand.h"
#include "aarch64_mem_layout.h"
#include "cg_assert.h"
#include "special_func.h"

#undef DEBUG
#define DEBUG 0

namespace maplebe {

void AArch64OptLocalRef::DoOptLocalRef() {
  if (CfgHasCycle() == true) {
    // Do not handle loops yet.
    return;
  }
  OptLRInit();

  if (giveup == true) {
    goto optlrfini;
  }

  DoOptimize();

  if (giveup == true) {
    goto optlrfini;
  }

  FinalizeLocRef();
  FixMrtCall();

optlrfini:
  OptLRFini();
}

void AArch64OptLocalRef::PrintBbTraceList(string str, bool more) {
  LogInfo::MapleLogger() << "===" << str << "===\n";
  if (more == false) {
    CHECK_FATAL(bb_trace.size(), "container size check");
    BB *top = bb_trace.front();
    BB *cbb = top;
    do {
      LogInfo::MapleLogger() << cbb->id << " ";
      bb_trace.pop_front();
      bb_trace.push_back(cbb);
      cbb = bb_trace.front();
    } while (cbb != top);
    LogInfo::MapleLogger() << "\n";
    return;
  }

  MapleList<BB *>::iterator it;
  for (it = bb_trace.begin(); it != bb_trace.end(); it++) {
    BB *bb = *it;
    Iinfo *info = nullptr;
    for (info = insn_info_in_bb[bb->id]; info; info = info->next) {
      LogInfo::MapleLogger() << "bb " << info->insn->bb->id << " imm " << info->imm << " map " << info->assigned << "\n";
    }
  }
}

/* The current algorithm does not handle cycles.
 * All traces has single path from entry to exit.
 */
bool AArch64OptLocalRef::DetectCycle() {
  BB *bb = nullptr;
  bool childPushed;
  while (dfs_bbs.empty() == false) {
    childPushed = false;
    bb = dfs_bbs.top();
    dfs_bbs.pop();
    CHECK_FATAL(bb != nullptr, "bb is null in AArch64OptLocalRef::DFS");
    visited_bbs[bb->id] = true;
    if (bb->level == 0) {
      bb->level = 1;
    }
    std::stack<BB *> succs;
    // Mimic more of the recursive DFS by reversing the order of the succs.
    for (MapleList<BB *>::iterator it = bb->succs.begin(); it != bb->succs.end(); ++it) {
      BB *ibb = *it;
      succs.push(ibb);
    }
    while (succs.empty() == false) {
      BB *ibb = succs.top();
      succs.pop();
      if (visited_bbs[ibb->id] == false) {
        childPushed = true;
        ibb->level = bb->level + 1;
        sorted_bbs[ibb->id] = bb;  // tracking parent of traversed child
        dfs_bbs.push(ibb);
      } else if ((ibb->level != 0) && (bb->level >= ibb->level)) {
        // Backedge
        return true;
      }
    }
    for (MapleList<BB *>::iterator it = bb->eh_succs.begin(); it != bb->eh_succs.end(); ++it) {
      BB *ibb = *it;
      succs.push(ibb);
    }
    while (succs.empty() == false) {
      BB *ibb = succs.top();
      succs.pop();
      if (visited_bbs[ibb->id] == false) {
        childPushed = true;
        ibb->level = bb->level + 1;
        sorted_bbs[ibb->id] = bb;  // tracking parent of traversed child
        dfs_bbs.push(ibb);
      } else if ((ibb->level != 0) && (bb->level >= ibb->level)) {
        // Backedge
        return true;
      }
    }
    // Remove duplicate bb that are visited from top of stack
    if (dfs_bbs.empty() == false) {
      BB *nbb = dfs_bbs.top();
      while (nbb) {
        if (visited_bbs[nbb->id] == true) {
          dfs_bbs.pop();
        } else {
          break;
        }
        if (dfs_bbs.empty() == false) {
          nbb = dfs_bbs.top();
        } else {
          break;
        }
      }
    }
    if (childPushed == false && dfs_bbs.empty() == false) {
      // reached the bottom of visited chain, reset level to the next visit bb
      bb->level = 0;
      BB *nbb = dfs_bbs.top();
      if (sorted_bbs[nbb->id]) {
        nbb = sorted_bbs[nbb->id];
        // All bb up to the top of stack bb's parent's child (its sibling)
        while (bb) {
          // get parent bb
          BB *pbb = sorted_bbs[bb->id];
          if (pbb == nullptr || pbb == nbb) {
            break;
          }
          pbb->level = 0;
          bb = pbb;
        }
      }
    }
  }
  return false;
}

bool AArch64OptLocalRef::CfgHasCycle() {
  visited_bbs.resize(cgfunc->NumBBs());
  for (uint32 i = 0; i < cgfunc->NumBBs(); i++) {
    visited_bbs[i] = false;
    sorted_bbs.push_back(nullptr);
  }
  FOR_ALL_BB(bb, cgfunc) {
    bb->level = 0;
  }
  bool changed;
  do {
    changed = false;
    FOR_ALL_BB(bb, cgfunc) {
      if (visited_bbs[bb->id] == false) {
        dfs_bbs.push(bb);
        if (DetectCycle() == true) {
          return true;
        }
        changed = true;
      }
    }
  } while (changed == true);
  return false;
}

bool AArch64OptLocalRef::IsLdStLocalRefVar(Operand *src) {
  AArch64MemOperand *memopnd = static_cast<AArch64MemOperand *>(src);
  if (memopnd->GetAddrMode() != AArch64MemOperand::kAddrModeBOi) {
    return false;
  }
  Operand *base = memopnd->GetBaseRegister();
  if (base && static_cast<RegOperand *>(base)->GetRegisterNumber() != RFP) {
    return false;
  }
  AArch64OfstOperand *imm = memopnd->GetOffsetImmediate();
  if (imm == nullptr) {
    return false;
  }
  int32 val = imm->GetOffsetValue();
  if (val >= min_imm && val <= max_imm) {
    return true;
  }
  return false;
}

/* Given a call insn, look backward to find the immediate value that
 * is assigned and modify it to the new value.
 * This is used to change the slot numbers for MRT (init cleanup) calls.
 */
void AArch64OptLocalRef::ModifyParamValue(Insn *insn, uint32 reg, uint32 val) {
  // Assuming at this point the parameters are generated in order and in bb.
  // Therefore x1, x2 will always not be the first insn in bb.
  for (Insn *prev = insn->prev; prev; prev = prev->prev) {
    Operand *dst = prev->GetOperand(0);
    RegOperand *regopnd = static_cast<RegOperand *>(dst);
    if (regopnd->GetRegisterNumber() == reg) {
      Operand *src = nullptr;
      if (prev->GetOpndNum() == 1) {
        // move insn
        src = prev->GetOpnd(0);
      } else {
        // add insn
        src = prev->GetOpnd(1);
      }
      CHECK_FATAL(src, "null ptr check");
      if (src->IsRegister()) {
        RegOperand *regsrc = static_cast<RegOperand *>(src);
        return ModifyParamValue(prev, regsrc->GetRegisterNumber(), val);
      } else {
        CG_ASSERT(src->IsIntImmediate(), "ModifyParamValue not imm const");
        ImmOperand *imm = static_cast<ImmOperand *>(src);
        imm->SetValue(val);
        return;
      }
    }
  }
  CG_ASSERT(0, "ModifyParamValue got no imm const");
}

/* Given a call insn, look backward to find the immediate value that
 * is assigned and then returns that immediate value.
 */
uint32 AArch64OptLocalRef::GetParamValue(Insn *insn, uint32 reg) {
  // Assuming at this point the parameters are generated in order and in bb.
  // Therefore x1, x2 will always not be the first insn in bb.
  for (Insn *prev = insn->prev; prev; prev = prev->prev) {
    Operand *dst = prev->GetOperand(0);
    RegOperand *regopnd = static_cast<RegOperand *>(dst);
    if (regopnd->GetRegisterNumber() == reg) {
      Operand *src = nullptr;
      if (prev->GetOpndNum() == 1) {
        // move insn
        src = prev->GetOpnd(0);
      } else {
        // add insn
        src = prev->GetOpnd(1);
      }
      CHECK_FATAL(src, "null ptr check");
      if (src->IsRegister()) {
        RegOperand *regsrc = static_cast<RegOperand *>(src);
        return GetParamValue(prev, regsrc->GetRegisterNumber());
      } else {
        CG_ASSERT(src->IsIntImmediate(), "GetParamValue not imm const");
        ImmOperand *imm = static_cast<ImmOperand *>(src);
        // LogInfo::MapleLogger() << "Return " << imm->GetValue() << "\n";
        return static_cast<uint32>(imm->GetValue());
      }
    }
  }
  CG_ASSERT(0, "GetParamValue got no imm const");
  return 0;
}

/* At the end of the function, there might be a cleanup bbs
 * which is jumped to when encountering exception.  The cleanup
 * bbs can be reached from anywhere, so treat it special.
 */
void AArch64OptLocalRef::HandleCleanupBB(BB *bb) {
  // Cleanup bbs should be at the end of the function.
  // Nothing is afterward.
  // Since it can be reached from anywhere, make sure the
  // local ref stack slot is not modified.
  for (; bb; bb = bb->next) {
    FOR_BB_INSNS(insn, bb) {
      if (IsRcStackCall(insn)) {
        continue;
      }
      if (insn->IsLoad() == false) {
        continue;
      }
      Operand *src = nullptr;
      bool isLdPair = false;
      if (IsLdStPair(insn)) {
        src = insn->GetOperand(2);
        isLdPair = true;
      } else {
        src = insn->GetOperand(1);
        isLdPair = false;
      }
      int32 val;
      if (CanGetMemOpndImm(src, val)) {
        xzr_init_local_offset.insert(val);
        if (isLdPair) {
          xzr_init_local_offset.insert(val + 8);
        }
#if (DEBUG > 1)
        LogInfo::MapleLogger() << "HandleCleanupBB - insert cleanup xzr " << val << "\n";
#endif
      }
    }
  }
}

/* Look for MRT (init, cleanup, cleanupskip) calls.
 */
bool AArch64OptLocalRef::IsRcStackCall(Insn *insn) {
  if (insn->IsCall()) {
    Operand *opnd = insn->GetOperand(0);
    FuncNameOperand *callName = dynamic_cast<FuncNameOperand *>(opnd);
    if (callName) {
      string name = callName->GetName();
      if (name == GetIntrinsicFuncName(INTRN_MCCInitializeLocalStackRef)) {
        CG_ASSERT(rc_stack_init_start_loc == 0, "IsRcStackCall-Init loc set already");
        rc_stack_init_start_loc = GetParamValue(insn, R0);
#if (DEBUG > 0)
        LogInfo::MapleLogger() << "\tset rc_stack_init_start_loc to " << rc_stack_init_start_loc << "\n";
#endif
        rc_init_call.push(insn);
        CG_ASSERT(rc_localrefvar_slots == 0, "IsRcStackCall-Init loc set already");
        rc_localrefvar_slots = GetParamValue(insn, R1);
#if (DEBUG > 0)
        LogInfo::MapleLogger() << "\tset rc_localrefvar_slots to " << rc_localrefvar_slots << "\n";
#endif

        min_imm = rc_stack_init_start_loc;
        max_imm = rc_stack_init_start_loc + (rc_localrefvar_slots - 1) * 8;
        has_rc_init = true;
        return true;
      } else if (name == GetIntrinsicFuncName(INTRN_MCCCleanupLocalStackRefNaiveRCFast)) {
        if (has_rc_init == false) {
          giveup = true;
          return true;
        }
        rc_cleanup_call.push(insn);
        int skipSlot = static_cast<AArch64cleancallInsn *>(insn)->ref_skipindex;
        if (skipSlot != -1) {
          giveup = true;
          uint32 val = static_cast<uint32>(skipSlot);
          rc_cleanup_skip_num.insert(val);
#if (DEBUG > 0)
          LogInfo::MapleLogger() << "\tinsert MrtCleanup skip num " << val << "\n";
#endif
        }
        has_rc_cleanup = true;
        return true;
      } else if (name == GetIntrinsicFuncName(INTRN_MCCCleanupLocalStackRefSkipNaiveRCFast)) {
        if (has_rc_init == false) {
          giveup = true;
          return true;
        }
        // x1: # slots, x2: skip slot
        rc_skip_call.push(insn);
        uint32 val = GetParamValue(insn, R1);
        if (rc_localrefvar_slots != val) {
          giveup = true;
          return true;
        }
        int skipSlot = static_cast<AArch64cleancallInsn *>(insn)->ref_skipindex;
        if (skipSlot != -1) {
          giveup = true;
          val = static_cast<uint32>(skipSlot);
          rc_cleanup_skip_num.insert(val);
#if (DEBUG > 0)
          LogInfo::MapleLogger() << "\tinsert MrtCleanupSkip skip num " << val << "\n";
#endif
        }
        has_rc_cleanup = true;
        return true;
      } else if (name == GetIntrinsicFuncName(INTRN_MCCDecRefReset)) {
        giveup = true;
        return true;
      }
    }
  }
  return false;
}

/* load/store pair insn can be tricky to handle.  Remember them.
 */
void AArch64OptLocalRef::RecordLdStPair(Insn *insn) {
  Operand *src = insn->GetOperand(2);
  MemOperand *memopnd = static_cast<MemOperand *>(src);
  Operand *base = memopnd->GetBaseRegister();
  if (base->IsRegister()) {
    RegOperand *regOpnd = static_cast<RegOperand *>(base);
    if (regOpnd->GetRegisterNumber() != R29) {
      return;
    }
  }
  AArch64MemOperand *mem = static_cast<AArch64MemOperand *>(src);
  AArch64OfstOperand *imm = mem->GetOffsetImmediate();
  if (imm == nullptr) {
    return;
  }
  int32 val = imm->GetOffsetValue();
  CG_ASSERT((val & 0x7) == 0, "Ld/St pair imm offset not 8 aligned");
  if (static_cast<RegOperand *>(insn->GetOperand(0))->IsZeroRegister() &&
      static_cast<RegOperand *>(insn->GetOperand(1))->IsZeroRegister()) {
    xzr_init_local_offset.insert(val);
    xzr_init_local_offset.insert(val + 8);
#if (DEBUG > 2)
    LogInfo::MapleLogger() << "RecordLdStPair - insert pair xzr " << val << " + 8\n";
#endif
  } else {
    ld_st_pair_offset.insert(val);
    ld_st_pair_offset.insert(val + 8);
#if (DEBUG > 2)
    LogInfo::MapleLogger() << "RecordLdStPair - insert pair ldst " << val << " + 8\n";
#endif
  }
}

/* Localrefvar can be initialized by explicit store of 0 (XZR) instead
 * of a call to MRT_Init.
 * Although it is not handled now, remember them.
 */
bool AArch64OptLocalRef::RecordXzrInit(Insn *insn) {
  if (static_cast<RegOperand *>(insn->GetOperand(0))->IsZeroRegister()) {
    Operand *src1 = insn->GetOperand(1);
    MemOperand *memopnd = static_cast<MemOperand *>(src1);
    Operand *base = memopnd->GetBaseRegister();
    if (base->IsRegister()) {
      RegOperand *regOpnd = static_cast<RegOperand *>(base);
      if (regOpnd->GetRegisterNumber() != R29) {
        return false;
      }
    }
    AArch64MemOperand *mem = static_cast<AArch64MemOperand *>(src1);
    AArch64OfstOperand *imm = mem->GetOffsetImmediate();
    if (imm != nullptr) {
      int32 val = imm->GetOffsetValue();
      CG_ASSERT((val & 0x7) == 0, "Ld/St pair imm offset not 8 aligned");
#if (DEBUG > 2)
      LogInfo::MapleLogger() << "RecordXzrInit - insert xzr " << val << "\n";
#endif
      xzr_init_local_offset.insert(val);
      return true;
    }
  }
  return false;
}

bool AArch64OptLocalRef::IsLdStPair(Insn *insn) {
  switch (insn->mop_) {
    case MOP_wldp:
    case MOP_wldaxp:
    case MOP_wstp:
    case MOP_wstlxp:
      CG_ASSERT(0, "AArch64OptLocalRef IsLdStPair not handle 32 bit");
    case MOP_xldp:
    case MOP_xldpsw:
    case MOP_sldp:
    case MOP_dldp:
    case MOP_xldaxp:
    case MOP_xstp:
    case MOP_sstp:
    case MOP_dstp:
    case MOP_xstlxp:
      return true;
  }
  return false;
}

/* Make sure mem operand is af a certain format.
 */
bool AArch64OptLocalRef::CanGetMemOpndImm(Operand *src, int32 &val) {
  CG_ASSERT(src->IsMemoryAccessOperand(), "CanGetMemOpndImm - not mem operand");
  MemOperand *memopnd = static_cast<MemOperand *>(src);
  Operand *base = memopnd->GetBaseRegister();
  if (base == nullptr) {
    return false;
  }
  if (base->IsRegister()) {
    RegOperand *regOpnd = static_cast<RegOperand *>(base);
    if (regOpnd->GetRegisterNumber() != R29) {
      return false;
    }
  }
  AArch64MemOperand *mem = static_cast<AArch64MemOperand *>(src);
  AArch64OfstOperand *imm = mem->GetOffsetImmediate();
  if (imm == nullptr) {
    return false;
  }
  val = imm->GetOffsetValue();
  return true;
}

bool AArch64OptLocalRef::ImmOffUsedInXzrInit(int32 imm) {
  MapleSet<int32>::iterator it;
  for (it = xzr_init_local_offset.begin(); it != xzr_init_local_offset.end(); it++) {
    int32 val = static_cast<int32>(*it);
    if (imm == val) {
      return true;
    }
  }
  return false;
}

bool AArch64OptLocalRef::ImmOffUsedInLdStPair(int32 imm) {
  MapleSet<int32>::iterator it;
  for (it = ld_st_pair_offset.begin(); it != ld_st_pair_offset.end(); it++) {
    int32 val = static_cast<int32>(*it);
    if (imm == val) {
      return true;
    }
  }
  return false;
}

/* To simplify opt, remove from consideration localrefvar fitting certain
 * condition and do not modify these localrefvar.
 */
void AArch64OptLocalRef::RemoveImmUsedInSkip() {
  /* It is possible for MRT to skip initialization and cleanup.
   * 1. Skip first location in cleanup.
   * 2. Skip last location in cleanup.
   * 3. If in middle, use the cleanupskip interface.
   * The skipped locations are not relocated at this point.
   */
  for (uint32 i = 0; i < cgfunc->NumBBs(); i++) {
    Iinfo *insninfo = insn_info_in_bb[i];
    Iinfo *previnfo = nullptr;
    while (insninfo != nullptr) {
#if (DEBUG > 3)
      LogInfo::MapleLogger() << "bb(" << insninfo->insn->bb->id << ") imm(" << insninfo->imm << ")\n";
#endif
      int32 imm = insninfo->imm;
      bool found = false;
      MapleSet<int32>::iterator it;
      for (it = rc_cleanup_skip_num.begin(); it != rc_cleanup_skip_num.end(); it++) {
        int32 val = static_cast<int32>(*it);
        uint32 offset = min_imm + (val << 3);
#if (DEBUG > 3)
        LogInfo::MapleLogger() << "\ttest rc_cleanup_skip " << offset << "\n";
#endif
        if (imm == offset) {
#if (DEBUG > 3)
          LogInfo::MapleLogger() << "\tfound\n";
#endif
          found = true;
          break;
        }
      }
      if (found == false) {
        // it is not skipped
#if (DEBUG > 3)
        LogInfo::MapleLogger() << "keep cleanup\n";
#endif
        previnfo = insninfo;
        insninfo = insninfo->next;
      } else if (previnfo == nullptr) {
        // remove element
#if (DEBUG > 3)
        LogInfo::MapleLogger() << "skip cleanup\n";
#endif
        insninfo = insninfo->next;
        insn_info_in_bb[i] = insninfo;
      } else {
        // remove element
#if (DEBUG > 3)
        LogInfo::MapleLogger() << "skip cleanup\n";
#endif
        insninfo = insninfo->next;
        previnfo->next = insninfo;
      }
    }
  }
}

/* To simplify opt, remove from consideration localrefvar fitting certain
 * condition and do not modify these localrefvar.
 */
void AArch64OptLocalRef::RemoveImmUsedInLdSt() {
  for (uint32 i = 0; i < cgfunc->NumBBs(); i++) {
    Iinfo *insninfo = insn_info_in_bb[i];
    Iinfo *previnfo = nullptr;
    while (insninfo != nullptr) {
#if (DEBUG > 3)
      LogInfo::MapleLogger() << "bb(" << insninfo->insn->bb->id << ") imm(" << insninfo->imm << ")\n";
#endif
      int32 imm;
      imm = insninfo->imm;
      bool removed = false;
      if (ImmOffUsedInLdStPair(imm) == false) {
        // it is not used in ld/st pair
#if (DEBUG > 3)
        LogInfo::MapleLogger() << "\tkeep pair\n";
#endif
      } else if (previnfo == nullptr) {
        // remove element
#if (DEBUG > 3)
        LogInfo::MapleLogger() << "\tskip pair\n";
#endif
        insninfo = insninfo->next;
        insn_info_in_bb[i] = insninfo;
        removed = true;
      } else {
        // remove element
#if (DEBUG > 3)
        LogInfo::MapleLogger() << "\tskip pair\n";
#endif
        insninfo = insninfo->next;
        previnfo->next = insninfo;
        removed = true;
      }

      if (removed == true) {
        continue;
      }

      if (ImmOffUsedInXzrInit(imm) == false) {
#if (DEBUG > 3)
        LogInfo::MapleLogger() << "\tkeep xzr\n";
#endif
      } else if (previnfo == nullptr) {
        // remove element
#if (DEBUG > 3)
        LogInfo::MapleLogger() << "\tskip xzr\n";
#endif
        insninfo = insninfo->next;
        insn_info_in_bb[i] = insninfo;
        removed = true;
      } else {
        // remove element
#if (DEBUG > 3)
        LogInfo::MapleLogger() << "\tskip xzr\n";
#endif
        insninfo = insninfo->next;
        previnfo->next = insninfo;
        removed = true;
      }

      if (removed == true) {
        continue;
      }

      previnfo = insninfo;
      insninfo = insninfo->next;
      continue;
    }
  }
}

/* Find all localrefvar and intialize a list of themfor each bb.
 * Also try to remove localrefvar which should not be touched.
 */
void AArch64OptLocalRef::OptLRInit() {
  /* Discover potential local var stack locations.
   * If a memory instruction is a local var but is not marked as auto,
   * there are two cases.
   * 1) There is no other mem-op with the same location marked as auto.
   * In this case do not optimize, which is ok.
   * 2) Another mem-op with the same location is marked as auto.
   * Need to associate this instruction and location to local var.
   *
   * There are ldp/stp operate over consecutive stack locations.
   * Need to bind these locations together, or do not optimize.
   *
   * Initially will only optimize stack locations for single dest.
   */
  insn_info_in_bb.resize(cgfunc->NumBBs());
  for (uint32 i = 0; i < cgfunc->NumBBs(); i++) {
    insn_info_in_bb[i] = nullptr;
  }
  uint32 icnt = 0;
  FOR_ALL_BB(bb, cgfunc) {
    if (bb->firststmt == cgfunc->cleanup_label) {
#if (DEBUG > 3)
      LogInfo::MapleLogger() << "OptLRInit - cleanup bb " << bb->id << "\n";
#endif
      HandleCleanupBB(bb);
      // HandleCleanupBB guarantees we have reached the end of function
      break;
    }
    // LogInfo::MapleLogger() << "bb " << bb->id << "\n";
    FOR_BB_INSNS(insn, bb) {
      // insn->dump();
      if (giveup) {
#if (DEBUG > 0)
        LogInfo::MapleLogger() << "OptLRInit - give up in insn traversal\n";
#endif
        return;
      }
      if (insn->mop_ == MOP_comment) {
        Operand *src = insn->GetOperand(0);
        string str = static_cast<CommentOperand *>(src)->GetComment();
        if (str == "MPL_CLEANUP_LOCALREFVARS") {
          for (Insn *ninsn = insn->next; ninsn; ninsn = ninsn->next) {
            if (!ninsn->IsCall()) {
              continue;
            }
            FuncNameOperand *target = dynamic_cast<FuncNameOperand *>(ninsn->GetCallTargetOperand());
            if (target) {
              MIRSymbol *funcst = target->GetFunctionSymbol();
              CG_ASSERT(funcst->sKind == kStFunc, "");
              if (funcst->GetName() == GetIntrinsicFuncName(INTRN_MCCIncRef)) {
                giveup = true;
                return;
              }
            }
          }
        }
      }
      if (IsRcStackCall(insn)) {
        continue;
      }
      if (insn->IsLoad() == false && insn->IsStore() == false) {
        continue;
      }
      if (IsLdStPair(insn) == true) {
        RecordLdStPair(insn);
        continue;
      }
      if (RecordXzrInit(insn) == true) {
        continue;
      }
      Operand *src = insn->GetOperand(1);
      if (IsLdStLocalRefVar(src) == false) {
        continue;
      }
      // Found target mem-op
      int32 imm;
      if (CanGetMemOpndImm(src, imm) == false) {
        continue;
      }
      if (imm < min_imm || imm > max_imm) {
        continue;
      }
      if (ImmOffUsedInXzrInit(imm) == true) {
        continue;
      }
      if (ImmOffUsedInLdStPair(imm) == true) {
        continue;
      }

      insn->id = ++icnt;  // Mark this insn as recorded.
      Iinfo *insninfo = OptLocalRef::refmp->New<Iinfo>(insn, imm);
      Iinfo *tmp = insn_info_in_bb[insn->bb->id];
      insn_info_in_bb[insn->bb->id] = insninfo;
      insninfo->next = tmp;
    }
  }

  if ((has_rc_init == false) || (has_rc_cleanup == false)) {
#if (DEBUG > 3)
    LogInfo::MapleLogger() << "OptLRInit - give up after analysis\n";
#endif
    giveup = true;
    return;
  }

  /* There are ld/st which are not marked as local var.
   * Need to add them also.
   */
  FOR_ALL_BB(bb, cgfunc) {
    FOR_BB_INSNS(insn, bb) {
      if (insn->id != 0) {
        continue;  // Already recorded.
      }
      if (insn->IsLoad() == false && insn->IsStore() == false) {
        continue;
      }
      if (IsLdStPair(insn) == true) {
        continue;
      }
      Operand *src = insn->GetOperand(1);
      int32 imm;
      if (CanGetMemOpndImm(src, imm) == false) {
        continue;
      }
      if (imm < min_imm) {
        continue;
      }
      if (imm >= max_imm) {
        other_opnd.insert(static_cast<AArch64MemOperand *>(src));
        continue;
      }
      if (ImmOffUsedInXzrInit(imm) == true) {
        continue;
      }
      if (ImmOffUsedInLdStPair(imm) == true) {
        continue;
      }

      MapleSet<int32>::iterator it;
      for (it = rc_cleanup_skip_num.begin(); it != rc_cleanup_skip_num.end(); it++) {
        int32 val = static_cast<int32>(*it);
        uint32 offset = min_imm + (val << 3);
        if (imm == offset) {
#if (DEBUG > 2)
          LogInfo::MapleLogger() << "OptLRInit - CleanupStackSkip skipping " << imm << "\n";
#endif
          continue;
        }
      }

      insn->id = ++icnt;  // Mark this insn as recorded.
      Iinfo *insninfo = OptLocalRef::refmp->New<Iinfo>(insn, imm);
      Iinfo *tmp = insn_info_in_bb[insn->bb->id];
      insn_info_in_bb[insn->bb->id] = insninfo;
      insninfo->next = tmp;
    }
  }

  // Remember the src operands for later renaming.
  imm_operands.resize(max_imm - min_imm + 1);
  for (uint32 i = 0; i < cgfunc->NumBBs(); i++) {
    Iinfo *insninfo = insn_info_in_bb[i];
    if (insninfo == nullptr) {
      continue;
    }
    for (; insninfo; insninfo = insninfo->next) {
      int32 imm = insninfo->imm;
      uint32 pos = (imm - min_imm) >> 3;
#if (DEBUG > 3)
      LogInfo::MapleLogger() << "OptLRInit - bb " << insninfo->insn->bb->id << " imm[" << pos << "]\n";
      insninfo->insn->GetOperand(1)->dump();
      LogInfo::MapleLogger() << "\n";
#endif
      imm_operands[pos] = insninfo->insn->GetOperand(1);
    }
  }
  for (int i = 0; i < ((max_imm - min_imm) >> 3) + 1; i++) {
    if (imm_operands[i]) {
#if (DEBUG > 0)
      LogInfo::MapleLogger() << "i=" << i << " ";
      imm_operands[i]->dump();
      LogInfo::MapleLogger() << "\n";
#endif
    } else {
#if (DEBUG > 0)
      LogInfo::MapleLogger() << "i=" << i << "\n";
#endif
      giveup = true;
      return;
    }
  }

  if (ld_st_pair_offset.empty() == false || xzr_init_local_offset.empty() == false) {
    // Might have missed some ld/st pairs or xzr init
    RemoveImmUsedInLdSt();
  }
  // RC stack cleanup can skip, must retain the original mapping.
  RemoveImmUsedInSkip();
}

/* Clear field for later phases.
 */
void AArch64OptLocalRef::OptLRFini() {
  FOR_ALL_BB(bb, cgfunc) {
    FOR_BB_INSNS(insn, bb) {
      insn->id = 0;
    }
  }
}

/* There are situation where allocation of slots can conflict between
 * traces.  Attempt to rename the conflict to a higher unused slot.
 */
bool AArch64OptLocalRef::CanRenameConflictSlot(int32 imm) {
  uint32 offset = max_assigned;
  while (offset < max_imm) {
    offset = offset + 8;
    if (ImmOffUsedInXzrInit(offset) || ImmOffUsedInLdStPair(offset)) {
      continue;
    }
    bool matched = false;
    MapleSet<int32>::iterator sit;
    for (sit = rc_cleanup_skip_num.begin(); sit != rc_cleanup_skip_num.end(); sit++) {
      int32 val = static_cast<int32>(*sit);
      if (offset == (min_imm + (val << 3))) {
        matched = true;
        break;
      }
    }
    if (matched) {
      continue;
    }
  }

  if (offset > max_imm) {
    return false;
  }
  max_assigned = offset;

  FOR_ALL_BB(bb, cgfunc) {
    for (Iinfo *info = insn_info_in_bb[bb->id]; info; info = info->next) {
      if (info->imm == imm) {
        info->assigned = max_assigned;
      }
    }
  }
  return true;
}

/*
 * Perform mapping of discovered local ref's immediate offset
 * to new offset.
 * Does not do the insn modification.  Its done later.
 */
void AArch64OptLocalRef::AssignLocalRefForTrace() {
  vector<int32> immMap;
  vector<bool> seen;
  vector<bool> assigned;
  forward_list<uint32> allocSlot;

  uint32 slots = ((max_imm - min_imm) >> 3) + 1;
  immMap.resize(slots);
  seen.resize(slots);
  assigned.resize(slots);
  for (int32 i = (slots - 1); i >= 0; i--) {
    immMap[i] = 0;
    seen[i] = false;
    assigned[i] = false;
    allocSlot.push_front(i);
  }

  /* Get the already mapped offsets.
   * Remove them from allocation slot.
   */
  MapleList<BB *>::iterator it;
  for (it = bb_trace.begin(); it != bb_trace.end(); it++) {
    BB *bb = *it;
    // LogInfo::MapleLogger() << "\tAssign - bb " << bb->id << "\n";
    if (visited_bbs[bb->id] == false) {
      continue;
    }
    Iinfo *info = nullptr;
    // LogInfo::MapleLogger() << "bb " << bb->id << "\n";
    for (info = insn_info_in_bb[bb->id]; info; info = info->next) {
      int32 imm = info->imm;
      uint32 pos = (imm - min_imm) >> 3;
      // LogInfo::MapleLogger() << "\tpos " << pos << " of imm " << imm << " assign " << info->assigned << "\n";
      if ((seen[pos] == false && assigned[(info->assigned - min_imm) >> 3]) ||
          (seen[pos] && immMap[pos] != info->assigned)) {
        // Mapping conflict, abort
        if (CanRenameConflictSlot(imm) == false) {
          giveup = true;
          return;
        }
        // LogInfo::MapleLogger() << "\tconflict after assign " << info->assigned << "\n";
      }
      immMap[pos] = info->assigned;
      seen[pos] = true;

      pos = (info->assigned - min_imm) >> 3;
      assigned[pos] = true;
      allocSlot.remove(pos);
    }
  }

  /* Some slots should not be changed.
   * Remove them from allocation slot.
   */
  for (int32 i = 0; i < slots; i++) {
    uint32 offset = min_imm + (i >> 3);
    if (ImmOffUsedInXzrInit(offset) || ImmOffUsedInLdStPair(offset)) {
      immMap[i] = offset;
      allocSlot.remove((offset - min_imm) >> 3);
    }
  }
  MapleSet<int32>::iterator sit;
  for (sit = rc_cleanup_skip_num.begin(); sit != rc_cleanup_skip_num.end(); sit++) {
    int32 val = static_cast<int32>(*sit);
    uint32 offset = min_imm + (val << 3);
    immMap[val] = offset;
    allocSlot.remove(val);
  }
  // New assignments
  for (it = bb_trace.begin(); it != bb_trace.end(); it++) {
    uint32 nextAllocSlot;
    BB *bb = *it;
    if (visited_bbs[bb->id]) {
      continue;
    }
#if (DEBUG > 0)
    LogInfo::MapleLogger() << "AssignLocalRefForTrace - bb " << bb->id << "\n";
#endif
    visited_bbs[bb->id] = true;
    for (Iinfo *info = insn_info_in_bb[bb->id]; info; info = info->next) {
      int32 imm = info->imm;
      uint32 pos = (imm - min_imm) >> 3;
      if (immMap[pos] == 0) {
        // Not assigned from previous traces.  Assign new.
        if (allocSlot.empty() == true) {
          // Ran out of stack slots.
          giveup = true;
          return;
        }
        nextAllocSlot = allocSlot.front();
        allocSlot.pop_front();
        int32 assign = (nextAllocSlot << 3) + min_imm;
        immMap[pos] = info->assigned = assign;
        if (assign > max_assigned) {
          max_assigned = assign;
        }
#if (DEBUG > 0)
        LogInfo::MapleLogger() << "empty pos " << pos << " of imm " << imm << " assgined " << info->assigned << "\n";
#endif
      } else {
        info->assigned = immMap[pos];
#if (DEBUG > 0)
        LogInfo::MapleLogger() << "mapped pos " << pos << " of imm " << imm << " assigned " << info->assigned << "\n";
#endif
      }
    }
  }
}

/* Generating a trace.
 */
void AArch64OptLocalRef::DFS() {
  // There should be no cycle at this point.
  BB *bb = nullptr;
  bool childPushed;
  while (dfs_bbs.empty() == false) {
    bb = dfs_bbs.top();
    dfs_bbs.pop();
    bb_trace.push_front(bb);

#if (DEBUG > 5)
    LogInfo::MapleLogger() << "\tvisit bb " << bb->id << " sz " << bb_trace.size() << "\n";
#endif
    childPushed = false;
    MapleList<BB *>::iterator it;
    for (it = bb->succs.begin(); it != bb->succs.end(); ++it) {
      BB *ibb = *it;
      dfs_bbs.push(ibb);
      childPushed = true;
    }
    for (it = bb->eh_succs.begin(); it != bb->eh_succs.end(); ++it) {
      BB *ibb = *it;
      dfs_bbs.push(ibb);
      childPushed = true;
    }
    if ((bb->succs.size() + bb->eh_succs.size()) > 1) {
      // On a cfg fork, need to walk back bb_trace list.
      bb_walkback.push(bb);
    }
    if (childPushed == false) {
      num_traces++;
      if ((num_traces > kMaxNumTraces) || (bb_trace.size() > kMaxTraceLen)) {
#if (DEBUG > 0)
        LogInfo::MapleLogger() << "DoOptimize - give up with too many traces\n";
        LogInfo::MapleLogger() << "num_traces " << num_traces << " MAX " << kMaxNumTraces << "\n";
        LogInfo::MapleLogger() << "trace size " << bb_trace.size() << " MAX " << kMaxTraceLen << "\n";
#endif
        giveup = true;
        return;  // Give up on large traces
      }
#if (DEBUG > 0)
      PrintBbTraceList("TRACE");
#endif
      AssignLocalRefForTrace();
      if (giveup) {
#if (DEBUG > 0)
        LogInfo::MapleLogger() << "DoOptimize - give up with mapping conflicts\n";
#endif
        return;
      }
      // Walk back bb_trace list.
      bool found = false;
      do {
        if (bb_walkback.empty()) {
          break;
        }
        BB *lastForkBB = bb_walkback.top();
        if (bb_trace.front() != lastForkBB) {
          do {
            bb_trace.pop_front();
          } while (bb_trace.front() != lastForkBB);
#if (DEBUG > 4)
          PrintBbTraceList("POP");
#endif
        }
        /* When all child of the last_fork_bb has been visited
         * (ie. every child is a part of a bb_trace)
         * remove last_fork_bb from bb_walkback.
         */
        if (dfs_bbs.empty() == false) {
          // The next bb to be visited
          // See if it is a child of last_fork_bb.
          BB *dfsTopBb = dfs_bbs.top();
          for (it = lastForkBB->succs.begin(); it != lastForkBB->succs.end(); ++it) {
            BB *ibb = *it;
            if (ibb == dfsTopBb) {
              found = true;
              break;
            }
          }
          if (found == false) {
            for (it = lastForkBB->eh_succs.begin(); it != lastForkBB->eh_succs.end(); ++it) {
              BB *ibb = *it;
              if (ibb == dfsTopBb) {
                found = true;
                break;
              }
            }
          }
        }
        if (found == false) {
          // All successors visited, walkback.
#if (DEBUG > 4)
          LogInfo::MapleLogger() << "\tpop walkback << " << bb_walkback.top()->id << "\n";
#endif
          bb_walkback.pop();
        }
      } while (found == false);
    }
  }
}

void AArch64OptLocalRef::DoOptimize() {
  visited_bbs.resize(cgfunc->NumBBs());
  for (uint32 i = 0; i < cgfunc->NumBBs(); i++) {
    visited_bbs[i] = false;
  }
  while (dfs_bbs.empty() == false) {
    dfs_bbs.pop();
  }

#if (DEBUG > 0)
  LogInfo::MapleLogger() << "min " << min_imm << " max " << max_imm << "\n";
#endif
  dfs_bbs.push(cgfunc->firstbb);
  DFS();

  if (max_assigned >= max_imm) {
#if (DEBUG > 0)
    LogInfo::MapleLogger() << "DoOptimize - No benefit, giveup\n";
#endif
    giveup = true;
  }
}

/* Rename the slots for localrefvar.
 */
void AArch64OptLocalRef::FinalizeLocRef() {
  // Modify the actual insn.
#if (DEBUG > 0)
  LogInfo::MapleLogger() << "FinalizeLocRef " << cgfunc->GetName() << "\n";
#endif
  FOR_ALL_BB(bb, cgfunc) {
    if (visited_bbs[bb->id] == false) {
      continue;  // unreachable
    }
#if (DEBUG > 0)
    LogInfo::MapleLogger() << "bb " << bb->id << "\n";
#endif
    for (Iinfo *info = insn_info_in_bb[bb->id]; info; info = info->next) {
      Insn *insn = info->insn;
#if (DEBUG > 0)
      insn->dump();
      LogInfo::MapleLogger() << "===== assign =====>" << info->assigned << "\n";
#endif
      Operand *src = imm_operands[(info->assigned - min_imm) >> 3];
      CG_ASSERT(src, "FinalizeLocRef - no operand");
      insn->SetOperand(1, src);
    }
  }

  /* There are some memory operand offsets beyond the local vars that has already
   * been computed based on the old local var slot numbers.  Correct them.
   */
  int32 diff = rc_stack_init_start_loc + rc_localrefvar_slots * 8 - (max_assigned + 8);
  for (auto it = other_opnd.begin(); it != other_opnd.end(); it++) {
    AArch64MemOperand *src = *it;
    AArch64OfstOperand *ofstopnd = src->GetOffsetImmediate();
    int32 offset = ofstopnd->GetOffsetValue();
    if (offset <= max_assigned) {
      continue;
    }
    AArch64OfstOperand *offsetOpnd = cgfunc->memPool->New<AArch64OfstOperand>(offset - diff, 64);
    src->SetOffsetImmediate(offsetOpnd);
  }
}

/* Patch the MRT calls.
 */
void AArch64OptLocalRef::FixMrtCall() {
  MapleStack<Insn *>::iterator it;
  uint32 val = ((max_assigned - min_imm) >> 3) + 1;
  for (it = rc_init_call.begin(); it != rc_init_call.end(); ++it) {
    Insn *call = *it;
    ModifyParamValue(call, R1, val);
  }
  for (it = rc_cleanup_call.begin(); it != rc_cleanup_call.end(); ++it) {
    Insn *call = *it;
    ModifyParamValue(call, R0, rc_stack_init_start_loc);
    ModifyParamValue(call, R1, val);
  }
  for (it = rc_skip_call.begin(); it != rc_skip_call.end(); ++it) {
    Insn *call = *it;
    ModifyParamValue(call, R1, val);
  }
  // Update the new size of the local var on the stack.
  static_cast<AArch64MemLayout *>(cgfunc->memlayout)->Reflocals().size = val * SIZEOFPTR;
}

}  // namespace maplebe
