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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64LVAR_H
#define MAPLEBE_INCLUDE_CG_AARCH64LVAR_H

#include "cg_func.h"
#include "lvar.h"
#include "riscv64_operand.h"
#include <iostream>

namespace maplebe {

using namespace std;

class Iinfo;

class Riscv64OptLocalRef : public OptLocalRef {
  static const int32 kMaxTraceLen = 100;
  static const int32 kMaxNumTraces = 2000;

  MapleAllocator olr_allocator;
  uint32 min_imm;       // smallest local ref imm pre-opt
  uint32 max_imm;       // largest local ref imm pre-opt
  uint32 max_assigned;  // post-opt largest imm
  uint32 num_traces;
  uint32 rc_stack_init_start_loc;       // mrt-init call R0
  uint32 rc_localrefvar_slots;          // mrt-init call R1
  MapleSet<int32> rc_cleanup_skip_num;  // mrt-cleanupskip R2
  MapleVector<Operand *> imm_operands;  // mem operands used by local ref
  MapleVector<bool> visited_bbs;
  MapleVector<BB *> sorted_bbs;
  MapleStack<BB *> dfs_bbs;
  MapleStack<BB *> bb_walkback;
  MapleList<BB *> bb_trace;               // trace used for opt
  MapleStack<Insn *> rc_init_call;        // mrt-init
  MapleStack<Insn *> rc_cleanup_call;     // mrt-cleanup
  MapleStack<Insn *> rc_skip_call;        // mrt-cleanupskip
  MapleVector<Iinfo *> insn_info_in_bb;   // list of localref insn for bb
  MapleSet<int32> xzr_init_local_offset;  // offset of 0 init for localref
  MapleSet<int32> ld_st_pair_offset;
  MapleSet<Riscv64MemOperand *> other_opnd;  // other memory opnd requires patching

  bool has_rc_init;
  bool has_rc_cleanup;
  bool giveup;  // for some reason, do not opt

 private:
  void PrintBbTraceList(string str, bool more = false);

  bool DetectCycle();
  bool CfgHasCycle();
  bool IsLdStLocalRefVar(Operand *opnd);
  void ModifyParamValue(Insn *insn, uint32 reg, uint32 val);
  uint32 GetParamValue(Insn *insn, uint32 reg);
  void HandleCleanupBB(BB *bb);
  bool IsRcStackCall(Insn *insn);
  bool IsLdStPair(Insn *insn);
  bool RecordXzrInit(Insn *insn);
  void RecordLdStPair(Insn *insn);
  bool CanGetMemOpndImm(Operand *opnd, int32 &imm);
  bool ImmOffUsedInXzrInit(int32 imm);
  bool ImmOffUsedInLdStPair(int32 imm);
  void RemoveImmUsedInSkip();
  void RemoveImmUsedInLdSt();
  void OptLRInit();
  void OptLRFini();
  bool CanRenameConflictSlot(int32 imm);
  void AssignLocalRefForTrace();
  void DFS();
  void DoOptimize();
  void FinalizeLocRef();
  void FixMrtCall();

 public:
  explicit Riscv64OptLocalRef(CGFunc *func, MemPool *mp)
    : OptLocalRef(func, mp),
      olr_allocator(mp),
      min_imm(0x0fffffff),
      max_imm(0),
      max_assigned(0),
      num_traces(0),
      rc_stack_init_start_loc(0),
      rc_localrefvar_slots(0),
      rc_cleanup_skip_num(olr_allocator.Adapter()),
      imm_operands(olr_allocator.Adapter()),
      visited_bbs(olr_allocator.Adapter()),
      sorted_bbs(olr_allocator.Adapter()),
      dfs_bbs(olr_allocator.Adapter()),
      bb_walkback(olr_allocator.Adapter()),
      bb_trace(olr_allocator.Adapter()),
      rc_init_call(olr_allocator.Adapter()),
      rc_cleanup_call(olr_allocator.Adapter()),
      rc_skip_call(olr_allocator.Adapter()),
      insn_info_in_bb(olr_allocator.Adapter()),
      xzr_init_local_offset(olr_allocator.Adapter()),
      ld_st_pair_offset(olr_allocator.Adapter()),
      other_opnd(olr_allocator.Adapter()),
      has_rc_init(false),
      has_rc_cleanup(false),
      giveup(false) {}

  ~Riscv64OptLocalRef() {}

  void DoOptLocalRef() override;
};

class Iinfo {
 public:
  Insn *insn;
  int32 imm;
  int32 assigned;
  Iinfo *next;

  Iinfo(Insn *insn, int32 imm) : insn(insn), imm(imm), assigned(0), next(nullptr) {}
  virtual ~Iinfo() {}
};

}  // namespace maplebe
#endif  // MAPLEBE_INCLUDE_CG_AARCH64LVAR_H
