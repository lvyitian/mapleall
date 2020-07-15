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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64REGALLOC_H_
#define MAPLEBE_INCLUDE_CG_AARCH64REGALLOC_H_
#include "reg_alloc.h"
#include "aarch64_operand.h"
#include "aarch64_insn.h"
#include "aarch64_abi.h"

#define O1_INT_REG_FOR_SPILL R15
#define O1_FLT_REG_FOR_SPILL V31
#define MAX_INT_SPILL 3
#define MAX_FP_SPILL 2

namespace maplebe {

class AArch64RegAllocator : public RegAllocator {
 protected:
  bool avail_reg_set_[MAXREG];
  MapleMap<uint32, AArch64reg_t> reg_map_;  // virtual-register-to-physical-register map
  MapleSet<uint8> live_reg_;                // a set of currently live physical registers
  MapleSet<Operand *> allocated_set_;       // already allocated

  AArch64reg_t atomic_store_result_reg;

 protected:
  Operand *AllocSrcOpnd(Operand *opnd, OpndProp *opndprop = nullptr, Insn *insn = nullptr, BB *bb = nullptr);

  Operand *AllocDestOpnd(Operand *opnd, Insn *insn, uint32 index, BB *bb = nullptr);

 public:
  AArch64RegAllocator(CGFunc *cgfunc, MapleAllocator *mallocator)
    : RegAllocator(cgfunc),
      reg_map_(std::less<uint32>(), mallocator->Adapter()),
      live_reg_(std::less<uint8>(), mallocator->Adapter()),
      allocated_set_(std::less<Operand *>(), mallocator->Adapter()),
      atomic_store_result_reg(kRinvalid) {
    for (int i = 0; i != MAXREG; i++) {
      avail_reg_set_[i] = false;
    }
  }

  virtual ~AArch64RegAllocator() {}

  void InitAvailReg();
  bool AllocatePhysicalRegister(RegOperand *opnd, OpndProp *prop);
  void PreAllocate();
  void ReleaseReg(RegType regty, AArch64reg_t reg);
  void ReleaseReg(RegOperand *regopnd, OpndProp *prop);
  void GetPhysicalRegisterBank(RegType regty, uint8 &start, uint8 &end);
  void AllocHandleCallee(Insn *insn, const AArch64MD *md);
  bool IsSpecialReg(AArch64reg_t reg);
  bool IsUntouchableReg(uint32 regno);
  void SaveCalleeSavedReg(RegOperand *opnd);
  virtual void StorePseudoRegister(RegOperand *regopnd, AArch64reg_t regNo, Insn *insn, BB *bb) {}

  virtual regno_t DoRegisterSpill(RegOperand *regopnd, Insn *insn, bool isDstRegister, BB *bb) {
    return 0;
  }

  const char *PhaseName() {
    return "regalloc";
  }
};

class DefaultO0RegAllocator : public AArch64RegAllocator {
 public:
  DefaultO0RegAllocator(CGFunc *cgfunc, MapleAllocator *mallocator) : AArch64RegAllocator(cgfunc, mallocator) {}

  ~DefaultO0RegAllocator() override {}

  bool AllocateRegisters() override;
};

class O1RegAllocator : public AArch64RegAllocator {
 public:
  O1RegAllocator(CGFunc *cgfunc, MapleAllocator *mallocator)
    : AArch64RegAllocator(cgfunc, mallocator), m_intRegIndex(0), m_floatRegIndex(0), m_validIndex(0) {}

  ~O1RegAllocator() override {}

  bool AllocateRegisters() override;
  regno_t DoRegisterSpill(RegOperand *regopnd, Insn *insn, bool isSrcRegister, BB *bb) override;
  void StorePseudoRegister(RegOperand *regopnd, AArch64reg_t regNo, Insn *insn, BB *bb) override;
  Operand *GetOperandFromAllocatedSet(regno_t);
  regno_t GetVirtualRegFromPhysicalReg(AArch64reg_t);

 private:
  void CollectPRegUsesInExpr(BaseNode *expr, BB *bb);

  void CollectPRegUses(Opcode c, StmtNode *s, BB *bb);

  void CollectPRegDefsUses(BB *bb);

  void InitValidSpillRegIndex(Insn *insn);

  int GetNextIntRegIndex() {
    while (!(m_validIndex & (0x1 << m_intRegIndex))) {
      m_intRegIndex++;
    }
    return m_intRegIndex++;
  }

  int GetNextFloatRegIndex() {
    while (!(m_validIndex & (0x1 << (m_floatRegIndex + 8)))) {
      m_floatRegIndex++;
    }
    return m_floatRegIndex++;
  }

  void ClearRegIndex() {
    m_intRegIndex = 0;
    m_floatRegIndex = 0;
  }

  unsigned int m_intRegIndex;
  unsigned int m_floatRegIndex;
  // bits[0-4] for integer registers, bits[8-12] for float registers.
  unsigned int m_validIndex;
};

class AArch64CallerSavedRegHandler : public CallerSavedRegHandler {
 public:
  AArch64CallerSavedRegHandler(CGFunc *f /*, MapleAllocator* mallocator*/) : CallerSavedRegHandler(f) {}

  ~AArch64CallerSavedRegHandler() {}

  inline static int Reg2BitPos(AArch64reg_t r) {
    return int(r) - 1;
  }

  inline static AArch64reg_t BitPos2Reg(int p) {
    return AArch64reg_t(p + 1);
  }
};

class LSRALinearScanRegAllocator : public AArch64RegAllocator {
  enum SimdSlot {
    /*
     * Can only use slots 0 and 1 for spilling.  Upper 64bits are not guaranteed
     * to be preserved across a call.
     */
    kRegSlot0 = 0,  // enum used as index, make sure to assign proper value.
    kRegSlot1 = 1,
    kRegSlotNone
  };

  enum RegInCatch {
    /*
     * RA do not want to allocate certain registers if a live interval is
     * only inside of catch blocks.
     */
    kRegCatchNotInit = 0,  // unitialized state
    kRegNotInCatch = 1,    // interval is part or all outside of catch
    kRegAllInCatch         // inteval is completely inside catch
  };

  enum RegInCleanup {
    // Similar to reg_in_catch_t
    kRegCleanupNotInit = 0,        // unitialized state
    kRegAllInFirstbb = 1,          // interval is all in the first bb
    kRegAllOutCleanup = 2,         // interval is all outside of cleanup, must in normal bb, may in first bb.
    kRegInCleanupAndFirstbb = 3,   // inteval is in cleanup and first bb.
    kRegInCleanupAndNormalbb = 4,  // inteval is in cleanup and non-first bb.
    kRegAllInCleanup               // inteval is inside cleanup, except for bb 1
  };

  enum IsLocalrefvar {
    // Similar to reg_in_catch_t
    kIsLocalrefvarUnknown = 0,
    kIsLocalrefvarInvalid = 1,
    kIsLocalrefvarValid
  };

  class LiveInterval {
   public:
    Insn *is_call;
    uint32 first_def;
    uint32 last_use;
    uint32 phys_use;
    uint32 regno;
    /* physical register, using cg defined reg based on R0/V0. */
    uint32 assigned_reg;
    uint32 stk_slot;
    RegType regtype;
    uint32 first_acrossed_call;
    bool end_by_call;
    bool use_before_def;
    bool should_save;
    bool multi_use_in_bb;  // vreg has more than 1 use in bb
    bool is_throwval;      // only for R0(R1?) which are used for explicit incoming value of throwval;
    bool is_simd_spilled;
    bool is_caller_spilled;
    bool must_allocate;  // The register cannot be spilled (clinit pair)
    uint32 ref_count;
    float priority;
    MapleVector<std::pair<uint32, uint32>> ranges;
    MapleVector<std::pair<uint32, uint32>> holes;
    MapleSet<uint32> use_positions;
    LiveInterval *li_parent;   // Current li is in aother li's hole.
    LiveInterval *li_child;    // Another li is in current li's hole.
    regno_t simd_spill_reg;    // simd reg used for spilling
    SimdSlot simd_slot;        // s[0] or s[1] or none
    int32 localrefvar_offset;  // if spilled stack loc associated
    uint32 result_count;       // number of times this vreg has been written
    uint32 localrefvar_count;  // number of times this vreg is matched with localrefvar
   private:
    uint8 in_catch_state;        // part or all of live interval is outside of catch blocks
    uint8 in_cleanup_state;      // part or all of live interval is outside of cleanup blocks
    uint8 is_localrefvar_valid;  // Is this live interval coupled with a local ref var
   public:
    LiveInterval(MapleAllocator *mallocator)
      : is_call(nullptr),
        first_def(0),
        last_use(0),
        phys_use(0),
        regno(0),
        assigned_reg(0),
        stk_slot(-1),
        regtype(kRegTyUndef),
        first_acrossed_call(0),
        end_by_call(false),
        use_before_def(false),
        should_save(false),
        multi_use_in_bb(false),
        is_throwval(false),
        is_simd_spilled(false),
        is_caller_spilled(false),
        must_allocate(false),
        ref_count(0),
        priority(0.0),
        ranges(mallocator->Adapter()),
        holes(mallocator->Adapter()),
        use_positions(mallocator->Adapter()),
        li_parent(nullptr),
        li_child(nullptr),
        simd_spill_reg(0),
        simd_slot(kRegSlotNone),
        localrefvar_offset(0),
        result_count(0),
        localrefvar_count(0),
        in_catch_state(kRegCatchNotInit),
        in_cleanup_state(kRegCleanupNotInit),
        is_localrefvar_valid(kIsLocalrefvarUnknown) {}

    virtual ~LiveInterval() {}

    void AddRange(uint32 from, uint32 to);
    void AddUsePos(uint32 pos);

    void SetInCatchState() {
      // Once in REG_NOT_IN_CATCH, it is irreversible since once an interval
      // is not in a catch, it is not completely in a catch.
      if (in_catch_state == kRegNotInCatch) {
        return;
      }
      in_catch_state = kRegAllInCatch;
    }

    void SetNotInCatchState() {
      in_catch_state = kRegNotInCatch;
    }

    bool IsInCatch() {
      return (in_catch_state == kRegAllInCatch);
    }

    void SetInCleanupState() {
      switch (in_cleanup_state) {
        case kRegCleanupNotInit:
          in_cleanup_state = kRegAllInCleanup;
          break;
        case kRegAllInFirstbb:
          in_cleanup_state = kRegInCleanupAndFirstbb;
          break;
        case kRegAllOutCleanup:
          in_cleanup_state = kRegInCleanupAndNormalbb;
          break;
        case kRegInCleanupAndFirstbb:
          break;
        case kRegInCleanupAndNormalbb:
          break;
        case kRegAllInCleanup:
          break;
        default:
          CG_ASSERT(false, "CG Internal error.");
          break;
      }
    }

    void SetNotInCleanupState(bool isFirstBB) {
      switch (in_cleanup_state) {
        case kRegCleanupNotInit: {
          if (isFirstBB) {
            in_cleanup_state = kRegAllInFirstbb;
          } else {
            in_cleanup_state = kRegAllOutCleanup;
          }
          break;
        }
        case kRegAllInFirstbb: {
          if (!isFirstBB) {
            in_cleanup_state = kRegAllOutCleanup;
          }
          break;
        }
        case kRegAllOutCleanup:
          break;
        case kRegInCleanupAndFirstbb: {
          if (!isFirstBB) {
            in_cleanup_state = kRegInCleanupAndNormalbb;
          }
          break;
        }
        case kRegInCleanupAndNormalbb:
          break;
        case kRegAllInCleanup: {
          if (isFirstBB) {
            in_cleanup_state = kRegInCleanupAndFirstbb;
          } else {
            in_cleanup_state = kRegInCleanupAndNormalbb;
          }
          break;
        }
        default:
          CG_ASSERT(false, "CG Internal error.");
          break;
      }
    }

    bool IsAllInCleanupOrFirstBB() {
      return (in_cleanup_state == kRegAllInCleanup || in_cleanup_state == kRegInCleanupAndFirstbb);
    }

    bool IsAllOutCleanup() {
      return in_cleanup_state == kRegAllInFirstbb || in_cleanup_state == kRegAllOutCleanup;
    }

    void SetLocalRefVarStateValid() {
      // Once in IS_LOCALREFVAR_INVALID, it is irreversible.
      if (is_localrefvar_valid == kIsLocalrefvarInvalid) {
        return;
      }
      is_localrefvar_valid = kIsLocalrefvarValid;
    }

    void SetLocalRefVarStateInvalid() {
      is_localrefvar_valid = kIsLocalrefvarInvalid;
    }

    bool IsLocalRefVar() {
      return is_localrefvar_valid == kIsLocalrefvarValid;
    }
  };

  struct ActiveCmp {
    bool operator()(const LiveInterval *lhs, const LiveInterval *rhs) const {
      // elements considered equal if return false
      if (lhs == rhs) {
        return false;
      }
      if (lhs->first_def == rhs->first_def && lhs->last_use == rhs->last_use && lhs->regno == rhs->regno &&
          lhs->regtype == rhs->regtype && lhs->assigned_reg == rhs->assigned_reg) {
        return false;
      }
      if (lhs->phys_use != 0 && rhs->phys_use != 0) {
        if (lhs->first_def == rhs->first_def) {
          return lhs->phys_use < rhs->phys_use;
        } else {
          return lhs->first_def < rhs->first_def;
        }
      }
      // At this point, lhs != rhs
      if (lhs->last_use == rhs->last_use) {
        if (lhs->first_def > rhs->first_def) {
          return false;
        } else {
          return true;
        }
      }
      return lhs->last_use < rhs->last_use;
    }
  };

 public:
  // Comparison function for LiveInterval
  MapleVector<bool> visited_bbs;
  MapleVector<BB *> sorted_bbs;
  MapleStack<BB *> dfs_bbs;
  MapleVector<LiveInterval *> LI_;
  MapleVector<LiveInterval *> last_int_param_li;
  MapleVector<LiveInterval *> last_fp_param_li;
  MapleQueue<LiveInterval *> initial_que;
  typedef MapleQueue<LiveInterval *> single_que;
  MapleVector<single_que> int_param_queue;
  MapleVector<single_que> fp_param_queue;
  MapleList<LiveInterval *> call_list;
  MapleSet<LiveInterval *, ActiveCmp> active;
  MapleSet<LiveInterval *, ActiveCmp>::iterator it_finded;

  // Change these into vectors so it can be added and deleted easily.
  MapleSet<regno_t> simd_spill_regs;           // using simd for spill int reg, both [1,0]
  MapleSet<regno_t> simd_spill_0;              // reg.s[0], available for spill
  MapleSet<regno_t> simd_spill_1;              // reg.s[1], available for spill
  MapleSet<uint32> int_caller_reg_set;         // integer caller saved
  MapleSet<uint32> int_callee_reg_set;         //         callee
  MapleSet<uint32> int_param_reg_set;          //         parameter
  MapleVector<regno_t> lvar_offset_regno_map;  // mapping of stack offsets
  // and register
  uint32 int_caller_mask;              // bit mask for all possible caller int
  uint32 int_callee_mask;              //                           callee
  uint32 int_param_mask;               //     (physical-register)   parameter
  MapleSet<uint32> fp_caller_reg_set;  // float caller saved
  MapleSet<uint32> fp_callee_reg_set;  //       callee
  MapleSet<uint32> fp_param_reg_set;   //       parameter
  MapleVector<uint32> callee_use_cnt;  // Number of time callee reg is seen
  uint32_t last_simd_insn_num;
  uint32_t last_simd_slot;
  uint32 fp_caller_mask;                     // bit mask for all possible caller fp
  uint32 fp_callee_mask;                     //                           callee
  uint32 fp_param_mask;                      //      (physical-register)  parameter
  regno_t int_spill_reg_set[MAX_INT_SPILL];  // integer regs put aside for spills
  regno_t fp_spill_reg_set[MAX_FP_SPILL];    //   float
  regno_t simd_reg_reclaim[3];
  regno_t simd_reg_reclaim_slot[3];
  uint32 simd_reg_reclaim_idx;
  uint32 int_bb_def_mask;  // locally which physical reg is defined
  uint32 fp_bb_def_mask;
  uint32 debug_spill_cnt;
  uint32 max_int_spill;  // # of spill registers reserved
  uint32 reg_used_in_bb_sz;
  uint64 *reg_used_in_bb;
  uint32 max_insn_num;
  regno_t min_vreg_num;
  regno_t max_vreg_num;
  bool fast_alloc;
  bool spill_all;
  bool has_3reg_opnd;
  bool is_spill_zero;
  bool is_mov_dst_simd_spilled;
  bool is_mov_src_simd_spilled;
  bool use_localvar_spill;
  bool should_opt_int_callee;
  bool should_opt_fp_callee;
  uint64 spill_count;
  uint64 reload_count;
  uint64 caller_save_spill_count;
  uint64 caller_save_reload_count;
  uint64 simd_spill_count;
  uint64 simd_reload_count;
  MapleAllocator *allocator;
  uint32 localrefvar_min_stack_loc;
  uint32 localrefvar_max_stack_loc;

 public:
  LSRALinearScanRegAllocator(CGFunc *cgfunc, MapleAllocator *mallocator)
    : AArch64RegAllocator(cgfunc, mallocator),
      visited_bbs(mallocator->Adapter()),
      sorted_bbs(mallocator->Adapter()),
      dfs_bbs(mallocator->Adapter()),
      LI_(mallocator->Adapter()),
      last_int_param_li(mallocator->Adapter()),
      last_fp_param_li(mallocator->Adapter()),
      initial_que(mallocator->Adapter()),
      int_param_queue(mallocator->Adapter()),
      fp_param_queue(mallocator->Adapter()),
      call_list(mallocator->Adapter()),
      active(mallocator->Adapter()),
      simd_spill_regs(mallocator->Adapter()),
      simd_spill_0(mallocator->Adapter()),
      simd_spill_1(mallocator->Adapter()),
      int_caller_reg_set(mallocator->Adapter()),
      int_callee_reg_set(mallocator->Adapter()),
      int_param_reg_set(mallocator->Adapter()),
      lvar_offset_regno_map(mallocator->Adapter()),
      int_caller_mask(0),
      int_callee_mask(0),
      int_param_mask(0),
      fp_caller_reg_set(mallocator->Adapter()),
      fp_callee_reg_set(mallocator->Adapter()),
      fp_param_reg_set(mallocator->Adapter()),
      callee_use_cnt(mallocator->Adapter()),
      last_simd_insn_num(0),
      last_simd_slot(0),
      fp_caller_mask(0),
      fp_callee_mask(0),
      fp_param_mask(0),
      simd_reg_reclaim{ 0, 0, 0 },
      simd_reg_reclaim_slot{ 0, 0, 0 },
      simd_reg_reclaim_idx(0),
      int_bb_def_mask(0),
      fp_bb_def_mask(0),
      debug_spill_cnt(0),
      max_int_spill(MAX_INT_SPILL),
      reg_used_in_bb_sz(0),
      reg_used_in_bb(nullptr),
      max_insn_num(0),
      min_vreg_num(0xFFFFFFFF),
      max_vreg_num(0),
      fast_alloc(false),
      spill_all(false),
      has_3reg_opnd(false),
      is_spill_zero(false),
      is_mov_dst_simd_spilled(false),
      is_mov_src_simd_spilled(false),
      use_localvar_spill(false),
      should_opt_int_callee(false),
      should_opt_fp_callee(false),
      spill_count(0),
      reload_count(0),
      caller_save_spill_count(0),
      caller_save_reload_count(0),
      simd_spill_count(0),
      simd_reload_count(0),
      allocator(mallocator),
      localrefvar_min_stack_loc(0),
      localrefvar_max_stack_loc(0) {
    for (int i = 0; i < 8; i++) {
      int_param_queue.push_back(initial_que);
      fp_param_queue.push_back(initial_que);
    }
  }
  ~LSRALinearScanRegAllocator() override {}

  bool AllocateRegisters() override;
  bool AllPredBBVisited(BB *);
  BB *MarkStraightLineBBInBFS(BB *);
  BB *SearchForStraightLineBBs(BB *);
  void BFS(BB *);
  void ComputeBlockOrder();
  void GetAvailablePRregs(RegType regty, uint32 &begin, uint32 &end, bool isCaller);
  void PrintRegSet(MapleSet<uint32> set, string str);
  void PrintLiveInterval(LiveInterval *li, string str);
  void PrintLiveRanges();
  void PrintParamQueue(string str);
  void PrintCallQueue(string str);
  void PrintActiveList(string str, uint32 len = 0);
  void PrintActiveListSimple();
  void PrintLiveIntervals();
  void DebugCheckActiveList();
  void InitFreeRegPool();
  void RecordCall(Insn *insn);
  void RecordPhysRegs(Insn *insn, RegOperand *regOpnd, uint32 insnNum, bool isDef, bool isCall = false);
  void SetupLiveInterval(Operand *opnd, Insn *insn, bool isDef, uint32 &nUses);
  void ComputeLiveInterval();
  void FindLowestPrioInActive(LiveInterval **li, RegType regtype = kRegTyInt, bool startRa = false);
  void LiveIntervalAnalysis();
  bool OpndNeedAllocation(Insn *insn, Operand *opnd, bool isdef, uint32 insnNum);
  void InsertParamToActive(Operand *opnd, uint32 insnNum);
  void InsertToActive(Operand *opnd, uint32 insnNum);
  void ReturnPregToSet(LiveInterval *li, uint32 preg);
  void ReleasePregToset(LiveInterval *li, uint32 preg);
  void RetireFromActive(const Insn *insn);
  void AssignPhysRegsForInsn(Insn *insn);
  uint32 FindLocalRefVarStackLoc(Insn *insn, uint32 param);
  bool DetectLocalRefVarInfo(Insn *insn, uint32 imm);
  regno_t FindLocalRefVarReg(Insn *);
  void MarkLocalRefVarForLiveInterval(Insn *, uint32 offset);
  RegOperand *GetReplaceOpnd(Insn *insn, Operand *opnd, uint32 &spillIdx, bool isdef);
  void FinalizeRegisters();
  bool UseSimdForSpill(Insn *insn, Operand *opnd, bool isDef, uint32 spillIdx = 0);
  void SpillOperand(Insn *insn, Operand *opnd, bool isDef, uint32 spillIdx);
  void SetOperandSpill(Operand *opnd);
  RegOperand *HandleSpillForInsn(Insn *insn, Operand *opnd);
  MemOperand *GetSpillMem(uint32 vregno, uint8 isDest, Insn *insn, AArch64reg_t regno, uint8 &isOutOfRange);
  RegOperand *InsertSpillReload(Insn *insn, Operand *opnd, bool isDef, uint32 spillIdx);
  void InsertCallerSave(Insn *insn, Operand *opnd, bool isDef);
  uint32 GetRegFromSet(MapleSet<uint32> &set, regno_t offset, LiveInterval *li, regno_t forcedReg = 0);
  uint32 AssignSpecialPhysRegPattern(Insn *insn, LiveInterval *li);
  uint32 FindAvailablePhyReg(LiveInterval *li, Insn *insn);
  RegOperand *AssignPhysRegs(Operand *opnd, Insn *insn);
  void BuildIntervalRanges();
  uint32 FillInHole(LiveInterval *li);
  void SimdSpillToFcmpOpt();
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_AARCH64REGALLOC_H_
