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

#ifndef MAPLEBE_INCLUDE_CG_CGBB_H
#define MAPLEBE_INCLUDE_CG_CGBB_H

#include "isa.h"
#include "insn.h"

// Maple IR headers
#include "mir_nodes.h"
#include "mir_symbol.h"

// Maple MP header
#include "mempool_allocator.h"

namespace maplebe {

#define UNDEFINED_INDEX (-1)

/*
 *  For live analysis, distinguish between partial registers which can be
 *  accessed independently.  Example, simd fp V8 is 128bits, which can become
 *  2 64bits or 4 32bits integer registers, represented by V8.d[0] & V8.d[1]
 *  or V8.s[0] & V8.s[1] & V8.s[2] & V8.s[3].
 *  Using upper bits to encode slots used so overlapping usage of the same
 *  register can be detected.
 *  V8.d[0] overlaps with V8.s[0] or V8.s[1].
 *  V8.s[0] is independent of V8.s[1].
 */
#define LIVE_REG_MASK (0x00ffffff)
#define LIVE_SLOT_MASK (0xff000000)
#define LIVE_POS_0 (0x10000000)
#define LIVE_POS_1 (0x20000000)
#define LIVE_POS_2 (0x40000000)
#define LIVE_POS_3 (0x80000000)
#define LIVE_POS_0_1 (0x30000000)
#define LIVE_POS_2_3 (0xc0000000)

/* For get bb */
#define FIRST_BB_OF_FUNC(FUNC) ((FUNC)->firstbb)
#define LAST_BB_OF_FUNC(FUNC) ((FUNC)->lastbb)

/* For iterating over basic blocks.  */
#define FOR_BB_BETWEEN(BASE, FROM, TO, DIR) for (BB * (BASE) = (FROM); (BASE) != (TO); (BASE) = (BASE)->DIR)
#define FOR_ALL_BB(BASE, FUNC) FOR_BB_BETWEEN(BASE, FIRST_BB_OF_FUNC(FUNC), nullptr, next)
#define FOR_ALL_BB_REV(BASE, FUNC) FOR_BB_BETWEEN(BASE, LAST_BB_OF_FUNC(FUNC), nullptr, prev)

/* For get insn */
#define FIRST_INSN(BLOCK) (BLOCK)->firstinsn
#define LAST_INSN(BLOCK) (BLOCK)->lastinsn
#define NEXT_INSN(INSN) (INSN)->next
#define PREV_INSN(INSN) (INSN)->prev

/* For iterating over insns in basic block.  */
#define FOR_INSN_BETWEEN(INSN, FROM, TO, DIR) \
  for (Insn * (INSN) = (FROM); (INSN) && (INSN) != (TO); (INSN) = (INSN)->DIR)

#define FOR_BB_INSNS(INSN, BLOCK) for (Insn * (INSN) = FIRST_INSN(BLOCK); (INSN); (INSN) = (INSN)->next)

#define FOR_BB_INSNS_REV(INSN, BLOCK) for (Insn * (INSN) = LAST_INSN(BLOCK); (INSN); (INSN) = (INSN)->prev)

/* For iterating over insns in basic block when we might remove the
   current insn.  */
#define FOR_BB_INSNS_SAFE(INSN, BLOCK, NEXT)                                                    \
  for (Insn * (INSN) = FIRST_INSN(BLOCK), *(NEXT) = (INSN) ? NEXT_INSN(INSN) : nullptr; (INSN); \
       (INSN) = (NEXT), (NEXT) = (INSN) ? NEXT_INSN(INSN) : nullptr)

#define FOR_BB_INSNS_REV_SAFE(INSN, BLOCK, NEXT)                                               \
  for (Insn * (INSN) = LAST_INSN(BLOCK), *(NEXT) = (INSN) ? PREV_INSN(INSN) : nullptr; (INSN); \
       (INSN) = (NEXT), (NEXT) = (INSN) ? PREV_INSN(INSN) : nullptr)

/* For data flow analysis. */
typedef enum InfoKind {
  kInfoReg = 0,
  kInfoMem
  // Info_RFlag
} InfoKind;

class DataAnalysisInfo {
 private:
  InfoKind kind;
  Operand *opnd;

 public:
  DataAnalysisInfo(InfoKind k, Operand *o) : kind(k), opnd(o) {}

  virtual ~DataAnalysisInfo() {}

  InfoKind GetKind() const {
    return kind;
  }

  Operand *GetOperand() const {
    return opnd;
  }

  bool IsRegInfo() const {
    return kind == kInfoReg;
  }

  bool IsMemInfo() const {
    return kind == kInfoMem;
  }

  // bool IsRFlagInfo() const { return kind == Info_RFlag; }

  MemOperand *GetMemOperand() const {
    if (kind == kInfoMem) {
      return static_cast<MemOperand *>(opnd);
    }
    return nullptr;
  }

  RegOperand *GetRegOperand() const {
    if (kind != kInfoMem) {
      return static_cast<RegOperand *>(opnd);
    }
    return nullptr;
  }
};

struct DataAnalysisInfoCmp {
  bool operator()(const DataAnalysisInfo &lhs, const DataAnalysisInfo &rhs) const {
    return lhs.GetOperand()->Less(rhs.GetOperand());
  }
};

class CgfuncLoops;

class BB {
 public:
  enum BBKind {
    kBBFallthru,  // default
    kBBIf,        // conditional branch
    kBBGoto,      // unconditional branch
    kBBCall,
    kBBReturn,
    kBBIntrinsic,  // BB created by inlining intrinsics; shares a lot with BB_if
    kBBRangegoto,
    kBBThrow,  // For call __java_throw_* and call exit, which will run out of function.
    kBBLast
  };

  enum color_t { kWhite = 0, kGray, kBlack };

  struct LiveCmp {
    bool operator()(const regno_t &lhs, const regno_t &rhs) const {
      // elements considered equal if return false
      uint32_t rn1 = lhs & LIVE_REG_MASK;
      uint32_t rn2 = rhs & LIVE_REG_MASK;
      if (rn1 != rn2) {
        return rn1 < rn2;
      }
      // See if sub fields overlap
      if (((lhs & LIVE_SLOT_MASK) & (rhs & LIVE_SLOT_MASK)) != 0) {
        return false;
      }
      return lhs < rhs;
    }
  };

 private:
  static const char *bbNames[kBBLast];

 public:
  uint32_t id;
  MapleAllocator *mallocator;
  uint32_t level;
  uint32_t frequency;
  BB *next, *prev;  // Doubly linked list of BBs;
  // They represent the order in which blocks are to be emitted.
  BBKind kind;  // The BB's last statement (i.e. laststmt) determines
  // what type this BB has. By default, kBBFallthru
  LabelIdx labidx;
  StmtNode *firststmt;
  StmtNode *laststmt;
  Insn *firstinsn;        // the first instruction
  Insn *lastinsn;         // the last instruction
  MapleList<BB *> preds;  // preds, succs represent CFG
  MapleList<BB *> succs;
  MapleList<BB *> eh_preds;
  MapleList<BB *> eh_succs;
  MapleList<BB *> loop_preds;
  MapleList<BB *> loop_succs;
  MapleSet<PregIdx> written_pseudo_regs;
  /*
     this is a set of pseudo registers in a basic block
     that are used (i.e., regread) before first written
     (i.e.,regassign)
   */
  MapleSet<PregIdx> use_pseudo_regs;
  /*this is for live in out analysis*/
  MapleVector<regno_t> use_regno;
  MapleVector<regno_t> def_regno;

  MapleUnorderedSet<regno_t> livein_regno;
  MapleUnorderedSet<regno_t> liveout_regno;

  CgfuncLoops *loop;
  bool livein_change;
  bool insertuse;
  bool unreachable;
  bool fastpath;  // prolog generation delayed
  bool is_catch;  // part of the catch bb, true does might also mean it is unreachable
  // Since is_catch is set early and unreachable detected later, there
  // are some overlap here.
  bool is_cleanup;  // true if the bb is cleanup bb. otherwise, false.
  bool isProEpilog;  // Temporary tag for modifying prolog/epilog bb

  /*This is for data flow analysis */
  std::map<DataAnalysisInfo, DataInsnInfo, DataAnalysisInfoCmp> gen;
  std::set<DataAnalysisInfo, DataAnalysisInfoCmp> kill;

  std::map<DataAnalysisInfo, std::set<DataInsnInfo, DataInsnInfoCmp>, DataAnalysisInfoCmp> in;
  std::map<DataAnalysisInfo, std::set<DataInsnInfo, DataInsnInfoCmp>, DataAnalysisInfoCmp> out;
  std::set<regno_t> lastuse_regno;

  // Different meaning for each data flow analysis.
  // For reaching definition, it means: true if BB is dirty for stack memory, otherwise false.
  // For storeloadopt, it indicates if it has a backedge entry.
  // For aarchregalloc.cpp, the bb is part of cleanup at end of function.
  // For aarchcolorra.cpp, the bb is part of cleanup at end of function.
  //                       also used for live range splitting.
  // For live analysis, it indicates if bb is cleanupbb.
  long internal_flag1;

  // Different meaning for each data flow analysis.
  // For reaching definition, it means: true if bb may run through a EH edge, otherwise false.
  // For aarchcolorra.cpp, used for live range splitting pruning of bb.
  long internal_flag2;

  // Different meaning for each data flow analysis.
  // For storeloadopt, it indicates if bb is visited.
  // For cgfunc.cpp, it temporarily marks for catch bb discovery.
  // For live analysis, it indicates if bb is visited.
  long internal_flag3;

  MapleList<Insn*> callInsns;

 public:
  explicit BB(uint32_t id, MapleAllocator *mallocator)
    : id(id),
      mallocator(mallocator),
      level(0),
      frequency(0),
      next(nullptr),
      prev(nullptr),
      kind(kBBFallthru /*default*/),
      labidx(MIRLabelTable::kDummyLabel),
      firststmt(nullptr),
      laststmt(nullptr),
      firstinsn(nullptr),
      lastinsn(nullptr),
      preds(mallocator->Adapter()),
      succs(mallocator->Adapter()),
      eh_preds(mallocator->Adapter()),
      eh_succs(mallocator->Adapter()),
      loop_preds(mallocator->Adapter()),
      loop_succs(mallocator->Adapter()),
      written_pseudo_regs(mallocator->Adapter()),
      use_pseudo_regs(mallocator->Adapter()),
      use_regno(mallocator->Adapter()),
      def_regno(mallocator->Adapter()),
      livein_regno(mallocator->Adapter()),
      liveout_regno(mallocator->Adapter()),
      loop(nullptr),
      livein_change(false),
      insertuse(false),
      unreachable(false),
      fastpath(false),
      is_catch(false),
      is_cleanup(false),
      isProEpilog(false),
      internal_flag1(0),
      internal_flag2(0),
      internal_flag3(0),
      callInsns(mallocator->Adapter()) {}

  virtual ~BB() {}

  virtual BB *Clone(MemPool *mp) const {
    return mp->Clone<BB>(*this);
  }

  void Dump();

  bool IsEmpty() const {
    return lastinsn == nullptr;
  }

  const char *GetKindName() const {
    return bbNames[kind];
  }

  void SetKind(BBKind k) {
    kind = k;
  }

  BBKind GetKind() const {
    return kind;
  }

  void AddLabel(LabelIdx lbl) {
    labidx = lbl;
  }

  void AppendBB(BB *bb) {
    CG_ASSERT(bb, "");
    bb->prev = this;
    bb->next = next;
    if (next) {
      next->prev = bb;
    }
    next = bb;
  }

  void PrependBB(BB *bb) {
    CG_ASSERT(bb, "");
    bb->next = this;
    bb->prev = this->prev;
    if (this->prev) {
      this->prev->next = bb;
    }
    this->prev = bb;
  }

  Insn *InsertInsnBefore(Insn *existing, Insn *newinsn);

  // returns newly inserted instruction
  Insn *InsertInsnAfter(Insn *existing, Insn *newinsn);

  Insn *InsertInsnBegin(Insn *insn) {
    if (lastinsn == nullptr) {
      firstinsn = lastinsn = insn;
      insn->bb = this;
    } else {
      InsertInsnBefore(firstinsn, insn);
    }
    return insn;
  }

  void AppendInsn(Insn *insn) {
    if (firstinsn == nullptr) {
      firstinsn = lastinsn = insn;
      insn->next = nullptr;
      insn->prev = nullptr;
    } else {
      InsertInsnAfter(lastinsn, insn);
    }
    insn->bb = this;
  }

  /*
     Insert an instruction
     1) if the last instruction is a control transfer instruction,
        before the instruction
     2) otherwise, append it to the end.
     Note that for 'call' instructions, the control returns to
     the call site, so we regard them as non-control transfer
     as far as this method is concerned.
   */
  void InsertBackBeforeControlTransfer(Insn *insn);

  void InsertBackBeforeControlTransferAndCall(Insn *insn);

  void ReplaceInsn(Insn *insn, Insn *newinsn);

  void RemoveInsn(Insn *insn);

  void RemoveInsnPair(Insn *insn, Insn *nextInsn);

  void RemoveInsnSequence(Insn *insn, Insn *nextInsn);

  // append all insns from bb into this bb
  void AppendBBInsns(BB *bb);

  // append all insns from bb into this bb
  void InsertAtBeginning(BB *bb);

  void ClearInsns()  // clear BB but don't remove insns of this
  {
    firstinsn = lastinsn = nullptr;
  }

  uint32 NumPreds() const {
    return preds.size();
  }

  bool IsPredecessor(const BB *p) {
    for (BB *bb : preds) {
      if (p == bb) {
        return true;
      }
    }
    return false;
  }

  bool IsBackEdgeDest() const {
    return loop_preds.size() > 0;
  }

  void RemoveFromPredecessorList(const BB *bb) {
    for (auto i = preds.begin(); i != preds.end(); ++i) {
      if (*i == bb) {
        preds.erase(i);
        return;
      }
    }
    CG_ASSERT(0, "request to remove a non-existent element?");
  }

  void RemoveFromSuccessorList(const BB *bb) {
    for (auto i = succs.begin(); i != succs.end(); ++i) {
      if (*i == bb) {
        succs.erase(i);
        return;
      }
    }
    CG_ASSERT(0, "request to remove a non-existent element?");
  }

  uint32 NumSuccs() const {
    return succs.size();
  }

  bool HasCall() const {
    return kind == kBBCall;
  };

  bool HasReturn() const {
    return kind == kBBReturn;
  }

  void AddPseudoRegisterWritten(PregIdx i) {
    written_pseudo_regs.insert(i);
  }

  void AddPseudoRegisterToUseList(PregIdx i) {
    use_pseudo_regs.insert(i);
  }

  void RemovePseudoRegisterFromUseList(PregIdx i) {
    use_pseudo_regs.erase(i);
  }

  MapleSet<PregIdx> &GetUsePRegs() {
    return use_pseudo_regs;
  }

  void EraseUseRegnoFromVector(regno_t reg) {
    for (auto it = use_regno.begin(); it != use_regno.end();) {
      if (*it == reg) {
        it = use_regno.erase(it);
      } else {
        it++;
      }
    }
  }

  void EraseDefRegnoFromVector(regno_t reg) {
    for (auto it = def_regno.begin(); it != def_regno.end();) {
      if (*it == reg) {
        it = def_regno.erase(it);
      } else {
        it++;
      }
    }
  }

  // Number of instructions excluding DbgInsn and comments
  int NumInsn();

};  // class BB

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_CGBB_H
