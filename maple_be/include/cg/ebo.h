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

#ifndef MAPLEBE_INCLUDE_CG_EBO_H
#define MAPLEBE_INCLUDE_CG_EBO_H

#include "cg_func.h"
#include "insn.h"
#include "cg_phase.h"

namespace maplebe {

#define EBO_DEFAULT_MEM_HASH 0
#define EBO_NO_ALIAS_MEM_HASH 1
#define EBO_SPILL_MEM_HASH 2
#define EBO_MAX_MEM_INSN_HASH 3
#define EBO_COPY_INSN_HASH 3
#define EBO_RESERVED_INSN_HASH 4
#define EBO_MAX_EXP_INSN_HASH 1024
#define EBO_MAX_OPND_HASH (1 << 8)
#define EBO_MAX_INSN_HASH (EBO_RESERVED_INSN_HASH + EBO_MAX_EXP_INSN_HASH)
#define EBO_EXP_INSN_HASH(val) ((EBO_MAX_EXP_INSN_HASH - 1) & (val >> 6))
#define EBO_MAX_OPND_INFO (2 * (Insn::kMaxOperandNum))
#define BYTE_PER_SIZE 8

/* forward decls */
class InsnInfo;
class LiveAnalysis;
/**
 * @class <OpndInfo>
 * @brief The infomation of Operand.
 * Define the key data structure that will be used to track information
 * associated with each definition of a Operand.
 */
class OpndInfo {
 public:
  int32_t hashval;             // Mem operand is placed in hash table, this is the hashval of it, and otherwise -1.
  Operand *opnd;               // Operand
  Operand *replacement_opnd;   // Rename opnd with this new name.
  OpndInfo *replacement_info;  // Rename opnd with this info.
  BB *bb;                      // The Definining bb.
  Insn *insn;                  // The Defining insn.
  InsnInfo *insninfo;
  bool redefined_inbb;  // A following definition exisit in bb.
  bool redefined;       // A following definition exisit.
  OpndInfo *same;       // Other definitions of the same operand.
  OpndInfo *prev;
  OpndInfo *next;
  OpndInfo *hash_next;
  int32_t refcount;  // Number of references to the operand.
 public:
  OpndInfo(Operand *opnd) : opnd(opnd), hash_next(nullptr), refcount(0) {
    hashval = 0;
    replacement_opnd = nullptr;
    replacement_info = nullptr;
    bb = nullptr;
    insn = nullptr;
    insninfo = nullptr;
    redefined = false;
    redefined_inbb = false;
    same = nullptr;
    prev = nullptr;
    next = nullptr;
  }
  virtual ~OpndInfo() {}
};

class MemOpndInfo : public OpndInfo {
 protected:
  OpndInfo *base;
  OpndInfo *offset;

 public:
  explicit MemOpndInfo(Operand *opnd) : OpndInfo(opnd), base(nullptr), offset(nullptr) {}

  ~MemOpndInfo() {}

 public:
  inline OpndInfo *GetBaseInfo() const {
    return base;
  }

  inline OpndInfo *GetOffsetInfo() const{
    return offset;
  }

  inline void SetBaseInfo(OpndInfo *baseinfo) {
    base = baseinfo;
  }

  inline void SetOffsetInfo(OpndInfo *offinfo) {
    offset = offinfo;
  }
};
/**
 * @class <InsnInfo>
 * @brief The infomation of insn.
 * Define the key data structure that will be used to track information
 * associated with each defining insn.
 */
class InsnInfo {
 public:
  int32_t hash_index;
  bool mustnot_be_removed;  // Some condition requires this insn.
  BB *bb;                   // The defining bb.
  Insn *insn;               // The defining insn.
  InsnInfo *same;           // Other insns with the same hash value.
  InsnInfo *prev;
  InsnInfo *next;
  OpndInfo **result;  // Result array.
  OpndInfo **orig_opnd;
  OpndInfo **optimal_opnd;
  OpndInfo *opnd_info[EBO_MAX_OPND_INFO];

 public:
  InsnInfo(Insn *insn)
    : hash_index(0),

      mustnot_be_removed(false),
      bb(insn->bb),
      insn(insn),
      same(nullptr),
      prev(nullptr),
      next(nullptr) {
    result = nullptr;
    orig_opnd = nullptr;
    optimal_opnd = nullptr;
    for (int i = 0; i < EBO_MAX_OPND_INFO; i++) {
      opnd_info[i] = nullptr;
    }
  }
  virtual ~InsnInfo() {}
};

/**
 * @class <Ebo>
 * @brief The Ebo structure.
 * Define the main data structure that will perform extended basic block optimizationn
 */
class Ebo {
 public:
  CGFunc *cgfunc;
  bool before_regalloc;  // True if perform Ebo before register register allocation.
  const char *phaseName;

 private:
  LiveAnalysis *live;
  int32_t bbnum;  // bb numbers for an extend block.
  MemPool *ebomp;
  MapleAllocator ebo_allocator;
  bool *visited_bbs;
  OpndInfo *first_opndinfo;
  OpndInfo *last_opndinfo;
  InsnInfo *first_insninfo;
  InsnInfo *last_insninfo;
  MapleMap<int32_t, OpndInfo *> vreginfo;
  OpndInfo **exprinfo_table;
  InsnInfo **insninfo_table;

 public:
  Ebo(CGFunc *func, MemPool *mp, LiveAnalysis *live, bool before, const char *phase)
    : cgfunc(func),
      before_regalloc(before),
      phaseName(phase),
      live(live),
      bbnum(0),
      ebomp(mp),
      ebo_allocator(mp),
      visited_bbs(nullptr),
      first_opndinfo(nullptr),
      last_opndinfo(nullptr),
      first_insninfo(nullptr),
      last_insninfo(nullptr),
      vreginfo(std::less<int32_t>(), ebo_allocator.Adapter()),
      exprinfo_table(ebomp->NewArray<OpndInfo *>(EBO_MAX_OPND_HASH)),
      insninfo_table(ebomp->NewArray<InsnInfo *>(EBO_MAX_INSN_HASH)) {}

  virtual ~Ebo() {}

  virtual regno_t GetLowVec(Operand *opnd) {
    return 0;
  };
  virtual regno_t GetHighVec(Operand *opnd) {
    return 0;
  };
  virtual bool IsFloatReg(RegOperand *opnd) {
    return false;
  };
  virtual bool IsFmov(Insn *insn) {
    return false;
  };
  virtual bool SpecialSequence(Insn *insn, Operand **opnds, OpndInfo **info) {
    return false;
  };
  virtual bool DoConstProp(Insn *insn, int i, Operand *opnd) {
    return false;
  };
  virtual bool DoConstantFold(Insn *insn, Operand **opnds, OpndInfo **opndInfo) {
    return false;
  };
  virtual bool ConstantOperand(Insn *insn, Operand **opnds, OpndInfo **opndInfo) {
    return false;
  };
  virtual bool ResoveCondBranch(Insn *insn, Operand **opnds) {
    return false;
  };
  virtual bool DeleteDupInsn(Insn *insn, OpndInfo **opndInfo, InsnInfo *insninfo) {
    return false;
  };
  virtual bool DeleteDupMemInsn(Insn *insn, OpndInfo **opndInfo, InsnInfo *insninfo, InsnInfo *prevInfo) {
    return false;
  };
  virtual bool RemoveRedundantLoad(BB *bb, Insn *insn, Operand **opnds, OpndInfo **opndInfo, OpndInfo **origInfo) {
    return false;
  };
  virtual int32_t GetOffsetVal(MemOperand *mem) {
    return 0;
  };
  virtual bool OperandEqSpecial(Operand *op1, Operand *op2) {
    return false;
  };
  virtual void MarkCalleeSavedRegs(Insn *insn){};
  virtual bool IsBranchCmpSpecial(Insn *insn) {
    return false;
  };
  virtual bool IsImplicit(Insn *insn) {
    return false;
  };
  virtual void InitCallerSaveRegisters(){};
  virtual void InitCallAndReturnUseRegisters(){};
  virtual void DefineCallerSaveRegisters(InsnInfo *insninfo){};
  virtual void DefineReturnUseRegister(Insn *insn){};
  virtual void DefineCallUseSpecialRegister(Insn *insn){};
  virtual void DefineClinitSpecialRegisters(InsnInfo *insninfo){};
  virtual bool ReplaceMovToVmov(Insn *insn, Insn *replaceInsn) {
    return false;
  }

  virtual bool IsMovToSIMDVmov(Insn *insn, Insn *replaceInsn) {
    return false;
  }

  virtual bool ChangeLdrMop(Insn *insn, Operand *opnd) {
    return false;
  }

  virtual bool IsAdd(Insn *insn) {
    return false;
  }

  virtual bool IsCmp(Insn *insn) {
    return false;
  }

  virtual bool IsVecReg(Operand *opnd) {
    return false;
  }

  virtual bool IsClinitCheck(Insn *insn) {
    return false;
  }

  int32_t GetRegNumForMem(Insn *insn);
  MemOpndInfo *GetMemInfo(InsnInfo *insninfo);
  bool IsGlobalNeeded(Insn *insn);
  bool GRAHomeable(Operand *opnd) const {
    return false;
  }

  void SetInsnInfo(int32_t hashval, InsnInfo *info) {
    insninfo_table[hashval] = info;
  }

  void IncRef(OpndInfo *info) const {
    info->refcount++;
  }

  void DecRef(OpndInfo *info) const {
    info->refcount--;
  }

  Operand *GetBase(Insn *insn);
  Operand *GetOffset(Insn *insn);
  MemOperand *GetMemOpnd(Insn *insn);
  bool IsSaveReg(Operand *opnd);
  RegOperand *GetRegOpnd(Insn *insn);
  bool IsFrameReg(Operand *opnd);
  bool OperandEqual(Operand *op1, Operand *op2);
  bool FindDupMemInsn(BB *bb, Insn *insn, Operand **opnds, OpndInfo **opndInfo, OpndInfo **origInfo);
  bool FindDupInsn(BB *bb, Insn *insn, Operand **opnds, OpndInfo **opndInfo, OpndInfo **origInfo);
  bool FindPrevConst(Insn *insn, OpndInfo **opndInfo) const;
  Operand *GetZeroOpnd(uint32_t size);
  bool IsPhysicalReg(Operand *opnd);
  bool HasAssignedReg(Operand *opnd);
  bool IsOfSameClass(Operand *op0, Operand *op1) const;
  bool OpndAvailableInBB(const BB *bb, OpndInfo *info);
  bool IsNotVisited(const BB *bb) {
    return (visited_bbs[bb->id] == false);
  };
  void SetBBVisited(const BB *bb) {
    visited_bbs[bb->id] = true;
  };
  void UpdateOpndInfo(Operand *opnd, OpndInfo *opndinfo, OpndInfo *newinfo, int32_t hashval);
  void SetOpndInfo(Operand *opnd, OpndInfo *opndinfo, int32_t hashval);
  bool RegistersIdentical(Operand *opnd0, Operand *opnd1);
  OpndInfo *GetOpndInfo(Operand *opnd, int32_t hashval);
  OpndInfo *GetNewOpndInfo(BB *bb, Insn *insn, Operand *opnd, int32_t hashval);
  OpndInfo *OperandInfoUse(BB *currentBb, Insn *currentInsn, Operand *localOpnd);
  bool NoEhinsnBetweenRedef(OpndInfo *opndinfo, OpndInfo *opndinfoPrev);
  OpndInfo *OperandInfoDef(BB *currentBb, Insn *currentInsn, Operand *localOpnd);
  InsnInfo *GetNewInsnInfo(Insn *insn);
  int32_t ComputeOpndHash(Operand *opnd);
  int32_t ComputeHashVal(Insn *insn, OpndInfo **opndInfo);
  void MarkOpndLiveIntoBB(Operand *opnd, BB *intoBb, BB *outofBb);
  bool LiveOutOfBB(Operand *opnd, BB *bb);
  void RemoveInsn(InsnInfo *insninfo);
  void RemoveUses(int32_t opndnum, OpndInfo **origInfo);
  void HashOpnd(Operand *opnd, OpndInfo *opndinfo);
  void HashInsn(Insn *insn, OpndInfo **origInfo, OpndInfo **opndInfo);
  bool BuildOperandInfo(BB *bb);
  InsnInfo *LocateInsnInfo(OpndInfo *info);
  void RemoveUnusedInsns(BB *bb, bool normal);
  void BackupOpndInfoList(OpndInfo *saveLast);
  void BackupInsnInfoList(InsnInfo *saveLast);
  void AddBB2EB(BB *bb);
  void EboInit();
  void EboProcessSingleBB();
  void EboProcess();
  void Run();
  const char *PhaseName() const {
    return phaseName;
  }
};

CGFUNCPHASE_CANSKIP(CgDoEbo, "ebo")
CGFUNCPHASE_CANSKIP(CgDoEbo1, "ebo1")
CGFUNCPHASE_CANSKIP(CgDoPostEbo, "postebo")
}  // namespace maplebe
#endif
