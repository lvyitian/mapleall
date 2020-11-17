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

#ifndef AARCH64COLORRA_H
#define AARCH64COLORRA_H
#include "reg_alloc.h"
#include "aarch64_operand.h"
#include "aarch64_insn.h"
#include "aarch64_abi.h"
#include "loop.h"

namespace maplebe {

#define RESERVED_REGS

#undef DO_PRE_LRA
#define USE_LRA
#define USE_SPLIT
#undef USE_BB_FREQUENCY
#define OPTIMIZE_FOR_PROLOG
#define REUSE_SPILLMEM
#define REMOVE_INSN
#define COLOR_SPLIT
#undef MOVE_COALESCE
#define PROPAGATE_REG

// for robust test
#undef CONSISTENT_MEMOPND
#undef RANDOM_PRIORITY

#define CLEAR_BIT_ARR_ELEM(vec, buckets)   \
  {                                        \
    for (uint32 i = 0; i < buckets; i++) { \
      (vec)[i] = 0LL;                      \
    }                                      \
  }

#define SET_BIT_ARR_ELEM(vec, num)                      \
  {                                                     \
    uint32 index = (num) / (sizeof(uint64) * CHAR_BIT); \
    uint64 bit = (num) % (sizeof(uint64) * CHAR_BIT);   \
    ((vec)[index] |= (1LL << bit));                     \
  }

#define SET_MEMBER_BIT_ARR_ELEM(lr, num)                \
  {                                                     \
    uint32 index = (num) / (sizeof(uint64) * CHAR_BIT); \
    uint64 bit = (num) % (sizeof(uint64) * CHAR_BIT);   \
    uint64 mask = 1LL << bit;                           \
    if (((lr)->bmember[index] & mask) == 0) {           \
      (lr)->numBmembers++;                              \
      (lr)->bmember[index] |= mask;                     \
    }                                                   \
  }

#define SET_CONFLICT_BIT_ARR_ELEM(lr, num)              \
  {                                                     \
    uint32 index = (num) / (sizeof(uint64) * CHAR_BIT); \
    uint64 bit = (num) % (sizeof(uint64) * CHAR_BIT);   \
    uint64 mask = 1LL << bit;                           \
    if (((lr)->bconflict[index] & mask) == 0) {         \
      (lr)->numBconflicts++;                            \
      (lr)->bconflict[index] |= mask;                   \
    }                                                   \
  }

#define UNSET_BIT_ARR_ELEM(vec, num)                    \
  {                                                     \
    uint32 index = (num) / (sizeof(uint64) * CHAR_BIT); \
    uint64 bit = (num) % (sizeof(uint64) * CHAR_BIT);   \
    ((vec)[index] &= (~(1LL << bit)));                  \
  }

#define UNSET_MEMBER_BIT_ARR_ELEM(lr, num)              \
  {                                                     \
    uint32 index = (num) / (sizeof(uint64) * CHAR_BIT); \
    uint64 bit = (num) % (sizeof(uint64) * CHAR_BIT);   \
    uint64 mask = 1LL << bit;                           \
    if ((lr)->bmember[index] & mask) {                  \
      (lr)->numBmembers--;                              \
      (lr)->bmember[index] &= (~mask);                  \
    }                                                   \
  }

#define UNSET_CONFLICT_BIT_ARR_ELEM(lr, num)            \
  {                                                     \
    uint32 index = (num) / (sizeof(uint64) * CHAR_BIT); \
    uint64 bit = (num) % (sizeof(uint64) * CHAR_BIT);   \
    uint64 mask = 1LL << bit;                           \
    if ((lr)->bconflict[index] & mask) {                \
      (lr)->numBconflicts--;                            \
      (lr)->bconflict[index] &= (~mask);                \
    }                                                   \
  }

#define IS_BIT_ARR_ELEM_SET(vec, num)                   \
  ({                                                    \
    uint32 index = (num) / (sizeof(uint64) * CHAR_BIT); \
    uint64 bit = (num) % (sizeof(uint64) * CHAR_BIT);   \
    bool isSet = ((vec)[index] & (1LL << bit));         \
    isSet;                                              \
  })

#define BIT_ARR_AND(vec1, vec2, result, buckets) \
  {                                              \
    for (uint32 i = 0; i < buckets; i++) {       \
      (result)[i] = (vec1)[i] & (vec2)[i];       \
    }                                            \
  }

#define BIT_ARR_OR(vec1, vec2, result, buckets) \
  {                                             \
    for (uint32 i = 0; i < buckets; i++) {      \
      (result)[i] = (vec1)[i] | (vec2)[i];      \
    }                                           \
  }

#define IS_BIT_ARR_EMPTY(vec)                \
  ({                                         \
    bool isEmpty = true;                     \
    for (uint32 i = 0; i < bbBuckets; i++) { \
      if ((vec)[i]) {                        \
        isEmpty = false;                     \
        break;                               \
      }                                      \
    }                                        \
    isEmpty;                                 \
  })

#define SET_BB_OVERLAP(vec1, vec2)           \
  ({                                         \
    bool isOverlap = false;                  \
    for (uint32 i = 0; i < bbBuckets; i++) { \
      if (((vec1)[i] & (vec2)[i]) != 0) {    \
        isOverlap = true;                    \
        break;                               \
      }                                      \
    }                                        \
    isOverlap;                               \
  })

#define COPY_BIT_ARR_ELEM(to, from, buckets) \
  {                                          \
    for (uint32 i = 0; i < buckets; i++) {   \
      to[i] = from[i];                       \
    }                                        \
  }

#define FOREACH_BB_ARR_ELEM(vec, num)                            \
  {                                                              \
    uint32 num;                                                  \
    for (uint32 i = 0; i < bbBuckets; i++) {                     \
      if ((vec)[i] == 0) continue;                               \
      for (uint32 b = 0; b < (sizeof(uint64) * CHAR_BIT); b++) { \
        if ((vec)[i] & (1LL << b)) {                             \
          num = i * sizeof(uint64) * CHAR_BIT + b;

#define END_FOREACH_BB_ARR_ELEM \
        }                       \
      }                         \
    }                           \
  }

#define FOREACH_REG_ARR_ELEM(vec, num)                           \
  {                                                              \
    regno_t num;                                                 \
    for (uint32 i = 0; i < regBuckets; i++) {                    \
      if ((vec)[i] == 0) continue;                               \
      for (uint32 b = 0; b < (sizeof(uint64) * CHAR_BIT); b++) { \
        if ((vec)[i] & (1LL << b)) {                             \
          num = i * sizeof(uint64) * CHAR_BIT + b;

#define END_FOREACH_REG_ARR_ELEM \
        }                        \
      }                          \
    }                            \
  }

#define FOREACH_REG_ARR_ELEM_NOT_SET(vec, num)                   \
  {                                                              \
    regno_t num;                                                 \
    for (uint32 i = 0; i < regBuckets; i++) {                    \
      for (uint32 b = 0; b < (sizeof(uint64) * CHAR_BIT); b++) { \
        if (!((vec)[i] & (1LL << b))) {                          \
          num = i * sizeof(uint64) * CHAR_BIT + b;

#define END_FOREACH_REG_ARR_ELEM_NOT_SET \
        }                                \
      }                                  \
    }                                    \
  }

#define FIND(set, item) (find((set).begin(), (set).end(), item))

#define FIND_NOT_IN(set, item) (FIND(set, item) == (set).end())

#define FIND_IN(set, item) (FIND(set, item) != (set).end())

// This is per bb per LR.
// LU info is particular to a bb in a LR.
class LiveUnit {
 public:
  uint32 bgn;    // first encounter in bb
  uint32 end;    // last encounter in bb
  bool hasCall;  // bb has a call
  uint32 defNum;
  uint32 useNum;  // used for priority calculation
  bool needRlod;
  bool needRstr;

  LiveUnit() : bgn(0), end(0), hasCall(false), defNum(0), useNum(0), needRlod(false), needRstr(false) {}

  void PrintLiveUnit() const;
};

struct SetBBCmpFunc {
  bool operator()(const BB *lhs, const BB *rhs) const {
    return (lhs->id < rhs->id);
  }
};

struct SortedBBCmpFunc {
  bool operator()(const BB *lhs, const BB *rhs) const {
    return (lhs->level < rhs->level);
  }
};

// LR is for each global vreg.
class LiveRange {
 public:
  MapleAllocator *allocator;
  regno_t regno;
  uint32 id;         // for priority tie breaker
  regno_t assigned;  // color assigned
  uint32 numCall;
  RegType regtype;
  float priority;
  uint32 numBmembers;  // number of bits set in bmember
  uint64 *bmember;     // Same as smember, but use bit array
                       // bit vector of array for each vreg's lr
                       // bit_vector[bb->id] = 1 if vreg is live in bb
  MapleVector<bool> pregveto;               // pregs cannot be assigned   -- SplitLr may clear forbidden
  MapleVector<bool> forbidden;              // pregs cannot be assigned
  uint32 numBconflicts;                     // number of bits set in bconflict
  uint32 numPregveto;
  uint32 numForbidden;
  uint64 *bconflict;                        // vreg interference from graph neighbors (bit)
  uint64 *oldConflict;
  MapleSet<regno_t> prefs;             // pregs that prefer
  MapleMap<uint32, LiveUnit *> luMap;  // info for each bb
  LiveRange *splitLr;                  // The 1st part of the split
#ifdef OPTIMIZE_FOR_PROLOG
  uint32 numDefs;
  uint32 numUses;
  uint32 frequency;
#endif                   // OPTIMIZE_FOR_PROLOG
  MemOperand *spillMem;  // memory operand used for spill, if any
  regno_t spillReg;      // register operand for spill at current point
  uint32 spillSize;      // 32 or 64 bit spill

  bool spilled;  // color assigned
  bool isNonLocal;

  explicit LiveRange(MapleAllocator *mallocator)
      : allocator(mallocator),
        regno(0),
        id(0),
        assigned(0),
        numCall(0),
        regtype(kRegTyUndef),
        priority(0.0),
        numBmembers(0),
        bmember(nullptr),
        pregveto(mallocator->Adapter()),
        forbidden(mallocator->Adapter()),
        numBconflicts(0),
        numPregveto(0),
        numForbidden(0),
        bconflict(nullptr),
        oldConflict(nullptr),
        prefs(mallocator->Adapter()),
        luMap(mallocator->Adapter()),
        splitLr(nullptr),
#ifdef OPTIMIZE_FOR_PROLOG
        numDefs(0),
        numUses(0),
        frequency(0),
#endif  // OPTIMIZE_FOR_PROLOG
        spillMem(nullptr),
        spillReg(0),
        spillSize(0),
        spilled(false),
        isNonLocal(false) {
  }

  void insertPregveto(regno_t reg) {
    if (pregveto[reg] == false) {
      numPregveto++;
      pregveto[reg] = true;
    }
  }

  void insertForbidden(regno_t reg) {
    if (forbidden[reg] == false) {
      numForbidden++;
      forbidden[reg] = true;
    }
  }
};

// One per bb, to communicate local usage to global RA
class LocalRaInfo {
 public:
  MapleAllocator *allocator;
  MapleMap<regno_t, uint16> defCnt;
  MapleMap<regno_t, uint16> useCnt;

  explicit LocalRaInfo(MapleAllocator *mallocator)
      : allocator(mallocator),
        defCnt(mallocator->Adapter()),
        useCnt(mallocator->Adapter()) {}
};

// For each bb, record info pertain to allocation
class BbAssignInfo {
 public:
  MapleAllocator *allocator;
  MapleVector<bool> globalsAssigned;  // globals used in a bb
  MapleMap<regno_t, regno_t> regMap;  // local vreg to preg mapping
  uint32 intLocalRegsNeeded;          // num local reg needs for each bb
  uint32 fpLocalRegsNeeded;           // num local reg needs for each bb
  uint32 numGlobalsAssigned;

  explicit BbAssignInfo(MapleAllocator *mallocator)
      : allocator(mallocator),
        globalsAssigned(mallocator->Adapter()),
        regMap(mallocator->Adapter()),
        intLocalRegsNeeded(0),
        fpLocalRegsNeeded(0),
        numGlobalsAssigned(0) {}

  void insertGlobalsAssigned(regno_t reg) {
    if (globalsAssigned[reg] == false) {
      numGlobalsAssigned++;
      globalsAssigned[reg] = true;
    }
  }
};

class FinalizeRegisterInfo {
 public:
  MapleAllocator *allocator;
  int32_t memOperandIdx;
  Operand *baseOperand;
  Operand *offsetOperand;
  MapleVector<Operand *> defOperands;
  MapleVector<int32_t> defIdx;
  MapleVector<Operand *> useOperands;
  MapleVector<int32_t> useIdx;

  FinalizeRegisterInfo(MapleAllocator *mallocator)
      : allocator(mallocator),
        memOperandIdx(0),
        baseOperand(nullptr),
        offsetOperand(nullptr),
        defOperands(mallocator->Adapter()),
        defIdx(mallocator->Adapter()),
        useOperands(mallocator->Adapter()),
        useIdx(mallocator->Adapter()) {}

  void ClearInfo() {
    memOperandIdx = 0;
    baseOperand = nullptr;
    offsetOperand = nullptr;
    defOperands.clear();
    defIdx.clear();
    useOperands.clear();
    useIdx.clear();
  }

  void SetBaseOperand(Operand *opnd, const int32_t idx) {
    baseOperand = opnd;
    memOperandIdx = idx;
  }

  void SetOffsetOperand(Operand *opnd) {
    offsetOperand = opnd;
  }

  void SetDefOperand(Operand *opnd, const int32_t idx) {
    defOperands.push_back(opnd);
    defIdx.push_back(idx);
  }

  void SetUseOperand(Operand *opnd, const int32_t idx) {
    useOperands.push_back(opnd);
    useIdx.push_back(idx);
  }
};

#define FOREACH_LRA_REGS(isInt, regno, localRa)                                        \
  {                                                                                    \
    for ((regno) = ((isInt) ? R0 : V0); (regno) <= ((isInt) ? RFP : V32); (regno)++) { \
      if (localRa->IsInRegs(regno, isInt) == false) {                                  \
          continue;                                                                    \
      }

#define END_FOREACH_LRA_REGS \
    }                        \
  }

class LocalRegAllocator {
 public:
  // The following local vars keeps track of allocation information in bb.
  uint64 *intRegAssigned;  // in this set if vreg is assigned
  uint64 *fpRegAssigned;
  MapleMap<regno_t, regno_t> intRegAssignmentMap;  // vreg -> preg map, which preg is the vreg assigned
  MapleMap<regno_t, regno_t> fpRegAssignmentMap;
  uint64 intRegUsed;  // pregs used in bb
  uint64 fpRegUsed;
  uint64 *intRegSpilled;  // on this list if spilled
  uint64 *fpRegSpilled;

  uint64 intRegs;  // available regs for assignement
  uint64 fpRegs;
  MapleMap<regno_t, uint16> useInfo;  // copy of local ra info for useCnt

  uint32 numIntRegUsed;
  uint32 numFpRegUsed;
  uint32 buckets;

  LocalRegAllocator(CGFunc *cgfunc, MapleAllocator *mallocator)
      : intRegAssignmentMap(mallocator->Adapter()),
        fpRegAssignmentMap(mallocator->Adapter()),
        intRegUsed(0),
        fpRegUsed(0),
        intRegs(0),
        fpRegs(0),
        useInfo(mallocator->Adapter()),
        numIntRegUsed(0),
        numFpRegUsed(0) {
    buckets = (cgfunc->GetMaxRegNum() / (sizeof(uint64) * CHAR_BIT)) + 1;
    intRegAssigned = cgfunc->memPool->NewArray<uint64>(buckets);
    fpRegAssigned = cgfunc->memPool->NewArray<uint64>(buckets);
    intRegSpilled = cgfunc->memPool->NewArray<uint64>(buckets);
    fpRegSpilled = cgfunc->memPool->NewArray<uint64>(buckets);
  }

  void ClearLocalRaInfo() {
    CLEAR_BIT_ARR_ELEM(intRegAssigned, buckets);
    CLEAR_BIT_ARR_ELEM(fpRegAssigned, buckets);
    intRegAssignmentMap.clear();
    fpRegAssignmentMap.clear();
    intRegUsed = 0;
    fpRegUsed = 0;
    CLEAR_BIT_ARR_ELEM(intRegSpilled, buckets);
    CLEAR_BIT_ARR_ELEM(fpRegSpilled, buckets);
    numIntRegUsed = 0;
    numFpRegUsed = 0;
  }

  regno_t RegBaseUpdate(bool isInt) {
    if (isInt) {
      return 0;
    } else {
      return V0 - R0;
    }
  }

  bool isInRegAssigned(regno_t regno, const bool isInt) {
    bool isSet;
    if (isInt) {
      isSet = IS_BIT_ARR_ELEM_SET(intRegAssigned, regno);
    } else {
      isSet = IS_BIT_ARR_ELEM_SET(fpRegAssigned, regno);
    }
    return isSet;
  }

  void SetRegAssigned(regno_t regno, const bool isInt) {
    if (isInt) {
      SET_BIT_ARR_ELEM(intRegAssigned, regno);
    } else {
      SET_BIT_ARR_ELEM(fpRegAssigned, regno);
    }
  }

  MapleMap<regno_t, regno_t> *GetRegAssignmentMap(const bool isInt) {
    if (isInt) {
      return &intRegAssignmentMap;
    } else {
      return &fpRegAssignmentMap;
    }
  }

  uint64 GetRegUsed(const bool isInt) {
    if (isInt) {
      return intRegUsed;
    } else {
      return fpRegUsed;
    }
  }

  void SetRegUsed(regno_t regno, const bool isInt) {
    if (isInt) {
      uint64 mask = 1LL << (regno - R0);
      if ((intRegUsed & mask) == 0) {
        numIntRegUsed++;
        intRegUsed |= mask;
      }
    } else {
      uint64 mask = 1LL << (regno - V0);
      if ((fpRegUsed & mask) == 0) {
        numFpRegUsed++;
        fpRegUsed |= mask;
      }
    }
  }

  bool isInRegSpilled(regno_t regno, const bool isInt) {
    bool isSet;
    if (isInt) {
      isSet = IS_BIT_ARR_ELEM_SET(intRegSpilled, regno);
    } else {
      isSet = IS_BIT_ARR_ELEM_SET(fpRegSpilled, regno);
    }
    return isSet;
  }

  void SetRegSpilled(regno_t regno, const bool isInt) {
    if (isInt) {
      SET_BIT_ARR_ELEM(intRegSpilled, regno);
    } else {
      SET_BIT_ARR_ELEM(fpRegSpilled, regno);
    }
  }

  uint64 GetRegs(const bool isInt) {
    if (isInt) {
      return intRegs;
    } else {
      return fpRegs;
    }
  }

  void SetRegs(regno_t regno, const bool isInt) {
    if (isInt) {
      intRegs |= (1LL << (regno - RegBaseUpdate(true)));
    } else {
      fpRegs |= (1LL << (regno - RegBaseUpdate(false)));
    }
  }

  void ClearRegs(regno_t regno, const bool isInt) {
    if (isInt) {
      intRegs &= (~(1LL << (regno - RegBaseUpdate(true))));
    } else {
      fpRegs &= (~(1LL << (regno - RegBaseUpdate(false))));
    }
  }

  bool IsInRegs(regno_t regno, const bool isInt) {
    bool isSet;
    if (isInt) {
      isSet = (intRegs & (1LL << (regno - RegBaseUpdate(true))));
    } else {
      isSet = (fpRegs & (1LL << (regno - RegBaseUpdate(false))));
    }
    return isSet;
  }

  void InitRegs(uint32 intMax, uint32 fpMax, bool hasYield) {
    intRegs = (((1LL << (R0)) - 1) ^ ((1LL << (intMax + 1)) - 1));
    if (hasYield) {
      intRegs &= (~(1 << R19));
    }
#ifdef RESERVED_REGS
    intRegs &= (~(1 << R16));
    intRegs &= (~(1 << R17));
#endif  // RESERVED_REGS
    fpRegs = (((1LL << (V0 - RegBaseUpdate(false))) - 1) ^ ((1LL << (((fpMax + 1) + V0) - RegBaseUpdate(false))) - 1));
  }
};

class SplitBbInfo {
 public:
  SplitBbInfo() : candidateBb(nullptr), startBb(nullptr) {}

  inline BB *GetCandidateBb() const {
    return candidateBb;
  }

  inline BB *GetStartBb() const {
    return startBb;
  }

  inline void SetCandidateBb(BB *bb) {
    candidateBb = bb;
  }

  inline void SetStartBb(BB *bb) {
    startBb = bb;
  }

 private:
  BB *candidateBb;
  BB *startBb;
};

class GraphColorRegAllocator : public RegAllocator {
 public:
  GraphColorRegAllocator(CGFunc *cgfunc, MapleAllocator *mallocator)
      : RegAllocator(cgfunc),
        allocator(mallocator),
        visitedBBs(mallocator->Adapter()),
        sortedBBs(mallocator->Adapter()),
        bbVec(mallocator->Adapter()),
        vregLive(mallocator->Adapter()),
        pregLive(mallocator->Adapter()),
        lrVec(mallocator->Adapter()),
        localRegVec(mallocator->Adapter()),
        bbRegInfo(mallocator->Adapter()),
        unconstrained(mallocator->Adapter()),
        constrained(mallocator->Adapter()),
#ifdef OPTIMIZE_FOR_PROLOG
        intDelayed(mallocator->Adapter()),
        fpDelayed(mallocator->Adapter()),
#endif  // OPTIMIZE_FOR_PROLOG
        intCallerRegSet(mallocator->Adapter()),
        intCalleeRegSet(mallocator->Adapter()),
        fpCallerRegSet(mallocator->Adapter()),
        fpCalleeRegSet(mallocator->Adapter()),
        intCalleeUsed(mallocator->Adapter()),
        fpCalleeUsed(mallocator->Adapter()),
        bbBuckets(0),
        regBuckets(0),
        spillMemopnd0(nullptr),
        spillMemopnd1(nullptr),
        spillMemopnd2(nullptr),
        doMultiPass(false),
#ifdef USE_LRA
        doLRA(true),
#else
        doLRA(false);,
#endif // USE_LRA
#ifdef OPTIMIZE_FOR_PROLOG
        doOptProlog(true),
#else
        doOptProlog(false),
#endif // OPTIMIZE_FOR_PROLOG
        hasSpill(false) {
    intSpillFillRegs[0] = intSpillFillRegs[1] = intSpillFillRegs[2] = 0;
    fpSpillFillRegs[0] = fpSpillFillRegs[1] = fpSpillFillRegs[2] = 0;
    numVregs = cgfunc_->GetMaxVReg();
    lrVec.resize(numVregs);
    localRegVec.resize(cgfunc_->NumBBs());
    bbRegInfo.resize(cgfunc_->NumBBs());
    Operand *rflag = cgfunc_->GetOrCreateRflag();
    ccReg = (static_cast<RegOperand *>(cgfunc_->GetOrCreateRflag()))->GetRegisterNumber();
  }

  ~GraphColorRegAllocator() override {}

  bool AllocateRegisters() override;
  const char *PhaseName() {
    return "regalloc";
  }

 private:
  const uint16 kLargeUint16 = 0x7fff;
  enum { kNoDelete, kDelayedDelete, kImmediateDelete};

  struct SetLiveRangeCmpFunc {
    bool operator()(const LiveRange *lhs, const LiveRange *rhs) const {
      if (lhs->priority == rhs->priority) {
        // This is to ensure the ordering is consistent as the reg#
        // differs going through VtableImpl.mpl file.
        if (lhs->id == rhs->id) {
          return lhs->regno < rhs->regno;
        } else {
          return lhs->id < rhs->id;
        }
      }
      return (lhs->priority > rhs->priority);
    }
  };

  MapleAllocator *allocator;
  MapleVector<bool> visitedBBs;
  MapleVector<BB *> sortedBBs;
  MapleVector<BB *> bbVec;
  MapleSet<regno_t> vregLive;
  MapleSet<regno_t> pregLive;
  MapleVector<LiveRange *> lrVec;
  MapleVector<LocalRaInfo *> localRegVec;  // local reg info for each bb, no local reg if null
  MapleVector<BbAssignInfo *> bbRegInfo;   // register assignment info for each bb
  MapleVector<LiveRange *> unconstrained;
  MapleVector<LiveRange *> constrained;
#ifdef OPTIMIZE_FOR_PROLOG
  MapleVector<LiveRange *> intDelayed;
  MapleVector<LiveRange *> fpDelayed;
#endif                               // OPTIMIZE_FOR_PROLOG
  MapleSet<uint32> intCallerRegSet;  // integer caller saved
  MapleSet<uint32> intCalleeRegSet;  //         callee
  MapleSet<uint32> fpCallerRegSet;   // float caller saved
  MapleSet<uint32> fpCalleeRegSet;   //       callee
  MapleSet<regno_t> intCalleeUsed;
  MapleSet<regno_t> fpCalleeUsed;

  uint32 bbBuckets;   // size of bit array for bb (each bucket == 64 bits)
  uint32 regBuckets;  // size of bit array for reg (each bucket == 64 bits)
  uint32 intRegNum;   // total available int preg
  uint32 fpRegNum;    // total available fp preg
  uint32 numVregs;    // number of vregs when starting
  regno_t ccReg;
  // For spilling of spill register if there are none available
  //   Example, all 3 operands spilled
  //                          sp_reg1 -> [spillMemopnd1]
  //                          sp_reg2 -> [spillMemopnd2]
  //                          ld sp_reg1 <- [addr-reg2]
  //                          ld sp_reg2 <- [addr-reg3]
  //   reg1 <- reg2, reg3     sp_reg1 <- sp_reg1, sp_reg2
  //                          st sp_reg1 -> [addr-reg1]
  //                          sp_reg1 <- [spillMemopnd1]
  //                          sp_reg2 <- [spillMemopnd2]
  MemOperand *spillMemopnd0;
  MemOperand *spillMemopnd1;
  MemOperand *spillMemopnd2;

  regno_t intSpillFillRegs[3];
  regno_t fpSpillFillRegs[3];

  bool doMultiPass;
  bool doLRA;
  bool doOptProlog;
  bool hasSpill;

  void PrintLiveUnitMap(const LiveRange *lr) const;
  void PrintLiveRangeConflicts(const LiveRange *lr) const;
  void PrintLiveBbBit(const LiveRange *li) const;
  void PrintLiveRange(const LiveRange *li, const string str) const;
  void PrintLiveRanges() const;
  void PrintLocalRaInfo(const string str) const;
  void PrintBbAssignInfo() const;
  void PrintBBs() const;

  bool AllPredBBVisited(BB *);
  BB *MarkStraightLineBBInBFS(BB *);
  BB *SearchForStraightLineBBs(BB *);
  void BFS(BB *);
  void ComputeBlockOrder();
  uint32 MaxIntPhysRegNum();
  uint32 MaxFloatPhysRegNum();
  regno_t StartingIntCallerReg();
  regno_t EndingIntCallerReg();
  regno_t StartingIntCalleeReg();
  regno_t EndingIntCalleeReg();
  regno_t StartingFpCallerReg();
  regno_t EndingFpCallerReg();
  regno_t StartingFpCalleeReg();
  regno_t EndingFpCalleeReg();
  void GetAvailablePRregs(RegType regty, uint32 &begin, uint32 &end, bool isCaller);
  void InitFreeRegPool();
  bool IsUnconcernedReg(regno_t regno);
  bool IsUnconcernedReg(RegOperand *regOpnd);
  LiveRange *NewLiveRange();
  void CalculatePriority(LiveRange *lr);
  bool CreateLiveRangeHandleLocal(regno_t regno, BB *bb, bool isDef);
  LiveRange *CreateLiveRangeAllocateAndUpdate(regno_t regno, BB *bb, bool isDef, uint32 currId);
  bool CreateLiveRange(regno_t regno, BB *bb, bool isDef, bool isMovk, uint32 currPoint, bool update_cnt);
  bool SetupLiveRangeByOpHandlePhysicalReg(RegOperand *op, Insn *insn, regno_t regno, bool isDef);
  uint8 SetupLiveRangeByOp(Operand *op, Insn *insn, bool isDef);
  void SetupLiveRangeByRegno(regno_t regno, BB *bb, uint32 currPoint);
  void ComputeLiveRangesForEachOperand(Insn *insn);
  void ComputeLiveRangesUpdateIfInsnIsCall(Insn *insn);
  void ComputeLiveRangesUpdateLiveUnitInsnRange(BB *bb, uint32 currPoint);
  void ComputeLiveRangesBmemberSize();
  void ComputeLiveOut(BB *bb);
  void ComputeLiveRanges();
  MemOperand *CreateSpillMem(uint32 spillIdx);
  void CheckInterference(LiveRange *lr1, LiveRange *lr2);
  void BuildInterferenceGraphSeparateIntFp(std::vector<LiveRange *> &intLrVec, std::vector<LiveRange *> &fpLrVec);
  void BuildInterferenceGraph();
  void SetBbInfoGlobalAssigned(uint32 bbid, regno_t regno);
  bool HaveAvailableColor(LiveRange *lr, uint32 num);
  void Separate();
  void SplitAndColor();
  void ColorForOptPrologEpilog();
  void SpillLiveRange();
  void ChooseColorForSpill(LiveRange *lr);
  void SpillAll();
  bool IsLocalReg(regno_t regno);
  bool IsLocalReg(LiveRange *lr);
  void HandleLocalRaDebug(regno_t regno, LocalRegAllocator *localRa, bool isInt);
  void HandleLocalRegAssignment(regno_t regno, LocalRegAllocator *localRa, bool isInt);
  bool HandleLocalRegDefWithNoUse(regno_t regno, LocalRegAllocator *localRa, bool isInt, bool isDef);
  void HandleLocalReg(Operand *op, LocalRegAllocator *localRa, BbAssignInfo *bbInfo, bool isDef, bool isInt);
  void LocalRaRegSetEraseReg(LocalRegAllocator *localRa, regno_t regno);
  bool LocalRaInitRegSet(LocalRegAllocator *localRa, uint32 bbid, bool doAllocate);
  void LocalRaInitAllocatableRegs(LocalRegAllocator *localRa, uint32 bbid, bool doAllocate);
  void LocalRaPrepareBb(BB *bb, LocalRegAllocator *localRa);
  void LocalRaFinalAssignment(LocalRegAllocator *localRa, BbAssignInfo *bbInfo);
  void LocalRaDebug(BB *bb, LocalRegAllocator *localRa);
  void LocalRegisterAllocator(bool allocate);
  MemOperand *GetSpillOrReuseMem(LiveRange *lr, uint32 regsize, uint8 &isOutOfRange, Insn *insn, bool isDef);
  void SpillOperandForSpillPre(Insn *insn, Operand *opnd, RegOperand *phyOpnd, uint32 spillIdx, bool needSpill);
  void SpillOperandForSpillPost(Insn *insn, Operand *opnd, RegOperand *phyOpnd, uint32 spillIdx, bool needSpill);
  Insn *SpillOperand(Insn *insn, Operand *opnd, bool isDef, RegOperand *phyOpnd, uint32 &spillIdx);
  MemOperand *GetConsistentReuseMem(uint32 vregno, uint32 size, RegType regtype);
  MemOperand *GetCommonReuseMem(uint32 vregno, uint32 size, RegType regtype);
  MemOperand *GetReuseMem(uint32 vregno, uint32 size, RegType regtype);
  MemOperand *GetSpillMem(uint32 vregno, uint8 isDest, Insn *insn, AArch64reg_t regno, uint8 &isOutOfRange);
  bool GetAvailableSpillReg(set<regno_t> &cannotUseReg, LiveRange *lr, uint64 &usedRegMask);
  void CollectCannotUseReg(set<regno_t> &cannotUseReg, LiveRange *lr, Insn *insn);
  bool PickRegForSpill(LiveRange *lr, Insn *insn, uint64 &usedRegMask, bool isDef);
  regno_t PickRegForLocalSpill(regno_t regno, RegType regtype, uint64 &usedRegMask);
  bool GetSpillReg(Insn *insn, LiveRange *lr, uint64 &usedRegMask, bool isDef);
  RegOperand *GetReplaceOpndForLRA(Insn *insn, Operand *opnd, uint32 &spillIdx, uint64 &usedRegMask, bool isDef);
  RegOperand *GetReplaceOpnd(Insn *insn, Operand *opnd, uint32 &spillIdx, uint64 &usedRegMask, bool isDef);
  void MarkCalleeSaveRegs();
  void MarkUsedRegs(Operand *opnd, uint64 &usedRegMask);
  uint64 FinalizeRegisterPreprocess(FinalizeRegisterInfo *fInfo, Insn *insn);
  void GenerateSpillFillRegs(Insn *insn);
  RegOperand *CreateSpillFillCode(RegOperand *opnd, Insn *insn, uint32 spillCnt, bool isdef = false);
  void SpillLiveRangeForSpills();
  void FinalizeRegisters();

  MapleVector<LiveRange *>::iterator GetHighPriorityLr(MapleVector<LiveRange *> &lrSet);
  void UpdateForbiddenForNeighbors(LiveRange *lr);
  void UpdatePregvetoForNeighbors(LiveRange *lr);
  regno_t FindColorForLr(LiveRange *lr);
  bool ShouldUseCallee(LiveRange *lr, MapleSet<regno_t> &calleeUsed, MapleVector<LiveRange *> &delayed);
  bool AssignColorToLr(LiveRange *lr, bool isDelayed = false);
  void PruneLrForSplit(LiveRange *lr, BB *bb, bool remove, set<CgfuncLoops *> &candidateInLoop,
                       set<CgfuncLoops *> &defInLoop);
  bool UseIsUncovered(BB *bb, BB *startBb);
  void FindUseForSplit(LiveRange *lr, SplitBbInfo &bbInfo, bool &remove, set<CgfuncLoops *> &candidateInLoop,
                       set<CgfuncLoops *> &defInLoop);
  void FindBbSharedInSplit(LiveRange *lr, set<CgfuncLoops *> &candidateInLoop, set<CgfuncLoops *> &defInLoop);
  void ComputeBbForNewSplit(LiveRange *newLr, LiveRange *oldLr);
  void ClearLrBbFlags(set<BB *, SortedBBCmpFunc> &member);
  void ComputeBbForOldSplit(LiveRange *newLr, LiveRange *oldLr);
  bool LrCanBeColored(LiveRange *lr, BB *bbAdded, set<regno_t> &conflictRegs);
  void MoveLrBbInfo(LiveRange *oldLr, LiveRange *newLr, BB *bb);
  bool ContainsLoop(CgfuncLoops *loop, set<CgfuncLoops *> &loops);
  void GetAllLrMemberLoops(LiveRange *lr, set<CgfuncLoops *> &loop);
  bool SplitLrShouldSplit(LiveRange *lr);
  bool SplitLrIsProfitable(LiveRange *newLr);
  void ResetSplitLr(LiveRange *origLr, LiveRange *newLr);
  bool SplitLrFindCandidateLr(LiveRange *lr, LiveRange *newLr, set<regno_t> &conflictRegs);
  void SplitLrHandleLoops(LiveRange *lr, LiveRange *newLr, set<CgfuncLoops *> &oldLoops, set<CgfuncLoops *> &newLoops);
  void SplitLrFixNewLrCallsAndRlod(LiveRange *newLr, set<CgfuncLoops *> &origLoops);
  void SplitLrFixOrigLrCalls(LiveRange *lr);
  void SplitLrUpdateInterference(LiveRange *lr);
  void SplitLrUpdateRegInfo(LiveRange *origLr, LiveRange *newLr, set<regno_t> &conflictRegs);
  void SplitLrErrorCheckAndDebug(LiveRange *origLr, LiveRange *newLr);
  LiveRange *SplitLr(LiveRange *lr);
};

}  // namespace maplebe

#endif  // AARCH64COLORRA_H
