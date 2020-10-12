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

#ifndef MAPLEBE_INCLUDE_CG_INSN_H
#define MAPLEBE_INCLUDE_CG_INSN_H

#include "operand.h"
#include "cg_assert.h"
#include "deps.h"

/// Maple IR header
#include "types_def.h"  // for uint32

/// C++ headers
#include <cstddef>  // for nullptr
#include <string>

namespace maplebe {

typedef uint32 MOperator;

// forward declaration
class BB;
class CG;
class Emitter;
class SSAValue;
class Insn;
struct DataInsnInfoCmp;
struct DataInsnInfoCmp2;

class DataInsnInfo {
 public:
  static const unsigned short kAll = 0xFFFF;
  static const unsigned short kNormal = 0x0;
  static const unsigned short kSecondMemAddr = 0x1;  // Second memory address of load/store pair insns
  static const unsigned short kBaseReg = 0x2;        // Base register of the memory operand.
  static const unsigned short kIndexReg = 0x3;       // Index register of the memory operand.
  static const unsigned short kCallParam = 0x4;      // Only if insn is a call, and the operand is in param list.

  static const unsigned short kDirty =
    0x10;  // Only for memory address. Set 1 if memory may be changed through definition to use.
           // Only for EH block, true if one operand defined more than once in a single BB.
           // On D-U chain, it indicates current insn may define the operand of the use insn.
           // On U-D chain, it indicates the operand of current insn may be defined by def insn.
  static const unsigned short kMultigen = 0x20;
  static const unsigned short kMaydef = 0x40;   // Currently only for R0/R1's definition when exception occurs.
  static const unsigned short kPartdef = 0x80;  // Only for partial definition insns "movk/movz/movn".

  static const unsigned short kIndexQuickcalc = 0x3;  // For quick calculate index of definition and uses.
  static const unsigned short kAnyIndex = 0x0F;
  static const unsigned short kDataProp = 0xF0;
  static const unsigned short kAnyProp = 0xFF;

 private:
  Insn *insn;  // Definition/Use insn.

  // operand index of the insn.
  // -1 means hide destination such as return value R0/V0 for call insn.
  // For D-U chain, if (property & CALLPARAM) is true, index indicates the ith call argument.
  short index;
  unsigned short property;         // Each bit indicate a property.
  DataInsnInfo *prevMultiGenInsn;  // Indicate previous insnInfo which with multiGen property.
 public:
  DataInsnInfo(Insn *i, short idx) : insn(i), index(idx), property(0), prevMultiGenInsn(nullptr) {}

  DataInsnInfo(Insn *i, short idx, int prop) : insn(i), index(idx), property(prop), prevMultiGenInsn(nullptr) {}

  DataInsnInfo(Insn *i, short idx, int prop, DataInsnInfo *list)
    : insn(i), index(idx), property(prop), prevMultiGenInsn(list) {}

  DataInsnInfo *Clone(MemPool *mp) const {
    return mp->Clone<DataInsnInfo>(*this);
  }

  virtual ~DataInsnInfo() {}

  inline Insn *GetInsn() const {
    return insn;
  }

  inline void SetInsn(Insn *i) {
    insn = i;
  }

  inline void SetIndex(short i) {
    index = i;
  }

  inline short GetIndex() const {
    return index;
  }

  inline unsigned short GetProperty() const {
    return property;
  }

  inline void SetProperty(unsigned short p) {
    property |= p;
  }

  inline void ClearProperty(unsigned short p) {
    property &= (~p);
  }

  inline void ClearAllProperty() {
    property = kNormal;
  }

  inline bool IsSecondMemAddr() const {
    return property & kSecondMemAddr;
  }

  inline bool IsBaseRegister() const {
    return property & kBaseReg;
  }

  inline bool IsIndexRegister() const {
    return property & kIndexReg;
  }

  inline bool IsCallParam() const {
    return property & kCallParam;
  }

  inline bool IsDirty() const {
    return property & kDirty;
  }

  inline bool IsMulGen() const {
    return property & kMultigen;
  }

  inline bool IsMayDef() const {
    return property & kMaydef;
  }

  inline bool IsPartDef() const {
    return property & kPartdef;
  }

  inline bool HasDataProp() const {
    return property & kDataProp;
  }

  inline short AdditionalMemIndex() const {
    return property & kIndexQuickcalc;
  }

  inline short AdditionalCallIndex() const {
    return ((property & kCallParam) ? 1 : 0);
  }

  inline short AdditionalIndex() const {
    return (AdditionalMemIndex() + AdditionalCallIndex());
  }

  inline short GetDUIndex() const {
    return ((index < 0 ? 0 : index) + AdditionalMemIndex());
  }

  inline short GetUDIndex() const {
    return (index + AdditionalIndex());
  }

  inline void SetPrevMultiGenInsn(DataInsnInfo *insnInfo) {
    prevMultiGenInsn = insnInfo;
  }

  inline DataInsnInfo *GetPrevMultiGenInsn() const {
    return prevMultiGenInsn;
  }
};

class Insn {
 public:
  static const int kMaxOperandNum = 5;
  static const int kMaxCallParamNum = 16;  // 8 for integer registers, 8 for FP registers.
  static const int kMaxUseOperandNum =
    kMaxCallParamNum + 1;  // For call insns, it may have 1 call address and 8 parameters.
  static const int kMaxDefineOperandNum =
    3;  // Some insns have 3 dest operand, such as: ldp/stp with pre/post addressing.
  static const int kInvalidDfn = -1;
  static const MOperator kMopGhost = MOperator(-1);

 private:
  enum OpKind {
    kOpUnknown = 0,
    kOpCondDef = 0x1,
    kOpSpill = (1 << 1),
    kOpAccessRefField = (1 << 30),  // load-from/store-into a ref-field
    kOpDassignToSaveRetValToLocal = (1 << 31)
  };

 public:
  MOperator mop_;
  Operand *opnds[kMaxOperandNum];  // the preg operands

 public:
  enum RetType {
    kRegNull,   // no return type
    kRegFloat,  // return register is V0
    kRegInt     // return register is R0
  };
  Insn *prev, *next;
  BB *bb;        // BB to which this insn belongs
  uint32 flags;
  uint32 id;
  bool is_throw;
  bool do_not_remove;  // caller reg cross call
  bool is_unsigned;      // false: signed, true: unsigned
  RetType ret_type;    // if this insn is call, it represent the return register type R0/V0
                       // For use->def.
                       // Index: For call insns: 0 is the call address, 1-16 indicates 0th-15th call parameters.
                       //        For non call insns: index indicates the index of insn operand.
                       //        For ldp/stp memory operand: Suppose the memory operand is the ith operand of insn,
                       //            then index i indicates the first memory address.
                       //                 index i+1 indicates the second memory address.
                       //        For base register changed: Suppose the memory operand is the ith operand of insn,
                       //            then index i+2 indicates the base register.
  MapleSet<DataInsnInfo, DataInsnInfoCmp> *defs[kMaxUseOperandNum];

  // For def->use. RAW True dependence.
  // Index: 0 for 1st definition operand;
  //        1 for second definition, only for ldp/stp, either second register definition or second memory definition.
  //        2 for base register changed. only for load/store with pre/post index addressing.
  MapleSet<DataInsnInfo, DataInsnInfoCmp2> *uses[kMaxDefineOperandNum];

  // For def->redef.  WAW dependence.
  // Index: 0 for 1st definition operand;
  //        1 for second definition, only for ldp/stp, either second register definition or second memory definition.
  //        2 for base register changed. only for load/store with pre/post index addressing.
  // Note: call insn do not hae any redefine. Only indicates a boundary.
  MapleSet<DataInsnInfo, DataInsnInfoCmp> *redefine[kMaxDefineOperandNum];

  // For def->previousDef. WAW dependence. Reverse of redefine.
  // Example: mov rd, rs1   // Insn1
  //          mov rd, rs2   // Insn2
  //          For rd, Insn2's predefine is Insn1.
  // Index: 0 for 1st definition operand;
  //        1 for second definition, only for ldp/stp, either second register definition or second memory definition.
  //        2 for base register changed. only for load/store with pre/post index addressing.
  // Note: call insn do not hae any predefine. Only indicates a boundary.
  MapleSet<DataInsnInfo, DataInsnInfoCmp> *predefine[kMaxDefineOperandNum];

  uint32 retsize;  // Byte size of the return value if insn is a call.

  DepNode *depNode;  // For dependence analysis, pointing to a dependence node.

 private:
  std::string comment;

 public:
  explicit Insn(MOperator opc, Operand *opnd0 = nullptr, Operand *opnd1 = nullptr, Operand *opnd2 = nullptr,
                Operand *opnd3 = nullptr, Operand *opnd4 = nullptr)
    : mop_(opc),
      prev(nullptr),
      next(nullptr),
      bb(nullptr),
      flags(0),
      id(0),
      is_throw(false),
      do_not_remove(false),
      is_unsigned(false),
      ret_type(kRegNull),
      defs{ nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
            nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr },
      uses{ nullptr, nullptr, nullptr },
      redefine{ nullptr, nullptr, nullptr },
      predefine{ nullptr, nullptr, nullptr },
      retsize(0),  //   uses{maplebe::CG::curCgFunc->memPool->Adapter() },
      comment() {
    SetOperand(0, opnd0);
    SetOperand(1, opnd1);
    SetOperand(2, opnd2);
    SetOperand(3, opnd3);
    SetOperand(4, opnd4);

#if !RELEASE
    Check();
#endif
  }

  inline MOperator GetMachineOpcode() const {
    return mop_;
  }

  virtual MOperator GetMOPUndef() {
    return 0;
  }

  static MOperator GetMOPGhost() {
    return kMopGhost;
  }

  inline void SetMOP(MOperator mop) {
    mop_ = mop;
  }

  inline Operand *GetOperand(int i) {
    CG_ASSERT(i < kMaxOperandNum, "index out of range");
    return opnds[i];
  }

  inline void SetOperand(int i, Operand *o) {
    CG_ASSERT(i < kMaxOperandNum, "index out of range");
    opnds[i] = o;
  }

  inline void SetResult(int32_t i, Operand *res) {
    if (IsStore()) {
      opnds[GetOpndNum() + i] = res;
    } else {
      opnds[i] = res;
    }
  }

  inline void SetRetSignType(bool sign) {
    CG_ASSERT(IsCall(), "Insn should be a call.");
    is_unsigned = sign;
  }

  inline bool IsCallRetUnsigned(void) {
    CG_ASSERT(IsCall(), "Insn should be a call.");
    return is_unsigned;
  }

  inline void SetRetSize(uint32 size) {
    CG_ASSERT(IsCall(), "Insn should be a call.");
    retsize = size;
  }

  inline uint32 GetRetSize() {
    CG_ASSERT(IsCall(), "Insn should be a call.");
    return retsize;
  }

  virtual bool IsMachineInstruction() = 0;

  virtual bool IsPseudoInstruction() {
    return false;
  }

  virtual bool IsBarrier() {
    return false;
  }

  virtual bool AccessRegBank() {
    return false;
  }

  virtual bool IsEffectiveCopy() {
    return false;
  }

  virtual int32_t CopyOperands() {
    return -1;
  }

  virtual bool IsSameRes() {
    return false;
  }

  virtual int32_t GetOpndNum() {
    return 0;
  }

  virtual int32_t GetResultNum() {
    return 0;
  }

  virtual Operand *GetOpnd(int i) {
    return nullptr;
  }

  virtual Operand *GetMemOpnd() {
    return nullptr;
  }

  virtual Operand *GetResult(int i) {
    return nullptr;
  }

  virtual Operand *GetResultMemOpnd() {
    return nullptr;
  }

  virtual void SetOpnd(int32_t i, Operand *opnd){};
  virtual bool IsGlobal() {
    return false;
  }

  virtual bool IsCall() {
    return false;
  }

  virtual bool IsTailCall() const {
    return false;
  }

  virtual bool IsClinit() {
    return false;
  }

  virtual bool IsReturn() {
    return false;
  }

  virtual bool IsFixedInsn() {
    return false;
  }

  virtual bool CanThrow() {
    return false;
  }

  virtual bool MayThrow() {
    return false;
  }

  virtual bool IsIndirectCall() {
    return false;
  }

  virtual bool IsCallToFunctionThatNeverReturns() {
    return false;
  }

  virtual bool IsBranch() {
    return false;
  }

  virtual bool IsMove() {
    return false;
  }

  virtual bool IsLoad() {
    return false;
  }

  virtual bool IsStore() {
    return false;
  }

  virtual bool IsLoadPair() {
    return false;
  }

  virtual bool IsStorePair() {
    return false;
  }

  virtual bool IsLoadAddress() {
    return false;
  }

  virtual bool IsAtomic() {
    return false;
  }

  virtual bool NoAlias() {
    return false;
  }

  virtual bool IsVolatile() {
    return false;
  }

  virtual bool IsMemAccessBar() {
    return false;
  }

  virtual bool IsMemAccess() {
    return false;
  }

  virtual bool HasSideEffects() {
    return false;
  }

  virtual bool IsComment() {
    return false;
  }

  virtual bool IsImmaterialInsn() {
    return false;
  }

  virtual bool IsYieldpoint() {
    return false;
  }

  virtual bool IsPartDef() const {
    return false;
  }

  virtual bool IsCfiInsn() {
    return false;
  }

  virtual bool IsFallthruCall() {
    return false;
  }

  virtual bool IsDMBInsn() const {
    return false;
  }

  virtual Operand *GetCallTargetOperand() {
    return nullptr;
  }

  /* returns a ListOperand */
  /* Note that we don't really need this for Emit
   * Rather, we need it for register allocation, to
   * correctly state the live ranges for operands
   * use for passing call arguments
   */
  virtual ListOperand *GetCallArgumentOperand() {
    return nullptr;
  }

  bool IsAtomicStore() {
    return IsStore() && IsAtomic();
  }

  void SetCondDef() {
    flags |= kOpCondDef;
  }

  bool IsCondDef() const {
    return flags & kOpCondDef;
  }

  void SetSpillOp() {
    flags |= kOpSpill;
  }

  bool IsSpillOp() const {
    return flags & kOpSpill;
  }

  bool AccessMem() {
    return IsLoad() || IsStore();
  }

  virtual unsigned int GetUnitType() {
    return 0;
  }

  virtual void Emit(CG &, Emitter &) = 0;

  virtual void dump() = 0;

#if !RELEASE
  virtual bool Check() {
    return true;
  }

#else
  virtual bool Check() = 0;
#endif

  inline void AddComment(const std::string &s) {
    comment = s;
  }

  inline const char *GetComment() {
    return comment.c_str();
  }

  inline void MarkAsSaveRetValToLocal() {
    flags |= kOpDassignToSaveRetValToLocal;
  }

  inline bool IsSaveRetValToLocal() const {
    return ((flags & kOpDassignToSaveRetValToLocal) != 0);
  }

  inline void MarkAsAccessRefField(bool cond) {
    if (cond) {
      flags |= kOpAccessRefField;
    }
  }

  inline bool IsAccessRefField() const {
    return ((flags & kOpAccessRefField) != 0);
  }

  int GetSSADefIdx();

  virtual bool IsDefinition() const = 0;

  virtual bool IsDestRegAlsoSrcReg() const {
    return false;
  }

  virtual bool IsDataMoveInstruction() const = 0;

  virtual bool IsConversionInstruction() const = 0;

  virtual bool IsConditionalSet() const = 0;

  bool MayBeElidedInSSA(const SSANode *srcOperand);

  // SSA
  // Note: we consider those instructions which return true for IsDefinition(); see above.
  // For such instructions, we expect they write values to register operands.
  virtual RegOperand *GetOperandDefined() {
    return nullptr;
  }

  inline int LiveIntervalId() {
    return GetSSADefIdx();
  }

  bool IsGhostInstruction() const {
    return mop_ == kMopGhost;
  }

  Insn *GetPreviousMachineInsn() {
    Insn *returnInsn = prev;
    for (; returnInsn && returnInsn->bb == bb && !returnInsn->IsMachineInstruction(); returnInsn = returnInsn->prev)
      ;
    if (returnInsn != nullptr) {
      if (returnInsn->bb != bb) {
        return nullptr;
      }
    }
    return returnInsn;
  }

  Insn *GetNextMachineInsn() {
    Insn *returnInsn = next;
    for (; returnInsn && returnInsn->bb == bb && !returnInsn->IsMachineInstruction(); returnInsn = returnInsn->next)
      ;
    if (returnInsn != nullptr) {
      if (returnInsn->bb != bb) {
        return nullptr;
      }
    }
    return returnInsn;
  }

  virtual uint32 GetLatencyType() const {
    return 0;
  }

  virtual int GetJumpTargetIdx() const {
    return 0;
  }
};

struct DataInsnInfoCmp {
  bool operator()(const DataInsnInfo &lhs, const DataInsnInfo &rhs) const {
    Insn *lhsInsn = lhs.GetInsn();
    Insn *rhsInsn = rhs.GetInsn();
    if (lhsInsn->id == rhsInsn->id) {
      return (lhsInsn < rhsInsn);
    }

    return (lhsInsn->id < rhsInsn->id);
  }
};

/* For def->use chain, 1 def may have multiple uses in a single insn.
   Example:   d = ...
                = d + d
 */
struct DataInsnInfoCmp2 {
  bool operator()(const DataInsnInfo &lhs, const DataInsnInfo &rhs) const {
    Insn *lhsInsn = lhs.GetInsn();
    Insn *rhsInsn = rhs.GetInsn();
    if (lhsInsn->id == rhsInsn->id) {
      if (lhsInsn != rhsInsn) {
        return (lhsInsn < rhsInsn);
      }

      // For find def->use vector contains certain use insn.
      if (lhs.GetIndex() == DataInsnInfo::kAnyIndex || rhs.GetIndex() == DataInsnInfo::kAnyIndex) {
        return false;
      }

      int lhsValue = lhs.GetIndex() + ((lhs.GetProperty() & DataInsnInfo::kCallParam) ? 1 : 0);
      int rhsValue = rhs.GetIndex() + ((rhs.GetProperty() & DataInsnInfo::kCallParam) ? 1 : 0);
      return (lhsValue < rhsValue);
    }

    return (lhsInsn->id < rhsInsn->id);
  }
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_INSN_H
