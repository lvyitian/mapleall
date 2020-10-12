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

#ifndef MAPLEBE_INCLUDE_BE_BELOWERER_H
#define MAPLEBE_INCLUDE_BE_BELOWERER_H

#include "be_common.h"

/// MapleIR headers.
#include "cmpl.h"
#include "mir_nodes.h"
#include "mir_module.h"
#include "mir_function.h"
#include "mir_lower.h"

/// C++ headers.
#include <utility>
#include <cstdio>
#include <cstddef>

namespace maplebe {

class BELowerer {
 protected:
  typedef uint32 builtin_func_id_t;

  enum be_target_t {
    kBeTargetMmpl,
    kBeTargetNommpl,
  };

  enum option_t {
    kUndefined = 0,
    kGenEh = 1ULL << 0,
    kVerboseAsm = 1ULL << 1,

    kAutoToPregs = 1ULL << 63
  };

  typedef uint64 option_flag_t;

 public:
  MIRModule &mirModule;
  BECommon &becommon;

 private:
  be_target_t target_;

 public:
  BlockNode *current_blk_;  // current block for lowered statements to be inserted to
  bool checkloadstore;

 private:
  option_flag_t options_;
  bool need_branch_cleanup_;
  bool has_javatry_;

 protected:
  static const PUIdx kfuncNotFound = PUIdx(-1);
  int64 seed;

  std::map<MIRSymbol *, int32> locksymbol;
  std::map<PregIdx, int32> lockreg;
  static constexpr const char *INTRN_RETVAL_PREFIX = "__iret";

 private:
  inline void SetCurrentFunc(MIRFunction *f) {
    mirModule.SetCurFunction(f);
  }

  inline bool IsTargetMMPL() const {
    return target_ == kBeTargetMmpl;
  }

  inline bool ShouldAddAdditionalComment() const {
    return ((options_ & kVerboseAsm) != 0);
  }

  inline bool GenerateExceptionHandlingCode() const {
    return ((options_ & kGenEh) != 0);
  }

 protected:
  inline bool ConvertAutoSymbolsToPseudoRegs() const {
    return ((options_ & kAutoToPregs) != 0);
  }

  void LowerTypePtr(BaseNode *expr) const;

  inline void SetOptions(option_flag_t o) {
    options_ = o;
  }

  BaseNode *LowerDreadBitfield(DreadNode *);
  BaseNode *LowerIreadBitfield(IreadNode *);
  StmtNode *LowerDassignBitfield(DassignNode *, BlockNode *);
  StmtNode *LowerIassignBitfield(IassignNode *, BlockNode *);

 public:
  BaseNode *NodeConvert(PrimType mtype, BaseNode *expr);
  BaseNode *LowerIaddrof(const IreadNode *iaddrof);
  BaseNode *SplitBinaryNodeOpnd1(BinaryNode *bnode, BlockNode *blknode);
  BaseNode *SplitTernaryNodeResult(TernaryNode *tnode, BaseNode *parent, BlockNode *blknode);
  bool IsComplexSelect(TernaryNode *tnode, BaseNode *parent, BlockNode *blknode);
  BaseNode *LowerComplexSelect(TernaryNode *tnode, BaseNode *parent, BlockNode *blknode);
  BaseNode *LowerFarray(ArrayNode *array);
  BaseNode *LowerArray(ArrayNode *array);
  virtual BaseNode *LowerExpr(BaseNode *, BaseNode *, BaseNode *, BlockNode *);

  bool Shouldoptarray() {
    return MIRLower::ShouldOptArrayMrt(mirModule.CurFunction());
  }

  virtual BaseNode *LowerDread(DreadNode *dread) {
    return (dread->fieldID == 0 ? dread : LowerDreadBitfield(dread));
  }

  virtual BaseNode *LowerIread(IreadNode *iread) {
    return (iread->fieldID == 0 ? iread : LowerIreadBitfield(iread));
  }

  virtual void LowerDassign(DassignNode *, BlockNode *) = 0;
  virtual void LowerRegassign(RegassignNode *, BlockNode *) = 0;

  virtual void LowerIassign(IassignNode *, BlockNode *);

  virtual BaseNode *LowerAddrof(AddrofNode *addrof) {
    return addrof;
  }

  DassignNode *SaveReturnValueInLocal(StIdx, uint16);
  void LowerCallStmt(StmtNode *, StmtNode *&, BlockNode *, MIRType *retty = nullptr);
  BlockNode *LowerCallAssignedStmt(StmtNode *);
  BaseNode *LowerRem(BaseNode *rem, BlockNode *);
  BlockNode *LowerJavaThrow(UnaryStmtNode *);
  BaseNode *LowerJavaDiv(BaseNode *, BinaryNode *, BlockNode *);
  virtual BaseNode *LowerSTACKMalloc(GCMallocNode *, BlockNode *);
  virtual BaseNode *LowerSTACKJarrayMalloc(JarrayMallocNode *node, BlockNode *block);
  // ctor
  explicit BELowerer(MIRModule &mod, BECommon &becmmn, be_target_t tgt, MIRFunction *func = nullptr)
    : mirModule(mod),
      becommon(becmmn),
      target_(tgt),
      current_blk_(nullptr),
      checkloadstore(false),
      options_(0),
      need_branch_cleanup_(false),
      has_javatry_(false),
      seed(0) {
    SetCurrentFunc(func);
  }

  // dtor
  ~BELowerer() {}

  void LowerStmt(StmtNode *, BlockNode *);

  inline MIRFunction *GetCurrentFunc() const {
    return mirModule.CurFunction();
  }

  MIRSymbol *CreateNewRetVar(const MIRType *ty, const char *prefix);

  void RegisterExternalLibraryFunctions();

  BlockNode *LowerBlock(BlockNode *);

  void LowerJavaTryCatchBlocks(BlockNode *body);

  void LowerCppCatch(BlockNode *blk);

  virtual void LowerFunc(MIRFunction *);

#if TARGARM || TARGAARCH64 || TARGARK
  BlockNode *LowerReturnStruct(NaryStmtNode *);
#endif
  virtual BlockNode *LowerReturn(NaryStmtNode *);
  void LowerEntry(MIRFunction *func);

  virtual void LowerFormalParameters(MIRFunction *func) {}

  virtual BlockNode *GetNewEntryBlock() {
    return nullptr;
  }

  /*
     Lower pointer/reference types if found in pseudo registers.
   */
  void LowerPseudoRegs(MIRFunction *func);

  void CheckFormalInReg(MIRFunction *func);

  virtual BaseNode *LowerIntrinsicop(BaseNode *parent, IntrinsicopNode *intrinnode, BlockNode *blk) {
    CHECK_FATAL(false, "LowerIntrinsicop NYI");
    return intrinnode;
  }

  virtual BaseNode *LowerIntrinsicopwithtype(BaseNode *parent, IntrinsicopNode *intrinnode, BlockNode *blk) {
    CHECK_FATAL(false, "LowerIntrinsicopwithtype NYI");
    return intrinnode;
  }

  virtual StmtNode *LowerIntrinsiccall(IntrinsiccallNode *, BlockNode *) {
    CHECK_FATAL(false, "LowerIntrinsiccall NYI");
  }

  virtual StmtNode *LowerSyncEnterSyncExit(StmtNode *stmt) = 0;

  // if it defines a built-in to use for the given intrinsic, return the name.
  // otherwise, return nullptr
  virtual PUIdx GetBuiltInToUse(builtin_func_id_t id) {
    return kfuncNotFound;
  }

  StmtNode *LowerCall(CallNode *, StmtNode *&, BlockNode *, MIRType *retty = nullptr);;

  void CleanupBranches(MIRFunction *func);

 protected:
  // true if the lower level (e.g. mplcg) can handle the intrinsic directly.
  // For example, the INTRN_MPL_ATOMIC_EXCHANGE_PTR can be directly handled by
  // mplcg, and generate machine code sequences not containing any function
  // calls.  Such intrinsics will bypass the lowering of "assigned", and let
  // mplcg handle the intrinsic results which are not return values.
  virtual bool IsIntrinsicCallHandledAtLowerLevel(MIRIntrinsicID intrinsic) {
    return false;
  }
  virtual bool IsIntrinsicOpHandledAtLowerLevel(MIRIntrinsicID intrinsic) = 0;

};  // class BELowerer

}  // namespace maplebe

#endif  //  MAPLEBE_INCLUDE_BE_BELOWERER_H
