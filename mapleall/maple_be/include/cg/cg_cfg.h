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

#ifndef MAPLEALL_MAPLEBE_INCLUDE_CG_CGCFG_H
#define MAPLEALL_MAPLEBE_INCLUDE_CG_CGCFG_H
#include "cg_func.h"

namespace maplebe {

class InsnVisitor {
 private:
  CGFunc *cgfunc;

 public:
  InsnVisitor(CGFunc *func) : cgfunc(func) {}

  ~InsnVisitor();
  CGFunc *GetCGFunc() const {
    return cgfunc;
  }

  /**
   * Precondition:
   * The last instruction in bb is either conditional or unconditional jump.
   *
   * The jump target of bb is modified to the location specified by targetLabel.
   */
  virtual void ModifyJumpTarget(LabelIdx targetLabel, BB *&bb) = 0;

  /**
   * Precondition:
   * The last instruction in bb is either conditional or unconditional jump.
   *
   * The jump target of bb is modified to the location specified by targetOperand.
   */
  virtual void ModifyJumpTarget(Operand *targetOperand, BB *&bb) = 0;

  /**
   * Precondition:
   * The last instruction in bb is either a conditional or an unconditional jump.
   * The last instruction in newTarget is an unconditional jump.
   *
   * The jump target of bb is modified to newTarget's jump target.
   */
  virtual void ModifyJumpTarget(BB *newTarget, BB *&bb) = 0;
  // return true if successfully modified
  virtual bool ModifyBrInsn(maple::LabelIdx targetLabel, BB *&curbb) = 0;
  // Check if it requires to add extra gotos when relocate bb
  virtual MOperator FlipConditionOp(MOperator flippedOp, int &targetIdx) = 0;

  virtual bool IsConditionAlwaysHold(Insn *cmpInsn, Insn *conditionBrInsn, Operand *operands[]) = 0;
  virtual Insn *CreateGotoInsn(Insn *condBrInsn) = 0;
  virtual Insn *CloneInsn(Insn *originalInsn) = 0;
  /**
   * Create a new virtual register operand which has the same type and size as the given one.
   */
  virtual RegOperand *CreateVregFromReg(RegOperand *reg) = 0;
  virtual Insn *CreateLdrInsn(MOperator mop, RegOperand *reg, MemOperand *mem) = 0;
  virtual Insn *CreateMoveInsn(RegOperand *dest, RegOperand *tempTar) = 0;
  virtual LabelIdx GetJumpLabel(Insn *insn) = 0;
  virtual Operand *GetStrTarget(Insn *insn) = 0;
  virtual Operand *GetStrSource(Insn *insn) = 0;
  virtual Operand *GetLdrTarget(Insn *insn) = 0;
  virtual Operand *GetLdrSource(Insn *insn) = 0;
  virtual bool EqualMemOperands(Operand *op1, Operand *op2) = 0;
  virtual bool IsCompareInsn(Insn *insn) = 0;
  virtual bool IsCompareAndBranchInsn(Insn *insn) = 0;
  virtual bool ModifyInsnOpnds(Insn *insn, Operand *src, Operand *tar) = 0;
  virtual bool SyncRegs(Insn *lastMemAccessInsn, Insn *csel) {
    return false;
  }
  virtual Insn *CreateCondSelectionInsn(Insn *branchInsn, MOperator originalMop, Operand *ret, Operand *srcIf,
                                        Operand *srcElse) {
    return nullptr;
  }
  virtual Insn *CreateCmpInsn(Insn *condbr) = 0;
  virtual Insn *BuildFmoveZero(RegOperand *dst, uint32 dsize) {
    return nullptr;
  }
  virtual Insn *BuildCondSet(Insn *branch, RegOperand *reg, bool inverse) {
    return nullptr;
  }
  virtual Insn *BuildCondSel(Insn *branch, MOperator mop, RegOperand *dst, RegOperand *src1, RegOperand *src2) {
    return nullptr;
  }
  virtual bool CanDoIco(Insn *insn) {
    return false;
  }
};  // class InsnVisitor;

class CGCFG {
 public:
  CGFunc *cgfunc;

 public:
  explicit CGCFG(CGFunc *cf) : cgfunc(cf) {}

  ~CGCFG() {}

  void BuildCFG();

  // cgcfgvisitor
 private:
  static InsnVisitor *insnVisitor;
  void MergeBB(BB *merger, BB *mergee);
  Operand *FindLastStrTarget(BB *bb);
  int IsLastRegOrMem(BB *bb);

 public:
  friend class Optimizer;
  enum ACCESS { kStore, kLoad, kEither };
  Insn *CheckIntegrity(BB *bb);
  void InitInsnVisitor(CGFunc *cgfunc, MemPool *mp);
  InsnVisitor *GetInsnModifier() const {
    return insnVisitor;
  }

  bool IsNext(BB *bb1, BB *bb2, bool ignoreComment);
  bool IsEmptyOrCommentOnly(BB *bb);
  bool IsSoloGoto(BB *bb);
  bool IsCommentBB(BB *bb);
  bool AreCommentAllPreds(BB *bb);
  int NumStore(BB *bb);
  bool CanMerge(BB *merger, BB *mergee);
  bool CanICO(BB *merger, BB *mergee);
  bool BBJudge(BB *first, BB *second);
  /**
   * Merge all instructions in mergee into merger, each BB's successors and
   * predecessors should be modified accordingly.
   */
  void MergeBB(BB *merger, BB *mergee, CGFunc *cgfunc);

  /**
   * Remove a BB from its position in the CFG.
   * Prev, next, preds and sucs are all modified accordingly.
   */
  void RemoveBB(BB *bb, bool isGotoIf=false);

  /**
   * Skip the successor of bb, directly jump to bb's successor'ssuccessor.
   */
  void SkipSucBB(BB *&bb);
  void RetargetJump(BB *&srcBB, BB *targetBB);

  /**
   * Loop up if the given label is in the exception tables in LSDA
   */
  bool InLSDA(LabelIdx label, EHFunc *ehfunc);
  bool InSwitchTable(LabelIdx label, CGFunc *ehfunc);
  void DumpBBInsn(BB *bb);

  RegOperand *CreateVregFromReg(RegOperand *pReg);
  Insn *CloneInsn(Insn *originalInsn);

  BB *GetTargetSuc(BB *curbb, bool branchOnly = false, bool isGotoIf = false);
  // Check if two BBs save to the same memory location at last
  bool IsAccessSameMem(BB *bb1, BB *bb2);
  bool IsDefSameReg(BB *bb1, BB *bb2);

  bool IsCompareInsn(Insn *insn);
  bool IsCompareAndBranchInsn(Insn *insn);

  Operand *FindLastStrSource(BB *bb);
  Operand *FindLastLdrTarget(BB *bb);
  MemOperand *FindLastAccessedMem(BB *bb);
  MemOperand *GetMemOperand(Insn *insn, ACCESS access = ACCESS::kEither);
  RegOperand *GetRegOperand(Insn *insn);
  Insn *FindLastMemAccessInsn(BB *bb);
  Insn *FindLastInsnAccessMem(BB *bb, MemOperand *mem, ACCESS access = ACCESS::kEither);
  Insn *FindFirstInsnAccessMem(BB *bb, MemOperand *mem, ACCESS access = ACCESS::kEither);
  Insn *FindLastLdrInsn(BB *bb);
  Insn *FindLastStrInsn(BB *bb);
  Insn *FindLastCondBrInsn(BB *bb);
  Insn *FindLastCmpInsn(BB *bb);
  Insn *FindLastDefInsn(BB *bb);

  Insn *BuildConditionSelectionInsn(BB *testBB, BB *ifBB, BB *elseBB);
  Insn *BuildCondSelForMove(BB *testBB, BB *ifBB, BB *elseBB);
  Insn *BuildCmpInsn(Insn *condbr);
  Insn *BuildSpeculativeAssign(Insn *sampleStr);

  bool IsUnreachable(BB *curbb, CGFunc *cgfunc);
  void FindAndMarkUnreachable(CGFunc *cgfunc);
  void FlushUnReachableStatusAndRemoveRelations(BB *curbb, CGFunc *cgfunc);
  bool Dominates(BB *bb1, BB *bb2, vector<BB *> searched, CGFunc *cgfunc);
  bool ChangeInsnTar(Insn *loadInsn, RegOperand *newTar);
  RegOperand *CheckSrcForDef(BB *bb, Insn *insn);
  int NumOfInsn(BB *bb);
  bool CheckCondMoveBB(BB *bb, map<Operand *, Operand *> &destSrcMap, vector<Operand *> &destRegs, Operand *flagReg);
  bool IsSetInsn(Insn *insn, Operand *&dest, Operand *&src);

  bool BuildCondMovInsn(BB *cmpBB, BB *bb, const map<Operand *, Operand *> &ifDestSrcMap,
                        const map<Operand *, Operand *> &elseDestSrcMap, bool elseBBisProcessed,
                        vector<Insn *> &generateInsn);

  void MarkLabelTakenBB();
  void UnreachCodeAnalysis();
};  // class CGCFG

}  // namespace maplebe

#endif
