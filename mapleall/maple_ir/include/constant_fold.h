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

#ifndef MAPLE_IR_INCLUDE_CONSTANT_FOLD_H
#define MAPLE_IR_INCLUDE_CONSTANT_FOLD_H

#include "mir_nodes.h"
using namespace maple;
namespace maple {

class ConstantFold {
 private:
  MIRModule *module;

 public:
  /*
     Fold an expression.

     It returns a new expression if there was something to fold, or
     nullptr otherwise.
   */
  BaseNode *Fold(BaseNode *x);

  /*
     Simplify a statement

     It returns the original statement or the changed statement if a
     simplification happened. If the statement can be deleted after a
     simplification, it returns nullptr.
   */
  StmtNode *Simplify(StmtNode *x);
  ConstantFold(MIRModule *mod) : module(mod) {}

  virtual ~ConstantFold() {}

  MIRConst *FoldFloorMIRConst(MIRConst *, PrimType, PrimType);
  MIRConst *FoldRoundMIRConst(MIRConst *, PrimType, PrimType);
  MIRConst *FoldTypeCvtMIRConst(MIRConst *, PrimType, PrimType);
  MIRConst *FoldSignExtendMIRConst(Opcode, PrimType, uint8, MIRConst *);
  MIRConst *FoldIntConstBinaryMIRConst(Opcode opcode, PrimType resTyp, MIRIntConst *cst0, MIRIntConst *cst1);
  MIRConst *FoldConstComparisonMIRConst(Opcode, PrimType, PrimType, MIRConst *, PrimType, MIRConst *, PrimType);

 private:
  StmtNode *SimplifyBinary(BinaryStmtNode *n);
  StmtNode *SimplifyBlock(BlockNode *n);
  StmtNode *SimplifyCondGoto(CondGotoNode *n);
  StmtNode *SimplifyCondGotoSelect(CondGotoNode *n);
  StmtNode *SimplifyDassign(DassignNode *n);
  StmtNode *SimplifyIassign(IassignNode *n);
  StmtNode *SimplifyNary(NaryStmtNode *n);
  StmtNode *SimplifyIcall(IcallNode *n);
  StmtNode *SimplifyIf(IfStmtNode *n);
  StmtNode *SimplifySwitch(SwitchNode *n);
  StmtNode *SimplifyUnary(UnaryStmtNode *n);
  StmtNode *SimplifyWhile(WhileStmtNode *n);

  std::pair<BaseNode *, int64> FoldArray(ArrayNode *n);
  std::pair<BaseNode *, int64> FoldBase(BaseNode *n) const;
  std::pair<BaseNode *, int64> FoldBinary(BinaryNode *n);
  std::pair<BaseNode *, int64> FoldCompare(CompareNode *n);
  std::pair<BaseNode *, int64> FoldDepositbits(DepositbitsNode *n);
  std::pair<BaseNode *, int64> FoldExtractbits(ExtractbitsNode *n);
  ConstvalNode *FoldSignExtend(Opcode opcode, PrimType restype, uint8 size, ConstvalNode *cst);
  std::pair<BaseNode *, int64> FoldIread(IreadNode *n);
  std::pair<BaseNode *, int64> FoldSizeoftype(SizeoftypeNode *n);
  std::pair<BaseNode *, int64> FoldRetype(RetypeNode *n);
  std::pair<BaseNode *, int64> FoldGcmallocjarray(JarrayMallocNode *n);
  std::pair<BaseNode *, int64> FoldUnary(UnaryNode *n);
  std::pair<BaseNode *, int64> FoldTernary(TernaryNode *n);
  std::pair<BaseNode *, int64> FoldTypeCvt(TypeCvtNode *n);

  ConstvalNode *FoldCeil(ConstvalNode *cst, PrimType fromtype, PrimType totype);
  ConstvalNode *FoldFloor(ConstvalNode *cst, PrimType fromtype, PrimType totype);
  ConstvalNode *FoldRound(ConstvalNode *cst, PrimType fromtype, PrimType totype);
  ConstvalNode *FoldTrunc(ConstvalNode *cst, PrimType fromtype, PrimType totype);
  ConstvalNode *FoldTypeCvt(ConstvalNode *cst, PrimType fromtype, PrimType totype);
  ConstvalNode *FoldConstComparison(Opcode opcode, PrimType resTyp, PrimType opndTyp, ConstvalNode *c0,
                                    ConstvalNode *c1);
  ConstvalNode *FoldConstBinary(Opcode opcode, PrimType resTyp, ConstvalNode *c0, ConstvalNode *c1);
  ConstvalNode *FoldIntConstComparison(Opcode opcode, PrimType resTyp, PrimType opndTyp, ConstvalNode *c0,
                                       ConstvalNode *c1);
  MIRIntConst *FoldIntConstComparisonMIRConst(Opcode, PrimType, PrimType, const MIRIntConst *, PrimType,
                                              const MIRIntConst *, PrimType);
  ConstvalNode *FoldIntConstBinary(Opcode opcode, PrimType resTyp, ConstvalNode *c0, ConstvalNode *c1);
  ConstvalNode *FoldFPConstComparison(Opcode opcode, PrimType resTyp, PrimType opndTyp, ConstvalNode *c0,
                                      ConstvalNode *c1);
  MIRIntConst *FoldFPConstComparisonMIRConst(Opcode opcode, PrimType resTyp, PrimType opndTyp, MIRConst *c0, PrimType,
                                             MIRConst *c1, PrimType);
  ConstvalNode *FoldFPConstBinary(Opcode opcode, PrimType resTyp, ConstvalNode *c0, ConstvalNode *c1);
  ConstvalNode *FoldConstUnary(Opcode opcode, PrimType resTyp, ConstvalNode *c);
  ConstvalNode *FoldIntConstUnary(Opcode opcode, PrimType resTyp, ConstvalNode *c);

  template <typename T>
  ConstvalNode *FoldFPConstUnary(Opcode opcode, PrimType resTyp, ConstvalNode *c);

  BaseNode *NegateTree(BaseNode *x);
  BaseNode *Negate(BaseNode *n);
  BaseNode *Negate(UnaryNode *x);
  BaseNode *Negate(ConstvalNode *x);

  BinaryNode *NewBinaryNode(BinaryNode *old, Opcode op, PrimType primType, BaseNode *l, BaseNode *r);
  UnaryNode *NewUnaryNode(UnaryNode *old, Opcode op, PrimType primType, BaseNode *e);

  std::pair<BaseNode *, int64> DispatchFold(BaseNode *n);
  BaseNode *PairToExpr(PrimType typ, const std::pair<BaseNode *, int64> &p);
  BaseNode *SimplifyDoubleCompare(CompareNode *n);
};
}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_CONSTANT_FOLD_H
