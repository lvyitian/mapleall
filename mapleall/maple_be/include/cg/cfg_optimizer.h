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

#ifndef MAPLEALL_MAPLEBE_INCLUDE_CG_CFGOPTIMIZER_H
#define MAPLEALL_MAPLEBE_INCLUDE_CG_CFGOPTIMIZER_H
#include "cg_cfg.h"
#include "optimize_common.h"

namespace maplebe {

class ChainingPattern : public OptimizationPattern {
 public:
  ChainingPattern(CGFunc *func) : OptimizationPattern(func) {
    patternname = "BB Chaining";
    dotColor = CFGO_CHAINING;
  }

  ~ChainingPattern();
  bool Optimize(BB *&curbb);

 protected:
  bool NoInsnBetween(BB *from, BB *to);
  bool DoSameThing(BB *bb1, Insn *last1, BB *bb2, Insn *last2);
};

class SequentialJumpPattern : public OptimizationPattern {
 public:
  SequentialJumpPattern(CGFunc *func) : OptimizationPattern(func) {
    patternname = "Sequential Jump";
    dotColor = CFGO_SJ;
  }

  ~SequentialJumpPattern();
  bool Optimize(BB *&curbb);

 protected:
  void SkipSucBB(BB *curBB, BB *sucBB);
};

class FlipBRPattern : public OptimizationPattern {
 public:
  FlipBRPattern(CGFunc *func) : OptimizationPattern(func) {
    patternname = "Condition Flip";
    dotColor = CFGO_FLIPCOND;
  }

  ~FlipBRPattern();
  bool Optimize(BB *&curbb);

 protected:
  bool NeedExtraGoto(BB *bb);
  bool NoFallthrough(BB *bb);
  bool IsSoloGoto(BB *bb);
};

/**
 * This class represents the scenario that the condition always hold, therefore
 * the conditional jump can be modified to unconditional jump.
 */
class AlwaysPattern : public OptimizationPattern {
 public:
  AlwaysPattern(CGFunc *func) : OptimizationPattern(func) {
    patternname = "Always Hold";
    dotColor = CFGO_ALWAYS;
    func->theCFG->InitInsnVisitor(func, func->memPool);
  }

  ~AlwaysPattern();
  bool Optimize(BB *&curbb);
  /**
   * This method is called from EBO
   * The index of the operand in operands should be the same as it is in the instruction.
   * For example:
   * CMP operands[0], operands[1]
   */
  bool Optimize(Insn *cmpInsn, Operand *operands[]);

 private:
  void NeverHold(Insn *cmpInsn);
  void AlwaysHold(Insn *cmpInsn);
  bool IsConditionAlwaysHold(MOperator mop, maple::int64 cmpRet);
};

/**
 * This class represents the scenario that the BB is unreachable.
 */
class UnreachBBPattern : public OptimizationPattern {
 public:
  UnreachBBPattern(CGFunc *func) : OptimizationPattern(func) {
    patternname = "Unreachable BB";
    dotColor = CFGO_UNREACH;
    func->theCFG->FindAndMarkUnreachable(cgfunc);
  }

  ~UnreachBBPattern();
  bool Optimize(BB *&curbb);
};

/**
 * This class represents the scenario that a common jump BB can be duplicated
 * to one of its another predecessor.
 */
class DuplicateBBPattern : public OptimizationPattern {
 private:
  int THRESHOLD = 10;

  void DumpBBInsn(BB *&bb);

 public:
  DuplicateBBPattern(CGFunc *func) : OptimizationPattern(func) {
    patternname = "Duplicate BB";
    dotColor = CFGO_DUP;
  }

  ~DuplicateBBPattern();
  bool Optimize(BB *&curbb);
};

/**
 * This class represents the scenario that a BB contains nothing.
 */
class EmptyBBPattern : public OptimizationPattern {
 public:
  EmptyBBPattern(CGFunc *func) : OptimizationPattern(func) {
    patternname = "Empty BB";
    dotColor = CFGO_EMPTY;
  }

  ~EmptyBBPattern();
  bool Optimize(BB *&curbb);
};

class CFGOptimizer : public Optimizer {
 public:
  CFGOptimizer(CGFunc *func, MemPool *mp) : Optimizer(func, mp) {
    name = "CFGO";
  }

  ~CFGOptimizer();
  void InitOptimizePatterns();

 private:
  void DoChaining();
  void DoSequentialJump();
  void SkipSucBB(BB *curBB);
};

CGFUNCPHASE_CANSKIP(CgDoCfgo, "cfgo")
CGFUNCPHASE_CANSKIP(CgDoPostCfgo, "postcfgo")
}  // namespace maplebe

#endif /* MAPLEALL_MAPLEBE_INCLUDE_CG_CFGOPTIMIZER_H */
