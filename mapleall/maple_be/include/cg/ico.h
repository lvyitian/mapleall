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

#ifndef MAPLEALL_MAPLEBE_INCLUDE_CG_ICO_H
#define MAPLEALL_MAPLEBE_INCLUDE_CG_ICO_H
#include "optimize_common.h"

namespace maplebe {
class IfConversionOptimizer : public Optimizer {
 public:
  IfConversionOptimizer(CGFunc *func, MemPool *mp, LiveAnalysis *live) : Optimizer(func, mp) {
    live_ = live;
    name = "ICO";
  }

  ~IfConversionOptimizer();
  void InitOptimizePatterns();

 private:
  LiveAnalysis *live_;
};

class Partition {
 private:
  std::set<Operand *> operands;
  std::vector<Insn *> insnList;

 public:
  Partition(Insn *insn);
  virtual ~Partition() {}
  bool CheckAndPut(Insn *insn);
};

class ICOPattern : public OptimizationPattern {
 public:
  const int kThreshold = 1;

 public:
  ICOPattern(CGFunc *func) : OptimizationPattern(func) {
    dotColor = ICO_ITE;
  }

  virtual bool Optimize(BB *&curbb) = 0;

 protected:
  bool DoOpt(BB *cmpBB, BB *ifBB, BB *elseBB, BB *joinBB);
  void InsertBeforeInsn(BB *fromBB, Insn *boundaryFromInsn, BB *toBB, Insn *boundaryToInsn);
  int PartitionBB(BB *bb);
  bool OperandsOverlapped(Insn *insn1, Insn *insn2);
};

/**
 * If-then-Else pattern
 */
class ITEPattern : public ICOPattern {
 public:
  ITEPattern(CGFunc *func) : ICOPattern(func) {
    patternname = "If-Then-Else";
  }

  ~ITEPattern();
  bool Optimize(BB *&curbb);
};

/**
 * If-Then pattern
 */
class ITPattern : public ICOPattern {
 public:
  ITPattern(CGFunc *func) : ICOPattern(func) {
    patternname = "If-Then";
  }

  ~ITPattern();
  bool Optimize(BB *&curbb);
};

CGFUNCPHASE_CANSKIP(CgDoIco, "ico")
}  // namespace maplebe
#endif /* MAPLEALL_MAPLEBE_INCLUDE_CG_ICO_H */
