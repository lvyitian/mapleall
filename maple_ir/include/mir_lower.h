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

/// Copyright [year] <Copyright Owner>
#ifndef MAPLE_IR_INCLUDE_MIR_LOWER_H
#define MAPLE_IR_INCLUDE_MIR_LOWER_H
#include <iostream>
#include "mir_nodes.h"
using namespace std;

namespace maple {
enum MirLowerPhase {
  kLowerUnder,
  kLowerMe,
  kLowerExpandArray,
  kLowerBe,
  kLowerCg,
  kLowerLno
};

#define LOWERME (1 << kLowerMe)
#define LOWEREXPANDARRAY (1 << kLowerExpandArray)
#define LOWERBE (1 << kLowerBe)
#define LOWERCG (1 << kLowerCg)
#define LOWERLNO (1 << kLowerLno)

// check if a block node ends with an unconditional jump
inline bool BlockNoFallThru(const BlockNode *blk) {
  return blk->GetLast()->op == OP_goto || blk->GetLast()->op == OP_return || blk->GetLast()->op == OP_switch ||
         blk->GetLast()->op == OP_throw || blk->GetLast()->op == OP_gosub || blk->GetLast()->op == OP_retsub;
}

class MIRLower {
 public:
  MIRModule &mirModule;
  MIRFunction *mirFunc;
  static const std::set<std::string> kSetArrayHotFunc;
  uint32 lowerPhase;

 public:
  explicit MIRLower(MIRModule &mod, MIRFunction *f) : mirModule(mod), mirFunc(f) {
    lowerPhase = 0;
  }
  virtual ~MIRLower() {}

  virtual BlockNode *LowerIfStmt(IfStmtNode *ifstmt, bool recursive = true);
  virtual BlockNode *LowerWhileStmt(WhileStmtNode *);
  BlockNode *LowerDowhileStmt(WhileStmtNode *);
  BlockNode *LowerDoloopStmt(DoloopNode *);
  BlockNode *LowerBlock(BlockNode *);
  void LowerBrCond(BlockNode *);
  void LowerFunc(MIRFunction *func);
  void ExpandArraymrt(MIRFunction *func);
  static bool ShouldOptArrayMrt(const MIRFunction *func);
  IfStmtNode *ExpandArraymrtifblock(IfStmtNode *node);
  WhileStmtNode *ExpandArraymrtwhileblock(WhileStmtNode *node);
  DoloopNode *ExpandArraymrtdoloopblock(DoloopNode *node);
  ForeachelemNode *ExpandArraymrtForeachelemblock(ForeachelemNode *node);
  BlockNode *ExpandArraymrtblock(BlockNode *block);
  void AddArraymrtmpl(BaseNode *exp, BlockNode *newblk);
  void SetLowerME() {
    lowerPhase |= LOWERME;
  }

  void SetLowerLNO() {
    lowerPhase |= LOWERLNO;
  }

  void SetLowerExpandArray() {
    lowerPhase |= LOWEREXPANDARRAY;
  }

  void SetLowerBE() {
    lowerPhase |= LOWERBE;
  }

  void SetLowerCG() {
    lowerPhase |= LOWERCG;
  }

  bool IsLowerME() const {
    return lowerPhase & LOWERME;
  }

  bool IsLowerLNO() const {
    return lowerPhase & LOWERLNO;
  }

  bool IsLowerExpandArray() const {
    return lowerPhase & LOWEREXPANDARRAY;
  }

  bool IsLowerBE() const {
    return lowerPhase & LOWERBE;
  }

  bool IsLowerCG() const {
    return lowerPhase & LOWERCG;
  }
};
}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_MIR_LOWER_H
