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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64GLOBALOPT_H
#define MAPLEBE_INCLUDE_CG_AARCH64GLOBALOPT_H

#include "global_opt.h"
#include "aarch64_operand.h"
#include "aarch64_isa.h"
#include <set>

namespace maplebe {
using namespace maple;

class AArch64GlobalOpt : public GlobalOpt {
 private:
  enum DefineType {
    kRedefine,
    kPredefine,
  };

 public:
  explicit AArch64GlobalOpt(CGFunc *func, MemPool *mp) : GlobalOpt(func, mp) {}

  ~AArch64GlobalOpt() {}

  void Run() override;
  void CmpCsetOptimize();
  void ConvertCselToCset();
  void DeleteRedundantUxt();

 private:
  bool GetInverseCond(CondOperand *cond, CondOperand *&inverseCond) const;
  bool FindAThroughPath(Insn *insnStart, Insn *insnEnd);
  bool TraverseNext(BB *curBb, BB *target, std::set<BB *> *traversedBb);
  bool OnlyOneDefine(Insn *targetInsn);
  void ForwardPropOfRegister();
  void BackPropOfRegister();
  void CleanDDForDestOperand(Insn *insn, unsigned short index);
  bool NoOverlap(Insn *targetInsn, int opndidx, bool isBg);
  static bool OpndOnlyUsedInCurBB(Insn *insn, short opndidx, bool isDest = true);
  static bool OpndOnlyDefInCurBB(Insn *targetInsn, int opndidx);
  void InsertDUUDForDestOperand(Insn *newInsn, short newIndex, unsigned short newProp, bool isRefInsnBeforeNewInsn);
  void ReplaceInsnSrcOperand(Insn *mInsn, short i, unsigned short prop, Operand *newOpnd, Insn *defInsn);
  bool HasMultiGen(Insn *newInsn);
  bool IsPartialDominate(Insn *start, Insn *end, std::set<Insn *> *middleInsnSet);
  bool TraverseDomNext(BB *start, BB *end, std::set<BB *> *middleSet, std::set<uint32_t> *traversed);
  ReachingDefinition *GetRD() {
    return cgfunc->GetRD();
  }

  int GetMaximumValidBit(Insn *insn, short udidx, std::set<Insn *> &insnChecked) const;
  bool OpndDefByOne(Insn *insn, int useidx) const;
  bool OpndDefByZero(Insn *insn, int useidx) const;
  bool OpndDefByOneOrZero(Insn *insn, int useidx) const;
  static int GetInsnValidBit(Insn *insn);
  static bool InsnDefOne(Insn *insn);
  static bool InsnDefZero(Insn *insn);
  static bool InsnDefOneOrZero(Insn *insn);
  static bool RedefPointIsExist(Insn *insn, short opndidx);
  static bool IsSameReg(Operand *firstOpnd, Operand *secondOpnd);
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_AARCH64GLOBALOPT_H
