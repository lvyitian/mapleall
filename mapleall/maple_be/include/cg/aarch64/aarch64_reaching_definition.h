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

#ifndef AARCH64REACHINGDEFINITION_H
#define AARCH64REACHINGDEFINITION_H

#include "reaching_definition.h"

namespace maplebe {

class AArch64ReachingDefinition : public ReachingDefinition {
 public:
  explicit AArch64ReachingDefinition(CGFunc *func, MemPool *mp, MapleAllocator ma, LiveAnalysis *live)
    : ReachingDefinition(func, mp, ma, live) {}

  ~AArch64ReachingDefinition() {}

  void InitStartGen() override final;
  void InitEhDefine(BB *bb) override final;
  void InitKillGen(BB *bb, int mode) override final;
  bool MayHasThrow(const Insn *headInsn, const Insn *tailInsn) override final;
  void GenerateUseDef(BB *bb, int mode) override final;
  void GenerateDefUse(BB *bb) override final;
  void KillAllCallerSavedRegs(BB *bb, Insn *callInsn) override final;
  void DirtyAllNonStackMem(BB *bb) override final;
  void GenReturnValue(BB *bb, Insn *insn) override final;
  void KillAllCallerSavedRegsOnBBIn(BB *bb, Insn *callInsn) override final;
  void GenReturnValueOnBBIn(BB *bb, Insn *insn) override final;
  void InsertUseToDef(DataInsnInfo &insnInfo, Insn *useInsn, short index, short userProperty) override final;
  void AddRetPseudoInsn(BB *bb) override final;
  void AddRetPseudoInsns() override final;
  void DumpUDChain(BB *bb) override final;
  void DumpDUChain(BB *bb) override final;
  void DumpWAWDependence(BB *bb) override final;
  void RemoveDUUDForInsn(Insn *insn) override final;
  void InsertDUUDForDestOperand(Insn *newInsn, short newIndex, unsigned short newProp, Insn *refInsn, short refIndex,
                                unsigned short refProp, bool isRefInsnBeforeNewInsn) override final;

  void InsertDUUDForSrcOperand(Insn *newInsn, short newIndex, unsigned short newProp, Insn *refInsn, short refIndex,
                               unsigned short refProp) override final;

  void ReplaceInsnSrcOperand(Insn *insn, short index, unsigned short prop, Operand *newOpnd, Insn *refInsn,
                             short refIndex, unsigned short refProp) override final;

  void ReplaceInsnDestOperand(Insn *insn, short index, unsigned short prop, Operand *newOpnd, Insn *refInsn,
                              short refIndex, unsigned short refProp, bool isRefInsnBeforeInsn) override final;
};

}  // namespace maplebe

#endif /* AARCH64REACHINGDEFINITION_H */
