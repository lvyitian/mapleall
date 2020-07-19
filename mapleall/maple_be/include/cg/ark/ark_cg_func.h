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

#ifndef MAPLEBE_INCLUDE_CG_ARK_ARKCGFUNC_H
#define MAPLEBE_INCLUDE_CG_ARK_ARKCGFUNC_H

#include "cg.h"
#include "cg_func.h"
#include "ark_mem_layout.h"
#include "name_mangler.h"

namespace maplebe {

class Insn;
class Operand;
class LabelOperand;
class RegAllocator;
class CallerSaveRegHandler;
class Ebo;
class LiveAnalysis;
class OptLocalRef;
class StoreLoadOpt;
class GlobalOpt;
class ReachingDefinition;
class Schedule;
class RegOperand;

class ArkCGFunc : public CGFunc {
 private:
  union {
    regno_t regno_javaCatch;  // For O2.
    Operand *opnd_javaCatch;  // For O0-O1.
  } ujavaCatch;
  MIRSymbol *reflect_strtab_sym;
  MIRSymbol *reflect_start_hot_strtab_sym;
  MIRSymbol *reflect_both_hot_strtab_sym;
  MIRSymbol *reflect_run_hot_strtab_sym;
  std::string GetReflectString(uint32_t offset);
 public:
  explicit ArkCGFunc(MIRModule *mod, CG *c, MIRFunction *f, BECommon *b, MemPool *mp, MapleAllocator *mallocator)
    : CGFunc(mod, c, f, b, mp, mallocator) {
    ujavaCatch.regno_javaCatch = 0;
    CGFunc::memlayout = mp->New<ArkMemLayout>(b, f, mallocator);
    CGFunc::memlayout->SetCurrFunction(this);
    if (func->module->IsJavaModule()) {
      reflect_strtab_sym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kReflectionStrtabPrefixStr + func->module->GetFileNameAsPostfix()));
      reflect_start_hot_strtab_sym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kReflectionStartHotStrtabPrefixStr +
                                                                         func->module->GetFileNameAsPostfix()));
      reflect_both_hot_strtab_sym =
        GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kReflectionBothHotStrTabPrefixStr + func->module->GetFileNameAsPostfix()));
      reflect_run_hot_strtab_sym =
      GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kReflectionRunHotStrtabPrefixStr + func->module->GetFileNameAsPostfix()));
    } else {
      reflect_strtab_sym = nullptr;
      reflect_start_hot_strtab_sym = nullptr;
      reflect_both_hot_strtab_sym = nullptr;
      reflect_run_hot_strtab_sym = nullptr;
    }
  }
  ~ArkCGFunc() {}
  void Savelockinfo(BB *bb) override {}
  void GenSavemethodinfoCode(BB *bb) override {}
  Insn *GenerateCfiRestores(BB *) override { return nullptr; }
  void GenerateCleanupCode(BB *bb) override {}
  bool NeedCleanup() override { return false; }
  void GenerateCleanupCodeForExtEpilog(BB *bb) override {}
  Operand *GetOrCreateRflag() override { return nullptr; }
  Operand *GetRflag() override { return nullptr; }
  LabelOperand *GetOrCreateLabelOperand(LabelIdx labidx) override { return nullptr; }
  RegAllocator *NewRegAllocator(CGFunc *cgfunc, MemPool *mp, MapleAllocator *mallocator) override { return nullptr; }
  CallerSavedRegHandler *NewCallerSavedRegisterHandler(CGFunc *cgfunc, MemPool *mp) override { return nullptr; }
  Ebo *NewEbo(CGFunc *cgfunc, MemPool *mp, LiveAnalysis *live, bool beforeRegalloc, const char *phase) override { return nullptr; }
  OptLocalRef *NewOptLocalRef(CGFunc *cgfunc, MemPool *mp) override { return nullptr; }
  StoreLoadOpt *NewStoreLoadOpt(CGFunc *cgfunc, MemPool *mp, ReachingDefinition *rd) override { return nullptr; }
  GlobalOpt *NewGlobalOpt(CGFunc *cgfunc, MemPool *mp) override { return nullptr; }
  LiveAnalysis *NewLiveAnalysis(CGFunc *cgfunc, MemPool *mp) override { return nullptr; }
  ReachingDefinition *NewReachingDefinition(CGFunc *cgfunc, MemPool *mp, MapleAllocator ma, LiveAnalysis *live) override { return nullptr; }
  Schedule *NewSchedule(CGFunc *cgfunc, MemPool *mp, LiveAnalysis *live, const char *phaseName) override { return nullptr; }
  void MergeReturn() override {}
  void HandleRCCall(bool begin, MIRSymbol *retRef = nullptr) override {}
  void HandleRetCleanup(NaryStmtNode *retnode) override {}
  void SelectDassign(DassignNode *stmt, Operand *opnd0) override {}
  void SelectRegassign(RegassignNode *stmt, Operand *opnd0) override {}
  void SelectAssertnull(UnaryStmtNode *stmt) override {}
  void SelectAggDassign(DassignNode *stmt) override {}
  void SelectIassign(IassignNode *stmt) override {}
  void SelectAggIassign(IassignNode *stmt, Operand *lhsaddropnd) override {}
  void SelectReturn(NaryStmtNode *stmt, Operand *opnd) override {}
  Operand *SelectIgoto(Operand *opnd0) override { return nullptr; }
  void SelectCondGoto(CondGotoNode *stmt, Operand *opnd0, Operand *opnd1) override {}
  void SelectCondSpecial(CondGotoNode *stmt, BaseNode *opnd0) override {}
  void SelectGoto(GotoNode *stmt) override {}
  void SelectCall(CallNode *callnode) override {}
  void SelectIcall(IcallNode *icallnode, Operand *fptropnd) override {}
  void SelectIntrinCall(IntrinsiccallNode *intrinsiccallnode) override {}
  void SelectMembar(StmtNode *membar) override {}
  void SelectComment(CommentNode *comment) override {}
  bool CanBBThrow(BB *) override { return false; }
  Operand *SelectDread(BaseNode *parent, AddrofNode *expr) override { return nullptr; }
  RegOperand *SelectRegread(BaseNode *parent, RegreadNode *expr) override { return nullptr; }
  Operand *SelectAddrof(AddrofNode *expr) override { return nullptr; }
  Operand *SelectAddroffunc(AddroffuncNode *expr) override { return nullptr; }
  Operand *SelectAddroflabel(AddroflabelNode *expr) override { return nullptr; }
  Operand *SelectIread(BaseNode *parent, IreadNode *expr) override { return nullptr; }
  Operand *SelectIntconst(MIRIntConst *floatconst) override { return nullptr; }
  Operand *SelectVectorIntconst(MIRVectorIntConst *vintconst) override { return nullptr; }
  Operand *SelectFloatconst(MIRFloatConst *floatconst) override { return nullptr; }
  Operand *SelectDoubleconst(MIRDoubleConst *doubleconst) override { return nullptr; }
  Operand *SelectStrconst(MIRStrConst *strconst) override { return nullptr; }
  Operand *SelectStr16const(MIRStr16Const *strconst) override { return nullptr; }
  void SelectAdd(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType mtype) override {}
  Operand *SelectAdd(BinaryNode *node, Operand *opnd0, Operand *opnd1) override { return nullptr; }
  Operand *SelectShift(BinaryNode *node, Operand *opnd0, Operand *opnd1) override { return nullptr; }
  Operand *SelectRem(BinaryNode *node, Operand *opnd0, Operand *opnd1) override { return nullptr; }
  Operand *SelectSub(BinaryNode *node, Operand *opnd0, Operand *opnd1) override { return nullptr; }
  Operand *CreateImmOperand(PrimType primType, int64 val) override { return nullptr; }
  Operand *CreateZeroOperand(PrimType primType) override { return nullptr; }
  RegOperand *GetOrCreateFramePointerRegOperand() override { return nullptr; }
  RegOperand *GetOrCreateStackBaseRegOperand() override { return nullptr; }
  uint32 GetBaseOffset(SymbolAlloc *symalloc) override { return 0; }
  MemOperand *GetOrCreateMemOpnd(MIRSymbol *symbol, int32 offset, int32 size) override { return nullptr; }
  MemOperand *CreateMemOpnd(RegOperand *baseOpnd, int32 offset, int32 size) override { return nullptr; }
  MemOperand *CreateMemOpnd(RegOperand *baseOpnd, int32 offset, int32 size, MIRSymbol *sym) override { return nullptr; }
  MemOperand *GetPseudoRegisterSpillMemoryOperand(PregIdx i) override { return nullptr; }
  RegType GetRegType(regno_t r) override { return kRegTyUndef; }
  bool IsCalleeSaved(regno_t r) override { return false; }
  bool IsCallerSaved(regno_t r) override { return false; }
  inline bool ShouldSaveFPLR() {
    return (cg->UseFP() || HasCall() || cg->NeedInsertInstrumentationFunction());
  }
  void SetJavaCatchRegno(regno_t regno) {
    ujavaCatch.regno_javaCatch = regno;
  }
  regno_t GetJavaCatchRegno() {
    return ujavaCatch.regno_javaCatch;
  }
  void EmitRefToMethodDesc(Emitter &emitter) override;
  void EmitRefToMethodInfo(Emitter &emitter) override;
  void EmitMethodDesc(Emitter &emitter) override;
  void Emit() override;
};

}  // namespace maplebe

#endif  //  MAPLEBE_INCLUDE_CG_ARK_ARKCGFUNC_H
