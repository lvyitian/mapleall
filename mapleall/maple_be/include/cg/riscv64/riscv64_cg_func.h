/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
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

#ifndef MAPLEBE_INCLUDE_CG_AARCH64CGFUNC_H
#define MAPLEBE_INCLUDE_CG_AARCH64CGFUNC_H

#include "cg_func.h"
#include "riscv64_abi.h"
#include "riscv64_operand.h"
#include "riscv64_insn.h"
#include "riscv64_mem_layout.h"
#include "riscv64_ebo.h"
#include "riscv64_live_analysis.h"
#include "riscv64_lvar.h"
#include "riscv64_reg_alloc.h"
#include "riscv64_color_ra.h"
#include "riscv64_store_load_opt.h"
#include "riscv64_global_opt.h"
#include "riscv64_reaching_definition.h"
//#include "riscv64_schedule.h"
#include "mpl_atomic.h"
#include "name_mangler.h"

#define DWARF_SCALAR_REG_BEGIN 0
#define DWARF_FP_REG_BEGIN 64

namespace maplebe {

typedef VirtualRegNode V_RegNode;

class Riscv64CGFunc : public CGFunc {
  friend class CGFunc;
  friend class DefaultO0RegAllocator;
  friend class O1RegAllocator;
  friend class Riscv64LinearScanRegAllocator;
  friend class LSRALinearScanRegAllocator;
  friend class GraphColorRegAllocator;

 private:
  MapleMap<regno_t, RegOperand *> vir_reg_operand_table;  // virtual regnos to virtual register operands map
  MapleMap<Riscv64RegOperand, Riscv64RegOperand *> phy_reg_operand_table;  // machine register operand table
  MapleMap<LabelIdx, LabelOperand *> hash_lblopnd_tb_;
  MapleMap<Riscv64OfstOperand, Riscv64OfstOperand *> hash_ofstopnd_tb_;
  MapleMap<Riscv64MemOperand, Riscv64MemOperand *> hash_memopnd_tb_;
  // Local variables, formal parameters that are passed via registers
  // need offset adjustment after callee-saved registers are known.
  MapleMap<StIdx, Riscv64MemOperand *> memopnds_requiring_offset_adjustment_;
  MapleMap<StIdx, Riscv64MemOperand *> memopnds_for_stkpassed_arguments;
  MapleMap<Riscv64SymbolAlloc *, Riscv64ImmOperand *> immopnds_requiring_offset_adjustment_;
  MapleMap<Riscv64SymbolAlloc *, Riscv64ImmOperand *> immopnds_requiring_offset_adjustment_for_stkarg_;
  MapleMap<Riscv64SymbolAlloc *, Riscv64ImmOperand *> immopnds_requiring_offset_adjustment_for_refloc_;
  MapleMap<Insn *, BaseNode *> callnativemap;
  union {
    regno_t regno_javaCatch;  // For O2.
    Operand *opnd_javaCatch;  // For O0-O1.
  } ujavaCatch;
  enum fpParamState {
    kNotFp,
    kIsFp32bit,
    kIsFp64bit,
    kStateUnknown,
  };
  Operand *rcc_;
  Operand *vary_;
  Operand *fsp_;  // used to point the address of local variables and formal parameters

  int32 current_cfa_;
  int32 mark_cfa_;
  int num_intreg_to_callee_save;
  int num_fpreg_to_callee_save;
  bool fprl_added_to_callee_saved;
  bool used_stp_sub_pair_to_allocate_call_frame;
  bool isLibcore;
  int split_stpldp_base_offset;
  regno_t method_handle_vreg;

  MIRSymbol *reflect_strtab_sym;
  MIRSymbol *reflect_start_hot_strtab_sym;
  MIRSymbol *reflect_both_hot_strtab_sym;
  MIRSymbol *reflect_run_hot_strtab_sym;

 public:
  MapleVector<Riscv64reg_t> callee_saved_regs;
  MapleVector<Riscv64reg_t> formal_reg_list_;  // store the parameters register used by this function
  MapleSet<Riscv64MemOperand *> gen_memopnds_requiring_offset_adjustment_;
                                               // some caller-saved registers.
  unsigned int refCount;  // Ref count number. 0 if function don't have "bl MCC_InitializeLocalStackRef"
  int beginOffset;        // Begin offset based x29.
  Insn *yieldPointInsn;   // The insn of yield point at the entry of the func.

  static const uint32 kParmMemcpySize = 40;

 private:
  MOperator PickStInsn(uint32 bitsize, PrimType rtype, Riscv64isa::memory_ordering_t mo = Riscv64isa::kMoNone);
  MOperator PickLdInsn(uint32 bitsize, PrimType rtype, Riscv64isa::memory_ordering_t mo = Riscv64isa::kMoNone);

  void SelectLoadAcquire(Operand *dest, PrimType dtype, Operand *src, PrimType stype, Riscv64isa::memory_ordering_t mo,
                         bool isDirect);
  void SelectStoreRelease(Operand *dest, PrimType dtype, Operand *src, PrimType stype, Riscv64isa::memory_ordering_t mo,
                          bool isDirect);

  MOperator PickAddInsn(PrimType, bool);
  MOperator PickMpyInsn(PrimType, bool);
  MOperator PickVecDup(PrimType);
  void PickCmovInsn(Opcode, PrimType, MOperator &, MOperator &, bool isels = false);
  MOperator PickJmpInsn(Opcode brop, Opcode cmpop, bool isfloat, bool issigned, bool isZero = false);
  void SelectSubI64(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype);
  bool IsInEpilogBB(BB *bb);
  Operand *GetZeroOpnd(uint32_t size) override;
  bool IsFrameReg(RegOperand *opnd) override;

  inline PrimType GetOperandType(bool isIntty, uint32 dsize, bool isSigned) {
    CG_ASSERT(dsize <= 64, "shouldn't be here");
    CG_ASSERT(!isSigned || isIntty, "");
    return (isIntty ? (dsize == 64 ? (isSigned ? PTY_i64 : PTY_u64) : (isSigned ? PTY_i32 : PTY_u32))
                    : (dsize == 64 ? PTY_f64 : PTY_f32));
  }

  inline RegOperand *LoadIntoRegister(Operand *o, bool isIntty, uint32 dsize, bool asSigned = false) {
    return LoadIntoRegister(o, GetOperandType(isIntty, dsize, asSigned));
  }

  inline RegOperand *LoadIntoRegister(Operand *o, PrimType oty) {
    return (o->IsRegister() ? static_cast<RegOperand *>(o) : SelectCopy(o, oty, oty));
  }

  Riscv64MemOperand *CreateReplacementMemOperand(Riscv64MemOperand *mo, uint32 bitlen, RegOperand *basereg,
                                                 int32 offset);

  Riscv64MemOperand *SplitOffsetWithAddInstruction(Riscv64MemOperand *mo, uint32 bitlen,
                                                   Riscv64reg_t baseReg = Riscv64reg_t::kRinvalid,
                                                   Insn *insn = nullptr, bool isDest = false);
  Riscv64MemOperand *SplitStpLdpOffsetForCalleeSavedWithAddInstruction(Riscv64MemOperand *mo, uint32 bitlen,
                                                                       Riscv64reg_t baseReg = Riscv64reg_t::kRinvalid);

  void CreateCallStructParamFieldPassByStack(int32 symSize, MIRSymbol *sym, uint32 symOffset, RegOperand *addropnd, int32 baseOffset);
  void CreateCallStructParamPassByStack(int32 symSize, MIRSymbol *sym, RegOperand *addropnd, int32 baseOffset);
  Riscv64RegOperand *GenUnalignedSymCallStructParam(Riscv64reg_t reg, MIRSymbol *sym, uint32 memOffset, PrimType pty, RegOperand *addropnd);
  void CreateCallStructParamPassByReg(Riscv64reg_t reg, MemOperand *mopnd, Riscv64ListOperand *srcopnds, CSR_call_info_t &ci, MIRSymbol *sym, int32 offset, fpParamState state, RegOperand *addropnd = nullptr);
  RegOperand *CreateCallStructParamMemcpy(MIRSymbol *sym, RegOperand *addropnd, uint32 structSize, int32 copyOffset, int32 fromOffset);
  Riscv64RegOperand *CreateCallStructParamCopyToStack(uint32 numMemOp, MIRSymbol *sym, RegOperand *addropnd, int32 copyOffset, int32 fromOffset, PLocInfo *ploc);
  void SelectParmList(StmtNode *narynode, Riscv64ListOperand *srcopnds, CSR_call_info_t &ci, bool iscallnative = false);

  Operand *SelectIgoto(Operand *opnd0) override;
  void SelectCondGoto(LabelOperand *targetopnd, Opcode jmpop, Opcode cmpop, Operand *opnd0, Operand *opnd1,
                      PrimType primType, bool signedCond);

  void EmitRiscv64Insn(Insn *insn);
  void EmitCfiInsn(Insn *insn);

  std::string GetReflectString(uint32_t offset);

 public:
  explicit Riscv64CGFunc(MIRModule *mod, CG *c, MIRFunction *f, BECommon *b, MemPool *mp, MapleAllocator *mallocator)
    : CGFunc(mod, c, f, b, mp, mallocator),
      vir_reg_operand_table(std::less<regno_t>(), mallocator->Adapter()),
      phy_reg_operand_table(std::less<Riscv64RegOperand>(), mallocator->Adapter()),
      hash_lblopnd_tb_(std::less<LabelIdx>(), mallocator->Adapter()),
      hash_ofstopnd_tb_(std::less<Riscv64OfstOperand>(), mallocator->Adapter()),
      hash_memopnd_tb_(std::less<Riscv64MemOperand>(), mallocator->Adapter()),
      memopnds_requiring_offset_adjustment_(std::less<StIdx>(), mallocator->Adapter()),
      memopnds_for_stkpassed_arguments(std::less<StIdx>(), mallocator->Adapter()),
      immopnds_requiring_offset_adjustment_(std::less<Riscv64SymbolAlloc *>(), mallocator->Adapter()),
      immopnds_requiring_offset_adjustment_for_stkarg_(std::less<Riscv64SymbolAlloc *>(), mallocator->Adapter()),
      immopnds_requiring_offset_adjustment_for_refloc_(std::less<Riscv64SymbolAlloc *>(), mallocator->Adapter()),
      callnativemap(std::less<Insn *>(), mallocator->Adapter()),
      rcc_(nullptr),
      vary_(nullptr),
      fsp_(nullptr),
      num_intreg_to_callee_save(0),
      num_fpreg_to_callee_save(0),
      fprl_added_to_callee_saved(false),
      used_stp_sub_pair_to_allocate_call_frame(false),
      split_stpldp_base_offset(0),
      method_handle_vreg(-1),
      callee_saved_regs(mallocator->Adapter()),
      formal_reg_list_(mallocator->Adapter()),
      gen_memopnds_requiring_offset_adjustment_(mallocator->Adapter()),
      refCount(0),
      beginOffset(0),
      yieldPointInsn(nullptr) {
    ujavaCatch.regno_javaCatch = 0;
    current_cfa_ = 0;
    mark_cfa_ = -1;

    CGFunc::memlayout = mp->New<Riscv64MemLayout>(b, f, mallocator);
    CGFunc::memlayout->SetCurrFunction(this);

    isLibcore =
      (GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangObjectStr))) != nullptr);

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

  ~Riscv64CGFunc() {}

  MOperator PickMovInsn(PrimType primtype);
  MOperator PickMovInsn(RegOperand *, RegOperand *);
  Ebo *NewEbo(CGFunc *cgfunc, MemPool *mp, LiveAnalysis *live, bool beforeRegalloc, const char *phase) override {
    return mp->New<Riscv64Ebo>(cgfunc, mp, live, beforeRegalloc, phase);
  }

  StoreLoadOpt *NewStoreLoadOpt(CGFunc *cgfunc, MemPool *mp, ReachingDefinition *rd) override {
    return mp->New<Riscv64StoreLoadOpt>(cgfunc, mp, rd);
  }

  GlobalOpt *NewGlobalOpt(CGFunc *cgfunc, MemPool *mp) override {
    return mp->New<Riscv64GlobalOpt>(cgfunc, mp);
  }

  LiveAnalysis *NewLiveAnalysis(CGFunc *cgfunc, MemPool *mp) override {
    return mp->New<Riscv64LiveAnalysis>(cgfunc, mp);
  }

  OptLocalRef *NewOptLocalRef(CGFunc *cgfunc, MemPool *mp) override {
    return mp->New<Riscv64OptLocalRef>(cgfunc, mp);
  }

  ReachingDefinition *NewReachingDefinition(CGFunc *cgfunc, MemPool *mp, MapleAllocator ma,
                                            LiveAnalysis *live) override {
    return mp->New<Riscv64ReachingDefinition>(cgfunc, mp, ma, live);
  }

  Schedule *NewSchedule(CGFunc *cgfunc, MemPool* mp, LiveAnalysis *live, const char *phaseName) override;

  RegAllocator *NewRegAllocator(CGFunc *cgfunc, MemPool *mp, MapleAllocator *mallocator) override {
    if (g->optim_level == 0) {
      return mp->New<DefaultO0RegAllocator>(cgfunc, mallocator);
    } else if (g->optim_level == 1) {
      return mp->New<O1RegAllocator>(cgfunc, mallocator);
    } else {
      if (cg->cgopt_.DoLinearScanRegisterAllocation()) {
        return mp->New<LSRALinearScanRegAllocator>(cgfunc, mallocator);
      } else if (cg->cgopt_.DoColoringBasedRegisterAllocation()) {
        return mp->New<GraphColorRegAllocator>(cgfunc, mallocator);
      } else {
        fprintf(stderr, "Warning: We only support Linear Scan and GraphColor register allocation\n");
      }
      return nullptr;
    }
  }

  CallerSavedRegHandler *NewCallerSavedRegisterHandler(CGFunc *cgfunc, MemPool *mp) override {
    return mp->New<Riscv64CallerSavedRegHandler>(cgfunc);
  }

//  regno_t New_V_Rflag() override {
//    CG_ASSERT(RFLAG < max_reg_count, "CG internal error.");
//    new (&v_reg_table[RFLAG]) VirtualRegNode(kRegTyCc, 4);
//    return RFLAG;
//  }

  regno_t AdjustRegno(regno_t regno) override {
    if (regno < VB32) {
      // Do nothing.
    }
    else if (regno < VB64) {
      regno = regno - VB32 + V0;
    }
    else if (regno < kMaxRegNum) {
      regno = regno - VB64 + V0;
    }
    return regno;
  }
  VectorType PickVectorType(SubRegType subRegty) {
    switch (subRegty) {
      case kSubRegTyUndef:
        return kVecNone;
      case kSubRegTyInt32:
      case kSubRegTyFloat32:
        return kVecSingle;
      case kSubRegTyInt64:
      case kSubRegTyFloat64:
        return kVecDouble;
    }
  }

  MOperator PickMovInsn(uint32_t bitlen, RegType rtype);
  void Savelockinfo(BB *bb) override;
  void GenSavemethodinfoCode(BB *bb) override;
  void GenNonescapedobjcleanup();
  void HandleRCCall(bool begin, MIRSymbol *retRef = nullptr) override;
  bool GenRetCleanup(IntrinsiccallNode *cleanupnode);
  void HandleRetCleanup(NaryStmtNode *retnode) override;
  void HandleParamRCDec();
  void MergeReturn() override;

  void SelectDassign(DassignNode *stmt, Operand *opnd0) override;
  void SelectRegassign(RegassignNode *stmt, Operand *opnd0) override;
  void SelectAssertnull(UnaryStmtNode *stmt) override;
  void SelectAggDassign(DassignNode *stmt) override;
  void SelectIassign(IassignNode *stmt) override;
  void SelectAggIassign(IassignNode *stmt, Operand *lhsaddropnd) override;
  void SelectReturn(NaryStmtNode *stmt, Operand *opnd0) override;
  void SelectCondGoto(CondGotoNode *stmt, Operand *opnd0, Operand *opnd1) override;
  void SelectCondSpecial(CondGotoNode *stmt, BaseNode *opnd0) override;
  void SelectGoto(GotoNode *stmt) override;
  void SelectCall(CallNode *callnode) override;
  void SelectIcall(IcallNode *icallnode, Operand *fptropnd) override;
  void SelectIntrinCall(IntrinsiccallNode *icallnode) override;
  void SelectMembar(StmtNode *membar) override;

  void SelectComment(CommentNode *comment) override;

  void HandleJavaCatch() override;

  bool CanBBThrow(BB *) override;

  Operand *SelectDread(BaseNode *parent, AddrofNode *expr) override;
  RegOperand *SelectRegread(BaseNode *parent, RegreadNode *expr) override;

  void SelectAddrof(Operand *result, StImmOperand *stimm);
  void SelectAddrof(Operand *result, Riscv64MemOperand *memopnd);
  Operand *SelectAddrof(AddrofNode *expr) override;
  Operand *SelectAddroffunc(AddroffuncNode *expr) override;
  Operand *SelectAddroflabel(AddroflabelNode *expr) override;

  Operand *SelectIread(BaseNode *parent, IreadNode *expr) override;

  Operand *SelectIntconst(MIRIntConst *expr, PrimType pty) override;
  Operand *SelectFloatconst(MIRFloatConst *floatconst) override;
  Operand *SelectDoubleconst(MIRDoubleConst *doubleconst) override;
  Operand *SelectVectorIntconst(MIRVectorIntConst *expr) override;
  Operand *SelectStrconst(MIRStrConst *strconst) override;
  Operand *SelectStr16const(MIRStr16Const *strconst) override;

  void SelectAdd(Operand *reso, Operand *o0, Operand *o1, PrimType mtype) override;
  void SelectVecAdd(Operand *reso, Operand *o0, Operand *o1, PrimType mtype);
  Operand *SelectAdd(BinaryNode *node, Operand *o0, Operand *o1) override;
  Operand *SelectCGArrayElemAdd(BinaryNode *node) override;
  Operand *SelectShift(BinaryNode *node, Operand *o0, Operand *o1) override;
  Operand *SelectSub(BinaryNode *node, Operand *o0, Operand *o1) override;
  void SelectSub(Operand *reso, Operand *o0, Operand *o1, PrimType prmtype) override;
  Operand *SelectBand(BinaryNode *node, Operand *o0, Operand *o1) override;
  void SelectBand(Operand *reso, Operand *o0, Operand *o1, PrimType mtype) override;
  void SelectTest(Operand *reso, Operand *o0, Operand *o1, PrimType dtype) override;
  Operand *SelectBior(BinaryNode *node, Operand *o0, Operand *o1) override;
  void SelectBior(Operand *reso, Operand *o0, Operand *o1, PrimType mtype) override;
  Operand *SelectBxor(BinaryNode *node, Operand *o0, Operand *o1) override;
  void SelectBxor(Operand *reso, Operand *o0, Operand *o1, PrimType mtype) override;

  Operand *SelectLand(BinaryNode *node, Operand *o0, Operand *o1) override;
  Operand *SelectLor(BinaryNode *node, Operand *o0, Operand *o1, bool parentIsBr = false) override;
  Operand *SelectMin(BinaryNode *node, Operand *o0, Operand *o1) override;
  void SelectMin(Operand *reso, Operand *o0, Operand *o1, PrimType mtype) override;
  Operand *SelectMax(BinaryNode *node, Operand *o0, Operand *o1) override;
  void SelectMax(Operand *reso, Operand *o0, Operand *o1, PrimType mtype) override;
  void SelectFMinFMax(Operand *reso, Operand *o0, Operand *o1, bool is64bits, bool isMin);
  void SelectCmpOp(Operand *reso, Operand *o0, Operand *o1, Opcode opcode, PrimType prmtype);

  Operand *SelectCmpOp(CompareNode *node, Operand *o0, Operand *o1) override;

  void SelectRiscv64Cmp(Operand *o, Operand *i, bool isIntty, uint32 dsize);
  void SelectFPCmpQuiet(Operand *o0, Operand *o1, uint32 dsize) override;
  void SelectShift(Operand *reso, Operand *o0, Operand *o1, SHIFTDIRECTION direct, PrimType prmtype);
  void SelectShift64(Operand *reso, Operand *o0, Operand *o1, SHIFTDIRECTION direct, PrimType prmtype);
  Operand *SelectMpy(BinaryNode *node, Operand *o0, Operand *o1) override;
  void SelectMpy(Operand *reso, Operand *o0, Operand *o1, PrimType prmtype) override;
  // method description contains method information which is metadata for reflection.
  void EmitMethodDesc(Emitter &emitter) override;
  void EmitRefToMethodDesc(Emitter &emitter) override;
  void EmitRefToMethodInfo(Emitter &emitter) override;
  void Emit() override;
  void EmitFullLSDA();
  void EmitFastLSDA();
  void EmitBBHeaderLabel(const char *name, LabelIdx labidx);
  void EmitPersonalityCfi();
  void EmitCfiDirectives(Insn *insn, bool inEpilog);
  void CheckImmMemSize();
  MemOperand *AdjustMemOperandIfOffsetOutOfRange(MemOperand *memopnd, regno_t regno, Insn *insn,
                                                 Riscv64reg_t regnum, uint8 &isOutOfRange, bool isDest = false);
  // run insert yieldpoint phase.
  void InsertYieldpoint() override;
  bool IsImmediateOffsetOutOfRange(Riscv64MemOperand *mo, uint32 bitlen);
  bool SizeIsRight(Insn *insn);
  bool CallIsVararg(StmtNode *narynode, uint32 &namedFormals, BB *bb);

 private:
  void SelectRem(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype, bool issigned, bool is64bits);

 public:
  Operand *SelectRem(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;

  void SelectDiv(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) override;
  Operand *SelectDiv(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  Operand *SelectAbs(UnaryNode *node, Operand *opnd0) override;
  Operand *SelectBnot(UnaryNode *node, Operand *opnd0) override;
  Operand *GenerateSext(Operand *resopnd, Operand *opnd0, PrimType ptype, uint32 bitSize);
  Operand *SelectSext(ExtractbitsNode *node, Operand *opnd0) override;
  Operand *SelectZeroSignExt(Operand *opnd0, PrimType ptype);
  Operand *GenerateZext(Operand *resopnd, Operand *opnd0, PrimType ptype, uint32 bitSize);
  Operand *SelectZext(ExtractbitsNode *node, Operand *opnd0) override;
  Operand *SelectExtractbits(ExtractbitsNode *node, Operand *opnd0) override;
  Operand *SelectDepositbits(DepositbitsNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectDepositbits(Operand *resopnd, Operand *opnd0, Operand *opnd1, uint32 bitsOffset, uint32 bitsSize, PrimType rtyp,
                         PrimType dtyp) override;
  Operand *SelectLnot(UnaryNode *node, Operand *opnd0) override;
  Operand *SelectNeg(UnaryNode *node, Operand *opnd0) override;
  void SelectNeg(Operand *dest, Operand *opnd0, PrimType prmtype);
  void SelectMvn(Operand *dest, Operand *opnd0, PrimType prmtype);
  Operand *SelectRecip(UnaryNode *node, Operand *opnd0) override;
  Operand *SelectSqrt(UnaryNode *node, Operand *opnd0) override;
  Operand *SelectCeil(TypeCvtNode *node, Operand *opnd0) override;
  Operand *SelectFloor(TypeCvtNode *node, Operand *opnd0) override;
  Operand *SelectRetype(TypeCvtNode *node, Operand *opnd0) override;
  Operand *SelectRound(TypeCvtNode *node, Operand *opnd0) override;
  Operand *SelectCvt(BaseNode *parent, TypeCvtNode *node, Operand *opnd0) override;
  Operand *SelectTrunc(TypeCvtNode *node, Operand *opnd0) override;
  bool IfParamVRegOperand(Operand *opnd);

 private:
  void SelectCvtInt2Int(BaseNode *parent, Operand *&resopnd, Operand *opnd0, PrimType fromtype, PrimType totype);
  void SelectCvtFloat2Float(Operand *resopnd, Operand *opnd0, PrimType fromtype, PrimType totype);
  void SelectCvtFloat2Int(Operand *resopnd, Operand *opnd0, PrimType itype, PrimType ftype);
  void SelectCvtInt2Float(Operand *resopnd, Operand *opnd0, PrimType itype, PrimType ftype);
  int64 GetOrCreatSpillRegLocation(regno_t vrnum) {
    Riscv64SymbolAlloc *symloc = static_cast<Riscv64SymbolAlloc *>(memlayout->GetLocOfSpillRegister(vrnum));
    int64 offset = GetBaseOffset(symloc);
    return offset;
  }

  void SelectMPLClinitCheck(IntrinsiccallNode *);
  void SelectCVaStart(IntrinsiccallNode *);
  Operand *SelectCclz(IntrinsicopNode *);
  Operand *SelectCctz(IntrinsicopNode *);
  Operand *SelectIntrinOp(IntrinsicopNode *intrinopnode) override;

 public:
  Operand *SelectSelect(TernaryNode *node, Operand *opnd0, Operand *opnd1, Operand *opnd2) override;
  Operand *SelectMalloc(UnaryNode *call, Operand *opnd0) override;
  Operand *SelectAlloca(UnaryNode *call, Operand *opnd0) override;
  Operand *SelectGCMalloc(GCMallocNode *call) override;
  Operand *SelectJarrayMalloc(JarrayMallocNode *call, Operand *opnd0) override;
  void SelectSelect(Operand *resopnd, Operand *condopnd, Operand *trueopnd, Operand *falseopnd, PrimType dtyp,
                    PrimType ctyp) override;
  void SelectRiscv64SelectOp(Operand *dest, Operand *opnd0, Operand *opnd1, Operand *trueOpnd, Operand *falseOpnd,
                           Opcode opcode, uint32 dsize, bool unsignedInt);
  void SelectRangegoto(RangegotoNode *rangegotonode, Operand *opnd0) override;
  RegOperand *SelectCopy(Operand *src, PrimType stype, PrimType dtype) override;
  void SelectCopy(Operand *dest, PrimType dtype, Operand *src, PrimType stype);
  RegOperand *MoveImm16Bits(uint32 val);
  RegOperand *CombineImmBits(Operand *dest, Operand *upper, Operand *lower, uint32 shift);
  void SelectCopyImm(Operand *dest, Riscv64ImmOperand *src, PrimType dtype);
  Operand *SelectCopyToVecRegister(Operand *, PrimType, PrimType);
  void SelectLibCall(const char *, vector<Operand *> &, PrimType, PrimType, bool is2nd = false);
  Operand *GetTargetRetOperand(PrimType ptype, int32 sreg) override;
  Operand *GetOrCreatevaryreg();
  RegOperand *CreateRegisterOperandOfType(PrimType primtype);
  RegOperand *CreateRegisterOperandOfType(RegType regtype, uint32 bytelen);
  RegOperand *CreateRflagOperand();

  RegOperand *GetOrCreateSpecialRegisterOperand(PregIdx sregidx, PrimType primType = PTY_i64);

  MemOperand *GetOrCreatSpillMem(regno_t vrnum, int32 spillOffset = 0);
  void FreeSpillRegMem(regno_t vrnum);
  void TestInterface() override;
  Riscv64RegOperand *GetOrCreatePhysicalRegisterOperand(Riscv64reg_t regNo, uint8 size, RegType type, uint32 flag = 0,
                                                        VectorType vecType = kVecNone, int vecPos = 0);

  LabelOperand *GetOrCreateLabelOperand(LabelIdx labidx) override;
  LabelOperand *CreateFuncLabelOperand(MIRSymbol *func);

  Riscv64ImmOperand *CreateImmOperand(PrimType primType, int64 val) override {
    return CreateImmOperand(val, GetPrimTypeBitSize(primType), IsSignedInteger(primType));
  }

  Operand *CreateZeroOperand(PrimType primType) override {
    CG_ASSERT(false, "NYI");
    return nullptr;
  }

  Operand *CreateFPImmZero(PrimType primType) override {
    return GetOrCreateFpZeroOperand(GetPrimTypeBitSize(primType));
  }

  // create an integer immediate operand
  inline Riscv64ImmOperand *CreateImmOperand(int64 val, uint8 size, bool isSigned, bool isVary = false,
                                             bool isFmov = false) {
    return memPool->New<Riscv64ImmOperand>(val, size, isSigned, isVary, isFmov);
  }

  inline ImmFPZeroOperand *GetOrCreateFpZeroOperand(uint8 size) {
    return ImmFPZeroOperand::allocate(size);
  }

  Riscv64OfstOperand *GetOrCreateOfstOpnd(uint32 offset, uint32 size);

  Riscv64OfstOperand *CreateOfstOpnd(uint32 offset, uint32 size) {
    return memPool->New<Riscv64OfstOperand>(offset, size);
  }

  inline StImmOperand *CreateStImmOperand(MIRSymbol *st, int64 offset, int32 relocs) {
    return memPool->New<StImmOperand>(st, offset, relocs);
  }

  RegOperand *GetOrCreateFramePointerRegOperand() override {
    return GetOrCreateStackBaseRegOperand();
  }

  RegOperand *GetOrCreateStackBaseRegOperand() override {
    if (cg->UseFP() || HasVLAOrAlloca()) {
      return GetOrCreatePhysicalRegisterOperand(RFP, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
    } else {
      return GetOrCreatePhysicalRegisterOperand(RSP, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
    }
  }

  RegOperand *GenStructParamIndex(RegOperand *base, BaseNode *indexExpr, int shift);

  MemOperand *GetOrCreateMemOpnd(MIRSymbol *symbol, int32 offset, int32 size) override;

  MemOperand *GetOrCreateArgMemOpnd(MIRSymbol *symbol, int32 offset, int32 size);

  Riscv64MemOperand *GetOrCreateMemOpnd(int32, RegOperand *, RegOperand *,
                                        Operand *, MIRSymbol *);

  inline MemOperand *CreateMemOpnd(Riscv64reg_t reg, int32 offset, int32 size) {
    Riscv64RegOperand *baseOpnd = GetOrCreatePhysicalRegisterOperand(reg, SIZEOFPTR * BITS_PER_BYTE, kRegTyInt);
    return CreateMemOpnd(baseOpnd, offset, size);
  }

  MemOperand *CreateMemOpnd(RegOperand *baseOpnd, int32 offset, int32 size) override;

  MemOperand *CreateMemOpnd(RegOperand *baseOpnd, int32 offset, int32 size, MIRSymbol *sym) override;

  MemOperand *CreateMemOpnd(PrimType ptype, BaseNode *parent, BaseNode *addrExpr, int offset = 0,
                            Riscv64isa::memory_ordering_t mo = Riscv64isa::kMoNone);

  inline int32 GetCurrentCFA() {
    return current_cfa_;
  }

  inline void SetCurrentCFA(int32 v) {
    current_cfa_ = v;
  }

  inline int32 GetMarkCFA() {
    return mark_cfa_;
  }

  inline void SetMarkCFA(int32 v) {
    mark_cfa_ = v;
  }

  Operand *GetOrCreateFuncNameOpnd(MIRSymbol *symbol);
  Insn *CreateMoveInsn(Operand *opnd);  // create a move instruction that copy opnd
  void ReplaceMachedOperand(Insn *orig, Insn *target, RegOperand *match, bool replaceOrigSrc);
  void ForwardPropagateAndRename(Insn *mv, Insn *ld, BB *terminateBb);
  bool BackwardFindDependency(BB *ifbb, BB *returnbb, RegOperand *tgtOpnd, Insn *&ld, Insn *&mov, Insn *&depMov,
                              std::list<Insn *> &list);
  void EmitOperand(Operand *, OpndProp *);
  void GenerateProlog(BB *) override;
  void MoveRegargs(BB *) override;
  void Genstackguard(BB *) override;
  void MoveVRegargs(BB *) override;
  BB *IsolateFastPath(BB *) override;
  void GenerateEpilog(BB *) override;
  void GenerateEpilogForCleanup(BB *) override;
  void GenerateRet(BB *bb) override;
  void GenerateYieldpoint(BB *) override;
  void GenerateCleanupCode(BB *) override;
  bool NeedCleanup() override;

  void GenerateCleanupCodeForExtEpilog(BB *) override;

  Operand *GetBaseReg(Riscv64SymbolAlloc *symballoc);
  uint32 GetBaseOffset(SymbolAlloc *symalloc) override;

  inline Operand *CreateCommentOperand(const char *s) {
    return memPool->New<CommentOperand>(s);
  }

  inline Operand *CreateCommentOperand(const std::string s) {
    return memPool->New<CommentOperand>(s.c_str());
  }

  inline Operand *CreateCommentOperand(const MapleString s) {
    return memPool->New<CommentOperand>(s.c_str());
  }

  inline void AddtoCalleeSaved(Riscv64reg_t reg) {
    MapleVector<Riscv64reg_t>::iterator it;
    for (it = callee_saved_regs.begin(); it != callee_saved_regs.end(); ++it)
      if (*it == reg) {
        return;
      }
    callee_saved_regs.push_back(reg);
    CG_ASSERT((Riscv64isa::IsGPRegister(reg) || Riscv64isa::IsFPSIMDRegister(reg)), "Int or FP registers are expected");
    if (Riscv64isa::IsGPRegister(reg)) {
      ++num_intreg_to_callee_save;
    } else {
      ++num_fpreg_to_callee_save;
    }
  }

  inline int32 SizeOfCalleeSaved() {
    return ((num_intreg_to_callee_save + num_fpreg_to_callee_save) * kIntregBytelen);
  }

  inline bool IsCalleeSavedPaired() {
    return (((num_intreg_to_callee_save + num_fpreg_to_callee_save) & 0x1) == 0);
  }

  void DBGFixCallFrameLocationOffsets() override;

  inline bool ShouldSaveFPLR() {
    return (cg->UseFP() || HasCall() || cg->NeedInsertInstrumentationFunction());
  }

  inline void NoteFPLRAddedToCalleeSavedList() {
    fprl_added_to_callee_saved = true;
  }

  inline bool IsFPLRAddedToCalleeSavedList() {
    return fprl_added_to_callee_saved;
  }

  inline bool UsedStpSubPairForCallFrameAllocation() {
    return used_stp_sub_pair_to_allocate_call_frame;
  }

  inline MapleVector<Riscv64reg_t> &GetCalleeSavedRegs() {
    return callee_saved_regs;
  }

  void OffsetAdjustmentForFPLR() override;

  void AppendInstructionPushSingle(Riscv64reg_t reg, RegType rty, int offset);

  void AppendInstructionPushPair(Riscv64reg_t reg0, Riscv64reg_t reg1, RegType rty, int offset);

  void AppendInstructionPushRZRPair(Riscv64reg_t reg0, Riscv64reg_t reg1, int offset);

  void AppendInstructionAllocateCallFrame(Riscv64reg_t reg0, Riscv64reg_t reg1, RegType rty);

  void AppendInstructionAllocateCallFrameDebug(Riscv64reg_t reg0, Riscv64reg_t reg1, RegType rty);

  void AppendInstructionPopSingle(Riscv64reg_t reg, RegType rty, int offset);

  void AppendInstructionPopPair(Riscv64reg_t reg0, Riscv64reg_t reg1, RegType rty, int offset);

  void AppendInstructionDeallocateCallFrame(Riscv64reg_t reg0, Riscv64reg_t reg1, RegType rty);

  void AppendInstructionDeallocateCallFrameDebug(Riscv64reg_t reg0, Riscv64reg_t reg1, RegType rty);

  void AppendInstructionStackCheck(Riscv64reg_t reg, RegType rty, int offset);

  void GeneratePushRegs();

  void GeneratePopRegs();

  Riscv64MemOperand *CreateStkTopOpnd(int32 offset, int32 size);

  // if offset < 0, allocation; otherwise, deallocation
  Riscv64MemOperand *CreateCallFrameOperand(int32 offset, int32 size);

  inline bool UseFP() {
    return cg->UseFP();
  }

  void AppendCall(MIRSymbol *func);

  void AppendJump(MIRSymbol *func);

// riscv assembler does the long branch patch
  int32 MaxCondBranchDistance() override {
    return Riscv64Abi::kMaxInstrForCondBr;
  }

  void InsertJumpPad(Insn *insn) override;

  void ConvertJumpToRegJump(Insn *insn) override;

  // CFI directives related stuffs
  Operand *CreateCfiRegOperand(uint32 reg, uint8 size) override {
    // Having kRinvalid=0 (see riscv64_isa.h) means
    // each register gets assigned an id number one greater than
    // its physical number
    if (reg < V0) {
      return memPool->New<cfi::RegOperand>(reg - R0 + DWARF_SCALAR_REG_BEGIN, size);
    } else {
      return memPool->New<cfi::RegOperand>(reg - V0 + DWARF_FP_REG_BEGIN, size);
    }
  }

  Insn *GenerateCfiRestores(BB *) override;

  // Store the address of `symbol` into the GCTIB field of a GC object.
  // Used right after object allocation.
  void SelectStoreGCTIB(RegOperand *obj, std::string symbol);

  void SetJavaCatchRegno(regno_t regno) {
    ujavaCatch.regno_javaCatch = regno;
  }

  regno_t GetJavaCatchRegno() {
    return ujavaCatch.regno_javaCatch;
  }

  void SetJavaCatchOpnd(Operand *opnd) {
    ujavaCatch.opnd_javaCatch = opnd;
  }

  Riscv64reg_t GetReturnRegisterNumber();

  RegType GetRegTyFromPrimTy(PrimType pty) override {
    return GetRegTyFromPrimTyRiscv64(pty);
  }

  RegType GetRegTyFromPrimTyRiscv64(PrimType primType) {
   switch (primType) {
      case PTY_u1:
      case PTY_i8:
      case PTY_u8:
      case PTY_i16:
      case PTY_u16:
      case PTY_i32:
      case PTY_u32:
      case PTY_i64:
      case PTY_u64:
      case PTY_a32:
      case PTY_a64:
      case PTY_ptr:
        return kRegTyInt;
      case PTY_f32:
      case PTY_f64:
      case PTY_v2i64: // simd uses the alias register bank of float
      case PTY_v4i32:
      case PTY_v8i16:
      case PTY_v16i8:
      case PTY_v2f64:
      case PTY_v4f32:
        return kRegTyFloat;
      default:
        return kRegTyUndef;
    }
  }

  /*
     MemOperand*
     GetorCreatSpillRegMemoryOperand(regno_t vrnum) override;
   */
 private:
  // Helper functions for translating complex Maple IR instructions/inrinsics
  void SelectDassign(StIdx stIdx, FieldID fieldID, PrimType rhsPtyp, Operand *opnd0);

  LabelIdx CreateLabeledBB(StmtNode *stmt);

  void SaveReturnValueInLocal(CallReturnVector *retvals, size_t index, PrimType pty, Operand *value,
                              StmtNode *parentStmt);

  // Translation for load-link store-conditional, and atomic RMW operations.
  MemOrd OperandToMemOrd(Operand *opnd);
  MOperator PickLoadStoreExclInsn(int byteP2size, bool store, bool acqRel);
  RegOperand *SelectLoadExcl(PrimType valPty, Riscv64MemOperand *loc, bool acquire);
  RegOperand *SelectStoreExcl(PrimType valPty, Riscv64MemOperand *loc, RegOperand *newVal, bool release);
  bool NeedAdjustOffSet(Riscv64Insn *insn);

  MemOperand *GetPseudoRegisterSpillMemoryOperand(PregIdx i) override;

  RegType GetRegType(regno_t r) override {
    CG_ASSERT(Riscv64isa::IsPhysicalRegister(r), "");
    return Riscv64isa::GetRegType(static_cast<Riscv64reg_t>(r));
  }

  bool IsCalleeSaved(regno_t r) override {
    return Riscv64Abi::IsCalleeSavedReg(Riscv64reg_t(r));
  }

  bool IsCallerSaved(regno_t r) override {
    return !IsCalleeSaved(r);
  }

  bool HasStackLoadStore();

  bool HasCall();

  bool HasClinit();

  bool HasLoop();

  bool TailCallOpt() override;

  bool OptimizeTailBB(BB *bb, set<Insn *> &callInsns);

  void TailCallBBOpt(BB *exitBB, set<Insn *> &callInsns);

  bool TestPredsOfRetBB(BB *exitBB);

  bool IsCommentBB(BB *bb);

  void InitialSpillSlot(BB *bb);

  MemOperand *LoadStructCopyBase(MIRSymbol *symbol, int32 offset, int datasize) override;

  void ReplaceLargeStackOffsetImm(Insn *insn);
};

}  // namespace maplebe

#endif  //  MAPLEBE_INCLUDE_CG_AARCH64CGFUNC_H
