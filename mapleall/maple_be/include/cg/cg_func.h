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

#ifndef MAPLEBE_INCLUDE_CG_CGFUNC_H
#define MAPLEBE_INCLUDE_CG_CGFUNC_H

#include "be_common.h"
#include "operand.h"
#include "eh_func.h"
#include "mem_layout.h"
#include "cg_bb.h"
#include "reg_alloc.h"
#include "cfi.h"
#include "dbg.h"
#include "ebo.h"
#include "live_analysis.h"
#include "loop.h"
#include "lvar.h"
#include "reaching_definition.h"
#include "schedule.h"
#include "store_load_opt.h"
#include "global_opt.h"
#include "super_bb.h"
#include "cg_cfg.h"

/// MapleIR headers.
#include "mir_parser.h"
#include "mir_function.h"
#include "debug_info.h"

/// Maple MP header
#include "mempool_allocator.h"

/// C++ headers.
#include <cstddef>
#include <functional>
#include <utility>
#include <cstdio>
#include <algorithm>

namespace maplebe {

class CG;
class LiveAnalysis;
class ReachingDefinition;
class Ebo;
class StoreLoadOpt;
class GlobalOpt;
class RegAllocator;
class CgfuncLoops;
class LoopFinder;
class SuperBBBuilder;
class CGCFG;
class Schedule;

struct MemOpndCmp {
  bool operator()(const MemOperand *lhs, const MemOperand *rhs) const {
    if (lhs == rhs) {
      return false;
    }
    return (lhs->Less((Operand *)rhs));
  }
};

class SpillmemoperandSet {
 public:
  MapleSet<MemOperand *, MemOpndCmp> reusespilllocmem;
  MapleAllocator *allocator;
  void Add(MemOperand *op) {
    reusespilllocmem.insert(op);
  }

  void Remove(MemOperand *op) {
    reusespilllocmem.erase(op);
  }

  MemOperand *GetOne() {
    if (!reusespilllocmem.empty()) {
      MemOperand *res = *reusespilllocmem.begin();
      reusespilllocmem.erase(res);
      return res;
    }
    return nullptr;
  }

  SpillmemoperandSet(MapleAllocator *mallocator) : reusespilllocmem(mallocator->Adapter()), allocator(mallocator) {}
  virtual ~SpillmemoperandSet() {}
};

class CGFunc {
  friend class LivenessAnalyser;
  friend class LSRALinearScanRegAllocator;

 private:
  CGFunc &operator=(const CGFunc &p);
  CGFunc(const CGFunc &);

 public:
  enum SHIFTDIRECTION { kShiftLeft, kShiftAright, kShiftLright };
  CG *cg;
  MIRModule &mirModule;
  MIRFunction *func;
  EHFunc *ehfunc;
  uint32 bbcnt;
  uint32 labelIdx;           // local label index number
  LabelNode *start_label;    // start label of the function
  LabelNode *end_label;      // end label of the function
  LabelNode *cleanup_label;  // label to indicate the entry of cleanup code.

  BB *firstbb;
  BB *cleanupbb;
  BB *cleanupEntrybb;
  BB *lastbb;
  BB *curbb;
  BB *commonStartbb;
  BB *commonEndbb;
  BB *dummybb;  // use this bb for add some instructions to bb that is not curbb
  MapleVector<BB *> exitbbsvec;
  MapleMap<LabelIdx, BB *> lab2bbmap;
  BECommon &becommon;
  MemLayout *memlayout;
  MemPool *memPool;
  MapleAllocator *funcscope_allocator_;
  MapleVector<MIRSymbol *> emitstvec_;  // symbol that needs to be emit as a local symbol. i.e, switch table
  std::vector<CgfuncLoops *> loops;
  CGCFG *theCFG;

 protected:
  uint32 first_mapleir_v_reg_no;  // positioned after physical regs
  uint32 first_non_preg_vreg_no;
  uint32 v_reg_count;                       // for assigning a number for each CG virtual register
  uint32 max_reg_count;                     // for the current virtual register number limit
  size_t lsymSize;                          // size of the local symbol table imported
  MapleVector<VirtualRegNode> v_reg_table;  // table of CG's virtual registers indexed by v_reg no
  MapleMap<regno_t, VRegOperand *> vreg_operand_table;
  MapleMap<PregIdx, MemOperand *> preg_spill_mem_operands;
  MapleMap<regno_t, MemOperand *> spillreg_mem_operands;
  MapleSet<MemOperand *> spillreg_mem_operands_adj;
  MapleMap<uint32, SpillmemoperandSet *> reusespillloc_mem;
  LabelIdx first_cggen_labelidx;

  int64 offset_from_cfa_; /* SP offset from Call Frame Address */

#if DEBUG
  MapleMap<PregIdx, StIdx> *pregs_to_vars_map;
#endif
 protected:
  MapleMap<Insn *, CSR_call_info_t> call_info_map;
  RegOperand *aggParamReg;
  int total_insns;
  int32 structCopySize;
  int32 maxParamStackSize;
  bool hasVLAOrAlloca;
  bool hasProEpilogue;
  bool hasNonescapedVar;
  bool isVolLoad;
  bool isVolStore;
  bool isAggParamInReg;
  bool isAfterRegAlloc;
  bool needSplit;
  bool hasTakenLabel;
  bool hasAlloca;
  uint32_t frequency;

 protected:
  // debugging info
  DebugInfo *dbginfo;
  MapleVector<DBGExprLoc *> dbg_callframe_locations;
  int64 dbg_callframe_offset;
  ReachingDefinition *rd;
  SuperBBBuilder *sbb;

 public:
  explicit CGFunc(MIRModule *mod, CG *c, MIRFunction *f, BECommon *bec, MemPool *mp, MapleAllocator *mallocator);

  virtual ~CGFunc();

  const std::string &GetName() const {
    return func->GetName();
  }

  MemPool *GetMemoryPool() {
    return memPool;
  }

  inline void LayoutStackFrame() {
    memlayout->LayoutStackFrame(structCopySize, maxParamStackSize);
  }

  inline bool HasCall() const {
    return func->HasCall();
  }

  inline bool HasVLAOrAlloca() const {
    return hasVLAOrAlloca;
  }

  inline void SetRD(ReachingDefinition *paramRd) {
    rd = paramRd;
  }

  inline bool GetRDStatus() const {
    return (rd != nullptr);
  }

  inline ReachingDefinition *GetRD() {
    return rd;
  }

  inline void SetSBB(SuperBBBuilder *paramSbb) {
    sbb = paramSbb;
  }

  inline SuperBBBuilder *GetSBB() {
    return sbb;
  }

  EHFunc *BuildEHFunc();
  EHFunc *BuildCppEHFunc();

  inline bool HasNonescapedVar() const {
    return hasNonescapedVar;
  }

  void GeneratePrologEpilog();
  void MoveRegargs();
  void MoveVRegargs();
  void IsolateFastPath();
  virtual void Savelockinfo(BB *bb) = 0;
  virtual void GenSavemethodinfoCode(BB *) = 0;

  virtual Insn *GenerateCfiRestores(BB *) = 0;
  virtual void GenerateCleanupCode(BB *) = 0;
  virtual bool NeedCleanup() = 0;
  virtual bool TailCallOpt() {
    return false;
  }

  virtual void GenerateCleanupCodeForExtEpilog(BB *) = 0;

  virtual Operand *GetOrCreateRflag() {
    return nullptr;
  }
  virtual Operand *GetRflag() {
    return nullptr;
  }
  virtual LabelOperand *GetOrCreateLabelOperand(LabelIdx labidx) = 0;

  virtual RegAllocator *NewRegAllocator(CGFunc *cgfunc, MemPool *mp, MapleAllocator *mallocator) = 0;

  virtual CallerSavedRegHandler *NewCallerSavedRegisterHandler(CGFunc *cgfunc,
                                                               MemPool *mp /*, MapleAllocator* mallocator*/) = 0;

  virtual Ebo *NewEbo(CGFunc *cgfunc, MemPool *mp, LiveAnalysis *live, bool beforeRegalloc, const char *phase) = 0;

  virtual OptLocalRef *NewOptLocalRef(CGFunc *cgfunc, MemPool *mp) = 0;

  virtual StoreLoadOpt *NewStoreLoadOpt(CGFunc *cgfunc, MemPool *mp, ReachingDefinition *rd) = 0;

  virtual GlobalOpt *NewGlobalOpt(CGFunc *cgfunc, MemPool *mp) = 0;

  virtual LiveAnalysis *NewLiveAnalysis(CGFunc *cgfunc, MemPool *mp) = 0;

  LoopFinder *NewLoopFinder(CGFunc *cgfunc, MemPool *mp) {
    return mp->New<LoopFinder>(cgfunc, mp);
  }

  virtual ReachingDefinition *NewReachingDefinition(CGFunc *cgfunc, MemPool *mp, MapleAllocator ma,
                                                    LiveAnalysis *live) = 0;

  virtual Schedule *NewSchedule(CGFunc *cgfunc, MemPool *mp, LiveAnalysis *live, const char *phaseName) = 0;

  SuperBBBuilder *NewSuperBBBuilder(CGFunc *cgfunc, MemPool *mp) {
    return mp->New<SuperBBBuilder>(cgfunc, mp);
  }

#if !TARGARK
  void HandleFunction(void);
#endif
  void HandleCondbr(CondGotoNode *stmt);    // br, goto, etc
  void HandleDassign(DassignNode *assign);  // dassign
  void HandleAssertnull(UnaryStmtNode *assertnode);
  void HandleRegassign(StmtNode *assign);          // regassign
  void HandleIassign(StmtNode *iassign);           // iassign
  void HandleCall(CallNode *call);                 // call
  void HandleICall(IcallNode *call);               // icall
  void HandleIntrinCall(IntrinsiccallNode *call);  // intrinsiccall
  void HandleReturn(NaryStmtNode *ret);            // return
  void HandleLabel(LabelNode *lbl);                // Label
  void HandleGoto(GotoNode *gt);                   // Goto
  void HandleIgoto(UnaryStmtNode *igt);            // Igoto
  void HandleRangegoto(RangegotoNode *);
  virtual void MergeReturn() = 0;
  void TraverseAndClearCatchMark(BB *bb);
  void MarkCatchBBs();
  void MarkCleanupEntryBB();
  void MarkCleanup(BB *bb);
  uint32 GetMaxRegNum() const {
    return max_reg_count;
  };
  size_t GetLsymSize() const {
    return lsymSize;
  }
#if !TARGARK
  void DumpCFG(void);
  void DumpCGIR(void);
  void DumpLoop(void);
  void CheckLoop(void);
  void ClearLoopInfo();
#endif
  LabelIdx CreateLabel();
  Operand *HandleExpr(BaseNode *parent, BaseNode *expr);
  Operand *HandleDread(BaseNode *parent, AddrofNode *expr);
  Operand *HandleRegread(BaseNode *parent, RegreadNode *expr);
  Operand *HandleAddrof(BaseNode *parent, AddrofNode *expr);
  Operand *HandleAddroffunc(BaseNode *parent, AddroffuncNode *expr);
  Operand *HandleAddroflabel(BaseNode *parent, AddroflabelNode *expr);
  Operand *HandleIread(BaseNode *parent, IreadNode *expr);
  Operand *HandleConstval(BaseNode *expr, PrimType pty);
  Operand *HandleConststr(BaseNode *expr);
  Operand *HandleConststr16(BaseNode *expr);
  void CreateStartEndLabel();
  void DetermineReturnTypeofCall();
  void PatchLongBranch();
  virtual int32 MaxCondBranchDistance() {
    return INT_MAX;
  }
  virtual void InsertJumpPad(Insn *) {
    return;
  }
  virtual void ConvertJumpToRegJump(Insn *) {
    return;
  }
  // handle rc reset
  virtual void HandleRCCall(bool begin, MIRSymbol *retRef = nullptr) = 0;
  virtual void HandleRetCleanup(NaryStmtNode *retnode) = 0;
  // select stmt
  virtual void SelectDassign(DassignNode *stmt, Operand *opnd0) = 0;
  virtual void SelectRegassign(RegassignNode *stmt, Operand *opnd0) = 0;
  virtual void SelectAssertnull(UnaryStmtNode *stmt) = 0;
  virtual void SelectAggDassign(DassignNode *stmt) = 0;
  virtual void SelectIassign(IassignNode *stmt) = 0;
  virtual void SelectAggIassign(IassignNode *stmt, Operand *lhsaddropnd) = 0;
  virtual void SelectReturn(NaryStmtNode *stmt, Operand *opnd) = 0;
  virtual Operand *SelectIgoto(Operand *opnd0) = 0;
  virtual void SelectCondGoto(CondGotoNode *stmt, Operand *opnd0, Operand *opnd1) = 0;
  virtual void SelectCondSpecial(CondGotoNode *stmt, BaseNode *opnd0) = 0;
  virtual void SelectGoto(GotoNode *stmt) = 0;
  virtual void SelectCall(CallNode *callnode) = 0;
  virtual void SelectIcall(IcallNode *icallnode, Operand *fptropnd) = 0;
  virtual void SelectIntrinCall(IntrinsiccallNode *intrinsiccallnode) = 0;
  virtual void SelectMembar(StmtNode *membar) = 0;
  virtual void SelectComment(CommentNode *comment) = 0;
  virtual Operand *SelectIntrinOp(IntrinsicopNode *ntrinsicopnode) = 0;
  virtual void HandleJavaCatch() {
    return;
  }

  virtual bool CanBBThrow(BB *) = 0;
  // select expr
  virtual Operand *SelectDread(BaseNode *parent, AddrofNode *expr) = 0;
  virtual RegOperand *SelectRegread(BaseNode *parent, RegreadNode *expr) = 0;
  virtual Operand *SelectAddrof(AddrofNode *expr) = 0;
  virtual Operand *SelectAddroffunc(AddroffuncNode *expr) = 0;
  virtual Operand *SelectAddroflabel(AddroflabelNode *expr) = 0;
  virtual Operand *SelectIread(BaseNode *parent, IreadNode *expr) = 0;
  virtual Operand *SelectIntconst(MIRIntConst *floatconst, PrimType pty) = 0;
  virtual Operand *SelectVectorIntconst(MIRVectorIntConst *vintconst) = 0;
  virtual Operand *SelectFloatconst(MIRFloatConst *floatconst) = 0;
  virtual Operand *SelectDoubleconst(MIRDoubleConst *doubleconst) = 0;
  virtual Operand *SelectStrconst(MIRStrConst *strconst) = 0;
  virtual Operand *SelectStr16const(MIRStr16Const *strconst) = 0;
  virtual void SelectAdd(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType mtype) = 0;
  virtual Operand *SelectAdd(BinaryNode *node, Operand *opnd0, Operand *opnd1) = 0;
  virtual Operand *SelectCGArrayElemAdd(BinaryNode *node) {
    return nullptr;
  }

  virtual Operand *SelectShift(BinaryNode *node, Operand *opnd0, Operand *opnd1) = 0;
  virtual void SelectMpy(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
    return;
  }

  virtual Operand *SelectMpy(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
    return nullptr;
  }

  virtual Operand *SelectRem(BinaryNode *node, Operand *opnd0, Operand *opnd1) = 0;
  virtual void SelectDiv(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
    return;
  }

  virtual Operand *SelectDiv(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
    return nullptr;
  }

  virtual Operand *SelectSub(BinaryNode *node, Operand *opnd0, Operand *opnd1) = 0;
  virtual void SelectSub(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
    return;
  }

  virtual Operand *SelectBand(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
    return nullptr;
  }

  virtual void SelectBand(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType mtype) {
    return;
  }

  virtual void SelectTest(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType dtype) {
    return;
  }

  virtual Operand *SelectLand(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
    return nullptr;
  }

  virtual Operand *SelectLor(BinaryNode *node, Operand *opnd0, Operand *opnd1, bool parentIsBr = false) {
    return nullptr;
  }

  virtual void SelectMin(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
    return;
  }

  virtual Operand *SelectMin(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
    return nullptr;
  }

  virtual void SelectMax(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
    return;
  }

  virtual Operand *SelectMax(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
    return nullptr;
  }

  virtual void SelectCmp(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
    return;
  }

  virtual Operand *SelectCmpOp(CompareNode *node, Operand *opnd0, Operand *opnd1) {
    return nullptr;
  }

  virtual Operand *SelectCmpEqExpr(Operand *opnd0, Operand *opnd1, PrimType stype, LabelIdx lbl) {
    return nullptr;
  }

  virtual void SelectCmpNeqExpr(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType stype, LabelIdx lbl) {
    return;
  }

  virtual void SelectCmpEqAssign(Operand *resopnd) {
    return;
  }

  virtual Operand *SelectBior(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
    return nullptr;
  }

  virtual void SelectBior(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
    return;
  }

  virtual Operand *SelectBxor(BinaryNode *node, Operand *opnd0, Operand *opnd1) {
    return nullptr;
  }

  virtual void SelectBxor(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) {
    return;
  }

  virtual Operand *SelectAbs(UnaryNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectBnot(UnaryNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectSext(ExtractbitsNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectZext(ExtractbitsNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectExtractbits(ExtractbitsNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectDepositbits(DepositbitsNode *node, Operand *opnd0, Operand *opnd1) {
    return nullptr;
  }

  virtual void SelectDepositbits(Operand *resopnd, Operand *opnd0, Operand *opnd1, uint32 bitsOffset, uint32 bitsSize,
                                 PrimType rtyp, PrimType dtyp) {
    return;
  }

  virtual Operand *SelectLnot(UnaryNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectNeg(UnaryNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectRecip(UnaryNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectSqrt(UnaryNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectCeil(TypeCvtNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectFloor(TypeCvtNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectRetype(TypeCvtNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectRound(TypeCvtNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectCvt(BaseNode *parent, TypeCvtNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectTrunc(TypeCvtNode *node, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectSelect(TernaryNode *node, Operand *opnd0, Operand *opnd1, Operand *opnd2) {
    return nullptr;
  }

  virtual Operand *SelectMalloc(UnaryNode *call, Operand *opnd0) {
    return nullptr;
  }

  virtual RegOperand *SelectCopy(Operand *src, PrimType stype, PrimType dtype) {
    return nullptr;
  };
  virtual void SelectFPCmpQuiet(Operand *o0, Operand *o1, uint32 dsize) {
    CG_ASSERT(0, "");
    return;
  };
  virtual Operand *SelectAlloca(UnaryNode *call, Operand *opnd0) {
    return nullptr;
  }

  virtual Operand *SelectGCMalloc(GCMallocNode *call) {
    return nullptr;
  }

  virtual Operand *SelectJarrayMalloc(JarrayMallocNode *call, Operand *opnd0) {
    return nullptr;
  }

  virtual void SelectSelect(Operand *resopnd, Operand *condopnd, Operand *trueopnd, Operand *falseopnd, PrimType dtyp,
                            PrimType ctyp) {
    return;
  }

  virtual void SelectRangegoto(RangegotoNode *rangegotonode, Operand *opnd0) {
    return;
  }

  virtual Operand *GetTargetRetOperand(PrimType ptype, int32 sreg) {
    return nullptr;
  }

  virtual Operand *CreateImmOperand(PrimType primType, int64 val) = 0;
  virtual Operand *CreateZeroOperand(PrimType primType) = 0;
  virtual Operand *CreateFPImmZero(PrimType primType) {
    return CreateZeroOperand(primType);
  }

  virtual void Emit() {
    return;
  }

  virtual void EmitMethodDesc(Emitter &emitter){};
  virtual void EmitRefToMethodInfo(Emitter &emitter){};
  virtual void EmitRefToMethodDesc(Emitter &emitter){};
  virtual void GenerateProlog(BB *) {}

  virtual void MoveRegargs(BB *) {}

  virtual void Genstackguard(BB *) {}

  virtual void MoveVRegargs(BB *) {}

  virtual BB *IsolateFastPath(BB *) {
    return nullptr;
  }

  virtual void GenerateEpilog(BB *) {}

  virtual void GenerateEpilogForCleanup(BB *) {}

  virtual void GenerateRet(BB *) {}

  virtual void GenerateYieldpoint(BB *) {}

  virtual void OffsetAdjustmentForFPLR() {}

  virtual void TestInterface() {}

  virtual void RemoveDecRef() {}

  regno_t New_V_Reg(RegType regtype, uint32 siz) {
    if (v_reg_count >= max_reg_count) {
      max_reg_count += 80;
      v_reg_table.resize(max_reg_count);
    }
#if TARGAARCH64 || TARGX86_64 || TARGRISCV64
    if (siz < 4) {
      siz = 4;
    }
    CG_ASSERT(siz == 4 || siz == 8 || siz == 16, "");
#endif
    new (&v_reg_table[v_reg_count]) VirtualRegNode(regtype, uint8_t(siz));
    return v_reg_count++;
  }

  virtual regno_t New_V_Rflag() {
    return 0;
  }

  virtual regno_t AdjustRegno(regno_t regno) {
    return regno;
  }

  RegOperand *CreateVirtualRegisterOperand(regno_t vregNo) {
    CG_ASSERT(vreg_operand_table.find(vregNo) == vreg_operand_table.end() || IsVRegNoForPseudoRegister(vregNo), "");
    uint8 bitsize = static_cast<uint8>((static_cast<uint32>(v_reg_table[vregNo].size)) * BITS_PER_BYTE);
    VRegOperand *res = memPool->New<VRegOperand>(vregNo, bitsize, v_reg_table.at(vregNo).regtype);
    vreg_operand_table[vregNo] = res;
    return res;
  }

  RegOperand *GetOrCreateVirtualRegisterOperand(regno_t vregNo) {
    auto it = vreg_operand_table.find(vregNo);
    return (it != vreg_operand_table.end() ? it->second : CreateVirtualRegisterOperand(vregNo));
  }

  virtual RegOperand *GetOrCreateFramePointerRegOperand() = 0;

  virtual RegOperand *GetOrCreateStackBaseRegOperand() = 0;

  virtual uint32 GetBaseOffset(SymbolAlloc *symalloc) = 0;

  virtual MemOperand *GetOrCreateMemOpnd(MIRSymbol *symbol, int32 offset, int32 size) = 0;

  virtual MemOperand *CreateMemOpnd(RegOperand *baseOpnd, int32 offset, int32 size) = 0;

  virtual MemOperand *CreateMemOpnd(RegOperand *baseOpnd, int32 offset, int32 size, MIRSymbol *sym) = 0;

  // For ebo issue.
  virtual Operand *GetTrueOpnd() {
    return nullptr;
  };
  virtual Operand *GetZeroOpnd(uint32_t size) {
    return nullptr;
  };
  virtual bool IsFrameReg(RegOperand *opnd) {
    return false;
  };
  virtual bool IsEffectivelyCopy(Insn *insn) {
    return false;
  };
  virtual bool OprandRegisterIdentical(Operand *op1, Operand *op2) {
    return false;
  };

  // run insert yieldpoint phase.
  virtual void InsertYieldpoint(){};

  RegOperand *GetVirtualRegisterOperand(regno_t vregNo) {
    auto it = vreg_operand_table.find(vregNo);
    CG_ASSERT(it != vreg_operand_table.end(), "");
    return it->second;
  }

  MIRSymbol *GetRetRefSymbol(BaseNode *expr);
  // CFI related routines
  inline int64 GetOffsetFromCFA() const {
    return offset_from_cfa_;
  }

  // add increment (can be negative) and return the new value
  inline int64 AddtoOffsetFromCFA(int64 delta) {
    offset_from_cfa_ += delta;
    return offset_from_cfa_;
  }

#if !TARGARK
  void GenerateCfiPrologEpilog();

  void GenerateCfiForLSDA(BB *bb, Insn *ipoint);

  Insn *InsertCFIDefCfaOffset(int &cfiOffset /*in-out*/, Insn *insertAfter);

  virtual Operand *CreateCfiRegOperand(uint32 reg, uint8 size) = 0;

  Operand *CreateCfiImmOperand(int64 val, uint8 size) {
    return memPool->New<cfi::ImmOperand>(val, size);
  }

  Operand *CreateCfiStrOperand(const char *str) {
    return memPool->New<cfi::StrOperand>(str);
  }

  Operand *CreateCfiLabelOperand(const char *parent, LabelIdx idx) {
    return memPool->New<cfi::LabelOperand>(parent, idx);
  }
#endif

  Operand *CreateDbgImmOperand(int64 val) {
    return memPool->New<mpldbg::ImmOperand>(val);
  }

  inline uint32_t NumBBs() const {
    return bbcnt;
  }

  inline uint32_t GetMaxVReg() const {
    return v_reg_count;
  }

  bool IsSpecialPseudoRegister(PregIdx spr) const {
    return spr < 0;
  }

  PregIdx GetSpecialPseudoRegisterIndex(PregIdx spr) const {
    return -spr;
  }

  uint32 GetVRegSize(regno_t vrnum) {
    CHECK(vrnum < v_reg_table.size(), "index out of range in GetVRegSize");
    return v_reg_table[vrnum].GetSize();
  }

  BB *CreateNewBBFromBB(BB *origBB) {
    BB *newBB = CreateNewBB();
    newBB->firststmt = origBB->firststmt;
    newBB->laststmt = origBB->laststmt;
    newBB->frequency = frequency;
    origBB->AppendBB(newBB);
    return newBB;
  }

  inline BB *CreateNewBB() {
    return memPool->New<BB>(bbcnt++, funcscope_allocator_);
  }

 protected:
  void SplitCallBB(BB *fromBB) {
    if (fromBB == curbb) {
      return;
    }
    Insn *call = nullptr;
    FOR_BB_INSNS_REV(insn, fromBB) {
      if (insn->IsCall()) {
        call = insn;
        break;
      }
    }
    if (call) {
      fromBB->SetKind(BB::kBBCall);
      Insn *insn = call->next;
      while (insn) {
        Insn *next = insn->next;
        fromBB->RemoveInsn(insn);
        curbb->AppendInsn(insn);
        insn = next;
      }
    }
  }

  template <bool stmt_is_curbb_last_stmt>
  BB *StartNewBBImpl(StmtNode *stmt) {
    BB *newbb = CreateNewBB();
    if (stmt_is_curbb_last_stmt) {
      CG_ASSERT(curbb, "");
      curbb->laststmt = stmt;
      curbb->AppendBB(newbb);
      newbb->firststmt = stmt->GetNext();
    } else {
      newbb->firststmt = stmt;
      if (curbb) {
        if (stmt->GetPrev()) {
          CG_ASSERT(stmt->GetPrev()->GetNext() == stmt, "");
        }
        curbb->laststmt = stmt->GetPrev();
        curbb->AppendBB(newbb);
      }
    }
    // used for handle function, frequency is the laststmt->frequency.
    if (curbb != nullptr) {
      curbb->frequency = frequency;
    } else {
      newbb->frequency = frequency;
    }
    CG_ASSERT(newbb->laststmt == nullptr, "");
    return newbb;
  }

  inline BB *StartNewBB(StmtNode *stmt) {
    BB *bb = curbb;
    if (stmt->GetNext() && stmt->GetNext()->op != OP_label) {
      bb = StartNewBBImpl<true>(stmt);
    }
    return bb;
  }

  regno_t GetVirtualRegNoFromPseudoRegIdx(PregIdx idx) const {
    return regno_t(idx + first_mapleir_v_reg_no);
  }

  PregIdx GetPseudoRegIdxFromVirtualRegNo(regno_t rn) {
    CG_ASSERT(IsVRegNoForPseudoRegister(rn), "");
    return PregIdx(rn - first_mapleir_v_reg_no);
  }

  bool IsVRegNoForPseudoRegister(regno_t vrnum) const {
    // 0 is not allowed for preg index
    uint32 n = static_cast<uint32>(vrnum);
    return (first_mapleir_v_reg_no < n && n < first_non_preg_vreg_no);
  }

  // public:
  VirtualRegNode &GetVirtualRegNodeFromPseudoRegIdx(PregIdx i) {
    return v_reg_table.at(GetVirtualRegNoFromPseudoRegIdx(i));
  }

  PrimType GetTypeFromPseudoRegIdx(PregIdx i) {
    VirtualRegNode &vrn = GetVirtualRegNodeFromPseudoRegIdx(i);
    RegType rtyp = vrn.GetType();
    CG_ASSERT(rtyp == kRegTyInt || rtyp == kRegTyFloat, "");
    uint32 sz = vrn.GetSize();  // in bytes
    CG_ASSERT(sz == sizeof(int32) || sz == sizeof(int64), "");
    return (rtyp == kRegTyInt ? (sz == sizeof(int32) ? PTY_i32 : PTY_i64) : (sz == sizeof(float) ? PTY_f32 : PTY_f64));
  }

  int64 GetPseudoRegisterSpillLocation(PregIdx i) {
    SymbolAlloc *symloc = memlayout->GetSpillLocOfPseduoRegister(i);
    return GetBaseOffset(symloc);
  }

  virtual MemOperand *GetPseudoRegisterSpillMemoryOperand(PregIdx i) = 0;

#if DEBUG
 public:
  void SetPRegsToVarsMap(MapleMap<PregIdx, StIdx> *m) {
    pregs_to_vars_map = m;
  }

  StIdx GetLocalVarReplacedByPreg(PregIdx reg) {
    auto it = pregs_to_vars_map->find(reg);
    return it != pregs_to_vars_map->end() ? it->second : StIdx();
  }

#endif

 public:
  void IncTotalNumberOfInstructions() {
    total_insns++;
  }

  void DecTotalNumberOfInstructions() {
    total_insns--;
  }

  int GetTotalNumberOfInstructions() const {
    return total_insns;
  }

  int32 GetStructCopySize() const {
    return structCopySize;
  }

  int32 GetMaxParamStackSize() const {
    return maxParamStackSize;
  }

  void AllocateLiveIntervalArray();

 private:
#if DEBUG
  void DumpLiveIntervals();
#endif

  virtual RegType GetRegType(regno_t r) = 0;

 public:
  virtual bool IsCalleeSaved(regno_t r) = 0;

  virtual bool IsCallerSaved(regno_t r) = 0;

  // return Register Type
  RegType GetRegisterType(regno_t rnum) {
    CHECK(rnum < v_reg_table.size(), "index out of range in GetVRegSize");
    return v_reg_table[rnum].GetType();
  }

  CSR_call_info_t &GetCSRCallInfo(Insn *callInsn) {
    return call_info_map[callInsn];
  }

  void SetIsAfterRegAlloc() {
    isAfterRegAlloc = true;
  }

  bool IsAfterRegAlloc() {
    return isAfterRegAlloc;
  }

  // Debugging support
  void SetDebugInfo(DebugInfo *di) {
    dbginfo = di;
  }

  void AddDIESymbolLocation(const MIRSymbol *sym, SymbolAlloc *loc);

  virtual void DBGFixCallFrameLocationOffsets(){};

  void TestSuperBBBuilder(MemPool *sbbMp);
  virtual RegType GetRegTyFromPrimTy(PrimType primType) {
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
        return kRegTyFloat;
      default:
        CG_ASSERT(false, "Unexpected pty");
        return kRegTyUndef;
    }
  }

  // See if the symbol is a structure parameter that requires a copy.
  bool IsParamStructCopy(MIRSymbol *symbol) {
    if (symbol->GetStorageClass() == kScFormal &&
        becommon.type_size_table.at(symbol->tyIdx.GetIdx()) > 16) {
      return true;
    }
    return false;
  }

  bool HasTakenLabel() {
    return hasTakenLabel;
  }

  void SetHasTakenLabel() {
    hasTakenLabel = true;
  }

  // For struct parameter that is copied to stack, load the base address.
  // Return the mem opnd with the base address.
  virtual MemOperand *LoadStructCopyBase(MIRSymbol *symbol, int32 offset, int datasize) {
    return nullptr;
  }

};  // class CGFunc

CGFUNCPHASE(CgDoLayoutSF, "layoutstackframe")
CGFUNCPHASE(CgDoCreateLabel, "createstartendlabel")
#if !TARGARK
CGFUNCPHASE(CgDoHandleFunc, "handlefunction")
CGFUNCPHASE(CgMoveRegargs, "moveregargs")
CGFUNCPHASE(CgMoveVRegargs, "movevregargs")
CGFUNCPHASE(CgDoMerge, "merge")
CGFUNCPHASE(CgDoSplit, "split")
CGFUNCPHASE(CgDoGenProEpiLog, "generateproepilog")
CGFUNCPHASE(CgDoOffAdjFPLR, "offsetadjustforfplr")
CGFUNCPHASE(CgFixCFLocOsft, "dbgfixcallframeoffsets")
CGFUNCPHASE(CgFixShortBranch, "fixshortbranch")
CGFUNCPHASE_CANSKIP(CgDoPrePeepHole, "prepeephole")
CGFUNCPHASE_CANSKIP(CgDoPrePeepHole1, "prepeephole1")
CGFUNCPHASE_CANSKIP(CgDoPeepHole0, "peephole0")
CGFUNCPHASE_CANSKIP(CgDoPeepHole, "peephole")
CGFUNCPHASE(CgDoGenCfi, "gencfi")
CGFUNCPHASE(CgYieldpointInsertion, "yieldpoint")
#endif
CGFUNCPHASE(CgDoEmission, "emit")

}  // namespace maplebe

#endif  //  MAPLEBE_INCLUDE_CG_CGFUNC_H
