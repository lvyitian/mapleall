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

#ifndef MAPLEBE_INCLUDE_CG_ARMCGFUNC_H_
#define MAPLEBE_INCLUDE_CG_ARMCGFUNC_H_

#include "arm_abi.h"
#include "arm_isa.h"
#include "arm_operand.h"
#include "arm_insn.h"
#include "arm_mem_layout.h"
#include "cg_func.h"

namespace maplebe {

class V_RegNode {
 public:
  RegType regtype;
  uint8 size;       // size in bytes
  Armregister reg;  // physical register assigned by register allocation
 public:
  V_RegNode() : regtype(kRegTyUndef), reg(RCC) {}
};

class ArmCGFunc : public CGFunc {
 private:
  uint32 first_mapleir_v_reg_no;       // positioned after physical regs
  uint32 v_reg_count;                  // for assigning a number for each CG virtual register
  uint32 max_reg_count;                // for the current virtual register number limit
  MapleVector<V_RegNode> v_reg_table;  // table of CG's virtual registers indexed by v_reg no
  MapleMap<ArmRegOperand, ArmRegOperand *> hash_regopnd_tb_;
  MapleMap<ArmImmOperand, ArmImmOperand *> hash_immopnd_tb_;
  MapleMap<LabelIdx, LabelOperand *> hash_lblopnd_tb_;
  MapleMap<ArmOfstOperand, ArmOfstOperand *> hash_ofstopnd_tb_;
  MapleMap<StImmOperand, StImmOperand *> hash_stimmopnd_tb_;
  MapleMap<ArmMemOperand, ArmMemOperand *> hash_memopnd_tb_;
  Operand *rcc_;
  ArmRegOperand *fsp_;  // used to point the address of local variables and formal parameters
  Operand *upfsp_;  // used to point to the address of the new function started. for arm used to pointed to upformals
 public:
  ArmListOperand *saved_opnds_;  // this function going to save some registers at prolog and restore at epilog;
  MapleVector<Armregister> formal_reg_list_;  // store the parameters register used by this function

 private:
  MOperator PickStInsn(PrimType primtype);
  MOperator PickStInsn(uint32 bitsize, PrimType rtype, bool exactsize = false);
  MOperator PickLdInsn(PrimType primtype, PrimType rtype);
  MOperator PickLdInsn(uint32 bitsize, PrimType rtype, bool exactsize = false);
  MOperator PickAddInsn(PrimType, bool);
  MOperator PickMpyInsn(PrimType, bool);
  void PickCmovInsn(Opcode, PrimType, MOperator &, MOperator &, bool isels = false);
  MOperator PickCmpInsn(PrimType primtype, Operand *opnd1);
  MOperator PickJmpInsn(Opcode brop, Opcode cmpop, bool isfloat, bool issigned);
  void SelectSubI64(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype);

 public:
  explicit ArmCGFunc(CG *c, MIRFunction *f, BECommon *b, MemPool *mp, MapleAllocator *mallocator)
    : CGFunc(c, f, b, mp, mallocator),
      v_reg_table(mallocator->Adapter()),
      hash_regopnd_tb_(std::less<ArmRegOperand>(), mallocator->Adapter()),
      hash_immopnd_tb_(std::less<ArmImmOperand>(), mallocator->Adapter()),
      hash_lblopnd_tb_(std::less<LabelIdx>(), mallocator->Adapter()),
      hash_ofstopnd_tb_(std::less<ArmOfstOperand>(), mallocator->Adapter()),
      hash_stimmopnd_tb_(std::less<StImmOperand>(), mallocator->Adapter()),
      hash_memopnd_tb_(std::less<ArmMemOperand>(), mallocator->Adapter()),
      rcc_(nullptr),
      fsp_(nullptr),
      upfsp_(nullptr),
      saved_opnds_(nullptr),
      formal_reg_list_(mallocator->Adapter()) {
    first_mapleir_v_reg_no = 100;
    v_reg_count = first_mapleir_v_reg_no + func->pregtab->Size();
    max_reg_count = 80 + v_reg_count;
    v_reg_table.resize(max_reg_count);
    // func->pregtab->pregTable[0] is nullptr, so skip it
    for (uint32 i = 1; i < func->pregtab->Size(); i++) {
      v_reg_table[i + first_mapleir_v_reg_no].regtype = GetRegTyFromPrimTy(func->pregtab->PregFromPregidx(i)->primType);
      v_reg_table[i + first_mapleir_v_reg_no].size = GetPrimTypeSize(func->pregtab->PregFromPregidx(i)->primType);
    }
    memlayout = mp->New<ArmMemLayout>(&becommon, f, mallocator);
    memlayout->SetCurrFunction(this);
  }

  uint32 New_V_Reg(RegType regtype, uint32 siz) {
    if (v_reg_count >= max_reg_count) {
      max_reg_count += 80;
      v_reg_table.resize(max_reg_count);
    }
    v_reg_table[v_reg_count].regtype = regtype;
    v_reg_table[v_reg_count].size = siz;
    return v_reg_count++;
  }

  MOperator PickMovInsn(PrimType primtype);
  MOperator PickMovInsn(RegOperand *, RegOperand *);
  void SelectDassign(DassignNode *stmt, Operand *opnd0) override;
  void SelectRegassign(RegassignNode *stmt, Operand *opnd0) override;
  void SelectAggDassign(DassignNode *stmt) override;
  void SelectIassign(IassignNode *stmt) override;
  void SelectAggIassign(IassignNode *stmt, Operand *lhsaddropnd) override;
  void SelectReturn(NaryStmtNode *stmt, Operand *opnd0) override;
  void SelectCondGoto(LabelOperand *targetopnd, Opcode jmpop, Opcode cmpop, Operand *opnd0, Operand *opnd1,
                      PrimType primType);
  void SelectCondGoto(CondGotoNode *stmt, Operand *opnd0, Operand *opnd1) override;
  void SelectGoto(GotoNode *stmt) override;
  void SelectParmList(StmtNode *narynode, ArmListOperand *srcopnds);
  void SelectCall(CallNode *callnode) override;
  void SelectIcall(IcallNode *icallnode, Operand *fptropnd) override;
  void SelectIntrinCall(IntrinsiccallNode *icallnode) override {
    CG_ASSERT(0, "NYI");
  }

  void SelectComment(CommentNode *comment) override {}

  void Emit() override;
  Operand *SelectDread(base_node_t *parent, AddrofNode *expr) override;
  RegOperand *SelectRegread(base_node_t *parent, RegreadNode *expr) override;
  void SelectAddrof(Operand *result, ArmMemOperand *memopnd);
  Operand *SelectAddrof(AddrofNode *expr) override;
  Operand *SelectAddroffunc(AddroffuncNode *expr) override;
  Operand *SelectIread(base_node_t *parent, IreadNode *expr) override;
  Operand *SelectIntconst(MIRIntConst *expr) override;
  Operand *SelectFloatconst(MIRFloatConst *floatconst) override;
  Operand *SelectDoubleconst(MIRDoubleConst *doubleconst) override;
  Operand *SelectVectorIntconst(MIRVectorIntConst *expr) override {
    CG_ASSERT(0, "NYI");
  }
  Operand *SelectStrconst(MIRStrConst *strconst) override {
    CG_ASSERT(0, "NYI");
  }

  Operand *SelectStr16const(MIRStr16Const *strconst) override {
    CG_ASSERT(0, "NYI");
  }

  Operand *SelectAdd(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectAdd(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType mtype) override;
  Operand *SelectShift(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  Operand *SelectSub(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectSub(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) override;
  Operand *SelectBand(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectBand(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType mtype) override;
  Operand *SelectBior(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectTest(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType dtype) override;
  void SelectBior(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType mtype) override;
  Operand *SelectBxor(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectBxor(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType mtype) override;
  Operand *SelectLand(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  Operand *SelectLor(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  Operand *SelectMin(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectMin(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType mtype) override;
  Operand *SelectMax(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectMax(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType mtype) override;
  void SelectCmpOpi64(Operand *resopnd, Operand *opnd0, Operand *opnd1, Opcode opcode, PrimType prmtype);
  void SelectCmpOp(Operand *resopnd, Operand *opnd0, Operand *opnd1, Opcode opcode, PrimType prmtype);
  Operand *SelectCmpOp(CompareNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectShift(Operand *resopnd, Operand *opnd0, Operand *opnd1, SHIFTDIRECTION direct, PrimType prmtype);
  void SelectShift64(Operand *resopnd, Operand *opnd0, Operand *opnd1, SHIFTDIRECTION direct, PrimType prmtype);
  Operand *SelectMpy(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectMpy(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) override;
  void SelectRem(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype);
  Operand *SelectRem(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectDiv(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) override;
  Operand *SelectDiv(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
  Operand *SelectAbs(UnaryNode *node, Operand *opnd0) override;
  Operand *SelectBnot(UnaryNode *node, Operand *opnd0) override;
  Operand *SelectExtractbits(ExtractbitsNode *node, Operand *opnd0) override;
  Operand *SelectDepositbits(DepositbitsNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectDepositbits(Operand *resopnd, Operand *opnd0, Operand *opnd1, uint32 boffset, uint32 bsize, PrimType rtyp,
                         PrimType dtyp) override;
  Operand *SelectLnot(UnaryNode *node, Operand *opnd0) override;
  Operand *SelectNeg(UnaryNode *node, Operand *opnd0) override;
  Operand *SelectRecip(UnaryNode *node, Operand *opnd0) override;
  Operand *SelectSqrt(UnaryNode *node, Operand *opnd0) override;
  Operand *SelectCeil(TypeCvtNode *node, Operand *opnd0) override;
  Operand *SelectFloor(TypeCvtNode *node, Operand *opnd0) override;
  Operand *SelectRetype(TypeCvtNode *node, Operand *opnd0) override;
  Operand *SelectRound(TypeCvtNode *node, Operand *opnd0) override;
  Operand *SelectCvt(TypeCvtNode *node, Operand *opnd0) override;
  Operand *SelectTrunc(TypeCvtNode *node, Operand *opnd0) override;
  void SelectCvtI2I(Operand *resopnd, Operand *opnd0, PrimType fromtype, PrimType totype);
  void SelectCvtF2F(Operand *resopnd, Operand *opnd0, PrimType fromtype, PrimType totype);
  void SelectFloat2Int(Operand *resopnd, Operand *opnd0, PrimType itype, PrimType ftype);
  void SelectInt2Float(Operand *resopnd, Operand *opnd0, PrimType itype, PrimType ftype);
  Operand *SelectSelect(TernaryNode *node, Operand *opnd0, Operand *opnd1, Operand *opnd2) override;
  void SelectSelect(Operand *resopnd, Operand *condopnd, Operand *trueopnd, Operand *falseopnd, PrimType dtyp,
                    PrimType ctyp) override;
  void SelectRangegoto(RangegotoNode *rangegotonode, Operand *opnd0) override;
  Operand *SelectCopy(Operand *src, PrimType ptype);
  void SelectCopy(Operand *dest, Operand *src, PrimType dtype);
  void SelectCopyImm(Operand *dest, ArmImmOperand *src, PrimType dtype);
  void SelectLibCall(const char *, vector<Operand *> &, PrimType, PrimType, bool is2nd = false);
  Operand *GetTargetRetOperand(PrimType ptype) override;
  Operand *GetOrCreateRflag();
  Operand *CreateOpndOfType(PrimType primtype);
  Operand *CreateOpndOfReg(RegOperand *opnd);
  Operand *CreateRflagOpnd();
  Operand *CreateImmOperand(PrimType primType, int64 val) override;
  Operand *CreateFloatImmOperand(float val);
  Operand *CreateDoubleImmOperand(double val);
  Operand *CreateZeroOperand(PrimType primType) override;
  ArmRegOperand *GetOrCreatePhysicalRegisterOperand(regno_t reg_no, uint8 size, RegType type, uint32 flag = 0);
  LabelOperand *GetOrCreateLabelOperand(LabelIdx labidx);
  ImmOperand *GetOrCreateImmOperand(int64 val, uint8 size, bool is_signed);
  ArmOfstOperand *GetOrCreateOfstOperand(uint32 offset, uint32 size);
  StImmOperand *GetOrCreateStImmOperand(MIRSymbol *st, int64 offset, int32 relocs);

  RegOperand *GetOrCreateFramePointerRegOperand() override {
    return GetOrCreatePhysicalRegisterOperand(R11, 32, kRegTyInt);
  }

  MemOperand *GetOrCreateMemOpnd(MIRSymbol *symbol, int32 offset, int32 size) override;

  inline MemOperand *CreateMemOpnd(Armregister reg, int32 offset, int32 size) {
    ArmRegOperand *base_opnd = GetOrCreatePhysicalRegisterOperand(reg, 32, kRegTyInt);
    return CreateMemOpnd(base_opnd, offset, size);
  }

  MemOperand *CreateMemOpnd(RegOperand *base_opnd, int32 offset, int32 size) override;

  ArmMemOperand *GetOrCreateMemOpnd(ArmMemOperand::ArmAdrMode, int32, RegOperand *, RegOperand *, Operand *, Operand *,
                                    MIRSymbol *);
  ArmMemOperand *GetOrCreateMemOpnd(Armregister reg, int32 offset, int32 size);

  Insn *CreateMoveInsn(Operand *opnd);  // create a move instruction that copy opnd
  void EmitOperand(Operand *, OpndProp *);
  void GenerateProlog(BB *) override;
  void GenerateEpilog(BB *) override;
  Operand *GetHigh32bitsOpnd(Operand *opnd);
  Operand *GetLow32bitsOpnd(Operand *opnd);
  Operand *GetBaseReg(ArmSymbolAlloc *symballoc);
  uint32 GetBaseOffset(ArmSymbolAlloc *symalloc);
  ArmListOperand *GetOrCreateSavedOpnds();
  void InsertCallAtFuncEntry(MIRSymbol *) {}

  // CFI directives related stuffs
  Operand *CreateCfiRegOperand(uint32 reg, uint8 size) override {
    return memPool->New<cfi::RegOperand>(reg - 1, size);
  }

  Insn *GenerateCfiRestores(BB *) override;

  void PrePopulateSSAValuesForFormals(BB *entry_bb) override {
    CG_ASSERT(0, "NYI");
  }

  bool IsCalleeSaved(regno_t r) override {
    CG_ASSERT(0, "NYI");
    return false;
  }

  void SetupFreeRegisters() override {
    CG_ASSERT(0, "NYI");
  }
};

}  // namespace maplebe

#endif  //  MAPLEBE_INCLUDE_CG_ARMCGFUNC_H_
