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

#ifndef MAPLEBE_INCLUDE_CG_X86CGFUNC_H_
#define MAPLEBE_INCLUDE_CG_X86CGFUNC_H_

#include "x86_abi.h"
#include "x86_isa.h"
#include "x86_operand.h"
#include "x86_insn.h"
#include "cg_func.h"

namespace maplebe {

class V_RegNode {
 public:
  RegType regtype;
  uint8 size;       // size in bytes
  X86register reg;  // physcial register assigned by register allocation
 public:
  V_RegNode() : regtype(kRegTyUndef), reg(RCC) {}
};

class X86CGFunc : public CGFunc {
 private:
  uint32 first_mapleir_v_reg_no;       // positioned after physical regs
  uint32 v_reg_count;                  // for assigning a number for each CG virtual register
  uint32 max_reg_count;                // for the current virtual register number limit
  MapleVector<V_RegNode> v_reg_table;  // table of CG's virtual registers indexed by v_reg no
  MapleMap<X86RegOperand, X86RegOperand *> hash_regopnd_tb_;
  MapleMap<X86ImmOperand, X86ImmOperand *> hash_immopnd_tb_;
  MapleMap<LabelIdx, LabelOperand *> hash_lblopnd_tb_;
  MapleMap<X86OfstOperand, X86OfstOperand *> hash_ofstopnd_tb_;
  MapleMap<StImmOperand, StImmOperand *> hash_stimmopnd_tb_;
  MapleMap<X86MemOperand, X86MemOperand *> hash_memopnd_tb_;

 private:
  MOperator PickStInsn(PrimType primtype);
  MOperator PickStInsn(uint32 bitsize, PrimType rtype, bool exactsize = false);
  MOperator PickLdInsn(PrimType primtype, PrimType rtype);
  MOperator PickLdInsn(uint32 bitsize, PrimType rtype, bool exactsize = false);
  MOperator PickAddInsn(PrimType, bool);
  MOperator PickMpyInsn(PrimType, bool);
  MOperator PickCmovInsn(bool, bool, Opcode);
  MOperator PickCmpInsn(PrimType primtype, Operand *opnd1);
  MOperator PickSubInsn(PrimType primtype, bool ismem);

 public:
  explicit X86CGFunc(CG *c, MIRFunction *f, BECommon *b, MemPool *mp, MapleAllocator *mallocator)
    : CGFunc(c, f, b, mp, mallocator),
      v_reg_table(mallocator->Adapter()),
      hash_regopnd_tb_(std::less<X86RegOperand>(), mallocator->Adapter()),
      hash_immopnd_tb_(std::less<X86ImmOperand>(), mallocator->Adapter()),
      hash_lblopnd_tb_(std::less<LabelIdx>(), mallocator->Adapter()),
      hash_ofstopnd_tb_(std::less<X86OfstOperand>(), mallocator->Adapter()),
      hash_stimmopnd_tb_(std::less<StImmOperand>(), mallocator->Adapter()),
      hash_memopnd_tb_(std::less<X86MemOperand>(), mallocator->Adapter()) {
    first_mapleir_v_reg_no = 100;
    v_reg_count = first_mapleir_v_reg_no + func->pregtab->Size();
    max_reg_count = 80 + v_reg_count;
    v_reg_table.resize(max_reg_count);
    // func->pregtab->pregTable[0] is nullptr, so skip it
    for (uint32 i = 1; i < func->pregtab->Size(); i++) {
      v_reg_table[i + first_mapleir_v_reg_no].regtype = GetRegTyFromPrimTy(func->pregtab->PregFromPregidx(i)->primType);
      v_reg_table[i + first_mapleir_v_reg_no].size = GetPrimTypeSize(func->pregtab->PregFromPregidx(i)->primType);
    }
    memlayout = mp->New<X86MemLayout>(&becommon, f, mallocator);
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

  MOperator PickMovInsn(PrimType primtype, bool ismovimm = false);
  MOperator PickMovInsn(RegOperand *, RegOperand *, bool ismovimm = false);
  void SelectDassign(DassignNode *stmt, Operand *opnd0) override;
  void SelectRegassign(RegassignNode *stmt, Operand *opnd0) override;
  void SelectAggDassign(DassignNode *stmt) override;
  void SelectIassign(IassignNode *stmt) override;
  void SelectAggIassign(IassignNode *stmt, Operand *lhsaddropnd) override;
  void SelectReturn(NaryStmtNode *stmt, Operand *opnd0) override;
  void SelectCondGoto(CondGotoNode *stmt, Operand *opnd0, Operand *opnd1) override;
  void SelectGoto(GotoNode *stmt) override;
  void SelectParmList(StmtNode *narynode);
  void SelectCall(CallNode *callnode) override;
  void SelectIcall(IcallNode *icallnode, Operand *fptropnd) override;
  void SelectIntrinCall(IntrinsiccallNode *icallnode) override {
    CG_ASSERT(0, "NYI");
  }

  void SelectComment(CommentNode *comment) override {}

  void Emit() override;
  // x86 select expr
  Operand *SelectDread(base_node_t *parent, AddrofNode *expr) override;
  RegOperand *SelectRegread(base_node_t *parent, RegreadNode *expr) override;
  Operand *SelectAddrof(AddrofNode *expr) override;
  Operand *SelectAddroffunc(AddroffuncNode *expr) override;
  Operand *SelectIread(base_node_t *parent, IreadNode *expr) override;

  Operand *SelectIntconst(MIRIntConst *expr) override;
  Operand *SelectVectorIntconst(MIRVectorIntConst *expr) override {
    CG_ASSERT(0, "NYI");
  }
  Operand *SelectFloatconst(MIRFloatConst *floatconst) override;
  Operand *SelectDoubleconst(MIRDoubleConst *doubleconst) override;
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
  void SelectCmp(Operand *resopnd, Operand *opnd0, Operand *opnd1, PrimType prmtype) override;
  Operand *SelectCmpOp(CompareNode *node, Operand *opnd0, Operand *opnd1) override;
  void SelectShift(Operand *resopnd, Operand *opnd0, Operand *opnd1, SHIFTDIRECTION direct, PrimType prmtype);
  Operand *SelectMpy(BinaryNode *node, Operand *opnd0, Operand *opnd1) override;
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
  RegOperand *GetTargetRetOperand(PrimType ptype) override;
  Operand *CreateOpndOfType(PrimType primtype);
  Operand *CreateOpndOfReg(X86RegOperand *opnd);
  Operand *CreateRflagOpnd();
  Operand *CreateImmOperand(PrimType primType, int64 val) override;
  Operand *CreateFloatImmOperand(float val);
  Operand *CreateDoubleImmOperand(double val);
  Operand *CreateZeroOperand(PrimType primType) override;
  X86RegOperand *GetOrCreatePhysicalRegisterOperand(regno_t reg_no, uint8 size, RegType kind);
  LabelOperand *GetOrCreateLabelOperand(LabelIdx labidx);
  ImmOperand *GetOrCreateImmOperand(int64 val, uint8 size, bool is_signed);
  X86OfstOperand *GetOrCreateOfstOpnd(uint32 offset, uint32 size);
  StImmOperand *GetOrCreateStImmOperand(MIRSymbol *st, int64 offset, int32 relocs);

  RegOperand *GetOrCreateFramePointerRegOperand() override {
    return GetOrCreatePhysicalRegisterOperand(RBP, 32, kRegTyInt);
  }

  MemOperand *GetOrCreateMemOpnd(MIRSymbol *symbol, int32 offset, int32 size) override;

  inline MemOperand *CreateMemOpnd(X86register reg, int32 offset, int32 size) {
    X86RegOperand *base_opnd = GetOrCreatePhysicalRegisterOperand(reg, 32, kRegTyInt);
    return CreateMemOpnd(base_opnd, offset, size);
  }

  MemOperand *CreateMemOpnd(RegOperand *base_opnd, int32 offset, int32 size) override;

  X86MemOperand *GetOrCreateMemOpnd(X86MemOperand::X86AdrMode, int32, RegOperand *, RegOperand *, Operand *, Operand *,
                                    MIRSymbol *);

  Insn *CreateMoveInsn(Operand *opnd);  // create a move instruction that copy opnd
  void EmitOperand(Operand *, OpndProp *);
  void GenerateProlog(BB *) override;
  void GenerateEpilog(BB *) override;
  void InsertCallAtFuncEntry(MIRSymbol *) {}

  // CFI directives related stuffs
  Operand *CreateCfiRegOperand(uint32 reg, uint8 size) override {
    /*
       System V Application Binary Interface
       AMD64 Architecture Processor Supplement. Draft Version 0.99.7
       https://www.uclibc.org/docs/psABI-x86_64.pdf $ 3.7 Figure 3.36
       (RBP->6, RSP->7)

       System V Application Binary Interface
       Inte386 Architecture Processor Supplement. Version 1.0
       https://www.uclibc.org/docs/psABI-i386.pdf $ 2.5 Table 2.14
       (EBP->5, ESP->4)
     */
    CG_ASSERT((reg == RSP || reg == RBP), "NYI");
    uint32 cfi_reg = reg == RSP ? 7 : 6;
    return memPool->New<cfi::RegOperand>(cfi_reg, size);
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

#endif  //  MAPLEBE_INCLUDE_CG_X86CGFUNC_H_
