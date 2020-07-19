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

#include "cg_func.h"
#include "cg.h"
#if TARGAARCH64
#include "aarch64/aarch64_cg.h"
#endif
#include "insn.h"
#include "reg_alloc.h"
#if TARGAARCH64
#include "aarch64_insn.h"
#endif
#include "mir_builder.h"
#include "name_mangler.h"
#include "cg_cfg.h"
#include "cg_assert.h"
#include <iostream>
#if DEBUG
#include <iomanip>
#endif

namespace maplebe {

using namespace maple;
using namespace std;

#define CLANG  (mirModule.IsCModule())
#define JAVALANG (mirModule.IsJavaModule())

const int kFreqBase = 10000;
CGFunc::CGFunc(MIRModule *mod, CG *c, MIRFunction *f, BECommon *bec, MemPool *mp, MapleAllocator *mallocator)
  : cg(c),
    mirModule(*mod),
    func(f),
    ehfunc(nullptr),
    bbcnt(0),
    labelIdx(0),
    start_label(nullptr),
    end_label(nullptr),
    cleanup_label(nullptr),
    firstbb(nullptr),
    cleanupbb(nullptr),
    cleanupEntrybb(nullptr),
    lastbb(nullptr),
    curbb(nullptr),
    commonStartbb(nullptr),
    commonEndbb(nullptr),
    exitbbsvec(mallocator->Adapter()),
    lab2bbmap(std::less<LabelIdx>(), mallocator->Adapter()),
    becommon(*bec),
    memlayout(nullptr),
    memPool(mp),
    funcscope_allocator_(mallocator),
    emitstvec_(mallocator->Adapter()),
    v_reg_table(mallocator->Adapter()),
    vreg_operand_table(std::less<regno_t>(), mallocator->Adapter()),
    preg_spill_mem_operands(std::less<PregIdx>(), mallocator->Adapter()),
    spillreg_mem_operands(std::less<regno_t>(), mallocator->Adapter()),
    spillreg_mem_operands_adj(mallocator->Adapter()),
    reusespillloc_mem(std::less<uint32>(), mallocator->Adapter()),
    offset_from_cfa_(0),
#if DEBUG
    pregs_to_vars_map(nullptr),
#endif
    call_info_map(std::less<Insn *>(), mallocator->Adapter()),
    aggParamReg(nullptr),
    total_insns(0),
    hasVLAOrAlloca(f->hasVlaoralloca),
    hasProEpilogue(false),
    hasNonescapedVar(false),
    isVolLoad(0),
    isVolStore(0),
    isAggParamInReg(false),
    isAfterRegAlloc(false),
    needSplit(false),
    hasTakenLabel(false),
    hasAlloca(false),
    frequency(0),
    rd(nullptr),
    sbb(nullptr) {
  mirModule.SetCurFunction(func);
  dummybb = CreateNewBB();
  first_mapleir_v_reg_no = 200;
  first_non_preg_vreg_no = v_reg_count = first_mapleir_v_reg_no + (func->pregTab == nullptr ? 0 : func->pregTab->Size());
  max_reg_count = v_reg_count + 1024;

  v_reg_table.resize(max_reg_count);
  if (func->pregTab != nullptr) {
    // func->pregTab->pregTable[0] is nullptr, so skip it
    CG_ASSERT(func->pregTab->PregFromPregIdx(0) == nullptr, "");
    for (uint32 i = 1; i < func->pregTab->Size(); i++) {
      PrimType primType = func->pregTab->PregFromPregIdx(i)->primType;
      uint8_t bytelen = GetPrimTypeSize(primType);
      if (bytelen < 4) {
        bytelen = 4;
      }
      new (&GetVirtualRegNodeFromPseudoRegIdx(i)) VirtualRegNode(GetRegTyFromPrimTy(primType), bytelen);
    }
  }
#if DEBUG
  pregs_to_vars_map = nullptr;
#endif
  if (func->labelTab == nullptr) {
    first_cggen_labelidx = 0;
  } else {
    first_cggen_labelidx = func->labelTab->GetLabelTableSize();

    for (size_t i = 1; i < func->symTab->GetSymbolTableSize(); i++) {
      MIRSymbol *sym = func->symTab->GetSymbolFromStIdx(i);
      if (sym && sym->GetType() && sym->GetType()->typeKind == kTypeClass) {
        hasNonescapedVar = true;
        break;
      }
    }
  }

  if (func->symTab) {
    lsymSize = func->symTab->GetSymbolTableSize();
  } else {
    lsymSize = 0;
  }
}

CGFunc::~CGFunc() {
  mirModule.SetCurFunction(nullptr);
}

LabelIdx CGFunc::CreateLabel() {
  std::string labstr;
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  std::string funcname = funcSt->GetName();
  labstr = funcname.append(to_string(labelIdx++));
  return func->GetOrCreateLablidxFromName(labstr);
}

void CGFunc::CreateStartEndLabel() {
  LabelIdx startLblidx = CreateLabel();
  MIRBuilder *mirbuilder = func->module->mirBuilder;
  start_label = mirbuilder->CreateStmtLabel(startLblidx);
  func->body->InsertFirst(start_label);
  LabelIdx endLblidx = CreateLabel();
  end_label = mirbuilder->CreateStmtLabel(endLblidx);
  func->body->InsertLast(end_label);
  CG_ASSERT(func->body->GetLast() == end_label, "");
}

void CGFunc::HandleLabel(LabelNode *stmt) {
  CG_ASSERT(stmt->op == OP_label, "error");
  LabelNode *lbnode = static_cast<LabelNode *>(stmt);
  BB *newbb = StartNewBBImpl<false>(stmt);
  newbb->AddLabel(lbnode->labelIdx);
  CG_ASSERT(newbb, "");
  lab2bbmap[newbb->labidx] = newbb;
  curbb = newbb;
}

void CGFunc::HandleGoto(GotoNode *stmt) {
  if (stmt->GetPrev()) {
    CG_ASSERT(stmt->GetPrev()->GetNext() == stmt, "");
  }
  GotoNode *gtnode = static_cast<GotoNode *>(stmt);
  CG_ASSERT(gtnode, "expect goto");
  curbb->SetKind(BB::kBBGoto);
  SelectGoto(gtnode);
  curbb = StartNewBB(gtnode);
  CG_ASSERT(stmt == gtnode, "");
  if (gtnode->GetNext() && gtnode->GetNext()->op != OP_label) {
    CG_ASSERT(curbb->prev->laststmt == stmt, "");
  }
}

void CGFunc::HandleIgoto(UnaryStmtNode *stmt) {
#if TARGARK
  return;
#else
  Operand *targetOpnd = HandleExpr(stmt, stmt->uOpnd);
  targetOpnd = SelectIgoto(targetOpnd);
  curbb->SetKind(BB::kBBIgoto);
#if TARGAARCH64
  curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_xbr, targetOpnd));
#endif
  curbb = StartNewBB(stmt);
#endif
}

void CGFunc::HandleCondbr(CondGotoNode *stmt) {
#if TARGARK
  return;
#else
  CondGotoNode *condgotonode = static_cast<CondGotoNode *>(stmt);
  CG_ASSERT(condgotonode, "expect cond br");
  BaseNode *condnode = condgotonode->Opnd(0);
  Opcode condOp = condgotonode->op;
  if (condnode->op == OP_constval) {
    ConstvalNode *constvalnode = static_cast<ConstvalNode *>(condnode);
    if ((constvalnode->constVal->IsZero() && OP_brfalse == condOp) ||
        (!constvalnode->constVal->IsZero() && OP_brtrue == condOp)) {
      GotoNode *gotostmt = memPool->New<GotoNode>(OP_goto);
      gotostmt->offset = condgotonode->offset;
      HandleGoto(gotostmt);
      LabelNode *labelstmt = memPool->New<LabelNode>();
      labelstmt->labelIdx = CreateLabel();
      HandleLabel(labelstmt);
    }
    return;
  }
  curbb->SetKind(BB::kBBIf);
  // if condnode is not a cmp node, cmp it with zero.
  if (!kOpcodeInfo.IsCompare(condnode->op)) {
    Operand *opnd0 = HandleExpr(stmt, condnode);
    PrimType primType = condnode->primType;
    Operand *zeroopnd = nullptr;
    if (IsPrimitiveInteger(primType)) {
      zeroopnd = CreateImmOperand(primType, 0);
    } else {
      CG_ASSERT((PTY_f32 == primType || PTY_f64 == primType), "we don't support half-precision FP operands yet");
#if TARGAARCH64
      zeroopnd = CreateFPImmZero(primType);
#else
      zeroopnd = CreateZeroOperand(primType);
#endif
    }
    SelectCondGoto(condgotonode, opnd0, zeroopnd);
    curbb = StartNewBB(stmt);
    return;
  }
  // Special case:
  // bgt (cmp (op0, op1), 0) ==>
  // bgt (op0, op1)
  // but skip the case cmp(op0, 0)
  BaseNode *op0 = condnode->Opnd(0);
  BaseNode *op1 = condnode->Opnd(1);
  if (op0->op == OP_cmp && op1->op == OP_constval) {
    ConstvalNode *constvalnode = static_cast<ConstvalNode *>(op1);
    MIRConst *mirconst = constvalnode->constVal;
    CompareNode *cmpnode = static_cast<CompareNode *>(op0);
    bool skip = false;
    if (cmpnode->Opnd(1)->op == OP_constval) {
      MIRConst *cst = static_cast<ConstvalNode *>(cmpnode->Opnd(1))->constVal;
      if (cst->IsZero()) {
        skip = true;
      }
    }
    if (!skip && mirconst->IsZero()) {
      SelectCondSpecial(condgotonode, op0);
      curbb = StartNewBB(stmt);
      return;
    }
  }
  // Special case:
  // brfalse(ge (cmpg (op0, op1), 0) ==>
  // fcmp op1, op2
  // blo
  if (stmt->op == OP_brfalse && condnode->op == OP_ge && op0->op == OP_cmpg && op1->op == OP_constval) {
    CompareNode *node = static_cast<CompareNode *>(op0);
    ConstvalNode *constvalnode = static_cast<ConstvalNode *>(op1);
    MIRConst *mirconst = constvalnode->constVal;
    if (mirconst->IsZero()) {
      Operand *opnd0 = HandleExpr(node, node->Opnd(0));
      Operand *opnd1 = HandleExpr(node, node->Opnd(1));
      PrimType operandType = node->opndType;
      opnd0 = opnd0->IsRegister() ? static_cast<RegOperand *>(opnd0) : SelectCopy(opnd0, operandType, operandType);
      Operand::OperandType opnd1ty = opnd1->op_kind_;
      if (opnd1ty != Operand::Opd_Immediate && opnd1ty != Operand::Opd_FPZeroImmediate) {
        opnd1 = opnd1->IsRegister() ? static_cast<RegOperand *>(opnd1) : SelectCopy(opnd1, operandType, operandType);
      }
      bool isfloat = IsPrimitiveFloat(operandType);
      CG_ASSERT(isfloat, "incorrect operand types");
      SelectFPCmpQuiet(opnd0, opnd1, GetPrimTypeBitSize(operandType));
      Operand *rflag = GetOrCreateRflag();
      LabelIdx labelIdx = stmt->offset;
      LabelOperand *targetopnd = GetOrCreateLabelOperand(labelIdx);
#if TARGAARCH64
      curbb->AppendInsn(cg->BuildInstruction<AArch64Insn>(MOP_blo, rflag, targetopnd));
#endif
      curbb = StartNewBB(stmt);
      return;
    }
  }
  Operand *opnd0 = HandleExpr(condnode, condnode->Opnd(0));
  Operand *opnd1 = HandleExpr(condnode, condnode->Opnd(1));
  SelectCondGoto(condgotonode, opnd0, opnd1);
  curbb = StartNewBB(stmt);
#endif
}

void CGFunc::HandleCall(CallNode *call) {
  CallNode *callnode = static_cast<CallNode *>(call);
  CG_ASSERT(callnode, "expect call");
  curbb->SetKind(BB::kBBCall);
  SelectCall(callnode);
  curbb = StartNewBB(callnode);
}

void CGFunc::HandleICall(IcallNode *icall) {
  IcallNode *icallnode = static_cast<IcallNode *>(icall);
  CG_ASSERT(icallnode, "expect icall");
  curbb->SetKind(BB::kBBCall);
  Operand *opnd0 = HandleExpr(icall, icallnode->nOpnd.at(0));
  SelectIcall(icallnode, opnd0);
  curbb = StartNewBB(icallnode);
}

void CGFunc::HandleIntrinCall(IntrinsiccallNode *call) {
  CG_ASSERT(call, "expect intrinsiccall");
  SelectIntrinCall(call);
}

void CGFunc::HandleIassign(StmtNode *iassign) {
  CG_ASSERT(iassign->op == OP_iassign, "expect iassign");
  IassignNode *iassignnode = static_cast<IassignNode *>(iassign);
  if (iassignnode->rhs->primType != PTY_agg) {
    SelectIassign(iassignnode);
  } else {
    BaseNode *addrnode = iassignnode->addrExpr;
    SelectAggIassign(iassignnode, HandleExpr(iassign, addrnode));
  }
}

void CGFunc::HandleAssertnull(UnaryStmtNode *cgAsserTnode) {
  SelectAssertnull(cgAsserTnode);
}

void CGFunc::HandleDassign(DassignNode *dassignnode) {
  CG_ASSERT(dassignnode->op == OP_dassign, "expect dassign");
  CG_ASSERT(dassignnode, "expect dassign");
  if ((dassignnode->GetRhs()->op == OP_malloc) || (dassignnode->GetRhs()->op == OP_alloca)) {
    Operand *opnd0 = HandleExpr(dassignnode, static_cast<UnaryStmtNode *>(dassignnode)->uOpnd);
    SelectDassign(dassignnode, opnd0);
  } else
  if (dassignnode->GetRhs()->primType != PTY_agg) {
    bool isSaveRetvalToLocal = false;
    BaseNode *rhs = dassignnode->GetRhs();
    if (rhs->op == OP_regread) {
      isSaveRetvalToLocal = (static_cast<RegreadNode *>(rhs)->regIdx == -kSregRetval0);
    }
    Operand *opnd0 = HandleExpr(dassignnode, dassignnode->GetRhs());
    SelectDassign(dassignnode, opnd0);
    if (isSaveRetvalToLocal) {
      curbb->lastinsn->MarkAsSaveRetValToLocal();
    }
  } else {
    SelectAggDassign(dassignnode);
  }
}

void CGFunc::HandleRegassign(StmtNode *regassign) {
  CG_ASSERT(regassign->op == OP_regassign, "expect regassign");
  RegassignNode *regassignnode = static_cast<RegassignNode *>(regassign);
  bool isSaveRetvalToLocal = false;
  BaseNode *operand = regassignnode->uOpnd;
  if (operand->op == OP_regread) {
    isSaveRetvalToLocal = (static_cast<RegreadNode *>(operand)->regIdx == -kSregRetval0);
  }
  Operand *opnd0 = HandleExpr(regassignnode, operand);
  SelectRegassign(regassignnode, opnd0);
  if (isSaveRetvalToLocal) {
    curbb->lastinsn->MarkAsSaveRetValToLocal();
  }
}

MIRSymbol *CGFunc::GetRetRefSymbol(BaseNode *expr) {
  Opcode opr = expr->op;
  if (opr != OP_dread) {
    return nullptr;
  }
  AddrofNode *retExpr = static_cast<AddrofNode *>(expr);
  MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(retExpr->stIdx);
  if (symbol->IsRefType()) {
    MIRSymbol *sym = nullptr;
    for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
      sym = func->formalDefVec[i].formalSym;
      if (sym == symbol) {
        return nullptr;
      }
    }
    return symbol;
  }
  return nullptr;
}

void CGFunc::HandleReturn(NaryStmtNode *retnode) {
  HandleRetCleanup(retnode);
  CG_ASSERT(retnode, "expect return node");
  CG_ASSERT(retnode->NumOpnds() <= 1, "NYI return nodes number > 1");
  Operand *opnd = nullptr;
  if (retnode->NumOpnds() != 0) {
    opnd = HandleExpr(retnode, retnode->Opnd(0));
  }
#if !TARGARK
  if (needSplit) {
    // Return might contain op that convers to a call
    BB *oldBB = curbb;
    curbb = StartNewBB(retnode);
    SplitCallBB(oldBB);
    needSplit = false;
  }
#endif
  SelectReturn(retnode, opnd);
  curbb->SetKind(BB::kBBReturn);
  curbb = StartNewBB(retnode);
}

void CGFunc::HandleRangegoto(RangegotoNode *rangegotonode) {
  curbb->SetKind(BB::kBBRangegoto);
  SelectRangegoto(rangegotonode, HandleExpr(rangegotonode, rangegotonode->Opnd(0)));
  curbb = StartNewBB(rangegotonode);
}

Operand *CGFunc::HandleDread(BaseNode *parent, AddrofNode *dreadnode) {
  CG_ASSERT(dreadnode, "expect dread");
  return SelectDread(parent, dreadnode);
}

Operand *CGFunc::HandleRegread(BaseNode *parent, RegreadNode *regreadnode) {
  if ((regreadnode->regIdx == -kSregRetval0) || (regreadnode->regIdx == -kSregRetval1)) {
    return GetTargetRetOperand(regreadnode->primType, -(regreadnode->regIdx));
  }
  return SelectRegread(parent, regreadnode);
}

Operand *CGFunc::HandleAddrof(BaseNode *parent, AddrofNode *addrofnode) {
  CG_ASSERT(addrofnode, "expect AddrofNode");
  return SelectAddrof(addrofnode);
}

Operand *CGFunc::HandleAddroffunc(BaseNode *parent, AddroffuncNode *expr) {
  CG_ASSERT(expr, "expect AddroffuncNode");
  return SelectAddroffunc(expr);
}

Operand *CGFunc::HandleAddroflabel(BaseNode *parent, AddroflabelNode *expr) {
  CG_ASSERT(expr, "expect AddroflabelNode");
  return SelectAddroflabel(expr);
}

Operand *CGFunc::HandleIread(BaseNode *parent, IreadNode *ireadnode) {
  CG_ASSERT(ireadnode, "expect iread");
  return SelectIread(parent, ireadnode);
}

Operand *CGFunc::HandleConstval(BaseNode *expr) {
  ConstvalNode *constvalnode = static_cast<ConstvalNode *>(expr);
  MIRConst *mirconst = constvalnode->constVal;
  if (MIRIntConst *mirintconst = dynamic_cast<MIRIntConst *>(mirconst)) {
    return SelectIntconst(mirintconst);
  } else if (MIRFloatConst *mirfloatconst = dynamic_cast<MIRFloatConst *>(mirconst)) {
    return SelectFloatconst(mirfloatconst);
  } else if (MIRDoubleConst *mirdoubleconst = dynamic_cast<MIRDoubleConst *>(mirconst)) {
    return SelectDoubleconst(mirdoubleconst);
  } else if (MIRVectorIntConst *mirVecconst = dynamic_cast<MIRVectorIntConst *>(mirconst)){
    return SelectVectorIntconst(mirVecconst);
  } else{
    CG_ASSERT(false, "NYI");
  }
  return nullptr;
}

Operand *CGFunc::HandleConststr(BaseNode *expr) {
  ConststrNode *conststrnode = static_cast<ConststrNode *>(expr);
  return SelectStrconst(memPool->New<MIRStrConst>(conststrnode->strIdx, GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_a64)));
}

Operand *CGFunc::HandleConststr16(BaseNode *expr) {
  Conststr16Node *conststr16node = static_cast<Conststr16Node *>(expr);
  return SelectStr16const(
    memPool->New<MIRStr16Const>(conststr16node->strIdx, GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_a64)));
}

Operand *CGFunc::HandleExpr(BaseNode *parent, BaseNode *expr) {
  Opcode opr = expr->op;
  Operand *result = nullptr;
  switch (opr) {
    case OP_dread:
      result = HandleDread(parent, static_cast<AddrofNode *>(expr));
      break;
    case OP_regread:
      result = HandleRegread(parent, static_cast<RegreadNode *>(expr));
      break;
    case OP_constval:
      result = HandleConstval(static_cast<BaseNode *>(expr));
      break;
    case OP_conststr:
      result = HandleConststr(static_cast<BaseNode *>(expr));
      break;
    case OP_conststr16:
      result = HandleConststr16(static_cast<BaseNode *>(expr));
      break;
    case OP_add:
      result =
        SelectAdd(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_CG_array_elem_add:
      result = SelectCGArrayElemAdd(static_cast<BinaryNode *>(expr));
      break;
    case OP_ashr:
    case OP_lshr:
    case OP_shl:
      result =
        SelectShift(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_mul:
      result =
        SelectMpy(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_div:
      result =
        SelectDiv(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_rem:
      result =
        SelectRem(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_addrof:
      result = HandleAddrof(parent, static_cast<AddrofNode *>(expr));
      break;
    case OP_addroffunc:
      result = HandleAddroffunc(parent, static_cast<AddroffuncNode *>(expr));
      break;
    case OP_addroflabel:
      result = HandleAddroflabel(parent, static_cast<AddroflabelNode *>(expr));
      break;
    case OP_iread:
      result = HandleIread(parent, static_cast<IreadNode *>(expr));
      break;
    case OP_sub:
      result =
        SelectSub(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_band:
      result =
        SelectBand(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_bior:
      result =
        SelectBior(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_bxor:
      result =
        SelectBxor(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_abs:
      result = SelectAbs(static_cast<UnaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_bnot:
      result = SelectBnot(static_cast<UnaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_sext:
    case OP_zext:
    case OP_extractbits:
      result = SelectExtractbits(static_cast<ExtractbitsNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_depositbits:
      result = SelectDepositbits(static_cast<DepositbitsNode *>(expr), HandleExpr(expr, expr->Opnd(0)),
                                 HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_lnot:
      result = SelectLnot(static_cast<UnaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_land:
      result =
        SelectLand(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_lior:
      if (parent->IsCondBr())
        result = SelectLor(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)),
                           HandleExpr(expr, expr->Opnd(1)), true);
      else
        result =
          SelectLor(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_min:
      result =
        SelectMin(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_max:
      result =
        SelectMax(static_cast<BinaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_neg:
      result = SelectNeg(static_cast<UnaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_recip:
      result = SelectRecip(static_cast<UnaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_sqrt:
      result = SelectSqrt(static_cast<UnaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_ceil:
      result = SelectCeil(static_cast<TypeCvtNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_floor:
      result = SelectFloor(static_cast<TypeCvtNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      if (CLANG) {
        needSplit = true;
      }
      break;
    case OP_retype:
      result = SelectRetype(static_cast<TypeCvtNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_cvt:
      result = SelectCvt(parent, static_cast<TypeCvtNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_round:
      result = SelectRound(static_cast<TypeCvtNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_trunc:
      result = SelectTrunc(static_cast<TypeCvtNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_select:
      result = SelectSelect(static_cast<TernaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)),
                            HandleExpr(expr, expr->Opnd(1)), HandleExpr(expr, expr->Opnd(2)));
      break;
    case OP_le:
    case OP_ge:
    case OP_gt:
    case OP_lt:
    case OP_ne:
    case OP_eq:
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg:
      result =
        SelectCmpOp(static_cast<CompareNode *>(expr), HandleExpr(expr, expr->Opnd(0)), HandleExpr(expr, expr->Opnd(1)));
      break;
    case OP_alloca:
      result = SelectAlloca(static_cast<UnaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      break;
    case OP_malloc:
      result = SelectMalloc(static_cast<UnaryNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      needSplit = true;
      break;
    case OP_gcmalloc:
    case OP_gcpermalloc:
      result = SelectGCMalloc(static_cast<GCMallocNode *>(expr));
      needSplit = true;
      break;
    case OP_gcmallocjarray:
    case OP_gcpermallocjarray:
      result = SelectJarrayMalloc(static_cast<JarrayMallocNode *>(expr), HandleExpr(expr, expr->Opnd(0)));
      needSplit = true;
      break;
    default:
      CG_ASSERT(false, "NYI");
  }
  return result;
}

void CGFunc::MoveRegargs() {
  MoveRegargs(firstbb);
  return;
}

void CGFunc::MoveVRegargs() {
  MoveVRegargs(firstbb);
  return;
}

void CGFunc::IsolateFastPath() {
  IsolateFastPath(firstbb);
}

void CGFunc::GeneratePrologEpilog() {
  CG_ASSERT(func->body->GetFirst()->op == OP_label, "The first statement should be a label");
  if (cg->DoPrologueEpilogue() && !cg->InstrumentWithProfile()) {
    hasProEpilogue = !TailCallOpt();
  } else {
    hasProEpilogue = true;
  }
  if (hasAlloca) {
    hasProEpilogue = true;
  }

  if (hasProEpilogue) {
    Genstackguard(firstbb);
  }

  BB *prolog = nullptr;
  if (g->optim_level == 2) {
    /* There are some O2 dependent assumptions made */
    prolog = IsolateFastPath(firstbb);
  }

  uint32 exitbbsize = exitbbsvec.size();
  if (exitbbsize == 0) {
    exitbbsize = 1;
    if (lastbb->prev->firststmt == cleanup_label && lastbb->prev->prev) {
      exitbbsvec.push_back(lastbb->prev->prev);
    } else {
      exitbbsvec.push_back(lastbb->prev);
    }
  }

  if (prolog) {
    GenerateProlog(prolog);
    prolog->fastpath = true;
    firstbb->fastpath = true;
  } else {
    GenerateProlog(firstbb);
  }

  for (uint32 i = 0; i < exitbbsize; i++) {
    GenerateEpilog(exitbbsvec[i]);
  }

  if (func->IsJava()) {
    GenerateEpilogForCleanup(cleanupbb);
  }
}

#if !TARGARK
void CGFunc::GenerateCfiForLSDA(BB *bb, Insn *ipoint) {
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  const char *funcname = funcSt->GetName().c_str();
  (void)bb->InsertInsnAfter(ipoint,
                            cg->BuildInstruction<cfi::CfiInsn>(
                              cfi::OP_CFI_lsda_label, CreateCfiImmOperand(0x1b, 8),
                              CreateCfiLabelOperand(funcname, ehfunc->lsda_header->lsda_label->labelIdx)));
}

void CGFunc::GenerateCfiPrologEpilog() {
  Insn *ipoint = cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_startproc);
  // prolog
  if (firstbb->firstinsn) {
    firstbb->InsertInsnBefore(firstbb->firstinsn, ipoint);
  } else {
    firstbb->AppendInsn(ipoint);
  }

  // always generate ".cfi_personality 155, DW.ref.__mpl_personality_v0" for Java methods.
  // we depend on this to tell whether it is a java method.
  if (func->IsJava()) {
    Insn *personality =
      cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_personality_symbol, CreateCfiImmOperand(EHFunc::kTypeEncoding, 8),
                                         CreateCfiStrOperand("DW.ref.__mpl_personality_v0"));
    ipoint = firstbb->InsertInsnAfter(ipoint, personality);
  } else if (mirModule.srcLang == kSrcLangCPlusPlus && ehfunc && ehfunc->NeedFullLSDA()) {
    Insn *personality =
      cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_personality_symbol, CreateCfiImmOperand(EHFunc::kTypeEncoding, 8),
                                         CreateCfiStrOperand("DW.ref.__gxx_personality_v0"));
    ipoint = firstbb->InsertInsnAfter(ipoint, personality);
    GenerateCfiForLSDA(firstbb, ipoint);
  }
  // epilog
  if (cg->GenerateCfiDirectives()) {
    lastbb->AppendInsn(cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_endproc));
  }
}

Insn *CGFunc::InsertCFIDefCfaOffset(int &cfiOffset /*in-out*/, Insn *insertAfter) {
  cfiOffset = AddtoOffsetFromCFA(cfiOffset);
  Insn *cfiInsn = cg->BuildInstruction<cfi::CfiInsn>(cfi::OP_CFI_def_cfa_offset, CreateCfiImmOperand(cfiOffset, 64));
  Insn *newIpoint = curbb->InsertInsnAfter(insertAfter, cfiInsn);
  CG_ASSERT(0, "InsertCFIDefCfaOffset() should be called only once?");
  return newIpoint;
}
#endif

void CGFunc::TraverseAndClearCatchMark(BB *bb) {
  // has bb been visited
  if (bb->internal_flag3) {
    return;
  }
  bb->is_catch = false;
  bb->internal_flag3 = 1;
  for (auto succBb : bb->succs) {
    TraverseAndClearCatchMark(succBb);
  }
}

/* Two types of successor edges, normal and eh.  Any bb which is not
 * reachable by a normal successor edge is considered to be in a
 * catch block.
 * Marking it as a catch block does not automatically make it into
 * a catch block.  Unreachables can be marked as such too.
 */
void CGFunc::MarkCatchBBs(void) {
  // First, suspect all bb to be in catch
  FOR_ALL_BB(bb, this) {
    bb->is_catch = true;
    bb->internal_flag3 = 0;  // mark as not visited
  }
  // Eliminate cleanup section from catch
  FOR_ALL_BB(bb, this) {
    if (bb->firststmt == cleanup_label) {
      bb->is_catch = false;
      CG_ASSERT(bb->succs.size() <= 1, "MarkCatchBBs incorrect cleanup label");
      BB *succ = nullptr;
      if (bb->succs.size()) {
        succ = bb->succs.front();
      } else {
        continue;
      }
      while (1) {
        CG_ASSERT(succ->succs.size() <= 1, "MarkCatchBBs incorrect cleanup label");
        succ->is_catch = false;
        if (succ->succs.size()) {
          succ = succ->succs.front();
        } else {
          break;
        }
      }
    }
  }
  // Unmark all normally reachable bb as NOT catch.
  TraverseAndClearCatchMark(firstbb);
}

/* Mark CleanupEntryBB
 * Note: Cleanup bbs and func body bbs are seperated, no edges between them.
 * No eh_succs or eh_prevs between cleanup bbs.
 */
void CGFunc::MarkCleanupEntryBB() {
  BB *cleanupBb = nullptr;
  FOR_ALL_BB(bb, this) {
    bb->is_cleanup = 0;      // Use to mark cleanup bb
    bb->internal_flag3 = 0;  // Use to mark if visited.
    if (bb->firststmt == this->cleanup_label) {
      cleanupBb = bb;
    }
  }
  // If a function without cleanup bb, return.
  if (cleanupBb == nullptr) {
    return;
  }
  MarkCleanup(cleanupBb);
  CG_ASSERT(cleanupBb->eh_succs.empty(), "CG internal error. Cleanup bb should not have eh_succs.");
#if DEBUG  // Please don't remove me.
  // Check if all of the cleanup bb is at bottom of the function.
  bool isCleanupArea = true;
  FOR_ALL_BB_REV(bb, this) {
    if (isCleanupArea) {
      CG_ASSERT(bb->is_cleanup, "CG internal error, cleanup BBs should be at the bottom of the function.");
    } else {
      CG_ASSERT(!bb->is_cleanup, "CG internal error, cleanup BBs should be at the bottom of the function.");
    }

    if (bb == cleanupBb) {
      isCleanupArea = false;
    }
  }
#endif  // DEBUG
  this->cleanupEntrybb = cleanupBb;
}

// Tranverse from current bb's successor and set is_cleanup true.
void CGFunc::MarkCleanup(BB *bb) {
  // If bb hasn't been visited, return.
  if (bb->internal_flag3) {
    return;
  }
  bb->internal_flag3 = 1;
  bb->is_cleanup = 1;
  for (auto tmpBB : bb->succs) {
    MarkCleanup(tmpBB);
  }
  CG_ASSERT(bb->eh_succs.empty(), "CG internal error. Cleanup bb should not have eh_succs.");
}

/*
 * Traverse all call insn to determine return type of it
 * If the following insn is mov/str/blr and use R0/V0, it means the call insn have reture value
 */
void CGFunc::DetermineReturnTypeofCall() {
  for (BB *bb = firstbb; bb; bb = bb->next) {
    if (bb->kind != BB::kBBCall || bb->unreachable) {
      continue;
    }
    Insn *insn = bb->lastinsn;
    CG_ASSERT(insn, "impossible");
    while (!insn->IsCall()) {
      insn = insn->GetPreviousMachineInsn();
      CG_ASSERT(insn, "impossible");
    }
    BB *nextBb = bb;
    Insn *nextInsn = nullptr;
    do {
      CHECK_FATAL(nextBb->succs.size() == 1 && nextBb->succs.front() == nextBb->next, "must be");
      nextInsn = nextBb->succs.front()->firstinsn;

      while (nextInsn == nullptr) {
        nextBb = nextBb->succs.front();
        if (nextBb->succs.size() == 0) {
          nextInsn = nullptr;
          break;
        }
        CHECK_FATAL(nextBb->succs.size() == 1 && nextBb->succs.front() == nextBb->next, "must be");
        nextInsn = nextBb->succs.front()->firstinsn;
      }

      if (nextInsn == nullptr) {
        break;
      }
      if (!nextInsn->IsMachineInstruction()) {
        nextInsn = nextInsn->GetNextMachineInsn();
      }
      CHECK_FATAL(nextBb->succs.size() == 1 && nextBb->succs.front() == nextBb->next, "must be");
      nextBb = nextBb->succs.front();
    } while (nextInsn == nullptr);

    if (nextInsn == nullptr) {
      continue;
    }
#if TARGAARCH64
    if ((nextInsn->IsMove() && nextInsn->opnds[1]->IsRegister()) || nextInsn->IsStore() ||
        (nextInsn->IsCall() && nextInsn->opnds[0]->IsRegister())) {
      RegOperand *srcOpnd = static_cast<RegOperand *>(nextInsn->GetOpnd(0));
      if (srcOpnd->IsPhysicalRegister()) {
        if (srcOpnd->GetRegisterNumber() == R0) {
          insn->ret_type = Insn::kRegInt;
        } else if (srcOpnd->GetRegisterNumber() == V0) {
          insn->ret_type = Insn::kRegFloat;
        }
      }
    }
#endif
  }
}

#if !TARGARK
void CGFunc::HandleFunction(void) {
  BlockNode *block = func->body;

  CG_ASSERT(block, "");
  bool withFreqInfo = (func->freqMap.size() != 0);
  if (withFreqInfo) {
    frequency = kFreqBase;
  }
  StmtNode *stmt = block->GetFirst();
  CG_ASSERT(stmt->op == OP_label, "The first statement should be a label");
  HandleLabel(static_cast<LabelNode *>(stmt));
  firstbb = curbb;
  stmt = stmt->GetNext();
  curbb = StartNewBBImpl<false>(stmt);
  curbb->frequency = frequency;
  if (JAVALANG) {
    HandleRCCall(true);
  }

  // First Pass:
  // Creates the doubly-linked list of BBs (next,prev)
  Opcode opcode;
  uint32 lastsrcloc = 0;
  uint32 lastmplloc = 0;
  bool isJavaCatchCall = false;
  Insn *tempinsn = nullptr;

  cout << "===============================================\n";
  for (; stmt; stmt = stmt->GetNext()) {
    needSplit = false;
    stmt->Dump(func->module,0);
    isVolLoad = false;
    opcode = stmt->op;
    StmtNode *next = stmt->GetRealNext();
    if (next) {
      if ((opcode == OP_membaracquire || opcode == OP_membarrelease) && next->op == stmt->op) {
        continue;
      }
      if (opcode == OP_membarstorestore && next->op == OP_membarrelease) {
        continue;
      }
      if (!CGOptions::useBarriersForVolatile && next->op == OP_membaracquire) {
        isVolLoad = true;
      }
    }
    bool tempload = isVolLoad;
    switch (opcode) {
      case OP_label:
        HandleLabel(static_cast<LabelNode *>(stmt));
        break;
      case OP_goto:
        // build eh func insert goto/condgoto stmts
        if (withFreqInfo && (func->freqMap.find(stmt->stmtID) != func->freqMap.end())) {
          frequency = func->freqMap[stmt->stmtID];
        }
        HandleGoto(static_cast<GotoNode *>(stmt));
        break;
      case OP_brfalse:
      case OP_brtrue:
        if (withFreqInfo && (func->freqMap.find(stmt->stmtID) != func->freqMap.end())) {
          frequency = func->freqMap[stmt->stmtID];
        }
        HandleCondbr(static_cast<CondGotoNode *>(stmt));
        break;
      case OP_return:
        if (withFreqInfo) {
          CHECK_FATAL(func->freqMap.find(stmt->stmtID) != func->freqMap.end(), "last stmt of BB");
          frequency = func->freqMap[stmt->stmtID];
        }
        HandleReturn(static_cast<NaryStmtNode *>(stmt));
        break;
      case OP_call:
        if (withFreqInfo && func->freqMap.find(stmt->stmtID) != func->freqMap.end()) {
          frequency = func->freqMap[stmt->stmtID];
        }
        HandleCall(static_cast<CallNode *>(stmt));
        if (isJavaCatchCall) {
          if (stmt->GetNext() && stmt->GetNext()->op == OP_label) {
            curbb = StartNewBBImpl<true>(stmt);
          }
          HandleJavaCatch();
          isJavaCatchCall = false;
        }
        break;
      case OP_icall:
        if (withFreqInfo && func->freqMap.find(stmt->stmtID) != func->freqMap.end()) {
          frequency = func->freqMap[stmt->stmtID];
        }
        HandleICall(static_cast<IcallNode *>(stmt));
        break;
      case OP_intrinsiccall:
      case OP_intrinsiccallassigned:
      case OP_intrinsiccallwithtype:
      case OP_intrinsiccallwithtypeassigned:
        HandleIntrinCall(static_cast<IntrinsiccallNode *>(stmt));
        break;
      case OP_dassign:
        HandleDassign(static_cast<DassignNode *>(stmt));
        break;
      case OP_regassign:
        HandleRegassign(static_cast<RegassignNode *>(stmt));
        break;
      case OP_iassign:
        HandleIassign(static_cast<IassignNode *>(stmt));
        break;
      case OP_eval:
        HandleExpr(stmt, static_cast<UnaryStmtNode *>(stmt)->uOpnd);
        break;
      case OP_rangegoto:
        if (withFreqInfo && func->freqMap.find(stmt->stmtID) != func->freqMap.end()) {
          frequency = func->freqMap[stmt->stmtID];
        }
        HandleRangegoto(static_cast<RangegotoNode *>(stmt));
        break;
      case OP_membarrelease: {
        if (!CGOptions::useBarriersForVolatile && next && (next->op == OP_iassign || stmt->GetNext()->op == OP_dassign) &&
            next->GetRealNext() && next->GetRealNext()->op == OP_membarstoreload) {
          isVolStore = true;
          SelectMembar(stmt);
          tempinsn = curbb->lastinsn;
          break;
        }
      }
      case OP_membaracquire:
      case OP_membarstoreload:
      case OP_membarstorestore:
        SelectMembar(stmt);
        break;
      case OP_comment:
        if (cg->GenerateVerboseAsm()) {
          SelectComment(static_cast<CommentNode *>(stmt));
        }
        break;
      case OP_catch:
      case OP_javacatch:
        CG_ASSERT(stmt->GetNext()->op == OP_call, "The next statement of OP_javacatch should be OP_call.");
        isJavaCatchCall = true;
        break;
      case OP_cppcatch:
        isJavaCatchCall = true;
        break;
      case OP_try:
      case OP_javatry:
      case OP_cpptry:
      case OP_endtry:
        break;
      case OP_syncenter:
      case OP_syncexit:
        CG_ASSERT(0, "should have been lowered to a call or inlined");
        break;
      case OP_customcallassigned:
        DCHECK(false, "Not supported: known issue");
        break;
      case OP_assertnonnull:
        HandleAssertnull(static_cast<UnaryStmtNode *>(stmt));
        break;
      case OP_igoto:
        HandleIgoto(static_cast<UnaryStmtNode *>(stmt));
        break;
      default:
        CG_ASSERT(false, "NYI");
        break;
    }
    // skip the membar acquire if it is just after the iread.
    if (tempload && !isVolLoad) {
      stmt = stmt->GetNext();
    }
    // skip the membarstoreload if there is the pattern for volatile write( membarrelease + store + membarstoreload )
    if (tempinsn) {
      if (stmt->op != OP_membarrelease && stmt->op != OP_comment) {
        if (!isVolStore) {
          // remove the generated membar release insn.
          curbb->RemoveInsn(tempinsn);
          // skip the membarstoreload.
          stmt = stmt->GetNext();
        }
        tempinsn = nullptr;
        isVolStore = false;
      }
    }
    if (needSplit) {
      BB *oldBB = curbb;
      curbb = StartNewBB(stmt);
      SplitCallBB(oldBB);
    }
  }
  // Set lastbb's frequency
  curbb->laststmt = block->GetLast();
  curbb->frequency = frequency;
  lastbb = curbb;
  // Create an empty bb as the return bb before possible cleanup
  if (lastbb->prev->firstinsn) {
    BB *newbb = CreateNewBB();
    newbb->frequency = lastbb->frequency;
    lastbb->PrependBB(newbb);
    newbb->SetKind(BB::kBBFallthru);
  }
  // bb for C so it can be placeholder for cleanup which is not needed
  if (func->module->IsCModule()) {
    BB *newbb = CreateNewBB();
    lastbb->PrependBB(newbb);
    newbb->SetKind(BB::kBBFallthru);
    LabelIdx labidx = CreateLabel();
    newbb->AddLabel(labidx);
    lab2bbmap[labidx] = newbb;
   }

  cleanupbb = lastbb->prev;
  // All stmts are handled
  frequency = 0;

  // merge multi return
  MergeReturn();
  CG_ASSERT(exitbbsvec.size() <= 1, "there are more than one BB_return in func");
  if (exitbbsvec.size() == 0) {
    BB *retbb = CreateNewBB();
    retbb->frequency = cleanupbb->frequency;
    cleanupbb->PrependBB(retbb);
    retbb->SetKind(BB::kBBReturn);
    LabelIdx labidx = CreateLabel();
    retbb->AddLabel(labidx);
    lab2bbmap[labidx] = retbb;
    exitbbsvec.push_back(retbb);
  }
  BB *bb = exitbbsvec[0];
  if (bb->NumInsn() > 0) {
    BB *retbbPart = CreateNewBB();
    LabelIdx retbbPartLabidx = bb->labidx;
    if (retbbPartLabidx != MIRLabelTable::kDummyLabel) {
      retbbPart->AddLabel(retbbPartLabidx);
      lab2bbmap[retbbPartLabidx] = retbbPart;
    }
    Insn *insn = bb->firstinsn;
    CG_ASSERT(bb->lastinsn != nullptr, "bb->lastinsn is null in CGFunc::HandleFunction");
    while (insn != bb->lastinsn->next) {
      retbbPart->AppendInsn(insn);
      bb->RemoveInsn(insn);
      insn = bb->firstinsn;
    }
    retbbPart->SetKind(BB::kBBFallthru);
    bb->PrependBB(retbbPart);
    retbbPart->frequency = bb->frequency;
    LabelIdx labidx = CreateLabel();
    bb->AddLabel(labidx);
    lab2bbmap[labidx] = bb;
  }

  if (func->IsJava()) {
    GenerateCleanupCodeForExtEpilog(cleanupbb);
  } else if (func->module->IsCModule()) {
  } else {
    GenerateCleanupCode(cleanupbb);
  }
  GenSavemethodinfoCode(firstbb);
  theCFG = memPool->New<CGCFG>(this);
  theCFG->BuildCFG();
  // Because callinsn is the lastinsn of a BB in our CGFunc now. so give the frequency be the real frequency.
  if (withFreqInfo) {
    FOR_ALL_BB(bb, this) {
      if (bb->GetKind() == BB::kBBCall) {
        CG_ASSERT(bb->succs.size() <= 1, "call BB has only one succor.");
        if (bb->succs.size() > 0) {
          bb->frequency = (*(bb->succs.begin()))->frequency;
        }
      }
    }
  }
  if (mirModule.IsJavaModule()) {
    MarkCatchBBs();
    MarkCleanupEntryBB();
  }
  DetermineReturnTypeofCall();
  theCFG->MarkLabelTakenBB();
  theCFG->UnreachCodeAnalysis();
}

void CGFunc::DumpCFG(void) {
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  LogInfo::MapleLogger() << "\n****** CFG built by CG for " << funcSt->GetName() << " *******\n";
  FOR_ALL_BB(bb, this) {
    LogInfo::MapleLogger() << "=== BB ( " << hex << bb << dec << " ) <" << bb->GetKindName() << "> ===\n";
    LogInfo::MapleLogger() << "BB id:" << bb->id << "\n";
    if (bb->preds.size() > 0) {
      LogInfo::MapleLogger() << " pred [ ";
      MapleList<BB *>::iterator it = bb->preds.begin();
      while (it != bb->preds.end()) {
        LogInfo::MapleLogger() << hex << (*it) << dec << " ";
        it++;
      }
      LogInfo::MapleLogger() << "]\n";
    }
    if (bb->succs.size() > 0) {
      LogInfo::MapleLogger() << " succ [ ";
      MapleList<BB *>::iterator it = bb->succs.begin();
      while (it != bb->succs.end()) {
        LogInfo::MapleLogger() << hex << (*it) << dec << " ";
        it++;
      }
      LogInfo::MapleLogger() << "]\n";
    }
    StmtNode *stmt = bb->firststmt;
    if (stmt) {
      bool done = false;
      do {
        done = stmt == bb->laststmt;
        stmt->Dump(&mirModule, 1);
        LogInfo::MapleLogger() << "\n";
        stmt = stmt->GetNext();
      } while (!done);
    } else {
      LogInfo::MapleLogger() << "<empty BB>\n";
    }
  }
}

void CGFunc::DumpCGIR(void) {
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  LogInfo::MapleLogger() << "\n******  CGIR for " << funcSt->GetName() << " *******\n";
  FOR_ALL_BB(bb, this) {
    bb->Dump();
  }
}

void CGFunc::CheckLoop(void) {
  for (auto lp : loops) {
    lp->CheckLoops();
  }
}

void CGFunc::DumpLoop(void) {
  for (auto lp : loops) {
    lp->PrintLoops();
  }
}

void CGFunc::ClearLoopInfo() {
  loops.clear();
  FOR_ALL_BB(bb, this) {
    bb->loop_preds.clear();
    bb->loop_succs.clear();
  }
}

CallerSavedRegHandler::CallerSavedRegHandler(CGFunc *f) : cgfunc(f) {
  PrimType ty = cgfunc->func->GetReturnType()->GetPrimType();
  if (ty == PTY_void) {
    return_reg_type = kRegTyUndef;
  } else if (IsPrimitiveFloat(ty)) {
    return_reg_type = kRegTyFloat;
  } else {
    CG_ASSERT(IsPrimitiveInteger(ty), "");
    return_reg_type = kRegTyInt;
  }
}
#endif

AnalysisResult *CgDoCreateLabel::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (cgfunc->func->labelTab == nullptr) {
    cgfunc->func->labelTab = cgfunc->mirModule.memPool->New<MIRLabelTable>(&cgfunc->mirModule.memPoolAllocator);
  }
  cgfunc->CreateStartEndLabel();

  return nullptr;
}

#if !TARGARK
AnalysisResult *CgDoHandleFunc::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  cgfunc->HandleFunction();
  if (!cgfunc->cg->cgopt_.DoEmitCode() || cgfunc->cg->cgopt_.DoDumpCFG()) {
    cgfunc->DumpCFG();
  }
  return nullptr;
}

AnalysisResult *CgMoveRegargs::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  cgfunc->MoveRegargs();
  return nullptr;
}

AnalysisResult *CgMoveVRegargs::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  cgfunc->MoveVRegargs();
  return nullptr;
}

AnalysisResult *CgDoMerge::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  MemPool *sbb_mp = mempoolctrler.NewMemPool("sbb");
  SuperBBBuilder *sbb = cgfunc->NewSuperBBBuilder(cgfunc, sbb_mp);
  CG_ASSERT(sbb, "");
  cgfunc->SetSBB(sbb);
  sbb->MergeProcess();
  return nullptr;
}

AnalysisResult *CgDoSplit::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (cgfunc->GetSBB()) {
    cgfunc->GetSBB()->SplitProcess();
    mempoolctrler.DeleteMemPool(cgfunc->GetSBB()->GetMemPool());
    cgfunc->SetSBB(nullptr);
  }
  return nullptr;
}

AnalysisResult *CgDoGenProEpiLog::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  cgfunc->GeneratePrologEpilog();
  return nullptr;
}

AnalysisResult *CgDoOffAdjFPLR::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  cgfunc->OffsetAdjustmentForFPLR();
  return nullptr;
}

AnalysisResult *CgFixCFLocOsft::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  return nullptr;
}

AnalysisResult *CgYieldpointInsertion::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  cgfunc->InsertYieldpoint();
  return nullptr;
}
#endif

}  // namespace maplebe
