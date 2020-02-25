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

#include "mmpllowerer.h"
#include <cmath>
#include <string>
#include <algorithm>
#include "intrinsic_op.h"
#include "mir_builder.h"

namespace maplebe {

using namespace maple;
using namespace std;

inline PrimType UnsignedPrimType(int32 bytesize) {
  if (bytesize == 4) {
    return PTY_u32;
  }
  if (bytesize == 2) {
    return PTY_u16;
  }
  if (bytesize == 1) {
    return PTY_u8;
  }
  return PTY_u32;
}

PregIdx MmplLowerer::GetSpecialRegFromSt(const MIRSymbol *sym) {
  MIRStorageClass storageClass = sym->storageClass;
  PregIdx specreg = 0;
  if (storageClass == kScAuto || storageClass == kScFormal) {
    CHECK(sym->GetStIndex() < memlayout->sym_alloc_table.size(),
          "index out of range in MmplLowerer::GetSpecialRegFromSt");
    SymbolAlloc symalloc = memlayout->sym_alloc_table[sym->GetStIndex()];
    if (symalloc.mem_segment->kind == MS_upformal || symalloc.mem_segment->kind == MS_formal ||
        symalloc.mem_segment->kind == MS_FPbased) {
      specreg = -kSregFp;
    } else if (symalloc.mem_segment->kind == MS_actual || symalloc.mem_segment->kind == MS_SPbased) {
      specreg = -kSregSp;
    } else {
      CHECK_FATAL(false, "MmplLowerer::LowerDread: bad memory layout for local variable");
    }
  } else if (storageClass == kScGlobal || storageClass == kScFstatic) {
    specreg = -kSregGp;
  } else {
    CHECK_FATAL(false, "MmplLowerer::LowerDread: NYI");
  }
  return specreg;
}

BaseNode *MmplLowerer::ReadregNodeForSymbol(MIRSymbol *sym) {
  return mirModule.mirBuilder->CreateExprRegread(PTY_a32, GetSpecialRegFromSt(sym));
}

BaseNode *MmplLowerer::LowerAddrof(AddrofNode *expr) {
  MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(expr->stIdx);
  if (symbol->storageClass == kScText) {
    return expr;
  }
  int32 offset = 0;
  if (expr->fieldID != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(symbol->GetType());
    CHECK_FATAL(structty, "MmplLowerer::LowerAddrof: non-zero fieldID for non-structure");
    offset = becommon.GetFieldOffset(structty, expr->fieldID).first;
  }
  BaseNode *rrn = ReadregNodeForSymbol(symbol);
  offset += symbol->IsLocal() ? memlayout->sym_alloc_table[symbol->GetStIndex()].offset
                              : globmemlayout->sym_alloc_table[symbol->GetStIndex()].offset;
  return (offset == 0) ? rrn
                       : mirModule.mirBuilder->CreateExprBinary(OP_add, globaltable.GetTypeFromTyIdx(PTY_a32), rrn,
                                                                 mirModule.mirBuilder->GetConstInt(offset));
}

BaseNode *MmplLowerer::LowerDread(AddrofNode *expr) {
  MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(expr->stIdx);
  PrimType symty = symbol->GetType()->primType;
  int32 offset = 0;
  if (expr->fieldID != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(symbol->GetType());
    CHECK_FATAL(structty, "MmplLowerer::LowerDread: non-zero fieldID for non-structure");
    FieldPair thepair = structty->TraverseToField(expr->fieldID);
    symty = globaltable.GetTypeFromTyIdx(thepair.second.first)->primType;
    offset = becommon.GetFieldOffset(structty, expr->fieldID).first;
  }
  // allow dread class reference
  PregIdx spcreg = GetSpecialRegFromSt(symbol);
  if (spcreg == -kSregFp) {
    CHECK_FATAL(symbol->IsLocal(), "load from fp non local?");
    IreadFPoffNode *ireadoff = mirModule.mirBuilder->CreateExprIreadFPoff(
      symty, memlayout->sym_alloc_table[symbol->GetStIndex()].offset + offset);
    return ireadoff;
  } else {
    BaseNode *rrn = mirModule.mirBuilder->CreateExprRegread(PTY_a32, spcreg);
    SymbolAlloc &symalloc = symbol->IsLocal() ? memlayout->sym_alloc_table[symbol->GetStIndex()]
                                              : globmemlayout->sym_alloc_table[symbol->GetStIndex()];
    IreadoffNode *ireadoff = mirModule.mirBuilder->CreateExprIreadoff(symty, symalloc.offset + offset, rrn);
    return ireadoff;
  }
}

static MIRType *GetPointedToType(const MIRPtrType *pointerty) {
  MIRType *atype = globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx);
  if (atype->GetKind() == kTypeArray) {
    MIRArrayType *arraytype = static_cast<MIRArrayType *>(atype);
    return globaltable.GetTypeFromTyIdx(arraytype->eTyIdx);
  }
  if (atype->GetKind() == kTypeFArray || atype->GetKind() == kTypeJArray) {
    MIRFarrayType *farraytype = static_cast<MIRFarrayType *>(atype);
    return globaltable.GetTypeFromTyIdx(farraytype->elemTyIdx);
  }
  return globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx);
}

BaseNode *MmplLowerer::LowerIread(IreadNode *expr) {
  int32 offset = 0;
  MIRType *type = globaltable.GetTypeFromTyIdx(expr->tyIdx);
  MIRPtrType *pointerty = static_cast<MIRPtrType *>(type);
  CHECK_FATAL(pointerty, "expect a pointer type at iread node");
  if (expr->fieldID != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx));
    CHECK_FATAL(structty, "SelectIread: non-zero fieldID for non-structure");
    FieldPair thepair = structty->TraverseToField(expr->fieldID);
    type = globaltable.GetTypeFromTyIdx(thepair.second.first);
    offset = becommon.GetFieldOffset(structty, expr->fieldID).first;
  } else {
    type = GetPointedToType(pointerty);
  }
  BaseNode *ireadoff = mirModule.mirBuilder->CreateExprIreadoff(type->GetPrimType(), offset, expr->uopnd);
  return ireadoff;
}

void MmplLowerer::LowerAggDassign(BlockNode *newblk, const DassignNode *dsnode) {
  MIRSymbol *lhssymbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dsnode->stIdx);
  int32 lhsoffset = 0;
  MIRType *lhsty = lhssymbol->GetType();
  if (dsnode->fieldID != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(lhssymbol->GetType());
    CHECK_FATAL(structty, "LowerAggDassign: non-zero fieldID for non-structure");
    FieldPair thepair = structty->TraverseToField(dsnode->fieldID);
    lhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
    lhsoffset = becommon.GetFieldOffset(structty, dsnode->fieldID).first;
  }
  uint32 lhsalign = becommon.type_align_table[lhsty->tyIdx.idx];
  uint32 lhssize = becommon.type_size_table[lhsty->tyIdx.idx];

  uint32 rhsalign;
  uint32 alignused;
  int32 rhsoffset = 0;
  BaseNode *loadnode = nullptr;
  IassignoffNode *iassignoff = nullptr;
  if (dsnode->GetRhs()->op == OP_dread) {
    AddrofNode *rhsdread = static_cast<AddrofNode *>(dsnode->GetRhs());
    MIRSymbol *rhssymbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(rhsdread->stIdx);
    MIRType *rhsty = rhssymbol->GetType();
    if (rhsdread->fieldID != 0) {
      MIRStructType *structty = dynamic_cast<MIRStructType *>(rhssymbol->GetType());
      CHECK_FATAL(structty, "SelectDassign: non-zero fieldID for non-structure");
      FieldPair thepair = structty->TraverseToField(rhsdread->fieldID);
      rhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(structty, rhsdread->fieldID).first;
    }
    rhsalign = becommon.type_align_table[rhsty->tyIdx.idx];
    BaseNode *rRrn = ReadregNodeForSymbol(rhssymbol);
    SymbolAlloc &rsymalloc = rhssymbol->IsLocal() ? memlayout->sym_alloc_table[rhssymbol->GetStIndex()]
                                                  : globmemlayout->sym_alloc_table[rhssymbol->GetStIndex()];
    BaseNode *lRrn = ReadregNodeForSymbol(lhssymbol);
    SymbolAlloc &lsymalloc = lhssymbol->IsLocal() ? memlayout->sym_alloc_table[lhssymbol->GetStIndex()]
                                                  : globmemlayout->sym_alloc_table[lhssymbol->GetStIndex()];

    alignused = std::min(lhsalign, rhsalign);
    alignused = std::min(alignused, 4u);  // max alignment is 32-bit
    if (!alignused) {
      alignused = 1u;
    }
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprIreadoff(UnsignedPrimType(alignused),
                                                           rsymalloc.offset + rhsoffset + i * alignused, rRrn);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(
        UnsignedPrimType(alignused), lsymalloc.offset + lhsoffset + i * alignused, lRrn, loadnode);
      newblk->AddStatement(iassignoff);
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssizeCovered = (lhssize / alignused) * alignused;
    uint32 newalignused = alignused;
    while (lhssizeCovered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssizeCovered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprIreadoff(UnsignedPrimType(newalignused),
                                                           rsymalloc.offset + rhsoffset + lhssizeCovered, rRrn);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(
        UnsignedPrimType(newalignused), lsymalloc.offset + lhsoffset + lhssizeCovered, lRrn, loadnode);
      newblk->AddStatement(iassignoff);
      lhssizeCovered += newalignused;
    }
  } else if (dsnode->GetRhs()->op == OP_regread) {
    RegreadNode *regread = static_cast<RegreadNode *>(dsnode->GetRhs());
    CHECK_FATAL(regread->regIdx == -kSregRetval0 && regread->primType == PTY_agg, "");

    BaseNode *lRrn = ReadregNodeForSymbol(lhssymbol);
    SymbolAlloc &lsymalloc = lhssymbol->IsLocal() ? memlayout->sym_alloc_table[lhssymbol->GetStIndex()]
                                                  : globmemlayout->sym_alloc_table[lhssymbol->GetStIndex()];

    alignused = std::min(lhsalign, 4u);  // max alignment is 32-bit
    PregIdx ridx = -kSregRetval0;
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprRegread(UnsignedPrimType(alignused), ridx - i);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(
        UnsignedPrimType(alignused), lsymalloc.offset + lhsoffset + i * alignused, lRrn, loadnode);
      newblk->AddStatement(iassignoff);
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssizeCovered = (lhssize / alignused) * alignused;
    ridx = -kSregRetval0 - (lhssize / alignused);
    uint32 newalignused = alignused;
    while (lhssizeCovered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssizeCovered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprRegread(UnsignedPrimType(newalignused), ridx--);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(
        UnsignedPrimType(newalignused), lsymalloc.offset + lhsoffset + lhssizeCovered, lRrn, loadnode);
      newblk->AddStatement(iassignoff);
      lhssizeCovered += newalignused;
    }
  } else {  // iread
    IreadNode *rhsiread = static_cast<IreadNode *>(dsnode->GetRhs());
    CHECK_FATAL(rhsiread, "LowerAggDassign: illegal rhs for dassign node of structure type");
    rhsiread->SetOpnd(LowerExpr(rhsiread, rhsiread, rhsiread->uopnd, newblk), 0);
    MIRType *rhsRdTy = globaltable.GetTypeFromTyIdx(rhsiread->tyIdx);
    MIRPtrType *pointerty = static_cast<MIRPtrType *>(rhsRdTy);
    CHECK_FATAL(pointerty, "LowerAggDassign: expect a pointer type at iread node");
    if (rhsiread->fieldID != 0) {
      MIRStructType *structty = dynamic_cast<MIRStructType *>(globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx));
      CHECK_FATAL(structty, "LowerAggDassign: non-zero fieldID for non-structure");
      FieldPair thepair = structty->TraverseToField(rhsiread->fieldID);
      rhsRdTy = globaltable.GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(structty, rhsiread->fieldID).first;
    } else {
      rhsRdTy = GetPointedToType(pointerty);
    }
    rhsalign = becommon.type_align_table[rhsRdTy->tyIdx.idx];
    BaseNode *lRrn = ReadregNodeForSymbol(lhssymbol);
    CHECK(lhssymbol->GetStIndex() < memlayout->sym_alloc_table.size() &&
            lhssymbol->GetStIndex() < globmemlayout->sym_alloc_table.size(),
          "index oout of range in MmplLowerer::LowerAggDassign");
    SymbolAlloc &lsymalloc = lhssymbol->IsLocal() ? memlayout->sym_alloc_table[lhssymbol->GetStIndex()]
                                                  : globmemlayout->sym_alloc_table[lhssymbol->GetStIndex()];

    alignused = std::min(lhsalign, rhsalign);
    alignused = std::min(alignused, 4u);  // max alignment is 32-bit
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprIreadoff(UnsignedPrimType(alignused), rhsoffset + i * alignused,
                                                           rhsiread->uopnd);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(
        UnsignedPrimType(alignused), lsymalloc.offset + lhsoffset + i * alignused, lRrn, loadnode);
      newblk->AddStatement(iassignoff);
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssizeCovered = (lhssize / alignused) * alignused;
    uint32 newalignused = alignused;
    while (lhssizeCovered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssizeCovered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprIreadoff(UnsignedPrimType(newalignused), rhsoffset + lhssizeCovered,
                                                           rhsiread->uopnd);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(
        UnsignedPrimType(newalignused), lsymalloc.offset + lhsoffset + lhssizeCovered, lRrn, loadnode);
      newblk->AddStatement(iassignoff);
      lhssizeCovered += newalignused;
    }
  }
}

void MmplLowerer::LowerRegassign(RegassignNode *regnode, BlockNode *newblk) {
  CHECK_FATAL(false, "NYI");
}

void MmplLowerer::LowerDassign(DassignNode *dsnode, BlockNode *newblk) {
  if (dsnode->GetRhs()->primType != PTY_agg) {
    dsnode->SetRhs(LowerExpr(dsnode, dsnode, dsnode->GetRhs(), newblk));
    MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dsnode->stIdx);
    int32 offset = 0;
    PrimType ptypused = symbol->GetType()->primType;
    if (dsnode->fieldID != 0) {
      MIRStructType *structty = dynamic_cast<MIRStructType *>(symbol->GetType());
      CHECK_FATAL(structty, "MmplLowerer::LowerDassign: non-zero fieldID for non-structure");
      offset = becommon.GetFieldOffset(structty, dsnode->fieldID).first;
      TyIdx ftyidx = structty->TraverseToField(dsnode->fieldID).second.first;
      ptypused = globaltable.GetTypeFromTyIdx(ftyidx)->primType;
    }
    PregIdx spcreg = GetSpecialRegFromSt(symbol);
    if (spcreg == -kSregFp) {
      IassignFPoffNode *iassignoff = mirModule.mirBuilder->CreateStmtIassignFPoff(
        ptypused, memlayout->sym_alloc_table[symbol->GetStIndex()].offset + offset, dsnode->GetRhs());
      newblk->AddStatement(iassignoff);
    } else {
      BaseNode *rrn = ReadregNodeForSymbol(symbol);
      SymbolAlloc &symalloc = symbol->IsLocal() ? memlayout->sym_alloc_table[symbol->GetStIndex()]
                                                : globmemlayout->sym_alloc_table[symbol->GetStIndex()];
      IassignoffNode *iassignoff =
        mirModule.mirBuilder->CreateStmtIassignoff(ptypused, symalloc.offset + offset, rrn, dsnode->GetRhs());
      newblk->AddStatement(iassignoff);
    }
  } else {
    LowerAggDassign(newblk, dsnode);
  }
}

void MmplLowerer::LowerAggIassign(BlockNode *newblk, IassignNode *iassign) {
  int32 lhsoffset = 0;
  MIRType *lhsty = globaltable.GetTypeFromTyIdx(iassign->tyIdx);
  MIRPtrType *pointerty = static_cast<MIRPtrType *>(lhsty);
  CHECK_FATAL(pointerty, "LowerAggIassign: expect a pointer type at iassign node");
  if (iassign->fieldID != 0) {
    MIRStructType *structty = dynamic_cast<MIRStructType *>(globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx));
    CHECK_FATAL(structty, "LowerAggDassign: non-zero fieldID for non-structure");
    FieldPair thepair = structty->TraverseToField(iassign->fieldID);
    lhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
    lhsoffset = becommon.GetFieldOffset(structty, iassign->fieldID).first;
  } else {
    lhsty = GetPointedToType(pointerty);
  }
  uint32 lhsalign = becommon.type_align_table[lhsty->tyIdx.idx];
  uint32 lhssize = becommon.type_size_table[lhsty->tyIdx.idx];

  uint32 rhsalign;
  uint32 alignused;
  int32 rhsoffset = 0;
  BaseNode *loadnode = nullptr;
  IassignoffNode *iassignoff = nullptr;
  if (iassign->rhs->op == OP_dread) {
    AddrofNode *rhsdread = static_cast<AddrofNode *>(iassign->rhs);
    MIRSymbol *rhssymbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(rhsdread->stIdx);
    MIRType *rhsty = rhssymbol->GetType();
    if (rhsdread->fieldID != 0) {
      MIRStructType *structty = dynamic_cast<MIRStructType *>(rhssymbol->GetType());
      CHECK_FATAL(structty, "SelectDassign: non-zero fieldID for non-structure");
      FieldPair thepair = structty->TraverseToField(rhsdread->fieldID);
      rhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(structty, rhsdread->fieldID).first;
    }
    rhsalign = becommon.type_align_table[rhsty->tyIdx.idx];
    BaseNode *rRrn = ReadregNodeForSymbol(rhssymbol);
    CHECK(rhssymbol->GetStIndex() < memlayout->sym_alloc_table.size() &&
            rhssymbol->GetStIndex() < globmemlayout->sym_alloc_table.size(),
          "index out of range in MmplLowerer::LowerAggIassign");
    SymbolAlloc &rsymalloc = rhssymbol->IsLocal() ? memlayout->sym_alloc_table[rhssymbol->GetStIndex()]
                                                  : globmemlayout->sym_alloc_table[rhssymbol->GetStIndex()];

    alignused = std::min(lhsalign, rhsalign);
    alignused = std::min(alignused, 4u);  // max alignment is 32-bit
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprIreadoff(UnsignedPrimType(alignused),
                                                           rsymalloc.offset + rhsoffset + i * alignused, rRrn);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(UnsignedPrimType(alignused), lhsoffset + i * alignused,
                                                               iassign->addrExpr, loadnode);
      newblk->AddStatement(iassignoff);
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssizeCovered = (lhssize / alignused) * alignused;
    uint32 newalignused = alignused;
    while (lhssizeCovered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssizeCovered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprIreadoff(UnsignedPrimType(newalignused),
                                                           rsymalloc.offset + rhsoffset + lhssizeCovered, rRrn);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(UnsignedPrimType(newalignused),
                                                               lhsoffset + lhssizeCovered, iassign->addrExpr, loadnode);
      newblk->AddStatement(iassignoff);
      lhssizeCovered += newalignused;
    }
  } else if (iassign->rhs->op == OP_regread) {
    RegreadNode *regread = static_cast<RegreadNode *>(iassign->rhs);
    CHECK_FATAL(regread->regIdx == -kSregRetval0 && regread->primType == PTY_agg, "");

    alignused = std::min(lhsalign, 4u);  // max alignment is 32-bit
    PregIdx ridx = -kSregRetval0;
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprRegread(UnsignedPrimType(alignused), ridx - i);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(UnsignedPrimType(alignused), lhsoffset + i * alignused,
                                                               iassign->addrExpr, loadnode);
      newblk->AddStatement(iassignoff);
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssizeCovered = (lhssize / alignused) * alignused;
    ridx = -kSregRetval0 - (lhssize / alignused);
    uint32 newalignused = alignused;
    while (lhssizeCovered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssizeCovered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprRegread(UnsignedPrimType(newalignused), ridx--);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(UnsignedPrimType(newalignused),
                                                               lhsoffset + lhssizeCovered, iassign->addrExpr, loadnode);
      newblk->AddStatement(iassignoff);
      lhssizeCovered += newalignused;
    }
  } else {  // iread
    IreadNode *rhsiread = static_cast<IreadNode *>(iassign->rhs);
    CHECK_FATAL(rhsiread, "LowerAggIassign: illegal rhs for dassign node of structure type");
    rhsiread->SetOpnd(LowerExpr(rhsiread, rhsiread, rhsiread->uopnd, newblk), 0);
    MIRType *rhsRdTy = globaltable.GetTypeFromTyIdx(rhsiread->tyIdx);
    MIRPtrType *pointerty = static_cast<MIRPtrType *>(rhsRdTy);
    CHECK_FATAL(pointerty, "LowerAggIassign: expect a pointer type at iread node");
    if (rhsiread->fieldID != 0) {
      MIRStructType *structty = dynamic_cast<MIRStructType *>(globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx));
      CHECK_FATAL(structty, "LowerAggIassign: non-zero fieldID for non-structure");
      FieldPair thepair = structty->TraverseToField(rhsiread->fieldID);
      rhsRdTy = globaltable.GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(structty, rhsiread->fieldID).first;
    } else {
      rhsRdTy = GetPointedToType(pointerty);
    }
    rhsalign = becommon.type_align_table[rhsRdTy->tyIdx.idx];

    alignused = std::min(lhsalign, rhsalign);
    alignused = std::min(alignused, 4u);  // max alignment is 32-bit
    for (uint32 i = 0; i < (lhssize / alignused); i++) {
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprIreadoff(UnsignedPrimType(alignused), rhsoffset + i * alignused,
                                                           rhsiread->uopnd);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(UnsignedPrimType(alignused), lhsoffset + i * alignused,
                                                               iassign->addrExpr, loadnode);
      newblk->AddStatement(iassignoff);
    }
    // take care of extra content at the end less than the unit of alignused
    uint32 lhssizeCovered = (lhssize / alignused) * alignused;
    uint32 newalignused = alignused;
    while (lhssizeCovered < lhssize) {
      newalignused = newalignused >> 1;
      if (lhssizeCovered + newalignused > lhssize) {
        continue;
      }
      // generate the load
      loadnode = mirModule.mirBuilder->CreateExprIreadoff(UnsignedPrimType(newalignused), rhsoffset + lhssizeCovered,
                                                           rhsiread->uopnd);
      // generate the store
      iassignoff = mirModule.mirBuilder->CreateStmtIassignoff(UnsignedPrimType(newalignused),
                                                               lhsoffset + lhssizeCovered, iassign->addrExpr, loadnode);
      newblk->AddStatement(iassignoff);
      lhssizeCovered += newalignused;
    }
  }
}

void MmplLowerer::LowerIassign(IassignNode *iassign, BlockNode *newblk) {
  iassign->addrExpr = LowerExpr(iassign, iassign, iassign->addrExpr, newblk);
  if (iassign->rhs->primType != PTY_agg) {
    iassign->rhs = LowerExpr(iassign, iassign, iassign->rhs, newblk);
    int32 offset = 0;
    MIRType *type = globaltable.GetTypeFromTyIdx(iassign->tyIdx);
    MIRPtrType *pointerty = static_cast<MIRPtrType *>(type);
    CHECK_FATAL(pointerty, "LowerIassign::expect a pointer type at iassign node");
    if (iassign->fieldID != 0) {
      MIRStructType *structty = dynamic_cast<MIRStructType *>(globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx));
      CHECK_FATAL(structty, "LowerAggIassign: non-zero fieldID for non-structure");
      offset = becommon.GetFieldOffset(structty, iassign->fieldID).first;
      TyIdx ftyidx = structty->TraverseToField(iassign->fieldID).second.first;
      type = globaltable.GetTypeFromTyIdx(ftyidx);
    } else {
      type = GetPointedToType(pointerty);
    }
    PrimType ptypused = type->primType;
    IassignoffNode *iassignoff =
      mirModule.mirBuilder->CreateStmtIassignoff(ptypused, offset, iassign->addrExpr, iassign->rhs);
    newblk->AddStatement(iassignoff);
  } else {
    LowerAggIassign(newblk, iassign);
  }
}

void MmplLowerer::LowerAggReturn(NaryStmtNode *retnode, BlockNode *newblk) {
  uint32 rhssize;
  uint32 rhsalign;
  MIRType *rhsty = nullptr;
  int32 rhsoffset = 0;
  if (retnode->nOpnd[0]->op == OP_dread) {
    AddrofNode *rhsdread = static_cast<AddrofNode *>(retnode->nOpnd[0]);
    MIRSymbol *rhssymbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(rhsdread->stIdx);
    rhsty = rhssymbol->GetType();
    if (rhsdread->fieldID != 0) {
      MIRStructType *structty = dynamic_cast<MIRStructType *>(rhssymbol->GetType());
      CHECK_FATAL(structty, "LowerAggReturn: non-zero fieldID for non-structure");
      FieldPair thepair = structty->TraverseToField(rhsdread->fieldID);
      rhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(structty, rhsdread->fieldID).first;
    }
    rhssize = becommon.type_size_table[rhsty->tyIdx.idx];
    rhsalign = becommon.type_align_table[rhsty->tyIdx.idx];
    rhsalign = std::min(rhsalign, 4u);  // max alignment is 32-bit
    BaseNode *rRrn = ReadregNodeForSymbol(rhssymbol);
    CHECK_FATAL(rhssymbol->GetStIndex() < memlayout->sym_alloc_table.size() &&
             rhssymbol->GetStIndex() < globmemlayout->sym_alloc_table.size(),
           "index out range in MmplLowerer::LowerAggReturn");
    SymbolAlloc &rsymalloc = rhssymbol->IsLocal() ? memlayout->sym_alloc_table[rhssymbol->GetStIndex()]
                                                  : globmemlayout->sym_alloc_table[rhssymbol->GetStIndex()];

    retnode->nOpnd.pop_back();
    for (uint32 i = 0; i < (rhssize / rhsalign); i++) {
      // generate the load
      retnode->nOpnd.push_back(mirModule.mirBuilder->CreateExprIreadoff(
        UnsignedPrimType(rhsalign), rsymalloc.offset + rhsoffset + i * rhsalign, rRrn));
    }
    // take care of extra content at the end less than the unit of rhsalign
    uint32 rhssizeCovered = (rhssize / rhsalign) * rhsalign;
    uint32 newalignused = rhsalign;
    while (rhssizeCovered < rhssize) {
      newalignused = newalignused >> 1;
      if (rhssizeCovered + newalignused > rhssize) {
        continue;
      }
      // generate the load
      retnode->nOpnd.push_back(mirModule.mirBuilder->CreateExprIreadoff(
        UnsignedPrimType(newalignused), rsymalloc.offset + rhsoffset + rhssizeCovered, rRrn));
      rhssizeCovered += newalignused;
    }
    return;
  }
  if (retnode->nOpnd[0]->op == OP_regread) {
    RegreadNode *regread = static_cast<RegreadNode *>(retnode->nOpnd[0]);
    if (regread->regIdx == -kSregRetval0 && regread->primType == PTY_agg) {  // rhs is %%retval, no need change anything
      return;
    }
  }
  if (retnode->nOpnd[0]->op == OP_iread) {  // rhs is iread
    IreadNode *rhsiread = static_cast<IreadNode *>(retnode->nOpnd[0]);
    rhsiread->SetOpnd(LowerExpr(rhsiread, rhsiread, rhsiread->uopnd, newblk), 0);

    rhsty = globaltable.GetTypeFromTyIdx(rhsiread->tyIdx);
    MIRPtrType *pointerty = static_cast<MIRPtrType *>(rhsty);
    CHECK_FATAL(pointerty, "expect a pointer type at iread node");
    if (rhsiread->fieldID != 0) {
      MIRStructType *structty = dynamic_cast<MIRStructType *>(globaltable.GetTypeFromTyIdx(pointerty->pointedTyIdx));
      CHECK_FATAL(structty, "SelectIread: non-zero fieldID for non-structure");
      FieldPair thepair = structty->TraverseToField(rhsiread->fieldID);
      rhsty = globaltable.GetTypeFromTyIdx(thepair.second.first);
      rhsoffset = becommon.GetFieldOffset(structty, rhsiread->fieldID).first;
    } else {
      rhsty = GetPointedToType(pointerty);
    }
    rhssize = becommon.type_size_table[rhsty->tyIdx.idx];
    rhsalign = becommon.type_align_table[rhsty->tyIdx.idx];
    rhsalign = std::min(rhsalign, 4u);  // max alignment is 32-bit
    retnode->nOpnd.pop_back();
    for (uint32 i = 0; i < (rhssize / rhsalign); i++) {
      // generate the load
      retnode->nOpnd.push_back(mirModule.mirBuilder->CreateExprIreadoff(UnsignedPrimType(rhsalign),
                                                                         rhsoffset + i * rhsalign, rhsiread->uopnd));
    }
    // take care of extra content at the end less than the unit of rhsalign
    uint32 rhssizeCovered = (rhssize / rhsalign) * rhsalign;
    uint32 newalignused = rhsalign;
    while (rhssizeCovered < rhssize) {
      newalignused = newalignused >> 1;
      if (rhssizeCovered + newalignused > rhssize) {
        continue;
      }
      // generate the load
      retnode->nOpnd.push_back(mirModule.mirBuilder->CreateExprIreadoff(UnsignedPrimType(newalignused),
                                                                         rhsoffset + rhssizeCovered, rhsiread->uopnd));
      rhssizeCovered += newalignused;
    }
    return;
  }
  CHECK_FATAL(false, "MmplLowerer::LowerAggReturn: operand must be either dread or iread");
}

// called only if the return has more than 1 operand
BlockNode *MmplLowerer::LowerReturn(NaryStmtNode *retnode) {
  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  CHECK_FATAL(retnode->nOpnd.size() > 0, "index out of range in MmplLowerer::LowerReturn");
  if (retnode->nOpnd[0]->primType == PTY_agg) {
    LowerAggReturn(retnode, blk);
  }
  CHECK_FATAL(retnode->nOpnd.size() == 1, "MmplLowerer::LowerReturn: return with more than 1 operand NYI");

  // insert regassign for the returned value
  BaseNode *rhs0 = LowerExpr(retnode, retnode, retnode->nOpnd[0], blk);
  CHECK_FATAL(rhs0 != nullptr, "rhs0 is null in MmplLowerer::LowerReturn");
  RegassignNode *regasgn = mirModule.mirBuilder->CreateStmtRegassign(rhs0->primType, -kSregRetval0, rhs0);
  blk->AddStatement(regasgn);
  retnode->nOpnd.pop_back();  // remove the return operand
  retnode->numOpnds--;
  blk->AddStatement(retnode);
  return blk;
}

BaseNode *MmplLowerer::LowerIntrinsicop(BaseNode *parent, IntrinsicopNode *intrinnode, BlockNode *newblk) {
  return intrinnode;
}

StmtNode *MmplLowerer::LowerIntrinsiccall(IntrinsiccallNode *intrincall, BlockNode *newblk) {
  for (int32 i = 0; i < intrincall->numOpnds; i++) {
    intrincall->SetOpnd(LowerExpr(intrincall, intrincall, intrincall->Opnd(i), newblk), i);
  }
  return intrincall;
}

}  // namespace maplebe
