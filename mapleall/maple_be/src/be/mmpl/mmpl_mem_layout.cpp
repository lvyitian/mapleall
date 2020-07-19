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

// For each function being compiled, lay out its parameters, return values and
// local variables on its stack frame.  This involves determining how parameters
// and return values are passed from analyzing their types.
//
// Allocate all the global variables within the global memory block which is
// addressed via offset from the global pointer GP during execution.  Allocate
// this block pointed to by mirModule.globalBlkMap and perform the static
// initializations.

#include "be_common.h"
#include "mmplmem_layout.h"
#include <iostream>

namespace maplebe {

using namespace maple;

static inline void SetBit(uint8 *bitvector, int32 bitno) {
  if (bitno < 0) {
    bitno = -bitno - 1;
  }
  uint32 byteindex = bitno >> 3;
  uint32 bitindex = bitno & 7;
  bitvector[byteindex] |= (1 << bitindex);
}

uint32 MmplMemLayout::FindLargestActualArea(StmtNode *stmt, int &maxActualSize) {
#ifdef DYNAMICLANG
  // Dynamic language donnot have a certain type for functions, so compiler cannot estimate the actual size.
  // Donot allocate space for arguments here. MAPLEVM will allocate space at runtime.
  maxActualSize = 0;
  return maxActualSize;
#else
  if (!stmt) {
    return max_actual_size;
  }
  Opcode opcode = stmt->op;
  switch (opcode) {
    case OP_block: {
      BlockNode *blcknode = static_cast<BlockNode *>(stmt);
      for (StmtNode *itstmt = blcknode->first; itstmt; itstmt = itstmt->next) {
        FindLargestActualArea(itstmt, max_actual_size);
      }
      break;
    }
    case OP_if: {
      IfStmtNode *ifnode = static_cast<IfStmtNode *>(stmt);
      FindLargestActualArea(ifnode->thenPart, max_actual_size);
      FindLargestActualArea(ifnode->elsePart, max_actual_size);
      break;
    }
    case OP_doloop: {
      FindLargestActualArea(static_cast<DoloopNode *>(stmt)->doBody, max_actual_size);
      break;
    }
    case OP_dowhile:
    case OP_while:
      FindLargestActualArea(static_cast<WhileStmtNode *>(stmt)->body, max_actual_size);
      break;
    case OP_call:
    case OP_icall:
    case OP_intrinsiccall: {
      ParmLocator parmlocator(be);  // instantiate a parm locator
      NaryStmtNode *callstmt = static_cast<NaryStmtNode *>(stmt);
      for (uint32 i = 0; i < callstmt->nOpnd.size(); i++) {
        base_node_t *opnd = callstmt->nOpnd[i];
        CHECK_FATAL(opnd->primType != PTY_void, "");
        MIRType *ty = nullptr;
        if (opnd->primType != PTY_agg) {
          ty = be.mirModule.typeTable[static_cast<uint32>(opnd->primType)];
        } else {
          Opcode opnd_opcode = opnd->op;
          CHECK_FATAL(opnd_opcode == OP_dread || opnd_opcode == OP_iread, "");
          if (opnd_opcode == OP_dread) {
            AddrofNode *dread = static_cast<AddrofNode *>(opnd);
            MIRSymbol *sym = be.mirModule.CurFunction()->GetLocalOrGlobalSymbol(dread->stIdx);
            ty = be.mirModule.GetTypeFromTyIdx(sym->GetTyIdx());
            if (dread->fieldID != 0) {
              CHECK_FATAL(ty->typeKind == kTypeStruct || ty->typeKind == kTypeClass, "");
              FieldPair thepair = static_cast<MIRStructType *>(ty)->TraverseToField(dread->fieldID);
              ty = be.mirModule.GetTypeFromTyIdx(thepair.second);
            }
          } else {  // OP_iread
            IreadNode *iread = static_cast<IreadNode *>(opnd);
            ty = be.mirModule.GetTypeFromTyIdx(iread->tyIdx);
            CHECK_FATAL(ty->typeKind == kTypePointer, "");
            ty = be.mirModule.GetTypeFromTyIdx(static_cast<MIRPtrType *>(ty)->pointedTyIdx);
            if (iread->fieldID != 0) {
              CHECK_FATAL(ty->typeKind == kTypeStruct || ty->typeKind == kTypeClass, "");
              FieldPair thepair = static_cast<MIRStructType *>(ty)->TraverseToField(iread->fieldID);
              ty = be.mirModule.GetTypeFromTyIdx(thepair.second);
            }
          }
        }
        PLocInfo ploc;
        parmlocator.LocateNextParm(ty, ploc);
        max_actual_size = std::max(max_actual_size, ploc.memoffset + ploc.memsize);
        max_actual_size = RoundUp(max_actual_size, SIZEOFPTR);
      }
      break;
    }
    default:
      return max_actual_size;
  }
  max_actual_size = RoundUp(max_actual_size, SIZEOFPTR);
  return max_actual_size;
#endif
}

// go over all outgoing calls in the function body and get the maximum space
// needed for storing the actuals based on the actual parameters and the ABI;
// this assumes that all nesting of statements has been removed, so that all
// the statements are at only one block level
uint32 MmplMemLayout::FindLargestActualArea(void) {
  int32 maxActualSize = 0;
  FindLargestActualArea(func->body, maxActualSize);
  return static_cast<uint32>(maxActualSize);
}

void MmplMemLayout::LayoutStackFrame(void) {
  MIRSymbol *sym = nullptr;
  // StIdx stIdx;
  // go through formal parameters
  ParmLocator parmlocator(be);  // instantiate a parm locator
  PLocInfo ploc;
  for (uint32 i = 0; i < func->formals.size(); i++) {
    sym = func->formals[i];
    MIRType *ty = globaltable.GetTypeFromTyIdx(func->argumentsTyidx[i]);
    parmlocator.LocateNextParm(ty, ploc);
    uint32 stindex = sym->GetStIndex();
    // always passed in memory, so allocate in seg_upformal
    sym_alloc_table[stindex].mem_segment = &seg_upformal;
    seg_upformal.size = RoundUp(seg_upformal.size, be.type_align_table[ty->tyIdx.idx]);
    sym_alloc_table[stindex].offset = seg_upformal.size;
    seg_upformal.size += be.type_size_table[ty->tyIdx.idx];
    seg_upformal.size = RoundUp(seg_upformal.size, SIZEOFPTR);
    LogInfo::MapleLogger() << "LAYOUT: formal %" << globaltable.GetStringFromGstridx(sym->GetNameStridx());
    LogInfo::MapleLogger() << " at seg_upformal offset " << sym_alloc_table[stindex].offset << " passed in memory\n";
  }

  // allocate seg_formal in seg_FPbased
  seg_formal.how_alloc.mem_segment = &seg_FPbased;
  seg_FPbased.size = RoundDown(seg_FPbased.size, SIZEOFPTR);
  seg_FPbased.size -= seg_formal.size;
  seg_FPbased.size = RoundDown(seg_FPbased.size, SIZEOFPTR);
  seg_formal.how_alloc.offset = seg_FPbased.size;
  LogInfo::MapleLogger() << "LAYOUT: seg_formal at seg_FPbased offset " << seg_formal.how_alloc.offset << " with size "
            << seg_formal.size << std::endl;

  // allocate the local variables
  uint32 symtabsize = func->symtab->GetSymbolTableSize();
  for (uint32 i = 0; i < symtabsize; i++) {
    sym = func->symtab->GetSymbolFromStidx(i);
    if (!sym) {
      continue;
    }
    if (sym->storageClass != kScAuto) {
      continue;
    }
    uint32 stindex = sym->GetStIndex();
    sym_alloc_table[stindex].mem_segment = &seg_FPbased;
    seg_FPbased.size -= be.type_size_table[sym->GetTyIdx().idx];
    seg_FPbased.size = RoundDown(seg_FPbased.size, be.type_align_table[sym->GetTyIdx().idx]);
    sym_alloc_table[stindex].offset = seg_FPbased.size;
    LogInfo::MapleLogger() << "LAYOUT: local %" << globaltable.GetStringFromGstridx(sym->GetNameStridx());
    LogInfo::MapleLogger() << " at FPbased offset " << sym_alloc_table[stindex].offset << std::endl;
  }
  seg_FPbased.size = RoundDown(seg_FPbased.size, SIZEOFPTR);

  // allocate seg_actual for storing the outgoing parameters; this requires
  // going over all outgoing calls and get the maximum space needed for the
  // actuals
  seg_actual.size = FindLargestActualArea();

  // allocate seg_actual in seg_SPbased
  seg_actual.how_alloc.mem_segment = &seg_SPbased;
  seg_actual.how_alloc.offset = seg_SPbased.size;
  seg_SPbased.size = RoundUp(seg_SPbased.size, SIZEOFPTR);
  seg_SPbased.size += seg_actual.size;
  seg_SPbased.size = RoundUp(seg_SPbased.size, SIZEOFPTR);
  LogInfo::MapleLogger() << "LAYOUT: seg_actual at seg_SPbased offset " << seg_actual.how_alloc.offset << " with size "
            << seg_actual.size << std::endl;

  // go thru the function's symbol table to set typetagged and refcounted bitvectors
  if (UpformalSize() != 0) {
    func->formalWordsTypeTagged = static_cast<uint8 *>(be.mirModule.memPool->Calloc(BlkSize2BitvectorSize(UpformalSize())));
    func->formalWordsRefCounted = static_cast<uint8 *>(be.mirModule.memPool->Calloc(BlkSize2BitvectorSize(UpformalSize())));
  }
  func->localWordsTypeTagged = static_cast<uint8 *>(be.mirModule.memPool->Calloc(BlkSize2BitvectorSize(UpformalSize())));
  func->localWordsRefCounted = static_cast<uint8 *>(be.mirModule.memPool->Calloc(BlkSize2BitvectorSize(UpformalSize())));
  for (uint32 i = 0; i < symtabsize; i++) {
    MIRType *ty = nullptr;
    sym = func->symtab->GetSymbolFromStidx(i);
    if (!sym) {
      continue;
    }
    uint32 stindex = sym->GetStIndex();
    int32 symaddress = sym_alloc_table[stindex].offset;
    uint8 *typetaggedBv = nullptr;
    uint8 *refcountedBv = nullptr;
    if (sym->storageClass == kScFormal && sym_alloc_table[stindex].mem_segment == &seg_upformal) {
      typetaggedBv = func->formalWordsTypeTagged;
      refcountedBv = func->formalWordsRefCounted;
    } else if (sym->storageClass == kScAuto) {
      typetaggedBv = func->localWordsTypeTagged;
      refcountedBv = func->localWordsRefCounted;
    } else {
      continue;
    }
    ty = sym->GetType();
    if (ty->GetKind() == kTypeScalar) {
      if (IsPrimitiveDynType(ty->GetPrimType())) {
        SetBit(typetaggedBv, symaddress / 4);
      } else if (ty->GetPrimType() == PTY_simplestr || ty->GetPrimType() == PTY_simpleobj) {
        SetBit(refcountedBv, symaddress / 4);
      }
    } else if (ty->GetKind() == kTypeArray) {
      MIRArrayType *arraytype = static_cast<MIRArrayType *>(ty);
      MIRType *elemtype = arraytype->GetElemType();
      int32 elemsize = elemtype->GetSize();
      CHECK_FATAL(arraytype->dim == 1, "");
      if (IsPrimitiveDynType(elemtype->GetPrimType())) {
        for (int32 i = 0; i < static_cast<int32>(arraytype->sizeArray[0]); i++) {
          SetBit(typetaggedBv, (symaddress + i * elemsize) / 4);
        }
      } else if (elemtype->GetPrimType() == PTY_simplestr || elemtype->GetPrimType() == PTY_simpleobj) {
        for (int32 i = 0; i < static_cast<int32>(arraytype->sizeArray[0]); i++) {
          SetBit(refcountedBv, (symaddress + i * elemsize) / 4);
        }
      }
    }
  }
}

inline uint8 GetU8Const(MIRConst *c) {
  MIRIntConst *intconst = static_cast<MIRIntConst *>(c);
  return static_cast<uint8>(intconst->value);
}

inline uint16 GetU16Const(MIRConst *c) {
  MIRIntConst *intconst = static_cast<MIRIntConst *>(c);
  return static_cast<uint16>(intconst->value);
}

inline uint32 GetU32Const(MIRConst *c) {
  MIRIntConst *intconst = static_cast<MIRIntConst *>(c);
  return static_cast<uint32>(intconst->value);
}

inline uint64 GetU64Const(MIRConst *c) {
  MIRIntConst *intconst = static_cast<MIRIntConst *>(c);
  return static_cast<uint64>(intconst->value);
}

inline uint32 GetF32Const(MIRConst *c) {
  MIRFloatConst *floatconst = static_cast<MIRFloatConst *>(c);
  return static_cast<uint32>(floatconst->value.ivalue_);
}

inline uint64 GetF64Const(MIRConst *c) {
  MIRDoubleConst *doubleconst = static_cast<MIRDoubleConst *>(c);
  return static_cast<uint64>(doubleconst->value.ivalue_);
}

void GlobalMemLayout::FillScalarValueInMap(uint32 startaddress, PrimType pty, MIRConst *c) {
  switch (pty) {
    case PTY_u1:
    case PTY_u8:
    case PTY_i8: {
      uint8 *p = &be_.mirModule.globalBlkMap[startaddress];
      *p = GetU8Const(c);
      break;
    }
    case PTY_u16:
    case PTY_i16: {
      uint16 *p = static_cast<uint16 *>(&be_.mirModule.globalBlkMap[startaddress]);
      *p = GetU16Const(c);
      break;
    }
    case PTY_u32:
    case PTY_i32: {
      uint32 *p = static_cast<uint32 *>(&be_.mirModule.globalBlkMap[startaddress]);
      *p = GetU32Const(c);
      break;
    }
    case PTY_u64:
    case PTY_i64: {
      uint64 *p = static_cast<uint64 *>(&be_.mirModule.globalBlkMap[startaddress]);
      *p = GetU64Const(c);
      break;
    }
    case PTY_f32: {
      uint32 *p = static_cast<uint32 *>(&be_.mirModule.globalBlkMap[startaddress]);
      *p = GetF32Const(c);
      break;
    }
    case PTY_f64: {
      uint64 *p = static_cast<uint64 *>(&be_.mirModule.globalBlkMap[startaddress]);
      *p = GetF64Const(c);
      break;
    }
    default:
      CHECK_FATAL(false, "FillScalarValueInMap: NYI");
  }
  return;
}

void GlobalMemLayout::FillTypeValueInMap(uint32 startaddress, MIRType *ty, MIRConst *c) {
  switch (ty->GetKind()) {
    case kTypeScalar:
      FillScalarValueInMap(startaddress, ty->GetPrimType(), c);
      break;
    case kTypeArray: {
      MIRArrayType *arraytype = static_cast<MIRArrayType *>(ty);
      MIRType *elemtype = arraytype->GetElemType();
      int32 elemsize = elemtype->GetSize();
      MIRAggConst *aggconst = dynamic_cast<MIRAggConst *>(c);
      CHECK_FATAL(aggconst, "FillTypeValueInMap: inconsistent array initialization specification");
      MapleVector<MIRConst *> &constvec = aggconst->const_vec;
      for (MapleVector<MIRConst *>::iterator it = constvec.begin(); it != constvec.end();
           it++, startaddress += elemsize) {
        FillTypeValueInMap(startaddress, elemtype, *it);
      }
      break;
    }
    case kTypeStruct: {
      MIRStructType *structty = static_cast<MIRStructType *>(ty);
      MIRAggConst *aggconst = dynamic_cast<MIRAggConst *>(c);
      CHECK_FATAL(aggconst, "FillTypeValueInMap: inconsistent struct initialization specification");
      MapleVector<MIRConst *> &constvec = aggconst->const_vec;
      uint32 fieldID = 0;
      for (MapleVector<MIRConst *>::iterator it = constvec.begin(); it != constvec.end(); ++it++, ++fieldID) {
        if ((*it)->fieldID) {
          fieldID = (*it)->fieldID;
        }
        FieldPair thepair = structty->TraverseToField(fieldID);
        MIRType *fieldty = globaltable.GetTypeFromTyIdx(thepair.second.first);
        uint32 offset = be_.GetFieldOffset(structty, fieldID).first;
        FillTypeValueInMap(startaddress + offset, fieldty, *it);
      }
      break;
    }
    case kTypeClass: {
      MIRClassType *classty = static_cast<MIRClassType *>(ty);
      MIRAggConst *aggconst = dynamic_cast<MIRAggConst *>(c);
      CHECK_FATAL(aggconst, "FillTypeValueInMap: inconsistent class initialization specification");
      MapleVector<MIRConst *> &constvec = aggconst->const_vec;
      uint32 fieldID = 0;
      for (MapleVector<MIRConst *>::iterator it = constvec.begin(); it != constvec.end(); ++it, ++fieldID) {
        if ((*it)->fieldID) {
          fieldID = (*it)->fieldID;
        }
        FieldPair thepair = classty->TraverseToField(fieldID);
        MIRType *fieldty = globaltable.GetTypeFromTyIdx(thepair.second.first);
        uint32 offset = be_.GetFieldOffset(classty, fieldID).first;
        FillTypeValueInMap(startaddress + offset, fieldty, *it);
      }
      break;
    }
    default:
      CHECK_FATAL(false, "FillTypeValueInMap: NYI");
  }
}

void GlobalMemLayout::FillSymbolValueInMap(const MIRSymbol *sym) {
  if (sym->GetConst() == nullptr) {
    return;
  }
  uint32 stindex = sym->GetStIndex();
  CHECK(stindex < sym_alloc_table.size(), "index out of range in GlobalMemLayout::FillSymbolValueInMap");
  uint32 symaddress = sym_alloc_table[stindex].offset;
  FillTypeValueInMap(symaddress, sym->GetType(), sym->GetConst());
  return;
}

void GlobalMemLayout::FillTypetaggedRefcountedBit(const MIRSymbol *sym) {
  MIRType *ty = sym->GetType();
  uint32 stindex = sym->GetStIndex();

  CHECK(stindex < sym_alloc_table.size(), "index out of range in GlobalMemLayout::FillTypetaggedRefcountedBit");
  int32 symaddress = sym_alloc_table[stindex].offset;
  if (ty->GetKind() == kTypeScalar) {
    if (IsPrimitiveDynType(ty->GetPrimType())) {
      SetBit(be_.mirModule.globalWordsTypeTagged, symaddress / 4);
    } else if (ty->GetPrimType() == PTY_simplestr || ty->GetPrimType() == PTY_simpleobj) {
      SetBit(be_.mirModule.globalWordsRefCounted, symaddress / 4);
    }
  } else if (ty->GetKind() == kTypeArray) {
    MIRArrayType *arraytype = static_cast<MIRArrayType *>(ty);
    MIRType *elemtype = arraytype->GetElemType();
    int32 elemsize = elemtype->GetSize();
    CHECK_FATAL(arraytype->dim == 1, "");
    int32 i;
    if (IsPrimitiveDynType(elemtype->GetPrimType()))
      for (i = 0; i < static_cast<int32>(arraytype->sizeArray[0]); i++) {
        SetBit(be_.mirModule.globalWordsTypeTagged, (symaddress + i * elemsize) / 4);
      }
    else if (elemtype->GetPrimType() == PTY_simplestr || elemtype->GetPrimType() == PTY_simpleobj)
      for (i = 0; i < static_cast<int32>(arraytype->sizeArray[0]); i++) {
        SetBit(be_.mirModule.globalWordsRefCounted, (symaddress + i * elemsize) / 4);
      }
  }
}

GlobalMemLayout::GlobalMemLayout(BECommon &be, MapleAllocator *mallocator)
  : seg_GPbased(MS_GPbased), sym_alloc_table(mallocator->Adapter()), be_(be) {
  uint32 symtabsize = globaltable.GetSymbolTableSize();
  sym_alloc_table.resize(symtabsize);
  MIRSymbol *sym = nullptr;
  // StIdx stIdx;
  // allocate the global variables ordered based on alignments
  for (int32 curalign = 8; curalign != 0; curalign >>= 1) {
    for (uint32 i = 0; i < symtabsize; i++) {
      sym = globaltable.GetSymbolFromStidx(i);
      if (!sym) {
        continue;
      }
      if (sym->storageClass != kScGlobal && sym->storageClass != kScFstatic) {
        continue;
      }
      if (be.type_align_table[sym->GetTyIdx().idx] != curalign) {
        continue;
      }
      uint32 stindex = sym->GetStIndex();
      sym_alloc_table[stindex].mem_segment = &seg_GPbased;
      seg_GPbased.size = RoundUp(seg_GPbased.size, be.type_align_table[sym->GetTyIdx().idx]);
      sym_alloc_table[stindex].offset = seg_GPbased.size;
      seg_GPbased.size += be.type_size_table[sym->GetTyIdx().idx];
      LogInfo::MapleLogger() << "LAYOUT: global %" << globaltable.GetStringFromGstridx(sym->GetNameStridx());
      LogInfo::MapleLogger() << " at GPbased offset " << sym_alloc_table[stindex].offset << std::endl;
    }
  }
  seg_GPbased.size = RoundUp(seg_GPbased.size, SIZEOFPTR);
  be.mirModule.globalMemSize = seg_GPbased.size;
  // allocate the memory map for the GP block
  be.mirModule.globalBlkMap = static_cast<uint8 *>(be.mirModule.memPool->Calloc(seg_GPbased.size));
  // allocate global_words_typetagged and global_words_refcounted
  be.mirModule.globalWordsTypeTagged =
      static_cast<uint8 *>(be.mirModule.memPool->Calloc(BlkSize2BitvectorSize(seg_GPbased.size)));
  be.mirModule.globalWordsRefCounted =
      static_cast<uint8 *>be.mirModule.memPool->Calloc(BlkSize2BitvectorSize(seg_GPbased.size));
  // perform initialization on globalblkmap
  for (uint32 i = 0; i < symtabsize; i++) {
    sym = globaltable.GetSymbolFromStidx(i);
    if (!sym) {
      continue;
    }
    if (sym->storageClass != kScGlobal && sym->storageClass != kScFstatic) {
      continue;
    }
    FillSymbolValueInMap(sym);
    FillTypetaggedRefcountedBit(sym);
  }
}

// LocateNextParm should be called with each parameter in the parameter list
// starting from the beginning, one call per parameter in sequence; it returns
// the information on how each parameter is passed in ploc
void ParmLocator::LocateNextParm(const MIRType *ty, PLocInfo &ploc) {
  ploc.memoffset = last_memoffset_;
  ploc.memsize = GetPrimTypeSize(ty->GetPrimType());

  uint32 rightpad = 0;
  parm_num_++;

  switch (ty->GetPrimType()) {
    case PTY_u1:
    case PTY_u8:
    case PTY_i8:
    case PTY_u16:
    case PTY_i16:
      rightpad = GetPrimTypeSize(PTY_i32) - ploc.memsize;
      break;
    case PTY_a32:
    case PTY_u32:
    case PTY_i32:
    case PTY_a64:
    case PTY_u64:
    case PTY_i64:
    case PTY_ptr:
    case PTY_ref:
#ifdef DYNAMICLANG
    case PTY_simplestr:
    case PTY_simpleobj:
    case PTY_dynany:
    case PTY_dyni32:
    case PTY_dynf64:
    case PTY_dynstr:
    case PTY_dynobj:
    case PTY_dynundef:
    case PTY_dynbool:
    case PTY_dynf32:
    case PTY_dynnone:
    case PTY_dynnull:
#endif
      break;

    case PTY_f32:
      rightpad = GetPrimTypeSize(PTY_f64) - ploc.memsize;
      break;
    case PTY_c64:
    case PTY_f64:
      break;

    case PTY_c128:
      break;

    case PTY_agg: {
      ploc.memsize = be_.type_size_table[ty->tyIdx.idx];
      // compute rightpad
      int32 paddedSize = RoundUp(ploc.memsize, 8);
      rightpad = paddedSize - ploc.memsize;
      break;
    }
    default:
      CHECK_FATAL(false, "");
  }

  last_memoffset_ = ploc.memoffset + ploc.memsize + rightpad;
  return;
}

// instantiated with the type of the function return value, it describes how
// the return value is to be passed back to the caller
ReturnMechanism::ReturnMechanism(const MIRType *retty, BECommon &be) : fake_first_parm(false) {
  switch (retty->GetPrimType()) {
    case PTY_u1:
    case PTY_u8:
    case PTY_i8:
    case PTY_u16:
    case PTY_i16:
    case PTY_a32:
    case PTY_u32:
    case PTY_i32:
    case PTY_a64:
    case PTY_u64:
    case PTY_i64:
    case PTY_f32:
    case PTY_f64:
#ifdef DYNAMICLANG
    case PTY_simplestr:
    case PTY_simpleobj:
    case PTY_dynany:
    case PTY_dyni32:
    case PTY_dynstr:
    case PTY_dynobj:
#endif
      ptype0 = retty->GetPrimType();
      return;

    case PTY_c64:
    case PTY_c128:
      fake_first_parm = true;
      ptype0 = PTY_a32;
      return;

    case PTY_agg: {
      uint32 size = be.type_size_table[retty->tyIdx.idx];
      if (size > 4) {
        fake_first_parm = true;
        ptype0 = PTY_a32;
      } else {
        ptype0 = PTY_u32;
      }
      return;
    }

    default:
      return;
  }
}

}  // namespace maplebe
