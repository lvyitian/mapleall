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

#include "be_common.h"
#include "arm_mem_layout.h"
#include "arm_abi.h"
#include "arm_cg.h"
#include "cg_assert.h"

namespace maplebe {

using namespace maple;

int32 ArmMemLayout::StackFrameSize() {
  return seg__args_regpassed.size + seg_locals.size + seg__args_to_stkpass.size;
}

// go over all outgoing calls in the function body and get the maximum space
// needed for storing the actuals based on the actual parameters and the ABI;
// this assumes that all nesting of statements has been removed, so that all
// the statements are at only one block level
uint32 ArmMemLayout::FindLargestActualArea(void) {
  StmtNode *stmt = func->body->first;
  if (!stmt) {
    return 0;
  }
  int32 max_actual_size = 0;
  for (; stmt; stmt = stmt->next) {
    Opcode opcode = stmt->op;
    if (opcode != OP_call && opcode != OP_icall && opcode != OP_intrinsiccall) {
      continue;
    }
    ParmLocator parmlocator(be);  // instantiate a parm locator
    int32 i = 0;
    if (opcode == OP_icall) {
      i++;
    }
    for (; i < stmt->NumOpnds(); i++) {
      base_node_t *opnd = GenericOpnd(stmt, i);
      MIRType *ty = nullptr;
      if (opnd->primType != PTY_agg) {
        ty = globaltable.typeTable[static_cast<uint32>(opnd->primType)];
      } else {
        Opcode opnd_opcode = opnd->op;
        CG_ASSERT(opnd_opcode == OP_dread || opnd_opcode == OP_iread, "");
        if (opnd_opcode == OP_dread) {
          AddrofNode *dread = static_cast<AddrofNode *>(opnd);
          MIRSymbol *sym;
          sym = be.mirModule.CurFunction()->GetLocalOrGlobalSymbol(dread->stidx);
          ty = globaltable.GetTypeFromTyIdx(sym->GetTyIdx());
          if (dread->fieldid != 0) {
            CG_ASSERT(ty->typeKind == kTypeStruct || ty->typeKind == kTypeClass, "");
            FieldPair thepair;
            if (ty->typeKind == kTypeStruct) {
              thepair = static_cast<MIRStructType *>(ty)->TraverseToField(dread->fieldid);
            } else {
              thepair = static_cast<MIRClassType *>(ty)->TraverseToField(dread->fieldid);
            }
            ty = globaltable.GetTypeFromTyIdx(thepair.second.first);
          }
        } else {  // OP_iread
          IreadNode *iread = static_cast<IreadNode *>(opnd);
          ty = globaltable.GetTypeFromTyIdx(iread->tyidx);
          CG_ASSERT(ty->typeKind == kTypePointer, "");
          ty = globaltable.GetTypeFromTyIdx(static_cast<MIRPtrType *>(ty)->pointedTyIdx);
          if (iread->fieldid != 0) {
            CG_ASSERT(ty->typeKind == kTypeStruct || ty->typeKind == kTypeClass, "");
            FieldPair thepair;
            if (ty->typeKind == kTypeStruct) {
              thepair = static_cast<MIRStructType *>(ty)->TraverseToField(iread->fieldid);
            } else {
              thepair = static_cast<MIRClassType *>(ty)->TraverseToField(iread->fieldid);
            }
            ty = globaltable.GetTypeFromTyIdx(thepair.second.first);
          }
        }
      }
      PLocInfo ploc;
      parmlocator.LocateNextParm(ty, ploc);
      if (ploc.reg0 != 0) {
        continue;  // passed in register, so no effect on actual area
      }
      max_actual_size = std::max(max_actual_size, ploc.memoffset + ploc.memsize);
      max_actual_size = RoundUp(max_actual_size, SIZEOFPTR);
    }
  }
  max_actual_size = RoundUp(max_actual_size, SIZEOFPTR * 2);
  return max_actual_size;
}

void ArmMemLayout::LayoutStackFrame() {
  MIRSymbol *sym = nullptr;
  ParmLocator parmlocator(be);
  PLocInfo ploc;
  for (uint32 i = 0; i < func->formals.size(); i++) {
    sym = func->formals[i];
    MIRType *ty = globaltable.GetTypeFromTyIdx(func->argumentsTyidx[i]);
    parmlocator.LocateNextParm(ty, ploc);
    uint32 stindex = sym->GetStIndex();
    sym_alloc_table[stindex] = mem_allocator->GetMemPool()->New<SymbolAlloc>();
    if (ploc.reg0 != 0) {
      sym_alloc_table[stindex]->mem_segment = &seg__args_regpassed;
      seg__args_regpassed.size = RoundUp(seg__args_regpassed.size, be.type_align_table[ty->_ty_idx.idx]);
      sym_alloc_table[stindex]->offset = seg__args_regpassed.size;
      seg__args_regpassed.size += be.type_size_table[ty->_ty_idx.idx];
      seg__args_regpassed.size = RoundUp(seg__args_regpassed.size, SIZEOFPTR);
    } else {
      sym_alloc_table[stindex]->mem_segment = &seg__args_stkpassed;
      seg__args_stkpassed.size = RoundUp(seg__args_stkpassed.size, be.type_align_table[ty->_ty_idx.idx]);
      sym_alloc_table[stindex]->offset = seg__args_stkpassed.size;
      seg__args_stkpassed.size += be.type_size_table[ty->_ty_idx.idx];
      seg__args_stkpassed.size = RoundUp(seg__args_stkpassed.size, SIZEOFPTR);
      std::cout << "LAYOUT: formal %" << globaltable.GetStringFromGstridx(sym->GetNameStridx());
      std::cout << " at seg__args_stkpassed offset " << sym_alloc_table[stindex]->offset << " passed in memory\n";
    }
  }
  seg__args_regpassed.size = RoundUp(seg__args_regpassed.size, SIZEOFPTR * 2);
  // allocate the local variables in the stack
  uint32 symtabsize = func->symtab->GetSymbolTableSize();
  for (uint32 i = 0; i < symtabsize; i++) {
    sym = func->symtab->GetSymbolFromStidx(i);
    if (!sym || sym->storageClass != kScAuto) {
      continue;
    }
    uint32 stindex = sym->GetStIndex();
    TyIdx tyidx = sym->GetTyIdx();
    sym_alloc_table[stindex] = mem_allocator->GetMemPool()->New<SymbolAlloc>();
    sym_alloc_table[stindex]->mem_segment = &seg_locals;
    seg_locals.size = RoundUp(seg_locals.size, be.type_align_table[tyidx.idx]);
    sym_alloc_table[stindex]->offset = seg_locals.size;
    seg_locals.size += be.type_size_table[tyidx.idx];
    seg_locals.size = RoundUp(seg_locals.size, SIZEOFPTR);
  }
  seg_locals.size = RoundUp(seg_locals.size, SIZEOFPTR * 2);
  // for the actual
  seg__args_to_stkpass.size = FindLargestActualArea();
}

}  // namespace maplebe
