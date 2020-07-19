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

#include "aarch64_mem_layout.h"
#include "aarch64_cg_func.h"
#include "aarch64_rt_support.h"
#include "be_common.h"
#include "mir_nodes.h"
#include "special_func.h"

namespace maplebe {

using namespace maple;

#define CLANG  (be.mirModule.IsCModule())

/*
   Returns stack space required for a call
   which is used to pass arguments that cannot be
   passed through registers
 */
uint32 AArch64MemLayout::ComputeStackSpaceRequirementForCall(StmtNode *stmt, int32 &aggCopySize, bool isIcall) {
  ParmLocator parmlocator(be);  // instantiate a parm locator
  uint32 sizeOfArgsToStkpass = 0;
  int32 i = 0;
  if (isIcall) { /* An indirect call's first operand is the invocation target */
    i++;
  }

  CallNode *callnode = dynamic_cast<CallNode *>(stmt);
  if (callnode) {
    MIRFunction *fn = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callnode->puIdx);
    MIRSymbol *fsym = be.mirModule.CurFunction()->GetLocalOrGlobalSymbol(fn->stIdx, false);
    if (IsRtNativeCall(fsym->GetName())) {
      i++;
    }
  }

  int32 structCopySize = 0;
  for (uint32 anum = 0; i < stmt->NumOpnds(); ++i, ++anum) {
    base_node_t *opnd = stmt->Opnd(i);
    MIRType *ty = nullptr;
    if (opnd->primType != PTY_agg) {
      ty = GlobalTables::GetTypeTable().typeTable[static_cast<uint32>(opnd->primType)];
    } else {
      Opcode opndOpcode = opnd->op;
      CG_ASSERT(opndOpcode == OP_dread || opndOpcode == OP_iread, "");
      if (opndOpcode == OP_dread) {
        DreadNode *dread = static_cast<DreadNode *>(opnd);
        MIRSymbol *sym = be.mirModule.CurFunction()->GetLocalOrGlobalSymbol(dread->stIdx);
        ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->GetTyIdx());
        if (dread->fieldID != 0) {
          CG_ASSERT(ty->typeKind == kTypeStruct || ty->typeKind == kTypeClass, "");
          FieldPair thepair;
          if (ty->typeKind == kTypeStruct) {
            thepair = static_cast<MIRStructType *>(ty)->TraverseToField(dread->fieldID);
          } else {
            thepair = static_cast<MIRClassType *>(ty)->TraverseToField(dread->fieldID);
          }
          ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
        }
      } else {  // OP_iread
        IreadNode *iread = static_cast<IreadNode *>(opnd);
        ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(iread->tyIdx);
        CG_ASSERT(ty->typeKind == kTypePointer, "");
        ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(ty)->pointedTyIdx);
        if (iread->fieldID != 0) {
          CG_ASSERT(ty->typeKind == kTypeStruct || ty->typeKind == kTypeClass, "");
          FieldPair thepair;
          if (ty->typeKind == kTypeStruct) {
            thepair = static_cast<MIRStructType *>(ty)->TraverseToField(iread->fieldID);
          } else {
            thepair = static_cast<MIRClassType *>(ty)->TraverseToField(iread->fieldID);
          }
          ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
        }
      }
    }
    PLocInfo ploc;
    structCopySize += parmlocator.LocateNextParm(ty, ploc, anum == 0);
    if (ploc.reg0 != 0) {
      continue;  // passed in register, so no effect on actual area
    }
    sizeOfArgsToStkpass = RoundUp(ploc.memoffset + ploc.memsize, SIZEOFPTR);
  }
  aggCopySize = structCopySize;
  return sizeOfArgsToStkpass;
}

void AArch64MemLayout::LayoutStackFrame(int32 &structCopySize, int32 &maxParmStackSize) {
  MIRSymbol *sym = nullptr;
  ParmLocator parmlocator(be);
  PLocInfo ploc;
  bool nostackpara = false;

  // Count named args passed in registers
  if (CLANG && func->GetAttr(FUNCATTR_varargs)) {
    uint32 nintregs = 0;
    uint32 nfpregs = 0;
    ParmLocator vparmlocator(be);
    PLocInfo vploc;
    for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
      if (i == 0) {
        auto funcIt = be.funcReturnType.find(func);
        if (funcIt != be.funcReturnType.end() && be.type_size_table[funcIt->second.GetIdx()] <= 16) {
          continue;
        }
      }
      MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->formalDefVec[i].formalTyIdx);
      uint32_t ptyIdx = ty->tyIdx.GetIdx();
      vparmlocator.LocateNextParm(ty, vploc);
      if (vploc.reg0 != kRinvalid) {
        if (vploc.reg0 >= R0 && vploc.reg0 <= R7) {
          nintregs++;
        } else if (vploc.reg0 >= V0 && vploc.reg0 <= V7) {
          nfpregs++;
        }
      }
      if (vploc.reg1 != kRinvalid) {
        if (vploc.reg1 >= R0 && vploc.reg1 <= R7) {
          nintregs++;
        } else if (vploc.reg1 >= V0 && vploc.reg1 <= V7) {
          nfpregs++;
        }
      }
      if (vploc.reg2 != kRinvalid) {
        if (vploc.reg2 >= R0 && vploc.reg2 <= R7) {
          nintregs++;
        } else if (vploc.reg2 >= V0 && vploc.reg2 <= V7) {
          nfpregs++;
        }
      }
      if (vploc.reg3 != kRinvalid) {
        if (vploc.reg3 >= R0 && vploc.reg3 <= R7) {
          nintregs++;
        } else if (vploc.reg3 >= V0 && vploc.reg3 <= V7) {
          nfpregs++;
        }
      }
    }
    seg_GRSavearea.size = (8 - nintregs) * SIZEOFPTR;
    seg_VRSavearea.size = (8 - nfpregs) * SIZEOFPTR * 2;
  }
  std::vector<MIRSymbol *> retDelays;
  for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
    sym = func->formalDefVec[i].formalSym;
    nostackpara = false;
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->formalDefVec[i].formalTyIdx);
    uint32_t ptyIdx = ty->tyIdx.GetIdx();
    parmlocator.LocateNextParm(ty, ploc, i == 0);
    uint32 stindex = sym->GetStIndex();
    AArch64SymbolAlloc *symloc = mem_allocator->GetMemPool()->New<AArch64SymbolAlloc>();
    sym_alloc_table[stindex] = symloc;
    CG_ASSERT(symloc, "sym loc should have been defined");
    uint32 align = be.type_align_table[ptyIdx];
    uint32 msize = be.type_size_table[ptyIdx];
    if (be.type_size_table[ptyIdx] > 16) {
      align = 8;  // size > 16 is passed on stack, the formal is just a pointer
      msize = SIZEOFPTR;
    }
    if (ploc.reg0 != kRinvalid) {  // register

      if (false && sym->IsRefType()) {
        symloc->SetRegisters(ploc.reg0, ploc.reg1, ploc.reg2, ploc.reg3);
        symloc->mem_segment = &seg_reflocals;
        seg_reflocals.size = RoundUp(seg_reflocals.size, align);
        symloc->offset = seg_reflocals.size;
        seg_reflocals.size += msize;
        seg_reflocals.size = RoundUp(seg_reflocals.size, SIZEOFPTR);
      } else {
        symloc->SetRegisters(ploc.reg0, ploc.reg1, ploc.reg2, ploc.reg3);
        if (!sym->IsPreg()) {
          symloc->mem_segment = &seg__args_regpassed;

          // the type's alignment requirement may be smaller than a registser's byte size
          seg__args_regpassed.size = RoundUp(seg__args_regpassed.size, align);
          symloc->offset = seg__args_regpassed.size;
          seg__args_regpassed.size += msize;
        } else {
          nostackpara = true;
        }
      }
    } else {  // stack
      symloc->mem_segment = &seg__args_stkpassed;
      seg__args_stkpassed.size = RoundUp(seg__args_stkpassed.size, align);
      symloc->offset = seg__args_stkpassed.size;
      seg__args_stkpassed.size += msize;
      // We need it as dictated by the AArch64 ABI $5.4.2 C12
      seg__args_stkpassed.size = RoundUp(seg__args_stkpassed.size, SIZEOFPTR);
    }
  }

  // We do need this as LDR/STR with immediate
  // requires imm be aligned at a 8/4-byte boundary,
  // and local varirables may need 8-byte alignment.
  seg__args_regpassed.size = RoundUp(seg__args_regpassed.size, SIZEOFPTR);
  // we do need this as SP has to be aligned at a 16-bytes bounardy
  seg__args_stkpassed.size = RoundUp(seg__args_stkpassed.size, SIZEOFPTR * 2);
  // allocate the local variables in the stack
  uint32 symtabsize = (func->symTab == nullptr ? 0 : func->symTab->GetSymbolTableSize());
  for (uint32 i = 0; i < symtabsize; i++) {
    sym = func->symTab->GetSymbolFromStIdx(i);
    if (!sym || sym->storageClass != kScAuto || sym->IsDeleted()) {
      continue;
    }
    uint32 stindex = sym->GetStIndex();
    TyIdx tyIdx = sym->GetTyIdx();
    MIRType *ty =  GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
    uint32 align = be.type_align_table[tyIdx.GetIdx()];
    AArch64SymbolAlloc *symloc = mem_allocator->GetMemPool()->New<AArch64SymbolAlloc>();
    sym_alloc_table[stindex] = symloc;
    CG_ASSERT(!symloc->IsRegister(), "");

    if (sym->IsRefType()) {
      if (func->retRefSym.find(sym) != func->retRefSym.end()) {
        // try to put ret_ref at the end of seg_reflocals
        retDelays.push_back(sym);
        continue;
      }
      symloc->mem_segment = &seg_reflocals;
      if (ty->GetPrimType() == PTY_agg && align < 8) {
        seg_reflocals.size = RoundUp(seg_reflocals.size, 8);
      } else {
        seg_reflocals.size = RoundUp(seg_reflocals.size, align);
      }
      symloc->offset = seg_reflocals.size;
      seg_reflocals.size += be.type_size_table[tyIdx.GetIdx()];

    } else {
      symloc->mem_segment = &seg_locals;
      if (ty->GetPrimType() == PTY_agg && align < 8) {
        seg_locals.size = RoundUp(seg_locals.size, 8);
      } else {
        seg_locals.size = RoundUp(seg_locals.size, align);
      }
      if (func->stackallocVarMap.find(sym) != func->stackallocVarMap.end()) {
        symloc->offset = seg_locals.size;
        MapleMap<MIRSymbol *, uint32>::iterator it = func->stackallocVarMap.find(sym);
        seg_locals.size += it->second;
      } else {
        if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->typeKind == kTypeClass) {
          // If this is a non-escaped object allocated on stack, we need to
          // manufacture an extra object header for RC to work correctly
          seg_locals.size += AArch64RTSupport::kObjectHeaderSize;
        }
        symloc->offset = seg_locals.size;
        seg_locals.size += be.type_size_table[tyIdx.GetIdx()];
      }
    }
  }

  // handle ret_ref sym now
  uint32 retsize = retDelays.size();
  for (uint32 i = 0; i < retsize; i++) {
    sym = retDelays[i];
    uint32 stindex = sym->GetStIndex();
    TyIdx tyIdx = sym->GetTyIdx();
    AArch64SymbolAlloc *symloc = mem_allocator->GetMemPool()->New<AArch64SymbolAlloc>();
    sym_alloc_table[stindex] = symloc;
    CG_ASSERT(!symloc->IsRegister(), "");

    CG_ASSERT(sym->IsRefType(), "");
    symloc->mem_segment = &seg_reflocals;
    seg_reflocals.size = RoundUp(seg_reflocals.size, be.type_align_table[tyIdx.GetIdx()]);
    symloc->offset = seg_reflocals.size;
    seg_reflocals.size += be.type_size_table[tyIdx.GetIdx()];
  }

  seg__args_to_stkpass.size = FindLargestActualArea(structCopySize);
  maxParmStackSize = seg__args_to_stkpass.size;
  if (g->optim_level < 2) {
    AssignSpillLocationsToPseudoRegisters();
  } else {
    AArch64CGFunc *aarchCgfunc = static_cast<AArch64CGFunc *>(cgfunc);
    aarchCgfunc->SetJavaCatchRegno(cgfunc->New_V_Reg(kRegTyInt, 8));
  }

  seg_reflocals.size = RoundUp(seg_reflocals.size, SIZEOFPTR);
  seg_locals.size = RoundUp(seg_locals.size, SIZEOFPTR);

  // for the actual arguments that cannot be pass through registers
  // need to allocate space for caller-save registers
  for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
    sym = func->formalDefVec[i].formalSym;
    if (sym->IsPreg()) {
      continue;
    }
    uint32 stindex = sym->GetStIndex();
    AArch64SymbolAlloc *symloc = static_cast<AArch64SymbolAlloc *>(sym_alloc_table[stindex]);
    if (symloc->mem_segment == &seg__args_regpassed) {  // register
      /*
         In O0, we store parameters passed via registers into memory.
         So, each of such parameter needs to get assigned storage in stack.
         If a function parameter is never accessed in the function body,
         and if we don't create its memory operand here, its offset gets
         computed when the instruction to store its value into stack
         is generated in the prologue when its memory operand is created.
         But, the parameter would see a different StackFrameSize than
         the parameters that are accessed in the body, because
         the size of the storage for FP/LR is added to the stack frame
         size in between.
         To make offset assignment easier, we create a memory operand
         for each of function parameters in advance.
         This has to be done after all of formal parameters and local
         variables get assigned their respecitve storage, i.e.
         CallFrameSize (discounting callee-saved and FP/LR) is known.
       */
      MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->formalDefVec[i].formalTyIdx);
      uint32_t ptyIdx = ty->tyIdx.GetIdx();
      static_cast<AArch64CGFunc *>(cgfunc)->GetOrCreateMemOpnd(sym, 0, be.type_align_table[ptyIdx] * BITS_PER_BYTE);
    }
  }

  fixstacksize = RealStackFrameSize();
}

void AArch64MemLayout::AssignSpillLocationsToPseudoRegisters() {
  MIRPregTable *pregTab = cgfunc->func->pregTab;

  size_t nRegs = pregTab->Size();
  spill_loc_table.resize(nRegs);
  for (uint32 i = 1; i < nRegs; i++) {
    PrimType primType = pregTab->PregFromPregIdx(i)->primType;
    AArch64SymbolAlloc *symloc = mem_allocator->GetMemPool()->New<AArch64SymbolAlloc>();
    symloc->mem_segment = &seg_locals;
    seg_locals.size = RoundUp(seg_locals.size, GetPrimTypeSize(primType));
    symloc->offset = seg_locals.size;
    MIRType *mirty = GlobalTables::GetTypeTable().typeTable[primType];
    seg_locals.size += be.type_size_table[mirty->tyIdx.GetIdx()];

    spill_loc_table[i] = symloc;
  }

  // Allocate additional stack space for "thrownval".
  seg_locals.size = RoundUp(seg_locals.size, 8);
  AArch64CGFunc *aarchCgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  RegOperand *baseOpnd = aarchCgfunc->GetOrCreateStackBaseRegOperand();
  int32 offset = seg_locals.size + seg_lockobjslot.size;

  if (!aarchCgfunc->UseFP() && !aarchCgfunc->HasVLAOrAlloca() && seg__args_to_stkpass.size > 0) {
    offset += seg__args_to_stkpass.size;
  }

  AArch64OfstOperand *offsetOpnd = aarchCgfunc->memPool->New<AArch64OfstOperand>(offset + 16, 64);
  AArch64MemOperand *throwmem = aarchCgfunc->memPool->New<AArch64MemOperand>(
    AArch64MemOperand::kAddrModeBOi, 64, baseOpnd, static_cast<AArch64RegOperand *>(nullptr), offsetOpnd, nullptr);
  aarchCgfunc->SetJavaCatchOpnd(throwmem);
  aarchCgfunc->gen_memopnds_requiring_offset_adjustment_.insert(throwmem);
  seg_locals.size += 8;
}

SymbolAlloc *AArch64MemLayout::AssignLocationToSpillReg(regno_t vrnum) {
  AArch64SymbolAlloc *symloc = mem_allocator->GetMemPool()->New<AArch64SymbolAlloc>();
  symloc->mem_segment = &seg__spillreg;
  uint32_t regsize = cgfunc->GetVRegSize(vrnum);
  seg__spillreg.size = RoundUp(seg__spillreg.size, regsize);
  symloc->offset = seg__spillreg.size;
  seg__spillreg.size += regsize;
  spillreg_loc_map[vrnum] = symloc;
  return symloc;
}

int32 AArch64MemLayout::GetadjustforRefloc() {
  int32 total = seg__args_regpassed.size + locals().size + GetSizeOfSpillReg();
  AArch64CGFunc *aarchCgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  if (!aarchCgfunc->UseFP() && !aarchCgfunc->HasVLAOrAlloca() && seg__args_to_stkpass.size > 0) {
    total += seg__args_to_stkpass.size;
  }
  return total;
}

int32 AArch64MemLayout::StackFrameSize() {
  int32 total = seg__args_regpassed.size +
                static_cast<AArch64CGFunc *>(cgfunc)->SizeOfCalleeSaved() +
                GetSizeOfRefLocals() +
                locals().size +
                GetSizeOfSpillReg() +
                seg_lockobjslot.size;

  if (GetSizeOfGRSavearea() > 0) {
    total += RoundUp(GetSizeOfGRSavearea(), AARCH64_STACK_PTR_ALIGNMENT);
  }
  if (GetSizeOfVRSavearea() > 0) {
    total += RoundUp(GetSizeOfVRSavearea(), AARCH64_STACK_PTR_ALIGNMENT);
  }

  // if the function does not have VLA nor alloca,
  // we allocate space for arguments to stack-pass
  // in the call frame; otherwise, it has to be allocated for each call and reclaimed afterward.
  total += seg__args_to_stkpass.size;
  int finalsize = RoundUp(total, AARCH64_STACK_PTR_ALIGNMENT);
  if (finalsize - total >= 8) {
    unusedslot = true;
  } else {
    unusedslot = false;
  }

  return finalsize;
}

bool AArch64MemLayout::GetUnusedSlot() {
  return unusedslot;
}

int32 AArch64MemLayout::RealStackFrameSize() {
  int32 size = StackFrameSize();
  if (cgfunc->cg->AddStackGuard()) {
    size += AARCH64_STACK_PTR_ALIGNMENT;
  }
  return size;
}

void AArch64MemLayout::AdjustOffsetForCalleeSaved(SymbolAlloc &sym) {
  sym.offset += static_cast<AArch64CGFunc *>(cgfunc)->SizeOfCalleeSaved();
}

int32 AArch64MemLayout::GetReflocbaseLoc() {
  AArch64CGFunc *aarchCgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  int32 beforesize = GetSizeOfLocals() + seg_lockobjslot.size;
  int32 argsToStkpassSize = SizeOfArgsToStackpass();
  if (!aarchCgfunc->UseFP() && !aarchCgfunc->HasVLAOrAlloca() && argsToStkpassSize > 0) {
    beforesize += argsToStkpassSize;
  }
  int sizeofFplr = 2 * kIntregBytelen;
  if (!aarchCgfunc->ShouldSaveFPLR() || aarchCgfunc->UsedStpSubPairForCallFrameAllocation()) {
    return beforesize;
  }
  return beforesize + sizeofFplr;
}

int32 AArch64MemLayout::GetVRSaveareaBaseLoc() {
  int32 total = RealStackFrameSize() -
                RoundUp(GetSizeOfGRSavearea(), AARCH64_STACK_PTR_ALIGNMENT) -
                RoundUp(GetSizeOfVRSavearea(), AARCH64_STACK_PTR_ALIGNMENT);

  AArch64CGFunc *aarchCgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  if (aarchCgfunc->UseFP()) {
    total -= SizeOfArgsToStackpass();
  }
  return total;
}

int32 AArch64MemLayout::GetGRSaveareaBaseLoc() {
  int32 total = RealStackFrameSize() -
                RoundUp(GetSizeOfGRSavearea(), AARCH64_STACK_PTR_ALIGNMENT);

  AArch64CGFunc *aarchCgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  if (aarchCgfunc->UseFP()) {
    total -= SizeOfArgsToStackpass();
  }
  return total;
}

}  // namespace maplebe
