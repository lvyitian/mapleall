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

#include "cg.h"
#include "ark_mem_layout.h"
#include "ark_cg_func.h"
#include "ark_rt_support.h"
#include "be_common.h"
#include "mir_nodes.h"
#include "special_func.h"

namespace maplebe {

using namespace maple;

/*
   Returns stack space required for a call
   which is used to pass arguments that cannot be
   passed through registers
 */
uint32 ArkMemLayout::ComputeStackSpaceRequirementForCall(StmtNode *stmt, int32 &aggCopySize, bool isIcall) {
  ASSERT(0, "ArkMemLayout ComputeStackSpaceRequirementForCall need to handle new parameter");
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

  for (; i < stmt->NumOpnds(); ++i) {
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
    parmlocator.LocateNextParm(ty, ploc);
    sizeOfArgsToStkpass = RoundUp(ploc.memoffset + ploc.memsize, SIZEOFPTR);
  }
  return sizeOfArgsToStkpass;
}

void ArkMemLayout::LayoutStackFrame(int32 &structCopySize, int32 &maxParmStackSize) {
  MIRSymbol *sym = nullptr;
  ParmLocator parmlocator(be);
  PLocInfo ploc;
  bool nostackpara = false;

  std::vector<MIRSymbol *> retDelays;
  for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
    sym = func->formalDefVec[i].formalSym;
    nostackpara = false;
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->formalDefVec[i].formalTyIdx);
    uint32_t ptyIdx = ty->tyIdx.GetIdx();
    parmlocator.LocateNextParm(ty, ploc);
    uint32 stindex = sym->GetStIndex();
    ArkSymbolAlloc *symloc = mem_allocator->GetMemPool()->New<ArkSymbolAlloc>();
    sym_alloc_table[stindex] = symloc;
    CG_ASSERT(symloc, "sym loc should have been defined");
    // passed on stack
    symloc->mem_segment = &seg__args_stkpassed;
    seg__args_stkpassed.size = RoundUp(seg__args_stkpassed.size, be.type_align_table[ptyIdx]);
    symloc->offset = seg__args_stkpassed.size;
    seg__args_stkpassed.size += be.type_size_table[ptyIdx];
    // We need it as dictated by the AArch64 ABI $5.4.2 C12
    seg__args_stkpassed.size = RoundUp(seg__args_stkpassed.size, SIZEOFPTR);
    if (cgfunc->cg->cgopt_.WithDwarf() && !nostackpara) {
      // for O0
      // LogInfo::MapleLogger() << "Add DIE for formal parameters" << endl
      //     << "    and remember them" << endl;
      cgfunc->AddDIESymbolLocation(sym, symloc);
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
    ArkSymbolAlloc *symloc = mem_allocator->GetMemPool()->New<ArkSymbolAlloc>();
    sym_alloc_table[stindex] = symloc;
    CG_ASSERT(!symloc->IsRegister(), "");

    if (sym->IsRefType()) {
      if (func->retRefSym.find(sym) != func->retRefSym.end()) {
        // try to put ret_ref at the end of seg_reflocals
        retDelays.push_back(sym);
        continue;
      }
      symloc->mem_segment = &seg_reflocals;
      seg_reflocals.size = RoundUp(seg_reflocals.size, be.type_align_table[tyIdx.GetIdx()]);
      symloc->offset = seg_reflocals.size;
      seg_reflocals.size += be.type_size_table[tyIdx.GetIdx()];

    } else {
      symloc->mem_segment = &seg_locals;
      seg_locals.size = RoundUp(seg_locals.size, be.type_align_table[tyIdx.GetIdx()]);
      if (func->stackallocVarMap.find(sym) != func->stackallocVarMap.end()) {
        symloc->offset = seg_locals.size;
        MapleMap<MIRSymbol *, uint32>::iterator it = func->stackallocVarMap.find(sym);
        seg_locals.size += it->second;
        // LogInfo::MapleLogger()<<"symbol "<<sym->GetName()<<" offset "<<symloc->offset<<" size "<<it->second<<endl;
      } else {
        if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->typeKind == kTypeClass) {
          // If this is a non-escaped object allocated on stack, we need to
          // manufacture an extra object header for RC to work correctly
          seg_locals.size += ArkRTSupport::kObjectHeaderSize;
        }
        symloc->offset = seg_locals.size;
        seg_locals.size += be.type_size_table[tyIdx.GetIdx()];
      }
    }

    if (cgfunc->cg->cgopt_.WithDwarf()) {
      // for O0
      // LogInfo::MapleLogger() << "Add DIE for formal parameters" << endl
      //     << "    and remember them" << endl;
      cgfunc->AddDIESymbolLocation(sym, symloc);
    }
  }

  // handle ret_ref sym now
  uint32 retsize = retDelays.size();
  for (uint32 i = 0; i < retsize; i++) {
    sym = retDelays[i];
    uint32 stindex = sym->GetStIndex();
    TyIdx tyIdx = sym->GetTyIdx();
    ArkSymbolAlloc *symloc = mem_allocator->GetMemPool()->New<ArkSymbolAlloc>();
    sym_alloc_table[stindex] = symloc;
    CG_ASSERT(!symloc->IsRegister(), "");

    CG_ASSERT(sym->IsRefType(), "");
    symloc->mem_segment = &seg_reflocals;
    seg_reflocals.size = RoundUp(seg_reflocals.size, be.type_align_table[tyIdx.GetIdx()]);
    symloc->offset = seg_reflocals.size;
    seg_reflocals.size += be.type_size_table[tyIdx.GetIdx()];

    if (cgfunc->cg->cgopt_.WithDwarf()) {
      // for O0
      // LogInfo::MapleLogger() << "Add DIE for formal parameters" << endl
      //     << "    and remember them" << endl;
      cgfunc->AddDIESymbolLocation(sym, symloc);
    }
  }

  ASSERT(0, "ArkMemLayout LayoutStackFrame need to handle new parameter");
  int32 aggCopySize = 0;
  seg__args_to_stkpass.size = FindLargestActualArea(aggCopySize);
  if (g->optim_level < 2) {
    AssignSpillLocationsToPseudoRegisters();
  } else {
    ArkCGFunc *arkCgfunc = static_cast<ArkCGFunc *>(cgfunc);
    arkCgfunc->SetJavaCatchRegno(cgfunc->New_V_Reg(kRegTyInt, 8));
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
    ArkSymbolAlloc *symloc = static_cast<ArkSymbolAlloc *>(sym_alloc_table[stindex]);
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
      static_cast<ArkCGFunc *>(cgfunc)->GetOrCreateMemOpnd(sym, 0, be.type_align_table[ptyIdx] * BITS_PER_BYTE);
    }
  }

  fixstacksize = RealStackFrameSize();
}

SymbolAlloc *ArkMemLayout::AssignLocationToSpillReg(regno_t vrnum) {
  ArkSymbolAlloc *symloc = mem_allocator->GetMemPool()->New<ArkSymbolAlloc>();
  symloc->mem_segment = &seg__spillreg;
  uint32_t regsize = cgfunc->GetVRegSize(vrnum);
  seg__spillreg.size = RoundUp(seg__spillreg.size, regsize);
  symloc->offset = seg__spillreg.size;
  seg__spillreg.size += regsize;
  spillreg_loc_map[vrnum] = symloc;
  return symloc;
}

int32 ArkMemLayout::GetadjustforRefloc() {
  int32 total = seg__args_regpassed.size + locals().size + GetSizeOfSpillReg();
  ArkCGFunc *aarchCgfunc = static_cast<ArkCGFunc *>(cgfunc);
  if (!aarchCgfunc->cg->UseFP() && !aarchCgfunc->HasVLAOrAlloca() && seg__args_to_stkpass.size > 0) {
    total += seg__args_to_stkpass.size;
  }
  return total;
}

int32 ArkMemLayout::StackFrameSize() {
  int32 total = seg__args_regpassed.size +
                GetSizeOfRefLocals() + locals().size + GetSizeOfSpillReg() + seg_lockobjslot.size;

  // if the function does not have VLA nor alloca,
  // we allocate space for arguments to stack-pass
  // in the call frame; otherwise, it has to be allocated for each call and reclaimed afterward.
  total += seg__args_to_stkpass.size;
  int finalsize = RoundUp(total, 16/*AARCH64_STACK_PTR_ALIGNMENT*/);
  if (finalsize - total >= 8) {
    unusedslot = true;
  } else {
    unusedslot = false;
  }

  return finalsize;
}

bool ArkMemLayout::GetUnusedSlot() {
  return unusedslot;
}

int32 ArkMemLayout::RealStackFrameSize() {
  int32 size = StackFrameSize();
  if (cgfunc->cg->AddStackGuard()) {
    size += 16/*AARCH64_STACK_PTR_ALIGNMENT*/;
  }
  return size;
}

int32 ArkMemLayout::GetReflocbaseLoc() {
  ArkCGFunc *aarchCgfunc = static_cast<ArkCGFunc *>(cgfunc);
  int32 beforesize = GetSizeOfLocals() + seg_lockobjslot.size;
  int32 argsToStkpassSize = SizeOfArgsToStackpass();
  if (!aarchCgfunc->cg->UseFP() && !aarchCgfunc->HasVLAOrAlloca() && argsToStkpassSize > 0) {
    beforesize += argsToStkpassSize;
  }
  int sizeofFplr = 2 * 8/*kIntregBytelen*/;
  if (!aarchCgfunc->ShouldSaveFPLR()) {
    return beforesize;
  }
  return beforesize + sizeofFplr;
}

}  // namespace maplebe
