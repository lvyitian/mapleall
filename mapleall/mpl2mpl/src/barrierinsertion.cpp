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

#include <set>

#include "barrierinsertion.h"
#include "vtable_impl.h"
#include "mir_function.h"
#include "mir_builder.h"
#include "global_tables.h"
#include "option.h"
#include "mpl_logging.h"
#include "opcode_info.h"

// BARDEBUG is turned on by passing -dump-phase=barrierinsertion to mpl2mpl
#define BARDEBUG (Options::dumpPhase.compare(PhaseName()) == 0)

// use this pass just for memory verification
#define MEMVERIFY

namespace maple {
CallNode *BarrierInsertion::RunFunction::CreateWriteRefVarCall(BaseNode *var, BaseNode *value) {
  MIRFunction *callee = builder->GetOrCreateFunction("MCC_WriteRefVar", (TyIdx)PTY_void);
  MapleVector<BaseNode *> funcArgs(func->module->memPoolAllocator.Adapter());
  funcArgs.push_back(var);
  funcArgs.push_back(value);
  return builder->CreateStmtCall(callee->puIdx, funcArgs);
}

CallNode *BarrierInsertion::RunFunction::CreateWriteRefFieldCall(BaseNode *obj, BaseNode *field, BaseNode *value) {
  MIRFunction *callee = builder->GetOrCreateFunction("MCC_WriteRefField", (TyIdx)PTY_void);
  MapleVector<BaseNode *> funcArgs(func->module->memPoolAllocator.Adapter());
  funcArgs.push_back(obj);
  funcArgs.push_back(field);
  funcArgs.push_back(value);
  return builder->CreateStmtCall(callee->puIdx, funcArgs);
}

CallNode *BarrierInsertion::RunFunction::CreateReleaseRefVarCall(BaseNode *var) {
  MIRFunction *callee = builder->GetOrCreateFunction("MCC_ReleaseRefVar", (TyIdx)PTY_void);
  MapleVector<BaseNode *> funcArgs(func->module->memPoolAllocator.Adapter());
  funcArgs.push_back(var);
  return builder->CreateStmtCall(callee->puIdx, funcArgs);
}

CallNode *BarrierInsertion::RunFunction::CreateMemCheckCall(BaseNode *var) {
  MIRFunction *callee = builder->GetOrCreateFunction("MCC_CheckObjMem", (TyIdx)PTY_void);
  MapleVector<BaseNode *> funcArgs(func->module->memPoolAllocator.Adapter());
  funcArgs.push_back(var);
  return builder->CreateStmtCall(callee->puIdx, funcArgs);
}

bool BarrierInsertion::RunFunction::SkipRhs(BaseNode *rhs) {
  // only consider reference
  if (rhs->primType != PTY_ref) {
    return true;
  }
  if (rhs->op == OP_conststr16) {
    return true;  // conststr
  }
  if (rhs->op == OP_intrinsicopwithtype) {
    IntrinsicopNode *innode = static_cast<IntrinsicopNode *>(rhs);
    if (innode->intrinsic == INTRN_JAVA_CONST_CLASS) {
      return true;
    }
  }
  return false;
}

// create a backup variable symbol using a counter inside its name
MIRSymbol *BarrierInsertion::RunFunction::NewBackupVariable(const char *suffix) {
  std::ostringstream oss;
  oss << "__backup" << backupVarCount << "_" << suffix << "__";
  backupVarCount++;
  std::string name = oss.str();
  MIRSymbol *backupSym = builder->GetOrCreateLocalDecl(name,
                     GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ref)));
  backupVarIndices.insert(backupSym->GetStIdx());
  return backupSym;
}

// go through each stmt in the block, memory check for all
// iread and iassign of PTY_ref type , i.e., read/write to heap objects
void BarrierInsertion::RunFunction::HandleBlock(BlockNode *block) {
  for (StmtNode *stmt = block->GetFirst(); stmt; stmt = stmt->GetNext()) {
    stmt = HandleStmt(stmt, block);
  }
}

/* This function exams one operand of a StmtNode and insert memory check calls
 * when it is iread of PTY_ref
 */
StmtNode *BarrierInsertion::RunFunction::CheckRefRead(BaseNode *opnd, StmtNode *stmt, BlockNode *block) {
  if (opnd && opnd->primType == PTY_ref) {
    if (opnd->op == OP_dread) {
      DreadNode *val = static_cast<DreadNode *>(opnd);
      MIRSymbol *sym = func->GetLocalOrGlobalSymbol((static_cast<DreadNode *>(val))->stIdx);
      if (sym && sym->IgnoreRC()) {
        return nullptr;
      }
      CallNode *checkStmt = CreateMemCheckCall(opnd);
      block->InsertBefore(stmt, checkStmt);
      return checkStmt;
    }
    if (opnd->op == OP_iread) {
      IreadNode *iread = static_cast<IreadNode *>(opnd);
      // after the reference value is read from heap, call for verification
      MIRPtrType *ptype = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iread->tyIdx));
      GStrIdx strIdx = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptype->pointedTyIdx)->nameStrIdx;

      static GStrIdx reflectClassNameIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(NameMangler::kJavaLangClassStr);
      static GStrIdx reflectMethodNameIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(NameMangler::kJavaLangReflectMethod);
      static GStrIdx reflectFieldNameIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(NameMangler::kJavaLangReflectField);
      if (strIdx == reflectClassNameIdx || strIdx == reflectMethodNameIdx || strIdx == reflectFieldNameIdx) {
        return nullptr;
      }
      CallNode *checkStmt = CreateMemCheckCall(opnd);
      block->InsertBefore(stmt, checkStmt);
      return checkStmt;
    } else if (opnd->op == OP_retype) {
      // do we need to check?
      return nullptr;
    }
  }
  return nullptr;
}

/* This function exams one StmtNode and carrys out necessary operations for
 * reference counting or memory verification.
 *
 * For barrier RC, below are the opcodes related and how they are handled:
 *
 * [dassign]
 * Condition: 1) rhs is reference (other than conststr16, global symbols)
 *            2) symbols not ignored
 *            3) Not a self-assignment (dassign v (dread v))
 * Handling:  1) Replace dassign with a call to MCC_WriteRefVar.
 *            2) If assigned to a formal parameter, make a backup variable in the
 *               prologue after processing all statements.
 *
 * [iassign]
 * Condition: 1) rhs is not addressof symbols with names starting "__vtab__" or
 *               "__itab__" (vtab, itab initialization
 * Handling:  1) Replace iassign with a call to CC_WriteRefField.
 *
 * [[|i|intrinsic]callassigned]
 * Condition: 1) Return type is ref.
 * Handling:  1) Assign return values to temporary variables, use
 *               MCC_ReleaseRefVar on the original return variable, and then
 *               dassign the tmp variable to the actual return var.
 *
 * [call]
 * Condition: 1) Return type is ref.
 * Handling:  1) Assign return values to temporary variables, and then call
 *               MCC_ReleaseRefVar on them.
 *
 * [return]
 * Assumption: 1) If the return type is ref, assume it is the result of a dread.
 * Handling:   1) If the return value is a reference, consider it as if it is
 *                assigned to.
 *             2) Call MCC_ReleaseRefVar on all local vars except un-assigned
 *                parameters and the return value. this is processed after
 *                procesing all statements.
 *
 * This function returns the last stmt it generated or visited.  HandleBlock
 * shall continue with the next statement after the return value, if any.
 */
StmtNode *BarrierInsertion::RunFunction::HandleStmt(StmtNode *stmt, BlockNode *block) {
  Opcode opcode = stmt->op;
#ifdef MEMVERIFY
  // ignore decref opcode due to missing initialization
  if (opcode != OP_decref && opcode != OP_intrinsiccall) {
    for (int32 i = (opcode == OP_iassign ? 1 : 0); i < stmt->NumOpnds(); i++) {
      CheckRefRead(stmt->Opnd(i), stmt, block);
    }
  }
#endif
  switch (opcode) {
    case OP_dassign: {
      DassignNode *dassign = static_cast<DassignNode *>(stmt);
      BaseNode *rhs = dassign->GetRhs();
      if (SkipRhs(rhs)) {
        break;
      }

      MIRSymbol *refsym = func->GetLocalOrGlobalSymbol(dassign->stIdx);
      CHECK_FATAL(refsym != nullptr, "Symbol is nullptr");

      if (refsym->IgnoreRC()) {
        break;
      }
      if (BARDEBUG) {
        dassign->Dump(func->module, 0);
      }

#ifdef BARRIERRC
      // Addr of lhs
      AddrofNode *lhs_addr = builder->CreateExprAddrof(dassign->fieldID, dassign->stIdx);
      // Call lib func
      CallNode *newstmt = CreateWriteRefVarCall(lhs_addr, rhs);
      block->ReplaceStmt1WithStmt2(stmt, newstmt);
      // Record formal parameters ever assigned to. Make back-ups for them (inc
      // them), and dec them before returning.
      if (refsym->storageClass == kScFormal) {
        assignedParams.insert(dassign->stIdx);
      }

      return newstmt;
#endif
#ifdef MEMVERIFY
      DreadNode *checkObjRead = builder->CreateExprDread(GlobalTables::GetTypeTable().GetRef(), dassign->fieldID, refsym);
      CallNode *checkStmt = CreateMemCheckCall(checkObjRead);
      block->InsertAfter(stmt, checkStmt);
      return checkStmt;
#endif
    }
    case OP_iassign: {
      IassignNode *iassign = static_cast<IassignNode *>(stmt);
      BaseNode *rhs = iassign->rhs;
      if (SkipRhs(rhs)) {
        break;
      }
      if (rhs->op == OP_addrof) {
        // ignore $__vtab__ such as in
        // iassign <* <$LPoint_3B>> 3 (dread ptr %_this, addrof ptr $__vtab__LPoint_3B)
        AddrofNode *anode = static_cast<AddrofNode *>(rhs);
        MIRSymbol *curSt = func->GetLocalOrGlobalSymbol(anode->stIdx);
        if (curSt->HasAddrOfValues()) {
          break;
        }
      }

      if (BARDEBUG) {
        iassign->Dump(func->module, 0);
      }
#ifdef BARRIERRC
      // Addr of lhs
      IaddrofNode *lhs_obj = builder->CreateExprIaddrof(PTY_ref, iassign->tyIdx, 0, iassign->addrExpr);
      IaddrofNode *lhs_field = builder->CreateExprIaddrof(PTY_ref, iassign->tyIdx, iassign->fieldID, iassign->addrExpr);
      // Call lib func
      CallNode *newstmt = CreateWriteRefFieldCall(lhs_obj, lhs_field, rhs);
      block->ReplaceStmt1WithStmt2(stmt, newstmt);
#endif
#ifdef MEMVERIFY
      // Call memory verification
      CallNode *newstmt = CreateMemCheckCall(rhs);
      block->InsertAfter(stmt, newstmt);
#endif
      return newstmt;
    }
#ifdef BARRIERRC
    case OP_callassigned: {
      CallNode *callassignednode = static_cast<CallNode *>(stmt);
      MIRFunction *callee = globaltable.GetFunctionFromPuidx(callassignednode->puIdx);
      MIRType *rettype = callee->GetReturnType();
      if (rettype->GetPrimType() == PTY_ref) {
        CHECK_FATAL(callassignednode->returnValues.size() == 1,
               "Callee is a single-return function, but callassigned has multiple return values");

        // Make a temporary variable to hold the return value
        MIRSymbol *tmp_var = NewBackupVariable("call_assign");

        // Make a temporary variable to hold the return value
        // Preserve old actual retvar
        CallReturnPair &pair = callassignednode->returnValues[0];
        MIRSymbol *actual_retvar = func->GetLocalOrGlobalSymbol(pair.first);
        FieldID fieldID = pair.second.GetFieldid();
        // Replace retvar with tmp var
        pair.first = tmp_var->GetStIdx();
        pair.second = RegFieldPair(0, 0);

        // Release the old actual return variable
        DreadNode *dread = builder->CreateDread(actual_retvar, PTY_ref);
        CallNode *release_call = CreateReleaseRefVarCall(dread);
        block->InsertAfter(callassignednode, release_call);

        // copy from tmp to the actual return variable
        DreadNode *rhs = builder->CreateDread(tmp_var, PTY_ref);
        DassignNode *dassign_copy = builder->CreateStmtDassign(actual_retvar, fieldID, rhs);
        block->InsertAfter(release_call, dassign_copy);

        return dassign_copy;
      }

      break;
    }
    case OP_icallassigned:
    case OP_intrinsiccallwithtype:
    case OP_intrinsiccallwithtypeassigned:
    case OP_intrinsiccallassigned: {
      break;
    }
    case OP_call: {
      CallNode *callnode = static_cast<CallNode *>(stmt);
      MIRFunction *callee = globaltable.GetFunctionFromPuidx(callnode->puIdx);
      MIRType *rettype = callee->GetReturnType();
      if (rettype->GetPrimType() == PTY_ref) {
        // Make a temporary variable to hold the return value
        MIRSymbol *tmp_var = NewBackupVariable("call_ignore");

        // Call-assign to the temporary varaible instead of the actual retval
        CallNode *callassignnode =
          builder->CreateStmtCallAssigned(callnode->puIdx, callnode->nOpnd, tmp_var, OP_callassigned);
        block->ReplaceStmt1WithStmt2(stmt, callassignnode);

        // Use MCC_ReleaseRefVar to release the reference.
        DreadNode *dread = builder->CreateDread(tmp_var, PTY_ref);
        CallNode *release_call = CreateReleaseRefVarCall(dread);
        block->InsertAfter(callassignnode, release_call);

        return release_call;
      }

      break;
    }
    case OP_virtualcall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_icall:
    case OP_virtualcallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
      (static_cast<CallNode *>(stmt))->Dump(func->module, 0);
      CHECK_FATAL(false, "intrinsic call type not supported yet by RC");
      break;
    case OP_intrinsiccall: // intrinsiccall JAVA_THROW_ARITHMETIC ()
      break;
    case OP_xintrinsiccallassigned: // intrinsicall for not-lowered interface calls
      break;
    case OP_xintrinsiccall:
      (static_cast<IntrinsiccallNode *>(stmt))->Dump(func->module, 0);
      CHECK_FATAL(false, "intrinsic call type not supported yet by RC");
      break;
    case OP_return: {
      NaryStmtNode *retnode = static_cast<NaryStmtNode *>(stmt);

      rets.insert(retnode);
      break;
    }
    case OP_if: {
      // going through thenPart and elsePart
      BlockNode *thenPart = static_cast<IfStmtNode *>(stmt)->thenPart;
      BlockNode *elsePart = static_cast<IfStmtNode *>(stmt)->elsePart;
      if (thenPart) {
        StmtNode *node = thenPart->GetFirst();
        while (node) {
          HandleStmt(node, thenPart);
          node = node->GetNext();
        }
      }
      if (elsePart) {
        StmtNode *node = elsePart->GetFirst();
        while (node) {
          HandleStmt(node, elsePart);
          node = node->GetNext();
        }
      }
      break;
    }
    case OP_dowhile: {
      BlockNode *body = static_cast<WhileStmtNode *>(stmt)->body;
      StmtNode *node = block->GetFirst();
      while (node) {
        HandleStmt(node, body);
        node = node->GetNext();
      }
      break;
    }
    case OP_doloop:       /* startExpr, condExpr, incrExpr, doBody */
    case OP_foreachelem:  // loopBody
      CHECK_FATAL(false, "this type of loop structure not supported yet by RC");
      break;
#endif
    default:
      break;
  }

  return stmt;
}

// this function handles local ref var initialization
// and formal ref parameters backup
void BarrierInsertion::RunFunction::InsertPrologue() {
  StmtNode *firstStmt = func->body->GetFirst();

  // Copy assigned formal parameters to backup variables using MCC_WriteRefVar.
  // This will inc those parameters, and those parameters will be dec-ed before
  // returning.
  for (StIdx asgnParam : assignedParams) {
    MIRSymbol *backupSym = NewBackupVariable("modified_param");

    // Clear the temporary variable
    ConstvalNode *rhsZero = builder->CreateIntConst(0, PTY_ref);
    DassignNode *dassignClear = builder->CreateStmtDassign(backupSym, 0, rhsZero);
    func->body->InsertBefore(firstStmt, dassignClear);

    // Backup the parameter
    AddrofNode *lhsAddr = builder->CreateExprAddrof(0, backupSym->GetStIdx());
    DreadNode *rhs = builder->CreateDread(func->symTab->GetSymbolFromStIdx(asgnParam.Idx()), PTY_ref);
    CallNode *newstmt = CreateWriteRefVarCall(lhsAddr, rhs);
    func->body->InsertBefore(firstStmt, newstmt);
  }

  // Initialize local variables (but not formal parameters) of ref type to 0.
  size_t bitsSize = func->symTab->GetSymbolTableSize();
  for (size_t i = 1; i < bitsSize; ++i) {
    // 0 is a reserved stIdx.
    MIRSymbol *sym = func->symTab->GetSymbolFromStIdx(i);
    CHECK_FATAL(sym != nullptr, "sym is nullptr");
    if (sym->storageClass == kScAuto && (GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->GetTyIdx())->GetPrimType() == PTY_ref)) {
      if (sym->IgnoreRC()) {
        continue;
      }
      if (backupVarIndices.find(sym->GetStIdx()) != backupVarIndices.end()) {
        continue;
      }
      if (BARDEBUG)
        LogInfo::MapleLogger() << "Local variable " << sym->GetName() << " set to null" << std::endl;
      func->body->InsertBefore(firstStmt, builder->CreateStmtDassign(sym, 0, builder->CreateIntConst(0, PTY_ref)));
    }
  }
}

void BarrierInsertion::RunFunction::HandleReturn(NaryStmtNode *retnode) {
  std::set<StIdx> retvalStidxs;
  for (int i = 0; i < retnode->NumOpnds(); i++) {
    BaseNode *val = retnode->Opnd(i);

    if (val->op == OP_constval && static_cast<ConstvalNode *>(val)->constVal->IsMagicNum()) {
      if (BARDEBUG) {
        LogInfo::MapleLogger() << "BARDEBUG: Ignoring magic number return" << endl;
      }
      continue;
    }

    if (val->primType != PTY_ref) {
      continue;
    }

    if (val->op != OP_dread) {
      if (val->op == OP_constval) {
        auto constvalNode = static_cast<ConstvalNode *>(val);
        MIRConst *con = constvalNode->constVal;
        if (con->kind == kConstInt) {
          auto intConst = static_cast<MIRIntConst *>(con);
          if (intConst->IsZero()) {
            // It is a nullptr.  Skip this return value.
            continue;
          }
        }
      }
#define __ASSUME_RETURN_REF_IS_DREAD__ 1
#if __ASSUME_RETURN_REF_IS_DREAD__
      CHECK_FATAL(
        false,
        "Found a return statement that returns a ref but is not from a dread.  Please enable the code below this line.");
#else
      /* Normalize
       *   (return (blah))
       * into
       *   (call MCC_WriteRefVar (__retval__ , (blah)))
       *   (return (dread __retval__))
       *
       * Backup variable for retval
       */
      MIRSymbol *backup_sym = NewBackupVariable("retval");

      // Clear the temporary variable
      ConstvalNode *rhs_zero = builder->CreateIntConst(0, PTY_ref);
      DassignNode *dassign_clear = builder->CreateStmtDassign(backup_sym, 0, rhs_zero);
      func->body->InsertBefore(first_stmt, dassign_clear);

      // Assign to a backup variable
      DassignNode *backup = builder->CreateStmtDassign(backup_sym, 0, val);
      func->body->InsertBefore(retnode, backup);

      // Replace the return operand with a dread
      DreadNode *new_dread = builder->CreateDread(backup_sym, PTY_ref);
      retnode->SetOpnd(new_dread, i);

      // Insert a MCC_WriteRefVar before the return
      AddrofNode *lhs_addr = builder->CreateExprAddrof(0, backup_sym->GetStIdx());
      CallNode *newstmt = CreateWriteRefVarCall(lhs_addr, val);
      func->body->InsertBefore(retnode, newstmt);
#endif
    } else {
      // It is a dassign.  Blacklist this symbol for this return.
      DreadNode *dreadNode = static_cast<DreadNode *>(val);
      MIRSymbol *sym = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
      CHECK_FATAL(sym != nullptr, "sym is null");

      if (sym->IgnoreRC()) {
        continue;
      }

      if (sym->storageClass == kScFormal && assignedParams.find(sym->GetStIdx()) == assignedParams.end()) {
        // If returning a parameter that is never assigned to,
        // insert a MCC_WriteRefVar to a temp var before returning.
        MIRSymbol *backupSym = NewBackupVariable("ret_prarm_unassigned");

        // Clear the temporary variable
        ConstvalNode *rhsZero = builder->CreateIntConst(0, PTY_ref);
        DassignNode *dassignClear = builder->CreateStmtDassign(backupSym, 0, rhsZero);
        func->body->InsertBefore(retnode, dassignClear);

        // Assign the parameter to the temporary variable
        AddrofNode *lhsAddr = builder->CreateExprAddrof(0, backupSym->GetStIdx());
        DreadNode *newDreadNode = builder->CreateDread(sym, PTY_ref);
        CallNode *newstmt = CreateWriteRefVarCall(lhsAddr, newDreadNode);
        func->body->InsertBefore(retnode, newstmt);
      }

      retvalStidxs.insert(dreadNode->stIdx);
    }
  }

  // now release all local reference except return values
  size_t size = func->symTab->GetSymbolTableSize();
  for (size_t i = 1; i < size; ++i) {
    // 0 is a reserved stIdx.
    MIRSymbol *localSym = func->symTab->GetSymbolFromStIdx(i);
    CHECK_FATAL(localSym != nullptr, "local_sym is nullptr");
    if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(localSym->GetTyIdx())->primType == PTY_ref &&
        (localSym->storageClass == kScAuto || assignedParams.find(localSym->GetStIdx()) != assignedParams.end())) {
      if (localSym->IgnoreRC()) {
        continue;
      }
      if (backupVarIndices.find(localSym->GetStIdx()) != backupVarIndices.end()) {
        continue;
      }
      if (retvalStidxs.find(localSym->GetStIdx()) != retvalStidxs.end()) {
        continue;
      }
      if (BARDEBUG)
        LogInfo::MapleLogger() << "Local variable " << localSym->GetName() << " going out of scope" << std::endl;

      // Attempt to release var if it is not the return value.
      DreadNode *localVar = builder->CreateDread(localSym, PTY_ref);
      func->body->InsertBefore(retnode, CreateReleaseRefVarCall(localVar));
    }
  }
}

void BarrierInsertion::RunFunction::Run() {
  CHECK_FATAL(func->body != nullptr, "Function has empty body");
  module->SetCurFunction(func);

  backupVarCount = 0;

  HandleBlock(func->body);

#ifdef BARRIERRC
  InsertPrologue();
  for (auto ret : rets) {
    HandleReturn(ret);
  }
#endif
}

void BarrierInsertion::EnsureLibraryFunction(MIRModule *module, const char *name, MIRType *rettype,
                                             const ArgVector &args) {
  MIRFunction *func = module->mirBuilder->CreateFunction(name, rettype, args);
  CHECK_FATAL(func != nullptr, "func is null in BarrierInsertion::EnsureLibraryFunction");
  func->body = nullptr;
  module->AddFunction(func);
}

void BarrierInsertion::EnsureLibraryFunctions(MIRModule *module) {
  {
    const char *name = "MCC_WriteRefVar";
    MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_void));
    ArgVector args(module->memPoolAllocator.Adapter());
    args.push_back(ArgPair("var", GlobalTables::GetTypeTable().GetVoidPtr()));
    args.push_back(ArgPair("value", GlobalTables::GetTypeTable().GetVoidPtr()));
    EnsureLibraryFunction(module, name, rettype, args);
  }
  {
    const char *name = "MCC_WriteRefField";
    MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_void));
    ArgVector args(module->memPoolAllocator.Adapter());
    args.push_back(ArgPair("obj", GlobalTables::GetTypeTable().GetVoidPtr()));
    args.push_back(ArgPair("field", GlobalTables::GetTypeTable().GetVoidPtr()));
    args.push_back(ArgPair("value", GlobalTables::GetTypeTable().GetVoidPtr()));
    EnsureLibraryFunction(module, name, rettype, args);
  }
  {
    const char *name = "MCC_ReleaseRefVar";
    MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_void));
    ArgVector args(module->memPoolAllocator.Adapter());
    args.push_back(ArgPair("var", GlobalTables::GetTypeTable().GetVoidPtr()));
    EnsureLibraryFunction(module, name, rettype, args);
  }
  {
    const char *name = "MCC_CheckObjMem";
    MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_void));
    ArgVector args(module->memPoolAllocator.Adapter());
    args.push_back(ArgPair("var", GlobalTables::GetTypeTable().GetVoidPtr()));
    EnsureLibraryFunction(module, name, rettype, args);
  }
}

AnalysisResult *BarrierInsertion::Run(MIRModule *module, ModuleResultMgr *mrm) {
#ifndef MEMVERIFY
  return 0;
#endif
  for (MIRFunction *func : module->functionList) {
    if (!PhaseName().empty()) {
      if (BARDEBUG) {
        LogInfo::MapleLogger() << " Handling function " << func->GetName() << std::endl;
      }
      if (Options::dumpFunc.compare(func->GetName()) == 0) {
        LogInfo::MapleLogger() << "Function found" << std::endl;
      }
      if (func->body == nullptr) {
        continue;
      }
      RunFunction runFunction(this, module, func);
      runFunction.Run();
    }
  }
  return nullptr;
}

}  // namespace maple
