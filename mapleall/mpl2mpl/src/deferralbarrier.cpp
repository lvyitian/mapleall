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

#include "deferralbarrier.h"
#include "vtable_impl.h"
#include "mir_function.h"
#include "mir_builder.h"
#include "global_tables.h"
#include "option.h"
#include "mpl_logging.h"
#include "opcode_info.h"

// RCDEBUG is turned on by passing -dump-phase=deferralBarrier to mpl2mpl
#define RCDEBUG (Options::dumpPhase.compare(PhaseName()) == 0)

/* Refer to the comments in mapleir/include/vtable_impl.h */

#define BARRIER_FUNC_NAME "MCC_WriteRefField"

/*
 * For deferral RC, write barrier or reference couting operation
 * for local variables is no longer needed, so we only insert write
 * barriers for object reference field updates.
 *
 * [iassign]
 * If:    rhs is not addressof symbols with names starting "__vtab__" or
 *        "__itab__" (vtab, itab initialization
 * Then:  Replace iassign with a call to CC_WriteRefField.
 *
 *
 * This function returns the last stmt it generated or visited.  HandleBlock
 * shall continue with the next statement after the return value, if any.
 */
namespace maple {
StmtNode *DeferralBarrier::RunFunction::HandleStmt(StmtNode *stmt, BlockNode *block) {
  switch (stmt->op) {
    case OP_incref:
    case OP_decref: {
      // incref/decref is removed if we use deferral RC.
      block->RemoveStmt(stmt);
      break;
    }

    case OP_iassign: {
      IassignNode *iassign = static_cast<IassignNode *>(stmt);
      MIRType *basetype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(iassign->tyIdx);
      ASSERT(dynamic_cast<MIRPtrType *>(basetype), "unexpected type");
      MIRType *ptype = static_cast<MIRPtrType *>(basetype)->GetPointedType();
      MIRStructType *structType = dynamic_cast<MIRStructType *>(ptype);
      if (!structType) {
        if (dynamic_cast<MIRPtrType *>(ptype)) {
          if (static_cast<MIRPtrType *>(ptype)->primType != PTY_ref) {
            break;
          }
        } else {
          break;
        }
      } else {
        if (iassign->fieldID == 0) {
          break;  // struct assign is not ref
        }
        TyIdx ftyidx = structType->TraverseToField(iassign->fieldID).second.first;
        if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx)->primType != PTY_ref) {
          break;
        }
      }
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

      // Addr of lhs
      IaddrofNode *lhsObj = builder->CreateExprIaddrof(PTY_ref, iassign->tyIdx, 0, iassign->addrExpr);
      IaddrofNode *lhsField = builder->CreateExprIaddrof(PTY_ref, iassign->tyIdx, iassign->fieldID, iassign->addrExpr);

      // Replace iassign with write barrier function call.
      CallNode *newstmt = CreateWriteRefFieldCall(lhsObj, lhsField, rhs);
      block->ReplaceStmt1WithStmt2(stmt, newstmt);

      return newstmt;
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

    case OP_polymorphiccallassigned:
      (static_cast<CallNode *>(stmt))->Dump(func->module, 0);
      CHECK_FATAL(false, "intrinsic call type not supported yet by RC");
      break;

    case OP_xintrinsiccall:
      (static_cast<IntrinsiccallNode *>(stmt))->Dump(func->module, 0);
      CHECK_FATAL(false, "intrinsic call type not supported yet by RC");
      break;

    case OP_doloop:       /* startExpr, condExpr, incrExpr, doBody */
    case OP_foreachelem:  // loopBody
      CHECK_FATAL(false, "this type of loop structure not supported yet by RC");
      break;

    default:
      break;
  }  // switch

  return stmt;
}

/*
 * Creates write barrier CallNode.
 */
CallNode *DeferralBarrier::RunFunction::CreateWriteRefFieldCall(BaseNode *obj, BaseNode *field, BaseNode *value) {
  MIRFunction *callee = builder->GetOrCreateFunction(BARRIER_FUNC_NAME, (TyIdx)PTY_void);
  MapleVector<BaseNode *> funcArgs(func->module->memPoolAllocator.Adapter());
  funcArgs.push_back(obj);
  funcArgs.push_back(field);
  funcArgs.push_back(value);
  return builder->CreateStmtCall(callee->puIdx, funcArgs);
}

/*
 * Returns true if RHS is not an object reference.
 * (copied from rcinsertion.cpp)
 */
bool DeferralBarrier::RunFunction::SkipRhs(BaseNode *rhs) {
  // only consider reference
  // Update: below check is moved to lhs
  // if (rhs->primType != PTY_ref) return true;
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

/*
 * Handles one code block for barrier insertion.
 */
void DeferralBarrier::RunFunction::HandleBlock(BlockNode *block) {
  for (StmtNode *stmt = block->GetFirst(); stmt; stmt = stmt->GetNext()) {
    stmt = HandleStmt(stmt, block);
  }
}

/*
 * Handles one MIRFunction for barrier insertion.
 */
void DeferralBarrier::RunFunction::Run() {
  CHECK_FATAL(func->body != nullptr, "Function has empty body");
  module->SetCurFunction(func);

  // add 'localrefvar' attribute to local ref variables,
  // so that cg can generate code to initialize them to null.
  // (copied from rcinsertion.cpp)
  size_t bitsSize = func->symTab->GetSymbolTableSize();
  for (size_t i = 0; i < bitsSize; ++i) {
    MIRSymbol *sym = func->symTab->GetSymbolFromStIdx(i);
    if (sym && sym->storageClass == kScAuto && (GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->GetTyIdx())->primType == PTY_ref)) {
      if (IgnoreSymbol(sym)) {
        continue;
      }
      sym->SetLocalRefVar();
      sym->SetAttr(ATTR_localrefvar);
    }
  }

  // handle function body.
  HandleBlock(func->body);
}

// copied from rcinsertion.cpp
bool DeferralBarrier::IgnoreSymbol(MIRSymbol *symbol) {
  if (!symbol || symbol->isDeleted) {
    return true;
  }

  const std::string &name = symbol->GetName();
  // ignore %current_vptr, %vtabptr, %itabptr, %funcptr, %env_ptr
  if (name == "current_vptr" || name == "vtabptr" || name == "itabptr" || name == "funcptr" || name == "env_ptr" ||
      name == "retvar_stubfunc" || name == "_dummy_stub_object_retval") {
    return true;
  }

  if (symbol->IsReflectionInfo() || symbol->IsRegJNITab()) {
    return true;
  }

  MIRType *type = symbol->GetType();
  // only consider reference
  if (type->GetPrimType() == PTY_ref) {
    if ((type->GetKind() == kTypeScalar) && (name != "__mapleRC__")) {
      return true;
    }
    // ignore ptr to types Ljava_2Flang_2FClass_3B,
    // Ljava_2Flang_2Freflect_2FMethod_3B, and Ljava_2Flang_2Freflect_2FField_3B
    MIRPtrType *ptype = static_cast<MIRPtrType *>(type);
    GStrIdx strIdx = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptype->pointedTyIdx)->nameStrIdx;
    // LogInfo::MapleLogger() << globaltable.GetStringFromGstridx(strIdx) << std::endl;
    if (strIdx == reflectClassNameIdx || strIdx == reflectMethodNameIdx || strIdx == reflectFieldNameIdx) {
      return true;
    }
  }

  return false;
}

/*
 * Declare write barrier function in IR.
 */
void DeferralBarrier::DeclareBarrierFunction(MIRModule *module) {
  MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_void));
  ArgVector args(module->memPoolAllocator.Adapter());
  args.push_back(ArgPair("obj", GlobalTables::GetTypeTable().GetVoidPtr()));
  args.push_back(ArgPair("field", GlobalTables::GetTypeTable().GetVoidPtr()));
  args.push_back(ArgPair("value", GlobalTables::GetTypeTable().GetVoidPtr()));
  MIRFunction *func = module->mirBuilder->CreateFunction(BARRIER_FUNC_NAME, rettype, args);
  CHECK_FATAL(func != nullptr, "func is null in DeferralBarrier::DeclareBarrierFunction");
  func->body = nullptr;
  module->AddFunction(func);
}

/*
 * Starts write barrier insertion for deferral RC.
 */
AnalysisResult *DeferralBarrier::Run(MIRModule *module, ModuleResultMgr *mrm) {
  DeclareBarrierFunction(module);

  for (MIRFunction *func : module->functionList) {
    if (RCDEBUG) {
      LogInfo::MapleLogger() << " Handling function " << func->GetName() << std::endl;
    }
    if (Options::dumpFunc.compare(func->GetName()) == 0) {
      LogInfo::MapleLogger() << "Function found" << std::endl;
    }
    if (func->body != nullptr) {
      RunFunction runFunction(this, module, func);
      runFunction.Run();
    }
  }

  return nullptr;
}

}  // namespace maple
