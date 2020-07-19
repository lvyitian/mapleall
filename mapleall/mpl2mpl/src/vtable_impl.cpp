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

#include "vtable_impl.h"
#include "vtable_analysis.h"
#include "itab_util.h"

namespace maple {

// This phase is mainly to lower interfacecall into icall

VtableImpl::VtableImpl(MIRModule *mod, KlassHierarchy *kh, bool dump) : FuncOptimizeImpl(mod, kh, dump) {
#ifdef USE_32BIT_REF
  mccItabFunc = builder->GetOrCreateFunction("MCC_getFuncPtrFromItab", TyIdx(PTY_ptr));
#else
  mccItabFunc = builder->GetOrCreateFunction("MCC_getFuncPtrFromItabSecondHash64", TyIdx(PTY_ptr));
#endif
  mccItabFunc->SetAttr(FUNCATTR_nosideeffect);
}

void VtableImpl::ProcessFunc(MIRFunction *func) {
  if (func->IsEmpty()) {
    return;
  }
  SetCurrentFunction(func);

  StmtNode *stmt = func->body->GetFirst();
  StmtNode *next = nullptr;
  while (stmt) {
    next = stmt->GetNext();
    Opcode opcode = stmt->op;
    switch (opcode) {
      case OP_callassigned: {
        CallNode *cnode = static_cast<CallNode *>(stmt);
        MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(cnode->puIdx);
        break;
      }
      case OP_regassign: {
        RegassignNode *regassign = static_cast<RegassignNode *>(stmt);
        BaseNode *rhs = regassign->uOpnd;
        if (rhs->op == maple::OP_resolveinterfacefunc) {
          ReplaceResolveInterface(stmt, static_cast<ResolveFuncNode *>(rhs));
        }
        break;
      }
      case OP_interfaceicallassigned:
      case OP_virtualicallassigned: {
        CallNode *callnode = static_cast<CallNode *>(stmt);
        MIRFunction *callee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callnode->puIdx);

        IcallNode *icallnode = builder->GetCurrentFuncCodeMp()->New<IcallNode>(
            builder->GetCurrentFuncCodeMpAllocator(), OP_icallassigned);
        icallnode->returnValues = callnode->returnValues;
        icallnode->retTyIdx = callee->GetReturnTyIdx();
        icallnode->srcPosition = callnode->srcPosition;
        icallnode->nOpnd.resize(callnode->nOpnd.size());
        icallnode->numOpnds = icallnode->nOpnd.size();
        for (uint32 i = 0; i < callnode->nOpnd.size(); i++) {
          icallnode->SetOpnd(callnode->nOpnd[i]->CloneTree(builder->GetCurrentFuncCodeMpAllocator()), i);
        }
        currFunc->body->ReplaceStmt1WithStmt2(stmt, icallnode);
        stmt = icallnode;
        // Fall-through
      }
      case OP_icallassigned: {
        IcallNode *icall = static_cast<IcallNode *>(stmt);
        BaseNode *firstParm = icall->nOpnd[0];
        if (firstParm->op == maple::OP_resolveinterfacefunc) {
          ReplaceResolveInterface(stmt, static_cast<ResolveFuncNode *>(firstParm));
        }
        break;
      }
      case OP_virtualcall:
      case OP_virtualcallassigned: {
        CHECK_FATAL(false, "VtableImpl::ProcessFunc does not expect to see virtucalcall");
        break;
      }
      case OP_interfacecall:
      case OP_interfacecallassigned: {
        CHECK_FATAL(false, "VtableImpl::ProcessFunc does not expect to see interfacecall");
        break;
      }
      default:
        break;
    }
    ASSERT(stmt->GetNext() == next, "");
    stmt = next;
  }

  if (trace) {
    func->Dump(false);
  }
}

void VtableImpl::ReplaceResolveInterface(StmtNode *stmt, const ResolveFuncNode *resolveNode) {
  std::string signature = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(resolveNode->puIdx)->GetBaseFuncNameWithType();
  uint64 hashCode = GetHashIndex(signature.c_str());
  PregIdx pregItabAddr = currFunc->pregTab->CreatePreg(PTY_ptr);
  RegassignNode *itabAddrAssign = builder->CreateStmtRegassign(PTY_ptr, pregItabAddr, resolveNode->GetTabBaseAddr());
  currFunc->body->InsertBefore(stmt, itabAddrAssign);
  // read funcvalue
  MIRType *compactPtrType = GlobalTables::GetTypeTable().GetCompactPtr();
  PrimType compactPtrPrim = compactPtrType->GetPrimType();
  BaseNode *offsetNode = builder->CreateIntConst(hashCode * TAB_ENTRY_SIZE, PTY_u32);
  BaseNode *addrNode = builder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetPtr(),
      builder->CreateExprRegread(PTY_ptr, pregItabAddr), offsetNode);
  BaseNode *readFuncPtr = builder->CreateExprIread(compactPtrType,
      GlobalTables::GetTypeTable().GetOrCreatePointerType(compactPtrType), 0, addrNode);
  PregIdx pregFuncPtr = currFunc->pregTab->CreatePreg(compactPtrPrim);
  RegassignNode *funcPtrAssign = builder->CreateStmtRegassign(compactPtrPrim, pregFuncPtr, readFuncPtr);
  currFunc->body->InsertBefore(stmt, funcPtrAssign);
  // In case not found in the fast path, fall to the slow path
  uint64 secondhashCode = GetSecondHashIndex(signature.c_str());
  MapleVector<BaseNode *> opnds(builder->GetCurrentFuncCodeMpAllocator()->Adapter());
  opnds.push_back(builder->CreateExprRegread(PTY_ptr, pregItabAddr));
  opnds.push_back(builder->CreateIntConst(secondhashCode, PTY_u64));
  UStrIdx strIdx = GlobalTables::GetUStrTable().GetOrCreateStrIdxFromName(signature);
  ConststrNode *signatureNode = builder->GetCurrentFuncCodeMp()->New<ConststrNode>(strIdx);
  signatureNode->primType = PTY_ptr;
  opnds.push_back(signatureNode);
  StmtNode *mccCallStmt = builder->CreateStmtCallRegassigned(mccItabFunc->puIdx, opnds, pregFuncPtr, OP_callassigned);

  BaseNode *checkExpr = builder->CreateExprCompare(OP_eq, GlobalTables::GetTypeTable().GetUInt1(), compactPtrType,
                                                    builder->CreateExprRegread(compactPtrPrim, pregFuncPtr),
                                                    builder->CreateIntConst(0, compactPtrPrim));
  IfStmtNode *ifStmt = static_cast<IfStmtNode *>(builder->CreateStmtIf(checkExpr));
  ifStmt->thenPart->AddStatement(mccCallStmt);
  currFunc->body->InsertBefore(stmt, ifStmt);

  if (stmt->op == OP_regassign) {
    RegassignNode *regAssign = static_cast<RegassignNode *>(stmt);
    regAssign->uOpnd = builder->CreateExprRegread(compactPtrPrim, pregFuncPtr);
  } else {
    IcallNode *icall = static_cast<IcallNode *>(stmt);
    const int nopndSize = icall->nOpnd.size();
    CHECK_FATAL(nopndSize > 0, "container check");
    icall->nOpnd[0] = builder->CreateExprRegread(compactPtrPrim, pregFuncPtr);
  }
}

}  // namespace maple
