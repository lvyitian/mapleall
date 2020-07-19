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

#include <iostream>
#include <algorithm>
#include "simplify.h"
#include "reflection_analysis.h"

namespace maple {

// This phase focus on specific funcs' optimization and is highly coupled with java.
bool Simplify::doclinitopt = false;

extern void DoDevirtual(Klass *klass, KlassHierarchy *klassHierarchy);

#ifdef USE_32BIT_REF
#define STRINGBASEOBJSIZE (16)  // shadow(4)+monitor(4)+count(4)+hash(4)
#else
#define STRINGBASEOBJSIZE (20)  // shadow(8)+monitor(4)+count(4)+hash(4)
#endif                          // USE_32BIT_REF

Simplify::Simplify(MIRModule *mod, KlassHierarchy *kh, bool dump) : FuncOptimizeImpl(mod, kh, dump) {
  localMp = mempoolctrler.NewMemPool("SimplifyTmp");
  inliner = new MInline(*mod, localMp, kh);
  CHECK_FATAL(inliner, "Failed to create a MInline in Simplify::Simplify");
}

Simplify::~Simplify() {
  delete inliner;
  mempoolctrler.DeleteMemPool(localMp);
}

bool Simplify::IsImplicitNullCheckModule() const {
  return (kImplicitNullCheckModule.find(module->fileName) != kImplicitNullCheckModule.end());
}

void Simplify::SimplifyStrCharAt(MIRFunction *func, StmtNode *stmt, MIRSymbol *retSt) {
  if (retSt == nullptr) {
    return;
  }
  CallNode *cnode = static_cast<CallNode *>(stmt);
  ASSERT(cnode->NumOpnds() == 2, "Two parameters for this function.");
  BaseNode *jstrOpnd = cnode->Opnd(0);
  BaseNode *indexOpnd = cnode->Opnd(1);

  if (jstrOpnd->op != OP_dread) {
    return;
  }

  const string &name = func->GetName();
  AddrofNode *jstrDread = static_cast<AddrofNode *>(jstrOpnd);
  MIRSymbol *jstrSt = currFunc->GetLocalOrGlobalSymbol(jstrDread->stIdx);

  BaseNode *baseVal = builder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetPtr(), jstrOpnd,
                                                 builder->CreateIntConst(STRINGBASEOBJSIZE, PTY_ref));
  MapleVector<BaseNode *> opnds(builder->GetCurrentFuncCodeMpAllocator()->Adapter());
  opnds.push_back(baseVal);
  opnds.push_back(indexOpnd);
  uint32_t arraySize;
  arraySize = 0;
  MIRArrayType *arraytype =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(GlobalTables::GetTypeTable().GetUInt16(), arraySize);
  ArrayNode *arrayexpr = builder->CreateExprArray(arraytype, opnds);
  arrayexpr->boundsCheck = false;

  MIRArrayType *arraytype8 =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(GlobalTables::GetTypeTable().GetUInt8(), arraySize);
  ArrayNode *arrayexpr8 = builder->CreateExprArray(arraytype8, opnds);
  arrayexpr8->boundsCheck = false;

  MIRPtrType *jstrPtype = static_cast<MIRPtrType *>(jstrSt->GetType());
  MIRType *jstrType = jstrPtype->GetPointedType();
  FieldID fldid = builder->GetStructFieldIdFromFieldNameParentFirst(jstrType, "count");
  BaseNode *ireadExpr = builder->CreateExprIread(GlobalTables::GetTypeTable().GetInt32(), jstrPtype, fldid, jstrDread);

  BaseNode *jstrIscompress =
      builder->CreateExprBinary(OP_band, GlobalTables::GetTypeTable().GetInt32(), ireadExpr, builder->CreateIntConst(1, PTY_i32));
  BaseNode *iscompressCond = builder->CreateExprCompare(OP_eq, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetUInt32(),
                                                          builder->CreateIntConst(0, PTY_i32), jstrIscompress);
  IfStmtNode *ifIscompressStmt = static_cast<IfStmtNode *>(builder->CreateStmtIfThenElse(iscompressCond));

  BaseNode *iread2ExprUint16 = builder->CreateExprIread(
      GlobalTables::GetTypeTable().GetUInt16(), GlobalTables::GetTypeTable().GetOrCreatePointerType(GlobalTables::GetTypeTable().GetUInt16()), 0, arrayexpr);
  DassignNode *assignCharatUint16 = builder->CreateStmtDassign(retSt, 0, iread2ExprUint16);

  BaseNode *iread2ExprUint8 = builder->CreateExprIread(
      GlobalTables::GetTypeTable().GetUInt8(), GlobalTables::GetTypeTable().GetOrCreatePointerType(GlobalTables::GetTypeTable().GetUInt8()), 0, arrayexpr8);
  MIRType *totype = GlobalTables::GetTypeTable().GetPrimType(PTY_u16);
  MIRType *srctype = GlobalTables::GetTypeTable().GetPrimType(PTY_u8);
  BaseNode *charatUint8 = builder->CreateExprTypeCvt(OP_cvt, totype, srctype, iread2ExprUint8);
  DassignNode *assignCharatUint8 = builder->CreateStmtDassign(retSt, 0, charatUint8);

  ifIscompressStmt->thenPart->AddStatement(assignCharatUint16);

  ifIscompressStmt->elsePart->AddStatement(assignCharatUint8);

  // some callers don't need EH judgement.
  BaseNode *jstrLength =
      builder->CreateExprBinary(OP_lshr, GlobalTables::GetTypeTable().GetInt32(), ireadExpr, builder->CreateIntConst(1, PTY_i32));
  BaseNode *cond2 =
      builder->CreateExprCompare(OP_ge, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetUInt32(), indexOpnd, jstrLength);
  IfStmtNode *ifStmt2 = static_cast<IfStmtNode *>(builder->CreateStmtIfThenElse(cond2));

  // throw: call MCCThrowStringIndexOutOfBoundsException that will never return
  MapleVector<BaseNode *> alloccallargs(module->memPoolAllocator.Adapter());
  IntrinsiccallNode *throwcall = builder->CreateStmtIntrinsicCall(INTRN_MCCThrowStringIndexOutOfBoundsException, alloccallargs);
  ifStmt2->thenPart->AddStatement(throwcall);
  ifStmt2->elsePart->AddStatement(ifIscompressStmt);
  func->body->ReplaceStmt1WithStmt2(stmt, ifStmt2);
}

BaseNode *Simplify::GetStringClassInfo(BaseNode *jstrOpnd) {
  MIRType *jstrType =
      GlobalTables::GetTypeTable().GetOrCreateClassType(NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangStringStr).c_str(), module);
  MIRPtrType *jstrPtype = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetOrCreatePointerType(jstrType));
  FieldID fldid = builder->GetStructFieldIdFromFieldNameParentFirst(jstrType, NameMangler::kShadowClassName);
  BaseNode *ireadExpr = builder->CreateExprIread(GlobalTables::GetTypeTable().GetPtr(), jstrPtype, fldid, jstrOpnd);
  return ireadExpr;
}

BaseNode *Simplify::GetStringCount(BaseNode *jstrOpnd) {
  MIRType *jstrType =
      GlobalTables::GetTypeTable().GetOrCreateClassType(NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangStringStr).c_str(), module);
  MIRPtrType *jstrPtype = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetOrCreatePointerType(jstrType));
  FieldID fldid = builder->GetStructFieldIdFromFieldNameParentFirst(jstrType, "count");
  BaseNode *ireadExpr = builder->CreateExprIread(GlobalTables::GetTypeTable().GetInt32(), jstrPtype, fldid, jstrOpnd);
  return ireadExpr;
}

void Simplify::SimplifyStrEquals(MIRFunction *func, StmtNode *stmt, MIRSymbol *retSt) {
  if (retSt == nullptr) {
    return;
  }
  CallNode *cnode = static_cast<CallNode *>(stmt);
  ASSERT(cnode->NumOpnds() == 2, "Two parameters for this function.");
  BaseNode *jstrOpnd = cnode->Opnd(0);

  if (jstrOpnd->op != OP_dread) {
    return;
  }
#ifdef USE_32BIT_REF
  MIRFunction *strequal_func = builder->GetOrCreateFunction(STRING_EQUALS_STR, (TyIdx)(PTY_u1));

  cnode->puIdx = strequal_func->puIdx;
  cnode->op = OP_callassigned;
#else
  DassignNode *assignFalse =
    builder->CreateStmtDassign(retSt, 0, builder->CreateIntConst(0, retSt->GetType()->GetPrimType()));
  func->body->ReplaceStmt1WithStmt2(stmt, assignFalse);
  BaseNode *compareToOpnd = cnode->Opnd(1);
  StmtNode *body = SimplifyStringEquals(func, jstrOpnd, compareToOpnd, retSt);
  func->body->InsertAfter(assignFalse, body);
#endif
}

StmtNode *Simplify::SimplifyStringEquals(MIRFunction *func, BaseNode *baseJstrOpnd, BaseNode *compareToOpnd,
                                         MIRSymbol *retSt) {
  BaseNode *nullptrConst = builder->CreateIntConst(0, PTY_ref);
  BaseNode *cond1 =
      builder->CreateExprCompare(OP_ne, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetRef(), compareToOpnd, nullptrConst);
  IfStmtNode *ifStmt1 = static_cast<IfStmtNode *>(builder->CreateStmtIf(cond1));

  // If 2
  BaseNode *cond2 =
      builder->CreateExprCompare(OP_eq, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetRef(), baseJstrOpnd, compareToOpnd);
  IfStmtNode *ifStmt2 = static_cast<IfStmtNode *>(builder->CreateStmtIfThenElse(cond2));
  ifStmt1->thenPart->AddStatement(ifStmt2);

  DassignNode *assignTrue =
    builder->CreateStmtDassign(retSt, 0, builder->CreateIntConst(1, retSt->GetType()->GetPrimType()));
  ifStmt2->thenPart->AddStatement(assignTrue);

  // else 2 & If 3

  BaseNode *baseJstrClassinfo = GetStringClassInfo(baseJstrOpnd);
  BaseNode *compareToClassinfo = GetStringClassInfo(compareToOpnd);

  BaseNode *cond3 = builder->CreateExprCompare(OP_eq, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetRef(), baseJstrClassinfo,
                                                compareToClassinfo);
  IfStmtNode *ifStmt3 = static_cast<IfStmtNode *>(builder->CreateStmtIf(cond3));
  ifStmt2->elsePart->AddStatement(ifStmt3);

  BaseNode *baseJstrLen = GetStringCount(baseJstrOpnd);
  BaseNode *compareToLen = GetStringCount(compareToOpnd);

  MIRSymbol *tmpLenSym = builder->GetOrCreateLocalDecl(TEMP_STRING_LENGTH_STR, GlobalTables::GetTypeTable().GetInt32());
  DassignNode *dassignJstrLen = builder->CreateStmtDassign(tmpLenSym, 0, baseJstrLen);
  ifStmt3->thenPart->AddStatement(dassignJstrLen);

  AddrofNode *dreadJstrLen = builder->CreateDread(tmpLenSym, PTY_i32);

  BaseNode *thisJstrLen =
      builder->CreateExprBinary(OP_lshr, GlobalTables::GetTypeTable().GetInt32(), dreadJstrLen, builder->CreateIntConst(1, PTY_i32));
  BaseNode *annotherJstrLen =
      builder->CreateExprBinary(OP_lshr, GlobalTables::GetTypeTable().GetInt32(), compareToLen, builder->CreateIntConst(1, PTY_i32));
  BaseNode *condCmpStrLen =
      builder->CreateExprCompare(OP_eq, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetInt32(), annotherJstrLen, thisJstrLen);
  IfStmtNode *ifStmtCmpStrLen = static_cast<IfStmtNode *>(builder->CreateStmtIf(condCmpStrLen));
  ifStmt3->thenPart->AddStatement(ifStmtCmpStrLen);

  BaseNode *condIsTheSameType =
      builder->CreateExprCompare(OP_eq, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetInt32(), dreadJstrLen, compareToLen);
  IfStmtNode *ifStmtIsTheSameType = static_cast<IfStmtNode *>(builder->CreateStmtIfThenElse(condIsTheSameType));
  ifStmtCmpStrLen->thenPart->AddStatement(ifStmtIsTheSameType);

  // if 4
  MIRSymbol *memcmplenSym = builder->GetOrCreateLocalDecl(TEMP_MEMCMPLEN_STR, GlobalTables::GetTypeTable().GetInt32());
  BaseNode *compressLen =
      builder->CreateExprBinary(OP_lshr, GlobalTables::GetTypeTable().GetInt32(), dreadJstrLen, builder->CreateIntConst(1, PTY_i32));
  DassignNode *memCmpLenCompress = builder->CreateStmtDassign(memcmplenSym, 0, compressLen);
  DassignNode *memCmpLen = builder->CreateStmtDassign(memcmplenSym, 0, dreadJstrLen);

  MIRFunction *memcmpFunc = builder->GetOrCreateFunction(MEMCMPMPL_STR, (TyIdx)(PTY_i32));

  BaseNode *baseJstrArray = builder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetPtr(), baseJstrOpnd,
                                                       builder->CreateIntConst(STRINGBASEOBJSIZE, PTY_ref));
  BaseNode *compareToJstrArray = builder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetPtr(), compareToOpnd,
                                                            builder->CreateIntConst(STRINGBASEOBJSIZE, PTY_ref));

  AddrofNode *memcpylen = builder->CreateDread(memcmplenSym, PTY_i32);
  MapleVector<BaseNode *> callargs(module->memPoolAllocator.Adapter());
  callargs.push_back(baseJstrArray);
  callargs.push_back(compareToJstrArray);
  callargs.push_back(memcpylen);

  MIRSymbol *tmpSym = builder->GetOrCreateLocalDecl(TEMP_STRING_EUQAL_RESULT_STR, GlobalTables::GetTypeTable().GetUInt1());

  CallNode *memcmpcallassign = builder->CreateStmtCallAssigned(memcmpFunc->puIdx, callargs, tmpSym, OP_callassigned);

  BaseNode *thisJstrCompressflag =
      builder->CreateExprBinary(OP_band, GlobalTables::GetTypeTable().GetInt32(), dreadJstrLen, builder->CreateIntConst(1, PTY_i32));
  BaseNode *condIsAllCompress = builder->CreateExprCompare(
      OP_eq, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetInt32(),
      thisJstrCompressflag, builder->CreateIntConst(1, PTY_i32));
  IfStmtNode *ifStmtIsAllCompress = static_cast<IfStmtNode *>(builder->CreateStmtIf(condIsAllCompress));

  ifStmtIsTheSameType->thenPart->AddStatement(memCmpLen);
  ifStmtIsTheSameType->thenPart->AddStatement(ifStmtIsAllCompress);
  ifStmtIsTheSameType->thenPart->AddStatement(memcmpcallassign);

  ifStmtIsAllCompress->thenPart->AddStatement(memCmpLenCompress);
  DassignNode *cmpAssign =
    builder->CreateStmtDassign(retSt, 0, builder->CreateDread(tmpSym, retSt->GetType()->GetPrimType()));
  ifStmtIsTheSameType->thenPart->AddStatement(cmpAssign);

  MIRFunction *stringEqualsFunc =
      builder->GetOrCreateFunction(MRT_STRING_EQUALS_NOTALLCOMPRESS_STR, (TyIdx)(PTY_u1));
  MIRSymbol *tmpMrtEqualsSym = builder->GetOrCreateLocalDecl("__tmp_mrt_str_equal_result", GlobalTables::GetTypeTable().GetUInt1());

  MapleVector<BaseNode *> equalscallargs(module->memPoolAllocator.Adapter());
  equalscallargs.push_back(baseJstrOpnd);
  equalscallargs.push_back(compareToOpnd);

  CallNode *stringEqualscallassign =
    builder->CreateStmtCallAssigned(stringEqualsFunc->puIdx, equalscallargs, tmpMrtEqualsSym, OP_callassigned);

  // one is Compress, the other is not
  ifStmtIsTheSameType->elsePart->AddStatement(stringEqualscallassign);

  AddrofNode *tmpSymNode = builder->CreateExprDread(GlobalTables::GetTypeTable().GetUInt1(), 0, tmpMrtEqualsSym);

  DassignNode *assignTrue4 = builder->CreateStmtDassign(retSt, 0, tmpSymNode);
  ifStmtIsTheSameType->elsePart->AddStatement(assignTrue4);
  return ifStmt1;
}

void Simplify::SimplifyCheckNotNull(MIRFunction *func, StmtNode *stmt, MIRSymbol *retSt) {
  CallNode *cnode = static_cast<CallNode *>(stmt);
  BaseNode *inputOpnd = cnode->Opnd(0);
  // assertnonnull
  UnaryStmtNode *nullcheck = builder->CreateStmtUnary(OP_assertnonnull, inputOpnd);
  func->body->InsertBefore(stmt, nullcheck);
  // if ret value is used, add a dassign stmt.
  if (retSt != nullptr) {
    DassignNode *dassign = builder->CreateStmtDassign(retSt, 0, inputOpnd);
    func->body->InsertBefore(stmt, dassign);
  }
  // remove the call stmt.
  func->body->RemoveStmt(stmt);
}

void Simplify::SimplifyCallAssigned(MIRFunction *func, StmtNode *stmt) {
  CallNode *cnode = dynamic_cast<CallNode *>(stmt);
  if (cnode == nullptr) {
    return;
  }
  MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(cnode->puIdx);
  CallReturnVector &returnValues = cnode->returnValues;
  MIRSymbol *retSt = nullptr;
  if (returnValues.size() == 0 || !kOpcodeInfo.IsCallAssigned(stmt->op)) {
    retSt = nullptr;
  } else {
    ASSERT(returnValues.size() == 1, "Single Ret value for now.");
    StIdx stIdx = returnValues[0].first;
    RegFieldPair regfieldpair = returnValues[0].second;
    if (!regfieldpair.IsReg()) {
      retSt = currFunc->GetLocalOrGlobalSymbol(stIdx);
    }
  }

  if (!calleefunc) {
    return;
  }
  std::string funcname = calleefunc->GetName();
  if (funcname == (std::string(NameMangler::kJavaLangObjectStr) + NameMangler::kInitSuffix)) {
    StmtNode *newStmt = module->CurFuncCodeMemPool()->New<StmtNode>(OP_membarstorestore);
    func->body->ReplaceStmt1WithStmt2(stmt, newStmt);
  }

  // Simplify functions in the list.
  int32_t id = FindTableId(funcname);
  if (id < 0) {
    return;
  }

  if (kFuncs[id].do_simplify) {
    switch (kFuncs[id].index) {
      case kStringCharAt:
        SimplifyStrCharAt(func, stmt, retSt);
        break;
      case kStringEquals:
        SimplifyStrEquals(func, stmt, retSt);
        break;
      case kObjectsRequireNonNull:
        if (IsImplicitNullCheckModule()) {
          SimplifyCheckNotNull(func, stmt, retSt);
        }
        break;
      default:
        break;
    }
  } else if (kFuncs[id].replacename != "") {
    MIRType *returnType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(calleefunc->GetReturnTyIdx());
    MIRFunction *newfunc = builder->GetOrCreateFunction(kFuncs[id].replacename, TyIdx(returnType->primType));
    StmtNode *newstmt = nullptr;
    if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
      newstmt = builder->CreateStmtCallAssigned(newfunc->puIdx, cnode->nOpnd, retSt, OP_callassigned);
    } else {
      newstmt = builder->CreateStmtCall(newfunc->puIdx, cnode->nOpnd);
    }
    func->body->ReplaceStmt1WithStmt2(stmt, newstmt);
  }
}

void Simplify::ProcessFunc(MIRFunction *func) {
  if (func->IsEmpty()) {
    return;
  }

  SetCurrentFunction(func);

  StmtNode *stmt = func->body->GetFirst();
  StmtNode *next = nullptr;
  while (stmt) {
    next = stmt->GetNext();
    Opcode op = stmt->op;
    switch (op) {
      case OP_call:
      case OP_virtualcall:
        break;
      case OP_callassigned:
      case OP_virtualcallassigned:
      case OP_superclasscallassigned:
      case OP_interfacecallassigned:
      case OP_customcallassigned:
      case OP_polymorphiccallassigned:
      case OP_icallassigned:
      case OP_intrinsiccallassigned:
      case OP_xintrinsiccallassigned:
      case OP_intrinsiccallwithtypeassigned: {
        SimplifyCallAssigned(func, stmt);
        break;
      }
      default:
        break;
    }
    stmt = next;
  }
}

void Simplify::Finish() {
}

}  // namespace maple
