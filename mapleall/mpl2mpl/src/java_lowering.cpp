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

#include "java_lowering.h"
#include "name_mangler.h"
#include <fstream>
#include <algorithm>
#include <cstdio>

// JavaLowering lowers several kinds of intrinsics:
// 1. INTRN_JAVA_MERGE
//    Check if INTRN_JAVA_MERGE is legal:
//    if yes, turn it into a Retype or CvtType; if no, assert
// 2. INTRN_JAVA_FILL_NEW_ARRAY
//    Turn it into a jarray malloc and jarray element-wise assignment
//
// JavaLowering also performs the following optimizations:
// 1. Turn single-parameter Class.forName call into three-parameter
//    Class.forName call, where the third-parameter points to the
//    current class loader being used.

namespace maple {
inline bool IsConstvalZero(BaseNode *n) {
  return (n->op == OP_constval && static_cast<ConstvalNode *>(n)->constVal->IsZero());
}

JavaLowering::JavaLowering(MIRModule *mod, KlassHierarchy *kh, bool dump) : FuncOptimizeImpl(mod, kh, dump) {
  InitTypes();
  InitFuncs();
  InitLists();
}

void JavaLowering::InitLists() {
  if (Options::dumpClassloaderInvocation || !Options::classloaderInvocationList.empty()) {
    LoadClassLoaderInvocation(Options::classloaderInvocationList);
  }

  if (Options::dumpClassloaderInvocation) {
    // Remove any existing output file
    const string &mplName = module->GetFileName();
    CHECK_FATAL(mplName.rfind(".mpl") != string::npos, "File name %s does not contain .mpl", mplName.c_str());
    string prefix = mplName.substr(0, mplName.rfind(".mpl"));
    outfileName = prefix + ".clinvocation";
    std::remove(outfileName.c_str());
  }
}

void JavaLowering::InitTypes() {
  MIRType *classLoaderType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(NameMangler::kJavaLangClassloader)));
  CHECK_FATAL(classLoaderType, "NameMangler::kJavaLangClassloader type can not be null");
  mClassLoaderPointerToType = GlobalTables::GetTypeTable().GetOrCreatePointerType(classLoaderType, PTY_ref);
}

void JavaLowering::InitFuncs() {
  string funcClassForNamePrefix(NameMangler::kJavaLangClassStr);
  funcClassForNamePrefix.append(NameMangler::kNameSplitterStr);
  funcClassForNamePrefix.append("forName_7C_28");
  funcClassForNamePrefix.append(NameMangler::kJavaLangStringStr);

  string funcClassForName1 = funcClassForNamePrefix + NameMangler::kRightBracketStr + NameMangler::kJavaLangClassStr;

  string funcClassForName3 = funcClassForNamePrefix + string("Z") + NameMangler::kJavaLangClassloader
        + NameMangler::kRightBracketStr + NameMangler::kJavaLangClassStr;
  mClassForname1Func = builder->GetFunctionFromName(funcClassForName1);
  CHECK_FATAL(mClassForname1Func, "mClassForname1Func is null in JavaLowering::ProcessForNameClassloader");
  mClassForname3Func = builder->GetFunctionFromName(funcClassForName3);
  CHECK_FATAL(mClassForname3Func, "mClassForname3Func is null in JavaLowering::ProcessForNameClassloader");

#define FUNC_GET_CURRENT_CL "MCC_GetCurrentClassLoader"
  // MCC_GetCurrentClassLoader
  mGetCurrentClassLoaderFunc = builder->GetFunctionFromName(FUNC_GET_CURRENT_CL);
  if (!mGetCurrentClassLoaderFunc) {
    ArgVector clArgs(module->memPoolAllocator.Adapter());
    MIRType *refTy = GlobalTables::GetTypeTable().GetRef();
    clArgs.push_back(ArgPair("caller", refTy));
    mGetCurrentClassLoaderFunc = builder->CreateFunction(FUNC_GET_CURRENT_CL, refTy, clArgs);
    CHECK_FATAL(mGetCurrentClassLoaderFunc, "mGetCurrentClassLoaderFunc is null in JavaLowering::ProcessForNameClassloader");
  }
}

void JavaLowering::ProcessStmt(StmtNode *stmt) {
  if (!stmt) {
    return;
  }
  Opcode opcode = stmt->op;
  switch (opcode) {
    case OP_dassign:
    case OP_regassign: {
      BaseNode *rhs = nullptr;
      if (opcode == OP_dassign) {
        DassignNode *dassign = static_cast<DassignNode *>(stmt);
        rhs = dassign->GetRhs();
      } else {
        RegassignNode *regassign = static_cast<RegassignNode *>(stmt);
        rhs = regassign->GetRhs();
      }
      if (rhs && rhs->op == OP_intrinsicop) {
        IntrinsicopNode *intrinnode = static_cast<IntrinsicopNode *>(rhs);
        if (intrinnode->intrinsic == INTRN_JAVA_MERGE) {
          ProcessJavaMerge(stmt, intrinnode);
        }
      }
      break;
    }
    case OP_callassigned: {
      CallNode *call = static_cast<CallNode *>(stmt);
      // currently it's only for classloader
      ProcessForNameClassloader(call);
      break;
    }
    case OP_intrinsiccallwithtypeassigned: {
      IntrinsiccallNode *intrinCall = static_cast<IntrinsiccallNode *>(stmt);
      if (intrinCall->intrinsic == INTRN_JAVA_FILL_NEW_ARRAY) {
        ProcessJavaFillNewArray(intrinCall);
      }
      break;
    }
    default:
      break;
  }
}

void JavaLowering::CheckClassloaderInvocation(const CallNode *callNode) const {
  MIRFunction *callee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callNode->puIdx);
  if (clInterfaceSet.find(callee->GetName()) != clInterfaceSet.end()) {
    auto range = clInvocationMap.equal_range(currFunc->GetName());
    for (auto i = range.first; i != range.second; i++) {
      std::string val = i->second;
      if (val == callee->GetName()) {
        return;
      }
    }
    CHECK_FATAL(false,
           "Check ClassLoader Invocation, failed. \
          Please copy \"%s,%s\" into %s, and submit it to review. mpl file:%s",
           NameMangler::DecodeName(currFunc->GetName()).c_str(), NameMangler::DecodeName(callee->GetName()).c_str(),
           Options::classloaderInvocationList.c_str(), module->GetFileName().c_str());
  }
}

void JavaLowering::DumpClassloaderInvocation(const CallNode *callNode) {
  MIRFunction *callee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callNode->puIdx);
  if (clInterfaceSet.find(callee->GetName()) != clInterfaceSet.end()) {
    builder->GlobalLock();
    ofstream outfile;
    outfile.open(outfileName, std::ios::out | std::ios::app);
    ASSERT(!outfile.fail(), "Dump ClassLoader Invocation, open file failed.");
    outfile << NameMangler::DecodeName(currFunc->GetName()) << "," << NameMangler::DecodeName(callee->GetName())
            << endl;
    outfile.close();

    LogInfo::MapleLogger() << "Dump ClassLoader Invocation, \"" << NameMangler::DecodeName(currFunc->GetName()) << ","
         << NameMangler::DecodeName(callee->GetName()) << "\", " << module->GetFileName() << endl;
    builder->GlobalUnlock();
  }
}

void JavaLowering::LoadClassLoaderInvocation(const std::string &list) {
  std::ifstream infile;
  infile.open(list);
  bool fileOpenStatus = infile.fail();
  if (fileOpenStatus == true) {
    CHECK_FATAL(false, "Load ClassLoader Invocation, open file %s failed.", list.c_str());
  }
  bool reachInvocation = false;
  std::string line;

  // Load ClassLoader Interface&Invocation Config.
  // There're two parts: interfaces and invocations in the list.
  // Firstly loading interfaces, then loading invocations.
  while (getline(infile, line)) {
    if (line.empty()) {
      continue;  // Ignore empty line.
    }
    if (line.front() == '#') {
      continue;  // Ignore comment line.
    }

    // Check if reach invocation parts by searching ','
    if (!reachInvocation && std::string::npos != line.find(',')) {
      reachInvocation = true;
    }

    if (!reachInvocation) {
      // Load interface.
      clInterfaceSet.insert(NameMangler::EncodeName(line));
    } else {
      // Load invocation, which has 2 elements seperated by ','.
      std::stringstream ss(line);
      std::string caller;
      std::string callee;

      if (!getline(ss, caller, ',')) {
        ASSERT(false, "Load ClassLoader Invocation, wrong format.");
      }
      if (!getline(ss, callee, ',')) {
        ASSERT(false, "Load ClassLoader Invocation, wrong format.");
      }
      clInvocationMap.insert(make_pair(NameMangler::EncodeName(caller), NameMangler::EncodeName(callee)));
    }
  }
  infile.close();
}

void JavaLowering::ProcessForNameClassloader(CallNode *callNode) {
  if (callNode->puIdx != mClassForname1Func->puIdx) {
    return;
  }

  MapleVector<BaseNode *> currentCLArgs(currFunc->codeMemPoolAllocator.Adapter());
  // jobject caller = this ? this : __classinfo_xxx;
  if (currFunc->IsStatic()) {
    // it's a static function.
    // pass caller functions's classinfo directly
    std::string callerName = CLASSINFO_PREFIX_STR + currFunc->GetBaseClassName();
    MIRSymbol *callerClassinfoSym = builder->GetOrCreateGlobalDecl(callerName, GlobalTables::GetTypeTable().GetVoidPtr(), kScExtern);
    currentCLArgs.push_back(builder->CreateExprAddrof(0, callerClassinfoSym));
  } else {
    // it's an instance function.
    // pass caller function's this pointer
    CHECK_FATAL(currFunc->formalDefVec.size() > 0, "index out of range in JavaLowering::ProcessForNameClassloader");
    MIRSymbol *formalst = currFunc->formalDefVec[0].formalSym;
    BaseNode *callerObjExpr = nullptr;
    if (formalst->sKind != kStPreg) {
      callerObjExpr = builder->CreateExprDread(formalst);
    } else {
      callerObjExpr = builder->CreateExprRegread(
        formalst->GetType()->primType, currFunc->pregTab->GetPregIdxFromPregNo(formalst->value.preg->pregNo));
    }
    currentCLArgs.push_back(callerObjExpr);
  }
  MIRSymbol *currentCL = builder->GetOrCreateLocalDecl("retvar_current_classloader", mClassLoaderPointerToType);
  // jobject current_cl = __mrt_get_current_classloader(jobject caller);
  CallNode *clCall =
    builder->CreateStmtCallAssigned(mGetCurrentClassLoaderFunc->puIdx, currentCLArgs, currentCL, OP_callassigned);
  currFunc->body->InsertBefore(callNode, clCall);

  // Class.forName(jstring name) ==>
  // Class.forName(jstring name, jboolean 1, jobject current_cl)
  // ensure initialized is true
  callNode->nOpnd.push_back(builder->GetConstUInt1(true));
  // classloader
  callNode->nOpnd.push_back(builder->CreateExprDread(currentCL));
  callNode->numOpnds = callNode->nOpnd.size();
  callNode->puIdx = mClassForname3Func->puIdx;

  if (!Options::dumpClassloaderInvocation && !Options::classloaderInvocationList.empty()) {
    CheckClassloaderInvocation(callNode);
  }
  if (Options::dumpClassloaderInvocation) {
    DumpClassloaderInvocation(callNode);
  }
}

void JavaLowering::ProcessJavaMerge(StmtNode *assignNode, const IntrinsicopNode *intrinnode) {
  CHECK_FATAL(intrinnode->numOpnds == 1, "invalid JAVA_MERGE intrinsic node");
  PrimType dtyp;
  DassignNode *dassign = nullptr;
  RegassignNode *regassign = nullptr;
  if (assignNode->op == OP_dassign) {
    dassign = static_cast<DassignNode *>(assignNode);
    MIRSymbol *dest = currFunc->GetLocalOrGlobalSymbol(dassign->stIdx);
    dtyp = dest->GetType()->GetPrimType();
  } else {
    regassign = static_cast<RegassignNode *>(assignNode);
    dtyp = regassign->primType;
  }

  BaseNode *resNode = intrinnode->Opnd(0);
  CHECK_FATAL(resNode != nullptr, "null ptr check");
  PrimType styp = resNode->primType;
  if (dtyp != styp) {
    resNode = JavaMergeToCvtType(dtyp, styp, resNode);
  }

  if (assignNode->op == OP_dassign) {
    dassign->SetRhs(resNode);
  } else {
    regassign->SetRhs(resNode);
  }
}

BaseNode *JavaLowering::JavaMergeToCvtType(PrimType dtyp, PrimType styp, BaseNode *src) {
  CHECK_FATAL(IsPrimitiveInteger(dtyp) || IsPrimitiveFloat(dtyp), "typemerge source type is not a primitive type");
  CHECK_FATAL(IsPrimitiveInteger(styp) || IsPrimitiveFloat(styp), "typemerge destination type is not a primitive type");
  // src i32, dest f32; src i64, dest f64
  if (!((IsPrimitiveInteger(styp) && IsPrimitiveFloat(dtyp) && GetPrimTypeBitSize(styp) <= GetPrimTypeBitSize(dtyp)) ||
        (IsPrimitiveInteger(styp) && IsPrimitiveInteger(dtyp)))) {
    CHECK_FATAL(false, "Wrong type in typemerge: styp is %s; dtyp is %s", styp, dtyp);
  }

  // src & dest are both of float type
  MIRType *totype = GlobalTables::GetTypeTable().GetPrimType(dtyp);
  MIRType *fromtype = GlobalTables::GetTypeTable().GetPrimType(styp);
  if (IsPrimitiveInteger(styp) && IsPrimitiveFloat(dtyp)) {
    if (GetPrimTypeBitSize(styp) == GetPrimTypeBitSize(dtyp)) {
      return builder->CreateExprRetype(totype, fromtype, src);
    } else {
      return builder->CreateExprTypeCvt(OP_cvt, totype, fromtype, src);
    }
  } else if (IsPrimitiveInteger(styp) && IsPrimitiveInteger(dtyp)) {
    if (GetPrimTypeBitSize(styp) >= GetPrimTypeBitSize(dtyp)) {
      if (dtyp == PTY_u1)  // e.g., type _Bool
        return builder->CreateExprCompare(OP_ne, totype, fromtype, src, builder->CreateIntConst(0, styp));
      else if (GetPrimTypeBitSize(styp) > GetPrimTypeBitSize(dtyp)) {
        return builder->CreateExprTypeCvt(OP_cvt, totype, fromtype, src);
      } else if (IsSignedInteger(styp) != IsSignedInteger(dtyp)) {
        return builder->CreateExprTypeCvt(OP_cvt, totype, fromtype, src);
      } else {
        src->primType = dtyp;
        return src;
      }
      // Force type cvt here because we currently do not run constant folding
      // or contanst propagation before CG. We may revisit this decision later.
    } else if (GetPrimTypeBitSize(styp) < GetPrimTypeBitSize(dtyp)) {
      return builder->CreateExprTypeCvt(OP_cvt, totype, fromtype, src);
    } else if (IsConstvalZero(src)) {
      return builder->CreateIntConst(0, dtyp);
    } else {
      CHECK_FATAL(false, "NYI. Don't know what to do");
    }
  } else {
    CHECK_FATAL(false, "NYI. Don't know what to do");
  }
}

void JavaLowering::ProcessJavaFillNewArray(IntrinsiccallNode *intrinCall) {
  // First create a new array
  CHECK_FATAL(intrinCall->returnValues.size() == 1, "INTRN_JAVA_FILL_NEW_ARRAY should have 1 return value");
  CallReturnPair retPair = intrinCall->returnValues.at(0);
  bool isReg = retPair.second.IsReg();
  MIRType *retType = nullptr;
  if (!isReg) {
    retType = currFunc->GetLocalOrGlobalSymbol(retPair.first)->GetType();
  } else {
    PregIdx pregidx = retPair.second.GetPregidx();
    MIRPreg *mirpreg = currFunc->pregTab->PregFromPregIdx(pregidx);
    CHECK_FATAL(mirpreg->primType == PTY_ref || mirpreg->primType == PTY_ptr, "Dst preg needs to be a pointer or reference type");
    retType = mirpreg->mirType;
  }
  CHECK_FATAL(retType->typeKind == kTypePointer, "Return type of INTRN_JAVA_FILL_NEW_ARRAY should point to a Jarray");
  MIRType *arrayType = static_cast<MIRPtrType *>(retType)->GetPointedType();
  BaseNode *lenNode = builder->CreateIntConst(intrinCall->NumOpnds(), PTY_i32);
  JarrayMallocNode *newArrayNode = builder->CreateExprJarrayMalloc(OP_gcmallocjarray, retType, arrayType, lenNode);
  // Then fill each array element one by one
  BaseNode *addrExpr = nullptr;
  StmtNode *assignStmt = nullptr;
  if (!isReg) {
    MIRSymbol *retSym = currFunc->GetLocalOrGlobalSymbol(retPair.first);
    assignStmt = builder->CreateStmtDassign(retSym, retPair.second.GetFieldid(), newArrayNode);
    currFunc->body->ReplaceStmt1WithStmt2(intrinCall, assignStmt);
    addrExpr = builder->CreateExprDread(retSym);
  } else {
    PregIdx pregidx = retPair.second.GetPregidx();
    MIRPreg *mirpreg = currFunc->pregTab->PregFromPregIdx(pregidx);
    assignStmt = builder->CreateStmtRegassign(mirpreg->primType, pregidx, newArrayNode);
    currFunc->body->ReplaceStmt1WithStmt2(intrinCall, assignStmt);
    addrExpr = builder->CreateExprRegread(mirpreg->primType, pregidx);
  }
  StmtNode *stmt = assignStmt;
  for (int i = 0; i < intrinCall->NumOpnds(); i++) {
    MapleVector<BaseNode *> opnds(currFunc->codeMemPoolAllocator.Adapter());
    opnds.push_back(addrExpr);
    opnds.push_back(builder->CreateIntConst(i, PTY_i32));
    ArrayNode *arrayexpr = builder->CreateExprArray(arrayType, opnds);
    arrayexpr->boundsCheck = false;
    StmtNode *storeStmt = builder->CreateStmtIassign(retType, 0, arrayexpr, intrinCall->Opnd(i));
    currFunc->body->InsertAfter(stmt, storeStmt);
    stmt = storeStmt;
  }
}

}  // namespace maple
