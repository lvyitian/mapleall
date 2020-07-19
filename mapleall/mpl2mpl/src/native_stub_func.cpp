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

#include "native_stub_func.h"
#include "name_mangler.h"
#include "vtable_analysis.h"
#include "reflection_analysis.h"
#include <iostream>
#include <fstream>
#include "name_mangler.h"

/*
 * This phase is the processing of the java native function. It
 * generates an extra stubFunc for each native function, preparing
 * the preparations before the actual native function is called,
 * including parameter mapping, GC preparation, and so on.
 */
namespace maple {

#define PRE_NATIVE_FUNC "MCC_PreNativeCall"
#define POST_NATIVE_FUNC "MCC_PostNativeCall"
#define DECODE_REF_FUNC "MCC_DecodeReference"
#define FIND_NATIVE_FUNC "MCC_FindNativeMethodPtr"
#define FIND_NATIVE_FUNC_NOEH "MCC_FindNativeMethodPtrWithoutException"
#define DUMMY_NATIVE_FUNC "MCC_DummyNativeMethodPtr"
#define CHECK_THROW_PENDING_EXCEPTION_FUNC "MCC_CheckThrowPendingException"
#define CALL_FAST_NATIVE_FUNC "MCC_CallFastNative"
#define CALL_FAST_NATIVE_EXT_FUNC "MCC_CallFastNativeExt"
#define CALL_SLOW_NATIVE_EXT_FUNC "MCC_CallSlowNativeExt"
#define SET_RELIABLE_UNWIND_CONTEXT_FUNC "MCC_SetReliableUnwindContext"

static constexpr int64 kInvalidCode = 0x00000000000000FF;

GenNativeStubFunc::GenNativeStubFunc(MIRModule *mod, KlassHierarchy *kh, bool dump) : FuncOptimizeImpl(mod, kh, dump) {
  MIRType *jstrType =
      GlobalTables::GetTypeTable().GetOrCreateClassType(NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangStringStr).c_str(), mod);
  MIRPtrType *jstrPtype = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetOrCreatePointerType(jstrType, PTY_ref));
  jstrPtrTyidx = jstrPtype->tyIdx;

  LoadNativeFuncProperty();
  InitManualFastAndCriticalNative();
  GenRegTableEntryType();
  GenHelperFuncDecl();
  GenRegFuncTabEntryType();
  InitStaticBindingMethodList();
}

MIRFunction *GenNativeStubFunc::GetOrCreateDefaultNativeFunc(MIRFunction *stubFunc) {
  // if only support dynamic binding , we won't stub any weak symbols
  if (Options::regNativeDynamicOnly && !(IsStaticBindingListMode() && IsStaticBindingMethod(stubFunc->GetName()))) {
    return stubFunc;
  }
  std::string nativeName = NameMangler::NativeJavaName(stubFunc->GetName().c_str());
  // No need to create a default function with exact arguments here
  MIRFunction *nativeFunc = builder->GetOrCreateFunction(nativeName, stubFunc->GetReturnTyIdx());
  nativeFunc->srcPosition.SetMplLinenum(stubFunc->srcPosition.MplLinenum());

  if (!nativeFunc->body) {
    if (nativeFunc->symTab == nullptr) {
      nativeFunc->symTab = nativeFunc->dataMemPool->New<MIRSymbolTable>(&nativeFunc->dataMPAllocator);
      nativeFunc->pregTab = nativeFunc->dataMemPool->New<MIRPregTable>(&nativeFunc->dataMPAllocator);
      nativeFunc->typeNameTab = nativeFunc->dataMemPool->New<MIRTypeNameTable>(&nativeFunc->dataMPAllocator);
      nativeFunc->labelTab = nativeFunc->dataMemPool->New<MIRLabelTable>(&nativeFunc->dataMPAllocator);
    }
    builder->SetCurrentFunction(nativeFunc);
    nativeFunc->SetAttr(FUNCATTR_weak);
    nativeFunc->body = nativeFunc->codeMemPool->New<BlockNode>();
    // we would not throw exception here.
    // Use regnative-dynamic-only option when run case expr14301a_setFields__IF as qemu solution.
    MIRType *voidPtrTy = GlobalTables::GetTypeTable().GetVoidPtr();

    // It will throw java.lang.UnsatisfiedLinkError, while issue a runtime
    // warning on Qemu/arm64-server (because it lacks most of the required native libraries)
    MIRFunction *findNativeFunc = nullptr;
    if (ReturnsJstr(stubFunc->GetReturnTyIdx())) {
      // a dialet for string
      findNativeFunc =
        builder->GetOrCreateFunction("MCC_CannotFindNativeMethod_S", voidPtrTy->GetTypeIndex());
    } else {
      MIRType *returnType = stubFunc->GetReturnType();
      if ((returnType->typeKind == kTypePointer) &&
          ((static_cast<MIRPtrType *>(returnType))->GetPointedType()->typeKind == kTypeJArray)) {
        // a dialet for array
        findNativeFunc =
          builder->GetOrCreateFunction("MCC_CannotFindNativeMethod_A", voidPtrTy->GetTypeIndex());
      }
    }

    // default callback
    if (findNativeFunc == nullptr) {
      findNativeFunc =
        builder->GetOrCreateFunction("MCC_CannotFindNativeMethod", voidPtrTy->GetTypeIndex());
    }

    findNativeFunc->SetAttr(FUNCATTR_nosideeffect);

    MapleVector<BaseNode *> opnds(nativeFunc->codeMemPoolAllocator.Adapter());
    // fatal message parameter
    std::string nativeSymbolName = stubFunc->GetName().c_str();
    UStrIdx strIdx = GlobalTables::GetUStrTable().GetOrCreateStrIdxFromName(nativeSymbolName);
    ConststrNode *signatureNode = nativeFunc->codeMemPool->New<ConststrNode>(strIdx);
    signatureNode->primType = PTY_ptr;
    opnds.push_back(signatureNode);

    CallNode *callGetFindNativeFunc =
      builder->CreateStmtCallAssigned(findNativeFunc->puIdx, opnds, nullptr, OP_callassigned);
    nativeFunc->body->AddStatement(callGetFindNativeFunc);

    module->AddFunction(nativeFunc);
    builder->SetCurrentFunction(stubFunc);
    // call to __MRT_CannotFindnativeMethod will return a ref to java.lang.Class
    // use it as the return value of the fake-native-function if it needs to return something.
  }
  return nativeFunc;
}

// The final order of statements inside of this stub function may need to be adjusted.
//   syncenter (dread ref %_this) // if native func is synchronized
//   callassigned &__MRT_PreNativeCall (addrof ptr $__cinf_calling_class) {regassign ptr %2}
//   // or callassigned &__MRT_PreNativeCall (regread ref %_this) {regassign ptr %2}
//
//   call to the actual registered or implemented native function
//
//   callassigned &MCC_DecodeReference(dread ref %retvar_stubfunc) {dassign ref %retvar_stubfunc}
//   callassigned &__MRT_PostNativeCall (dread ptr %env_ptr) {}
//   syncexit (dread ref %_this) // if native func is synchronized
//
//   in the end and before return to Java frame, check pending exception
//   callassigned &MCC_CheckThrowPendingException () {}
void GenNativeStubFunc::ProcessFunc(MIRFunction *func) {
  if ((!func->GetAttr(FUNCATTR_native) &&
       !func->GetAttr(FUNCATTR_fast_native) &&
       !func->GetAttr(FUNCATTR_critical_native)) ||
      func->GetAttr(FUNCATTR_bridge)) {
    return;
  }

  SetCurrentFunction(func);

  if (trace) {
    cerr << "Create stub func: " << func->GetName() << endl;
  }

  if (func->body) {
    func->body->ResetBlock();
  } else {
    func->body = func->codeMemPool->New<BlockNode>();
  }

  if (IsManualCriticalNative(func->GetName())) {
    func->SetAttr(FUNCATTR_critical_native);
  }
  if (IsManualFastNative(func->GetName())) {
    func->SetAttr(FUNCATTR_fast_native);
  }

  NativeFuncProperty funcProperty;
  bool existsFuncProperty = GetNativeFuncProperty(func->GetName(), funcProperty);
  bool needNativeCall =
    (!func->GetAttr(FUNCATTR_critical_native)) && (!existsFuncProperty || funcProperty.jniType == kJniTypeNormal);
  bool needCheckThrowPendingExceptionFunc = needNativeCall;

  if (funcProperty.jniType == kJnitTypeCriticalNative) {
    func->SetAttr(FUNCATTR_critical_native);
  }

  GStrIdx classObjSymStridx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(CLASSINFO_PREFIX_STR + func->GetBaseClassName());
  MIRSymbol *classObjSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(classObjSymStridx);
  ASSERT(classObjSym, "Classinfo for %s is not found", func->GetBaseClassName().c_str());

  // Generate MonitorEnter if this is a synchronized method

  if (func->GetAttr(FUNCATTR_synchronized)) {
    BaseNode *monitor = nullptr;
    if (func->GetAttr(FUNCATTR_static)) {
      // Grab class object
      monitor = builder->CreateExprAddrof(0, classObjSym);
    } else {
      // Grab _this pointer
      const int funcFormalsSize = func->formalDefVec.size();
      CHECK_FATAL(funcFormalsSize > 0, "container check");
      MIRSymbol *formal0st = func->formalDefVec[0].formalSym;
      if (formal0st->sKind == kStPreg)
        monitor = builder->CreateExprRegread(formal0st->GetType()->primType,
                                                func->pregTab->GetPregIdxFromPregNo(formal0st->GetPreg()->pregNo));
      else {
        monitor = builder->CreateExprDread(formal0st);
      }
    }

    NaryStmtNode *syncenter = builder->CreateStmtNary(OP_syncenter, monitor);
    func->body->AddStatement(syncenter);
  }

  // Get Env pointer, skip for critical native functions who do not need Env
  // Generate stubfunc call/return stmt, extra args only for non-critical_native calls
  MIRSymbol *envPtrSym = nullptr;
  PregIdx envpregidx = 0;
  MapleVector<BaseNode *> alloccallargs(func->codeMemPoolAllocator.Adapter());
  if (!func->GetAttr(FUNCATTR_critical_native)) {
    // a return value
    if (Options::usepreg) {
      envpregidx = func->pregTab->CreatePreg(PTY_ptr);
    } else {
      envPtrSym = builder->CreateLocalDecl("env_ptr", GlobalTables::GetTypeTable().GetVoidPtr());
    }

    if (needNativeCall) {
      // Generate a MRT call for extra work before calling the native
      BaseNode *callerObj = nullptr;  // it will be used by PreNativeCall, and might be used by syncenter
      if (func->GetAttr(FUNCATTR_static)) {
        // Grab class object
        callerObj = builder->CreateExprAddrof(0, classObjSym);
      } else {
        // Grab _this pointer
        MIRSymbol *formal0st = func->formalDefVec[0].formalSym;
        if (formal0st->sKind == kStPreg)
          callerObj = builder->CreateExprRegread(formal0st->GetType()->primType,
                                                    func->pregTab->GetPregIdxFromPregNo(formal0st->GetPreg()->pregNo));
        else {
          callerObj = builder->CreateExprDread(formal0st);
        }
      }
      MapleVector<BaseNode *> preArgs(func->codeMemPoolAllocator.Adapter());
      preArgs.push_back(callerObj);  // caller object
      CallNode *preFuncCall =
        Options::usepreg
          ? builder->CreateStmtCallRegassigned(MRTPreNativeFunc->puIdx, preArgs, envpregidx, OP_callassigned)
          : builder->CreateStmtCallAssigned(MRTPreNativeFunc->puIdx, preArgs, envPtrSym, OP_callassigned);
      func->body->AddStatement(preFuncCall);
    }

    // set up env
    alloccallargs.push_back(Options::usepreg ?
                            (static_cast<BaseNode *>(builder->CreateExprRegread(PTY_ptr, envpregidx))) :
                            (static_cast<BaseNode *>(builder->CreateExprDread(envPtrSym))));
    // set up class
    if (func->GetAttr(FUNCATTR_static)) {
      alloccallargs.push_back(builder->CreateExprAddrof(0, classObjSym));
    }
  }

  for (FormalDef formalDef : func->formalDefVec) {
    MIRSymbol *argSt = formalDef.formalSym;
    BaseNode *argExpr = nullptr;
    if (argSt->sKind == kStPreg)
      argExpr = builder->CreateExprRegread(argSt->GetType()->primType,
                                              func->pregTab->GetPregIdxFromPregNo(argSt->GetPreg()->pregNo));
    else {
      argExpr = builder->CreateExprDread(argSt);
    }
    alloccallargs.push_back(argExpr);
  }

  bool voidRet = (func->GetReturnType()->primType == PTY_void);
  MIRSymbol *stubfuncRet = nullptr;
  if (!voidRet)
    stubfuncRet = builder->CreateLocalDecl("retvar_stubfunc", func->GetReturnType());

  MIRFunction *nativeFunc = GetOrCreateDefaultNativeFunc(func);
  if (Options::regNativeFunc) {
    GenRegisteredNativeFunctionCall(func, nativeFunc, alloccallargs, stubfuncRet);
  } else if (Options::nativeWrapper) {
    GenNativeWrapperFunctionCall(func, nativeFunc, alloccallargs, stubfuncRet);
  } else {
    CallNode *callassign =
      builder->CreateStmtCallAssigned(nativeFunc->puIdx, alloccallargs, stubfuncRet, OP_callassigned);
    func->body->AddStatement(callassign);
  }

  if (func->GetReturnType()->primType == PTY_ref) {
    // Generate a MRT call to decode the tagged pointer
    MapleVector<BaseNode *> decodeArgs(func->codeMemPoolAllocator.Adapter());
    CHECK_FATAL(stubfuncRet != nullptr, "stubfunc_ret is nullptr");
    decodeArgs.push_back(builder->CreateExprDread(stubfuncRet));
    CallNode *decodeFuncCall =
      builder->CreateStmtCallAssigned(MRTDecodeRefFunc->puIdx, decodeArgs, stubfuncRet, OP_callassigned);
    func->body->AddStatement(decodeFuncCall);
  }

  // Generate a MRT call for extra work after calling the native
  if (needNativeCall) {
    MapleVector<BaseNode *> postArgs(func->codeMemPoolAllocator.Adapter());
    postArgs.push_back(Options::usepreg ?
                       (static_cast<BaseNode *>(builder->CreateExprRegread(PTY_ptr, envpregidx))) :
                       (static_cast<BaseNode *>(builder->CreateExprDread(envPtrSym))));
    CallNode *postFuncCall =
      builder->CreateStmtCallAssigned(MRTPostNativeFunc->puIdx, postArgs, nullptr, OP_callassigned);
    func->body->AddStatement(postFuncCall);
  }

  // Generate MonitorExit if this is a synchronized method
  if (func->GetAttr(FUNCATTR_synchronized)) {
    BaseNode *monitor = nullptr;
    if (func->GetAttr(FUNCATTR_static)) {
      // Grab class object
      monitor = builder->CreateExprAddrof(0, classObjSym);
    } else {
      // Grab _this pointer
      MIRSymbol *formal0st = func->formalDefVec[0].formalSym;
      if (formal0st->sKind == kStPreg)
        monitor = builder->CreateExprRegread(formal0st->GetType()->primType,
                                                func->pregTab->GetPregIdxFromPregNo(formal0st->GetPreg()->pregNo));
      else {
        monitor = builder->CreateExprDread(formal0st);
      }
    }
    NaryStmtNode *syncexit = builder->CreateStmtNary(OP_syncexit, monitor);
    func->body->AddStatement(syncexit);
  }

  // check pending exception just before leaving this stub frame except for critical natives
  if (needCheckThrowPendingExceptionFunc) {
    MapleVector<BaseNode *> getExceptArgs(func->codeMemPoolAllocator.Adapter());
    CallNode *callGetExceptFunc = builder->CreateStmtCallAssigned(MRTCheckThrowPendingExceptionFunc->puIdx,
                                                                             getExceptArgs, nullptr, OP_callassigned);
    func->body->AddStatement(callGetExceptFunc);
  }

  // to exclude this funciton
  // this function is a bridge function generated for Java Genetic
  if ((func->GetAttr(FUNCATTR_native) || func->GetAttr(FUNCATTR_fast_native)) &&
      !func->GetAttr(FUNCATTR_critical_native) && !func->GetAttr(FUNCATTR_bridge)) {
    MapleVector<BaseNode *> frameStatusArgs(func->codeMemPoolAllocator.Adapter());
    CallNode *callSetFrameStatusFunc = builder->CreateStmtCallAssigned(
      MCCSetReliableUnwindContextFunc->puIdx, frameStatusArgs, nullptr, OP_callassigned);
    func->body->AddStatement(callSetFrameStatusFunc);
  }

  if (!voidRet) {
    StmtNode *stmt = builder->CreateStmtReturn(builder->CreateExprDread(stubfuncRet));
    func->body->AddStatement(stmt);
  }
}

void GenNativeStubFunc::GenRegFuncTabEntryType() {
  uint32_t arraySize;
  arraySize = 0;

  MIRArrayType *arraytype =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(GlobalTables::GetTypeTable().GetVoidPtr(), arraySize);
  regFuncTabConst = module->memPool->New<MIRAggConst>(module, arraytype);

  std::string regFuncTab = NameMangler::kRegJNIFuncTabPrefixStr + module->GetFileNameAsPostfix();

  regFuncSymbol = builder->CreateGlobalDecl(regFuncTab, regFuncTabConst->type, kScGlobal);
}

void GenNativeStubFunc::GenRegFuncTabEntry() {
  uint64_t locIdx = regFuncTabConst->constVec.size();
  MIRConst *newconst =
      module->memPool->New<MIRIntConst>((uint64_t)((locIdx << 4) | 0xFF00000000000000), GlobalTables::GetTypeTable().GetVoidPtr());
  regFuncTabConst->constVec.push_back(newconst);
}

void GenNativeStubFunc::GenRegFuncTab() {
  MIRArrayType *arraytype = static_cast<MIRArrayType *>(regFuncTabConst->type);
  ASSERT(arraytype, "Can not get arraytype from ref_func_tab");
  arraytype->sizeArray[0] = regFuncTabConst->constVec.size();
  regFuncSymbol->SetConst(regFuncTabConst);
}

void GenNativeStubFunc::GenRegTabEntry(const MIRFunction *func) {
  std::string tmp = func->GetName();
  tmp = NameMangler::DecodeName(tmp);
  std::string base = func->GetBaseClassName();
  base = NameMangler::DecodeName(base);
  if (tmp.length() > base.length() && tmp.find(base) != string::npos) {
    tmp.replace(tmp.find(base), base.length() + 1, "");
  }
  uint32 nameIdx = ReflectionAnalysis::FindOrInsertRepeatString(tmp, true);    // always used
  uint32 classIdx = ReflectionAnalysis::FindOrInsertRepeatString(base, true);  // always used
  // Using MIRIntConst instead of MIRStruct for RegTable.
  MIRConst *classidx = module->memPool->New<MIRIntConst>(classIdx, GlobalTables::GetTypeTable().GetVoidPtr());
  regTableConst->constVec.push_back(classidx);
  MIRConst *newconst = module->memPool->New<MIRIntConst>(nameIdx, GlobalTables::GetTypeTable().GetVoidPtr());
  regTableConst->constVec.push_back(newconst);
}

void GenNativeStubFunc::GenRegisteredNativeFunctionCall(MIRFunction *func, const MIRFunction *nativeFunc,
                                                        MapleVector<BaseNode *> &args, MIRSymbol *ret) {
  // Generate registration table entry.
  GenRegTabEntry(func);
  GenRegFuncTabEntry();

  CallReturnVector returnValues(func->codeMemPoolAllocator.Adapter());
  if (ret) {
    CHECK_FATAL(ret->storageClass == kScAuto || ret->storageClass == kScFormal || ret->storageClass == kScExtern || ret->storageClass == kScGlobal,
           "");
    returnValues.push_back(CallReturnPair(ret->GetStIdx(), RegFieldPair(0, 0)));
  }

  uint32_t loc = regFuncTabConst->constVec.size();
  MIRArrayType *regArraytype = static_cast<MIRArrayType *>(regFuncTabConst->type);

  MapleVector<BaseNode *> opnds(builder->GetCurrentFuncCodeMpAllocator()->Adapter());
  AddrofNode *regFuncExpr = builder->CreateExprAddrof(0, regFuncSymbol);
  opnds.push_back(regFuncExpr);
  opnds.push_back(builder->CreateIntConst(loc - 1, PTY_i32));

  ArrayNode *arrayExpr = builder->CreateExprArray(regArraytype, opnds);
  arrayExpr->boundsCheck = false;
  MIRType *elemtype = static_cast<MIRArrayType *>(regArraytype)->GetElemType();
  BaseNode *ireadExpr = builder->CreateExprIread(elemtype, GlobalTables::GetTypeTable().GetOrCreatePointerType(elemtype), 0, arrayExpr);

  // assign registered func ptr to a preg.
  auto funcptrPreg = func->pregTab->CreatePreg(PTY_ptr);
  RegassignNode *funcptrAssign = builder->CreateStmtRegassign(PTY_ptr, funcptrPreg, ireadExpr);

  // read func ptr from preg
  auto readFuncPtr = builder->CreateExprRegread(PTY_ptr, funcptrPreg);

  NativeFuncProperty funcProperty;
  bool existsFuncProperty = GetNativeFuncProperty(func->GetName(), funcProperty);
  bool needCheckThrowPendingExceptionFunc =
    (!func->GetAttr(FUNCATTR_critical_native)) && (!existsFuncProperty || funcProperty.jniType == kJniTypeNormal);
  // get current native method function ptr from reg_jni_func_tab slot
  // define a temp register for shift operation
  auto funcptrshiftPreg = func->pregTab->CreatePreg(PTY_ptr);
  BaseNode *regreadExpr = builder->CreateExprRegread(PTY_ptr, funcptrPreg);
  BaseNode *shiftExpr =
      builder->CreateExprBinary(OP_lshr, GlobalTables::GetTypeTable().GetPtr(), regreadExpr, builder->CreateIntConst(56, PTY_u64));
  RegassignNode *funcptrshiftAssign = builder->CreateStmtRegassign(PTY_ptr, funcptrshiftPreg, shiftExpr);
  auto readFuncptrshift = builder->CreateExprRegread(PTY_ptr, funcptrshiftPreg);
  BaseNode *checkRegExpr =
      builder->CreateExprCompare(OP_eq, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetPtr(), readFuncptrshift,
                                  builder->CreateIntConst(kInvalidCode, PTY_ptr));
  IfStmtNode *ifStmt = static_cast<IfStmtNode *>(builder->CreateStmtIf(checkRegExpr));
  // get find_native_func function
  MIRType *voidPtrTy = GlobalTables::GetTypeTable().GetVoidPtr();
  // set parameter of find_native_func
  MapleVector<BaseNode *> dynamicStubOpnds(func->codeMemPoolAllocator.Adapter());
  dynamicStubOpnds.push_back(arrayExpr);

  // use native wrapper if required.
  if (Options::nativeWrapper) {
    // now native have three mode: default mode, dynamic-only mode, static binding list mode
    // default mode, it will generate a week function, which can link in compile time
    // dynamic only mode, it won't generate any week function, it can't link in compile time
    // static binding list mode, it will generate a week function only in list
    if (IsStaticBindingListMode() && IsStaticBindingMethod(func->GetName())) {
      // get current func_ptr (strong/weak symbol address)
      auto *nativeFuncAddr = builder->CreateExprAddroffunc(nativeFunc->puIdx);
      funcptrAssign = builder->CreateStmtRegassign(PTY_ptr, funcptrPreg, nativeFuncAddr);
      func->body->AddStatement(funcptrAssign);
      // define wrapper function call
      StmtNode *wrapperCall = CreateNativeWrapperCallNode(func, readFuncPtr, args, ret);
      func->body->AddStatement(wrapperCall);
    } else if (!Options::regNativeDynamicOnly) {
      func->body->AddStatement(funcptrAssign);
      func->body->AddStatement(funcptrshiftAssign);
      // get find_native_func function
      MIRFunction *findNativeFunc =
        builder->GetOrCreateFunction(FIND_NATIVE_FUNC_NOEH, voidPtrTy->GetTypeIndex());
      findNativeFunc->SetAttr(FUNCATTR_nosideeffect);
      // CallAssigned statement for unregistered situation
      CallNode *callGetFindNativeFunc = builder->CreateStmtCallRegassigned(
        findNativeFunc->puIdx, dynamicStubOpnds, funcptrPreg, OP_callassigned);
      // check return value of dynamic linking stub
      MIRFunction *dummyNativeFunc =
        builder->GetOrCreateFunction(DUMMY_NATIVE_FUNC, voidPtrTy->GetTypeIndex());
      dummyNativeFunc->SetAttr(FUNCATTR_nosideeffect);
      auto dummyFuncPreg = func->pregTab->CreatePreg(PTY_ptr);
      auto readDummyFuncPtr = builder->CreateExprRegread(PTY_ptr, dummyFuncPreg);
      MapleVector<BaseNode *> dummyFuncOpnds(func->codeMemPoolAllocator.Adapter());
      CallNode *callDummyNativeFunc = builder->CreateStmtCallRegassigned(
          dummyNativeFunc->puIdx, dummyFuncOpnds, dummyFuncPreg, OP_callassigned);
      BaseNode *checkStubReturnExpr = builder->CreateExprCompare(
          OP_eq, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetPtr(), readFuncPtr, readDummyFuncPtr);
      IfStmtNode *subIfStmt = static_cast<IfStmtNode *>(builder->CreateStmtIf(checkStubReturnExpr));
      // assign with address of strong/weak symbol
      auto *nativeFuncAddr = builder->CreateExprAddroffunc(nativeFunc->puIdx);
      funcptrAssign = builder->CreateStmtRegassign(PTY_ptr, funcptrPreg, nativeFuncAddr);
      subIfStmt->thenPart->AddStatement(funcptrAssign);
      // rewrite reg_jni_func_tab with current func_ptr(weak/strong symbol address)
      auto nativeMethodPtr = builder->CreateExprRegread(PTY_ptr, funcptrPreg);
      IassignNode *nativeFuncTableEntry = builder->CreateStmtIassign(
          GlobalTables::GetTypeTable().GetOrCreatePointerType(elemtype), 0, arrayExpr, nativeMethodPtr);
      subIfStmt->thenPart->AddStatement(nativeFuncTableEntry);
      // Add if-statement to function body
      ifStmt->thenPart->AddStatement(callGetFindNativeFunc);
      ifStmt->thenPart->AddStatement(callDummyNativeFunc);
      ifStmt->thenPart->AddStatement(subIfStmt);
      if (needCheckThrowPendingExceptionFunc) {
        func->body->AddStatement(ifStmt);
        StmtNode *wrapperCall = CreateNativeWrapperCallNode(func, readFuncPtr, args, ret);
        func->body->AddStatement(wrapperCall);
      } else {
        StmtNode *wrapperCall = CreateNativeWrapperCallNode(func, readFuncPtr, args, ret);
        ifStmt->thenPart->AddStatement(wrapperCall);
        MapleVector<BaseNode *> getExceptArgs(func->codeMemPoolAllocator.Adapter());
        CallNode *callGetExceptFunc = builder->CreateStmtCallAssigned(
          MRTCheckThrowPendingExceptionFunc->puIdx, getExceptArgs, nullptr, OP_callassigned);
        ifStmt->thenPart->AddStatement(callGetExceptFunc);
        BlockNode *elseblock = func->codeMemPool->New<BlockNode>();
        ifStmt->elsePart = elseblock;
        ifStmt->numOpnds = 3;
        wrapperCall = CreateNativeWrapperCallNode(func, readFuncPtr, args, ret);
        elseblock->AddStatement(wrapperCall);
        func->body->AddStatement(ifStmt);
      }
    } else {
      func->body->AddStatement(funcptrAssign);
      func->body->AddStatement(funcptrshiftAssign);
      MIRFunction *findNativeFunc =
        builder->GetOrCreateFunction(FIND_NATIVE_FUNC, voidPtrTy->GetTypeIndex());
      findNativeFunc->SetAttr(FUNCATTR_nosideeffect);
      // CallAssigned statement for unregistered situation
      CallNode *callGetFindNativeFunc = builder->CreateStmtCallRegassigned(
        findNativeFunc->puIdx, dynamicStubOpnds, funcptrPreg, OP_callassigned);
      // Add if-statement to function body
      ifStmt->thenPart->AddStatement(callGetFindNativeFunc);
      if (!needCheckThrowPendingExceptionFunc) {
        MapleVector<BaseNode *> getExceptArgs(func->codeMemPoolAllocator.Adapter());
        CallNode *callGetExceptFunc = builder->CreateStmtCallAssigned(
          MRTCheckThrowPendingExceptionFunc->puIdx, getExceptArgs, nullptr, OP_callassigned);
        ifStmt->thenPart->AddStatement(callGetExceptFunc);
      }
      func->body->AddStatement(ifStmt);
      StmtNode *wrapperCall = CreateNativeWrapperCallNode(func, readFuncPtr, args, ret);
      func->body->AddStatement(wrapperCall);
    }
    return;
  }

  // without native wrapper
  // ICall Node
  IcallNode *icall = func->codeMemPool->New<IcallNode>(module, OP_icallassigned);
  icall->numOpnds = args.size() + 1;
  icall->nOpnd.resize(icall->numOpnds);
  icall->returnValues = returnValues;

  for (uint32 i = 1; i < icall->nOpnd.size(); i++) {
    icall->nOpnd[i] = args[i - 1]->CloneTree(module);
  }
  icall->nOpnd[0] = readFuncPtr;
  icall->retTyIdx = nativeFunc->GetReturnTyIdx();

  // Check if funcptr is Invalid
  MIRFunction *findNativeFunc =
    builder->GetOrCreateFunction(FIND_NATIVE_FUNC, voidPtrTy->GetTypeIndex());
  findNativeFunc->SetAttr(FUNCATTR_nosideeffect);
  // CallAssigned statement for unregistered situation
  CallNode *callGetFindNativeFunc = builder->CreateStmtCallRegassigned(
    findNativeFunc->puIdx, dynamicStubOpnds, funcptrPreg, OP_callassigned);
  ifStmt->thenPart->AddStatement(callGetFindNativeFunc);
  if (!needCheckThrowPendingExceptionFunc) {
    MapleVector<BaseNode *> getExceptArgs(func->codeMemPoolAllocator.Adapter());
    CallNode *callGetExceptFunc = builder->CreateStmtCallAssigned(
      MRTCheckThrowPendingExceptionFunc->puIdx, getExceptArgs, nullptr, OP_callassigned);
    ifStmt->thenPart->AddStatement(callGetExceptFunc);
  }
  func->body->AddStatement(ifStmt);
  func->body->AddStatement(icall);
}

// Use wrapper to call the native function, the logic is:
//     if func is fast_native or critical_native {
//      if num_of_args < 8 {
//        MCC_CallFastNative(nativeFunc, ...);
//      } else {
//        MCC_CallFastNativeExt(nativeFunc, num_of_args, ...);
//      }
//    } else {
//      if num_of_args < 8 {
//        MCC_CallSlowNative(nativeFunc, ...);
//      } else {
//        MCC_CallSlowNativeExt(nativeFunc, num_of_args, ...);
//      }
//    }
StmtNode *GenNativeStubFunc::CreateNativeWrapperCallNode(MIRFunction *func, BaseNode *funcPtr,
                                                         MapleVector<BaseNode *> &args, MIRSymbol *ret) {
  MIRFunction *wrapperFunc = nullptr;
  MapleVector<BaseNode *> wrapperArgs(func->codeMemPoolAllocator.Adapter());

  // the first arg is the natvie function pointer.
  wrapperArgs.push_back(funcPtr);

  // is fast native?
  auto isFast = (func->GetAttr(FUNCATTR_fast_native) || func->GetAttr(FUNCATTR_critical_native));

  // Do not need native wrapper for critical natives
  // if num_of_args < 8
  if (func->GetAttr(FUNCATTR_critical_native) && args.size() <= 7) {
    IcallNode *icall = func->codeMemPool->New<IcallNode>(module, OP_icallassigned);
    CallReturnVector returnValues(func->codeMemPoolAllocator.Adapter());
    if (ret) {
      CHECK_FATAL(ret->storageClass == kScAuto || ret->storageClass == kScFormal || ret->storageClass == kScExtern || ret->storageClass == kScGlobal,
             "");
      returnValues.push_back(CallReturnPair(ret->GetStIdx(), RegFieldPair(0, 0)));
    }

    icall->numOpnds = args.size() + 1;
    icall->nOpnd.resize(icall->numOpnds);
    icall->returnValues = returnValues;

    for (uint32 i = 1; i < icall->nOpnd.size(); i++) {
      icall->nOpnd[i] = args[i - 1]->CloneTree(module);
    }
    icall->nOpnd[0] = funcPtr;
    icall->retTyIdx = func->GetReturnTyIdx();
    return icall;
  }

  // if num of args > 8
  if (args.size() > 8) {
    wrapperFunc = isFast ? MRTCallFastNativeExtFunc : MRTCallSlowNativeExtFunc;
  } else if (isFast) {
    wrapperFunc = MRTCallFastNativeFunc;
  } else {
    wrapperFunc = MRTCallSlowNativeFunc[args.size()];
  }

  // push back all original args.
  wrapperArgs.insert(wrapperArgs.end(), args.begin(), args.end());

  // if no return (aka void)
  if (ret == nullptr) {
    // use 'call' statement if no return value.
    return builder->CreateStmtCall(wrapperFunc->puIdx, wrapperArgs);
  } else {
    // use 'callassigned' if the function has return value.
    return builder->CreateStmtCallAssigned(wrapperFunc->puIdx, wrapperArgs, ret, OP_callassigned);
  }
}

void GenNativeStubFunc::GenNativeWrapperFunctionCall(MIRFunction *func, const MIRFunction *nativeFunc,
                                                     MapleVector<BaseNode *> &args, MIRSymbol *ret) {
  func->body->AddStatement(
    CreateNativeWrapperCallNode(func, builder->CreateExprAddroffunc(nativeFunc->puIdx), args, ret));
}

TyIdx GenNativeStubFunc::GenNewStructType(MIRStructType &newtype, const std::string &str) {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(str);
  TyIdx tyIdx = GlobalTables::GetTypeTable().CreateMIRType(&newtype);
  module->typeNameTab->SetGStrIdxToTyIdx(strIdx, tyIdx);
  module->typeDefOrder.push_back(strIdx);
  CHECK(tyIdx.GetIdx() < GlobalTables::GetTypeTable().typeTable.size(), "index out of range in GenNativeStubFunc::GenNewStructType");
  if (GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]->nameStrIdx == GStrIdx(0)) {
    GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]->nameStrIdx = strIdx;
  }

  return tyIdx;
}

void GenNativeStubFunc::GenRegTableEntryType() {
  uint32_t arraySize;
  arraySize = 0;
  // Use MIRIntType instead of MIRStructType in RegTableEntry
  MIRArrayType *arraytype =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(GlobalTables::GetTypeTable().GetVoidPtr(), arraySize);
  regTableConst = module->memPool->New<MIRAggConst>(module, arraytype);
}

void GenNativeStubFunc::GenHelperFuncDecl() {
  MIRType *voidTy = GlobalTables::GetTypeTable().GetVoid();
  MIRType *voidPtrTy = GlobalTables::GetTypeTable().GetVoidPtr();
  MIRType *refTy = GlobalTables::GetTypeTable().GetRef();

  // MRT_PendingException
  MRTCheckThrowPendingExceptionFunc =
    builder->GetOrCreateFunction(CHECK_THROW_PENDING_EXCEPTION_FUNC, voidTy->GetTypeIndex());
  CHECK_FATAL(MRTCheckThrowPendingExceptionFunc,
         "MRTCheckThrowPendingExceptionFunc is null in GenNativeStubFunc::GenHelperFuncDecl");
  MRTCheckThrowPendingExceptionFunc->SetAttr(FUNCATTR_nosideeffect);
  MRTCheckThrowPendingExceptionFunc->body = nullptr;

  // MRT_PreNativeCall
  ArgVector preArgs(module->memPoolAllocator.Adapter());
  preArgs.push_back(ArgPair("caller", refTy));
  MRTPreNativeFunc = builder->CreateFunction(PRE_NATIVE_FUNC, voidPtrTy, preArgs);
  CHECK_FATAL(MRTPreNativeFunc, "MRTPreNativeFunc is null in GenNativeStubFunc::GenHelperFuncDecl");
  MRTPreNativeFunc->body = nullptr;

  // MRT_PostNativeCall
  ArgVector postArgs(module->memPoolAllocator.Adapter());
  postArgs.push_back(ArgPair("env", voidPtrTy));
  MRTPostNativeFunc = builder->CreateFunction(POST_NATIVE_FUNC, voidTy, postArgs);
  CHECK_FATAL(MRTPostNativeFunc, "MRTPostNativeFunc is null in GenNativeStubFunc::GenHelperFuncDecl");
  MRTPostNativeFunc->body = nullptr;

  // MRT_DecodeReference
  ArgVector decodeArgs(module->memPoolAllocator.Adapter());
  decodeArgs.push_back(ArgPair("obj", refTy));
  MRTDecodeRefFunc = builder->CreateFunction(DECODE_REF_FUNC, refTy, decodeArgs);
  CHECK_FATAL(MRTDecodeRefFunc, "MRTDecodeRefFunc is null in GenNativeStubFunc::GenHelperFuncDecl");
  MRTDecodeRefFunc->SetAttr(FUNCATTR_nosideeffect);
  MRTDecodeRefFunc->body = nullptr;

  // MCC_CallFastNative
  ArgVector callArgs(module->memPoolAllocator.Adapter());
  callArgs.push_back(ArgPair("func", voidPtrTy));
  MRTCallFastNativeFunc = builder->CreateFunction(CALL_FAST_NATIVE_FUNC, voidPtrTy, callArgs);
  CHECK_FATAL(MRTCallFastNativeFunc, "MRTCallFastNativeFunc is null in GenNativeStubFunc::GenHelperFuncDecl");
  MRTCallFastNativeFunc->body = nullptr;

  // MCC_CallSlowNative
  for (int i = 0; i < SLOWNATIVEFUNCNUM; i++) {
    MRTCallSlowNativeFunc[i] = builder->CreateFunction(kCallSlowNativeFuncs[i], voidPtrTy, callArgs);
    CHECK_FATAL(MRTCallSlowNativeFunc[i], "MRTCallSlowNativeFunc is null in GenNativeStubFunc::GenHelperFuncDecl");
    MRTCallSlowNativeFunc[i]->body = nullptr;
  }

  // MCC_CallFastNativeExt
  ArgVector callExtArgs(module->memPoolAllocator.Adapter());
  callExtArgs.push_back(ArgPair("func", voidPtrTy));
  MRTCallFastNativeExtFunc = builder->CreateFunction(CALL_FAST_NATIVE_EXT_FUNC, voidPtrTy, callExtArgs);
  CHECK_FATAL(MRTCallFastNativeExtFunc != nullptr,
         "MRTCallFastNativeExtFunc is null in GenNativeStubFunc::GenHelperFuncDecl");
  MRTCallFastNativeExtFunc->body = nullptr;

  // MCC_CallSlowNativeExt
  MRTCallSlowNativeExtFunc = builder->CreateFunction(CALL_SLOW_NATIVE_EXT_FUNC, voidPtrTy, callExtArgs);
  CHECK_FATAL(MRTCallSlowNativeExtFunc != nullptr,
         "MRTCallSlowNativeExtFunc is null in GenNativeStubFunc::GenHelperFuncDecl");
  MRTCallSlowNativeExtFunc->body = nullptr;

  // MCC_SetReliableUnwindContext
  MCCSetReliableUnwindContextFunc =
    builder->GetOrCreateFunction(SET_RELIABLE_UNWIND_CONTEXT_FUNC, voidTy->GetTypeIndex());
  CHECK_FATAL(MCCSetReliableUnwindContextFunc,
         "MCCSetReliableUnwindContextFunc is null in GenNativeStubFunc::GenHelperFuncDecl");
  MCCSetReliableUnwindContextFunc->SetAttr(FUNCATTR_nosideeffect);
  MCCSetReliableUnwindContextFunc->body = nullptr;
}

void GenNativeStubFunc::GenRegTable() {
  MIRArrayType *arraytype = static_cast<MIRArrayType *>(regTableConst->type);
  arraytype->sizeArray[0] = regTableConst->constVec.size();
  std::string regJniTabName = NameMangler::kRegJNITabPrefixStr + module->GetFileNameAsPostfix();

  MIRSymbol *regJniSt = builder->CreateGlobalDecl(regJniTabName, regTableConst->type, kScGlobal);
  regJniSt->SetConst(regTableConst);
}

bool GenNativeStubFunc::IsStaticBindingListMode() const {
  return (Options::staticBindingList != "" && Options::staticBindingList.length());
}

void GenNativeStubFunc::InitStaticBindingMethodList() {
  if (!IsStaticBindingListMode()) {
    return;
  }

  fstream file(Options::staticBindingList.c_str());
  string content;
  while (std::getline(file, content)) {
    staticBindingMethodsSet.insert(content);
  }
}

bool GenNativeStubFunc::IsStaticBindingMethod(const std::string &methodName) {
  return (staticBindingMethodsSet.find(NameMangler::NativeJavaName(methodName.c_str())) !=
          staticBindingMethodsSet.end());
}

bool GenNativeStubFunc::GetNativeFuncProperty(const std::string &funcName, NativeFuncProperty &property) {
  auto it = nativeFuncProperty.find(funcName);
  if (it != nativeFuncProperty.end()) {
    property = it->second;
    return true;
  }

  return false;
}

void GenNativeStubFunc::LoadNativeFuncProperty() {
  const string kNativeFuncPropertyFile = Options::nativeFuncPropertyFile;

  ifstream in(kNativeFuncPropertyFile.c_str(), ios::in | ios::binary);
  if (!in.is_open()) {
    cerr << "Open native function property file failed: " << kNativeFuncPropertyFile << endl;
    return;
  }

  // mangled name of java function\tfile name of native function\tjni function\tenv be used flag\tthrow exception flag\n
  // System_currentTimeMillis    0   0
  while (!in.eof()) {
    NativeFuncProperty property;
    in >> property.javaFunc >> property.nativeFile >> property.nativeFunc >> property.jniType;

    if (!property.javaFunc.empty()) {
      nativeFuncProperty[property.javaFunc] = property;
    }
  }
}

void GenNativeStubFunc::LoadNativeData(const std::string &file, std::unordered_set<std::string> &preloadSet) {
  std::ifstream infile;
  infile.open(file);
  if (infile.fail()) {
    cerr << "Cannot open native stub file " << file << "\n";
    return;
  }

  std::string name;
  while (infile >> name) {
    if (name.empty()) {
      continue;
    }
    preloadSet.insert(name);
  }
  infile.close();
}

bool GenNativeStubFunc::IsManualFastNative(const std::string &funcName) {
  return (manualFastNativeSet.find(funcName) != manualFastNativeSet.end());
}

bool GenNativeStubFunc::IsManualCriticalNative(const std::string &funcName) {
  return (manualCriticalNativeSet.find(funcName) != manualCriticalNativeSet.end());
}

void GenNativeStubFunc::InitManualFastAndCriticalNative() {
  LoadNativeData(Options::criticalNativeFile, manualCriticalNativeSet);
  LoadNativeData(Options::fastNativeFile, manualFastNativeSet);
}

void GenNativeStubFunc::Finish() {
  if (regTableConst->constVec.size() > 0) {
    GenRegTable();
    GenRegFuncTab();
  }
  if (!Options::mapleLinker) {
    // If use maplelinker, we postpone this generation to MUIDReplacement
    ReflectionAnalysis::GenStrTab(module);
  }
}

}  // namespace maple
