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

#include <iostream>
#include <algorithm>
#include "gen_check_cast.h"
#include "reflection_analysis.h"

// This phase does two things:
// #1 implement the opcode check-cast vx, type_id
//    according to the check-cast definition:
//    Checks whether the object reference in vx can be cast
//    to an instance of a class referenced by type_id.
//    Throws ClassCastException if the cast is not possible, continues execution otherwise.
//    in our case check if object can be cast or insert MCC_Reflect_ThrowCastException
//    before the stmt.
// #2 optimise opcode instance-of vx,vy,type_id
//    If target class is is final class or is private and inner class and has no subclass,it
//    means there can't be subclass of target class,so if  a obj is instance of target
//    class , the obj must be referred the target class,replace the instance-of with
//    maple IR,the IR do the things check if obj.getClass() == target_class,below is the detail
//    suppose the obj is %obj, target-class is T,result is saved in reg %1
//    regassign u1 %1 (constVal u1 0)
//    brfalse (ne u1 ptr (regread ref %obj, constVal ptr 0))  #check if obj is null
//    #check if obj's class is equal the target class ,if equal set the result 1
//    brflase (eq u1 ptr(
//      iread ptr <* <$Ljava_2Flang_2FObject_3B>> 1 (regread ref %obj),
//      addrof ptr T))
//    regassign u1 %1 (constVal u1 1)

namespace maple {

CheckCastGeneration::CheckCastGeneration(MIRModule *mod, KlassHierarchy *kh, bool dump)
  : FuncOptimizeImpl(mod, kh, dump) {
  InitTypes();
  InitFuncs();
}

void CheckCastGeneration::InitTypes() {
  mPtrObjType = GlobalTables::GetTypeTable().GetOrCreatePointerType(WKTypes::Util::GetJavaLangObjectType());
}

void CheckCastGeneration::InitFuncs() {
  mThrowCastException = builder->GetOrCreateFunction("MCC_Reflect_ThrowCastException", TyIdx(PTY_void));
  mThrowCastException->SetAttr(FUNCATTR_nosideeffect);

  mCheckCastingNoArray = builder->GetOrCreateFunction("MCC_Reflect_Check_Casting_NoArray", TyIdx(PTY_void));
  mCheckCastingNoArray->SetAttr(FUNCATTR_nosideeffect);

  mCheckCastingArray = builder->GetOrCreateFunction("MCC_Reflect_Check_Casting_Array", TyIdx(PTY_void));
  mCheckCastingArray->SetAttr(FUNCATTR_nosideeffect);
}

MIRSymbol *CheckCastGeneration::GetOrCreateClassinfoSymbol(const std::string &className) {
  std::string classinfoName = CLASSINFO_PREFIX_STR + className;
  MIRType *classType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(className)));
  MIRStorageClass storageClass = (classType && static_cast<MIRClassType *>(classType)->IsLocal()) ? kScGlobal : kScExtern;
  return builder->GetOrCreateGlobalDecl(classinfoName, GlobalTables::GetTypeTable().GetPtr(), storageClass);
}

void CheckCastGeneration::GenCheckCast(BaseNode *stmt, BaseNode *latestInstanceOfStmt, StIdx lastOpndStidx) {
  // handle the special case like (Type)null, we dont need a checkcast.
  if (stmt->op == OP_intrinsiccallwithtypeassigned) {
    IntrinsiccallNode *callnode = static_cast<IntrinsiccallNode *>(stmt);
    ASSERT(callnode->nOpnd.size() == 1, "");
    BaseNode *opnd = callnode->Opnd(0);
    if (opnd->op == OP_constval) {
      const int callnodeNretsSize = callnode->returnValues.size();
      CHECK_FATAL(callnodeNretsSize > 0, "container check");
      CallReturnPair callretpair = callnode->returnValues[0];
      StmtNode *assignretypenode = nullptr;
      if (!callretpair.second.IsReg())
        assignretypenode = builder->CreateStmtDassign(callretpair.first, callretpair.second.GetFieldid(), opnd);
      else {
        PregIdx pregidx = callretpair.second.GetPregidx();
        MIRPreg *mirpreg = currFunc->pregTab->PregFromPregIdx(pregidx);
        assignretypenode = builder->CreateStmtRegassign(mirpreg->primType, pregidx, opnd);
      }
      currFunc->body->ReplaceStmt1WithStmt2(static_cast<StmtNode *>(stmt), assignretypenode);
      return;
    }
  }
  // Do type check first
  IntrinsiccallNode *callNode = static_cast<IntrinsiccallNode *>(stmt);
  TyIdx checkTyidx = callNode->tyIdx;
  MIRType *checkType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(checkTyidx);
  Klass *checkKlass = klassHierarchy->GetKlassFromTyidx(static_cast<MIRPtrType *>(checkType)->pointedTyIdx);

  bool hasChecked = false;
  if (latestInstanceOfStmt != nullptr) {
    IntrinsicopNode *latestInstanceOfNode = static_cast<IntrinsicopNode *>(latestInstanceOfStmt);
    if (latestInstanceOfNode->tyIdx == checkTyidx) {
      const int latestInstanceOfNodeNopndSize = latestInstanceOfNode->nOpnd.size();
      CHECK_FATAL(latestInstanceOfNodeNopndSize > 0, "contain check");
      AddrofNode *instanceOfNode = static_cast<AddrofNode *>(latestInstanceOfNode->nOpnd[0]);
      const int callNodeNopndSize = callNode->nOpnd.size();
      CHECK_FATAL(callNodeNopndSize > 0, "contain check");
      AddrofNode *castNode = static_cast<AddrofNode *>(callNode->nOpnd[0]);
      if ((instanceOfNode->stIdx.Idx() == castNode->stIdx.Idx()) || (lastOpndStidx.Idx() == castNode->stIdx.Idx())) {
        hasChecked = true;
      }
    }
  }

  if (!hasChecked) {
    if (checkKlass && strcmp("", checkKlass->GetKlassName().c_str())) {
      if (!strcmp(checkKlass->GetKlassName().c_str(), NameMangler::kJavaLangObjectStr)) {
        const int callNodeNopndSize1 = callNode->nOpnd.size();
        CHECK_FATAL(callNodeNopndSize1 > 0, "container check");
        if (callNode->nOpnd[0]->primType != PTY_ref && callNode->nOpnd[0]->primType != PTY_ptr) {
          MapleVector<BaseNode *> opnds(currFunc->codeMemPoolAllocator.Adapter());
          MIRSymbol *classSt = GetOrCreateClassinfoSymbol(checkKlass->GetKlassName());
          BaseNode *valueExpr = builder->CreateExprAddrof(0, classSt);
          opnds.push_back(valueExpr);
          opnds.push_back(callNode->nOpnd[0]);
          opnds.push_back(builder->CreateIntConst(0, PTY_ptr));
          StmtNode *dassignStmt = builder->CreateStmtCall(mThrowCastException->puIdx, opnds, OP_call);
          currFunc->body->InsertBefore(static_cast<StmtNode *>(stmt), dassignStmt);
        }
      } else {
        MapleVector<BaseNode *> opnds(currFunc->codeMemPoolAllocator.Adapter());
        MIRSymbol *classSt = GetOrCreateClassinfoSymbol(checkKlass->GetKlassName());
        BaseNode *valueExpr = builder->CreateExprAddrof(0, classSt);
        BaseNode *nullPtr = builder->CreateIntConst(0, PTY_ptr);
        const int callNodeNopndSize2 = callNode->nOpnd.size();
        CHECK_FATAL(callNodeNopndSize2 > 0, "container check");

        BaseNode *cond = builder->CreateExprCompare(
            OP_ne, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetPtr(), callNode->nOpnd[0], nullPtr);
        IfStmtNode *ifStmt = static_cast<IfStmtNode *>(builder->CreateStmtIf(cond));

        MIRType *mVoidPtr = GlobalTables::GetTypeTable().GetVoidPtr();
        CHECK_FATAL(mVoidPtr != nullptr, "builder->GetVoidPtr() is null in CheckCastGeneration::GenCheckCast");
        MIRSymbol *retst = builder->GetOrCreateLocalDecl("_retst", mVoidPtr);
        BaseNode *retVal = builder->CreateExprDread(retst);
        CHECK_FATAL(callNode->nOpnd[0]->op == OP_dread, "expect dread node for MCC_Reflect_Check_Casting_NoArray");
        BaseNode *ireadExpr = GetObjectShadow(callNode->nOpnd[0]);

        StmtNode *dassignTaget = builder->CreateStmtDassign(retst, 0, ireadExpr);

        BaseNode *innerCond =
            builder->CreateExprCompare(OP_ne, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetPtr(), valueExpr, retVal);
        IfStmtNode *innerIfStmt = static_cast<IfStmtNode *>(builder->CreateStmtIf(innerCond));

        opnds.push_back(valueExpr);
        opnds.push_back(callNode->nOpnd[0]);
        StmtNode *dassignStmt = builder->CreateStmtCall(mCheckCastingNoArray->puIdx, opnds, OP_call);

        innerIfStmt->thenPart->AddStatement(dassignStmt);
        ifStmt->thenPart->AddStatement(dassignTaget);
        ifStmt->thenPart->AddStatement(innerIfStmt);

        currFunc->body->InsertBefore(static_cast<StmtNode *>(stmt), ifStmt);
      }
    } else {
      MIRType *pointedType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(checkType)->pointedTyIdx);
      if (pointedType->GetKind() == kTypeJArray) {
        // java array
        MapleVector<BaseNode *> opnds(currFunc->codeMemPoolAllocator.Adapter());
        MIRJarrayType *jarrayType = static_cast<MIRJarrayType *>(pointedType);
        string arrayname = jarrayType->GetJavaName();
        int dim = 0;
        while (arrayname[dim] == 'A') {
          dim++;
        }
        MIRSymbol *elemClassSt = nullptr;
        string elementname = arrayname.substr(dim, arrayname.size() - dim);
        MIRType *mVoidPtr2 = GlobalTables::GetTypeTable().GetVoidPtr();
        CHECK_FATAL(mVoidPtr2 != nullptr, "null ptr check");
        if (elementname == "I" || elementname == "F" || elementname == "B" || elementname == "C" ||
            elementname == "S" || elementname == "J" || elementname == "D" || elementname == "Z" ||
            elementname == "V") {
          string primClassinfoName = PRIMITIVECLASSINFO_PREFIX_STR + elementname;
          builder->GlobalLock();
          elemClassSt = builder->GetGlobalDecl(primClassinfoName);
          if (elemClassSt == nullptr) {
            elemClassSt = builder->CreateGlobalDecl(primClassinfoName, GlobalTables::GetTypeTable().GetPtr(), kScGlobal);
          }
          builder->GlobalUnlock();
        } else {
          elemClassSt = GetOrCreateClassinfoSymbol(elementname);
        }
        BaseNode *valueExpr = builder->CreateExprAddrof(0, elemClassSt);
        builder->GlobalLock();
        UStrIdx strIdx = GlobalTables::GetUStrTable().GetOrCreateStrIdxFromName(jarrayType->GetJavaName());
        builder->GlobalUnlock();
        ConststrNode *signatureNode = currFunc->codeMemPool->New<ConststrNode>(strIdx);
        signatureNode->primType = PTY_ptr;
        opnds.push_back(valueExpr);
        const int callNodeNopndSize3 = callNode->nOpnd.size();
        CHECK_FATAL(callNodeNopndSize3 > 0, "container check");
        opnds.push_back(callNode->nOpnd[0]);
        opnds.push_back(builder->CreateIntConst(dim, PTY_ptr));
        opnds.push_back(signatureNode);
        StmtNode *dassignStmt = builder->CreateStmtCall(mCheckCastingArray->puIdx, opnds, OP_call);
        currFunc->body->InsertBefore(static_cast<StmtNode *>(stmt), dassignStmt);
      } else {
        MIRTypeKind kd = pointedType->GetKind();
        if (kd == kTypeStructIncomplete || kd == kTypeClassIncomplete || kd == kTypeInterfaceIncomplete) {
          LogInfo::MapleLogger() << "Warining: CheckCastGeneration::GenCheckCast "
                    << GlobalTables::GetStrTable().GetStringFromStrIdx(pointedType->nameStrIdx) << " INCOMPLETE " << std::endl;
        } else {
          CHECK_FATAL(false, "unsupport kind");
        }
      }
    }
  }
  if (callNode->op == OP_intrinsiccallwithtype) {
    return;
  }

  IntrinsiccallNode *callnode = static_cast<IntrinsiccallNode *>(stmt);
  ASSERT(callnode->nOpnd.size() == 1, "");
  BaseNode *opnd = callnode->Opnd(0);
  ASSERT(opnd->op == OP_dread || opnd->op == OP_regread || opnd->op == OP_iread || opnd->op == OP_retype, "");

  MIRType *fromType = nullptr;
  if (opnd->op == OP_dread) {
    fromType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(
      currFunc->GetLocalOrGlobalSymbol(static_cast<AddrofNode *>(opnd)->stIdx)->GetTyIdx());
  } else if (opnd->op == OP_retype) {
    fromType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<RetypeNode *>(opnd)->tyIdx);
  } else if (opnd->op == OP_iread) {
    IreadNode *irnode = static_cast<IreadNode *>(opnd);
    MIRPtrType *pointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(irnode->tyIdx));
    if (irnode->fieldID != 0) {
      MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
      MIRStructType *structty = nullptr;
      if (pointedty->GetKind() != kTypeJArray) {
        structty = static_cast<MIRStructType *>(pointedty);
      } else {
        // it's a Jarray type. using it's parent's field info: java.lang.Object
        structty = static_cast<MIRJarrayType *>(pointedty)->GetParentType();
      }
      CHECK_FATAL(structty, "null ptr check");
      FieldPair thepair = structty->TraverseToField(irnode->fieldID);
      fromType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
    } else {
      fromType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
    }
  } else if (opnd->op == OP_regread) {
    RegreadNode *regreadnode = static_cast<RegreadNode *>(opnd);
    MIRPreg *mirpreg = currFunc->pregTab->PregFromPregIdx(regreadnode->regIdx);
    CHECK_FATAL(mirpreg->primType == PTY_ref || mirpreg->primType == PTY_ptr, "must be reference or ptr type for preg");
    if (opnd->primType == PTY_ref) {
      fromType = mirpreg->mirType;
    } else {
      const int gTypeTableSize = GlobalTables::GetTypeTable().typeTable.size();
      CHECK_FATAL(gTypeTableSize > PTY_ptr, "container check");
      fromType = GlobalTables::GetTypeTable().typeTable[PTY_ptr];
    }
  }
  MIRType *toType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(callnode->tyIdx);
  ASSERT((fromType->primType == maple::PTY_ptr || fromType->primType == maple::PTY_ref) &&
            (toType->primType == maple::PTY_ptr || toType->primType == maple::PTY_ref),
          "");
  const int callnodeNretSize1 = callnode->returnValues.size();
  CHECK_FATAL(callnodeNretSize1 > 0, "container check");
  CallReturnPair callretpair = callnode->returnValues[0];
  StmtNode *assignretypenode = nullptr;
  if (!callretpair.second.IsReg())
    assignretypenode = builder->CreateStmtDassign(callretpair.first, callretpair.second.GetFieldid(), opnd);
  else {
    PregIdx pregidx = callretpair.second.GetPregidx();
    MIRPreg *mirpreg = currFunc->pregTab->PregFromPregIdx(pregidx);
    assignretypenode = builder->CreateStmtRegassign(mirpreg->primType, pregidx, opnd);
  }

  currFunc->body->ReplaceStmt1WithStmt2(static_cast<StmtNode *>(stmt), assignretypenode);
}

bool CheckCastGeneration::FindDef(BaseNode *x, MIRSymbol *symbol) {
  if (x == nullptr) {
    return false;
  }
  Opcode op = x->op;
  switch (op) {
    case OP_call:
    case OP_virtualcall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_customcall:
    case OP_polymorphiccall:
    case OP_icall:
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_intrinsiccallwithtype:
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_icallassigned:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned:
    case OP_callinstant:
    case OP_virtualcallinstant:
    case OP_superclasscallinstant:
    case OP_interfacecallinstant:
    case OP_callinstantassigned:
    case OP_virtualcallinstantassigned:
    case OP_superclasscallinstantassigned:
    case OP_interfacecallinstantassigned: {
      CallNode *cnode = static_cast<CallNode *>(x);
      if (cnode) {
        CallReturnVector &returnValues = cnode->returnValues;
        MIRSymbol *retSt = nullptr;
        if (returnValues.size() == 0 || !kOpcodeInfo.IsCallAssigned(x->op)) {
          retSt = nullptr;
        } else {
          ASSERT(returnValues.size() == 1, "Single Ret value for now.");
          StIdx stIdx = returnValues[0].first;
          RegFieldPair regfieldpair = returnValues[0].second;
          if (!regfieldpair.IsReg()) {
            retSt = currFunc->GetLocalOrGlobalSymbol(stIdx);
          }
        }
        if (retSt == symbol) {
          return true;
        }
      }
    } break;
    case OP_maydassign:
    case OP_dassign: {
      DassignNode *dnode = static_cast<DassignNode *>(x);
      MIRSymbol *st = currFunc->GetLocalOrGlobalSymbol(dnode->stIdx, true);
      if (symbol == st) {
        return true;
      }
      if (dnode->GetRhs()) {
        return FindDef(dnode->GetRhs(), st);
      }
    } break;
    case OP_regassign: {
      RegassignNode *node = static_cast<RegassignNode *>(x);
      if (FindDef(node->uOpnd, symbol)) {
        return true;
      }
    } break;
    default:;
  }
  return false;
}

void CheckCastGeneration::GenAllCheckCast() {
  BaseNode *latestInstanceOfStmt = nullptr;
  StIdx lastOpndStidx;
  for (StmtNode *stmt = currFunc->body->GetFirst(), *next = nullptr; stmt; stmt = next) {
    next = stmt->GetNext();
    if (stmt->op == OP_dassign) {
      for (int32 i = 0; i < stmt->NumOpnds(); i++) {
        BaseNode *opnd = stmt->Opnd(i);
        IntrinsicopNode *callnode =
          (opnd->op == OP_intrinsicopwithtype) ? static_cast<IntrinsicopNode *>(opnd) : nullptr;
        if (callnode && callnode->intrinsic == INTRN_JAVA_INSTANCE_OF) {
          latestInstanceOfStmt = static_cast<StmtNode *>(opnd);
          break;
        } else if ((latestInstanceOfStmt != nullptr) && (latestInstanceOfStmt->op == OP_intrinsicopwithtype)) {
          IntrinsicopNode *latestInstanceOfNode = static_cast<IntrinsicopNode *>(latestInstanceOfStmt);
          DreadNode *instanceOfNode = static_cast<DreadNode *>(latestInstanceOfNode->nOpnd[0]);
          DassignNode *dnode = static_cast<DassignNode *>(stmt);
          if (dnode->GetRhs()) {
            DreadNode *rhs = static_cast<DreadNode *>(dnode->GetRhs());
            if (rhs) {
              if (rhs->stIdx.Idx() == instanceOfNode->stIdx.Idx()) {
                lastOpndStidx = dnode->stIdx;
              }
            }
          }
          MIRSymbol *st = currFunc->GetLocalOrGlobalSymbol(instanceOfNode->stIdx, true);
          if (FindDef(stmt, st)) {
            latestInstanceOfStmt = nullptr;
            break;
          }
        }
      }
    }
    if (stmt->op == OP_intrinsiccallwithtypeassigned || stmt->op == OP_intrinsiccallwithtype) {
      IntrinsiccallNode *callnode = static_cast<IntrinsiccallNode *>(stmt);
      if (callnode && callnode->intrinsic == INTRN_JAVA_CHECK_CAST) {
        GenCheckCast(stmt, latestInstanceOfStmt, lastOpndStidx);
        if (stmt->op == OP_intrinsiccallwithtype) {
          currFunc->body->RemoveStmt(stmt);
        }
      }
    }
  }
}

BaseNode *CheckCastGeneration::GetObjectShadow(BaseNode *opnd) {
  FieldID fldid =
      builder->GetStructFieldIdFromFieldNameParentFirst(WKTypes::Util::GetJavaLangObjectType(), NameMangler::kShadowClassName);
  BaseNode *ireadExpr = builder->CreateExprIread(GlobalTables::GetTypeTable().GetPtr(), mPtrObjType, fldid, opnd);
  return ireadExpr;
}

/* use "obj.getClass() == targetclass" replace instanceof if target class is final */
void CheckCastGeneration::ReplaceInstanceof(StmtNode *stmt, const MIRClassType *targetClassType,
                                           const IntrinsicopNode *intrinsicNode) {
  BaseNode *opnd = intrinsicNode->nOpnd[0];
  std::string className = GlobalTables::GetStrTable().GetStringFromStrIdx(targetClassType->nameStrIdx);
  StmtNode *comnode = stmt->GetPrev();
  if (comnode && comnode->op == OP_comment) {
    CommentNode *commentnode = static_cast<CommentNode *>(comnode);
    commentnode->comment += "\n  #" + className + " is final class or is private and inner class and has no subclass";
  }

  MIRSymbol *targetClassSy = GetOrCreateClassinfoSymbol(className);
  BaseNode *targetClassNode = builder->CreateExprAddrof(0, targetClassSy);

  MIRSymbol *instanceOfRet = builder->GetOrCreateLocalDecl("_instanceOfRet", GlobalTables::GetTypeTable().GetUInt1());
  ConstvalNode *falseVal = builder->GetConstUInt1(false);
  StmtNode *initRet = builder->CreateStmtDassign(instanceOfRet, 0, falseVal);
  currFunc->body->InsertBefore(stmt, initRet);

  BaseNode *ireadExpr = GetObjectShadow(opnd);
  MIRSymbol *objectClassSym = builder->GetOrCreateLocalDecl("_objectClassSym", GlobalTables::GetTypeTable().GetPtr());
  StmtNode *dassignObjectClassSym = builder->CreateStmtDassign(objectClassSym, 0, ireadExpr);

  BaseNode *nullPtr = builder->CreateIntConst(0, PTY_ptr);
  BaseNode *cond = builder->CreateExprCompare(OP_ne, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetPtr(), opnd, nullPtr);
  ConstvalNode *trueVal = builder->GetConstUInt1(true);
  StmtNode *retNode = builder->CreateStmtDassign(instanceOfRet, 0, trueVal);

  BaseNode *innerCond = builder->CreateExprCompare(OP_eq, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetPtr(),
                                                    builder->CreateExprDread(objectClassSym), targetClassNode);
  IfStmtNode *innerIfStmt = static_cast<IfStmtNode *>(builder->CreateStmtIf(innerCond));
  innerIfStmt->thenPart->AddStatement(retNode);

  IfStmtNode *ifStmt = static_cast<IfStmtNode *>(builder->CreateStmtIf(cond));
  ifStmt->thenPart->AddStatement(dassignObjectClassSym);
  ifStmt->thenPart->AddStatement(innerIfStmt);
  currFunc->body->InsertBefore(stmt, ifStmt);

  DassignNode *dsnode = static_cast<DassignNode *>(stmt);
  dsnode->SetRhs(builder->CreateExprDread(instanceOfRet));
}

void CheckCastGeneration::CheckInstanceof(StmtNode *stmt, IntrinsicopNode *intrinsicNode) {
  MIRType *targetClassType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(intrinsicNode->tyIdx);
  MIRPtrType *ptrType = static_cast<MIRPtrType *>(targetClassType);
  MIRType *ptype = ptrType->GetPointedType();
  if (ptype->typeKind == kTypeClass) {
    MIRClassType *classType = static_cast<MIRClassType *>(ptype);
    Klass *clazz = klassHierarchy->GetKlassFromTyidx(classType->tyIdx);
    if (ReflectionAnalysis::IsFinalClass(classType) || (clazz && clazz->IsPrivateInnerAndNoSubClass())) {
      ReplaceInstanceof(stmt, classType, intrinsicNode);
    }
  }
}

void CheckCastGeneration::OptimizeInstanceof() {
  StmtNode *stmt = currFunc->body->GetFirst();
  StmtNode *next = nullptr;
  while (stmt) {
    next = stmt->GetNext();
    Opcode op = stmt->op;
    if (op == OP_dassign) {
      DassignNode *dsnode = static_cast<DassignNode *>(stmt);
      if (dsnode->GetRhs()->op == OP_intrinsicopwithtype) {
        IntrinsicopNode *intrinsicNode = static_cast<IntrinsicopNode *>(dsnode->GetRhs());
        std::string intrinsicName = GetIntrinsicName(intrinsicNode->intrinsic);
        if (intrinsicName == "JAVA_INSTANCE_OF") {
          CheckInstanceof(stmt, intrinsicNode);
        }
      }
    }
    stmt = next;
  }
}

void CheckCastGeneration::ProcessFunc(MIRFunction *func) {
  if (func->IsEmpty()) {
    return;
  }

  SetCurrentFunction(func);
  GenAllCheckCast();
  /* optimize instanceof */
  /* do it after CheckCast */
  OptimizeInstanceof();
}

}  // namespace maple
