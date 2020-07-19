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

#include "decouple.h"
#include "reflection_analysis.h"
#include <algorithm>

namespace maple {

DeCouple::DeCouple(MIRModule *mod, KlassHierarchy *kh, bool dump) : FuncOptimizeImpl(mod, kh, dump){
  GenOffsetTableType();
  GenNonDeCoupleClass();
}

void DeCouple::GenLocalClassinfo() {
  uint32 arraySize = 0;
  MIRType *type = GlobalTables::GetTypeTable().GetVoidPtr();
  MIRArrayType *arraytype = GlobalTables::GetTypeTable().GetOrCreateArrayType(type, arraySize);
  MIRAggConst *newconst = module->memPool->New<MIRAggConst>(module, arraytype);
  for (Klass *klass : klassHierarchy->GetTopoSortedKlasses()) {
    MIRStructType *structtype = klass->GetMIRClassType();
    if (!structtype->IsLocal()) {
      continue;
    }
    MIRSymbol *sym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(
      GlobalTables::GetStrTable().GetStrIdxFromName(std::string(CLASSINFO_PREFIX_STR) + klass->GetKlassName()));
    if (!NeedGenOffset(klass->GetKlassName())) {
      continue;
    }
    AddrofNode *classInfo = builder->CreateExprAddrof(0, sym, module->memPool);
    MIRType *ptrtype = GlobalTables::GetTypeTable().typeTable[PTY_ptr];
    MIRConst *classconst = module->memPool->New<MIRAddrofConst>(classInfo->stIdx, classInfo->fieldID, ptrtype);
    newconst->constVec.push_back(classconst);
  }
  arraySize = newconst->constVec.size();
  arraytype->sizeArray[0] = arraySize;
  std::string symName = NameMangler::kLocalClassInfoStr + module->GetFileNameAsPostfix();
  MIRSymbol *constV = builder->CreateGlobalDecl(symName, arraytype, kScGlobal);
  constV->SetConst(newconst);
}

void DeCouple::GenNonDeCoupleClass() {
  // We could consider adding some interfaces here, e.g. Ljava_2Flang_2FCloneable_3B
  vector<std::string> initClass = { { "AB", "AC", "AD", "AF", "AI", "AJ", "AS", "AZ", NameMangler::kJavaLangObjectStr } };
  for (auto item : initClass) {
    nonDeCoupleClass.insert(item);
  }

  for (Klass *klass : klassHierarchy->GetTopoSortedKlasses()) {
    MIRStructType *structtype = klass->GetMIRClassType();
    if (!structtype->IsLocal()) {
      continue;
    }

    bool toBeInserted = true;
    for (const Klass *parent : klass->GetSuperKlasses()) {
      if (nonDeCoupleClass.count(parent->GetKlassName()) == 0) {
        // Has a parent not to be excluded
        toBeInserted = false;
        break;
      }
    }
    if (toBeInserted) {
      for (const Klass *interface : klass->GetImplInterfaces()) {
        if (nonDeCoupleClass.count(interface->GetKlassName()) == 0) {
          // Implements an interface not to be excluded
          toBeInserted = false;
          break;
        }
      }
    }
    if (toBeInserted) {
      nonDeCoupleClass.insert(klass->GetKlassName());
      klass->SetNeedDecoupling(false);
    }
  }
}

void DeCouple::GenOffsetTableType() {
  uint32 arraySize;
  arraySize = 0;
  // Create Field_offset_table key table
  {
    FieldVector parentFields;
    FieldVector fields;
    GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "classSym", GlobalTables::GetTypeTable().GetVoidPtr());
    GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "fieldIndex", GlobalTables::GetTypeTable().GetUInt32());
    GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "callIndex", GlobalTables::GetTypeTable().GetUInt32());
    fieldOffsetTableKeyType = static_cast<MIRStructType *>(
        GlobalTables::GetTypeTable().GetOrCreateStructType("FieldOffsetTableKey", fields, parentFields, module));
    arrayTypeOfFieldKey =
        GlobalTables::GetTypeTable().GetOrCreateArrayType(fieldOffsetTableKeyType,arraySize);
  }
  // Create Vtable_offset_table key table
  {
    FieldVector parentFields;
    FieldVector fields;
    GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "classSym", GlobalTables::GetTypeTable().GetVoidPtr());
    GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "methodnameIndex", GlobalTables::GetTypeTable().GetUInt32());
    GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "signameIndex", GlobalTables::GetTypeTable().GetUInt32());
    GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "callIndex", GlobalTables::GetTypeTable().GetUInt32());
    vtableOffsetTableKeyType = static_cast<MIRStructType *>(
        GlobalTables::GetTypeTable().GetOrCreateStructType("VtableOffsetTableKey", fields, parentFields, module));
    arrayTypeOfVtableKey =
        GlobalTables::GetTypeTable().GetOrCreateArrayType(vtableOffsetTableKeyType,arraySize);
  }
  // Create Offset_value_table
  {
    FieldVector parentFields;
    FieldVector fields;
    GlobalTables::GetTypeTable().PushIntoFieldVector(&fields, "offset", GlobalTables::GetTypeTable().GetUInt32());
    offsetTableEntryType = static_cast<MIRStructType *>(
        GlobalTables::GetTypeTable().GetOrCreateStructType("OffsetTableEntry", fields, parentFields, module));
    arrayTypeOfValue =
        GlobalTables::GetTypeTable().GetOrCreateArrayType(offsetTableEntryType, arraySize);
  }
}

void DeCouple::GenOffsetTable() {
  MIRAggConst *fieldKeyOffsetTableConst = module->memPool->New<MIRAggConst>(module, arrayTypeOfFieldKey);
  MIRAggConst *vtableKeyOffsetTableConst = module->memPool->New<MIRAggConst>(module, arrayTypeOfVtableKey);
  MIRAggConst *valueOffsetTableConst = module->memPool->New<MIRAggConst>(module, arrayTypeOfValue);

  for (auto offsetTableItem : offsetTableV) {
    uint32 fieldID = 1;
    MIRAggConst *entryConst = module->memPool->New<MIRAggConst>(module, offsetTableEntryType);
    builder->AddIntFieldConst(offsetTableEntryType, entryConst, fieldID++, offsetTableItem.offset);
    valueOffsetTableConst->constVec.push_back(entryConst);

    if (offsetTableItem.flag == FLAG_OFFSET::kField) {
      fieldID = 1;
      MIRAggConst *keyConst = module->memPool->New<MIRAggConst>(module, fieldOffsetTableKeyType);
      builder->AddAddrofFieldConst(fieldOffsetTableKeyType, keyConst, fieldID++, offsetTableItem.classSym);
      builder->AddIntFieldConst(fieldOffsetTableKeyType, keyConst, fieldID++, offsetTableItem.index.fieldIndex);
      builder->AddIntFieldConst(fieldOffsetTableKeyType, keyConst, fieldID++, offsetTableItem.callIndex);
      fieldKeyOffsetTableConst->constVec.push_back(keyConst);
    } else if (offsetTableItem.flag == FLAG_OFFSET::kVtable) {
      fieldID = 1;
      MIRAggConst *keyConst = module->memPool->New<MIRAggConst>(module, vtableOffsetTableKeyType);
      builder->AddAddrofFieldConst(vtableOffsetTableKeyType, keyConst, fieldID++, offsetTableItem.classSym);
      builder->AddIntFieldConst(vtableOffsetTableKeyType, keyConst, fieldID++,
                                   offsetTableItem.index.vtableIndex.methodnameIndex);
      builder->AddIntFieldConst(vtableOffsetTableKeyType, keyConst, fieldID++,
                                   offsetTableItem.index.vtableIndex.signameIndex);
      builder->AddIntFieldConst(vtableOffsetTableKeyType, keyConst, fieldID++, offsetTableItem.callIndex);
      vtableKeyOffsetTableConst->constVec.push_back(keyConst);
    }
  }
  std::string fieldSymName = NameMangler::kFieldOffsetTabStr + module->GetFileNameAsPostfix();
  std::string vtableSymName = NameMangler::kVtabOffsetTabStr + module->GetFileNameAsPostfix();
  std::string valueSymName = NameMangler::kOffsetTabStr + module->GetFileNameAsPostfix();
  std::vector<MIRAggConst *> constV = { { fieldKeyOffsetTableConst, vtableKeyOffsetTableConst,
                                          valueOffsetTableConst } };
  std::vector<MIRSymbol *> symV = { { fieldOffsetTableKeySym, vtableOffsetTableKeySym, offsetTableSym } };
  std::vector<std::string> symnameV = { { fieldSymName, vtableSymName, valueSymName } };
  std::vector<MIRArrayType *> arrayTypeV = { { arrayTypeOfFieldKey, arrayTypeOfVtableKey, arrayTypeOfValue } };
  for (int i = 0; i < 3; i++) {
    if (!constV[i]->constVec.size()) {
      continue;
    }
    arrayTypeV[i]->sizeArray[0] = constV[i]->constVec.size();
    symV[i] = builder->GetOrCreateGlobalDecl(symnameV[i], arrayTypeV[i], kScGlobal);
    symV[i]->storageClass = kScFstatic;
    symV[i]->SetConst(constV[i]);
  }
}

bool DeCouple::NeedGenOffset(const std::string &className) const {
  std::string str = className;
  if (className[0] == 'A' && className.length() > kPrimitiveArrayLength) {
    str = className.substr(1, className.length() - 1);
  }
  return nonDeCoupleClass.end() == nonDeCoupleClass.find(str);
}

void DeCouple::InsertFieldCallItem(MIRType *pointedTy, FieldID fieldID, int &callIndex) {
  MIRClassType *ctype = static_cast<MIRClassType *>(pointedTy);
  if (pointedTy->GetKind() == kTypeClass) {
    MIRClassType *ctype = static_cast<MIRClassType *>(pointedTy);
    FieldPair fpair = ctype->TraverseToField(fieldID);
    const std::string &fieldname = NameMangler::DecodeName(GlobalTables::GetStrTable().GetStringFromStrIdx(fpair.first));
    const std::string &classname = ctype->GetName();
    MIRSymbol *sym =
      GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(std::string(CLASSINFO_PREFIX_STR) + classname));
    if (!sym) {
      sym = builder->CreateGlobalDecl((std::string(CLASSINFO_PREFIX_STR) + classname).c_str(), GlobalTables::GetTypeTable().GetPtr(), kScGlobal);
      sym->storageClass = kScExtern;
    }
    OffsetTableT item = {
      sym, { { ReflectionAnalysis::FindOrInsertRepeatString(fieldname) } }, fieldID, 0, FLAG_OFFSET::kField
    };
    auto it = find(offsetTableV.begin(), offsetTableV.end(), item);
    if (it != offsetTableV.end()) {
      callIndex = it->callIndex;
    } else {
      callIndex = item.callIndex = offsetTableV.size();
      offsetTableV.push_back(item);
    }
  }
}

int DeCouple::InsertVirtualCallItem(const MIRFunction *callee, int entryoffset) {
  std::string classinfoName = std::string(CLASSINFO_PREFIX_STR) + callee->GetBaseClassName();
  MIRSymbol *classSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(classinfoName));
  if (!classSym) {
    classSym = builder->CreateGlobalDecl(classinfoName, GlobalTables::GetTypeTable().GetPtr(), kScGlobal);
    classSym->storageClass = kScExtern;
  }
  std::string baseMethodname = NameMangler::DecodeName(callee->GetBaseFuncName());
  std::string baseSigname = NameMangler::DecodeName(callee->GetSignature());
  ReflectionAnalysis::ConvertMethodSig(baseSigname);
  OffsetTableT item = { classSym,
                          { { ReflectionAnalysis::FindOrInsertRepeatString(baseMethodname),
                              ReflectionAnalysis::FindOrInsertRepeatString(baseSigname) } },
                          entryoffset,
                          0,
                          FLAG_OFFSET::kVtable };
  int callIndex;
  auto it = find(offsetTableV.begin(), offsetTableV.end(), item);
  if (it != offsetTableV.end()) {
    callIndex = it->callIndex;
  } else {
    item.offset = entryoffset;
    callIndex = item.callIndex = offsetTableV.size();
    offsetTableV.push_back(item);
  }
  return callIndex;
}

void DeCouple::CollectDreadStmt(MIRFunction *curFunc, StmtNode *stmt) {
  if (!curFunc || !stmt) {
    return;
  }
  switch (stmt->op) {
    case OP_if: {
      IfStmtNode *inode = static_cast<IfStmtNode *>(stmt);
      CollectDreadExpr(curFunc, stmt, inode->Opnd(0));
      CollectDreadStmt(curFunc, inode->thenPart);
      CollectDreadStmt(curFunc, inode->elsePart);
      break;
    }
    case OP_while: {
      WhileStmtNode *wnode = static_cast<WhileStmtNode *>(stmt);
      CollectDreadExpr(curFunc, stmt, wnode->Opnd(0));
      CollectDreadStmt(curFunc, wnode->body);
      break;
    }
    case OP_block: {
      BlockNode *bnode = static_cast<BlockNode *>(stmt);
      for (StmtNode *s = bnode->GetFirst(); s; s = s->GetNext()) {
        CollectDreadStmt(curFunc, s);
      }
      break;
    }
    default:
      for (int i = 0; i < stmt->NumOpnds(); i++) {
        CollectDreadExpr(curFunc, stmt, stmt->Opnd(i));
      }
  }
}

void DeCouple::CollectDreadExpr(MIRFunction *curFunc, StmtNode *stmt, BaseNode *expr) {
  if (!curFunc || !stmt || !expr) {
    return;
  }

  switch (expr->op) {
    case OP_iaddrof:
    case OP_iread: {
      UnaryNode *uOpnd = static_cast<IreadNode *>(expr);
      IreadNode *inode = static_cast<IreadNode *>(expr);
      MIRType *pointedTy = GlobalTables::GetTypeTable().GetTypeFromTyIdx(
        static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(inode->tyIdx))->pointedTyIdx);
      int callIndex = 0;
      MIRClassType *ctype = static_cast<MIRClassType *>(pointedTy);
      if (pointedTy->GetKind() == kTypeClass && NeedGenOffset(ctype->GetName())) {
        InsertFieldCallItem(pointedTy, inode->fieldID, callIndex);
        MapleVector<BaseNode*> ops(builder->mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
        ops.push_back(GenOffsetTableArrayExpr(callIndex));
        BaseNode *intrinsicsNode = builder->CreateExprIntrinsicop(
            INTRN_MPL_READ_OVTABLE_ENTRY, GlobalTables::GetTypeTable().GetUInt32(), ops);
        BaseNode *ireadExprBase =
            builder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetPtr(), inode->uOpnd, intrinsicsNode);
        inode->tyIdx = GlobalTables::GetTypeTable().GetOrCreatePointerType(ctype->GetFieldType(inode->fieldID))->tyIdx;
        inode->fieldID = 0;
        inode->uOpnd = ireadExprBase;
      }
      break;
    }
    default:
      for (int i = 0; i < expr->NumOpnds(); i++) {
        CollectDreadExpr(curFunc, stmt, expr->Opnd(i));
      }
  }
  return;
}

void DeCouple::ResolveVirtual(CallNode *stmt) {
  MIRFunction *callee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(stmt->puIdx);
  CHECK_FATAL(callee, "Invalid callee from ResolveVirtualNode in %s", currFunc->GetName().c_str());
  const int stmtNopndSize = stmt->nOpnd.size();
  CHECK_FATAL(stmtNopndSize > 0, "container check");
  BaseNode *readFuncPtr = stmt->nOpnd[0];
  CHECK_FATAL(readFuncPtr->op == OP_iread, "The first parameter of virtualicallassigned should be iread");
  BaseNode *addrNode = static_cast<IreadNode *>(readFuncPtr)->uOpnd;
  CHECK_FATAL(addrNode->op == OP_add, "The address of iread should be base+offset");
  BaseNode *offsetNode = static_cast<BinaryNode *>(addrNode)->Opnd(1);
  CHECK_FATAL(offsetNode->op == OP_constval, "The offset should be constVal");
  int entryoffset = static_cast<MIRIntConst *>(static_cast<ConstvalNode *>(offsetNode)->constVal)->value;
  if (NeedGenOffset(callee->GetBaseClassName())) {
    int callIndex = InsertVirtualCallItem(callee, entryoffset);
    MapleVector<BaseNode*> ops(builder->mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
    ops.push_back(GenOffsetTableArrayExpr(callIndex));
    offsetNode = builder->CreateExprIntrinsicop(INTRN_MPL_READ_OVTABLE_ENTRY, GlobalTables::GetTypeTable().GetUInt32(), ops);
    static_cast<BinaryNode *>(addrNode)->SetOpnd(offsetNode, 1);
  }
}

ArrayNode *DeCouple::GenOffsetTableArrayExpr(int callIndex) {
  AddrofNode *baseExpr = nullptr;
  std::string symName = NameMangler::kOffsetTabStr + module->GetFileNameAsPostfix();
  offsetTableSym = builder->GetOrCreateGlobalDecl(symName, arrayTypeOfValue, kScGlobal);
  baseExpr = builder->CreateExprAddrof(0, offsetTableSym);
  MIRArrayType *arrayType = static_cast<MIRArrayType *>(offsetTableSym->GetType());
  ConstvalNode *fieldOffsetExpr = builder->CreateIntConst(callIndex, PTY_i32);
  MapleVector<BaseNode *> opnds(builder->GetCurrentFuncCodeMpAllocator()->Adapter());
  opnds.push_back(baseExpr);
  opnds.push_back(fieldOffsetExpr);
  ArrayNode *arrayExpr = builder->CreateExprArray(arrayType, opnds);
  arrayExpr->boundsCheck = false;
  return arrayExpr;
}

void DeCouple::ProcessFunc(MIRFunction *func) {
  if (trace) {
    LogInfo::MapleLogger() << "DeCouple::ProcessFunc processing " << func->GetName() << endl;
  }
  if (func->IsEmpty()) {
    return;
  }
  SetCurrentFunction(func);

  StmtNode *stmt = func->body->GetFirst();
  StmtNode *next = nullptr;
  while (stmt) {
    next = stmt->GetNext();
    CollectDreadStmt(func, stmt);
    Opcode opcode = stmt->op;
    CHECK_FATAL(opcode != OP_virtualcallassigned && opcode != OP_virtualcall,
           "DeCouple::ProcessFunc expects virtualcall have been lowered into virtualicall");
    if (opcode == OP_virtualicallassigned) {
      ResolveVirtual(static_cast<CallNode *>(stmt));
    } else if (opcode == OP_iassign) {
      IassignNode *inode = static_cast<IassignNode *>(stmt);
      MIRType *pointedTy = GlobalTables::GetTypeTable().GetTypeFromTyIdx(
        static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(inode->tyIdx))->pointedTyIdx);
      MIRClassType *ctype = static_cast<MIRClassType *>(pointedTy);
      int callIndex = 0;
      if (pointedTy->GetKind() == kTypeClass && NeedGenOffset(ctype->GetName())) {
        InsertFieldCallItem(pointedTy, inode->fieldID, callIndex);
        MapleVector<BaseNode*> ops(builder->mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
        ops.push_back(GenOffsetTableArrayExpr(callIndex));
        BaseNode *intrinsicsNode = builder->CreateExprIntrinsicop(
            INTRN_MPL_READ_OVTABLE_ENTRY, GlobalTables::GetTypeTable().GetUInt32(), ops);
        BaseNode *addrExp = builder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetPtr(), inode->addrExpr, intrinsicsNode);
        inode->tyIdx = GlobalTables::GetTypeTable().GetOrCreatePointerType(ctype->GetFieldType(inode->fieldID))->tyIdx;
        inode->fieldID = 0;
        inode->addrExpr = addrExp;
      }
    }
    stmt = next;
  }
}

void DeCouple::Finish() {
  GenOffsetTable();
  GenLocalClassinfo();
}

}  // namespace maple
