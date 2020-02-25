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

#include "analyzector.h"

// AnalyzeCtor analyzes which fields are assgiend inside of of each constructor
namespace maple {

void AnalyzeCtor::ProcessFunc(MIRFunction *func) {
  if (!func->IsConstructor() || func->IsEmpty() || func->formalDefVec.size() == 0) {
    return;
  }
  SetCurrentFunction(func);

  mHasSideEffect = false;
  mFieldSet.clear();
  ProcessBlock(func->body);

  PUIdx puIdx = func->puIdx;
  MapleMap<PUIdx, MapleSet<FieldID> *> &puidxFieldMap = module->puIdxFieldInitializedMap;
  CHECK_FATAL(puidxFieldMap.find(puIdx) == puidxFieldMap.end(), "%s has been processed before", func->GetName().c_str());

  // if the function has calls with sideeffect, conservatively
  // we assume all fields are modified in ctor
  if (mHasSideEffect) {
    MapleSet<FieldID> *fieldsubset =
      module->memPool->New<MapleSet<FieldID>>(std::less<FieldID>(), module->memPoolAllocator.Adapter());
    fieldsubset->insert(0);  // write to all
    puidxFieldMap[puIdx] = fieldsubset;
  } else if (mFieldSet.size() > 0) {
    MapleSet<FieldID> *fieldsubset =
      module->memPool->New<MapleSet<FieldID>>(std::less<FieldID>(), module->memPoolAllocator.Adapter());
    std::copy(mFieldSet.begin(), mFieldSet.end(), std::inserter(*fieldsubset, fieldsubset->begin()));
    puidxFieldMap[puIdx] = fieldsubset;
  } else {
    // no fields are assigned in constructor
    puidxFieldMap[puIdx] = nullptr;
  }
}

// collect field ids which are assigned inside the stmt and mark sideeffect
// flag for non-ctor calls
void AnalyzeCtor::ProcessStmt(StmtNode *stmt) {
  if (!stmt) {
    return;
  }
  switch (stmt->op) {
    case OP_iassign: {
      IassignNode *iassign = static_cast<IassignNode *>(stmt);
      MIRType *basetype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(iassign->tyIdx);
      ASSERT(dynamic_cast<MIRPtrType *>(basetype), "unexpected type");
      MIRType *ptype = static_cast<MIRPtrType *>(basetype)->GetPointedType();
      MIRStructType *structType = dynamic_cast<MIRStructType *>(ptype);
      if (structType) {
        MIRType *fieldType = structType->GetFieldType(iassign->fieldID);
        if (fieldType->primType != PTY_ref) {
          break;
        }
      }
      mFieldSet.insert(iassign->fieldID);
      break;
    }
    case OP_callassigned:
    case OP_call:
    case OP_icall:
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_virtualcall:
    case OP_superclasscall:
    case OP_interfacecall: {
      mHasSideEffect = true;
      break;
    }
    default:
      break;
  }
}

void AnalyzeCtor::Finish() {
  if (trace) {
    for (auto &pit : module->puIdxFieldInitializedMap) {
      GlobalTables::GetFunctionTable().GetFunctionFromPuidx(pit.first)->Dump(true);
      LogInfo::MapleLogger() << "field:";
      MapleSet<FieldID> *fldidset = pit.second;
      if (fldidset == nullptr) {
        LogInfo::MapleLogger() << "write nothing\n";
        continue;
      }
      for (FieldID fid : *fldidset) {
        LogInfo::MapleLogger() << fid << " ";
      }
      LogInfo::MapleLogger() << "\n";
    }
  }
}

}  // namespace maple
