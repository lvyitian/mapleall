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

#include "orig_symbol.h"

// following cannot be assumed final even though they are declared final
static const std::set<std::string> kStaticFinalBlackList{
  "Ljava_2Flang_2FSystem_3B_7Cout",
  "Ljava_2Flang_2FSystem_3B_7Cerr",
  "Ljava_2Flang_2FSystem_3B_7Cin",
};

namespace maple {

bool OriginalSt::Equal(const OriginalSt *ost) const {
  if (ostType == kSymbolOst) {
    return (ost && symOrPreg.mirSt == ost->symOrPreg.mirSt && fieldID == ost->fieldID &&
            indirectLev == ost->indirectLev);
  } else if (ostType == kPregOst) {
    return (ost && symOrPreg.pregIdx == ost->symOrPreg.pregIdx && indirectLev == ost->indirectLev);
  }
  return false;
}

void OriginalSt::Dump() const {
  if (ostType == kSymbolOst) {
    LogInfo::MapleLogger() << (symOrPreg.mirSt->stIdx.IsGlobal() ? "$" : "%") << symOrPreg.mirSt->GetName();
    if (indexRenamedFrom.idx != 0) {
      LogInfo::MapleLogger() << "[" << indexRenamedFrom.idx << "->" << index.idx << "]";
    }
    if (fieldID != 0) {
      LogInfo::MapleLogger() << "{" << fieldID << "}";
    }
    LogInfo::MapleLogger() << "<" << static_cast<int32>(indirectLev) << ">";
    if (isFinal) {
      LogInfo::MapleLogger() << "F";
    }
    if (isPrivate) {
      LogInfo::MapleLogger() << "P";
    }
  } else if (ostType == kPregOst) {
    LogInfo::MapleLogger() << "%" << GetMIRPreg()->pregNo;
    LogInfo::MapleLogger() << "<" << static_cast<int32>(indirectLev) << ">";
  }
}

void OriginalStTable::Dump() {
  LogInfo::MapleLogger() << "==========original st table===========\n";
  for (uint32 i = 1; i < Size(); i++) {
    OriginalSt *verst = GetOriginalStFromid(OStIdx(i));
    verst->Dump();
  }
  LogInfo::MapleLogger() << "\n=======end original st table===========\n";
}

OriginalSt *OriginalStTable::FindOrCreateSymbolOriginalSt(MIRSymbol *mirst, PUIdx pidx, FieldID fld) {
  MapleUnorderedMap<MIRSymbol *, OStIdx>::iterator it = mirst2ost_.find(mirst);
  if (it == mirst2ost_.end()) {
    // create a new OriginalSt
    OriginalSt *ost = CreateSymbolOriginalSt(mirst, pidx, fld);
    return ost;
  } else {
    CHECK(it->second.idx < original_st_vector_.size(),
          "index out of range in OriginalStTable::FindOrCreateSymbolOriginalSt");
    OriginalSt *ost = original_st_vector_[it->second.idx];
    if (ost->fieldID == fld) {
      return ost;
    }
    return FindOrCreateDiffFieldOriginalSt(ost, fld);
  }
}

OriginalSt *OriginalStTable::FindOrCreatePregOriginalSt(PregIdx regIdx, PUIdx pidx) {
  MapleUnorderedMap<PregIdx, OStIdx>::iterator it = preg2ost_.find(regIdx);
  if (it == preg2ost_.end()) {
    // create a new OriginalSt
    OriginalSt *ost = CreatePregOriginalSt(regIdx, pidx);
    return ost;
  } else {
    return original_st_vector_.at(it->second.idx);
  }
}

OriginalSt *OriginalStTable::CreateSymbolOriginalSt(MIRSymbol *mirst, PUIdx pidx, FieldID fld) {
  OriginalSt *ost =
    alloc.GetMemPool()->New<OriginalSt>(original_st_vector_.size(), mirst, pidx, fld, &alloc);
  if (fld == 0) {
    ost->tyIdx = mirst->tyIdx;
    const std::string &symname = mirst->GetName();
    ost->isFinal = (mirst->IsFinal() && kStaticFinalBlackList.find(symname) == kStaticFinalBlackList.end()) ||
                    mirst->IsLiteral() || mirst->IsLiteralPtr();
    ost->isPrivate = mirst->IsPrivate();
  } else {
    CHECK_FATAL(mirst->GetType()->HasFields(), "CreateSymbolOriginalSt: non-zero fieldID for non-structure");
    MIRStructType *structty = mirst->GetType()->EmbeddedStructType();
    TyidxFieldAttrPair fldpair = structty->TraverseToField(fld).second;
    ost->tyIdx = fldpair.first;
    ost->isFinal = fldpair.second.GetAttr(FLDATTR_final) && !mirModule->CurFunction()->IsConstructor();
    ost->isPrivate = fldpair.second.GetAttr(FLDATTR_private);
  }
  original_st_vector_.push_back(ost);
  mirst2ost_[mirst] = ost->index;
  return ost;
}

OriginalSt *OriginalStTable::CreatePregOriginalSt(PregIdx regIdx, PUIdx pidx) {
  OriginalSt *ost =
    alloc.GetMemPool()->New<OriginalSt>(original_st_vector_.size(), regIdx, pidx, &alloc);
  if (regIdx < 0) {
    ost->tyIdx = TyIdx(PTY_unknown);
  } else {
    ost->tyIdx = GlobalTables::GetTypeTable().typeTable.at(ost->GetMIRPreg()->primType)->tyIdx;
  }
  original_st_vector_.push_back(ost);
  preg2ost_[regIdx] = ost->index;
  return ost;
}

OriginalSt *OriginalStTable::FindSymbolOriginalSt(MIRSymbol *mirst) {
  MapleUnorderedMap<MIRSymbol *, OStIdx>::iterator it = mirst2ost_.find(mirst);
  if (it == mirst2ost_.end()) {
    return nullptr;
  } else {
    CHECK(it->second.idx < original_st_vector_.size(), "index out of range in OriginalStTable::FindSymbolOriginalSt");
    return original_st_vector_[it->second.idx];
  }
}

OriginalSt *OriginalStTable::FindOrCreateAddrofSymbolOriginalSt(OriginalSt *ost) {
  if (ost->prevlevelnode != nullptr) {
    return ost->prevlevelnode;
  }
  // create a new node
  OriginalSt *prevlevOst = alloc.GetMemPool()->New<OriginalSt>(original_st_vector_.size(), ost->GetMIRSymbol(),
                                                                ost->puIdx, 0, &alloc);
  original_st_vector_.push_back(prevlevOst);
  prevlevOst->indirectLev = -1;
  MIRPtrType pointtype(ost->symOrPreg.mirSt->tyIdx, PTY_ptr);
  TyIdx newtyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&pointtype);
  prevlevOst->tyIdx = newtyidx;
  prevlevOst->fieldID = 0;
  prevlevOst->nextlevelnodes.push_front(ost);
  ost->prevlevelnode = prevlevOst;
  return prevlevOst;
}

OriginalSt *OriginalStTable::FindOrCreateExtraLevSymOrRegOriginalSt(OriginalSt *ost, TyIdx ptyidx, FieldID fld) {
  MapleForwardList<OriginalSt *>::iterator it = ost->nextlevelnodes.begin();
  for (; it != ost->nextlevelnodes.end(); it++) {
    if ((*it)->fieldID == fld) {
      break;
    }
  }
  if (it != ost->nextlevelnodes.end()) {
    return *it;
  }
  // create a new node
  OriginalSt *nextlevOst = nullptr;
  if (ost->ostType == OriginalSt::kSymbolOst) {
    nextlevOst = alloc.GetMemPool()->New<OriginalSt>(original_st_vector_.size(), ost->GetMIRSymbol(), ost->puIdx,
                                                      fld, &alloc);
  } else {
    nextlevOst = alloc.GetMemPool()->New<OriginalSt>(original_st_vector_.size(), ost->GetPregIdx(), ost->puIdx,
                                                      &alloc);
  }
  original_st_vector_.push_back(nextlevOst);
  nextlevOst->indirectLev = ost->indirectLev + 1;
  nextlevOst->prevlevelnode = ost;
  if (ptyidx != 0) {
    MIRPtrType *pttype = dynamic_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptyidx));
    if (pttype != nullptr) {
      TyidxFieldAttrPair fldpair = pttype->GetPointedTyidxFldAttrPairWithFieldId(fld);
      CHECK_FATAL(fldpair.first != 0, "FindOrCreateExtraLevSymOrRegOriginalSt: cannot determine field type");
      nextlevOst->tyIdx = fldpair.first;
      nextlevOst->isFinal = fldpair.second.GetAttr(FLDATTR_final) && !mirModule->CurFunction()->IsConstructor();
      nextlevOst->isPrivate = fldpair.second.GetAttr(FLDATTR_private);
    } else {
      nextlevOst->tyIdx = (TyIdx)PTY_void;
    }
  } else if (ost->tyIdx != 0) {
    MIRPtrType *pttype = dynamic_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(ost->tyIdx));
    if (pttype != nullptr) {
      TyidxFieldAttrPair fldpair = pttype->GetPointedTyidxFldAttrPairWithFieldId(fld);
      nextlevOst->tyIdx = fldpair.first;
      nextlevOst->isFinal = fldpair.second.GetAttr(FLDATTR_final) && !mirModule->CurFunction()->IsConstructor();
      nextlevOst->isPrivate = fldpair.second.GetAttr(FLDATTR_private);
    } else {
      nextlevOst->tyIdx = (TyIdx)PTY_void;
    }
  }
  if (GlobalTables::GetTypeTable().typeTable[ost->tyIdx.GetIdx()]->PointsToConstString()) {
    nextlevOst->isFinal = true;
  }
  ost->nextlevelnodes.push_front(nextlevOst);
  ASSERT(original_st_vector_[ost->index.idx] == ost, "OriginalStTable:: index inconsistent");
  return nextlevOst;
}

OriginalSt *OriginalStTable::FindOrCreateExtraLevOriginalSt(OriginalSt *ost, TyIdx ptyidx, FieldID fld) {
  if (ost->ostType == OriginalSt::kSymbolOst || ost->ostType == OriginalSt::kPregOst) {
    return FindOrCreateExtraLevSymOrRegOriginalSt(ost, ptyidx, fld);
  }
  ASSERT(false, "FindOrCreateExtraLevOriginalSt: unexpected ost_type");
  return nullptr;
}

OriginalSt *OriginalStTable::FindExtraLevOriginalSt(OriginalSt *ost, FieldID fld) {
  MapleForwardList<OriginalSt *>::iterator it = ost->nextlevelnodes.begin();
  for (; it != ost->nextlevelnodes.end(); it++) {
    if ((*it)->fieldID == fld) {
      return *it;
    }
  }
  return nullptr;
}

OriginalSt *OriginalStTable::FindOrCreateDiffFieldOriginalSt(OriginalSt *ost, FieldID fld) {
  OriginalSt *parentost = ost->prevlevelnode;
  if (parentost == nullptr) {
    CHECK_FATAL(ost->ostType == OriginalSt::kSymbolOst, "only SymbolOriginalSt expected");
    parentost = FindOrCreateAddrofSymbolOriginalSt(ost);
  }
  MapleForwardList<OriginalSt *>::iterator it = parentost->nextlevelnodes.begin();
  for (; it != parentost->nextlevelnodes.end(); it++) {
    if ((*it)->fieldID == fld) {
      break;
    }
  }
  if (it != parentost->nextlevelnodes.end()) {
    return *it;
  }

  // create a new node
  TyidxFieldAttrPair nextlevFldpair;
  if (parentost->tyIdx != 0) {
    MIRPtrType *pttype = dynamic_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentost->tyIdx));
    if (pttype != nullptr) {
      nextlevFldpair = pttype->GetPointedTyidxFldAttrPairWithFieldId(fld);
      //    type_has_special_name = pttype->GetPointedType()->GetName() == "__class_meta__";
    }
  }
  if (parentost->ostType == OriginalSt::kSymbolOst) {
    OriginalSt *nextlevOst = alloc.GetMemPool()->New<OriginalSt>(
        original_st_vector_.size(), parentost->GetMIRSymbol(), parentost->puIdx, fld, &alloc);
    original_st_vector_.push_back(nextlevOst);
    nextlevOst->indirectLev = parentost->indirectLev + 1;
    nextlevOst->prevlevelnode = parentost;
    nextlevOst->tyIdx = nextlevFldpair.first;
    nextlevOst->isFinal = nextlevFldpair.second.GetAttr(FLDATTR_final) && !mirModule->CurFunction()->IsConstructor();
    if (GlobalTables::GetTypeTable().typeTable[parentost->tyIdx.GetIdx()]->PointsToConstString()) {
      nextlevOst->isFinal = true;
    }
    nextlevOst->isPrivate = nextlevFldpair.second.GetAttr(FLDATTR_private);
    parentost->nextlevelnodes.push_front(nextlevOst);
    return nextlevOst;
  }
  ASSERT(false, "FindOrCreateDiffFieldOriginalSt: unexpected ost_type");
  return nullptr;
}

OriginalSt *OriginalStTable::FindDiffFieldOriginalSt(OriginalSt *ost, FieldID fld) {
  OriginalSt *parentost = ost->prevlevelnode;
  MapleForwardList<OriginalSt *>::iterator it = parentost->nextlevelnodes.begin();
  for (; it != parentost->nextlevelnodes.end(); it++) {
    if ((*it)->fieldID == fld) {
      return *it;
    }
  }
  ASSERT(false, "FindDiffFieldOriginalSt: search failure");
  return nullptr;
}

}  // namespace maple
