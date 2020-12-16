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

#ifndef MAPLE_ME_INCLUDE_ORIG_SYMBOL_H_
#define MAPLE_ME_INCLUDE_ORIG_SYMBOL_H_
#include "mir_module.h"
#include "mir_symbol.h"
#include "mir_preg.h"
#include "mir_function.h"
#include "bb.h"

// This file defines the data structure OriginalSt that represents a program
// symbol occurring in the code of the program being optimized.

namespace maple {

#define INIT_VERSION (0)

class VarMeExpr;

class OriginalSt {
 public:
  OStIdx index;  // index number in original_st_vector_
  MapleVector<uint32> versionsIndex;  // the i-th element refers the index of versionst in versionst table
  uint32 zeroVersionIndex;            // same as versionsIndex[0]
  TyIdx tyIdx;                       // type of this symbol at this level; 0 for unknown
  FieldID fieldID;                     // at each level of indirection
  int8 indirectLev;                   // level of indirection; -1 for address, 0 for itself
  bool isLocal : 1;    // get from defined stmt or use expr
  bool isFormal : 1;  // it's from the formal parameters so the type must be kSymbolOst or kPregOst after rename2preg
  bool addressTaken : 1;
  bool is_vr_needed : 1;               // need to allocate a new virtual register
  bool isFinal : 1;                   // if the field has final attribute, only when fieldID != 0
  bool isPrivate : 1;                 // if the field has private attribute, only when fieldID != 0
  bool ignoreRC : 1;                   // base on MIRSymbol's IgnoreRC()
  bool epreLocalrefvar : 1;            // is a localrefvar temp created by epre phase
  bool symRenamed : 1;                 // has been renamed by symrename phase
  enum OSTType { kUnkonwnOst, kSymbolOst, kPregOst} ostType;
  union {
    PregIdx pregIdx;
    MIRSymbol *mirSt;
  } symOrPreg;
  PUIdx puIdx;
  MapleForwardList<OriginalSt *> nextlevelnodes;  // link up list of its next indirect level with varying fieldID
  OriginalSt *prevlevelnode;
  OStIdx indexRenamedFrom;         // the old ostidx if created by renamesym

  OriginalSt(OStIdx index, MapleAllocator *alloc, bool local, bool isformal, FieldID fldid)
    : index(index),
      versionsIndex(alloc->Adapter()),
      zeroVersionIndex(0),
      tyIdx(0),
      fieldID(fldid),
      indirectLev(0),
      isLocal(local),
      isFormal(isformal),
      addressTaken(false),
      is_vr_needed(false),
      isFinal(false),
      isPrivate(false),
      ignoreRC(false),
      epreLocalrefvar(false),
      symRenamed(false),
      ostType(kUnkonwnOst),
      symOrPreg(),
      puIdx(0),
      nextlevelnodes(alloc->Adapter()),
      prevlevelnode(nullptr),
      indexRenamedFrom(0) {}

  OriginalSt(uint32 index, PregIdx rIdx, PUIdx pIdx, FieldID fldid, MapleAllocator *alloc)
      : OriginalSt(OStIdx(index), alloc, true, false, fldid) {
    ostType = kPregOst;
    symOrPreg.pregIdx = rIdx;
    puIdx = pIdx;
  }

  OriginalSt(uint32 index, MIRSymbol *mirSt, PUIdx pIdx, FieldID fldid, MapleAllocator *alloc)
      : OriginalSt(OStIdx(index), alloc, mirSt->IsLocal(), mirSt->storageClass == kScFormal, fldid) {
    ostType = kSymbolOst;
    symOrPreg.mirSt = mirSt;
    puIdx = pIdx;
    ignoreRC = mirSt->IgnoreRC();
  }

  void Dump() const;

  PregIdx GetPregIdx() const {
    CHECK_FATAL(ostType == kPregOst, "OriginalSt must be PregOst");
    return symOrPreg.pregIdx;
  }

  MIRPreg *GetMIRPreg() const {
    CHECK_FATAL(ostType == kPregOst, "OriginalSt must be PregOst");
    return GlobalTables::GetGsymTable().module->CurFunction()->pregTab->PregFromPregIdx(symOrPreg.pregIdx);
  }

  MIRSymbol *GetMIRSymbol() const {
    CHECK_FATAL(ostType == kSymbolOst, "OriginalSt must be SymbolOst");
    return symOrPreg.mirSt;
  }

  bool HasAttr(AttrKind x) const {
    if (ostType == kSymbolOst) {
      TypeAttrs typeattr = symOrPreg.mirSt->GetAttrs();
      if (typeattr.GetAttr(x)) {
        return true;
      }
    }
    return false;
  }

  bool IsVolatile() const {
    if (ostType == kSymbolOst) {
      return symOrPreg.mirSt->IsVolatile();
    }
    return false;
  }

  bool IsVrNeeded() const {
    if (ostType == kSymbolOst) {
      return symOrPreg.mirSt->isTmp;
    }
    return false;
  }

  bool Equal(const OriginalSt *ost) const;

  bool IsSymbol() const {
    return ostType == kSymbolOst;
  }

  bool IsFormal() const {
    return isFormal == true;
  }

  bool IsPregSymbol() const {
    return (ostType == kPregOst);
  }

  bool IsSymRenamed() const {
    return symRenamed || (indexRenamedFrom.idx != 0);
  }

  bool IsIVCandidate() const {
    if (indirectLev != 0 ||
        (IsSymbol() && GetMIRSymbol()->GetName() == "__nads_dummysym__")) {
      return false;
    }
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
    return IsPrimitiveInteger(mirtype->primType) && mirtype->typeKind != kTypeBitField;
  }

  MIRType *GetType() const {
    return GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  }
};

// This Table is for original symobols only. There is no SSA info attached and SSA is built based on this table.
class OriginalStTable {
 public:
  MapleAllocator alloc;
  MIRModule *mirModule;
  MapleVector<OriginalSt *> original_st_vector_;  // the vector that map a OriginalSt's index to its pointer
  MapleUnorderedMap<MIRSymbol *, OStIdx>
    mirst2ost_;  // mir symbol to original table, this only exists for no-original variables.
  MapleUnorderedMap<PregIdx, OStIdx> preg2ost_;
  MapleMap<TyIdx, OStIdx>
    ptype2ost_;  // mir type to virtual variables in original table. this only exists for no-original variables.
  MapleMap<std::pair<base_node_t *, uint32>, OStIdx>
    malloc2ost_;  // malloc info to virtual variables in original table. this only exists for no-original variables.
  MapleMap<uint32, OStIdx> thisfield2ost_;  // field of this_memory to virtual variables in original table.

  OStIdx virtualost_unkown_mem;
  OStIdx virtualost_const_mem;

  OriginalStTable(MemPool *mp, MIRModule *mod)
    : alloc(mp),
      mirModule(mod),
      original_st_vector_(alloc.Adapter()),
      mirst2ost_(alloc.Adapter()),
      preg2ost_(alloc.Adapter()),
      ptype2ost_(std::less<TyIdx>(), alloc.Adapter()),
      malloc2ost_(alloc.Adapter()),
      thisfield2ost_(std::less<uint32>(), alloc.Adapter()),
      virtualost_unkown_mem(0),
      virtualost_const_mem(0) {
    original_st_vector_.push_back(static_cast<OriginalSt*>(nullptr));
  }
  ~OriginalStTable() {}

  OriginalSt *FindOrCreateSymbolOriginalSt(MIRSymbol *mirst, PUIdx pidx, FieldID fld);
  OriginalSt *FindOrCreatePregOriginalSt(PregIdx regIdx, PUIdx pidx);
  OriginalSt *CreateSymbolOriginalSt(MIRSymbol *mirst, PUIdx pidx, FieldID fld);
  OriginalSt *CreatePregOriginalSt(PregIdx regIdx, PUIdx pidx);
  OriginalSt *FindSymbolOriginalSt(MIRSymbol *mirst);
  OriginalSt *FindOrCreateAddrofSymbolOriginalSt(OriginalSt *ost);
  OriginalSt *FindOrCreateExtraLevSymOrRegOriginalSt(OriginalSt *ost, TyIdx ptyidx, FieldID fld);
  OriginalSt *FindOrCreateExtraLevOriginalSt(OriginalSt *ost, TyIdx ptyidx, FieldID fld);
  OriginalSt *FindExtraLevOriginalSt(OriginalSt *ost, FieldID fld);
  OriginalSt *FindOrCreateDiffFieldOriginalSt(OriginalSt *ost, FieldID fld);
  OriginalSt *FindDiffFieldOriginalSt(OriginalSt *ost, FieldID fld);

  OriginalSt *GetOriginalStFromid(OStIdx id, bool checkfirst = false) {
    if (checkfirst && id.idx >= original_st_vector_.size()) {
      return nullptr;
    }
    ASSERT(id.idx < original_st_vector_.size(), "symbol table index out of range");
    return original_st_vector_[id.idx];
  }

  uint32 Size() const {
    return original_st_vector_.size();
  }

  MIRSymbol *GetMIRSymbolFromid(OStIdx id) {
    return GetOriginalStFromid(id, false)->GetMIRSymbol();
  }

  void Dump();
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ORIG_SYMBOL_H_
