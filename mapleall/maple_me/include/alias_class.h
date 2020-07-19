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

#ifndef MAPLE_ME_INCLUDE_ALIAS_CLASS_H
#define MAPLE_ME_INCLUDE_ALIAS_CLASS_H
#include "mempool.h"
#include "mempool_allocator.h"
#include "phase.h"
#include "ssa_tab.h"
#include "union_find.h"
#include "class_hierarchy.h"

namespace maple {

class AliasElem {
 public:
  uint32 id;  // the original alias class id, before any union; start from 0
  bool notAllDefsSeen;           // applied to current level; unused for lev -1
  bool nextLevNotAllDefsSeen;  // remember that next level's elements need to be made notAllDefsSeen
  OriginalSt *ost;
  MapleSet<uint> *classSet;   // points to the set of members of its class; nullptr for single-member classes
  MapleSet<uint> *assignSet;  // points to the set of members that have assignments among themselves

  AliasElem(uint32 i, OriginalSt *origst)
      : id(i),
        notAllDefsSeen(false),
        nextLevNotAllDefsSeen(false),
        ost(origst),
        classSet(nullptr),
        assignSet(nullptr) {}

  ~AliasElem() {}

  void Dump(MIRModule *mod) const;
};

// this is only for use as return value of CreateAliasElemsExpr()
class AliasInfo {
 public:
  AliasElem *ae;
  FieldID fieldID;              // corresponds to fieldID in OP-addrof/OP_iaddrof

  AliasInfo() : ae(nullptr), fieldID(0) {}
  AliasInfo(AliasElem *ae0, FieldID fld) : ae(ae0), fieldID(fld) {}
  ~AliasInfo() {}
};

class AliasClass : public AnalysisResult {
 public:
  MemPool *acMemPool;
  MapleAllocator acAlloc;
  MIRModule *mirModule;
  SSATab *ssaTab;
  MapleVector<AliasElem *> osym2Elem;  // index is OStIdx
  MapleVector<AliasElem *> id2Elem;    // index is the id
  UnionFind unionfind;
  MapleVector<AliasElem *> notAllDefsSeenClassSetRoots;  // root of the notAllDefsSeen class sets
  MapleSet<uint> globalsAffectedByCalls;  // set of class ids of globals
                                             // aliased at calls; needed only when wholeProgramScope is true
  MapleSet<OStIdx> globalsMayAffectedByClinitCheck;
  bool lessThrowAlias;
  bool finalFieldAlias;  // whether to regard final fields as having alias;
  bool ignoreIPA;        // whether to ignore information provided by IPA
  bool debugPrint;
  bool calleeHasSideEffect;
  KlassHierarchy *klassHierarchy;

  AliasClass(MemPool *mp, MIRModule *mod, SSATab *ssatb, bool lessaliasatthrow, bool finalfieldhasalias, bool ignoreipa,
             bool verbose, bool setCalleeHasSideEffect = false, KlassHierarchy *kh = nullptr)
      : AnalysisResult(mp),
        acMemPool(mp),
        acAlloc(mp),
        mirModule(mod),
        ssaTab(ssatb),
        osym2Elem(ssatb->originalStTable.Size(), nullptr, acAlloc.Adapter()),
        id2Elem(acAlloc.Adapter()),
        unionfind(mp),
        notAllDefsSeenClassSetRoots(acAlloc.Adapter()),
        globalsAffectedByCalls(std::less<uint>(), acAlloc.Adapter()),
        globalsMayAffectedByClinitCheck(std::less<OStIdx>(), acAlloc.Adapter()),
        lessThrowAlias(lessaliasatthrow),
        finalFieldAlias(finalfieldhasalias),
        ignoreIPA(ignoreipa),
        debugPrint(verbose),
        calleeHasSideEffect(setCalleeHasSideEffect),
        klassHierarchy(kh) {}

  ~AliasClass() {}

  AliasElem *FindAliasElem(const OriginalSt *ost) {
    return osym2Elem.at(ost->index.idx);
  }

  bool IsCreatedByElimRC(const OriginalSt *ost) const {
    return ost->index.idx >= osym2Elem.size();
  }

  void ApplyUnionForCopies(StmtNode *stmt);
  void CreateAssignSets();
  void DumpAssignSets();
  void UnionAllPointedTos();
  void ApplyUnionForPointedTos();
  void UnionForNotAllDefsSeen();
  void ApplyUnionForStorageOverlaps();
  void CollectAliasGroups(std::map<uint, std::set<uint>> &aliasGroups);
  bool AliasAccordingToType(TyIdx tyidxA, TyIdx tyidxB);
  bool AliasAccordingToFieldId(const OriginalSt *ostA, const OriginalSt *ostB) const;
  void ReconstructAliasGroups();
  void CollectNotAllDefsSeenAes();
  void CreateClassSets();
  void DumpClassSets();
  void InsertMayDefUseCall(StmtNode *stmt, BBId bbid, bool hasSideEffect, bool hasNoPrivateDefEffect);
  void GenericInsertMayDefUse(StmtNode *stmt, BBId bbid);

 protected:
  virtual bool InConstructorFunc() {
    return true;
  }

 private:
  bool CallHasSideEffect(StmtNode *stmt);
  bool CallHasNoPrivateDefEffect(StmtNode *stmt);
  AliasElem *FindOrCreateAliasElem(OriginalSt *ost);
  virtual bool IsFormalParm(OriginalSt *ost) const {
    return ost->GetMIRSymbol()->storageClass == kScFormal;
  }

  AliasInfo CreateAliasElemsExpr(BaseNode *expr);
  void SetNotAllDefsSeenForMustDefs(const StmtNode *callas);
  void SetPtrOpndsNextLevNADS(uint start, uint end, MapleVector<BaseNode *> &opnds);
  void ApplyUnionForDassignCopy(const AliasElem *lhsAe, AliasElem *rhsAe, const BaseNode *rhs);
  AliasElem *FindOrCreateDummyNADSAe();
  void CollectMayDefForMustDefs(const StmtNode *stmt, std::set<OriginalSt *> &mayDefOsts);
  void CollectMayUseForCallOpnd(StmtNode *stmt, std::set<OriginalSt *> &mayUseOsts);
  void InsertMayDefNodeForCall(std::set<OriginalSt *> &mayDefOsts, MapleMap<OStIdx, MayDefNode> *mayDefNodes,
                               StmtNode *stmt, BBId bbid, bool hasNoPrivateDefEffect);
  void InsertMayUseExpr(BaseNode *expr);
  void CollectMayUseFromGlobalsAffectedByCalls(std::set<OriginalSt *> &mayUseOsts);
  void CollectMayUseFromNADS(std::set<OriginalSt *> &mayUseOsts);
  void InsertMayUseNode(std::set<OriginalSt *> &mayUseOsts, MapleMap<OStIdx, MayUseNode> *mayUseNodes);
  void InsertMayUseReturn(const StmtNode *stmt);
  void CollectPtsToOfReturnOpnd(OriginalSt *ost, std::set<OriginalSt *> &mayUseOsts);
  void InsertReturnOpndMayUse(StmtNode *stmt);
  void InsertMayUseAll(const StmtNode *stmt);
  void CollectMayDefForDassign(const StmtNode *stmt, std::set<OriginalSt *> &mayDefOsts);
  void InsertMayDefNode(std::set<OriginalSt *> &mayDefOsts, MapleMap<OStIdx, MayDefNode> *mayDefNodes, StmtNode *stmt,
                        BBId bbid);
  void InsertMayDefDassign(StmtNode *stmt, BBId bbid);
  void CollectMayDefForIassign(StmtNode *stmt, std::set<OriginalSt *> &mayDefOsts);
  void InsertMayDefNodeExcludeFinalOst(std::set<OriginalSt *> &mayDefOsts, MapleMap<OStIdx, MayDefNode> *mayDefNodes,
                                       StmtNode *stmt, BBId bbid);
  void InsertMayDefIassign(StmtNode *stmt, BBId bbid);
  void InsertMayDefUseSyncops(StmtNode *stmt, BBId bbid);
  void InsertMayUseNodeExcludeFinalOst(std::set<OriginalSt *> &mayUseOsts, MapleMap<OStIdx, MayUseNode> *mayUseNodes);
  void InsertMayDefUseIntrncall(StmtNode *stmt, BBId bbid);
  void InsertMayDefUseClinitCheck(IntrinsiccallNode *stmt, BBId bbid);
  virtual BB *GetBB(BBId id) = 0;
};
}  // namespace maple
#endif  // end MAPLE_ME_INCLUDE_ALIASCLASS_H
