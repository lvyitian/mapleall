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

#ifndef MAPLE_ME_INCLUDE_BB_H
#define MAPLE_ME_INCLUDE_BB_H
#include "ssa.h"

namespace maple {
class VersionSt;
class OriginalSt;
class MeStmt;
class MePhiNode;
class IRMap;
enum BBKind {
  kBBUnknown,  // uninitialized
  kBBCondGoto,
  kBBGoto,  // unconditional branch
  kBBFallthru,
  kBBReturn,
  kBBAfterGosub,  // the BB that follows a gosub, as it is an entry point
  kBBSwitch,
  kBBIgoto,
  kBBInvalid
};

enum BBAttr {
  kBBNone,
  kBBIsEntry,   // is this BB a function entry point?
  kBBIsExit,    // is this BB a function exit point?
  kBBWontExit,  // this BB will not reach any exit block
  kBBIsTry,     // bb is tryblock which means more successor(handler)
  kBBIsTryEnd,  // bb is tryblock and with endtry
  kBBIsJSCatch,
  kBBIsJSFinally,
  kBBIsCatch,        // bb is start of catch handler
  kBBIsJavaFinally,  // bb is start of finally handler
  kBBArtificial,     // bb is inserted by maple_me
  kBBIsInLoop        // Is bb in a loop body
};

struct BBId {
  size_t idx;

  BBId() : idx(0) {}

  explicit BBId(size_t i) : idx(i) {}

  bool operator==(const BBId &x) const {
    return idx == x.idx;
  }

  bool operator!=(const BBId &x) const {
    return idx != x.idx;
  }

  bool operator<(const BBId &x) const {
    return idx < x.idx;
  }
};

struct OStIdx {
  size_t idx;

  OStIdx() {
    idx = 0;
  }

  explicit OStIdx(size_t i) : idx(i) {}

  bool operator==(const OStIdx &x) const {
    return idx == x.idx;
  }

  bool operator!=(const OStIdx &x) const {
    return idx != x.idx;
  }

  bool operator<(const OStIdx &x) const {
    return idx < x.idx;
  }
};

using StmtNodes = PtrList<StmtNode>;
using MeStmts = PtrList<MeStmt>;

class BB {
 public:
  BBId id;
  LabelIdx bbLabel;  // the BB's label
  MapleVector<BB*> pred;  // predecessor list
  MapleVector<BB*> succ;  // successor list
  MapleMap<OriginalSt *, PhiNode> *phiList;
  MapleMap<OStIdx, MePhiNode *> mePhiList;
  StmtNodes stmtNodeList;
  MeStmts meStmtList;
  uint32_t frequency;

  BBKind kind : 4;
  bool isEntry : 1;    // is this BB a function entry point?
  bool isExit : 1;     // is this BB a function exit point?
  bool wontExit : 1;   // this BB will not reach any exit block
  bool isTry : 1;      // bb is tryblock which means more successor(handler)
  bool isTryEnd : 1;   // bb is tryblock and with endtry
  bool isJSCatch : 1;
  bool isJSFinally : 1;
  bool isCatch : 1;    // bb is start of catch handler
  bool isJavaFinally : 1;    // bb is start of finally handler
  bool artificial : 1; // bb is inserted by mapleme
  bool isInLoop : 1;   // Is bb in a loop body

 public:
  BB(MapleAllocator *alloc, BBId id)
    : id(id),
      bbLabel(0),
      pred(2, nullptr, alloc->Adapter()),
      succ(2, nullptr, alloc->Adapter()),
      phiList(nullptr),
      mePhiList(alloc->Adapter()),
      frequency(0),
      kind(kBBUnknown),
      isEntry(false),
      isExit(false),
      wontExit(false),
      isTry(false),
      isTryEnd(false),
      isJSCatch(false),
      isJSFinally(false),
      isCatch(false),
      isJavaFinally(false),
      artificial(false),
      isInLoop(false)  {
    pred.pop_back();
    pred.pop_back();
    succ.pop_back();
    succ.pop_back();
  }

  BB(MapleAllocator *alloc, BBId id, StmtNode *fstmt, StmtNode *lstmt)
    : id(id),
      bbLabel(0),
      pred(2, nullptr, alloc->Adapter()),
      succ(2, nullptr, alloc->Adapter()),
      phiList(nullptr),
      mePhiList(alloc->Adapter()),
      stmtNodeList(fstmt, lstmt),
      frequency(0) {
    pred.pop_back();
    pred.pop_back();
    succ.pop_back();
    succ.pop_back();
  }

  virtual bool InTryBlock() const {
    return isTry;
  }

  virtual bool IsEndTry() const {
    return isTryEnd;
  }

  virtual bool IsJavaFinally() const {
    return isJavaFinally;
  }

  virtual bool IsExit() const {
    return isExit;
  }

  virtual bool WontExit() const {
    return wontExit;
  }

  virtual bool IsGoto() const {
    return kind == kBBGoto;
  }

  virtual bool IsFuncEntry() const {
    return false;
  }

  virtual bool IsCatch() const {
    return isCatch;
  }

  virtual bool IsInLoop() const {
    return isInLoop;
  }

  virtual bool AddBackEndTry() const {
    return IsEndTry();
  }

  virtual bool Artificial() const {
    return artificial;
  }

  void Dump(MIRModule *mod);
  void DumpHeader(MIRModule *mod);
  void DumpBBAttribute(MIRModule *mod);

  std::string StrAttribute() const;
  void InsertBefore(BB *bb);  // insert this before bb in optimizer bb list
  void AddPredBB(BB *predVal) {
    ASSERT(predVal != nullptr, "");
    pred.push_back(predVal);
    predVal->succ.push_back(this);
  }

  // This is to help new bb to keep some flags from original bb after logically splitting.
  void CopyFlagsAfterSplit(const BB *bb) {
    isTry = bb->isTry;
    wontExit = bb->wontExit;
  }

  MeStmt *GetCondBrStmt();
  MeStmt *GetTheOnlyMeStmt();
  MeStmt *GetTheOnlyMeStmtWithGoto();

  uint32 UintId() const {
    return id.idx;
  }

  bool IsEmpty() const {
    return stmtNodeList.empty();
  }

  void SetFirst(StmtNode *stmt) {
    stmtNodeList.update_front(stmt);
  }

  void SetFirstMe(MeStmt *stmt);
  void SetLast(StmtNode *stmt) {
    stmtNodeList.update_back(stmt);
  }

  void SetLastMe(MeStmt *stmt);
  bool IsInList(MapleVector<BB*> &) const;
  bool IsPredBB(BB *bb) const {
    // if this is a pred of bb return true;
    // otherwise return false;
    return IsInList(bb->pred);
  }

  bool IsSuccBB(BB *bb) const {
    return IsInList(bb->succ);
  }

  void AddSuccBB(BB *succPara) {
    succ.push_back(succPara);
    succPara->pred.push_back(this);
  }

  void ReplacePred(const BB *old, BB *newPred);
  void ReplaceSucc(const BB *old, BB *newSucc);
  void ReplaceSuccOfCommonEntryBB(const BB *old, BB *newSucc);
  void AddStmtNode(StmtNode *stmt);
  void PrependStmtNode(StmtNode *stmt);
  void RemoveStmtNode(StmtNode *stmt);
  void RemoveLastStmt();
  void InsertStmtBefore(StmtNode *stmt, StmtNode *newStmt);
  void ReplaceStmt(StmtNode *stmt, StmtNode *newStmt);

  int RemoveBBFromVector(MapleVector<BB *> &);
  void RemoveBBFromPred(BB *bb);
  void RemoveBBFromSucc(BB *bb);
  void RemovePred(BB *predbb) {
    predbb->RemoveBBFromSucc(this);
    RemoveBBFromPred(predbb);
  }

  void RemoveSucc(BB *succbb) {
    succbb->RemoveBBFromPred(this);
    RemoveBBFromSucc(succbb);
  }

  void FindReachableBBs(std::vector<bool> &);
  void FindWillExitBBs(std::vector<bool> &);
  PhiNode *PhiofVerstInserted(VersionSt *vsym);
  void InsertPhi(MapleAllocator *alloc, VersionSt *vsym);
  void DumpPhi(const MIRModule *);

  bool IsMeStmtEmpty() const {
    return meStmtList.empty();
  }

  void PrependMeStmt(MeStmt *mestmt);
  void RemoveMeStmt(MeStmt *mestmt);
  void AddMeStmtFirst(MeStmt *mestmt);
  void AddMeStmtLast(MeStmt *mestmt);
  void InsertMeStmtBefore(MeStmt *mestmt, MeStmt *instmt);
  void InsertMeStmtAfter(MeStmt *mestmt, MeStmt *instmt);
  void InsertMeStmtLastBr(MeStmt *instmt);
  void ReplaceMeStmt(MeStmt *stmt, MeStmt *newstmt);
  void DumpMePhiList(IRMap *irMap);
  virtual ~BB(){};

  void EmitBB(SSATab *ssaTab, BlockNode *curblk, MapleMap<uint32, uint32> &freqmap, bool need_another_pass = false);
};

}  // namespace maple

namespace std {

template <>
struct hash<maple::BBId> {
  size_t operator()(const maple::BBId &x) const {
    return x.idx;
  }
};

template <>
struct hash<maple::OStIdx> {
  size_t operator()(const maple::OStIdx &x) const {
    return x.idx;
  }
};

}  // namespace std

#endif  // MAPLE_ME_INCLUDE_BB_H
