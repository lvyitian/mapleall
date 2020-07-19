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

#ifndef MAPLE_ME_INCLUDE_ME_SSU_PRE_H
#define MAPLE_ME_INCLUDE_ME_SSU_PRE_H
#include "me_function.h"
#include "me_irmap.h"
#include "me_dominance.h"

namespace maple {

typedef enum { kSOccUndef, kSOccReal, kSOccLambda, kSOccLambdaRes, kSOccEntry, kSOccKill } SOccType;

class SOcc {
 public:
  SOccType occty;
  int32 classid;
  BB *mirbb;  // the BB it occurs in
  SOcc *use;  // points to its single use

  SOcc(SOccType ty, BB *bb) : occty(ty), classid(0), mirbb(bb), use(nullptr) {}

  virtual void Dump() = 0;
  bool IsPostdominate(Dominance *dom, SOcc *occ) {
    return dom->Postdominate(mirbb, occ->mirbb);
  }
};

class SRealOcc : public SOcc {
 public:
  MeStmt *mestmt;      // the stmt of this real occurrence; null for formal at entry
  VarMeExpr *vmeexpr;  // the varmeexpr of this real occurrence
  bool real_from_def;  // used only by placementrc
  bool redundant;

  SRealOcc() : SOcc(kSOccReal, nullptr), mestmt(nullptr), real_from_def(false), redundant(true) {}

  SRealOcc(MeStmt *s) : SOcc(kSOccReal, s->bb), mestmt(s), vmeexpr(nullptr), real_from_def(false), redundant(true){};
  SRealOcc(MeStmt *s, VarMeExpr *v)
      : SOcc(kSOccReal, s->bb), mestmt(s), vmeexpr(v), real_from_def(false), redundant(true){};
  SRealOcc(BB *bb, VarMeExpr *v)
      : SOcc(kSOccReal, bb), mestmt(nullptr), vmeexpr(v), real_from_def(false), redundant(true){};
  void Dump() {
    LogInfo::MapleLogger() << "RealOcc at bb" << mirbb->id.idx;
    if (real_from_def) {
      LogInfo::MapleLogger() << "(from-def)";
    }
    LogInfo::MapleLogger() << " classid" << classid;
  }
};

class SLambdaOcc;

class SLambdaResOcc : public SOcc {
 public:
  SLambdaOcc *use_lambdaocc;  // its rhs use
  bool has_real_use;
  bool inserthere;

  SLambdaResOcc(BB *bb) : SOcc(kSOccLambdaRes, bb), use_lambdaocc(nullptr), has_real_use(false), inserthere(false) {}

  void Dump() {
    LogInfo::MapleLogger() << "LambdaresOcc at bb" << mirbb->id.idx << " classid" << classid;
  }
};

class SLambdaOcc : public SOcc {
 public:
  bool is_upsafe;
  bool is_canbeant;
  bool is_earlier;
  MapleVector<SLambdaResOcc *> lambdaRes;

  SLambdaOcc(BB *bb, MapleAllocator *alloc)
      : SOcc(kSOccLambda, bb), is_upsafe(true), is_canbeant(true), is_earlier(true), lambdaRes(alloc->Adapter()) {}

  bool WillBeAnt() const {
    return is_canbeant && !is_earlier;
  }

  void Dump() {
    LogInfo::MapleLogger() << "LambdaOcc at bb" << mirbb->id.idx << " classid" << classid << " Lambda[";
    for (uint32 i = 0; i < lambdaRes.size(); i++) {
      lambdaRes[i]->Dump();
      if (i != lambdaRes.size() - 1) {
        LogInfo::MapleLogger() << ", ";
      }
    }
    LogInfo::MapleLogger() << "]";
  }
};

class SEntryOcc : public SOcc {
 public:
  SEntryOcc(BB *bb) : SOcc(kSOccEntry, bb) {}

  void Dump() {
    LogInfo::MapleLogger() << "EntryOcc at bb" << mirbb->id.idx;
  }
};

class SKillOcc : public SOcc {
 public:
  SKillOcc(BB *bb) : SOcc(kSOccKill, bb) {}

  void Dump() {
    LogInfo::MapleLogger() << "KillOcc at bb" << mirbb->id.idx;
  }
};

class SSUPreWorkCand {
 public:
  SSUPreWorkCand *next;
  OriginalSt *theost;       // the stored symbol of this workcand
  VarMeExpr *thevar;              // any existing node of the lhs var
  MapleVector<SOcc *> real_occs;  // maintained in order of pdtPreOrder
  enum {kZeroDef, kOneDef, kMultiDefs} defCnt;  // number of def occurrences
  bool has_critical_edge : 1;     // determined by Finalize step
  bool not_rcfre_cand : 1;     // RCFRE not to be performed for this localrefvar
  bool not_all_redundant : 1;       // used by RCFRE only

  SSUPreWorkCand(MapleAllocator *alloc, OriginalSt *ost)
      : next(nullptr),
        theost(ost),
        thevar(nullptr),
        real_occs(alloc->Adapter()),
        defCnt(kZeroDef),
        has_critical_edge(false),
        not_rcfre_cand(false),
        not_all_redundant(false) {}

  void IncDefCnt() { defCnt = (defCnt == kZeroDef) ? kOneDef : kMultiDefs; }
};

class MeSSUPre {
 protected:
  MeFunction *func;
  SSATab *ssaTab;
  MeIRMap *irMap;
  MIRModule *mirModule;
  Dominance *dominance;
  MemPool *ssuPreMempool;
  MapleAllocator ssuPreAlloc;
 public:
  MapleMap<OStIdx, SSUPreWorkCand *> workcand_map;
 protected:
  SSUPreWorkCand *work_cand;  // current SSUPreWorkCand
  // step 1 lambda insertion data structures:
  // following are set of BBs in terms of their dfn's; index into
  // dominance->pdtPreOrder to get their bbid's
  MapleSet<uint32> lambda_dfns;  // set by FormLambdas()
  // step 2 renaming
  uint32 class_count;  // for assigning new class id
  // the following 3 lists are all maintained in order of pdtPreOrder
  MapleVector<SOcc *> all_occs;           // cleared at start of each workcand
  MapleVector<SLambdaOcc *> lambda_occs;  // cleared at start of each workcand
  MapleVector<SEntryOcc *> entry_occs;    // this is shared by all workcands
  // used in steps 5 and 6
  MapleSet<BBId> catchBlocks2Insert;  // need insertions at entries to these catch blocks
 public:
  enum PreKind { kStorePre, kDecrefPre, k2ndDecrefPre, kRCFre } prekind;

  explicit MeSSUPre(MeFunction *f, Dominance *dom, MemPool *mp, PreKind kind)
      : func(f),
        ssaTab(f->meSSATab),
        irMap(f->irMap),
        mirModule(&f->meSSATab->mirModule),
        dominance(dom),
        ssuPreMempool(mp),
        ssuPreAlloc(mp),
        workcand_map(std::less<OStIdx>(), ssuPreAlloc.Adapter()),
        work_cand(nullptr),
        lambda_dfns(std::less<uint32>(), ssuPreAlloc.Adapter()),
        class_count(0),
        all_occs(ssuPreAlloc.Adapter()),
        lambda_occs(ssuPreAlloc.Adapter()),
        entry_occs(ssuPreAlloc.Adapter()),
        catchBlocks2Insert(ssuPreAlloc.Adapter()),
        prekind(kind) {}

  void ApplySSUPre();

 protected:
  // step 6 methods
  virtual void CodeMotion() = 0;
  // step 5 methods
  void Finalize();
  // step 4 methods
  void ResetCanBeAnt(SLambdaOcc *lambda0);
  virtual void ComputeCanBeAnt();
  void ResetEarlier(SLambdaOcc *lambda0);
  void ComputeEarlier();
  // step 3 methods
  void ResetUpsafe(SLambdaResOcc *lambdaRes);
  void ComputeUpsafe();
  // step 2 methods
  void Rename();
  // step 1 methods
  void GetIterPdomFrontier(const BB *bb, MapleSet<uint32> *pdfset) {
    for (BBId bbid : dominance->iterPdomFrontier[bb->id.idx]) {
      pdfset->insert(dominance->pdtDfn[bbid.idx]);
    }
  }
  void FormLambdas();
  void FormLambdaRes();
  void CreateSortedOccs();
  // step 0 methods
  void CreateEntryOcc(BB *bb) {
    SEntryOcc *entryocc = ssuPreMempool->New<SEntryOcc>(bb);
    entry_occs.push_back(entryocc);
  }

  virtual void BuildWorkListBB(BB *bb) = 0;
  virtual void PerCandInit() = 0;
  virtual void CreateEmptyCleanupIntrinsics() {}
  virtual bool CandCanSkipSSUPre(SSUPreWorkCand *wkcand) { return false; }

  virtual std::string PhaseName() const = 0;
};
};  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_SSU_PRE_H
