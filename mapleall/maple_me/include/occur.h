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

#ifndef MAPLE_ME_INCLUDE_OCCUR_H
#define MAPLE_ME_INCLUDE_OCCUR_H

// the data structures that represent occurrences and work candidates for SSAPRE

#include "me_function.h"
#include "irmap.h"

const int kWorkcandHashLength = 229;

namespace maple {

typedef enum {
  kOccUndef,
  kOccReal,
  kOccPhiocc,
  kOccPhiopnd,
  kOccExit,
  kOccInserted,
  kOccUse,     // for use appearances when candidate is dassign
  kOccMembar,  // for representing occurrence of memory barriers (use MeRealOcc)
} OccType;

class MePhiOcc;

class MeOccur {
 public:
  OccType occty;  // kinds of occ
  int classid;    // class id
  BB *mirbb;      // the BB it occurs in
  MeOccur *def;

  MeOccur(OccType ty, int cid, MeOccur *df) : occty(ty), classid(cid), mirbb(nullptr), def(df) {}

  virtual void Dump(IRMap *) = 0;
  void DumpOccur(IRMap *);
  bool IsDominate(Dominance *dom, MeOccur *);
  BB *GetBB() const {
    return mirbb;
  }
};

class MeRealOcc : public MeOccur {
 public:
  MeStmt *mestmt;      // the stmt that has this occ
  MeExpr *meexpr;      // the expr it's corresponding to
  MeExpr *saved_expr;  // the real occ saved to, must be a VarMeExpr/RegMeExpr
  int seq;             // mestmt sequence number in the bb
  uint32_t position = 0;   // the position in the work_cand->real_occs vector
  bool is_reload = false;
  bool is_save = false;
  bool is_lhs = false;
  bool is_formal_at_entry = false;  // the fake lhs occurrence at entry for formals

  MeRealOcc(MeStmt *stmt, int sq, MeExpr *expr)
    : MeOccur(kOccReal, 0, nullptr),
      mestmt(stmt),
      meexpr(expr),
      saved_expr(nullptr),
      seq(sq) {
    if (stmt) {
      mirbb = stmt->bb;
    } else {
      mirbb = nullptr;
    }
  }

  void Dump(IRMap *);
  void SetReload() {
    is_reload = true;
  }

  void SetSave() {
    is_save = true;
  }
};

class MeInsertedOcc : public MeOccur {
 public:
  MeExpr *meexpr;
  MeStmt *mestmt;
  MeExpr *saved_expr;

  MeInsertedOcc(MeExpr *expr, MeStmt *stmt, BB *bb)
    : MeOccur(kOccInserted, 0, nullptr), meexpr(expr), mestmt(stmt), saved_expr(nullptr) {
    mirbb = bb;
  }

  void Dump(IRMap *);
};

class MePhiOpndOcc : public MeOccur {
 public:
  bool is_processed;
  bool has_real_use;
  bool is_insertedocc;    // the phi operand was inserted by inserted occ
  bool is_phiopndreload;  // if insertedocc and redefined the def, set this flag
  MePhiOcc *def_phiocc;   // its lhs
  union {
    MeExpr *meexpr;  // the current expression at the end of the block containing this PhiOpnd
    MeStmt *mestmt;  // which will be inserted during finalize
  } current_expr;
  MeExpr *phiopnd4temp; // must be a VarMeExpr/RegMeExpr, set in CodeMotion phse

  MePhiOpndOcc(BB *dbb)
    : MeOccur(kOccPhiopnd, 0, nullptr),
      is_processed(false),
      has_real_use(false),
      is_insertedocc(false),
      is_phiopndreload(false),
      def_phiocc(nullptr),
      phiopnd4temp(nullptr) {
    mirbb = dbb;
    current_expr.mestmt = nullptr;
  }

  bool IsOkToInsert();
  void Dump(IRMap *);
  MeExpr *GetCurrentMeExpr() {
    return current_expr.meexpr;
  }

  MeStmt *GetCurrentMeStmt() {
    return current_expr.mestmt;
  }
};

class MePhiOcc : public MeOccur {
 public:
  bool is_downsafety;         // default is true
  bool speculative_downsafe = false;  // is downsafe due to speculation
  bool is_canbeavail = true;
  bool is_later = true;
  bool is_extraneous = false;
  bool is_removed = false;  // during finalize2, marked this phiocc is removed or not
  MapleVector<MePhiOpndOcc *> phiopnds;
  MePhiNode *regPhi;  // the reg phi being inserted. TODO: maybe can delete it later
  MePhiNode *varPhi;  // the Var phi being inserted. TODO: maybe can delete it later

  MePhiOcc(BB *bb, MapleAllocator *alloc)
    : MeOccur(kOccPhiocc, 0, nullptr),
      is_downsafety(!bb->IsCatch()),
      phiopnds(alloc->Adapter()),
      regPhi(nullptr),
      varPhi(nullptr) {
    mirbb = bb;
  }

  bool IsWillBeAvail() const {
    return is_canbeavail && !is_later;
  }

  bool IsOpndDefByRealOrInserted();
  void Dump(IRMap *);
};

class MeExitOcc : public MeOccur {
 public:
  MeExitOcc(BB *dbb) : MeOccur(kOccExit, 0, nullptr) {
    mirbb = dbb;
  }

  void Dump(IRMap *);
};

// for killing purpose only
class MeUseOcc : public MeOccur {
 public:
  MeUseOcc(BB *dbb) : MeOccur(kOccUse, 0, nullptr) {
    mirbb = dbb;
  }

  void Dump(IRMap *);
};

// each singly linked list repersents each bucket in workcandHashTable
class PreWorkCand {
public:
  PreWorkCand *next;
  int32 index;
  MapleVector<MeRealOcc *> real_occs;  // maintained in order of dtPreOrder
  MeExpr *themeexpr;                   // the expression of this workcand
  PUIdx puIdx;                         // if 0, its occ span multiple PUs; initial value must
                                       // not be 0; if set to 0, will be stuck at 0
  bool has_local_opnd = false;         // true if any opnd in the expression is local
                                       // puIdx cannot be 0 if has_local_opnd is true
  bool redo2handle_crit_edges = false; // redo to make critical edges affect canbevail
  bool needlocalrefvar = false;        // for the candidate, if necessary to introduce
                                       // localrefvar in addition to the temp register to for saving the value
  bool isSRCand = false;               // is a strength reduction candidate
  static PreWorkCand *workcandHashTable[kWorkcandHashLength];

  PreWorkCand(MapleAllocator *alloc, int32 idx, MeExpr *meexpr, PUIdx pidx)
    : next(nullptr),
      index(idx),
      real_occs(alloc->Adapter()),
      themeexpr(meexpr),
      puIdx(pidx) {
    ASSERT(pidx != 0, "PreWorkCand: initial puIdx cannot be 0");
  }

  void Dump(IRMap *irMap) {
    irMap->ssaTab->mirModule.out << "========index: " << index << " has the following occ\n";
    for (MeOccur *occ : real_occs) {
      occ->Dump(irMap);
    }
  }

  void AddRealOccAsLast(MeRealOcc *occ, PUIdx pidx) {
    real_occs.push_back(occ);  // add as last
    ASSERT(pidx != 0, "puIdx of realocc cannot be 0");
    if (pidx != puIdx) {
      ASSERT(!has_local_opnd, "candidate with local opnd cannot have real occurrences in more than one PU");
      puIdx = 0;
    }
  }

  void AddRealOccSorted(Dominance *dom, MeRealOcc *occ, PUIdx pidx);
  PrimType GetPrimType() const {
    PrimType primType = themeexpr->primType;
    CHECK_FATAL(primType != kPtyInvalid, "");
    return primType;
  }

  static uint32 ComputeWorkCandHashIndex(MeExpr *x);
  virtual void DumpCand(IRMap *irMap) {
    themeexpr->Dump(irMap);
  }

 private:
  void InsertRealOccAt(MeRealOcc *occ, MapleVector<MeRealOcc *>::iterator it, PUIdx pidx);
};

class PreStmtWorkCand : public PreWorkCand {
 public:
  MeStmt *themestmt;  // the statement of this workcand
  bool lhs_is_final;  // used only if candidate is an assignment

  PreStmtWorkCand(MapleAllocator *alloc, int32 idx, MeStmt *mestmt, PUIdx pidx)
    : PreWorkCand(alloc, idx, nullptr, pidx), themestmt(mestmt), lhs_is_final(false) {}

  static uint32 ComputeStmtWorkCandHashIndex(MeStmt *stmt);
  void DumpCand(IRMap *irMap) {
    themestmt->Dump(irMap);
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_OCCUR_H
