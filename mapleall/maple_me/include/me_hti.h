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


#ifndef MAPLE_ME_INCLUDE_ME_ME_HTI_H
#define MAPLE_ME_INCLUDE_ME_ME_HTI_H
#include "bb.h"
#include "me_cfg.h"
#include <iostream>
#include "me_phase.h"
#include "me_irmap.h"

using namespace std;

namespace maple {
typedef enum {
  kNotProcessed,  // not yet processed by MeTI (start out in this state)
  kNoneedInfer,   // already fixed type or is virtual variable
  kMustNotInfer,  // no further change when in this state
  kMustInfer,     // inferred_type must be set; no further change when
  // reached this state
  kMaybeInfer,  // subject to further change; inferred_type indicates
                // one possible inferred type; if found a second possible
                // inferred type, state changes to kMustNotInfer; if this
                // is final state and only one inferred type, treat as
                // kMustInfer
} InferStatus;

class MeTI {
 public:
  explicit MeTI(MeFunction *func, Dominance *dom, MeIRMap *irMap, MemPool *mp)
    : func(func),
      module(&func->mirModule),
      dom(dom),
      irMap(irMap),
      vst_size(irMap->verst2MeExprTable.size()),
      ti_allocator(mp),
      stmts_with_use(vst_size, nullptr, ti_allocator.Adapter()),
      phis_with_use(vst_size, nullptr, ti_allocator.Adapter()),
      infer_status(vst_size, kNotProcessed, ti_allocator.Adapter()),
      orig_type(vst_size, kPtyInvalid, ti_allocator.Adapter()),
      inferred_type(vst_size, kPtyInvalid, ti_allocator.Adapter()),
      new_symbol(vst_size, nullptr, ti_allocator.Adapter()),
      bb_rebuilt(func->bbVec.size(), false, ti_allocator.Adapter()),
      worklist(std::less<VarMeExpr *>(), ti_allocator.Adapter()) {}

  void BuildUseFromExpr(MeExpr *expr, MeStmt *mestmt);
  void AnalyzeChiList(MapleMap<OStIdx, ChiMeNode *> chilist);
  void AnalyzeMuList(MapleMap<OStIdx, ScalarMeExpr *> mulist);
  void FindMuList(MeExpr *expr);
  void BuildUseAndFindMustNotInfersFromStmt(MeStmt *stmt);
  void BuildUseListsAndFindMustNotInfers();
  void Init();                   // steps 1 and 2
  void PropagateMustNotInfer();  // step 3
  void UpdateInferStatus(VarMeExpr *var, PrimType pty, InferStatus status);
  void DetermineMustOrMaybeInferForPhi(MePhiNode *phi);
  void AnalyzeExpr(MeExpr *, PrimType *inftype, InferStatus *infstatus);
  void DetermineMustOrMaybeInferForStmt(MeStmt *mestmt);
  void AnalyzeIntrAssigned(MeStmt *);
  void DetermineMustOrMaybeInfer();  // step 4
  void MainPropagation();            // step 5
  void TypeInfer();

  MeExpr *CreateConst(int64_t val, PrimType pty);
  MeExpr *RebuildConst(MeExpr *opnd, PrimType pty);
  MeExpr *CreateExprTypeCvt(Opcode o, MeExpr *opnd, PrimType type);
  MeExpr *CreateBinary(Opcode o, MeExpr *opnd0, MeExpr *opnd1, PrimType primType);
  MeExpr *CreateIntrExpr(MIRIntrinsicID intrinsic, MapleVector<MeExpr *> opnds, PrimType primType);
  MeStmt *CreateDassign(MeExpr *lhs, MeExpr *rhs, uint32_t fieldId, BBId bbId);
  MeExpr *CreateNewVar(MeExpr *op, PrimType pty);
  void ReplaceMeStmt(MeStmt *from, MeStmt *to);
  MeExpr *StripExpr(MeExpr *expr);
  MeExpr *RebuildExpr(MeExpr *expr);
  MeExpr *RebuildIntrinop(MeExpr *meexpr);
  void RebuildChiList(MapleMap<OStIdx, ChiMeNode *> &chilist);
  void RebuildMuList(MapleMap<OStIdx, ScalarMeExpr *> &mulist);
  void RebuildMustDefList(IntrinsiccallMeStmt *intrin, MapleVector<MustDefMeNode> *mustdefList);
  void RebuildStmt(MeStmt *);
  void RebuildBB(BB *bb);
  void Rebuild();
  void DumpStatus();

 private:
  MeFunction *func;
  MIRModule *module;
  Dominance *dom;
  MeIRMap *irMap;
  uint32 vst_size;  // total number of VarMeExpr nodes

  MapleAllocator ti_allocator;

  MapleVector<MapleForwardList<MeStmt *> *> stmts_with_use;       // map VarMeExpr's vstIdx to stmts that have uses
  MapleVector<MapleForwardList<MePhiNode *> *> phis_with_use;  // map VarMeExpr's vstIdx to phis where it is an opnd
  MapleVector<InferStatus> infer_status;                          // index is VarMeExpr's vstIdx
  MapleVector<PrimType> orig_type;                                // index is VarMeExpr's vstIdx
  MapleVector<PrimType> inferred_type;                            // index is VarMeExpr's vstIdx
  MapleVector<MeExpr *> new_symbol;                               // index is VarMeExpr's vstIdx, to record
  // the node with the inferred type that replaces it
  MapleVector<bool> bb_rebuilt;                                   // index is BB's id_
  MapleSet<VarMeExpr *> worklist;

  MeExpr *GetOrCreateNewVar(VarMeExpr *v, PrimType pty);
  std::string PhaseName() const {
    return "ti";
  }
};

class MeDohTI : public MeFuncPhase {
 public:
  MeDohTI(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "ti";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_ME_HTI_H
