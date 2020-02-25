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

#ifndef MAPLE_ME_INCLUDE_ME_BDC_OPT
#define MAPLE_ME_INCLUDE_ME_BDC_OPT
#include "me_ident_loops.h"

namespace maple {

class MeBDC {
 public:
  MeFunction *mefunc;
  IdentifyLoops *meloop;
  bool is_debug;

  MeBDC(MeFunction *mf, IdentifyLoops *ml) : mefunc(mf), meloop(ml), is_debug(false) {}

  void DoIt();

 private:
  void CollectArrayExpr(MeStmt *, MeExpr *, std::vector<std::pair<MeStmt *, NaryMeExpr *>> &);
  bool CollectDef(ScalarMeExpr *, std::set<MeStmt *> &, std::set<MePhiNode *> &);
  bool AnalyzeInitDefFromUpper(std::set<MeStmt *> &, const MeExpr *);
  bool AnalyzeInitDefFromLower(std::set<MeStmt *> &);
  bool AnalyzeInitCD(LoopDesc *, const ScalarMeExpr *, bool, const MeExpr *);
  bool AnalyzeStep(LoopDesc *, const std::set<MeStmt *> &, bool);
  bool AnalyzeExitCondGoto(LoopDesc *, ScalarMeExpr *, const BB *, bool, const MeExpr *);
  bool AnalyzeOneExit(LoopDesc *, BB *, ScalarMeExpr *, bool, const MeExpr *);
  void DoBdcForArray(LoopDesc *, const MeStmt *, NaryMeExpr *);
  void DoBdcForLoop(LoopDesc *);
  MeExpr *GetTheNonVar(MeExpr *);
  VarMeExpr *GetTheDefVar(VarMeExpr *);
  bool IndexIsInductionVar(LoopDesc *, VarMeExpr *, MeExpr *&, bool);
  bool OperandGE0(MeExpr *, std::set<MePhiNode *> &);
  bool ArrayWith0AsIndexOpnd(BB *, NaryMeExpr *, BB *);
  bool AnalyzeABDCByCondArrayExpr(BB *, NaryMeExpr *);
  void AnalyzeABDCByCond(BB *, MeExpr *, std::vector<BB *> &);
  bool IsCmp2ArrayLength(const MeExpr *, MeExpr *);
  bool FindCondArrayLength(bool, bool, const VarMeExpr *, const MeExpr *, OpMeExpr *);
  bool FoundCondMeExpr(MeExpr *, std::set<MeExpr *> &);
  bool ExpectValue(MeStmt *, const MeExpr *, int64);
  bool IsOperandGE0(MeExpr *);
  bool FindIndexGE0(bool, bool, const VarMeExpr *, MeExpr *, OpMeExpr *);
  bool AnalyzeABDC4VariableIndexOneCondBB(BB *, NaryMeExpr *, BB *, bool &, bool &);
  bool AnalyzeABDC4VariableIndex(BB *, NaryMeExpr *, std::vector<BB *> &);
  bool IndexLtJarrayLength(NaryMeExpr *, MeExpr *, MeExpr *);
  void AnalyzeNewlyOneABDC(BB *, NaryMeExpr *);
  void AnalyzeNewlyABDC(BB *, MeExpr *);
  void BDCCollectArrayExpr(MeStmt *, MeExpr *, std::vector<std::pair<MeStmt *, NaryMeExpr *>> &);
};

class MeDoBDCOpt : public MeFuncPhase {
 public:
  explicit MeDoBDCOpt(MePhaseID id) : MeFuncPhase(id) {}

  virtual ~MeDoBDCOpt() {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *frm, ModuleResultMgr *mrm) override;
  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m) override {
    ASSERT(false, "should not be here");
    return nullptr;
  }

  virtual std::string PhaseName() const override {
    return "bdcopt";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_BDC_OPT
