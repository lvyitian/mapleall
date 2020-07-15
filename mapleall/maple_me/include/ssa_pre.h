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

#ifndef MAPLE_ME_INCLUDE_SSA_PRE_H_
#define MAPLE_ME_INCLUDE_SSA_PRE_H_
#include "irmap.h"
#include "occur.h"
#include "securec.h"

namespace maple {
class SSAPre {
 protected:
  IRMap *irMap;
  SSATab *ssaTab;
  MIRModule *mirModule;
  Dominance *dominance;
  MemPool *ssapre_mp;
  MapleAllocator ssapre_allocator;
  MemPool *percand_mp;
  MapleAllocator percand_allocator;

 public:
  MapleVector<PreWorkCand *> worklist;
  PreWorkCand *work_cand;  // current PreWorkCand
 protected:
  uint32 cur_treeid;                   // based on number of rooted trees processed in collecting
                                       // PRE work candidates; incremented by 2 for each tree;
                                       // purpose is to avoid processing a node the third time
                                       // inside a tree (which is a DAG)
                                       // the following 3 lists are all maintained in order of dtPreOrder
  MapleVector<MeOccur *> all_occs;     // cleared at start of each workcand
  MapleVector<MePhiOcc *> phi_occs;    // cleared at start of each workcand
  MapleVector<MeExitOcc *> exit_occs;  // this is shared by all workcands
 public:
  enum PreKind { kExprPre, kStmtPre, kLoadPre, kAddrPre } prekind;
  bool ssapredebug;
  bool rclowering_on;
  bool regreadAtReturn;
  bool spillatcatch;
  bool placementrc_on;
  bool addedNewLocalrefvars;
  bool strengthreduction;

 protected:
  uint32 prelimit;  // set by command-line option to limit the number of candidates optimized (for debugging purpose)

  // step 1 phi insertion data structures
  // following are set of BBs in terms of their dfn's; index into
  // dominance->pdtPreOrder to get their bbid's
  MapleSet<uint32> dfphi_dfns;   // phis inserted due to dominance frontiers
  MapleSet<uint32> varphi_dfns;  // phis inserted due to the var operands
  // step 2 renaming data structures
  uint32 class_count;            // count class created during renaming
  MapleSet<uint32> rename2_set;  // set created by rename1 for use rename2;
                                 // value is index into work_cand->real_occs
  // step 6 codemotion data structures
  ScalarMeExpr *cur_temp;              // the created temp for current work_cand
  VarMeExpr *cur_localrefvar;    // the created localrefvar for ref-type iread
  MapleMap<RegMeExpr *, VarMeExpr *> temp2localrefvar_map;
  int32 rebuilt_occ_index;  // stores the size of worklist every time when try to add new worklist, update before each
                            // code motion
  uint32 stridx_count;  // ssapre will create a lot of temp variables if using var to store redundances, start from 0
 public:
  SSAPre(IRMap *hmap, Dominance *dom, MemPool *mp, MemPool *mp2, PreKind kind, uint32 limit)
    : irMap(hmap),
      ssaTab(hmap->ssaTab),
      mirModule(&hmap->ssaTab->mirModule),
      dominance(dom),
      ssapre_mp(mp),
      ssapre_allocator(mp),
      percand_mp(mp2),
      percand_allocator(mp2),
      worklist(ssapre_allocator.Adapter()),
      work_cand(nullptr),
      cur_treeid(0),
      all_occs(ssapre_allocator.Adapter()),
      phi_occs(ssapre_allocator.Adapter()),
      exit_occs(ssapre_allocator.Adapter()),
      prekind(kind),
      ssapredebug(false),
      rclowering_on(false),
      regreadAtReturn(false),
      spillatcatch(false),
      placementrc_on(false),
      addedNewLocalrefvars(false),
      strengthreduction(false),
      prelimit(limit),
      dfphi_dfns(std::less<uint32>(), ssapre_allocator.Adapter()),
      varphi_dfns(std::less<uint32>(), ssapre_allocator.Adapter()),
      class_count(0),
      rename2_set(std::less<uint32>(), ssapre_allocator.Adapter()),
      cur_temp(nullptr),
      cur_localrefvar(nullptr),
      temp2localrefvar_map(ssapre_allocator.Adapter()),
      rebuilt_occ_index(-1),
      stridx_count(0) {
    errno_t ret =
      memset_s(PreWorkCand::workcandHashTable, kWorkcandHashLength * sizeof(PreWorkCand::workcandHashTable), 0,
               sizeof(PreWorkCand::workcandHashTable));
    CHECK_FATAL(ret == EOK, "call memset_s failed in SSAPre");
  }

  void ApplySSAPRE();

 private:
  virtual void DoSSAFRE() {}

 protected:
  // step 6 codemotion methods
  ScalarMeExpr *CreateNewCurTemp(MeExpr *);
  VarMeExpr *CreateNewCurLocalrefvar();
  virtual void GenerateSaveRealocc(MeRealOcc *) = 0;
  virtual void GenerateReloadRealocc(MeRealOcc *) = 0;
  void GenerateSaveInsertedocc(MeInsertedOcc *);
  void GenerateSavePhiocc(MePhiOcc *);
  void UpdateInsertedPhioccOpnd();
  virtual void CodeMotion();
  // step 5 Finalize methods
  virtual void Finalize1();
  void SetSave(MeOccur *);
  void SetReplacement(MePhiOcc *, MeOccur *);
  virtual void Finalize2();
  // step 4 willbevail methods
  void ComputeCanBeAvail();
  void ResetCanBeAvail(MePhiOcc *);
  void ComputeLater();
  void ResetLater(MePhiOcc *);
  // step 3 downsafety methods
  void ResetDS(MePhiOpndOcc *);
  void ComputeDS();
  // step 2 renaming methods
  virtual bool AllVarsSameVersion(MeRealOcc *realocc1, MeRealOcc *realocc2) {
    return realocc1->meexpr == realocc2->meexpr;
  }

  void Rename1();
  MeExpr *GetReplaceMeExpr(MeExpr *, const BB *, uint32);
  virtual MeExpr *PhiOpndFromRes(MeRealOcc *, uint32) = 0;
  virtual void Rename2();
  // step 1 phi insertion methods
  void SetVarPhis(MeExpr *);
  virtual void ComputeVarAndDfPhis() = 0;
  virtual void CreateSortedOccs();
  // phi insertion methods end
  virtual void BuildWorkList() = 0;
  virtual void BuildEntryLhsOcc4Formals() {}

  virtual void BuildWorkListLHSOcc(MeStmt *mestmt, int32 seqstmt) {}

  virtual void BuildWorkListIvarLHSOcc(MeStmt *mestmt, int32 seqstmt, bool isrebuild, MeExpr *tempvar) {}

  virtual void BuildWorkListExpr(MeStmt *, int32, MeExpr *, bool, MeExpr *tempvar, bool isRootExpr) = 0;
  virtual void BuildWorkListStmt(MeStmt *, uint32, bool, MeExpr *tempvar = nullptr);
  virtual void BuildWorkListBB(BB *);
  virtual void ConstructUseOccurMap() {}

  bool DefVarDominateOcc(MeExpr *, MeOccur *);

 protected:
  virtual void CollectVarForMeExpr(MeExpr *meexpr, std::vector<MeExpr *> &varvec) = 0;
  virtual void CollectVarForCand(MeRealOcc *realocc, std::vector<MeExpr *> &varvec) = 0;
  const MapleVector<MeRealOcc *> &GetRealOccList() const {
    return work_cand->real_occs;
  }

  virtual IassignMeStmt *CopyIassignMeStmt(const IassignMeStmt *);
  void IncTreeid() {
    cur_treeid += 2;
  }

  virtual void DumpWorkList();
  virtual void DumpWorkListWrap();
  GStrIdx NewTempStridx();

  void CreateMembarOcc(MeStmt *mestmt, int seqstmt);
  virtual void CreateMembarOccAtCatch(BB *bb);
  void CreateExitOcc(BB *bb) {
    MeExitOcc *exitocc = ssapre_mp->New<MeExitOcc>(bb);
    exit_occs.push_back(exitocc);
  }

  bool CheckIfAnyLocalOpnd(MeExpr *x);
  MeRealOcc *CreateRealOcc(MeStmt *mestmt, int32 seqstmt, MeExpr *meexpr, bool isrebuilt, bool islhs = false);

 public:
  virtual BB *GetBB(BBId id) = 0;
  virtual PUIdx GetPuidx(MeStmt *stmt) = 0;
  virtual PUIdx GetPuidx(BB *bb) = 0;
  virtual void SetCurFunction(PUIdx pidx) {}

  virtual void GetIterDomFrontier(BB *bb, MapleSet<uint32> *dfset) {
    for (BBId bbid : dominance->iterDomFrontier[bb->id.idx]) {
      dfset->insert(dominance->dtDfn[bbid.idx]);
    }
  }

 protected:
  virtual bool ScreenPhiBB(BBId bbid) = 0;
  virtual bool EpreLocalrefvar() {
    return false;
  }

  virtual void EnterCandsForSsaupdate(OStIdx, BB *) {}

  virtual bool IsLoopHeadBB(BBId bbid) {
    return false;
  }
  virtual VarMeExpr *ResolveAllInjuringDefs(VarMeExpr *varx) { return varx; }
  virtual RegMeExpr *ResolveAllInjuringDefs(RegMeExpr *regx) { return regx; }
  virtual MeExpr *ResolveAllInjuringDefs(MeExpr *x) { return x; }
  virtual void SubstituteOpnd(MeExpr *x, MeExpr *oldopnd, MeExpr *newopnd) {}
  virtual void SRSetNeedRepair(MeOccur *useocc, std::set<MeStmt *> *needRepairInjuringDefs) {}
  virtual MeExpr *SRRepairInjuries(MeOccur *useocc,
      std::set<MeStmt *> *needRepairInjuringDefs,
      std::set<MeStmt *> *repairedInjuringDefs) { return nullptr; }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_SSA_PRE_H_
