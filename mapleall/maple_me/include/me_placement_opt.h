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

#ifndef MAPLE_ME_INCLUDE_ME_PLACEMENT_OPT_H_
#define MAPLE_ME_INCLUDE_ME_PLACEMENT_OPT_H_

namespace maple {

typedef enum { kBBOccUndef, kBBOccReal, kBBOccLambda, kBBOccLambdares, kBBOccEntry } BBOccType;

class BBOcc {
 public:
  BBOccType occty;
  BBId bbid;
  int32 classid;

  BBOcc(BBOccType ty, BBId bid) : occty(ty), bbid(bid), classid(0) {}
};

class BBLambdaresOcc;

class BBLambdaOcc : public BBOcc {
 public:
  bool is_upsafe;
  bool canbeant;
  MapleVector<BBLambdaresOcc *> lambdaRes;

  BBLambdaOcc(BBId bid, MapleAllocator *alloc)
    : BBOcc(kBBOccLambda, bid), is_upsafe(true), canbeant(true), lambdaRes(alloc->Adapter()) {}
};

class BBLambdaresOcc : public BBOcc {
 public:
  BBOcc *def;
  bool has_real_use;

  BBLambdaresOcc(BBId bid) : BBOcc(kBBOccLambdares, bid), def(nullptr), has_real_use(false) {}
};

class PlacementOpt {
 public:
  MeFunction *func;
  Dominance *dominance;
  MemPool *percand_mp;
  MapleAllocator percand_allocator;
  bool placementoptdebug;

 private:
  // following are set of BBs in terms of their dfn's; index into
  // dominance->pdtPreOrder to get their bbid's
  MapleSet<uint32> entry_dfns;  // this is fixed by the constructor
  MapleSet<uint32> occur_dfns;  // this is per invocation of ComputePlacement()
  MapleSet<uint32> lambda_dfns;
  MapleSet<uint32> lambdares_dfns;
  MapleVector<BBLambdaOcc *> lambda_occs;  // include only the lambda occs
  MapleVector<BBOcc *> ordered_occs;       // include all occs
  int32 classcount;                        // reinitialize to 0 for each candidate
  PlacementOpt &operator=(const PlacementOpt &);
  PlacementOpt(const PlacementOpt &);

 public:
  // following two sets represent the result of placement optimization
  MapleSet<BBId> inserted_bbs;  // decref to be placed at entry of these BBs
  MapleSet<BBId> lastuse_bbs;   // decref to be placed at exit of these BBs
 private:
  void GetIterPdomFrontier(const BB *bb, MapleSet<uint32> *pdfset) {
    for (BBId bbid : dominance->iterPdomFrontier[bb->id.idx]) {
      pdfset->insert(dominance->pdtDfn[bbid.idx]);
    }
  }
  void FormLambdas();
  void FormLambdaRes();
  void CreateSortedOccs();
  void RenameOccs();
  void ResetUpsafe(BBLambdaresOcc *lambdaRes);
  void ComputeUpsafe();
  void ResetCanbeant(BBLambdaOcc *lambdaOcc);
  void ComputeCanbeant();

 public:
  PlacementOpt(MeFunction *f, Dominance *dom)
    : func(f),
      dominance(dom),
      percand_mp(mempoolctrler.NewMemPool("Per Placementopt Candidate")),
      percand_allocator(percand_mp),
      placementoptdebug(false),
      entry_dfns(std::less<uint32>(), percand_allocator.Adapter()),
      occur_dfns(std::less<uint32>(), percand_allocator.Adapter()),
      lambda_dfns(std::less<uint32>(), percand_allocator.Adapter()),
      lambdares_dfns(std::less<uint32>(), percand_allocator.Adapter()),
      lambda_occs(percand_allocator.Adapter()),
      ordered_occs(percand_allocator.Adapter()),
      classcount(0),
      inserted_bbs(std::less<BBId>(), percand_allocator.Adapter()),
      lastuse_bbs(std::less<BBId>(), percand_allocator.Adapter()) {
    for (BB *entrybb : func->commonEntryBB->succ) {
      entry_dfns.insert(dominance->pdtDfn[entrybb->id.idx]);
    }
  }

  ~PlacementOpt() {
    mempoolctrler.DeleteMemPool(percand_mp);
  }

  void ComputePlacement(MapleSet<BBId> *occurbbs);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_PLACEMENT_OPT_H_
