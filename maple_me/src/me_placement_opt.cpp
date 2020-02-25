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

#include "me_dominance.h"
#include "me_placement_opt.h"

// This phase is invoked by the analyzerc phase.  When localrefvars become dead
// before function exit, they need to be cleaned up by performing a decref.
// If these decref's are inserted before all return statements in the function,
// some of them can be redundant for a given function invocation because a given
// localref may not have been active within the paths of execution leading to
// a return statement.  The purpose of this phase is to determine mroe optimal
// placements for these decref's, thus elimination partial redundancy in the
// inserted decref's.  The decref placement optimization can be viewed as
// either moving (if not deleting) the decref's from the return statement
// backwards so they would not be uselessly executed on paths leading to the
// return statement where the localrefvar has not been active.  The effect is
// to insert the decrefs as early as possible while still covering all the last
// uses of the localrefvar.
//
// For each invocation of this phase, one localrefvar is processed.
// The entry point to this phase is ComputePlacement(MapleSet<BBId> *occurbbs)
// where occurbbs gives the set of BBs where the current localrefvar is active.
// The output of this phase is represented by inserted_bbs and lastuse_bbs.
// inserted_bbs give the set of BBs where decref should be inserted at BB entry.
// lastuse_bbs give the set of BBs where decref should be inserted at BB exit,
// because there is a last use of the localref within those BBs.
//
// The algorithm is the dual of the SSAPRE algorithm, where lambdas are isnerted
// at control flow split points according to post-dominance frontiers of BBs in
// occurbbs.  The control flow graph is viewed upside down.  The data flow
// analysis computes upsafe followed by canbeant.

// accumulate the BBs that are in the iterated postdominance frontiers of bb in
// the set dfset, visiting each BB only once

namespace maple {

void PlacementOpt::FormLambdas() {
  lambda_dfns.clear();
  for (uint32 dfn : occur_dfns) {
    BBId bbid = dominance->pdtPreOrder[dfn];
    BB *bb = func->bbVec[bbid.idx];
    GetIterPdomFrontier(bb, &lambda_dfns);
  }
}

void PlacementOpt::FormLambdaRes() {
  lambdares_dfns.clear();
  for (uint32 dfn : lambda_dfns) {
    BBId bbid = dominance->pdtPreOrder[dfn];
    BB *bb = func->bbVec[bbid.idx];
    for (BB *succ : bb->succ) {
      lambdares_dfns.insert(dominance->pdtDfn[succ->id.idx]);
    }
  }
}

// create ordered_occs consisting of all 4 kinds of BBOcc nodes, sorted
// according to pdtPreOrder; also create lambda_occs for only the lambda occs
// sorted in pdtPreOrder
void PlacementOpt::CreateSortedOccs() {
  std::unordered_map<BBId, BBLambdaresOcc *> bb2lambdaresMap;
  ordered_occs.clear();
  lambda_occs.clear();
  MapleSet<uint32>::iterator lambdadfnIt = lambda_dfns.begin();
  MapleSet<uint32>::iterator realdfnIt = occur_dfns.begin();
  MapleSet<uint32>::iterator lambdaresdfnIt = lambdares_dfns.begin();
  MapleSet<uint32>::iterator entrydfnIt = entry_dfns.begin();
  BBLambdaOcc *nextLambdaocc = nullptr;
  if (lambdadfnIt != lambda_dfns.end())
    nextLambdaocc = percand_mp->New<BBLambdaOcc>(dominance->pdtPreOrder[*lambdadfnIt], &percand_allocator);
  BBOcc *nextRealocc = nullptr;
  if (realdfnIt != occur_dfns.end())
    nextRealocc = percand_mp->New<BBOcc>(kBBOccReal, dominance->pdtPreOrder[*realdfnIt]);
  BBOcc *nextEntryocc = nullptr;
  if (entrydfnIt != entry_dfns.end())
    nextEntryocc = percand_mp->New<BBOcc>(kBBOccEntry, dominance->pdtPreOrder[*entrydfnIt]);
  BBLambdaresOcc *nextLambdaresocc = nullptr;
  if (lambdaresdfnIt != lambdares_dfns.end()) {
    CHECK(*lambdaresdfnIt < dominance->pdtPreOrder.size(), "index out of range in PlacementOpt::CreateSortedOccs");
    nextLambdaresocc = percand_mp->New<BBLambdaresOcc>(dominance->pdtPreOrder[*lambdaresdfnIt]);
    bb2lambdaresMap[nextLambdaresocc->bbid] = nextLambdaresocc;
  }
  BBOcc *pickedocc;
  do {
    pickedocc = nullptr;
    // the 4 kinds of occ must be checked in this order, so it will be right
    // if more than 1 has the same dfn
    if (nextLambdaocc) {
      pickedocc = nextLambdaocc;
    }
    if (nextRealocc && (pickedocc == nullptr || *realdfnIt < dominance->pdtDfn[pickedocc->bbid.idx])) {
      pickedocc = nextRealocc;
    }
    if (nextLambdaresocc && (pickedocc == nullptr || *lambdaresdfnIt < dominance->pdtDfn[pickedocc->bbid.idx])) {
      pickedocc = nextLambdaresocc;
    }
    if (nextEntryocc && (pickedocc == nullptr || *entrydfnIt < dominance->pdtDfn[pickedocc->bbid.idx])) {
      pickedocc = nextEntryocc;
    }
    if (pickedocc != nullptr) {
      ordered_occs.push_back(pickedocc);
      switch (pickedocc->occty) {
        case kBBOccLambda:
          lambda_occs.push_back(static_cast<BBLambdaOcc *>(pickedocc));
          lambdadfnIt++;
          if (lambdadfnIt != lambda_dfns.end())
            nextLambdaocc = percand_mp->New<BBLambdaOcc>(dominance->pdtPreOrder[*lambdadfnIt], &percand_allocator);
          else {
            nextLambdaocc = nullptr;
          }
          break;
        case kBBOccReal:
          realdfnIt++;
          if (realdfnIt != occur_dfns.end())
            nextRealocc = percand_mp->New<BBOcc>(kBBOccReal, dominance->pdtPreOrder[*realdfnIt]);
          else {
            nextRealocc = nullptr;
          }
          break;
        case kBBOccEntry:
          entrydfnIt++;
          if (entrydfnIt != entry_dfns.end())
            nextEntryocc = percand_mp->New<BBOcc>(kBBOccEntry, dominance->pdtPreOrder[*entrydfnIt]);
          else {
            nextEntryocc = nullptr;
          }
          break;
        case kBBOccLambdares:
          lambdaresdfnIt++;
          if (lambdaresdfnIt != lambdares_dfns.end()) {
            nextLambdaresocc = percand_mp->New<BBLambdaresOcc>(dominance->pdtPreOrder[*lambdaresdfnIt]);
            bb2lambdaresMap[nextLambdaresocc->bbid] = nextLambdaresocc;
          } else {
            nextLambdaresocc = nullptr;
          }
          break;
        default:
          ASSERT(false, "CreateSortedOccs: unexpected occty");
      }
    }
  } while (pickedocc != nullptr);

  // initialize lambdaRes vector in each BBLambdaOcc node
  for (BBLambdaOcc *lambdaOcc : lambda_occs) {
    BB *bb = func->bbVec[lambdaOcc->bbid.idx];
    for (BB *succbb : bb->succ) {
      lambdaOcc->lambdaRes.push_back(bb2lambdaresMap[succbb->id]);
    }
  }
}

void PlacementOpt::RenameOccs() {
  MapleStack<BBOcc *> occStack(percand_allocator.Adapter());
  for (BBOcc *occ : ordered_occs) {
    while (!occStack.empty() &&
           !dominance->Postdominate(func->bbVec[occStack.top()->bbid.idx], func->bbVec[occ->bbid.idx])) {
      occStack.pop();
    }
    switch (occ->occty) {
      case kBBOccReal:
        if (occStack.empty()) {
          occ->classid = classcount++;
          occStack.push(occ);
        } else {
          occ->classid = occStack.top()->classid;
          if (occStack.top()->occty == kBBOccLambda) {
            occStack.push(occ);
          }
        }
        break;
      case kBBOccLambda:
        occ->classid = classcount++;
        occStack.push(occ);
        break;
      case kBBOccLambdares:
        if (occStack.empty()) {
          // keep classid as -1 and def as nullptr
        } else {
          BBLambdaresOcc *lmbdaresocc = static_cast<BBLambdaresOcc *>(occ);
          lmbdaresocc->def = occStack.top();
          lmbdaresocc->classid = occStack.top()->classid;
          if (occStack.top()->occty == kBBOccReal) {
            lmbdaresocc->has_real_use = true;
            // if the real occ is defined by lambda, make def point to the lambda
            if (occStack.size() > 1) {
              BBOcc *savedTos = occStack.top();
              occStack.pop();
              if (occStack.top()->occty == kBBOccLambda && occStack.top()->classid == lmbdaresocc->classid) {
                lmbdaresocc->def = occStack.top();
              }
              occStack.push(savedTos);
            }
          }
        }
        break;
      case kBBOccEntry:
        if (occStack.empty()) {
          break;
        }
        if (occStack.top()->occty == kBBOccLambda) {
          BBLambdaOcc *lambdaOcc = static_cast<BBLambdaOcc *>(occStack.top());
          lambdaOcc->is_upsafe = false;
        }
        break;
      default:
        ASSERT(false, "RenameOccs: unexpected occty");
    }
  }
}

// propagate not-is_upsafe
void PlacementOpt::ResetUpsafe(BBLambdaresOcc *lmbdares) {
  if (lmbdares->def == nullptr) {
    return;
  }
  if (lmbdares->def->occty != kBBOccLambda) {
    return;
  }
  BBLambdaOcc *lambda = static_cast<BBLambdaOcc *>(lmbdares->def);
  if (!lambda->is_upsafe) {
    return;
  }
  lambda->is_upsafe = false;
  for (BBLambdaresOcc *lmbdares0 : lambda->lambdaRes)
    if (!lmbdares0->has_real_use) {
      ResetUpsafe(lmbdares0);
    }
}

void PlacementOpt::ComputeUpsafe() {
  for (BBLambdaOcc *lambdaOcc : lambda_occs) {
    if (!lambdaOcc->is_upsafe) {
      for (BBLambdaresOcc *lmbdares0 : lambdaOcc->lambdaRes)
        if (!lmbdares0->has_real_use) {
          ResetUpsafe(lmbdares0);
        }
    }
  }
}

// propagate not-canbeant
void PlacementOpt::ResetCanbeant(BBLambdaOcc *lmbdaocc) {
  lmbdaocc->canbeant = false;
  // loop thru all occ's to find lmbdaocc's result opnds and reset
  for (BBLambdaOcc *lambdaOcc : lambda_occs) {
    for (BBLambdaresOcc *lmbdares0 : lambdaOcc->lambdaRes)
      if (lmbdares0->def && lmbdares0->def == lmbdaocc) {
        if (!lmbdares0->has_real_use && lambdaOcc->canbeant && !lambdaOcc->is_upsafe) {
          ResetCanbeant(lambdaOcc);
        }
      }
  }
}

void PlacementOpt::ComputeCanbeant() {
  for (BBLambdaOcc *lambdaOcc : lambda_occs) {
    if (!lambdaOcc->canbeant || lambdaOcc->is_upsafe) {
      continue;
    }
    for (BBLambdaresOcc *lmbdares0 : lambdaOcc->lambdaRes)
      if (lmbdares0->def == nullptr) {
        ResetCanbeant(lambdaOcc);
        break;
      }
  }
  // go thru again to check insertion at critical edge and fix up canbeant
  bool has_crit_edge;
  do {
    has_crit_edge = false;
    for (BBLambdaOcc *lambdaOcc : lambda_occs) {
      if (!lambdaOcc->canbeant) {
        continue;
      }
      for (BBLambdaresOcc *lmbdares0 : lambdaOcc->lambdaRes) {
        if (lmbdares0->has_real_use) {
          continue;
        }
        if (lmbdares0->def == nullptr)
          ;
        else if (lmbdares0->def->occty != kBBOccLambda) {
          continue;
        } else {
          BBLambdaOcc *lambda = static_cast<BBLambdaOcc *>(lmbdares0->def);
          if (lambda->canbeant) {
            continue;
          }
        }
        // check for critical edge
        BB *insertbb = func->bbVec[lmbdares0->bbid.idx];
        if (insertbb->pred.size() > 1) {
          ResetCanbeant(lambdaOcc);
          has_crit_edge = true;
          break;
        }
      }
    }
  } while (has_crit_edge);
}

void PlacementOpt::ComputePlacement(MapleSet<BBId> *occurbbs) {
  classcount = 0;
  // initialize occur_dfns
  MapleSet<BBId>::iterator occurbbidIt = occurbbs->begin();
  occur_dfns.clear();
  for (; occurbbidIt != occurbbs->end(); occurbbidIt++) {
    occur_dfns.insert(dominance->pdtDfn[occurbbidIt->idx]);
  }

  FormLambdas();    // result put in the set lambda_bbs
  FormLambdaRes();  // result put in the set lambdares_bbs

  CreateSortedOccs();
  RenameOccs();
  ComputeUpsafe();
  ComputeCanbeant();

  if (placementoptdebug) {
    LogInfo::MapleLogger() << "--- occur at bbs:";
    for (BBId bbid : *occurbbs) {
      LogInfo::MapleLogger() << " " << bbid.idx;
    }
    LogInfo::MapleLogger() << std::endl;

    LogInfo::MapleLogger() << "--- lambdas at bbs:";
    for (BBLambdaOcc *lambdaOcc : lambda_occs) {
      LogInfo::MapleLogger() << " " << lambdaOcc->bbid.idx;
      if (lambdaOcc->is_upsafe) {
        LogInfo::MapleLogger() << " upsafe";
      }
      if (lambdaOcc->canbeant) {
        LogInfo::MapleLogger() << " canbeant";
      }
    }
    LogInfo::MapleLogger() << std::endl;

    LogInfo::MapleLogger() << "--- full occ list:";
    for (BBOcc *occ : ordered_occs) {
      LogInfo::MapleLogger() << " " << occ->bbid.idx;
      if (occ->occty == kBBOccReal) {
        LogInfo::MapleLogger() << "(real " << occ->classid << ")";
      } else if (occ->occty == kBBOccLambda) {
        LogInfo::MapleLogger() << "(lambda " << occ->classid << ")";
      } else if (occ->occty == kBBOccLambdares) {
        LogInfo::MapleLogger() << "(lmbdares " << occ->classid << ")";
      } else if (occ->occty == kBBOccEntry) {
        LogInfo::MapleLogger() << "(entry)";
      }
    }
    LogInfo::MapleLogger() << std::endl;
  }

  // determine inserted_bbs
  inserted_bbs.clear();
  for (BBLambdaOcc *lambdaOcc : lambda_occs) {
    if (!lambdaOcc->canbeant) {
      continue;
    }
    MapleVector<BBLambdaresOcc *>::iterator lambdaresIt = lambdaOcc->lambdaRes.begin();
    for (; lambdaresIt != lambdaOcc->lambdaRes.end(); lambdaresIt++) {
      BBLambdaresOcc *lambdares0 = *lambdaresIt;
      if (lambdares0->has_real_use) {
        continue;
      }
      if (lambdares0->def == nullptr)
        ;
      else if (lambdares0->def->occty != kBBOccLambda) {
        continue;
      } else {
        BBLambdaOcc *lambda = static_cast<BBLambdaOcc *>(lambdares0->def);
        if (lambda->canbeant) {
          continue;
        }
      }
      BB *insertbb = func->bbVec[lambdares0->bbid.idx];
      CHECK_FATAL(insertbb->pred.size() == 1, "ComputePlacement: cannot insert at critical edge");
      inserted_bbs.insert(lambdares0->bbid);
    }
  }
  if (placementoptdebug) {
    LogInfo::MapleLogger() << "--- inserted decrefs at entries of bbs:";
    for (BBId bbid : inserted_bbs) {
      LogInfo::MapleLogger() << " " << bbid.idx;
    }
    LogInfo::MapleLogger() << std::endl;
  }

  // determine lastuse_bbs by pre-order traversal of post-dominator tree
  lastuse_bbs.clear();
  std::vector<bool> classidVisited(classcount, false);  // index is classid
  for (BBOcc *occ : ordered_occs) {
    switch (occ->occty) {
      case kBBOccReal:
        if (!classidVisited[occ->classid]) {
          classidVisited[occ->classid] = true;
          lastuse_bbs.insert(occ->bbid);
        }
        break;
      case kBBOccLambda: {
        BBLambdaOcc *lambdaOcc = static_cast<BBLambdaOcc *>(occ);
        if (lambdaOcc->canbeant) {
          classidVisited[occ->classid] = true;
        }
        break;
      }
      default:;
    }
  }
  if (placementoptdebug) {
    LogInfo::MapleLogger() << "--- lastuse decrefs at exits of bbs:";
    for (BBId bbid : lastuse_bbs) {
      LogInfo::MapleLogger() << " " << bbid.idx;
    }
    LogInfo::MapleLogger() << std::endl;
  }
}

}  // namespace maple
