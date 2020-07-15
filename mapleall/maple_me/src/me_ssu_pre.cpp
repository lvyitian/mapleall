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

#include "me_ssu_pre.h"

namespace maple {

#define JAVALANG (mirModule->IsJavaModule())

// ================ Step 5: Finalize ================

void MeSSUPre::Finalize() {
  std::vector<SOcc *> anticipatedDefVec(class_count + 1, nullptr);
  // preorder traversal of post-dominator tree
  for (SOcc *occ : all_occs) {
    uint32 classx = occ->classid;
    switch (occ->occty) {
      case kSOccLambda: {
        SLambdaOcc *lambdaOcc = static_cast<SLambdaOcc *>(occ);
        if (lambdaOcc->WillBeAnt()) {
          anticipatedDefVec[classx] = lambdaOcc;
        }
        break;
      }
      case kSOccReal: {
        SRealOcc *realocc = static_cast<SRealOcc *>(occ);
        if (anticipatedDefVec[classx] == nullptr || !anticipatedDefVec[classx]->IsPostdominate(dominance, occ)) {
          realocc->redundant = false;
          anticipatedDefVec[classx] = realocc;
        } else {
          realocc->redundant = true;
        }
        break;
      }
      case kSOccLambdaRes: {
        SLambdaResOcc *lambdaResOcc = static_cast<SLambdaResOcc *>(occ);
        SLambdaOcc *lambdaOcc = lambdaResOcc->use_lambdaocc;
        if (lambdaOcc->WillBeAnt()) {
          if (lambdaResOcc->use == nullptr || (!lambdaResOcc->has_real_use && lambdaResOcc->use->occty == kSOccLambda &&
                                               !static_cast<SLambdaOcc *>(lambdaResOcc->use)->WillBeAnt())) {
            // insert a store
            BB *insertbb = lambdaResOcc->mirbb;
            if (insertbb->IsCatch()) {
              if (prekind == kDecrefPre) {
                catchBlocks2Insert.insert(insertbb->id);
                break;
              } else { // kStorePre: omit insertion at entry of catch blocks
                break;
              }
            }
            if (lambdaResOcc->mirbb->pred.size() != 1) { // critical edge
              if (prekind != kDecrefPre) {
                CHECK_FATAL(false, "MeSSUPre::Finalize: insertion at critical edge");
                work_cand->has_critical_edge = true;
                return;
              }
            }
            if (insertbb->artificial && insertbb->isExit) {
              // do not insert at fake BBs created due to infinite loops
              break;
            }
            lambdaResOcc->inserthere = true;
          } else {
            lambdaResOcc->use = anticipatedDefVec[classx];
          }
        }
        break;
      }
      case kSOccEntry:
      case kSOccKill:
        break;
      default:
        ASSERT(false, "Finalize: unexpected occ type");
    }
  }

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << " _______ after finalize _______" << std::endl;
    for (SOcc *occ : all_occs) {
      if (occ->occty == kSOccReal) {
        SRealOcc *realocc = static_cast<SRealOcc *>(occ);
        if (realocc->redundant) {
          occ->Dump();
          LogInfo::MapleLogger() << " deleted" << std::endl;
        }
      } else if (occ->occty == kSOccLambdaRes) {
        SLambdaResOcc *lambdaResOcc = static_cast<SLambdaResOcc *>(occ);
        if (lambdaResOcc->inserthere) {
          occ->Dump();
          LogInfo::MapleLogger() << " inserthere" << std::endl;
        }
      }
    }
  }
}

// ================ Step 4: WillBeAnt Computation ================

void MeSSUPre::ResetCanBeAnt(SLambdaOcc *lambda0) {
  lambda0->is_canbeant = false;
  // the following loop finds lambda0's defs and reset them
  for (SLambdaOcc *lambdaOcc : lambda_occs) {
    for (SLambdaResOcc *lambdaResOcc : lambdaOcc->lambdaRes) {
      if (lambdaResOcc->use && lambdaResOcc->use == lambda0) {
        if (!lambdaResOcc->has_real_use && !lambdaOcc->is_upsafe && lambdaOcc->is_canbeant) {
          ResetCanBeAnt(lambdaOcc);
        }
      }
    }
  }
}

void MeSSUPre::ComputeCanBeAnt() {
  for (SLambdaOcc *lambdaOcc : lambda_occs) {
    if (!lambdaOcc->is_upsafe && lambdaOcc->is_canbeant) {
      bool existNullUse = false;
      for (SLambdaResOcc *lambdaResOcc : lambdaOcc->lambdaRes) {
        if (lambdaResOcc->use == nullptr) {
          existNullUse = true;
          break;
        }
      }
      if (existNullUse) {
        ResetCanBeAnt(lambdaOcc);
      }
    }
  }
}

void MeSSUPre::ResetEarlier(SLambdaOcc *lambda0) {
  lambda0->is_earlier = false;
  // the following loop finds lambda0's defs and reset them
  for (SLambdaOcc *lambdaOcc : lambda_occs) {
    for (SLambdaResOcc *lambdaResOcc : lambdaOcc->lambdaRes) {
      if (lambdaResOcc->use && lambdaResOcc->use == lambda0) {
        if (lambdaOcc->is_earlier) {
          ResetEarlier(lambdaOcc);
        }
      }
    }
  }
}

void MeSSUPre::ComputeEarlier() {
  for (SLambdaOcc *lambdaOcc : lambda_occs) {
    lambdaOcc->is_earlier = lambdaOcc->is_canbeant;
  }

  for (SLambdaOcc *lambdaOcc : lambda_occs) {
    if (lambdaOcc->is_earlier) {
      bool existNonNullUse = false;
      for (SLambdaResOcc *lambdaResOcc : lambdaOcc->lambdaRes) {
        if (lambdaResOcc->use && lambdaResOcc->has_real_use) {
          existNonNullUse = true;
          break;
        }
      }
      if (existNonNullUse) {
        ResetEarlier(lambdaOcc);
      }
    }
  }

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << " _______ after earlier computation _______" << std::endl;
    for (SLambdaOcc *lambdaOcc : lambda_occs) {
      lambdaOcc->Dump();
      if (lambdaOcc->is_canbeant) {
        LogInfo::MapleLogger() << " canbeant";
      }
      if (lambdaOcc->is_earlier) {
        LogInfo::MapleLogger() << " earlier";
      }
      if (lambdaOcc->is_canbeant && !lambdaOcc->is_earlier) {
        LogInfo::MapleLogger() << " will be ant";
      }
      LogInfo::MapleLogger() << std::endl;
    }
  }
}

// ================ Step 3: Upsafe Computation ================

void MeSSUPre::ResetUpsafe(SLambdaResOcc *lambdaRes) {
  if (lambdaRes->has_real_use) {
    return;
  }
  SOcc *useocc = lambdaRes->use;
  if (useocc == nullptr || useocc->occty != kSOccLambda) {
    return;
  }

  SLambdaOcc *uselambdaocc = static_cast<SLambdaOcc *>(useocc);
  if (!uselambdaocc->is_upsafe) {
    return;
  }
  uselambdaocc->is_upsafe = false;
  for (SLambdaResOcc *lambdaResOcc : uselambdaocc->lambdaRes) {
    ResetUpsafe(lambdaResOcc);
  }
}

void MeSSUPre::ComputeUpsafe() {
  for (SLambdaOcc *lambdaOcc : lambda_occs) {
    if (!lambdaOcc->is_upsafe) {
      // propagate not-upsafe forward along def-use edges
      for (SLambdaResOcc *lambdaResOcc : lambdaOcc->lambdaRes) {
        ResetUpsafe(lambdaResOcc);
      }
    }
  }

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << " _______ after upsafe computation _______" << std::endl;
    for (SLambdaOcc *lambdaOcc : lambda_occs) {
      lambdaOcc->Dump();
      if (lambdaOcc->is_upsafe) {
        LogInfo::MapleLogger() << " upsafe";
      }
      LogInfo::MapleLogger() << std::endl;
    }
  }
}

// ================ Step 2: rename ================
void MeSSUPre::Rename() {
  std::stack<SOcc *> occStack;
  class_count = 0;
  // iterate thru the occurrences in order of preorder traversal of
  // post-dominator tree
  for (SOcc *occ : all_occs) {
    while (!occStack.empty() && !occStack.top()->IsPostdominate(dominance, occ)) {
      occStack.pop();
    }
    switch (occ->occty) {
      case kSOccKill:
        if (!occStack.empty()) {
          SOcc *topocc = occStack.top();
          if (topocc->occty == kSOccLambda) {
            static_cast<SLambdaOcc *>(topocc)->is_upsafe = false;
          }
        }
        occStack.push(occ);
        break;
      case kSOccEntry:
        if (!occStack.empty()) {
          SOcc *topocc = occStack.top();
          if (topocc->occty == kSOccLambda) {
            static_cast<SLambdaOcc *>(topocc)->is_upsafe = false;
          }
        }
        break;
      case kSOccLambda:
        // assign new class
        occ->classid = ++class_count;
        occStack.push(occ);
        break;
      case kSOccReal: {
        if (occStack.empty()) {
          // assign new class
          occ->classid = ++class_count;
          occStack.push(occ);
          break;
        }
        SOcc *topocc = occStack.top();
        if (topocc->occty == kSOccKill) {
          // assign new class
          occ->classid = ++class_count;
          occStack.push(occ);
          break;
        }
        ASSERT(topocc->occty == kSOccLambda || topocc->occty == kSOccReal, "Rename: unexpected top-of-stack occ");
        occ->classid = topocc->classid;
        if (topocc->occty == kSOccLambda) {
          occStack.push(occ);
        }
        break;
      }
      case kSOccLambdaRes: {
        if (occStack.empty()) {
          // leave classid as 0
          break;
        }
        SOcc *topocc = occStack.top();
        if (topocc->occty == kSOccKill) {
          // leave classid as 0
          break;
        }
        ASSERT(topocc->occty == kSOccLambda || topocc->occty == kSOccReal, "Rename: unexpected top-of-stack occ");
        occ->use = topocc;
        occ->classid = topocc->classid;
        if (topocc->occty == kSOccReal) {
          static_cast<SLambdaResOcc *>(occ)->has_real_use = true;
        }
        break;
      }
      default:
        ASSERT(false, "Rename: unexpected type of occurrence");
    }
  }

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << " _______ after rename _______" << std::endl;
    for (SOcc *occ : all_occs) {
      occ->Dump();
      LogInfo::MapleLogger() << std::endl;
    }
  }
}

// ================ Step 1: insert lambdas ================

// form lambda occ based on the real occ in work_cand->real_occs; result is
// stored in lambda_dfns
void MeSSUPre::FormLambdas() {
  lambda_dfns.clear();
  for (SOcc *occ : work_cand->real_occs) {
    GetIterPdomFrontier(occ->mirbb, &lambda_dfns);
  }
}

// form all_occs inclusive of real, kill, lambda, lambdaRes, entry occurrences;
// form lambda_occs containing only the lambdas
void MeSSUPre::CreateSortedOccs() {
  // form lambdaRes occs based on the succs of the lambda occs ; result is
  // stored in lambdares_dfns
  std::multiset<uint32> lambdares_dfns;
  for (uint32 dfn : lambda_dfns) {
    BBId bbid = dominance->pdtPreOrder[dfn];
    BB *bb = func->bbVec[bbid.idx];
    for (BB *succ : bb->succ) {
      lambdares_dfns.insert(dominance->pdtDfn[succ->id.idx]);
    }
  }

  all_occs.clear();
  lambda_occs.clear();

  std::unordered_map<BBId, std::forward_list<SLambdaResOcc *>> bb2lambdaresMap;
  MapleVector<SOcc *>::iterator realoccIt = work_cand->real_occs.begin();
  MapleVector<SEntryOcc *>::iterator entryoccIt = entry_occs.begin();
  MapleSet<uint32>::iterator lambdadfnIt = lambda_dfns.begin();
  std::multiset<uint32>::iterator lambdaresdfnIt = lambdares_dfns.begin();
  SOcc *nextRealocc = nullptr;
  if (realoccIt != work_cand->real_occs.end()) {
    nextRealocc = *realoccIt;
  }
  SEntryOcc *nextEntryocc = nullptr;
  if (entryoccIt != entry_occs.end()) {
    nextEntryocc = *entryoccIt;
  }
  SLambdaOcc *nextLambdaocc = nullptr;
  if (lambdadfnIt != lambda_dfns.end()) {
    nextLambdaocc = ssuPreMempool->New<SLambdaOcc>(func->bbVec[dominance->pdtPreOrder[*lambdadfnIt].idx], &ssuPreAlloc);
  }
  SLambdaResOcc *nextLambdaresocc = nullptr;
  if (lambdaresdfnIt != lambdares_dfns.end()) {
    nextLambdaresocc = ssuPreMempool->New<SLambdaResOcc>(func->bbVec[dominance->pdtPreOrder[*lambdaresdfnIt].idx]);

    std::unordered_map<BBId, std::forward_list<SLambdaResOcc *>>::iterator it =
        bb2lambdaresMap.find(dominance->pdtPreOrder[*lambdaresdfnIt]);
    if (it == bb2lambdaresMap.end()) {
      std::forward_list<SLambdaResOcc *> newlist = { nextLambdaresocc };
      bb2lambdaresMap[dominance->pdtPreOrder[*lambdaresdfnIt]] = newlist;
    } else {
      it->second.push_front(nextLambdaresocc);
    }
  }

  SOcc *pickedocc;  // the next picked occ in order of preorder traversal of post-dominator tree
  do {
    pickedocc = nullptr;
    if (nextLambdaocc) {
      pickedocc = nextLambdaocc;
    }
    if (nextRealocc && (pickedocc == nullptr || dominance->pdtDfn[nextRealocc->mirbb->id.idx] <
                                                    dominance->pdtDfn[pickedocc->mirbb->id.idx])) {
      pickedocc = nextRealocc;
    }
    if (nextLambdaresocc && (pickedocc == nullptr || *lambdaresdfnIt < dominance->pdtDfn[pickedocc->mirbb->id.idx])) {
      pickedocc = nextLambdaresocc;
    }
    if (nextEntryocc && (pickedocc == nullptr || dominance->pdtDfn[nextEntryocc->mirbb->id.idx] <
                                                     dominance->pdtDfn[pickedocc->mirbb->id.idx])) {
      pickedocc = nextEntryocc;
    }
    if (pickedocc != nullptr) {
      all_occs.push_back(pickedocc);
      switch (pickedocc->occty) {
        case kSOccReal:
        case kSOccKill:
          // get the next real/kill occ
          CHECK_FATAL(realoccIt != work_cand->real_occs.end(), "iterator check");
          realoccIt++;
          if (realoccIt != work_cand->real_occs.end()) {
            nextRealocc = *realoccIt;
          } else {
            nextRealocc = nullptr;
          }
          break;
        case kSOccEntry:
          CHECK_FATAL(entryoccIt != entry_occs.end(), "iterator check");
          entryoccIt++;
          if (entryoccIt != entry_occs.end()) {
            nextEntryocc = *entryoccIt;
          } else {
            nextEntryocc = nullptr;
          }
          break;
        case kSOccLambda:
          lambda_occs.push_back(static_cast<SLambdaOcc *>(pickedocc));
          CHECK_FATAL(lambdadfnIt != lambda_dfns.end(), "iterator check");
          lambdadfnIt++;
          if (lambdadfnIt != lambda_dfns.end()) {
            nextLambdaocc =
                ssuPreMempool->New<SLambdaOcc>(func->bbVec[dominance->pdtPreOrder[*lambdadfnIt].idx], &ssuPreAlloc);
          } else {
            nextLambdaocc = nullptr;
          }
          break;
        case kSOccLambdaRes:
          CHECK_FATAL(lambdaresdfnIt != lambdares_dfns.end(), "iterator check");
          lambdaresdfnIt++;
          if (lambdaresdfnIt != lambdares_dfns.end()) {
            nextLambdaresocc = ssuPreMempool->New<SLambdaResOcc>(func->bbVec[dominance->pdtPreOrder[*lambdaresdfnIt].idx]);
            std::unordered_map<BBId, std::forward_list<SLambdaResOcc *>>::iterator it =
                bb2lambdaresMap.find(dominance->pdtPreOrder[*lambdaresdfnIt]);
            if (it == bb2lambdaresMap.end()) {
              std::forward_list<SLambdaResOcc *> newlist = { nextLambdaresocc };
              bb2lambdaresMap[dominance->pdtPreOrder[*lambdaresdfnIt]] = newlist;
            } else {
              it->second.push_front(nextLambdaresocc);
            }
          } else {
            nextLambdaresocc = nullptr;
          }
          break;
        default:
          ASSERT(false, "CreateSortedOccs: unexpected occty");
      }
    }
  } while (pickedocc != nullptr);

  // initialize lambdaRes vector in each SLambdaOcc node
  for (SLambdaOcc *lambdaOcc : lambda_occs) {
    for (BB *succ : lambdaOcc->mirbb->succ) {
      SLambdaResOcc *lambdaResOcc = bb2lambdaresMap[succ->id].front();
      lambdaOcc->lambdaRes.push_back(lambdaResOcc);
      lambdaResOcc->use_lambdaocc = lambdaOcc;
      bb2lambdaresMap[succ->id].pop_front();
    }
  }

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << " _______ after lambda insertion _______" << std::endl;
    for (SOcc *occ : all_occs) {
      occ->Dump();
      LogInfo::MapleLogger() << std::endl;
    }
  }
}

void MeSSUPre::ApplySSUPre() {
  BuildWorkListBB(func->commonExitBB);
  if (prekind != k2ndDecrefPre) { // #0 build worklist
    CreateEmptyCleanupIntrinsics();
  }
  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "------ worklist initial size " << workcand_map.size() << std::endl;
  }
  int32 candnum = 0;
  for (std::pair<OStIdx, SSUPreWorkCand *> wkcandpair : workcand_map) {
    if (prekind == kStorePre) {
      if (wkcandpair.second->defCnt == SSUPreWorkCand::kZeroDef) {
        continue;
      }
    } else if (prekind == kDecrefPre) {
      if (wkcandpair.second->theost->isFormal && wkcandpair.second->defCnt == SSUPreWorkCand::kZeroDef) {
        continue;
      }
    }
    if (prekind == kDecrefPre && CandCanSkipSSUPre(wkcandpair.second)) {
      continue;
    }
    work_cand = wkcandpair.second;
    if (DEBUGFUNC(func)) {
      LogInfo::MapleLogger() << "||||||| SPRE candidate " << candnum << " (ostidx " << work_cand->theost->index.idx << "): ";
      work_cand->theost->Dump();
      LogInfo::MapleLogger() << std::endl;
    }
    PerCandInit();
    // #1 insert lambdas; results in all_occs and lambda_occs
    FormLambdas();  // result put in the set lambda_bbs
    CreateSortedOccs();
    // #2 rename
    Rename();
    if (!lambda_occs.empty()) {
      // #3 UpSafety
      ComputeUpsafe();
      // #4 CanBeAnt
      ComputeCanBeAnt();
      ComputeEarlier();
    }
    // #5 Finalize
    Finalize();
    // #6 Code Mmotion
    if (!work_cand->has_critical_edge) {
      CodeMotion();
    }

    candnum++;
  }
}

}  // namespace maple
