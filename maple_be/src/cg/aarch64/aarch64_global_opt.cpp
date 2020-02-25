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

#include "aarch64_global_opt.h"
#include "aarch64_store_load_opt.h"
#include "aarch64_isa.h"
#include "aarch64_operand.h"
#include "aarch64_insn.h"
#include "aarch64_cg_func.h"
#include "aarch64_cg.h"
#include <map>
#include "optimize_common.h"

namespace maplebe {

using namespace maple;

void AArch64GlobalOpt::Run() {
  if (!cgfunc->GetSBB()) {
    BackPropOfRegister();
  }
  ForwardPropOfRegister();
  ConvertCselToCset();
  CmpCsetOptimize();
  DeleteRedundantUxt();
}

/**
 *this aim to find the pattern:
 *insn1: mop vreg1, vreg2
 *insn2: mop vreg3, vreg4
 *...
 *
 **insn3: mov vreg4, vreg1 // target
 **insn4: mop vreg4, ....
 **insn5: mop .... , vreg1
 **If do bp of vreg4 in insn3 (replace all vreg1 with vreg4) will change the defination of vreg4 in insn2 and insn5
 **So, if this pattern appear, will not do back prop
 * is_bg shows it is back_prop or forward prop
 **target_insn is mov so use opndidx: 0 as dest_idx, opndidx: 1 as use_idx
 */
bool AArch64GlobalOpt::NoOverlap(Insn *targetInsn, int opndidx, bool isBg) {
  CHECK_FATAL((targetInsn->GetMachineOpcode() == MOP_xmovrr || targetInsn->GetMachineOpcode() == MOP_wmovrr) &&
           targetInsn->defs[1],
         "must be");
  BB *curBb = targetInsn->bb;
  Insn *defInsn = targetInsn->defs[1]->begin()->GetInsn();
  int defIndex = targetInsn->defs[1]->begin()->GetDUIndex();

  int defId = defInsn->id;
  if (isBg) {
    if (targetInsn->predefine[0] == nullptr && targetInsn->redefine[0] == nullptr) {
      return true;
    }
    if (targetInsn->predefine[0]) {
      for (auto preDef : *(targetInsn->predefine[0])) {
        Insn *preDefInsn = preDef.GetInsn();
        int preDefIndex = preDef.GetDUIndex();
        if (preDefInsn->uses[preDefIndex] == nullptr) {
          continue;
        }
        for (auto preUse : *(preDefInsn->uses[preDefIndex])) {
          Insn *preUseInsn = preUse.GetInsn();
          if (preUseInsn->bb == curBb && preUseInsn->id > defId) {
            return false;
          }
        }
      }
    }
    if (targetInsn->redefine[0]) {
      for (auto reDef : *(targetInsn->redefine[0])) {
        Insn *reDefInsn = reDef.GetInsn();
        if (reDefInsn->bb != curBb) {
          continue;
        }
        for (auto defUse : *(defInsn->uses[defIndex])) {
          Insn *defUseInsn = defUse.GetInsn();
          CHECK_FATAL(defUseInsn->bb == curBb, "must in same bb");
          if (defUseInsn->id > reDefInsn->id) {
            return false;
          }
        }
      }
    }
  } else {
    if (defInsn->redefine[defIndex] == nullptr) {
      return true;
    }
    for (auto redef : *(defInsn->redefine[defIndex])) {
      Insn *reDef = redef.GetInsn();
      for (auto use : *(targetInsn->uses[0])) {
        Insn *useInsn = use.GetInsn();
        if (reDef->bb == useInsn->bb) {
          if (reDef->id < useInsn->id) {
            return false;
          }
        } else {
          if (FindAThroughPath(reDef, useInsn)) {
            return false;
          }
        }
      }
    }
  }
  return true;
}

/*
 *try to find a path for insn_start to insn_end
 *if exist return true
 */
bool AArch64GlobalOpt::FindAThroughPath(Insn *insnStart, Insn *insnEnd) {
  BB *bbStart = insnStart->bb;
  BB *bbEnd = insnEnd->bb;
  if (bbStart == bbEnd) {
    if (insnStart->id < insnEnd->id) {
      return true;
    }
  }
  bool found = false;
  std::set<BB *> traversed;
  traversed.insert(bbStart);
  for (auto succ : bbStart->succs) {
    found = found || TraverseNext(succ, bbEnd, &traversed);
  }
  for (auto ehSucc : bbStart->eh_succs) {
    found = found || TraverseNext(ehSucc, bbEnd, &traversed);
  }
  return found;
}

bool AArch64GlobalOpt::TraverseNext(BB *curBb, BB *target, std::set<BB *> *traversedBb) {
  if (curBb == target) {
    return true;
  }
  traversedBb->insert(curBb);
  bool found = false;
  for (auto succ : curBb->succs) {
    if (succ != target && traversedBb->find(succ) != traversedBb->end()) {
      continue;
    }
    found = found || TraverseNext(succ, target, traversedBb);
  }
  for (auto ehSucc : curBb->eh_succs) {
    if (ehSucc != target && traversedBb->find(ehSucc) != traversedBb->end()) {
      continue;
    }
    found = found || TraverseNext(ehSucc, target, traversedBb);
  }
  return found;
}

/*
 *If one of the relative insn have multigen prop return true
 */
bool AArch64GlobalOpt::HasMultiGen(Insn *newInsn) {
  CHECK_FATAL(newInsn->GetMachineOpcode() == MOP_xmovrr || newInsn->GetMachineOpcode() == MOP_wmovrr, "MUST BE");
  bool foundSelf = false;
  for (auto def : *(newInsn->defs[1])) {
    Insn *defInsn = def.GetInsn();
    unsigned short defProp = def.GetProperty();
    if (defProp & DataInsnInfo::kMultigen) {
      return true;
    }
    int defIndex = def.GetDUIndex();
    CHECK_FATAL(defInsn->uses[defIndex], "impossible");
    for (auto defUse : *(defInsn->uses[defIndex])) {
      if (defUse.GetInsn() == newInsn) {
        foundSelf = true;
      }
      unsigned short defUseProp = defUse.GetProperty();
      if (defUseProp & DataInsnInfo::kMultigen) {
        return true;
      }
    }
  }
  CHECK_FATAL(foundSelf, "impossible");
  return false;
}

/*
 * Do Forward prop when insn is mov
 * mov xx, x1
 * ... /// BBs and x1 is live
 * mop yy, xx
 *
 **=>
 * mov x1, x1
 * ... /// BBs and x1 is live
 * mop yy, x1
 */
void AArch64GlobalOpt::ForwardPropOfRegister() {
  bool secondTime = false;
  std::set<BB *> modifiedBb;
  do {
    FOR_ALL_BB(bb, cgfunc) {
      if (bb->unreachable || (secondTime && modifiedBb.find(bb) == modifiedBb.end())) {
        continue;
      }
      if (secondTime) {
        modifiedBb.erase(bb);
      }
      FOR_BB_INSNS(insn, bb) {
        if (!insn->IsMachineInstruction()) {
          continue;
        }
        if (insn->GetMachineOpcode() != MOP_xmovrr && insn->GetMachineOpcode() != MOP_wmovrr) {
          continue;
        }
        if (!insn->opnds[1]->IsZeroRegister() && static_cast<RegOperand *>(insn->opnds[1])->IsVirtualRegister() &&
            static_cast<RegOperand *>(insn->opnds[0])->IsVirtualRegister() && insn->uses[0] != nullptr) {
          if (insn->defs[1] != nullptr && !IsSameReg(insn->opnds[1], insn->opnds[0]) && OnlyOneDefine(insn) &&
              NoOverlap(insn, 0, false)) {
            if (HasMultiGen(insn)) {
              continue;
            }
            for (auto use : *(insn->uses[0])) {
              Insn *useInsn = use.GetInsn();
              short i = use.GetIndex();
              CHECK_FATAL(i >= 0, "must not be call insn");
              if (useInsn->opnds[i]->IsRegister()) {
                CHECK_FATAL(IsSameReg(useInsn->opnds[i], insn->opnds[0]), "must equal");
                useInsn->SetOperand(i, insn->opnds[1]);
                if ((useInsn->GetMachineOpcode() == MOP_xmovrr || useInsn->GetMachineOpcode() == MOP_wmovrr) &&
                    static_cast<RegOperand *>(useInsn->opnds[1])->IsVirtualRegister() &&
                    static_cast<RegOperand *>(useInsn->opnds[0])->IsVirtualRegister()) {
                  modifiedBb.insert(useInsn->bb);
                }
              } else if (useInsn->opnds[i]->IsMemoryAccessOperand()) {
                MemOperand *mem = static_cast<MemOperand *>(useInsn->opnds[i]);
                bool memBaseSame = mem->GetBaseRegister() && IsSameReg(mem->GetBaseRegister(), insn->opnds[0]);
                bool memIndexSame = mem->GetIndexRegister() && IsSameReg(mem->GetIndexRegister(), insn->opnds[0]);
                if (memBaseSame || memIndexSame) {
                  MemOperand *newmem = static_cast<MemOperand *>(mem->Clone(cgfunc->memPool));
                  CG_ASSERT(newmem != nullptr, "null ptr check");
                  if (memBaseSame) {
                    newmem->SetBaseRegister(static_cast<RegOperand *>(insn->opnds[1]));
                  }
                  if (memIndexSame) {
                    newmem->SetIndexRegister(static_cast<RegOperand *>(insn->opnds[1]));
                  }
                  useInsn->SetOperand(i, newmem);
                }
              } else {
                CHECK_FATAL(false, "impossible case");
              }
            }
            CleanDDForDestOperand(insn, 0);
            InsertDUUDForDestOperand(insn, 0, DataInsnInfo::kNormal, true);
            insn->SetOperand(0, insn->opnds[1]);
          }
        }
      }
    }
    secondTime = true;
  } while (!modifiedBb.empty());
}

/*
 * Replace new_insn's dest opnd with its src opnd.
 * After replacing, maintain the UDDU and WAW of the new dest opnd
 * new_insn : mov insn
 * isRefInsnBeforeNewInsn : indicate new_insn is inserted after refInsn or before
 * new_index and new_prop : not used right now, but will used in future
 *
 */
void AArch64GlobalOpt::InsertDUUDForDestOperand(Insn *newInsn, short newIndex, unsigned short newProp,
                                                bool isRefInsnBeforeNewInsn) {
  CHECK_FATAL(newInsn, "CG internal error, new_insn should not be nullptr.");
  std::set<Insn *> middleInsnSet;
  MapleSet<DataInsnInfo, DataInsnInfoCmp>::iterator itSet;    // for all others
  MapleSet<DataInsnInfo, DataInsnInfoCmp2>::iterator itSet2;  // for use
  if (isRefInsnBeforeNewInsn) {
    CHECK_FATAL(newInsn->defs[1], "impossible");

    for (auto def : *(newInsn->defs[1])) {
      Insn *defInsn = def.GetInsn();
      short defI = def.GetIndex();
      CHECK_FATAL(defI >= 0, "def_insn is not call insn");
      if (defInsn->opnds[defI]->IsRegister()) {
        CHECK_FATAL(IsSameReg(defInsn->opnds[defI], newInsn->opnds[1]), "must equal");
      }
      int defIndex = def.GetDUIndex();
      CHECK_FATAL(defInsn->uses[defIndex], "impossible");
      // update ref's use and new insn's use && ref's use's def && def_insn's use
      std::vector<DataInsnInfo> tmp;
      for (auto defUse : *(defInsn->uses[defIndex])) {
        tmp.push_back(defUse);
      }
      for (auto defUse : tmp) {
        Insn *defUseInsn = defUse.GetInsn();
        if (defUseInsn == newInsn) {
          continue;
        }
        short defUseI = defUse.GetIndex();
        CHECK_FATAL(defUseI >= 0, "def_use_insn can not be call");
        if (defUseInsn->opnds[defUseI]->IsRegister() && defInsn->opnds[defI]->IsRegister())
          CHECK_FATAL(IsSameReg(defUseInsn->opnds[defUseI], defInsn->opnds[defI]), "must equal");
        int defUseIndex = defUse.GetUDIndex();
        /// Insn middle dominate Insn end when begin from Insn start
        middleInsnSet.clear();
        middleInsnSet.insert(newInsn);
        for (auto defUseDef : *(defUseInsn->defs[defUseIndex])) {
          Insn *defUseDefInsn = defUseDef.GetInsn();
          if (defUseDefInsn != defInsn) {
            middleInsnSet.insert(defUseDefInsn);
          }
        }
        if (IsPartialDominate(defInsn, defUseInsn, &middleInsnSet)) {
          // remove def_use_insn from def_insn's use set
          itSet2 = defInsn->uses[defIndex]->find(defUse);
          defInsn->uses[defIndex]->erase(itSet2);
          // remove def_insn from def_use_insn's def set
          itSet = defUseInsn->defs[defUseIndex]->find(def);
          defUseInsn->defs[defUseIndex]->erase(itSet);
          // add def_use_insn into new_insn's use set
          newInsn->uses[0]->insert(defUse);
          // add new_insn into def_use_insn's def set
          defUseInsn->defs[defUseIndex]->insert(DataInsnInfo(newInsn, 0));
        } else if (FindAThroughPath(newInsn, defUseInsn)) {
          middleInsnSet.clear();
          for (auto defUseDef : *(defUseInsn->defs[defUseIndex])) {
            Insn *defUseDefInsn = defUseDef.GetInsn();
            middleInsnSet.insert(defUseDefInsn);
          }
          if (!IsPartialDominate(newInsn, defUseInsn, &middleInsnSet)) {
            // add def_use_insn into new_insn's use set
            newInsn->uses[0]->insert(defUse);
            // add new_insn into def_use_insn's def set
            defUseInsn->defs[defUseIndex]->insert(DataInsnInfo(newInsn, 0));
          }
        }
      }
      // update predef of new
      if (newInsn->predefine[0] == nullptr) {
        newInsn->predefine[0] =
          GetRD()->GetMemPool()->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(GetRD()->GetMemAllocator().Adapter());
      }
      newInsn->predefine[0]->insert(def);
      if (newInsn->redefine[0] == nullptr) {
        newInsn->redefine[0] =
          GetRD()->GetMemPool()->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(GetRD()->GetMemAllocator().Adapter());
      }
      tmp.clear();
      if (defInsn->redefine[defIndex]) {
        for (auto defUse : *(defInsn->redefine[defIndex])) {
          tmp.push_back(defUse);
        }
      }
      // update redef of def_insn
      if (defInsn->redefine[defIndex] == nullptr) {
        defInsn->redefine[defIndex] =
          GetRD()->GetMemPool()->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(GetRD()->GetMemAllocator().Adapter());
      }
      defInsn->redefine[defIndex]->insert(DataInsnInfo(newInsn, 0));
      // update redef of new && update redef of def_insn
      for (auto defRedef : tmp) {
        Insn *defRedefInsn = defRedef.GetInsn();
        short defRedefI = defRedef.GetIndex();
        CHECK_FATAL(defRedefI >= 0, "def_redef_insn must not be call insn");
        if (defRedefInsn->opnds[defRedefI]->IsRegister() && defInsn->opnds[defI]->IsRegister())
          CHECK_FATAL(IsSameReg(defRedefInsn->opnds[defRedefI], defInsn->opnds[defI]), "must equal");
        int defRedefIndex = defRedef.GetDUIndex();
        /// Insn middle dominate Insn end when begin from Insn start
        middleInsnSet.clear();
        middleInsnSet.insert(newInsn);
        for (auto defRedefPredef : *(defRedefInsn->predefine[defRedefIndex])) {
          Insn *defRedefPredefInsn = defRedefPredef.GetInsn();
          if (defRedefPredefInsn != defInsn) {
            middleInsnSet.insert(defRedefPredefInsn);
          }
        }
        if (IsPartialDominate(defInsn, defRedefInsn, &middleInsnSet)) {
          // remove def_redef_insn from def_insn's redef set
          itSet = defInsn->redefine[defIndex]->find(defRedef);
          defInsn->redefine[defIndex]->erase(itSet);
          // remove def_insn from def_redef_insn's predef set
          itSet = defRedefInsn->predefine[defRedefIndex]->find(def);
          defRedefInsn->predefine[defRedefIndex]->erase(itSet);
          // add def_redef_insn into new_insn's redef set
          newInsn->redefine[0]->insert(defRedef);
          // add new_insn into def_redef_insn's predef set
          defRedefInsn->predefine[defRedefIndex]->insert(DataInsnInfo(newInsn, 0));
        } else if (FindAThroughPath(newInsn, defRedefInsn)) {
          middleInsnSet.clear();
          for (auto defRedefPredef : *(defRedefInsn->predefine[defRedefIndex])) {
            Insn *defRedefPredefInsn = defRedefPredef.GetInsn();
            middleInsnSet.insert(defRedefPredefInsn);
          }
          if (!IsPartialDominate(newInsn, defRedefInsn, &middleInsnSet)) {
            // add def_redef_insn into new_insn's redef set
            newInsn->redefine[0]->insert(defRedef);
            // add new_insn into def_redef_insn's predef set
            defRedefInsn->predefine[defRedefIndex]->insert(DataInsnInfo(newInsn, 0));
          }
        }
      }
    }

    middleInsnSet.clear();
    for (auto def : *(newInsn->defs[1])) {
      Insn *defInsn = def.GetInsn();
      middleInsnSet.insert(defInsn);
    }
    if (newInsn->bb->loop && !IsPartialDominate(newInsn, newInsn, &middleInsnSet)) {
      newInsn->predefine[0]->insert(DataInsnInfo(newInsn, 0));
      newInsn->redefine[0]->insert(DataInsnInfo(newInsn, 0));
      newInsn->defs[1]->insert(DataInsnInfo(newInsn, 0));
      newInsn->uses[0]->insert(DataInsnInfo(newInsn, 1));
    }
  } else {
    CHECK_FATAL(false, "Not supported yet");
  }
}

bool AArch64GlobalOpt::TraverseDomNext(BB *start, BB *end, std::set<BB *> *middleSet, std::set<uint32_t> *traversed) {
  if (start == end) {
    return false;
  }
  for (auto middle : *middleSet) {
    if (start == middle) {
      return true;
    }
  }
  traversed->insert(start->id);
  for (auto succ : start->succs) {
    if (succ != end && traversed->find(succ->id) != traversed->end()) {
      continue;
    }
    if (!TraverseDomNext(succ, end, middleSet, traversed)) {
      return false;
    }
  }
  for (auto ehSucc : start->eh_succs) {
    if (ehSucc != end && traversed->find(ehSucc->id) != traversed->end()) {
      continue;
    }
    if (!TraverseDomNext(ehSucc, end, middleSet, traversed)) {
      return false;
    }
  }
  return true;
}

/*
 * if Insn middle partial dominate Insn end when begin from Insn start, return true
   partial dominate : can not find a path from start to end that does not pass through middle_insn
   prerequirement : exist a path from start to end
   return true : Is partial dominate
   return false : Is NOT partial dominate
 */
bool AArch64GlobalOpt::IsPartialDominate(Insn *startInsn, Insn *endInsn, std::set<Insn *> *middleInsnSet) {
  BB *start = startInsn->bb;
  BB *end = endInsn->bb;
  std::set<BB *> middleSet;
  for (auto middleInsn : *middleInsnSet) {
    BB *middle = middleInsn->bb;
    middleSet.insert(middle);
    if (start == middle && middle != end) {
      if (startInsn->id <= middleInsn->id) {
        return true;
      } else {
        middleSet.erase(middle);
        continue;
      }
    } else if (start != middle && middle == end) {
      if (middleInsn->id <= endInsn->id) {
        return true;
      } else {
        middleSet.erase(middle);
        continue;
      }
    } else if (start == middle && start == end) {
      CHECK_FATAL(middleInsn != endInsn, "check what happens");
      if (middleInsn == startInsn) {
        return true;
      }
      if (endInsn != startInsn) {
        if ((middleInsn->id < endInsn->id && startInsn->id < middleInsn->id) ||
            (startInsn->id > endInsn->id && middleInsn->id < endInsn->id)) {
          return true;
        } else {
          middleSet.erase(middle);
          continue;
        }
      } else {
        return true;
      }
    } else if (start == end && end != middle) {
      if (startInsn != endInsn && startInsn->id < endInsn->id) {
        middleSet.erase(middle);
        continue;
      }
      // if start_insn == end_insn || start_insn->id > end_insn->id  fall through
    } else {
      CHECK_FATAL(start != end && start != middle && middle != end, "must be");
    }
  }
  if (middleSet.empty()) {
    return false;
  }
  std::set<uint32_t> traversed;
  traversed.insert(start->id);
  for (auto succ : start->succs) {
    if (!TraverseDomNext(succ, end, &middleSet, &traversed)) {
      return false;
    }
  }
  for (auto ehSucc : start->eh_succs) {
    if (!TraverseDomNext(ehSucc, end, &middleSet, &traversed)) {
      return false;
    }
  }
  return true;
}

bool AArch64GlobalOpt::OnlyOneDefine(Insn *targetInsn) {
  for (auto use : *(targetInsn->uses[0])) {
    Insn *useInsn = use.GetInsn();
    short useIdx = use.GetUDIndex();
    unsigned short useProp = use.GetProperty();
    if (useProp & DataInsnInfo::kMultigen) {
      return false;
    }
    if (useInsn->defs[useIdx]->size() != 1) {
      return false;
    }
  }
  return true;
}

bool AArch64GlobalOpt::OpndOnlyDefInCurBB(Insn *targetInsn, int opndidx) {
  BB *curBb = targetInsn->bb;
  if (targetInsn->defs[opndidx] == nullptr) {
    return false;
  }
  for (auto def : *(targetInsn->defs[opndidx])) {
    if (def.GetInsn()->bb != curBb) {
      return false;
    }
  }
  return true;
}

/*
 * connect insn->predefinsn and insn->redefinsn
 * delete insn->predef and insn->redef
 * insn : target
 * index : indicate the opnd
 */
void AArch64GlobalOpt::CleanDDForDestOperand(Insn *insn, unsigned short index) {
  if (insn->predefine[index] != nullptr &&
      insn->predefine[index]->find(DataInsnInfo(insn, index)) != insn->predefine[index]->end()) {
    CHECK_FATAL(insn->redefine[index]->find(DataInsnInfo(insn, index)) != insn->redefine[index]->end(), "MUST NOT BE");
    insn->predefine[index]->erase(DataInsnInfo(insn, index));
    insn->redefine[index]->erase(DataInsnInfo(insn, index));
  }
  // Rebuild redefine.
  if (insn->predefine[index] != nullptr) {
    for (auto predefInsnInfo : *(insn->predefine[index])) {
      Insn *predefInsn = predefInsnInfo.GetInsn();
      int idx = predefInsnInfo.GetDUIndex();

      CG_ASSERT(predefInsn->redefine[idx], "CG internal error.");

      predefInsn->redefine[idx]->erase(DataInsnInfo(insn, index));

      if (insn->redefine[index]) {
        for (auto redefInsnInfo : *(insn->redefine[index])) {
          predefInsn->redefine[idx]->insert(redefInsnInfo);
          Insn *redefInsn = redefInsnInfo.GetInsn();

          // Call insn do not have predefine.
          if (!redefInsn->IsCall()) {
            int redefIdx = redefInsnInfo.GetDUIndex();

            CG_ASSERT(redefInsn->predefine[redefIdx], "CG internal error.");

            redefInsn->predefine[redefIdx]->insert(predefInsnInfo);
          }
        }
      }
    }
    insn->predefine[index]->clear();
    insn->predefine[index] = nullptr;
  }

  // Remove predefine from redefInsn to insn.
  if (insn->redefine[index]) {
    for (auto redefInsnInfo : *(insn->redefine[index])) {
      Insn *redefInsn = redefInsnInfo.GetInsn();

      // Call insn do not have predefine.
      if (!redefInsn->IsCall()) {
        int redefIdx = redefInsnInfo.GetDUIndex();

        CG_ASSERT(redefInsn->predefine[redefIdx], "CG internal error.");

        redefInsn->predefine[redefIdx]->erase(DataInsnInfo(insn, index));
      }
    }
    insn->redefine[index]->clear();
    insn->redefine[index] = nullptr;
  }
}

/*
 * Do back propagate of vreg/preg when encount following insn:
 *
 * mov vreg/preg1, vreg2
 *
 * back propagate reg1 to all vreg2's use points and def points, when all of them is in same bb
 */
void AArch64GlobalOpt::BackPropOfRegister() {
  bool secondTime = false;
  std::set<BB *> modifiedBb;
  do {
    FOR_ALL_BB(bb, cgfunc) {
      if (bb->unreachable || (secondTime && modifiedBb.find(bb) == modifiedBb.end())) {
        continue;
      }
      if (secondTime) {
        modifiedBb.erase(bb);
      }
      FOR_BB_INSNS_REV(insn, bb) {
        if (!insn->IsMachineInstruction() ||
            (insn->GetMachineOpcode() != MOP_xmovrr && insn->GetMachineOpcode() != MOP_wmovrr) ||
            IsSameReg(insn->opnds[0], insn->opnds[1]) || static_cast<RegOperand *>(insn->opnds[0])->IsZeroRegister() ||
            !static_cast<RegOperand *>(insn->opnds[1])->IsVirtualRegister() || insn->uses[0] == nullptr ||
            !OpndOnlyDefInCurBB(insn, 1) || !OpndOnlyUsedInCurBB(insn, 1, false) || !NoOverlap(insn, 1, true)) {
          continue;
        }
        CHECK_FATAL(insn->defs[1]->size() == 1, "def point must be 1");
        Insn *defInsn = insn->defs[1]->begin()->GetInsn();
        unsigned short defProp = insn->defs[1]->begin()->GetProperty();
        short defI = insn->defs[1]->begin()->GetIndex();
        CHECK_FATAL(defI >= 0, "MUST NOT BE CALL INSN");
        int defIndex = insn->defs[1]->begin()->GetDUIndex();
        if (HasMultiGen(insn) || defInsn->GetMachineOpcode() == MOP_xmovkri16 ||
            defInsn->GetMachineOpcode() == MOP_wmovkri16) {
          continue;
        }
        int targetId = insn->id;
        CHECK_FATAL(IsSameReg(defInsn->opnds[defI], insn->opnds[1]), "must equal");
        MapleSet<DataInsnInfo, DataInsnInfoCmp2> tmp(*(defInsn->uses[defIndex]));
        modifiedBb.insert(bb);
        for (auto use : tmp) {
          Insn *useInsn = use.GetInsn();
          short i = use.GetIndex();
          CHECK_FATAL(i >= 0, "must not be call insn");
          CHECK_FATAL(useInsn->opnds[i], "Can not be null");
          if (useInsn->opnds[i]->IsRegister()) {
            CHECK_FATAL(IsSameReg(useInsn->opnds[i], defInsn->opnds[defI]), "must equal");
            if (useInsn->id > targetId) {
              ReplaceInsnSrcOperand(useInsn, i, use.GetProperty(), insn->opnds[0], insn);
            } else {
              useInsn->SetOperand(i, insn->opnds[0]);
            }
          } else if (useInsn->opnds[i]->IsMemoryAccessOperand()) {
            MemOperand *mem = static_cast<MemOperand *>(useInsn->opnds[i]);
            bool memBaseSame = mem->GetBaseRegister() && IsSameReg(mem->GetBaseRegister(), defInsn->opnds[defI]);
            bool memIndexSame = mem->GetIndexRegister() && IsSameReg(mem->GetIndexRegister(), defInsn->opnds[defI]);
            if (memBaseSame || memIndexSame) {
              if (useInsn->id > targetId) {
                ReplaceInsnSrcOperand(useInsn, i, use.GetProperty(), insn->opnds[0], insn);
              } else {
                MemOperand *newmem = static_cast<MemOperand *>(mem->Clone(cgfunc->memPool));
                CG_ASSERT(newmem != nullptr, "null ptr check");
                if (memBaseSame) {
                  newmem->SetBaseRegister(static_cast<RegOperand *>(insn->opnds[0]));
                }
                if (memIndexSame) {
                  newmem->SetIndexRegister(static_cast<RegOperand *>(insn->opnds[0]));
                }
                useInsn->SetOperand(i, newmem);
              }
            }
          } else {
            CHECK_FATAL(false, "impossible case");
          }
        }
        CleanDDForDestOperand(defInsn, defIndex);
        GetRD()->InsertDUUDForDestOperand(defInsn, defI, defProp, insn, 0, DataInsnInfo::kNormal, false);
        defInsn->SetOperand(0, insn->opnds[0]);
      }
    }
    secondTime = true;
  } while (!modifiedBb.empty());
}

/*
 * replace m_insn i th opnd with new_opnd, and the new_opnd's def point is def_insn.
 * m_insn and def_insn is in the same bb
 * def_insn is mov
 */
void AArch64GlobalOpt::ReplaceInsnSrcOperand(Insn *mInsn, short i, unsigned short prop, Operand *newOpnd,
                                             Insn *defInsn) {
  CHECK_FATAL(i >= 0, "must not be call insn");
  CHECK_FATAL(mInsn->bb == defInsn->bb, "must in the same bb");
  int mIndex = i + (prop & DataInsnInfo::kIndexQuickcalc);
  MapleSet<DataInsnInfo, DataInsnInfoCmp2>::iterator itSet2;
  if (mInsn->defs[mIndex]) {
    // Remove old def->use.
    for (auto udInsnInfo : *(mInsn->defs[mIndex])) {
      Insn *defInsn = udInsnInfo.GetInsn();
      int duIdx = udInsnInfo.GetDUIndex();

      CG_ASSERT(defInsn->uses[duIdx], "CG internal error, defInsn should have uses.");

      itSet2 = defInsn->uses[duIdx]->find(DataInsnInfo(mInsn, i, prop & DataInsnInfo::kCallParam));
      CG_ASSERT(itSet2 != defInsn->uses[duIdx]->end(), "CG internal error, defInsn should have insn use.");
      if (itSet2 != defInsn->uses[duIdx]->end()) {
        defInsn->uses[duIdx]->erase(itSet2);
        if (defInsn->uses[duIdx]->empty()) {
          defInsn->uses[duIdx] = nullptr;
        }
      }
    }

    // Remove all use->def.
    mInsn->defs[mIndex]->clear();
  }
  if (mInsn->defs[mIndex] == nullptr) {
    mInsn->defs[mIndex] =
      GetRD()->GetMemPool()->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(GetRD()->GetMemAllocator().Adapter());
  }
  if (defInsn->uses[0] == nullptr) {
    defInsn->uses[0] =
      GetRD()->GetMemPool()->New<MapleSet<DataInsnInfo, DataInsnInfoCmp2>>(GetRD()->GetMemAllocator().Adapter());
  }

  defInsn->uses[0]->insert(DataInsnInfo(mInsn, i, prop));
  mInsn->defs[mIndex]->insert(DataInsnInfo(defInsn, 0));

  switch (prop & DataInsnInfo::kAnyIndex) {
    case DataInsnInfo::kNormal:
      mInsn->opnds[i] = newOpnd;
      break;
    case DataInsnInfo::kSecondMemAddr:
      CG_ASSERT(false, "CG internal error, Could not replace the second memory address.");
      break;
    case DataInsnInfo::kBaseReg: {
      CG_ASSERT(newOpnd->IsRegister(), "CG Internal error, new_opnd should be a register operand.");
      CG_ASSERT(mInsn->opnds[i]->IsMemoryAccessOperand(),
                "CG Internal error, insn->opnds[index] should be a memory operand.");

      MemOperand *memOpnd = static_cast<MemOperand *>(static_cast<MemOperand *>(mInsn->opnds[i])->Clone(cgfunc->memPool));
      CG_ASSERT(memOpnd != nullptr, "null ptr check");
      mInsn->SetOperand(i, memOpnd);
      memOpnd->SetBaseRegister(static_cast<RegOperand *>(newOpnd));
      break;
    }
    case DataInsnInfo::kIndexReg: {
      CG_ASSERT(newOpnd->IsRegister(), "CG Internal error, new_opnd should be a register operand.");
      CG_ASSERT(mInsn->opnds[i]->IsMemoryAccessOperand(),
                "CG Internal error, insn->opnds[index] should be a memory operand.");

      MemOperand *memOpnd = static_cast<MemOperand *>(static_cast<MemOperand *>(mInsn->opnds[i])->Clone(cgfunc->memPool));
      mInsn->SetOperand(i, memOpnd);
      break;
    }
    case DataInsnInfo::kCallParam:
      CG_ASSERT(false, "CG internal error, we don't need to replace call params.");
      break;
    default:
      CG_ASSERT(false, "CG invalid property.");
      break;
  }
}

/*uxtb  w0, w0    -->   null
 *uxth  w0, w0    -->   null
 *
 * condition:
 * 1. validbits(w0)<=8,16,32
 * 2. the first operand is same as the second operand
 *
 **uxtb  w0, w1    -->   null
 **uxth  w0, w1    -->   null
 *
 * condition:
 * 1. validbits(w1)<=8,16,32
 * 2. the use points of w0 has only one define point, that is uxt w0, w1
 */

void AArch64GlobalOpt::DeleteRedundantUxt() {
  FOR_ALL_BB(bb, cgfunc) {
    if (bb->unreachable) {
      continue;
    }
    FOR_BB_INSNS_SAFE(insn, bb, ninsn) {
      std::set<Insn *> insnChecked1;
      std::set<Insn *> insnChecked2;
      if ((insn->GetMachineOpcode() == MOP_xuxth32 && GetMaximumValidBit(insn, 1, insnChecked1) <= 16) ||
          (insn->GetMachineOpcode() == MOP_xuxtb32 && GetMaximumValidBit(insn, 1, insnChecked2) <= 8)) {
        Operand *firstOpnd = insn->GetOperand(0);
        Operand *secondOpnd = insn->GetOperand(1);
        if (IsSameReg(firstOpnd, secondOpnd)) {
          GetRD()->RemoveDUUDForInsn(insn);
          bb->RemoveInsn(insn);
        } else {
          if (!insn->uses[0]) {
            GetRD()->RemoveDUUDForInsn(insn);
            bb->RemoveInsn(insn);
            continue;
          }

          CG_ASSERT(insn->uses[0], "Operand must be defined before used");
          bool toDoOpt = true;
          vector<DataInsnInfo> useinfos;
          for (auto useInfo : *(insn->uses[0])) {
            Insn *useInsn = useInfo.GetInsn();
            int udidx = useInfo.GetUDIndex();
            useinfos.push_back(useInfo);
            if (useInsn->defs[udidx]->size() > 1) {
              toDoOpt = false;
              break;
            }
          }
          if (!toDoOpt) {
            continue;
          }
          for (vector<DataInsnInfo>::iterator itr = useinfos.begin(); itr != useinfos.end(); itr++) {
            GetRD()->ReplaceInsnSrcOperand(itr->GetInsn(), itr->GetIndex(), itr->GetProperty(), insn->GetOperand(1),
                                           insn, 1, DataInsnInfo::kNormal);
          }
          GetRD()->RemoveDUUDForInsn(insn);
          bb->RemoveInsn(insn);
        }
      }
    }
  }
}

/* condition:
 *   1. if operand is defined by mov, find the defination of it's src continually
 *
 **NOTE:
 *   set insn_checked is used to avoid loop defination,leading to program can't exit
 *   for example:
 *
 *   mov reg1, reg2 <-------
 *   ....                  |
 *   uxth reg 3, reg2      |
 *   ....                  |
 *   mov reg2, reg1    ----|
 */
int AArch64GlobalOpt::GetMaximumValidBit(Insn *insn, short udidx, std::set<Insn *> &insnChecked) const {
  // CG_ASSERT(insn->defs[udidx], ",wrong arguments, operand must be defined before used");
  if (insn->defs[udidx] == nullptr) {
    return 64;
  }

  int validBit;
  int nMaxValidBit = 0;

  for (auto defInfo : *(insn->defs[udidx])) {
    Insn *defInsn = defInfo.GetInsn();
    // insn_checked is used to avoid loop definition
    if (insnChecked.find(defInsn) == insnChecked.end()) {
      insnChecked.insert(defInsn);
    } else {
      continue;
    }

    MOperator mop = defInsn->GetMachineOpcode();
    if (mop == MOP_wmovrr || mop == MOP_xmovrr) {
      validBit = GetMaximumValidBit(defInsn, 1, insnChecked);
    } else {
      validBit = GetInsnValidBit(defInsn);
    }

    nMaxValidBit = nMaxValidBit < validBit ? validBit : nMaxValidBit;

    if (defInfo.IsMulGen()) {
      DataInsnInfo *insnInfo = defInfo.GetPrevMultiGenInsn();
      CG_ASSERT(insnInfo, "get prev multigen insninfo failed");
      do {
        Insn *insn = insnInfo->GetInsn();
        // insn_checked is used to avoid loop definition
        if (insnChecked.find(insn) == insnChecked.end()) {
          insnChecked.insert(insn);
        } else {
          insnInfo = insnInfo->GetPrevMultiGenInsn();
          continue;
        }

        mop = defInsn->GetMachineOpcode();
        if (mop == MOP_wmovrr || mop == MOP_xmovrr) {
          validBit = GetMaximumValidBit(defInsn, 1, insnChecked);
        } else {
          validBit = GetInsnValidBit(defInsn);
        }

        nMaxValidBit = nMaxValidBit < validBit ? validBit : nMaxValidBit;

        insnInfo = insnInfo->GetPrevMultiGenInsn();
      } while (insnInfo);
    }
  }

  return nMaxValidBit;
}

int AArch64GlobalOpt::GetInsnValidBit(Insn *insn) {
  MOperator mop = insn->GetMachineOpcode();
  int nRet;
  switch (mop) {
    case MOP_wcsetrc:
    case MOP_xcsetrc:
      nRet = 1;
      break;
    case MOP_wldrb:
    case MOP_wldrsb:
    case MOP_wldarb:
    case MOP_wldlarb:
    case MOP_wldaprb:
    case MOP_wldxrb:
    case MOP_wldaxrb:
      nRet = 8;
      break;
    case MOP_wldrh:
    case MOP_wldrsh:
    case MOP_wldarh:
    case MOP_wldlarh:
    case MOP_wldaprh:
    case MOP_wldxrh:
    case MOP_wldaxrh:
      nRet = 16;
      break;
    case MOP_wmovrr:
    case MOP_xmovri32:
    case MOP_wldli:
    case MOP_wldr:
    case MOP_wldp:
    case MOP_wldar:
    case MOP_wldlar:
    case MOP_wldapr:
    case MOP_wmovkri16:
    case MOP_wmovzri16:
    case MOP_wmovnri16:
    case MOP_wldxr:
    case MOP_wldaxr:
    case MOP_wldaxp:
    case MOP_wcsincrrrc:
    case MOP_wcselrrrc:
    case MOP_wcsinvrrrc:
      nRet = 32;
      break;
    default:
      nRet = 64;
      break;
  }

  return nRet;
}

/* mov w5, #1
 *  ...                   --> cset w5, NE
 * mov w0, #0
 * csel w5, w5, w0, NE
 *
 * mov w5, #0
 *  ...                   --> cset w5,EQ
 * mov w0, #1
 * csel w5, w5, w0, NE
 *
 * condition:
 *    1.all define points of w5 are defined by:   mov w5, #1(#0)
 *    2.all define points of w0 are defined by:   mov w0, #0(#1)
 *    3.w0 will not be used after: csel w5, w5, w0, NE(EQ)
 */
void AArch64GlobalOpt::ConvertCselToCset() {
  FOR_ALL_BB(bb, cgfunc) {
    FOR_BB_INSNS_SAFE(insn, bb, ninsn) {
      MOperator mopCode = insn->GetMachineOpcode();
      if (mopCode == MOP_xcselrrrc || mopCode == MOP_wcselrrrc) {
        Operand *opnd0 = insn->GetOperand(0);
        Operand *cond = insn->GetOperand(3);
        MOperator newMop = (opnd0->GetSize() == 64 ? MOP_xcsetrc : MOP_wcsetrc);
        if (OpndDefByOne(insn, 1) && OpndDefByZero(insn, 2)) {
          Insn *newInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(newMop, opnd0, cond);
          newInsn->id = insn->id;
          GetRD()->InsertDUUDForSrcOperand(newInsn, 1, DataInsnInfo::kNormal, insn, 3, DataInsnInfo::kNormal);
          GetRD()->InsertDUUDForDestOperand(newInsn, 0, DataInsnInfo::kNormal, insn, 0, DataInsnInfo::kNormal, true);
          GetRD()->RemoveDUUDForInsn(insn);
          bb->ReplaceInsn(insn, newInsn);
        } else if (OpndDefByZero(insn, 1) && OpndDefByOne(insn, 2)) {
          CondOperand *inverseCond = nullptr;
          CondOperand *originCond = static_cast<CondOperand *>(cond);
          bool res = GetInverseCond(originCond, inverseCond);
          if (res) {
            Insn *newInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(newMop, opnd0, inverseCond);
            newInsn->id = insn->id;
            GetRD()->InsertDUUDForSrcOperand(newInsn, 1, DataInsnInfo::kNormal, insn, 3, DataInsnInfo::kNormal);
            GetRD()->InsertDUUDForDestOperand(newInsn, 0, DataInsnInfo::kNormal, insn, 0, DataInsnInfo::kNormal, true);
            GetRD()->RemoveDUUDForInsn(insn);
            bb->ReplaceInsn(insn, newInsn);
          }
        }
      }
    }
  }
}

// if used Operand in insn is defined by one in all define insn, return true
bool AArch64GlobalOpt::OpndDefByOne(Insn *insn, int useidx) const {
  CG_ASSERT(insn->GetOperand(useidx) && insn->GetOperand(useidx)->IsRegister(), "the used Operand must be Register");
  // Zero Register don't need be defined
  if (insn->GetOperand(useidx)->IsZeroRegister()) {
    return false;
  }
  CG_ASSERT(insn->defs[useidx], "Operand must be defined before used");
  for (auto defInfo : *(insn->defs[useidx])) {
    if (defInfo.IsMulGen()) {
      return false;
    }
    Insn *definsn = defInfo.GetInsn();
    CG_ASSERT(definsn, "definsn must be exist");
    if (!InsnDefOne(definsn)) {
      return false;
    }
  }
  return true;
}

// if used Operand in insn is defined by zero in all define insn, return true
bool AArch64GlobalOpt::OpndDefByZero(Insn *insn, int useidx) const {
  CG_ASSERT(insn->GetOperand(useidx) && insn->GetOperand(useidx)->IsRegister(), "the used Operand must be Register");
  // Zero Register don't need be defined
  if (insn->GetOperand(useidx)->IsZeroRegister()) {
    return true;
  }
  CG_ASSERT(insn->defs[useidx], "Operand must be defined before used");
  for (auto defInfo : *(insn->defs[useidx])) {
    if (defInfo.IsMulGen()) {
      return false;
    }
    Insn *definsn = defInfo.GetInsn();
    CG_ASSERT(definsn, "definsn must be exist");
    if (!InsnDefZero(definsn)) {
      return false;
    }
  }
  return true;
}

bool AArch64GlobalOpt::GetInverseCond(CondOperand *cond, CondOperand *&inverseCond) const {
  AArch64CC_t ccCode;
  switch (cond->GetCode()) {
    case CC_NE:
      ccCode = CC_EQ;
      break;
    case CC_EQ:
      ccCode = CC_NE;
      break;
    case CC_LT:
      ccCode = CC_GE;
      break;
    case CC_GE:
      ccCode = CC_LT;
      break;
    case CC_GT:
      ccCode = CC_LE;
      break;
    case CC_LE:
      ccCode = CC_GT;
      break;
    default:
      ccCode = kCcLast;
      break;
  }
  if (ccCode == kCcLast) {
    inverseCond = nullptr;
    return false;
  }
  AArch64CGFunc *f = static_cast<AArch64CGFunc *>(cgfunc);
  inverseCond = f->GetCondOperand(ccCode);
  return true;
}

// if defined operand(must be first insn currently) in insn is const one, return true
bool AArch64GlobalOpt::InsnDefOne(Insn *insn) {
  CG_ASSERT(insn, "insn must not be null");
  MOperator defMop = insn->GetMachineOpcode();
  switch (defMop) {
    case MOP_xmovri32:
    case MOP_xmovri64: {
      Operand *srcOpnd = insn->GetOperand(1);
      CG_ASSERT(srcOpnd->IsIntImmediate(), "expects ImmOperand");
      ImmOperand *srcConst = static_cast<ImmOperand *>(srcOpnd);
      int64 srcConstValue = srcConst->GetValue();
      if (srcConstValue == 1) {
        return true;
      }
      return false;
    }
    default:
      return false;
  }
}

// if defined operand(must be first insn currently) in insn is const zero, return true
bool AArch64GlobalOpt::InsnDefZero(Insn *insn) {
  CG_ASSERT(insn, "insn must not be null");
  MOperator defMop = insn->GetMachineOpcode();
  switch (defMop) {
    case MOP_xmovri32:
    case MOP_xmovri64: {
      Operand *srcOpnd = insn->GetOperand(1);
      CG_ASSERT(srcOpnd->IsIntImmediate(), "expects ImmOperand");
      ImmOperand *srcConst = static_cast<ImmOperand *>(srcOpnd);
      int64 srcConstValue = srcConst->GetValue();
      if (srcConstValue == 0) {
        return true;
      }
      return false;
    }
    case MOP_xmovrr:
    case MOP_wmovrr:
      return insn->GetOperand(1)->IsZeroRegister();
    default:
      return false;
  }
}

/*  when w0 has only one valid bit, these tranformation will be done
 *  cmp  w0, #0
 *  cset w1, NE --> mov w1, w0
 *
 *  cmp  w0, #0
 *  cset w1, EQ --> eor w1, w0, 1
 *
 *  cmp  w0, #1
 *  cset w1, NE --> eor w1, w0, 1
 *
 *  cmp  w0, #1
 *  cset w1, EQ --> mov w1, w0
 *
 *  cmp w0,  #0
 *  cset w0, NE -->null
 *
 *  cmp w0, #1
 *  cset w0, EQ -->null
 *
 *  condition:
 *    1. the first operand of cmp instruction must has only one valid bit
 *    2. the second operand of cmp instruction must be 0 or 1
 *    3. flag register of cmp isntruction must not be used later
 */
void AArch64GlobalOpt::CmpCsetOptimize() {
  AArch64CGFunc *aarchcgfunc = static_cast<AArch64CGFunc *>(cgfunc);
  FOR_ALL_BB(bb, cgfunc) {
    Insn *ninsn = nullptr;
    for (Insn *insn = bb->firstinsn; insn && insn != bb->lastinsn->next; insn = ninsn) {
      ninsn = insn->GetNextMachineInsn();
      if (!ninsn) {
        break;
      }
      if (!insn->IsMachineInstruction()) {
        continue;
      }
      MOperator firstMop = insn->GetMachineOpcode();
      MOperator secondMop = ninsn->GetMachineOpcode();
      if ((firstMop == MOP_wcmpri || firstMop == MOP_xcmpri) &&
          (secondMop == MOP_wcsetrc || secondMop == MOP_xcsetrc)) {
        // get cmp_first operand
        Operand *cmpFirstOpnd = insn->GetOperand(1);
        // get cmp second Operand, ImmOperand must be 0 or 1
        Operand *cmpSecondOpnd = insn->GetOperand(2);
        CG_ASSERT(cmpSecondOpnd->IsIntImmediate(), "expects ImmOperand");
        ImmOperand *cmpConst = static_cast<ImmOperand *>(cmpSecondOpnd);
        int64 cmpConstVal = cmpConst->GetValue();
        // get cset first Operand
        Operand *csetFirstOpnd = ninsn->GetOperand(0);
        // get condition Operand
        CondOperand *cond = static_cast<CondOperand *>(ninsn->GetOperand(1));

        if ((cmpConstVal != 0 && cmpConstVal != 1) || (insn->uses[0] && insn->uses[0]->size() > 1) ||
            cmpFirstOpnd->GetSize() != csetFirstOpnd->GetSize() || !OpndDefByOneOrZero(insn, 1)) {
          continue;
        }

        Insn *csetInsn = ninsn;
        ninsn = ninsn->GetNextMachineInsn();

        if ((cmpConstVal == 0 && cond->GetCode() == CC_NE) || (cmpConstVal == 1 && cond->GetCode() == CC_EQ)) {
          if (IsSameReg(cmpFirstOpnd, csetFirstOpnd)) {
            GetRD()->RemoveDUUDForInsn(insn);
            GetRD()->RemoveDUUDForInsn(csetInsn);
            bb->RemoveInsn(insn);
            bb->RemoveInsn(csetInsn);
          } else {
            MOperator mopCode = (cmpFirstOpnd->GetSize() == 64) ? MOP_xmovrr : MOP_wmovrr;
            Insn *newInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(mopCode, csetFirstOpnd, cmpFirstOpnd);
            newInsn->id = insn->id;
            GetRD()->InsertDUUDForSrcOperand(newInsn, 1, DataInsnInfo::kNormal, insn, 1, DataInsnInfo::kNormal);
            GetRD()->InsertDUUDForDestOperand(newInsn, 0, DataInsnInfo::kNormal, csetInsn, 0, DataInsnInfo::kNormal,
                                              true);
            GetRD()->RemoveDUUDForInsn(insn);
            GetRD()->RemoveDUUDForInsn(csetInsn);
            bb->ReplaceInsn(insn, newInsn);
            bb->RemoveInsn(csetInsn);
          }
        } else if ((cmpConstVal == 1 && cond->GetCode() == CC_NE) || (cmpConstVal == 0 && cond->GetCode() == CC_EQ)) {
          MOperator mopCode = (cmpFirstOpnd->GetSize() == 64) ? MOP_xeorrri13 : MOP_weorrri12;
          ImmOperand *one = aarchcgfunc->CreateImmOperand(1, 8, false);
          Insn *newInsn = cgfunc->cg->BuildInstruction<AArch64Insn>(mopCode, csetFirstOpnd, cmpFirstOpnd, one);
          newInsn->id = insn->id;
          GetRD()->InsertDUUDForSrcOperand(newInsn, 1, DataInsnInfo::kNormal, insn, 1, DataInsnInfo::kNormal);
          GetRD()->InsertDUUDForDestOperand(newInsn, 0, DataInsnInfo::kNormal, csetInsn, 0, DataInsnInfo::kNormal,
                                            true);
          GetRD()->RemoveDUUDForInsn(insn);
          GetRD()->RemoveDUUDForInsn(csetInsn);
          bb->ReplaceInsn(insn, newInsn);
          bb->RemoveInsn(csetInsn);
        }
      }
    }
  }
}

// if used Operand in insn is defined by one valid bit in all define insn, return true
bool AArch64GlobalOpt::OpndDefByOneOrZero(Insn *insn, int useidx) const {
  if (insn->GetOperand(useidx)->IsZeroRegister()) {
    return true;
  }
  CG_ASSERT(insn->defs[useidx], "Operand must be defined before used");
  for (auto defInfo : *(insn->defs[useidx])) {
    if (defInfo.IsMulGen()) {
      return false;
    }
    Insn *definsn = defInfo.GetInsn();
    CG_ASSERT(definsn, "definsn must be exist");
    if (!InsnDefOneOrZero(definsn)) {
      return false;
    }
  }
  return true;
}

// if defined operand(must be first insn currently) in insn has only one valid bit, return true
bool AArch64GlobalOpt::InsnDefOneOrZero(Insn *insn) {
  CG_ASSERT(insn, "insn must not be null");
  MOperator defMop = insn->GetMachineOpcode();
  switch (defMop) {
    case MOP_wcsetrc:
    case MOP_xcsetrc:
      return true;
    case MOP_xmovri32:
    case MOP_xmovri64: {
      Operand *defOpnd = insn->GetOperand(1);
      CG_ASSERT(defOpnd->IsIntImmediate(), "expects ImmOperand");
      ImmOperand *defConst = static_cast<ImmOperand *>(defOpnd);
      int64 defConstValue = defConst->GetValue();
      if (defConstValue != 0 && defConstValue != 1) {
        return false;
      } else {
        return true;
      }
    }
    case MOP_xmovrr:
    case MOP_wmovrr: {
      return insn->GetOperand(1)->IsZeroRegister();
    }
    case MOP_wlsrrri5:
    case MOP_xlsrrri6: {
      Operand *opnd2 = insn->GetOperand(2);
      CG_ASSERT(opnd2 && opnd2->IsIntImmediate(), "expects ImmOperand");
      ImmOperand *opndimm = static_cast<ImmOperand *>(opnd2);
      int64 shiftbits = opndimm->GetValue();
      if ((defMop == MOP_wlsrrri5 && shiftbits == 31) || (defMop == MOP_xlsrrri6 && shiftbits == 63)) {
        return true;
      } else {
        return false;
      }
    }
    default:
      return false;
  }
}

bool AArch64GlobalOpt::OpndOnlyUsedInCurBB(Insn *insn, short opndidx, bool isDest) {
  if (isDest) {
    if (insn->uses[opndidx] == nullptr) {
      return true;
    }
    BB *curBB = insn->bb;
    for (auto useInfo : *(insn->uses[opndidx])) {
      Insn *useinsn = useInfo.GetInsn();
      if (useinsn->bb != curBB) {
        return false;
      }
    }
    return true;
  } else {
    if (insn->defs[opndidx] == nullptr) {
      return false;
    }
    BB *curBB = insn->bb;
    for (auto def : *(insn->defs[opndidx])) {
      Insn *defInsn = def.GetInsn();
      int defInsnIndex = def.GetDUIndex();
      for (auto use : *(defInsn->uses[defInsnIndex])) {
        CHECK_FATAL(use.GetIndex() >= 0, "should be");
        if (use.GetInsn()->bb != curBB || !OpndOnlyDefInCurBB(use.GetInsn(), use.GetIndex())) {
          return false;
        }
      }
    }
    return true;
  }
}

bool AArch64GlobalOpt::RedefPointIsExist(Insn *insn, short opndidx) {
  return insn->redefine[opndidx] != nullptr;
}

bool AArch64GlobalOpt::IsSameReg(Operand *firstOpnd, Operand *secondOpnd) {
  CG_ASSERT(firstOpnd->IsRegister() && secondOpnd->IsRegister(),
            "first_opnd and second_opnd should be Register Operand");
  RegOperand *firstReg = static_cast<RegOperand *>(firstOpnd);
  RegOperand *secondReg = static_cast<RegOperand *>(secondOpnd);
  return firstReg->RegNumEqual(secondReg) && firstReg->GetSize() == secondReg->GetSize();
}

}  // namespace maplebe
