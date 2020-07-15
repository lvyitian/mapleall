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

#include "cg_option.h"
#include "cg_func.h"
#include "cg.h"
#include "optimize_common.h"
#include <algorithm>

/* this phase build use->define chain, define->use chain, define->predefine chain and define->redefine chain.
   this algorithm mainly include 3 parts:
      1. initialize each BB
        (1) insert pseudoInsns for function parameters, ehBB, and return R0/V0
        (2) init bb->in, bb->kill, bb->gen, bb->out
      2. build in and out
        (1) In[BB] = Union all of out[Parents(bb)]
        (2) Out[BB] = gen[BB] union (in[BB] - kill[BB] )
      3. build UD-chain and DU-chain
 */

namespace maplebe {
using namespace std;

#define REACHINGDEFINITIONDUMP CGDEBUGFUNC(cgfunc)

#define RDA_MAX_INSN_NUM 18000

bool ReachingDefinition::IsFrameReg(Operand *opnd) {
  if (!opnd->IsRegister()) {
    return false;
  }
  RegOperand *reg = static_cast<RegOperand *>(opnd);
  return cgfunc->IsFrameReg(reg);
}

/* intialize bb->out, bb->out only include generated DataInsnInfo
 */
void ReachingDefinition::InitOut(BB *bb) {
  for (auto genElem : bb->gen) {
    const DataAnalysisInfo kDataInfo = genElem.first;
    if (kDataInfo.GetOperand()->IsConditionCode()) {
      continue;
    }
    const DataInsnInfo kInsnInfo = genElem.second;

    set<DataInsnInfo, DataInsnInfoCmp> insnInfoSet;
    insnInfoSet.insert(kInsnInfo);
    bb->out.insert(pair<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>>(kDataInfo, insnInfoSet));
  }
}

/*update bb->kill and  bb->gen for RegOperand.
   input:
   bb: current bb
   regOpnd: defined Register
   insn: insn defining regOpnd
   index: the index of regOpnd in insn
   prop: property.
 */
void ReachingDefinition::DefineRegister(BB *bb, RegOperand *regOpnd, Insn *insn, short index, int prop) {
  if (regOpnd->IsZeroRegister()) {
    return;
  }

  map<DataAnalysisInfo, DataInsnInfo, DataAnalysisInfoCmp>::iterator itGen;

  if (!insn->IsPseudoInstruction() && bb->lastuse_regno.find(regOpnd->GetRegisterNumber()) != bb->lastuse_regno.end()) {
    // If current register is not live out, only kill, withou gen.
    bb->kill.insert(DataAnalysisInfo(kInfoReg, regOpnd));
    return;
  }

  itGen = bb->gen.find(DataAnalysisInfo(kInfoReg, regOpnd));

  DataInsnInfo insnInfo(insn, index, prop);

  if (itGen != bb->gen.end()) {
    Insn *oldDefInsn = itGen->second.GetInsn();

    if (!bb->eh_succs.empty()) {
      // Check if exist throw between two gen.
      if (MayHasThrow(oldDefInsn, insn)) {
        insnInfo.SetProperty(DataInsnInfo::kMultigen);
        // clone it_gen->second
        DataInsnInfo *pinsnInfo = itGen->second.Clone(mp);
        insnInfo.SetPrevMultiGenInsn(pinsnInfo);
      }
    }

    bb->gen.erase(itGen);
  }

  bb->kill.insert(DataAnalysisInfo(kInfoReg, regOpnd));
  bb->gen.insert(pair<DataAnalysisInfo, DataInsnInfo>(DataAnalysisInfo(kInfoReg, regOpnd), insnInfo));
}

/*after building bb->in and bb->out for RegOperand, this function is used to update bb->in by traversaling each insn.
   input:
   bb: current bb
   regOpnd: Register to be defined in insn
   insn: insn defining regOpnd
   index: the index of regOpnd in insn
   prop: property.
 */
void ReachingDefinition::DefineRegisterOnBBIn(BB *bb, Operand *opnd, Insn *insn, short index, int prop) {
  if (opnd->IsZeroRegister()) {
    return;
  }
  if (opnd->IsConditionCode()) {
    return;
  }
  CG_ASSERT(opnd->IsRegister(), "operand should be a register operand.");

  map<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>, DataAnalysisInfoCmp>::iterator itIn;
  itIn = bb->in.find(DataAnalysisInfo(kInfoReg, opnd));

  DataInsnInfo insnInfo(insn, index, prop);

  if (itIn != bb->in.end()) {
    // Save redefine.
    for (auto insnInfoElem : itIn->second) {
      Insn *oldDefInsn = insnInfoElem.GetInsn();
      int idx = insnInfoElem.GetProperty() & DataInsnInfo::kIndexQuickcalc;
      if (!oldDefInsn->redefine[idx]) {
        oldDefInsn->redefine[idx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
      }

      oldDefInsn->redefine[idx]->insert(insnInfo);

      idx = prop & DataInsnInfo::kIndexQuickcalc;
      if (!insn->predefine[idx]) {
        insn->predefine[idx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
      }

      insn->predefine[idx]->insert(insnInfoElem);
    }

    // erase it if already exist.
    bb->in.erase(itIn);
  }

  // Define again.
  set<DataInsnInfo, DataInsnInfoCmp> tmpset;
  tmpset.insert(insnInfo);
  bb->in.insert(pair<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>>(DataAnalysisInfo(kInfoReg, opnd), tmpset));
}

/*update bb->kill and  bb->gen for MemOperand.
   input:
   bb: current bb
   opnd: MemOperand to be defined in insn
   insn: insn defining regOpnd
   index: the index of MemOpnd in insn
   prop: property.
 */
void ReachingDefinition::DefineMem(BB *bb, Operand *opnd, Insn *insn, short index, int prop) {
  map<DataAnalysisInfo, DataInsnInfo, DataAnalysisInfoCmp>::iterator itGen;

  DataAnalysisInfo dataInfo(kInfoMem, opnd);

  itGen = bb->gen.find(DataAnalysisInfo(kInfoMem, opnd));

  DataInsnInfo insnInfo(insn, index, prop);

  if (itGen != bb->gen.end()) {
    if (!bb->eh_succs.empty()) {
      // Check if exist throw between two gen.
      if (MayHasThrow(itGen->second.GetInsn(), insn)) {
        insnInfo.SetProperty(DataInsnInfo::kMultigen);
        // clone it_gen->second
        DataInsnInfo *pinsnInfo = itGen->second.Clone(mp);
        insnInfo.SetPrevMultiGenInsn(pinsnInfo);
      }
    }

    bb->gen.erase(itGen);
  }

  bb->kill.insert(DataAnalysisInfo(kInfoMem, opnd));
  bb->gen.insert(pair<DataAnalysisInfo, DataInsnInfo>(DataAnalysisInfo(kInfoMem, opnd), insnInfo));
}

/*after building bb->in and bb->out for MemOperand, this function is used to update bb->in by traversaling each insn.
   input:
   bb: current bb
   opnd: MemOperand to be defined in insn
   insn: insn defining MemOerand
   index: the index of MemOpnd in insn
   prop: property.
 */
void ReachingDefinition::DefineMemOnBBIn(BB *bb, Operand *opnd, Insn *insn, short index, int prop) {
  map<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>, DataAnalysisInfoCmp>::iterator itIn;
  itIn = bb->in.find(DataAnalysisInfo(kInfoMem, opnd));

  DataInsnInfo insnInfo(insn, index, prop);

  if (itIn != bb->in.end()) {
    // Save redefine.
    for (auto insnInfoElem : itIn->second) {
      Insn *oldDefInsn = insnInfoElem.GetInsn();
      int idx = insnInfoElem.GetProperty() & DataInsnInfo::kIndexQuickcalc;
      if (!oldDefInsn->redefine[idx]) {
        oldDefInsn->redefine[idx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
      }

      oldDefInsn->redefine[idx]->insert(insnInfo);

      idx = prop & DataInsnInfo::kIndexQuickcalc;
      if (!insn->predefine[idx]) {
        insn->predefine[idx] = mp->New<MapleSet<DataInsnInfo, DataInsnInfoCmp>>(rdalloc.Adapter());
      }

      insn->predefine[idx]->insert(insnInfoElem);
    }

    // erase it if already exist.
    bb->in.erase(itIn);
  }

  // Define again.
  set<DataInsnInfo, DataInsnInfoCmp> tmpset;
  tmpset.insert(insnInfo);
  bb->in.insert(pair<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>>(DataAnalysisInfo(kInfoMem, opnd), tmpset));
}

/*Property of all defined stack MemOperand in bb->gen is set dirty. that means all value stored in MemOperand is
 * unreliable*/
void ReachingDefinition::DirtyAllStackMemGen(BB *bb) {
  for (auto m : bb->gen) {
    if (m.first.IsMemInfo()) {
      Operand *base = m.first.GetMemOperand()->GetBaseRegister();
      if (base && IsFrameReg(base)) {
        m.second.SetProperty(DataInsnInfo::kDirty);
      }
    }
  }
}

/* This function will destroy original BB->In. property of all defined stack MemOperand is set dirty by traversaling
   insns. that means all value stored in MemOperand is unreliable
 */
void ReachingDefinition::DirtyAllStackMemOnBBIn(BB *bb) {
  for (auto m : bb->in) {
    if (m.first.IsMemInfo()) {
      Operand *base = m.first.GetMemOperand()->GetBaseRegister();
      if (base && IsFrameReg(base)) {
        for (auto s : m.second) {
          s.SetProperty(DataInsnInfo::kDirty);
        }
      }
    }
  }
}

/*Property of all defined stack MemOperand in bb->out is set dirty. that means all value stored in MemOperand is
 * unreliable*/
void ReachingDefinition::DirtyAllBBOut(BB *bb) {
  for (auto m : bb->out) {
    for (auto s : m.second) {
      s.SetProperty(DataInsnInfo::kDirty);
    }
  }
}

/* Out[BB] = gen[BB] union (in[BB] - kill[BB] )
   If bb has eh_succs, we can't in[BB] - kill[BB], we need to build multigen.
   input:
   bb: bb to be computed out
   set_changed_dataInfo: used to saved all changed DataInsnInfo.
   retrun:
   return true if bb->out is changed, else return false.
 */
bool ReachingDefinition::GenerateOut(BB *bb, set<DataAnalysisInfo, DataAnalysisInfoCmp> &setChangedDataInfo) {
  std::map<DataAnalysisInfo, DataInsnInfo, DataAnalysisInfoCmp>::iterator itGen;

  bool bRet = false;
  bool hasEhSuccs = !bb->eh_succs.empty();
  // Check if out is changed.
  if (bb->internal_flag2 && hasEhSuccs && bb->kill.size() >= setChangedDataInfo.size()) {
    for (auto dataInfo : setChangedDataInfo) {
      if (bb->kill.find(dataInfo) == bb->kill.end()) {
        bRet = true;
        break;
      }
    }

    if (bRet == false) {
      return false;
    }
  }

  bb->internal_flag2 = 1;
  bb->out.clear();

  if (hasEhSuccs) {
    // out[BB] = gen[BB] union in[BB]; If gen and
    for (auto inElem : bb->in) {
      const DataAnalysisInfo &dataInfo = inElem.first;
      itGen = bb->gen.find(dataInfo);
      if (itGen != bb->gen.end()) {
        // Generate Multigen.
        DataInsnInfo insnInfo = itGen->second;
        DataInsnInfo *pTailInsnInfo = &insnInfo;
        for (auto inInsnElem : inElem.second) {
          DataInsnInfo *pinsnInfo = inInsnElem.Clone(mp);
          pTailInsnInfo->SetPrevMultiGenInsn(pinsnInfo);
          pTailInsnInfo->SetProperty(DataInsnInfo::kMultigen);
          pTailInsnInfo = pinsnInfo;
        }

        set<DataInsnInfo, DataInsnInfoCmp> tmpset;
        tmpset.insert(insnInfo);
        bb->out.insert(pair<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>>(dataInfo, tmpset));
      } else {
        bb->out.insert(inElem);
      }
    }

    // out[BB] = out[BB] union gen[BB].
    for (auto genElem : bb->gen) {
      // If gen's elem has exist in out, then continue.
      if (bb->out.find(genElem.first) != bb->out.end()) {
        continue;
      }

      set<DataInsnInfo, DataInsnInfoCmp> tmpset;
      tmpset.insert(genElem.second);
      bb->out.insert(pair<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>>(genElem.first, tmpset));
    }
  } else {
    bb->out = bb->in;
    // out[BB] = in[BB] - kill[BB]
    for (auto killDataInfo : bb->kill) {
      bb->out.erase(killDataInfo);
    }

    // Set all dirty if BB exist dirty insn.
    if (bb->internal_flag1) {
      DirtyAllBBOut(bb);
    }

    // out[BB] = out[BB] union gen[BB]
    for (auto m : bb->gen) {
      if (m.first.GetOperand()->IsConditionCode() || m.first.GetOperand() == cgfunc->GetRflag()) {
        continue;
      }
      set<DataInsnInfo, DataInsnInfoCmp> tmpset;
      tmpset.insert(m.second);
      bb->out.insert(pair<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>>(m.first, tmpset));
    }
  }

  return true;
}

/* check if dataInfo is may exist multigen.
   if dataInfo is generated in prevBB and the edge between currBB and  prevBB is not exception edge, return true.
   Otherwise return false.
 */
bool ReachingDefinition::CheckMultigenPossibility(const BB *currBB, const BB *prevBB,
                                                  const DataAnalysisInfo &dataInfo) {
  if (prevBB->gen.find(dataInfo) != prevBB->gen.end()) {
    auto itList = find(currBB->eh_preds.begin(), currBB->eh_preds.end(), prevBB);
    if (itList != currBB->eh_preds.end()) {
      // Has a eh edge from prevBB to currBB.
      return true;
    } else {
      return false;
    }
  } else {
    return true;
  }
}

/* In[BB] = Union all of out[Parents(bb)]
   set_changed_dataInfo[in/out] : For indicate which operands' info changed.
   return value: true if bb->in changed.
 */
bool ReachingDefinition::GenerateIn(BB *bb, set<DataAnalysisInfo, DataAnalysisInfoCmp> &setChangedDataInfo) {
  bool bRet = false;
  for (auto prevBB : bb->preds) {
    for (auto outElem : prevBB->out) {
      Operand *elemFirst = outElem.first.GetOperand();
      if (elemFirst->IsConditionCode() || elemFirst == cgfunc->GetRflag()) {
        continue;
      }
      map<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>, DataAnalysisInfoCmp>::iterator itMap;
      itMap = bb->in.find(outElem.first);
      bool mayMultiGgen = CheckMultigenPossibility(bb, prevBB, outElem.first);
      if (itMap != bb->in.end()) {
        // Insert DataInsnInfo, and set dirty if one of the DataInsnInfo is dirty.
        for (auto setElem : outElem.second) {
          // Insert with dirty.
          // Check if exist in bb->in.
          set<DataInsnInfo, DataInsnInfoCmp>::iterator itSet;
          itSet = itMap->second.find(setElem);
          if (itSet != itMap->second.end()) {
            // DataInsnInfo exist.
            bool bSetDirty = false;
            bool bSetMulGen = false;

            // In[BB] should set dirty.
            if (!itSet->IsDirty() && setElem.IsDirty()) {
              bSetDirty = true;
            }

            if (mayMultiGgen && !itSet->IsMulGen() && setElem.IsMulGen()) {
              bSetMulGen = true;
            }

            if (bSetDirty || bSetMulGen) {
              DataInsnInfo dataInsnInfo = *itSet;
              if (bSetDirty) {
                dataInsnInfo.SetProperty(DataInsnInfo::kDirty);
              }
              if (bSetMulGen) {
                dataInsnInfo.SetProperty(DataInsnInfo::kMultigen);
                dataInsnInfo.SetPrevMultiGenInsn(setElem.GetPrevMultiGenInsn());
              }

              itMap->second.erase(itSet);
              itMap->second.insert(dataInsnInfo);
              bRet = true;
              setChangedDataInfo.insert(outElem.first);
            }
          } else {
            // Insert normally.
            if (!mayMultiGgen && setElem.IsMulGen()) {
              // If bb not allow multigen, and the out of prevBB has multigen property, remove the multigen property.
              DataInsnInfo newInsnInfo = setElem;
              newInsnInfo.ClearProperty(DataInsnInfo::kMultigen);
              newInsnInfo.SetPrevMultiGenInsn(nullptr);
              itMap->second.insert(newInsnInfo);
            } else {
              itMap->second.insert(setElem);
            }
            bRet = true;
            setChangedDataInfo.insert(outElem.first);
          }
        }
      } else {
        // Insert it.
        const DataInsnInfo &outElemFirstInsnInfo = *(outElem.second.begin());
        if (!mayMultiGgen && outElemFirstInsnInfo.IsMulGen()) {
          // If bb not allow multigen, and the out of prevBB has multigen property, remove the multigen property.
          CG_ASSERT(outElem.second.size() == 1,
                    "CG internal error. The number of dataInsnInfo should be 1 if it with multigen property.");

          DataInsnInfo newInsnInfo = outElemFirstInsnInfo;
          newInsnInfo.ClearProperty(DataInsnInfo::kMultigen);
          newInsnInfo.SetPrevMultiGenInsn(nullptr);
          set<DataInsnInfo, DataInsnInfoCmp> tmpset;
          tmpset.insert(newInsnInfo);
          bb->in.insert(pair<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>>(outElem.first, tmpset));
        } else {
          bb->in.insert(outElem);
        }

        bRet = true;
        setChangedDataInfo.insert(outElem.first);
      }
    }
  }

  for (auto prevEhBB : bb->eh_preds) {
    for (auto outElem : prevEhBB->out) {
      map<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>, DataAnalysisInfoCmp>::iterator itMap;
      itMap = bb->in.find(outElem.first);
      bool mayMultiGgen = CheckMultigenPossibility(bb, prevEhBB, outElem.first);
      if (itMap != bb->in.end()) {
        // Insert DataInsnInfo, and set dirty if one of the DataInsnInfo is dirty.
        for (auto setElem : outElem.second) {
          // Insert with dirty.
          // Check if exist in bb->in.
          set<DataInsnInfo, DataInsnInfoCmp>::iterator itSet;
          itSet = itMap->second.find(setElem);
          if (itSet != itMap->second.end()) {
            // DataInsnInfo exist.
            bool bSetDirty = false;
            bool bSetMulGen = false;

            // In[BB] should set dirty.
            if (!itSet->IsDirty() && setElem.IsDirty()) {
              bSetDirty = true;
            }

            if (mayMultiGgen && !itSet->IsMulGen() && setElem.IsMulGen()) {
              bSetMulGen = true;
            }

            if (bSetDirty || bSetMulGen) {
              DataInsnInfo dataInsnInfo = *itSet;
              if (bSetDirty) {
                dataInsnInfo.SetProperty(DataInsnInfo::kDirty);
              }
              if (bSetMulGen) {
                dataInsnInfo.SetProperty(DataInsnInfo::kMultigen);
                dataInsnInfo.SetPrevMultiGenInsn(setElem.GetPrevMultiGenInsn());
              }

              itMap->second.erase(itSet);
              itMap->second.insert(dataInsnInfo);
              bRet = true;
              setChangedDataInfo.insert(outElem.first);
            }
          } else {
            // Insert normally.
            if (!mayMultiGgen && setElem.IsMulGen()) {
              // If bb not allow multigen, and the out of prevBB has multigen property, remove the multigen property.
              DataInsnInfo newInsnInfo = setElem;
              newInsnInfo.ClearProperty(DataInsnInfo::kMultigen);
              newInsnInfo.SetPrevMultiGenInsn(nullptr);
              itMap->second.insert(newInsnInfo);
            } else {
              itMap->second.insert(setElem);
            }
            bRet = true;
            setChangedDataInfo.insert(outElem.first);
          }
        }
      } else {
        // Insert it.
        const DataInsnInfo &outElemFirstInsnInfo = *(outElem.second.begin());
        if (!mayMultiGgen && outElemFirstInsnInfo.IsMulGen()) {
          // If bb not allow multigen, and the out of prevBB has multigen property, remove the multigen property.
          CG_ASSERT(outElem.second.size() == 1,
                    "CG internal error. The number of dataInsnInfo should be 1 if it with multigen property.");

          DataInsnInfo newInsnInfo = outElemFirstInsnInfo;
          newInsnInfo.ClearProperty(DataInsnInfo::kMultigen);
          newInsnInfo.SetPrevMultiGenInsn(nullptr);
          set<DataInsnInfo, DataInsnInfoCmp> tmpset;
          tmpset.insert(newInsnInfo);
          bb->in.insert(pair<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>>(outElem.first, tmpset));
        } else {
          bb->in.insert(outElem);
        }

        bRet = true;
        setChangedDataInfo.insert(outElem.first);
      }
    }
  }

  return bRet;
}

/* In[firstCleanupBB] = Union all of out[bb_normal_set]
   input:
   firstCleanupBB: First bb of the clean up BBs.
   bb_normal_set : set of function body bbs (except cleanup BBs).
   regno_set : register number which may live both in function body BBs and cleanup BBs.
   set_changed_dataInfo[in/out] : For indicate which operands' info changed.
   return value: true if bb->in changed.
 */
bool ReachingDefinition::GenerateInForFirstCleanupBB(BB *firstCleanupBB, const set<BB *, BBIdCmp> &bbNormalSet,
                                                     const set<regno_t> &regnoSet,
                                                     set<DataAnalysisInfo, DataAnalysisInfoCmp> &setChangedDataInfo) {
  map<DataAnalysisInfo, set<DataInsnInfo, DataInsnInfoCmp>, DataAnalysisInfoCmp>::iterator itMap;
  bool bRet = false;

  CG_ASSERT(firstCleanupBB->in.empty(), "CG internal error.");

  for (auto normalBb : bbNormalSet) {
    for (auto outMap : normalBb->out) {
      const DataAnalysisInfo &operandInfo = outMap.first;
      if (operandInfo.GetKind() == kInfoReg) {
        RegOperand *regOpnd = operandInfo.GetRegOperand();

        CG_ASSERT(regOpnd, "CG internal error.");

        if (regnoSet.find(regOpnd->GetRegisterNumber()) != regnoSet.end()) {
          // Union normal_bb->out to firstCleanupBB->in.
          // Check if current register exist in firstCleanupBB->in.
          itMap = firstCleanupBB->in.find(outMap.first);
          if (itMap != firstCleanupBB->in.end()) {
            for (auto setElem : outMap.second) {
              // Check if the dataInsnInfo exist in bb->in.
              set<DataInsnInfo, DataInsnInfoCmp>::iterator itSet;
              itSet = itMap->second.find(setElem);
              if (itSet != itMap->second.end()) {
                bool bSetDirty = false;
                bool bSetMulGen = false;

                // In[BB] should set dirty.
                if (!itSet->IsDirty() && setElem.IsDirty()) {
                  bSetDirty = true;
                }

                if (!itSet->IsMulGen() && setElem.IsMulGen()) {
                  bSetMulGen = true;
                }

                if (bSetDirty || bSetMulGen) {
                  DataInsnInfo dataInsnInfo = *itSet;
                  if (bSetDirty) {
                    dataInsnInfo.SetProperty(DataInsnInfo::kDirty);
                  }
                  if (bSetMulGen) {
                    dataInsnInfo.SetProperty(DataInsnInfo::kMultigen);
                    dataInsnInfo.SetPrevMultiGenInsn(setElem.GetPrevMultiGenInsn());
                  }

                  itMap->second.erase(itSet);
                  itMap->second.insert(dataInsnInfo);
                  setChangedDataInfo.insert(outMap.first);
                  bRet = true;
                }
              } else {
                // Insert normally.
                itMap->second.insert(setElem);
                setChangedDataInfo.insert(outMap.first);
                bRet = true;
              }
            }
          } else {
            firstCleanupBB->in.insert(outMap);
            setChangedDataInfo.insert(outMap.first);
            bRet = true;
          }
        } else {
          // Do nothing.
          // Only collect register infos that in set regno_set.
        }
      } else {
        CG_ASSERT(operandInfo.GetKind() == kInfoMem, "CG internal error.");

        // Union normal_bb->out to firstCleanupBB->in.
        // Check if current register exist in firstCleanupBB->in.
        itMap = firstCleanupBB->in.find(outMap.first);
        if (itMap != firstCleanupBB->in.end()) {
          for (auto setElem : outMap.second) {
            // Check if the dataInsnInfo exist in bb->in.
            set<DataInsnInfo, DataInsnInfoCmp>::iterator itSet;
            itSet = itMap->second.find(setElem);
            if (itSet != itMap->second.end()) {
              bool bSetDirty = false;
              bool bSetMulGen = false;

              // In[BB] should set dirty.
              if (!itSet->IsDirty() && setElem.IsDirty()) {
                bSetDirty = true;
              }

              if (normalBb->internal_flag2) {
                if (!itSet->IsMulGen() && setElem.IsMulGen()) {
                  bSetMulGen = true;
                }
              }

              if (bSetDirty || bSetMulGen) {
                DataInsnInfo dataInsnInfo = *itSet;
                if (bSetDirty) {
                  dataInsnInfo.SetProperty(DataInsnInfo::kDirty);
                }
                if (bSetMulGen) {
                  dataInsnInfo.SetProperty(DataInsnInfo::kMultigen);
                  dataInsnInfo.SetPrevMultiGenInsn(setElem.GetPrevMultiGenInsn());
                }

                itMap->second.erase(itSet);
                itMap->second.insert(dataInsnInfo);
                setChangedDataInfo.insert(outMap.first);
                bRet = true;
              }
            } else {
              // Insert normally.
              itMap->second.insert(setElem);
              setChangedDataInfo.insert(outMap.first);
              bRet = true;
            }
          }
        } else {
          firstCleanupBB->in.insert(outMap);
          setChangedDataInfo.insert(outMap.first);
          bRet = true;
        }
      }
    }
  }

  return bRet;
}

void ReachingDefinition::GenerateUDChain(int mode) {
  FOR_ALL_BB(bb, cgfunc) {
    GenerateUseDef(bb, mode);
  }
}

void ReachingDefinition::GenerateDUChain() {
  FOR_ALL_BB(bb, cgfunc) {
    GenerateDefUse(bb);
  }
}

void ReachingDefinition::AdjustLastUse(int mode) {
  if (!(mode & RD_REGANALYSIS)) {
    return;
  }

  set<regno_t> allLastUse;
  set<regno_t> multiDef;
  set<regno_t>::iterator itSet;

  FOR_ALL_BB(bb, cgfunc) {
    for (auto lu : bb->lastuse_regno) {
      if (allLastUse.find(lu) != allLastUse.end()) {
        multiDef.insert(lu);
      } else {
        allLastUse.insert(lu);
      }
    }
  }

  FOR_ALL_BB(bb, cgfunc) {
    if (bb->loop) {
      bb->lastuse_regno.clear();
      // Regno in a loop should be insert to multiDef to remove lastuse.
      for (auto defRegno : bb->def_regno) {
        multiDef.insert(defRegno);
      }
      for (auto useRegno : bb->use_regno) {
        multiDef.insert(useRegno);
      }
    }
  }

  FOR_ALL_BB(bb, cgfunc) {
    for (auto mlu : multiDef) {
      bb->lastuse_regno.erase(mlu);
    }
  }
}

/*entry for ReachingDefinition Analysis, mode represent to analyze RegOperand, MemOperand or both of them*/
void ReachingDefinition::AnalysisStart(int mode, CgFuncResultMgr *m) {
  set<BB *, BBIdCmp> bbNormalSet;
  set<BB *, BBIdCmp> bbCleanupSet;
  set<BB *, BBIdCmp>::iterator itSet;

  BB *firstCleanupBB = nullptr;

  if (!cgfunc->firstbb) {
    return;
  }
  // Ordering the instructions.
  uint32 i = 2;
  FOR_ALL_BB(bb, cgfunc) {
    FOR_BB_INSNS(insn, bb) {
      if (insn->IsMachineInstruction()) {
        insn->id = i;
        i += 3;
      }
    }
  }

  // Fix me: if the function is too big, no enough memory to compile it.
  if (i > RDA_MAX_INSN_NUM) {
    cgfunc->SetRD(nullptr);
    return;
  }

  AddRetPseudoInsns();
  // Init bbs.
  FOR_ALL_BB(bb, cgfunc) {
    InitBB(bb, mode);
  }

  MemPool *sbbMp = mempoolctrler.NewMemPool("sbb");
  SuperBBBuilder *sbb = cgfunc->NewSuperBBBuilder(cgfunc, sbbMp);
  cgfunc->SetSBB(sbb);
  sbb->MergeProcess();

  live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));
  m->GetAnalysisResult(CgFuncPhase_LOOPNOEH, cgfunc);

  FOR_ALL_BB(bb, cgfunc) {
    GenLastUse(bb, mode);
  }

  // If a (virtual/physical) register has multiple definition in the whole function,
  // Then remove it from last use.
  AdjustLastUse(mode);

  // InitCatchBlock();

  FOR_ALL_BB(bb, cgfunc) {
    InitKillGen(bb, mode);
    InitOut(bb);
    // Insert all function body bb to bb_set.
    if (!bb->is_cleanup) {
      bbNormalSet.insert(bb);
    } else if (bb->firststmt == cgfunc->cleanup_label) {
      firstCleanupBB = bb;
    }
  }

  // Build in/out for function body first. (Except cleanup bb)
  set<DataAnalysisInfo, DataAnalysisInfoCmp> setChangedDataInfo;
  while (!bbNormalSet.empty()) {
    itSet = bbNormalSet.begin();
    BB *bb = *itSet;
    bbNormalSet.erase(itSet);

    setChangedDataInfo.clear();

    if (GenerateIn(bb, setChangedDataInfo)) {
      if (GenerateOut(bb, setChangedDataInfo)) {
        for (auto succ : bb->succs) {
          bbNormalSet.insert(succ);
        }
        for (auto ehSucc : bb->eh_succs) {
          bbNormalSet.insert(ehSucc);
        }
      }
    }
  }

  CG_ASSERT(bbNormalSet.empty(), "CG internal error.");

  // If cleanup bb exists, build in/out for cleanup bbs.
  // firstCleanupBB->in = Union all non-cleanup bb's out.
  if (firstCleanupBB) {
    set<regno_t> regnoSet;
    for (auto regno : firstCleanupBB->livein_regno) {
      if (live->CleanupBBIgnoreReg(regno)) {
        continue;
      }

      regnoSet.insert(regno);
    }

    // Seperate normal function bbs and cleanup bbs.
    FOR_ALL_BB(bb, cgfunc) {
      if (bb->is_cleanup) {
        bbCleanupSet.insert(bb);
      } else {
        bbNormalSet.insert(bb);
      }
    }

    setChangedDataInfo.clear();

    // Calculate in for first cleanup bb.
    if (GenerateInForFirstCleanupBB(firstCleanupBB, bbNormalSet, regnoSet, setChangedDataInfo)) {
      // Generate out for first cleanup bb.
      GenerateOut(firstCleanupBB, setChangedDataInfo);
    }

    while (!bbCleanupSet.empty()) {
      itSet = bbCleanupSet.begin();
      BB *bb = *itSet;
      bbCleanupSet.erase(itSet);

      setChangedDataInfo.clear();

      if (GenerateIn(bb, setChangedDataInfo)) {
        if (GenerateOut(bb, setChangedDataInfo)) {
          for (auto succ : bb->succs) {
            bbCleanupSet.insert(succ);
          }
          for (auto ehSucc : bb->eh_succs) {
            bbCleanupSet.insert(ehSucc);
          }
        }
      }
    }

    CG_ASSERT(bbCleanupSet.empty(), "CG internal error.");
  }

  if (mode & DUMPGENKILLINOUTLASTUSE) {
    LogInfo::MapleLogger() << "\n\n##############      Reaching definition: start dump gen/kill/in/out           ###########\n" << endl;
    Dump(DUMPGENKILLINOUTLASTUSE);
    LogInfo::MapleLogger() << "##############      Reaching definition: finish dump gen/kill/in/out           ###########\n" << endl;
  }

  // build U-D chain.
  GenerateUDChain(mode);

  // build D-U chain.
  GenerateDUChain();

  cgfunc->SetRD(this);

  ClearTempInfo();

  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  m->InvalidAnalysisResult(CgFuncPhase_LOOPNOEH, cgfunc);
  sbb->SplitProcess();
  mempoolctrler.DeleteMemPool(cgfunc->GetSBB()->GetMemPool());
  cgfunc->SetSBB(nullptr);
}

void ReachingDefinition::DumpGen(BB *bb) {
  LogInfo::MapleLogger() << "    Gen <" << bb->id << "> : " << bb->gen.size() << endl;

  for (auto m : bb->gen) {
    LogInfo::MapleLogger() << "      Opnd( ";
    m.first.GetOperand()->dump();
    LogInfo::MapleLogger() << ") : ";
    LogInfo::MapleLogger() << "SecondDef[" << (m.second.IsSecondMemAddr() ? "true" : "false") << "], ";
    LogInfo::MapleLogger() << "BaseRegDef[" << (m.second.IsBaseRegister() ? "true" : "false") << "], ";
    LogInfo::MapleLogger() << "Dirty[" << (m.second.IsDirty() ? "true" : "false") << "], ";
    LogInfo::MapleLogger() << "MulGen[" << (m.second.IsMulGen() ? "true" : "false") << "], ";
    LogInfo::MapleLogger() << "MayDef[" << (m.second.IsMayDef() ? "true" : "false") << "], ";
    LogInfo::MapleLogger() << "PartDef[" << (m.second.IsPartDef() ? "true" : "false") << "] ";
    LogInfo::MapleLogger() << "Define insn: \n        ";
    m.second.GetInsn()->dump();
    LogInfo::MapleLogger() << endl;
  }
}

void ReachingDefinition::DumpKill(BB *bb) {
  LogInfo::MapleLogger() << "\n    Kill: <" << bb->id << "> : " << bb->kill.size() << endl;

  for (auto s : bb->kill) {
    LogInfo::MapleLogger() << "      Opnd( ";
    s.GetOperand()->dump();
    LogInfo::MapleLogger() << ")\n";
  }
}

void ReachingDefinition::DumpLastuse(BB *bb) {
  LogInfo::MapleLogger() << "\n    Lastuse: <" << bb->id << "> : " << bb->lastuse_regno.size();
  int count = 0;
  for (auto s : bb->lastuse_regno) {
    if (count++ % 5 == 0) {
      LogInfo::MapleLogger() << "\n      ";
    }
    LogInfo::MapleLogger() << "Reg( " << s << " );  ";
  }
  LogInfo::MapleLogger() << "\n";
}

void ReachingDefinition::DumpIn(BB *bb) {
  LogInfo::MapleLogger() << "\n    Definiton IN <" << bb->id << "> : " << bb->in.size() << endl;

  for (auto m : bb->in) {
    LogInfo::MapleLogger() << "      Definition of operand( ";
    m.first.GetOperand()->dump();
    LogInfo::MapleLogger() << ") : \n";
    for (auto s : m.second) {
      LogInfo::MapleLogger() << "        Define insn: ";
      s.GetInsn()->dump();
      LogInfo::MapleLogger() << "            SecondDef[" << (s.IsSecondMemAddr() ? "true" : "false") << "], ";
      LogInfo::MapleLogger() << "BaseRegDef[" << (s.IsBaseRegister() ? "true" : "false") << "], ";
      LogInfo::MapleLogger() << "Dirty[" << (s.IsDirty() ? "true" : "false") << "], ";
      LogInfo::MapleLogger() << "MulGen[" << (s.IsMulGen() ? "true" : "false") << "], ";
      LogInfo::MapleLogger() << "MayDef[" << (s.IsMayDef() ? "true" : "false") << "], ";
      LogInfo::MapleLogger() << "PartDef[" << (s.IsPartDef() ? "true" : "false") << "] ";
      LogInfo::MapleLogger() << endl;
    }
  }
}

void ReachingDefinition::DumpOut(BB *bb) {
  LogInfo::MapleLogger() << "\n    Definition OUT <" << bb->id << "> : " << bb->out.size() << endl;

  for (auto m : bb->out) {
    LogInfo::MapleLogger() << "      Definition of operand( ";
    m.first.GetOperand()->dump();
    LogInfo::MapleLogger() << ") : \n";
    for (auto s : m.second) {
      LogInfo::MapleLogger() << "        Define insn: ";
      s.GetInsn()->dump();
      LogInfo::MapleLogger() << "            SecondDef[" << (s.IsSecondMemAddr() ? "true" : "false") << "], ";
      LogInfo::MapleLogger() << "BaseRegDef[" << (s.IsBaseRegister() ? "true" : "false") << "], ";
      LogInfo::MapleLogger() << "Dirty[" << (s.IsDirty() ? "true" : "false") << "], ";
      LogInfo::MapleLogger() << "MulGen[" << (s.IsMulGen() ? "true" : "false") << "], ";
      LogInfo::MapleLogger() << "MayDef[" << (s.IsMayDef() ? "true" : "false") << "], ";
      LogInfo::MapleLogger() << "PartDef[" << (s.IsPartDef() ? "true" : "false") << "] ";
      LogInfo::MapleLogger() << endl;
    }
  }
}

void ReachingDefinition::Dump(int flag) {
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(cgfunc->func->stIdx.Idx());
  LogInfo::MapleLogger() << "\n---------  Reaching definition analysis for " << funcSt->GetName();
  FOR_ALL_BB(bb, cgfunc) {
    LogInfo::MapleLogger() << "  === BB_" << bb->id << " (" << hex << bb << ") " << dec << " <" << bb->GetKindName();
    if (bb->labidx != MIRLabelTable::kDummyLabel) {
      LogInfo::MapleLogger() << "[labeled with " << bb->labidx << "]";
    }
    LogInfo::MapleLogger() << "> ===\n";

    if (bb->preds.size() > 0) {
      LogInfo::MapleLogger() << "    pred [ ";
      for (auto preds : bb->preds) {
        LogInfo::MapleLogger() << preds->id << " (" << hex << preds << ") " << dec << " ";
      }
      LogInfo::MapleLogger() << "]\n";
    }
    if (bb->succs.size() > 0) {
      LogInfo::MapleLogger() << "    succ [ ";
      for (auto succs : bb->preds) {
        LogInfo::MapleLogger() << succs->id << " (" << hex << succs << ") " << dec << " ";
      }
      LogInfo::MapleLogger() << "]\n";
    }

    if (flag & DUMPGEN) {
      DumpGen(bb);
    }

    if (flag & DUMPKILL) {
      DumpKill(bb);
    }

    if (flag & DUMPLASTUSE) {
      DumpLastuse(bb);
    }

    if (flag & DUMPIN) {
      DumpIn(bb);
    }

    if (flag & DUMPOUT) {
      DumpOut(bb);
    }

    if (flag & DUMPUD) {
      DumpUDChain(bb);
    }

    if (flag & DUMPDU) {
      DumpDUChain(bb);
    }

    if (flag & DUMPWAWDEPENDENCE) {
      DumpWAWDependence(bb);
    }
    LogInfo::MapleLogger() << "\n";
  }
  LogInfo::MapleLogger() << "---------------------------\n";
}

void ReachingDefinition::ClearTempInfo() {
  FOR_ALL_BB(bb, cgfunc) {
    bb->gen.clear();
    bb->kill.clear();
    bb->in.clear();
    bb->out.clear();
    bb->lastuse_regno.clear();
  }
}

void ReachingDefinition::ClearDefUseInfo() {
  // Remove pseudo insns first.
  for (auto insn : pseudoInsns) {
    insn->bb->RemoveInsn(insn);
  }

  FOR_ALL_BB(bb, cgfunc) {
    FOR_BB_INSNS(insn, bb) {
      for (int i = 0; i < Insn::kMaxUseOperandNum; i++) {
        insn->defs[i] = nullptr;
      }

      for (int i = 0; i < Insn::kMaxDefineOperandNum; i++) {
        insn->uses[i] = nullptr;
        insn->redefine[i] = nullptr;
        insn->predefine[i] = nullptr;
      }
    }
  }

  cgfunc->SetRD(nullptr);
}

/*lastuse_regno[BB] = deg_regno[BB] union use_regno[BB] - liveout_regno[BB] */
void ReachingDefinition::GenLastUse(BB *bb, int mode) {
  if (!(mode & RD_REGANALYSIS)) {
    return;
  }

  set<regno_t> unionRegno;
  set<regno_t> ordered;
  set<regno_t> ordered1;
  ordered.insert(bb->def_regno.begin(), bb->def_regno.end());
  ordered1.insert(bb->use_regno.begin(), bb->use_regno.end());
  set_union(ordered.begin(), ordered.end(), ordered1.begin(), ordered1.end(),
            inserter(unionRegno, unionRegno.begin()));

  ordered.clear();
  ordered.insert(bb->liveout_regno.begin(), bb->liveout_regno.end());
  set_difference(unionRegno.begin(), unionRegno.end(), ordered.begin(), ordered.end(),
                 inserter(bb->lastuse_regno, bb->lastuse_regno.begin()));

  return;
}

void ReachingDefinition::InitBB(BB *bb, int mode) {
  bb->gen.clear();
  bb->kill.clear();
  bb->in.clear();
  bb->out.clear();
  bb->lastuse_regno.clear();

  if (bb == cgfunc->firstbb && (mode & RD_MEMANALYSIS)) {
    InitStartGen();
  }

  if (!bb->eh_preds.empty()) {
    InitEhDefine(bb);
  }
}

AnalysisResult *CgDoReachingDefinition::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  int dumpFlag = 0;

  if (REACHINGDEFINITIONDUMP) {
    DotGenerator::GenerateDot("rda", cgfunc, &(cgfunc->mirModule), true);
    dumpFlag = DUMPGENKILLINOUTLASTUSE;
  }

  LiveAnalysis *live = nullptr;
  ;

  MemPool *rdaMp = mempoolctrler.NewMemPool(PhaseName().c_str());
  CHECK_FATAL(rdaMp != nullptr, "rda_mp is null in CgDoLiveAnalysis::Run");
  MapleAllocator rdaMa(rdaMp);

  ReachingDefinition *rd = cgfunc->NewReachingDefinition(cgfunc, rdaMp, rdaMa, live);
  CHECK_FATAL(rd != nullptr, "reaching definition analysis is null in CgDoReachingDefinition::Run");
  rd->AnalysisStart(RD_ALLANALYSIS | dumpFlag, m);

  if (REACHINGDEFINITIONDUMP) {
    LogInfo::MapleLogger() << "\n\n##############      Reaching definition: start dump DU/DU/redefine/predefine           ###########\n"
         << endl;
    rd->Dump(DUMPDUUD | DUMPWAWDEPENDENCE);
    LogInfo::MapleLogger() << "\n\n##############      Reaching definition: finish dump DU/DU/redefine/predefine           ###########\n"
         << endl;
  }

  return rd;
}

}  // namespace maplebe
