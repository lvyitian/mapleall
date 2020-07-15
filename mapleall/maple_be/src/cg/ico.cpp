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

#include "ico.h"
#include <stdio.h>
#include <iostream>
#include "cg_option.h"
#include <map>
#include "aarch64_isa.h"
#include "cg.h"
#include "cg_assert.h"

// This phase implements if-conversion optimization,
// which tries to convert conditional branches into cset/csel instructions
#define ICODUMP CGDEBUGFUNC(cgfunc)
namespace maplebe {

void IfConversionOptimizer::InitOptimizePatterns() {
  singlePassPatterns.push_back(memPool->New<ITEPattern>(cgfunc));
}

Partition::Partition(Insn *insn) {
  int numOperands = insn->GetOpndNum() + insn->GetResultNum();
  for (int i = 0; i < numOperands; i++) {
    Operand *opnd = insn->GetOperand(i);
    /* Because there are some use-def regs,
     * so NumOperands may bigger than the actual total number
     * of operands, so opnd may be null.
     */
    if (opnd == nullptr || (!opnd->IsRegister() && !opnd->IsMemoryAccessOperand())) {
      continue;
    }
    operands.insert(opnd);
  }
  insnList.push_back(insn);
}

/**
 * Return true and put the given insn into current partition if
 * any of its operands is already in the partitiion.
 */
bool Partition::CheckAndPut(Insn *insn) {
  if (insn->IsBranch()) {
    return true;
  }
  int numOperands = insn->GetOpndNum() + insn->GetResultNum();
  bool inPartition = false;
  vector<Operand *> candidates;
  for (int i = 0; i < numOperands; i++) {
    Operand *opnd = insn->opnds[i];
    if (!opnd) {
      continue;
    }
    if (!opnd->IsRegister() && !opnd->IsMemoryAccessOperand()) {
      continue;
    }
    if (operands.find(opnd) != operands.end()) {
      inPartition = true;
    } else {
      candidates.push_back(opnd);
    }
  }
  if (inPartition) {
    for (Operand *o : candidates) {
      operands.insert(o);
    }
  }
  return inPartition;
}

int ICOPattern::PartitionBB(BB *bb) {
  if (!bb) {
    return 0;
  }
  vector<Partition *> partitions;
  FOR_BB_INSNS(insn, bb) {
    if (!insn->IsMachineInstruction()) {
      continue;
    }
    bool foundPartition = false;
    for (Partition *p : partitions) {
      if (p->CheckAndPut(insn)) {
        foundPartition = true;
        break;
      }
    }
    if (!foundPartition) {
      Partition *newPartition = cgfunc->memPool->New<Partition>(insn);
      partitions.push_back(newPartition);
    }
  }
  return partitions.size();
}

/**
 * Insert instructions in fromBB from the first instruction to the
 * given boundaryFromInsn(exclusively for store and inclusive for load)
 * to toBB before the given boundaryToInsn
 *
 */
void ICOPattern::InsertBeforeInsn(BB *fromBB, Insn *boundaryFromInsn, BB *toBB, Insn *boundaryToInsn) {
  // Insert instructions in branches before cselInsn
  Insn *boundary = nullptr;
  if (boundaryFromInsn->IsLoad()) {
    boundary = boundaryFromInsn->next;
  } else {
    boundary = boundaryFromInsn;
  }
  map<RegOperand *, RegOperand *> preg2vreg;
  for (Insn *insn = fromBB->firstinsn; insn && insn != boundary; insn = insn->next) {
    if (fromBB->GetKind() == BB::kBBReturn) {
      for (int i = 0; i < insn->kMaxOperandNum; i++) {
        if (dynamic_cast<RegOperand *>(insn->opnds[i])) {
          RegOperand *reg = static_cast<RegOperand *>(insn->opnds[i]);
          if (reg->IsPhysicalRegister()) {
            RegOperand *vReg = nullptr;
            auto it = preg2vreg.find(reg);
            if (it == preg2vreg.end()) {
              vReg = cgfunc->theCFG->CreateVregFromReg(reg);
              preg2vreg.insert(pair<RegOperand *, RegOperand *>(reg, vReg));

            } else {
              vReg = it->second;
            }
            insn->SetOperand(i, vReg);
          }
        }
      }
    }
    Insn *newInsn = cgfunc->theCFG->CloneInsn(insn);
    toBB->InsertInsnBefore(boundaryToInsn, newInsn);
  }
}

// convert conditional branches into cset/csel instructions
bool ICOPattern::DoOpt(BB *cmpBB, BB *ifBB, BB *elseBB, BB *joinBB) {
  Insn *condbr = cgfunc->theCFG->FindLastCondBrInsn(cmpBB);
  CG_ASSERT(condbr, "");
  Insn *cmpInsn = cgfunc->theCFG->FindLastCmpInsn(cmpBB);
  Operand *flagOpnd = nullptr;
  // for cbnz and cbz institution
  if (cgfunc->theCFG->IsCompareAndBranchInsn(condbr)) {
    if (condbr->GetOperand(0)->IsZeroRegister()) {
      return false;
    }
    cmpInsn = condbr;
    flagOpnd = condbr->GetOperand(0);
  }

  // tbz will not be optimized

  if (condbr->mop_ == MOP_xtbz || condbr->mop_ == MOP_wtbz || condbr->mop_ == MOP_xtbnz || condbr->mop_ == MOP_wtbnz) {
    return false;
  }

  if (!cmpInsn) {
    return false;
  }

  vector<Operand *> ifDestRegs;
  vector<Operand *> elseDestRegs;

  map<Operand *, Operand *> ifDestSrcMap;
  map<Operand *, Operand *> elseDestSrcMap;

  if (!cgfunc->theCFG->CheckCondMoveBB(elseBB, elseDestSrcMap, elseDestRegs, flagOpnd) ||
      (ifBB && !cgfunc->theCFG->CheckCondMoveBB(ifBB, ifDestSrcMap, ifDestRegs, flagOpnd))) {
    return false;
  }

  int count = elseDestRegs.size();

  for (vector<Operand *>::iterator itr = ifDestRegs.begin(); itr != ifDestRegs.end(); itr++) {
    bool foundInElse = false;
    for (vector<Operand *>::iterator elseItr = elseDestRegs.begin(); elseItr != elseDestRegs.end(); elseItr++) {
      RegOperand *elseDestReg = dynamic_cast<RegOperand *>(*elseItr);
      RegOperand *ifDestReg = dynamic_cast<RegOperand *>(*itr);
      CHECK_FATAL(ifDestReg != nullptr, "if_dest_reg is null in ICOPattern::DoOpt");
      CHECK_FATAL(elseDestReg != nullptr, "else_dest_reg is null in ICOPattern::DoOpt");
      if (ifDestReg->GetRegisterNumber() == elseDestReg->GetRegisterNumber()) {
        foundInElse = true;
        break;
      }
    }
    if (foundInElse) {
      continue;
    } else {
      ++count;
    }
  }
  if (count > kThreshold) {
    return false;
  }

  // generate insns
  vector<Insn *> elseGenerateInsn;
  vector<Insn *> ifGenerateInsn;
  bool elseBbProcessResult =
    cgfunc->theCFG->BuildCondMovInsn(cmpBB, elseBB, ifDestSrcMap, elseDestSrcMap, false, elseGenerateInsn);
  bool ifBbProcessResult =
    cgfunc->theCFG->BuildCondMovInsn(cmpBB, ifBB, ifDestSrcMap, elseDestSrcMap, true, ifGenerateInsn);

  if (!elseBbProcessResult || (ifBB && !ifBbProcessResult)) {
    return false;
  }

  // insert insn
  if (cgfunc->theCFG->IsCompareAndBranchInsn(condbr)) {
    Insn *innerCmpInsn = cgfunc->theCFG->BuildCmpInsn(condbr);
    cmpBB->InsertInsnBefore(condbr, innerCmpInsn);
    cmpInsn = innerCmpInsn;
  }

  if (elseBB) {
    cmpBB->SetKind(elseBB->GetKind());
  } else {
    cmpBB->SetKind(ifBB->GetKind());
  }
  // delete condbr
  cmpBB->RemoveInsn(condbr);
  // Insert goto insn after csel insn.
  if (cmpBB->GetKind() == BB::kBBGoto) {
    if (elseBB) {
      cmpBB->InsertInsnAfter(cmpBB->lastinsn, elseBB->lastinsn);
    } else {
      cmpBB->InsertInsnAfter(cmpBB->lastinsn, ifBB->lastinsn);
    }
  }

  // Insert instructions in branches after cmpInsn
  for (vector<Insn *>::reverse_iterator itr = elseGenerateInsn.rbegin(); itr != elseGenerateInsn.rend(); itr++) {
    cmpBB->InsertInsnAfter(cmpInsn, *itr);
  }
  for (vector<Insn *>::reverse_iterator itr = ifGenerateInsn.rbegin(); itr != ifGenerateInsn.rend(); itr++) {
    cmpBB->InsertInsnAfter(cmpInsn, *itr);
  }

  // Remove branches and merge join
  cgfunc->theCFG->RemoveBB(ifBB);
  cgfunc->theCFG->RemoveBB(elseBB);

  if (cmpBB->next == joinBB
      && !cgfunc->theCFG->InLSDA(joinBB->labidx, cgfunc->ehfunc)
      && cgfunc->theCFG->CanMerge(cmpBB,joinBB)) {
    cgfunc->theCFG->MergeBB(cmpBB, joinBB, cgfunc);
    keepPosition=true;
  }
  return true;
}

// find IF-THEN-ELSE or IF-THEN basic block pattern,
// and then invoke DoOpt(...) to finish optimize.
bool ITEPattern::Optimize(BB *&curbb) {
  if (curbb->GetKind() == BB::kBBIf) {
    BB *ifBB = nullptr;
    BB *elseBB = nullptr;
    BB *joinBB = nullptr;

    BB *thenDest = cgfunc->theCFG->GetTargetSuc(curbb);
    BB *elseDest = curbb->next;
    CHECK_FATAL(thenDest != nullptr, "then_dest is null in ITEPattern::Optimize");
    // IF-THEN-ELSE
    if (thenDest->NumPreds() == 1 && thenDest->NumSuccs() == 1 && elseDest->NumSuccs() == 1 &&
        elseDest->NumPreds() == 1 && thenDest->succs.front() == elseDest->succs.front()) {
      ifBB = thenDest;
      elseBB = elseDest;
      joinBB = thenDest->succs.front();
    }
    // IF-THEN
    else if (elseDest->NumPreds() == 1 && elseDest->NumSuccs() == 1 && elseDest->succs.front() == thenDest) {
      ifBB = nullptr;
      elseBB = elseDest;
      joinBB = thenDest;
    } else
    // not a form we can handle
    {
      return false;
    }

    if (cgfunc->theCFG->InLSDA(elseBB->labidx, cgfunc->ehfunc) ||
        cgfunc->theCFG->InSwitchTable(elseBB->labidx, cgfunc)) {
      return false;
    }

    if (ifBB &&
        (cgfunc->theCFG->InLSDA(ifBB->labidx, cgfunc->ehfunc) || cgfunc->theCFG->InSwitchTable(ifBB->labidx, cgfunc))) {
      return false;
    }
    return DoOpt(curbb, ifBB, elseBB, joinBB);
  }
  return false;
}

AnalysisResult *CgDoIco::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  LiveAnalysis *live = nullptr;
  live = static_cast<LiveAnalysis *>(m->GetAnalysisResult(CgFuncPhase_LIVE, cgfunc));
  if (ICODUMP) {
    DotGenerator::GenerateDot("ico-before", cgfunc, &(cgfunc->mirModule));
  }
  MemPool *mp = mempoolctrler.NewMemPool("ico");
  IfConversionOptimizer *ico = mp->New<IfConversionOptimizer>(cgfunc, mp, live);
  MIRFunction *mirFunc = cgfunc->func;
  string funcclass = mirFunc->GetBaseClassName();
  string funcname = mirFunc->GetBaseFuncName();
  string name = funcclass + funcname;
  ico->Run(name.c_str());
  if (ICODUMP) {
    DotGenerator::GenerateDot("ico-after", cgfunc, &(cgfunc->mirModule));
  }
  // the live range info may changed, so invalid the info.
  m->InvalidAnalysisResult(CgFuncPhase_LIVE, cgfunc);
  mempoolctrler.DeleteMemPool(mp);
  return nullptr;
}

}  // namespace maplebe
