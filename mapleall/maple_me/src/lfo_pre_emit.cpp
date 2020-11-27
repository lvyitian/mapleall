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

#include "me_irmap.h"
#include "lfo_mir_nodes.h"
#include "lfo_function.h"
#include "lfo_pre_emit.h"
#include "constant_fold.h"
#include "mir_lower.h"

namespace maple {

StmtNode *LfoPreEmitter::GetOrCreateStmtNodeFromMestmt(MeStmt *defStmt) {
  MapleMap<MeStmt *, StmtNode *>::iterator it = meir2stmtMap.find(defStmt);
  if (it != meir2stmtMap.end())
    return it->second;
  else {
    StmtNode *mpStmt = EmitLfoStmt(defStmt, NULL);
    meir2stmtMap[defStmt] = mpStmt;
    return mpStmt;
  }
}

void LfoPreEmitter::UpdateUsesDefs(BaseNode *baseNode, MeExpr *meExpr) {
  if (meExpr->op == OP_dread) {
    CHECK_FATAL(baseNode->op == OP_dread, "not synced");
    LfoDreadNode *lnoDrnode = static_cast<LfoDreadNode *>(baseNode);
    std::set<MePhiNode *> visited;
    UpdateUsesDefs4Dread(lnoDrnode, static_cast<VarMeExpr *>(meExpr), visited);
  } else {
   CHECK_FATAL(baseNode->NumOpnds() == meExpr->NumMeExprOpnds(), "should be the same");
   for (uint32 i = 0; i < meExpr->NumMeExprOpnds(); i++) {
     MeExpr *kidMeexpr = meExpr->GetOpnd(i);
     BaseNode *kidBasenode = baseNode->Opnd(i);
     UpdateUsesDefs(kidBasenode, kidMeexpr);
   }
  }
}

void LfoPreEmitter::UpdateUsesDefs4Dread(LfoDreadNode *ldnode, VarMeExpr *drexpr, std::set<MePhiNode *> &visited) {
  MeDefBy defBy = drexpr->defBy;
  if (defBy == kDefByNo)
    return;
  if (defBy == kDefByStmt) {
    StmtNode *defStmt = GetOrCreateStmtNodeFromMestmt(drexpr->def.defStmt);
    CHECK_FATAL(defStmt->op == OP_dassign, "NYI");
    LfoDassignNode *lnodefStmt = static_cast<LfoDassignNode *>(defStmt);
    lnodefStmt->usesList.push_front(ldnode);
    ldnode->defsList.push_front(defStmt);
  } else if (defBy == kDefByChi) {
    StmtNode *defStmt = GetOrCreateStmtNodeFromMestmt(drexpr->def.defChi->base);
    switch(defStmt->op) {
      case OP_dassign:
        static_cast<LfoDassignNode *>(defStmt)->usesList.push_front(ldnode);
        break;
      case OP_iassign:
        static_cast<LfoIassignNode *>(defStmt)->usesList.push_front(ldnode);
        break;
      case OP_call:
      case OP_callassigned:
        static_cast<LfoCallNode *>(defStmt)->usesList.push_front(ldnode);
        break;
      case OP_icall:
      case OP_icallassigned:
        static_cast<LfoIcallNode *>(defStmt)->usesList.push_front(ldnode);
        break;
      case OP_intrinsiccall:
      case OP_xintrinsiccall:
      case OP_intrinsiccallwithtype:
      case OP_intrinsiccallassigned:
      case OP_xintrinsiccallassigned:
      case OP_intrinsiccallwithtypeassigned:
        static_cast<LfoIntrinsiccallNode *>(defStmt)->usesList.push_front(ldnode);
        break;
      default:
        CHECK_FATAL(false, "error or NYI");
    }
    ldnode->defsList.push_front(defStmt);
  } else if (defBy == kDefByMustdef) {
    StmtNode *defStmt = GetOrCreateStmtNodeFromMestmt(drexpr->def.defMustDef->base);
    switch(defStmt->op) {
      case OP_call:
      case OP_callassigned:
        static_cast<LfoCallNode *>(defStmt)->usesList.push_front(ldnode);
        break;
      case OP_icall:
      case OP_icallassigned:
        static_cast<LfoIcallNode *>(defStmt)->usesList.push_front(ldnode);
        break;
      case OP_intrinsiccall:
      case OP_xintrinsiccall:
      case OP_intrinsiccallwithtype:
      case OP_intrinsiccallassigned:
      case OP_xintrinsiccallassigned:
      case OP_intrinsiccallwithtypeassigned:
        static_cast<LfoIntrinsiccallNode *>(defStmt)->usesList.push_front(ldnode);
        break;
      default:
        CHECK_FATAL(false, "error or NYI");
    }
    ldnode->defsList.push_front(defStmt);
  } else {
    MePhiNode *defPhi = drexpr->def.defPhi;
    if (visited.count(defPhi) == 0) {
      visited.insert(defPhi);
      for (uint32 i = 0; i < defPhi->opnds.size(); i++) {
        UpdateUsesDefs4Dread(ldnode, static_cast<VarMeExpr *>(defPhi->opnds[i]), visited);
      }
    }
  }
}

// when we emit a statement before, we might hit infinite recursive updating
// uses-defs, so parameter isUpdateUD prevents that from happening, we will
// update the use later if isUpdate is false
LfoParentPart *LfoPreEmitter::EmitLfoDread(VarMeExpr *drexpr, LfoParentPart *parent, bool isUpdateUD) {
  MIRSymbol *sym = drexpr->ost->GetMIRSymbol();
  if (sym->IsLocal()) {
    sym->ResetIsDeleted();
  }
  LfoDreadNode *ldnode = mirFunc->codeMemPool->New<LfoDreadNode>(&(mirFunc->codeMemPoolAllocator), parent);
  ldnode->fieldID = drexpr->ost->fieldID;
  ldnode->stIdx = sym->stIdx;
  InitBaseNodeByMeExpr(ldnode, drexpr);
  std::set<MePhiNode *> visited;
  if (isUpdateUD) {
    BaseNode *parentBase = parent->Cvt2BaseNode();
    if (parentBase->op == OP_doloop) {
      // doloop node, the defmust be the node itself
      ldnode->defsList.push_front(static_cast<StmtNode *>(parentBase));
    } else {
      // see if it's an iv inside of the doloop
      CHECK_FATAL(parent != nullptr, "error");
      LfoParentPart *pParent = parent->parent;
      while(pParent && pParent->Cvt2BaseNode()->op != OP_doloop) {
        pParent = pParent->parent;
      }
      if (pParent != nullptr) {
        LfoDoloopNode *doloopParent = static_cast<LfoDoloopNode *>(pParent);
        if (doloopParent->doVarStIdx == sym->stIdx)
          ldnode->defsList.push_front(doloopParent);
        else
          UpdateUsesDefs4Dread(ldnode, drexpr, visited);
      } else {
        UpdateUsesDefs4Dread(ldnode, drexpr, visited);
      }
    }
  }
  return ldnode;
}

LfoParentPart *LfoPreEmitter::EmitLfoExpr(MeExpr *meexpr, LfoParentPart *parent, bool isUpdateUD) {
  MemPool *codeMap = mirFunc->codeMemPool;
  MapleAllocator *codeAlloc = &(mirFunc->codeMemPoolAllocator);
  switch (meexpr->op) {
    case OP_constval: {
      LfoConstvalNode *lcvlNode =
        codeMap->New<LfoConstvalNode>(static_cast<ConstMeExpr *>(meexpr)->constVal, parent);
      InitBaseNodeByMeExpr(lcvlNode, meexpr);
      return lcvlNode;
    }
    case OP_dread: {
      return EmitLfoDread(static_cast<VarMeExpr *>(meexpr), parent, isUpdateUD);
    }
    case OP_eq:
    case OP_ne:
    case OP_ge:
    case OP_gt:
    case OP_le:
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg:
    case OP_lt: {
      OpMeExpr *cmpNode = static_cast<OpMeExpr *>(meexpr);
      LfoCompareNode *lnocmpNode =
        codeMap->New<LfoCompareNode>(meexpr->op, cmpNode->primType, cmpNode->opndType,
                                     nullptr, nullptr, parent);
      LfoParentPart *opnd0 = EmitLfoExpr(cmpNode->GetOpnd(0), lnocmpNode, isUpdateUD);
      LfoParentPart *opnd1 = EmitLfoExpr(cmpNode->GetOpnd(1), lnocmpNode, isUpdateUD);
      lnocmpNode->bOpnd[0] = opnd0->Cvt2BaseNode();
      lnocmpNode->bOpnd[1] = opnd1->Cvt2BaseNode();
      InitBaseNodeByMeExpr(lnocmpNode, meexpr);
      lnocmpNode->opndType = cmpNode->opndType;
      return lnocmpNode;
    }
    case OP_array: {
      NaryMeExpr *arrExpr = static_cast<NaryMeExpr *>(meexpr);
      LfoArrayNode *lnoarrNode =
        codeMap->New<LfoArrayNode>(codeAlloc, arrExpr->primType, arrExpr->tyIdx, parent);
      lnoarrNode->tyIdx = arrExpr->tyIdx;
      lnoarrNode->boundsCheck = arrExpr->boundCheck;
      for (uint32 i = 0; i < arrExpr->numOpnds; i++) {
        LfoParentPart *opnd = EmitLfoExpr(arrExpr->GetOpnd(i), lnoarrNode, isUpdateUD);
        lnoarrNode->nOpnd.push_back(opnd->Cvt2BaseNode());
      }
      InitBaseNodeByMeExpr(lnoarrNode, meexpr);
      return lnoarrNode;
    }
    case OP_ashr:
    case OP_band:
    case OP_bior:
    case OP_bxor:
    case OP_cand:
    case OP_cior:
    case OP_div:
    case OP_land:
    case OP_lior:
    case OP_lshr:
    case OP_max:
    case OP_min:
    case OP_mul:
    case OP_rem:
    case OP_shl:
    case OP_sub:
    case OP_add: {
      OpMeExpr *opExpr = static_cast<OpMeExpr *>(meexpr);
      LfoBinaryNode *lnobinNode =
        codeMap->New<LfoBinaryNode>(meexpr->op, meexpr->primType, parent);
      lnobinNode->bOpnd[0] = EmitLfoExpr(opExpr->GetOpnd(0), lnobinNode, isUpdateUD)->Cvt2BaseNode();
      lnobinNode->bOpnd[1] = EmitLfoExpr(opExpr->GetOpnd(1), lnobinNode, isUpdateUD)->Cvt2BaseNode();
      InitBaseNodeByMeExpr(lnobinNode, meexpr);
      return lnobinNode;
    }
    case OP_iread: {
      IvarMeExpr *ivarExpr = static_cast<IvarMeExpr *>(meexpr);
      LfoIreadNode *lnoirdNode = codeMap->New<LfoIreadNode>(meexpr->op, parent);
      lnoirdNode->uOpnd = EmitLfoExpr(ivarExpr->base, lnoirdNode, isUpdateUD)->Cvt2BaseNode();
      lnoirdNode->tyIdx = ivarExpr->tyIdx;
      lnoirdNode->fieldID = ivarExpr->fieldID;
      InitBaseNodeByMeExpr(lnoirdNode, meexpr);
      return lnoirdNode;
    }
    case OP_addrof: {
      AddrofMeExpr *addrMeexpr = static_cast<AddrofMeExpr *> (meexpr);
      LfoAddrofNode *lnoaddrofNode = codeMap->New<LfoAddrofNode>(parent);
      MIRSymbol *sym = meirmap->ssaTab->GetMIRSymbolFromid(addrMeexpr->ostIdx);
      if (sym->IsLocal()) {
        sym->ResetIsDeleted();
      }
      lnoaddrofNode->stIdx = sym->stIdx;
      lnoaddrofNode->fieldID = addrMeexpr->fieldID;
      InitBaseNodeByMeExpr(lnoaddrofNode, meexpr);
      return lnoaddrofNode;
    }
    case OP_addroflabel: {
      AddroflabelMeExpr *addroflabel = static_cast<AddroflabelMeExpr *>(meexpr);
      LfoAddroflabelNode *lnoaddroflabel = codeMap->New<LfoAddroflabelNode>(parent);
      lnoaddroflabel->primType = PTY_ptr;
      lnoaddroflabel->offset = addroflabel->labelIdx;;
      InitBaseNodeByMeExpr(lnoaddroflabel, meexpr);
      return lnoaddroflabel;
    }
    case OP_addroffunc: {
      AddroffuncMeExpr *addrMeexpr = static_cast<AddroffuncMeExpr *>(meexpr);
      LfoAddroffuncNode *addrfunNode = codeMap->New<LfoAddroffuncNode>(parent);
      addrfunNode->puIdx = addrMeexpr->puIdx;
      InitBaseNodeByMeExpr(addrfunNode, meexpr);
      return addrfunNode;
    }
    case OP_gcmalloc:
    case OP_gcpermalloc:
    case OP_stackmalloc: {
      GcmallocMeExpr *gcMeexpr = static_cast<GcmallocMeExpr *> (meexpr);
      LfoGCMallocNode *gcMnode = codeMap->New<LfoGCMallocNode>(meexpr->op, parent);
      gcMnode->tyIdx = gcMeexpr->tyIdx;
      InitBaseNodeByMeExpr(gcMnode, meexpr);
      return gcMnode;
    }
    case OP_retype: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoRetypeNode *lnoRetNode = codeMap->New<LfoRetypeNode>(OP_retype, parent);
      lnoRetNode->fromPrimType = opMeexpr->opndType;
      lnoRetNode->tyIdx = opMeexpr->tyIdx;
      lnoRetNode->uOpnd = EmitLfoExpr(opMeexpr->GetOpnd(0), lnoRetNode, isUpdateUD)->Cvt2BaseNode();
      InitBaseNodeByMeExpr(lnoRetNode, meexpr);
      return lnoRetNode;
    }
    case OP_ceil:
    case OP_cvt:
    case OP_floor:
    case OP_trunc: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoTypeCvtNode *tycvtNode = codeMap->New<LfoTypeCvtNode>(meexpr->op, parent);
      tycvtNode->fromPrimType = opMeexpr->opndType;
      tycvtNode->uOpnd = EmitLfoExpr(opMeexpr->GetOpnd(0), tycvtNode, isUpdateUD)->Cvt2BaseNode();
      InitBaseNodeByMeExpr(tycvtNode, meexpr);
      return tycvtNode;
    }
    case OP_sext:
    case OP_zext:
    case OP_extractbits: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoExtractbitsNode *extNode = codeMap->New<LfoExtractbitsNode>(meexpr->op, parent);
      extNode->uOpnd = EmitLfoExpr(opMeexpr->GetOpnd(0), extNode, isUpdateUD)->Cvt2BaseNode();
      extNode->bitsOffset = opMeexpr->bitsOffset;
      extNode->bitsSize = opMeexpr->bitsSize;
      InitBaseNodeByMeExpr(extNode, meexpr);
      return extNode;
    }
    case OP_regread: {
      RegMeExpr *regMeexpr = static_cast<RegMeExpr *>(meexpr);
      LfoRegreadNode *regNode = codeMap->New<LfoRegreadNode>(parent);
      regNode->primType = regMeexpr->primType;
      regNode->regIdx = regMeexpr->regIdx;
      InitBaseNodeByMeExpr(regNode, meexpr);
      return regNode;
    }
    case OP_sizeoftype: {
      SizeoftypeMeExpr *sizeofMeexpr = static_cast<SizeoftypeMeExpr *>(meexpr);
      LfoSizeoftypeNode *sizeofTynode = codeMap->New<LfoSizeoftypeNode>(parent);
      sizeofTynode->tyIdx = sizeofMeexpr->tyIdx;
      InitBaseNodeByMeExpr(sizeofTynode, meexpr);
      return sizeofTynode;
    }
    case OP_fieldsdist: {
      FieldsDistMeExpr *fdMeexpr = static_cast<FieldsDistMeExpr *>(meexpr);
      LfoFieldsDistNode *fieldsNode = codeMap->New<LfoFieldsDistNode>(parent);
      fieldsNode->tyIdx = fdMeexpr->GetTyIdx();
      fieldsNode->fieldID1 = fdMeexpr->GetFieldID1();
      fieldsNode->fieldID2 = fdMeexpr->GetFieldID2();
      InitBaseNodeByMeExpr(fieldsNode, meexpr);
      return fieldsNode;
    }
    case OP_conststr: {
      ConststrMeExpr *constrMeexpr = static_cast<ConststrMeExpr *>(meexpr);
      LfoConststrNode *constrNode = codeMap->New<LfoConststrNode>(parent);
      constrNode->strIdx = constrMeexpr->strIdx;
      InitBaseNodeByMeExpr(constrNode, meexpr);
      return constrNode;
    }
    case OP_conststr16: {
      Conststr16MeExpr *constr16Meexpr = static_cast<Conststr16MeExpr *>(meexpr);
      LfoConststr16Node *constr16Node = codeMap->New<LfoConststr16Node>(parent);
      constr16Node->strIdx = constr16Meexpr->strIdx;
      InitBaseNodeByMeExpr(constr16Node, meexpr);
      return constr16Node;
    }
    case OP_abs:
    case OP_bnot:
    case OP_lnot:
    case OP_neg:
    case OP_recip:
    case OP_sqrt:
    case OP_alloca:
    case OP_malloc: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoUnaryNode *unNode = codeMap->New<LfoUnaryNode>(meexpr->op, parent);
      unNode->uOpnd = EmitLfoExpr(opMeexpr->GetOpnd(0), unNode, isUpdateUD)->Cvt2BaseNode();
      InitBaseNodeByMeExpr(unNode, meexpr);
      return unNode;
    }
    case OP_iaddrof: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoIreadNode *unNode = codeMap->New<LfoIreadNode>(meexpr->op, parent);
      unNode->uOpnd = EmitLfoExpr(opMeexpr->GetOpnd(0), unNode, isUpdateUD)->Cvt2BaseNode();
      unNode->tyIdx = opMeexpr->tyIdx;
      unNode->fieldID = opMeexpr->fieldID;
      InitBaseNodeByMeExpr(unNode, meexpr);
      return unNode;
    }
    case OP_resolveinterfacefunc:
    case OP_resolvevirtualfunc: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoResolveFuncNode *resNode = codeMap->New<LfoResolveFuncNode >(meexpr->op, parent);
      resNode->bOpnd[0] = EmitLfoExpr(opMeexpr->GetOpnd(0), resNode, isUpdateUD)->Cvt2BaseNode();
      resNode->bOpnd[1] = EmitLfoExpr(opMeexpr->GetOpnd(1), resNode, isUpdateUD)->Cvt2BaseNode();
      resNode->puIdx = opMeexpr->fieldID;
      InitBaseNodeByMeExpr(resNode, meexpr);
      return resNode;
    }
    case OP_select: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoTernaryNode *tNode = codeMap->New<LfoTernaryNode>(OP_select, parent);
      tNode->topnd[0] = EmitLfoExpr(opMeexpr->GetOpnd(0), tNode, isUpdateUD)->Cvt2BaseNode();
      tNode->topnd[1] = EmitLfoExpr(opMeexpr->GetOpnd(1), tNode, isUpdateUD)->Cvt2BaseNode();
      tNode->topnd[2] = EmitLfoExpr(opMeexpr->GetOpnd(2), tNode, isUpdateUD)->Cvt2BaseNode();
      InitBaseNodeByMeExpr(tNode, meexpr);
      return tNode;
    }
    case OP_intrinsicop:
    case OP_intrinsicopwithtype: {
      NaryMeExpr *nMeexpr = static_cast<NaryMeExpr *>(meexpr);
      LfoIntrinsicopNode *intrnNode = codeMap->New<LfoIntrinsicopNode>(codeAlloc, meexpr->op, parent);
      intrnNode->tyIdx = nMeexpr->tyIdx;
      intrnNode->intrinsic = nMeexpr->intrinsic;
      for (uint32 i = 0; i < nMeexpr->numOpnds; i++) {
        LfoParentPart *opnd = EmitLfoExpr(nMeexpr->GetOpnd(i), intrnNode, isUpdateUD);
        intrnNode->nOpnd.push_back(opnd->Cvt2BaseNode());
      }
      InitBaseNodeByMeExpr(intrnNode, meexpr);
      return intrnNode;
    }
    default:
      CHECK_FATAL(false, "NYI");
  }
}

StmtNode *LfoPreEmitter::EmitLfoDassign(DassignMeStmt *dsmestmt, LfoParentPart *parent) {
  LfoDassignNode *ldssnode =
    mirFunc->codeMemPool->New<LfoDassignNode>(&(mirFunc->codeMemPoolAllocator), parent);
  MIRSymbol *sym = dsmestmt->lhs->ost->GetMIRSymbol();
  if (sym->IsLocal()) {
    sym->ResetIsDeleted();
  }
  ldssnode->stIdx = sym->stIdx;
  ldssnode->fieldID = static_cast<VarMeExpr *>(dsmestmt->lhs)->ost->fieldID;
  ldssnode->srcPosition = dsmestmt->srcPos;
  // dsmestmt's parent is not null means the dsmestmt was not assigned to any block yet
  ldssnode->uOpnd= EmitLfoExpr(dsmestmt->rhs, ldssnode, parent != nullptr)->Cvt2BaseNode();
  return ldssnode;
}

StmtNode *LfoPreEmitter::EmitLfoRegassign(AssignMeStmt *asmestmt, LfoParentPart *parent) {
  LfoRegassignNode *lrssnode =
    mirFunc->codeMemPool->New<LfoRegassignNode>(&(mirFunc->codeMemPoolAllocator), parent);
  lrssnode->primType = asmestmt->lhs->primType;
  lrssnode->regIdx = asmestmt->GetRegLhs()->regIdx;
  lrssnode->uOpnd= EmitLfoExpr(asmestmt->rhs, lrssnode, parent != nullptr)->Cvt2BaseNode();
  lrssnode->srcPosition = asmestmt->srcPos;
  return lrssnode;
}

StmtNode *LfoPreEmitter::EmitLfoIassign(IassignMeStmt *issmestmt, LfoParentPart *parent) {
  LfoIassignNode *lnoIassign =  mirFunc->codeMemPool->New<LfoIassignNode>(&(mirFunc->codeMemPoolAllocator), parent);
  lnoIassign->tyIdx = issmestmt->tyIdx;
  lnoIassign->fieldID = issmestmt->lhsVar->fieldID;
  // issmestmt's parent is not null means the dsmestmt was not assigned to any block yet
  lnoIassign->addrExpr = EmitLfoExpr(issmestmt->lhsVar->base, lnoIassign, parent != nullptr)->Cvt2BaseNode();
  lnoIassign->rhs = EmitLfoExpr(issmestmt->rhs, lnoIassign, parent != nullptr)->Cvt2BaseNode();
  lnoIassign->srcPosition = issmestmt->srcPos;
  return lnoIassign;
}


StmtNode* LfoPreEmitter::EmitLfoStmt(MeStmt *mestmt, LfoParentPart *parent) {
  MapleMap<MeStmt *, StmtNode *>::iterator it = meir2stmtMap.find(mestmt);
  bool wasEmitedstmt = (it != meir2stmtMap.end());
  switch (mestmt->op) {
    case OP_dassign: {
      DassignMeStmt *dssMestmt = static_cast<DassignMeStmt *>(mestmt);
      if (wasEmitedstmt) {
        LfoDassignNode *lDssnode = static_cast<LfoDassignNode *>(it->second);
        lDssnode->parent = parent;
        UpdateUsesDefs(lDssnode->uOpnd, dssMestmt->rhs);
        return it->second;
      } else {
        StmtNode *retStmt = EmitLfoDassign(dssMestmt, parent);
        meir2stmtMap[mestmt] = retStmt;
        return retStmt;
      }
    }
    case OP_regassign: {
      AssignMeStmt *asMestmt = static_cast<AssignMeStmt *>(mestmt);
      if (wasEmitedstmt) {
        LfoRegassignNode *lRssnode = static_cast<LfoRegassignNode *>(it->second);
        lRssnode->parent = parent;
        UpdateUsesDefs(lRssnode->uOpnd, asMestmt->rhs);
        return it->second;
      } else {
        StmtNode *retStmt = EmitLfoRegassign(asMestmt, parent);
        meir2stmtMap[mestmt] = retStmt;
        return retStmt;
      }
    }
    case OP_iassign: {
      IassignMeStmt *issMestmt = static_cast<IassignMeStmt *>(mestmt);
      if (wasEmitedstmt) {
        LfoIassignNode *lIssnode = static_cast<LfoIassignNode *>(it->second);
        lIssnode->parent = parent;
        UpdateUsesDefs(lIssnode->addrExpr, issMestmt->lhsVar->base);
        UpdateUsesDefs(lIssnode->rhs, issMestmt->rhs);
        return lIssnode;
      } else {
        StmtNode *retStmt = EmitLfoIassign(issMestmt, parent);
        meir2stmtMap[mestmt] = retStmt;
        return retStmt;
      }
    }
    case OP_return: {
      if (wasEmitedstmt) {
        static_cast<LfoNaryStmtNode *>(it->second)->parent = parent;
        return it->second;
      }
      LfoNaryStmtNode *lnoRet =
        mirFunc->codeMemPool->New<LfoNaryStmtNode>(&(mirFunc->codeMemPoolAllocator), OP_return, parent);
      NaryMeStmt *naryMestmt = static_cast<NaryMeStmt *>(mestmt);
      for (uint32 i = 0; i < naryMestmt->opnds.size(); i++) {
        lnoRet->nOpnd.push_back(EmitLfoExpr(naryMestmt->opnds[i], lnoRet, parent != nullptr)->Cvt2BaseNode());
      }
      lnoRet->numOpnds = naryMestmt->opnds.size();
      lnoRet->srcPosition = naryMestmt->srcPos;
      return lnoRet;
    }
    case OP_goto: {
      CHECK_FATAL(!wasEmitedstmt, "EmitLfoStmt:: goto could not have been emitedfalse impossible");
      GotoMeStmt *gotoStmt = static_cast<GotoMeStmt *>(mestmt);
      if (lfoFunc->LabelCreatedByLfo(gotoStmt->offset)) {
        return NULL;
      }
      LfoGotoNode *gto = mirFunc->codeMemPool->New<LfoGotoNode>(OP_goto, parent);
      gto->offset = gotoStmt->offset;
      gto->srcPosition = gotoStmt->srcPos;
      return gto;
    }
    case OP_igoto: {
      CHECK_FATAL(!wasEmitedstmt, "EmitLfoStmt:: igoto could not have been emited");
      UnaryMeStmt *igotoMeStmt = static_cast<UnaryMeStmt *>(mestmt);
      LfoUnaryStmtNode *igto = mirFunc->codeMemPool->New<LfoUnaryStmtNode>(OP_igoto, parent);
      igto->uOpnd = EmitLfoExpr(igotoMeStmt->opnd, igto, parent != nullptr)->Cvt2BaseNode();
      igto->srcPosition = igotoMeStmt->srcPos;
      return igto;
    }
    case OP_comment: {
      CommentMeStmt *cmtmeNode = static_cast<CommentMeStmt *>(mestmt);
      LfoCommentNode *cmtNode =
        mirFunc->codeMemPool->New<LfoCommentNode>(&(mirFunc->codeMemPoolAllocator), parent);
      cmtNode->comment = cmtmeNode->comment;
      cmtNode->srcPosition = cmtmeNode->srcPos;
      return cmtNode;
    }
    case OP_call:
    case OP_virtualcall:
    case OP_virtualicall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_interfaceicall:
    case OP_customcall:
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccall:
    case OP_polymorphiccallassigned: {
      if (wasEmitedstmt) {
        static_cast<LfoCallNode *>(it->second)->parent = parent;
        return it->second;
      }
      LfoCallNode *callnode =
        mirFunc->codeMemPool->New<LfoCallNode>(&(mirFunc->codeMemPoolAllocator), mestmt->op, parent);
      CallMeStmt *callMeStmt = static_cast<CallMeStmt *>(mestmt);
      callnode->puIdx = callMeStmt->puIdx;
      callnode->tyIdx = callMeStmt->tyIdx;
      callnode->numOpnds = callMeStmt->opnds.size();
      callnode->srcPosition = callMeStmt->srcPos;
      mestmt->EmitCallReturnVector(meirmap->mirFunc->meSSATab, &callnode->returnValues);
      for (uint32 i = 0; i < callMeStmt->opnds.size(); i++) {
        callnode->nOpnd.push_back(EmitLfoExpr(callMeStmt->opnds[i], callnode, parent != nullptr)->Cvt2BaseNode());
      }
      return callnode;
    }
    case OP_icall:
    case OP_icallassigned: {
      if (wasEmitedstmt) {
        static_cast<LfoCallNode *>(it->second)->parent = parent;
        return it->second;
      }
      IcallMeStmt *icallMeStmt = static_cast<IcallMeStmt *> (mestmt);
      LfoIcallNode *icallnode =
        mirFunc->codeMemPool->New<LfoIcallNode>(&(mirFunc->codeMemPoolAllocator), OP_icallassigned, icallMeStmt->retTyIdx, parent);
      for (uint32 i = 0; i < icallMeStmt->opnds.size(); i++) {
        icallnode->nOpnd.push_back(EmitLfoExpr(icallMeStmt->opnds[i], icallnode, parent != nullptr)->Cvt2BaseNode());
      }
      icallnode->numOpnds = icallMeStmt->opnds.size();
      icallnode->srcPosition = mestmt->srcPos;
      mestmt->EmitCallReturnVector(meirmap->mirFunc->meSSATab, &icallnode->returnValues);
      icallnode->retTyIdx = TyIdx(PTY_void);
      for (uint32 j = 0; j < icallnode->returnValues.size(); j++) {
        CallReturnPair retpair = icallnode->returnValues[j];
        if (!retpair.second.IsReg()) {
          StIdx stIdx = retpair.first;
          MIRSymbolTable *symtab = mirFunc->symTab;
          MIRSymbol *sym = symtab->GetSymbolFromStIdx(stIdx.Idx());
          icallnode->retTyIdx = sym->GetType()->GetTypeIndex();
          if (stIdx.Islocal()) {
            sym->ResetIsDeleted();
          }
        } else {
          PregIdx pregidx = (PregIdx)retpair.second.pregIdx;
          MIRPreg *preg = mirFunc->pregTab->PregFromPregIdx(pregidx);
          icallnode->retTyIdx = TyIdx(preg->primType);
        }
      }
      return icallnode;
    }
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_intrinsiccallwithtype:
    case OP_intrinsiccallwithtypeassigned: {
      if (wasEmitedstmt) {
        static_cast<LfoCallNode *>(it->second)->parent = parent;
        return it->second;
      }
      IntrinsiccallMeStmt *callMeStmt = static_cast<IntrinsiccallMeStmt *> (mestmt);
      LfoIntrinsiccallNode *callnode =
        mirFunc->codeMemPool->New<LfoIntrinsiccallNode>(&(mirFunc->codeMemPoolAllocator), mestmt->op,
            callMeStmt->intrinsic, parent);
      callnode->intrinsic = callMeStmt->intrinsic;
      callnode->tyIdx = callMeStmt->tyIdx;
      for (uint32 i = 0; i < callMeStmt->opnds.size(); i++) {
        callnode->nOpnd.push_back(EmitLfoExpr(callMeStmt->opnds[i], callnode, parent != nullptr)->Cvt2BaseNode());
      }
      callnode->numOpnds = callnode->nOpnd.size();
      callnode->srcPosition = mestmt->srcPos;
      if (kOpcodeInfo.IsCallAssigned(mestmt->op)) {
        mestmt->EmitCallReturnVector(meirmap->mirFunc->meSSATab, &callnode->returnValues);
        for (uint32 j = 0; j < callnode->returnValues.size(); j++) {
          CallReturnPair retpair = callnode->returnValues[j];
          if (!retpair.second.IsReg()) {
            StIdx stIdx = retpair.first;
            if (stIdx.Islocal()) {
              MIRSymbolTable *symtab = mirFunc->symTab;
              MIRSymbol *sym = symtab->GetSymbolFromStIdx(stIdx.Idx());
              sym->ResetIsDeleted();
            }
          }
        }
      }
      return callnode;
    }
    case OP_jscatch:
    case OP_finally:
    case OP_endtry:
    case OP_cleanuptry:
    case OP_membaracquire:
    case OP_membarrelease:
    case OP_membarstorestore:
    case OP_membarstoreload: {
      LfoStmtNode *lnoStmtNode = mirFunc->codeMemPool->New<LfoStmtNode>(parent, mestmt->op);
      lnoStmtNode->srcPosition = mestmt->srcPos;
      return lnoStmtNode;
    }
    case OP_retsub: {
      LfoWithUsesStmtNode * usesStmtNode = mirFunc->codeMemPool->New<LfoWithUsesStmtNode>(&(mirFunc->codeMemPoolAllocator), parent, mestmt->op);
      usesStmtNode->srcPosition = mestmt->srcPos;
      return usesStmtNode;
    }
    case OP_gosub: {
      LfoGosubNode *lnoGosubNode = mirFunc->codeMemPool->New<LfoGosubNode>(&(mirFunc->codeMemPoolAllocator), parent);
      lnoGosubNode->srcPosition = mestmt->srcPos;
      return lnoGosubNode;
    }
    case OP_brfalse:
    case OP_brtrue: {
      LfoCondGotoNode *lnoCondNode = mirFunc->codeMemPool->New<LfoCondGotoNode>(mestmt->op, parent);
      CondGotoMeStmt *condMeStmt = static_cast<CondGotoMeStmt *> (mestmt);
      lnoCondNode->offset = condMeStmt->offset;
      lnoCondNode->srcPosition = mestmt->srcPos;
      lnoCondNode->uOpnd = EmitLfoExpr(condMeStmt->opnd, lnoCondNode, parent != nullptr)->Cvt2BaseNode();
      return lnoCondNode;
    }
    case OP_try:
    case OP_cpptry:
    case OP_javatry: {
      LfoTryNode *jvTryNode = mirFunc->codeMemPool->New<LfoTryNode>(&(mirFunc->codeMemPoolAllocator), mestmt->op, parent);
      TryMeStmt *tryMeStmt = static_cast<TryMeStmt *> (mestmt);
      uint32 offsetsSize = tryMeStmt->offsets.size();
      jvTryNode->offsets.resize(offsetsSize);
      for (uint32 i = 0; i < offsetsSize; i++) {
        jvTryNode->offsets[i] = tryMeStmt->offsets[i];
      }
      jvTryNode->srcPosition = tryMeStmt->srcPos;
      return jvTryNode;
    }
    case OP_cppcatch: {
      LfoCppCatchNode *cppCatchNode = mirFunc->codeMemPool->New<LfoCppCatchNode>(parent);
      CppCatchMeStmt *catchMestmt = static_cast<CppCatchMeStmt *> (mestmt);
      cppCatchNode->exceptionTyIdx = catchMestmt->exceptionTyIdx;
      cppCatchNode->srcPosition = catchMestmt->srcPos;
      return cppCatchNode;
    }
    case OP_catch:
    case OP_javacatch: {
      LfoCatchNode *jvCatchNode = mirFunc->codeMemPool->New<LfoCatchNode>(&(mirFunc->codeMemPoolAllocator),
          parent);
      JavaCatchMeStmt *catchMestmt = static_cast<JavaCatchMeStmt *> (mestmt);
      jvCatchNode->exceptionTyIdxVec = catchMestmt->exceptionTyIdxVec;
      jvCatchNode->srcPosition = catchMestmt->srcPos;
      return jvCatchNode;
    }
    case OP_throw: {
      LfoThrowStmt *throwStmt = mirFunc->codeMemPool->New<LfoThrowStmt>(&(mirFunc->codeMemPoolAllocator),
        mestmt->op, parent);
      ThrowMeStmt *throwMeStmt = static_cast<ThrowMeStmt *>(mestmt);
      throwStmt->uOpnd = EmitLfoExpr(throwMeStmt->opnd, throwStmt, parent != nullptr)->Cvt2BaseNode();
      throwStmt->srcPosition = throwMeStmt->srcPos;
      return throwStmt;
    }
    case OP_syncenter:
    case OP_syncexit: {
      LfoSyncStmt *syncStmt = mirFunc->codeMemPool->New<LfoSyncStmt>(&(mirFunc->codeMemPoolAllocator),
              mestmt->op, parent);
      SyncMeStmt *tmestmt = static_cast<SyncMeStmt *>(mestmt);
      for (uint32 i = 0; i < tmestmt->opnds.size(); i++) {
        syncStmt->nOpnd.push_back(EmitLfoExpr(tmestmt->opnds[i], syncStmt, parent != nullptr)->Cvt2BaseNode());
      }
      syncStmt->srcPosition = tmestmt->srcPos;
      return syncStmt;
    }
    case OP_assertnonnull:
    case OP_eval:
    case OP_free: {
      LfoUnaryStmtNode *unaryStmt = mirFunc->codeMemPool->New<LfoUnaryStmtNode>(mestmt->op, parent);
      UnaryMeStmt *uMeStmt = static_cast<UnaryMeStmt *>(mestmt);
      unaryStmt->uOpnd = EmitLfoExpr(uMeStmt->opnd, unaryStmt, parent != nullptr)->Cvt2BaseNode();
      unaryStmt->srcPosition = uMeStmt->srcPos;
      return unaryStmt;
    }
    case OP_switch: {
      LfoSwitchNode *switchNode = mirFunc->codeMemPool->New<LfoSwitchNode>(&(mirFunc->codeMemPoolAllocator),
           parent);
      SwitchMeStmt *meSwitch = static_cast<SwitchMeStmt *>(mestmt);
      switchNode->switchOpnd = EmitLfoExpr(meSwitch->opnd, switchNode, parent != nullptr)->Cvt2BaseNode();
      switchNode->defaultLabel = meSwitch->defaultLabel;
      switchNode->switchTable = meSwitch->switchTable;
      switchNode->srcPosition = meSwitch->srcPos;
      return switchNode;
    }
    case OP_jstry:
    default:
      CHECK_FATAL(false, "nyi");
  }
}

LfoBlockNode* LfoPreEmitter::EmitLfoBlockNode (LfoParentPart *parent) {
  LfoBlockNode *blkNode =
    mirFunc->codeMemPool->New<LfoBlockNode>(parent);
  return blkNode;
}

void LfoPreEmitter::EmitBB(BB *bb, LfoBlockNode *curblk) {
  CHECK_FATAL(curblk != nullptr, "null ptr check");
  // emit head. label
  LabelIdx labidx = bb->bbLabel;
  if (labidx != 0 && !lfoFunc->LabelCreatedByLfo(labidx)) {
    // not a empty bb
    LabelNode *lbnode = meirmap->ssaTab->mirModule.CurFunction()->codeMemPool->New<LabelNode>();
    lbnode->labelIdx = labidx;
    curblk->AddStatement(lbnode);
  }
  for (auto mestmt : bb->meStmtList) {
    StmtNode *stmt = EmitLfoStmt(mestmt, curblk);
    if (!stmt) // can be null i.e, a goto to a label that was created by lno lower
      continue;
    curblk->AddStatement(stmt);
  }
  if (bb->IsEndTry()) {
    /* generate op_endtry */
    StmtNode *endtry = meirmap->ssaTab->mirModule.CurFunction()->codeMemPool->New<StmtNode>(OP_endtry);
    curblk->AddStatement(endtry);
  }
}

DoloopNode *LfoPreEmitter::EmitLfoDoloop(BB *mewhilebb, LfoBlockNode *curblk, LfoWhileInfo *whileInfo) {
  LabelIdx labidx = mewhilebb->bbLabel;
  CHECK_FATAL(labidx != 0, "no go back edge for the while bb");
  CHECK_FATAL(mewhilebb->meStmtList.last->op == OP_brfalse, "expect brfalse for while loop cond");
  MemPool *codeMap = mirFunc->codeMemPool;
  LfoDoloopNode *lnoDoloopnode = codeMap->New<LfoDoloopNode>(curblk);
  LfoParentPart *lnoIvnode = EmitLfoExpr(whileInfo->iv, lnoDoloopnode, true);
  CHECK_FATAL(lnoIvnode->Cvt2BaseNode()->op == OP_dread, "error");
  LfoDreadNode *dreadIvnode = static_cast<LfoDreadNode *>(lnoIvnode);
  MeStmt *lastmestmt = mewhilebb->meStmtList.last;
  CHECK_FATAL(lastmestmt->op == OP_brfalse, "NYI");
  //BaseNode *testnode = static_cast<CondGotoMeStmt *>(lastmestmt)->opnd->EmitExpr(meirmap->ssaTab);
  lnoDoloopnode->doVarStIdx = dreadIvnode->stIdx;
  lnoDoloopnode->startExpr =  EmitLfoExpr(whileInfo->startmeexpr, lnoDoloopnode, true)->Cvt2BaseNode();
  lnoDoloopnode->condExpr = EmitLfoExpr(static_cast<CondGotoMeStmt *>(lastmestmt)->opnd, lnoDoloopnode, true)->Cvt2BaseNode();
  lnoDoloopnode->incrExpr = EmitLfoExpr(whileInfo->incrmeexpr, lnoDoloopnode, true)->Cvt2BaseNode();
  lnoDoloopnode->doBody = codeMap->New<LfoBlockNode>(lnoDoloopnode);
  curblk->AddStatement(lnoDoloopnode);
  return lnoDoloopnode;
}

WhileStmtNode *LfoPreEmitter::EmitLfoWhile(BB *meWhilebb, LfoBlockNode *curblk) {
  LabelIdx labidx = meWhilebb->bbLabel;
  MeStmt *lastmestmt = meWhilebb->meStmtList.last;
  MemPool *codeMap = mirFunc->codeMemPool;
  LfoWhileStmtNode *lnoWhilestmt = codeMap->New<LfoWhileStmtNode>(curblk);
  CondGotoMeStmt *condGotostmt = static_cast<CondGotoMeStmt *>(lastmestmt);
  lnoWhilestmt->uOpnd = EmitLfoExpr(condGotostmt->opnd, lnoWhilestmt, false)->Cvt2BaseNode();
  lnoWhilestmt->body = codeMap->New<LfoBlockNode>(lnoWhilestmt);
  curblk->AddStatement(lnoWhilestmt);
  return lnoWhilestmt;
}

uint32 LfoPreEmitter::Raise2LfoWhile(uint32 curj, LfoBlockNode *curblk) {
  MapleVector<BB *> &bbvec = cfg->bbVec;
  BB *curbb = bbvec[curj];
  LabelIdx whilelabidx = curbb->bbLabel;
  LfoWhileInfo *whileInfo = lfoFunc->label2WhileInfo[whilelabidx];

  // find the end label bb
  BB *suc0 = curbb->succ[0];
  BB *suc1 = curbb->succ[1];
  MeStmt *laststmt = curbb->meStmtList.last;
  CHECK_FATAL(laststmt->op == OP_brfalse, "Riase2LfoWhile: NYI");
  CondGotoMeStmt *condgotomestmt = static_cast<CondGotoMeStmt *>(laststmt);
  BB *endlblbb = condgotomestmt->offset == suc1->bbLabel ? suc1 : suc0;
  BB *firstWhilebb = condgotomestmt->offset == suc1->bbLabel ? suc0 : suc1;
  LfoBlockNode *lnoDobody = nullptr;
  if (whileInfo->canConvertDoloop) {  // emit doloop
    DoloopNode *doloopnode = EmitLfoDoloop(curbb, curblk, whileInfo);
    ++curj;
    lnoDobody = static_cast<LfoBlockNode *>(doloopnode->doBody);
  } else { // emit while loop
    WhileStmtNode *whileNode = EmitLfoWhile(curbb, curblk);
    ++curj;
    lnoDobody = static_cast<LfoBlockNode *> (whileNode->body);
  }
  // emit loop body
  while (bbvec[curj]->id != endlblbb->id) {
    curj = EmitLfoBB(curj, lnoDobody);
    while (bbvec[curj] == nullptr) {
      curj++;
    }
  }
  return curj;
}

uint32 LfoPreEmitter::Raise2LfoIf(uint32 curj, LfoBlockNode *curblk) {
  MapleVector<BB *> &bbvec = cfg->bbVec;
  BB *curbb = bbvec[curj];
  // emit BB contents before the if statement
  LabelIdx labidx = curbb->bbLabel;
  if (labidx != 0 && !lfoFunc->LabelCreatedByLfo(labidx)) {
    LabelNode *lbnode = mirFunc->codeMemPool->New<LabelNode>();
    lbnode->labelIdx = labidx;
    curblk->AddStatement(lbnode);
  }
  MeStmt *mestmt = curbb->meStmtList.first;
  while (mestmt->op != OP_brfalse && mestmt->op != OP_brtrue) {
    StmtNode *stmt = EmitLfoStmt(mestmt, curblk);
    curblk->AddStatement(stmt);
    mestmt = mestmt->next;
  }
  // emit the if statement
  CHECK_FATAL(mestmt != nullptr && (mestmt->op == OP_brfalse || mestmt->op == OP_brtrue), "Raise2LfoIf: cannot find conditional branch");
  CondGotoMeStmt *condgoto = static_cast <CondGotoMeStmt *>(mestmt);
  LfoIfInfo *ifInfo = lfoFunc->label2IfInfo[condgoto->offset];
  CHECK_FATAL(ifInfo->endLabel != 0, "Raise2LfoIf: endLabel not found");
  LfoIfStmtNode *lnoIfstmtNode = mirFunc->codeMemPool->New<LfoIfStmtNode>(curblk);
  LfoParentPart *condnode = EmitLfoExpr(condgoto->opnd, lnoIfstmtNode, true);
  lnoIfstmtNode->uOpnd = condnode->Cvt2BaseNode();
  curblk->AddStatement(lnoIfstmtNode);
  if (ifInfo->elseLabel != 0) {  // both else and then are not empty;
    LfoBlockNode *elseBlk = EmitLfoBlockNode(lnoIfstmtNode);
    LfoBlockNode *thenBlk = EmitLfoBlockNode(lnoIfstmtNode);
    lnoIfstmtNode->thenPart = thenBlk;
    lnoIfstmtNode->elsePart = elseBlk;
    BB *elsemebb = cfg->labelBBIdMap[ifInfo->elseLabel];
    BB *endmebb = cfg->labelBBIdMap[ifInfo->endLabel];
    CHECK_FATAL(elsemebb, "Raise2LfoIf: cannot find else BB");
    CHECK_FATAL(endmebb, "Raise2LfoIf: cannot find BB at end of IF");
    // emit then branch;
    uint32 j = curj + 1;
    while (j != elsemebb->id.idx) {
      j = EmitLfoBB(j, thenBlk);
    }
    CHECK_FATAL(j < bbvec.size(), "");
    while (j != endmebb->id.idx) {
      j = EmitLfoBB(j, elseBlk);
    }
    CHECK_FATAL(j < bbvec.size(), "");
    return j;
  } else {  // there is only then or else part in this if stmt
    LfoBlockNode *branchBlock = EmitLfoBlockNode(lnoIfstmtNode);
    LfoBlockNode *emptyBlock = EmitLfoBlockNode(lnoIfstmtNode);
    if (condgoto->op == OP_brtrue) {
      lnoIfstmtNode->elsePart = branchBlock;
      lnoIfstmtNode->thenPart = emptyBlock;
    } else {
      lnoIfstmtNode->thenPart = branchBlock;
      lnoIfstmtNode->elsePart = emptyBlock;
    }
    BB *endmebb = cfg->labelBBIdMap[ifInfo->endLabel];
    uint32 j = curj + 1;
    while (j != endmebb->id.idx) {
      j = EmitLfoBB(j, branchBlock);
    }
    CHECK_FATAL(j < bbvec.size(), "");
    return j;
  }
}

uint32 LfoPreEmitter::EmitLfoBB(uint32 curj, LfoBlockNode *curblk) {
  MapleVector<BB *> &bbvec = cfg->bbVec;
  BB *mebb = bbvec[curj];
  if (!mebb || mebb == cfg->commonEntryBB || mebb == cfg->commonExitBB) {
    return curj + 1;
  }
  if (mebb->bbLabel != 0) {
    MapleMap<LabelIdx, LfoWhileInfo*>::iterator it = lfoFunc->label2WhileInfo.find(mebb->bbLabel);
    if (it != lfoFunc->label2WhileInfo.end()) {
      if (mebb->succ.size() == 2) {
        curj = Raise2LfoWhile(curj, curblk);
        return curj;
      } else {
        lfoFunc->lfoCreatedLabelSet.erase(mebb->bbLabel);
      }
    }
  }
  if (!mebb->meStmtList.empty() &&
      (mebb->meStmtList.last->op == OP_brfalse ||
       mebb->meStmtList.last->op == OP_brtrue)) {
    CondGotoMeStmt *condgoto = static_cast<CondGotoMeStmt *>(mebb->meStmtList.last);
    MapleMap<LabelIdx, LfoIfInfo*>::iterator it = lfoFunc->label2IfInfo.find(condgoto->offset);
    if (it != lfoFunc->label2IfInfo.end()) {
      curj = Raise2LfoIf(curj, curblk);
      return curj;
    }
  }
  EmitBB(mebb, curblk);
  return ++curj;
}

AnalysisResult *DoLfoPreEmission::Run(MeFunction *func, MeFuncResultMgr *m) {
  if (func->theCFG->NumBBs() == 0) {
    func->isLfo = false;
    return nullptr;
  }
  MeIRMap *hmap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func));
  ASSERT(hmap != nullptr, "irmapbuild has problem");
  MIRFunction *mirfunction = func->mirFunc;
  if (mirfunction->codeMemPool != nullptr) {
    mempoolctrler.DeleteMemPool(mirfunction->codeMemPool);
  }
  mirfunction->codeMemPool = mempoolctrler.NewMemPool("IR from IRMap::Emit()");
  mirfunction->codeMemPoolAllocator.SetMemPool(mirfunction->codeMemPool);
  // initialize isDeleted field to true; will reset when emitting Maple IR
  for (size_t k = 1; k < mirfunction->symTab->GetSymbolTableSize(); k++) {
    MIRSymbol *sym = mirfunction->symTab->GetSymbolFromStIdx(k);
    if (sym->sKind == kStVar &&
        sym->GetName() != "__Exc_Ptr__" &&
        sym->GetName() != "__Exc_Filter__") {
      sym->SetIsDeleted();
    }
  }
#if 0
  mirfunction->body = mirfunction->codeMemPool->New<BlockNode>();
  for (uint32 i = 0; i < func->theCFG->bbVec.size(); i++) {
    BB *bb = func->theCFG->bbVec[i];
    if (bb == nullptr) {
      continue;
    }
    bb->EmitBB(func->meSSATab, func->mirFunc->body, func->mirFunc->freqMap, true);
  }
#else
  LfoPreEmitter emitter(hmap, func->lfoFunc);
  LfoBlockNode *curblk = emitter.EmitLfoBlockNode(nullptr);
  mirfunction->body = curblk;
  uint32 i = 0;
  while (i < func->theCFG->bbVec.size()) {
    i = emitter.EmitLfoBB(i, curblk);
  }
#endif

  m->InvalidAllResults();
  func->meSSATab = nullptr;
  func->irMap = nullptr;
  func->isLfo = false;

  maple::ConstantFold cf(&func->mirModule);
  cf.Simplify(func->mirFunc->body);

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n**** After lfopreemit phase ****\n";
    mirfunction->Dump(false);
  }

#if 1  // use this only if directly feeding to mainopt
  MIRLower mirlowerer(func->mirModule, mirfunction);
  mirlowerer.SetLowerME();
  mirlowerer.SetLowerExpandArray();
  mirlowerer.LowerFunc(mirfunction);
#endif

  return nullptr;
}

}  // namespace maple
