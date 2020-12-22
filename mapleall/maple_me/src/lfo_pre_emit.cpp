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

LfoParentPart *LfoPreEmitter::EmitLfoExpr(MeExpr *meexpr, LfoParentPart *parent) {
  switch (meexpr->op) {
    case OP_constval: {
      LfoConstvalNode *lcvlNode =
        codeMP->New<LfoConstvalNode>(static_cast<ConstMeExpr *>(meexpr)->constVal, parent);
      return lcvlNode;
    }
    case OP_dread: {
      VarMeExpr *varmeexpr = static_cast<VarMeExpr *>(meexpr);
      MIRSymbol *sym = varmeexpr->ost->GetMIRSymbol();
      LfoDreadNode *ldnode = codeMP->New<LfoDreadNode>(varmeexpr->primType, sym->stIdx, varmeexpr->ost->fieldID, parent, varmeexpr);
      return ldnode;
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
        codeMP->New<LfoCompareNode>(meexpr->op, cmpNode->primType, cmpNode->opndType,
                                     nullptr, nullptr, parent);
      LfoParentPart *opnd0 = EmitLfoExpr(cmpNode->GetOpnd(0), lnocmpNode);
      LfoParentPart *opnd1 = EmitLfoExpr(cmpNode->GetOpnd(1), lnocmpNode);
      lnocmpNode->bOpnd[0] = opnd0->Cvt2BaseNode();
      lnocmpNode->bOpnd[1] = opnd1->Cvt2BaseNode();
      lnocmpNode->opndType = cmpNode->opndType;
      return lnocmpNode;
    }
    case OP_array: {
      NaryMeExpr *arrExpr = static_cast<NaryMeExpr *>(meexpr);
      LfoArrayNode *lnoarrNode =
        codeMP->New<LfoArrayNode>(codeMPAlloc, arrExpr->primType, arrExpr->tyIdx, parent);
      lnoarrNode->tyIdx = arrExpr->tyIdx;
      lnoarrNode->boundsCheck = arrExpr->boundCheck;
      for (uint32 i = 0; i < arrExpr->numOpnds; i++) {
        LfoParentPart *opnd = EmitLfoExpr(arrExpr->GetOpnd(i), lnoarrNode);
        lnoarrNode->nOpnd.push_back(opnd->Cvt2BaseNode());
      }
      lnoarrNode->numOpnds = meexpr->numOpnds;
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
        codeMP->New<LfoBinaryNode>(meexpr->op, meexpr->primType, parent);
      lnobinNode->bOpnd[0] = EmitLfoExpr(opExpr->GetOpnd(0), lnobinNode)->Cvt2BaseNode();
      lnobinNode->bOpnd[1] = EmitLfoExpr(opExpr->GetOpnd(1), lnobinNode)->Cvt2BaseNode();
      return lnobinNode;
    }
    case OP_iread: {
      IvarMeExpr *ivarExpr = static_cast<IvarMeExpr *>(meexpr);
      LfoIreadNode *lnoirdNode = codeMP->New<LfoIreadNode>(meexpr->op, meexpr->primType, parent, ivarExpr);
      lnoirdNode->uOpnd = EmitLfoExpr(ivarExpr->base, lnoirdNode)->Cvt2BaseNode();
      lnoirdNode->tyIdx = ivarExpr->tyIdx;
      lnoirdNode->fieldID = ivarExpr->fieldID;
      return lnoirdNode;
    }
    case OP_addrof: {
      AddrofMeExpr *addrMeexpr = static_cast<AddrofMeExpr *> (meexpr);
      MIRSymbol *sym = meirmap->ssaTab->GetMIRSymbolFromid(addrMeexpr->ostIdx);
      LfoAddrofNode *lnoaddrofNode = codeMP->New<LfoAddrofNode>(addrMeexpr->primType, sym->stIdx, addrMeexpr->fieldID, parent);
      return lnoaddrofNode;
    }
    case OP_addroflabel: {
      AddroflabelMeExpr *addroflabel = static_cast<AddroflabelMeExpr *>(meexpr);
      LfoAddroflabelNode *lnoaddroflabel = codeMP->New<LfoAddroflabelNode>(addroflabel->labelIdx, parent);
      lnoaddroflabel->primType = PTY_ptr;
      lnoaddroflabel->primType = meexpr->primType;
      return lnoaddroflabel;
    }
    case OP_addroffunc: {
      AddroffuncMeExpr *addrMeexpr = static_cast<AddroffuncMeExpr *>(meexpr);
      LfoAddroffuncNode *addrfunNode = codeMP->New<LfoAddroffuncNode>(addrMeexpr->primType, addrMeexpr->puIdx, parent);
      return addrfunNode;
    }
    case OP_gcmalloc:
    case OP_gcpermalloc:
    case OP_stackmalloc: {
      GcmallocMeExpr *gcMeexpr = static_cast<GcmallocMeExpr *> (meexpr);
      LfoGCMallocNode *gcMnode = codeMP->New<LfoGCMallocNode>(meexpr->op, meexpr->primType, gcMeexpr->tyIdx, parent);
      gcMnode->tyIdx = gcMeexpr->tyIdx;
      return gcMnode;
    }
    case OP_retype: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoRetypeNode *lnoRetNode = codeMP->New<LfoRetypeNode>(OP_retype, meexpr->primType, parent);
      lnoRetNode->fromPrimType = opMeexpr->opndType;
      lnoRetNode->tyIdx = opMeexpr->tyIdx;
      lnoRetNode->uOpnd = EmitLfoExpr(opMeexpr->GetOpnd(0), lnoRetNode)->Cvt2BaseNode();
      return lnoRetNode;
    }
    case OP_ceil:
    case OP_cvt:
    case OP_floor:
    case OP_trunc: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoTypeCvtNode *tycvtNode = codeMP->New<LfoTypeCvtNode>(meexpr->op, meexpr->primType, parent);
      tycvtNode->fromPrimType = opMeexpr->opndType;
      tycvtNode->uOpnd = EmitLfoExpr(opMeexpr->GetOpnd(0), tycvtNode)->Cvt2BaseNode();
      return tycvtNode;
    }
    case OP_sext:
    case OP_zext:
    case OP_extractbits: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoExtractbitsNode *extNode = codeMP->New<LfoExtractbitsNode>(meexpr->op, meexpr->primType, parent);
      extNode->uOpnd = EmitLfoExpr(opMeexpr->GetOpnd(0), extNode)->Cvt2BaseNode();
      extNode->bitsOffset = opMeexpr->bitsOffset;
      extNode->bitsSize = opMeexpr->bitsSize;
      return extNode;
    }
    case OP_regread: {
      RegMeExpr *regMeexpr = static_cast<RegMeExpr *>(meexpr);
      LfoRegreadNode *regNode = codeMP->New<LfoRegreadNode>(parent, regMeexpr);
      regNode->primType = regMeexpr->primType;
      regNode->regIdx = regMeexpr->regIdx;
      return regNode;
    }
    case OP_sizeoftype: {
      SizeoftypeMeExpr *sizeofMeexpr = static_cast<SizeoftypeMeExpr *>(meexpr);
      LfoSizeoftypeNode *sizeofTynode = codeMP->New<LfoSizeoftypeNode>(sizeofMeexpr->primType, sizeofMeexpr->tyIdx, parent);
      return sizeofTynode;
    }
    case OP_fieldsdist: {
      FieldsDistMeExpr *fdMeexpr = static_cast<FieldsDistMeExpr *>(meexpr);
      LfoFieldsDistNode *fieldsNode = codeMP->New<LfoFieldsDistNode>(
        fdMeexpr->primType, fdMeexpr->GetTyIdx(), fdMeexpr->GetFieldID1(),
        fdMeexpr->GetFieldID2(), parent);
      return fieldsNode;
    }
    case OP_conststr: {
      ConststrMeExpr *constrMeexpr = static_cast<ConststrMeExpr *>(meexpr);
      LfoConststrNode *constrNode = codeMP->New<LfoConststrNode>(constrMeexpr->primType, constrMeexpr->strIdx, parent);
      return constrNode;
    }
    case OP_conststr16: {
      Conststr16MeExpr *constr16Meexpr = static_cast<Conststr16MeExpr *>(meexpr);
      LfoConststr16Node *constr16Node = codeMP->New<LfoConststr16Node>(constr16Meexpr->primType, constr16Meexpr->strIdx, parent);
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
      LfoUnaryNode *unNode = codeMP->New<LfoUnaryNode>(meexpr->op, meexpr->primType, parent);
      unNode->uOpnd = EmitLfoExpr(opMeexpr->GetOpnd(0), unNode)->Cvt2BaseNode();
      return unNode;
    }
    case OP_iaddrof: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoIaddrofNode *unNode = codeMP->New<LfoIaddrofNode>(meexpr->op, meexpr->primType, parent);
      unNode->uOpnd = EmitLfoExpr(opMeexpr->GetOpnd(0), unNode)->Cvt2BaseNode();
      unNode->tyIdx = opMeexpr->tyIdx;
      unNode->fieldID = opMeexpr->fieldID;
      return unNode;
    }
    case OP_select: {
      OpMeExpr *opMeexpr = static_cast<OpMeExpr *>(meexpr);
      LfoTernaryNode *tNode = codeMP->New<LfoTernaryNode>(OP_select, meexpr->primType, parent);
      tNode->topnd[0] = EmitLfoExpr(opMeexpr->GetOpnd(0), tNode)->Cvt2BaseNode();
      tNode->topnd[1] = EmitLfoExpr(opMeexpr->GetOpnd(1), tNode)->Cvt2BaseNode();
      tNode->topnd[2] = EmitLfoExpr(opMeexpr->GetOpnd(2), tNode)->Cvt2BaseNode();
      return tNode;
    }
    case OP_intrinsicop:
    case OP_intrinsicopwithtype: {
      NaryMeExpr *nMeexpr = static_cast<NaryMeExpr *>(meexpr);
      LfoIntrinsicopNode *intrnNode = codeMP->New<LfoIntrinsicopNode>(codeMPAlloc, meexpr->op, meexpr->primType, nMeexpr->tyIdx, parent);
      intrnNode->intrinsic = nMeexpr->intrinsic;
      for (uint32 i = 0; i < nMeexpr->numOpnds; i++) {
        LfoParentPart *opnd = EmitLfoExpr(nMeexpr->GetOpnd(i), intrnNode);
        intrnNode->nOpnd.push_back(opnd->Cvt2BaseNode());
      }
      intrnNode->numOpnds = nMeexpr->numOpnds;
      return intrnNode;
    }
    default:
      CHECK_FATAL(false, "NYI");
  }
}

StmtNode* LfoPreEmitter::EmitLfoStmt(MeStmt *mestmt, LfoParentPart *parent) {
  switch (mestmt->op) {
    case OP_dassign: {
      DassignMeStmt *dsmestmt = static_cast<DassignMeStmt *>(mestmt);
      LfoDassignNode *dass = codeMP->New<LfoDassignNode>(parent, dsmestmt);
      MIRSymbol *sym = dsmestmt->lhs->ost->GetMIRSymbol();
      dass->stIdx = sym->stIdx;
      dass->fieldID = static_cast<VarMeExpr *>(dsmestmt->lhs)->ost->fieldID;
      dass->uOpnd= EmitLfoExpr(dsmestmt->rhs, dass)->Cvt2BaseNode();
      dass->srcPosition = dsmestmt->srcPos;
      return dass;
    }
    case OP_regassign: {
      AssignMeStmt *asMestmt = static_cast<AssignMeStmt *>(mestmt);
      LfoRegassignNode *lrssnode = codeMP->New<LfoRegassignNode>(parent, asMestmt);
      lrssnode->primType = asMestmt->lhs->primType;
      lrssnode->regIdx = asMestmt->GetRegLhs()->regIdx;
      lrssnode->uOpnd= EmitLfoExpr(asMestmt->rhs, lrssnode)->Cvt2BaseNode();
      lrssnode->srcPosition = asMestmt->srcPos;
      return lrssnode;
    }
    case OP_iassign: {
      IassignMeStmt *iass = static_cast<IassignMeStmt *>(mestmt);
      LfoIassignNode *lnoIassign =  codeMP->New<LfoIassignNode>(parent, iass);
      lnoIassign->tyIdx = iass->tyIdx;
      lnoIassign->fieldID = iass->lhsVar->fieldID;
      lnoIassign->addrExpr = EmitLfoExpr(iass->lhsVar->base, lnoIassign)->Cvt2BaseNode();
      lnoIassign->rhs = EmitLfoExpr(iass->rhs, lnoIassign)->Cvt2BaseNode();
      lnoIassign->srcPosition = iass->srcPos;
      return lnoIassign;
    }
    case OP_return: {
      RetMeStmt *retMestmt = static_cast<RetMeStmt *>(mestmt);
      LfoReturnStmtNode *lnoRet = codeMP->New<LfoReturnStmtNode>(codeMPAlloc, parent, retMestmt);
      for (uint32 i = 0; i < retMestmt->opnds.size(); i++) {
        lnoRet->nOpnd.push_back(EmitLfoExpr(retMestmt->opnds[i], lnoRet)->Cvt2BaseNode());
      }
      lnoRet->numOpnds = retMestmt->opnds.size();
      lnoRet->srcPosition = retMestmt->srcPos;
      return lnoRet;
    }
    case OP_goto: {
      GotoMeStmt *gotoStmt = static_cast<GotoMeStmt *>(mestmt);
      if (lfoFunc->LabelCreatedByLfo(gotoStmt->offset)) {
        return NULL;
      }
      LfoGotoNode *gto = codeMP->New<LfoGotoNode>(OP_goto, parent);
      gto->offset = gotoStmt->offset;
      gto->srcPosition = gotoStmt->srcPos;
      return gto;
    }
    case OP_igoto: {
      UnaryMeStmt *igotoMeStmt = static_cast<UnaryMeStmt *>(mestmt);
      LfoUnaryStmtNode *igto = codeMP->New<LfoUnaryStmtNode>(OP_igoto, parent);
      igto->uOpnd = EmitLfoExpr(igotoMeStmt->opnd, igto)->Cvt2BaseNode();
      igto->srcPosition = igotoMeStmt->srcPos;
      return igto;
    }
    case OP_comment: {
      CommentMeStmt *cmtmeNode = static_cast<CommentMeStmt *>(mestmt);
      CommentNode *cmtNode = codeMP->New<CommentNode>(codeMPAlloc);
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
      CallMeStmt *callMeStmt = static_cast<CallMeStmt *>(mestmt);
      LfoCallNode *callnode = codeMP->New<LfoCallNode>(codeMPAlloc, mestmt->op, parent, callMeStmt);
      callnode->puIdx = callMeStmt->puIdx;
      callnode->tyIdx = callMeStmt->tyIdx;
      callnode->numOpnds = callMeStmt->opnds.size();
      callnode->srcPosition = callMeStmt->srcPos;
      mestmt->EmitCallReturnVector(meirmap->mirFunc->meSSATab, &callnode->returnValues);
      for (uint32 i = 0; i < callMeStmt->opnds.size(); i++) {
        callnode->nOpnd.push_back(EmitLfoExpr(callMeStmt->opnds[i], callnode)->Cvt2BaseNode());
      }
      return callnode;
    }
    case OP_icall:
    case OP_icallassigned: {
      IcallMeStmt *icallMeStmt = static_cast<IcallMeStmt *> (mestmt);
      LfoIcallNode *icallnode = codeMP->New<LfoIcallNode>(codeMPAlloc, OP_icallassigned, icallMeStmt->retTyIdx, parent, icallMeStmt);
      for (uint32 i = 0; i < icallMeStmt->opnds.size(); i++) {
        icallnode->nOpnd.push_back(EmitLfoExpr(icallMeStmt->opnds[i], icallnode)->Cvt2BaseNode());
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
      IntrinsiccallMeStmt *callMeStmt = static_cast<IntrinsiccallMeStmt *> (mestmt);
      LfoIntrinsiccallNode *callnode = codeMP->New<LfoIntrinsiccallNode>(codeMPAlloc,
                mestmt->op, callMeStmt->intrinsic, parent, callMeStmt);
      callnode->intrinsic = callMeStmt->intrinsic;
      callnode->tyIdx = callMeStmt->tyIdx;
      for (uint32 i = 0; i < callMeStmt->opnds.size(); i++) {
        callnode->nOpnd.push_back(EmitLfoExpr(callMeStmt->opnds[i], callnode)->Cvt2BaseNode());
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
      LfoStmtNode *lnoStmtNode = codeMP->New<LfoStmtNode>(parent, mestmt->op);
      lnoStmtNode->srcPosition = mestmt->srcPos;
      return lnoStmtNode;
    }
    case OP_retsub: {
      LfoStmtNode * usesStmtNode = codeMP->New<LfoStmtNode>(parent, mestmt->op);
      usesStmtNode->srcPosition = mestmt->srcPos;
      return usesStmtNode;
    }
    case OP_brfalse:
    case OP_brtrue: {
      LfoCondGotoNode *lnoCondNode = codeMP->New<LfoCondGotoNode>(mestmt->op, parent);
      CondGotoMeStmt *condMeStmt = static_cast<CondGotoMeStmt *> (mestmt);
      lnoCondNode->offset = condMeStmt->offset;
      lnoCondNode->srcPosition = mestmt->srcPos;
      lnoCondNode->uOpnd = EmitLfoExpr(condMeStmt->opnd, lnoCondNode)->Cvt2BaseNode();
      return lnoCondNode;
    }
    case OP_try:
    case OP_cpptry:
    case OP_javatry: {
      TryNode *jvTryNode = codeMP->New<TryNode>(codeMPAlloc, mestmt->op);
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
      CppCatchNode *cppCatchNode = codeMP->New<CppCatchNode>();
      CppCatchMeStmt *catchMestmt = static_cast<CppCatchMeStmt *> (mestmt);
      cppCatchNode->exceptionTyIdx = catchMestmt->exceptionTyIdx;
      cppCatchNode->srcPosition = catchMestmt->srcPos;
      return cppCatchNode;
    }
    case OP_catch:
    case OP_javacatch: {
      CatchNode *jvCatchNode = codeMP->New<CatchNode>(codeMPAlloc);
      JavaCatchMeStmt *catchMestmt = static_cast<JavaCatchMeStmt *> (mestmt);
      jvCatchNode->exceptionTyIdxVec = catchMestmt->exceptionTyIdxVec;
      jvCatchNode->srcPosition = catchMestmt->srcPos;
      return jvCatchNode;
    }
    case OP_throw: {
      LfoUnaryStmtNode *throwStmt = codeMP->New<LfoUnaryStmtNode>(mestmt->op, parent);
      ThrowMeStmt *throwMeStmt = static_cast<ThrowMeStmt *>(mestmt);
      throwStmt->uOpnd = EmitLfoExpr(throwMeStmt->opnd, throwStmt)->Cvt2BaseNode();
      throwStmt->srcPosition = throwMeStmt->srcPos;
      return throwStmt;
    }
    case OP_assertnonnull:
    case OP_eval:
    case OP_free: {
      LfoUnaryStmtNode *unaryStmt = codeMP->New<LfoUnaryStmtNode>(mestmt->op, parent);
      UnaryMeStmt *uMeStmt = static_cast<UnaryMeStmt *>(mestmt);
      unaryStmt->uOpnd = EmitLfoExpr(uMeStmt->opnd, unaryStmt)->Cvt2BaseNode();
      unaryStmt->srcPosition = uMeStmt->srcPos;
      return unaryStmt;
    }
    case OP_switch: {
      LfoSwitchNode *switchNode = codeMP->New<LfoSwitchNode>(codeMPAlloc, parent);
      SwitchMeStmt *meSwitch = static_cast<SwitchMeStmt *>(mestmt);
      switchNode->switchOpnd = EmitLfoExpr(meSwitch->opnd, switchNode)->Cvt2BaseNode();
      switchNode->defaultLabel = meSwitch->defaultLabel;
      switchNode->switchTable = meSwitch->switchTable;
      switchNode->srcPosition = meSwitch->srcPos;
      return switchNode;
    }
    default:
      CHECK_FATAL(false, "nyi");
  }
}

void LfoPreEmitter::EmitBB(BB *bb, LfoBlockNode *curblk) {
  CHECK_FATAL(curblk != nullptr, "null ptr check");
  // emit head. label
  LabelIdx labidx = bb->bbLabel;
  if (labidx != 0 && !lfoFunc->LabelCreatedByLfo(labidx)) {
    // not a empty bb
    LabelNode *lbnode = codeMP->New<LabelNode>();
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
    StmtNode *endtry = codeMP->New<StmtNode>(OP_endtry);
    curblk->AddStatement(endtry);
  }
}

DoloopNode *LfoPreEmitter::EmitLfoDoloop(BB *mewhilebb, LfoBlockNode *curblk, LfoWhileInfo *whileInfo) {
  LabelIdx labidx = mewhilebb->bbLabel;
  MeStmt *lastmestmt = mewhilebb->meStmtList.last;
  CHECK_FATAL(lastmestmt->prev == nullptr || dynamic_cast<AssignMeStmt *>(lastmestmt->prev) == nullptr,
              "EmitLfoWhile: there are other statements at while header bb");
  LfoDoloopNode *lnoDoloopnode = codeMP->New<LfoDoloopNode>(curblk);
  lnoDoloopnode->doVarStIdx = whileInfo->ivOst->GetMIRSymbol()->stIdx;
  CondGotoMeStmt *condGotostmt = static_cast<CondGotoMeStmt *>(lastmestmt);
  lnoDoloopnode->startExpr =  EmitLfoExpr(whileInfo->initExpr, lnoDoloopnode)->Cvt2BaseNode();
  lnoDoloopnode->condExpr = EmitLfoExpr(condGotostmt->opnd, lnoDoloopnode)->Cvt2BaseNode();
  lnoDoloopnode->doBody = codeMP->New<LfoBlockNode>(lnoDoloopnode);
  MIRIntConst *intConst = mirFunc->dataMemPool->New<MIRIntConst>(whileInfo->stepValue, whileInfo->ivOst->GetType());
  LfoConstvalNode *lfoconstnode = codeMP->New<LfoConstvalNode>(intConst, lnoDoloopnode);
  lnoDoloopnode->incrExpr = lfoconstnode;
  lnoDoloopnode->isPreg = false;
  curblk->AddStatement(lnoDoloopnode);
  return lnoDoloopnode;
}

WhileStmtNode *LfoPreEmitter::EmitLfoWhile(BB *meWhilebb, LfoBlockNode *curblk) {
  LabelIdx labidx = meWhilebb->bbLabel;
  MeStmt *lastmestmt = meWhilebb->meStmtList.last;
  CHECK_FATAL(lastmestmt->prev == nullptr || dynamic_cast<AssignMeStmt *>(lastmestmt->prev) == nullptr,
              "EmitLfoWhile: there are other statements at while header bb");
  LfoWhileStmtNode *lnoWhilestmt = codeMP->New<LfoWhileStmtNode>(curblk);
  CondGotoMeStmt *condGotostmt = static_cast<CondGotoMeStmt *>(lastmestmt);
  lnoWhilestmt->uOpnd = EmitLfoExpr(condGotostmt->opnd, lnoWhilestmt)->Cvt2BaseNode();
  lnoWhilestmt->body = codeMP->New<LfoBlockNode>(lnoWhilestmt);
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
  LfoBlockNode *lfoDobody = nullptr;
  if (whileInfo->canConvertDoloop) {  // emit doloop
    DoloopNode *doloopnode = EmitLfoDoloop(curbb, curblk, whileInfo);
    ++curj;
    lfoDobody = static_cast<LfoBlockNode *>(doloopnode->doBody);
  } else { // emit while loop
    WhileStmtNode *whileNode = EmitLfoWhile(curbb, curblk);
    ++curj;
    lfoDobody = static_cast<LfoBlockNode *> (whileNode->body);
  }
  // emit loop body
  while (bbvec[curj]->id != endlblbb->id) {
    curj = EmitLfoBB(curj, lfoDobody);
    while (bbvec[curj] == nullptr) {
      curj++;
    }
  }
  if (whileInfo->canConvertDoloop) {  // delete the increment statement
    StmtNode *bodylaststmt = lfoDobody->GetLast();
    CHECK_FATAL(bodylaststmt->op == OP_dassign, "Raise2LfoWhile: cannot find increment stmt");
    DassignNode *dassnode = static_cast<DassignNode *>(bodylaststmt);
    CHECK_FATAL(dassnode->stIdx == whileInfo->ivOst->GetMIRSymbol()->stIdx,
                "Raise2LfoWhile: cannot find IV increment");
    lfoDobody->RemoveStmt(dassnode);
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
  LfoParentPart *condnode = EmitLfoExpr(condgoto->opnd, lnoIfstmtNode);
  lnoIfstmtNode->uOpnd = condnode->Cvt2BaseNode();
  curblk->AddStatement(lnoIfstmtNode);
  if (ifInfo->elseLabel != 0) {  // both else and then are not empty;
    LfoBlockNode *elseBlk = codeMP->New<LfoBlockNode>(lnoIfstmtNode);
    LfoBlockNode *thenBlk = codeMP->New<LfoBlockNode>(lnoIfstmtNode);
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
    LfoBlockNode *branchBlock = codeMP->New<LfoBlockNode>(lnoIfstmtNode);
    LfoBlockNode *emptyBlock = codeMP->New<LfoBlockNode>(lnoIfstmtNode);
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
  LfoBlockNode *curblk = func->mirFunc->codeMemPool->New<LfoBlockNode>(nullptr);
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
