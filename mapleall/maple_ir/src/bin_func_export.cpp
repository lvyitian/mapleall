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


#include "mir_function.h"
#include "name_mangler.h"
#include "opcode_info.h"
#include "mir_pragma.h"
#include "mir_builder.h"
#include "bin_mplt.h"

#include <sstream>
#include <vector>

using namespace std;
namespace maple {

void BinaryMplExport::OutputInfoVector(MIRInfoVector &infovector, MapleVector<bool> &infovector_isstring) {
  WriteNum(infovector.size());
  for (uint32 i = 0; i < infovector.size(); i++) {
    OutputStr(infovector[i].first);
    WriteNum(infovector_isstring[i]);
    if (!infovector_isstring[i]) {
      WriteNum(infovector[i].second);
    } else {
      OutputStr(GStrIdx(infovector[i].second));
    }
  }
}

void BinaryMplExport::OutputFuncIdInfo(MIRFunction *func) {
  WriteNum(kBinFuncIdInfoStart);
  WriteNum(func->puIdxOrigin);  // the funcid
  OutputInfoVector(func->info, func->infoIsString);
  WriteNum(~kBinFuncIdInfoStart);
}

void BinaryMplExport::OutputBaseNode(const BaseNode *b) {
  WriteNum(b->op);
  WriteNum(b->primType);
  WriteNum(b->numOpnds);
}

void BinaryMplExport::OutputLocalSymbol(const MIRSymbol *sym) {
  if (sym == nullptr) {
    WriteNum(0);
    return;
  }
  WriteNum(kBinSymbol);
  WriteNum(sym->GetStIndex());  // preserve original st index
  OutputStr(sym->nameStrIdx);
  WriteNum(sym->sKind);
  WriteNum(sym->storageClass);
  OutputTypeAttrs(sym->typeAttrs);
  WriteNum(sym->isTmp);
  if (sym->sKind == kStVar || sym->sKind == kStFunc) {
    OutputSrcPos(sym->srcPosition);
  }
  OutputTypeViaTypeName(sym->tyIdx);
  if (sym->sKind == kStPreg) {
    WriteNum(sym->value.preg->pregNo);
  } else if (sym->sKind == kStConst || sym->sKind == kStVar) {
    OutputConst(sym->value.konst);
  } else if (sym->sKind == kStFunc) {
    OutputFuncViaSymName(sym->value.mirFunc->puIdx);
    OutputTypeViaTypeName(sym->tyIdx);
  } else {
    CHECK_FATAL(false, "should not used");
  }
}

void BinaryMplExport::OutputLocalSymTab(const MIRFunction *func) {
  WriteNum(kBinSymStart);
  uint64 outsymSizeIdx = buf.size();
  ExpandFourBuffSize();  /// size of OutSym
  int32 size = 0;

  for (uint32 i = 1; i < func->symTab->GetSymbolTableSize(); i++) {
    MIRSymbol *s = func->symTab->GetSymbolFromStIdx(i);
    if (s->IsDeleted()) {
      s = nullptr;
    }
    OutputLocalSymbol(s);
    size++;
  }

  Fixup(outsymSizeIdx, size);
  WriteNum(~kBinSymStart);
}

void BinaryMplExport::OutputPregTab(const MIRFunction *func) {
  WriteNum(kBinPregStart);
  uint64 outRegSizeIdx = buf.size();
  ExpandFourBuffSize();  /// size of OutReg
  int32 size = 0;

  for (uint32 i = 1; i < func->pregTab->Size(); i++) {
    MIRPreg *mirpreg = func->pregTab->pregTable[i];
    if (mirpreg == nullptr) {
      WriteNum(0);
      continue;
    }
    WriteNum(kBinPreg);
    WriteNum(mirpreg->pregNo);
    TyIdx tyIdx = (mirpreg->mirType == nullptr) ? TyIdx(0) : mirpreg->mirType->tyIdx;
    OutputTypeViaTypeName(tyIdx);
    WriteNum(mirpreg->primType);
    size++;
  }

  Fixup(outRegSizeIdx, size);
  WriteNum(~kBinPregStart);
}

void BinaryMplExport::OutputLabelTab(const MIRFunction *func) {
  WriteNum(kBinLabelStart);
  WriteNum(func->labelTab->Size()-1);  // entry 0 is skipped
  for (uint32 i = 1; i < func->labelTab->Size(); i++) {
    OutputStr(func->labelTab->labelTable[i]);
  }
  WriteNum(~kBinLabelStart);
}

void BinaryMplExport::OutputLocalTypeNameTab(const MIRTypeNameTable *tnametab) {
  WriteNum(kBinTypenameStart);
  WriteNum(tnametab->gStrIdxToTyIdxMap.size());
  for (std::pair<GStrIdx, TyIdx> it : tnametab->gStrIdxToTyIdxMap) {
    OutputStr(it.first);
    OutputTypeViaTypeName(it.second);
  }
  WriteNum(~kBinTypenameStart);
}

void BinaryMplExport::OutputFormalsStIdx(const MIRFunction *func) {
  WriteNum(kBinFormalStart);
  WriteNum(func->formalDefVec.size());
  for (FormalDef formalDef : func->formalDefVec) {
    WriteNum(formalDef.formalSym->GetStIndex());
  }
  WriteNum(~kBinFormalStart);
}

void BinaryMplExport::OutputAliasMap(MapleMap<GStrIdx, MIRAliasVars> &aliasVarMap) {
  WriteNum(kBinAliasMapStart);
  WriteInt(aliasVarMap.size());
  for (std::pair<GStrIdx, MIRAliasVars> it : aliasVarMap) {
    OutputStr(it.first);
    OutputStr(it.second.memPoolStrIdx);
    OutputTypeViaTypeName(it.second.tyIdx);
    OutputStr(it.second.sigStrIdx);
  }
  WriteNum(~kBinAliasMapStart);
}

void BinaryMplExport::OutputFuncViaSymName(PUIdx puIdx) {
  MIRFunction *func = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  WriteNum(kBinKindFuncViaSymname);
  OutputStr(funcSt->nameStrIdx);
}

void BinaryMplExport::OutputExpression(BaseNode *e) {
  WriteNum(kBinOpExpression);
  OutputBaseNode(e);
  switch (e->op) {
    // leaf
    case OP_constval: {
      MIRConst *constVal = static_cast<ConstvalNode *>(e)->constVal;
      OutputConst(constVal);
      return;
    }
    case OP_conststr: {
      UStrIdx strIdx = static_cast<ConststrNode *>(e)->strIdx;
      OutputUsrStr(strIdx);
      return;
    }
    case OP_addroflabel: {
      AddroflabelNode *lNode = static_cast<AddroflabelNode *>(e);
      WriteNum(lNode->offset);
      return;
    }
    case OP_addroffunc: {
      AddroffuncNode *addrNode = static_cast<AddroffuncNode *>(e);
      OutputFuncViaSymName(addrNode->puIdx);
      return;
    }
    case OP_sizeoftype: {
      SizeoftypeNode *sot = static_cast<SizeoftypeNode *>(e);
      OutputTypeViaTypeName(sot->tyIdx);
      return;
    }
    case OP_addrof:
    case OP_dread: {
      AddrofNode *drNode = static_cast<AddrofNode *>(e);
      WriteNum(drNode->fieldID);
      WriteNum(drNode->stIdx.Scope());
      if (drNode->stIdx.Islocal()) {
        WriteNum(drNode->stIdx.Idx());  // preserve original st index
      } else {
        MIRSymbol *sym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(drNode->stIdx.Idx());
        WriteNum(kBinKindSymViaSymname);
        OutputStr(sym->nameStrIdx);
      }
      return;
    }
    case OP_regread: {
      RegreadNode *regreadNode = static_cast<RegreadNode *>(e);
      WriteNum(regreadNode->regIdx);
      return;
    }
    case OP_gcmalloc:
    case OP_gcpermalloc:
    case OP_stackmalloc: {
      GCMallocNode *gcNode = static_cast<GCMallocNode *>(e);
      OutputTypeViaTypeName(gcNode->tyIdx);
      return;
    }
    // unary
    case OP_ceil:
    case OP_cvt:
    case OP_floor:
    case OP_trunc: {
      TypeCvtNode *typecvtNode = static_cast<TypeCvtNode *>(e);
      WriteNum(typecvtNode->fromPrimType);
      break;
    }
    case OP_retype: {
      RetypeNode *retypeNode = static_cast<RetypeNode *>(e);
      OutputTypeViaTypeName(retypeNode->tyIdx);
      break;
    }
    case OP_iread:
    case OP_iaddrof: {
      IreadNode *irNode = static_cast<IreadNode *>(e);
      OutputTypeViaTypeName(irNode->tyIdx);
      WriteNum(irNode->fieldID);
      break;
    }
    case OP_sext:
    case OP_zext:
    case OP_extractbits: {
      ExtractbitsNode *extNode = static_cast<ExtractbitsNode *>(e);
      WriteNum(extNode->bitsOffset);
      WriteNum(extNode->bitsSize);
      break;
    }
    case OP_gcmallocjarray:
    case OP_gcpermallocjarray: {
      JarrayMallocNode *gcNode = static_cast<JarrayMallocNode *>(e);
      OutputTypeViaTypeName(gcNode->tyIdx);
      break;
    }
    // binary
    case OP_sub:
    case OP_mul:
    case OP_div:
    case OP_rem:
    case OP_ashr:
    case OP_lshr:
    case OP_shl:
    case OP_max:
    case OP_min:
    case OP_band:
    case OP_bior:
    case OP_bxor:
    case OP_cand:
    case OP_cior:
    case OP_land:
    case OP_lior:
    case OP_add: {
      break;
    }
    case OP_eq:
    case OP_ne:
    case OP_lt:
    case OP_gt:
    case OP_le:
    case OP_ge:
    case OP_cmpg:
    case OP_cmpl:
    case OP_cmp: {
      CompareNode *cmpNode = static_cast<CompareNode *>(e);
      WriteNum(cmpNode->opndType);
      break;
    }
    case OP_resolveinterfacefunc:
    case OP_resolvevirtualfunc: {
      ResolveFuncNode *rsNode = static_cast<ResolveFuncNode *>(e);
      OutputFuncViaSymName(rsNode->puIdx);
      break;
    }
    // ternary
    case OP_select: {
      break;
    }
    // nary
    case OP_array: {
      ArrayNode *arrNode = static_cast<ArrayNode *>(e);
      OutputTypeViaTypeName(arrNode->tyIdx);
      WriteNum(arrNode->boundsCheck);
      WriteNum(arrNode->NumOpnds());
      break;
    }
    case OP_intrinsicop: {
      IntrinsicopNode *intrnNode = static_cast<IntrinsicopNode *>(e);
      WriteNum(intrnNode->intrinsic);
      WriteNum(intrnNode->NumOpnds());
      break;
    }
    case OP_intrinsicopwithtype: {
      IntrinsicopNode *intrnNode = static_cast<IntrinsicopNode *>(e);
      WriteNum(intrnNode->intrinsic);
      OutputTypeViaTypeName(intrnNode->tyIdx);
      WriteNum(intrnNode->NumOpnds());
      break;
    }
    default:
      break;
  }
  for (int32 i = 0; i < e->NumOpnds(); i++) {
    OutputExpression(e->Opnd(i));
  }
}

static SrcPosition lastOutputSrcPosition;

void BinaryMplExport::OutputSrcPos(const SrcPosition &pos) {
  if (pos.Filenum() == 0 || pos.Linenum() == 0) {  // error case, so output 0
    WriteNum(lastOutputSrcPosition.RawData());
    WriteNum(lastOutputSrcPosition.Linenum());
    return;
  }
  WriteNum(pos.RawData());
  WriteNum(pos.Linenum());
  lastOutputSrcPosition = pos;
}

void BinaryMplExport::OutputReturnValues(const CallReturnVector *retv) {
  WriteNum(kBinReturnvals);
  WriteNum(retv->size());
  for (uint32 i = 0; i < retv->size(); i++) {
    WriteNum((*retv)[i].first.Idx());
    WriteNum((*retv)[i].second.fieldID);
    WriteNum((*retv)[i].second.pregIdx);
  }
}

void BinaryMplExport::OutputBlockNode(BlockNode *block) {
  WriteNum(kBinNodeBlock);
  if (!block->GetStmtNodes().empty()) {
    OutputSrcPos(block->srcPosition);
  } else {
    OutputSrcPos(SrcPosition());  // output 0
  }
  int32 num = 0;
  uint64 idx = buf.size();
  ExpandFourBuffSize();  // place holder, Fixup later
  for (StmtNode *s = block->GetFirst(); s; s = s->GetNext()) {
    bool doneWithOpnds = false;
    WriteNum(kBinOpStatement);
    OutputSrcPos(s->srcPosition);
    WriteNum(s->op);
    switch (s->op) {
      case OP_dassign: {
        DassignNode *dass = static_cast<DassignNode *>(s);
        WriteNum(dass->fieldID);
        WriteNum(dass->stIdx.Scope());
        if (dass->stIdx.Islocal()) {
          WriteNum(dass->stIdx.Idx());  // preserve original st index
        } else {
          MIRSymbol *sym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(dass->stIdx.Idx());
          WriteNum(kBinKindSymViaSymname);
          OutputStr(sym->nameStrIdx);
        }
        break;
      }
      case OP_regassign: {
        RegassignNode *rass = static_cast<RegassignNode *>(s);
        WriteNum(rass->primType);
        WriteNum(rass->regIdx);
        break;
      }
      case OP_iassign: {
        IassignNode *iass = static_cast<IassignNode *>(s);
        OutputTypeViaTypeName(iass->tyIdx);
        WriteNum(iass->fieldID);
        break;
      }
      case OP_call:
      case OP_virtualcall:
      case OP_virtualicall:
      case OP_superclasscall:
      case OP_interfacecall:
      case OP_interfaceicall:
      case OP_customcall:
      case OP_polymorphiccall: {
        CallNode *callnode = static_cast<CallNode *>(s);
        OutputFuncViaSymName(callnode->puIdx);
        if (s->op == OP_polymorphiccall) {
          OutputTypeViaTypeName(static_cast<CallNode *>(callnode)->tyIdx);
        }
        WriteNum(s->numOpnds);
        break;
      }
      case OP_callassigned:
      case OP_virtualcallassigned:
      case OP_virtualicallassigned:
      case OP_superclasscallassigned:
      case OP_interfacecallassigned:
      case OP_interfaceicallassigned:
      case OP_customcallassigned: {
        CallNode *callnode = static_cast<CallNode *>(s);
        OutputFuncViaSymName(callnode->puIdx);
        OutputReturnValues(&callnode->returnValues);
        WriteNum(s->numOpnds);
        break;
      }
      case OP_polymorphiccallassigned: {
        CallNode *callnode = static_cast<CallNode *>(s);
        OutputFuncViaSymName(callnode->puIdx);
        OutputTypeViaTypeName(callnode->tyIdx);
        OutputReturnValues(&callnode->returnValues);
        WriteNum(s->numOpnds);
        break;
      }
      case OP_icall: {
        IcallNode *icallnode = static_cast<IcallNode *>(s);
        OutputTypeViaTypeName(icallnode->retTyIdx);
        WriteNum(s->numOpnds);
        break;
      }
      case OP_icallassigned: {
        IcallNode *icallnode = static_cast<IcallNode *>(s);
        OutputTypeViaTypeName(icallnode->retTyIdx);
        OutputReturnValues(&icallnode->returnValues);
        WriteNum(s->numOpnds);
        break;
      }
      case OP_intrinsiccall:
      case OP_xintrinsiccall: {
        IntrinsiccallNode *intrnNode = static_cast<IntrinsiccallNode *>(s);
        WriteNum(intrnNode->intrinsic);
        WriteNum(s->numOpnds);
        break;
      }
      case OP_intrinsiccallassigned:
      case OP_xintrinsiccallassigned: {
        IntrinsiccallNode *intrnNode = static_cast<IntrinsiccallNode *>(s);
        WriteNum(intrnNode->intrinsic);
        OutputReturnValues(&intrnNode->returnValues);
        WriteNum(s->numOpnds);
        break;
      }
      case OP_intrinsiccallwithtype: {
        IntrinsiccallNode *intrnNode = static_cast<IntrinsiccallNode *>(s);
        WriteNum(intrnNode->intrinsic);
        OutputTypeViaTypeName(intrnNode->tyIdx);
        WriteNum(s->numOpnds);
        break;
      }
      case OP_intrinsiccallwithtypeassigned: {
        IntrinsiccallNode *intrnNode = static_cast<IntrinsiccallNode *>(s);
        WriteNum(intrnNode->intrinsic);
        OutputTypeViaTypeName(intrnNode->tyIdx);
        OutputReturnValues(&intrnNode->returnValues);
        WriteNum(s->numOpnds);
        break;
      }
      case OP_syncenter:
      case OP_syncexit:
      case OP_return: {
        WriteNum(s->numOpnds);
        break;
      }
      case OP_jscatch:
      case OP_cppcatch:
      case OP_finally:
      case OP_endtry:
      case OP_cleanuptry:
      case OP_retsub:
      case OP_membaracquire:
      case OP_membarrelease:
      case OP_membarstorestore:
      case OP_membarstoreload: {
        break;
      }
      case OP_eval:
      case OP_throw:
      case OP_free:
      case OP_decref:
      case OP_incref:
      case OP_decrefreset:
      case OP_assertnonnull:
      case OP_igoto: {
        break;
      }
      case OP_label: {
        LabelNode *lNode = static_cast<LabelNode *>(s);
        WriteNum(lNode->labelIdx);
        break;
      }
      case OP_goto:
      case OP_gosub: {
        GotoNode *gtoNode = static_cast<GotoNode *>(s);
        WriteNum(gtoNode->offset);
        break;
      }
      case OP_brfalse:
      case OP_brtrue: {
        CondGotoNode *cgotoNode = static_cast<CondGotoNode *>(s);
        WriteNum(cgotoNode->offset);
        break;
      }
      case OP_switch: {
        SwitchNode *swNode = static_cast<SwitchNode *>(s);
        WriteNum(swNode->defaultLabel);
        WriteNum(swNode->switchTable.size());
        for (CasePair cpair : swNode->switchTable) {
          WriteNum(cpair.first);
          WriteNum(cpair.second);
        }
        break;
      }
      case OP_jstry: {
        JsTryNode *tryNode = static_cast<JsTryNode *>(s);
        WriteNum(tryNode->catchOffset);
        WriteNum(tryNode->finallyOffset);
        break;
      }
      case OP_cpptry:
      case OP_try:
      case OP_javatry: {
        TryNode *tryNode = static_cast<TryNode *>(s);
        WriteNum(tryNode->offsets.size());
        for (LabelIdx lidx : tryNode->offsets) {
          WriteNum(lidx);
        }
        break;
      }
      case OP_catch:
      case OP_javacatch: {
        CatchNode *catchNode = static_cast<CatchNode *>(s);
        WriteNum(catchNode->exceptionTyIdxVec.size());
        for (TyIdx tidx : catchNode->exceptionTyIdxVec) {
          OutputTypeViaTypeName(tidx);
        }
        break;
      }
      case OP_comment: {
        string str(static_cast<CommentNode *>(s)->comment.c_str());
        WriteAsciiStr(str);
        break;
      }
      case OP_dowhile:
      case OP_while: {
        WhileStmtNode *whileNode = static_cast<WhileStmtNode *>(s);
        OutputBlockNode(whileNode->body);
        OutputExpression(whileNode->uOpnd);
        doneWithOpnds = true;
        break;
      }
      case OP_if: {
        IfStmtNode *ifNode = static_cast<IfStmtNode *>(s);
        bool hasElsePart = ifNode->elsePart != nullptr;
        WriteNum(hasElsePart);
        OutputBlockNode(ifNode->thenPart);
        if (hasElsePart) {
          OutputBlockNode(ifNode->elsePart);
        }
        OutputExpression(ifNode->uOpnd);
        doneWithOpnds = true;
        break;
      }
      case OP_block: {
        BlockNode *blockNode = static_cast<BlockNode *>(s);
        OutputBlockNode(blockNode);
        doneWithOpnds = true;
        break;
      }
      default:
        CHECK_FATAL(false, "Unhandled opcode %d", s->op);
        break;
    }
    num++;
    if (!doneWithOpnds) {
      for (int32 i = 0; i < s->numOpnds; i++) {
        OutputExpression(s->Opnd(i));
      }
    }
  }
  Fixup(idx, num);
}

void BinaryMplExport::WriteFunctionBodyField(uint64 contentIdx, std::unordered_set<std::string> *dumpFuncSet) {
  Fixup(contentIdx, buf.size());
  // LogInfo::MapleLogger() << "Write FunctionBody Field " << std::endl;
  WriteNum(kBinFunctionBodyStart);
  uint64 totalSizeIdx = buf.size();
  ExpandFourBuffSize();  /// total size of this field to ~BIN_FUNCTIONBODY_START
  uint64 outFunctionBodySizeIdx = buf.size();
  ExpandFourBuffSize();  /// size of outFunctionBody
  int32 size = 0;

  if (not2mplt) {
    for (MIRFunction *func : GetMIRModule().functionList) {
      if (func->GetAttr(FUNCATTR_optimized)) {
        continue;
      }
      if (func->codeMemPool == nullptr || func->body == nullptr) {
        continue;
      }
      if (dumpFuncSet != nullptr && !dumpFuncSet->empty()) {
        // output only if this func matches any name in *dumpFuncSet
        const std::string &name = func->GetName();
        bool matched = false;
        for (std::string elem : *dumpFuncSet) {
          if (name.find(elem.c_str()) != string::npos) {
            matched = true;
            break;
          }
        }
        if (!matched) {
          continue;
        }
      }
      OutputFunction(func->puIdx);
      CHECK_FATAL(func->body != nullptr, "WriteFunctionBodyField: no function body");
      OutputFuncIdInfo(func);
      OutputLocalSymTab(func);
      OutputPregTab(func);
      OutputLabelTab(func);
      OutputLocalTypeNameTab(func->typeNameTab);
      OutputFormalsStIdx(func);
      OutputAliasMap(func->aliasVarMap);
      lastOutputSrcPosition = SrcPosition();
      OutputBlockNode(func->body);
      size++;
    }
  }

  Fixup(totalSizeIdx, buf.size() - totalSizeIdx);
  Fixup(outFunctionBodySizeIdx, size);
  WriteNum(~kBinFunctionBodyStart);
  return;
}

}  // namespace maple
