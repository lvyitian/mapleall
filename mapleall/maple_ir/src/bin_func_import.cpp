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

#include "bin_mpl_export.h"
#include "bin_mpl_import.h"
#include "mir_function.h"
#include "name_mangler.h"
#include "opcode_info.h"
#include "mir_pragma.h"
#include "debug_info.h"
#include "mir_builder.h"

#include <sstream>
#include <vector>
#include <unordered_set>
using namespace std;

namespace maple {

void BinaryMplImport::ImportInfoVector(MIRInfoVector &infovector, MapleVector<bool> &infovector_isstring) {
  int64 size = ReadNum();
  for (int64 i = 0; i < size; i++) {
    GStrIdx gStrIdx = ImportStr();
    bool isstring = ReadNum();
    infovector_isstring.push_back(isstring);
    if (isstring) {
      GStrIdx fieldval = ImportStr();
      infovector.push_back(MIRInfoPair(gStrIdx, fieldval.GetIdx()));
    } else {
      uint32 fieldval = ReadNum();
      infovector.push_back(MIRInfoPair(gStrIdx, fieldval));
    }
  }
}

void BinaryMplImport::ImportFuncIdInfo(MIRFunction *func) {
  int64 tag = ReadNum();
  CHECK_FATAL(tag == kBinFuncIdInfoStart, "kBinFuncIdInfoStart expected");
  func->puIdxOrigin = ReadNum();
  ImportInfoVector(func->info, func->infoIsString);
  tag = ReadNum();
  CHECK_FATAL(tag == ~kBinFuncIdInfoStart, "pattern mismatch in ImportFuncIdInfo()");
}

void BinaryMplImport::ImportBaseNode(Opcode &o, PrimType &typ, uint8 &numopr) {
  o = (Opcode)ReadNum();
  typ = (PrimType)ReadNum();
  numopr = ReadNum();
}

void BinaryMplImport::ImportLocalSymbol(MIRFunction *func) {
  int64 tag = ReadNum();
  if (tag == 0) {
    func->symTab->PushNullSymbol();
    return;
  }
  CHECK_FATAL(tag == kBinSymbol, "expecting kBinSymbol in ImportLocalSymbol()");
  uint32 indx = ReadNum();
  CHECK_FATAL(indx == func->symTab->GetSymbolTableSize(), "inconsistant local stIdx");
  MIRSymbol *sym = func->symTab->CreateSymbol(kScopeLocal);
  sym->nameStrIdx = ImportStr();
  func->symTab->AddToStringSymbolMap(sym);
  sym->tyIdx = ImportType();
  sym->sKind = (MIRSymKind)ReadNum();
  sym->storageClass = (MIRStorageClass)ReadNum();
  sym->typeAttrs = ImportTypeAttrs();
  sym->isTmp = ReadNum();
  if (sym->sKind == kStPreg) {
    uint32 thepregno = ReadNum();
    MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
    PregIdx pregidx = func->pregTab->EnterPregNo(thepregno, mirType->primType, mirType);
    MIRPregTable *pregTab = func->pregTab;
    MIRPreg *preg = pregTab->PregFromPregIdx(pregidx);
    preg->primType = mirType->primType;
    sym->value.preg = preg;
  } else if (sym->sKind == kStConst || sym->sKind == kStVar) {
    sym->value.konst = ImportConst(func);
  } else if (sym->sKind == kStFunc) {
    PUIdx puIdx = ImportFuncViaSymName();
    TyIdx tyIdx = ImportType();
    sym->tyIdx = tyIdx;
    sym->value.mirFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
  }
}

void BinaryMplImport::ImportLocalSymTab(MIRFunction *func) {
  int64 tag = ReadNum();
  CHECK_FATAL(tag == kBinSymStart, "kBinSymStart expected in ImportLocalSymTab()");
  int32 size = ReadInt();
  for (int64 i = 0; i < size; i++) {
    ImportLocalSymbol(func);
  }
  tag = ReadNum();
  CHECK_FATAL(tag == ~kBinSymStart, "pattern mismatch in ImportLocalSymTab()");
}

void BinaryMplImport::ImportPregTab(MIRFunction *func) {
  int64 tag = ReadNum();
  CHECK_FATAL(tag == kBinPregStart, "kBinPregStart expected in ImportPregTab()");
  int32 size = ReadInt();
  for (int64 i = 0; i < size; i++) {
    int64 tag = ReadNum();
    if (tag == 0) {
      func->pregTab->pregTable.push_back(nullptr);
      continue;
    }
    CHECK_FATAL(tag == kBinPreg, "expecting kBinPreg in ImportPregTab()");
    int32 pregNo = ReadNum();
    TyIdx tyIdx = ImportType();
    MIRType *ty = (tyIdx == 0) ? nullptr : GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
    PrimType primType = (PrimType)ReadNum();
    CHECK_FATAL(ty == nullptr || primType == ty->primType, "ImportPregTab: inconsistent primitive type");
    PregIdx pregIdx = func->pregTab->EnterPregNo(pregNo, primType, ty);
    MIRPreg *preg = func->pregTab->PregFromPregIdx(pregIdx);
  }
  tag = ReadNum();
  CHECK_FATAL(tag == ~kBinPregStart, "pattern mismatch in ImportPregTab()");
}

void BinaryMplImport::ImportLabelTab(MIRFunction *func) {
  int64 tag = ReadNum();
  CHECK_FATAL(tag == kBinLabelStart, "kBinLabelStart expected in ImportLabelTab()");
  int32 size = ReadNum();
  for (int64 i = 0; i < size; i++) {
    GStrIdx gStrIdx = ImportStr();
    func->labelTab->AddLabel(gStrIdx);
  }
  tag = ReadNum();
  CHECK_FATAL(tag == ~kBinLabelStart, "pattern mismatch in ImportLabelTab()");
}

void BinaryMplImport::ImportLocalTypeNameTable(MIRTypeNameTable *typeNameTab) {
  int64 tag = ReadNum();
  CHECK_FATAL(tag == kBinTypenameStart, "kBinTypenameStart expected in ImportLocalTypeNameTable()");
  int32 size = ReadNum();
  for (int64 i = 0; i < size; i++) {
    GStrIdx strIdx = ImportStr();
    TyIdx tyIdx = ImportType();
    typeNameTab->SetGStrIdxToTyIdx(strIdx, tyIdx);
  }
  tag = ReadNum();
  CHECK_FATAL(tag == ~kBinTypenameStart, "pattern mismatch in ImportTypenametab()");
}

void BinaryMplImport::ImportFormalsStIdx(MIRFunction *func) {
  int64 tag = ReadNum();
  CHECK_FATAL(tag == kBinFormalStart, "kBinFormalStart expected in ImportFormalsStIdx()");
  int32 size = ReadNum();
  for (int64 i = 0; i < size; i++) {
    uint32 indx = ReadNum();
    func->formalDefVec[i].formalSym = func->symTab->GetSymbolFromStIdx(indx);
  }
  tag = ReadNum();
  CHECK_FATAL(tag == ~kBinFormalStart, "pattern mismatch in ImportFormalsStIdx()");
}

void BinaryMplImport::ImportAliasMap(MIRFunction *func) {
  int64 tag = ReadNum();
  CHECK_FATAL(tag == kBinAliasMapStart, "kBinAliasMapStart expected in ImportAliasMap()");
  int32 size = ReadInt();
  for (int32 i = 0; i < size; i++) {
    MIRAliasVars aliasvars;
    GStrIdx strIdx = ImportStr();
    aliasvars.memPoolStrIdx = ImportStr();
    aliasvars.tyIdx = ImportType();
    /* aliasvars.sigStrIdx = */ ImportStr();  // not assigning to mimic parser
    func->aliasVarMap[strIdx] = aliasvars;
  }
  tag = ReadNum();
  CHECK_FATAL(tag == ~kBinAliasMapStart, "pattern mismatch in ImportAliasMap()");
}

PUIdx BinaryMplImport::ImportFuncViaSymName() {
  int64 tag = ReadNum();
  CHECK_FATAL(tag == kBinKindFuncViaSymname, "kBinKindFuncViaSymname expected");
  GStrIdx strIdx = ImportStr();
  MIRSymbol *sym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
  MIRFunction *func = sym->value.mirFunc;
  return func->puIdx;
}

BaseNode *BinaryMplImport::ImportExpression(MIRFunction *func) {
  int64 tag = ReadNum();
  CHECK_FATAL(tag == kBinOpExpression, "kBinOpExpression expected");
  Opcode op;
  PrimType typ;
  uint8 numopr;
  ImportBaseNode(op, typ, numopr);
  switch (op) {
    // leaf
    case OP_constval: {
      MIRConst *constv = ImportConst(func);
      ConstvalNode *constNode = mod.CurFuncCodeMemPool()->New<ConstvalNode>(constv);
      constNode->primType = typ;
      return constNode;
    }
    case OP_addroflabel: {
      AddroflabelNode *alabNode = mod.CurFuncCodeMemPool()->New<AddroflabelNode>();
      alabNode->offset = ReadNum();
      return alabNode;
    }
    case OP_addroffunc: {
      PUIdx puIdx = ImportFuncViaSymName();
      AddroffuncNode *addrNode = mod.CurFuncCodeMemPool()->New<AddroffuncNode>(typ, puIdx);
      return addrNode;
    }
    case OP_sizeoftype: {
      TyIdx tidx = ImportType();
      SizeoftypeNode *sot = mod.CurFuncCodeMemPool()->New<SizeoftypeNode>(tidx);
      return sot;
    }
    case OP_addrof:
    case OP_dread: {
      AddrofNode *drNode = mod.CurFuncCodeMemPool()->New<AddrofNode>(op);
      drNode->primType = typ;
      drNode->fieldID = ReadNum();
      drNode->stIdx.SetScope(ReadNum());
      if (drNode->stIdx.Islocal()) {
        drNode->stIdx.SetIdx(ReadNum());
      } else {
        int32 stag = ReadNum();
        CHECK_FATAL(stag == kBinKindSymViaSymname, "kBinKindSymViaSymname expected");
        GStrIdx strIdx = ImportStr();
        MIRSymbol *sym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
        drNode->stIdx.SetIdx(sym->stIdx.Idx());
      }
      return drNode;
    }
    case OP_regread: {
      RegreadNode *regreadNode = mod.CurFuncCodeMemPool()->New<RegreadNode>();
      regreadNode->regIdx = ReadNum();
      regreadNode->primType = typ;
      return regreadNode;
    }
    case OP_gcmalloc:
    case OP_gcpermalloc:
    case OP_stackmalloc: {
      TyIdx tyIdx = ImportType();
      GCMallocNode *gcNode = mod.CurFuncCodeMemPool()->New<GCMallocNode>(op, typ, tyIdx);
      return gcNode;
    }
    // unary
    case OP_abs:
    case OP_bnot:
    case OP_lnot:
    case OP_neg:
    case OP_recip:
    case OP_sqrt:
    case OP_alloca:
    case OP_malloc: {
      CHECK_FATAL(numopr == 1, "expected numOpnds to be 1");
      UnaryNode *unNode = mod.CurFuncCodeMemPool()->New<UnaryNode>(op, typ);
      unNode->uOpnd = ImportExpression(func);
      return unNode;
    }
    case OP_ceil:
    case OP_cvt:
    case OP_floor:
    case OP_trunc: {
      CHECK_FATAL(numopr == 1, "expected numOpnds to be 1");
      TypeCvtNode *typecvtNode = mod.CurFuncCodeMemPool()->New<TypeCvtNode>(op, typ);
      typecvtNode->fromPrimType = (PrimType)ReadNum();
      typecvtNode->uOpnd = ImportExpression(func);
      return typecvtNode;
    }
    case OP_retype: {
      CHECK_FATAL(numopr == 1, "expected numOpnds to be 1");
      RetypeNode *retypeNode = mod.CurFuncCodeMemPool()->New<RetypeNode>(typ);
      retypeNode->tyIdx = ImportType();
      retypeNode->uOpnd = ImportExpression(func);
      return retypeNode;
    }
    case OP_iread:
    case OP_iaddrof: {
      CHECK_FATAL(numopr == 1, "expected numOpnds to be 1");
      IreadNode *irNode = mod.CurFuncCodeMemPool()->New<IreadNode>(op, typ);
      irNode->tyIdx = ImportType();
      irNode->fieldID = ReadNum();
      irNode->uOpnd = ImportExpression(func);
      return irNode;
    }
    case OP_sext:
    case OP_zext:
    case OP_extractbits: {
      CHECK_FATAL(numopr == 1, "expected numOpnds to be 1");
      ExtractbitsNode *extNode = mod.CurFuncCodeMemPool()->New<ExtractbitsNode>(op, typ);
      extNode->bitsOffset = ReadNum();
      extNode->bitsSize = ReadNum();
      extNode->uOpnd = ImportExpression(func);
      return extNode;
    }
    case OP_gcmallocjarray:
    case OP_gcpermallocjarray: {
      CHECK_FATAL(numopr == 1, "expected numOpnds to be 1");
      JarrayMallocNode *gcNode = mod.CurFuncCodeMemPool()->New<JarrayMallocNode>(op, typ);
      gcNode->tyIdx = ImportType();
      gcNode->uOpnd = ImportExpression(func);
      return gcNode;
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
    case OP_land:
    case OP_lior:
    case OP_add: {
      CHECK_FATAL(numopr == 2, "expected numOpnds to be 2");
      BinaryNode *binNode = mod.CurFuncCodeMemPool()->New<BinaryNode>(op, typ);
      binNode->bOpnd[0] = ImportExpression(func);
      binNode->bOpnd[1] = ImportExpression(func);
      return binNode;
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
      CHECK_FATAL(numopr == 2, "expected numOpnds to be 2");
      CompareNode *cmpNode = mod.CurFuncCodeMemPool()->New<CompareNode>(op, typ);
      cmpNode->opndType = (PrimType)ReadNum();
      cmpNode->bOpnd[0] = ImportExpression(func);
      cmpNode->bOpnd[1] = ImportExpression(func);
      return cmpNode;
    }
    case OP_resolveinterfacefunc:
    case OP_resolvevirtualfunc: {
      CHECK_FATAL(numopr == 2, "expected numOpnds to be 2");
      ResolveFuncNode *rsNode = mod.CurFuncCodeMemPool()->New<ResolveFuncNode>(op, typ);
      rsNode->puIdx = ImportFuncViaSymName();
      rsNode->bOpnd[0] = ImportExpression(func);
      rsNode->bOpnd[1] = ImportExpression(func);
      return rsNode;
    }
    // ternary
    case OP_select: {
      CHECK_FATAL(numopr == 3, "expected numOpnds to be 3");
      TernaryNode *tNode = mod.CurFuncCodeMemPool()->New<TernaryNode>(op, typ);
      tNode->topnd[0] = ImportExpression(func);
      tNode->topnd[1] = ImportExpression(func);
      tNode->topnd[2] = ImportExpression(func);
      return tNode;
    }
    // nary
    case OP_array: {
      TyIdx tidx = ImportType();
      uint32 bchk = ReadNum();
      ArrayNode *arrNode = mod.CurFuncCodeMemPool()->New<ArrayNode>(func->GetCodeMpAllocator(), typ, tidx, bchk);
      uint32 n = ReadNum();
      for (uint32 i = 0; i < n; i++) {
        arrNode->nOpnd.push_back(ImportExpression(func));
      }
      arrNode->numOpnds = arrNode->nOpnd.size();
      return arrNode;
    }
    case OP_intrinsicop: {
      IntrinsicopNode *intrnNode = mod.CurFuncCodeMemPool()->New<IntrinsicopNode>(func->GetCodeMpAllocator(), op, typ);
      intrnNode->intrinsic = (MIRIntrinsicID)ReadNum();
      uint32 n = ReadNum();
      for (uint32 i = 0; i < n; i++) {
        intrnNode->nOpnd.push_back(ImportExpression(func));
      }
      intrnNode->numOpnds = intrnNode->nOpnd.size();
      return intrnNode;
    }
    case OP_intrinsicopwithtype: {
      IntrinsicopNode *intrnNode = mod.CurFuncCodeMemPool()->New<IntrinsicopNode>(func->GetCodeMpAllocator(), OP_intrinsicopwithtype, typ);
      intrnNode->intrinsic = (MIRIntrinsicID)ReadNum();
      intrnNode->tyIdx = ImportType();
      uint32 n = ReadNum();
      for (uint32 i = 0; i < n; i++) {
        intrnNode->nOpnd.push_back(ImportExpression(func));
      }
      intrnNode->numOpnds = intrnNode->nOpnd.size();
      return intrnNode;
    }
    default:
      CHECK_FATAL(false, "Unhandled tag %d", tag);
      break;
  }
}

void BinaryMplImport::ImportSrcPos(SrcPosition &pos) {
  pos.SetRawData(ReadNum());
  pos.SetLinenum(ReadNum());
}

void BinaryMplImport::ImportReturnValues(MIRFunction *func, CallReturnVector *retv) {
  int64 tag = ReadNum();
  CHECK_FATAL(tag == kBinReturnvals, "expecting return values");
  uint32 size = ReadNum();
  for (uint32 i = 0; i < size; i++) {
    CallReturnPair crPair;
    uint32 idx = ReadNum();
    FieldID fid = ReadNum();
    PregIdx16 ridx = ReadNum();
    retv->push_back(std::make_pair(StIdx(kScopeLocal, idx), RegFieldPair(fid,ridx)));
    MIRSymbol *lsym = func->symTab->GetSymbolFromStIdx(idx, 0);
    if (lsym->GetName().find("L_STR") == 0) {
      MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(lsym->tyIdx);
      CHECK_FATAL(ty->GetKind() == kTypePointer, "Pointer type expected for L_STR prefix");
      MIRPtrType tempType(static_cast<MIRPtrType *>(ty)->pointedTyIdx, PTY_ptr);
      TyIdx newTyidx = GlobalTables::GetTypeTable().CreateMIRType(&tempType);
      MIRType *newty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(newTyidx);
      lsym->tyIdx = newTyidx;
    }
  }
}

BlockNode *BinaryMplImport::ImportBlockNode(MIRFunction *func) {
  int64 tag = ReadNum();
  ASSERT(tag == kBinNodeBlock, "expecting a BlockNode");

  BlockNode *block = func->codeMemPool->New<BlockNode>();
  Opcode op;
  PrimType typ;
  uint8 numopr;
  ImportSrcPos(block->srcPosition);
  int32 size = ReadInt();
  for (int32 i = 0; i < size; i++) {
    tag = ReadNum();
    CHECK_FATAL(tag == kBinOpStatement, "kBinOpStatement expected");
    SrcPosition thesrcPosition;
    ImportSrcPos(thesrcPosition);
    op = (Opcode)ReadNum();
    StmtNode *stmt = nullptr;
    switch (op) {
      case OP_dassign: {
        DassignNode *s = func->codeMemPool->New<DassignNode>();
        s->fieldID = ReadNum();
        s->stIdx.SetScope(ReadNum());
        if (s->stIdx.Islocal()) {
          s->stIdx.SetIdx(ReadNum());
        } else {
          int32 stag = ReadNum();
          CHECK_FATAL(stag == kBinKindSymViaSymname, "kBinKindSymViaSymname expected");
          GStrIdx strIdx = ImportStr();
          MIRSymbol *sym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
          s->stIdx.SetIdx(sym->stIdx.Idx());
        }
        s->uOpnd = ImportExpression(func);
        stmt = s;
        break;
      }
      case OP_regassign: {
        RegassignNode *s = func->codeMemPool->New<RegassignNode>();
        s->primType = (PrimType)ReadNum();
        s->regIdx = ReadNum();
        s->uOpnd = ImportExpression(func);
        stmt = s;
        break;
      }
      case OP_iassign: {
        IassignNode *s = func->codeMemPool->New<IassignNode>();
        s->tyIdx = ImportType();
        s->fieldID = ReadNum();
        s->addrExpr = ImportExpression(func);
        s->rhs = ImportExpression(func);
        stmt = s;
        break;
      }
      case OP_call:
      case OP_virtualcall:
      case OP_virtualicall:
      case OP_superclasscall:
      case OP_interfacecall:
      case OP_interfaceicall:
      case OP_customcall: {
        CallNode *s = func->codeMemPool->New<CallNode>(&mod, op);
        s->puIdx = ImportFuncViaSymName();
        numopr = ReadNum();
        s->numOpnds = numopr;
        for (int32 i = 0; i < numopr; i++) {
          s->nOpnd.push_back(ImportExpression(func));
        }
        stmt = s;
        break;
      }
      case OP_callassigned:
      case OP_virtualcallassigned:
      case OP_virtualicallassigned:
      case OP_superclasscallassigned:
      case OP_interfacecallassigned:
      case OP_interfaceicallassigned:
      case OP_customcallassigned: {
        CallNode *s = func->codeMemPool->New<CallNode>(&mod, op);
        s->puIdx = ImportFuncViaSymName();
        ImportReturnValues(func, &s->returnValues);
        numopr = ReadNum();
        s->numOpnds = numopr;
        for (int32 i = 0; i < numopr; i++) {
          s->nOpnd.push_back(ImportExpression(func));
        }
        stmt = s;
        break;
      }
      case OP_polymorphiccall: {
        CallNode *s = func->codeMemPool->New<CallNode>(&mod, op);
        s->puIdx = ImportFuncViaSymName();
        s->tyIdx = ImportType();
        numopr = ReadNum();
        s->numOpnds = numopr;
        for (int32 i = 0; i < numopr; i++) {
          s->nOpnd.push_back(ImportExpression(func));
        }
        stmt = s;
        break;
      }
      case OP_polymorphiccallassigned: {
        CallNode *s = func->codeMemPool->New<CallNode>(&mod, op);
        s->puIdx = ImportFuncViaSymName();
        s->tyIdx = ImportType();
        ImportReturnValues(func, &s->returnValues);
        numopr = ReadNum();
        s->numOpnds = numopr;
        for (int32 i = 0; i < numopr; i++) {
          s->nOpnd.push_back(ImportExpression(func));
        }
        stmt = s;
        break;
      }
      case OP_icall: {
        IcallNode *s = func->codeMemPool->New<IcallNode>(&mod, op);
        s->retTyIdx = ImportType();
        numopr = ReadNum();
        s->numOpnds = numopr;
        for (int32 i = 0; i < numopr; i++) {
          s->nOpnd.push_back(ImportExpression(func));
        }
        stmt = s;
        break;
      }
      case OP_icallassigned: {
        IcallNode *s = func->codeMemPool->New<IcallNode>(&mod, op);
        s->retTyIdx = ImportType();
        ImportReturnValues(func, &s->returnValues);
        numopr = ReadNum();
        s->numOpnds = numopr;
        for (int32 i = 0; i < numopr; i++) {
          s->nOpnd.push_back(ImportExpression(func));
        }
        stmt = s;
        break;
      }
      case OP_intrinsiccall:
      case OP_xintrinsiccall: {
        IntrinsiccallNode *s = func->codeMemPool->New<IntrinsiccallNode>(&mod, op);
        s->intrinsic = (MIRIntrinsicID)ReadNum();
        numopr = ReadNum();
        s->numOpnds = numopr;
        for (int32 i = 0; i < numopr; i++) {
          s->nOpnd.push_back(ImportExpression(func));
        }
        stmt = s;
        break;
      }
      case OP_intrinsiccallassigned:
      case OP_xintrinsiccallassigned: {
        IntrinsiccallNode *s = func->codeMemPool->New<IntrinsiccallNode>(&mod, op);
        s->intrinsic = (MIRIntrinsicID)ReadNum();
        ImportReturnValues(func, &s->returnValues);
        numopr = ReadNum();
        s->numOpnds = numopr;
        for (int32 i = 0; i < numopr; i++) {
          s->nOpnd.push_back(ImportExpression(func));
        }
        if (s->returnValues.size() == 1 && s->returnValues[0].first.Idx() != 0) {
          MIRSymbol *retsymbol = func->symTab->GetSymbolFromStIdx(s->returnValues[0].first.Idx());
          MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(retsymbol->tyIdx);
          CHECK_FATAL(rettype != nullptr, "rettype is null in MIRParser::ParseStmtIntrinsiccallAssigned");
          s->primType = rettype->GetPrimType();
        }
        stmt = s;
        break;
      }
      case OP_intrinsiccallwithtype: {
        IntrinsiccallNode *s = func->codeMemPool->New<IntrinsiccallNode>(&mod, op);
        s->intrinsic = (MIRIntrinsicID)ReadNum();
        s->tyIdx = ImportType();
        numopr = ReadNum();
        s->numOpnds = numopr;
        for (int32 i = 0; i < numopr; i++) {
          s->nOpnd.push_back(ImportExpression(func));
        }
        stmt = s;
        break;
      }
      case OP_intrinsiccallwithtypeassigned: {
        IntrinsiccallNode *s = func->codeMemPool->New<IntrinsiccallNode>(&mod, op);
        s->intrinsic = (MIRIntrinsicID)ReadNum();
        s->tyIdx = ImportType();
        ImportReturnValues(func, &s->returnValues);
        numopr = ReadNum();
        s->numOpnds = numopr;
        for (int32 i = 0; i < numopr; i++) {
          s->nOpnd.push_back(ImportExpression(func));
        }
        if (s->returnValues.size() == 1 && s->returnValues[0].first.Idx() != 0) {
          MIRSymbol *retsymbol = func->symTab->GetSymbolFromStIdx(s->returnValues[0].first.Idx());
          MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(retsymbol->tyIdx);
          CHECK_FATAL(rettype != nullptr, "rettype is null in MIRParser::ParseStmtIntrinsiccallAssigned");
          s->primType = rettype->GetPrimType();
        }
        stmt = s;
        break;
      }
      case OP_syncenter:
      case OP_syncexit:
      case OP_return: {
        NaryStmtNode *s = func->codeMemPool->New<NaryStmtNode>(&mod, op);
        numopr = ReadNum();
        s->numOpnds = numopr;
        for (int32 i = 0; i < numopr; i++) {
          s->nOpnd.push_back(ImportExpression(func));
        }
        stmt = s;
        break;
      }
      case OP_jscatch:
      case OP_finally:
      case OP_endtry:
      case OP_cleanuptry:
      case OP_retsub:
      case OP_membaracquire:
      case OP_membarrelease:
      case OP_membarstorestore:
      case OP_membarstoreload: {
        stmt = mod.CurFuncCodeMemPool()->New<StmtNode>(op);
        break;
      }
      case OP_eval:
      case OP_throw:
      case OP_free:
      case OP_decref:
      case OP_incref:
      case OP_decrefreset:
      case OP_assertnonnull: {
        UnaryStmtNode *s = mod.CurFuncCodeMemPool()->New<UnaryStmtNode>(op);
        s->uOpnd = ImportExpression(func);
        stmt = s;
        break;
      }
      case OP_label: {
        LabelNode *s = mod.CurFuncCodeMemPool()->New<LabelNode>();
        s->labelIdx = ReadNum();
        stmt = s;
        break;
      }
      case OP_goto:
      case OP_gosub: {
        GotoNode *s = mod.CurFuncCodeMemPool()->New<GotoNode>(op);
        s->offset = ReadNum();
        stmt = s;
        break;
      }
      case OP_brfalse:
      case OP_brtrue: {
        CondGotoNode *s = mod.CurFuncCodeMemPool()->New<CondGotoNode>(op);
        s->offset = ReadNum();
        s->uOpnd = ImportExpression(func);
        stmt = s;
        break;
      }
      case OP_switch: {
        SwitchNode *s = mod.CurFuncCodeMemPool()->New<SwitchNode>(&mod);
        s->defaultLabel = ReadNum();
        uint32 size = ReadNum();
        for (uint32 i = 0; i < size; i++) {
          CasePair cpair = std::make_pair(ReadNum(), ReadNum());
          s->switchTable.push_back(cpair);
        }
        s->switchOpnd = ImportExpression(func);
        stmt = s;
        break;
      }
      case OP_jstry: {
        JsTryNode *s = mod.CurFuncCodeMemPool()->New<JsTryNode>();
        s->catchOffset = ReadNum();
        s->finallyOffset = ReadNum();
        stmt = s;
        break;
      }
      case OP_cpptry:
      case OP_try:
      case OP_javatry: {
        TryNode *s = mod.CurFuncCodeMemPool()->New<TryNode>(&mod, op);
        uint32 numLabels = ReadNum();
        for (uint32 i = 0; i < numLabels; i++) {
          s->offsets.push_back(ReadNum());
        }
        stmt = s;
        break;
      }
      case OP_catch:
      case OP_javacatch: {
        CatchNode *s = mod.CurFuncCodeMemPool()->New<CatchNode>(&mod);
        uint32 numTys = ReadNum();
        for (uint32 i = 0; i < numTys; i++) {
          s->exceptionTyIdxVec.push_back(ImportType());
        }
        stmt = s;
        break;
      }
      case OP_comment: {
        CommentNode *s = mod.CurFuncCodeMemPool()->New<CommentNode>(&mod);
        string str;
        ReadAsciiStr(str);
        s->comment = str;
        stmt = s;
        break;
      }
      case OP_dowhile:
      case OP_while: {
        WhileStmtNode *s = mod.CurFuncCodeMemPool()->New<WhileStmtNode>(op);
        s->body = ImportBlockNode(func);
        s->uOpnd = ImportExpression(func);
        stmt = s;
        break;
      }
      case OP_if: {
        IfStmtNode *s = mod.CurFuncCodeMemPool()->New<IfStmtNode>();
        bool hasElsePart = ReadNum();
        s->thenPart = ImportBlockNode(func);
        if (hasElsePart) {
          s->elsePart = ImportBlockNode(func);
        }
        s->uOpnd = ImportExpression(func);
        stmt = s;
        break;
      }
      default:
        CHECK_FATAL(false, "Unhandled opcode tag %d", tag);
        break;
    }
    stmt->srcPosition = thesrcPosition;
    block->AddStatement(stmt);
  }
  if (func != nullptr) {
    func->body = block;
  }
  return block;
}

void BinaryMplImport::ReadFunctionBodyField() {
  ReadInt();  /// skip total size
  int32 size = ReadInt();
  for (int64 i = 0; i < size; i++) {
    PUIdx puIdx = ImportFunction();
    MIRFunction *fn = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
    mod.SetCurFunction((mir_func_t *)fn);

    fn->symTab = fn->dataMemPool->New<MIRSymbolTable>(&fn->dataMPAllocator);
    fn->pregTab = fn->dataMemPool->New<MIRPregTable>(&fn->dataMPAllocator);
    fn->typeNameTab = fn->dataMemPool->New<MIRTypeNameTable>(&fn->dataMPAllocator);
    fn->labelTab = fn->dataMemPool->New<MIRLabelTable>(&fn->dataMPAllocator);

    ImportFuncIdInfo(fn);
    ImportLocalSymTab(fn);
    ImportPregTab(fn);
    ImportLabelTab(fn);
    ImportLocalTypeNameTable(fn->typeNameTab);
    ImportFormalsStIdx(fn);
    ImportAliasMap(fn);
    ImportBlockNode(fn);
    mod.functionList.push_back(fn);
  }
  int64 tag = ReadNum();
  CHECK_FATAL(tag == ~kBinFunctionBodyStart, "pattern mismatch in Read FunctionBody");
  return;
}

}  // namespace maple
