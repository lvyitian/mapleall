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

#include "be_lowerer.h"
#include "switch_lowerer.h"
#include "intrinsic_op.h"
#include "mir_builder.h"
#include "opcode_info.h"
#include "aarch64_rt_support.h"
#include "cg_assert.h"
#include <vector>
#include <map>
#include <cmath>
#include <algorithm>
#include <iostream>
#include <inttypes.h>  // PRId64
#include "securec.h"
#include "special_func.h"

namespace maplebe {

using namespace maple;

enum ext_func_t { kFmodDouble, kFmodFloat };

struct ext_func_descr_t {
  ext_func_t fid;
  const char *name;
  PrimType rettype;
  PrimType argtypes[8];
};

static ext_func_descr_t extFnDescrs[] = {
  { kFmodDouble, "fmod", PTY_f64, { PTY_f64, PTY_f64, kPtyInvalid } },
  { kFmodFloat, "fmodf", PTY_f32, { PTY_f32, PTY_f32, kPtyInvalid } },
};

static std::vector<std::pair<ext_func_t, PUIdx>> extFuncs;
static StmtNode *curStmt;

MIRSymbol *BELowerer::CreateNewRetVar(const MIRType *ty, const char *prefix) {
  std::string buf(prefix);
  MIRFunction *f = GetCurrentFunc();
  MIRSymbol *var = f->symTab->CreateSymbol(kScopeLocal);

  buf += std::to_string(++seed);
  var->SetNameStridx(mirModule.mirBuilder->GetOrCreateStringIndex(buf));
  var->SetTyIdx(ty->tyIdx);
  var->storageClass = kScAuto;
  var->sKind = kStVar;
  f->symTab->AddToStringSymbolMap(var);
  return var;
}

void BELowerer::RegisterExternalLibraryFunctions() {
  for (uint i = 0; i < sizeof(extFnDescrs) / sizeof(extFnDescrs[0]); ++i) {
    ext_func_t id = extFnDescrs[i].fid;
    CHECK_FATAL(id == i, "");

    MIRFunction *f = mirModule.mirBuilder->GetOrCreateFunction(extFnDescrs[i].name, TyIdx(extFnDescrs[i].rettype));
    MIRSymbol *fsym = f->GetFuncSymbol();
    fsym->SetStorageClass(kScExtern);

    // return type
    MIRType *retty = GlobalTables::GetTypeTable().GetPrimType(extFnDescrs[i].rettype);

    // use void* for PTY_dynany
    if (retty->GetPrimType() == PTY_dynany) {
      retty = GlobalTables::GetTypeTable().GetPtr();
    }
    f->SetReturnTyIdx(retty->GetTypeIndex());

    for (uint j = 0; extFnDescrs[i].argtypes[j] != kPtyInvalid; ++j) {
      PrimType pty = extFnDescrs[i].argtypes[j];
      MIRType *argty = GlobalTables::GetTypeTable().GetPrimType(pty);
      // use void* for PTY_dynany
      if (argty->GetPrimType() == PTY_dynany) {
        argty = GlobalTables::GetTypeTable().GetPtr();
      }
      if (f->symTab == nullptr) {
        f->symTab = mirModule.memPool->New<MIRSymbolTable>(&mirModule.memPoolAllocator);
      }
      MIRSymbol *argst = f->symTab->CreateSymbol(kScopeLocal);
      std::string buf = "p";
      buf += std::to_string(j);

      argst->SetNameStridx(mirModule.mirBuilder->GetOrCreateStringIndex(buf));
      argst->SetTyIdx(argty->tyIdx);
      argst->storageClass = kScFormal;
      argst->sKind = kStVar;
      f->symTab->AddToStringSymbolMap(argst);
      f->AddArgument(argst);
    }
    extFuncs.push_back(std::pair<ext_func_t, PUIdx>(id, f->puIdx));
  }
}

BaseNode *BELowerer::NodeConvert(PrimType mtype, BaseNode *expr) {
  PrimType srctype = expr->primType;
  if (GetPrimTypeSize(mtype) == GetPrimTypeSize(srctype)) {
    return expr;
  }
  TypeCvtNode *cvtnode = mirModule.CurFuncCodeMemPool()->New<TypeCvtNode>(OP_cvt);
  cvtnode->fromPrimType = srctype;
  cvtnode->primType = mtype;
  cvtnode->uOpnd = expr;
  return cvtnode;
}

BaseNode *BELowerer::LowerIaddrof(const IreadNode *iaddrof) {
  if (iaddrof->fieldID == 0) {
    return iaddrof->Opnd(0);
  }
  int32 offset = 0;
  MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(iaddrof->tyIdx);
  MIRPtrType *pointerty = dynamic_cast<MIRPtrType *>(type);
  CHECK_FATAL(pointerty, "LowerIaddrof: expect a pointer type at iaddrof node");
  MIRStructType *structty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx)->EmbeddedStructType();
  CHECK_FATAL(structty, "LowerIaddrof: non-zero fieldID for non-structure");
  offset = becommon.GetFieldOffset(structty, iaddrof->fieldID).first;
  if (offset == 0) {
    return iaddrof->Opnd(0);
  }
  uint32 loweredPtrType = static_cast<uint32>(LOWERED_PTR_TYPE);
  MIRIntConst *offsetconst =
    mirModule.memPool->New<MIRIntConst>(offset, GlobalTables::GetTypeTable().typeTable.at(loweredPtrType));
  BaseNode *offsetnode = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>(offsetconst);
  offsetnode->primType = LOWERED_PTR_TYPE;

  BinaryNode *addnode = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_add);
  addnode->primType = LOWERED_PTR_TYPE;
  addnode->bOpnd[0] = iaddrof->Opnd(0);
  addnode->bOpnd[1] = offsetnode;
  return addnode;
}

// Put result of node into a local.
BaseNode *BELowerer::SplitBinaryNodeOpnd1(BinaryNode *bnode, BlockNode *blknode) {
  if (becommon.optim_level != 0) {
    return bnode;
  }
  MIRBuilder *mirbuilder = mirModule.mirBuilder;
  static uint32 val = 0;
  std::string name("bnaryTmp");
  name.append(std::to_string(val++));

  BaseNode *opnd1 = bnode->Opnd(1);
  MIRSymbol *dnodeSt = mirbuilder->GetOrCreateLocalDecl(name, GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)(opnd1->primType)));
  DassignNode *dnode = mirbuilder->CreateStmtDassign(dnodeSt, 0, opnd1);
  blknode->InsertAfter(blknode->GetLast(), dnode);

  BaseNode *dreadNode = mirbuilder->CreateExprDread(dnodeSt);
  bnode->SetOpnd(dreadNode, 1);

  return bnode;
}

// Put result of node into a local.
BaseNode *BELowerer::SplitTernaryNodeResult(TernaryNode *tnode, BaseNode *parent, BlockNode *blknode) {
  if (becommon.optim_level != 0) {
    return tnode;
  }
  MIRBuilder *mirbuilder = mirModule.mirBuilder;
  static uint32 val = 0;
  std::string name("tnaryTmp");
  name.append(std::to_string(val++));

  MIRSymbol *dassignNodeSym = mirbuilder->GetOrCreateLocalDecl(name, GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)(tnode->primType)));
  DassignNode *dassignNode = mirbuilder->CreateStmtDassign(dassignNodeSym, 0, tnode);
  blknode->InsertAfter(blknode->GetLast(), dassignNode);

  BaseNode *dreadNode = mirbuilder->CreateExprDread(dassignNodeSym);
  for (int32 i = 0; i < parent->NumOpnds(); i++) {
    if (parent->Opnd(i) == tnode) {
      parent->SetOpnd(dreadNode, i);
      break;
    }
  }

  return dreadNode;
}


// Check if the operand of the select node is complex enough for either functionality or performance reason so we need to lower it to if-then-else.
bool BELowerer::IsComplexSelect(TernaryNode *tnode, BaseNode *parent, BlockNode *blknode) {
    if (tnode->primType == PTY_agg)
      return true;

    // Iread may have side effect which may cause correctness issue.
    if (tnode->Opnd(1)->op == OP_iread ||tnode->Opnd(2)->op == OP_iread)
      return true;

    return false;
}

// Lower agg select node back to if-then-else stmt.
BaseNode *BELowerer::LowerComplexSelect(TernaryNode *tnode, BaseNode *parent, BlockNode *blknode) {
  MIRBuilder *mirbuilder = mirModule.mirBuilder;
  static uint32 val = 0;
  std::string name("ComplexSelectTmp");
  name.append(std::to_string(val++));

  MIRType *resultTy = 0;
  if (tnode->primType == PTY_agg) {
    if (tnode->Opnd(1)->op == OP_dread) {
      DreadNode *trueNode = static_cast<DreadNode *>(tnode->Opnd(1));
      resultTy = mirModule.CurFunction()->GetLocalOrGlobalSymbol(trueNode->stIdx)->GetType();
    } else if (tnode->Opnd(1)->op == OP_iread) {
      IreadNode *trueNode = static_cast<IreadNode *>(tnode->Opnd(1));
      MIRPtrType *ptrty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(trueNode->tyIdx));
      resultTy = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptrty->pointedTyIdx));
      if (trueNode->fieldID != 0) {
        MIRStructType *structty = static_cast<MIRStructType *>(resultTy);
        FieldPair thepair = structty->TraverseToField(trueNode->fieldID);
        resultTy = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
      }
    } else {
      CHECK_FATAL(false, "NYI: LowerComplexSelect");
    }
  } else {
    resultTy =  GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)(tnode->primType));
  }

  MIRSymbol * resultSym = mirbuilder->GetOrCreateLocalDecl(name, resultTy);
  CondGotoNode *brTargetStmt = mirModule.CurFuncCodeMemPool()->New<CondGotoNode>(OP_brfalse);
  brTargetStmt->uOpnd = tnode->Opnd(0);
  LabelIdx targetIdx = mirModule.CurFunction()->labelTab->CreateLabel();
  mirModule.CurFunction()->labelTab->AddToStringLabelMap(targetIdx);
  brTargetStmt->offset = targetIdx;
  blknode->InsertAfter(blknode->GetLast(), brTargetStmt);

  DassignNode *dassignTrue = mirbuilder->CreateStmtDassign(resultSym, 0, tnode->Opnd(1));
  blknode->InsertAfter(blknode->GetLast(), dassignTrue);

  GotoNode *gotoStmt = mirModule.CurFuncCodeMemPool()->New<GotoNode>(OP_goto);
  LabelIdx EndIdx = mirModule.CurFunction()->labelTab->CreateLabel();
  mirModule.CurFunction()->labelTab->AddToStringLabelMap(EndIdx);
  gotoStmt->offset = EndIdx;
  blknode->InsertAfter(blknode->GetLast(), gotoStmt);

  LabelNode *lableStmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
  lableStmt->labelIdx = targetIdx;
  blknode->InsertAfter(blknode->GetLast(), lableStmt);

  DassignNode *dassignFalse = mirbuilder->CreateStmtDassign(resultSym, 0, tnode->Opnd(2));
  blknode->InsertAfter(blknode->GetLast(), dassignFalse);

  lableStmt = mirModule.CurFuncCodeMemPool()->New<LabelNode>();
  lableStmt->labelIdx = EndIdx;
  blknode->InsertAfter(blknode->GetLast(), lableStmt);

  BaseNode *dreadNode = mirbuilder->CreateExprDread(resultSym);
  for (int32 i = 0; i < parent->NumOpnds(); i++) {
    if (parent->Opnd(i) == tnode) {
      parent->SetOpnd(dreadNode, i);
      break;
    }
  }

  return dreadNode;
}

BaseNode *BELowerer::LowerFarray(ArrayNode *array) {
  MIRFarrayType *farraytype = static_cast<MIRFarrayType *>(array->GetArrayType(&GlobalTables::GetTypeTable()));
  uint32 esize = GlobalTables::GetTypeTable().GetTypeFromTyIdx(farraytype->elemTyIdx)->GetSize();
  if (farraytype->GetKind() == kTypeJArray) {
    if (farraytype->GetElemType()->GetKind() != kTypeScalar) {
      // not the last dimension of primitive array
      esize = AArch64RTSupport::kRefFieldSize;
    }
  }

  // how about multi-dimension array?
  if (array->GetIndex(0)->op == OP_constval) {
    const ConstvalNode *pConstvalNode = static_cast<const ConstvalNode *>(array->GetIndex(0));
    if (pConstvalNode->constVal->kind == kConstInt) {
      const MIRIntConst *pIntConst = static_cast<const MIRIntConst *>(pConstvalNode->constVal);
      CHECK_FATAL(pIntConst->value >= 0, "Array index should >= 0.");
      int64 eleOffset = pIntConst->value * esize;

      if (farraytype->GetKind() == kTypeJArray) {
        eleOffset += AArch64RTSupport::kArrayContentOffset;
      }

      BaseNode *baseNode = NodeConvert(array->primType, array->GetBase());
      if (eleOffset == 0) {
        return baseNode;
      }

      MIRIntConst *eleConst = mirModule.memPool->New<MIRIntConst>(eleOffset, GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(array->primType)));
      BaseNode *offsetNode = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>(eleConst);
      offsetNode->primType = array->primType;

      BaseNode *rAdd = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_add);
      rAdd->primType = array->primType;
      rAdd->SetOpnd(baseNode, 0);
      rAdd->SetOpnd(offsetNode, 1);
      return rAdd;
    }
  }

  BaseNode *resNode = NodeConvert(array->primType, array->GetIndex(0));
  BaseNode *rMul = nullptr;

  if (farraytype->GetKind() == kTypeJArray && resNode->op == OP_constval) {
    ConstvalNode *idxNode = static_cast<ConstvalNode *>(resNode);
    int64 idx = static_cast<MIRIntConst *>(idxNode->constVal)->value;
    MIRIntConst *econst = mirModule.memPool->New<MIRIntConst>(idx * esize, GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(array->primType)));
    rMul = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>(econst);
    rMul->primType = array->primType;
  } else {
    MIRIntConst *econst = mirModule.memPool->New<MIRIntConst>(esize, GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(array->primType)));
    BaseNode *eSize = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>(econst);
    eSize->primType = array->primType;
    rMul = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_mul);
    rMul->primType = array->primType;
    rMul->SetOpnd(resNode, 0);
    rMul->SetOpnd(eSize, 1);
  }

  BaseNode *baseNode = NodeConvert(array->primType, array->GetBase());

  if (farraytype->GetKind() == kTypeJArray) {
    BaseNode *jarrayBaseNode = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_add);
    MIRIntConst *arrayHeaderNode = mirModule.CurFuncCodeMemPool()->New<MIRIntConst>(
      AArch64RTSupport::kArrayContentOffset, GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(array->primType)));
    BaseNode *arrayHeaderCstNode = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>(arrayHeaderNode);
    arrayHeaderCstNode->primType = array->primType;
    jarrayBaseNode->primType = array->primType;
    jarrayBaseNode->SetOpnd(baseNode, 0);
    jarrayBaseNode->SetOpnd(arrayHeaderCstNode, 1);
    baseNode = jarrayBaseNode;
  }

  BaseNode *rAdd = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_add);
  rAdd->primType = array->primType;
  rAdd->SetOpnd(baseNode, 0);
  rAdd->SetOpnd(rMul, 1);
  return rAdd;
}

BaseNode *BELowerer::LowerArray(ArrayNode *array) {
  MIRType *atype = array->GetArrayType(&GlobalTables::GetTypeTable());
  if (atype->GetKind() == kTypeFArray || atype->GetKind() == kTypeJArray) {
    return LowerFarray(array);
  }

  MIRArrayType *arraytype = static_cast<MIRArrayType *>(atype);
  /* There are two cases where dimension > 1.
   * 1) arraytype->dim > 1.  Process the current arraytype. (nestedArray = false)
   * 2) arraytype->dim == 1, but arraytype->eTyIdx is another array. (nestedArray = true)
   * Assume at this time 1) and 2) cannot mix.
   * Along with the array dimension, there is the array indexing.
   * It is allowed to index arrays less than the dimension.
   * This is dictated by the number of indexes.
   */
  bool nestedArray = false;
  int dim = arraytype->dim;
  MIRType *innerType = nullptr;
  MIRArrayType *innerArrayType = nullptr;
  uint32 elemSize = 0;
  if (dim == 1) {
    innerType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(arraytype->eTyIdx);
    if (innerType->GetKind() == kTypeArray) {
      nestedArray = true;
      do {
        innerArrayType = static_cast<MIRArrayType *>(innerType);
        elemSize = RoundUp(becommon.type_size_table[innerArrayType->eTyIdx.GetIdx()],
                           becommon.type_align_table[arraytype->tyIdx.GetIdx()]);
        dim++;
        innerType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(innerArrayType->eTyIdx);
      } while (innerType->GetKind() == kTypeArray);
    }
  }

  int32 numIndex = array->NumOpnds() - 1;
  MIRArrayType *curArrayType = arraytype;
  BaseNode *resNode = NodeConvert(array->primType, array->GetIndex(0));
  if (dim > 1) {
    BaseNode *prevNode = nullptr;
    for (int i = 0; (i < dim) && (i < numIndex); i++) {
      uint32 mpyDim = 1;
      if (nestedArray) {
        CHECK_FATAL(arraytype->sizeArray[0] > 0, "Zero size array dimension");
        innerType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(curArrayType->eTyIdx);
        curArrayType = static_cast<MIRArrayType *>(innerType);
        while (innerType->GetKind() == kTypeArray) {
          innerArrayType = static_cast<MIRArrayType *>(innerType);
          mpyDim *= innerArrayType->sizeArray[0];
          innerType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(innerArrayType->eTyIdx);
        }
      } else {
        CHECK_FATAL(arraytype->sizeArray[i] > 0, "Zero size array dimension");
        for (int j = i + 1; j < dim; j++) {
          mpyDim *= arraytype->sizeArray[j];
        }
      }

      BaseNode *index = static_cast<ConstvalNode *>(array->GetIndex(i));
      bool isConst = false;
      int32 indexVal = 0;
      if (index->op == OP_constval) {
        ConstvalNode *constNode = static_cast<ConstvalNode *>(index);
        indexVal = (static_cast<MIRIntConst *>(constNode->constVal))->value;
        isConst = true;
        MIRIntConst *newConstNode = mirModule.memPool->New<MIRIntConst>(
                    indexVal * mpyDim,
                    GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(array->primType)));
        BaseNode *newValNode = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>(newConstNode);
        newValNode->primType = array->primType;
        if (i == 0) {
          prevNode = newValNode;
          continue;
        } else {
          resNode = newValNode;
        }
      }
      if (i > 0 && isConst == false) {
        resNode = NodeConvert(array->primType, array->GetIndex(i));
      }

      BaseNode *mpyNode;
      if (isConst) {
        MIRIntConst *mulConst = mirModule.memPool->New<MIRIntConst>(
                    mpyDim * indexVal,
                    GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(array->primType)));
        BaseNode *mulSize = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>(mulConst);
        mulSize->primType = array->primType;
        mpyNode = mulSize;
      } else if (mpyDim == 1 && prevNode) {
        mpyNode = prevNode;
        prevNode = resNode;
      } else {
        mpyNode = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_mul);
        mpyNode->primType = array->primType;
        MIRIntConst *mulConst = mirModule.memPool->New<MIRIntConst>(
                    mpyDim,
                    GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(array->primType)));
        BaseNode *mulSize = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>(mulConst);
        mulSize->primType = array->primType;
        mpyNode->SetOpnd(NodeConvert(array->primType, mulSize), 0);
        mpyNode->SetOpnd(resNode, 1);
      }
      if (i == 0) {
        prevNode = mpyNode;
        continue;
      }
      BaseNode *newResNode = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_add);
      newResNode->primType = array->primType;
      newResNode->SetOpnd(mpyNode, 0);
      newResNode->SetOpnd(prevNode, 1);
      prevNode = newResNode;
    }
    resNode = prevNode;
  }

  BaseNode *rMul = nullptr;
  // esize is the size of the array element (eg. int = 4 long = 8)
  uint32 esize;
  if (nestedArray) {
    esize = elemSize;
  } else {
    esize = becommon.type_size_table[arraytype->eTyIdx.GetIdx()];
  }
  Opcode opadd = OP_add;
  if (resNode->op == OP_constval) {
    // index is a constant, we can calculate the offset now
    ConstvalNode *idxNode = static_cast<ConstvalNode *>(resNode);
    int64 idx = static_cast<MIRIntConst *>(idxNode->constVal)->value;
    MIRIntConst *econst = mirModule.memPool->New<MIRIntConst>(
                    idx * esize,
                    GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(array->primType)));
    rMul = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>(econst);
    rMul->primType = array->primType;
    if (dim == 1 && array->GetBase()->op == OP_addrof) {
      opadd = OP_CG_array_elem_add;
    }
  } else {
    MIRIntConst *econst = mirModule.memPool->New<MIRIntConst>(
                    esize,
                    GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(array->primType)));
    BaseNode *eSize = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>(econst);
    eSize->primType = array->primType;
    rMul = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_mul);
    rMul->primType = array->primType;
    rMul->SetOpnd(resNode, 0);
    rMul->SetOpnd(eSize, 1);
  }
  BaseNode *baseNode = NodeConvert(array->primType, array->GetBase());
  BaseNode *rAdd = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(opadd);
  rAdd->primType = array->primType;
  rAdd->SetOpnd(baseNode, 0);
  rAdd->SetOpnd(rMul, 1);
  return rAdd;
}

BaseNode *BELowerer::LowerDreadBitfield(DreadNode *dread) {
  MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dread->stIdx);
  MIRStructType *structty = static_cast<MIRStructType *>(symbol->GetType());
  CHECK_FATAL(structty, "LowerDreadBitfield: non-zero fieldID for non-structure");
  FieldPair thepair = structty->TraverseToField(dread->fieldID);
  TyIdx ftyidx = thepair.second.first;
  CHECK_FATAL(ftyidx != TyIdx(0), "LoweDreadBitField: field id out of range for the structure");
  MIRType *ftype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx);
  if (ftype->GetKind() != kTypeBitField) {
    return dread;
  }
  uint8 fieldalign = becommon.type_align_table.at(ftyidx.GetIdx());
  std::pair<int32, int32> bytebitoffsets = becommon.GetFieldOffset(structty, dread->fieldID);
  CHECK_FATAL((bytebitoffsets.first % fieldalign) == 0, "LowerDreadBitfield: bitfield offset not multiple of its alignment");

  AddrofNode *addrofnode = mirModule.CurFuncCodeMemPool()->New<AddrofNode>(OP_addrof);
  addrofnode->primType = LOWERED_PTR_TYPE;
  addrofnode->stIdx = dread->stIdx;

  ConstvalNode *constnode = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>();
  constnode->primType = LOWERED_PTR_TYPE;
  uint32 loweredPtrType = static_cast<uint32>(LOWERED_PTR_TYPE);
  constnode->constVal =
    mirModule.memPool->New<MIRIntConst>(bytebitoffsets.first, GlobalTables::GetTypeTable().typeTable[loweredPtrType]);

  BinaryNode *addnode = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_add);
  addnode->primType = LOWERED_PTR_TYPE;
  addnode->bOpnd[0] = addrofnode;
  addnode->bOpnd[1] = constnode;

  IreadNode *ireadnode = mirModule.CurFuncCodeMemPool()->New<IreadNode>(OP_iread);
  ireadnode->primType = GetRegPrimType(ftype->primType);
  ireadnode->uOpnd = addnode;
  MIRType pointedtype(kTypeScalar, ftype->primType);
  TyIdx pointedTyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&pointedtype);
  MIRType *pointtype = becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointedTyIdx));
  ireadnode->tyIdx = pointtype->tyIdx;

  ExtractbitsNode *extrbitsnode = mirModule.CurFuncCodeMemPool()->New<ExtractbitsNode>(OP_extractbits);
  extrbitsnode->primType = GetRegPrimType(ftype->primType);
  extrbitsnode->bitsOffset = bytebitoffsets.second;
  extrbitsnode->bitsSize = static_cast<MIRBitfieldType *>(ftype)->fieldSize;
  extrbitsnode->uOpnd = ireadnode;

  PrimType ptype = extrbitsnode->primType;
  if (ptype == PTY_u8 || ptype == PTY_u16 || ptype == PTY_u32 || ptype == PTY_u64) {
    uint32 bitSz = extrbitsnode->bitsSize;
    uint32 byteSz = (bitSz <= 8) ? 1 : ((bitSz <= 16) ? 2 : ((bitSz <= 32) ? 4 : 8));
    if (GetPrimTypeSize(extrbitsnode->primType) > byteSz) {
      TypeCvtNode *cvtNode = mirModule.CurFuncCodeMemPool()->New<TypeCvtNode>(OP_cvt);
      cvtNode->fromPrimType = ftype->primType;
      cvtNode->primType = (byteSz == 1) ? PTY_u8 : (byteSz == 2) ? PTY_u16 : (byteSz == 4) ? PTY_u32 : PTY_u64;
      cvtNode->uOpnd = extrbitsnode;
      return cvtNode;
    }
  }

  return extrbitsnode;
}

BaseNode *BELowerer::LowerIreadBitfield(IreadNode *iread) {
  MIRPtrType *pointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().typeTable[iread->tyIdx.GetIdx()]);
  CHECK_FATAL(pointerty, "LowerIreadBitField: type in iread should be pointer type");
  MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
  // Here pointed type can be Struct or array
  MIRStructType *structty = pointedty->EmbeddedStructType();
  CHECK_FATAL(structty, "LowerIreadBitField: type in iread does not point to a struct");
  FieldPair thepair = structty->TraverseToField(iread->fieldID);
  TyIdx ftyidx = thepair.second.first;
  CHECK_FATAL(ftyidx != TyIdx(0), "LowerIreadBitField: field id out of range for the structure");
  MIRType *ftype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx);
  if (ftype->GetKind() != kTypeBitField) {
    return iread;
  }
  uint8 fieldalign = becommon.type_align_table.at(ftyidx.GetIdx());
  std::pair<int32, int32> bytebitoffsets = becommon.GetFieldOffset(structty, iread->fieldID);
  CHECK_FATAL((bytebitoffsets.first % fieldalign) == 0, "LowerIreadBitfield: bitfield offset not multiple of its alignment");

  ConstvalNode *constnode = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>();
  constnode->primType = LOWERED_PTR_TYPE;
  uint32 loweredPtrType = static_cast<uint32>(LOWERED_PTR_TYPE);
  constnode->constVal =
    mirModule.memPool->New<MIRIntConst>(bytebitoffsets.first, GlobalTables::GetTypeTable().typeTable[loweredPtrType]);

  BinaryNode *addnode = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_add);
  addnode->primType = LOWERED_PTR_TYPE;
  addnode->bOpnd[0] = iread->uOpnd;
  addnode->bOpnd[1] = constnode;

  IreadNode *ireadnode = mirModule.CurFuncCodeMemPool()->New<IreadNode>(OP_iread);
  ireadnode->primType = GetRegPrimType(ftype->primType);
  ireadnode->uOpnd = addnode;
  MIRType pointedtype(kTypeScalar, ftype->primType);
  TyIdx pointedTyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&pointedtype);
  MIRType *pointtype = becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointedTyIdx));
  ireadnode->tyIdx = pointtype->tyIdx;

  ExtractbitsNode *extrbitsnode = mirModule.CurFuncCodeMemPool()->New<ExtractbitsNode>(OP_extractbits);
  extrbitsnode->primType = GetRegPrimType(ftype->primType);
  extrbitsnode->bitsOffset = bytebitoffsets.second;
  extrbitsnode->bitsSize = static_cast<MIRBitfieldType *>(ftype)->fieldSize;
  extrbitsnode->uOpnd = ireadnode;

  return extrbitsnode;
}

/*
   For integer division 'a/b', we want to have
   1) if b is a leaf
    res = a/b
    if( b == 0 )
      call throw_Arithmetic_Exception
    return dread res
   2) if b is not a leaf
    b_var = b
    res = a/b_var
    if( b_var == 0 )
      call throw_Arithmetic_Exception
    return dread res
 */
BaseNode *BELowerer::LowerJavaDiv(BaseNode *parent, BinaryNode *expr, BlockNode *blknode) {
  PrimType ptype = expr->primType;
  MIRBuilder *mirbuilder = mirModule.mirBuilder;
  if (IsPrimitiveInteger(ptype)) {
    // store divopnd to a tmp st if not a leaf node
    BaseNode *divopnd = expr->Opnd(1);
    if (!divopnd->IsLeaf()) {
      MIRSymbol *divopndst = mirbuilder->GetOrCreateLocalDecl("__div_opnd1", GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)ptype));
      DassignNode *dssdivnode = mirbuilder->CreateStmtDassign(divopndst, 0, divopnd);
      blknode->AddStatement(dssdivnode);
      divopnd = mirbuilder->CreateExprDread(divopndst);
      expr->bOpnd[1] = divopnd;
    }

    MIRSymbol *divresst = mirbuilder->GetOrCreateLocalDecl("__div_res", GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)ptype));
    // put expr result to dssnode
    DassignNode *dssnode = mirbuilder->CreateStmtDassign(divresst, 0, expr);
    blknode->AddStatement(dssnode);

    // check if the second operand of the div expression is 0
    // insert condition jump,
    CompareNode *cmpnode =
        mirbuilder->CreateExprCompare(OP_eq, GlobalTables::GetTypeTable().GetInt32(), GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)ptype),
                                      divopnd, mirbuilder->CreateIntConst(0, ptype));

    LabelIdx divby0lbidx = mirbuilder->CreateLabidx(mirModule.CurFunction());

    CondGotoNode *condgotonode = mirbuilder->CreateStmtCondGoto(cmpnode, OP_brfalse, divby0lbidx);
    blknode->AddStatement(condgotonode);

    // call the throw function that will never return
    MIRFunction *func = mirModule.mirBuilder->GetOrCreateFunction(GetIntrinsicFuncName(INTRN_JAVA_THROW_ARITHMETIC), (TyIdx)(PTY_void));
    func->SetNoReturn();
    MapleVector<BaseNode *> alloccallargs(mirModule.memPoolAllocator.Adapter());
    CallNode *callassign = mirModule.mirBuilder->CreateStmtCall(func->puIdx, alloccallargs);
    blknode->AddStatement(callassign);

    LabelNode *divby0lb = mirbuilder->CreateStmtLabel(divby0lbidx);
    blknode->AddStatement(divby0lb);

    // make dread from the divresst and return it as new expression for this function
    return mirModule.mirBuilder->CreateExprDread(divresst, 0);
  } else {
    return expr;
  }
}

BaseNode *BELowerer::LowerSTACKMalloc(GCMallocNode *node, BlockNode *block) {
  return node;
}

BaseNode *BELowerer::LowerSTACKJarrayMalloc(JarrayMallocNode *node, BlockNode *block) {
  return node;
}

void BELowerer::LowerTypePtr(BaseNode *node) const {
  if (node->primType == PTY_ptr || node->primType == PTY_ref) {
    node->primType = LOWERED_PTR_TYPE;
  }

  if (kOpcodeInfo.IsTypeCvt(node->op)) {
    TypeCvtNode *cvt = static_cast<TypeCvtNode *>(node);
    if (cvt->fromPrimType == PTY_ptr || cvt->fromPrimType == PTY_ref) {
      cvt->fromPrimType = LOWERED_PTR_TYPE;
    }
  } else if (kOpcodeInfo.IsCompare(node->op)) {
    CompareNode *cmp = static_cast<CompareNode *>(node);
    if (cmp->opndType == PTY_ptr || cmp->opndType == PTY_ref) {
      cmp->opndType = LOWERED_PTR_TYPE;
    }
  }
}

BaseNode *BELowerer::LowerExpr(BaseNode *originParent, BaseNode *parent, BaseNode *expr, BlockNode *blknode) {
  LowerTypePtr(expr);

  for (int32 i = 0; i < expr->NumOpnds(); i++) {
    expr->SetOpnd(LowerExpr(originParent, expr, expr->Opnd(i), blknode), i);
  }

  switch (expr->op) {
    case OP_array: {
      if (!Shouldoptarray() && (mirModule.IsJavaModule())) {
        // Array boundary check
        MIRFunction *curFunc = mirModule.CurFunction();
        ArrayNode *arrayNode = static_cast<ArrayNode *>(expr);
        StmtNode *boundaryCheckStmt = nullptr;
        if (arrayNode->boundsCheck) {
          CHECK_FATAL(arrayNode->nOpnd.size() == 2, "unexpected nOpnd size");
          BaseNode *opnd0 = arrayNode->nOpnd[0];
          if (opnd0->op == OP_iread) {
            PregIdx pregidx = curFunc->pregTab->CreatePreg(opnd0->primType);
            RegassignNode *temp = mirModule.mirBuilder->CreateStmtRegassign(opnd0->primType, pregidx, opnd0);
            blknode->InsertAfter(blknode->GetLast(), temp);
            arrayNode->nOpnd[0] = mirModule.mirBuilder->CreateExprRegread(opnd0->primType, pregidx);
          }
#if !TARGARK
          MapleVector<BaseNode *> opnds(mirModule.CurFuncCodeMemPoolAllocator()->Adapter());
          opnds.push_back(arrayNode->nOpnd[0]);
          opnds.push_back(arrayNode->nOpnd[1]);
          MIRFunction *fn = mirModule.mirBuilder->GetOrCreateFunction(GetIntrinsicFuncName(INTRN_MCCArrayBoundaryCheck), TyIdx(PTY_void));
          boundaryCheckStmt = mirModule.mirBuilder->CreateStmtCall(fn->puIdx, opnds, OP_call);
#else
          IntrinDesc *intrinDesc = &IntrinDesc::intrintable[INTRN_MCCArrayBoundaryCheck];
          MIRType *retTyp = intrinDesc->GetReturnType();
          MapleVector<BaseNode*> ops(mirModule.mirBuilder->mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
          ops.push_back(arrayNode->nOpnd[0]);
          ops.push_back(arrayNode->nOpnd[1]);
          boundaryCheckStmt = mirModule.mirBuilder->CreateStmtIntrinsicCall( INTRN_MCCArrayBoundaryCheck, ops);
          boundaryCheckStmt->primType = retTyp->GetPrimType();
#endif
          blknode->InsertAfter(blknode->GetLast(), boundaryCheckStmt);
        }
      }

      return LowerArray(static_cast<ArrayNode *>(expr));
    }

    case OP_dread:
      return LowerDread(static_cast<DreadNode *>(expr));

    case OP_addrof:
      return LowerAddrof(static_cast<AddrofNode *>(expr));

    case OP_iread:
      return LowerIread(static_cast<IreadNode *>(expr));

    case OP_iaddrof:
      return LowerIaddrof(static_cast<IreadNode *>(expr));

    case OP_select:
      if (IsComplexSelect(static_cast<TernaryNode *>(expr), parent, blknode)) {
        return LowerComplexSelect(static_cast<TernaryNode *>(expr), parent, blknode);
      }
      else {
        return SplitTernaryNodeResult(static_cast<TernaryNode *>(expr), parent, blknode);
      }

    case OP_sizeoftype: {
      CHECK(static_cast<SizeoftypeNode *>(expr)->tyIdx.GetIdx() < becommon.type_size_table.size(),
            "index out of range in BELowerer::LowerExpr");
      int64 typesize = becommon.type_size_table[static_cast<SizeoftypeNode *>(expr)->tyIdx.GetIdx()];
      return mirModule.mirBuilder->CreateIntConst(typesize, PTY_u32);
    }

    case OP_intrinsicop:
      if (IsIntrinsicOpHandledAtLowerLevel(static_cast<IntrinsicopNode *>(expr)->intrinsic)) {
        return expr;
      }
      return LowerIntrinsicop(parent, static_cast<IntrinsicopNode *>(expr), blknode);

    case OP_stackmalloc: {
      return LowerSTACKMalloc(static_cast<GCMallocNode *>(expr), blknode);
    }
    case OP_alloca: {
      GetCurrentFunc()->hasVlaoralloca = true;
      return expr;
    }
    case OP_stackmallocjarray: {
      return LowerSTACKJarrayMalloc(static_cast<JarrayMallocNode *>(expr), blknode);
    }
    case OP_rem:
      return LowerRem(expr, blknode);

    case OP_cand:
      expr->op = OP_land;
      return SplitBinaryNodeOpnd1(static_cast<BinaryNode *>(expr), blknode);
    case OP_cior:
      expr->op = OP_lior;
      return SplitBinaryNodeOpnd1(static_cast<BinaryNode *>(expr), blknode);
    default:
      return expr;
  }
}

#if TARGARM || TARGAARCH64 || TARGARK || TARGRISCV64
BlockNode *BELowerer::LowerReturnStruct(NaryStmtNode *retnode) {
  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  for (uint32 i = 0; i < retnode->nOpnd.size(); i++) {
    retnode->SetOpnd(LowerExpr(retnode, retnode, retnode->nOpnd[i], blk), i);
  }
  BaseNode *opnd0 = retnode->Opnd(0);
  if (!(opnd0 && opnd0->primType == PTY_agg)) {
    // It is possible function never returns and have a dummy return const instead of a struct.
    CG_WARN("return struct should have a kid\n");
  }

  MIRFunction *curfunc = GetCurrentFunc();
  MIRSymbol *retst = curfunc->formalDefVec[0].formalSym;
  MIRPtrType *retty = static_cast<MIRPtrType *>(retst->GetType());
  IassignNode *iassign = mirModule.CurFuncCodeMemPool()->New<IassignNode>();
  if (becommon.type_size_table[retty->pointedTyIdx.GetIdx()] > 16 || !opnd0 || opnd0->primType != PTY_agg) {
    iassign->tyIdx = retty->tyIdx;
  } else {
    // struct goes into registers
    iassign->tyIdx = retty->pointedTyIdx;
  }
  iassign->fieldID = 0;
  iassign->rhs = opnd0;
  if (retst->IsPreg()) {
    RegreadNode *regnode = mirModule.mirBuilder->CreateExprRegread(
      SIZEOFPTR == 4 ? PTY_a32 : PTY_a64, curfunc->pregTab->GetPregIdxFromPregNo(retst->GetPreg()->pregNo));
    iassign->addrExpr = regnode;
  } else {
    AddrofNode *dreadnode = mirModule.CurFuncCodeMemPool()->New<AddrofNode>(OP_dread);
    dreadnode->primType = (SIZEOFPTR == 4 ? PTY_a32 : PTY_a64);
    dreadnode->stIdx = retst->GetStIdx();
    iassign->addrExpr = dreadnode;
  }
  blk->AddStatement(iassign);
  retnode->nOpnd.clear();
  retnode->numOpnds = 0;
  blk->AddStatement(retnode);
  return blk;
}

#endif /* TARGARM || TARGAARCH64 || TARGARK */

BlockNode *BELowerer::LowerReturn(NaryStmtNode *retnode) {
  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  if (retnode->NumOpnds() != 0) {
    BaseNode *expr = retnode->Opnd(0);
    Opcode opr = expr->op;
    if (opr == OP_dread) {
      AddrofNode *retExpr = static_cast<AddrofNode *>(expr);
      MIRFunction *mirFunc = mirModule.CurFunction();
      MIRSymbol *sym = mirFunc->GetLocalOrGlobalSymbol(retExpr->stIdx);
      if (sym->GetAttr(ATTR_localrefvar)) {
        mirFunc->retRefSym.insert(sym);
      }
    }
  }
  for (uint32 i = 0; i < retnode->nOpnd.size(); i++) {
    retnode->SetOpnd(LowerExpr(retnode, retnode, retnode->nOpnd[i], blk), i);
  }
  blk->AddStatement(retnode);
  return blk;
}

StmtNode *BELowerer::LowerDassignBitfield(DassignNode *dassign, BlockNode *newblk) {
  dassign->SetRhs(LowerExpr(dassign, dassign, dassign->GetRhs(), newblk));
  MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dassign->stIdx);
  MIRStructType *structty = static_cast<MIRStructType *>(symbol->GetType());
  CHECK_FATAL(structty, "LowerDassignBitfield: non-zero fieldID for non-structure");
  FieldPair thepair = structty->TraverseToField(dassign->fieldID);
  TyIdx ftyidx = thepair.second.first;
  CHECK_FATAL(ftyidx != TyIdx(0), "LowerDassignBitField: field id out of range for the structure");
  MIRType *ftype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx);
  if (ftype->GetKind() != kTypeBitField) {
    return dassign;
  }
  uint8 fieldalign = becommon.type_align_table.at(ftyidx.GetIdx());
  std::pair<int32, int32> bytebitoffsets = becommon.GetFieldOffset(structty, dassign->fieldID);
  CHECK_FATAL((bytebitoffsets.first % fieldalign) == 0,
         "LowerDassignBitfield: bitfield offset not multiple of its alignment");

  AddrofNode *addrofnode = mirModule.CurFuncCodeMemPool()->New<AddrofNode>(OP_addrof);
  addrofnode->primType = LOWERED_PTR_TYPE;
  addrofnode->stIdx = dassign->stIdx;

  ConstvalNode *constnode = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>();
  constnode->primType = LOWERED_PTR_TYPE;
  uint32 loweredPtrType = static_cast<uint32>(LOWERED_PTR_TYPE);
  constnode->constVal =
    mirModule.memPool->New<MIRIntConst>(bytebitoffsets.first, GlobalTables::GetTypeTable().typeTable[loweredPtrType]);

  BinaryNode *addnode = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_add);
  addnode->primType = LOWERED_PTR_TYPE;
  addnode->bOpnd[0] = addrofnode;
  addnode->bOpnd[1] = constnode;

  IreadNode *ireadnode = mirModule.CurFuncCodeMemPool()->New<IreadNode>(OP_iread);
  ireadnode->primType = GetRegPrimType(ftype->primType);
  ireadnode->uOpnd = addnode;
  MIRType pointedtype(kTypeScalar, ftype->primType);
  TyIdx pointedTyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&pointedtype);
  MIRType *pointtype = becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointedTyIdx));
  ireadnode->tyIdx = pointtype->tyIdx;

  DepositbitsNode *depositbits = mirModule.CurFuncCodeMemPool()->New<DepositbitsNode>();
  depositbits->primType = GetRegPrimType(ftype->primType);
  depositbits->bitsOffset = bytebitoffsets.second;
  depositbits->bitsSize = static_cast<MIRBitfieldType *>(ftype)->fieldSize;
  depositbits->bOpnd[0] = ireadnode;
  depositbits->bOpnd[1] = dassign->GetRhs();

  IassignNode *iassignstmt = mirModule.CurFuncCodeMemPool()->New<IassignNode>();
  iassignstmt->tyIdx = pointtype->tyIdx;
  iassignstmt->addrExpr = addnode->CloneTree(&mirModule);
  iassignstmt->rhs = depositbits;

  return iassignstmt;
}

StmtNode *BELowerer::LowerIassignBitfield(IassignNode *iassign, BlockNode *newblk) {
  iassign->addrExpr = LowerExpr(iassign, iassign, iassign->addrExpr, newblk);
  iassign->rhs = LowerExpr(iassign, iassign, iassign->rhs, newblk);

  MIRPtrType *pointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().typeTable[iassign->tyIdx.GetIdx()]);
  CHECK_FATAL(pointerty, "LowerIassignBitField: type in iassign should be pointer type");
  MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
  // Here pointed type can be Struct or array
  MIRStructType *structty = pointedty->EmbeddedStructType();
  CHECK_FATAL(structty, "LowerIassignBitField: type in iassign does not point to a struct");
  FieldPair thepair = structty->TraverseToField(iassign->fieldID);
  TyIdx ftyidx = thepair.second.first;
  CHECK_FATAL(ftyidx != TyIdx(0), "LowerIassignBitField: field id out of range for the structure");
  MIRType *ftype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx);
  if (ftype->GetKind() != kTypeBitField) {
    return iassign;
  }
  uint8 fieldalign = becommon.type_align_table.at(ftyidx.GetIdx());
  std::pair<int32, int32> bytebitoffsets = becommon.GetFieldOffset(structty, iassign->fieldID);
  CHECK_FATAL((bytebitoffsets.first % fieldalign) == 0,
         "LowerIassignBitfield: bitfield offset not multiple of its alignment");

  ConstvalNode *constnode = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>();
  constnode->primType = LOWERED_PTR_TYPE;
  uint32 loweredPtrType = static_cast<uint32>(LOWERED_PTR_TYPE);
  constnode->constVal =
    mirModule.memPool->New<MIRIntConst>(bytebitoffsets.first, GlobalTables::GetTypeTable().typeTable[loweredPtrType]);

  BinaryNode *addnode = mirModule.CurFuncCodeMemPool()->New<BinaryNode>(OP_add);
  addnode->primType = LOWERED_PTR_TYPE;
  addnode->bOpnd[0] = iassign->addrExpr;
  addnode->bOpnd[1] = constnode;

  IreadNode *ireadnode = mirModule.CurFuncCodeMemPool()->New<IreadNode>(OP_iread);
  ireadnode->primType = GetRegPrimType(ftype->primType);
  ireadnode->uOpnd = addnode;
  MIRType pointedtype(kTypeScalar, ftype->primType);
  TyIdx pointedTyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&pointedtype);
  MIRType *pointtype = becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointedTyIdx));
  ireadnode->tyIdx = pointtype->tyIdx;

  DepositbitsNode *depositbits = mirModule.CurFuncCodeMemPool()->New<DepositbitsNode>();
  depositbits->primType = GetRegPrimType(ftype->primType);
  depositbits->bitsOffset = bytebitoffsets.second;
  depositbits->bitsSize = static_cast<MIRBitfieldType *>(ftype)->fieldSize;
  depositbits->bOpnd[0] = ireadnode;
  depositbits->bOpnd[1] = iassign->rhs;

  IassignNode *iassignstmt = mirModule.CurFuncCodeMemPool()->New<IassignNode>();
  iassignstmt->tyIdx = pointtype->tyIdx;
  iassignstmt->addrExpr = addnode->CloneTree(&mirModule);
  iassignstmt->rhs = depositbits;

  return iassignstmt;
}

void BELowerer::LowerIassign(IassignNode *iassign, BlockNode *newblk) {
  StmtNode *newstmt = nullptr;
  if (iassign->fieldID != 0) {
    newstmt = LowerIassignBitfield(iassign, newblk);
  } else {
    CHECK_FATAL((iassign->primType != PTY_ptr && iassign->primType != PTY_ref), "should have been lowered already");
    LowerStmt(iassign, newblk);
    newstmt = iassign;
  }
  newblk->AddStatement(newstmt);
}

DassignNode *BELowerer::SaveReturnValueInLocal(StIdx stIdx, uint16 fieldID) {
  MIRSymbol *var = mirModule.CurFunction()->symTab->GetSymbolFromStIdx(stIdx.Idx());
  CHECK_FATAL(var, "");
  RegreadNode *regread = mirModule.mirBuilder->CreateExprRegread(
    GlobalTables::GetTypeTable().typeTable.at(var->GetTyIdx().GetIdx())->GetPrimType(), -kSregRetval0);
  return mirModule.mirBuilder->CreateStmtDassign(var, fieldID, regread);
}

BaseNode *BELowerer::LowerRem(BaseNode *expr, BlockNode *blk) {
  BinaryNode *remExpr = static_cast<BinaryNode *>(expr);
  if (!IsPrimitiveFloat(remExpr->primType)) {
    return expr;
  }
  MapleVector<BaseNode *> operands(mirModule.CurFuncCodeMemPoolAllocator()->Adapter());
  operands.push_back(remExpr->Opnd(0));
  operands.push_back(remExpr->Opnd(1));
  ext_func_t fmodFunc = remExpr->primType == PTY_f32 ? kFmodFloat : kFmodDouble;
  uint32 i = 0;
  for (; i < extFuncs.size(); ++i) {
    if (extFuncs[i].first == fmodFunc) {
      break;
    }
  }
  CHECK_FATAL(i < extFuncs.size(), "");
  MIRSymbol *ret = CreateNewRetVar(GlobalTables::GetTypeTable().GetPrimType(remExpr->primType), INTRN_RETVAL_PREFIX);
  CallNode *callstmt =
    mirModule.mirBuilder->CreateStmtCallAssigned(extFuncs[i].second, operands, ret, OP_callassigned);
  blk->AppendStatementsFromBlock(LowerCallAssignedStmt(callstmt));
  return mirModule.mirBuilder->CreateExprDread(GlobalTables::GetTypeTable().GetPrimType(extFnDescrs[fmodFunc].rettype), 0,
                                                ret);
}

BlockNode *BELowerer::LowerJavaThrow(UnaryStmtNode *throwstmt) {
  BlockNode *newblk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  BaseNode *opnd0 = throwstmt->uOpnd;
  CHECK_FATAL(opnd0->op == OP_dread && (opnd0->primType == PTY_ptr || opnd0->primType == PTY_ref),
         "expect a dread of a pointer to get its type");
  throwstmt->primType = (SIZEOFPTR == 4 ? PTY_a32 : PTY_a64);
  DreadNode *drnode = static_cast<DreadNode *>(LowerExpr(throwstmt, throwstmt, opnd0, newblk));
  CHECK_FATAL(drnode != nullptr, "drnode is null in BELowerer::LowerJavaThrow");
  MIRSymbol *st = mirModule.CurFunction()->GetLocalOrGlobalSymbol(drnode->stIdx);
  MIRType *psttype = st->GetType();
  CHECK_FATAL(psttype->typeKind == kTypePointer, "");

  MIRType *sttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(psttype)->pointedTyIdx);
  if (sttype->typeKind == kTypeClass) {
    MapleVector<BaseNode *> alloccallargs(mirModule.memPoolAllocator.Adapter());
    BaseNode *clonedDrnode = drnode->CloneTree(&mirModule);
    LowerTypePtr(clonedDrnode);
    alloccallargs.push_back(clonedDrnode);
    becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetVoid());
    MIRFunction *calleefunc = mirModule.mirBuilder->GetOrCreateFunction(GetIntrinsicFuncName(INTRN_MCCThrowException), (TyIdx)(PTY_void));
    calleefunc->SetNoReturn();
    CallNode *callassign = mirModule.mirBuilder->CreateStmtCall(calleefunc->puIdx, alloccallargs);
    newblk->AddStatement(callassign);
    return newblk;
  } else {
    CHECK_FATAL(sttype->primType == PTY_void, "error Java cannot throw a non-object");
    newblk->AddStatement(throwstmt);
    return newblk;
  }
}

// to lower call (including icall) and intrinsicall statements
void BELowerer::LowerCallStmt(StmtNode *stmt, StmtNode *&nextstmt, BlockNode *newblk, MIRType *retty) {
  StmtNode *newstmt = nullptr;
  if (stmt->op == OP_intrinsiccall) {
    IntrinsiccallNode *intrnnode = static_cast<IntrinsiccallNode *>(stmt);
    newstmt = LowerIntrinsiccall(intrnnode, newblk);
  } else {
    // We note the function has a user-defined (i.e., not an intrinsic) call.
    GetCurrentFunc()->SetHasCall();
    newstmt = stmt;
  }

  if (newstmt == nullptr) {
    return;
  }

  if (newstmt->op == OP_call || newstmt->op == OP_icall) {
    newstmt = LowerCall(static_cast<CallNode *>(newstmt), nextstmt, newblk, retty);
  }

  newblk->AddStatement(newstmt);
}

BlockNode *BELowerer::LowerCallAssignedStmt(StmtNode *stmt) {
  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  StmtNode *newcall = nullptr;
  CallReturnVector *p2nrets = nullptr;
  PUIdx funcCalled = kfuncNotFound;
  bool handledAtLowerLevel = false;
  switch (stmt->op) {
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned: {
      CallNode *origcall = static_cast<CallNode *>(stmt);
      if (stmt->op == OP_callassigned) {
        newcall = mirModule.mirBuilder->CreateStmtCall(origcall->puIdx, origcall->nOpnd);
      } else if (stmt->op == OP_virtualcallassigned) {
        newcall = mirModule.mirBuilder->CreateStmtVirtualCall(origcall->puIdx, origcall->nOpnd);
      } else if (stmt->op == OP_superclasscallassigned) {
        newcall = mirModule.mirBuilder->CreateStmtSuperclassCall(origcall->puIdx, origcall->nOpnd);
      } else if (stmt->op == OP_interfacecallassigned) {
        newcall = mirModule.mirBuilder->CreateStmtInterfaceCall(origcall->puIdx, origcall->nOpnd);
      }
      CHECK_FATAL(newcall, "");
      p2nrets = &origcall->returnValues;
      static_cast<CallNode *>(newcall)->returnValues = *p2nrets;
      funcCalled = origcall->puIdx;
      CHECK_FATAL((newcall->op == OP_call || newcall->op == OP_interfacecall),
             "virtual call or super class call are not expected");
      break;
    }
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned: {
      IntrinsiccallNode *origcall = static_cast<IntrinsiccallNode *>(stmt);
      handledAtLowerLevel = IsIntrinsicCallHandledAtLowerLevel(origcall->intrinsic);
      if (handledAtLowerLevel) {
        // If the lower level can handle the intrinsic, just let it pass through.
        newcall = origcall;
      } else {
        PUIdx bfunc = GetBuiltInToUse(origcall->intrinsic);
        if (bfunc != kfuncNotFound) {
          MapleVector<BaseNode *> args(mirModule.memPoolAllocator.Adapter());
          for (int i = 0; i < origcall->NumOpnds(); ++i) {
            args.push_back(origcall->Opnd(i));
          }
          newcall = mirModule.mirBuilder->CreateStmtCall(bfunc, args);
        } else {
          if (stmt->op == OP_intrinsiccallassigned) {
            newcall = mirModule.mirBuilder->CreateStmtIntrinsicCall(origcall->intrinsic, origcall->nOpnd);
          } else {
            newcall = mirModule.mirBuilder->CreateStmtXintrinsicCall(origcall->intrinsic, origcall->nOpnd);
          }
        }
        p2nrets = &origcall->returnValues;
        static_cast<IntrinsiccallNode *>(newcall)->returnValues = *p2nrets;
        funcCalled = bfunc;
        CHECK_FATAL((newcall->op == OP_call || newcall->op == OP_intrinsiccall), "xintrinsic call is not expected");
      }
      break;
    }
    case OP_intrinsiccallwithtypeassigned: {
      IntrinsiccallNode *origcall = static_cast<IntrinsiccallNode *>(stmt);
      handledAtLowerLevel = IsIntrinsicCallHandledAtLowerLevel(origcall->intrinsic);
      if (handledAtLowerLevel) {
        // If the lower level can handle the intrinsic, just let it pass through.
        newcall = origcall;
      } else {
        PUIdx bfunc = GetBuiltInToUse(origcall->intrinsic);
        if (bfunc != kfuncNotFound) {
          MapleVector<BaseNode *> args(mirModule.memPoolAllocator.Adapter());
          if (origcall->intrinsic == INTRN_MPL_MEMSET_LOCALVAR) {
            MIRClassType *classType = static_cast<MIRClassType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(origcall->tyIdx));
            becommon.ComputeTypeSizesAligns(classType);
            // If this is a non-escaped object allocated on stack, we need to
            // manufacture an extra object header for RC to work correctly
            CHECK_FATAL(classType->GetTypeIndex().GetIdx() < becommon.type_size_table.size(),
                   "index out of range in BELowerer::LowerCallAssignedStmt");
            uint64 objSize =
              becommon.type_size_table[classType->GetTypeIndex().GetIdx()] + AArch64RTSupport::kObjectHeaderSize;
            BinaryNode *subNode = mirModule.mirBuilder->CreateExprBinary(
                OP_sub, GlobalTables::GetTypeTable().GetAddr64(), origcall->Opnd(0),
                mirModule.mirBuilder->CreateIntConst(AArch64RTSupport::kObjectHeaderSize, PTY_u64));
            args.push_back(subNode);
            args.push_back(mirModule.mirBuilder->CreateIntConst(objSize, PTY_u32));
            args.push_back(mirModule.mirBuilder->CreateIntConst(0, PTY_u8));
            args.push_back(mirModule.mirBuilder->CreateIntConst(objSize, PTY_u32));
          } else {
            for (int i = 0; i < origcall->NumOpnds(); ++i) {
              args.push_back(origcall->Opnd(i));
            }
          }
          newcall = mirModule.mirBuilder->CreateStmtCall(bfunc, args);
        } else {
          newcall = mirModule.mirBuilder->CreateStmtIntrinsicCallwithtype(origcall->intrinsic, origcall->tyIdx,
                                                                            origcall->nOpnd);
        }
        p2nrets = &origcall->returnValues;
        static_cast<IntrinsiccallNode *>(newcall)->returnValues = *p2nrets;
        funcCalled = bfunc;
        CHECK_FATAL((newcall->op == OP_call || newcall->op == OP_intrinsiccallwithtype),
               "OP_call or OP_intrinsiccallwithtype is expected");
      }
      break;
    }
    case OP_icallassigned: {
      IcallNode *origcall = static_cast<IcallNode *>(stmt);
      newcall = mirModule.mirBuilder->CreateStmtIcall(origcall->nOpnd);
      CHECK_FATAL(newcall, "");
      p2nrets = &origcall->returnValues;
      funcCalled = kfuncNotFound;
      static_cast<IcallNode *>(newcall)->returnValues = *p2nrets;
      break;
    }
    default:
      CHECK_FATAL(false, "NIY");
      return nullptr;
  }

  // Currently, we assume all OP_<*>assigned is lowered either to OP_call or OP_intrinsicall
  // except when they are intrinsics and they are handled at lower level.
  switch (newcall->op) {
    case OP_icall:
      break;
    case OP_intrinsiccallwithtype:
      CHECK_FATAL(0, "intrinsiccallwithtype found\n");
    case OP_interfacecall:
      fprintf(stderr, "interfacecall found\n");
      break;
    case OP_virtualcall:
      CHECK_FATAL(0, "OP_virtualcall found\n");
    case OP_superclasscall:
      CHECK_FATAL(0, "OP_superclasscall found\n");
    case OP_xintrinsiccall:
      CHECK_FATAL(0, "OP_xintrinsiccall found\n");
    default:
      break;
  }

  // transfer srcPosition location info
  newcall->srcPosition = stmt->srcPosition;

  CHECK_FATAL(
    // Either they are lowered
    newcall->op == OP_intrinsiccall || newcall->op == OP_call || newcall->op == OP_icall ||
      // or they are still "assigned", but shall be handled at lower level.
      ((newcall->op == OP_intrinsiccallassigned || newcall->op == OP_intrinsiccallwithtypeassigned) &&
       handledAtLowerLevel),
    "call assigned stmts are expected to be lowered to OP_intrinsiccall or OP_call,"
    " unless it is an intrinsic call which is handled at lower level");

  blk->AddStatement(newcall);

  if (!handledAtLowerLevel) {
    CHECK_FATAL(p2nrets != nullptr && p2nrets->size() <= 1, "");
    // Create DassignStmt to save kSregRetval0.
    StmtNode *dstmt = nullptr;
    MIRType *rettype = nullptr;
    if (p2nrets->size() == 1) {
      StIdx stIdx = (*p2nrets)[0].first;
      MIRSymbol *sym = GetCurrentFunc()->symTab->GetSymbolFromStIdx(stIdx.Idx());
      bool sizeIs0 = false;
      if (sym) {
        rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
        if (becommon.type_size_table[rettype->tyIdx.GetIdx()] == 0) {
          sizeIs0 = true;
        }
      }
      if (sizeIs0 == false) {
        RegFieldPair regfieldpair = (*p2nrets)[0].second;
        if (!regfieldpair.IsReg()) {
          uint16 fieldID = regfieldpair.GetFieldid();
          DassignNode *dn = SaveReturnValueInLocal(stIdx, fieldID);
          CHECK_FATAL(dn->fieldID == 0, "");
          LowerDassign(dn, blk);
          CHECK_FATAL(newcall == blk->GetLast() || newcall->GetNext() == blk->GetLast(), "");
          dstmt = (newcall == blk->GetLast()) ? nullptr : blk->GetLast();
          CHECK_FATAL(newcall->GetNext() == dstmt, "");
        } else {
          uint32 pregIdx = static_cast<uint32>(regfieldpair.GetPregidx());
          MIRPreg *mirpreg = GetCurrentFunc()->pregTab->PregFromPregIdx(pregIdx);
          RegreadNode *regnode = mirModule.mirBuilder->CreateExprRegread(mirpreg->primType, -kSregRetval0);
          RegassignNode *regassign =
            mirModule.mirBuilder->CreateStmtRegassign(mirpreg->primType, regfieldpair.GetPregidx(), regnode);
          blk->AddStatement(regassign);
          dstmt = regassign;
        }
      }
    }

    blk->ResetBlock();

    // if VerboseAsm, insert a comment
    if (ShouldAddAdditionalComment()) {
      CommentNode *cmnt = mirModule.CurFuncCodeMemPool()->New<CommentNode>(&mirModule);
      cmnt->comment = kOpcodeInfo.GetName(stmt->op);
      if (funcCalled == kfuncNotFound) {
        cmnt->comment.append(" : unknown");
      } else {
        cmnt->comment.append(" : ").append(GlobalTables::GetFunctionTable().GetFunctionFromPuidx(funcCalled)->GetName());
      }
      blk->AddStatement(cmnt);
    }

    CHECK_FATAL(!dstmt || dstmt->GetNext() == nullptr, "");
    LowerCallStmt(newcall, dstmt, blk, rettype);
    if (dstmt != nullptr) {
      blk->AddStatement(dstmt);
    }
  }

  return blk;
}

void BELowerer::LowerStmt(StmtNode *stmt, BlockNode *newblk) {
  CHECK_FATAL((stmt->primType != PTY_ptr && stmt->primType != PTY_ref), "should have been lowered already");
  for (int i = 0; i < stmt->NumOpnds(); i++) {
    stmt->SetOpnd(LowerExpr(stmt, stmt, stmt->Opnd(i), newblk), i);
  }
}

BlockNode *BELowerer::LowerBlock(BlockNode *block) {
  BlockNode *newblk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  BlockNode *tmpBlockNode = nullptr;
  if (!block->GetFirst()) {
    return newblk;
  }

  StmtNode *nextstmt = block->GetFirst();
  do {
    StmtNode *stmt = nextstmt;
    //stmt->Dump(&mirModule, 0);
    curStmt = stmt;
    nextstmt = stmt->GetNext();
    stmt->SetNext(nullptr);
    current_blk_ = newblk;

    LowerTypePtr(stmt);

    switch (stmt->op) {
      case OP_switch: {
        LowerStmt(stmt, newblk);
        MemPool *switchMp = mempoolctrler.NewMemPool("switchlowerer");
        MapleAllocator switchAllocator(switchMp);
        SwitchLowerer switchLowerer(mirModule, static_cast<SwitchNode *>(stmt), this, &switchAllocator);
        BlockNode *blk = switchLowerer.LowerSwitch();
        if (blk->GetFirst()) {
          newblk->AppendStatementsFromBlock(blk);
        }
        mempoolctrler.DeleteMemPool(switchMp);
        need_branch_cleanup_ = true;
        break;
      }
      case OP_block:
        tmpBlockNode = LowerBlock(static_cast<BlockNode *>(stmt));
        CHECK_FATAL(tmpBlockNode, "null ptr check");
        newblk->AppendStatementsFromBlock(tmpBlockNode);
        break;
      case OP_dassign: {
        LowerDassign(static_cast<DassignNode *>(stmt), newblk);
        break;
      }
      case OP_regassign: {
        LowerRegassign(static_cast<RegassignNode *>(stmt), newblk);
        break;
      }
      case OP_iassign: {
        LowerIassign(static_cast<IassignNode *>(stmt), newblk);
        break;
      }
      case OP_callassigned:
      case OP_virtualcallassigned:
      case OP_superclasscallassigned:
      case OP_interfacecallassigned:
      case OP_icallassigned:
      case OP_intrinsiccallassigned:
      case OP_xintrinsiccallassigned:
      case OP_intrinsiccallwithtypeassigned:
        newblk->AppendStatementsFromBlock(LowerCallAssignedStmt(stmt));
        break;
      case OP_intrinsiccall:
      case OP_call:
      case OP_icall:
#if TARGARM || TARGAARCH64 || TARGARK || TARGRISCV64
        LowerCallStmt(stmt, nextstmt, newblk);
#else
        LowerStmt(stmt, newblk);
#endif
        break;
      case OP_return: {
#if TARGARM || TARGAARCH64 || TARGARK || TARGRISCV64
        if (GetCurrentFunc()->IsReturnStruct()) {
          newblk->AppendStatementsFromBlock(LowerReturnStruct(static_cast<NaryStmtNode *>(stmt)));
        } else
#endif
        {
          NaryStmtNode *retnode = static_cast<NaryStmtNode *>(stmt);
          if (retnode->nOpnd.size() == 0) {
            newblk->AddStatement(stmt);
          } else {
            tmpBlockNode = LowerReturn(retnode);
            CHECK_FATAL(tmpBlockNode, "null ptr check");
            newblk->AppendStatementsFromBlock(tmpBlockNode);
          }
        }
        break;
      }
      // Do not leave comment stmt to mmpl.
      case OP_comment:
        if (!IsTargetMMPL()) {
          newblk->AddStatement(stmt);
        }
        break;
      case OP_try:
      case OP_javatry:
      case OP_cpptry:
        LowerStmt(stmt, newblk);
        newblk->AddStatement(stmt);
        has_javatry_ = true;
        break;
      case OP_endtry:
        LowerStmt(stmt, newblk);
        newblk->AddStatement(stmt);
        break;
      case OP_catch:
      case OP_javacatch:
        LowerStmt(stmt, newblk);
        newblk->AddStatement(stmt);
        break;
      case OP_cppcatch:
        newblk->AddStatement(stmt);
        LowerCppCatch(newblk);
        break;
      case OP_throw:
        if (mirModule.IsJavaModule()) {
          if (GenerateExceptionHandlingCode()) {
            LowerStmt(stmt, newblk);
            newblk->AddStatement(stmt);
          } else {  // skip the throw statement
            ;
          }
        } else {
          LowerStmt(stmt, newblk);
          newblk->AddStatement(stmt);
        }
        break;
      case OP_syncenter:
      case OP_syncexit: {
        LowerStmt(stmt, newblk);
        StmtNode *tmp = LowerSyncEnterSyncExit(stmt);
        CHECK_FATAL(tmp, "null ptr check");
        newblk->AddStatement(tmp);
        break;
      }
      default:
        LowerStmt(stmt, newblk);
        newblk->AddStatement(stmt);
        break;
    }
  } while (nextstmt);
  return newblk;
}

StmtNode *BELowerer::LowerCall(CallNode *callnode, StmtNode *&nextstmt /*in-out*/, BlockNode *newblk, MIRType *retty) {
  // call $foo(constVal u32 128)
  // dassign %jlt (dread agg %%retval)
  bool isarraystore = false;

  if (callnode->op == OP_call) {
    MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callnode->puIdx);
    if (calleefunc->GetName() == GetIntrinsicFuncName(INTRN_MCCWrite) && callnode->Opnd(1)->op == OP_iaddrof) {
      IreadNode *addrexpr = static_cast<IreadNode *>(callnode->Opnd(1));
      if (addrexpr->Opnd(0)->op == OP_array) {
        isarraystore = true;
      }
    }
  }

  for (uint32 i = 0; i < callnode->nOpnd.size(); i++) {
    callnode->SetOpnd(LowerExpr(callnode, callnode, callnode->nOpnd[i], newblk), i);
  }

  if (isarraystore && checkloadstore) {
    MIRType *arraytype = nullptr;
    MIRType *valuetype = nullptr;
    bool needcheckstore = true;
    BaseNode *arraynode = callnode->Opnd(0);
    if (arraynode->op == OP_regread) {
      RegreadNode *rrnode = static_cast<RegreadNode *>(arraynode);
      MIRPreg *preg = mirModule.CurFunction()->pregTab->PregFromPregIdx(rrnode->regIdx);
      if (preg->IsRef()) {
        arraytype = preg->mirType;
      }
    }
    if (arraynode->op == OP_dread) {
      DreadNode *dnode = static_cast<DreadNode *>(arraynode);
      MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dnode->stIdx);
      arraytype = symbol->GetType();
    }
    MIRType *arrayelemtype = nullptr;
    if (arraytype) {
      MIRType *sttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(arraytype)->pointedTyIdx);
      while (kTypeJArray == sttype->typeKind) {
        MIRJarrayType *arraytype1 = static_cast<MIRJarrayType *>(sttype);
        MIRType *elemtype = arraytype1->GetElemType();
        if (elemtype->typeKind == kTypePointer) {
          sttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(elemtype)->pointedTyIdx);
        } else {
          sttype = elemtype;
        }
      }

      arrayelemtype = sttype;
    }

    BaseNode *valuenode = callnode->Opnd(2);
    if (valuenode->op == OP_regread) {
      RegreadNode *rrnode = static_cast<RegreadNode *>(valuenode);
      MIRPreg *preg = mirModule.CurFunction()->pregTab->PregFromPregIdx(rrnode->regIdx);

      if (preg->IsRef()) {
        valuetype = preg->mirType;
      }
    }
    if (valuenode->op == OP_dread) {
      DreadNode *dnode = static_cast<DreadNode *>(valuenode);
      MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dnode->stIdx);
      valuetype = symbol->GetType();
    }
    MIRType *valuerealtype = nullptr;
    if (valuetype) {
      MIRType *sttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(valuetype)->pointedTyIdx);
      while (kTypeJArray == sttype->typeKind) {
        MIRJarrayType *arraytype1 = static_cast<MIRJarrayType *>(sttype);
        MIRType *elemtype = arraytype1->GetElemType();
        if (elemtype->typeKind == kTypePointer) {
          sttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(elemtype)->pointedTyIdx);
        } else {
          sttype = elemtype;
        }
      }

      valuerealtype = sttype;
    }

    if (arrayelemtype && valuerealtype && arrayelemtype->typeKind == kTypeClass &&
        static_cast<MIRClassType *>(arrayelemtype)->IsFinal() && valuerealtype->typeKind == kTypeClass &&
        static_cast<MIRClassType *>(valuerealtype)->IsFinal() && valuerealtype->tyIdx == arrayelemtype->tyIdx) {
      needcheckstore = false;
    }

    if (needcheckstore) {
      MIRFunction *fn = mirModule.mirBuilder->GetOrCreateFunction(GetIntrinsicFuncName(INTRN_MCCReflectCheckArrayStore), TyIdx(PTY_void));
      MapleVector<BaseNode *> opnds(mirModule.CurFuncCodeMemPoolAllocator()->Adapter());
      opnds.push_back(callnode->Opnd(0));
      opnds.push_back(callnode->Opnd(2));
      StmtNode *checkstoreStmt = mirModule.mirBuilder->CreateStmtCall(fn->puIdx, opnds, OP_call);
      newblk->AddStatement(checkstoreStmt);
    }
  }

  DassignNode *dassignnode = nullptr;
  if (nextstmt && nextstmt->op == OP_dassign) {
    dassignnode = static_cast<DassignNode *>(nextstmt);
  }

  // if nextstmt is not a dassign stmt, return
  if (!dassignnode) {
    return callnode;
  }

  if (retty && becommon.type_size_table[retty->tyIdx.GetIdx()] <= 16) {
      // return structure fitting in one or two regs.
      return callnode;
  }

  MIRType *rettype = nullptr;
  if (callnode->op == OP_icall) {
    if (retty == nullptr) {
      return callnode;
    } else {
      rettype = retty;
    }
  }

  if (rettype == nullptr) {
    MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callnode->puIdx);
    rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(calleefunc->GetReturnTyIdx());
    if (calleefunc->IsReturnStruct() && rettype->primType == PTY_void) {
      MIRPtrType *prettype = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(calleefunc->formalDefVec[0].formalTyIdx));
      CHECK_FATAL(prettype, "null ptr check");
      rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(prettype->pointedTyIdx);
      CHECK_FATAL((rettype->typeKind == kTypeStruct || rettype->typeKind == kTypeUnion), "");
    }
  }

  // if return type is not of a struct, return
  if (rettype->typeKind != kTypeStruct && rettype->typeKind != kTypeUnion) {
    return callnode;
  }

  MIRSymbol *dsgnst = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dassignnode->stIdx);
  MIRStructType *structty = dynamic_cast<MIRStructType *>(dsgnst->GetType());
  if (!structty) {
    return callnode;
  }

  RegreadNode *regreadnode = nullptr;
  if (dassignnode->Opnd(0)->op == OP_regread) {
    regreadnode = static_cast<RegreadNode *>(dassignnode->Opnd(0));
  }
  if (!regreadnode) {
    return callnode;
  }
  if (regreadnode->regIdx != -kSregRetval0) {
    return callnode;
  }

  MapleVector<BaseNode *> newnopnd(mirModule.CurFuncCodeMemPoolAllocator()->Adapter());
  AddrofNode *addrofnode = mirModule.CurFuncCodeMemPool()->New<AddrofNode>(OP_addrof);
  addrofnode->primType = (SIZEOFPTR == 4 ? PTY_a32 : PTY_a64);
  addrofnode->stIdx = dsgnst->GetStIdx();
  if (callnode->op == OP_icall) {
    MapleVector<BaseNode *>::iterator ond;
    ond = callnode->nOpnd.begin();
    newnopnd.push_back(*ond);
    newnopnd.push_back(addrofnode);
    for (++ond; ond != callnode->nOpnd.end(); ++ond) {
      newnopnd.push_back(*ond);
    }
  } else {
    newnopnd.push_back(addrofnode);
    for (auto it : callnode->nOpnd) {
      newnopnd.push_back(it);
    }
  }
  callnode->nOpnd = newnopnd;
  callnode->numOpnds = callnode->nOpnd.size();
  CHECK_FATAL(nextstmt, "");
  nextstmt = nextstmt->GetNext();
  return callnode;
}

void BELowerer::LowerEntry(MIRFunction *func) {
  if (func->IsReturnStruct()) {
    MIRSymbol *retst = func->symTab->CreateSymbol(kScopeLocal);
    retst->storageClass = kScFormal;
    retst->sKind = kStVar;
    std::string retname(".return.");
    MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
    retname.append(funcSt->GetName());
    retst->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(retname));
    TyIdx tyidx = func->GetReturnTyIdx();
    MIRType *pointtype = becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyidx));

    retst->SetTyIdx(pointtype->tyIdx);
    FormalDef formalDef(retst, retst->GetType()->tyIdx, TypeAttrs());
    func->formalDefVec.insert(func->formalDefVec.begin(), formalDef);

    becommon.funcReturnType[func] = tyidx;

    MIRFuncType *newFuncType = static_cast<MIRFuncType*>(becommon.BeGetOrCreateFunctionType(TyIdx(PTY_void), func->funcType->paramTypeList, func->funcType->paramAttrsList));
    newFuncType->isVarArgs = func->funcType->isVarArgs;
    func->funcType = static_cast<MIRFuncType *>(GlobalTables::GetTypeTable().GetOrCreateMIRTypeNode(newFuncType));
  }
}

void BELowerer::LowerPseudoRegs(MIRFunction *func) {
  if (func->pregTab == nullptr) {
    return;
  }
  for (uint32 i = 1; i < func->pregTab->Size(); i++) {
    MIRPreg *ipr = func->pregTab->PregFromPregIdx(i);
    PrimType primType = ipr->primType;
    if (primType == PTY_ptr || primType == PTY_ref) {
      ipr->primType = LOWERED_PTR_TYPE;
#if !TARGARK
    } else if (primType == PTY_u1) {
      ipr->primType = PTY_u32;
#endif
    }
  }
}

void BELowerer::CleanupBranches(MIRFunction *func) {
  BlockNode *block = func->body;
  StmtNode *prev = nullptr, *next = nullptr;
  for (StmtNode *curr = block->GetFirst(); curr; curr = next) {
    next = curr->GetNext();
    if (next) {
      CHECK_FATAL(curr == next->GetPrev(), "unexpected node");
    }
    if (next && prev && curr->op == OP_goto) {
      // Skip until find a label.
      // Note that the CURRent 'goto' statement may be the last statement
      // when discounting comment statements.
      // Make sure we don't lose any comments.
      StmtNode *cmtB = nullptr;
      StmtNode *cmtE = nullptr;
      bool isCleanable = true;
      while (next && next->op != OP_label) {
        if (next->op == OP_javatry || next->op == OP_try || next->op == OP_endtry ||
            next->op == OP_javacatch || next->op == OP_catch || next->op == OP_cpptry || next->op == OP_cppcatch) {
          isCleanable = false;
          break;
        }
        next = next->GetNext();
      }
      if (next && isCleanable == false) {
        prev = next->GetPrev();
        continue;
      }

      next = curr->GetNext();

      while (next && next->op != OP_label) {
        if (next->op == OP_comment) {
          if (cmtB == nullptr) {
            cmtB = cmtE = next;
          } else {
            CHECK_FATAL(cmtE != nullptr, "cmt_e is null in BELowerer::CleanupBranches");
            cmtE->SetNext(next);
            next->SetPrev(cmtE);
            cmtE = next;
          }
        }
        next = next->GetNext();
      }

      curr->SetNext(next);

      if (next) {
        next->SetPrev(curr);
      }

      StmtNode *insertAfter = nullptr;

      if (next && (static_cast<GotoNode *>(curr))->offset == (static_cast<LabelNode *>(next))->labelIdx) {
        insertAfter = prev;
        prev->SetNext(next);  // skip goto statement (which is pointed by curr)
        next->SetPrev(prev);
        curr = next;        // make curr point to the label statement
        next = next->GetNext();  // advance next to the next statement of the label statement
      } else {
        insertAfter = curr;
      }

      // insert comments before 'curr'
      if (cmtB) {
        CHECK_FATAL(cmtE, "");
        StmtNode *iaNext = insertAfter->GetNext();
        if (iaNext) {
          iaNext->SetPrev(cmtE);
        }
        cmtE->SetNext(iaNext);

        insertAfter->SetNext(cmtB);
        cmtB->SetPrev(insertAfter);

        if (insertAfter == curr) {
          curr = cmtE;
        }
      }
      if (next == nullptr) {
        func->body->SetLast(curr);
      }
    }
    prev = curr;
  }
  CHECK_FATAL(func->body->GetLast() == prev, "");
}

struct bb_t {
  enum bbty_t {
    kBBPlain,
    kBBJavatry,
    kBBEndtry,
    kBBjavacatch,
  };
  /*
     = if stmt is a switch/rangegoto,
      succs gets defined, and condjmp_br == fallthru_br == nullptr.
     = otherwise, succs.size() ==0 &&
     1. for cond br inst, both condjmp_br and fallthru_br
        are defined.
     2. if bb ends with 'throw', both fields get nullptr.
     3. for the others, condjmp_br == nullptr && only
        fallthru_br is defined
   */
  bbty_t type;
  bb_t *br_condjmp;
  bb_t *br_fallthru;
  std::vector<bb_t *> succs;
  LabelIdx labidx;
  StmtNode *first_stmt;
  StmtNode *last_stmt;
  StmtNode *key_stmt;

 public:
  explicit bb_t(StmtNode *s, StmtNode *e)
    : type(kBBPlain),
      br_condjmp(nullptr),
      br_fallthru(nullptr),
      succs(),
      labidx(MIRLabelTable::kDummyLabel),
      first_stmt(s ? s : e),
      last_stmt(e),
      key_stmt(nullptr) {}

  void Extend(const StmtNode *s, StmtNode *e) {
    CHECK_FATAL(last_stmt && (s ? last_stmt->GetNext() == s : last_stmt->GetNext() == e), "");
    last_stmt = e;
  }

  void SetLabel(LabelIdx li) {
    labidx = li;
  }

  bool IsLabeled() const {
    return (labidx != MIRLabelTable::kDummyLabel);
  }

  LabelIdx GetLabel() const {
    return labidx;
  }

  void SetType(bbty_t t, StmtNode *k) {
    type = t;
    key_stmt = k;
  }

  bool IsJavatry() const {
    return type == kBBJavatry;
  }

  bool IsEndtry() const {
    return type == kBBEndtry;
  }

  bool IsJavacatch() const {
    return type == kBBjavacatch;
  }

#if DEBUG
  void dump(MIRModule *mod) {
    if (IsJavatry()) {
      LogInfo::MapleLogger() << "JavaTry" << std::endl;
    } else if (IsEndtry()) {
      LogInfo::MapleLogger() << "EndTry" << std::endl;
    } else if (IsJavacatch()) {
      LogInfo::MapleLogger() << "JavaCatch" << std::endl;
    } else {
      LogInfo::MapleLogger() << "Plain" << std::endl;
    }
    if (first_stmt) {
      first_stmt->Dump(mod, 0);
      LogInfo::MapleLogger() << std::endl;
      if (key_stmt) {
        key_stmt->Dump(mod, 0);
        LogInfo::MapleLogger() << std::endl;
      } else {
        LogInfo::MapleLogger() << "<<No-Key-Stmt>>" << std::endl;
      }
      last_stmt->Dump(mod, 0);
      LogInfo::MapleLogger() << std::endl;
    } else {
      LogInfo::MapleLogger() << "<<Empty>>" << std::endl;
    }
  }

#endif
};

static bb_t *LCreateNewBB(StmtNode *first, StmtNode *last, MIRModule &mod, std::vector<bb_t *> &bblist) {
  bb_t *newbb = mod.memPool->New<bb_t>(first, last);
  bblist.push_back(newbb);
  return newbb;
}

static bb_t *LFindTargetBBlock(LabelIdx idx, std::vector<bb_t *> &bbs) {
  for (auto &target : bbs) {
    if (target->labidx == idx) {
      return target;
    }
  }
  return nullptr;
}

// returns the first statement that is moved in into the javatry block.
// If none is moved in, nullptr is returned
static StmtNode *LMoveCondGotoIntoJavaTry(bb_t *jtbb, bb_t *condbrBb, std::vector<bb_t *> &labeledBbsInJavatry,
                                          MIRModule *mod) {
  StmtNode *firstStmtMovedIn = nullptr;
  std::vector<bb_t *> &bbs = labeledBbsInJavatry;
  /*  A work-around for potential java compiler bug
      the following is the front-end output.
      the first brtrue statement must be enclosed in the javatry-endtry block.
      for EH to correctly generate call site info.

      brtrue @label5 (le u1 i32 (dread i32 %Reg2_I, dread i32 %Reg1_I))
      javatry { @label8 @label9 @label10 @label11 @label12 }
      ...
     @label5
      dassign %Reg1_I 0 (cvt i32 i16 (constVal i16 50))
      brtrue @label6 (le u1 i32 (dread i32 %Reg2_I, dread i32 %Reg1_I))
      ...
     @label6
      callassigned &LEhMany_3B_7Cxxx_7C_28_29V () {}
      endtry
    */
  StmtNode *jtstmt = jtbb->key_stmt;
#if DEBUG
  StmtNode *js = jtbb->first_stmt;
  while (js->op != OP_javatry && js->op != OP_try) {
    js = js->GetNext();
  }
  CHECK_FATAL(js == jtstmt, "");
#endif
  CHECK_FATAL(condbrBb, "");

  StmtNode *ts = jtbb->first_stmt->GetPrev();
  while (ts && ts->op == OP_comment) {
    ts = ts->GetPrev();
  }

  if (ts && ts->IsCondBr()) {
    CHECK_FATAL(ts->GetNext() == jtbb->first_stmt, "");
    StmtNode *jtbbB = jtbb->first_stmt;
    // [ jtbb_b..jtstmt ]; either jtbb_b is a comment or jtbb_b == jtstmt
    LabelIdx id = static_cast<CondGotoNode *>(ts)->offset;
    for (auto &lbb : bbs) {
      if (lbb->labidx == id) {
        // this cond goto jumps into the javatry block;
        // let the javatry block enclose it
        // first find the preceding comment statements if any
        StmtNode *brS = ts;
        while (ts->GetPrev() && ts->GetPrev()->op == OP_comment) {
          ts = ts->GetPrev();
        }
        StmtNode *brbbB = ts;  // beginning statement of branch block
        // [ brbb_b..br_s ]; either brbb_b is a comment or brbb_b == br_s
        jtbbB->SetPrev(brbbB->GetPrev());
        if (brbbB->GetPrev()) {
          brbbB->GetPrev()->SetNext(jtbbB);
        }

        jtstmt->GetNext()->SetPrev(brS);
        brS->SetNext(jtstmt->GetNext());

        brbbB->SetPrev(jtstmt);
        jtstmt->SetNext(brbbB);

        condbrBb->last_stmt = jtbbB->GetPrev();

        CHECK_FATAL(condbrBb->br_fallthru == jtbb, "");
        condbrBb->br_fallthru = jtbb;
        condbrBb->br_condjmp = nullptr;

        firstStmtMovedIn = brbbB;

        break;
      }
    }
  }
  return firstStmtMovedIn;
}

#if DEBUG
static void lValidateStmtList(StmtNode *head, StmtNode *detached = nullptr) {
  static int nStmts = 0;
  int n = 0, m = 0;
  if (head == nullptr && detached == nullptr) {
    nStmts = 0;
    return;
  }
  for (StmtNode *s = head; s; s = s->GetNext()) {
    if (s->GetNext()) {
      CHECK_FATAL(s->GetNext()->GetPrev() == s, "");
    }
    if (s->GetPrev()) {
      CHECK_FATAL(s->GetPrev()->GetNext() == s, "");
    }
    ++n;
  }
  for (StmtNode *s = detached; s; s = s->GetNext()) {
    if (s->GetNext()) {
      CHECK_FATAL(s->GetNext()->GetPrev() == s, "");
    }
    if (s->GetPrev()) {
      CHECK_FATAL(s->GetPrev()->GetNext() == s, "");
    }
    ++m;
  }
  CHECK_FATAL(nStmts <= n + m, "");
  if (nStmts < n + m) {
    nStmts = n + m;
  }
}

#endif

void BELowerer::LowerCppCatch(BlockNode *blk) {
  MIRFunction *func = mirModule.CurFunction();
  GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName("__Exc_Ptr__");
  MIRSymbol *thrownValSym = func->symTab->GetSymbolFromStrIdx(strIdx);
  CHECK_FATAL(thrownValSym != nullptr, "LowerCppCatch: cannot find __Exc_Ptr__ symbol");
  strIdx = GlobalTables::GetStrTable().GetStrIdxFromName("__Exc_Filter__");
  MIRSymbol *filterSym = func->symTab->GetSymbolFromStrIdx(strIdx);
  CHECK_FATAL(filterSym != nullptr, "LowerCppCatch: cannot find __Exc_Filter__ symbol");
  RegreadNode *regread = mirModule.mirBuilder->CreateExprRegread(PTY_a64, -kSregRetval0);
  blk->AddStatement(mirModule.mirBuilder->CreateStmtDassign(thrownValSym, 0, regread));
  regread = mirModule.mirBuilder->CreateExprRegread(PTY_a64, -kSregRetval1);
  blk->AddStatement(mirModule.mirBuilder->CreateStmtDassign(filterSym, 0, regread));
}

void BELowerer::LowerJavaTryCatchBlocks(BlockNode *body) {
  if (!has_javatry_) {
    return;
  }

#if DEBUG
  lValidateStmtList(nullptr, nullptr);
#endif

  std::vector<bb_t *> bblist;
  std::vector<bb_t *> condbrBbs;
  std::vector<bb_t *> switchBbs;
  std::vector<bb_t *> labeledBbs;
  std::vector<bb_t *> enclosedBbs;
  typedef std::pair<bb_t *, bb_t *> bb_pair_t;
  std::vector<bb_pair_t> javatryBbs;
  std::vector<bb_t *> javacatchBbs;
  std::map<bb_t *, bb_t *> prevBbOfJavatry;

  CHECK_FATAL(body->GetFirst(), "");
  StmtNode *bodyFirst = body->GetFirst();
  StmtNode *next = bodyFirst;

  // comment block [ begin, end ]
  // We treat comment statements as if they are parts
  // of the immediately following non-comment statement
  StmtNode *commentB = nullptr, *commentE = nullptr;

  bb_t *curbb = nullptr;
  bb_t *lastBb = nullptr;
  bb_t *openJavatry = nullptr;

  // recover basic blocks
  for (StmtNode *stmt = next; stmt; stmt = next) {
    next = stmt->GetNext();

    if (stmt->op == OP_comment) {
      if (commentB == nullptr) {
        commentB = commentE = stmt;
      } else {
        CHECK_FATAL(commentE && commentE->GetNext() == stmt, "");
        commentE = stmt;
      }
      continue;
    }

    CHECK_FATAL(stmt->op != OP_comment, "");
    CHECK_FATAL(commentB == nullptr || (commentE != nullptr && commentE->GetNext() == stmt), "");

    if (curbb) {
      if (stmt->op != OP_label && stmt->op != OP_javatry && stmt->op != OP_try && stmt->op != OP_endtry && stmt->op != OP_cpptry) {
        curbb->Extend(commentB, stmt);
      } else {
        // java catch blockes always start with a label (i.e., OP_javacatch)
        CHECK_FATAL(curbb->br_condjmp == nullptr && curbb->br_fallthru == nullptr, "");
        // a 'label' statement starts a new basic block
        bb_t *newbb = LCreateNewBB(commentB, stmt, mirModule, bblist);
        // if the immediately preceding statement (discounting comments)
        // was throw, goto or return, curbb is to be reset to nullptr,
        // so the control  won't come here.
        curbb->br_fallthru = newbb;
        curbb = newbb;
      }
    } else {
      // start a new basic block with 'comment_b -- stmt'
      curbb = LCreateNewBB(commentB, stmt, mirModule, bblist);
      if (lastBb) {
        Opcode lastbbLaststmtOp = lastBb->last_stmt->op;
        if (lastBb->last_stmt->IsCondBr() || lastbbLaststmtOp == OP_endtry) {
          lastBb->br_fallthru = curbb;
        }
        // else don't connect curbb to last_bb
      }
    }
    commentB = commentE = nullptr;

    switch (stmt->op) {
      case OP_throw:
      case OP_return:
      case OP_goto:
        // start a new bb at the next stmt
        lastBb = curbb;
        curbb = nullptr;
        break;
      case OP_label: {
        LabelNode *labelStmt = static_cast<LabelNode *>(stmt);
        labeledBbs.push_back(curbb);
        curbb->SetLabel((LabelIdx)labelStmt->labelIdx);
      } break;
      case OP_brtrue:
      case OP_brfalse:
        condbrBbs.push_back(curbb);
        lastBb = curbb;
        curbb = nullptr;
        break;
      case OP_switch:
        switchBbs.push_back(curbb);
        lastBb = curbb;
        curbb = nullptr;
        break;
      /*
         We deal javatry and endtry slightly differently.
         javatry begins a basic block which includes the
         javatry statement and the subsequent statements
         up to one that results in non-sequential control
         transfer such as unconditional/conditional branches.

         endtry will create its own basic block which contains
         the endtry statement and nothing else.
       */
      case OP_try:
      case OP_javatry:
      case OP_endtry: {
        CHECK_FATAL(curbb, "");  // because a label statement is inserted at the function entry
        CHECK_FATAL(curbb->br_condjmp == nullptr && curbb->br_fallthru == nullptr, "");
        CHECK_FATAL(curbb->last_stmt->op == stmt->op, "");
        if (stmt->op == OP_javatry || stmt->op == OP_try) {
          CHECK_FATAL(openJavatry == nullptr, "javatrys are not expected to be nested");
          curbb->SetType(bb_t::kBBJavatry, stmt);
          openJavatry = curbb;
          prevBbOfJavatry[openJavatry] = lastBb;
        } else {
          javatryBbs.push_back(bb_pair_t(openJavatry, curbb));
          openJavatry = nullptr;
          curbb->SetType(bb_t::kBBEndtry, stmt);
          lastBb = curbb;
          curbb = nullptr;
        }
        break;
      }
      case OP_catch:
      case OP_javacatch: {
#if DEBUG
        StmtNode *ss = stmt->GetPrev();
        while (ss && ss->op == OP_comment) {
          ss = ss->GetPrev();
        }
        CHECK_FATAL(ss && ss->op == OP_label, "");
        for (auto &tb : javacatchBbs) {
          CHECK_FATAL(tb != curbb, "");
        }
#endif
        javacatchBbs.push_back(curbb);
        curbb->SetType(bb_t::kBBjavacatch, stmt);
        break;
      }
      case OP_block:
        CHECK_FATAL(0, "");
      default:
        break;
    }
  }

  for (auto &cbbb : condbrBbs) {
    CHECK_FATAL(cbbb->last_stmt->IsCondBr(), "");
    CondGotoNode *s = static_cast<CondGotoNode *>(cbbb->last_stmt);
    cbbb->br_condjmp = LFindTargetBBlock((LabelIdx)s->offset, labeledBbs);
  }

  for (auto &swbb : switchBbs) {
    CHECK_FATAL(swbb->last_stmt->op == OP_switch, "");
    SwitchNode *ss = static_cast<SwitchNode *>(swbb->last_stmt);

    swbb->succs.push_back(LFindTargetBBlock(ss->defaultLabel, labeledBbs));
    for (auto &cp : ss->switchTable) {
      swbb->succs.push_back(LFindTargetBBlock(cp.second, labeledBbs));
    }
  }

  /*
     We want to place catch blocks so that they don't
     come before any of java trys that refer to them.
     In order to do that, we take advantage of the fact
     that the mpl. source we get is already flattened and
     no java-try-end-try block is enclosed in any other
     java-try-end-try block.
     Note that bblist contains basic blocks in the order
     they appear in the mpl file.
     We process each bb in bblist from the front to the end,
     and while doing so, we maintain a list of catch blocks
     we have seen.
     When we get to an end-try block, we examine each
     catch block label it has (offsets), and if we find any
     catch block in the "seen" list, we move the block after
     the end-try block. Note that we need to find a basic block
     which does not have 'br_fallthru' control path.
     (Appending the catch block to any basic block that has
     the 'br_fallthru' control path will alter the program semantics)
   */
  std::map<StmtNode *, bb_t *> firstStmtToBbMap;
  for (auto &bb : bblist) {
    firstStmtToBbMap[bb->first_stmt] = bb;
  }

  std::vector<bb_t *> javacatchesSeenSoFar;

  CHECK_FATAL(!openJavatry, "");
  for (auto &bb : bblist) {
    if (bb->IsJavacatch() && !openJavatry) {
      // Add to the list of javacatch blocks seen so far.
      javacatchesSeenSoFar.push_back(bb);
    }
    bool bodyEndWithEndtry = false;
    if (!openJavatry) {
      if (bb->IsJavatry()) {
        StmtNode *firstNonCommentStmt = bb->first_stmt;
        while (firstNonCommentStmt && firstNonCommentStmt->op == OP_comment) {
          firstNonCommentStmt = firstNonCommentStmt->GetNext();
        }
        CHECK_FATAL(((bb->last_stmt->op != OP_javatry && bb->last_stmt->op != OP_try && bb->last_stmt->op != OP_cpptry) ||
                ((bb->last_stmt->op == OP_javatry || bb->last_stmt->op == OP_try || bb->last_stmt->op == OP_cpptry) && bb->last_stmt == firstNonCommentStmt)) ||
                 !GenerateExceptionHandlingCode(),
               "");
        // prepare for processing a java try block
        openJavatry = bb;
        enclosedBbs.clear();
      }
      continue;
    }

    // We should have not a javatry block enclosed in another java try block!!
    CHECK_FATAL(!bb->IsJavatry(), "");
    if (!bb->IsEndtry()) {
      enclosedBbs.push_back(bb);
    } else {
      bb_t *endtryBb = bb;
      if (endtryBb->last_stmt == body->GetLast()) {
        bodyEndWithEndtry = true;
      }
      StmtNode *jtstmt = openJavatry->key_stmt;
      CHECK_FATAL(jtstmt->op == OP_javatry || jtstmt->op == OP_try || jtstmt->op == OP_cpptry, "");
      std::vector<bb_t *> bbsToRelocate;
      std::vector<bb_t *> labeledBbsInJavatry;
#if DEBUG
      for (unsigned int i = 0; i < enclosedBbs.size(); ++i) {
        CHECK_FATAL(enclosedBbs[i], "");
      }
#endif
      for (unsigned int i = 0; i < enclosedBbs.size(); ++i) {
        bb_t *ebb = enclosedBbs[i];
        unsigned int nextEnclosedIdx = i + 1;
        if (!ebb) {
          continue;  // we may have removed the element
        }
        if (!ebb->IsLabeled()) {
          continue;
        }
        labeledBbsInJavatry.push_back(ebb);

        /*
           It seems the way a finally is associated with its javatry
           is to put the catch block inside the java-try-end-try block.
           So, keep the 'javacatch(void*)' in it.
         */
        LabelIdx label = ebb->labidx;
        bool found = false;
        for (auto &id : static_cast<TryNode *>(jtstmt)->offsets) {
          // if this labeled bb is a catch block,
          // remove it from the list of blocks enclosed in this try-block'
          if (label == id) {
            found = true;
            enclosedBbs[i] = nullptr;
            std::vector<bb_t *> currBbThread;
            // append it to the list of blocks placed after the end try block
            currBbThread.push_back(ebb);
            while (ebb->br_fallthru) {
              ebb = ebb->br_fallthru;
              nextEnclosedIdx++;
              CHECK_FATAL(!ebb->IsJavatry(), "");
              if (ebb->IsEndtry()) {
                CHECK_FATAL(ebb == endtryBb, "");
                break;
              }
              for (unsigned int j = 0; j < enclosedBbs.size(); ++j) {
                if (enclosedBbs[j] == ebb) {
                  enclosedBbs[j] = nullptr;
                  break;
                }
              }
              currBbThread.push_back(ebb);
            }

            if (!ebb->IsEndtry()) {
              for (auto &e : currBbThread) {
                bbsToRelocate.push_back(e);
              }
            } else {
              /*
                 We have the following case.
                  bb_head -> bb_1 -> .. bb_n -> endtry_bb -> succ

                 For this particular case, we swap endtry bb and curr_bb_thread
                 because the bblock that contains the endtry statement
                 does not contain any other statements!!
               */
              CHECK_FATAL(endtryBb->first_stmt->op == OP_comment || endtryBb->first_stmt->op == OP_endtry, "");
              CHECK_FATAL(endtryBb->last_stmt->op == OP_endtry, "");

              // we move endtry_bb before thread_head
              bb_t *threadHead = currBbThread.front();
              CHECK_FATAL(threadHead->first_stmt->GetPrev(), "");
              CHECK_FATAL(threadHead->first_stmt->op == OP_comment || threadHead->first_stmt->op == OP_label, "");
              CHECK_FATAL(threadHead->first_stmt->GetPrev()->GetNext() == threadHead->first_stmt, "");
              threadHead->first_stmt->GetPrev()->SetNext(endtryBb->first_stmt);
              endtryBb->first_stmt->SetPrev(threadHead->first_stmt->GetPrev());
              bb_t *threadTail = currBbThread.back();
              threadTail->last_stmt->SetNext(endtryBb->last_stmt->GetNext());
              if (endtryBb->last_stmt->GetNext()) {
                endtryBb->last_stmt->GetNext()->SetPrev(threadTail->last_stmt);
              }
              endtryBb->last_stmt->SetNext(threadHead->first_stmt);

              CHECK_FATAL(endtryBb->br_condjmp == nullptr, "");
              if (threadTail->br_fallthru) {
                threadTail->br_fallthru = firstStmtToBbMap[threadTail->last_stmt->GetNext()];
              }
              endtryBb->br_fallthru = nullptr;
              if (bodyEndWithEndtry) {
                body->SetLast(threadTail->last_stmt);
              }
            }
            break;
          }
        }
        // fill cur_bb_thread until meet the next javacatch
        if (!found && ebb->IsJavacatch()) {
          enclosedBbs[i] = nullptr;
          std::vector<bb_t *> currBbThread;
          bb_t *nextBbThreadHead = nullptr;
          bool isFirstTime = true;
          bool hasMoveEndtry = false;
          do {
            if (nextBbThreadHead) {
              isFirstTime = false;
            }
            nextBbThreadHead = nullptr;
            currBbThread.clear();
            currBbThread.push_back(ebb);
            while (ebb->br_fallthru) {
              ebb = ebb->br_fallthru;
              nextEnclosedIdx++;
              if (ebb->IsEndtry()) {
                CHECK_FATAL(ebb == endtryBb, "");
                break;
              }

              for (unsigned int j = 0; j < enclosedBbs.size(); ++j) {
                if (enclosedBbs[j] == ebb) {
                  enclosedBbs[j] = nullptr;
                  break;
                }
              }
              if (ebb->IsJavacatch()) {
                nextBbThreadHead = ebb;
                break;
              }
              currBbThread.push_back(ebb);
            }

            if (nextBbThreadHead == nullptr && !ebb->br_fallthru && ebb != endtryBb &&
                nextEnclosedIdx < enclosedBbs.size() && enclosedBbs[nextEnclosedIdx]) {
              /*
                 Using a loop to find the next_bb_thread_head when it's a catch_BB or a normal_BB which
                 is after a catch_BB. Other condition, push_back into the curr_bb_thread.
               */
              do {
                ebb = enclosedBbs[nextEnclosedIdx];
                enclosedBbs[nextEnclosedIdx++] = nullptr;
                bb_t *head = currBbThread.front();
                if (head->IsJavacatch() || ebb->IsJavacatch()) {
                  nextBbThreadHead = ebb;
                  break;
                }
                currBbThread.push_back(ebb);
              } while (nextEnclosedIdx < enclosedBbs.size());
            }

            /*
               Wrap this catch block with javatry-endtry block
             */
            for (auto &e : currBbThread) {
              CHECK_FATAL(!e->IsJavatry(), "");
            }
            {
              bb_t *threadHead = currBbThread.front();
              if (threadHead->IsJavacatch()) {
                CHECK_FATAL(threadHead->IsJavacatch(), "");
                StmtNode *jcstmt = threadHead->key_stmt;
                CHECK_FATAL(jcstmt->GetNext(), "");
                TryNode *jtcopy = static_cast<TryNode *>(jtstmt)->CloneTree(&mirModule);
                jtcopy->SetNext(jcstmt->GetNext());
                jtcopy->SetPrev(jcstmt);
                jcstmt->GetNext()->SetPrev(jtcopy);
                jcstmt->SetNext(jtcopy);

                bb_t *threadTail = currBbThread.back();

                // for this endtry stmt, we don't need to create a basic block
                StmtNode *newEndtry = endtryBb->key_stmt->CloneTree(&mirModule);
                newEndtry->SetPrev(threadTail->last_stmt);
                newEndtry->SetNext(threadTail->last_stmt->GetNext());
                if (bodyEndWithEndtry && hasMoveEndtry) {
                  if (threadTail->last_stmt->GetNext()) {
                    threadTail->last_stmt->GetNext()->SetPrev(newEndtry);
                  }
                } else {
                  CHECK_FATAL(threadTail->last_stmt->GetNext(), "");
                  threadTail->last_stmt->GetNext()->SetPrev(newEndtry);
                }
                threadTail->last_stmt->SetNext(newEndtry);

                threadTail->last_stmt = newEndtry;
                // if moved endtry_bb before thread_head, judge whether next thread exists.
                if (hasMoveEndtry && !nextBbThreadHead) {
                  body->SetLast(threadTail->last_stmt);
                }
              } else {
                /* For cases javatry->javacatch->normal_bb->normal_bb->endtry*/
                // Combine normal bb first.
                while (nextEnclosedIdx < enclosedBbs.size()) {
                  if (nextBbThreadHead != nullptr) {
                    if (nextBbThreadHead->IsJavacatch()) {
                      break;
                    }
                  }
                  bb_t *nextEbb = enclosedBbs[nextEnclosedIdx];
                  enclosedBbs[nextEnclosedIdx++] = nullptr;
                  CHECK_FATAL(nextEbb != endtryBb, "");
                  if (nextEbb->IsJavacatch()) {
                    nextBbThreadHead = nextEbb;
                    break;
                  }
                  currBbThread.push_back(nextEbb);
                }
                // normal bb.
                StmtNode *stmt = threadHead->first_stmt;

                TryNode *jtcopy = static_cast<TryNode *>(jtstmt)->CloneTree(&mirModule);
                jtcopy->SetNext(stmt);
                jtcopy->SetPrev(stmt->GetPrev());
                stmt->GetPrev()->SetNext(jtcopy);
                stmt->SetPrev(jtcopy);
                threadHead->first_stmt = jtcopy;

                bb_t *threadTail = currBbThread.back();

                // for this endtry stmt, we don't need to create a basic block
                StmtNode *newEndtry = endtryBb->key_stmt->CloneTree(&mirModule);
                newEndtry->SetPrev(threadTail->last_stmt);
                newEndtry->SetNext(threadTail->last_stmt->GetNext());
                if (bodyEndWithEndtry && hasMoveEndtry) {
                  if (threadTail->last_stmt->GetNext()) {
                    threadTail->last_stmt->GetNext()->SetPrev(newEndtry);
                  }
                } else {
                  CHECK_FATAL(threadTail->last_stmt->GetNext(), "");
                  threadTail->last_stmt->GetNext()->SetPrev(newEndtry);
                }
                threadTail->last_stmt->SetNext(newEndtry);

                threadTail->last_stmt = newEndtry;
                if (hasMoveEndtry && !nextBbThreadHead) {
                  body->SetLast(threadTail->last_stmt);
                }
              }
            }
            if (isFirstTime) {
              /*
                 We have the following case.
                  bb_head -> bb_1 -> .. bb_n -> endtry_bb -> succ

                 For this particular case, we swap endtry bb and curr_bb_thread
                 because the bblock that contains the endtry statement
                 does not contain any other statements!!
               */
              CHECK_FATAL(endtryBb->first_stmt->op == OP_comment || endtryBb->first_stmt->op == OP_endtry, "");
              CHECK_FATAL(endtryBb->last_stmt->op == OP_endtry, "");

              // we move endtry_bb before bb_head
              bb_t *threadHead = currBbThread.front();
              CHECK_FATAL(threadHead->first_stmt->GetPrev(), "");
              CHECK_FATAL(threadHead->first_stmt->op == OP_comment || threadHead->first_stmt->op == OP_label, "");
              CHECK_FATAL(threadHead->first_stmt->GetPrev()->GetNext() == threadHead->first_stmt, "");

              endtryBb->first_stmt->GetPrev()->SetNext(endtryBb->last_stmt->GetNext());
              if (endtryBb->last_stmt->GetNext()) {
                endtryBb->last_stmt->GetNext()->SetPrev(endtryBb->first_stmt->GetPrev());
              }

              threadHead->first_stmt->GetPrev()->SetNext(endtryBb->first_stmt);
              endtryBb->first_stmt->SetPrev(threadHead->first_stmt->GetPrev());

              endtryBb->last_stmt->SetNext(threadHead->first_stmt);
              threadHead->first_stmt->SetPrev(endtryBb->last_stmt);

              CHECK_FATAL(endtryBb->br_condjmp == nullptr, "");
              endtryBb->br_fallthru = nullptr;
              if (bodyEndWithEndtry) {
                hasMoveEndtry = true;
                if (!nextBbThreadHead) {
                  body->SetLast(currBbThread.back()->last_stmt);
                }
              }
            }
          } while (nextBbThreadHead != nullptr);
        }
      }

      // Now, connect the remaining ones again
      // n_enclosed_bbs includes 'nullptr's (i.e., deleted entries)
      int nEnclosedBbs = enclosedBbs.size();
      {
        int k = 0;
        while (k < nEnclosedBbs && !enclosedBbs[k]) {
          ++k;
        }

        if (k < nEnclosedBbs) {
          bb_t *prevbb = enclosedBbs[k];

          openJavatry->last_stmt->SetNext(prevbb->first_stmt);
          prevbb->first_stmt->SetPrev(openJavatry->last_stmt);

          for (++k; k < nEnclosedBbs; ++k) {
            bb_t *tbb = enclosedBbs[k];
            if (!tbb) {
              continue;
            }
            prevbb->last_stmt->SetNext(tbb->first_stmt);
            tbb->first_stmt->SetPrev(prevbb->last_stmt);
            prevbb = tbb;
          }

          prevbb->last_stmt->SetNext(endtryBb->first_stmt);
          endtryBb->first_stmt->SetPrev(prevbb->last_stmt);
        } else {
          openJavatry->last_stmt->SetNext(endtryBb->first_stmt);
          endtryBb->first_stmt->SetPrev(openJavatry->last_stmt);
        }
      }

      bb_t *insertAfter = endtryBb;
      CHECK_FATAL(endtryBb->last_stmt->op == OP_endtry, "");
      bb_t *iaOpenJavatry = nullptr;
      while (insertAfter->br_fallthru || iaOpenJavatry != nullptr) {
        if (insertAfter->br_fallthru) {
          insertAfter = insertAfter->br_fallthru;
        } else {
          CHECK_FATAL(iaOpenJavatry != nullptr, "");
          insertAfter = firstStmtToBbMap[insertAfter->last_stmt->GetNext()];
          CHECK_FATAL(!insertAfter->IsJavatry(), "");
        }

        if (insertAfter->IsJavatry()) {
          iaOpenJavatry = insertAfter;
        } else if (insertAfter->IsEndtry()) {
          iaOpenJavatry = nullptr;
        }
      }

      {
        StmtNode *iaLast = insertAfter->last_stmt;
        CHECK_FATAL(iaLast, "");

        StmtNode *iaNext = iaLast->GetNext();
        if (!iaNext) {
          CHECK_FATAL(body->GetLast() == iaLast, "");
        }
        bb_t *prevbb = insertAfter;
        for (auto &rbb : bbsToRelocate) {
          prevbb->last_stmt->SetNext(rbb->first_stmt);
          rbb->first_stmt->SetPrev(prevbb->last_stmt);
          prevbb = rbb;
        }
        prevbb->last_stmt->SetNext(iaNext);
        if (iaNext) {
          iaNext->SetPrev(prevbb->last_stmt);
        } else {
          // !ia_next means we started with insert_after
          // that was the last bblock
          // Refer to the above CHECK_FATAL.
          body->SetLast(prevbb->last_stmt);
          body->GetLast()->SetNext(nullptr);
        }
      }

#if DEBUG
      CHECK_FATAL(body->GetLast()->GetNext() == nullptr, "");
      lValidateStmtList(bodyFirst);
#endif

      if (prevBbOfJavatry[openJavatry]) {
        StmtNode *firstStmtMovedIn =
          LMoveCondGotoIntoJavaTry(openJavatry, prevBbOfJavatry[openJavatry], labeledBbsInJavatry, &mirModule);
        if (firstStmtMovedIn == bodyFirst) {
          bodyFirst = openJavatry->first_stmt;
          prevBbOfJavatry[openJavatry] = nullptr;
        }
      }

      // Now, examine each offset attached to this javatry
      // and move any catch block that is not in 'bbs_to_relocate'
      // but in 'javacatches_seen_so_far'
      for (auto &id : static_cast<TryNode *>(jtstmt)->offsets) {
        bool myCatchBlock = false;
        for (auto &jcb : bbsToRelocate) {
          if (!jcb->IsLabeled()) {
            continue;
          }
          myCatchBlock = (id == jcb->GetLabel());
          if (myCatchBlock) {
            break;
          }
        }
        /*
           If the catch block is the one enclosed in this javatry-endtry block,
           we just relocated it above, so we don't need to consider it again
         */
        if (myCatchBlock) {
          continue;
        }

        CHECK_FATAL(body->GetLast()->GetNext() == nullptr, "");
        for (auto &jcb : javacatchesSeenSoFar) {
          CHECK_FATAL(jcb->IsLabeled(), "");
          if (id == jcb->GetLabel()) {
            /*
               Remove jcb and all of the blocks that are reachable
               by following br_fallthru.
               If we hit a javatry block, cut there, append an unconditional
               jump to it to the preceding bblock, and relocate them.
               We may need to insert a label in the javatry block
             */
            lastBb = jcb;
            while (lastBb->br_fallthru && !lastBb->br_fallthru->IsJavatry()) {
              lastBb = lastBb->br_fallthru;
            }

#if DEBUG
            lValidateStmtList(bodyFirst);
#endif

            if (lastBb->br_fallthru) {
              bb_t *jtbb = lastBb->br_fallthru;
              CHECK_FATAL(jtbb->IsJavatry(), "");
              if (!jtbb->IsLabeled()) {
                LabelIdx jtlabidx = mirModule.mirBuilder->CreateLabidx(mirModule.CurFunction());
                jtbb->SetLabel(jtlabidx);
                StmtNode *labelStmt = mirModule.mirBuilder->CreateStmtLabel(jtlabidx);
                bool adjustBbFirstStmt = (jtbb->key_stmt == jtbb->first_stmt);
                labelStmt->SetNext(jtbb->key_stmt);
                labelStmt->SetPrev(jtbb->key_stmt->GetPrev());
                CHECK_FATAL(jtbb->key_stmt->GetPrev(), "");
                jtbb->key_stmt->GetPrev()->SetNext(labelStmt);
                CHECK_FATAL(jtbb->key_stmt->GetNext(), "");
                jtbb->key_stmt->SetPrev(labelStmt);
                if (adjustBbFirstStmt) {
                  firstStmtToBbMap.erase(jtbb->first_stmt);
                  jtbb->first_stmt = labelStmt;
                  firstStmtToBbMap[jtbb->first_stmt] = jtbb;
                }
              }
              CHECK_FATAL(jtbb->IsLabeled(), "");
              CHECK_FATAL(lastBb->last_stmt->op != OP_goto, "");
              StmtNode *gotoStmt = mirModule.mirBuilder->CreateStmtGoto(OP_goto, jtbb->GetLabel());

              StmtNode *lastBbLastStmt = lastBb->last_stmt;
              gotoStmt->SetNext(lastBbLastStmt->GetNext());
              gotoStmt->SetPrev(lastBbLastStmt);
              if (lastBbLastStmt->GetNext()) {
                lastBbLastStmt->GetNext()->SetPrev(gotoStmt);
              }
              lastBbLastStmt->SetNext(gotoStmt);

              lastBb->last_stmt = gotoStmt;
              lastBb->br_fallthru = nullptr;

#if DEBUG
              CHECK_FATAL(body->GetLast()->GetNext() == nullptr, "");
              lValidateStmtList(bodyFirst);
#endif
            }

            // we want to remove [jcb .. last_bb], inclusively.
            if (jcb->first_stmt == body->GetFirst()) {
              body->SetFirst(lastBb->last_stmt->GetNext());
              body->GetFirst()->SetPrev(nullptr);
              lastBb->last_stmt->GetNext()->SetPrev(nullptr);
              bodyFirst = body->GetFirst();
            } else {
              CHECK_FATAL(jcb->first_stmt->GetPrev(), "");
              CHECK_FATAL(jcb->first_stmt->GetPrev()->GetNext() == jcb->first_stmt, "");
              if (lastBb->last_stmt->GetNext()) {
                jcb->first_stmt->GetPrev()->SetNext(lastBb->last_stmt->GetNext());
                lastBb->last_stmt->GetNext()->SetPrev(jcb->first_stmt->GetPrev());
              } else {
                CHECK_FATAL(lastBb->last_stmt == body->GetLast(), "");
                body->SetLast(jcb->first_stmt->GetPrev());
                body->GetLast()->SetNext(nullptr);
                jcb->first_stmt->GetPrev()->SetNext(nullptr);
              }
            }
            jcb->first_stmt->SetPrev(nullptr);
            lastBb->last_stmt->SetNext(nullptr);

#if DEBUG
            CHECK_FATAL(body->GetLast()->GetNext() == nullptr, "");
            lValidateStmtList(body->GetFirst(), jcb->first_stmt);
#endif

            // append it (i.e., [jcb->first_stmt .. last_bb->last_stmt]) after insert_after
            CHECK_FATAL(insertAfter->br_fallthru == nullptr, "");
            if (insertAfter->last_stmt == body->GetLast()) {
              CHECK_FATAL(insertAfter->last_stmt->GetNext() == nullptr, "");
            }

            jcb->first_stmt->SetPrev(insertAfter->last_stmt);
            lastBb->last_stmt->SetNext(insertAfter->last_stmt->GetNext());

            CHECK_FATAL(body->GetLast()->GetNext() == nullptr, "");

            if (insertAfter->last_stmt->GetNext()) {
              insertAfter->last_stmt->GetNext()->SetPrev(lastBb->last_stmt);
              CHECK_FATAL(body->GetLast()->GetNext() == nullptr, "");
            } else {
              // note that we have a single BlockNode that contains
              // all the instructions of a method. What that means is each instruction's
              // next is not nullptr except for the very last instruction.
              // insert_after->last_stmt->next == nullptr, means insert_after->last_stmt
              // is indeed the last instruction, and we are moving instructions of 'last_bb'
              // after it. Thus, we need to fix the BlockNode's last field.
              body->SetLast(lastBb->last_stmt);
              CHECK_FATAL(body->GetLast()->GetNext() == nullptr, "");
            }
            insertAfter->last_stmt->SetNext(jcb->first_stmt);
            if (jcb->first_stmt->GetPrev()) {
              CHECK_FATAL(jcb->first_stmt->GetPrev()->GetNext() == jcb->first_stmt, "");
            }
            if (lastBb->last_stmt->GetNext()) {
              CHECK_FATAL(lastBb->last_stmt->GetNext()->GetPrev() == lastBb->last_stmt, "");
            }

            CHECK_FATAL(body->GetLast()->GetNext() == nullptr, "");
          }
        }
      }

      // close the javatry that is open
      openJavatry = nullptr;
    }
#if DEBUG
    CHECK_FATAL(body->GetLast()->GetNext() == nullptr, "");
    lValidateStmtList(bodyFirst);
#endif
  }

  body->SetFirst(bodyFirst);

#if DEBUG
  {
    StmtNode *openJt = nullptr;
    for (StmtNode *stmt = body->GetFirst(); stmt; stmt = stmt->GetNext()) {
      switch (stmt->op) {
        case OP_try:
        case OP_javatry:
        case OP_cpptry:
          openJt = stmt;
          break;
        case OP_endtry:
          openJt = nullptr;
          break;
        case OP_catch:
        case OP_javacatch:
          if (openJt) {
            CatchNode *jcn = static_cast<CatchNode *>(stmt);
            const MapleVector<TyIdx> &exceptiontyidxvec = jcn->exceptionTyIdxVec;
            for (auto etyit : exceptiontyidxvec) {
              MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(etyit);
              MIRPtrType *ptr = static_cast<MIRPtrType *>(type);
              type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptr->pointedTyIdx);
              CHECK_FATAL(type->GetPrimType() == PTY_void, "");
            }
          }
          break;
        default:
          break;
      }
    }
  }
#endif
}

void BELowerer::CheckFormalInReg(MIRFunction *func) {
  if (becommon.optim_level == 0) {
    uint32 i = 0;
    for (FormalDef fd : func->formalDefVec) {
      i++;
      MIRSymbol *s = fd.formalSym;
      if (s != nullptr) {
        if (s->sKind == kStPreg) {
          CHECK_FATAL(0, "Cannot handle promoting params to regs with -O0");
        }
      }
    }
  }
}

void BELowerer::LowerFunc(MIRFunction *func) {
  CheckFormalInReg(func);
  SetCurrentFunc(func);
  has_javatry_ = false;
  LowerEntry(func);
  LowerPseudoRegs(func);
  if (mirModule.IsJavaModule() && func->body->GetFirst()) {
    LowerFormalParameters(func);
  }
  BlockNode *origbody = func->body;
  CHECK_FATAL(origbody, "");

  locksymbol.clear();
  lockreg.clear();
  BlockNode *newbody = LowerBlock(origbody);
  becommon.FinalizeTypeTable();
  func->body = newbody;
  if (need_branch_cleanup_) {
    CleanupBranches(func);
  }
  if (mirModule.IsJavaModule() && func->body->GetFirst()) {
#if DEBUG
    BlockNode *blk = GetNewEntryBlock();
    if (blk && blk->GetFirst()) {
      BlockNode *entryBlk = func->body;
      CHECK_FATAL(blk->GetFirst()->GetPrev() == nullptr, "");
      CHECK_FATAL(entryBlk->GetFirst()->GetPrev() == nullptr, "");
      blk->GetLast()->SetNext(entryBlk->GetFirst());
      entryBlk->GetFirst()->SetPrev(blk->GetLast());

      entryBlk->SetFirst(blk->GetFirst());

      blk->ResetBlock();
    }
#endif

    if (GenerateExceptionHandlingCode()) {
      LowerJavaTryCatchBlocks(func->body);
    }
  }
}

}  // namespace maplebe
