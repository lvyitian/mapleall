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

#include "cg_lowerer.h"
#include "mir_symbol.h"
#include "mir_function.h"
#include "mir_builder.h"
#include <cmath>
#include <string>
#include <algorithm>
#include <cinttypes>
#include "aarch64_rt_support.h"
#include "cg_option.h"
#include "special_func.h"
#include "name_mangler.h"

namespace maplebe {

using namespace maple;
using namespace std;

inline bool IsAccessingTheSameMemoryLocation(const DassignNode *dassign, RegreadNode *rread, const CGLowerer *cglowerer) {
  StIdx stIdx = cglowerer->GetSymbolReferredToByPseudoRegister(rread->regIdx);
  return (dassign->stIdx == stIdx && dassign->fieldID == 0);
}

inline bool IsAccessingTheSameMemoryLocation(const DassignNode *dassign, const DreadNode *dread) {
  return (dassign->stIdx == dread->stIdx && dassign->fieldID == dread->fieldID);
}

inline bool IsDassignNOP(DassignNode *dassign) {
  if (dassign->GetRhs()->op == OP_dread) {
    return IsAccessingTheSameMemoryLocation(dassign, static_cast<DreadNode *>(dassign->GetRhs()));
  } else {
    return false;
  }
}

inline bool IsConstvalZero(BaseNode *n) {
  return (n->op == OP_constval && static_cast<ConstvalNode *>(n)->constVal->IsZero());
}

#define NEXT_ID(x) (x + 1)
#define INTRN_JAVA_SYNC_ENTER_0 NEXT_ID(INTRN_LAST)
#define INTRN_JAVA_SYNC_ENTER_1 NEXT_ID(INTRN_JAVA_SYNC_ENTER_0)
#define INTRN_JAVA_SYNC_ENTER_2 NEXT_ID(INTRN_JAVA_SYNC_ENTER_1)
#define INTRN_JAVA_SYNC_ENTER_3 NEXT_ID(INTRN_JAVA_SYNC_ENTER_2)
#define INTRN_JAVA_SYNC_EXIT NEXT_ID(INTRN_JAVA_SYNC_ENTER_3)

static MIRIntrinsicID cgBuiltins[] = {
  INTRN_JAVA_ARRAY_LENGTH,
  INTRN_JAVA_ARRAY_FILL,
  INTRN_JAVA_CHECK_CAST,
  INTRN_JAVA_CONST_CLASS,
  INTRN_JAVA_INSTANCE_OF,
  INTRN_JAVA_INTERFACE_CALL,
  INTRN_JAVA_POLYMORPHIC_CALL,
  INTRN_JAVA_GET_CLASS,
  INTRN_MPL_SET_CLASS,
  INTRN_MPL_MEMSET_LOCALVAR,
};

vector<pair<CGLowerer::builtin_func_id_t, PUIdx>> CGLowerer::builtinFuncIds;
unordered_map<IntrinDesc*, PUIdx> CGLowerer::intrinFuncIds;

static uint64_t GetWellKnownFrameWorksClassFlag(std::string className) {
  for (auto it = wellKnownFrameWorksClass.begin(); it != wellKnownFrameWorksClass.end(); ++it) {
    if (className == (*it).first) {
      return (*it).second;
    }
  }
  return 0;
}

void CGLowerer::InsertVarForReturn(MIRFunction *mirFunc, vector<StmtNode *> &retstmtvec) {
  BlockNode *funcbody = mirFunc->body;
  MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(mirFunc->GetReturnTyIdx());
  StmtNode *newretstmt = nullptr;
  MIRSymbol *tmpst = nullptr;
  if (funcbody->GetLast()->op == OP_return) {  // going to use this one as the final one
    newretstmt = funcbody->GetLast();
    NaryStmtNode *narystmt = static_cast<NaryStmtNode *>(newretstmt);
    if (narystmt->NumOpnds() > 0) {
      CHECK_FATAL(narystmt->NumOpnds() == 1, "");
      BaseNode *opnd0 = narystmt->nOpnd[0];
      if (opnd0->op == OP_dread) {
        tmpst = mirFunc->GetLocalOrGlobalSymbol(static_cast<DreadNode *>(opnd0)->stIdx);
      } else {  // create a new regassign and put the regread as return
        tmpst = mirbuilder->CreateLocalDecl("rettmp", rettype);
        DassignNode *dssnode = mirbuilder->CreateStmtDassign(tmpst, 0, opnd0);
        funcbody->InsertBefore(newretstmt, dssnode);
        CHECK(narystmt->nOpnd.size() > 0, "index out of range in CGLowerer::InsertVarForReturn");
        narystmt->nOpnd[0] = mirbuilder->CreateExprDread(rettype, tmpst);
      }
    }
  } else {
    BaseNode *retopnd = nullptr;
    PrimType primType = rettype->primType;
    if (primType != PTY_void) {
      CHECK_FATAL(primType != PTY_agg, "not yet support return agg or no need");
      tmpst = mirbuilder->CreateLocalDecl("rettmp", rettype);
      retopnd = mirbuilder->CreateExprDread(rettype, tmpst);
    }
    newretstmt = mirbuilder->CreateStmtReturn(retopnd);
    funcbody->InsertLast(newretstmt);
  }

  // insert the new return stmt and all other returns jump to here
  LabelIdx labelIdx = mirbuilder->CreateLabidx(mirFunc);
  LabelNode *labelnode = mirbuilder->CreateStmtLabel(labelIdx);
  funcbody->InsertBefore(newretstmt, labelnode);
  // replace the old return with an unconditional jump to labelnode
  for (uint32 i = 0; i < retstmtvec.size(); i++) {
    StmtNode *stmtnode = retstmtvec[i];
    if (stmtnode == newretstmt) {
      continue;
    }
    NaryStmtNode *narystmt = static_cast<NaryStmtNode *>(stmtnode);
    if (tmpst != nullptr) {
      // means the return has an opeand.
      CHECK_FATAL(narystmt->NumOpnds() > 0, "must has a return value");
      DassignNode *bkregassnode = mirbuilder->CreateStmtDassign(tmpst, 0, narystmt->nOpnd[0]);
      GotoNode *gotonode = mirbuilder->CreateStmtGoto(OP_goto, labelIdx);
      funcbody->InsertBefore(narystmt, bkregassnode);
      funcbody->ReplaceStmt1WithStmt2(narystmt, gotonode);
    } else {
      GotoNode *gotonode = mirbuilder->CreateStmtGoto(OP_goto, labelIdx);
      funcbody->ReplaceStmt1WithStmt2(narystmt, gotonode);
    }
  }
}

void CGLowerer::InsertPregForReturn(MIRFunction *mirFunc, vector<StmtNode *> &retstmtvec) {
  BlockNode *funcbody = mirFunc->body;
  MIRType *rettype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(mirFunc->GetReturnTyIdx());
  StmtNode *newretstmt = nullptr;
  PregIdx pregidx = 0;
  if (funcbody->GetLast()->op == OP_return) {  // going to use this one as the final one
    newretstmt = funcbody->GetLast();
    NaryStmtNode *narystmt = static_cast<NaryStmtNode *>(newretstmt);
    if (narystmt->NumOpnds() > 0) {
      CHECK_FATAL(narystmt->NumOpnds() == 1, "");
      BaseNode *opnd0 = narystmt->nOpnd[0];
      if (opnd0->op == OP_regread) {
        pregidx = static_cast<RegreadNode *>(opnd0)->regIdx;
      } else {  // create a new regassign and put the regread as return
        PrimType primType = opnd0->primType;
        pregidx = mirFunc->pregTab->CreatePreg(primType);
        RegassignNode *regassnode = mirbuilder->CreateStmtRegassign(primType, pregidx, opnd0);
        // put the regassnode before the return;
        funcbody->InsertBefore(newretstmt, regassnode);
        CHECK_FATAL(narystmt->nOpnd.size() > 0, "index out of range in CGLowerer::InsertPregForReturn");
        narystmt->nOpnd[0] = mirbuilder->CreateExprRegread(primType, pregidx);
      }
    }
  } else {
    BaseNode *retopnd = nullptr;
    PrimType primType = rettype->primType;
    if (primType != PTY_void) {
      CHECK_FATAL(primType != PTY_agg, "not yet support return agg or no need");
      pregidx = mirFunc->pregTab->CreatePreg(primType);
      retopnd = mirbuilder->CreateExprRegread(primType, pregidx);
    }
    newretstmt = mirbuilder->CreateStmtReturn(retopnd);
    funcbody->InsertLast(newretstmt);
  }

  // insert the new return stmt and all other returns jump to here
  LabelIdx labelIdx = mirbuilder->CreateLabidx(mirFunc);
  LabelNode *labelnode = mirbuilder->CreateStmtLabel(labelIdx);
  funcbody->InsertBefore(newretstmt, labelnode);
  // replace the old return with an unconditional jump to labelnode
  for (uint32 i = 0; i < retstmtvec.size(); i++) {
    StmtNode *stmtnode = retstmtvec[i];
    if (stmtnode == newretstmt) {
      continue;
    }
    NaryStmtNode *narystmt = static_cast<NaryStmtNode *>(stmtnode);
    if (pregidx != 0) {
      // means the return has an opeand.
      CHECK_FATAL(narystmt->NumOpnds() > 0, "must has a return value");
      RegassignNode *bkregassnode =
        mirbuilder->CreateStmtRegassign(narystmt->nOpnd[0]->primType, pregidx, narystmt->nOpnd[0]);
      GotoNode *gotonode = mirbuilder->CreateStmtGoto(OP_goto, labelIdx);
      funcbody->InsertBefore(narystmt, bkregassnode);
      funcbody->ReplaceStmt1WithStmt2(narystmt, gotonode);
    } else {
      GotoNode *gotonode = mirbuilder->CreateStmtGoto(OP_goto, labelIdx);
      funcbody->ReplaceStmt1WithStmt2(narystmt, gotonode);
    }
  }
}

//
void CGLowerer::InsertExit(MIRFunction *mirFunc) const {
  // Do merge later
  return;
}

MIRFunction *CGLowerer::RegisterFunctionVoidStarToVoid(builtin_func_id_t id, const char *name, const char *paramName) {
  MIRFunction *f = mirbuilder->GetOrCreateFunction(name, GlobalTables::GetTypeTable().GetVoid()->GetTypeIndex());
  MIRSymbol *fsym = f->GetFuncSymbol();
  fsym->SetStorageClass(kScExtern);
  MIRType *argty = GlobalTables::GetTypeTable().GetPtr();
  if (f->symTab == nullptr) {
    f->symTab = mirModule.memPool->New<MIRSymbolTable>(&mirModule.memPoolAllocator);
  }
  MIRSymbol *argst = f->symTab->CreateSymbol(kScopeLocal);
  argst->SetNameStridx(mirbuilder->GetOrCreateStringIndex(paramName));
  argst->SetTyIdx(argty->tyIdx);
  argst->storageClass = kScFormal;
  argst->sKind = kStVar;
  f->symTab->AddToStringSymbolMap(argst);
  f->AddArgument(argst);
  if (IsFuncSyncEnterOrExit(name)) {
    argst = f->symTab->CreateSymbol(kScopeLocal);
    argst->SetNameStridx(mirbuilder->GetOrCreateStringIndex("monitor_slot"));
    argst->SetTyIdx(argty->tyIdx);
    argst->storageClass = kScFormal;
    argst->sKind = kStVar;
    f->symTab->AddToStringSymbolMap(argst);
    f->AddArgument(argst);
  }

  builtinFuncIds.push_back(pair<builtin_func_id_t, PUIdx>(id, f->puIdx));
  return f;
}

void CGLowerer::RegisterBuiltIns() {
  for (uint i = 0; i < sizeof(cgBuiltins) / sizeof(cgBuiltins[0]); ++i) {
    builtin_func_id_t id = cgBuiltins[i];
    IntrinDesc &desc = IntrinDesc::intrintable[id];

    MIRFunction *f = mirbuilder->GetOrCreateFunction(GetIntrinsicFuncName(cgBuiltins[i]), GlobalTables::GetTypeTable().GetVoid()->GetTypeIndex());
    if (f->symTab == nullptr) {
      f->symTab = mirModule.memPool->New<MIRSymbolTable>(&mirModule.memPoolAllocator);
    }
    MIRSymbol *fsym = f->GetFuncSymbol();
    fsym->SetStorageClass(kScExtern);

    // return type
    MIRType *retty = desc.GetReturnType();
    CHECK_FATAL(retty, "null ptr check ");
    // use void* for PTY_dynany
    if (retty->GetPrimType() == PTY_dynany) {
      retty = GlobalTables::GetTypeTable().GetPtr();
    }
    f->SetReturnTyIdx(retty->GetTypeIndex());

    const char *params[IntrinDesc::kMaxArgsNum] = { "p0", "p1", "p2", "p3", "p4", "p5" };
    for (uint j = 0; j < IntrinDesc::kMaxArgsNum; ++j) {
      MIRType *argty = desc.GetArgType(j);
      if (!argty) {
        break;
      }
      // use void* for PTY_dynany
      if (argty->GetPrimType() == PTY_dynany) {
        argty = GlobalTables::GetTypeTable().GetPtr();
      }
      MIRSymbol *argst = f->symTab->CreateSymbol(kScopeLocal);
      argst->SetNameStridx(mirbuilder->GetOrCreateStringIndex(params[j]));
      argst->SetTyIdx(argty->tyIdx);
      argst->storageClass = kScFormal;
      argst->sKind = kStVar;
      f->symTab->AddToStringSymbolMap(argst);
      f->AddArgument(argst);
    }

    builtinFuncIds.push_back(pair<builtin_func_id_t, PUIdx>(id, f->puIdx));
  }

  // register __builtin_sync_enter
  RegisterFunctionVoidStarToVoid(INTRN_JAVA_SYNC_ENTER_0, GetIntrinsicFuncName(INTRN_MCCSyncEnterFast0).c_str(), "obj");
  RegisterFunctionVoidStarToVoid(INTRN_JAVA_SYNC_ENTER_1, GetIntrinsicFuncName(INTRN_MCCSyncEnterFast1).c_str(), "obj");
  RegisterFunctionVoidStarToVoid(INTRN_JAVA_SYNC_ENTER_2, GetIntrinsicFuncName(INTRN_MCCSyncEnterFast2).c_str(), "obj");
  RegisterFunctionVoidStarToVoid(INTRN_JAVA_SYNC_ENTER_3, GetIntrinsicFuncName(INTRN_MCCSyncEnterFast3).c_str(), "obj");
  // register __builtin_sync_exit
  RegisterFunctionVoidStarToVoid(INTRN_JAVA_SYNC_EXIT, GetIntrinsicFuncName(INTRN_MCCSyncExitFast).c_str(), "obj");
}

void CGLowerer::RegisterNoReturnFunctions(const char *const fnames[], const int n) {
  for (int i = 0; i < n; ++i) {
    GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName(fnames[i]);
    if (strIdx == GStrIdx(0)) {
      continue;
    }
    MIRSymbol *funcst = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
    if (funcst == nullptr) {
      continue;
    }
    CHECK_FATAL(funcst->sKind == kStFunc, "");
    MIRFunction *func = funcst->GetFunction();
    func->SetNoReturn();
  }
}

/*
   From Maple IR Document as of Apr 14, 2017
   Type Conversion Expression Opcodes
   Conversions between integer types of different sizes require the cvt opcode.
   Conversion between signed and unsigned integers of the same size does not
   require any operation, not even retype.

   cvt :
    Convert the operand's value from <from-type> to <to-type>.
    If the sizes of the two types are the same, the conversion must involve
    altering the bits.

   retype:
    <opnd0> is converted to <prim-type> which has derived type <type> without
    changing any bits.  The size of <opnd0> and <prim-type> must be the same.
    <opnd0> may be of aggregate type.
 */
BaseNode *CGLowerer::JavaMergeToCvtType(PrimType dtyp, PrimType styp, BaseNode *src) {
  CHECK_FATAL(IsPrimitiveInteger(dtyp) || IsPrimitiveFloat(dtyp), "");
  CHECK_FATAL(IsPrimitiveInteger(styp) || IsPrimitiveFloat(styp), "");
  // src i32, dest f32; src i64, dest f64
  CHECK_FATAL(
    (IsPrimitiveInteger(styp) && IsPrimitiveFloat(dtyp) && (GetPrimTypeBitSize(styp) == GetPrimTypeBitSize(dtyp))) ||
      (IsPrimitiveInteger(styp) && IsPrimitiveInteger(dtyp)),
    "");

  // src & dest are both of float type
  MIRType *totype = GlobalTables::GetTypeTable().GetPrimType(dtyp);
  MIRType *fromtype = GlobalTables::GetTypeTable().GetPrimType(styp);
  if (IsPrimitiveInteger(styp) && IsPrimitiveFloat(dtyp) && GetPrimTypeBitSize(styp) == GetPrimTypeBitSize(dtyp)) {
    return mirbuilder->CreateExprRetype(totype, fromtype, src);
  } else if (IsPrimitiveInteger(styp) && IsPrimitiveInteger(dtyp)) {
    if (GetPrimTypeBitSize(styp) >= GetPrimTypeBitSize(dtyp)) {
      if (dtyp == PTY_u1) {  // e.g., type _Bool
#if TARGARK
        totype = GlobalTables::GetTypeTable().GetPrimType(PTY_u1);
#else
        totype = GlobalTables::GetTypeTable().GetPrimType(PTY_u8);
#endif
        return mirbuilder->CreateExprCompare(OP_ne, totype, fromtype, src, mirbuilder->CreateIntConst(0, styp));
      } else if (GetPrimTypeBitSize(styp) > GetPrimTypeBitSize(dtyp)) {
        return mirbuilder->CreateExprTypeCvt(OP_cvt, totype, fromtype, src);
      } else if (IsSignedInteger(styp) != IsSignedInteger(dtyp)) {
        return mirbuilder->CreateExprTypeCvt(OP_cvt, totype, fromtype, src);
      } else {
        src->primType = dtyp;
        return src;
      }
      // Force type cvt here because we currently do not run constant folding
      // or contanst propagation before CG. We may revisit this decision later.
    } else if (GetPrimTypeBitSize(styp) < GetPrimTypeBitSize(dtyp)) {
      return mirbuilder->CreateExprTypeCvt(OP_cvt, totype, fromtype, src);
    } else if (IsConstvalZero(src)) {
      return mirbuilder->CreateIntConst(0, dtyp);
    } else {
      CHECK_FATAL(false, "NYI. Don't know what to do");
    }
  } else {
    CHECK_FATAL(false, "NYI. Don't know what to do");
  }
}

BaseNode *CGLowerer::LowerExpr(BaseNode *originParent, BaseNode *parent, BaseNode *expr, BlockNode *blknode) {
#if !TARGARK
  if (expr->primType == PTY_u1) {
    expr->primType = PTY_u8;
  }
#endif
  switch (expr->op) {
    case OP_intrinsicopwithtype:
      return LowerIntrinsicopwithtype(parent, static_cast<IntrinsicopNode *>(expr), blknode);
    default:
      return BELowerer::LowerExpr(parent, parent, expr, blknode);
  }
}

BaseNode *CGLowerer::LowerDread(DreadNode *dread) {
#if !TARGARK
  // use PTY_u8 for boolean type in dread/iread
  if (dread->primType == PTY_u1) {
    dread->primType = PTY_u8;
  }
#endif
#if DEBUG
  if (ConvertAutoSymbolsToPseudoRegs() && mirModule.IsJavaModule()) {
    MIRSymbol *sym = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dread->stIdx);
    if ((sym->storageClass == kScAuto || sym->storageClass == kScFormal) && dread->fieldID == 0) {
      PrimType primType = dread->primType;
      auto it = autos_to_pregs->find(dread->stIdx);
      if (it != autos_to_pregs->end()) {
        return mirbuilder->CreateExprRegread(primType, (*it).second);
      } else {
        // we should keep the type in preg table, but when we create
        // an entry in the virtual register table, we promote the type!!
        MIRPregTable *pregtab = mirModule.CurFunction()->pregTab;
        PregIdx pidx = pregtab->CreatePreg(primType);
        autos_to_pregs->insert(pair<StIdx, PregIdx>(dread->stIdx, pidx));
        pregs_to_autos->insert(pair<PregIdx, StIdx>(pidx, dread->stIdx));
        return mirbuilder->CreateExprRegread(primType, pidx);
      }
    } else {
      return BELowerer::LowerDread(dread);
    }
  } else {
#endif
    return BELowerer::LowerDread(dread);
#if DEBUG
  }

#endif
}

void CGLowerer::LowerRegassign(RegassignNode *regnode, BlockNode *newblk) {
  CHECK_FATAL((regnode->primType != PTY_ptr && regnode->primType != PTY_ref), "should have been lowered already");
  BaseNode *rhsopnd = regnode->Opnd(0);
  Opcode op = rhsopnd->op;
  if (op == OP_gcmalloc || op == OP_gcpermalloc) {
    LowerGCMalloc(regnode, static_cast<GCMallocNode *>(rhsopnd), newblk, op == OP_gcpermalloc);
    return;
  } else if (op == OP_gcmallocjarray || op == OP_gcpermallocjarray) {
    LowerJarrayMalloc(regnode, static_cast<JarrayMallocNode *>(rhsopnd), newblk, op == OP_gcpermallocjarray);
    return;
  } else {
    regnode->uOpnd = LowerExpr(regnode, regnode, rhsopnd, newblk);
    newblk->AddStatement(regnode);
  }
}

void CGLowerer::LowerDassign(DassignNode *dsnode, BlockNode *newblk) {
  StmtNode *newStmt = nullptr;
  BaseNode *rhs = nullptr;
  Opcode op = dsnode->GetRhs()->op;
  if (dsnode->fieldID != 0) {
    newStmt = LowerDassignBitfield(dsnode, newblk);
  } else if (op == OP_intrinsicop) {
    IntrinsicopNode *intrinnode = static_cast<IntrinsicopNode *>(dsnode->GetRhs());
    MIRType *rettype = IntrinDesc::intrintable[intrinnode->intrinsic].GetReturnType();
    CHECK_FATAL(rettype, "null ptr check ");
    if (rettype->typeKind == kTypeStruct) {
      newStmt = LowerIntrinsicopDassign(dsnode, intrinnode, newblk);
    } else {
      rhs = LowerExpr(dsnode, dsnode, intrinnode, newblk);
      dsnode->SetRhs(rhs);
      CHECK_FATAL(dsnode->GetRhs() != nullptr, "dsnode->rhs is null in CGLowerer::LowerDassign");
      if (!IsDassignNOP(dsnode)) {
        newStmt = dsnode;
      }
    }
  } else if (op == OP_gcmalloc || op == OP_gcpermalloc) {
    LowerGCMalloc(dsnode, static_cast<GCMallocNode *>(dsnode->GetRhs()), newblk, op == OP_gcpermalloc);
    return;
  } else if (op == OP_gcmallocjarray || op == OP_gcpermallocjarray) {
    LowerJarrayMalloc(dsnode, static_cast<JarrayMallocNode *>(dsnode->GetRhs()), newblk, op == OP_gcpermallocjarray);
    return;
  } else {
    rhs = LowerExpr(dsnode, dsnode, dsnode->GetRhs(), newblk);
    dsnode->SetRhs(rhs);
    newStmt = dsnode;
  }

  if (newStmt) {
#if DEBUG
    if (rhs && rhs->op == OP_dread) {
      static_cast<DassignNode *>(newStmt)->SetRhs(LowerDread(static_cast<DreadNode *>(rhs)));
    }
    if (newStmt->op == OP_dassign && ConvertAutoSymbolsToPseudoRegs() && mirModule.IsJavaModule()) {
      MIRSymbol *sym = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dsnode->stIdx);
      if ((sym->storageClass == kScAuto || sym->storageClass == kScFormal) && dsnode->fieldID == 0) {
        PrimType primType = sym->GetType()->GetPrimType();
        auto it = autos_to_pregs->find(dsnode->stIdx);
        if (it != autos_to_pregs->end()) {
          newStmt = mirbuilder->CreateStmtRegassign(primType, (*it).second, static_cast<DassignNode *>(newStmt)->GetRhs());
        } else {
          MIRPregTable *pregtab = mirModule.CurFunction()->pregTab;
          PregIdx pidx = pregtab->CreatePreg(primType);
          autos_to_pregs->insert(pair<StIdx, PregIdx>(dsnode->stIdx, pidx));
          pregs_to_autos->insert(pair<PregIdx, StIdx>(pidx, dsnode->stIdx));
          BaseNode *dassignRhs = static_cast<DassignNode *>(newStmt)->GetRhs();
#if !TARGARK
          if (dassignRhs->primType == PTY_u1) {
            dassignRhs->primType = PTY_u8;
          }
#endif
          newStmt = mirbuilder->CreateStmtRegassign(primType, pidx, dassignRhs);
        }
      }
    }
#endif
    newblk->AddStatement(newStmt);
  }
}

StmtNode *CGLowerer::LowerIntrinsicopDassign(const DassignNode *dsnode, IntrinsicopNode *intrinnode, BlockNode *newblk) {
  for (int32 i = 0; i < intrinnode->numOpnds; i++) {
    intrinnode->SetOpnd(LowerExpr(intrinnode, intrinnode, intrinnode->Opnd(i), newblk), i);
  }
  MIRIntrinsicID intrnid = intrinnode->intrinsic;
  IntrinDesc *intrindesc = &IntrinDesc::intrintable[intrnid];
  MIRSymbol *st = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  const std::string kName = intrindesc->name;
  CHECK_FATAL(intrindesc->name != nullptr, "");
  st->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(kName));
  st->storageClass = kScText;
  st->sKind = kStFunc;
  MIRFunction *fn = mirModule.memPool->New<MIRFunction>(&mirModule, st->GetStIdx());
  MapleVector<BaseNode *> &nopnds = intrinnode->nOpnd;
  st->SetFunction(fn);
  std::vector<TyIdx> fntyvec;
  std::vector<TypeAttrs> fntavec;

  if (intrindesc->IsJsOp()) {  // setup parameters
    for (uint32 i = 0; i < nopnds.size(); i++) {
      fntyvec.push_back(GlobalTables::GetTypeTable().typeTable[PTY_a32]->tyIdx);
      fntavec.push_back(TypeAttrs());
      BaseNode *addrnode = becommon.GetAddressOfNode(nopnds[i]);
      if (addrnode) {
        nopnds[i] = addrnode;
      } else {
        CHECK_FATAL(false, "NYI can not get address");
      }
    }
  } else {
    CHECK_FATAL(false, "");
  }

  MIRSymbol *dst = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dsnode->stIdx);
  MIRType *ty = dst->GetType();
  fn->SetReturnTyIdx(ty->tyIdx);
  st->SetTyIdx(becommon.BeGetOrCreateFunctionType(ty->tyIdx, fntyvec, fntavec)->tyIdx);
  CHECK_FATAL(ty->typeKind == kTypeStruct, "");
  CHECK_FATAL(dsnode->fieldID == 0, "NYI");
  AddrofNode *addrofnode = mirbuilder->CreateAddrof(dst, PTY_a32);
  MapleVector<BaseNode *> newopnd(mirModule.CurFuncCodeMemPoolAllocator()->Adapter());
  newopnd.push_back(addrofnode);
  newopnd.insert(newopnd.end(), nopnds.begin(), nopnds.end());
  CallNode *callstmt = mirModule.CurFuncCodeMemPool()->New<CallNode>(&mirModule, OP_call);
  callstmt->puIdx = st->GetFunction()->puIdx;
  callstmt->nOpnd = newopnd;
  return callstmt;
}

/*
   From mapleir/include/intrinsic_java.def
   JAVA_ARRAY_LENGTH
   JAVA_ARRAY_FILL
   JAVA_FILL_NEW_ARRAY
   JAVA_CHECK_CAST
   JAVA_CONST_CLASS
   JAVA_INSTANCE_OF
   JAVA_MERGE
   JAVA_RANDOM

   INTRN_<<name>>
   intrinsic
 */

BaseNode *CGLowerer::LowerJavascriptIntrinsicop(BaseNode *parent, IntrinsicopNode *intrinnode, IntrinDesc *desc) {
  MIRSymbol *st = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  const std::string kName = desc->name;
  CHECK_FATAL(desc->name != nullptr, "");
  st->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(kName));
  st->storageClass = kScText;
  st->sKind = kStFunc;
  MIRFunction *fn = mirModule.memPool->New<MIRFunction>(&mirModule, st->GetStIdx());
  MapleVector<BaseNode *> &nopnds = intrinnode->nOpnd;
  st->SetFunction(fn);
  std::vector<TyIdx> fntyvec;
  std::vector<TypeAttrs> fntavec;

  CHECK_FATAL(desc->IsJsOp(), "");
  // setup parameters
  for (uint32 i = 0; i < nopnds.size(); i++) {
    fntyvec.push_back(GlobalTables::GetTypeTable().typeTable[PTY_a32]->tyIdx);
    fntavec.push_back(TypeAttrs());
    BaseNode *addrnode = becommon.GetAddressOfNode(nopnds[i]);
    if (addrnode) {
      nopnds[i] = addrnode;
    } else {
      CHECK_FATAL(false, "NYI can not get address");
    }
  }

  MIRType *rettype = desc->GetReturnType();
  CHECK_FATAL(rettype, "null ptr check ");
  if (rettype->typeKind == kTypeStruct) {
    // create a local symbol and dread it;
    std::string tmpstr("__ret_struct_tmp_st");
    static uint32 tmpidx = 0;
    tmpstr.append(to_string(tmpidx++));
    MIRSymbol *tmpst = mirbuilder->GetOrCreateLocalDecl(tmpstr, rettype);
    fn->SetReturnTyIdx(rettype->tyIdx);
    st->SetTyIdx(becommon.BeGetOrCreateFunctionType(rettype->tyIdx, fntyvec, fntavec)->tyIdx);
    AddrofNode *addrofnode = mirbuilder->CreateAddrof(tmpst, PTY_a32);
    MapleVector<BaseNode *> newopnd(mirModule.CurFuncCodeMemPoolAllocator()->Adapter());
    newopnd.push_back(addrofnode);
    newopnd.insert(newopnd.end(), nopnds.begin(), nopnds.end());
    CallNode *callstmt = mirModule.CurFuncCodeMemPool()->New<CallNode>(&mirModule, OP_call);
    callstmt->puIdx = st->GetFunction()->puIdx;
    callstmt->nOpnd = newopnd;
    current_blk_->AddStatement(callstmt);
    // return the dread
    AddrofNode *drretst = mirbuilder->CreateDread(tmpst, PTY_agg);
    return drretst;
  } else {
    CHECK_FATAL(st->GetStIdx().FullIdx(), "check return");
    CallNode *callstmt = static_cast<CallNode *>(mirbuilder->CreateStmtCall(st->GetStIdx().FullIdx(), nopnds));
    current_blk_->AddStatement(callstmt);
    PrimType promotedptyp = intrinnode->primType == PTY_u1 ? PTY_u32 : intrinnode->primType;
    BaseNode *drretst = mirbuilder->CreateExprRegread(promotedptyp, -kSregRetval0);
    /* for safty dassign the return value to a register and return the dread to that register
         to avoid such code:
         call $__js_int32 (addrof ptr %temp_var_8 0)
         call $__jsop_getelem (addrof a32 %temp_var_9 0, addrof a32 $arr 0, dread i32 %%retval 0)
         for many target, the first actual parameter and return value would use R0, which would cause the above
         case fail
     */
    PregIdx tmpregidx = GetCurrentFunc()->pregTab->CreatePreg(promotedptyp);
    RegassignNode *dstoreg = mirbuilder->CreateStmtRegassign(promotedptyp, tmpregidx, drretst);
    current_blk_->AddStatement(dstoreg);
    RegreadNode *outdsnode = mirbuilder->CreateExprRegread(promotedptyp, tmpregidx);
    return outdsnode;
  }
}

StmtNode *CGLowerer::CreateStmtCallWithReturnValue(IntrinsicopNode *intrinnode, const MIRSymbol *ret, PUIdx bfunc,
                                                   BaseNode *extraInfo) {
  MapleVector<BaseNode *> args(mirModule.memPoolAllocator.Adapter());
  for (int i = 0; i < intrinnode->NumOpnds(); ++i) {
    args.push_back(intrinnode->Opnd(i));
  }
  if (extraInfo) {
    args.push_back(extraInfo);
  }
  return mirbuilder->CreateStmtCallAssigned(bfunc, args, ret, OP_callassigned);
}

StmtNode *CGLowerer::CreateStmtCallWithReturnValue(IntrinsicopNode *intrinnode, PregIdx retpidx, PUIdx bfunc,
                                                   BaseNode *extraInfo) {
  MapleVector<BaseNode *> args(mirModule.memPoolAllocator.Adapter());
  for (int i = 0; i < intrinnode->NumOpnds(); ++i) {
    args.push_back(intrinnode->Opnd(i));
  }
  if (extraInfo) {
    args.push_back(extraInfo);
  }
  return mirbuilder->CreateStmtCallRegassigned(bfunc, args, retpidx, OP_callassigned);
}

BaseNode *CGLowerer::LowerJavaIntrinsicop(BaseNode *parent, IntrinsicopNode *intrinnode, IntrinDesc *desc) {
  BaseNode *resNode = intrinnode;
  if (intrinnode->intrinsic == INTRN_JAVA_MERGE) {
    CHECK_FATAL(intrinnode->numOpnds > 0, "invalid JAVA_MERGE intrinsic node");
    BaseNode *candidate = intrinnode->Opnd(0);
    resNode = candidate;
    if (parent->op == OP_regassign) {
      PrimType styp = resNode->primType;
      RegassignNode *regassign = static_cast<RegassignNode *>(parent);
      PrimType primType = GetCurrentFunc()->pregTab->PregFromPregIdx(regassign->regIdx)->primType;
      if (styp != primType) {
        resNode = JavaMergeToCvtType(primType, styp, resNode);
      }

    } else if (parent->op == OP_dassign) {
      DassignNode *dassign = static_cast<DassignNode *>(parent);
      if (candidate->op == OP_constval) {
        MIRSymbol *dest = GetCurrentFunc()->GetLocalOrGlobalSymbol(dassign->stIdx);
        MIRType *totype = dest->GetType();
        PrimType dtyp = totype->GetPrimType();
        PrimType styp = resNode->primType;
        if (dtyp != styp) {
          resNode = JavaMergeToCvtType(dtyp, styp, resNode);
        }
      } else {
        if (candidate->op != OP_dread && candidate->op != OP_regread) {
          CHECK_FATAL(false, "don't know how to handle it");
        }
        bool differentLocation =
          (candidate->op == OP_dread)
            ? !IsAccessingTheSameMemoryLocation(dassign, static_cast<DreadNode *>(candidate))
            : !IsAccessingTheSameMemoryLocation(dassign, static_cast<RegreadNode *>(candidate), this);
        if (differentLocation) {
          bool simpleMove = false;
          // res_node already contains the 0-th operand.
          for (int32 i = 1; i < intrinnode->numOpnds; i++) {
            candidate = intrinnode->Opnd(i);
            if (candidate->op != OP_dread && candidate->op != OP_regread) {
              CHECK_FATAL(false, "don't know how to handle it");
            }
            bool sameLocation =
              (candidate->op == OP_dread)
                ? IsAccessingTheSameMemoryLocation(dassign, static_cast<DreadNode *>(candidate))
                : IsAccessingTheSameMemoryLocation(dassign, static_cast<RegreadNode *>(candidate), this);
            if (sameLocation) {
              simpleMove = true;
              resNode = candidate;
              break;
            }
          }
          if (!simpleMove) {
            // if source and destination types don't match, insert 'retype'
            MIRSymbol *dest = GetCurrentFunc()->GetLocalOrGlobalSymbol(dassign->stIdx);
            MIRType *totype = dest->GetType();
            PrimType dtyp = totype->GetPrimType();
            if (dtyp == PTY_agg || dassign->fieldID > 0) {
              CHECK_FATAL(false, "NYI && aggregate type?");
            }
            PrimType styp = resNode->primType;
            if (dtyp != styp) {
              resNode = JavaMergeToCvtType(dtyp, styp, resNode);
            }
          }
        }
      }
    } else {
      CHECK_FATAL(false, "don't know how to handle it");
    }
  } else if (intrinnode->intrinsic == INTRN_JAVA_ARRAY_LENGTH) {
    PUIdx bfunc = GetBuiltInToUse(intrinnode->intrinsic);
    CHECK_FATAL(bfunc != kfuncNotFound, "");
    MIRFunction *biFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(bfunc);

    BaseNode *arraddr = intrinnode->Opnd(0);
    if ((arraddr->primType == PTY_a64 || arraddr->primType == PTY_ref) &&
        (parent->op == OP_regassign || parent->op == OP_dassign || parent->op == OP_ge)) {

      MIRIntConst *arrayHeaderNode = mirModule.CurFuncCodeMemPool()->New<MIRIntConst>(
        AArch64RTSupport::kArrayLengthOffset, GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(arraddr->primType)));
      BaseNode *arrayHeaderCstNode = mirModule.CurFuncCodeMemPool()->New<ConstvalNode>(arrayHeaderNode);
      arrayHeaderCstNode->primType = arraddr->primType;

      MIRType *addrtyp = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(arraddr->primType));
      BaseNode *reflenaddr = mirbuilder->CreateExprBinary(OP_add, addrtyp, arraddr, arrayHeaderCstNode);
      MIRType *infolentyp = GlobalTables::GetTypeTable().GetInt32();
      MIRType *ptrtyp = becommon.BeGetOrCreatePointerType(infolentyp);
      resNode = mirbuilder->CreateExprIread(infolentyp, ptrtyp, 0, reflenaddr);

      string suffix = to_string(mirModule.CurFunction()->labelTab->GetLabelTableSize());
      GStrIdx labelStridx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("__label_nonnull_" + suffix);
      LabelIdx labidx = mirModule.CurFunction()->labelTab->AddLabel(labelStridx);
      LabelNode *labelNonnull = mirbuilder->CreateStmtLabel(labidx);

      BaseNode *cond = mirbuilder->CreateExprCompare(OP_ne, GlobalTables::GetTypeTable().GetUInt1(), GlobalTables::GetTypeTable().GetRef(), arraddr,
                                                     mirbuilder->CreateIntConst(0, PTY_ref));
      CondGotoNode *brtureNode = mirbuilder->CreateStmtCondGoto(cond, OP_brtrue, labidx);

      MapleVector<BaseNode *> opnds(mirModule.CurFuncCodeMemPoolAllocator()->Adapter());
      MIRFunction *newfunc =
          mirbuilder->GetOrCreateFunction(GetIntrinsicFuncName(INTRN_MCCThrowNullArrayNullPointerException), GlobalTables::GetTypeTable().GetVoid()->tyIdx);
      StmtNode *call = mirbuilder->CreateStmtCallAssigned(newfunc->puIdx, opnds, nullptr, OP_callassigned);

      current_blk_->AddStatement(brtureNode);
      current_blk_->AppendStatementsFromBlock(LowerCallAssignedStmt(call));
      current_blk_->AddStatement(labelNonnull);

    } else if (parent->op == OP_regassign) {
      RegassignNode *regassign = static_cast<RegassignNode *>(parent);
      StmtNode *biCall = CreateStmtCallWithReturnValue(intrinnode, regassign->regIdx, bfunc);
      current_blk_->AppendStatementsFromBlock(LowerCallAssignedStmt(biCall));
      PrimType primType = GetCurrentFunc()->pregTab->PregFromPregIdx(regassign->regIdx)->primType;
      resNode = mirbuilder->CreateExprRegread(primType, regassign->regIdx);
    } else if (parent->op == OP_dassign) {
      DassignNode *dassign = static_cast<DassignNode *>(parent);
      MIRSymbol *ret = GetCurrentFunc()->GetLocalOrGlobalSymbol(dassign->stIdx);
      StmtNode *biCall = CreateStmtCallWithReturnValue(intrinnode, ret, bfunc);
      current_blk_->AppendStatementsFromBlock(LowerCallAssignedStmt(biCall));
      resNode = mirbuilder->CreateExprDread(biFunc->GetReturnType(), 0, ret);
    } else {
      CHECK_FATAL(false, "don't know how to handle it");
    }
  }

  return resNode;
}

BaseNode *CGLowerer::LowerJavaIntrinsicopwithtype(BaseNode *parent, IntrinsicopNode *intrinnode,
                                                 IntrinDesc *desc) {
  BaseNode *resNode = intrinnode;
  if (intrinnode->intrinsic == INTRN_JAVA_CONST_CLASS || intrinnode->intrinsic == INTRN_JAVA_INSTANCE_OF) {
    PUIdx bfunc = GetBuiltInToUse(intrinnode->intrinsic);
    CHECK_FATAL(bfunc != kfuncNotFound, "");
    MIRFunction *biFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(bfunc);

    MIRType *classType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(intrinnode->tyIdx);
    BaseNode *classinfoExpr = nullptr;
    std::string classinfo;

    MIRPtrType *ptrType = static_cast<MIRPtrType *>(classType);
    MIRType *ptype = ptrType->GetPointedType();
    CHECK_FATAL(ptype, "Class type not found for INTRN_JAVA_CONST_CLASS");
    bool classinfoFromRt = false;  // whether the classinfo is generated by RT
    if (ptype || classType->typeKind == kTypeScalar) {
      MIRType *typeScalar = nullptr;
      if (ptype && ptype->typeKind == kTypeScalar) {
        typeScalar = ptype;
      } else if (classType->typeKind == kTypeScalar) {
        typeScalar = classType;
      }
      if (typeScalar) {
        std::string ename(GetPrimTypeJavaName(typeScalar->primType));
        classinfo = PRIMITIVECLASSINFO_PREFIX_STR + ename;
      }
      if (ptype->typeKind == kTypeByName || ptype->typeKind == kTypeClass || ptype->typeKind == kTypeInterface) {
        MIRStructType *structType = static_cast<MIRStructType *>(ptype);
        classinfo = CLASSINFO_PREFIX_STR + structType->GetName();
      } else if (ptype->typeKind == kTypeArray || ptype->typeKind == kTypeJArray) {
        MIRJarrayType *jarraytype = dynamic_cast<MIRJarrayType *>(ptype);
        CHECK_FATAL(jarraytype != nullptr, "jarraytype is null in CGLowerer::LowerJavaIntrinsicopwithtype");
        std::string baseName = jarraytype->GetJavaName();
        if (jarraytype->IsPrimitiveArray() && jarraytype->GetDim() <= 3) {
          classinfo = PRIMITIVECLASSINFO_PREFIX_STR;
          classinfo += baseName;
        } else if (baseName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaLangObjectStr)) ||
                   baseName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaLangClassStr)) ||
                   baseName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaLangStringStr))) {
          classinfo = CLASSINFO_PREFIX_STR;
          classinfo += baseName;
        } else {
          classinfoFromRt = true;
          classinfo = baseName;
        }
      }

      if (classinfoFromRt) {
        // generate runtime call to get class information
        // jclass __mrt_getclass(jobject caller, const char *name);
        // if the calling function is an instance function, it's the calling obj
        // if the calling function is a static function, it's the calling class

        PUIdx getclassFunc = GetBuiltInToUse(INTRN_JAVA_GET_CLASS);
        CHECK_FATAL(getclassFunc != kfuncNotFound, "");
        // return jclass
        MIRType *voidptrType = GlobalTables::GetTypeTable().GetPtr();
        MIRSymbol *ret0 = CreateNewRetVar(voidptrType, INTRN_RETVAL_PREFIX);

        MapleVector<BaseNode *> args(mirModule.memPoolAllocator.Adapter());

        MIRFunction *curfunc = mirModule.CurFunction();

        if (curfunc->IsStatic()) {
          // it's a static function.
          // pass caller functions's classinfo directly
          std::string callerName = CLASSINFO_PREFIX_STR;
          callerName += mirModule.CurFunction()->GetBaseClassName();
          GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(callerName);
          MIRSymbol *callerClassinfoSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
          BaseNode *callerClassinfoExpr = nullptr;
          if (!callerClassinfoSym) {
            callerClassinfoSym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
            callerClassinfoSym->SetNameStridx(strIdx);
            callerClassinfoSym->storageClass = kScGlobal;
            callerClassinfoSym->sKind = kStVar;
            // it must be a local symbol
            GlobalTables::GetGsymTable().AddToStringSymbolMap(callerClassinfoSym);
            callerClassinfoSym->SetTyIdx((TyIdx)PTY_ptr);
          }

          callerClassinfoExpr = mirbuilder->CreateExprAddrof(0, callerClassinfoSym);
          args.push_back(callerClassinfoExpr);
        } else {
          // it's an instance function.
          // pass caller function's this pointer
          CHECK_FATAL(curfunc->formalDefVec.size() > 0, "index out of range in CGLowerer::LowerJavaIntrinsicopwithtype");
          MIRSymbol *formalst = curfunc->formalDefVec[0].formalSym;
          BaseNode *callerObjExpr = nullptr;
          if (formalst->IsPreg())
            callerObjExpr =
              mirbuilder->CreateExprRegread(GlobalTables::GetTypeTable().GetTypeFromTyIdx(curfunc->formalDefVec[0].formalTyIdx)->primType,
                                            curfunc->pregTab->GetPregIdxFromPregNo(formalst->GetPreg()->pregNo));
          else {
            callerObjExpr = mirbuilder->CreateExprDread(formalst);
          }
          args.push_back(callerObjExpr);
        }
        // classname
        std::string klassJavaDescriptor;
        NameMangler::DecodeMapleNameToJavaDescriptor(classinfo, klassJavaDescriptor);
        UStrIdx classnameStridx = GlobalTables::GetUStrTable().GetOrCreateStrIdxFromName(klassJavaDescriptor);
        ConststrNode *classnameExpr = mirModule.memPool->New<ConststrNode>(classnameStridx);
        classnameExpr->primType = PTY_ptr;
        args.push_back(classnameExpr);

        StmtNode *getclassCall = mirbuilder->CreateStmtCallAssigned(getclassFunc, args, ret0, OP_callassigned);

        current_blk_->AppendStatementsFromBlock(LowerCallAssignedStmt(getclassCall));
        classinfoExpr = mirbuilder->CreateExprDread(voidptrType, 0, ret0);
      } else {
        GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(classinfo);
        MIRSymbol *classinfoSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
        if (classinfoSym) {
          classinfoExpr = mirbuilder->CreateExprAddrof(0, classinfoSym);
        } else {
          classinfoSym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
          classinfoSym->SetNameStridx(strIdx);
          classinfoSym->storageClass = kScGlobal;
          classinfoSym->sKind = kStVar;
          if (CGOptions::doPIC) {
            classinfoSym->storageClass = kScExtern;
          } else {
            classinfoSym->SetAttr(ATTR_weak);
          }
          GlobalTables::GetGsymTable().AddToStringSymbolMap(classinfoSym);
          classinfoSym->SetTyIdx((TyIdx)PTY_ptr);

          classinfoExpr = mirbuilder->CreateExprAddrof(0, classinfoSym);
          LowerTypePtr(classinfoExpr);
        }
      }
    }

    if (parent->op == OP_regassign) {
      if (intrinnode->intrinsic == INTRN_JAVA_CONST_CLASS) {
        CHECK_FATAL(classinfoExpr, "null ptr check");
        if (classinfoExpr->primType == PTY_ptr || classinfoExpr->primType == PTY_ref) {
          classinfoExpr->primType = PTY_a64;
        }
        resNode = classinfoExpr;
      } else {
        RegassignNode *regassign = static_cast<RegassignNode *>(parent);
        StmtNode *biCall = CreateStmtCallWithReturnValue(intrinnode, regassign->regIdx, bfunc, classinfoExpr);
        current_blk_->AppendStatementsFromBlock(LowerCallAssignedStmt(biCall));
        PrimType primType = GetCurrentFunc()->pregTab->PregFromPregIdx(regassign->regIdx)->primType;
        resNode = mirbuilder->CreateExprRegread(primType, regassign->regIdx);
      }
    } else if (parent->op == OP_dassign) {
      if (intrinnode->intrinsic == INTRN_JAVA_CONST_CLASS) {
        CHECK_FATAL(classinfoExpr, "null ptr check");
        if (classinfoExpr->primType == PTY_ptr || classinfoExpr->primType == PTY_ref) {
          classinfoExpr->primType = PTY_a64;
        }
        resNode = classinfoExpr;
      } else {
        DassignNode *dassign = static_cast<DassignNode *>(parent);
        MIRSymbol *ret = GetCurrentFunc()->GetLocalOrGlobalSymbol(dassign->stIdx);
        StmtNode *biCall = CreateStmtCallWithReturnValue(intrinnode, ret, bfunc, classinfoExpr);
        current_blk_->AppendStatementsFromBlock(LowerCallAssignedStmt(biCall));
        resNode = mirbuilder->CreateExprDread(biFunc->GetReturnType(), 0, ret);
      }
    } else {
      CHECK_FATAL(false, "don't know how to handle it");
    }

  } else {
    CHECK_FATAL(false, "NYI; add builtin func");
  }
  return resNode;
}

BaseNode *CGLowerer::LowerIntrinsicop(BaseNode *parent, IntrinsicopNode *intrinnode, BlockNode *newblk) {
  for (int32 i = 0; i < intrinnode->numOpnds; i++) {
    intrinnode->SetOpnd(LowerExpr(intrinnode, intrinnode, intrinnode->Opnd(i), newblk), i);
  }

  MIRIntrinsicID intrnid = intrinnode->intrinsic;
  IntrinDesc *intrindesc = &IntrinDesc::intrintable[intrnid];
  if (intrindesc->IsJS()) {
    return LowerJavascriptIntrinsicop(parent, intrinnode, intrindesc);
  } else if (intrindesc->IsJava()) {
    return LowerJavaIntrinsicop(parent, intrinnode, intrindesc);
  } else if (intrnid == INTRN_C_constant_p) {
    BaseNode *opnd = intrinnode->nOpnd[0];
    return mirbuilder->CreateIntConst(opnd->op == OP_constval || opnd->op == OP_sizeoftype ||
                  opnd->op == OP_conststr || opnd->op == OP_conststr16 ||
                  opnd->op == OP_addrof || opnd->op == OP_addroffunc, PTY_i32);
  } else {
    if (intrinFuncIds.find(intrindesc) == intrinFuncIds.end()) {
      // add funcid into map
      intrinFuncIds[intrindesc] = mirbuilder->GetOrCreateFunction(intrindesc->name, TyIdx(intrinnode->primType))->puIdx;
    }
    PregIdx pregIdx = mirModule.CurFunction()->pregTab->CreatePreg(intrinnode->primType);
    StmtNode *callstmt = CreateStmtCallWithReturnValue(intrinnode, pregIdx, intrinFuncIds[intrindesc], nullptr);
    newblk->AppendStatementsFromBlock(LowerCallAssignedStmt(callstmt));
    return mirbuilder->CreateExprRegread(intrinnode->primType, pregIdx);
  }
}

BaseNode *CGLowerer::LowerIntrinsicopwithtype(BaseNode *parent, IntrinsicopNode *intrinnode, BlockNode *blk) {
  for (int32 i = 0; i < intrinnode->numOpnds; i++) {
    intrinnode->SetOpnd(LowerExpr(intrinnode, intrinnode, intrinnode->Opnd(i), blk), i);
  }
  MIRIntrinsicID intrnid = intrinnode->intrinsic;
  IntrinDesc *intrindesc = &IntrinDesc::intrintable[intrnid];
  if (intrindesc->IsJS()) {
    CHECK_FATAL(false, "NYI");
  } else if (intrindesc->IsJava()) {
    return LowerJavaIntrinsicopwithtype(parent, intrinnode, intrindesc);
  } else {
    return BELowerer::LowerIntrinsicopwithtype(parent, intrinnode, blk);
  }
}

StmtNode *CGLowerer::LowerIntrinsiccall(IntrinsiccallNode *intrincall, BlockNode *newblk) {
  bool isarraystore = false;
  MIRIntrinsicID intrnid = intrincall->intrinsic;
  if (intrnid >= INTRN_MCCWriteVolNoInc && intrnid <= INTRN_MCCWrite) {
    //Intrinsics from WriteVolXXXX to WriteXXXX
    if (intrincall->Opnd(1)->op == OP_iaddrof) {
      IreadNode *addrexpr = static_cast<IreadNode *>(intrincall->Opnd(1));
      if (addrexpr->Opnd(0)->op == OP_array) {
        isarraystore = true;
      }
    }
  }
  for (int32 i = 0; i < intrincall->numOpnds; i++) {
    intrincall->SetOpnd(LowerExpr(intrincall, intrincall, intrincall->Opnd(i), newblk), i);
  }
  if (intrnid == INTRN_MPL_CLEAR_STACK) {
    StmtNode *newStmt = mirbuilder->CreateStmtIassign(becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetUInt8()), 0,
                                                      intrincall->Opnd(0), mirbuilder->GetConstUInt8(0));
    newblk->AddStatement(newStmt);

    BaseNode *length = intrincall->Opnd(1);
    PrimType primType = PTY_i64;
    PregIdx pidx = GetCurrentFunc()->pregTab->CreatePreg(primType);
    newStmt = mirbuilder->CreateStmtRegassign(primType, pidx, mirbuilder->CreateIntConst(1, primType));
    newblk->AddStatement(newStmt);
    MIRFunction *func = GetCurrentFunc();

    LabelIdx label1 =
      GetCurrentFunc()->GetOrCreateLablidxFromName(func->GetName() + std::string("_Lalloca_") + to_string(labelIdx++));
    LabelIdx label2 =
      GetCurrentFunc()->GetOrCreateLablidxFromName(func->GetName() + std::string("_Lalloca_") + to_string(labelIdx++));

    newStmt = mirbuilder->CreateStmtGoto(OP_goto, label2);
    newblk->AddStatement(newStmt);
    LabelNode *ln = mirbuilder->CreateStmtLabel(label1);
    newblk->AddStatement(ln);

    RegreadNode *reglen = mirbuilder->CreateExprRegread(primType, pidx);

    BinaryNode *addr = mirbuilder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetAddr64(), intrincall->Opnd(0), reglen);

    newStmt = mirbuilder->CreateStmtIassign(becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetUInt8()), 0, addr,
                                            mirbuilder->GetConstUInt8(0));
    newblk->AddStatement(newStmt);

    BinaryNode *sublen = mirbuilder->CreateExprBinary(
        OP_add, GlobalTables::GetTypeTable().GetPrimType(primType), reglen, mirbuilder->CreateIntConst(1, primType));
    newStmt = mirbuilder->CreateStmtRegassign(primType, pidx, sublen);
    newblk->AddStatement(newStmt);

    ln = mirbuilder->CreateStmtLabel(label2);
    newblk->AddStatement(ln);

    CompareNode *cmpexp =
        mirbuilder->CreateExprCompare(OP_lt, GlobalTables::GetTypeTable().GetUInt32(), GlobalTables::GetTypeTable().GetPrimType(primType), reglen, length);
    newStmt = mirbuilder->CreateStmtCondGoto(cmpexp, OP_brtrue, label1);

    return newStmt;
  }

  if (isarraystore && checkloadstore) {
    MIRType *arraytype = nullptr;
    MIRType *valuetype = nullptr;
    bool needcheckstore = true;
    BaseNode *arraynode = intrincall->Opnd(0);
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

    BaseNode *valuenode = intrincall->Opnd(2);
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
      opnds.push_back(intrincall->Opnd(0));
      opnds.push_back(intrincall->Opnd(2));
      StmtNode *checkstoreStmt = mirModule.mirBuilder->CreateStmtCall(fn->puIdx, opnds, OP_call);
      newblk->AddStatement(checkstoreStmt);
    }
  }

  IntrinDesc *intrindesc = &IntrinDesc::intrintable[intrnid];

  // Lower RC Intrinsics
  if (intrindesc->IsRc()) {
    if (intrinFuncIds.find(intrindesc) == intrinFuncIds.end()) {
      // add funcid into map
      intrinFuncIds[intrindesc] = mirbuilder->GetOrCreateFunction(intrindesc->name, TyIdx(PTY_void))->puIdx;
    }
    CallNode *callstmt = mirModule.CurFuncCodeMemPool()->New<CallNode>(&mirModule, OP_call);
    callstmt->puIdx = intrinFuncIds[intrindesc];
    for (uint32 i = 0; i < intrincall->nOpnd.size(); ++i) {
      callstmt->nOpnd.push_back(intrincall->nOpnd[i]);
      callstmt->numOpnds++;
    }
    return callstmt;
  }

  MIRSymbol *st = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  const std::string kName = intrindesc->name;
  CHECK_FATAL(intrindesc->name != nullptr, "intrinsic without name");
  st->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(kName));
  st->storageClass = kScText;
  st->sKind = kStFunc;
  MIRFunction *fn = mirbuilder->GetOrCreateFunction(intrindesc->name, TyIdx(0));
  st->SetFunction(fn);
  switch (intrnid) {
    // default lowers intrinsic call to real function call, here we leave them to CGFunc::SelectIntrinCall()
    case INTRN_MPL_CLEANUP_LOCALREFVARS_SKIP: {
      MIRFunction *mirFunc = mirModule.CurFunction();
      BaseNode *skipexpr = intrincall->Opnd(intrincall->NumOpnds() - 1);

      CHECK_FATAL(skipexpr != nullptr && skipexpr->op == OP_dread, "should be dread");
      DreadNode *refnode = static_cast<DreadNode *>(skipexpr);
      MIRSymbol *skipsym = mirFunc->GetLocalOrGlobalSymbol(refnode->stIdx);
      if (skipsym->GetAttr(ATTR_localrefvar)) {
        mirFunc->retRefSym.insert(skipsym);
      }

      return intrincall;
    }
    case INTRN_C_va_start:
    case INTRN_MPL_CLEANUP_LOCALREFVARS:
    case INTRN_MPL_CLINIT_CHECK: {
      return intrincall;
    }
    case INTRN_MPL_STACK: {
      MIRFunction *mirFunc = mirModule.CurFunction();
      BaseNode *symexpr = intrincall->Opnd(0);

      CHECK_FATAL(symexpr != nullptr && symexpr->op == OP_dread, "should be dread");
      DreadNode *refnode = static_cast<DreadNode *>(symexpr);
      MIRSymbol *skipsym = mirFunc->GetLocalOrGlobalSymbol(refnode->stIdx);

      ConstvalNode *constvalnode = static_cast<ConstvalNode *>(intrincall->Opnd(1));
      MIRConst *mirconst = constvalnode->constVal;
      CHECK_FATAL(mirconst->kind == kConstInt, "should be int const");
      MIRIntConst *mirintconst = static_cast<MIRIntConst *>(mirconst);

      mirFunc->stackallocVarMap[skipsym] = mirintconst->GetValueUnderType();

      return intrincall;
    }
    default: {
      std::vector<TyIdx> functyvec;
      std::vector<TypeAttrs> fntavec;
      MapleVector<BaseNode *> &nopnds = intrincall->nOpnd;
      MIRType *retty = intrindesc->GetReturnType();
      CHECK_FATAL(retty, "null ptr check ");
      if (retty->typeKind == kTypeStruct) {
        functyvec.push_back(becommon.BeGetOrCreatePointerType(retty)->tyIdx);
        fntavec.push_back(TypeAttrs());
        fn->SetReturnTyIdx((TyIdx)PTY_void);
        fn->SetReturnStruct();
      } else {
        fn->SetReturnTyIdx(retty->tyIdx);
      }
      for (uint32 i = 0; i < nopnds.size(); i++) {
        MIRType *argty = intrindesc->GetArgType(i);
        CHECK_FATAL(argty, "NYI");
        if (argty->typeKind == kTypeStruct) {
          functyvec.push_back(GlobalTables::GetTypeTable().typeTable[PTY_a32]->tyIdx);
          fntavec.push_back(TypeAttrs());
          BaseNode *addrnode = becommon.GetAddressOfNode(nopnds[i]);
          if (addrnode) {
            nopnds[i] = addrnode;
          } else {
            CHECK_FATAL(false, "NYI can not get address");
          }
        } else {
          functyvec.push_back(argty->tyIdx);
          fntavec.push_back(TypeAttrs());
        }
      }
      for (size_t i = 0; i < functyvec.size(); i++) {
        FormalDef formalDef(nullptr, functyvec[i], fntavec[i]);
        fn->formalDefVec.push_back(formalDef);
      }
      st->SetTyIdx(becommon.BeGetOrCreateFunctionType(retty->tyIdx, functyvec, fntavec)->tyIdx);
      CallNode *callstmt = static_cast<CallNode *>(mirbuilder->CreateStmtCall(fn->puIdx, nopnds));
      return callstmt;
    }
  }
}

StmtNode *CGLowerer::LowerSyncEnterSyncExit(StmtNode *stmt) {
  CHECK_FATAL(stmt->op == OP_syncenter || stmt->op == OP_syncexit, "");

  NaryStmtNode *nstmt = static_cast<NaryStmtNode *>(stmt);
  builtin_func_id_t id;
  if (nstmt->op == OP_syncenter) {
    if (nstmt->NumOpnds() == 1) {
      // Just as ParseNaryStmt do for syncenter
      MIRType *inttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_i32);
      // default 2 for __sync_enter_fast()
      MIRIntConst *intconst = mirModule.memPool->New<MIRIntConst>(2, inttype);
      ConstvalNode *exprconst = mirModule.memPool->New<ConstvalNode>();
      exprconst->primType = PTY_i32;
      exprconst->constVal = intconst;
      nstmt->nOpnd.push_back(exprconst);
      nstmt->numOpnds = nstmt->nOpnd.size();
    }
    CHECK_FATAL(nstmt->NumOpnds() == 2, "wrong args for syncenter");
    CHECK_FATAL(nstmt->Opnd(1)->op == OP_constval, "wrong 2nd arg type for syncenter");
    ConstvalNode *cst = static_cast<ConstvalNode *>(nstmt->nOpnd[1]);
    MIRIntConst *intconst = static_cast<MIRIntConst *>(cst->constVal);
    switch ((uint32_t)intconst->value) {
      case 0:
        id = INTRN_JAVA_SYNC_ENTER_0;
        break;
      case 1:
        id = INTRN_JAVA_SYNC_ENTER_1;
        break;
      case 2:
        id = INTRN_JAVA_SYNC_ENTER_2;
        break;
      case 3:
        id = INTRN_JAVA_SYNC_ENTER_3;
        break;
      default:
        CHECK_FATAL(false, "wrong kind for syncenter");
        break;
    }
  } else {
    CHECK_FATAL(nstmt->NumOpnds() == 1, "wrong args for syncexit");
    id = INTRN_JAVA_SYNC_EXIT;
  }
  PUIdx bfunc = GetBuiltInToUse(id);
  CHECK_FATAL(bfunc != kfuncNotFound, "");
  int32 slotoff = 0;
  if (nstmt->Opnd(0)->op == OP_dread) {
    DreadNode *dsym = static_cast<DreadNode *>(nstmt->Opnd(0));

    MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dsym->stIdx);

    auto it = locksymbol.find(symbol);
    if (it != locksymbol.end()) {
      slotoff = it->second;
    } else {
      slotoff = mirModule.CurFunction()->lockslotnum;
      locksymbol[symbol] = slotoff;
    }
  }

  if (nstmt->Opnd(0)->op == OP_addrof) {
    AddrofNode *dsym = static_cast<AddrofNode *>(nstmt->Opnd(0));

    MIRSymbol *symbol = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dsym->stIdx);

    auto it = locksymbol.find(symbol);
    if (it != locksymbol.end()) {
      slotoff = it->second;
    } else {
      slotoff = mirModule.CurFunction()->lockslotnum;
      locksymbol[symbol] = slotoff;
    }
  }

  if (nstmt->Opnd(0)->op == OP_regread) {
    RegreadNode *dsym = static_cast<RegreadNode *>(nstmt->Opnd(0));
    PregIdx regid = dsym->regIdx;
    auto it = lockreg.find(regid);
    if (it != lockreg.end()) {
      slotoff = it->second;
    } else {
      slotoff = mirModule.CurFunction()->lockslotnum;
      lockreg[regid] = slotoff;
    }
  }
  MapleVector<BaseNode *> opnds(mirbuilder->GetCurrentFuncCodeMpAllocator()->Adapter());
  opnds.push_back(nstmt->Opnd(0));
  return mirbuilder->CreateStmtCall(bfunc, opnds);
}

PUIdx CGLowerer::GetBuiltInToUse(builtin_func_id_t id) {
  // use std::vector & linear search as the number of entries is small.
  // we may revisit it if the number of entries gets larger.
  for (uint32 i = 0; i < builtinFuncIds.size(); ++i)
    if (builtinFuncIds[i].first == id) {
      return builtinFuncIds[i].second;
    }
  return kfuncNotFound;
}

BaseNode *CGLowerer::LowerSTACKMalloc(GCMallocNode *stmalloc, BlockNode *blknode) {
  // need be agreed with mrt

  const size_t kDwordBytes = 8;
  const size_t kHeaderSize = kDwordBytes;

  TyIdx tyIdx = stmalloc->tyIdx;
  CHECK(tyIdx.GetIdx() < becommon.type_size_table.size(), "index out of range in CGLowerer::LowerSTACKMalloc ");
  uint64_t size = becommon.type_size_table[tyIdx.GetIdx()];
  ConstvalNode *sizenode = mirbuilder->CreateIntConst(size + kHeaderSize, PTY_u64);

  PregIdx addrpidx = GetCurrentFunc()->pregTab->CreatePreg(PTY_a64);
  GetCurrentFunc()->hasVlaoralloca = true;
  UnaryNode *allonod = mirbuilder->CreateExprUnary(OP_alloca, GlobalTables::GetTypeTable().GetAddr64(), sizenode);
  StmtNode *newStmt = mirbuilder->CreateStmtRegassign(PTY_a64, addrpidx, allonod);

  blknode->AddStatement(newStmt);
  RegreadNode *addreg = mirbuilder->CreateExprRegread(PTY_a64, addrpidx);

  // clear
  MapleVector<BaseNode*> ops(mirbuilder->mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
  ops.push_back(addreg);
  ops.push_back(sizenode);
  IntrinsiccallNode *clearnode = mirbuilder->CreateStmtIntrinsicCall(INTRN_MPL_CLEAR_STACK, ops);

  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  CHECK_FATAL(LowerIntrinsiccall(clearnode, blk) != nullptr,
         "LowerIntrinsiccall(clearnode, blk) is null in CGLowerer::LowerSTACKMalloc");
  blk->AddStatement(LowerIntrinsiccall(clearnode, blk));
  CHECK_FATAL(blk->GetFirst() != nullptr, "blk is null in CGLowerer::LowerSTACKMalloc");
  blknode->AppendStatementsFromBlock(blk);

  ConstvalNode *headnode = mirbuilder->CreateIntConst(kHeaderSize, PTY_u64);
  BinaryNode *objadd = mirbuilder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetAddr64(), addreg, headnode);

  // set klass
  MIRStructType *classType = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(stmalloc->tyIdx));
  std::string classinfoName = CLASSINFO_PREFIX_STR + classType->GetName();
  MIRSymbol *classSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(classinfoName.c_str()));
  if (!classSym) {
    classSym = mirbuilder->CreateGlobalDecl(classinfoName, GlobalTables::GetTypeTable().GetVoidPtr(), kScExtern);
  }

  AddrofNode *klassaddr = mirbuilder->CreateExprAddrof(0, classSym);
  klassaddr->primType = PTY_a64;
  newStmt =
      mirbuilder->CreateStmtIassign(becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetAddr64()), 0, objadd, klassaddr);
  blknode->AddStatement(newStmt);

  return objadd;
}

void CGLowerer::LowerGCMalloc(BaseNode *node, const GCMallocNode *gcmalloc, BlockNode *blknode, bool perm) {
  MIRFunction *func = mirbuilder->GetOrCreateFunction((perm ? GetIntrinsicFuncName(INTRN_MCCNewPermanentObject) :
                      GetIntrinsicFuncName(INTRN_MCCNewObjFixedClass)), (TyIdx)(PTY_a64));
  MapleVector<BaseNode *> args(mirModule.memPoolAllocator.Adapter());

  // Get the classinfo
  MIRStructType *classType = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(gcmalloc->tyIdx));
  std::string classinfoName = CLASSINFO_PREFIX_STR + classType->GetName();
  MIRSymbol *classSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(classinfoName.c_str()));
  if (!classSym) {
    classSym = mirbuilder->CreateGlobalDecl(classinfoName, GlobalTables::GetTypeTable().GetVoidPtr(), kScExtern);
  }
  CallNode *callassign = nullptr;
  if (classSym->GetAttr(ATTR_abstract) || classSym->GetAttr(ATTR_interface)) {
    MIRFunction *intrinFunc = mirbuilder->GetOrCreateFunction(GetIntrinsicFuncName(INTRN_MCCReflectThrowInstantiationError), (TyIdx)(PTY_a64));
    args.push_back(mirbuilder->CreateExprAddrof(0, classSym));
    if (node->op == OP_dassign) {
      DassignNode *dsnode = static_cast<DassignNode *>(node);
      MIRSymbol *ret = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dsnode->stIdx);
      callassign = mirbuilder->CreateStmtCallAssigned(intrinFunc->puIdx, args, ret, OP_callassigned);
    } else {
      CHECK_FATAL(node->op == OP_regassign, "regassign expected");
      callassign = mirbuilder->CreateStmtCallRegassigned(func->puIdx, args, static_cast<RegassignNode *>(node)->regIdx,
                                                         OP_callassigned);
    }
    blknode->AppendStatementsFromBlock(LowerCallAssignedStmt(callassign));
    return;
  }
  args.push_back(mirbuilder->CreateExprAddrof(0, classSym));

  if (node->op == OP_dassign) {
    MIRSymbol *ret = mirModule.CurFunction()->GetLocalOrGlobalSymbol(static_cast<DassignNode *>(node)->stIdx);
    callassign = mirbuilder->CreateStmtCallAssigned(func->puIdx, args, ret, OP_callassigned);
  } else {
    CHECK_FATAL(node->op == OP_regassign, "regassign expected");
    callassign = mirbuilder->CreateStmtCallRegassigned(func->puIdx, args, static_cast<RegassignNode *>(node)->regIdx,
                                                       OP_callassigned);
  }
  blknode->AppendStatementsFromBlock(LowerCallAssignedStmt(callassign));
}

BaseNode *CGLowerer::LowerSTACKJarrayMalloc(JarrayMallocNode *node, BlockNode *blknode) {
  TyIdx tyIdx = node->tyIdx;
  MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  auto jarytype = dynamic_cast<MIRJarrayType *>(type);
  CHECK_FATAL((jarytype != nullptr), "Type param of gcmallocjarray is not a MIRJarrayType");

  // Inspect element type
  MIRType *elemType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(jarytype->elemTyIdx);
  PrimType elemPrimtype = elemType->GetPrimType();
  uint64_t elemSize = GetPrimTypeSize(elemPrimtype);
  ConstvalNode *elemsizenode = mirbuilder->CreateIntConst(elemSize, PTY_u64);
  BinaryNode *alloclen = mirbuilder->CreateExprBinary(OP_mul, GlobalTables::GetTypeTable().GetAddr64(), node->Opnd(0), elemsizenode);
  uint64_t arrayHeaderSize = AArch64RTSupport::kArrayContentOffset + AArch64RTSupport::kObjectHeaderSize;
  ConstvalNode *arrheadnode = mirbuilder->CreateIntConst(arrayHeaderSize, PTY_u64);
  alloclen = mirbuilder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetAddr64(), alloclen, arrheadnode);

  PregIdx addrpidx = GetCurrentFunc()->pregTab->CreatePreg(PTY_a64);
  GetCurrentFunc()->hasVlaoralloca = true;
  UnaryNode *allonod = mirbuilder->CreateExprUnary(OP_alloca, GlobalTables::GetTypeTable().GetAddr64(), alloclen);
  StmtNode *newStmt = mirbuilder->CreateStmtRegassign(PTY_a64, addrpidx, allonod);

  blknode->AddStatement(newStmt);

  RegreadNode *addreg = mirbuilder->CreateExprRegread(PTY_a64, addrpidx);

  // clear
  MapleVector<BaseNode*> ops(mirbuilder->mirModule->CurFuncCodeMemPoolAllocator()->Adapter());
  ops.push_back(addreg);
  ops.push_back(alloclen);
  IntrinsiccallNode *clearnode = mirbuilder->CreateStmtIntrinsicCall(INTRN_MPL_CLEAR_STACK, ops);

  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  CHECK_FATAL(blk != nullptr && blk->GetFirst() != nullptr, "blk is nullptr in CGLowerer::LowerSTACKJarrayMalloc");
  CHECK_FATAL(LowerIntrinsiccall(clearnode, blk) != nullptr, "LowerIntrinsiccall(clearnode, blk) return nullptr");
  blk->AddStatement(LowerIntrinsiccall(clearnode, blk));
  blknode->AppendStatementsFromBlock(blk);

  ConstvalNode *headnode = mirbuilder->CreateIntConst(AArch64RTSupport::kObjectHeaderSize, PTY_u64);
  BinaryNode *objadd = mirbuilder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetAddr64(), addreg, headnode);
  // set arrlen
  ConstvalNode *lenoffset = mirbuilder->CreateIntConst(AArch64RTSupport::kArrayLengthOffset, PTY_u64);
  BinaryNode *lenadd = mirbuilder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetAddr64(), objadd, lenoffset);

  newStmt = mirbuilder->CreateStmtIassign(
      becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetUInt32()), 0, lenadd, node->Opnd(0));
  blknode->AddStatement(newStmt);

  // set klass
  std::string classinfoName = CLASSINFO_PREFIX_STR + jarytype->GetJavaName();
  MIRSymbol *classSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(classinfoName.c_str()));
  if (!classSym) {
    classSym = mirbuilder->CreateGlobalDecl(classinfoName, GlobalTables::GetTypeTable().GetVoidPtr(), kScExtern);
  }

  AddrofNode *klassaddr = mirbuilder->CreateExprAddrof(0, classSym);
  klassaddr->primType = PTY_a64;
  newStmt =
      mirbuilder->CreateStmtIassign(becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetAddr64()), 0, objadd, klassaddr);
  blknode->AddStatement(newStmt);

  return objadd;
}

void CGLowerer::LowerJarrayMalloc(StmtNode *stmt, const JarrayMallocNode *node, BlockNode *blknode, bool perm) {
  MIRFunction *func = mirbuilder->GetOrCreateFunction((perm? GetIntrinsicFuncName(INTRN_MCCNewPermanentArray) :
                      GetIntrinsicFuncName(INTRN_MCCNewObjFlexibleCname)), (TyIdx)(PTY_a64));
  MapleVector<BaseNode *> args(mirModule.memPoolAllocator.Adapter());

  // Extract jarray type
  TyIdx tyIdx = node->tyIdx;
  MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  auto jarytype = dynamic_cast<MIRJarrayType *>(type);
  CHECK_FATAL((jarytype != nullptr), "Type param of gcmallocjarray is not a MIRJarrayType");

  // Inspect element type
  MIRType *elemType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(jarytype->elemTyIdx);
  PrimType elemPrimtype = elemType->GetPrimType();
  uint64_t elemSize = GetPrimTypeSize(elemPrimtype);
  if (elemType->GetKind() != kTypeScalar) {  // element is reference
    elemSize = AArch64RTSupport::kRefFieldSize;
  }

  args.push_back(mirbuilder->CreateIntConst(elemSize, PTY_u64));  // elem_size
  args.push_back(node->Opnd(0));                                  // n_elems

  std::string klassName = jarytype->GetJavaName();
  std::string arrayclassinfoName;
  bool isPredefinedArrayClass = false;
  if (jarytype->IsPrimitiveArray() && jarytype->GetDim() <= 3) {
    arrayclassinfoName = PRIMITIVECLASSINFO_PREFIX_STR + klassName;
    isPredefinedArrayClass = true;
  } else if (klassName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaLangObjectStr)) ||
             klassName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaLangClassStr)) ||
             klassName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaLangStringStr)) ||
             klassName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaUtilFormatterFlags)) ||
             klassName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaUtilHashMapNode)) ||
             klassName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaUtilFormatterFormatString)) ||
             klassName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaLangCharSequence)) ||
             klassName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaLangThreadLocalMapEntry)) ||
             klassName == (JARRAY_PREFIX_STR + string(NameMangler::kJavaUtilHashTableHashTableEntry))) {
    arrayclassinfoName = CLASSINFO_PREFIX_STR + klassName;
    isPredefinedArrayClass = true;
  }
  if (isPredefinedArrayClass) {
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(arrayclassinfoName);
    MIRSymbol *arrayClassSym =
      GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(arrayclassinfoName.c_str()));
    if (!arrayClassSym) {
      arrayClassSym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
      arrayClassSym->SetNameStridx(strIdx);
      arrayClassSym->storageClass = kScGlobal;
      arrayClassSym->sKind = kStVar;
      if (CGOptions::doPIC) {
        arrayClassSym->storageClass = kScExtern;
      } else {
        arrayClassSym->SetAttr(ATTR_weak);
      }
      GlobalTables::GetGsymTable().AddToStringSymbolMap(arrayClassSym);
      arrayClassSym->SetTyIdx((TyIdx)PTY_ptr);
    }
    args.push_back(mirbuilder->CreateExprAddrof(0, arrayClassSym));
  } else {
    std::string klassJavaDescriptor;
    NameMangler::DecodeMapleNameToJavaDescriptor(klassName, klassJavaDescriptor);
    UStrIdx classnameStridx = GlobalTables::GetUStrTable().GetOrCreateStrIdxFromName(klassJavaDescriptor);
    ConststrNode *classnameExpr = mirModule.memPool->New<ConststrNode>(classnameStridx);
    classnameExpr->primType = PTY_ptr;
    args.push_back(classnameExpr);  // class_name
  }

  MIRFunction *curfunc = mirModule.CurFunction();
  if (curfunc->IsStatic()) {
    // it's a static function.
    // pass caller functions's classinfo directly
    std::string callerName = CLASSINFO_PREFIX_STR;
    callerName += mirModule.CurFunction()->GetBaseClassName();
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(callerName);
    MIRSymbol *callerClassinfoSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
    if (!callerClassinfoSym) {
      MIRType *mtype = GlobalTables::GetTypeTable().GetVoidPtr();
      CHECK_FATAL(mtype != nullptr, "type is null in CGLowerer::LowerJarrayMalloc");
      callerClassinfoSym = mirbuilder->CreateGlobalDecl(callerName, mtype, kScExtern);
    }

    BaseNode *callerClassinfoExpr = mirbuilder->CreateExprAddrof(0, callerClassinfoSym);
    args.push_back(callerClassinfoExpr);  // caller_obj
  } else {
    // it's an instance function.
    // pass caller function's this pointer
    BaseNode *callerObjExpr = nullptr;
    CHECK(curfunc->formalDefVec.size() > 0, "index out of range in CGLowerer::LowerJarrayMalloc");
    MIRSymbol *symbol = curfunc->formalDefVec[0].formalSym;
    if (symbol->IsPreg()) {
      callerObjExpr = mirbuilder->CreateExprRegread(symbol->GetType()->primType,
                                                    curfunc->pregTab->GetPregIdxFromPregNo(symbol->GetPreg()->pregNo));
    } else {
      callerObjExpr = mirbuilder->CreateExprDread(symbol);
    }
    args.push_back(callerObjExpr);  // caller_obj
  }

  // set class flag
#define CLASSOBJECTFLAG 0xF0
  uint64_t wellKnownClassFlag = GetWellKnownFrameWorksClassFlag(jarytype->GetJavaName());
  uint64_t classFlag = isPredefinedArrayClass ? (wellKnownClassFlag | CLASSOBJECTFLAG) : wellKnownClassFlag;
  args.push_back(mirbuilder->CreateIntConst(classFlag, PTY_u64));
  CallNode *callassign = nullptr;
  if (stmt->op == OP_dassign) {
    DassignNode *dsnode = static_cast<DassignNode *>(stmt);
    MIRSymbol *ret = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dsnode->stIdx);

    callassign = mirbuilder->CreateStmtCallAssigned(func->puIdx, args, ret, OP_callassigned);
  } else {
    RegassignNode *regnode = static_cast<RegassignNode *>(stmt);
    callassign = mirbuilder->CreateStmtCallRegassigned(func->puIdx, args, regnode->regIdx, OP_callassigned);
  }
  blknode->AppendStatementsFromBlock(LowerCallAssignedStmt(callassign));
}

bool CGLowerer::IsIntrinsicCallHandledAtLowerLevel(MIRIntrinsicID intrinsic) {
  switch (intrinsic) {
    case INTRN_MPL_ATOMIC_EXCHANGE_PTR:
    case INTRN_C_va_start:
      return true;
    default:
      return false;
  }
}

#if DEBUG
void CGLowerer::InsertRegassignsFromFormalParameters(MIRFunction *func) {
  MIRPregTable *pregtab = func->pregTab;
  // Allocate a new block and store instruction there
  // because inserting them into func->body make them lowered in LowerFunc()
  // which we want to avoid; the formal parameters must be 'dread'.
  // We cannot insert them after LowerFunc() because we need to know
  // pregs for formal parameters so that we can refer to them
  // during LowerFunc()
  //
  BlockNode *blk = mirModule.CurFuncCodeMemPool()->New<BlockNode>();
  for (FormalDef formalDef : func->formalDefVec) {
    MIRSymbol *s = formalDef.formalSym;
    MIRType *type = s->GetType();
    PrimType primType = type->GetPrimType();
    PregIdx pidx = pregtab->CreatePreg(primType);
    autos_to_pregs->insert(pair<StIdx, PregIdx>(s->GetStIdx(), pidx));
    pregs_to_autos->insert(pair<PregIdx, StIdx>(pidx, s->GetStIdx()));
    DreadNode *rhs = mirbuilder->CreateExprDread(type, 0, s);
    StmtNode *newStmt = mirbuilder->CreateStmtRegassign(primType, pidx, rhs);
    blk->InsertFirst(newStmt);
  }
  if (blk->GetFirst()) {
    new_entry_blk = blk;
  }
}

#endif

}  // namespace maplebe
