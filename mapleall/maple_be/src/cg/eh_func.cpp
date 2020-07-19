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

#include "eh_func.h"
#include "cg_func.h"
#include "cg.h"
#include "mir_builder.h"
#include "cg_lowerer.h"
#include "switch_lowerer.h"
#include "cg_assert.h"
#include "special_func.h"

namespace maplebe {

using namespace maple;

#define EHFUNCDEBUG 0

extern bool doItQuietly;

void EHJavaTry::DumpEHJavaTry(const MIRModule &mirModule) {
  this->javatry_node->Dump(&mirModule);
  endtry_node->Dump(&mirModule);
  for (uint32 i = 0; i < javacatch_vec.size(); i++) {
    CatchNode *javacatch = javacatch_vec[i];
    javacatch->Dump(&mirModule, 0);
  }
}

LabelIdx EHJavaTry::GetDefaultCatchLabidx() {
  for (MapleVector<CatchNode *>::iterator it = this->javacatch_vec.begin(); it != javacatch_vec.end(); it++) {
    CatchNode *javacatchnode = *it;
    const MapleVector<TyIdx> &exceptiontyidxvec = javacatchnode->exceptionTyIdxVec;
    for (MapleVector<TyIdx>::const_iterator etyit = exceptiontyidxvec.begin(); etyit != exceptiontyidxvec.end();
         etyit++) {
      MIRPtrType *pttype = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(*etyit));
      if (pttype->pointedTyIdx == (TyIdx)PTY_void) {
        CG_ASSERT(javacatchnode->GetPrev()->op == OP_label, "");
        LabelNode *lblnode = static_cast<LabelNode *>(javacatchnode->GetPrev());
        return lblnode->labelIdx;
      }
    }
  }
  return LabelIdx(0);
}

void EHThrow::ConvertThrowToRuntime(EHFunc *ehfunc, MIRType *sttype, BaseNode *arg) {
  MIRFunction *mirFunc = ehfunc->cgfunc->func;
  MIRModule *mirModule = mirFunc->module;
  // create a callee function
  // wrap __cxa_allocate_exception by __java_allocate_exception(dread st, typesize)
  // which would do:
  //  if (st is null) {
  //     throw_null_pointer_exception()
  //  }
  MapleVector<BaseNode *> alloccallargs(mirModule->memPoolAllocator.Adapter());
  alloccallargs.push_back(arg);  // drnode->CloneTree(mirModule) );
  ehfunc->cgfunc->becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetVoid());
  MIRFunction *calleefunc = mirModule->mirBuilder->GetOrCreateFunction(GetIntrinsicFuncName(INTRN_MCCThrowException), (TyIdx)(PTY_void));
  calleefunc->SetNoReturn();
  CallNode *callassign = mirModule->mirBuilder->CreateStmtCall(calleefunc->puIdx, alloccallargs);
  mirFunc->body->ReplaceStmt1WithStmt2(rethrow, callassign);
}

void EHThrow::ConvertThrowToRethrow(EHFunc *ehfunc) {
  MIRFunction *mirFunc = ehfunc->cgfunc->func;
  MIRModule *mirModule = mirFunc->module;
  MIRBuilder *mirbuilder = mirModule->mirBuilder;
  MapleVector<BaseNode *> urcallargs(mirModule->memPoolAllocator.Adapter());
  urcallargs.push_back(rethrow->uOpnd);
  MIRFunction *unfunc = mirbuilder->GetOrCreateFunction(GetIntrinsicFuncName(INTRN_MCCRethrowException), (TyIdx)PTY_void);
  unfunc->SetNoReturn();
  CallNode *callnode = mirbuilder->CreateStmtCall(unfunc->puIdx, urcallargs);
  mirFunc->body->ReplaceStmt1WithStmt2(rethrow, callnode);
}

void EHThrow::Lower(EHFunc *ehfunc) {
  BaseNode *opnd0 = rethrow->uOpnd;
  CG_ASSERT((opnd0->primType == PTY_a64 || opnd0->primType == PTY_ref), "except a dread of a point to get its type");
  CG_ASSERT(SIZEOFPTR == 8, "");
  MIRFunction *mirFunc = ehfunc->cgfunc->func;
  MIRModule *mirModule = mirFunc->module;
  MIRBuilder *mirbuilder = mirModule->mirBuilder;
  MIRSymbol *st = nullptr;
  BaseNode *arg = nullptr;
  MIRType *psttype = nullptr;
  switch (opnd0->op) {
    case OP_dread: {
      DreadNode *drnode = static_cast<DreadNode *>(opnd0);
      st = mirFunc->GetLocalOrGlobalSymbol(drnode->stIdx);
      psttype = st->GetType();
      arg = drnode->CloneTree(mirModule);
      break;
    }
    case OP_iread: {
      IreadNode *irnode = static_cast<IreadNode *>(opnd0);
      MIRPtrType *pointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(irnode->tyIdx));
      if (irnode->fieldID != 0) {
        MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
        MIRStructType *structty = nullptr;
        if (pointedty->GetKind() != kTypeJArray) {
          structty = static_cast<MIRStructType *>(pointedty);
        } else {
          // it's a Jarray type. using it's parent's field info: java.lang.Object
          structty = static_cast<MIRJarrayType *>(pointedty)->GetParentType();
        }
        CG_ASSERT(structty != nullptr, "structty is nullptr in EHThrow::Lower ");
        FieldPair thepair = structty->TraverseToField(irnode->fieldID);
        psttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
      } else {
        psttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
      }
      arg = irnode->CloneTree(mirModule);
      break;
    }
    case OP_regread: {
      RegreadNode *rrnode = static_cast<RegreadNode *>(opnd0);
      MIRPreg *preg = mirFunc->pregTab->PregFromPregIdx(rrnode->regIdx);
      CG_ASSERT(preg->primType == PTY_a64, "");
      psttype = preg->mirType;
      arg = rrnode->CloneTree(mirModule);
      break;
    }
    case OP_retype: {
      RetypeNode *retypenode = static_cast<RetypeNode *>(opnd0);
      psttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(retypenode->tyIdx);
      arg = retypenode->CloneTree(mirModule);
      break;
    }
    default:
      CG_ASSERT(false, " NYI throw something");
  }
  CHECK_FATAL(psttype, "psttype is null in EHThrow::Lower");
  if (psttype->typeKind != kTypePointer) {
    LogInfo::MapleLogger() << "Error in function " << mirFunc->GetName() << endl;
    rethrow->Dump(mirModule);
    LogInfo::MapleLogger() << "psttype is supposed to be Pointer, but is ";
    psttype->Dump(0);
  }
  CHECK_FATAL(psttype->typeKind == kTypePointer, "");
  MIRType *sttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(psttype)->pointedTyIdx);
  if (IsUnderJavaTry()) {
    // no need to generate a LSDA for this throw, just lower the throw to java runtime throw
    if (sttype->typeKind == kTypeClass) {
      ConvertThrowToRuntime(ehfunc, sttype, arg);
    } else {
      ConvertThrowToRethrow(ehfunc);
    }
  } else {
    // in this case the throw happens without a try...endtry wrapping it, need to generate lsda.
    // insert 2 labels before and after throw
    LabelNode *throwbeginlbl = mirbuilder->CreateStmtLabel(mirbuilder->CreateLabidx(mirFunc));
    LabelNode *throwendlbl = mirbuilder->CreateStmtLabel(mirbuilder->CreateLabidx(mirFunc));
    BlockNode *bodynode = mirFunc->body;
    bodynode->InsertBefore(rethrow, throwbeginlbl);
    bodynode->InsertAfter(rethrow, throwendlbl);
    start_label = throwbeginlbl;
    end_label = throwendlbl;
    if (sttype->typeKind == kTypeClass) {
      ConvertThrowToRuntime(ehfunc, sttype, arg);
    } else {
      ConvertThrowToRethrow(ehfunc);
    }
  }
}

EHFunc::EHFunc(CGFunc *func)
  : cgfunc(func),
    labelIdx(0),
    javatry_vec(func->funcscope_allocator_->Adapter()),
    eh_ty_table(func->funcscope_allocator_->Adapter()),
    ty2index_table(std::less<TyIdx>(), func->funcscope_allocator_->Adapter()),
    lsda_header(nullptr),
    lsda_callsite_table(nullptr),
    lsda_action_table(nullptr),
    exception_type(nullptr),
    rethrow_vec(func->funcscope_allocator_->Adapter()) {}

EHFunc *CGFunc::BuildEHFunc() {
  MIRFunction *mirFunc = this->func;
  MIRModule *mirModule = mirFunc->module;
  EHFunc *newehfunc = memPool->New<EHFunc>(this);
  this->ehfunc = newehfunc;
  EHJavaTry *lastJavatry = nullptr;  // record last javatry
  // curjavatry: record the current javatry wrapping the current statement,
  //             reset to null when meet a endtry
  EHJavaTry *curjavatry = nullptr;
  std::vector<std::pair<LabelIdx, CatchNode *>> catchvec;
  StmtNode *nextstmt = nullptr;

  // collect all try-catch blocks
  for (StmtNode *stmt = mirFunc->body->GetFirst(); stmt; stmt = nextstmt) {
    nextstmt = stmt->GetNext();
    Opcode op = stmt->op;
    switch (op) {
      case OP_try:
      case OP_javatry: {
        TryNode *javatrynode = static_cast<TryNode *>(stmt);
        EHJavaTry *ehjavatry = memPool->New<EHJavaTry>(funcscope_allocator_, javatrynode);
        lastJavatry = ehjavatry;
        curjavatry = ehjavatry;
        newehfunc->javatry_vec.push_back(ehjavatry);
        break;
      }
      case OP_endtry: {
        CHECK_FATAL(lastJavatry != nullptr, "");
        lastJavatry->endtry_node = stmt;
        lastJavatry = nullptr;
        curjavatry = nullptr;
        break;
      }
      case OP_catch:
      case OP_javacatch: {
        CatchNode *javacatchnode = static_cast<CatchNode *>(stmt);
        CG_ASSERT(stmt->GetPrev()->op == OP_label, "catch's previous node is not a label");
        LabelNode *labelstmt = static_cast<LabelNode *>(stmt->GetPrev());
        catchvec.push_back(std::pair<LabelIdx, CatchNode *>(labelstmt->labelIdx, javacatchnode));
        // rename the type of <* void> to <* Throwable>
        const MapleVector<TyIdx> &exceptiontyidxvec = javacatchnode->exceptionTyIdxVec;
        for (uint32 i = 0; i < exceptiontyidxvec.size(); i++) {
          MIRType *ehty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(javacatchnode->exceptionTyIdxVec[i]);
          CG_ASSERT(ehty->typeKind == kTypePointer, "");
          MIRPtrType *ehpointedty = static_cast<MIRPtrType *>(ehty);
          if (ehpointedty->pointedTyIdx == (TyIdx)PTY_void) {
            CG_ASSERT(mirFunc->module->throwableTyidx != TyIdx(0), "");
            javacatchnode->exceptionTyIdxVec[i] =
              becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetTypeFromTyIdx(mirModule->throwableTyidx))->tyIdx;
          }
        }
        break;
      }
      case OP_throw: {
        if (!cg->cgopt_.GenerateExceptionHandlingCode() || (cg->IsExclusiveEH() && cg->IsExclusiveFunc(mirFunc))) {
          // remove the statment
          BlockNode *bodynode = mirFunc->body;
          bodynode->RemoveStmt(stmt);
          break;
        }
        UnaryStmtNode *thrownode = static_cast<UnaryStmtNode *>(stmt);
        EHThrow *ehrethrow = memPool->New<EHThrow>(thrownode);
        ehrethrow->java_try = curjavatry;
        newehfunc->rethrow_vec.push_back(ehrethrow);
        break;
      }
      case OP_block:
        CG_ASSERT(false, "should've lowered earlier");
      default:
        break;
    }
  }

  newehfunc->BuildEHTypeTable(catchvec);
  newehfunc->InsertEHSwitchTable();
  newehfunc->InsertCxaBeginEndCatch(catchvec);

  // generate clean-up-BB
  newehfunc->GenerateCleanupLabel();

  if (newehfunc->NeedFullLSDA()) {
    newehfunc->CreateLSDA();
  } else if (newehfunc->HasThrow()) {
    newehfunc->LowerThrow();
  }
  if (cg->cgopt_.GenerateExceptionHandlingCode()) {
    newehfunc->CreateTypeInfoSt();
  }
  if (EHFUNCDEBUG) {
    newehfunc->DumpEHFunc();
  }
  return newehfunc;
}

bool EHFunc::NeedFullLSDA() {
  if (cgfunc->func->IsJava() || cgfunc->mirModule.srcLang == kSrcLangCPlusPlus) {
    return HasJavaTry();
  } else {
    return false;
  }
}

bool EHFunc::NeedFastLSDA() {
  if (cgfunc->func->IsJava()) {
    return !HasJavaTry();
  } else {
    return false;
  }
}

bool EHFunc::HasJavaTry() const {
  if (javatry_vec.size() > 0) {
    return true;
  } else {
    return false;
  }
}

void EHFunc::CreateTypeInfoSt() {
  MIRFunction *mirFunc = cgfunc->func;
  bool ctordefined = false;
  if (mirFunc->GetAttr(FUNCATTR_constructor) &&
      // skip clinit
      !mirFunc->GetAttr(FUNCATTR_static) && mirFunc->body) {
    ctordefined = true;
  }

  if (!ctordefined) {
    return;
  }

  MIRClassType *classtype = static_cast<MIRClassType *>(mirFunc->GetClassType());
  CG_ASSERT(classtype, "");
  if (classtype->methods.size() == 0 && classtype->fields.size() == 0) {
    return;
  }

  if (classtype->GetExceptionRootType() == nullptr) {
    return;  // not a exception type
  }
  MIRBuilder *mirbuilder = mirFunc->module->mirBuilder;
  cgfunc->becommon.BeGetOrCreatePointerType(GlobalTables::GetTypeTable().GetVoid());
  MIRClassType *roottype = classtype->GetExceptionRootType();
  mirbuilder->GetOrCreateTypeSymbol(classtype, roottype);
}

void EHFunc::LowerThrow() {
  // just lower without building LSDA
  for (EHThrow *rethrow : rethrow_vec) {
    BaseNode *opnd0 = rethrow->rethrow->uOpnd;
    // "except a dread of a point to get its type");
    CG_ASSERT(SIZEOFPTR == 8, "");
    MIRFunction *mirFunc = cgfunc->func;
    switch (opnd0->op) {
      case OP_retype: {
        RetypeNode *retypenode = static_cast<RetypeNode *>(opnd0);
        MIRType *psttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(retypenode->tyIdx);
        CG_ASSERT(psttype->typeKind == kTypePointer, "expecting a pointer type");
        MIRType *receiverType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(psttype)->pointedTyIdx);
        rethrow->ConvertThrowToRuntime(this, receiverType, retypenode->CloneTree(mirFunc->module));
        break;
      }
      case OP_dread: {
        DreadNode *drnode = static_cast<DreadNode *>(opnd0);
        MIRSymbol *st = mirFunc->GetLocalOrGlobalSymbol(drnode->stIdx);
        MIRType *psttype = st->GetType();
        CG_ASSERT(psttype->typeKind == kTypePointer, "");
        MIRType *sttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(psttype)->pointedTyIdx);
        rethrow->ConvertThrowToRuntime(this, sttype, drnode->CloneTree(mirFunc->module));
        break;
      }
      case OP_iread: {
        IreadNode *irnode = static_cast<IreadNode *>(opnd0);
        MIRType *receiverType = nullptr;
        MIRPtrType *receiverptrType = nullptr;
        if (irnode->fieldID != 0) {
          MIRPtrType *pointerty = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(irnode->tyIdx));
          MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
          MIRStructType *structty = nullptr;
          if (pointedty->GetKind() != kTypeJArray) {
            structty = static_cast<MIRStructType *>(pointedty);
          } else {
            // it's a Jarray type. using it's parent's field info: java.lang.Object
            structty = static_cast<MIRJarrayType *>(pointedty)->GetParentType();
          }
          CG_ASSERT(structty != nullptr, "structty is nullptr in EHFunc::LowerThrow");
          FieldPair thepair = structty->TraverseToField(irnode->fieldID);
          receiverptrType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first));
        } else {
          receiverptrType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(irnode->tyIdx));
          receiverptrType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(receiverptrType->pointedTyIdx));
        }
        CG_ASSERT(receiverptrType->typeKind == kTypePointer, "expecting a pointer type");
        receiverType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(receiverptrType->pointedTyIdx);
        rethrow->ConvertThrowToRuntime(this, receiverType, irnode->CloneTree(mirFunc->module));
        break;
      }
      case OP_regread: {
        RegreadNode *rrnode = static_cast<RegreadNode *>(opnd0);
        MIRPreg *preg = mirFunc->pregTab->PregFromPregIdx(rrnode->regIdx);
        CG_ASSERT(preg->primType == PTY_a64, "");
        MIRType *psttype = preg->mirType;
        CG_ASSERT(psttype->typeKind == kTypePointer, "");
        MIRType *sttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(psttype)->pointedTyIdx);
        rethrow->ConvertThrowToRuntime(this, sttype, rrnode->CloneTree(mirFunc->module));
        break;
      }
      case OP_constval: {
        ConstvalNode *constvalNode = static_cast<ConstvalNode *>(opnd0);
        rethrow->ConvertThrowToRuntime(this, nullptr, constvalNode->CloneTree(mirFunc->module));
        break;
      }
      default:
        CHECK_FATAL(false, "unexpected or NYI");
    }
  }
}

// catchvec is going to be released by the caller
void EHFunc::BuildEHTypeTable(const std::vector<std::pair<LabelIdx, CatchNode *>> &catchvec) {
  MIRBuilder *mirbuilder = cgfunc->func->module->mirBuilder;
  // merge javacatch to javatry
  for (auto ehjavatry : javatry_vec) {
    CG_ASSERT(ehjavatry->javacatch_vec.size() == 0, "");
    for (auto o : ehjavatry->javatry_node->offsets) {
      for (auto p : catchvec) {
        LabelIdx lbidx = p.first;
        CatchNode *jcatchnode = p.second;
        if (lbidx == o) {
          ehjavatry->javacatch_vec.push_back(jcatchnode);
          break;
        }
      }
    }
    CG_ASSERT(ehjavatry->javacatch_vec.size() == ehjavatry->javatry_node->offsets.size(), "");
  }

  if (catchvec.size() > 0) {
    // the first one assume to be <* void>
    TyIdx voidtyidx(PTY_void);
    eh_ty_table.push_back(voidtyidx);
    CG_ASSERT(eh_ty_table.size() == 1, "");
    ty2index_table[voidtyidx] = 0;

    // create void pointer and update becommon's size table
    cgfunc->becommon.UpdateTypeTable(GlobalTables::GetTypeTable().GetVoidPtr());
  }
  // create the type table for this function, just iterate each javacatch
  for (std::vector<std::pair<LabelIdx, CatchNode *>>::const_iterator it = catchvec.begin(); it != catchvec.end();
       it++) {
    CatchNode *jcatchnode = it->second;
    const MapleVector<TyIdx> &exceptiontyidxvec = jcatchnode->exceptionTyIdxVec;
    for (uint32 i = 0; i < exceptiontyidxvec.size(); i++) {
      MIRType *ptrty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(exceptiontyidxvec[i]);
      CG_ASSERT(ptrty->typeKind == kTypePointer, "");
      TyIdx ehtyIdx = static_cast<MIRPtrType *>(ptrty)->pointedTyIdx;
      if (ty2index_table.find(ehtyIdx) != ty2index_table.end()) {
        continue;
      }
      ty2index_table[ehtyIdx] = eh_ty_table.size();
      eh_ty_table.push_back(ehtyIdx);
      // create typinfost
      MIRClassType *catchtype = static_cast<MIRClassType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(ehtyIdx));
      MIRClassType *rootype = catchtype->GetExceptionRootType();
      CG_ASSERT(rootype != nullptr, "");
      mirbuilder->GetOrCreateTypeSymbol(catchtype, rootype);
    }
  }
}

void EHFunc::DumpEHFunc() {
  MIRModule &mirModule = *cgfunc->func->module;
  for (uint32 i = 0; i < this->javatry_vec.size(); i++) {
    LogInfo::MapleLogger() << "\n========== start " << i << " th eh:\n";
    EHJavaTry *ehjavatry = javatry_vec[i];
    ehjavatry->DumpEHJavaTry(mirModule);
    LogInfo::MapleLogger() << "========== end " << i << " th eh =========\n";
  }

  LogInfo::MapleLogger() << "\n========== start LSDA type table ========\n";
  for (uint32 i = 0; i < this->eh_ty_table.size(); i++) {
    LogInfo::MapleLogger() << i << " vector to ";
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(eh_ty_table[i])->Dump(0);
    LogInfo::MapleLogger() << "\n";
  }
  LogInfo::MapleLogger() << "========== end LSDA type table ========\n";

  LogInfo::MapleLogger() << "\n========== start type-index map ========\n";
  for (MapleMap<TyIdx, uint32>::iterator mapit = ty2index_table.begin(); mapit != ty2index_table.end(); mapit++) {
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(mapit->first)->Dump(0);
    LogInfo::MapleLogger() << " map to ";
    LogInfo::MapleLogger() << mapit->second << "\n";
  }
  LogInfo::MapleLogger() << "========== end type-index map ========\n";
}

// cleanup_label is an LabelNode, and placed just before end_label.
// cleanup_label is the first statement of cleanupbb.
// the layout of clean up code is:
// //return bb
//   ...
// //cleanup bb = lastbb->prev; cleanupbb->PrependBB(retbb)
//   cleanup_label:
//     ...
// //lastbb
//   end_label:
//     .cfi_endproc
//   .label.xx.end:
//     .size
void EHFunc::GenerateCleanupLabel() {
  MIRModule *mirModule = cgfunc->func->module;
  cgfunc->cleanup_label = mirModule->mirBuilder->CreateStmtLabel(CreateLabel(".LCLEANUP"));
  BlockNode *blocknode = cgfunc->func->body;
  blocknode->InsertBefore(cgfunc->end_label, cgfunc->cleanup_label);
}

// this is also the landing pad code.
void EHFunc::InsertEHSwitchTable() {
  MIRModule &mirModule = *cgfunc->func->module;
  BlockNode *blocknode = cgfunc->func->body;
  for (MapleVector<EHJavaTry *>::iterator tryit = javatry_vec.begin(); tryit != javatry_vec.end(); tryit++) {
    EHJavaTry *ehjavatry = *tryit;
    StmtNode *trynextstmt = ehjavatry->endtry_node->GetNext();
    StmtNode *posafterendtry = cgfunc->end_label->GetPrev();
    // get the next statement of the trynode. when no throw happend in try block, jump to the node directly
    if (false && trynextstmt) {  // trynextstmt is the statement of next endtry before inserting switch statement
      LabelNode *nothrowlabel = nullptr;
      if (trynextstmt->op == OP_label) {
        StmtNode *underlabelstmt = trynextstmt->GetNext();
        if (!underlabelstmt || (underlabelstmt->op != OP_javacatch && underlabelstmt->op != OP_catch)) {
          nothrowlabel = static_cast<LabelNode *>(trynextstmt);
        }
      } else {
        nothrowlabel = mirModule.mirBuilder->CreateStmtLabel(cgfunc->CreateLabel());
        blocknode->InsertAfter(ehjavatry->endtry_node, nothrowlabel);
      }
      if (nothrowlabel) {
        // when no throw happens, goto the nothrowlabel directly
        GotoNode *gotonode = mirModule.mirBuilder->CreateStmtGoto(OP_goto, nothrowlabel->labelIdx);
        blocknode->InsertAfter(ehjavatry->endtry_node, gotonode);
        posafterendtry = gotonode;
        ehjavatry->fallthru_goto = gotonode;
      }
    }
    // create a switch statement and insert after tryend;
    SwitchNode *switchnode = mirModule.CurFuncCodeMemPool()->New<SwitchNode>(&mirModule);
    LabelIdx dflabidx = ehjavatry->GetDefaultCatchLabidx();
    if (dflabidx == 0) {
      // create a new label as default, and if program excute here, error it
      dflabidx = cgfunc->func->labelTab->CreateLabel();
      cgfunc->func->labelTab->AddToStringLabelMap(dflabidx);
      StmtNode *dflbStmt = mirModule.mirBuilder->CreateStmtLabel(dflabidx);
      blocknode->InsertAfter(posafterendtry, dflbStmt);
      MIRFunction *calleefunc = mirModule.mirBuilder->GetOrCreateFunction("abort", (TyIdx)(PTY_void));
      MapleVector<BaseNode *> callargs(mirModule.memPoolAllocator.Adapter());
      CallNode *callexit = mirModule.mirBuilder->CreateStmtCall(calleefunc->puIdx, callargs);
      blocknode->InsertAfter(dflbStmt, callexit);
    }
    switchnode->defaultLabel = dflabidx;
    // create s special symbol that use the second return of __builtin_eh_return()
    MIRSymbol *secretst =
      mirModule.mirBuilder->CreateLocalDecl("__eh_index__", GlobalTables::GetTypeTable().GetInt32());
    switchnode->switchOpnd = mirModule.mirBuilder->CreateExprDread(secretst);
    // update switch node's cases
    for (MapleVector<CatchNode *>::iterator ctit = ehjavatry->javacatch_vec.begin();
         ctit != ehjavatry->javacatch_vec.end(); ctit++) {
      CatchNode *catchnode = *ctit;
      const MapleVector<TyIdx> &exceptiontyidxvec = catchnode->exceptionTyIdxVec;
      for (uint32 i = 0; i < exceptiontyidxvec.size(); i++) {
        MIRPtrType *pttype = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(exceptiontyidxvec[i]));
        MapleMap<TyIdx, uint32>::iterator ty2idxit = ty2index_table.find(pttype->pointedTyIdx);
        CHECK_FATAL(ty2idxit != ty2index_table.end(), "");
        uint32 tableidx = ty2idxit->second;
        CG_ASSERT(catchnode->GetPrev()->op == OP_label, "");
        LabelNode *catchlabelnode = static_cast<LabelNode *>(catchnode->GetPrev());
        CasePair p(tableidx, catchlabelnode->labelIdx);
        bool inserted = false;
        for (auto x : switchnode->switchTable) {
          if (x == p) {
            inserted = true;
            break;
          }
        }
        if (!inserted) {
          switchnode->switchTable.push_back(p);
        }
      }
    }
    CGLowerer belower(mirModule, cgfunc->becommon, cgfunc->func);
    SwitchLowerer switchlower(mirModule, switchnode, &belower,
                              cgfunc->funcscope_allocator_);
    blocknode->InsertBlockAfter(switchlower.LowerSwitch(), posafterendtry);
    ehjavatry->fallthru_goto = posafterendtry->GetNext();
  }
  if (!cgfunc->cg->DoItQuietly()) {
    cgfunc->func->Dump();
  }
}

LabelIdx EHFunc::CreateLabel(const char *cstr) {
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(cgfunc->func->stIdx.Idx());
  std::string *funcname = cgfunc->memPool->New<string>(funcSt->GetName());
  std::string labstr = funcname->append(cstr).append(to_string(labelIdx++));
  return cgfunc->func->GetOrCreateLablidxFromName(labstr);
}

// return the unconditional jump of the catchnode
StmtNode *EHFunc::GetLastStmtCatchBlock(BlockNode *funcbody, CatchNode *catchnode) {
  // find the first uncondition jump, or a label which would create a new block
  StmtNode *uncondstmt = catchnode->GetNext();
  CG_ASSERT(uncondstmt, "catch node doesn't have a successor");
  for (; uncondstmt; uncondstmt = uncondstmt->GetNext()) {
    Opcode op = uncondstmt->op;
    if (op == OP_goto || op == OP_return || op == OP_throw || op == OP_label) {
      break;
    }
    if (op == OP_call) {
      CallNode *callnode = static_cast<CallNode *>(uncondstmt);
      MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callnode->puIdx);
      if (IsEhTerminatingFunction(calleefunc->GetName())) {
        break;
      }
    }
  }
  CG_ASSERT(uncondstmt, "cannot find unconditional jump after javatry");
  return uncondstmt;
}

// think about moving this to BELowerer where LowerThrownval is already written
void EHFunc::InsertCxaBeginEndCatch(const vector<std ::pair<LabelIdx, CatchNode *>> &catchvec) {
  MIRModule &mirModule = *cgfunc->func->module;
  BlockNode *funcbody = cgfunc->func->body;
  for (std::vector<std::pair<LabelIdx, CatchNode *>>::const_iterator it = catchvec.begin(); it != catchvec.end();
       it++) {
    CatchNode *jcatchnode = it->second;
    /*
      For each java catch, we want
      void* e = __java_begin_catch(thrownval);
      ...
      __java_end_catch()
    */
    // insert __cxa_begin_catch right after the javacatch node and
    // __cxa_end_catch at the end of catch block(often ending with an uncondition jump)
    MIRType *tmp = GlobalTables::GetTypeTable().GetVoidPtr();
    CHECK_FATAL(tmp, "null ptr check");
    TyIdx voidpty = tmp->tyIdx;
    MIRFunction *calleefunc = mirModule.mirBuilder->GetOrCreateFunction(GetIntrinsicFuncName(INTRN_JAVA_BEGIN_CATCH), voidpty);
    MapleVector<BaseNode *> alloccallargs(mirModule.memPoolAllocator.Adapter());
    RegreadNode *retregread0 = mirModule.CurFuncCodeMemPool()->New<RegreadNode>();
    retregread0->regIdx = -kSregRetval0;
    retregread0->primType = PTY_a64;
    alloccallargs.push_back(retregread0);
    CallNode *callassign = mirModule.mirBuilder->CreateStmtCall(calleefunc->puIdx, alloccallargs);
    funcbody->InsertAfter(jcatchnode, callassign);
  }
}

void EHFunc::CreateLSDA() {
  MIRBuilder *mirbuilder = cgfunc->func->module->mirBuilder;
  BlockNode *bodynode = cgfunc->func->body;
  // create header
  LSDAHeader *lsdaheader = cgfunc->memPool->New<LSDAHeader>();
  LabelIdx lsdahdlblidx = CreateLabel("LSDAHD");  // LSDA head
  LabelNode *lsdahdlblnode = mirbuilder->CreateStmtLabel(lsdahdlblidx);
  lsdaheader->lsda_label = lsdahdlblnode;
  lsdaheader->lpstart_encoding = 0xff;
  lsdaheader->ttype_encoding = 0x9b;
  LabelIdx lsdattstartidx = CreateLabel("LSDAALLS");  // LSDA all start;
  LabelNode *lsdattlblnode = mirbuilder->CreateStmtLabel(lsdattstartidx);
  LabelIdx lsdattendidx = CreateLabel("LSDAALLE");  // LSDA all end;
  LabelNode *lsdacstelblnode = mirbuilder->CreateStmtLabel(lsdattendidx);
  lsdaheader->ttype_offset.start_offset = lsdattlblnode;
  lsdaheader->ttype_offset.end_offset = lsdacstelblnode;
  lsdaheader->callsite_encoding = 0x1;
  lsda_header = lsdaheader;
  // create callsite table
  LSDACallSiteTable *lsdacallsitetable = cgfunc->memPool->New<LSDACallSiteTable>(cgfunc->funcscope_allocator_);
  lsda_callsite_table = lsdacallsitetable;
  LabelIdx lsdacststartidx = CreateLabel("LSDACSTS");  // LSDA callsite table start;
  LabelNode *lsdacststartlabel = mirbuilder->CreateStmtLabel(lsdacststartidx);
  LabelIdx lsdacstendidx = CreateLabel("LSDACSTE");  // LSDA callsite table end;
  LabelNode *lsdacstendlabel = mirbuilder->CreateStmtLabel(lsdacstendidx);
  lsdacallsitetable->cs_table.start_offset = lsdacststartlabel;
  lsdacallsitetable->cs_table.end_offset = lsdacstendlabel;
  // create each callsite by iterating the EHJavaTry
  for (MapleVector<EHJavaTry *>::iterator tryit = javatry_vec.begin(); tryit != javatry_vec.end(); tryit++) {
    EHJavaTry *ehjavatry = *tryit;
    TryNode *javatrynode = ehjavatry->javatry_node;
    StmtNode *endtrynode = ehjavatry->endtry_node;
    // replace javatry with a label which is the callsite_start
    LabelIdx csstartlblidx = CreateLabel("LSDACS");
    LabelNode *cslblnode = mirbuilder->CreateStmtLabel(csstartlblidx);
    LabelIdx csendlblidx = CreateLabel("LSDACE");
    LabelNode *celblnode = mirbuilder->CreateStmtLabel(csendlblidx);
    bodynode->ReplaceStmt1WithStmt2(javatrynode, cslblnode);
    bodynode->ReplaceStmt1WithStmt2(endtrynode, celblnode);
    LabelNode *ladpadendlabel = nullptr;
    if (ehjavatry->fallthru_goto) {
      ladpadendlabel = mirbuilder->CreateStmtLabel(CreateLabel("LSDALPE"));
      bodynode->InsertBefore(ehjavatry->fallthru_goto, ladpadendlabel);
    } else {
      ladpadendlabel = celblnode;
    }
    mirbuilder->CreateStmtLabel(CreateLabel(""));
    LSDACallSite *lsdacallsite = cgfunc->memPool->New<LSDACallSite>();
    // When there is only one catch, the exception table is optimized.
    if (ehjavatry->javacatch_vec.size() == 1) {
      ladpadendlabel = static_cast<LabelNode *>(ehjavatry->javacatch_vec[0]->GetPrev());
    }
    lsdacallsite->Init(cgfunc->start_label, cslblnode, cslblnode, celblnode, cgfunc->start_label, ladpadendlabel, 0x1);
    ehjavatry->lsdacallsite = lsdacallsite;
    lsdacallsitetable->callsite_table.push_back(lsdacallsite);
  }

  for (MapleVector<EHThrow *>::iterator rethit = rethrow_vec.begin(); rethit != rethrow_vec.end(); rethit++) {
    EHThrow *rethrow = *rethit;
    // replace throw (void * obj) with call __java_rethrow and unwind resume
    rethrow->Lower(this);
    if (rethrow->HasLSDA()) {
      LSDACallSite *lsdacallsite = cgfunc->memPool->New<LSDACallSite>();
      lsdacallsite->Init(cgfunc->start_label, rethrow->start_label, rethrow->start_label, rethrow->end_label, nullptr,
                         nullptr, 0x0);
      lsdacallsitetable->callsite_table.push_back(lsdacallsite);
    }
  }
  // LSDAAction table
  CreateLSDAAction();
}

void EHFunc::CreateLSDAAction() {
  // iterate each javatry and its corresponding catch
  LSDAActionTable *actiontable = cgfunc->memPool->New<LSDAActionTable>(cgfunc->funcscope_allocator_);
  lsda_action_table = actiontable;

  for (uint32 i = 0; i < javatry_vec.size(); i++) {
    EHJavaTry *javatrynode = javatry_vec[i];
    LSDAAction *lastaction = nullptr;
    LSDACallSite *lsdacallsite = javatrynode->lsdacallsite;
    for (int32 j = javatrynode->javacatch_vec.size() - 1; j >= 0; j--) {
      CatchNode *catchnode = javatrynode->javacatch_vec[j];
      for (uint32 ii = 0; ii < catchnode->exceptionTyIdxVec.size(); ii++) {
        MIRPtrType *pttype = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(catchnode->exceptionTyIdxVec[ii]));
        if (pttype->pointedTyIdx == (TyIdx)PTY_void) {
          CG_ASSERT(j == static_cast<int32>(javatrynode->javacatch_vec.size() - 1), "wish default catch being the last catch");
          continue;
        }
        uint32 tyindex = ty2index_table[pttype->pointedTyIdx];  // get the index of pttype of eh_ty_table;
        CG_ASSERT(tyindex != 0, "");
        LSDAAction *lsdaaction = cgfunc->memPool->New<LSDAAction>(tyindex, lastaction == nullptr ? 0 : 0x7d);
        lastaction = lsdaaction;
        actiontable->action_table.push_back(lsdaaction);
      }
    }
    lsdacallsite->cs_action = (actiontable->action_table.size() - 1) * 2 + 1;
  }
}

EHFunc *CGFunc::BuildCppEHFunc() {
  MIRFunction *mirFunc = this->func;
  MIRModule *mirModule = mirFunc->module;
  EHFunc *newehfunc = memPool->New<EHFunc>(this);
  this->ehfunc = newehfunc;
  std::vector<EHJavaTry *> cpptryStack;  // top of stack is last cpptry
  std::unordered_map<LabelNode *, CppCatchNode *> label2catch_map;
  StmtNode *nextstmt = nullptr;

  // collect all try-catch blocks
  for (StmtNode *stmt = mirFunc->body->GetFirst(); stmt; stmt = nextstmt) {
    nextstmt = stmt->GetNext();
    Opcode op = stmt->op;
    switch (op) {
      case OP_cpptry: {
        TryNode *cpptrynode = static_cast<TryNode *>(stmt);
        EHJavaTry *ehcpptry = memPool->New<EHJavaTry>(funcscope_allocator_, cpptrynode);
        cpptryStack.push_back(ehcpptry);
        newehfunc->javatry_vec.push_back(ehcpptry);
        break;
      }
      case OP_endtry: {
        CHECK_FATAL(!cpptryStack.empty(), "BuildCppEHFunc: no matching cpptry for endtry");
        EHJavaTry *lastEHCppTry = cpptryStack.back();
        lastEHCppTry->endtry_node = stmt;
        cpptryStack.pop_back();
        break;
      }
      case OP_cppcatch: {
        CppCatchNode *cppcatchnode = static_cast<CppCatchNode *>(stmt);
        CG_ASSERT(stmt->GetPrev()->op == OP_label, "BuildCppEHFunc: cppcatch's previous node is not a label");
        LabelNode *labelstmt = static_cast<LabelNode *>(stmt->GetPrev());
        label2catch_map[labelstmt] = cppcatchnode;
        break;
      }
      case OP_block:
        CG_ASSERT(false, "should've lowered earlier");
      default:
        break;
    }
  }

  newehfunc->BuildCppEHTypeTable(label2catch_map);

  if (newehfunc->javatry_vec.size() > 0) {
    newehfunc->CreateCppLSDA(label2catch_map);
  }

  if (EHFUNCDEBUG) {
    newehfunc->DumpEHFunc();
  }
  return newehfunc;
}

// catchvec is going to be released by the caller
void EHFunc::BuildCppEHTypeTable(const std::unordered_map<LabelNode *, CppCatchNode *> &label2catch_map) {
  MIRBuilder *mirbuilder = cgfunc->func->module->mirBuilder;

  if (label2catch_map.size() > 0) {
    // the first one assume to be <void>
    TyIdx voidtyidx(PTY_void);
    eh_ty_table.push_back(voidtyidx);
    CG_ASSERT(eh_ty_table.size() == 1, "");
    ty2index_table[voidtyidx] = 0;

    // create void pointer and update becommon's size table
    cgfunc->becommon.UpdateTypeTable(GlobalTables::GetTypeTable().GetVoidPtr());
  }
}

void EHFunc::CreateCppLSDA(std::unordered_map<LabelNode *, CppCatchNode *> &label2catch_map) {
  // create map from each cpptry to its first catch
  std::unordered_map<TryNode *, std::pair<LabelNode *, CppCatchNode *>> try2catch_map;
  for (uint32 i = 0; i < javatry_vec.size(); i++) {
    EHJavaTry *ehcpptry = javatry_vec[i];
    TryNode *cpptrynode = ehcpptry->javatry_node;
    if (cpptrynode->offsets.size() != 0) {
      // find the LabelNode for the landing pad of this cpptry
      std::unordered_map<LabelNode *, CppCatchNode *>::iterator it = label2catch_map.begin();
      for (; it != label2catch_map.end(); it++) {
        if (it->first->labelIdx == cpptrynode->offsets[0]) {
          try2catch_map.insert(std::make_pair(cpptrynode, *it));
          break;
        }
      }
    }
  }

  MIRBuilder *mirbuilder = cgfunc->func->module->mirBuilder;
  BlockNode *bodynode = cgfunc->func->body;
  // create header
  LSDAHeader *lsdaheader = cgfunc->memPool->New<LSDAHeader>();
  LabelIdx lsdahdlblidx = CreateLabel("LSDAHD");  // LSDA head
  LabelNode *lsdahdlblnode = mirbuilder->CreateStmtLabel(lsdahdlblidx);
  lsdaheader->lsda_label = lsdahdlblnode;
  lsdaheader->lpstart_encoding = 0xff;
  lsdaheader->ttype_encoding = 0x9b;
  LabelIdx lsdattstartidx = CreateLabel("LSDAALLS");  // LSDA all start;
  LabelNode *lsdattlblnode = mirbuilder->CreateStmtLabel(lsdattstartidx);
  LabelIdx lsdattendidx = CreateLabel("LSDAALLE");  // LSDA all end;
  LabelNode *lsdacstelblnode = mirbuilder->CreateStmtLabel(lsdattendidx);
  lsdaheader->ttype_offset.start_offset = lsdattlblnode;
  lsdaheader->ttype_offset.end_offset = lsdacstelblnode;
  lsdaheader->callsite_encoding = 0x1;
  lsda_header = lsdaheader;
  // create callsite table
  LSDACallSiteTable *lsdacallsitetable = cgfunc->memPool->New<LSDACallSiteTable>(cgfunc->funcscope_allocator_);
  lsda_callsite_table = lsdacallsitetable;
  LabelIdx lsdacststartidx = CreateLabel("LSDACSTS");  // LSDA callsite table start;
  LabelNode *lsdacststartlabel = mirbuilder->CreateStmtLabel(lsdacststartidx);
  LabelIdx lsdacstendidx = CreateLabel("LSDACSTE");  // LSDA callsite table end;
  LabelNode *lsdacstendlabel = mirbuilder->CreateStmtLabel(lsdacstendidx);
  lsdacallsitetable->cs_table.start_offset = lsdacststartlabel;
  lsdacallsitetable->cs_table.end_offset = lsdacstendlabel;
  // create each callsite by iterating the EHJavaTry
  for (uint32 i = 0; i < javatry_vec.size(); i++) {
    EHJavaTry *ehcpptry = javatry_vec[i];
    TryNode *cpptrynode = ehcpptry->javatry_node;
    StmtNode *endtrynode = ehcpptry->endtry_node;
    // replace javatry with a label which is the callsite_start
    LabelIdx csstartlblidx = CreateLabel("LSDACS");
    LabelNode *cslblnode = mirbuilder->CreateStmtLabel(csstartlblidx);
    LabelIdx csendlblidx = CreateLabel("LSDACE");
    LabelNode *celblnode = mirbuilder->CreateStmtLabel(csendlblidx);
    bodynode->ReplaceStmt1WithStmt2(cpptrynode, cslblnode);
    bodynode->ReplaceStmt1WithStmt2(endtrynode, celblnode);
    LabelNode *ladpadendlabel = nullptr;
    std::unordered_map<TryNode *, std::pair<LabelNode *, CppCatchNode *>>::iterator it = try2catch_map.find(cpptrynode);
    if (it != try2catch_map.end()) {
      ladpadendlabel = it->second.first;
    }
    LSDACallSite *lsdacallsite = cgfunc->memPool->New<LSDACallSite>();
    lsdacallsite->Init(cgfunc->start_label, cslblnode, cslblnode, celblnode, ladpadendlabel ? cgfunc->start_label : 0, ladpadendlabel, 0x1);
    ehcpptry->lsdacallsite = lsdacallsite;
    lsdacallsitetable->callsite_table.push_back(lsdacallsite);
  }

  // LSDAAction table
  CreateCppLSDAAction(try2catch_map);
}

void EHFunc::CreateCppLSDAAction(std::unordered_map<TryNode *, std::pair<LabelNode *, CppCatchNode *>> &try2catch_map) {
  // iterate each cpptry and its corresponding catch
  LSDAActionTable *actiontable = cgfunc->memPool->New<LSDAActionTable>(cgfunc->funcscope_allocator_);
  lsda_action_table = actiontable;

  // determine if __TYPEINFO_TABLE__ exists
  GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName("__TYPEINFO_TABLE__");
  MIRSymbol *typeinfoTableSym = cgfunc->func->symTab->GetSymbolFromStrIdx(strIdx);
  bool hasTypeinfoTable = (typeinfoTableSym != nullptr);

  for (uint32 i = 0; i < javatry_vec.size(); i++) {
    EHJavaTry *ehcpptry = javatry_vec[i];
    TryNode *cpptrynode = ehcpptry->javatry_node;
    LSDAAction *lastaction = nullptr;
    LSDACallSite *lsdacallsite = ehcpptry->lsdacallsite;
    CppCatchNode *catchnode = nullptr;
    std::unordered_map<TryNode *, std::pair<LabelNode *, CppCatchNode *>>::iterator it = try2catch_map.find(cpptrynode);
    if (it != try2catch_map.end()) {
      catchnode = it->second.second;
    }
    MIRType *mirtype = nullptr;
    if (catchnode && catchnode->exceptionTyIdx.GetIdx() != 0) {
      mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(catchnode->exceptionTyIdx);
    }
    uint32 tyindex = hasTypeinfoTable ? 1 : 0;
    if (mirtype) {
      tyindex = ty2index_table[mirtype->tyIdx]; // get its index in eh_ty_table
    }
    LSDAAction *lsdaaction = cgfunc->memPool->New<LSDAAction>(tyindex, lastaction == nullptr ? 0 : 0x7d);
    lastaction = lsdaaction;
    actiontable->action_table.push_back(lsdaaction);
    lsdacallsite->cs_action = (actiontable->action_table.size() - 1) * 2 + 1;
    if (catchnode == nullptr) {
      lsdacallsite->cs_action = 0;
    }
  }
}

AnalysisResult *CgDoBuildEHFunc::Run(CGFunc *cgfunc, CgFuncResultMgr *m) {
  if (cgfunc->mirModule.srcLang == kSrcLangCPlusPlus) {
    cgfunc->BuildCppEHFunc();
  } else {
    cgfunc->BuildEHFunc();
  }
  return nullptr;
}

}  // namespace maplebe
