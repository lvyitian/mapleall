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

#include "clone.h"
#include "inline.h"
#include "mpl_logging.h"
#include "name_mangler.h"
#include <iostream>
#include <fstream>
using namespace std;
namespace maple {

// This phase replaces a function call site with the body of the called function.
//   Step 0: See if CALLEE have been inlined to CALLER once.
//   Step 1: Clone CALLEE's body.
//   Step 2: Rename symbols, labels, pregs.
//   Step 3: Replace symbols, labels, pregs.
//   Step 4: Null check 'this' and assign actuals to formals.
//   Step 5: Insert the callee'return jump dest label.
//   Step 6: Handle return values.
//   Step 7: Remove the successive goto statement and label statement in some circumstances.
//   Step 8: Replace the call-stmt with new CALLEE'body.
//   Step 9: Update inlined_times.

#define DEBUG_INLINE 0
int MInline::level = 0;
string MInline::inlineFuncList = "";
#define LIST_ONLY 1

static bool IsFinalMethod(const MIRFunction *mirFunc) {
  if (mirFunc == nullptr) {
    return false;
  }
  MIRClassType *ctype = static_cast<MIRClassType *>(mirFunc->GetClassType());
  // Return true if the method or its class is declared as final
  return (ctype != nullptr && (mirFunc->IsFinal() || ctype->IsFinal()));
}

// trim both leading and trailing space and tab
static void TrimString(std::string &str) {
  size_t pos = str.find_first_not_of(SPACE_TAB_STR);
  if (pos != std::string::npos) {
    str = str.substr(pos);
  } else {
    str.clear();
  }

  pos = str.find_last_not_of(SPACE_TAB_STR);
  if (pos != std::string::npos) {
    str = str.substr(0, pos + 1);
  }
}

void MInline::ApplyInlineListInfo() {
  if (MInline::inlineFuncList.empty()) {
    return;
  }

  std::ifstream infile(MInline::inlineFuncList);
  if (!infile.is_open()) {
    fprintf(stderr, "error open function list file %s.\n", MInline::inlineFuncList.c_str());
    return;
  }
  std::string str;
  GStrIdx calleeStridx;
  while (getline(infile, str)) {
    TrimString(str);
    if (str.size() == 0 || str[0] == COMMENTSIGN_STR) {
      continue;
    }

    MapleMap<GStrIdx, MapleSet<GStrIdx>>::iterator it2;
    if (str[0] != HYPHEN_STR) {
      calleeStridx = GlobalTables::GetStrTable().GetStrIdxFromName(str.c_str());
      MapleMap<GStrIdx, MapleSet<GStrIdx> *>::iterator it = inline_prefer.find(calleeStridx);
      if (it == inline_prefer.end()) {
        MapleSet<GStrIdx> *callerList = alloc.GetMemPool()->New<MapleSet<GStrIdx>>(alloc.Adapter());
        inline_prefer[calleeStridx] = callerList;
      }
    } else {
      size_t pos = str.find_first_not_of(APPOINT_STR);
      CHECK_FATAL(pos != std::string::npos, "cannot find '->' ");
      str = str.substr(pos);
      GStrIdx callerStridx = GlobalTables::GetStrTable().GetStrIdxFromName(str.c_str());
      MapleMap<GStrIdx, MapleSet<GStrIdx> *>::iterator it = inline_prefer.find(calleeStridx);
      it->second->insert(callerStridx);
    }
  }
  infile.close();

  // dump info
  if (DEBUG_INLINE > 2) {
    MapleMap<GStrIdx, MapleSet<GStrIdx> *>::iterator it;
    for (it = inline_prefer.begin(); it != inline_prefer.end(); it++) {
      GStrIdx calleeId = it->first;
      LogInfo::MapleLogger() << "callee " << calleeId.GetIdx() << " supposed to be inlined by";
      MapleSet<GStrIdx>::iterator it1;
      for (it1 = it->second->begin(); it1 != it->second->end(); it1++) {
        LogInfo::MapleLogger() << it1->GetIdx() << ",";
      }
      LogInfo::MapleLogger() << endl;
    }
  }
}

// Common rename function

uint32 MInline::RenameSymbols(MIRFunction *caller, MIRFunction *callee, uint32 inlinedTimes,
                              unordered_map<uint32, uint32> &staticOld2new) {
  uint32 symtabsize = callee->symTab->GetSymbolTableSize();
  uint32 stidxOff = caller->symTab->GetSymbolTableSize() - 1;
  for (uint32 i = 0; i < symtabsize; i++) {
    MIRSymbol *sym = callee->symTab->GetSymbolFromStIdx(i);
    if (!sym) {
      continue;
    }
    string syName(UNDERLINE_STR);
    // Use puIdx here instead of func name because our mangled func name can be
    // really long
    syName.append(to_string(callee->puIdx));
    syName.append(VERTICAL_LINE_STR);
    syName.append(sym->GetName());
    syName.append(UNDERLINE_STR);
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(syName + to_string(inlinedTimes));
    CHECK_FATAL(sym->GetScopeIdx(), "sym->GetScopeIdx() should be != 0");

    MIRSymbol *newSym = nullptr;
    if (sym->GetStorageClass() == kScPstatic) {
      // Convert static variables to global file-static ones
      if (inlinedTimes == 0) {
        newSym = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
        newSym->SetNameStridx(strIdx);
        newSym->SetStorageClass(kScFstatic);
        newSym->tyIdx = sym->tyIdx;
        newSym->sKind = sym->sKind;
        newSym->typeAttrs = sym->typeAttrs;
        newSym->value = sym->value;
        GlobalTables::GetGsymTable().AddToStringSymbolMap(newSym);
      } else {
        GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(syName + NUMBER_ZERO_STR);
        newSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
      }
      staticOld2new.insert(pair<uint32, uint32>(sym->stIdx.FullIdx(), newSym->stIdx.FullIdx()));
    } else {
      newSym = Clone::CloneLocalSymbol(sym, caller);
      newSym->SetNameStridx(strIdx);
      if (newSym->GetStorageClass() == kScFormal) {
        newSym->SetStorageClass(kScAuto);
      }
      newSym->isTmp = 1;
      newSym->ResetIsDeleted();
      if (!caller->symTab->AddStOutside(newSym)) {
        CHECK_FATAL(false, "Reduplicate names.");
      }
      CHECK_FATAL(newSym->GetStIndex() == i + stidxOff, "wrong symbol table index");
      CHECK_FATAL(caller->symTab->IsValidIdx(newSym->GetStIndex()), "symbol table index out of range");
    }
    if (DEBUG_INLINE > 2)
      LogInfo::MapleLogger() << "Renaming symbol: " << sym->GetName() << " New symbol: " << syName + to_string(inlinedTimes)
           << " index: " << newSym->GetStIndex() << endl;
  }
  return stidxOff;
}

static void UpdateIdx(StIdx &stIdx, uint32 stidxOff, unordered_map<uint32, uint32> &staticOld2new) {
  auto it = staticOld2new.find(stIdx.FullIdx());
  if (it != staticOld2new.end()) {
    stIdx.SetFullIdx(it->second);
  } else {
    stIdx.SetIdx(stIdx.Idx() + stidxOff);
  }
}

void MInline::ReplaceSymbols(BaseNode *bn, uint32 stidxOff, unordered_map<uint32, uint32> &staticOld2new) {
  if (bn == nullptr) {
    return;
  }

  if (bn->op == OP_block) {
    BlockNode *blk = static_cast<BlockNode *>(bn);
    for (StmtNode *stmt = blk->GetFirst(); stmt != nullptr; stmt = static_cast<StmtNode *>(stmt)->GetNext()) {
      ReplaceSymbols(stmt, stidxOff, staticOld2new);
    }
  } else if (bn->op == OP_dassign) {
    DassignNode *dsnode = static_cast<DassignNode *>(bn);
    // Skip globals.
    if (dsnode->stIdx.Islocal()) {
      UpdateIdx(dsnode->stIdx, stidxOff, staticOld2new);
    }
  } else if ((bn->op == OP_addrof || bn->op == OP_dread)) {
    AddrofNode *addrnode = static_cast<AddrofNode *>(bn);
    // Skip globals.
    if (addrnode->stIdx.Islocal()) {
      UpdateIdx(addrnode->stIdx, stidxOff, staticOld2new);
    }
  } else if (CallReturnVector *returnValues = bn->GetCallReturnVector()) {
    CHECK_FATAL(returnValues->size() <= 1, "multiple return values are not supported");
    // Skip globals.
    if (returnValues->size() == 1 && !(*returnValues)[0].second.IsReg() && (*returnValues)[0].first.Islocal()) {
      UpdateIdx((*returnValues)[0].first, stidxOff, staticOld2new);
    }
  } else if (bn->op == OP_foreachelem) {
    ForeachelemNode *foreachnode = static_cast<ForeachelemNode *>(bn);
    // Skip globals.
    if (foreachnode->elemStIdx.Idx()) {
      UpdateIdx(foreachnode->elemStIdx, stidxOff, staticOld2new);
    }
    if (foreachnode->arrayStIdx.Idx()) {
      UpdateIdx(foreachnode->arrayStIdx, stidxOff, staticOld2new);
    }
  } else if (bn->op == OP_doloop) {
    DoloopNode *doloopnode = static_cast<DoloopNode *>(bn);
    // Skip globals.
    if (!doloopnode->isPreg && doloopnode->doVarStIdx.Idx()) {
      UpdateIdx(doloopnode->doVarStIdx, stidxOff, staticOld2new);
    }
  }
  // Search for nested dassign/dread/addrof node that may include a symbol index.
  for (int32 i = 0; i < bn->NumOpnds(); i++) {
    ReplaceSymbols(bn->Opnd(i), stidxOff, staticOld2new);
  }
}

uint32 MInline::RenameLabels(MIRFunction *caller, const MIRFunction *callee, uint32 inlinedTimes) {
  uint32 labeltabsize = callee->labelTab->GetLabelTableSize();
  uint32 labidxOff = caller->labelTab->GetLabelTableSize() - 1;
  for (uint32 i = 1 /* label table start at 1. */; i < labeltabsize; i++) {
    string labelName = callee->labelTab->GetName(i);
    string newLableName(UNDERLINE_STR);
    newLableName.append(to_string(callee->puIdx));
    newLableName.append(VERTICAL_LINE_STR);
    newLableName.append(labelName);
    newLableName.append(UNDERLINE_STR);
    newLableName.append(to_string(inlinedTimes));
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(newLableName);
    LabelIdx labidx = caller->labelTab->AddLabel(strIdx);
    if (DEBUG_INLINE > 2) {
      LogInfo::MapleLogger() << "Renaming Label: " << labelName << " New Label: " << newLableName << ";" << endl;
    }
    CHECK_FATAL(labidx == i + labidxOff, "wrong label index");
  }
  return labidxOff;
}

#define MPLTOOL (1)
void MInline::ReplaceLabels(BaseNode *bn, uint32 labidxOff) {
// Now only mpltool would allow javatry in the callee.
#if MPLTOOL
  if (bn->op == OP_javatry) {
    TryNode *javatrynode = static_cast<TryNode *>(bn);
    for (uint32 i = 0; i < javatrynode->offsets.size(); i++) {
      javatrynode->offsets[i] += labidxOff;
    }
  }
#else
  CHECK_FATAL(bn->op != OP_javatry && bn->op != OP_try, "Java `try` not allowed");
#endif
  if (bn->op == OP_block) {
    BlockNode *blk = static_cast<BlockNode *>(bn);
    for (StmtNode *stmt = blk->GetFirst(); stmt != nullptr; stmt = static_cast<StmtNode *>(stmt)->GetNext()) {
      ReplaceLabels(stmt, labidxOff);
    }
  } else if (bn->IsCondBr()) {
    static_cast<CondGotoNode *>(bn)->offset += labidxOff;
  } else if (bn->op == OP_label) {
    static_cast<LabelNode *>(bn)->labelIdx += labidxOff;
  } else if (bn->op == OP_addroflabel) {
    static_cast<AddroflabelNode *>(bn)->offset += labidxOff;
  } else if (bn->op == OP_goto) {
    static_cast<GotoNode *>(bn)->offset += labidxOff;
  } else if (bn->op == OP_multiway || bn->op == OP_rangegoto) {
    ASSERT(false,
            "MInline::ReplaceLabels: OP_multiway and OP_rangegoto are "
            "not supported");
  } else if (bn->op == OP_switch) {
    SwitchNode *switchnode = static_cast<SwitchNode *>(bn);
    switchnode->defaultLabel += labidxOff;
    for (uint32 i = 0; i < switchnode->switchTable.size(); i++) {
      switchnode->switchTable[i].second += labidxOff;
    }
  } else if (bn->op == OP_doloop) {
    ReplaceLabels(static_cast<DoloopNode *>(bn)->doBody, labidxOff);
  } else if (bn->op == OP_foreachelem) {
    ReplaceLabels(static_cast<ForeachelemNode *>(bn)->loopBody, labidxOff);
  } else if (bn->op == OP_dowhile || bn->op == OP_while) {
    ReplaceLabels(static_cast<WhileStmtNode *>(bn)->body, labidxOff);
  } else if (bn->op == OP_if) {
    ReplaceLabels(static_cast<IfStmtNode *>(bn)->thenPart, labidxOff);
    if (static_cast<IfStmtNode *>(bn)->elsePart) {
      ReplaceLabels(static_cast<IfStmtNode *>(bn)->elsePart, labidxOff);
    }
  }
}

void MInline::RenamePregs(MIRFunction *caller, MIRFunction *callee, unordered_map<PregIdx, PregIdx> &pregOld2new) {
  MapleVector<MIRPreg *> &tab = callee->pregTab->pregTable;
  for (uint32 i = 1; i < tab.size(); i++) {
    MIRPreg *mirpreg = tab[i];
    PregIdx idx = 0;
    if (mirpreg->primType == PTY_ptr || mirpreg->primType == PTY_ref) {
      idx = caller->pregTab->ClonePreg(mirpreg);
    } else {
      idx = caller->pregTab->CreatePreg(mirpreg->primType);
    }
    pregOld2new.insert(pair<PregIdx, PregIdx>(i, idx));
  }
}

static PregIdx GetNewPregIdx(PregIdx regIdx, unordered_map<PregIdx, PregIdx> &pregOld2new) {
  auto it = pregOld2new.find(regIdx);
  CHECK_FATAL(it != pregOld2new.end(), "Unable to find the regIdx to replace");
  return it->second;
}

void MInline::ReplacePregs(BaseNode *bn, unordered_map<PregIdx, PregIdx> &pregOld2new) {
  if (bn == nullptr) {
    return;
  }

  switch (bn->op) {
    case OP_block: {
      BlockNode *blk = static_cast<BlockNode *>(bn);
      for (StmtNode *stmt = blk->GetFirst(); stmt != nullptr; stmt = static_cast<StmtNode *>(stmt)->GetNext()) {
        ReplacePregs(stmt, pregOld2new);
      }
      break;
    }
    case OP_regassign: {
      RegassignNode *regassign = static_cast<RegassignNode *>(bn);
      regassign->regIdx = GetNewPregIdx(regassign->regIdx, pregOld2new);
      break;
    }
    case OP_regread: {
      RegreadNode *regread = static_cast<RegreadNode *>(bn);
      regread->regIdx = GetNewPregIdx(regread->regIdx, pregOld2new);
      break;
    }
    case OP_doloop: {
      DoloopNode *doloop = static_cast<DoloopNode *>(bn);
      if (doloop->isPreg) {
        PregIdx oldIdx = (PregIdx)doloop->doVarStIdx.FullIdx();
        doloop->doVarStIdx.SetFullIdx(static_cast<uint32>(GetNewPregIdx(oldIdx, pregOld2new)));
      }
      break;
    }
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_icallassigned:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned: {
      CallReturnVector *retvec = bn->GetCallReturnVector();
      CHECK_FATAL(retvec != nullptr, "retvec is nullptr in MInline::ReplacePregs");
      for (uint32 i = 0; i < retvec->size(); i++) {
        CallReturnPair &callpair = (*retvec)[i];
        if (callpair.second.IsReg()) {
          PregIdx oldIdx = callpair.second.GetPregidx();
          callpair.second.SetPregidx(GetNewPregIdx(oldIdx, pregOld2new));
        }
      }
      break;
    }

    default:
      break;
  }

  for (int32 i = 0; i < bn->NumOpnds(); i++) {
    ReplacePregs(bn->Opnd(i), pregOld2new);
  }
}

LabelIdx MInline::CreateReturnLabel(MIRFunction *caller, const MIRFunction *callee, uint32 inlinedTimes) {
  string labelName(UNDERLINE_STR);
  labelName.append(to_string(callee->puIdx));
  labelName.append(VERTICAL_LINE_STR);
  labelName.append(RETURNLOC_STR);
  labelName.append(to_string(inlinedTimes));
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(labelName);
  LabelIdx labidx = caller->labelTab->AddLabel(strIdx);
  return labidx;
}

void MInline::UpdateReturnStmts(MIRFunction *caller, BlockNode *newBody, LabelIdx retLabidx, CallReturnVector &returnValues,
                                int &retCount, GotoNode *&lastGoto) {
  CHECK_FATAL(newBody, "expecting a BlockNode, found nullptr");
  // For callee: return f ==>
  //             rval = f; goto label x;
  for (StmtNode *stmt = newBody->GetFirst(); stmt != nullptr; stmt = static_cast<StmtNode *>(stmt)->GetNext()) {
    switch (stmt->op) {
      case OP_foreachelem:
        UpdateReturnStmts(caller, static_cast<ForeachelemNode *>(stmt)->loopBody, retLabidx, returnValues, retCount, lastGoto);
        break;
      case OP_doloop:
        UpdateReturnStmts(caller, static_cast<DoloopNode *>(stmt)->doBody, retLabidx, returnValues, retCount, lastGoto);
        break;
      case OP_dowhile:
      case OP_while:
        UpdateReturnStmts(caller, static_cast<WhileStmtNode *>(stmt)->body, retLabidx, returnValues, retCount, lastGoto);
        break;
      case OP_if:
        UpdateReturnStmts(caller, static_cast<IfStmtNode *>(stmt)->thenPart, retLabidx, returnValues, retCount, lastGoto);
        if (static_cast<IfStmtNode *>(stmt)->elsePart) {
          UpdateReturnStmts(caller, static_cast<IfStmtNode *>(stmt)->elsePart, retLabidx, returnValues, retCount, lastGoto);
        }
        break;
      case OP_return: {
        CHECK_FATAL(returnValues.size() <= 1, "multiple return values are not supported");
        retCount++;
        GotoNode *gotonode = builder_.CreateStmtGoto(OP_goto, retLabidx);
        lastGoto = gotonode;
        if (returnValues.size() == 1) {
          BaseNode *bn = static_cast<NaryStmtNode *>(stmt)->Opnd(0);
          StmtNode *dStmt = nullptr;
          if (!returnValues[0].second.IsReg()) {
            dStmt = builder_.CreateStmtDassign(returnValues[0].first, returnValues[0].second.GetFieldid(), bn);
          } else {
            PregIdx pregidx = returnValues[0].second.GetPregidx();
            MIRPreg *mirpreg = caller->pregTab->PregFromPregIdx(static_cast<uint32>(pregidx));
            dStmt = builder_.CreateStmtRegassign(mirpreg->primType, pregidx, bn);
          }
          newBody->ReplaceStmt1WithStmt2(stmt, dStmt);
          newBody->InsertAfter(dStmt, gotonode);
        } else {
          newBody->ReplaceStmt1WithStmt2(stmt, gotonode);
        }
        break;
      }
      default:
        break;
    }
  }
}

// Inline CALLEE into CALLER.
void MInline::PerformInline(MIRFunction *caller, BlockNode *enclosingBlk, CallNode *callStmt, MIRFunction *callee) {
  if (callee->body->IsEmpty()) {
    enclosingBlk->RemoveStmt(callStmt);
    return;
  }

  if (LIST_ONLY) {
    GStrIdx calleeStridx = GlobalTables::GetStrTable().GetStrIdxFromName(callee->GetName());
    GStrIdx callerStridx = GlobalTables::GetStrTable().GetStrIdxFromName(caller->GetName());
    MapleMap<GStrIdx, MapleSet<GStrIdx> *>::iterator it = inline_prefer.find(calleeStridx);
    // match the config in inlineFuncList
    if (it != inline_prefer.end()) {
      MapleSet<GStrIdx> *callerList = (it->second);

      if (callee->GetAttr(FUNCATTR_virtual) && !IsFinalMethod(callee) && !callerList) {
        CHECK_FATAL(0, "expect caller info for %s in Inline phase!\n", callee->GetName().c_str());
      }

      if (callerList && !(callerList->empty()) && callerList->find(callerStridx) == callerList->end()) {
        return;
      }
    }
  }

  // Step 0: See if CALLEE have been inlined to CALLER once.
  uint32 inlinedTimes;
  if (inline_times_map_.find(callee) != inline_times_map_.end()) {
    inlinedTimes = inline_times_map_[callee];
  } else {
    inlinedTimes = 0;
  }
  // Step 1: Clone CALLEE's body.
  BlockNode *newBody = callee->body->CloneTree(&mod_);
  // Step 2: Rename symbols, labels, pregs
  unordered_map<uint32, uint32> staticOld2new;
  uint32 stidxOff = RenameSymbols(caller, callee, inlinedTimes, staticOld2new);
  uint32 labidxOff = RenameLabels(caller, callee, inlinedTimes);
  unordered_map<PregIdx, PregIdx> pregOld2new;
  RenamePregs(caller, callee, pregOld2new);
  // Step 3: Replace symbols, labels, pregs
  CHECK_FATAL(newBody, "null ptr check ");
  ReplaceSymbols(newBody, stidxOff, staticOld2new);
  ReplaceLabels(newBody, labidxOff);
  ReplacePregs(newBody, pregOld2new);

  // Step 4: Null check 'this' and assign actuals to formals.
  CHECK_FATAL(static_cast<uint32>(callStmt->NumOpnds()) == callee->formalDefVec.size(),
         "# formal arguments != # actual arguments");
  if (callee->formalDefVec.size() > 0 && callee->formalDefVec[0].formalSym->GetName() == THIS_STR) {
    UnaryStmtNode *nullcheck = mod_.CurFuncCodeMemPool()->New<UnaryStmtNode>(OP_assertnonnull);
    nullcheck->uOpnd = callStmt->Opnd(0);
    newBody->InsertBefore(newBody->GetFirst(), nullcheck);
  }
  for (int32 i = 0; i < callStmt->NumOpnds(); i++) {
    BaseNode *bn = callStmt->Opnd(i);
    MIRSymbol *newFormal = caller->symTab->GetSymbolFromStIdx(callee->formalDefVec[i].formalSym->GetStIndex() + stidxOff);
    DassignNode *stmt = builder_.CreateStmtDassign(newFormal, 0, bn);
    newBody->InsertBefore(newBody->GetFirst(), stmt);
  }

  // Step 5: Insert the callee'return jump dest label.
  // For caller: a = foo() ==>
  //             a = foo(); label x;
  LabelIdx retLabidx;
  // record the created label
  StmtNode *labelStmt = nullptr;
  if (callStmt->GetNext() && callStmt->GetNext()->op == OP_label) {
    // if the next stmt is a label, just reuse it
    LabelNode *nextlabel = static_cast<LabelNode *>(callStmt->GetNext());
    retLabidx = nextlabel->labelIdx;
  } else {
    retLabidx = CreateReturnLabel(caller, callee, inlinedTimes);
    labelStmt = builder_.CreateStmtLabel(retLabidx);
    newBody->AddStatement(labelStmt);
  }

  // Step 6: Handle return values.
  // Find the rval of call-stmt
  // calcute number of return stmt in CALLEE'body
  int retCount = 0;
  // record the last return stmt in CALLEE'body
  GotoNode *lastGoto = nullptr;
  CallReturnVector returnValues(alloc.Adapter());
  if (callStmt->op == OP_callassigned || callStmt->op == OP_virtualcallassigned ||
      callStmt->op == OP_superclasscallassigned || callStmt->op == OP_interfacecallassigned) {
    returnValues = static_cast<CallNode *>(callStmt)->returnValues;
    CHECK_FATAL(returnValues.size() <= 1, "multiple return values are not supported");
  }
  UpdateReturnStmts(caller, newBody, retLabidx, returnValues, retCount, lastGoto);

  // Step 6.5: remove the successive goto statement and label statement in some circumstances.
  // There is no return stmt in CALLEE'body, if we have create a new label in Step5, remove it.
  if (retCount == 0) {
    if (labelStmt) {
      newBody->RemoveStmt(labelStmt);
    }
  } else {
    // There are one or more goto stmt, remove the successive goto stmt and label stmt,
    // if there is only one return stmt, we can remove the label created in Step5, too.
    CHECK_FATAL(lastGoto, "there should be at least one goto statement");
    if (labelStmt) {
      // if we have created a new label in Step5, then last_goto->next == label_stmt means they are successive.
      if (lastGoto->GetNext() == labelStmt) {
        newBody->RemoveStmt(lastGoto);
        if (retCount == 1) {
          newBody->RemoveStmt(labelStmt);
        }
      }
    } else {
      // if we haven't created a new label in Step5, then new_body->last == last_goto means they are successive.
      if (newBody->GetLast() == lastGoto) {
        newBody->RemoveStmt(lastGoto);
      }
    }
  }

  // Step 7: Replace the call-stmt with new CALLEE'body.
  // begin inlining function
  std::string beginCmt("inlining begin: FUNC ");
  beginCmt.append(callee->GetName());
  enclosingBlk->InsertBefore(callStmt, builder_.CreateStmtComment(beginCmt));

  // end inlining function
  std::string endCmt("inlining end: FUNC ");
  endCmt.append(callee->GetName());
  if (enclosingBlk->GetLast() != nullptr && callStmt != enclosingBlk->GetLast()) {
    CHECK_FATAL(callStmt->GetNext(), "null ptr check");
  }
  enclosingBlk->InsertAfter(callStmt, builder_.CreateStmtComment(endCmt));
  CHECK_FATAL(callStmt->GetNext(), "null ptr check");
  enclosingBlk->ReplaceStmtWithBlock(callStmt, newBody);

  // Step 8: Update inlined_times.
  inline_times_map_[callee] = inlinedTimes + 1;
}

bool MInline::SpendBudget(BaseNode *bn, int32 &budget, unordered_map<MIRFunction *, int32> &toInline, int inlineLevel,
                          MIRFunction *curfunc) {
  if (bn == nullptr) {
    return true;
  }

  Opcode op = bn->op;
  // For a more complex node (ex. while), estimate the extra nodes that would
  // result after lowering it (ex. extra brfalse and brtrue nodes) and decrease
  // the budget accordingly
  switch (op) {
    case OP_block: {
      BlockNode *blk = static_cast<BlockNode *>(bn);
      for (StmtNode *stmt = blk->GetFirst(); stmt != nullptr; stmt = static_cast<StmtNode *>(stmt)->GetNext()) {
        if (!SpendBudget(stmt, budget, toInline, inlineLevel, curfunc)) {
          return false;
        }
      }
      break;
    }
    case OP_switch: {
      if (inlineLevel < 2) {
        return false;
      }
      SwitchNode *n = static_cast<SwitchNode *>(bn);
      budget -= static_cast<int32>(n->switchTable.size() + 1);
      break;
    }
    case OP_multiway: {
      if (inlineLevel < 2) {
        return false;
      }
      MultiwayNode *n = static_cast<MultiwayNode *>(bn);
      budget -= static_cast<int32>(n->multiWayTable.size() + 1);
      break;
    }
    case OP_rangegoto: {
      if (inlineLevel < 2) {
        return false;
      }
      RangegotoNode *n = static_cast<RangegotoNode *>(bn);
      budget -= static_cast<int32>(n->rangegotoTable.size() + 1);
      break;
    }
    case OP_foreachelem:
    case OP_doloop:
      if (inlineLevel < 2) {
        return false;
      }
      budget -= 4;
      break;
    case OP_while:
      if (inlineLevel < 2) {
        return false;
      }
      budget -= 2;
      break;
    case OP_goto:
    case OP_brfalse:
    case OP_brtrue:
    case OP_dowhile:
    case OP_if:
      if (inlineLevel < 2) {
        return false;
      }
      budget--;
      break;
    case OP_intrinsiccall:
    case OP_intrinsiccallwithtype:
    case OP_xintrinsiccall:
    case OP_intrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned:
    case OP_xintrinsiccallassigned:
      if (inlineLevel <= 2) {
        return false;
      }
      budget -= 4;
      break;
    // call is okay, if inlinable we need to have the budget for the body.
    case OP_virtualcall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_virtualcallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_call:
    case OP_callassigned: {
      CallNode *callStmt = static_cast<CallNode *>(bn);
      MIRFunction *callee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callStmt->puIdx);
      auto f = toInline.find(callee);
      if (f != toInline.end()) {
        // decrease budget by the cost of the body of callee
        budget -= f->second;
      } else {
        budget--;
      }
      break;
    }
    case OP_throw:
      if (inlineLevel < 2) {
        return false;
      }
      budget -= 4;
      break;
    case OP_intrinsicop:
    case OP_intrinsicopwithtype:
      if (inlineLevel < 2) {
        return false;
      }
      budget -= 4;
      break;
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_customcall:
    case OP_polymorphiccall:
    case OP_jstry:
    case OP_javatry:
    case OP_try:
    case OP_cpptry:
    case OP_jscatch:
    case OP_javacatch:
    case OP_catch:
    case OP_cppcatch:
    case OP_finally:
    case OP_cleanuptry:
    case OP_endtry:
    case OP_gosub:
    case OP_retsub:
    case OP_stackmallocjarray:
    case OP_stackmalloc:
    // With sync block
    case OP_syncenter:
    case OP_syncexit:
      return false;
    case OP_membaracquire:
    case OP_membarrelease:
    case OP_membarstoreload:
    case OP_membarstorestore:
      if (curfunc->IsConstructor()) {
        return false;
      }
      budget--;
      break;
    case OP_comment:
      // skip
      break;
    default:
      budget--;
  }
  if (budget < 0) {
    return false;
  }
  for (int32 i = 0; i < bn->NumOpnds(); i++) {
    if (!SpendBudget(bn->Opnd(i), budget, toInline, inlineLevel, curfunc)) {
      return false;
    }
  }
  return true;
}

static void MarkParent(CGNode *node) {
  for (MapleSet<CGNode *>::iterator it = node->CallerBegin(); it != node->CallerEnd(); it++) {
    CGNode *parent = *it;
    parent->must_not_be_inlined = true;
  }
}

static bool StartWith(const string &str, const string &head) {
  return str.compare(0, head.size(), head) == 0;
}

bool MInline::IsSimpleFunc(MIRFunction *func) const {
  StmtNode *stmt = func->body->GetFirst();
  StmtNode *next = nullptr;
  uint32 dassignNum = 0;
  uint32 callNum = 0;
  uint32 stmtNum = 0;
  while (stmt) {
    next = stmt->GetNext();
    Opcode op = stmt->op;
    switch (op) {
      case OP_callassigned:
        callNum++;
        break;
      case OP_dassign:
        dassignNum++;
        break;
      case OP_comment:
        break;
      case OP_intrinsiccallwithtype: {
        IntrinsiccallNode *callnode = static_cast<IntrinsiccallNode *>(stmt);
        if (callnode->intrinsic == INTRN_JAVA_CLINIT_CHECK) {
          break;
        }
      }
      default:
        stmtNum++;
        break;
    }

    if (stmtNum > 3) {
      return false;
    }
    if (dassignNum > 4 || callNum > 1) {
      return false;
    }
    stmt = next;
  }
  return true;
}

bool MInline::NodeInlinable(CGNode *node) {
  MIRFunction *func = node->GetMIRFunction();
  string name = func->GetName();
  if (node->must_not_be_inlined) {
    node->must_not_be_inlined = true;
    if (node->HasCaller()) {
      MarkParent(node);
    }
    if (func->GetAttr(FUNCATTR_synthetic)) {
      node->must_not_be_inlined = false;
    } else {
      if (DEBUG_INLINE) {
        LogInfo::MapleLogger() << "[INLINE_FAILED] " << name << endl;
      }
      return false;
    }
  }

  if (!func->GetAttr(FUNCATTR_static) && !func->GetAttr(FUNCATTR_final) && !func->GetAttr(FUNCATTR_private) &&
      !func->GetAttr(FUNCATTR_public) && !func->IsPackagePrivate()) {
    return false;
  }

  // Keep in sync with mapleall/maple_ir/include/func_attrs.def
  // Allow FUNCATTR: static, final, private, public, noexcept
  if (func->GetAttr(FUNCATTR_abstract) || func->GetAttr(FUNCATTR_const) || func->GetAttr(FUNCATTR_critical_native) ||
      func->GetAttr(FUNCATTR_declared_synchronized) || func->GetAttr(FUNCATTR_fast_native) ||
      func->GetAttr(FUNCATTR_native) || func->GetAttr(FUNCATTR_synchronized) || func->GetAttr(FUNCATTR_weak)) {
    return false;
  }

  return true;
}

void MInline::CanInline(CGNode *node, unordered_map<MIRFunction *, int32> &toInline) {
  MIRFunction *func = node->GetMIRFunction();

  int inlineLevel = -1;
  if (func == nullptr || func->body == nullptr) {
    return;
  }

  if (!NodeInlinable(node)) {
    return;
  }

  // optimized function are of high priority+
  if (LIST_ONLY) {
    GStrIdx calleeStridx = GlobalTables::GetStrTable().GetStrIdxFromName(func->GetName());
    MapleMap<GStrIdx, MapleSet<GStrIdx> *>::iterator it2 = inline_prefer.find(calleeStridx);
    if (it2 != inline_prefer.end()) {
      inlineLevel = 3;
    }
  }

  if (inlineLevel != 3) {
    return;
  }

  // Keep in sync with mapleall/maple_ir/include/func_attrs.def
  // Allow FUNCATTR: static, final, private, public, noexcept
  if (func->GetAttr(FUNCATTR_declared_synchronized) || func->GetAttr(FUNCATTR_abstract) ||
      (func->GetAttr(FUNCATTR_bridge) && inlineLevel <= 2) || func->GetAttr(FUNCATTR_const) ||
      func->GetAttr(FUNCATTR_local) || func->GetAttr(FUNCATTR_protected) || func->GetAttr(FUNCATTR_extern) ||
      func->GetAttr(FUNCATTR_generic) || func->GetAttr(FUNCATTR_implicit) || func->GetAttr(FUNCATTR_interface) ||
      func->GetAttr(FUNCATTR_native) || func->GetAttr(FUNCATTR_critical_native) ||
      func->GetAttr(FUNCATTR_fast_native) || func->GetAttr(FUNCATTR_protected) || func->GetAttr(FUNCATTR_strict) ||
      func->GetAttr(FUNCATTR_synchronized) || (func->GetAttr(FUNCATTR_synthetic) && inlineLevel <= 2) ||
      func->GetAttr(FUNCATTR_varargs) || (func->GetAttr(FUNCATTR_virtual) && inlineLevel <= 2) ||
      func->GetAttr(FUNCATTR_nosideeffect) || func->GetAttr(FUNCATTR_pure) ||
      (func->GetAttr(FUNCATTR_weak) && func->GetName() != std::string(NameMangler::kJavaLangObjectStr)+NameMangler::kInitSuffix)) {
    return;
  }

  int32 budget = max_budget;
  if (SpendBudget(func->body, budget, toInline, inlineLevel, func) && budget >= 0) {
    // func is inlinable and we have enough budget left
    int32 cost = max_budget - budget;
    toInline.insert(pair<MIRFunction *, int32>(func, cost));
    if (DEBUG_INLINE > 2) {
      LogInfo::MapleLogger() << "-> can inline '" << func->GetName() << "', cost " << cost << endl;
    }
  }

  if (DEBUG_INLINE && budget < 0) {
    LogInfo::MapleLogger() << "[INLINE_FAILED] [SpendBudget] " << func->GetName() << endl;
  }
}

void MInline::CheckCalleeAndInline(MIRFunction *caller, BlockNode *enclosingBlk, CallNode *callStmt,
                                   MIRFunction *callee) {
  if (!callee->body) {
    return;
  }
  CGNode *node = cg_->GetCGNode(callee);
  if (!node || !NodeInlinable(node)) {
    return;
  }

  const string &name = callee->GetName();
  if (name != (std::string(NameMangler::kJavaUtil) + "ArrayList_3B_7Csize_7C_28_29I")) {
    return;
  }

  if ((IsFinalMethod(callee) || callStmt->op == OP_callassigned || callStmt->op == OP_call) && IsSimpleFunc(callee)) {
    mod_.SetCurFunction(caller);
    PerformInline(caller, enclosingBlk, callStmt, callee);
  }
}

void MInline::InlineCalls(const CGNode *node, unordered_map<MIRFunction *, int32> &toInline) {
  MIRFunction *func = node->GetMIRFunction();
  if (func == nullptr || func->body == nullptr) {
    return;
  }
  if (toInline.size() == 0) {
    return;
  }
  InlineCallsBlock(func, func->body, func->body, toInline);
}

void MInline::InlineCallsBlock(MIRFunction *func, BlockNode *enclosingBlk, BaseNode *bn,
                               unordered_map<MIRFunction *, int32> &toInline) {
  if (bn->op == OP_block) {
    BlockNode *blk = static_cast<BlockNode *>(bn);
    for (StmtNode *stmt = blk->GetFirst(); stmt != nullptr; stmt = stmt->GetNext()) {
      InlineCallsBlock(func, blk, stmt, toInline);
    }
  } else if (bn->op == OP_callassigned || bn->op == OP_call || bn->op == OP_virtualcallassigned ||
             bn->op == OP_superclasscallassigned || bn->op == OP_interfacecallassigned) {
    CallNode *callStmt = static_cast<CallNode *>(bn);
    MIRFunction *callee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callStmt->puIdx);
    auto it = toInline.find(callee);
    if (it != toInline.end()) {
      mod_.SetCurFunction(func);
      PerformInline(func, enclosingBlk, callStmt, callee);
      total_size += it->second;
    } else {
      if (callee->body && callee->GetAttr(FUNCATTR_synthetic) && (!callee->GetAttr(FUNCATTR_virtual))) {
        // handle synthetic functions
        int32 budget = max_budget;
        CGNode *node = cg_->GetCGNode(callee);
        if (node && NodeInlinable(node) && (SpendBudget(callee->body, budget, toInline, 3, callee) && budget >= 0)) {
          mod_.SetCurFunction(func);
          PerformInline(func, enclosingBlk, callStmt, callee);
        }
      } else {
        // check if it is a simple function
        CheckCalleeAndInline(func, enclosingBlk, callStmt, callee);
      }
    }
  } else if (bn->op == OP_doloop) {
    BlockNode *blk = static_cast<DoloopNode *>(bn)->doBody;
    InlineCallsBlock(func, enclosingBlk, blk, toInline);
  } else if (bn->op == OP_foreachelem) {
    BlockNode *blk = static_cast<ForeachelemNode *>(bn)->loopBody;
    InlineCallsBlock(func, enclosingBlk, blk, toInline);
  } else if (bn->op == OP_dowhile || bn->op == OP_while) {
    BlockNode *blk = static_cast<WhileStmtNode *>(bn)->body;
    InlineCallsBlock(func, enclosingBlk, blk, toInline);
  } else if (bn->op == OP_if) {
    BlockNode *blk = static_cast<IfStmtNode *>(bn)->thenPart;
    InlineCallsBlock(func, enclosingBlk, blk, toInline);
    blk = static_cast<IfStmtNode *>(bn)->elsePart;
    if (blk) {
      InlineCallsBlock(func, enclosingBlk, blk, toInline);
    }
  }
}

void MInline::ComputeTotalSize() {
  for (CGNode *caller : cg_->GetNodesMap()) {
    if (caller == nullptr) {
      continue;
    }
    total_size += caller->GetNodeCount();
  }
}

void MInline::CollectMustInlineFuncs(std::unordered_map<MIRFunction *, int32> &mustInline) {
  unordered_set<string> funcs = {
    // Add here the function names that must be inlined with --inline-lev=0
  };
  for (MapleVector<MIRFunction *>::iterator it = mod_.functionList.begin(); it != mod_.functionList.end(); it++) {
    MIRFunction *func = *it;
    auto itMust = funcs.find(func->GetName());
    if (itMust != funcs.end() && func->body) {
      mustInline.insert(pair<MIRFunction *, int32>(func, 0));
    }
    // analyze inline_func.list
    GStrIdx calleeStridx = GlobalTables::GetStrTable().GetStrIdxFromName(func->GetName());
    MapleMap<GStrIdx, MapleSet<GStrIdx> *>::iterator it2 = inline_prefer.find(calleeStridx);
    if (it2 != inline_prefer.end()) {
      CGNode *node = cg_->GetCGNode(func);
      CHECK_FATAL(node != nullptr, "node in null in MInline::CollectMustInlineFuncs");
      CanInline(node, mustInline);
    }
  }
}

void MInline::Inline() {
  if (MInline::level < 0) {
    return;
  } else if (MInline::level == 1) {
    max_budget = 5;
    growth_rate_threshold = 0.05;  // max 5% bigger
  } else {
    max_budget = 80;
    growth_rate_threshold = 0.10;  // max 10% bigger
  }

  // map<function to inline, cost of function's body>
  unordered_map<MIRFunction *, int32> toInline;
  if (LIST_ONLY) {
    ApplyInlineListInfo();
  }
  CollectMustInlineFuncs(toInline);

  const MapleVector<SCCNode *> &topvec = cg_->GetSCCTopVec();
  ComputeTotalSize();
  for (int32 i = (static_cast<int32>(topvec.size())) - 1; i >= 0; i--) {
    if (DEBUG_INLINE > 2) {
      topvec[i]->Dump();
    }
    for (auto const kIt : topvec[i]->GetCGNodes()) {
      CGNode *node = kIt;
      if (MInline::level > 0) {
        // if func has been analysed before, skip it.
        MIRFunction *func = node->GetMIRFunction();
        auto f = toInline.find(func);
        if (f == toInline.end()) {
          CanInline(node, toInline);
        }
      }
      // It increments total_size
      InlineCalls(node, toInline);
    }
  }
  return;
}

void MInline::CleanupInline() {
  const MapleVector<SCCNode *> &topvec = cg_->GetSCCTopVec();
  for (int32 i = (static_cast<int32>(topvec.size())) - 1; i >= 0; i--) {
    for (auto const kIt : topvec[i]->GetCGNodes()) {
      CGNode *node = kIt;
      MIRFunction *func = node->GetMIRFunction();
      if (func && func->GetAttr(FUNCATTR_optimized)) {
        // visit all the func which has been inlined, mark the static symbol, string symbol and function symbol as used.
        auto f = inline_times_map_.find(func);
        if (f != inline_times_map_.end() && inline_times_map_[func] > 0) {
          MarkUsedSymbols(func->body);
        }
        func->body = nullptr;
      }
    }
  }

  // after marking all the used symbols, set the other symbols as unused.
  for (uint32 i = 1; i < GlobalTables::GetGsymTable().GetSymbolTableSize(); i++) {
    MIRSymbol *symbol = GlobalTables::GetGsymTable().GetSymbolFromStIdx(i);
    if (symbol && symbol->istmpunused) {
      symbol->storageClass = kScUnused;
      if (DEBUG_INLINE) {
        LogInfo::MapleLogger() << "[INLINE_UNUSED_SYMBOL] " << symbol->GetName() << endl;
      }
    }
  }

  if (DEBUG_INLINE) {
    LogInfo::MapleLogger() << "[INLINE_SUMMARY] " << mod_.GetFileName() << endl;
    for (auto it = inline_times_map_.begin(); it != inline_times_map_.end(); ++it) {
      LogInfo::MapleLogger() << "[INLINE] " << it->first->GetName() << " => " << it->second << endl;
    }
    LogInfo::MapleLogger() << "[INLINE_SUMMARY] " << mod_.GetFileName() << endl;
  }
  return;
}

void MInline::MarkUsedSymbols(BaseNode *bn) {
  if (bn == nullptr) {
    return;
  }

  Opcode op = bn->op;
  switch (op) {
    case OP_block: {
      BlockNode *blk = static_cast<BlockNode *>(bn);
      for (StmtNode *stmt = blk->GetFirst(); stmt != nullptr; stmt = static_cast<StmtNode *>(stmt)->GetNext()) {
        MarkUsedSymbols(stmt);
      }
      break;
    }
    case OP_dassign: {
      DassignNode *dassignNode = static_cast<DassignNode *>(bn);
      StIdx symbolIdx = dassignNode->stIdx;
      MIRSymbol *symbol = GlobalTables::GetGsymTable().GetSymbolFromStIdx(symbolIdx.Idx());
      symbol->istmpunused = 0;
      string syName = symbol->GetName();
      // when _PTR_C_STR_XXXX is used, mark _C_STR_XXXX as used too.
      if (StartWith(syName, string(NameMangler::kPtrPrefixStr))) {
        MIRSymbol *anotherSymbol = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(syName.substr(strlen(NameMangler::kPtrPrefixStr))));
        anotherSymbol->istmpunused = 0;
      }
      break;
    }
    case OP_addrof:
    case OP_dread: {
      AddrofNode *dreadnode = static_cast<AddrofNode *>(bn);
      StIdx symbolIdx = dreadnode->stIdx;
      MIRSymbol *symbol = GlobalTables::GetGsymTable().GetSymbolFromStIdx(symbolIdx.Idx());
      symbol->istmpunused = 0;
      string syName = symbol->GetName();
      // when _PTR_C_STR_XXXX is used, mark _C_STR_XXXX as used too.
      if (StartWith(syName, NameMangler::kPtrPrefixStr)) {
        MIRSymbol *anotherSymbol = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(syName.substr(strlen(NameMangler::kPtrPrefixStr))));
        anotherSymbol->istmpunused = 0;
      }
      break;
    }
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned: {
      CallNode *callStmt = static_cast<CallNode *>(bn);
      MIRFunction *callee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(callStmt->puIdx);
      MIRSymbol *symbol = callee->GetFuncSymbol();
      symbol->istmpunused = 0;
      break;
    }
    default: {
      break;
    }
  }

  for (int32 i = 0; i < bn->NumOpnds(); i++) {
    MarkUsedSymbols(bn->Opnd(i));
  }
}

/* Unified interface to run inline module phase. */
AnalysisResult *DoInline::Run(MIRModule *module, ModuleResultMgr *m) {
  MemPool *mp = mempoolctrler.NewMemPool("inline mempool");
  KlassHierarchy *klasshierarchy = static_cast<KlassHierarchy *>(m->GetAnalysisResult(MoPhase_CHA, module));
  CHECK_FATAL(klasshierarchy != nullptr, "Expecting a valid KlassHierarchy, found nullptr");
  CallGraph *cg = static_cast<CallGraph *>(m->GetAnalysisResult(MoPhase_CALLGRAPH_ANALYSIS, module));
  CHECK_FATAL(cg != nullptr, "Expecting a valid CallGraph, found nullptr");
  MInline minline(*module, mp, klasshierarchy, cg);
  minline.Inline();
  minline.CleanupInline();
  m->InvalidAnalysisResult(MoPhase_CALLGRAPH_ANALYSIS, module);
  mempoolctrler.DeleteMemPool(mp);
  return nullptr;
}

}  // namespace maple
