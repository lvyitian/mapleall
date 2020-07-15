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

#include <fstream>
#include <iostream>
#include "clone.h"
#include "me_const_prop.h"
#include "constant_fold.h"
#include "mir_nodes.h"
#include "me_function.h"
#include "ssa_mir_nodes.h"
#include "mir_builder.h"

namespace maple {

static bool IsConstant(BaseNode *stmt, MapleMap<VersionSt *, BaseNode *> constantMp) {
  MapleMap<VersionSt *, BaseNode *>::iterator it;
  Opcode op = stmt->op;
  if (op == OP_dassign) {
    DassignNode *node = static_cast<DassignNode *>(stmt);
    for (int32 i = 0; i < stmt->NumOpnds(); i++) {
      if (node->Opnd(i)->op == OP_intrinsicop) {
        IntrinsicopNode *childNode = static_cast<IntrinsicopNode *>(node->Opnd(i));
        if (childNode->intrinsic == maple::INTRN_JAVA_MERGE) {
          node->SetOpnd(childNode->Opnd(0), i);
        }
      }
      if (node->Opnd(i)->op == OP_dread) {
        AddrofSSANode *addofnode = static_cast<AddrofSSANode *>(node->Opnd(i));
        VersionSt *vsym = addofnode->ssaVar;
        it = constantMp.find(vsym);
        if (it != constantMp.end()) {
          node->SetOpnd(it->second, i);
        }
      }
      if (!IsConstant(node->Opnd(i), constantMp)) {
        return false;
      }
    }
    return true;
  } else if (op == OP_cvt) {
    TypeCvtNode *node = static_cast<TypeCvtNode *>(stmt);
    for (int32 i = 0; i < stmt->NumOpnds(); i++) {
      if (node->Opnd(i)->op == OP_dread) {
        AddrofSSANode *addofnode = static_cast<AddrofSSANode *>(node->Opnd(i));
        VersionSt *vsym = addofnode->ssaVar;
        it = constantMp.find(vsym);
        if (it != constantMp.end()) {
          node->SetOpnd(it->second, i);
        }
      }
      if (!IsConstant(node->Opnd(i), constantMp)) {
        return false;
      }
    }
    return true;
  } else if (stmt->IsCondBr()) {
    CondGotoNode *node = static_cast<CondGotoNode *>(stmt);
    for (int32 i = 0; i < stmt->NumOpnds(); i++) {
      if (node->Opnd(i)->op == OP_dread) {
        AddrofSSANode *addofnode = static_cast<AddrofSSANode *>(node->Opnd(i));
        VersionSt *vsym = addofnode->ssaVar;
        it = constantMp.find(vsym);
        if (it != constantMp.end()) {
          node->SetOpnd(it->second, i);
        }
      }
      if (!IsConstant(node->Opnd(i), constantMp)) {
        return false;
      }
    }
    return true;
  } else if (op == OP_ne || op == OP_eq || op == OP_ne || op == OP_ge || op == OP_gt || op == OP_le || op == OP_lt ||
             op == OP_cmp || op == OP_cmpl || op == OP_cmpg) {
    CompareNode *node = static_cast<CompareNode *>(stmt);
    for (int32 i = 0; i < stmt->NumOpnds(); i++) {
      if (node->Opnd(i)->op == OP_dread) {
        AddrofSSANode *addofnode = static_cast<AddrofSSANode *>(node->Opnd(i));
        VersionSt *vsym = addofnode->ssaVar;
        it = constantMp.find(vsym);
        if (it != constantMp.end()) {
          node->SetOpnd(it->second, i);
        }
      }
      if (!IsConstant(node->Opnd(i), constantMp)) {
        return false;
      }
    }
    return true;
  } else if (op == OP_call || op == OP_callassigned || op == OP_virtualcall || op == OP_virtualcallassigned ||
             op == OP_virtualicall || op == OP_virtualicallassigned || op == OP_superclasscall ||
             op == OP_superclasscallassigned || op == OP_interfacecall || op == OP_interfacecallassigned ||
             op == OP_interfaceicall || op == OP_interfaceicallassigned || op == OP_customcall ||
             op == OP_customcallassigned || op == OP_polymorphiccall || op == OP_polymorphiccallassigned) {
    CallNode *node = static_cast<CallNode *>(stmt);
    for (int32 i = 0; i < stmt->NumOpnds(); i++) {
      if (node->Opnd(i)->op == OP_dread) {
        AddrofSSANode *addofnode = static_cast<AddrofSSANode *>(node->Opnd(i));
        VersionSt *vsym = addofnode->ssaVar;
        it = constantMp.find(vsym);
        if (it != constantMp.end()) {
          node->SetOpnd(it->second, i);
        }
      }
    }
    return false;
  } else if (op == OP_dread) {
    AddrofSSANode *dread = static_cast<AddrofSSANode *>(stmt);
    it = constantMp.find(dread->ssaVar);
    return it != constantMp.end();
  } else if (op == OP_constval) {
    return true;
  } else {
    return false;
  }
}

void MeConstProp::IntraConstProp() {
  MapleMap<VersionSt *, BaseNode *> constantMp(std::less<VersionSt *>(), constprop_alloc.Adapter());
  maple::ConstantFold cf(&func->mirModule);
  for (uint32 i = 0; i < func->bbVec.size(); i++) {
    BB *bb = func->bbVec[i];
    if (bb == nullptr) {
      continue;
    }
    for (auto stmt : bb->stmtNodeList) {
      if (IsConstant(stmt, constantMp)) {
        if (stmt->op == OP_dassign) {
          DassignNode *dass = static_cast<DassignNode *>(stmt);
          MayDefPartWithVersionSt *thessapart =
            static_cast<MayDefPartWithVersionSt *>(func->meSSATab->stmtsSSAPart.SsapartOf(stmt));

          constantMp[thessapart->ssaVar] = dass->Opnd(0);
        }
      }
    }
  }
}

// Clone a function
MIRFunction *CloneFunction(MIRFunction *oldfunc, std::string newfuncname) {
  MIRType *returnType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(oldfunc->GetReturnTyIdx());
  MapleAllocator cgalloc(oldfunc->codeMemPool);
  ArgVector argument(cgalloc.Adapter());
  for (uint32 i = 0; i < oldfunc->formalDefVec.size(); i++) {
    argument.push_back(
      ArgPair(oldfunc->formalDefVec[i].formalSym->GetName(), GlobalTables::GetTypeTable().GetTypeFromTyIdx(oldfunc->formalDefVec[i].formalTyIdx)));
  }
  maple::MIRBuilder mirBuilder(oldfunc->module);
  MIRFunction *newfunc = mirBuilder.CreateFunction(newfuncname, returnType, argument);
  CHECK_FATAL(newfunc != nullptr, "create dummymain function failed");

  newfunc->SetBaseClassFuncNames(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(newfuncname));

  newfunc->flag = oldfunc->flag;
  newfunc->classTyIdx = oldfunc->classTyIdx;
  MIRInfoVector &fninfo = oldfunc->info;
  MapleVector<bool> &fninfoIsstring = oldfunc->infoIsString;
  uint32 size = fninfo.size();
  for (uint32 i = 0; i < size; i++) {
    newfunc->info.push_back(std::pair<GStrIdx, uint32>(fninfo[i].first, fninfo[i].second));
    newfunc->infoIsString.push_back(fninfoIsstring[i]);
  }

  mirBuilder.mirModule->AddFunction(newfunc);
  mirBuilder.SetCurrentFunction(newfunc);
  newfunc->body = oldfunc->body->CloneTree(oldfunc->module);
  Clone::CloneSymbols(newfunc, oldfunc);
  Clone::CloneLabels(newfunc, oldfunc);

  // Insert function name for debugging purpose
  std::string commFuncName("CLONED FUNC ");
  commFuncName.append(oldfunc->GetName());
  auto tmp = newfunc->body;
  CHECK_FATAL(tmp != nullptr, "null ptr check");
  newfunc->body->InsertBefore(tmp->GetFirst(), mirBuilder.CreateStmtComment(commFuncName));
  return newfunc;
}

std::vector<ClonedFunction> clonedFunctions;
void MeConstProp::InterConstProp() {
  PUIdx puIdx = -1;
  MIRFunction *curFunction = func->mirFunc;
  BlockNode *body = curFunction->body;
  std::vector<MIRFunction *> newcalleev;
  maple::MIRBuilder mirBuilder(&func->mirModule);
  if (body) {
    for (StmtNode *stmt = body->GetFirst(); stmt != nullptr; stmt = static_cast<StmtNode *>(stmt)->GetNext()) {
      Opcode op = stmt->op;
      if (op == OP_call || op == OP_callassigned) {
        CallNode *callnode = static_cast<CallNode *>(stmt);
        puIdx = callnode->puIdx;
        // Check if one actual parameter is constant, clone a new callee function
        MIRFunction *oldcallee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
        if (oldcallee->GetName().find("_MAPLECONSTANTPROP_") != std::string::npos) {
          continue;
        }
        uint32 i = 0;
        uint32 argsize = static_cast<uint32>(callnode->NumOpnds()) < oldcallee->formalDefVec.size() ?
                         static_cast<uint32>(callnode->NumOpnds()) : oldcallee->formalDefVec.size();
        for (; i < argsize; i++) {
          if (callnode->Opnd(i)->op != OP_dread) {
            break;
          }
        }

        // Only deal with user-defined non-abstract function, don't deal with cloned function, only clone once.
        if (i < argsize && oldcallee->body != nullptr && oldcallee->body->GetFirst() != nullptr) {
          maple::ConstantFold cf(oldcallee->module);
          ActualArgVector actualarg;
          for (uint32 i = 0; i < argsize; i++) {
            if (callnode->Opnd(i)->op != OP_dread) {
              CHECK_FATAL(callnode->Opnd(i)->op == OP_cvt || callnode->Opnd(i)->op == OP_constval, "");
              if (callnode->Opnd(i)->op == OP_cvt) {
                TypeCvtNode *cvtnode = static_cast<TypeCvtNode *>(callnode->Opnd(i));
                BaseNode *tmp = cf.Fold(cvtnode);
                CHECK_FATAL(tmp, "null ptr check ");
                MIRConst *constnode = static_cast<ConstvalNode *>(tmp)->constVal;
                MIRType *type = constnode->type;
                if (IsPrimitiveInteger(type->primType)) {
                  actualarg.push_back(ActualArgPair(i, static_cast<MIRIntConst *>(constnode)->value));
                }
              } else if (callnode->Opnd(i)->op == OP_constval) {
                MIRConst *constnode = static_cast<ConstvalNode *>(callnode->Opnd(i))->constVal;
                MIRType *type = constnode->type;
                if (IsPrimitiveInteger(type->primType)) {
                  actualarg.push_back(ActualArgPair(i, static_cast<MIRIntConst *>(constnode)->value));
                }
              }
            }
          }

          uint32 i = 0;
          for (; i < clonedFunctions.size(); i++) {
            if (!strcmp(oldcallee->GetName().c_str(), clonedFunctions[i].oldfuncname.c_str())) {
              if (actualarg.size() == clonedFunctions[i].actualarg.size()) {
                uint32 j = 0;
                for (; j < actualarg.size(); j++) {
                  if (actualarg[j].first != clonedFunctions[i].actualarg[j].first ||
                      actualarg[j].second != clonedFunctions[i].actualarg[j].second) {
                    break;
                  }
                }
                if (j == actualarg.size()) {
                  callnode->puIdx = clonedFunctions[i].clonedidx;
                  break;
                }
              }
            }
          }
          if (i < clonedFunctions.size()) {
            continue;
          }
          // construct new function name
          string oldfuncname = oldcallee->GetName();
          std::size_t found = oldfuncname.find_last_of("|");
          if (found == std::string::npos) {
            FATAL(kLncFatal, "can not find \"|\" in oldfuncname");
          }
          std::string newfuncname("");
          newfuncname.append(oldfuncname.substr(0, found));
          newfuncname.append("_MAPLECONSTANTPROP_" + std::to_string(GlobalTables::GetFunctionTable().funcTable.size()) + "|");
          newfuncname.append(oldfuncname.substr(found + 1));
          MIRFunction *newcallee = CloneFunction(oldcallee, newfuncname);

          ClonedFunction *clonedFunction = new ClonedFunction();
          clonedFunction->oldfuncname = oldcallee->GetName();
          clonedFunction->clonedfuncname = newcallee->GetName();
          for (uint32 i = 0; i < actualarg.size(); i++) {
            clonedFunction->actualarg.push_back(actualarg[i]);
          }
          clonedFunction->clonedidx = newcallee->puIdx;
          clonedFunctions.push_back(*clonedFunction);
          delete clonedFunction;
          clonedFunction = nullptr;

          for (uint32 i = 0; i < argsize; i++) {
            if (callnode->Opnd(i)->op != OP_dread) {
              MIRSymbol *newFormal = newcallee->symTab->GetSymbolFromStIdx(newcallee->formalDefVec[i].formalSym->GetStIndex());
              DassignNode *dassignstmt = mirBuilder.CreateStmtDassign(newFormal, 0, callnode->Opnd(i));
              newcallee->body->InsertBefore(newcallee->body->GetFirst(), dassignstmt);
            }
          }
          // update class method
          if (oldcallee->classTyIdx != 0) {
            MIRType *classtype = GlobalTables::GetTypeTable().typeTable[oldcallee->classTyIdx.GetIdx()];
            MIRStructType *mirstructtype = static_cast<MIRStructType *>(classtype);
            uint32 i = 0;
            for (; i < mirstructtype->methods.size(); i++) {
              if (mirstructtype->methods[i].first == oldcallee->stIdx) {
                mirstructtype->methods.push_back(MethodPair(newcallee->stIdx, mirstructtype->methods[i].second));
                break;
              }
            }
            CHECK_FATAL(i < mirstructtype->methods.size(), "");
          }
          mirBuilder.SetCurrentFunction(curFunction);
          // Update call site function idx of the call stmt
          callnode->puIdx = newcallee->puIdx;
        }
      }
    }
  }
}

AnalysisResult *MeDoIntraConstProp::Run(MeFunction *func, MeFuncResultMgr *frm, ModuleResultMgr *mrm) {
  MemPool *constpropmp = mempoolctrler.NewMemPool(PhaseName().c_str());
  MeConstProp *meconstprop = constpropmp->New<MeConstProp>(func, constpropmp);
  meconstprop->IntraConstProp();
  /* this is a transform phase, delete mempool */
  mempoolctrler.DeleteMemPool(constpropmp);
  return nullptr;
}

AnalysisResult *MeDoInterConstProp::Run(MeFunction *func, MeFuncResultMgr *frm, ModuleResultMgr *mrm) {
  MemPool *constpropmp = mempoolctrler.NewMemPool(PhaseName().c_str());
  MeConstProp *meconstprop = constpropmp->New<MeConstProp>(func, constpropmp);
  meconstprop->InterConstProp();
  /* this is a transform phase, delete mempool */
  mempoolctrler.DeleteMemPool(constpropmp);
  return nullptr;
}

}  // namespace maple
