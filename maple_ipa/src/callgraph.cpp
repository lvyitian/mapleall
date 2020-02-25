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

#include <iostream>
#include <fstream>
#include <queue>
#include <unordered_set>
#include <algorithm>
#include "callgraph.h"
#include "option.h"
#include "retype.h"

////////////////////////////////////////////////////////////////////////
//                   Call Graph Anlysis                               //
////////////////////////////////////////////////////////////////////////
// This phase is a foundation phase of compilation. This phase build
// the call graph not only for this module also for the modules it
// depends on when this phase is running for IPA.
// The main procedure shows as following.
// A. Devirtual virtual call of private final static and none-static
//    variable. This step aims to reduce the callee set for each call
//    which can benefit IPA analysis.
// B. Build Call Graph.
//    i)  For IPA, it rebuild all the call graph of the modules this
//        module depends on. All necessary infomation is stored in mplt.
//    ii) Anlysis each function in this module. For each call statement
//        create a CGNode, and collect potential callee functions to
//        generate Call Graph.
// C. Find All Root Node for the Call Graph.
// D. Construct SCC based on Tarjan Algorithm
// E. Set compilation order as the bottom-up order of callgraph. So callee
//    is always compiled before caller. This benifits thoses optimizations
//     need interprocedure information like escape analysis.
////////////////////////////////////////////////////////////////////////

using namespace std;
namespace maple {
const char *CallInfo::GetCallTypeName() const {
  switch (ctype) {
    case kCallTypeCall:
      return "c";
    case kCallTypeVirtualCall:
      return "v";
    case kCallTypeSuperCall:
      return "s";
    case kCallTypeInterfaceCall:
      return "i";
    case kCallTypeIcall:
      return "icall";
    case kCallTypeIntrinsicCall:
      return "intrinsiccall";
    case kCallTypeXinitrinsicCall:
      return "xintrinsiccall";
    case kCallTypeIntrinsicCallWithType:
      return "intrinsiccallwithtype";
    case kCallTypeFakeThreadStartRun:
      return "fakecallstartrun";
    case kCallTypeCustomCall:
      return "customcall";
    case kCallTypePolymorphicCall:
      return "polymorphiccall";
    default:
      CHECK_FATAL(false, "unsupport CALL type");
      return nullptr;
  }
}

void CallInfo::Dump() const {
  LogInfo::MapleLogger() << "\tCallSite < " << GetCallTypeName() << " , " << line << " >\n";
}

const char *CallInfo::GetCalleeName() const {
  if ((ctype >= kCallTypeCall) && (ctype <= kCallTypeInterfaceCall)) {
    MIRFunction *mirf = mirFunc;
    return mirf->GetName().c_str();
  } else if (ctype == kCallTypeIcall) {
    return "IcallUnknown";
  } else if ((ctype >= kCallTypeIntrinsicCall) && (ctype <= kCallTypeIntrinsicCallWithType)) {
    return "IntrinsicCall";
  } else if (ctype == kCallTypeCustomCall) {
    return "CustomCall";
  } else if (ctype == kCallTypePolymorphicCall) {
    return "PolymorphicCall";
  }
  CHECK_FATAL(false, "should not be here");
  return nullptr;
}

void CGNode::DumpDetail() const {
  LogInfo::MapleLogger() << "---CGNode  @" << this << ": " << mirFunc->GetName() << "\t";
  if (HasOneCandidate() != nullptr) {
    LogInfo::MapleLogger() << "@One Candidate\n";
  } else {
    LogInfo::MapleLogger() << endl;
  }

  if (HasSetVCallCandidates()) {
    for (uint32 i = 0; i < vcallCands.size(); i++) {
      LogInfo::MapleLogger() << "   virtual call candidates: " << vcallCands[i]->GetName() << "\n";
    }
  }

  for (auto const &it : callees) {
    CallInfo *ci = it.first;
    CGNode *node = it.second;
    MIRFunction *mf = node->GetMIRFunction();
    if (mf) {
      LogInfo::MapleLogger() << "\tcallee in module : " << mf->GetName() << "  ";
      ;
    } else {
      LogInfo::MapleLogger() << "\tcallee external: " << ci->GetCalleeName();
    }
  }

  // dump caller
  for (auto const &callernode : callerSet) {
    CHECK_FATAL(callernode && callernode->mirFunc, "");
    LogInfo::MapleLogger() << "\tcaller : " << callernode->mirFunc->GetName() << endl;
  }
}

void CGNode::Dump(ofstream &fout) const {
  /* if dumpall == 1, dump whole call graph
   * else dump callgraph with function defined in same module */
  CHECK_FATAL(mirFunc != nullptr, "");
  if (callees.size() == 0) {
    fout << "\"" << mirFunc->GetName() << "\";\n";
    return;
  }
  for (auto const &it : callees) {
    CallInfo *ci = it.first;
    CGNode *node = it.second;
    if (!node) {
      continue;
    }
    MIRFunction *func = node->GetMIRFunction();
    fout << "\"" << mirFunc->GetName() << "\" -> ";
    if (func) {
      if (node->GetSCCNode() != nullptr && node->GetSCCNode()->GetCGNodes().size() > 1) {
        fout << "\"" << func->GetName() << "\"[label=" << node->GetSCCNode()->id << " color=red];\n";
      } else {
        fout << "\"" << func->GetName() << "\"[label=" << 0 << " color=blue];\n";
      }
    } else {
      /* unknown / external function with empty function body */
      fout << "\"" << ci->GetCalleeName() << "\"[label=" << ci->GetCallTypeName() << " color=blue];\n";
    }
  }
}

void CGNode::AddCallsite(CallInfo *ci, CGNode *node) {
  callees.push_back(std::make_pair(ci, node));
  if (node) {
    node->AddNumRefs();
  }
}

bool CGNode::HasCalleeFunc(const char *calleename) {
  bool result = false;
  std::queue<CGNode *> callees;
  std::unordered_set<CGNode *> visited;
  callees.push(this);
  while (!callees.empty()) {
    CGNode *curr = callees.front();
    callees.pop();
    visited.insert(curr);  // set visited
    if (curr->GetMIRFuncName().find(calleename) != std::string::npos) {
      return true;
    }
    for (auto cs : curr->callees) {
      if (visited.count(cs.second) > 0) {
        continue;
      }
      callees.push(cs.second);
    }
  }
  return result;
}

void CGNode::RemoveCallsite(const CallInfo *ci, CGNode *node) {
  for (auto it = CalleeBegin(); it != CalleeEnd(); it++) {
    if ((GetCallInfo(it)) == ci && (GetCalleeNode(it) == node)) {
      node->DecreaseNumRefs();
      if (callees.size()) {
        *it = callees.back();
        callees.pop_back();
      } else {
        FATAL(kLncFatal, "CALLEES IS EMPTY CAN NOT BE POP BACK AND ACCESS BACK");
      }
      return;
    }
  }
}

bool CGNode::IsCalleeOf(CGNode *func) {
  return callerSet.find(func) != callerSet.end();
}

void CallGraph::DelNode(CGNode *node) {
  if (!node->GetMIRFunction()) {
    return;
  }
  for (auto it = node->CalleeBegin(); it != node->CalleeEnd(); it++) {
    if (!((*it).second)) {
      continue;
    }
    (*it).second->DelCaller(node);
    if (!(*it).second->HasCaller()) {
      DelNode((*it).second);
    }
  }
  MIRFunction *func = node->GetMIRFunction();
  // Delete the method of class info
  if (func->classTyIdx != 0) {
    MIRType *classtype = GlobalTables::GetTypeTable().typeTable.at(func->classTyIdx.GetIdx());
    MIRStructType *mirstructtype = static_cast<MIRStructType *>(classtype);
    uint32 j = 0;
    for (; j < mirstructtype->methods.size(); j++) {
      if (mirstructtype->methods[j].first == func->stIdx) {
        mirstructtype->methods.erase(mirstructtype->methods.begin() + j);
        break;
      }
    }
  }
  for (uint32 i = 0; i < GlobalTables::GetFunctionTable().funcTable.size(); i++) {
    if (GlobalTables::GetFunctionTable().funcTable[i] == func) {
      uint32 j = 0;
      for (; j < mirModule->functionList.size(); j++) {
        if (mirModule->functionList[j] == GlobalTables::GetFunctionTable().funcTable[i]) {
          break;
        }
      }
      if (j < mirModule->functionList.size()) {
        mirModule->functionList.erase(mirModule->functionList.begin() + j);
      }
      GlobalTables::GetFunctionTable().funcTable[i] = nullptr;
      break;
    }
  }
  nodesMap.at(func->puIdx) = nullptr;
  // Update Klass info as it has been built
  if (klassh->GetKlassFromFunc(func)) {
    klassh->GetKlassFromFunc(func)->DelMethod(func);
  }
}

CallGraph::CallGraph(MIRModule *m, MemPool *mp, KlassHierarchy *kh, const char *fn)
  : AnalysisResult(mp),
    mirModule(m),
    cgalloc(mp),
    mirBuilder(cgalloc.GetMemPool()->New<MIRBuilder>(m)),
    entry_node(nullptr),
    rootNodes(cgalloc.Adapter()),
    fileName(fn),
    klassh(kh),
    nodesMap(GlobalTables::GetFunctionTable().funcTable.size(), nullptr, cgalloc.Adapter()),
    sccTopologicalVec(cgalloc.Adapter()),
    numOfNodes(0),
    numOfSccs(0) {
  CHECK_FATAL(fn != nullptr, "");
  callExternal = cgalloc.GetMemPool()->New<CGNode>(static_cast<MIRFunction *>(nullptr), &cgalloc, numOfNodes++);
  debug_flag = false;
  debug_scc = false;
}

CallType CallGraph::GetCallType(Opcode op) const {
  CallType t = kCallTypeInvalid;
  switch (op) {
    case OP_call:
    case OP_callassigned:
      t = kCallTypeCall;
      break;
    case OP_virtualcall:
    case OP_virtualcallassigned:
      t = kCallTypeVirtualCall;
      break;
    case OP_superclasscall:
    case OP_superclasscallassigned:
      t = kCallTypeSuperCall;
      break;
    case OP_interfacecall:
    case OP_interfacecallassigned:
      t = kCallTypeInterfaceCall;
      break;
    case OP_icall:
    case OP_icallassigned:
      t = kCallTypeIcall;
      break;
    case OP_intrinsiccall:
    case OP_intrinsiccallassigned:
      t = kCallTypeIntrinsicCall;
      break;
    case OP_xintrinsiccall:
    case OP_xintrinsiccallassigned:
      t = kCallTypeXinitrinsicCall;
      break;
    case OP_intrinsiccallwithtype:
    case OP_intrinsiccallwithtypeassigned:
      t = kCallTypeIntrinsicCallWithType;
      break;
    case OP_customcall:
    case OP_customcallassigned:
      t = kCallTypeCustomCall;
      break;
    case OP_polymorphiccall:
    case OP_polymorphiccallassigned:
      t = kCallTypePolymorphicCall;
      break;
    default:
      break;
  }
  return t;
}

CGNode *CallGraph::GetCGNode(MIRFunction *func) const {
  return nodesMap.at(func->puIdx);
}

CGNode *CallGraph::GetCGNode(PUIdx puIdx) const {
  return GetCGNode(GlobalTables::GetFunctionTable().GetFunctionFromPuidx((PUIdx)puIdx));
}

SCCNode *CallGraph::GetSCCNode(MIRFunction *func) const {
  CGNode *cgnode = GetCGNode(func);
  return cgnode ? cgnode->GetSCCNode() : nullptr;
}

bool CallGraph::IsRootNode(MIRFunction *func) const {
  if (GetCGNode(func)) {
    return (!GetCGNode(func)->HasCaller());
  } else {
    return false;
  }
}

CGNode *CallGraph::GetOrGenCGNode(PUIdx puIdx) {
  if (puIdx == 0) {
    return nullptr;
  }
  CGNode *node = GetCGNode(puIdx);
  if (node) {
    return node;
  } else {
    MIRFunction *mirFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
    node = cgalloc.GetMemPool()->New<CGNode>(mirFunc, &cgalloc, numOfNodes++);
    if (puIdx >= nodesMap.size()) {
      nodesMap.resize(puIdx+10, nullptr);
    }
    nodesMap[puIdx] = node;
    return node;
  }
}

void CallGraph::HandleBody(MIRFunction *func, BlockNode *body, CGNode *node, uint32 loopDepth) {
  char *line = nullptr;
  StmtNode *stmtNext = nullptr;
  for (StmtNode *stmt = body->GetFirst(); stmt != nullptr; stmt = stmtNext) {
    stmtNext = static_cast<StmtNode *>(stmt)->GetNext();
    Opcode op = stmt->op;
    if (op == OP_comment) {
      CommentNode *n = static_cast<CommentNode *>(stmt);
      line = n->comment.c_str();
      continue;
    } else if (op == OP_doloop) {
      DoloopNode *n = static_cast<DoloopNode *>(stmt);
      HandleBody(func, n->doBody, node, loopDepth + 1);
    } else if (op == OP_dowhile || op == OP_while) {
      WhileStmtNode *n = static_cast<WhileStmtNode *>(stmt);
      HandleBody(func, n->body, node, loopDepth + 1);
    } else if (op == OP_if) {
      IfStmtNode *n = static_cast<IfStmtNode *>(stmt);
      HandleBody(func, n->thenPart, node, loopDepth);
      if (n->elsePart) {
        HandleBody(func, n->elsePart, node, loopDepth);
      }
    } else {
      node->IncrStmtCount();
      CallType ct = GetCallType(op);
      if (ct == kCallTypeInvalid) {
        continue;
      } else if (ct == kCallTypeVirtualCall) {
        PUIdx calleePUIdx = (static_cast<CallNode *>(stmt))->puIdx;
        MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(calleePUIdx);
        CallInfo *callinfo = GenCallInfo(kCallTypeVirtualCall, line, calleefunc, stmt, loopDepth, stmt->stmtID);
        // Add a call node whether or not the calleefunc has its body
        CGNode *calleenode = GetOrGenCGNode(calleefunc->puIdx);
        ASSERT(calleenode != nullptr, "calleenode is null in CallGraph::HandleBody");
        calleenode->AddCaller(node);
        node->AddCallsite(callinfo, calleenode);
        Klass *klass = klassh->GetKlassFromFunc(calleefunc);
        if (!klass) {  // Array
          klass = klassh->GetKlassFromName(NameMangler::kJavaLangObjectStr);
        }
        // Traverse all subclasses
        std::vector<Klass *> klassVector;
        if (klass) {
          klassVector.push_back(klass);
        }
        for (unsigned int index = 0; index < klassVector.size(); index++) {
          Klass *currKlass = klassVector[index];
          for (MIRFunction *const &method : currKlass->GetMethods()) {
            if (calleefunc->GetBaseFuncNameWithTypeStridx() == method->GetBaseFuncNameWithTypeStridx()) {
              CGNode *calleenode = GetOrGenCGNode(method->puIdx);
              ASSERT(calleenode != nullptr, "calleenode is null in CallGraph::HandleBody");
              calleenode->AddCaller(node);
              node->AddCallsite(callinfo, calleenode);
              break;
            }
          }
          // add subclass of currKlass into vecotr
          for (Klass *subKlass : currKlass->GetSubKlasses()) {
            klassVector.push_back(subKlass);
          }
        }
        if (!klass) {
          continue;
        }
        // If klass.foo does not exist, search superclass and find the nearest one
        bool foundFunc = false;
        for (auto &it : klass->GetMethods()) {
          if (it == calleefunc) {
            foundFunc = true;
            break;
          }
        }
        // klass.foo does not exist
        if (!foundFunc) {
          Klass *superKlass = klass->GetSuperKlass();
          while (superKlass) {
            bool found = false;
            for (MIRFunction *const &method : superKlass->GetMethods()) {
              if (calleefunc->GetBaseFuncNameWithTypeStridx() == method->GetBaseFuncNameWithTypeStridx()) {
                CGNode *calleenode = GetOrGenCGNode(method->puIdx);
                ASSERT(calleenode != nullptr, "calleenode is null in CallGraph::HandleBody");
                calleenode->AddCaller(node);
                node->AddCallsite(callinfo, calleenode);
                found = true;
                break;
              }
            }
            if (found) {
              break;
            }
            superKlass = superKlass->GetSuperKlass();
          }
        }
      } else if (ct == kCallTypeInterfaceCall) {
        PUIdx calleePUIdx = (static_cast<CallNode *>(stmt))->puIdx;
        MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(calleePUIdx);
        CallInfo *callinfo = GenCallInfo(kCallTypeInterfaceCall, line, calleefunc, stmt, loopDepth, stmt->stmtID);
        // Add a call node whether or not the calleefunc has its body
        CGNode *calleenode = GetOrGenCGNode(calleefunc->puIdx);
        ASSERT(calleenode != nullptr, "calleenode is null in CallGraph::HandleBody");
        calleenode->AddCaller(node);
        node->AddCallsite(callinfo, calleenode);

        Klass *klass = klassh->GetKlassFromFunc(calleefunc);
        if (!klass) {
          klass = klassh->GetKlassFromName(NameMangler::kJavaLangObjectStr);
        }
        if (klass) {
          // Traverse all classes which implement the interface
          for (Klass *implKlass : klass->GetImplKlasses()) {
            for (MIRFunction *const &method : implKlass->GetMethods()) {
              if (calleefunc->GetBaseFuncNameWithTypeStridx() == method->GetBaseFuncNameWithTypeStridx()) {
                CGNode *calleenode = GetOrGenCGNode(method->puIdx);
                ASSERT(calleenode != nullptr, "calleenode is null in CallGraph::HandleBody");
                calleenode->AddCaller(node);
                node->AddCallsite(callinfo, calleenode);
                break;
              }
            }
          }
          // Traverse all subclasses
          std::vector<Klass *> klassVector;
          klassVector.push_back(klass);
          for (unsigned int index = 0; index < klassVector.size(); index++) {
            Klass *currKlass = klassVector[index];
            for (MIRFunction *const &method : currKlass->GetMethods()) {
              if (calleefunc->GetBaseFuncNameWithTypeStridx() == method->GetBaseFuncNameWithTypeStridx() &&
                  method->IsStatic()) {
                CGNode *calleenode = GetOrGenCGNode(method->puIdx);
                ASSERT(calleenode != nullptr, "calleenode is null in CallGraph::HandleBody");
                calleenode->AddCaller(node);
                node->AddCallsite(callinfo, calleenode);
                break;
              }
            }
            // add subclass of currKlass into vector
            for (Klass *subKlass : currKlass->GetSubKlasses()) {
              klassVector.push_back(subKlass);
            }
          }
          // If klass.foo does not exist, search superclass and find the nearest one
          bool foundFunc = false;
          for (auto &it : klass->GetMethods()) {
            if (it == calleefunc && it->IsStatic()) {
              foundFunc = true;
              break;
            }
          }
          // klass.foo does not exist
          if (!foundFunc) {
            klassVector.clear();
            for (Klass *superklass : klass->GetSuperKlasses()) {
              klassVector.push_back(superklass);
            }
            for (unsigned int index = 0; index < klassVector.size(); index++) {
              Klass *superklass = klassVector[index];
              bool found = false;
              for (MIRFunction *const &method : superklass->GetMethods()) {
                if (calleefunc->GetBaseFuncNameWithTypeStridx() == method->GetBaseFuncNameWithTypeStridx() &&
                    method->IsStatic()) {
                  CGNode *calleenode = GetOrGenCGNode(method->puIdx);
                  ASSERT(calleenode != nullptr, "calleenode is null in CallGraph::HandleBody");
                  calleenode->AddCaller(node);
                  node->AddCallsite(callinfo, calleenode);
                  found = true;
                  break;
                }
              }
              if (found) {
                break;
              }
              for (Klass *s : superklass->GetSuperKlasses()) {
                klassVector.push_back(s);
              }
            }
          }
        }
      } else if (ct == kCallTypeCall || ct == kCallTypeSuperCall) {
        if (op == OP_superclasscallassigned) {
          PUIdx calleePUIdx = (static_cast<CallNode *>(stmt))->puIdx;
          MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(calleePUIdx);
          Klass *klass = klassh->GetKlassFromFunc(calleefunc);
          if (klass == nullptr) {  // Fix CI
            continue;
          }
          ASSERT(klass != nullptr, "Klass not found");

          MapleVector<MIRFunction *> *cands = klass->GetCandidates(calleefunc->GetBaseFuncNameWithTypeStridx());
          // continue to search its implinterfaces
          if (!cands) {
            for (Klass *implinterface : klass->GetImplInterfaces()) {
              cands = implinterface->GetCandidates(calleefunc->GetBaseFuncNameWithTypeStridx());
              if (cands && cands->size() != 0) {
                break;
              }
            }
          }
          if (!cands || cands->size() == 0) {
            continue;  // Fix CI
            CHECK_FATAL(false, "Method candidate not found");
          }
          MIRFunction *actualMirfunc = cands->at(0);
          CallInfo *callinfo = GenCallInfo(kCallTypeCall, line, actualMirfunc, stmt, loopDepth, stmt->stmtID);
          CGNode *calleenode = GetOrGenCGNode(actualMirfunc->puIdx);
          ASSERT(calleenode != nullptr, "calleenode is null in CallGraph::HandleBody");
          calleenode->AddCaller(node);
          node->AddCallsite(callinfo, calleenode);
        } else {
          PUIdx calleePUIdx = (static_cast<CallNode *>(stmt))->puIdx;
          MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(calleePUIdx);
          CallInfo *callinfo = GenCallInfo(kCallTypeCall, line, calleefunc, stmt, loopDepth, stmt->stmtID);
          CGNode *calleenode = GetOrGenCGNode(calleefunc->puIdx);
          ASSERT(calleenode != nullptr, "calleenode is null in CallGraph::HandleBody");
          calleenode->AddCaller(node);
          node->AddCallsite(callinfo, calleenode);
        }
      } else if (ct == kCallTypeIntrinsicCall || ct == kCallTypeXinitrinsicCall) {
      } else if (ct == kCallTypeIntrinsicCallWithType) {
        IntrinsiccallNode *callnode = static_cast<IntrinsiccallNode *>(stmt);
        if (callnode->intrinsic == INTRN_JAVA_CLINIT_CHECK) {
          std::string typeName =
            GlobalTables::GetStrTable().GetStringFromStrIdx(GlobalTables::GetTypeTable().GetTypeFromTyIdx(callnode->tyIdx)->nameStrIdx);
          typeName.append(NameMangler::kClinitSuffix);
          GStrIdx clinitFuncStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(typeName.c_str());
          if (clinitFuncStrIdx != 0) {
            MIRFunction *clinitFunc = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(clinitFuncStrIdx)->GetFunction();
            CallInfo *callinfo =
              GenCallInfo(kCallTypeIntrinsicCallWithType, line, clinitFunc, stmt, loopDepth, stmt->stmtID);
            CGNode *calleenode = GetOrGenCGNode(clinitFunc->puIdx);
            ASSERT(calleenode != nullptr, "calleenode is null in CallGraph::HandleBody");
            calleenode->AddCaller(node);
            node->AddCallsite(callinfo, calleenode);
          }
        }
      } else if (ct == kCallTypeCustomCall) {
      } else if (ct == kCallTypePolymorphicCall) {
      } else if (ct == kCallTypeIcall) {
      } else {
        CHECK_FATAL(false, "TODO::unsupport call type");
      }
    }
  }
}

void CallGraph::UpdateCallGraphNode(CGNode *node) {
  node->Reset();
  MIRFunction *func = node->GetMIRFunction();
  BlockNode *body = func->body;
  HandleBody(func, body, node, 0);
}

void CallGraph::RecomputeSCC() {
  sccTopologicalVec.clear();
  numOfSccs = 0;
  BuildSCC();
}

void CallGraph::AddCallGraphNode(MIRFunction *func) {
  CGNode *node = GetOrGenCGNode(func->puIdx);
  CHECK_FATAL(node != nullptr, "node is null in CallGraph::GenCallGraph");
  BlockNode *body = func->body;
  HandleBody(func, body, node, 0);

  /* set root if current function is static main */
  if (func->GetName() == mirModule->entryFuncName) {
    mirModule->SetEntryFunction(func);
    entry_node = node;
  }
}

static void ResetInferredType(std::vector<MIRSymbol *> &inferredSymbols) {
  for (unsigned int i = 0; i < inferredSymbols.size(); i++) {
    inferredSymbols[i]->inferredTyidx = TyIdx();
  }
  inferredSymbols.clear();
}

static void ResetInferredType(std::vector<MIRSymbol *> &inferredSymbols, MIRSymbol *s) {
  if (!s) {
    return;
  }
  if (s->inferredTyidx == kInitTyIdx || s->inferredTyidx == kNoneTyIdx) {
    return;
  }
  unsigned int i = 0;
  for (; i < inferredSymbols.size(); i++) {
    if (inferredSymbols[i] == s) {
      s->inferredTyidx = TyIdx();
      inferredSymbols.erase(inferredSymbols.begin() + i);
      break;
    }
  }
  // ASSERT(i<inferredSymbols.size(), "s must be in inferredSymbols");
}

static void SetInferredType(std::vector<MIRSymbol *> &inferredSymbols, MIRSymbol *s, TyIdx idx) {
  s->inferredTyidx = idx;
  unsigned int i = 0;
  for (; i < inferredSymbols.size(); i++) {
    if (inferredSymbols[i] == s) {
      break;
    }
  }
  if (i == inferredSymbols.size()) {
    inferredSymbols.push_back(s);
  }
}

void IPODevirtulize::SearchDefInClinit(const Klass *klass) {
  MIRClassType *classtype = static_cast<MIRClassType *>(klass->GetMIRStructType());
  std::vector<MIRSymbol *> staticFinalPrivateSymbols;
  for (uint32 i = 0; i < classtype->staticFields.size(); i++) {
    FieldAttrs attribute = classtype->staticFields[i].second.second;
    if (attribute.GetAttr(FLDATTR_final)) {
      staticFinalPrivateSymbols.push_back(GlobalTables::GetGsymTable().GetSymbolFromStrIdx(classtype->staticFields[i].first));
    }
  }

  std::string typeName = klass->GetKlassName();
  typeName.append(NameMangler::kClinitSuffix);
  GStrIdx clinitFuncStrIdx = GlobalTables::GetStrTable().GetStrIdxFromName(typeName.c_str());
  if (clinitFuncStrIdx == 0) {
    return;
  }
  MIRFunction *func = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(clinitFuncStrIdx)->GetFunction();
  if (!func->body) {
    return;
  }

  StmtNode *stmtNext = nullptr;
  std::vector<MIRSymbol *> gcmallocSymbols;
  for (StmtNode *stmt = func->body->GetFirst(); stmt != nullptr; stmt = stmtNext) {
    stmtNext = stmt->GetNext();
    Opcode op = stmt->op;
    switch (op) {
      case OP_comment:
        break;
      case OP_dassign: {
        DassignNode *dassignNode = static_cast<DassignNode *>(stmt);
        MIRSymbol *leftSymbol = func->GetLocalOrGlobalSymbol(dassignNode->stIdx);
        unsigned i = 0;
        for (; i < staticFinalPrivateSymbols.size(); i++) {
          if (staticFinalPrivateSymbols[i] == leftSymbol) {
            break;
          }
        }
        if (i < staticFinalPrivateSymbols.size()) {
          if (dassignNode->GetRhs()->op == OP_dread) {
            DreadNode *dreadNode = static_cast<DreadNode *>(dassignNode->GetRhs());
            MIRSymbol *rightSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
            if (rightSymbol->inferredTyidx != kInitTyIdx && rightSymbol->inferredTyidx != kNoneTyIdx &&
                (staticFinalPrivateSymbols[i]->inferredTyidx == kInitTyIdx ||
                 (staticFinalPrivateSymbols[i]->inferredTyidx == rightSymbol->inferredTyidx))) {
              staticFinalPrivateSymbols[i]->inferredTyidx = rightSymbol->inferredTyidx;
            } else {
              staticFinalPrivateSymbols[i]->inferredTyidx = kInitTyIdx;
              staticFinalPrivateSymbols.erase(staticFinalPrivateSymbols.begin() + i);
            }
          } else {
            staticFinalPrivateSymbols[i]->inferredTyidx = kInitTyIdx;
            staticFinalPrivateSymbols.erase(staticFinalPrivateSymbols.begin() + i);
          }
        } else if (dassignNode->GetRhs()->op == OP_gcmalloc) {
          GCMallocNode *gcmallocNode = static_cast<GCMallocNode *>(dassignNode->GetRhs());
          TyIdx inferredTypeIdx = gcmallocNode->tyIdx;
          SetInferredType(gcmallocSymbols, leftSymbol, inferredTypeIdx);
        } else {
          ResetInferredType(gcmallocSymbols, leftSymbol);
        }
        break;
      }
      case OP_call: {
        CallNode *cnode = static_cast<CallNode *>(stmt);
        MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(cnode->puIdx);
        if (calleefunc->GetName().find(NameMangler::kClinitSubStr, 0) != std::string::npos) {
          // ignore all side effect of initizlizor
          continue;
        }
        for (unsigned int i = 0; i < cnode->nOpnd.size(); i++) {
          base_node_t *node = cnode->nOpnd[i];
          if (node->op != OP_dread) {
            continue;
          }
          // ASSERT(node->op == OP_dread, "Must be dread");
          DreadNode *dreadNode = static_cast<DreadNode *>(node);
          MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
          ResetInferredType(gcmallocSymbols, tmpSymbol);
        }
        break;
      }
      case OP_callassigned: {
        CallNode *cnode = static_cast<CallNode *>(stmt);
        MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(cnode->puIdx);
        if (calleefunc->GetName().find(NameMangler::kClinitSubStr, 0) != std::string::npos) {
          // ignore all side effect of initizlizor
          continue;
        }
        for (unsigned int i = 0; i < cnode->returnValues.size(); i++) {
          StIdx stIdx = cnode->returnValues[i].first;
          MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(stIdx);
          ResetInferredType(gcmallocSymbols, tmpSymbol);
        }
        for (unsigned int i = 0; i < cnode->nOpnd.size(); i++) {
          base_node_t *node = cnode->nOpnd[i];
          if (node->op != OP_dread) {
            continue;
          }
          // ASSERT(node->op == OP_dread, "Must be dread");
          DreadNode *dreadNode = static_cast<DreadNode *>(node);
          MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
          ResetInferredType(gcmallocSymbols, tmpSymbol);
        }
        break;
      }
      case OP_intrinsiccallwithtype: {
        IntrinsiccallNode *callnode = static_cast<IntrinsiccallNode *>(stmt);
        if (callnode->intrinsic != INTRN_JAVA_CLINIT_CHECK) {
          ResetInferredType(gcmallocSymbols);
        }
        break;
      }
      default:
        ResetInferredType(gcmallocSymbols);
        break;
    }
  }
}

void IPODevirtulize::SearchDefInMemberMethods(Klass *klass) {
  SearchDefInClinit(klass);

  MIRClassType *classtype = static_cast<MIRClassType *>(klass->GetMIRStructType());
  std::vector<FieldID> finalPrivateFieldID;
  for (uint32 i = 0; i < classtype->fields.size(); i++) {
    FieldAttrs attribute = classtype->fields[i].second.second;
    if (attribute.GetAttr(FLDATTR_final)) {
      // Conflict with simplify
      if (!strcmp(GlobalTables::GetStrTable().GetStringFromStrIdx(classtype->fields[i].first).c_str(), "mActivities")) {
        continue;
      }
      FieldID id = mirBuilder->GetStructFieldIdFromFieldNameParentFirst(
        classtype, GlobalTables::GetStrTable().GetStringFromStrIdx(classtype->fields[i].first).c_str());
      finalPrivateFieldID.push_back(id);
    }
  }

  std::vector<MIRFunction *> initMethods;
  std::string typeName = klass->GetKlassName();
  typeName.append(NameMangler::kCinitStr);
  for (MIRFunction *const &method : klass->GetMethods()) {
    if (!strncmp(method->GetName().c_str(), typeName.c_str(), typeName.length())) {
      initMethods.push_back(method);
    }
  }
  if (initMethods.size() == 0) {
    return;
  }
  ASSERT(initMethods.size() > 0, "Must have initializor");

  StmtNode *stmtNext = nullptr;
  for (unsigned int i = 0; i < initMethods.size(); i++) {
    MIRFunction *func = initMethods[i];
    if (!func->body) {
      continue;
    }
    std::vector<MIRSymbol *> gcmallocSymbols;
    for (StmtNode *stmt = func->body->GetFirst(); stmt != nullptr; stmt = stmtNext) {
      stmtNext = stmt->GetNext();
      Opcode op = stmt->op;
      switch (op) {
        case OP_comment:
          break;
        case OP_dassign: {
          DassignNode *dassignNode = static_cast<DassignNode *>(stmt);
          MIRSymbol *leftSymbol = func->GetLocalOrGlobalSymbol(dassignNode->stIdx);
          if (dassignNode->GetRhs()->op == OP_gcmalloc) {
            GCMallocNode *gcmallocNode = static_cast<GCMallocNode *>(dassignNode->GetRhs());
            SetInferredType(gcmallocSymbols, leftSymbol, gcmallocNode->tyIdx);
          } else if (dassignNode->GetRhs()->op == OP_retype) {
            RetypeNode *retystmt = static_cast<RetypeNode *>(dassignNode->GetRhs());
            base_node_t *fromnode = retystmt->Opnd(0);
            if (fromnode->op == OP_dread) {
              DreadNode *dreadNode = static_cast<DreadNode *>(fromnode);
              MIRSymbol *fromSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
              SetInferredType(gcmallocSymbols, leftSymbol, fromSymbol->inferredTyidx);
            } else {
              ResetInferredType(gcmallocSymbols, leftSymbol);
            }
          } else {
            ResetInferredType(gcmallocSymbols, leftSymbol);
          }
          break;
        }
        case OP_call: {
          CallNode *cnode = static_cast<CallNode *>(stmt);
          MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(cnode->puIdx);
          if (calleefunc->GetName().find(NameMangler::kClinitSubStr, 0) != std::string::npos) {
            // ignore all side effect of initizlizor
            continue;
          }
          for (unsigned int i = 0; i < cnode->nOpnd.size(); i++) {
            base_node_t *node = cnode->nOpnd[i];
            if (node->op != OP_dread) {
              continue;
            }
            DreadNode *dreadNode = static_cast<DreadNode *>(node);
            MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
            ResetInferredType(gcmallocSymbols, tmpSymbol);
          }
          break;
        }
        case OP_callassigned: {
          CallNode *cnode = static_cast<CallNode *>(stmt);
          MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(cnode->puIdx);
          if (calleefunc->GetName().find(NameMangler::kClinitSubStr, 0) != std::string::npos) {
            // ignore all side effect of initizlizor
            continue;
          }
          for (unsigned int i = 0; i < cnode->returnValues.size(); i++) {
            StIdx stIdx = cnode->returnValues[i].first;
            MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(stIdx);
            ResetInferredType(gcmallocSymbols, tmpSymbol);
          }
          for (unsigned int i = 0; i < cnode->nOpnd.size(); i++) {
            base_node_t *node = cnode->nOpnd[i];
            if (node->op != OP_dread) {
              continue;
            }
            // ASSERT(node->op == OP_dread, "Must be dread");
            DreadNode *dreadNode = static_cast<DreadNode *>(node);
            MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
            ResetInferredType(gcmallocSymbols, tmpSymbol);
          }
          break;
        }
        case OP_intrinsiccallwithtype: {
          IntrinsiccallNode *callnode = static_cast<IntrinsiccallNode *>(stmt);
          if (callnode->intrinsic != INTRN_JAVA_CLINIT_CHECK) {
            ResetInferredType(gcmallocSymbols);
          }
          break;
        }
        case OP_iassign: {
          IassignNode *iassignNode = static_cast<IassignNode *>(stmt);
          MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(iassignNode->tyIdx);
          ASSERT(type->typeKind == kTypePointer, "Must be pointer type");
          MIRPtrType *pointedType = static_cast<MIRPtrType *>(type);
          if (pointedType->pointedTyIdx == classtype->tyIdx) {
            // set field of current class
            FieldID fieldID = iassignNode->fieldID;
            unsigned i = 0;
            for (; i < finalPrivateFieldID.size(); i++) {
              if (finalPrivateFieldID[i] == fieldID) {
                break;
              }
            }
            if (i < finalPrivateFieldID.size()) {
              if (iassignNode->rhs->op == OP_dread) {
                DreadNode *dreadNode = static_cast<DreadNode *>(iassignNode->rhs);
                MIRSymbol *rightSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
                if (rightSymbol->inferredTyidx != kInitTyIdx && rightSymbol->inferredTyidx != kNoneTyIdx &&
                    (classtype->GetElemInferredTyidx(fieldID) == kInitTyIdx ||
                     (classtype->GetElemInferredTyidx(fieldID) == rightSymbol->inferredTyidx))) {
                  classtype->SetElemInferredTyidx(fieldID, rightSymbol->inferredTyidx);
                } else {
                  classtype->SetElemInferredTyidx(fieldID, kInitTyIdx);
                  finalPrivateFieldID.erase(finalPrivateFieldID.begin() + i);
                }
              } else {
                classtype->SetElemInferredTyidx(fieldID, kInitTyIdx);
                finalPrivateFieldID.erase(finalPrivateFieldID.begin() + i);
              }
            }
          }
          break;
        }
        default:
          ResetInferredType(gcmallocSymbols);
          break;
      }
    }
  }
}

void DoDevirtual(Klass *klass, KlassHierarchy *klassh) {
  MIRClassType *classtype = static_cast<MIRClassType *>(klass->GetMIRStructType());
  for (auto &func : klass->GetMethods()) {
    if (!func->body) {
      continue;
    }
    StmtNode *stmtNext = nullptr;
    std::vector<MIRSymbol *> inferredSymbols;
    for (StmtNode *stmt = func->body->GetFirst(); stmt != nullptr; stmt = stmtNext) {
      stmtNext = stmt->GetNext();
      Opcode op = stmt->op;
      switch (op) {
        case OP_comment:
        case OP_assertnonnull:
        case OP_brtrue:
        case OP_brfalse:
          break;
        case OP_dassign: {
          DassignNode *dassignNode = static_cast<DassignNode *>(stmt);
          MIRSymbol *leftSymbol = func->GetLocalOrGlobalSymbol(dassignNode->stIdx);
          if (dassignNode->GetRhs()->op == OP_dread) {
            DreadNode *dreadNode = static_cast<DreadNode *>(dassignNode->GetRhs());
            if (func->GetLocalOrGlobalSymbol(dreadNode->stIdx)->inferredTyidx != kInitTyIdx) {
              SetInferredType(inferredSymbols, leftSymbol,
                              func->GetLocalOrGlobalSymbol(dreadNode->stIdx)->inferredTyidx);
            }
          } else if (dassignNode->GetRhs()->op == OP_iread) {
            IreadNode *ireadNode = static_cast<IreadNode *>(dassignNode->GetRhs());
            MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ireadNode->tyIdx);
            ASSERT(type->typeKind == kTypePointer, "Must be pointer type");
            MIRPtrType *pointedType = static_cast<MIRPtrType *>(type);
            if (pointedType->pointedTyIdx == classtype->tyIdx) {
              FieldID fieldID = ireadNode->fieldID;
              FieldID tmpID = fieldID;
              TyIdx tmpTyIdx = classtype->GetElemInferredTyidx(tmpID);
              if (tmpTyIdx != kInitTyIdx && tmpTyIdx != kNoneTyIdx) {
                SetInferredType(inferredSymbols, leftSymbol, classtype->GetElemInferredTyidx(fieldID));
              }
            }
          } else {
            ResetInferredType(inferredSymbols, leftSymbol);
          }
          break;
        }
        case OP_interfacecall:
        case OP_interfacecallassigned:
        case OP_virtualcall:
        case OP_virtualcallassigned: {
          CallNode *calleeNode = static_cast<CallNode *>(stmt);
          MIRFunction *calleefunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(calleeNode->puIdx);
          if (calleeNode->nOpnd[0]->op == OP_dread) {
            DreadNode *dreadNode = static_cast<DreadNode *>(calleeNode->nOpnd[0]);
            MIRSymbol *rightSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
            if (rightSymbol->inferredTyidx != kInitTyIdx && rightSymbol->inferredTyidx != kNoneTyIdx) {
              // Devirtual
              Klass *currKlass = klassh->GetKlassFromTyidx(rightSymbol->inferredTyidx);
              if (op == OP_interfacecall || op == OP_interfacecallassigned || op == OP_virtualcall ||
                  op == OP_virtualcallassigned) {
                std::vector<Klass *> klassVector;
                klassVector.push_back(currKlass);
                bool hasDevirtualed = false;
                for (unsigned int index = 0; index < klassVector.size(); index++) {
                  Klass *tmpKlass = klassVector[index];
                  for (MIRFunction *const &method : tmpKlass->GetMethods()) {
                    if (calleefunc->GetBaseFuncNameWithTypeStridx() == method->GetBaseFuncNameWithTypeStridx()) {
                      calleeNode->puIdx = method->puIdx;
                      if (op == OP_virtualcall || op == OP_interfacecall) {
                        calleeNode->op = OP_call;
                      }
                      if (op == OP_virtualcallassigned || op == OP_interfacecallassigned) {
                        calleeNode->op = OP_callassigned;
                      }
                      hasDevirtualed = true;
                      if (false) {
                        LogInfo::MapleLogger() << "Devirtualize In function:" + func->GetName() << '\n';
                        LogInfo::MapleLogger() << calleeNode->op << '\n';
                        LogInfo::MapleLogger() << "    From:" << calleefunc->GetName() << '\n';
                        LogInfo::MapleLogger() << "    To  :" << GlobalTables::GetFunctionTable().GetFunctionFromPuidx(calleeNode->puIdx)->GetName() << '\n';
                      }
                      break;
                    }
                  }
                  if (hasDevirtualed) {
                    break;
                  }
                  // add subclass of currKlass into vecotr
                  for (Klass *superKlass : tmpKlass->GetSuperKlasses()) {
                    klassVector.push_back(superKlass);
                  }
                }
                if (hasDevirtualed) {
                  for (unsigned int i = 0; i < calleeNode->nOpnd.size(); i++) {
                    base_node_t *node = calleeNode->nOpnd[i];
                    if (node->op != OP_dread) {
                      continue;
                    }
                    DreadNode *dreadNode = static_cast<DreadNode *>(node);
                    MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
                    ResetInferredType(inferredSymbols, tmpSymbol);
                  }
                  if (op == OP_interfacecallassigned || op == OP_virtualcallassigned) {
                    CallNode *cnode = static_cast<CallNode *>(stmt);
                    for (unsigned int i = 0; i < cnode->returnValues.size(); i++) {
                      StIdx stIdx = cnode->returnValues[i].first;
                      MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(stIdx);
                      ResetInferredType(inferredSymbols, tmpSymbol);
                    }
                  }

                  break;
                }

                // Search default function in interfaces
                Klass *tmpInterface = nullptr;
                MIRFunction *tmpMethod = nullptr;
                for (Klass *iklass : currKlass->GetImplInterfaces()) {
                  for (MIRFunction *const &method : iklass->GetMethods()) {
                    if (calleefunc->GetBaseFuncNameWithTypeStridx() == method->GetBaseFuncNameWithTypeStridx() &&
                        !method->funcAttrs.GetAttr(FUNCATTR_abstract)) {
                      if (!tmpInterface || klassh->IsSuperKlassForInterface(tmpInterface, iklass)) {
                        tmpInterface = iklass;
                        tmpMethod = method;
                      }
                      break;
                    }
                  }
                }

                ASSERT(tmpMethod, "Must not be null");
                calleeNode->puIdx = tmpMethod->puIdx;
                if (op == OP_virtualcall || op == OP_interfacecall) {
                  calleeNode->op = OP_call;
                }
                if (op == OP_virtualcallassigned || op == OP_interfacecallassigned) {
                  calleeNode->op = OP_callassigned;
                }
                if (false) {
                  LogInfo::MapleLogger() << "Devirtualize In function:" + func->GetName() << '\n';
                  LogInfo::MapleLogger() << calleeNode->op << '\n';
                  LogInfo::MapleLogger() << "    From:" << calleefunc->GetName() << '\n';
                  LogInfo::MapleLogger() << "    To  :" << GlobalTables::GetFunctionTable().GetFunctionFromPuidx(calleeNode->puIdx)->GetName() << '\n';
                }
                for (unsigned int i = 0; i < calleeNode->nOpnd.size(); i++) {
                  base_node_t *node = calleeNode->nOpnd[i];
                  if (node->op != OP_dread) {
                    continue;
                  }
                  DreadNode *dreadNode = static_cast<DreadNode *>(node);
                  MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
                  ResetInferredType(inferredSymbols, tmpSymbol);
                }
                if (op == OP_interfacecallassigned || op == OP_virtualcallassigned) {
                  CallNode *cnode = static_cast<CallNode *>(stmt);
                  for (unsigned int i = 0; i < cnode->returnValues.size(); i++) {
                    StIdx stIdx = cnode->returnValues[i].first;
                    MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(stIdx);
                    ResetInferredType(inferredSymbols, tmpSymbol);
                  }
                }
                break;
              }
            }
          }
        }
        case OP_call: {
          CallNode *cnode = static_cast<CallNode *>(stmt);
          for (unsigned int i = 0; i < cnode->nOpnd.size(); i++) {
            base_node_t *node = cnode->nOpnd[i];
            if (node->op != OP_dread) {
              continue;
            }
            DreadNode *dreadNode = static_cast<DreadNode *>(node);
            MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
            ResetInferredType(inferredSymbols, tmpSymbol);
          }
          break;
        }
        case OP_callassigned: {
          CallNode *cnode = static_cast<CallNode *>(stmt);
          for (unsigned int i = 0; i < cnode->returnValues.size(); i++) {
            StIdx stIdx = cnode->returnValues[i].first;
            MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(stIdx);
            ResetInferredType(inferredSymbols, tmpSymbol);
          }
          for (unsigned int i = 0; i < cnode->nOpnd.size(); i++) {
            base_node_t *node = cnode->nOpnd[i];
            if (node->op != OP_dread) {
              continue;
            }
            DreadNode *dreadNode = static_cast<DreadNode *>(node);
            MIRSymbol *tmpSymbol = func->GetLocalOrGlobalSymbol(dreadNode->stIdx);
            ResetInferredType(inferredSymbols, tmpSymbol);
          }
          break;
        }
        default:
          ResetInferredType(inferredSymbols);
          break;
      }
    }
  }
}

void IPODevirtulize::DevirtualFinal() {
  // Search all klass in order to find final variables
  MapleMap<GStrIdx, Klass *>::const_iterator it = klassh->GetKlasses().begin();
  for (; it != klassh->GetKlasses().end(); ++it) {
    Klass *klass = it->second;
    if (klass->IsClass()) {
      MIRClassType *classtype = static_cast<MIRClassType *>(klass->GetMIRStructType());
      // Initialize inferred type of member fileds as kInitTyIdx
      for (unsigned int i = 0; i < classtype->fields.size(); i++) {  // Don't include parent's field
        classtype->SetElemInferredTyidx(i, kInitTyIdx);
      }

      SearchDefInMemberMethods(klass);

      for (unsigned int i = 0; i < classtype->fieldInferredTyidx.size(); i++) {
        if (classtype->GetElemInferredTyidx(i) != kInitTyIdx && classtype->GetElemInferredTyidx(i) != kNoneTyIdx) {
          if (debugFlag) {
            FieldID tmpID = i;
            FieldPair pair = classtype->TraverseToFieldRef(tmpID);
            LogInfo::MapleLogger() << "Inferred Final Private None-Static Variable:" + klass->GetKlassName() + ":" +
                      GlobalTables::GetStrTable().GetStringFromStrIdx(pair.first)
                 << '\n';
          }
        }
      }
      for (uint32 i = 0; i < classtype->staticFields.size(); i++) {
        FieldAttrs attribute = classtype->staticFields[i].second.second;
        if (!GlobalTables::GetGsymTable().GetSymbolFromStrIdx(classtype->staticFields[i].first)) {
          continue;
        }
        if (GlobalTables::GetGsymTable().GetSymbolFromStrIdx(classtype->staticFields[i].first)->inferredTyidx != kInitTyIdx &&
            GlobalTables::GetGsymTable().GetSymbolFromStrIdx(classtype->staticFields[i].first)->inferredTyidx != kNoneTyIdx) {
          ASSERT(attribute.GetAttr(FLDATTR_final), "Must be final private");
          if (debugFlag) {
            LogInfo::MapleLogger() << "Final Private Static Variable:" +
                      GlobalTables::GetStrTable().GetStringFromStrIdx(classtype->staticFields[i].first)
                 << '\n';
          }
        }
      }
      DoDevirtual(klass, GetKlassh());
    }
  }
}

typedef std::pair<CallInfo *, PUIdx> CallSite;
void CallGraph::GenCallGraph() {
  // Read existing call graph from mplt, std::map<PUIdx, std::vector<CallSite> >
  for (auto const &it : mirModule->method2TargetMap) {
    CGNode *node = GetOrGenCGNode(it.first);
    CHECK_FATAL(node != nullptr, "node is null in CallGraph::GenCallGraph");
    std::vector<CallSite> callees = it.second;
    for (std::vector<CallSite>::iterator itinner = callees.begin(); itinner != callees.end(); ++itinner) {
      CGNode *calleenode = GetOrGenCGNode((*itinner).second);
      CHECK_FATAL(calleenode != nullptr, "calleenode is null in CallGraph::GenCallGraph");
      calleenode->AddCaller(node);
      node->AddCallsite((*itinner).first, calleenode);
    }
  }

  // Deal with function override, function in current module override functions from mplt
  for (auto it = GlobalTables::GetFunctionTable().funcTable.begin(); it != GlobalTables::GetFunctionTable().funcTable.end(); it++) {
    MIRFunction *mirFunc = *it;
    if (!mirFunc || !mirFunc->body) {
      continue;
    }
    mirModule->SetCurFunction(static_cast<mir_func_t *>(mirFunc));

    CGNode *node = GetOrGenCGNode(mirFunc->puIdx);
    CHECK_FATAL(node != nullptr, "node is null in CallGraph::GenCallGraph");
    // Update existing call node imported from mplt
    Klass *klass = klassh->GetKlassFromFunc(mirFunc);
    if (klass) {
      // For Array, don't need to handle
      // Traverse all superclasses
      std::vector<Klass *> klassVector;
      for (Klass *const &superclass : klass->GetSuperKlasses()) {
        klassVector.push_back(superclass);
      }
      for (unsigned int index = 0; index < klassVector.size(); index++) {
        Klass *currKlass = klassVector[index];
        MIRFunction *func =
          mirBuilder->GetFunctionFromName((currKlass->GetKlassName() + NameMangler::kNameSplitterStr +
                                   GlobalTables::GetStrTable().GetStringFromStrIdx(mirFunc->GetBaseFuncNameWithTypeStridx())));
        if (func) {
          CGNode *calleenode = GetCGNode(func->puIdx);
          // Get caller
          if (calleenode) {
            for (auto itCaller = calleenode->CallerBegin(); itCaller != calleenode->CallerEnd(); itCaller++) {
              CGNode *callerNode = *itCaller;
              for (unsigned int i = 0; i < callerNode->GetCallee().size(); i++) {
                if (calleenode == callerNode->GetCallee()[i].second &&
                    (callerNode->GetCallee()[i].first->GetCallType() == kCallTypeVirtualCall ||
                     callerNode->GetCallee()[i].first->GetCallType() == kCallTypeInterfaceCall)) {
                  node->AddCaller(callerNode);
                  callerNode->AddCallsite(callerNode->GetCallee()[i].first, node);
                }
              }
            }
          }
        }
        // add superclass of currKlass into vecotr
        for (Klass *superclass : currKlass->GetSuperKlasses()) {
          klassVector.push_back(superclass);
        }
      }

      // Traverse all implemented interfaces
      for (Klass *implInterface : klass->GetImplInterfaces()) {
        MIRFunction *func =
          mirBuilder->GetFunctionFromName((implInterface->GetKlassName() + NameMangler::kNameSplitterStr +
                                   GlobalTables::GetStrTable().GetStringFromStrIdx(mirFunc->GetBaseFuncNameWithTypeStridx())));
        if (func) {
          CGNode *calleenode = GetCGNode(func->puIdx);
          // Get caller
          if (calleenode) {
            for (auto itCaller = calleenode->CallerBegin(); itCaller != calleenode->CallerEnd(); itCaller++) {
              CGNode *callerNode = *itCaller;
              for (unsigned int i = 0; i < callerNode->GetCallee().size(); i++) {
                if (calleenode == callerNode->GetCallee()[i].second &&
                    (callerNode->GetCallee()[i].first->GetCallType() == kCallTypeVirtualCall ||
                     callerNode->GetCallee()[i].first->GetCallType() == kCallTypeInterfaceCall)) {
                  node->AddCaller(callerNode);
                  callerNode->AddCallsite(callerNode->GetCallee()[i].first, node);
                }
              }
            }
          }
        }
      }
    }
  }

  for (auto it = GlobalTables::GetFunctionTable().funcTable.begin(); it != GlobalTables::GetFunctionTable().funcTable.end(); it++) {
    MIRFunction *mirFunc = *it;
    if (!mirFunc || !mirFunc->body) {
      continue;
    }
    mirModule->SetCurFunction(static_cast<mir_func_t *>(mirFunc));
    AddCallGraphNode(mirFunc);
    CGNode *callGraphNode = GetOrGenCGNode(mirFunc->puIdx);
    for (uint32 i = 0; i < callGraphNode->callees.size(); i++) {
      bool foundCall = false;
      bool foundICall = false;
      bool foundVCall = false;
      Callsite callSite = callGraphNode->callees[i];
      CallInfo *callInfo = callSite.first;
      CGNode *cgNode = callSite.second;
      if (callInfo->GetCallType() == kCallTypeCall) {
        foundCall = true;
      }
      if (callInfo->GetCallType() == kCallTypeInterfaceCall) {
        foundICall = true;
      }
      if (callInfo->GetCallType() == kCallTypeVirtualCall) {
        foundVCall = true;
      }
      for (uint32 j = i + 1; j < callGraphNode->callees.size(); j++) {
        Callsite callSiteInner = callGraphNode->callees[j];
        CallInfo *callInfoInner = callSiteInner.first;
        CGNode *cgNodeInner = callSiteInner.second;
        if (cgNodeInner == cgNode) {
          if (callInfoInner->GetCallType() == kCallTypeCall) {
            if (foundCall) {
              callGraphNode->callees.erase(callGraphNode->callees.begin() + j);
              j--;
            } else {
              foundCall = true;
            }
          }
          if (callInfoInner->GetCallType() == kCallTypeInterfaceCall) {
            if (foundICall) {
              callGraphNode->callees.erase(callGraphNode->callees.begin() + j);
              j--;
            } else {
              foundICall = true;
            }
          }
          if (callInfoInner->GetCallType() == kCallTypeVirtualCall) {
            if (foundVCall) {
              callGraphNode->callees.erase(callGraphNode->callees.begin() + j);
              j--;
            } else {
              foundVCall = true;
            }
          }
        }
      }
    }
  }

  // Add all root nodes
  FindRootNodes();
  BuildSCC();
}

void CallGraph::FindRootNodes() {
  CHECK_FATAL(rootNodes.empty(), "rootNodes has already been set");
  for (CGNode *node : nodesMap) {
    if (node == nullptr) {
      continue;
    }
    if (!node->HasCaller()) {
      rootNodes.push_back(node);
    }
  }
}

void CallGraph::Dump() const {
  for (CGNode *node : nodesMap) {
    if (node == nullptr) {
      continue;
    }
    node->DumpDetail();
  }
}

void CallGraph::DumpToFile(bool dumpall) {
  if (Options::noDot) {
    return;
  }
  ofstream cgfile;
  char *outName = nullptr;
  MapleString outfile(fileName, GetMempool());
  if (dumpall) {
    outName = (outfile.append("-callgraph.dot")).c_str();
  } else {
    outName = (outfile.append("-callgraphlight.dot")).c_str();
  }
  cgfile.open(outName, ios::trunc);
  cgfile << "digraph graphname {\n";
  for (CGNode *node : nodesMap) {
    if (node == nullptr) {
      continue;
    }
    // dump user defined function
    if (dumpall) {
      node->Dump(cgfile);
    } else {
      if ((node->GetMIRFunction() != nullptr) && (!node->GetMIRFunction()->IsEmpty())) {
        node->Dump(cgfile);
      }
    }
  }
  cgfile << "}\n";
  cgfile.close();
}

void CallGraph::BuildCallGraph() {
  GenCallGraph();

  // Dump callgraph to dot file
  if (debug_flag) {
    DumpToFile(true);
  }
  SetCompilationFunclist();
}

// Sort CGNode within an SCC. Try best to arrange callee appears before
// its (direct) caller, so that caller can benefit from summary info.
// If we have iterative inter-procedure analysis, then would not bother
// do this.
static bool CGNodeCompare(CGNode *left, CGNode *right) {
  // special case: left calls right and right calls left, then compare by id
  if (left->IsCalleeOf(right) && right->IsCalleeOf(left)) {
    return left->GetID() < right->GetID();
  }

  // left is right's direct callee, then make left appears first
  if (left->IsCalleeOf(right)) {
    return true;
  } else if (right->IsCalleeOf(left)) {
    return false;
  }

  return left->GetID() < right->GetID();
}

// Set compilation order as the bottom-up order of callgraph. So callee
// is always compiled before caller. This benifits thoses optimizations
// need interprocedure information like escape analysis.
void CallGraph::SetCompilationFunclist() {
  mirModule->compilationList.clear();
  mirModule->functionList.clear();
  const MapleVector<SCCNode *> &sccTopVec = GetSCCTopVec();
  for (int i = sccTopVec.size() - 1; i >= 0; i--) {
    SCCNode *sccNode = sccTopVec[i];
    std::sort(sccNode->cgNodes.begin(), sccNode->cgNodes.end(), CGNodeCompare);
    for (auto const kIt : sccNode->cgNodes) {
      CGNode *node = kIt;
      MIRFunction *func = node->GetMIRFunction();
      if ((func && func->body) || (func && !func->IsNative())) {
        mirModule->compilationList.push_back(func);
        mirModule->functionList.push_back(func);
      }
    }
  }
  CHECK_FATAL((mirModule->compilationList.size() == mirModule->functionList.size() ||
          mirModule->compilationList.size() == mirModule->functionList.size() - mirModule->optimizedFuncs.size()),
         "should be equal");
}

bool SCCNode::HasRecursion() const {
  CHECK_FATAL(cgNodes.size() != 0, "");

  if (cgNodes.size() > 1) {
    return true;
  }

  CGNode *node = cgNodes[0];

  for (auto itCallee = node->CalleeBegin(); itCallee != node->CalleeEnd(); itCallee++) {
    CGNode *calleeNode = node->GetCalleeNode(itCallee);
    if (!calleeNode) {
      continue;
    }
    if (node == calleeNode) {
      return true;
    }
  }
  return false;
}

void SCCNode::Dump() {
  printf("SCC %d contains\n", id);
  for (auto const kIt : cgNodes) {
    CGNode *node = kIt;
    if (node->GetMIRFunction()) {
      printf("  function(%d): %s\n", node->GetMIRFunction()->puIdxOrigin, node->GetMIRFunction()->GetName().c_str());
    } else {
      printf("  function: external\n");
    }
  }
}

void SCCNode::DumpCycle() {
  CGNode *currNode = cgNodes[0];
  std::vector<CGNode *> searched;
  searched.push_back(currNode);
  std::vector<CGNode *> invalidNodes;
  while (true) {
    bool findNewCallee = false;
    for (unsigned int i = 0; i < currNode->GetCallee().size(); i++) {
      CGNode *calleeNode = currNode->GetCallee()[i].second;
      if (calleeNode->GetSCCNode() == this) {
        unsigned int j = 0;
        for (; j < invalidNodes.size(); j++) {
          if (invalidNodes[j] == calleeNode) {
            break;
          }
        }
        // Find a invalid node
        if (j < invalidNodes.size()) {
          continue;
        }
        for (j = 0; j < searched.size(); j++) {
          if (searched[j] == calleeNode) {
            break;
          }
        }
        if (j == searched.size()) {
          currNode = calleeNode;
          searched.push_back(currNode);
          findNewCallee = true;
          break;
        }
      }
    }
    if (searched.size() == cgNodes.size()) {
      break;
    }
    if (!findNewCallee) {
      invalidNodes.push_back(searched[searched.size() - 1]);
      searched.pop_back();
      currNode = searched[searched.size() - 1];
    }
  }
  for (std::vector<CGNode *>::iterator it = searched.begin(); it != searched.end(); it++) {
    LogInfo::MapleLogger() << (*it)->GetMIRFunction()->GetName() << '\n';
  }
}

void SCCNode::Verify() {
  CHECK_FATAL(cgNodes.size() > 0, "");
  for (CGNode *const &node : cgNodes) {
    CHECK_FATAL(node->GetSCCNode() == this, "");
  }
}

void SCCNode::Setup() {
  for (CGNode *const &node : cgNodes) {
    for (auto itCallee = node->CalleeBegin(); itCallee != node->CalleeEnd(); itCallee++) {
      CGNode *calleeNode = node->GetCalleeNode(itCallee);
      if (!calleeNode) {
        continue;
      }
      if (calleeNode->GetSCCNode() == this) {
        continue;
      }
      calleeScc.insert(calleeNode->GetSCCNode());
    }

    for (auto itCaller = node->CallerBegin(); itCaller != node->CallerEnd(); itCaller++) {
      CGNode *callerNode = *itCaller;
      if (callerNode->GetSCCNode() == this) {
        continue;
      }
      callerScc.insert(callerNode->GetSCCNode());
    }
  }
}

void CallGraph::BuildSCCDFS(CGNode *caller, uint32 &visitIndex, std::vector<SCCNode *> &sccNodes,
                            std::vector<CGNode *> &cgNodes, std::vector<uint32> &visitedOrder,
                            std::vector<uint32> &lowestOrder, std::vector<bool> &inStack,
                            std::stack<uint32> &visitStack) {
  uint32 id = caller->GetID();
  cgNodes[id] = caller;
  visitedOrder[id] = visitIndex;
  lowestOrder[id] = visitIndex;
  visitIndex++;
  visitStack.push(id);
  inStack[id] = true;

  for (auto itCallee = caller->CalleeBegin(); itCallee != caller->CalleeEnd(); itCallee++) {
    CGNode *calleeNode = caller->GetCalleeNode(itCallee);
    if (!calleeNode) {
      continue;
    }
    uint32 calleeId = calleeNode->GetID();
    if (!visitedOrder[calleeId]) {
      // callee has not been processed yet
      BuildSCCDFS(calleeNode, visitIndex, sccNodes, cgNodes, visitedOrder, lowestOrder, inStack, visitStack);
      if (lowestOrder[calleeId] < lowestOrder[id]) {
        lowestOrder[id] = lowestOrder[calleeId];
      }
    } else if (inStack[calleeId] && visitedOrder[calleeId] < lowestOrder[id]) {
      // back edge
      lowestOrder[id] = visitedOrder[calleeId];
    }
  }

  if (visitedOrder.at(id) == lowestOrder.at(id)) {
    SCCNode *sccNode = cgalloc.GetMemPool()->New<SCCNode>(numOfSccs++, &cgalloc);
    uint32 stackTopId;
    do {
      stackTopId = visitStack.top();
      visitStack.pop();
      inStack[stackTopId] = false;
      CGNode *topNode = cgNodes[stackTopId];
      topNode->SetSCCNode(sccNode);
      sccNode->AddCGNode(topNode);
    } while (stackTopId != id);

    sccNodes.push_back(sccNode);
  }
}

void CallGraph::VerifySCC() {
  for (CGNode *node : nodesMap) {
    if (node == nullptr) {
      continue;
    }
    CHECK_FATAL(node->GetSCCNode() != nullptr, "");
  }
}

void CallGraph::BuildSCC() {
  // This is the mapping between cg_id to cg_node. We may consider putting this in the CallGraph if it will be used
  // frenqutenly in the future.
  std::vector<CGNode *> cgNodes(numOfNodes, nullptr);
  std::vector<uint32> visitedOrder(numOfNodes, 0);
  std::vector<uint32> lowestOrder(numOfNodes, 0);
  std::vector<bool> inStack(numOfNodes, false);
  std::vector<SCCNode *> sccNodes;

  uint32 visitIndex = 1;
  std::stack<uint32> visitStack;

  // Starting from roots is a good strategy for DSF
  for (CGNode *const &root : rootNodes) {
    if (debug_scc) {
      printf("  root CGNode for function(%d): %s is id %d\n", root->GetMIRFunction()->puIdxOrigin, root->GetMIRFunction()->GetName().c_str(), root->id);
    }
    BuildSCCDFS(root, visitIndex, sccNodes, cgNodes, visitedOrder, lowestOrder, inStack, visitStack);
  }
  // However, not all SCC can be reached from roots.
  // E.g. foo()->foo(), foo is not considered as a root.
  for (CGNode *node : nodesMap) {
    if (node == nullptr) {
      continue;
    }
    if (!node->GetSCCNode()) {
      BuildSCCDFS(node, visitIndex, sccNodes, cgNodes, visitedOrder, lowestOrder, inStack, visitStack);
    }
  }

  for (SCCNode *const &scc : sccNodes) {
    // LogInfo::MapleLogger() << "SCC:" << scc->cgNodes.size() << '\n';
    scc->Verify();
    scc->Setup();  // fix caller and callee info.
    if (debug_scc && scc->HasRecursion()) {
      scc->Dump();
    }
  }

  SCCTopologicalSort(sccNodes);
}

void CallGraph::SCCTopologicalSort(std::vector<SCCNode *> &sccNodes) {
  std::set<SCCNode *> inQueue;  // Local variable, no need to use MapleSet
  for (SCCNode *const &node : sccNodes) {
    if (!node->HasCaller()) {
      sccTopologicalVec.push_back(node);
      inQueue.insert(node);
    }
  }

  // Top-down iterates all nodes
  for (unsigned i = 0; i < sccTopologicalVec.size(); i++) {
    SCCNode *sccNode = sccTopologicalVec[i];
    for (SCCNode *callee : sccNode->calleeScc) {
      if (inQueue.find(callee) == inQueue.end()) {
        // callee has not been visited
        bool callerAllVisited = true;
        // Check whether all callers of the current callee have been visited
        for (SCCNode *caller : callee->callerScc) {
          if (inQueue.find(caller) == inQueue.end()) {
            callerAllVisited = false;
            break;
          }
        }
        if (callerAllVisited) {
          sccTopologicalVec.push_back(callee);
          inQueue.insert(callee);
        }
      }
    }
  }
}

void CGNode::AddCandsForCallNode(const KlassHierarchy *kh) {
  /* already set vcall candidates information */
  if (HasSetVCallCandidates()) {
    return;
  }
  CHECK_FATAL(mirFunc != nullptr, "");
  Klass *klass = kh->GetKlassFromFunc(mirFunc);
  if (klass) {
    MapleVector<MIRFunction *> *v = klass->GetCandidates(mirFunc->GetBaseFuncNameWithTypeStridx());
    if (v) {
      vcallCands = *v;  // Vector copy
    }
  }
}

MIRFunction *CGNode::HasOneCandidate() const {
  int count = 0;
  MIRFunction *cand = nullptr;
  if (!mirFunc->IsEmpty()) {
    count++;
    cand = mirFunc;
  }
  /* scan candidates */
  for (uint32 i = 0; i < vcallCands.size(); i++) {
    CHECK_FATAL(vcallCands[i] != nullptr, "");
    if (!vcallCands[i]->IsEmpty()) {
      count++;
      if (!cand) {
        cand = vcallCands[i];
      }
    }
  }
  return count == 1 ? cand : nullptr;
}

AnalysisResult *DoCallGraph::Run(MIRModule *module, ModuleResultMgr *m) {
  MemPool *mp = mempoolctrler.NewMemPool("callgraph mempool");

  KlassHierarchy *cha = static_cast<KlassHierarchy *>(m->GetAnalysisResult(MoPhase_CHA, module));
  CHECK_FATAL(cha != nullptr, "");
  CallGraph *cg = mp->New<CallGraph>(module, mp, cha, module->fileName.c_str());
  cg->debug_flag = TRACE_PHASE;

  cg->BuildCallGraph();
  m->AddResult(GetPhaseId(), module, cg);

  // do retype
  MemPool *localMp = mempoolctrler.NewMemPool(PhaseName().c_str());
  maple::MIRBuilder mirBuilder(module);
  KlassHierarchy *retypeKh = static_cast<KlassHierarchy *>(m->GetAnalysisResult(MoPhase_CHA, module));

  Retype retype(module, localMp, mirBuilder, retypeKh);
  retype.DoRetype();

  mempoolctrler.DeleteMemPool(localMp);
  return cg;
}

AnalysisResult *DoIPODevirtulize::Run(MIRModule *module, ModuleResultMgr *m) {
  MemPool *mp = mempoolctrler.NewMemPool("ipodevirulize mempool");
  KlassHierarchy *cha = static_cast<KlassHierarchy *>(m->GetAnalysisResult(MoPhase_CHA, module));
  CHECK_FATAL(cha != nullptr, "");
  IPODevirtulize *dev = mp->New<IPODevirtulize>(module, mp, cha);
  // Devirtualize vcall of final variable
  dev->DevirtualFinal();
  mempoolctrler.DeleteMemPool(mp);
  return nullptr;
}
}  // namespace maple
