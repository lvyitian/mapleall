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

#include "ssa.h"
#include "ssa_tab.h"
#include <iostream>
#include "ssa_mir_nodes.h"
#include "ver_symbol.h"
#include "dominance.h"

using namespace std;

namespace maple {

void SSA::InitRenameStack(OriginalStTable *otable, uint32 bbsize, VersionStTable &versttab) {
  vstStacks.resize(otable->Size(), nullptr);
  bbRenamed.resize(bbsize);

  for (uint32 i = 1; i < otable->Size(); i++) {
    OriginalSt *ost = otable->GetOriginalStFromid(OStIdx(i));
    if (ost->indirectLev < 0) {
      continue;
    }
    MapleStack<VersionSt *> *vstack = ssaAlloc.GetMemPool()->New<MapleStack<VersionSt *>>(ssaAlloc.Adapter());
    vstack->push(versttab.GetZeroVersionSt(ost));
    vstStacks[i] = vstack;
  }

  for (uint32 i = 0; i < bbRenamed.size(); i++) {
    bbRenamed[i] = false;
  }
}

VersionSt *SSA::CreateNewVersion(VersionSt *vsym, BB *defBb) {
  // volatile variables will keep zero version.
  if (vsym->GetOrigSt()->IsVolatile()) {
    return vsym;
  }

  VersionSt *newvsym = nullptr;
  if (!runRenameOnly) {
    CHECK_FATAL(vsym->version == INIT_VERSION, "rename before?");
    newvsym = ssaTab->versionStTable.CreateNextVersionSt(vsym->ost);
  } else {
    CHECK_FATAL(vsym->version != INIT_VERSION, "rename before?");
    newvsym = vsym;
  }
  vstStacks[vsym->GetOrigIdx().idx]->push(newvsym);
  newvsym->SetDefBB(defBb);
  return newvsym;
}

void SSA::RenamePhi(BB *bb) {
  MapleMap<OriginalSt *, PhiNode>::iterator phiIt;
  for (phiIt = bb->phiList.begin(); phiIt != bb->phiList.end(); phiIt++) {
    VersionSt *vsym = (*phiIt).second.result;

    VersionSt *newVsym = CreateNewVersion(vsym, bb);
    (*phiIt).second.result = newVsym;
    newVsym->defType = VersionSt::kPhi;
    newVsym->defStmt.phi = &(*phiIt).second;
  }
}

void SSA::RenameDefs(StmtNode *stmt, BB *defBb) {
  Opcode opcode = stmt->op;
  switch (opcode) {
    case OP_regassign: {
      RegassignNode *regnode = static_cast<RegassignNode *>(stmt);
      if (regnode->regIdx < 0) {
        return;
      }
      VersionSt *thessapart = static_cast<VersionSt *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      VersionSt *newVsym = CreateNewVersion(thessapart, defBb);
      newVsym->defType = VersionSt::kRegassign;
      newVsym->defStmt.regassign = regnode;
      ssaTab->stmtsSSAPart.SetSsapartOf(stmt, newVsym);
      return;
    }
    case OP_dassign: {
      DassignNode *dnode = static_cast<DassignNode *>(stmt);
      MayDefPartWithVersionSt *thessapart =
        static_cast<MayDefPartWithVersionSt *>(ssaTab->stmtsSSAPart.SsapartOf(stmt));
      VersionSt *newVsym = CreateNewVersion(thessapart->ssaVar, defBb);
      thessapart->ssaVar = newVsym;
      newVsym->defType = VersionSt::kDassign;
      newVsym->defStmt.dassign = dnode;
    }
    // intentional fall though
    case OP_call:
    case OP_virtualcall:
    case OP_virtualicall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_interfaceicall:
    case OP_customcall:
    case OP_polymorphiccall:
    case OP_icall:
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_intrinsiccallwithtype:
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_icallassigned:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned:
    case OP_syncenter:
    case OP_syncexit:
    case OP_maydassign:
    case OP_iassign: {
      MapleMap<OStIdx, MayDefNode> *maydeflist = SSAGenericGetMayDefNodes(stmt, &ssaTab->stmtsSSAPart);
      for (MapleMap<OStIdx, MayDefNode>::iterator it = maydeflist->begin(); it != maydeflist->end(); it++) {
        MayDefNode &mayDef = it->second;
        VersionSt *vsym = mayDef.result;
        mayDef.opnd = vstStacks[vsym->GetOrigIdx().idx]->top();
        VersionSt *newVsym = CreateNewVersion(vsym, defBb);
        mayDef.result = newVsym;
        newVsym->defType = VersionSt::kMayDef;
        newVsym->defStmt.mayDef = &mayDef;
        if (opcode == OP_iassign && mayDef.base != nullptr) {
          mayDef.base = vstStacks[mayDef.base->GetOrigIdx().idx]->top();
        }
      }
      return;
    }
    default:
      return;
  }
}

void SSA::RenameMustDefs(const StmtNode *stmt, BB *defBb) {
  Opcode opcode = stmt->op;
  switch (opcode) {
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_icallassigned:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned: {
      MapleVector<MustDefNode> *mustdefs = SSAGenericGetMustDefNode(stmt, &ssaTab->stmtsSSAPart);
      MapleVector<MustDefNode>::iterator it = mustdefs->begin();
      for (; it != mustdefs->end(); it++) {
        VersionSt *newVsym = CreateNewVersion((*it).result, defBb);
        (*it).result = newVsym;
        newVsym->defType = VersionSt::kMustDef;
        newVsym->defStmt.mustDef = &(*it);
      }
      return;
    }
    default:
      return;
  }
}

void SSA::RenameMayUses(BaseNode *node) {
  if (node->op == OP_iread) {
    IreadSSANode *iread = static_cast<IreadSSANode *>(node);
    VersionSt *vsym = iread->mayUse.opnd;
    CHECK_FATAL(vsym != nullptr, "SSA::RenameMayUses: iread has no mayUse opnd");
    {
      CHECK(vsym->GetOrigIdx().idx < vstStacks.size(), "index out of range in SSA::RenameMayUses");
      iread->mayUse.opnd = vstStacks[vsym->GetOrigIdx().idx]->top();
    }
    return;
  }
  MapleMap<OStIdx, MayUseNode> *mayuselist = SSAGenericGetMayUseNode(node, &ssaTab->stmtsSSAPart);
  MapleMap<OStIdx, MayUseNode>::iterator it = mayuselist->begin();
  for (; it != mayuselist->end(); it++) {
    MayUseNode &mayUse = it->second;
    VersionSt *vsym = mayUse.opnd;
    mayUse.opnd = vstStacks[vsym->GetOrigIdx().idx]->top();
  }
}

void SSA::RenameExpr(BaseNode *expr) {
  if (expr->op == OP_addrof || expr->op == OP_dread) {
    AddrofSSANode *addofnode = static_cast<AddrofSSANode *>(expr);
    VersionSt *vsym = addofnode->ssaVar;
    addofnode->ssaVar = vstStacks[vsym->GetOrigIdx().idx]->top();
    return;
  } else if (expr->op == OP_regread) {
    RegreadSSANode *regnode = static_cast<RegreadSSANode *>(expr);
    if (regnode->regIdx < 0) {
      return;
    }
    VersionSt *vsym = regnode->ssaVar;
    CHECK(vsym->GetOrigIdx().idx < vstStacks.size(), "index out of range in SSA::RenameExpr");
    regnode->ssaVar = vstStacks[vsym->GetOrigIdx().idx]->top();
    return;
  } else if (expr->op == OP_iread) {
    RenameMayUses(expr);
    RenameExpr(expr->Opnd(0));
  } else {
    for (int32 i = 0; i < expr->NumOpnds(); i++) {
      RenameExpr(expr->Opnd(i));
    }
  }
}

void SSA::RenameUses(StmtNode *stmt) {
  Opcode opcode = stmt->op;
  switch (opcode) {
    case OP_brfalse:
    case OP_brtrue: {
      RenameExpr(stmt->Opnd(0));
      break;
    }
    case OP_return: {
      NaryStmtNode *retnode = static_cast<NaryStmtNode *>(stmt);
      BaseNode *retvalue = retnode->NumOpnds() == 0 ? nullptr : retnode->Opnd(0);
      RenameMayUses(stmt);
      if (retvalue) {
        RenameExpr(retvalue);
      }
      break;
    }
    case OP_call:
    case OP_virtualcall:
    case OP_virtualicall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_interfaceicall:
    case OP_customcall:
    case OP_polymorphiccall:
    case OP_icall:
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_intrinsiccallwithtype:
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_virtualicallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_interfaceicallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_icallassigned:
    case OP_intrinsiccallassigned:
    case OP_xintrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned:
    case OP_syncenter:
    case OP_syncexit: {
      RenameMayUses(stmt);
      for (int32 i = 0; i < stmt->NumOpnds(); i++) {
        BaseNode *argexpr = stmt->Opnd(i);
        RenameExpr(argexpr);
      }
      break;
    }
    case OP_maydassign:
    case OP_dassign: {
      DassignNode *dnode = static_cast<DassignNode *>(stmt);
      RenameExpr(dnode->GetRhs());
      break;
    }
    case OP_regassign: {
      RegassignNode *rnode = static_cast<RegassignNode *>(stmt);
      if (rnode->regIdx < 0) {
        return;
      }
      RenameExpr(rnode->Opnd(0));
      break;
    }
    case OP_iassign: {
      IassignNode *inode = static_cast<IassignNode *>(stmt);
      RenameExpr(inode->addrExpr);
      RenameExpr(inode->rhs);
      break;
    }
    case OP_throw:
      RenameMayUses(stmt);
    //  fallthrough;
    case OP_assertnonnull:
    case OP_eval:
    case OP_free:
    case OP_igoto:
    case OP_switch: {
      BaseNode *argexpr = stmt->Opnd(0);
      RenameExpr(argexpr);
      break;
    }
    case OP_gosub:
    case OP_retsub:
      RenameMayUses(stmt);
      break;
    case OP_comment:
    case OP_label:
    case OP_goto:
    case OP_jstry:
    case OP_jscatch:
    case OP_finally:
    case OP_endtry:
    case OP_cleanuptry:
    case OP_try:
    case OP_javatry:
    case OP_cpptry:
    case OP_catch:
    case OP_javacatch:
    case OP_cppcatch:
    case OP_membaracquire:
    case OP_membarrelease:
    case OP_membarstoreload:
    case OP_membarstorestore:
      break;
    default:
      CHECK_FATAL(false, "NYI");
      break;
  }
}

void SSA::RenamePhiUseInSucc(BB *bb) {
  for (BB *succBb : bb->succ) {
    // find index of bb in succ_bb->pred[]
    uint32 index = 0;
    while (index < succBb->pred.size()) {
      if (succBb->pred[index] == bb) {
        break;
      }
      index++;
    }
    ASSERT(index < succBb->pred.size(), "RenamePhiUseInSucc: cannot find corresponding pred");
    // rename the phiOpnds[index] in all the phis in succ_bb
    for (MapleMap<OriginalSt *, PhiNode>::iterator phiIt = succBb->phiList.begin(); phiIt != succBb->phiList.end();
         phiIt++)
      phiIt->second.phiOpnds[index] = vstStacks[phiIt->second.phiOpnds[index]->GetOrigIdx().idx]->top();
  }
}

void SSA::RenameBB(BB *bb) {
  if (bbRenamed[bb->id.idx]) {
    return;
  } else {
    bbRenamed[bb->id.idx] = true;
  }

  // record stack size for variable versions before processing rename. It is used for stack pop up.
  std::vector<uint32> oriStackSize(vstStacks.size(), 0);

  for (uint32 i = 1; i < vstStacks.size(); i++) {
    if (vstStacks[i] == nullptr) {
      continue;
    }
    oriStackSize[i] = vstStacks[i]->size();
  }

  RenamePhi(bb);

  for (auto stmt : bb->stmtNodeList) {
    RenameUses(stmt);
    RenameDefs(stmt, bb);
    RenameMustDefs(stmt, bb);
  }

  RenamePhiUseInSucc(bb);

  // Rename child in Dominator Tree.
  CHECK(bb->id.idx < dom_->domChildren.size(), "index out of range in MeSSA::RenameBB");
  MapleSet<BBId> *children = &dom_->domChildren[bb->id.idx];
  for (BBId child : *children) {
    RenameBB(bbVec[child.idx]);
  }

  for (uint32 i = 1; i < vstStacks.size(); i++) {
    if (vstStacks[i] == nullptr) {
      continue;
    }
    while (vstStacks[i]->size() > oriStackSize[i]) {
      vstStacks[i]->pop();
    }
  }
}

void PhiNode::Dump(const MIRModule *mod) {
  result->Dump(mod);
  LogInfo::MapleLogger() << " = PHI(";
  for (uint32 i = 0; i < phiOpnds.size(); i++) {
    phiOpnds[i]->Dump(mod);
    if (i < phiOpnds.size() - 1) {
      LogInfo::MapleLogger() << ',';
    }
  }
  LogInfo::MapleLogger() << ")" << endl;
}

}  // namespace maple
