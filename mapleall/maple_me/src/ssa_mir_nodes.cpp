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


#include "ssa_mir_nodes.h"
#include "opcode_info.h"
#include "mir_function.h"
#include "printing.h"
#include "ssa_tab.h"

using namespace std;
namespace maple {

void GenericSSAPrint(MIRModule *mod, BaseNode *x, int32 indent, StmtsSSAPart *stmtsSsaprt) {
  x->Dump(mod, indent);
  // print SSAPart
  Opcode op = x->op;
  StmtNode *y = static_cast<StmtNode *>(x);
  switch (op) {
    case OP_maydassign:
    case OP_dassign: {
      LogInfo::MapleLogger() << " ";
      MayDefPartWithVersionSt *thessapart = static_cast<MayDefPartWithVersionSt *>(stmtsSsaprt->SsapartOf(y));
      thessapart->ssaVar->Dump(mod);
      thessapart->MayDefPart::Print(mod, indent);
      return;
    }
    case OP_regassign: {
      LogInfo::MapleLogger() << "  ";
      VersionSt *thessapart = static_cast<VersionSt *>(stmtsSsaprt->SsapartOf(y));
      thessapart->Dump(mod);
      return;
    }
    case OP_iassign: {
      MayDefPart *thessapart = static_cast<MayDefPart *>(stmtsSsaprt->SsapartOf(y));
      thessapart->MayDefPart::Print(mod, indent);
      return;
    }
    case OP_throw:
    case OP_retsub:
    case OP_return: {
      MayUsePart *thessapart = static_cast<MayUsePart *>(stmtsSsaprt->SsapartOf(y));
      thessapart->MayUsePart::Print(mod, indent);
      LogInfo::MapleLogger() << std::endl;
      return;
    }
    case OP_syncenter:
    case OP_syncexit:
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
    case OP_intrinsiccallwithtype: {
      MayDefMayUsePart *thessapart = static_cast<MayDefMayUsePart *>(stmtsSsaprt->SsapartOf(y));
      thessapart->MayUsePart::Print(mod, indent);
      LogInfo::MapleLogger() << std::endl;
      thessapart->MayDefPart::Print(mod, indent);
      return;
    }
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
      MayDefMayUseMustDefPart *thessapart = static_cast<MayDefMayUseMustDefPart *>(stmtsSsaprt->SsapartOf(y));
      thessapart->MayUsePart::Print(mod, indent);
      thessapart->MustDefPart::Print(mod, indent);
      thessapart->MayDefPart::Print(mod, indent);
      return;
    }
    default:
      return;
  }
}

void SSAGenericInsertMayUseNode(const BaseNode *x, VersionSt *mayUse, StmtsSSAPart *stmtsSsaprt) {
  MapleMap<OStIdx, MayUseNode> *mayUseNodes = SSAGenericGetMayUseNode(x, stmtsSsaprt);
  if (mayUseNodes == nullptr) {
    return;
  }
  mayUseNodes->insert(std::make_pair(mayUse->ost->index, MayUseNode(mayUse)));
}

MapleMap<OStIdx, MayUseNode> *SSAGenericGetMayUseNode(const BaseNode *x, StmtsSSAPart *stmtsSsaprt) {
  switch (x->op) {
    case OP_syncenter:
    case OP_syncexit:
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
    case OP_intrinsiccallwithtype: {
      MayDefMayUsePart *thessapart =
        static_cast<MayDefMayUsePart *>(stmtsSsaprt->SsapartOf(static_cast<const StmtNode *>(x)));
      return &thessapart->mayUseNodes;
    }
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
      MayDefMayUseMustDefPart *thessapart =
        static_cast<MayDefMayUseMustDefPart *>(stmtsSsaprt->SsapartOf(static_cast<const StmtNode *>(x)));
      return &thessapart->mayUseNodes;
    }
    case OP_return:
    case OP_throw:
    case OP_gosub:
    case OP_retsub: {
      MayUsePart *thessapart = static_cast<MayUsePart *>(stmtsSsaprt->SsapartOf(static_cast<const StmtNode *>(x)));
      return &thessapart->mayUseNodes;
    }
    default:
      CHECK_FATAL(false, "Unexpected may use node");
  }
}

void SSAGenericInsertMayDefNode(const StmtNode *x, VersionSt *sym, StmtNode *s, StmtsSSAPart *stmtsSsaprt) {
  MapleMap<OStIdx, MayDefNode> *mayDefNodes = SSAGenericGetMayDefNodes(x, stmtsSsaprt);
  if (mayDefNodes == nullptr) {
    return;
  }
  mayDefNodes->insert(std::make_pair(sym->ost->index, MayDefNode(sym, s)));
}

MapleMap<OStIdx, MayDefNode> *SSAGenericGetMayDefNodes(const StmtNode *x, StmtsSSAPart *stmtsSsaprt) {
  if (x == nullptr) {
    return nullptr;
  }
  switch (x->op) {
    case OP_syncenter:
    case OP_syncexit:
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
    case OP_intrinsiccallwithtype: {
      MayDefMayUsePart *thessapart =
        static_cast<MayDefMayUsePart *>(stmtsSsaprt->SsapartOf(static_cast<const StmtNode *>(x)));
      return &thessapart->mayDefNodes;
    }
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
      MayDefMayUseMustDefPart *thessapart =
        static_cast<MayDefMayUseMustDefPart *>(stmtsSsaprt->SsapartOf(static_cast<const StmtNode *>(x)));
      return &thessapart->mayDefNodes;
    }
    case OP_iassign: {
      MayDefPart *thessapart = static_cast<MayDefPart *>(stmtsSsaprt->SsapartOf(static_cast<const StmtNode *>(x)));
      return &thessapart->mayDefNodes;
    }
    case OP_maydassign:
    case OP_dassign: {
      MayDefPartWithVersionSt *thessapart =
        static_cast<MayDefPartWithVersionSt *>(stmtsSsaprt->SsapartOf(static_cast<const StmtNode *>(x)));
      return &thessapart->mayDefNodes;
    }
    default:
      CHECK_FATAL(false, "Unexpected may def node");
  }
}

static MapleMap<OStIdx, MayDefNode> *SSAGenericGetMayDefsFromVersionSt(VersionSt *vst, StmtsSSAPart *ssapart,
                                                                         std::unordered_set<VersionSt *> &visited) {
  if (vst == nullptr || vst->IsInitVersion() || visited.find(vst) != visited.end()) {
    return nullptr;
  }
  visited.insert(vst);

  if (vst->defType == VersionSt::kPhi) {
    PhiNode *phi = vst->defStmt.phi;
    for (uint32_t i = 0; i < phi->phiOpnds.size(); i++) {
      VersionSt *vsym = phi->phiOpnds[i];
      MapleMap<OStIdx, MayDefNode> *maydefs = SSAGenericGetMayDefsFromVersionSt(vsym, ssapart, visited);
      if (maydefs != nullptr) {
        return maydefs;
      }
    }
  } else if (vst->defType == VersionSt::kMayDef) {
    MayDefNode *mayDef = vst->defStmt.mayDef;
    return SSAGenericGetMayDefNodes(mayDef->stmt, ssapart);
  } else {
    // NYI
    return nullptr;
  }
  return nullptr;
}

MapleMap<OStIdx, MayDefNode> *SSAGenericGetMayDefsFromVersionSt(VersionSt *sym, StmtsSSAPart *ssapart) {
  std::unordered_set<VersionSt *> visited;
  return SSAGenericGetMayDefsFromVersionSt(sym, ssapart, visited);
}

MapleVector<MustDefNode> *SSAGenericGetMustDefNode(const BaseNode *x, StmtsSSAPart *stmtsSsaprt) {
  switch (x->op) {
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
      MayDefMayUseMustDefPart *thessapart =
        static_cast<MayDefMayUseMustDefPart *>(stmtsSsaprt->SsapartOf(static_cast<const StmtNode *>(x)));
      return &thessapart->mustDefNodes;
    }
    default:
      CHECK_FATAL(false, "Unexpected may def node");
  }
}

bool HasMayUseDefPart(const StmtNode *x) {
  switch (x->op) {
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
      return true;
    default:
      return false;
  }
}

bool HasMayDefPart(const StmtNode *x) {
  if (HasMayUseDefPart(x)) {
    return true;
  }
  switch (x->op) {
    case OP_iassign:
    case OP_dassign:
      return true;
    default:
      return false;
  }
}

bool HasMayUsePart(const BaseNode *x) {
  if (HasMayUseDefPart(static_cast<const StmtNode *>(x))) {
    return true;
  }
  switch (x->op) {
    case OP_iread:
    case OP_throw:
    case OP_gosub:
    case OP_retsub:
      return true;
    default:
      return false;
  }
}

bool HasMayUseOpnd(BaseNode *x, SSATab *func) {
  if (HasMayUsePart(x)) {
    MapleMap<OStIdx, MayUseNode> *mayuses = SSAGenericGetMayUseNode(x, &func->stmtsSSAPart);
    if (!mayuses->empty()) {
      return true;
    }
  }

  for (int32 i = 0; i < x->NumOpnds(); i++) {
    if (HasMayUseOpnd(x->Opnd(i), func)) {
      return true;
    }
  }
  return false;
}

bool HasMayDef(const StmtNode *x, SSATab *func) {
  if (HasMayDefPart(x)) {
    MapleMap<OStIdx, MayDefNode> *maydefs = SSAGenericGetMayDefNodes(x, &func->stmtsSSAPart);
    if (!maydefs->empty()) {
      return true;
    }
  }
  return false;
}

}  // namespace maple
