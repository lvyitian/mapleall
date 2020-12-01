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


#ifndef MAPLE_ME_INCLUDE_SSA_MIR_NODES_H
#define MAPLE_ME_INCLUDE_SSA_MIR_NODES_H

#include "bb.h"
#include "ver_symbol.h"
#include <iostream>

/* This file define data structures to store SSA information in the IR
   instructions */

namespace maple {

struct OriginalStComparator {
  bool operator()(const OriginalSt *lhs, const OriginalSt *rhs) const {
    return lhs->index < rhs->index;
  }
};

class MayDefNode {
 public:
  VersionSt *opnd;
  VersionSt *result;
  StmtNode *stmt;
  VersionSt *base; // only provided if indirectLev is 1 and attached to iassign

  explicit MayDefNode(VersionSt *sym, StmtNode *st) : opnd(sym), result(sym), stmt(st), base(nullptr) {}

  ~MayDefNode() {}

  VersionSt *GetOpnd() {
    return opnd;
  };
  VersionSt *GetResult() {
    return result;
  };
  bool IsRequired() const {
    return result->IsLive();
  };
  void MarkRequired() {
    result->MarkLive();
  };
  void Dump(const MIRModule *mod) {
    result->Dump(mod);
    LogInfo::MapleLogger() << " = MAYD(";
    opnd->Dump(mod);
    LogInfo::MapleLogger() << ")";
  }
};

class MayUseNode {
 public:
  VersionSt *opnd;

  explicit MayUseNode(VersionSt *sym) : opnd(sym) {}

  ~MayUseNode() {}

  VersionSt *GetOpnd() {
    return opnd;
  };
  void Dump(const MIRModule *mod) const {
    LogInfo::MapleLogger() << " MAYU(";
    opnd->Dump(mod);
    LogInfo::MapleLogger() << ")";
  }
};

// this is only used in the callassigned type of call statements
class MustDefNode {
 public:
  VersionSt *result;
  StmtNode *stmt;

  MustDefNode() {
    result = nullptr, stmt = nullptr;
  }

  explicit MustDefNode(VersionSt *sym, StmtNode *st) : result(sym), stmt(st) {}

  ~MustDefNode() {}

  VersionSt *GetResult() {
    return result;
  };
  bool IsRequired() const {
    return result->IsLive();
  };
  void MarkRequired() {
    result->MarkLive();
  };
  void Dump(const MIRModule *mod) {
    result->Dump(mod);
    LogInfo::MapleLogger() << " = MUSTDEF" << std::endl;
  }
};

class MayDefPart {
 public:
  MapleMap<OStIdx, MayDefNode> mayDefNodes;

  MayDefPart(MapleAllocator *alloc) : mayDefNodes(std::less<OStIdx>(), alloc->Adapter()) {}

  virtual ~MayDefPart() {}

  void InsertMayDefNode(VersionSt *sym, StmtNode *s) {
    mayDefNodes.insert(std::make_pair(sym->ost->index, MayDefNode(sym, s)));
  }

  void Print(const MIRModule *mod, int32 indent) {
    for (auto it = mayDefNodes.begin(); it != mayDefNodes.end(); it++) {
      MayDefNode &mayDef = it->second;
      mayDef.Dump(mod);
    }
  }
};

class MayUsePart {
 public:
  MapleMap<OStIdx, MayUseNode> mayUseNodes;

  MayUsePart(MapleAllocator *alloc) : mayUseNodes(std::less<OStIdx>(), alloc->Adapter()) {}

  virtual ~MayUsePart() {}

  void Print(const MIRModule *mod, int32 indent) {
    for (std::pair<OStIdx, MayUseNode> mapitem : mayUseNodes) {
      mapitem.second.Dump(mod);
    }
  }
};

class MustDefPart {
 public:
  MapleVector<MustDefNode> mustDefNodes;

  MustDefPart(MapleAllocator *alloc) : mustDefNodes(alloc->Adapter()) {}

  virtual ~MustDefPart() {}

  void InsertMustDefNode(VersionSt *sym, StmtNode *s) {
    mustDefNodes.push_back(MustDefNode(sym, s));
  }

  void Print(const MIRModule *mod, int32 indent) {
    for (MapleVector<MustDefNode>::iterator it = mustDefNodes.begin(); it != mustDefNodes.end(); it++) {
      MustDefNode &mustDef = *it;
      mustDef.Dump(mod);
    }
  }
};

class MayDefPartWithVersionSt : public MayDefPart {
 public:
  VersionSt *ssaVar;

  MayDefPartWithVersionSt(MapleAllocator *alloc) : MayDefPart(alloc), ssaVar(nullptr) {}

  ~MayDefPartWithVersionSt() {}
};

class MayDefMayUsePart : public MayDefPart, public MayUsePart {
 public:
  MayDefMayUsePart(MapleAllocator *alloc) : MayDefPart(alloc), MayUsePart(alloc) {}

  ~MayDefMayUsePart() {}
};

class MayDefMayUseMustDefPart : public MayDefPart, public MayUsePart, public MustDefPart {
 public:
  MayDefMayUseMustDefPart(MapleAllocator *alloc) : MayDefPart(alloc), MayUsePart(alloc), MustDefPart(alloc) {}

  ~MayDefMayUseMustDefPart() {}
};

// statement nodes are covered by StmtsSSAPart

class StmtsSSAPart {
 public:
  MemPool *ssaPartMp;
  MapleAllocator ssaPartAlloc;
  // Key of ssaPart is stmtID
  // Each element of ssaPart, depending on the stmt, can be pointer to one of:
  // (1) MayDefPart
  // (2) MayUsePart
  // (3) MayDefMayUsePart
  // (4) MayDefPartWithVersionSt
  // (5) MayDefMayUseMustDefPart
  // (6) VersionSt
  MapleMap<uint32, void *> ssaPart;  // key is stmtID

  explicit StmtsSSAPart(MemPool *mp) : ssaPartMp(mp), ssaPartAlloc(mp), ssaPart(ssaPartAlloc.Adapter()) {}

  ~StmtsSSAPart() {}

  void *SsapartOf(const StmtNode *s) {
    return ssaPart[s->stmtID];
  }

  template <class T>
  void SetSsapartOf(const StmtNode *s, T *p) {
    ssaPart[s->stmtID] = p;
  }
};

// The following expression nodes need extra fields to represent SSA

class AddrofSSANode : public AddrofNode {
 public:
  VersionSt *ssaVar;

  AddrofSSANode(const AddrofNode *addrnode) : AddrofNode(addrnode->op) {
    primType = addrnode->primType;
    stIdx = addrnode->stIdx;
    fieldID = addrnode->fieldID;
    ssaVar = nullptr;
  }

  ~AddrofSSANode() {}

  void Print(const MIRModule *mod, int32 indent) const {
    AddrofNode::Dump(mod, indent);
    if (ssaVar) {
      ssaVar->Dump(mod, true);
    }
  }
};

class IreadSSANode : public IreadNode {
 public:
  MayUseNode mayUse;

  IreadSSANode(MapleAllocator *alloc, IreadNode *inode) : IreadNode(inode->op), mayUse(nullptr) {
    primType = inode->primType;
    tyIdx = inode->tyIdx;
    fieldID = inode->fieldID;
    uOpnd = inode->uOpnd;
  }

  ~IreadSSANode() {}

  void Print(const MIRModule *mod, int32 indent) const {
    if (mayUse.opnd != nullptr) {
      mayUse.Dump(mod);
    }
    IreadNode::Dump(mod, indent);
  }
};

class RegreadSSANode : public RegreadNode {
 public:
  VersionSt *ssaVar;

  RegreadSSANode(const RegreadNode *rreadnode) : RegreadNode() {
    primType = rreadnode->primType;
    regIdx = rreadnode->regIdx;
    ssaVar = nullptr;
  }

  ~RegreadSSANode() {}

  void Print(const MIRModule *mod, int32 indent) const {
    RegreadNode::Dump(mod, indent);
    if (ssaVar) {
      ssaVar->Dump(mod, true);
    }
  }
};

extern void GenericSSAPrint(MIRModule *mod, BaseNode *x, int32 indent, StmtsSSAPart *stmtsSsaprt);
extern void SSAGenericInsertMayUseNode(const BaseNode *x, VersionSt *usesym, StmtsSSAPart *stmtsSsaprt);
extern void SSAGenericInsertMayDefNode(const StmtNode *x, VersionSt *sym, StmtNode *s, StmtsSSAPart *stmtsSsaprt);
extern MapleMap<OStIdx, MayUseNode> *SSAGenericGetMayUseNode(const BaseNode *x, StmtsSSAPart *stmtsSsaprt);
extern MapleMap<OStIdx, MayDefNode> *SSAGenericGetMayDefNodes(const StmtNode *x, StmtsSSAPart *stmtsSsaprt);
extern MapleMap<OStIdx, MayDefNode> *SSAGenericGetMayDefsFromVersionSt(VersionSt *sym, StmtsSSAPart *ssapart);
extern MapleVector<MustDefNode> *SSAGenericGetMustDefNode(const BaseNode *x, StmtsSSAPart *stmtsSsaprt);
bool HasMayUseDefPart(const StmtNode *x);
bool HasMayDefPart(const StmtNode *x);
bool HasMayUsePart(const BaseNode *x);
bool HasMayUseOpnd(BaseNode *x, SSATab *func);
bool HasMayDef(const StmtNode *x, SSATab *func);
inline bool HasMallocOpnd(const BaseNode *x) {
  return x->op == OP_malloc || x->op == OP_gcmalloc || x->op == OP_gcmallocjarray || x->op == OP_alloca ||
         x->op == OP_stackmalloc || x->op == OP_stackmallocjarray;
}

}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_SSA_MIR_NODES_H
