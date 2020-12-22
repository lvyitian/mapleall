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

#ifndef MAPLE_ME_INCLUDE_LFO_MIR_NODES_H_
#define MAPLE_ME_INCLUDE_LFO_MIR_NODES_H_
#include "me_ir.h"
#include "mir_nodes.h"

namespace maple {

class LfoParentPart {
 public:
  LfoParentPart *parent;

 public:
  LfoParentPart (LfoParentPart *pt) : parent(pt) {}
  virtual BaseNode *Cvt2BaseNode() = 0;
  bool IsParentOf(LfoParentPart *canNode) {
    LfoParentPart *dParent = canNode->parent;
    while(dParent && dParent != this)
     dParent = dParent->parent;
    return dParent != NULL;
  }
};

class LfoUnaryNode : public UnaryNode, public LfoParentPart{
 public:
  LfoUnaryNode(Opcode o, PrimType ptyp, LfoParentPart *parent) : UnaryNode(o, ptyp), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoTypeCvtNode : public TypeCvtNode, public LfoParentPart {
 public:
  LfoTypeCvtNode(Opcode o, PrimType ptyp, LfoParentPart *parent) :
        TypeCvtNode(o, ptyp), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoRetypeNode : public RetypeNode, public LfoParentPart {
 public:
  LfoRetypeNode(Opcode o, PrimType ptyp,  LfoParentPart *parent) :
        RetypeNode(ptyp), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoExtractbitsNode : public ExtractbitsNode, public LfoParentPart {
 public:
  LfoExtractbitsNode(Opcode o, PrimType ptyp, LfoParentPart *parent) : ExtractbitsNode(o, ptyp), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoIreadNode : public IreadNode, public LfoParentPart {
 public:
  IvarMeExpr *ivarx;

 public:
  LfoIreadNode(Opcode o, PrimType ptyp, LfoParentPart *parent, IvarMeExpr *v) :
       IreadNode(o, ptyp), LfoParentPart(parent), ivarx(v) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoIaddrofNode : public IreadNode, public LfoParentPart {
 public:
   LfoIaddrofNode(Opcode o, PrimType pty, LfoParentPart *parent) : IreadNode(o, pty), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoBinaryNode : public BinaryNode, public LfoParentPart {
 public:
   LfoBinaryNode (Opcode o, PrimType typ, LfoParentPart *parent) : BinaryNode (o,typ),
   LfoParentPart (parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoCompareNode : public CompareNode, public LfoParentPart {
 public:
  LfoCompareNode (Opcode o, PrimType typ, PrimType otype, BaseNode *l, BaseNode *r, LfoParentPart *parent) :
   CompareNode (o, typ, otype, l, r), LfoParentPart (parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoTernaryNode : public TernaryNode, public LfoParentPart {
 public:
  LfoTernaryNode (Opcode o, PrimType ptyp,  LfoParentPart *parent) : TernaryNode(o, ptyp),
     LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoNaryNode : public NaryNode, public LfoParentPart {
 public:
  LfoNaryNode (MapleAllocator *allc, Opcode o, PrimType pty, LfoParentPart *parent) : NaryNode (allc, o, pty),
     LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoIntrinsicopNode : public IntrinsicopNode, public LfoParentPart {
 public:
  LfoIntrinsicopNode (MapleAllocator *allc, Opcode o, PrimType ptyp, TyIdx tidx, LfoParentPart *parent) :
     IntrinsicopNode(allc, o, ptyp, tidx), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoConstvalNode : public ConstvalNode, public LfoParentPart {
 public:
  LfoConstvalNode(MIRConst *constv, LfoParentPart *parent) : ConstvalNode(constv->type->primType, constv), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoConststrNode : public ConststrNode, public LfoParentPart {
 public:
  LfoConststrNode(PrimType ptyp, UStrIdx i, LfoParentPart *parent) : ConststrNode(ptyp, i), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoConststr16Node : public Conststr16Node, public LfoParentPart {
 public:
  LfoConststr16Node(PrimType ptyp, U16StrIdx i, LfoParentPart *parent) : Conststr16Node(ptyp, i), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoSizeoftypeNode : public SizeoftypeNode, public LfoParentPart {
 public:
  LfoSizeoftypeNode(PrimType ptyp, TyIdx tidx, LfoParentPart *parent) : SizeoftypeNode(ptyp, tidx), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoArrayNode : public ArrayNode, public LfoParentPart {
 public:
  LfoArrayNode(MapleAllocator *allc, PrimType typ, TyIdx idx, LfoParentPart *parent) : ArrayNode (allc, typ, idx), LfoParentPart (parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoDreadNode : public AddrofNode, public LfoParentPart {
 public:
  VarMeExpr *varx;

 public:
  LfoDreadNode(PrimType ptyp, StIdx sidx, FieldID fid, LfoParentPart *parent, VarMeExpr *v) :
      AddrofNode(OP_dread, ptyp, sidx, fid), LfoParentPart(parent), varx(v) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoAddrofNode : public AddrofNode, public LfoParentPart {
 public:
  LfoAddrofNode(PrimType ptyp, StIdx sidx, FieldID fid, LfoParentPart *parent) : AddrofNode(OP_addrof, ptyp, sidx, fid), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoRegreadNode : public RegreadNode, public LfoParentPart {
 public:
  ScalarMeExpr *scalarx;

 public:
  LfoRegreadNode(LfoParentPart *parent, ScalarMeExpr *s) : RegreadNode(), LfoParentPart(parent), scalarx(s) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoAddroffuncNode : public AddroffuncNode, public LfoParentPart {
 public:
  LfoAddroffuncNode(PrimType ptyp, PUIdx pidx, LfoParentPart *parent) : AddroffuncNode(ptyp, pidx), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoAddroflabelNode : public AddroflabelNode, public LfoParentPart {
 public:
  LfoAddroflabelNode(uint32 o, LfoParentPart *parent) : AddroflabelNode(o), LfoParentPart (parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoGCMallocNode : public GCMallocNode, public LfoParentPart {
 public:
  LfoGCMallocNode(Opcode o, PrimType pty, TyIdx tidx, LfoParentPart *parent) : GCMallocNode(o, pty, tidx), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoFieldsDistNode : public FieldsDistNode, public LfoParentPart {
 public:
  LfoFieldsDistNode(PrimType ptyp, TyIdx tidx, FieldID f1, FieldID f2, LfoParentPart *parent) :
    FieldsDistNode(ptyp, tidx, f1, f2), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoIassignNode : public IassignNode, public LfoParentPart {
 public:
  IassignMeStmt *iasgn;

 public:
  LfoIassignNode(LfoParentPart *parent, IassignMeStmt *ia) : IassignNode(), LfoParentPart(parent), iasgn(ia) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoGotoNode : public GotoNode, public LfoParentPart {
 public:
  LfoGotoNode (Opcode o, LfoParentPart *parent) : GotoNode(o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoDassignNode : public DassignNode, public LfoParentPart {
 public:
  DassignMeStmt *dasgn;

 public:
  LfoDassignNode(LfoParentPart *parent, DassignMeStmt *das) : DassignNode(), LfoParentPart(parent), dasgn(das) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoRegassignNode : public RegassignNode, public LfoParentPart {
 public:
  AssignMeStmt *asgn;

 public:
  LfoRegassignNode(LfoParentPart *parent, AssignMeStmt *as) : RegassignNode(), LfoParentPart(parent), asgn(as) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoCondGotoNode : public CondGotoNode, public LfoParentPart {
 public:
  LfoCondGotoNode(Opcode o, LfoParentPart *parent) : CondGotoNode(o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoWhileStmtNode : public WhileStmtNode, public LfoParentPart {
 public:
  LfoWhileStmtNode(LfoParentPart *parent) : WhileStmtNode(OP_while), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoDoloopNode : public DoloopNode, public LfoParentPart {
 public:
  LfoDoloopNode (LfoParentPart *parent) : DoloopNode (), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
  void InitLfoDoloopNode (StIdx stIdx, bool ispg, BaseNode *startExp, BaseNode *contExp, BaseNode *incrExp, BlockNode *blk) {
    doVarStIdx = stIdx;
    isPreg = ispg;
    startExpr = startExp;
    condExpr = contExp;
    incrExpr = incrExp;
    doBody = blk;
  }
};

class LfoNaryStmtNode : public NaryStmtNode, public LfoParentPart {
 public:
  LfoNaryStmtNode (MapleAllocator *allc, Opcode o, LfoParentPart *parent) :
     NaryStmtNode(allc, o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoReturnStmtNode : public NaryStmtNode, public LfoParentPart {
 public:
  RetMeStmt *retmestmt;

 public:
  LfoReturnStmtNode(MapleAllocator *allc, LfoParentPart *parent, RetMeStmt *ret) :
     NaryStmtNode(allc, OP_return), LfoParentPart(parent), retmestmt(ret) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoCallNode : public CallNode, public LfoParentPart {
 public:
  CallMeStmt *callmestmt;

 public:
  LfoCallNode(MapleAllocator *allc, Opcode o, LfoParentPart *parent, CallMeStmt *cl) : CallNode(allc, o), LfoParentPart(parent), callmestmt(cl) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoIcallNode : public IcallNode, public LfoParentPart {
 public:
  IcallMeStmt *icallmestmt;

 public:
   LfoIcallNode (MapleAllocator *allc, Opcode o, TyIdx idx, LfoParentPart *parent, IcallMeStmt *ic) :
     IcallNode (allc, o, idx), LfoParentPart(parent), icallmestmt(ic) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoIntrinsiccallNode : public IntrinsiccallNode, public LfoParentPart {
 public:
  IntrinsiccallMeStmt *intrinmestmt;

 public:
   LfoIntrinsiccallNode (MapleAllocator *allc, Opcode o, MIRIntrinsicID id, LfoParentPart *parent, IntrinsiccallMeStmt *intncall) :
     IntrinsiccallNode(allc, o, id), LfoParentPart(parent), intrinmestmt(intncall) {}
   BaseNode *Cvt2BaseNode () { return this; }
};

class LfoIfStmtNode : public IfStmtNode, public LfoParentPart {
 public:
  LfoIfStmtNode(LfoParentPart *parent) : IfStmtNode(),LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoBlockNode : public BlockNode, public LfoParentPart {
 public:
  LfoBlockNode (LfoParentPart *parent) : BlockNode(),LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoStmtNode : public StmtNode, public LfoParentPart {
 public:
  LfoStmtNode (LfoParentPart *parent, Opcode o) : StmtNode (o),
      LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

class LfoUnaryStmtNode : public UnaryStmtNode, public LfoParentPart {
 public:
  LfoUnaryStmtNode(Opcode o, LfoParentPart *parent) : UnaryStmtNode(o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoSwitchNode : public SwitchNode, public LfoParentPart {
 public:
  LfoSwitchNode(MapleAllocator *allc, LfoParentPart *parent) :
    SwitchNode(allc), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() { return this; }
};

}  // namespace maple
#endif  // MAPLE_LFO_INCLUDE_LFO_MIR_NODES_H_
