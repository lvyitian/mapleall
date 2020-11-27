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
  LfoUnaryNode(Opcode o, LfoParentPart *parent) : UnaryNode(o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoTypeCvtNode : public TypeCvtNode, public LfoParentPart {
 public:
   LfoTypeCvtNode(Opcode o, LfoParentPart *parent) :
        TypeCvtNode(o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoRetypeNode : public RetypeNode, public LfoParentPart {
 public:
   LfoRetypeNode(Opcode o, LfoParentPart *parent) :
        RetypeNode (), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoExtractbitsNode : public ExtractbitsNode, public LfoParentPart {
 public:
   LfoExtractbitsNode(Opcode o, LfoParentPart *parent) : ExtractbitsNode(o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoIreadNode : public IreadNode, public LfoParentPart {
 public:
   LfoIreadNode(Opcode o, LfoParentPart *parent) : IreadNode(o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoIreadoffNode : public IreadoffNode, public LfoParentPart {
 public:
   LfoIreadoffNode (LfoParentPart *parent) : IreadoffNode(),
            LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoBinaryNode : public BinaryNode, public LfoParentPart {
 public:
   LfoBinaryNode (Opcode o, PrimType typ, LfoParentPart *parent) : BinaryNode (o,typ),
   LfoParentPart (parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoCompareNode : public CompareNode, public LfoParentPart {
 public:
   LfoCompareNode (Opcode o, PrimType typ, PrimType otype, BaseNode *l, BaseNode *r, LfoParentPart *parent) :
   CompareNode (o, typ, otype, l, r), LfoParentPart (parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoTernaryNode : public TernaryNode, public LfoParentPart {
 public:
   LfoTernaryNode (Opcode o, LfoParentPart *parent) : TernaryNode(o),
     LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoNaryNode : public NaryNode, public LfoParentPart {
 public:
   LfoNaryNode (MapleAllocator *allc, Opcode o, LfoParentPart *parent) : NaryNode (allc, o),
     LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoIntrinsicopNode : public IntrinsicopNode, public LfoParentPart {
 public:
  LfoIntrinsicopNode (MapleAllocator *allc, Opcode o, LfoParentPart *parent) :
     IntrinsicopNode(allc, o), LfoParentPart(parent) {}
  LfoIntrinsicopNode (MapleAllocator *allc, TyIdx tyIdx, LfoParentPart *parent) :
     IntrinsicopNode(allc, OP_intrinsicop, tyIdx), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoConstvalNode : public ConstvalNode, public LfoParentPart {
 public:
   LfoConstvalNode(LfoParentPart *parent) : ConstvalNode(), LfoParentPart(parent) {}
   LfoConstvalNode(MIRConst *constv, LfoParentPart *parent) : ConstvalNode(constv), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoConststrNode : public ConststrNode, public LfoParentPart {
 public:
   LfoConststrNode(LfoParentPart *parent) : ConststrNode(), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoConststr16Node : public Conststr16Node, public LfoParentPart {
 public:
  LfoConststr16Node(LfoParentPart *parent) : Conststr16Node(), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoSizeoftypeNode : public SizeoftypeNode, public LfoParentPart {
 public:
  LfoSizeoftypeNode(LfoParentPart *parent) : SizeoftypeNode(), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoArrayNode : public ArrayNode, public LfoParentPart {
 public:
   LfoArrayNode(MapleAllocator *allc, PrimType typ, TyIdx idx, LfoParentPart *parent) : ArrayNode (allc, typ, idx),
   LfoParentPart (parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoDreadNode : public AddrofNode, public LfoParentPart {
 public:
   MapleForwardList<StmtNode *> defsList;

 public:
  LfoDreadNode (MapleAllocator *allocator, LfoParentPart *parent) : AddrofNode(OP_dread),
    LfoParentPart(parent), defsList(allocator->Adapter()){}
  BaseNode *Cvt2BaseNode() {
    return this;
  }

};

class LfoAddrofNode : public AddrofNode, public LfoParentPart {
 public:
  LfoAddrofNode(LfoParentPart *parent) : AddrofNode(OP_addrof),
  LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoRegreadNode : public RegreadNode, public LfoParentPart {
 public:
  LfoRegreadNode (LfoParentPart *parent) : RegreadNode(), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoAddroffuncNode : public AddroffuncNode, public LfoParentPart {
 public:
  LfoAddroffuncNode(LfoParentPart *parent) : AddroffuncNode(), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoAddroflabelNode : public AddroflabelNode, public LfoParentPart {
 public:
  LfoAddroflabelNode(LfoParentPart *parent) : AddroflabelNode(), LfoParentPart (parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoGCMallocNode : public GCMallocNode, public LfoParentPart {
 public:
  LfoGCMallocNode(Opcode o, LfoParentPart *parent) : GCMallocNode(o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoFieldsDistNode : public FieldsDistNode, public LfoParentPart {
 public:
  LfoFieldsDistNode(LfoParentPart *parent) : FieldsDistNode(), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoResolveFuncNode : public ResolveFuncNode, public LfoParentPart {
 public:
  LfoResolveFuncNode(Opcode o, LfoParentPart *parent) : ResolveFuncNode(o),
    LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoIassignNode : public IassignNode, public LfoParentPart {
 public:
  MapleForwardList<LfoParentPart *> usesList;

 public:
   LfoIassignNode(MapleAllocator *allc, LfoParentPart *parent) : IassignNode(),
     LfoParentPart(parent), usesList(allc->Adapter()) {}
   BaseNode *Cvt2BaseNode() {
     return this;
   }
};

class LfoGotoNode : public GotoNode, public LfoParentPart {
 public:
  LfoGotoNode (Opcode o, LfoParentPart *parent) : GotoNode(o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoDassignNode : public DassignNode, public LfoParentPart {
 public:
 MapleForwardList<LfoParentPart *> usesList; // the use-chain of this definition

 public:
  LfoDassignNode(MapleAllocator *allc, LfoParentPart *parent) : DassignNode(), LfoParentPart(parent), usesList(allc->Adapter()) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoRegassignNode : public RegassignNode, public LfoParentPart {
 public:
 MapleForwardList<LfoParentPart *> usesList; // the use-chain of this definition

 public:
  LfoRegassignNode(MapleAllocator *allc, LfoParentPart *parent) : RegassignNode(), LfoParentPart(parent), usesList(allc->Adapter()) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoCondGotoNode : public CondGotoNode, public LfoParentPart {
 public:
  LfoCondGotoNode(Opcode o, LfoParentPart *parent) : CondGotoNode(o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoWhileStmtNode : public WhileStmtNode, public LfoParentPart {
 public:
  LfoWhileStmtNode(LfoParentPart *parent) : WhileStmtNode(OP_while), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoDoloopNode : public DoloopNode, public LfoParentPart {
 public:
  LfoDoloopNode (LfoParentPart *parent) : DoloopNode (), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
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
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoCallNode : public CallNode, public LfoParentPart {
 public:
  MapleForwardList<LfoParentPart *> usesList;
 public:
  LfoCallNode (MapleAllocator *allc, Opcode o, LfoParentPart *parent) : CallNode(allc, o), LfoParentPart(parent),usesList(allc->Adapter()) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoIcallNode : public IcallNode, public LfoParentPart {
 public:
   MapleForwardList<LfoParentPart *> usesList;
 public:
   LfoIcallNode (MapleAllocator *allc, Opcode o, TyIdx idx, LfoParentPart *parent) :
     IcallNode (allc, o, idx), LfoParentPart(parent), usesList(allc->Adapter()) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoIntrinsiccallNode : public IntrinsiccallNode, public LfoParentPart {
 public:
   MapleForwardList<LfoParentPart *> usesList;
 public:
   LfoIntrinsiccallNode (MapleAllocator *allc, Opcode o, MIRIntrinsicID id, LfoParentPart *parent) :
     IntrinsiccallNode(allc, o, id), LfoParentPart(parent), usesList(allc->Adapter()) {}
   BaseNode *Cvt2BaseNode () {
     return this;
   }
};

class LfoLabelNode : public LabelNode, public LfoParentPart {
 public:
  LfoLabelNode(LfoParentPart *parent) : LabelNode(), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoCommentNode : public CommentNode, public LfoParentPart {
 public:
  LfoCommentNode(MapleAllocator *allc, LfoParentPart *parent) : CommentNode(allc), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoIfStmtNode : public IfStmtNode, public LfoParentPart {
 public:
  LfoIfStmtNode(LfoParentPart *parent) : IfStmtNode(),LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoBlockNode : public BlockNode, public LfoParentPart {
 public:
  LfoBlockNode (LfoParentPart *parent) : BlockNode(),LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};


class LfoGosubNode : public LfoGotoNode {
 public:
  MapleForwardList<LfoParentPart *> usesList;
 public:
  LfoGosubNode(MapleAllocator *allc, LfoParentPart *parent) : LfoGotoNode(OP_gosub, parent),
    usesList(allc->Adapter()) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};


class LfoStmtNode : public StmtNode, public LfoParentPart {
 public:
  LfoStmtNode (LfoParentPart *parent, Opcode o) : StmtNode (o),
      LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoWithUsesStmtNode : public LfoStmtNode {
 public:
   MapleForwardList<LfoParentPart *> usesList;
 public:
  LfoWithUsesStmtNode(MapleAllocator *allc, LfoParentPart *parent, Opcode o) : LfoStmtNode(parent, o),
      usesList(allc->Adapter()) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoTryNode : public TryNode, public LfoParentPart {
 public:
  LfoTryNode(MapleAllocator *allc, Opcode o, LfoParentPart *parent) : TryNode(allc, o),
    LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoCppCatchNode : public CppCatchNode, public LfoParentPart {
 public:
  LfoCppCatchNode(LfoParentPart *parent) : CppCatchNode(), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoCatchNode : public CatchNode, public LfoParentPart {
 public:
  LfoCatchNode(MapleAllocator *alloc, LfoParentPart *parent) : CatchNode(alloc),
    LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoUnaryStmtNode : public UnaryStmtNode, public LfoParentPart {
 public:
  LfoUnaryStmtNode(Opcode o, LfoParentPart *parent) : UnaryStmtNode(o), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoThrowStmt : public LfoUnaryStmtNode {
 public:
  MapleForwardList<LfoParentPart *> usesList;
 public:
  LfoThrowStmt(MapleAllocator *allc, Opcode o, LfoParentPart *parent) :
    LfoUnaryStmtNode(o, parent), usesList(allc->Adapter()) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoSyncStmt : public LfoNaryStmtNode {
 public:
  MapleForwardList<LfoParentPart *> usesList;
 public:
  LfoSyncStmt(MapleAllocator *allc, Opcode o, LfoParentPart *parent) :
    LfoNaryStmtNode(allc, o, parent), usesList(allc->Adapter()) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

class LfoSwitchNode : public SwitchNode, public LfoParentPart {
 public:
  LfoSwitchNode(MapleAllocator *allc, LfoParentPart *parent) :
    SwitchNode(allc), LfoParentPart(parent) {}
  BaseNode *Cvt2BaseNode() {
    return this;
  }
};

}  // namespace maple
#endif  // MAPLE_LFO_INCLUDE_LFO_MIR_NODES_H_
