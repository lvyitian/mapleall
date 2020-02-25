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

#ifndef MAPLE_IR_INCLUDE_MIR_NODES_H
#define MAPLE_IR_INCLUDE_MIR_NODES_H
#include <iostream>
#include <sstream>
#include <utility>
#include "opcodes.h"
#include "opcode_info.h"
#include "mir_type.h"
#include "cmpl.h"
#include "mir_module.h"
#include "mir_const.h"
#include "maple_string.h"
#include "ptr_list_ref.h"

namespace maple {
class MIRPregTable;
class TypeTable;

struct RegFieldPair {
  FieldID fieldID;
  PregIdx16 pregIdx;

  RegFieldPair() : fieldID(0), pregIdx(0) {}

  RegFieldPair(FieldID fidx, PregIdx16 pidx) : fieldID(fidx), pregIdx(pidx) {}

  bool IsReg() const {
    return pregIdx > 0;
  }

  FieldID GetFieldid() const {
    return fieldID;
  }

  PregIdx16 GetPregidx() const {
    return pregIdx;
  }

  void SetFldid(FieldID fld) {
    fieldID = fld;
  }

  void SetPregidx(PregIdx16 idx) {
    pregIdx = idx;
  }
};

using CallReturnPair = std::pair<StIdx, RegFieldPair>;
using CallReturnVector = MapleVector<CallReturnPair>;

// Made public so that other modules (such as maplebe) can print intrinsic names
// in debug information or comments in assembly files.
const char *GetIntrinsicName(MIRIntrinsicID intrn);
MIRIntrinsicID LowerToIntrinsic(std::string funcName);
std::string GetIntrinsicFuncName(MIRIntrinsicID intrn);

class BaseNode : public base_node_t {
 public:
  explicit BaseNode(Opcode o) {
    op = o;
    primType = kPtyInvalid;
    typeflag = 0;
    numOpnds = 0;
  }

  BaseNode(Opcode o, uint8 numopr) {
    op = o;
    primType = kPtyInvalid;
    typeflag = 0;
    numOpnds = numopr;
  }

  BaseNode(Opcode o, PrimType typ, uint8 numopr) {
    op = o;
    primType = typ;
    typeflag = 0;
    numOpnds = numopr;
  }

  virtual ~BaseNode() = default;

  virtual BaseNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<BaseNode>(*this);
  }

  virtual BaseNode *MakeCopy(MIRModule *mod)  // non-recursive
  {
    return MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  virtual BaseNode *CloneTree(MapleAllocator *allocator) {
    return allocator->mp->New<BaseNode>(*this);
  }

  virtual BaseNode *CloneTree(MIRModule *mod)  // recursive
  {
    return CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  void DumpBase(const MIRModule *mod, int32 indent) const;
  virtual void Dump(const MIRModule *mod, int32 indent) const {
    DumpBase(mod, indent);
  }

  void Dump(const MIRModule *mod) const {
    Dump(mod, 0);
    LogInfo::MapleLogger() << std::endl;
  }

  virtual bool HasSymbol(MIRModule *mod, MIRSymbol *st) {
    return false;
  }

  virtual uint8 SizeOfInstr() {
    return kOpcodeInfo.GetTableItemAt(op).instrucSize;
  }
  const char *GetOpName() const;
  bool MayThrowException(void);
  virtual int32 NumOpnds(void) {
    return numOpnds;
  }

  virtual BaseNode *Opnd(size_t i) const {
    ASSERT(0, "override needed");
    return nullptr;
  }

  virtual void SetOpnd(BaseNode *node, size_t i) {
    ASSERT(0, "This should not happen");
  }

  virtual bool IsLeaf(void) {
    return true;
  }

  virtual CallReturnVector *GetCallReturnVector() {
    return nullptr;
  }

  virtual MIRType *GetCallReturnType() {
    return nullptr;
  }

  virtual bool IsUnaryNode(void) {
    return false;
  }

  virtual bool IsBinaryNode(void) {
    return false;
  }

  bool IsCondBr() const {
    return kOpcodeInfo.IsCondBr(op);
  }

  virtual bool Verify() const {
    return true;
  }

  PrimType GetPrimType() const {
    return primType;
  }

  void SetPrimType(PrimType pty) {
    primType = pty;
  }
};

class UnaryNode : public BaseNode {
 public:
  BaseNode *uOpnd;
 public:
  UnaryNode(Opcode o) : BaseNode(o), uOpnd(nullptr) {
    numOpnds = 1;
  }

  UnaryNode(Opcode o, PrimType typ) : BaseNode(o, typ, 1), uOpnd(nullptr) {}

  UnaryNode(Opcode o, PrimType typ, BaseNode *expr) : BaseNode(o, typ, 1), uOpnd(expr) {}

  ~UnaryNode() {}

  void DumpOpnd(const MIRModule *mod, int32 indent) const;
  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  UnaryNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<UnaryNode>(*this);
  }

  UnaryNode *MakeCopy(MIRModule *mod) {
    return UnaryNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  UnaryNode *CloneTree(MapleAllocator *allocator) {
    UnaryNode *nd = allocator->mp->New<UnaryNode>(*this);
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  UnaryNode *CloneTree(MIRModule *mod) {
    return UnaryNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  BaseNode *Opnd(size_t i) const {
    ASSERT(i == 0, "Invalid operand idx in UnaryNode");
    return uOpnd;
  }

  bool HasSymbol(MIRModule *mod, MIRSymbol *st) {
    return uOpnd->HasSymbol(mod, st);
  }

  int32 NumOpnds(void) {
    return 1;
  }

  void SetOpnd(BaseNode *node, size_t i) {
    uOpnd = node;
  }

  bool IsLeaf(void) {
    return false;
  }

  bool IsUnaryNode(void) {
    return true;
  }
};

class TypeCvtNode : public UnaryNode {
 public:
  PrimType fromPrimType : 8;
  uint8 fromtypeflag;  // a flag to speed up type related operations
  uint8 _padding_[2];
 public:
  explicit TypeCvtNode(Opcode o) : UnaryNode(o), fromPrimType(kPtyInvalid) {}

  TypeCvtNode(Opcode o, PrimType typ) : UnaryNode(o, typ), fromPrimType(kPtyInvalid) {}

  TypeCvtNode(Opcode o, PrimType typ, PrimType fromtyp, BaseNode *expr)
      : UnaryNode(o, typ, expr), fromPrimType(fromtyp), fromtypeflag(0) {}

  ~TypeCvtNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  TypeCvtNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<TypeCvtNode>(*this);
  }

  TypeCvtNode *MakeCopy(MIRModule *mod) {
    return TypeCvtNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  TypeCvtNode *CloneTree(MapleAllocator *allocator) {
    TypeCvtNode *nd = allocator->mp->New<TypeCvtNode>(*this);
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  TypeCvtNode *CloneTree(MIRModule *mod) {
    return TypeCvtNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  PrimType FromType(void) const {
    return fromPrimType;
  }
};

// used for retype
class RetypeNode : public TypeCvtNode {
 public:
  TyIdx tyIdx;

 public:
  RetypeNode() : TypeCvtNode(OP_retype), tyIdx(0) {}

  explicit RetypeNode(PrimType typ) : TypeCvtNode(OP_retype, typ), tyIdx(0) {}

  ~RetypeNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  RetypeNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<RetypeNode>(*this);
  }

  RetypeNode *MakeCopy(MIRModule *mod) {
    return RetypeNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  RetypeNode *CloneTree(MapleAllocator *allocator) {
    RetypeNode *nd = allocator->mp->New<RetypeNode>(*this);
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  RetypeNode *CloneTree(MIRModule *mod) {
    return RetypeNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// used for extractbits, sext, zext
class ExtractbitsNode : public UnaryNode {
 public:
  uint8 bitsOffset;
  uint8 bitsSize;
  uint16 _padding_;
 public:
  explicit ExtractbitsNode(Opcode o) : UnaryNode(o), bitsOffset(0), bitsSize(0) {}

  ExtractbitsNode(Opcode o, PrimType typ) : UnaryNode(o, typ), bitsOffset(0), bitsSize(0) {}

  ExtractbitsNode(Opcode o, PrimType typ, uint8 offset, uint8 size)
      : UnaryNode(o, typ), bitsOffset(offset), bitsSize(size) {}

  ExtractbitsNode(Opcode o, PrimType typ, uint8 offset, uint8 size, BaseNode *expr)
      : UnaryNode(o, typ, expr), bitsOffset(offset), bitsSize(size) {}

  ~ExtractbitsNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  ExtractbitsNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<ExtractbitsNode>(*this);
  }

  ExtractbitsNode *MakeCopy(MIRModule *mod) {
    return ExtractbitsNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  ExtractbitsNode *CloneTree(MapleAllocator *allocator) {
    ExtractbitsNode *nd = allocator->mp->New<ExtractbitsNode>(*this);
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  ExtractbitsNode *CloneTree(MIRModule *mod) {
    return ExtractbitsNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// used for gcmalloc, stackmalloc, gcpermalloc
class GCMallocNode : public BaseNode {
 public:
  TyIdx tyIdx;

 public:
  explicit GCMallocNode(Opcode o) : BaseNode(o), tyIdx(0) {}

  GCMallocNode(Opcode o, PrimType typ, TyIdx tidx) : BaseNode(o, typ, 0), tyIdx(tidx) {}

  ~GCMallocNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  GCMallocNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<GCMallocNode>(*this);
  }

  GCMallocNode *MakeCopy(MIRModule *mod) {
    return GCMallocNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  GCMallocNode *CloneTree(MapleAllocator *allocator) {
    GCMallocNode *nd = allocator->mp->New<GCMallocNode>(*this);
    return nd;
  }

  GCMallocNode *CloneTree(MIRModule *mod) {
    return GCMallocNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class JarrayMallocNode : public UnaryNode {
 public:
  TyIdx tyIdx;

 public:
  explicit JarrayMallocNode(Opcode o) : UnaryNode(o), tyIdx(0) {}

  JarrayMallocNode(Opcode o, PrimType typ) : UnaryNode(o, typ), tyIdx(0) {}

  JarrayMallocNode(Opcode o, PrimType typ, TyIdx typeIdx) : UnaryNode(o, typ), tyIdx(typeIdx) {}

  ~JarrayMallocNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  JarrayMallocNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<JarrayMallocNode>(*this);
  }

  JarrayMallocNode *MakeCopy(MIRModule *mod) {
    return JarrayMallocNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  JarrayMallocNode *CloneTree(MapleAllocator *allocator) {
    JarrayMallocNode *nd = allocator->mp->New<JarrayMallocNode>(*this);
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  JarrayMallocNode *CloneTree(MIRModule *mod) {
    return JarrayMallocNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// iaddrof also use this node
class IreadNode : public UnaryNode {
 public:
  TyIdx tyIdx;
  FieldID fieldID;

 public:
  explicit IreadNode(Opcode o) : UnaryNode(o), tyIdx(0), fieldID(0) {}

  IreadNode(Opcode o, PrimType typ) : UnaryNode(o, typ), tyIdx(0), fieldID(0) {}

  IreadNode(Opcode o, PrimType typ, TyIdx typeIdx, FieldID fid) : UnaryNode(o, typ), tyIdx(typeIdx), fieldID(fid) {}

  IreadNode(Opcode o, PrimType typ, TyIdx typeIdx, FieldID fid, BaseNode *expr)
      : UnaryNode(o, typ, expr), tyIdx(typeIdx), fieldID(fid) {}

  ~IreadNode() = default;
  virtual void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  IreadNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<IreadNode>(*this);
  }

  IreadNode *MakeCopy(MIRModule *mod) {
    return IreadNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  IreadNode *CloneTree(MapleAllocator *allocator) {
    IreadNode *nd = allocator->mp->New<IreadNode>(*this);
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  IreadNode *CloneTree(MIRModule *mod) {
    return IreadNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  // the base of an address expr is either a leaf or an iread
  BaseNode *GetAddrExprBase() const {
    BaseNode *base = uOpnd;
    while (base->NumOpnds() != 0 && base->op != OP_iread) {
      base = base->Opnd(0);
    }
    return base;
  }
};

// IaddrofNode has the same member fields and member methods as IreadNode
using IaddrofNode = IreadNode;

class IreadoffNode : public UnaryNode {
 public:
  int32 offset;
 public:
  IreadoffNode() : UnaryNode(OP_ireadoff), offset(0) {}

  IreadoffNode(PrimType primType, int32 ofst) : UnaryNode(OP_ireadoff, primType), offset(ofst) {}

  ~IreadoffNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  IreadoffNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<IreadoffNode>(*this);
  }

  IreadoffNode *MakeCopy(MIRModule *mod) {
    return IreadoffNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  IreadoffNode *CloneTree(MapleAllocator *allocator) {
    IreadoffNode *nd = allocator->mp->New<IreadoffNode>(*this);
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  IreadoffNode *CloneTree(MIRModule *mod) {
    return IreadoffNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class IreadFPoffNode : public BaseNode {
 public:
  int32 offset;
 public:
  IreadFPoffNode() : BaseNode(OP_ireadfpoff), offset(0) {}

  ~IreadFPoffNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  IreadFPoffNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<IreadFPoffNode>(*this);
  }

  IreadFPoffNode *MakeCopy(MIRModule *mod) {
    return IreadFPoffNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  IreadFPoffNode *CloneTree(MapleAllocator *allocator) {
    IreadFPoffNode *nd = allocator->mp->New<IreadFPoffNode>(*this);
    return nd;
  }

  IreadFPoffNode *CloneTree(MIRModule *mod) {
    return IreadFPoffNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class IreadPCoffNode : public IreadFPoffNode {
  public:

  IreadPCoffNode(Opcode o, PrimType typ, uint8 numopns) {
    op = o;
    primType = typ;
    numOpnds = numopns;
  }
};

typedef IreadPCoffNode AddroffPCNode;

class BinaryOpnds {
 public:
  BaseNode *bOpnd[2];

 public:
  void Dump(const MIRModule *mod, int32 indent) const;
};

class BinaryNode : public BaseNode, public BinaryOpnds {
 public:
  BinaryNode(Opcode o) : BaseNode(o) {
    numOpnds = 2;
  }

  BinaryNode(Opcode o, PrimType typ) : BaseNode(o, typ, 2) {}

  BinaryNode(Opcode o, PrimType typ, BaseNode *l, BaseNode *r) : BaseNode(o, typ, 2) {
    bOpnd[0] = l;
    bOpnd[1] = r;
  }

  void Dump(const MIRModule *mod, int32 indent) const;
  void Dump(const MIRModule *mod) const;
  bool Verify() const;

  BinaryNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<BinaryNode>(*this);
  }

  BinaryNode *MakeCopy(MIRModule *mod) {
    return BinaryNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  BinaryNode *CloneTree(MapleAllocator *allocator) {
    BinaryNode *nd = allocator->mp->New<BinaryNode>(*this);
    nd->bOpnd[0] = bOpnd[0]->CloneTree(allocator);
    nd->bOpnd[1] = bOpnd[1]->CloneTree(allocator);
    return nd;
  }

  BinaryNode *CloneTree(MIRModule *mod) {
    return BinaryNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  bool IsCommutative() const {
    switch (op) {
      case OP_add:
      case OP_mul:
      case OP_band:
      case OP_bior:
      case OP_bxor:
      case OP_land:
      case OP_lior:
        return true;
      default:
        return false;
    }
  }

  bool HasSymbol(MIRModule *mod, MIRSymbol *st) {
    return bOpnd[0]->HasSymbol(mod, st) || bOpnd[1]->HasSymbol(mod, st);
  }

  BaseNode *Opnd(size_t i) const {
    ASSERT(i < 2, "invalid operand idx in BinaryNode");
    return bOpnd[i];
  }

  int32 NumOpnds(void) {
    return 2;
  }

  void SetOpnd(BaseNode *node, size_t i) {
    bOpnd[i] = node;
  }

  bool IsLeaf(void) {
    return false;
  }

  bool IsBinaryNode(void) {
    return true;
  }
};

class CompareNode : public BinaryNode {
 public:
  PrimType opndType : 8;  // type of operands.
  uint8 opndtypeflag;     // typeflag of opntype.
  uint8 _padding_[2];
 public:
  explicit CompareNode(Opcode o) : BinaryNode(o), opndType(kPtyInvalid) {}

  CompareNode(Opcode o, PrimType typ) : BinaryNode(o, typ), opndType(kPtyInvalid) {}

  CompareNode(Opcode o, PrimType typ, PrimType otype, BaseNode *l, BaseNode *r)
      : BinaryNode(o, typ, l, r), opndType(otype) {}

  ~CompareNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  void Dump(const MIRModule *mod) const;
  bool Verify() const;

  CompareNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<CompareNode>(*this);
  }

  CompareNode *MakeCopy(MIRModule *mod) {
    return CompareNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  CompareNode *CloneTree(MapleAllocator *allocator) {
    CompareNode *nd = allocator->mp->New<CompareNode>(*this);
    nd->bOpnd[0] = bOpnd[0]->CloneTree(allocator);
    nd->bOpnd[1] = bOpnd[1]->CloneTree(allocator);
    return nd;
  }

  CompareNode *CloneTree(MIRModule *mod) {
    return CompareNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class DepositbitsNode : public BinaryNode {
 public:
  uint8 bitsOffset;
  uint8 bitsSize;
 public:
  DepositbitsNode() : BinaryNode(OP_depositbits), bitsOffset(0), bitsSize(0) {}

  DepositbitsNode(Opcode o, PrimType typ) : BinaryNode(o, typ), bitsOffset(0), bitsSize(0) {}

  DepositbitsNode(Opcode o, PrimType typ, uint8 offset, uint8 size, BaseNode *l, BaseNode *r)
      : BinaryNode(o, typ, l, r), bitsOffset(offset), bitsSize(size) {}

  ~DepositbitsNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  DepositbitsNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<DepositbitsNode>(*this);
  }

  DepositbitsNode *MakeCopy(MIRModule *mod) {
    return DepositbitsNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  DepositbitsNode *CloneTree(MapleAllocator *allocator) {
    DepositbitsNode *nd = allocator->mp->New<DepositbitsNode>(*this);
    nd->bOpnd[0] = bOpnd[0]->CloneTree(allocator);
    nd->bOpnd[1] = bOpnd[1]->CloneTree(allocator);
    return nd;
  }

  DepositbitsNode *CloneTree(MIRModule *mod) {
    return DepositbitsNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// used for resolveinterfacefunc, resolvevirtualfunc
// bOpnd[0] stores base vtab/itab address
// bOpnd[1] stores offset
class ResolveFuncNode : public BinaryNode {
 public:
  PUIdx puIdx;

 public:
  explicit ResolveFuncNode(Opcode o) : BinaryNode(o), puIdx(0) {}

  ResolveFuncNode(Opcode o, PrimType typ) : BinaryNode(o, typ), puIdx(0) {}

  ResolveFuncNode(Opcode o, PrimType typ, PUIdx idx) : BinaryNode(o, typ), puIdx(idx) {}

  ResolveFuncNode(Opcode o, PrimType typ, PUIdx pIdx, BaseNode *opnd0, BaseNode *opnd1)
      : BinaryNode(o, typ, opnd0, opnd1), puIdx(pIdx) {}

  ~ResolveFuncNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  ResolveFuncNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<ResolveFuncNode>(*this);
  }

  ResolveFuncNode *MakeCopy(MIRModule *mod) {
    return ResolveFuncNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  ResolveFuncNode *CloneTree(MapleAllocator *allocator) {
    ResolveFuncNode *nd = allocator->mp->New<ResolveFuncNode>(*this);
    nd->bOpnd[0] = bOpnd[0]->CloneTree(allocator);
    nd->bOpnd[1] = bOpnd[1]->CloneTree(allocator);
    return nd;
  }

  ResolveFuncNode *CloneTree(MIRModule *mod) {
    return ResolveFuncNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  BaseNode *GetTabBaseAddr() const {
    return bOpnd[0];
  }

  BaseNode *GetOffset() const {
    return bOpnd[1];
  }
};

class TernaryNode : public BaseNode {
 public:
  BaseNode *topnd[3];

 public:
  explicit TernaryNode(Opcode o) : BaseNode(o, 3) {
    topnd[0] = nullptr;
    topnd[1] = nullptr;
    topnd[2] = nullptr;
  }

  TernaryNode(Opcode o, PrimType typ) : BaseNode(o, typ, 3) {
    topnd[0] = nullptr;
    topnd[1] = nullptr;
    topnd[2] = nullptr;
  }

  TernaryNode(Opcode o, PrimType typ, BaseNode *e0, BaseNode *e1, BaseNode *e2) : BaseNode(o, typ, 3) {
    topnd[0] = e0;
    topnd[1] = e1;
    topnd[2] = e2;
  }

  ~TernaryNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  TernaryNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<TernaryNode>(*this);
  }

  TernaryNode *MakeCopy(MIRModule *mod) {
    return TernaryNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  TernaryNode *CloneTree(MapleAllocator *allocator) {
    TernaryNode *nd = allocator->mp->New<TernaryNode>(*this);
    nd->topnd[0] = topnd[0]->CloneTree(allocator);
    nd->topnd[1] = topnd[1]->CloneTree(allocator);
    nd->topnd[2] = topnd[2]->CloneTree(allocator);
    return nd;
  }

  TernaryNode *CloneTree(MIRModule *mod) {
    return TernaryNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
  BaseNode *Opnd(size_t i) const {
    return topnd[i];
  }

  int32 NumOpnds(void) {
    return 3;
  }

  void SetOpnd(BaseNode *node, size_t i) {
    topnd[i] = node;
  }

  bool IsLeaf(void) {
    return false;
  }

  bool HasSymbol(MIRModule *mod, MIRSymbol *st) {
    return topnd[0]->HasSymbol(mod, st) || topnd[1]->HasSymbol(mod, st) || topnd[2]->HasSymbol(mod, st);
  }
};

class NaryOpnds {
 public:
  MapleVector<BaseNode *> nOpnd;

 public:
  explicit NaryOpnds(MapleAllocator *mpallocter) : nOpnd(mpallocter->Adapter()) {}

  void Dump(const MIRModule *mod, int32 indent) const;
  bool VerifyOpnds() const;
};

class NaryNode : public BaseNode, public NaryOpnds {
 public:
  NaryNode(MapleAllocator *allocator, Opcode o) : BaseNode(o), NaryOpnds(allocator) {}

  NaryNode(const MIRModule *mod, Opcode o) : NaryNode(mod->CurFuncCodeMemPoolAllocator(), o) {}

  NaryNode(MapleAllocator *allocator, Opcode o, PrimType typ) : BaseNode(o, typ, 0), NaryOpnds(allocator) {}

  NaryNode(const MIRModule *mod, Opcode o, PrimType typ) : NaryNode(mod->CurFuncCodeMemPoolAllocator(), o, typ) {}

  NaryNode(MapleAllocator *allocator, const NaryNode *node)
    : BaseNode(node->op, node->primType, node->numOpnds), NaryOpnds(allocator) {}

  NaryNode(const MIRModule *mod, const NaryNode *node) : NaryNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  NaryNode(NaryNode &node) = delete;
  NaryNode &operator=(const NaryNode &node) = delete;
  ~NaryNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  NaryNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<NaryNode>(allocator, this);
  }

  NaryNode *MakeCopy(MIRModule *mod) {
    return NaryNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  NaryNode *CloneTree(MapleAllocator *allocator) {
    NaryNode *nd = allocator->mp->New<NaryNode>(allocator, this);
    for (uint32 i = 0; i < nOpnd.size(); i++) {
      nd->nOpnd.push_back(nOpnd[i]->CloneTree(allocator));
    }
    return nd;
  }

  NaryNode *CloneTree(MIRModule *mod) {
    return NaryNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  BaseNode *Opnd(size_t i) const {
    return nOpnd.at(i);
  }

  int32 NumOpnds(void) {
    ASSERT(numOpnds == nOpnd.size(), "NaryNode has wrong numOpnds field");
    return nOpnd.size();
  }

  void SetOpnd(BaseNode *node, size_t i) {
    nOpnd.at(i) = node;
  }

  bool IsLeaf(void) {
    return false;
  }

  bool HasSymbol(MIRModule *mod, MIRSymbol *st) {
    for (uint32 i = 0; i < nOpnd.size(); i++)
      if (nOpnd[i]->HasSymbol(mod, st)) {
        return true;
      }
    return false;
  }
};

class IntrinsicopNode : public NaryNode {
 public:
  MIRIntrinsicID intrinsic;
  TyIdx tyIdx;
 public:
  explicit IntrinsicopNode(MapleAllocator *allocator, Opcode o, TyIdx typeIdx = TyIdx())
      : NaryNode(allocator, o), intrinsic(INTRN_UNDEFINED), tyIdx(typeIdx) {}

  explicit IntrinsicopNode(const MIRModule *mod, Opcode o, TyIdx typeIdx = TyIdx())
      : IntrinsicopNode(mod->CurFuncCodeMemPoolAllocator(), o, typeIdx) {}

  IntrinsicopNode(MapleAllocator *allocator, Opcode o, PrimType typ, TyIdx typeIdx = TyIdx())
      : NaryNode(allocator, o, typ), intrinsic(INTRN_UNDEFINED), tyIdx(typeIdx) {}

  IntrinsicopNode(const MIRModule *mod, Opcode o, PrimType typ, TyIdx typeIdx = TyIdx())
      : IntrinsicopNode(mod->CurFuncCodeMemPoolAllocator(), o, typ, typeIdx) {}

  IntrinsicopNode(MapleAllocator *allocator, const IntrinsicopNode *node)
      : NaryNode(allocator, node), intrinsic(node->intrinsic), tyIdx(node->tyIdx) {}

  IntrinsicopNode(const MIRModule *mod, const IntrinsicopNode *node)
      : IntrinsicopNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  IntrinsicopNode(IntrinsicopNode &node) = delete;
  IntrinsicopNode &operator=(const IntrinsicopNode &node) = delete;
  ~IntrinsicopNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  IntrinsicopNode *CloneTree(MapleAllocator *allocator) {
    IntrinsicopNode *nd = allocator->mp->New<IntrinsicopNode>(allocator, this);
    for (size_t i = 0; i < nOpnd.size(); i++) {
      nd->nOpnd.push_back(nOpnd[i]->CloneTree(allocator));
    }
    nd->numOpnds = nOpnd.size();
    return nd;
  }
  IntrinsicopNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<IntrinsicopNode>(allocator, this);
  }
  IntrinsicopNode *MakeCopy(MIRModule *mod) {
    return IntrinsicopNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }
  IntrinsicopNode *CloneTree(MIRModule *mod) {
    return IntrinsicopNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class ConstvalNode : public BaseNode {
 public:
  MIRConst *constVal;

 public:
  ConstvalNode() : BaseNode(OP_constval), constVal(nullptr) {}

  explicit ConstvalNode(PrimType typ) : BaseNode(OP_constval, typ, 0), constVal(nullptr) {}

  explicit ConstvalNode(MIRConst *constv) : BaseNode(OP_constval), constVal(constv) {}

  ConstvalNode(PrimType typ, MIRConst *constv) : BaseNode(OP_constval, typ, 0), constVal(constv) {}
  ~ConstvalNode() = default;
  void Dump(const MIRModule *mod, int32 indent) const;

  ConstvalNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<ConstvalNode>(*this);
  }

  ConstvalNode *MakeCopy(MIRModule *mod) {
    return ConstvalNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  ConstvalNode *CloneTree(MapleAllocator *allocator) {
    return allocator->mp->New<ConstvalNode>(*this);
  }

  ConstvalNode *CloneTree(MIRModule *mod) {
    return ConstvalNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class ConststrNode : public BaseNode {
 public:
  UStrIdx strIdx;

 public:
  ConststrNode() : BaseNode(OP_conststr), strIdx(0) {}

  explicit ConststrNode(UStrIdx i) : BaseNode(OP_conststr), strIdx(i) {}

  ConststrNode(PrimType typ, UStrIdx i) : BaseNode(OP_conststr, typ, 0), strIdx(i) {}

  ~ConststrNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  ConststrNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<ConststrNode>(*this);
  }

  ConststrNode *MakeCopy(MIRModule *mod) {
    return ConststrNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  ConststrNode *CloneTree(MapleAllocator *allocator) {
    return allocator->mp->New<ConststrNode>(*this);
  }

  ConststrNode *CloneTree(MIRModule *mod) {
    return ConststrNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class Conststr16Node : public BaseNode {
 public:
  U16StrIdx strIdx;

 public:
  Conststr16Node() : BaseNode(OP_conststr16), strIdx(0) {}

  explicit Conststr16Node(U16StrIdx i) : BaseNode(OP_conststr16), strIdx(i) {}

  Conststr16Node(PrimType typ, U16StrIdx i) : BaseNode(OP_conststr16, typ, 0), strIdx(i) {}

  ~Conststr16Node() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  Conststr16Node *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<Conststr16Node>(*this);
  }

  Conststr16Node *MakeCopy(MIRModule *mod) {
    return Conststr16Node::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  Conststr16Node *CloneTree(MapleAllocator *allocator) {
    return allocator->mp->New<Conststr16Node>(*this);
  }

  Conststr16Node *CloneTree(MIRModule *mod) {
    return Conststr16Node::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class SizeoftypeNode : public BaseNode {
 public:
  TyIdx tyIdx;

 public:
  SizeoftypeNode() : BaseNode(OP_sizeoftype), tyIdx(0) {}

  explicit SizeoftypeNode(TyIdx t) : BaseNode(OP_sizeoftype), tyIdx(t) {}

  SizeoftypeNode(PrimType type, TyIdx t) : BaseNode(OP_sizeoftype, type, 0), tyIdx(t) {}

  ~SizeoftypeNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  SizeoftypeNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<SizeoftypeNode>(*this);
  }

  SizeoftypeNode *MakeCopy(MIRModule *mod) {
    return SizeoftypeNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  SizeoftypeNode *CloneTree(MapleAllocator *allocator) {
    return allocator->mp->New<SizeoftypeNode>(*this);
  }

  SizeoftypeNode *CloneTree(MIRModule *mod) {
    return SizeoftypeNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class FieldsDistNode : public BaseNode {
 public:
  TyIdx tyIdx;
  FieldID fieldID1;
  FieldID fieldID2;

 public:
  FieldsDistNode() : BaseNode(OP_fieldsdist), tyIdx(0), fieldID1(0), fieldID2(0) {}

  FieldsDistNode(TyIdx t, FieldID f1, FieldID f2) : BaseNode(OP_fieldsdist), tyIdx(t), fieldID1(f1), fieldID2(f2) {}

  FieldsDistNode(PrimType typ, TyIdx t, FieldID f1, FieldID f2)
      : BaseNode(OP_fieldsdist, typ, 0), tyIdx(t), fieldID1(f1), fieldID2(f2) {}

  ~FieldsDistNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  FieldsDistNode *CloneTree(MapleAllocator *allocator) {
    return allocator->mp->New<FieldsDistNode>(*this);
  }

  FieldsDistNode *CloneTree(MIRModule *mod) {
    return FieldsDistNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  TyIdx GetTyIdx() const {
    return tyIdx;
  }

  void SetTyIdx(TyIdx idx) {
    tyIdx = idx;
  }

  FieldID GetFiledID1() const {
    return fieldID1;
  }

  void SetFiledID1(FieldID id) {
    fieldID1 = id;
  }

  FieldID GetFiledID2() const {
    return fieldID2;
  }

  void SetFiledID2(FieldID id) {
    fieldID2 = id;
  }
};

class ArrayNode : public NaryNode {
 public:
  TyIdx tyIdx;
  bool boundsCheck;

 public:
  explicit ArrayNode(MapleAllocator *allocator, bool check = true) : NaryNode(allocator, OP_array), boundsCheck(check) {
    numOpnds = 0;
  }

  explicit ArrayNode(const MIRModule *mod) : ArrayNode(mod->CurFuncCodeMemPoolAllocator()) {}

  ArrayNode(MapleAllocator *allocator, PrimType typ, TyIdx idx, bool bcheck = true)
      : NaryNode(allocator, OP_array, typ), tyIdx(idx), boundsCheck(bcheck) {}

  ArrayNode(const MIRModule *mod, PrimType typ, TyIdx idx) : ArrayNode(mod->CurFuncCodeMemPoolAllocator(), typ, idx) {}

  ArrayNode(const MIRModule *mod, PrimType typ, TyIdx idx, bool bcheck)
      : ArrayNode(mod->CurFuncCodeMemPoolAllocator(), typ, idx, bcheck) {}

  ArrayNode(MapleAllocator *allocator, const ArrayNode *node)
      : NaryNode(allocator, node), tyIdx(node->tyIdx), boundsCheck(node->boundsCheck) {}

  ArrayNode(const MIRModule *mod, const ArrayNode *node) : ArrayNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  ArrayNode(ArrayNode &node) = delete;
  ArrayNode &operator=(const ArrayNode &node) = delete;
  ~ArrayNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  void Dump(const MIRModule *mod) const;
  bool Verify() const;
  bool IsSameBase(ArrayNode *);

  int32 NumOpnds(void) {
    ASSERT(numOpnds == nOpnd.size(), "ArrayNode has wrong numOpnds field");
    return nOpnd.size();
  }

  ArrayNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<ArrayNode>(allocator, this);
  }

  ArrayNode *MakeCopy(MIRModule *mod) {
    return ArrayNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  ArrayNode *CloneTree(MapleAllocator *allocator) {
    ArrayNode *nd = allocator->mp->New<ArrayNode>(allocator, this);
    for (uint32 i = 0; i < nOpnd.size(); i++) {
      nd->nOpnd.push_back(nOpnd[i]->CloneTree(allocator));
    }
    nd->boundsCheck = boundsCheck;
    nd->numOpnds = nOpnd.size();
    return nd;
  }

  ArrayNode *CloneTree(MIRModule *mod) {
    return ArrayNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  MIRType *GetArrayType(TypeTable *tt);

  BaseNode *GetIndex(int i) {
    return Opnd(i + 1);
  }

  BaseNode *GetDim(const MIRModule *mod, TypeTable *tt, int i);

  BaseNode *GetBase() {
    return Opnd(0);
  }
};

class AddrofNode : public BaseNode {
 public:
  StIdx stIdx;
  FieldID fieldID;
 public:
  explicit AddrofNode(Opcode o) : BaseNode(o), stIdx(), fieldID(0) {}

  AddrofNode(Opcode o, PrimType typ, StIdx sIdx, FieldID fid) : BaseNode(o, typ, 0), stIdx(sIdx), fieldID(fid) {}

  ~AddrofNode() = default;

  virtual void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;
  bool CheckNode(const MIRModule *mod);
  bool HasSymbol(MIRModule *mod, MIRSymbol *st);

  AddrofNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<AddrofNode>(*this);
  }

  AddrofNode *MakeCopy(MIRModule *mod) {
    return AddrofNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  AddrofNode *CloneTree(MapleAllocator *allocator) {
    return allocator->mp->New<AddrofNode>(*this);
  }

  AddrofNode *CloneTree(MIRModule *mod) {
    return AddrofNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// DreadNode has the same member fields and member methods as AddrofNode
using DreadNode = AddrofNode;

class RegreadNode : public BaseNode {
 public:
  PregIdx regIdx;       // 32bit, negative if special register
 public:
  explicit RegreadNode() : BaseNode(OP_regread), regIdx(0) {}

  RegreadNode(PregIdx pIdx) : BaseNode(OP_regread), regIdx(pIdx) {}

  ~RegreadNode() = default;

  virtual void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  RegreadNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<RegreadNode>(*this);
  }

  RegreadNode *MakeCopy(MIRModule *mod) {
    return RegreadNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  RegreadNode *CloneTree(MapleAllocator *allocator) {
    return allocator->mp->New<RegreadNode>(*this);
  }

  RegreadNode *CloneTree(MIRModule *mod) {
    return RegreadNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class AddroffuncNode : public BaseNode {
 public:
  PUIdx puIdx;
 public:
  AddroffuncNode() : BaseNode(OP_addroffunc), puIdx(0) {}

  AddroffuncNode(PrimType typ, PUIdx pIdx) : BaseNode(OP_addroffunc, typ, 0), puIdx(pIdx) {}

  ~AddroffuncNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  AddroffuncNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<AddroffuncNode>(*this);
  }

  AddroffuncNode *MakeCopy(MIRModule *mod) {
    return AddroffuncNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  AddroffuncNode *CloneTree(MapleAllocator *allocator) {
    return allocator->mp->New<AddroffuncNode>(*this);
  }

  AddroffuncNode *CloneTree(MIRModule *mod) {
    return AddroffuncNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class AddroflabelNode : public BaseNode {
 public:
  uint32 offset;
 public:
  AddroflabelNode() : BaseNode(OP_addroflabel), offset(0) {}

  explicit AddroflabelNode(uint32 ofst) : BaseNode(OP_addroflabel), offset(ofst) {}

  ~AddroflabelNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  AddroflabelNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<AddroflabelNode>(*this);
  }

  AddroflabelNode *MakeCopy(MIRModule *mod) {
    return AddroflabelNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  AddroflabelNode *CloneTree(MapleAllocator *allocator) {
    return allocator->mp->New<AddroflabelNode>(*this);
  }

  AddroflabelNode *CloneTree(MIRModule *mod) {
    return AddroflabelNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// to store source position information
class SrcPosition {
 private:
  union {
    struct {
      uint16 fileNum;
      uint16 column : 12;
      uint16 stmtBegin : 1;
      uint16 bbBegin : 1;
      uint16 unused : 2;
    } fileColumn;
    uint32 word0;
  } u;
  uint32 lineNum;     // line number of original src file, like foo.java
  uint32 mplLineNum;  // line number of mpl file
 public:
  SrcPosition() : lineNum(0), mplLineNum(0) {
    u.word0 = 0;
  }

  virtual ~SrcPosition() = default;

  uint32 RawData() const {
    return u.word0;
  }

  uint32 Filenum() const {
    return u.fileColumn.fileNum;
  }

  uint32 Column() const {
    return u.fileColumn.column;
  }

  uint32 Linenum() const {
    return lineNum;
  }

  uint32 MplLinenum() const {
    return mplLineNum;
  }

  void SetFilenum(int n) {
    u.fileColumn.fileNum = n;
  }

  void SetColumn(int n) {
    u.fileColumn.column = n;
  }

  void SetLinenum(int n) {
    lineNum = n;
  }

  void SetRawData(uint32 n) {
    u.word0 = n;
  }

  void SetMplLinenum(int n) {
    mplLineNum = n;
  }

  void CondSetLinenum(int n) {
    lineNum = lineNum ? lineNum : n;
  }

  void CondSetFilenum(int n) {
    uint32 i = u.fileColumn.fileNum;
    u.fileColumn.fileNum = i ? i : n;
  }
};

// for cleanuptry, jscatch, finally, retsub, endtry, membaracquire, membarrelease,
// membarstoreload, membarstorestore
class StmtNode : public BaseNode, public PtrListNodeBase<StmtNode> {
 public:
  static uint32 stmtIDNext;          // for assigning stmtID, initialized to 1; 0 is reserved
  static uint32 lastPrintedLinenum;  // used during printing ascii output
  SrcPosition srcPosition;
  uint32 stmtID;                     // a unique ID assigned to it

 public:
  explicit StmtNode(Opcode o) : BaseNode(o), PtrListNodeBase(), stmtID(stmtIDNext) {
    stmtIDNext++;
  }

  StmtNode(Opcode o, uint8 numopr) : BaseNode(o, numopr), PtrListNodeBase(), stmtID(stmtIDNext) {
    stmtIDNext++;
  }

  StmtNode(Opcode o, PrimType typ, uint8 numopr) : BaseNode(o, typ, numopr), PtrListNodeBase(), stmtID(stmtIDNext) {
    stmtIDNext++;
  }

  virtual ~StmtNode() = default;

  void DumpBase(const MIRModule *mod, int32 indent) const;
  void Dump(const MIRModule *mod, int32 indent) const;
  void Dump(const MIRModule *mod) const;
  void InsertBefore(StmtNode *pos);
  void InsertAfter(StmtNode *pos);

  virtual StmtNode *MakeCopy(MapleAllocator *allocator) {
    StmtNode *s = allocator->mp->New<StmtNode>(*this);
    s->stmtID = stmtIDNext++;
    return s;
  }

  virtual StmtNode *MakeCopy(MIRModule *mod) {
    return MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  virtual StmtNode *CloneTree(MapleAllocator *allocator) {
    StmtNode *s = allocator->mp->New<StmtNode>(*this);
    s->stmtID = stmtIDNext++;
    return s;
  }

  virtual bool Verify() const {
    return true;
  }

  virtual StmtNode *CloneTree(MIRModule *mod) {
    return CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  StmtNode *GetRealNext();
};

class IassignNode : public StmtNode {
 public:
  TyIdx tyIdx;
  FieldID fieldID;
  BaseNode *addrExpr;
  BaseNode *rhs;

 public:
  IassignNode() : StmtNode(OP_iassign), tyIdx(0), fieldID(0), addrExpr(nullptr), rhs(nullptr) {
    numOpnds = 2;
  }

  ~IassignNode() = default;

  BaseNode *Opnd(size_t i) const {
    if (i == 0) {
      return addrExpr;
    }
    return rhs;
  }

  int32 NumOpnds(void) {
    return 2;
  }

  void SetOpnd(BaseNode *node, size_t i) {
    if (i == 0) {
      addrExpr = node;
    } else {
      rhs = node;
    }
  }

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  IassignNode *MakeCopy(MapleAllocator *allocator) {
    return allocator->mp->New<IassignNode>(*this);
  }

  IassignNode *MakeCopy(MIRModule *mod) {
    return IassignNode::MakeCopy(mod->CurFuncCodeMemPoolAllocator());
  }

  IassignNode *CloneTree(MapleAllocator *allocator) {
    IassignNode *bn = allocator->mp->New<IassignNode>(*this);
    bn->stmtID = stmtIDNext++;
    bn->addrExpr = addrExpr->CloneTree(allocator);
    bn->rhs = rhs->CloneTree(allocator);
    return bn;
  }

  IassignNode *CloneTree(MIRModule *mod) {
    return IassignNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  // the base of an address expr is either a leaf or an iread
  BaseNode *GetAddrExprBase() const {
    BaseNode *base = addrExpr;
    while (base->NumOpnds() != 0 && base->op != OP_iread) {
      base = base->Opnd(0);
    }
    return base;
  }
};

// goto and gosub
class GotoNode : public StmtNode {
 public:
  uint32 offset;

 public:
  explicit GotoNode(Opcode o) : StmtNode(o), offset(0) {}

  ~GotoNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  GotoNode *CloneTree(MapleAllocator *allocator) {
    GotoNode *g = allocator->mp->New<GotoNode>(*this);
    g->stmtID = stmtIDNext++;
    return g;
  }

  GotoNode *CloneTree(MIRModule *mod) {
    return GotoNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// jstry
class JsTryNode : public StmtNode {
 public:
  uint16 catchOffset;
  uint16 finallyOffset;

 public:
  JsTryNode() : StmtNode(OP_jstry), catchOffset(0), finallyOffset(0) {}

  JsTryNode(uint16 catchofst, uint16 finallyofset)
      : StmtNode(OP_jstry), catchOffset(catchofst), finallyOffset(finallyofset) {}

  ~JsTryNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  JsTryNode *CloneTree(MapleAllocator *allocator) {
    JsTryNode *t = allocator->mp->New<JsTryNode>(*this);
    t->stmtID = stmtIDNext++;
    return t;
  }

  JsTryNode *CloneTree(MIRModule *mod) {
    return JsTryNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// try, javatry, cpptry
class TryNode : public StmtNode {
 public:
  MapleVector<LabelIdx> offsets;

 public:
  explicit TryNode(MapleAllocator *allocator, Opcode o) : StmtNode(o), offsets(allocator->Adapter()) {}
  explicit TryNode(const MIRModule *mod, Opcode o) : TryNode(mod->CurFuncCodeMemPoolAllocator(), o) {}

  TryNode(TryNode &node) = delete;
  TryNode &operator=(const TryNode &node) = delete;
  ~TryNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  void Dump(const MIRModule *mod) const;

  TryNode *CloneTree(MapleAllocator *allocator) {
    TryNode *nd = allocator->mp->New<TryNode>(allocator, op);
    nd->stmtID = stmtIDNext++;
    for (uint32 i = 0; i < offsets.size(); i++) {
      nd->offsets.push_back(offsets[i]);
    }
    return nd;
  }

  TryNode *CloneTree(MIRModule *mod) {
    return CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// cppcatch
class CppCatchNode : public StmtNode {
 public:
  TyIdx exceptionTyIdx;

 public:
  explicit CppCatchNode(TyIdx idx) : StmtNode(OP_cppcatch), exceptionTyIdx(idx) {}
  explicit CppCatchNode() : CppCatchNode(TyIdx(0)) {}

  CppCatchNode(CppCatchNode &node) = delete;
  CppCatchNode &operator=(const CppCatchNode &node) = delete;
  ~CppCatchNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  CppCatchNode *CloneTree(MapleAllocator *allocator) {
    CppCatchNode *j = allocator->mp->New<CppCatchNode>();
    j->stmtID = stmtIDNext++;
    j->exceptionTyIdx = exceptionTyIdx;
    return j;
  }

  CppCatchNode *CloneTree(MIRModule *mod) {
    return CppCatchNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// catch, javacatch
class CatchNode : public StmtNode {
 public:
  MapleVector<TyIdx> exceptionTyIdxVec;

 public:
  explicit CatchNode(MapleAllocator *allocator) : StmtNode(OP_catch), exceptionTyIdxVec(allocator->Adapter()) {}

  explicit CatchNode(const MIRModule *mod) : CatchNode(mod->CurFuncCodeMemPoolAllocator()) {}

  CatchNode(CatchNode &node) = delete;
  CatchNode &operator=(const CatchNode &node) = delete;
  ~CatchNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  CatchNode *CloneTree(MapleAllocator *allocator) {
    CatchNode *j = allocator->mp->New<CatchNode>(allocator);
    j->stmtID = stmtIDNext++;
    for (uint32 i = 0; i < exceptionTyIdxVec.size(); i++) {
      j->exceptionTyIdxVec.push_back(exceptionTyIdxVec[i]);
    }
    return j;
  }

  CatchNode *CloneTree(MIRModule *mod) {
    return CatchNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

using CasePair = std::pair<int32, LabelIdx>;
using CaseVector = MapleVector<CasePair>;

class SwitchNode : public StmtNode {
 public:
  BaseNode *switchOpnd;
  LabelIdx defaultLabel;
  CaseVector switchTable;

 public:
  explicit SwitchNode(MapleAllocator *allocator)
    : StmtNode(OP_switch, 1), switchOpnd(nullptr), defaultLabel(0), switchTable(allocator->Adapter()) {}

  explicit SwitchNode(const MIRModule *mod) : SwitchNode(mod->CurFuncCodeMemPoolAllocator()) {}

  SwitchNode(MapleAllocator *allocator, const SwitchNode *node)
    : StmtNode(node->op, node->primType, node->numOpnds),
      switchOpnd(nullptr),
      defaultLabel(node->defaultLabel),
      switchTable(allocator->Adapter()) {}

  SwitchNode(const MIRModule *mod, const SwitchNode *node) : SwitchNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  SwitchNode(SwitchNode &node) = delete;
  SwitchNode &operator=(const SwitchNode &node) = delete;
  ~SwitchNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  SwitchNode *CloneTree(MapleAllocator *allocator) {
    SwitchNode *nd = allocator->mp->New<SwitchNode>(allocator, this);
    nd->switchOpnd = switchOpnd->CloneTree(allocator);
    for (uint32 i = 0; i < switchTable.size(); i++) {
      nd->switchTable.push_back(switchTable[i]);
    }
    return nd;
  }

  SwitchNode *CloneTree(MIRModule *mod) {
    return SwitchNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  BaseNode *Opnd(size_t i) const {
    ASSERT(i == 0, "it is not same as original");
    return switchOpnd;
  }

  void SetOpnd(BaseNode *node, size_t i = 0) {
    ASSERT(i == 0, "it is not same as original");
    switchOpnd = node;
  }
};

using MCasePair = std::pair<BaseNode *, LabelIdx>;
using MCaseVector = MapleVector<MCasePair>;

class MultiwayNode : public StmtNode {
 public:
  BaseNode *multiWayOpnd;
  LabelIdx defaultLabel;
  MCaseVector multiWayTable;

 public:
  explicit MultiwayNode(MapleAllocator *allocator)
    : StmtNode(OP_multiway, 1), multiWayOpnd(nullptr), defaultLabel(0), multiWayTable(allocator->Adapter()) {}

  explicit MultiwayNode(const MIRModule *mod) : MultiwayNode(mod->CurFuncCodeMemPoolAllocator()) {}
  BaseNode *Opnd(size_t i) const {
    return *(&multiWayOpnd + i);
  }

  MultiwayNode(MapleAllocator *allocator, const MultiwayNode *node)
    : StmtNode(node->op, node->primType, node->numOpnds),
      multiWayOpnd(nullptr),
      defaultLabel(node->defaultLabel),
      multiWayTable(allocator->Adapter()) {}

  MultiwayNode(const MIRModule *mod, const MultiwayNode *node) : MultiwayNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  MultiwayNode(MultiwayNode &node) = delete;
  MultiwayNode &operator=(const MultiwayNode &node) = delete;
  ~MultiwayNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  MultiwayNode *CloneTree(MapleAllocator *allocator) {
    MultiwayNode *nd = allocator->mp->New<MultiwayNode>(allocator, this);
    nd->multiWayOpnd = static_cast<BaseNode *>(multiWayOpnd->CloneTree(allocator));
    for (size_t i = 0; i < multiWayTable.size(); i++) {
      BaseNode *node = multiWayTable[i].first->CloneTree(allocator);
      MCasePair pair(static_cast<BaseNode *>(node), multiWayTable[i].second);
      nd->multiWayTable.push_back(pair);
    }
    return nd;
  }

  MultiwayNode *CloneTree(MIRModule *mod) {
    return MultiwayNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// eval, throw, free, decref, incref, decrefreset, assertnonnull
class UnaryStmtNode : public StmtNode {
 public:
  BaseNode *uOpnd;
 public:
  explicit UnaryStmtNode(Opcode o) : StmtNode(o, 1), uOpnd(nullptr) {}

  UnaryStmtNode(Opcode o, PrimType typ) : StmtNode(o, typ, 1), uOpnd(nullptr) {}

  UnaryStmtNode(Opcode o, PrimType typ, BaseNode *opnd) : StmtNode(o, typ, 1), uOpnd(opnd) {}

  virtual ~UnaryStmtNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  void Dump(const MIRModule *mod) const;
  void DumpOpnd(const MIRModule *mod, int32 indent) const;

  bool Verify() const {
    return uOpnd->Verify();
  }

  UnaryStmtNode *CloneTree(MapleAllocator *allocator) {
    UnaryStmtNode *nd = allocator->mp->New<UnaryStmtNode>(*this);
    nd->stmtID = stmtIDNext++;
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  UnaryStmtNode *CloneTree(MIRModule *mod) {
    return UnaryStmtNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  bool IsLeaf(void) {
    return false;
  }

  BaseNode *GetRhs() const {
    return Opnd(0);
  }

  void SetRhs(BaseNode *rhs) {
    this->SetOpnd(rhs, 0);
  }

  BaseNode *Opnd(size_t i) const {
    ASSERT(i == 0, "Unary operand");
    return uOpnd;
  }

  void SetOpnd(BaseNode *node, size_t i) {
    ASSERT(i == 0, "Unary operand");
    uOpnd = node;
  }
};

// dassign, maydassign
class DassignNode : public UnaryStmtNode {
 public:
  StIdx stIdx;
  FieldID fieldID;

 public:
  DassignNode() : UnaryStmtNode(OP_dassign), stIdx(), fieldID(0) {}

  explicit DassignNode(PrimType typ) : UnaryStmtNode(OP_dassign, typ), stIdx(), fieldID(0) {}

  DassignNode(PrimType typ, BaseNode *opnd) : UnaryStmtNode(OP_dassign, typ, opnd), stIdx(), fieldID(0) {}

  DassignNode(PrimType typ, BaseNode *opnd, StIdx idx, FieldID fieldID)
      : UnaryStmtNode(OP_dassign, typ, opnd), stIdx(idx), fieldID(fieldID) {}

  ~DassignNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  DassignNode *CloneTree(MIRModule *mod) {
    return CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  DassignNode *CloneTree(MapleAllocator *allocator) {
    DassignNode *nd = allocator->mp->New<DassignNode>(*this);
    nd->stmtID = stmtIDNext++;
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  int32 NumOpnds(void) {
    return 1;
  }

  bool IsIdentityDassign() const {
    BaseNode *rhs = GetRhs();
    if (rhs->op != OP_dread) {
      return false;
    }
    AddrofNode *dread = static_cast<AddrofNode *>(rhs);
    return (stIdx == dread->stIdx);
  }

  BaseNode *GetRhs() const {
    return UnaryStmtNode::GetRhs();
  }

  void SetRhs(BaseNode *rhs) {
    UnaryStmtNode::SetOpnd(rhs, 0);
  }
};

class RegassignNode : public UnaryStmtNode {
 public:
  PregIdx regIdx;  // 32bit, negative if special register
 public:
  RegassignNode() : UnaryStmtNode(OP_regassign), regIdx(0) {}

  explicit RegassignNode(RegassignNode *node) : UnaryStmtNode(*node), regIdx(node->regIdx) {}

  ~RegassignNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  RegassignNode *CloneTree(MapleAllocator *allocator) {
    RegassignNode *nd = allocator->mp->New<RegassignNode>(this);
    nd->stmtID = stmtIDNext++;
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  RegassignNode *CloneTree(MIRModule *mod) {
    return RegassignNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  BaseNode *GetRhs() const {
    return UnaryStmtNode::GetRhs();
  }

  void SetRhs(BaseNode *rhs) {
    UnaryStmtNode::SetOpnd(rhs, 0);
  }
};

// brtrue and brfalse
class CondGotoNode : public UnaryStmtNode {
 public:
  uint32 offset;

 public:
  explicit CondGotoNode(Opcode o) : UnaryStmtNode(o), offset(0) {}

  ~CondGotoNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  CondGotoNode *CloneTree(MapleAllocator *allocator) {
    CondGotoNode *nd = allocator->mp->New<CondGotoNode>(*this);
    nd->stmtID = stmtIDNext++;
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  CondGotoNode *CloneTree(MIRModule *mod) {
    return CondGotoNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

using SmallCasePair = std::pair<uint16, uint16>;
using SmallCaseVector = MapleVector<SmallCasePair>;

class RangegotoNode : public UnaryStmtNode {
 public:
  int32 tagOffset;
  // add each tag to tagOffset field to get the actual tag values
  SmallCaseVector rangegotoTable;

 public:
  explicit RangegotoNode(MapleAllocator *allocator)
    : UnaryStmtNode(OP_rangegoto), tagOffset(0), rangegotoTable(allocator->Adapter()) {}

  explicit RangegotoNode(const MIRModule *mod) : RangegotoNode(mod->CurFuncCodeMemPoolAllocator()) {}

  RangegotoNode(MapleAllocator *allocator, const RangegotoNode *node)
      : UnaryStmtNode(node->op, node->primType),
        tagOffset(node->tagOffset),
        rangegotoTable(allocator->Adapter()) {}

  RangegotoNode(const MIRModule *mod, const RangegotoNode *node)
      : RangegotoNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  RangegotoNode(RangegotoNode &node) = delete;
  RangegotoNode &operator=(const RangegotoNode &node) = delete;
  ~RangegotoNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  RangegotoNode *CloneTree(MapleAllocator *allocator) {
    RangegotoNode *nd = allocator->mp->New<RangegotoNode>(allocator, this);
    nd->uOpnd = uOpnd->CloneTree(allocator);
    for (size_t i = 0; i < rangegotoTable.size(); i++) {
      nd->rangegotoTable.push_back(rangegotoTable[i]);
    }
    return nd;
  }

  RangegotoNode *CloneTree(MIRModule *mod) {
    return RangegotoNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class BlockNode : public StmtNode {
 public:
  using StmtNodes = PtrListRef<StmtNode>;

  BlockNode() : StmtNode(OP_block) {}

  ~BlockNode() {
    stmtNodeList.clear();
  }

  void AddStatement(StmtNode *stmt);
  void AppendStatementsFromBlock(BlockNode *blk);
  void InsertFirst(StmtNode *stmt);  // Insert stmt as the first
  void InsertLast(StmtNode *stmt);   // Insert stmt as the last
  void ReplaceStmtWithBlock(StmtNode *ss, BlockNode *blk);
  void ReplaceStmt1WithStmt2(StmtNode *ss1, StmtNode *ss2);
  void RemoveStmt(StmtNode *ss2);
  void InsertBefore(StmtNode *ss1, StmtNode *ss2);  // Insert ss2 before ss1 in current block.
  void InsertAfter(StmtNode *ss1, StmtNode *ss2);   // Insert ss2 after ss1 in current block.
  void InsertBlockAfter(BlockNode *inblock,
                        StmtNode *stmt1);  // insert all the stmts in inblock to the current block after stmt1

  void Dump(const MIRModule *mod, int32 indent, const MIRSymbolTable *thesymtab,
             MIRPregTable *thepregtab, bool withInfo, bool isFuncbody) const;
  void Dump(const MIRModule *mod, int32 indent) const {
    Dump(mod, indent, nullptr, nullptr, false, false);
  }
  bool Verify() const;

  BlockNode *CloneTree(MapleAllocator *allocator) {
    BlockNode *blk = allocator->mp->New<BlockNode>();
    blk->stmtID = stmtIDNext++;
    for (StmtNode &stmt : stmtNodeList) {
      StmtNode *newStmt = static_cast<StmtNode*>(stmt.CloneTree(allocator));
      ASSERT(newStmt != nullptr, "null ptr check");
      newStmt->SetPrev(nullptr);
      newStmt->SetNext(nullptr);
      blk->AddStatement(newStmt);
    }
    return blk;
  }

  BlockNode *CloneTree(MIRModule *mod) {
    return BlockNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  bool IsEmpty() const {
    return stmtNodeList.empty();
  }

  void ResetBlock() {
    stmtNodeList.clear();
  }

  StmtNode *GetFirst() {
    return &(stmtNodeList.front());
  }

  const StmtNode *GetFirst() const {
    return &(stmtNodeList.front());
  }

  void SetFirst(StmtNode *node) {
    stmtNodeList.update_front(node);
  }

  StmtNode *GetLast() {
    return &(stmtNodeList.back());
  }

  const StmtNode *GetLast() const {
    return &(stmtNodeList.back());
  }

  void SetLast(StmtNode *node) {
    stmtNodeList.update_back(node);
  }

  StmtNodes &GetStmtNodes() {
    return stmtNodeList;
  }

  const StmtNodes &GetStmtNodes() const {
    return stmtNodeList;
  }

 private:
  StmtNodes stmtNodeList;
};

class IfStmtNode : public UnaryStmtNode {
 public:
  BlockNode *thenPart;
  BlockNode *elsePart;

 public:
  IfStmtNode() : UnaryStmtNode(OP_if), thenPart(nullptr), elsePart(nullptr) {
    numOpnds = 2;
  }

  ~IfStmtNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  IfStmtNode *CloneTree(MapleAllocator *allocator) {
    IfStmtNode *nd = allocator->mp->New<IfStmtNode>(*this);
    nd->stmtID = stmtIDNext++;
    nd->uOpnd = uOpnd->CloneTree(allocator);
    nd->thenPart = thenPart->CloneTree(allocator);
    if (elsePart) {
      nd->elsePart = elsePart->CloneTree(allocator);
    }
    return nd;
  }

  IfStmtNode *CloneTree(MIRModule *mod) {
    return IfStmtNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  BaseNode *Opnd(size_t i) const {
    if (i == 0) {
      return uOpnd;
    } else if (i == 1) {
      return thenPart;
    } else if (i == 2) {
      ASSERT(elsePart != nullptr && numOpnds == 3, "IfStmtNode has wrong numOpnds field, the elsePart is nullptr");
      return elsePart;
    }
    ASSERT(false, "IfStmtNode has wrong numOpnds field: %u", numOpnds);
    return nullptr;
  }

  int32 NumOpnds(void) {
    return numOpnds;
  }
};

// for both while and dowhile
class WhileStmtNode : public UnaryStmtNode {
 public:
  BlockNode *body;

 public:
  explicit WhileStmtNode(Opcode o) : UnaryStmtNode(o), body(nullptr) {
    numOpnds = 2;
  }

  ~WhileStmtNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  WhileStmtNode *CloneTree(MapleAllocator *allocator) {
    WhileStmtNode *nd = allocator->mp->New<WhileStmtNode>(*this);
    nd->stmtID = stmtIDNext++;
    nd->uOpnd = uOpnd->CloneTree(allocator);
    nd->body = body->CloneTree(allocator);
    return nd;
  }

  WhileStmtNode *CloneTree(MIRModule *mod) {
    return WhileStmtNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class DoloopNode : public StmtNode {
 public:
  StIdx doVarStIdx;  // must be local; cast to PregIdx for preg
  bool isPreg;
  BaseNode *startExpr;
  BaseNode *condExpr;
  BaseNode *incrExpr;
  BlockNode *doBody;

 public:
  DoloopNode()
      : StmtNode(OP_doloop, 4),
        doVarStIdx(),
        isPreg(false),
        startExpr(nullptr),
        condExpr(nullptr),
        incrExpr(nullptr),
        doBody(nullptr) {}

  ~DoloopNode() = default;

  void DumpDovar(const MIRModule *mod) const;
  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  DoloopNode *CloneTree(MapleAllocator *allocator) {
    DoloopNode *nd = allocator->mp->New<DoloopNode>(*this);
    nd->stmtID = stmtIDNext++;
    nd->startExpr = startExpr->CloneTree(allocator);
    nd->condExpr = condExpr->CloneTree(allocator);
    nd->incrExpr = incrExpr->CloneTree(allocator);
    nd->doBody = doBody->CloneTree(allocator);
    return nd;
  }

  DoloopNode *CloneTree(MIRModule *mod) {
    return DoloopNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  BaseNode *Opnd(size_t i) const {
    if (i == 0) {
      return startExpr;
    }
    if (i == 1) {
      return condExpr;
    }
    if (i == 2) {
      return incrExpr;
    }
    return *(&doBody + i - 3);
  }

  int32 NumOpnds(void) {
    return 4;
  }

  void SetOpnd(BaseNode *node, size_t i) {
    if (i == 0) {
      startExpr = node;
    }
    if (i == 1) {
      condExpr = node;
    }
    if (i == 2) {
      incrExpr = node;
    } else {
      *(&doBody + i - 3) = static_cast<BlockNode *>(node);
    }
  }
};

class ForeachelemNode : public StmtNode {
 public:
  StIdx elemStIdx;   // must be local symbol
  StIdx arrayStIdx;  // symbol table entry of the array/collection variable
  BlockNode *loopBody;

 public:
  ForeachelemNode() : StmtNode(OP_foreachelem), loopBody(nullptr) {
    numOpnds = 1;
  }

  ~ForeachelemNode() = default;

  BaseNode *Opnd(size_t i) const {
    return loopBody;
  }

  int32 NumOpnds(void) {
    return numOpnds;
  }

  void Dump(const MIRModule *mod, int32 indent) const;

  ForeachelemNode *CloneTree(MapleAllocator *allocator) {
    ForeachelemNode *nd = allocator->mp->New<ForeachelemNode>(*this);
    nd->stmtID = stmtIDNext++;
    nd->loopBody = loopBody->CloneTree(allocator);
    return nd;
  }

  ForeachelemNode *CloneTree(MIRModule *mod) {
    return ForeachelemNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// used by assertge, assertlt
class BinaryStmtNode : public StmtNode, public BinaryOpnds {
 public:
  explicit BinaryStmtNode(Opcode o) : StmtNode(o, 2) {}

  ~BinaryStmtNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  virtual bool Verify() const;
  BinaryStmtNode *CloneTree(MapleAllocator *allocator) {
    BinaryStmtNode *nd = allocator->mp->New<BinaryStmtNode>(*this);
    nd->stmtID = stmtIDNext++;
    nd->bOpnd[0] = bOpnd[0]->CloneTree(allocator);
    nd->bOpnd[1] = bOpnd[1]->CloneTree(allocator);
    return nd;
  }

  BinaryStmtNode *CloneTree(MIRModule *mod) {
    return BinaryStmtNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
  BaseNode *Opnd(size_t i) const {
    ASSERT(i < 2, "Invalid operand idx in BinaryStmtNode");
    return bOpnd[i];
  }

  int32 NumOpnds(void) {
    return 2;
  }

  void SetOpnd(BaseNode *node, size_t i) {
    bOpnd[i] = node;
  }

  bool IsLeaf(void) {
    return false;
  }
};

class IassignoffNode : public BinaryStmtNode {
 public:
  int32 offset;

 public:
  IassignoffNode() : BinaryStmtNode(OP_iassignoff), offset(0) {}

  explicit IassignoffNode(int32 ofst) : BinaryStmtNode(OP_iassignoff), offset(ofst) {}

  ~IassignoffNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  IassignoffNode *CloneTree(MapleAllocator *allocator) {
    IassignoffNode *nd = allocator->mp->New<IassignoffNode>(*this);
    nd->stmtID = stmtIDNext++;
    nd->bOpnd[0] = bOpnd[0]->CloneTree(allocator);
    nd->bOpnd[1] = bOpnd[1]->CloneTree(allocator);
    return nd;
  }

  IassignoffNode *CloneTree(MIRModule *mod) {
    return IassignoffNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class IassignFPoffNode : public UnaryStmtNode {
 public:
  int32 offset;

 public:
  IassignFPoffNode() : UnaryStmtNode(OP_iassignfpoff), offset(0) {}

  explicit IassignFPoffNode(int32 ofst) : UnaryStmtNode(OP_iassignfpoff), offset(ofst) {}

  ~IassignFPoffNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
  bool Verify() const;

  IassignFPoffNode *CloneTree(MapleAllocator *allocator) {
    IassignFPoffNode *nd = allocator->mp->New<IassignFPoffNode>(*this);
    nd->stmtID = stmtIDNext++;
    nd->uOpnd = uOpnd->CloneTree(allocator);
    return nd;
  }

  IassignFPoffNode *CloneTree(MIRModule *mod) {
    return IassignFPoffNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

typedef IassignFPoffNode IassignPCoffNode;

// used by return, syncenter, syncexit
class NaryStmtNode : public StmtNode, public NaryOpnds {
 public:
  NaryStmtNode(MapleAllocator *allocator, Opcode o) : StmtNode(o), NaryOpnds(allocator) {}

  NaryStmtNode(const MIRModule *mod, Opcode o) : NaryStmtNode(mod->CurFuncCodeMemPoolAllocator(), o) {}

  NaryStmtNode(MapleAllocator *allocator, const NaryStmtNode *node)
      : StmtNode(node->op, node->primType, node->numOpnds), NaryOpnds(allocator) {}

  NaryStmtNode(const MIRModule *mod, const NaryStmtNode *node)
      : NaryStmtNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  NaryStmtNode(NaryStmtNode &node) = delete;
  NaryStmtNode &operator=(const NaryStmtNode &node) = delete;
  ~NaryStmtNode() {}

  void Dump(const MIRModule *mod, int32 indent) const;
  virtual bool Verify() const;

  NaryStmtNode *CloneTree(MapleAllocator *allocator) {
    NaryStmtNode *nd = allocator->mp->New<NaryStmtNode>(allocator, this);
    for (size_t i = 0; i < nOpnd.size(); i++) {
      nd->nOpnd.push_back(nOpnd[i]->CloneTree(allocator));
    }
    nd->numOpnds = nOpnd.size();
    return nd;
  }

  NaryStmtNode *CloneTree(MIRModule *mod) {
    return NaryStmtNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }

  BaseNode *Opnd(size_t i) const {
    return nOpnd.at(i);
  }

  void SetOpnd(BaseNode *node, size_t i) {
    nOpnd.at(i) = node;
  }

  int32 NumOpnds(void) {
    ASSERT(numOpnds == nOpnd.size(), "NaryStmtNode has wrong numOpnds field");
    return nOpnd.size();
  }
};

// used by call, virtualcall, virtualicall, superclasscall, interfacecall,
// interfaceicall, customcall
// polymorphiccall
// callassigned, virtualcallassigned, virtualicallassigned,
// superclasscallassigned, interfacecallassigned, interfaceicallassigned,
// customcallassigned
// polymorphiccallassigned
class CallNode : public NaryStmtNode {
 public:
  PUIdx puIdx;
  TyIdx tyIdx;
  CallReturnVector returnValues;

 public:
  CallNode(MapleAllocator *allocator, Opcode o)
      : NaryStmtNode(allocator, o), puIdx(0), tyIdx(0), returnValues(allocator->Adapter()) {}

  CallNode(MapleAllocator *allocator, Opcode o, PUIdx idx, TyIdx tdx)
      : NaryStmtNode(allocator, o), puIdx(idx), tyIdx(tdx), returnValues(allocator->Adapter()) {}

  CallNode(const MIRModule *mod, Opcode o) : CallNode(mod->CurFuncCodeMemPoolAllocator(), o) {}

  CallNode(const MIRModule *mod, Opcode o, PUIdx idx, TyIdx tdx)
      : CallNode(mod->CurFuncCodeMemPoolAllocator(), o, idx, tdx) {}

  CallNode(MapleAllocator *allocator, const CallNode *node)
      : NaryStmtNode(allocator, node),
        puIdx(node->puIdx),
        tyIdx(node->tyIdx),
        returnValues(allocator->Adapter()) {}

  CallNode(const MIRModule *mod, const CallNode *node) : CallNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  CallNode(CallNode &node) = delete;
  CallNode &operator=(const CallNode &node) = delete;
  ~CallNode() = default;
  void Dump(const MIRModule *mod, int32 indent, bool newline) const;
  void Dump(const MIRModule *mod, int32 indent) const {
    Dump(mod, indent, true);
  }
  bool Verify() const;

  CallNode *CloneTree(MapleAllocator *allocator) {
    CallNode *nd = allocator->mp->New<CallNode>(allocator, this);
    for (size_t i = 0; i < nOpnd.size(); i++) {
      nd->nOpnd.push_back(nOpnd[i]->CloneTree(allocator));
    }
    for (size_t i = 0; i < returnValues.size(); i++) {
      nd->returnValues.push_back(returnValues[i]);
    }
    nd->numOpnds = nOpnd.size();
    return nd;
  }
  CallNode *CloneTree(MIRModule *mod) {
    return CallNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
  int32 NumOpnds(void) {
    ASSERT(numOpnds == nOpnd.size(), "CallNode has wrong numOpnds field");
    return nOpnd.size();
  }
  CallReturnVector *GetCallReturnVector() {
    if (kOpcodeInfo.IsCallAssigned(op)) {
      return &returnValues;
    }
    return nullptr;
  }
  MIRType *GetCallReturnType();
};

class IcallNode : public NaryStmtNode {
 public:
  TyIdx retTyIdx;  // return type for callee
  CallReturnVector returnValues;
  // the 0th operand is the function pointer
 public:
  IcallNode(MapleAllocator *allocator, Opcode o)
      : NaryStmtNode(allocator, o), retTyIdx(0), returnValues(allocator->Adapter()) {
    numOpnds = 1;
  }

  IcallNode(MapleAllocator *allocator, Opcode o, TyIdx idx)
      : NaryStmtNode(allocator, o), retTyIdx(idx), returnValues(allocator->Adapter()) {
    numOpnds = 1;
  }

  IcallNode(const MIRModule *mod, Opcode o) : IcallNode(mod->CurFuncCodeMemPoolAllocator(), o) {}

  IcallNode(const MIRModule *mod, Opcode o, TyIdx idx) : IcallNode(mod->CurFuncCodeMemPoolAllocator(), o, idx) {}

  IcallNode(MapleAllocator *allocator, const IcallNode *node)
      : NaryStmtNode(allocator, node), retTyIdx(node->retTyIdx), returnValues(allocator->Adapter()) {}

  IcallNode(const MIRModule *mod, const IcallNode *node) : IcallNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  IcallNode(IcallNode &node) = delete;
  IcallNode &operator=(const IcallNode &node) = delete;
  ~IcallNode() = default;
  void Dump(const MIRModule *mod, int32 indent, bool newline) const;
  void Dump(const MIRModule *mod, int32 indent) const {
    Dump(mod, indent, true);
  }
  bool Verify() const;

  IcallNode *CloneTree(MapleAllocator *allocator) {
    IcallNode *nd = allocator->mp->New<IcallNode>(allocator, this);
    for (size_t i = 0; i < nOpnd.size(); i++) {
      nd->nOpnd.push_back(nOpnd[i]->CloneTree(allocator));
    }
    for (size_t i = 0; i < returnValues.size(); i++) {
      nd->returnValues.push_back(returnValues[i]);
    }
    nd->numOpnds = nOpnd.size();
    return nd;
  }
  IcallNode *CloneTree(MIRModule *mod) {
    return IcallNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
  int32 NumOpnds(void) {
    ASSERT(numOpnds == nOpnd.size(), "IcallNode has wrong numOpnds field");
    return nOpnd.size();
  }
  CallReturnVector *GetCallReturnVector() {
    if (kOpcodeInfo.IsCallAssigned(op)) {
      return &returnValues;
    }
    return nullptr;
  }
  MIRType *GetCallReturnType();
};

// used by intrinsiccall and xintrinsiccall
class IntrinsiccallNode : public NaryStmtNode {
 public:
  MIRIntrinsicID intrinsic;
  TyIdx tyIdx;
  CallReturnVector returnValues;

 public:
  IntrinsiccallNode(MapleAllocator *allocator, Opcode o)
      : NaryStmtNode(allocator, o), intrinsic(INTRN_UNDEFINED), tyIdx(0), returnValues(allocator->Adapter()) {}

  IntrinsiccallNode(MapleAllocator *allocator, Opcode o, MIRIntrinsicID id)
      : NaryStmtNode(allocator, o), intrinsic(id), tyIdx(0), returnValues(allocator->Adapter()) {}

  IntrinsiccallNode(const MIRModule *mod, Opcode o) : IntrinsiccallNode(mod->CurFuncCodeMemPoolAllocator(), o) {}

  IntrinsiccallNode(const MIRModule *mod, Opcode o, MIRIntrinsicID id)
      : IntrinsiccallNode(mod->CurFuncCodeMemPoolAllocator(), o, id) {}

  IntrinsiccallNode(MapleAllocator *allocator, const IntrinsiccallNode *node)
      : NaryStmtNode(allocator, node),
        intrinsic(node->intrinsic),
        tyIdx(node->tyIdx),
        returnValues(allocator->Adapter()) {}

  IntrinsiccallNode(const MIRModule *mod, const IntrinsiccallNode *node)
      : IntrinsiccallNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  IntrinsiccallNode(IntrinsiccallNode &node) = delete;
  IntrinsiccallNode &operator=(const IntrinsiccallNode &node) = delete;
  ~IntrinsiccallNode() = default;
  void Dump(const MIRModule *mod, int32 indent, bool newline) const;
  void Dump(const MIRModule *mod, int32 indent) const {
    Dump(mod, indent, true);
  }
  bool Verify() const;

  IntrinsiccallNode *CloneTree(MapleAllocator *allocator) {
    IntrinsiccallNode *nd = allocator->mp->New<IntrinsiccallNode>(allocator, this);
    for (size_t i = 0; i < nOpnd.size(); i++) {
      nd->nOpnd.push_back(nOpnd[i]->CloneTree(allocator));
    }
    for (size_t i = 0; i < returnValues.size(); i++) {
      nd->returnValues.push_back(returnValues[i]);
    }
    nd->numOpnds = nOpnd.size();
    return nd;
  }
  IntrinsiccallNode *CloneTree(MIRModule *mod) {
    return IntrinsiccallNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
  int32 NumOpnds(void) {
    ASSERT(numOpnds == nOpnd.size(), "IntrinsiccallNode has wrong numOpnds field");
    return nOpnd.size();
  }
  CallReturnVector *GetCallReturnVector() {
    if (kOpcodeInfo.IsCallAssigned(op)) {
      return &returnValues;
    }
    return nullptr;
  }
  PrimType GetReturnType() const {
    return primType;
  }
  MIRType *GetCallReturnType();
};

// used by callinstant, virtualcallinstant, superclasscallinstant and
// interfacecallinstant, callinstantassigned, virtualcallinstantassigned,
// superclasscallinstantassigned and interfacecallinstantassigned
class CallinstantNode : public CallNode {
 public:
  TyIdx instVecTyIdx;

 public:
  CallinstantNode(MapleAllocator *allocator, Opcode o, TyIdx tidx) : CallNode(allocator, o), instVecTyIdx(tidx) {}

  CallinstantNode(const MIRModule *mod, Opcode o, TyIdx tidx)
      : CallinstantNode(mod->CurFuncCodeMemPoolAllocator(), o, tidx) {}

  CallinstantNode(MapleAllocator *allocator, const CallinstantNode *node)
      : CallNode(allocator, node), instVecTyIdx(node->instVecTyIdx) {}

  CallinstantNode(const MIRModule *mod, const CallinstantNode *node)
      : CallinstantNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  CallinstantNode(CallinstantNode &node) = delete;
  CallinstantNode &operator=(const CallinstantNode &node) = delete;
  ~CallinstantNode() = default;

  void Dump(const MIRModule *mod, int32 indent, bool newline) const;
  void Dump(const MIRModule *mod, int32 indent) const {
    Dump(mod, indent, true);
  }

  CallinstantNode *CloneTree(MapleAllocator *allocator) {
    CallinstantNode *nd = allocator->mp->New<CallinstantNode>(allocator, this);
    for (size_t i = 0; i < nOpnd.size(); i++) {
      nd->nOpnd.push_back(nOpnd[i]->CloneTree(allocator));
    }
    for (size_t i = 0; i < returnValues.size(); i++) {
      nd->returnValues.push_back(returnValues[i]);
    }
    nd->numOpnds = nOpnd.size();
    return nd;
  }
  CallinstantNode *CloneTree(MIRModule *mod) {
    return CallinstantNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

// for java boundary check
class AssertStmtNode : public BinaryStmtNode {
 public:
  bool isLt;

 public:
  explicit AssertStmtNode(Opcode op) : BinaryStmtNode(op) {
    isLt = (op == OP_assertlt);
  }

  ~AssertStmtNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;
};

class LabelNode : public StmtNode {
 public:
  LabelIdx labelIdx;

 public:
  LabelNode() : StmtNode(OP_label), labelIdx(0) {}

  explicit LabelNode(LabelIdx idx) : StmtNode(OP_label), labelIdx(idx) {}

  ~LabelNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  LabelNode *CloneTree(MapleAllocator *allocator) {
    LabelNode *l = allocator->mp->New<LabelNode>(*this);
    l->stmtID = stmtIDNext++;
    return l;
  }

  LabelNode *CloneTree(MIRModule *mod) {
    return LabelNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

class CommentNode : public StmtNode {
 public:
  MapleString comment;

 public:
  explicit CommentNode(MapleAllocator *allocator) : StmtNode(OP_comment), comment(allocator->mp) {}

  explicit CommentNode(const MIRModule *mod) : CommentNode(mod->CurFuncCodeMemPoolAllocator()) {}

  CommentNode(MapleAllocator *allocator, const char *cmt)
      : StmtNode(OP_comment), comment(cmt, allocator->mp) {}

  CommentNode(MIRModule *mod, const char *cmt) : CommentNode(mod->CurFuncCodeMemPoolAllocator(), cmt) {}

  CommentNode(MapleAllocator *allocator, const CommentNode *node)
    : StmtNode(node->op, node->primType, node->numOpnds), comment(node->comment, allocator->mp) {}

  CommentNode(const MIRModule *mod, const CommentNode *node) : CommentNode(mod->CurFuncCodeMemPoolAllocator(), node) {}

  CommentNode(CommentNode &node) = delete;
  CommentNode &operator=(const CommentNode &node) = delete;
  ~CommentNode() = default;

  void Dump(const MIRModule *mod, int32 indent) const;

  CommentNode *CloneTree(MapleAllocator *allocator) {
    CommentNode *c = allocator->mp->New<CommentNode>(allocator, this);
    return c;
  }

  CommentNode *CloneTree(MIRModule *mod) {
    return CommentNode::CloneTree(mod->CurFuncCodeMemPoolAllocator());
  }
};

void DumpCallReturns(const MIRModule *mod, CallReturnVector returnValues, int32 indent);
}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_MIR_NODES_H

