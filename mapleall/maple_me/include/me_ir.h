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

#ifndef MAPLE_ME_INCLUDE_ME_IR_H
#define MAPLE_ME_INCLUDE_ME_IR_H
#include "ssa_tab.h"

namespace maple {

class BB;
class PhiNode;
class MeStmt;
class IRMap;
class SSATab;
class VarMeExpr;
class OriginalSt;
class Dominance;
class AssignMeStmt;

enum MeExprOp : std::uint8_t {
  kMeOpUnknown,
  kMeOpVar,
  kMeOpIvar,
  kMeOpAddrof,
  kMeOpAddroffunc,
  kMeOpAddroflabel,
  kMeOpGcmalloc,
  kMeOpReg,
  kMeOpConst,
  kMeOpConststr,
  kMeOpConststr16,
  kMeOpSizeoftype,
  kMeOpFieldsDist,
  kMeOpOp,
  kMeOpNary,
};  // cache the op to avoid dynamic cast

class MeExpr {
 public:
  Opcode op : 8;
  PrimType primType : 8;
  uint8 numOpnds : 8;
  MeExprOp meOp : 8;
  int32 exprID;
  uint32 treeID;  // for bookkeeping purpose during SSAPRE
  MeExpr *next;

 public:
  MeExpr(int32 exprid, MeExprOp meop, Opcode op, PrimType t, size_t n)
    : op(op), primType(t), numOpnds(n), meOp(meop), exprID(exprid), treeID(0), next(nullptr) {}

  virtual ~MeExpr() {}

  virtual size_t GetHashIndex() const {
    return 0;
  }

  virtual void Dump(IRMap *, int32 indent = 0) {}

  virtual size_t GetDepth() const {
    return 0;
  }
  virtual bool IsIdentical(MeExpr *meexpr) = 0;

  virtual bool IsZero() {
    return false;
  }

  virtual bool IsUseSameSymbol(MeExpr *expr) {
    return expr && (exprID == expr->exprID);
  }

  virtual BaseNode *EmitExpr(SSATab *) = 0;
  bool IsLeaf() const {
    return numOpnds == 0;
  }

  bool IsGcmalloc() const {
    return op == OP_gcmalloc || op == OP_gcmallocjarray ||
           op == OP_gcpermalloc || op == OP_gcpermallocjarray;
  }

  bool IsTheSameWorkcand(MeExpr *);

  int32 NumMeExprOpnds() const {
    return numOpnds;
  }

  virtual MeExpr *GetOpnd(uint32 i) {
    return nullptr;
  }
  virtual void SetOpnd(MeExpr *x, uint32 i) {}
  virtual MIRType *GetType() const { return nullptr; }

  void UpdateDepth();              // update the depth, suppose all sub nodes have already depth done.
  MeExpr *GetAddrExprBase();       // get the base of the address expression
  MeExpr *FindSymAppearance(OStIdx oidx);  // find the appearance of the symbol
                                   // in the expression; nullptr otherwise
  bool SymAppears(OStIdx oidx);  // check if symbol appears in the expression
  bool HasIvar();                  // whether there is any iread node in the expression
  bool Pure() const {
    return !kOpcodeInfo.NotPure(op);
  }

  bool IsSameVariableValue(VarMeExpr *);
  MeExpr *ResolveMeExprValue();
  bool CouldThrowException();

  bool IsJavaMerge();
  bool PointsToSomethingThatNeedsIncref();
  virtual bool StrengthReducible() { return false; }
  virtual int64 SRMultiplier() { return 1; }
};

enum MeDefBy {
  kDefByNo,
  kDefByStmt,
  kDefByPhi,
  kDefByChi,
  kDefByMustdef,  // only applies to callassigned and its siblings
};

class ChiMeNode;      // forward decl
class MustDefMeNode;  // forward decl
class IassignMeStmt;  // forward decl

/* Do NOT change the order of enum definitions. */
typedef enum {
  GlobalEsc = 0,
  kArgEsc = 1,  // escapes through iassign to an Arg or Ret
  Arg = 2,
  Ret = 3,
  NoEsc = 4,
  kAbortedEsc = NoEsc + 1,  // this state should not appear during escape analysis
} EscapeStatus;

class MePhiNode;

// base class for VarMeExpr
class ScalarMeExpr : public MeExpr {
 public:
  OriginalSt *ost;
  uint32 vstIdx;    // the index in MEOptimizer's VersionStTable, 0 if not in VersionStTable
  union {
    AssignMeStmt *defStmt;  // definition stmt of this var
    MePhiNode *defPhi;
    ChiMeNode *defChi;          // definition node by Chi
    MustDefMeNode *defMustDef;  // definition by callassigned
  } def;
  MeDefBy defBy : 3;
  EscapeStatus escStatus : 3;  // used by escape analysis

 public:
  ScalarMeExpr(int32 exprid, OriginalSt *origSt, uint32 vidx, MeExprOp meop, Opcode o, PrimType ptyp)
    : MeExpr(exprid, meop, o, ptyp, 0),
      ost(origSt),
      vstIdx(vidx),
      defBy(kDefByNo),
      escStatus(NoEsc) {
    def.defStmt = nullptr;
  }

  bool IsIdentical(MeExpr *meexpr) {
    CHECK_FATAL(false, "ScalarMeExpr::IsIdentical() should not be called");
    return true;
  }
  bool IsUseSameSymbol(MeExpr *);
  void SetDefByStmt(AssignMeStmt *defStmt) {
    defBy = kDefByStmt;
    def.defStmt = defStmt;
  }
  bool IsDefByPhi() const {
    return defBy == kDefByPhi;
  }
  PregIdx GetPregIdx() const {
    CHECK_FATAL(ost->IsPregSymbol(), "GetPregIdx: not a preg");
    return ost->symOrPreg.pregIdx;
  }
  BB *DefByBB();
  BaseNode *EmitExpr(SSATab *ssaTab);
  ScalarMeExpr *FindDefByStmt(std::set<ScalarMeExpr *> *visited);
  void Dump(IRMap *, int32 indent = 0);
};

using RegMeExpr = ScalarMeExpr;

// represant dread
class VarMeExpr : public ScalarMeExpr {
 public:
  TyIdx inferredTyIdx; /* Non zero if it has a known type (allocation type is seen). */
  bool maybeNull : 1;             // false if definitely not null
  bool noDelegateRC : 1;          // true if this cannot be optimized by delegaterc

 public:
  VarMeExpr(int32 exprid, OriginalSt *ost, uint32 vidx, PrimType ptyp)
    : ScalarMeExpr(exprid, ost, vidx, kMeOpVar, OP_dread, ptyp),
      inferredTyIdx(0),
      maybeNull(true),
      noDelegateRC(false) {}
  ~VarMeExpr() {}

  void Dump(IRMap *, int32 indent = 0);
  BaseNode *EmitExpr(SSATab *);
  bool IsVolatile(SSATab *ssaTab);
  // indicate if the variable is local variable but not a function formal variable
  bool IsPureLocal(SSATab *, const MIRFunction *);
  bool IsZeroVersion();
  BB *GetDefByBBMeStmt(Dominance *, MeStmt *&);
  MIRType *GetType() const {
    return GlobalTables::GetTypeTable().GetTypeFromTyIdx(ost->tyIdx);
  }
  VarMeExpr *ResolveVarMeValue();
};

class MePhiNode {
 public:
  ScalarMeExpr *lhs = nullptr;
  MapleVector<ScalarMeExpr *> opnds;
  bool isLive = true;
  BB *defBB = nullptr;  // the bb that defines this phi

 public:
  MePhiNode(MapleAllocator *alloc) : opnds(2, nullptr, alloc->Adapter()) {
    opnds.resize(0);
  }

  MePhiNode(MapleAllocator *alloc, ScalarMeExpr *expr) : lhs(expr), opnds(2, nullptr, alloc->Adapter()) {
    opnds.resize(0);
  }

  ~MePhiNode() {}
  void UpdateLhs(ScalarMeExpr *expr) {
    lhs = expr;
    lhs->defBy = kDefByPhi;
    lhs->def.defPhi = this;
  }
  bool UseReg() {
    return lhs->meOp == kMeOpReg;
  }
  void Dump(IRMap *irMap);
};

class ConstMeExpr : public MeExpr {
 public:
  // int64 value; // TODO: I prefer MIRConst*
  MIRConst *constVal;

  ConstMeExpr(int32 exprid, MIRConst *constVal, PrimType t) : MeExpr(exprid, kMeOpConst, OP_constval, t, 0), constVal(constVal){};
  ~ConstMeExpr() {}
  void Dump(IRMap *, int32 indent = 0);
  size_t GetDepth() const {
    return 0;
  }
  bool IsIdentical(MeExpr *meexpr) {
    if (meexpr->op != op) {
      return false;
    }
    if (meexpr->primType != primType) {
      return false;
    }
    const ConstMeExpr *constmeexpr = static_cast< const ConstMeExpr *>(meexpr);
    MIRConst *itmirconst = constmeexpr->constVal;
    return *constVal == *itmirconst;
  }
  BaseNode *EmitExpr(SSATab *);
  bool GeZero();
  bool GtZero();
  bool IsZero();
  bool IsOne();
  int64 GetIntValue();
  size_t GetHashIndex() const {
    MIRIntConst *intConst = dynamic_cast<MIRIntConst *>(constVal);
    if (intConst != nullptr) {
      return intConst->value;
    }
    MIRFloatConst *fltkonst = dynamic_cast<MIRFloatConst *>(constVal);
    if (fltkonst != nullptr) {
      return fltkonst->GetIntValue();
    }
    MIRDoubleConst *dblkonst = dynamic_cast<MIRDoubleConst *>(constVal);
    if (dblkonst != nullptr) {
      return dblkonst->GetIntValue();
    }
    MIRLblConst *lblkonst = dynamic_cast<MIRLblConst *>(constVal);
    if (lblkonst != nullptr) {
      return lblkonst->value;
    }
    ASSERT(false, "ConstMeExpr::GetHashIndex: const type not yet implemented");
    return 0;
  }
};

class ConststrMeExpr : public MeExpr {
 public:
  UStrIdx strIdx;

  ConststrMeExpr(int32 exprid, UStrIdx idx, PrimType t) : MeExpr(exprid, kMeOpConststr, OP_conststr, t, 0), strIdx(idx){};
  ~ConststrMeExpr() {}
  void Dump(IRMap *, int32 indent = 0);
  bool IsIdentical(MeExpr *meexpr) {
    if (meexpr->op != op) {
      return false;
    }
    const ConststrMeExpr *constmeexpr = static_cast<const ConststrMeExpr *>(meexpr);
    if (strIdx != constmeexpr->strIdx) {
      return false;
    }
    return true;
  }
  BaseNode *EmitExpr(SSATab *);
  size_t GetHashIndex() const {
    return strIdx.GetIdx() << 6;
  }
};

class Conststr16MeExpr : public MeExpr {
 public:
  U16StrIdx strIdx;

  Conststr16MeExpr(int32 exprid, U16StrIdx idx, PrimType t) : MeExpr(exprid, kMeOpConststr16, OP_conststr16, t, 0), strIdx(idx){};
  ~Conststr16MeExpr() {}
  void Dump(IRMap *, int32 indent = 0);
  bool IsIdentical(MeExpr *meexpr) {
    if (meexpr->op != op) {
      return false;
    }
    const Conststr16MeExpr *constmeexpr = static_cast<const Conststr16MeExpr *>(meexpr);
    if (strIdx != constmeexpr->strIdx) {
      return false;
    }
    return true;
  }
  BaseNode *EmitExpr(SSATab *);
  size_t GetHashIndex() const {
    return strIdx.GetIdx() << 6;
  }
};

class SizeoftypeMeExpr : public MeExpr {
 public:
  TyIdx tyIdx;

  SizeoftypeMeExpr(int32 exprid, PrimType t,  TyIdx idx) : MeExpr(exprid, kMeOpSizeoftype, OP_sizeoftype, t, 0), tyIdx(idx){};
  ~SizeoftypeMeExpr() {}
  void Dump(IRMap *, int32 indent = 0);
  bool IsIdentical(MeExpr *meexpr) {
    if (meexpr->op != op) {
      return false;
    }
    const SizeoftypeMeExpr *constmeexpr = static_cast<const SizeoftypeMeExpr *>(meexpr);
    if (tyIdx != constmeexpr->tyIdx) {
      return false;
    }
    return true;
  }
  BaseNode *EmitExpr(SSATab *);
  size_t GetHashIndex() const {
    return tyIdx.GetIdx() << 5;
  }
};

class FieldsDistMeExpr : public MeExpr {
 public:
  FieldsDistMeExpr(int32 exprid, PrimType t, TyIdx idx, FieldID f1, FieldID f2)
      : MeExpr(exprid, kMeOpFieldsDist, OP_fieldsdist, t, 0), tyIdx(idx), fieldID1(f1), fieldID2(f2) {}

  ~FieldsDistMeExpr() = default;
  void Dump(IRMap*, int32 indent = 0);
  bool IsIdentical(MeExpr *meexpr) {
    if (meexpr->op != op) {
      return false;
    }
    const FieldsDistMeExpr *x = static_cast<const FieldsDistMeExpr *>(meexpr);
    if (tyIdx != x->tyIdx || fieldID1 != x->fieldID1 || fieldID2 != x->fieldID2) {
      return false;
    }
    return true;
  }
  BaseNode *EmitExpr(SSATab*);

  TyIdx &GetTyIdx() {
    return tyIdx;
  }

  FieldID GetFieldID1() {
    return fieldID1;
  }

  FieldID GetFieldID2() {
    return fieldID2;
  }

  size_t GetHashIndex() const {
    return (tyIdx.GetIdx() << 10) + (static_cast<uint32>(fieldID1) << 5) + fieldID2;
  }

 private:
  TyIdx tyIdx;
  FieldID fieldID1;
  FieldID fieldID2;
};

class AddrofMeExpr : public MeExpr {
 public:
  OStIdx ostIdx;  // the index in MEOptimizer: OriginalStTable;

  AddrofMeExpr(int32 exprid, PrimType t, OStIdx idx) : MeExpr(exprid, kMeOpAddrof, OP_addrof, t, 0), ostIdx(idx) {}

  ~AddrofMeExpr() {}
  void Dump(IRMap *, int32 indent = 0);
  bool IsIdentical(MeExpr *meexpr) {
    if (meexpr->op != op) {
      return false;
    }
    const AddrofMeExpr *x = static_cast<const AddrofMeExpr *>(meexpr);
    if (ostIdx != x->ostIdx) {
      return false;
    }
    return true;
  }
  bool IsUseSameSymbol(MeExpr *);
  BaseNode *EmitExpr(SSATab *);

  size_t GetHashIndex() const {
    return ostIdx.idx << 4;
  }
};

class AddroffuncMeExpr : public MeExpr {
 public:
  PUIdx puIdx;

  AddroffuncMeExpr(int32 exprid, PUIdx puid) : MeExpr(exprid, kMeOpAddroffunc, OP_addroffunc, PTY_ptr, 0), puIdx(puid) {}

  ~AddroffuncMeExpr() {}

  void Dump(IRMap *, int32 indent = 0);
  bool IsIdentical(MeExpr *meexpr) {
    if (meexpr->op != op) {
      return false;
    }
    const AddroffuncMeExpr *x = static_cast<const AddroffuncMeExpr *>(meexpr);
    if (puIdx != x->puIdx) {
      return false;
    }
    return true;
  }
  BaseNode *EmitExpr(SSATab *);

  size_t GetHashIndex() const {
    return puIdx << 5;
  }
  MIRType *GetType() const {
    MIRFunction *func = GlobalTables::GetFunctionTable().funcTable[puIdx];
    return GlobalTables::GetTypeTable().GetOrCreatePointerType(func->funcType, PTY_ptr);
  }
};

class AddroflabelMeExpr : public MeExpr {
 public:
  LabelIdx labelIdx;

  AddroflabelMeExpr(int32 exprid, LabelIdx lidx) : MeExpr(exprid, kMeOpAddroflabel, OP_addroflabel, PTY_ptr, 0), labelIdx(lidx) {}

  ~AddroflabelMeExpr() {}

  void Dump(IRMap *, int32 indent = 0);
  bool IsIdentical(MeExpr *meexpr) {
    if (meexpr->op != op) {
      return false;
    }
    const AddroflabelMeExpr *x = static_cast<const AddroflabelMeExpr *>(meexpr);
    if (labelIdx != x->labelIdx) {
      return false;
    }
    return true;
  }
  BaseNode *EmitExpr(SSATab *);

  size_t GetHashIndex() const {
    return labelIdx << 4;
  }
};

class GcmallocMeExpr : public MeExpr {
 public:
  TyIdx tyIdx;

  GcmallocMeExpr(int32 exprid, Opcode o, PrimType t, TyIdx tyid) : MeExpr(exprid, kMeOpGcmalloc, o, t, 0), tyIdx(tyid) {}

  ~GcmallocMeExpr() {}

  void Dump(IRMap *, int32 indent = 0);
  bool IsIdentical(MeExpr *meexpr) {
    return false;
  }
  BaseNode *EmitExpr(SSATab *);

  size_t GetHashIndex() const {
    return tyIdx.GetIdx() << 4;
  }
};

constexpr int kOpndNumOfOpMeExpr = 3;
class OpMeExpr : public MeExpr {
 private:
  MeExpr *opnds[kOpndNumOfOpMeExpr];       // kid
 public:
  PrimType opndType : 8;  // from type
  uint8 bitsOffset;
  uint8 bitsSize;
 private:
  uint8 depth;
 public:
  TyIdx tyIdx;
  FieldID fieldID;  // this is also used to store puIdx

  OpMeExpr(int32 exprid, Opcode o, PrimType t, size_t n) : MeExpr(exprid, kMeOpOp, o, t, n),
      opndType(kPtyInvalid), bitsOffset(0), bitsSize(0), depth(0), tyIdx(0), fieldID(0) {
    opnds[0] = nullptr;
    opnds[1] = nullptr;
    opnds[2] = nullptr;
  }

  OpMeExpr(const OpMeExpr &opmeexpr, int32 idx) : MeExpr(idx, kMeOpOp, opmeexpr.op, opmeexpr.primType, opmeexpr.numOpnds),
      opndType(opmeexpr.opndType), bitsOffset(opmeexpr.bitsOffset), bitsSize(opmeexpr.bitsSize),
      depth(opmeexpr.depth), tyIdx(opmeexpr.tyIdx), fieldID(opmeexpr.fieldID) {
    opnds[0] = opmeexpr.opnds[0];
    opnds[1] = opmeexpr.opnds[1];
    opnds[2] = opmeexpr.opnds[2];
  }

  ~OpMeExpr() = default;

  OpMeExpr &operator=(const OpMeExpr &)  = delete;

  void Dump(IRMap *, int32 indent = 0);
  size_t GetDepth() const {
    return depth;
  }
  bool IsIdentical(MeExpr *meexpr);
  bool IsUseSameSymbol(MeExpr *);
  BaseNode *EmitExpr(SSATab *);
  MeExpr *GetOpnd(uint32 idx) {
    ASSERT(idx < 3, "OpMeExpr cannot have more than 3 operands");
    return opnds[idx];
  }
  void SetOpnd(MeExpr *x, uint32 i) {
    opnds[i] = x;
    if (depth <= x->GetDepth()) {
      depth = x->GetDepth();
      if (depth != MAXUINT8) {
        depth++;
      }
    }
  }
  MIRType *GetType() const {
    if (tyIdx != TyIdx(0)) {
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
    }
    return nullptr;
  }
  bool StrengthReducible();
  int64 SRMultiplier();

  size_t GetHashIndex() const {
    size_t hashIdx = static_cast<size_t>(op);
    hashIdx += static_cast<size_t>(opnds[0]->exprID) << 3;
    if (numOpnds > 1) {
      hashIdx += static_cast<size_t>(opnds[1]->exprID) << 3;
      if (numOpnds > 2) {
        hashIdx += static_cast<size_t>(opnds[2]->exprID) << 3;
      }
    }
    return hashIdx;
  }
};

class IvarMeExpr : public MeExpr {
 public:
  IassignMeStmt *defStmt;
  MeExpr *base;
  TyIdx tyIdx;
  TyIdx inferredTyIdx;  // may be a subclass of above tyIdx
  FieldID fieldID;
  EscapeStatus escStatus : 3;
  bool maybeNull : 1;  // false if definitely not null
  bool volatileFromBaseSymbol : 1;  // volatile due to its base symbol being vol
  ScalarMeExpr *mu;        // use of mu, only one for IvarMeExpr

  IvarMeExpr(int32 exprid, PrimType t, TyIdx tidx, FieldID fid)
    : MeExpr(exprid, kMeOpIvar, OP_iread, t, 1),
      defStmt(nullptr),
      base(nullptr),
      tyIdx(tidx),
      inferredTyIdx(0),
      fieldID(fid),
      escStatus(NoEsc),
      maybeNull(true),
      volatileFromBaseSymbol(false),
      mu(nullptr) {}

  IvarMeExpr(int32 exprid, const IvarMeExpr &ivarme)
    : MeExpr(exprid, kMeOpIvar, ivarme.op, ivarme.primType, 1),
      defStmt(ivarme.defStmt),
      base(ivarme.base),
      tyIdx(ivarme.tyIdx),
      fieldID(ivarme.fieldID),
      escStatus(NoEsc),
      maybeNull(true),
      volatileFromBaseSymbol(ivarme.volatileFromBaseSymbol),
      mu(nullptr) {
    mu = ivarme.mu;
  }

  ~IvarMeExpr() {}

  void Dump(IRMap *, int32 indent = 0);
  size_t GetDepth() const {
    return base->GetDepth() + 1;
  }
  bool IsIdentical(MeExpr *meexpr);
  BaseNode *EmitExpr(SSATab *);
  bool IsVolatile();
  bool IsFinal();
  bool IsRCWeak();
  bool IsUseSameSymbol(MeExpr *);
  MeExpr *GetOpnd(uint32 idx) {
    ASSERT(idx == 0, "IvarMeExpr can only have 1 operand");
    return base;
  }
  void SetOpnd(MeExpr *x, uint32 i) {
    base = x;
  }
  MIRType *GetType() const {
    MIRPtrType *ptrtype = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx));
    if (fieldID == 0) {
      return ptrtype->GetPointedType();
    }
    return GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptrtype->GetPointedTyidxWithFieldId(fieldID));
  }

  size_t GetHashIndex() const {
    return static_cast<uint32>(OP_iread) + fieldID + (static_cast<uint32>(base->exprID) << 4);
  }
};

// for array, intrinsicop and intrinsicopwithtype
class NaryMeExpr : public MeExpr {
 public:
  TyIdx tyIdx;
  MIRIntrinsicID intrinsic;
 private:
  MapleVector<MeExpr *> opnds;
  uint8 depth;
 public:
  bool boundCheck;

  NaryMeExpr(MapleAllocator *alloc, int32 expid, Opcode o, PrimType t, size_t n, TyIdx tidx, MIRIntrinsicID intrinid, bool bcheck)
    : MeExpr(expid, kMeOpNary, o, t, n), tyIdx(tidx), intrinsic(intrinid), opnds(alloc->Adapter()), depth(0), boundCheck(bcheck) {}

  NaryMeExpr(MapleAllocator *alloc, int32 expid, NaryMeExpr &meexpr)
    : MeExpr(expid, kMeOpNary, meexpr.op, meexpr.primType, meexpr.numOpnds),
      tyIdx(meexpr.tyIdx),
      intrinsic(meexpr.intrinsic),
      opnds(alloc->Adapter()),
      depth(meexpr.depth),
      boundCheck(meexpr.boundCheck) {
    for (uint32 i = 0; i < meexpr.opnds.size(); i++) {
      opnds.push_back(meexpr.opnds[i]);
    }
  }

  ~NaryMeExpr() = default;

  void Dump(IRMap *, int32 indent = 0);
  size_t GetDepth() const {
    return depth;
  }
  bool IsIdentical(MeExpr *meexpr);
  bool IsUseSameSymbol(MeExpr *);
  BaseNode *EmitExpr(SSATab *);
  MeExpr *GetOpnd(uint32 idx) {
    ASSERT(idx < opnds.size(), "NaryMeExpr operand out of bounds");
    return opnds[idx];
  }
  void SetOpnd(MeExpr *x, uint32 i) {
    opnds[i] = x;
    if (depth <= x->GetDepth()) {
      depth = x->GetDepth();
      if (depth != MAXUINT8) {
        depth++;
      }
    }
  }
  void PushOpnd(MeExpr *x) {
    opnds.push_back(x);
    if (depth <= x->GetDepth()) {
      depth = x->GetDepth();
      if (depth != MAXUINT8) {
        depth++;
      }
    }
  }

  size_t GetHashIndex() const {
    size_t hashIdx = static_cast<size_t>(op);
    for (size_t i = 0; i < numOpnds; i++) {
      hashIdx += opnds[i]->exprID << 3;
    }
    return hashIdx;
  }
};

class MeStmt {
 public:
  Opcode op : 8;
  bool isLive : 8;
  BB *bb;
  SrcPosition srcPos;
  MeStmt *prev;
  MeStmt *next;

  MeStmt(const StmtNode *sst) : op(sst->op), isLive(true), bb(nullptr), srcPos(sst->srcPosition), prev(nullptr),
                                next(nullptr) {}

  MeStmt(Opcode op1) : op(op1), isLive(true), bb(nullptr), prev(nullptr), next(nullptr) {}

  virtual ~MeStmt() {}

  virtual void Dump(IRMap *);
  MeStmt *GetNextMeStmt();
  MeStmt *GetPrevMeStmt();
  void SetBB(BB *curBB) {
    bb = curBB;
  }
  void SetPrev(MeStmt *v) {
    prev = v;
  }

  void SetNext(MeStmt *n) {
    next = n;
  }

  MeStmt *GetPrev() const {
    return prev;
  }

  MeStmt *GetNext() const {
    return next;
  }
  virtual int32 NumMeStmtOpnds() {
    return 0;
  }

  virtual MeExpr *GetMeStmtOpnd(uint32) {
    return nullptr;
  }

  virtual void SetMeStmtOpnd(uint32 idx, MeExpr *val) {}

  bool IsAssertBce() const {
    return op == OP_assertlt || op == OP_assertge;
  }

  bool IsReturn() const {
    return op == OP_gosub || op == OP_retsub || op == OP_throw || op == OP_return;
  }

  bool IsCondBr() const {
    return op == OP_brtrue || op == OP_brfalse;
  }

  void SetCallReturn(ScalarMeExpr *);
  void EmitCallReturnVector(SSATab *ssaTab, CallReturnVector *returnValues);
  virtual MapleVector<MustDefMeNode> *GetMustDefList() {
    return nullptr;
  }

  virtual MeExpr *GetAssignedLhs() {
    return nullptr;
  }

  virtual MapleMap<OStIdx, ChiMeNode *> *GetChiList() {
    return nullptr;
  }

  void CopyBase(MeStmt *mestmt) {
    bb = mestmt->bb;
    srcPos = mestmt->srcPos;
    isLive = mestmt->isLive;
  }

  bool IsTheSameWorkcand(MeStmt *);
  virtual bool NeedDecref() {
    return false;
  }

  virtual void SetNeedDecref(bool val = true) {}

  virtual bool NeedIncref() {
    return false;
  }

  virtual void SetNeedIncref(bool val = true) {}

  virtual MeExpr *GetRhs() {
    return nullptr;
  }

  virtual ScalarMeExpr *GetVarLhs() {
    return nullptr;
  }

  virtual MeExpr *GetLhsRef(SSATab *ssaTab, bool excludelocalrefvar) {
    return nullptr;
  }

  virtual StmtNode *EmitStmt(SSATab *ssaTab);

  void AddNext(MeStmt *node) {
    // add node to the next of this list
    node->next = next;
    node->prev = this;
    if (next) {
      next->prev = node;
    }
    next = node;
  }

  void AddPrev(MeStmt *node) {
    // add node to the prev of this list
    node->prev = prev;
    node->next = this;
    if (prev) {
      prev->next = node;
    }
    prev = node;
  }
};

class ChiMeNode {
 public:
  ScalarMeExpr *rhs;
  ScalarMeExpr *lhs;
  MeStmt *base;
  bool isLive;

  ChiMeNode(MeStmt *mestmt) : rhs(nullptr), lhs(nullptr), base(mestmt), isLive(true) {}

  ~ChiMeNode() {}

  void Dump(IRMap *irMap);
};

class MustDefMeNode {
 public:
  ScalarMeExpr *lhs;  // could be var or register, can we make this private?
  MeStmt *base;
  bool isLive;

  MustDefMeNode(ScalarMeExpr *x, MeStmt *mestmt) : lhs(x), base(mestmt), isLive(true) {
    ScalarMeExpr *reg = static_cast<ScalarMeExpr*>(x);
    reg->defBy = kDefByMustdef;
    reg->def.defMustDef = this;
  }
  MustDefMeNode(const MustDefMeNode& mustDef) {
    lhs = mustDef.lhs;
    base = mustDef.base;
    isLive = mustDef.isLive;
    UpdateLhs(lhs);
  }
  void UpdateLhs(ScalarMeExpr *x) {
    lhs = x;
    x->defBy = kDefByMustdef;
    x->def.defMustDef = this;
  }
  ~MustDefMeNode() {}
  void Dump(IRMap *);
};

class AssignMeStmt : public MeStmt {
 public:
  MeExpr *rhs;
  ScalarMeExpr *lhs;
  bool needIncref : 1;    // to be determined by analyzerc phase
  bool needDecref : 1;    // to be determined by analyzerc phase
  bool isIncDecStmt : 1;   // has the form of an increment or decrement stmt

  AssignMeStmt(const StmtNode *stt)
    : MeStmt(stt),
      rhs(nullptr),
      lhs(nullptr),
      needIncref(false),
      needDecref(false),
      isIncDecStmt(false) {}

  AssignMeStmt(Opcode op, ScalarMeExpr *lhsVal, MeExpr *rhsVal)
    : MeStmt(op),
      rhs(rhsVal),
      lhs(lhsVal),
      needIncref(false),
      needDecref(false),
      isIncDecStmt(false) {}

  ~AssignMeStmt() {}

  int32 NumMeStmtOpnds() {
    return 1;
  }

  MeExpr *GetMeStmtOpnd(uint32) {
    return rhs;
  }

  void SetMeStmtOpnd(uint32 idx, MeExpr *val) {
    rhs = val;
  }

  bool NeedIncref() {
    return needIncref;
  }

  void SetNeedIncref(bool val = true) {
    needIncref = val;
  }

  void SetNoNeedIncref() {
    needIncref = false;
  }

  MeExpr *GetRhs() {
    return rhs;
  }

  ScalarMeExpr *GetLhs() {
    return lhs;
  }

  VarMeExpr *GetVarLhs() {
    CHECK_FATAL(lhs->meOp == kMeOpVar, "Var expected");
    return static_cast<VarMeExpr*>(lhs);
  }

  RegMeExpr *GetRegLhs() {
    CHECK_FATAL(lhs->meOp == kMeOpReg, "Reg expected");
    return static_cast<RegMeExpr*>(lhs);
  }

  void UpdateLhs(ScalarMeExpr *var) {
    lhs = var;
    var->defBy = kDefByStmt;
    var->def.defStmt = this;
  }

  void Dump(IRMap *);
  StmtNode *EmitStmt(SSATab *ssaTab);
};

class DassignMeStmt : public AssignMeStmt {
 public:
  MapleMap<OStIdx, ChiMeNode *> chiList;
  bool propagated : 1;     // the RHS has been propagated forward
  bool wasMayDassign : 1; // was converted back to dassign by may2dassign phase

  DassignMeStmt(MapleAllocator *alloc, const StmtNode *stt)
    : AssignMeStmt(stt),
      chiList(std::less<OStIdx>(), alloc->Adapter()),
      propagated(false),
      wasMayDassign(false) {}

  DassignMeStmt(MapleAllocator *alloc, ScalarMeExpr *lhsVal, MeExpr *rhsVal)
    : AssignMeStmt(OP_dassign, lhsVal, rhsVal),
      chiList(std::less<OStIdx>(), alloc->Adapter()),
      propagated(false),
      wasMayDassign(false) {}

  DassignMeStmt(MapleAllocator *alloc, DassignMeStmt *dass)
    : AssignMeStmt(dass->op, dass->lhs, dass->rhs),
      chiList(std::less<OStIdx>(), alloc->Adapter()),
      propagated(false),
      wasMayDassign(false) {}

  ~DassignMeStmt() {}

  MapleMap<OStIdx, ChiMeNode *> *GetChiList() {
    return &chiList;
  }

  bool NeedDecref() {
    return needDecref;
  }

  void SetNeedDecref(bool val = true) {
    needDecref = val;
  }

  bool Propagated() const {
    return propagated;
  }

  void Dump(IRMap *);

  MeExpr *GetLhsRef(SSATab *ssaTab, bool excludelocalrefvar);

  StmtNode *EmitStmt(SSATab *ssaTab);
};

class MaydassignMeStmt : public MeStmt {
 public:
  MeExpr *rhs;
  FieldID fieldID;
  MapleMap<OStIdx, ChiMeNode *> chiList;
  bool needDecref;  // to be determined by analyzerc phase
  bool needIncref;  // to be determined by analyzerc phase

  MaydassignMeStmt(MapleAllocator *alloc, const StmtNode *stt)
    : MeStmt(stt),
      rhs(nullptr),
      fieldID(0),
      chiList(std::less<OStIdx>(), alloc->Adapter()),
      needDecref(false),
      needIncref(false) {}

  ~MaydassignMeStmt() {}

  int32 NumMeStmtOpnds() {
    return 1;
  }

  MeExpr *GetMeStmtOpnd(uint32) {
    return rhs;
  }

  void SetMeStmtOpnd(uint32 idx, MeExpr *val) {
    rhs = val;
  }

  MapleMap<OStIdx, ChiMeNode *> *GetChiList() {
    return &chiList;
  }

  bool NeedDecref() {
    return needDecref;
  }

  void SetNeedDecref(bool val = true) {
    needDecref = val;
  }

  bool NeedIncref() {
    return needIncref;
  }

  void SetNeedIncref(bool val = true) {
    needIncref = val;
  }

  void Dump(IRMap *);
  MeExpr *GetRhs() {
    return rhs;
  }

  VarMeExpr *GetVarLhs() {
    return static_cast<VarMeExpr *>(chiList.begin()->second->lhs);
  }

  MeExpr *GetLhsRef(SSATab *ssaTab, bool excludelocalrefvar);

  MIRSymbol *GetLhsSym(SSATab *ssaTab) {
    return ssaTab->GetMIRSymbolFromid(GetVarLhs()->ost->index);
  }

  StmtNode *EmitStmt(SSATab *ssaTab);
};

class IassignMeStmt : public MeStmt {
 public:
  TyIdx tyIdx;
  IvarMeExpr *lhsVar;
  MeExpr *rhs;
  MapleMap<OStIdx, ChiMeNode *> chiList;
  bool needDecref;  // to be determined by analyzerc phase
  bool needIncref;  // to be determined by analyzerc phase

  IassignMeStmt(MapleAllocator *alloc, const StmtNode *stt)
    : MeStmt(stt),
      tyIdx(0),
      lhsVar(nullptr),
      rhs(nullptr),
      chiList(std::less<OStIdx>(), alloc->Adapter()),
      needDecref(false),
      needIncref(false) {}

  IassignMeStmt(MapleAllocator *alloc, const IassignMeStmt &iss)
    : MeStmt(iss),
      tyIdx(iss.tyIdx),
      lhsVar(iss.lhsVar),
      rhs(iss.rhs),
      chiList(iss.chiList),
      needDecref(false),
      needIncref(false) {}

  IassignMeStmt(MapleAllocator *alloc, TyIdx tidx, IvarMeExpr *l, MeExpr *r,
                const MapleMap<OStIdx, ChiMeNode *> *clist)
    : MeStmt(OP_iassign), tyIdx(tidx), lhsVar(l), rhs(r), chiList(*clist), needDecref(false), needIncref(false) {
    l->defStmt = this;
  }

  ~IassignMeStmt() {}

  int32 NumMeStmtOpnds() {
    return 2;
  }

  MeExpr *GetMeStmtOpnd(uint32 idx) {
    return idx == 0 ? lhsVar->base : rhs;
  }

  void SetMeStmtOpnd(uint32 idx, MeExpr *val) {
    if (idx == 0) {
      lhsVar->base = val;
    } else {
      rhs = val;
    }
  }

  MapleMap<OStIdx, ChiMeNode *> *GetChiList() {
    return &chiList;
  }

  MeExpr *GetLhsRef(SSATab *ssaTab, bool excludelocalrefvar);

  bool NeedDecref() {
    return needDecref;
  }

  void SetNeedDecref(bool val = true) {
    needDecref = val;
  }

  bool NeedIncref() {
    return needIncref;
  }

  void SetNeedIncref(bool val = true) {
    needIncref = val;
  }

  void Dump(IRMap *);
  MeExpr *GetRhs() {
    return rhs;
  }

  StmtNode *EmitStmt(SSATab *ssaTab);
};

class NaryMeStmt : public MeStmt {
 public:
  MapleVector<MeExpr *> opnds;

  NaryMeStmt(MapleAllocator *alloc, const StmtNode *stt) : MeStmt(stt), opnds(alloc->Adapter()) {}

  NaryMeStmt(MapleAllocator *alloc, Opcode op) : MeStmt(op), opnds(alloc->Adapter()) {}

  NaryMeStmt(MapleAllocator *alloc, NaryMeStmt *nstmt) : MeStmt(nstmt->op), opnds(alloc->Adapter()) {
    for (MeExpr *o : nstmt->opnds) {
      opnds.push_back(o);
    }
  }

  virtual ~NaryMeStmt() {}

  int32 NumMeStmtOpnds() {
    return opnds.size();
  }

  MeExpr *GetMeStmtOpnd(uint32 idx) {
    return opnds[idx];
  }

  void SetMeStmtOpnd(uint32 idx, MeExpr *val) {
    opnds[idx] = val;
  }

  void DumpOpnds(IRMap *);
  void Dump(IRMap *);
  virtual MapleMap<OStIdx, ScalarMeExpr *> *GetMuList() {
    return nullptr;
  }

  StmtNode *EmitStmt(SSATab *ssaTab);
};

class MuChiMePart {
 public:
  MapleMap<OStIdx, ScalarMeExpr *> muList;
  MapleMap<OStIdx, ChiMeNode *> chiList;

  MuChiMePart(MapleAllocator *alloc)
    : muList(std::less<OStIdx>(), alloc->Adapter()), chiList(std::less<OStIdx>(), alloc->Adapter()) {}

  virtual ~MuChiMePart() {}
};

class AssignedPart {
 public:
  MapleVector<MustDefMeNode> mustDefList;
  bool needDecref;  // to be determined by analyzerc phase
  bool needIncref;  // to be determined by analyzerc phase
  explicit AssignedPart(MapleAllocator *alloc)
    : mustDefList(alloc->Adapter()), needDecref(false), needIncref(false) {}

  explicit AssignedPart(const MapleVector<MustDefMeNode> &mustdefList)
    : mustDefList(mustdefList), needDecref(false), needIncref(false) {}

  virtual ~AssignedPart() {}
  void DumpAssignedPart(IRMap *irMap);
  VarMeExpr *GetAssignedPartLhsRef(SSATab *ssaTab, bool excludelocalrefvar);
};

class CallMeStmt : public NaryMeStmt, public MuChiMePart, public AssignedPart {
 public:
  PUIdx puIdx;
  // Used in trim call graph
  uint32 stmtID;
  TyIdx tyIdx;

  CallMeStmt(MapleAllocator *alloc, const StmtNode *stt)
    : NaryMeStmt(alloc, stt),
      MuChiMePart(alloc),
      AssignedPart(alloc),
      puIdx(static_cast<const CallNode *>(stt)->puIdx),
      stmtID(stt->stmtID),
      tyIdx(static_cast<const CallNode*>(stt)->tyIdx) {}

  CallMeStmt(MapleAllocator *alloc, Opcode op)
    : NaryMeStmt(alloc, op), MuChiMePart(alloc), AssignedPart(alloc), puIdx(0), stmtID(0) {}

  CallMeStmt(MapleAllocator *alloc, CallMeStmt *cstmt)
    : NaryMeStmt(alloc, cstmt),
      MuChiMePart(alloc),
      AssignedPart(cstmt->mustDefList),
      puIdx(cstmt->puIdx),
      stmtID(0) {}

  virtual ~CallMeStmt() {}

  void Dump(IRMap *);

  MapleMap<OStIdx, ScalarMeExpr*> *GetMuList() {
    return &muList;
  }

  MapleMap<OStIdx, ChiMeNode*> *GetChiList() {
    return &chiList;
  }

  MapleVector<MustDefMeNode> *GetMustDefList() {
    return &mustDefList;
  }

  MeExpr *GetAssignedLhs() {
    return mustDefList.empty() ? nullptr : mustDefList.front().lhs;
  }
  MeExpr *GetLhsRef(SSATab *ssaTab, bool excludelocalrefvar) {
    return GetAssignedPartLhsRef(ssaTab, excludelocalrefvar);
  }

  bool NeedDecref() {
    return needDecref;
  }

  void SetNeedDecref(bool val = true) {
    needDecref = val;
  }

  bool NeedIncref() {
    return needIncref;
  }

  void SetNeedIncref(bool val = true) {
    needIncref = val;
  }

  StmtNode *EmitStmt(SSATab *ssaTab);
};

class IcallMeStmt : public NaryMeStmt, public MuChiMePart, public AssignedPart {
public:
  // return type for callee
  TyIdx retTyIdx;
  // Used in trim call graph
  uint32 stmtID;

  IcallMeStmt(MapleAllocator *alloc, const StmtNode *stt)
    : NaryMeStmt(alloc, stt),
      MuChiMePart(alloc),
      AssignedPart(alloc),
      retTyIdx(static_cast<const IcallNode *>(stt)->retTyIdx),
      stmtID(stt->stmtID) {}
  IcallMeStmt(MapleAllocator *alloc, Opcode op)
    : NaryMeStmt(alloc, op), MuChiMePart(alloc), AssignedPart(alloc), retTyIdx(0), stmtID(0) {}
  IcallMeStmt(MapleAllocator *alloc, IcallMeStmt *cstmt)
    : NaryMeStmt(alloc, cstmt),
      MuChiMePart(alloc),
      AssignedPart(cstmt->mustDefList),
      retTyIdx(cstmt->retTyIdx),
      stmtID(cstmt->stmtID) {}
  virtual ~IcallMeStmt() { }
  void Dump(IRMap *);
  MapleMap<OStIdx, ScalarMeExpr*> *GetMuList() {
    return &muList;
  }

  MapleMap<OStIdx, ChiMeNode*> *GetChiList() {
    return &chiList;
  }

  MapleVector<MustDefMeNode> *GetMustDefList() {
    return &mustDefList;
  }

  MeExpr *GetAssignedLhs() {
    return mustDefList.empty() ? nullptr : mustDefList.front().lhs;
  }
  MeExpr *GetLhsRef(SSATab *ssaTab, bool excludelocalrefvar) {
    return GetAssignedPartLhsRef(ssaTab, excludelocalrefvar);
  }
  bool NeedDecref() { return needDecref; }
  void SetNeedDecref(bool val = true) { needDecref = val; }
  bool NeedIncref() { return needIncref; }
  void SetNeedIncref(bool val = true) { needIncref = val; }
  StmtNode *EmitStmt(SSATab *ssaTab);
};

class IntrinsiccallMeStmt : public NaryMeStmt, public MuChiMePart, public AssignedPart {
 public:
  MIRIntrinsicID intrinsic;
  TyIdx tyIdx;
  // Used to store return value type
  PrimType retPType;

  IntrinsiccallMeStmt(MapleAllocator *alloc, const StmtNode *stt)
    : NaryMeStmt(alloc, stt),
      MuChiMePart(alloc),
      AssignedPart(alloc),
      intrinsic(static_cast<const IntrinsiccallNode *>(stt)->intrinsic),
      tyIdx(static_cast<const IntrinsiccallNode*>(stt)->tyIdx),
      retPType(stt->primType) {}

  IntrinsiccallMeStmt(MapleAllocator *alloc, Opcode op, MIRIntrinsicID id, TyIdx tyid = TyIdx())
    : NaryMeStmt(alloc, op),
      MuChiMePart(alloc),
      AssignedPart(alloc),
      intrinsic(id),
      tyIdx(tyid),
      retPType(kPtyInvalid) {}

  IntrinsiccallMeStmt(MapleAllocator *alloc, IntrinsiccallMeStmt *intrn)
    : NaryMeStmt(alloc, intrn),
      MuChiMePart(alloc),
      AssignedPart(intrn->mustDefList),
      intrinsic(intrn->intrinsic),
      tyIdx(intrn->tyIdx),
      retPType(intrn->retPType) {}

  virtual ~IntrinsiccallMeStmt() {}

  void Dump(IRMap *);

  MapleMap<OStIdx, ScalarMeExpr*> *GetMuList() {
    return &muList;
  }

  MapleMap<OStIdx, ChiMeNode*> *GetChiList() {
    return &chiList;
  }

  MapleVector<MustDefMeNode> *GetMustDefList() {
    return &mustDefList;
  }

  MeExpr *GetAssignedLhs() {
    return mustDefList.empty() ? nullptr : mustDefList.front().lhs;
  }
  MeExpr *GetLhsRef(SSATab *ssaTab, bool excludelocalrefvar) {
    return GetAssignedPartLhsRef(ssaTab, excludelocalrefvar);
  }
  PrimType GetReturnType() const {
    return retPType;
  }

  bool NeedDecref() {
    return needDecref;
  }

  void SetNeedDecref(bool val = true) {
    needDecref = val;
  }

  bool NeedIncref() {
    return needIncref;
  }

  void SetNeedIncref(bool val = true) {
    needIncref = val;
  }
  StmtNode *EmitStmt(SSATab *ssaTab);
};

class RetMeStmt : public NaryMeStmt {
 public:
  MapleMap<OStIdx, ScalarMeExpr *> muList;

  RetMeStmt(MapleAllocator *alloc, const StmtNode *stt)
    : NaryMeStmt(alloc, stt), muList(std::less<OStIdx>(), alloc->Adapter()) {}

  ~RetMeStmt() {}

  void Dump(IRMap *);
  MapleMap<OStIdx, ScalarMeExpr *> *GetMuList() {
    return &muList;
  }
};

// eval, free, decref, incref, assertnonnull, igoto
class UnaryMeStmt : public MeStmt {
 public:
  MeExpr *opnd;
  bool not_need_lock;
  bool decref_before_exit;  // true if decref is inserted due to anticipated function exit
  bool decrefresetAtCatch;  // true if decrefreset is special insertion at catch

  UnaryMeStmt(const StmtNode *stt) : MeStmt(stt), opnd(nullptr), not_need_lock(false), decref_before_exit(false), decrefresetAtCatch(false) {}

  UnaryMeStmt(Opcode o) : MeStmt(o), opnd(nullptr), not_need_lock(false), decref_before_exit(false), decrefresetAtCatch(false) {}

  UnaryMeStmt(UnaryMeStmt *umestmt)
    : MeStmt(umestmt->op), opnd(umestmt->opnd), not_need_lock(umestmt->not_need_lock), decref_before_exit(false), decrefresetAtCatch(false) {}

  virtual ~UnaryMeStmt() {}

  int32 NumMeStmtOpnds() {
    return 1;
  }

  MeExpr *GetMeStmtOpnd(uint32 idx) {
    return opnd;
  }

  void SetMeStmtOpnd(uint32 idx, MeExpr *val) {
    opnd = val;
  }

  void Dump(IRMap *);
  StmtNode *EmitStmt(SSATab *ssaTab);
};

// decrefreset
class DecrefresetMeStmt : public UnaryMeStmt {
 public:
  VarMeExpr *thevar;           // to provide to variable use information

  DecrefresetMeStmt(VarMeExpr *v) : UnaryMeStmt(OP_decrefreset), thevar(v) {}

  virtual ~DecrefresetMeStmt() {}
};

class GotoMeStmt : public MeStmt {
 public:
  uint32 offset;  // the label
  GotoMeStmt(const StmtNode *stt) : MeStmt(stt), offset(static_cast<const GotoNode *>(stt)->offset) {}
  GotoMeStmt(uint32 o) : MeStmt(OP_goto), offset(o) {}

  ~GotoMeStmt() {}

  StmtNode *EmitStmt(SSATab *ssaTab);
};

class CondGotoMeStmt : public UnaryMeStmt {
 public:
  uint32 offset;  // the label
 public:
  CondGotoMeStmt(const StmtNode *stt) : UnaryMeStmt(stt), offset(static_cast<const CondGotoNode *>(stt)->offset) {}

  ~CondGotoMeStmt() {}

  void Dump(IRMap *);
  StmtNode *EmitStmt(SSATab *ssaTab);
};

class JsTryMeStmt : public MeStmt {
 public:
  uint16 catchOffset;
  uint16 finallyOffset;

  JsTryMeStmt(const StmtNode *stt)
    : MeStmt(stt),
      catchOffset(static_cast<const JsTryNode *>(stt)->catchOffset),
      finallyOffset(static_cast<const JsTryNode *>(stt)->finallyOffset) {}

  ~JsTryMeStmt() {}

  StmtNode *EmitStmt(SSATab *ssaTab);
};

class TryMeStmt : public MeStmt {
 public:
  MapleVector<LabelIdx> offsets;

  TryMeStmt(MapleAllocator *alloc, const StmtNode *stt) : MeStmt(stt), offsets(alloc->Adapter()) {}

  ~TryMeStmt() {}

  StmtNode *EmitStmt(SSATab *ssaTab);
};

class CppCatchMeStmt : public MeStmt {
 public:
  TyIdx exceptionTyIdx;

  CppCatchMeStmt(StmtNode *stt) : MeStmt(stt), exceptionTyIdx(static_cast<CppCatchNode *>(stt)->exceptionTyIdx) {}

  ~CppCatchMeStmt() {}

  StmtNode *EmitStmt(SSATab *ssaTab);
};

class JavaCatchMeStmt : public MeStmt {
 public:
  MapleVector<TyIdx> exceptionTyIdxVec;

  JavaCatchMeStmt(MapleAllocator *alloc, StmtNode *stt) : MeStmt(stt), exceptionTyIdxVec(alloc->Adapter()) {
    for (auto it : static_cast<CatchNode *>(stt)->exceptionTyIdxVec) {
      exceptionTyIdxVec.push_back(it);
    }
  }

  ~JavaCatchMeStmt() {}

  StmtNode *EmitStmt(SSATab *ssaTab);
};

class SwitchMeStmt : public UnaryMeStmt {
 public:
  LabelIdx defaultLabel;
  CaseVector switchTable;

  SwitchMeStmt(MapleAllocator *alloc, const StmtNode *stt)
    : UnaryMeStmt(stt), defaultLabel(static_cast<const SwitchNode *>(stt)->defaultLabel),
      switchTable(alloc->Adapter()) {
    switchTable = static_cast<const SwitchNode *>(stt)->switchTable;
  }

  ~SwitchMeStmt() {}

  void Dump(IRMap *);
  StmtNode *EmitStmt(SSATab *ssaTab);
};

class CommentMeStmt : public MeStmt {
 public:
  MapleString comment;

  CommentMeStmt(MapleAllocator *alloc, const StmtNode *stt) : MeStmt(stt), comment(alloc->GetMemPool()) {
    comment = static_cast<const CommentNode *>(stt)->comment;
  }

  ~CommentMeStmt() {}

  StmtNode *EmitStmt(SSATab *ssaTab);
};

class WithMuMeStmt : public MeStmt {
 public:
  MapleMap<OStIdx, ScalarMeExpr *> muList;

  WithMuMeStmt(MapleAllocator *alloc, const StmtNode *stt)
      : MeStmt(stt), muList(std::less<OStIdx>(), alloc->Adapter()) {}

  virtual ~WithMuMeStmt() {}
};

class GosubMeStmt : public WithMuMeStmt {
 public:
  uint32 offset;  // the label

  GosubMeStmt(MapleAllocator *alloc, const StmtNode *stt)
    : WithMuMeStmt(alloc, stt), offset(static_cast<const GotoNode *>(stt)->offset) {}

  ~GosubMeStmt() {}

  void Dump(IRMap *);
  StmtNode *EmitStmt(SSATab *ssaTab);
};

class ThrowMeStmt : public WithMuMeStmt {
 public:
  MeExpr *opnd;

  ThrowMeStmt(MapleAllocator *alloc, const StmtNode *stt) : WithMuMeStmt(alloc, stt), opnd(nullptr) {}

  ~ThrowMeStmt() {}

  int32 NumMeStmtOpnds() {
    return 1;
  }

  MeExpr *GetMeStmtOpnd(uint32 idx) {
    return opnd;
  }

  void SetMeStmtOpnd(uint32 idx, MeExpr *val) {
    opnd = val;
  }

  void Dump(IRMap *);
  StmtNode *EmitStmt(SSATab *ssaTab);
};

class SyncMeStmt : public NaryMeStmt, public MuChiMePart {
 public:
  SyncMeStmt(MapleAllocator *alloc, const StmtNode *stt) : NaryMeStmt(alloc, stt), MuChiMePart(alloc) {}

  ~SyncMeStmt() {}

  void Dump(IRMap *);
  MapleMap<OStIdx, ScalarMeExpr *> *GetMuList() {
    return &muList;
  }

  MapleMap<OStIdx, ChiMeNode *> *GetChiList() {
    return &chiList;
  }
};

// assert ge or lt for java boundary check
class AssertMeStmt : public MeStmt {
 public:
  MeExpr *opnds[2];

  AssertMeStmt(const StmtNode *stt) : MeStmt(stt) {
    Opcode op = stt->op;
    CHECK_FATAL(stt->op == OP_assertge || stt->op == OP_assertlt, "");
  }

  AssertMeStmt(Opcode op1) : MeStmt(op1) {
    Opcode op = op1;
    CHECK_FATAL(op == OP_assertge || op == OP_assertlt, "");
  }

  AssertMeStmt(const AssertMeStmt &assmestmt) : MeStmt(assmestmt.op) {
    op = assmestmt.op;
    opnds[0] = assmestmt.opnds[0];
    opnds[1] = assmestmt.opnds[1];
  }

  AssertMeStmt(MeExpr *arrexpr, MeExpr *indxexpr, bool ilt) : MeStmt(ilt ? OP_assertlt : OP_assertge) {
    opnds[0] = arrexpr;
    opnds[1] = indxexpr;
  }

  ~AssertMeStmt() {}

  bool HasSameVersion(const AssertMeStmt *assmestmt) const {
    CHECK_FATAL(op == assmestmt->op, "");
    return (opnds[0] == assmestmt->opnds[0] && opnds[1] == assmestmt->opnds[1]);
  }

  bool IsTheSameWorkcand(AssertMeStmt *);
  MeExpr *GetIndexExpr() {
    return opnds[1];
  }

  MeExpr *GetArrayExpr() {
    return opnds[0];
  }

  void Dump(IRMap *);
  StmtNode *EmitStmt(SSATab *ssaTab);

 private:
  AssertMeStmt &operator=(const AssertMeStmt &assmestmt) {
    if (&assmestmt == this) {
      return *this;
    }
    op = assmestmt.op;
    opnds[0] = assmestmt.opnds[0];
    opnds[1] = assmestmt.opnds[1];
    return *this;
  }
};

MapleMap<OStIdx, ChiMeNode *> *GenericGetChiListFromVarMeExpr(ScalarMeExpr *expr);
void DumpMuList(IRMap *irMap, MapleMap<OStIdx, ScalarMeExpr *> &mulist, int32 indent);
void DumpChiList(IRMap *irMap, MapleMap<OStIdx, ChiMeNode *> &chilist);

class DumpOptions {
 public:
  static bool dumpEscape;
  static bool simpleDump;
  static int dumpVsymNum;
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_IR_H
