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

/// Copyright [year] <Copyright Owner>
#ifndef MAPLE_IR_INCLUDE_MIR_BUILDER_H
#define MAPLE_IR_INCLUDE_MIR_BUILDER_H
#include <string>
#include <utility>
#include <vector>
#include <map>
#include "opcodes.h"
#include "prim_types.h"
#include "mir_type.h"
#include "mir_const.h"
#include "mir_symbol.h"
#include "mir_nodes.h"
#include "mir_module.h"
#include "mir_preg.h"
#include "mir_function.h"
#include "printing.h"
#include "intrinsic_op.h"
#include "opcode_info.h"
#include "global_tables.h"

namespace maple {
using namespace std;
using ArgPair = std::pair<std::string, MIRType *>;
using ArgVector = MapleVector<ArgPair>;

class MIRBuilder {
 public:
  enum MatchStyle {
    kUpdateFieldID = 0,  // do not match but traverse to update fieldID
    kMatchTopField = 1,  // match top level field only
    kMatchAnyField = 2,  // match any field
    kParentFirst = 4,    // traverse parent first
    kFoundInChild = 8,   // found in child
  };

  MIRModule *mirModule;
  MapleSet<TyIdx> incompleteTypeRefedSet;
  // <classname strIdx, fieldname strIdx, typename strIdx, attr list strIdx>
  std::vector<std::tuple<uint32, uint32, uint32, uint32>> extraFieldsTuples;
  unsigned lineNum;

 public:
  explicit MIRBuilder(MIRModule *module)
    : mirModule(module),
      incompleteTypeRefedSet(std::less<TyIdx>(), mirModule->memPoolAllocator.Adapter()),
      lineNum(0) {
  }

  virtual ~MIRBuilder() {}

  void SetCurrentFunction(mir_func_t *fun) {
    mirModule->SetCurFunction(fun);
  }

  virtual MIRFunction *GetCurrentFunction(void) const {
    return mirModule->CurFunction();
  }
  MIRFunction *GetCurrentFunctionNotNull() const {
    MIRFunction *func = GetCurrentFunction();
    CHECK_FATAL(func != nullptr, "nullptr check");
    return func;
  }

  GStrIdx GetOrCreateStringIndex(const string &str) const {
    return GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(str);
  }

  GStrIdx GetOrCreateStringIndex(GStrIdx strIdx, const string &str) const {
    std::string firststring(GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx));
    firststring.append(str);
    return GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(firststring);
  }

  GStrIdx GetStringIndex(const string &str) const {
    return GlobalTables::GetStrTable().GetStrIdxFromName(str);
  }

  MIRFunction *GetOrCreateFunction(const string &str, TyIdx rettyidx) {
    return GetOrCreateFunction(str.c_str(), rettyidx);
  }

  MIRFunction *GetFunctionFromSymbol(MIRSymbol *funcst) const;
  MIRFunction *GetFunctionFromStidx(StIdx stIdx);
  MIRFunction *GetFunctionFromName(const string&);

  // For compiler-generated metadata struct
  void AddIntFieldConst(const MIRStructType *sType, MIRAggConst *newConst, uint32 fieldID, int64 constValue);
  void AddAddrofFieldConst(const MIRStructType *sType, MIRAggConst *newConst, uint32 fieldID, const MIRSymbol *fieldSt);
  void AddAddroffuncFieldConst(const MIRStructType *sType, MIRAggConst *newConst,
                               uint32 fieldID, const MIRSymbol *funcSt);
  bool TraverseToNamedField(MIRStructType *structType, GStrIdx nameIdx, uint32 &fieldID);
//  bool TraverseToNamedParentField(MIRStructType *structType, GStrIdx nameidx, int32 &fieldID) const;
  bool IsOfSameType(MIRType *type1, MIRType *type2);
  bool TraverseToNamedFieldWithTypeAndMatchStyle(MIRStructType *structType, GStrIdx nameIdx, TyIdx typeIdx,
                                                 uint32 &fieldID, unsigned matchStyle);
  void TraverseToNamedFieldWithType(MIRStructType *structType, GStrIdx nameIdx, TyIdx typeIdx, uint32 &fieldID,
                                    uint32 &idx);
  FieldID GetStructFieldIdFromNameAndType(MIRType *type, const string &name, TyIdx tid, unsigned int matchStyle);
  FieldID GetStructFieldIdFromNameAndType(MIRType *type, const string &name, TyIdx tid) {
    return GetStructFieldIdFromNameAndType(type, name, tid, kMatchAnyField);
  }

  FieldID GetStructFieldIdFromNameAndTypeParentFirst(MIRType *type, const string &name, TyIdx tid) {
    return GetStructFieldIdFromNameAndType(type, name, tid, kParentFirst);
  }

  FieldID GetStructFieldIdFromNameAndTypeParentFirstFoundInChild(MIRType *type, const string &name, TyIdx tid) {
    return GetStructFieldIdFromNameAndType(type, name, tid, (kFoundInChild | kParentFirst | kUpdateFieldID));
  }

  FieldID GetStructFieldIdFromFieldName(MIRType *type, const string &name) {
    return GetStructFieldIdFromNameAndType(type, name, TyIdx(0), kMatchAnyField);
  }

  FieldID GetStructFieldIdFromFieldNameParentFirst(MIRType *type, const string &name) {
    return GetStructFieldIdFromNameAndType(type, name, TyIdx(0), kParentFirst);
  }

//  void SetStructFieldIdFromFieldName(MIRType *sType, const string &name,
//                                     GStrIdx newStrIdx, const MIRType *newFieldType);
  // for creating Function.
  MIRSymbol *GetFunctionArgument(MIRFunction *fun, uint32 index) const {
    CHECK(index < fun->formalDefVec.size(), "index out of range in GetFunctionArgument");
    return fun->formalDefVec[index].formalSym;
  }

  MIRFunction *CreateFunction(const string &name, const MIRType *returnType, const ArgVector &arguments,
                              bool isVarg = false, bool createBody = true);
  virtual void UpdateFunction(MIRFunction *func, MIRType *returnType, const ArgVector &arguments) {
    return;
  }

 public:
  virtual MIRSymbol *GetOrCreateLocalDecl(const string &name, MIRType *type);
  virtual MIRSymbol *GetOrCreateLocalDecl(const char *name, MIRType *type);
  MIRSymbol *GetLocalDecl(const string &name);
  MIRSymbol *GetLocalDecl(const char *name);
  MIRSymbol *CreateLocalDecl(const string &name, MIRType *type);
  MIRSymbol *CreateLocalDecl(const char *name, MIRType *type);

  MIRSymbol *GetGlobalDecl(const string &name);
  MIRSymbol *GetGlobalDecl(const char *name);
  MIRSymbol *CreateGlobalDecl(const string &str, const MIRType *type, MIRStorageClass storageClass);
  MIRSymbol *CreateGlobalDecl(const char *str, const MIRType *type, MIRStorageClass storageClass);
  MIRSymbol *GetOrCreateGlobalDecl(const string &name, MIRType *type, MIRStorageClass storageClass = kScGlobal);
  MIRSymbol *GetOrCreateGlobalDecl(const char *name, MIRType *type, MIRStorageClass storageClass = kScGlobal);

 public:
  // for creating Expression.
  ConstvalNode *CreateIntConst(int64, PrimType);
  ConstvalNode *CreateFloatConst(float val);
  ConstvalNode *CreateDoubleConst(double val);
  ConstvalNode *CreateFloat128Const(const uint64 *val);
  ConstvalNode *GetConstInt(int i) {
    return CreateIntConst(i, PTY_i32);
  }

  ConstvalNode *GetConstUInt1(bool i) {
    return CreateIntConst(i, PTY_u1);
  }

  ConstvalNode *GetConstUInt8(uint8 i) {
    return CreateIntConst(i, PTY_u8);
  }

  ConstvalNode *GetConstUInt16(uint16 i) {
    return CreateIntConst(i, PTY_u16);
  }

  ConstvalNode *GetConstUInt32(uint32 i) {
    return CreateIntConst(i, PTY_u32);
  }

  ConstvalNode *GetConstUInt64(uint64 i) {
    return CreateIntConst(i, PTY_u64);
  }

  ConstvalNode *GetConstArray(MIRType *, BaseNode *, uint32 length);
  ConstvalNode *CreateAddrofConst(BaseNode *);
  ConstvalNode *CreateAddroffuncConst(BaseNode *);
  ConstvalNode *CreateStrConst(BaseNode *);
  ConstvalNode *CreateStr16Const(BaseNode *);
  ConstvalNode *CreateVectorIntConst(const std::vector<int64> &, PrimType,  MIRType *);

  PrimType UpCvt32(PrimType pty) const;

  SizeoftypeNode *CreateExprSizeoftype(const MIRType *type);
  FieldsDistNode *CreateExprFieldsDist(const MIRType *type, FieldID field1, FieldID field2);
  AddrofNode *CreateExprAddrof(FieldID fieldID, const MIRSymbol *symbol, MemPool *mp = nullptr);
  AddrofNode *CreateExprAddrof(FieldID fieldID, StIdx symbolStIdx, MemPool *mp = nullptr);
  AddroffuncNode *CreateExprAddroffunc(PUIdx, MemPool *mp = nullptr);
  AddrofNode *CreateExprDread(const MIRType *type, FieldID fieldID, const MIRSymbol *symbol);
  virtual AddrofNode *CreateExprDread(MIRType *type, MIRSymbol *symbol);
  virtual AddrofNode *CreateExprDread(MIRSymbol *symbol);
  AddrofNode *CreateExprDread(PregIdx pregid, PrimType pty);
  AddrofNode *CreateExprDread(MIRSymbol *, uint16);
  RegreadNode *CreateExprRegread(PrimType pty, PregIdx regIdx);
  IreadNode *CreateExprIread(const MIRType *returnType, const MIRType *ptrType, FieldID fieldID, BaseNode *addr);
  IreadNode *CreateExprIread(TyIdx *returnTypeIdx, TyIdx *ptrTypeIdx, FieldID fieldID, BaseNode *addr);
  IreadoffNode *CreateExprIreadoff(PrimType pty, int32 offset, BaseNode *opnd0);
  IreadFPoffNode *CreateExprIreadFPoff(PrimType pty, int32 offset);
  IaddrofNode *CreateExprIaddrof(const MIRType *returnType, const MIRType *ptrType, FieldID fieldID, BaseNode *addr);
  IaddrofNode *CreateExprIaddrof(PrimType returnTypePty, TyIdx ptrTypeIdx, FieldID fieldID, BaseNode *addr);
  BinaryNode *CreateExprBinary(Opcode opcode, const MIRType *type, BaseNode *opnd0, BaseNode *opnd1);
  TernaryNode *CreateExprTernary(Opcode opcode, const MIRType *type, BaseNode *opnd0, BaseNode *opnd1, BaseNode *opnd2);
  CompareNode *CreateExprCompare(Opcode opcode, const MIRType *type, const MIRType *opndType, BaseNode *opnd0, BaseNode *opnd1);
  UnaryNode *CreateExprUnary(Opcode opcode, const MIRType *type, BaseNode *opnd);
  GCMallocNode *CreateExprGCMalloc(Opcode opcode, const MIRType *ptype, const MIRType *type);
  JarrayMallocNode *CreateExprSTACKJarrayMalloc(const MIRType *ptype, const MIRType *type, BaseNode *opnd);
  JarrayMallocNode *CreateExprJarrayMalloc(Opcode opcode, const MIRType *ptype, const MIRType *type, BaseNode *opnd);
  TypeCvtNode *CreateExprTypeCvt(Opcode o, const MIRType *type, const MIRType *fromType, BaseNode *opnd);
  ExtractbitsNode *CreateExprExtractbits(Opcode o, const MIRType *type, uint32 bitsOffset, uint32 bitsSize, BaseNode *opnd);
  RetypeNode *CreateExprRetype(const MIRType *type, const MIRType *fromType, BaseNode *opnd);
  ArrayNode *CreateExprArray(MIRType *arrayType, const MapleVector<BaseNode *> &opnds, bool bcheck = true);
  IntrinsicopNode *CreateExprIntrinsicop(MIRIntrinsicID idx, const MIRType *type, const MapleVector<BaseNode *> &ops, bool withType = false);

  // for creating Statement.
  NaryStmtNode *CreateStmtReturn(BaseNode *rVal);
  NaryStmtNode *CreateStmtNary(Opcode op, BaseNode *rVal);
  NaryStmtNode *CreateStmtNary(Opcode op, const MapleVector<BaseNode *> &rVals);
  UnaryStmtNode *CreateStmtUnary(Opcode op, BaseNode *rVal);
  UnaryStmtNode *CreateStmtThrow(BaseNode *rVal);
  DassignNode *CreateStmtDassign(const MIRSymbol *var, FieldID fieldID, BaseNode *src);
  DassignNode *CreateStmtDassign(StIdx sIdx, FieldID fieldId, BaseNode *src);
  RegassignNode *CreateStmtRegassign(PrimType pty, PregIdx regIdx, BaseNode *src);
  IassignNode *CreateStmtIassign(const MIRType *type, FieldID fieldID, BaseNode *addr, BaseNode *src);
  IassignoffNode *CreateStmtIassignoff(PrimType pty, int32 offset, BaseNode *opnd0, BaseNode *src);
  IassignFPoffNode *CreateStmtIassignFPoff(PrimType pty, int32 offset, BaseNode *src);
  CallNode *CreateStmtCall(PUIdx puIdx, const MapleVector<BaseNode *> &args, Opcode opCode = OP_call);
  CallNode *CreateStmtCall(const string &name, const MapleVector<BaseNode *> &args, Opcode opCode = OP_call);

  CallNode *CreateStmtVirtualCall(PUIdx puIdx, const MapleVector<BaseNode *> &args) {
    return CreateStmtCall(puIdx, args, OP_virtualcall);
  }

  CallNode *CreateStmtSuperclassCall(PUIdx puIdx, const MapleVector<BaseNode *> &args) {
    return CreateStmtCall(puIdx, args, OP_superclasscall);
  }

  CallNode *CreateStmtInterfaceCall(PUIdx puIdx, const MapleVector<BaseNode *> &args) {
    return CreateStmtCall(puIdx, args, OP_interfacecall);
  }

  IcallNode *CreateStmtIcall(const MapleVector<BaseNode *> &args);
  // For Call, VirtualCall, SuperclassCall, InterfaceCall
  IntrinsiccallNode *CreateStmtIntrinsicCall(MIRIntrinsicID idx, const MapleVector<BaseNode *> &arguments);
  IntrinsiccallNode *CreateStmtXintrinsicCall(MIRIntrinsicID idx, const MapleVector<BaseNode *> &arguments);
  IntrinsiccallNode *CreateStmtIntrinsicCallwithtype(MIRIntrinsicID idx, const MIRType *type,
                                                              const MapleVector<BaseNode *> &arguments);
  IntrinsiccallNode *CreateStmtIntrinsicCallwithtype(MIRIntrinsicID idx, TyIdx tyIdx,
                                                              const MapleVector<BaseNode *> &arguments);
  CallNode *CreateStmtCallAssigned(PUIdx puIdx, const MapleVector<BaseNode *> &args,
                                           const MIRSymbol *ret, Opcode op);
  CallNode *CreateStmtCallRegassigned(PUIdx, const MapleVector<BaseNode *> &, PregIdx, Opcode);
  CallNode *CreateStmtCallwithtypeAssigned(PUIdx puIdx, const MapleVector<BaseNode *> &args,
                                                           const MIRSymbol *ret, uint32 prototyidx, Opcode op);
  IntrinsiccallNode *CreateStmtIntrinsicCallAssigned(MIRIntrinsicID idx,
                                                              const MapleVector<BaseNode *> &args,
                                                              PregIdx retpregidx);
  IntrinsiccallNode *CreateStmtIntrinsicCallAssigned(MIRIntrinsicID idx,
                                                              const MapleVector<BaseNode *> &args,
                                                              const MIRSymbol *ret);
  IntrinsiccallNode *CreateStmtXintrinsicCallAssigned(MIRIntrinsicID idx,
                                                               const MapleVector<BaseNode *> &args,
                                                               const MIRSymbol *ret);
  IntrinsiccallNode *CreateStmtIntrinsicCallwithtypeAssigned(MIRIntrinsicID idx,
                                                                              const MIRType *type,
                                                                              const MapleVector<BaseNode *> &args,
                                                                              const MIRSymbol *ret);
  CallinstantNode *CreateStmtCallinstant(PUIdx puIdx, TyIdx tyIdx, const MapleVector<BaseNode *> &args, Opcode op);
  CallinstantNode *CreateStmtCallinstantAssigned(PUIdx puIdx, TyIdx tyIdx,
                                                         const MapleVector<BaseNode *> &args,
                                                         const MIRSymbol *ret, Opcode op);
  IfStmtNode *CreateStmtIf(BaseNode *cond);
  IfStmtNode *CreateStmtIfThenElse(BaseNode *cond);
  DoloopNode *CreateStmtDoloop(StIdx, bool, BaseNode *, BaseNode *, BaseNode *);
  SwitchNode *CreateStmtSwitch(BaseNode *opnd, LabelIdx defaultLabel, const CaseVector &switchTable);
  GotoNode *CreateStmtGoto(Opcode o, LabelIdx labidx);
  TryNode *CreateStmtTry(Opcode o, MapleVector<LabelIdx> &cLabIdxs);
  JsTryNode *CreateStmtJsTry(Opcode o, LabelIdx cLabidx, LabelIdx fLabidx);
  TryNode *CreateStmtJavaTry(MapleVector<LabelIdx> &cLabIdxs);
  CppCatchNode *CreateStmtCppCatch(const TyIdx idx);
  CatchNode *CreateStmtJavaCatch(const MapleVector<TyIdx> &tyidxvec);
  LabelIdx GetorCreateMIRLabel(const string &name);
  LabelIdx CreateLabidx(MIRFunction *);
  LabelNode *CreateStmtLabel(LabelIdx labidx);
  StmtNode *CreateStmtComment(const string &cmnt);
  CondGotoNode *CreateStmtCondGoto(BaseNode *cond, Opcode op, LabelIdx labidx);
  AssertStmtNode *CreateStmtAssert(bool, BaseNode *, BaseNode *);
  void AddStmtInCurrentFunctionBody(StmtNode *stmt);

  // create nodes
  AddrofNode *CreateAddrof(const MIRSymbol *st, PrimType pty = PTY_ptr);  // TODO: merge with CreateExprAddrof
  AddrofNode *CreateDread(const MIRSymbol *st, PrimType pty);             // TODO: merge with CreateDread
  void DeepCopyPointer(MIRSymbol *, MIRSymbol *, BlockNode *);
  // const std::string & GetOrCreateTypeInfoStName(MIRType *);
  std::string GetZTIString(const MIRType *);
  std::string GetZTSString(const MIRType *);
  virtual MemPool *GetCurrentFuncCodeMp();
  virtual MapleAllocator *GetCurrentFuncCodeMpAllocator();
  virtual void GlobalLock() {}

  virtual void GlobalUnlock() {}

  //TODO: cleanup those interfaces with c style char *

  GStrIdx GetOrCreateStringIndex(const char *str) const {
    std::string string(str);
    return GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(string);
  }

  GStrIdx GetOrCreateStringIndex(GStrIdx strIdx, const char *str) const {
    std::string firststring(GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx));
    firststring.append(str);
    return GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(firststring);
  }

  GStrIdx GetStringIndex(const char *str) const {
    std::string string(str);
    return GlobalTables::GetStrTable().GetStrIdxFromName(string);
  }

  MIRFunction *GetOrCreateFunction(const char *, TyIdx);
  MIRFunction *GetFunctionFromName(const char *);

  FieldID GetStructFieldIdFromNameAndType(MIRType *type, const char *name, TyIdx tid, unsigned matchStyle);
  FieldID GetStructFieldIdFromNameAndType(MIRType *type, const char *name, TyIdx tid) {
    return GetStructFieldIdFromNameAndType(type, name, tid, 2);
  }

  FieldID GetStructFieldIdFromNameAndTypeParentFirst(MIRType *type, const char *name, TyIdx tid) {
    return GetStructFieldIdFromNameAndType(type, name, tid, 4);
  }

  FieldID GetStructFieldIdFromNameAndTypeParentFirstFoundInChild(MIRType *type, const char *name, TyIdx tid) {
    return GetStructFieldIdFromNameAndType(type, name, tid, 0xc);
  }

  FieldID GetStructFieldIdFromFieldName(MIRType *type, const char *name) {
    return GetStructFieldIdFromNameAndType(type, name, TyIdx(0), 2);
  }

  FieldID GetStructFieldIdFromFieldNameParentFirst(MIRType *type, const char *name) {
    return GetStructFieldIdFromNameAndType(type, name, TyIdx(0), 4);
  }

//  void SetStructFieldIdFromFieldName(MIRType *sType, const char *name,
//                                     GStrIdx newStridx, const MIRType *newFieldtype);

  LabelIdx GetorCreateMIRLabel(const char *name);
  StmtNode *CreateStmtComment(const char *cmnt);

  MIRSymbol *CreatePregFormalSymbol(TyIdx, PregIdx, MIRFunction *);
  MIRSymbol *GetOrCreateTypeSymbol(MIRType *, MIRClassType *);
  // for creating symbol
  MIRSymbol *GetSymbol(TyIdx tyIdx, GStrIdx strIdx, MIRSymKind sKind, MIRStorageClass storageClass,
                       uint8 scpid, MIRFunction *func = nullptr, bool checktype = false);
  MIRSymbol *CreateSymbol(TyIdx, GStrIdx, MIRSymKind, MIRStorageClass, uint8, MIRFunction *);

 private:
  void DeepCopyPointerField(MIRType *, MIRSymbol *, MIRSymbol *, BlockNode *, FieldID &);
};

class MIRBuilderExt : public MIRBuilder {
 public:
  MIRFunction *curFunction = nullptr;
  pthread_mutex_t *mMutex = nullptr;

 public:
  explicit MIRBuilderExt(MIRModule *module, pthread_mutex_t *mutex = nullptr);
  virtual ~MIRBuilderExt() {}

  void SetCurrentFunction(MIRFunction *func) {
    curFunction = func;
  }

  virtual MIRFunction *GetCurrentFunction() const {
    return curFunction;
  }

  virtual MemPool *GetCurrentFuncCodeMp();
  virtual MapleAllocator *GetCurrentFuncCodeMpAllocator();
  virtual void GlobalLock();
  virtual void GlobalUnlock();
};
}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_MIR_BUILDER_H
