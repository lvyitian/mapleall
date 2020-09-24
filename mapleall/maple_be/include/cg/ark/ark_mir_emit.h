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

// The compact MIR implementation: translate full MIR (kMmpl) to
// compact MIR (kCmpl-v2)
// Note:
// 1.MIR deserialization (read) is in cmpl.cpp
// 2.this code should be compiled with MIR_FEATURE_FULL=1
//
// 1. it shouldn't have any pointers in the IR
// 2. assume LittleEndian for binary format (we don't explicitly handle it here)
// 3. postorder layout for evaluation efficiency.
//
// TODO:
// 1. change it to CPP style to unify the coding convention
// 1. using ROPE for string
// 2. binary compatibility across platforms (e.g., for endianness)
//

#ifndef MAPLE_INCLUDE_MIRGEN_H
#define MAPLE_INCLUDE_MIRGEN_H

#include "cmpl_generator.h"
#include "cmpl.h"
#include "mir_module.h"
#include "mir_symbol.h"
#include "mir_nodes.h"
#include "mir_function.h"
#include "mir_parser.h"
#include "opcode_info.h"
#include "mempool.h"
#include "be_common.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <map>
#include <list>
#include <stack>

using namespace maple;
using namespace std;
using namespace maplebe;

typedef enum {
  kREOpUndef,
#define OPCODE(STR, YY, ZZ, SS) RE_##STR,
#include "mre_opcodes.def"
#undef OPCODE
  kREOpLast
} RE_Opcode;

// MapleRE base instruction format
struct mre_instr_t {
  RE_Opcode op : 8;      // Runtime Engine opcode
  PrimType primType : 8;
  union {
    int16 value;
    int16 offset;        // ireadoff, iassignoff
    int16 frameIdx;      // regread, regassign, addrof
    uint16 numCases;     // javatry numcatches, javacatch numextypes, rangegot numcases
    struct {
      uint8 intrinsicId; // intrinsiccall
      uint8 numOpnds;
    } intrinsic;
    struct {
      uint8 opPtyp;      // cmp, cmpl, cmpg, eq, ge, gt, le, lt, ne, ceil, floor, retype, round, trunc, cvt
      uint8 numOpnds;
    } type;
    union {
      int8   i8;
      uint8  u8;
      int16  i16;
      uint16 u16;
    } constval;          // constval
    struct {
      uint8 boffset;
      uint8 bsize;
    } extractbits;       // extractbits, zext, sext
  } param;
  base_node_t *bn() {
    return reinterpret_cast<base_node_t *>(this);
  }
  bool Is16BitInt(int64 constInt) {
    return((primType==PTY_i8 || primType==PTY_i16 || primType==PTY_i32 || primType==PTY_i64) &&
           constInt <= 32767 && constInt >= -32768);
  }
  bool Is16BitUint(uint64 constInt) {
    return((primType == PTY_u1 || primType==PTY_u8 || primType==PTY_u16 || primType==PTY_u32 ||
            primType==PTY_u64 || primType==PTY_a64 || primType==PTY_ref) &&
           constInt <= 65535);
  }
  PrimType GetPtyp() {
    return primType;
  }
  PrimType GetOpPtyp() {
    return (PrimType)param.type.opPtyp;
  }
  mre_instr_t(RE_Opcode o, PrimType p, uint8 n) {
    op = o;
    primType = p;
    param.value = 0;
    param.type.numOpnds = n;
  }
  mre_instr_t(RE_Opcode o) {
    op = o;
    primType = PTY_unknown;
    param.value = 0;
  }
  mre_instr_t(BaseNode *b) {
    bn()->op = b->op;
    bn()->primType = b->primType;
    bn()->typeflag = 0;
    bn()->numOpnds = b->numOpnds;
  }
};

//
// Additional statement struct defs to cmplsupp.h
//
typedef call_stmt_t callassigned_stmt_t;
typedef icall_stmt_t icallassigned_stmt_t;
typedef intrinsiccall_stmt_t intrinsiccallassigned_stmt_t;

struct dassign_stmt_t : public unary_stmt_t {
  StIdx stidx;
  FieldID fieldid;
};

struct javatry_stmt_t : public stmt_node_t {
  uint32 numCatches;
  // array of uint32 offset to the try stmt's catch block follows
};

struct javacatch_stmt_t : public stmt_node_t {
  uint32 numExTypes;
  // array of typidx_t of exception types handled by the catch follows
};

//
// node struct defs in addiotn to cmpl.h and cmplsupp.h
//

struct iassign_stmt_t : public stmt_node_t {
  TyIdx tyidx;
  FieldID fieldid;
};

struct iread_node_t : public base_node_t {
  TyIdx tyidx;
  FieldID fieldid;
};

typedef iread_node_t iaddrof_type;

struct array_node_t : public base_node_t {
  TyIdx tyidx;
  uint8 bounds_check;
  uint8 padding[3];
};

struct jarraymalloc_node_t : public unary_node_t {
  TyIdx tyidx;
};

struct conststr_node_t : public base_node_t {
  UStrIdx stridx;
};

struct ireadpcoff_node_t : public base_node_t {
  int32 offset;
};

struct addroffpc_node_t : public base_node_t {
  int32 offset;
};

struct callreturn_pair_t {
  StIdx stidx;
  FieldID fieldID;
  PregIdx16 pregIdx;  // TODO: pad to 4B boundary

  void Copy(CallReturnPair &pair) {
    stidx = pair.first;
    fieldID = pair.second.fieldID;
    pregIdx = pair.second.pregIdx;
  }
};

struct callreturn_vec_t {
  uint32 numElem;  // change to uint8 + 3B padding?

  void Emit(CallReturnVector &vec, BinmirImage &image) {
    numElem = vec.size();
    callreturn_pair_t *pairs = image.AllocArray<callreturn_pair_t>(numElem);
    for (int i = 0; i < numElem; i++) {
        pairs[i].Copy(vec[i]);
    }
  }

  uint32 Size(void) {
    return(sizeof(numElem) + sizeof(callreturn_pair_t)*numElem);
  }
};

enum RE_FuncAttr {
  FuncAttrWeak     = 1 << 0,
  FuncAttrFinalize = 1 << 1,
  FuncAttrStatic   = 1 << 2,
  FuncAttrConstructor = 1 << 3
};

class RE_Func {
 public:
  MIRFunction *func;
  StmtNode *currentTry;
  std::set<LabelIdx> funcLabels;

  // Formal args and auto vars of functions are referenced in maple ir
  // instructions as registers using regidx or var names using sym idx.
  // For Maple interpreter, these are mapped to slot idx in the function's
  // storage frame. References to preg and var symbols in formal args
  // are mapped to +ve numbers starting from 1, and the ones in auto vars
  // are mapped to -ve numbers starting from -2. %%retval0 and %%thrownval
  // are special cases that map to 0 and -1.
  int numFormalArgs;
  int numAutoVars;
  int evalStackDepth;
  int funcAttrs;
  std::map<int, int> formalPregs;       // pregno to formals idx
  std::map<MIRSymbol *, int> formalVars;// varsym to formals idx
  std::map<int, int> localPregs;        // local pregno to autos idx
  std::map<MIRSymbol *, int> localVars; // local var to autos idx;
  std::set<MIRSymbol*> cleanupLocalRefVars;  // localrefvar that require cleanup
  bool callsCleanupLocalRefVars;  // boolean indicating if function calls intrinsic CLEANUP_LOCALVARS
  std::set<MIRSymbol*> cleanupFormalVars;  // formal variables that require cleanup
  std::set<MIRSymbol*> cleanupLocalVarsInc;  // local variables that require RC increase
  std::set<PregIdx> cleanupPregsInc;  // pregs that require RC increase

 public:
  void Init(MIRFunction *f) {
    func = f;
    numFormalArgs = 0;
    numAutoVars = 2;  // 2 slots reserved for %%retval0 and %%throwval
    evalStackDepth = 0;
    funcAttrs = 0;
    currentTry = nullptr;
    formalPregs.clear();
    formalVars.clear();
    localPregs.clear();
    localVars.clear();
    funcLabels.clear();
  }
  void SetWeakAttr() {
    funcAttrs |= FuncAttrWeak;
  }
  void SetFinalizeAttr() {
    funcAttrs |= FuncAttrFinalize;
  }
  void SetStaticAttr() {
    funcAttrs |= FuncAttrStatic;
  }
  void SetConstructorAttr() {
    funcAttrs |= FuncAttrConstructor;
  }
  void AddFormalPreg(int pregno) {
    formalPregs[pregno] = 1+numFormalArgs++;
  }
  void AddFormalVar(MIRSymbol *sym) {
    formalVars[sym] = 1+numFormalArgs++;
  }
  void AddLocalPreg(int pregno) {
    localPregs[pregno] = numAutoVars++;
  }
  void AddLocalVar(MIRSymbol *sym) {
    localVars[sym] = numAutoVars++;
  }
  bool FindFormalPreg(int pregno) {
    return(formalPregs.find(pregno) != formalPregs.end());
  }
  bool FindLocalPreg(int pregno) {
    return(localPregs.find(pregno) != localPregs.end());
  }
  bool FindFormalVar(MIRSymbol *sym) {
    return(formalVars.find(sym) != formalVars.end());
  }
  bool FindLocalVar(MIRSymbol *sym) {
    return(localVars.find(sym) != localVars.end());
  }
  int32_t EncodePreg(int pregno) {
    if (FindFormalPreg(pregno)) {
      return formalPregs[pregno];
    } else if (FindLocalPreg(pregno)) {
      return -localPregs[pregno];
    } else if (pregno == -kSregRetval0) {
      return 0;
    } else if (pregno == -kSregThrownval) {
      return -1;
    }
    ASSERT(0, "preg not found in frameinfo");
    return 0;
  }
  int32_t EncodeSym(MIRSymbol *sym) {
    if (FindFormalVar(sym)) {
      return formalVars[sym];
    } else if (FindLocalVar(sym)) {
      return -localVars[sym];
    }
    ASSERT(0, "var not found in frameinfo");
    return 0;
  }
};

class MirGenerator : public CmplGenerator {
 private:
  StmtNode *curStmt;
  int funcOffset = 0;

 public:
  // The (stack of) try stmts remain to be (post)processed.
  stack<TryNode *> *javatry_stmts;

  MIRModule &mmodule;
  std::ofstream &os;
  BECommon &becommon;
  RE_Func curFunc;

  MirGenerator(MIRModule &mod, std::ofstream &ostream, BECommon &be) :
    javatry_stmts(nullptr),
    mmodule(mod), os(ostream), becommon(be) {}
  void FlattenExpr(BaseNode *fexpr);
  void EmitExpr(Opcode expr, PrimType exprType, BaseNode *fexpr);
  void EmitStmt(StmtNode *fstmt);
  void EmitFuncDef(BlockNode *fblock);
  void EmitFunc(MIRFunction *ffunc);
  void OutputMIR(bool genMpl);

  void EmitAsmFuncInfo(MIRFunction *);
  void EmitAsmBaseNode(mre_instr_t &);
  void EmitAsmBaseNode(mre_instr_t &, string comment);
  void EmitAsmCall(CallNode *fstmt);
  void EmitAsmComment(CommentNode *comment);
  void EmitBytes(uint8 *b, int count);
  void EmitBytesComment(uint8 *b, int count, const string &comment);
  void EmitBytesCommentOffset(uint8 *b, int count, const string &comment, int offset);
  void EmitAsmShort(uint16 s);
  void EmitAsmWord(uint32 word);
  void EmitAsmWord(uint32 word, string comment);
  void EmitAsmLong(uint64 quad);
  void EmitAsmFormalArgInfo(MIRFunction *func);
  void EmitAsmFormalArgNameInfo(MIRFunction *func);
  void EmitAsmAutoVarsInfo(MIRFunction *func);
  void EmitAsmAutoVarsNameInfo(MIRFunction *func);
  void EmitAsmIreadPCoff(AddrofNode *fexpr, const string &var);
  void EmitAsmAddroffPC(AddrofNode *fexpr, const string &var);
  void EmitAsmFuncAddr(string funcName);
  void EmitAsmLabel(LabelIdx lbidx, bool genOffset);
  void EmitAsmConststr(UStrIdx);
  void EmitString(const std::string &str, int bytes);
  void EmitStringNoTab(const std::string &str, int bytes);
  void EmitYieldPoint(void);
  void EmitModuleInfo(void);
  void EmitGlobalDecl(void);
  void EmitOpCodes(void);
  void CheckYieldPointInsert(StmtNode *fstmt);
  int GetFormalsInfo(MIRFunction *func);
  int GetLocalsInfo(MIRFunction *func);
  int MaxEvalStack(MIRFunction *func);
  uint32 GetFieldOffsetType(TyIdx tyidx, FieldID fieldid, MIRType *&);
  RE_Opcode MapEHOpcode(RE_Opcode op);
  void CheckInsertOpCvt(Opcode expr, PrimType exprType, PrimType insnType);
  std::string BuildLabelString(LabelIdx lbidx);

  int GetFuncOffset(void) {
    return funcOffset;
  }
  void SetFuncOffset(int offset) {
    funcOffset = offset;
  }
  void IncFuncOffset(int delta) {
    funcOffset += delta;
  }
  inline void SetCurStmt(StmtNode *fstmt) {
    curStmt = fstmt;
  }
  inline StmtNode *GetCurStmt(void) {
    return curStmt;
  }
  inline MIRFunction *GetCurFunction(void) {
    return mmodule.CurFunction();
  }};  // class MirGenerator

extern std::vector <std::string> RE_OpName;

#endif  // MAPLE_INCLUDE_MIRGEN_H
