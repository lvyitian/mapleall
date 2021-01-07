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

#ifndef MAPLE_IR_INCLUDE_MIR_PARSER_H
#define MAPLE_IR_INCLUDE_MIR_PARSER_H
#include "mir_module.h"
#include "lexer.h"
#include "mir_nodes.h"
#include "mir_preg.h"
#include "parser_opt.h"

namespace maple {

class FormalDef;

class MIRParser {
 public:
  explicit MIRParser(MIRModule &md)
      : lexer(&md),
        mod(md),
        options(kKeepFirst),
        definedLabels(mod.memPoolAllocator.Adapter()),
        incompleteTypes(mod.memPoolAllocator.Adapter()),
        dummyFunction(nullptr),
        curFunc(nullptr),
        lastFileNum(0),
        lastLineNum(0),
        firstLineNum(0),
        paramParseLocalType(false),
        paramIsIPA(false),
        paramIsComb(false),
        paramOpForStmt(kOpUndef),
        paramTokenKindForStmt(TK_invalid),
        paramCurrFuncForParseStmtBlock(nullptr) {}

  ~MIRParser() {}

 public:
  const std::string &GetError();
  const std::string &GetWarning();
  bool ParseFileinfo(void);
  void PrepareParsingMIR();
  bool ParseMIR(uint32 fileIdx = 0, uint32 options = 0, bool isIPA = false, bool isComb = false);
  bool ParseMIR(std::ifstream *mplfile);         // the main entry point
  bool ParseMPLT(std::ifstream *mpltfile, const std::string &importfilename);
  bool ParseMPLTstandalone(std::ifstream *mpltfile, std::string &importfilename);
  bool ParseTypeFromString(std::string &src, TyIdx &tyIdx);
  void EmitError(const std::string &);
  void EmitWarning(const std::string &);
  uint32 GetOptions() {
    return options;
  }

  bool ParseInlineFuncBody(std::ifstream *mplfile);
  void EmitIncompleteTypes();

 private:
  MIRLexer lexer;
  MIRModule &mod;
  std::string message;
  std::string warningMessage;
  uint32 options;
  MapleVector<bool> definedLabels;  // true if label at labidx is defined
  MapleUnorderedSet<uint32> incompleteTypes;
  MIRFunction *dummyFunction;
  MIRFunction *curFunc;
  uint16 lastFileNum;                    // to remember first number after LOC
  uint32 lastLineNum;                    // to remember second number after LOC
  uint32 firstLineNum;                   // to track function starting line
  std::map<TyIdx, TyIdx> typeDefIdxMap;  // map previous declared tyIdx

  // param for ParseTypedef
  bool paramParseLocalType;

  // param for ParseMIR()
  uint32 paramFileIdx;
  bool paramIsIPA;
  bool paramIsComb;
  TokenKind paramTokenKind;
  std::vector<std::string> paramImportFileList;

  // func ptr map for ParseMIR()
  using FuncPtrParseMIRForElem = bool (MIRParser::*)();
  static std::map<TokenKind, FuncPtrParseMIRForElem> funcPtrMapForParseMIR;
  static std::map<TokenKind, FuncPtrParseMIRForElem> InitFuncPtrMapForParseMIR();

  // func for ParseMIR
  bool ParseMIRForFunc();
  bool ParseMIRForVar();
  bool ParseMIRForClass();
  bool ParseMIRForInterface();
  bool ParseMIRForFlavor();
  bool ParseMIRForSrcLang();
  bool ParseMIRForGlobalMemSize();
  bool ParseMIRForGlobalMemMap();
  bool ParseMIRForGlobalWordsTypeTagged();
  bool ParseMIRForGlobalWordsRefCounted();
  bool ParseMIRForID();
  bool ParseMIRForNumFuncs();
  bool ParseMIRForEntryFunc();
  bool ParseMIRForFileInfo();
  bool ParseMIRForFileData();
  bool ParseMIRForSrcFileInfo();
  bool ParseMIRForImport();
  bool ParseMIRForImportPath();

  // func for ParseExpr
  using FuncPtrParseExpr = bool (MIRParser::*)(BaseNode* &ptr);
  static std::map<TokenKind, FuncPtrParseExpr> funcPtrMapForParseExpr;
  static std::map<TokenKind, FuncPtrParseExpr> InitFuncPtrMapForParseExpr();

  // func and param for ParseStmt
  Opcode paramOpForStmt;
  TokenKind paramTokenKindForStmt;
  using FuncPtrParseStmt = bool (MIRParser::*)(StmtNode* &stmt);
  static std::map<TokenKind, FuncPtrParseStmt> funcPtrMapForParseStmt;
  static std::map<TokenKind, FuncPtrParseStmt> InitFuncPtrMapForParseStmt();

  // func and param for ParseStmtBlock
  MIRFunction *paramCurrFuncForParseStmtBlock;
  using FuncPtrParseStmtBlock = bool (MIRParser::*)();
  static std::map<TokenKind, FuncPtrParseStmtBlock> funcPtrMapForParseStmtBlock;
  static std::map<TokenKind, FuncPtrParseStmtBlock> InitFuncPtrMapForParseStmtBlock();
  void ParseStmtBlockForSeenComment(BlockNode* blk, uint32 mplNum);
  bool ParseStmtBlockForVar(TokenKind stmtTK);
  bool ParseStmtBlockForVar();
  bool ParseStmtBlockForTempVar();
  bool ParseStmtBlockForReg();
  bool ParseStmtBlockForType();
  bool ParseStmtBlockForFrameSize();
  bool ParseStmtBlockForUpformalSize();
  bool ParseStmtBlockForModuleID();
  bool ParseStmtBlockForFuncSize();
  bool ParseStmtBlockForFuncID();
  bool ParseStmtBlockForFormalWordsTypeTagged();
  bool ParseStmtBlockForLocalWordsTypeTagged();
  bool ParseStmtBlockForFormalWordsRefCounted();
  bool ParseStmtBlockForLocalWordsRefCounted();
  bool ParseStmtBlockForFuncInfo();

  MIRFunction *CreateDummyFunction();
  void ResetCurrentFunction() {
    mod.SetCurFunction((mir_func_t *)dummyFunction);
  }

  bool ParseLoc();
  bool ParseLocStmt(StmtNode *&stmt);
  bool ParseAlias(StmtNode *&stmt);
  uint8 *ParseWordsInfo(uint32 size);
  bool ParseSwitchCase(int32 &constVal, LabelIdx &lblidx);
  bool ParseExprOneOperand(BaseNode *&expr);
  bool ParseExprTwoOperand(BaseNode *&opnd0, BaseNode *&opnd1);
  bool ParseExprNaryOperand(MapleVector<BaseNode *> &opndvec);
  bool IsDelimitationTK(TokenKind tk);
  Opcode GetOpFromToken(TokenKind tk);
  bool IsStatement(TokenKind tk);
  bool IsExpression(TokenKind tk);
  PrimType GetPrimitiveType(TokenKind tk);
  MIRIntrinsicID GetIntrinsicId(TokenKind tk);
  bool ParserVectorValue(MIRConst *&stype, MIRType *type);
  bool ParseScalarValue(MIRConst *&stype, MIRType *type);
  bool ParseConstAddrLeafExpr(MIRConst *&, MIRType *);
  bool ParseInitValue(MIRConst *&arrayconst, TyIdx tyIdx);
  bool ParseDeclaredSt(StIdx &);
  bool ParseDeclaredFunc(PUIdx &);
  bool ParseTypeAttrs(TypeAttrs &tA);
  bool ParseVarTypeAttrs(MIRSymbol *st);
  bool ParseAlignAttrs(TypeAttrs &tA);
  bool ParseFieldAttrs(FieldAttrs &tA);
  bool ParseFuncAttrs(FuncAttrs &tA);
  bool CheckPrimAndDerivedType(TokenKind tk, TyIdx &tyIdx);
  bool ParsePrimType(TyIdx &tyIdx);
  bool ParseFarrayType(TyIdx &tyIdx);
  bool ParseArrayType(TyIdx &tyIdx);
  bool ParseBitfieldType(TyIdx &tyIdx);
  bool ParsePragmaElement(MIRPragmaElement *elem);
  bool ParsePragmaElementForArray(MIRPragmaElement *elem);
  bool ParsePragmaElementForAnnotation(MIRPragmaElement *elem);
  bool ParsePragma(MIRStructType &type);
  bool ParseFields(MIRStructType &type);
  bool ParseStructType(TyIdx &tyIdx);
  bool ParseClassType(TyIdx &tyIdx);
  bool ParseInterfaceType(TyIdx &tyIdx);
  bool ParseDefinedTypename(TyIdx &tyIdx, MIRTypeKind kind = kTypeUnknown);
  bool ParseTypeParam(TyIdx &tyIdx);
  bool ParsePointType(TyIdx &tyIdx);
  bool ParseFuncType(TyIdx &tyIdx);
  bool ParseGenericInstantVector(GenericInstantVector &instantvec);
  bool ParseDerivedType(TyIdx &tyIdx, MIRTypeKind kind = kTypeUnknown);
  bool ParseType(TyIdx &tyIdx);
  bool ParseStatement(StmtNode *&stmt);
  bool ParseSpecialReg(PregIdx &pRegIdx);
  bool ParsePseudoreg(PrimType pty, PregIdx &pregidx);
  bool ParseStmtBlock(BlockNode *&blk);
  bool ParsePrototype(MIRFunction *fn, MIRSymbol *funcSt, TyIdx &funcTyidx);
  bool ParseFunction(uint32 fileidx = 0);
  bool ParseStorageClass(MIRSymbol *st);
  bool ParseDeclareVar(MIRSymbol *);
  bool ParseDeclareReg(MIRSymbol *, MIRFunction *);
  bool ParseDeclareFormal(FormalDef *);

  // Stmt Parser
  bool ParseStmtDassign(StmtNode *&stmt);
  bool ParseStmtRegassign(StmtNode *&stmt);
  bool ParseStmtIassign(StmtNode *&stmt);
  bool ParseStmtIassignoff(StmtNode *&stmt);
  bool ParseStmtIassignFPoff(StmtNode *&stmt);
  bool ParseStmtDoloop(StmtNode *&);
  bool ParseStmtForeachelem(StmtNode *&);
  bool ParseStmtDowhile(StmtNode *&);
  bool ParseStmtIf(StmtNode *&);
  bool ParseStmtWhile(StmtNode *&);
  bool ParseStmtLabel(StmtNode *&);
  bool ParseStmtGoto(StmtNode *&);
  bool ParseStmtBr(StmtNode *&);
  bool ParseStmtSwitch(StmtNode *&);
  bool ParseStmtRangegoto(StmtNode *&);
  bool ParseStmtMultiway(StmtNode *&);
  PUIdx EnterUndeclaredFunction(void);
  PUIdx EnterUndeclaredFunctionMcount(void);  // for -pg in order to add "void _mcount()"
  bool ParseStmtCall(StmtNode *&);
  bool ParseStmtCallMcount(StmtNode *&);  // for -pg in order to add "void _mcount()" to all the functions
  bool ParseStmtIcall(StmtNode *&, bool isAssigned);
  bool ParseStmtIcall(StmtNode *&);
  bool ParseStmtIcallAssigned(StmtNode *&);
  bool ParseStmtIntrinsiccall(StmtNode *&, bool isAssigned);
  bool ParseStmtIntrinsiccall(StmtNode *&);
  bool ParseStmtIntrinsiccallassigned(StmtNode *&);
  bool ParseStmtIntrinsiccallwithtype(StmtNode *&, bool isAssigned);
  bool ParseStmtIntrinsiccallwithtype(StmtNode *&);
  bool ParseStmtIntrinsiccallwithtypeassigned(StmtNode *&);
  bool ParseCallReturns(CallReturnVector &);
  bool ParseBinaryStmt(StmtNode *&, Opcode op);
  bool ParseBinaryStmtAssertGE(StmtNode* &);
  bool ParseBinaryStmtAssertLT(StmtNode* &);
  bool ParseNaryStmt(StmtNode *&, Opcode op);
  bool ParseNaryStmtReturn(StmtNode* &);
  bool ParseNaryStmtSyncEnter(StmtNode* &);
  bool ParseNaryStmtSyncExit(StmtNode* &);
  bool ParseStmtJsTry(StmtNode *&);
  bool ParseStmtJavaTry(StmtNode *&);
  bool ParseStmtJavaCatch(StmtNode *&);
  bool ParseStmtCppCatch(StmtNode *&);
  bool ParseUnaryStmt(Opcode op, StmtNode *&);
  bool ParseUnaryStmtThrow(StmtNode* &);
  bool ParseUnaryStmtDecRef(StmtNode* &);
  bool ParseUnaryStmtIncRef(StmtNode* &);
  bool ParseUnaryStmtDecRefReset(StmtNode* &);
  bool ParseUnaryStmtIGoto(StmtNode* &);
  bool ParseUnaryStmtEval(StmtNode* &);
  bool ParseUnaryStmtFree(StmtNode* &);
  bool ParseUnaryStmtAssertNonNull(StmtNode* &);
  bool ParseStmtMarker(StmtNode *&);
  bool ParseStmtGosub(StmtNode *&);

  // Expression Parser
  bool ParseExpression(BaseNode *&expr);
  bool ParseExprDread(BaseNode *&expr);
  bool ParseExprRegread(BaseNode *&expr);
  bool ParseExprBinary(BaseNode *&expr);
  bool ParseExprCompare(BaseNode *&expr);
  bool ParseExprDepositbits(BaseNode *&expr);
  bool ParseExprConstval(BaseNode *&expr);
  bool ParseExprConststr(BaseNode *&expr);
  bool ParseExprConststr16(BaseNode *&expr);
  bool ParseExprSizeoftype(BaseNode *&expr);
  bool ParseExprFieldsDist(BaseNode *&expr);
  bool ParseExprIreadIaddrof(IreadNode *expr);
  bool ParseExprIread(BaseNode *&expr);
  bool ParseExprIreadoff(BaseNode *&expr);
  bool ParseExprIreadFPoff(BaseNode *&expr);
  bool ParseExprIaddrof(BaseNode *&expr);
  bool ParseExprAddrof(BaseNode *&expr);
  bool ParseExprAddroffunc(BaseNode *&expr);
  bool ParseExprAddroflabel(BaseNode *&expr);
  bool ParseExprUnary(BaseNode *&expr);
  bool ParseExprJarray(BaseNode *&expr);
  bool ParseExprSTACKJarray(BaseNode *&expr);
  bool ParseExprGCMalloc(BaseNode *&expr);
  bool ParseExprSTACKMalloc(BaseNode *&expr);
  bool ParseExprExtractbits(BaseNode *&expr);
  bool ParseExprTyconvert(BaseNode *&expr);
  bool ParseExprRetype(BaseNode *&expr);
  bool ParseExprTernary(BaseNode *&expr);
  bool ParseExprArray(BaseNode *&expr);
  bool ParseExprIntrinsicop(BaseNode *&expr);
  bool ParseExprIntrinsicopwithtype(BaseNode *&expr);

  bool ParseTypeDef();
  bool ParseJavaClassInterface(MIRSymbol*, bool);
  bool ParseIntrinsicId(IntrinsicopNode *);
  void Error(const char *str);
  void Warning(const char *str);
  void FixForwardReferencedTypeForOneAgg(MIRType *ty);
  void FixupForwardReferencedTypeByMap();

  // common func
  void SetSrcPos(SrcPosition &srcPosition, uint32 mplNum);

  void CollectIncompleteTypes();
  bool IsIncomplete() {
    return incompleteTypes.size() != 0;
  }
  bool ParseFiledata(void);
  bool ParseSrcFileinfo(void);
  bool ParseFuncinfo(void);
  void DumpOptFuncMap(void);
};
}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_MIR_PARSER_H
