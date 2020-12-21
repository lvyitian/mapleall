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

#include "mir_parser.h"
#include "mir_function.h"
#include "name_mangler.h"

using namespace std;
namespace maple {
std::map<TokenKind, MIRParser::FuncPtrParseExpr> MIRParser::funcPtrMapForParseExpr =
    MIRParser::InitFuncPtrMapForParseExpr();

static Opcode GetUnaryOp(TokenKind tk) {
  switch (tk) {
#define UNARYOP(P) \
  case TK_##P:     \
    return OP_##P;
#include "unary_op.def"
#undef UNARYOP
    default:
      return kOpUndef;
  }
}

static Opcode GetBinaryOp(TokenKind tk) {
  switch (tk) {
#define BINARYOP(P) \
  case TK_##P:      \
    return OP_##P;
#include "binary_op.def"
#undef BINARYOP
    default:
      return kOpUndef;
  }
}

static Opcode GetConvertOp(TokenKind tk) {
  switch (tk) {
    case TK_ceil:
      return OP_ceil;
    case TK_cvt:
      return OP_cvt;
    case TK_floor:
      return OP_floor;
    case TK_round:
      return OP_round;
    case TK_trunc:
      return OP_trunc;
    default:
      return kOpUndef;
  }
}

bool MIRParser::ParseExprOneOperand(BaseNode *&expr)  // (<opnd0>)
{
  if (lexer.GetTokenKind() != TK_lparen) {
    Error("expect ( parsing operand parsing unary ");
    return false;
  }
  lexer.NextToken();

  if (!ParseExpression(expr)) {
    Error("expect expression as openrand of unary expression ");
    return false;
  }

  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing operand parsing unary ");
    return false;
  }

  return true;
}

bool MIRParser::ParseExprTwoOperand(BaseNode *&opnd0, BaseNode *&opnd1)  // (<opnd0>, <opnd1>)
{
  if (lexer.GetTokenKind() != TK_lparen) {
    Error("expect ( parsing operand parsing unary ");
    return false;
  }
  lexer.NextToken();

  if (!ParseExpression(opnd0)) {
    return false;
  }
  if (lexer.GetTokenKind() != TK_coma) {
    Error("expect , between two operands but get ");
    return false;
  }
  lexer.NextToken();
  if (!ParseExpression(opnd1)) {
    return false;
  }

  if (lexer.GetTokenKind() != TK_rparen) {
    Error("expect ) parsing operand parsing unary ");
    return false;
  }

  return true;
}

bool MIRParser::ParseExprNaryOperand(MapleVector<BaseNode *> &opndvec)  // parse (<opnd0>, <opnd1>...)
{
  if (lexer.GetTokenKind() != TK_lparen) {
    Error("expect ( parsing operand parsing nary operands ");
    return false;
  }
  TokenKind tk = lexer.NextToken();
  while (tk != TK_rparen) {
    BaseNode *opnd = nullptr;
    if (!ParseExpression(opnd)) {
      Error("expect expression parsing nary operands ");
      return false;
    }
    opndvec.push_back(opnd);
    tk = lexer.GetTokenKind();
    if (tk == TK_coma) {
      tk = lexer.NextToken();
    }
  }

  return true;
}

bool MIRParser::ParseDeclaredSt(StIdx &stIdx) {
  TokenKind varTk = lexer.GetTokenKind();
  GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName(lexer.GetName());

  if (strIdx == GStrIdx(0)) {
    Error("symbol not declared ");
    return false;
  }
  if (varTk == TK_gname) {
    stIdx = GlobalTables::GetGsymTable().GetStIdxFromStrIdx(strIdx);
    if (stIdx.FullIdx() == 0) {
      Error("global symbol not declared ");
      return false;
    }
  } else if (varTk == TK_lname) {
    stIdx = mod.CurFunction()->symTab->GetStIdxFromStrIdx(strIdx);
    if (stIdx.FullIdx() == 0) {
      Error("local symbol not declared ");
      return false;
    }
  } else {
    Error("expect global/local name but get ");
    return false;
  }
  return true;
}

bool MIRParser::ParseDeclaredFunc(PUIdx &puIdx) {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetStrIdxFromName(lexer.GetName());
  if (strIdx == GStrIdx(0)) {
    Error("symbol not declared ");
    return false;
  }
  StIdx stIdx = GlobalTables::GetGsymTable().GetStIdxFromStrIdx(strIdx);
  if (stIdx.FullIdx() == 0) {
    Error("function symbol not declared ");
    return false;
  }
  MIRSymbol *st = GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx());
  if (st->sKind != kStFunc) {
    Error("function name not declared as function");
    return false;
  }
  MIRFunction *func = st->value.mirFunc;
  puIdx = func->puIdx;
  st->appearsincode = true;
  return true;
}

bool MIRParser::ParseExprDread(BaseNode *&expr) {
  if (lexer.GetTokenKind() != TK_dread) {
    Error("expect dread but get ");
    return false;
  }
  AddrofNode *dexpr = mod.CurFuncCodeMemPool()->New<AddrofNode>(OP_dread);
  expr = dexpr;

  lexer.NextToken();

  TyIdx tyIdx(0);
  bool parseRet = ParsePrimType(tyIdx);
  if (tyIdx == TyIdx(0) || !parseRet) {
    Error("expect primitive type but get ");
    return false;
  }
  expr->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;

  StIdx stIdx;

  if (!ParseDeclaredSt(stIdx)) {
    return false;
  }
  if (stIdx.FullIdx() == 0) {
    Error("expect a symbol ParseExprDread failed");
    return false;
  }
  dexpr->stIdx = stIdx;

  TokenKind endtk = lexer.NextToken();
  if (endtk == TK_intconst) {
    dexpr->fieldID = lexer.GetTheIntVal();
    lexer.NextToken();
  } else if (!IsDelimitationTK(endtk)) {
    Error("expect , or ) delimitation token but get ");
    return false;
  } else {
    dexpr->fieldID = 0;
  }

  if (mod.srcLang == kSrcLangJava && !dexpr->CheckNode(&mod)) {
    Error("dread is not legal");
    return false;
  }

  return true;
}

bool MIRParser::ParseExprRegread(BaseNode *&expr) {
  RegreadNode *regread = mod.CurFuncCodeMemPool()->New<RegreadNode>();
  expr = regread;
  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    return false;
  }
  if (tyIdx == TyIdx(0)) {
    Error("expect primitive type but get ");
    return false;
  }
  expr->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;
  if (lexer.GetTokenKind() == TK_specialreg) {
    return ParseSpecialReg(regread->regIdx);
  }
  if (lexer.GetTokenKind() == TK_preg) {
    return ParsePseudoreg(expr->primType, regread->regIdx);
  }
  Error("expect special or pseudo register but get ");
  return false;
}

bool MIRParser::ParserVectorValue(MIRConst *&mirconst, MIRType *type) {
  PrimType ptp = type->primType;
  if (IsPrimitiveVectorInt(ptp)) {
    if (lexer.GetTokenKind() != TK_lbrack) {
      Error("vector value should start with [");
      return false;
    }
    std::vector<int64> elemVec;
    TokenKind tk = lexer.NextToken();
    if (lexer.GetTokenKind() != TK_intconst) {
      Error("expect const int");
      return false;
    }
    do {
      elemVec.push_back(lexer.GetTheIntVal());
      tk = lexer.NextToken();
      if (tk != TK_coma && tk!= TK_rbrack) {
        Error("expect , or ]");
      }
      if (tk == TK_coma) {
        tk = lexer.NextToken();
        if (tk != TK_intconst) {
          Error("expect const int after ,");
          return false;
        }
      }
    } while (tk == TK_intconst);
    uint32 vecSize = elemVec.size();
    CHECK_FATAL(vecSize == 4 || vecSize == 8 || vecSize == 16, "wrong size");
    MIRVectorIntConst *vecIntCon = mod.memPool->New<MIRVectorIntConst>(vecSize, type);
    for (uint32 i = 0; i < vecSize; i++) {
      vecIntCon->AddValue(elemVec[i], i);
    }
    mirconst = vecIntCon;
  } else {
    CHECK_FATAL(false, "NYI");
  }
  return true;
}

bool MIRParser::ParseExprConstval(BaseNode *&expr) {
  ConstvalNode *exprconst = mod.CurFuncCodeMemPool()->New<ConstvalNode>();
  TokenKind typeTk = lexer.NextToken();

  if (!IsPrimitiveType(typeTk)) {
    Error("expect type for constVal but get ");
    return false;
  }
  exprconst->primType = GetPrimitiveType(typeTk);

  lexer.NextToken();
  MIRConst *constVal = nullptr;
  if (IsPrimitiveVector(exprconst->primType)) {
    if(!ParserVectorValue(constVal, GlobalTables::GetTypeTable().typeTable.at(static_cast<uint32>(exprconst->primType)))) {
      Error("expect vector type but get");
      return false;
    }
  } else if (!ParseScalarValue(constVal, GlobalTables::GetTypeTable().typeTable.at(static_cast<uint32>(exprconst->primType)))) {
    Error("expect scalar type but get ");
    return false;
  }
  exprconst->constVal = constVal;
  expr = exprconst;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprConststr(BaseNode *&expr) {
  ConststrNode *strconst = mod.CurFuncCodeMemPool()->New<ConststrNode>();
  TokenKind tk = lexer.NextToken();

  if (!IsPrimitiveType(tk)) {
    Error("expect primitive type for conststr but get ");
    return false;
  }
  strconst->primType = GetPrimitiveType(tk);
  if (!IsAddress(strconst->primType)) {
    Error("expect primitive type for conststr but get ");
    return false;
  }

  tk = lexer.NextToken();
  if (tk != TK_string) {
    Error("expect string literal for conststr but get ");
    return false;
  }
  strconst->strIdx = GlobalTables::GetUStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  expr = strconst;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprConststr16(BaseNode *&expr) {
  Conststr16Node *str16const = mod.CurFuncCodeMemPool()->New<Conststr16Node>();
  TokenKind tk = lexer.NextToken();

  if (!IsPrimitiveType(tk)) {
    Error("expect primitive type for conststr16 but get ");
    return false;
  }
  str16const->primType = GetPrimitiveType(tk);
  if (!IsAddress(str16const->primType)) {
    Error("expect primitive type for conststr16 but get ");
    return false;
  }

  tk = lexer.NextToken();
  if (tk != TK_string) {
    Error("expect string literal for conststr16 but get ");
    return false;
  }
  // UTF-16 strings in mpl files are presented as UTF-8 strings
  // to keep the printable chars in ascii form
  // so we need to do a UTF8ToUTF16 conversion
  std::string str = lexer.GetName();
  std::u16string str16;
  NameMangler::UTF8ToUTF16(str16, str);
  str16const->strIdx = GlobalTables::GetU16StrTable().GetOrCreateStrIdxFromName(str16);
  expr = str16const;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprSizeoftype(BaseNode *&expr) {
  SizeoftypeNode *exprsizeoftype = mod.CurFuncCodeMemPool()->New<SizeoftypeNode>();
  TokenKind typeTk = lexer.NextToken();

  if (!IsPrimitiveType(typeTk)) {
    Error("expect type for constVal but get ");
    return false;
  }
  exprsizeoftype->primType = GetPrimitiveType(typeTk);

  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("expect type parsing array but get ");
    return false;
  }
  exprsizeoftype->tyIdx = tyIdx;
  expr = exprsizeoftype;
  return true;
}

bool MIRParser::ParseExprFieldsDist(BaseNode* &expr) {
  TokenKind typeTk = lexer.NextToken();
  if (!IsPrimitiveType(typeTk)) {
    Error("expect type for GetConstVal but get ");
    return false;
  }
  FieldsDistNode *node = mod.CurFuncCodeMemPool()->New<FieldsDistNode>();
  node->primType = GetPrimitiveType(typeTk);
  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("expect type parsing array but get ");
    return false;
  }
  node->SetTyIdx(tyIdx);
  TokenKind tk = lexer.GetTokenKind();
  if (tk != TK_intconst) {
    Error("expect type int but get");
    return false;
  }
  node->SetFiledID1(lexer.GetTheIntVal());
  tk = lexer.NextToken();
  if (tk != TK_intconst) {
    Error("expect type int but get");
    return false;
  }
  node->SetFiledID2(lexer.GetTheIntVal());
  lexer.NextToken();
  expr = node;
  return true;
}

bool MIRParser::ParseExprBinary(BaseNode *&expr) {
  Opcode opcode = GetBinaryOp(lexer.GetTokenKind());
  if (opcode == kOpUndef) {
    Error("expect add operator but get ");
    return false;
  }
  BinaryNode *addexpr = mod.CurFuncCodeMemPool()->New<BinaryNode>(opcode);
  if (!IsPrimitiveType(lexer.NextToken())) {
    Error("expect type parsing binary operator but get ");
    return false;
  }
  addexpr->primType = GetPrimitiveType(lexer.GetTokenKind());
  lexer.NextToken();
  BaseNode *opnd0 = nullptr;
  BaseNode *opnd1 = nullptr;
  if (!ParseExprTwoOperand(opnd0, opnd1)) {
    return false;
  }
  addexpr->bOpnd[0] = opnd0;
  addexpr->bOpnd[1] = opnd1;
  expr = addexpr;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprCompare(BaseNode *&expr) {
  Opcode opcode = GetBinaryOp(lexer.GetTokenKind());
  CompareNode *addexpr = mod.CurFuncCodeMemPool()->New<CompareNode>(opcode);
  if (!IsPrimitiveType(lexer.NextToken())) {
    Error("expect type parsing compare operator but get ");
    return false;
  }
  addexpr->primType = GetPrimitiveType(lexer.GetTokenKind());
  if (!IsPrimitiveType(lexer.NextToken())) {
    Error("expect operand type parsing compare operator but get ");
    return false;
  }
  addexpr->opndType = GetPrimitiveType(lexer.GetTokenKind());
  lexer.NextToken();
  BaseNode *opnd0 = nullptr;
  BaseNode *opnd1 = nullptr;
  if (!ParseExprTwoOperand(opnd0, opnd1)) {
    return false;
  }
  addexpr->bOpnd[0] = opnd0;
  addexpr->bOpnd[1] = opnd1;
  expr = addexpr;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprDepositbits(BaseNode *&expr) {
  // syntax: depositbits <int-type> <bitsOffset> <bitsSize> (<opnd0>, <opnd1>)
  if (lexer.GetTokenKind() != TK_depositbits) {
    Error("expect depositbits but get ");
    return false;
  }
  DepositbitsNode *dpsbnode = mod.CurFuncCodeMemPool()->New<DepositbitsNode>();
  expr = dpsbnode;
  PrimType primType = GetPrimitiveType(lexer.NextToken());
  if (!IsPrimitiveInteger(primType)) {
    Error("expect <int-type> but get ");
    return false;
  }
  dpsbnode->primType = primType;
  if (lexer.NextToken() != TK_intconst) {
    Error("expect bitsOffset but get ");
    return false;
  }
  dpsbnode->bitsOffset = lexer.GetTheIntVal();

  if (lexer.NextToken() != TK_intconst) {
    Error("expect bitsSize but get ");
    return false;
  }
  dpsbnode->bitsSize = lexer.GetTheIntVal();
  lexer.NextToken();

  BaseNode *opnd0 = nullptr;
  BaseNode *opnd1 = nullptr;
  if (!ParseExprTwoOperand(opnd0, opnd1)) {
    Error("ParseExprDepositbits when parsing two operand");
    return false;
  }
  dpsbnode->bOpnd[0] = opnd0;
  dpsbnode->bOpnd[1] = opnd1;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprIreadIaddrof(IreadNode *expr) {
  // syntax : iread/iaddrof <prim-type> <type> <field-id> (<addr-expr>)
  if (!IsPrimitiveType(lexer.NextToken())) {
    Error("expect primitive type but get ");
    return false;
  }
  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    return false;
  }
  expr->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;
  tyIdx = TyIdx(0);
  if (!ParseDerivedType(tyIdx)) {
    Error("ParseExprIreadIaddrof failed when paring derived type");
    return false;
  }
  expr->tyIdx = tyIdx;
  if (lexer.GetTokenKind() == TK_intconst) {
    expr->fieldID = lexer.GetTheIntVal();
    lexer.NextToken();
  }
  BaseNode *opnd0 = nullptr;
  if (!ParseExprOneOperand(opnd0)) {
    return false;
  }
  expr->uOpnd = opnd0;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprIread(BaseNode *&expr) {
  // syntax : iread <prim-type> <type> <field-id> (<addr-expr>)
  IreadNode *iexpr = mod.CurFuncCodeMemPool()->New<IreadNode>(OP_iread);
  if (!ParseExprIreadIaddrof(iexpr)) {
    Error("ParseExprIread failed when trying to parse addof");
    return false;
  }
  expr = iexpr;
  return true;
}

bool MIRParser::ParseExprIaddrof(BaseNode *&expr) {
  // syntax : iaddrof <prim-type> <type> <field-id> (<addr-expr>)
  IreadNode *iexpr = mod.CurFuncCodeMemPool()->New<IreadNode>(OP_iaddrof);
  if (!ParseExprIreadIaddrof(iexpr)) {
    Error("ParseExprIaddrof failed when trying to parse addof");
    return false;
  }
  expr = iexpr;
  return true;
}

bool MIRParser::ParseExprIreadoff(BaseNode *&expr) {
  // syntax : iread <prim-type> <offset> (<addr-expr>)
  IreadoffNode *ireadoff = mod.CurFuncCodeMemPool()->New<IreadoffNode>();
  expr = ireadoff;
  if (!IsPrimitiveType(lexer.NextToken())) {
    Error("expect primitive type but get ");
    return false;
  }
  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    return false;
  }
  ireadoff->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;
  if (!IsPrimitiveScalar(ireadoff->primType)) {
    Error("only scalar types allowed for ireadoff");
    return false;
  }

  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect offset but get ");
    return false;
  }
  ireadoff->offset = lexer.GetTheIntVal();
  lexer.NextToken();

  BaseNode *opnd = nullptr;
  if (!ParseExprOneOperand(opnd)) {
    Error("ParseExprIreadoff when paring one operand");
    return false;
  }
  ireadoff->uOpnd = opnd;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprIreadFPoff(BaseNode *&expr) {
  // syntax : iread <prim-type> <offset>
  IreadFPoffNode *ireadoff = mod.CurFuncCodeMemPool()->New<IreadFPoffNode>();
  expr = ireadoff;
  if (!IsPrimitiveType(lexer.NextToken())) {
    Error("expect primitive type but get ");
    return false;
  }
  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    return false;
  }
  ireadoff->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;
  if (!IsPrimitiveScalar(ireadoff->primType)) {
    Error("only scalar types allowed for ireadoff");
    return false;
  }

  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect offset but get ");
    return false;
  }
  ireadoff->offset = lexer.GetTheIntVal();
  lexer.NextToken();

  return true;
}

bool MIRParser::ParseExprAddrof(BaseNode *&expr) {
  // syntax: addrof <prim-type> <var-name> <field-id>
  AddrofNode *addrofnode = mod.CurFuncCodeMemPool()->New<AddrofNode>(OP_addrof);
  expr = addrofnode;
  if (lexer.GetTokenKind() != TK_addrof) {
    Error("expect addrof but get ");
    return false;
  }
  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    Error("expect primitive type but get ");
    return false;
  }
  addrofnode->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;

  StIdx stIdx;

  if (!ParseDeclaredSt(stIdx)) {
    return false;
  }
  if (stIdx.FullIdx() == 0) {
    Error("expect symbol ParseExprAddroffunc");
    return false;
  }
  addrofnode->stIdx = stIdx;

  TokenKind tk = lexer.NextToken();
  if (IsDelimitationTK(tk)) {
    addrofnode->fieldID = 0;
  } else if (tk == TK_intconst) {
    addrofnode->fieldID = lexer.GetTheIntVal();
    lexer.NextToken();
  } else {
    addrofnode->fieldID = 0;
  }
  return true;
}

bool MIRParser::ParseExprAddroffunc(BaseNode *&expr) {
  AddroffuncNode *addroffuncnode = mod.CurFuncCodeMemPool()->New<AddroffuncNode>();
  expr = addroffuncnode;
  TokenKind tk = lexer.NextToken();
  if (tk != TK_a32 && tk != TK_a64 && tk != TK_ptr) {
    Error("expect address primitive type but get ");
    return false;
  }
  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    Error("ParseExprAddroffunc failed when parsing primitive type");
    return false;
  }
  addroffuncnode->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;
  if (lexer.GetTokenKind() != TK_fname) {
    Error("expect function name but get ");
    return false;
  }
  PUIdx pidx;
  if (!ParseDeclaredFunc(pidx)) {
    if (mod.flavor < kMmpl) {
      Error("expect .mmpl file");
      return false;
    }
    pidx = EnterUndeclaredFunction();
  }
  addroffuncnode->puIdx = pidx;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprAddroflabel(BaseNode *&expr) {
  // syntax: addroflabel <prim-type> <label>
  AddroflabelNode *addroflabelnode = mod.CurFuncCodeMemPool()->New<AddroflabelNode>();
  expr = addroflabelnode;
  TokenKind tk = lexer.NextToken();
  if (tk != TK_a32 && tk != TK_a64 && tk != TK_ptr) {
    Error("expect address primitive type but get ");
    return false;
  }
  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    Error("ParseExprAddroflabel failed");
    return false;
  }
  addroflabelnode->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;

  if (lexer.GetTokenKind() != TK_label) {
    Error("expect label but get ");
    return false;
  }
  LabelIdx lblidx = mod.CurFunction()->GetOrCreateLablidxFromName(lexer.GetName());
  addroflabelnode->offset = lblidx;
  mod.CurFunction()->labelTab->addrTakenLabels.insert(lblidx);
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprUnary(BaseNode *&expr) {
  // syntax op <prim-type> <label>
  Opcode op = GetUnaryOp(lexer.GetTokenKind());
  if (op == kOpUndef) {
    Error("expect unary op but get ");
    return false;
  }
  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    Error("expect primitive parsing unary operator ");
    return false;
  }
  UnaryNode *unarynode = mod.CurFuncCodeMemPool()->New<UnaryNode>(op);
  expr = unarynode;
  unarynode->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;
  BaseNode *opnd = nullptr;
  if (!ParseExprOneOperand(opnd)) {
    Error("parsing unary wrong ");
    return false;
  }
  unarynode->uOpnd = opnd;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprSTACKMalloc(BaseNode *&expr) {
  // syntax op <prim-type> <type>
  if (lexer.GetTokenKind() != TK_stackmalloc) {
    Error("expect stackmalloc but get ");
    return false;
  }
  lexer.NextToken();
  TyIdx ptyidx(0);
  if (IsPrimitiveType(lexer.GetTokenKind())) {
    if (!ParsePrimType(ptyidx)) {
      return false;
    }
  }
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("expect type parsing unary operator ");
    return false;
  }
  GCMallocNode *mallocnode = mod.CurFuncCodeMemPool()->New<GCMallocNode>(OP_stackmalloc);
  expr = mallocnode;
  mallocnode->primType = GlobalTables::GetTypeTable().typeTable[ptyidx.GetIdx()]->primType;
  mallocnode->tyIdx = tyIdx;
  return true;
}

bool MIRParser::ParseExprGCMalloc(BaseNode *&expr) {
  // syntax op <prim-type> <type>
  TokenKind tk = lexer.GetTokenKind();
  if (tk != TK_gcmalloc && tk != TK_gcpermalloc) {
    Error("expect gcmalloc or gcpermalloc but get ");
    return false;
  }
  Opcode op = (tk == TK_gcmalloc) ? OP_gcmalloc : OP_gcpermalloc;
  lexer.NextToken();
  TyIdx ptyidx(0);
  if (IsPrimitiveType(lexer.GetTokenKind())) {
    if (!ParsePrimType(ptyidx)) {
      return false;
    }
  }
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("expect type parsing unary operator ");
    return false;
  }
  GCMallocNode *mallocnode = mod.CurFuncCodeMemPool()->New<GCMallocNode>(op);
  expr = mallocnode;
  mallocnode->primType = GlobalTables::GetTypeTable().typeTable.at(ptyidx.GetIdx())->primType;
  mallocnode->tyIdx = tyIdx;
  return true;
}

bool MIRParser::ParseExprSTACKJarray(BaseNode *&expr) {
  // syntax op <prim-type> <java-array-type> <label>
  Opcode op = GetUnaryOp(lexer.GetTokenKind());
  if (op != OP_stackmallocjarray) {
    Error("expect stackmallocjarray but get ");
    return false;
  }
  TyIdx ptyidx(0);
  lexer.NextToken();
  if (IsPrimitiveType(lexer.GetTokenKind())) {
    if (!ParsePrimType(ptyidx)) {
      return false;
    }
  }
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("expect primitive parsing unary operator ");
    return false;
  }
  JarrayMallocNode *jarraynode = mod.CurFuncCodeMemPool()->New<JarrayMallocNode>(op);
  expr = jarraynode;
  CHECK(ptyidx.GetIdx() < GlobalTables::GetTypeTable().typeTable.size(), "index out of range in MIRParser::ParseExprSTACKJarray");
  jarraynode->primType = GlobalTables::GetTypeTable().typeTable[ptyidx.GetIdx()]->primType;
  jarraynode->tyIdx = tyIdx;
  BaseNode *opnd = nullptr;
  if (!ParseExprOneOperand(opnd)) {
    Error("parsing unary wrong ");
    return false;
  }
  jarraynode->uOpnd = opnd;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprJarray(BaseNode *&expr) {
  // syntax op <prim-type> <java-array-type> <label>
  Opcode op = GetUnaryOp(lexer.GetTokenKind());
  if (op != OP_gcmallocjarray && op != OP_gcpermallocjarray) {
    Error("expect gcmallocjarray or gcpermallocjarray but get ");
    return false;
  }
  TyIdx ptyidx(0);
  lexer.NextToken();
  if (IsPrimitiveType(lexer.GetTokenKind())) {
    if (!ParsePrimType(ptyidx)) {
      return false;
    }
  }
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("expect primitive parsing unary operator ");
    return false;
  }
  JarrayMallocNode *jarraynode = mod.CurFuncCodeMemPool()->New<JarrayMallocNode>(op);
  expr = jarraynode;
  jarraynode->primType = GlobalTables::GetTypeTable().typeTable.at(ptyidx.GetIdx())->primType;
  jarraynode->tyIdx = tyIdx;
  BaseNode *opnd = nullptr;
  if (!ParseExprOneOperand(opnd)) {
    Error("parsing unary wrong ");
    return false;
  }
  jarraynode->uOpnd = opnd;
  lexer.NextToken();
  return true;
}

// parse extractbits, sext, zext
bool MIRParser::ParseExprExtractbits(BaseNode *&expr) {
  // extractbits <int-type> <bitsOffset> <bitsSize> (<opnd0>)
  Opcode op = GetUnaryOp(lexer.GetTokenKind());
  if (op == kOpUndef) {
    Error("expect unary op but get ");
    return false;
  }
  ExtractbitsNode *extrctnode = mod.CurFuncCodeMemPool()->New<ExtractbitsNode>(op);
  expr = extrctnode;
  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    Error("expect int type after extractbits after");
    return false;
  }
  PrimType primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;
  if (!IsPrimitiveInteger(primType)) {
    Error("sematical error expect int type for extractbits");
    return false;
  }
  extrctnode->primType = primType;

  if (op == OP_extractbits) {
    if (lexer.GetTokenKind() != TK_intconst) {
      Error("expect bitsOffset but get ");
      return false;
    }
    extrctnode->bitsOffset = lexer.GetTheIntVal();
    lexer.NextToken();
  }

  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect bitsSize but get ");
    return false;
  }
  extrctnode->bitsSize = lexer.GetTheIntVal();
  lexer.NextToken();

  BaseNode *opnd = nullptr;
  if (!ParseExprOneOperand(opnd)) {
    Error("ParseExprExtractbits failed");
    return false;
  }
  extrctnode->uOpnd = opnd;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprTyconvert(BaseNode *&expr) {
  Opcode op = GetConvertOp(lexer.GetTokenKind());
  if (op == kOpUndef) {
    Error("expect covertion operator but get ");
    return false;
  }
  TypeCvtNode *cvtnode = mod.CurFuncCodeMemPool()->New<TypeCvtNode>(op);
  expr = static_cast<BaseNode *>(cvtnode);

  PrimType totype = GetPrimitiveType(lexer.NextToken());
  if (totype == kPtyInvalid) {
    Error("expect to-type parsing conversion");
    return false;
  }
  cvtnode->primType = totype;

  lexer.NextToken();
  PrimType fromtype = GetPrimitiveType(lexer.GetTokenKind());
  if (fromtype == kPtyInvalid) {
    Error("expect type parsing conversion ");
    return false;
  }
  cvtnode->fromPrimType = fromtype;
  lexer.NextToken();

  BaseNode *opnd = nullptr;
  if (!ParseExprOneOperand(opnd)) {
    return false;
  }
  cvtnode->uOpnd = opnd;
  if (op == OP_retype) {
    cvtnode->fromPrimType = opnd->primType;
  }

  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprRetype(BaseNode *&expr) {
  RetypeNode *cvtnode = mod.CurFuncCodeMemPool()->New<RetypeNode>();
  expr = static_cast<BaseNode *>(cvtnode);

  PrimType totype = GetPrimitiveType(lexer.NextToken());
  if (totype == kPtyInvalid) {
    Error("expect to-type parsing conversion");
    return false;
  }
  cvtnode->primType = totype;

  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParseDerivedType(tyIdx)) {
    Error("ParseExprRetype failed when parsing derived type ");
    return false;
  }
  cvtnode->tyIdx = tyIdx;
  BaseNode *opnd = nullptr;
  if (!ParseExprOneOperand(opnd)) {
    return false;
  }
  cvtnode->uOpnd = opnd;
  cvtnode->fromPrimType = opnd->primType;
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprTernary(BaseNode *&expr) {
  if (lexer.GetTokenKind() != TK_select) {
    Error("expect select but get ");
    return false;
  }
  TernaryNode *ternarynode = mod.CurFuncCodeMemPool()->New<TernaryNode>(OP_select);
  expr = ternarynode;
  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    Error("expect primtype type but get ");
    return false;
  }
  ternarynode->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;
  MapleVector<BaseNode *> opndvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
  if (!ParseExprNaryOperand(opndvec)) {
    Error("ParseExprTernary failed");
    return false;
  }
  if (opndvec.size() != 3) {  // expect number of operands to be 3
    Error("expect 3 operands for ternary operator ");
    return false;
  }
  ternarynode->topnd[0] = opndvec[0];
  ternarynode->topnd[1] = opndvec[1];
  ternarynode->topnd[2] = opndvec[2];
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprArray(BaseNode *&expr) {
  // syntax: array <addr-type> <array-type> (<opnd0>, <opnd1>, . . . , <opndn>)
  ArrayNode *arraynode = mod.CurFuncCodeMemPool()->New<ArrayNode>(&mod);
  expr = arraynode;
  if (lexer.GetTokenKind() != TK_array) {
    Error("expect array but get ");
    return false;
  }
  lexer.NextToken();
  if (lexer.GetTokenKind() == TK_intconst) {
    if (lexer.GetTheIntVal() == 1) {
      arraynode->boundsCheck = true;
    } else if (lexer.GetTheIntVal() == 0) {
      arraynode->boundsCheck = false;
    } else {
      Error("expect boundsCheck(0/1) but get ");
      return false;
    }
  }
  lexer.NextToken();
  TyIdx tyIdx;
  if (!ParsePrimType(tyIdx)) {
    Error("expect address type but get ");
    return false;
  }
  arraynode->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;
  tyIdx = TyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("expect type parsing array but get ");
    return false;
  }
  arraynode->tyIdx = tyIdx;

  // number of operand can not be zero
  MapleVector<BaseNode *> opndvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
  if (!ParseExprNaryOperand(opndvec)) {
    Error("ParseExprArray failed");
    return false;
  }
  if (opndvec.size() == 0) {
    Error("sematic error operands number of array expression is 0 ");
    return false;
  }
  arraynode->nOpnd = opndvec;
  arraynode->numOpnds = opndvec.size();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseIntrinsicId(IntrinsicopNode *intrnopnode) {
  MIRIntrinsicID intrinid = GetIntrinsicId(lexer.GetTokenKind());
  if (intrinid <= INTRN_UNDEFINED || intrinid >= INTRN_LAST) {
    Error("wrong intrinsic id ");
    return false;
  }
  intrnopnode->intrinsic = intrinid;
  return true;
}

bool MIRParser::ParseExprIntrinsicop(BaseNode *&expr) {
  // syntax: intrinsicop <prim-type> <intrinsic> (<opnd0>, ..., <opndn>)
  if (lexer.GetTokenKind() != TK_intrinsicop) {
    Error("expect intrinsicop but get ");
    return false;
  }
  IntrinsicopNode *intrnopnode = mod.CurFuncCodeMemPool()->New<IntrinsicopNode>(&mod, OP_intrinsicop);
  expr = intrnopnode;
  lexer.NextToken();

  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    Error("ParseExprIntrinsicop failed when parsing type");
    return false;
  }

  intrnopnode->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;
  if (!ParseIntrinsicId(intrnopnode)) {
    return false;
  }

  // number of operand can not be zero
  lexer.NextToken();
  MapleVector<BaseNode *> opndvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
  if (!ParseExprNaryOperand(opndvec)) {
    Error("ParseExprIntrinsicop failed");
    return false;
  }
  intrnopnode->nOpnd = opndvec;
  intrnopnode->numOpnds = opndvec.size();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseExprIntrinsicopwithtype(BaseNode *&expr) {
  if (lexer.GetTokenKind() != TK_intrinsicopwithtype) {
    Error("expect intrinsicopwithtype but get ");
    return false;
  }
  IntrinsicopNode *intrnopnode = mod.CurFuncCodeMemPool()->New<IntrinsicopNode>(&mod, OP_intrinsicopwithtype);
  expr = intrnopnode;
  lexer.NextToken();

  TyIdx tyIdx(0);
  if (!ParsePrimType(tyIdx)) {
    Error("ParseExprIntrinsicopwithtype failed when parsing type");
    return false;
  }
  intrnopnode->primType = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx())->primType;

  tyIdx = TyIdx(0);
  if (!ParseDerivedType(tyIdx)) {
    Error("ParseExprIntrinsicopwithtype failed when parsing derived type ");
    return false;
  }
  intrnopnode->tyIdx = tyIdx;

  if (!ParseIntrinsicId(intrnopnode)) {
    return false;
  }

  // number of operand can not be zero
  lexer.NextToken();
  MapleVector<BaseNode *> opndvec(mod.CurFuncCodeMemPoolAllocator()->Adapter());
  if (!ParseExprNaryOperand(opndvec)) {
    Error("ParseExprIntrinsicopwithtype failed");
    return false;
  }
  intrnopnode->nOpnd = opndvec;
  intrnopnode->numOpnds = opndvec.size();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseScalarValue(MIRConst *&stype, MIRType *type) {
  PrimType ptp = type->primType;
  if (IsPrimitiveInteger(ptp) || IsPrimitiveDynType(ptp) || ptp == PTY_gen) {
    if (lexer.GetTokenKind() == TK_intconst) {
      stype = mod.memPool->New<MIRIntConst>(lexer.GetTheIntVal(), type);
    } else if (lexer.GetTokenKind() == TK_doubleconst) {
      stype = mod.memPool->New<MIRDoubleConst>(lexer.GetTheDoubleVal(), type);
    } else {
      Error("constant value incompatible with integer type at ");
      return false;
    }
  } else if (ptp == PTY_f32) {
    if (lexer.GetTokenKind() != TK_floatconst) {
      Error("constant value incompatible with single-precision float type at ");
      return false;
    }
    MIRFloatConst *fconst = GlobalTables::GetFpConstTable().GetOrCreateFloatConst(lexer.GetTheFloatVal());
    stype = fconst;
  } else if (ptp == PTY_f64) {
    if (lexer.GetTokenKind() != TK_doubleconst && lexer.GetTokenKind() != TK_intconst) {
      Error("constant value incompatible with double-precision float type at ");
      return false;
    }
    MIRDoubleConst *dconst = GlobalTables::GetFpConstTable().GetOrCreateDoubleConst(lexer.GetTheDoubleVal());
    stype = dconst;
  }
#ifdef DYNAMICLANG
#endif
  else {
    return false;
  }
  return true;
}

bool MIRParser::ParseConstAddrLeafExpr(MIRConst *&cexpr, MIRType *type) {
  BaseNode *expr = nullptr;
  if (!ParseExpression(expr)) {
    return false;
  }
  if (expr->op != OP_addrof && expr->op != OP_addroffunc && expr->op != OP_addroflabel && expr->op != OP_conststr && expr->op != OP_conststr16) {
    Error("ParseConstAddrLeafExpr expects one of OP_addrof, OP_addroffunc, OP_conststr and OP_conststr16");
    return false;
  }
  if (expr->op == OP_addrof) {
    AddrofNode *anode = static_cast<AddrofNode *>(expr);
    MIRFunction *currfn = static_cast<MIRFunction *>(mod.CurFunction());
    MIRSymbol *var = currfn->GetLocalOrGlobalSymbol(anode->stIdx);
    var->needForwDecl = 1;
    mod.someSymbolNeedForwDecl = true;
    TyIdx ptyidx = var->GetTyIdx();
    MIRPtrType ptrtype(ptyidx, (mod.IsJavaModule() ? PTY_ref : PTY_ptr));
    ptyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&ptrtype);
    MIRType *exprty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptyidx);
    int32 ofst = 0;
    if (lexer.GetTokenKind() == TK_lparen) {
      lexer.NextToken();
      if (lexer.GetTokenKind() != TK_intconst) {
        Error("ParseConstAddrLeafExpr: wrong offset specification for addrof");
        return false;
      } else {
        ofst = lexer.GetTheIntVal();
      }
      lexer.NextToken();
      if (lexer.GetTokenKind() != TK_rparen) {
        Error("ParseConstAddrLeafExpr expects closing paren after offset value for addrof");
        return false;
      }
      lexer.NextToken();
    }
    cexpr = mod.CurFunction()->dataMemPool->New<MIRAddrofConst>(anode->stIdx, anode->fieldID, exprty, ofst);
  } else if (expr->op == OP_addroffunc) {
    AddroffuncNode *aof = static_cast<AddroffuncNode *>(expr);
    MIRFunction *f = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(aof->puIdx);
    MIRSymbol *fname = f->GetFuncSymbol();
    TyIdx ptyidx = fname->GetTyIdx();
    MIRPtrType ptrtype(ptyidx);
    ptyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&ptrtype);
    MIRType *exprty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptyidx);
    cexpr = mod.CurFunction()->dataMemPool->New<MIRAddroffuncConst>(aof->puIdx, exprty);
  } else if (expr->op == OP_addroflabel) {
    AddroflabelNode *aol = static_cast<AddroflabelNode *>(expr);
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr));
    cexpr = mod.CurFunction()->dataMemPool->New<MIRLblConst>(aol->offset, mod.CurFunction()->puIdx, mirtype);
  } else if (expr->op == OP_conststr) {
    ConststrNode *cs = static_cast<ConststrNode *>(expr);
    UStrIdx strIdx = cs->strIdx;
    TyIdx ptyidx = GlobalTables::GetTypeTable().typeTable[PTY_u8]->tyIdx;
    MIRPtrType ptrtype(ptyidx);
    ptyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&ptrtype);
    MIRType *exprty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptyidx);
    cexpr = mod.memPool->New<MIRStrConst>(strIdx, exprty);
  } else {
    Conststr16Node *cs = static_cast<Conststr16Node *>(expr);
    U16StrIdx strIdx = cs->strIdx;
    TyIdx ptyidx = GlobalTables::GetTypeTable().typeTable.at(PTY_u16)->tyIdx;
    MIRPtrType ptrtype(ptyidx);
    ptyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&ptrtype);
    MIRType *exprty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptyidx);
    cexpr = mod.memPool->New<MIRStr16Const>(strIdx, exprty);
  }
  return true;
}

bool MIRParser::ParseExpression(BaseNode *&expr) {
  TokenKind tk = lexer.GetTokenKind();
  std::map<TokenKind, MIRParser::FuncPtrParseExpr>::iterator itFuncPtr = funcPtrMapForParseExpr.find(tk);
  if (itFuncPtr == funcPtrMapForParseExpr.end()) {
    Error("expect expression but get ");
    return false;
  } else {
    if (!(this->*(itFuncPtr->second))(expr)) {
      return false;
    }
  }
  return true;
}

std::map<TokenKind, MIRParser::FuncPtrParseExpr> MIRParser::InitFuncPtrMapForParseExpr() {
  std::map<TokenKind, MIRParser::FuncPtrParseExpr> funcPtrMap;
  funcPtrMap[TK_addrof] = &MIRParser::ParseExprAddrof;
  funcPtrMap[TK_addroffunc] = &MIRParser::ParseExprAddroffunc;
  funcPtrMap[TK_addroflabel] = &MIRParser::ParseExprAddroflabel;
  funcPtrMap[TK_abs] = &MIRParser::ParseExprUnary;
  funcPtrMap[TK_bnot] = &MIRParser::ParseExprUnary;
  funcPtrMap[TK_lnot] = &MIRParser::ParseExprUnary;
  funcPtrMap[TK_neg] = &MIRParser::ParseExprUnary;
  funcPtrMap[TK_recip] = &MIRParser::ParseExprUnary;
  funcPtrMap[TK_sqrt] = &MIRParser::ParseExprUnary;
  funcPtrMap[TK_alloca] = &MIRParser::ParseExprUnary;
  funcPtrMap[TK_malloc] = &MIRParser::ParseExprUnary;
  funcPtrMap[TK_gcmalloc] = &MIRParser::ParseExprGCMalloc;
  funcPtrMap[TK_gcpermalloc] = &MIRParser::ParseExprGCMalloc;
  funcPtrMap[TK_gcmallocjarray] = &MIRParser::ParseExprJarray;
  funcPtrMap[TK_gcpermallocjarray] = &MIRParser::ParseExprJarray;
  funcPtrMap[TK_sext] = &MIRParser::ParseExprExtractbits;
  funcPtrMap[TK_zext] = &MIRParser::ParseExprExtractbits;
  funcPtrMap[TK_extractbits] = &MIRParser::ParseExprExtractbits;
  funcPtrMap[TK_ceil] = &MIRParser::ParseExprTyconvert;
  funcPtrMap[TK_cvt] = &MIRParser::ParseExprTyconvert;
  funcPtrMap[TK_floor] = &MIRParser::ParseExprTyconvert;
  funcPtrMap[TK_round] = &MIRParser::ParseExprTyconvert;
  funcPtrMap[TK_trunc] = &MIRParser::ParseExprTyconvert;
  funcPtrMap[TK_retype] = &MIRParser::ParseExprRetype;
  funcPtrMap[TK_select] = &MIRParser::ParseExprTernary;
  funcPtrMap[TK_array] = &MIRParser::ParseExprArray;
  funcPtrMap[TK_intrinsicop] = &MIRParser::ParseExprIntrinsicop;
  funcPtrMap[TK_intrinsicopwithtype] = &MIRParser::ParseExprIntrinsicopwithtype;
  funcPtrMap[TK_constval] = &MIRParser::ParseExprConstval;
  funcPtrMap[TK_conststr] = &MIRParser::ParseExprConststr;
  funcPtrMap[TK_conststr16] = &MIRParser::ParseExprConststr16;
  funcPtrMap[TK_sizeoftype] = &MIRParser::ParseExprSizeoftype;
//funcPtrMap[TK_fieldsdist] = &MIRParser::ParseExprFieldsDist;
  funcPtrMap[TK_iaddrof] = &MIRParser::ParseExprIaddrof;
  funcPtrMap[TK_iread] = &MIRParser::ParseExprIread;
  funcPtrMap[TK_ireadoff] = &MIRParser::ParseExprIreadoff;
  funcPtrMap[TK_ireadfpoff] = &MIRParser::ParseExprIreadFPoff;
  funcPtrMap[TK_dread] = &MIRParser::ParseExprDread;
  funcPtrMap[TK_regread] = &MIRParser::ParseExprRegread;
  funcPtrMap[TK_add] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_ashr] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_band] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_bior] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_bxor] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_cand] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_cior] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_div] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_land] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_lior] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_lshr] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_max] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_min] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_mul] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_rem] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_shl] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_sub] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_CG_array_elem_add] = &MIRParser::ParseExprBinary;
  funcPtrMap[TK_cmp] = &MIRParser::ParseExprCompare;
  funcPtrMap[TK_cmpl] = &MIRParser::ParseExprCompare;
  funcPtrMap[TK_cmpg] = &MIRParser::ParseExprCompare;
  funcPtrMap[TK_eq] = &MIRParser::ParseExprCompare;
  funcPtrMap[TK_ge] = &MIRParser::ParseExprCompare;
  funcPtrMap[TK_gt] = &MIRParser::ParseExprCompare;
  funcPtrMap[TK_le] = &MIRParser::ParseExprCompare;
  funcPtrMap[TK_lt] = &MIRParser::ParseExprCompare;
  funcPtrMap[TK_ne] = &MIRParser::ParseExprCompare;
  funcPtrMap[TK_depositbits] = &MIRParser::ParseExprDepositbits;
  return funcPtrMap;
}

}  // namespace maple
