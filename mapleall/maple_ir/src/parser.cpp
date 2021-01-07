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

#include <climits>
#include <cstdlib>
#include <sstream>
#include <fstream>
#include "mir_parser.h"
#include "mir_function.h"
#include "name_mangler.h"
#include "opcode_info.h"
#include "mir_pragma.h"
#include "debug_info.h"
#include "bin_mplt.h"
#include "option.h"
#include "clone.h"

using namespace std;
namespace maple {
constexpr char kLexerStringSp[] = "SP";
constexpr char kLexerStringFp[] = "FP";
constexpr char kLexerStringGp[] = "GP";
constexpr char kLexerStringThrownval[] = "thrownval";
constexpr char kLexerStringRetval[] = "retval";
std::map<TokenKind, MIRParser::FuncPtrParseMIRForElem> MIRParser::funcPtrMapForParseMIR =
    MIRParser::InitFuncPtrMapForParseMIR();

MIRFunction *MIRParser::CreateDummyFunction() {
  MapleVector<std::pair<const char *, MIRType *>> arguments(mod.memPoolAllocator.Adapter());
  MIRSymbol *funcst = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("__$$__");
  funcst->SetNameStridx(strIdx);
  GlobalTables::GetGsymTable().AddToStringSymbolMap(funcst);
  funcst->storageClass = kScUnused;
  funcst->sKind = kStFunc;

  // Don't add the function to the function table.
  // It appears Storage class kScUnused is not honored.
  MIRFunction *func = mod.memPool->New<MIRFunction>(&mod, funcst->GetStIdx());
  func->puIdx = GlobalTables::GetFunctionTable().funcTable.size();
  func->puIdxOrigin = func->puIdx;
  MIRFuncType *funcType = mod.memPool->New<MIRFuncType>();
  func->funcType = funcType;
  MIRType *returnType = GlobalTables::GetTypeTable().typeTable.at(PTY_void);
  func->SetReturnTyIdx(returnType->tyIdx);
  func->classTyIdx = TyIdx(0);
  func->SetBaseClassFuncNames(strIdx);
  funcst->SetFunction(func);
  return func;
}

bool MIRParser::IsDelimitationTK(TokenKind tk) {
  switch (tk) {
    case TK_rparen:
    case TK_coma:
      return true;
    default:
      return false;
  }
}

inline bool IsPowerOf2(uint64 i) {
  return (~(i - 1) & i) == i;
}

Opcode MIRParser::GetOpFromToken(TokenKind tk) {
  switch (tk) {
#define OPCODE(X, Y, Z, S) \
  case TK_##X:          \
    return OP_##X;
#include "opcodes.def"
#undef OPCODE
    default:
      return kOpUndef;
  }
}

static bool IsClassInterfaceTypeName(std::string namestr) {
  if (namestr[0] != 'L') {
    return false;
  }
  if (namestr.find("_3B") == std::string::npos) {
    return false;
  }
  return true;
}

bool MIRParser::IsStatement(TokenKind tk) {
  if (tk == TK_LOC || tk == TK_ALIAS) {
    return true;
  }
  Opcode op = GetOpFromToken(tk);
  return kOpcodeInfo.IsStmt(op);
}

inline bool MIRParser::IsExpression(TokenKind tk) {
  return !IsStatement(tk);
}

PrimType MIRParser::GetPrimitiveType(TokenKind tk) {
#define LOAD_ALGO_PRIMARY_TYPE
  switch (tk) {
#define PRIMTYPE(P) \
    case TK_##P:      \
      return PTY_##P;
#include "prim_types.def"
#undef PRIMTYPE
    default:
      return kPtyInvalid;
  }
}

MIRIntrinsicID MIRParser::GetIntrinsicId(TokenKind tk) {
  switch (tk) {
    default:
#define DEF_MIR_INTRINSIC(P, NAME, INTRN_CLASS, RETURN_TYPE, ...) \
  case TK_##P:                                                    \
    return INTRN_##P;
#include "intrinsics.def"
#undef DEF_MIR_INTRINSIC
  }
}

void MIRParser::Error(const char *str) {
  std::stringstream strStream;
  const std::string &lexName = lexer.GetName();
  int curIdx = lexer.GetCurIdx() - lexName.length() + 1;
  strStream << "line: " << lexer.GetLineNum() << ":" << curIdx << ":";
  message.append(strStream.str());
  message.append(str);
  message.append(": ");
  message.append(lexer.GetTokenString());
  message.append("\n");

  mod.dbgInfo->SetErrPos(lexer.GetLineNum(), lexer.GetCurIdx());
}

const std::string &MIRParser::GetError() {
  if (lexer.GetTokenKind() == TK_invalid) {
    std::stringstream strStream;
    strStream << "line: " << lexer.GetLineNum() << ":" << lexer.GetCurIdx() << ":";
    message.append(strStream.str());
    message.append(" invalid token\n");
  }
  return message;
}

void MIRParser::Warning(const char *str) {
  std::stringstream strStream;
  const std::string &lexName = lexer.GetName();
  int curIdx = lexer.GetCurIdx() - lexName.length() + 1;
  strStream << "  >> warning line: " << lexer.GetLineNum() << ":" << curIdx << ":";
  warningMessage.append(strStream.str());
  warningMessage.append(str);
  warningMessage.append("\n");
}

const std::string &MIRParser::GetWarning() {
  return warningMessage;
}

bool MIRParser::ParseSpecialReg(PregIdx &pRegIdx) {
  const std::string &lexName = lexer.GetName();
  int32 lexSize = lexName.size();
  int32 retValSize = strlen(kLexerStringRetval);
  if (strncmp(lexName.c_str(), kLexerStringRetval, retValSize) == 0 && (lexSize > retValSize) &&
      isdigit(lexName[retValSize])) {
    int32 retvalNo = lexName[retValSize] - '0';
    for (int32 i = retValSize + 1; (i < lexSize) && isdigit(lexName[i]); i++) {
      retvalNo = retvalNo * 10 + lexName[i] - '0';
    }
    pRegIdx = -kSregRetval0 - retvalNo;
    lexer.NextToken();
    return true;
  }

  std::map<std::string, enum maple::SpecialReg> pregMapIdx = { { kLexerStringSp, kSregSp },
                                                               { kLexerStringFp, kSregFp },
                                                               { kLexerStringGp, kSregGp },
                                                               { kLexerStringThrownval, kSregThrownval },
                                                               { kLexerStringRetval, kSregRetval0 } };
  if (pregMapIdx.find(lexName) != pregMapIdx.end()) {
    pRegIdx = -pregMapIdx[lexName];
    lexer.NextToken();
    return true;
  }

  Error("unrecognized special register ");
  return false;
}

bool MIRParser::ParsePseudoreg(PrimType pty, PregIdx &pregidx) {
  uint32 pregNo = static_cast<uint32>(lexer.GetTheIntVal());
  CHECK_FATAL(pregNo <= 0xffff, "preg number must be 16 bits");
  MIRFunction *curfunc = mod.CurFunction();
  pregidx = curfunc->pregTab->EnterPregNo(pregNo, pty);
  MIRPreg *preg = curfunc->pregTab->PregFromPregIdx(pregidx);
  if (pty != kPtyInvalid) {
    if (preg->primType != pty) {
      if ((pty == PTY_ref || pty == PTY_ptr) && (preg->primType == PTY_ref || preg->primType == PTY_ptr))
        ;  // PTY_ref and PTY_ptr are compatible with each other
      else {
        Error("inconsistent preg primitive type at ");
        return false;
      }
    }
  }
  lexer.NextToken();
  return true;
}

bool MIRParser::CheckPrimAndDerivedType(TokenKind tokenKind, TyIdx &tyIdx) {
  if (IsPrimitiveType(tokenKind)) {
    return ParsePrimType(tyIdx);
  }
  if (tokenKind == TK_langle) {
    return ParseDerivedType(tyIdx);
  }
  return false;
}

bool MIRParser::ParseType(TyIdx &tyIdx) {
  TokenKind tk = lexer.GetTokenKind();
  if (IsVarName(tk)) {
    return ParseDefinedTypename(tyIdx);
  }
  if (CheckPrimAndDerivedType(tk, tyIdx)) {
    return true;
  }
  Error("token is not a type ");
  return false;
}

bool MIRParser::ParseFarrayType(TyIdx &arrayTyIdx) {
  TokenKind tokenKind = lexer.NextToken();
  TyIdx tyIdx;
  if (!CheckPrimAndDerivedType(tokenKind, tyIdx)) {
    Error("unexpect token parsing flexible array element type ");
    return false;
  }
  CHECK_FATAL(tyIdx != 0, "error encountered parsing flexible array element type ");
  if (mod.IsJavaModule()) {
    MIRJarrayType jarraytype(tyIdx);
    arrayTyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&jarraytype);
  } else {
    MIRFarrayType farraytype(tyIdx);
    arrayTyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&farraytype);
  }
  return true;
}

bool MIRParser::ParseArrayType(TyIdx &arrayTyIdx) {
  TokenKind tokenKind = lexer.GetTokenKind();
  if (tokenKind != TK_lbrack) {
    Error("expect [ for array type but get ");
    return false;
  }
  std::vector<uint32> vec;
  while (tokenKind == TK_lbrack) {
    tokenKind = lexer.NextToken();
    if (tokenKind == TK_rbrack && vec.empty()) {
      break;
    }
    if (tokenKind != TK_intconst) {
      Error("expect int value parsing array type after [ but get ");
      return false;
    }
    int64 val = lexer.GetTheIntVal();
    if (val < 0) {
      Error("expect array value >= 0 ");
      return false;
    }
    vec.push_back(val);
    if (lexer.NextToken() != TK_rbrack) {
      Error("expect ] after int value parsing array type but get ");
      return false;
    }
    tokenKind = lexer.NextToken();
  }
  if (tokenKind == TK_rbrack && vec.empty()) {
    return ParseFarrayType(arrayTyIdx);
  }
  TyIdx tyIdx;
  if (!CheckPrimAndDerivedType(tokenKind, tyIdx)) {
    Error("unexpect token parsing array type after ] ");
    return false;
  }
  CHECK_FATAL(tyIdx != 0, "something wrong with parsing element type ");
  MIRArrayType arrayType(tyIdx, vec);
  if (!ParseTypeAttrs(arrayType.typeAttrs)) {
    Error("bad type attribute in pointer type specification");
    return false;
  }
  arrayTyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&arrayType);
  return true;
}

bool MIRParser::ParseBitfieldType(TyIdx &ftyidx) {
  if (lexer.GetTokenKind() != TK_colon) {
    Error("expect : parsing field type but get ");
    return false;
  }
  if (lexer.NextToken() != TK_intconst) {
    Error("expect int const val parsing field type but get ");
    return false;
  }
  uint8 bitsize = lexer.GetTheIntVal();
  PrimType primtype = GetPrimitiveType(lexer.NextToken());
  if (primtype == kPtyInvalid) {
    Error("expect primitive type but get ");
    return false;
  }
  if (!IsPrimitiveInteger(primtype)) {
    Error("syntax error bit field should be integer type but get ");
    return false;
  }
  MIRBitfieldType bitfieldtype(bitsize, primtype);
  ftyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&bitfieldtype);
  lexer.NextToken();
  return true;
}

bool MIRParser::ParsePragmaElement(MIRPragmaElement *elem) {
  CHECK_FATAL(elem != nullptr, "elem is null");

  TokenKind tk = lexer.GetTokenKind();
  lexer.NextToken();
  std::map<enum maple::TokenKind, enum maple::PragmaValueType> tkPragmaValType = {
    { TK_i8, kValueByte },          { TK_i16, kValueShort },
    { TK_u16, kValueChar },         { TK_i32, kValueInt },
    { TK_i64, kValueLong },         { TK_f32, kValueFloat },
    { TK_f64, kValueDouble },       { TK_retype, kValueMethodType },
    { TK_ref, kValueMethodHandle }, { TK_ptr, kValueString },
    { TK_type, kValueType },        { TK_var, kValueField },
    { TK_func, kValueMethod },      { TK_enum, kValueEnum },
    { TK_array, kValueArray },      { TK_annotation, kValueAnnotation },
    { TK_const, kValueNull },       { TK_u1, kValueBoolean }
  };
  if (tkPragmaValType.find(tk) == tkPragmaValType.end()) {
    Error("parsing pragma error: wrong element type");
    return false;
  }
  elem->type_ = tkPragmaValType[tk];

  switch (tk) {
    case TK_i8:
    case TK_i16:
    case TK_i32:
      elem->val_.i = (int32)lexer.GetTheIntVal();
      break;
    case TK_u16:
    case TK_ref:
      elem->val_.u = (uint64)lexer.GetTheIntVal();
      break;
    case TK_i64:
    case TK_retype:
    case TK_const:
    case TK_u1:
      elem->val_.j = lexer.GetTheIntVal();
      break;
    case TK_f32:
      elem->val_.f = lexer.GetTheFloatVal();
      break;
    case TK_f64:
      elem->val_.d = lexer.GetTheDoubleVal();
      break;
    case TK_ptr:
    case TK_var:
    case TK_func:
    case TK_enum:
      elem->val_.i = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName()).GetIdx();
      break;
    case TK_type:
      lexer.NextToken();
      elem->val_.i = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName()).GetIdx();
      lexer.NextToken();
      break;
    case TK_array:
      if (!ParsePragmaElementForArray(elem)) {
        return false;
      }
      break;
    case TK_annotation:
      if (!ParsePragmaElementForAnnotation(elem)) {
        return false;
      }
      break;
    default:
      return false;
  }
  return true;
}

bool MIRParser::ParsePragmaElementForArray(MIRPragmaElement *elem) {
  CHECK_FATAL(elem != nullptr, "elem is null");

  TokenKind tk;
  tk = lexer.GetTokenKind();
  if (tk != TK_lbrack) {
    Error("parsing pragma error: expecting [ but get ");
    return false;
  }
  tk = lexer.NextToken();
  if (tk != TK_intconst) {
    Error("parsing pragma error: expecting int but get ");
    return false;
  }
  int64 size = lexer.GetTheIntVal();
  tk = lexer.NextToken();
  if (tk != TK_coma && size) {
    Error("parsing pragma error: expecting , but get ");
    return false;
  }
  for (int64 i = 0; i < size; i++) {
    MIRPragmaElement *e0 = mod.memPool->New<MIRPragmaElement>(&mod);
    tk = lexer.NextToken();
    if (!ParsePragmaElement(e0)) {
      Error("parsing pragma error type ");
      return false;
    }
    elem->subelemvec_.push_back(e0);
    tk = lexer.NextToken();
    if (tk != TK_coma && tk != TK_rbrack) {
      Error("parsing pragma error: expecting , or ] but get ");
      return false;
    }
  }
  return true;
}

bool MIRParser::ParsePragmaElementForAnnotation(MIRPragmaElement *elem) {
  CHECK_FATAL(elem != nullptr, "elem is null");
  TokenKind tk;
  tk = lexer.GetTokenKind();
  if (tk != TK_langle) {
    Error("parsing pragma error: expecting < but get ");
    return false;
  }
  tk = lexer.NextToken();
  elem->typestridx_ = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  tk = lexer.NextToken();
  if (tk != TK_rangle) {
    Error("parsing pragma error: expecting > but get ");
    return false;
  }
  tk = lexer.NextToken();
  if (tk != TK_lbrack) {
    Error("parsing pragma error: expecting [ but get ");
    return false;
  }
  tk = lexer.NextToken();
  if (tk != TK_intconst) {
    Error("parsing pragma error: expecting int but get ");
    return false;
  }
  int64 size = lexer.GetTheIntVal();
  tk = lexer.NextToken();
  if (tk != TK_coma && size) {
    Error("parsing pragma error: expecting , but get ");
    return false;
  }
  for (int i = 0; i < size; i++) {
    MIRPragmaElement *e0 = mod.memPool->New<MIRPragmaElement>(&mod);
    tk = lexer.NextToken();
    if (tk != TK_label) {
      Error("parsing pragma error: expecting @ but get ");
      return false;
    }
    e0->namestridx_ = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
    tk = lexer.NextToken();
    if (!ParsePragmaElement(e0)) {
      Error("parsing pragma error type ");
      return false;
    }
    elem->subelemvec_.push_back(e0);
    tk = lexer.NextToken();
    if (tk != TK_coma && tk != TK_rbrack) {
      Error("parsing pragma error: expecting , or ] but get ");
      return false;
    }
  }
  return true;
}

bool MIRParser::ParsePragma(MIRStructType &type) {
  MIRPragma *p = mod.memPool->New<MIRPragma>(&mod);
  p->visibility = lexer.GetTheIntVal();
  TokenKind tk = lexer.NextToken();

  std::map<enum maple::TokenKind, enum maple::PragmaKind> tkPragmaKind = { { TK_class, kPragmaClass },
                                                                           { TK_func, kPragmaFunc },
                                                                           { TK_var, kPragmaVar },
                                                                           { TK_param, kPragmaParam },
                                                                           { TK_func_ex, kPragmaFuncExecptioni },
                                                                           { TK_func_var, kPragmaFuncVar } };
  if (tkPragmaKind.find(tk) == tkPragmaKind.end()) {
    Error("parsing pragma error: wrong kind ");
    return false;
  }
  p->pragmaKind = tkPragmaKind[tk];

  if (tk == TK_param) {
    tk = lexer.NextToken();
    p->paramNum = lexer.GetTheIntVal();
  }
  tk = lexer.NextToken();
  if (tk != TK_gname && tk != TK_lname && tk != TK_fname) {
    Error("expect global or local token parsing pragma but get ");
    return false;
  }
  p->strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  tk = lexer.NextToken();
  TyIdx tyIdx;
  if (!ParseType(tyIdx)) {
    Error("parsing pragma error: wrong type ");
    return false;
  }
  p->tyIdx = tyIdx;
  tk = lexer.GetTokenKind();
  if (tk != TK_lbrace) {
    Error("parsing pragma error: expecting { but get ");
    return false;
  }
  tk = lexer.NextToken();
  while (tk != TK_rbrace) {
    MIRPragmaElement *e = mod.memPool->New<MIRPragmaElement>(&mod);
    e->namestridx_ = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
    tk = lexer.NextToken();
    if (!ParsePragmaElement(e)) {
      Error("parsing pragma error type ");
      return false;
    }
    p->elementVec.push_back(e);
    tk = lexer.NextToken();
    if (tk != TK_rbrace && tk != TK_coma) {
      Error("parsing pragma error syntax ");
      return false;
    }
    if (tk == TK_coma) {
      lexer.NextToken();
    }
  }
  lexer.NextToken();
  if (type.GetKind() == kTypeClass || type.GetKind() == kTypeClassIncomplete)
    static_cast<MIRClassType*>(&type)->pragmaVec.push_back(p);
  else static_cast<MIRInterfaceType*>(&type)->pragmaVec.push_back(p);
  return true;
}

// lexer.GetTokenKind() assumed to be the TK_lbrace that starts the fields
bool MIRParser::ParseFields(MIRStructType &type) {
  if (type.IsIncomplete()) {
    Warning("incomplete class/interface type");
  }
  TokenKind tk = lexer.NextToken();
  while (tk == TK_label || tk == TK_prntfield || tk == TK_pragma) {
    bool ispragma = tk == TK_pragma;
    bool notatype = false;
    TyIdx ftyidx(0);
    bool isparentfield = false;
    if (tk == TK_prntfield) {
      isparentfield = true;
    }
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
    tk = lexer.NextToken();
    uint32 lnum = lexer.GetLineNum();
    if (ispragma) {
      if (type.GetKind() == kTypeClass || type.GetKind() == kTypeClassIncomplete || type.GetKind() == kTypeInterface ||
          type.GetKind() == kTypeInterfaceIncomplete) {
        if (!ParsePragma(type)) {
          Error("parsing pragma error ");
          return false;
        }
      } else {
        Error("parsing pragma error ");
      }
      notatype = true;
    } else if ((tk == TK_lbrack) && (GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx) == "staticValue")) {
      while (tk != TK_coma) {
        EncodedValue elem;
        if (tk != TK_lbrack) {
          Error("parsing staticValue error ");
        }

        tk = lexer.NextToken();
        uint32 i = 0;
        while (tk != TK_rbrack) {
          if (tk != TK_intconst) {
            Error("parsing staticValue error ");
          }
          elem.encodedValue[i++] = lexer.GetTheIntVal();
          tk = lexer.NextToken();
        }
        tk = lexer.NextToken();

        if (type.typeKind == kTypeClass || type.typeKind == kTypeClassIncomplete) {
          MIRClassType *classtype = static_cast<MIRClassType *>(&type);
          classtype->staticValue.push_back(elem);
        } else if (type.typeKind == kTypeInterface || type.typeKind == kTypeInterfaceIncomplete) {
          MIRInterfaceType *interfacetype = static_cast<MIRInterfaceType *>(&type);
          interfacetype->staticValue.push_back(elem);
        } else {
          Error("parsing staticValue error ");
        }
      }
      notatype = true;
    } else if (tk == TK_colon) {  // a bitfield
      if (!ParseBitfieldType(ftyidx)) {
        Error("parsing struct type error ");
        return false;
      }
    } else if (tk == TK_langle) {
      if (!ParseDerivedType(ftyidx)) {
        Error("parsing struct type error ");
        return false;
      }
    } else if ((IsPrimitiveType(tk))) {
      if (!ParsePrimType(ftyidx)) {
        Error("expect :<val> or primitive type or derived type parsing struct type ");
        return false;
      }
    } else if ((tk == TK_intconst || tk == TK_string) && !isparentfield &&
               (type.typeKind == kTypeClass || type.typeKind == kTypeClassIncomplete || type.typeKind == kTypeInterface ||
                type.typeKind == kTypeInterfaceIncomplete)) {
      if (type.typeKind == kTypeClass || type.typeKind == kTypeClassIncomplete) {
        MIRClassType *classtype = static_cast<MIRClassType *>(&type);
        if (tk == TK_intconst) {
          uint32 infoval = lexer.GetTheIntVal();
          classtype->info.push_back(MIRInfoPair(strIdx, infoval));
          classtype->infoIsString.push_back(false);
        } else {
          GStrIdx litstridx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
          classtype->info.push_back(MIRInfoPair(strIdx, litstridx.GetIdx()));
          classtype->infoIsString.push_back(true);
        }
      } else if (type.typeKind == kTypeInterface || type.typeKind == kTypeInterfaceIncomplete) {
        MIRInterfaceType *interfacetype = static_cast<MIRInterfaceType *>(&type);
        if (tk == TK_intconst) {
          uint32 infoval = lexer.GetTheIntVal();
          interfacetype->info.push_back(MIRInfoPair(strIdx, infoval));
          interfacetype->infoIsString.push_back(false);
        } else {
          GStrIdx litstridx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
          interfacetype->info.push_back(MIRInfoPair(strIdx, litstridx.GetIdx()));
          interfacetype->infoIsString.push_back(true);
        }
      }
      notatype = true;
      lexer.NextToken();
    } else {
      Error("unexpected type parsing struct type at ");
      return false;
    }
    ASSERT((ftyidx != TyIdx(0) || notatype), "something wrong parsing struct type");
    if (!notatype) {
      FieldAttrs tA;
      if (!ParseFieldAttrs(tA)) {
        Error("bad type attribute in struct field at ");
        return false;
      }
      if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx)->HasTypeParam()) {
        tA.SetAttr(FLDATTR_generic);
      }
      FieldPair p = FieldPair(strIdx, TyidxFieldAttrPair(ftyidx, tA));
      bool isstaticfield = false;
      if (tA.GetAttr(FLDATTR_static)) {
        // static and parent share the same ^ token
        isstaticfield = true;
        isparentfield = false;
      }
      if (isparentfield) {
        type.parentFields.push_back(p);
      } else if (isstaticfield) {
        type.staticFields.push_back(p);
      } else {
        type.fields.push_back(p);
      }
      tk = lexer.GetTokenKind();
      bool isConst = tA.GetAttr(FLDATTR_static) && tA.GetAttr(FLDATTR_final) &&
                     (tA.GetAttr(FLDATTR_public) || tA.GetAttr(FLDATTR_protected));
      if (isConst && tk == TK_eqsign) {
        tk = lexer.NextToken();
        MIRConst *mirconst = nullptr;
        if (!ParseInitValue(mirconst, ftyidx)) {
          Error("wrong initialiaton value at ");
          return false;
        }
        GlobalTables::GetConstPool().InsertConstPool(p.first, mirconst);
        tk = lexer.GetTokenKind();
      }
    } else {
      tk = lexer.GetTokenKind();
    }
    tk = lexer.GetTokenKind();
    if (tk == TK_coma) {
      tk = lexer.NextToken();
      if (tk == TK_rbrace) {
        Error(",} is not legal, expect another field type after ,");
        return false;
      }
    } else if (tk != TK_rbrace) {
      Error(", missing after ");
      return false;
    } else {
      return true;
    }
  }
  while (tk == TK_fname) {
    uint32 lnum = lexer.GetLineNum();
    const std::string &funcName = lexer.GetName();
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcName);
    MIRSymbol *prevFuncSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
    if (prevFuncSt && (prevFuncSt->storageClass != kScText || prevFuncSt->sKind != kStFunc)) {
      // Based on the current maple format, a previous declaration at this
      // point can only come from another module. Check consistency.
      Error("redeclaration of name as func in ");
      return false;
    }

    // Always create a new symbol because we can not reuse symbol from other module
    MIRSymbol *funcSt = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
    funcSt->SetNameStridx(strIdx);
    GlobalTables::GetGsymTable().AddToStringSymbolMap(funcSt);
    funcSt->storageClass = kScText;
    funcSt->sKind = kStFunc;
    SetSrcPos(funcSt->srcPosition, lexer.GetLineNum());

    MIRFunction *fn = mod.memPool->New<MIRFunction>(&mod, funcSt->GetStIdx());
    fn->puIdx = GlobalTables::GetFunctionTable().funcTable.size();
    GlobalTables::GetFunctionTable().funcTable.push_back(fn);
    funcSt->SetFunction(fn);
    fn->fileIndex = 0;
    fn->SetBaseClassFuncNames(funcSt->nameStrIdx);

    FuncAttrs tA;
    if (lexer.NextToken() != TK_lparen) {
      if (!ParseFuncAttrs(tA)) {
        return false;
      }
      // Skip attribute checking
      fn->SetAttrs(tA);
    }

    TyIdx funcTyidx;
    if (!ParseFuncType(funcTyidx)) {
      return false;
    }
    // tyIdx does not work. Calling Equalto does not work either.
    MIRFuncType *functype = static_cast<MIRFuncType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(funcTyidx));
    fn->funcType = functype;
    switch (GlobalTables::GetTypeTable().GetTypeFromTyIdx(functype->retTyIdx)->typeKind) {
      case kTypeUnion:
      case kTypeStruct:
      case kTypeStructIncomplete:
      case kTypeClass:
      case kTypeClassIncomplete:
      case kTypeInterface:
      case kTypeInterfaceIncomplete:
        fn->SetReturnStruct();
        break;
      default:;
    }
    funcSt->SetTyIdx(funcTyidx);

    for (size_t i = 0; i < functype->paramTypeList.size(); i++) {
      FormalDef formalDef(nullptr, functype->paramTypeList[i], functype->paramAttrsList[i]);
      fn->formalDefVec.push_back(formalDef);
    }

    MethodPair p = MethodPair(funcSt->stIdx, TyidxFuncAttrPair(funcTyidx, FuncAttrs(tA)));
    type.methods.push_back(p);

    tk = lexer.GetTokenKind();
    if (tk == TK_coma) {
      tk = lexer.NextToken();
      if (tk == TK_rbrace) {
        Error(",} is not legal, expect another field type after ,");
        return false;
      }
    } else if (tk != TK_rbrace) {
      Error(", missing after ");
      return false;
    } else {
      return true;
    }
  }

  // interfacesImplemented
  while (tk == TK_gname) {
    tk = lexer.NextToken();
    if ((tk == TK_coma || tk == TK_rbrace) && (type.typeKind == kTypeClass || type.typeKind == kTypeClassIncomplete)) {
      MIRClassType *classtype = static_cast<MIRClassType *>(&type);
      std::string namestr = lexer.GetName();
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(namestr);
      TyIdx tyIdx = mod.typeNameTab->GetTyIdxFromGStrIdx(strIdx);
      if (tyIdx == TyIdx(0)) {
        MIRInterfaceType interfacetype(kTypeInterfaceIncomplete);
        interfacetype.nameStrIdx = strIdx;
        tyIdx = GlobalTables::GetTypeTable().CreateMIRType(&interfacetype);
        mod.AddClass(tyIdx);
        mod.AddExternStructType(tyIdx);
        mod.typeNameTab->SetGStrIdxToTyIdx(strIdx, tyIdx);
      }
      classtype->interfacesImplemented.push_back(tyIdx);
    }
    if (tk == TK_coma) {
      tk = lexer.NextToken();
    }
  }

  // allow empty class for third party classes we do not have info
  if (tk == TK_rbrace) {
    return true;
  }

  Error("expect field or member function name in struct/class body but get ");
  return false;
}

bool MIRParser::ParseStructType(TyIdx &styIdx) {
  MIRTypeKind tkind = kTypeInvalid;
  switch (lexer.GetTokenKind()) {
    case TK_struct:
      tkind = kTypeStruct;
      break;
    case TK_structincomplete:
      tkind = kTypeStructIncomplete;
      break;
    case TK_union:
      tkind = kTypeUnion;
      break;
    default:;
  }
  if (lexer.NextToken() != TK_lbrace) {
    Error("expect { parsing struct body");
    return false;
  }
  MIRStructType structType(tkind);
  if (mod.srcLang == kSrcLangCPlusPlus) {
    structType.isCPlusPlus = true;
  }
  if (!ParseFields(structType)) {
    return false;
  }
  if (styIdx != 0) {
    MIRType *prevType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(styIdx);
    ASSERT(prevType->GetKind() == kTypeStruct || prevType->GetKind() == kTypeStructIncomplete,
           "type kind should be consistent.");
    if (prevType->IsIncomplete() && !(structType.IsIncomplete())) {
      structType.nameStrIdx = prevType->nameStrIdx;
      structType.tyIdx = styIdx;
      GlobalTables::GetTypeTable().typeTable[styIdx.GetIdx()] = structType.CopyMIRTypeNode();
    }
  } else {
    styIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&structType);
  }
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseClassType(TyIdx &styIdx) {
  MIRTypeKind tkind = (lexer.GetTokenKind() == TK_class) ? kTypeClass : kTypeClassIncomplete;
  TyIdx prntTyidx(0);
  if (lexer.NextToken() == TK_langle) {
    // parsing parent as class
    if (!ParseDerivedType(prntTyidx, kTypeClass)) {
      Error("parsing class parent type error ");
      return false;
    }
  }
  MIRClassType classType(tkind);
  classType.parentTyIdx = prntTyidx;
  if (!ParseFields(classType)) {
    return false;
  }
  if (styIdx != 0) {
    MIRType *prevType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(styIdx);
    ASSERT(prevType->GetKind() == kTypeClass || prevType->GetKind() == kTypeClassIncomplete,
           "type kind should be consistent.");
    if (prevType->IsIncomplete() && !(classType.IsIncomplete())) {
      classType.nameStrIdx = prevType->nameStrIdx;
      classType.tyIdx = styIdx;
      GlobalTables::GetTypeTable().typeTable[styIdx.GetIdx()] = classType.CopyMIRTypeNode();
    }
  } else {
    styIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&classType);
    // set up classTyIdx for methods
    for (size_t i = 0; i < classType.methods.size(); ++i) {
      StIdx stIdx = classType.methods[i].first;
      MIRSymbol *st = GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx());
      ASSERT(st->sKind == kStFunc, "unexpected st->sKind");
      st->GetFunction()->SetClassTyIdx(styIdx);
    }
    mod.AddClass(styIdx);
  }
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseInterfaceType(TyIdx &sTyIdx) {
  MIRTypeKind tkind = (lexer.GetTokenKind() == TK_interface) ? kTypeInterface : kTypeInterfaceIncomplete;
  std::vector<TyIdx> parents;
  TokenKind tk = lexer.NextToken();
  while (tk == TK_langle) {
    TyIdx prntTyidx(0);
    // parsing parents as interfaces
    if (!ParseDerivedType(prntTyidx, kTypeInterface)) {
      Error("parsing interface parent type error ");
      return false;
    }
    parents.push_back(prntTyidx);
    tk = lexer.GetTokenKind();
  }
  MIRInterfaceType interfaceType(tkind);
  interfaceType.parentsTyIdx = parents;
  if (!ParseFields(interfaceType)) {
    return false;
  }
  if (sTyIdx != 0) {
    MIRType *prevType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sTyIdx);
    ASSERT(prevType->GetKind() == kTypeInterface || prevType->IsIncomplete(),
           "type kind should be consistent.");
    if (prevType->IsIncomplete() && !(interfaceType.IsIncomplete())) {
      interfaceType.nameStrIdx = prevType->nameStrIdx;
      interfaceType.tyIdx = sTyIdx;
      GlobalTables::GetTypeTable().typeTable[sTyIdx.GetIdx()] = interfaceType.CopyMIRTypeNode();
    }
  } else {
    sTyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&interfaceType);
    // set up classTyIdx for methods
    for (size_t i = 0; i < interfaceType.methods.size(); ++i) {
      StIdx stIdx = interfaceType.methods[i].first;
      MIRSymbol *st = GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx());
      ASSERT(st != nullptr, "st is null");
      ASSERT(st->sKind == kStFunc, "unexpected st->sKind");
      st->GetFunction()->SetClassTyIdx(sTyIdx);
    }
    mod.AddClass(sTyIdx);
  }
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseAlignAttrs(TypeAttrs &tA) {
  CHECK_FATAL(TK_align == lexer.GetTokenKind(), "wrong TK kind taken from file");
  if (lexer.NextToken() != TK_lparen) {
    Error("unexpected token in alignment specification after ");
    return false;
  }
  if (lexer.NextToken() != TK_intconst) {
    Error("unexpected token in alignment specification after ");
    return false;
  }
  if (!IsPowerOf2(lexer.GetTheIntVal())) {
    Error("specified alignment must be power of 2 instead of ");
    return false;
  }
  tA.SetAlign(lexer.GetTheIntVal());
  if (lexer.NextToken() != TK_rparen) {
    Error("unexpected token in alignment specification after ");
    return false;
  }
  return true;
}

bool MIRParser::ParseTypeAttrs(TypeAttrs &attrs) {
  do {
    switch (lexer.GetTokenKind()) {
#define TYPE_ATTR
#define ATTR(X)            \
      case TK_##X:             \
        attrs.SetAttr(ATTR_##X); \
        break;
#include "all_attributes.def"
#undef ATTR
#undef TYPE_ATTR
      case TK_align: {
        if (!ParseAlignAttrs(attrs)) {
          return false;
        }
        break;
      }
      default:
        return true;
    }  // switch
    lexer.NextToken();
  } while (true);
}

inline bool MIRParser::ParseVarTypeAttrs(MIRSymbol *st) {
  return ParseTypeAttrs(st->typeAttrs);
}

bool MIRParser::ParseFieldAttrs(FieldAttrs &attrs) {
  do {
    switch (lexer.GetTokenKind()) {
#define FIELD_ATTR
#define ATTR(X)               \
      case TK_##X:                \
        attrs.SetAttr(FLDATTR_##X); \
        break;
#include "all_attributes.def"
#undef ATTR
#undef FIELD_ATTR
      case TK_align: {
        if (lexer.NextToken() != TK_lparen) {
          Error("unexpected token in alignment specification after ");
          return false;
        }
        if (lexer.NextToken() != TK_intconst) {
          Error("unexpected token in alignment specification after ");
          return false;
        }
        if (!IsPowerOf2(lexer.GetTheIntVal())) {
          Error("specified alignment must be power of 2 instead of ");
          return false;
        }
        attrs.SetAlign(lexer.GetTheIntVal());
        if (lexer.NextToken() != TK_rparen) {
          Error("unexpected token in alignment specification after ");
          return false;
        }
        break;
      }
      default:
        return true;
    }  // switch
    lexer.NextToken();
  } while (true);
}

bool MIRParser::ParseFuncAttrs(FuncAttrs &attrs) {
  do {
    switch (lexer.GetTokenKind()) {
#define FUNC_ATTR
#define ATTR(X)                \
      case TK_##X:                 \
        attrs.SetAttr(FUNCATTR_##X); \
        break;
#include "all_attributes.def"
#undef ATTR
#undef FUNC_ATTR
      default:
        return true;
    }  // switch
    lexer.NextToken();
  } while (true);
}

bool MIRParser::ParsePrimType(TyIdx &tyIdx) {
  PrimType primtype = GetPrimitiveType(lexer.GetTokenKind());
  if (primtype == kPtyInvalid) {
    tyIdx = TyIdx(0);
    Error("ParsePrimType failed, invalid token");
    return false;
  }
  lexer.NextToken();
  tyIdx = GlobalTables::GetTypeTable().typeTable.at(static_cast<uint32>(primtype))->tyIdx;
  return true;
}

bool MIRParser::ParseTypeParam(TyIdx &dtyidx) {
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  MIRTypeParam typeparm(strIdx);
  dtyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&typeparm);
  lexer.NextToken();
  return true;
}

// LB not handled in binary format
bool MIRParser::ParseDefinedTypename(TyIdx &dtyidx, MIRTypeKind kind) {
  TokenKind tk = lexer.GetTokenKind();
  if (tk != TK_gname && tk != TK_lname) {
    Error("expect global or local token parsing typedef type but get ");
    return false;
  }

  std::string namestr = lexer.GetName();
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(namestr);

  // check if type already exist
  dtyidx = mod.typeNameTab->GetTyIdxFromGStrIdx(strIdx);
  TyIdx prevTyidx(0);
  if (dtyidx.GetIdx()) {
    MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(dtyidx);
    MIRStructType *stype = dynamic_cast<MIRStructType *>(type);
    if (stype) {
      // check whether need to update from incomplete class to interface
      if (stype->typeKind == kind) {
        lexer.NextToken();
        return true;
      } else {
        prevTyidx = dtyidx;
      }
    }
  }

  if (tk == TK_gname) {
    dtyidx = mod.typeNameTab->GetTyIdxFromGStrIdx(strIdx);
    if (dtyidx == TyIdx(0)) {
      if (kind == kTypeInterface || kind == kTypeInterfaceIncomplete) {
        MIRInterfaceType interfaceType(kTypeInterfaceIncomplete);
        interfaceType.nameStrIdx = strIdx;
        dtyidx = GlobalTables::GetTypeTable().CreateMIRType(&interfaceType);
        mod.AddClass(dtyidx);
        mod.AddExternStructType(dtyidx);
        mod.typeNameTab->SetGStrIdxToTyIdx(strIdx, dtyidx);
      } else if (kind == kTypeClass || kind == kTypeClassIncomplete || IsClassInterfaceTypeName(namestr)) {
        MIRClassType classType(kTypeClassIncomplete);
        classType.nameStrIdx = strIdx;
        dtyidx = GlobalTables::GetTypeTable().CreateMIRType(&classType);
        mod.AddClass(dtyidx);
        mod.AddExternStructType(dtyidx);
        mod.typeNameTab->SetGStrIdxToTyIdx(strIdx, dtyidx);
      } else {
        MIRType *newtype = mod.memPool->New<MIRTypeByName>(strIdx);
        newtype->tyIdx = TyIdx(GlobalTables::GetTypeTable().typeTable.size());
        GlobalTables::GetTypeTable().typeTable.push_back(newtype);
        mod.typeNameTab->SetGStrIdxToTyIdx(strIdx, newtype->tyIdx);
        dtyidx = newtype->tyIdx;
      }
    }
  } else {
    dtyidx = mod.CurFunction()->typeNameTab->GetTyIdxFromGStrIdx(strIdx);
    if (dtyidx == TyIdx(0)) {
      MIRType *newtype = mod.memPool->New<MIRTypeByName>(strIdx);
      newtype->tyIdx = TyIdx(GlobalTables::GetTypeTable().typeTable.size());
      GlobalTables::GetTypeTable().typeTable.push_back(newtype);
      mod.CurFunction()->typeNameTab->SetGStrIdxToTyIdx(strIdx, newtype->tyIdx);
      dtyidx = newtype->tyIdx;
    }
  }

  // replace prev_tyidx with dtyidx
  if (prevTyidx != TyIdx(0) && prevTyidx != dtyidx) {
    // replace all uses of prev_tyidx by tyIdx in typeTable
    typeDefIdxMap[prevTyidx] = dtyidx;
    // remove prev_tyidx from classlist
    mod.RemoveClass(prevTyidx);
  }
  lexer.NextToken();
  return true;
}

bool MIRParser::ParsePointType(TyIdx &ptyidx) {
  TokenKind pdtk;
  if (lexer.GetTokenKind() == TK_func) {  // a function pointer
    pdtk = lexer.GetTokenKind();
  } else if (lexer.GetTokenKind() != TK_asterisk) {
    Error("expect * for point type but get ");
    return false;
  } else {
    pdtk = lexer.NextToken();
  }

  TyIdx pptyidx(0);
  if (IsPrimitiveType(pdtk)) {
    if (!ParsePrimType(pptyidx)) {
      return false;
    }
  } else if (pdtk == TK_asterisk) {  // a point type
    if (!ParsePointType(pptyidx)) {
      return false;
    }
  } else if (pdtk == TK_lbrack) {  // a array type
    if (!ParseArrayType(pptyidx)) {
      return false;
    }
  } else if (pdtk == TK_struct || pdtk == TK_union || pdtk == TK_structincomplete) {
    if (!ParseStructType(pptyidx)) {
      return false;
    }
  } else if (pdtk == TK_class || pdtk == TK_classincomplete) {
    if (!ParseClassType(pptyidx)) {
      return false;
    }
  } else if (pdtk == TK_gname) {
    if (!ParseDefinedTypename(pptyidx)) {
      return false;
    }
  } else if (pdtk == TK_langle) {
    if (!ParseDerivedType(pptyidx)) {
      return false;
    }
  } else if (pdtk == TK_func) {
    lexer.NextToken();
    if (!ParseFuncType(pptyidx)) {
      return false;
    }
  } else {
    Error("unexpect type ");
    return false;
  }
  ASSERT(pptyidx != TyIdx(0), "something wrong with parsing element type ");
  PrimType pty = mod.IsJavaModule() ? PTY_ref : PTY_ptr;
  if (pdtk == maple::TK_constStr || pdtk == maple::TK_func) {
    pty = PTY_ptr;
  }
  MIRPtrType pointtype(pptyidx, pty);  // use reference type here
  if (!ParseTypeAttrs(pointtype.typeAttrs)) {
    Error("bad type attribute in pointer type specification");
    return false;
  }
  ptyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&pointtype);
  return true;
}

// used in parsing the parameter list (types only, without parameter names)
// in function pointer specification and member function prototypes inside
// structs and classes
bool MIRParser::ParseFuncType(TyIdx &tyIdx) {
  // parse parameters
  if (lexer.GetTokenKind() != TK_lparen) {
    Error("expect ( parse function type parameters but get ");
    return false;
  }
  std::vector<TyIdx> vecTy;
  std::vector<TypeAttrs> vecAt;
  TokenKind tk = lexer.NextToken();
  bool varargs = false;
  while (tk != TK_rparen) {
    if (tk == TK_dotdotdot) {
      if (vecTy.size() == 0) {
        Error("variable arguments can only appear after fixed parameters ");
        return false;
      }
      varargs = true;
      tk = lexer.NextToken();
      if (tk != TK_rparen) {
        Error("expect ) after ... but get");
        return false;
      }
      break;
    }
    TyIdx tyIdx(0);
    if (!ParseType(tyIdx)) {
      Error("expect type parsing function parameters ");
      return false;
    }
    TypeAttrs tA;
    if (!ParseTypeAttrs(tA)) {
      Error("bad attribute in function parameter type at ");
      return false;
    }
    tk = lexer.GetTokenKind();
    if (tk == TK_coma) {
      tk = lexer.NextToken();
      if (tk == TK_rparen) {
        Error("syntax error, meeting ,) expect another type after , or ) without , ");
        return false;
      }
    }
    if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->HasTypeParam()) {
      tA.SetAttr(ATTR_generic);
    }
    vecTy.push_back(tyIdx);
    vecAt.push_back(tA);
  }
  // parse return type
  lexer.NextToken();
  TyIdx rettypeidx(0);
  if (!ParseType(rettypeidx)) {
    Error("expect return type for function type but get ");
    return false;
  }
  MIRFuncType functype(rettypeidx, vecTy, vecAt);
  functype.isVarArgs = varargs;
  tyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&functype);
  return true;
}

// parse the generic type instantiation vector enclosed inside braces; syntax
// is: { <type-param> = <real-type> [, <type-param> = <real-type>] }
// where the contents enclosed in [ and ] can occur 0 or more times
bool MIRParser::ParseGenericInstantVector(GenericInstantVector &instantvec) {
  TokenKind tk;
  TyIdx tparmidx;
  do {
    tk = lexer.NextToken();  // skip the lbrace or comma
    if (!ParseTypeParam(tparmidx)) {
      Error("type parameter incorrectly specified in generic type/function instantiation at ");
      return false;
    }
    tk = lexer.GetTokenKind();
    if (tk != TK_eqsign) {
      Error("missing = in generic type/function instantiation at ");
      return false;
    }
    tk = lexer.NextToken();  // skip the =
    TyIdx realtyidx;
    if (!ParseType(realtyidx)) {
      Error("error parsing type in generic type/function instantiation at ");
      return false;
    }
    instantvec.push_back(TypePair(tparmidx, realtyidx));
    tk = lexer.GetTokenKind();
    if (tk == TK_rbrace) {
      lexer.NextToken();  // skip the rbrace
      return true;
    }
  } while (tk == TK_coma);
  Error("error parsing generic type/function instantiation at ");
  return false;
}

bool MIRParser::ParseDerivedType(TyIdx &tyIdx, MIRTypeKind kind) {
  if (lexer.GetTokenKind() != TK_langle) {
    Error("expect langle but get ");
    return false;
  }

  TokenKind ltk = lexer.NextToken();
  if (IsPrimitiveType(ltk)) {
    if (!ParsePrimType(tyIdx)) {
      Error("ParseDerivedType failed when parsing tyIdx at ");
      return false;
    }
  } else {
    switch (ltk) {
      case TK_asterisk:  // point type
      case TK_func:
        if (!ParsePointType(tyIdx)) {
          Error("point type wrong when parsing derived type at ");
          return false;
        }
        break;
      case TK_lbrack:  // array type
        if (!ParseArrayType(tyIdx)) {
          Error("array type wrong when parsing derived type at ");
          return false;
        }
        break;
      case TK_struct:            // struct type
      case TK_structincomplete:  // structincomplete type
      case TK_union:             // union type
        if (!ParseStructType(tyIdx)) {
          Error("struct/union type wrong when parsing derived type at ");
          return false;
        }
        break;
      case TK_class:  // class type
      case TK_classincomplete:
        if (!ParseClassType(tyIdx)) {
          Error("class type wrong when parsing derived type at ");
          return false;
        }
        break;
      case TK_interface:  // interface type
      case TK_interfaceincomplete:
        if (!ParseInterfaceType(tyIdx)) {
          Error("interface type wrong when parsing derived type at ");
          return false;
        }
        break;
      case TK_lname:  // local type
      case TK_gname:  // global type
        if (!ParseDefinedTypename(tyIdx, kind)) {
          Error("type name wrong when parsing derived type at ");
          return false;
        }
        if (lexer.GetTokenKind() == TK_lbrace) {
          MIRGenericInstantType genericinstty(tyIdx);
          if (!ParseGenericInstantVector(genericinstty.instantVec)) {
            Error("error parsing generic type instantiation at ");
            return false;
          }
          tyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&genericinstty);
        }
        break;
      case TK_typeparam:
        if (!ParseTypeParam(tyIdx)) {
          Error("type parameter wrong when parsing derived type at ");
          return false;
        }
        break;
      default:
        Error("expect type token but get ");
        return false;
    }
  }

  // parse >
  if (lexer.GetTokenKind() != TK_rangle) {
    Error("expect > parse derived type but get ");
    return false;
  }
  lexer.NextToken();
  return true;
}

void MIRParser::FixForwardReferencedTypeForOneAgg(MIRType *ty) {
  if (ty->typeKind == kTypePointer) {
    MIRPtrType *ptrtype = static_cast<MIRPtrType *>(ty);
    std::map<TyIdx, TyIdx>::iterator it = typeDefIdxMap.find(ptrtype->pointedTyIdx);
    if (it != typeDefIdxMap.end()) {
      ptrtype->pointedTyIdx = it->second;
    }
  } else if (ty->typeKind == kTypeArray) {
    MIRArrayType *arrayType = static_cast<MIRArrayType *>(ty);
    std::map<TyIdx, TyIdx>::iterator it = typeDefIdxMap.find(arrayType->eTyIdx);
    if (it != typeDefIdxMap.end()) {
      arrayType->eTyIdx = it->second;
    }
  } else if (ty->typeKind == kTypeFArray || ty->typeKind == kTypeJArray) {
    MIRFarrayType *arrayType = static_cast<MIRFarrayType *>(ty);
    std::map<TyIdx, TyIdx>::iterator it = typeDefIdxMap.find(arrayType->elemTyIdx);
    if (it != typeDefIdxMap.end()) {
      arrayType->elemTyIdx = it->second;
    }
  } else if (ty->typeKind == kTypeStruct || ty->typeKind == kTypeStructIncomplete || ty->typeKind == kTypeUnion ||
             ty->typeKind == kTypeClass || ty->typeKind == kTypeClassIncomplete || ty->typeKind == kTypeInterface ||
             ty->typeKind == kTypeInterfaceIncomplete) {
    if (ty->typeKind == kTypeClass || ty->typeKind == kTypeClassIncomplete) {
      MIRClassType *classty = static_cast<MIRClassType *>(ty);
      std::map<TyIdx, TyIdx>::iterator it = typeDefIdxMap.find(classty->parentTyIdx);
      if (it != typeDefIdxMap.end()) {
        classty->parentTyIdx = it->second;
      }
      for (uint32 j = 0; j < classty->interfacesImplemented.size(); j++) {
        std::map<TyIdx, TyIdx>::iterator it2 = typeDefIdxMap.find(classty->interfacesImplemented[j]);
        if (it2 != typeDefIdxMap.end()) {
          classty->interfacesImplemented[j] = it2->second;
        }
      }
    } else if (ty->typeKind == kTypeInterface || ty->typeKind == kTypeInterfaceIncomplete) {
      MIRInterfaceType *interfacety = static_cast<MIRInterfaceType *>(ty);
      for (uint32 j = 0; j < interfacety->parentsTyIdx.size(); j++) {
        std::map<TyIdx, TyIdx>::iterator it = typeDefIdxMap.find(interfacety->parentsTyIdx[j]);
        if (it != typeDefIdxMap.end()) {
          interfacety->parentsTyIdx[j] = it->second;
        }
      }
    }
    MIRStructType *structty = static_cast<MIRStructType *>(ty);
    for (uint32 j = 0; j < structty->fields.size(); j++) {
      TyIdx fieldtyidx = structty->fields[j].second.first;
      std::map<TyIdx, TyIdx>::iterator it = typeDefIdxMap.find(fieldtyidx);

      if (it != typeDefIdxMap.end()) {
        structty->fields[j].second.first = it->second;
      }
    }
    for (uint32 j = 0; j < structty->staticFields.size(); j++) {
      TyIdx fieldtyidx = structty->staticFields[j].second.first;
      std::map<TyIdx, TyIdx>::iterator it = typeDefIdxMap.find(fieldtyidx);
      if (it != typeDefIdxMap.end()) {
        structty->staticFields[j].second.first = it->second;
      }
    }
    for (uint32 j = 0; j < structty->methods.size(); j++) {
      TyIdx methodtyidx = structty->methods[j].second.first;
      std::map<TyIdx, TyIdx>::iterator it = typeDefIdxMap.find(methodtyidx);
      if (it != typeDefIdxMap.end()) {
        structty->methods[j].second.first = it->second;
      }
    }
  }
}

void MIRParser::FixupForwardReferencedTypeByMap() {
  for (uint32 i = 1; i < GlobalTables::GetTypeTable().typeTable.size(); i++) {
    MIRType *ty = GlobalTables::GetTypeTable().typeTable[i];
    FixForwardReferencedTypeForOneAgg(ty);
  }
}

bool MIRParser::ParseTypeDef() {
  bool isLocal = paramParseLocalType;
  if (lexer.GetTokenKind() != TK_type) {
    Error("expect type but get ");
    return false;
  }

  TokenKind tk = lexer.NextToken();

  if (tk != TK_gname && tk != TK_lname) {
    Error("expect type name but get ");
    return false;
  }

  const std::string &name = lexer.GetName();
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(name);
  TyIdx prevTyidx;
  MIRStructType *prevStype = nullptr;
  TyIdx tyIdx(0);
  // dbginfo class/interface init
  DBGDie *die = nullptr;
  if (tk == TK_gname) {
    if (isLocal) {
      Error("A local type must use local type name ");
      return false;
    }
    prevTyidx = mod.typeNameTab->GetTyIdxFromGStrIdx(strIdx);
    if (prevTyidx != TyIdx(0)) {
      MIRType *prevType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(prevTyidx);
      prevStype = dynamic_cast<MIRStructType *>(prevType);
      if ((prevType->GetKind() != kTypeByName) && (prevStype && !prevStype->IsIncomplete())) {
        // allow duplicated type def if kKeepFirst is set which is the default
        if (options & kKeepFirst) {
          lexer.NextToken();
          Warning("redefined global type");
          if (!ParseDerivedType(tyIdx, kTypeUnknown)) {
            Error("error passing derived type at ");
            return false;
          }
          return true;
        } else {
          Error("redefined global type");
          return false;
        }
      }
    }
  } else {
    if (!isLocal) {
      Error("A global type must use global type name ");
      return false;
    }
    prevTyidx = mod.CurFunction()->typeNameTab->GetTyIdxFromGStrIdx(strIdx);
    if (prevTyidx != TyIdx(0)) {
      MIRType *prevType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(prevTyidx);
      prevStype = dynamic_cast<MIRStructType *>(prevType);
      if ((prevType->GetKind() != kTypeByName) && (prevStype && !prevStype->IsIncomplete())) {
        Error("redefined local type name ");
        return false;
      }
    }
  }
  // at this point,if prev_tyidx is not zero, this type name has been
  // forward-referenced
  tk = lexer.NextToken();
  tyIdx = TyIdx(0);
  if (IsPrimitiveType(tk)) {
    if (!ParsePrimType(tyIdx)) {
      Error("expect primitive type after typedef but get ");
      return false;
    }
  } else if (!ParseDerivedType(tyIdx, kTypeUnknown)) {
    Error("error passing derived type at ");
    return false;
  }
  // for class/interface types, prev_tyidx could also be set during processing
  // so we check again right before SetGStrIdxToTyIdx
  if (isLocal) {
    prevTyidx = mod.CurFunction()->typeNameTab->GetTyIdxFromGStrIdx(strIdx);
    mod.CurFunction()->typeNameTab->SetGStrIdxToTyIdx(strIdx, tyIdx);
    if (GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]->nameStrIdx == GStrIdx(0)) {
      GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]->nameStrIdx = strIdx;
      GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]->nameIsLocal = true;
    }
  } else {
    prevTyidx = mod.typeNameTab->GetTyIdxFromGStrIdx(strIdx);
    mod.typeNameTab->SetGStrIdxToTyIdx(strIdx, tyIdx);
    mod.typeDefOrder.push_back(strIdx);
    if (GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]->nameStrIdx == GStrIdx(0)) {
      GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]->nameStrIdx = strIdx;
    }
  }

  if (prevTyidx != TyIdx(0) && prevTyidx != tyIdx) {
    // replace all uses of prev_tyidx by tyIdx in typeTable
    typeDefIdxMap[prevTyidx] = tyIdx;                            // record the real tydix
    // remove prev_tyidx from classlist
    mod.RemoveClass(prevTyidx);
  }

  // Merge class or interface type at the cross-module level
  MIRType *type = GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()];
  if (!isLocal && (type->typeKind == kTypeClass || type->typeKind == kTypeClassIncomplete || type->typeKind == kTypeInterface ||
                   type->typeKind == kTypeInterfaceIncomplete)) {
    prevTyidx = GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(strIdx);
    if (prevTyidx == TyIdx(0)) {
      GlobalTables::GetTypeNameTable().SetGStrIdxToTyIdx(strIdx, tyIdx);
    } else {
      MIRStructType *sty = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]);
      MIRStructType *prevSty = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().typeTable.at(prevTyidx.GetIdx()));
      if (!sty->IsIncomplete()) {
        if (prevSty->IsIncomplete()) {
          // New definition is strong and previous definition is weak.
          GlobalTables::GetTypeNameTable().SetGStrIdxToTyIdx(strIdx, tyIdx);
        } else if (!prevSty->IsIncomplete() && !sty->IsIncomplete()) {
          // Both are strong, something must be wrong
          Error("redefined class/interface type name");
          return false;
        }
      }
    }
    // dbginfo class/interface build
    // setup eh root type
    MIRType *ehtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
    if (mod.throwableTyidx == 0 && (ehtype->typeKind == kTypeClass || ehtype->typeKind == kTypeClassIncomplete)) {
      GStrIdx ehtynameidx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(NameMangler::kJavaLangObjectStr);
      if (ehtynameidx == ehtype->nameStrIdx) {
        mod.throwableTyidx = tyIdx;
      }
    }
  }
  // dbginfo struct/union build
  return true;
}

bool MIRParser::ParseJavaClassInterface(MIRSymbol *symbol, bool isClass) {
  CHECK_FATAL(symbol != nullptr, "symbol is null");
  TokenKind tk = lexer.NextToken();
  if (tk != TK_gname) {
    Error("expect global name for javaclass but get ");
    return false;
  }
  symbol->SetNameStridx(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName()));
  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("ParseType failed trying parsing the type");
    return false;
  }
  MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  if (isClass && type->GetKind() != kTypeClass && type->GetKind() != kTypeClassIncomplete) {
    Error("type in javaclass declaration must be of class type at ");
    return false;
  } else if (!isClass && type->GetKind() != kTypeInterface && type->GetKind() != kTypeInterfaceIncomplete) {
    Error("type in javainterface declaration must be of interface type at ");
    return false;
  }
  symbol->SetTyIdx(tyIdx);
  if (!ParseTypeAttrs(symbol->typeAttrs)) {
    Error("bad type attribute in variable declaration at ");
    return false;
  }
  if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->HasTypeParam()) {
    symbol->SetAttr(ATTR_generic);
  }
  return true;
}

bool MIRParser::ParseStorageClass(MIRSymbol *st) {
  TokenKind tk = lexer.GetTokenKind();
  bool result = false;
  switch (tk) {
    case TK_fstatic:
      st->storageClass = kScFstatic;
      result = true;
      break;
    case TK_pstatic:
      st->storageClass = kScPstatic;
      result = true;
      break;
    case TK_extern:
      st->storageClass = kScExtern;
      result = true;
      break;
    default:
      break;
  }
  return result;
}

bool MIRParser::ParseDeclareReg(MIRSymbol *st, MIRFunction *curfunc) {
  TokenKind tk = lexer.GetTokenKind();
  // i.e, reg %1 u1
  if (tk != TK_reg) {  // reg
    Error("expect reg bug get ");
    return false;
  }
  TokenKind regnumtk = lexer.NextToken();
  if (regnumtk != TK_preg) {
    Error("expect preg but get");
    return false;
  }
  uint32 thepregno = static_cast<uint32>(lexer.GetTheIntVal());

  uint32 lnum = lexer.GetLineNum();
  lexer.NextToken();
  // parse ty
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("ParseDeclarePreg failed while parsing the type");
    return false;
  }
  CHECK_FATAL(tyIdx.GetIdx() > 0, "parse declare preg failed");

  if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->GetKind() == kTypeByName) {
    Error("type in var declaration cannot be forward-referenced at ");
    return false;
  }
  st->SetTyIdx(tyIdx);
  MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  PregIdx pregidx = curfunc->pregTab->EnterPregNo(thepregno, mirType->primType, mirType);
  MIRPregTable *pregTab = curfunc->pregTab;
  MIRPreg *preg = pregTab->PregFromPregIdx(pregidx);
  preg->primType = mirType->primType;
  st->value.preg = preg;
  if (!ParseVarTypeAttrs(st)) {
    Error("bad type attribute in variable declaration at ");
    return false;
  }
  if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->HasTypeParam()) {
    st->typeAttrs.SetAttr(ATTR_generic);
    mod.CurFunction()->funcAttrs.SetAttr(FUNCATTR_generic);
  }

  // add dbginfo
  return true;
}

bool MIRParser::ParseDeclareVar(MIRSymbol *st) {
  TokenKind tk = lexer.GetTokenKind();
  // i.e, var %i i32
  if (tk != TK_var && tk != TK_tempvar) {  // var
    Error("expect var but get ");
    return false;
  }
  bool isLocal = st->IsLocal();

  // %i
  TokenKind nameTk = lexer.NextToken();
  if (isLocal) {
    if (nameTk == TK_static) {
      st->storageClass = kScPstatic;
      nameTk = lexer.NextToken();
    }
    if (nameTk != TK_lname) {
      Error("expect local name but get ");
      return false;
    }
  } else {
    if (nameTk != TK_gname) {
      Error("expect global name but get ");
      return false;
    }
  }
  uint32 lnum = lexer.GetLineNum();
  GStrIdx symstrid = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  st->SetNameStridx(symstrid);

  tk = lexer.NextToken();

  if (ParseStorageClass(st)) {
    lexer.NextToken();
  }

  // i32
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("ParseDeclareVar failed when parsing the type");
    return false;
  }
  ASSERT(tyIdx.GetIdx() > 0, "parse declare var failed ");
  if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->GetKind() == kTypeByName) {
    Error("type in var declaration cannot be forward-referenced at ");
    return false;
  }
  st->SetTyIdx(tyIdx);
  if (!ParseVarTypeAttrs(st)) {
    Error("bad type attribute in variable declaration at ");
    return false;
  }
  if (st->storageClass == kScExtern && st->IsStatic()) {
    const std::string &staticfieldName = st->GetName();
    size_t pos = staticfieldName.find(NameMangler::kClassNameSplitterStr) + 3;
    if (pos != 0 && pos != std::string::npos) {
      std::string className = staticfieldName.substr(0, pos);
      MIRSymbol *classSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(className));
      if (classSt) {
        st->storageClass = kScGlobal;
      }
    }
  }
  if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->HasTypeParam()) {
    st->typeAttrs.SetAttr(ATTR_generic);
    if (isLocal) {
      mod.CurFunction()->funcAttrs.SetAttr(FUNCATTR_generic);
    }
  }

  tk = lexer.GetTokenKind();

  // take a look if there are any initialized values
  if (tk == TK_eqsign) {
    // parse initialized values
    MIRConst *mirconst = nullptr;
    lexer.NextToken();
    if (!ParseInitValue(mirconst, tyIdx)) {
      Error("wrong initialiaton value at ");
      return false;
    }
    st->SetConst(mirconst);
  }
  return true;
}

bool MIRParser::ParseDeclareFormal(FormalDef *formalDef) {
  TokenKind tk = lexer.GetTokenKind();
  if (tk != TK_var && tk != TK_reg) {
    return false;
  }
  TokenKind nameTk = lexer.NextToken();
  if (tk == TK_var) {
    if (nameTk != TK_lname) {
      Error("expect local name but get ");
      return false;
    }
    formalDef->formalStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  } else {  // tk == TK_reg
    if (nameTk != TK_preg) {
      Error("expect preg but get ");
      return false;
    }
    formalDef->formalStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(std::to_string(lexer.GetTheIntVal()));
  }
  tk = lexer.NextToken();
  if (!ParseType(formalDef->formalTyIdx)) {
    Error("ParseDeclareFormal failed when parsing the type");
    return false;
  }
  if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(formalDef->formalTyIdx)->GetKind() == kTypeByName) {
    Error("type in var declaration cannot be forward-referenced at ");
    return false;
  }
  if (!ParseTypeAttrs(formalDef->formalAttrs)) {
    Error("ParseDeclareFormal failed when parsing type attributes");
    return false;
  }
  return true;
}

bool MIRParser::ParsePrototype(MIRFunction *func, MIRSymbol *funcSt, TyIdx &funcTyidx) {
  if (lexer.GetTokenKind() == TK_lbrace) {
    if (mod.flavor < kMmpl) {
      Error("funcion prototype missing for non-MMPL flavor of Maple IR");
      return false;
    }
    // mmpl flavor has no prototype declaration, return normally
    return true;
  }

  std::vector<TyIdx> vecTy;    // for storing the parameter types
  std::vector<TypeAttrs> vecAt;  // for storing the parameter type attributes
  // this part for parsing the argument list and return type
  if (lexer.GetTokenKind() != TK_lparen) {
    Error("expect ( for func but get ");
    return false;
  }
  // parse parameters
  bool varargs = false;
  TokenKind pmTk = lexer.NextToken();

  while (pmTk != TK_rparen) {
    if (pmTk == TK_dotdotdot) {
      varargs = true;
      func->SetVarargs();
      pmTk = lexer.NextToken();
      if (pmTk != TK_rparen) {
        Error("expect ) after ... but get");
        return false;
      }
      break;
    } else {
      FormalDef formalDef;
      if (!ParseDeclareFormal(&formalDef)) {
        Error("ParsePrototype expects formal parameter declaration");
        return false;
      }
      func->formalDefVec.push_back(formalDef);
      vecTy.push_back(formalDef.formalTyIdx);
      vecAt.push_back(formalDef.formalAttrs);
      pmTk = lexer.GetTokenKind();
      if (pmTk == TK_coma) {
        pmTk = lexer.NextToken();
        if (pmTk == TK_rparen) {
          Error("\',\' cannot be followed by");
          return false;
        }
      }
    }
  }

  // parse return type
  lexer.NextToken();    // skip the right paren
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    return false;
  }
  MIRType *rettype = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx());
  switch (rettype->typeKind) {
    case kTypeUnion:
    case kTypeStruct:
    case kTypeStructIncomplete:
    case kTypeClass:
    case kTypeClassIncomplete:
    case kTypeInterface:
    case kTypeInterfaceIncomplete:
      func->SetReturnStruct();
      break;
    default:;
  }

  MIRType *funcType = GlobalTables::GetTypeTable().GetOrCreateFunctionType(&mod, tyIdx, vecTy, vecAt, varargs, true);
  funcTyidx = GlobalTables::GetTypeTable().GetOrCreateMIRType(funcType);
  funcSt->SetTyIdx(funcTyidx);
  func->funcType = static_cast<MIRFuncType*>(funcType);
  return true;
}

// if prototype declaration, only create the symbol and type;
// if a redeclaration, re-process the symbol and type declaration;
// if function declaration, link MIRFunction to ->mod in addition
bool MIRParser::ParseFunction(uint32 fileidx) {
  TokenKind tk = lexer.GetTokenKind();
  if (tk != TK_func) {
    Error("expect func but get ");
    return false;
  }
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_gname && lexer.GetTokenKind() != TK_fname) {
    Error("expect function name for func but get ");
    return false;
  }
  uint32 lnum = lexer.GetLineNum();

  const std::string &funcName = lexer.GetName();
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcName);
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(strIdx);
  MIRFunction *fn = nullptr;

  lexer.NextToken();
  FuncAttrs tA;
  if (!ParseFuncAttrs(tA)) {
    Error("bad function attribute in function declaration at ");
    return false;
  }
  if (funcSt) {
    // there has been an earlier forward declaration, so check consistency
    if (funcSt->storageClass != kScText || funcSt->sKind != kStFunc) {
      Error("redeclaration of name as func in ");
      return false;
    }
    if (funcSt->GetFunction()->body) {
      // Function definition has been processed. Here it may be
      // another declaration due to multi-mpl merge. If this
      // is indeed another definition, we will throw error.
      MIRSymbol *tmpSt = mod.memPool->New<MIRSymbol>();
      tmpSt->storageClass = kScText;
      tmpSt->appearsincode = true;
      tmpSt->sKind = kStFunc;
      MIRFunction *tmpFn = mod.memPool->New<MIRFunction>(&mod, tmpSt->GetStIdx());
      tmpSt->SetFunction(tmpFn);
      TyIdx tmpTyidx;
      if (!ParsePrototype(tmpFn, tmpSt, tmpTyidx)) {
        return false;
      }
      if (lexer.GetTokenKind() == TK_lbrace) {
        Error("function body defined second time in ");
        return false;
      }
      return true;
    }
    // Skip attribute checking
    fn = funcSt->value.mirFunc;
    fn->formalDefVec.clear();

    // update with current attr
    if (tA.attrFlag) {
      if (fn->IsIpaSeen()) {
        tA.SetAttr(FUNCATTR_ipaseen);
      }
      if (fn->IsNoDefEffect()) {
        tA.SetAttr(FUNCATTR_nodefeffect);
      }
      if (fn->IsPure()) {
        tA.SetAttr(FUNCATTR_pure);
      }
      if (fn->IsNoThrowException()) {
        tA.SetAttr(FUNCATTR_nothrow_exception);
      }
      if (fn->IsNoPrivateDefEffect()) {
        tA.SetAttr(FUNCATTR_noprivate_defeffect);
      }

      // t_a.SetAttr(FuncAttrKind x,bool un_set = false)
      fn->funcAttrs = tA;
    }
  } else {
    funcSt = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
    // when parsing func in mplt_inline file, set it as tmpunused.
    if (options & kParseInlineFuncBody) {
      funcSt->istmpunused = 1;
    }
    funcSt->SetNameStridx(strIdx);
    GlobalTables::GetGsymTable().AddToStringSymbolMap(funcSt);
    funcSt->storageClass = kScText;
    funcSt->sKind = kStFunc;
    SetSrcPos(funcSt->srcPosition, lexer.GetLineNum());

    fn = mod.memPool->New<MIRFunction>(&mod, funcSt->GetStIdx());
    fn->puIdx = GlobalTables::GetFunctionTable().funcTable.size();
    GlobalTables::GetFunctionTable().funcTable.push_back(fn);
    funcSt->SetFunction(fn);
    fn->funcAttrs = tA;
  }
  fn->fileIndex = fileidx;
  curFunc = fn;

  if (mod.IsJavaModule()) {
    fn->SetBaseClassFuncNames(funcSt->nameStrIdx);
  }

  TyIdx funcTyidx;
  if (!ParsePrototype(fn, funcSt, funcTyidx)) {
    return false;
  }

  bool retval = false;
  if (lexer.GetTokenKind() == TK_lbrace) {  // #2 parse Function body
    funcSt->appearsincode = true;
    definedLabels.clear();
    mod.SetCurFunction(fn);
    mod.AddFunction(fn);

    // set maple line number for function
    fn->GetFuncSymbol()->srcPosition.SetMplLinenum(lexer.GetLineNum());

    // initialize source line number to be 0
    // to avoid carrying over info from previous function
    firstLineNum = 0;
    lastLineNum = 0;

    fn->NewBody();
    BlockNode *block = nullptr;
    if (!ParseStmtBlock(block)) {
      Error("ParseFunction failed when parsing stmt block");
      retval = false;
      goto done;
    }
    fn->body = block;

    // set source file number for function
    fn->GetFuncSymbol()->srcPosition.SetLinenum(firstLineNum);
    fn->GetFuncSymbol()->srcPosition.SetFilenum(lastFileNum);

    // check if any local type name is undefined
    for (auto it : fn->typeNameTab->gStrIdxToTyIdxMap) {
      MIRType *type = GlobalTables::GetTypeTable().typeTable[it.second.GetIdx()];
      if (type->GetKind() == kTypeByName) {
        std::stringstream strStream;
        const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(it.first);
        strStream << "type %" << name << " used but not defined\n";
        message.append(strStream.str());
        retval = false;
        goto done;
      }
    }
    if (fn->classTyIdx == 0 && maple::ReplaceRetIgnored::IsClonedFunc(fn->GetName())) {
      // deal with cloned functions only, because they are not in mplt files, so their
      // classTyIdx are not correct
      auto classStridx = fn->GetBaseClassNameStridx();
      fn->classTyIdx.SetIdx(GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(classStridx).GetIdx());
    }
  }
  retval = true;
done:
  ResetCurrentFunction();
  return retval;
}



bool MIRParser::ParseInitValue(MIRConst *&theconst, TyIdx tyIdx) {
  TokenKind tk = lexer.GetTokenKind();
  MIRType *type = GlobalTables::GetTypeTable().typeTable.at(tyIdx.GetIdx());
  if (tk != TK_lbrack) {  // scalar
    MIRConst *aconst = nullptr;
    if (IsConstValue(tk)) {
      if (!ParseScalarValue(aconst, type)) {
        Error("ParseInitValue expect scalar value");
        return false;
      }
      lexer.NextToken();
    } else if (IsConstAddrExpr(tk)) {
      if (!ParseConstAddrLeafExpr(aconst, type)) {
        Error("ParseInitValue expect const addr expr");
        return false;
      }
    } else {
      Error("initialiation value expected but get ");
      return false;
    }
    theconst = aconst;
  } else {  // aggregates
    FixForwardReferencedTypeForOneAgg(type);
    if (type->typeKind == kTypeArray) {
      MIRArrayType *atype = static_cast<MIRArrayType *>(type);
      MIRType *etype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(atype->eTyIdx);
      MIRAggConst *newconst = mod.memPool->New<MIRAggConst>(&mod, type);
      theconst = newconst;
      MapleVector<MIRConst *> &constvec = newconst->constVec;
      tk = lexer.NextToken();
      if (tk == TK_rbrack) {
        Error("illegal empty initialization for array at ");
        return false;
      }
      do {
        // parse single const or another dimension array
        MIRConst *subconst = nullptr;
        if (IsConstValue(tk)) {
          if (!ParseScalarValue(subconst, etype)) {
            Error("ParseInitValue expect scalar value");
            return false;
          }
          lexer.NextToken();
        } else if (IsConstAddrExpr(tk)) {
          if (!ParseConstAddrLeafExpr(subconst, type)) {
            Error("ParseInitValue expect const addr expr");
            return false;
          }
        } else if (tk == TK_lbrack) {
          if (etype->typeKind == kTypeStruct && atype->dim == 1) {
            if (!ParseInitValue(subconst, atype->eTyIdx)) {
              Error("initializaton value wrong when parsing structure array ");
              return false;
            }
          } else {
            TyIdx elemTyIdx;
            if (atype->dim == 1) {
              elemTyIdx = etype->tyIdx;
            } else {
              vector<uint32> sizeSubArray;
              for (int32 i = 1; i < atype->dim; i++) {
                sizeSubArray.push_back(atype->sizeArray[i]);
              }
              MIRArrayType subAtype(etype->tyIdx, sizeSubArray);
              elemTyIdx = GlobalTables::GetTypeTable().GetOrCreateMIRType(&subAtype);
            }
            if (!ParseInitValue(subconst, elemTyIdx)) {
              Error("initializaton value wrong when parsing sub array ");
              return false;
            }
          }
        } else {
          Error("expect const value or group of const values but get ");
          return false;
        }
        constvec.push_back(subconst);
        // parse comma or rbrack
        tk = lexer.GetTokenKind();
        if (tk == TK_coma) {
          tk = lexer.NextToken();
          if (tk == TK_rbrack) {
            Error("not expect, followed by ] ");
            return false;
          }
        }
      } while (tk != TK_rbrack);
      lexer.NextToken();
    } else if (type->typeKind == kTypeStruct || type->typeKind == kTypeUnion) {
      MIRAggConst *newconst = mod.memPool->New<MIRAggConst>(&mod, type);
      uint32 thefieldidx;
      TyIdx fieldtyidx;
      theconst = newconst;
      MapleVector<MIRConst *> &constvec = newconst->constVec;
      tk = lexer.NextToken();
      if (tk == TK_rbrack) {
        Error("illegal empty initialization for struct at ");
        return false;
      }
      do {
        if (lexer.GetTokenKind() != TK_intconst) {
          Error("expect field ID in struct initialization but get ");
          return false;
        }
        thefieldidx = lexer.GetTheIntVal();
        if (lexer.NextToken() != TK_eqsign) {
          Error("expect = after field ID in struct initialization but get ");
          return false;
        }
        FieldPair thepair = static_cast<MIRStructType *>(type)->fields[thefieldidx-1];
        fieldtyidx = thepair.second.first;
        if (fieldtyidx == 0) {
          Error("field ID out of range at struct initialization at ");
          return false;
        }
        tk = lexer.NextToken();
        MIRConst *subconst = nullptr;
        if (IsConstValue(tk)) {
          if (!ParseScalarValue(subconst, GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx))) {
            Error("ParseInitValue expect scalar value");
            return false;
          }
          lexer.NextToken();
        } else if (IsConstAddrExpr(tk)) {
          if (!ParseConstAddrLeafExpr(subconst, type)) {
            Error("ParseInitValue expect const addr expr");
            return false;
          }
        } else if (tk == TK_lbrack) {
          if (!ParseInitValue(subconst, fieldtyidx)) {
            Error("parse init value wrong when parse sub struct ");
            return false;
          }
        } else {
          Error("expect const value or group of const values but get ");
          return false;
        }
        CHECK_FATAL(subconst != nullptr, "subconst is null in MIRParser::ParseInitValue");
        subconst->fieldID = thefieldidx;
        constvec.push_back(subconst);
        tk = lexer.GetTokenKind();
        // parse comma or rbrack
        if (tk == TK_coma) {
          tk = lexer.NextToken();
          if (tk == TK_rbrack) {
            Error("not expect \',\' followed by ] ");
            return false;
          }
        }
      } while (tk != TK_rbrack);
      lexer.NextToken();
    } else {
      Error("non-struct should not have aggregate initialization in ");
      return false;
    }
  }
  return true;
}

bool MIRParser::ParseFileinfo(void) {
  if (lexer.GetTokenKind() != TK_lbrace) {
    Error("expect left brace after fileInfo but get ");
    return false;
  }
  TokenKind tk = lexer.NextToken();
  while (tk == TK_label) {
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
    tk = lexer.NextToken();
    if (tk == TK_intconst) {
      uint32 fieldval = lexer.GetTheIntVal();
      mod.fileInfo.push_back(MIRInfoPair(strIdx, fieldval));
      mod.fileInfoIsString.push_back(false);
    } else if (tk == TK_string) {
      GStrIdx litstridx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
      mod.fileInfo.push_back(MIRInfoPair(strIdx, litstridx.GetIdx()));
      mod.fileInfoIsString.push_back(true);
    } else {
      Error("illegal value after fileInfo field name at ");
      return false;
    }
    tk = lexer.NextToken();
    if (tk == TK_coma) {
      tk = lexer.NextToken();
    } else if (tk == TK_rbrace) {
      lexer.NextToken();
      return true;
    } else {
      Error("expect comma after fileInfo field value but get ");
      return false;
    }
  }
  Error("expect field name in fileInfo but get ");
  return false;
}

bool MIRParser::ParseFiledata(void) {
  if (lexer.GetTokenKind() != TK_lbrace) {
    Error("expect left brace after fileData but get ");
    return false;
  }
  TokenKind tk = lexer.NextToken();
  while (tk == TK_label) {
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
    tk = lexer.NextToken();
    std::vector<uint8> data;
    while (tk == TK_intconst) {
      uint32 fieldval = lexer.GetTheIntVal();
      data.push_back(fieldval);
      tk = lexer.NextToken();
    }
    mod.fileData.push_back(MIRDataPair(strIdx, data));
    if (tk == TK_coma) {
      tk = lexer.NextToken();
    } else if (tk == TK_rbrace) {
      lexer.NextToken();
      return true;
    } else {
      Error("expect comma after fileData field value but get ");
      return false;
    }
  }
  Error("expect field name in fileData but get ");
  return false;
}

bool MIRParser::ParseSrcFileinfo(void) {
  if (lexer.GetTokenKind() != TK_lbrace) {
    Error("expect left brace after fileInfo but get ");
    return false;
  }
  TokenKind tk = lexer.NextToken();
  while (tk == TK_intconst) {
    uint32 fieldval = lexer.GetTheIntVal();
    tk = lexer.NextToken();
    if (tk == TK_string) {
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
      mod.srcFileInfo.push_back(MIRInfoPair(strIdx, fieldval));
    } else {
      Error("illegal value after srcFileInfo field name at ");
      return false;
    }
    tk = lexer.NextToken();
    if (tk == TK_coma) {
      tk = lexer.NextToken();
    } else if (tk == TK_rbrace) {
      lexer.NextToken();
      return true;
    } else {
      Error("expect comma after srcFileInfo field value but get ");
      return false;
    }
  }
  Error("expect field name in srcFileInfo but get ");
  return false;
}

bool MIRParser::ParseFuncinfo(void) {
  if (lexer.GetTokenKind() != TK_lbrace) {
    Error("expect left brace after funcinfo but get ");
    return false;
  }
  MIRFunction *fn = mod.CurFunction();
  TokenKind tk = lexer.NextToken();
  while (tk == TK_label) {
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
    tk = lexer.NextToken();
    if (tk == TK_intconst) {
      uint32 fieldval = lexer.GetTheIntVal();
      fn->info.push_back(MIRInfoPair(strIdx, fieldval));
      fn->infoIsString.push_back(false);
    } else if (tk == TK_string) {
      GStrIdx litstridx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
      fn->info.push_back(MIRInfoPair(strIdx, litstridx.GetIdx()));
      fn->infoIsString.push_back(true);
    } else {
      Error("illegal value after funcinfo field name at ");
      return false;
    }
    tk = lexer.NextToken();
    if (tk == TK_coma) {
      tk = lexer.NextToken();
    } else if (tk == TK_rbrace) {
      lexer.NextToken();
      return true;
    } else {
      Error("expect comma after funcinfo field value but get ");
      return false;
    }
  }
  Error("expect field name in funcinfo but get ");
  return false;
}

bool MIRParser::ParseAlias(StmtNode *&stmt) {
  TokenKind nameTk = lexer.NextToken();
  if (nameTk != TK_lname) {
    Error("expect local in ALIAS but get ");
    return false;
  }
  GStrIdx srcstrid = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  nameTk = lexer.NextToken();
  if (nameTk != TK_lname) {
    Error("expect local in ALIAS but get ");
    return false;
  }
  GStrIdx mplstrid = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  lexer.NextToken();
  TyIdx tyIdx(0);
  if (!ParseType(tyIdx)) {
    Error("parseType failed when parsing ALIAS ");
    return false;
  }
  GStrIdx sigstrid(0);
  if (lexer.GetTokenKind() == TK_string) {
    // ignore the signature string
    lexer.NextToken();
  }
  MIRAliasVars aliasvar;
  aliasvar.memPoolStrIdx = mplstrid;
  aliasvar.tyIdx = tyIdx;
  aliasvar.sigStrIdx = sigstrid;
  mod.CurFunction()->aliasVarMap[srcstrid] = aliasvar;
  return true;
}

uint8 *MIRParser::ParseWordsInfo(uint32 size) {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_eqsign) {
    Error("expect = after it but get ");
    return nullptr;
  }
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_lbrack) {
    Error("expect [ for it but get ");
    return nullptr;
  }
  uint8 *origp = static_cast<uint8*>(mod.memPool->Malloc(BlkSize2BitvectorSize(size)));
  // initialize it based on the input
  for (uint32 *p = reinterpret_cast<uint32*>(origp); lexer.NextToken() == TK_intconst; p++) {
    *p = static_cast<uint32>(lexer.GetTheIntVal());
  }
  if (lexer.GetTokenKind() != TK_rbrack) {
    Error("expect ] at end of globalwordstypetagged but get ");
    return nullptr;
  }
  lexer.NextToken();
  return origp;
}

void MIRParser::DumpOptFuncMap() {}

static void GenJStringType(MIRModule *module) {
  MIRStructType metaClassType(kTypeStructIncomplete);
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("dummy");
  metaClassType.fields.push_back(FieldPair(strIdx, TyidxFieldAttrPair(TyIdx(PTY_ref), FieldAttrs())));
  strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("__class_meta__");
  TyIdx tyIdx = GlobalTables::GetTypeTable().CreateMIRType(&metaClassType);
  // Global?
  module->typeNameTab->SetGStrIdxToTyIdx(strIdx, tyIdx);
  if (GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]->nameStrIdx == GStrIdx(0)) {
    GlobalTables::GetTypeTable().typeTable[tyIdx.GetIdx()]->nameStrIdx = strIdx;
  }
}

bool MIRParser::ParseMIR(std::ifstream *mplfile) {
  std::ifstream *origfile = lexer.GetFile();

  // parse mplfile
  lexer.SetFile(mplfile);

  // try to read the first line
  if (lexer.ReadALine() < 0) {
    lexer.lineNum = 0;
  } else {
    lexer.lineNum = 1;
  }

  // for optimized functions file
  bool status = ParseMIR(0, kParseOptFunc);
  DumpOptFuncMap();

  // restore airFile
  lexer.SetFile(origfile);

  return status;
}

bool MIRParser::ParseInlineFuncBody(std::ifstream *mplfile) {
  std::ifstream *origfile = lexer.GetFile();
  lexer.SetFile(mplfile);
  // try to read the first line
  if (lexer.ReadALine() < 0) {
    lexer.lineNum = 0;
  } else {
    lexer.lineNum = 1;
  }
  // parse mplfile
  bool status = ParseMIR(0, kParseOptFunc | kParseInlineFuncBody);
  // restore airFile
  lexer.SetFile(origfile);
  return status;
}

bool MIRParser::ParseMIR(uint32 fileIdx, uint32 option, bool isIPA, bool isComb) {
  if ((option & kParseOptFunc) == 0) {
    PrepareParsingMIR();
  }
  if (dummyFunction == nullptr) {
    dummyFunction = CreateDummyFunction();
    mod.SetCurFunction(static_cast<mir_func_t *>(dummyFunction));
  }

  if (option) {
    options |= option;
  }

  // profiling setup
  mod.withProfileInfo = ((options & kWithProfileInfo) != 0);
  bool atEof = false;
  paramFileIdx = fileIdx;
  paramIsIPA = isIPA;
  paramIsComb = isComb;
  lexer.NextToken();
  while (!atEof) {
    paramTokenKind = lexer.GetTokenKind();
    std::map<TokenKind, FuncPtrParseMIRForElem>::iterator itFuncPtr = funcPtrMapForParseMIR.find(paramTokenKind);
    if (itFuncPtr == funcPtrMapForParseMIR.end()) {
      if (paramTokenKind == TK_eof) {
        atEof = true;
      } else {
        Error("expect func or var but get ");
        return false;
      }
    } else {
      if (!(this->*(itFuncPtr->second))()) {
        return false;
      }
    }
  }

  // fix the typedef type
  FixupForwardReferencedTypeByMap();
  // check if any global type name is undefined
  for (MapleMap<GStrIdx, TyIdx>::iterator it = mod.typeNameTab->gStrIdxToTyIdxMap.begin();
       it != mod.typeNameTab->gStrIdxToTyIdxMap.end(); it++) {
    MIRType *type = GlobalTables::GetTypeTable().typeTable[it->second.GetIdx()];
    if (type->GetKind() == kTypeByName) {
      std::stringstream strStream;
      const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(it->first);
      if (name == "__class_meta__") {
        continue;
      }
      strStream << "type $" << name << " used but not defined\n";
      message.append(strStream.str());
      return false;
    }
  }
  if (!isIPA && isComb) {
    for (auto it = paramImportFileList.begin(); it != paramImportFileList.end(); it++) {
      BinaryMplt binMplt(mod);
      std::string importfilename = *it;
      if (importfilename.compare(importfilename.length() - strlen(".so"), strlen(".so"), ".so") == 0) {
        continue; // skip .so file
      }
      if (!binMplt.Import(importfilename, false, true)) {  // not a binary mplt
        std::ifstream mpltFile(importfilename);
        if (!mpltFile.is_open()) {
          FATAL(kLncFatal, "cannot open MPLT file: %s\n", importfilename.c_str());
        }
        bool failedParse = !ParseMPLT(&mpltFile, importfilename);
        mpltFile.close();
        if (failedParse) {
          return false;
        }
      }
    }
  }

  if (options & kCheckCompleteType) {
    CollectIncompleteTypes();
  }

  return true;
}

std::map<TokenKind, MIRParser::FuncPtrParseMIRForElem> MIRParser::InitFuncPtrMapForParseMIR() {
  std::map<TokenKind, MIRParser::FuncPtrParseMIRForElem> funcPtrMap;
  funcPtrMap[TK_func] = &MIRParser::ParseMIRForFunc;
  funcPtrMap[TK_tempvar] = &MIRParser::ParseMIRForVar;
  funcPtrMap[TK_var] = &MIRParser::ParseMIRForVar;
  funcPtrMap[TK_javaclass] = &MIRParser::ParseMIRForClass;
  funcPtrMap[TK_javainterface] = &MIRParser::ParseMIRForInterface;
  funcPtrMap[TK_type] = &MIRParser::ParseTypeDef;
  funcPtrMap[TK_flavor] = &MIRParser::ParseMIRForFlavor;
  funcPtrMap[TK_srclang] = &MIRParser::ParseMIRForSrcLang;
  funcPtrMap[TK_globalmemsize] = &MIRParser::ParseMIRForGlobalMemSize;
  funcPtrMap[TK_globalmemmap] = &MIRParser::ParseMIRForGlobalMemMap;
  funcPtrMap[TK_globalwordstypetagged] = &MIRParser::ParseMIRForGlobalWordsTypeTagged;
  funcPtrMap[TK_globalwordsrefcounted] = &MIRParser::ParseMIRForGlobalWordsRefCounted;
  funcPtrMap[TK_id] = &MIRParser::ParseMIRForID;
  funcPtrMap[TK_numfuncs] = &MIRParser::ParseMIRForNumFuncs;
  funcPtrMap[TK_entryfunc] = &MIRParser::ParseMIRForEntryFunc;
  funcPtrMap[TK_fileinfo] = &MIRParser::ParseMIRForFileInfo;
  funcPtrMap[TK_filedata] = &MIRParser::ParseMIRForFileData;
  funcPtrMap[TK_srcfileinfo] = &MIRParser::ParseMIRForSrcFileInfo;
  funcPtrMap[TK_import] = &MIRParser::ParseMIRForImport;
  funcPtrMap[TK_importpath] = &MIRParser::ParseMIRForImportPath;
  funcPtrMap[TK_LOC] = &MIRParser::ParseLoc;
  return funcPtrMap;
}

bool MIRParser::ParseMIRForFunc() {
  curFunc = nullptr;
  if (!ParseFunction(paramFileIdx)) {
    return false;
  }
  if ((this->options & kParseOptFunc) && curFunc) {
    curFunc->SetAttr(FUNCATTR_optimized);
    mod.optimizedFuncs.push_back(curFunc);
  }
  return true;
}

bool MIRParser::ParseMIRForVar() {
  MIRSymbol st(0, kScopeGlobal);
  st.SetStorageClass(kScGlobal);
  st.sKind = kStVar;
  if (paramTokenKind == TK_tempvar) {
    st.isTmp = true;
  }
  if (!ParseDeclareVar(&st)) {
    return false;
  }
  MIRSymbol *prevSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(st.GetNameStridx());
  if (prevSt != nullptr) {
    if (prevSt->GetTyIdx() == st.GetTyIdx() &&
        (prevSt->GetStorageClass() == st.GetStorageClass() || prevSt->GetStorageClass() == kScExtern ||
         st.GetStorageClass() == kScExtern) &&
        prevSt->sKind == st.sKind && (prevSt->GetConst() == nullptr || st.GetConst() == nullptr)) {
      // previously declared: accumulate new information to the symbol
      prevSt->typeAttrs.attrFlag = prevSt->typeAttrs.attrFlag | st.typeAttrs.attrFlag;
      if (prevSt->GetConst() == nullptr) {
        prevSt->SetConst(st.GetConst());
      }
      if (prevSt->GetStorageClass() == kScExtern) {
        prevSt->SetStorageClass(st.GetStorageClass());
      }
    } else {
    }
  } else {  // seeing the first time
    maple::MIRBuilder mirBuilder(&mod);
    MIRSymbol *newst = mirBuilder.CreateSymbol(st.GetTyIdx(), st.GetNameStridx(), st.sKind, st.GetStorageClass(),
                                               kScopeGlobal, nullptr);
    newst->SetAttrs(st.GetAttrs());
    newst->SetNameStridx(st.GetNameStridx());
    newst->value = st.value;
    SetSrcPos(newst->srcPosition, lexer.GetLineNum());
  }
  return true;
}

bool MIRParser::ParseMIRForClass() {
  MIRSymbol *st = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  st->SetStorageClass(kScInvalid);
  st->sKind = kStJavaClass;
  if (!ParseJavaClassInterface(st, true)) {
    return false;
  }
  if (!GlobalTables::GetGsymTable().AddToStringSymbolMap(st)) {
    Error("duplicate symbol name used in javaclass at ");
    return false;
  }
  return true;
}

bool MIRParser::ParseMIRForInterface() {
  MIRSymbol *st = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
  st->SetStorageClass(kScInvalid);
  st->sKind = kStJavaInterface;
  if (!ParseJavaClassInterface(st, false)) {
    return false;
  }
  if (!GlobalTables::GetGsymTable().AddToStringSymbolMap(st)) {
    Error("duplicate symbol name used in javainterface at ");
    return false;
  }
  return true;
}

bool MIRParser::ParseMIRForFlavor() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect integer after flavor but get ");
    return false;
  }
  mod.flavor = (MIRFlavor)lexer.GetTheIntVal();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseMIRForSrcLang() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect integer after srclang but get ");
    return false;
  }
  mod.srcLang = (MIRSrcLang)lexer.GetTheIntVal();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseMIRForGlobalMemSize() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect integer after globalmemsize but get ");
    return false;
  }
  mod.globalMemSize = lexer.GetTheIntVal();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseMIRForGlobalMemMap() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_eqsign) {
    Error("expect = after globalmemmap but get ");
    return false;
  }
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_lbrack) {
    Error("expect [ for globalmemmap but get ");
    return false;
  }
  mod.globalBlkMap = static_cast<uint8*>(mod.memPool->Malloc(mod.globalMemSize));
  // initialize globalblkmap based on the input
  for (uint32 *p = reinterpret_cast<uint32*>(mod.globalBlkMap); lexer.NextToken() == TK_intconst; p++) {
    *p = static_cast<uint32>(lexer.GetTheIntVal());
  }
  if (lexer.GetTokenKind() != TK_rbrack) {
    Error("expect ] at end of globalmemmap but get ");
    return false;
  }
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseMIRForGlobalWordsTypeTagged() {
  uint8 *typeAddr = ParseWordsInfo(mod.globalMemSize);
  if (typeAddr == nullptr) {
    Error("parser error for globalwordstypetagged");
    return false;
  }
  mod.globalWordsTypeTagged = typeAddr;
  return true;
}

bool MIRParser::ParseMIRForGlobalWordsRefCounted() {
  uint8 *refAddr = ParseWordsInfo(mod.globalMemSize);
  if (refAddr == nullptr) {
    Error("parser error for globalwordsrefcounted");
    return false;
  }
  mod.globalWordsRefCounted = refAddr;
  return true;
}

bool MIRParser::ParseMIRForID() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect integer after id but get ");
    return false;
  }
  mod.id = lexer.GetTheIntVal();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseMIRForNumFuncs() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_intconst) {
    Error("expect integer after numfuncs but get ");
    return false;
  }
  mod.numFuncs = lexer.GetTheIntVal();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseMIRForEntryFunc() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_fname) {
    Error("expect function name for func but get ");
    return false;
  }
  mod.entryFuncName = lexer.GetName();
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseMIRForFileInfo() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_lbrace) {
    Error("expect left brace after fileInfo but get ");
    return false;
  }
  TokenKind tk = lexer.NextToken();
  while (tk == TK_label) {
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
    tk = lexer.NextToken();
    if (tk == TK_intconst) {
      uint32 fieldval = lexer.GetTheIntVal();
      mod.fileInfo.push_back(MIRInfoPair(strIdx, fieldval));
      mod.fileInfoIsString.push_back(false);
    } else if (tk == TK_string) {
      GStrIdx litstridx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
      mod.fileInfo.push_back(MIRInfoPair(strIdx, litstridx.GetIdx()));
      mod.fileInfoIsString.push_back(true);
    } else {
      Error("illegal value after fileInfo field name at ");
      return false;
    }
    tk = lexer.NextToken();
    if (tk == TK_rbrace) {
      lexer.NextToken();
      return true;
    }
    if (tk == TK_coma) {
      tk = lexer.NextToken();
    } else {
      Error("expect comma after fileInfo field value but get ");
      return false;
    }
  }
  Error("expect field name in fileInfo but get ");
  return false;
}

bool MIRParser::ParseMIRForFileData() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_lbrace) {
    Error("expect left brace after fileData but get ");
    return false;
  }
  TokenKind tk = lexer.NextToken();
  while (tk == TK_label) {
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
    tk = lexer.NextToken();
    std::vector<uint8> data;
    while (tk == TK_intconst) {
      uint32 fieldval = lexer.GetTheIntVal();
      data.push_back(fieldval);
      tk = lexer.NextToken();
    }
    mod.fileData.push_back(MIRDataPair(strIdx, data));
    if (tk == TK_coma) {
      tk = lexer.NextToken();
    } else if (tk == TK_rbrace) {
      lexer.NextToken();
      return true;
    } else {
      Error("expect comma after fileData field value but get ");
      return false;
    }
  }
  Error("expect field name in fileData but get ");
  return false;
}

bool MIRParser::ParseMIRForSrcFileInfo() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_lbrace) {
    Error("expect left brace after fileInfo but get ");
    return false;
  }
  TokenKind tk = lexer.NextToken();
  while (tk == TK_intconst) {
    uint32 fieldval = lexer.GetTheIntVal();
    tk = lexer.NextToken();
    if (tk == TK_string) {
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
      mod.srcFileInfo.push_back(MIRInfoPair(strIdx, fieldval));
    } else {
      Error("illegal value after srcfileinfo field name at ");
      return false;
    }
    tk = lexer.NextToken();
    if (tk == TK_rbrace) {
      lexer.NextToken();
      return true;
    }
    if (tk == TK_coma) {
      tk = lexer.NextToken();
    } else {
      Error("expect comma after srcfileinfo field value but get ");
      return false;
    }
  }
  Error("expect field name in srcfileinfo but get ");
  return false;
}

bool MIRParser::ParseMIRForImport() {
  bool firstImport = true;
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_string) {
    Error("expect file name string after import but get ");
    return false;
  }
  std::string importFileName = lexer.GetName();
  paramImportFileList.push_back(importFileName);
  // check illegal file name for the .mplt/.so file
  std::string::size_type lastDot = importFileName.find_last_of(".");
  bool isSo = false;  // import file is .so
  if (lastDot == std::string::npos) {
    FATAL(kLncFatal, "MPLT file has no suffix: %s\n", importFileName.c_str());
  }
  if (lastDot == importFileName.length() - strlen(".mplt") && importFileName.compare(lastDot, strlen(".mplt"), ".mplt") == 0) {
    // suffix is .mplt
  }
  else if (lastDot == importFileName.length() - strlen(".so") && importFileName.compare(lastDot, strlen(".so"), ".so") == 0) {
    isSo = true;
  }
  else {
    FATAL(kLncFatal, "Import file has wrong suffix: %s\n", importFileName.c_str());
  }

  if (paramIsIPA && firstImport) {
    BinaryMplt *binMplt = new BinaryMplt(mod);
    mod.binMplt = binMplt;
    INFO(kLncInfo, "importing %s", importFileName.c_str());
    if (!binMplt->Import(importFileName, paramIsIPA && !firstImport, paramIsComb)) {  // not a binary mplt
      std::ifstream mpltFile(importFileName);
      if (!mpltFile.is_open()) {
        FATAL(kLncFatal, "cannot open MPLT file: %s\n", importFileName.c_str());
      }
      bool failedParse = !ParseMPLT(&mpltFile, importFileName);
      mpltFile.close();
      if (failedParse) {  // parse the mplt file
        return false;
      }
      firstImport = false;
    } else {
      INFO(kLncInfo, "finished import of %s", importFileName.c_str());
    }
  } else {
    if (isSo == false) {
      BinaryMplt binMplt(mod);
      INFO(kLncInfo, "importing %s", importFileName.c_str());
      if (!binMplt.Import(importFileName, paramIsIPA, false)) {  // not a binary mplt
        std::ifstream mpltFile(importFileName);
        if (!mpltFile.is_open()) {
          FATAL(kLncFatal, "cannot open MPLT file: %s\n", importFileName.c_str());
        }
        bool failedParse = !ParseMPLT(&mpltFile, importFileName);
        mpltFile.close();
        if (failedParse) {  // parse the mplt file
          return false;
        }
      } else {
        INFO(kLncInfo, "finished import of %s", importFileName.c_str());
      }
    }
  }
  if (GlobalTables::GetStrTable().GetStrIdxFromName("__class_meta__") == 0) {
    GenJStringType(&mod);
  }
  GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(importFileName);
  mod.importFiles.push_back(strIdx);
  // record the imported file for later reading summary info, if exists
  mod.importedMplt.push_back(importFileName);
  lexer.NextToken();
  return true;
}

bool MIRParser::ParseMIRForImportPath() {
  lexer.NextToken();
  if (lexer.GetTokenKind() != TK_string) {
    Error("expect path string after importpath but get ");
    return false;
  }
  GStrIdx litStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(lexer.GetName());
  mod.importPaths.push_back(litStrIdx);
  lexer.NextToken();
  return true;
}

void MIRParser::PrepareParsingMIR() {
  dummyFunction = CreateDummyFunction();
  mod.SetCurFunction(static_cast<mir_func_t *>(dummyFunction));
  lexer.PrepareForFile(mod.fileName.c_str());
}

bool MIRParser::ParseTypeFromString(std::string &src, TyIdx &tyIdx) {
  lexer.PrepareForString(src);
  return ParseType(tyIdx);
}

bool MIRParser::ParseMPLT(std::ifstream *mpltfile, const std::string &importFileName) {
  // save relevant values for the main input file
  std::ifstream *airFileSave = lexer.airFile;
  int linenumSave = lexer.lineNum;
  std::string modFileNameSave = mod.fileName;
  // set up to read next line from the import file
  lexer.curIdx = 0;
  lexer.currentLineSize = 0;
  lexer.airFile = mpltfile;
  lexer.lineNum = 0;
  mod.fileName = importFileName;

  bool atEof = false;
  lexer.NextToken();
  while (!atEof) {
    TokenKind tk = lexer.GetTokenKind();
    switch (tk) {
      default: {
        Error("expect func or var but get ");
        return false;
      }
      case TK_eof:
        atEof = true;
        break;
      case TK_type:
        paramParseLocalType = false;
        if (!ParseTypeDef()) {
          return false;
        }
        break;
      case TK_var: {
        tk = lexer.NextToken();
        if (tk == TK_gname) {
          string literalName = lexer.name;
          GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(literalName);
          GlobalTables::GetConstPool().PutLiteralNameAsImported(strIdx);
          lexer.NextToken();
        } else {
          return false;
        }
        break;
      }
    }
  }

  // restore old values to continue reading from the main input file
  lexer.curIdx = 0;  // to force reading new line
  lexer.currentLineSize = 0;
  lexer.lineNum = linenumSave;
  lexer.airFile = airFileSave;
  mod.fileName = modFileNameSave;
  return true;
}

bool MIRParser::ParseMPLTstandalone(std::ifstream *mpltfile, std::string &importFileName) {
  PrepareParsingMIR();
  if (!ParseMPLT(mpltfile, importFileName)) {
    return false;
  }
  lexer.lineNum = 0;

  // fix the typedef type
  FixupForwardReferencedTypeByMap();

  return true;
}

void MIRParser::CollectIncompleteTypes() {
  for (auto tyIdx : mod.classList) {
    MIRStructType *stype = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().typeTable[tyIdx]);
    if (stype->IsIncomplete()) {
      incompleteTypes.insert(tyIdx);
    }
  }
}

void MIRParser::EmitIncompleteTypes() {
  if (!incompleteTypes.size()) {
    return;
  }
  for (auto tyIdx : incompleteTypes) {
    MIRStructType *stype = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().typeTable[tyIdx]);
    if (stype->IsIncomplete()) {
      const std::string kName = GlobalTables::GetStrTable().GetStringFromStrIdx(stype->nameStrIdx);
    }
  }
}

void MIRParser::EmitError(const std::string &fileName) {
  if (!strlen(GetError().c_str())) {
    return;
  }
  mod.dbgInfo->compilemsg_->EmitMsg();
  ERR(kLncErr, "%s \n%s", fileName.c_str(), GetError().c_str());
}

void MIRParser::EmitWarning(const std::string &fileName) {
  if (options & kCheckCompleteType) {
    EmitIncompleteTypes();
  }
  if (!strlen(GetWarning().c_str())) {
    return;
  }
  WARN(kLncWarn, "%s \n%s\n", fileName.c_str(), GetWarning().c_str());
}

}  // namespace maple
