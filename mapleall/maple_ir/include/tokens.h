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

#ifndef MAPLE_IR_INCLUDE_TOKENS_H
#define MAPLE_IR_INCLUDE_TOKENS_H

namespace maple {
enum TokenKind {
  TK_invalid,
// keywords from this file
#define KEYWORD(STR) TK_##STR,
#include "keywords.def"
#undef KEYWORD
  // non-keywords starting here
  // constants
  TK_intconst,
  TK_floatconst,
  TK_doubleconst,
  // local name
  TK_lname,
  // global name
  TK_gname,
  // function name
  TK_fname,
  // pseudo register
  TK_preg,
  // special register
  TK_specialreg,
  // parent field
  TK_prntfield,
  // type parameter name
  TK_typeparam,
  // misc.
  TK_newline,
  TK_lparen,     // (
  TK_rparen,     // )
  TK_lbrace,     // {
  TK_rbrace,     // }
  TK_lbrack,     // [
  TK_rbrack,     // ]
  TK_langle,     // <
  TK_rangle,     // >
  TK_eqsign,     // =
  TK_coma,       // ,
  TK_dotdotdot,  // ...
  TK_colon,      // :
  TK_asterisk,   // *
  TK_string,     // a literal string enclosed between "
  TK_eof
};

}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_TOKENS_H
