/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
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

#include "mir_builder.h"
#include "printing.h"
#include "maple_string.h"
#include "name_mangler.h"
#include "debug_info.h"
#include "global_tables.h"
#include "mir_type.h"
#include <cstring>
#include "securec.h"
#include "mpl_logging.h"

namespace maple {

// utility functions to get the string from tag value etc.
// get_DW_TAG_name(unsigned n)
#define DW_FIRST_TAG(name, value)           \
  const char *get_DW_TAG_name(unsigned n) { \
    switch (n) {                            \
      DW_TAG(name, value)
#define DW_TAG(name, value) \
  case name:                \
    return #name;
#define DW_END_TAG \
  }                \
  return 0;        \
  }
#define DW_TAG_DUP(name, value)

// get_DW_FORM_name(unsigned n)
#define DW_FIRST_FORM(name, value)           \
  const char *get_DW_FORM_name(unsigned n) { \
    switch (n) {                             \
      DW_FORM(name, value)
#define DW_FORM(name, value) \
  case name:                 \
    return #name;
#define DW_END_FORM \
  }                 \
  return 0;         \
  }

// get_DW_AT_name(unsigned n)
#define DW_FIRST_AT(name, value)           \
  const char *get_DW_AT_name(unsigned n) { \
    switch (n) {                           \
      DW_AT(name, value)
#define DW_AT(name, value) \
  case name:               \
    return #name;
#define DW_END_AT \
  }               \
  return 0;       \
  }
#define DW_AT_DUP(name, value)

// get_DW_OP_name(unsigned n)
#define DW_FIRST_OP(name, value)           \
  const char *get_DW_OP_name(unsigned n) { \
    switch (n) {                           \
      DW_OP(name, value)
#define DW_OP(name, value) \
  case name:               \
    return #name;
#define DW_END_OP \
  }               \
  return 0;       \
  }
#define DW_OP_DUP(name, value)

// get_DW_ATE_name(unsigned n)
#define DW_FIRST_ATE(name, value)           \
  const char *get_DW_ATE_name(unsigned n) { \
    switch (n) {                            \
      DW_ATE(name, value)
#define DW_ATE(name, value) \
  case name:                \
    return #name;
#define DW_END_ATE \
  }                \
  return 0;        \
  }
#define DW_ATE_DUP(name, value)

// get_DW_CFA_name(unsigned n)
#define DW_FIRST_CFA(name, value)           \
  const char *get_DW_CFA_name(unsigned n) { \
    switch (n) {                            \
      DW_CFA(name, value)
#define DW_CFA(name, value) \
  case name:                \
    return #name;
#define DW_END_CFA \
  }                \
  return 0;        \
  }

#define DW_CFA_DUP(name, value)
#define DW_FIRST_IDX(name, value)
#define DW_IDX(name, value)
#define DW_IDX_DUP(name, value)
#define DW_END_IDX

#include "dwarf2.def"

dw_ate GetAteFromPTY(PrimType pty) {
  switch (pty) {
    case PTY_u1:
      return DW_ATE_boolean;
    case PTY_u8:
      return DW_ATE_unsigned_char;
    case PTY_u16:
    case PTY_u32:
    case PTY_u64:
      return DW_ATE_unsigned;
    case PTY_i8:
      return DW_ATE_signed_char;
    case PTY_i16:
    case PTY_i32:
    case PTY_i64:
      return DW_ATE_signed;
    case PTY_f32:
    case PTY_f64:
    case PTY_f128:
      return DW_ATE_float;
    case PTY_agg:
    case PTY_ref:
    case PTY_ptr:
    case PTY_a32:
    case PTY_a64:
      return DW_ATE_address;
    case PTY_c64:
    case PTY_c128:
      return DW_ATE_complex_float;
    case PTY_void:
      return DW_ATE_void;
    default:
      return DW_ATE_void;
  }
}

}  // namespace maple
