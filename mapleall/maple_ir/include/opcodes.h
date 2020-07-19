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

#ifndef MAPLE_IR_INCLUDE_OPCODES_H
#define MAPLE_IR_INCLUDE_OPCODES_H

namespace maple {
enum Opcode : std::uint8_t {
  kOpUndef,
#define OPCODE(STR, YY, ZZ, SS) OP_##STR,
#include "opcodes.def"
#undef OPCODE
  kOpLast,
};

}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_OPCODES_H
