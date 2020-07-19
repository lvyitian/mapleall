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

#include "opcode_info.h"
#include "mir_nodes.h"

namespace maple {
OpcodeTable::OpcodeTable() {
#define STR(s) #s
#define OPCODE(O, P, F, S)     \
  table[OP_##O].flag = F;      \
  table[OP_##O].name = STR(O); \
  table[OP_##O].instrucSize = S;
#include "opcodes.def"
#undef OPCODE
  table[OP_maydassign].name = "dassign";  // maydassign is printed dassign
}

const OpcodeTable kOpcodeInfo;
}  // namespace maple
