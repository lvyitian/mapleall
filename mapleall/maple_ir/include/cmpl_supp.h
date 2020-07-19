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

// license/copyrights come here
//
// A compact version of MMPL, for VM running on resource
// constrained device, using only POD (plain old data) in C++03,
// for deterministic memory-layout.
// in this IR, only information needed for APP execution is kept in.
//
// Note: this is CMPL (Compact Machine maPLe) version 2
//       different with version 1, it has no pointers in the MIR
// Note: it's the release version of CMPL
// Note: some statement opcodes are laid out differently from their
//       corresponding nodes defined in mir_nodes.h
//
//       It's a binary form of MMPL with relative-pointers

#ifndef MAPLE_IR_INCLUDE_CMPL_SUPP_H
#define MAPLE_IR_INCLUDE_CMPL_SUPP_H

// Still need constant value from MIR
#include "cmpl.h"


// facility for statement operation:
// Warning: get_next_stmt is no longer provided
// DassignNode shouldn't be seen here.

// TODO: should there be regassign in CMPL? seems not used.

using namespace maple;

typedef base_node_t stmt_node_t;

struct regassign_stmt_t : public stmt_node_t {  // 8B
  PregIdx regIdx;                             // 32bit, negative if special register
                                                // base_node_t *rhs;  // removed. it's now operand 0
};

// Warning: get_rhs now removed
//
// IassignNode shouldn't be seen here?

struct iassignoff_stmt_t : public stmt_node_t {  // 8B
  int32 offset;
  // base_node_t *addrExpr; // removed. it's now operand 0
  // base_node_t *rhs;     // removed. it's now operand 1
};

// Warning: get_addrexp now removed

struct goto_stmt_t : public stmt_node_t {  // 8B
  // LabelIdx labelIdx; replace by an offset to target
  int32 offset;  // offset is relative to current function
};

struct condgoto_stmt_t : public goto_stmt_t {  // 8B
                                               // base_node_t *cond; // removed. now operand 0
};

/* try stmt is newly added in CMPL-v2. The original
   use of goto_stmt_t for try is nolonger feasible because
   we can no longer get the catch/finally by follow stmt->next
   pointers in CMPL-v2.

   Note: to make it compact, the offset to both catch stmt and
   finally is limited to 64KB.
   unlike other offsets in the CMPL, this offset is relative to
   the begin try_stmt_t node.
   Note: here assume catch/finally are lexically after try statement.
 */
struct try_stmt_t : public stmt_node_t {  // 8B
  uint16 catchOffset;                    // the (try_stmt_t relative) offset to catch stmt
  uint16 finallyOffset;                  // the (try_stmt_t relative) offset to finally stmt
};

// Warning: get_cond now removed

struct small_case_pair_t {  // 4B
  uint16 first;
  // LabelIdx second; replace by an offset to target
  int16 second;  // it's the offset to target
};

struct small_case_vec_t {  // 4B
  uint8 num_cases;         // max of 256 cases. Is this a limitation?
  // small_case_pair_t *cases; // removed. cases follow small_case_vec_t implicitly
  uint8 _padding_[3];
};

struct rangegoto_stmt_t : public stmt_node_t {  // 8B
  // base_node_t *rangegotoopnd; // removed. now operand 0
  int32 tagOffset;
  // Note: the rangegotoTable now implicitly immediately follow rangegotoopnd
  // small_case_vec_t rangegotoTable; // removed.
};

// Warning: get_rangegotoopnd/get_rangegoto_case/get_rangegoto_case_num
//          now removed
// Warning: block_stmt is no longer in CMPL-v2

typedef stmt_node_t unary_stmt_t;  // they're equivalent
typedef stmt_node_t nary_stmt_t;   // they're equivalent

struct call_stmt_t : public nary_stmt_t {  // 8B
  PUIdx puIdx;
};

struct icall_stmt_t : public nary_stmt_t {  // 8B
                                            // base_node_t *puptrexp; // it's operand[0], no longer explicitly accessed
};

// make sure MIRIntrinsicID is 4B
// Note: this is not supported by c++0x
// for both intrinsiccall and xintrinsiccall
struct intrinsiccall_stmt_t : public nary_stmt_t {  // 8B
  MIRIntrinsicID intrinsic;
};

/* binmir file operation interfaces */
namespace maple {
struct mir_module_t;
}

// load compact mir from a binary file.
mir_module_t *LoadBinMir(const char *fileName);

mir_module_t *LoadBinMirArm(const char *fileName);
// free all the resources used by the module.
void CloseBinMir(mir_module_t *module);

// set up the MIR data structure from an image in memory
mir_module_t *SetupBinMir(void *binmirImage, uint32 imageSize);

// the binmir file type.
#endif  // MAPLE_IR_INCLUDE_CMPL_SUPP_H
