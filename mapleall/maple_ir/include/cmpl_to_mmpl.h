/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan PSL v1.
 * You can use this software according to the terms and conditions of the Mulan PSL v1.
 * You may obtain a copy of Mulan PSL v1 at:
 *
 *     http://license.coscl.org.cn/MulanPSL
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v1 for more details.
 */

// license/copyrights comes here
//
// printing the compact MIR in a user friendly format
// This is the text-printer for kCmpl-v2
//
// TODO:
// 1. change to CPP style
//
#ifndef MAPLE_IR_INCLUDE_CMPL_TO_MMPL_H
#define MAPLE_IR_INCLUDE_CMPL_TO_MMPL_H

#ifdef MIR_FEATURE_FULL
#undef MIR_FEATURE_FULL
#endif

#define MIR_FEATURE_FULL 1

#include "types_def.h"
#include "cmpl.h"
#include "cmpl_supp.h"
#include "mir_nodes.h"
#include "opcode_info.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define BINMIR_FILE_TYPE MJSVM_FILE_TYPE_CMPL  // moved from cmpl.h
#if !defined(BINMIR_FILE_TYPE) || (BINMIR_FILE_TYPE != MJSVM_FILE_TYPE_CMPL)
#error "include the right kCmpl header file: this implements kCmpl-v2"
#endif

using namespace maple;

// for label to stmt reconstruction, and function information
#include <map>
#include <vector>
#include <stack>
#include <list>
using namespace std;

class CmplPrinter {
 public:
  static const char kSpace[33];
  int current_indent;
  int func_label_index;
  /* Map an offset (inside function) to a lable index of this function.
     Used to reconstruct label information in a funtion.
     The offset should be the start offset of a statement.
   */
  map<uint32, int> *offset_label_map;
  // current function being processed. Used for offset related calculation.
  mir_func_t *cur_func;
  /* The start pointer of a stmt. Since now kCmpl is stored in postorder,
     the node which contains the stmt operator information is different from its
     starting expression address.
   */
  map<stmt_node_t *, base_node_t *> *stmt_start_map;
  /* Total (virtual) instructions in a stmt. It determine whether the
     stmt should be printed in multiple lines.
   */
  map<stmt_node_t *, uint32> *stmt_inst_num_map;
  /* Part of the reconstructed AST: list of stmts in a function.
     Note: they're ordered according to lexical their appearances.
   */
  list<stmt_node_t *> *cur_func_stmts;
  /* Part of the reconstructed AST: children of all nodes.
     Note: leaf nodes has a nullptr child
     Note: for a node, the children are ordered.
   */
  map<base_node_t *, vector<base_node_t *> *> *node_children_map;

 public:
  CmplPrinter()
    : current_indent(0),
      func_label_index(0),
      offset_label_map(nullptr),
      cur_func(nullptr),
      stmt_start_map(nullptr),
      stmt_inst_num_map(nullptr),
      cur_func_stmts(nullptr),
      node_children_map(nullptr) {}

  virtual ~CmplPrinter() {}

  const char *GetIntrinsicName(MIRIntrinsicID intrn) const;
  void PrintIdentByLevel(int indentLevel) const;
  inline void PrintIdent() const;
  const char *GetTypeName(PrimType ty) const;
  const char *GetPregName(PregIdx preg);
  const char *TypeName(const base_node_t *node);
  const char *OpName(const base_node_t *node);
  bool IsCvtOp(Opcode opcode) const;
  bool IsCmpOp(Opcode opcode) const;
  int GetOpnodeSize(Opcode opcode) const;
  int GetNodeSize(base_node_t *node) const;
  vector<base_node_t *> *GetOperands(base_node_t *node);
  base_node_t *GetOperand(base_node_t *node, uint32 nth);
  inline bool OpcodeIsLeaf(Opcode op) const;
  bool CanPrintOneLine(base_node_t *node);
  int GetLabelForOffset(uint32 offset);
  int GetLabelForStmt(stmt_node_t *stmt);
  void PrintExprDelimiter(bool multiLine) const;
  void PrintLn(bool multiLine) const;
  inline void PrintExprBegin(const base_node_t *expr, bool indentFlag);
  void PrintExpr(base_node_t *expr, bool multiline);
  void PrintStmtOperands(stmt_node_t *stmt);
  inline void PrintStmtBegin(const stmt_node_t *stmt);
  void PrintStmt(stmt_node_t *stmt);
  void AddNewLabel(uint32 offset);
  void CollectFuncInfo(mir_func_t *func);
  void PrintFunc(mir_func_t *func, int puid);
  void PrintModule(mir_module_t *module);
};
#endif  // MAPLE_IR_INCLUDE_CMPL_TO_MMPL_H
