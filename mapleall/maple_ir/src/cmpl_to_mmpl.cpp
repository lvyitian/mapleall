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

#include <cinttypes>

#include "cmpl_to_mmpl.h"
#include "mir_preg.h"
#include "securec.h"

// mirnodes.cpp has some external dependencies. replicated the code here.
const char *CmplPrinter::GetIntrinsicName(MIRIntrinsicID intrn) const {
  switch (intrn) {
    default:
#define DEF_MIR_INTRINSIC(STR, NAME, INTRN_CLASS, RETURN_TYPE, ...) \
  case INTRN_##STR:                                                 \
    return #STR;
#include "intrinsics.def"
#undef DEF_MIR_INTRINSIC
  }
}

const char CmplPrinter::kSpace[33] = "                                ";
// Note: here all idents are indent-level, rather than the actual indent
void CmplPrinter::PrintIdentByLevel(int indentLevel) const {
  // one indent level corresponds to 2 spaces
  while (indentLevel > 16) {
    printf("%s", CmplPrinter::kSpace);
    indentLevel -= 16;
  }

  if (indentLevel > 0) {
    printf("%s", CmplPrinter::kSpace + (long)(32 - indentLevel * 2));
  }
}

inline void CmplPrinter::PrintIdent() const {
  PrintIdentByLevel(current_indent);
}

#define indented_printf(...) \
  do {                       \
    PrintIdent();            \
    printf(__VA_ARGS__);     \
  } while (0)

#define CMPL_PRINT(indentflag, ...) \
  do {                              \
    if (indentflag)                 \
      indented_printf(__VA_ARGS__); \
    else                            \
      printf(__VA_ARGS__);          \
  } while (0)

// replicated from mirtypes.cpp
const char *CmplPrinter::GetTypeName(PrimType ty) const {
  switch (ty) {
    default:
    case kPtyInvalid:
      return "kPtyInvalid";
#define PRIMTYPE(P) \
  case PTY_##P:     \
    return #P;
#define LOAD_ALGO_PRIMARY_TYPE
#include "prim_types.def"
#undef PRIMTYPE
    case kPtyDerived:
      return "derived";  // just for test: no primitive type for derived
  }
  // SIMD types to be defined
}

// replicated from mirnodes.cpp
const char *CmplPrinter::GetPregName(PregIdx preg) {
  static char pregName[32];  // internal buffer
  if (preg < 0) {
    switch (-preg) {
      case kSregSp:
        return "%%SP";
      case kSregFp:
        return "%%FP";
      case kSregGp:
        return "%%GP";
      case kSregThrownval:
        return "%%thrownval";
      case kSregMethodhdl:
        return "%%methodhdl";
      default:
        int eNum = snprintf_s(pregName, sizeof(pregName) - 1, 31, "%%%%retval%d", static_cast<int32>((-preg) - kSregRetval0));
        if (eNum < 0) {
          FATAL(kLncFatal, "snprintf_s failed  ");
        }
        pregName[31] = '\0';
        return pregName;
    }
  } else {
    // for index-to-name translation
    int eNum = snprintf_s(pregName, sizeof(pregName) - 1, 31, "%%%d", static_cast<int32>(preg));
    if (eNum < 0) {
      FATAL(kLncFatal, "snprintf_s failed  ");
    }
    pregName[31] = '\0';
    return pregName;
  }
}

const char *CmplPrinter::TypeName(const base_node_t *node) {
  ASSERT(node, "");
  return GetTypeName(GetPrimtype(node));
}

const char *CmplPrinter::OpName(const base_node_t *node) {
  ASSERT(node, "");
  return maple::kOpcodeInfo.GetName(GetOpcode(node));
}

bool CmplPrinter::IsCvtOp(Opcode opcode) const {
  switch (opcode) {
    case OP_ceil:
    case OP_cvt:
    case OP_floor:
    case OP_retype:
    case OP_round:
    case OP_trunc:
      return true;
    default:
      return false;
  }
}

bool CmplPrinter::IsCmpOp(Opcode opcode) const {
  switch (opcode) {
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg:
    case OP_eq:
    case OP_ne:
    case OP_ge:
    case OP_gt:
    case OP_le:
    case OP_lt:
      return true;
    default:
      return false;
  }
}

/* opnode size in bytes.
   Note: opnode is the node which contains only operator inforamtion,
         not include operands and other associated data structures
 */
int CmplPrinter::GetOpnodeSize(Opcode opcode) const {
  int nodeSize = 0;
  switch (opcode) {
    // expressions
    case OP_addroffunc:
    case OP_addroflabel:
    case OP_extractbits:
    case OP_regread:
    case OP_ireadoff:
    case OP_ireadfpoff:
    case OP_intrinsicop:
    // type conversion
    case OP_ceil:
    case OP_cvt:
    case OP_floor:
    case OP_retype:
    case OP_round:
    case OP_trunc:
      nodeSize = 8;
      break;
    // unary
    case OP_abs:
    case OP_bnot:
    case OP_lnot:
    case OP_neg:
    case OP_recip:
    case OP_sqrt:
    case OP_alloca:
    case OP_malloc:
    case OP_gcmalloc:
    case OP_gcpermalloc:
    case OP_stackmalloc:
    // binary
    case OP_add:
    case OP_ashr:
    case OP_band:
    case OP_bior:
    case OP_bxor:
    case OP_depositbits:
    case OP_div:
    case OP_land:
    case OP_lior:
    case OP_lshr:
    case OP_max:
    case OP_min:
    case OP_mul:
    case OP_rem:
    case OP_shl:
    case OP_CG_array_elem_add:
    case OP_sub:
      nodeSize = 4;
      break;
    case OP_eq:  // now compare operantions are 8B
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg:
    case OP_ge:
    case OP_gt:
    case OP_le:
    case OP_lt:
    case OP_ne:
      nodeSize = 8;
      break;
    // tenary
    case OP_select:
      nodeSize = 4;
      break;
    // const value is special. 4B base_node + 8B value
    case OP_constval:
      nodeSize = 12;
      break;

    // statements
    case OP_jscatch:
    case OP_throw:
    case OP_retsub:
    case OP_finally:
    case OP_cleanuptry:
    case OP_endtry:
    case OP_free:
    case OP_syncenter:
    case OP_syncexit:
    case OP_return:
      nodeSize = 4;
      break;
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_call:
    case OP_icall:
    case OP_iassignoff:
    case OP_iassignfpoff:
    case OP_regassign:
    case OP_brtrue:
    case OP_brfalse:
    case OP_goto:
    case OP_gosub:
    case OP_jstry:
    case OP_rangegoto:
      nodeSize = 8;
      break;

    case OP_switch:
    case OP_comment:
    case OP_label:
      MIR_FATAL("operator %d shouldn't be in kCmpl(v2)\n", opcode);
      break;
    default:
      MIR_FATAL("unknown operations\n");
      break;
  }

  return nodeSize;
}

/* node size with all its associated data-structure, but not include
   its operands.
   currently, only rangegoto has associated data-structure
 */
int CmplPrinter::GetNodeSize(base_node_t *node) const {
  Opcode opcode = GetOpcode(node);
  int nodeSize = GetOpnodeSize(opcode);

  if (opcode == OP_rangegoto) {
    small_case_vec_t *caseVec = (small_case_vec_t *)(reinterpret_cast<uint8 *>(node) + nodeSize);
    nodeSize += sizeof(small_case_vec_t) + sizeof(small_case_pair_t) * caseVec->num_cases;
  }

  return nodeSize;
}

/* Returns all operands of all base_node_t derived data structures.
   The operands inforamtion is recovered during kCmpl inforamtion collection
   phase.
 */
vector<base_node_t *> *CmplPrinter::GetOperands(base_node_t *node) {
  ASSERT(node_children_map, "");
  map<base_node_t *, vector<base_node_t *> *>::const_iterator iter;
  iter = node_children_map->find(node);
  if (iter == node_children_map->end()) {
    MIR_FATAL("cannot find node in AST\n");
  }

  vector<base_node_t *> *operands = iter->second;
  return operands;
}

// Returns a specific operand of a base_node_t derived data structure
base_node_t *CmplPrinter::GetOperand(base_node_t *node, uint32 nth) {
  ASSERT(nth < GetOperandsNum(node), "");
  vector<base_node_t *> *operands = GetOperands(node);
  ASSERT(operands, "");
  ASSERT(operands->size() == GetOperandsNum(node), "");
  return operands->at(nth);
}

// Returns a specific operand of a stmt_node_t derived data structure
// helper function to avoid type conversion
// these nodes are leaf node, i.e., don't have child expression
inline bool CmplPrinter::OpcodeIsLeaf(Opcode op) const {
  switch (op) {
    case OP_addroffunc:
    case OP_addroflabel:
    case OP_constval:
    case OP_regread:
      return true;
    default:
      return false;
  }
}

/* Whether a basenode can be printed in one line
   expression with all leaf operands can be printed in one line
   The exception is ireadoff_node_t(can be printed in one line)
*/
bool CmplPrinter::CanPrintOneLine(base_node_t *node) {
  ASSERT(node, "");
  if (GetOperandsNum(node) <= 1) {
    return true;
  }
  for (uint32 i = 0; i < GetOperandsNum(node); i++) {
    Opcode op = GetOpcode(GetOperand(node, i));
    if (!(OpcodeIsLeaf(op))) {
      return false;
    }
  }
  return true;
}

/* Given an offset in current function, determine whether it's the target
   of a label.
   If yes, return its label index, if no, return -1
 */
int CmplPrinter::GetLabelForOffset(uint32 offset) {
  ASSERT(offset_label_map, "");
  map<uint32, int>::const_iterator iter = offset_label_map->find(offset);
  if (iter == offset_label_map->end()) {
    return -1;
  }

  return iter->second;
}

/* Given a stmt, determine whether it's the target of a label
   if yes, return its label index, if no, return -1
 */
int CmplPrinter::GetLabelForStmt(stmt_node_t *stmt) {
  ASSERT(stmt_start_map, "");
  // get the real-start of a stmt
  map<stmt_node_t *, base_node_t *>::const_iterator iter;
  iter = stmt_start_map->find(stmt);
  if (iter == stmt_start_map->end()) {
    MIR_FATAL("cannot find stmt start address information.\n");
  }

  base_node_t *start = iter->second;

  uint32 offset = static_cast<uint32>((uintptr_t)start - (uintptr_t)cur_func);
  return GetLabelForOffset(offset);
}

void CmplPrinter::PrintExprDelimiter(bool multiLine) const {
  if (multiLine) {
    printf(",\n");
  } else {
    printf(", ");
  }
}

void CmplPrinter::PrintLn(bool multiLine) const {
  if (multiLine) {
    printf("\n");
  }
}

// Printing the expression common information
inline void CmplPrinter::PrintExprBegin(const base_node_t *expr, bool indentFlag) {
  CMPL_PRINT(indentFlag, "%s %s ", OpName(expr), TypeName(expr));
}

// print expression
void CmplPrinter::PrintExpr(base_node_t *expr, bool multiline) {
  ASSERT(expr, "");
  PrintExprBegin(expr, multiline);
  multiline = !CanPrintOneLine(expr);
  Opcode opcode = GetOpcode(expr);
  if (IsCvtOp(opcode)) {
    printf("%s ", GetTypeName((static_cast<typecvt_node_t *>(expr))->fromPrimType));
  } else if (IsCmpOp(opcode)) {
    printf("%s ", GetTypeName((static_cast<compare_node_t *>(expr))->opndType));
  } else if (opcode == OP_intrinsicop) {
    printf("%s ", GetIntrinsicName((static_cast<intrinsicop_node_t *>(expr))->intrinsic));
  } else if (opcode == OP_ireadoff) {
    printf("%d ", (static_cast<ireadoff_node_t *>(expr))->offset);
  } else if (opcode == OP_ireadfpoff) {
    printf("%d ", (static_cast<ireadoff_node_t *>(expr))->offset);
    return;
  }

  bool hasOpnd = !OpcodeIsLeaf(opcode);
  if (hasOpnd) {
    printf("(");
  }

  switch (opcode) {
    // leaf opcodes
    case OP_addroffunc:
      printf("&func%d", (static_cast<addroffunc_node_t *>(expr))->puIdx);
      break;
    case OP_addroflabel:
      printf("&label%d", GetLabelForOffset((static_cast<addroflabel_node_t *>(expr))->offset));
      break;
    case OP_constval:
      printf("0x%" PRIx64, static_cast<uint64>(GetConstval(static_cast<constval_node_t *>(expr))->value));
      break;
    case OP_regread:
      printf("%s", GetPregName((static_cast<regread_node_t *>(expr))->regIdx));
      break;
    // Unary opcodes
    case OP_extractbits:
      MIR_FATAL("not implemented yet");
      break;
    case OP_ireadoff:
    case OP_abs:
    case OP_bnot:
    case OP_lnot:
    case OP_neg:
    case OP_recip:
    case OP_sqrt:
    case OP_alloca:
    case OP_malloc:
    case OP_gcmalloc:
    case OP_gcpermalloc:
    case OP_stackmalloc:
    // Type conversion
    case OP_ceil:
    case OP_cvt:
    case OP_floor:
    case OP_retype:
    case OP_round:
    case OP_trunc:
      PrintExpr(GetOperand(expr, 0), false);
      break;
    case OP_intrinsicop: {
      uint32 operandsNum = GetOperandsNum(expr);
      if (operandsNum == 0) {
        break;
      }
      PrintLn(multiline);

      if (multiline) {
        current_indent++;
      }
      for (uint32 i = 0; i < operandsNum - 1; i++) {
        PrintExpr(GetOperand(expr, i), multiline);
        PrintExprDelimiter(multiline);
      }
      PrintExpr(GetOperand(expr, operandsNum - 1), multiline);
      if (multiline) {
        current_indent--;
      }
      break;
    }
    // Binary
    case OP_add:
    case OP_ashr:
    case OP_band:
    case OP_bior:
    case OP_bxor:
    case OP_depositbits:
    case OP_div:
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg:
    case OP_eq:
    case OP_ge:
    case OP_gt:
    case OP_le:
    case OP_lt:
    case OP_ne:
    case OP_land:
    case OP_lior:
    case OP_lshr:
    case OP_max:
    case OP_min:
    case OP_mul:
    case OP_rem:
    case OP_shl:
    case OP_sub:
    case OP_CG_array_elem_add:
      PrintLn(multiline);

      if (multiline) {
        current_indent++;
      }
      PrintExpr(GetOperand(expr, 0), multiline);
      PrintExprDelimiter(multiline);
      PrintExpr(GetOperand(expr, 1), multiline);
      if (multiline) {
        current_indent--;
      }
      break;
    case OP_select:
      MIR_FATAL("not implemented yet");
      break;
    default:
      MIR_FATAL("unknown expression opcode");
  }
  if (hasOpnd) {
    printf(")");
  }
}

void CmplPrinter::PrintStmtOperands(stmt_node_t *stmt) {
  bool multiline = !CanPrintOneLine(stmt);

  printf("(");
  PrintLn(multiline);
  if (multiline) {
    current_indent++;
  }

  for (uint32 i = 0; i < GetOperandsNum(static_cast<base_node_t *>(stmt)); i++) {
    if (i > 0) {
      PrintExprDelimiter(multiline);
    }
    PrintExpr(GetOperand(stmt, i), multiline);
  }

  printf(")\n");
  if (multiline) {
    current_indent--;
  }
}

// print statement
inline void CmplPrinter::PrintStmtBegin(const stmt_node_t *stmt) {
  indented_printf("%s ", OpName(stmt));
}

void CmplPrinter::PrintStmt(stmt_node_t *stmt) {
  ASSERT(stmt, "");
  Opcode opcode = GetOpcode(static_cast<base_node_t *>(stmt));

  int labelIndex = GetLabelForStmt(stmt);
  if (labelIndex >= 0) {
    printf("@label%d", labelIndex);
  }

  PrintStmtBegin(stmt);
  switch (opcode) {
    case OP_intrinsiccall:
      printf("%s ", GetIntrinsicName((static_cast<intrinsiccall_stmt_t *>(stmt))->intrinsic));
      break;
    case OP_xintrinsiccall:
      printf("%d ", (static_cast<intrinsiccall_stmt_t *>(stmt))->intrinsic);
      break;
    case OP_call:
      printf("&func%d ", (static_cast<call_stmt_t *>(stmt))->puIdx);
      break;
    /*
       case OP_icall:
       stmt = stmt_alloc_n_copy<icall_stmt_t>(image, fstmt, func);
       // no other field to handle. puptrexp is not used any more in VM
       break;
     */
    case OP_iassignoff:
    case OP_iassignfpoff:
      printf("%s ", TypeName(stmt));
      printf("%d ", (static_cast<iassignoff_stmt_t *>(stmt))->offset);
      break;

    case OP_jscatch:
    case OP_finally:
    case OP_endtry:
    case OP_cleanuptry:
    case OP_retsub:
      printf("\n");
      return;

    case OP_throw:
    case OP_syncenter:
    case OP_syncexit:
    case OP_return:
      break;

    case OP_goto:
    case OP_gosub:
      printf("@label%d\n", GetLabelForOffset((static_cast<goto_stmt_t *>(stmt))->offset));
      return;

    case OP_jstry:
      printf("@label%d @label%d\n",
             GetLabelForOffset((static_cast<try_stmt_t *>(stmt))->catchOffset +
                               static_cast<uint32>((uintptr_t)stmt - (uintptr_t)cur_func)),
             GetLabelForOffset((static_cast<try_stmt_t *>(stmt))->finallyOffset +
                               static_cast<uint32>((uintptr_t)stmt - (uintptr_t)cur_func)));
      return;

    case OP_brtrue:
    case OP_brfalse:
      printf("@label%d ", GetLabelForOffset((static_cast<goto_stmt_t *>(stmt))->offset));
      break;
    case OP_rangegoto: {
      rangegoto_stmt_t *rgtStmt = static_cast<rangegoto_stmt_t *>(stmt);
      printf("(");
      uint8 *currPtr = reinterpret_cast<uint8 *>(rgtStmt) + sizeof(rangegoto_stmt_t);
      base_node_t *rangegotoOpnd = GetOperand(static_cast<base_node_t *>(rgtStmt), 0);
      PrintExpr(rangegotoOpnd, false);

      printf(") %d {", rgtStmt->tagOffset);

      current_indent++;
      small_case_vec_t *caseVec = (small_case_vec_t *)currPtr;
      currPtr += sizeof(small_case_vec_t);
      for (uint32 i = 0; i < caseVec->num_cases; i++) {
        printf("\n");
        small_case_pair_t *casepair = (small_case_pair_t *)currPtr;
        indented_printf("%d: goto @label%d", casepair->first, GetLabelForOffset(casepair->second));
        currPtr += sizeof(small_case_pair_t);
      }
      printf(" }\n");
      current_indent--;
      return;
    }
    case OP_regassign:
      printf("%s ", TypeName(stmt));
      printf("%s ", GetPregName((static_cast<regassign_stmt_t *>(stmt))->regIdx));
      break;
    default:
      MIR_FATAL("unknown statement opcode\n");
  }
  // preorder printing
  PrintStmtOperands(stmt);
}

// (next) Label index assigned to a new label.
void CmplPrinter::AddNewLabel(uint32 offset) {
  ASSERT(offset_label_map, "");
  if (offset_label_map->find(offset) == offset_label_map->end()) {
    offset_label_map->insert(pair<uint32, int>(offset, func_label_index++));
  }
}

/* Sequentially walk through the function body instruction sequence and collect
   necessary information for printing. Currently, it does:
   1. how many possible labels there.
   2. reconstruct the AST tree.
   3. how many total sub-expressions a stmt have (to determine single/multile
      line for the stmt)
 */
void CmplPrinter::CollectFuncInfo(mir_func_t *func) {
  // set up function related global context.
  func_label_index = 0;

  base_node_t *inst = static_cast<base_node_t *>(func->FirstInst());
  CHECK_FATAL(inst != nullptr, "null ptr check");
  base_node_t *stmtFirstInst = nullptr;  // first instruction of a stmt;
  bool isStmt = false;                   // current inst is a stmt opnode
  uint32 stmtInstNum = 0;                // number of instructions in a stmt
  // pending nodes are those encounted but not yet added into AST.
  stack<base_node_t *> *pendingNodes = new stack<base_node_t *>();

  while (inst) {
    if (stmtFirstInst == nullptr) {
      stmtFirstInst = inst;
    }
    stmtInstNum++;

    Opcode opcode = GetOpcode(static_cast<base_node_t *>(inst));
    switch (opcode) {
      // only those label involved opcodes and stmt opcodes
      // stmts
      case OP_brtrue:
      case OP_brfalse:
        isStmt = true;
        AddNewLabel((static_cast<goto_stmt_t *>(inst))->offset);
        break;
      case OP_rangegoto: {
        int offset = sizeof(rangegoto_stmt_t);
        small_case_vec_t *caseVec = (small_case_vec_t *)(reinterpret_cast<uint8 *>(inst) + offset);
        offset += sizeof(small_case_vec_t);
        for (uint32 i = 0; i < caseVec->num_cases; i++) {
          small_case_pair_t *casePair = (small_case_pair_t *)(reinterpret_cast<uint8 *>(inst) + offset);
          AddNewLabel(casePair->second);
          offset += sizeof(small_case_pair_t);
        }
        isStmt = true;
        break;
      }
      case OP_goto:
      case OP_gosub:
        isStmt = true;
        AddNewLabel((static_cast<goto_stmt_t *>(inst))->offset);
        break;
      case OP_jscatch:
        isStmt = true;
        break;
      case OP_jstry:
        isStmt = true;
        AddNewLabel((static_cast<try_stmt_t *>(inst))->catchOffset +
                    static_cast<uint32>((uintptr_t)inst - (uintptr_t)func));
        AddNewLabel((static_cast<try_stmt_t *>(inst))->finallyOffset +
                    static_cast<uint32>((uintptr_t)inst - (uintptr_t)func));
        break;

      case OP_throw:
      case OP_retsub:
      case OP_finally:
      case OP_return:
      case OP_intrinsiccall:
      case OP_xintrinsiccall:
      case OP_call:
      case OP_icall:
      case OP_iassignoff:
      case OP_iassignfpoff:
      case OP_regassign:
      case OP_endtry:
      case OP_cleanuptry:
      case OP_syncenter:
      case OP_syncexit:
        isStmt = true;
        break;

      // expressions
      case OP_addroflabel:
        AddNewLabel((static_cast<addroflabel_node_t *>(inst))->offset);
        break;

      // Note: check for unsupport opcode is delayed to call to GetNodeSize
      default:
        isStmt = false;
        break;
    }

    // reconstruct parent-children relationship
    uint32 numOperands = GetOperandsNum(static_cast<base_node_t *>(inst));
    if (numOperands == 0) {  // it's a leaf node, don't have child
      // add it to the list to make sure every node is there
      node_children_map->insert(pair<base_node_t *, vector<base_node_t *> *>(inst, nullptr));
    } else {
      vector<base_node_t *> *nodeChildren = new vector<base_node_t *>();
      CHECK_FATAL(numOperands <= pendingNodes->size(), "");
      for (uint32 i = 0; i < numOperands; i++) {
        vector<base_node_t *>::iterator iter = nodeChildren->begin();
        // using insert to kept the operand order correct
        nodeChildren->insert(iter, pendingNodes->top());
        pendingNodes->pop();
      }

      node_children_map->insert(pair<base_node_t *, vector<base_node_t *> *>(inst, nodeChildren));
    }

    if (!isStmt) {
      // add expression to the stack
      CHECK_FATAL(pendingNodes, "null ptr check");
      pendingNodes->push(inst);
    } else {
      stmt_node_t *stmt = static_cast<stmt_node_t *>(inst);
      // add the stmt to current function's stmt list
      cur_func_stmts->push_back(stmt);
      // set up stmt to its start address map (due to preorder storage)
      stmt_start_map->insert(pair<stmt_node_t *, base_node_t *>(stmt, stmtFirstInst));
      // set up stmt (virtual) instruction number map for printing decision
      stmt_inst_num_map->insert(pair<stmt_node_t *, uint32>(stmt, stmtInstNum));

      // reset for next node
      stmtInstNum = 0;
      stmtFirstInst = nullptr;
    }

    // next instruction in current function body
    inst = reinterpret_cast<base_node_t *>((reinterpret_cast<uint8 *>(inst) + GetNodeSize(inst)));
    // out of function range
    if ((uintptr_t)inst >= ((uintptr_t)func + func->FuncCodeSize())) {
      inst = nullptr;
      delete pendingNodes;
      pendingNodes = nullptr;
    }
  }
}

/* Puid is required to synthesize a function name, because we don't
   have a symbol table here
 */
void CmplPrinter::PrintFunc(mir_func_t *func, int puid) {
  // set up function scope global context and related data structure.
  cur_func = func;  // function context
  offset_label_map = new map<uint32, int>();
  cur_func_stmts = new list<stmt_node_t *>();
  stmt_start_map = new map<stmt_node_t *, base_node_t *>();
  stmt_inst_num_map = new map<stmt_node_t *, uint32>();
  node_children_map = new map<base_node_t *, vector<base_node_t *> *>();

  // pass 1: collect function information:
  CollectFuncInfo(func);

  // pass 2: actually output cmpl
  indented_printf("func &func%d {\n", puid);
  current_indent++;

  if (func->upFormalSize) {
    indented_printf("upformalsize %d\n", func->upFormalSize);
    indented_printf("formalwordstypetagged = [ ");
    uint32 *gmemPtr = reinterpret_cast<uint32 *>(func->formalWordsTypeTagged);
    int gmemToPrint = BlkSize2BitvectorSize(func->upFormalSize);
    while (gmemToPrint > 0) {
      printf("0x%x ", *gmemPtr);
      gmemPtr++;
      gmemToPrint -= 4;
    }
    printf("]\n");
    indented_printf("formalwordsrefcounted = [ ");
    gmemPtr = reinterpret_cast<uint32 *>(func->formalWordsRefCounted);
    gmemToPrint = BlkSize2BitvectorSize(func->upFormalSize);
    while (gmemToPrint > 0) {
      printf("0x%x ", *gmemPtr);
      gmemPtr++;
      gmemToPrint -= 4;
    }
    printf("]\n");
  }
  indented_printf("framesize %d\n", func->frameSize);
  indented_printf("localwordstypetagged = [ ");
  uint32 *gmemPtr = reinterpret_cast<uint32 *>(func->localWordsTypeTagged);
  int gmemToPrint = BlkSize2BitvectorSize(func->frameSize);
  while (gmemToPrint > 0) {
    printf("0x%x ", *gmemPtr);
    gmemPtr++;
    gmemToPrint -= 4;
  }
  printf("]\n");
  indented_printf("localwordsrefcounted = [ ");
  gmemPtr = reinterpret_cast<uint32 *>(func->localWordsRefCounted);
  gmemToPrint = BlkSize2BitvectorSize(func->frameSize);
  while (gmemToPrint > 0) {
    printf("0x%x ", *gmemPtr);
    gmemPtr++;
    gmemToPrint -= 4;
  }
  printf("]\n");
  indented_printf("moduleid %d\n", func->moduleID);
  indented_printf("funcSize %d\n", func->funcSize);

  // print statements.
  list<stmt_node_t *>::iterator stmtIter;
  for (stmtIter = cur_func_stmts->begin(); stmtIter != cur_func_stmts->end(); stmtIter++) {
    PrintStmt(*stmtIter);
  }

  current_indent--;
  indented_printf("}\n");

  // clean up function scope data structure
  delete offset_label_map;
  offset_label_map = nullptr;
  delete cur_func_stmts;
  cur_func_stmts = nullptr;
  delete stmt_start_map;
  stmt_start_map = nullptr;
  delete stmt_inst_num_map;
  stmt_inst_num_map = nullptr;

  map<base_node_t *, vector<base_node_t *> *>::iterator iter;
  for (iter = node_children_map->begin(); iter != node_children_map->end(); iter++) {
    delete iter->second;
  }

  delete node_children_map;
  node_children_map = nullptr;
}

void CmplPrinter::PrintModule(mir_module_t *module) {
  ASSERT(module, "");

  current_indent = 0;

  // module header
  indented_printf("flavor %d\n", module->flavor);
  indented_printf("srclang %d\n", module->srcLang);
  indented_printf("id %d\n", module->id);
  indented_printf("globalmemsize %d\n", module->globalMemSize);
  indented_printf("globalmemmap = [ ");
  uint32 *gmemPtr = reinterpret_cast<uint32 *>(module->globalBlkMap);
  int gmemToPrint = module->globalMemSize;
  ASSERT(gmemToPrint % 4 == 0, "");  // 4 bytes aligned?
  while (gmemToPrint > 0) {
    printf("0x%x ", *gmemPtr);
    gmemPtr++;
    gmemToPrint -= 4;
  }
  printf("]\n");
  indented_printf("globalwordstypetagged = [ ");
  gmemPtr = reinterpret_cast<uint32 *>(module->globalWordsTypeTagged);
  gmemToPrint = BlkSize2BitvectorSize(module->globalMemSize);
  while (gmemToPrint > 0) {
    printf("0x%x ", *gmemPtr);
    gmemPtr++;
    gmemToPrint -= 4;
  }
  printf("]\n");
  indented_printf("globalwordsrefcounted = [ ");
  gmemPtr = reinterpret_cast<uint32 *>(module->globalWordsRefCounted);
  gmemToPrint = BlkSize2BitvectorSize(module->globalMemSize);
  while (gmemToPrint > 0) {
    printf("0x%x ", *gmemPtr);
    gmemPtr++;
    gmemToPrint -= 4;
  }
  printf("]\n");

  indented_printf("numfuncs %d\n", module->numFuncs);
  if (module->mainFuncID > 0) {
    indented_printf("entryfunc &func%d\n", module->mainFuncID);
  }

  for (uint32 i = 0; i < module->numFuncs; i++) {
    mir_func_t *func = module->funcs[i];
    PrintFunc(func, i);
  }
}

int main(int argc, char **argv) {
  if (argc < 2) {
    MIR_FATAL("using: ./mmpl2cmpl <cmpl file>\n");
  }
  CmplPrinter cmplPrinter;
  mir_module_t *module = LoadBinMir(argv[1]);
  CHECK_FATAL(module, "null ptr check");

  cmplPrinter.PrintModule(module);

  CloseBinMir(module);
  return 0;
}
