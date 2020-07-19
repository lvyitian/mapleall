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

#include "cmpl_generator.h"
#include "bin_mir_file.h"
#include <climits>
#include "securec.h"

// return puid of main function. 0 if main function not found
uint32 CmplGenerator::FindMainFuncPuid(const MIRModule &fmodule) {
  uint32 mainFuncPuid = 0;
  for (uint32 i = 1; i < GlobalTables::GetFunctionTable().funcTable.size(); i++) {
    MIRFunction *currFunc = GlobalTables::GetFunctionTable().GetFunctionFromPuidx((PUIdx)i);
    MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(currFunc->stIdx.Idx());
    if (funcSt->GetName() == fmodule.entryFuncName) {
      mainFuncPuid = i;
      break;
    }
  }

  return mainFuncPuid;
}

/* Type flag is provided to speedup the operation on PrimType.
   It's here because we have spare space for it in the data structure so far due
   to the 4B/8B size constrain for each opnode.
 */
uint8 CmplGenerator::GenTypeFlag(PrimType type) const {
  uint8 typeflag = 0;
  typeflag |= IsPrimitiveDynType(type) ? TYPEFLAG_DYN_MASK : 0;
  typeflag |= IsPrimitiveScalar(type) ? TYPEFLAG_SCALAR_MASK : 0;
  typeflag |= IsPrimitiveFloat(type) ? TYPEFLAG_FLOAT_MASK : 0;
  typeflag |= IsPrimitiveInteger(type) ? TYPEFLAG_INTEGER_MASK : 0;
  typeflag |= IsPrimitiveDynFloat(type) ? TYPEFLAG_DYN_FLOAT_MASK : 0;
  typeflag |= IsPrimitiveDynInteger(type) ? TYPEFLAG_DYN_INTEGER_MASK : 0;

  uint32 typesize = GetPrimTypeSize(type);
  if (typesize != 0) {
    uint8 logsize = 0;
    while (!(typesize & 0x1)) {
      typesize >>= 1;
      logsize++;
    }
    typeflag |= TYPEFLAG_CONCRETE_MASK | logsize;
  }
  return typeflag;
}

/* convert full-feature MIR (kMmpl at the time) to compact MIR.
   naming: objects from full-feature MIR is prefixed with "f",
          types from full-feature MIR are mostly C++ style CamelCase
          types from compact MIR are mostly C style undercore_case

   e.g.: BaseNode *fexpr <==> base_node_t *expr
 */
// convert BaseNode to base_node_t
void CmplGenerator::CopyFields(base_node_t *fbase, base_node_t *base, mir_func_t *func, BinmirImage &image) const {
  // handle non-ptr fields
  base->op = fbase->op;
  base->primType = fbase->primType;
  base->typeflag = GenTypeFlag(fbase->primType);

  base->numOpnds = fbase->NumOpnds();
}

// T should be a derived type of base_node_t
template <typename T>
base_node_t *CmplGenerator::ExprAllocNCopy(BinmirImage &image, BaseNode *fexpr, mir_func_t *func) {
  // handle sub-expressions. sub-expressions go first.
  for (int i = 0; i < fexpr->NumOpnds(); i++) {
    MmplToCmpl(fexpr->Opnd(i), func, image);
  }

  base_node_t *expr = static_cast<base_node_t *>(image.Alloc<T>());
  CopyFields(fexpr, expr, func, image);
  return expr;
}

// record an address (to be fixed with offset) for a label related field
void CmplGenerator::RecordToFixLabelAddress(int32 *address, LabelIdx labelIdx) {
  ASSERT(label_tofix, "");
  label_tofix->insert(pair<int32 *, LabelIdx>(address, labelIdx));
}

void CmplGenerator::RecordToFixLabel16Address(int16 *address, LabelIdx labelIdx) {
  ASSERT(label16_tofix, "");
  label16_tofix->insert(pair<int16 *, LabelIdx>(address, labelIdx));
}

// the labelIdx related operations will be replaced with offset
void CmplGenerator::FixLabelOffset(LabelIdx labelIdx, uint32 offset) {
  ASSERT(label_2_offset, "");
  label_2_offset->insert(pair<LabelIdx, uint32>(labelIdx, offset));
}

/* general routine to handle expression conversion
   Note: ExprAllocNCopy should go first, to guarantee postorder
 */
base_node_t *CmplGenerator::MmplToCmpl(BaseNode *fexpr, mir_func_t *func, BinmirImage &image) {
  base_node_t *expr = nullptr;
  switch (fexpr->op) {
    // leaf opcodes
    case OP_addroffunc:
      expr = ExprAllocNCopy<addroffunc_node_t>(image, fexpr, func);
      (static_cast<addroffunc_node_t *>(expr))->puIdx = (static_cast<AddroffuncNode *>(fexpr))->puIdx;
      break;
    case OP_addroflabel:
      expr = ExprAllocNCopy<addroflabel_node_t>(image, fexpr, func);
      RecordToFixLabelAddress(&((static_cast<addroflabel_node_t *>(expr))->offset), ((AddroflabelNode *)fexpr)->offset);
      break;
    case OP_constval: {
      expr = ExprAllocNCopy<constval_node_t>(image, fexpr, func);
      // const value follows the node
      mir_intconst_t *val = image.Alloc<mir_intconst_t>();
      val->value = (static_cast<MIRIntConst *>((static_cast<ConstvalNode *>(fexpr))->constVal))->value;
      break;
    }
    // Unary opcodes
    case OP_extractbits:
      expr = ExprAllocNCopy<extractbits_node_t>(image, fexpr, func);
      (static_cast<extractbits_node_t *>(expr))->bitsOffset = (static_cast<ExtractbitsNode *>(fexpr))->bitsOffset;
      (static_cast<extractbits_node_t *>(expr))->bitsSize = (static_cast<ExtractbitsNode *>(fexpr))->bitsSize;
      break;
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
      expr = ExprAllocNCopy<unary_node_t>(image, fexpr, func);
      // no additional field to handle
      break;
    // Type conversion
    case OP_ceil:
    case OP_cvt:
    case OP_floor:
    case OP_retype:
    case OP_round:
    case OP_trunc:
      expr = ExprAllocNCopy<typecvt_node_t>(image, fexpr, func);
      (static_cast<typecvt_node_t *>(expr))->fromPrimType = (static_cast<TypeCvtNode *>(fexpr))->fromPrimType;
      (static_cast<typecvt_node_t *>(expr))->typeflag =
          GenTypeFlag((static_cast<TypeCvtNode *>(fexpr))->fromPrimType);
      break;
    case OP_intrinsicop:
      expr = ExprAllocNCopy<intrinsicop_node_t>(image, fexpr, func);
      (static_cast<intrinsicop_node_t *>(expr))->intrinsic = ((IntrinsicopNode *)fexpr)->intrinsic;
      break;
    // Binary
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
    case OP_sub:
    case OP_CG_array_elem_add:
      expr = ExprAllocNCopy<binary_node_t>(image, fexpr, func);
      // no extra fields to handle
      break;
    // comparision operation now separated to add the operand type in
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg:
    case OP_eq:
    case OP_ge:
    case OP_gt:
    case OP_le:
    case OP_lt:
    case OP_ne: {
      expr = ExprAllocNCopy<compare_node_t>(image, fexpr, func);
      // if one of its operands' type is dynamic types, the opndType
      // will be the first dynamic types.
      // otherwise, it's the type of the operands, and the operands
      // should have the same type.
      PrimType rpty0 = fexpr->Opnd(0)->primType;
      PrimType rpty1 = fexpr->Opnd(1)->primType;
      compare_node_t *compareNode = static_cast<compare_node_t *>(expr);
      compareNode->opndType = rpty0;
#ifdef DYNAMICLANG
      if (!IsPrimitiveDynType(rpty0)) {
        if (IsPrimitiveDynType(rpty1)) {
          compareNode->opndType = rpty1;
        } else {
          ASSERT(rpty0 == rpty1, "illegal comparison des type");
        }
      }
#else
      ASSERT(rpty0 == rpty1, "illegal comparison des type");
#endif

      break;
    }
    case OP_regread:
      expr = ExprAllocNCopy<regread_node_t>(image, fexpr, func);
      (static_cast<regread_node_t *>(expr))->regIdx = ((RegreadNode *)fexpr)->regIdx;
      break;
    case OP_ireadoff:
      expr = ExprAllocNCopy<ireadoff_node_t>(image, fexpr, func);
      (static_cast<ireadoff_node_t *>(expr))->offset = ((IreadoffNode *)fexpr)->offset;
      break;
    case OP_ireadfpoff:
      expr = ExprAllocNCopy<ireadoff_node_t>(image, fexpr, func);
      (static_cast<ireadoff_node_t *>(expr))->offset = ((IreadFPoffNode *)fexpr)->offset;
      break;
    case OP_select:
      expr = ExprAllocNCopy<ternary_node_t>(image, fexpr, func);
      // no extra fields to handle
      break;
    default:
      MIR_FATAL("unknown expression opcode");
  }

  return expr;
}

// T should be a derived type of stmt_node_t
template <typename T>
stmt_node_t *CmplGenerator::StmtAllocNCopy(BinmirImage &image, StmtNode *fstmt, mir_func_t *func) {
  // handle expressions. expressions go first.
  for (int i = 0; i < fstmt->NumOpnds(); i++) {
    MmplToCmpl(fstmt->Opnd(i), func, image);
  }

  stmt_node_t *stmt = static_cast<stmt_node_t *>(image.Alloc<T>());

  // handle fields from base-node
  CopyFields(static_cast<BaseNode *>(fstmt), static_cast<base_node_t *>(stmt), func, image);

  return stmt;
}

/* general routine to handle statement conversion
   Note: StmtAllocNCopy should go first, to guarantee postorder.
 */
stmt_node_t *CmplGenerator::MmplToCmpl(MIRModule &fmodule, StmtNode *fstmt, mir_func_t *func, BinmirImage &image) {
  // in postorder, stmt now starts with its expressions.
  uint8 *stmtStart = image.free_mem;
  stmt_node_t *stmt = nullptr;
  /* pending labels is used to tracking all label statement (immediately)
     before a non-label statement
   */
  static list<LabelIdx> pendingLabels;  // there're pending lable to fix
  switch (fstmt->op) {
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
      stmt = StmtAllocNCopy<intrinsiccall_stmt_t>(image, fstmt, func);
      (static_cast<intrinsiccall_stmt_t *>(stmt))->intrinsic = (static_cast<IntrinsiccallNode *>(fstmt))->intrinsic;
      break;
    case OP_call:
      stmt = StmtAllocNCopy<call_stmt_t>(image, fstmt, func);
      (static_cast<call_stmt_t *>(stmt))->puIdx = (static_cast<CallNode *>(fstmt))->puIdx;
      break;
    case OP_icall:
      stmt = StmtAllocNCopy<icall_stmt_t>(image, fstmt, func);
      // no other field to handle. puptrexp is not used any more in VM
      break;
    case OP_iassignoff:
      stmt = StmtAllocNCopy<iassignoff_stmt_t>(image, fstmt, func);
      (static_cast<iassignoff_stmt_t *>(stmt))->offset = (static_cast<IassignoffNode *>(fstmt))->offset;
      break;
    case OP_iassignfpoff:
      stmt = StmtAllocNCopy<iassignoff_stmt_t>(image, fstmt, func);
      (static_cast<iassignoff_stmt_t *>(stmt))->offset = (static_cast<IassignFPoffNode *>(fstmt))->offset;
      break;
    case OP_regassign:
      stmt = StmtAllocNCopy<regassign_stmt_t>(image, fstmt, func);
      (static_cast<regassign_stmt_t *>(stmt))->regIdx = (static_cast<RegassignNode *>(fstmt))->regIdx;
      break;
    case OP_brtrue:
    case OP_brfalse:
      stmt = StmtAllocNCopy<condgoto_stmt_t>(image, fstmt, func);
      RecordToFixLabelAddress(&((static_cast<goto_stmt_t *>(stmt))->offset), ((CondGotoNode *)fstmt)->offset);
      break;
    case OP_return: {
      // remove the redundant return 0 statement at the end of a function
      stmt = StmtAllocNCopy<nary_stmt_t>(image, fstmt, func);
      // no extra field to handle
      // set the return's primitive type to its first operand if have
      if (fstmt->NumOpnds() > 0) {
        (static_cast<base_node_t *>(stmt))->primType = fstmt->Opnd(0)->primType;
      }
      break;
    }
    case OP_goto:
    case OP_gosub:
      stmt = StmtAllocNCopy<goto_stmt_t>(image, fstmt, func);
      RecordToFixLabelAddress(&((static_cast<goto_stmt_t *>(stmt))->offset), ((GotoNode *)fstmt)->offset);
      break;
    case OP_jstry: {
      stmt = StmtAllocNCopy<try_stmt_t>(image, fstmt, func);
      try_stmt_t *tryStmt = static_cast<try_stmt_t *>(stmt);
      tryStmt->catchOffset = 0;
      tryStmt->finallyOffset = 0;
      try_stmts->push(tryStmt);
      break;
    }
    case OP_jscatch: {
      stmt = StmtAllocNCopy<stmt_node_t>(image, fstmt, func);
      try_stmt_t *tryStmt = try_stmts->top();
      ASSERT(tryStmt, "");
      ASSERT((uintptr_t)stmt > (uintptr_t)tryStmt, "");
      uint32 offset = static_cast<uint32>((uintptr_t)stmt - (uintptr_t)tryStmt);
      ASSERT(offset <= MAXUINT16, "");
      tryStmt->catchOffset = static_cast<uint16>(offset);
      break;
    }
    case OP_finally: {
      stmt = StmtAllocNCopy<stmt_node_t>(image, fstmt, func);
      try_stmt_t *tryStmt = try_stmts->top();
      ASSERT(tryStmt, "");
      ASSERT((uintptr_t)stmt > (uintptr_t)tryStmt, "");
      uint32 offset = static_cast<uint32>((uintptr_t)stmt - (uintptr_t)tryStmt);
      ASSERT(offset <= MAXUINT16, "");
      tryStmt->finallyOffset = static_cast<uint16>(offset);
      break;
    }
    case OP_retsub:
    case OP_cleanuptry:
      stmt = StmtAllocNCopy<stmt_node_t>(image, fstmt, func);
      // no extra field to handle
      break;
    case OP_endtry:
      stmt = StmtAllocNCopy<stmt_node_t>(image, fstmt, func);
      ASSERT(try_stmts->top() != nullptr, "");
      try_stmts->pop();
      break;
    case OP_rangegoto: {
      stmt = StmtAllocNCopy<rangegoto_stmt_t>(image, fstmt, func);

      // handle switch table
      int numCases = (static_cast<RangegotoNode *>(fstmt))->rangegotoTable.size();
      ASSERT((numCases >= 0) && (numCases < 256), "");
      small_case_vec_t *caseVec = image.Alloc<small_case_vec_t>();
      caseVec->num_cases = (uint8)numCases;
      small_case_pair_t *cases = image.AllocArray<small_case_pair_t>(numCases);

      for (int i = 0; i < numCases; i++) {
        cases[i].first = (static_cast<RangegotoNode *>(fstmt))->rangegotoTable[i].first;
        RecordToFixLabel16Address(&(cases[i].second),
                                  (LabelIdx)(static_cast<RangegotoNode *>(fstmt))->rangegotoTable[i].second);
      }

      break;
    }
    case OP_throw:
    case OP_free:
    case OP_syncenter:
    case OP_syncexit:
      stmt = StmtAllocNCopy<unary_stmt_t>(image, fstmt, func);
      // no extra field to handle
      break;
    case OP_label:
      // add the label to pending label list. No need to translate it into kCmpl
      pendingLabels.push_front((static_cast<LabelNode *>(fstmt))->labelIdx);
      break;
    case OP_comment:
      break;  // just omit
    default:
      MIR_FATAL("unknown statement opcode\n");
  }

  if (stmt && !pendingLabels.empty()) {
    // this stmt is a target of label(s): add it to label_2_offset map
    uint32 offset = static_cast<uint32>((unsigned long)image.GetRelativePtr(func, stmtStart));
    list<LabelIdx>::iterator iter;
    for (iter = pendingLabels.begin(); iter != pendingLabels.end(); ++iter) {
      FixLabelOffset(*iter, offset);
    }
    // empty the list now
    pendingLabels.clear();
  }
  return stmt;
}

// handle block specially
stmt_node_t *CmplGenerator::MmplToCmpl(MIRModule &fmodule, BlockNode *fblock, mir_func_t *func, BinmirImage &image) {
  StmtNode *fstmt = fblock->GetFirst();
  StmtNode *firstStmt = nullptr;
  while (fstmt) {
    stmt_node_t *stmt = MmplToCmpl(fmodule, fstmt, func, image);
    // stmt can be nullptr; because some fstmts are omited in compact IR
    if (stmt && !firstStmt) {
      firstStmt = static_cast<StmtNode *>(stmt);
    }
    fstmt = fstmt->GetNext();
  }

  return firstStmt;
}

// translate MIRFunction into BinmirImage.
mir_func_t *CmplGenerator::MmplToCmpl(MIRModule &fmodule, MIRFunction *ffunc, BinmirImage &image) {
  // translation context setup
  ASSERT(
    (label_tofix == nullptr) && (label_2_offset == nullptr) && (label16_tofix == nullptr) && (try_stmts == nullptr),
    "");
  label_2_offset = new map<LabelIdx, int32>();
  label_tofix = new map<int32 *, LabelIdx>();
  label16_tofix = new map<int16 *, LabelIdx>();
  try_stmts = new stack<try_stmt_t *>();

  // translation phase-1
  // :also collect necessary information for phase-2
  mir_func_t *func = image.Alloc<mir_func_t>();
  errno_t eNum = memset_s(func, sizeof(mir_func_t), 0, sizeof(mir_func_t));
  if (eNum) {
    FATAL(kLncFatal, "memset_s failed");
  }
  func->frameSize = static_cast<uint16>(ffunc->frameSize);
  func->upFormalSize = static_cast<uint16>(ffunc->upFormalSize);
  MmplToCmpl(fmodule, ffunc->body, func, image);

  // translation phase2a: fix labels with relative offset to the function
  map<int32 *, LabelIdx>::const_iterator tofixIter;
  for (tofixIter = label_tofix->begin(); tofixIter != label_tofix->end(); ++tofixIter) {
    LabelIdx labidx = tofixIter->second;
    if (label_2_offset->find(labidx) != label_2_offset->end()) {
      int32 offset = label_2_offset->find(labidx)->second;
      int32 *tofixOffset = tofixIter->first;
      *tofixOffset = offset;
    } else {
      MIR_FATAL("cannot find the label %d to fix\n", labidx);
    }
  }
  /* translation phase2b: fix labels stored in 16 bits with relative offset
     to the function
     TODO: should we make it relative to the switch stmt instead of function?
   */
  map<int16 *, LabelIdx>::const_iterator tofix16Iter;
  for (tofix16Iter = label16_tofix->begin(); tofix16Iter != label16_tofix->end(); ++tofix16Iter) {
    LabelIdx labidx = tofix16Iter->second;
    if (label_2_offset->find(labidx) != label_2_offset->end()) {
      int32 offset = label_2_offset->find(labidx)->second;
      int16 *tofixOffset = tofix16Iter->first;
      ASSERT((offset >> 16) == 0, "");  // the offset better fits within 16 bits
      *tofixOffset = offset;
    } else {
      MIR_FATAL("cannot find the label %d to fix\n", labidx);
    }
  }
  /* output the formalwordstypetagged, formalwordsrefcounted,
     localwordstypetagged and localwordsrefcounted bitvectors */
  size_t size;
  if (func->upFormalSize != 0) {
    func->formalWordsTypeTagged = static_cast<uint8 *>(image.Alloc(BlkSize2BitvectorSize(ffunc->upFormalSize)));
    size = (size_t)BlkSize2BitvectorSize(ffunc->upFormalSize);
    eNum = memcpy_s(static_cast<void *>(func->formalWordsTypeTagged), size,
                    (const void *)ffunc->formalWordsTypeTagged, size);
    if (eNum != 0) {
      CHECK_FATAL(eNum == 0, "memcpy_s failed");
    }

    func->formalWordsRefCounted = static_cast<uint8 *>(image.Alloc(BlkSize2BitvectorSize(ffunc->upFormalSize)));
    size = (size_t)BlkSize2BitvectorSize(ffunc->upFormalSize);
    eNum = memcpy_s(static_cast<void *>(func->formalWordsRefCounted), size,
                    (const void *)ffunc->formalWordsRefCounted, size);
    if (eNum != 0) {
      CHECK_FATAL(eNum == 0, "memcpy_s failed");
    }
  }
  func->localWordsTypeTagged = static_cast<uint8 *>(image.Alloc(BlkSize2BitvectorSize(ffunc->frameSize)));
  size = (size_t)BlkSize2BitvectorSize(ffunc->frameSize);
  eNum = memcpy_s(static_cast<void *>(func->localWordsTypeTagged), size,
                  (const void *)ffunc->localWordsTypeTagged, size);
  if (eNum != 0) {
    CHECK_FATAL(eNum == 0, "memcpy_s failed");
  }

  func->localWordsRefCounted = static_cast<uint8 *>(image.Alloc(BlkSize2BitvectorSize(ffunc->frameSize)));
  size = (size_t)BlkSize2BitvectorSize(ffunc->frameSize);
  eNum = memcpy_s(static_cast<void *>(func->localWordsRefCounted), size,
                  (const void *)ffunc->localWordsRefCounted, size);
  if (eNum != 0) {
    CHECK_FATAL(eNum == 0, "memcpy_s failed");
  }

  delete label_2_offset;
  delete label_tofix;
  delete label16_tofix;
  delete try_stmts;
  label_2_offset = nullptr;
  label_tofix = nullptr;
  label16_tofix = nullptr;
  try_stmts = nullptr;
  return func;
}

// translate MIRModule into a binary-format image (corresponding to an IR file);
// file_name is only for generating a unique module id
mir_module_t *CmplGenerator::MmplToCmpl(MIRModule &fmodule, BinmirImage &image, const char *fileName) {
  // collect global information.
  // note: mir_module_t will not be in the image currently
  mir_module_t *module = static_cast<mir_module_t *>(malloc(sizeof(mir_module_t)));
  if (module == nullptr){
    CHECK_FATAL(false, "malloc failed");
  }
  module->flavor = fmodule.flavor;
  module->srcLang = fmodule.srcLang;

  uint32 fileNameLength = strlen(fileName);
  uint16 moduleid = fileNameLength & 0xf;
  moduleid |= (fileName[fileNameLength - 1] & 0xf) << 4;
  if (fileNameLength >= 2) {
    moduleid |= (fileName[fileNameLength - 2] & 0xf) << 8;
  }
  if (fileNameLength >= 3) {
    moduleid |= (fileName[fileNameLength - 3] & 0xf) << 12;
  }
  module->id = moduleid;
  module->globalMemSize = fmodule.globalMemSize;
  module->globalBlkMap = fmodule.globalBlkMap;
  module->globalWordsTypeTagged = fmodule.globalWordsTypeTagged;
  module->globalWordsRefCounted = fmodule.globalWordsRefCounted;
  module->numFuncs = GlobalTables::GetFunctionTable().funcTable.size() - 1;

  /*         Bin MIR image actually starts from here         */
  // binmir image header
  binmir_file_header_t *header = image.Alloc<binmir_file_header_t>();
  errno_t eNum = strncpy_s(header->magic, sizeof(header->magic), BIN_MIR_FILE_ID, sizeof(BIN_MIR_FILE_ID));
  if (eNum) {
    FATAL(kLncFatal, "strncpy_s failed");
  }
  header->segNum = 1;               // 1 segment so far
  header->type = BINMIR_FILE_TYPE;  // from mircompact.h
  header->version = MAKE_VERSION_NUM(VERSION_MAJOR, VERSION_MINOR);
  MIR_INFO("genCMPL: %d segment(s) in the image\n", header->segNum);


  uint32 *numFuncs = image.Alloc<uint32>();
  *numFuncs = module->numFuncs;

  // entrance move to here, for padding.
  uint32 mainFuncId = FindMainFuncPuid(fmodule);
  uint32 *moduleEntrance = image.Alloc<uint32>();
  *moduleEntrance = mainFuncId;
  uint16 *moduleId = image.Alloc<uint16>();
  *moduleId = moduleid;
  uint16 *srclang = image.Alloc<uint16>();
  *srclang = module->srcLang;

  // allocate function table, table size already in header
  module->funcs = image.AllocArray<mir_func_t *>(module->numFuncs);
  ASSERT(module->funcs, "");
  MIR_INFO("genCMPL: %d funcs in the image\n", module->numFuncs);

  // We kept it in the memory map to enable compatibility with full MIR
  uint32 *globalBlkSize = image.Alloc<uint32>();
  *globalBlkSize = fmodule.globalMemSize;

  uint8 *globalBlkMap = image.AllocArray<uint8>(fmodule.globalMemSize);
  memcpy_s(static_cast<void *>(globalBlkMap), (size_t)fmodule.globalMemSize,
           (const void *)fmodule.globalBlkMap, (size_t)fmodule.globalMemSize);
  uint8 *globalwordstypetagged = image.AllocArray<uint8>(BlkSize2BitvectorSize(fmodule.globalMemSize));
  memcpy_s(static_cast<void *>(globalwordstypetagged),
           (size_t)BlkSize2BitvectorSize(fmodule.globalMemSize),
           (const void *)fmodule.globalWordsTypeTagged,
           (size_t)BlkSize2BitvectorSize(fmodule.globalMemSize));

  uint8 *globalwordsrefcounted = image.AllocArray<uint8>(BlkSize2BitvectorSize(fmodule.globalMemSize));
  memcpy_s(static_cast<void *>(globalwordsrefcounted),
           (size_t)BlkSize2BitvectorSize(fmodule.globalMemSize),
           (const void *)fmodule.globalWordsRefCounted,
           (size_t)BlkSize2BitvectorSize(fmodule.globalMemSize));

  MIR_INFO("genCMPL: write global block map: %d Bytes\n", fmodule.globalMemSize);
  MIR_INFO("genCMPL: write global words typetagged: %d Bytes\n", BlkSize2BitvectorSize(fmodule.globalMemSize));
  MIR_INFO("genCMPL: write global words refcounted: %d Bytes\n", BlkSize2BitvectorSize(fmodule.globalMemSize));

  mir_func_t *func = nullptr;
  mir_func_t *lastFunc = nullptr;
  for (MapleVector<MIRFunction *>::iterator it = fmodule.functionList.begin(); it != fmodule.functionList.end();
       it++) {
    MIRFunction *fcurrFunc = *it;
    func = MmplToCmpl(fmodule, fcurrFunc, image);
    if (lastFunc) {
      lastFunc->funcSize = static_cast<uint32>((uintptr_t)func - (uintptr_t)lastFunc);
    }
    func->moduleID = moduleid;
    if (func->formalWordsTypeTagged != nullptr) {
      func->formalWordsTypeTagged = image.GetRelativePtr(func, func->formalWordsTypeTagged);
    }
    if (func->formalWordsRefCounted != nullptr) {
      func->formalWordsRefCounted = image.GetRelativePtr(func, func->formalWordsRefCounted);
    }
    if (func->localWordsTypeTagged != nullptr) {
      func->localWordsTypeTagged = image.GetRelativePtr(func, func->localWordsTypeTagged);
    }
    if (func->localWordsRefCounted != nullptr) {
      func->localWordsRefCounted = image.GetRelativePtr(func, func->localWordsRefCounted);
    }
    module->funcs[fcurrFunc->puIdx - 1] = image.GetRelativePtr(func);
    //  fprintf(stderr, "emitting function $%s\n",
    //  module->symTab->GetSymbolFromStIdx(fcurr_func->stIdx.Idx())->GetName().c_str());
    lastFunc = func;
  }
  if (lastFunc) {
    CHECK_FATAL(func != nullptr, "func is null in CmplGenerator::MmplToCmpl");
    func->funcSize = static_cast<uint32>((uintptr_t)image.free_mem - (uintptr_t)lastFunc);
  }

  return module;
}

void CmplGenerator::StoreBinMir(mir_module_t *module, const BinmirImage &image, const char *binmirName) {
  FILE *binmirFile = fopen(binmirName, "wb");
  if (!binmirFile) {
    MIR_FATAL("cannot open bin-mir file %s to write\n", binmirName);
  }

  MIR_INFO("genCMPL: write binmir image into file: %d Bytes\n", static_cast<int>(image.alloced_size));
  int numwritten = fwrite(image.alloc_mem, image.alloced_size, 1, binmirFile);
  if (numwritten != 1) {
    CHECK_FATAL(false, "call fwrite failed");
  }
  fclose(binmirFile);
}

// given the filename stem, generate the cmpl from module and output it to
// the file with .cmpl suffix.
void OutputCmpl(MIRModule &module, const char *filestem) {
  CmplGenerator cmplGenerator;
  BinmirImage image;
  mir_module_t *compactMir = cmplGenerator.MmplToCmpl(module, image, filestem);
  ASSERT(compactMir, "");
  char *outFileName = new char[strlen(filestem) + 6];
  errno_t eNum = strcpy_s(outFileName, strlen(filestem) + 6, filestem);
  if (eNum) {
    CHECK_FATAL(false, "strcpy_s failed");
  }
  eNum = strcat_s(outFileName, strlen(filestem) + 6, ".cmpl");
  if (eNum) {
    CHECK_FATAL(false, "strcpy_s failed");
  }
  outFileName[strlen(filestem) + 5] = '\0';
  cmplGenerator.StoreBinMir(compactMir, image, outFileName);
  delete[] outFileName;
  outFileName = nullptr;
  free(static_cast<void *>(compactMir));
  return;
}
