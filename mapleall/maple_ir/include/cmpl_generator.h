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

// license/copyrights comes here
//
// The compact MIR implementation: translate full MIR (kMmpl) to
// compact MIR (kCmpl-v2)
// Note:
// 1.MIR deserialization (read) is in cmpl.cpp
// 2.this code should be compiled with MIR_FEATURE_FULL=1
//
// 1. it shouldn't have any pointers in the IR
// 2. assume LittleEndian for binary format (we don't explicitly handle it here)
// 3. postorder layout for evaluation efficiency.
//
// TODO:
// 1. change it to CPP style to unify the coding convention
// 1. using ROPE for string
// 2. binary compatibility across platforms (e.g., for endianness)
//

#ifndef MAPLE_IR_INCLUDE_CMPL_GENERATOR_H
#define MAPLE_IR_INCLUDE_CMPL_GENERATOR_H

#ifdef MIR_FEATURE_FULL
#undef MIR_FEATURE_FULL
#endif

#define MIR_FEATURE_FULL 1

#include "cmpl.h"
#include "cmpl_supp.h"
#include "mir_module.h"
#include "mir_symbol.h"
#include "mir_nodes.h"
#include "mir_function.h"
#include "mir_parser.h"
#include "opcode_info.h"
#include "../../mempool/include/mempool.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <map>
#include <list>
#include <stack>

using namespace maple;
using namespace std;

#define BINMIR_FILE_TYPE kMjsvmFileTypeCmpl  // moved from cmpl.h
#define BINMIR_MEM_ALLOC_SIZE (1 << 16)      // 64KB
// a simple memory manager for compact-MIR assemble.

class BinmirImage {
 public:
  size_t total_size;    // the maximum size of the image memory
  void *alloc_mem;      // the memory area holding binmir
  uint8 *free_mem;      // ptr to (next) free mem in the image
  size_t alloced_size;  // how much memory already used in the image
 public:
  BinmirImage(const BinmirImage &p) = default;
  BinmirImage &operator=(const BinmirImage &p) = default;
  BinmirImage() {
    total_size = BINMIR_MEM_ALLOC_SIZE;
    alloc_mem = malloc(total_size);
    ASSERT(alloc_mem, "");
    free_mem = (uint8 *)alloc_mem;
    alloced_size = 0;
  }

  ~BinmirImage() {
    free(alloc_mem);
  }

  void *Alloc(size_t size) {
    ASSERT(size + alloced_size < total_size, "");
    void *returnPtr = (void *)free_mem;
    free_mem += size;
    alloced_size += size;
    return returnPtr;
  }

  template <typename T>
  inline T *Alloc() {
    return (T *)Alloc(sizeof(T));
  }

  template <typename T>
  inline T *AllocArray(int numElements) {
    return (T *)Alloc(sizeof(T) * numElements);
  }

  /* return the offseted ptr relative to the image,
   * it should be allocated by this memory manager */
  template <typename T>
  inline T *GetRelativePtr(const T *ptr) {
    ASSERT(ptr, "");
    ASSERT(((void *)ptr >= alloc_mem) && ((uint8 *)ptr <= (uint8 *)alloc_mem + total_size), "");
    return (T *)((uint8 *)ptr - (uint8 *)alloc_mem);
  }

  /* return the offseted ptr relative to a function,
   * it should be allocated by this memory manager */
  template <typename T>
  inline T *GetRelativePtr(const mir_func_t *func, const T *ptr) {
    ASSERT(((void *)ptr >= (void *)func) && ((uint8 *)ptr <= ((uint8 *)alloc_mem + total_size)), "");
    return (T *)((uint8 *)ptr - (uint8 *)func);
  }
};

class CmplGenerator {
 private:
  // label (index) to offset translation table
  map<LabelIdx, int32> *label_2_offset;
  // The address to be fixed (with offset) for labels (32bit offset)
  map<int32 *, LabelIdx> *label_tofix;
  // The address to be fixed (with offset) for labels (16bit offset)
  map<int16 *, LabelIdx> *label16_tofix;
  // The (stack of) try stmts remain to be (post)processed.
  stack<try_stmt_t *> *try_stmts;

 public:
  CmplGenerator() : label_2_offset(nullptr), label_tofix(nullptr), label16_tofix(nullptr), try_stmts(nullptr) {}

  virtual ~CmplGenerator() {}

  uint32 FindMainFuncPuid(const MIRModule &fmodule);
  uint8 GenTypeFlag(PrimType type) const;
  void CopyFields(base_node_t *fbase, base_node_t *base, mir_func_t *func, BinmirImage &image) const;
  template <typename T>
  base_node_t *ExprAllocNCopy(BinmirImage &image, BaseNode *fexpr, mir_func_t *func);
  void RecordToFixLabelAddress(int32 *address, LabelIdx labelIdx);
  void RecordToFixLabel16Address(int16 *address, LabelIdx labelIdx);
  void FixLabelOffset(LabelIdx labelIdx, uint32 offset);
  base_node_t *MmplToCmpl(BaseNode *fexpr, mir_func_t *func, BinmirImage &image);
  template <typename T>
  stmt_node_t *StmtAllocNCopy(BinmirImage &image, StmtNode *fstmt, mir_func_t *func);
  stmt_node_t *MmplToCmpl(MIRModule &fmodule, StmtNode *fstmt, mir_func_t *func, BinmirImage &image);
  stmt_node_t *MmplToCmpl(MIRModule &fmodule, BlockNode *fblock, mir_func_t *func, BinmirImage &image);
  mir_func_t *MmplToCmpl(MIRModule &fmodule, MIRFunction *ffunc, BinmirImage &image);
  mir_module_t *MmplToCmpl(MIRModule &fmodule, BinmirImage &image, const char *);
  void StoreBinMir(mir_module_t *module, const BinmirImage &image, const char *binmirName);
};  // class cmplGenerator

extern void OutputCmpl(MIRModule &module, const char *filestem);

#endif  // MAPLE_IR_INCLUDE_CMPL_GENERATOR_H
