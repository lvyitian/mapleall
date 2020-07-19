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
// This define the lowest level MAPLE IR data structures that are compatible
// with both the C++ and C coding environments of MAPLE

#ifndef MAPLE_IR_INCLUDE_CMPL_H
#define MAPLE_IR_INCLUDE_CMPL_H

// Still need constant value from MIR
#include "mir_config.h"
#include "types_def.h"
#include "opcodes.h"
#include "prim_types.h"
#include "intrinsics.h"
#include <stdint.h>


namespace maple {

enum MIRFlavor {
  kFlavorUnknown,
  kFeProduced,
  kMeProduced,
  kBeLowered,
  kMmpl,
  kCmplV1,
  kCmpl  // == CMPLv2
};

enum MIRSrcLang {
  kSrcLangUnknown,
  kSrcLangC,
  kSrcLangJs,
  kSrcLangJava,
  kSrcLangCPlusPlus,
};

// blksize gives the size of the memory block in bytes; there are (blksize+3)/4
// words; 1 bit for each word, so the bit vector's length in bytes is
// ((blksize+3)/4+7)/8
static inline uint32 BlkSize2BitvectorSize(uint32 blksize) {
  uint32 bitvectorlen = ((blksize + 3) / 4 + 7) / 8;
  return ((bitvectorlen + 3) >> 2) << 2;  // round up to word boundary
}

struct mir_func_t {  // 28B
  uint16 frameSize;
  uint16 upFormalSize;
  uint16 moduleID;
  uint32 funcSize;                 // size of code in words
  uint8 *formalWordsTypeTagged = nullptr;  // bit vector where the Nth bit tells whether
                                   // the Nth word in the formal parameters area
                                   // addressed upward from %%FP (that means
                                   // the word at location (%%FP + N*4)) has
                                   // typetag; if yes, the typetag is the word
                                   // at (%%FP + N*4 + 4); the bitvector's size
                                   // is given by BlkSize2BitvectorSize(upFormalSize)
  uint8 *localWordsTypeTagged = nullptr;  // bit vector where the Nth bit tells whether
                                  // the Nth word in the local stack frame
                                  // addressed downward from %%FP (that means
                                  // the word at location (%%FP - N*4)) has
                                  // typetag; if yes, the typetag is the word
                                  // at (%%FP - N*4 + 4); the bitvector's size
                                  // is given by BlkSize2BitvectorSize(frameSize)
  uint8 *formalWordsRefCounted = nullptr;  // bit vector where the Nth bit tells whether
                                   // the Nth word in the formal parameters area
                                   // addressed upward from %%FP (that means
                                   // the word at location (%%FP + N*4)) points to
                                   // a dynamic memory block that needs reference
                                   // count; the bitvector's size is given by
                                   // BlkSize2BitvectorSize(upFormalSize)
  uint8 *localWordsRefCounted = nullptr;  // bit vector where the Nth bit tells whether
                                  // the Nth word in the local stack frame
                                  // addressed downward from %%FP (that means
                                  // the word at location (%%FP - N*4)) points to
                                  // a dynamic memory block that needs reference
                                  // count; the bitvector's size is given by
                                  // BlkSize2BitvectorSize(frameSize)
                                  // uint16 numlabels; // removed. label table size
                                  // StmtNode **lbl2stmt; // lbl2stmt table, removed;

  // the first statement immediately follow mir_func_t
  // since it starts with expression, base_node_t* is returned
  inline void *FirstInst() const {
    return (void *)((uint8 *)this + sizeof(mir_func_t));
  }

  // there are 4 bitvectors that follow the function code
  inline uint32 FuncCodeSize() const {
    return funcSize - 2 * BlkSize2BitvectorSize(upFormalSize) - 2 * BlkSize2BitvectorSize(frameSize);
  }

  virtual ~mir_func_t() {};
};

struct mir_module_t {
 private:
  mir_func_t *curFunction;

 public:
  MIRFlavor flavor;    // should be kCmpl
  MIRSrcLang srcLang;  // the source language
  uint16 id;
  uint32 globalMemSize;  // size of storage space for all global variables
  uint8 *globalBlkMap;   // the memory map of the block containing all the
  // globals, for specifying static initializations
  uint8 *globalWordsTypeTagged;  // bit vector where the Nth bit tells whether
  // the Nth word in globalBlkMap has typetag;
  // if yes, the typetag is the N+1th word; the
  // bitvector's size is given by
  // BlkSize2BitvectorSize(globalMemSize)
  uint8 *globalWordsRefCounted;  // bit vector where the Nth bit tells whether
  // the Nth word points to a reference-counted
  // dynamic memory block; the bitvector's size
  // is given by BlkSize2BitvectorSize(globalMemSize)
  PUIdx mainFuncID;  // the entry function; 0 if no main function
  uint32 numFuncs;      // because puIdx 0 is reserved, numFuncs is also the highest puIdx

  mir_func_t **funcs;  // list of all funcs in the module.

                             // the js2mpl buld always set HAVE_MMAP to 1 // binmir file mmap info
  int binMIRImageFD;         // file handle for mmap
                             // HAVE_MMAP
  void *binMIRImageStart;    // binimage memory start
  uint32 binMIRImageLength;  // binimage memory size

  inline mir_func_t *FuncFromPuidx(PUIdx puIdx) const {
    MIR_ASSERT(puIdx <= (uint32)numFuncs && "");  // puIdx starts from 1
    return funcs[puIdx - 1];
  }

  virtual ~mir_module_t() {};

  inline mir_func_t *MainFunc() const {
    return mainFuncID == 0 ? static_cast<mir_func_t *>(nullptr) : FuncFromPuidx(mainFuncID);
  }

  inline void SetCurFunction(mir_func_t *const f) {
    curFunction = f;
  };

  inline mir_func_t *GetCurFunction() const {
    return curFunction;
  }
};

// At this stage, mir_const_t don't need all information in MIRConst
// Note: only be used within Constval node:
// Warning: it's different from full feature MIR.
// only support 32bit int const (lower 32bit). higher 32bit are tags
union mir_intconst_t {
  int64 value;
  uint32 val_[2];  // ARM target load/store 2 32bit val instead of 1 64bit
};

// currently in VM, only intconst are used.
typedef mir_intconst_t mir_const_t;

/*
 * It's a stacking of POD data structure to allow precise memory layout
 * control and emulate the inheritance relationship of corresponding C++
 * data structures to keep the interface consistent (as much as possible).
 *
 * Rule:
 * 1. base struct should be the first member (to allow safe pointer casting)
 * 2. each node (just ops, no data) should be of either 4B or 8B.
 * 3. casting the node to proper base type to access base type's fields.
 *
 * Current memory layout of nodes follows the postfix notation:
 * Each operand instruction is positioned immediately before its parent or
 * next operand. Memory layout of sub-expressions tree is done recursively.
 * E.g. the code for (a + b) contains 3 instructions, starting with the READ a,
 * READ b, and then followed by ADD.
 * For (a + (b - c)), it is:
 *
 * READ a
 * READ b
 * READ c
 * SUB
 * ADD
 *
 */

// base_node_t is an abstraction of expression.
struct base_node_t {  // 4B
  Opcode op : 8;
  PrimType primType : 8;
  uint8 typeflag;      // a flag to speed up type related operations in the VM
  uint8 numOpnds : 8;  // only used for N-ary operators, switch and rangegoto
  // operands immediately before each node
  // base_node_t **operands;
  int32 NumOpnds(void) const {
    if (op == OP_switch || op == OP_rangegoto) {
      return 1;
    }
    return numOpnds;
  }

  // virtual ~base_node_t(){};
};

/* typeflag is a 8bit flag to provide short-cut information for its
 * associated PrimType, because many type related information extraction
 * is not very lightweight.
 * Here is the convention:
 * |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
 *   dyn    f     i     sc    c     (log2(size))
 *
 * bit 0 - bit 3 is for type size information. (now not used in VM?)
 *   bit 0-2 represents the size of concrete types (not void/aggregate)
 *   it's the result of log2 operation on the real size to fit in 3 bits.
 *   which has the following correspondence:
 *   | 2 | 1 | 0 |              type size (in Bytes)
 *     0   0   0                      1
 *     0   0   1                      2
 *     0   1   0                      4
 *     0   1   1                      8
 *     1   0   0                     16
 *
 * bit 3 is the flag of "concrete types", i.e., types we know the type
 * details.
 * when it's 1, the bit0-2 size are valid
 * when it's 0, the size of the type is 0, and bit0-2 are meaningless.
 *
 * bit 4 is for scalar types (1 if it's a scalar type)
 * bit 5 is for integer types (1 if it's an integer type)
 * bit 6 is for floating types (1 if it's a floating type)
 * bit 7 is for dynamic types (1 if it's a dynamic type)
 *
 * refer to mirtypes.h/mirtypes.cpp in mapleir directory for more information.
 */
#define TYPEFLAG_DYN_MASK 0x80
#define TYPEFLAG_FLOAT_MASK 0x40
#define TYPEFLAG_INTEGER_MASK 0x20
#define TYPEFLAG_SCALAR_MASK 0x10
#define TYPEFLAG_CONCRETE_MASK 0x08
#define TYPEFLAG_SIZE_MASK 0x07
#define TYPEFLAG_DYN_FLOAT_MASK (TYPEFLAG_DYN_MASK | TYPEFLAG_FLOAT_MASK)
#define TYPEFLAG_DYN_INTEGER_MASK (TYPEFLAG_DYN_MASK | TYPEFLAG_INTEGER_MASK)

inline bool IsDynType(uint8 typeflag) {
  return ((typeflag)&TYPEFLAG_DYN_MASK);
}

inline bool IsDynFloat(uint8 typeflag) {
  return (((typeflag)&TYPEFLAG_DYN_FLOAT_MASK) == TYPEFLAG_DYN_FLOAT_MASK);
}

inline bool IsDynInteger(uint8 typeflag) {
  return (((typeflag)&TYPEFLAG_DYN_INTEGER_MASK) == TYPEFLAG_DYN_INTEGER_MASK);
}

// IsFloat means "is statically floating types", i.e., float, but not dynamic
inline bool IsFloat(uint8 typeflag) {
  return (((typeflag)&TYPEFLAG_DYN_FLOAT_MASK) == TYPEFLAG_FLOAT_MASK);
}

inline bool IsScalarType(uint8 typeflag) {
  return ((typeflag)&TYPEFLAG_SCALAR_MASK);
}

inline Opcode GetOpcode(const base_node_t *nodeptr) {
  return nodeptr->op;
}

inline PrimType GetPrimtype(const base_node_t *nodeptr) {
  return nodeptr->primType;
}

inline uint32 GetOperandsNum(const base_node_t *nodeptr) {
  return nodeptr->numOpnds;
}

typedef base_node_t unary_node_t;  // alias

struct typecvt_node_t : public base_node_t {  // 8B
  PrimType fromPrimType : 8;
  uint8 fromtypeflag;  // a flag to speed up type related operations
  uint8 _padding_[2];
  inline PrimType FromType() const {
    return fromPrimType;
  }
};

struct extractbits_node_t : public base_node_t {  // 8B
  uint8 bitsOffset;
  uint8 bitsSize;
  uint16 _padding_;
};

struct ireadoff_node_t : public base_node_t {  // 8B
  int32 offset;
};

typedef base_node_t binary_node_t;

// Add expression types to compare node, to
// facilitate the evaluation of postorder stored kCmpl
// Note: the two operands should have the same type if they're
//       not dynamic types
struct compare_node_t : public base_node_t {  // 8B
  PrimType opndType : 8;                      // type of operands.
  uint8 opndtypeflag;                         // typeflag of opntype.
  uint8 _padding_[2];
};

typedef base_node_t ternary_node_t;
typedef base_node_t nary_node_t;

// need to guarantee MIRIntrinsicID is 4B
// Note: this is not supported by c++0x
// static_assert(sizeof(MIRIntrinsicID) == 4);

struct intrinsicop_node_t : public base_node_t {  // 8B
  MIRIntrinsicID intrinsic;
};

#pragma pack(push,4)
struct constval_node_t : public base_node_t {  // 4B + 8B const value
  mir_const_t constVal; // constVal follows constval_node_t
  inline mir_const_t *Constval() const {
    return (const_cast<mir_const_t *>(&constVal));
  }
};
#pragma pack(pop)

// full MIR exported a pointer to mir_const_t
inline mir_const_t *GetConstval(constval_node_t *node) {
  return node->Constval();
}

// SizeoftypeNode shouldn't be seen here
// ArrayNode shouldn't be seen here

struct addrof_node_t : public base_node_t {  // 12B
  StIdx stIdx;
  FieldID fieldID;
};

typedef addrof_node_t dread_node_t;  // same shape.

struct addroffunc_node_t : public base_node_t {  // 8B
  PUIdx puIdx;                                 // 32bit now
};

struct regread_node_t : public base_node_t {  // 8B
  PregIdx regIdx;                           // 32bit, negative if special register
};

struct addroflabel_node_t : public base_node_t {  // 8B
  // LabelIdx labelIdx; // 32bit, replaced by offset
  int32 offset;
};

}  // namespace maple

#endif  // MAPLE_IR_INCLUDE_CMPL_H
