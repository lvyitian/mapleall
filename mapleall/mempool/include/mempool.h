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

#ifndef MEMORY_POOL_DATA_HPP
#define MEMORY_POOL_DATA_HPP

#include "../../maple_ir/include/mir_config.h"
#include "mpl_logging.h"

// Local debug control
#ifdef MP_DEBUG
#include <iostream>
#endif  // MP_DEBUG

#include <list>
#include <set>
#include <stack>
#include <map>
#include <string>
#define FALSE false
#define TRUE true

namespace maple {

// Class declaration
class MemPool;

// Memory Pool controller class
class MemPoolCtrler {
  friend MemPool;

 public:  // Methods
  MemPoolCtrler() {}

  ~MemPoolCtrler();
  MemPool *NewMemPool(const char *);
  void DeleteMemPool(MemPool *mp);
  bool IsEmpty() const {
    return mem_pools.size() == 0;
  }

  unsigned GetMempoolSize() const {
    return mem_pools.size();
  }

 private:  // Methods
  struct MemBlock {
    unsigned int avail;      // Available memory size
    unsigned int orig_size;  // original size
    void *ptr;               // Current pointer to the first available position
  };
  class MemBlockCmp {
   public:
    bool operator()(const MemBlock *l, const MemBlock *r) const {
      return l->avail > r->avail;
    }
  };

  // Free small/large size memory block list
  std::list<MemBlock *> free_mem_blocks;
  std::map<unsigned int, std::set<MemBlock *, MemBlockCmp>> large_free_mem_blocks;
  std::set<MemPool *> mem_pools;  // set of mempools managed by it
};

class MemPool {
  friend MemPoolCtrler;

 public:  // Methods
  MemPool(MemPoolCtrler *ctl, const char *name) : ctrler(ctl), name(name) {
#ifdef MP_DEBUG
    frozen = FALSE;
    std::cout << "MEMPOOL: New " << name << std::endl;
#endif
  }

  ~MemPool();

  void *Malloc(size_t size);
  void *Calloc(size_t size);
  void *Realloc(const void *ptr, size_t oldSize, size_t newSize);
  void Push();
  bool Pop();

  const std::string &GetName(void) const {
    return name;
  }

  template <class T>
  inline T *Clone(const T &t) {
    void *p = Malloc(sizeof(T));
    p = new (p) T(t);  // Call clone constructor
    return static_cast<T *>(p);
  }

  // New templates
  template <class T, typename... Arguments>
  inline T *New(Arguments... args) {
    void *p = Malloc(sizeof(T));
    ASSERT(p != nullptr, "null ptr check");
    p = new (p) T(args...);  // Call constructor
    return static_cast<T *>(p);
  }

  // New Array template
  template <class T>
  inline T *NewArray(unsigned int num) {
    void *p = Malloc(sizeof(T) * num);
    p = new (p) T[num];
    return static_cast<T *>(p);
  }

#define BitsAlign(size) (((size) + 7) & (~0U << 3))
#define MemBlockFirstPtr(x) static_cast<void *>((reinterpret_cast<char *>(x)) + BitsAlign(sizeof(MemPoolCtrler::MemBlock)))
 private:                                     // constants
  static const size_t kMinBlockSize = 0x800;  // Minimum BlockSize is 2K
  static const size_t kMemBlockOverhead = (BitsAlign(sizeof(MemPoolCtrler::MemBlock)));

  MemPoolCtrler::MemBlock *GetLargeMemBlock(size_t size);  // Raw allocate large memory block
  MemPoolCtrler::MemBlock *GetMemBlock(size_t size);

  MemPoolCtrler *ctrler;  // Hookup controller object
  std::string name;       // Name of the memory pool

  // Save the memory block stack
  std::stack<MemPoolCtrler::MemBlock *> mem_block_stack;
  std::stack<MemPoolCtrler::MemBlock *> large_mem_block_stack;

  // Save mem_block and large_mem_block pointers when push()
  std::stack<std::pair<MemPoolCtrler::MemBlock *, MemPoolCtrler::MemBlock *>> marker_stack;

#ifdef MP_DEBUG
  bool isValid(void);
  bool frozen;
#endif
};

extern MemPoolCtrler mempoolctrler;

}  // namespace maple
#endif
