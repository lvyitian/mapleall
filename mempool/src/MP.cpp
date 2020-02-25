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

#include "../include/mempool.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include "securec.h"
#include "mpl_logging.h"
namespace maple {

MemPoolCtrler mempoolctrler;

/*******************************************************************************
   Name:         ~MemPoolCtrler()
   Function:     Destructor, free all allocated memory pool and blocks
*******************************************************************************/
MemPoolCtrler::~MemPoolCtrler() {
  // Delete Memory Pool
  std::set<MemPool *>::iterator itr;
  for (itr = mem_pools.begin(); itr != mem_pools.end(); itr++) {
    MemPool *mp = *itr;
    if (mp) {
#ifdef MP_DEBUG
      std::cout << "MEMPOOL: Left mempool " << mp->name << std::endl;
#endif
      delete (mp);
    }
  }

  // Delete all free_memory_block
  std::list<MemBlock *>::iterator itr2;
  for (itr2 = free_mem_blocks.begin(); itr2 != free_mem_blocks.end(); itr2++) {
    MemBlock *mb = *itr2;
    if (mb) {
      free(mb);
    }
  }
  for (auto itr3 = large_free_mem_blocks.begin(); itr3 != large_free_mem_blocks.end(); itr3++) {
    // std::vector<MemBlock*>::iterator itr4;
    for (auto itr4 = (*itr3).second.begin(); itr4 != (*itr3).second.end(); itr4++) {
      MemBlock *mb = *itr4;
      if (mb) {
        free(mb);
      }
    }
  }
}

/*******************************************************************************
   Name:         NewMemPool()
   Function:     Allocate a new memory pool and register it in controller
*******************************************************************************/
MemPool *MemPoolCtrler::NewMemPool(const char *name) {
  auto mp = new MemPool(this, name);
  if (mp == nullptr) {
    MIR_ERROR("ERROR: Can't allocate new memory pool");
    return nullptr;
  }
  mem_pools.insert(mp);
  return mp;
}

/********************************************************************************
   Name:         DeleteMemPool()
   Function:     Re-cycle all memory blocks allocated on a memory pool to free list
 ******************************************************************************/
void MemPoolCtrler::DeleteMemPool(MemPool *mp) {
#ifdef MP_DEBUG
  if (!mp->isValid()) {
    MIR_ERROR("MEMPOOL: Re-cycled wrong memory pool\n");
    return;
  }
#endif

  // Transfer memory blocks to ctrler->free_mem_blocks stack
  while (!mp->mem_block_stack.empty()) {
    MemBlock *p = mp->mem_block_stack.top();
    p->avail = p->orig_size;
    p->ptr = MemBlockFirstPtr(p);
    free_mem_blocks.push_back(p);
    mp->mem_block_stack.pop();
  }

  // Transfer large memory blocks to ctrler->free_mem_blocks stack
  while (!mp->large_mem_block_stack.empty()) {
    MemBlock *p = mp->large_mem_block_stack.top();
    p->avail = p->orig_size;
    p->ptr = MemBlockFirstPtr(p);
    unsigned int key = (p->avail / 0x800) + 1;
    if (large_free_mem_blocks.find(key) == large_free_mem_blocks.end()) {
      std::set<MemBlock *, MemBlockCmp> tmp;
      large_free_mem_blocks[key] = tmp;
    }
    large_free_mem_blocks[key].insert(p);
    mp->large_mem_block_stack.pop();
  }

  // Delete entry of this mempool in mem_pools and delete the mempool node
  mem_pools.erase(mp);
  delete mp;
}

MemPool::~MemPool() {
  while (!mem_block_stack.empty()) {
    free(mem_block_stack.top());
    mem_block_stack.pop();
  }
  while (!large_mem_block_stack.empty()) {
    free(large_mem_block_stack.top());
    large_mem_block_stack.pop();
  }
#ifdef MP_DEBUG
  std::cout << "MEMPOOL: Deleted " << name << std::endl;
#endif
}

/********************************************************************************
   Name:         Malloc()
   Function:     Return a pointer that points to size of memory from memory block
********************************************************************************/
void *MemPool::Malloc(size_t size) {
#ifdef MP_DEBUG
  // If controller is not set, or the memory pool is invalid
  if (!isValid()) {
    return nullptr;
  }

  if (_size > 0xfffffffU) {
    MIR_ERROR("ERROR: MemPool allocator cannot handle block size larger than 4GB\n");
  }
#endif

  void *result = nullptr;
  size_t alignedSize = BitsAlign(size);
  MemPoolCtrler::MemBlock *b = nullptr;
  // If size is smaller than 2K, fetch size of memory from the last memory block
  if (size <= kMinBlockSize) {
    // Get the top memory block from the top
    b = mem_block_stack.empty() ? nullptr : mem_block_stack.top();
    if (b == nullptr || (!marker_stack.empty() && b == marker_stack.top().first) ||  // the last operation was a push
        b->avail < size) {
      b = GetMemBlock(size);
    }
    // Return the pointer that points to the starting point + 8;
  } else {
    b = GetLargeMemBlock(size);
  }

  if (b == nullptr) {
    return nullptr;
  }

  result = static_cast<void *>(b->ptr);
  b->ptr = static_cast<void *>(static_cast<char *>(b->ptr) + size);

  // available size decrease
  int tmp = b->avail - size;
  if (tmp < 0) {
    MIR_ERROR("ERROR: Malloc error\n");
    return nullptr;
  }
  b->avail = tmp;
  return result;
}

/********************************************************************************
   Name:         Calloc()
   Function:     Malloc size of memory from memory pool, then set 0
********************************************************************************/
void *MemPool::Calloc(size_t size) {
#ifdef MP_DEBUG
  if (!isValid()) {
    return nullptr;
  }
#endif

  void *p = Malloc(BitsAlign(size));

  if (p == nullptr) {
    MIR_ERROR("ERROR: Calloc error\n");
    return nullptr;
  }
  errno_t eNum = memset_s(p, BitsAlign(size), 0, BitsAlign(size));
  if (eNum) {
    ASSERT(false, "memset_s failed");
  }
  return p;
}

/********************************************************************************
   Name:         Realloc
   Function:     Realloc new size of memory
********************************************************************************/
void *MemPool::Realloc(const void *ptr, size_t oldSize, size_t newSize) {
#ifdef MP_DEBUG
  if (!isValid()) {
    return nullptr;
  }
#endif

  void *result = nullptr;
  result = Malloc(newSize);

  if (result != nullptr) {
    size_t copySize = newSize > oldSize ? oldSize : newSize;
    if (copySize != 0 && ptr != nullptr) {
      errno_t eNum = memcpy_s(result, copySize, ptr, copySize);
      if (eNum) {
        FATAL(kLncFatal, "memcpy_s failed");
      }
    }
  } else {
    MIR_ERROR("Error Realloc\n");
  }
  return result;
}

/*******************************************************************************
   Name:         Push()
   Function:     Push current mem block pointer and large mem block pointer to
              marker_stack
*******************************************************************************/
void MemPool::Push() {
  // Get current memory block pointer
  MemPoolCtrler::MemBlock *mb = mem_block_stack.empty() ? nullptr : mem_block_stack.top();
  MemPoolCtrler::MemBlock *lmb = large_mem_block_stack.empty() ? nullptr : large_mem_block_stack.top();

#ifdef MP_DEBUG
  std::cout << "MEMPOOL: Push one memory block" << std::endl;
#endif

  marker_stack.push(std::pair<MemPoolCtrler::MemBlock *, MemPoolCtrler::MemBlock *>(mb, lmb));
  return;
}

/*******************************************************************************
   Name:        Pop()
   Function:    Recycle all memory blocks allocated since the last push.
*           Pop marker_stack by one level.
*           Return false if pop is unsuccessful due to marker_stack being empty
*******************************************************************************/

bool MemPool::Pop() {
#ifdef MP_DEBUG
  std::cout << "MEMPOOL: Pop one memory block" << std::endl;
#endif

  if (!marker_stack.empty()) {
    std::pair<MemPoolCtrler::MemBlock *, MemPoolCtrler::MemBlock *> p = marker_stack.top();

    MemPoolCtrler::MemBlock *mb = p.first;
    MemPoolCtrler::MemBlock *lmb = p.second;

    if (mb != nullptr) {
      while (mem_block_stack.top() != mb) {
        MemPoolCtrler::MemBlock *p1 = mem_block_stack.top();
        p1->avail = p1->orig_size;
        p1->ptr = MemBlockFirstPtr(p1);
        ctrler->free_mem_blocks.push_back(p1);
        mem_block_stack.pop();
      }
    }
    if (lmb != nullptr) {
      while (large_mem_block_stack.top() != lmb) {
        MemPoolCtrler::MemBlock *p2 = large_mem_block_stack.top();
        p2->avail = p2->orig_size;
        p2->ptr = MemBlockFirstPtr(p2);
        unsigned int key = (p2->avail / 0x800) + 1;
        if (ctrler->large_free_mem_blocks.find(key) == ctrler->large_free_mem_blocks.end()) {
          std::set<MemPoolCtrler::MemBlock *, MemPoolCtrler::MemBlockCmp> tmp;
          ctrler->large_free_mem_blocks[key] = tmp;
        }
        ctrler->large_free_mem_blocks[key].insert(p2);
        // ctrler->large_free_mem_blocks.push_back(p);
        large_mem_block_stack.pop();
      }
    }
    marker_stack.pop();
    return TRUE;
  }
  return FALSE;
}

/********************************************************************************
   Name:         GetMemBlock()
   Function:     Allocate a new memory block
********************************************************************************/
MemPoolCtrler::MemBlock *MemPool::GetMemBlock(size_t size) {
  MemPoolCtrler::MemBlock *block = nullptr;

  // Try to fetch one from free_memory_list
  if (!ctrler->free_mem_blocks.empty()) {
    block = ctrler->free_mem_blocks.front();
    ctrler->free_mem_blocks.erase(ctrler->free_mem_blocks.begin());
  } else {
    // Allocate a block of memory, 8K + MemBlock head size
    int total = kMinBlockSize + kMemBlockOverhead;
    block = static_cast<MemPoolCtrler::MemBlock *>(malloc(total));
    if (block == nullptr) {
      MIR_ERROR("ERROR: Allocate memory block failed\n");
      return nullptr;
    }
    // Available memory size is BlockSize
    block->avail = kMinBlockSize;
    block->orig_size = kMinBlockSize;
    // Set the pointer points to the first available byte
    block->ptr = MemBlockFirstPtr(block);
  }

  mem_block_stack.push(block);
  return block;
}

/********************************************************************************
   Name:         GetLargeMemBlock
   Function:     Allocate a large memory block when user allocates memory size> 2K
********************************************************************************/
MemPoolCtrler::MemBlock *MemPool::GetLargeMemBlock(size_t size) {
  MemPoolCtrler::MemBlock *block = nullptr;
  unsigned int key = (size / kMinBlockSize) + 1;
  if (ctrler->large_free_mem_blocks.find(key) != ctrler->large_free_mem_blocks.end() &&
      ctrler->large_free_mem_blocks[key].size()) {
    block = *(ctrler->large_free_mem_blocks[key].begin());
    if (block->orig_size >= size) {
      ctrler->large_free_mem_blocks[key].erase(ctrler->large_free_mem_blocks[key].begin());
    } else {
      block = nullptr;
    }
  }
  if (block == nullptr) {
    // Allocate a large memory block
    block = static_cast<MemPoolCtrler::MemBlock *>(malloc(size + kMemBlockOverhead));
    if (block == nullptr) {
      MIR_ERROR("ERROR: Fail to allocate large memory block\n");
      return nullptr;
    }
    block->orig_size = size;
    block->avail = size;
    block->ptr = MemBlockFirstPtr(block);
  } else {
    // Delete the block from free list
    // ctrler->large_free_mem_blocks.erase(itr);
  }


  large_mem_block_stack.push(block);
  return block;
}

#ifdef MP_DEBUG
/*******************************************************************************
   Name:         isValid()
   Function:     Whether the memory pool is valid
*******************************************************************************/
bool MemPool::isValid(void) {
  if (ctrler == nullptr) {
    std::cout << "ERROR: Memory pool is unmanaged";
    return FALSE;
  }
  if (frozen) {
    std::cout << "Operate on a frozen pool " << name << std::endl;
    return FALSE;
  }
  return TRUE;
}

#endif

}  // namespace maple
