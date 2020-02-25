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

/*
 * File:   mempool_allocator.h
 * Author: Shawn
 *
 * Created on March 30, 2015, 9:57 AM
 */

#ifndef MPALLOCATOR_H
#define MPALLOCATOR_H
#include <stddef.h>
#include <limits>
#include <vector>
#include <deque>
#include <queue>
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <forward_list>
#include "mempool.h"

namespace maple {

template <typename T>
class MapleAllocatorAdapter;

class MapleAllocator {
 public:
  MemPool *mp;

 public:
  explicit MapleAllocator(MemPool *m) : mp(m) {}

  virtual ~MapleAllocator() {}

  // Get adapter for use in STL containers. See arena_containers.h .
  MapleAllocatorAdapter<void> Adapter();
  void *Alloc(size_t bytes) {
    return (mp ? mp->Malloc(bytes) : nullptr);
  }

  template <typename T>
  T *AllocArray(size_t length) {
    return static_cast<T *>(Alloc(length * sizeof(T)));
  }

  inline MemPool *GetMemPool() {
    return mp;
  }

  inline void SetMemPool(MemPool *m) {
    mp = m;
  }

 private:
  template <typename U>
  friend class MapleAllocatorAdapter;
};  // MapleAllocator

template <typename T>
class MapleAllocatorAdapter;

template <typename T>
using MapleQueue = std::deque<T, MapleAllocatorAdapter<T>>;

template <typename T>
using MapleVector = std::vector<T, MapleAllocatorAdapter<T>>;

template <typename T>
class MapleStack : public MapleVector<T> {
 public:
  MapleStack(MapleAllocatorAdapter<T> adapter) : MapleVector<T>(adapter) {}

  ~MapleStack() {}

  void push(T x) {
    this->push_back(x);
  }

  void pop() {
    this->pop_back();
  }

  T top() {
    return this->back();
  }

  void clear() {
    this->resize(0);
  }
};

template <typename T>
using MapleList = std::list<T, MapleAllocatorAdapter<T>>;

template <typename T>
using MapleForwardList = std::forward_list<T, MapleAllocatorAdapter<T>>;

template <typename T, typename Comparator = std::less<T>>
using MapleSet = std::set<T, Comparator, MapleAllocatorAdapter<T>>;

template <typename T, typename Comparator = std::less<T>>
using MapleMultiSet = std::multiset<T, Comparator, MapleAllocatorAdapter<T>>;

template <typename T, typename Hash = std::hash<T>, typename Equal = std::equal_to<T>>
using MapleUnorderedSet = std::unordered_set<T, Hash, Equal, MapleAllocatorAdapter<T>>;

template <typename T, typename Hash = std::hash<T>, typename Equal = std::equal_to<T>>
using MapleUnorderedMultiSet = std::unordered_multiset<T, Hash, Equal, MapleAllocatorAdapter<T>>;

template <typename K, typename V, typename Comparator = std::less<K>>
using MapleMap = std::map<K, V, Comparator, MapleAllocatorAdapter<std::pair<const K, V>>>;

template <typename K, typename V, typename Comparator = std::less<K>>
using MapleMultiMap = std::multimap<K, V, Comparator, MapleAllocatorAdapter<std::pair<const K, V>>>;

template <typename K, typename V, typename Hash = std::hash<K>, typename Equal = std::equal_to<K>>
using MapleUnorderedMap = std::unordered_map<K, V, Hash, Equal, MapleAllocatorAdapter<std::pair<const K, V>>>;

template <typename K, typename V, typename Hash = std::hash<K>, typename Equal = std::equal_to<K>>
using MapleUnorderedMultiMap = std::unordered_multimap<K, V, Hash, Equal, MapleAllocatorAdapter<std::pair<const K, V>>>;

// Implementation details below.
template <>
class MapleAllocatorAdapter<void> {
 public:
  typedef void value_type;
  typedef void *pointer;
  typedef const void *const_pointer;

  template <typename U>
  struct rebind {
    typedef MapleAllocatorAdapter<U> other;
  };

  explicit MapleAllocatorAdapter(MapleAllocator *mapleAllocator) : maple_allocator_(mapleAllocator) {}

  template <typename U>
  MapleAllocatorAdapter(const MapleAllocatorAdapter<U> &other) : maple_allocator_(other.maple_allocator_) {}

  MapleAllocatorAdapter(const MapleAllocatorAdapter &other) = default;
  MapleAllocatorAdapter &operator=(const MapleAllocatorAdapter &other) = default;
  ~MapleAllocatorAdapter() = default;

 private:
  MapleAllocator *maple_allocator_;

  template <typename U>
  friend class MapleAllocatorAdapter;
};

template <typename T>
class MapleAllocatorAdapter {
 public:
  typedef T value_type;
  typedef T *pointer;
  typedef T &reference;
  typedef const T *const_pointer;
  typedef const T &const_reference;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  template <typename U>
  struct rebind {
    typedef MapleAllocatorAdapter<U> other;
  };

  explicit MapleAllocatorAdapter(MapleAllocator *mapleAllocator) : maple_allocator_(mapleAllocator) {}

  template <typename U>
  MapleAllocatorAdapter(const MapleAllocatorAdapter<U> &other) : maple_allocator_(other.maple_allocator_) {}

  MapleAllocatorAdapter(const MapleAllocatorAdapter &other) = default;

  MapleAllocatorAdapter &operator=(const MapleAllocatorAdapter &other) = default;

  ~MapleAllocatorAdapter() = default;

  size_type max_size() const {
    return static_cast<size_type>(-1) / sizeof(T);
  }

  pointer address(reference x) const {
    return &x;
  }

  const_pointer address(const_reference x) const {
    return &x;
  }

  pointer allocate(size_type n, MapleAllocatorAdapter<void>::pointer hint = nullptr) {
    return reinterpret_cast<T *>(maple_allocator_->Alloc(n * sizeof(T)));
  }

  void deallocate(pointer p, size_type n) {}

  void construct(const pointer p, const_reference val) {
    new (static_cast<void *>(p)) value_type(val);
  }

  void destroy(const pointer p) {
    p->~value_type();
  }

 private:
  MapleAllocator *maple_allocator_;

  template <typename U>
  friend class MapleAllocatorAdapter;

  template <typename U>
  friend bool operator==(const MapleAllocatorAdapter<U> &lhs, const MapleAllocatorAdapter<U> &rhs);
};

template <typename T>
inline bool operator==(const MapleAllocatorAdapter<T> &lhs, const MapleAllocatorAdapter<T> &rhs) {
  return lhs.maple_allocator_ == rhs.maple_allocator_;
}

template <typename T>
inline bool operator!=(const MapleAllocatorAdapter<T> &lhs, const MapleAllocatorAdapter<T> &rhs) {
  return !(lhs == rhs);
}

inline MapleAllocatorAdapter<void> MapleAllocator::Adapter() {
  return MapleAllocatorAdapter<void>(this);
}

}  // namespace maple

#endif /* MPALLOCATOR_H */
