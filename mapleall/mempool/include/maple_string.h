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

/*
 * File:   maple_string.h
 * Author: shawn
 *
 * Created on April 15, 2015, 12:32 PM
 */

#ifndef MAPLESTRING_H
#define MAPLESTRING_H
#include "mempool.h"
#include <cstring>
#include "../../../huawei_secure_c/include/securec.h"
#include "../../maple_util/include/mpl_logging.h"

namespace maple {

class MapleString {
 public:
  char *data;
  MapleString() : data(nullptr), _mp(nullptr), data_len(0) {}

  explicit MapleString(MemPool *mp) : data(nullptr), _mp(mp), data_len(0) {}

  MapleString(const char *str, MemPool *mp);
  MapleString(const char *str, size_t size, MemPool *mp);  // copyin
  MapleString(unsigned int size, MemPool *mp);
  MapleString(const MapleString &str, MemPool *mp);
  MapleString(const std::string &str, MemPool *mp);
  ~MapleString() {
    // delete[] data;
  }

  inline void setMemPool(MemPool *mp) {
    _mp = mp;
  }

  inline unsigned int length() const {
    return data_len;
  }

  inline operator char *() {
    return data;
  }

  inline operator const char *() const {
    return data;
  }

  inline char *c_str() {
    if (data_len <= 0 && data == nullptr) {
      return nullptr;
    }
    return data;
  }

  const char *c_str() const {
    if (data_len <= 0 && data == nullptr) {
      return nullptr;
    }

    return data;
  }

  inline char &operator[](const int x) {
    return data[x];
  }

  inline const char &operator[](const int x) const {
    return data[x];
  }

  MapleString &operator=(const char c) {
    data = static_cast<char *>(_mp->Malloc(2 * sizeof(char)));
    data[0] = c;
    data[1] = '\0';
    data_len = 1;
    return *this;
  }

  MapleString &operator=(const char *str) {
    if (!str)
    // If str is nullptr, do nothing
    {
      return *this;
    }
    size_t size = strlen(str);
    // if data is null, old_size =0, elese +1
    size_t oldSize = (data == nullptr) ? 0 : (data_len + 1);
    if (oldSize < (1 + size)) {
      data = static_cast<char *>(_mp->Realloc(data, oldSize * sizeof(char), (1 + size) * sizeof(char)));
    }
    ASSERT(data != nullptr, "null ptr check ");
    if (size == 0) {
      data[0] = '\0';
      return *this;
    }
    errno_t eNum = memcpy_s(data, (size_t)(size + 1), str, (size_t)size);
    if (eNum){
      ASSERT(false, "memcpy_s failed");
    }
    data_len = size;
    ASSERT(data != nullptr, "null ptr check ");
    data[data_len] = '\0';
    return *this;
  }

  MapleString &operator=(const std::string &str) {
    size_t size = str.length();
    size_t oldSize = (data == nullptr) ? 0 : (data_len + 1);
    if (oldSize < (1 + size)) {
      data = static_cast<char *>(_mp->Realloc(data, oldSize * sizeof(char), (1 + size) * sizeof(char)));
    }
    ASSERT(data != nullptr, "null ptr check ");
    if (size == 0) {
      data[0] = '\0';
      return *this;
    }
    errno_t eNum = memcpy_s(data, size, str.data(), (size_t)size);
    if (eNum){
      ASSERT(false, "memcpy_s failed");
    }
    data_len = size;
    ASSERT(data != nullptr, "null ptr check");
    data[data_len] = '\0';
    return *this;
  }

  MapleString(const MapleString &str) {
    _mp = str._mp;
    data = static_cast<char *>(_mp->Malloc((str.data_len + 1) * sizeof(char)));
    errno_t eNum = 0;
    if (str.data_len > 0) {
      eNum = memcpy_s(data, static_cast<size_t>(str.data_len), str.data, static_cast<size_t>(str.data_len));
    }
    if (eNum) {
      ASSERT(false, "memcpy_s failed");
    }
    data_len = str.data_len;
    data[data_len] = '\0';
  }

  MapleString &operator=(const MapleString &str) {
    if (&str == this) {
        return *this;
    }
    size_t size = str.data_len;
    int oldSize = (data == nullptr) ? 0 : (data_len + 1);
    data = static_cast<char *>(_mp->Realloc(data, oldSize * sizeof(char), (1 + size) * sizeof(char)));
    ASSERT(data, "null ptr check");
    if (size == 0) {
      data[0] = '\0';
      return *this;
    }
    errno_t eNum = memcpy_s(data, size, str.data, size);
    if (eNum){
      ASSERT(false, "memcpy_s failed");
    }
    data_len = size;
    data[data_len] = '\0';
    return *this;
  }

  MapleString &operator+=(const char c) {
    int oldSize = (data == nullptr) ? 0 : (data_len + 1);
    data = static_cast<char *>(_mp->Realloc(data, oldSize * sizeof(char), (data_len + 1 + 1) * sizeof(char)));
    data_len++;
    data[data_len - 1] = c;
    data[data_len] = '\0';

    return *this;
  }

  MapleString &operator+=(const char *str) {
    if (!str)
    // If str is nullptr, do nothing
    {
      return *this;
    }
    size_t size = strlen(str);
    int oldSize = (data == nullptr) ? 0 : (data_len + 1);
    data = static_cast<char *>(_mp->Realloc(data, oldSize * sizeof(char), (data_len + size + 1) * sizeof(char)));
    ASSERT(data, "null ptr check");
    errno_t eNum = memcpy_s(data + data_len, size, str, size);
    if (eNum){
      ASSERT(false, "memcpy_s failed");
    }
    data_len += size;
    data[data_len] = '\0';
    return *this;
  }

  MapleString &operator+=(const MapleString &str) {
    int oldSize = (data == nullptr) ? 0 : (data_len + 1);
    data =
      static_cast<char *>(_mp->Realloc(data, oldSize * sizeof(char), (data_len + str.data_len + 1) * sizeof(char)));
    errno_t eNum = memcpy_s(data + data_len, (size_t)str.data_len, str.data, (size_t)str.data_len);
    if (eNum){
      ASSERT(false, "memcpy_s failed");
    }
    data_len += str.data_len;
    data[data_len] = '\0';
    return *this;
  }

  MapleString &operator+=(const std::string &str) {
    size_t size = str.length();
    int oldSize = (data == nullptr) ? 0 : (data_len + 1);
    data = static_cast<char *>(_mp->Realloc(data, oldSize * sizeof(char), (data_len + size + 1) * sizeof(char)));
    ASSERT(data, " null ptr check");
    errno_t eNum = memcpy_s(data + data_len, (size_t)size, str.data(), (size_t)size);
    if (eNum){
      ASSERT(false, "memcpy_s failed");
    }
    data_len += size;
    data[data_len] = '\0';
    return *this;
  }

  void clear();
  inline bool empty() const {
    if (data_len <= 0 || data == nullptr) {
      return true;
    } else {
      return false;
    }
  }

  MapleString &push_back(const char c);
  MapleString &append(const MapleString &str);
  MapleString &append(const MapleString &str, unsigned int subpos, unsigned int sublen);
  MapleString &append(const char *s);
  MapleString &append(const char *s, unsigned int n);
  MapleString &append(unsigned int n, char c);
  MapleString &append(const std::string &str);

  size_t find(const MapleString &str, unsigned int pos = 0) const;
  size_t find(const char *s, unsigned int pos = 0) const;
  size_t find(const char *s, unsigned int pos, unsigned int n) const;
  size_t find(char c, unsigned int pos = 0) const;
  size_t find_last_of(const char *, unsigned int pos = 0) const;

  MapleString substr(unsigned int pos, unsigned int len) const;
  MapleString &insert(unsigned int pos, const MapleString &str);
  MapleString &insert(unsigned int pos, const MapleString &str, unsigned int subpos, unsigned int sublen);

  MapleString &insert(unsigned int pos, const char *s);
  MapleString &insert(unsigned int pos, const char *s, unsigned int n);
  MapleString &insert(unsigned int pos, unsigned int n, char c);

  MapleString &assign(const MapleString &str);
  MapleString &assign(const MapleString &str, unsigned int subpos, unsigned int sublen);
  MapleString &assign(const char *s);
  MapleString &assign(const char *s, unsigned int n);
  MapleString &assign(unsigned int n, char c);

 private:
  MemPool *_mp;

  unsigned int data_len;

  friend bool operator==(const MapleString &, const MapleString &);
  friend bool operator==(const MapleString &, const char *);
  friend bool operator==(const char *, const MapleString &);
  friend bool operator<(const MapleString &str1, const MapleString &str2);
};
// global operators
bool operator==(const MapleString &str1, const MapleString &str2);
bool operator==(const MapleString &str1, const char *str2);
bool operator==(const char *str1, const MapleString &str2);

bool operator!=(const MapleString &str1, const MapleString &str2);
bool operator!=(const MapleString &str1, const char *str2);
bool operator!=(const char *str1, const MapleString &str2);
bool operator<(const MapleString &str1, const MapleString &str2);

}  // namespace maple

#endif /* MAPLESTRING_H */
