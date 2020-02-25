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
 * File:   maple_string.h
 * Author: shawn
 *
 * Created on April 15, 2015, 12:32 PM
 */
#include "../include/maple_string.h"
#include "securec.h"

namespace maple {

MapleString::MapleString(const char *str, size_t size, MemPool *mp) {
  MIR_ASSERT(mp && "Pointer to Mempool can not be nullptr");
  _mp = mp;
  if (!str) {
    data = nullptr;
    data_len = 0;
  } else {
    MIR_ASSERT(_mp && "Pointer to Mempool can not be nullptr");
    data = static_cast<char *>(_mp->Malloc((size + 1) * sizeof(char)));
    errno_t eNum = 0;
    if (size > 0) {
      eNum = memcpy_s(data, (size_t)size, str, (size_t)size);
    }
    if (eNum) {
      ASSERT(false, "memset_s failed");
    }
    data_len = size;
    data[data_len] = '\0';
  }
}

MapleString::MapleString(const char *str, MemPool *mp) {
  MIR_ASSERT(mp && "Pointer to Mempool can not be nullptr");
  _mp = mp;
  if (!str) {
    data = nullptr;
    data_len = 0;
  } else {
    size_t size = strlen(str);
    data = static_cast<char *>(_mp->Malloc((size + 1) * sizeof(char)));
    errno_t eNum = 0;
    if (size > 0) {
      eNum = memcpy_s(data, size, str, size);
    }
    if (eNum) {
      ASSERT(false, "memcpy_s failed");
    }
    data_len = size;
    data[data_len] = '\0';
  }
}

MapleString::MapleString(unsigned int size, MemPool *mp) {
  MIR_ASSERT(mp && "Pointer to Mempool can not be nullptr");
  _mp = mp;
  data = static_cast<char *>(_mp->Malloc((size + 1) * sizeof(char)));
  data_len = size;
  data[data_len] = '\0';
}

MapleString::MapleString(const MapleString &str, MemPool *mp) {
  MIR_ASSERT(mp && "Pointer to Mempool can not be nullptr");
  _mp = mp;
  data = static_cast<char *>(_mp->Malloc((str.data_len + 1) * sizeof(char)));
  errno_t eNum = 0;
  if (str.data_len > 0) {
    eNum = memcpy_s(data, (size_t)str.data_len, str.data, (size_t)str.data_len);
  }
  if (eNum) {
    ASSERT(false, "memcpy_s failed");
  }
  data_len = str.data_len;
  data[data_len] = '\0';
}

MapleString::MapleString(const std::string &str, MemPool *mp) {
  MIR_ASSERT(mp && "Pointer to Mempool can not be nullptr");
  _mp = mp;
  size_t size = str.length();

  data = static_cast<char *>(_mp->Malloc((1 + size) * sizeof(char)));
  errno_t eNum = 0;
  if (size > 0) {
    eNum = memcpy_s(data, size, str.data(), size);
  }
  if (eNum) {
    FATAL(kLncFatal, "memcpy_s failed");
  }
  data_len = size;
  data[data_len] = '\0';
}

void MapleString::clear() {
  data = nullptr;
  data_len = 0;
}

size_t MapleString::find(const MapleString &str, unsigned int pos) const {
  if ((data_len - pos) < str.data_len) {
    return std::string::npos;
  }
  for (unsigned int i = pos; i < (data_len - str.data_len + 1); i++) {
    if (data[i] == str[0]) {
      unsigned int j;
      for (j = 0; j < str.data_len; ++j) {
        if (data[i + j] == str[j]) {
          continue;
        } else {
          break;
        }
      }
      if (j == str.data_len) {
        return i;
      }
    }
  }
  return std::string::npos;
}

size_t MapleString::find(const char *str, unsigned int pos) const {
  if (!str) {
    return std::string::npos;
  }
  unsigned int strLen = strlen(str);
  if ((data_len - pos) < strLen) {
    return std::string::npos;
  }
  for (unsigned int i = pos; i < (data_len - strLen + 1); i++) {
    if (data[i] == str[0]) {
      unsigned int j;
      for (j = 0; j < strLen; ++j) {
        if (data[i + j] == str[j]) {
          continue;
        } else {
          break;
        }
      }
      if (j == strLen) {
        return i;
      }
    }
  }
  return std::string::npos;
}

size_t MapleString::find_last_of(const char *str, unsigned int pos) const {
  if (!str) {
    return std::string::npos;
  }
  unsigned int strLen = strlen(str);
  if ((data_len - pos) < strLen) {
    return std::string::npos;
  }
  for (unsigned int i = (data_len - strLen); i >= pos; i--) {
    if (data[i] == str[0]) {
      unsigned int j;
      for (j = 0; j < strLen; ++j) {
        if (data[i + j] == str[j]) {
          continue;
        } else {
          break;
        }
      }
      if (j == strLen) {
        return i;
      }
    }
  }
  return std::string::npos;
}

size_t MapleString::find(const char *str, unsigned int pos, unsigned int n) const {
  if (!str) {
    return std::string::npos;
  }
  if ((data_len - pos) < n) {
    return std::string::npos;
  }
  for (unsigned int i = pos; i < (data_len - n + 1); i++) {
    if (data[i] == str[0]) {
      unsigned int j;
      for (j = 0; j < n; ++j) {
        if (data[i + j] == str[j]) {
          continue;
        } else {
          break;
        }
      }
      if (j == n) {
        return i;
      }
    }
  }
  return std::string::npos;
}

size_t MapleString::find(char c, unsigned int pos) const {
  if (data_len == 0 || pos >= data_len) {
    return std::string::npos;
  }
  unsigned int i;
  for (i = pos; i < data_len; i++) {
    if (data[i] == c) {
      return i;
    }
  }
  return std::string::npos;
}

MapleString MapleString::substr(unsigned int pos, unsigned int len) const {
  if (len == 0) {
    MIR_FATAL("Error: MapleString substr len is 0");
  }

  if (pos > data_len) {
    MIR_FATAL("Error: MapleString substr pos is out of boundary");
  }

  len = (len + pos) > data_len ? (data_len - pos) : len;
  MapleString newStr(_mp);
  newStr.data = static_cast<char *>(newStr._mp->Malloc((1 + len) * sizeof(char)));
  for (unsigned int i = 0; i < len; i++) {
    newStr[i] = this->data[i + pos];
  }
  newStr.data_len = len;
  newStr.data[newStr.data_len] = '\0';
  return newStr;
}

MapleString &MapleString::insert(unsigned int pos, const MapleString &str) {
  if (pos > data_len || str.data_len == 0) {
    return *this;
  }
  data = static_cast<char *>(
    _mp->Realloc(data, (1 + data_len) * sizeof(char), (1 + data_len + str.data_len) * sizeof(char)));
  ASSERT(data, "null ptr check ");
  MapleString temp(_mp);
  if (data_len - pos) {
    temp = this->substr(pos, data_len - pos);
  } else {
    temp = "";
  }

  data_len += str.data_len;

  unsigned int i;
  for (i = 0; i < str.data_len; i++) {
    data[pos + i] = str.data[i];
  }

  ASSERT(temp != nullptr, "temp null ptr check");
  for (unsigned int j = 0; j < temp.data_len; j++) {
    data[pos + str.data_len + j] = temp.data[j];
  }
  data[data_len] = '\0';
  return *this;
}

MapleString &MapleString::insert(unsigned int pos, const MapleString &str, unsigned int subpos, unsigned int sublen) {
  MapleString subStr = str.substr(subpos, sublen);
  this->insert(pos, subStr);
  return *this;
}

MapleString &MapleString::insert(unsigned int pos, const char *s) {
  if (!s) {
    return *this;
  }
  unsigned int sLen = strlen(s);
  if (pos > data_len || sLen == 0) {
    return *this;
  }

  MapleString subStr(_mp);
  subStr = s;
  this->insert(pos, subStr);
  return *this;
}

MapleString &MapleString::insert(unsigned int pos, const char *s, unsigned int n) {
  if (!s) {
    return *this;
  }
  unsigned int sLen = strlen(s);
  if (pos > data_len || sLen == 0) {
    return *this;
  }
  n = n > sLen ? sLen : n;

  MapleString subStr(_mp);
  subStr = s;
  subStr = subStr.substr(0, n);
  this->insert(pos, subStr);

  return *this;
}

MapleString &MapleString::insert(unsigned int pos, unsigned int n, char c) {
  if (pos > data_len) {
    return *this;
  }
  MapleString subStr(n, _mp);
  for (unsigned int i = 0; i < n; i++) {
    subStr[i] = c;
  }
  this->insert(pos, subStr);
  return *this;
}

MapleString &MapleString::push_back(const char c) {
  this->append(1, c);
  return *this;
}

MapleString &MapleString::append(const MapleString &str) {
  if (str.empty()) {
    return *this;
  }
  this->insert(data_len, str);
  return *this;
}

MapleString &MapleString::append(const std::string &str) {
  if (str.length() <= 0) {
    return *this;
  }
  this->insert(data_len, str.c_str());
  return *this;
}

MapleString &MapleString::append(const MapleString &str, unsigned int subpos, unsigned int sublen) {
  this->append(str.substr(subpos, sublen));
  return *this;
}

MapleString &MapleString::append(const char *s) {
  if (s == nullptr) {
    return *this;
  }
  MapleString subStr(_mp);
  subStr = s;
  this->append(subStr);
  return *this;
}

MapleString &MapleString::append(const char *s, unsigned int n) {
  if (s == nullptr) {
    return *this;
  }
  MapleString subStr(_mp);
  subStr = s;
  this->append(subStr, 0, n);
  return *this;
}

MapleString &MapleString::append(unsigned int n, char c) {
  MapleString subStr(n, _mp);
  for (unsigned int i = 0; i < n; i++) {
    subStr[i] = c;
  }
  this->append(subStr);
  return *this;
}

MapleString &MapleString::assign(const MapleString &str) {
  *this = str;
  return *this;
}

MapleString &MapleString::assign(const MapleString &str, unsigned int subpos, unsigned int sublen) {
  *this = str.substr(subpos, sublen);
  return *this;
}

MapleString &MapleString::assign(const char *s) {
  *this = s;
  return *this;
}

MapleString &MapleString::assign(const char *s, unsigned int n) {
  MapleString subStr(_mp);
  subStr = s;
  subStr = subStr.substr(0, n);
  *this = subStr;
  return *this;
}

MapleString &MapleString::assign(unsigned int n, char c) {
  MapleString subStr(n, _mp);
  for (unsigned int i = 0; i < n; i++) {
    subStr[i] = c;
  }
  this->assign(subStr);
  return *this;
}

// global operators
bool operator==(const MapleString &str1, const MapleString &str2) {
  if (str1.data_len != str2.data_len) {
    return false;
  }
  char *tmp1 = str1.data;
  char *tmp2 = str2.data;
  while (*tmp1 != 0) {
    if (*tmp1 != *tmp2) {
      return false;
    }
    ++tmp1;
    ++tmp2;
  }
  return true;
}

bool operator==(const MapleString &str1, const char *str2) {
  if (!str2) {
    return false;  // Should we return str1.data_len==0 ?
  }
  unsigned size = strlen(str2);
  if (str1.data_len != size) {
    return false;
  }
  char *tmp = str1.data;
  while (*tmp != 0) {
    if (*tmp != *str2) {
      return false;
    }
    ++tmp;
    ++str2;
  }
  return true;
}

bool operator==(const char *str1, const MapleString &str2) {
  unsigned size = strlen(str1);
  if (str2.data_len != size) {
    return false;
  }
  char *tmp = str2.data;
  while (*tmp != 0) {
    if (*tmp != *str1) {
      return false;
    }
    ++tmp;
    ++str1;
  }
  return true;
}

bool operator!=(const MapleString &str1, const MapleString &str2) {
  return !(str1 == str2);
}

bool operator!=(const MapleString &str1, const char *str2) {
  return !(str1 == str2);
}

bool operator!=(const char *str1, const MapleString &str2) {
  return !(str1 == str2);
}

bool operator<(const MapleString &str1, const MapleString &str2) {
  ASSERT(str1.c_str() && str2.c_str(), "null ptr check");
  return (strcmp(str1.c_str(), str2.c_str()) < 0);
}

}  // namespace maple
