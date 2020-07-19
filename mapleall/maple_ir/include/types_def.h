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

#ifndef MAPLE_IR_INCLUDE_TYPES_DEF_H
#define MAPLE_IR_INCLUDE_TYPES_DEF_H

// NOTE: Since we already committed to -std=c++0x, we should eventually use the
// standard definitions in the <cstdint> and <limits> headers rather than
// reinventing our own primitive types.
#include <cstdint>
#include <cstddef>

namespace maple {

// Let's keep the following definitions so that existing code will continue to work.
#define MAXINT64 INT64_MAX
#define MININT64 INT64_MIN
#define MAXINT32 INT32_MAX
#define MININT32 INT32_MIN
#define MAXINT16 INT16_MAX
#define MININT16 INT16_MIN
#define MAXINT8  INT8_MAX
#define MININT8  INT8_MIN
#define MAXUINT32 UINT32_MAX
#define MAXUINT16 UINT16_MAX
#define MAXUINT8 UINT8_MAX
using int8 = std::int8_t;
using int16 = std::int16_t;
using int32 = std::int32_t;
using int64 = std::int64_t;
using uint8 = std::uint8_t;
using uint16 = std::uint16_t;
using uint32 = std::uint32_t;
using uint64 = std::uint64_t;
class StIdx {  // scope nesting level + symbol table index
 private:
  uint32 fullIdx;  // high level 24 bits for idx, low order 8 bits for scope

 public:
  StIdx(): fullIdx(0) {}

  StIdx(uint32 level, uint32 i) {
    fullIdx = (i << 8) | (level & 0xffu);
  }

  ~StIdx() {}

  uint32 Idx() const {
    return fullIdx >> 8;
  }

  void SetIdx(uint32 idx) {
    fullIdx = (idx << 8) | (fullIdx & 0xffu);
  }

  uint32 Scope() const {
    return fullIdx & 0xffu;
  }

  void SetScope(uint32 level) {
    uint32 *pFullIdx = &fullIdx;
    *((uint8 *)pFullIdx) = (uint8)level;
  }

  uint32 FullIdx() const {
    return fullIdx;
  }

  void SetFullIdx(uint32 idx) {
    fullIdx = idx;
  }

  bool Islocal() const {
    return (fullIdx & 0xffu) > 1;
  }

  bool IsGlobal() const {
    return (fullIdx & 0xffu) == 1;
  }

  bool operator==(const StIdx &x) const {
    return fullIdx == x.fullIdx;
  }

  bool operator!=(const StIdx &x) const {
    return fullIdx != x.fullIdx;
  }

  bool operator<(const StIdx &x) const {
    return fullIdx < x.fullIdx;
  }
};

using LabelIdx = uint32;
using LabelIDOrder = uint32;
using PUIdx = uint32;
using PregIdx = int32;
using PregIdx16 = int16;
using ExprIdx = int32;
using FieldID = int32;
class TyIdx {  // global type table index
 public:
  TyIdx() : idx(0) {}

  explicit TyIdx(uint32 i) : idx(i) {}

  TyIdx(const TyIdx &x) {
    idx = x.idx;
  }

  ~TyIdx() {}

  TyIdx &operator=(const TyIdx &x) {
    if (&x == this) {
      return *this;
    }
    idx = x.idx;
    return *this;
  }

  bool operator==(const TyIdx &x) const {
    return idx == x.idx;
  }

  bool operator!=(const TyIdx &x) const {
    return idx != x.idx;
  }

  bool operator==(const uint32 id) const {
    return idx == id;
  }

  bool operator!=(const uint32 id) const {
    return idx != id;
  }

  bool operator<(const TyIdx &x) const {
    return idx < x.idx;
  }

  uint32 GetIdx() const {
    return idx;
  }

  void SetIdx(uint32 i) {
    idx = i;
  }

 private:
  uint32 idx;
};

const TyIdx kInitTyIdx = TyIdx(0);
const TyIdx kNoneTyIdx = TyIdx(UINT32_MAX);
class GStrIdx {  // global string table index
 public:
  GStrIdx() {
    idx = 0;
  }

  explicit GStrIdx(uint32 i) : idx(i) {}

  ~GStrIdx() {}

  bool operator==(const GStrIdx &x) const {
    return idx == x.idx;
  }

  bool operator!=(const GStrIdx &x) const {
    return idx != x.idx;
  }

  bool operator==(const uint32 id) const {
    return idx == id;
  }

  bool operator!=(const uint32 id) const {
    return idx != id;
  }

  bool operator<(const GStrIdx &x) const {
    return idx < x.idx;
  }

  uint32 GetIdx() const {
    return idx;
  }

  void SetIdx(uint32 i) {
    idx = i;
  }

 private:
  uint32 idx;
};

class UStrIdx {  // user string table index (from the conststr opcode)
 public:
  UStrIdx() : idx(0) {}

  explicit UStrIdx(uint32 i) : idx(i) {}

  ~UStrIdx() {}

  bool operator==(const UStrIdx &x) const {
    return idx == x.idx;
  }

  bool operator!=(const UStrIdx &x) const {
    return idx != x.idx;
  }

  bool operator==(const uint32 id) const {
    return idx == id;
  }

  bool operator!=(const uint32 id) const {
    return idx != id;
  }

  bool operator<(const UStrIdx &x) const {
    return idx < x.idx;
  }

  uint32 GetIdx() const {
    return idx;
  }

  void SetIdx(uint32 i) {
    idx = i;
  }

 private:
  uint32 idx;
};

class U16StrIdx {  // user string table index (from the conststr opcode)
 public:
  U16StrIdx() : idx(0) {}

  explicit U16StrIdx(uint32 i) : idx(i) {}

  ~U16StrIdx() {}

  bool operator==(const U16StrIdx &x) const {
    return idx == x.idx;
  }

  bool operator!=(const U16StrIdx &x) const {
    return idx != x.idx;
  }

  bool operator==(const uint32 id) const {
    return idx == id;
  }

  bool operator!=(const uint32 id) const {
    return idx != id;
  }

  bool operator<(const U16StrIdx &x) const {
    return idx < x.idx;
  }

  uint32 GetIdx() const {
    return idx;
  }

  void SetIdx(uint32 i) {
    idx = i;
  }

 private:
  uint32 idx;
};

}  // namespace maple
#endif
