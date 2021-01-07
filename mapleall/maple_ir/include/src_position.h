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

#ifndef MAPLE_IR_INCLUDE_SRC_POSITION_H
#define MAPLE_IR_INCLUDE_SRC_POSITION_H

namespace maple {

// to store source position information
class SrcPosition {
 private:
  union {
    struct {
      uint16 fileNum;
      uint16 column : 12;
      uint16 stmtBegin : 1;
      uint16 bbBegin : 1;
      uint16 unused : 2;
    } fileColumn;
    uint32 word0;
  } u;
  uint32 lineNum;     // line number of original src file, like foo.java
  uint32 mplLineNum;  // line number of mpl file
 public:
  SrcPosition() : lineNum(0), mplLineNum(0) {
    u.word0 = 0;
  }

  virtual ~SrcPosition() = default;

  uint32 RawData() const {
    return u.word0;
  }

  uint32 Filenum() const {
    return u.fileColumn.fileNum;
  }

  uint32 Column() const {
    return u.fileColumn.column;
  }

  uint32 Linenum() const {
    return lineNum;
  }

  uint32 MplLinenum() const {
    return mplLineNum;
  }

  void SetFilenum(int n) {
    u.fileColumn.fileNum = n;
  }

  void SetColumn(int n) {
    u.fileColumn.column = n;
  }

  void SetLinenum(int n) {
    lineNum = n;
  }

  void SetRawData(uint32 n) {
    u.word0 = n;
  }

  void SetMplLinenum(int n) {
    mplLineNum = n;
  }

  void CondSetLinenum(int n) {
    lineNum = lineNum ? lineNum : n;
  }

  void CondSetFilenum(int n) {
    uint32 i = u.fileColumn.fileNum;
    u.fileColumn.fileNum = i ? i : n;
  }
};
}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_SRC_POSITION_H

