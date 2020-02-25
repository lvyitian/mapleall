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

#include "mir_module.h"
#include "types_def.h"
#include <iostream>

#define INDENTUNIT 2  // number of blank chars of each indentation

namespace maple {

std::string const kBlankString = "                                                                                ";

void PrintIndentation(int32 indent) {
  int32 indentamt = indent * INDENTUNIT;
  do {
    LogInfo::MapleLogger() << kBlankString.substr(0, indentamt);
    indentamt -= kBlankString.length();
  } while (indentamt > 0);
}

void PrintString(const std::string &str) {
  uint32 i = 0;
  std::u16string str16;
  LogInfo::MapleLogger() << " \"";
  while (i < str.length()) {
    unsigned char c = str[i++];
    // differentiate printable and non-printable charactors
    if (c >= 0x20 && c <= 0x7e) {
      // escape "
      switch (c) {
        case '"':
          LogInfo::MapleLogger() << "\\\"";
          break;
        case '\\':
          LogInfo::MapleLogger() << "\\\\";
          break;
        default:
          LogInfo::MapleLogger() << c;
          break;
      }
    } else {
      LogInfo::MapleLogger() << "\\x" << std::hex << std::setfill('0') << std::setw(2) << (unsigned)c << std::dec;
    }
  }
  LogInfo::MapleLogger() << "\"";
}

}  // namespace maple
