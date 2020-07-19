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

#ifndef PROFILE_H
#define PROFILE_H
#include <string>
#include <unordered_set>
#include <unordered_map>
#include "profile_type.h"
#include "mpl_logging.h"


class Profile {
 public:
  struct FuncItem {
    std::uint32_t callTimes;
    std::uint32_t type;
  };
  static const uint8_t kProfileMagic[];
  static const uint8_t kStringEnd;
  bool CheckMethodHot(const std::string &className) const;
  bool CheckFieldHot(const std::string &className) const;
  bool CheckClassHot(const std::string &className) const;
  bool DeCompress(const std::string &fileName, const std::string &name);
  bool CheckProfValid() const;
  void Dump() const;
  Profile();

 private:
  bool valid;
  static bool debug;
  std::unordered_set<std::string> classMeta;
  std::unordered_set<std::string> methodMeta;
  std::unordered_set<std::string> fieldMeta;
  std::unordered_map<std::string, FuncItem> funcProfData;
  std::unordered_set<std::string> &GetMeta(uint8_t type);
  bool CheckProfileHeader(const Header *header) const;
  std::string GetProfileNameByType(uint8_t type) const;
};
#endif
