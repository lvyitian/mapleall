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
  bool DeCompress(const std::string &fileName, const std::string &javaName);
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
