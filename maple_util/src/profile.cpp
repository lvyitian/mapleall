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

#include "profile.h"
#include "name_mangler.h"
#include <cassert>
#include <cstring>
#include <fstream>
#include <istream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <sys/types.h>
#include <cerrno>
const uint8_t Profile::kProfileMagic[] = { 'm', 'a', 'p', 'l', 'e', '.', 'p', 'r', 'o', 'f', 'i', 'l', 'e', '\0' };
const uint8_t Profile::kStringEnd = 0x00;
bool Profile::debug = false;

Profile::Profile() : valid(false) {}

bool Profile::CheckProfileHeader(const Header *header) const {
  return (memcmp(header->magic, kProfileMagic, sizeof(kProfileMagic)) == 0);
}

std::string Profile::GetProfileNameByType(uint8_t type) const {
  switch (type) {
    case kFunction:
      return "FUNCTION";
    case kClassMeta:
      return "CLASSMETA";
    case kFieldMeta:
      return "FIELDMETA";
    case kMethodMeta:
      return "METHODMETA";
    default:
      CHECK_FATAL(false, "type not found");
  }
  return "";
}

bool Profile::DeCompress(const std::string &path, const std::string &javaName) {
  bool res = true;
  std::ifstream in(path, std::ios::binary);
  if (!in) {
    if (errno != ENOENT && errno != EACCES) {
      std::cout << "WARN: DeCompress("
                << "), failed to open " << path << ", " << strerror(errno) << std::endl;
    }
    res = false;
    return res;
  }
  in.seekg(0, std::ios::end);
  std::size_t byteCount = in.tellg();
  in.seekg(0, std::ios::beg);
  std::vector<char> bufVector;
  bufVector.resize(byteCount);
  char *buf = reinterpret_cast<char *>(bufVector.data());
  if (!in.read(buf, byteCount)) {
    std::cout << "WARN: DeCompress("
              << "), failed to read all data for " << path << ", " << strerror(errno) << std::endl;
    res = false;
    return res;
  }
  if (byteCount < sizeof(Header)) {
    std::cout << "WARN: DeCompress("
              << "), failed, read no data for " << path << ", " << strerror(errno) << std::endl;
    res = false;
    return res;
  }
  Header *header = reinterpret_cast<Header *>(buf);
  if (!CheckProfileHeader(header)) {
    if (debug) {
      std::cout << "invalid maigc number " << reinterpret_cast<char *>(header->magic) << std::endl;
    }
    res = false;
    return res;
  }

  uint32_t stringTabSize = byteCount - header->stringTabOff + 1;
  if (debug) {
    std::cout << "Header summary "
              << "profile num " << static_cast<uint32_t>(header->profileNum) << "string table size" << stringTabSize
              << std::endl;
  }
  const char *strBuf = buf + header->stringTabOff;
  std::vector<std::string> strMap;
  uint32_t idx = 0;
  strMap.push_back(strBuf);
  while (idx < stringTabSize) {
    if (*(strBuf + idx) == kStringEnd) {
      strMap.push_back(strBuf + idx + 1);
    }
    idx++;
  }
  if (debug) {
    std::cout << "str size " << idx << std::endl;
    for (auto item : strMap) {
      std::cout << item << std::endl;
    }
    std::cout << "str size print end  " << std::endl;
  }
  for (idx = 0; idx < header->profileNum; idx++) {
    ProfileDataInfo *profileData = &(header->data[idx]);
    if (debug) {
      std::cout << "profile num for type  " << GetProfileNameByType(profileData->profileType) << " "
                << static_cast<uint32_t>(profileData->mapleFileNum) << std::endl;
    }
    uint8_t mapleFileIdx = 0;
    uint32_t offset = 0;
    for (mapleFileIdx = 0; mapleFileIdx < profileData->mapleFileNum; mapleFileIdx++) {
      uint32_t item = 0;
      if (profileData->profileType == kFunction) {
        MapleFileProf<FunctionItem> *funcProf =
            reinterpret_cast<MapleFileProf<FunctionItem> *>(buf + profileData->profileDataOff + offset);
        if (debug) {
          std::cout << " function profile java "
                    << ":" << strMap.at(funcProf->idx) << ":" << funcProf->num << "\n";
        }
        if (javaName.find(strMap.at(funcProf->idx)) != std::string::npos) {
          for (item = 0; item < funcProf->num; item++) {
            FunctionItem *funcItem = &(funcProf->items[item]);
            std::string funcName = strMap.at(funcItem->classIdx) + NameMangler::kNameSplitterStr + strMap.at(funcItem->methodIdx) + NameMangler::kNameSplitterStr +
                                   strMap.at(funcItem->sigIdx);
            funcProfData.insert(
                std::make_pair(funcName, (FuncItem){ .callTimes = funcItem->callTimes, .type = funcItem->type }));
          }
        }
        offset += sizeof(MapleFileProf<FunctionItem>) + sizeof(FunctionItem) * (funcProf->num - 1);
      } else {
        MapleFileProf<MetaItem> *metaProf =
            reinterpret_cast<MapleFileProf<MetaItem> *>(buf + profileData->profileDataOff + offset);
        if (debug) {
          std::cout << " meta profile java "
                    << ":" << strMap.at(metaProf->idx) << ":" << metaProf->num << "\n";
        }
        std::unordered_set<std::string> &metaData = GetMeta(profileData->profileType);
        if (javaName.find(strMap.at(metaProf->idx)) != std::string::npos) {
          for (item = 0; item < metaProf->num; item++) {
            MetaItem *metaItem = &(metaProf->items[item]);
            metaData.insert(strMap.at(metaItem->idx));
          }
        }
        offset += sizeof(MapleFileProf<MetaItem>) + sizeof(MetaItem) * (metaProf->num - 1);
      }
    }
  }
  std::cout << "SUCC parse " << path << std::endl;
  valid = true;
  return res;
}

bool Profile::CheckMethodHot(const std::string &className) const {
  if (valid) {
    if (methodMeta.find(className) == methodMeta.end()) {
      return false;
    }
    return true;
  }
  return false;
}

bool Profile::CheckFieldHot(const std::string &className) const {
  if (valid) {
    if (fieldMeta.find(className) == fieldMeta.end()) {
      return false;
    }
    return true;
  }
  return false;
}

bool Profile::CheckClassHot(const std::string &className) const {
  if (valid) {
    if (classMeta.find(className) == classMeta.end()) {
      return false;
    }
    return true;
  }
  return false;
}

std::unordered_set<std::string> &Profile::GetMeta(uint8_t type) {
  switch (type) {
    case kClassMeta:
      return classMeta;
    case kFieldMeta:
      return fieldMeta;
    case kMethodMeta:
      return methodMeta;
    default:
      CHECK_FATAL(0, "type not found");
      return classMeta;
  }
}

void Profile::Dump() const {
  std::ofstream outfile;
  outfile.open("prof.dump");
  for (auto item : classMeta) {
    outfile << item << std::endl;
  }
}
