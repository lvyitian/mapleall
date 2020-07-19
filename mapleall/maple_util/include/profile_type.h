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

#ifndef PROFILE_TYPE_H
#define PROFILE_TYPE_H

enum ProfileType : uint8_t { kFunction = 0x00, kClassMeta = 0x01, kFieldMeta = 0x02, kMethodMeta = 0x03 };

typedef struct {
  uint32_t profileDataOff;
  uint8_t profileType;
  uint8_t mapleFileNum;
} ProfileDataInfo;

typedef struct {
  uint32_t classIdx;
  uint32_t methodIdx;
  uint32_t sigIdx;
  uint32_t callTimes;
  uint8_t type;
} FunctionItem;

typedef struct MetaItem {
  uint32_t idx;
} MetaItem;

template <typename T>
struct MapleFileProf {
  uint32_t idx;
  uint32_t num;
  uint32_t size;
  T items[1];
};

typedef struct {
  uint8_t magic[14];
  uint8_t ver[3];
  uint8_t checkSum[4];
  uint8_t profileNum;
  uint16_t pad;
  uint32_t headerSize;
  uint32_t stringCount;
  uint32_t stringTabOff;
  ProfileDataInfo data[1];
} Header;

#endif
