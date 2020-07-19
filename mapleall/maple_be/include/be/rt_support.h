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

#ifndef MAPLEBE_INCLUDE_CG_RTSUPPORT_H
#define MAPLEBE_INCLUDE_CG_RTSUPPORT_H

#include <cstdint>
#include <string>

namespace maplebe {

/**
 * This class contains constants about the ABI of the runtime, such as symbols
 * for GC-related metadata in generated binary files.
 */
class RTSupport {
 public:
  static const char *OBJECT_MAP_SECTION_NAME;
  static const char *GCTIB_LABEL_ARRAY_OF_OBJECT;
  static const char *GCTIB_LABEL_JAVA_OBJECT;
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_RTSUPPORT_H
