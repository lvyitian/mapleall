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

#include "rt_support.h"
#include "mir_nodes.h"
#include "name_mangler.h"
#include "special_func.h"

namespace maplebe {

const char *RTSupport::OBJECT_MAP_SECTION_NAME = ".maple.objectmap";
const char *RTSupport::GCTIB_LABEL_ARRAY_OF_OBJECT = static_cast<const char*>(strdup((GCTIB_PREFIX_STR + string(NameMangler::kArrayObject)).c_str()));
const char *RTSupport::GCTIB_LABEL_JAVA_OBJECT = static_cast<const char*>(strdup((GCTIB_PREFIX_STR + string(NameMangler::kJavaLangObjectStr)).c_str()));
}  // namespace maplebe
