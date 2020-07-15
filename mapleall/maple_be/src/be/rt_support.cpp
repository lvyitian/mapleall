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

#include "rt_support.h"
#include "mir_nodes.h"
#include "name_mangler.h"
#include "special_func.h"

namespace maplebe {

const char *RTSupport::OBJECT_MAP_SECTION_NAME = ".maple.objectmap";
const char *RTSupport::GCTIB_LABEL_ARRAY_OF_OBJECT = (GCTIB_PREFIX_STR + string(NameMangler::kArrayObject)).c_str();
const char *RTSupport::GCTIB_LABEL_JAVA_OBJECT = (GCTIB_PREFIX_STR + string(NameMangler::kJavaLangObjectStr)).c_str();
}  // namespace maplebe
