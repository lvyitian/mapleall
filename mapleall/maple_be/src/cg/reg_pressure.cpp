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

#include <algorithm>

#include "cg_bb.h"
#include "cg_func.h"
#include "reg_pressure.h"
#include "aarch64_schedule.h"
#include "deps.h"

namespace maplebe {
/* ------- RegPressure function -------- */
int RegPressure::maxRegClassNum = 0;

// print regpressure information
void RegPressure::DumpRegPressure() {
  const int kweight = 12;
  PRINT_STR_VAL("Priority: ", priority);
  PRINT_STR_VAL("maxDepth: ", maxDepth);
  PRINT_STR_VAL("near: ", near);
  LogInfo::MapleLogger() << endl;
  LogInfo::MapleLogger() << left << setw(kweight) << "usereg: ";
  for (auto use : uses) {
    LogInfo::MapleLogger() << "R" << use << " ";
  }
  LogInfo::MapleLogger() << endl;
  LogInfo::MapleLogger() << left << setw(kweight) << "defreg: ";
  for (auto def : defs) {
    LogInfo::MapleLogger() << "R" << def << " ";
  }
  LogInfo::MapleLogger() << endl;
}

} // namespace maplebe

