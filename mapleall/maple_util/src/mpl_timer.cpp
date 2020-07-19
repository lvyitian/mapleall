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

#include "mpl_timer.h"

#include <chrono>

namespace maple {
MPLTimer::MPLTimer() {}

MPLTimer::~MPLTimer() {}

void MPLTimer::Start() {
  startTime = std::chrono::system_clock::now();
}

void MPLTimer::Stop() {
  endTime = std::chrono::system_clock::now();
}

long MPLTimer::Elapsed() {
  return std::chrono::duration_cast<std::chrono::seconds>(endTime - startTime).count();
}

long MPLTimer::ElapsedMilliseconds() {
  return std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();
}

long MPLTimer::ElapsedMicroseconds() {
  return std::chrono::duration_cast<std::chrono::microseconds>(endTime - startTime).count();
}
}
