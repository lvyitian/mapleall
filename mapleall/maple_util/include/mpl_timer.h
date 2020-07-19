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

#ifndef MAPLEJAVA_MPLTIMER_H
#define MAPLEJAVA_MPLTIMER_H

#include <chrono>

namespace maple {
class MPLTimer {
 public:
  MPLTimer();
  ~MPLTimer();
  void Start();
  void Stop();
  long Elapsed();
  long ElapsedMilliseconds();
  long ElapsedMicroseconds();
 private:
  std::chrono::system_clock::time_point startTime, endTime;
};
}
#endif //MAPLEJAVA_MPLTIMER_H
