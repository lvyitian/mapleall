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

#include "phasedriver.h"
#include "mpl_timer.h"

namespace maple {
thread_local PhaseDriverImpl *PhaseDriver::mPhaseImplLocal = nullptr;

PhaseDriver::PhaseDriver(const char *phaseName) : MplScheduler(phaseName), mModule(nullptr),
                                                  mPhaseImpl(nullptr),
                                                  mPhaseName(phaseName) {}

void PhaseDriver::RunAll(MIRModule *module, int thread, bool bSeq) {
  mModule = module;
  mPhaseImpl = NewPhase();
  ASSERT(mPhaseImpl != nullptr, "null ptr check");
  mPhaseImpl->GlobalInit();
  if (thread == 1) {
    RunSerial();
  } else {
    RunParallel(thread, bSeq);
  }
  delete mPhaseImpl;
  mPhaseImpl = nullptr;
}

void PhaseDriver::RunSerial() {
  mPhaseImplLocal = NewPhase();
  mPhaseImplLocal->LocalInit();
  MPLTimer timer;
  if (dumpTime) {
    timer.Start();
  }
  RegisterTasks();
  if (dumpTime) {
    timer.Stop();
    INFO(kLncInfo, "PhaseDriver::RegisterTasks (%s): %lf ms", mPhaseName.c_str(), timer.ElapsedMicroseconds() / 1000.0);
    timer.Start();
  }

  MplTask *task = GetTaskToRun();
  while (task) {
    void *paramRun = CallbackGetTaskRunParam();
    task->Run(paramRun);
    void *paramFinish = CallbackGetTaskFinishParam();
    task->Run(paramFinish);
  }
  if (dumpTime) {
    timer.Stop();
    INFO(kLncInfo, "PhaseDriver::RunTask (%s): %lf ms", mPhaseName.c_str(), timer.ElapsedMicroseconds() / 1000.0);
  }
}

void PhaseDriver::RunParallel(int thread, bool bSeq) {
  MPLTimer timer;
  if (dumpTime) {
    timer.Start();
  }
  RegisterTasks();
  if (dumpTime) {
    timer.Stop();
    INFO(kLncInfo, "PhaseDriver::RegisterTasks (%s): %lf ms", mPhaseName.c_str(), timer.ElapsedMicroseconds() / 1000.0);
  }

  if (dumpTime) {
    timer.Start();
  }
  RunTask(thread, bSeq);
  if (dumpTime) {
    timer.Stop();
    INFO(kLncInfo, "PhaseDriver::RunTask (%s): %lf ms", mPhaseName.c_str(), timer.ElapsedMicroseconds() / 1000.0);
  }
}

void PhaseDriver::CallbackThreadMainStart() {
  mPhaseImplLocal = NewPhase();
  mPhaseImplLocal->LocalInit();
}

void PhaseDriver::CallbackThreadMainEnd() {
  delete mPhaseImplLocal;
  mPhaseImplLocal = nullptr;
}

}  // namespace maple
