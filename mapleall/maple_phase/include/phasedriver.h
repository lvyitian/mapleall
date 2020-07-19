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

#ifndef MAPLEPHASE_INCLUDE_PHASEDRIVER_H
#define MAPLEPHASE_INCLUDE_PHASEDRIVER_H

#include "mir_module.h"
#include "mir_function.h"
#include "mpl_scheduler.h"

namespace maple {
class PhaseDriverImpl {
 public:
  virtual ~PhaseDriverImpl() {}

  virtual void GlobalInit() {}

  virtual void LocalInit() {}

  virtual void ProcessRun(uint32 taskId, void *target, void *paramEx) {}

  virtual void ProcessFinish(uint32 taskId, void *target, void *paramEx) {}
};

class PhaseDriver : public MplScheduler {
 public:
  class Task : public MplTask {
   public:
    void *mTarget;
    void *mParamEx;

   public:
    Task(void *target, void *paramEx = NULL) : mTarget(target), mParamEx(paramEx) {}

    ~Task() {}

    virtual int Run(void *param) {
      static_cast<PhaseDriverImpl*>(param)->ProcessRun(taskId, mTarget, mParamEx);
      return 0;
    }

    virtual int Finish(void *param) {
      static_cast<PhaseDriverImpl*>(param)->ProcessFinish(taskId, mTarget, mParamEx);
      return 0;
    }
  };

 public:
  MIRModule *mModule;
  PhaseDriverImpl *mPhaseImpl;
  thread_local static PhaseDriverImpl *mPhaseImplLocal;
  std::string mPhaseName;

 public:
  PhaseDriver(const char *phaseName);
  virtual ~PhaseDriver() {}

  virtual void RunAll(MIRModule *module, int thread, bool bSeq = false);
  virtual void RunSerial();
  virtual void RunParallel(int thread, bool bSeq = false);
  virtual PhaseDriverImpl *NewPhase() = 0;
  virtual void RegisterTasks() = 0;

 protected:
  virtual void CallbackThreadMainStart();
  virtual void CallbackThreadMainEnd();
  virtual void *CallbackGetTaskRunParam() {
    return mPhaseImplLocal;
  }

  virtual void *CallbackGetTaskFinishParam() {
    return mPhaseImplLocal;
  }
};
}  // namespace maple

#endif
