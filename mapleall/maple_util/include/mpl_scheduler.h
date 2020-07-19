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

#ifndef MAPLEUTIL_INCLUDE_MPLSCHEDULER_H
#define MAPLEUTIL_INCLUDE_MPLSCHEDULER_H

#include <vector>
#include <set>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <inttypes.h>
#include <pthread.h>
#include <inttypes.h>
#include <iostream>
#include <string>

namespace maple {
#define MP_SYNC        \
  (stmt) GlobalLock(); \
  stmt GlobalUnlock();

class MplTask {
 public:
  MplTask() : taskId(0) {}

  virtual ~MplTask() {}

  virtual int Run(void *param = NULL) {
    return 0;
  }

  virtual int Finish(void *param = NULL) {
    return 0;
  }

  inline void SetTaskId(uint32_t id) {
    taskId = id;
  }

  inline uint32_t GetTaskId() const {
    return taskId;
  }

 protected:
  uint32_t taskId;
};

class MplScheduler {
 public:
  MplScheduler(const char *name);
  virtual ~MplScheduler() {}

  virtual void AddTask(MplTask *task);
  virtual int RunTask(uint32_t nthreads, bool seq = false);
  virtual void *EncodeThreadMainEnv(uint32_t threadId) {
    return nullptr;
  }

  virtual void DecodeThreadMainEnv(void *env) {}

  virtual void *EncodeThreadFinishEnv() {
    return nullptr;
  }

  virtual void DecodeThreadFinishEnv(void *env) {}

  inline void GlobalLock() {
    pthread_mutex_lock(&mutexGlobal);
  }

  inline void GlobalUnlock() {
    pthread_mutex_unlock(&mutexGlobal);
  }

  void Reset();

 protected:
  std::string schedulerName;
  std::vector<MplTask *> tbTasks;
  std::set<uint32_t> tbTaskIdsToFinish;
  uint32_t taskIdForAdd;
  uint32_t taskIdToRun;
  uint32_t taskIdExpected;
  uint32_t numberTasks;
  uint32_t nTasksFinish;
  pthread_mutex_t mutexTaskIdsToRun;
  pthread_mutex_t mutexTaskIdsToFinish;
  pthread_mutex_t mutexTaskFinishProcess;
  pthread_mutex_t mutexGlobal;
  pthread_cond_t conditionFinishProcess;
  bool isSchedulerSeq;
  bool dumpTime;

  typedef enum { kThreadStop, kThreadRun, kThreadPause } THREAD_STATUS;
  THREAD_STATUS statusFinish;
  virtual int FinishTask(MplTask *task);
  virtual MplTask *GetTaskToRun();
  virtual uint32_t GetTaskIdsFinishSize();
  virtual MplTask *GetTaskFinishFirst();
  virtual void RemoveTaskFinish(uint32_t id);
  virtual void TaskIdFinish(uint32_t id);
  void ThreadMain(uint32_t threadID, void *env);
  void ThreadFinishNoSeq(void *env);
  void ThreadFinishSeq(void *env);
  void ThreadFinish(void *env);
  // Callback Function
  virtual void CallbackThreadMainStart() {}

  virtual void CallbackThreadMainEnd() {}

  virtual void CallbackThreadFinishStart() {}

  virtual void CallbackThreadFinishEnd() {}

  virtual void *CallbackGetTaskRunParam() {
    return NULL;
  }

  virtual void *CallbackGetTaskFinishParam() {
    return NULL;
  }
};
}

#endif // namespace maple
