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

#include "mpl_scheduler.h"
#include "mpl_timer.h"
#include "mpl_logging.h"
namespace maple {

// ========== MplScheduler ==========
MplScheduler::MplScheduler(const char *name)
  : schedulerName(name),
    taskIdForAdd(0),
    taskIdToRun(0),
    taskIdExpected(0),
    numberTasks(0),
    nTasksFinish(0),
    isSchedulerSeq(false),
    dumpTime(false),
    statusFinish(kThreadRun) {
  char *envstr = getenv("MP_DUMPTIME");
  if (envstr && atoi(envstr) == 1) {
    dumpTime = true;
  }
  pthread_mutex_init(&mutexTaskIdsToRun, nullptr);
  pthread_mutex_init(&mutexTaskIdsToFinish, nullptr);
  pthread_mutex_init(&mutexGlobal, nullptr);
  mutexTaskFinishProcess = PTHREAD_MUTEX_INITIALIZER;
  conditionFinishProcess = PTHREAD_COND_INITIALIZER;
}

void MplScheduler::AddTask(MplTask *task) {
  task->SetTaskId(taskIdForAdd);
  tbTasks.push_back(task);
  taskIdForAdd++;
  numberTasks++;
}

MplTask *MplScheduler::GetTaskToRun() {
  MplTask *task = nullptr;
  pthread_mutex_lock(&mutexTaskIdsToRun);
  if (taskIdToRun < numberTasks) {
    task = tbTasks[taskIdToRun++];
  }
  pthread_mutex_unlock(&mutexTaskIdsToRun);
  return task;
}

uint32_t MplScheduler::GetTaskIdsFinishSize() {
  pthread_mutex_lock(&mutexTaskIdsToFinish);
  uint32_t size = tbTaskIdsToFinish.size();
  pthread_mutex_unlock(&mutexTaskIdsToFinish);
  return size;
}

MplTask *MplScheduler::GetTaskFinishFirst() {
  MplTask *task = nullptr;
  pthread_mutex_lock(&mutexTaskIdsToFinish);
  if (tbTaskIdsToFinish.size() > 0) {
    task = tbTasks[*(tbTaskIdsToFinish.begin())];
  }
  pthread_mutex_unlock(&mutexTaskIdsToFinish);
  return task;
}

void MplScheduler::RemoveTaskFinish(uint32_t id) {
  pthread_mutex_lock(&mutexTaskIdsToFinish);
  tbTaskIdsToFinish.erase(id);
  pthread_mutex_unlock(&mutexTaskIdsToFinish);
}

void MplScheduler::TaskIdFinish(uint32_t id) {
  pthread_mutex_lock(&mutexTaskIdsToFinish);
  tbTaskIdsToFinish.insert(id);
  pthread_mutex_unlock(&mutexTaskIdsToFinish);
}

int MplScheduler::RunTask(uint32_t nthreads, bool seq) {
  isSchedulerSeq = seq;
  if (nthreads > 0) {
    taskIdExpected = 0;
    std::thread threads[nthreads];
    std::thread threadFinish;
    for (uint32_t i = 0; i < nthreads; i++) {
      threads[i] = std::thread(&MplScheduler::ThreadMain, this, i, EncodeThreadMainEnv(i));
    }
    if (isSchedulerSeq) {
      threadFinish = std::thread(&MplScheduler::ThreadFinish, this, EncodeThreadFinishEnv());
    }
    for (uint32_t i = 0; i < nthreads; i++) {
      threads[i].join();
    }
    if (isSchedulerSeq) {
      threadFinish.join();
    }
  }
  return 0;
}

int MplScheduler::FinishTask(MplTask *task) {
  TaskIdFinish(task->GetTaskId());
  pthread_mutex_lock(&mutexTaskFinishProcess);
  if (statusFinish == kThreadPause) {
    statusFinish = kThreadRun;
    pthread_cond_signal(&conditionFinishProcess);
  }
  pthread_mutex_unlock(&mutexTaskFinishProcess);
  return 0;
}

void MplScheduler::Reset() {
  for (MplTask *task : tbTasks) {
    if (task) {
      delete task;
    }
  }
  tbTasks.clear();
  tbTaskIdsToFinish.clear();
  taskIdForAdd = 0;
  taskIdToRun = 0;
  taskIdExpected = 0;
  numberTasks = 0;
  nTasksFinish = 0;
  isSchedulerSeq = false;
}

void MplScheduler::ThreadMain(uint32_t threadID, void *env) {
  MPLTimer timerTotal;
  MPLTimer timerRun;
  MPLTimer timerToRun;
  MPLTimer timerFinish;
  double timeRun = 0.0;
  double timeToRun = 0.0;
  double timeFinish = 0.0;
  if (dumpTime) {
    timerTotal.Start();
  }
  DecodeThreadMainEnv(env);
  CallbackThreadMainStart();
  if (dumpTime) {
    timerToRun.Start();
  }
  MplTask *task = GetTaskToRun();
  if (dumpTime) {
    timerToRun.Stop();
    timeToRun += timerRun.ElapsedMicroseconds();
  }
  while (task) {
    if (dumpTime) {
      timerRun.Start();
    }
    void *paramRun = CallbackGetTaskRunParam();
    task->Run(paramRun);
    if (dumpTime) {
      timerRun.Stop();
      timeRun += timerRun.ElapsedMicroseconds();
      timerFinish.Start();
    }
    if (isSchedulerSeq) {
      FinishTask(task);
    } else {
      void *paramFinish = CallbackGetTaskFinishParam();
      task->Finish(paramFinish);
    }
    if (dumpTime) {
      timerFinish.Stop();
      timeFinish += timerFinish.ElapsedMicroseconds();
      timerToRun.Start();
    }
    task = GetTaskToRun();
    if (dumpTime) {
      timerToRun.Stop();
      timeToRun += timerToRun.ElapsedMicroseconds();
    }
  }
  CallbackThreadMainEnd();
  if (dumpTime) {
    timerTotal.Stop();
    GlobalLock();
    std::cout << "MP TimeDump(" << schedulerName.c_str() << ")::Thread" << threadID << "::ThreadMain "
              << timerTotal.ElapsedMicroseconds() / 1000.0 << "ms" << std::endl;
    std::cout << "MP TimeDump(" << schedulerName.c_str() << ")::Thread" << threadID << "::ThreadMain::Task::Run "
              << timeRun / 1000.0 << "ms" << std::endl;
    std::cout << "MP TimeDump(" << schedulerName.c_str() << ")::Thread" << threadID << "::ThreadMain::Task::ToRun "
              << timeToRun / 1000.0 << "ms" << std::endl;
    std::cout << "MP TimeDump(" << schedulerName.c_str() << ")::Thread" << threadID << "::ThreadMain::Task::Finish "
              << timeFinish / 1000.0 << "ms" << std::endl;
    GlobalUnlock();
  }
  pthread_exit(nullptr);
}

void MplScheduler::ThreadFinish(void *env) {
  statusFinish = kThreadRun;
  DecodeThreadFinishEnv(env);
  CallbackThreadFinishStart();
  MplTask *task = nullptr;
  uint32_t taskId;
  while (nTasksFinish < numberTasks) {
    while (statusFinish == kThreadPause) {
      pthread_mutex_lock(&mutexTaskFinishProcess);
      pthread_cond_wait(&conditionFinishProcess, &mutexTaskFinishProcess);
      pthread_mutex_unlock(&mutexTaskFinishProcess);
    }
    while (true) {
      if (GetTaskIdsFinishSize() == 0) {
        pthread_mutex_lock(&mutexTaskFinishProcess);
        statusFinish = kThreadPause;
        pthread_mutex_unlock(&mutexTaskFinishProcess);
        break;
      }
      task = GetTaskFinishFirst();
      CHECK_FATAL(task != nullptr, "null ptr check");
      taskId = task->GetTaskId();
      if (isSchedulerSeq) {
        if (taskId != taskIdExpected) {
          break;
        }
      }
      void *paramFinish = CallbackGetTaskFinishParam();
      task->Finish(paramFinish);
      nTasksFinish++;
      if (isSchedulerSeq) {
        taskIdExpected++;
      }
      RemoveTaskFinish(task->GetTaskId());
    }
  }
  CallbackThreadFinishEnd();
  pthread_exit(nullptr);
}

}  // namespace maple
