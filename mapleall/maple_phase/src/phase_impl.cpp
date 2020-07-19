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

#include <cstdlib>
#include "phase_impl.h"
#include "mpl_timer.h"

namespace maple {
// ========== FuncOptimizeImpl ==========
FuncOptimizeImpl::FuncOptimizeImpl(MIRModule *mod, KlassHierarchy *kh, bool trace) :
    module(mod), klassHierarchy(kh), trace(trace) {
  builder = new MIRBuilderExt(module);
}

FuncOptimizeImpl::~FuncOptimizeImpl() {
  if (builder) {
    delete builder;
  }
}

void FuncOptimizeImpl::CreateLocalBuilder(pthread_mutex_t *mutex) {
  // Each thread needs to use its own MIRBuilderExt.
  builder = new MIRBuilderExt(module, mutex);
}

void FuncOptimizeImpl::ProcessFunc(MIRFunction *func) {
  SetCurrentFunction(func);
  ProcessBlock(func->body);
}

void FuncOptimizeImpl::ProcessBlock(StmtNode *stmt) {
  if (!stmt) {
    return;
  }
  switch (stmt->op) {
    case OP_if: {
      IfStmtNode *ifnode = static_cast<IfStmtNode *>(stmt);
      ProcessBlock(ifnode->thenPart);
      ProcessBlock(ifnode->elsePart);
      break;
    }
    case OP_while:
    case OP_dowhile: {
      WhileStmtNode *whilenode = static_cast<WhileStmtNode *>(stmt);
      ProcessBlock(whilenode->body);
      break;
    }
    case OP_block: {
      BlockNode *block = static_cast<BlockNode *>(stmt);
      for (StmtNode *s = block->GetFirst(), *next = nullptr; s; s = next) {
        next = s->GetNext();
        ProcessBlock(s);
      }
      break;
    }
    default: {
      ProcessStmt(stmt);
      break;
    }
  }
}

// ========== FuncOptimizeIterator ==========
thread_local FuncOptimizeImpl *FuncOptimizeIterator::phaseImplLocal = nullptr;

FuncOptimizeIterator::FuncOptimizeIterator(std::string phaseName, FuncOptimizeImpl *phaseImpl)
  : MplScheduler(phaseName.c_str()), phaseImpl(phaseImpl) {
  char *envstr = getenv("MP_DUMPTIME");
  dumpTime = (envstr && atoi(envstr) == 1);
}

FuncOptimizeIterator::~FuncOptimizeIterator() {
  if (phaseImpl) {
    delete phaseImpl;
  }
}

void FuncOptimizeIterator::Run(uint32_t nThread, bool isSeq) {
  if (nThread == 1) {
    RunSerial();
  } else {
    RunParallel(nThread, isSeq);
  }
}

void FuncOptimizeIterator::RunSerial() {
  MPLTimer timer;
  if (dumpTime) {
    timer.Start();
  }
  for (size_t i = 0; i < phaseImpl->GetModule()->functionList.size(); i++) {
    MIRFunction *func = phaseImpl->GetModule()->functionList[i];
    phaseImpl->ProcessFunc(func);
  }
  phaseImpl->Finish();
  if (dumpTime) {
    timer.Stop();
    INFO(kLncInfo, "FuncOptimizeIterator::RunSerial (%s): %lf ms", schedulerName.c_str(), timer.ElapsedMicroseconds() / 1000.0);
  }
}

void FuncOptimizeIterator::RunParallel(uint32_t nThread, bool isSeq) {
  MPLTimer timer;
  if (dumpTime) {
    timer.Start();
  }
  Reset();
  for (MIRFunction *func : phaseImpl->GetModule()->functionList) {
    Task *task = new Task(func);
    AddTask(task);
  }
  if (dumpTime) {
    timer.Stop();
    INFO(kLncInfo, "FuncOptimizeIterator::RunParallel (%s): AddTask() = %lf ms", schedulerName.c_str(),
         timer.ElapsedMicroseconds() / 1000.0);
  }

  if (dumpTime) {
    timer.Start();
  }
  RunTask(nThread, isSeq);
  phaseImpl->Finish();

  Reset();
  if (dumpTime) {
    timer.Stop();
    INFO(kLncInfo, "FuncOptimizeIterator::RunParallel (%s): RunTask() = %lf ms", schedulerName.c_str(),
         timer.ElapsedMicroseconds() / 1000.0);
  }
}

}  // namespace maple
