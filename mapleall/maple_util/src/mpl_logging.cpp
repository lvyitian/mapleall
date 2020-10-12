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

#include "mpl_logging.h"

#include <sys/syscall.h>
#include <unistd.h>
#include <cstring>
#include <ctime>
#include "securec.h"
namespace maple {

LogInfo logInfo;

const char *long_log_levels[] = { /*[kLlDbg] =*/  "D",     /*[kLlLog] =*/ "L",     /*[kLlInfo] =*/  "Info ",
                                  /*[kLlWarn] =*/ "Warn ", /*[kLlErr] =*/ "Error", /*[kLlFatal] =*/ "Fatal" };

const char *tags[] = {
  /*[kLtThread] =*/ "TR",
  /*[kLtLooper] =*/ "LP",
};

std::ostream& LogInfo::MapleLogger(LogLevel l) {
  return std::cout;
}

void LogInfo::EmitLogForDevelop(enum LogTags tag, enum LogLevel ll, const char *file, const char *func, int line,
                            const char *fmt, ...) {
  char buf[kMaxLogLen];

  CHECK_FATAL(tag <= kLtAll, "illegal log tag");
  pid_t tid = syscall(__NR_gettid);

  time_t timeSeconds = time(nullptr);
  struct tm *nowTime = localtime(&timeSeconds);
  if (nowTime == nullptr) {
    CHECK_FATAL(false, "(nowTime) null ptr check");
  }
  int month = nowTime->tm_mon + 1;
  int day = nowTime->tm_mday;

  int hour = nowTime->tm_hour;
  int min = nowTime->tm_min;
  int sec = nowTime->tm_sec;

  int lenFront = snprintf_s(buf, kMaxLogLen, kMaxLogLen - 1, "%02d-%02d %02d:%02d:%02d %s %d %s ", month, day, hour, min,
                        sec, tags[tag], tid, long_log_levels[ll]);
  if (lenFront == -1) {
    FATAL(kLncFatal, "snprintf_s failed in mpl_logging.cpp");
  }
  va_list l;
  va_start(l, fmt);

  int lenBack = vsnprintf_s(buf + lenFront, kMaxLogLen - lenFront, kMaxLogLen - lenFront - 1, fmt, l);
  if (lenBack == -1) {
    FATAL(kLncFatal, "vsnprintf_s  failed ");
  }
  if (outMode) {
    int eNum = snprintf_s(buf + lenFront + lenBack, kMaxLogLen - lenFront - lenBack,
                          kMaxLogLen - lenFront - lenBack - 1, " [%s] [%s:%d]", func, file, line);
    if (eNum == -1) {
      FATAL(kLncFatal, "snprintf_s failed");
    }
  } else {
    int eNum = snprintf_s(buf + lenFront + lenBack, kMaxLogLen - lenFront - lenBack,
                          kMaxLogLen - lenFront - lenBack - 1, " [%s]", func);
    if (eNum == -1) {
      FATAL(kLncFatal, "snprintf_s failed");
    }
  }
  va_end(l);

  fprintf(outStream, "%s\n", buf);

  return;
}

void LogInfo::EmitLogForUser(enum LogNumberCode num, enum LogLevel ll, const char *fmt, ...) const {
  char buf[kMaxLogLen];

  int len = snprintf_s(buf, kMaxLogLen, kMaxLogLen - 1, "%s %02d: ", long_log_levels[ll], num);
  if (len == -1) {
    CHECK_FATAL(false, "snprintf_s failded");
  }
  if (outMode) {
    va_list l;
    va_start(l, fmt);
    int eNum = vsnprintf_s(buf + len, kMaxLogLen - len, kMaxLogLen - len - 1, fmt, l);
    if (eNum == -1) {
      CHECK_FATAL(false, "vsnprintf_s failed");
    }
    va_end(l);
  }

  fprintf(stderr, "%s\n", buf);

  return;
}

void LogInfo::EmitLogForUser(enum LogNumberCode num, enum LogLevel ll, const std::string &message ) const {
  EmitLogForUser(num, ll, message.c_str());
}

void LogInfo::EmitErrorMessage(const std::string &cond, const std::string &file, unsigned int line, const char *fmt, ...) const {
  char buf[kMaxLogLen];

  pid_t tid = syscall(__NR_gettid);

  int len = snprintf_s(buf, kMaxLogLen, kMaxLogLen - 1, "Tid(%d): CHECK/CHECK_FATAL failure: %s at [%s:%d] ", tid, cond.c_str(), file.c_str(),
                   line);
  if (len == -1) {
    CHECK_FATAL(false, "snprintf_s failed");
  }

  va_list l;
  va_start(l, fmt);
  int eNum = vsnprintf_s(buf + len, kMaxLogLen - len, kMaxLogLen - len - 1, fmt, l);
  if (eNum == -1) {
    CHECK_FATAL(false, "vsnprintf_s failed");
  }
  va_end(l);

  fprintf(stderr, "%s", buf);

  return;
}

}  // namespace maple
