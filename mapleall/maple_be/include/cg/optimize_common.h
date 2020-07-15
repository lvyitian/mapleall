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

#ifndef MAPLEALL_MAPLEBE_INCLUDE_OPTIMIZATIONPATTERN_H
#define MAPLEALL_MAPLEBE_INCLUDE_OPTIMIZATIONPATTERN_H
#include "cg_func.h"

namespace maplebe {
#define CFGO_CHAINING "red"
#define CFGO_SJ "burlywood1"
#define CFGO_FLIPCOND "cadetblue1"
#define CFGO_ALWAYS "green"
#define CFGO_UNREACH "yellow"
#define CFGO_DUP "orange"
#define CFGO_EMPTY "purple"
#define ICO_ITE "blue"  // if conversion optimization, if-then-else
#define ICO_IT "grey"   // if conversion optimization, if-then-else

class OptimizationPattern {
 protected:
  bool keepPosition;
  const char *patternname;
  CGFunc *cgfunc;
  const char *dotColor;
  bool checkOnly;

 public:
  OptimizationPattern(CGFunc *func) : cgfunc(func) {
    checkOnly = false;
    patternname = nullptr;
    dotColor = nullptr;
    keepPosition = false;
  }

  const char *GetPatternname() const {
    return patternname;
  }

  bool IsKeepPosition() const {
    return keepPosition;
  }

  void SetKeepPosition(bool flag) {
    keepPosition = flag;
  }

  void Search2Op(bool checkOnly);
  virtual bool Optimize(BB *&curbb) = 0;

 protected:
  void Log(int bbId);
};

class Optimizer {
 public:
  CGFunc *cgfunc;
  const char *name;

 protected:
  MemPool *memPool;
  // patterns need to run in different passes of cgfunc
  vector<OptimizationPattern *> diffPassPatterns;
  // patterns can run in a single pass of cgfunc
  vector<OptimizationPattern *> singlePassPatterns;

 public:
  Optimizer(CGFunc *func, MemPool *mp) : cgfunc(func), name(nullptr), memPool(mp) {
    func->theCFG->InitInsnVisitor(func, mp);
  }

  ~Optimizer();
  void Run(const char *funcname, bool checkOnly = false);
  virtual void InitOptimizePatterns() = 0;
};

class OptimzeLogger {
 private:
  map<const char *, int> globalStat;
  map<const char *, int> localStat;

 private:
  OptimzeLogger() {}

  ~OptimzeLogger() {}

  OptimzeLogger(const OptimzeLogger &);
  OptimzeLogger &operator=(const OptimzeLogger &);

 public:
  static OptimzeLogger &GetLogger() {
    static OptimzeLogger instance;
    return instance;
  }

  void Log(const char *patternname);
  void ClearLocal();
  void ClearGlobal();
  void Print(const char *funcname);
};

class DotGenerator {
 private:
  static map<int, const char *> coloringMap;

 public:
  static void SetColor(int bbId, const char *color);
  static void GenerateDot(const char *prefix, const CGFunc *cgfunc, const MIRModule *mod, bool includeEh = false,
                          regno_t vreg = 0);
};
}  // namespace maplebe

#endif /* MAPLEALL_MAPLEBE_INCLUDE_OPTIMIZATIONPATTERN_H */
