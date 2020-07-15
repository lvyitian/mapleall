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

#ifndef MAPLEBE_INCLUDE_CG_GLOBALOPT_H
#define MAPLEBE_INCLUDE_CG_GLOBALOPT_H

#include "cg_func.h"
#include "insn.h"
#include "cg_phase.h"
#include "store_load_opt.h"

namespace maplebe {

class LiveAnalysis;

class GlobalOpt {
 public:
  CGFunc *cgfunc;
  MemPool *gomp;

 public:
  GlobalOpt(CGFunc *func, MemPool *mp) : cgfunc(func), gomp(mp) {}

  virtual ~GlobalOpt() {}

  virtual void Run() {}

  const char *PhaseName() const {
    return "globalopt";
  }
};

CGFUNCPHASE_CANSKIP(CgDoGlobalOpt, "globalopt")

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_GLOBALOPT_H
