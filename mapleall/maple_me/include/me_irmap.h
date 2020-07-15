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

#ifndef MAPLE_ME_INCLUDE_ME_IRMAP_H
#define MAPLE_ME_INCLUDE_ME_IRMAP_H
#include "ssa_tab.h"
#include "me_function.h"
#include "irmap.h"

const int kHmapHashLength = 24593;  // from planetmath.org/goodhashtableprimes

namespace maple {
class MeIRMap : public IRMap {
 public:
  MeFunction *mirFunc;

  MeIRMap(MeFunction *f, Dominance *dom, MemPool *mp, MemPool *tempmp)
    : IRMap(f->meSSATab, dom, mp, tempmp, kHmapHashLength), mirFunc(f) {
    dumpStmtNum = MeOption::stmtNum;
  }

  ~MeIRMap() {}

  // following are virtual functions
  BB *GetBB(BBId id) {
    return mirFunc->bbVec.at(id.idx);
  }

  BB *GetBBForLabidx(LabelIdx lidx, PUIdx pidx = 0) {
    return mirFunc->labelBBIdMap[lidx];
  }

  void Dump();
};

}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_IRMAP_H
