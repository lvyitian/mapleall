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

#ifndef MAPLE_LFO_INCLUDE_LFO_FUNCTION_H
#define MAPLE_LFO_INCLUDE_LFO_FUNCTION_H
#include "lfo_mir_nodes.h"
#include "me_ir.h"

namespace maple {

class MeFunction;

class LfoWhileInfo {
 public:
  MIRSymbol *injectedIVSym = nullptr;   // the injected IV
  OriginalSt *ivOst = nullptr;          // the primary IV
  MeExpr *initExpr = nullptr;
  int32 stepValue = 0;
  MeExpr *tripCount = nullptr;
  bool canConvertDoloop = false;
};

class LfoIfInfo {
 public:
  LabelIdx endLabel = 0;   // the label that is out of the if statement
  LabelIdx elseLabel = 0;  // the label that is the begin of else branch
};


class LfoFunction {
 public:
  MemPool *lfomp;
  MapleAllocator lfoAlloc;
  MeFunction *meFunc;
  MapleMap<LabelIdx, LfoWhileInfo*> label2WhileInfo; // key is label at beginning of lowered while code sequence
  MapleMap<LabelIdx, LfoIfInfo*> label2IfInfo;       // key is target label of first conditional branch of lowered if code sequence
  MapleSet<LabelIdx> lfoCreatedLabelSet;  // for the labels that were created by lfo, we won't emit it

 public:
  LfoFunction(MemPool *mp, MeFunction *func) :
      lfomp(mp),
      lfoAlloc(mp),
      meFunc(func),
      label2WhileInfo(lfoAlloc.Adapter()),
      label2IfInfo(lfoAlloc.Adapter()),
      lfoCreatedLabelSet(lfoAlloc.Adapter()) {}

  void SetLabelCreatedByLfo(LabelIdx lbidx) {
    lfoCreatedLabelSet.insert(lbidx);
  }

  bool LabelCreatedByLfo(LabelIdx lbidx) {
   return lfoCreatedLabelSet.count(lbidx) != 0;
  }
};

}  // namespace maple
#endif  // MAPLE_LFO_INCLUDE_LFO_FUNCTION_H
