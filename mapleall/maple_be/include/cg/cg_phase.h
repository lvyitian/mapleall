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

#ifndef MAPLECG_INCLUDE_CGPHASE_H
#define MAPLECG_INCLUDE_CGPHASE_H

#include <string>
#include <map>
#include "phase.h"
#include "module_phase.h"
#include "cg_assert.h"

using namespace maple;

namespace maplebe {
enum CgPhaseID {
  kCgPhaseDonothing,
#define FUNCAPHASE(CGPHASEID, CLASSNACG) CGPHASEID,
#define FUNCTPHASE(CGPHASEID, CLASSNACG) CGPHASEID,
#include "cg_phases.def"
#undef FUNCAPHASE
#undef FUNCTPHASE
  kCgPhaseMax
};

class FuncPhase;
class CGFunc;

typedef AnalysisResultManager<maplebe::CGFunc, CgPhaseID, FuncPhase> CgFuncResultMgr;

class FuncPhase : public Phase {
 private:
  CgPhaseID phaseID;
  std::string prevPhaseName; /*used in filename for emit*/
 public:
  FuncPhase(CgPhaseID id) : Phase(), phaseID(id) {}

  ~FuncPhase(){}

  /* if phase is analysis phase, return analysis result
   * else return nullptr */
  virtual AnalysisResult *Run(maplebe::CGFunc *ir, CgFuncResultMgr *frm) {
    CG_ASSERT(false, "should not be here");
    return nullptr;
  }

  std::string GetPreviousPhaseName() const {
    return prevPhaseName;
  }

  void SetPreviousPhaseName(std::string phaseName) {
    prevPhaseName = phaseName;
  }

  CgPhaseID GetPhaseId() const {
    return phaseID;
  }

  virtual bool CanSkip() {
    return false;
  }
};
}  // namespace maplebe

#define CGFUNCPHASE(CLASSNAME, PHASENAME)                             \
  class CLASSNAME : public FuncPhase {                                \
   public:                                                            \
    CLASSNAME(CgPhaseID id) : FuncPhase(id) {}                        \
    AnalysisResult *Run(CGFunc *cgfunc, CgFuncResultMgr *m) override; \
    std::string PhaseName() const override {                                \
      return PHASENAME;                                               \
    }                                                                 \
  };
#define CGFUNCPHASE_CANSKIP(CLASSNAME, PHASENAME)                     \
  class CLASSNAME : public FuncPhase {                                \
   public:                                                            \
    CLASSNAME(CgPhaseID id) : FuncPhase(id) {}                        \
    AnalysisResult *Run(CGFunc *cgfunc, CgFuncResultMgr *m) override; \
    std::string PhaseName() const override {                                \
      return PHASENAME;                                               \
    }                                                                 \
    bool CanSkip() override {                                         \
      return true;                                                    \
    }                                                                 \
  };
#endif  // MAPLECG_IMCLUDE_CGPHASE_H
