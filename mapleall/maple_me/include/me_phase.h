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

#ifndef MAPLE_ME_INCLUDE_ME_PHASE_H
#define MAPLE_ME_INCLUDE_ME_PHASE_H

#include <string>
#include <map>
#include "phase.h"
#include "module_phase.h"

namespace maple {

enum MePhaseID {
  kMePhaseDonothing,
#define FUNCAPHASE(MEPHASEID, CLASSNAME) MEPHASEID,
#define FUNCTPHASE(MEPHASEID, CLASSNAME) MEPHASEID,
#include "me_phases.def"
#undef FUNCAPHASE
#undef FUNCTPHASE
  kMePhaseMax
};

class MeFuncPhase;
class MeFunction;

typedef AnalysisResultManager<MeFunction, MePhaseID, MeFuncPhase> MeFuncResultMgr;

class MeFuncPhase : public Phase {
 public:
  MeFuncPhase(MePhaseID id) : Phase() {
    phaseID = id;
  }

  /* if phase is analysis phase, return analysis result
   * else return nullptr */
  virtual AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *frm) {
    ASSERT(false, "should not be here");
    return nullptr;
  }

  // By default mrm will not be used because most ME phases do not need IPA
  // result. For those will use IPA result, this function will be overrode.
  virtual AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *frm, ModuleResultMgr *mrm) {
    return Run(ir, frm);
  }

  std::string GetPreviousPhaseName() const {
    return prevPhaseName;
  }

  void SetPreviousPhaseName(std::string phaseName) {
    prevPhaseName = phaseName;
  }

  MePhaseID GetPhaseId() const {
    return phaseID;
  }

  virtual std::string PhaseName() const = 0;

  virtual ~MeFuncPhase(){};

 private:
  MePhaseID phaseID;
  std::string prevPhaseName; /* used in filename for emit */
};
}  // namespace maple
#endif  // MAPLE_ME_IMCLUDE_ME_PHASE_H
