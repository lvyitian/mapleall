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

#ifndef MAPLE_ME_INCLUDE_ME_RC_LOWERING_H
#define MAPLE_ME_INCLUDE_ME_RC_LOWERING_H

#include "class_hierarchy.h"
#include "me_function.h"
#include "me_irmap.h"
#include "me_phase.h"
#include "mir_builder.h"

#define RC_INTRINSICS(V)                     \
  V(MCCIncRef, kIncRef)                      \
  V(MCCDecRef, kDecRef)                      \
  V(MCCDecRefReset, kDecReset)               \
  V(MCCLoadRefSVol, kLoadVolStatic)          \
  V(MCCLoadRefS, kLoadStatic)                \
  V(MCCIncDecRef, kIncDec)                   \
  V(MCCLoadWeakVol, kLoadVolWeak)            \
  V(MCCLoadWeak, kLoadWeak)                  \
  V(MCCLoadRef, kLoadRef)                    \
  V(MCCLoadRefVol, kLoadVol)                 \
  V(MCCWriteReferent, kWriteReferent)        \
  V(MCCWriteSVolNoInc, kWriteVolStaticNoInc) \
  V(MCCWriteSVolNoDec, kWriteVolStaticNoDec) \
  V(MCCWriteSVolNoRC, kWriteVolStaticNoRc)   \
  V(MCCWriteSVol, kWriteVolStatic)           \
  V(MCCWriteSNoInc, kWriteStaticNoInc)       \
  V(MCCWriteSNoDec, kWriteStaticNoDec)       \
  V(MCCWriteSNoRC, kWriteStaticNoRc)         \
  V(MCCWriteS, kWriteStatic)                 \
  V(MCCWriteVolNoInc, kWriteVolNoInc)        \
  V(MCCWriteVolNoDec, kWriteVolNoDec)        \
  V(MCCWriteVolNoRC, kWriteVolNoRc)          \
  V(MCCWriteVol, kWriteVol)                  \
  V(MCCWriteNoInc, kWriteNoInc)              \
  V(MCCWriteNoDec, kWriteNoDec)              \
  V(MCCWriteNoRC, kWriteNoRc)                \
  V(MCCWrite, kWrite)                        \
  V(MCCWriteVolWeak, kWriteVolWeak)          \
  V(MCCWriteWeak, kWriteWeak)

namespace maple {
enum MrtRcCall {
#define GETINDEX(name, index) index,
  RC_INTRINSICS(GETINDEX)
#undef GETINDEX
  kRcCallTotal,
  kRcCallOneArg = kLoadStatic,      // last function with one argument
  kRcCallOneTwoArg = kWriteStatic,  // last function with two arguments
  kRCReserved
};

class RCLowering {
 public:
  RCLowering(MeFunction *f, KlassHierarchy *kh)
      : func(f),
      mirModule(&f->mirModule),
      irMap(f->irMap),
      ssaTab(f->meSSATab),
      lastTempReg(nullptr),
      klassh(kh) {}

  std::set<MIRSymbol*> assignedPtrSym;
  std::set<VarMeExpr*> tempLocalRefVars;
  bool rcCheckReferent = false;

  void PreBBLower();
  void EpreFixup(BB *bb);
  void RCLower(BB *bb);
  void FastRCLower(BB *bb);
  void PostBBLower();
  std::string PhaseName() const {
    return "rclowering";
  }

 private:
  MeFunction *func;
  MIRModule *mirModule;
  IRMap *irMap;
  SSATab *ssaTab;
  std::vector<MeStmt*> rets;  // vector of return statement
  RegMeExpr *lastTempReg;
  unsigned tempCount = 0;
  bool globalWrite = false;
  KlassHierarchy *klassh;

  void InitRCFunc();
  void CompactRC(BB *bb);
  // create new symbol from name and return its ost
  OriginalSt *RetrieveOst(const char *name, bool isLocalrefvar) const;
  // create new symbol from tempName and return its VarMeExpr
  // new symbols are stored in a set
  VarMeExpr *GetTemp(const char *tempName, bool isLocalrefvar);
  VarMeExpr *GetMeExprForNewTemp(bool isLocalrefvar);
  // create new reg temp and update lastTempReg
  RegMeExpr *GetRegTemp(PrimType primType) {
    lastTempReg = irMap->CreateRegMeExpr(primType);
    return lastTempReg;
  }
  VarMeExpr *GetMeExprFromSym(MIRSymbol *sym) const;
  // return true if the rhs is simple so we can adjust RC count before assignments
  bool RCFirst(MeExpr *rhs);
  bool HandleSpecialRHS(MeStmt *stmt);
  MrtRcCall PrepareVolatileCall(MeStmt *stmt, MrtRcCall index = kRCReserved);
  IntrinsiccallMeStmt *CreateRCIntrinsic(MrtRcCall index, MeStmt *stmt, bool assigned);
  IntrinsiccallMeStmt *CreateIntrinsicWithOneArg(MrtRcCall index, MeStmt *stmt,
                                                 MeExpr *arg0, bool assigned = false);
  IntrinsiccallMeStmt *CreateIntrinsicWithTwoArg(MrtRcCall index, MeStmt *stmt,
                                                 MeExpr *arg0, MeExpr *arg1, bool assigned = false);
  IntrinsiccallMeStmt *CreateIntrinsicWithThreeArg(MrtRcCall index, MeStmt *stmt,
                                                   MeExpr *arg0, MeExpr *arg1, MeExpr *arg2, bool assigned = false);
  void HandleReturnDecRef(MeStmt *stmt, MeExpr *pendingDec, BB *bb);
};

class MeDoRCLowering : public MeFuncPhase {
 public:
  MeDoRCLowering(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *, MeFuncResultMgr *, ModuleResultMgr *) override;
  std::string PhaseName() const override {
    return "rclowering";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_RC_LOWERING_H
