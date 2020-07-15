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

#ifndef MAPLE_ME_INCLUDE_IPA_SIDE_EFFECT_H
#define MAPLE_ME_INCLUDE_IPA_SIDE_EFFECT_H
#include "me_phase.h"
#include "callgraph.h"
#include "mir_nodes.h"
#include "mir_builder.h"
#include "me_ir.h"

namespace maple {

class BB;
class Dominance;
class VersionSt;
class MeFunction;

/* Table with pre-compiled library names and their side effects. */
class FuncWithSideEffect {
 public:
  FuncWithSideEffect(std::string name, bool p, bool u, bool d, bool o, bool e)
    : funcName(name), pure(p), use(u), def(d), object(o), exception(e), privateUse(false), privateDef(false){};
  std::string GetFuncName() {
    return funcName;
  }

  bool GetPure() const {
    return pure;
  }

  bool GetUse() const {
    return use;
  }

  bool GetDef() const {
    return def;
  }

  bool GetObject() const {
    return object;
  }

  bool GetException() const {
    return exception;
  }

  bool GetPrivateUse() const {
    return privateUse;
  }

  bool GetPrivateDef() const {
    return privateDef;
  }

 private:
  std::string funcName;
  bool pure;
  bool use;
  bool def;
  bool object;
  bool exception;
  bool privateUse;
  bool privateDef;
};

// SCC side effects mask position
enum SCCEffectPosition : uint8 {
    kHasThrow = 0x01,
    kHasObj = 0x02,
    kHasDef = 0x04,
    kHasUse = 0x08,
    kPure = 0x10,
    KNotPure = kPure,  // Same position as kPure, depending on usage.
    kHasPrivateUse = 0x20,
    kHasPrivateDef = 0x40
};

class IpaSideEffect {
 private:
  bool notPure;          // Does module produce result purely based on the inputs
  bool hasUse;           // Has a use of a field of any pre-existing object
  bool hasDef;           // Has a definition of a field of any pre-existing object
  bool hasRetallocobj;   // Returns freshly allocated object
  bool hasThrexception;  // throws an exception
  bool hasPrivateUse;    // access field is use and private
  bool hasPrivateDef;    // access field is def and private

  MeFunction *mefunc;
  MapleAllocator alloc;
  CallGraph *callgraph;
  uint32 sccId;
  Dominance *dominance;

 public:
  IpaSideEffect(MeFunction *, MemPool *, CallGraph *, Dominance *);
  void InitCGNodeSccIdMap();
  void DoAnalysis();

 private:
  bool IsIgnoreMethod(const MIRFunction *func);
  void AnalyzeUseThrowEhExpr(MeExpr *expr);
  bool AnalyzeReturnAllocObj(MeExpr *expr, std::vector<MeExpr *>);
  //  void SideEffectAnalyzeFunction (MIRFunction *);
  void SideEffectAnalyzeBB(BB *, std::vector<bool> &);
  void SetEffectsForAllCallees(MIRFunction *baseFunc);
  bool AnalyzeReturnAllocObjVst(MeExpr *, std::vector<MeExpr *>);
  bool MatchPuidxAndSetSideEffects(PUIdx idx);
  void MapFuncNameToPuidx();
  void SetEffectsTrue();
  void CopySccSideEffectToAllFunctions(MIRFunction *f, uint8 seMask);
  void SetFuncCalleeSideEffects(MIRFunction *callee, const MIRFunction *caller);
  void DumpFuncInfo(const std::string msg, const std::string name = std::string());
  void TrimCallGraph(maple::BB *bb, vector<bool> &bbvisited);
  uint32 GetSCCNodeId(MIRFunction *f);
  bool IsCallingIntoSCC(uint32 sccid) const;
  void UpdateExternalFuncSideEffects(MIRFunction *externCaller);
  bool AnalyzeDefExpr(VersionSt *, std::vector<VersionSt *> &);
  bool MEAnalyzeDefExpr(MeExpr *, std::vector<MeExpr *> &);
};

class DoIpaSideEffect : public MeFuncPhase {
 public:
  DoIpaSideEffect(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *, MeFuncResultMgr *, ModuleResultMgr *) override;
  AnalysisResult *Run(MeFunction *ir, MeFuncResultMgr *m) override {
    CHECK_FATAL(false, "should not be here");
    return nullptr;
  }

  std::string PhaseName() const override {
    return "sideeffect";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_IPA_SIDE_EFFECT_H
