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

#ifndef REACHINGDEFINITION_H
#define REACHINGDEFINITION_H

#include "cg_func.h"
#include "cg_phase.h"
#include "live_analysis.h"
#include "loop.h"
#include <set>

namespace maplebe {

using namespace std;

#define RD_REGANALYSIS 0x1
#define RD_MEMANALYSIS 0x2
#define RD_ALLANALYSIS 0x3

#define DUMPALL 0xFFFF0
#define DUMPUD 0x10
#define DUMPDU 0x20
#define DUMPDUUD 0x30
#define DUMPGEN 0x100
#define DUMPKILL 0x200
#define DUMPGENKILL 0x300
#define DUMPIN 0x1000
#define DUMPOUT 0x2000
#define DUMPINOUT 0x3000
#define DUMPLASTUSE 0x4000
#define DUMPGENKILLINOUTLASTUSE 0x7300
#define DUMPWAWDEPENDENCE 0x10000

struct BBIdCmp {
  bool operator()(const BB *lhs, const BB *rhs) const {
    return lhs->id < rhs->id;
  }
};

class ReachingDefinition : public AnalysisResult {
 public:
  void AnalysisStart(int mode, CgFuncResultMgr *m);
  void AnalysisFinish() const {
    // mempoolctrler.DeleteMemPool(memPool);
  }

  void Dump(int);
  void DumpGen(BB *);
  void DumpKill(BB *);
  void DumpIn(BB *);
  void DumpOut(BB *);
  void DumpLastuse(BB *bb);
  virtual void DumpUDChain(BB *bb) {}

  virtual void DumpDUChain(BB *bb) {}

  virtual void DumpWAWDependence(BB *bb) {}

  void ClearAll();
  void ClearTempInfo();
  void ClearDefUseInfo();

 protected:
  // int iteration;
  CGFunc *cgfunc;
  MemPool *mp;             // For allocate defs/uses of insn.
  MapleAllocator rdalloc;  // For allocate defs/uses of insn.
  LiveAnalysis *live;
  set<Insn *> pseudoInsns;

 public:
  explicit ReachingDefinition(CGFunc *func, MemPool *mp, MapleAllocator ma, LiveAnalysis *la)
    : AnalysisResult(mp), cgfunc(func), mp(mp), rdalloc(ma), live(la), pseudoInsns() {}

  ~ReachingDefinition() {}

  void InitBB(BB *bb, int mode);
  void GenLastUse(BB *bb, int mode);
  void InitOut(BB *bb);
  bool GenerateIn(BB *bb, set<DataAnalysisInfo, DataAnalysisInfoCmp> &setChangedDataInfo);
  bool GenerateOut(BB *bb, set<DataAnalysisInfo, DataAnalysisInfoCmp> &setChangedDataInfo);
  void DirtyAllBBOut(BB *bb);
  void DirtyAllStackMemGen(BB *bb);
  void DirtyAllStackMemOnBBIn(BB *bb);
  bool CheckMultigenPossibility(const BB *currBB, const BB *prevBB, const DataAnalysisInfo &dataInfo);
  void DefineRegister(BB *, RegOperand *, Insn *, short index, int prop = 0);
  void DefineRegisterOnBBIn(BB *, Operand *, Insn *, short index, int prop = 0);
  void DefineMem(BB *, Operand *, Insn *, short index, int prop = 0);
  void DefineMemOnBBIn(BB *, Operand *, Insn *, short index, int prop = 0);
  bool GenerateInForFirstCleanupBB(BB *firstCleanupBB, const set<BB *, BBIdCmp> &bbNormalSet,
                                   const set<regno_t> &regnoSet,
                                   set<DataAnalysisInfo, DataAnalysisInfoCmp> &setChangedDataInfo);
  MemPool *GetMemPool() const {
    return mp;
  }

  MapleAllocator &GetMemAllocator() {
    return rdalloc;
  }

  bool IsFrameReg(Operand *opnd);
  void GenerateUDChain(int mode);
  void GenerateDUChain();
  void AdjustLastUse(int mode);

 public:
  virtual void InitStartGen() {}

  virtual void InitEhDefine(BB *bb) {}

  virtual void InitKillGen(BB *bb, int mode){};
  virtual bool MayHasThrow(const Insn *headInsn, const Insn *tailInsn) {
    return false;
  }

  virtual void GenerateUseDef(BB *bb, int mode) {}

  virtual void GenerateDefUse(BB *bb) {}

  virtual void DirtyAllNonStackMem(BB *bb) {}

  virtual void KillAllCallerSavedRegs(BB *bb, Insn *callInsn) {}

  virtual void GenReturnValue(BB *bb, Insn *insn) {}

  virtual void KillAllCallerSavedRegsOnBBIn(BB *bb, Insn *callInsn) {}

  virtual void GenReturnValueOnBBIn(BB *bb, Insn *insn) {}

  virtual void InsertUseToDef(DataInsnInfo &insnInfo, Insn *useInsn, short index, short userProperty) {}

  virtual void AddRetPseudoInsn(BB *bb) {}

  virtual void AddRetPseudoInsns() {}

  virtual void RemoveDUUDForInsn(Insn *insn) {}

  virtual void InsertDUUDForDestOperand(Insn *newInsn, short newIndex, unsigned short newProp, Insn *refInsn,
                                        short refIndex, unsigned short refProp, bool isRefInsnBeforeNewInsn) {}

  virtual void InsertDUUDForSrcOperand(Insn *newInsn, short newIndex, unsigned short newProp, Insn *refInsn,
                                       short refIndex, unsigned short refProp) {}

  virtual void ReplaceInsnSrcOperand(Insn *insn, short index, unsigned short prop, Operand *newOpnd, Insn *refInsn,
                                     short refIndex, unsigned short refProp) {}

  virtual void ReplaceInsnDestOperand(Insn *insn, short index, unsigned short prop, Operand *newOpnd, Insn *refInsn,
                                      short refIndex, unsigned short refProp, bool isRefInsnBeforeInsn) {}
};

CGFUNCPHASE(CgDoReachingDefinition, "reachingdefinition")

}  // namespace maplebe

#endif /* REACHINGDEFINITION_H */
