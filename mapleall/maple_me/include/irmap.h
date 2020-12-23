/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
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

#ifndef MAPLE_ME_INCLUDE_IRMAP_H
#define MAPLE_ME_INCLUDE_IRMAP_H

#include "bb.h"
#include "ver_symbol.h"
#include "ssa_tab.h"
#include "me_ir.h"
#include "dominance.h"

namespace maple {

class IRMap : public AnalysisResult {
 public:
  SSATab *ssaTab;
  MIRModule *mirModule;
  Dominance *dominance;
  MapleAllocator irMapAlloc;
  MapleAllocator tempAlloc;
  int32 exprID;                                       // for allocating exprID in MeExpr
  uint32 mapHashLength;                             // size of hashTable
  MapleVector<MeExpr *> hashTable;                    // the value number hash table
  MapleVector<MeExpr *> verst2MeExprTable;             // map versionst to MeExpr.
  MapleUnorderedMap<OStIdx, RegMeExpr *> lpreTemps;  // for passing LPRE's temp usage to SPRE
  bool dumpStmtNum = false;

  IRMap(SSATab *ssaTab, Dominance *dom, MemPool *mp, MemPool *tempmp, uint32 hashTableSize)
    : AnalysisResult(mp),
      ssaTab(ssaTab),
      mirModule(&ssaTab->mirModule),
      dominance(dom),
      irMapAlloc(mp),
      tempAlloc(tempmp),
      exprID(1),        // starts from 1
      mapHashLength(hashTableSize),
      hashTable(mapHashLength, nullptr, irMapAlloc.Adapter()),
      verst2MeExprTable(ssaTab->versionStTable.Size(), nullptr, irMapAlloc.Adapter()),
      lpreTemps(irMapAlloc.Adapter()) {}

  virtual ~IRMap() {}

  virtual BB *GetBB(BBId id) = 0;
  virtual BB *GetBBForLabidx(LabelIdx lidx, PUIdx pidx = 0) = 0;
  Dominance *GetDominance() {
    return dominance;
  }

  MeExpr *HashMeExpr(MeExpr *meexpr);
  void PutToBucket(uint32, MeExpr *);
  void BuildAssertMeStmt(NaryMeExpr *);
  IvarMeExpr *BuildLhsIvar(MeExpr *baseaddr, IassignMeStmt *iassmestmt, FieldID fieldID);
  IvarMeExpr *BuildLhsIvarFromIassMeStmt(IassignMeStmt *iassmestmt);
  MeExpr *ReplaceMeExprExpr(MeExpr *, MeExpr *, MeExpr *);
  bool ReplaceMeExprStmt(MeStmt *, MeExpr *, MeExpr *);
  MeExpr *GetMeExprByVerId(uint32 verid) {
    return verst2MeExprTable[verid];
  }

  MeExpr *GetMeExpr(uint32 indx) {
    ASSERT(indx < verst2MeExprTable.size(), "");
    MeExpr *meexpr = verst2MeExprTable.at(indx);
    if (meexpr) {
      ASSERT(meexpr->meOp == kMeOpVar || meexpr->meOp == kMeOpReg, "");
    }
    return meexpr;
  }

  // for creating VarMeExpr
  VarMeExpr *CreateVarMeExprVersion(OriginalSt *ost);
  VarMeExpr *CreateVarMeExprVersion(const VarMeExpr *varx) {
    return CreateVarMeExprVersion(varx->ost);
  }
  VarMeExpr *GetOrCreateZeroVersionVarMeExpr(OriginalSt *ost);
  VarMeExpr *CreateNewVar(GStrIdx strIdx, PrimType primType, bool isGlobal);
  VarMeExpr *CreateNewLocalrefvarTemp(GStrIdx strIdx, TyIdx tidx);

  // for creating RegMeExpr
  RegMeExpr *CreateRegMeExprVersion(OriginalSt *ost);
  RegMeExpr *CreateRegMeExprVersion(const RegMeExpr *regx) {
    return CreateRegMeExprVersion(regx->ost);
  }
  RegMeExpr *CreateRegMeExpr(PrimType primType);
  RegMeExpr *CreateRegMeExpr(MIRType *mirType);
  RegMeExpr *CreateRegMeExpr(MeExpr *meexpr) {
    MIRType *mirType = meexpr->GetType();
    if (mirType == nullptr || mirType->primType == PTY_agg) {
      return CreateRegMeExpr(meexpr->primType);
    }
    return CreateRegMeExpr(mirType);
  }

  MeExpr *CreateAddrofMeExpr(MeExpr *);

  AssignMeStmt *CreateAssignMeStmt(ScalarMeExpr *, MeExpr *, BB *);
  void InsertMeStmtBefore(BB *, MeStmt *, MeStmt *);

  MePhiNode *CreateMePhi(ScalarMeExpr *);
  bool Verify();  // iterate hash table and check with meexpr_table
  BB *GetFalseBrBb(CondGotoMeStmt *);
  std::string PhaseName() const {
    return "irMap";
  }

  void DumpBB(BB *bb) {
    int i = 0;
    for (auto mestmt : bb->meStmtList) {
      if (dumpStmtNum) {
        LogInfo::MapleLogger() << "(" << i++ << ") ";
      }
      mestmt->Dump(this);
    }
  }
  virtual void Dump() = 0;
  virtual void SetCurFunction(BB *bb) {}

  MeExpr *CreateIntConstMeExpr(int64, PrimType);
  MeExpr *CreateConstMeExpr(PrimType, MIRConst *);
  MeExpr *CreateMeExprUnary(Opcode, PrimType, MeExpr *);
  MeExpr *CreateMeExprBinary(Opcode, PrimType, MeExpr *, MeExpr *);
  MeExpr *CreateMeExprSelect(PrimType, MeExpr *, MeExpr *, MeExpr *);
  MeExpr *CreateMeExprCompare(Opcode, PrimType, PrimType, MeExpr *, MeExpr *);
  MeExpr *CreateMeExprIntrinsiciop1(MIRIntrinsicID, PrimType, MeExpr *);
  MeExpr *CreateMeExprTypeCvt(PrimType, PrimType, MeExpr *);
  IntrinsiccallMeStmt *CreateIntrinsicCallMeStmt(MIRIntrinsicID idx, std::vector<MeExpr*> &opnds,
                                                 TyIdx tyidx = TyIdx());
  IntrinsiccallMeStmt *CreateIntrinsicCallAssignedMeStmt(MIRIntrinsicID idx, std::vector<MeExpr*> &opnds, MeExpr *ret,
                                                         TyIdx tyidx = TyIdx());
  MeExpr *CreateAddrofMeExprFromNewSymbol(MIRSymbol *, PUIdx);

  MeExpr *SimplifyOpMeExpr(OpMeExpr *opmeexpr);
  MeExpr *SimplifyMeExpr(MeExpr *opmeexpr);

  template <class T, typename... Arguments>
  inline T *NewInPool(Arguments... args) {
    return irMapAlloc.GetMemPool()->New<T>(&irMapAlloc, args...);
  }

  template <class T, typename... Arguments>
  inline T *New(Arguments... args) {
    return irMapAlloc.GetMemPool()->New<T>(args...);
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_IRMAP_H
