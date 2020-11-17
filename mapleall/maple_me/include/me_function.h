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

#ifndef MAPLE_ME_INCLUDE_ME_FUNCTION_H
#define MAPLE_ME_INCLUDE_ME_FUNCTION_H
#include "mir_parser.h"
#include "mir_function.h"
#include "opcode_info.h"
#include "me_option.h"
#include "me_phase.h"
#include "mempool.h"
#include "mempool_allocator.h"
#include "ver_symbol.h"
#include "bb.h"
#include "ssa_tab.h"
#include "func_emit.h"
#include "me_ir.h"

namespace maple {

class MirCFG;
class MeIRMap;

#if DEBUG
extern MIRModule *g_mirmodule;
extern MeFunction *g_func;
extern MeIRMap *g_irmap;
extern SSATab *g_ssatab;
#endif

class MeFunction : public FuncEmit {
 public:
  MemPool *memPool;
  MapleAllocator alloc;
  MemPool *versMemPool;
  MapleAllocator versAlloc;
  MIRModule &mirModule;
  MIRFunction *mirFunc;
  BB *first_bb_;
  BB *last_bb_;
  BB *commonEntryBB;
  BB *commonExitBB;
  uint32 nextBBId;
  /* mempool */
  MapleUnorderedMap<LabelIdx, BB *> labelBBIdMap;
  MapleVector<BB *> bbVec;
  MirCFG *theCFG;
  SSATab *meSSATab;
  MeIRMap *irMap;
  MapleUnorderedMap<BB *, StmtNode *> bbTryNodeMap;  // maps isTry bb to its try stmt
  MapleUnorderedMap<BB *, BB *> endTryBB2TryBB;           // maps endtry bb to its try bb
  /* input */
  std::string fileName;

  uint32 regNum;   // count virtual registers
  bool hasEH;      /* current has try statement */
  bool secondPass;  // second pass for the same function
  bool isLfo;
  bool placementRCOn;   // whethering doing placement RC

  explicit MeFunction(MIRModule *mod, MIRFunction *func, MemPool *mp, MemPool *versmp, const std::string &fileName,
                      bool issecondpass = false, bool islfo = false)
    : memPool(mp),
      alloc(mp),
      versMemPool(versmp),
      versAlloc(versmp),
      mirModule(*mod),
      mirFunc(func),
      first_bb_(nullptr),
      last_bb_(nullptr),
      commonEntryBB(nullptr),
      commonExitBB(nullptr),
      nextBBId(0),
      labelBBIdMap(alloc.Adapter()),
      bbVec(alloc.Adapter()),
      meSSATab(nullptr),
      bbTryNodeMap(alloc.Adapter()),
      endTryBB2TryBB(alloc.Adapter()),
      fileName(fileName) {
    PartialInit(issecondpass);
    isLfo = islfo;
  }

  virtual ~MeFunction() {}

  uint32 NumBBs(void) const {
    return nextBBId;
  }

  void DumpFunction();
  void DumpFunctionNoSSA();
  void DumpMeFunc();
  void DumpMayDUFunction();
  virtual void Prepare(unsigned long rangeNum);
  void Verify();
  const std::string &GetName() const {
    return mirModule.CurFunction()->GetName();
  }

  BB *NewBasicBlock();
  void DeleteBasicBlock(const BB *bb);
  BB *NextBB(const BB *bb);
  BB *PrevBB(const BB *bb);

  /* create label for bb */
  void CreateBBLabel(BB *bb);
  /* clone stmtnodes from orig to newbb */
  void CloneBasicBlock(BB *newbb, BB *orig);
  BB *SplitBB(BB *bb, StmtNode *splitPoint);

  const bool HasException() const {
    return hasEH;
  }

  void SetSecondPass() {
    secondPass = true;
  }

  bool IsSecondPass() const {
    return secondPass;
  }

  void CreateBasicBlocks(MirCFG *cfg);
  void RemoveEhEdgesInSyncRegion();

 private:
  void PartialInit(bool issecondpass);
  void SetTryBlockInfo(StmtNode *javatryStmt, BB *lastjavatryBb, const StmtNode *nextstmt, BB *curbb, BB *newbb);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_FUNCTION_H
