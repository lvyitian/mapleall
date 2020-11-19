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
  MIRModule &mirModule;
  MIRFunction *mirFunc;
  MirCFG *theCFG;
  SSATab *meSSATab;
  MeIRMap *irMap;
  std::string fileName;

  uint32 regNum;   // count virtual registers
  bool hasEH;      /* current has try statement */
  bool secondPass;  // second pass for the same function
  bool isLfo;
  bool placementRCOn;   // whethering doing placement RC

  explicit MeFunction(MIRModule *mod, MIRFunction *func, const std::string &fileName,
                      bool issecondpass, bool islfo)
    : mirModule(*mod),
      mirFunc(func),
      theCFG(nullptr),
      meSSATab(nullptr),
      irMap(nullptr),
      fileName(fileName),
      regNum(0),
      hasEH(false),
      secondPass(issecondpass),
      isLfo(islfo),
      placementRCOn(false) {
    if (mod->IsJavaModule() && (mirFunc->info.size() > 0)) {
      // set regNum
      std::string string("INFO_registers");
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(string);
      regNum = mirModule.CurFunction()->GetInfo(strIdx);
      // set hasEH
      std::string trynum("INFO_tries_size");
      strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(trynum);
      uint32 num = mirModule.CurFunction()->GetInfo(strIdx);
      hasEH = (num != 0);
    }
  }

  virtual ~MeFunction() {}

  void DumpFunction();
  void DumpFunctionNoSSA();
  void DumpMeFunc();
  void DumpMayDUFunction();
  void Verify();
  const std::string &GetName() const {
    return mirModule.CurFunction()->GetName();
  }

  BB *NewBasicBlock();

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
  void SetTryBlockInfo(StmtNode *javatryStmt, BB *lastjavatryBb, const StmtNode *nextstmt, BB *curbb, BB *newbb);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_FUNCTION_H
