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

#ifndef MAPLEBE_INCLUDE_CG_CG_H
#define MAPLEBE_INCLUDE_CG_CG_H

#include "operand.h"
#include "insn.h"
#include "cg_func.h"
#include "reg_alloc.h"
#include "cg_assert.h"
#include "live_analysis.h"
#include "cg_option.h"
#include "optimize_common.h"
/// MapleIR headers.
#include "opcode_info.h"
#include "global_tables.h"
#include "mir_function.h"
#include "mad.h"

/// C++ headers.
#include <cstddef>
#include <utility>
#include <cstdio>
#include <string>


namespace maple {
class MIRModule;
class MIRParser;
};  // namespace maple

namespace maplebe {

class BECommon;
class Emitter;

struct Globals {
  const OpcodeTable *kOpcodeInfo;
  GlobalTables *globaltable;
  MIRParser *parser;
  BECommon *becommon;
  MAD *mad;
  int optim_level;

  Globals(const OpcodeTable *oi, GlobalTables *gt)
    : kOpcodeInfo(oi), globaltable(gt), parser(nullptr), becommon(nullptr), mad(nullptr), optim_level(0) {}

  ~Globals() {}
};

extern Globals *g;

class CG {
 public:
  typedef uint64_t generate_flag_t;
  static const char *kCMacroDefSuffix;
  static const char *kGctibSuffix;
  static const char *kGrootListSuffix;
  static const char *kPrimorListSuffix;

 public:
  MIRModule *mirModule;
  Emitter *emitter_;
  LabelIDOrder label_order_cnt_;
  uint64_t label_debug_index;
  static CGFunc *curCgFunc;  // current cg function being compiled
  static PUIdx curPuIdx;
  CGOptions cgopt_;
  bool run_cg_;
  MIRSymbol *instru_func_;
  MIRSymbol *dbg_trace_enter_;
  MIRSymbol *dbg_trace_exit_;
  MIRSymbol *dbg_func_profile_;

 public:
  explicit CG(MIRModule *mod, const CGOptions &cgopt, bool runCg, const char *output)
    : mirModule(mod),
      emitter_(nullptr),
      label_order_cnt_(0),
      label_debug_index(0),
      cgopt_(cgopt),
      run_cg_(false),
      instru_func_(nullptr) {
    DefineDebugTraceFunctions();
  }

  virtual ~CG() {
    // delete emitter_;
  }

  void GenFieldOffsetMap(const std::string &classListFileName);
  void GenExtraTypeMetadata(const std::string &classListFileName, const std::string &outputBasename);
  void GenGlobalRootList(const std::string &outputBasename);
  void GenPrimordialObjectList(const std::string &outputBasename);
  void LowerIR();
  void GenerateCode(std::string &fileName, std::string &asmFileName);

  template <typename I>
  inline Insn *BuildInstruction(MOperator opcode) {
    curCgFunc->IncTotalNumberOfInstructions();
    return curCgFunc->memPool->New<I>(opcode);
  }

  template <typename I>
  inline Insn *BuildInstruction(MOperator opcode, Operand *opnd0) {
    curCgFunc->IncTotalNumberOfInstructions();
    return curCgFunc->memPool->New<I>(opcode, opnd0);
  }

  template <typename I>
  inline Insn *BuildInstruction(MOperator opcode, Operand *opnd0, Operand *opnd1) {
    curCgFunc->IncTotalNumberOfInstructions();
    return curCgFunc->memPool->New<I>(opcode, opnd0, opnd1);
  }

  template <typename I>
  inline Insn *BuildInstruction(MOperator opcode, Operand *opnd0, Operand *opnd1, Operand *opnd2) {
    curCgFunc->IncTotalNumberOfInstructions();
    return curCgFunc->memPool->New<I>(opcode, opnd0, opnd1, opnd2);
  }

  template <typename I>
  inline Insn *BuildInstruction(MOperator opcode, Operand *opnd0, Operand *opnd1, Operand *opnd2, Operand *opnd3) {
    curCgFunc->IncTotalNumberOfInstructions();
    return curCgFunc->memPool->New<I>(opcode, opnd0, opnd1, opnd2, opnd3);
  }

  template <typename I>
  inline Insn *BuildInstruction(MOperator opcode, Operand *opnd0, Operand *opnd1, Operand *opnd2, Operand *opnd3,
                                Operand *opnd4) {
    curCgFunc->IncTotalNumberOfInstructions();
    return curCgFunc->memPool->New<I>(opcode, opnd0, opnd1, opnd2, opnd3, opnd4);
  }

  virtual Insn *CreateGhostInstruction() = 0;

  virtual CGFunc *CreateCGFunc(MIRModule *mod, MIRFunction *, BECommon &, MemPool *, MapleAllocator *) = 0;

  virtual InsnVisitor *NewInsnModifier(CGFunc *cgfunc, MemPool *mp) = 0;

  bool IsExclusiveEH() const {
    return CGOptions::exclusiveEh;
  }

  bool DoItQuietly() const {
    return CGOptions::quiet;
  }

  virtual bool IsExclusiveFunc(MIRFunction *mirFunc) {
    return false;
  }

  inline bool UseFP() {
    return cgopt_.UseFP();
  }

  inline bool GenerateCfiDirectives() const {
    return CGOptions::genCfi;
  }

  inline bool ConvertLocalsToPRegs() {
    return cgopt_.ConvertLocalsToPRegs();
  }

  // NOTE: Consider making be_common a field of CG.
  virtual void GenerateObjectMaps(BECommon &becommon);

  // Used for GCTIB pattern merging
  virtual std::string FindGCTIBPatternName(const std::string &name);

  inline bool GenerateVerboseAsm() {
    return cgopt_.GenerateVerboseAsm();
  }

  inline bool GenerateTestInfo() {
    return cgopt_.GenerateTestInfo();
  }

  inline bool DoPrologueEpilogue() {
    return cgopt_.DoPrologueEpilogue();
  }

  inline bool DoCheckSOE() {
    return cgopt_.DoCheckSOE();
  }

  inline bool GenerateDebugFriendlyCode() {
    return cgopt_.GenerateDebugFriendlyCode();
  }

  inline bool UseFastUnwind() const {
    return true;
  }

  inline bool UsenewFastUnwind() const {
    return true;
  }

  inline bool AddStackGuard() {
    return cgopt_.AddStackGuard();
  }

  inline bool NeedInsertInstrumentationFunction() {
    return cgopt_.NeedInsertInstrumentationFunction();
  }

  void SetInstrumentationFunction(std::string name);
  inline MIRSymbol *GetInstrumentationFunction() {
    return instru_func_;
  }

  inline bool InstrumentWithDebugTraceCall() {
    return cgopt_.InstrumentWithDebugTraceCall();
  }

  inline bool InstrumentWithProfile() {
    return cgopt_.InstrumentWithProfile();
  }

  inline bool GenYieldpoint() {
    return cgopt_.GenYieldpoint();
  }

  inline bool GenLocalRC() {
    return cgopt_.GenLocalRC();
  }

  void AddStackGuardvar();
  void DefineDebugTraceFunctions();

  inline MIRSymbol *GetDebugTraceEnterFunction() {
    return dbg_trace_enter_;
  }

  inline MIRSymbol *GetProfileFunction() {
    return dbg_func_profile_;
  }

  inline MIRSymbol *GetDebugTraceExitFunction() {
    return dbg_trace_exit_;
  }

  // Object map generation helper
  std::vector<int64_t> GetReferenceOffsets64(BECommon &beCommon, MIRStructType *structty);

  void AddLabelDieToLabelIdxMapping(DBGDie *, LabelIdx);

};  // class CG

}  // namespace maplebe

#endif  //  MAPLEBE_INCLUDE_CG_CG_H
