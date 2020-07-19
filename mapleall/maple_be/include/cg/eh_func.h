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

#ifndef MAPLEBE_INCLUDE_EH_EHFUNC_H
#define MAPLEBE_INCLUDE_EH_EHFUNC_H
#include "mir_parser.h"
#include "mir_function.h"
#include "lsda.h"
#include "cg_phase.h"

namespace maplebe {

class CGFunc;
class EHFunc;

class EHJavaTry {
 public:
  TryNode *javatry_node;
  StmtNode *endtry_node;
  StmtNode *fallthru_goto;  // no throw in the try block, the goto stmt to the fall through
  MapleVector<CatchNode *> javacatch_vec;
  LSDACallSite *lsdacallsite;  // one try has a callsite

 public:
  explicit EHJavaTry(MapleAllocator *alloc, TryNode *trynode)
    : javatry_node(trynode),
      endtry_node(nullptr),
      fallthru_goto(nullptr),
      javacatch_vec(alloc->Adapter()),
      lsdacallsite(nullptr) {}

  virtual ~EHJavaTry() {}

  void DumpEHJavaTry(const MIRModule &);
  LabelIdx GetDefaultCatchLabidx();
};

class EHThrow {
 public:
  UnaryStmtNode *rethrow;       // must be a throw stmt
  EHJavaTry *java_try;          // the javatry statement wrapping this throw
  LabelNode *start_label;       // the label that RETHROWEXCEPTION begin
  LabelNode *end_label;         // the label that RETHROWEXCEPTION end
  LabelNode *ladpadendlabel;    // the label that "_Unwind_Resume" begin
  LabelNode *end_unwind_label;  // the label that "_Unwind_Resume" end

 public:
  explicit EHThrow(UnaryStmtNode *rtnode)
    : rethrow(rtnode),
      java_try(nullptr),
      start_label(nullptr),
      end_label(nullptr),
      ladpadendlabel(nullptr),
      end_unwind_label(nullptr) {}

  void Lower(EHFunc *);
  bool IsUnderJavaTry() const {
    return java_try != nullptr;
  }

  bool HasLSDA() const {
    return start_label != nullptr;
  }

  void ConvertThrowToRethrow(EHFunc *);
  void ConvertThrowToRuntime(EHFunc *, MIRType *, BaseNode *);
};

class EHFunc {
 public:
  CGFunc *cgfunc;
  LabelIdx labelIdx;
  MapleVector<EHJavaTry *> javatry_vec;      // try stmt node
  MapleVector<TyIdx> eh_ty_table;          // the type that would emit in LSDA
  MapleMap<TyIdx, uint32> ty2index_table;  // use the TyIdx to get the index of eh_ty_table;
  LSDAHeader *lsda_header;
  static const uint8 kTypeEncoding = 0x9b;  // same thing as LSDAHeader.kTypeEncoding
  LSDACallSiteTable *lsda_callsite_table;
  LSDAActionTable *lsda_action_table;
  MIRClassType *exception_type;        // the top type of  exception. i.e class Exception
  MapleVector<EHThrow *> rethrow_vec;  // EHRethrow

 public:
  explicit EHFunc(CGFunc *func);
  void DumpEHFunc();
  void InsertEHSwitchTable();
  void CreateLSDA();
  void CreateCppLSDA(std::unordered_map<LabelNode *, CppCatchNode *> &label2catch_map);
  LabelIdx CreateLabel(const char *);
  bool NeedFullLSDA();
  bool NeedFastLSDA();
  void InsertCxaBeginEndCatch(const std::vector<std::pair<LabelIdx, CatchNode *>> &);
  void GenerateCleanupLabel();
  void BuildEHTypeTable(const std::vector<std::pair<LabelIdx, CatchNode *>> &);
  void BuildCppEHTypeTable(const std::unordered_map<LabelNode *, CppCatchNode *> &);
  bool HasThrow() const {
    return rethrow_vec.size() > 0;
  }

  virtual ~EHFunc() {}

  void LowerThrow();  // for non-personality function
  void CreateTypeInfoSt();
  bool HasJavaTry() const;

 private:
  StmtNode *GetLastStmtCatchBlock(BlockNode *, CatchNode *);
  void CreateLSDAAction();
  void CreateCppLSDAAction(std::unordered_map<TryNode *, std::pair<LabelNode *, CppCatchNode *>> &try2catch_map);
};

CGFUNCPHASE(CgDoBuildEHFunc, "buildehfunc")

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_EHFUNC_H_
