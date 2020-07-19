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

#ifndef MPL2MPL_INCLUDE_SIMPLIFY_H
#define MPL2MPL_INCLUDE_SIMPLIFY_H

#include "inline.h"
#include "module_phase.h"
#include "phase_impl.h"
#include <string>
#include "name_mangler.h"

#define STRING_EQUALS_STR "__string_equals"
#define TEMP_STRING_LENGTH_STR "__tmp_str_len"
#define TEMP_MEMCMPLEN_STR "__tmp_memcmplen"
#define MEMCMPMPL_STR "memcmpMpl"
#define TEMP_STRING_EUQAL_RESULT_STR "__tmp_str_equal_result"
#define MRT_STRING_EQUALS_NOTALLCOMPRESS_STR "MCC_String_Equals_NotallCompress"

namespace maple {
enum FuncIndex {
  kStringCharAt,
  kStringEquals,
  kStringGetChars,
  kObjectClone,
  kStringCompareTo,
  kStringGetCharsNoCheck,
  kStringNewStringFromBytes,
  kStringNewStringFromChars,
  kStringNewStringFromString,
  kStringToCharArray,
  kStringConcat,
  kStringFastSubStr,
  kStringIntern,
  kStringDoReplace,
  kStringFastIndexOf,
  kThreadCurrentthread,
  kCaseUnpaddedIntArray,
  kArrayIterNext,
  kPreconditionsCheckNotNull,
  kObjectsRequireNonNull,
  kLast
};

struct FuncInfo {
  const unsigned index;
  std::string mplname;
  bool do_simplify;  // true:do some simplification.(e.g. inline, replaced by C/asm.)
  // false: can directly replaced by a new funcname at the called place.
  std::string replacename;
};

class Simplify : public FuncOptimizeImpl {
 public:
  static bool doclinitopt;

  explicit Simplify(MIRModule *mod, KlassHierarchy *kh, bool dump);
  ~Simplify();

  FuncOptimizeImpl *Clone() override {
    return new Simplify(*this);
  }

  void ProcessFunc(MIRFunction *func) override;
  void Finish() override;

 private:
  MemPool *localMp;
  MInline *inliner;

  const FuncInfo kFuncs[kLast] = {
#include "simplify.def"
  };

  const unordered_set<string> kImplicitNullCheckModule = {
    "libcore-all.mpl",
    "libcore-all.bpl"
  };

  int32_t FindTableId(const std::string &name) {
    for (int i = 0; i < kLast; i++) {
      if (kFuncs[i].mplname == name) {
        return i;
      }
    }
    return -1;
  }
  bool IsImplicitNullCheckModule() const;

  void SimplifyStrCharAt(MIRFunction *func, StmtNode *stmt, MIRSymbol *ret);
  void SimplifyStrEquals(MIRFunction *func, StmtNode *stmt, MIRSymbol *ret);
  BaseNode *GetStringClassInfo(BaseNode *jstrOpnd);
  BaseNode *GetStringCount(BaseNode *jstrOpnd);
  void SimplifyCheckNotNull(MIRFunction *func, StmtNode *stmt, MIRSymbol *retSt);
  void SimplifyCallAssigned(MIRFunction *func, StmtNode *stmt);

  StmtNode *SimplifyStringEquals(MIRFunction *func, BaseNode *baseJstrOpnd, BaseNode *compareToOpnd, MIRSymbol *retSt);
};

class DoSimplify : public ModulePhase {
 public:
  explicit DoSimplify(ModulePhaseID id) : ModulePhase(id) {}

  std::string PhaseName() const override {
    return "Simplify";
  }

  ~DoSimplify() = default;

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(Simplify);
    return nullptr;
  }
};

}  // namespace maple
#endif
