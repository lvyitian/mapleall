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

#define VIRTUALCALL_TO_CALL_STR "_vcall_to_call_"
#define JAVA_LANG_STRING_STR "Ljava_2Flang_2FString_3B"
#define STRING_EQUALS_STR "__string_equals"
#define TEMP_STRING_LENGTH_STR "__tmp_str_len"
#define TEMP_MEMCMPLEN_STR "__tmp_memcmplen"
#define MEMCMPMPL_STR "memcmpMpl"
#define TEMP_STRING_EUQAL_RESULT_STR "__tmp_str_equal_result"
#define MRT_STRING_EQUALS_NOTALLCOMPRESS_STR "MCC_String_Equals_NotallCompress"
#define ARRAYLIST_STR "Ljava_2Futil_2FArrayList_3B"
#define CONCURRENTLINKEDQUEUE_STR "Ljava_2Futil_2Fconcurrent_2FConcurrentLinkedQueue_3B"
#define ARRAYLIST_INIT_STR "Ljava_2Futil_2FArrayList_3B_7C_3Cinit_3E_7C_28_29V"
#define MCALLSESSIONSPOOL_STR "mCallSessionsPool"
#define THIS_STR "_this"
#define MLOCK_STR "mLock"
#define OBJECT_STR "Ljava_2Flang_2FObject_3B"
#define SYNC_TRY_STR "_label_sync_try"
#define LIST_STR "__List"
#define SIZE_STR "size"
#define NEW_STR "_new"
#define TMP_STR "__tmp"
#define POOL_STR "__pool"
#define BOOL_RETURN_STR "_bool_ret"
#define PROFILEIDS_STR "_profileIds"
#define INDEX_STR "_index"
#define ARRAYLENGTH_STR "_arrayLength"
#define LABEL_START_STR "_lableStart"
#define LABEL_END_STR "_lableEnd"
#define TEMP_STR "_temp"
#define INTEGER_STR "Ljava_2Flang_2FInteger_3B"
#define INTEGER_TEMP_STR "_IntegerTemp"
#define SET_STR "Ljava_2Futil_2FSet_3B"
#define SETUSERIDS_STR "_setUserIds"
#define OBJECT_TEMP_STR "_objectTemp"
#define TEMP_RETUEN_ST_STR "__tmp_ret_st"
#define FIGO_RETV_STR "__figo_retv__"
#define FIGO_RETV_FILENAMELENGTH_STR "__figo_retv_FileNameLen__"
#define FIGO_RETV_IDENTIFYFILENAMELEN_STR "__figo_retv_IdentifyFileNameLen__"
#define FIGO_RETV_SUB_STR "__figo_retv_sub__"
#define FIGO_RETV_CHARAT_STR "__figo_retv_CharAt__"
#define MARRAY_STR "mArray"
#define MINDEX_STR "mIndex"
#define LOCALDECL_MINDEX_STR "__mIndex"
#define MOFFSET_STR "mOffset"
#define LOCALDECL_MOFFSET_STR "__mOffset"
#define MCANREMOVE_STR "mCanRemove"
#define TID_STR "_tid"
#define GET_TID_STR "gettid"
#define MANIMATIONTHREADID_STR "mAnimationThreadId"
#define TEMP_MANIMATIONTHREADID_STR "_temp_mAnimationThreadId"
#define MSURFACEANIMATIONTHREADID_STR "mSurfaceAnimationThreadId"
#define TEMP_MSURFACEANIMATIONTHREADID_STR "_temp_mSurfaceAnimationThreadId"
#define ERRNO_STR "errno"
#define PREVPRIORITY_STR "_prevPriority"
#define GETPRIORITY_STR "getpriority"
#define MCC_THROWSECURITYEXCEPTION_STR "MCC_ThrowSecurityException"
#define CHANGED_STR "_changed"
#define MBOOSTTOPRIORITY_STR "mBoostToPriority"
#define TEMP_MBOOSTTOPRIORITY_STR "_temp_mBoostToPriority"
#define SETPRIORITY_STR "setpriority"
#define OPTIMIZATION_FOR_COLLECTION "__optimizedForCollection"
#define CLASSINIT_STR "_7C_3Cclinit_3E_7C_28_29V"
#define PTR_C_STR "_PTR_C_STR"
#define MRT_ARRAYMAP_STRING_INT_CLEAR_STR "MCC_ArrayMap_String_Int_clear"
#define MRT_ARRAYMAP_STRING_INT_PUT_STR "MCC_ArrayMap_String_Int_put"
#define MRT_ARRAYMAP_STRING_INT_SIZE_STR "MCC_ArrayMap_String_Int_size"
#define MRT_ARRAYMAP_STRING_INT_GETORDEFAULT_STR "MCC_ArrayMap_String_Int_getOrDefault"

namespace maple {
enum FuncIndex {
  kStringCharAt,
  kStringEquals,
  kStringGetChars,
  kFilterOutIdentifyFiles,
  kBinderCallsStatsInit,
  kBinderCallsStatsInitZ,
  kBinderCallsStatsInitQ,
  kBinderCallsStatsStarted,
  kBinderCallsStatsStartedQ,
  kBinderCallsStatsEnded,
  kBinderCallsStatsEndedQ,
  kGetProfileIds,
  kComputeGids,
  kStrictModeSetPolicy,
  kStrictModeClearVio,
  kStrictModeHasVio,
  kStrictModeInit,
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
  kSystemArraycopy,
  kSystemArraycopyChar,
  kSystemArraycopyByte,
  kSystemArraycopyShort,
  kSystemArraycopyInt,
  kSystemArraycopyLong,
  kSystemArraycopyFloat,
  kSystemArraycopyDouble,
  kSystemArraycopyBoolean,
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
