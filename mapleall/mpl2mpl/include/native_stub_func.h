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

#ifndef MPL2MPL_INCLUDE_NATIVESTUBFUNC_H
#define MPL2MPL_INCLUDE_NATIVESTUBFUNC_H

#include "module_phase.h"
#include "phase_impl.h"
#include "name_mangler.h"

#define SLOWNATIVEFUNCNUM 9
namespace maple {

const int kJniTypeNormal = 0;
const int kJniTypeMapleCriticalNative = 1;
const int kJnitTypeCriticalNative = 2;

struct NativeFuncProperty {
  NativeFuncProperty() {
    jniType = kJniTypeNormal;
  }

  std::string javaFunc;
  std::string nativeFile;
  std::string nativeFunc;
  int jniType;
};

class GenNativeStubFunc : public FuncOptimizeImpl {
 private:
  std::unordered_map<std::string, NativeFuncProperty> nativeFuncProperty;
  // a static binding function list
  std::unordered_set<std::string> staticBindingMethodsSet;
  std::unordered_set<std::string> manualFastNativeSet;
  std::unordered_set<std::string> manualCriticalNativeSet;

  TyIdx regTableEntryTyidx;
  TyIdx jstrPtrTyidx;
  MIRAggConst *regTableConst = nullptr;
  MIRSymbol *regFuncSymbol = nullptr;
  MIRAggConst *regFuncTabConst = nullptr;

  bool IsStaticBindingListMode() const;

  inline bool ReturnsJstr(TyIdx retTyIdx) {
    if (retTyIdx == jstrPtrTyidx) {
      return true;
    }
    MIRType *retType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(retTyIdx);
    MIRPtrType *ptrType = dynamic_cast<MIRPtrType *>(retType);
    if (ptrType == nullptr) {
      return false;
    }
    MIRType *pointedType = ptrType->GetPointedType();
    return pointedType->GetName() == NameMangler::GetInternalNameLiteral(NameMangler::kJavaLangStringStr);
  }

  void InitStaticBindingMethodList();
  bool IsStaticBindingMethod(const std::string &methodName);
  void LoadNativeFuncProperty();
  bool GetNativeFuncProperty(const std::string &funcName, NativeFuncProperty &property);
  bool IsManualFastNative(const std::string &funcName);
  bool IsManualCriticalNative(const std::string &funcName);
  void InitManualFastAndCriticalNative();
  void LoadNativeData(const std::string &file, std::unordered_set<std::string> &preloadSet);

  MIRFunction *MRTPreNativeFunc = nullptr;
  MIRFunction *MRTPostNativeFunc = nullptr;
  MIRFunction *MRTDecodeRefFunc = nullptr;
  MIRFunction *MRTCheckThrowPendingExceptionFunc = nullptr;
  MIRFunction *MRTCallFastNativeFunc = nullptr;
  MIRFunction *MRTCallFastNativeExtFunc = nullptr;
  MIRFunction *MRTCallSlowNativeFunc[SLOWNATIVEFUNCNUM] = {nullptr};  // for native func which args <=8, use x0-x7
  MIRFunction *MRTCallSlowNativeExtFunc = nullptr;
  MIRFunction *MCCSetReliableUnwindContextFunc = nullptr;
  const std::string kCallSlowNativeFuncs[SLOWNATIVEFUNCNUM] = {
      "MCC_CallSlowNative0", "MCC_CallSlowNative1", "MCC_CallSlowNative2", "MCC_CallSlowNative3", "MCC_CallSlowNative4",
      "MCC_CallSlowNative5", "MCC_CallSlowNative6", "MCC_CallSlowNative7", "MCC_CallSlowNative8"};

  MIRFunction *GetOrCreateDefaultNativeFunc(MIRFunction *stubFunc);
  void GenRegisteredNativeFunctionCall(MIRFunction *func, const MIRFunction *nativeFunc,
                                       MapleVector<BaseNode *> &args, MIRSymbol *ret);
  StmtNode *CreateNativeWrapperCallNode(MIRFunction *func, BaseNode *funcPtr, MapleVector<BaseNode *> &args,
                                        MIRSymbol *ret);
  void GenNativeWrapperFunctionCall(MIRFunction *func, const MIRFunction *nativeFunc,
                                    MapleVector<BaseNode *> &args, MIRSymbol *ret);
  void GenHelperFuncDecl();
  void GenRegTabEntry(const MIRFunction *func);
  void GenRegTableEntryType();
  void GenRegTable();
  TyIdx GenNewStructType(MIRStructType &newtype, const std::string &str);
  void GenRegFuncTabEntryType();
  void GenRegFuncTabEntry();
  void GenRegFuncTab();

 public:
  GenNativeStubFunc(MIRModule *mod, KlassHierarchy *kh, bool dump);
  ~GenNativeStubFunc() {}
  void ProcessFunc(MIRFunction *func) override;
  void Finish() override;

  FuncOptimizeImpl *Clone() override {
    return new GenNativeStubFunc(*this);
  }
};

class DoGenNativeStubFunc : public ModulePhase {
 public:
  explicit DoGenNativeStubFunc(ModulePhaseID id) : ModulePhase(id) {}

  ~DoGenNativeStubFunc() = default;

  std::string PhaseName() const override {
    return "GenNativeStubFunc";
  }

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(GenNativeStubFunc);
    return nullptr;
  }
};
}  // namespace maple

#endif
