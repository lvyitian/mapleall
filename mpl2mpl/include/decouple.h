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

#ifndef MPL2MPL_INCLUDE_DECOUPLE_H
#define MPL2MPL_INCLUDE_DECOUPLE_H

#include "phase_impl.h"
#include "module_phase.h"

namespace maple {

static const int32_t kPrimitiveArrayLength = 2;

enum FLAG_OFFSET { kField = 1, kVtable };

typedef struct OffsetTable {
  MIRSymbol *classSym;
  struct VtableIndexT {
    uint32_t methodnameIndex;
    uint32_t signameIndex;
    bool operator==(const VtableIndexT &t) const {
      return t.methodnameIndex == methodnameIndex && t.signameIndex == signameIndex;
    }
  };
  union {
    VtableIndexT vtableIndex;
    uint32_t fieldIndex;
  } index;
  int offset;
  int callIndex;
  int flag;
  bool operator==(const OffsetTable &t) const {
    if (flag == FLAG_OFFSET::kField) {
      return flag == t.flag && *(t.classSym) == *classSym && t.index.fieldIndex == index.fieldIndex;
    } else if (flag == FLAG_OFFSET::kVtable) {
      return flag == t.flag && *(t.classSym) == *classSym && t.index.vtableIndex == index.vtableIndex;
    } else {
      return false;
    }
  }

} OffsetTableT;

class DeCouple : public FuncOptimizeImpl {
 private:
  MIRSymbol *fieldOffsetTableKeySym = nullptr;
  MIRStructType *fieldOffsetTableKeyType = nullptr;
  MIRArrayType *arrayTypeOfFieldKey = nullptr;
  MIRSymbol *vtableOffsetTableKeySym = nullptr;
  MIRStructType *vtableOffsetTableKeyType = nullptr;
  MIRArrayType *arrayTypeOfVtableKey = nullptr;
  MIRSymbol *offsetTableSym = nullptr;
  MIRStructType *offsetTableEntryType = nullptr;
  MIRArrayType *arrayTypeOfValue = nullptr;
  std::vector<OffsetTableT> offsetTableV;
  std::unordered_set<std::string> nonDeCoupleClass;

  void GenOffsetTableType();
  void GenOffsetTable();
  void InsertFieldCallItem(MIRType *pointedTy, FieldID fieldID, int &callIndex);
  int InsertVirtualCallItem(const MIRFunction *callee, int entryoffset);
  void CollectDreadStmt(MIRFunction *curFunc, StmtNode *stmt);
  void CollectDreadExpr(MIRFunction *curFunc, StmtNode *stmt, BaseNode *expr);
  void ResolveVirtual(CallNode *stmt);
  void GenLocalClassinfo();
  ArrayNode *GenOffsetTableArrayExpr(int callIndex);
  void GenNonDeCoupleClass();
  bool NeedGenOffset(const std::string &className) const;

 public:
  DeCouple(MIRModule *mod, KlassHierarchy *kh, bool dump);
  ~DeCouple() {}
  void ProcessFunc(MIRFunction *func) override;
  void Finish() override;

  FuncOptimizeImpl *Clone() override {
    return new DeCouple(*this);
  }
};

class DoDeCouple : public ModulePhase {
 public:
  explicit DoDeCouple(ModulePhaseID id) : ModulePhase(id) {}

  std::string PhaseName() const override {
    return "DeCouple";
  }

  ~DoDeCouple() = default;

  AnalysisResult *Run(MIRModule *mod, ModuleResultMgr *mrm) override {
    OPT_TEMPLATE(DeCouple);
    return nullptr;
  }
};

}  // namespace maple

#endif
