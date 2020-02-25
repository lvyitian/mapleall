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

#ifndef DEPS_H
#define DEPS_H

#include "mad.h"
#include "reg_pressure.h"
namespace maplebe {

class Insn;
class Unit;
class Reservation;

enum DepType {
  kDependenceTypeTrue,
  kDependenceTypeOutput,
  kDependenceTypeAnti,
  kDependenceTypeControl,
  kDependenceTypeMembar,
  kDependenceTypeThrow,
  kDependenceTypeSeparator,
  kDependenceTypeNone
};

enum NodeType {
  kNodeTypeNormal,
  kNodeTypeSeparator,
  kNodeTypeEmpty
};

enum ScheduleState {
  kNormal,
  kReady,
  kScheduled,
};

class DepNode;

class DepLink {
 public:
  DepLink(DepNode *f, DepNode *t, DepType typ) : from(f), to(t), depType(typ), latency(0) {}
  virtual ~DepLink() {}

  inline DepNode *GetFrom() const {
    return from;
  }
  inline DepNode *GetTo() const {
    return to;
  }
  inline void SetDepType(DepType dt) {
    depType = dt;
  }
  inline DepType GetDepType() const {
    return depType;
  }
  inline void SetLatency(uint32_t l) {
    latency = l;
  }
  inline uint32_t GetLatency() const {
    return latency;
  }

 private:
  DepNode *from;
  DepNode *to;
  DepType depType;
  uint32_t latency;
};

class DepNode {
 public:
  bool CanBeScheduled() const;
  void OccupyUnits();

  explicit DepNode(Insn *insn) : insn(insn), units(nullptr), reservation(nullptr), unitNum(0),
      eStart(0), lStart(0), visit(0), type(kNodeTypeNormal), state(kNormal), index(0), validPredsSize(0),
      validSuccsSize(0), preds(), succs(), comments(), cfiInsns(), clinitInsns(), regPressure(nullptr) {}

  DepNode(Insn *insn, Unit * const *unit, uint32 num, Reservation *rev) : insn(insn), units(unit),
      reservation(rev), unitNum(num), eStart(0), lStart(0), visit(0), type(kNodeTypeNormal), state(kNormal),
      index(0), validPredsSize(0), validSuccsSize(0), preds(), succs(), comments(), cfiInsns(), clinitInsns(),
      regPressure(nullptr) {}
  virtual ~DepNode() {}

  inline Insn *GetInsn() const {
    return insn;
  }
  inline void SetInsn(Insn *i) {
    insn = i;
  }
  inline void SetUnits(Unit * const *u) {
    units = u;
  }
  inline Unit *GetUnitByIndex(int index) const {
    return units[index];
  }
  inline Reservation *GetReservation() const {
    return reservation;
  }
  inline void SetReservation(Reservation *rev) {
    reservation = rev;
  }
  inline uint32 GetUnitNum() const {
    return unitNum;
  }
  inline void SetUnitNum(uint32 num) {
    unitNum = num;
  }
  inline uint32 GetEStart() const {
    return eStart;
  }
  inline void SetEStart(uint32 e) {
    eStart = e;
  }
  inline uint32 GetLStart() const {
    return lStart;
  }
  inline void SetLStart(uint32 l) {
    lStart = l;
  }
  inline uint32 GetVisit() const {
    return visit;
  }
  inline void SetVisit(uint32 v) {
    visit = v;
  }
  inline void IncreaseVisit() {
    visit++;
  }
  inline NodeType GetType() const {
    return type;
  }
  inline void SetType(NodeType t) {
    type = t;
  }
  inline ScheduleState GetState() const {
    return state;
  }
  inline void SetState(ScheduleState s) {
    state = s;
  }
  inline uint32 GetIndex() const {
    return index;
  }
  inline void SetIndex(uint32 i) {
    index = i;
  }
  inline uint32 GetValidPredsSize() const {
    return validPredsSize;
  }
  inline void SetValidPredsSize(uint32 size) {
    validPredsSize = size;
  }
  inline void DescreaseValidPredsSize() {
    validPredsSize--;
  }
  inline uint32 GetValidSuccsSize() const {
    return validSuccsSize;
  }
  inline void SetValidSuccsSize(uint32 size) {
    validSuccsSize = size;
  }
  inline const std::vector<DepLink*> &GetPreds() const {
    return preds;
  }
  inline void AddPred(DepLink *depLink) {
    preds.push_back(depLink);
  }
  inline void RemovePred() {
    preds.pop_back();
  }
  inline const std::vector<DepLink*> &GetSuccs() const{
    return succs;
  }
  inline void AddSucc(DepLink *depLink) {
    succs.push_back(depLink);
  }
  inline void RemoveSucc() {
    succs.pop_back();
  }
  inline const std::vector<Insn *> &GetComments() const {
    return comments;
  }
  inline void SetComments(std::vector<Insn *> com) {
    comments = com;
  }
  inline const std::vector<Insn *> &GetCfiInsns() const {
    return cfiInsns;
  }
  inline void SetCfiInsns(std::vector<Insn *> insns) {
    cfiInsns = insns;
  }
  inline void AddCfiInsn(Insn *insn) {
    cfiInsns.push_back(insn);
  }
  inline void ClearCfiInsns() {
    cfiInsns.clear();
  }
  inline const std::vector<Insn *> &GetClinitInsns() const {
    return clinitInsns;
  }
  inline void SetClinitInsns(std::vector<Insn *> insns) {
    clinitInsns = insns;
  }
  inline void AddClinitInsn(Insn *insn) {
    clinitInsns.push_back(insn);
  }
  inline const RegPressure *GetRegPressure() const {
    return regPressure;
  }
  inline void SetRegPressure(RegPressure *pressure) {
    regPressure = pressure;
  }
  inline void DumpRegPressure() const {
    regPressure->DumpRegPressure();
  }
  inline void InitPressure() {
    regPressure->InitPressure();
  }
  inline const int *GetPressure() const {
    return regPressure->GetPressure();
  }
  inline void SetPressure(int *p) {
    regPressure->SetPressure(p);
  }
  inline void IncPressureByIndex(int index) {
    regPressure->IncPressureByIndex(index);
  }
  inline void DecPressureByIndex(int index) {
    regPressure->DecPressureByIndex(index);
  }
  inline void AddUseReg(regno_t reg) {
    regPressure->AddUseReg(reg);
  }
  inline void AddDefReg(regno_t reg) {
    regPressure->AddDefReg(reg);
  }
  inline void SetRegUses(regno_t regno, RegList *regList) {
    regPressure->SetRegUses(regno, regList);
  }
  inline int GetIncPressure() const {
    return regPressure->GetIncPressure();
  }
  inline void SetIncPressure(bool value) {
    regPressure->SetIncPressure(value);
  }
  inline int GetMaxDepth() const {
    return regPressure->GetMaxDepth();
  }
  inline void SetMaxDepth(int value) {
    regPressure->SetMaxDepth(value);
  }
  inline int GetNear() const {
    return regPressure->GetNear();
  }
  inline void SetNear(int value) {
    regPressure->SetNear(value);
  }
  inline int GetPriority() const {
    return regPressure->GetPriority();
  }
  inline void SetPriority(int value) {
    regPressure->SetPriority(value);
  }
  inline const std::set<regno_t> &GetUses() const {
    return regPressure->GetUses();
  }
  inline const std::set<regno_t> &GetDefs() const {
    return regPressure->GetDefs();
  }
  inline const std::map<regno_t, RegList*> &GetRegUses() const {
    return regPressure->GetRegUses();
  }

 private:
  Insn *insn;
  Unit * const *units;
  Reservation *reservation;
  uint32 unitNum;
  uint32 eStart;
  uint32 lStart;
  uint32 visit;
  NodeType type;
  ScheduleState state;
  uint32 index;

  // For scheduling, denotes unscheduled preds/succs number.
  uint32 validPredsSize;
  uint32 validSuccsSize;

  // Dependence links.
  std::vector<DepLink*> preds;
  std::vector<DepLink*> succs;

  // Non-machine instructions prior to insn, such as comments.
  std::vector<Insn *> comments;

  // Non-machine instructions which follows insn, such as cfi instructions.
  std::vector<Insn *> cfiInsns;

  // Special instructions which follows insn, such as clinit instructions.
  std::vector<Insn *> clinitInsns;

  // For register pressure analysis
  RegPressure *regPressure;
};

}  // namespace maplebe

#endif  // DEPS_H
