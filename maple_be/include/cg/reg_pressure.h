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

#ifndef MAPLEBE_INCLUDE_CG_REGPRESSURE_H
#define MAPLEBE_INCLUDE_CG_REGPRESSURE_H

namespace maplebe {
class Insn;
struct RegList {
  Insn *insn;
  RegList *next;
};

#define FOR_ALL_REGCLASS(i) \
  for (int i = 0; i < RegPressure::GetMaxRegClassNum(); i++)

class RegPressure {
 public:
  explicit RegPressure(MemPool *mp)
      : uses(), defs(), regUses(), pressure(nullptr), priority(0), maxDepth(0), near(0), incPressure(false) {}
  virtual ~RegPressure() {}
  void DumpRegPressure();

  void AddUseReg(regno_t reg) {
    uses.insert(reg);
  }
  void AddDefReg(regno_t reg) {
    defs.insert(reg);
  }
  void SetRegUses(regno_t regno, RegList *regList) {
    regUses.insert(std::pair<regno_t, RegList*>(regno, regList));
  }
  static void SetMaxRegClassNum(int n) {
    maxRegClassNum = n;
  }
  static int GetMaxRegClassNum() { return maxRegClassNum; }
  inline int GetPriority() const {
    return priority;
  }
  inline void SetPriority(int value) {
    priority = value;
  }
  inline int GetMaxDepth() const {
    return maxDepth;
  }
  inline void SetMaxDepth(int value) {
    maxDepth = value;
  }
  inline int GetNear() const {
    return near;
  }
  inline void SetNear(int value) {
    near = value;
  }
  inline int GetIncPressure() const {
    return incPressure;
  }
  inline void SetIncPressure(bool value) {
    incPressure = value;
  }
  inline const int *GetPressure() const {
    return pressure;
  }
  inline void SetPressure(int *p) {
    pressure = p;
  }
  inline void IncPressureByIndex(int index) {
    pressure[index]++;
  }
  inline void DecPressureByIndex(int index) {
    pressure[index]--;
  }
  inline void InitPressure() {
    FOR_ALL_REGCLASS(i) {
      pressure[i] = 0;
      incPressure = false;
    }
  }
  inline const std::set<regno_t> &GetUses() const {
    return uses;
  }
  inline const std::set<regno_t> &GetDefs() const {
    return defs;
  }
  inline const std::map<regno_t, RegList*> &GetRegUses() const {
    return regUses;
  }

 private:
  std::set<regno_t> uses;
  std::set<regno_t> defs;
  // save reglist of every uses'register
  std::map<regno_t, RegList*> regUses;
  int *pressure;
  // max number of reg's class
  static int maxRegClassNum;
  int priority;
  int maxDepth;
  int near;
  // if a type register increase then set incPressure as true.
  bool incPressure;
};

} // namespace maplebe

#endif // MAPLEBE_INCLUDE_CG_REGPRESSURE_H
