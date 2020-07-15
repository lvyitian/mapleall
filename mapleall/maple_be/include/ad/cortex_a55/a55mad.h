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

#ifndef AARCH64MAD_H
#define AARCH64MAD_H

#include "mad.h"

namespace maplebe {

class A55Unit : public Unit {
 public:
  // This method initializes an elementary unit.
  explicit A55Unit(enum UnitId theUnitId) : Unit(theUnitId) {}

  A55Unit(enum UnitType unitType, enum UnitId unitID, int numOfUnits, Unit *unit1, Unit *unit2)
      : Unit(unitType, unitID, numOfUnits, unit1, unit2) {}

  ~A55Unit() {}

  const char *GetName() const;
};

class A55MAD : public MAD {
 public:
  A55MAD() : MAD() {
    InitUnits();
    InitParallelism();
    InitReservation();
    InitBypass();
  }

  ~A55MAD() {}

  void InitUnits();
  void InitParallelism();
  void InitReservation();
  void InitBypass();
  bool IsSlot0Free() const;
  bool IsFullIssued() const;
  void AddReservation(LatencyType t, int l, Unit **u, int n);
};

}  // namespace maplebe

#endif /* AARCH64MAD_H */
