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

#ifndef MAD_H
#define MAD_H

#include "insn.h"

namespace maplebe {

class Insn;

enum UnitId {
#ifdef MPLAD_CORTEX_A55
#include "cortex_a55/mplad_unit_id.def"
#endif  // MPLAD_CORTEX_A55
  kUnitIdNone
};

enum UnitType { kUnitTypePrimart, kUnitTypeOr, kUnitTypeAnd, KUnitTypeNone };

enum SlotType {
  kSlotNone,
  kSlot0,
  kSlot1,
  kSlotAny,
  kSlots,
};

// machine model
enum LatencyType {
  // LT: latency
  kLtUndef,
#ifdef MPLAD_CORTEX_A55
#include "cortex_a55/mplad_latency_type.def"
#endif
  kLtLast,
};

class Reservation;
class Bypass;

class Unit {
 public:
  explicit Unit(enum UnitId theUnitId);
  Unit(enum UnitType theUnitType, enum UnitId theUnitId, int numOfUnits, ...);
  ~Unit();

  inline enum UnitType GetUnitType() const {
    return unitType;
  }
  inline enum UnitId GetUnitId() const {
    return unitId;
  };
  std::vector<Unit*> &GetCompositeUnits();
  virtual const char *GetName() const {
    return NULL;
  }

  bool IsFree(uint32 cycle) const;
  void Occupy(Insn *insn, uint32 cycle);
  unsigned int GetUnitTypeNum() const;
  void ReleaseAll();
  Insn *GetOwner(uint32 cycle);
  void AdvanceCycle();
  void Dump(int indent = 0);
  int GetSizeOfOccupancyTable() const;

 private:
  inline void PrintIndent(int indent);

  static const int kSizeOfOccupancyTable = 7;
  enum UnitId unitId;
  enum UnitType unitType;
  Insn *occupancyTable[kSizeOfOccupancyTable];
  std::vector<Unit*> compositeUnits;
};

class MAD {
 public:
  MAD() {}

  virtual ~MAD() {}

  virtual void InitUnits() {}

  virtual void InitParallelism() {}

  virtual void InitReservation() {}

  virtual void InitBypass() {}

  virtual bool IsSlot0Free() const {
    return false;
  }

  virtual bool IsFullIssued() const {
    return false;
  }

  int GetLatency(Insn *def, Insn *use) const;
  int DefaultLatency(Insn *insn) const;
  virtual Reservation *FindReservation(Insn *insn) const;
  void AdvanceCycle();
  inline int GetMaxParallelism() const {
    return parallelism;
  }
  inline const Unit *GetUnitByUnitId(enum UnitId uId) const {
    return allUnits[uId];
  }
  static void AddUnit(Unit *u) {
    allUnits.push_back(u);
  }
  static void AddReservation(Reservation *rev) {
    allReservations.push_back(rev);
  }
  static void AddBypass(Bypass *bp) {
    allBypass.push_back(bp);
  }

 protected:
  inline void SetMaxParallelism(int num) {
    parallelism = num;
  }

  virtual int BypassLatency(Insn *def, Insn *use) const;

 private:
  static int parallelism;
  static std::vector<Unit*> allUnits;
  static std::vector<Reservation*> allReservations;
  static std::vector<Bypass*> allBypass;
};

class Reservation {
 public:
  Reservation(LatencyType t, int l, int n, ...);
  virtual ~Reservation() {}

  inline bool IsEqual(uint32 t) const {
    return type == t;
  }

  inline int GetLatency() const {
    return latency;
  }

  inline int GetUnitNum() const {
    return unitNum;
  }

  inline enum SlotType GetSlot() const {
    return slot;
  }

  inline Unit * const *GetUnit() const {
    return units;
  }

 private:
  static const int kmaxUnit = 6;
  LatencyType type;
  int latency;
  int unitNum;
  Unit *units[kmaxUnit];
  enum SlotType slot;
};

class Bypass {
 public:
  Bypass(LatencyType d, LatencyType u, int l) : def(d), use(u), latency(l) {}
  virtual ~Bypass() {}

  virtual bool CanBypass(Insn *defInsn, Insn *useInsn) const;

  int GetLatency() const {
    return latency;
  }

 private:
  LatencyType def;
  LatencyType use;
  int latency;
};

}  // namespace maplebe

#endif /* MAD_H */
