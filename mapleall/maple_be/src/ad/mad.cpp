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


#include "mad.h"
#include <algorithm>

namespace maplebe {
using namespace std;

/**********************************     Unit      *****************************/
Unit::Unit(enum UnitId theUnitId) : unitId(theUnitId), unitType(kUnitTypePrimart), compositeUnits() {
  memset_s(occupancyTable, sizeof(Insn *) * kSizeOfOccupancyTable, 0, sizeof(Insn *) * kSizeOfOccupancyTable);
  MAD::AddUnit(this);
}

Unit::Unit(enum UnitType theUnitType, enum UnitId theUnitId, int numOfUnits, ...)
  : unitId(theUnitId), unitType(theUnitType) {
  CG_ASSERT(numOfUnits > 1, "CG internal error, composite unit with less than 2 unit elements.");
  va_list ap;
  va_start(ap, numOfUnits);

  for (int i = 0; i < numOfUnits; i++) {
    compositeUnits.push_back(static_cast<Unit *>(va_arg(ap, Unit *)));
  }
  memset_s(occupancyTable, sizeof(Insn *) * kSizeOfOccupancyTable, 0, sizeof(Insn *) * kSizeOfOccupancyTable);
  va_end(ap);

  MAD::AddUnit(this);
}

Unit::~Unit() {}

/* Check if unit is free at next "cycle" cycle. */
bool Unit::IsFree(uint32 cycle) const {
  if (GetUnitType() == kUnitTypeOr) {
    for (auto unit : compositeUnits) {
      if (unit->IsFree(cycle)) {
        return true;
      }
    }
    return false;
  } else if (GetUnitType() == kUnitTypeAnd) {
    for (auto unit : compositeUnits) {
      if (!unit->IsFree(cycle)) {
        return false;
      }
    }
    return true;
  } else {
    if (occupancyTable[cycle]) {
      return false;
    }
    return true;
  }
}

/* Occupy unit at next "cycle" cycle. */
void Unit::Occupy(Insn *insn, uint32 cycle) {
  if (GetUnitType() == kUnitTypeOr) {
    for (auto unit : GetCompositeUnits()) {
      if (unit->IsFree(cycle)) {
        unit->Occupy(insn, cycle);
        return;
      }
    }

    CG_ASSERT(false, "CG internal error, should not be reach here.");
    return;
  } else if (GetUnitType() == kUnitTypeAnd) {
    for (auto unit : GetCompositeUnits()) {
      unit->Occupy(insn, cycle);
    }
    return;
  } else {
    occupancyTable[cycle] = insn;
    return;
  }
}

/* Get owner.
 * cycle: at next "cycle" cycles.
 * owner[out]: for return owner.
 */
Insn *Unit::GetOwner(uint32 cycle) {
  if (GetUnitType() != kUnitTypeOr) {
    for (auto unit : GetCompositeUnits()) {
      if (!unit->IsFree(cycle)) {
        return (unit->GetOwner(cycle));
      }
    }
    CG_ASSERT(false, "CG internal error, should not be reach here.");
  } else if (GetUnitType() == kUnitTypeAnd) {
    return (GetCompositeUnits().front()->GetOwner(cycle));
  } else {
    return (occupancyTable[cycle]);
  }

  CG_ASSERT(false, "CG internal error, should not be reach here.");
  return nullptr;
}

/* Advance all units one cycle */
void Unit::AdvanceCycle() {
  if (GetUnitType() != kUnitTypePrimart) {
    return;
  }

  for (int i = 0; i < kSizeOfOccupancyTable - 1; i++) {
    occupancyTable[i] = occupancyTable[i + 1];
  }
  occupancyTable[kSizeOfOccupancyTable - 1] = nullptr;
}

/* Release all units. */
void Unit::ReleaseAll() {
  if (GetUnitType() != kUnitTypePrimart) {
    return;
  }

  for (int i = 0; i < kSizeOfOccupancyTable; i++) {
    occupancyTable[i] = nullptr;
  }
}

vector<Unit*> &Unit::GetCompositeUnits() {
  return compositeUnits;
}

void Unit::PrintIndent(int indent) {
  for (int i = 0; i < indent; i++) {
    cout << " ";
  }
}

void Unit::Dump(int indent) {
  PrintIndent(indent);
  cout << "Unit " << GetName() << " (ID " << GetUnitId() << "): ";
  switch (GetUnitType()) {
    case kUnitTypePrimart:
      for (int i = 0; i < kSizeOfOccupancyTable; i++) {
        if (occupancyTable[i]) {
          PrintIndent(indent);
          cout << "Index: [" << i << "] Owned by: ";
          occupancyTable[i]->dump();
          cout << "\n";
        }
      }
      cout << endl;
      break;
    case kUnitTypeOr:
      PrintIndent(indent);
      cout << "Or of { ";
      for (auto unit : GetCompositeUnits()) {
        unit->Dump(indent + 2);
      }
      cout << "} " << endl;
      break;
    case kUnitTypeAnd:
      PrintIndent(indent);
      cout << "And of { ";
      for (auto unit : GetCompositeUnits()) {
        unit->Dump(indent + 2);
      }
      cout << "} " << endl;
      break;
    default:
      CG_ASSERT(false, "Unexpected unit type.");
  }
}

int Unit::GetSizeOfOccupancyTable() const {
  return kSizeOfOccupancyTable;
}
/**********************************    MAD   *****************************/

int MAD::parallelism;
std::vector<Unit*> MAD::allUnits;
std::vector<Reservation*> MAD::allReservations;
std::vector<Bypass*> MAD::allBypass;

Reservation *MAD::FindReservation(Insn *insn) const {
  CG_ASSERT(insn, "");
  uint32 insnType = insn->GetLatencyType();
  for (auto reservation : allReservations) {
    if (reservation->IsEqual(insnType)) {
      return reservation;
    }
  }
  return nullptr;
}

int MAD::GetLatency(Insn *def, Insn *use) const {
  int latency = BypassLatency(def, use);
  if (latency < 0) {
    latency = DefaultLatency(def);
  }

  return latency;
}

int MAD::BypassLatency(Insn *def, Insn *use) const {
  int latency = -1;

  for (auto bypass : allBypass) {
    if (bypass->CanBypass(def, use)) {
      latency = bypass->GetLatency();
      break;
    }
  }
  return latency;
}

int MAD::DefaultLatency(Insn *insn) const {
  Reservation *res = insn->depNode->GetReservation();
  return res ? res->GetLatency() : 0;
}

void MAD::AdvanceCycle() {
  for (auto unit : allUnits) {
    unit->AdvanceCycle();
  }
}

bool Bypass::CanBypass(Insn *defInsn, Insn *useInsn) const {
  return defInsn->GetLatencyType() == def && useInsn->GetLatencyType() == use;
}

/*****************************    Reservation   **************************/
Reservation::Reservation(LatencyType t, int l, int n, ...)
    : type(t), latency(l), unitNum(n) {
  CG_ASSERT(l >= 0 && n >= 0, "CG internal error, latency and unitNum should not be less than 0.");
  va_list ap;
  va_start(ap, n);

  for (int i = 0; i < unitNum; i++) {
    units[i] = static_cast<Unit*>(va_arg(ap, Unit*));
  }
  va_end(ap);

  MAD::AddReservation(this);
  // if there are units, get slot's type.
  if (n > 0) {
    switch (units[0]->GetUnitId()) {
      case kUnitIdSlot0:
      case kUnitIdSlot0LdAgu:
      case kUnitIdSlot0StAgu:
        slot = kSlot0;
        break;

      case kUnitIdSlot1:
        slot = kSlot1;
        break;

      case kUnitIdSlotS:
      case kUnitIdSlotSHazard:
      case kUnitIdSlotSMul:
      case kUnitIdSlotSBranch:
      case kUnitIdSlotSAgen:
        slot = kSlotAny;
        break;

      case kUnitIdSlotD:
      case kUnitIdSlotDAgen:
        slot = kSlots;
        break;

      default:
        slot = kSlotNone;
    }
  } else {
    slot = kSlotNone;
  }
}

}  // namespace maplebe
