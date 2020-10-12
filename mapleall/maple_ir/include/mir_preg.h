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

#ifndef MAPLE_IR_INCLUDE_MIR_PREG_H
#define MAPLE_IR_INCLUDE_MIR_PREG_H

#if MIR_FEATURE_FULL
#include <iostream>
#include "mir_module.h"
#endif  // MIR_FEATURE_FULL

namespace maple {
extern void PrintIndentation(int32 );

// these special registers are encoded by negating the enumeration
enum SpecialReg : signed int {
  kSregSp = 1,
  kSregFp = 2,
  kSregGp = 3,
  kSregThrownval = 4,
  kSregMethodhdl = 5,
  kSregRetval0 = 6,
  kSregRetval1 = 7,
  kSregLast = 8,
};

#if MIR_FEATURE_FULL
class MIRPreg {
 public:
  PrimType primType : 8;
  bool needRC : 1;  // if it's true, skip reference counting update
  int32 pregNo;  // the number in maple IR after the %
  MIRType *mirType;  // null if not a ref type and not a function pointer type

 public:
  explicit MIRPreg(uint32 n = 0) : primType(kPtyInvalid), needRC(false), pregNo(n), mirType(nullptr) {}
  MIRPreg(uint32 n, PrimType ptyp) : primType(ptyp), needRC(false),
                      pregNo(n), mirType(nullptr) {}
  MIRPreg(uint32 n, PrimType ptyp, MIRType *mtype) : primType(ptyp),needRC(false),
                      pregNo(n), mirType(mtype) {}
  ~MIRPreg(){};

  bool IsRef() {
    return mirType != nullptr && primType == PTY_ref;
  }

  bool NeedRC() {
    return needRC;
  }
  void SetNeedRC() {
    needRC = true;
  }
};


class MIRPregTable {
 private:
  MapleAllocator *mAllocator;

 public:
  uint32 maxPregNo;  //  the max pregNo that has been allocated
  MapleMap<uint32, PregIdx> pregNoToPregIdxMap;  // for quick lookup based on pregNo
  MapleVector<MIRPreg *> pregTable;
  MIRPreg specPregTable[kSregLast];  // for the MIRPreg nodes corresponding to special registers
 public:
  MIRPregTable(MapleAllocator *allocator)
    : mAllocator(allocator),
      maxPregNo(0),
      pregNoToPregIdxMap(std::less<uint32>(), mAllocator->Adapter()),
      pregTable(mAllocator->Adapter()) {
    pregTable.push_back((MIRPreg *)nullptr);
    specPregTable[0].pregNo = 0;
    specPregTable[kSregSp].pregNo = -kSregSp;
    specPregTable[kSregFp].pregNo = -kSregFp;
    specPregTable[kSregGp].pregNo = -kSregGp;
    specPregTable[kSregThrownval].pregNo = -kSregThrownval;
    specPregTable[kSregMethodhdl].pregNo = -kSregMethodhdl;
    specPregTable[kSregRetval0].pregNo = -kSregRetval0;
    for (uint32 i = 0; i < kSregLast; i++) {
      specPregTable[i].primType = PTY_unknown;
    }
  }

  ~MIRPregTable();

  PregIdx CreatePreg(PrimType primType, MIRType *mtype = nullptr) {
    CHECK_FATAL(!mtype || mtype->primType == PTY_ref || mtype->primType == PTY_ptr, "ref or ptr type");
    uint32 index = ++maxPregNo;
    MIRPreg *preg = mAllocator->mp->New<MIRPreg>(index, primType, mtype);
    return AddPreg(preg);
  }

  PregIdx ClonePreg(MIRPreg *rfpreg) {
    PregIdx idx = CreatePreg(rfpreg->primType, rfpreg->mirType);
    MIRPreg *preg = pregTable[idx];
    preg->needRC = rfpreg->needRC;
    return idx;
  }

  MIRPreg *PregFromPregIdx(PregIdx pregidx) {
    if (pregidx < 0) {  // special register
      return &specPregTable[-pregidx];
    } else {
      return pregTable.at(pregidx);
    }
  }

  PregIdx GetPregIdxFromPregNo(uint32 pregNo) {
    MapleMap<uint32, PregIdx>::iterator it = pregNoToPregIdxMap.find(pregNo);
    if (it == pregNoToPregIdxMap.end()) {
      return (PregIdx)0;
    }
    return it->second;
  }

  void DumpPregsWithTypes(int32 indent) {
    MapleVector<MIRPreg *> &pregtable = pregTable;
    for (uint32 i = 1; i < pregtable.size(); i++) {
      MIRPreg *mirpreg = pregtable[i];
      if (mirpreg->mirType == nullptr) {
        continue;
      }
      PrintIndentation(indent);
      LogInfo::MapleLogger() << "reg ";
      LogInfo::MapleLogger() << "%" << mirpreg->pregNo;
      LogInfo::MapleLogger() << " ";
      mirpreg->mirType->Dump(0);
      LogInfo::MapleLogger() << " " << (mirpreg->needRC ? 1 : 0);
      LogInfo::MapleLogger() << "\n";
    }
  }
  size_t Size(void) {
    return pregTable.size();
  }

  PregIdx AddPreg(MIRPreg *preg) {
    PregIdx idx = pregTable.size();
    pregTable.push_back(preg);
    pregNoToPregIdxMap[preg->pregNo] = idx;
    return idx;
  }

  PregIdx EnterPregNo(uint32 pregNo, PrimType ptyp, MIRType *ty = nullptr) {
    PregIdx idx = GetPregIdxFromPregNo(pregNo);
    if (idx == 0) {
      if (pregNo > maxPregNo) {
        maxPregNo = pregNo;
      }
      MIRPreg *preg = mAllocator->mp->New<MIRPreg>(pregNo, ptyp, ty);
      return AddPreg(preg);
    }
    return idx;
  }
};
#endif  // MIR_FEATURE_FULL

}  // namespace maple
#endif  // MAPLE_IR_INCLUDE_MIR_PREG_H
