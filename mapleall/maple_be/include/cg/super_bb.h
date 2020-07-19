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

#ifndef MAPLEBE_INCLUDE_CG_SUPERBB_H
#define MAPLEBE_INCLUDE_CG_SUPERBB_H

#include "cg_bb.h"
#include "cg_func.h"
#include "insn.h"
#include "mir_symbol.h"
#include "mempool_allocator.h"

namespace maplebe {

class SuperBB : public BB {
 private:
  MapleVector<BB *> enclosedBBs;
  bool hasUpdated;

 public:
  explicit SuperBB(uint32_t id, MapleAllocator *allocator)
    : BB(id, allocator),
      enclosedBBs(allocator->Adapter()),
      hasUpdated(false) {}

  ~SuperBB() {}

  virtual BB *Clone(MemPool *mp) const override {
    return mp->Clone<SuperBB>(*this);
  }

 public:
  MapleVector<BB *> &GetEnclosedBBs() {
    return enclosedBBs;
  }

  bool GetUpdatedFlag() const {
    return hasUpdated;
  }

  void SetUpdatedFlag(bool flag) {
    hasUpdated = flag;
  }

};  // class SuperBB

class SuperBBBuilder {
 public:
  CGFunc *cgFunc;

 private:
  MemPool *sBBMp;
  MapleAllocator sBBMa;
  SuperBB *firstSuperBB;
  SuperBB *lastSuperBB;
  MapleVector<SuperBB *> sBBVect;
  uint32 sBBCnt;

 public:
  explicit SuperBBBuilder(CGFunc *func, MemPool *mp)
    : cgFunc(func), sBBMp(mp), sBBMa(mp), sBBVect(sBBMa.Adapter()), sBBCnt(1) {}

  ~SuperBBBuilder() {}

 private:
  SuperBB *GetFirstSuperBB() const {
    return firstSuperBB;
  }

  void SetFirstSuperBB(SuperBB *spb) {
    firstSuperBB = spb;
  }

  SuperBB *GetLastSuperBB() const {
    return lastSuperBB;
  }

  void SetLastSuperBB(SuperBB *spb) {
    lastSuperBB = spb;
  }

  void AddBB2SuperBB(BB *&bb, SuperBB *&sBB);

  inline SuperBB *CreateNewSuperBB() {
    return sBBMp->New<SuperBB>(sBBCnt++, &sBBMa);
  }

  Insn *FindLastMachineInsn(BB *bb) const;
  bool HasOtherPreds(const BB *bb) const;
  bool IsEhsuccsSame(const BB *merger, const BB *mergee) const;
  bool IsInLSDA(const BB *bb, SuperBB *sbb) const;
  BB *GetFirstBBInSuperBB(SuperBB *sbb) const;
  BB *GetLastBBInSuperBB(SuperBB *sbb) const;
  void UpdateSuperBBInfo(SuperBB *sbb);
  void UpdateCGFunc(bool state);
  void SetPredsAndSuccs(SuperBB *superBB);
  void UpdateInsnBB();
  bool FindInsnLink(const Insn *insn, const BB *bb) const;
  int GetCallBBCount(SuperBB *sbb) const;
  bool CheckTryCatchStatus() const;
  bool CheckBBInsnStatus() const;
  void ResetKind(BB *bb);
  void DeleteEmptyBB();
  BB *CreateNewFallThroughBB(BB *prevBB);

 public:
  MemPool *GetMemPool() const {
    return sBBMp;
  }

  void SplitProcessOld(); // disabled
  void MergeProcess();
  void SplitProcess();

};  // class SuperBBBuilder

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_SUPERBB_H
