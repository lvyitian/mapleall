/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
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

#ifndef MAPLEBE_INCLUDE_CG_DBG_H
#define MAPLEBE_INCLUDE_CG_DBG_H

#include "insn.h"
#include "mempool_allocator.h"

#define DWARF_VERSION 4

namespace maple {
class MIRSymbol;
}

#define TEXT_BEGIN text0
#define TEXT_END etext0
#define DEBUG_INFO_0 debug_info0
#define DEBUG_ABBREV_0 debug_abbrev0
#define DEBUG_LINE_0 debug_line0
#define DEBUG_STR_LABEL ASF
#define DEBUG_MAPLE_THIS "_this"

namespace mpldbg {

// https://sourceware.org/binutils/docs-2.28/as/Loc.html
enum LocOpt { kBB, kProEnd, kEpiBeg, kIsStmt, kIsa, kDisc };

enum DbgOpcode {
#define DBG_DEFINE(k, sub, n, o0, o1, o2) OP_DBG_##k##sub,
#include "dbg.def"
#undef DBG_DEFINE
  kOpDbgLast
};

class DbgInsn : public maplebe::Insn {
 private:
  static const char *dbg_names[kOpDbgLast + 1];
  DbgInsn &operator=(const DbgInsn &);

 public:
  explicit DbgInsn(maplebe::MOperator op) : Insn(op) {}

  explicit DbgInsn(maplebe::MOperator op, maplebe::Operand *opnd0) : Insn(op, opnd0) {}

  explicit DbgInsn(maplebe::MOperator op, maplebe::Operand *opnd0, maplebe::Operand *opnd1) : Insn(op, opnd0, opnd1) {}

  explicit DbgInsn(const DbgInsn &originalInsn, MemPool *mp)
    : Insn(originalInsn.mop_, originalInsn.opnds[0], originalInsn.opnds[1], originalInsn.opnds[2],
           originalInsn.opnds[3], originalInsn.opnds[4]) {
    prev = originalInsn.prev;
    next = originalInsn.next;
    bb = originalInsn.bb;
    flags = originalInsn.flags;
    mop_ = originalInsn.mop_;
    for (int i = 0; i < kMaxOperandNum; i++) {
      if (originalInsn.opnds[i] == nullptr) {
        opnds[i] = nullptr;
      } else {
        opnds[i] = originalInsn.opnds[i]->Clone(mp);
      }
    }
  }

  ~DbgInsn() {}

  inline maplebe::MOperator GetMachineOpcode() const {
    return mop_;
  }

  bool IsMachineInstruction() override {
    return false;
  }

  void Emit(maplebe::CG &cg, maplebe::Emitter &emitter) override;
  void dump() override;
  bool Check() override;
  bool IsDefinition() const override {
    return false;
  }

  bool IsDataMoveInstruction() const override {
    return false;
  }

  bool IsConversionInstruction() const override {
    return false;
  }

  bool IsConditionalSet() const override {
    return false;
  }

  uint32 GetLoc() const;
};

class ImmOperand : public maplebe::Operand {
 private:
  uint64 val_;

 public:
  explicit ImmOperand(int64 val) : Operand(Opd_Immediate, 32), val_(val) {}

  ~ImmOperand() {}

  mpldbg::ImmOperand *Clone(MemPool *mp) const override {
    return mp->Clone<mpldbg::ImmOperand>(*this);
  }

  void Emit(maplebe::Emitter &emitter, maplebe::OpndProp *) override;
  bool Less(Operand *right) const override {
    return false;
  }

  void dump() override;
  uint64 GetVal() const {
    return val_;
  }
};

}  // namespace mpldbg

#endif  // MAPLEBE_INCLUDE_CG_DBG_H
