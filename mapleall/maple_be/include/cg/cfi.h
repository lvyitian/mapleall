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

#ifndef MAPLEBE_INCLUDE_CG_CFI_H
#define MAPLEBE_INCLUDE_CG_CFI_H

#include "insn.h"
#include "mempool_allocator.h"

/*
   Reference:
   GNU Binutils. AS documentation
   https://sourceware.org/binutils/docs-2.28/as/index.html

   CFI blog
   https://www.imperialviolet.org/2017/01/18/cfi.html

   System V Application Binary Interface
   AMD64 Architecture Processor Supplement. Draft Version 0.99.7
   https://www.uclibc.org/docs/psABI-x86_64.pdf $ 3.7 Figure 3.36
   (RBP->6, RSP->7)

   System V Application Binary Interface
   Inte386 Architecture Processor Supplement. Version 1.0
   https://www.uclibc.org/docs/psABI-i386.pdf $ 2.5 Table 2.14
   (EBP->5, ESP->4)
 */

namespace maple {
class MIRSymbol;
}

namespace cfi {

enum CfiOpcode {
#define CFI_DEFINE(k, sub, n, o0, o1, o2) OP_CFI_##k##sub,
#include "cfi.def"
#undef CFI_DEFINE
  kOpCfiLast
};

class CfiInsn : public maplebe::Insn {
  static const char *cfi_names[kOpCfiLast + 1];

 public:
  explicit CfiInsn(maplebe::MOperator op) : Insn(op) {}

  explicit CfiInsn(maplebe::MOperator op, maplebe::Operand *opnd0) : Insn(op, opnd0) {}

  explicit CfiInsn(maplebe::MOperator op, maplebe::Operand *opnd0, maplebe::Operand *opnd1) : Insn(op, opnd0, opnd1) {}

  explicit CfiInsn(const CfiInsn &originalInsn, MemPool *mp)
    : Insn(originalInsn.mop_, originalInsn.opnds[0], originalInsn.opnds[1]) {
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

  ~CfiInsn() {}

  inline maplebe::MOperator GetMachineOpcode() const {
    return mop_;
  }

  bool IsMachineInstruction() override {
    return false;
  }

  static const char *GetCFINameStr(CfiOpcode op) {
    return cfi_names[op];
  }

  void Emit(maplebe::CG &cg, maplebe::Emitter &emitter) override;

  void dump() override;

  bool Check() override;

  bool IsDefinition() const override {
    return false;
  }

  bool IsUncondBranch() const override {
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

  bool IsCfiInsn() override {
    return true;
  }

 private:
  CfiInsn &operator=(const CfiInsn &);
};

class RegOperand : public maplebe::Operand {
  uint32 reg_no_;

 public:
  explicit RegOperand(uint32 no, uint8 size) : Operand(Opd_Register, size), reg_no_(no) {}

  ~RegOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<RegOperand>(*this);
  }

  void Emit(maplebe::Emitter &emitter, maplebe::OpndProp *) override;

  void dump() override;

  bool Less(Operand *right) const override {
    return false;
  }
};

class ImmOperand : public maplebe::Operand {
  int64 val_;

 public:
  explicit ImmOperand(int64 val, uint8 size) : Operand(Opd_Immediate, size), val_(val) {}

  ~ImmOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<ImmOperand>(*this);
  }

  void Emit(maplebe::Emitter &emitter, maplebe::OpndProp *) override;

  void dump() override;

  bool Less(Operand *right) const override {
    return false;
  }
};

class SymbolOperand : public maplebe::Operand {
  maple::MIRSymbol *symbol_;

 public:
  explicit SymbolOperand(maple::MIRSymbol *s, uint8 size) : Operand(Opd_StImmediate, size), symbol_(s) {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<SymbolOperand>(*this);
  }

  void Emit(maplebe::Emitter &, maplebe::OpndProp *) override;

  bool Less(Operand *right) const override {
    return false;
  }

  void dump() override {}
};

class StrOperand : public maplebe::Operand {
  const char *str_;

 public:
  explicit StrOperand(const char *s) : Operand(Opd_String, 0), str_(s) {}

  ~StrOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<StrOperand>(*this);
  }

  void Emit(maplebe::Emitter &emitter, maplebe::OpndProp *opndprop) override;

  bool Less(Operand *right) const override {
    return false;
  }

  void dump() override;
};

class LabelOperand : public maplebe::Operand {
  LabelIdx labidx_;

 public:
  explicit LabelOperand(const char *parent, LabelIdx labidx)
    : Operand(Opd_BbAddress, 0), labidx_(labidx) {}

  ~LabelOperand() {}

  Operand *Clone(MemPool *mp) const override {
    return mp->Clone<LabelOperand>(*this);
  }

  void Emit(maplebe::Emitter &emitter, maplebe::OpndProp *opndprop) override;

  bool Less(Operand *right) const override {
    return false;
  }

  void dump() override;
};

}  // namespace cfi

#endif  // MAPLEBE_INCLUDE_CG_CFI_H
