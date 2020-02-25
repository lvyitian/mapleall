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

#ifndef MAPLE_ME_INCLUDE_ME_CFG_OPT
#define MAPLE_ME_INCLUDE_ME_CFG_OPT
#include "me_function.h"

namespace maple {

class MeCfgOpt {
 public:
  MeIRMap *meirmap;
  bool isCFGChanged;

  MeCfgOpt(MeIRMap *mip) : meirmap(mip), isCFGChanged(false) {}

  bool Run(MeFunction *);
  bool IsCfgChanged() const {
    return isCFGChanged;
  }

  void SetCfgChanged() {
    isCFGChanged = true;
  }

  bool IsExpensiveOp(MeExpr *meexpr) {
    Opcode op = meexpr->op;
    if (op == OP_intrinsicop || op == OP_intrinsicopwithtype || op == OP_iread) {
      return true;
    }
    if (meexpr->primType == PTY_ref) {
      return true;
    }
    for (int32 i = 0; i < meexpr->NumMeExprOpnds(); i++) {
      if (!IsExpensiveOp(meexpr->GetOpnd(i))) {
        return true;
      }
    }
    return false;
  }

 private:
  bool PreCheck(MeFunction *);
  bool IsOk2Select(MeExpr *, MeExpr *);
  // collect expensive ops and if there is reference, return true
  static bool IsExpensiveOp(Opcode op) {
    switch (op) {
      case OP_abs:
      case OP_bnot:
      case OP_lnot:
      case OP_sqrt:
      case OP_neg:
        return false;
      case OP_recip:
      case OP_div:
      case OP_rem:
      case OP_alloca:
      case OP_malloc:
      case OP_gcmalloc:
      case OP_stackmalloc:
      case OP_gcmallocjarray:
      case OP_stackmallocjarray:
        return true;
      case OP_ceil:
      case OP_cvt:
      case OP_floor:
      case OP_retype:
      case OP_round:
      case OP_trunc:
        return false;
      case OP_sext:
      case OP_zext:
      case OP_extractbits:
        return false;
      case OP_iaddrof:
      case OP_iread:
        return true;
      case OP_ireadoff:
        return true;
      case OP_ireadfpoff:
        return true;
      case OP_add:
      case OP_ashr:
      case OP_band:
      case OP_bior:
      case OP_bxor:
      case OP_cand:
      case OP_cior:
      case OP_land:
      case OP_lior:
      case OP_lshr:
      case OP_max:
      case OP_min:
      case OP_mul:
      case OP_shl:
      case OP_sub:
        return false;
      case OP_CG_array_elem_add:
        return true;
      case OP_eq:
      case OP_ne:
      case OP_ge:
      case OP_gt:
      case OP_le:
      case OP_lt:
      case OP_cmp:
      case OP_cmpl:
      case OP_cmpg:
        return false;
      case OP_depositbits:
        return true;
      case OP_select:
        return false;
      case OP_intrinsicop:
      case OP_intrinsicopwithtype:
        return true;
      case OP_constval:
      case OP_conststr:
      case OP_conststr16:
        return false;
      case OP_sizeoftype:
        return false;
      case OP_array:
        return true;
      case OP_addrof:
      case OP_dread:
      case OP_regread:
      case OP_addroffunc:
      case OP_addroflabel:
        return false;
      // statement nodes start here
      default:
        CHECK_FATAL(false, "should be an expression or NYI");
    }
  }

  bool CollectExpensiveOps(MeExpr *meexpr, std::set<int32> &exprset) {
    if (IsExpensiveOp(meexpr->op)) {
      exprset.insert(meexpr->exprID);
    }
    PrimType primType = meexpr->primType;
    bool isref = (primType == PTY_ref || primType == PTY_ptr || primType == PTY_a64 || primType == PTY_a32);
    if (isref) {
      return true;
    }
    for (int32 i = 0; i < meexpr->NumMeExprOpnds(); i++) {
      if (!CollectExpensiveOps(meexpr->GetOpnd(i), exprset)) {
        isref = false;
      }
    }
    return isref;
  }

  bool HasFloatCmp(MeExpr *);
};

class MeDoCfgOpt : public MeFuncPhase {
 public:
  MeDoCfgOpt(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *, MeFuncResultMgr *) override;
  std::string PhaseName() const override {
    return "cfgopt";
  }

  void EmitMapleir(MeFunction *, MeFuncResultMgr *);
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_CFG_OPT
