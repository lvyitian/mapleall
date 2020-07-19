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

#include "constant_fold.h"
#include "mpl_logging.h"
#include "mir_function.h"
#include "mir_builder.h"
#include <cmath>  // for sqrt(), fabs()
#include <climits>

namespace maple {

// This phase is designed to achieve compiler optimization by
// simplifying constant expressions. The constant expression
// is evaluated and replaced by the value calculated on compile
// time to save time on runtime.
//
// The main procedure shows as following:
// A. Analyze expression type
// B. Analysis operator type
// C. Replace the expression with the result of the operation

BinaryNode *ConstantFold::NewBinaryNode(BinaryNode *old, Opcode op, PrimType primType, BaseNode *l, BaseNode *r) {
  BinaryNode *res = nullptr;
  if (old->op == op && old->primType == primType && old->Opnd(0) == l && old->Opnd(1) == r) {
    res = old;
  } else {
    res = module->CurFuncCodeMemPool()->New<BinaryNode>(op, primType, l, r);
  }
  return res;
}

UnaryNode *ConstantFold::NewUnaryNode(UnaryNode *old, Opcode op, PrimType primType, BaseNode *e) {
  UnaryNode *res = nullptr;
  if (old->op == op && old->primType == primType && old->Opnd(0) == e) {
    res = old;
  } else {
    res = module->CurFuncCodeMemPool()->New<UnaryNode>(op, primType, e);
  }
  return res;
}

BaseNode *ConstantFold::PairToExpr(PrimType resTyp, const std::pair<BaseNode *, int64> &p) {
  BaseNode *res = p.first;

  if (p.second != 0) {
    if (p.first->op == OP_neg && p.second > 0) {
      /* -a, 5 -> 5 - a */
      ConstvalNode *val = module->mirBuilder->CreateIntConst(p.second, resTyp);
      BaseNode *r = static_cast<UnaryNode *>(p.first)->uOpnd;
      res = module->CurFuncCodeMemPool()->New<BinaryNode>(OP_sub, resTyp, val, r);
    } else {
      if (p.second > 0) {
        /* +-a, 5 -> a + 5 */
        ConstvalNode *val = module->mirBuilder->CreateIntConst(p.second, resTyp);
        res = module->CurFuncCodeMemPool()->New<BinaryNode>(OP_add, resTyp, p.first, val);
      } else {
        /* +-a, -5 -> a + -5 */
        ConstvalNode *val = module->mirBuilder->CreateIntConst(-p.second, resTyp);
        res = module->CurFuncCodeMemPool()->New<BinaryNode>(OP_sub, resTyp, p.first, val);
      }
    }
  }
  return res;
}

std::pair<BaseNode *, int64> ConstantFold::FoldBase(BaseNode *n) const {
  return std::make_pair(n, 0);
}

StmtNode *ConstantFold::Simplify(StmtNode *x) {
  switch (x->op) {
    case OP_dassign:
    case OP_maydassign:
      return SimplifyDassign(static_cast<DassignNode *>(x));
    case OP_iassign:
      return SimplifyIassign(static_cast<IassignNode *>(x));
    case OP_block:
      return SimplifyBlock(static_cast<BlockNode *>(x));
    case OP_if:
      return SimplifyIf(static_cast<IfStmtNode *>(x));
    case OP_dowhile:
    case OP_while:
      return SimplifyWhile(static_cast<WhileStmtNode *>(x));
    case OP_switch:
      return SimplifySwitch(static_cast<SwitchNode *>(x));
    case OP_eval:
    case OP_throw:
    case OP_free:
    case OP_decref:
    case OP_incref:
    case OP_decrefreset:
    case OP_regassign:
    case OP_assertnonnull:
    case OP_igoto:
      return SimplifyUnary(static_cast<UnaryStmtNode *>(x));
    case OP_assertge:
    case OP_assertlt:
      return SimplifyBinary(static_cast<BinaryStmtNode *>(x));
    case OP_brfalse:
    case OP_brtrue:
      return SimplifyCondGoto(static_cast<CondGotoNode *>(x));
    case OP_return:
    case OP_syncenter:
    case OP_syncexit:
    case OP_call:
    case OP_virtualcall:
    case OP_superclasscall:
    case OP_interfacecall:
    case OP_customcall:
    case OP_polymorphiccall:
    case OP_intrinsiccall:
    case OP_xintrinsiccall:
    case OP_intrinsiccallwithtype:
    case OP_callassigned:
    case OP_virtualcallassigned:
    case OP_superclasscallassigned:
    case OP_interfacecallassigned:
    case OP_customcallassigned:
    case OP_polymorphiccallassigned:
    case OP_intrinsiccallassigned:
    case OP_intrinsiccallwithtypeassigned:
    case OP_xintrinsiccallassigned:
    case OP_callinstant:
    case OP_callinstantassigned:
    case OP_virtualcallinstant:
    case OP_virtualcallinstantassigned:
    case OP_superclasscallinstant:
    case OP_superclasscallinstantassigned:
    case OP_interfacecallinstant:
    case OP_interfacecallinstantassigned:
      return SimplifyNary(static_cast<NaryStmtNode *>(x));
    case OP_icall:
    case OP_icallassigned:
      return SimplifyIcall(static_cast<IcallNode *>(x));
    default:
      return x;
  }
}

std::pair<BaseNode *, int64> ConstantFold::DispatchFold(BaseNode *n) {
  switch (n->op) {
    case OP_sizeoftype:
      return FoldSizeoftype(static_cast<SizeoftypeNode *>(n));
    case OP_abs:
    case OP_bnot:
    case OP_lnot:
    case OP_neg:
    case OP_recip:
    case OP_sqrt:
      return FoldUnary(static_cast<UnaryNode *>(n));
    case OP_ceil:
    case OP_floor:
    case OP_round:
    case OP_trunc:
    case OP_cvt:
      return FoldTypeCvt(static_cast<TypeCvtNode *>(n));
    case OP_sext:
    case OP_zext:
    case OP_extractbits:
      return FoldExtractbits(static_cast<ExtractbitsNode *>(n));
    case OP_iaddrof:
    case OP_iread:
      return FoldIread(static_cast<IreadNode *>(n));
    case OP_add:
    case OP_ashr:
    case OP_band:
    case OP_bior:
    case OP_bxor:
    case OP_cand:
    case OP_cior:
    case OP_div:
    case OP_land:
    case OP_lior:
    case OP_lshr:
    case OP_max:
    case OP_min:
    case OP_mul:
    case OP_rem:
    case OP_shl:
    case OP_sub:
      return FoldBinary(static_cast<BinaryNode *>(n));
    case OP_eq:
    case OP_ne:
    case OP_ge:
    case OP_gt:
    case OP_le:
    case OP_lt:
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg:
      return FoldCompare(static_cast<CompareNode *>(n));
    case OP_depositbits:
      return FoldDepositbits(static_cast<DepositbitsNode *>(n));
    case OP_select:
      return FoldTernary(static_cast<TernaryNode *>(n));
    case OP_array:
      return FoldArray(static_cast<ArrayNode *>(n));
    case OP_retype:
      return FoldRetype(static_cast<RetypeNode *>(n));
    case OP_gcmallocjarray:
    case OP_gcpermallocjarray:
      return FoldGcmallocjarray(static_cast<JarrayMallocNode *>(n));
    default:
      return FoldBase(static_cast<BaseNode *>(n));
  }
}

BaseNode *ConstantFold::Negate(BaseNode *n) {
  return module->CurFuncCodeMemPool()->New<UnaryNode>(OP_neg, n->primType, n);
}

BaseNode *ConstantFold::Negate(UnaryNode *x) {
  BaseNode *res = nullptr;
  if (x->op == OP_neg) {
    res = static_cast<BaseNode *>(x->uOpnd);
  } else {
    BaseNode *n = static_cast<BaseNode *>(x);
    res = NewUnaryNode(x, OP_neg, n->primType, n);
  }
  return res;
}

BaseNode *ConstantFold::Negate(ConstvalNode *x) {
  ConstvalNode *copy = x->MakeCopy(module);
  CHECK_FATAL(copy, "null ptr check");
  copy->constVal->Neg();
  return copy;
}

BaseNode *ConstantFold::NegateTree(BaseNode *x) {
  if (x->IsUnaryNode()) {
    return Negate(static_cast<UnaryNode *>(x));
  } else if (x->op == OP_constval) {
    return Negate(static_cast<ConstvalNode *>(x));
  } else {
    return Negate(static_cast<BaseNode *>(x));
  }
}

MIRIntConst *ConstantFold::FoldIntConstComparisonMIRConst(Opcode opcode, PrimType resTyp, PrimType opndTyp,
                                                          const MIRIntConst *cst0, PrimType cst0ptyp,
                                                          const MIRIntConst *cst1, PrimType cst1ptyp) {
  uint64 result = 0;

  bool use64 = GetPrimTypeSize(opndTyp) == 8;
  switch (opcode) {
    case OP_eq: {
      if (use64) {
        result = cst0->value == cst1->value;
      } else {
        result = static_cast<int32>(cst0->value) == static_cast<int32>(cst1->value);
      }
      break;
    }
    case OP_ge: {
      if (IsUnsignedInteger(opndTyp)) {
        if (use64) {
          result = static_cast<uint64>(cst0->value) >= static_cast<uint64>(cst1->value);
        } else {
          result = static_cast<uint32>(cst0->value) >= static_cast<uint32>(cst1->value);
        }
      } else {
        if (use64) {
          result = cst0->value >= cst1->value;
        } else {
          result = static_cast<int32>(cst0->value) >= static_cast<int32>(cst1->value);
        }
      }
      break;
    }
    case OP_gt: {
      if (IsUnsignedInteger(opndTyp)) {
        if (use64) {
          result = static_cast<uint64>(cst0->value) > static_cast<uint64>(cst1->value);
        } else {
          result = static_cast<uint32>(cst0->value) > static_cast<uint32>(cst1->value);
        }
      } else {
        if (use64) {
          result = cst0->value > cst1->value;
        } else {
          result = static_cast<int32>(cst0->value) > static_cast<int32>(cst1->value);
        }
      }
      break;
    }
    case OP_le: {
      if (IsUnsignedInteger(opndTyp)) {
        if (use64) {
          result = static_cast<uint64>(cst0->value) <= static_cast<uint64>(cst1->value);
        } else {
          result = static_cast<uint32>(cst0->value) <= static_cast<uint32>(cst1->value);
        }
      } else {
        if (use64) {
          result = cst0->value <= cst1->value;
        } else {
          result = static_cast<int32>(cst0->value) <= static_cast<int32>(cst1->value);
        }
      }
      break;
    }
    case OP_lt: {
      if (IsUnsignedInteger(opndTyp)) {
        if (use64) {
          result = static_cast<uint64>(cst0->value) < static_cast<uint64>(cst1->value);
        } else {
          result = static_cast<uint32>(cst0->value) < static_cast<uint32>(cst1->value);
        }
      } else {
        if (use64) {
          result = cst0->value < cst1->value;
        } else {
          result = static_cast<int32>(cst0->value) < static_cast<int32>(cst1->value);
        }
      }
      break;
    }
    case OP_ne: {
      if (use64) {
        result = cst0->value != cst1->value;
      } else {
        result = static_cast<int32>(cst0->value) != static_cast<int32>(cst1->value);
      }
      break;
    }
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg: {
      if (IsUnsignedInteger(opndTyp)) {
        if (use64) {
          if (static_cast<uint64>(cst0->value) > static_cast<uint64>(cst1->value)) {
            result = 1;
          }
          if (static_cast<uint64>(cst0->value) == static_cast<uint64>(cst1->value)) {
            result = 0;
          }
          if (static_cast<uint64>(cst0->value) < static_cast<uint64>(cst1->value)) {
            result = -1;
          }
        } else {
          if (static_cast<uint32>(cst0->value) > static_cast<uint32>(cst1->value)) {
            result = 1;
          }
          if (static_cast<uint32>(cst0->value) == static_cast<uint32>(cst1->value)) {
            result = 0;
          }
          if (static_cast<uint32>(cst0->value) < static_cast<uint32>(cst1->value)) {
            result = -1;
          }
        }
      } else {
        if (use64) {
          if (cst0->value > cst1->value) {
            result = 1;
          }
          if (cst0->value == cst1->value) {
            result = 0;
          }
          if (cst0->value < cst1->value) {
            result = -1;
          }
        } else {
          if (static_cast<int32>(cst0->value) > static_cast<int32>(cst1->value)) {
            result = 1;
          }
          if (static_cast<int32>(cst0->value) == static_cast<int32>(cst1->value)) {
            result = 0;
          }
          if (static_cast<int32>(cst0->value) < static_cast<int32>(cst1->value)) {
            result = -1;
          }
        }
      }
      break;
    }
    default:
      ASSERT(false, "Unknown opcode for FoldIntConstComparison");
  }
  // determine the type
  MIRType *type = GlobalTables::GetTypeTable().GetPrimType(resTyp);
  // form the constant
  MIRIntConst *constVal = module->memPool->New<MIRIntConst>(result, type);
  return constVal;
}

ConstvalNode *ConstantFold::FoldIntConstComparison(Opcode opcode, PrimType resTyp, PrimType opndTyp, ConstvalNode *c0,
                                                   ConstvalNode *c1) {
  MIRIntConst *cst0 = static_cast<MIRIntConst *>(c0->constVal);
  MIRIntConst *cst1 = static_cast<MIRIntConst *>(c1->constVal);
  MIRIntConst *constVal = FoldIntConstComparisonMIRConst(opcode, resTyp, opndTyp, cst0, c0->primType, cst1, c1->primType);
  // form the ConstvalNode
  ConstvalNode *resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  resultConst->primType = resTyp;
  resultConst->constVal = constVal;
  return resultConst;
}

MIRConst *ConstantFold::FoldIntConstBinaryMIRConst(Opcode opcode, PrimType resTyp, MIRIntConst *cst0, MIRIntConst *cst1) {
  uint64 result64 = 0;
  uint32 result32 = 0;

  bool useResult64 = GetPrimTypeSize(resTyp) == 8;
  switch (opcode) {
    case OP_add: {
      if (useResult64) {
        result64 = cst0->value + cst1->value;
      } else {
        result32 = static_cast<int32>(cst0->value) + static_cast<int32>(cst1->value);
      }
      break;
    }
    case OP_sub: {
      if (useResult64) {
        result64 = cst0->value - cst1->value;
      } else {
        result32 = static_cast<int32>(cst0->value) - static_cast<int32>(cst1->value);
      }
      break;
    }
    case OP_mul: {
      if (useResult64) {
        result64 = cst0->value * cst1->value;
      } else {
        result32 = static_cast<int32>(cst0->value) * static_cast<int32>(cst1->value);
      }
      break;
    }
    case OP_div: {
      if (IsUnsignedInteger(resTyp)) {
        if (useResult64) {
          result64 = static_cast<uint64>(cst0->value) / static_cast<uint64>(cst1->value);
        } else {
          result32 = static_cast<uint32>(cst0->value) / static_cast<uint32>(cst1->value);
        }
      } else {
        if (useResult64) {
          result64 = cst0->value / cst1->value;
        } else {
          result32 = static_cast<int32>(cst0->value) / static_cast<int32>(cst1->value);
        }
      }
      break;
    }
    case OP_rem: {
      if (IsUnsignedInteger(resTyp)) {
        if (useResult64) {
          result64 = static_cast<uint64>(cst0->value) % static_cast<uint64>(cst1->value);
        } else {
          result32 = static_cast<uint32>(cst0->value) % static_cast<uint32>(cst1->value);
        }
      } else {
        if (useResult64) {
          result64 = cst0->value % cst1->value;
        } else {
          result32 = static_cast<int32>(cst0->value) % static_cast<int32>(cst1->value);
        }
      }
      break;
    }
    case OP_ashr: {
      if (useResult64) {
        result64 = cst0->value >> cst1->value;
      } else {
        result32 = static_cast<int32>(cst0->value) >> static_cast<int32>(cst1->value);
      }
      break;
    }
    case OP_lshr: {
      if (useResult64) {
        result64 = (static_cast<uint64>(cst0->value)) >> cst1->value;
      } else {
        result32 = static_cast<uint32>(cst0->value) >> static_cast<uint32>(cst1->value);
      }
      break;
    }
    case OP_shl: {
      if (useResult64) {
        result64 = static_cast<uint64>(cst0->value) << static_cast<uint64>(cst1->value);
      } else {
        result32 = static_cast<uint32>(cst0->value) << static_cast<uint32>(cst1->value);
      }
      break;
    }
    case OP_max: {
      if (IsUnsignedInteger(resTyp)) {
        if (useResult64) {
          result64 = (static_cast<uint64>(cst0->value) >= static_cast<uint64>(cst1->value)) ?
                      cst0->value : cst1->value;
        } else {
          result32 = (static_cast<uint32>(cst0->value) >= static_cast<uint32>(cst1->value)) ?
                      cst0->value : cst1->value;
        }
      } else {
        if (useResult64) {
          result64 = (cst0->value >= cst1->value) ? cst0->value : cst1->value;
        } else {
          result32 = (static_cast<int32>(cst0->value) >= static_cast<int32>(cst1->value)) ?
                      cst0->value : cst1->value;
        }
      }
      break;
    }
    case OP_min: {
      if (IsUnsignedInteger(resTyp)) {
        if (useResult64) {
          result64 = (static_cast<uint64>(cst0->value) <= static_cast<uint64>(cst1->value)) ?
                      cst0->value : cst1->value;
        } else {
          result32 = (static_cast<uint32>(cst0->value) <= static_cast<uint32>(cst1->value)) ?
                      cst0->value : cst1->value;
        }
      } else {
        if (useResult64) {
          result64 = (cst0->value <= cst1->value) ? cst0->value : cst1->value;
        } else {
          result32 = (static_cast<int32>(cst0->value) <= static_cast<int32>(cst1->value)) ?
                      cst0->value : cst1->value;
        }
      }
      break;
    }
    case OP_band: {
      if (useResult64) {
        result64 = static_cast<uint64>(cst0->value) & static_cast<uint64>(cst1->value);
      } else {
        result32 = static_cast<uint32>(cst0->value) & static_cast<uint32>(cst1->value);
      }
      break;
    }
    case OP_bior: {
      if (useResult64) {
        result64 = static_cast<uint64>(cst0->value) | static_cast<uint64>(cst1->value);
      } else {
        result32 = static_cast<uint32>(cst0->value) | static_cast<uint32>(cst1->value);
      }
      break;
    }
    case OP_bxor: {
      if (useResult64) {
        result64 = static_cast<uint64>(cst0->value) ^ static_cast<uint64>(cst1->value);
      } else {
        result32 = static_cast<uint32>(cst0->value) ^ static_cast<uint32>(cst1->value);
      }
      break;
    }
    case OP_cand:
    case OP_land: {
      if (useResult64) {
        result64 = static_cast<uint64>(cst0->value) && static_cast<uint64>(cst1->value);
      } else {
        result32 = static_cast<uint32>(cst0->value) && static_cast<uint32>(cst1->value);
      }
      break;
    }
    case OP_cior:
    case OP_lior: {
      if (useResult64) {
        result64 = static_cast<uint64>(cst0->value) || static_cast<uint64>(cst1->value);
      } else {
        result32 = static_cast<uint32>(cst0->value) || static_cast<uint32>(cst1->value);
      }
      break;
    }
    case OP_depositbits: {
      // handled in FoldDepositbits
      ASSERT(false, "Unexpected opcode in FoldIntConstBinary");
      break;
    }
    default:
      ASSERT(false, "Unknown opcode for FoldIntConstBinary");
  }
  // determine the type
  MIRType *type = GlobalTables::GetTypeTable().GetPrimType(resTyp);
  // form the constant
  MIRIntConst *constVal = nullptr;
  if (useResult64) {
    constVal = module->memPool->New<MIRIntConst>(result64, type);
  } else {
    constVal = module->memPool->New<MIRIntConst>(result32, type);
  }
  return constVal;
}

ConstvalNode *ConstantFold::FoldIntConstBinary(Opcode opcode, PrimType resTyp, ConstvalNode *c0, ConstvalNode *c1) {
  MIRIntConst *cst0 = static_cast<MIRIntConst *>(c0->constVal);
  MIRIntConst *cst1 = static_cast<MIRIntConst *>(c1->constVal);
  MIRConst *constVal = FoldIntConstBinaryMIRConst(opcode, resTyp, cst0, cst1);
  // form the ConstvalNode
  ConstvalNode *resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  resultConst->primType = resTyp;
  resultConst->constVal = constVal;
  return resultConst;
}

ConstvalNode *ConstantFold::FoldFPConstBinary(Opcode opcode, PrimType resTyp, ConstvalNode *c0, ConstvalNode *c1) {
  ASSERT(c0->primType == c1->primType, "The types of the operands must match");
  MIRDoubleConst *cst0D = nullptr, *cst1D = nullptr;
  MIRFloatConst *cst0F = nullptr, *cst1F = nullptr;
  bool useDouble = PTY_f64 == c0->primType;
  ConstvalNode *resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  resultConst->primType = resTyp;
  if (useDouble) {
    cst0D = static_cast<MIRDoubleConst *>(c0->constVal);
    cst1D = static_cast<MIRDoubleConst *>(c1->constVal);
    CHECK_FATAL(cst0D && cst1D, "pointer is null in ConstantFold::FoldFPConstBinary");
  } else {
    cst0F = static_cast<MIRFloatConst *>(c0->constVal);
    cst1F = static_cast<MIRFloatConst *>(c1->constVal);
    CHECK_FATAL(cst0F && cst1F, "pointer is null in ConstantFold::FoldFPConstBinary");
  }
  float constvalF = 0.0;
  double constvalD = 0.0;

  switch (opcode) {
    case OP_add: {
      if (useDouble) {
        constvalD = cst0D->GetValue() + cst1D->GetValue();
      } else {
        constvalF = cst0F->GetFloatValue() + cst1F->GetFloatValue();
      }
      break;
    }
    case OP_sub: {
      if (useDouble) {
        constvalD = cst0D->GetValue() - cst1D->GetValue();
      } else {
        constvalF = cst0F->GetFloatValue() - cst1F->GetFloatValue();
      }
      break;
    }
    case OP_mul: {
      if (useDouble) {
        constvalD = cst0D->GetValue() * cst1D->GetValue();
      } else {
        constvalF = cst0F->GetFloatValue() * cst1F->GetFloatValue();
      }
      break;
    }
    case OP_div: {
      // for floats div by 0 is well defined
      if (useDouble) {
        constvalD = cst0D->GetValue() / cst1D->GetValue();
      } else {
        constvalF = cst0F->GetFloatValue() / cst1F->GetFloatValue();
      }
      break;
    }
    case OP_max: {
      if (useDouble)
        constvalD = (cst0D->GetValue() >= cst1D->GetValue()) ? cst0D->GetValue() : cst1D->GetValue();
      else
        constvalF = (cst0F->GetFloatValue() >= cst1F->GetFloatValue()) ? cst0F->GetFloatValue() : cst1F->GetFloatValue();
      break;
    }
    case OP_min: {
      if (useDouble)
        constvalD = (cst0D->GetValue() <= cst1D->GetValue()) ? cst0D->GetValue() : cst1D->GetValue();
      else
        constvalF = (cst0F->GetFloatValue() <= cst1F->GetFloatValue()) ? cst0F->GetFloatValue() : cst1F->GetFloatValue();
      break;
    }
    case OP_rem:
    case OP_ashr:
    case OP_lshr:
    case OP_shl:
    case OP_band:
    case OP_bior:
    case OP_bxor:
    case OP_cand:
    case OP_land:
    case OP_cior:
    case OP_lior:
    case OP_depositbits: {
      ASSERT(false, "Unexpected opcode in FoldFPConstBinary");
      break;
    }
    default:
      ASSERT(false, "Unknown opcode for FoldFPConstBinary");
  }
  if (resTyp == PTY_f64) {
    resultConst->constVal = GlobalTables::GetFpConstTable().GetOrCreateDoubleConst(constvalD);
  } else {
    resultConst->constVal = GlobalTables::GetFpConstTable().GetOrCreateFloatConst(constvalF);
  }
  return resultConst;
}

MIRIntConst *ConstantFold::FoldFPConstComparisonMIRConst(Opcode opcode, PrimType resTyp, PrimType opndTyp,
                                                         MIRConst *c0constval, PrimType c0ptyp, MIRConst *c1constval,
                                                         PrimType c1ptyp) {
  ASSERT(c0ptyp == c1ptyp, "The types of the operands must match");
  MIRDoubleConst *cst0D = nullptr, *cst1D = nullptr;
  MIRFloatConst *cst0F = nullptr, *cst1F = nullptr;
  bool useDouble = PTY_f64 == opndTyp;
  if (useDouble) {
    cst0D = static_cast<MIRDoubleConst *>(c0constval);
    CHECK_FATAL(cst0D != nullptr, "cst0D is nullptr");
    cst1D = static_cast<MIRDoubleConst *>(c1constval);
    CHECK_FATAL(cst1D != nullptr, "cst1D is nullptr");
  } else {
    cst0F = static_cast<MIRFloatConst *>(c0constval);
    CHECK_FATAL(cst0F != nullptr, "cst0F is nullptr");
    cst1F = static_cast<MIRFloatConst *>(c1constval);
    CHECK_FATAL(cst1F != nullptr, "cst1F is nullptr");
  }
  MIRType *type = GlobalTables::GetTypeTable().GetPrimType(resTyp);

  int64 constvalI = 0;

  switch (opcode) {
    case OP_eq: {
      if (useDouble) {
        constvalI = (cst0D->GetValue() == cst1D->GetValue()) ? 1 : 0;
      } else {
        constvalI = (cst0F->GetFloatValue() == cst1F->GetFloatValue()) ? 1 : 0;
      }
      break;
    }
    case OP_ge: {
      if (useDouble) {
        constvalI = (cst0D->GetValue() >= cst1D->GetValue()) ? 1 : 0;
      } else {
        constvalI = (cst0F->GetFloatValue() >= cst1F->GetFloatValue()) ? 1 : 0;
      }
      break;
    }
    case OP_gt: {
      if (useDouble) {
        constvalI = (cst0D->GetValue() > cst1D->GetValue()) ? 1 : 0;
      } else {
        constvalI = (cst0F->GetFloatValue() > cst1F->GetFloatValue()) ? 1 : 0;
      }
      break;
    }
    case OP_le: {
      if (useDouble) {
        constvalI = (cst0D->GetValue() <= cst1D->GetValue()) ? 1 : 0;
      } else {
        constvalI = (cst0F->GetFloatValue() <= cst1F->GetFloatValue()) ? 1 : 0;
      }
      break;
    }
    case OP_lt: {
      if (useDouble) {
        constvalI = (cst0D->GetValue() < cst1D->GetValue()) ? 1 : 0;
      } else {
        constvalI = (cst0F->GetFloatValue() < cst1F->GetFloatValue()) ? 1 : 0;
      }
      break;
    }
    case OP_ne: {
      if (useDouble) {
        constvalI = (cst0D->GetValue() != cst1D->GetValue()) ? 1 : 0;
      } else {
        constvalI = (cst0F->GetFloatValue() != cst1F->GetFloatValue()) ? 1 : 0;
      }
      break;
    }
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg: {
      if (useDouble) {
        CHECK_FATAL(cst0D != nullptr && cst1D != nullptr, "pointer is nullptr in ConstantFold::FoldFPConstComparison");
        if (cst0D->GetValue() > cst1D->GetValue()) {
          constvalI = 1;
        } else if (cst0D->GetValue() == cst1D->GetValue()) {
          constvalI = 0;
        } else if (cst0D->GetValue() < cst1D->GetValue()) {
          constvalI = -1;
        }
      } else {
        if (cst0F->GetFloatValue() > cst1F->GetFloatValue()) {
          constvalI = 1;
        } else if (cst0F->GetFloatValue() == cst1F->GetFloatValue()) {
          constvalI = 0;
        } else if (cst0F->GetFloatValue() < cst1F->GetFloatValue()) {
          constvalI = -1;
        }
      }
      break;
    }
    default:
      ASSERT(false, "Unknown opcode for FoldFPConstComparison");
  }
  return module->memPool->New<MIRIntConst>(constvalI, type);
}

ConstvalNode *ConstantFold::FoldFPConstComparison(Opcode opcode, PrimType resTyp, PrimType opndTyp, ConstvalNode *c0,
                                                  ConstvalNode *c1) {
  ASSERT(c0->primType == c1->primType, "The types of the operands must match");
  ConstvalNode *resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  resultConst->primType = resTyp;
  resultConst->constVal =
    FoldFPConstComparisonMIRConst(opcode, resTyp, opndTyp, c0->constVal, c0->primType, c1->constVal, c1->primType);
  return resultConst;
}

MIRConst *ConstantFold::FoldConstComparisonMIRConst(Opcode opcode, PrimType resTyp, PrimType opndTyp, MIRConst *c0,
                                                    PrimType c0ptyp, MIRConst *c1, PrimType c1ptyp) {
  MIRConst *retval = nullptr;
  if (IsPrimitiveInteger(opndTyp) || IsPrimitiveDynInteger(opndTyp)) {
    retval = FoldIntConstComparisonMIRConst(opcode, resTyp, opndTyp, static_cast<MIRIntConst *>(c0), c0ptyp,
                                            static_cast<MIRIntConst *>(c1), c1ptyp);
  } else if (PTY_f32 == opndTyp || PTY_f64 == opndTyp) {
    retval = FoldFPConstComparisonMIRConst(opcode, resTyp, opndTyp, c0, c0ptyp, c1, c1ptyp);
  } else {
    ASSERT(false, "Unhandled case for FoldConstComparisonMIRConst");
  }
  return retval;
}

ConstvalNode *ConstantFold::FoldConstComparison(Opcode opcode, PrimType resTyp, PrimType opndTyp, ConstvalNode *c0,
                                                ConstvalNode *c1) {
  ConstvalNode *retval = nullptr;
  if (IsPrimitiveInteger(opndTyp) || IsPrimitiveDynInteger(opndTyp)) {
    retval = FoldIntConstComparison(opcode, resTyp, opndTyp, c0, c1);
  } else if (PTY_f32 == opndTyp || PTY_f64 == opndTyp) {
    retval = FoldFPConstComparison(opcode, resTyp, opndTyp, c0, c1);
  } else {
    ASSERT(false, "Unhandled case for FoldConstComparison");
  }
  return retval;
}

ConstvalNode *ConstantFold::FoldConstBinary(Opcode opcode, PrimType resTyp, ConstvalNode *c0, ConstvalNode *c1) {
  ConstvalNode *retval = nullptr;
  if (IsPrimitiveInteger(resTyp) || IsPrimitiveDynInteger(resTyp)) {
    retval = FoldIntConstBinary(opcode, resTyp, c0, c1);
  } else if (PTY_f32 == resTyp || PTY_f64 == resTyp) {
    retval = FoldFPConstBinary(opcode, resTyp, c0, c1);
  } else {
    ASSERT(false, "Unhandled case for FoldConstBinary");
  }
  return retval;
}

ConstvalNode *ConstantFold::FoldIntConstUnary(Opcode opcode, PrimType resTyp, ConstvalNode *c) {
  MIRIntConst *cst = static_cast<MIRIntConst *>(c->constVal);
  uint32 result32 = 0;
  uint64 result64 = 0;
  bool useResult64 = GetPrimTypeSize(resTyp) == 8;
  switch (opcode) {
    case OP_abs: {
      if (IsUnsignedInteger(resTyp)) {
        if (useResult64) {
          result64 = cst->value;
        } else {
          result32 = cst->value;
        }
      } else {
        if (useResult64) {
          result64 = (cst->value >= 0) ? cst->value : -cst->value;
        } else {
          result32 = (static_cast<int32>(cst->value) >= 0) ? cst->value : -static_cast<int32>(cst->value);
        }
      }
      break;
    }
    case OP_bnot: {
      if (useResult64) {
        result64 = ~cst->value;
      } else {
        result32 = ~static_cast<uint32>(cst->value);
      }
      break;
    }
    case OP_lnot: {
      if (useResult64) {
        result64 = cst->value == 0;
      } else {
        result32 = static_cast<uint32>(cst->value) == 0;
      }
      break;
    }
    case OP_neg: {
      if (useResult64) {
        result64 = -cst->value;
      } else {
        result32 = -static_cast<int32>(cst->value);
      }
      break;
    }
    case OP_sext:         // handled in FoldExtractbits
    case OP_zext:         // handled in FoldExtractbits
    case OP_extractbits:  // handled in FoldExtractbits
    case OP_recip:
    case OP_sqrt: {
      ASSERT(false, "Unexpected opcode in FoldIntConstUnary");
      break;
    }
    default:
      ASSERT(false, "Unknown opcode for FoldIntConstUnary");
  }
  // determine the type
  MIRType *type = GlobalTables::GetTypeTable().GetPrimType(resTyp);
  // form the constant
  MIRIntConst *constVal = nullptr;
  if (useResult64) {
    constVal = module->memPool->New<MIRIntConst>(result64, type);
  } else {
    constVal = module->memPool->New<MIRIntConst>(result32, type);
  }
  // form the ConstvalNode
  ConstvalNode *resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  resultConst->primType = type->primType;
  resultConst->constVal = constVal;
  return resultConst;
}

template <typename T>
ConstvalNode *ConstantFold::FoldFPConstUnary(Opcode opcode, PrimType resTyp, ConstvalNode *c) {
  ConstvalNode *resultConst = c;
  typename T::value_type constVal = 0.0;
  T *fpCst = static_cast<T *>(c->constVal);

  switch (opcode) {
    case OP_recip:
    case OP_neg:
    case OP_abs: {
      if (OP_recip == opcode) {
        constVal = typename T::value_type(1.0 / fpCst->GetValue());
      } else if (OP_neg == opcode) {
        constVal = typename T::value_type(-fpCst->GetValue());
      } else if (OP_abs == opcode) {
        constVal = typename T::value_type(fabs(fpCst->GetValue()));
      }
      break;
    }
    case OP_sqrt: {
      constVal = typename T::value_type(sqrt(fpCst->GetValue()));
      break;
    }
    case OP_bnot:
    case OP_lnot:
    case OP_sext:
    case OP_zext:
    case OP_extractbits: {
      ASSERT(false, "Unexpected opcode in FoldFPConstUnary");
      break;
    }
    default:
      ASSERT(false, "Unknown opcode for FoldFPConstUnary");
  }
  resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  resultConst->primType = resTyp;
  if (resTyp == PTY_f32) {
    resultConst->constVal = GlobalTables::GetFpConstTable().GetOrCreateFloatConst(constVal);
  } else {
    resultConst->constVal = GlobalTables::GetFpConstTable().GetOrCreateDoubleConst(constVal);
  }
  return resultConst;
}

ConstvalNode *ConstantFold::FoldConstUnary(Opcode opcode, PrimType resTyp, ConstvalNode *c) {
  ConstvalNode *retval = nullptr;
  if (IsPrimitiveInteger(resTyp) || IsPrimitiveDynInteger(resTyp)) {
    retval = FoldIntConstUnary(opcode, resTyp, c);
  } else if (PTY_f32 == resTyp) {
    retval = FoldFPConstUnary<MIRFloatConst>(opcode, resTyp, c);
  } else if (PTY_f64 == resTyp) {
    retval = FoldFPConstUnary<MIRDoubleConst>(opcode, resTyp, c);
  } else if (PTY_f128 == resTyp) {
  } else {
    ASSERT(false, "Unhandled case for FoldConstUnary");
  }
  return retval;
}

std::pair<BaseNode *, int64> ConstantFold::FoldSizeoftype(SizeoftypeNode *n) {
  BaseNode *res = n;
  MIRType *argType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(n->tyIdx);
  if (argType->typeKind == kTypeScalar) {
    MIRType *resType = GlobalTables::GetTypeTable().GetPrimType(n->primType);
    uint32 size = GetPrimTypeSize(argType->primType);
    ConstvalNode *cn = module->CurFuncCodeMemPool()->New<ConstvalNode>();
    cn->primType = n->primType;
    cn->constVal = module->memPool->New<MIRIntConst>(static_cast<int64>(size), resType);
    res = cn;
  }
  return std::make_pair(res, 0);
}

std::pair<BaseNode *, int64> ConstantFold::FoldRetype(RetypeNode *n) {
  BaseNode *res = n;
  std::pair<BaseNode *, int64> p = DispatchFold(n->Opnd(0));
  if (n->Opnd(0) != p.first) {
    RetypeNode *newRetNode;
    newRetNode = n->MakeCopy(module);
    newRetNode->uOpnd = PairToExpr(n->Opnd(0)->primType, p);
    res = newRetNode;
  }
  return std::make_pair(res, 0);
}

std::pair<BaseNode *, int64> ConstantFold::FoldGcmallocjarray(JarrayMallocNode *n) {
  BaseNode *res = n;
  std::pair<BaseNode *, int64> p = DispatchFold(n->Opnd(0));
  if (n->Opnd(0) != p.first) {
    JarrayMallocNode *newRetNode;
    newRetNode = n->MakeCopy(module);
    CHECK_FATAL(newRetNode != nullptr, "newRetNode is null in ConstantFold::FoldGcmallocjarray");
    newRetNode->uOpnd = PairToExpr(n->Opnd(0)->primType, p);
    res = newRetNode;
  }
  return std::make_pair(res, 0);
}

std::pair<BaseNode *, int64> ConstantFold::FoldUnary(UnaryNode *n) {
  BaseNode *res = nullptr;
  int64 sum;

  std::pair<BaseNode *, int64> p = DispatchFold(n->Opnd(0));
  ConstvalNode *cnst = dynamic_cast<ConstvalNode *>(p.first);

  if (cnst) {
    res = FoldConstUnary(n->op, n->primType, cnst);
    sum = 0;
  } else {
    bool isInt = IsPrimitiveInteger(n->primType);
    if (isInt && n->op == OP_neg) {
      res = NegateTree(p.first);
      if (res->op == OP_neg && res->primType == n->primType && static_cast<UnaryNode *>(res)->Opnd(0) == n->Opnd(0)) {
        // NegateTree returned an UnaryNode quivalent to `n`, so keep the
        // original UnaryNode to preserve identity
        res = n;
      }
      sum = -p.second;
    } else {
      res = NewUnaryNode(n, n->op, n->primType, PairToExpr(n->Opnd(0)->primType, p));
      sum = 0;
    }
  }
  return std::make_pair(res, sum);
}

static bool FloatToIntOverflow(float fval, PrimType totype) {
  static const float safeFloatMaxToInt32 = 2147483520.0f; // 2^31 - 128
  static const float safeFloatMinToInt32 = -2147483520.0f;
  static const float safeFloatMaxToInt64 = 9223372036854775680.0f; // 2^63 - 128
  static const float safeFloatMinToInt64 = -9223372036854775680.0f;
  if (!std::isfinite(fval)) {
    return true;
  }
  if (totype == PTY_i64 || totype == PTY_u64) {
    if (fval < safeFloatMinToInt64 || fval > safeFloatMaxToInt64) {
      return true;
    }
  } else {
    if (fval < safeFloatMinToInt32 || fval > safeFloatMaxToInt32) {
      return true;
    }
  }
  return false;
}

static bool DoubleToIntOverflow(double dval, PrimType totype) {
  static const double safeDoubleMaxToInt32 = 2147482624.0; // 2^31 - 1024
  static const double safeDoubleMinToInt32 = -2147482624.0;
  static const double safeDoubleMaxToInt64 = 9223372036854774784.0; // 2^63 - 1024
  static const double safeDoubleMinToInt64 = -9223372036854774784.0;
  if (!std::isfinite(dval)) {
    return true;
  }
  if (totype == PTY_i64 || totype == PTY_u64) {
    if (dval < safeDoubleMinToInt64 || dval > safeDoubleMaxToInt64) {
      return true;
    }
  } else {
    if (dval < safeDoubleMinToInt32 || dval > safeDoubleMaxToInt32) {
      return true;
    }
  }
  return false;
}

ConstvalNode *ConstantFold::FoldCeil(ConstvalNode *cst, PrimType fromtype, PrimType totype) {
  ConstvalNode *resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  resultConst->primType = totype;
  MIRType *restype = GlobalTables::GetTypeTable().GetPrimType(totype);
  if (PTY_f32 == fromtype) {
    MIRFloatConst *cstVal = static_cast<MIRFloatConst *>(cst->constVal);
    float fval = ceil(cstVal->GetFloatValue());
    if (FloatToIntOverflow(fval, totype)) {
      return nullptr;
    }
    resultConst->constVal = module->memPool->New<MIRIntConst>(static_cast<int64>(fval), restype);
  } else {
    MIRDoubleConst *cstVal = static_cast<MIRDoubleConst *>(cst->constVal);
    double dval = ceil(cstVal->GetValue());
    if (DoubleToIntOverflow(dval, totype)) {
      return nullptr;
    }
    resultConst->constVal = module->memPool->New<MIRIntConst>(static_cast<int64>(dval), restype);
  }
  return resultConst;
}

MIRConst *ConstantFold::FoldFloorMIRConst(MIRConst *cst, PrimType fromtype, PrimType totype) {
  MIRType *restype = GlobalTables::GetTypeTable().GetPrimType(totype);
  if (PTY_f32 == fromtype) {
    MIRFloatConst *cstVal = static_cast<MIRFloatConst *>(cst);
    float fval = floor(cstVal->GetFloatValue());
    if (FloatToIntOverflow(fval, totype)) {
      return nullptr;
    }
    return module->memPool->New<MIRIntConst>(static_cast<int64>(fval), restype);
  } else {
    MIRDoubleConst *cstVal = static_cast<MIRDoubleConst *>(cst);
    double dval = floor(cstVal->GetValue());
    if (DoubleToIntOverflow(dval, totype)) {
      return nullptr;
    }
    return module->memPool->New<MIRIntConst>(static_cast<int64>(dval), restype);
  }
}

ConstvalNode *ConstantFold::FoldFloor(ConstvalNode *cst, PrimType fromtype, PrimType totype) {
  ConstvalNode *resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  resultConst->primType = totype;
  resultConst->constVal = FoldFloorMIRConst(cst->constVal, fromtype, totype);
  return resultConst;
}

MIRConst *ConstantFold::FoldRoundMIRConst(MIRConst *cst, PrimType fromtype, PrimType totype) {
  MIRType *restype = GlobalTables::GetTypeTable().GetPrimType(totype);
  if (PTY_f32 == fromtype) {
    MIRFloatConst *cstVal = static_cast<MIRFloatConst *>(cst);
    float fval = round(cstVal->GetFloatValue());
    if (FloatToIntOverflow(fval, totype)) {
      return nullptr;
    }
    return module->memPool->New<MIRIntConst>(static_cast<int64>(fval), restype);
  } else if (PTY_f64 == fromtype) {
    MIRDoubleConst *cstVal = static_cast<MIRDoubleConst *>(cst);
    double dval = round(cstVal->GetValue());
    if (DoubleToIntOverflow(dval, totype)) {
      return nullptr;
    }
    return module->memPool->New<MIRIntConst>(static_cast<int64>(dval), restype);
  } else if (PTY_f32 == totype && IsPrimitiveInteger(fromtype)) {
    MIRIntConst *cstVal = static_cast<MIRIntConst *>(cst);
    if (IsSignedInteger(fromtype)) {
      int64 fromval = cstVal->value;
      float fval = round(static_cast<float>(fromval));
      if (static_cast<int64>(fval) == fromval) {
        return GlobalTables::GetFpConstTable().GetOrCreateFloatConst(fval);
      }
    } else {
      uint64 fromval = cstVal->value;
      float fval = round(static_cast<float>(fromval));
      if (static_cast<uint64>(fval) == fromval) {
        return GlobalTables::GetFpConstTable().GetOrCreateFloatConst(fval);
      }
    }
  } else if (PTY_f64 == totype && IsPrimitiveInteger(fromtype)) {
    MIRIntConst *cstVal = static_cast<MIRIntConst *>(cst);
    if (IsSignedInteger(fromtype)) {
      int64 fromval = cstVal->value;
      double dval = round(static_cast<double>(fromval));
      if (static_cast<int64>(dval) == fromval) {
        return GlobalTables::GetFpConstTable().GetOrCreateDoubleConst(dval);
      }
    } else {
      uint64 fromval = cstVal->value;
      double dval = round(static_cast<double>(fromval));
      if (static_cast<uint64>(dval) == fromval) {
        return GlobalTables::GetFpConstTable().GetOrCreateDoubleConst(dval);
      }
    }
  }
  return nullptr;
}

ConstvalNode *ConstantFold::FoldRound(ConstvalNode *cst, PrimType fromtype, PrimType totype) {
  ConstvalNode *resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  resultConst->primType = totype;
  resultConst->constVal = FoldRoundMIRConst(cst->constVal, fromtype, totype);
  return resultConst;
}

ConstvalNode *ConstantFold::FoldTrunc(ConstvalNode *cst, PrimType fromtype, PrimType totype) {
  ConstvalNode *resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  resultConst->primType = totype;
  MIRType *restype = GlobalTables::GetTypeTable().GetPrimType(totype);
  if (PTY_f32 == fromtype) {
    MIRFloatConst *cstVal = static_cast<MIRFloatConst *>(cst->constVal);
    float fval = truncf(cstVal->GetFloatValue());
    if (FloatToIntOverflow(fval, totype)) {
      return nullptr;
    }
    resultConst->constVal = module->memPool->New<MIRIntConst>(static_cast<int64>(fval), restype);
  } else {
    MIRDoubleConst *cstVal = static_cast<MIRDoubleConst *>(cst->constVal);
    double dval = trunc(cstVal->GetValue());
    if (DoubleToIntOverflow(dval, totype)) {
      return nullptr;
    }
    resultConst->constVal = module->memPool->New<MIRIntConst>(static_cast<int64>(dval), restype);
  }
  return resultConst;
}

MIRConst *ConstantFold::FoldTypeCvtMIRConst(MIRConst *cst, PrimType fromtype, PrimType totype) {
  if (IsPrimitiveDynType(fromtype) || IsPrimitiveDynType(totype)) {
    // do not fold
    return nullptr;
  }
  if (IsPrimitiveInteger(fromtype) && IsPrimitiveInteger(totype)) {
    MIRConst *tocst = nullptr;
    uint32 fromSize = GetPrimTypeBitSize(fromtype);
    uint32 toSize = GetPrimTypeBitSize(totype);
    if (toSize > fromSize) {
      Opcode op = OP_zext;
      if (IsSignedInteger(fromtype)) {
        op = OP_sext;
      }
      tocst = FoldSignExtendMIRConst(op, totype, fromSize, cst);
    } else {
      MIRIntConst *c = static_cast<MIRIntConst *>(cst);
      MIRType *type = GlobalTables::GetTypeTable().GetPrimType(totype);
      tocst = module->memPool->New<MIRIntConst>(c->value, type);
    }
    return tocst;
  }
  if (IsPrimitiveFloat(fromtype) && IsPrimitiveFloat(totype)) {
    MIRConst *tocst = nullptr;
    if (GetPrimTypeBitSize(totype) < GetPrimTypeBitSize(fromtype)) {
      ASSERT(GetPrimTypeBitSize(totype) == 32, "We suppot F32 and F64");
      MIRDoubleConst *fromval = static_cast<MIRDoubleConst *>(cst);
      float fval = static_cast<float>(fromval->GetValue());
      MIRFloatConst *toval = GlobalTables::GetFpConstTable().GetOrCreateFloatConst(fval);
      tocst = toval;
    } else {
      ASSERT(GetPrimTypeBitSize(totype) == 64, "We suppot F32 and F64");
      MIRFloatConst *fromval = static_cast<MIRFloatConst *>(cst);
      double dval = static_cast<double>(fromval->GetFloatValue());
      MIRDoubleConst *toval = GlobalTables::GetFpConstTable().GetOrCreateDoubleConst(dval);
      tocst = toval;
    }
    return tocst;
  }
  if (IsPrimitiveFloat(fromtype) && IsPrimitiveInteger(totype)) {
    return FoldFloorMIRConst(cst, fromtype, totype);
  }
  if (IsPrimitiveInteger(fromtype) && IsPrimitiveFloat(totype)) {
    return FoldRoundMIRConst(cst, fromtype, totype);
  }
  CHECK_FATAL(false, "Unexpected case in ConstFoldTypeCvt");
  return nullptr;
}

ConstvalNode *ConstantFold::FoldTypeCvt(ConstvalNode *cst, PrimType fromtype, PrimType totype) {
  MIRConst *tocstval = FoldTypeCvtMIRConst(cst->constVal, fromtype, totype);
  if (!tocstval) {
    return nullptr;
  }
  ConstvalNode *tocst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  tocst->primType = tocstval->type->primType;
  tocst->constVal = tocstval;
  return tocst;
}

std::pair<BaseNode *, int64> ConstantFold::FoldTypeCvt(TypeCvtNode *n) {
  BaseNode *res = nullptr;

  std::pair<BaseNode *, int64> p = DispatchFold(n->uOpnd);

  ConstvalNode *cst = dynamic_cast<ConstvalNode *>(p.first);
  if (cst) {
    switch (n->op) {
      case OP_ceil: {
        res = FoldCeil(cst, n->fromPrimType, n->primType);
        break;
      }
      case OP_cvt: {
        res = FoldTypeCvt(cst, n->fromPrimType, n->primType);
        break;
      }
      case OP_floor: {
        res = FoldFloor(cst, n->fromPrimType, n->primType);
        break;
      }
      case OP_round: {
        res = FoldRound(cst, n->fromPrimType, n->primType);
        break;
      }
      case OP_trunc: {
        res = FoldTrunc(cst, n->fromPrimType, n->primType);
        break;
      }
      default:
        res = nullptr;
        ASSERT(false, "Unexpected opcode in TypeCvtNodeConstFold");
    }
  } else if (n->op == OP_cvt) {
    if ((IsPossible64BitAddress(n->fromPrimType) && IsPossible64BitAddress(n->primType)) ||
        (IsPossible32BitAddress(n->fromPrimType) && IsPossible32BitAddress(n->primType))) {
      return p; // the cvt is redundant
    }
  }

  if (res == nullptr) {
    BaseNode *e = PairToExpr(n->uOpnd->primType, p);
    if (e != n->uOpnd) {
      res = module->CurFuncCodeMemPool()->New<TypeCvtNode>(n->op, n->primType, n->fromPrimType, e);
    } else {
      res = n;
    }
  }
  return std::make_pair(res, 0);
}

MIRConst *ConstantFold::FoldSignExtendMIRConst(Opcode opcode, PrimType restype, uint8 size, MIRConst *cst) {
  MIRIntConst *c = static_cast<MIRIntConst *>(cst);
  uint64 result64;
  if (opcode == OP_sext) {
    result64 = (c->value << (64 - size)) >> (64 - size);
  } else {
    result64 = ((static_cast<uint64>(c->value)) << (64 - size)) >> (64 - size);
  }
  MIRType *type = GlobalTables::GetTypeTable().GetPrimType(restype);
  MIRIntConst *constVal = module->memPool->New<MIRIntConst>(result64, type);
  return constVal;
}

ConstvalNode *ConstantFold::FoldSignExtend(Opcode opcode, PrimType restype, uint8 size, ConstvalNode *cst) {
  ConstvalNode *resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
  MIRConst *tocst = FoldSignExtendMIRConst(opcode, restype, size, cst->constVal);
  resultConst->primType = tocst->type->primType;
  resultConst->constVal = tocst;
  return resultConst;
}

// sext and zext also handled automatically
std::pair<BaseNode *, int64> ConstantFold::FoldExtractbits(ExtractbitsNode *n) {
  BaseNode *res = nullptr;
  uint8 offset = n->bitsOffset;
  uint8 size = n->bitsSize;
  Opcode opcode = n->op;
  std::pair<BaseNode *, int64> p = DispatchFold(n->Opnd(0));

  ConstvalNode *cst = dynamic_cast<ConstvalNode *>(p.first);
  if (cst && (opcode == OP_sext || opcode == OP_zext)) {
    res = FoldSignExtend(opcode, n->primType, size, cst);
  } else {
    BaseNode *e = PairToExpr(n->Opnd(0)->primType, p);
    if (e != n->uOpnd) {
      res = module->CurFuncCodeMemPool()->New<ExtractbitsNode>(opcode, n->primType, offset, size, e);
    } else {
      res = n;
    }
  }
  return std::make_pair(res, 0);
}

std::pair<BaseNode *, int64> ConstantFold::FoldIread(IreadNode *n) {
  BaseNode *res = n;
  Opcode op = n->op;
  FieldID fieldID = n->fieldID;

  std::pair<BaseNode *, int64> p = DispatchFold(n->uOpnd);
  BaseNode *e = PairToExpr(n->uOpnd->primType, p);

  if (op == OP_iaddrof && e->op == OP_addrof) {
    AddrofNode *addrofNode = static_cast<AddrofNode *>(e);
    AddrofNode *newAddrof = addrofNode->MakeCopy(module);
    newAddrof->fieldID += fieldID;
    res = newAddrof;
  } else if (op == OP_iread && e->op == OP_addrof) {
    AddrofNode *addrofNode = static_cast<AddrofNode *>(e);
    MIRSymbol *msy = module->CurFunction()->GetLocalOrGlobalSymbol(addrofNode->stIdx);
    TyIdx typeId = msy->GetTyIdx();
    MIRType *msyType = GlobalTables::GetTypeTable().typeTable[typeId.GetIdx()];
    if (msyType->GetKind() == kTypeStruct || msyType->GetKind() == kTypeClass) {
      FieldID newFieldid = fieldID + addrofNode->fieldID;
      MIRStructType *stty = static_cast<MIRStructType *>(msyType);
      MIRType *fieldTy = stty->GetFieldType(newFieldid);
      res = module->CurFuncCodeMemPool()->New<AddrofNode>(OP_dread, fieldTy->GetPrimType(), addrofNode->stIdx, newFieldid);
      if (newFieldid == 0) {  // use original iread's primType
        res->primType = n->primType;
      }
    }
  } else if (e != n->uOpnd) {
    res = module->CurFuncCodeMemPool()->New<IreadNode>(op, n->primType, n->tyIdx, fieldID, static_cast<BaseNode *>(e));
  }
  return std::make_pair(res, 0);
}

std::pair<BaseNode *, int64> ConstantFold::FoldBinary(BinaryNode *n) {
  BaseNode *res = nullptr;
  int64 sum;
  Opcode op = n->op;
  PrimType primType = n->primType;
  PrimType lpPtyp = n->Opnd(0)->primType;
  PrimType rpPtyp = n->Opnd(1)->primType;

  std::pair<BaseNode *, int64> lp = DispatchFold(n->Opnd(0));
  std::pair<BaseNode *, int64> rp = DispatchFold(n->Opnd(1));

  BaseNode *l = lp.first;
  BaseNode *r = rp.first;

  ConstvalNode *lConst = dynamic_cast<ConstvalNode *>(l);
  ConstvalNode *rConst = dynamic_cast<ConstvalNode *>(r);

  bool isInt = IsPrimitiveInteger(primType);

  if (lConst && rConst) {
    // Don't fold div by 0, for floats div by 0 is well defined.
    if ((op == OP_div || op == OP_rem) && isInt &&
        (static_cast<MIRIntConst *>(rConst->constVal)->value == 0 ||
         static_cast<MIRIntConst *>(lConst->constVal)->value == LONG_MIN ||
         static_cast<MIRIntConst *>(lConst->constVal)->value == INT_MIN)) {
      res = NewBinaryNode(n, op, primType, lConst, rConst);
      sum = 0;
    } else {
      // 4 + 2 -> return a pair(res = ConstValNode(6), sum = 0)
      // Create a new ConstvalNode for 6 but keep the sum = 0. This simplify the
      // logic since the alternative is to return pair(res = nullptr, sum = 6).
      // Doing so would introduce many nullptr checks in the code. See previous
      // commits that implemented that logic for a comparison.
      res = FoldConstBinary(op, primType, lConst, rConst);
      sum = 0;
    }
  } else if (lConst && isInt) {
    MIRIntConst *mcst = static_cast<MIRIntConst *>(lConst->constVal);
    PrimType cstTyp = mcst->type->primType;
    int64 cst = mcst->value;

    if (op == OP_add) {
      sum = cst + rp.second;
      res = r;
    } else if (op == OP_sub) {
      sum = cst - rp.second;
      res = NegateTree(r);
    } else if ((op == OP_mul || op == OP_div || op == OP_rem || op == OP_ashr || op == OP_lshr || op == OP_shl ||
                op == OP_band || op == OP_cand || op == OP_land) &&
               cst == 0) {
      // 0 * X -> 0
      // 0 / X -> 0
      // 0 % X -> 0
      // 0 >> X -> 0
      // 0 << X -> 0
      // 0 & X -> 0
      // 0 && X -> 0
      sum = 0;
      res = module->mirBuilder->CreateIntConst(0, cstTyp);
    } else if (op == OP_mul && cst == 1) {
      // 1 * X --> X
      sum = 0;
      res = r;
    } else if (op == OP_bior && cst == -1) {
      // (-1) | X -> -1
      sum = 0;
      res = module->mirBuilder->CreateIntConst(-1, cstTyp);
    } else if (op == OP_lior || op == OP_cior) {
      sum = 0;
      if (cst != 0) {
        // 5 || X -> 1
        res = module->mirBuilder->CreateIntConst(1, cstTyp);
      } else {
        // when cst is zero
        // 0 || X -> (X != 0);
        res = module->CurFuncCodeMemPool()->New<CompareNode>(OP_ne, primType, r->primType, r,
                            module->mirBuilder->CreateIntConst(0, r->primType));
      }
    } else if ((op == OP_cand || op == OP_land) && cst != 0) {
      // 5 && X -> (X != 0)
      sum = 0;
      res = module->CurFuncCodeMemPool()->New<CompareNode>(OP_ne, primType, r->primType, r,
                          module->mirBuilder->CreateIntConst(0, r->primType));
    } else if ((op == OP_bior || op == OP_bxor) && cst == 0) {
      // 0 | X -> X
      // 0 ^ X -> X
      sum = 0;
      res = r;
    } else {
      res = NewBinaryNode(n, op, primType, l, PairToExpr(rpPtyp, rp));
      sum = 0;
    }
  } else if (rConst && isInt) {
    MIRIntConst *mcst = static_cast<MIRIntConst *>(rConst->constVal);
    PrimType cstTyp = mcst->type->primType;
    int64 cst = mcst->value;

    if (op == OP_add) {
      res = l;
      sum = lp.second + cst;
    } else if (op == OP_sub) {
      res = l;
      sum = lp.second - cst;
    } else if ((op == OP_mul || op == OP_band || op == OP_cand || op == OP_land) && cst == 0) {
      // X * 0 -> 0
      // X & 0 -> 0
      // X && 0 -> 0
      sum = 0;
      res = module->mirBuilder->CreateIntConst(0, cstTyp);
    } else if ((op == OP_mul || op == OP_div) && cst == 1) {
      // case [X * 1 -> X]
      // case [X / 1 = X]
      sum = 0;
      res = l;
    } else if (op == OP_band && cst == -1) {
      // X & (-1) -> X
      sum = 0;
      res = l;
    } else if (op == OP_bior && cst == -1) {
      // X | (-1) -> -1
      sum = 0;
      res = module->mirBuilder->CreateIntConst(-1, cstTyp);
    } else if ((op == OP_lior || op == OP_cior)) {
      sum = 0;
      if (cst > 0) {
        // X || 5 -> 1
        res = module->mirBuilder->CreateIntConst(1, cstTyp);
      } else if (cst == 0) {
        // X || 0 -> X
        res = l;
      } else {
        res = NewBinaryNode(n, op, primType, PairToExpr(lpPtyp, lp), r);
      }
    } else if ((op == OP_ashr || op == OP_lshr || op == OP_shl || op == OP_bior || op == OP_bxor) && cst == 0) {
      // X >> 0 -> X
      // X << 0 -> X
      // X | 0 -> X
      // X ^ 0 -> X
      sum = 0;
      res = l;
    } else if (op == OP_bxor && cst == 1 && primType != PTY_u1) {
      // bxor i32 (
      //   cvt i32 u1 (regread u1 %13),
      //  constVal i32 1),
      res = NewBinaryNode(n, op, primType, PairToExpr(lpPtyp, lp), PairToExpr(rpPtyp, rp));
      sum = 0;
      if (l->op == OP_cvt) {
        TypeCvtNode *cvtnode = static_cast<TypeCvtNode *>(l);
        if (cvtnode->uOpnd->primType == PTY_u1) {
          BaseNode *base = cvtnode->uOpnd;
          BaseNode *constVal = module->mirBuilder->CreateIntConst(1, base->primType);
          std::pair<BaseNode *, int64> p = DispatchFold(base);
          BinaryNode *temp = NewBinaryNode(n, op, PTY_u1, PairToExpr(base->primType, p), constVal);
          res = module->CurFuncCodeMemPool()->New<TypeCvtNode>(OP_cvt, primType, PTY_u1, temp);
        }
      }
    } else if (op == OP_rem && cst == 1) {
      // X % 1 -> 0
      sum = 0;
      res = module->mirBuilder->CreateIntConst(0, cstTyp);
    } else {
      res = NewBinaryNode(n, op, primType, PairToExpr(lpPtyp, lp), r);
      sum = 0;
    }
  } else if (isInt && (op == OP_add || op == OP_sub)) {
    if (op == OP_add) {
      res = NewBinaryNode(n, op, primType, l, r);
      sum = lp.second + rp.second;
    } else {
      res = NewBinaryNode(n, op, primType, l, r);
      sum = lp.second - rp.second;
    }
  } else {
    res = NewBinaryNode(n, op, primType, PairToExpr(lpPtyp, lp), PairToExpr(rpPtyp, rp));
    sum = 0;
  }
  return std::make_pair(res, sum);
}

BaseNode *ConstantFold::SimplifyDoubleCompare(CompareNode *n) {
  // For cases on gitlab issue 636.
  // See arm manual B.cond(P2993) and FCMP(P1091)
  BaseNode *res = n;
  BaseNode *l = n->Opnd(0);
  BaseNode *r = n->Opnd(1);

  if (n->op == OP_ne || n->op == OP_eq) {
    if ((l->op == OP_cmpl || l->op == OP_cmpg || l->op == OP_cmp) && r->op == OP_constval) {
      ConstvalNode *constNode = static_cast<ConstvalNode *>(r);
      if (constNode->constVal->kind == kConstInt && constNode->constVal->IsZero()) {
        CompareNode *compNode = static_cast<CompareNode *>(l);
        res = module->CurFuncCodeMemPool()->New<CompareNode>(n->op, n->primType, compNode->opndType, compNode->Opnd(0),
                                                      compNode->Opnd(1));
      }
    } else if ((r->op == OP_cmpl || r->op == OP_cmpg || r->op == OP_cmp) && l->op == OP_constval) {
      ConstvalNode *constNode = static_cast<ConstvalNode *>(l);
      if (constNode->constVal->kind == kConstInt && constNode->constVal->IsZero()) {
        CompareNode *compNode = static_cast<CompareNode *>(r);
        res = module->CurFuncCodeMemPool()->New<CompareNode>(n->op, n->primType, compNode->opndType, compNode->Opnd(0),
                                                      compNode->Opnd(1));
      }
    } else if (n->op == OP_ne && r->op == OP_constval) {
      // ne (u1 x, constVal 0)  <==> x
      ConstvalNode *constNode = static_cast<ConstvalNode *>(r);
      if (constNode->constVal->kind == kConstInt && constNode->constVal->IsZero()) {
        BaseNode *opnd = l;
        do {
          if (opnd->primType == PTY_u1) {
            res = opnd;
            break;
          } else if (opnd->op == OP_cvt) {
            TypeCvtNode *cvtnode = static_cast<TypeCvtNode *>(opnd);
            opnd = cvtnode->uOpnd;
          } else {
            opnd = nullptr;
          }
        } while (opnd);
      }
    }
  } else if (n->op == OP_gt || n->op == OP_lt) {
    if ((l->op == OP_cmpg || l->op == OP_cmp) && r->op == OP_constval) {
      ConstvalNode *constNode = static_cast<ConstvalNode *>(r);
      if (constNode->constVal->kind == kConstInt && constNode->constVal->IsZero()) {
        CompareNode *compNode = static_cast<CompareNode *>(l);
        res = module->CurFuncCodeMemPool()->New<CompareNode>(n->op, n->primType, compNode->opndType, compNode->Opnd(0),
                                                      compNode->Opnd(1));
      }
    } else if ((r->op == OP_cmpl || r->op == OP_cmp) && l->op == OP_constval) {
      ConstvalNode *constNode = static_cast<ConstvalNode *>(l);
      if (constNode->constVal->kind == kConstInt && constNode->constVal->IsZero()) {
        CompareNode *compNode = static_cast<CompareNode *>(r);
        res = module->CurFuncCodeMemPool()->New<CompareNode>(n->op, n->primType, compNode->opndType, compNode->Opnd(1),
                                                      compNode->Opnd(0));
      }
    }
  }

  return res;
}

std::pair<BaseNode *, int64> ConstantFold::FoldCompare(CompareNode *n) {
  BaseNode *res = nullptr;
  std::pair<BaseNode *, int64> lp = DispatchFold(n->Opnd(0));
  std::pair<BaseNode *, int64> rp = DispatchFold(n->Opnd(1));

  ConstvalNode *lConst = dynamic_cast<ConstvalNode *>(lp.first);
  ConstvalNode *rConst = dynamic_cast<ConstvalNode *>(rp.first);

  if (lConst && rConst && !IsPrimitiveDynType(n->opndType)) {
    res = FoldConstComparison(n->op, n->primType, n->opndType, lConst, rConst);
  } else {
    BaseNode *l = PairToExpr(n->Opnd(0)->primType, lp);
    BaseNode *r = PairToExpr(n->Opnd(1)->primType, rp);
    if (l != n->Opnd(0) || r != n->Opnd(1)) {
      res = module->CurFuncCodeMemPool()->New<CompareNode>(n->op, n->primType, n->opndType, l, r);
    } else {
      res = n;
    }
    res = SimplifyDoubleCompare(static_cast<CompareNode *>(res));
  }
  return std::make_pair(res, 0);
}

BaseNode *ConstantFold::Fold(BaseNode *x) {
  if (x == nullptr || kOpcodeInfo.IsStmt(x->op)) {
    return nullptr;
  }
  std::pair<BaseNode *, int64> p = DispatchFold(x);
  BaseNode *res = PairToExpr(x->primType, p);
  if (res == x) {
    res = nullptr;
  }
  return res;
}

std::pair<BaseNode *, int64> ConstantFold::FoldDepositbits(DepositbitsNode *n) {
  BaseNode *res = nullptr;
  uint8 bitsOffset = n->bitsOffset;
  uint8 bitsSize = n->bitsSize;

  std::pair<BaseNode *, int64> lp = DispatchFold(n->Opnd(0));
  std::pair<BaseNode *, int64> rp = DispatchFold(n->Opnd(1));

  ConstvalNode *lConst = dynamic_cast<ConstvalNode *>(lp.first);
  ConstvalNode *rConst = dynamic_cast<ConstvalNode *>(rp.first);

  if (lConst && rConst) {
    MIRIntConst *int0 = static_cast<MIRIntConst *>(lConst->constVal);
    MIRIntConst *int1 = static_cast<MIRIntConst *>(rConst->constVal);
    ConstvalNode *resultConst = module->CurFuncCodeMemPool()->New<ConstvalNode>();
    resultConst->primType = n->primType;
    MIRType *type = GlobalTables::GetTypeTable().GetPrimType(n->primType);
    MIRIntConst *constVal = module->memPool->New<MIRIntConst>(0, type);
    int64 op0ExtractVal = 0;
    int64 op1ExtractVal = 0;
    int64 mask0 = ((static_cast<int64>(0x1)) << (bitsSize + bitsOffset)) - 1;
    int64 mask1 = ((static_cast<int64>(0x1)) << bitsOffset) - 1;
    int64 op0Mask = ~(static_cast<int64>(mask0 ^ mask1));
    op0ExtractVal = (int0->value & op0Mask);
    op1ExtractVal = (int1->value << bitsOffset) & ((static_cast<int64>(0x1) << (bitsSize + bitsOffset)) - 1);
    constVal->value = (static_cast<uint64>(op0ExtractVal)) | (static_cast<uint64>(op1ExtractVal));
    resultConst->constVal = constVal;
    res = resultConst;
  } else {
    BaseNode *l = PairToExpr(n->Opnd(0)->primType, lp);
    BaseNode *r = PairToExpr(n->Opnd(1)->primType, rp);
    if (l != n->Opnd(0) || r != n->Opnd(1)) {
      res = module->CurFuncCodeMemPool()->New<DepositbitsNode>(n->op, n->primType, bitsOffset, bitsSize, l, r);
    } else {
      res = n;
    }
  }
  return std::make_pair(res, 0);
}

std::pair<BaseNode *, int64> ConstantFold::FoldArray(ArrayNode *n) {
  BaseNode *res = nullptr;

  unsigned i = 0;
  bool isFolded = false;

  ArrayNode *arrNode = module->CurFuncCodeMemPool()->New<ArrayNode>(module, n->primType, n->tyIdx, n->boundsCheck);
  for (i = 0; i < n->nOpnd.size(); i++) {
    std::pair<BaseNode *, int64> p = DispatchFold(n->nOpnd[i]);
    BaseNode *x = PairToExpr(n->nOpnd[i]->primType, p);
    if (x != n->nOpnd[i]) {
      isFolded = true;
    }
    arrNode->nOpnd.push_back(x);
    arrNode->numOpnds++;
  }
  if (isFolded) {
    res = arrNode;
  } else {
    res = n;
  }
  return std::make_pair(res, 0);
}

std::pair<BaseNode *, int64> ConstantFold::FoldTernary(TernaryNode *n) {
  BaseNode *res = n;

  PrimType p0Ptyp = n->Opnd(0)->primType;
  PrimType p1Ptyp = n->Opnd(1)->primType;
  PrimType p2Ptyp = n->Opnd(2)->primType;

  std::pair<BaseNode *, int64> p0 = DispatchFold(n->Opnd(0));
  std::pair<BaseNode *, int64> p1 = DispatchFold(n->Opnd(1));
  std::pair<BaseNode *, int64> p2 = DispatchFold(n->Opnd(2));

  if (n->op == OP_select) {
    ConstvalNode *c0 = dynamic_cast<ConstvalNode *>(p0.first);
    if (c0) {
      MIRIntConst *int0 = static_cast<MIRIntConst *>(c0->constVal);
      if (int0->value == 0) {
        res = PairToExpr(p2Ptyp, p2);
      } else {
        res = PairToExpr(p1Ptyp, p1);
      }
    } else if (n->Opnd(0)->op == OP_dread && p0Ptyp == PTY_u1) {
      ConstvalNode *c1 = dynamic_cast<ConstvalNode *>(p1.first);
      ConstvalNode *c2 = dynamic_cast<ConstvalNode *>(p2.first);
      if (c1 && c2) {
        MIRIntConst *int1 = static_cast<MIRIntConst *>(c1->constVal);
        MIRIntConst *int2 = static_cast<MIRIntConst *>(c2->constVal);
        if (int1->value == 1 && int2->value == 0) {
          BaseNode *node = n->Opnd(0);
          if (n->primType != PTY_u1)
            node = module->CurFuncCodeMemPool()->New<TypeCvtNode>(OP_cvt, n->primType, PTY_u1, n->Opnd(0));
          std::pair<BaseNode *, int64> p = DispatchFold(node);
          res = PairToExpr(n->primType, p);
        } else if (int1->value == 0 && int2->value == 1) {
          BaseNode *lnot = module->CurFuncCodeMemPool()->New<UnaryNode>(OP_lnot, PTY_u1, n->Opnd(0));
          BaseNode *node = lnot;
          if (n->primType != PTY_u1)
            node = module->CurFuncCodeMemPool()->New<TypeCvtNode>(OP_cvt, n->primType, PTY_u1, lnot);
          std::pair<BaseNode *, int64> p = DispatchFold(node);
          res = PairToExpr(n->primType, p);
        }
      }
    }
  } else {
    BaseNode *e0 = PairToExpr(p0Ptyp, p0);
    BaseNode *e1 = PairToExpr(p1Ptyp, p1);
    BaseNode *e2 = PairToExpr(p2Ptyp, p2);
    if (e0 != n->Opnd(0) || e1 != n->Opnd(1) || e2 != n->Opnd(2)) {
      res = module->CurFuncCodeMemPool()->New<TernaryNode>(n->op, n->primType, e0, e1, e2);
    }
  }
  return std::make_pair(res, 0);
}

StmtNode *ConstantFold::SimplifyDassign(DassignNode *n) {
  BaseNode *retval = nullptr;
  retval = Fold(n->GetRhs());
  if (retval) {
    n->SetRhs(retval);
  }
  return n;
}

StmtNode *ConstantFold::SimplifyIassign(IassignNode *n) {
  BaseNode *retval = nullptr;
  retval = Fold(n->addrExpr);
  if (retval) {
    n->addrExpr = retval;
  }
  retval = Fold(n->rhs);
  if (retval) {
    n->rhs = retval;
  }
  MIRPtrType *iassPtType = dynamic_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(n->tyIdx));
  if (iassPtType == nullptr) {
    return n;
  }

  switch (n->addrExpr->op) {
    case OP_addrof: {
      AddrofNode *addrofNode = static_cast<AddrofNode *>(n->addrExpr);
      MIRSymbol *lhsSym = module->CurFunction()->GetLocalOrGlobalSymbol(addrofNode->stIdx);
      TyIdx lhsTyIdx = lhsSym->GetTyIdx();
      if (addrofNode->fieldID != 0) {
        MIRStructType *lhsStructTy = dynamic_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(lhsTyIdx));
        if (lhsStructTy == nullptr) {
          break;
        }
        lhsTyIdx = lhsStructTy->GetFieldType(addrofNode->fieldID)->tyIdx;
      }
      if (iassPtType->pointedTyIdx == lhsTyIdx || n->fieldID == 0) {
        DassignNode *dassignNode = module->CurFuncCodeMemPool()->New<DassignNode>();
        dassignNode->stIdx = addrofNode->stIdx;
        dassignNode->SetRhs(n->rhs);
        dassignNode->fieldID = addrofNode->fieldID + n->fieldID;
        return dassignNode;
      }
      break;
    }
    case OP_iaddrof: {
      IreadNode *iaddrofNode = static_cast<IreadNode *>(n->addrExpr);
      MIRPtrType *iaddrofPtType = dynamic_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iaddrofNode->tyIdx));
      if (iaddrofPtType == nullptr) {
        break;
      }
      if (iaddrofNode->fieldID == 0) {
        // this iaddrof is redundant
        n->addrExpr = iaddrofNode->Opnd(0);
        break;
      }
      MIRStructType *lhsStructTy = static_cast<MIRStructType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iaddrofPtType->pointedTyIdx));
      TyIdx lhsTyIdx = lhsStructTy->GetFieldType(iaddrofNode->fieldID)->tyIdx;
      if (iassPtType->pointedTyIdx == lhsTyIdx) {
        // eliminate the iaddrof by updating the iassign's fieldID and tyIdx
        n->fieldID += iaddrofNode->fieldID;
        n->tyIdx = iaddrofNode->tyIdx;
        n->addrExpr = iaddrofNode->Opnd(0);
        // recursive call for the new iassign
        return SimplifyIassign(n);
      }
      break;
    }
    default:
      break;
  }
  return n;
}

StmtNode *ConstantFold::SimplifyCondGoto(CondGotoNode *n) {
  BaseNode *retval = nullptr;
  retval = Fold(n->uOpnd);
  if (retval) {
    n->uOpnd = retval;
    ConstvalNode *cst = dynamic_cast<ConstvalNode *>(n->uOpnd);
    if (!cst) {
      return n;
    }
    MIRIntConst *intcst = static_cast<MIRIntConst *>(cst->constVal);
    if ((OP_brtrue == n->op && intcst->GetValueUnderType() != 0) ||
        (OP_brfalse == n->op && intcst->GetValueUnderType() == 0)) {
      GotoNode *gotonode = module->CurFuncCodeMemPool()->New<GotoNode>(OP_goto);
      gotonode->offset = n->offset;
      return gotonode;
    } else {
      return nullptr;
    }
  } else if (n->uOpnd->op == OP_select) {
    return SimplifyCondGotoSelect(n);
  }

  return n;
}

StmtNode *ConstantFold::SimplifyCondGotoSelect(CondGotoNode *n) {
  TernaryNode *sel = static_cast<TernaryNode *>(n->uOpnd);
  if (!sel || sel->op != OP_select) {
    return n;
  }

  ConstvalNode *c1 = dynamic_cast<ConstvalNode *>(sel->Opnd(1));
  ConstvalNode *c2 = dynamic_cast<ConstvalNode *>(sel->Opnd(2));
  if (c1 && c2) {
    MIRIntConst *int1 = static_cast<MIRIntConst *>(c1->constVal);
    MIRIntConst *int2 = static_cast<MIRIntConst *>(c2->constVal);
    if (int1->value == 1 && int2->value == 0) {
      n->uOpnd = sel->Opnd(0);
    } else if (int1->value == 0 && int2->value == 1) {
      n->op = (n->op == OP_brfalse) ? OP_brtrue : OP_brfalse;
      n->uOpnd = sel->Opnd(0);
    }
  }
  return n;
}

StmtNode *ConstantFold::SimplifySwitch(SwitchNode *n) {
  BaseNode *retval = nullptr;
  retval = Fold(n->switchOpnd);
  if (retval) {
    n->switchOpnd = retval;
    ConstvalNode *cst = dynamic_cast<ConstvalNode *>(n->switchOpnd);
    if (!cst) {
      return n;
    }
    MIRIntConst *intcst = static_cast<MIRIntConst *>(cst->constVal);
    GotoNode *gotonode = module->CurFuncCodeMemPool()->New<GotoNode>(OP_goto);
    bool isdefault = true;
    for (unsigned i = 0; i < n->switchTable.size(); i++) {
      if (n->switchTable[i].first == intcst->value) {
        isdefault = false;
        gotonode->offset = (LabelIdx)n->switchTable[i].second;
        break;
      }
    }
    if (isdefault) {
      gotonode->offset = n->defaultLabel;
    }
    return gotonode;
  }
  return n;
}

StmtNode *ConstantFold::SimplifyUnary(UnaryStmtNode *n) {
  BaseNode *retval = nullptr;
  if (!n->uOpnd) {
    return n;
  }
  retval = Fold(n->uOpnd);
  if (retval) {
    n->uOpnd = retval;
  }
  return n;
}

StmtNode *ConstantFold::SimplifyBinary(BinaryStmtNode *n) {
  BaseNode *retval = nullptr;
  retval = Fold(n->bOpnd[0]);
  if (retval) {
    n->bOpnd[0] = retval;
  }
  retval = Fold(n->bOpnd[1]);
  if (retval) {
    n->bOpnd[1] = retval;
  }
  return n;
}

StmtNode *ConstantFold::SimplifyBlock(BlockNode *n) {
  if (!n->GetFirst()) {
    return n;
  }

  StmtNode *s = n->GetFirst();
  StmtNode *prevstmt = nullptr;
  do {
    StmtNode *retval = Simplify(s);
    if (retval != nullptr) {
      if (retval->op == OP_block) {
        BlockNode *blk = static_cast<BlockNode *>(retval);
        n->ReplaceStmtWithBlock(s, blk);
      } else {
        n->ReplaceStmt1WithStmt2(s, retval);
      }
      prevstmt = s;
      s = s->GetNext();
    } else {
      // delete s from block
      StmtNode *nextstmt = s->GetNext();
      if (s == n->GetFirst()) {
        n->SetFirst(nextstmt);
        if (nextstmt != nullptr) {
          nextstmt->SetPrev(nullptr);
        }
      } else {
        CHECK_FATAL(prevstmt != nullptr, "null ptr check");
        prevstmt->SetNext(nextstmt);
        if (nextstmt != nullptr) {
          nextstmt->SetPrev(prevstmt);
        }
      }
      if (s == n->GetLast()) {
        n->SetLast(prevstmt);
      }
      s = nextstmt;
    }
  } while (s);
  return n;
}

StmtNode *ConstantFold::SimplifyIf(IfStmtNode *n) {
  BaseNode *retval = nullptr;

  Simplify(n->thenPart);
  if (n->elsePart) {
    Simplify(n->elsePart);
  }
  retval = Fold(n->uOpnd);
  if (retval) {
    n->uOpnd = retval;
    ConstvalNode *cst = dynamic_cast<ConstvalNode *>(n->uOpnd);
    if (!cst) {
      return n;
    }
    MIRIntConst *intcst = static_cast<MIRIntConst *>(cst->constVal);
    if (0 == intcst->value) {
      return n->elsePart;
    } else {
      return n->thenPart;
    }
  }
  return n;
}

StmtNode *ConstantFold::SimplifyWhile(WhileStmtNode *n) {
  BaseNode *retval = nullptr;
  if (!n->uOpnd) {
    return n;
  }
  if (n->body) {
    Simplify(n->body);
  }
  retval = Fold(n->uOpnd);
  if (retval) {
    n->uOpnd = retval;
    ConstvalNode *cst = dynamic_cast<ConstvalNode *>(n->uOpnd);
    if (!cst) {
      return n;
    }
    if (cst->constVal->IsZero()) {
      if (OP_while == n->op) {
        return nullptr;
      } else {
        return n->body;
      }
    }
  }
  return n;
}

StmtNode *ConstantFold::SimplifyNary(NaryStmtNode *n) {
  BaseNode *retval = nullptr;
  for (int32 i = 0; i < n->NumOpnds(); i++) {
    retval = Fold(n->nOpnd[i]);
    if (retval) {
      n->nOpnd[i] = retval;
    }
  }
  return n;
}

StmtNode *ConstantFold::SimplifyIcall(IcallNode *n) {
  BaseNode *retval = nullptr;
  for (int32 i = 0; i < n->NumOpnds(); i++) {
    retval = Fold(n->nOpnd[i]);
    if (retval) {
      n->nOpnd[i] = retval;
    }
  }
  // icall node transform to call node
  switch (n->nOpnd[0]->op) {
    case OP_addroffunc: {
      AddroffuncNode *addrofNode = static_cast<AddroffuncNode *>(n->nOpnd.at(0));
      CallNode *callNode = nullptr;
      CallNode *callassNode = nullptr;
      if (n->op == OP_icall) {
        callNode = module->CurFuncCodeMemPool()->New<CallNode>(module, OP_call);
      } else {
        CHECK_FATAL(n->op == OP_icallassigned, "SimplifyIcall: unexpected opcode");
        callassNode = module->CurFuncCodeMemPool()->New<CallNode>(module, OP_callassigned);
        callassNode->returnValues = static_cast<IcallNode *>(n)->returnValues;
        callNode = callassNode;
      }
      callNode->puIdx = addrofNode->puIdx;
      for (uint32 i = 1; i < n->nOpnd.size(); i++) {
        callNode->nOpnd.push_back(n->nOpnd[i]);
      }
      callNode->numOpnds = callNode->nOpnd.size();
      return callNode;
    }
    default:
      break;
  }
  return n;
}

}  // namespace maple
