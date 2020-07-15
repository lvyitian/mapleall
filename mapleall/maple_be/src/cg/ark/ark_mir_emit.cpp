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

#include "mir_builder.h"
#include "ark_mir_emit.h"
#include "cg_option.h"
#include "cmpl_generator.h"
#include <climits>
#include "securec.h"
#include "special_func.h"

#include <algorithm>
#include <iostream>
#include <fstream>
#include <string>
#include <iomanip>

using std::hex;

// MapleRE opcode ascii names
std::vector <std::string> RE_OpName = {
  "",
#define STR(s) #s
#define OPCODE(O, P, Q, S) STR(O),
#include "mre_opcodes.def"
#undef OPCODE
};

// Flatten statements, expressions and sub-expressions to postfix order
void MirGenerator::FlattenExpr(BaseNode *fexpr) {
  for (int i = 0; i < fexpr->NumOpnds(); i++) {
    EmitExpr(fexpr->Opnd(i));
  }
}

// These were hardcoded in libmaplert.so mrt_primitive_class.def and need special handling
std::set<std::string> PreDefClassInfo = {
  string(CLASSINFO_PREFIX_STR) + string(JARRAY_PREFIX_STR) + string(NameMangler::kJavaLangCharSequence),
  string(CLASSINFO_PREFIX_STR) + string(JARRAY_PREFIX_STR) + string(NameMangler::kJavaLangClassStr),
  string(CLASSINFO_PREFIX_STR) + string(JARRAY_PREFIX_STR) + string(NameMangler::kJavaLangObjectStr),
  string(CLASSINFO_PREFIX_STR) + string(JARRAY_PREFIX_STR) + string(NameMangler::kJavaLangStringStr),
  string(CLASSINFO_PREFIX_STR) + string(JARRAY_PREFIX_STR) + string(NameMangler::kJavaLangThreadLocalMapEntry),
  string(CLASSINFO_PREFIX_STR) + string(JARRAY_PREFIX_STR) + string(NameMangler::kJavaUtilFormatterFlags),
  string(CLASSINFO_PREFIX_STR) + string(JARRAY_PREFIX_STR) + string(NameMangler::kJavaUtilFormatterFormatString),
  string(CLASSINFO_PREFIX_STR) + string(JARRAY_PREFIX_STR) + string(NameMangler::kJavaUtilHashMapNode),
  string(CLASSINFO_PREFIX_STR) + string(JARRAY_PREFIX_STR) + string(NameMangler::kJavaUtilHashTableHashTableEntry),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(B)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(C)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(D)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(F)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(I)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(J)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(S)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(Z)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AB)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AC)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AD)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AF)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AI)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AJ)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AS)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AZ)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AAAB)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AAAI)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AAB)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AAD)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AAI)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AAJ)),
  string(PRIMITIVECLASSINFO_PREFIX_STR) + string(TO_STR(AAS)),
};

// Maple runtime calls which will be lowered from OP_call to OP_intrinsiccall.
// TODO: Do lowering in mplcg belowering pass or earlier maplecomb phases.
// TODO: add memcmpMpl to list when we have x86 replacement.
// NOTE: IntrinsicList is moved to special_func.cpp

inline void MirGenerator::EmitString(const std::string &str, int bytes) {
  int offset = GetFuncOffset();
  os << "\t" << str << "\n";
  SetFuncOffset(offset + bytes);
}

inline void MirGenerator::EmitStringNoTab(const std::string &str, int bytes) {
  int offset = GetFuncOffset();
  os << str << "\n";
  SetFuncOffset(offset + bytes);
}

/* general routine to handle expression conversion
   Note: FlattenExpr should go first, to guarantee postorder
 */
void MirGenerator::EmitExpr(BaseNode *fexpr) {
  mre_instr_t expr(fexpr);
  switch (fexpr->op) {
    // leaf opcodes
    case OP_addroffunc: {
      MIRFunction *callFunc = GlobalTables::GetFunctionTable().funcTable.at((static_cast<AddroffuncNode *>(fexpr))->puIdx);
      FlattenExpr(fexpr);
      EmitAsmBaseNode(expr);
      EmitAsmFuncAddr(callFunc->GetName());
      break;
    }
    case OP_constval: {
      FlattenExpr(fexpr);
      if (expr.primType == PTY_ref) {
        // TODO: this should be done in belowering
        expr.primType = PTY_a64;
      }
      MIRConst *constval = (static_cast<ConstvalNode *>(fexpr))->constVal;
      switch(constval->kind) {
        case kConstInt: {
          int64 constInt = (static_cast<MIRIntConst *>(constval))->value;
          if (expr.Is16BitInt(constInt)) {
            expr.param.constval.i16 = constInt;
            EmitAsmBaseNode(expr);
          } else if (expr.Is16BitUint(constInt)) {
            expr.param.constval.u16 = constInt;
            EmitAsmBaseNode(expr);
          } else {
            expr.op = RE_constval64;
            EmitAsmBaseNode(expr);
            EmitAsmLong(constInt);
          }
          break;
        }
        case kConstDoubleConst: {
          expr.op = RE_constval64;
          EmitAsmBaseNode(expr);
          EmitAsmLong((static_cast<MIRDoubleConst *>(constval))->value.intValue);
          break;
        }
        case kConstFloatConst:
          expr.op = RE_constval64;
          EmitAsmBaseNode(expr);
          EmitAsmLong(static_cast<MIRFloatConst *>(constval)->value.intValue);
          break;
        default:
          ASSERT(0, "OP_constval currently only support int and double types - type %u unsupported", constval->kind  );
          break;
      }
      break;
    }
    // Unary opcodes
    case OP_extractbits:
    case OP_zext:
    case OP_sext:
      // numOpnds is implicitly 1. original numOpnds field for these instr now occupied by bize.
      FlattenExpr(fexpr);
      expr.param.extractbits.boffset = (static_cast<ExtractbitsNode *>(fexpr))->bitsOffset;  // not used by zext and sext
      expr.param.extractbits.bsize = (static_cast<ExtractbitsNode *>(fexpr))->bitsSize;
      EmitAsmBaseNode(expr);
      break;
    case OP_abs:
    case OP_bnot:
    case OP_lnot:
    case OP_neg:
    case OP_recip:
    case OP_sqrt:
      FlattenExpr(fexpr);
      // no additional field to handle
      EmitAsmBaseNode(expr);
      break;
    // Type conversion
    case OP_ceil:
    case OP_floor:
    case OP_retype:
    case OP_round:
    case OP_trunc:
      FlattenExpr(fexpr);
      expr.param.type.opPtyp = (static_cast<TypeCvtNode *>(fexpr))->fromPrimType;
      EmitAsmBaseNode(expr);
      break;
    case OP_cvt: {
      TypeCvtNode *cvtNode = static_cast<TypeCvtNode *>(fexpr);
      FlattenExpr(fexpr);
      expr.param.type.opPtyp = cvtNode->fromPrimType;
      if ((cvtNode->uOpnd->op == OP_zext || cvtNode->uOpnd->op == OP_sext) &&
          cvtNode->uOpnd->primType != cvtNode->fromPrimType) {
        // workaround for sext/zext ptyp problem
        // insert cvt to convert from sext/zext ptyp to expected ptyp
        mre_instr_t cvt(RE_cvt, cvtNode->fromPrimType, 1);
        cvt.param.type.opPtyp = cvtNode->uOpnd->primType;
        EmitAsmBaseNode(cvt);
      }
      EmitAsmBaseNode(expr);
      break;
    }
    // Binary
    case OP_add:
    case OP_ashr:
    case OP_band:
    case OP_bior:
    case OP_bxor:
    case OP_depositbits:
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
    case OP_CG_array_elem_add:
      FlattenExpr(fexpr);
      EmitAsmBaseNode(expr);
      // no extra fields to handle
      break;
    // comparision operation now separated to add the operand type in
    case OP_cmp:
    case OP_cmpl:
    case OP_cmpg:
    case OP_eq:
    case OP_ge:
    case OP_gt:
    case OP_le:
    case OP_lt:
    case OP_ne: {
      // TODO: handle dynamic types
      FlattenExpr(fexpr);
      // if one of its operands' type is dynamic types, the opndtype
      // will be the first dynamic types.
      // otherwise, it's the type of the operands, and the operands
      // should have the same type.
      PrimType rpty0 = fexpr->Opnd(0)->primType;
      PrimType rpty1 = fexpr->Opnd(1)->primType;
      // ASSERT(rpty0 == rpty1, "Compare operands not same ptype");
      expr.param.type.opPtyp = (static_cast<CompareNode *>(fexpr))->opndType; // use opnd type instead?
      EmitAsmBaseNode(expr);
      break;
    }
    case OP_regread: {
      string regName;
      FlattenExpr(fexpr);
      MIRPreg *preg = curFunc.func->pregTab->PregFromPregIdx(((RegreadNode *)fexpr)->regIdx);
      ASSERT(preg, "preg is null");
      regName.append("%");
      regName.append(std::to_string(preg->pregNo));
      expr.param.frameIdx = curFunc.EncodePreg(preg->pregNo);
      EmitAsmBaseNode(expr, regName);
      break;
    }
    case OP_ireadoff: {
      int32 offset = static_cast<IreadoffNode *>(fexpr)->offset;
      FlattenExpr(fexpr);
      // generate 4 byte instr if offset fits in 16 bits else generate 8 byte instr
      if (offset <= 32767 && offset >= -32768) {
        expr.param.offset = offset;
        EmitAsmBaseNode(expr);
      } else {
        expr.op = RE_ireadoff32;
        EmitAsmBaseNode(expr);
        EmitAsmWord(offset);
      }
      break;
    }
    case OP_select:
      FlattenExpr(fexpr);
      // no extra fields to handle
      EmitAsmBaseNode(expr);
      break;
    case OP_iread: {
      // lower iread to ireadoff for interpreter
      int32 offset = GetFieldOffset(((IreadNode *)fexpr)->tyIdx, ((IreadNode *)fexpr)->fieldID);
      // Evaluate operands
      EmitExpr(static_cast<IreadNode *>(fexpr)->uOpnd);
      mre_instr_t ireadoff(RE_ireadoff, fexpr->primType, fexpr->numOpnds);
      // generate 4 byte instr if offset fits in 16 bits else generate 8 byte instr
      if (offset <= 32767 && offset >= -32768) {
        ireadoff.param.offset = offset;
        EmitAsmBaseNode(ireadoff);
      } else {
        ireadoff.op = RE_ireadoff32;
        EmitAsmBaseNode(ireadoff);
        EmitAsmWord(offset);
      }
      break;
    }
    case OP_addrof: {
      // TODO: calculate and add field-id offset to addr
      ASSERT(((AddrofNode *)fexpr)->fieldID == 0, "OP_addrof field is non zero");
      MIRSymbol *st = GetCurFunction()->GetLocalOrGlobalSymbol(((AddrofNode *)fexpr)->stIdx);

      if (GetCurStmt()->op == OP_intrinsiccall &&
        (static_cast<IntrinsiccallNode *>(GetCurStmt()))->intrinsic == INTRN_MPL_CLINIT_CHECK) {
        // CLINIT_CHECK need to use _PTR_xxx for class objects directly so special case here
        string prefix = "_PTR";
        EmitAsmAddroffPC((AddrofNode *)fexpr, prefix+st->GetName());
        break;
      }
      if (st->storageClass == kScExtern) {
        if (PreDefClassInfo.find(st->GetName()) != PreDefClassInfo.end()) {
          // libmaplert.so predefines a set of class info objects for arrays.
          // These are external symbols that have no corresponding _PTRxxx label in
          // Maple mpl files where they're referenced from, so they have to be special cased.
          FlattenExpr(fexpr);
          expr.op = RE_addrof32;
          expr.primType = PTY_a64;
          EmitAsmBaseNode(expr);
          EmitString(".4byte "+st->GetName()+"@GOTPCREL", 4);
        } else {
          // If var xxx is extern, CG generates a label _PTRxxx for the .quad location
          // in the current module where the linker will fill in with the address of xxx.
          // - _PTR_xxx-. is the PC rel addr to the location that holds the addr of xxx
          // - ireadpcoff _PTR_xxx-. returns the address of xxx.
          // So "addrof <var>" is lowered in this case to "ireadpcoff PTR_<var>-."
          EmitAsmIreadPCoff((AddrofNode *)fexpr, st->GetName());
        }
      } else {
        if (st->storageClass == kScFormal || st->storageClass == kScAuto) {
          // if var is func's auto or formal, we return an index to func's corresponding
          // storage frame slot and indicate it with a primtype of PTY_i32.
          // TODO: should mark this as compact op, although this usage did not appear in libcore-all
          int32_t frameIdx = curFunc.EncodeSym(st);
          FlattenExpr(fexpr);
          expr.primType = PTY_a64;
          expr.param.frameIdx = frameIdx;
          EmitAsmBaseNode(expr);
        }  else {
          // if here, var should be global and defined within module so generate a pc relative offset to it
          // however, it was seen that st->storageClass for the class object of some Java Primary types (e.g.
          // __pinf_C, __pinf_I, __pinf_J) were not set to k_ScExtern even though they exist in a different
          // though exists in a different so (libmaplert.so), so we have use GOT entry address to avoid overflow.
          if (PreDefClassInfo.find(st->GetName()) != PreDefClassInfo.end()) {
            // libmaplert.so predefines a set of class info objects for arrays.
            // These are external symbols that have no corresponding _PTRxxx label in
            // Maple mpl files where they're referenced from, so they have to be special cased.
            FlattenExpr(fexpr);
            expr.op = RE_addrof32;
            expr.primType = PTY_a64;
            EmitAsmBaseNode(expr);
            EmitString(".4byte "+st->GetName()+"@GOTPCREL", 4);
          } else {
            EmitAsmAddroffPC((AddrofNode *)fexpr, st->GetName());
          }
        }
      }
      break;
    }
    case OP_dread: {
      DreadNode *dread = (DreadNode *)fexpr;
      ASSERT(dread->fieldID == 0, "OP_dread field non-zero");
      MIRSymbol *s = GetCurFunction()->GetLocalOrGlobalSymbol(dread->stIdx);
      if (s->storageClass == kScAuto || s->storageClass == kScFormal) {
        // map stIdx to func storage frame idx
        FlattenExpr(fexpr);
        expr.param.frameIdx = curFunc.EncodeSym(s);
        EmitAsmBaseNode(expr, s->GetName());
      } else {
        // lower to OP_iread and insert an OP_addrof instr as its <addr-expr>
        AddrofNode addrofNode(OP_addrof, PTY_a64, dread->stIdx, dread->fieldID);
        if (s->storageClass == kScExtern) {
          EmitAsmIreadPCoff(&addrofNode, s->GetName());
        } else {
          EmitAsmAddroffPC(&addrofNode, s->GetName());
        }
        mre_instr_t iread(RE_iread, dread->primType, 1);
        EmitAsmBaseNode(iread);
      }
      break;
    }
    case OP_conststr:
      FlattenExpr(fexpr);
      EmitAsmBaseNode(expr);
      EmitAsmConststr(reinterpret_cast<ConststrNode *>(fexpr)->strIdx);
      break;
    default:
      MIR_FATAL("unknown expression opcode: [%d]:(%s)\n", fexpr->op, kOpcodeInfo.GetName(fexpr->op));
  }
}


/* general routine to handle statement conversion
   Note: FlattenExpr should go first, to guarantee postorder.
 */
void MirGenerator::EmitStmt(StmtNode *fstmt) {
  // in postorder, stmt now starts with its expressions.
  mre_instr_t stmt(fstmt);
  /* pending labels is used to tracking all label statement (immediately)
     before a non-label statement
   */
  static list<LabelIdx> pendingLabels;  // there're pending lable to fix
  SetCurStmt(fstmt);
  switch (fstmt->op) {
    case OP_intrinsiccall:
      FlattenExpr(fstmt);
      stmt.param.intrinsic.intrinsicId = static_cast<uint8>(static_cast<IntrinsiccallNode *>(fstmt)->intrinsic);
      EmitAsmBaseNode(stmt, GetIntrinsicName(static_cast<IntrinsiccallNode *>(fstmt)->intrinsic));
      break;
    case OP_call:
      EmitAsmCall(static_cast<CallNode *>(fstmt));
      break;
    case OP_icall: {
      FlattenExpr(fstmt);
#if 0 // TODO: Check why ret_tyidx is always 0
      MIRType *type = globaltable.GetTypeFromTyIdx(((IcallNode *)fstmt)->ret_tyidx);
      stmt->ptyp = type->GetPrimType();
#else
      // Workaround to init icall ptyp with type of return value in %%retval0
      stmt.primType = PTY_void;
      if (fstmt->GetNext() &&
          (fstmt->GetNext()->op == OP_regassign || fstmt->GetNext()->op == OP_dassign) &&
          ((UnaryStmtNode *)fstmt->GetNext())->uOpnd->op == OP_regread &&
          ((RegreadNode *)(((UnaryStmtNode *)fstmt->GetNext())->uOpnd))->regIdx == -kSregRetval0) {
        stmt.primType = ((UnaryStmtNode *)fstmt->GetNext())->uOpnd->primType;
      }
#endif
      // TODO: check if ret_tyidx exists in IcallNode, its in mirnodes.h but not in MapleIR spec.
      // if ret_tyidx needs to be added to icall_stmt_t, enable fld copy in OP_icall and OP_icallassigned
      // and change icall and icallassigned node size in mmpgenl.cpp:GetOpNodeSize() from 4 to 8.
      //(static_cast<icallassigned_stmt_t *>(stmt))->ret_tyidx = ((IcallAssignedNode *)fstmt)->ret_tyidx;
      // no other field to handle. puptrexp is not used any more in VM
      EmitAsmBaseNode(stmt);
      break;
    }
    case OP_iassignoff: {
      int32 offset = static_cast<IassignoffNode *>(fstmt)->offset;
      FlattenExpr(fstmt);
      // generate 4 byte instr if offset fits in 16 bits else generate 8 byte instr
      if (offset <= 32767 && offset >= -32768) {
        stmt.param.offset = offset;
        EmitAsmBaseNode(stmt);
      } else {
        stmt.op = RE_iassignoff32;
        EmitAsmBaseNode(stmt);
        EmitAsmWord(offset);
      }
      break;
    }
    case OP_regassign: {
      string regName;
      FlattenExpr(fstmt);
      MIRPreg *preg = curFunc.func->pregTab->PregFromPregIdx(static_cast<RegassignNode *>(fstmt)->regIdx);
      ASSERT(preg, "preg is null");
      regName.append("%");
      regName.append(std::to_string(preg->pregNo));
      stmt.param.frameIdx = curFunc.EncodePreg(preg->pregNo);
      EmitAsmBaseNode(stmt, regName);
      break;
    }
    case OP_brtrue:
    case OP_brfalse:
      FlattenExpr(fstmt);
      if (stmt.bn()->op == OP_brtrue) {
        stmt.op = RE_brtrue32;
      } else {
        stmt.op = RE_brfalse32;
      }
      EmitAsmBaseNode(stmt);
      EmitAsmLabel(((CondGotoNode *)fstmt)->offset, true);
      break;
    case OP_return: {
      // remove the redundant return 0 statement at the end of a function
      FlattenExpr(fstmt);
      // no extra field to handle
      // set the return's primitive type to its first operand if there is one
      if (fstmt->NumOpnds() > 0) {
        stmt.primType = fstmt->Opnd(0)->primType;
      }
      EmitAsmBaseNode(stmt);
      break;
    }
    case OP_goto:
      FlattenExpr(fstmt);
      stmt.op = RE_goto32;
      EmitAsmBaseNode(stmt);
      EmitAsmLabel(((GotoNode *)fstmt)->offset, true);
      break;
    case OP_rangegoto: {
      RangegotoNode *rNode = static_cast<RangegotoNode *>(fstmt);
      SmallCaseVector &rTable = rNode->rangegotoTable;
      int numCases = rTable.size();
      ASSERT((numCases >= 0) && (numCases < 256), "");
      stmt.param.numCases = numCases;
      FlattenExpr(fstmt);
      EmitAsmBaseNode(stmt); // emit base instr with num jump table entry
      EmitAsmWord(rNode->tagOffset); // emit tagOffset
      for (int i = 0; i < numCases; i++) { // emit jump table
        MIRFunction *func = GetCurFunction();
        string label = "mirbin_label_"+to_string(func->puIdxOrigin)+"_"+to_string(rTable[i].second);
        EmitString(".2byte "+label+"-. "+"\t// jmptbl["+to_string(rTable[i].first)+"]", 2);
      }
      break;
    }
    case OP_throw:
    case OP_free:
    case OP_syncenter:
    case OP_syncexit:
      FlattenExpr(fstmt);
      EmitAsmBaseNode(stmt);
      break;
    case OP_label:
      EmitAsmLabel((static_cast<LabelNode *>(fstmt))->labelIdx, false);
      break;
    case OP_comment:
      EmitAsmComment(static_cast<CommentNode *>(fstmt));
      break;
    case OP_membarrelease:
    case OP_membarstorestore:
    case OP_membarstoreload:
    case OP_membaracquire:
      EmitAsmBaseNode(stmt);
      break;
    case OP_assertnonnull:
      FlattenExpr(fstmt);
      EmitAsmBaseNode(stmt);
      break;
    case OP_dassign: {
      // if assign target is formal/local var, use compact OP_dassign (with frameIdx)
      // else assign target is global, lower to OP_ireadpcoff/OP_addroffpc + compact OP_iassignoff (fieldID always 0)
      // note: ptyp is not set in the dassign IR in the input VtableImpl.mpl so have to be set here for ARK engine
      DassignNode *dassign = (DassignNode *)fstmt;
      ASSERT(dassign->fieldID == 0, "OP_dassign field != 0");
      MIRSymbol *s = GetCurFunction()->GetLocalOrGlobalSymbol(dassign->stIdx);
      if (s->storageClass == kScAuto || s->storageClass == kScFormal) {
        FlattenExpr(fstmt); // val <rhs-expr>
        if ((dassign->uOpnd->op == OP_zext || dassign->uOpnd->op == OP_sext) &&
            dassign->uOpnd->primType != dassign->primType) {
          // workaround for sext/zext ptyp problem
          // insert cvt to convert from zext ptyp to dassign assignee ptyp
          mre_instr_t cvt(RE_cvt, s->GetType()->GetPrimType(), 1);
          cvt.param.type.opPtyp = dassign->uOpnd->primType;
          EmitAsmBaseNode(cvt);
        }
        stmt.primType = s->GetType()->GetPrimType();
        stmt.param.frameIdx = curFunc.EncodeSym(s);
        EmitAsmBaseNode(stmt, s->GetName());
      } else {
        // lower to OP_iassignoff, and insert an OP_ireadpcoff/OP_addroffpc instr for its <addrexpr>
        AddrofNode addrofNode(OP_addrof, dassign->primType, dassign->stIdx, dassign->fieldID);
        if (s->storageClass == kScExtern) {
          EmitAsmIreadPCoff(&addrofNode, s->GetName());
        } else {
          EmitAsmAddroffPC(&addrofNode, s->GetName());
        }
        EmitExpr(dassign->uOpnd); // eval <rhs-expr> before generating iassign node
        mre_instr_t iassignoff(RE_iassignoff, s->GetType()->GetPrimType(), 2);
        iassignoff.param.offset = 0; // offset always 0 because fieldID is always 0
        EmitAsmBaseNode(iassignoff);
      }
      break;
    }
    case OP_iassign: {
      // Lower iassign to iassignoff (can be either compact or regular instr) for interpreter
      IassignNode *iassign = (IassignNode *)fstmt;
      int32 offset = GetFieldOffset(iassign->tyIdx, iassign->fieldID);
      // eval <addrexp> and <rhs>
      EmitExpr(iassign->addrExpr);
      EmitExpr(iassign->rhs);
      mre_instr_t iassignoff(RE_iassignoff, iassign->rhs->primType, fstmt->numOpnds);
      // generate 4 byte instr if offset fits in 16 bits else generate 8 byte instr
      if (offset <= 32767 && offset >= -32768) {
        iassignoff.param.offset = offset;
        EmitAsmBaseNode(iassignoff);
      } else {
        iassignoff.op = RE_iassignoff32;
        EmitAsmBaseNode(iassignoff);
        EmitAsmWord(offset);
      }
      break;
    }
    case OP_catch:
    case OP_javacatch: {
      CatchNode *c = static_cast<CatchNode *>(fstmt);
      uint32 numExTypes = c->exceptionTyIdxVec.size();
      stmt.param.numCases = numExTypes;

      stmt.op = MapEHOpcode(stmt.op);
      EmitAsmBaseNode(stmt);
      for (int i=0; i < numExTypes; i++) {
        MIRPtrType *ptype = (MIRPtrType *)(GlobalTables::GetTypeTable().GetTypeFromTyIdx(c->exceptionTyIdxVec[i]));
        MIRType *type = ptype->GetPointedType();
        if (type->typeKind == kTypeScalar && type->primType == PTY_void) {
          EmitAsmWord(0);  // receiving opaque pointer- javacatch { <* void> }
        } else {
          EmitString(".long _PTR__cinf_"+type->GetName()+"-.", 4);
        }
      }
      break;
    }
    case OP_cpptry:
    case OP_try:
    case OP_javatry: {
      TryNode *t = static_cast<TryNode *>(fstmt);
      uint32 numCatches = t->offsets.size();
      stmt.param.numCases = numCatches;
      stmt.op = MapEHOpcode(stmt.op);
      EmitAsmBaseNode(stmt);
      for (int i=0; i < numCatches; i++) {
        EmitAsmLabel(t->offsets[i], true);
      }
      // for matching nested try/catch blocks
      javatry_stmts->push(t);
      break;
    }
    case OP_endtry:
      ASSERT(javatry_stmts->top() != nullptr, "");
      javatry_stmts->pop();
      EmitAsmBaseNode(stmt);
      break;
    default:
      MIR_FATAL("unknown statement opcode: [%d]:(%s)\n", fstmt->op, kOpcodeInfo.GetName(fstmt->op));
  }
}

// Generate function instructions
void MirGenerator::EmitFuncDef(BlockNode *fblock) {
  StmtNode *fstmt = fblock->GetFirst();
  StmtNode *firstStmt = nullptr;
  while (fstmt) {
    EmitStmt(fstmt);
    fstmt = fstmt->GetNext();
  }
  // WARNING: workaround for mpl generation problem
  // maple compiler is not generating a function end return IR for
  // void functions that ends with a JNI stub.
  // Temporary workaround here to insert one.
  fstmt = fblock->GetLast();
  if (fstmt->op != OP_return) {
    mre_instr_t retStmt(RE_return, PTY_void, 0);
    EmitAsmBaseNode(retStmt);
  }
}

// Generate Maple function into maplere image.
void MirGenerator::EmitFunc(MIRFunction *ffunc) {
  SetFuncOffset(0);
  EmitAsmFuncInfo(ffunc);
  ASSERT((javatry_stmts == nullptr), "");
  javatry_stmts = new stack<TryNode *>();
  EmitFuncDef(ffunc->body);
  delete javatry_stmts;
  javatry_stmts = nullptr;
}

// Output to file the CG lowered IR used to generate maplre image.
// The output file name is the vtable input file name plus .mir.mpl suffix.
void MirGenerator::OutputMIR(bool genMpl=false) {
  mir_module_t *compactMir = nullptr; // unused
  std::string filename = mmodule.fileName + ".mir";
  if (genMpl) {
    std::ofstream dumpfile;
    dumpfile.open(filename+".mpl", std::ios::trunc);
    std::streambuf *backup = std::cout.rdbuf();
    std::cout.rdbuf(dumpfile.rdbuf());
    for (MIRFunction *mirfunc : mmodule.functionList) {
      if (mirfunc->body == nullptr) {
        continue;
      }
      mirfunc->Dump();
    }
    std::cout.rdbuf(backup);
    cout << "          " << filename+".mpl" << std::endl;
  }
}

// Formal args can be either pregs or vars.
// Get count, pregNo to formals index mapping, and type
int MirGenerator::GetFormalsInfo(MIRFunction *func) {
  int argSize = func->formalDefVec.size();
  for (int i=0; i < argSize; i++) {
    MIRSymbol *arg = func->formalDefVec[i].formalSym;

    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->formalDefVec[i].formalTyIdx);
    if (arg) {
      if (arg->sKind == kStPreg) {
        // the formal arg is a preg
        curFunc.AddFormalPreg(arg->value.preg->pregNo);
      } else {
        // the formal arg is a var
        curFunc.AddFormalVar(arg);
      }
    }
  }
  return func->formalDefVec.size();
}

// A function's local storage includes:
// - auto variables in function's symtab
// - pregs in function's pregTab
// - special pregs (eg. %%retval0, 1, ..) in function's _spec_preg_table
// pregTab includes pregs
// - declared in local decl
// - declared in formal ars
// - used but not declared
int MirGenerator::GetLocalsInfo(MIRFunction *func) {
  MapleVector<MIRPreg *> pregtable = func->pregTab->pregTable;
  MIRSymbolTable *st = func->symTab;
  int localsCount = 0;

  // Walk func symtab for auto vars
  for (int32 i = 0; i < st->GetSymbolTableSize(); i++) {
    MIRSymbol *s = st->GetSymbolFromStIdx(i);
    if (!s) {
      continue;
    }
    if (s->IsDeleted()) {
      continue;
    }

    // TODO: check if var is already in formals

    if (s->storageClass != kScAuto) {
      continue;
    }
    ++localsCount;
#if 0
    printf("locals stIdx %d %s ", i, s->GetName().c_str());
    if (s->GetTyIdx().idx != 0) {
      printf(" ptype 0x%x size 0x%x\n", globaltable.type_table_[s->GetTyIdx().idx]->_primtype,
        GetPrimTypeSize(globaltable.type_table_[s->GetTyIdx().idx]->_primtype));
    }
#endif
    curFunc.AddLocalVar(s);
  }

  // Walk func's pregtable which has both pregs declared in formal args, pregs
  // declared in func's local decl section and pregs used in func but not declared.
  // Exclude the ones already accounted for in formals, and include undeclared ones.
  for (int32 i = 1; i < pregtable.size(); i++) {
    MIRPreg *preg = pregtable[i];
    if (curFunc.FindFormalPreg(preg->pregNo)) {
      continue;
    }
    curFunc.AddLocalPreg(preg->pregNo);
    ++localsCount;
#if 0
    printf("locals %d %%%d ", i, preg->pregNo);
    printf("type 0x%x size 0x%x\n", preg->ptyp, GetPrimTypeSize(preg->ptyp));
#endif
  }
  return localsCount;
}

int VisitNode(BaseNode *node) {
  int maxStack = 1; // at least 1 for pushing expr res or leaf val

  for (int i = 0; i < node->NumOpnds(); i++) {
    int stackSize =  VisitNode(node->Opnd(i)) + i;
    if (stackSize > maxStack) {
      maxStack = stackSize;
    }
  }
  if (node->op == OP_dread || node->op == OP_call || node->op == OP_dassign) {
    // Any MIRNode that triggers additional lowering during mre image generation
    // here (i.e. the lowering wasn't done prior to EmitStmt) needs to
    // add the number of additional expressions evluated to the maxStack
    // returned (which is used by stack machines doing expression evaluation).
    maxStack += 1;
  }
  return maxStack;
}

int MirGenerator::MaxEvalStack(MIRFunction *func) {
  int maxEvalStackSize = 0;
  ASSERT(func->body, "Function has no body");
  StmtNode *fstmt = func->body->GetFirst();
  while (fstmt) {
    int evalStackSize = 0;
    if (fstmt->NumOpnds()) {
      evalStackSize = VisitNode(fstmt);
    } else if (fstmt->op == OP_call) {
      // Case where fstmt is a call IR with no parameters.
      // This case needs 1 stack frame slot for the call addr pushed on eval
      // stack when the call is lowered to icall during maplere image generation.
      // TODO: don't need to do this if do the lowering in separate maplere
      // lowering phase before emitting maplere image.
      evalStackSize = 1;
    }
    if (evalStackSize > maxEvalStackSize) {
      maxEvalStackSize = evalStackSize;
    }
    fstmt = fstmt->GetNext();
  }
  return maxEvalStackSize;
}

void MirGenerator::EmitAsmAutoVarsInfo(MIRFunction *func) {
  MapleVector<MIRPreg *> pregtable = func->pregTab->pregTable;
  MIRSymbolTable *st = func->symTab;

  // %%retval0 and %%thrownval
  os << "\t.byte " << hex << "0x" << GlobalTables::GetTypeTable().typeTable.at(func->funcType->retTyIdx.GetIdx())->GetPrimType() << "\t// %%retval\n";
  os << "\t.byte " << hex << "0x" << PTY_unknown << "\t// %%thrownval\n";
  // local sregs in func
  for (int32 i = 1; i < pregtable.size(); i++) {
    MIRPreg *preg = pregtable[i];
    if (!curFunc.FindFormalPreg(preg->pregNo)) {
      os << "\t.byte " << hex << "0x" << preg->primType
         << dec << "\t// %" << preg->pregNo << "\n";
    }
  }
  // local vars in func
  for (int32 i = 0; i < st->GetSymbolTableSize(); i++) {
    MIRSymbol *s = st->GetSymbolFromStIdx(i);
    if (!s) {
      continue;
    }
    if (s->IsDeleted()) {
      continue;
    }
    if (s->storageClass != kScAuto) {
      continue;
    }
    if (s->GetTyIdx().GetIdx() != 0) {
      os << "\t" << ".byte " << hex
         << "0x" << GlobalTables::GetTypeTable().typeTable[s->GetTyIdx().GetIdx()]->primType
         << "\t// " << s->GetName() << "\n";
    }
  }
}

void MirGenerator::EmitAsmFormalArgInfo(MIRFunction *func) {
  for (int i = 0; i< func->formalDefVec.size(); i++) {
    MIRSymbol *arg = func->formalDefVec[i].formalSym;
    MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->formalDefVec[i].formalTyIdx);
    os << hex << "\t.byte " << "0x" << ty->primType << "\t// ";
    if (arg->sKind == kStPreg) {
      os << dec << "%" << arg->value.preg->pregNo << "\n";
    }
    else {
      os << arg->GetName() << "\n";
    }
  }
}

void MirGenerator::EmitAsmFuncInfo(MIRFunction *func) {
  MIRSymbol *fnSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  std::string funcName = fnSt->GetName();
  std::string codeLabel = funcName + "_mirbin_code";
  std::string infoLabel = funcName + "_mirbin_info";

  //printf("func %d \n", func->puIdxOrigin);
  curFunc.Init(func);
  curFunc.numFormalArgs = GetFormalsInfo(func);
  curFunc.numAutoVars = GetLocalsInfo(func) + 2; // incl. %%retval0 and %%thrownval
  curFunc.evalStackDepth = MaxEvalStack(func);
  if (func->IsWeak()) {
    curFunc.SetWeakAttr();
  }
  if (GlobalTables::GetStrTable().GetStringFromStrIdx(func->GetBaseFuncNameStridx()) == "finalize") {
    curFunc.SetFinalizeAttr();
  }
  // insert interpreter shim and signature
#if 0
    os << "\t" << "movq  " << infoLabel <<"(%rip), %r11\n";
    os << "\t" << "jmp __engine_shim@PLT\n";
#endif
  os << "\t.ascii \"MPLI\"\n";
  os << infoLabel << ":\n";
  os << "\t" << ".long " << codeLabel << " - .\n";
  os << "\t" << ".word " << curFunc.numFormalArgs << ", " << curFunc.numAutoVars << ", " << curFunc.evalStackDepth <<  ", " << curFunc.funcAttrs << " // func storage info\n";

  if (curFunc.numFormalArgs) {
    os << "\t" << "// PrimType of formal arguments\n";
  }
  EmitAsmFormalArgInfo(func);
  if (curFunc.numAutoVars) {
    os << "\t" << "// PrimType of automatic variables\n";
  }
  EmitAsmAutoVarsInfo(func);

  os << "\t.p2align 1\n";
  os << codeLabel << ":\n";
}

void MirGenerator::EmitAsmBaseNode(mre_instr_t &m) {
  std::stringstream ss1, ss2;
  base_node_t *b = m.bn();
  ASSERT(m.op < kREOpLast, "Opcode out of range");
  ss1 << ".byte OP_" << RE_OpName[m.op] << ", " << hex
    << "0x" << static_cast<int>(b->primType) << ", "
    << "0x" << static_cast<int>(b->typeflag) << ", "
    << "0x" << static_cast<int>(b->numOpnds);
  ss2 << left << setw(50) << ss1.str() << "// " << right << setfill('0') << setw(4) << hex << GetFuncOffset();
  EmitString(ss2.str(), 4);
}

void MirGenerator::EmitAsmBaseNode(mre_instr_t &m, string comment) {
  stringstream ss1, ss2;
  base_node_t *b = m.bn();
  ASSERT(m.op < kREOpLast, "Opcode out of range");
  ss1 << ".byte OP_" << RE_OpName[m.op] << ", " << hex
    << "0x" << static_cast<int>(b->primType) << ", "
    << "0x" << static_cast<int>(b->typeflag) << ", "
    << "0x" << static_cast<int>(b->numOpnds);
  ss2 << left << setw(50) << ss1.str() << "// " << right << setfill('0') << setw(4) << hex << GetFuncOffset()
      << ": " << comment;
  EmitString(ss2.str(), 4);
}

void MirGenerator::EmitBytes(uint8 *b, int count) {
  stringstream ss;
  ss << ".byte ";
  for (int i=0; i<count; ++i) {
    ss << hex << "0x" << static_cast<int>(b[i]);
    if (i<count-1) {
      ss << ", ";
    }
  }
  EmitString(ss.str(), count);
}

void MirGenerator::EmitBytesComment(uint8 *b, int count, string &comment) {
  stringstream ss;
  ss << ".byte ";
  for (int i=0; i<count; ++i) {
    ss << hex << "0x" << static_cast<int>(b[i]);
    if (i<count-1) {
      ss << ", ";
    }
  }
  ss << "\t" << comment;
  EmitString(ss.str(), count);
}

inline void MirGenerator::EmitAsmShort(uint16 s) {
  EmitBytes(reinterpret_cast<uint8 *>(&s), 2);
}

inline void MirGenerator::EmitAsmWord(uint32_t w) {
  EmitBytes(reinterpret_cast<uint8 *>(&w), 4);
}

inline void MirGenerator::EmitAsmLong(uint64_t q) {
  EmitBytes(reinterpret_cast<uint8 *>(&q), 8);
}

inline void MirGenerator::EmitAsmWord(uint32_t w, string comment) {
  EmitBytesComment(reinterpret_cast<uint8 *>(&w), 4, comment);
}

void MirGenerator::EmitAsmCall(CallNode *fstmt) {
  MIRFunction *callFunc = GlobalTables::GetFunctionTable().funcTable.at(fstmt->puIdx);
  ASSERT(callFunc, "EmitAsmCall function null");
  std::string funcName = callFunc->GetName();
  PrimType pType = PTY_void;

  // Find call return type
#if 0
  // TODO: check why values returned is always PTY_void
  MIRType *type = globaltable.GetTypeFromTyIdx(callFunc->returnTyidx);
  pType = type->GetPrimType();
#endif
  // Workaround for above - set call ret type to type of %%retval0 if there is return val
  if (fstmt->GetNext() &&
      (fstmt->GetNext()->op == OP_regassign || fstmt->GetNext()->op == OP_dassign) &&
      ((UnaryStmtNode *)fstmt->GetNext())->uOpnd->op == OP_regread &&
      ((RegreadNode *)(((UnaryStmtNode *)fstmt->GetNext())->uOpnd))->regIdx == -kSregRetval0) {
    pType = ((UnaryStmtNode *)fstmt->GetNext())->uOpnd->primType;
  }
  // WARNING: special case for MCC_CallFastNative to call the native func directly as an icall
  if (funcName.compare("MCC_CallFastNative") == 0) {
    mre_instr_t node(RE_icall, pType, fstmt->numOpnds);
    // evaluate call parameters - 1st one is addr of native func
    FlattenExpr(fstmt);
    EmitAsmBaseNode(node);
    return;
  }
  // TODO: move lowerings to mplcg belowering or earlier maplecomb phases
  mre_instr_t node(RE_addroffunc);
  string tmpName;
  if (funcName.compare(0, 18, "MCC_CallSlowNative") == 0) {
    tmpName = "MCC_CallSlowNative";
  } else {
    tmpName = funcName;
  }
  MIRIntrinsicID intrn = LowerToIntrinsic(tmpName);
  if (intrn == INTRN_UNDEFINED) {
    // not in intrinsic list - lower call to addroffunc + icall
    node.primType = PTY_a64;
    EmitAsmBaseNode(node);
    EmitAsmFuncAddr(funcName);
  }
  FlattenExpr(fstmt);
  if (intrn == INTRN_UNDEFINED) {
    node.op = RE_icall;
    node.primType = pType;
    node.bn()->numOpnds = fstmt->numOpnds+1;
    EmitAsmBaseNode(node);
  } else {
    // lower call to intrinsiccall
    node.op = RE_intrinsiccall;
    node.primType = pType;
    node.param.intrinsic.numOpnds = fstmt->numOpnds;
    node.param.intrinsic.intrinsicId = static_cast<uint8>(intrn);
    EmitAsmBaseNode(node, GetIntrinsicName(intrn));
  }
}

uint32 MirGenerator::GetFieldOffset(TyIdx tyidx, FieldID fieldID) {
  int32 offset = 0;
  MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyidx);
  MIRPtrType *pointerty = static_cast<MIRPtrType *>(type);
  ASSERT(pointerty, "expect a pointer type at iread node");

  MIRType *pointedType = nullptr;
  if (fieldID != 0) {
     MIRType *pointedty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pointerty->pointedTyIdx);
     MIRStructType *structty = nullptr;
     if (pointedty->GetKind() != kTypeJArray) {
      structty = static_cast<MIRStructType *>(pointedty);
    } else {
      // it's a Jarray type. using it's parent's field info: java.lang.Object
      structty = static_cast<MIRJarrayType *>(pointedty)->GetParentType();
    }
    ASSERT(structty != nullptr, "structty is null in MirGenerator::GetFieldOffset");
    FieldPair thepair = structty->TraverseToField(fieldID);
    pointedType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(thepair.second.first);
    offset = becommon.GetFieldOffset(structty, fieldID).first;
    // use pointedType->GetPrimType() as new type?
  } else {
    // type = GetPointedToType(pointerty);
    // use type->GetPrimType() as new primtype?
  }
  return offset;
}

void MirGenerator::EmitAsmComment(CommentNode *stmt) {
  // cross ref info to source are in the comments nodes from .mpl input
  os << "\t// " << stmt->comment.data << "\n";
}

void MirGenerator::EmitAsmIreadPCoff(AddrofNode *fexpr, const string &var) {
  mre_instr_t expr(RE_ireadpcoff, fexpr->primType, fexpr->numOpnds);
  BaseNode node(OP_ireadpcoff, fexpr->primType, fexpr->numOpnds);
  FlattenExpr(&node);
  EmitAsmBaseNode(expr);
  EmitString(".long _PTR"+var+"-.", 4);
}

void MirGenerator::EmitAsmAddroffPC(AddrofNode *fexpr, const string &var) {
  mre_instr_t expr(RE_addroffpc, fexpr->primType, fexpr->numOpnds);
  BaseNode node(OP_addroffpc, fexpr->primType, fexpr->numOpnds);
  FlattenExpr(&node);
  EmitAsmBaseNode(expr);
  EmitString(".long "+var+"-.", 4);
}

void MirGenerator::EmitAsmFuncAddr(std::string funcName) {
  EmitString(".quad " + funcName, 8);
}

void MirGenerator::EmitAsmLabel(LabelIdx lbidx, bool genOffset) {
  MIRFunction *func = GetCurFunction();

#if 1
  string label = "mirbin_label_"+to_string(func->puIdxOrigin)+"_"+to_string(lbidx);
  // TODO: fix issue - currently have to disable fpm->run in CG to avoid duplicate labels
  if (genOffset) {
    EmitString(".long "+label+"-.", 4);
  } else {
    os << label+":\n";
  }
#else
  string label = func->GetLabelName(lbidx);
  // Many strangeness with labels if we generate labels into .s
  // by the label's name in string table:
  // - we get @ and | characters in name string that the assembler complains
  // - duplicate name strings across different label idx in the same function
  // - duplicate label idx in same function if all of CG's phases are run.
  if (label.find("CLEANUP") != string::npos) {
    return;
  }
  replace(label.begin(), label.end(), '@', '_');
  replace(label.begin(), label.end(), '|', '_');
  if (genOffset) {
    os << "\t.long " << ". - "
       << "mirbin_label_" << dec << func->puIdxOrigin << "_" << label << "\n";
  } else {
    os << "mirbin_label_" << dec << func->puIdxOrigin << "_" << label << ":\n";
  }
#endif
}

void MirGenerator::EmitAsmConststr(UStrIdx strIdx) {
  std::string labelStr;
  labelStr.append("__Ustr_");
  labelStr.append(std::to_string(strIdx.GetIdx()));
  EmitString(".quad "+labelStr+"\t// "+GlobalTables::GetUStrTable().GetStringFromStrIdx(strIdx), 8);
  GStrIdx labstridx = GlobalTables::GetStrTable().GetStrIdxFromName(labelStr);
  MIRSymbol *labelSym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(labstridx);
  if (!labelSym) {
    labelSym = mmodule.mirBuilder->CreateGlobalDecl(labelStr,  GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_a64), kScGlobal);
    labelSym->storageClass = kScFstatic;
    labelSym->sKind = kStConst;
    labelSym->SetConst(mmodule.memPool->New<MIRStrConst>(strIdx, GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_a64)));
  }
}

RE_Opcode MirGenerator::MapEHOpcode(RE_Opcode op) {
  MIRModule *module = GetCurFunction()->module;
  if (module->IsCModule()) {
    if (op == RE_catch) {
      op = RE_cppcatch;
    } else if (op == RE_try) {
      op = RE_cpptry;
    }
  } else if (module->IsJavaModule()) {
    if (op == RE_catch) {
      op = RE_javacatch;
    } else if (op == RE_try) {
      op = RE_javatry;
    }
  }
  return op;
}

