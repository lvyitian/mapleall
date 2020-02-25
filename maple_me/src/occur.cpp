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

#include "occur.h"
#include "ssa_pre.h"

// The methods associated with the data structures that represent occurrences
// and work candidates for SSAPRE

PreWorkCand *PreWorkCand::workcandHashTable[kWorkcandHashLength];

namespace maple {

void MeOccur::DumpOccur(IRMap *irMap) {
  MIRModule *mod = &irMap->ssaTab->mirModule;
  LogInfo::MapleLogger() << "MeOccur ";
  Dump(irMap);
  LogInfo::MapleLogger() << "\n  class: " << classid << std::endl;
  if (def) {
    LogInfo::MapleLogger() << "  def by class: " << def->classid;
  } else {
    LogInfo::MapleLogger() << "  no-def";
  }
  LogInfo::MapleLogger() << std::endl;
}

// return if this occur dominate occ
bool MeOccur::IsDominate(Dominance *dom, MeOccur *occ) {
  switch (occty) {
    case kOccReal: {
      switch (occ->occty) {
        case kOccReal: {
          if (mirbb == occ->mirbb) {
            MeRealOcc *thisrealocc = static_cast<MeRealOcc *>(this);
            MeRealOcc *domocc = static_cast<MeRealOcc *>(occ);
            return thisrealocc->seq <= domocc->seq;
          }
          return dom->Dominate(mirbb, occ->mirbb);
        }
        case kOccPhiocc: {
          if (mirbb == occ->mirbb) {
            return false;
          }
          return dom->Dominate(mirbb, occ->mirbb);
        }
        case kOccPhiopnd:
        case kOccExit:
        case kOccMembar:
        case kOccUse:
          return dom->Dominate(mirbb, occ->mirbb);
        default:
          CHECK_FATAL(false, "");
      }
      break;
    }
    case kOccPhiocc:
    case kOccMembar:
    case kOccUse:
      return dom->Dominate(mirbb, occ->mirbb);
    default:
      CHECK_FATAL(false, "");
  }
}

// return true if either:
// operand is nullptr (def is null), or
// has_real_use is false and defined by a PHI not will be avail
bool MePhiOpndOcc::IsOkToInsert() {
  // return (def == nullptr || (!has_real_use && ))
  if (!def) {
    return true;
  }
  if (!has_real_use) {
    MeOccur *defocc = def;
    if (defocc->occty == kOccPhiocc && !static_cast<MePhiOcc *>(defocc)->IsWillBeAvail()) {
      return true;
    }
  }
  return false;
}

bool MePhiOcc::IsOpndDefByRealOrInserted() {
  for (uint32 i = 0; i < phiopnds.size(); i++) {
    MePhiOpndOcc *phiopnd = phiopnds[i];
    MeOccur *defocc = phiopnd->def;
    if (defocc->occty == kOccReal || defocc->occty == kOccInserted) {
      return true;
    }
  }

  return false;
}

void MeRealOcc::Dump(IRMap *irMap) {
  MIRModule *mod = &irMap->ssaTab->mirModule;
  if (occty == kOccReal) {
    if (!is_lhs) {
      LogInfo::MapleLogger() << "RealOcc ";
    } else {
      LogInfo::MapleLogger() << "RealOcc(LHS) ";
    }
    if (meexpr) {
      meexpr->Dump(irMap);
    } else {
      mestmt->Dump(irMap);
    }
    if (mestmt && mestmt->bb)
      LogInfo::MapleLogger() << " at bb" << mestmt->bb->id.idx << " seq " << seq << " classid " << classid;
    else {
      LogInfo::MapleLogger() << " classid " << classid;
    }
  } else {
    LogInfo::MapleLogger() << "MembarOcc ";
    LogInfo::MapleLogger() << " at bb" << mirbb->id.idx << " seq " << seq;
  }
}

void MePhiOcc::Dump(IRMap *irMap) {
  MIRModule *mod = &irMap->ssaTab->mirModule;
  LogInfo::MapleLogger() << "PhiOcc ";
  LogInfo::MapleLogger() << "PHI(";
  uint32 size = phiopnds.size();
  for (uint32 i = 0; i < size; i++) {
    phiopnds[i]->Dump(irMap);
    if (i != size - 1) {
      LogInfo::MapleLogger() << ",";
    }
  }
  LogInfo::MapleLogger() << ")";
  LogInfo::MapleLogger() << " at bb" << mirbb->id.idx << " classid " << classid;
}

void MePhiOpndOcc::Dump(IRMap *irMap) {
  MIRModule *mod = &irMap->ssaTab->mirModule;
  LogInfo::MapleLogger() << "PhiOpndOcc at bb" << mirbb->id.idx << " classid " << classid;
  if (has_real_use) {
    LogInfo::MapleLogger() << "(has_real_use) ";
  }
}

void MeExitOcc::Dump(IRMap *irMap) {
  MIRModule *mod = &irMap->ssaTab->mirModule;
  LogInfo::MapleLogger() << "ExitOcc at bb" << mirbb->id.idx;
}

void MeInsertedOcc::Dump(IRMap *irMap) {
  MIRModule *mod = &irMap->ssaTab->mirModule;
  LogInfo::MapleLogger() << "InsertedOcc at bb" << mirbb->id.idx << " classid " << classid;
}

void MeUseOcc::Dump(IRMap *irMap) {
  MIRModule *mod = &irMap->ssaTab->mirModule;
  LogInfo::MapleLogger() << "UseOcc at bb" << mirbb->id.idx;
}

// compute bucket index for the work candidate in workcandHashTable
uint32 PreWorkCand::ComputeWorkCandHashIndex(MeExpr *x) {
  uint32 hidx = 0;
  MeExprOp meOp = x->meOp;
  switch (meOp) {
    case kMeOpAddrof:
    case kMeOpAddroffunc:
    case kMeOpGcmalloc:
    case kMeOpConst:
    case kMeOpConststr:
    case kMeOpConststr16:
    case kMeOpSizeoftype:
      hidx = (static_cast<uint32>(x->exprID)) << 5;
      break;
    case kMeOpVar: {
      VarMeExpr *v = static_cast<VarMeExpr *>(x);
      hidx = v->ost->index.idx << 4;
      break;
    }
    case kMeOpReg: {
      RegMeExpr *regmeexpr = static_cast<RegMeExpr *>(x);
      hidx = (static_cast<uint32>(regmeexpr->regIdx)) << 6;
      break;
    }
    case kMeOpIvar: {
      IvarMeExpr *iv = static_cast<IvarMeExpr *>(x);
      hidx = ComputeWorkCandHashIndex(iv->base) + (iv->tyIdx.GetIdx() << 3) + iv->fieldID;
      break;
    }
    case kMeOpOp: {
      OpMeExpr *o = static_cast<OpMeExpr *>(x);
      hidx = static_cast<uint32>(o->op);
      hidx += ComputeWorkCandHashIndex(o->GetOpnd(0)) << 2;
      if (o->GetOpnd(1)) {
        hidx += ComputeWorkCandHashIndex(o->GetOpnd(1)) << 2;
        if (o->GetOpnd(2)) {
          hidx += ComputeWorkCandHashIndex(o->GetOpnd(2)) << 2;
        }
      }
      break;
    }
    case kMeOpNary: {
      NaryMeExpr *a = static_cast<NaryMeExpr *>(x);
      hidx = static_cast<uint32>(x->op);
      for (int32 i = 0; i < a->NumMeExprOpnds(); i++) {
        hidx += ComputeWorkCandHashIndex(a->GetOpnd(i)) << 2;
      }
      break;
    }
    default:
      CHECK_FATAL(false, "MeOP NIY");
  }
  return hidx % kWorkcandHashLength;
}

// insert occ as real_occs[pos] after shifting the vector elements further down
void PreWorkCand::InsertRealOccAt(MeRealOcc *occ, MapleVector<MeRealOcc *>::iterator it, PUIdx pidx) {
  ASSERT(pidx != 0, "puIdx of realocc cannot be 0");
  if (pidx != puIdx) {
    ASSERT(!has_local_opnd, "candidate with local opnd cannot have real occurrences in more than one PU");
    puIdx = 0;
  }
  CHECK(real_occs.size() != 0, "real_occs has no element in PreWorkCand::InsertRealOccAt");
  real_occs.insert(it, occ);
}

// insert occ in real_occs maintaining sorted order according to dtPreOrder
void PreWorkCand::AddRealOccSorted(Dominance *dom, MeRealOcc *occ, PUIdx pidx) {
  ASSERT(real_occs.size() > 0, "AddRealOccSorted: real_occs is empty");
  uint32 occdfn = dom->dtDfn[occ->mirbb->id.idx];
  // check the end of real_occs first because inserting at end is most frequent
  if (occdfn > dom->dtDfn[real_occs.back()->mirbb->id.idx]) {
    AddRealOccAsLast(occ, pidx);
  } else if (occdfn == dom->dtDfn[real_occs.back()->mirbb->id.idx]) {
    if (occ->seq >= real_occs.back()->seq) {
      AddRealOccAsLast(occ, pidx);
    } else {
      MapleVector<MeRealOcc *>::reverse_iterator rit = real_occs.rbegin();
      rit++;
      while (rit != real_occs.rend()) {
        if (occdfn > dom->dtDfn[(*rit)->mirbb->id.idx]) {
          break;
        }
        if (occ->seq >= (*rit)->seq) {
          break;
        }
      }
      InsertRealOccAt(occ, rit.base(), pidx);
    }
  } else {
    // search from beginning of real_occs
    MapleVector<MeRealOcc *>::iterator it = real_occs.begin();
    bool found = false;
    do {
      if (occdfn > dom->dtDfn[(*it)->mirbb->id.idx]) {
        it++;
      } else if (occdfn == dom->dtDfn[(*it)->mirbb->id.idx]) {
        if (occ->seq > (*it)->seq) {
          it++;
        } else {
          found = true;
        }
      } else {
        found = true;
      }
    } while (!found && it != real_occs.end());
    if (!found) {
      AddRealOccAsLast(occ, pidx);
    } else {
      InsertRealOccAt(occ, it, pidx);
    }
  }
}

// compute bucket index for the work candidate in workcandHashTable
uint32 PreStmtWorkCand::ComputeStmtWorkCandHashIndex(MeStmt *stmt) {
  uint32 hidx = (static_cast<uint32>(stmt->op)) << 3;
  switch (stmt->op) {
    case OP_assertnonnull: {
      UnaryMeStmt *unarystmt = static_cast<UnaryMeStmt *>(stmt);
      hidx += ComputeWorkCandHashIndex(unarystmt->opnd) << 2;
      break;
    }
    case OP_dassign: {
      CHECK_FATAL(stmt->GetVarLhs() != nullptr && stmt->GetRhs() != nullptr, "null ptr check");
      VarMeExpr *varmeexpr = stmt->GetVarLhs();
      hidx += varmeexpr->ost->index.idx << 4;
      hidx += ComputeWorkCandHashIndex(stmt->GetRhs()) << 1;
      break;
    }
    case OP_intrinsiccall:
    case OP_intrinsiccallwithtype: {
      IntrinsiccallMeStmt *intrnstmt = static_cast<IntrinsiccallMeStmt *>(stmt);
      hidx += (static_cast<uint32>(intrnstmt->intrinsic)) << 3;
      if (stmt->op == OP_intrinsiccallwithtype) {
        IntrinsiccallMeStmt *withtypestmt = static_cast<IntrinsiccallMeStmt *>(stmt);
        hidx += withtypestmt->tyIdx.GetIdx() << 1;
      }
      for (int32 i = 0; i < intrnstmt->NumMeStmtOpnds(); i++) {
        hidx += ComputeWorkCandHashIndex(intrnstmt->opnds[i]) << 1;
      }
      break;
    }
    case OP_callassigned: {
      CallMeStmt *callass = static_cast<CallMeStmt *>(stmt);
      hidx += callass->puIdx;
      for (int32 i = 0; i < callass->NumMeStmtOpnds(); i++) {
        hidx += ComputeWorkCandHashIndex(callass->opnds[i]) << 2;
      }
      if (!callass->mustDefList.empty()) {
        MeExpr *lhs = callass->mustDefList.front().lhs;
        VarMeExpr *lhsVar = static_cast<VarMeExpr *>(lhs);
        hidx += lhsVar->ost->index.idx << 1;
      }
      break;
    }
    default:
      CHECK_FATAL(false, "ComputeStmtWorkCandHashIndex: NYI");
  }
  return hidx % kWorkcandHashLength;
}

}  // namespace maple
