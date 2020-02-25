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

#include <iostream>
#include <fstream>
#include "ssa_mir_nodes.h"
#include "opcode_info.h"
#include "mir_function.h"
#include "ssa_tab.h"

// Allocate data structures to store SSA information. Only statement nodes and
// tree nodes that incur defs and uses are relevant. Tree nodes are made larger
// by allocating the larger SSANodes.  Statement nodes' SSA information is
// stored in class SSATab's StmtsSSAPart, which has an array of pointers indexed
// by the stmtID field of each statement node.

namespace maple {

BaseNode *SSATab::CreateSSAExpr(BaseNode *expr) {
  if (expr->op == OP_addrof || expr->op == OP_dread) {
    AddrofNode *addrofnode = static_cast<AddrofNode *>(expr);
    AddrofSSANode *ssaNode = mirModule.CurFunction()->codeMemPool->New<AddrofSSANode>(addrofnode);
    MIRSymbol *st = mirModule.CurFunction()->GetLocalOrGlobalSymbol(ssaNode->stIdx);
    OriginalSt *ost = FindOrCreateSymbolOriginalSt(st, mirModule.CurFunction()->puIdx, ssaNode->fieldID);
    versionStTable.CreateZeroVersionSt(ost);
    ssaNode->ssaVar = versionStTable.GetZeroVersionSt(ost);
    return ssaNode;
  } else if (expr->op == OP_regread) {
    RegreadNode *rreadnode = static_cast<RegreadNode *>(expr);
    RegreadSSANode *ssaNode = mirModule.CurFunction()->codeMemPool->New<RegreadSSANode>(rreadnode);
    OriginalSt *ost = originalStTable.FindOrCreatePregOriginalSt(ssaNode->regIdx, mirModule.CurFunction()->puIdx);
    versionStTable.CreateZeroVersionSt(ost);
    ssaNode->ssaVar = versionStTable.GetZeroVersionSt(ost);
    return ssaNode;
  } else if (expr->op == OP_iread) {
    IreadNode *ireadnode = static_cast<IreadNode *>(expr);
    IreadSSANode *ssaNode =
        mirModule.CurFunction()->codeMemPool->New<IreadSSANode>(mirModule.CurFuncCodeMemPoolAllocator(), ireadnode);
    BaseNode *newopnd = CreateSSAExpr(expr->Opnd(0));
    if (newopnd) {
      ssaNode->SetOpnd(newopnd, 0);
    }
    return ssaNode;
  } else {
    for (int32 i = 0; i < expr->NumOpnds(); i++) {
      BaseNode *newopnd = CreateSSAExpr(expr->Opnd(i));
      if (newopnd) {
        expr->SetOpnd(newopnd, i);
      }
    }
    return nullptr;
  }
}

void SSATab::CreateSSAStmt(StmtNode *stmt, const BB *curbb, bool ignoreCallassignedDefs) {
  for (int32 i = 0; i < stmt->NumOpnds(); i++) {
    BaseNode *newopnd = CreateSSAExpr(stmt->Opnd(i));
    if (newopnd) {
      stmt->SetOpnd(newopnd, i);
    }
  }
  switch (stmt->op) {
    case OP_maydassign:
    case OP_dassign: {
      MayDefPartWithVersionSt *thessapart =
          stmtsSSAPart.ssaPartMp->New<MayDefPartWithVersionSt>(&stmtsSSAPart.ssaPartAlloc);
      stmtsSSAPart.SetSsapartOf(stmt, thessapart);

      DassignNode *dnode = static_cast<DassignNode *>(stmt);
      MIRSymbol *st = mirModule.CurFunction()->GetLocalOrGlobalSymbol(dnode->stIdx);
      OriginalSt *ost = FindOrCreateSymbolOriginalSt(st, mirModule.CurFunction()->puIdx, dnode->fieldID);
      AddDefBB4Ost(ost->index, curbb->id);
      versionStTable.CreateZeroVersionSt(ost);
      thessapart->ssaVar = versionStTable.GetZeroVersionSt(ost);
      // if the rhs may throw exception, we insert MayDef of the lhs var
      if (stmt->op == OP_maydassign) {
        thessapart->InsertMayDefNode(thessapart->ssaVar, dnode);
      }
      return;
    }
    case OP_regassign: {
      RegassignNode *regnode = static_cast<RegassignNode *>(stmt);
      OriginalSt *ost =
          originalStTable.FindOrCreatePregOriginalSt(regnode->regIdx, mirModule.CurFunction()->puIdx);
      versionStTable.CreateZeroVersionSt(ost);
      VersionSt *vst = versionStTable.GetZeroVersionSt(ost);
      AddDefBB4Ost(ost->index, curbb->id);
      stmtsSSAPart.SetSsapartOf(stmt, vst);
      return;
    }
    case OP_return:
    case OP_throw:
    case OP_gosub:
    case OP_retsub:
      stmtsSSAPart.SetSsapartOf(stmt, stmtsSSAPart.ssaPartMp->New<MayUsePart>(&stmtsSSAPart.ssaPartAlloc));
      return;
    case OP_syncenter:
    case OP_syncexit:
      stmtsSSAPart.SetSsapartOf(stmt, stmtsSSAPart.ssaPartMp->New<MayDefMayUsePart>(&stmtsSSAPart.ssaPartAlloc));
      return;
    case OP_iassign:
      stmtsSSAPart.SetSsapartOf(stmt, stmtsSSAPart.ssaPartMp->New<MayDefPart>(&stmtsSSAPart.ssaPartAlloc));
      return;
    default: {
      if (kOpcodeInfo.IsCallAssigned(stmt->op)) {
        MayDefMayUseMustDefPart *thessapart =
            stmtsSSAPart.ssaPartMp->New<MayDefMayUseMustDefPart>(&stmtsSSAPart.ssaPartAlloc);
        stmtsSSAPart.SetSsapartOf(stmt, thessapart);
        // insert the mustdefs
        CallReturnVector *returnValues = stmt->GetCallReturnVector();
        CHECK_FATAL(returnValues != nullptr, "CreateSSAStmt: failed to retrieve call return vector");
        if (returnValues->size() == 0) {
          return;
        }
        for (uint32 j = 0; j < returnValues->size(); j++) {
          CallReturnPair retpair = (*returnValues)[j];
          if (!retpair.second.IsReg()) {
            StIdx stIdx = (*returnValues)[j].first;
            MIRSymbolTable *symtab = mirModule.CurFunction()->symTab;
            MIRSymbol *st = symtab->GetSymbolFromStIdx(stIdx.Idx());
            OriginalSt *ost = FindOrCreateSymbolOriginalSt(st, mirModule.CurFunction()->puIdx,
                                                                             retpair.second.GetFieldid());
            versionStTable.CreateZeroVersionSt(ost);
            VersionSt *vst = versionStTable.GetZeroVersionSt(ost);
            if (!ignoreCallassignedDefs) {
              AddDefBB4Ost(ost->index, curbb->id);
            }
            thessapart->InsertMustDefNode(vst, stmt);
          } else {
            CHECK_FATAL(false, "NYI");
          }
        }
        return;
      } else if (kOpcodeInfo.IsCall(stmt->op)) {
        stmtsSSAPart.SetSsapartOf(stmt, stmtsSSAPart.ssaPartMp->New<MayDefMayUsePart>(&stmtsSSAPart.ssaPartAlloc));
        return;
      }
    }
  }
  return;
}

}  // namespace maple
