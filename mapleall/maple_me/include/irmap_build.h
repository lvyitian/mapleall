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

#ifndef MAPLE_ME_INCLUDE_IRMAP_BUILD_H
#define MAPLE_ME_INCLUDE_IRMAP_BUILD_H
#include "irmap.h"

namespace maple {
class Prop;

// This class contains methods to convert Maple IR to MeIR.
class IrMapBuild {
 public:
  IRMap *irMap;
  Prop *propagater;
  MIRModule *mirModule;
  SSATab *ssaTab;
  Dominance *dominance;

  IrMapBuild(IRMap *hmap, Prop *prop) : irMap(hmap), propagater(prop),
    mirModule(hmap->mirModule), ssaTab(irMap->ssaTab), dominance(irMap->dominance), curBB(nullptr) {}
  ~IrMapBuild() {}

  VarMeExpr *GetOrCreateVarFromVerSt(const VersionSt *verst);
  RegMeExpr *GetOrCreateRegFromVerSt(const VersionSt *verst);
  MeExpr *BuildLhsVar(const VersionSt *verst, DassignMeStmt *defmestmt, DassignNode *dassign);
  MeExpr *BuildLhsReg(const VersionSt *verst, AssignMeStmt *defmestmt, const RegassignNode *regassign);
  void BuildChiList(MeStmt *mestmt, MapleMap<OStIdx, MayDefNode> &mayDefNodes,
                    MapleMap<OStIdx, ChiMeNode *> &outlist);
  void BuildMustdefList(MeStmt *mestmt, MapleVector<MustDefNode> &mustdeflist,
                        MapleVector<MustDefMeNode> &mustdefList);
  void BuildPhiMeNode(BB *bb);
  void BuildMuList(MapleMap<OStIdx, MayUseNode> &mayuselist, MapleMap<OStIdx, VarMeExpr *> &mulist);
  MeExpr *BuildExpr(BaseNode *mirnode, bool atParm, bool noProp);
  MeStmt *BuildMeStmt(StmtNode *stmt);
  void BuildBB(BB *bb, std::vector<bool> &bbIrmapProcessed);

 private:
  BB *curBB;  // current mapleme::BB being visited
};

}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_IRMAP_BUILD_H
