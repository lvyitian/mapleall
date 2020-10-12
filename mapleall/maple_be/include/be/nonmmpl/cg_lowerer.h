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

#ifndef MAPLEBE_INCLUDE_BE_NONMMPL_CGLOWERER_H_
#define MAPLEBE_INCLUDE_BE_NONMMPL_CGLOWERER_H_
#include "be_lowerer.h"
#include "intrinsics.h"  // For IntrinDesc. This includes 'intrinsic_op.h' as well
#include <vector>
#include <unordered_map>

namespace maplebe {

class CGLowerer : public BELowerer {
  typedef BELowerer::builtin_func_id_t builtin_func_id_t;

  static std::vector<std::pair<builtin_func_id_t, PUIdx>> builtinFuncIds;
  MIRBuilder *mirbuilder;
  bool is_use_reg;  // true if use preg to store the return value
  uint32 labelIdx;
  static std::unordered_map<IntrinDesc*, PUIdx> intrinFuncIds;

#if DEBUG
  MapleMap<StIdx, PregIdx> *autos_to_pregs;
  MapleMap<PregIdx, StIdx> *pregs_to_autos;
  MapleMap<MIRFunction *, MapleMap<PregIdx, StIdx> *> *funcs_to_pregs_to_autos_map;

  BlockNode *new_entry_blk;
#endif

  BaseNode *JavaMergeToCvtType(PrimType dtyp, PrimType styp, BaseNode *src);
  BaseNode *LowerJavascriptIntrinsicop(BaseNode *parent, IntrinsicopNode *intrinnode, IntrinDesc *desc);
  StmtNode *CreateStmtCallWithReturnValue(IntrinsicopNode *intrinnode, const MIRSymbol *ret, PUIdx bfunc,
                                          BaseNode *extraInfo = nullptr);
  StmtNode *CreateStmtCallWithReturnValue(IntrinsicopNode *intrinnode, PregIdx retpidx, PUIdx bfunc,
                                          BaseNode *extraInfo = nullptr);
  BaseNode *LowerJavaIntrinsicop(BaseNode *parent, IntrinsicopNode *intrinnode, IntrinDesc *desc);
  BaseNode *LowerJavaIntrinsicopwithtype(BaseNode *parent, IntrinsicopNode *intrinnode, IntrinDesc *desc);

 public:
  explicit CGLowerer(MIRModule &mod, BECommon &becmmn, MIRFunction *func = nullptr, bool geneh = true)
    : BELowerer(mod, becmmn, kBeTargetNommpl, func) {
    SetOptions(kGenEh);
    is_use_reg = false;  // use preg to store return value
    labelIdx = 0;
    mirbuilder = mod.mirBuilder;
#if DEBUG
    autos_to_pregs = nullptr;
    pregs_to_autos = nullptr;
    funcs_to_pregs_to_autos_map = new MapleMap<MIRFunction *, MapleMap<PregIdx, StIdx> *>(
      std::less<MIRFunction *>(), mirModule.memPoolAllocator.Adapter());
    new_entry_blk = nullptr;
#endif
  }

  explicit CGLowerer(MIRModule &mod, BECommon &becmmn, bool geneh, bool verboseAsm, int32 optlevel)
    : BELowerer(mod, becmmn, kBeTargetNommpl, nullptr) {
    option_flag_t o = 0;
    if (geneh) {
      o |= kGenEh;
    }
    if (verboseAsm) {
      o |= kVerboseAsm;
    }
    SetOptions(o);
    is_use_reg = optlevel >= 2 ? true : false;
    labelIdx = 0;
    mirbuilder = mod.mirBuilder;
#if DEBUG
    autos_to_pregs = nullptr;
    pregs_to_autos = nullptr;
    funcs_to_pregs_to_autos_map = new MapleMap<MIRFunction *, MapleMap<PregIdx, StIdx> *>(
      std::less<MIRFunction *>(), mirModule.memPoolAllocator.Adapter());
    new_entry_blk = nullptr;
#endif
  }

#if DEBUG
  explicit CGLowerer(MIRModule &mod, BECommon &becmmn, bool geneh, bool verbose_asm, bool cvt_locals_to_pregs,
                     int32 optlevel)
    : BELowerer(mod, becmmn, kBeTargetNommpl, nullptr) {
    option_flag_t o = 0;
    if (geneh) {
      o |= kGenEh;
    }
    if (verbose_asm) {
      o |= kVerboseAsm;
    }
    if (cvt_locals_to_pregs) {
      o |= kAutoToPregs;
    }
    SetOptions(o);
    autos_to_pregs = nullptr;
    pregs_to_autos = nullptr;
    is_use_reg = optlevel >= 2 ? true : false;
    labelIdx = 0;
    mirbuilder = mod.mirBuilder;
    funcs_to_pregs_to_autos_map = new MapleMap<MIRFunction *, MapleMap<PregIdx, StIdx> *>(
      std::less<MIRFunction *>(), mirModule.memPoolAllocator.Adapter());
    new_entry_blk = nullptr;
  }

#endif

  ~CGLowerer() {}

  MIRFunction *RegisterFunctionVoidStarToVoid(builtin_func_id_t id, const char *name, const char *paramName);

  void RegisterBuiltIns();

  void RegisterNoReturnFunctions(const char *const fnames[], const int n);
  void InsertExit(MIRFunction *) const;

  void LowerFunc(MIRFunction *f) override {
    labelIdx = 0;
#if DEBUG
    autos_to_pregs = new MapleMap<StIdx, PregIdx>(std::less<StIdx>(), mirModule.memPoolAllocator.Adapter());
    pregs_to_autos = new MapleMap<PregIdx, StIdx>(std::less<PregIdx>(), mirModule.memPoolAllocator.Adapter());
    CHECK_FATAL(funcs_to_pregs_to_autos_map, "");
    funcs_to_pregs_to_autos_map->insert(std::pair<MIRFunction *, MapleMap<PregIdx, StIdx> *>(f, pregs_to_autos));
#endif

    BELowerer::LowerFunc(f);

#if DEBUG
    delete autos_to_pregs;
    autos_to_pregs = nullptr;
#endif
  }

#if DEBUG
  void LowerFormalParameters(MIRFunction *func) override {
    if (ConvertAutoSymbolsToPseudoRegs() && mirModule.IsJavaModule()) {
      InsertRegassignsFromFormalParameters(func);
    }
  }

#endif

  BaseNode *LowerIntrinsicop(BaseNode *, IntrinsicopNode *, BlockNode *) override;

  BaseNode *LowerIntrinsicopwithtype(BaseNode *, IntrinsicopNode *, BlockNode *) override;

  StmtNode *LowerIntrinsiccall(IntrinsiccallNode *intrincall, BlockNode *) override;

  StmtNode *LowerSyncEnterSyncExit(StmtNode *stmt) override;

  PUIdx GetBuiltInToUse(builtin_func_id_t id) override;

  BaseNode *LowerExpr(BaseNode *, BaseNode *, BaseNode *, BlockNode *) override;

  BaseNode *LowerDread(DreadNode *dread) override;

  BaseNode *LowerIread(IreadNode *iread) override {
#if !TARGARK
    // use PTY_u8 for boolean type in dread/iread
    if (iread->primType == PTY_u1) {
      iread->primType = PTY_u8;
    }
#endif
    return BELowerer::LowerIread(iread);
  }

  void LowerDassign(DassignNode *, BlockNode *) override;

  void LowerRegassign(RegassignNode *, BlockNode *) override;

  BaseNode *LowerAddrof(AddrofNode *addrof) override {
    // disable follow assertion, addrof operand is used in RC write barrier.
#if FALSE && DEBUG
    MIRSymbol *sym = mirModule.CurFunction()->GetLocalOrGlobalSymbol(addrof->stIdx);
    CHECK_FATAL(!mirModule.IsJavaModule() || sym->storageClass != kScAuto, "");
#endif
    return BELowerer::LowerAddrof(addrof);
  }

  StmtNode *LowerIntrinsicopDassign(const DassignNode *, IntrinsicopNode *, BlockNode *);

  void LowerGCMalloc(BaseNode *, const GCMallocNode *, BlockNode *, bool perm = false);

  BaseNode *  // remains virtual for further per-platform overriding
  LowerSTACKMalloc(GCMallocNode *, BlockNode *) override;

  void LowerJarrayMalloc(StmtNode *, const JarrayMallocNode *, BlockNode *, bool perm = false);

  BaseNode *LowerSTACKJarrayMalloc(JarrayMallocNode *node, BlockNode *blknode) override;
#if DEBUG
 private:
  void InsertRegassignsFromFormalParameters(MIRFunction *func);

  BlockNode *GetNewEntryBlock() override {
    return new_entry_blk;
  }

 public:
  MapleMap<MIRFunction *, MapleMap<PregIdx, StIdx> *> *GetPregsToLocalVarsMap() {
    return funcs_to_pregs_to_autos_map;
  }

#endif /* DEBUG */

  /*
     A pseudo register refers to a symbol when DreadNode is converted to RegreadNode.
   */
  StIdx GetSymbolReferredToByPseudoRegister(PregIdx regno) const {
#if DEBUG
    auto it = pregs_to_autos->find(regno);
    return it == pregs_to_autos->end() ? StIdx() : it->second;
#else
    return StIdx();
#endif
  }

 private:
  void InsertPregForReturn(MIRFunction *, std::vector<StmtNode *> &);
  void InsertVarForReturn(MIRFunction *, std::vector<StmtNode *> &);

 protected:
  bool IsIntrinsicCallHandledAtLowerLevel(MIRIntrinsicID intrinsic) override;
  bool IsIntrinsicOpHandledAtLowerLevel(MIRIntrinsicID intrinsic) override;
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_BE_NONMMPL_CGLOWERER_H_
