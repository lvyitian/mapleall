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

#ifndef MAPLE_IPA_INCLUDE_INLINE_H
#define MAPLE_IPA_INCLUDE_INLINE_H
#include "mir_parser.h"
#include "mir_function.h"
#include "opcode_info.h"
#include "mir_builder.h"
#include "mempool.h"
#include "mempool_allocator.h"
#include "callgraph.h"
#include "module_phase.h"

namespace maple {

#define SPACE_TAB_STR " \t"
#define COMMENTSIGN_STR '#'
#define HYPHEN_STR '-'
#define APPOINT_STR "->"
#define UNDERLINE_STR "_"
#define VERTICAL_LINE_STR "|"
#define NUMBER_ZERO_STR "0"
#define RETURNLOC_STR "return_loc_"
#define THIS_STR "_this"

class MInline {
 public:
  MapleAllocator alloc;
  MIRModule &mod_;
  MIRBuilder &builder_;

  MapleMap<MIRFunction *, bool> inlinable_map_;
  MapleMap<MIRFunction *, uint32> inline_times_map_;
  MapleMap<GStrIdx, MapleSet<GStrIdx> *> inline_prefer;
  MapleMap<GStrIdx, MapleSet<GStrIdx> *> inline_ref2ptr;
  static int level;
  static std::string inlineFuncList;

 private:
  int32 max_budget;
  double growth_rate_threshold;
  uint64 total_size;

 protected:
  KlassHierarchy *klasshierarchy;
  CallGraph *cg_;

 public:
  explicit MInline(MIRModule &mod, MemPool *mp, KlassHierarchy *kh = nullptr, CallGraph *cg = nullptr)
    : alloc(mp),
      mod_(mod),
      builder_(*mod.mirBuilder),
      inlinable_map_(std::less<MIRFunction *>(), alloc.Adapter()),
      inline_times_map_(std::less<MIRFunction *>(), alloc.Adapter()),
      inline_prefer(std::less<GStrIdx>(), alloc.Adapter()),
      inline_ref2ptr(std::less<GStrIdx>(), alloc.Adapter()),
      max_budget(0),
      growth_rate_threshold(0.0),
      total_size(0),
      klasshierarchy(kh),
      cg_(cg) {
  };

  void PerformInline(MIRFunction *, BlockNode *enclosingBlk, CallNode *, MIRFunction *);
  virtual void Inline();
  void CleanupInline();
  virtual ~MInline() {}

 private:
  void ApplyInlineListInfo();
  uint32 RenameSymbols(MIRFunction *, MIRFunction *, uint32, std::unordered_map<uint32, uint32> &);
  void ReplaceSymbols(BaseNode *, uint32, std::unordered_map<uint32, uint32> &);
  uint32 RenameLabels(MIRFunction *, const MIRFunction *, uint32);
  void ReplaceLabels(BaseNode *, uint32);
  void RenamePregs(MIRFunction *, MIRFunction *, std::unordered_map<PregIdx, PregIdx> &);
  void ReplacePregs(BaseNode *, std::unordered_map<PregIdx, PregIdx> &);
  LabelIdx CreateReturnLabel(MIRFunction *, const MIRFunction *, uint32);
  bool SpendBudget(BaseNode *, int32 &, std::unordered_map<MIRFunction *, int32> &, int inlineLevel, MIRFunction *);
  bool NodeInlinable(CGNode *);
  void CanInline(CGNode *, std::unordered_map<MIRFunction *, int32> &);
  virtual bool CanInline(CGNode *node, std::unordered_map<MIRFunction *, bool> &canInline) {
    return false;
  }
  bool IsSimpleFunc(MIRFunction *func) const;
  void CheckCalleeAndInline(MIRFunction *, BlockNode *enclosingBlk, CallNode *, MIRFunction *);
  void InlineCalls(const CGNode *, std::unordered_map<MIRFunction *, int32> &);
  void InlineCallsBlock(MIRFunction *, BlockNode *, BaseNode *, std::unordered_map<MIRFunction *, int32> &);
  void DoRef2ptr(MIRSymbol *retSt);
  void UpdateReturnStmts(MIRFunction *, BlockNode *, LabelIdx, CallReturnVector &, int &, GotoNode *&);
  void CollectMustInlineFuncs(std::unordered_map<MIRFunction *, int32> &);
  void ComputeTotalSize();
  void MarkUsedSymbols(BaseNode *);
};

class DoInline : public ModulePhase {
 public:
  DoInline(ModulePhaseID id) : ModulePhase(id) {}

  AnalysisResult *Run(MIRModule *module, ModuleResultMgr *m) override;
  std::string PhaseName() const override {
    return "inline";
  }

  virtual ~DoInline(){};
};
}  // namespace maple
#endif  // MAPLE_IPA_INCLUDE_INLINE_H
