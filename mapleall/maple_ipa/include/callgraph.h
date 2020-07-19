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

#ifndef MAPLE_IPA_INCLUDE_CALLGRAPH_H
#define MAPLE_IPA_INCLUDE_CALLGRAPH_H
#include "module_phase.h"
#include "mir_nodes.h"
#include "class_hierarchy.h"
#include "mir_builder.h"

namespace maple {
class SCCNode;
typedef enum {
  kCallTypeInvalid,
  kCallTypeCall,
  kCallTypeVirtualCall,
  kCallTypeSuperCall,
  kCallTypeInterfaceCall,
  kCallTypeIcall,
  kCallTypeIntrinsicCall,
  kCallTypeXinitrinsicCall,
  kCallTypeIntrinsicCallWithType,
  kCallTypeCustomCall,
  kCallTypePolymorphicCall,
  kCallTypeFakeThreadStartRun
} CallType;

// Information description of each callsite
class CallInfo {
 private:
  CallType ctype : 4;    /* call type */
  char *line;            /* line info of .java file */
  MIRFunction *mirFunc;  /* Used to get signature */
  StmtNode *callStmt;    /* call statement */
  uint32 loopDepth;
  uint32 id;

 public:
  CallInfo(CallType type, char *l, MIRFunction *call, StmtNode *s, uint32 ld, uint32 stmtID)
    : ctype(type), line(l), mirFunc(call), callStmt(s), loopDepth(ld), id(stmtID) {}

  virtual ~CallInfo() {}

  void Dump() const;
  char *GetLine() {
    return line;
  }

  uint32 GetID() const {
    return id;
  }

  const char *GetCalleeName() const;
  CallType GetCallType() const {
    return ctype;
  }

  uint32 GetLoopDepth() const {
    return loopDepth;
  }

  const char *GetCallTypeName() const;
  StmtNode *GetCallStmt() const {
    return callStmt;
  }

  MIRFunction *GetFunc() const {
    return mirFunc;
  }
};

class CGNode;
typedef std::pair<CallInfo *, CGNode *> Callsite;

/* node in callgraph */
class CGNode {
 public:
  /* mirFunc is generated from callStmt's puIdx from mpl instruction
   * mirFunc will be nullptr if CGNode represents a external/intrinsic call */
  uint32 id;
  SCCNode *sccNode;  // the id of the scc where this cgnode belongs to
  MIRFunction *mirFunc;
  MapleVector<Callsite> callees; /* vector of all callsites in current node */
  uint32 numReferences;          /* The number of the node in this or other CGNode's callees */

  /* funcation candidate for virtual call
   * now the candidates would be same function name from base class to subclass
   * with type inference, the candidates would be reduced */
  MapleSet<CGNode *> callerSet;
  uint32 stmtcount;  // count number of statements in the function, reuse this as callsite id
  uint32 nodecount;  /* count number of MIR nodes in the function */

  void AddNumRefs() {
    ++numReferences;
  }

  void DecreaseNumRefs() {
    --numReferences;
  }

 public:
  // this flag is used to mark the function which will read the current method invocation stack or something else,
  // so it cannot be inlined and all the parent nodes which contain this node should not be inlined, either.
  bool must_not_be_inlined = false;
  MapleVector<MIRFunction *> vcallCands;
  CGNode(MIRFunction *func, MapleAllocator *alloc, uint32 index)
    : id(index),
      sccNode(nullptr),
      mirFunc(func),
      callees(alloc->Adapter()),
      numReferences(0),
      callerSet(std::less<CGNode *>(), alloc->Adapter()),
      stmtcount(0),
      nodecount(0),
      vcallCands(alloc->Adapter()) {}

  ~CGNode() {}

  void Dump(std::ofstream &fout) const;
  void DumpDetail() const;
  MIRFunction *GetMIRFunction() const {
    return mirFunc;
  }

  void AddCallsite(CallInfo *, CGNode *);
  void RemoveCallsite(const CallInfo *, CGNode *);
  uint32 GetID() const {
    return id;
  }

  SCCNode *GetSCCNode() {
    return sccNode;
  }

  void SetSCCNode(SCCNode *node) {
    sccNode = node;
  }

  int32 GetPuidx() const {
    return mirFunc ? mirFunc->puIdx : -1;
  }

  const std::string &GetMIRFuncName() const {
    return mirFunc ? mirFunc->GetName() : GlobalTables::GetStrTable().GetStringFromStrIdx(0);
  }

  void AddCandsForCallNode(const KlassHierarchy *kh);
  void AddVCallCandidate(MIRFunction *func) {
    vcallCands.push_back(func);
  }

  bool HasSetVCallCandidates() const {
    return vcallCands.size() != 0;
  }

  MIRFunction *HasOneCandidate() const;
  MapleVector<MIRFunction *> &GetVCallCandidates() {
    return vcallCands;
  }

  /* add caller to CGNode */
  inline void AddCaller(CGNode *caller) {
    callerSet.insert(caller);
  }

  inline void DelCaller(CGNode *caller) {
    callerSet.erase(caller);
  }

  bool HasCaller() const {
    return (callerSet.size() != 0);
  }

  uint32 NumberOfUses() const {
    return callerSet.size();
  }

  bool IsCalleeOf(CGNode *func);
  bool HasCalleeFunc(const char *callname);
  void IncrStmtCount() {
    ++stmtcount;
  }

  void IncrNodeCountBy(uint32 x) {
    nodecount += x;
  }

  uint32 GetStmtCount() const {
    return stmtcount;
  }

  uint32 GetNodeCount() const {
    return nodecount;
  }

  void Reset() {
    stmtcount = 0;
    nodecount = 0;
    numReferences = 0;
    callees.clear();
    vcallCands.clear();
  }

  uint32 NumberOfCallSites() const {
    return callees.size();
  }

  /* iterator */
  typedef MapleVector<Callsite>::iterator iterator;
  MapleVector<Callsite> &GetCallee() {
    return callees;
  }

  inline iterator CalleeBegin() {
    return callees.begin();
  }

  inline iterator CalleeEnd() {
    return callees.end();
  }

  inline CallInfo *GetCallInfo(iterator it) const {
    return (*it).first;
  }

  inline CGNode *GetCalleeNode(iterator it) const {
    return (*it).second;
  }

  inline MapleSet<CGNode *>::iterator CallerBegin() const {
    return callerSet.begin();
  }

  inline MapleSet<CGNode *>::iterator CallerEnd() const {
    return callerSet.end();
  }
};

class SCCNode {
 public:
  uint32 id;
  MapleVector<CGNode *> cgNodes;
  MapleSet<SCCNode *> callerScc;
  MapleSet<SCCNode *> calleeScc;

  explicit SCCNode(uint32 index, MapleAllocator *alloc)
    : id(index),
      cgNodes(alloc->Adapter()),
      callerScc(std::less<SCCNode *>(), alloc->Adapter()),
      calleeScc(std::less<SCCNode *>(), alloc->Adapter()) {}

  virtual ~SCCNode() {}

  void AddCGNode(CGNode *cgn) {
    cgNodes.push_back(cgn);
  }

  void Dump();
  void DumpCycle();
  void Verify();
  void Setup();
  const MapleVector<CGNode *> &GetCGNodes() const {
    return cgNodes;
  }

  const MapleSet<SCCNode *> &GetCalles() const {
    return calleeScc;
  }

  bool HasRecursion() const;
  bool HasCaller() const {
    return (callerScc.size() != 0);
  }
};

class CallGraph : public AnalysisResult {
 private:
  MIRModule *mirModule;
  MapleAllocator cgalloc;
  MIRBuilder *mirBuilder;
  CGNode *entry_node;  // For main function, nullptr if there is multiple entries
  MapleVector<CGNode *> rootNodes;
  const char *fileName; /* used for output dot file */
  KlassHierarchy *klassh;
  MapleVector<CGNode *> nodesMap;  // index is PUIdx
  MapleVector<SCCNode *> sccTopologicalVec;
  CGNode *callExternal; /* Auxiliary node used in icall/intrinsic call */
  uint32 numOfNodes;
  uint32 numOfSccs;
  std::unordered_set<uint64> callsiteHash;

 private:
  void GenCallGraph();
  CGNode *GetOrGenCGNode(PUIdx puIdx);
  CallType GetCallType(Opcode op) const;
  CallInfo *GenCallInfo(CallType type, char *l, MIRFunction *call, StmtNode *s, uint32 loopDepth, uint32 callsiteid) {
    return cgalloc.GetMemPool()->New<CallInfo>(type, l, call, s, loopDepth, callsiteid);
  }

  void FindRootNodes();
  void SCCTopologicalSort(std::vector<SCCNode *> &sccNodes);
  void SetCompilationFunclist();
  void IncrNodesCount(CGNode *cgnode, base_node_t *bn);

 public:
  CallGraph(MIRModule *m, MemPool *mp, KlassHierarchy *kh, const char *fn);
  ~CallGraph() {}
  CGNode *CallExternal() const {
    return callExternal;
  }

  void BuildCallGraph();
  CGNode *GetEntryNode() const {
    return entry_node;
  }

  const MapleVector<CGNode *> &GetRootNodes() const {
    return rootNodes;
  }

  KlassHierarchy *GetKlassh() const {
    return klassh;
  }

  const MapleVector<SCCNode *> &GetSCCTopVec() const {
    return sccTopologicalVec;
  }

  const MapleVector<CGNode *> GetNodesMap() const {
    return nodesMap;
  }

  void HandleBody(MIRFunction *, BlockNode *, CGNode *, uint32);
  void AddCallGraphNode(MIRFunction *);
  void DumpToFile(bool dumpall = true);
  void Dump() const;
  CGNode *GetCGNode(MIRFunction *func) const;
  CGNode *GetCGNode(PUIdx puIdx) const;
  SCCNode *GetSCCNode(MIRFunction *func) const;
  bool IsRootNode(MIRFunction *func) const;
  void UpdateCallGraphNode(CGNode *node);
  void RecomputeSCC();

  void DelNode(CGNode *node);

  bool debug_flag;
  bool debug_scc;
  void BuildSCC();
  void VerifySCC();
  void BuildSCCDFS(CGNode *caller, uint &visitIndex, std::vector<SCCNode *> &sccNodes, std::vector<CGNode *> &cgNodes,
                   std::vector<uint32> &visitedOrder, std::vector<uint32> &lowestOrder, std::vector<bool> &inStack,
                   std::stack<uint32> &visitStack);
};

class DoCallGraph : public ModulePhase {
 public:
  DoCallGraph(ModulePhaseID id) : ModulePhase(id) {}

  AnalysisResult *Run(MIRModule *module, ModuleResultMgr *m) override;
  std::string PhaseName() const override {
    return "callgraph";
  }

  virtual ~DoCallGraph(){};
};

class IPODevirtulize {
 public:
  IPODevirtulize(MIRModule *m, MemPool *mp, KlassHierarchy *kh) : cgalloc(mp),
      mirBuilder(cgalloc.GetMemPool()->New<MIRBuilder>(m)),
      klassh(kh), debugFlag(false) { }
  void DevirtualFinal();
  KlassHierarchy *GetKlassh() const {
    return klassh;
  }

 private:
  MapleAllocator cgalloc;
  MIRBuilder *mirBuilder;
  KlassHierarchy *klassh;
  bool debugFlag;

  void SearchDefInMemberMethods(Klass *klass);
  void SearchDefInClinit(const Klass *klass);
};

class DoIPODevirtulize : public ModulePhase {
 public:
  explicit DoIPODevirtulize(ModulePhaseID id) : ModulePhase(id) {}
  AnalysisResult *Run(MIRModule *module, ModuleResultMgr *m) override;
  std::string PhaseName() const override {
    return "ipodevirtulize";
  }
  virtual ~DoIPODevirtulize() {};
};
}  // namespace maple
#endif  // MAPLE_IPA_INCLUDE_CALLGRAPH_H
