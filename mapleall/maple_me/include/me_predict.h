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

#ifndef MAPLE_ME_INCLUDE_ME_PREDICT_H
#define MAPLE_ME_INCLUDE_ME_PREDICT_H
#include "me_function.h"
#include "bb.h"
#include "me_phase.h"
#include "dominance.h"
#include "me_ident_loops.h"
namespace maple {
// Information about each branch predictor.
struct PredictorInfo {
  const char *name;  /* Name used in the debugging dumps.  */
  const int khitRate;  /* Expected hitrate used by PredictDef call.  */
};

#define DEF_PREDICTOR(ENUM, NAME, HITRATE) ENUM,
enum Predictor {
#include "me_predict.def"
  kEndPrediction
};
#undef DEF_PREDICTOR
enum Prediction { kNotTaken, kTaken };
// Indicate the edge from src to dest.
class Edge {
 public:
  BB *src;
  BB *dest;
  Edge *next;  // the edge with the same src
  uint32_t probability;
  uint32_t frequency;

  Edge(BB *bb1, BB *bb2) : src(bb1), dest(bb2), next(nullptr), probability(0), frequency(0) {}
};

// Represents predictions on edge.
class EdgePrediction {
 public:
  Edge *epEdge;
  EdgePrediction *epNext;
  Predictor epPredictor;
  int32_t epProbability;

  EdgePrediction(Edge *e) : epEdge(e), epNext(nullptr), epPredictor(kPredNoPrediction), epProbability(-1) {}
};
// Emistimate frequency for MeFunction.
class MePrediction : public AnalysisResult {
 public:
  bool predictDebug;
  static const PredictorInfo kPredictorInfo[kEndPrediction + 1];
  explicit MePrediction(MemPool *mp, MemPool *tmppool, MeFunction *mf, Dominance *dm, IdentifyLoops *loops,
                        MeIRMap *map)
      : AnalysisResult(mp),
        predictDebug(false),
        mePredAlloc(mp),
        tmpAlloc(tmppool),
        func(mf),
        dom(dm),
        meLoop(loops),
        hmap(map),
        bbPredictions(tmpAlloc.Adapter()),
        edges(tmpAlloc.Adapter()),
        backEdgeProb(tmpAlloc.Adapter()),
        bbVisited(tmpAlloc.Adapter()),
        backEdges(tmpAlloc.Adapter()) {}

  Edge *FindEdge(const BB *src, const BB *dest);
  bool IsBackEdge(const Edge *e);
  bool IsExit(BB *bb, LoopDesc *loop);
  Predictor ReturnPrediction(MeExpr *, Prediction *);
  void PredictEdge(Edge *e, Predictor predictor, int probability);
  void PredEdgeDef(Edge *e, Predictor pred, Prediction taken);
  void BBLevelPredictions();
  void Init();
  bool PredictedByLoopHeuristic(const BB *bb);
  void SortLoops();
  void PredictLoops();
  void PredictByOpcode(BB *bb);
  void EstimateBBProb(BB *bb);
  void ClearBBPredictions(const BB *bb);
  void CombinePredForBB(const BB *bb);
  void PropagateFreq(BB *head, BB *bb);
  void EstimateLoops();
  void EstimateBBFrequencies();
  void EstimateProbability();
  void Dump();
 protected:
  MapleAllocator mePredAlloc;
  MapleAllocator tmpAlloc;
  MeFunction *func;
  Dominance *dom;
  IdentifyLoops *meLoop;
  MeIRMap *hmap;
  MapleVector<EdgePrediction *> bbPredictions;
  MapleVector<Edge *> edges;
  MapleMap<Edge *, double> backEdgeProb;  // used in EstimateBBFrequency
  MapleVector<bool> bbVisited;
  MapleVector<Edge *> backEdges;  // all backedges of loops
};

class MeDoPredict : public MeFuncPhase {
 public:
  explicit MeDoPredict(MePhaseID id) : MeFuncPhase(id) {}

  AnalysisResult *Run(MeFunction *func, MeFuncResultMgr *m) override;
  std::string PhaseName() const override {
    return "mepredict";
  }
};
}  // namespace maple
#endif  // MAPLE_ME_INCLUDE_ME_PREDICT_H
