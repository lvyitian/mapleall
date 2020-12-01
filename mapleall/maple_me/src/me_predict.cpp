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

#include <iostream>
#include <fstream>
#include <queue>
#include <unordered_set>
#include <algorithm>
#include "me_predict.h"
#include "me_ir.h"
#include "me_irmap.h"
namespace maple {
// The base value for branch probability notes and edge probabilities.
const int kProbBase = 10000;
// The base value for BB frequency.
const int kFreqBase = 10000;
/* PROB_VERY_UNLIKELY should be small enough so basic block predicted
   by it gets below HOT_BB_FREQUENCY_FRACTION.  */
#define PROB_VERY_UNLIKELY (kProbBase / 2000 - 1)
#define PROB_EVEN (kProbBase / 2)
#define PROB_VERY_LIKELY (kProbBase - PROB_VERY_UNLIKELY)
#define PROB_ALWAYS (kProbBase)
#define PROB_UNLIKELY (kProbBase / 5 - 1)
#define PROB_LIKELY (kProbBase - PROB_UNLIKELY)
#define PROB_UNINITIALIZED (0)
// Recompute hitrate in percent to our representation.
#define HITRATE(VAL) ((int)((VAL)*kProbBase + 50) / 100)
#define DEF_PREDICTOR(ENUM, NAME, HITRATE) { NAME, HITRATE },
const PredictorInfo MePrediction::kPredictorInfo[kEndPrediction + 1] = {
#include "me_predict.def"
  // Upper bound on predictors.
  { NULL, 0 }
};

// return the edge src->dest if it exists.
Edge *MePrediction::FindEdge(const BB *src, const BB *dest) {
  Edge *e = edges[src->id.idx];
  while (e) {
    if (dest == e->dest) {
      return e;
    }
    e = e->next;
  }
  return nullptr;
}

// Recognize backedges identified by loops.
bool MePrediction::IsBackEdge(const Edge *e) {
  for (MapleVector<Edge *>::iterator it = backEdges.begin(); it != backEdges.end(); it++) {
    if (*it == e) {
      return true;
    }
  }
  return false;
}

// Try to guess whether the value of return means error code.
Predictor MePrediction::ReturnPrediction(MeExpr *val, Prediction *prediction) {
  /* VOID.  */
  if (!val || val->meOp != kMeOpReg) {
    return kPredNoPrediction;
  }
  RegMeExpr *reg = static_cast<RegMeExpr *>(val);
  if (reg->defBy != kDefByStmt) {
    return kPredNoPrediction;
  }
  MeStmt *def = reg->def.defStmt;
  if (def->op != OP_regassign) {
    return kPredNoPrediction;
  }
  MeExpr *rhs = static_cast<AssignMeStmt *>(def)->rhs;
  if (rhs->meOp != kMeOpConst) {
    return kPredNoPrediction;
  }
  ConstMeExpr *constVal = static_cast<ConstMeExpr *>(rhs);
  if (constVal->primType == PTY_ref) {
    // NULL is usually not returned.
    if (constVal->IsZero()) {
      *prediction = kNotTaken;
      return kPredNullReturn;
    }
  } else if (IsPrimitiveInteger(constVal->primType)) {
    // Negative return values are often used to indicate errors.
    if (constVal->GetIntValue() < 0) {
      *prediction = kNotTaken;
      return kPredNegativeReturn;
    }
    /* Constant return values seems to be commonly taken.Zero/one often represent
       booleans so exclude them from the heuristics.  */
    if (!constVal->IsZero() && !constVal->IsOne()) {
      *prediction = kNotTaken;
      return kPredConstReturn;
    }
  }
  return kPredNoPrediction;
}

// Predict edge E with the given PROBABILITY.
void MePrediction::PredictEdge(Edge *e, Predictor predictor, int probability) {
  if (e->src != cfg->commonEntryBB && e->src->succ.size() > 1) {
    BB *src = e->src;
    EdgePrediction *i = tmpAlloc.GetMemPool()->New<EdgePrediction>(e);
    EdgePrediction *pred = bbPredictions[src->id.idx];
    i->epNext = pred;
    bbPredictions[src->id.idx] = i;
    i->epProbability = probability;
    i->epPredictor = predictor;
    i->epEdge = e;
  }
}

// Predict edge e by given predictor if possible.
void MePrediction::PredEdgeDef(Edge *e, Predictor predictor, Prediction taken) {
  int probability = kPredictorInfo[static_cast<int>(predictor)].khitRate;
  if (taken != kTaken) {
    probability = kProbBase - probability;
  }
  PredictEdge(e, predictor, probability);
}

/* Look for basic block that contains unlikely to happen events
   (such as noreturn calls) and mark all paths leading to execution
   of this basic blocks as unlikely.  */
void MePrediction::BBLevelPredictions() {
  RetMeStmt *retStmt = NULL;
  for (BB *bb : cfg->commonExitBB->pred) {
    MeStmt *meLast = bb->meStmtList.last;
    if (meLast && meLast->op == OP_return) {
      retStmt = static_cast<RetMeStmt *>(meLast);
      break;
    }
  }
  if (!retStmt || retStmt->opnds.size() == 0) {
    return;
  }
  MeExpr *retval = retStmt->opnds[0];
  if (!retval || retval->meOp != kMeOpReg) {
    return;
  }
  RegMeExpr *reg = static_cast<RegMeExpr *>(retval);
  if (reg->defBy != kDefByPhi) {
    return;
  }
  MePhiNode *defPhi = reg->def.defPhi;
  Prediction direction;
  const int defphiOpndSize = defPhi->opnds.size();
  CHECK_FATAL(defphiOpndSize > 0, "container check");
  Predictor pred = ReturnPrediction(defPhi->opnds[0], &direction);
  /* Avoid the degenerate case where all return values form the function
     belongs to same category (ie they are all positive constants)
     so we can hardly say something about them.  */
  int phiNumArgs = defPhi->opnds.size();
  for (int i = 0; i < phiNumArgs; i++) {
    pred = ReturnPrediction(defPhi->opnds[i], &direction);
    if (pred != kPredNoPrediction) {
      BB *dest = defPhi->defBB;
      BB *src = dest->pred[i];
      Edge *findEdgeresult = FindEdge(src, dest);
      CHECK_FATAL(findEdgeresult != nullptr, "null ptr check");
      PredEdgeDef(findEdgeresult, pred, direction);
    }
  }
}

// Make edges for all bbs in the cfg.
void MePrediction::Init() {
  bbPredictions.resize(cfg->bbVec.size());
  edges.resize(cfg->bbVec.size());
  bbVisited.resize(cfg->bbVec.size());
  for (uint32_t i = 0; i < cfg->bbVec.size(); i++) {
    bbVisited[i] = true;
    bbPredictions[i] = nullptr;
    edges[i] = nullptr;
    BB *bb = cfg->bbVec[i];
    if (bb == nullptr) {
      continue;
    }
    for (MapleVector<BB *>::iterator it = bb->succ.begin(); it != bb->succ.end(); it++) {
      Edge *e = tmpAlloc.GetMemPool()->New<Edge>(bb, *it);
      e->next = edges[i];
      edges[i] = e;
    }
  }
  if (cfg->commonEntryBB != cfg->first_bb) {
    bbVisited[cfg->commonEntryBB->id.idx] = true;
  }
  if (cfg->commonExitBB != cfg->last_bb) {
    bbVisited[cfg->commonExitBB->id.idx] = true;
  }
}

// Return true if e is predicated by one of loop heuristics.
bool MePrediction::PredictedByLoopHeuristic(const BB *bb) {
  EdgePrediction *i = bbPredictions[bb->id.idx];
  if (i == nullptr) {
    return false;
  }
  do {
    if (i->epPredictor == kPredLoopExit) {
      return true;
    }
    i = i->epNext;
  } while (i);
  return false;
}

// Sort loops first so that hanle innermost loop first in EstimateLoops.
void MePrediction::SortLoops() {
  uint32_t size = meLoop->meloops.size();
  for (uint32_t i = 0; i < size; i++) {
    for (uint32_t j = 1; j < size - i; j++) {
      LoopDesc *loop = meLoop->meloops[j - 1];
      LoopDesc *loop1 = meLoop->meloops[j];
      if (loop->nestdepth < loop1->nestdepth) {
        LoopDesc *temp = loop;
        meLoop->meloops[j - 1] = loop1;
        meLoop->meloops[j] = temp;
      }
    }
  }
}

void MePrediction::PredictLoops() {
  for (uint32 i = 0; i < meLoop->meloops.size(); i++) {
    LoopDesc *loop = meLoop->meloops[i];
    MapleSet<BBId> &loopbbs = loop->loop_bbs;
    // Find loop exit bbs.
    MapleVector<Edge *> exits(tmpAlloc.Adapter());
    for (MapleSet<BBId>::iterator stit = loopbbs.begin(); stit != loopbbs.end(); stit++) {
      BB *bb = cfg->bbVec[stit->idx];
      if (bb->succ.size() < 2) {
        continue;
      }
      for (uint32 i = 0; i < bb->succ.size(); i++) {
        if (!loop->Has(bb->succ[i])) {
          Edge *e = FindEdge(bb, bb->succ[i]);
          exits.push_back(e);
          break;
        }
      }
    }
    // predicate loop exit.
    if (exits.size() == 0) {
      return;
    }
    for (MapleVector<Edge *>::iterator it = exits.begin(); it != exits.end(); it++) {
      // Loop heuristics do not expect exit conditional to be inside
      // inner loop.  We predict from innermost to outermost loop.
      if (PredictedByLoopHeuristic((*it)->src)) {
        continue;
      }
      int32_t probability = kProbBase - kPredictorInfo[kPredLoopExit].khitRate;
      PredictEdge(*it, kPredLoopExit, probability);
    }
  }
}

// Predict using opcode of the last statement in basic block.
void MePrediction::PredictByOpcode(BB *bb) {
  MeStmt *meLast = bb->meStmtList.last;
  if (!bb || !meLast || !meLast->IsCondBr()) {
    return;
  }
  CondGotoMeStmt *condStmt = static_cast<CondGotoMeStmt *>(meLast);
  bool istruebr = condStmt->op == OP_brtrue;
  MeExpr *testExpr = condStmt->opnd;
  MeExpr *op0 = nullptr;
  MeExpr *op1 = nullptr;
  Opcode cmp;
  // Only predict MeOpOp operands now.
  if (testExpr->meOp != kMeOpOp) {
    return;
  } else {
    OpMeExpr *cmpExpr = static_cast<OpMeExpr *>(testExpr);
    op0 = cmpExpr->GetOpnd(0);
    op1 = cmpExpr->GetOpnd(1);
    cmp = testExpr->op;
  }
  Edge *e0 = edges[bb->id.idx];
  Edge *e1 = e0->next;
  Edge *thenEdge = istruebr ? ((e0->dest->bbLabel == condStmt->offset) ? e0 : e1)
                             : ((e0->dest->bbLabel == condStmt->offset) ? e1 : e0);
  PrimType pty = op0->primType;
  // Try "pointer heuristic." A comparison ptr == 0 is predicted as false.
  // Similarly, a comparison ptr1 == ptr2 is predicted as false.
  if (pty == PTY_ptr || pty == PTY_ref) {
    if (cmp == OP_eq) {
      PredEdgeDef(thenEdge, kPredPointer, kNotTaken);
    } else if (cmp == OP_ne) {
      PredEdgeDef(thenEdge, kPredPointer, kTaken);
    }
  } else {
    // Try "opcode heuristic." EQ tests are usually false and NE tests are usually true. Also,
    // most quantities are positive, so we can make the appropriate guesses
    // about signed comparisons against zero.
    switch (cmp) {
      case OP_eq:
      case OP_ne: {
        Prediction taken = ((cmp == OP_eq) ? kNotTaken : kTaken);
        // identify that a comparerison of an integer equal to a const or floating point numbers
        // are equal to be not taken
        if (IsPrimitiveFloat(pty) || (IsPrimitiveInteger(pty) && (op1->meOp == kMeOpConst))) {
          PredEdgeDef(thenEdge, kPredOpcodeNonEqual, taken);
        }
        break;
      }
      case OP_lt:
      case OP_le:
      case OP_gt:
      case OP_ge: {
        if (op1->meOp == kMeOpConst) {
          ConstMeExpr *constVal = static_cast<ConstMeExpr *>(op1);
          if (constVal->IsZero() || constVal->IsOne()) {
            Prediction taken = ((cmp == OP_lt || cmp == OP_le) ? kNotTaken : kTaken);
            PredEdgeDef(thenEdge, kPredOpcodePositive, taken);
          }
        }
        break;
      }
      default:
        break;
    }
  }
}

void MePrediction::EstimateBBProb(BB *bb) {
  for (uint32 i = 0; i < bb->succ.size(); i++) {
    BB *dest = bb->succ[i];
    // javatry fallthrou if taken.
    MeStmt *meLast = bb->meStmtList.last;
    MeStmt *destLast = dest->meStmtList.last;
    if (meLast && (meLast->op == OP_javatry || meLast->op == OP_try || meLast->op == OP_cpptry) && i == 0) {
      Edge *findEdgeResult = FindEdge(bb, dest);
      CHECK_FATAL(findEdgeResult != nullptr, "null ptr check");
      PredEdgeDef(findEdgeResult, kPredJavaTry, kTaken);
    } else if (destLast && destLast->op == OP_return) {
      Edge *findEdgeResult = FindEdge(bb, dest);
      CHECK_FATAL(findEdgeResult != nullptr, "null ptr check");
      PredEdgeDef(findEdgeResult, kPredEarlyReturn, kNotTaken);
    } else if (dest != cfg->commonExitBB && dest != bb && dom->Dominate(bb, dest) && !dom->Postdominate(dest, bb)) {
      for (auto stmt : dest->meStmtList) {
        if (stmt->op == OP_call || stmt->op == OP_callassigned) {
          PUIdx puIdx = static_cast<CallMeStmt *>(stmt)->puIdx;
          MIRFunction *callee = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
          if (callee) {
            // call heuristic : exceptional calls not taken.
            if (!callee->IsPure()) {
              Edge *findEdgeResult = FindEdge(bb, dest);
              CHECK_FATAL(findEdgeResult != nullptr, "null ptr check");
              PredEdgeDef(findEdgeResult, kPredCall, kNotTaken);
              break;
            } else {
              Edge *findEdgeResult = FindEdge(bb, dest);
              CHECK_FATAL(findEdgeResult != nullptr, "null ptr check");
              // call heristic : normal call taken.
              PredEdgeDef(findEdgeResult, kPredCall, kTaken);
              break;
            }
          }
        }
      }
    }
  }
  PredictByOpcode(bb);
}

void MePrediction::ClearBBPredictions(const BB *bb) {
  EdgePrediction *preds = bbPredictions[bb->id.idx];
  if (!preds) {
    return;
  }
  preds = nullptr;
}

/* Combine predictions into single probability and store them into CFG.
   Remove now useless prediction entries.  */
void MePrediction::CombinePredForBB(const BB *bb) {
  /* When there is no successor or only one choice, prediction is easy.
     When we have a basic block with more than 2 successors, the situation
     is more complicated as DS theory cannot be used literally.
     More precisely, let's assume we predicted edge e1 with probability p1,
     thus: m1({b1}) = p1.  As we're going to combine more than 2 edges, we
     need to find probability of e.g. m1({b2}), which we don't know.
     The only approximation is to equally distribute 1-p1 to all edges
     different from b1.  */

  if (bb->succ.size() != 2) {
    MapleSet<Edge *> unlikelyEdges(tmpAlloc.Adapter());

    // Identify all edges that have a probability close to very unlikely.
    EdgePrediction *preds = bbPredictions[bb->id.idx];
    if (preds) {
      EdgePrediction *pred = nullptr;
      for (pred = preds; pred; pred = pred->epNext) {
        if (pred->epProbability <= PROB_VERY_UNLIKELY) {
          unlikelyEdges.insert(pred->epEdge);
        }
      }
    }
    uint32_t all = PROB_ALWAYS;
    uint32_t nedges = 0;
    uint32_t unlikelyCount = 0;
    Edge *e = edges[bb->id.idx];
    for (Edge *e1 = e; e1; e1 = e1->next) {
      if (e1->probability > 0) {
        all -= e1->probability;
      } else {
        nedges++;
        if (unlikelyEdges.size() > 0 && unlikelyEdges.find(e) != unlikelyEdges.end()) {
          all -= PROB_VERY_UNLIKELY;
          e1->probability = PROB_VERY_UNLIKELY;
          unlikelyCount++;
        }
      }
    }
    if (unlikelyCount == nedges) {
      unlikelyEdges.clear();
      ClearBBPredictions(bb);
      return;
    }

    uint32_t total = 0;
    for (Edge *e1 = e; e1; e1 = e1->next) {
      if (e1->probability == PROB_UNINITIALIZED) {
        e1->probability = all / (nedges - unlikelyCount);
        total += e1->probability;
      }
      if (predictDebug) {
        LogInfo::MapleLogger() << "Predictions for bb " << bb->id.idx << " \n";
        if (unlikelyEdges.size() == 0) {
          LogInfo::MapleLogger() << nedges << " edges in bb " << bb->id.idx << " predicted to even probabilities.\n";
        } else {
          LogInfo::MapleLogger() << nedges << " edges in bb " << bb->id.idx << " predicted with some unlikely edges\n";
        }
      }
    }
    if (total != all) {
      e->probability += all - total;
    }
    ClearBBPredictions(bb);
    return;
  }
  if (predictDebug) {
    LogInfo::MapleLogger() << "Predictions for bb " << bb->id.idx << " \n";
  }
  int nunknown = 0;
  Edge *first = nullptr;
  Edge *second = nullptr;
  for (Edge *e = edges[bb->id.idx]; e != nullptr; e = e->next) {
    if (!first) {
      first = e;
    } else if (!second) {
      second = e;
    }
    if (e->probability == PROB_UNINITIALIZED) {
      nunknown++;
    }
  }
  // If we have only one successor which is unknown, we can compute missing probablity.
  if (nunknown == 1) {
    int32_t prob = PROB_ALWAYS;
    Edge *missing = nullptr;
    for (Edge *e = edges[bb->id.idx]; e != nullptr; e = e->next) {
      if (e->probability > 0) {
        prob -= e->probability;
      } else if (missing == nullptr) {
        missing = e;
      } else {
        CHECK_FATAL(false, "unreachable");
      }
    }
    CHECK_FATAL(missing != nullptr, "null ptr check");
    missing->probability = prob;
    return;
  }
  EdgePrediction *preds = bbPredictions[bb->id.idx];
  int combinedProbability = kProbBase / 2;
  int denominator = 0;
  if (preds) {
    // use DS Theory.
    for (EdgePrediction *pred = preds; pred; pred = pred->epNext) {
      Predictor predictor = pred->epPredictor;
      int probability = pred->epProbability;
      if (pred->epEdge != first) {
        probability = kProbBase - probability;
      }
      denominator = (combinedProbability * probability + (kProbBase - combinedProbability) * (kProbBase - probability));
      /* Use FP math to avoid overflows of 32bit integers.  */
      if (denominator == 0) {
        /* If one probability is 0% and one 100%, avoid division by zero.  */
        combinedProbability = kProbBase / 2;
      } else {
        combinedProbability = static_cast<int>(static_cast<double>(combinedProbability) *
                                               probability * kProbBase / denominator);
      }
    }
  }
  if (predictDebug) {
    LogInfo::MapleLogger() << "combined heuristics of edge BB" << bb->id.idx
         << "->BB" << first->dest->id.idx << ":" << combinedProbability * 100/kProbBase << "%\n";
    if (preds) {
      for (EdgePrediction *pred = preds; pred; pred = pred->epNext) {
        Predictor predictor = pred->epPredictor;
        int probability = pred->epProbability;
        LogInfo::MapleLogger() << kPredictorInfo[predictor].name << " heuristics of edge BB" << pred->epEdge->src->id.idx
             << "->BB" << pred->epEdge->dest->id.idx << ":" << probability * 100/kProbBase << "%\n";
      }
    }
  }
  ClearBBPredictions(bb);
  first->probability = combinedProbability;
  second->probability = kProbBase - combinedProbability;
}

void MePrediction::PropagateFreq(BB *head, BB *bb) {
  if (bbVisited[bb->id.idx]) {
    return;
  }
  // 1. find bfreq(bb)
  if (bb == head) {
    head->frequency = kFreqBase;
  } else {
    for (uint32_t i = 0; i < bb->pred.size(); i++) {
      BB *pred = bb->pred[i];
      Edge *e = FindEdge(pred, bb);
      if (!bbVisited[pred->id.idx] && pred != bb && !IsBackEdge(e)) {
        if (predictDebug) {
          LogInfo::MapleLogger() << "BB" << bb->id.idx << " can't be estimated because it's predecessor BB" << pred->id.idx
               << " hasn't be estimated yet\n";
          if (bb->IsInLoop() && (bb->InTryBlock() || bb->IsCatch() || pred->InTryBlock() || pred->IsCatch())) {
            LogInfo::MapleLogger() << "BB" << bb->id.idx << " can't be recognized as loop head/tail because of eh.\n";
          }
        }
        return;
      }
    }
    uint32_t freq = 0;
    double cyclicProb = 0;
    for (BB *pred : bb->pred) {
      Edge *e = FindEdge(pred, bb);
      if (IsBackEdge(e) && e->dest == head) {
        cyclicProb += backEdgeProb[e];
      } else {
        freq += e->frequency;
      }
    }
    if (cyclicProb > 1 - std::numeric_limits<double>::epsilon()) {
      cyclicProb = 1 - std::numeric_limits<double>::epsilon();
    }
    bb->frequency = static_cast<uint32_t>(freq / (1 - cyclicProb));
  }
  // 2. calculate frequencies of bb's out edges
  if (predictDebug) {
    LogInfo::MapleLogger() << "Estimate Frequency of BB" << bb->id.idx << "\n";
  }

  bbVisited[bb->id.idx] = true;
  uint32_t tmp = 0;
  uint32_t total = 0;
  Edge *bestEdge = nullptr;
  for (uint32_t i = 0; i < bb->succ.size(); i++) {
    Edge *e = FindEdge(bb, bb->succ[i]);
    if (i == 0) {
      bestEdge = e;
      tmp = e->probability;
    } else {
      if (e->probability > tmp) {
        tmp = e->probability;
        bestEdge = e;
      }
    }
    e->frequency = e->probability * bb->frequency * 1.00 / kProbBase;
    total += e->frequency;
    if (e->dest == head) {
      backEdgeProb[e] = e->probability * bb->frequency * 1.00 / (kProbBase * kFreqBase);
    }
  }
  if (bestEdge != nullptr && total != bb->frequency) {
    bestEdge->frequency += bb->frequency - total;
  }
  // 3. propagate to successor blocks
  for (uint32_t i = 0; i < bb->succ.size(); i++) {
    BB *succ = bb->succ[i];
    if (!bbVisited[succ->id.idx]) {
      PropagateFreq(head, succ);
    }
  }
}

void MePrediction::EstimateLoops() {
  for (uint32 i = 0; i < meLoop->meloops.size(); i++) {
    LoopDesc *loop = meLoop->meloops[i];
    MapleSet<BBId> &loopbbs = loop->loop_bbs;
    Edge *backEdge = FindEdge(loop->tail, loop->head);
    backEdges.push_back(backEdge);
    for (MapleSet<BBId>::iterator stit = loopbbs.begin(); stit != loopbbs.end(); stit++) {
      bbVisited[stit->idx] = false;
    }
    PropagateFreq(loop->head, loop->head);
  }
  // Now propagate the frequencies through all the blocks.
  for (uint32_t i = 0; i < bbVisited.size(); i++) {
    bbVisited[i] = false;
  }
  if (cfg->commonEntryBB != cfg->first_bb) {
    bbVisited[cfg->commonEntryBB->id.idx] = false;
  }
  if (cfg->commonExitBB != cfg->last_bb) {
    bbVisited[cfg->commonExitBB->id.idx] = false;
  }
  cfg->commonEntryBB->frequency = kFreqBase;
  for (BB *bb : cfg->commonEntryBB->succ) {
    PropagateFreq(bb, bb);
  }
}

void MePrediction::EstimateBBFrequencies() {
  BB *entry = cfg->commonEntryBB;
  edges[entry->id.idx]->probability = PROB_ALWAYS;
  double backProb = 0;
  for (uint32_t i = 0; i < cfg->bbVec.size(); i++) {
    Edge *e = edges[i];
    while (e) {
      if (e->probability > 0) {
        backProb = e->probability;
      } else {
        backProb = kProbBase / 2;
      }
      backProb = backProb * 1.00 / kProbBase;
      backEdgeProb.insert(std::make_pair(e, backProb));
      e = e->next;
    }
  }
  // First compute frequencies locally for each loop from innermost
  // to outermost to examine frequencies for back edges.
  EstimateLoops();
}

// Main function
void MePrediction::EstimateProbability() {
  Init();
  BBLevelPredictions();
  //  RecordLoopExits ();
  if (meLoop->meloops.size() > 0) {
    // innermost loop in the first place for EstimateFrequencies.
    SortLoops();
    PredictLoops();
  }
  for (uint32 i = 0; i < cfg->bbVec.size(); i++) {
    BB *bb = cfg->bbVec[i];
    if (bb != nullptr) {
      EstimateBBProb(bb);
    }
  }
  for (uint32 i = 0; i < cfg->bbVec.size(); i++) {
    BB *bb = cfg->bbVec[i];
    if (bb != nullptr) {
      CombinePredForBB(bb);
    }
  }
  for (uint32 i = 0; i < cfg->bbVec.size(); i++) {
    int32_t all = 0;
    for (Edge *e = edges[i]; e != nullptr; e = e->next) {
      if (predictDebug) {
        LogInfo::MapleLogger() << "probability for edge BB" << e->src->id.idx << "->BB" << e->dest->id.idx << " is "
             << e->probability / 100 << "%\n";
      }
      all += e->probability;
    }
    if (edges[i] != nullptr) {
      CHECK_FATAL(all == kProbBase, "total probability is not 1");
    }
  }
  EstimateBBFrequencies();
}

// Estimate the execution frequecy for all bbs.
AnalysisResult *MeDoPredict::Run(MeFunction *func, MeFuncResultMgr *m) {
  MeIRMap *hmap = static_cast<MeIRMap *>(m->GetAnalysisResult(MeFuncPhase_IRMAPBUILD, func, !MeOption::quiet));
  CHECK_FATAL(hmap != nullptr, "hssamap is nullptr");

  Dominance *dom = static_cast<Dominance *>(m->GetAnalysisResult(MeFuncPhase_DOMINANCE, func, !MeOption::quiet));
  CHECK_FATAL(dom, "dominance phase has problem");

  IdentifyLoops *meloop = static_cast<IdentifyLoops *>(m->GetAnalysisResult(MeFuncPhase_IDENTLOOPS, func, !MeOption::quiet));
  CHECK_FATAL(meloop != nullptr, "identloops has problem");

  std::string mePredPhaseName = PhaseName();
  MemPool *mePredMp = mempoolctrler.NewMemPool(mePredPhaseName.c_str());
  MemPool *tempMp = mempoolctrler.NewMemPool("mePrediction temporaries");

  MePrediction *mePredict = mePredMp->New<MePrediction>(mePredMp, tempMp, func, dom, meloop, hmap);
  if (DEBUGFUNC(func)) {
    mePredict->predictDebug = true;
  }
  mePredict->EstimateProbability();

  if (DEBUGFUNC(func)) {
    LogInfo::MapleLogger() << "\n============== Prediction =============" << std::endl;
    hmap->Dump();
  }
  mempoolctrler.DeleteMemPool(tempMp);
  return mePredict;
}

}  // namespace maple
