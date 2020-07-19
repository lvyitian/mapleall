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

#ifndef MAPLEBE_INCLUDE_CG_ARK_ARKCG_H
#define MAPLEBE_INCLUDE_CG_ARK_ARKCG_H

#include "cg.h"
#include "ark_cg_func.h"

namespace maplebe {
using namespace std;

// Supporting classes for GCTIB merging
class GCTIBKey {
  uint32_t header;
  vector<uint64_t> bitmap_words;

 public:
  const uint32_t GetHeader() const {
    return header;
  }

  const vector<uint64_t> GetBitmapWords() const {
    return bitmap_words;
  }

  GCTIBKey(uint32_t &rcHeader, vector<uint64_t> &patternWords) : header(rcHeader), bitmap_words(patternWords) {}
  virtual ~GCTIBKey() {}
};

class Hasher {
 public:
  size_t operator()(const GCTIBKey &key) const {
    size_t hash = key.GetHeader();
    return hash;
  }
};

class EqualFn {
 public:
  bool operator()(const GCTIBKey &t1, const GCTIBKey &t2) const {
    vector<uint64_t> t1Words = t1.GetBitmapWords();
    vector<uint64_t> t2Words = t2.GetBitmapWords();

    if (t1.GetHeader() != t2.GetHeader() || t1Words.size() != t2Words.size()) {
      return false;
    }

    for (int i = 0; i < t1Words.size(); i++) {
      if (t1Words[i] != t2Words[i]) {
        return false;
      }
    }
    return true;
  }
};

class GCTIBPattern {
  int id;
  string name;
  GCTIBKey key;

 public:
  int GetId() {
    static int id = 0;
    return id++;
  }

  string GetName() const {
    return name;
  }

  void SetName(const string &ptnName) {
    name = ptnName;
  }

  GCTIBPattern(GCTIBKey &patternKey) : key(patternKey) {
    id = GetId();
    name = GCTIB_PREFIX_STR + string("PTN_") + to_string(id);
 }
  virtual ~GCTIBPattern() {}
};

class ArkCG : public CG {
 public:
  const vector<string> &eh_exclusive_name_vec;
  const unordered_map<string, vector<string>> &cycle_pattern_map;
  unordered_map<GCTIBKey, GCTIBPattern, Hasher, EqualFn> key2pattern;
  unordered_map<string, GCTIBPattern> sym2pattern;

 public:
  explicit ArkCG(MIRModule *mod, const CGOptions &opts, bool runCg, const char *output,
                     const vector<string> &namevec, const unordered_map<string, vector<string>> &patternMap)
    : CG(mod, opts, runCg, output), eh_exclusive_name_vec(namevec), cycle_pattern_map(patternMap) {}

#if DEBUG
  explicit ArkCG(MIRModule *mod, const CGOptions &opts, bool run_cg, const char *output,
                     const vector<string> &namevec, const unordered_map<string, vector<string>> &pattern_map,
                     bool cvt_locals_to_pregs)
    : CG(mod, opts, run_cg, output), eh_exclusive_name_vec(namevec), cycle_pattern_map(pattern_map) {}

#endif

  ~ArkCG() {}

  Insn *CreateGhostInstruction() override { return nullptr; }

  CGFunc *CreateCGFunc(MIRModule *mod, MIRFunction *mirFunc, BECommon &bec, MemPool *mp,
                       MapleAllocator *mallocator) override {
    return mp->New<ArkCGFunc>(mod, this, mirFunc, &bec, mp, mallocator);
  }

  InsnVisitor *NewInsnModifier(CGFunc *cgfunc, MemPool *mp) override { return nullptr; }

  void GenerateObjectMaps(BECommon &becommon) override;

  bool IsExclusiveFunc(MIRFunction *) override;

  void FindOrCreateRepresentiveSym(vector<uint64_t> &bitmapWords, uint32_t rcHeader, string name);

  string FindGCTIBPatternName(const string &name) override;

 public:
  enum { kR8List, kR16List, kR32List, kR64List, kV64List, kV128List, kRVLast};
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_ARK_ARKCG_H
