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

#ifndef MAPLE_IPA_INCLUDE_MODULEIPA_H
#define MAPLE_IPA_INCLUDE_MODULEIPA_H
#include "graph.h"
#include "mempool.h"
#include "mempool_allocator.h"

namespace maple {
class ModuleIPA {
 public:
  MemPool *mempool;
  MapleAllocator alloc;
  Graph *module_graph;
  MapleMap<std::string, void *> string2node_map;

 public:
  ModuleIPA(MemPool *mp)
    : mempool(mp), alloc(mp), module_graph(nullptr), string2node_map(std::less<std::string>(), alloc.Adapter()) {}

  virtual ~ModuleIPA() {}

  void BuildTestCase();
  void BuildModuleGraph(const char *);
  ModuleNode *BuildModuleNode();
  void AddModuleNode(const std::string &strname, ModuleNode *node) {
    string2node_map[strname] = node;
  }

  ModuleNode *GetModuleNodeFromName(const std::string &strname) {
    MapleMap<std::string, void *>::iterator it = string2node_map.find(strname);
    if (it != string2node_map.end()) {
      return static_cast<ModuleNode *>(it->second);
    } else {
      return nullptr;
    }
  }

 private:
  ModuleNode *ParseMplFileNameNode(uint32 &, const char *, uint32);
  void ParseLine(const char *);
  void ParseIpaGraph(const char *);
  void ParseSkipBlank(uint32 &curindex, const char *strline, uint32 length) const {
    while (curindex < length && (strline[curindex] == ' ' || strline[curindex] == '\r')) {  // get rid of blank
      curindex++;
    }
  }
};
}  // namespace maple
#endif  // MAPLE_IPA_INCLUDE_MODULEIPA_H
