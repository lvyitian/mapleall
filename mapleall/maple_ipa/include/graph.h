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

#ifndef MAPLE_IPA_INCLUDE_GRAPH_H
#define MAPLE_IPA_INCLUDE_GRAPH_H
#include <string>
#include "mpl_logging.h"
#include "types_def.h"
#include "mempool.h"
#include "mempool_allocator.h"

namespace maple {
class ModuleIPA;
class ModuleNode {
 public:
  ModuleNode(MapleAllocator *alloc, const std::string &nm, uint32 vnum)
    : id(vnum), in_nums(0), dyn_in_nums(0), name(nm), kid_nodes(alloc->Adapter()) {}

  virtual ~ModuleNode() {}

  void AddEdge(ModuleNode *);

 public:
  uint32 id;           // the unique number of each vertex
  uint32 in_nums;      // the original dynamic incoming
  uint32 dyn_in_nums;  // this field is used for visiting the graph. record in_nums
  std::string name;
  MapleVector<ModuleNode *> kid_nodes;
};

class Graph {
 public:
  MemPool *mp;
  MapleAllocator alloc;
  ModuleIPA *moduleipa;
  ModuleNode *root_node;
  uint32 num_vertex;
  MapleVector<ModuleNode *> vertex_vec;  // map num to node, size must be equal to num_vertex

 public:
  Graph(ModuleIPA *);
  virtual ~Graph() {}
  ModuleNode *BuildModuleNode(const std::string &);
  void TopologicalSorting(
    std::vector<ModuleNode *> &);  // user delete the vector of the parameter, otherwise memory leak
  void ReverseDump(std::vector<ModuleNode *> &);
  void DumpMakefile();
  void SetRootNode(ModuleNode *mn) {
    CHECK_FATAL(mn->in_nums == 0, "root node's in nums shouldn't be 0");
    this->root_node = mn;
  }
};
}  // namespace maple
#endif  // MAPLE_IPA_INCLUDE_GRAPH_H
