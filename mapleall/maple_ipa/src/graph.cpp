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

#include "moduleipa.h"
#include "graph.h"
#include <iostream>
#include <fstream>
namespace maple {

Graph::Graph(ModuleIPA *mipa)
  : mp(mempoolctrler.NewMemPool("module graph mempool")),
    alloc(mp),
    moduleipa(mipa),
    root_node(nullptr),
    num_vertex(0),
    vertex_vec(alloc.Adapter()) {}

void Graph::TopologicalSorting(std::vector<ModuleNode *> &resvec) {
  std::queue<ModuleNode *> qu;
  qu.push(root_node);
  while (!qu.empty()) {
    ModuleNode *node = qu.front();
    qu.pop();
    resvec.push_back(node);
    for (uint32 i = 0; i < node->kid_nodes.size(); i++) {
      ModuleNode *kidnode = node->kid_nodes[i];
      uint32 innums = kidnode->in_nums--;
      if (innums == 1) {
        qu.push(kidnode);
      }
    }
  }
}

void Graph::DumpMakefile() {
  // output the dependence of the graph to  dependence.txt
  // breadth first search
  if (!root_node) {
    return;
  }
  std::vector<bool> visited(vertex_vec.size(), false);
  std::queue<ModuleNode *> qu;
  std::ofstream outfile("dependence.txt");
  qu.push(root_node);
  while (!qu.empty()) {
    ModuleNode *node = qu.front();
    qu.pop();
    for (uint32 i = 0; i < node->kid_nodes.size(); i++) {
      ModuleNode *kidnode = node->kid_nodes[i];
      if (!visited[kidnode->id]) {
        qu.push(kidnode);
      }
    }
    if (visited[node->id]) {
      continue;
    }
    visited[node->id] = true;
    outfile << node->name;
    if (node->kid_nodes.size() > 0) {
      outfile << ":";
      for (uint32 i = 0; i < node->kid_nodes.size(); i++) {
        outfile << node->kid_nodes[i]->name << " ";
      }
    }
    outfile << "\n";
  }
  outfile.close();
}

void Graph::ReverseDump(std::vector<ModuleNode *> &resvec) {
  for (uint32 i = resvec.size(); i > 0; i--) {
    ModuleNode *node = resvec[i - 1];
    LogInfo::MapleLogger() << node->name << "\n";
  }
}

ModuleNode *Graph::BuildModuleNode(const std::string &name) {
  ModuleNode *node = moduleipa->GetModuleNodeFromName(name);
  if (node) {
    return node;
  }
  node = mp->New<ModuleNode>(&alloc, name, num_vertex++);
  vertex_vec.push_back(node);
  moduleipa->AddModuleNode(name, node);
  return node;
}

void ModuleNode::AddEdge(ModuleNode *kidnode) {
  this->kid_nodes.push_back(kidnode);
  kidnode->in_nums++;
}

}  // namespace maple
