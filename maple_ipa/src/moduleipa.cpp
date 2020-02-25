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

#include "moduleipa.h"
#include "mpl_logging.h"
#include <string>
#include "graph.h"
#include <iostream>
#include <cstring>

namespace maple {
void ModuleIPA::BuildTestCase() {
  std::string stra = "A.mpl";
  std::string strb = "B.mpl";
  std::string strc = "C.mpl";
  std::string strlibcore = "/home/lym/emui_workspace/maple/out/common/share/libcore-all.mpl";

  module_graph = mempool->New<Graph>(this);

  ModuleNode *nodea = module_graph->BuildModuleNode(stra);
  module_graph->SetRootNode(nodea);
  ModuleNode *nodeb = module_graph->BuildModuleNode(strb);
  ModuleNode *nodec = module_graph->BuildModuleNode(strc);
  ModuleNode *nodelibcore = module_graph->BuildModuleNode(strlibcore);

  nodea->AddEdge(nodeb);
  nodea->AddEdge(nodec);
  nodea->AddEdge(nodelibcore);
  nodeb->AddEdge(nodec);
  nodeb->AddEdge(nodelibcore);
  nodec->AddEdge(nodelibcore);
}

ModuleNode *ModuleIPA::ParseMplFileNameNode(uint32 &curindex, const char *strline, uint32 length) {
  CHECK_FATAL(curindex < length, "");
  if (strline[curindex++] != '"') {
    std::cerr << "expect \" for head mpl\n";
    exit(-1);
  }
  char headfile[256];
  uint32 hdi = 0;
  while (curindex < length && strline[curindex] != '"') {
    CHECK_FATAL(hdi < 256, "array overflow");
    headfile[hdi++] = strline[curindex++];
  }
  if (strline[curindex++] != '"') {
    std::cerr << "missing \" for head mpl\n";
    exit(-1);
  }
  headfile[hdi] = '\0';
  ModuleNode *headnode = module_graph->BuildModuleNode(std::string(headfile));
  return headnode;
}

void ModuleIPA::ParseLine(const char *strline) {
  uint32 length = strlen(strline);
  if (length >= 1024) {
    CHECK_FATAL(false, "unexpected length");
  }
  uint32 curindex = 0;
  if (!module_graph) {
    module_graph = mempool->New<Graph>(this);
  }
  ModuleNode *headnode = ParseMplFileNameNode(curindex, strline, length);
  if (!module_graph->root_node) {
    module_graph->SetRootNode(headnode);
  }
  ParseSkipBlank(curindex, strline, length);

  if (strline[curindex] == '\n' || curindex == length) {
    // only head hit, return it
    return;
  } else {
    CHECK_FATAL(curindex < length, "curindex >= length");
    if (strline[curindex++] == '{') {
      // parse sub graph of head mpl file
      while (curindex < length && strline[curindex] != '"') {
        curindex++;
      }
      while (curindex < length) {
        ModuleNode *subnode = ParseMplFileNameNode(curindex, strline, length);
        headnode->AddEdge(subnode);
        // expect '}' in the end
        if (curindex >= length) {
          std::cerr << "ipa module parsing sub node error\n";
          exit(-1);
        }
        if (strline[curindex] == '}') {
          // end parse the line
          return;
        }
        if (strline[curindex] == ',') {
          // continue parse next sub node
          // get rid of blank;
          curindex++;
          ParseSkipBlank(curindex, strline, length);
          if (curindex == length || strline[curindex] != '"') {
            // expect ""
            std::cerr << "ipa module parsing sub node error\n";
            exit(-1);
          }
        } else {
          std::cerr << "expect } or , after a sub graph";
        }
      }
    } else {
      std::cerr << "ipa module parsing sub node error expect { to begin with\n";
      exit(-1);
    }
  }
}

void ModuleIPA::ParseIpaGraph(const char *filename) {
  FILE *graphfilefp = fopen(filename, "r");
  if (graphfilefp == nullptr) {
    std::cerr << "couldn't open graph file" << filename << "\n";
    exit(-1);
  }
  char strline[1024];
  while (!feof(graphfilefp)) {
    if (fgets(strline, 1024, graphfilefp) != nullptr) {
      ParseLine(strline);
    }
  }
  fclose(graphfilefp);
}

void ModuleIPA::BuildModuleGraph(const char *filename) {
  ParseIpaGraph(filename);
  std::vector<ModuleNode *> topsortvec;
  module_graph->TopologicalSorting(topsortvec);
  module_graph->DumpMakefile();
  module_graph->ReverseDump(topsortvec);
  mempoolctrler.DeleteMemPool(module_graph->mp);
}

}  // namespace maple
