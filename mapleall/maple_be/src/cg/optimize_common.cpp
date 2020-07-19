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
#include <string>
#include <map>
#include "optimize_common.h"
#include "cg_bb.h"
#include "cg.h"
#include "aarch64_insn.h"
#include "aarch64_operand.h"
#include "cg_option.h"
#include "securec.h"
#include "cg_assert.h"

// This file provides common class and function for cfgo and ico.

namespace maplebe {

void Optimizer::Run(const char *funcname, bool checkOnly) {
  // Initialize cfg optimization patterns
  InitOptimizePatterns();

  // For each pattern, search cgfunc for optimization
  for (OptimizationPattern *p : diffPassPatterns) {
    p->Search2Op(checkOnly);
  }
  // Search the cgfunc for multiple possible optimizations in one pass
  if (singlePassPatterns.size() > 0) {
    BB *curbb = cgfunc->firstbb;
    bool flag = false;
    while (curbb) {
      for (OptimizationPattern *p : singlePassPatterns) {
        if (p->Optimize(curbb)) {
          flag = p->IsKeepPosition();
          p->SetKeepPosition(false);
          break;
        }
      }

      if (flag) {
        flag = false;
      } else {
        curbb = curbb->next;
      }
    }
  }

  if (CGOptions::dumpOLog) {
    char post[80];
    errno_t cpyRet = strcpy_s(post, sizeof(post), "post-");
    CG_ASSERT((cpyRet == 0 && "call strcpy_s failed"), "");
    errno_t catRes = strcat_s(post, sizeof(post), name);
    CG_ASSERT((catRes == 0 && "call strcat_s failed "), "");
    OptimzeLogger::GetLogger().Print(funcname);
  }
  OptimzeLogger::GetLogger().ClearLocal();
}

void OptimizationPattern::Search2Op(bool noOptimize) {
  checkOnly = noOptimize;
  BB *curbb = cgfunc->firstbb;
  while (curbb) {
    Optimize(curbb);
    if (keepPosition) {
      keepPosition = false;
    } else {
      curbb = curbb->next;
    }
  }
}

void OptimizationPattern::Log(int bbId) {
  OptimzeLogger::GetLogger().Log(patternname);
  DotGenerator::SetColor(bbId, dotColor);
}

map<int, const char *> DotGenerator::coloringMap;

void DotGenerator::SetColor(int bbId, const char *color) {
  coloringMap[bbId] = color;
}

/*Generate dot file for cfg*/
void DotGenerator::GenerateDot(const char *prefix, const CGFunc *cgfunc, const MIRModule *mod, bool includeEh,
                               regno_t vreg) {
  // if (!WpoOptions::dumpdot) return;
  ofstream cfgfile;
  streambuf *coutbuf = LogInfo::MapleLogger().rdbuf(); /*keep original LogInfo::MapleLogger() buffer*/
  streambuf *buf = cfgfile.rdbuf();
  LogInfo::MapleLogger().rdbuf(buf);
  string filename;
  if (prefix != nullptr) {
    filename.append(prefix);
    filename.append("-");
  }
  filename.append(mod->GetFileName().c_str());
  for (uint32 i = 0; i < filename.length(); i++) {
    if (filename[i] == ';' || filename[i] == '/' || filename[i] == '|') {
      filename[i] = '_';
    }
  }
  filename.append(".dot");
  cfgfile.open(filename.c_str(), ios::trunc);
  cfgfile << "digraph {\n";
  /*dump edge*/
  FOR_ALL_BB(bb, cgfunc) {
    for (auto it = bb->succs.begin(); it != bb->succs.end(); it++) {
      cfgfile << "BB" << bb->id;
      cfgfile << " -> "
              << "BB" << (*it)->id;
      cfgfile << ";\n";
    }
    if (includeEh == true)
      for (auto it = bb->eh_succs.begin(); it != bb->eh_succs.end(); it++) {
        cfgfile << "BB" << bb->id;
        cfgfile << " -> "
                << "BB" << (*it)->id;
        cfgfile << "[color=red]";
        cfgfile << ";\n";
      }
  }

  /*dump instruction in each BB*/
  FOR_ALL_BB(bb, cgfunc) {
    if (vreg != 0) {
      FOR_BB_INSNS(insn, bb) {
        bool found = false;
        for (int32_t i = 0; i < Insn::kMaxOperandNum; i++) {
          Operand *opnd = insn->GetOperand(i);
          if (!opnd) {
            continue;
          }
          if (opnd->IsList()) {
            ListOperand *listopnd = static_cast<ListOperand *>(opnd);
            for (auto op : listopnd->GetOperands()) {
              if (!op->IsRegister()) {
                continue;
              }
              RegOperand *regOpnd = static_cast<RegOperand *>(op);
              if (regOpnd->GetRegisterNumber() == vreg) {
                LogInfo::MapleLogger() << "BB" << insn->bb->id << " [style=filled, fillcolor=red];\n";
                found = true;
                break;
              }
            }
            if (found) {
              break;
            }
          } else if (opnd->IsMemoryAccessOperand()) {
            MemOperand *memopnd = static_cast<MemOperand *>(opnd);
            Operand *base = memopnd->GetBaseRegister();
            Operand *offset = memopnd->GetIndexRegister();
            if (base != nullptr && base->IsRegister()) {
              RegOperand *regOpnd = static_cast<RegOperand *>(base);
              if (regOpnd->GetRegisterNumber() == vreg) {
                LogInfo::MapleLogger() << "BB" << insn->bb->id << " [style=filled, fillcolor=red];\n";
                found = true;
                break;
              }
            }
            if (offset != nullptr && offset->IsRegister()) {
              RegOperand *regOpnd = static_cast<RegOperand *>(offset);
              if (regOpnd->GetRegisterNumber() == vreg) {
                LogInfo::MapleLogger() << "BB" << insn->bb->id << " [style=filled, fillcolor=red];\n";
                found = true;
                break;
              }
            }
          } else {
            if (!opnd->IsRegister()) {
              continue;
            }
            RegOperand *regOpnd = static_cast<RegOperand *>(opnd);
            if (regOpnd->GetRegisterNumber() == vreg) {
              LogInfo::MapleLogger() << "BB" << insn->bb->id << " [style=filled, fillcolor=red];\n";
              found = true;
              break;
            }
          }
        }
        if (found) {
          break;
        }
      }
      continue;
    }
    cfgfile << "BB" << bb->id << "[";
    auto it = coloringMap.find(bb->id);
    if (it != coloringMap.end()) {
      cfgfile << "style=filled,fillcolor=" << it->second << ",";
    }
    if (bb->GetKind() == BB::kBBIf) {
      cfgfile << "shape=diamond,label= \" BB" << bb->id << ":\n";
    } else {
      cfgfile << "shape=box,label= \" BB" << bb->id << ":\n";
    }
    cfgfile << "{ ";

    cfgfile << bb->GetKindName() << "\n";

    cfgfile << "}\"];\n";
  }

  cfgfile << "}\n";
  coloringMap.clear();
  cfgfile.flush();
  cfgfile.close();
  LogInfo::MapleLogger().rdbuf(coutbuf);
}

void OptimzeLogger::Print(const char *funcname) {
  if (localStat.size() > 0) {
    LogInfo::MapleLogger() << funcname << endl;
    for (auto it : localStat) {
      LogInfo::MapleLogger() << "Optimized " << it.first << ":" << it.second << endl;
    }

    ClearLocal();
    LogInfo::MapleLogger() << "Total:" << endl;
    for (auto it : globalStat) {
      LogInfo::MapleLogger() << "Optimized " << it.first << ":" << it.second << endl;
    }
  }
}

void OptimzeLogger::Log(const char *patternname) {
  auto itemInGlobal = globalStat.find(patternname);
  if (itemInGlobal != globalStat.end()) {
    itemInGlobal->second++;
  } else {
    globalStat.insert(pair<const char *, int>(patternname, 1));
  }
  auto itemInLocal = localStat.find(patternname);
  if (itemInLocal != localStat.end()) {
    itemInLocal->second++;
  } else {
    localStat.insert(pair<const char *, int>(patternname, 1));
  }
}

void OptimzeLogger::ClearLocal() {
  localStat.clear();
}

void OptimzeLogger::ClearGlobal() {
  globalStat.clear();
}

}  // namespace maplebe
