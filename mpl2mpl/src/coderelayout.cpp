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

#include <algorithm>
#include <fstream>
#include <iostream>
#include "coderelayout.h"

// This phase layout the function according the profile.
// First parse the profile,find the corresponding file's
// function type info,the profile file give three function type
//  1.boot-only-hot  function which is only hot in phone boot phase
//  2.both-hot       function which is hot in phone run phase and phone boot phase
//  3.run-hot        function which is only hot in phone run phase
// Every functon have just one layout type,layout the function
// together according there layout type.Currently the layout is  below
//
// [BootHot]
// [BothHot]
// [RunHot]
// [StartupOnly]
// [UsedOnce]
// [Executed] function excuted in some condition
// [Unused]

namespace maple {

CodeReLayout::CodeReLayout(MIRModule *mod, KlassHierarchy *kh, bool dump) : FuncOptimizeImpl(mod, kh, dump) {
  if (!Options::profileData.empty()) {
    std::size_t pos;
    if ((pos = Options::profileData.find(':')) != std::string::npos) {
      Options::profileFuncData = Options::profileData.substr(0, pos);
      Options::profileClassData = Options::profileData.substr(pos + 1);
    }
    Options::profileFuncData = Options::profileData;
    LogInfo::MapleLogger() << "func profile " << Options::profileFuncData << " class profile " << Options::profileClassData << endl;
  }
  if (Options::profileStaticfields) {
    std::string staticFieldsFile = StaticFieldFilename(module->fileName);
    LogInfo::MapleLogger() << staticFieldsFile << std::endl;
    std::ofstream static_fields;
    static_fields.open(staticFieldsFile, ofstream::trunc);
    if (!static_fields.is_open()) {
      ERR(kLncErr, " %s open failed!", staticFieldsFile.c_str());
    }
    static_fields.close();
  }
}

CallNode *CodeReLayout::CreateRecordFieldStaticCall(BaseNode *node, const std::string &name) {
  MIRFunction *callee = builder->GetOrCreateFunction("MCC_RecordStaticField", (TyIdx)PTY_void);
  MapleVector<BaseNode *> funcArgs(builder->GetCurrentFuncCodeMpAllocator()->Adapter());
  BaseNode *nameAddr = builder->CreateExprAddrof(0, GetorCreateStaticFieldSym(name));
  funcArgs.push_back(node);
  funcArgs.push_back(nameAddr);
  return builder->CreateStmtCall(callee->puIdx, funcArgs);
}

std::string CodeReLayout::StaticFieldFilename(const std::string &mplfile) {
  size_t pos = mplfile.rfind(".mpl");
  CHECK_FATAL(pos != string::npos && pos == mplfile.length() - 4, "Not compiling .mpl file?");
  string smryfilename = mplfile.substr(0, pos) + ".staticFields";
  return smryfilename;
}

void CodeReLayout::AddStaticFieldRecord() {
  StmtNode *stmt = currFunc->body->GetFirst();
  StmtNode *next = nullptr;
  while (stmt) {
    next = stmt->GetNext();
    FindDreadRecur(stmt, stmt);
    if (stmt->op == OP_dassign) {
      MIRSymbol *mirsym = currFunc->GetLocalOrGlobalSymbol(static_cast<DassignNode *>(stmt)->stIdx);
      if (mirsym->IsStatic()) {
        BaseNode *node = builder->CreateExprAddrof((static_cast<DassignNode *>(stmt))->fieldID,
                                                      (static_cast<DassignNode *>(stmt))->stIdx);
        CallNode *call = CreateRecordFieldStaticCall(node, mirsym->GetName());
        currFunc->body->InsertBefore(stmt, call);
        // static_fields << mirsym->GetName() << '\n';
      }
    }
    stmt = next;
  }
}

void CodeReLayout::FindDreadRecur(StmtNode *stmt, BaseNode *node) {
  if (!node) {
    return;
  }

  BaseNode *uOpnd = nullptr;
  BinaryOpnds *bopnds = nullptr;
  switch (node->op) {
    case OP_dread:
    case OP_addrof: {
      return InsertProfileBeforeDread(stmt, node);
    }
    case OP_array:
    case OP_intrinsicop:
    case OP_intrinsicopwithtype:
    case OP_call:
    case OP_callassigned:
    case OP_icall:
    case OP_icallassigned:
    case OP_intrinsiccall:
    case OP_intrinsiccallwithtype:
    case OP_return:
    case OP_switch:
    case OP_dassign:
    case OP_iassign: {
      for (int i = 0; i < node->NumOpnds(); i++) {
        FindDreadRecur(stmt, node->Opnd(i));
      }
      break;
    }
    default: {
      if (node->IsUnaryNode()) {
        UnaryNode *unode = static_cast<UnaryNode *>(node);
        FindDreadRecur(stmt, unode->uOpnd);
      } else if (node->IsBinaryNode()) {
        BinaryNode *bnode = static_cast<BinaryNode *>(node);
        bopnds = static_cast<BinaryOpnds *>(bnode);
        FindDreadRecur(stmt, bopnds->bOpnd[0]);
        FindDreadRecur(stmt, bopnds->bOpnd[1]);
      } else {
        break;
      }
    }
  }
}

template <typename Out>
void CodeReLayout::Split(const std::string &s, char delim, Out result) {
  std::stringstream ss;
  ss.str(s);
  std::string item;
  while (std::getline(ss, item, delim)) {
    *(result++) = item;
  }
}

void CodeReLayout::InsertProfileBeforeDread(StmtNode *stmt, BaseNode *opnd) {
  if (!opnd || (opnd->op != OP_dread && opnd->op != OP_addrof)) {
    return;
  }

  DreadNode *dreadNode = static_cast<DreadNode *>(opnd);
  MIRSymbol *mirsym = currFunc->GetLocalOrGlobalSymbol(dreadNode->stIdx);
  if (mirsym->IsStatic()) {
    BaseNode *node = opnd;
    if (opnd->op == OP_dread) {
      node = builder->CreateExprAddrof(dreadNode->fieldID, dreadNode->stIdx);
    }
    CallNode *call = CreateRecordFieldStaticCall(node, mirsym->GetName());
    currFunc->body->InsertBefore(stmt, call);
  }
}

void CodeReLayout::ProcessFunc(MIRFunction *func) {
  if (func->IsEmpty()) {
    return;
  }
  SetCurrentFunction(func);
  if (Options::profileStaticfields) {
    AddStaticFieldRecord();
  }
  if (func->IsClinit()) {
    func->layoutType = kLayoutUsedOnce;
  }
}

void CodeReLayout::Finish() {
  if (!Options::profileFuncData.empty()) {
    std::ifstream funcProfileFile(Options::profileFuncData);
    if (!funcProfileFile.is_open()) {
      ERR(kLncErr, " %s open failed!", Options::profileFuncData.c_str());
    } else {
      std::string funcName;
      std::string tag;
      std::string line;
      bool exeFuncStarted = false;
      uint32 javaNameIdx = module->GetFileinfo(GlobalTables::GetStrTable().GetOrCreateStrIdxFromName("INFO_filename"));
      const std::string &javaName = GlobalTables::GetStrTable().GetStringFromStrIdx(GStrIdx(javaNameIdx));
      std::size_t pos = 0;
      std::size_t posEnd = 0;
      std::string profileSoName;
      bool profileFound = false;
      std::size_t profileCount = 0;
      std::size_t profileNum = 0;
      std::size_t callTimes = 0;
      if (trace) {
        LogInfo::MapleLogger() << "java name  " << javaName << std::endl;
      }
      while (getline(funcProfileFile, line)) {
        if (!Options::quiet) {
          LogInfo::MapleLogger() << "sortString " << line << std::endl;
        }
        if (!profileFound) {
          if (line.length() && line[0] == '#') {
            if (line.find(kProfileStartTag) != std::string::npos) {
              // format:#profile_start:HwSystemServer:72
              pos = line.find_last_of(':');
              size_t tagLength = kProfileStartTag.length();
              profileSoName = line.substr(tagLength + 1, pos - (tagLength + 1));
              profileNum = atoi(line.substr(pos + 1).c_str());
              if (trace) {
                LogInfo::MapleLogger() << "profile so name  " << profileSoName << " count " << profileNum << std::endl;
              }
              if (javaName.find(profileSoName) != std::string::npos) {
                if (trace) {
                  LogInfo::MapleLogger() << "find profile info for " << profileSoName << std::endl;
                }
                profileFound = true;
              }
            } else if (line.find(kProfileSummaryTag) != std::string::npos) {
              // #profile_summary:[so1_name]:[so2_name]
              bool haveSoProfile = false;
              std::vector<std::string> soNames;
              std::string soNamesStr = line.substr(kProfileSummaryTag.length() + 1);
              Split(soNamesStr, ':', std::back_inserter(soNames));
              for (auto &str : soNames) {
                if (trace) {
                  LogInfo::MapleLogger() << str << std::endl;
                }
                if (javaName.find(str) != std::string::npos) {
                  haveSoProfile = true;
                }
              }
              if (!haveSoProfile) {
                if (trace) {
                  LogInfo::MapleLogger() << "profile doesn't have " << javaName << " profile info" << std::endl;
                }
                break;
              }
            } else {
              continue;
            }
          }
        } else {
          // funcname:tag
          pos = line.find_first_of(':');
          funcName = line.substr(0, pos);
          posEnd = line.find_last_of(':');
          tag = line.substr(pos + 1, posEnd - pos - 1);
          callTimes = atoi(line.substr(posEnd + 1).c_str());
          if (funcName == kExeFuncTag) {
            exeFuncStarted = true;
            continue;
          }
          MIRFunction *sortFunction = builder->GetFunctionFromName(funcName);
          if (sortFunction && sortFunction->body) {
            sortFunction->callTimes = callTimes;
            if (sortFunction->IsClinit()) {
              sortFunction->layoutType = kLayoutStartupOnly;
            } else {
              if (exeFuncStarted) {
                sortFunction->layoutType = kLayoutExecuted;
              } else {
                if (tag == "boot-only-hot") {
                  sortFunction->layoutType = kLayoutBootHot;
                } else if (tag == "both-hot") {
                  sortFunction->layoutType = kLayoutBothHot;
                } else if (tag == "run-hot") {
                  sortFunction->layoutType = kLayoutRunHot;
                }
              }
            }
          }
          profileCount++;
          if (profileCount == profileNum) {
            break;
          }
        }
      }
      for (auto &function : module->functionList) {
        ++layout_count[static_cast<size_t>(function->layoutType)];
      }
      if (trace) {
        for (uint32_t i = 0; i < static_cast<uint32_t>(LayoutType::kLayoutTypeCount); ++i) {
          LogInfo::MapleLogger() << "function in category\t" << i << "\tcount=" << layout_count[i] << std::endl;
        }
      }
      std::stable_sort(module->functionList.begin(), module->functionList.end(),
                       [](const MIRFunction *a, const MIRFunction *b) { return a->layoutType < b->layoutType; });
      uint32_t last = 0;
      for (uint32_t i = 0; i <= static_cast<uint32_t>(LayoutType::kLayoutRunHot); i++) {
        if (trace) {
          LogInfo::MapleLogger() << "last\t" << last << "\tcount\t" << layout_count[i] << std::endl;
        }
        std::stable_sort(module->functionList.begin() + last,
                         module->functionList.begin() + last + layout_count[i],
                         [](const MIRFunction *a, const MIRFunction *b) { return a->callTimes < b->callTimes; });
        last += layout_count[i];
      }
      // Create layoutInfo
      GenLayoutSym();
    }
  }
}

MIRSymbol *CodeReLayout::GenStrSym(const std::string &str) {
  uint32 strtabSize;
  std::string newstr = str + '\0';
  strtabSize = newstr.length();
  MIRArrayType *strtabType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(GlobalTables::GetTypeTable().GetUInt8(), strtabSize);
  std::string strtabName = NameMangler::kStaticFieldNamePrefixStr + str;
  MIRSymbol *staticSym = builder->CreateGlobalDecl(strtabName, strtabType, kScGlobal);
  MIRAggConst *strtabAggconst = module->memPool->New<MIRAggConst>(module, strtabType);
  staticSym->SetStorageClass(kScFstatic);
  for (const char &c : newstr) {
    MIRConst *newconst = module->memPool->New<MIRIntConst>(c, GlobalTables::GetTypeTable().GetUInt8());
    strtabAggconst->constVec.push_back(newconst);
  }
  staticSym->SetConst(strtabAggconst);
  return staticSym;
}

MIRSymbol *CodeReLayout::GetorCreateStaticFieldSym(const std::string &fieldname) {
  auto it = str2sym_map.find(fieldname);
  if (it != str2sym_map.end()) {
    return it->second;
  } else {
    MIRSymbol *sym = GenStrSym(fieldname);
    str2sym_map.insert(std::make_pair(fieldname, sym));
    return sym;
  }
}

void CodeReLayout::GenLayoutSym() {
  uint32 arraySize;
  arraySize = 1;
  MIRArrayType *arrayType =
      GlobalTables::GetTypeTable().GetOrCreateArrayType(GlobalTables::GetTypeTable().GetVoidPtr(), arraySize);
  MIRAggConst *funcLayoutConst = module->memPool->New<MIRAggConst>(module, arrayType);
  uint32 funcIdx = 0;
  MIRConst *fieldconst = nullptr;
  MIRFunction *vmethod = nullptr;
  for (uint32_t i = 0; i < static_cast<uint32_t>(LayoutType::kLayoutTypeCount); ++i) {
    if (funcIdx < module->functionList.size()) {
      vmethod = module->functionList[funcIdx];
    } else {
      std::cerr << "no method for codelayout type " << GetLayoutTypeString(i) << std::endl;
      return;
    }
    while (vmethod->IsAbstract() || vmethod->body == nullptr) {
      // find the function not Abstract
      if (trace) {
        LogInfo::MapleLogger() << "encounter valid method " << funcIdx << std::endl;
      }
      funcIdx++;
      if (funcIdx < module->functionList.size()) {
        vmethod = module->functionList[funcIdx];
      } else {
        std::cerr << "no method for codelayout" << GetLayoutTypeString(i) << std::endl;
        return;
      }
    }
    if (trace) {
      LogInfo::MapleLogger() << "Start of category " << i << " in funcIdx " << funcIdx << " " << vmethod->GetName() << std::endl;
    }
    AddroffuncNode *addroffuncExpr = builder->CreateExprAddroffunc(vmethod->puIdx, module->memPool);
    fieldconst = module->memPool->New<MIRAddroffuncConst>(addroffuncExpr->puIdx, GlobalTables::GetTypeTable().GetVoidPtr());
    funcLayoutConst->constVec.push_back(fieldconst);
    funcIdx += layout_count[i];
  }
  // funcLayoutConst->GetConstVec().push_back(entryConst);
  std::string funcLayoutSymName = NameMangler::kFunctionLayoutStr + module->GetFileNameAsPostfix();
  MIRSymbol *funcLayoutSym = builder->CreateGlobalDecl(funcLayoutSymName, arrayType, kScGlobal);
  funcLayoutSym->SetConst(funcLayoutConst);
  funcLayoutSym->SetStorageClass(kScFstatic);
}

}  // namespace maple
