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

#include <cstdlib>
#include <iostream>
#include "mir_parser.h"
#include "opcode_info.h"
#include "callgraph.h"
#include "me_cfg.h"
#include "module_phase_manager.h"
#include "option_parser.h"
#include "inline.h"
#include "moduleipa.h"
#include "interleaved_manager.h"
#include "option.h"
#include "simplify.h"
#include "bin_mpl_import.h"

/*
 * Inter procedual analysis
 * 1. function side effect.
 * 2. escape analysis
 * Don't make any modification on mpl. Analysis result will be written into mplt
*/
using namespace std;
using namespace mapleOption;

static bool effectIpa = false;
static bool timePhases = false;

string optName = "inline";
#define MPLIPA_VERSION "mplipa  (CBG Programming Platform TD V100R001C00B824SP01)  1.1.824.1 20181213"

enum OptionIndex { kUnknown, kHelp, kVersion, kOptL1, kOptL2, kEffectIPA, kInlineHint, kQuiet, kTimePhases };
const vector<string> kIpaUsage = {
  "libcore-all.mpl",
  "libcore-all.bpl",
};

const Descriptor kUsage[] = {
  { kUnknown, 0, "", "", kBuildTypeAll, kArgCheckPolicyUnknown,
    "USAGE:  mplipa [options] *.mpl\n\n"
    "OPTIONS:" },
  { kHelp, 0, "h", "help", kBuildTypeAll, kArgCheckPolicyNone, "  -h, --help                        Print usage and exit" },
  { kVersion, 0, "v", "version", kBuildTypeAll, kArgCheckPolicyNone, "  -v, --version                     Print version and exit" },

  { kOptL1, 0, "", "O1", kBuildTypeAll, kArgCheckPolicyNone, "  --O1                              Enable basic inlining" },
  { kOptL2, 0, "", "O2", kBuildTypeAll, kArgCheckPolicyNone, "  --O2                              Enable greedy inlining" },
  { kEffectIPA, 0, "", "effectipa", kBuildTypeAll, kArgCheckPolicyNone,
    "  --effectipa                       Enable method side effect for ipa" },
  { kInlineHint, 0, "", "inlinefunclist", kBuildTypeAll, kArgCheckPolicyRequired,
    "--inlinefunclist=                   Inlining related configuration" },
  { kQuiet, 0, "", "quiet", kBuildTypeAll, kArgCheckPolicyNone, "  --quiet                           Disable out debug info" },
  { kTimePhases, 0, "", "time-phases", kBuildTypeAll, kArgCheckPolicyNone, "  --time-phases                     Timing phases and print percentages" },
  { 0, 0, nullptr, nullptr, kBuildTypeAll, kArgCheckPolicyUnknown, nullptr }
};

void ParseCmdline(int argc, char **argv, vector<string> &fileNames) {
  OptionParser optionParser(kUsage);
  int ret = optionParser.Parse(argc, argv);
  CHECK_FATAL(ret == ErrorCode::kErrorNoError, "option parser error");
  // Default value
  MInline::inlineFuncList = "";
  for (auto opt : optionParser.GetOptions()) {
    switch (opt.Index()) {
      case kHelp:
        if (opt.Args().empty()) {
          optionParser.PrintUsage();
          exit(1);
        }
        break;
      case kOptL1:
        optName = "inline";
        break;
      case kOptL2:
        optName = "greedyinline";
        break;
      case kEffectIPA:
        effectIpa = true;
        break;
      case kQuiet:
        MeOption::quiet = true;
        Options::quiet = true;
        break;
      case kTimePhases:
        timePhases = true;
        break;
      case kVersion:
        LogInfo::MapleLogger() << MPLIPA_VERSION << std::endl;
        break;
      case kInlineHint:
        MInline::inlineFuncList = opt.Args();
        break;
      default:
        ASSERT(false, "unhandled case in ParseCmdline");
    }
  }

  for (int i = 0; i < optionParser.GetNonOptionsCount(); i++) {
    fileNames.push_back(optionParser.GetNonOptions().at(i));
  }
}

void DoIpaSideEffectAnalysis(MIRModule *mirModule) {
  MemPool *optmp = mempoolctrler.NewMemPool("ipa sideeffect mempool");
  std::string ipainput;
  InterleavedManager mgr(optmp, mirModule, ipainput, timePhases);
  vector<string> modphases;
  modphases.push_back(string("classhierarchy"));
  modphases.push_back(string("ipodevirtulize"));
  modphases.push_back(string("callgraph"));
  Simplify::doclinitopt = true;
  modphases.push_back(string("Simplify"));
  MInline::level = 2;
  modphases.push_back(string("inline"));
  mgr.AddPhases(modphases, true, timePhases);
  modphases.clear();

  vector<string> mephases;
  mephases.push_back(string("loopcanon"));
  mephases.push_back(string("splitcriticaledge"));
  mephases.push_back(string("aliasclass"));
  mephases.push_back(string("ssa"));
  mephases.push_back(string("ssadevirt"));
  mephases.push_back(string("emit"));
  mgr.AddPhases(mephases, false, timePhases);
  mephases.clear();

  modphases.push_back(string("callgraph"));
  mgr.AddPhases(modphases, true, timePhases);
  modphases.clear();

  mephases.push_back(string("aliasclass"));
  mephases.push_back(string("ssa"));
  mephases.push_back(string("ssadevirt"));
  mephases.push_back(string("sideeffect"));
  mgr.AddPhases(mephases, false, timePhases);
  modphases.clear();

  modphases.push_back(string("updatemplt"));
  mgr.AddPhases(modphases, true, timePhases);
  mgr.Run();
  mempoolctrler.DeleteMemPool(optmp);
}

int main(int argc, char **argv) {
  vector<string> fileNames;
  ParseCmdline(argc, argv, fileNames);

  MemPool *modulemp = mempoolctrler.NewMemPool("maple modulephase mempool");
  vector<maple::MIRModule *> themodule(argc, nullptr);
  for (maple::uint32 i = 0; i < fileNames.size(); i++) {
    bool found = false;
    themodule[i] = new maple::MIRModule(fileNames[i].c_str());
    for (unsigned int j = 0; j < kIpaUsage.size(); j++) {
      if (themodule[i]->fileName == kIpaUsage[j]) {
        found = true;
        break;
      }
    }
    if (!found) {
      continue;
    }
    std::string::size_type lastdot = fileNames[i].find_last_of(".");
    bool isbpl = fileNames[i].compare(lastdot, 5, ".bpl\0") == 0;
    if (!isbpl) {
      maple::MIRParser theparser(*themodule[i]);
      if (theparser.ParseMIR(0, 0, true, false)) {
        DoIpaSideEffectAnalysis(themodule[i]);
      } else {
        theparser.EmitError(themodule[i]->fileName.c_str());
        return 1;
      }
    } else {
      BinaryMplImport binMplt(*themodule[i]);
      binMplt.imported = false;
      if (binMplt.Import(fileNames[i], true)) {
        DoIpaSideEffectAnalysis(themodule[i]);
      } else {
        FATAL(kLncFatal, "cannot open .bpl file: %s", fileNames[i].c_str());
        return 1;
      }
    }
  }
  mempoolctrler.DeleteMemPool(modulemp);
  return 0;
}
