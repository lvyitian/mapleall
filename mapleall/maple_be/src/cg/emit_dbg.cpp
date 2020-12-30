/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
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

#include "emit.h"
#include "name_mangler.h"
#include "dbg.h"
#include "cg_assert.h"
#include "be_common.h"  // SIZEOFPTR
#include "global_tables.h"
#include "mir_builder.h"
#include "mir_symbol.h"

#if TARGRISCV64
#define CMNT "\t# "
#else
#define CMNT "\t// "
#endif

namespace maple {
const char *get_DW_TAG_name(unsigned n);
const char *get_DW_FORM_name(unsigned n);
const char *get_DW_AT_name(unsigned n);
}  // namespace maple

namespace maplebe {

using namespace maple;

// extern bool do_pie;

using namespace cfi;

DBGDie *LFindDieWithName(DBGDie *die, dw_tag tag, GStrIdx key) {
  if (die->tag_ == tag) {
    for (auto a : die->attrvec_) {
      if (a->dwattr_ == DW_AT_name) {
        if (a->dwform_ == DW_FORM_string && a->val_.id == key.GetIdx()) {
          return die;
        } else {
          break;
        }
      }
    }
  }

  for (auto c : die->subdievec_) {
    DBGDie *res = LFindDieWithName(c, tag, key);
    if (res) {
      return res;
    }
  }
  return nullptr;
}

DBGDie *LFindChildDieWithName(DBGDie *die, dw_tag tag, GStrIdx key) {
  for (DBGDie *c : die->subdievec_) {
    if (c->tag_ == tag) {
      for (DBGDieAttr *a : c->attrvec_) {
        if (a->dwattr_ == DW_AT_name) {
          if ((a->dwform_ == DW_FORM_string || a->dwform_ == DW_FORM_strp) && a->val_.id == key.GetIdx()) {
            return c;
          } else {
            break;
          }
        }
      }
    }
  }
  return nullptr;
}

DBGDieAttr *LFindDieAttr(DBGDie *die, dw_at attrname) {
  for (DBGDieAttr *attr : die->attrvec_) {
    if (attr->dwattr_ == attrname) {
      return attr;
    }
  }
  return nullptr;
}

void LReplaceFORMValueInAbbrevEntry(DBGAbbrevEntry *diae, dw_at attrName, dw_form formValue) {
  MapleVector<uint32> &attrpairs = diae->attrpairs_;  // kDwAt kDwForm pairs
  for (int i = 0; i < attrpairs.size(); i += 2) {
    if (attrpairs[i] == attrName) {
      // If the following CG_ASSERTion ever fires off, instead of replacing
      // DW_FOMR value in place, we should clone the abbrev entry, make change to
      // to the cloned one, and replace the abbrev entry in the referring DIE.
      CG_ASSERT(attrpairs[i + 1] == DW_FORM_data1 || (attrpairs[i + 1] == formValue),
                "Conflicting change? Read the above comment.");
      attrpairs[i + 1] = formValue;
      return;
    }
  }
}

static void LUpdateAttrValue(DBGDieAttr *attr, int64_t newval, DBGAbbrevEntry *diae) {
  attr->val_.i = int32_t(newval);
  return;
}

void CGFunc::AddDIESymbolLocation(const MIRSymbol *sym, SymbolAlloc *loc) {
  CG_ASSERT(dbginfo, "");
  DBGDie *sdie = dbginfo->GetLocalDie(func, sym->GetNameStridx());
  if (!sdie) {
    return;
  }
  CG_ASSERT(sdie, "");

  DBGExprLoc *exprloc = sdie->GetExprLoc();
  CHECK_FATAL(exprloc != nullptr, "exprloc is null in CGFunc::AddDIESymbolLocation");
  exprloc->symloc_ = loc;

  dbg_callframe_locations.push_back(exprloc);
}

#define XSTR(s) str(s)
#define str(s) #s

void Emitter::EmitDIHeader() {
  Emit(".L" XSTR(TEXT_BEGIN) ":\n");
}

void Emitter::EmitDIFooter() {
  Emit("\t.text\n");
  Emit(".L" XSTR(TEXT_END) ":\n");
}

void Emitter::EmitDIHeaderFileInfo() {
  Emit(CMNT "dummy header file 1\n");
  Emit(CMNT "dummy header file 2\n");
  Emit(CMNT "dummy header file 3\n");
}

void Emitter::AddLabelDieToLabelIdxMapping(DBGDie *lbldie, LabelIdx lblidx) {
  labdie2labidx_table.insert(pair<DBGDie *, LabelIdx>(lbldie, lblidx));
}

LabelIdx Emitter::GetLabelIdxForLabelDie(DBGDie *lbldie) {
  auto it = labdie2labidx_table.find(lbldie);
  CHECK_FATAL(it != labdie2labidx_table.end(), "");
  return it->second;
}

void Emitter::ApplyInPrefixOrder(DBGDie *die, const std::function<void(DBGDie *)> &func) {
  func(die);
  CG_ASSERT(die, "");
  if (die->subdievec_.size() > 0) {
    for (auto c : die->subdievec_) {
      ApplyInPrefixOrder(c, func);
    }
    // mark the end of the sibling list
    func(nullptr);
  }
}

DBGDieAttr *LFindAttribute(MapleVector<DBGDieAttr *> &vec, dw_at key) {
  for (DBGDieAttr *at : vec)
    if (at->dwattr_ == key) {
      return at;
    }
  return nullptr;
}

DBGAbbrevEntry *LFindAbbrevEntry(MapleVector<DBGAbbrevEntry *> &abbvec, unsigned int key) {
  for (DBGAbbrevEntry *daie : abbvec) {
    if (!daie) {
      continue;
    }
    if (daie->abbrevid_ == key) {
      return daie;
    }
  }
  CG_ASSERT(0, "");
  return nullptr;
}

bool LShouldEmit(unsigned int dwform) {
  switch (dwform) {
    case DW_FORM_flag_present:
      return false;
  }
  return true;
}

void Emitter::EmitDIFormSpecification(unsigned int dwform) {
  switch (dwform) {
    case DW_FORM_string:
      Emit(".string  ");
      break;
    case DW_FORM_strp:
    case DW_FORM_data4:
    case DW_FORM_ref4:
      Emit(".4byte   ");
      break;
    case DW_FORM_data1:
      Emit(".byte    ");
      break;
    case DW_FORM_data2:
      Emit(".2byte   ");
      break;
    case DW_FORM_data8:
      Emit(".8byte   ");
      break;
    case DW_FORM_sec_offset:
      // if DWARF64, should be .8byte?
      Emit(".4byte   ");
      break;
    case DW_FORM_addr: /* Should we use DWARF64? for now, we generate .8byte as gcc does for DW_FORM_addr */
      Emit(".8byte   ");
      break;
    case DW_FORM_exprloc:
      Emit(".uleb128 ");
      break;
    default:
      CHECK_FATAL(maple::get_DW_FORM_name(dwform) != nullptr,
             "get_DW_FORM_name return null in Emitter::EmitDIFormSpecification");
      LogInfo::MapleLogger() << "unhandled : " << maple::get_DW_FORM_name(dwform) << endl;
      CG_ASSERT(0, "NYI");
  }
}

void Emitter::EmitDIAttrValue(DBGDie *die, DBGDieAttr *attr, dw_at attrName, dw_tag tagName, DebugInfo *di) {
  MapleVector<DBGDieAttr *> &attrvec = die->attrvec_;

  switch (attr->dwattr_) {
    case DW_AT_decl_file:
      EmitHexUnsigned(1);  // file num, 1 for debugging .mpl
      return;
    default:
      break;
  }

  switch (attr->dwform_) {
    case DW_FORM_string: {
      const string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(attr->val_.id);
      Emit("\"").Emit(name).Emit("\"");
      Emit(CMNT "len = ");
      EmitDecUnsigned(name.length() + 1);
    } break;
    case DW_FORM_strp:
      Emit(".L" XSTR(DEBUG_STR_LABEL));
      out << attr->val_.id;
      break;
    case DW_FORM_data1:
#if DEBUG
      if (attr->val_.i == kDbgDefaultVal) {
        EmitHexUnsigned(attr->val_.i);
      } else
#endif
        EmitHexUnsigned(uint8_t(attr->val_.i));
      break;
    case DW_FORM_data2:
#if DEBUG
      if (attr->val_.i == kDbgDefaultVal) {
        EmitHexUnsigned(attr->val_.i);
      } else
#endif
        EmitHexUnsigned(uint16_t(attr->val_.i));
      break;
    case DW_FORM_data4:
#if DEBUG
      if (attr->val_.i == kDbgDefaultVal) {
        EmitHexUnsigned(attr->val_.i);
      } else
#endif
        EmitHexUnsigned(uint32_t(attr->val_.i));
      break;
    case DW_FORM_data8:
      if (attrName == DW_AT_high_pc) {
        if (tagName == DW_TAG_compile_unit) {
          Emit(".L" XSTR(TEXT_END) "-.L" XSTR(TEXT_BEGIN));
        } else if (tagName == DW_TAG_subprogram) {
          DBGDieAttr *name = LFindAttribute(attrvec, DW_AT_name);
          if (name == nullptr) {
            DBGDieAttr *spec = LFindAttribute(attrvec, DW_AT_specification);
            CHECK_FATAL(spec != nullptr, "spec is null in Emitter::EmitDIAttrValue");
            DBGDie *decl = di->GetDie(spec->val_.id);
            name = LFindAttribute(decl->attrvec_, DW_AT_name);
          }
          CHECK_FATAL(name != nullptr, "name is null in Emitter::EmitDIAttrValue");
          const string &str = GlobalTables::GetStrTable().GetStringFromStrIdx(name->val_.id);

          MIRBuilder *mirbuilder = cg_->mirModule->mirBuilder;
          MIRFunction *mfunc = mirbuilder->GetFunctionFromName(str);
          MapleMap<MIRFunction*, pair<LabelIdx,LabelIdx> >::iterator it =
            CG::funcWrapLabels.find(mfunc);
          if (it != CG::funcWrapLabels.end()) {
            EmitLabelForFunc(mfunc, (*it).second.second);  // end label
          } else {
            EmitLabelRef(str.c_str(), attr->val_.id);      // maybe deadbeef
          }
          Emit("-");
          if (it != CG::funcWrapLabels.end()) {
            EmitLabelForFunc(mfunc, (*it).second.first);  // start label
          } else {
            DBGDieAttr *lowpc = LFindAttribute(attrvec, DW_AT_low_pc);
            CHECK_FATAL(lowpc != nullptr, "lowpc is null in Emitter::EmitDIAttrValue");
            EmitLabelRef(str.c_str(), lowpc->val_.id);    // maybe deadbeef
          }
        }
      } else {
        EmitHexUnsigned(attr->val_.i);
      }
      break;
    case DW_FORM_sec_offset:
      if (attrName == DW_AT_stmt_list) {
        Emit(".L");
        Emit(XSTR(DEBUG_LINE_0));
      }
      break;
    case DW_FORM_addr:
      if (attrName == DW_AT_low_pc) {
        if (tagName == DW_TAG_compile_unit) {
          Emit(".L" XSTR(TEXT_BEGIN));
        } else if (tagName == DW_TAG_subprogram) {
          // if decl, name should be found; if def, we try DW_AT_specification
          DBGDieAttr *name = LFindAttribute(attrvec, DW_AT_name);
          if (name == nullptr) {
            DBGDieAttr *spec = LFindAttribute(attrvec, DW_AT_specification);
            CHECK_FATAL(spec != nullptr, "spec is null in Emitter::EmitDIAttrValue");
            DBGDie *decl = di->GetDie(spec->val_.id);
            name = LFindAttribute(decl->attrvec_, DW_AT_name);
          }
          CHECK_FATAL(name != nullptr, "name is null in Emitter::EmitDIAttrValue");
          const string &str = GlobalTables::GetStrTable().GetStringFromStrIdx(name->val_.id);
          MIRBuilder *mirbuilder = cg_->mirModule->mirBuilder;
          MIRFunction *mfunc = mirbuilder->GetFunctionFromName(str);
          MapleMap<MIRFunction*, pair<LabelIdx,LabelIdx> >::iterator
          it = CG::funcWrapLabels.find(mfunc);
          if (it != CG::funcWrapLabels.end()) {
            EmitLabelForFunc(mfunc, (*it).second.first);  // it is a <pair>
          } else {
            EmitLabelRef(str.c_str(), attr->val_.id);     // maybe deadbeef
          }
        } else if (tagName == DW_TAG_label) {
          LabelIdx labelIdx = GetLabelIdxForLabelDie(die);
          DBGDie *subpgm = die->parent;
          CG_ASSERT(subpgm->tag_ == DW_TAG_subprogram, "Label DIE should be a child of a Subprogram DIE");
          DBGDieAttr *fnameAttr = LFindAttribute(subpgm->attrvec_, DW_AT_name);
          if (!fnameAttr) {
            DBGDieAttr *specAttr = LFindAttribute(subpgm->attrvec_, DW_AT_specification);
            CHECK_FATAL(specAttr, "pointer is null");
            DBGDie *twin = di->GetDie(specAttr->val_.u);
            fnameAttr = LFindAttribute(twin->attrvec_, DW_AT_name);
          }
          CHECK_FATAL(fnameAttr, "");
          const string &fnameStr = GlobalTables::GetStrTable().GetStringFromStrIdx(fnameAttr->val_.id);
          LabelOperand *res = memPool->New<LabelOperand>(fnameStr.c_str(), labelIdx);
          res->Emit(*this, nullptr);
        }
      } else if (attrName == DW_AT_high_pc) {
        if (tagName == DW_TAG_compile_unit) {
          Emit(".L" XSTR(TEXT_END) "-.L" XSTR(TEXT_BEGIN));
        }
      } else {
        Emit("XXX--ADDR--XXX");
      }
      break;
    case DW_FORM_ref4:
      if (attrName == DW_AT_type) {
        DBGDie *die = di->GetDie(attr->val_.u);
        if (die->Offset) {
          EmitHexUnsigned(die->Offset);
        } else {
          // unknown type, missing mplt
          EmitHexUnsigned(di->dummytypedie_->Offset);
          Emit(CMNT "Warning: dummy type used");
        }
      } else if (attrName == DW_AT_specification || attrName == DW_AT_sibling) {
        DBGDie *die = di->GetDie(attr->val_.u);
        CG_ASSERT(die->Offset, "");
        EmitHexUnsigned(die->Offset);
      } else if (attrName == DW_AT_object_pointer) {
        GStrIdx thisIdx = GlobalTables::GetStrTable().GetStrIdxFromName(DEBUG_MAPLE_THIS);
        DBGDie *that = LFindChildDieWithName(die, DW_TAG_formal_parameter, thisIdx);
        // need to find the this or self based on the source language
        // what is the name for 'this' used in mapleir?
        // this has to be with respect to a function
        if (that) {
          EmitHexUnsigned(that->Offset);
        } else {
          EmitHexUnsigned(attr->val_.u);
        }
      } else {
        Emit(" OFFSET ");
        EmitHexUnsigned(attr->val_.u);
      }
      break;
    case DW_FORM_exprloc: {
      DBGExprLoc *elp = attr->val_.ptr;
      switch (elp->GetOp()) {
        case DW_OP_call_frame_cfa:
          EmitHexUnsigned(1);
          Emit("\n\t.byte    ");
          EmitHexUnsigned(elp->GetOp());
          break;
        case DW_OP_addr:
          EmitHexUnsigned(9);
          Emit("\n\t.byte    ");
          EmitHexUnsigned(elp->GetOp());
          Emit("\n\t.8byte   ");
          Emit(GlobalTables::GetStrTable().GetStringFromStrIdx(elp->GetGvarStridx()).c_str());
          break;
        case DW_OP_fbreg:
          EmitHexUnsigned(1 + NameMangler::GetSleb128Size(elp->GetFboffset()));
          Emit("\n\t.byte    ");
          EmitHexUnsigned(elp->GetOp());
          Emit("\n\t.sleb128 ");
          EmitDecSigned(elp->GetFboffset());
          break;
        default:
          EmitHexUnsigned(uintptr_t(elp));
          break;
      }
    } break;
    default:
      CHECK_FATAL(maple::get_DW_FORM_name(attr->dwform_) != nullptr,
             "get_DW_FORM_name return null in Emitter::EmitDIAttrValue");
      LogInfo::MapleLogger() << "unhandled : " << maple::get_DW_FORM_name(attr->dwform_) << endl;
      CG_ASSERT(0, "NYI");
  }
}

void Emitter::EmitDIDebugInfoSection(DebugInfo *mirdi) {
  // From DWARF Standard Specification V4. 7.5.1
  // collect section size
  Emit("\t.section\t.debug_info,\"\",@progbits\n");
  // label to mark start of the .debug_info section
  Emit(".L" XSTR(DEBUG_INFO_0) ":\n");
  // $ 7.5.1.1
  Emit("\t.4byte\t");
  EmitHexUnsigned(mirdi->debug_info_length_);
  Emit(CMNT "section length\n");
  // DWARF version. uhalf.
  Emit("\t.2byte\t0x" XSTR(DWARF_VERSION) "\n");  // 4 for version 4.
  // debug_abbrev_offset. 4byte for 32-bit, 8byte for 64-bit
  Emit("\t.4byte\t.L" XSTR(DEBUG_ABBREV_0) "\n");
  // address size. ubyte
  Emit("\t.byte\t0x" XSTR(SIZEOFPTR) "\n");
  /*
   * 7.5.1.2 type unit header
   * currently empty...
   *
   * 7.5.2 Debugging Information Entry (DIE)
   */
  Emitter *emitter = this;
  MapleVector<DBGAbbrevEntry *> &abbrevVec = mirdi->abbrev_vec_;
  ApplyInPrefixOrder(mirdi->cu_, [&abbrevVec, &emitter, &mirdi](DBGDie *die) {
    if (!die) {
      // emit the null entry and return
      emitter->Emit("\t.byte    0x0\n");
      return;
    }
    bool verbose = emitter->cg_->GenerateVerboseAsm();
    if (verbose) {
      emitter->Emit("\n");
    }
    emitter->Emit("\t.uleb128 ");
    emitter->EmitHexUnsigned(die->abbrevid_);
    if (verbose) {
      emitter->Emit(CMNT);
      CHECK_FATAL(maple::get_DW_TAG_name(die->tag_) != nullptr,
             "get_DW_TAG_name(die->tag_) return null in Emitter::EmitDIDebugInfoSection");
      emitter->Emit(maple::get_DW_TAG_name(die->tag_));
      emitter->Emit(" Offset= ");
      emitter->EmitHexUnsigned(die->Offset);
      emitter->Emit(" (");
      emitter->EmitDecUnsigned(die->Offset);
      emitter->Emit(" ),  Size= ");
      emitter->EmitHexUnsigned(die->Size);
      emitter->Emit(" (");
      emitter->EmitDecUnsigned(die->Size);
      emitter->Emit(" )\n");
    } else {
      emitter->Emit("\n");
    }
    DBGAbbrevEntry *diae = LFindAbbrevEntry(abbrevVec, die->abbrevid_);
    CHECK_FATAL(diae != nullptr, "diae is null in Emitter::EmitDIDebugInfoSection");
    MapleVector<uint32> &apl = diae->attrpairs_;  // attribute pair list

    string sfile, spath;
    if (diae->tag_ == DW_TAG_compile_unit && sfile.empty()) {
      // get full source path from fileMap[2]
      string srcPath = emitter->fileMap[2];
      size_t t = srcPath.rfind("/");
      CG_ASSERT(t != string::npos, "");
      sfile = srcPath.substr(t+1);
      spath = srcPath.substr(0, t-1);
    }

    for (int i = 0; i < diae->attrpairs_.size(); i += 2) {
      DBGDieAttr *attr = LFindAttribute(die->attrvec_, dw_at(apl[i]));
      if (!LShouldEmit(unsigned(apl[i + 1]))) {
        continue;
      }
      // update DW_AT_name and DW_AT_comp_dir attrs under DW_TAG_compile_unit
      // to be C/C++
      if (attr->dwattr_ == DW_AT_name) {
        attr->val_.id = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(sfile).GetIdx();
      } else if (attr->dwattr_ == DW_AT_comp_dir) {
        attr->val_.id = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(spath).GetIdx();
      }
      emitter->Emit("\t");
      emitter->EmitDIFormSpecification(unsigned(apl[i + 1]));
      emitter->EmitDIAttrValue(die, attr, unsigned(apl[i]), diae->tag_, mirdi);
      if (verbose) {
        emitter->Emit(CMNT);
        emitter->Emit(maple::get_DW_AT_name(unsigned(apl[i])));
        emitter->Emit(" : ");
        emitter->Emit(maple::get_DW_FORM_name(unsigned(apl[i + 1])));
        if (apl[i + 1] == DW_FORM_strp || apl[i + 1] == DW_FORM_string) {
          emitter->Emit(" : ");
          emitter->Emit(GlobalTables::GetStrTable().GetStringFromStrIdx(attr->val_.id).c_str());
        } else if (apl[i] == DW_AT_data_member_location) {
          emitter->Emit(" : ");
          emitter->Emit(apl[i + 1]).Emit("  attr= ");
          emitter->EmitHexUnsigned(uintptr_t(attr));
        }
      }
      emitter->Emit("\n");
    }
  });
}

void Emitter::EmitDIDebugAbbrevSection(DebugInfo *mirdi) {
  Emit("\t.section\t.debug_abbrev,\"\",@progbits\n");
  Emit(".L" XSTR(DEBUG_ABBREV_0) ":\n");

  // construct a list of DI abbrev entries
  // 1. DW_TAG_compile_unit 0x11
  // 2. DW_TAG_subprogram   0x2e
  bool verbose = cg_->GenerateVerboseAsm();
  for (DBGAbbrevEntry *diae : mirdi->abbrev_vec_) {
    if (!diae) {
      continue;
    }
    // ID
    if (verbose) {
      Emit("\n");
    }
    Emit("\t.uleb128 ");
    EmitHexUnsigned(diae->abbrevid_);
    if (verbose) {
      Emit(CMNT "Abbrev Entry ID");
    }
    Emit("\n");
    // TAG
    Emit("\t.uleb128 ");
    EmitHexUnsigned(diae->tag_);
    CHECK_FATAL(maple::get_DW_TAG_name(diae->tag_) != nullptr,
           "get_DW_TAG_name return null in Emitter::EmitDIDebugAbbrevSection");
    if (verbose) {
      Emit(CMNT);
      Emit(maple::get_DW_TAG_name(diae->tag_));
    }
    Emit("\n");

    MapleVector<uint32> &apl = diae->attrpairs_;  // attribute pair list
    // children?
    Emit("\t.byte    ");
    EmitHexUnsigned(diae->withchildren_);
    if (verbose) {
      Emit(diae->withchildren_ ? CMNT "DW_CHILDREN_yes" : CMNT "DW_CHILDREN_no");
    }
    Emit("\n");

    for (int i = 0; i < diae->attrpairs_.size(); i += 2) {
      // odd entry -- DW_AT_*, even entry -- DW_FORM_*
      Emit("\t.uleb128 ");
      EmitHexUnsigned(apl[i]);
      CHECK_FATAL(maple::get_DW_AT_name(unsigned(apl[i])) != nullptr,
             "get_DW_AT_name return null in Emitter::EmitDIDebugAbbrevSection");
      if (verbose) {
        Emit(CMNT);
        Emit(maple::get_DW_AT_name(unsigned(apl[i])));
      }
      Emit("\n");
      Emit("\t.uleb128 ");
      EmitHexUnsigned(apl[i + 1]);
      CHECK_FATAL(maple::get_DW_FORM_name(unsigned(apl[i + 1])) != nullptr,
             "get_DW_FORM_name return null in Emitter::EmitDIDebugAbbrevSection");
      if (verbose) {
        Emit(CMNT);
        Emit(maple::get_DW_FORM_name(unsigned(apl[i + 1])));
      }
      Emit("\n");
    }
    // end of an abbreviation record
    Emit("\t.byte    0x0\n");
    Emit("\t.byte    0x0\n");
  }
  Emit("\t.byte    0x0\n");
}

void Emitter::EmitDIDebugARangesSection() {
  Emit("\t.section\t.debug_aranges,\"\",@progbits\n");
}

void Emitter::EmitDIDebugRangesSection() {
  Emit("\t.section\t.debug_ranges,\"\",@progbits\n");
}

void Emitter::EmitDIDebugLineSection() {
  Emit("\t.section\t.debug_line,\"\",@progbits\n");
  Emit(".L" XSTR(DEBUG_LINE_0) ":\n");
}

void Emitter::EmitDIDebugStrSection() {
  Emit("\t.section\t.debug_str,\"MS\",@progbits,1\n");
  for (auto it : cg_->mirModule->dbgInfo->strps_) {
    Emit(".L" XSTR(DEBUG_STR_LABEL));
    out << it;
    Emit(":\n");
    const string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(it);
    Emit("\t.string \"").Emit(name).Emit("\"\n");
  }
}

void Emitter::FillInClassByteSize(DBGDie *die, DBGDieAttr *byteSizeAttr, DBGAbbrevEntry *diae) {
  CG_ASSERT(byteSizeAttr->dwform_ == DW_FORM_data1 || byteSizeAttr->dwform_ == DW_FORM_data2 ||
              byteSizeAttr->dwform_ == DW_FORM_data4 || byteSizeAttr->dwform_ == DW_FORM_data8,
            "Unknown FORM value for DW_AT_byte_size");
  if (byteSizeAttr->val_.i == static_cast<uint32>(kDbgDefaultVal)) {
    // get class size
    DBGDieAttr *nameAttr = LFindDieAttr(die, DW_AT_name);
    CHECK_FATAL(nameAttr != nullptr, "name_attr is nullptr in Emitter::FillInClassByteSize");
    TyIdx tyIdx =
      GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(GStrIdx(nameAttr->val_.id));  // hope this is a global string index as it is a type name
    CHECK_FATAL(tyIdx.GetIdx() < g->becommon->type_size_table.size(), "index out of range in Emitter::FillInClassByteSize");
    int64_t byteSize = g->becommon->type_size_table[tyIdx.GetIdx()];
    LUpdateAttrValue(byteSizeAttr, byteSize, diae);
  }
}

void Emitter::SetupDBGInfo(DebugInfo *mirdi) {
  Emitter *emitter = this;
  MapleVector<DBGAbbrevEntry *> &abbrevVec = mirdi->abbrev_vec_;
  ApplyInPrefixOrder(mirdi->cu_, [&abbrevVec, &emitter](DBGDie *die) {
    if (!die) {
      return;
    }

    CHECK_FATAL(maple::get_DW_TAG_name(die->tag_) != nullptr,
           "maple::get_DW_TAG_name(die->tag_) is nullptr in Emitter::SetupDBGInfo");
    if (die->abbrevid_ == 0) {
      LogInfo::MapleLogger() << maple::get_DW_TAG_name(die->tag_) << endl;
    }
    CHECK_FATAL(die->abbrevid_ < abbrevVec.size(), "index out of range in Emitter::SetupDBGInfo");
    CG_ASSERT(abbrevVec[die->abbrevid_]->abbrevid_ == die->abbrevid_, "");
    DBGAbbrevEntry *diae = abbrevVec[die->abbrevid_];
    switch (diae->tag_) {
      case DW_TAG_subprogram: {
        DBGExprLoc *exprloc = emitter->memPool->New<DBGExprLoc>(emitter->cg_->mirModule);
        exprloc->simploc_->dwop_ = DW_OP_call_frame_cfa;
        die->SetAttr(DW_AT_frame_base, exprloc);
      } break;
      case DW_TAG_structure_type:
      case DW_TAG_class_type:
      case DW_TAG_interface_type: {
        DBGDieAttr *byteSizeAttr = LFindDieAttr(die, DW_AT_byte_size);
        if (byteSizeAttr) {
          emitter->FillInClassByteSize(die, byteSizeAttr, diae);
        }
        // get the name
        DBGDieAttr *atName = LFindDieAttr(die, DW_AT_name);
        CHECK_FATAL(atName != nullptr, "at_name is null in Emitter::SetupDBGInfo");
        // get the type from string name
        TyIdx ctyIdx = GlobalTables::GetTypeNameTable().GetTyIdxFromGStrIdx(GStrIdx(atName->val_.id));
        MIRType *mty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ctyIdx);
        MIRStructType *sty = static_cast<MIRStructType *>(mty);
        CHECK_FATAL(sty != nullptr, "pointer cast failed");
        CHECK_FATAL(sty->tyIdx.GetIdx() < g->becommon->struct_fieldcount_table.size(), "");
        int32_t myEnd = g->becommon->struct_fieldcount_table[sty->tyIdx.GetIdx()];
        int32_t myBegin = myEnd - sty->fields.size() + 1;
        for (int i = myBegin; i <= myEnd; i++) {
          int offset = g->becommon->GetFieldOffset(sty, i).first;
          GStrIdx fldName = sty->fields[i - myBegin].first;
          DBGDie *cdie = LFindChildDieWithName(die, DW_TAG_member, fldName);
          CHECK_FATAL(cdie != nullptr, "cdie is null in Emitter::SetupDBGInfo");
          DBGDieAttr *mloc = LFindDieAttr(cdie, DW_AT_data_member_location);
          CHECK_FATAL(mloc != nullptr, "mloc is null in Emitter::SetupDBGInfo");
          DBGAbbrevEntry *childDiae = abbrevVec[cdie->abbrevid_];
          CHECK_FATAL(childDiae != nullptr, "child_diae is null in Emitter::SetupDBGInfo");
          LUpdateAttrValue(mloc, offset, childDiae);
        }
      } break;
      default:
        break;
    }
  });

  // compute DIE sizes and offsets
  mirdi->ComputeSizeAndOffsets();
}

}  // namespace maplebe
