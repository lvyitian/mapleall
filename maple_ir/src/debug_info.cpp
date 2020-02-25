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

#include "mir_builder.h"
#include "printing.h"
#include "maple_string.h"
#include "name_mangler.h"
#include "debug_info.h"
#include "global_tables.h"
#include "mir_type.h"
#include <cstring>
#include "securec.h"

namespace maple {

// utility functions to get the string from tag value etc.
// get_DW_TAG_name(unsigned n)
#define DW_FIRST_TAG(name, value)           \
  const char *get_DW_TAG_name(unsigned n) { \
    switch (n) {                            \
      DW_TAG(name, value)
#define DW_TAG(name, value) \
  case name:                \
    return #name;
#define DW_END_TAG \
  }                \
  return 0;        \
  }
#define DW_TAG_DUP(name, value)

// get_DW_FORM_name(unsigned n)
#define DW_FIRST_FORM(name, value)           \
  const char *get_DW_FORM_name(unsigned n) { \
    switch (n) {                             \
      DW_FORM(name, value)
#define DW_FORM(name, value) \
  case name:                 \
    return #name;
#define DW_END_FORM \
  }                 \
  return 0;         \
  }

// get_DW_AT_name(unsigned n)
#define DW_FIRST_AT(name, value)           \
  const char *get_DW_AT_name(unsigned n) { \
    switch (n) {                           \
      DW_AT(name, value)
#define DW_AT(name, value) \
  case name:               \
    return #name;
#define DW_END_AT \
  }               \
  return 0;       \
  }
#define DW_AT_DUP(name, value)

// get_DW_OP_name(unsigned n)
#define DW_FIRST_OP(name, value)           \
  const char *get_DW_OP_name(unsigned n) { \
    switch (n) {                           \
      DW_OP(name, value)
#define DW_OP(name, value) \
  case name:               \
    return #name;
#define DW_END_OP \
  }               \
  return 0;       \
  }
#define DW_OP_DUP(name, value)

// get_DW_ATE_name(unsigned n)
#define DW_FIRST_ATE(name, value)           \
  const char *get_DW_ATE_name(unsigned n) { \
    switch (n) {                            \
      DW_ATE(name, value)
#define DW_ATE(name, value) \
  case name:                \
    return #name;
#define DW_END_ATE \
  }                \
  return 0;        \
  }
#define DW_ATE_DUP(name, value)

// get_DW_CFA_name(unsigned n)
#define DW_FIRST_CFA(name, value)           \
  const char *get_DW_CFA_name(unsigned n) { \
    switch (n) {                            \
      DW_CFA(name, value)
#define DW_CFA(name, value) \
  case name:                \
    return #name;
#define DW_END_CFA \
  }                \
  return 0;        \
  }

#define DW_FIRST_LANG(name, value)           \
  const char *get_DW_LANG_name(unsigned n) { \
    switch (n) {                             \
      DW_LANG(name, value)
#define DW_LANG(name, value) \
  case name:                 \
    return #name;
#define DW_END_LANG \
  }                 \
  return 0;         \
  }

#define DW_FIRST_ACCESSIBILITY(name, value)           \
  const char *get_DW_ACCESSIBILITY_name(unsigned n) { \
    switch (n) {                                      \
      DW_ACCESSIBILITY(name, value)
#define DW_ACCESSIBILITY(name, value) \
  case name:                          \
    return #name;
#define DW_END_ACCESSIBILITY \
  }                          \
  return 0;                  \
  }

#include "../../../zeiss/prebuilt/tools/gdb/include/dwarf2.def"
#include "../../../zeiss/prebuilt/tools/gdb/include/dwarf2_lang.def"

static dw_ate GetAteFromPTY(PrimType pty) {
  switch (pty) {
    case PTY_u1:
      return DW_ATE_boolean;
    case PTY_u8:
      return DW_ATE_unsigned_char;
    case PTY_u16:
    case PTY_u32:
    case PTY_u64:
      return DW_ATE_unsigned;
    case PTY_i8:
      return DW_ATE_signed_char;
    case PTY_i16:
    case PTY_i32:
    case PTY_i64:
      return DW_ATE_signed;
    case PTY_f32:
    case PTY_f64:
    case PTY_f128:
      return DW_ATE_float;
    case PTY_agg:
    case PTY_ref:
    case PTY_ptr:
    case PTY_a32:
    case PTY_a64:
      return DW_ATE_address;
    case PTY_c64:
    case PTY_c128:
      return DW_ATE_complex_float;
    case PTY_void:
      return DW_ATE_void;
    default:
      return DW_ATE_void;
  }
}

// DBGDie methods
DBGDie::DBGDie(MIRModule *m, dw_tag tag)
  : mod_(m),
    tag_(tag),
    id(m->dbgInfo->maxid_),
    withchildren_(false),
    sibling(nullptr),
    firstchild(nullptr),
    abbrevid_(0),
    tyidx_(0),
    Offset(0),
    Size(0),
    attrvec_(m->memPoolAllocator.Adapter()),
    subdievec_(m->memPoolAllocator.Adapter()) {
  if (mod_->dbgInfo->parentdiestack_.size()) {
    parent = mod_->dbgInfo->parentdiestack_.top();
  } else {
    parent = nullptr;
  }
  attrvec_.clear();
  subdievec_.clear();
  m->dbgInfo->id_die_map_[m->dbgInfo->maxid_++] = this;
  ;
}

void DBGDie::ResetParentDie() {
  mod_->dbgInfo->ResetParentDie();
}

DBGDieAttr *DBGDie::AddAttr(dw_at at, dw_form form, uint64 val) {
  // collect strps which need label
  if (form == DW_FORM_strp) {
    mod_->dbgInfo->strps_.insert(val);
  }
  DBGDieAttr *attr = mod_->dbgInfo->CreateAttr(at, form, val);
  AddAttr(attr);
  return attr;
}

DBGDieAttr *DBGDie::AddSimpLocAttr(dw_at at, dw_form form, uint64 val) {
  DBGExprLoc *p = mod_->memPool->New<DBGExprLoc>(mod_, DW_OP_fbreg);
  if (val != kDbgDefaultVal) {
    p->AddSimpLocOpnd(val);
  }
  DBGDieAttr *attr = mod_->dbgInfo->CreateAttr(at, form, reinterpret_cast<uint64>(p));
  AddAttr(attr);
  return attr;
}

DBGDieAttr *DBGDie::AddGlobalLocAttr(dw_at at, dw_form form, uint64 val) {
  DBGExprLoc *p = mod_->memPool->New<DBGExprLoc>(mod_, DW_OP_addr);
  p->SetGvarStridx(val);
  DBGDieAttr *attr = mod_->dbgInfo->CreateAttr(at, form, reinterpret_cast<uint64>(p));
  AddAttr(attr);
  return attr;
}

DBGDieAttr *DBGDie::AddFrmBaseAttr(dw_at at, dw_form form, uint64 val) {
  DBGExprLoc *p = mod_->memPool->New<DBGExprLoc>(mod_, DW_OP_call_frame_cfa);
  DBGDieAttr *attr = mod_->dbgInfo->CreateAttr(at, form, reinterpret_cast<uint64>(p));
  AddAttr(attr);
  return attr;
}

DBGExprLoc *DBGDie::GetExprLoc() {
  for (auto it : attrvec_) {
    if (it->dwattr_ == DW_AT_location) {
      return it->val_.ptr;
    }
  }
  return nullptr;
}

bool DBGDie::SetAttr(dw_at attr, uint64 val) {
  for (auto it : attrvec_) {
    if (it->dwattr_ == attr) {
      it->val_.u = val;
      return true;
    }
  }
  return false;
}

bool DBGDie::SetAttr(dw_at attr, int val) {
  for (auto it : attrvec_) {
    if (it->dwattr_ == attr) {
      it->val_.i = val;
      return true;
    }
  }
  return false;
}

bool DBGDie::SetAttr(dw_at attr, uint32 val) {
  for (auto it : attrvec_) {
    if (it->dwattr_ == attr) {
      it->val_.id = val;
      return true;
    }
  }
  return false;
}

bool DBGDie::SetAttr(dw_at attr, int64 val) {
  for (auto it : attrvec_) {
    if (it->dwattr_ == attr) {
      it->val_.j = val;
      return true;
    }
  }
  return false;
}

bool DBGDie::SetAttr(dw_at attr, float val) {
  for (auto it : attrvec_) {
    if (it->dwattr_ == attr) {
      it->val_.f = val;
      return true;
    }
  }
  return false;
}

bool DBGDie::SetAttr(dw_at attr, double val) {
  for (auto it : attrvec_) {
    if (it->dwattr_ == attr) {
      it->val_.d = val;
      return true;
    }
  }
  return false;
}

bool DBGDie::SetAttr(dw_at attr, DBGExprLoc *ptr) {
  for (auto it : attrvec_) {
    if (it->dwattr_ == attr) {
      it->val_.ptr = ptr;
      return true;
    }
  }
  return false;
}

// DBGAbbrevEntry methods
DBGAbbrevEntry::DBGAbbrevEntry(MIRModule *m, DBGDie *die) : attrpairs_(m->memPoolAllocator.Adapter()) {
  tag_ = die->tag_;
  abbrevid_ = 0;
  withchildren_ = die->withchildren_;
  for (auto it : die->attrvec_) {
    attrpairs_.push_back(it->dwattr_);
    attrpairs_.push_back(it->dwform_);
  }
}

bool DBGAbbrevEntry::Equalto(DBGAbbrevEntry *entry) {
  if (attrpairs_.size() != entry->attrpairs_.size()) {
    return false;
  }
  if (withchildren_ != entry->withchildren_) {
    return false;
  }
  for (uint32 i = 0; i < attrpairs_.size(); i++) {
    if (attrpairs_[i] != entry->attrpairs_[i]) {
      return false;
    }
  }
  return true;
}

// DebugInfo methods
void DebugInfo::Init() {
  mplsrcidx_ = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(mod_->fileName);
  cu_ = mod_->memPool->New<DBGDie>(mod_, DW_TAG_compile_unit);
  id_die_map_[1] = cu_;
  ResetParentDie();
}

void DebugInfo::SetupCU() {
  cu_->withchildren_ = true;
  /* Add the Producer (Compiler) Information */
  const char *producer = "Maple Version 0.5.0 (tags/RELEASE-xxx/final)";
  GStrIdx strIdx = mod_->mirBuilder->GetOrCreateStringIndex(producer);
  cu_->AddAttr(DW_AT_producer, DW_FORM_strp, strIdx.GetIdx());

  /* Source Languate  */
  cu_->AddAttr(DW_AT_language, DW_FORM_data4, DW_LANG_C99);

  /* Add the compiled source file information */
  cu_->AddAttr(DW_AT_name, DW_FORM_strp, mplsrcidx_.GetIdx());
  strIdx = mod_->mirBuilder->GetOrCreateStringIndex("/to/be/done/current/path");
  cu_->AddAttr(DW_AT_comp_dir, DW_FORM_strp, strIdx.GetIdx());

  cu_->AddAttr(DW_AT_low_pc, DW_FORM_addr, kDbgDefaultVal);
  cu_->AddAttr(DW_AT_high_pc, DW_FORM_data8, kDbgDefaultVal);

  cu_->AddAttr(DW_AT_stmt_list, DW_FORM_sec_offset, kDbgDefaultVal);
}

void DebugInfo::BuildAliasDIEs() {
  for (auto it : func_lstridx_dieid_map_) {
    MIRFunction *func = it.first;
    for (std::pair<GStrIdx, MIRAliasVars> i : func->aliasVarMap) {
      DBGDie *die = GetLocalDie(func, i.second.memPoolStrIdx);
      // this local mpl variable does not exist because it is optimized away
      // by register renameing. please use mplme -O1 instead of -O2
      if (!die) {
        continue;
      }
      DBGDie *aliasdie = mod_->memPool->New<DBGDie>(mod_, DW_TAG_variable);

      // clone attributes, note DBGExprLoc pointer is copied as well
      // so the fboffset are the same as aliased maple variable
      for (auto attr : die->attrvec_) {
        aliasdie->AddAttr(attr->dwattr_, attr->dwform_, attr->val_.u);
      }
      // update name with aliased src variable name
      aliasdie->SetAttr(DW_AT_name, i.first.GetIdx());
      aliasdie->parent = die->parent;

      // add alias var name to debug_str section
      mod_->dbgInfo->strps_.insert(i.first.GetIdx());

      uint32 funcdieid = stridx_dieid_map_[func->GetNameStridx().GetIdx()];
      DBGDie *funcdie = id_die_map_[funcdieid];
      funcdie->subdievec_.push_back(aliasdie);
    }
  }
}

void DebugInfo::Finish() {
  SetupCU();
  FillTypeAttrWithDieId();
  BuildAliasDIEs();
  // build tree from root DIE cu_
  BuildDieTree();
  BuildAbbrev();
  ComputeSizeAndOffsets();
}

DBGDieAttr *DebugInfo::CreateAttr(dw_at at, dw_form form, uint64 val) {
  DBGDieAttr *attr = mod_->memPool->New<DBGDieAttr>(kDwAt);
  attr->dwattr_ = at;
  attr->dwform_ = form;
  attr->val_.u = val;
  return attr;
}

void DebugInfo::SetLocalDie(MIRFunction *func, GStrIdx strIdx, const DBGDie *die) {
  (func_lstridx_dieid_map_[func])[strIdx.GetIdx()] = die->id;
}

DBGDie *DebugInfo::GetLocalDie(MIRFunction *func, GStrIdx strIdx) {
  uint32 id = (func_lstridx_dieid_map_[func])[strIdx.GetIdx()];
  return id_die_map_[id];
}

void DebugInfo::SetLocalDie(GStrIdx strIdx, const DBGDie *die) {
  (func_lstridx_dieid_map_[mod_->CurFunction()])[strIdx.GetIdx()] = die->id;
}

DBGDie *DebugInfo::GetLocalDie(GStrIdx strIdx) {
  uint32 id = (func_lstridx_dieid_map_[mod_->CurFunction()])[strIdx.GetIdx()];
  return id_die_map_[id];
}

void DebugInfo::SetLabelIdx(MIRFunction *func, GStrIdx strIdx, LabelIdx labidx) {
  (func_lstridx_labidx_map_[func])[strIdx.GetIdx()] = labidx;
}

LabelIdx DebugInfo::GetLabelIdx(MIRFunction *func, GStrIdx strIdx) {
  LabelIdx labidx = (func_lstridx_labidx_map_[func])[strIdx.GetIdx()];
  return labidx;
}

void DebugInfo::SetLabelIdx(GStrIdx strIdx, LabelIdx labidx) {
  (func_lstridx_labidx_map_[mod_->CurFunction()])[strIdx.GetIdx()] = labidx;
}

LabelIdx DebugInfo::GetLabelIdx(GStrIdx strIdx) {
  LabelIdx labidx = (func_lstridx_labidx_map_[mod_->CurFunction()])[strIdx.GetIdx()];
  return labidx;
}

DBGDie *DebugInfo::CreateFormalParaDie(MIRType *type, GStrIdx nameidx, uint32 lnum) {
  DBGDie *die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_formal_parameter);

  (void)GetOrCreateTypeDie(type);
  die->AddAttr(DW_AT_type, DW_FORM_ref4, type->tyIdx.GetIdx());

  /* var Name */
  if (nameidx.GetIdx()) {
    die->AddAttr(DW_AT_name, DW_FORM_strp, nameidx.GetIdx());
    die->AddAttr(DW_AT_decl_file, DW_FORM_data4, mplsrcidx_.GetIdx());
    die->AddAttr(DW_AT_decl_line, DW_FORM_data4, lnum);
    die->AddSimpLocAttr(DW_AT_location, DW_FORM_exprloc, kDbgDefaultVal);
    SetLocalDie(nameidx, die);
  }
  return die;
}

DBGDie *DebugInfo::GetOrCreateLabelDie(LabelIdx labid) {
  MIRFunction *func = mod_->CurFunction();
  CHECK(labid < func->labelTab->labelTable.size(), "index out of range in DebugInfo::GetOrCreateLabelDie");
  GStrIdx strid = func->labelTab->labelTable[labid];
  if ((func_lstridx_dieid_map_[func]).size() &&
      (func_lstridx_dieid_map_[func]).find(strid.GetIdx()) != (func_lstridx_dieid_map_[func]).end()) {
    return GetLocalDie(strid);
  }

  DBGDie *die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_label);
  die->AddAttr(DW_AT_name, DW_FORM_strp, strid.GetIdx());
  die->AddAttr(DW_AT_decl_file, DW_FORM_data4, mplsrcidx_.GetIdx());
  die->AddAttr(DW_AT_decl_line, DW_FORM_data4, lexer_->GetLineNum());
  die->AddAttr(DW_AT_low_pc, DW_FORM_addr, kDbgDefaultVal);
  GetParentDie()->subdievec_.push_back(die);
  SetLocalDie(strid, die);
  SetLabelIdx(strid, labid);
  return die;
}

DBGDie *DebugInfo::CreateVarDie(MIRSymbol *sym, uint32 lnum) {
  // filter vtab
  if (sym->GetName().find(VTAB_PREFIX_STR) == 0) {
    return nullptr;
  }

  if (sym->GetName().find(GCTIB_PREFIX_STR) == 0) {
    return nullptr;
  }

  if (sym->storageClass == kScFormal) {
    return nullptr;
  }

  bool isLocal = sym->IsLocal();

  if (isLocal) {
    MIRFunction *func = mod_->CurFunction();
    if ((func_lstridx_dieid_map_[func]).size() &&
        (func_lstridx_dieid_map_[func]).find(sym->nameStrIdx.GetIdx()) != (func_lstridx_dieid_map_[func]).end()) {
      return GetLocalDie(sym->nameStrIdx);
    }
  } else {
    if (stridx_dieid_map_.find(sym->nameStrIdx.GetIdx()) != stridx_dieid_map_.end()) {
      uint32 id = stridx_dieid_map_[sym->nameStrIdx.GetIdx()];
      return id_die_map_[id];
    }
  }

  DBGDie *die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_variable);

  /* var Name */
  die->AddAttr(DW_AT_name, DW_FORM_strp, sym->nameStrIdx.GetIdx());
  die->AddAttr(DW_AT_decl_file, DW_FORM_data4, mplsrcidx_.GetIdx());
  die->AddAttr(DW_AT_decl_line, DW_FORM_data4, lnum);

  if (isLocal) {
    die->AddSimpLocAttr(DW_AT_location, DW_FORM_exprloc, kDbgDefaultVal);
  } else {
    // global var just use its name as address in .s
    uint64 idx = sym->nameStrIdx.GetIdx();
    if ((sym->IsReflectionClassInfo() && !sym->IsReflectionArrayClassInfo()) || sym->IsStatic()) {
      std::string ptrName = std::string(NameMangler::kPtrPrefixStr) + sym->GetName();
      idx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(ptrName).GetIdx();
    }
    die->AddGlobalLocAttr(DW_AT_location, DW_FORM_exprloc, idx);
  }

  MIRType *type = sym->GetType();
  (void)GetOrCreateTypeDie(type);
  die->AddAttr(DW_AT_type, DW_FORM_ref4, type->tyIdx.GetIdx());

  GetParentDie()->subdievec_.push_back(die);
  if (isLocal) {
    SetLocalDie(sym->nameStrIdx, die);
  } else {
    stridx_dieid_map_[sym->nameStrIdx.GetIdx()] = die->id;
  }
  return die;
}

DBGDie *DebugInfo::GetOrCreateFuncDeclDie(MIRFunction *func, uint32 lnum) {
  GStrIdx funcnameidx = func->GetNameStridx();
  if (funcdecl_dieid_map_.find(funcnameidx.GetIdx()) != funcdecl_dieid_map_.end()) {
    uint32 id = funcdecl_dieid_map_[funcnameidx.GetIdx()];
    return id_die_map_[id];
  }

  DBGDie *die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_subprogram);

  die->AddAttr(DW_AT_external, DW_FORM_flag_present, 1);

  // Function Name
  die->AddAttr(DW_AT_name, DW_FORM_strp, funcnameidx.GetIdx());
  die->AddAttr(DW_AT_decl_file, DW_FORM_data4, mplsrcidx_.GetIdx());
  die->AddAttr(DW_AT_decl_line, DW_FORM_data4, lnum);

  // Attributes for DW_AT_accessibility
  uint32 access = 0;
  if (func->IsPublic()) {
    access = DW_ACCESS_public;
  } else if (func->IsPrivate()) {
    access = DW_ACCESS_private;
  } else if (func->IsProtected()) {
    access = DW_ACCESS_protected;
  }
  if (access) {
    die->AddAttr(DW_AT_accessibility, DW_FORM_data4, access);
  }

  die->AddAttr(DW_AT_GNU_all_tail_call_sites, DW_FORM_flag_present, kDbgDefaultVal);

  // formal parameter
  GStrIdx strIdx(0);
  for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
    MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->formalDefVec[i].formalTyIdx);
    DBGDie *param = CreateFormalParaDie(type, strIdx, lnum);
    die->subdievec_.push_back(param);
    param->parent = die;
  }

  GetParentDie()->subdievec_.push_back(die);
  funcdecl_dieid_map_[funcnameidx.GetIdx()] = die->id;

  return die;
}

bool LIsCompilerGenerated(const MIRFunction *func) {
  return ((func->GetName().c_str())[0] != 'L');
}

DBGDie *DebugInfo::GetOrCreateFuncDefDie(MIRFunction *func, uint32 lnum) {
  GStrIdx funcnameidx = func->GetNameStridx();
  if (stridx_dieid_map_.find(funcnameidx.GetIdx()) != stridx_dieid_map_.end()) {
    uint32 id = stridx_dieid_map_[funcnameidx.GetIdx()];
    return id_die_map_[id];
  }

  DBGDie *funcdecldie = GetOrCreateFuncDeclDie(func, lnum);
  DBGDie *die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_subprogram);

  die->AddAttr(DW_AT_specification, DW_FORM_ref4, funcdecldie->id);
  die->AddAttr(DW_AT_decl_line, DW_FORM_data4, lnum);
  die->AddAttr(DW_AT_low_pc, DW_FORM_addr, kDbgDefaultVal);
  die->AddAttr(DW_AT_high_pc, DW_FORM_data8, kDbgDefaultVal);
  die->AddFrmBaseAttr(DW_AT_frame_base, DW_FORM_exprloc, kDbgDefaultVal);
  if (!func->IsStatic() && !LIsCompilerGenerated(func)) {
    die->AddAttr(DW_AT_object_pointer, DW_FORM_ref4, kDbgDefaultVal);
  }
  die->AddAttr(DW_AT_GNU_all_tail_call_sites, DW_FORM_flag_present, kDbgDefaultVal);

  cu_->subdievec_.push_back(die);
  stridx_dieid_map_[funcnameidx.GetIdx()] = die->id;

  // formal parameter
  for (uint32 i = 0; i < func->formalDefVec.size(); i++) {
    MIRSymbol *sym = func->formalDefVec[i].formalSym;
    MIRType *type = sym->GetType();
    DBGDie *param = CreateFormalParaDie(type, sym->nameStrIdx, lnum);
    die->subdievec_.push_back(param);
    param->parent = die;
  }

  return die;
}

DBGDie *DebugInfo::GetOrCreatePrimTypeDie(PrimType pty) {
  if (tyidx_dieid_map_.find(static_cast<uint32>(pty)) != tyidx_dieid_map_.end()) {
    uint32 id = tyidx_dieid_map_[static_cast<uint32>(pty)];
    return id_die_map_[id];
  }

  DBGDie *die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_base_type);
  die->tyidx_ = static_cast<uint32>(pty);

  die->AddAttr(DW_AT_byte_size, DW_FORM_data4, GetPrimTypeSize(pty));
  die->AddAttr(DW_AT_encoding, DW_FORM_data4, GetAteFromPTY(pty));

  cu_->subdievec_.push_back(die);
  die->parent = cu_;
  tyidx_dieid_map_[static_cast<uint32>(pty)] = die->id;
  id_die_map_[die->id] = die;
  return die;
}

DBGDie *DebugInfo::GetOrCreateTypeDie(MIRType *type) {
  if (!type) {
    return nullptr;
  }

  uint32 tid = type->tyIdx.GetIdx();
  if (tyidx_dieid_map_.find(tid) != tyidx_dieid_map_.end()) {
    uint32 id = tyidx_dieid_map_[tid];
    return id_die_map_[id];
  }

  uint32 sid = type->nameStrIdx.GetIdx();
  if (sid)
    if (stridx_dieid_map_.find(sid) != stridx_dieid_map_.end()) {
      uint32 id = stridx_dieid_map_[sid];
      return id_die_map_[id];
    }

  if (type && type->tyIdx == static_cast<uint32>(type->primType)) {
    return GetOrCreatePrimTypeDie(type->primType);
  }

  DBGDie *die = nullptr;
  switch (type->typeKind) {
    case kTypePointer: {
      MIRPtrType *ptype = static_cast<MIRPtrType *>(type);
      die = GetOrCreatePointTypeDie(ptype);
      tyidx_dieid_map_[type->tyIdx.GetIdx()] = die->id;
      id_die_map_[die->id] = die;
      break;
    }
    case kTypeArray:
    case kTypeFArray:
    case kTypeJArray: {
      MIRArrayType *atype = static_cast<MIRArrayType *>(type);
      die = GetOrCreateArrayTypeDie(atype);
      tyidx_dieid_map_[type->tyIdx.GetIdx()] = die->id;
      id_die_map_[die->id] = die;
      break;
    }
    case kTypeUnion:
    case kTypeStruct:
    case kTypeClass:
    case kTypeInterface: {
      uint32 id = tyidx_dieid_map_[type->tyIdx.GetIdx()];
      uint32 id1 = stridx_dieid_map_[type->tyIdx.GetIdx()];
      CHECK_FATAL(id == id1, "id & id1 not equal");
      die = id_die_map_[id];
      CHECK_FATAL(die != nullptr, "class and interface types should have being defined");
      break;
    }
    case kTypeClassIncomplete:
    case kTypeInterfaceIncomplete: {
      die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_class_type);
      die->AddAttr(DW_AT_decl_line, DW_FORM_data4, lexer_->GetLineNum());
      if (type->nameStrIdx.GetIdx()) {
        stridx_dieid_map_[type->nameStrIdx.GetIdx()] = die->id;
        die->AddAttr(DW_AT_name, DW_FORM_strp, type->nameStrIdx.GetIdx());
      }
      tyidx_dieid_map_[type->tyIdx.GetIdx()] = die->id;
      id_die_map_[die->id] = die;
      break;
    }
    case kTypeBitField:
      break;
    default:
      CHECK_FATAL(false, "TODO: support type");
      break;
  }

  return die;
}

DBGDie *DebugInfo::GetOrCreatePointTypeDie(const MIRPtrType *ptrtype) {
  uint32 tid = ptrtype->tyIdx.GetIdx();
  if (tyidx_dieid_map_.find(tid) != tyidx_dieid_map_.end()) {
    uint32 id = tyidx_dieid_map_[tid];
    return id_die_map_[id];
  }

  MIRType *type = ptrtype->GetPointedType();
  // for <* void>
  if (type && type->primType == PTY_void) {
    DBGDie *die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_pointer_type);
    die->AddAttr(DW_AT_byte_size, DW_FORM_data4, 8);
    tyidx_dieid_map_[ptrtype->tyIdx.GetIdx()] = die->id;
    id_die_map_[die->id] = die;
    cu_->subdievec_.push_back(die);
    die->parent = cu_;
    return die;
  }

  (void)GetOrCreateTypeDie(type);
  if (typedef_tyidx_map_.find(type->tyIdx.GetIdx()) != typedef_tyidx_map_.end()) {
    uint32 tid = typedef_tyidx_map_[type->tyIdx.GetIdx()];
    if (pointed_pointer_map_.find(tid) != pointed_pointer_map_.end()) {
      uint32 tyid = pointed_pointer_map_[tid];
      if (tyidx_dieid_map_.find(tyid) != tyidx_dieid_map_.end()) {
        uint32 dieid = tyidx_dieid_map_[tyid];
        DBGDie *die = id_die_map_[dieid];
        return die;
      }
    }
  }

  // update incomplete type from stridx_dieid_map_ to tyidx_dieid_map_
  MIRStructType *stype = dynamic_cast<MIRStructType *>(type);
  if (stype && stype->IsIncomplete()) {
    uint32 sid = stype->nameStrIdx.GetIdx();
    if (stridx_dieid_map_.find(sid) != stridx_dieid_map_.end()) {
      uint32 dieid = stridx_dieid_map_[sid];
      if (dieid) {
        tyidx_dieid_map_[stype->tyIdx.GetIdx()] = dieid;
      }
    }
  }

  DBGDie *die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_pointer_type);
  die->AddAttr(DW_AT_byte_size, DW_FORM_data4, 8);
  // fill with type idx instead of typedie->id to avoid nullptr typedie of
  // forward reference of class types
  die->AddAttr(DW_AT_type, DW_FORM_ref4, type->tyIdx.GetIdx());
  tyidx_dieid_map_[ptrtype->tyIdx.GetIdx()] = die->id;
  id_die_map_[die->id] = die;

  cu_->subdievec_.push_back(die);
  die->parent = cu_;

  return die;
}

DBGDie *DebugInfo::GetOrCreateArrayTypeDie(const MIRArrayType *arraytype) {
  uint32 tid = arraytype->tyIdx.GetIdx();
  if (tyidx_dieid_map_.find(tid) != tyidx_dieid_map_.end()) {
    uint32 id = tyidx_dieid_map_[tid];
    return id_die_map_[id];
  }

  MIRType *type = arraytype->GetElemType();
  (void)GetOrCreateTypeDie(type);

  DBGDie *die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_array_type);
  die->AddAttr(DW_AT_byte_size, DW_FORM_data4, 8);
  // fill with type idx instead of typedie->id to avoid nullptr typedie of
  // forward reference of class types
  die->AddAttr(DW_AT_type, DW_FORM_ref4, type->tyIdx.GetIdx());
  tyidx_dieid_map_[arraytype->tyIdx.GetIdx()] = die->id;
  id_die_map_[die->id] = die;

  cu_->subdievec_.push_back(die);
  die->parent = cu_;

  // maple uses array of 1D array to represent 2D array
  // so only one DW_TAG_subrange_type entry is needed
  DBGDie *rangedie = mod_->memPool->New<DBGDie>(mod_, DW_TAG_subrange_type);
  PrimType prmtype = PTY_u32;
  (void)GetOrCreatePrimTypeDie(prmtype);
  rangedie->AddAttr(DW_AT_type, DW_FORM_ref4, PTY_u32);
  rangedie->AddAttr(DW_AT_upper_bound, DW_FORM_data4, arraytype->sizeArray[0]);

  die->subdievec_.push_back(rangedie);
  rangedie->parent = die;

  return die;
}

DBGDie *DebugInfo::GetOrCreateFieldDie(maple::FieldPair pair, uint32 lnum) {
  for (DBGDie *it : GetParentDie()->subdievec_)
    for (DBGDieAttr *at : it->attrvec_)
      if (at->dwattr_ == DW_AT_name && at->val_.id == pair.first.GetIdx()) {
        return nullptr;
      }

  DBGDie *die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_member);

  die->AddAttr(DW_AT_name, DW_FORM_strp, pair.first.GetIdx());
  die->AddAttr(DW_AT_decl_file, DW_FORM_data4, mplsrcidx_.GetIdx());
  die->AddAttr(DW_AT_decl_line, DW_FORM_data4, lnum);

  MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(pair.second.first);
  (void)GetOrCreateTypeDie(type);
  // fill with type idx instead of typedie->id to avoid nullptr typedie of
  // forward reference of class types
  die->AddAttr(DW_AT_type, DW_FORM_ref4, type->tyIdx.GetIdx());

  die->AddAttr(DW_AT_data_member_location, DW_FORM_data4, kDbgDefaultVal);
  GetParentDie()->subdievec_.push_back(die);

  return die;
}

// shared between classes and interfaces
DBGDie *DebugInfo::InitClassTypeDie(GStrIdx strIdx, uint32 lnum) {
  DBGDie *die = nullptr;
  if (stridx_dieid_map_.find(strIdx.GetIdx()) != stridx_dieid_map_.end()) {
    uint32 id = stridx_dieid_map_[strIdx.GetIdx()];
    die = id_die_map_[id];
  }
  if (!die) {
    die = mod_->memPool->New<DBGDie>(mod_, DW_TAG_class_type);
    die->AddAttr(DW_AT_decl_line, DW_FORM_data4, lnum);
    die->AddAttr(DW_AT_name, DW_FORM_strp, strIdx.GetIdx());
    stridx_dieid_map_[strIdx.GetIdx()] = die->id;
  }
  PushParentDie(die);
  return die;
}

// shared between struct and union
void DebugInfo::BuildStructTypeDie(GStrIdx strIdx, const MIRStructType *structtype, DBGDie *&die) {
  for (uint32 i = 0; i < cu_->subdievec_.size(); i++)
    if (cu_->subdievec_[i] == die) {
      return;
    }

  uint32 tid = structtype->tyIdx.GetIdx();
  if (tyidx_dieid_map_.find(tid) != tyidx_dieid_map_.end()) {
    uint32 dieid = tyidx_dieid_map_[tid];
    DBGDie *typedie = id_die_map_[dieid];
    uint32 size = typedie->subdievec_.size();
    // clone attributes
    if (size > 0) {
      for (uint32 i = 0; i < size; i++) {
        DBGDie *subdie = mod_->memPool->New<DBGDie>(mod_, DW_TAG_member);
        for (auto attr : typedie->subdievec_[i]->attrvec_) {
          subdie->AddAttr(attr->dwattr_, attr->dwform_, attr->val_.u);
        }
        die->subdievec_.push_back(subdie);
      }
    }
  } else {
    tyidx_dieid_map_[tid] = die->id;
  }

  if (strIdx.GetIdx()) {
    stridx_dieid_map_[strIdx.GetIdx()] = die->id;
  }

  if (structtype->typeKind == kTypeStruct) {
    die->tag_ = DW_TAG_structure_type;
  } else if (structtype->typeKind == kTypeUnion) {
    die->tag_ = DW_TAG_union_type;
  }

  die->AddAttr(DW_AT_byte_size, DW_FORM_data4, kDbgDefaultVal);
  die->AddAttr(DW_AT_decl_file, DW_FORM_data4, mplsrcidx_.GetIdx());

  cu_->subdievec_.push_back(die);
  ResetParentDie();
  return;
}

void DebugInfo::BuildClassTypeDie(GStrIdx strIdx, const MIRClassType *classtype, DBGDie *&die) {
  for (uint32 i = 0; i < cu_->subdievec_.size(); i++)
    if (cu_->subdievec_[i] == die) {
      return;
    }

  tyidx_dieid_map_[classtype->tyIdx.GetIdx()] = die->id;
  if (strIdx.GetIdx()) {
    stridx_dieid_map_[strIdx.GetIdx()] = die->id;
  }

  die->AddAttr(DW_AT_byte_size, DW_FORM_data4, kDbgDefaultVal);
  die->AddAttr(DW_AT_decl_file, DW_FORM_data4, mplsrcidx_.GetIdx());

  cu_->subdievec_.push_back(die);

  // parent
  uint32 ptid = classtype->parentTyIdx.GetIdx();
  if (ptid) {
    MIRType *parenttype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(classtype->parentTyIdx);
    DBGDie *parentdie = GetOrCreateTypeDie(parenttype);
    if (parentdie) {
      parentdie = mod_->memPool->New<DBGDie>(mod_, DW_TAG_inheritance);
      parentdie->AddAttr(DW_AT_name, DW_FORM_strp, parenttype->nameStrIdx.GetIdx());
      parentdie->AddAttr(DW_AT_type, DW_FORM_ref4, ptid);

      // set to DW_ACCESS_public for now
      parentdie->AddAttr(DW_AT_accessibility, DW_FORM_data4, DW_ACCESS_public);
      die->subdievec_.push_back(parentdie);
    }
  }

  ResetParentDie();
  return;
}

void DebugInfo::BuildInterfaceTypeDie(GStrIdx strIdx, MIRInterfaceType *interfacetype, DBGDie *&die) {
  for (uint32 i = 0; i < cu_->subdievec_.size(); i++)
    if (cu_->subdievec_[i] == die) {
      return;
    }

  tyidx_dieid_map_[interfacetype->tyIdx.GetIdx()] = die->id;
  if (strIdx.GetIdx()) {
    stridx_dieid_map_[strIdx.GetIdx()] = die->id;
  }

  die->tag_ = DW_TAG_interface_type;
  die->AddAttr(DW_AT_decl_file, DW_FORM_data4, mplsrcidx_.GetIdx());

  cu_->subdievec_.push_back(die);

  // parents
  for (auto it : interfacetype->parentsTyIdx) {
    MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(it);
    DBGDie *parentdie = GetOrCreateTypeDie(type);
    if (parentdie) {
      continue;
    }
    parentdie = mod_->memPool->New<DBGDie>(mod_, DW_TAG_inheritance);
    parentdie->AddAttr(DW_AT_name, DW_FORM_strp, type->nameStrIdx.GetIdx());
    parentdie->AddAttr(DW_AT_type, DW_FORM_ref4, it.GetIdx());
    parentdie->AddAttr(DW_AT_data_member_location, DW_FORM_data4, kDbgDefaultVal);

    // set to DW_ACCESS_public for now
    parentdie->AddAttr(DW_AT_accessibility, DW_FORM_data4, DW_ACCESS_public);
    die->subdievec_.push_back(parentdie);
  }

  ResetParentDie();
  return;
}

uint32 DebugInfo::GetAbbrevId(DBGAbbrevEntryVec *vec, DBGAbbrevEntry *entry) {
  for (auto it : vec->entryvec_) {
    if (it->Equalto(entry)) {
      return it->abbrevid_;
    }
  }
  return 0;
}

void DebugInfo::BuildAbbrev() {
  uint32 abbrevid = 1;
  for (uint32 i = 1; i < maxid_; i++) {
    DBGDie *die = id_die_map_[i];
    DBGAbbrevEntry *entry = mod_->memPool->New<DBGAbbrevEntry>(mod_, die);

    if (!tag_abbrev_map_[die->tag_]) {
      tag_abbrev_map_[die->tag_] = mod_->memPool->New<DBGAbbrevEntryVec>(mod_, die->tag_);
    }

    uint32 id = GetAbbrevId(tag_abbrev_map_[die->tag_], entry);
    if (id) {
      // using existing abbrev id
      die->abbrevid_ = id;
    } else {
      // add entry to vector
      entry->abbrevid_ = abbrevid++;
      tag_abbrev_map_[die->tag_]->entryvec_.push_back(entry);
      abbrev_vec_.push_back(entry);
      // update abbrevid in die
      die->abbrevid_ = entry->abbrevid_;
    }
  }
}

void DebugInfo::BuildDieTree() {
  for (auto it : id_die_map_) {
    if (!it.first) {
      continue;
    }
    DBGDie *die = it.second;
    uint32 size = die->subdievec_.size();
    die->withchildren_ = (size > 0);
    if (size) {
      die->firstchild = die->subdievec_[0];
      for (uint32 i = 0; i < size - 1; i++) {
        DBGDie *it = die->subdievec_[i];
        DBGDie *it1 = die->subdievec_[i + 1];
        if (it->subdievec_.size()) {
          it->sibling = it1;
          it->AddAttr(DW_AT_sibling, DW_FORM_ref4, it1->id);
        }
      }
    }
  }
}

void DebugInfo::FillTypeAttrWithDieId() {
  for (auto it : id_die_map_) {
    DBGDie *die = it.second;
    for (auto at : die->attrvec_) {
      if (at->dwattr_ == DW_AT_type) {
        uint32 tid = at->val_.id;
        MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(tid));
        if (type) {
          uint32 dieid = tyidx_dieid_map_[tid];
          if (dieid) {
            at->val_.id = dieid;
          } else {
            LogInfo::MapleLogger() << "dieid not found, typeKind = " << type->typeKind << " primType = " << type->primType
                      << " nameStrIdx = " << type->nameStrIdx.GetIdx() << std::endl;
          }
        } else {
          LogInfo::MapleLogger() << "type not found, tid = " << tid << std::endl;
        }
        break;
      }
    }
  }
}

DBGDie *DebugInfo::GetDie(const MIRFunction *func) {
  uint32 id = stridx_dieid_map_[func->GetNameStridx().GetIdx()];
  if (id) {
    return id_die_map_[id];
  }
  return nullptr;
}

// Methods for calculating Offset and Size of DW_AT_xxx
uint32 DBGDieAttr::SizeOf(DBGDieAttr *attr) {
  dw_form form = attr->dwform_;
  switch (form) {
    // case DW_FORM_implicitconst:
    case DW_FORM_flag_present:
      return 0;  // Not handled yet.
    case DW_FORM_flag:
    case DW_FORM_ref1:
    case DW_FORM_data1:
      return sizeof(int8);
    case DW_FORM_ref2:
    case DW_FORM_data2:
      return sizeof(int16);
    case DW_FORM_ref4:
    case DW_FORM_data4:
      return sizeof(int32);
    case DW_FORM_ref8:
    case DW_FORM_ref_sig8:
    case DW_FORM_data8:
      return sizeof(int64);
    case DW_FORM_addr:
      return sizeof(int64);
    case DW_FORM_sec_offset:
    case DW_FORM_ref_addr:
    case DW_FORM_strp:
    case DW_FORM_GNU_ref_alt:
      // case DW_FORM_line_strp:
      // case DW_FORM_strp_sup:
      // case DW_FORM_ref_sup:
      return 4;  // DWARF32, 8 if DWARF64

    case DW_FORM_string: {
      const std::string &str = GlobalTables::GetStrTable().GetStringFromStrIdx(attr->val_.id);
      return str.length() + 1 /* terminal null byte */;
    }
    case DW_FORM_exprloc: {
      DBGExprLoc *ptr = attr->val_.ptr;
      CHECK_FATAL(ptr != (DBGExprLoc *)0xdeadbeef, "wrong ptr");
      switch (ptr->GetOp()) {
        case DW_OP_call_frame_cfa:
          return 2;  // size 1 byte + DW_OP_call_frame_cfa 1 byte
        case DW_OP_fbreg: {
          // DW_OP_fbreg 1 byte
          uint32 size = 1 + NameMangler::GetSleb128Size(ptr->GetFboffset());
          return size + NameMangler::GetUleb128Size(size);
        }
        case DW_OP_addr: {
          return NameMangler::GetUleb128Size(9) + 9;
        }
        default:
          return 4;
      }
    }
    default:
      CHECK_FATAL(maple::get_DW_FORM_name(form) != nullptr,
             "get_DW_FORM_name return null in DebugInfo::FillTypeAttrWithDieId");
      LogInfo::MapleLogger() << "unhandled SizeOf: " << maple::get_DW_FORM_name(form) << std::endl;
      return 0;
  }
}

void DebugInfo::ComputeSizeAndOffsets() {
  // CU-relative offset is reset to 0 here.
  uint32 cuOffset = sizeof(int32_t)  // Length of Unit Info
                    + sizeof(int16)  // DWARF version number      : 0x0004
                    + sizeof(int32)  // Offset into Abbrev. Section : 0x0000
                    + sizeof(int8);  // Pointer Size (in bytes)       : 0x08

  // After returning from this function, the length value is the size
  // of the .debug_info section
  ComputeSizeAndOffset(mod_->dbgInfo->cu_, cuOffset);
  mod_->dbgInfo->debug_info_length_ = cuOffset - sizeof(int32_t);
}

// Compute the size and offset of a DIE. The Offset is relative to start of the CU.
// It returns the offset after laying out the DIE.
void DebugInfo::ComputeSizeAndOffset(DBGDie *die, uint32 &cuOffset) {
  uint32 cuOffsetOrg = cuOffset;
  die->Offset = cuOffset;

  // Add the byte size of the abbreviation code
  cuOffset += static_cast<uint32>(NameMangler::GetUleb128Size(uint64_t(die->abbrevid_)));

  // Add the byte size of all the DIE attributes.
  for (const auto &attr : die->attrvec_) {
    cuOffset += attr->SizeOf(attr);
  }

  die->Size = cuOffset - cuOffsetOrg;

  // Let the children compute their offsets.
  if (die->withchildren_) {
    uint32 size = die->subdievec_.size();

    for (uint32 i = 0; i < size; i++) {
      DBGDie *childDie = die->subdievec_[i];
      ComputeSizeAndOffset(childDie, cuOffset);
    }

    // Each child chain is terminated with a zero byte, adjust the offset.
    cuOffset += sizeof(int8);
  }
}

///////////////////////////////
// Dumps
///////////////////////////////
void DebugInfo::Dump(int indent) {
  LogInfo::MapleLogger() << "\n" << std::endl;
  LogInfo::MapleLogger() << "maple_debug_information {"
             << "  Length: " << HEX(debug_info_length_) << std::endl;
  cu_->Dump(indent + 1);
  LogInfo::MapleLogger() << "}\n" << std::endl;
  LogInfo::MapleLogger() << "maple_debug_abbrev {" << std::endl;
  for (uint32 i = 1; i < abbrev_vec_.size(); i++) {
    abbrev_vec_[i]->Dump(indent + 1);
  }
  LogInfo::MapleLogger() << "}" << std::endl;
  return;
}

void DBGExprLoc::Dump() {
  LogInfo::MapleLogger() << " " << HEX(GetOp());
  for (auto it : simploc_->opnds_) {
    LogInfo::MapleLogger() << " " << HEX(it);
  }
}

void DBGDieAttr::Dump(int indent) {
  PrintIndentation(indent);
  CHECK_FATAL(get_DW_FORM_name(dwform_) && get_DW_AT_name(dwattr_), "null ptr check");
  LogInfo::MapleLogger() << get_DW_AT_name(dwattr_) << " " << get_DW_FORM_name(dwform_);
  if (dwform_ == DW_FORM_string || dwform_ == DW_FORM_strp) {
    GStrIdx idx(val_.id);
    LogInfo::MapleLogger() << " 0x" << std::hex << val_.u << std::dec;
    LogInfo::MapleLogger() << " \"" << GlobalTables::GetStrTable().GetStringFromStrIdx(idx).c_str() << "\"";
  } else if (dwform_ == DW_FORM_ref4) {
    LogInfo::MapleLogger() << " <" << HEX(val_.id) << ">";
  } else if (dwattr_ == DW_AT_encoding) {
    CHECK_FATAL(get_DW_ATE_name(val_.u), "null ptr check");
    LogInfo::MapleLogger() << " " << get_DW_ATE_name(val_.u);
  } else if (dwattr_ == DW_AT_language) {
    CHECK_FATAL(get_DW_LANG_name(val_.u), "null ptr check ");
    LogInfo::MapleLogger() << " " << get_DW_LANG_name(val_.u);
  } else if (dwattr_ == DW_AT_accessibility) {
    CHECK_FATAL(get_DW_ACCESSIBILITY_name(val_.u), "null ptr check");
    LogInfo::MapleLogger() << " " << get_DW_ACCESSIBILITY_name(val_.u);
  } else if (dwattr_ == DW_AT_location) {
    val_.ptr->Dump();
  } else {
    LogInfo::MapleLogger() << " 0x" << std::hex << val_.u << std::dec;
  }
  LogInfo::MapleLogger() << std::endl;
}

void DBGDie::Dump(int indent) {
  PrintIndentation(indent);
  LogInfo::MapleLogger() << "<" << HEX(id) << "><" << HEX(Offset);
  LogInfo::MapleLogger() << "><" << HEX(Size) << "><"
             << "> abbrev id: " << HEX(abbrevid_);
  CHECK_FATAL(get_DW_TAG_name(tag_), "null ptr check");
  LogInfo::MapleLogger() << " (" << get_DW_TAG_name(tag_) << ") ";
  if (parent) {
    LogInfo::MapleLogger() << "parent <" << HEX(parent->id);
  }
  LogInfo::MapleLogger() << "> {";
  if (tyidx_) {
    MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(tyidx_));
    if (type->typeKind == kTypeStruct || type->typeKind == kTypeClass || type->typeKind == kTypeInterface) {
      MIRStructType *stype = static_cast<MIRStructType *>(type);
      LogInfo::MapleLogger() << "           # " << stype->GetName();
    } else {
      LogInfo::MapleLogger() << "           # " << GetPrimTypeName(type->primType);
    }
  }
  LogInfo::MapleLogger() << std::endl;
  ;
  for (auto it : attrvec_) {
    it->Dump(indent + 1);
  }
  PrintIndentation(indent);
  LogInfo::MapleLogger() << "} ";
  if (subdievec_.size()) {
    LogInfo::MapleLogger() << " {" << std::endl;
    for (auto it : subdievec_) {
      it->Dump(indent + 1);
    }
    PrintIndentation(indent);
    LogInfo::MapleLogger() << "}";
  }
  LogInfo::MapleLogger() << std::endl;
  return;
}

void DBGAbbrevEntry::Dump(int indent) {
  PrintIndentation(indent);
  CHECK_FATAL(get_DW_TAG_name(tag_), "null ptr check ");
  LogInfo::MapleLogger() << "<" << HEX(abbrevid_) << "> " << get_DW_TAG_name(tag_);
  if (withchildren_) {
    LogInfo::MapleLogger() << " [with children] {" << std::endl;
  } else {
    LogInfo::MapleLogger() << " [no children] {" << std::endl;
  }
  for (uint32 i = 0; i < attrpairs_.size(); i += 2) {
    PrintIndentation(indent + 1);
    CHECK_FATAL(get_DW_AT_name(attrpairs_[i]) && get_DW_FORM_name(attrpairs_[i + 1]), "NULLPTR CHECK");

    LogInfo::MapleLogger() << " " << get_DW_AT_name(attrpairs_[i]) << " " << get_DW_FORM_name(attrpairs_[i + 1]) << " " << std::endl;
  }
  PrintIndentation(indent);
  LogInfo::MapleLogger() << "}" << std::endl;
  return;
}

void DBGAbbrevEntryVec::Dump(int indent) {
  for (auto it : entryvec_) {
    PrintIndentation(indent);
    it->Dump(indent);
  }
  return;
}

// DBGCompileMsgInfo methods
void DBGCompileMsgInfo::ClearLine(uint32 n) {
  errno_t eNum = memset_s(line_[n], MAXLINELEN, 0, MAXLINELEN);
  if (eNum) {
    FATAL(kLncFatal, "memset_s failed");
  }
}

DBGCompileMsgInfo::DBGCompileMsgInfo() : startline_(0), errpos_(0) {
  linenum_[0] = 0;
  linenum_[1] = 0;
  linenum_[2] = 0;
  ClearLine(0);
  ClearLine(1);
  ClearLine(2);
  err_l_num_ = 0;
  err_c_num_ = 0;
}

void DBGCompileMsgInfo::SetErrPos(uint32 lnum, uint32 cnum) {
  err_l_num_ = lnum;
  err_c_num_ = cnum;
}

void DBGCompileMsgInfo::UpdateMsg(uint32 lnum, const char *line) {
  // LogInfo::MapleLogger() << "get #" << lnum << " "<< line << std::endl;
  size_t size = strlen(line);
  if (size > MAXLINELEN - 1) {
    size = MAXLINELEN - 1;
  }
  startline_ = (startline_ + 2) % 3;
  ClearLine(startline_);
  errno_t eNum = memcpy_s(line_[startline_], MAXLINELEN, line, size);
  if (eNum) {
    FATAL(kLncFatal, "memcpy_s failed");
  }
  line_[startline_][size] = '\0';
  linenum_[startline_] = lnum;
}

void DBGCompileMsgInfo::EmitMsg() {
  char str[MAXLINELEN + 1];

  errpos_ = err_c_num_;
  errpos_ = (errpos_ < 2) ? 2 : errpos_;
  errpos_ = (errpos_ > MAXLINELEN) ? MAXLINELEN : errpos_;
  for (uint32 i = 0; i < errpos_ - 1; i++) {
    str[i] = ' ';
  }
  str[errpos_ - 1] = '^';
  str[errpos_] = '\0';

  fprintf(stderr, "\n===================================================================\n");
  fprintf(stderr, "==================");
  fprintf(stderr, BOLD YEL "  Compilation Error Diagnosis  " RESET);
  fprintf(stderr, "==================\n");
  fprintf(stderr, "===================================================================\n");
  fprintf(stderr, "line %4d %s\n", linenum_[(startline_ + 2) % 3],
          reinterpret_cast<char *>(line_[(startline_ + 2) % 3]));
  fprintf(stderr, "line %4d %s\n", linenum_[(startline_ + 1) % 3],
          reinterpret_cast<char *>(line_[(startline_ + 1) % 3]));
  fprintf(stderr, "line %4d %s\n", linenum_[(startline_ + 0) % 3],
          reinterpret_cast<char *>(line_[(startline_ + 0) % 3]));
  fprintf(stderr, BOLD RED "          %s\n" RESET, str);
  fprintf(stderr, "===================================================================\n");
}

}  // namespace maple
