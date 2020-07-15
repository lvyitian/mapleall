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

#ifndef MAPLE_IR_INCLUDE_DBG_INFO_H
#define MAPLE_IR_INCLUDE_DBG_INFO_H
#include <iostream>

#include "mpl_logging.h"
#include "types_def.h"
#include "prim_types.h"
#include "mir_nodes.h"
#include "lexer.h"
#include "../../../zeiss/prebuilt/tools/gdb/include/dwarf2.h"

using namespace maple;

namespace maple {

// for more color code: http://ascii-table.com/ansi-escape-sequences.php
#define RESET "\x1B[0m"
#define BOLD "\x1B[1m"
#define RED "\x1B[31m"
#define GRN "\x1B[32m"
#define YEL "\x1B[33m"

const uint32 kDbgDefaultVal = 0xdeadbeef;
#define HEX(val) std::hex << "0x" << val << std::dec

class MIRModule;
class MIRType;
class MIRSymbol;
class MIRSymbolTable;
class MIRTypeNameTable;
class DBGBuilder;
class DBGCompileMsgInfo;
class MIRLexer;

// for compiletime warnings
class DBGLine {
 public:
  uint32 linenum_;
  const char *line_;

  DBGLine(uint32 lnum, const char *l) : linenum_(lnum), line_(l) {}

  void Dump() {
    LogInfo::MapleLogger() << "LINE: " << linenum_ << " " << line_ << std::endl;
  }
};

#define MAXLINELEN 4096

class DBGCompileMsgInfo {
 public:
  uint32 startline_;  // mod 3
  uint32 err_l_num_;
  uint32 err_c_num_;
  uint32 errpos_;
  uint32 linenum_[3];
  uint8 line_[3][MAXLINELEN];  // 3 round-robin line buffers

  DBGCompileMsgInfo();
  virtual ~DBGCompileMsgInfo() {}
  void ClearLine(uint32 n);
  void SetErrPos(uint32 lnum, uint32 cnum);
  void UpdateMsg(uint32 lnum, const char *line);
  void EmitMsg();
};

enum DBGDieKind { kDwTag, kDwAt, kDwOp, kDwAte, kDwForm, kDwCfa };

typedef uint32 dw_tag;   // for DW_TAG_*
typedef uint32 dw_at;    // for DW_AT_*
typedef uint32 dw_op;    // for DW_OP_*
typedef uint32 dw_ate;   // for DW_ATE_*
typedef uint32 dw_form;  // for DW_FORM_*
typedef uint32 dw_cfa;   // for DW_CFA_*

class DBGDieAttr;

class DBGExpr {
 public:
  dw_op dwop_;
  // for local var fboffset, global var strIdx
  int val_;
  MapleVector<uint64> opnds_;
  DBGExpr(MIRModule *m) : dwop_(0), val_(kDbgDefaultVal), opnds_(m->memPoolAllocator.Adapter()) {}

  DBGExpr(MIRModule *m, dw_op op) : dwop_(op), val_(kDbgDefaultVal), opnds_(m->memPoolAllocator.Adapter()) {}

  virtual ~DBGExpr() {}

  void AddOpnd(uint64 val) {
    opnds_.push_back(val);
  }
};

class DBGExprLoc {
 public:
  MIRModule *mod_;
  DBGExpr *simploc_;
  MapleVector<DBGExpr> expr_vec_;
  void *symloc_;

  DBGExprLoc(MIRModule *m) : mod_(m), expr_vec_(m->memPoolAllocator.Adapter()), symloc_(nullptr) {
    simploc_ = m->memPool->New<DBGExpr>(mod_);
  }

  DBGExprLoc(MIRModule *m, dw_op op) : mod_(m), expr_vec_(m->memPoolAllocator.Adapter()), symloc_(nullptr) {
    simploc_ = m->memPool->New<DBGExpr>(mod_, op);
  }

  virtual ~DBGExprLoc() {}

  bool IsSimp() const {
    return (expr_vec_.size() == 0 && simploc_->val_ != (int)kDbgDefaultVal);
  }

  int GetFboffset() const {
    return simploc_->val_;
  }

  void SetFboffset(int offset) {
    simploc_->val_ = offset;
  }

  int GetGvarStridx() const {
    return simploc_->val_;
  }

  void SetGvarStridx(int idx) {
    simploc_->val_ = idx;
  }

  dw_op GetOp() const {
    return simploc_->dwop_;
  }

  uint32 GetSize() const {
    return simploc_->opnds_.size();
  }

  void ClearOpnd() {
    simploc_->opnds_.clear();
  }

  void AddSimpLocOpnd(uint64 val) {
    simploc_->AddOpnd(val);
  }

  void Dump();
};

class DBGDieAttr {
 public:
  DBGDieKind kind_;
  dw_at dwattr_;
  dw_form dwform_;  // type for the attribute value
  union {
    int32 i;
    uint32 id;  // dieid when dwform_ is of DW_FORM_ref
    // strIdx when dwform_ is of DW_FORM_string
    int64 j;
    uint64 u;
    float f;
    double d;

    DBGExprLoc *ptr;
  } val_;

  uint32 SizeOf(DBGDieAttr *attr);
  DBGDieAttr(DBGDieKind k) : kind_(k), dwattr_(DW_AT_GNU_deleted), dwform_(DW_FORM_GNU_strp_alt) {
    val_.u = kDbgDefaultVal;
  }

  virtual ~DBGDieAttr() {}

  void AddSimpLocOpnd(uint64 val) {
    val_.ptr->AddSimpLocOpnd(val);
  }

  void ClearSimpLocOpnd() {
    val_.ptr->ClearOpnd();
  }

  void Dump(int indent);
};

class DBGDie {
 public:
  MIRModule *mod_;
  dw_tag tag_;
  uint32 id;  // starts from 1 which is root die cu_
  bool withchildren_;
  DBGDie *parent;
  DBGDie *sibling;
  DBGDie *firstchild;
  uint32 abbrevid_;  // id in .debug_abbrev
  uint32 tyidx_;     // for type TAG
  uint32 Offset;     // Dwarf CU relative offset
  uint32 Size;       // DIE Size in .debug_info
  MapleVector<DBGDieAttr *> attrvec_;
  MapleVector<DBGDie *> subdievec_;

  DBGDie(MIRModule *m, dw_tag tag);
  virtual ~DBGDie() {}
  void AddAttr(DBGDieAttr *attr) {
    attrvec_.push_back(attr);
  }

  DBGDieAttr *AddAttr(dw_at attr, dw_form form, uint64 val);
  DBGDieAttr *AddSimpLocAttr(dw_at at, dw_form form, uint64 val);
  DBGDieAttr *AddGlobalLocAttr(dw_at at, dw_form form, uint64 val);
  DBGDieAttr *AddFrmBaseAttr(dw_at at, dw_form form, uint64 val);
  DBGExprLoc *GetExprLoc();
  bool SetAttr(dw_at attr, uint64 val);
  bool SetAttr(dw_at attr, int64 val);
  bool SetAttr(dw_at attr, uint32 val);
  bool SetAttr(dw_at attr, int32 val);
  bool SetAttr(dw_at attr, float val);
  bool SetAttr(dw_at attr, double val);
  bool SetSimpLocAttr(dw_at attr, int64 val);
  bool SetAttr(dw_at attr, DBGExprLoc *ptr);
  void ResetParentDie();
  void Dump(int indent);
};

class DBGAbbrevEntry {
 public:
  dw_tag tag_;
  uint32 abbrevid_;
  bool withchildren_;
  MapleVector<uint32> attrpairs_;  // kDwAt kDwForm pairs
  DBGAbbrevEntry(MIRModule *m, DBGDie *die);
  virtual ~DBGAbbrevEntry() {}
  bool Equalto(DBGAbbrevEntry *entry);
  void Dump(int indent);
};

class DBGAbbrevEntryVec {
 public:
  dw_tag tag_;
  MapleVector<DBGAbbrevEntry *> entryvec_;
  DBGAbbrevEntryVec(MIRModule *m, dw_tag tag) : tag_(tag), entryvec_(m->memPoolAllocator.Adapter()) {}

  virtual ~DBGAbbrevEntryVec() {}

  uint32 GetId(MapleVector<uint32> &attrs);
  void Dump(int indent);
};

class DebugInfo {
 public:
  MIRModule *mod_;
  DBGDie *cu_;            // root die: compilation unit
  DBGDie *dummytypedie_;  // workaround for unknown types
  MIRLexer *lexer_;
  uint32 maxid_;
  DBGBuilder *builder_;
  GStrIdx mplsrcidx_;
  uint32 debug_info_length_;

  // for compilation messages
  DBGCompileMsgInfo *compilemsg_;

  MapleStack<DBGDie *> parentdiestack_;
  MapleMap<uint32, DBGDie *> id_die_map_;
  MapleVector<DBGAbbrevEntry *> abbrev_vec_;  // valid entry starting from index 1
  MapleMap<uint32, DBGAbbrevEntryVec *> tag_abbrev_map_;

  // to be used when derived type references a base type die
  MapleMap<uint32, uint32> tyidx_dieid_map_;
  MapleMap<uint32, uint32> stridx_dieid_map_;
  MapleMap<uint32, uint32> typedef_tyidx_map_;  // prevtyidx_typidx_map
  MapleMap<uint32, uint32> pointed_pointer_map_;
  MapleMap<MIRFunction *, std::map<uint32, uint32>> func_lstridx_dieid_map_;
  MapleMap<uint32, uint32> funcdecl_dieid_map_;
  MapleMap<MIRFunction *, std::map<uint32, LabelIdx>> func_lstridx_labidx_map_;
  MapleSet<uint32> strps_;

 public:
  DebugInfo(MIRModule *m)
    : mod_(m),
      cu_(nullptr),
      dummytypedie_(nullptr),
      lexer_(nullptr),
      maxid_(1),
      builder_(nullptr),
      mplsrcidx_(0),
      debug_info_length_(0),
      compilemsg_(nullptr),
      parentdiestack_(m->memPoolAllocator.Adapter()),
      id_die_map_(std::less<uint32>(), m->memPoolAllocator.Adapter()),
      abbrev_vec_(m->memPoolAllocator.Adapter()),
      tag_abbrev_map_(std::less<uint32>(), m->memPoolAllocator.Adapter()),
      tyidx_dieid_map_(std::less<uint32>(), m->memPoolAllocator.Adapter()),
      stridx_dieid_map_(std::less<uint32>(), m->memPoolAllocator.Adapter()),
      typedef_tyidx_map_(std::less<uint32>(), m->memPoolAllocator.Adapter()),
      pointed_pointer_map_(std::less<uint32>(), m->memPoolAllocator.Adapter()),
      func_lstridx_dieid_map_(std::less<MIRFunction *>(), m->memPoolAllocator.Adapter()),
      funcdecl_dieid_map_(std::less<uint32>(), m->memPoolAllocator.Adapter()),
      func_lstridx_labidx_map_(std::less<MIRFunction *>(), m->memPoolAllocator.Adapter()),
      strps_(std::less<uint32>(), m->memPoolAllocator.Adapter()) {
    // valid entry starting from index 1 as abbrevid starting from 1 as well
    abbrev_vec_.push_back(nullptr);
    InitMsg();
  }

  virtual ~DebugInfo() {}

  void InitMsg() {
    compilemsg_ = mod_->memPool->New<DBGCompileMsgInfo>();
  }

  void UpdateMsg(uint32 lnum, const char *line) {
    compilemsg_->UpdateMsg(lnum, line);
  }

  void SetErrPos(uint32 lnum, uint32 cnum) {
    compilemsg_->SetErrPos(lnum, cnum);
  }

  void EmitMsg() {
    compilemsg_->EmitMsg();
  }

  DBGDie *GetDie(uint32 id) {
    return id_die_map_[id];
  }

  DBGDie *GetDie(const MIRFunction *func);

  void Init();
  void Finish();
  void SetupCU();
  void BuildAliasDIEs();
  void Dump(int indent);

  // build tree to populate withchildren_, sibling, firstchild
  // also insert DW_AT_sibling attributes when needed
  void BuildDieTree();

  // replace type idx with die id in DW_AT_type attributes
  void FillTypeAttrWithDieId();

  void BuildAbbrev();
  uint32 GetAbbrevId(DBGAbbrevEntryVec *, DBGAbbrevEntry *);

  void SetLocalDie(GStrIdx strIdx, const DBGDie *die);
  void SetLocalDie(MIRFunction *func, GStrIdx strIdx, const DBGDie *die);
  DBGDie *GetLocalDie(GStrIdx strIdx);
  DBGDie *GetLocalDie(MIRFunction *func, GStrIdx strIdx);

  LabelIdx GetLabelIdx(GStrIdx strIdx);
  LabelIdx GetLabelIdx(MIRFunction *func, GStrIdx strIdx);
  void SetLabelIdx(GStrIdx strIdx, LabelIdx idx);
  void SetLabelIdx(MIRFunction *func, GStrIdx strIdx, LabelIdx idx);

  DBGDie *GetParentDie() {
    return parentdiestack_.top();
  }

  void PushParentDie(DBGDie *die) {
    parentdiestack_.push(die);
  }

  void PopParentDie() {
    parentdiestack_.pop();
  }

  void ResetParentDie() {
    parentdiestack_.clear();
    parentdiestack_.push(cu_);
  }

  void SetTyidxDieIdMap(TyIdx tyIdx, const DBGDie *die) {
    tyidx_dieid_map_[tyIdx.GetIdx()] = die->id;
  }

  DBGDieAttr *CreateAttr(dw_at attr, dw_form form, uint64 val);

  DBGDie *CreateFormalParaDie(MIRType *type, GStrIdx nameidx, uint32 lnum);
  DBGDie *CreateVarDie(MIRSymbol *sym, uint32 lnum);
  DBGDie *GetOrCreateLabelDie(LabelIdx labid);
  DBGDie *GetOrCreateTypeAttrDie(MIRSymbol *sym);
  DBGDie *GetOrCreateConstTypeDie(TypeAttrs attr, DBGDie *typedie);
  DBGDie *GetOrCreateVolatileTypeDie(TypeAttrs attr, DBGDie *typedie);
  DBGDie *GetOrCreateFuncDeclDie(MIRFunction *func, uint32 lnum);
  DBGDie *GetOrCreateFuncDefDie(MIRFunction *func, uint32 lnum);
  DBGDie *GetOrCreatePrimTypeDie(PrimType pty);
  DBGDie *GetOrCreateTypeDie(MIRType *type);
  DBGDie *GetOrCreatePointTypeDie(const MIRPtrType *type);
  DBGDie *GetOrCreateArrayTypeDie(const MIRArrayType *type);
  DBGDie *GetOrCreateFieldDie(maple::FieldPair pair, uint32 lnum);
  DBGDie *InitClassTypeDie(GStrIdx strIdx, uint32 lnum);
  void BuildStructTypeDie(GStrIdx strIdx, const MIRStructType *classtype, DBGDie *&die);
  void BuildClassTypeDie(GStrIdx strIdx, const MIRClassType *classtype, DBGDie *&die);
  void BuildInterfaceTypeDie(GStrIdx strIdx, MIRInterfaceType *classtype, DBGDie *&die);

  // Functions for calculating the size and offset of each DW_TAG_xxx and DW_AT_xxx
  void ComputeSizeAndOffsets();
  void ComputeSizeAndOffset(DBGDie *die, uint32 &offset);
};

}  // namespace maple

#endif  // MAPLE_IR_INCLUDE_DBG_INFO_H
