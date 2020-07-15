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

#ifndef MAPLEBE_INCLUDE_CG_EMIT_H
#define MAPLEBE_INCLUDE_CG_EMIT_H

#include "isa.h"
#include "lsda.h"
#include "asm_info.h"
#include "cg.h"
#include "cg_assert.h"

/// Maple IR headers
#include "mir_module.h"
#include "mir_const.h"
#include "debug_info.h"

#include "mempool_allocator.h"

/// C++ headers
#include <iostream>
#include <fstream>
#include <functional>
#include <climits>
#include <map>

#if TARGARK
class MirGenerator;
#endif

namespace maple {

class DebugInfo;

}

namespace maplebe {

// forward declaration
class BECommon;
class CG;

class StructEmitInfo {
 public:
  // Next field offset in struct.
  uint8 nextfieldOffset_;
  uint8 combinebfldwidth_;
  uint64 combinebfldvalue_;
  // Total size emitted in current struct.
  uint64 tsize_;

 public:
  // default ctor
  StructEmitInfo() : nextfieldOffset_(0), combinebfldwidth_(0), combinebfldvalue_(0), tsize_(0) {}

  ~StructEmitInfo() {}
};

class Emitter {
 public:
  CG *cg_;
#if TARGARK
  MirGenerator *mirg_;
#endif

 public:
  const Asminfo *asminfo_;
  std::ofstream out;
  MemPool *memPool;

 private:
  MOperator curr_mop;
  MapleMap<DBGDie *, LabelIdx> labdie2labidx_table;

 public:
  Emitter(CG *cg, const std::string &asmFileName)
      : cg_(cg),
        curr_mop(UINT_MAX),
        labdie2labidx_table(std::less<DBGDie *>(), cg->mirModule->memPoolAllocator.Adapter()) {
    out.open(asmFileName.c_str(), std::ios::trunc);
    MIRModule &mirModule = *cg_->mirModule;
    asminfo_ = mirModule.memPool->New<Asminfo>(0, mirModule.memPool);
    memPool = mirModule.memPool;
  }

  ~Emitter() {}

  void CloseOutput() {
    out.close();
  }

  inline MOperator GetCurrentMOP() const {
    return curr_mop;
  }

  inline void SetCurrentMOP(MOperator mop) {
    curr_mop = mop;
  }

  void EmitAsmLabel(Asmlabel al);
  void EmitAsmLabel(const MIRSymbol *st, Asmlabel al);
  void EmitFileInfo(const std::string &fileName);
  void EmitBlockMarker(const char *markerName);  // a symbol start/end a block
  void EmitBlockMarkerWithAddr(const char *markerName, const std::string &addrName);
  void EmitNullConstant(uint32 size);
  void EmitCombineBfldValue(StructEmitInfo *semitinfo);
  void EmitBitFieldConstant(StructEmitInfo *semitinfo, MIRConst *ct, const MIRType *nety, uint32 fieldoffset);
  void EmitScalarConstant(MIRConst *ct, bool newline, bool flag32);
  void EmitStrConstant(MIRStrConst *ct);
  void EmitStr16Constant(MIRStr16Const *ct);
  void EmitConstantTable(MIRSymbol *st, MIRConst *ct, const std::map<GStrIdx, MIRType *> &stridx2type);
  void EmitClassinfoSequential(MIRSymbol *st, const std::map<GStrIdx, MIRType *> &stridx2type,
                               const std::string &sectionName);
  void EmitClassinfoSequential(MIRSymbol *st1, MIRSymbol *st2, const std::map<GStrIdx, MIRType *> &stridx2type);
  void EmitLiterals(std::vector<std::pair<MIRSymbol *, bool>> &literals,
                    const std::map<GStrIdx, MIRType *> &stridx2type);
  void EmitFuncLayoutInfo(const MIRSymbol *layout);
  void EmitGlobalVars(std::vector<std::pair<MIRSymbol *, bool>> &globalvars);
  void EmitGlobalVar(MIRSymbol *globalvar);
  void EmitStaticFields(std::vector<MIRSymbol *> &fields);
  void EmitLiteral(MIRSymbol *literal, const std::map<GStrIdx, MIRType *> &stridx2type);
  void EmitMetaDataSymbols(std::vector<MIRSymbol *> &symV, std::map<GStrIdx, MIRType *> &stridx2type,
                           std::string prefixStr);
  void GetHotandColdMetaSymbolinfo(std::vector<MIRSymbol *> &symVIn, std::vector<MIRSymbol *> &hotfieldinfoStV,
                                   std::vector<MIRSymbol *> &coldfieldinfoStV, std::string prefixStr);
  void EmitMetaDataSymbolWithMarkFlag(std::vector<MIRSymbol *> &symV,
                                      const std::map<GStrIdx, MIRType *> &stridx2type,
                                      std::string prefixStr, bool isHotFlag);
  void MarkVtabOrItabEndFlag(std::vector<MIRSymbol *> &symV, std::vector<MIRSymbol *> &hotSymV);
  void EmitArrayConstant(MIRConst *ct);
  void EmitStructConstant(MIRConst *ct);
  void EmitGlobalVariable();
  void EmitMplPersonalityV0();
  void EmitGlobalRootList(const MIRSymbol *st);
  void EmitMuidTable(const std::vector<MIRSymbol *> &ve, const std::map<GStrIdx, MIRType *> &stridx2type,
                     const std::string &sectionName);

  Emitter &Emit(int64 val) {
    out << val;
    return *this;
  }

  Emitter &Emit(const char *str) {
    CG_ASSERT(str, "null string to emit?");
    out << str;
    return *this;
  }

  Emitter &Emit(const std::string &str) {
    out << str.c_str();
    return *this;
  }

  void EmitLabelRef(const char *, LabelIdx);
  void EmitStmtLabel(const char *, LabelIdx);
  void EmitLabelPair(const char *, const LabelPair &);

  // Emit alignment directive (".align")
  //
  // The location counter is padded to (1<<pow2) bytes.  This function will
  // handle the differences between platforms.
  void EmitAlignDirective(int pow2);

  // Emit a label (label + ":")
  void EmitLabel(const char *label);

  // Emit signed/unsigned integer literals in decimal or hexadecimal
  void EmitDecSigned(int64 num);
  void EmitHexSigned(int64 num);
  void EmitDecUnsigned(uint64 num);
  void EmitHexUnsigned(uint64 num);

  // debug info
  void FillInClassByteSize(DBGDie *die, DBGDieAttr *byteSizeAttr, DBGAbbrevEntry *diae);
  void SetupDBGInfo(DebugInfo *);
  void ApplyInPrefixOrder(DBGDie *die, const std::function<void(DBGDie *)> &func);

  void AddLabelDieToLabelIdxMapping(DBGDie *, LabelIdx);
  LabelIdx GetLabelIdxForLabelDie(DBGDie *);
  void EmitDIHeader();
  void EmitCFISectionNames(const char *const names[]);
  void EmitDIFooter();
  void EmitDIHeaderFileInfo();
  void EmitDIDebugInfoSection(DebugInfo *);
  void EmitDIDebugAbbrevSection(DebugInfo *);
  void EmitDIDebugARangesSection();
  void EmitDIDebugRangesSection();
  void EmitDIDebugLineSection();
  void EmitDIDebugStrSection();

  void EmitDIFormSpecification(const DBGDieAttr *attr) {
    EmitDIFormSpecification(attr->dwform_);
  }

  void EmitDIFormSpecification(unsigned int dwform);

  void EmitDIAttrValue(DBGDie *die, DBGDieAttr *attr, dw_at attrName, dw_tag tagName, DebugInfo *di);

  // GC header for primordial objects
  void EmitGCHeader();

 private:
  Asmlabel GetTypeAsmInfoName(PrimType pty);
  void EmitTypeInfo(const std::vector<MIRSymbol *> &, const std::vector<MIRSymbol *> &) const;
  void EmitDWRef(const char *name);
  void EmitDummyReflectionInfo(const char *name);
};

}  // namespace maplebe

#endif
