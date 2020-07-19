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

#ifndef MAPLE_IR_INCLUDE_BIN_MPL_IMPORT_H
#define MAPLE_IR_INCLUDE_BIN_MPL_IMPORT_H
#include "mir_module.h"
#include "mir_nodes.h"
#include "mir_preg.h"
#include "parser_opt.h"
#include "mir_builder.h"

namespace maple {
class BinaryMplImport {
  using CallSite = std::pair<CallInfo*, PUIdx>;

 public:
  BinaryMplImport &operator = (const BinaryMplImport &) = delete;
  BinaryMplImport(MIRModule &md) : mod(md), _mirbuilder(&md) {
    importing_from_mplt = false;
    imported = true;
    bufI = 0;
    in_cg = false;
    in_ipa = false;
  }

  virtual ~BinaryMplImport() {}

  bool Import(const std::string &modid, bool readSymbols = false, bool readSe = false);
  void ReadContentField();
  void ReadStrField();
  void ReadHeaderField();
  void ReadTypeField();
  void ReadSymField();
  void ReadSymTabField();
  void ReadCgField();
  void Jump2NextField();
  void ReadSeField();
  CallInfo *InCallInfo();
  void MergeDuplicated(PUIdx methodPuidx, std::vector<CallSite> &targetSet, std::vector<CallSite> &newSet);
  void Reset();
  MIRSymbol *GetOrCreateSymbol(TyIdx tyIdx, GStrIdx strIdx, MIRSymKind mclass, MIRStorageClass storageClass,
                               MIRFunction *func, uint8 scpid);
  MIRType *InsertInTypeTables(MIRType *ptype);
  void SetupEhRootType();
  void UpdateMethodSymbols();
  void ImportConstBase(MIRConstKind &kind, MIRType *&type, uint32 &fieldID);
  MIRConst *ImportConst(MIRFunction *func);
  GStrIdx ImportStr();
  UStrIdx ImportUsrStr();
  TyIdx ImportType();
  void ImportTypeBase(PrimType &primtype, GStrIdx &strIdx, bool &nameIsLocal);
  void InSymTypeTable();
  void ImportTypePairs(std::vector<TypePair> &tpairs);
  TypeAttrs ImportTypeAttrs();
  MIRPragmaElement *ImportPragmaElement();
  MIRPragma *ImportPragma();
  void ImportFieldPair(FieldPair &fp);
  void ImportMethodPair(MethodPair &mp);
  void ImportFieldsOfStructType(FieldVector &fields);
  void ImportMethodsOfStructType(MethodVector &Methods);
  void ImportStructTypeData(MIRStructType *type);
  void ImportInterfacesOfClassType(std::vector<TyIdx> &interfaces);
  void ImportInfoIsStringOfClassType(std::vector<bool> &infoIsString);
  void ImportInfoOfClassType(std::vector<bool> &infoIsString, std::vector<MIRInfoPair> &infos);
  void ImportPragmaOfClassType(std::vector<MIRPragma*> &pragmas);
  void SetClassTyidxOfMethods(MIRStructType *type);
  void ImportClassTypeData(MIRClassType *type);
  void ImportInterfaceTypeData(MIRInterfaceType *type);
  MIRSymbol *InSymbol(MIRFunction *func);
  void ImportLocalWords(MIRFunction *func);
  void ImportFormalWords(MIRFunction *func);
  void ImportInfoVector(MIRInfoVector &infovector, MapleVector<bool> &infovector_isstring);
  void ImportLocalTypeNameTable(MIRTypeNameTable *typeNameTab);
  /**/ void ImportSymTab(MIRFunction *func);
  PUIdx ImportFunction();
  void ImportFuncIdInfo(MIRFunction *func);
  void ImportLocalSymbol(MIRFunction *func);
  void ImportLocalSymTab(MIRFunction *func);
  void ImportPregTab(MIRFunction *func);
  void ImportLabelTab(MIRFunction *func);
  void ImportFormalsStIdx(MIRFunction *func);
  void ImportAliasMap(MIRFunction *func);
  void ImportSrcPos(SrcPosition &pos);
  void ImportBaseNode(Opcode &o, PrimType &typ, uint8 &numopr);
  PUIdx ImportFuncViaSymName();
  BaseNode *ImportExpression(MIRFunction *func);

  void ImportReturnValues(MIRFunction *func, CallReturnVector *retv);
  BlockNode *ImportBlockNode(MIRFunction *fn);
  void ReadFunctionBodyField();

  void ReadFileAt(const std::string &modid, int32 offset);

  uint8 Read();
  int32 ReadInt();
  int64 ReadInt64();
  void ReadAsciiStr(std::string &str);
  int64 ReadNum();

  int32 GetIPAFileIndex(std::string &name);

  void SkipTotalSize();

 public:
  uint64 bufI;
  std::vector<uint8> buf;
  std::map<int64, int32> content;
  bool imported;  // used only by irbuild to convert to ascii
 private:
  std::map<GStrIdx, int64> type_table_cg;
  std::map<GStrIdx, int64> sym_table_cg;
  MIRModule &mod;
  MIRBuilder _mirbuilder;
  std::vector<GStrIdx> gstr_tab;
  std::vector<UStrIdx> ustr_tab;
  std::vector<MIRType *> typ_tab;
  std::vector<MIRFunction *> func_tab;
  std::vector<MIRSymbol *> sym_tab;
  std::vector<CallInfo *> callinfo_tab;
  std::vector<MIRSymbol *> method_symbols;     // TODO merge with sym_tab?
  std::map<TyIdx, TyIdx> typedef_idx_map;  // map previous declared tyIdx
  std::vector<bool> _defined_labels;
  std::string import_file_name;

 public:
  bool importing_from_mplt; // decided based on magic number
  bool in_cg;
  bool in_ipa;
};
}  // namespace maple
#endif
