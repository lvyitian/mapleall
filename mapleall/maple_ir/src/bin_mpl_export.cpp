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

#include "bin_mpl_export.h"
#include <sstream>
#include <vector>
#include "mir_function.h"
#include "name_mangler.h"
#include "opcode_info.h"
#include "mir_pragma.h"
#include "bin_mplt.h"
#include "factory.h"

namespace {

using namespace maple;

using OutputConstFactory = FunctionFactory<MIRConstKind, void, MIRConst*, BinaryMplExport*>;
using OutputTypeFactory = FunctionFactory<MIRTypeKind, void, MIRType*, BinaryMplExport*, bool>;

void OutputConstInt(MIRConst *constVal, BinaryMplExport *mplExport) {
  mplExport->WriteNum(kBinKindConstInt);
  mplExport->OutputConstBase(constVal);
  mplExport->WriteNum(static_cast<MIRIntConst*>(constVal)->value);
}

void OutputConstAddrof(MIRConst *constVal, BinaryMplExport *mplExport) {
  MIRAddrofConst *addrof = static_cast<MIRAddrofConst*>(constVal);
  if (addrof->GetSymbolIndex().IsGlobal()) {
    mplExport->WriteNum(kBinKindConstAddrof);
  } else {
    mplExport->WriteNum(kBinKindConstAddrofLocal);
  }
  mplExport->OutputConstBase(constVal);
  if (addrof->GetSymbolIndex().IsGlobal()) {
    mplExport->OutputSymbol(mplExport->GetMIRModule().CurFunction()->GetLocalOrGlobalSymbol(addrof->GetSymbolIndex()));
  } else {
    mplExport->WriteNum(addrof->GetSymbolIndex().FullIdx());
  }
  mplExport->WriteNum(addrof->GetFieldID());
  mplExport->WriteNum(addrof->GetOffset());
}

void OutputConstAddrofFunc(MIRConst *constVal, BinaryMplExport *mplExport) {
  mplExport->WriteNum(kBinKindConstAddrofFunc);
  mplExport->OutputConstBase(constVal);
  MIRAddroffuncConst *addrfunc = static_cast<MIRAddroffuncConst*>(constVal);
  mplExport->OutputFunction(addrfunc->GetValue());
}

void OutputConstLbl(MIRConst *constVal, BinaryMplExport *mplExport) {
  mplExport->WriteNum(kBinKindConstAddrofLabel);
  mplExport->OutputConstBase(constVal);
  MIRLblConst *lblConst = static_cast<MIRLblConst *>(constVal);
  mplExport->WriteNum(lblConst->value);  // LabelIdx
  // not needed to output puIdx
}

void OutputConstStr(MIRConst *constVal, BinaryMplExport *mplExport) {
  mplExport->WriteNum(kBinKindConstStr);
  mplExport->OutputConstBase(constVal);
  MIRStrConst *strc = static_cast<MIRStrConst*>(constVal);
  mplExport->OutputUsrStr(strc->value);
}

void OutputConstStr16(MIRConst *constVal, BinaryMplExport *mplExport) {
  mplExport->WriteNum(kBinKindConstStr16);
  mplExport->OutputConstBase(constVal);
  MIRStr16Const *mirStr16 = static_cast<MIRStr16Const*>(constVal);
  std::u16string str16 = GlobalTables::GetU16StrTable().GetStringFromStrIdx(U16StrIdx(mirStr16->value.GetIdx()));
  std::string str;
  NameMangler::UTF16ToUTF8(str, str16);
  mplExport->WriteNum(str.length());
  for (uint64 i = 0; i < str.length(); i++) {
    mplExport->Write((uint8)str[i]);
  }
}

void OutputConstFloat(MIRConst *constVal, BinaryMplExport *mplExport) {
  mplExport->WriteNum(kBinKindConstFloat);
  mplExport->OutputConstBase(constVal);
  MIRFloatConst *fconst = static_cast<MIRFloatConst*>(constVal);
  mplExport->WriteNum(fconst->GetIntValue());
}

void OutputConstDouble(MIRConst *constVal, BinaryMplExport *mplExport) {
  mplExport->WriteNum(kBinKindConstDouble);
  mplExport->OutputConstBase(constVal);
  MIRDoubleConst *dconst = static_cast<MIRDoubleConst*>(constVal);
  mplExport->WriteNum(dconst->GetIntValue());
}

void OutputConstAgg(MIRConst *constVal, BinaryMplExport *mplExport) {
  mplExport->WriteNum(kBinKindConstAgg);
  mplExport->OutputConstBase(constVal);
  MIRAggConst *aggConst = static_cast<MIRAggConst*>(constVal);
  size_t size = aggConst->constVec.size();
  mplExport->WriteNum(size);
  for (size_t i = 0; i < size; i++) {
    mplExport->OutputConst(aggConst->constVec[i]);
  }
}

void OutputConstSt(MIRConst *constVal, BinaryMplExport *mplExport) {
  mplExport->WriteNum(kBinKindConstSt);
  mplExport->OutputConstBase(constVal);
  MIRStConst *stConst = static_cast<MIRStConst*>(constVal);
  size_t size = stConst->stVec.size();
  mplExport->WriteNum(size);
  for (size_t i = 0; i < size; i++) {
    mplExport->OutputSymbol(stConst->stVec[i]);
  }
  size = stConst->stOffsetVec.size();
  mplExport->WriteNum(size);
  for (size_t i = 0; i < size; i++) {
    mplExport->WriteNum(stConst->stOffsetVec[i]);
  }
}

void InitOutputConstFactory() {
  RegisterFactoryFunction<OutputConstFactory>(kConstInt, OutputConstInt);
  RegisterFactoryFunction<OutputConstFactory>(kConstAddrof, OutputConstAddrof);
  RegisterFactoryFunction<OutputConstFactory>(kConstAddrofFunc, OutputConstAddrofFunc);
  RegisterFactoryFunction<OutputConstFactory>(kConstLblConst, OutputConstLbl);
  RegisterFactoryFunction<OutputConstFactory>(kConstStrConst, OutputConstStr);
  RegisterFactoryFunction<OutputConstFactory>(kConstStr16Const, OutputConstStr16);
  RegisterFactoryFunction<OutputConstFactory>(kConstFloatConst, OutputConstFloat);
  RegisterFactoryFunction<OutputConstFactory>(kConstDoubleConst, OutputConstDouble);
  RegisterFactoryFunction<OutputConstFactory>(kConstAggConst, OutputConstAgg);
  RegisterFactoryFunction<OutputConstFactory>(kConstStConst, OutputConstSt);
}

void OutputTypeScalar(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  mplExport->WriteNum(kBinKindTypeScalar);
  mplExport->OutputTypeBase(ty);
}

void OutputTypePointer(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRPtrType *type = static_cast<MIRPtrType*>(ty);
  mplExport->WriteNum(kBinKindTypePointer);
  mplExport->OutputTypeBase(type);
  mplExport->OutputType(type->pointedTyIdx, canUseTypename);
  mplExport->OutputTypeAttrs(type->typeAttrs);
}

void OutputTypeByName(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  mplExport->WriteNum(kBinKindTypeByName);
  mplExport->OutputTypeBase(ty);
}

void OutputTypeFArray(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRFarrayType *type = static_cast<MIRFarrayType*>(ty);
  mplExport->WriteNum(kBinKindTypeFArray);
  mplExport->OutputTypeBase(type);
  mplExport->OutputType(type->elemTyIdx, canUseTypename);
}

void OutputTypeJArray(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRJarrayType *type = static_cast<MIRJarrayType*>(ty);
  mplExport->WriteNum(kBinKindTypeJarray);
  mplExport->OutputTypeBase(type);
  mplExport->OutputType(type->elemTyIdx, canUseTypename);
}

void OutputTypeArray(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRArrayType *type = static_cast<MIRArrayType*>(ty);
  mplExport->WriteNum(kBinKindTypeArray);
  mplExport->OutputTypeBase(type);
  mplExport->WriteNum(type->GetDim());
  for (int i = 0; i < type->GetDim(); i++) {
    mplExport->WriteNum(type->sizeArray[i]);
  }
  mplExport->OutputType(type->eTyIdx, canUseTypename);
  mplExport->OutputTypeAttrs(type->typeAttrs);
}

void OutputTypeFunction(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRFuncType *type = static_cast<MIRFuncType*>(ty);
  mplExport->WriteNum(kBinKindTypeFunction);
  mplExport->OutputTypeBase(type);
  mplExport->OutputType(type->retTyIdx, canUseTypename);
  mplExport->WriteNum(type->isVarArgs);
  size_t size = type->paramTypeList.size();
  mplExport->WriteNum(size);
  for (size_t i = 0; i < size; i++) {
    mplExport->OutputType(type->paramTypeList[i], canUseTypename);
  }
  size = type->paramAttrsList.size();
  mplExport->WriteNum(size);
  for (size_t i = 0; i < size; i++) {
    mplExport->OutputTypeAttrs(type->paramAttrsList[i]);
  }
}

void OutputTypeParam(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRTypeParam *type = static_cast<MIRTypeParam*>(ty);
  mplExport->WriteNum(kBinKindTypeParam);
  mplExport->OutputTypeBase(type);
}

void OutputTypeInstantVector(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRInstantVectorType *type = static_cast<MIRInstantVectorType*>(ty);
  mplExport->WriteNum(kBinKindTypeInstantVector);
  mplExport->OutputTypeBase(type);
  mplExport->WriteNum(ty->GetKind());
  mplExport->OutputTypePairs(type);
}

void OutputTypeGenericInstant(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRGenericInstantType *type = static_cast<MIRGenericInstantType*>(ty);
  mplExport->WriteNum(kBinKindTypeGenericInstant);
  mplExport->OutputTypeBase(type);
  mplExport->OutputTypePairs(type);
  mplExport->OutputType(type->genericTyIdx, canUseTypename);
}

void OutputTypeBitField(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRBitfieldType *type = static_cast<MIRBitfieldType*>(ty);
  mplExport->WriteNum(kBinKindTypeBitField);
  mplExport->OutputTypeBase(type);
  mplExport->WriteNum(type->fieldSize);
}

// for Struct/StructIncomplete/Union
void OutputTypeStruct(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRStructType *type = static_cast<MIRStructType*>(ty);
  mplExport->WriteNum(kBinKindTypeStruct);
  mplExport->OutputTypeBase(type);
  MIRTypeKind kind = ty->GetKind();
  mplExport->WriteNum(kind);
  if (kind != kTypeStructIncomplete) {
    mplExport->OutputStructTypeData(type);
  }
}

void OutputTypeClass(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRClassType *type = static_cast<MIRClassType*>(ty);
  mplExport->WriteNum(kBinKindTypeClass);
  mplExport->OutputTypeBase(type);
  MIRTypeKind kind = ty->GetKind();
  mplExport->WriteNum(kind);
  if (kind != kTypeClassIncomplete) {
    mplExport->OutputStructTypeData(type);
    mplExport->OutputClassTypeData(type);
  }
}

void OutputTypeInterface(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  MIRInterfaceType *type = static_cast<MIRInterfaceType*>(ty);
  mplExport->WriteNum(kBinKindTypeInterface);
  mplExport->OutputTypeBase(type);
  MIRTypeKind kind = ty->GetKind();
  mplExport->WriteNum(kind);
  if (kind != kTypeInterfaceIncomplete) {
    mplExport->OutputStructTypeData(type);
    mplExport->OutputInterfaceTypeData(type);
  }
}

void OutputTypeConstString(MIRType *ty, BinaryMplExport *mplExport, bool canUseTypename) {
  ASSERT(false, "Type's kind not yet implemented: %d", ty->GetKind());
}

void InitOutputTypeFactory() {
  RegisterFactoryFunction<OutputTypeFactory>(kTypeScalar, OutputTypeScalar);
  RegisterFactoryFunction<OutputTypeFactory>(kTypePointer, OutputTypePointer);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeByName, OutputTypeByName);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeFArray, OutputTypeFArray);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeJArray, OutputTypeJArray);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeArray, OutputTypeArray);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeFunction, OutputTypeFunction);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeParam, OutputTypeParam);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeInstantVector, OutputTypeInstantVector);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeGenericInstant, OutputTypeGenericInstant);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeBitField, OutputTypeBitField);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeStruct, OutputTypeStruct);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeStructIncomplete, OutputTypeStruct);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeUnion, OutputTypeStruct);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeClass, OutputTypeClass);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeClassIncomplete, OutputTypeClass);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeInterface, OutputTypeInterface);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeInterfaceIncomplete, OutputTypeInterface);
  RegisterFactoryFunction<OutputTypeFactory>(kTypeConstString, OutputTypeConstString);
}
};  // namespace

namespace maple {

int BinaryMplExport::typeMarkOffset = 0;

BinaryMplExport::BinaryMplExport(MIRModule &md) : mod(md), func2SE_map(nullptr) {
  bufI = 0;
  Init();
  InitOutputConstFactory();
  InitOutputTypeFactory();
  in_ipa = false;
  not2mplt = false;
}

uint8 BinaryMplExport::Read() {
  CHECK_FATAL(bufI < buf.size(), "Index out of bound in BinaryMplImport::Read()");
  return buf[bufI++];
}

/* Little endian */
int32 BinaryMplExport::ReadInt() {
  uint32 x0 = static_cast<uint32>(Read());
  uint32 x1 = static_cast<uint32>(Read());
  uint32 x2 = static_cast<uint32>(Read());
  uint32 x3 = static_cast<uint32>(Read());
  int32 x = static_cast<int32>((((((x3 << 8) + x2) << 8) + x1) << 8) + x0);
  return x;
}

void BinaryMplExport::Write(uint8 b) {
  buf.push_back(b);
}

/* Little endian */
void BinaryMplExport::WriteInt(int32 x) {
  Write(static_cast<uint8>(static_cast<uint32>(x) & 0xFF));
  Write(static_cast<uint8>((static_cast<uint32>(x) >> 8) & 0xFF));
  Write(static_cast<uint8>((static_cast<uint32>(x) >> 16) & 0xFF));
  Write(static_cast<uint8>((static_cast<uint32>(x) >> 24) & 0xFF));
}

void BinaryMplExport::ExpandFourBuffSize() {
  WriteInt(0);
}

void BinaryMplExport::Fixup(size_t i, int32 x) {
  constexpr int fixupCount = 4;
  CHECK(i <= buf.size() - fixupCount, "Index out of bound in BinaryMplImport::Fixup()");
  buf[i] = static_cast<uint8>(static_cast<uint32>(x) & 0xFF);
  buf[i + 1] = static_cast<uint8>((static_cast<uint32>(x) >> 8) & 0xFF);
  buf[i + 2] = static_cast<uint8>((static_cast<uint32>(x) >> 16) & 0xFF);
  buf[i + 3] = static_cast<uint8>((static_cast<uint32>(x) >> 24) & 0xFF);
}

void BinaryMplExport::WriteInt64(int64 x) {
  WriteInt(static_cast<int32>(static_cast<uint64>(x) & 0xFFFFFFFF));
  WriteInt(static_cast<int32>((static_cast<uint64>(x) >> 32) & 0xFFFFFFFF));
}

/* LEB128 */
void BinaryMplExport::WriteNum(int64 x) {
  while (x < -0x40 || x >= 0x40) {
    Write(static_cast<uint8>((static_cast<uint64>(x) & 0x7F) + 0x80));
    x = x >> 7; // This is a compress algorithm, do not cast int64 to uint64. If do so, small negtivate number like -3
                // will occupy 9 bits and we will not get the compressed benefit.
  }
  Write(static_cast<uint8>(static_cast<uint64>(x) & 0x7F));
}

void BinaryMplExport::WriteAsciiStr(const std::string &str) {
  WriteNum(str.size());
  for (size_t i = 0; i < str.size(); i++) {
    Write(static_cast<uint8>(str[i]));
  }
}

void BinaryMplExport::DumpBuf(const std::string &name) {
  FILE *f = fopen(name.c_str(), "wb");
  if (f == nullptr) {
    LogInfo::MapleLogger(kLlErr) << "Error while creating the binary file: " << name << '\n';
    FATAL(kLncFatal, "Error while creating the binary file: %s\n", name.c_str());
  }
  size_t size = buf.size();
  size_t k = fwrite(&buf[0], sizeof(uint8), size, f);
  fclose(f);
  if (k != size) {
    LogInfo::MapleLogger(kLlErr) << "Error while writing the binary file: " << name << '\n';
  }
}

void BinaryMplExport::OutputConstBase(const MIRConst *c) {
  WriteNum(c->kind);
  OutputTypeViaTypeName(c->type->tyIdx);
  WriteNum(c->fieldID);
}

void BinaryMplExport::OutputConst(MIRConst *constVal) {
  if (constVal == nullptr) {
    WriteNum(0);
  } else {
    auto func = CreateProductFunction<OutputConstFactory>(constVal->kind);
    if (func != nullptr) {
      func(constVal, this);
    }
  }
}

void BinaryMplExport::OutputStr(GStrIdx gstr) {
  if (gstr == 0) {
    WriteNum(0);
    return;
  }

  auto it = gStrMark.find(gstr);
  if (it != gStrMark.end()) {
    WriteNum(-(it->second));
    return;
  }

  size_t mark = gStrMark.size();
  gStrMark[gstr] = mark;
  WriteNum(kBinString);
  WriteAsciiStr(GlobalTables::GetStrTable().GetStringFromStrIdx(gstr.GetIdx()));
}

void BinaryMplExport::OutputUsrStr(UStrIdx ustr) {
  if (ustr == 0) {
    WriteNum(0);
    return;
  }

  auto it = uStrMark.find(ustr);
  if (it != uStrMark.end()) {
    WriteNum(-(it->second));
    return;
  }

  size_t mark = uStrMark.size();
  uStrMark[ustr] = mark;
  WriteNum(kBinUsrString);
  WriteAsciiStr(GlobalTables::GetUStrTable().GetStringFromStrIdx(ustr));
}

void BinaryMplExport::OutputPragmaElement(MIRPragmaElement *e) {
  OutputStr(e->namestridx_);
  OutputStr(e->typestridx_);
  WriteNum(e->type_);

  if (e->type_ == kValueString || e->type_ == kValueType || e->type_ == kValueField || e->type_ == kValueMethod ||
      e->type_ == kValueEnum) {
    OutputStr(GStrIdx(e->val_.i));
  } else {
    WriteInt64(e->val_.u);
  }
  int64 size = e->subelemvec_.size();
  WriteNum(size);
  for (int64 i = 0; i < size; i++) {
    OutputPragmaElement(e->subelemvec_[i]);
  }
}

void BinaryMplExport::OutputPragma(MIRPragma *p) {
  WriteNum(p->pragmaKind);
  WriteNum(p->visibility);
  OutputStr(p->strIdx);
  OutputType(p->tyIdx, false);
  OutputType(p->tyIdxEx, false);
  WriteNum(p->paramNum);
  size_t size = p->elementVec.size();
  WriteNum(size);
  for (int64 i = 0; i < size; i++) {
    OutputPragmaElement(p->elementVec[i]);
  }
}

void BinaryMplExport::OutputTypeBase(const MIRType *type) {
  WriteNum(type->primType);
  OutputStr(type->nameStrIdx);
  WriteNum(type->nameIsLocal);
}

void BinaryMplExport::OutputFieldPair(const FieldPair &fp) {
  OutputStr(fp.first);          // GStrIdx
  OutputType(fp.second.first, false);  // TyIdx
  FieldAttrs fa = fp.second.second;
  WriteNum(fa.attrFlag);
  WriteNum(fa.attrAlign);
  if (fa.GetAttr(FLDATTR_static) && fa.GetAttr(FLDATTR_final) &&
      (fa.GetAttr(FLDATTR_public) || fa.GetAttr(FLDATTR_protected))) {
    std::string fieldname = GlobalTables::GetStrTable().GetStringFromStrIdx(fp.first);
    MIRSymbol *fieldVar = GetMIRModule().mirBuilder->GetGlobalDecl(fieldname);
    if (fieldVar && dynamic_cast<MIRStr16Const *>(fieldVar->value.konst)) {
      WriteNum(kBinInitConst);
      fieldVar->value.konst->type = fieldVar->GetType();
      OutputConst(fieldVar->value.konst);
    } else {
      WriteNum(0);
    }
  }
}

void BinaryMplExport::OutputMethodPair(const MethodPair &mp) {
  // use GStrIdx instead, StIdx will be created by InMethodPair
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(mp.first.Idx());
  WriteAsciiStr(GlobalTables::GetStrTable().GetStringFromStrIdx(funcSt->GetNameStridx()));
  OutputType(mp.second.first, false);              // TyIdx
  WriteNum(mp.second.second.attrFlag);  // FuncAttrs
}

void BinaryMplExport::OutputFieldsOfStruct(const FieldVector &fields) {
  WriteNum(fields.size());
  for (const FieldPair &fp : fields) {
    OutputFieldPair(fp);
  }
}

void BinaryMplExport::OutputMethodsOfStruct(const MethodVector &methods) {
  WriteNum(methods.size());
  for (const MethodPair &memPool : methods) {
    OutputMethodPair(memPool);
  }
}

void BinaryMplExport::OutputStructTypeData(MIRStructType *type) {
  CHECK_FATAL(type != nullptr, "MIRStructType is null.");
  OutputFieldsOfStruct(type->fields);
  OutputFieldsOfStruct(type->staticFields);
  OutputFieldsOfStruct(type->parentFields);
  OutputMethodsOfStruct(type->methods);
}

void BinaryMplExport::OutputImplementedInterfaces(const std::vector<TyIdx> &interfaces) {
  WriteNum(interfaces.size());
  for (const TyIdx &tyIdx : interfaces) {
    OutputType(tyIdx, false);
  }
}

void BinaryMplExport::OutputInfoIsString(const std::vector<bool> &infoIsString) {
  WriteNum(infoIsString.size());
  for (bool isString : infoIsString) {
    WriteNum(isString);
  }
}

void BinaryMplExport::OutputInfo(const std::vector<MIRInfoPair> &info, const std::vector<bool> &infoIsString) {
  size_t size = info.size();
  WriteNum(size);
  for (size_t i = 0; i < size; i++) {
    OutputStr(info[i].first);  // GStrIdx
    if (infoIsString[i]) {
      OutputStr(GStrIdx(info[i].second));
    } else {
      WriteNum(info[i].second);
    }
  }
}

void BinaryMplExport::OutputPragmaVec(const std::vector<MIRPragma*> &pragmaVec) {
  WriteNum(pragmaVec.size());
  for (MIRPragma *pragma : pragmaVec) {
    OutputPragma(pragma);
  }
}

void BinaryMplExport::OutputClassTypeData(MIRClassType *type) {
  CHECK_FATAL(type != nullptr, "MIRClassType is null.");
  OutputType(type->parentTyIdx, false);
  OutputImplementedInterfaces(type->interfacesImplemented);
  OutputInfoIsString(type->infoIsString);
  if (!in_ipa) {
    OutputInfo(type->info, type->infoIsString);
    OutputPragmaVec(type->pragmaVec);
  }
}

void BinaryMplExport::OutputInterfaceTypeData(MIRInterfaceType *type) {
  CHECK_FATAL(type != nullptr, "MIRInterfaceType is null.");
  OutputImplementedInterfaces(type->parentsTyIdx);
  OutputInfoIsString(type->infoIsString);
  if (!in_ipa) {
    OutputInfo(type->info, type->infoIsString);
    OutputPragmaVec(type->pragmaVec);
  }
}

void BinaryMplExport::Init() {
  gStrMark[GStrIdx(0)] = 0;
  uStrMark[UStrIdx(0)] = 0;
  symMark[nullptr] = 0;
  funcMark[nullptr] = 0;
  for (int32 pti = static_cast<int32>(PTY_begin); pti < static_cast<int32>(PTY_end); pti++) {
    typMark[GlobalTables::GetTypeTable().typeTable[pti]] = pti;
  }
}

void BinaryMplExport::OutputSymbol(MIRSymbol *sym) {
  if (sym == nullptr) {
    WriteNum(0);
    return;
  }

  auto it = symMark.find(sym);
  if (it != symMark.end()) {
    WriteNum(-(it->second));
    return;
  }

  WriteNum(kBinSymbol);
  WriteNum(sym->GetScopeIdx());
  OutputStr(sym->nameStrIdx);
  WriteNum(sym->sKind);
  WriteNum(sym->storageClass);
  int64 mark = symMark.size();
  symMark[sym] = mark;
  OutputTypeAttrs(sym->typeAttrs);
  WriteNum(sym->isTmp ? 1 : 0);
  if (sym->sKind == kStPreg) {
    WriteNum(sym->value.preg->pregNo);
  } else if (sym->sKind == kStConst || sym->sKind == kStVar) {
    if (sym->value.konst != nullptr) {
      sym->value.konst->type = sym->GetType();
    }
    OutputConst(sym->value.konst);
  } else if (sym->sKind == kStFunc) {
    OutputFunction(sym->value.mirFunc->puIdx);
  } else if (sym->sKind == kStJavaClass || sym->sKind == kStJavaInterface) {
  } else {
    CHECK_FATAL(false, "should not used");
  }
  if (sym->sKind == kStVar || sym->sKind == kStFunc) {
    OutputSrcPos(sym->srcPosition);
  }
  OutputTypeViaTypeName(sym->tyIdx);
}

void BinaryMplExport::OutputFormalWords(MIRFunction *func) {
  WriteNum(func->upFormalSize);
  if (func->formalWordsTypeTagged != nullptr) {
    WriteNum(kBinFormalWordsTypeTagged);
    int32 num = 0;
    uint64 idx = buf.size();
    ExpandFourBuffSize();  // place holder, Fixup later
    uint32 *p = reinterpret_cast<uint32 *>(func->formalWordsTypeTagged);
    while (p < reinterpret_cast<uint32 *>(func->formalWordsTypeTagged + BlkSize2BitvectorSize(func->upFormalSize))) {
      WriteNum(*p);
      num++;
      p++;
    }
    Fixup(idx, num);
  } else {
    WriteNum(0);
  }
  if (func->formalWordsRefCounted != nullptr) {
    WriteNum(kBinFormalWordsRefCounted);
    int32 num = 0;
    uint64 idx = buf.size();
    ExpandFourBuffSize();  // place holder, Fixup later
    uint32 *p = reinterpret_cast<uint32 *>(func->formalWordsRefCounted);
    while (p < reinterpret_cast<uint32 *>(func->formalWordsRefCounted + BlkSize2BitvectorSize(func->upFormalSize))) {
      WriteNum(*p);
      num++;
      p++;
    }
    Fixup(idx, num);
  } else {
    WriteNum(0);
  }
}

void BinaryMplExport::OutputLocalWords(MIRFunction *func) {
  WriteNum(func->frameSize);
  if (func->localWordsTypeTagged != nullptr) {
    WriteNum(kBinLocalWordsTypeTagged);
    int32 num = 0;
    uint64 idx = buf.size();
    ExpandFourBuffSize();  // place holder, Fixup later
    uint32 *p = reinterpret_cast<uint32 *>(func->localWordsTypeTagged);
    while (p < reinterpret_cast<uint32 *>(func->localWordsTypeTagged + BlkSize2BitvectorSize(func->frameSize))) {
      WriteNum(*p);
      num++;
      p++;
    }
    Fixup(idx, num);
  } else {
    WriteNum(0);
  }
  if (func->localWordsRefCounted != nullptr) {
    WriteNum(kBinLocalWordsRefCounter);
    int32 num = 0;
    uint64 idx = buf.size();
    ExpandFourBuffSize();  // place holder, Fixup later
    uint32 *p = reinterpret_cast<uint32 *>(func->localWordsRefCounted);
    while (p < reinterpret_cast<uint32 *>(func->localWordsRefCounted + BlkSize2BitvectorSize(func->frameSize))) {
      WriteNum(*p);
      num++;
      p++;
    }
    Fixup(idx, num);
  } else {
    WriteNum(0);
  }
}

void BinaryMplExport::OutputFunction(PUIdx puIdx) {
  if (puIdx == 0) {
    WriteNum(0);
    mod.SetCurFunction(nullptr);
    return;
  }
  MIRFunction *func = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
  CHECK_FATAL(func != nullptr, "Cannot get MIRFunction.");
  auto it = funcMark.find(func);
  if (it != funcMark.end()) {
    WriteNum(-it->second);
    mod.SetCurFunction(func);
    return;
  }
  int64 mark = funcMark.size();
  funcMark[func] = mark;
  mir_func_t *savedFunc = GetMIRModule().CurFunction();
  mod.SetCurFunction(func);

  WriteNum(kBinFunction);
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx());
  CHECK(funcSt != nullptr, "can't get symbol! Check it!");
  OutputSymbol(funcSt);
  OutputTypeViaTypeName(func->funcType->tyIdx);
  WriteNum(func->funcAttrs.attrFlag);
  WriteNum(func->flag);
  OutputTypeViaTypeName(func->classTyIdx);
  // output formal parameter information
  WriteNum(func->formalDefVec.size());
  for (FormalDef formalDef : func->formalDefVec) {
    OutputStr(formalDef.formalStrIdx);
    OutputType(formalDef.formalTyIdx, false);
    WriteNum(formalDef.formalAttrs.attrFlag);
  }

  //  store Side Effect for each func
  if (func2SE_map) {
    uint32 isSee = func->IsIpaSeen() == true ? 1 : 0;
    uint32 isPur = func->IsPure() == true ? 1 : 0;
    uint32 noDef = func->IsNoDefEffect() == true ? 1 : 0;
    uint32 noThr = func->IsNoThrowException() == true ? 1 : 0;
    uint32 noPriDef = func->IsNoPrivateDefEffect() == true ? 1 : 0;
    uint8 se = noPriDef << 7 | isSee << 5 | isPur << 4 | noDef << 2 | noThr;
    if ((*func2SE_map).find(func->GetNameStridx()) == (*func2SE_map).end()) {
      (*func2SE_map)[func->GetNameStridx()] = se;
    } else if ((*func2SE_map)[func->GetNameStridx()] != se) {
      CHECK_FATAL(false, "Check if it is a bug");
    }
  }
  mod.SetCurFunction(savedFunc);
}

void BinaryMplExport::WriteStrField(uint64 contentIdx) {
  Fixup(contentIdx, buf.size());
  WriteNum(kBinStrStart);
  size_t totalSizeIdx = buf.size();
  ExpandFourBuffSize();  /// total size of this field to ~BIN_STR_START
  size_t outstrSizeIdx = buf.size();
  ExpandFourBuffSize();  /// size of OutputStr

  int32 size = 0;
  for (const auto& entity : GlobalTables::GetConstPool().constU16StringPool) {
    MIRSymbol *sym = entity.second;
    if (sym->IsLiteral()) {
      OutputStr(sym->GetNameStridx());
      size++;
    }
  }
  Fixup(totalSizeIdx, buf.size() - totalSizeIdx);
  Fixup(outstrSizeIdx, size);
  WriteNum(~kBinStrStart);
  return;
}

void BinaryMplExport::WriteHeaderField(uint64 contentIdx) {
  Fixup(contentIdx, buf.size());
  WriteNum(kBinHeaderStart);
  size_t totalSizeIdx = buf.size();
  ExpandFourBuffSize();  /// total size of this field to ~BIN_IMPORT_START
  WriteNum(mod.flavor);
  WriteNum(mod.srcLang);
  WriteNum(mod.id);
  WriteNum(mod.numFuncs);
  WriteAsciiStr(mod.entryFuncName);
  OutputInfoVector(mod.fileInfo, mod.fileInfoIsString);

  WriteNum(mod.srcFileInfo.size());
  for (uint32 i = 0; i < mod.srcFileInfo.size(); i++) {
    OutputStr(mod.srcFileInfo[i].first);
    WriteNum(mod.srcFileInfo[i].second);
  }

  WriteNum(mod.importFiles.size());
  for (GStrIdx strIdx : mod.importFiles) {
    OutputStr(strIdx);
  }

  Fixup(totalSizeIdx, buf.size() - totalSizeIdx);
  WriteNum(~kBinHeaderStart);
  return;
}

void BinaryMplExport::WriteTypeField(uint64 contentIdx, bool useClassList) {
  Fixup(contentIdx, buf.size());
  WriteNum(kBinTypeStart);
  size_t totalSizeIdx = buf.size();
  ExpandFourBuffSize();  /// total size of this field to ~BIN_TYPE_START
  size_t outtypeSizeIdx = buf.size();
  ExpandFourBuffSize();  /// size of OutputType
  int32 size = 0;
  if (useClassList) {
    for (uint32 tyIdx : GetMIRModule().classList) {
      TyIdx curTyidx(tyIdx);
      MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(curTyidx);
      CHECK_FATAL(type != nullptr, "Pointer type is nullptr, cannot get type, check it!");
      if (type->typeKind == kTypeClass || type->typeKind == kTypeInterface) {
        MIRStructType *structtype = static_cast<MIRStructType *>(type);
        // skip imported class/interface and incomplete types
        if (!structtype->isImported && !structtype->IsIncomplete()) {
          OutputType(curTyidx, false);
          size++;
        }
      }
    }
  } else {
    uint32 idx = GlobalTables::GetTypeTable().lastDefaultTyIdx.GetIdx();
    for (idx = idx + 1; idx < GlobalTables::GetTypeTable().typeTable.size(); idx++) {
      OutputType(TyIdx(idx), false);
      size++;
    }
  }
  Fixup(totalSizeIdx, buf.size() - totalSizeIdx);
  Fixup(outtypeSizeIdx, size);
  WriteNum(~kBinTypeStart);
}

void BinaryMplExport::WriteSymField(uint64 contentIdx) {
  Fixup(contentIdx, buf.size());
  // LogInfo::MapleLogger() << "Write SYM Field " << std::endl;
  WriteNum(kBinSymStart);
  uint64 totalSizeIdx = buf.size();
  ExpandFourBuffSize();  /// total size of this field to ~BIN_SYM_START
  uint64 outsymSizeIdx = buf.size();
  ExpandFourBuffSize();  /// size of OutSym
  int32 size = 0;

  if (not2mplt) {
    for (auto sit = GetMIRModule().symbolDefOrder.begin(); sit != GetMIRModule().symbolDefOrder.end(); sit++) {
      MIRSymbol *s = GlobalTables::GetGsymTable().GetSymbolFromStIdx((*sit).Idx());
      // Verify: all wpofake variables should have been deleted from globaltable
      ASSERT(!(s->wpofakeParm || s->wpofakeRet) || s->IsDeleted(), "wpofake var not deleted");
      MIRStorageClass storageClass = s->storageClass;
      MIRSymKind sKind = s->sKind;

      if (s->IsDeleted() ||
          storageClass == kScUnused ||
          (s->isImported && !s->appearsincode) ||
          (storageClass == kScExtern && sKind == kStFunc)) {
        continue;
      }
      // printf("EXPORT Writing symbol %s\n", s->GetName().c_str());
      OutputSymbol(s);
      size++;
    }
  }

  Fixup(totalSizeIdx, buf.size() - totalSizeIdx);
  Fixup(outsymSizeIdx, size);
  WriteNum(~kBinSymStart);
  return;
}

void BinaryMplExport::WriteSymtabField(uint64 contentIdx) {
  Fixup(contentIdx, buf.size());
  WriteNum(kBinSymTabStart);
  uint64 totalSizeIdx = buf.size();
  ExpandFourBuffSize();  // total size of this field to ~BIN_TYPE_START
  uint64 outtableSizeIdx = buf.size();
  ExpandFourBuffSize();  // size of OutputType
  int32 size = 0;
  ///   std::unordered_map<MIRSymbol *, int64> symMark;
  for (auto it = symMark.begin(); it != symMark.end(); it++) {
    if ((*it).first == nullptr) {
      continue;
    }
    MIRStorageClass storageClass = (*it).first->storageClass;
    MIRSymKind sKind = (*it).first->sKind;
    if (storageClass == kScUnused || storageClass == kScFormal || (storageClass == kScExtern && sKind == kStFunc) || sKind != kStFunc) {
      continue;
    }
    WriteAsciiStr(GlobalTables::GetStrTable().GetStringFromStrIdx((*it).first->nameStrIdx));
    WriteNum((*it).second);
    size++;
  }
  LogInfo::MapleLogger() << "@@@ sym table size :: " << size << std::endl;
  Fixup(totalSizeIdx, buf.size() - totalSizeIdx);
  Fixup(outtableSizeIdx, size);
  WriteNum(~kBinSymTabStart);
  return;
}

void BinaryMplExport::OutputCallInfo(const CallInfo *callInfo) {
  auto it = callInfoMark.find(callInfo->GetID());
  if (it != callInfoMark.end()) {
    WriteNum(-it->second);
    return;
  }
  WriteNum(kBinCallinfo);
  int64 mark = callInfoMark.size();
  callInfoMark[callInfo->GetID()] = mark;
  WriteNum(callInfo->GetCallType());  /// call type

  WriteInt(callInfo->GetLoopDepth());
  WriteInt(callInfo->GetID());
  OutputSymbol(callInfo->GetFunc()->GetFuncSymbol());
}

void BinaryMplExport::WriteCgField(uint64 contentIdx, const CallGraph *cg, KlassHierarchy *kh) {
  if (contentIdx) {
    Fixup(contentIdx, buf.size());
  }
  WriteNum(kBinCgStart);
  uint64 totalSizeIdx = buf.size();
  ExpandFourBuffSize();  /// total size of this field to ~BIN_CG_START
  uint64 outcgSizeIdx = buf.size();
  ExpandFourBuffSize();  /// size of OutCG
  int32 size = 0;
  if (cg) {
    // LogInfo::MapleLogger() << "Out CG INFO " << std::endl;
    ////  entry : MapleMap<MIRFunction *, CGNode *>
    for (size_t i = 0; i < cg->GetNodesMap().size(); i++) {
      CGNode *cgnode = cg->GetNodesMap().at(i);
      if (cgnode == nullptr) {
        continue;
      }
      MIRSymbol *methodSym = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(i)->GetFuncSymbol();
      WriteNum(kStartMethod);
      OutputSymbol(methodSym);
      uint64 targetSetIdx = buf.size();
      ExpandFourBuffSize();
      int32 targSize = 0;
      callInfoMark.clear();
      callInfoMark[0xffffffff] = 0;
      // LogInfo::MapleLogger() << "OUT method : " << method_sym->GetName() << std::endl;
      for (auto callsite = cgnode->CalleeBegin(); callsite != cgnode->CalleeEnd(); callsite++) {
        OutputCallInfo((*callsite).first);
        // LogInfo::MapleLogger() << "    ID : " << (*callsite).first->GetID()<< std::endl;
        MIRSymbol *targetSym = nullptr;
        if ((*callsite).second) {
          targetSym = (*callsite).second->GetMIRFunction()->GetFuncSymbol();
        } else {
          targetSym = nullptr;
        }
        // LogInfo::MapleLogger() << "    target : id:" << (*callsite).first->GetID() << "  name : " << target_sym->GetName() <<
        // std::endl;
        OutputSymbol(targetSym);
        targSize++;
      }
      Fixup(targetSetIdx, targSize);
      WriteNum(~kStartMethod);
      size++;
    }
  }
  if ((buf.size() - totalSizeIdx) > 0xffffffff) {
    CHECK_FATAL(false, "integer overflow");
  }
  Fixup(totalSizeIdx, buf.size() - totalSizeIdx);
  Fixup(outcgSizeIdx, size);
  WriteNum(~kBinCgStart);
  return;
}

void BinaryMplExport::WriteSeField() {
  CHECK_FATAL(func2SE_map, "Expecting a func2SE map");
  WriteNum(kBinSeStart);
  uint64 totalSizeIdx = buf.size();
  ExpandFourBuffSize();  /// total size of this field to ~BIN_SYM_START
  uint64 outseSizeIdx = buf.size();
  ExpandFourBuffSize();  /// size of OutSym
  int32 size = 0;

  for (auto it = (*func2SE_map).begin(); it != (*func2SE_map).end(); it++) {
    OutputStr(it->first);
    Write(it->second);
    size++;
    // LogInfo::MapleLogger() << "func : " << globaltable.GetStringFromStrIdx(it->first) << "  se : "
    //           << static_cast<int32>(it->second) << std::endl;
  }
  Fixup(totalSizeIdx, buf.size() - totalSizeIdx);
  Fixup(outseSizeIdx, size);
  WriteNum(~kBinSeStart);
  return;
}

void BinaryMplExport::WriteContentField4mplt(int fieldNum, uint64 *fieldStartP) {
  WriteNum(kBinContentStart);
  uint64 totalSizeIdx = buf.size();
  ExpandFourBuffSize();         /// total size of this field to ~BIN_SYM_START

  WriteInt(fieldNum);  /// size of Content item

  WriteNum(kBinStrStart);
  fieldStartP[0] = buf.size();
  ExpandFourBuffSize();

  WriteNum(kBinTypeStart);
  fieldStartP[1] = buf.size();
  ExpandFourBuffSize();

  WriteNum(kBinCgStart);
  fieldStartP[2] = buf.size();
  ExpandFourBuffSize();

  Fixup(totalSizeIdx, buf.size() - totalSizeIdx);
  WriteNum(~kBinContentStart);
}

void BinaryMplExport::Export(const string &fname, std::unordered_set<std::string> *dumpFuncSet) {
  uint64 fieldStartPoint[5];
  if (!not2mplt) {
    WriteInt(kMpltMagicNumber);
    WriteContentField4mplt(3, fieldStartPoint);
    WriteStrField(fieldStartPoint[0]);
    WriteTypeField(fieldStartPoint[1]);
    importFileName = fname;
  } else {
    WriteInt(kMpltMagicNumber+0x10);
    if (mod.IsJavaModule()) {
      WriteContentField4nonmplt(3, fieldStartPoint);
      WriteHeaderField(fieldStartPoint[0]);
      WriteSymField(fieldStartPoint[1]);
      WriteFunctionBodyField(fieldStartPoint[2], dumpFuncSet);
    } else {
      WriteContentField4nonJava(5, fieldStartPoint);
      WriteHeaderField(fieldStartPoint[0]);
      WriteStrField(fieldStartPoint[1]);
      WriteTypeField(fieldStartPoint[2], false/*useClassList*/);
      WriteSymField(fieldStartPoint[3]);
      WriteFunctionBodyField(fieldStartPoint[4], dumpFuncSet);
    }
  }
  WriteNum(kBinFinish);
  DumpBuf(fname);
}

void BinaryMplExport::WriteContentField4nonmplt(int fieldNum, uint64 *fieldStartP) {
  CHECK_FATAL(fieldStartP != nullptr, "fieldStartP is null.");
  WriteNum(kBinContentStart);
  size_t totalSizeIdx = buf.size();
  ExpandFourBuffSize();         /// total size of this field to ~BIN_SYM_START

  WriteInt(fieldNum);  /// size of Content item

  WriteNum(kBinHeaderStart);
  fieldStartP[0] = buf.size();
  ExpandFourBuffSize();

  WriteNum(kBinSymStart);
  fieldStartP[1] = buf.size();
  ExpandFourBuffSize();

  WriteNum(kBinFunctionBodyStart);
  fieldStartP[2] = buf.size();
  ExpandFourBuffSize();

  Fixup(totalSizeIdx, buf.size() - totalSizeIdx);
  WriteNum(~kBinContentStart);
}

void BinaryMplExport::WriteContentField4nonJava(int fieldNum, uint64 *fieldStartP) {
  CHECK_FATAL(fieldStartP != nullptr, "fieldStartP is null.");
  WriteNum(kBinContentStart);
  size_t totalSizeIdx = buf.size();
  ExpandFourBuffSize();         /// total size of this field to ~BIN_SYM_START

  WriteInt(fieldNum);  /// size of Content item

  WriteNum(kBinHeaderStart);
  fieldStartP[0] = buf.size();
  ExpandFourBuffSize();

  WriteNum(kBinStrStart);
  fieldStartP[1] = buf.size();
  ExpandFourBuffSize();

  WriteNum(kBinTypeStart);
  fieldStartP[2] = buf.size();
  ExpandFourBuffSize();

  WriteNum(kBinSymStart);
  fieldStartP[3] = buf.size();
  ExpandFourBuffSize();

  WriteNum(kBinFunctionBodyStart);
  fieldStartP[4] = buf.size();
  ExpandFourBuffSize();

  Fixup(totalSizeIdx, buf.size() - totalSizeIdx);
  WriteNum(~kBinContentStart);
}

void BinaryMplExport::AppendAt(const string &name, int32 offset) {
  FILE *f = fopen(name.c_str(), "r+b");
  if (f == nullptr) {
    LogInfo::MapleLogger(kLlErr) << "Error while opening the binary file: " << name << '\n';
    FATAL(kLncFatal, "Error while creating the binary file: %s\n", name.c_str());
  }
  int seekRet = fseek(f, (long int)offset, SEEK_SET);
  CHECK_FATAL(seekRet == 0, "Call fseek failed.");
  size_t size = buf.size();
  size_t k = fwrite(&buf[0], sizeof(uint8), size, f);
  fclose(f);
  if (k != size) {
    LogInfo::MapleLogger(kLlErr) << "Error while writing the binary file: " << name << '\n';
  }
}

void BinaryMplExport::OutputTypePairs(MIRInstantVectorType *type) {
  size_t size = type->instantVec.size();
  WriteNum(size);
  for (TypePair &tpair : type->instantVec) {
    OutputType(tpair.first, false);
    OutputType(tpair.second, false);
  }
}

void BinaryMplExport::OutputTypeAttrs(const TypeAttrs &ta) {
  WriteNum(ta.attrFlag);
  WriteNum(ta.attrAlign);
}

void BinaryMplExport::OutputType(const TyIdx &tyIdx, bool canUseTypename) {
  if (tyIdx.GetIdx() == 0) {
    WriteNum(0);
    return;
  }
  MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  CHECK(ty != nullptr, "if get's nulltype, should have been returned!");
  auto it = typMark.find(ty);
  if (it != typMark.end()) {
    WriteNum(-(it->second));
    return;
  }
  int64 mark = typMark.size();
  typMark[ty] = mark;

  if (canUseTypename && !ty->nameIsLocal && ty->nameStrIdx != GStrIdx(0)) {
    WriteNum(kBinKindTypeViaTypename);
    OutputStr(ty->nameStrIdx);
    return;
  }

  auto func = CreateProductFunction<OutputTypeFactory>(ty->typeKind);
  if (func != nullptr) {
    func(ty, this, canUseTypename);
  } else {
    CHECK_FATAL(false, "Type's kind not yet implemented: %d", ty->GetKind());
  }
}

void DoUpdateMplt::UpdateCgField(BinaryMplt *binMplt, const CallGraph *cg, KlassHierarchy *kh) {
  BinaryMplImport &binimport = binMplt->GetBinImport();
  BinaryMplExport &binexport = binMplt->GetBinExport();
  binimport.bufI = 0;
  if (binimport.buf.size() == 0 || binimport.ReadInt() != kMpltMagicNumber) {
    INFO(kLncInfo, " This Module depends on nothing");
    return;
  }
  int64 cgStart = binimport.content[kBinCgStart];
  CHECK_FATAL(cgStart, "should be updated in import processing");
  binimport.bufI = cgStart;
  CHECK_FATAL(binimport.ReadNum() == kBinCgStart, "should be cg start point");
  int32 totalSize = binimport.ReadInt();
  binimport.bufI += totalSize - 4;
  CHECK_FATAL(binimport.ReadNum() == ~kBinCgStart, "should be end of cg");
  binexport.Init();
  std::map<GStrIdx, uint8> tmp;
  binexport.func2SE_map = &tmp;
  binexport.in_ipa = true;
  binexport.WriteCgField(0, cg, kh);
  binexport.gStrMark.clear();
  binexport.gStrMark[GStrIdx(0)] = 0;
  binexport.WriteSeField();
  binexport.WriteNum(kBinFinish);
  // LogInfo::MapleLogger() << " update mplt : " << binMplt->importFileName << std::endl;
  std::string filename(binMplt->GetImportFileName());
  binexport.AppendAt(filename, cgStart);
  // TO BE UPDATED
  // binimport.content[kBinCgStart] = 0;
  return;
}

DoUpdateMplt::ManualSideEffect manualIpaList[]{
#include <../../maple_ipa/include/manual_ipa.def>
};

void DoUpdateMplt::UpdateManualIpa() {
  int manualIpaSize = sizeof(manualIpaList) / sizeof(ManualSideEffect);
  for (int i = 0; i < manualIpaSize; i++) {
    // LogInfo::MapleLogger() << manualIpaList[i].GetFuncName() << " use : " << manualIpaList[i].GetUse()<< std::endl;
    MIRSymbol *sym = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(GlobalTables::GetStrTable().GetStrIdxFromName(manualIpaList[i].GetFuncName()));
    if (sym) {
      MIRFunction *mirFunc = sym->GetFunction();
      if (manualIpaList[i].GetPure()) {
        mirFunc->SetPure();
      }
      if (manualIpaList[i].GetDef()) {
        mirFunc->SetNoDefEffect();
      }
      if (manualIpaList[i].GetException()) {
        mirFunc->SetNoThrowException();
      }
    }
  }
}

AnalysisResult *DoUpdateMplt::Run(MIRModule *module, ModuleResultMgr *m) {
  // LogInfo::MapleLogger() << "-------   Starting DO UPDATE MPLT  --------"  << std::endl;
  CallGraph *cg = static_cast<CallGraph *>(m->GetAnalysisResult(MoPhase_CALLGRAPH_ANALYSIS, module));
  CHECK_FATAL(cg != nullptr, "Expecting a valid CallGraph, found nullptr");
  KlassHierarchy *kh = static_cast<KlassHierarchy *>(m->GetAnalysisResult(MoPhase_CHA, module));
  ASSERT(kh != nullptr, "Expecting a valid KlassHierarchy, found nullptr");
  BinaryMplt *binMplt = module->binMplt;
  CHECK_FATAL(binMplt, "Expecting a valid binMplt");
  UpdateManualIpa();
  delete module->binMplt;
  module->binMplt = nullptr;
  return nullptr;
}

}  // namespace maple
