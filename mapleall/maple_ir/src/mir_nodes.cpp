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

#include "mir_nodes.h"
#include "mir_function.h"
#include "printing.h"
#include "maple_string.h"
#include <algorithm>
#include "opcode_info.h"
#include "name_mangler.h"
namespace maple {
MIRModule *theModule;
uint32 StmtNode::stmtIDNext = 1;  // 0 is reserved
uint32 StmtNode::lastPrintedLinenum = 0;

const char *GetIntrinsicName(MIRIntrinsicID intrn) {
  switch (intrn) {
    default:
#define DEF_MIR_INTRINSIC(STR, NAME, INTRN_CLASS, RETURN_TYPE, ...) \
  case INTRN_##STR:                                                 \
    return #STR;
#include "intrinsics.def"
#undef DEF_MIR_INTRINSIC
  }
}

static std::unordered_map<std::string, MIRIntrinsicID> IntrinsicList = {
#define DEF_MIR_INTRINSIC(STR, NAME, INTRN_CLASS, RETURN_TYPE, ...) \
  {#NAME , INTRN_##STR},
#include "intrinsics.def"
#undef DEF_MIR_INTRINSIC
};

MIRIntrinsicID LowerToIntrinsic(std::string funcName) {
  auto it = IntrinsicList.find(funcName);
  if (it == IntrinsicList.end()) {
    return(INTRN_UNDEFINED);
  } else {
    return(it->second);
  }
}

std::string GetIntrinsicFuncName(MIRIntrinsicID intrn) {
  switch (intrn) {
    default:
      CHECK_FATAL(0, "Unknown intrinsic id");
#define DEF_MIR_INTRINSIC(STR, NAME, INTRN_CLASS, RETURN_TYPE, ...) \
  case INTRN_##STR:                                                 \
    return #NAME;
#include "intrinsics.def"
#undef DEF_MIR_INTRINSIC
  }
}

const char *BaseNode::GetOpName() const {
  return kOpcodeInfo.GetName(op);
}

bool BaseNode::MayThrowException() {
  if (kOpcodeInfo.MayThrowException(op)) {
    if (op != OP_array) {
      return true;
    }
    ArrayNode *arry = static_cast<ArrayNode *>(this);
    if (arry->boundsCheck) {
      return true;
    }
  } else if (op == OP_intrinsicop) {
    IntrinsicopNode *innode = static_cast<IntrinsicopNode *>(this);
    if (innode->intrinsic == INTRN_JAVA_ARRAY_LENGTH) {
      return true;
    }
  }
  for (size_t i = 0; i < NumOpnds(); i++) {
    if (Opnd(i)->MayThrowException()) {
      return true;
    }
  }
  return false;
}

bool AddrofNode::CheckNode(const MIRModule *mod) {
  MIRSymbol *st = mod->CurFunction()->GetLocalOrGlobalSymbol(stIdx);
  MIRType *ty = st->GetType();
  switch (ty->typeKind) {
    case kTypeScalar: {
#ifdef DYNAMICLANG
      if (primType == PTY_dynany) {
        return true;
      } else {
        return IsPrimitiveScalar(primType) || IsPrimitiveVectorInt(primType)
               || IsPrimitiveVectorFloat(primType);
      }
#else
      return IsPrimitiveScalar(primType) || IsPrimitiveVectorInt(primType)
             || IsPrimitiveVectorFloat(primType);
#endif
    }
    case kTypeArray: {
      return primType == PTY_agg;
    }
    case kTypeUnion:
    case kTypeStruct:
    case kTypeStructIncomplete: {
      if (fieldID == 0) {
        return primType == PTY_agg;
      } else {
        MIRStructType *structType = static_cast<MIRStructType *>(ty);
        TyIdx ftyidx = structType->TraverseToField(fieldID).second.first;
        MIRType *subty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx);
        MIRTypeKind subkind = subty->typeKind;
        return (subkind == kTypeBitField && VerifyPrimType(subty->primType, primType)) ||
               (subkind == kTypeScalar && IsPrimitiveScalar(primType)) ||
               (subkind == kTypePointer && IsPrimitivePoint(primType)) || (subkind == kTypeStruct && primType == PTY_agg) ||
               (ftyidx != 0 && primType == PTY_agg);
      }
    }
    case kTypeClass:
    case kTypeClassIncomplete: {
      if (fieldID == 0) {
        return primType == PTY_agg;
      } else {
        MIRClassType *classType = static_cast<MIRClassType *>(ty);
        TyIdx ftyidx = classType->TraverseToField(fieldID).second.first;
        MIRType *subty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx);
        MIRTypeKind subkind = subty->typeKind;
        return (subkind == kTypeBitField && VerifyPrimType(subty->primType, primType)) ||
               (subkind == kTypeScalar && IsPrimitiveScalar(primType)) ||
               (subkind == kTypePointer && IsPrimitivePoint(primType)) || (subkind == kTypeStruct && primType == PTY_agg);
      }
    }
    case kTypeInterface:
    case kTypeInterfaceIncomplete: {
      if (fieldID == 0) {
        return primType == PTY_agg;
      } else {
        MIRInterfaceType *interfaceType = static_cast<MIRInterfaceType *>(ty);
        TyIdx ftyidx = interfaceType->TraverseToField(fieldID).second.first;
        MIRType *subty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ftyidx);
        MIRTypeKind subkind = subty->typeKind;
        return (subkind == kTypeBitField && VerifyPrimType(subty->primType, primType)) ||
               (subkind == kTypeScalar && IsPrimitiveScalar(primType)) ||
               (subkind == kTypePointer && IsPrimitivePoint(primType)) || (subkind == kTypeStruct && primType == PTY_agg);
      }
    }
    case kTypePointer:
      return IsPrimitivePoint(primType);
    case kTypeParam:
    case kTypeGenericInstant:
      return true;
    default:
      return false;
  }
}

bool AddrofNode::HasSymbol(MIRModule *mod, MIRSymbol *st) {
  return st == mod->CurFunction()->GetLocalOrGlobalSymbol(stIdx);
}

void BlockNode::AddStatement(StmtNode *stmt) {
  ASSERT(stmt != nullptr, "null ptr check");
  stmtNodeList.push_back(stmt);
}

void BlockNode::AppendStatementsFromBlock(BlockNode *blk) {
  ASSERT(blk != nullptr, "null ptr check");
  stmtNodeList.splice(stmtNodeList.end(), blk->GetStmtNodes());
}

/// Insert stmt as the first
void BlockNode::InsertFirst(StmtNode *stmt) {
  ASSERT(stmt != nullptr, "stmt is null");
  stmtNodeList.push_front(stmt);
}

/// Insert stmt as the last
void BlockNode::InsertLast(StmtNode *stmt) {
  ASSERT(stmt != nullptr, "stmt is null");
  stmtNodeList.push_back(stmt);
}

void BlockNode::ReplaceStmtWithBlock(StmtNode *stmtNode, BlockNode *blk) {
  stmtNodeList.splice(stmtNode, blk->GetStmtNodes());
  stmtNodeList.erase(stmtNode);
  stmtNode->SetNext(blk->GetLast()->GetNext());
}

void BlockNode::ReplaceStmt1WithStmt2(StmtNode *stmtNode1, StmtNode *stmtNode2) {
  if (stmtNode2 == stmtNode1) {
    // do nothing
  } else if (stmtNode2 == nullptr) {
    // delete stmtNode1
    stmtNodeList.erase(stmtNode1);
  } else {
    // replace stmtNode1 with stmtNode2
    stmtNodeList.insert(stmtNode1, stmtNode2);
    stmtNodeList.erase(stmtNode1);
  }
}

// remove sstmtNode1 from block
void BlockNode::RemoveStmt(StmtNode *stmtNode1) {
  ASSERT(stmtNode1 != nullptr, "delete a null stmtment");
  stmtNodeList.erase(stmtNode1);
}

/// Insert stmtNode2 before stmtNode1 in current block.
void BlockNode::InsertBefore(StmtNode *stmtNode1, StmtNode *stmtNode2) {
  stmtNodeList.insert(stmtNode1, stmtNode2);
}

/// Insert stmtNode2 after stmtNode1 in current block.
void BlockNode::InsertAfter(StmtNode *stmtNode1, StmtNode *stmtNode2) {
  stmtNodeList.insertAfter(stmtNode1, stmtNode2);
}

// insert all the stmts in inblock to the current block after stmt1
void BlockNode::InsertBlockAfter(BlockNode *inblock, StmtNode *stmt1) {
  ASSERT(stmt1 != nullptr, "null ptr check");
  ASSERT(!inblock->IsEmpty(), "NYI");
  stmtNodeList.splice(stmt1, inblock->GetStmtNodes());
}

void BaseNode::DumpBase(const MIRModule *mod, int32 indent) const {
  PrintIndentation(indent);
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
}

void CppCatchNode::Dump(const MIRModule *mod, int32 indent) const {
  PrintIndentation(indent);
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " { ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(exceptionTyIdx)->Dump(indent + 1);
  LogInfo::MapleLogger() << " }" << std::endl;
}

void CatchNode::Dump(const MIRModule *mod, int32 indent) const {
  PrintIndentation(indent);
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " {";
  int size = exceptionTyIdxVec.size();
  for (int i = 0; i < size; i++) {
    LogInfo::MapleLogger() << " ";
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(exceptionTyIdxVec[i])->Dump(indent + 1);
  }
  LogInfo::MapleLogger() << " }" << std::endl;
}

void UnaryNode::DumpOpnd(const MIRModule *mod, int indent) const {
  LogInfo::MapleLogger() << " (";
  uOpnd->Dump(mod, indent);
  LogInfo::MapleLogger() << ")";
}

void UnaryNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  DumpOpnd(mod, indent);
}

void TypeCvtNode::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " ";
  LogInfo::MapleLogger() << GetPrimTypeName(primType) << " " << GetPrimTypeName(fromPrimType);
  DumpOpnd(mod, indent);
}

void RetypeNode::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " ";
  LogInfo::MapleLogger() << GetPrimTypeName(primType) << " ";
  MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  if (ty->typeKind == kTypeScalar) {
    LogInfo::MapleLogger() << "<";
    ty->Dump(indent + 1);
    LogInfo::MapleLogger() << ">";
  } else {
    ty->Dump(indent + 1);
  }
  DumpOpnd(mod, indent);
}

void ExtractbitsNode::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
  if (op == OP_extractbits) {
    LogInfo::MapleLogger() << " " << static_cast<int32>(bitsOffset) << " " << static_cast<int32>(bitsSize);
  } else {
    LogInfo::MapleLogger() << " " << static_cast<int32>(bitsSize);
  }
  DumpOpnd(mod, indent);
}

void IreadNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  LogInfo::MapleLogger() << " ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->Dump(0);
  LogInfo::MapleLogger() << " " << fieldID;
  DumpOpnd(mod, indent);
}

void IreadoffNode::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
  LogInfo::MapleLogger() << " " << offset;
  DumpOpnd(mod, indent);
}

void IreadFPoffNode::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
  LogInfo::MapleLogger() << " " << offset;
}

void BinaryNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  BinaryOpnds::Dump(mod, indent);
}

void BinaryNode::Dump(const MIRModule *mod) const {
  this->BaseNode::Dump(mod);
}

void BinaryOpnds::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << " (";
  if (bOpnd[0]->IsLeaf() && bOpnd[1]->IsLeaf()) {
    bOpnd[0]->Dump(mod, 0);
    LogInfo::MapleLogger() << ", ";
    bOpnd[1]->Dump(mod, 0);
  } else {
    LogInfo::MapleLogger() << std::endl;
    PrintIndentation(indent + 1);
    bOpnd[0]->Dump(mod, indent + 1);
    LogInfo::MapleLogger() << "," << std::endl;
    PrintIndentation(indent + 1);
    bOpnd[1]->Dump(mod, indent + 1);
  }
  LogInfo::MapleLogger() << ")";
}

void ResolveFuncNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  MIRFunction *func = GlobalTables::GetFunctionTable().funcTable.at(puIdx);
  LogInfo::MapleLogger() << " &" << func->GetName();
  BinaryOpnds::Dump(mod, indent);
}

void CompareNode::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
  LogInfo::MapleLogger() << " " << GetPrimTypeName(opndType);
  BinaryOpnds::Dump(mod, indent);
}

void CompareNode::Dump(const MIRModule *mod) const {
  this->BaseNode::Dump(mod);
}

void DepositbitsNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  LogInfo::MapleLogger() << " " << static_cast<int32>(bitsOffset) << " " << static_cast<int32>(bitsSize) << " (";
  if (bOpnd[0]->IsLeaf() && bOpnd[1]->IsLeaf()) {
    bOpnd[0]->Dump(mod, 0);
    LogInfo::MapleLogger() << ", ";
    bOpnd[1]->Dump(mod, 0);
  } else {
    LogInfo::MapleLogger() << std::endl;
    PrintIndentation(indent + 1);
    bOpnd[0]->Dump(mod, indent + 1);
    LogInfo::MapleLogger() << "," << std::endl;
    PrintIndentation(indent + 1);
    bOpnd[1]->Dump(mod, indent + 1);
  }
  LogInfo::MapleLogger() << ")";
}

void TernaryNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  LogInfo::MapleLogger() << " (";
  if (topnd[0]->IsLeaf() && topnd[1]->IsLeaf() && topnd[2]->IsLeaf()) {
    topnd[0]->Dump(mod, 0);
    LogInfo::MapleLogger() << ", ";
    topnd[1]->Dump(mod, 0);
    LogInfo::MapleLogger() << ", ";
    topnd[2]->Dump(mod, 0);
  } else {
    LogInfo::MapleLogger() << std::endl;
    PrintIndentation(indent + 1);
    topnd[0]->Dump(mod, indent + 1);
    LogInfo::MapleLogger() << "," << std::endl;
    PrintIndentation(indent + 1);
    topnd[1]->Dump(mod, indent + 1);
    LogInfo::MapleLogger() << "," << std::endl;
    PrintIndentation(indent + 1);
    topnd[2]->Dump(mod, indent + 1);
  }
  LogInfo::MapleLogger() << ")";
}

void NaryOpnds::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << " (";
  if (nOpnd.size() == 0) {
    LogInfo::MapleLogger() << ")";
    return;
  }
  if (nOpnd.size() == 1) {
    nOpnd[0]->Dump(mod, indent);
  } else {
    bool allisleaf = true;
    for (size_t i = 0; i < nOpnd.size(); i++)
      if (!nOpnd[i]->IsLeaf()) {
        allisleaf = false;
        break;
      }
    if (allisleaf) {
      nOpnd[0]->Dump(mod, 0);
      for (size_t i = 1; i < nOpnd.size(); i++) {
        LogInfo::MapleLogger() << ", ";
        nOpnd[i]->Dump(mod, 0);
      }
    } else {
      LogInfo::MapleLogger() << std::endl;
      PrintIndentation(indent + 1);
      nOpnd[0]->Dump(mod, indent + 1);
      for (size_t i = 1; i < nOpnd.size(); i++) {
        LogInfo::MapleLogger() << "," << std::endl;
        PrintIndentation(indent + 1);
        nOpnd[i]->Dump(mod, indent + 1);
      }
    }
  }
  LogInfo::MapleLogger() << ")";
}

bool NaryOpnds::VerifyOpnds() const {
  bool nOpndsVerify = true;
  for (size_t i = 0; i < nOpnd.size(); i++) {
    if (!nOpnd[i]->Verify()) {
      nOpndsVerify = false;
      break;
    }
  }
  return nOpndsVerify;
}

void NaryNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  NaryOpnds::Dump(mod, indent);
}

MIRType *ArrayNode::GetArrayType(TypeTable *tt) {
  MIRPtrType *pointType = MIR_DYN_CAST(tt->typeTable.at(tyIdx.GetIdx()), MIRPtrType *);
  ASSERT(pointType, "expect array type pointer");
  return tt->GetTypeFromTyIdx(pointType->pointedTyIdx);
}

BaseNode *ArrayNode::GetDim(const MIRModule *mod, TypeTable *tt, int i) {
  MIRArrayType *arraytype = static_cast<MIRArrayType *>(GetArrayType(tt));
  MIRConst *mirconst = mod->CurFuncCodeMemPool()->New<MIRConst>(tt->GetTypeFromTyIdx(arraytype->eTyIdx));
  return mod->CurFuncCodeMemPool()->New<ConstvalNode>(mirconst);
}

void ArrayNode::Dump(const MIRModule *mod, int32 indent) const {
  // BaseNode::DumpBase(mod, 0);
  PrintIndentation(0);
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " ";
  if (boundsCheck) {
    LogInfo::MapleLogger() << "1 ";
  } else {
    LogInfo::MapleLogger() << "0 ";
  }
  LogInfo::MapleLogger() << GetPrimTypeName(primType);
  LogInfo::MapleLogger() << " ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->Dump(0);
  NaryOpnds::Dump(mod, indent);
}

void ArrayNode::Dump(const MIRModule *mod) const {
  this->BaseNode::Dump(mod);
}

bool ArrayNode::IsSameBase(ArrayNode *arry) {
  if (arry == this)
    return true;
  BaseNode *curBase = this->GetBase();
  BaseNode *otherBase = arry->GetBase();
  if (curBase->op != OP_addrof || otherBase->op != OP_addrof)
    return false;
  return static_cast<AddrofNode *>(curBase)->stIdx ==
             static_cast<AddrofNode *>(otherBase)->stIdx;
}

void IntrinsicopNode::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
  if (op == OP_intrinsicopwithtype) {
    LogInfo::MapleLogger() << " ";
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->Dump(indent + 1);
  }
  LogInfo::MapleLogger() << " " << GetIntrinsicName(intrinsic);
  NaryOpnds::Dump(mod, indent);
}

void ConstvalNode::Dump(const MIRModule *mod, int32 indent) const {
  if (constVal->type->GetKind() != kTypePointer) {
    BaseNode::DumpBase(mod, 0);
    LogInfo::MapleLogger() << " ";
  }
  constVal->Dump();
}

void ConststrNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  const std::string kStr = GlobalTables::GetUStrTable().GetStringFromStrIdx(strIdx);
  PrintString(kStr);
}

void Conststr16Node::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  const std::u16string kStr16 = GlobalTables::GetU16StrTable().GetStringFromStrIdx(strIdx);
  // UTF-16 string are dumped as UTF-8 string in mpl to keep the printable chars in ascii form
  std::string str;
  NameMangler::UTF16ToUTF8(str, kStr16);
  PrintString(str);
}

void SizeoftypeNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  LogInfo::MapleLogger() << " ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->Dump(0);
}

void FieldsDistNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  LogInfo::MapleLogger() << " ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->Dump(0);
  LogInfo::MapleLogger() << " " << fieldID1 << " " << fieldID2;
}

void AddrofNode::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
  MIRSymbol *st = mod->CurFunction()->GetLocalOrGlobalSymbol(stIdx);
  LogInfo::MapleLogger() << (stIdx.Islocal() ? " %" : " $");
  LogInfo::MapleLogger() << st->GetName();
  if (fieldID) {
    LogInfo::MapleLogger() << " " << fieldID;
  }
}

void RegreadNode::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
  if (regIdx >= 0) {
    LogInfo::MapleLogger() << " %" << mod->CurFunction()->pregTab->PregFromPregIdx(static_cast<uint32>(regIdx))->pregNo;
  } else {
    LogInfo::MapleLogger() << " %%";
    if (regIdx == -kSregSp) {
      LogInfo::MapleLogger() << "SP";
    } else if (regIdx == -kSregFp) {
      LogInfo::MapleLogger() << "FP";
    } else if (regIdx == -kSregGp) {
      LogInfo::MapleLogger() << "GP";
    } else if (regIdx == -kSregThrownval) {
      LogInfo::MapleLogger() << "thrownval";
    } else if (regIdx == -kSregMethodhdl) {
      LogInfo::MapleLogger() << "methodhdl";
    } else {
      int32 retvalidx = (-regIdx) - kSregRetval0;
      LogInfo::MapleLogger() << "retval" << retvalidx;
    }
  }
}

void AddroffuncNode::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
  MIRFunction *func = GlobalTables::GetFunctionTable().funcTable.at(puIdx);
  LogInfo::MapleLogger() << " &" << GlobalTables::GetGsymTable().GetSymbolFromStIdx(func->stIdx.Idx())->GetName();
}

void AddroflabelNode::Dump(const MIRModule *mod, int32 indent) const {
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op) << " " << GetPrimTypeName(primType);
  LogInfo::MapleLogger() << " @" << mod->CurFunction()->GetLabelName((LabelIdx)offset);
}

void StmtNode::DumpBase(const MIRModule *mod, int32 indent) const {
  if (srcPosition.Filenum() != 0 && srcPosition.Linenum() != 0 && srcPosition.Linenum() != lastPrintedLinenum &&
      mod->CurFunction()->withLocInfo) {
    LogInfo::MapleLogger() << "LOC " << srcPosition.Filenum() << " " << srcPosition.Linenum() << std::endl;
    lastPrintedLinenum = srcPosition.Linenum();
  }
  PrintIndentation(indent);
  LogInfo::MapleLogger() << kOpcodeInfo.GetName(op);
}

void StmtNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << std::endl;
}

void StmtNode::Dump(const MIRModule *mod) const {
  this->BaseNode::Dump(mod);
}

// Get the next stmt skip the comment stmt.
StmtNode *StmtNode::GetRealNext() {
  StmtNode *stmt = this->GetNext();
  while (stmt) {
    if (stmt->op != OP_comment) {
      break;
    }
    stmt = stmt->GetNext();
  }
  return stmt;
}

// insert this before pos
void StmtNode::InsertBefore(StmtNode *pos) {
  ASSERT(pos != nullptr, "null ptr check");
  this->SetNext(pos);
  if (pos->GetPrev()) {
    this->SetPrev(pos->GetPrev());
    pos->GetPrev()->SetNext(this);
  }
  pos->SetPrev(this);
}

// insert this after pos
void StmtNode::InsertAfter(StmtNode *pos) {
  ASSERT(pos != nullptr, "");
  this->SetPrev(pos);
  if (pos->GetNext()) {
    this->SetNext(pos->GetNext());
    pos->GetNext()->SetPrev(this);
  }
  pos->SetNext(this);
}

void DassignNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  MIRSymbol *st = mod->CurFunction()->GetLocalOrGlobalSymbol(stIdx);
  LogInfo::MapleLogger() << (st->IsLocal() ? " %" : " $");
  LogInfo::MapleLogger() << st->GetName() << " " << fieldID;

  LogInfo::MapleLogger() << " (";
  if (GetRhs()) {
    GetRhs()->Dump(mod, indent + 1);
  } else {
    LogInfo::MapleLogger() << "/*empty-rhs*/";
  }
  LogInfo::MapleLogger() << ")" << std::endl;
}

void RegassignNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << " " << GetPrimTypeName(primType);
  if (regIdx >= 0) {
    LogInfo::MapleLogger() << " %" << mod->CurFunction()->pregTab->PregFromPregIdx(static_cast<uint32>(regIdx))->pregNo;
  } else {
    LogInfo::MapleLogger() << " %%";
    if (regIdx == -kSregSp) {
      LogInfo::MapleLogger() << "SP";
    } else if (regIdx == -kSregFp) {
      LogInfo::MapleLogger() << "FP";
    } else if (regIdx == -kSregGp) {
      LogInfo::MapleLogger() << "GP";
    } else if (regIdx == -kSregThrownval) {
      LogInfo::MapleLogger() << "thrownval";
    } else if (regIdx == -kSregMethodhdl) {
      LogInfo::MapleLogger() << "methodhdl";
    } else if (regIdx == -kSregRetval0) {
      LogInfo::MapleLogger() << "retval0";
    }
  }

  LogInfo::MapleLogger() << " (";
  uOpnd->Dump(mod, indent + 1);
  LogInfo::MapleLogger() << ")" << std::endl;
}

void IassignNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << " ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->Dump(0);
  LogInfo::MapleLogger() << " " << fieldID;
  LogInfo::MapleLogger() << " (";
  if (addrExpr->IsLeaf() && rhs->IsLeaf()) {
    addrExpr->Dump(mod, 0);
    LogInfo::MapleLogger() << ", ";
    rhs->Dump(mod, 0);
  } else {
    LogInfo::MapleLogger() << std::endl;
    PrintIndentation(indent + 1);
    addrExpr->Dump(mod, indent + 1);
    LogInfo::MapleLogger() << ", " << std::endl;
    PrintIndentation(indent + 1);
    rhs->Dump(mod, indent + 1);
  }
  LogInfo::MapleLogger() << ")" << std::endl;
}

void IassignoffNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << " " << GetPrimTypeName(primType) << " " << offset;
  BinaryOpnds::Dump(mod, indent);
  LogInfo::MapleLogger() << std::endl;
}

void IassignFPoffNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << " " << GetPrimTypeName(primType) << " " << offset;
  DumpOpnd(mod, indent);
  LogInfo::MapleLogger() << std::endl;
}

void GotoNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  if (offset == 0) {
    LogInfo::MapleLogger() << std::endl;
  } else {
    LogInfo::MapleLogger() << " @" << mod->CurFunction()->GetLabelName((LabelIdx)offset) << std::endl;
  }
}

void TryNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << " {";
  for (size_t i = 0; i < offsets.size(); i++) {
    int64 offset = offsets[i];
    LogInfo::MapleLogger() << " @" << mod->CurFunction()->GetLabelName((LabelIdx)offset);
  }
  LogInfo::MapleLogger() << " }" << std::endl;
}

void TryNode::Dump(const MIRModule *mod) const {
  this->BaseNode::Dump(mod);
}

void JsTryNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  if (catchOffset == 0) {
    LogInfo::MapleLogger() << " 0";
  } else {
    LogInfo::MapleLogger() << " @" << mod->CurFunction()->GetLabelName((LabelIdx)catchOffset);
  }
  if (finallyOffset == 0) {
    LogInfo::MapleLogger() << " 0" << std::endl;
  } else {
    LogInfo::MapleLogger() << " @" << mod->CurFunction()->GetLabelName((LabelIdx)finallyOffset) << std::endl;
  }
}

void CondGotoNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << " @" << mod->CurFunction()->GetLabelName((LabelIdx)offset);
  LogInfo::MapleLogger() << " (";
  uOpnd->Dump(mod, indent);
  LogInfo::MapleLogger() << ")" << std::endl;
}

void SwitchNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << " (";
  switchOpnd->Dump(mod, indent);
  if (defaultLabel == 0) {
    LogInfo::MapleLogger() << ") 0 {";
  } else {
    LogInfo::MapleLogger() << ") @" << mod->CurFunction()->GetLabelName(defaultLabel) << " {";
  }
  for (CaseVector::const_iterator it = switchTable.begin(); it != switchTable.end(); it++) {
    LogInfo::MapleLogger() << std::endl;
    PrintIndentation(indent + 1);
    LogInfo::MapleLogger() << std::hex << "0x" << (it)->first << std::dec;
    LogInfo::MapleLogger() << ": goto @" << mod->CurFunction()->GetLabelName((it)->second);
  }
  LogInfo::MapleLogger() << " }" << std::endl;
}

void RangegotoNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << " (";
  uOpnd->Dump(mod, indent);
  LogInfo::MapleLogger() << ") " << tagOffset << " {";
  for (SmallCaseVector::const_iterator it = rangegotoTable.begin(); it != rangegotoTable.end(); it++) {
    LogInfo::MapleLogger() << std::endl;
    PrintIndentation(indent + 1);
    LogInfo::MapleLogger() << std::hex << "0x" << (it)->first << std::dec;
    LogInfo::MapleLogger() << ": goto @" << mod->CurFunction()->GetLabelName((it)->second);
  }
  LogInfo::MapleLogger() << " }" << std::endl;
}

void MultiwayNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << " (";
  multiWayOpnd->Dump(mod, indent);
  if (defaultLabel == 0) {
    LogInfo::MapleLogger() << ") 0 {";
  } else {
    LogInfo::MapleLogger() << ") @" << mod->CurFunction()->GetLabelName(defaultLabel) << " {";
  }
  for (MCaseVector::const_iterator it = multiWayTable.begin(); it != multiWayTable.end(); it++) {
    LogInfo::MapleLogger() << std::endl;
    PrintIndentation(indent);
    LogInfo::MapleLogger() << " (";
    it->first->Dump(mod, indent + 1);
    LogInfo::MapleLogger() << "): goto @" << mod->CurFunction()->GetLabelName((it)->second);
  }
  LogInfo::MapleLogger() << " }" << std::endl;
}

void UnaryStmtNode::DumpOpnd(const MIRModule *mod, int indent) const {
  LogInfo::MapleLogger() << " (";
  uOpnd->Dump(mod, indent);
  LogInfo::MapleLogger() << ")";
}

void UnaryStmtNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << " (";
  if (uOpnd) {
    uOpnd->Dump(mod, indent);
  }
  LogInfo::MapleLogger() << ")" << std::endl;
}

void UnaryStmtNode::Dump(const MIRModule *mod) const {
  this->BaseNode::Dump(mod);
}

void GCMallocNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  LogInfo::MapleLogger() << " ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->Dump(0);
}

void JarrayMallocNode::Dump(const MIRModule *mod, int32 indent) const {
  BaseNode::DumpBase(mod, 0);
  LogInfo::MapleLogger() << " ";
  GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->Dump(0, false);
  DumpOpnd(mod, indent);
}

void IfStmtNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  LogInfo::MapleLogger() << " (";
  uOpnd->Dump(mod, indent);
  LogInfo::MapleLogger() << ")";
  thenPart->Dump(mod, indent);
  if (elsePart) {
    PrintIndentation(indent);
    LogInfo::MapleLogger() << "else {" << std::endl;
    for (StmtNode *stmt = elsePart->GetFirst(); stmt != nullptr; stmt = stmt->GetNext()) {
      stmt->Dump(mod, indent + 1);
    }
    PrintIndentation(indent);
    LogInfo::MapleLogger() << "}" << std::endl;
  }
}

void WhileStmtNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  if (op == OP_while) {
    LogInfo::MapleLogger() << " (";
    uOpnd->Dump(mod, indent);
    LogInfo::MapleLogger() << ")";
    body->Dump(mod, indent);
  } else {  // OP_dowhile
    LogInfo::MapleLogger() << " {" << std::endl;
    for (StmtNode *stmt = body->GetFirst(); stmt; stmt = stmt->GetNext()) {
      stmt->Dump(mod, indent + 1);
    }
    PrintIndentation(indent);
    LogInfo::MapleLogger() << "} (";
    uOpnd->Dump(mod, indent);
    LogInfo::MapleLogger() << ")" << std::endl;
  }
}

void DoloopNode::DumpDovar(const MIRModule *mod) const {
  if (isPreg) {
    LogInfo::MapleLogger() << " %" << mod->CurFunction()->pregTab->PregFromPregIdx(doVarStIdx.FullIdx())->pregNo << " ("
              << std::endl;
  } else {
    MIRSymbol *st = mod->CurFunction()->GetLocalOrGlobalSymbol(doVarStIdx);
    LogInfo::MapleLogger() << " %" << st->GetName().c_str() << " (" << std::endl;
  }
}

void DoloopNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  DumpDovar(mod);
  PrintIndentation(indent + 1);
  startExpr->Dump(mod, indent + 1);
  LogInfo::MapleLogger() << "," << std::endl;
  PrintIndentation(indent + 1);
  condExpr->Dump(mod, indent + 1);
  LogInfo::MapleLogger() << "," << std::endl;
  PrintIndentation(indent + 1);
  incrExpr->Dump(mod, indent + 1);
  LogInfo::MapleLogger() << ")";
  doBody->Dump(mod, indent + 1);
}

void ForeachelemNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  MIRSymbol *st = mod->CurFunction()->GetLocalOrGlobalSymbol(elemStIdx);
  LogInfo::MapleLogger() << " %" << st->GetName().c_str();
  st = mod->CurFunction()->GetLocalOrGlobalSymbol(arrayStIdx);
  LogInfo::MapleLogger() << (arrayStIdx.Islocal() ? " %" : " $");
  LogInfo::MapleLogger() << st->GetName().c_str();
  loopBody->Dump(mod, indent + 1);
}

void BinaryStmtNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  BinaryOpnds::Dump(mod, indent);
  LogInfo::MapleLogger() << std::endl;
}

void AssertStmtNode::Dump(const MIRModule *mod, int32 indent) const {
  BinaryStmtNode::Dump(mod, indent);
}

void NaryStmtNode::Dump(const MIRModule *mod, int32 indent) const {
  StmtNode::DumpBase(mod, indent);
  NaryOpnds::Dump(mod, indent);
  LogInfo::MapleLogger() << std::endl;
}

void DumpCallReturns(const MIRModule *mod, CallReturnVector returnValues, int32 indent) {
  MIRFunction *mirfunc = mod->CurFunction();
  if (!returnValues.size()) {
    LogInfo::MapleLogger() << " {}" << std::endl;
    return;
  } else if (returnValues.size() == 1) {
    StIdx stIdx = returnValues.begin()->first;
    RegFieldPair regfieldpair = returnValues.begin()->second;
    if (!regfieldpair.IsReg()) {
      MIRSymbol *st = mod->CurFunction()->GetLocalOrGlobalSymbol(stIdx);
      uint16 fieldID = regfieldpair.GetFieldid();
      LogInfo::MapleLogger() << " { dassign ";
      LogInfo::MapleLogger() << (stIdx.Islocal() ? "%" : "$");
      LogInfo::MapleLogger() << st->GetName() << " " << fieldID << " }" << std::endl;
      return;
    } else {
      PregIdx16 regIdx = regfieldpair.GetPregidx();
      MIRPreg *mirpreg = mirfunc->pregTab->PregFromPregIdx(static_cast<uint32>(regIdx));
      LogInfo::MapleLogger() << " { regassign";
      LogInfo::MapleLogger() << " " << GetPrimTypeName(mirpreg->primType);
      LogInfo::MapleLogger() << " %" << mirpreg->pregNo << "}" << std::endl;
      return;
    }
  }

  LogInfo::MapleLogger() << " {" << std::endl;
  for (CallReturnVector::iterator it = returnValues.begin(); it != returnValues.end(); it++) {
    PrintIndentation(indent + 2);
    StIdx stIdx = (it)->first;
    RegFieldPair regfieldpair = it->second;
    if (!regfieldpair.IsReg()) {
      uint16 fieldID = regfieldpair.fieldID;
      LogInfo::MapleLogger() << "dassign";
      MIRSymbol *st = mirfunc->GetLocalOrGlobalSymbol(stIdx);
      LogInfo::MapleLogger() << (stIdx.Islocal() ? " %" : " $");
      LogInfo::MapleLogger() << st->GetName() << " " << fieldID;
      LogInfo::MapleLogger() << std::endl;
    } else {
      PregIdx16 regIdx = regfieldpair.pregIdx;
      MIRPreg *mirpreg = mirfunc->pregTab->PregFromPregIdx(static_cast<uint32>(regIdx));
      LogInfo::MapleLogger() << "regassign";
      LogInfo::MapleLogger() << " " << GetPrimTypeName(mirpreg->primType);
      LogInfo::MapleLogger() << " %" << mirpreg->pregNo << std::endl;
    }
  }
  PrintIndentation(indent + 1);
  LogInfo::MapleLogger() << "}" << std::endl;
}

MIRType *CallNode::GetCallReturnType() {
  if (!kOpcodeInfo.IsCallAssigned(op)) {
    return nullptr;
  }
  MIRFunction *mirFunc = GlobalTables::GetFunctionTable().funcTable[puIdx];
  return mirFunc->GetReturnType();
}

void CallNode::Dump(const MIRModule *mod, int32 indent, bool newline) const {
  StmtNode::DumpBase(mod, indent);
  if (tyIdx != 0) {
    LogInfo::MapleLogger() << " ";
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->Dump(indent + 1);
  }
  MIRFunction *func = GlobalTables::GetFunctionTable().funcTable.at(puIdx);
  LogInfo::MapleLogger() << " &" << func->GetName();
  NaryOpnds::Dump(mod, indent);
  if (kOpcodeInfo.IsCallAssigned(op)) {
    DumpCallReturns(mod, this->returnValues, indent);
  } else if (newline) {
    LogInfo::MapleLogger() << std::endl;
  }
}

MIRType *IcallNode::GetCallReturnType() {
  return GlobalTables::GetTypeTable().GetTypeFromTyIdx(retTyIdx);
}

void IcallNode::Dump(const MIRModule *mod, int32 indent, bool newline) const {
  StmtNode::DumpBase(mod, indent);
  NaryOpnds::Dump(mod, indent);
  if (kOpcodeInfo.IsCallAssigned(op)) {
    DumpCallReturns(mod, this->returnValues, indent);
  } else if (newline) {
    LogInfo::MapleLogger() << std::endl;
  }
}

MIRType *IntrinsiccallNode::GetCallReturnType() {
  IntrinDesc *intrindesc = &IntrinDesc::intrintable[intrinsic];
  return intrindesc->GetReturnType();
}

void IntrinsiccallNode::Dump(const MIRModule *mod, int32 indent, bool newline) const {
  StmtNode::DumpBase(mod, indent);
  if (tyIdx != 0) {
    LogInfo::MapleLogger() << " ";
    GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->Dump(indent + 1);
  }
  if (op == OP_intrinsiccall || op == OP_intrinsiccallassigned ||
      op == OP_intrinsiccallwithtype || op == OP_intrinsiccallwithtypeassigned) {
    LogInfo::MapleLogger() << " " << GetIntrinsicName(intrinsic);
  } else {
    LogInfo::MapleLogger() << " " << intrinsic;
  }
  NaryOpnds::Dump(mod, indent);
  if (kOpcodeInfo.IsCallAssigned(op)) {
    DumpCallReturns(mod, this->returnValues, indent);
  } else if (newline) {
    LogInfo::MapleLogger() << std::endl;
  }
}

void CallinstantNode::Dump(const MIRModule *mod, int32 indent, bool newline) const {
  StmtNode::DumpBase(mod, indent);
  MIRFunction *func = GlobalTables::GetFunctionTable().funcTable.at(puIdx);
  LogInfo::MapleLogger() << " &" << func->GetName();
  MIRType *ty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(instVecTyIdx);
  LogInfo::MapleLogger() << "<";
  MIRInstantVectorType *instvecty = static_cast<MIRInstantVectorType *>(ty);
  instvecty->Dump(indent);
  LogInfo::MapleLogger() << ">";
  NaryOpnds::Dump(mod, indent);
  if (kOpcodeInfo.IsCallAssigned(op)) {
    DumpCallReturns(mod, this->returnValues, indent);
  } else if (newline) {
    LogInfo::MapleLogger() << std::endl;
  }
}

void BlockNode::Dump(const MIRModule *mod, int32 indent, const MIRSymbolTable *thesymtab,
                      MIRPregTable *thepregTab, bool withInfo, bool isFuncbody) const {
  if (!withInfo) {
    LogInfo::MapleLogger() << " {" << std::endl;
  }

  // output puid for debugging purpose
  // PrintIndentation(indent+1);

  if (isFuncbody) {
    LogInfo::MapleLogger() << "  funcid " << mod->CurFunction()->puIdxOrigin << std::endl;

    lastPrintedLinenum = 0; // this ensures at least 1 LOC is printed func body
    if (!mod->CurFunction()->IsInfoPrinted()) {
      MIRFunction *curFunc = mod->CurFunction();
      curFunc->SetInfoPrinted();
      if (curFunc->upFormalSize) {
        PrintIndentation(indent + 1);
        LogInfo::MapleLogger() << "upFormalSize " << static_cast<uint32>(curFunc->upFormalSize) << std::endl;
        if (curFunc->formalWordsTypeTagged != nullptr) {
          PrintIndentation(indent + 1);
          LogInfo::MapleLogger() << "formalwordstypetagged = [ ";
          uint32 *p = reinterpret_cast<uint32 *>(curFunc->formalWordsTypeTagged);
          LogInfo::MapleLogger() << std::hex;
          while (p < reinterpret_cast<uint32 *>(curFunc->formalWordsTypeTagged +
                                                BlkSize2BitvectorSize(curFunc->upFormalSize))) {
            LogInfo::MapleLogger() << std::hex << "0x" << *p << " ";
            p++;
          }
          LogInfo::MapleLogger() << std::dec << "]\n";
        }
        if (curFunc->formalWordsRefCounted != nullptr) {
          PrintIndentation(indent + 1);
          LogInfo::MapleLogger() << "formalwordsrefcounted = [ ";
          uint32 *p = reinterpret_cast<uint32 *>(curFunc->formalWordsRefCounted);
          LogInfo::MapleLogger() << std::hex;
          while (p < reinterpret_cast<uint32 *>(curFunc->formalWordsRefCounted +
                                                BlkSize2BitvectorSize(curFunc->upFormalSize))) {
            LogInfo::MapleLogger() << std::hex << "0x" << *p << " ";
            p++;
          }
          LogInfo::MapleLogger() << std::dec << "]\n";
        }
      }
      if (curFunc->frameSize) {
        PrintIndentation(indent + 1);
        LogInfo::MapleLogger() << "frameSize " << curFunc->frameSize << std::endl;
        if (curFunc->localWordsTypeTagged != nullptr) {
          PrintIndentation(indent + 1);
          LogInfo::MapleLogger() << "localwordstypetagged = [ ";
          uint32 *p = reinterpret_cast<uint32 *>(curFunc->localWordsTypeTagged);
          LogInfo::MapleLogger() << std::hex;
          while (p < reinterpret_cast<uint32 *>(curFunc->localWordsTypeTagged +
                                                BlkSize2BitvectorSize(curFunc->frameSize))) {
            LogInfo::MapleLogger() << std::hex << "0x" << *p << " ";
            p++;
          }
          LogInfo::MapleLogger() << std::dec << "]\n";
        }
        if (curFunc->localWordsRefCounted != nullptr) {
          PrintIndentation(indent + 1);
          LogInfo::MapleLogger() << "localwordsrefcounted = [ ";
          uint32 *p = reinterpret_cast<uint32 *>(curFunc->localWordsRefCounted);
          LogInfo::MapleLogger() << std::hex;
          while (p < reinterpret_cast<uint32 *>(curFunc->localWordsRefCounted +
                                                BlkSize2BitvectorSize(curFunc->frameSize))) {
            LogInfo::MapleLogger() << std::hex << "0x" << *p << " ";
            p++;
          }
          LogInfo::MapleLogger() << std::dec << "]\n";
        }
      }
      if (curFunc->moduleID) {
        PrintIndentation(indent + 1);
        LogInfo::MapleLogger() << "moduleid " << static_cast<uint32>(curFunc->moduleID) << std::endl;
      }
      if (curFunc->funcSize) {
        PrintIndentation(indent + 1);
        LogInfo::MapleLogger() << "funcSize " << curFunc->funcSize << std::endl;
      }

      if (curFunc->info.size() != 0) {
        MIRInfoVector &fninfo = curFunc->info;
        MapleVector<bool> &fninfoIsstring = curFunc->infoIsString;
        PrintIndentation(indent + 1);
        LogInfo::MapleLogger() << "funcinfo {\n";
        uint32 size = fninfo.size();
        for (size_t i = 0; i < size; i++) {
          PrintIndentation(indent + 2);
          LogInfo::MapleLogger() << "@" << GlobalTables::GetStrTable().GetStringFromStrIdx(fninfo[i].first) << " ";
          if (!fninfoIsstring[i]) {
            LogInfo::MapleLogger() << fninfo[i].second;
          } else {
            LogInfo::MapleLogger() << "\"" << GlobalTables::GetStrTable().GetStringFromStrIdx(fninfo[i].second) << "\"";
          }
          if (i < size - 1) {
            LogInfo::MapleLogger() << ",\n";
          } else {
            LogInfo::MapleLogger() << "}\n";
          }
        }
        LogInfo::MapleLogger() << std::endl;
      }
    }
    if (thesymtab) {
      // print the locally declared type names
      for (auto it : mod->CurFunction()->typeNameTab->gStrIdxToTyIdxMap) {
        const std::string &name = GlobalTables::GetStrTable().GetStringFromStrIdx(it.first);
        MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(it.second);
        PrintIndentation(indent + 1);
        LogInfo::MapleLogger() << "type %" << name << " ";
        if (type->GetKind() != kTypeByName) {
          type->Dump(indent + 2, true);
        } else {
          type->Dump(indent + 2);
        }
        LogInfo::MapleLogger() << std::endl;
      }
      // print the locally declared variables
      thesymtab->Dump(true, indent + 1);
      thepregTab->DumpRef(indent + 1);
    }
    LogInfo::MapleLogger() << std::endl;
    for (std::pair<GStrIdx, MIRAliasVars> it : mod->CurFunction()->aliasVarMap) {
      LogInfo::MapleLogger() << "ALIAS %" << GlobalTables::GetStrTable().GetStringFromStrIdx(it.first) << " %"
                << GlobalTables::GetStrTable().GetStringFromStrIdx(it.second.memPoolStrIdx) << " ";
      GlobalTables::GetTypeTable().GetTypeFromTyIdx(it.second.tyIdx)->Dump(0);
      if (it.second.sigStrIdx.GetIdx()) {
        LogInfo::MapleLogger() << " \"" << GlobalTables::GetStrTable().GetStringFromStrIdx(it.second.sigStrIdx) << "\"";
      }
      LogInfo::MapleLogger() << std::endl;
    }
  }

  if (srcPosition.Filenum() != 0 && srcPosition.Linenum() != 0 && srcPosition.Linenum() != lastPrintedLinenum &&
      mod->CurFunction()->withLocInfo) {
    LogInfo::MapleLogger() << "LOC " << srcPosition.Filenum() << " " << srcPosition.Linenum() << std::endl;
    lastPrintedLinenum = srcPosition.Linenum();
  }
  for (StmtNode &stmt : GetStmtNodes()) {
    stmt.Dump(mod, indent + 1);
  }
  PrintIndentation(indent);
  LogInfo::MapleLogger() << "}" << std::endl;
}

void LabelNode::Dump(const MIRModule *mod, int32 indent) const {
  if (srcPosition.Filenum() != 0 && srcPosition.Linenum() != 0 && srcPosition.Linenum() != lastPrintedLinenum &&
      mod->CurFunction()->withLocInfo) {
    LogInfo::MapleLogger() << "LOC " << srcPosition.Filenum() << " " << srcPosition.Linenum() << std::endl;
    lastPrintedLinenum = srcPosition.Linenum();
  }
  LogInfo::MapleLogger() << "@" << mod->CurFunction()->GetLabelName(labelIdx) << " ";
}

void CommentNode::Dump(const MIRModule *mod, int32 indent) const {
  if (srcPosition.Filenum() != 0 && srcPosition.Linenum() != 0 && srcPosition.Linenum() != lastPrintedLinenum &&
      mod->CurFunction()->withLocInfo) {
    LogInfo::MapleLogger() << "LOC " << srcPosition.Filenum() << " " << srcPosition.Linenum() << std::endl;
    lastPrintedLinenum = srcPosition.Linenum();
  }
  PrintIndentation(indent);
  LogInfo::MapleLogger() << "#" << comment << std::endl;
}

// Start of type verification for Maple IR nodes.
//
// General rules:
//
// 1. For binary operations, the types of the two operands must be compatible.
//
// 2. In checking type compatibility, other than identical types, the types in
// each of the following group are compatible with each other:
//            [i32, u32, ptr, ref, a32]
//            [i64, u64, ptr, ref, a64]
//
// 3. dynany is compatiable with any dyn* type.
//
// 4. u1, i8, u8, i16, u16 must not be types of arithmetic operations, because
// many machines do not provide instructions for such types as they lack such
// register sizes.  Similarly, these types must not be used as result types for
// any read instructions: dread/iread/regread/ireadoff/ireadfpoff.
//
// 5. When an opcode only specifies one type (which is called the result type),
// it expects both operands and results to be of this same type.  Thus, the
// types among the operands and this result type must be compatible.
//
// 6. When an opcode specifies two types, the additional (second) type is
// the operand type.  The types of the operands and the operand type must be
// compatible.
//
// 7. The opcodes addrof, addroflabel, addroffunc and iaddrof form addresses.
// Thus, their result type must be in [ptr,ref,a32,a64].
//
// 8. The opcodes bnot, extractbits, sext, zext, lnot must have result type in
// [i32, u32, i64, u64].
//
// 9. The opcodes abs, neg must have result type in
// [i32, u32, i64, u64, f32, f64].
//
// 10. The opcodes recip, sqrt must have result type in [f32, f64].
//
// 11. The opcodes ceil, floor, round, trunc must have result-type in
// [i32, u32, i64, u64] and operand-type in [f32, f64].
//
// 12. The opcodes add, div, sub, mpy, max, min must have result-type in
// [i32, u32, i64, u64, f32, f64].
//
// 13. The opcodes eq, ge, gt, le, lt, ne must have result-type in
// any signed or unsigned integer type; they also specifies operand-type, and
// this operand-type and the types of their two operands must be compatible.
//
// 14. The opcodes ashr, band, bior, bxor, depositbits, land, lior, lshr, shl,
// rem must have  result-type in [i32, u32, i64, u64].
//
// 15. select's result-type and the types of its second and third operands must
// be compatible; its first operand must be of integer type.
//
// 16. array's result-type must be in [ptr,ref,a32,a64]; the type of <opnd0> must
// also be in [ptr,ref,a32,a64]; the types of the rest of the operands must be in
// [i32, u32, i64, u64].
//
// 17. The operand of brfalse, trfalse must be of integer type.
//
// 18. The operand of switch, rangegoto must be in [i32, u32, i64, u64].

static bool ExcludeSmallIntTypeVerify(const BaseNode *opnd) {
  switch (opnd->primType) {
    case PTY_u1:
    case PTY_i8:
    case PTY_u8:
    case PTY_i16:
    case PTY_u16:
      return false;
    default:
      break;
  }
  return true;
}

static bool ArithTypeVerify(const BaseNode *opnd) {
  ASSERT(opnd != nullptr, "null ptr check");
  bool verifyResult = ExcludeSmallIntTypeVerify(opnd);
  if (!verifyResult) {
    LogInfo::MapleLogger() << "\n#Error:u1,i8,u8,i16,u16 should not be used as types of arithmetic operations\n";
    opnd->Dump(theModule);
  }
  return verifyResult;
}

static inline bool ReadTypeVerify(const BaseNode *opnd) {
  bool verifyResult = ExcludeSmallIntTypeVerify(opnd);
  if (!verifyResult) {
    LogInfo::MapleLogger()
        << "\n#Error:u1,i8,u8,i16,u16 should not be used as result types for dread/iread/regread/ireadoff/ireadfpoff\n";
    opnd->Dump(theModule);
  }
  return verifyResult;
}

static inline bool IntTypeVerify(PrimType ptyp) {
  return ptyp == PTY_i32 || ptyp == PTY_u32 || ptyp == PTY_i64 || ptyp == PTY_u64;
}

static inline bool UnaryTypeVerify0(PrimType ptyp) {
  bool verifyResult = IntTypeVerify(ptyp);
  if (!verifyResult) {
    LogInfo::MapleLogger() << "\n#Error:result type of bnot,extractbits,sext,zext must be in [i32,u32,i64,u64]\n";
  }
  return verifyResult;
}

static bool ArithResTypeVerify(PrimType ptyp) {
  switch (ptyp) {
    case PTY_i32:
    case PTY_u32:
    case PTY_i64:
    case PTY_u64:
    case PTY_f32:
    case PTY_f64:
      return true;
    case PTY_a32:
    case PTY_a64:
    case PTY_ptr:
      return theModule->IsCModule();
    default:
      break;
  }
  return false;
}

static inline bool UnaryTypeVerify1(PrimType ptyp) {
  bool verifyResult = ArithResTypeVerify(ptyp);
  if (!verifyResult) {
    LogInfo::MapleLogger() << "\n#Error:result type of abs,neg must be in [i32,u32,i64,u64,f32,f64]\n";
  }
  return verifyResult;
}

static inline bool UnaryTypeVerify2(PrimType ptyp) {
  bool verifyResult = IsPrimitiveFloat(ptyp);
  if (!verifyResult) {
    LogInfo::MapleLogger() << "\n#Error:result-type of recip,sqrt must be in [f32,f64]\n";
  }
  return verifyResult;
}

static inline bool BinaryTypeVerify(PrimType ptyp) {
  return ArithResTypeVerify(ptyp) || IsPrimitiveDynType(ptyp);
}

static inline bool BinaryGenericVerify(const BaseNode *bOpnd0, const BaseNode *bOpnd1) {
  ASSERT(bOpnd0 != nullptr, "null ptr check");
  ASSERT(bOpnd1 != nullptr, "null ptr check");
  return bOpnd0->Verify() && bOpnd1->Verify() && ArithTypeVerify(bOpnd0) && ArithTypeVerify(bOpnd1);
}

static inline bool CompareTypeVerify(PrimType pType) {
  bool verifyResult = IsPrimitiveInteger(pType);
  if (!verifyResult) {
    LogInfo::MapleLogger() << "\n#Error:result type of eq,ge,gt,le,lt,ne must be primitive integer\n";
  }
  return verifyResult;
}

enum PTYGroup {
  kPTYGi32u32a32,
  kPTYGi32u32a32PtrRef,
  kPTYGi64u64a64,
  kPTYGPtrRef,
  kPTYGDynall,
  kPTYGu1,
  kPTYGSimpleObj,
  kPTYGSimpleStr,
  kPTYGOthers
};

static uint8 GetCompGroupID(const BaseNode *opnd) {
  switch (opnd->primType) {
    case PTY_i32:
    case PTY_u32:
    case PTY_a32:
      return kPTYGi32u32a32;
    case PTY_i64:
    case PTY_u64:
    case PTY_a64:
      return kPTYGi64u64a64;
    case PTY_ref:
    case PTY_ptr:
      return kPTYGPtrRef;
    case PTY_dynany:
    case PTY_dyni32:
    case PTY_dynf64:
    case PTY_dynstr:
    case PTY_dynobj:
    case PTY_dynundef:
    case PTY_dynbool:
    case PTY_dynf32:
    case PTY_dynnone:
    case PTY_dynnull:
      return kPTYGDynall;
    case PTY_u1:
      return kPTYGu1;
    case PTY_simpleobj:
      return kPTYGSimpleObj;
    case PTY_simplestr:
      return kPTYGSimpleStr;
    default:
      return kPTYGOthers;
  }
}

/*
  Refer to C11 Language Specification.
  $ 6.3.1.8 Usual arithmetic conversions
 */
static bool CompatibleTypeVerify(const BaseNode *opnd1, const BaseNode *opnd2) {
  uint8 groupID1 = GetCompGroupID(opnd1);
  uint8 groupID2 = GetCompGroupID(opnd2);
  Opcode opCode2 = opnd2->op;
  bool verifyResult = groupID1 == groupID2;
  if (opCode2 == OP_gcmallocjarray || opCode2 == OP_gcpermallocjarray) {
    verifyResult = groupID1 == kPTYGi32u32a32;
  }
  if (!verifyResult) {
    LogInfo::MapleLogger() << "\n#Error:incompatible operand types :\n";
    opnd1->Dump(theModule);
    opnd2->Dump(theModule);
  }
  return verifyResult;
}

static bool FloatIntCvtTypeVerify(PrimType resPType, PrimType opndPType) {
  bool resTypeVerf = resPType == PTY_i32 || resPType == PTY_u32 || resPType == PTY_i64 || resPType == PTY_u64;
  if (!resTypeVerf) {
    LogInfo::MapleLogger() << "\n#Error:result-type of ceil,floor,round,trunc must be in [i32,u32,i64,u64]\n";
  }
  bool opndTypeVerf = opndPType == PTY_f32 || opndPType == PTY_f64;
  if (!opndTypeVerf) {
    LogInfo::MapleLogger() << "\n#Error:oerand-type of ceil,floor,round,trunc must be in [f32,f64]\n";
  }
  return resTypeVerf && opndTypeVerf;
}

static inline MIRTypeKind GetTypeKind(StIdx stIdx) {
  MIRSymbol *var = theModule->CurFunction()->GetLocalOrGlobalSymbol(stIdx);
  ASSERT(var != nullptr, "null ptr check");
  MIRType *type = var->GetType();
  ASSERT(type != nullptr, "null ptr check");
  return type->GetKind();
}

static inline MIRTypeKind GetTypeKind(TyIdx tyIdx) {
  MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  ASSERT(type != nullptr, "null ptr check");
  return type->GetKind();
}

static inline MIRType *GetPointedMIRType(TyIdx tyIdx) {
  MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  ASSERT(type != nullptr, "null ptr check");
  MIRPtrType *ptrType = dynamic_cast<MIRPtrType*>(type);
  ASSERT(ptrType != nullptr, "null pointer check");
  return ptrType->GetPointedType();
}

static inline MIRTypeKind GetPointedTypeKind(TyIdx tyIdx) {
  MIRType *pointedType = GetPointedMIRType(tyIdx);
  ASSERT(pointedType != nullptr, "null ptr check");
  return pointedType->GetKind();
}

static bool GetFieldType(const MIRStructType *structType, FieldID targetFid, TyIdx &tid) {
  if (structType == nullptr) {
    return false;
  }
  // For Java module class, find targetFid in inheritance chain firstly
  if (theModule->IsJavaModule()) {
    if (structType->GetKind() == kTypeClass || structType->GetKind() == kTypeClassIncomplete) {
      const MIRClassType *classType = static_cast<const MIRClassType*>(structType);
      std::stack<MIRStructType*> inheritChain;
      TyIdx parentTyIdx = classType->parentTyIdx;
      while (parentTyIdx.GetIdx() > 0) {
        MIRStructType *parentType =
            static_cast<MIRStructType*>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(parentTyIdx));
        inheritChain.push(parentType);
        parentTyIdx = static_cast<MIRClassType*>(parentType)->parentTyIdx;
      }
      targetFid -= inheritChain.size();
      while (!inheritChain.empty()) {
        MIRClassType *curClassType = static_cast<MIRClassType*>(inheritChain.top());
        if (0 < static_cast<uint32>(targetFid) && static_cast<uint32>(targetFid) <= curClassType->fields.size()) {
          tid = curClassType->fields.at(targetFid - 1).second.first;
          return true;
        } else {
          targetFid -= curClassType->fields.size();
        }
        inheritChain.pop();
      }
      if (0 < static_cast<uint32>(targetFid) && static_cast<uint32>(targetFid) <= classType->fields.size()) {
        tid = classType->fields.at(targetFid - 1).second.first;
        return true;
      }
    }
  }
  return false;
}

static MIRTypeKind GetFieldTypeKind(const MIRStructType *structType, FieldID fieldID) {
  TyIdx fieldTyIdx;
  if (fieldID > 0) {
    bool isValid = GetFieldType(structType, fieldID, fieldTyIdx);
    // when mpl does not have complete class info
    if (!isValid) {
      LogInfo::MapleLogger() << "\n#Error:field not found, must have complete class info\n";
      return kTypeInvalid;
    }
  } else {
    ASSERT(static_cast<unsigned>(-fieldID) < structType->parentFields.size() + 1, "array index out of range");
    fieldTyIdx = structType->parentFields.at(-fieldID - 1).second.first;
  }
  return GetTypeKind(fieldTyIdx);
}

static inline bool IsStructureTypeKind(MIRTypeKind kind) {
  return kind == kTypeStruct || kind == kTypeStructIncomplete || kind == kTypeUnion || kind == kTypeClass ||
         kind == kTypeClassIncomplete || kind == kTypeInterface || kind == kTypeInterfaceIncomplete;
}

static inline bool IsStructureVerify(FieldID fieldID, StIdx stIdx) {
  if (fieldID != 0) {
    if (!IsStructureTypeKind(GetTypeKind(stIdx))) {
      LogInfo::MapleLogger() << "\n#Error:if fieldID is not 0, the variable must be a structure\n";
      return false;
    }
  }
  return true;
}

static bool IsSignedType(const BaseNode *opnd) {
  switch (opnd->primType) {
    case PTY_i32:
    case PTY_i64:
    case PTY_f32:
    case PTY_f64:
    case PTY_dyni32:
    case PTY_dynf32:
    case PTY_dynf64:
      return true;
    default:
      break;
  }
  return false;
}

static inline bool BinaryStrictSignVerify0(const BaseNode *bOpnd0, const BaseNode *bOpnd1) {
  ASSERT(bOpnd0 != nullptr, "bOpnd0 is null");
  ASSERT(bOpnd1 != nullptr, "bOpnd1 is null");
  bool isDynany = (bOpnd0->primType == PTY_dynany || bOpnd1->primType == PTY_dynany);
  return isDynany || (IsSignedType(bOpnd0) && IsSignedType(bOpnd1)) || (!IsSignedType(bOpnd0) && !IsSignedType(bOpnd1));
}

static bool BinaryStrictSignVerify1(const BaseNode *bOpnd0, const BaseNode *bOpnd1, const BaseNode *res) {
  if (GetCompGroupID(res) == kPTYGDynall) {
    return BinaryStrictSignVerify0(bOpnd0, res) && BinaryStrictSignVerify0(bOpnd1, res) &&
           BinaryStrictSignVerify0(bOpnd0, bOpnd1);
  }
  return (IsSignedType(bOpnd0) && IsSignedType(bOpnd1) && IsSignedType(res)) ||
         (!IsSignedType(bOpnd0) && !IsSignedType(bOpnd1) && !IsSignedType(res));
}

bool UnaryNode::Verify() const {
  bool resTypeVerf = true;
  if (op == OP_bnot) {
    resTypeVerf = UnaryTypeVerify0(primType);
  } else if (op == OP_lnot) {
    if (!IsPrimitiveInteger(primType)) {
      resTypeVerf = false;
      LogInfo::MapleLogger() << "\n#Error:result-type of lnot must be primitive integer\n";
    }
  } else if (op == OP_abs || op == OP_neg) {
    resTypeVerf = UnaryTypeVerify1(primType);
  } else if (op == OP_recip || op == OP_sqrt) {
    resTypeVerf = UnaryTypeVerify2(primType);
  }
  bool opndTypeVerf = true;
  if (op != OP_lnot) {
    opndTypeVerf = ArithTypeVerify(uOpnd);
  }
  // When an opcode only specifies one type, check for compatibility
  // between the operands and the result-type.
  bool compVerf = CompatibleTypeVerify(uOpnd, this);
  bool opndExprVerf = uOpnd->Verify();
  return resTypeVerf && opndTypeVerf && compVerf && opndExprVerf;
}

bool TypeCvtNode::Verify() const {
  bool opndTypeVerf = true;
  bool opndSizeVerf = true;
  if (op == OP_ceil || op == OP_floor || op == OP_round || op == OP_trunc) {
    opndTypeVerf = FloatIntCvtTypeVerify(primType, uOpnd->primType);
  } else if (op == OP_retype) {
    if (GetPrimTypeSize(primType) != GetPrimTypeSize(uOpnd->primType)) {
      opndSizeVerf = false;
      LogInfo::MapleLogger() << "\n#Error:The size of opnd0 and prim-type must be the same\n";
    }
  }
  bool opndExprVerf = uOpnd->Verify();
  return opndTypeVerf && opndSizeVerf && opndExprVerf;
}

bool IreadNode::Verify() const {
  bool addrExprVerf = uOpnd->Verify();
  bool pTypeVerf = ReadTypeVerify(this);
  bool structVerf = true;
  if (GetTypeKind(tyIdx) != kTypePointer) {
    LogInfo::MapleLogger() << "\n#Error:<type> must be a pointer type\n";
    return false;
  }
  if (op == OP_iaddrof) {
    pTypeVerf = IsAddress(primType);
    if (!pTypeVerf) {
      LogInfo::MapleLogger() << "\n#Error:prim-type must be either ptr, ref, a32 or a64\n";
    }
  } else {
    if (fieldID == 0 && IsStructureTypeKind(GetPointedTypeKind(tyIdx))) {
      if (primType != PTY_agg) {
        pTypeVerf = false;
        LogInfo::MapleLogger()
            << "\n#Error:If the content dereferenced is a structure, then <prim-type> should specify agg\n";
      }
    }
  }
  if (fieldID != 0) {
    if (!IsStructureTypeKind(GetPointedTypeKind(tyIdx))) {
      structVerf = false;
      LogInfo::MapleLogger() << "\n#Error:If field-id is not 0, then type must specify pointer to a structure\n";
    } else {
      MIRType *type = GetPointedMIRType(tyIdx);
      MIRStructType *stty = dynamic_cast<MIRStructType*>(type);
      if (op == OP_iread && stty->fields.size() != 0) {
        if (IsStructureTypeKind(GetFieldTypeKind(stty, fieldID))) {
          if (primType != PTY_agg) {
            pTypeVerf = false;
            LogInfo::MapleLogger() << "\n#Error:If the field itself is a structure, prim-type should specify agg\n";
          }
        }
      }
    }
  }
  return addrExprVerf && pTypeVerf && structVerf;
}

bool RegreadNode::Verify() const {
  bool pTypeVerf = ReadTypeVerify(this);
  return pTypeVerf;
}

bool IreadoffNode::Verify() const {
  bool pTypeVerf = ReadTypeVerify(this);
  return pTypeVerf;
}

bool IreadFPoffNode::Verify() const {
  bool pTypeVerf = ReadTypeVerify(this);
  return pTypeVerf;
}

bool ExtractbitsNode::Verify() const {
  bool opndExprVerf = uOpnd->Verify();
  bool opndTypeVerf = ArithTypeVerify(uOpnd);
  bool compVerf = CompatibleTypeVerify(uOpnd, this);
  bool resTypeVerf = UnaryTypeVerify0(primType);
  constexpr int kNumBitsInByte = 8;
  bool opnd0SizeVerf = (kNumBitsInByte * GetPrimTypeSize(uOpnd->primType) >= bitsSize);
  if (!opnd0SizeVerf) {
    LogInfo::MapleLogger()
        << "\n#Error: The operand of extractbits must be large enough to contain the specified bitfield\n";
  }
  return opndExprVerf && opndTypeVerf && compVerf && resTypeVerf && opnd0SizeVerf;
}

bool BinaryNode::Verify() const {
  bool opndsVerf = BinaryGenericVerify(bOpnd[0], bOpnd[1]);
  bool resTypeVerf = BinaryTypeVerify(primType);
  if (!resTypeVerf && theModule->IsCModule()) {
    if ((IsAddress(bOpnd[0]->primType) && !IsAddress(bOpnd[1]->primType)) ||
        (!IsAddress(bOpnd[0]->primType) && IsAddress(bOpnd[1]->primType))) {
      resTypeVerf = true;  // don't print the same kind of error message twice
      if (op != OP_add && op != OP_sub) {
        LogInfo::MapleLogger() << "\n#Error: Only add and sub are allowed for pointer arithemetic\n";
        this->Dump(theModule);
      } else if (!IsAddress(primType)) {
        LogInfo::MapleLogger()
            << "\n#Error: Adding an offset to a pointer or subtracting one from a pointer should result in a pointer "
               "value\n";
        this->Dump(theModule);
      }
    }
  }
  if (!resTypeVerf) {
    LogInfo::MapleLogger()
        << "\n#Error:result type of [add,div,sub,mul,max,min] and [ashr,band,bior,bxor,land,lior,lshr,shl,rem] must "
           "be in [i32,u32,i64,u64,f32,f64,dynamic-type]\n";
    this->Dump(theModule);
  }
  bool comp0Verf = CompatibleTypeVerify(bOpnd[0], this);
  bool comp1Verf = CompatibleTypeVerify(bOpnd[1], this);
  bool signVerf = true;
  bool typeVerf = resTypeVerf && comp0Verf && comp1Verf;
  if (typeVerf) {
    if (op == OP_div || op == OP_mul || op == OP_rem || op == OP_max ||
        op == OP_min) {
      signVerf = BinaryStrictSignVerify1(bOpnd[0], bOpnd[1], this);
      if (!signVerf) {
        LogInfo::MapleLogger()
            << "\n#Error:the result and operands of [div,mul,rem,max,min] must be of the same sign\n";
      }
    }
  }
  return opndsVerf && typeVerf && signVerf;
}

bool CompareNode::Verify() const {
  bool opndsVerf = BinaryGenericVerify(bOpnd[0], bOpnd[1]);
  bool compVerf = CompatibleTypeVerify(bOpnd[0], bOpnd[1]);
  bool resTypeVerf = CompareTypeVerify(primType);
  if (!resTypeVerf) {
    this->Dump(theModule);
  }
  bool signVerf = true;
  bool typeVerf = compVerf && resTypeVerf;
  if (typeVerf && op != OP_eq && op != OP_ne) {
    signVerf = BinaryStrictSignVerify0(bOpnd[0], bOpnd[1]);
    if (!signVerf) {
      LogInfo::MapleLogger() << "\n#Error:the operands of [ge,gt,le,lt] must be of the same sign\n";
    }
  }
  return opndsVerf && typeVerf && signVerf;
}

bool DepositbitsNode::Verify() const {
  bool opndsVerf = BinaryGenericVerify(bOpnd[0], bOpnd[1]);
  bool resTypeVerf = IntTypeVerify(primType);
  constexpr int kNumBitsInByte = 8;
  bool opnd0SizeVerf = (kNumBitsInByte * GetPrimTypeSize(bOpnd[0]->primType) >= bitsSize);
  if (!opnd0SizeVerf) {
    LogInfo::MapleLogger() << "\n#Error:opnd0 of depositbits must be large enough to contain the specified bitfield\n";
  }
  return opndsVerf && resTypeVerf && opnd0SizeVerf;
}

bool IntrinsicopNode::Verify() const {
  return VerifyOpnds();
}

bool TernaryNode::Verify() const {
  bool comp1Verf = CompatibleTypeVerify(topnd[1], this);
  bool comp2Verf = CompatibleTypeVerify(topnd[2], this);
  bool opnd0TypeVerf = IsPrimitiveInteger(topnd[0]->primType);
  if (!opnd0TypeVerf) {
    LogInfo::MapleLogger() << "\n#Error:select-opnd0 must be of integer type\n";
  }
  return comp1Verf && comp2Verf && opnd0TypeVerf;
}

bool SizeoftypeNode::Verify() const {
  return true;
}

bool ArrayNode::Verify() const {
  bool opndsVerf = VerifyOpnds();
  bool resTypeVerf = IsAddress(primType);
  bool opndsTypeVerf = true;
  if (!resTypeVerf) {
    LogInfo::MapleLogger() << "\n#Error:result-type of array must be in [ptr,ref,a32,a64]\n";
  }
  bool opnd0TypeVerf = IsAddress(nOpnd[0]->primType);
  if (!opnd0TypeVerf) {
    LogInfo::MapleLogger() << "\n#Error:result-type of array-opnd0 must be in [ptr,ref,a32,a64]\n";
  }
  for (size_t i = 1; i < numOpnds; i++) {
    if (!IntTypeVerify(nOpnd[i]->primType)) {
      opndsTypeVerf = false;
      LogInfo::MapleLogger() << "\n#Error:result of the array index operands must be in [i32,u32,i64,u64]\n";
    }
  }
  return opndsVerf && resTypeVerf && opnd0TypeVerf && opndsTypeVerf;
}

bool DassignNode::Verify() const {
  bool structVerf = IsStructureVerify(fieldID, stIdx);
  bool rhsVerf = uOpnd->Verify();
  return structVerf && rhsVerf;
}

bool AddrofNode::Verify() const {
  bool pTypeVerf = true;
  bool structVerf = IsStructureVerify(fieldID, stIdx);
  if (op == OP_dread) {
    pTypeVerf = ReadTypeVerify(this);
    if (fieldID == 0 && IsStructureTypeKind(GetTypeKind(stIdx))) {
      if (primType != PTY_agg) {
        pTypeVerf = false;
        LogInfo::MapleLogger() << "\n#Error:if variable is a structure, prim-type should specify agg\n";
      }
    }
    if (fieldID != 0 && structVerf) {
      MIRSymbol *var = theModule->CurFunction()->GetLocalOrGlobalSymbol(stIdx);
      MIRType *type = var->GetType();
      MIRStructType *stty = dynamic_cast<MIRStructType*>(type);
      ASSERT(stty != nullptr, "null pointer check");
      if (IsStructureTypeKind(GetFieldTypeKind(stty, fieldID))) {
        if (primType != PTY_agg) {
          pTypeVerf = false;
          LogInfo::MapleLogger() << "\n#Error:if the field itself is a structure, prim-type should specify agg\n";
        }
      }
    }
  } else {
    pTypeVerf = IsAddress(primType);
    if (!pTypeVerf) {
      LogInfo::MapleLogger()
          << "\n#Error:result-type of addrof,addroflabel,addroffunc,iaddrof must be in [ptr,ref,a32,a64]\n";
    }
  }
  return pTypeVerf && structVerf;
}

bool AddroffuncNode::Verify() const {
  bool addrTypeVerf = IsAddress(primType);
  if (!addrTypeVerf) {
    LogInfo::MapleLogger()
        << "\n#Error:result-type of addrof,addroflabel,addroffunc,iaddrof must be in [ptr,ref,a32,a64]\n";
  }
  return addrTypeVerf;
}

bool AddroflabelNode::Verify() const {
  bool addrTypeVerf = IsAddress(primType);
  if (!addrTypeVerf) {
    LogInfo::MapleLogger()
        << "\n#Error:result-type of addrof,addroflabel,addroffunc,iaddrof must be in [ptr,ref,a32,a64]\n";
  }
  return addrTypeVerf;
}

bool IassignNode::Verify() const {
  bool addrExpVerf = addrExpr->Verify();
  bool rhsVerf = rhs->Verify();
  bool structVerf = true;
  if (GetTypeKind(tyIdx) != kTypePointer) {
    LogInfo::MapleLogger() << "\n#Error:<type> must be a pointer type\n";
    return false;
  }
  if (fieldID != 0) {
    if (!IsStructureTypeKind(GetPointedTypeKind(tyIdx))) {
      structVerf = false;
      LogInfo::MapleLogger() << "\n#Error:If field-id is not 0, the computed address must correspond to a structure\n";
    }
  }
  return addrExpVerf && rhsVerf && structVerf;
}

bool IassignoffNode::Verify() const {
  bool addrVerf = bOpnd[0]->Verify();
  bool rhsVerf = bOpnd[1]->Verify();
  bool compVerf = CompatibleTypeVerify(this, bOpnd[1]);
  return addrVerf && rhsVerf && compVerf;
}

bool IassignFPoffNode::Verify() const {
  bool rhsVerf = uOpnd->Verify();
  bool compVerf = CompatibleTypeVerify(this, uOpnd);
  return rhsVerf && compVerf;
}

bool RegassignNode::Verify() const {
  bool rhsVerf = uOpnd->Verify();
  bool compVerf = CompatibleTypeVerify(this, uOpnd);
  return rhsVerf && compVerf;
}

bool CondGotoNode::Verify() const {
  bool opndExprVerf = UnaryStmtNode::uOpnd->Verify();
  bool opndTypeVerf = true;
  if (!IsPrimitiveInteger(UnaryStmtNode::uOpnd->primType)) {
    opndTypeVerf = false;
    LogInfo::MapleLogger() << "\n#Error:the operand of brfalse and trfalse must be primitive integer\n";
  }
  return opndExprVerf && opndTypeVerf;
}

bool SwitchNode::Verify() const {
  bool opndExprVerf = switchOpnd->Verify();
  bool opndTypeVerf = IntTypeVerify(switchOpnd->primType);
  if (!opndTypeVerf) {
    LogInfo::MapleLogger() << "\n#Error: the operand of switch must be in [i32,u32,i64,u64]\n";
  }
  return opndExprVerf && opndTypeVerf;
}

bool BinaryStmtNode::Verify() const {
  return bOpnd[0]->Verify() && bOpnd[1]->Verify() && CompatibleTypeVerify(bOpnd[0], bOpnd[1]) &&
         BinaryStrictSignVerify0(bOpnd[0], bOpnd[1]);
}

bool RangegotoNode::Verify() const {
  bool opndExprVerf = uOpnd->Verify();
  bool opndTypeVerf = IntTypeVerify(uOpnd->primType);
  if (!opndTypeVerf) {
    LogInfo::MapleLogger() << "\n#Error: the operand of rangegoto must be in [i32,u32,i64,u64]\n";
  }
  return opndExprVerf && opndTypeVerf;
}

bool BlockNode::Verify() const {
  for (auto &stmt : GetStmtNodes()) {
    if (!stmt.Verify()) {
      return false;
    }
  }
  return true;
}

bool DoloopNode::Verify() const {
  bool startVerf = startExpr->Verify();
  bool contVerf = condExpr->Verify();
  bool incrVerf = incrExpr->Verify();
  bool doBodyVerf = true;
  if (doBody) {
    doBodyVerf = doBody->Verify();
  }
  return startVerf && contVerf && incrVerf && doBodyVerf;
}

bool IfStmtNode::Verify() const {
  bool condVerf = uOpnd->Verify();
  bool thenVerf = true;
  bool elseVerf = true;
  if (thenPart) {
    thenVerf = thenPart->Verify();
  }
  if (elsePart) {
    elseVerf = elsePart->Verify();
  }
  return condVerf && thenVerf && elseVerf;
}

bool WhileStmtNode::Verify() const {
  bool condVerf = uOpnd->Verify();
  bool bodyVerf = true;
  if (body) {
    bodyVerf = body->Verify();
  }
  return condVerf && bodyVerf;
}

bool NaryStmtNode::Verify() const {
  return VerifyOpnds();
}

bool CallNode::Verify() const {
  return VerifyOpnds();
}

bool IcallNode::Verify() const {
  bool nOpndsVerf = true;
  for (size_t i = 0; i < numOpnds; i++) {
    if (!nOpnd[i]->Verify()) {
      nOpndsVerf = false;
      break;
    }
  }
  return nOpndsVerf;
}

bool IntrinsiccallNode::Verify() const {
  return VerifyOpnds();
}

}  // namespace maple
