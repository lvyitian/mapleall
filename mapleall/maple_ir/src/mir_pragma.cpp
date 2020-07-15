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
#include "name_mangler.h"
#include "mir_pragma.h"
#include <iomanip>

namespace maple {

static std::string GetKind(PragmaValueType kind) {
  switch (kind) {
    case kValueByte:
      return "i8";
    case kValueShort:
      return "i16";
    case kValueChar:
      return "u16";
    case kValueInt:
      return "i32";
    case kValueLong:
      return "i64";
    case kValueFloat:
      return "f32";
    case kValueDouble:
      return "f64";
    case kValueMethodType:
      return "retype";
    case kValueMethodHandle:
      return "ref";
    case kValueString:
      return "ptr";
    case kValueType:
      return "type";
    case kValueField:
      return "var";
    case kValueMethod:
      return "func";
    case kValueEnum:
      return "enum";
    case kValueArray:
      return "array";
    case kValueAnnotation:
      return "annotation";
    case kValueNull:
      return "const";
    case kValueBoolean:
      return "u1";
    default:
      return "unknown";
  }
}

// status: 0 : unexpected char, stop
//         1 : < start subvec
//         2 : normal type str end with ';'
//         3 : normal type str end with '<' -- need append ';'
//         4 : > end subvec
//         5 : ; ignore and continue
//
//  2: Lfoofoofoofoo;xx  3: Lfoofoofoofoo<
//     |             |      |            |
//     start         end    start        end
static void GetTypeStr(const std::string str, uint32 &start, uint32 &end, uint32 &status) {
  uint32 i = start;
  std::string result = str;
  status = 0;

  while (str[i] == '[') {
    i++;
  }

  start = i;
  end = i;
  switch (str[i]) {
    case 'Z':
    case 'B':
    case 'S':
    case 'C':
    case 'I':
    case 'J':
    case 'F':
    case 'D':
    case 'V':
      status = 2;
      end = i + 1;
      break;
    case 'L':
    case 'T':
      // Lfoo; or Lfoo<...>;
      while (i < str.length()) {
        if (str[i] == ';') {
          status = 2;
          end = i + 1;
          break;
        } else if (str[i] == '<') {
          status = 3;
          end = i;
          break;
        } else {
          i++;
        }
      }
      break;
    case '<':
      status = 1;
      end = i + 1;
      break;
    case '>':
      status = 4;
      end = i + 1;
      break;
    case ';':  // case '+':
      // continue cases
      status = 5;
      end = i + 1;
      break;
    default:
      // termination - reached unexpected char
      end = i;
      break;
  }
  return;
}

MIRPragmaElement *MIRPragma::GetPragmaElemFromSignature(const std::string signature) {
  if (signature.size() == 0) {
    return nullptr;
  }

  std::stack<MIRPragmaElement *> elemstack;

  MIRPragmaElement *elem = mod->memPool->New<MIRPragmaElement>(mod);
  elem->type_ = kValueArray;
  elemstack.push(elem);

  uint32 status = 0;
  uint32 start = 0;
  uint32 end = 0;

  while (1) {
    if (signature.size() <= start) {
      break;
    }

    GetTypeStr(signature, start, end, status);

    // status: 0:stop/1:start_subvec/2:normal/3:normal+';'/4:end_subvec/5:ignore_and_continue
    switch (status) {
      case 0:
        return elem;
      case 1: {
        MIRPragmaElement *etmp = mod->memPool->New<MIRPragmaElement>(mod);
        etmp->type_ = kValueArray;
        elemstack.top()->subelemvec_.push_back(etmp);
        elemstack.push(etmp);
        break;
      }
      case 2: {
        MIRPragmaElement *etmp = mod->memPool->New<MIRPragmaElement>(mod);
        etmp->type_ = kValueType;
        std::string typestr = signature.substr(start, end - start);
        etmp->val_.u = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(typestr).GetIdx();
        elemstack.top()->subelemvec_.push_back(etmp);
        break;
      }
      case 3: {
        MIRPragmaElement *etmp = mod->memPool->New<MIRPragmaElement>(mod);
        etmp->type_ = kValueType;
        std::string typestr = signature.substr(start, end - start) + ";";
        etmp->val_.u = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(typestr).GetIdx();
        elemstack.top()->subelemvec_.push_back(etmp);
        break;
      }
      case 4:
        if (elemstack.empty()) {  // Invalid annotation signature format
          return nullptr;
        }
        elemstack.pop();
        break;
      case 5:
        break;
      default:
        CHECK_FATAL(false, "unexpected status");
        break;
    }

    start = end;
  }

  return elem;
}

void MIRPragmaElement::Dump(int indent) {
  GStrIdx strIdx;
  std::string str = GetKind(type_);
  switch (type_) {
    case kValueByte:
      LogInfo::MapleLogger() << str.c_str() << " " << val_.i;
      break;
    case kValueShort:
      LogInfo::MapleLogger() << str.c_str() << " " << val_.i;
      break;
    case kValueChar:
      LogInfo::MapleLogger() << str.c_str() << " " << val_.u;
      break;
    case kValueInt:
      LogInfo::MapleLogger() << str.c_str() << " " << val_.i;
      break;
    case kValueLong:
      LogInfo::MapleLogger() << str.c_str() << " " << val_.j;
      break;
    case kValueFloat:
      LogInfo::MapleLogger() << std::setiosflags(std::ios::scientific) << str.c_str() << " " << std::setprecision(7) << val_.f
                << "f";
      break;
    case kValueDouble:
      LogInfo::MapleLogger() << std::setiosflags(std::ios::scientific) << str.c_str() << " " << std::setprecision(16) << val_.d;
      break;
    case kValueMethodType:
      LogInfo::MapleLogger() << str.c_str() << " $" << std::hex << "0x" << val_.u << std::dec;
      break;
    case kValueMethodHandle:
      LogInfo::MapleLogger() << str.c_str() << " " << std::hex << "0x" << val_.u << std::dec;
      break;
    case kValueString:
      strIdx.SetIdx(val_.u);
      LogInfo::MapleLogger() << str.c_str() << " \"" << GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx).c_str() << "\"";
      break;
    case kValueType:
      strIdx.SetIdx(val_.u);
      LogInfo::MapleLogger() << str.c_str() << " <$" << GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx).c_str() << ">";
      break;
    case kValueField:
      strIdx.SetIdx(val_.u);
      LogInfo::MapleLogger() << str.c_str() << " @" << GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx).c_str();
      break;
    case kValueMethod:
      strIdx.SetIdx(val_.u);
      LogInfo::MapleLogger() << str.c_str() << " &" << GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx).c_str();
      break;
    case kValueEnum:
      strIdx.SetIdx(val_.u);
      LogInfo::MapleLogger() << str.c_str() << " " << GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx).c_str();
      break;
    case kValueArray: {
      uint32 num = subelemvec_.size();
      LogInfo::MapleLogger() << "array [" << num;
      if (num) {
        if (num > 1) {
          LogInfo::MapleLogger() << "," << std::endl;
        } else {
          LogInfo::MapleLogger() << ", ";
        }
        uint32 i = 0;
        while (i < num) {
          if (num > 1) {
            PrintIndentation(indent + 2);
          }
          subelemvec_[i]->Dump(indent + 2);
          if (i != num - 1) {
            LogInfo::MapleLogger() << "," << std::endl;
          }
          i++;
        }
      }
      LogInfo::MapleLogger() << "]";
      break;
    }
    case kValueAnnotation: {
      uint32 num = subelemvec_.size();
      LogInfo::MapleLogger() << "annotation <$";
      LogInfo::MapleLogger() << GlobalTables::GetStrTable().GetStringFromStrIdx(typestridx_).c_str() << "> [" << num;
      if (num) {
        if (num > 1) {
          LogInfo::MapleLogger() << "," << std::endl;
        } else {
          LogInfo::MapleLogger() << ", ";
        }
        uint32 i = 0;
        while (i < num) {
          if (num > 1) {
            PrintIndentation(indent + 2);
          }
          LogInfo::MapleLogger() << "@" << GlobalTables::GetStrTable().GetStringFromStrIdx(subelemvec_[i]->namestridx_).c_str() << " ";
          subelemvec_[i]->Dump(indent + 2);
          if (i != num - 1) {
            LogInfo::MapleLogger() << "," << std::endl;
          }
          i++;
        }
      }
      LogInfo::MapleLogger() << "]";
      break;
    }
    case kValueNull:
      LogInfo::MapleLogger() << str.c_str() << " NULL";
      break;
    case kValueBoolean:
      LogInfo::MapleLogger() << str.c_str() << " " << val_.u;
      break;
  }
}

void MIRPragma::Dump(int indent) {
  LogInfo::MapleLogger() << std::endl;
  PrintIndentation(indent);
  LogInfo::MapleLogger() << "pragma " << static_cast<int>(visibility) << " ";
  switch (pragmaKind) {
    case kPragmaClass:
      LogInfo::MapleLogger() << "class $";
      break;
    case kPragmaFunc:
      LogInfo::MapleLogger() << "func &";
      break;
    case kPragmaField:
      LogInfo::MapleLogger() << "var @";
      break;
    case kPragmaVar:
      LogInfo::MapleLogger() << "var %";
      break;
    case kPragmaParam:
      LogInfo::MapleLogger() << "param " << paramNum << " &";
      break;
    case kPragmaFuncExecptioni:
      LogInfo::MapleLogger() << "func_ex &";
      break;
    case kPragmaFuncVar:
      LogInfo::MapleLogger() << "func_var &";
      break;
    default:
      CHECK_FATAL(false, "unexpected func kind");
      break;
  }
  LogInfo::MapleLogger() << GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx) << " ";
  GStrIdx strIdx = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx)->nameStrIdx;
  if (tyIdxEx != 0) {
    MIRType *typeEx = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdxEx);
    LogInfo::MapleLogger() << "\"" << GetPrimTypeName(typeEx->primType) << "\" ";
  }
  LogInfo::MapleLogger() << "<$" << GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx) << "> {";
  for (uint32 j = 0; j < elementVec.size(); j++) {
    LogInfo::MapleLogger() << std::endl;
    PrintIndentation(indent + 1);
    MIRPragmaElement *e = elementVec[j];
    strIdx = e->namestridx_;
    LogInfo::MapleLogger() << "@" << GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx) << " ";
    e->Dump(indent);
    if (j != elementVec.size() - 1) {
      LogInfo::MapleLogger() << ",";
    }
  }
  LogInfo::MapleLogger() << "}";
  return;
}

}  // namespace maple
