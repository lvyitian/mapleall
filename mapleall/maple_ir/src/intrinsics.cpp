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

#include "mir_module.h"
#include "intrinsics.h"
#include "mir_type.h"
#include "mir_builder.h"

namespace maple {
MIRType *IntrinDesc::jsvalueType = nullptr;
MIRModule *IntrinDesc::mirModule = nullptr;

IntrinDesc IntrinDesc::intrintable[INTRN_LAST + 1] = {
#define DEF_MIR_INTRINSIC(X, NAME, INTRN_CLASS, RETURN_TYPE, ...) \
  { (#NAME), (INTRN_CLASS), { (RETURN_TYPE), ##__VA_ARGS__ } },
#include "intrinsics.def"
#undef DEF_MIR_INTRINSIC
};

MIRType *IntrinDesc::GetOrCreateJSValueType() {
  if (jsvalueType) {
    return jsvalueType;
  }

  MIRBuilder *jsbuilder = mirModule->mirBuilder;
  /*
        union {
            int32_t        i32;
            uint32_t       u32;
            uint32_t       boo;
            JSString       *str; // NIY
            JSObject       *obj; // NIY
            void           *ptr;
            JSWhyMagic     why; // NIY
            size_t         word; // NIY
            uintptr_t      uintptr; // NIY
        } payload;
   */
  FieldVector payloadFields;
  GStrIdx i32 = jsbuilder->GetOrCreateStringIndex("i32");
  GStrIdx u32 = jsbuilder->GetOrCreateStringIndex("u32");
  GStrIdx boo = jsbuilder->GetOrCreateStringIndex("boo");
  GStrIdx ptr = jsbuilder->GetOrCreateStringIndex("ptr");
  payloadFields.push_back(FieldPair(i32, TyidxFieldAttrPair(GlobalTables::GetTypeTable().GetInt32()->tyIdx, FieldAttrs())));
  payloadFields.push_back(FieldPair(u32, TyidxFieldAttrPair(GlobalTables::GetTypeTable().GetUInt32()->tyIdx, FieldAttrs())));
  payloadFields.push_back(FieldPair(boo, TyidxFieldAttrPair(GlobalTables::GetTypeTable().GetUInt32()->tyIdx, FieldAttrs())));
  payloadFields.push_back(FieldPair(ptr, TyidxFieldAttrPair(GlobalTables::GetTypeTable().GetVoidPtr()->tyIdx, FieldAttrs())));
  FieldVector parentFields;
  MIRType *payloadType = GlobalTables::GetTypeTable().GetOrCreateUnionType("payload_type", payloadFields, parentFields, mirModule);
  /*
     struct {
        union payload;
        JSValueTag tag;
     } s;
   */
  FieldVector sFields;
  GStrIdx payload = jsbuilder->GetOrCreateStringIndex("payload");
  GStrIdx tag = jsbuilder->GetOrCreateStringIndex("tag");
  sFields.push_back(FieldPair(payload, TyidxFieldAttrPair(payloadType->tyIdx, FieldAttrs())));
  sFields.push_back(FieldPair(tag, TyidxFieldAttrPair(GlobalTables::GetTypeTable().GetUInt32()->tyIdx, FieldAttrs())));
  MIRType *sType = GlobalTables::GetTypeTable().GetOrCreateStructType("s_type", sFields, parentFields, mirModule);
  /*
     typedef union jsval_layout
        uint64_t asBits;
        struct s;
        double asDouble;
        void *asPtr;
   */
  FieldVector jsvalLayoutFields;
  GStrIdx asBits = jsbuilder->GetOrCreateStringIndex("asBits");
  GStrIdx s = jsbuilder->GetOrCreateStringIndex("s");
  GStrIdx asDouble = jsbuilder->GetOrCreateStringIndex("asDouble");
  GStrIdx asPtr = jsbuilder->GetOrCreateStringIndex("asPtr");
  jsvalLayoutFields.push_back(FieldPair(asBits, TyidxFieldAttrPair(GlobalTables::GetTypeTable().GetUInt64()->tyIdx, FieldAttrs())));
  jsvalLayoutFields.push_back(FieldPair(s, TyidxFieldAttrPair(sType->tyIdx, FieldAttrs())));
  jsvalLayoutFields.push_back(FieldPair(asDouble, TyidxFieldAttrPair(GlobalTables::GetTypeTable().GetDouble()->tyIdx, FieldAttrs())));
  jsvalLayoutFields.push_back(FieldPair(asPtr, TyidxFieldAttrPair(GlobalTables::GetTypeTable().GetVoidPtr()->tyIdx, FieldAttrs())));

  MIRType *jsvalLayoutType =
      GlobalTables::GetTypeTable().GetOrCreateUnionType("jsval_layout_type", jsvalLayoutFields, parentFields, mirModule);
  return jsvalLayoutType;
}

void IntrinDesc::InitMIRModule(MIRModule *mod) {
  mirModule = mod;
}

MIRType *IntrinDesc::GetTypeFromArgTy(IntrinArgType argtype) {
  switch (argtype) {
    case kArgTyVoid:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_void);
    case kArgTyI8:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_i8);
    case kArgTyI16:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_i16);
    case kArgTyI32:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_i32);
    case kArgTyI64:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_i64);
    case kArgTyU8:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_u8);
    case kArgTyU16:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_u16);
    case kArgTyU32:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_u32);
    case kArgTyU64:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_u64);
    case kArgTyU1:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_u1);
    case kArgTyPtr:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_ptr);
    case kArgTyRef:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_ref);
    case kArgTyA32:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_a32);
    case kArgTyA64:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_a64);
    case kArgTyF32:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_f32);
    case kArgTyF64:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_f64);
    case kArgTyF128:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_f128);
    case kArgTyC64:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_c64);
    case kArgTyC128:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_c128);
    case kArgTyAgg:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_agg);
#ifdef DYNAMICLANG
    case kArgTySimplestr:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_simplestr);
    case kArgTySimpleobj:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_simpleobj);
    case kArgTyDynany:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_dynany);
    case kArgTyDyni32:
      return GlobalTables::GetTypeTable().GetTypeFromTyIdx((TyIdx)PTY_dyni32);
#endif
    default:
      return nullptr;
  }
}

MIRType *IntrinDesc::GetArgType(uint32 index) {
  // 0 is the arg of return type
  return GetTypeFromArgTy(argtypes_[index + 1]);
}

MIRType *IntrinDesc::GetReturnType() {
  return GetTypeFromArgTy(argtypes_[0]);
}

}  // namespace maple
