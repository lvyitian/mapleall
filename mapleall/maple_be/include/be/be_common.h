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

#ifndef MAPLEBE_INCLUDE_BE_BECOMMON_H
#define MAPLEBE_INCLUDE_BE_BECOMMON_H

/// Basic Maple-independent utility functions
#include "common_utils.h"

/// MapleIR headers.
#include "mir_nodes.h"   // mapleir/include, for BaseNode
#include "mir_type.h"    // mapleir/include, for MIRType
#include "mir_module.h"  // mapleir/include, for MIRModule

/// C++ headers.
#include <cstddef>
#include <utility>
#include <cstdio>

namespace maplebe {

#if TARGX86_64 || TARGAARCH64 || TARGARK || TARGRISCV64 || TARGVM
#define LOWERED_PTR_TYPE PTY_a64
#define SIZEOFPTR 8
#elif TARGX86 || TARGARM
#define LOWERED_PTR_TYPE PTY_a32
#define SIZEOFPTR 4
#else
#error "Unsupported target"
#endif
#define BITS_PER_BYTE 8
#define LOG2_BITS_PER_BYTE 3

struct JClassFieldInfo {  // common java class field info
  bool is_ref;            // used to generate object-map
  bool is_unowned;        // used to mark unowned fields for RC
  bool is_weak;           // used to mark weak fields for RC
  uint32 offset;          // offset from the start of the java object

  // constructors
  JClassFieldInfo() : is_ref(false), is_unowned(false), is_weak(false), offset(0) {}

  JClassFieldInfo(bool isRef, bool isUnowned, bool isWeak, uint32 offset)
    : is_ref(isRef), is_unowned(isUnowned), is_weak(isWeak), offset(offset) {}
};

typedef MapleVector<JClassFieldInfo> JClassLayout;  // java class layout info

class BECommon {
 public:
  MIRModule &mirModule;
  MapleVector<uint64> type_size_table;           // index is TyIdx
  MapleVector<uint8> type_align_table;           // index is TyIdx
  MapleVector<uint8> type_natural_align_table;   // index is TyIdx
  MapleVector<bool> type_has_flexible_array;           // index is TyIdx
  MapleVector<FieldID> struct_fieldcount_table;  // gives number of fields inside
                                                 // each struct inclusive of nested structs, for speeding up
                                                 // traversal for locating the field for a given fieldID
  // a lookup table for class layout. the vector is indexed by field-id
  // Note: currently only for java class types.
  MapleUnorderedMap<MIRClassType *, JClassLayout *> jclass_layout_table;
  MapleUnorderedMap<MIRFunction *, TyIdx> funcReturnType;

  int optim_level;

 private:
  bool TyIsInSizeAlignTable(const MIRType *);
  void AddAndComputeSizeAlign(MIRType *);

 public:
  explicit BECommon(MIRModule &mod);

  ~BECommon() {}

  void LowerTypeAttribute(MIRType *ty);

  void LowerJavaTypeAttribute(MIRType *ty);

  void LowerJavaVolatileInClassType(MIRClassType *ty);

  void LowerJavaVolatileForSymbol(MIRSymbol *sym);

  void ComputeTypeSizesAligns(void);

  void ComputeTypeSizesAligns(MIRType *, uint8 align = 0);

  void GenFieldOffsetMap(const std::string &classname);

  void GenFieldOffsetMap(MIRClassType *classtype, FILE *outfile);

  void GenObjSize(MIRClassType *classtype, FILE *outfile);

  std::pair<int32, int32> GetFieldOffset(MIRStructType *structty, FieldID fieldID);

  bool IsRefField(MIRStructType *structty, FieldID fieldID);

  // some class may has incomplete type definition. provide an interface to check them.
  bool HasJClassLayout(MIRClassType *klass) {
    return (jclass_layout_table.find(klass) != jclass_layout_table.end());
  }

  JClassLayout &GetJClassLayout(MIRClassType *klass) {
    return *(jclass_layout_table[klass]);
  }

  MIRType *BeGetOrCreateArrayType(const MIRType *, uint32, const uint32 *);

  MIRType *BeGetOrCreatePointerType(MIRType *pointedTy);

  MIRType *BeGetOrCreateFunctionType(TyIdx tyIdx, std::vector<TyIdx> &vecTy, std::vector<TypeAttrs> &vecAt);

  BaseNode *GetAddressOfNode(BaseNode *node);

  PrimType GetAddressPrimType() const {
    return LOWERED_PTR_TYPE;
  }

  void UpdateTypeTable(MIRType *ty)  // update type_size_table and type_align_table when new type is created
  {
    if (!TyIsInSizeAlignTable(ty)) {
      AddAndComputeSizeAlign(ty);
    }
  }

  // Global type table might be updated.
  void FinalizeTypeTable();

  int GetFieldIdxIncrement(MIRType *ty) const {
    if (ty->GetKind() == kTypeClass)
    // number of fields + 2
    {
      return static_cast<MIRClassType *>(ty)->fields.size() + 2;
    } else if (ty->GetKind() == kTypeStruct)
    // number of fields + 1
    {
      return static_cast<MIRStructType *>(ty)->fields.size() + 1;
    } else {
      return 1;
    }
  }

};  // class BECommon

}  // namespace maplebe

#endif  //  MAPLEBE_INCLUDE_BE_BECOMMON_H
