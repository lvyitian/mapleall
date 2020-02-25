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

#include <cinttypes>
#include <list>

#include "aarch64_rt_support.h"
#include "cg_option.h"
#include "be_common.h"
#include "mir_builder.h"
#include "mpl_logging.h"

namespace maplebe {

using namespace maple;

BECommon::BECommon(MIRModule &mod)
  : mirModule(mod),
    type_size_table(GlobalTables::GetTypeTable().typeTable.size(), 0, mirModule.memPoolAllocator.Adapter()),
    type_align_table(GlobalTables::GetTypeTable().typeTable.size(), 0, mirModule.memPoolAllocator.Adapter()),
    struct_fieldcount_table(GlobalTables::GetTypeTable().typeTable.size(), 0, mirModule.memPoolAllocator.Adapter()),
    jclass_layout_table(mirModule.memPoolAllocator.Adapter()),
    optim_level(0) {
  for (uint32 i = 1; i < GlobalTables::GetTypeTable().typeTable.size(); ++i) {
    MIRType *ty = GlobalTables::GetTypeTable().typeTable[i];
    ComputeTypeSizesAligns(ty);
    LowerTypeAttribute(ty);
  }

  if (mirModule.IsJavaModule()) {
    for (uint32 i = 0; i < GlobalTables::GetGsymTable().GetSymbolTableSize(); ++i) {
      MIRSymbol *sym = GlobalTables::GetGsymTable().GetSymbol(i);
      if (!sym) {
        continue;
      }
      LowerJavaVolatileForSymbol(sym);
    }
  }
}

// try to find an available padding slot, and allocate the given field in it.
// return the offset of the allocated memory. 0 if not available
// Note: this will update lists in padding_slots
// Note: padding slots is a list of un-occupied (small size) slots
//       available to allocate new fields. so far, just for 1, 2, 4 bytes
//       types (map to array index 0, 1, 2)
static uint32 TryAllocInPaddingSlots(std::list<uint32> paddingSlots[],
                                     uint32 fieldsize,
                                     uint32 fieldalign,
                                     size_t paddingSlotsLength) {
  CHECK_FATAL(paddingSlotsLength > 0, "invalid index");
  if (fieldsize > 4) {
    return 0;  // padding slots are for size 1/2/4 bytes
  }

  uint32 fieldOffset = 0;
  // here is a greedy search
  for (uint32 freeSlot = (fieldsize >> 1); freeSlot < 3; freeSlot++) {
    if (paddingSlots[freeSlot].size() != 0) {
      uint32 paddingOffset = paddingSlots[freeSlot].front();
      if (IsAlignedTo(paddingOffset, fieldalign)) {
        // reuse one padding slot
        paddingSlots[freeSlot].pop_front();
        fieldOffset = paddingOffset;
        // check whether there're still space left in this slot
        uint32 leftSize = (1 << freeSlot) - fieldsize;
        if (leftSize) {
          uint32 leftOffset = paddingOffset + fieldsize;
          if (leftSize & 0x1) {
            paddingSlots[0].push_front(leftOffset);
            leftOffset += 1;
          }
          if (leftSize & 0x2) {
            paddingSlots[1].push_front(leftOffset);
          }
          // now there should be no more space left
        }
        break;
      }
    }
  }
  return fieldOffset;
}

static void AddPaddingSlot(std::list<uint32> paddingSlots[], uint32 offset, uint32 size, size_t paddingSlotsLength) {
  CHECK_FATAL(paddingSlotsLength > 0, "invalid index");
  // decompose the padding into 1/2/4 bytes slots.
  // to satisfy alignment constraints.
  for (uint32 i = 0; i < 3; i++) {
    if (size & (1 << i)) {
      paddingSlots[i].push_front(offset);
      offset += (1 << i);
    }
  }
}

// Note: also do java class layout
void BECommon::ComputeTypeSizesAligns(MIRType *ty, uint8 align) {
  TyIdx i = ty->tyIdx;
  if (struct_fieldcount_table.size() > i.GetIdx() && struct_fieldcount_table[i.GetIdx()] != 0) {
    return;  // processed before
  }

  if (ty->primType == PTY_ptr || ty->primType == PTY_ref) {
    ty->primType = LOWERED_PTR_TYPE;
  }

  switch (ty->typeKind) {
    case kTypeScalar:
    case kTypePointer:
    case kTypeBitField:
    case kTypeFunction:
      type_size_table[i.GetIdx()] = GetPrimTypeSize(ty->primType);
      type_align_table[i.GetIdx()] = type_size_table[i.GetIdx()];
      break;

    case kTypeArray: {
      MIRArrayType *arrayty = static_cast<MIRArrayType *>(ty);
      MIRType *elemty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(arrayty->eTyIdx);
      uint32 elemsize = type_size_table[elemty->tyIdx.GetIdx()];
      if (elemsize == 0) {
        ComputeTypeSizesAligns(elemty);
        elemsize = type_size_table[elemty->tyIdx.GetIdx()];
      }
      CHECK_FATAL(elemsize != 0, "");
      CHECK_FATAL(type_align_table[elemty->tyIdx.GetIdx()] != 0, "");
      elemsize = std::max(elemsize, static_cast<uint32>(type_align_table[elemty->tyIdx.GetIdx()]));
      // compute total number of elements from the multipel dimensions
      uint64 numelems = 1;
      for (int d = 0; d < arrayty->dim; d++) {
        numelems *= arrayty->sizeArray[d];
      }
      type_size_table[i.GetIdx()] = elemsize * numelems;
      type_align_table[i.GetIdx()] = type_align_table[elemty->tyIdx.GetIdx()];
      break;
    }

    case kTypeFArray:
    case kTypeJArray: {
      MIRFarrayType *arrayty = static_cast<MIRFarrayType *>(ty);
      MIRType *elemty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(arrayty->elemTyIdx);
      uint32 elemsize = type_size_table[elemty->tyIdx.GetIdx()];
      if (elemsize == 0) {
        ComputeTypeSizesAligns(elemty);
        elemsize = type_size_table[elemty->tyIdx.GetIdx()];
      }
      CHECK_FATAL(elemsize != 0, "");
      CHECK_FATAL(type_align_table[elemty->tyIdx.GetIdx()] != 0, "");
      elemsize = std::max(elemsize, static_cast<uint32>(type_align_table[elemty->tyIdx.GetIdx()]));
      type_size_table[i.GetIdx()] = 0;
      type_align_table[i.GetIdx()] = type_align_table[elemty->tyIdx.GetIdx()];
      break;
    }

    case kTypeUnion:
    case kTypeStruct: {
      MIRStructType *structty = static_cast<MIRStructType *>(ty);
      FieldVector fields = structty->fields;
      uint64 allocedSize = 0;
      uint64 allocedSizeInBits = 0;
      struct_fieldcount_table[structty->tyIdx.GetIdx()] = fields.size();
      for (uint32 j = 0; j < fields.size(); j++) {
        TyIdx fieldtyidx = fields[j].second.first;
        MIRType *fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);
        uint32 fieldsize = type_size_table[fieldtyidx.GetIdx()];
        if (fieldsize == 0) {
          ComputeTypeSizesAligns(fieldty);
          fieldsize = type_size_table[fieldtyidx.GetIdx()];
        }
        uint8 fieldalign = type_align_table[fieldtyidx.GetIdx()];
        CHECK_FATAL(fieldalign != 0, "");

        MIRStructType *substructty = fieldty->EmbeddedStructType();
        if (substructty != nullptr) {
          struct_fieldcount_table[structty->tyIdx.GetIdx()] += struct_fieldcount_table[substructty->tyIdx.GetIdx()];
        }

        if (structty->typeKind != kTypeUnion) {
          if (fieldty->typeKind == kTypeBitField) {
            uint32 fldsize = static_cast<MIRBitfieldType *>(fieldty)->fieldSize;
            // is this field is crossing the align boundary of its base type?
            if (allocedSizeInBits / (fieldalign * 8) != (allocedSizeInBits + fldsize - 1) / (fieldalign * 8)) {
              // the field is crossing the align boundary of its base type;
              // align alloced_size_in_bits to fieldalign
              allocedSizeInBits = RoundUp(allocedSizeInBits, fieldalign * 8);
            }
            // allocate the bitfield
            allocedSizeInBits += fldsize;
            allocedSize = std::max(allocedSize, RoundUp(allocedSizeInBits, fieldalign * 8) / 8);
          } else {
            // pad alloced_size according to the field alignment
            allocedSize = RoundUp(allocedSize, fieldalign);
            allocedSize += fieldsize;
            allocedSizeInBits = allocedSize * 8;
          }
        } else {  // for unions, bitfields are treated as non-bitfields
          allocedSize = std::max(allocedSize, static_cast<uint64>(fieldsize));
        }
        type_align_table[i.GetIdx()] = std::max(type_align_table[i.GetIdx()], fieldalign);
      }
      type_size_table[i.GetIdx()] = RoundUp(allocedSize, align);
      break;
    }

    case kTypeInterface: {  // interface shouldn't have instance fields
      type_align_table[i.GetIdx()] = 0;
      type_size_table[i.GetIdx()] = 0;
      struct_fieldcount_table[i.GetIdx()] = 0;
      break;
    }

    case kTypeClass: {  // cannot have union or bitfields
      uint64 allocedSize = 0;
      FieldVector &fields = static_cast<MIRStructType *>(ty)->fields;

      MIRClassType *classty = static_cast<MIRClassType *>(ty);
      TyIdx prntTyidx = classty->parentTyIdx;

      // process parent class
      if (prntTyidx != TyIdx(0)) {
        MIRClassType *prntTy = static_cast<MIRClassType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(prntTyidx));
        uint32 prntSize = type_size_table[prntTyidx.GetIdx()];
        if (prntSize == 0) {
          ComputeTypeSizesAligns(prntTy);
          prntSize = type_size_table[prntTyidx.GetIdx()];
        }
        uint8 prntAlign = type_align_table[prntTyidx.GetIdx()];
        struct_fieldcount_table[i.GetIdx()] += 1 + struct_fieldcount_table[prntTyidx.GetIdx()];
        // pad alloced_size according to the field alignment
        allocedSize = RoundUp(allocedSize, prntAlign);

        JClassLayout *layout = mirModule.memPool->New<JClassLayout>(mirModule.memPoolAllocator.Adapter());
        // add parent's record to the front
        layout->push_back(JClassFieldInfo(false, false, false, allocedSize));
        // copy parent's layout plan into my plan
        if (HasJClassLayout(prntTy)) {  // parent may have incomplete type definition.
          JClassLayout &parentlayout = GetJClassLayout(prntTy);
          layout->insert(layout->end(), parentlayout.begin(), parentlayout.end());
          allocedSize += prntSize;
          type_align_table[i.GetIdx()] = std::max(type_align_table[i.GetIdx()], prntAlign);
        } else {
          printf("Warning:try to layout class with incomplete type:%s\n", prntTy->GetName().c_str());
        }

        jclass_layout_table[classty] = layout;
      } else {
        // This is the root class, say, The Object
        jclass_layout_table[classty] = mirModule.memPool->New<JClassLayout>(mirModule.memPoolAllocator.Adapter());
      }

      // a list of un-occupied (small size) slots available for insertion
      // so far, just for 1, 2, 4 bytes types (map to array index 0, 1, 2)
      std::list<uint32> paddingSlots[3];
      JClassLayout &layout = GetJClassLayout(classty);

      // process fields
      struct_fieldcount_table[i.GetIdx()] += fields.size();
      for (uint32 j = 0; j < fields.size(); j++) {
        TyIdx fieldtyidx = fields[j].second.first;
        MIRType *fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);
        FieldAttrs fieldattr = fields[j].second.second;
        uint32 fieldsize = type_size_table[fieldtyidx.GetIdx()];
        if (fieldsize == 0) {
          ComputeTypeSizesAligns(fieldty);
          fieldsize = type_size_table[fieldtyidx.GetIdx()];
        }
        uint8 fieldalign = type_align_table[fieldtyidx.GetIdx()];

        if ((fieldty->typeKind == kTypePointer) && (fieldty->primType == PTY_a64)) {
          // handle class reference field
          fieldsize = AArch64RTSupport::kRefFieldSize;
          fieldalign = AArch64RTSupport::kRefFieldAlign;
        }

        // try to alloc the field in one of previously created padding slots
        uint32 currentFieldOffset = TryAllocInPaddingSlots(paddingSlots, fieldsize, fieldalign, sizeof(paddingSlots));

        // cannot reuse one padding slot. layout to current end
        if (!currentFieldOffset) {
          // pad alloced_size according to the field alignment
          currentFieldOffset = RoundUp(allocedSize, fieldalign);
          if (currentFieldOffset != allocedSize) {
            // rounded up, create one padding-slot
            uint32 paddingSize = currentFieldOffset - allocedSize;
            AddPaddingSlot(paddingSlots, allocedSize, paddingSize, sizeof(paddingSlots));
            allocedSize = currentFieldOffset;
          }
          // need new memory for this field
          allocedSize += fieldsize;
        }
        layout.push_back(JClassFieldInfo(fieldty->GetKind() == kTypePointer,
                                         fieldattr.GetAttr(FLDATTR_rcunowned) == true,
                                         fieldattr.GetAttr(FLDATTR_rcweak) == true, currentFieldOffset));
        type_align_table[i.GetIdx()] = std::max(type_align_table[i.GetIdx()], fieldalign);
      }
      type_size_table[i.GetIdx()] = RoundUp(allocedSize, align);
      break;
    }

    case kTypeByName:
    case kTypeVoid:
    default:
      type_size_table.at(i.GetIdx()) = 0;
      break;
  }
  // there may be passed-in align attribute declared with the symbol
  type_align_table[i.GetIdx()] = std::max(type_align_table[i.GetIdx()], align);
}

void BECommon::LowerTypeAttribute(MIRType *ty) {
  if (mirModule.IsJavaModule()) {
    LowerJavaTypeAttribute(ty);
  }
}

void BECommon::LowerJavaTypeAttribute(MIRType *ty) {
  // we process volatile only for now
  switch (ty->typeKind) {
    case kTypeClass:  // cannot have union or bitfields
      LowerJavaVolatileInClassType(static_cast<MIRClassType *>(ty));
      break;

    default:
      break;
  }
}

void BECommon::LowerJavaVolatileInClassType(MIRClassType *ty) {
  for (uint32 i = 0; i < ty->fields.size(); i++) {
    if (ty->fields[i].second.second.GetAttr(FLDATTR_volatile)) {
      ty->fields[i].second.second.SetAttr(FLDATTR_memory_order_acquire);
      ty->fields[i].second.second.SetAttr(FLDATTR_memory_order_release);
    } else {
      MIRType *fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ty->fields[i].second.first);
      if (fieldty->typeKind == kTypeClass) {
        LowerJavaVolatileInClassType(static_cast<MIRClassType *>(fieldty));
      }
    }
  }
}

bool BECommon::IsRefField(MIRStructType *structty, FieldID fieldID) {
  if (structty->typeKind == kTypeClass) {
    if (HasJClassLayout(static_cast<MIRClassType *>(structty))) {
      JClassLayout &layout = GetJClassLayout(static_cast<MIRClassType *>(structty));
      CHECK_FATAL(!layout.empty(), "layout is null in BECommon::IsRefField");
      return layout[fieldID - 1].is_ref;
    } else {
      // Otherwise we should report an error
      CHECK_FATAL(false, "Cannot found java class layout information");
    }
  }
  return false;
}

void BECommon::LowerJavaVolatileForSymbol(MIRSymbol *sym) {
  // type attr is associated with symbol
  if (sym->GetAttr(ATTR_volatile)) {
    sym->SetAttr(ATTR_memory_order_acquire);
    sym->SetAttr(ATTR_memory_order_release);
  }
}

void BECommon::GenFieldOffsetMap(const std::string &classname) {
  MIRType *type = GlobalTables::GetTypeTable().GetClassType(classname.c_str(), &mirModule);
  CHECK_FATAL(type, "unknown class");
  MIRClassType *classtype = static_cast<MIRClassType *>(type);
  for (int32 i = 1; i <= struct_fieldcount_table[classtype->tyIdx.GetIdx()]; i++) {
    int32_t fid = i;
    FieldPair fp = classtype->TraverseToFieldRef(fid);
    GStrIdx strIdx = fp.first;
    if (strIdx == GStrIdx(0)) {
      continue;
    }

    const char *fieldname = GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx).c_str();

    TyIdx fieldtyidx = fp.second.first;
    uint32 fieldsize = type_size_table[fieldtyidx.GetIdx()];
    MIRType *fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);

    if ((fieldty->typeKind == kTypePointer) && (fieldty->primType == PTY_a64)) {
      // handle class reference field
      fieldsize = AArch64RTSupport::kRefFieldSize;
    }

    std::pair<int32, int32> p = GetFieldOffset(classtype, i);
    CHECK_FATAL(p.second == 0, "");
    printf("CLASS_FIELD_OFFSET_MAP(%s, %s, %d, %d)\n", classname.c_str(), fieldname, p.first, fieldsize);
  }
}

void BECommon::GenFieldOffsetMap(MIRClassType *classtype, FILE *outfile) {
  const std::string &classname = classtype->GetName();

  // We only enumerate fields defined in the current class.  There are cases
  // where a parent classes may define private fields that have the same name as
  // a field in the current class.This table is generated for the convenience of
  // C programmers.  If the C programmer wants to access parent class fields,
  // the programmer should access them as `Parent.field`.
  int32_t myEnd = struct_fieldcount_table.at(classtype->tyIdx.GetIdx());
  int32_t myBegin = myEnd - classtype->fields.size() + 1;

  for (int32_t i = myBegin; i <= myEnd; i++) {
    int32_t fid = i;
    FieldPair fp = classtype->TraverseToFieldRef(fid);
    GStrIdx strIdx = fp.first;
    if (strIdx == GStrIdx(0)) {
      continue;
    }
    FieldAttrs attrs = fp.second.second;
    if (attrs.GetAttr(FLDATTR_static)) {
      continue;
    }

    const char *fieldname = GlobalTables::GetStrTable().GetStringFromStrIdx(strIdx).c_str();

    TyIdx fieldtyidx = fp.second.first;
    uint32 fieldsize = type_size_table[fieldtyidx.GetIdx()];
    MIRType *fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);

    if ((fieldty->typeKind == kTypePointer) && (fieldty->primType == PTY_a64)) {
      // handle class reference field
      fieldsize = AArch64RTSupport::kRefFieldSize;
    }

    std::pair<int32, int32> p = GetFieldOffset(classtype, i);
    CHECK_FATAL(p.second == 0, "");
    fprintf(outfile, "__MRT_CLASS_FIELD(%s, %s, %d, %d)\n", classname.c_str(), fieldname, p.first, fieldsize);
  }
}

void BECommon::GenObjSize(MIRClassType *classtype, FILE *outfile) {
  const std::string &classname = classtype->GetName();
  uint64_t objsize = type_size_table.at(classtype->GetTypeIndex().GetIdx());

  if (objsize == 0) {
    // fprintf(stderr, "Warning: objsize is zero!  class: %s\n", classname.c_str());
    return;
  }

#if 1  // this provides a quick way to find super class
  TyIdx parenttyidx = classtype->parentTyIdx;
  MIRType *parentty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(parenttyidx);
  const char *parentname = nullptr;
  if (parentty) {
    MIRClassType *parentclass = static_cast<MIRClassType *>(parentty);
    parentname = parentclass->GetName().c_str();
  } else {
    parentname = "THIS_IS_ROOT";
  }
  fprintf(outfile, "__MRT_CLASS(%s, %" PRId64 ", %s)\n", classname.c_str(), objsize, parentname);
#else
  fprintf(outfile, "__MRT_CLASS(%s, %" PRId64 ")\n", classname.c_str(), objsize);
#endif
}

// compute the offset of the field given by fieldID within the structure type
// structy; it returns the answer in the pair (byteoffset, bitoffset) such that
// if it is a bitfield, byteoffset gives the offset of the container for
// extracting the bitfield and bitoffset is with respect to the container
std::pair<int32, int32> BECommon::GetFieldOffset(MIRStructType *structty, FieldID fieldID) {
  CHECK_FATAL(fieldID <= struct_fieldcount_table[structty->tyIdx.GetIdx()], "GetFieldOFfset: fieldID too large");
  uint64 allocedSize = 0;
  uint64 allocedSizeInBits = 0;
  FieldID curFieldid = 1;
  if (fieldID == 0) {
    return std::pair<int32, int32>(0, 0);
  }

  if (structty->typeKind == kTypeClass) {
    if (HasJClassLayout(static_cast<MIRClassType *>(structty))) {
      JClassLayout &layout = GetJClassLayout(static_cast<MIRClassType *>(structty));
      return std::pair<int32, int32>(static_cast<int32>(layout[fieldID - 1].offset), 0);
    } else {
      // Otherwise we should report an error
      CHECK_FATAL(false, "Cannot found java class layout information");
    }
  }

  // process the struct fields

  FieldVector fields = structty->fields;
  for (uint32 j = 0; j < fields.size(); j++) {
    TyIdx fieldtyidx = fields[j].second.first;
    MIRType *fieldty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fieldtyidx);
    uint32 fieldsize = type_size_table[fieldtyidx.GetIdx()];
    uint8 fieldalign = type_align_table[fieldtyidx.GetIdx()];
    CHECK_FATAL(fieldalign != 0, "");
    if (structty->typeKind != kTypeUnion) {
      if (fieldty->typeKind == kTypeBitField) {
        uint32 fldsize = static_cast<MIRBitfieldType *>(fieldty)->fieldSize;
        // is this field is crossing the align boundary of its base type?
        if (allocedSizeInBits / (fieldalign * 8) != (allocedSizeInBits + fldsize - 1) / (fieldalign * 8)) {
          // the field is crossing the align boundary of its base type;
          // align alloced_size_in_bits to fieldalign
          allocedSizeInBits = RoundUp(allocedSizeInBits, fieldalign * 8);
        }
        // allocate the bitfield
        if (curFieldid == fieldID) {
          return std::pair<int32, int32>((allocedSizeInBits / (fieldalign * 8)) * fieldalign,
                                         allocedSizeInBits % (fieldalign * 8));
        } else if (fieldty->typeKind == kTypeStruct) {
          if (curFieldid + struct_fieldcount_table[fieldtyidx.GetIdx()] < fieldID) {
            curFieldid += struct_fieldcount_table[fieldtyidx.GetIdx()] + 1;
          } else {
            MIRStructType *substructty = static_cast<MIRStructType *>(fieldty);
            std::pair<int32, int32> result = GetFieldOffset(substructty, fieldID - curFieldid);
            return std::pair<int32, int32>(result.first + allocedSize, result.second);
          }
        } else {
          curFieldid++;
        }
        allocedSizeInBits += fldsize;
        allocedSize = std::max(allocedSize, RoundUp(allocedSizeInBits, fieldalign * 8) / 8);
      } else {
        allocedSize = RoundUp(allocedSize, fieldalign);

        if (curFieldid == fieldID) {
          return std::pair<int32, int32>(allocedSize, 0);
        } else {
          MIRStructType *substructty = fieldty->EmbeddedStructType();
          if (substructty == nullptr) {
            curFieldid++;
          } else {
            if (curFieldid + struct_fieldcount_table[substructty->tyIdx.GetIdx()] < fieldID) {
              curFieldid += struct_fieldcount_table[fieldtyidx.GetIdx()] + 1;
            } else {
              std::pair<int32, int32> result = GetFieldOffset(substructty, fieldID - curFieldid);
              return std::pair<int32, int32>(result.first + allocedSize, result.second);
            }
          }
        }

        allocedSize += fieldsize;
        allocedSizeInBits = allocedSize * 8;
      }
    } else {  // for unions, bitfields are treated as non-bitfields
      if (curFieldid == fieldID) {
        return std::pair<int32, int32>(0, 0);
      } else if (fieldty->typeKind == kTypeStruct) {  // union cannot be kTypeClass
        if (curFieldid + struct_fieldcount_table[fieldtyidx.GetIdx()] < fieldID) {
          curFieldid += struct_fieldcount_table[fieldtyidx.GetIdx()] + 1;
        } else {
          return GetFieldOffset(static_cast<MIRStructType *>(fieldty), fieldID - curFieldid);
        }
      } else {
        curFieldid++;
      }
    }
  }
  CHECK_FATAL(false, "GetFieldOffset() fails to find field");

  return std::pair<int32, int32>(0, 0);
}

bool BECommon::TyIsInSizeAlignTable(const MIRType *ty) {
  CHECK_FATAL(type_size_table.size() == type_align_table.size(), "");
  return ty->tyIdx.GetIdx() < type_size_table.size();
}

void BECommon::AddAndComputeSizeAlign(MIRType *ty) {
  CHECK_FATAL(ty->tyIdx == TyIdx(type_size_table.size()), "");  // make sure the ty idx is exactly the table size
  type_align_table.push_back(0);
  type_size_table.push_back(0);
  ComputeTypeSizesAligns(ty);
}

MIRType *BECommon::BeGetOrCreateArrayType(const MIRType *ty, uint32 dim, const uint32 *arraydim) {
  MIRType *newty = GlobalTables::GetTypeTable().GetOrCreateArrayType(ty, dim, arraydim);
  if (TyIsInSizeAlignTable(newty)) {
    return newty;
  }
  AddAndComputeSizeAlign(newty);
  return newty;
}

MIRType *BECommon::BeGetOrCreatePointerType(MIRType *pointedTy) {
  MIRType *newty = GlobalTables::GetTypeTable().GetOrCreatePointerType(pointedTy);
  if (TyIsInSizeAlignTable(newty)) {
    return newty;
  }
  AddAndComputeSizeAlign(newty);
  return newty;
}

MIRType *BECommon::BeGetOrCreateFunctionType(TyIdx tyIdx, std::vector<TyIdx> &vecTy,
                                             std::vector<TypeAttrs> &vecAt) {
  MIRType *newty = GlobalTables::GetTypeTable().GetOrCreateFunctionType(&mirModule, tyIdx, vecTy, vecAt);
  if (TyIsInSizeAlignTable(newty)) {
    return newty;
  }
  AddAndComputeSizeAlign(newty);
  return newty;
}

BaseNode *BECommon::GetAddressOfNode(BaseNode *node) {
  switch (node->op) {
    case OP_dread: {
      DreadNode *dnode = static_cast<DreadNode *>(node);
      return mirModule.mirBuilder->CreateAddrof(mirModule.CurFunction()->GetLocalOrGlobalSymbol(dnode->stIdx));
    }
    case OP_iread: {
      IreadNode *inode = static_cast<IreadNode *>(node);
      if (inode->fieldID == 0) {
        return inode->Opnd(0);
      }

      MIRType *pointedTy = GlobalTables::GetTypeTable().typeTable.at(
        static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().typeTable.at(inode->tyIdx.GetIdx()))->pointedTyIdx.GetIdx());
      std::pair<int32, int32> bytebitoffset = GetFieldOffset(static_cast<MIRStructType *>(pointedTy), inode->fieldID);
#if TARGAARCH64
      ASSERT(GetAddressPrimType() == PTY_a64, "incorrect address type");
#endif
      return mirModule.mirBuilder->CreateExprBinary(
          OP_add, GlobalTables::GetTypeTable().GetPrimType(GetAddressPrimType()), static_cast<BaseNode *>(inode->Opnd(0)),
          mirModule.mirBuilder->CreateIntConst(bytebitoffset.first, PTY_u32));
    }
    default:
      return nullptr;
  }
  return nullptr;
}

}  // namespace maplebe
