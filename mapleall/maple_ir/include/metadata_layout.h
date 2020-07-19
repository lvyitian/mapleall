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

#ifndef MAPLE_IR_INCLUDE_METADATA_LAYOUT_H
#define MAPLE_IR_INCLUDE_METADATA_LAYOUT_H


// metadata layout is shared between maple compiler and runtime, thus not in namespace maplert
#ifdef __cplusplus
extern "C" {
#endif

// some of the reference field of metadata is stored as relative offset
// for example, declaring class of Fields/Methods
// which can be negative
#ifdef USE_32BIT_REF
using metaref_t = uint32_t;      // consistent with reffield_t in address.h
using metaref_offset_t = int32_t;
#else
using metaref_t = uintptr_t;     // consistent iwth reffield_t in address.h
using metaref_offset_t = int64_t;
#endif // USE_32BIT_REF

inline void MRT_SetMetarefOffset(metaref_offset_t *pOffset, char *addr) {
  (*pOffset) = (metaref_offset_t)(addr - (char*)pOffset);
}

inline void *MRT_GetAddressFromMetarefOffset(metaref_offset_t *pOffset) {
  return (void*)((char*)pOffset + (*pOffset));
}

typedef struct {
  union {
    uint64_t realOffset; // for non-static
    void *absAddress; // for static
  } offset;
  metaref_offset_t declaringclass;
  uint16_t flag;
  uint16_t padding;
  int32_t fieldname;
  int32_t annotation;

#ifndef USE_32BIT_REF
  uint32_t padding1;
#endif //!USE_32BIT_REF

} FieldMetadataRO;

typedef metaref_offset_t FieldMetadataRORef;

typedef struct {
  // object common fields
  metaref_t shadow;  // pointer to classinfo of java/lang/reflect/Field
  int32_t monitor;

  // RW fields
  uint32_t mod;
  // put fieldinforo here for alignment requirements
  FieldMetadataRORef fieldinforo;

  union {
    // this works because klass pointer is always aligned to 8 Bytes(with bit 0-2 set to 0).
    struct {
      int32_t taboffset;  // the offset to relectstr_*.tab (mod 8 == 0).
                          // LSB (bit 0) is used to indicate whether it's name(1) or klass(0)
      int32_t offset;    // the offset inside reflectstr_*_tab
    } name;
    void *klass;   // should be ClassMetadata *
  } type; // the content relies on the flag bit

} FieldMetadata;

typedef struct {
  uint32_t mod;
  int32_t type_name;
  int32_t offset;
  uint16_t flag;
  uint16_t padding;
  int32_t fieldname;
  int32_t annotation;
} FieldMetadataCompact;

typedef struct {
  int32_t methodname;
  int32_t signaturename;
  void *addr;                // TODO: change this to 32bit
  char *annotationvalue;
  metaref_offset_t declaringclass;
  uint16_t flag;
  uint16_t argsize;

#ifndef USE_32BIT_REF
  int32_t padding;
#endif //!USE_32BIT_REF
  int64_t method_in_vtab_index;
} MethodMetadataRO;

typedef metaref_offset_t MethodMetadataRORef;

typedef struct {
  // object common fields
  metaref_t shadow;  // point to classinfo of java/lang/reflect/Method
  int32_t monitor;

  // RW fields
  uint32_t mod;

  MethodMetadataRORef methodinforo;
} MethodMetadata;

typedef struct {
  uint32_t mod;
  int32_t methodname;
  int32_t signaturename;
  int32_t addr;
  int32_t annotationvalue;
  int32_t declaringclass;
  uint16_t flag;
  uint16_t argsize;
  int32_t  method_in_vtab_index;
} MethodMetadataCompact;

// MethodDesc contains MethodMetadata and stack map
typedef struct {
  // relative offset for method metadata relative to current PC.
  // method metadata is in compact format if this offset is odd.
  uint32_t metadataOffset;

  uint16_t localRefOffset;
  uint16_t localRefNumber;

  // stack map for a methed might be placed here
} MethodDesc;

// Note: class init in maplebe and cg is highly dependent on this type.
// update aarch64_rt_support.h if you modify this definition.
typedef struct {
  char *classname;
  FieldMetadata *fields; // point to info of fields
  MethodMetadata *methods; // point to info of methods
  union {  // Element classinfo of array, others parent classinfo
    struct CLASSMETADATA **superclass;
    struct CLASSMETADATA *componentclass;
  } familyclass;

  uint16_t numoffields;
  uint16_t numofmethods;

#ifndef USE_32BIT_REF
  uint16_t flag;
  uint16_t numofsuperclasses;
  uint32_t padding;
#endif // !USE_32BIT_REF

  uint32_t mod; // 32 bit 0X00000000    modifier: 0x00FFFFFF   override: 0x80000000
  char *annotation;
} ClassMetadataRO;

typedef struct CLASSMETADATA {
  // object common fields
  metaref_t shadow;  // point to classinfo of java/lang/Class
  int32_t monitor;

  // other fields
  uint16_t clindex; // 8bit ClassLoader index, used for querying the address of related ClassLoader instance.
  union {
    uint16_t objsize;
    uint16_t componentsize;
  } sizeinfo;

#ifdef USE_32BIT_REF // for alignment purpose
  uint16_t flag;
  uint16_t numofsuperclasses;
#endif // USE_32BIT_REF

  void *itab;  // itab of current class, used for virtual call, will insert the content into classinfo
  void *vtab;  // vtab of current class, used for interface call, will insert the content into classinfo
  void *gctib; // for rc
  ClassMetadataRO *classinforo;
  union {
    intptr_t initstate;    // if class is not initialized
    intptr_t staticfields; // if class is already initialized
  }; // class init state, this field must be accessed atomically.
} ClassMetadata;

static inline intptr_t ClassMetadataOffsetOfInitFlag() {
  ClassMetadata* base = (ClassMetadata*)(0);
  return (intptr_t)(&(base->initstate));
}

static inline intptr_t ClassMetadataOffsetOfStaticFields() {
  ClassMetadata* base = (ClassMetadata*)(0);
  return (intptr_t)(&(base->staticfields));
}

#ifdef __cplusplus
}
#endif

// function to set Class/Field/Method metadata's shadow field to avoid type conversion
// Note 1: here we don't do NULL-check and type-compatibility check
// NOte 2: C should be of jclass/ClassMetata* type
template<typename M, typename C>
static inline void MRT_SetMetadataShadow(M *meta, C cls) {
  meta->shadow = (metaref_t)(uintptr_t)cls;
}

// help function to set Field/Method's declaringclass field to avoid type conversion
// Note: here we don't do NULL-check and type-compatibility check
// Note: Declaring class is encoded as relative offset which can be negative
// Note: here declaring class is stored as an offset rather than a raw pointer
template<typename M, typename C>
static inline void MRT_SetMetadataDeclaringClass(M *meta, C cls) {
  meta->declaringclass = (metaref_offset_t)(intptr_t)cls;
}

#endif // MAPLE_IR_INCLUDE_METADATA_LAYOUT_H
