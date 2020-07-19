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

#ifndef MAPLEBE_INCLUDE_CG_ASM_INFO_H
#define MAPLEBE_INCLUDE_CG_ASM_INFO_H

#include "maple_string.h"

namespace maplebe {

typedef enum {
  kAsmGlbl,
  kAsmLocal,
  kAsmWeak,
  kAsmBss,
  kAsmComm,
  kAsmData,
  kAsmAlign,
  kAsmSyname,
  kAsmZero,
  kAsmByte,
  kAsmShort,
  kAsmValue,
  kAsmLong,
  kAsmQuad,
  kAsmSize,
  kAsmType,
  kAsmHidden
} Asmlabel;

class Asminfo {
 public:
  MapleString asm_cmnt;
  MapleString asm_atobt;
  MapleString asm_file;
  MapleString asm_section;
  MapleString asm_rodata;
  MapleString asm_global;
  MapleString asm_local;
  MapleString asm_weak;
  MapleString asm_bss;
  MapleString asm_comm;
  MapleString asm_data;
  MapleString asm_align;
  MapleString asm_zero;
  MapleString asm_byte;
  MapleString asm_short;
  MapleString asm_value;
  MapleString asm_long;
  MapleString asm_quad;
  MapleString asm_size;
  MapleString asm_type;
  MapleString asm_hidden;
  int init;

 public:
  explicit Asminfo(int in, MemPool *mp) {
    init = in;
    asm_cmnt.setMemPool(mp);
#if TARGX86 || TARGX86_64
    asm_cmnt = "\t#\t";
#elif TARGARM
    asm_cmnt = "\t@\t";
#else
    asm_cmnt = "\t//\t";
#endif

    asm_atobt.setMemPool(mp);
#if TARGX86 || TARGX86_64
    asm_atobt = "\t@object\t";
#else
    asm_atobt = "\t%object\t";
#endif

    asm_file.setMemPool(mp);
    asm_file = "\t.file\t";

    asm_section.setMemPool(mp);
    asm_section = "\t.section\t";

    asm_rodata.setMemPool(mp);
    asm_rodata = ".rodata\t";
    asm_global.setMemPool(mp);
    asm_global = "\t.global\t";
    asm_local.setMemPool(mp);
    asm_local = "\t.local\t";
    asm_weak.setMemPool(mp);
    asm_weak = "\t.weak\t";
    asm_bss.setMemPool(mp);
    asm_bss = "\t.bss\t";
    asm_comm.setMemPool(mp);
    asm_comm = "\t.comm\t";
    asm_data.setMemPool(mp);
    asm_data = "\t.data\t";
    asm_align.setMemPool(mp);
#if TARGARK
    asm_align = "\t.p2align\t";
#else
    asm_align = "\t.align\t";
#endif
    asm_zero.setMemPool(mp);
    asm_zero = "\t.zero\t";
    asm_byte.setMemPool(mp);
    asm_byte = "\t.byte\t";
    asm_short.setMemPool(mp);
    asm_short = "\t.short\t";
    asm_value.setMemPool(mp);
#ifdef TARGARM
    asm_value = "\t.short\t";
#else
    asm_value = "\t.value\t";
#endif
    asm_long.setMemPool(mp);
#ifdef TARGARM
    asm_long = "\t.word\t";
#else
    asm_long = "\t.long\t";
#endif
    asm_quad.setMemPool(mp);
    asm_quad = "\t.quad\t";
    asm_size.setMemPool(mp);
    asm_size = "\t.size\t";
    asm_type.setMemPool(mp);
    asm_type = "\t.type\t";
    asm_hidden.setMemPool(mp);
    asm_hidden = "\t.hidden\t";
  }
  virtual ~Asminfo() {}
};

}  // namespace maplebe

#endif
