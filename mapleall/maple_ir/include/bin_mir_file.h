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

// The common facility for binary fomat MIR files.
// Currently, it implements the LiteOS binary file format.

#ifndef MAPLE_IR_INCLUDE_BIN_MIR_FILE_H
#define MAPLE_IR_INCLUDE_BIN_MIR_FILE_H

#include "types_def.h"

using namespace maple;

#define BIN_MIR_FILE_ID "HWCMPL"  // for magic in file header

#define VERSION_MAJOR 0  // experimental version
#define VERSION_MINOR 1

#define MAX_NUM_SEGS_PER_FILE (1 << 4)

enum BinMirFileType {
  kMjsvmFileTypeCmplV1 = 0,
  kMjsvmFileTypeCmpl,  // kCmpl v2 is the release version of
  kMjsvmFileTypeUnknown
};

#define MAKE_VERSION_NUM(major, minor) ((uint8)(((major & 0xF) << 4) | (minor & 0xF)))

// file header for binary format kMmpl, 8B in total
// Note the header is different with the specification
struct binmir_file_header_t {
  char magic[7];     // “HWCMPL”, or "HWLOS_"
  uint8 segNum : 4;  // number of segments (e.g. one raw IR file is a segment unit)
  uint8 type : 4;    // enum of type of VM file (e.g. MapleIR, TE)
  uint8 version;     // version of IR format (should be major.minor)
};

#endif  // MAPLE_IR_INCLUDE_BIN_MIR_FILE_H
