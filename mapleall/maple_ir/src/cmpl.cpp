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

// license/copyrights comes here
//
// The compact MIR implementation - version 2.0.
//
#include "cmpl.h"
#include "bin_mir_file.h"
#if COMPACT_VM
#include "vmutils.h"
#include "vmmemory.h"
#endif

#include <cstdint>
#include <climits>
#include <cstdlib>

#if HAVE_MMAP
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#else
#include <cstdio>
#endif  // HAVE_MMAP

#define BINMIR_FILE_TYPE kMjsvmFileTypeCmpl  // moved from cmpl.h
// sanity check for different version of kCmpl
#if !defined(BINMIR_FILE_TYPE) || (BINMIR_FILE_TYPE != MJSVM_FILE_TYPE_CMPL)
#error "include the right kCmpl header file: this implements kCmpl-v2"
#endif

/**
   code for compact MIR (CMPL) manipulation could be used by compact-vm
   or by server-side CMPL tools, so we need to provide an interface for
   memory allocation.
   For compact VM, memory is allocated by memory_manager, in Internal memory
   area (needn't free)
   For standalone CMPL tools, it's by malloc. User should handle memory free.
 */
#if COMPACT_VM
template <typename T>
inline T *CmplAlloc() {
  return (T *)memory_manager->MallocInternal(sizeof(T));
}

template <typename T>
inline T *CmplAllocArray(uint16 num) {
  return (T *)memory_manager->MallocInternal(sizeof(T) * num);
}

template <typename T>
inline void CmplFree(T *ptr) {
  // do nothing: internal memory are designed to be free together when exit.
}

#else  // for server-side CMPL tools

template <typename T>
inline T *CmplAlloc() {
  return (T *)malloc(sizeof(T));
}

template <typename T>
inline T *CmplAllocArray(uint16 num) {
  MIR_ASSERT(num > 0);
  return (T *)malloc(sizeof(T) * num);
}

template <typename T>
inline void CmplFree(T *ptr) {
  MIR_ASSERT(ptr);
  free(static_cast<void *>(ptr));
}

#endif  // COMPACT_VM

// set up the necessary binMIR data structure given a binmir image
mir_module_t *SetupBinMir(void *binmirImage, uint32 imageSize) {
  MIR_ASSERT(binmirImage);
  // static_assert doesn't work for C++0x, so we do a dynamic assert here
  MIR_ASSERT(sizeof(MIRIntrinsicID) == 4);

  mir_module_t *module = CmplAlloc<mir_module_t>();
  MIR_ASSERT(module);

  module->flavor = kCmpl;

  // now building up in memory module information
  uint8 *imagePtr = static_cast<uint8 *>(binmirImage);
  uint32 currOffset = 0;
  binmir_file_header_t *header = reinterpret_cast<binmir_file_header_t *>(imagePtr);

  if (header->type != BINMIR_FILE_TYPE) {
    MIR_FATAL("unsupported binary mir type: %d! expected: %d\n", header->type, BINMIR_FILE_TYPE);
    return static_cast<mir_module_t *>(nullptr);
  }

  // 8B header
  imagePtr += sizeof(binmir_file_header_t);
  currOffset += sizeof(binmir_file_header_t);

  // 4B numFuncs
  module->numFuncs = *(reinterpret_cast<uint32 *>(imagePtr));
  imagePtr += 4;
  currOffset += 4;
  // 4B mainFuncID
  module->mainFuncID = *(reinterpret_cast<uint32 *>(imagePtr));
  if (module->mainFuncID > module->numFuncs) {
    MIR_FATAL("IR error: main function id (%d) > num of functions in module (%d)", module->mainFuncID,
              module->numFuncs);
    return static_cast<mir_module_t *>(nullptr);
  }

  imagePtr += 4;
  currOffset += 4;

  // 2B module id
  module->id = *(reinterpret_cast<int16 *>(imagePtr));
  imagePtr += 2;
  currOffset += 2;

  // 2B srclang
  module->srcLang = (maple::MIRSrcLang) * (reinterpret_cast<int16 *>(imagePtr));
  imagePtr += 2;
  currOffset += 2;

  module->funcs = CmplAllocArray<mir_func_t *>(module->numFuncs);
  if (!module->funcs) {
    MIR_FATAL("allocation error for function table, size: %d\n", module->numFuncs);
    return static_cast<mir_module_t *>(nullptr);
  }

  if ((currOffset + sizeof(uint32) * module->numFuncs) > imageSize) {
    MIR_FATAL("IR error: expected offset exceeds image size\n");
    return static_cast<mir_module_t *>(nullptr);
  }

  // each func_table item is 32-bit
  uint32 *funcTable = reinterpret_cast<uint32 *>(imagePtr);
  for (uint32 i = 0; i < module->numFuncs; i++) { // fix the pointer in memory
    module->funcs[i] = (mir_func_t *)(static_cast<uint8 *>(binmirImage) + funcTable[i]);
    // fix the pointer for formalWordsTypeTagged, formalWordsRefCounted,
    // localWordsTypeTagged and localWordsRefCounted
    mir_func_t *afunc = module->funcs[i];
    afunc->formalWordsTypeTagged = reinterpret_cast<uint8 *>(afunc) + (unsigned long)afunc->formalWordsTypeTagged;
    afunc->formalWordsRefCounted = reinterpret_cast<uint8 *>(afunc) + (unsigned long)afunc->formalWordsRefCounted;
    afunc->localWordsTypeTagged = reinterpret_cast<uint8 *>(afunc) + (unsigned long)afunc->localWordsTypeTagged;
    afunc->localWordsRefCounted = reinterpret_cast<uint8 *>(afunc) + (unsigned long)afunc->localWordsRefCounted;
  }

  imagePtr += sizeof(uint32) * module->numFuncs;

  // global-blk-map is at the end of the image.
  // to support current design, we use it here
  module->globalMemSize = *(reinterpret_cast<uint32 *>(imagePtr));
  imagePtr += sizeof(uint32);

  if (module->globalMemSize) {
    module->globalBlkMap = imagePtr;
    module->globalWordsTypeTagged = static_cast<uint8 *>(imagePtr) + module->globalMemSize;
    module->globalWordsRefCounted =
      static_cast<uint8 *>(module->globalWordsTypeTagged) + BlkSize2BitvectorSize(module->globalMemSize);
  } else {
    module->globalBlkMap = static_cast<uint8 *>(nullptr);
    module->globalWordsTypeTagged = static_cast<uint8 *>(nullptr);
    module->globalWordsRefCounted = static_cast<uint8 *>(nullptr);
  }

  return module;
}

// load compact MIR from binary file.
// Note:
//   1. returned pointer is malloced by memory manager. Need to free
//      by user. call
//        VMFree(module_ptr, sizeof(mir_module_t));
//      to free it.
#if HAVE_MMAP
mir_module_t *LoadBinMir(const char *file_name) {
  int binmir_fd = open(file_name, O_RDONLY);
  if (binmir_fd == -1) {
    MIR_FATAL("cannot open bin-mir file %s to read\n", file_name);
  }

  // mmap the file into memory
  // Do we have fstat?
  struct stat s;
  if (fstat(binmir_fd, &s) < 0) {
    MIR_FATAL("cannot check status of binmir file\n");
  }

  size_t image_size = s.st_size;
  void *binmir_image = mmap(nullptr, image_size, PROT_WRITE, MAP_PRIVATE, binmir_fd, 0);
  if (binmir_image == static_cast<void *>(-1)) {
    MIR_FATAL("cannot mmap binmir file to memory\n");
  }

#if MM_DEBUG
  printf("[Binmir Loader]: load app binmir: %s\n", file_name);
  printf("[Binmir Loader]: mmap %d Bytes app binmir into memory\n", image_size);
#endif  // MM_DEBUG

  mir_module_t *module = setup_bin_mir(binmir_image, image_size);
  if (module) {
    module->binMIRImageStart = binmir_image;
    module->binMIRImageLength = image_size;
    module->binMIRImageFD = binmir_fd;
  }

  return module;
}

#else  // no mmap, using buffered FILE I/O. current libc

mir_module_t *LoadBinMir(const char *fileName) {
  // current libc doesn't accept 'b' mode, so just ignore it
  // FILE *binmir_file = fopen(file_name, "rb");
  FILE *binmirFile = fopen(fileName, "r");
  if (!binmirFile) {
    MIR_FATAL("cannot open bin-mir file %s to read\n", fileName);
  }
  // read the whole image into memory
  //
  // get image size. This is a work-around for fstat,
  // because current libc don't have it. using fseek/ftell instead
  int ret1 = fseek(binmirFile, 0, SEEK_END);
  if (ret1 != 0) {
    MIR_FATAL("call fseek failed in LoadBinMir\n");
  }
  uint32 imageSize = static_cast<uint32>(ftell(binmirFile));
  MIR_ASSERT(imageSize != static_cast<uint32>(-1));
  int ret2 = fseek(binmirFile, 0, SEEK_SET);
  if (ret2 != 0) {
    MIR_FATAL("call fseek failed in LoadBinMir\n");
  }
  void *binmirImage = static_cast<void *>(CmplAllocArray<uint8>(imageSize));
  MIR_ASSERT(binmirImage);
  size_t readedSize = fread(binmirImage, imageSize, 1, binmirFile);
  if (readedSize != 1) {
    fclose(binmirFile);
    CmplFree(binmirImage);
    binmirImage = nullptr;
    return nullptr;
  }

#if MM_DEBUG
  printf("[Binmir Loader]: load app binmir: %s\n", fileName);
  printf("[Binmir Loader]: %d Bytes binmir loaded into memory\n", imageSize);
#endif  // MM_DEBUG

  mir_module_t *module = SetupBinMir(binmirImage, imageSize);
  module->binMIRImageStart = binmirImage;
  module->binMIRImageLength = imageSize;

  fclose(binmirFile);
  binmirImage = nullptr;
  return module;
}

#endif  // HAVE_MMAP

#if ARM
extern char app_array[];
mir_module_t *LoadBinMir_arm(const char *file_name) {
  mir_module_t *module = setup_bin_mir(static_cast<char *>(app_array), 1);
  module->binMIRImageStart = app_array;
  module->binMIRImageLength = 1;
  return module;
}

#endif

void CloseBinMir(mir_module_t *module) {
  MIR_ASSERT(module);

  // release allocated memory
  CmplFree(module->funcs);
#if HAVE_MMAP
  MIR_ASSERT(module && module->binMIRImageStart);
  if (munmap(module->binMIRImageStart, module->binMIRImageLength)) {
    MIR_FATAL("cannot unmap the binary mir file\n");
  }
  close(module->binMIRImageFD);
#else   // image readed into memory
  CmplFree(module->binMIRImageStart);
#endif  // HAVE_MMAP
  CmplFree(module);
}
