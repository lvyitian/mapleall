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

/// Copyright [year] <Copyright Owner>

/* configuration definition for code in mapleir namespace */

#ifndef MAPLE_IR_INCLUDE_MIR_CONFIG_H
#define MAPLE_IR_INCLUDE_MIR_CONFIG_H

/**
 * MIR_FEATURE_FULL = 1 : for host/server size building, by default.
 * MIR_FEATURE_FULL = 0 : for resource-constrained devices. optimized for memory size
 */
#if !defined(MIR_FEATURE_FULL)
#define MIR_FEATURE_FULL 1  // default to full feature building, for debugging
#endif                      // MIR_FEATURE_FULL define

/**
 * MIR_DEBUG = 0 : for release building.
 * MIR_DEBUG = 1 : for debug building.
 */
#ifndef MIR_DEBUG
#define MIR_DEBUG 0  // currently default to none. turn it on explicitly
#endif               // MIR_DEBUG

/**
 * MIR_DEBUG_LEVEL = 0: no debuging information at all.
 *                   1: with error information.
 *                   2: with severe warning information
 *                   3: with normal warning information
 *                   4: with normal information
 *                   5: with everything
 */
#define MIR_DL_NONE 0
#define MIR_DL_FATAL 1
#define MIR_DL_ERROR 2
#define MIR_DL_WARNING 3
#define MIR_DL_INFO 4
#define MIR_DL_ALL 5

#ifndef MIR_DEBUG_LEVEL
#define MIR_DEBUG_LEVEL 0
#endif  // MIR_DEBUG_LEVEL

// assertion
#if !MIR_FEATURE_FULL

#define MIR_ASSERT(...) \
  do {                  \
  } while (0)
#define MIR_PRINTF(...) \
  do {                  \
  } while (0)
#define MIR_INFO(...) \
  do {                \
  } while (0)
#define MIR_ERROR(...) \
  do {                 \
  } while (0)
#define MIR_WARNING(...) \
  do {                   \
  } while (0)
#define MIR_CAST_TO(var, totype) ((totype)(var))
#define MIR_DYN_CAST(var, totype) ((totype)(var))

#include <stdlib.h>
#if DEBUG  // TODO: need to cleanup of debug flags, especially MIR_DEBUG
#include <stdio.h>
#define MIR_FATAL(...)                                   \
  do {                                                   \
    printf("FATAL ERROR: (%s:%d) ", __FILE__, __LINE__); \
    printf(__VA_ARGS__);                                 \
    exit(1);                                             \
  } while (0)
#else
#define MIR_FATAL(...) \
  do {                 \
    exit(1);           \
  } while (0)
#endif  // DEBUG
#else   // MIR_FEATURE_FULL

#include <cassert>
#include <cstdio>
#include <cstdlib>
#define MIR_ASSERT(...) assert(__VA_ARGS__)

#define MIR_FATAL(...)                                            \
  do {                                                            \
    fprintf(stderr, "FATAL ERROR: (%s:%d) ", __FILE__, __LINE__); \
    fprintf(stderr, __VA_ARGS__);                                 \
    exit(EXIT_FAILURE);                                           \
  } while (0)

#define MIR_ERROR(...)                                      \
  do {                                                      \
    fprintf(stderr, "ERROR: (%s:%d) ", __FILE__, __LINE__); \
    fprintf(stderr, __VA_ARGS__);                           \
  } while (0)

#define MIR_WARNING(...)                                      \
  do {                                                        \
    fprintf(stderr, "WARNING: (%s:%d) ", __FILE__, __LINE__); \
    fprintf(stderr, __VA_ARGS__);                             \
  } while (0)

#define MIR_PRINTF(...) printf(__VA_ARGS__)
#define MIR_INFO(...) printf(__VA_ARGS__)
// TODO: each sub-module should have its own LOG facility and control
//
#define MIR_CAST_TO(var, totype) static_cast<totype>(var)
#define MIR_DYN_CAST(var, totype) dynamic_cast<totype>(var)

#endif  // !MIR_FEATURE_FULL

#if MIR_DEBUG
// TODO: add debugging level control
// TODO: at least provide MIR_INFO, MIR_WARNING, MIR_ERROR
#else
#endif  // MIR_DEBUG

/**
 * MIR specific configurations.
 */
// Note: fix size definition cannot handle arbitary long MIR lines, such
// as those array initialization lines.
#define MIR_MAX_LINE_SIZE 3072  // a max of 3K characters per line initially

// LIBRARY API availability
#if MIR_FEATURE_FULL
#define HAVE_STRTOD 1   // strtod
#define HAVE_MALLOC 1   // malloc/free
#define HAVE_SPRINTF 1  // sprintf
#else                   // compact VM
#define HAVE_STRTOD 1   // strtod in current libc
#define HAVE_MALLOC 0   // no malloc/free in current libc
#define HAVE_SPRINTF 0  // no sprintf in current libc
#endif                  // MIR_FEATURE_FULL

#endif  // MAPLE_IR_INCLUDE_MIR_CONFIG_H
