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

#ifndef MAPLEBE_INCLUDE_CG_ARK_ARKABI_H
#define MAPLEBE_INCLUDE_CG_ARK_ARKABI_H

#include "types_def.h"
#include "be_common.h"

namespace maplebe {

using namespace maple;

// for specifying how a parameter is passed
struct PLocInfo {
  int32 memoffset;
  int32 memsize;
};

/*
   We use the names used in ARM IHI 0055C_beta. $ 5.4.2.
   NSAA (= _last_memoffset): Next Stacked Argument Address
 */
// for processing an incoming or outgoing parameter list
class ParmLocator {
 private:
  BECommon &_be;
  int32 _parm_num;  // number of all types of parameters processed so far
  int32 NSAA;

 public:
  // IHI 0055C_beta $ 5.4.2 Stage A (NSAA is set to 0, meaning "relative to the current SP")
  ParmLocator(BECommon &b) : _be(b), _parm_num(0), NSAA(0) {}

  virtual ~ParmLocator() {}

  void LocateNextParm(MIRType *ty, PLocInfo &ploc);
};

}  // namespace maplebe

#endif  // MAPLEBE_INCLUDE_CG_ARK_ARKABI_H
