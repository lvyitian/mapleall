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

/* Avoid duplicate header files,not include securecutil.h */
#include "securecutil.h"

/* high Num << 8 | num of SPC Ver */
#define SECUREC_C_VERSION     (0x5 << 8)
#define SECUREC_SPC_VERSION   7
#define SECUREC_VERSION_STR   "Huawei Secure C V100R001C01SPC007B002"

/* SPC verNumber<->verStr like:
 * 0X201<->C01
 * 0X202<->SPC001   Redefine numbers after this version
 * 0X502<->SPC002
 * 0X503<->SPC003
 * ...
 * 0X50a<->SPC010
 * 0X50b<->SPC011
 * ...
 */
/* CP  verNumber<->verStr like:
 * 0X601<->CP0001
 * 0X602<->CP0002
 * ...
 */
const char *GetHwSecureCVersion(unsigned short *verNumber)
{
    if (verNumber != NULL) {
        *verNumber = (unsigned short)(SECUREC_C_VERSION | SECUREC_SPC_VERSION);
    }
    return SECUREC_VERSION_STR;
}
#if SECUREC_IN_KERNEL
EXPORT_SYMBOL(GetHwSecureCVersion);
#endif

