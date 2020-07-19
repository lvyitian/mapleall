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

#include "secinput.h"

/*
 * <FUNCTION DESCRIPTION>
 *    The  vwscanf_s  function  is  the  wide-character  equivalent  of the vscanf_s function
 *    The vwscanf_s function is the wide-character version of vscanf_s. The
 *    function reads data from the standard input stream stdin and writes the
 *    data into the location that's given by argument. Each argument  must be a
 *    pointer to a variable of a type that corresponds to a type specifier in
 *    format. If copying occurs between strings that overlap, the behavior is
 *    undefined.
 *
 * <INPUT PARAMETERS>
 *    format                 Format control string.
 *    argList                pointer to list of arguments
 *
 * <OUTPUT PARAMETERS>
 *    argList                the converted value stored in user assigned address
 *
 * <RETURN VALUE>
 *    Returns the number of fields successfully converted and assigned;
 *    the return value does not include fields that were read but not assigned.
 *    A return value of 0 indicates that no fields were assigned.
 *    return -1 if an error occurs.
 */
int vwscanf_s(const wchar_t *format, va_list argList)
{
    int retVal;                 /* If initialization causes  e838 */
    SecFileStream fStr;

    SECUREC_INIT_SEC_FILE_STREAM(fStr, SECUREC_FROM_STDIN_FLAG, stdin, 0, NULL, 0);
    if (format == NULL || fStr.pf == NULL) {
        SECUREC_ERROR_INVALID_PARAMTER("vwscanf_s");
        return SECUREC_SCANF_EINVAL;
    }

    SECUREC_LOCK_STDIN(0, fStr.pf);

    retVal = SecInputSW(&fStr, format, argList);

    SECUREC_UNLOCK_STDIN(0, fStr.pf);

    if (retVal < 0) {
        SECUREC_ERROR_INVALID_PARAMTER("vwscanf_s");
        return SECUREC_SCANF_EINVAL;
    }

    return retVal;
}


