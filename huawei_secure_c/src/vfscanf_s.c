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
 *    The vfscanf_s function is equivalent to fscanf_s, with the variable argument list replaced by argList
 *    The vfscanf_s function reads data from the current position of stream into
 *    the locations given by argument (if any). Each argument must be a pointer
 *    to a variable of a type that corresponds to a type specifier in format.
 *    format controls the interpretation of the input fields and has the same
 *    form and function as the format argument for scanf.
 *
 * <INPUT PARAMETERS>
 *    stream               Pointer to FILE structure.
 *    format               Format control string, see Format Specifications.
 *    argList              pointer to list of arguments
 *
 * <OUTPUT PARAMETERS>
 *    argList              the converted value stored in user assigned address
 *
 * <RETURN VALUE>
 *    Each of these functions returns the number of fields successfully converted
 *    and assigned; the return value does not include fields that were read but
 *    not assigned. A return value of 0 indicates that no fields were assigned.
 *    return -1 if an error occurs.
 */
int vfscanf_s(FILE *stream, const char *format, va_list argList)
{
    int retVal;                 /* If initialization causes  e838 */
    SecFileStream fStr;

    if ((stream == NULL) || (format == NULL)) {
        SECUREC_ERROR_INVALID_PARAMTER("vfscanf_s");
        return SECUREC_SCANF_EINVAL;
    }
    if (stream == stdin) {
        return vscanf_s(format, argList);
    }

    SECUREC_LOCK_FILE(stream);
    SECUREC_INIT_SEC_FILE_STREAM(fStr, SECUREC_FILE_STREAM_FLAG, stream, SECUREC_UNINITIALIZED_FILE_POS, NULL, 0);
    retVal = SecInputS(&fStr, format, argList);
    SECUREC_UNLOCK_FILE(stream);
    if (retVal < 0) {
        SECUREC_ERROR_INVALID_PARAMTER("vfscanf_s");
        return SECUREC_SCANF_EINVAL;
    }

    return retVal;
}


