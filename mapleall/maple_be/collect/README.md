#
# Copyright (c) [2020] Huawei Technologies Co., Ltd. All rights reserved.
#
# Licensed under the Mulan Permissive Software License v2.
# You can use this software according to the terms and conditions of the MulanPSL - 2.0.
# You may obtain a copy of MulanPSL - 2.0 at:
#
#     https://opensource.org/licenses/MulanPSL-2.0
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
# FIT FOR A PARTICULAR PURPOSE.
# See the MulanPSL - 2.0 for more details.
#

# "collect" tools

This directory holds post-processing tools which work on the outputs of
`mplcg`.  They collect outputs from multiple `.mpl` files, and usually generate
a combined output, which can be processed further.  The name "collect" comes
from the `collect2` utility of the GCC toolchain.

Tools are usually written in scripting languages such as Bash or Python.  The
name usually starts with `mpl_collect_`.

Here is a summary of tools included in this directory.

-   `mpl_collect_macro_defs`: Collect `.macros.def` files, de-duplicate, and
    generate a combined file `unified.macros.def`.

-   `mpl_collect_groot_lists`: Collect `.groots.txt` files, de-duplicate, and
    generate an assembly file `unified.groots.s` which contains an array of
    pointers to such roots.


<!--
vim: tw=80 ts=4 sw=4 sts=4 et
-->
