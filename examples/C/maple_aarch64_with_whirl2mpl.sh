# Copyright (C) [2020] Futurewei Technologies, Inc. All rights reverved.
#
# Licensed under the Mulan Permissive Software License v2
# You can use this software according to the terms and conditions of the MulanPSL - 2.0.
# You may obtain a copy of MulanPSL - 2.0 at:
#
#   https://opensource.org/licenses/MulanPSL-2.0
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
# FIT FOR A PARTICULAR PURPOSE.
# See the MulanPSL - 2.0 for more details.
#

[ -n "$MAPLE_ROOT" ] || { echo MAPLE_ROOT not set. Please source envsetup.sh.; exit 1; }

CURRDIR=`pwd`
WORKDIR=$CURRDIR/aarch64_use_whirl2mpl

mkdir $WORKDIR
echo cd $WORKDIR
cd $WORKDIR

name=printHuawei

echo ========================================================================
echo ============= Use clangfe/whirl2mpl as C Frontend =======================
echo ========================================================================

cp $CURRDIR/$name.c .

FLAGS="-cc1 -emit-llvm -triple aarch64-linux-gnu -D__clang__ -D__BLOCKS__ -isystem /usr/aarch64-linux-gnu/include -isystem /usr/lib/gcc-cross/aarch64-linux-gnu/5/include"
echo $MAPLE_ROOT/tools/open64_prebuilt/x86/aarch64/bin/clangfe $FLAGS $name.c
$MAPLE_ROOT/tools/open64_prebuilt/x86/aarch64/bin/clangfe $FLAGS $name.c > doit.log 2>&1

echo $MAPLE_ROOT/tools/open64_prebuilt/x86/aarch64/bin/whirl2mpl $name.B
$MAPLE_ROOT/tools/open64_prebuilt/x86/aarch64/bin/whirl2mpl $name.B >> doit.log 2>&1

echo $MAPLE_ROOT/bin/aarch64-clang-release/maple -exe=me,mplcg -option=\"-O2 --quiet:-O2 -quiet\" $name.bpl
$MAPLE_ROOT/bin/aarch64-clang-release/maple -exe=me,mplcg -option="-O2 --quiet:-O2 -quiet" $name.bpl >> doit.log 2>&1

echo /usr/bin/aarch64-linux-gnu-gcc -o $name.out $name.s
/usr/bin/aarch64-linux-gnu-gcc -o $name.out $name.s

echo qemu-aarch64 -L /usr/aarch64-linux-gnu/ $name.out
qemu-aarch64 -L /usr/aarch64-linux-gnu/ $name.out


