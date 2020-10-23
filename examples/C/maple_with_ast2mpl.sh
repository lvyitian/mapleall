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
WORKDIR=$CURRDIR/use_ast2mp

mkdir $WORKDIR
echo cd $WORKDIR
cd $WORKDIR

name=printHuawei

echo ========================================================================
echo ================== Use ast2mpl as C Frontend ===========================
echo ========================================================================
# .c -> .mpl
INC=$(clang -v |& grep "Selected GCC installation" | sed "s/.*: //")
echo INC=$INC

cp $CURRDIR/$name.c .

echo $MAPLE_ROOT/bin/ast2mpl $name.c -I $INC/include
$MAPLE_ROOT/bin/ast2mpl $name.c -I $INC/include

# .mpl -> .s
echo $MAPLE_ROOT/bin/arm64-clang-release/maple -exe=me,mplcg -option=\"-O2 --quiet:-O2 -quiet\" $name.mpl
$MAPLE_ROOT/bin/arm64-clang-release/maple -exe=me,mplcg -option="-O2 --quiet:-O2 -quiet" $name.mpl > doit.log

# .s -> .out
echo /usr/bin/aarch64-linux-gnu-gcc -o $name.out $name.s
/usr/bin/aarch64-linux-gnu-gcc -o $name.out $name.s

# execute .out either with qemu or native
echo qemu-aarch64 -L /usr/aarch64-linux-gnu/ $name.out
qemu-aarch64 -L /usr/aarch64-linux-gnu/ $name.out

#2. using maple engine .s
# pleasse refer to maple_engine at https://gitee.com/openarkcompiler-incubator/maple_engine
