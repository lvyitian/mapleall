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
WORKDIR=$CURRDIR/use_whirl2mpl

mkdir $WORKDIR
echo cd $WORKDIR
cd $WORKDIR

name=printHuawei

echo ========================================================================
echo ============= Use opencc/whirl2mpl as C Frontend =======================
echo ========================================================================

cp $CURRDIR/$name.c .

echo $MAPLE_ROOT/tools/open64ark/bin/opencc -O0 -fe -keep -show -std=gnu99 $name.c
$MAPLE_ROOT/tools/open64ark/bin/opencc -O0 -fe -keep -show -std=gnu99 $name.c > doit.log 2>&1

echo $MAPLE_ROOT/tools/open64ark/bin/whirl2mpl $name.B
$MAPLE_ROOT/tools/open64ark/bin/whirl2mpl $name.B >> doit.log 2>&1

echo $MAPLE_ROOT/bin/arm64-clang-release/maple -exe=me,mplcg -option=\"-O2 --quiet:-quiet\" $name.mpl
$MAPLE_ROOT/bin/arm64-clang-release/maple -exe=me,mplcg -option="-O2 --quiet:-quiet" $name.bpl >> doit.log 2>&1

echo /usr/bin/aarch64-linux-gnu-gcc -o $name.out $name.s -lm
/usr/bin/aarch64-linux-gnu-gcc -o $name.out $name.s -lm

echo qemu-aarch64 -L /usr/aarch64-linux-gnu/ $name.out
qemu-aarch64 -L /usr/aarch64-linux-gnu/ $name.out


