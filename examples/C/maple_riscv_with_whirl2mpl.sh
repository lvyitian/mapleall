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
WORKDIR=$CURRDIR/riscv_use_whirl2mpl

mkdir $WORKDIR
echo cd $WORKDIR
cd $WORKDIR

name=printHuawei

echo ========================================================================
echo ============= Use clangfe/whirl2mpl as C Frontend =======================
echo ========================================================================

cp $CURRDIR/$name.c .

# to handle proper version
INC=$(find /usr/riscv64-linux-gnu/lib/gcc/riscv64-unknown-linux-gnu/*/include | head -1)
FLAGS="-cc1 -emit-llvm -triple riscv64-linux-gnu -D__clang__ -D__BLOCKS__ -D__riscv_xlen=64 -U __riscv_float_abi_soft -D__riscv_float_abi_double -isystem $INC -isystem /usr/riscv64-linux-gnu/sysroot/usr/include -U __SIZEOF_INT128__"
echo $MAPLE_ROOT/tools/open64_prebuilt/x86/riscv64/bin/clangfe $FLAGS $name.c
$MAPLE_ROOT/tools/open64_prebuilt/x86/riscv64/bin/clangfe $FLAGS $name.c > doit.log 2>&1

echo $MAPLE_ROOT/tools/open64_prebuilt/x86/riscv64/bin/whirl2mpl $name.B
$MAPLE_ROOT/tools/open64_prebuilt/x86/riscv64/bin/whirl2mpl $name.B >> doit.log 2>&1

echo $MAPLE_ROOT/bin/riscv64-clang-release/maple -exe=me,mplcg -option=\"-O2 --quiet:-O2 -quiet\" $name.bpl
$MAPLE_ROOT/bin/riscv64-clang-release/maple -exe=me,mplcg -option="-O2 --quiet:-O2 -quiet" $name.bpl >> doit.log 2>&1

echo /usr/riscv64-linux-gnu/bin/riscv64-unknown-linux-gnu-gcc -o $name.out $name.s
/usr/riscv64-linux-gnu/bin/riscv64-unknown-linux-gnu-gcc -o $name.out $name.s

echo LD_LIBRARY_PATH=/usr/riscv64-linux-gnu/sysroot/lib /usr/bin/qemu-riscv64 $name.out
LD_LIBRARY_PATH=/usr/riscv64-linux-gnu/sysroot/lib /usr/bin/qemu-riscv64 $name.out


