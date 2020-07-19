#
# Copyright (C) [2020] Futurewei Technologies, Inc. All rights reverved.
#
# Licensed under the Mulan Permissive Software License v2.
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

function print_usage {
  echo " "
  echo "usage: source envsetup.sh arm/ark release/debug"
  echo " "
}

if [ "$#" -lt 2 ]; then
  print_usage
  return
fi

curdir=$(pwd)
unset MAPLE_ROOT
export MAPLE_ROOT=${curdir}

unset TARGET_ARCH
export TARGET_ARCH=64

if [ $1 = "arm" ]; then
  PLATFORM=arm64
  USEOJ=0
else if [ $1 = "ark" ]; then
    PLATFORM=ark
    USEOJ=1
  else
    print_usage
    return
  fi
fi

if [ "$2" = "release" ]; then
  TYPE=release
else if [ "$2" = "debug" ]; then
    TYPE=debug
  else
    print_usage
    return
  fi
fi

unset TARGET_PROCESSOR
export TARGET_PROCESSOR=${PLATFORM}

unset TARGET_SCOPE
export TARGET_SCOPE=${TYPE}

unset USE_OJ_LIBCORE
export USE_OJ_LIBCORE=${USEOJ}

unset TARGET_TOOLCHAIN
export TARGET_TOOLCHAIN=clang

unset MAPLE_BUILD_TYPE
export MAPLE_BUILD_TYPE=${TARGET_PROCESSOR}-${TARGET_TOOLCHAIN}-${TARGET_SCOPE}

unset MAPLE_EXECUTE_BIN
export MAPLE_EXECUTE_BIN=${MAPLE_ROOT}/bin/${MAPLE_BUILD_TYPE}

