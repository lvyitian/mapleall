#!/bin/bash
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

if [ ! -f ../bin/ast2mpl ]; then
  cd ../bin/ast2mpl_files
  cat ast2mpl_aa ast2mpl_ab ast2mpl_ac ast2mpl_ad > ast2mpl.gz
  gunzip ast2mpl.gz
  chmod 775 ast2mpl
  mv ast2mpl ..
  cd ../../tools
  echo Merged ast2mpl.
fi

if [ ! -f ./ninja_1.9.0/ninja ]; then
  mkdir -p ./ninja_1.9.0
  cd ./ninja_1.9.0 || exit 3
  wget https://github.com/ninja-build/ninja/releases/download/v1.9.0/ninja-linux.zip
  unzip ninja-linux.zip
  cd ..
  echo Downloaded ninja 1.9.0.
fi

if [ ! -f ./gn/gn ]; then
  git clone https://gitee.com/xlnb/gn_binary.git gn
  chmod +x gn/gn
  echo Downloaded gn.
fi

if [ ! -f ./open64_prebuilt/README.md ]; then
  git clone https://gitee.com/open64ark/open64_prebuilt.git
fi
if [ ! -f ./open64_prebuilt/x86/riscv64/bin/clangfe ]; then
  cd ./open64_prebuilt/x86
  git pull
  tar zxf open64ark-aarch64.tar.gz
  tar zxf open64ark-riscv.tar.gz
  mv riscv riscv64
  cd ../..
  echo Downloaded open64_prebuilt.
fi

if [ ! -f ./dwarf/include/dwarf2.h ]; then
  git clone https://gitee.com/hu-_-wen/dwarf_files.git dwarf
  echo Downloaded dwarf header files.
fi

