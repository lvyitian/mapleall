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

if [ ! -f ./ninja_1.9.0/ninja ]; then
  mkdir -p ./ninja_1.9.0
  cd ./ninja_1.9.0 || exit 3
  wget https://github.com/ninja-build/ninja/releases/download/v1.9.0/ninja-linux.zip
  unzip ninja-linux.zip
  cd ..
  echo Downloaded ninja 1.9.0.
fi
if [ ! -f ./gn/gn ]; then
  mkdir -p gn
  cd gn || exit 4
  git clone https://gitee.com/xlnb/gn_binary.git
  ln -sf gn_binary/gn .
  chmod +x gn_binary/gn
  cd ..
  echo Downloaded gn.
fi
