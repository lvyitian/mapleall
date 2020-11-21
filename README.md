```
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
```

## MAPLE - Multiple Architecture and Programming Language Environment

Let **MAPLE_ROOT** be the top directory for mapleall repository.

The directory structure as follows:

```
      $MAPLE_ROOT
            README.md    : this file
            Copyright    : Copyright file
            license      : Mulan license file
            doc          : MapleIR documentation
                         : tutorial & FAQ
            mapleall     : maple compiler source
            maple_engine : maple engine opcode header file
            bin/ast2mpl  : C frontend: clangAST to MapleIR
            tools        : ninja, gn and clangfe for C frontend
                         : downloaded by setup_tools.sh
            Makefile     : makefile
            build        : environment set up and build files
            BUILD.gn     : gn build file
            bin          : maple executables during make install
            out          : created during make
```

### Branches
1. master branch: for stable versions
2. dev branch: for daily development

### Set up tools
1. `cd $MAPLE_ROOT`
2. `cd tools`
3. `./setup_tools.sh`

### Build compiler
1. `cd $MAPLE_ROOT`
2. choose {**TARGET**, **VERSION**} combo from {**arm**/**engine**(or **ark**)/**riscv**, **release**/**debug**}
   where **arm** for aarch64 target .s, **engine** (or **ark**) for maple engine target .s and riscv for riscv64 target .s
3. `source envsetup.sh` **TARGET** **VERSION**
   * for example:
   * `source envsetup.sh arm release`
   * `source envsetup.sh engine release`
   * `source envsetup.sh ark release`
   * `source envsetup.sh riscv release`
4. `make`
5. `make install`

### Output
maple excutables are in $MAPLE_ROOT/bin directory

### Usage
refer to examples/ for C language examples
1. `cd examples/C`
2. `./maple_aarch64_with_ast2mpl.sh`
3. `./maple_aarch64_with_whirl2mpl.sh`
3. `./maple_riscv64_with_whirl2mpl.sh`


refer to maple_engine git repository for java2asm.sh and asm2so.sh scripts
1. java2asm.sh: .java -> .mpl -> .s
2. asm2so.sh  : .s -> .so

### Tutorial
Please find step by step instructions to build Maple and run examples in the [tutorial.md](./doc/tutorial.md).

If we want to build Maple for RISC-V, please follow the [instructions](./doc/setup_riscv_qemu_env.md) to prepare QEMU cross-compilation and execution environment.

### Possible issues
You might need to install required packages like:
```
        sudo apt-get install clang
        sudo apt-get install libelf-dev
        sudo apt-get install libssl-dev
```

Please refer to the [FAQ](./doc/tutorial.md#faq-and-errors) page for anwswers to more questions and issues.
