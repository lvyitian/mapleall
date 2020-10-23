# A Tutorial and FAQ for Building Maple for ARM and Compiling C Programs
last updated: 2020-10-22

## A General Guidance with Step by Step Instructions
Here is a step by step example of building Maple for ARM relase and running a real example which wiil print some ASCII text. We did this demo in the environment below:

        Operating System:       Ubuntu 18.04.5 LTS
        Kernel:                 4.15.0
        GCC:                    7.5.0

        Note: ubuntu 16.04.4 LTS with gcc 5.4.0 also works

1. Clone the repository from the gitee to your local environment.

        git clone https://gitee.com/openarkcompiler-incubator/mapleall.git
        cd mapleall

2. Set up tools for testing.

        cd tools/
        ./setup_tools.sh
        cd ..

3. Initialize the environment for building the Maple for ARM with the release version.

        source envsetup.sh arm release

4. Now, can we make and install the Maple, and all maple excutables are in **$MAPLE_ROOT/bin** directory

        make
        make install

5. Run the two examples for fun.

    First, we will use **ast2mpl** as C Frontend to print the ASCII text.

        cd examples/C
        ./maple_with_ast2mpl.sh

    The output is as below:

        cd /home/lin/mapleall/examples/C/use_ast2mp
        ========================================================================
        ================== Use ast2mpl as C Frontend ===========================
        ========================================================================
        INC=/usr/bin/../lib/gcc/x86_64-linux-gnu/7.5.0
        /home/lin/mapleall/bin/ast2mpl printHuawei.c -I /usr/bin/../lib/gcc/x86_64-linux-gnu/7.5.0/include
        /home/lin/mapleall/bin/arm64-clang-release/maple -exe=me,mplcg -option="-O2 --quiet:-O2 -quiet" printHuawei.mpl
        /usr/bin/aarch64-linux-gnu-gcc -o printHuawei.out printHuawei.s -lm
        qemu-aarch64 -L /usr/aarch64-linux-gnu/ printHuawei.out

        HHHHHH         HHHHHH    HHHHHH         HHHHHH           HHHHH        HHHHHH        HHHHHH        HHHHHH       HHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH          HHHHHHH        HHHHHH      HHHHHHHH      HHHHHH       HHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH         HHHHHHHHH       HHHHHH      HHHHHHHH      HHHHHH      HHHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH         HHHHHHHHH       HHHHHH     HHHHHHHHH      HHHHHH     HHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH        HHHHHHHHHHH       HHHHH     HHHHHHHHHH     HHHHH     HHHHHHH                HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH        HHHHHHHHHHH       HHHHHH    HHHHHHHHHH    HHHHHH     HHHHHH                 HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH       HHHHHH HHHHHH      HHHHHH   HHHHHHHHHHH    HHHHHH    HHHHHH                  HHHHHH
        HHHHHHHHHHHHHHHHHHHHH    HHHHHH         HHHHHH       HHHHHH HHHHHH       HHHHH   HHHHH HHHHHH   HHHHH     HHHHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHHHHHHHHHHHHHHHHH    HHHHHH         HHHHHH      HHHHHH   HHHHHH      HHHHHH  HHHHH  HHHHH  HHHHHH     HHHHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHHHHHHHHHHHHHHHHH    HHHHHH         HHHHHH      HHHHHH   HHHHHH      HHHHHH HHHHHH  HHHHHH HHHHH      HHHHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHHHHHHHHHHHHHHHHH    HHHHHH         HHHHHH     HHHHHH     HHHHHH     HHHHHH HHHHH   HHHHHH HHHHH      HHHHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH     HHHHHHHHHHHHHHHHH      HHHHH HHHHH    HHHHH HHHHH      HHHHHH                  HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH    HHHHHHHHHHHHHHHHHHH     HHHHHHHHHHH    HHHHHHHHHHH      HHHHHH                  HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH    HHHHHHHHHHHHHHHHHHH     HHHHHHHHHH      HHHHHHHHHH      HHHHHHH                 HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH   HHHHHHHHHHHHHHHHHHHHH     HHHHHHHHH      HHHHHHHHH        HHHHHH                 HHHHHH
        HHHHHH         HHHHHH     HHHHHH       HHHHHH    HHHHHH         HHHHHH     HHHHHHHHH      HHHHHHHHH        HHHHHHHH               HHHHHH
        HHHHHH         HHHHHH     HHHHHHHHHHHHHHHHHHH   HHHHHH           HHHHHH    HHHHHHHH        HHHHHHHH         HHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH      HHHHHHHHHHHHHHHHH    HHHHHH           HHHHHH    HHHHHHHH        HHHHHHHH          HHHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH       HHHHHHHHHHHHHHH    HHHHHH             HHHHHH    HHHHHHH        HHHHHHH             HHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH         HHHHHHHHHHH      HHHHHH             HHHHHH    HHHHHH          HHHHHH               HHHHHHHHHHHH     HHHHHH


    Then, we can use **opencc/whirl2mpl** as C Frontend to print the same text.

        ./maple_with_whirl2mpl.sh

    Here is the output:

        cd /home/lin/mapleall/examples/C/use_whirl2mpl
        ========================================================================
        ============= Use clangfe/whirl2mpl as C Frontend =======================
        ========================================================================
        /home/lin/mapleall/tools/aarch64/bin/clangfe -cc1 -emit-llvm -triple aarch64-linux-gnu -D__clang__ -D__BLOCKS__ -isystem /usr/aarch64-linux-gnu/include -isystem /usr/lib/gcc-cross/aarch64-linux-gnu/5/include printHuawei.c
        /home/lin/mapleall/tools/aarch64/bin/whirl2mpl printHuawei.B
        /home/lin/mapleall/bin/arm64-clang-release/maple -exe=me,mplcg -option="-O2 --quiet:-O2 -quiet" printHuawei.mpl
        /usr/bin/aarch64-linux-gnu-gcc -o printHuawei.out printHuawei.s -lm
        qemu-aarch64 -L /usr/aarch64-linux-gnu/ printHuawei.out

        HHHHHH         HHHHHH    HHHHHH         HHHHHH           HHHHH        HHHHHH        HHHHHH        HHHHHH       HHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH          HHHHHHH        HHHHHH      HHHHHHHH      HHHHHH       HHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH         HHHHHHHHH       HHHHHH      HHHHHHHH      HHHHHH      HHHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH         HHHHHHHHH       HHHHHH     HHHHHHHHH      HHHHHH     HHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH        HHHHHHHHHHH       HHHHH     HHHHHHHHHH     HHHHH     HHHHHHH                HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH        HHHHHHHHHHH       HHHHHH    HHHHHHHHHH    HHHHHH     HHHHHH                 HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH       HHHHHH HHHHHH      HHHHHH   HHHHHHHHHHH    HHHHHH    HHHHHH                  HHHHHH
        HHHHHHHHHHHHHHHHHHHHH    HHHHHH         HHHHHH       HHHHHH HHHHHH       HHHHH   HHHHH HHHHHH   HHHHH     HHHHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHHHHHHHHHHHHHHHHH    HHHHHH         HHHHHH      HHHHHH   HHHHHH      HHHHHH  HHHHH  HHHHH  HHHHHH     HHHHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHHHHHHHHHHHHHHHHH    HHHHHH         HHHHHH      HHHHHH   HHHHHH      HHHHHH HHHHHH  HHHHHH HHHHH      HHHHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHHHHHHHHHHHHHHHHH    HHHHHH         HHHHHH     HHHHHH     HHHHHH     HHHHHH HHHHH   HHHHHH HHHHH      HHHHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH     HHHHHHHHHHHHHHHHH      HHHHH HHHHH    HHHHH HHHHH      HHHHHH                  HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH    HHHHHHHHHHHHHHHHHHH     HHHHHHHHHHH    HHHHHHHHHHH      HHHHHH                  HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH    HHHHHHHHHHHHHHHHHHH     HHHHHHHHHH      HHHHHHHHHH      HHHHHHH                 HHHHHH
        HHHHHH         HHHHHH    HHHHHH         HHHHHH   HHHHHHHHHHHHHHHHHHHHH     HHHHHHHHH      HHHHHHHHH        HHHHHH                 HHHHHH
        HHHHHH         HHHHHH     HHHHHH       HHHHHH    HHHHHH         HHHHHH     HHHHHHHHH      HHHHHHHHH        HHHHHHHH               HHHHHH
        HHHHHH         HHHHHH     HHHHHHHHHHHHHHHHHHH   HHHHHH           HHHHHH    HHHHHHHH        HHHHHHHH         HHHHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH      HHHHHHHHHHHHHHHHH    HHHHHH           HHHHHH    HHHHHHHH        HHHHHHHH          HHHHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH       HHHHHHHHHHHHHHH    HHHHHH             HHHHHH    HHHHHHH        HHHHHHH             HHHHHHHHHHHHHH     HHHHHH
        HHHHHH         HHHHHH         HHHHHHHHHHH      HHHHHH             HHHHHH    HHHHHH          HHHHHH               HHHHHHHHHHHH     HHHHHH


***

## FAQ and Errors
1. The make complains the **clang** package as below:

        /bin/sh: 1: clang: not found
        ninja: build stopped: subcommand failed

    **[Solution]**

        sudo apt install -y clang


2. if the system misses the **elf** library, you will get error below:

        /usr/bin/ld: cannot find -lelf
        clang: error: linker command failed with exit code 1 (use -v to see invocation)

    **[Solution]**

        sudo apt install -y libelf-dev libssl-dev

3. The build lools for **aarch64-linux-gnu-gcc** which requires the **ARM cross compiling toolchain**.

        ./maple_with_ast2mpl.sh: line 44: /usr/bin/aarch64-linux-gnu-gcc: No such file or directory

    **[Solution]**

        sudo apt install -y gcc-7-aarch64-linux-gnu
        sudo ln -s /usr/bin/aarch64-linux-gnu-gcc-7 /usr/bin/aarch64-linux-gnu-gcc

        note: using gcc-5-aarch64-linux-gnu for ubuntu 16.04.4 works as well

4. If the execution of example complains the missing command **qemu-aarch64** as below, we need install the QEMU for ARM.

        ./maple_with_ast2mpl.sh: line 48: qemu-aarch64: command not found

    **[Solution]**

        sudo apt install -y qemu-system-arm qemu-efi-aarch64 qemu-utils qemu-user
