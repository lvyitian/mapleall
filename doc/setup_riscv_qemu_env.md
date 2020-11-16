#Set up a QEMU Environment for Cross Compilation and Execution in RISC-V
last updated: 2020-11-10


Here are step-by-step instructions for setting up . We did this demo in the environment below:

        Operating System:       Ubuntu 18.04.5 LTS
        Kernel:                 4.15.0
        GCC:                    7.5.0

        Note: ubuntu 16.04.4 LTS with gcc 5.4.0 also works

##Build the RISC-V Linux GNU Toolchain

1. Update environment with required packages for building GNU toolchain.

        sudo apt install -y autoconf automake autotools-dev bc bison bison build-essential curl flex gawk gperf libexpat-dev libgmp-dev libmpc-dev libmpfr-dev libtool patchutils python3 texinfo zlib1g-dev

2. Clone the repository from the gitee to your local environment.

        git clone --recursive https://github.com/riscv/riscv-gnu-toolchain

        sudo mkdir /usr/riscv64-linux-gnu
        export PATH=/usr/riscv64-linux-gnu/bin:$PATH

        cd riscv-gnu-toolchain/
        ./configure --prefix=/usr/riscv64-linux-gnu
        sudo make -j linux

        sudo ln -s /usr/riscv64-linux-gnu/sysroot/lib/ld-linux-riscv64-lp64d.so.1 /lib

##Build the QEMU

3. Update environment with required packages for building QEMU.

        sudo apt install -y libglib2.0-dev libpixman-1-dev pkg-config zlib1g-dev

4. Build QEMU

        cd qemu
        
        ./configure --static --disable-system --target-list=riscv64-linux-user
        make -j

        sudo cp riscv64-linux-user/qemu-riscv64 /usr/bin

##Test It

5. Prepare a C source file with content below, and put the file name as **hello_world.c**

```c
        #include <stdio.h>

        int main() {
        printf("Hello World from RISC-V QEMU!\n");
        return 0;
        }
```


6. Compile the above helllo-world example and run it for fun.

        /usr/riscv64-linux-gnu/bin/riscv64-unknown-linux-gnu-gcc hello_world.c
        LD_LIBRARY_PATH=/usr/riscv64-linux-gnu/sysroot/lib qemu-riscv64 ./a.out

    The output is as below:

        qemu $ LD_LIBRARY_PATH=/usr/riscv64-linux-gnu/sysroot/lib qemu-riscv64 ./a.out
        Hello World from RISC-V QEMU!
        qemu $
