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

OUTPUT_DIR := ${MAPLE_ROOT}/out/${MAPLE_BUILD_TYPE}
OUTPUT_BIN_DIR := ${MAPLE_ROOT}/out/${MAPLE_BUILD_TYPE}/bin
BUILD_TYPE := RELEASE
HOST_ARCH := 64
MIR_JAVA := 1
GN := ${MAPLE_ROOT}/tools/gn/gn
NINJA := ${MAPLE_ROOT}/tools/ninja/ninja

USE_CLANG_OP := 1
HOST_ARCH_OP := 64
JAVA_OP := 1
USE_ZRT := 0
DEFERRAL_RC := OFF
STRICT_NAIVE_RC := OFF
RC_TESTING := OFF
USE_MALLOC := 
COV_CHECK := 0

ifeq ($(TARGET_PROCESSOR), aarch64)
  TARGET=aarch64
else
  ifeq ($(TARGET_PROCESSOR), riscv64)
    TARGET=riscv64
  else
    TARGET=ark
  endif
endif

ifeq ($(TARGET_SCOPE), release)
  BUILD_TYPE_OP := RELEASE
else
  BUILD_TYPE_OP := DEBUG
endif


GN_OPTIONS := \
  GN_INSTALL_PREFIX="$(MAPLE_ROOT)" \
  GN_BUILD_TYPE="$(BUILD_TYPE_OP)" \
  USE_CLANG=$(USE_CLANG_OP) \
  HOST_ARCH=$(HOST_ARCH_OP) \
  JAVA=$(JAVA_OP) \
  USE_ZRT=$(USE_ZRT) \
  DEFERRAL_RC="$(DEFERRAL_RC)" \
  STRICT_NAIVE_RC="$(STRICT_NAIVE_RC)" \
  RC_TESTING="$(RC_TESTING)" \
  USE_MALLOC="$(USE_MALLOC)" \
  COV_CHECK=$(COV_CHECK) \
  PLATFORM_SDK_VERSION=27 \
  TARGET="$(TARGET)" \
  X86=1

.PHONY: default
default: mapleall

.PHONY: mapleall
mapleall:
	$(call build_gn, ${GN_OPTIONS}, irbuild maple mplcg)

.PHONY: js2mpl
js2mpl:
	$(call build_gn, ${GN_OPTIONS}, js2mpl)

.PHONY: mplbe
mplbe:
	$(call build_gn, ${GN_OPTIONS}, mplbe)

.PHONY: install
install: mapleall
	$(shell mkdir -p ${MAPLE_ROOT}/bin; \
          rm -rf ${MAPLE_EXECUTE_BIN}; \
          mkdir -p ${MAPLE_EXECUTE_BIN}; \
          cp -p ${OUTPUT_BIN_DIR}/* ${MAPLE_EXECUTE_BIN})

.PHONY: clean
clean:
	@rm -rf out/bin/${MAPLE_BUILD_TYPE} out/${MAPLE_BUILD_TYPE}

.PHONY: cleanall
cleanall:
	@rm -rf out bin/*-clang-*

.PHONY: clobber
clobber: cleanall

define build_gn
    mkdir -p ${OUTPUT_DIR}; \
    $(GN) gen ${OUTPUT_DIR} --args='$(1)' --export-compile-commands; \
    cd ${OUTPUT_DIR}; \
    $(NINJA) -v $(2);
endef
