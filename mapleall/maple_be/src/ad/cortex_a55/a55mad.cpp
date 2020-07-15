/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan PSL v1.
 * You can use this software according to the terms and conditions of the Mulan PSL v1.
 * You may obtain a copy of Mulan PSL v1 at:
 *
 *     http://license.coscl.org.cn/MulanPSL
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v1 for more details.
 */


#include "a55mad.h"

namespace maplebe {
using namespace std;

const char *A55Unit::GetName() const {
  switch (GetUnitId()) {
#include "mplad_unit.name"
    default:
      return nullptr;
  }
}

void A55MAD::InitUnits() {
#include "mplad_unit_define.def"
}

void A55MAD::InitParallelism() {
#include "mplad_arch_define.def"
}

bool A55MAD::IsSlot0Free() const {

  if (GetUnitByUnitId(kUnitIdSlot0)->IsFree(0)) {
    return false;
  } else {
    return true;
  }
}

bool A55MAD::IsFullIssued() const {
  if (GetUnitByUnitId(kUnitIdSlot0)->IsFree(0) ||
      GetUnitByUnitId(kUnitIdSlot1)->IsFree(0)) {
    return false;
  } else {
    return true;
  }
}

void A55MAD::InitReservation() {
  #include "mplad_reservation_define.def"
}

#define NEWBYPASS(DEFLTTY, USELTTY, LT) (new Bypass(DEFLTTY, USELTTY, LT))

#define ADDBYPASS(DEFLTTY, USELTTY, LT) AddBypass((NEWBYPASS(DEFLTTY, USELTTY, LT)))

void A55MAD::InitBypass() {
  ADDBYPASS(kLtShift, kLtAlu, 0);
  ADDBYPASS(kLtShiftReg, kLtAlu, 0);

  ADDBYPASS(kLtShift, kLtShift, 1);
  ADDBYPASS(kLtShift, kLtShiftReg, 1);
  ADDBYPASS(kLtShiftReg, kLtShift, 1);
  ADDBYPASS(kLtShiftReg, kLtShiftReg, 1);
  ADDBYPASS(kLtShift, kLtAluShift, 1);
  ADDBYPASS(kLtShift, kLtAluShiftReg, 1);
  ADDBYPASS(kLtShiftReg, kLtAluShift, 1);
  ADDBYPASS(kLtShiftReg, kLtAluShiftReg, 1);

  ADDBYPASS(kLtAlu, kLtAlu, 1);
  ADDBYPASS(kLtAluShift, kLtAlu, 1);
  ADDBYPASS(kLtAluShiftReg, kLtAlu, 1);

  ADDBYPASS(kLtAlu, kLtAluShift, 1);      // "arm_no_early_alushift_dep"
  ADDBYPASS(kLtAlu, kLtAluShiftReg, 1);
  ADDBYPASS(kLtAluShift, kLtAluShift, 1);
  ADDBYPASS(kLtAluShift, kLtAluShiftReg, 1);
  ADDBYPASS(kLtAluShiftReg, kLtAluShift, 1);
  ADDBYPASS(kLtAluShiftReg, kLtAluShiftReg, 1);
  ADDBYPASS(kLtAluExtr, kLtAluShift, 1);
  ADDBYPASS(kLtAluExtr, kLtAluShiftReg, 1);

  ADDBYPASS(kLtAlu, kLtAluShift, 2);
  ADDBYPASS(kLtAlu, kLtAluShiftReg, 2);
  ADDBYPASS(kLtAlu, kLtAluExtr, 2);
  ADDBYPASS(kLtAluShift, kLtAluShift, 2);
  ADDBYPASS(kLtAluShift, kLtAluShiftReg, 2);
  ADDBYPASS(kLtAluShift, kLtAluExtr, 2);
  ADDBYPASS(kLtAluShiftReg, kLtAluShift, 2);
  ADDBYPASS(kLtAluShiftReg, kLtAluShiftReg, 2);
  ADDBYPASS(kLtAluShiftReg, kLtAluExtr, 2);
  ADDBYPASS(kLtAluExtr, kLtAluShift, 2);
  ADDBYPASS(kLtAluExtr, kLtAluShiftReg, 2);
  ADDBYPASS(kLtAluExtr, kLtAluExtr, 2);
  ADDBYPASS(kLtAlu, kLtShift, 2);
  ADDBYPASS(kLtAlu, kLtShiftReg, 2);
  ADDBYPASS(kLtAluShift, kLtShift, 2);
  ADDBYPASS(kLtAluShift, kLtShiftReg, 2);
  ADDBYPASS(kLtAluShiftReg, kLtShift, 2);
  ADDBYPASS(kLtAluShiftReg, kLtShiftReg, 2);
  ADDBYPASS(kLtAluExtr, kLtShift, 2);
  ADDBYPASS(kLtAluExtr, kLtShiftReg, 2);

  ADDBYPASS(kLtMul, kLtMul, 2);  //"aarch_accumulator_forwarding"

  ADDBYPASS(kLtMul, kLtAlu, 2);

  ADDBYPASS(kLtMul, kLtAluShift, 3);
  ADDBYPASS(kLtMul, kLtAluShiftReg, 3);
  ADDBYPASS(kLtMul, kLtAluExtr, 3);
  ADDBYPASS(kLtMul, kLtShift, 3);
  ADDBYPASS(kLtMul, kLtShiftReg, 3);

  ADDBYPASS(kLtLoad1, kLtAlu, 2);

  ADDBYPASS(kLtLoad1, kLtAluShift, 3);
  ADDBYPASS(kLtLoad1, kLtAluShiftReg, 3);
  ADDBYPASS(kLtLoad1, kLtAluExtr, 3);
  ADDBYPASS(kLtLoad1, kLtShift, 3);
  ADDBYPASS(kLtLoad1, kLtShiftReg, 3);

  ADDBYPASS(kLtLoad2, kLtAlu, 3);

  ADDBYPASS(kLtAlu, kLtStore1, 0);  //"arm_no_early_store_addr_dep"
  ADDBYPASS(kLtAlu, kLtStore2, 0);
  ADDBYPASS(kLtAlu, kLtStore3plus, 0);
  ADDBYPASS(kLtAluShift, kLtStore1, 0);
  ADDBYPASS(kLtAluShift, kLtStore2, 0);
  ADDBYPASS(kLtAluShift, kLtStore3plus, 0);
  ADDBYPASS(kLtAluShiftReg, kLtStore1, 0);
  ADDBYPASS(kLtAluShiftReg, kLtStore2, 0);
  ADDBYPASS(kLtAluShiftReg, kLtStore3plus, 0);
  ADDBYPASS(kLtAluExtr, kLtStore1, 0);
  ADDBYPASS(kLtAluExtr, kLtStore2, 0);
  ADDBYPASS(kLtAluExtr, kLtStore3plus, 0);

  ADDBYPASS(kLtShift, kLtStore1, 0);  //"arm_no_early_store_addr_dep"
  ADDBYPASS(kLtShift, kLtStore2, 0);
  ADDBYPASS(kLtShift, kLtStore3plus, 0);
  ADDBYPASS(kLtShiftReg, kLtStore1, 0);
  ADDBYPASS(kLtShiftReg, kLtStore2, 0);
  ADDBYPASS(kLtShiftReg, kLtStore3plus, 0);

  ADDBYPASS(kLtMul, kLtStore1, 1);  //"arm_no_early_store_addr_dep"
  ADDBYPASS(kLtMul, kLtStore2, 1);
  ADDBYPASS(kLtMul, kLtStore3plus, 1);

  ADDBYPASS(kLtLoad1, kLtStore1, 1);  //"arm_no_early_store_addr_dep"
  ADDBYPASS(kLtLoad1, kLtStore2, 1);
  ADDBYPASS(kLtLoad1, kLtStore3plus, 1);
  ADDBYPASS(kLtLoad2, kLtStore1, 1);
  ADDBYPASS(kLtLoad2, kLtStore2, 1);
  ADDBYPASS(kLtLoad2, kLtStore3plus, 1);
  ADDBYPASS(kLtLoad3plus, kLtStore1, 1);
  ADDBYPASS(kLtLoad3plus, kLtStore2, 1);
  ADDBYPASS(kLtLoad3plus, kLtStore3plus, 1);

  ADDBYPASS(kLtLoad1, kLtLoad1, 3);  //"arm_early_load_addr_dep_ptr"
  ADDBYPASS(kLtLoad1, kLtLoad2, 3);
  ADDBYPASS(kLtLoad1, kLtLoad3plus, 3);

  ADDBYPASS(kLtLoad1, kLtStore1, 3);  //"arm_early_store_addr_dep_ptr"
  ADDBYPASS(kLtLoad1, kLtStore2, 3);
  ADDBYPASS(kLtLoad1, kLtStore3plus, 3);

  ADDBYPASS(kLtAlu, kLtR2f, 0);
  ADDBYPASS(kLtAluShift, kLtR2f, 0);
  ADDBYPASS(kLtAluShiftReg, kLtR2f, 0);
  ADDBYPASS(kLtAluExtr, kLtR2f, 0);

  ADDBYPASS(kLtShift, kLtR2f, 0);
  ADDBYPASS(kLtShiftReg, kLtR2f, 0);

  ADDBYPASS(kLtMul, kLtR2f, 1);
  ADDBYPASS(kLtLoad1, kLtR2f, 1);
  ADDBYPASS(kLtLoad2, kLtR2f, 1);

  ADDBYPASS(kLtAlu, kLtR2fCvt, 2);
  ADDBYPASS(kLtAluShift, kLtR2fCvt, 2);
  ADDBYPASS(kLtAluShiftReg, kLtR2fCvt, 2);
  ADDBYPASS(kLtAluExtr, kLtR2fCvt, 2);

  ADDBYPASS(kLtMul, kLtR2fCvt, 3);
  ADDBYPASS(kLtLoad1, kLtR2fCvt, 3);
  ADDBYPASS(kLtLoad2, kLtR2fCvt, 3);

  ADDBYPASS(kLtAlu, kLtBranch, 0);
  ADDBYPASS(kLtAluShift, kLtBranch, 0);
  ADDBYPASS(kLtAluShiftReg, kLtBranch, 0);
  ADDBYPASS(kLtAluExtr, kLtBranch, 0);

  ADDBYPASS(kLtShift, kLtBranch, 0);
  ADDBYPASS(kLtShiftReg, kLtBranch, 0);

  ADDBYPASS(kLtFpalu, kLtFpmac, 1);  //"aarch_accumulator_forwarding"
  ADDBYPASS(kLtFpmul, kLtFpmac, 1);
  ADDBYPASS(kLtR2f, kLtFpmac, 1);
  ADDBYPASS(kLtR2fCvt, kLtFpmac, 1);
  ADDBYPASS(kLtFconst, kLtFpmac, 1);

  ADDBYPASS(kLtFLoad64, kLtFpmac, 1);
  ADDBYPASS(kLtFLoadMany, kLtFpmac, 1);

  ADDBYPASS(kLtFpmac, kLtFpmac, 4);             //"aarch_accumulator_forwarding"
  ADDBYPASS(kLtCryptoAese, kLtCryptoAesmc, 0);  //"aarch_crypto_can_dual_issue"
}

}  // namespace maplebe
