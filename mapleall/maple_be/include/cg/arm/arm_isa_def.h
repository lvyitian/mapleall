/*
 * Copyright (c) [2020] Huawei Technologies Co., Ltd. All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan Permissive Software License v2.
 * You can use this software according to the terms and conditions of the MulanPSL - 2.0.
 * You may obtain a copy of MulanPSL - 2.0 at:
 *
 *   https://opensource.org/licenses/MulanPSL-2.0
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the MulanPSL - 2.0 for more details.
 */

MOP_Tbadcrrr, MOP_Tbaddrri8, MOP_Tbaddrri3s, MOP_Tbaddri8, MOP_Tbaddrrr, MOP_Tbadd64rrrs, MOP_Tbadd64rris,
  MOP_Tbaddrrlh, MOP_Tbaddrrhl, MOP_Tbaddrrhh, MOP_Tbaddpcrel, MOP_Tbaddsprel, MOP_Tbaddspi7, MOP_Tbandrr,
  MOP_Tbasrrri5, MOP_Tbasrsrrr, MOP_Tbbcond, MOP_Tbbuncond, MOP_Tbbicrr, MOP_Tbbkpt, MOP_Tbbl, MOP_Tbblx2, MOP_Tbbl1,
  MOP_Tbbl2, MOP_Tbblxr, MOP_Tbbx, MOP_Tbcmnrr, MOP_Tbcmpri8, MOP_Tbcmprr, MOP_Tbcmplh, MOP_Tbcmphl, MOP_Tbcmphh,
  MOP_Tbeorrr, MOP_Tbldmia, MOP_Tbld, MOP_Tbldwb, MOP_Tbld64l, MOP_Tbld64h, MOP_Tbldrbrri5, MOP_Tbldrb, MOP_Tbldrhrri5,
  MOP_Tbldrh, MOP_Tbldrsb, MOP_Tbldrsh, MOP_Tblslrri5, MOP_Tblslsrrr, MOP_Tblsrrri5, MOP_Tblsrsrrr, MOP_Tbmovimm8,
  MOP_Tbmovrr, MOP_Tbmovrr_h2h, MOP_Tbmovrr_l2l, MOP_Tbmovrr_l2h, MOP_Tbmov64ri12h, MOP_Tbmov64ri12l, MOP_Tbmul,
  MOP_Tbmvn, MOP_Tbmvn64i12l, MOP_Tbmvn64i12h, MOP_Tbneg, MOP_Tborr, MOP_Tbpop, MOP_Tbpush, MOP_Tbrev, MOP_Tbrevsh,
  MOP_Tbrorrr, MOP_Tbsbc, MOP_Tbstmia, MOP_Tbstr, MOP_Tbstrsprel, MOP_Tbstrbrri5, MOP_Tbstrb, MOP_Tbstrhrri5,
  MOP_Tbstrh, MOP_Tbsubrri3, MOP_Tbsubri8, MOP_Tbsubrrr, MOP_Tbsubspi7, MOP_Tbswi, MOP_Tbtst, MOP_Tb2vldrs,
  MOP_Tb2vldrd, MOP_Tb2vmuls, MOP_Tb2vmuld, MOP_Tb2vmlas, MOP_Tb2vmlad, MOP_Tb2vmlss, MOP_Tb2vmlsd, MOP_Tb2vstrs,
  MOP_Tb2vstrd, MOP_Tb2vsubs, MOP_Tb2vsubd, MOP_Tb2vadds, MOP_Tb2vaddd, MOP_Tb2vdivs, MOP_Tb2vdivd, MOP_Tb2vmlaf64,
  MOP_Tb2vcvtif, MOP_Tb2vcvtuf, MOP_Tb2vcvtid, MOP_Tb2vcvtud, MOP_Tb2vcvtfi, MOP_Tb2vcvtfu, MOP_Tb2vcvtdi,
  MOP_Tb2vcvtdu, MOP_Tb2vcvtfd, MOP_Tb2vcvtdf, MOP_Tb2vcvtf64s32, MOP_Tb2vcvtf64u32, MOP_Tb2vsqrts, MOP_Tb2vsqrtd,
  MOP_Tb2movimm8, MOP_Tb2movi8m, MOP_Tb2movimm12, MOP_Tb2strrri12, MOP_Tb2ldrrri12, MOP_Tb2strrri8predec,
  MOP_Tb2ldrrri8predec, MOP_Tb2cbnz, MOP_Tb2cbz, MOP_Tb2addrri12, MOP_Tb2movrr, MOP_Tb2vmovs, MOP_Tb2vmovd,
  MOP_Tb2vmovsc, MOP_Tb2vmovdc, MOP_Tb2ldmia, MOP_Tb2stmia, MOP_Tb2addrrr, MOP_Tb2subrrr, MOP_Tb2sbcrrr, MOP_Tb2cmprr,
  MOP_Tb2subrri12, MOP_Tb2mvni12, MOP_Tb2sel, MOP_Tb2ubfx, MOP_Tb2sbfx, MOP_Tb2ldrrrr, MOP_Tb2ldrhrrr, MOP_Tb2ldrsh,
  MOP_Tb2ldrbrrr, MOP_Tb2ldrsbrrr, MOP_Tb2strrrr, MOP_Tb2strh, MOP_Tb2strb, MOP_Tb2ldrhrri12, MOP_Tb2ldrshrri12,
  MOP_Tb2ldrbrri12, MOP_Tb2ldrsbrri12, MOP_Tb2strhrri12, MOP_Tb2strbrri12, MOP_Tb2pop, MOP_Tb2push, MOP_Tb2cmpri8m,
  MOP_Tb2cmnri8m, MOP_Tb2adc64rrr, MOP_Tb2adc64rri, MOP_Tb2orr64lrrr, MOP_Tb2orr64hrrr, MOP_Tb2and64lrrr,
  MOP_Tb2and64hrrr, MOP_Tb2andrrr, MOP_Tb2bicrrr, MOP_Tb2cmnrr, MOP_Tb2eorrrr, MOP_Tb2mulrrr, MOP_Tb2sdivrrr,
  MOP_Tb2udivrrr, MOP_Tb2mnvrr, MOP_Tb2rsubrri8, MOP_Tb2rsubsrri8, MOP_Tb2negrr, MOP_Tb2orrrrr, MOP_Tb2tstrr,
  MOP_Tb2lslrrr, MOP_Tb2lsrrrr, MOP_Tb2asrrrr, MOP_Tb2rorrrr, MOP_Tb2lslrri5, MOP_Tb2lsrrri5, MOP_Tb2asrrri5,
  MOP_Tb2rorrri5, MOP_Tb2bicrri8m, MOP_Tbandrri8, MOP_Tbandrri8l, MOP_Tbandrri8h, MOP_Tb2andrri8m, MOP_Tborrri8l,
  MOP_Tborrri8h, MOP_Tborrri8, MOP_Tb2orrrri8m, MOP_Tb2eorrri8m, MOP_Tb2addrri8m, MOP_Tb2adcrri8m, MOP_Tb2subsrri8,
  MOP_Tb2sbcrri8m, MOP_Tb2revrr, MOP_Tb2revshrr, MOP_Tb2it, MOP_Tb2fmstat, MOP_Tb2vcmpd, MOP_Tb2vcmps,
  MOP_Tb2ldrpcrel12, MOP_Tb2bcond, MOP_Tb2fmrs, MOP_Tb2fmsr, MOP_Tb2fmrrd, MOP_Tb2fmdrr, MOP_Tb2vabsd, MOP_Tb2vabss,
  MOP_Tb2vnegd, MOP_Tb2vnegs, MOP_Tb2vmovs_imm8, MOP_Tb2vmovd_imm8, MOP_Tb2mla, MOP_Tb2mls, MOP_Tb2umull, MOP_Tb2ldrex,
  MOP_Tb2ldrexd, MOP_Tb2strex, MOP_Tb2strexd, MOP_Tb2clrex, MOP_Tb2bfi, MOP_Tb2bfc, MOP_Tb2dmb, MOP_Tb2ldrpcreln12,
  MOP_Tb2stm, MOP_Tbundefined, MOP_Tb2vpopcs, MOP_Tb2vpushcs, MOP_Tb2vldms, MOP_Tb2vstms, MOP_Tb2buncond,
  MOP_Tb2movimm16h, MOP_Tb2movimm16l, MOP_Tb2addpcr, MOP_Tb2adr, MOP_Tb2movimm16lst, MOP_Tb2movimm16hst, MOP_Tb2ldmiawb,
  MOP_Tb2orrsrrr, MOP_Tb2push1, MOP_Tb2pop1, MOP_Tb2rsubrrr, MOP_Tb2smull, MOP_Tb2ldrd, MOP_Tb2strd, MOP_beq, MOP_bne,
  MOP_blt, MOP_ble, MOP_bgt, MOP_bge, MOP_blo, MOP_bls, MOP_bhs, MOP_bhi, MOP_bpl, MOP_Tb2mul64rlh, MOP_Tb2mla64rhlr,
  MOP_Tb2umull64rrll, MOP_Tb2mov64lr, MOP_Tb2add64hrr, MOP_Tbrsbmiri, MOP_Tbitle, MOP_Tbitls, MOP_Tbitlt, MOP_Tbitcc,
  MOP_Tbitge, MOP_Tbitcs, MOP_Tbitgt, MOP_Tbithi, MOP_Tbitmi, MOP_Tbiteq, MOP_Tbitne, MOP_Tbitpl, MOP_Tbittpl,
  MOP_Tbitele, MOP_Tbitels, MOP_Tbitelt, MOP_Tbitecc, MOP_Tbitege, MOP_Tbitecs, MOP_Tbitegt, MOP_Tbitehi, MOP_Tbitemi,
  MOP_Tbiteeq, MOP_Tbitene, MOP_Tbitepl, MOP_Tb2asrplrrr, MOP_Tb2orrplrrr, MOP_Tb2moveqimm12, MOP_Tb2movneimm12,
  MOP_Tb2movleimm12, MOP_Tb2movlsimm12, MOP_Tb2movltimm12, MOP_Tb2movccimm12, MOP_Tb2movgeimm12, MOP_Tb2movcsimm12,
  MOP_Tb2movgtimm12, MOP_Tb2movhiimm12, MOP_Tb2movmiimm12, MOP_Tb2movplimm12, MOP_Tb2moveqrr, MOP_Tb2fconsts,
  MOP_Tb2fconstd,
