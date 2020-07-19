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

// arithmetic
MOP_add32rr, MOP_add32ri, MOP_add32rm,
  // MOP_add32mr,
  // MOP_add32mi,
  MOP_add64rr, MOP_add64ri, MOP_add64rm,
  // MOP_add64mr,
  // MOP_add64mi,
  MOP_addsdrr, MOP_addsdrm, MOP_addssrr, MOP_addssrm, MOP_addc32rr, MOP_addc32ri, MOP_addc32rm,
  // MOP_addc32mr,
  // MOP_addc32mi,
  MOP_div32r, MOP_div32m, MOP_div64r, MOP_div64m, MOP_idiv32r, MOP_idiv32m, MOP_idiv64r, MOP_idiv64m, MOP_imul32rr,
  MOP_imul32ri, MOP_imul32rm, MOP_imul64rr, MOP_imul64ri, MOP_imul64rm, MOP_mul32, MOP_mul64, MOP_mulsdrr, MOP_mulsdrm,
  MOP_mulssrr, MOP_mulssrm, MOP_inc32r, MOP_inc64r,
  // MOP_inc32m,
  // MOP_inc64m,
  MOP_dec32r, MOP_dec64r,
  // MOP_dec32m,
  // MOP_dec64m,
  MOP_xchg32rr, MOP_xchg32rm, MOP_xchg64rr, MOP_xchg64rm, MOP_neg32r,
  // MOP_neg32m,
  MOP_neg64r,
  // MOP_neg64m,
  MOP_sqrtss, MOP_sqrtsd, MOP_not32r,
  // MOP_not32m,
  MOP_not64r,
  // MOP_not64m,
  MOP_sub32rr, MOP_sub32ri, MOP_sub32rm,
  // MOP_sub32mr,
  // MOP_sub32mi,
  MOP_sub64rr, MOP_sub64ri, MOP_sub64rm,
  // MOP_sub64mr,
  // MOP_sub64mi,
  MOP_subsdrr, MOP_subsdrm, MOP_subssrr, MOP_subssrm, MOP_divssrr, MOP_divssrm, MOP_divsdrr, MOP_divsdrm,
  // logic
  MOP_and32rr, MOP_and32ri, MOP_and32rm,
  // MOP_and32mr,
  // MOP_and32mi,
  MOP_and64rr, MOP_and64ri, MOP_and64rm, MOP_andpsrr, MOP_andpdrr, MOP_andnpsrr, MOP_andnpdrr, MOP_orpsrr, MOP_orpdrr,
  // MOP_and64mr,
  // MOP_and64mi,
  MOP_or32rr, MOP_or32ri, MOP_or32rm,
  // MOP_or32mr,
  // MOP_or32mi,
  MOP_or64rr, MOP_or64ri, MOP_or64rm,
  // MOP_or64mr,
  // MOP_or64mi,
  // MOP_ori32,
  // MOP_ori64,
  MOP_ror32, MOP_ror64, MOP_rori32, MOP_rori64, MOP_rol32, MOP_rol64, MOP_roli32, MOP_roli64, MOP_xor32rr, MOP_xor32ri,
  MOP_xor32rm,
  // MOP_xor32mr,
  // MOP_xor32mi,
  MOP_xor64rr, MOP_xor64ri, MOP_xor64rm, MOP_xorps32rr, MOP_xorps32rm, MOP_xorpd64rr, MOP_xorpd64rm,
  // MOP_xor64mr,
  // MOP_xor64mi,
  // MOP_ori32,
  // MOP_ori64,
  MOP_sar32, MOP_sar64, MOP_sari32, MOP_sari64, MOP_shl32, MOP_shld32, MOP_shldi32, MOP_shrd32, MOP_shrdi32, MOP_shl64,
  MOP_shli32, MOP_shli64, MOP_shr32, MOP_shr64, MOP_shri32, MOP_shri64, MOP_minssrr, MOP_minssrm, MOP_minsdrr,
  MOP_minsdrm, MOP_maxssrr, MOP_maxssrm, MOP_maxsdrr, MOP_maxsdrm, MOP_cmp32rr, MOP_cmp32ri, MOP_cmp32rm, MOP_cmp64rr,
  MOP_cmp64ri, MOP_cmp64rm, MOP_comisdrr, MOP_comisdrm, MOP_comissrr, MOP_comissrm, MOP_ucomisdrr, MOP_ucomisdrm,
  MOP_ucomissrr, MOP_ucomissrm, MOP_cmpssrr, MOP_cmpssrm, MOP_cmpsdrr, MOP_cmpsdrm, MOP_test32rr, MOP_test32ri,
  MOP_test32rm, MOP_test64rr, MOP_test64ri, MOP_test64rm,
  // program flow
  MOP_call, MOP_icallr, MOP_icallm, MOP_jb, MOP_jae, MOP_jp, MOP_jnp, MOP_je, MOP_jne, MOP_jbe, MOP_ja, MOP_jl, MOP_jge,
  MOP_jle, MOP_jg, MOP_jcxz, MOP_jecxz, MOP_jrcxz, MOP_js, MOP_jns, MOP_jmp, MOP_ijmpr, MOP_ijmpm, MOP_leave, MOP_ret,
  MOP_reti, MOP_setb, MOP_setae, MOP_setp, MOP_setnp, MOP_sete, MOP_setne, MOP_setbe, MOP_seta, MOP_setl, MOP_setge,
  MOP_setle, MOP_setg,

  // string operation
  // data movement
  MOP_cmovb32, MOP_cmovae32, MOP_cmovp32, MOP_cmovnp32, MOP_cmove32, MOP_cmovne32, MOP_cmovbe32, MOP_cmova32,
  MOP_cmovl32, MOP_cmovge32, MOP_cmovle32, MOP_cmovg32, MOP_cmovs32, MOP_cmovz32, MOP_cmovo32, MOP_cmovns32,
  MOP_cmovb64, MOP_cmovae64, MOP_cmovp64, MOP_cmovnp64, MOP_cmove64, MOP_cmovne64, MOP_cmovbe64, MOP_cmova64,
  MOP_cmovl64, MOP_cmovge64, MOP_cmovle64, MOP_cmovg64, MOP_cmovs64, MOP_cmovz64, MOP_cmovo64, MOP_cmovns64,
  // MOP_ld8,
  // MOP_ld16,
  MOP_ld32, MOP_ld64,
  // MOP_ldu8,
  // MOP_ldu16,
  MOP_ldss, MOP_ldsd, MOP_st8, MOP_st16, MOP_st32, MOP_st64, MOP_stss, MOP_stsd, MOP_lea32, MOP_lea64, MOP_ldc32,
  MOP_ldc64, MOP_ldc32abs, MOP_ldc64abs, MOP_mov32, MOP_mov64, MOP_movabs32, MOP_movabs64, MOP_movss, MOP_movsd,
  MOP_movsbl, MOP_movzbl, MOP_movswl, MOP_movzwl, MOP_movsbq, MOP_movzbq, MOP_movswq, MOP_movzwq, MOP_movslq,
  MOP_movzlq, MOP_movzql, MOP_movi2fd, MOP_movi2fq, MOP_movf2id, MOP_movf2iq, MOP_ldsbl, MOP_ldzbl, MOP_ldswl,
  MOP_ldzwl, MOP_ldsbq, MOP_ldzbq, MOP_ldswq, MOP_ldzwq, MOP_ldslq, MOP_ldzlq, MOP_ldi2fd, MOP_ldi2fq, MOP_ldf2id,
  MOP_ldf2iq, MOP_popl, MOP_popq, MOP_pushl, MOP_pushq, MOP_cvtss2sdr, MOP_cvtss2sdm, MOP_cvtsd2ssr, MOP_cvtsd2ssm,
  MOP_cvtsi2sdr, MOP_cvtsi2sdm, MOP_cvtsi2ssr, MOP_cvtsi2ssm, MOP_cvtsi2sdqr, MOP_cvtsi2sdqm, MOP_cvtsi2ssqr,
  MOP_cvtsi2ssqm, MOP_cvtss2sir, MOP_cvtss2sim, MOP_cvtsd2sir, MOP_cvtsd2sim, MOP_cvtss2siqr, MOP_cvtss2siqm,
  MOP_cvtsd2siqr, MOP_cvtsd2siqm, MOP_cvttss2sir, MOP_cvttss2sim, MOP_cvttss2si64r, MOP_cvttss2si64m, MOP_cvttsd2sir,
  MOP_cvttsd2sim, MOP_cvttsd2si64r, MOP_cvttsd2si64m, MOP_cvttss2siqr, MOP_cvttss2siqm, MOP_cvttsd2siqr,
  MOP_cvttsd2siqm,
  // mis
  // MOP_cltd,
  // MOP_cqto,
  MOP_zero32i, MOP_zero32f, MOP_zero64f,
