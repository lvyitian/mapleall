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

#ifndef HAVE_OPENSSL
#include "muid.h"

#if 0
void MuidFinal(unsigned char *result, MD5_CTX *ctx)
{
  unsigned long used, available;

  used = ctx->lo & 0x3f;

  ctx->buffer[used++] = 0x80;

  available = 64 - used;

  if (available < 8) {
    (void)memset_s(&ctx->buffer[used], 64, 0, available);
    (void)body(ctx, ctx->buffer, 64);
    used = 0;
    available = 64;
  }

  (void)memset_s(&ctx->buffer[used], 64, 0, available - 8);
  ctx->lo <<= 3;
  OUT(&ctx->buffer[56], ctx->lo)
  OUT(&ctx->buffer[60], ctx->hi)

  (void)body(ctx, ctx->buffer, 64);

  OUT(&result[0], ctx->a)
  OUT(&result[4], ctx->b)

  (void)memset_s(ctx, sizeof(*ctx), 0, sizeof(*ctx));
}
#endif

MUID_t GetMUID(const std::string &symbolName, bool forSystem) {
  MUID_t ret;
  MD5_CTX c;

  MD5_Init(&c);
  MD5_Update(&c, symbolName.c_str(), symbolName.length());
#ifdef USE_64BIT_MUID
  // MuidFinal((unsigned char *)&ret.data, &c);
  assert(false);
#else
  MD5_Final((unsigned char *)&ret.data, &c);
#endif // USE_64BIT_MUID
  if (forSystem) {
    ret.SetSystemNameSpace();
  } else {
    ret.SetApkNameSpace();
  }
  return ret;
}

#endif

