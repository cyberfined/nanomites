#ifndef _AES_H_
#define _AES_H_

#include <stdint.h>
#include <stddef.h>

#define CBC 1
#define AES256 1
#define AES_BLOCKLEN 16
#define AES_KEYLEN 32
#define AES_keyExpSize 240

struct AES_ctx
{
  uint8_t *RoundKey;
  uint8_t *Iv;
};

void AES_CBC_encrypt_buffer(struct AES_ctx* ctx, uint8_t* buf, size_t length);

#endif
