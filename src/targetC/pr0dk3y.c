#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define BYTE3(x) ((unsigned char)((x) >> 24))
#define BYTE2(x) ((unsigned char)((x) >> 16))
#define BYTE1(x) ((unsigned char)((x) >> 8))
#define HIBYTE(x) ((unsigned char)((x) >> 8))

char byte_2030B0[16] = {0, 1, 3, 2, 6, 7, 5, 4, 0xc, 0xd, 0xf, 0xe, 0xa, 0xb};
char byte_1FC0[256] = {0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8};

__int64 sub_19CC(char *flagbuf);
int k20_1_E10(short a1);
int k20_2_DA2(unsigned short a1);
int key_all_F0F(unsigned char *flagbuf, int size);
int k0_k4_k8_1_E40(int k0, int k4, unsigned int k8);
int k16_1566(unsigned int k16);
int k0_1_CA0(unsigned int k0);
int k0_2_D51(unsigned int a1);
int k0_prime_C15(unsigned int k0);
int k4_17B5(unsigned int k4);
int k8_1846(unsigned int k8);
int k0_k4_k8_2_18D2(unsigned int k4, unsigned int k8, int k0);
unsigned short k4_k8_1061(unsigned int k4, unsigned int k8);
unsigned char num_1bit_FCB(unsigned char a1);
int num_1bit_1D20(unsigned __int64 k16);

int main(void){
  printf("%d\n",sub_19CC("abcdefghijklmnopqrstuv"));
  return 0;
}


__int64 sub_19CC(char *flagbuf)
{
  __int64 result; // rax@3
  short k20; // [sp+1Ah] [bp-16h]@1
  unsigned int k0; // [sp+1Ch] [bp-14h]@1
  unsigned int k4; // [sp+20h] [bp-10h]@1
  unsigned int k8; // [sp+24h] [bp-Ch]@1
  int k12; // [sp+28h] [bp-8h]@1
  unsigned int k16; // [sp+2Ch] [bp-4h]@1

  k0 = *(int *)flagbuf;
  k4 = *((int *)flagbuf + 1);
  k8 = *((int *)flagbuf + 2);
  k12 = *((int *)flagbuf + 3);
  k16 = *((int *)flagbuf + 4);
  k20 = *((short *)flagbuf + 10);
  if ( k20_1_E10(k20) || k20_2_DA2(k20) )
  {
    result = 0LL;
  }
  else if ( (key_all_F0F((unsigned char *)flagbuf, 20) & 0x7FFF) == k20 )
  {
    if ( k0_k4_k8_1_E40(k0, k4, k8) == k12 )
    {
      if ( k16_1566(k16) )
      {
        if ( k0_1_CA0(k0) )
        {
          result = 0LL;
        }
        else if ( k0_2_D51(k0) )
        {
          result = 0LL;
        }
        else if ( k0_prime_C15(k0) )
        {
          if ( k4_17B5(k4) )
          {
            if ( k8_1846(k8) )
            {
              if ( k0_k4_k8_2_18D2(k4, k8, k0) )
                result = k4_k8_1061(k4, k8) == k20;
              else
                result = 0LL;
            }
            else
            {
              result = 0LL;
            }
          }
          else
          {
            result = 0LL;
          }
        }
        else
        {
          result = 0LL;
        }
      }
      else
      {
        result = 0LL;
      }
    }
    else
    {
      result = 0LL;
    }
  }
  else
  {
    result = 0LL;
  }
  return result;
}

int k20_1_E10(short a1)
{
  return HIBYTE(a1) == (unsigned char)a1;
}

int k20_2_DA2(unsigned short a1)
{
  return !(((a1 >> 12) ^ HIBYTE(a1)) & 0xF)
      || !((HIBYTE(a1) ^ (unsigned char)(a1 >> 4)) & 0xF)
      || !(((unsigned char)(a1 >> 4) ^ (unsigned char)a1) & 0xF);
}

int key_all_F0F(unsigned char *flagbuf, int size)
{
  signed int v2; // ecx@2
  unsigned short v4; // [sp+14h] [bp-8h]@1
  unsigned short v5; // [sp+16h] [bp-6h]@1
  int i; // [sp+18h] [bp-4h]@1

  v4 = 0;
  v5 = 0;
  for ( i = 0; i < size; ++i )
  {
    v2 = v4 + flagbuf[i];
    v4 = v2 - 255 * (((signed int)(v2 + (-0x7F7F7F7FuLL * v2 >> 32)) >> 7) - (v2 >> 31));
    v5 = (v5 + v4) % 255;
  }
  return (v5 << 8) | v4;
}

int k0_k4_k8_1_E40(int k0, int k4, unsigned int k8)
{
  unsigned int v3; // ST0C_4@1
  unsigned int v4; // ST08_4@1
  unsigned int v5; // ST04_4@1
  int v6; // ST0C_4@1
  int v7; // ST08_4@1
  int v8; // ST04_4@1
  int v9; // ST0C_4@1

  v3 = (k8 >> 13) ^ (k0 - k4 - k8);
  v4 = (v3 << 8) ^ (k4 - k8 - v3);
  v5 = (v4 >> 13) ^ (k8 - v3 - v4);
  v6 = (v5 >> 12) ^ (v3 - v4 - v5);
  v7 = (v6 << 16) ^ (v4 - v5 - v6);
  v8 = ((unsigned int)v7 >> 5) ^ (v5 - v6 - v7);
  v9 = ((unsigned int)v8 >> 3) ^ (v6 - v7 - v8);
  return (((v9 << 10) ^ (unsigned int)(v7 - v8 - v9)) >> 15) ^ (v8 - v9 - ((v9 << 10) ^ (v7 - v8 - v9)));
}

int k16_1566(unsigned int k16)
{
  return (k16 >> 16) - 391 * (k16 / 0x1870000) == (unsigned char)k16
      && (k16 >> 16) & (0xFFFF % (((k16 & 0xFF00) >> 8) + 1) == 0)
      && num_1bit_1D20(k16) > 8;
}

int k0_1_CA0(unsigned int k0)
{
  return k0 >> 28 == ((k0 >> 24) & 0xF)
      || !((BYTE3(k0) ^ (unsigned char)(k0 >> 20)) & 0xF)
      || !(((unsigned char)(k0 >> 20) ^ (unsigned char)(k0 >> 16)) & 0xF)
      || !(((unsigned char)(k0 >> 16) ^ (unsigned char)(k0 >> 12)) & 0xF)
      || !(((unsigned char)(k0 >> 12) ^ BYTE1(k0)) & 0xF)
      || !((BYTE1(k0) ^ (unsigned char)(k0 >> 4)) & 0xF)
      || !(((unsigned char)k0 ^ (unsigned char)(k0 >> 4)) & 0xF);
}

int k0_2_D51(unsigned int a1)
{
  return a1 >> 24 == (unsigned char)(a1 >> 16)
      || !((unsigned char)(a1 >> 16) ^ BYTE1(a1))
      || !((unsigned char)a1 ^ BYTE1(a1));
}

int k0_prime_C15(unsigned int k0)
{
  int result; // eax@2
  unsigned int v2; // [sp+Ch] [bp-8h]@7
  unsigned int i; // [sp+10h] [bp-4h]@7

  if ( k0 == 2 )
  {
    result = 1;
  }
  else if ( k0 & 1 )
  {
    if ( k0 > 0xFFFFFE )
    {
      v2 = 3;
      for ( i = 9; i < k0; i = v2 * v2 )
      {
        if ( !(k0 % v2) )
          return 0;
        v2 += 2;
      }
      result = i != k0;
    }
    else
    {
      result = 0;
    }
  }
  else
  {
    result = 0;
  }
  return result;
}

int k4_17B5(unsigned int k4)
{
  unsigned short v2; // [sp+Ch] [bp-8h]@1
  unsigned short i; // [sp+Eh] [bp-6h]@1
  signed int idx; // [sp+10h] [bp-4h]@1

  v2 = 0;
  idx = 15;
  for ( i = 0; i <= 0xFu; ++i )
    v2 = (k4 >> (2 * idx-- + 1)) & 1 | 2 * v2;
  return v2 == 7 * (v2 / 7u);
}

int k8_1846(unsigned int k8)
{
  unsigned short v2; // [sp+Ch] [bp-8h]@1
  unsigned short i; // [sp+Eh] [bp-6h]@1
  signed int v4; // [sp+10h] [bp-4h]@1

  v2 = 0;
  v4 = 15;
  for ( i = 0; i <= 0xFu; ++i )
    v2 = (k8 >> (2 * v4-- + 1)) & 1 | 2 * v2;
  return v2 == 13 * ((unsigned short)(20165 * (unsigned int)v2 >> 16) >> 2);
}

int k0_k4_k8_2_18D2(unsigned int k4, unsigned int k8, int k0)
{
  int result; // eax@5
  int k0_; // [sp+4h] [bp-1Ch]@1
  unsigned short comp_k4; // [sp+10h] [bp-10h]@1
  unsigned short comp_k8; // [sp+12h] [bp-Eh]@1
  unsigned short i; // [sp+14h] [bp-Ch]@1
  unsigned short j; // [sp+14h] [bp-Ch]@6
  signed int v9; // [sp+18h] [bp-8h]@1
  signed int v10; // [sp+18h] [bp-8h]@6

  k0_ = k0;
  comp_k4 = 0;
  comp_k8 = 0;
  v9 = 15;
  for ( i = 0; i <= 0xFu; ++i )
    comp_k4 = (k4 >> 2 * v9--) & 1 | 2 * comp_k4;
  if ( num_1bit_1D20(comp_k4) > 2 )
  {
    v10 = 15;
    for ( j = 0; j <= 0xFu; ++j )
      comp_k8 = (k8 >> 2 * v10--) & 1 | 2 * comp_k8;
    if ( num_1bit_1D20(comp_k8) > 3 )
      result = k0_ + 1 == comp_k4 * comp_k8;
    else
      result = 0;
  }
  else
  {
    result = 0;
  }
  return result;
}

unsigned short k4_k8_1061(unsigned int k4, unsigned int k8)
{
  signed short v2; // ax@2
  signed short v3; // ax@5
  signed short v4; // ax@8
  signed short v5; // ax@11
  signed short v6; // ax@14
  signed short v7; // ax@17
  signed short v8; // ax@20
  signed short v9; // ax@23
  signed short v10; // ax@26
  signed short v11; // ax@29
  signed short v12; // ax@32
  signed short v13; // ax@35
  signed short v14; // ax@38
  signed short v15; // ax@41
  unsigned short v16; // ax@44
  short v18; // [sp+1Eh] [bp-2h]@1
  short v19; // [sp+1Eh] [bp-2h]@4
  short v20; // [sp+1Eh] [bp-2h]@7
  short v21; // [sp+1Eh] [bp-2h]@10
  short v22; // [sp+1Eh] [bp-2h]@13
  short v23; // [sp+1Eh] [bp-2h]@16
  short v24; // [sp+1Eh] [bp-2h]@19
  short v25; // [sp+1Eh] [bp-2h]@22
  short v26; // [sp+1Eh] [bp-2h]@25
  short v27; // [sp+1Eh] [bp-2h]@28
  short v28; // [sp+1Eh] [bp-2h]@31
  short v29; // [sp+1Eh] [bp-2h]@34
  short v30; // [sp+1Eh] [bp-2h]@37
  short v31; // [sp+1Eh] [bp-2h]@40
  short v32; // [sp+1Eh] [bp-2h]@43

  v18 = num_1bit_FCB(byte_2030B0[k4 >> 28]) == 2;
  if ( num_1bit_FCB(byte_2030B0[k4 >> 28] ^ byte_2030B0[(k4 >> 24) & 0xF]) == 2 )
    v2 = 2;
  else
    v2 = 0;
  v19 = v18 | v2;
  if ( num_1bit_FCB(byte_2030B0[(k4 >> 24) & 0xF] ^ byte_2030B0[(k4 >> 20) & 0xF]) == 2 )
    v3 = 4;
  else
    v3 = 0;
  v20 = v19 | v3;
  if ( num_1bit_FCB(byte_2030B0[(k4 >> 20) & 0xF] ^ byte_2030B0[(k4 >> 16) & 0xF]) == 2 )
    v4 = 8;
  else
    v4 = 0;
  v21 = v20 | v4;
  if ( num_1bit_FCB(byte_2030B0[(k4 >> 16) & 0xF] ^ byte_2030B0[(unsigned short)k4 >> 12]) == 2 )
    v5 = 16;
  else
    v5 = 0;
  v22 = v21 | v5;
  if ( num_1bit_FCB(byte_2030B0[(unsigned short)k4 >> 12] ^ byte_2030B0[(k4 >> 8) & 0xF]) == 2 )
    v6 = 32;
  else
    v6 = 0;
  v23 = v22 | v6;
  if ( num_1bit_FCB(byte_2030B0[(k4 >> 8) & 0xF] ^ byte_2030B0[(unsigned char)k4 >> 4]) == 2 )
    v7 = 64;
  else
    v7 = 0;
  v24 = v23 | v7;
  if ( num_1bit_FCB(byte_2030B0[(unsigned char)k4 >> 4] ^ byte_2030B0[k4 & 0xF]) == 2 )
    v8 = 128;
  else
    v8 = 0;
  v25 = v24 | v8;
  if ( num_1bit_FCB(byte_2030B0[k4 & 0xF] ^ byte_2030B0[k8 >> 28]) == 2 )
    v9 = 256;
  else
    v9 = 0;
  v26 = v25 | v9;
  if ( num_1bit_FCB(byte_2030B0[k8 >> 28] ^ byte_2030B0[(k8 >> 24) & 0xF]) == 2 )
    v10 = 512;
  else
    v10 = 0;
  v27 = v26 | v10;
  if ( num_1bit_FCB(byte_2030B0[(k8 >> 24) & 0xF] ^ byte_2030B0[(k8 >> 20) & 0xF]) == 2 )
    v11 = 1024;
  else
    v11 = 0;
  v28 = v27 | v11;
  if ( num_1bit_FCB(byte_2030B0[(k8 >> 20) & 0xF] ^ byte_2030B0[(k8 >> 16) & 0xF]) == 2 )
    v12 = 2048;
  else
    v12 = 0;
  v29 = v28 | v12;
  if ( num_1bit_FCB(byte_2030B0[(k8 >> 16) & 0xF] ^ byte_2030B0[(unsigned short)k8 >> 12]) == 2 )
    v13 = 4096;
  else
    v13 = 0;
  v30 = v29 | v13;
  if ( num_1bit_FCB(byte_2030B0[(unsigned short)k8 >> 12] ^ byte_2030B0[(k8 >> 8) & 0xF]) == 2 )
    v14 = 0x2000;
  else
    v14 = 0;
  v31 = v30 | v14;
  if ( num_1bit_FCB(byte_2030B0[(k8 >> 8) & 0xF] ^ byte_2030B0[(unsigned char)k8 >> 4]) == 2 )
    v15 = 0x4000;
  else
    v15 = 0;
  v32 = v31 | v15;
  if ( num_1bit_FCB(byte_2030B0[(unsigned char)k8 >> 4] ^ byte_2030B0[k8 & 0xF]) == 2 )
    v16 = 0x8000u;
  else
    v16 = 0;
  return v32 | v16;
}

unsigned char num_1bit_FCB(unsigned char a1)
{
  unsigned char result; // al@1
  __int64 v2; // rcx@1
  char v3; // [sp+10h] [bp-20h]@1
  char v4; // [sp+11h] [bp-1Fh]@1
  char v5; // [sp+12h] [bp-1Eh]@1
  char v6; // [sp+13h] [bp-1Dh]@1
  char v7; // [sp+14h] [bp-1Ch]@1
  char v8; // [sp+15h] [bp-1Bh]@1
  char v9; // [sp+16h] [bp-1Ah]@1
  char v10; // [sp+17h] [bp-19h]@1
  char v11; // [sp+18h] [bp-18h]@1
  char v12; // [sp+19h] [bp-17h]@1
  char v13; // [sp+1Ah] [bp-16h]@1
  char v14; // [sp+1Bh] [bp-15h]@1
  char v15; // [sp+1Ch] [bp-14h]@1
  char v16; // [sp+1Dh] [bp-13h]@1
  char v17; // [sp+1Eh] [bp-12h]@1
  char v18; // [sp+1Fh] [bp-11h]@1
  __int64 v19; // [sp+28h] [bp-8h]@1

  v3 = 0;
  v4 = 1;
  v5 = 1;
  v6 = 2;
  v7 = 1;
  v8 = 2;
  v9 = 2;
  v10 = 3;
  v11 = 1;
  v12 = 2;
  v13 = 2;
  v14 = 3;
  v15 = 2;
  v16 = 3;
  v17 = 3;
  v18 = 4;
  result = *(&v3 + (a1 >> 4)) + *(&v3 + (a1 & 0xF));
  return result;
}

int num_1bit_1D20(unsigned __int64 k16)
{
  int result; // eax@1
  int idx; // ecx@1
  unsigned __int64 v3; // rdx@2

  result = 0;
  idx = 0;
  do
  {
    v3 = k16 >> idx;
    idx += 8;
    result += byte_1FC0[(unsigned char)v3];
  }
  while ( idx != 64 );
  return result;
}