//
// Copyright (c) 2023 Serge Vakulenko
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
#ifndef BESM6_ARCH_H
#define BESM6_ARCH_H

#include <vector>

//
// Memory has 32768 words.
//
static const unsigned MEMORY_NWORDS = 32 * 1024;

//
// One row of 8-bit values (signed).
//
union Word {
    uint8_t byte[8]; // Only bytes 5:0 are used
    uint64_t word;   // Only bits 47:0 are used
};

//
// Array of words.
//
using Words = std::vector<Word>;

//
// Convert assembly source code into binary word.
//
Word besm6_asm(const char *source);

//
// Find highest bit.
// Bit 48 returns 1, bit 47-й -> 2 and so on.
//
unsigned besm6_highest_bit(uint64_t val);

//
// Pack bits by mask.
//
uint64_t besm6_pack(uint64_t val, uint64_t mask);

//
// Unpack bits by mask.
//
uint64_t besm6_unpack(uint64_t val, uint64_t mask);

//
// Count bits.
//
unsigned besm6_count_ones(uint64_t word);

//
// Bits of memory word, from right to left, starting from 1.
//
#define BBIT(n)         (1 << (n-1))            // один бит, от 1 до 32
#define BIT40           000010000000000000LL    // 40-й бит - старший разряд мантиссы
#define BIT41           000020000000000000LL    // 41-й бит - знак
#define BIT42           000040000000000000LL    // 42-й бит - дубль-знак в мантиссе
#define BIT48           004000000000000000LL    // 48-й бит - знак порядка
#define BIT49           010000000000000000LL    // бит 49
#define BITS(n)         (~0U >> (32-n))         // маска битов n..1
#define BITS40          00017777777777777LL     // биты 40..1 - мантисса
#define BITS41          00037777777777777LL     // биты 41..1 - мантисса и знак
#define BITS42          00077777777777777LL     // биты 42..1 - мантисса и оба знака
#define BITS48          07777777777777777LL     // биты 48..1
#define ADDR(x)         ((x) & BITS(15))        // адрес слова

//
// Разряды режима АУ.
//
#define RAU_NORM_DISABLE        001     // блокировка нормализации
#define RAU_ROUND_DISABLE       002     // блокировка округления
#define RAU_LOG                 004     // признак логической группы
#define RAU_MULT                010     // признак группы умножения
#define RAU_ADD                 020     // признак группы слодения
#define RAU_OVF_DISABLE         040     // блокировка переполнения

#define RAU_MODE                (RAU_LOG | RAU_MULT | RAU_ADD)
#define SET_MODE(x,m)           (((x) & ~RAU_MODE) | (m))
#define SET_LOGICAL(x)          (((x) & ~RAU_MODE) | RAU_LOG)
#define SET_MULTIPLICATIVE(x)   (((x) & ~RAU_MODE) | RAU_MULT)
#define SET_ADDITIVE(x)         (((x) & ~RAU_MODE) | RAU_ADD)
#define IS_LOGICAL(x)           (((x) & RAU_MODE) == RAU_LOG)
#define IS_MULTIPLICATIVE(x)    (((x) & (RAU_ADD | RAU_MULT)) == RAU_MULT)
#define IS_ADDITIVE(x)          ((x) & RAU_ADD)

#endif // BESM6_ARCH_H
