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
// 48-bit memory word in lower bits of uint64_t value.
//
using Word = uint64_t;

//
// Array of words.
//
using Words = std::vector<Word>;

//
// Convert assembly source code into binary word.
//
Word besm6_asm(const char *source);

//
// Get instruction mnemonics by opcode, in range 000..077 or 0200..0370.
//
const char *besm6_opname(unsigned opcode);

//
// Get instruction opcode by mnemonics (UTF-8).
//
unsigned besm6_opcode(const char *opname);

//
// Find highest bit.
// Bit 48 returns 1, bit 47-й -> 2 and so on.
//
unsigned besm6_highest_bit(Word val);

//
// Pack bits by mask.
//
Word besm6_pack(Word val, Word mask);

//
// Unpack bits by mask.
//
Word besm6_unpack(Word val, Word mask);

//
// Count bits.
//
unsigned besm6_count_ones(Word word);

//
// Check whether instruction is extracode.
//
bool is_extracode(unsigned opcode);

//
// Convert float value between IEEE and BESM-6 formats.
//
Word ieee_to_besm6(double d);
double besm6_to_ieee(Word word);

//
// Print BESM-6 instruction.
//
void besm6_print_instruction_octal(std::ostream &out, unsigned cmd);
void besm6_print_instruction_mnemonics(std::ostream &out, unsigned cmd);

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
// Bits of ALU mode.
//
enum {
    RAU_NORM_DISABLE  = 001, // блокировка нормализации
    RAU_ROUND_DISABLE = 002, // блокировка округления
    RAU_LOG           = 004, // признак логической группы
    RAU_MULT          = 010, // признак группы умножения
    RAU_ADD           = 020, // признак группы сложения
    RAU_OVF_DISABLE   = 040, // блокировка переполнения
    RAU_MODE          = RAU_LOG | RAU_MULT | RAU_ADD,
};

#endif // BESM6_ARCH_H
