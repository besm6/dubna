//
// BESM-6 architecture details.
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

#include <cstdint>
#include <ostream>
#include <vector>

//
// Page, or zone, has 1024 words.
//
static const unsigned PAGE_NWORDS = 1024;

//
// Sector, or paragraph, has 256 words.
//
static const unsigned SECTOR_NWORDS = 256;

//
// Memory has 32 pages, or 32768 words.
//
static const unsigned MEMORY_NWORDS = 32 * PAGE_NWORDS;

//
// One zone in the disk image takes 8 extra words for OS info.
//
static const unsigned DISK_ZONE_NWORDS = 8 + 1024;

//
// Four first physical zones in the disk image are reserved by OS.
//
static const unsigned DISK_ZONE_OFFSET = 4;

//
// Total 32 disks on units 030-067.
// Total 32 drums on units 0-027, 070-077.
//
static const unsigned NDISKS = 32;
static const unsigned NDRUMS = 32;

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
bool besm6_opcode(const char *opname, unsigned &opcode);

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
// Print BESM-6 word.
//
void besm6_print_word_octal(std::ostream &out, Word cmd);

//
// Print BESM-6 instruction.
//
void besm6_print_instruction_octal(std::ostream &out, unsigned cmd);
void besm6_print_instruction_mnemonics(std::ostream &out, unsigned cmd);

//
// Convert numbers to strings.
//
std::string to_octal(unsigned val);

//
// Encode string to COSY format.
//
std::string encode_cosy(std::string line);

//
// Bits of memory word, from right to left, starting from 1.
//
#define ONEBIT(n)      (1ULL << (n - 1))             // один бит, от 1 до 64
#define BITS(n)        ((uint64_t)~0ULL >> (64 - n)) // маска битов n..1
#define ADDR(x)        ((x)&BITS(15))                // адрес слова
#define FIELD(x, n, w) (((x) >> (n - 1)) & BITS(w)) // поле шириной w бит, начиная с бита n

#define BIT40  0'0010'0000'0000'0000LL // 40-й бит - старший разряд мантиссы
#define BIT41  0'0020'0000'0000'0000LL // 41-й бит - знак
#define BIT42  0'0040'0000'0000'0000LL // 42-й бит - дубль-знак в мантиссе
#define BIT48  0'4000'0000'0000'0000LL // 48-й бит - знак порядка
#define BITS40 0'0017'7777'7777'7777LL // биты 40..1 - мантисса
#define BITS41 0'0037'7777'7777'7777LL // биты 41..1 - мантисса и знак
#define BITS42 0'0077'7777'7777'7777LL // биты 42..1 - мантисса и оба знака
#define BITS48 0'7777'7777'7777'7777LL // биты 48..1

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

//
// Floating point value as represented in ALU.
//
class MantissaExponent {
public:
    int64_t mantissa;  // Note: signed value
    unsigned exponent; // offset by 64

    //
    // Constructors.
    //
    MantissaExponent() : mantissa(0), exponent(0) {}

    explicit MantissaExponent(Word val)
    {
        exponent = (val >> 41) & BITS(7);
        mantissa = val & BITS41;

        // Sign extend.
        mantissa <<= 64 - 41;
        mantissa >>= 64 - 41;
    }

    //
    // Whether the number is negative.
    //
    bool is_negative() { return (mantissa & BIT41) != 0; }

    //
    // Вернуть true если число ненормализованное.
    // У нормализованного числа биты 42 и 41 совпадают.
    //
    bool is_denormal() { return ((mantissa >> 40) ^ (mantissa >> 41)) & 1; }

    //
    // Change sign of the mantissa.
    // Note: the number may become denormalized.
    //
    void negate() { mantissa = -mantissa; }

    //
    // Normalize "to the right".
    // Increment the exponent and update the mantissa.
    //
    void normalize_to_the_right()
    {
        mantissa >>= 1;
        ++exponent;
    }
};

#endif // BESM6_ARCH_H
