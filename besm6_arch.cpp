//
// BESM-6 architecture details.
//
// Copyright (c) 2022-2023 Leonid Broukhis, Serge Vakulenko
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
#include "besm6_arch.h"
#include <iomanip>
#include <cmath>

//
// 48-й разряд -> 1, 47-й -> 2 и т.п.
// Единица 1-го разряда и нулевое слово -> 48,
// как в первоначальном варианте системы команд.
//
unsigned besm6_highest_bit(Word val)
{
    int n = 32, cnt = 0;
    do {
        Word tmp = val;
        if (tmp >>= n) {
            cnt += n;
            val = tmp;
        }
    } while (n >>= 1);
    return 48 - cnt;
}

//
// Сборка значения по маске.
//
Word besm6_pack(Word val, Word mask)
{
    Word result;

    result = 0;
    for (; mask; mask>>=1, val>>=1)
        if (mask & 1) {
            result >>= 1;
            if (val & 1)
                result |= BIT48;
        }
    return result;
}

//
// Разборка значения по маске.
//
Word besm6_unpack(Word val, Word mask)
{
    Word result;
    unsigned i;

    result = 0;
    for (i=0; i<48; ++i) {
        result <<= 1;
        if (mask & BIT48) {
            if (val & BIT48)
                result |= 1;
            val <<= 1;
        }
        mask <<= 1;
    }
    return result;
}

//
// Подсчёт количества единиц в слове.
//
unsigned besm6_count_ones(Word word)
{
    unsigned c;

    for (c=0; word; ++c)
        word &= word-1;
    return c;
}

//
// Check whether instruction is extracode.
//
bool is_extracode(unsigned opcode)
{
    switch (opcode) {
    case 050: case 051: case 052: case 053: // э50...э77
    case 054: case 055: case 056: case 057:
    case 060: case 061: case 062: case 063:
    case 064: case 065: case 066: case 067:
    case 070: case 071: case 072: case 073:
    case 074: case 075: case 076: case 077:
    case 0200:                              // э20
    case 0210:                              // э21
        return true;
    }
    return false;
}

//
// Преобразование вещественного числа в формат БЭСМ-6.
//
// Представление чисел в IEEE 754 (double):
//      64   63———53 52————–1
//      знак порядок мантисса
// Старший (53-й) бит мантиссы не хранится и всегда равен 1.
//
// Представление чисел в БЭСМ-6:
//      48——–42 41   40————————————————–1
//      порядок знак мантисса в доп. коде
//
Word ieee_to_besm6(double d)
{
    Word word;
    int exponent;
    int sign;

    sign = d < 0;
    if (sign)
        d = -d;
    d = frexp(d, &exponent);
    // 0.5 <= d < 1.0
    d = ldexp(d, 40);
    word = (Word)d;
    if (d - word >= 0.5)
        word += 1;                      // Округление
    if (exponent < -64)
        return 0LL;                     // Близкое к нулю число
    if (exponent > 63) {
        return sign ?
            0xFEFFFFFFFFFFLL :          // Максимальное число
            0xFF0000000000LL;           // Минимальное число
    }
    if (sign)
        word = 0x20000000000LL-word;    // Знак
    word |= ((Word) (exponent + 64)) << 41;
    return word;
}

double besm6_to_ieee(Word word)
{
    double mantissa;
    int exponent;

    // Убираем свертку
    word &= BITS48;

    // Сдвигаем так, чтобы знак мантиссы пришелся на знак целого;
    // таким образом, mantissa равно исходной мантиссе, умноженной на 2**63.
    //
    mantissa = (double)(((int64_t) word) << (64 - 48 + 7));

    exponent = word >> 41;

    // Порядок смещен вверх на 64, и мантиссу нужно скорректировать
    return ldexp(mantissa, exponent - 64 - 63);
}

//
// Печать машинной инструкции с мнемоникой.
//
void besm6_print_instruction_mnemonics(std::ostream &out, unsigned cmd)
{
    auto save_flags = out.flags();
    unsigned reg, opcode, addr;

    reg = (cmd >> 20) & 017;
    if (cmd & BBIT(20)) {
        opcode = (cmd >> 12) & 0370;
        addr = cmd & BITS(15);
    } else {
        opcode = (cmd >> 12) & 077;
        addr = cmd & 07777;
        if (cmd & BBIT(19))
            addr |= 070000;
    }
    out << besm6_opname(opcode) << std::oct;
    if (addr) {
        out << ' ';
        if (addr >= 077700)
            out << '-' << ((addr ^ 077777) + 1);
        else
            out << addr;
    }
    if (reg) {
        if (! addr)
            out << ' ';
        out << '(' << reg << ')';
    }

    // Restore.
    out.flags(save_flags);
}

//
// Печать машинной инструкции в восьмеричном виде.
//
void besm6_print_instruction_octal(std::ostream &out, unsigned cmd)
{
    auto save_flags = out.flags();

    out << std::oct << std::setfill('0') << std::setw(2) << (cmd >> 20) << ' ';
    if (cmd & BBIT(20)) {
        out << std::setfill('0') << std::setw(2) << ((cmd >> 15) & 037) << ' ';
        out << std::setfill('0') << std::setw(5) << (cmd & BITS(15));
    } else {
        out << std::setfill('0') << std::setw(3) << ((cmd >> 12) & 0177) << ' ';
        out << std::setfill('0') << std::setw(4) << (cmd & BITS(12));
    }

    // Restore.
    out.flags(save_flags);
}

//
// Print 48-bit value as octal.
//
void besm6_print_word_octal(std::ostream &out, Word value)
{
    auto save_flags = out.flags();

    out << std::oct;
    out << std::setfill('0') << std::setw(4) << ((int) (value >> 36) & BITS(12)) << ' ';
    out << std::setfill('0') << std::setw(4) << ((int) (value >> 24) & BITS(12)) << ' ';
    out << std::setfill('0') << std::setw(4) << ((int) (value >> 12) & BITS(12)) << ' ';
    out << std::setfill('0') << std::setw(4) << ((int) value & BITS(12));

    // Restore.
    out.flags(save_flags);
}
