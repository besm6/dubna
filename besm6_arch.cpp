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

#include <cmath>
#include <iomanip>
#include <iostream>
#include <sstream>

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
    for (; mask; mask >>= 1, val >>= 1)
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
    for (i = 0; i < 48; ++i) {
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

    for (c = 0; word; ++c)
        word &= word - 1;
    return c;
}

//
// Check whether instruction is extracode.
//
bool is_extracode(unsigned opcode)
{
    switch (opcode) {
    case 050:
    case 051:
    case 052:
    case 053: // э50...э77
    case 054:
    case 055:
    case 056:
    case 057:
    case 060:
    case 061:
    case 062:
    case 063:
    case 064:
    case 065:
    case 066:
    case 067:
    case 070:
    case 071:
    case 072:
    case 073:
    case 074:
    case 075:
    case 076:
    case 077:
    case 0200: // э20
    case 0210: // э21
        return true;
    }
    return false;
}

//
// Преобразование вещественного числа в формат БЭСМ-6.
// Всегда возвращает нормализованное число.
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
Word ieee_to_besm6(const double input)
{
    // Split into mantissa and exponent.
    int exponent;
    double mantissa = frexp(input, &exponent);
    if (mantissa == 0.0) {
        return 0;
    }

    // Multiply mantissa by 2^40.
    mantissa = ldexp(mantissa, 40);

    Word word;
    if (mantissa > 0) {
        // Positive value in range [0.5, 1) * 2⁴⁰
        // Get 40 bits of mantissa.
        word = (Word)mantissa;
        if (mantissa - word >= 0.5) {
            // Rounding.
            word += 1;
            if (word == 1LL << 40) {
                // Normalize.
                word >>= 1;
                exponent += 1;
            }
        }
        if (exponent > 63) {
            // Overflow: return the most positive number.
            return 07757'7777'7777'7777LL;
        }
    } else {
        // Negative value in range (-1, -0.5] * 2⁴⁰.
        // Convert it to [-1, -0.5).
        if (mantissa == -(1LL << 39)) {
            if (exponent == -64) {
                // The smallest negative number.
                return 0027'7777'7777'7777LL;
            }
            mantissa += mantissa;
            exponent -= 1;
        }

        // Account for the sign bit.
        // The value becomes positive.
        mantissa += 1LL << 40;

        // Get 40 bits of mantissa.
        word = (Word)mantissa;
        if (mantissa - word > 0.5) {
            // Rounding.
            word += 1;
            if (word == 1LL << 40) {
                // Normalize.
                word >>= 1;
                exponent += 1;
            }
        }
        if (exponent > 63) {
            // Overflow: return the most negative number.
            return 07760'0000'0000'0000LL;
        }
        word |= 1LL << 40;
    }
    if (exponent < -64) {
        // Underflow.
        return 0LL;
    }

    // Add exponent.
    word |= ((Word)(exponent + 64)) << 41;
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
    mantissa = (double)(((int64_t)word) << (64 - 48 + 7));

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
    if (cmd & ONEBIT(20)) {
        opcode = (cmd >> 12) & 0370;
        addr   = cmd & BITS(15);
    } else {
        opcode = (cmd >> 12) & 077;
        addr   = cmd & 07777;
        if (cmd & ONEBIT(19))
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
        if (!addr)
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
    if (cmd & ONEBIT(20)) {
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
    out << std::setfill('0') << std::setw(4) << ((int)(value >> 36) & BITS(12)) << ' ';
    out << std::setfill('0') << std::setw(4) << ((int)(value >> 24) & BITS(12)) << ' ';
    out << std::setfill('0') << std::setw(4) << ((int)(value >> 12) & BITS(12)) << ' ';
    out << std::setfill('0') << std::setw(4) << ((int)value & BITS(12));

    // Restore.
    out.flags(save_flags);
}

//
// Print 48-bit value as bytes.
//
void besm6_print_word_bytes(std::ostream &out, Word value)
{
    auto save_flags = out.flags();

    out << std::oct;
    out << std::setfill('0') << std::setw(3) << ((int)(value >> 40) & BITS(8)) << ' ';
    out << std::setfill('0') << std::setw(3) << ((int)(value >> 32) & BITS(8)) << ' ';
    out << std::setfill('0') << std::setw(3) << ((int)(value >> 24) & BITS(8)) << ' ';
    out << std::setfill('0') << std::setw(3) << ((int)(value >> 16) & BITS(8)) << ' ';
    out << std::setfill('0') << std::setw(3) << ((int)(value >> 8) & BITS(8)) << ' ';
    out << std::setfill('0') << std::setw(3) << ((int)value & BITS(8));

    // Restore.
    out.flags(save_flags);
}

//
// Convert number to string as octal.
//
std::string to_octal(unsigned val)
{
    std::ostringstream buf;
    buf << std::oct << val;
    return buf.str();
}

//
// Encode string to COSY format.
//
std::string encode_cosy(std::string line)
{
    // Extend to 83 characters and append newline.
    line.append(83 - line.size(), ' ');
    line.append(1, '\n');

    // Pack spaces.
    unsigned num_spaces        = 0;
    unsigned first_space_index = 0;
    unsigned n;
    for (n = 0; n < line.size(); n++) {
        if (line[n] == ' ') {
            // Space.
            if (num_spaces == 0) {
                first_space_index = n;
            }
            num_spaces++;
        } else {
            // Non-space character.
            if (num_spaces > 0) {
                // Replace spaces with packed byte.
                line[first_space_index] = '\200' + num_spaces;
                if (num_spaces > 1) {
                    // Remove spaces.
                    line.erase(first_space_index + 1, num_spaces - 1);
                    n -= num_spaces - 1;
                }
                num_spaces        = 0;
                first_space_index = 0;
            }
        }
    }

    // Align to 6 bytes.
    switch (line.size() % 6) {
    case 1:
        line.append("\40\40\40\40\12");
        break;
    case 2:
        line.append("\40\40\40\12");
        break;
    case 3:
        line.append("\40\40\12");
        break;
    case 4:
        line.append("\40\12");
        break;
    case 5:
        line.append("\12");
        break;
    }
    return line;
}

//
// Square root function.
//
Word besm6_sqrt(Word input)
{
    const double arg    = besm6_to_ieee(input);
    const double result = std::sqrt(arg);
    if (std::isnan(result)) {
        throw std::runtime_error("Function sqrt() failed");
    }
    Word output = ieee_to_besm6(result);
    return output;
}

//
// Sine function.
//
Word besm6_sin(Word input)
{
    const double result = std::sin(besm6_to_ieee(input));
    if (std::isnan(result)) {
        throw std::runtime_error("Function sin() failed");
    }
    return ieee_to_besm6(result);
}

//
// Cosine function.
//
Word besm6_cos(Word input)
{
    const double result = std::cos(besm6_to_ieee(input));
    if (std::isnan(result)) {
        throw std::runtime_error("Function cos() failed");
    }
    return ieee_to_besm6(result);
}

//
// Arc tangent function.
//
Word besm6_arctan(Word input)
{
    const double result = std::atan(besm6_to_ieee(input));
    if (std::isnan(result)) {
        throw std::runtime_error("Function arctan() failed");
    }
    return ieee_to_besm6(result);
}

//
// Arc sine function.
//
Word besm6_arcsin(Word input)
{
    const double result = std::asin(besm6_to_ieee(input));
    if (std::isnan(result)) {
        throw std::runtime_error("Function arcsin() failed");
    }
    return ieee_to_besm6(result);
}

//
// Natural logarithm function.
//
Word besm6_log(Word input)
{
    const double result = std::log(besm6_to_ieee(input));
    if (std::isnan(result)) {
        throw std::runtime_error("Function log() failed");
    }
    return ieee_to_besm6(result);
}

//
// Exponential function with natural base 'e'.
//
Word besm6_exp(Word input)
{
    const double result = std::exp(besm6_to_ieee(input));
    if (std::isinf(result)) {
        throw std::runtime_error("Function exp() failed");
    }
    return ieee_to_besm6(result);
}

//
// Largest integral value not greater than argument.
//
Word besm6_floor(Word input)
{
    const double result = std::floor(besm6_to_ieee(input));
    if (std::isnan(result)) {
        throw std::runtime_error("Function floor() failed");
    }
    return ieee_to_besm6(result);
}
