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

//
// 48-й разряд -> 1, 47-й -> 2 и т.п.
// Единица 1-го разряда и нулевое слово -> 48,
// как в первоначальном варианте системы команд.
//
unsigned besm6_highest_bit(uint64_t val)
{
    int n = 32, cnt = 0;
    do {
        uint64_t tmp = val;
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
uint64_t besm6_pack(uint64_t val, uint64_t mask)
{
    uint64_t result;

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
uint64_t besm6_unpack(uint64_t val, uint64_t mask)
{
    uint64_t result;
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
unsigned besm6_count_ones(uint64_t word)
{
    unsigned c;

    for (c=0; word; ++c)
        word &= word-1;
    return c;
}
