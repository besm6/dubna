//
// BESM-6 arithmetics.
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
#include "processor.h"

//
// Сложение и все варианты вычитаний.
// Исходные значения: регистр ACC и аргумент 'val'.
// Результат помещается в регистр ACC и 40-1 разряды RMR.
//
void Processor::arith_add(Word val, bool negate_acc, bool negate_val)
{
    MantissaExponent acc(core.ACC);
    MantissaExponent word(val);

    if (!negate_acc) {
        if (!negate_val) {
            // Сложение
        } else {
            // Вычитание
            word.negate();
        }
    } else {
        if (!negate_val) {
            // Обратное вычитание
            acc.negate();
        } else {
            // Вычитание модулей
            if (acc.is_negative())
                acc.negate();
            if (!word.is_negative())
                word.negate();
        }
    }

    MantissaExponent a1, a2;
    int diff = acc.exponent - word.exponent;
    if (diff < 0) {
        diff = -diff;
        a1   = acc;
        a2   = word;
    } else {
        a1 = word;
        a2 = acc;
    }

    Word mr         = 0;
    bool neg        = a1.is_negative();
    bool round_flag = false;
    if (diff == 0) {
        // Nothing to do.
    } else if (diff <= 40) {
        mr          = (a1.mantissa << (40 - diff)) & BITS40;
        round_flag  = (mr != 0);
        a1.mantissa = ((a1.mantissa >> diff) | (neg ? (~0ull << (40 - diff)) : 0)) & BITS42;
    } else if (diff <= 80) {
        diff -= 40;
        round_flag = (a1.mantissa != 0);
        mr         = ((a1.mantissa >> diff) | (neg ? (~0ull << (40 - diff)) : 0)) & BITS40;
        if (neg) {
            a1.mantissa = BITS42;
        } else {
            a1.mantissa = 0;
        }
    } else {
        round_flag = (a1.mantissa != 0);
        if (neg) {
            mr          = BITS40;
            a1.mantissa = BITS42;
        } else {
            mr = a1.mantissa = 0;
        }
    }
    acc.exponent = a2.exponent;
    acc.mantissa = a1.mantissa + a2.mantissa;

    // Если требуется нормализация вправо, биты 42:41
    // принимают значение 01 или 10.
    if (acc.is_denormal()) {
        round_flag |= acc.mantissa & 1;
        mr = (mr >> 1) | ((acc.mantissa & 1) << 39);
        acc.normalize_to_the_right();
    }
    arith_normalize_and_round(acc, mr, round_flag);
}

//
// Нормализация и округление.
// Результат помещается в регистры ACC и 40-1 разряды RMR.
// 48-41 разряды RMR сохраняются.
//
void Processor::arith_normalize_and_round(MantissaExponent acc, Word mr, bool round_flag)
{
    Word rr = 0;
    int i;
    Word r;

    if (core.RAU & RAU_NORM_DISABLE)
        goto chk_rnd;

    i = (acc.mantissa >> 39) & 3;
    if (i == 0) {
        r = acc.mantissa & BITS40;
        if (r) {
            int cnt = besm6_highest_bit(r) - 9;
            r <<= cnt;
            rr           = mr >> (40 - cnt);
            acc.mantissa = r | rr;
            mr <<= cnt;
            acc.exponent -= cnt;
            goto chk_zero;
        }
        r = mr & BITS40;
        if (r) {
            int cnt = besm6_highest_bit(r) - 9;
            rr      = mr;
            r <<= cnt;
            acc.mantissa = r;
            mr           = 0;
            acc.exponent -= 40 + cnt;
            goto chk_zero;
        }
        goto zero;
    } else if (i == 3) {
        r = ~acc.mantissa & BITS40;
        if (r) {
            int cnt      = besm6_highest_bit(r) - 9;
            r            = (r << cnt) | ((1LL << cnt) - 1);
            rr           = mr >> (40 - cnt);
            acc.mantissa = BIT41 | (~r & BITS40) | rr;
            mr <<= cnt;
            acc.exponent -= cnt;
            goto chk_zero;
        }
        r = ~mr & BITS40;
        if (r) {
            int cnt      = besm6_highest_bit(r) - 9;
            rr           = mr;
            r            = (r << cnt) | ((1LL << cnt) - 1);
            acc.mantissa = BIT41 | (~r & BITS40);
            mr           = 0;
            acc.exponent -= 40 + cnt;
            goto chk_zero;
        } else {
            rr           = 1;
            acc.mantissa = BIT41;
            mr           = 0;
            acc.exponent -= 80;
            goto chk_zero;
        }
    }
chk_zero:
    if (rr)
        round_flag = false;

chk_rnd:
    if (acc.exponent & 0x8000)
        goto zero;

    if (!(core.RAU & RAU_ROUND_DISABLE) && round_flag) {
        acc.mantissa |= 1;
    }

    if (!acc.mantissa && !(core.RAU & RAU_NORM_DISABLE)) {
    zero:
        core.ACC = 0;
        core.RMR &= ~BITS40;
        return;
    }

    core.ACC = (Word)(acc.exponent & BITS(7)) << 41 | (acc.mantissa & BITS41);
    core.RMR = mr & BITS40;

    // При переполнении мантисса и младшие разряды порядка верны
    if (acc.exponent & 0x80) {
        if (!(core.RAU & RAU_OVF_DISABLE)) {
            throw Exception(MSG_ARITH_OVERFLOW);
        }
    }
}

//
// Изменение порядка числа на сумматоре ACC.
// Результат помещается в регистр ACC, RMR гасится.
//
void Processor::arith_add_exponent(int val)
{
    MantissaExponent acc(core.ACC);

    acc.exponent += val;
    core.RMR = 0;
    arith_normalize_and_round(acc, 0, 0);
}

//
// Изменение знака числа на сумматоре ACC.
// Результат помещается в регистр ACC, RMR гасится.
//
void Processor::arith_change_sign(bool negate_acc)
{
    MantissaExponent acc(core.ACC);

    if (negate_acc) {
        acc.negate();
        if (acc.is_denormal()) {
            acc.normalize_to_the_right();
        }
    }
    core.RMR = 0;
    arith_normalize_and_round(acc, 0, 0);
}

//
// Умножение.
// Исходные значения: регистр ACC и аргумент 'val'.
// Результат помещается в регистр ACC и 40-1 разряды RMR.
//
void Processor::arith_multiply(Word val)
{
    if (!core.ACC || !val) {
        // multiplication by zero is zero
        core.ACC = 0;
        core.RMR &= ~BITS40;
        return;
    }
    MantissaExponent acc(core.ACC);
    MantissaExponent word(val);

    Word mr = acc.multiply(word.mantissa);
    acc.exponent += word.exponent - 64;

    if (acc.is_denormal()) {
        acc.normalize_to_the_right();
    }
    arith_normalize_and_round(acc, mr, mr != 0);
}

//
// Multiply by a signed 41-bit integer.
// Store upper 41 bits (signed) into mantissa.
// Return lower 40 bits (unsigned).
//
uint64_t MantissaExponent::multiply(int64_t x)
{
    // Compute sign.
    unsigned negative = 0;
    if (mantissa < 0) {
        mantissa = -mantissa;
        negative ^= 1;
    }
    if (x < 0) {
        x = -x;
        negative ^= 1;
    }
    uint64_t a = x >> 20;
    uint64_t b = x & 0xfffff;

    uint64_t mr = mantissa * b;
    mantissa *= a;
    mr += (mantissa & 0xfffff) << 20;
    mantissa >>= 20;
    mantissa += mr >> 40;
    mr &= 0xfffff'fffff;

    // Negate.
    if (negative) {
        mantissa = ~mantissa;
        mr ^= 0xfffff'fffff;
        mr += 1;
        mantissa += mr >> 40;
        mr &= 0xfffff'fffff;
    }
    return mr;
}

//
// non-restoring division
//
static inline MantissaExponent nrdiv(MantissaExponent n, MantissaExponent d)
{
    MantissaExponent quot;

    if (d.mantissa == BIT40) {
        // Divide by a positive power of 2.
        quot.mantissa = n.mantissa;
        quot.exponent = n.exponent - d.exponent + 64 + 1;
        return quot;
    }

    // to compensate for potential normalization to the right
    n.mantissa <<= 1;
    d.mantissa <<= 1;

    if (llabs(n.mantissa) >= llabs(d.mantissa)) {
        n.normalize_to_the_right();
    }

    // Compute exponent.
    quot.exponent = n.exponent - d.exponent + 64;

    // Compute mantissa.
    quot.mantissa = 0;
    for (int64_t bitmask = BIT40; bitmask > 0; bitmask >>= 1) {
        if (n.mantissa == 0)
            break;

        if (llabs(n.mantissa) < BIT40) {
            // magic shortcut
            n.mantissa *= 2;

        } else if ((n.mantissa > 0) == (d.mantissa > 0)) {
            // Same sign
            quot.mantissa += bitmask;
            n.mantissa *= 2;
            n.mantissa -= d.mantissa;
        } else {
            // Different sign
            quot.mantissa -= bitmask;
            n.mantissa *= 2;
            n.mantissa += d.mantissa;
        }
    }
    return quot;
}

//
// Деление.
// Исходные значения: регистр ACC и аргумент 'val'.
// Результат помещается в регистр ACC, содержимое RMR не определено.
//
void Processor::arith_divide(Word val)
{
    if (((val ^ (val << 1)) & BIT41) == 0) {
        // Ненормализованный делитель: деление на ноль.
        throw Exception(MSG_ARITH_DIVZERO);
    }

    MantissaExponent dividend(core.ACC);
    MantissaExponent divisor(val);
    MantissaExponent acc = nrdiv(dividend, divisor);

    arith_normalize_and_round(acc, 0, 0);
}

//
// Сдвиг сумматора ACC с выдвижением в регистр младших разрядов RMR.
// Величина сдвига находится в диапазоне -64..63.
//
void Processor::arith_shift(int nbits)
{
    core.RMR = 0;
    if (nbits > 0) {
        // Сдвиг вправо.
        if (nbits < 48) {
            core.RMR = (core.ACC << (48 - nbits)) & BITS48;
            core.ACC >>= nbits;
        } else {
            core.RMR = core.ACC >> (nbits - 48);
            core.ACC = 0;
        }
    } else if (nbits < 0) {
        // Сдвиг влево.
        nbits = -nbits;
        if (nbits < 48) {
            core.RMR = core.ACC >> (48 - nbits);
            core.ACC = (core.ACC << nbits) & BITS48;
        } else {
            core.RMR = (core.ACC << (nbits - 48)) & BITS48;
            core.ACC = 0;
        }
    }
}
