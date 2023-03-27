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
#include "processor.h"
#include "besm6_arch.h"

static Processor::AluReg toalu(Word val)
{
    Processor::AluReg ret;

    ret.exponent = (val >> 41) & BITS(7);
    ret.mantissa = val & BITS41;

    // Sign extend.
    ret.mantissa <<= 64 - 41;
    ret.mantissa >>= 64 - 41;
    return ret;
}

static inline int is_negative(const Processor::AluReg &word)
{
    return (word.mantissa & BIT41) != 0;
}

//
// Вернуть true если число ненормализованное.
// У нормализованного числа биты 42 и 41 совпадают.
//
static inline int is_denormal(const Processor::AluReg &val)
{
    return ((val.mantissa >> 40) ^ (val.mantissa >> 41)) & 1;
}

//
// Change sign of the mantissa.
// Note: the number may become denormalized.
//
static void negate(Processor::AluReg &val)
{
    val.mantissa = - val.mantissa;
}

//
// Normalize "to the right".
// Increment the exponent and update the mantissa.
//
static void normalize_to_the_right(Processor::AluReg &val)
{
    val.mantissa >>= 1;
    ++val.exponent;
}

//
// Сложение и все варианты вычитаний.
// Исходные значения: регистр ACC и аргумент 'val'.
// Результат помещается в регистр ACC и 40-1 разряды RMR.
//
void Processor::arith_add(Word val, int negate_acc, int negate_val)
{
    Word mr;
    AluReg acc, word, a1, a2;
    int diff, neg, rnd_rq = 0;

    acc = toalu(core.ACC);
    word = toalu(val);
    if (! negate_acc) {
        if (! negate_val) {
            // Сложение
        } else {
            // Вычитание
            negate(word);
        }
    } else {
        if (! negate_val) {
            // Обратное вычитание
            negate(acc);
        } else {
            // Вычитание модулей
            if (is_negative(acc))
                negate(acc);
            if (! is_negative(word))
                negate(word);
        }
    }

    diff = acc.exponent - word.exponent;
    if (diff < 0) {
        diff = -diff;
        a1 = acc;
        a2 = word;
    } else {
        a1 = word;
        a2 = acc;
    }
    mr = 0;
    neg = is_negative(a1);
    if (diff == 0) {
        // Nothing to do.
    } else if (diff <= 40) {
        rnd_rq = (mr = (a1.mantissa << (40 - diff)) & BITS40) != 0;
        a1.mantissa = ((a1.mantissa >> diff) |
                       (neg ? (~0ll << (40 - diff)) : 0)) & BITS42;
    } else if (diff <= 80) {
        diff -= 40;
        rnd_rq = a1.mantissa != 0;
        mr = ((a1.mantissa >> diff) |
              (neg ? (~0ll << (40 - diff)) : 0)) & BITS40;
        if (neg) {
            a1.mantissa = BITS42;
        } else
            a1.mantissa = 0;
    } else {
        rnd_rq = a1.mantissa != 0;
        if (neg) {
            mr = BITS40;
            a1.mantissa = BITS42;
        } else
            mr = a1.mantissa = 0;
    }
    acc.exponent = a2.exponent;
    acc.mantissa = a1.mantissa + a2.mantissa;

    // Если требуется нормализация вправо, биты 42:41
    // принимают значение 01 или 10.
    if (is_denormal(acc)) {
        rnd_rq |= acc.mantissa & 1;
        mr = (mr >> 1) | ((acc.mantissa & 1) << 39);
        normalize_to_the_right(acc);
    }
    arith_normalize_and_round(acc, mr, rnd_rq);
}

//
// Нормализация и округление.
// Результат помещается в регистры ACC и 40-1 разряды RMR.
// 48-41 разряды RMR сохраняются.
//
void Processor::arith_normalize_and_round(AluReg acc, Word mr, int rnd_rq)
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
            rr = mr >> (40 - cnt);
            acc.mantissa = r | rr;
            mr <<= cnt;
            acc.exponent -= cnt;
            goto chk_zero;
        }
        r = mr & BITS40;
        if (r) {
            int cnt = besm6_highest_bit(r) - 9;
            rr = mr;
            r <<= cnt;
            acc.mantissa = r;
            mr = 0;
            acc.exponent -= 40 + cnt;
            goto chk_zero;
        }
        goto zero;
    } else if (i == 3) {
        r = ~acc.mantissa & BITS40;
        if (r) {
            int cnt = besm6_highest_bit(r) - 9;
            r = (r << cnt) | ((1LL << cnt) - 1);
            rr = mr >> (40 - cnt);
            acc.mantissa = BIT41 | (~r & BITS40) | rr;
            mr <<= cnt;
            acc.exponent -= cnt;
            goto chk_zero;
        }
        r = ~mr & BITS40;
        if (r) {
            int cnt = besm6_highest_bit(r) - 9;
            rr = mr;
            r = (r << cnt) | ((1LL << cnt) - 1);
            acc.mantissa = BIT41 | (~r & BITS40);
            mr = 0;
            acc.exponent -= 40 + cnt;
            goto chk_zero;
        } else {
            rr = 1;
            acc.mantissa = BIT41;
            mr = 0;
            acc.exponent -= 80;
            goto chk_zero;
        }
    }
chk_zero:
    if (rr)
        rnd_rq = 0;

chk_rnd:
    if (acc.exponent & 0x8000)
        goto zero;

    if (! (core.RAU & RAU_ROUND_DISABLE) && rnd_rq) {
        acc.mantissa |= 1;
    }

    if (! acc.mantissa && ! (core.RAU & RAU_NORM_DISABLE)) {
zero:   core.ACC = 0;
        core.RMR &= ~BITS40;
        return;
    }

    core.ACC = (Word) (acc.exponent & BITS(7)) << 41 |
        (acc.mantissa & BITS41);
    core.RMR = (core.RMR & ~BITS40) | (mr & BITS40);

    // При переполнении мантисса и младшие разряды порядка верны
    if (acc.exponent & 0x80) {
        if (! (core.RAU & RAU_OVF_DISABLE))
            longjmp(exception, ESS_OVFL);
    }
}

//
// Изменение порядка числа на сумматоре ACC.
// Результат помещается в регистр ACC, RMR гасится.
//
void Processor::arith_add_exponent(int val)
{
    AluReg acc = toalu(core.ACC);

    acc.exponent += val;
    core.RMR = 0;
    arith_normalize_and_round(acc, 0, 0);
}

//
// Изменение знака числа на сумматоре ACC.
// Результат помещается в регистр ACC, RMR гасится.
//
void Processor::arith_change_sign(int negate_acc)
{
    AluReg acc = toalu(core.ACC);

    if (negate_acc) {
        negate(acc);
        if (is_denormal(acc)) {
            normalize_to_the_right(acc);
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
    if (! core.ACC || ! val) {
        // multiplication by zero is zero
        core.ACC = 0;
        core.RMR &= ~BITS40;
        return;
    }
    AluReg acc = toalu(core.ACC);
    AluReg word = toalu(val);
    Word mr;

    //
    // Multiply two signed 41-bit integers a and b, giving a 81-bit result.
    // Put upper 41 bits into signed *hi.
    // Put lower 40 bits into unsigned *lo.
    //
    __int128 result = (__int128) acc.mantissa * word.mantissa;
    acc.mantissa = (int64_t) (result >> 40);
    mr = (Word)result & BITS40;

    acc.exponent += word.exponent - 64;

    if (is_denormal(acc)) {
        normalize_to_the_right(acc);
    }
    arith_normalize_and_round(acc, mr, mr != 0);
}

//
// non-restoring division
//
static inline Processor::AluReg nrdiv(Processor::AluReg n, Processor::AluReg d)
{
    Processor::AluReg quot;

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
        normalize_to_the_right(n);
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
    AluReg acc;
    AluReg dividend, divisor;

    if (((val ^ (val << 1)) & BIT41) == 0) {
        // Ненормализованный делитель: деление на ноль.
        longjmp(exception, ESS_DIVZERO);
    }
    dividend = toalu(core.ACC);
    divisor = toalu(val);

    acc = nrdiv(dividend, divisor);

    arith_normalize_and_round(acc, 0, 0);
}

//
// Сдвиг сумматора ACC с выдвижением в регистр младших разрядов RMR.
// Величина сдвига находится в диапазоне -64..63.
//
void Processor::arith_shift(int i)
{
    core.RMR = 0;
    if (i > 0) {
        // Сдвиг вправо.
        if (i < 48) {
            core.RMR = (core.ACC << (48-i)) & BITS48;
            core.ACC >>= i;
        } else {
            core.RMR = core.ACC >> (i-48);
            core.ACC = 0;
        }
    } else if (i < 0) {
        // Сдвиг влево.
        i = -i;
        if (i < 48) {
            core.RMR = core.ACC >> (48-i);
            core.ACC = (core.ACC << i) & BITS48;
        } else {
            core.RMR = (core.ACC << (i-48)) & BITS48;
            core.ACC = 0;
        }
    }
}
