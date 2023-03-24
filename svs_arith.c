/*
 * SVS arithmetic instructions.
 *
 * Copyright (c) 2022 Leonid Broukhis, Serge Vakulenko
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#include "el_svs_api.h"
#include "el_svs_internal.h"
#include <math.h>
#include <stdlib.h>

typedef struct {
    int64_t mantissa;                   // Note: signed value
    unsigned exponent;                  // offset by 64
} alureg_t;                             // ALU register type

static alureg_t toalu(uint64_t val)
{
    alureg_t ret;

    ret.exponent = (val >> 41) & BITS(7);
    ret.mantissa = val & BITS41;

    // Sign extend.
    ret.mantissa <<= 64 - 41;
    ret.mantissa >>= 64 - 41;
    return ret;
}

static inline int is_negative(alureg_t *word)
{
    return (word->mantissa & BIT41) != 0;
}

//
// Change sign of the mantissa.
// Note: the number may become denormalized.
//
static void negate(alureg_t *val)
{
    val->mantissa = - val->mantissa;
}

//
// Вернуть true если число ненормализованное.
// У нормализованного числа биты 42 и 41 совпадают.
//
static inline int is_denormal(alureg_t *val)
{
    return ((val->mantissa >> 40) ^ (val->mantissa >> 41)) & 1;
}

//
// Normalize "to the right".
// Increment the exponent and update the mantissa.
//
static void normalize_to_the_right(alureg_t *val)
{
    val->mantissa >>= 1;
    ++val->exponent;
}

//
// 48-й разряд -> 1, 47-й -> 2 и т.п.
// Единица 1-го разряда и нулевое слово -> 48,
// как в первоначальном варианте системы команд.
//
int svs_highest_bit(uint64_t val)
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
// Нормализация и округление.
// Результат помещается в регистры ACC и 40-1 разряды RMR.
// 48-41 разряды RMR сохраняются.
//
static void normalize_and_round(struct ElSvsProcessor *cpu, alureg_t acc, uint64_t mr, int rnd_rq)
{
    uint64_t rr = 0;
    int i;
    uint64_t r;

    if (cpu->core.RAU & RAU_NORM_DISABLE)
        goto chk_rnd;
    i = (acc.mantissa >> 39) & 3;
    if (i == 0) {
        r = acc.mantissa & BITS40;
        if (r) {
            int cnt = svs_highest_bit(r) - 9;
            r <<= cnt;
            rr = mr >> (40 - cnt);
            acc.mantissa = r | rr;
            mr <<= cnt;
            acc.exponent -= cnt;
            goto chk_zero;
        }
        r = mr & BITS40;
        if (r) {
            int cnt = svs_highest_bit(r) - 9;
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
            int cnt = svs_highest_bit(r) - 9;
            r = (r << cnt) | ((1LL << cnt) - 1);
            rr = mr >> (40 - cnt);
            acc.mantissa = BIT41 | (~r & BITS40) | rr;
            mr <<= cnt;
            acc.exponent -= cnt;
            goto chk_zero;
        }
        r = ~mr & BITS40;
        if (r) {
            int cnt = svs_highest_bit(r) - 9;
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
    if (! (cpu->core.RAU & RAU_ROUND_DISABLE) && rnd_rq)
        acc.mantissa |= 1;

    if (! acc.mantissa && ! (cpu->core.RAU & RAU_NORM_DISABLE)) {
zero:   cpu->core.ACC = 0;
        cpu->core.RMR &= ~BITS40;
        return;
    }

    cpu->core.ACC = (uint64_t) (acc.exponent & BITS(7)) << 41 |
        (acc.mantissa & BITS41);
    cpu->core.RMR = (cpu->core.RMR & ~BITS40) | (mr & BITS40);
    // При переполнении мантисса и младшие разряды порядка верны
    if (acc.exponent & 0x80) {
        if (! (cpu->core.RAU & RAU_OVF_DISABLE))
            longjmp(cpu->exception, ESS_OVFL);
    }
}

//
// Сложение и все варианты вычитаний.
// Исходные значения: регистр ACC и аргумент 'val'.
// Результат помещается в регистр ACC и 40-1 разряды RMR.
//
void svs_add(struct ElSvsProcessor *cpu, uint64_t val, int negate_acc, int negate_val)
{
    uint64_t mr;
    alureg_t acc, word, a1, a2;
    int diff, neg, rnd_rq = 0;

    acc = toalu(cpu->core.ACC);
    word = toalu(val);
    if (! negate_acc) {
        if (! negate_val) {
            // Сложение
        } else {
            // Вычитание
            negate(&word);
        }
    } else {
        if (! negate_val) {
            // Обратное вычитание
            negate(&acc);
        } else {
            // Вычитание модулей
            if (is_negative(&acc))
                negate(&acc);
            if (! is_negative(&word))
                negate(&word);
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
    neg = is_negative(&a1);
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
    if (is_denormal(&acc)) {
        rnd_rq |= acc.mantissa & 1;
        mr = (mr >> 1) | ((acc.mantissa & 1) << 39);
        normalize_to_the_right(&acc);
    }
    normalize_and_round(cpu, acc, mr, rnd_rq);
}

//
// non-restoring division
//
static alureg_t nrdiv(alureg_t n, alureg_t d)
{
    alureg_t quot;

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
        normalize_to_the_right(&n);
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
void svs_divide(struct ElSvsProcessor *cpu, uint64_t val)
{
    alureg_t acc;
    alureg_t dividend, divisor;

    if (((val ^ (val << 1)) & BIT41) == 0) {
        // Ненормализованный делитель: деление на ноль.
        longjmp(cpu->exception, ESS_DIVZERO);
    }
    dividend = toalu(cpu->core.ACC);
    divisor = toalu(val);

    acc = nrdiv(dividend, divisor);

    normalize_and_round(cpu, acc, 0, 0);
}

//
// Multiply two signed 41-bit integers a and b, giving a 81-bit result.
// Put upper 41 bits into signed *hi.
// Put lower 40 bits into unsigned *lo.
//
void mul_i41_by_i41(int64_t a, int64_t b, int64_t *hi, uint64_t *lo)
{
    __int128 result = (__int128) a * b;
    *hi = (int64_t) (result >> 40);
    *lo = (uint64_t)result & BITS40;
}

//
// Умножение.
// Исходные значения: регистр ACC и аргумент 'val'.
// Результат помещается в регистр ACC и 40-1 разряды RMR.
//
void svs_multiply(struct ElSvsProcessor *cpu, uint64_t val)
{
    if (! cpu->core.ACC || ! val) {
        // multiplication by zero is zero
        cpu->core.ACC = 0;
        cpu->core.RMR &= ~BITS40;
        return;
    }
    alureg_t acc = toalu(cpu->core.ACC);
    alureg_t word = toalu(val);
    uint64_t mr;

    mul_i41_by_i41(acc.mantissa, word.mantissa, &acc.mantissa, &mr);
    acc.exponent += word.exponent - 64;

    if (is_denormal(&acc)) {
        normalize_to_the_right(&acc);
    }

    normalize_and_round(cpu, acc, mr, mr != 0);
}

//
// Изменение знака числа на сумматоре ACC.
// Результат помещается в регистр ACC, RMR гасится.
//
void svs_change_sign(struct ElSvsProcessor *cpu, int negate_acc)
{
    alureg_t acc;

    acc = toalu(cpu->core.ACC);
    if (negate_acc) {
        negate(&acc);
        if (is_denormal(&acc)) {
            normalize_to_the_right(&acc);
        }
    }
    cpu->core.RMR = 0;
    normalize_and_round(cpu, acc, 0, 0);
}

//
// Изменение порядка числа на сумматоре ACC.
// Результат помещается в регистр ACC, RMR гасится.
//
void svs_add_exponent(struct ElSvsProcessor *cpu, int val)
{
    alureg_t acc;

    acc = toalu(cpu->core.ACC);
    acc.exponent += val;
    cpu->core.RMR = 0;
    normalize_and_round(cpu, acc, 0, 0);
}

//
// Сборка значения по маске.
//
uint64_t svs_pack(uint64_t val, uint64_t mask)
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
uint64_t svs_unpack(uint64_t val, uint64_t mask)
{
    uint64_t result;
    int i;

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
int svs_count_ones(uint64_t word)
{
    int c;

    for (c=0; word; ++c)
        word &= word-1;
    return c;
}

//
// Сдвиг сумматора ACC с выдвижением в регистр младших разрядов RMR.
// Величина сдвига находится в диапазоне -64..63.
//
void svs_shift(struct ElSvsProcessor *cpu, int i)
{
    cpu->core.RMR = 0;
    if (i > 0) {
        // Сдвиг вправо.
        if (i < 48) {
            cpu->core.RMR = (cpu->core.ACC << (48-i)) & BITS48;
            cpu->core.ACC >>= i;
        } else {
            cpu->core.RMR = cpu->core.ACC >> (i-48);
            cpu->core.ACC = 0;
        }
    } else if (i < 0) {
        // Сдвиг влево.
        i = -i;
        if (i < 48) {
            cpu->core.RMR = cpu->core.ACC >> (48-i);
            cpu->core.ACC = (cpu->core.ACC << i) & BITS48;
        } else {
            cpu->core.RMR = (cpu->core.ACC << (i-48)) & BITS48;
            cpu->core.ACC = 0;
        }
    }
}
