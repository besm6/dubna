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
// Деление мантисс так, как его выполняет АУ (ТО ИЫ1 700 000 ТО-3, ч. IV,
// п. 4.16, листы 86-102).  Деление без восстановления остатка, но остаток
// хранится в двухрядном коде (суммы и переносы), переносы не приводятся;
// приводятся только старшие разряды 41-39 (РСУД, РПУД, РУД).  По ним и по
// предыдущему действию (ЗУ+ЧВР) выбирается следующее действие: вычитание
// делителя, прибавление или только сдвиг (таблица 4.4).  Частное набирается
// двумя составляющими по 42 разряда (41 + доп. разряд): положительной (РОМ)
// и отрицательной (ВРУ); в конце частное = РОМ - ВРУ со специальным
// округлением (лист 102), при котором деление нацело всегда точно.
//
// Мантиссы - в 42-разрядном дополнительном коде (два знаковых разряда),
// делитель нормализован.  Результат: мантисса частного в той же сетке,
// |q| <= 2, нормализация - за вызывающим.  Эталонная модель:
// tools/b6div.py в проекте simh-besm6.
//
static inline int dbit(uint64_t x, int n)
{
    return (x >> (n - 1)) & 1;
}

static uint64_t au_divide(uint64_t x, uint64_t y)
{
    const uint64_t lo   = BITS40 >> 2; // двухрядная часть: 38..1
    const uint64_t lo39 = BITS40 >> 1; // разряды 39..1
    const int yneg      = dbit(y, 41);

    // Остаток R0 = X/2: старшие разряды 41-39 - на РСУД, младшие -
    // однорядным кодом на СМ.  s2/c2 - двухрядный код остатка, уже
    // сдвинутый влево (разряды 39-1), на первом такте это сам X.
    uint64_t s2  = x & lo39;
    uint64_t c2  = 0;
    int sud      = dbit(x, 42) << 2 | dbit(x, 41) << 1 | dbit(x, 40);
    int pud      = 0;
    int rud      = dbit(x, 41) << 1 | dbit(x, 40); // разряды 40,39 остатка
    bool zu_plus = !dbit(x, 41) && yneg;           // лист 100
    uint64_t pos = 0, neg = 0;

    for (int i = 0; i < 42; i++) {
        // Таблица 4.4: при первом знаке 0 действие повторяет предыдущее;
        // иначе знак остатка - по переносу из приведённых разрядов 40,39;
        // код I,II - знак неизвестен, только сдвиг.
        char act;
        if (!(sud & 4))
            act = zu_plus ? '+' : '-';
        else if ((sud & 3) + pud == 3)
            act = 's';
        else
            act = (((sud & 3) + pud >= 4) != yneg) ? '-' : '+';
        zu_plus = (act == '+');
        pos     = pos << 1 | (act == '-');
        neg     = neg << 1 | (act == '+');

        uint64_t lo_s, lo_c;
        if (act == 's') {
            // Только сдвиг: 41,40 РСУД = 1, разряд 39 двухрядного кода
            // складывается без делителя, перенос - в РПУД40 (лист 98).
            // Разряд 38 (перенос - в РПУД39, как в схеме листа 99) лист 98
            // не упоминает: он подобран по константе Е'Е-10' Диспака.
            int a = dbit(s2, 39);
            int b = dbit(c2, 39);
            sud   = 6 | (a ^ b);
            pud   = (a & b) << 1;
            a     = dbit(s2, 38);
            b     = dbit(c2, 38);
            pud |= a & b;
            lo_s = (s2 & (lo >> 1)) | (uint64_t)(a ^ b) << 37;
            lo_c = c2 & (lo >> 1);
        } else {
            // +ЧВР - прямой код делителя, -ЧВР - обратный и 1 в 1-й разряд
            uint64_t yv = (act == '+') ? y : ~y & BITS42;
            uint64_t sm = s2 ^ c2 ^ (yv & lo39);
            uint64_t cr = ((s2 & c2) | (s2 & yv & lo39) | (c2 & yv & lo39)) << 1;
            if (act == '-')
                cr |= 1;

            // Разряд 39 суммы - в РСУД, переносы из 39 и 38 - в РПУД;
            // разряды 41,40 - сдвинутый РУД плюс делитель.
            int t39 = rud & 1;
            int t40 = rud >> 1;
            sud     = (t40 ^ dbit(yv, 41) ^ (t39 & dbit(yv, 40))) << 2 | (t39 ^ dbit(yv, 40)) << 1 |
                      dbit(sm, 39);
            pud     = dbit(cr, 40) << 1 | dbit(cr, 39);
            lo_s    = sm & lo;
            lo_c    = cr & lo;
        }
        // РУД - приведённые разряды 40,39 нового остатка
        rud = ((sud & 3) + pud) & 3;
        s2  = lo_s << 1;
        c2  = lo_c << 1;
    }

    // Частное = РОМ + обратный код ВРУ + ДПР, плюс 1 округления в 1рРС2
    int pe = pos & 1;
    int ne = neg & 1;
    pos >>= 1;
    neg >>= 1;
    int p1  = pos & 1;
    int n1  = neg & 1;
    int dpr = pe || (!ne && !pe) || (!p1 && ne && !n1);
    int r1  = pe && (n1 || p1);
    return (pos + (~neg & BITS42) + dpr + r1) & BITS42;
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
    MantissaExponent acc;

    acc.mantissa = au_divide(dividend.mantissa & BITS42, divisor.mantissa & BITS42);
    acc.exponent = dividend.exponent - divisor.exponent + 64;

    // Sign extend.
    acc.mantissa <<= 64 - 42;
    acc.mantissa >>= 64 - 42;

    if (acc.is_denormal()) {
        // Нормализация вправо
        acc.normalize_to_the_right();
    }

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
