/*
 * SVS utility routines
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
 *
 * This file implements two essential functions:
 *
 * svs_load()   - Load memory from file.
 * svs_dump()   - Dump memory to file.
 */

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
uint64_t ieee_to_svs(double d)
{
    uint64_t word;
    int exponent;
    int sign;

    sign = d < 0;
    if (sign)
        d = -d;
    d = frexp(d, &exponent);
    // 0.5 <= d < 1.0
    d = ldexp(d, 40);
    word = (uint64_t)d;
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
    word |= ((uint64_t) (exponent + 64)) << 41;
    return word;
}

double svs_to_ieee(uint64_t word)
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
void svs_fprint_cmd(FILE *of, unsigned cmd)
{
    int reg, opcode, addr;

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
    fprintf(of, "%s", svs_opname(opcode));
    if (addr) {
        fprintf (of, " ");
        if (addr >= 077700)
            fprintf(of, "-%o", (addr ^ 077777) + 1);
        else
            fprintf(of, "%o", addr);
    }
    if (reg) {
        if (! addr)
            fprintf(of, " ");
        fprintf(of, "(%o)", reg);
    }
}

//
// Печать машинной инструкции в восьмеричном виде.
//
void svs_fprint_insn(FILE *of, unsigned insn)
{
    if (insn & BBIT(20))
        fprintf(of, "%02o %02o %05o ",
                 insn >> 20, (insn >> 15) & 037, insn & BITS(15));
    else
        fprintf(of, "%02o %03o %04o ",
                 insn >> 20, (insn >> 12) & 0177, insn & 07777);
}

//
// Чтение строки входного файла.
// Форматы строк:
// п 76543                     - адрес пуска
// в 12345                     - адрес ввода
// ч -123.45e+6                - вещественное число
// с 0123 4567 0123 4567       - восьмеричное слово
// к 00 22 00000 00 010 0000   - команды
//
static bool svs_read_line(FILE *input, int *type, uint64_t *val)
{
    char buf[512];
    const char *p;
    int i, c;
again:
    if (! fgets(buf, sizeof(buf), input)) {
        *type = 0;
        return true;
    }
    p = skip_spaces(buf);
    if (*p == '\n' || *p == ';')
        goto again;
    c = utf8_to_unicode(&p);
    if (c == CYRILLIC_SMALL_LETTER_VE ||
        c == CYRILLIC_CAPITAL_LETTER_VE ||
        c == 'b' || c == 'B') {
        // Адрес размещения данных.
        *type = ':';
        *val = strtol(p, 0, 8);
        return true;
    }
    if (c == CYRILLIC_SMALL_LETTER_PE ||
        c == CYRILLIC_CAPITAL_LETTER_PE ||
        c == 'p' || c == 'P') {
        // Стартовый адрес.
        *type = '@';
        *val = strtol(p, 0, 8);
        return true;
    }
    if (c == CYRILLIC_SMALL_LETTER_CHE ||
        c == CYRILLIC_CAPITAL_LETTER_CHE ||
        c == 'f' || c == 'F') {
        // Вещественное число.
        *type = '=';
        *val = ieee_to_svs(strtod(p, 0));
        return true;
    }
    if (c == CYRILLIC_SMALL_LETTER_ES ||
        c == CYRILLIC_CAPITAL_LETTER_ES ||
        c == 'c' || c == 'C') {
        // Восьмеричное слово.
        *type = '=';
        *val = 0;
        for (i=0; i<16; ++i) {
            p = skip_spaces(p);
            if (*p < '0' || *p > '7') {
                if (i == 0) {
                    // слишком короткое слово
                    goto bad;
                }
                break;
            }
            *val = *val << 3 | (*p++ - '0');
        }
        return true;
    }
    if (c == CYRILLIC_SMALL_LETTER_KA ||
        c == CYRILLIC_CAPITAL_LETTER_KA ||
        c == 'k' || c == 'K') {
        // Команда.
        *type = '*';
        if (!parse_instruction_word(p, val))
            goto bad;
        return true;
    }
    // Неверная строка входного файла
bad:
    printf("Invalid input line: %s", buf);
    return false;
}

//
// Load memory from file.
//
bool svs_load(struct ElSvsProcessor *cpu, FILE *input)
{
    int addr, type;
    uint64_t word;

    addr = 1;
    cpu->core.PC = 1;
    for (;;) {
        if (!svs_read_line(input, &type, &word))
            return false;

        switch (type) {
        case 0:                 // EOF
            return true;
        case ':':               // address
            addr = (int)word;
            break;
        case '=':               // word
        case '*':               // instruction
            if (addr < 010) {
                cpu->pult[addr] = word;
            } else {
                unsigned tag = (type == '*') ? TAG_INSN48 : TAG_NUMBER48;
                elMasterRamWordWrite(addr, tag, word << 16);
            }
            ++addr;
            break;
        case '@':               // start address
            cpu->core.PC = (unsigned)word;
            break;
        }
        if (addr > SVS_MEMSIZE)
            return false;
    }
    return true;
}

//
// Dump memory to file.
//
void svs_dump(struct ElSvsProcessor *cpu, FILE *of, const char *fnam)
{
    int addr, last_addr = -1;
    ElMasterWord word;
    ElMasterTag tag;

    fprintf(of, "; %s\n", fnam);
    for (addr=1; addr<SVS_MEMSIZE; ++addr) {
        if (addr < 010) {
            word = cpu->pult[addr];
            tag = TAG_INSN48;
        } else {
            elMasterRamWordRead(addr, &tag, &word);
            word >>= 16;
        }

        if (word == 0)
            continue;

        if (addr != last_addr+1) {
            fprintf(of, "\nв %05o\n", addr);
        }
        last_addr = addr;
        if (IS_INSN48(tag)) {
            fprintf(of, "к ");
            svs_fprint_cmd(of, (unsigned)(word >> 24));
            fprintf(of, ", ");
            svs_fprint_cmd(of, word & BITS(24));
            fprintf(of, "\t\t; %05o - ", addr);
            fprintf(of, "%04o %04o %04o %04o\n",
                     (int) (word >> 36) & 07777,
                     (int) (word >> 24) & 07777,
                     (int) (word >> 12) & 07777,
                     (int) word & 07777);
        } else {
            fprintf(of, "с %04o %04o %04o %04o",
                     (int) (word >> 36) & 07777,
                     (int) (word >> 24) & 07777,
                     (int) (word >> 12) & 07777,
                     (int) word & 07777);
            fprintf(of, "\t\t; %05o\n", addr);
        }
    }
}
