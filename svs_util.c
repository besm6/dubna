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
#include "el_master_api.h"
#include "el_svs_api.h"
#include "el_svs_internal.h"
#include <math.h>
#include <string.h>
#include <stdlib.h>

static const char *opname_short_bemsh[64] = {
    "зп",  "зпм", "рег", "счм", "сл",  "вч",  "вчоб","вчаб",
    "сч",  "и",   "нтж", "слц", "знак","или", "дел", "умн",
    "сбр", "рзб", "чед", "нед", "слп", "вчп", "сд",  "рж",
    "счрж","счмр","зпп", "счп", "слпа","вчпа","сда", "ржа",
    "уи",  "уим", "счи", "счим","уии", "сли", "соп", "э47",
    "э50", "э51", "э52", "э53", "э54", "э55", "э56", "э57",
    "э60", "э61", "э62", "э63", "э64", "э65", "э66", "э67",
    "э70", "э71", "э72", "э73", "э74", "э75", "э76", "э77",
};

static const char *opname_long_bemsh[16] = {
    "э20", "э21", "мода","мод", "уиа", "слиа","по",  "пе",
    "пб",  "пв",  "выпр","стоп","пио", "пино","втбрз","цикл",
};

static const char *opname_short_madlen[64] = {
    "atx",  "stx",  "mod",  "xts",  "a+x",  "a-x",  "x-a",  "amx",
    "xta",  "aax",  "aex",  "arx",  "avx",  "aox",  "a/x",  "a*x",
    "apx",  "aux",  "acx",  "anx",  "e+x",  "e-x",  "asx",  "xtr",
    "rte",  "yta",  "*32",  "ext",  "e+n",  "e-n",  "asn",  "ntr",
    "ati",  "sti",  "ita",  "its",  "mtj",  "j+m",  "*46",  "*47",
    "*50",  "*51",  "*52",  "*53",  "*54",  "*55",  "*56",  "*57",
    "*60",  "*61",  "*62",  "*63",  "*64",  "*65",  "*66",  "*67",
    "*70",  "*71",  "*72",  "*73",  "*74",  "*75",  "*76",  "*77",
};

static const char *opname_long_madlen[16] = {
    "*20",  "*21",  "utc",  "wtc",  "vtm",  "utm",  "uza",  "u1a",
    "uj",   "vjm",  "ij",   "stop", "vzm",  "v1m",  "*36",  "vlm",
};

//
// Выдача мнемоники по коду инструкции.
// Код должен быть в диапазоне 000..077 или 0200..0370.
//
static const char *svs_opname(int opcode)
{
#if 0
    // Madlen mnemonics.
    if (opcode & 0200)
        return opname_long_madlen[(opcode >> 3) & 017];
    return opname_short_madlen[opcode];
#else
    // Bemsh mnemonics.
    if (opcode & 0200)
        return opname_long_bemsh[(opcode >> 3) & 017];
    return opname_short_bemsh[opcode];
#endif
}

//
// Выдача кода инструкции по мнемонике (UTF-8).
//
static int svs_opcode(char *instr)
{
    int i;

    for (i=0; i<64; ++i)
        if (strcmp(opname_short_bemsh[i], instr) == 0 ||
            strcmp(opname_short_madlen[i], instr) == 0)
            return i;
    for (i=0; i<16; ++i)
        if (strcmp(opname_long_bemsh[i], instr) == 0 ||
            strcmp(opname_long_madlen[i], instr) == 0)
            return (i << 3) | 0200;
    return -1;
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
// Пропуск пробелов.
//
static const char *skip_spaces(const char *p)
{
    for (;;) {
        if (*p == (char) 0xEF && p[1] == (char) 0xBB && p[2] == (char) 0xBF) {
            // Skip zero width no-break space.
            p += 3;
            continue;
        }
        if (*p == ' ' || *p == '\t' || *p == '\r') {
            ++p;
            continue;
        }
        if (*p == '#') {
            // Comment.
            return p + strlen(p);
        }
        return p;
    }
}

//
// Fetch Unicode symbol from UTF-8 string.
// Advance string pointer.
//
static int utf8_to_unicode(const char **p)
{
    int c1, c2, c3;

    c1 = (unsigned char) *(*p)++;
    if (! (c1 & 0x80))
        return c1;
    c2 = (unsigned char) *(*p)++;
    if (! (c1 & 0x20))
        return (c1 & 0x1f) << 6 | (c2 & 0x3f);
    c3 = (unsigned char) *(*p)++;
    return (c1 & 0x0f) << 12 | (c2 & 0x3f) << 6 | (c3 & 0x3f);
}

static char *svs_parse_octal(const char *cptr, int *offset)
{
    char *eptr;

    *offset = strtol(cptr, &eptr, 8);
    if (eptr == cptr)
        return 0;
    return eptr;
}

static const char *get_alnum(const char *iptr, char *optr)
{
    while ((*iptr >= 'a' && *iptr<='z') ||
           (*iptr >= 'A' && *iptr<='Z') ||
           (*iptr >= '0' && *iptr<='9') || (*iptr & 0x80)) {
        *optr++ = *iptr++;
    }
    *optr = 0;
    return iptr;
}

//
// Parse single instruction (half word).
// Allow mnemonics or octal code.
//
static const char *parse_instruction(const char *cptr, uint32_t *val)
{
    int opcode, reg, addr, negate;
    char buf[BUFSIZ];

    cptr = skip_spaces(cptr);                       // absorb spaces
    if (*cptr >= '0' && *cptr <= '7') {
        // Восьмеричное представление.
        cptr = svs_parse_octal(cptr, &reg);         // get register
        if (! cptr || reg > 15) {
            //printf("Bad register\n");
            return 0;
        }
        cptr = skip_spaces(cptr);                   // absorb spaces
        if (*cptr == '2' || *cptr == '3') {
            // Длинная команда.
            cptr = svs_parse_octal(cptr, &opcode);
            if (! cptr || opcode < 020 || opcode > 037) {
                //printf("Bad long opcode\n");
                return 0;
            }
            opcode <<= 3;
        } else {
            // Короткая команда.
            cptr = svs_parse_octal(cptr, &opcode);
            if (! cptr || opcode > 0177) {
                //printf("Bad short opcode\n");
                return 0;
            }
        }
        cptr = svs_parse_octal(cptr, &addr);        // get address
        if (! cptr || addr > BITS(15) ||
            (opcode <= 0177 && addr > BITS(12))) {
            //printf("Bad address\n");
            return 0;
        }
    } else {
        // Мнемоническое представление команды.
        cptr = get_alnum(cptr, buf);               // get opcode
        opcode = svs_opcode(buf);
        if (opcode < 0) {
            //printf("Bad opname: %s\n", buf);
            return 0;
        }
        negate = 0;
        cptr = skip_spaces(cptr);                   // absorb spaces
        if (*cptr == '-') {                         // negative offset
            negate = 1;
            cptr = skip_spaces(cptr + 1);           // absorb spaces
        }
        addr = 0;
        if (*cptr >= '0' && *cptr <= '7') {
            // Восьмеричный адрес.
            cptr = svs_parse_octal(cptr, &addr);
            if (! cptr || addr > BITS(15)) {
                //printf("Bad address: %o\n", addr);
                return 0;
            }
            if (negate)
                addr = (- addr) & BITS(15);
            if (opcode <= 077 && addr > BITS(12)) {
                if (addr < 070000) {
                    //printf("Bad short address: %o\n", addr);
                    return 0;
                }
                opcode |= 0100;
                addr &= BITS(12);
            }
        }
        reg = 0;
        cptr = skip_spaces(cptr);                   // absorb spaces
        if (*cptr == '(') {
            // Индекс-регистр в скобках.
            cptr = svs_parse_octal(cptr+1, &reg);
            if (! cptr || reg > 15) {
                //printf("Bad register: %o\n", reg);
                return 0;
            }
            cptr = skip_spaces(cptr);               // absorb spaces
            if (*cptr != ')') {
                //printf("No closing brace\n");
                return 0;
            }
            ++cptr;
        }
    }
    *val = reg << 20 | opcode << 12 | addr;
    return cptr;
}

//
// Instruction parse: two commands per word.
//
static bool parse_instruction_word(const char *cptr, uint64_t *val)
{
    uint32_t left, right;

    *val = 0;
    cptr = parse_instruction(cptr, &left);
    if (! cptr)
        return false;
    right = 0;
    cptr = skip_spaces(cptr);
    if (*cptr == ',') {
        cptr = parse_instruction(cptr + 1, &right);
        if (! cptr)
            return false;
    }
    cptr = skip_spaces(cptr);                       // absorb spaces
    if (*cptr != 0 && *cptr != ';' && *cptr != '\n' && *cptr != '\r') {
        //printf("Extra symbols at eoln: %s\n", cptr);
        return false;
    }
    *val = (uint64_t) left << 24 | right;
    return true;
}

//
// Печать машинной инструкции с мнемоникой.
//
void svs_fprint_cmd(FILE *of, uint32_t cmd)
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
void svs_fprint_insn(FILE *of, uint32_t insn)
{
    if (insn & BBIT(20))
        fprintf(of, "%02o %02o %05o ",
                 insn >> 20, (insn >> 15) & 037, insn & BITS(15));
    else
        fprintf(of, "%02o %03o %04o ",
                 insn >> 20, (insn >> 12) & 0177, insn & 07777);
}

//
// Convert assembly source code into binary word.
//
uint64_t ElSvsAsm(const char *src)
{
    uint64_t val;

    if (!parse_instruction_word(src, &val)) {
        printf("ElSvsAsm: bad source: %s\n", src);
        exit(1);
    }
    return val;
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
            cpu->core.PC = (uint32_t)word;
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
            svs_fprint_cmd(of, (uint32_t)(word >> 24));
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
