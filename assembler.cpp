//
// Trivial assembler.
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
#include <string>
#include <cstring>

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
const char *besm6_opname(unsigned opcode)
{
#if 1
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
// Get opcode by mnemonics (UTF-8).
// Return -1 when not found.
//
int besm6_opcode(const char *opname)
{
    unsigned i;

    for (i=0; i<64; ++i)
        if (strcmp(opname_short_bemsh[i], opname) == 0 ||
            strcmp(opname_short_madlen[i], opname) == 0)
            return i;

    for (i=0; i<16; ++i)
        if (strcmp(opname_long_bemsh[i], opname) == 0 ||
            strcmp(opname_long_madlen[i], opname) == 0)
            return (i << 3) | 0200;

    return -1;
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
// Get octal number.
//
static char *parse_octal(const char *cptr, unsigned &result)
{
    char *eptr;

    result = strtoul(cptr, &eptr, 8);
    if (eptr == cptr)
        return 0;
    return eptr;
}

//
// Get alphanumeric string.
// Also accept symbols * / + -.
//
static const char *get_alnum(const char *iptr, char *optr)
{
    while ((*iptr >= 'a' && *iptr<='z') ||
           (*iptr >= 'A' && *iptr<='Z') ||
           (*iptr == '*') || (*iptr == '/') || (*iptr == '+') || (*iptr == '-') ||
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
static const char *parse_instruction(const char *cptr, unsigned &result)
{
    unsigned opcode, reg, addr;

    cptr = skip_spaces(cptr);                       // absorb spaces
    if (*cptr >= '0' && *cptr <= '7') {
        // Восьмеричное представление.
        cptr = parse_octal(cptr, reg);              // get register
        if (! cptr || reg > 15) {
            //printf("Bad register\n");
            return 0;
        }
        cptr = skip_spaces(cptr);                   // absorb spaces
        if (*cptr == '2' || *cptr == '3') {
            // Длинная команда.
            cptr = parse_octal(cptr, opcode);
            if (! cptr || opcode < 020 || opcode > 037) {
                //printf("Bad long opcode\n");
                return 0;
            }
            opcode <<= 3;
        } else {
            // Короткая команда.
            cptr = parse_octal(cptr, opcode);
            if (! cptr || opcode > 0177) {
                //printf("Bad short opcode\n");
                return 0;
            }
        }
        cptr = parse_octal(cptr, addr);             // get address
        if (! cptr || addr > BITS(15) ||
            (opcode <= 0177 && addr > BITS(12))) {
            //printf("Bad address\n");
            return 0;
        }
    } else {
        // Мнемоническое представление команды.
        char buf[BUFSIZ];
        cptr = get_alnum(cptr, buf);               // get opcode
        opcode = besm6_opcode(buf);
        if (opcode < 0) {
            //printf("Bad opname: %s\n", buf);
            return 0;
        }
        int negate = 0;
        cptr = skip_spaces(cptr);                   // absorb spaces
        if (*cptr == '-') {                         // negative offset
            negate = 1;
            cptr = skip_spaces(cptr + 1);           // absorb spaces
        }
        addr = 0;
        if (*cptr >= '0' && *cptr <= '7') {
            // Восьмеричный адрес.
            cptr = parse_octal(cptr, addr);
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
            cptr = parse_octal(cptr+1, reg);
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
    result = reg << 20 | opcode << 12 | addr;
    return cptr;
}

//
// Convert assembly source code into binary word.
//
Word besm6_asm(const char *src)
{
    unsigned left = 0, right = 0;

    const char *cptr = parse_instruction(src, left);
    if (! cptr) {
        throw std::runtime_error("besm6_asm: bad left instruction: " + std::string(src));
    }

    cptr = skip_spaces(cptr);
    if (*cptr == ',') {
        cptr = parse_instruction(cptr + 1, right);
        if (! cptr) {
            throw std::runtime_error("besm6_asm: bad right instruction: " + std::string(src));
        }
    }

    cptr = skip_spaces(cptr);
    if (*cptr != 0 && *cptr != ';' && *cptr != '\n' && *cptr != '\r') {
        throw std::runtime_error("besm6_asm: bad extra symbols: " + std::string(src));
    }

    return (Word) left << 24 | right;
}
