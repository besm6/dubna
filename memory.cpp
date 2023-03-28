//
// BESM-6 memory unit.
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
#include "memory.h"

//
// Backdoor read from memory.
// No tracing.
//
void Memory::read_words(Words &output, unsigned nwords, unsigned addr)
{
    output.resize(nwords);
    memcpy(output.data(), &mem[addr], nwords * sizeof(Word));
}

//
// Backdoor write to memory.
// No tracing.
//
void Memory::write_words(const Words &input, unsigned addr)
{
    unsigned nwords = input.size();
    memcpy(&mem[addr], input.data(), nwords * sizeof(Word));
}

#if 0
//
// Чтение строки входного файла.
// Форматы строк:
// п 76543                     - адрес пуска
// в 12345                     - адрес ввода
// ч -123.45e+6                - вещественное число
// с 0123 4567 0123 4567       - восьмеричное слово
// к 00 22 00000 00 010 0000   - команды
//
static bool Memory::read_line(FILE *input, int *type, uint64_t *val)
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
        *val = ieee_to_besm6(strtod(p, 0));
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
bool Memory::load(struct ElSvsProcessor *cpu, FILE *input)
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
void Memory::dump(struct ElSvsProcessor *cpu, FILE *of, const char *fnam)
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
            besm6_print_instruction(of, (unsigned)(word >> 24));
            fprintf(of, ", ");
            besm6_print_instruction(of, word & BITS(24));
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
#endif
