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

#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>

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

//
// Bulk read from memory.
//
void Memory::read_words(Word output[], unsigned nwords, unsigned addr)
{
    memcpy(output, &mem[addr], nwords * sizeof(Word));
}

//
// Bulk write to memory.
//
void Memory::write_words(const Word input[], unsigned nwords, unsigned addr)
{
    memcpy(&mem[addr], input, nwords * sizeof(Word));
}

//
// Dump block of memory to file.
//
void Memory::dump(unsigned serial_num, unsigned disk_unit, unsigned zone, unsigned sector,
                  unsigned addr, unsigned nwords)
{
    // Create unique filename.
    std::ostringstream buf;
    buf << serial_num << "-disk" << std::oct << disk_unit << "-zone" << zone;
    if (nwords < 1024)
        buf << "-sector" << sector;
    buf << ".dump";

    // Open file.
    std::string filename = buf.str();
    std::ofstream out(filename);
    if (!out.is_open())
        throw std::runtime_error("Cannot create " + filename);

    out << "; " << filename << std::endl;
    for (; nwords > 0; nwords--, addr++) {
        auto word = mem[addr] & BITS48;
        if (word == 0)
            continue;

        out << "в " << std::oct << std::setfill('0') << std::setw(5) << addr << "  с ";
        besm6_print_word_octal(out, word);
        out << "  к ";
        besm6_print_instruction_mnemonics(out, (unsigned)(word >> 24));
        out << ", ";
        besm6_print_instruction_mnemonics(out, word & BITS(24));
        out << std::endl;
    }
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
#endif
