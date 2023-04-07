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
#include "gost10859.h"

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

static bool is_eof(uint8_t byte)
{
    switch (byte) {
    case 0231:
    case GOST_EOF:
    case GOST_END_OF_INFORMATION:
        return true;
    default:
        return false;
    }
}

//
// Check whether '231' or GOST_EOF or GOST_END_OF_INFORMATION
// symbol is present in the current word.
//
bool BytePointer::eof_in_word()
{
    const Word w = *memory.get_ptr(word_addr);
    return is_eof(w) || is_eof(w >> 8) || is_eof(w >> 16) || is_eof(w >> 24) || is_eof(w >> 32) || is_eof(w >> 40);
}
