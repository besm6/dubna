//
// Punched card output for BESM-6.
//
// Copyright (c) 2024 Leonid Broukhis
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
#include <cerrno>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>

#include "encoding.h"
#include "machine.h"

//
// Close all data files.
//
void Puncher::finish()
{
    if (braille.is_open()) {
        braille.close();
    }
    if (stdarray.is_open()) {
        stdarray.close();
    }
    if (cosy.is_open()) {
        cosy.close();
    }
}

//
// Print 3 lines of 40 Braille Unicode characters per line
// representing one punched card.
// Reference: https://en.wikipedia.org/wiki/Braille_Patterns
//
void Puncher::punch_braille(const unsigned char buf[144])
{
    // All cards are output in Braille representation
    if (braille.bad())
        return;
    if (!braille.is_open()) {
        braille.open("punch.out");
        if (!braille.is_open()) {
            std::cerr << "punch.out:" << std::strerror(errno) << std::endl;
            return;
        }
    }
    unsigned char bytes[3][40];
    int line, col;
    memset(bytes, 0, 120);
    for (line = 0; line < 12; ++line) {
        for (col = 0; col < 80; ++col) {
            int idx = 1 + 12 * line + (col >= 40) + col / 8;
            int bit = (buf[idx] >> (7 - col % 8)) & 1;
            if (bit)
                bytes[line / 4][col / 2] |=
                    "\x01\x08\x02\x10\x04\x20\x40\x80"[line % 4 * 2 + col % 2];
        }
    }
    for (line = 0; line < 3; ++line) {
        for (col = 0; col < 40; ++col) {
            braille << '\342' << char(0240 + (bytes[line][col] >> 6))
                    << char(0200 + (bytes[line][col] & 077));
        }
        braille << '\n';
    }
    braille << '\n'; // separate cards by an empty line
}

//
// Transpose the card image into a column-based representation.
//
void Puncher::transpose(const unsigned char buf[144], uint16_t columns[80])
{
    memset(columns, 0, 80 * sizeof(uint16_t));
    for (int col = 0; col < 80; ++col) {
        for (int line = 0; line < 12; ++line) {
            int idx = 1 + 12 * line + (col >= 40) + col / 8;
            int bit = (buf[idx] >> (7 - col % 8)) & 1;
            if (bit)
                columns[col] |= 1 << line;
        }
    }
}

//
// Given that the image matches the "standard array" (object module) pattern,
// and is not a title card, output its contents in an octal format
// suitable for input.
//
void Puncher::punch_stdarray(const uint16_t columns[80])
{
    if (columns[1] == 0) {
        // Title card, no useful data
        return;
    }
    if (stdarray.bad())
        return;
    if (!stdarray.is_open()) {
        stdarray.open("stdarray.out");
        if (!stdarray.is_open()) {
            std::cerr << "stdarray.out:" << std::strerror(errno) << std::endl;
            return;
        }
    }

    // Using the binary header to write the module name and the card number
    stdarray << "`77761 ";
    for (int col = 76; col < 80; ++col) {
        utf8_putc(text_to_unicode(columns[col] >> 6), stdarray);
        utf8_putc(text_to_unicode(columns[col] & 077), stdarray);
    }
    stdarray << ' ' << columns[1] << '\n';

    for (int col = 4; col < 75; col += 9) {
        for (int i = 0; i < 8; ++i) {
            if (i % 4 == 0)
                stdarray << '`';
            stdarray << std::oct << std::setfill('0') << std::setw(4) << columns[col + i];
            if (i % 4 == 3)
                stdarray << '\n';
        }
    }
}

//
// Given that the image matches the COSY (compressed symbols) array pattern,
// output its title from the 0-th card and its contents in text format.
// This can be used for extracting text files from disk images by
// *EDIT
// *R:<nuzzzz>
// *CP:<arbitrary name>
// *EE
//
void Puncher::punch_cosy(const uint16_t columns[80])
{
    if (cosy.bad())
        return;
    if (!cosy.is_open()) {
        cosy.open("cosy.out");
        if (!cosy.is_open()) {
            std::cerr << "cosy.out:" << std::strerror(errno) << std::endl;
            return;
        }
    }

    if (columns[1] == 0) {
        // Title card, no text contents.
        cosy_string.erase();
        // Printing the COSY array name in curly braces before the first line of the array.
        cosy << "{";
        for (int col = 76; col < 80; ++col) {
            utf8_putc(text_to_unicode(columns[col] >> 6), cosy);
            utf8_putc(text_to_unicode(columns[col] & 077), cosy);
        }
        cosy << "}\n";
        return;
    }

    for (int col = 4; col < 75; col += 9) {
        Word w{};
        for (int i = 0; i < 8; ++i) {
            if (i % 4 == 0)
                w = 0;
            w = (w << 12) | columns[col + i];
            if (i % 4 == 3) {
                for (int j = 40; j >= 0; j -= 8) {
                    unsigned char c = (w >> j) & 0xFF;
                    if (c == '\n') {
                        cosy_string.resize(cosy_string.find_last_not_of(" ") + 1);
                        for (size_t p = 0; p < cosy_string.size(); ++p) {
                            utf8_putc(koi7_to_unicode[unsigned(cosy_string[p])], cosy);
                        }
                        cosy << '\n';
                        cosy_string.erase();
                        break; // word-align
                    } else if (c < 0200) {
                        cosy_string += c;
                    } else
                        cosy_string += std::string(c - 0200, ' ');
                }
            }
        }
    }
}

//
// Punch a range of memory words. The range must represent
// an integer number of cards. An image of one card takes 24 words.
//
void Puncher::punch(uint16_t start_addr, uint16_t end_addr)
{
    auto a = start_addr;
    while (a < end_addr) {
        BytePointer bp(memory, ADDR(a));
        unsigned char buf[144];
        for (int i = 0; i < 144; ++i)
            buf[i] = bp.get_byte();
        punch_braille(buf);
        uint16_t columns[80];
        transpose(buf, columns);
        switch (columns[0]) {
        case 01200:
            punch_stdarray(columns);
            break;
        case 05000:
            punch_cosy(columns);
            break;
        }
        a += 24;
    }
}
