//
// Extracode 064 - text output.
//
// For details, see:
//   1) Pages 34-36 of book "Операционная система Диспак для БЭСМ-6"
//      https://github.com/besm6/besm6.github.io/raw/master/doc/%D0%9E%D0%BF%D0%B5%D1%80%D0%B0%D1%86%D0%B8%D0%BE%D0%BD%D0%BD%D0%B0%D1%8F-%D1%81%D0%B8%D1%81%D1%82%D0%B5%D0%BC%D0%B0-%D0%94%D0%98%D0%A1%D0%9F%D0%90%D0%9A.pdf
//   2) https://github.com/besm6/besm6.github.io/blob/master/wiki/extracodes.txt
//
// Copyright (c) 2023 Serge Vakulenko
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
#include <unistd.h>

#include <cstring>
#include <iomanip>
#include <iostream>

#include "encoding.h"
#include "gost10859.h"
#include "machine.h"

//
// Width of the printed line.
//
static const unsigned LINE_WIDTH = 128;

//
// Emit newline/newpage, then the line.
// Reset position to the beginning of the line.
// Clear overprint mode.
//
void Processor::e64_emit_line()
{
    // Emit line separator: newpage or several newlines or nothing.
    if (e64_skip_lines < 0) {
        // New page.
        if (isatty(1)) {
            // Output to a terminal: replace FormFeed by a NewLine.
            std::cout << std::endl;
        } else {
            std::cout << '\f' << std::flush;
        }
    } else {
        // Zero or more newlines.
        for (; e64_skip_lines > 0; e64_skip_lines--) {
            std::cout << std::endl;
        }
    }
    e64_flush_line();

    // Set separator and position for next line.
    e64_skip_lines = 1;
    e64_position = 0;
    e64_overprint = false;
}

//
// If the line is non-blank - print it to stdout.
// Erase it (fill with spaces).
// Don't change position.
//
void Processor::e64_flush_line()
{
    // Find the last symbol to print.
    int limit = e64_line.size();
    for (;;) {
        limit--;
        if (limit < 0) {
            // Nothing to print.
            return;
        }

        if (e64_line[limit] != GOST_SPACE) {
            // Write the line to the output.
            gost_write(e64_line, limit);
            e64_line_count++;

            // Erase the line: fill with spaces.
            std::fill(e64_line.begin(), e64_line.end(), GOST_SPACE);
            return;
        }
    }
}

//
// Emit the final newline.
//
void Processor::e64_finish()
{
    if (e64_line_count > 0) {
        std::cout << std::endl;
        e64_line_count = 0;
    }
}

//
// Print one character at a given position.
// Update the position.
//
void Processor::e64_putchar(int ch)
{
    if (e64_position >= LINE_WIDTH) {
        // Line is full.
        e64_emit_line();
    }
    if (e64_overprint && e64_line[e64_position] != GOST_SPACE) {
        // In overprint mode: cannot overwrite previous character.
        // Emit the line with overprint indicator (backslash).
        e64_flush_line();
        std::cout << '\\' << std::endl;
    }
    e64_line[e64_position] = ch;
    e64_position += 1;
}

//
// Print machine instruction at a given position.
// Update the position.
//
void Processor::e64_print_cmd(unsigned cmd)
{
    e64_putchar(cmd >> 23 & 1);
    e64_putchar(cmd >> 20 & 7);
    e64_putchar(GOST_SPACE);
    if (cmd & 02000000) {
        // long address command
        e64_putchar(cmd >> 18 & 3);
        e64_putchar(cmd >> 15 & 7);
        e64_putchar(GOST_SPACE);
        e64_putchar(cmd >> 12 & 7);
    } else {
        // short address command
        e64_putchar(cmd >> 18 & 1);
        e64_putchar(cmd >> 15 & 7);
        e64_putchar(cmd >> 12 & 7);
        e64_putchar(GOST_SPACE);
    }
    e64_putchar(cmd >> 9 & 7);
    e64_putchar(cmd >> 6 & 7);
    e64_putchar(cmd >> 3 & 7);
    e64_putchar(cmd & 7);
}

//
// Extract decimal exponent from the real value.
// Return value in range 0.1 - 0.9(9).
// Input value must be nonzero positive.
//
static double real_exponent(double value, int &exponent)
{
    exponent = 0;
    if (value <= 0)
        return 0; // cannot happen

    while (value >= 1000000) {
        exponent += 6;
        value /= 1000000;
    }
    while (value >= 1) {
        ++exponent;
        value /= 10;
    }
    while (value < 0.0000001) {
        exponent -= 6;
        value *= 1000000;
    }
    while (value < 0.1) {
        --exponent;
        value *= 10;
    }
    return value;
}

//
// Print string in ITM format.
// Return next data address.
//
unsigned Processor::e64_print_itm(unsigned addr0, unsigned addr1)
{
    BytePointer bp(memory, addr0);
    uint8_t last_ch = GOST_SPACE;

    while (bp.word_addr) {
        // No data to print.
        if (addr1 && bp.word_addr == addr1 + 1) {
            return bp.word_addr;
        }

        // No space left on the line.
        if (e64_position == LINE_WIDTH) {
            if (!addr1) {
                if (bp.byte_index) {
                    ++bp.word_addr;
                }
                return bp.word_addr;
            }
            e64_emit_line();
        }

        uint8_t ch = bp.get_byte();
        switch (ch) {
        case 0140: // end of information
            if (bp.byte_index) {
                ++bp.word_addr;
            }
            return bp.word_addr;

        case 040: // blank
            e64_putchar(GOST_SPACE);
            break;

        case 0173: // repeat last symbol
            ch = bp.get_byte();
            if (ch == 040) {
                // fill line by last symbol (?)
                std::fill(e64_line.begin(), e64_line.end(), last_ch);
                e64_emit_line();
            } else {
                while (ch-- & 017) {
                    e64_putchar(last_ch);
                }
            }
            break;

        default:
            last_ch = itm_to_gost[ch];
            e64_putchar(last_ch);
            break;
        }
    }
    return 0;
}

//
// Print word(s) in octal format.
// Return next data address.
//
unsigned Processor::e64_print_octal(unsigned addr0, unsigned addr1, unsigned digits, unsigned width,
                                    unsigned repeat)
{
    if (digits > 16) {
        digits = 16;
    }
    while (addr0) {
        // No data to print.
        if (addr1 && addr0 == addr1 + 1) {
            return addr0;
        }

        // No space left on the line.
        if (e64_position >= LINE_WIDTH) {
            if (!addr1) {
                return 0;
            }
            return addr0;
        }
        Word word = machine.mem_load(addr0);
        ++addr0;

        word <<= 64 - digits * 3;
        for (unsigned i = 0; i < digits; ++i) {
            e64_putchar((word >> 61) & 7);
            word <<= 3;
        }

        if (!repeat) {
            return addr0;
        }
        --repeat;
        if (width > digits) {
            e64_position += width - digits;
        }
    }
    return 0;
}

//
// Print word(s) in hexadecimal format.
// Return next data address.
//
unsigned Processor::e64_print_hex(unsigned addr0, unsigned addr1, unsigned digits, unsigned width,
                                  unsigned repeat)
{
    static const char hex_digit[16] = {
        GOST_0, GOST_1, GOST_2, GOST_3, GOST_4, GOST_5, GOST_6, GOST_7,
        GOST_8, GOST_9, GOST_A, GOST_B, GOST_C, GOST_D, GOST_E, GOST_F,
    };

    if (digits > 12) {
        digits = 12;
    }
    while (addr0) {
        // No data to print.
        if (addr1 && addr0 == addr1 + 1) {
            return addr0;
        }

        // No space left on the line.
        if (e64_position >= LINE_WIDTH) {
            if (!addr1) {
                return 0;
            }
            return addr0;
        }
        Word word = machine.mem_load(addr0);
        ++addr0;

        word <<= 64 - digits * 4;
        for (unsigned i = 0; i < digits; ++i) {
            char ch = hex_digit[(word >> 60) & 15];
            e64_putchar(ch);
            word <<= 4;
        }

        if (!repeat) {
            return addr0;
        }
        --repeat;
        if (width > digits) {
            e64_position += width - digits;
        }
    }
    return 0;
}

//
// Print CPU instruction(s).
// Return next data address.
//
unsigned Processor::e64_print_instructions(unsigned addr0, unsigned addr1, unsigned width,
                                           unsigned repeat)
{
    while (addr0) {
        // No data to print.
        if (addr1 && addr0 == addr1 + 1) {
            return addr0;
        }

        // No space left on the line.
        if (e64_position >= LINE_WIDTH) {
            if (!addr1) {
                return 0;
            }
            return addr0;
        }
        Word word  = machine.mem_load(addr0);
        unsigned a = word >> 24;
        unsigned b = word & BITS(24);
        ++addr0;

        e64_print_cmd(a);
        e64_putchar(GOST_SPACE);
        e64_print_cmd(b);

        if (!repeat) {
            return addr0;
        }
        --repeat;
        if (width > 23) {
            e64_position += width - 23;
        }
    }
    return 0;
}

//
// Print real number(s).
// Return next data address.
//
unsigned Processor::e64_print_real(unsigned addr0, unsigned addr1, unsigned digits, unsigned width,
                                   unsigned repeat)
{
    if (digits > 20) {
        digits = 20;
    }
    if (digits < 4) {
        digits = 4;
    }
    while (addr0) {
        // No data to print.
        if (addr1 && addr0 == addr1 + 1) {
            return addr0;
        }

        // No space left on the line.
        if (e64_position >= LINE_WIDTH) {
            if (!addr1) {
                return 0;
            }
            return addr0;
        }

        Word word     = machine.mem_load(addr0);
        bool negative = (word & BIT41);
        double value  = 0;
        int exponent  = 0;
        if (word & ~BIT41) {
            // Value is non-zero.
            value = besm6_to_ieee(word);
            if (value < 0) {
                value = -value;
            }
            value = real_exponent(value, exponent);
        }
        ++addr0;

        e64_putchar(GOST_SPACE);
        e64_putchar(negative ? GOST_MINUS : GOST_PLUS);

        for (unsigned i = 0; i < digits - 4; ++i) {
            value     = value * 10;
            int digit = (int)value;
            e64_putchar(digit);
            value -= digit;
        }
        e64_putchar(GOST_LOWER_TEN);
        if (exponent >= 0) {
            e64_putchar(GOST_PLUS);
        } else {
            e64_putchar(GOST_MINUS);
            exponent = -exponent;
        }
        e64_putchar(exponent / 10);
        e64_putchar(exponent % 10);

        if (!repeat) {
            return addr0;
        }
        --repeat;
        if (width > digits + 2) {
            e64_position += width - digits - 2;
        }
    }
    return 0;
}

//
// Print string in GOST format.
// Return next data address.
//
unsigned Processor::e64_print_gost(unsigned addr0, unsigned addr1)
{
    BytePointer bp(memory, addr0);
    unsigned char last_ch = GOST_SPACE;

    for (;;) {
        if (bp.word_addr == 0)
            return 0;

        // No data to print.
        if (addr1 && bp.word_addr == addr1 + 1)
            return bp.word_addr;

        unsigned char ch = bp.get_byte();
        if (is_gost_end_of_text(ch)) {
            if (bp.byte_index != 0)
                ++bp.word_addr;
            return bp.word_addr;
        }

        // Weirdness of e64 in Dispak: when '231' or other EOF
        // is present in the current word - byte #129 is ignored.
        if (e64_position == LINE_WIDTH) {
            e64_emit_line();
            if (bp.eof_in_word()) {
                continue;
            }
        }

        switch (ch) {
        case 0201:
            // New page.
            e64_putchar(GOST_SPACE);
            e64_skip_lines = -1;
            break;

        case GOST_CARRIAGE_RETURN:
        case GOST_NEWLINE:
            // New line.
            e64_emit_line();
            break;

        case 0143:
        case 0341:
            // Null width symbol.
            break;

        case GOST_SET_POSITION:
        case 0200:
            // Set position, defined by next byte.
            ch           = bp.get_byte();
            e64_position = ch % LINE_WIDTH;
            break;

        case 0174:
        case 0265:
            // Repeat last symbol as many times, as defined by next byte.
            ch = bp.get_byte();
            if (ch == 040) {
                // fill line by last symbol (?)
                std::fill(e64_line.begin(), e64_line.end(), last_ch);
                e64_emit_line();
            } else {
                while ((ch-- & 017) != 0 && e64_position < LINE_WIDTH) {
                    if (e64_line[e64_position] == GOST_SPACE) {
                        e64_putchar(last_ch);
                    } else {
                        ++e64_position;
                    }
                }
            }
            break;

        case GOST_SPACE2: // space
        case 0242:        // used as space by forex
            ch = GOST_SPACE;
            // fall through...
        default:
            // Printable character.
            last_ch = ch;
            e64_putchar(ch);
            break;
        }
    }
}

//
// Extracode 064: text output.
//
// The information array has the following format
// - First word:
//   iiii ..... xxxxxxxxxxxxxxx
//   jjjj ..... yyyyyyyyyyyyyyy
// - Other words:
//   ffff bbbbbbbb dddddddddddd
//   esss wwwwwwww rrrrrrrrrrrr
//
// Here:
// x+Ri	- start address of data
// y+Rj	- end address of data
// f	- print format
// b	- starting position, 0 - most left
// d 	- number of digits (for integer formats)
// e	- 1 for final word
// s	- skip this number of lines
// w	- total field width (for integer formats)
// r	- repetition counter (0-once, 1-twice etc)
//
// Print formats:
// 0 	- text in GOST encoding
// 1	- CPU instruction
// 2	- octal number
// 3	- real number (mantissa=digits-4)
// 4	- text in ITM encoding
//
void Processor::e64()
{
    switch (core.M[016]) {
    case 0:  // Disable paging.
        return;
    case 1:  // Enable paging.
        return;
    default: // Print something.
        break;
    }

    // Get start and end addresses from control word #0.
    unsigned ctl_addr = core.M[016];
    E64_Pointer ptr;
    ptr.word = machine.mem_load(ctl_addr);

    unsigned start_addr = ADDR(ptr.field.start_addr + core.M[ptr.field.start_reg]);
    unsigned end_addr   = ADDR(ptr.field.end_addr + core.M[ptr.field.end_reg]);
    if (end_addr <= start_addr) {
        // No limit.
        end_addr = 0;
    }
    if (start_addr == 0) {
        throw Processor::Exception("Bad start_addr in extracode e64");
    }

    // Allocate a line buffer, initialize with spaces.
    e64_line.resize(LINE_WIDTH, GOST_SPACE);

    // Execute every format word in order.
    for (;;) {
        // Get next control word.
        ctl_addr++;
        if (ctl_addr >= MEMORY_NWORDS)
            throw Processor::Exception("Unterminated info list in extracode e64");

        // Get format details from control word #1 onwards.
        E64_Info ctl;
        ctl.word = machine.mem_load(ctl_addr);
        machine.trace_e64(ctl, start_addr, end_addr);

        // Start at given position.
        e64_position = ctl.field.offset;

        switch (ctl.field.format) {
        case 0:
        case 8:
            // Text in GOST encoding.
            start_addr = e64_print_gost(start_addr, end_addr);
            break;

        case 1:
        case 5:
        case 9:
        case 13:
            // CPU instruction.
            start_addr = e64_print_instructions(start_addr, end_addr, ctl.field.width,
                                                ctl.field.repeat1);
            break;

        case 2:
        case 10:
            // Octal number.
            start_addr = e64_print_octal(start_addr, end_addr, ctl.field.digits, ctl.field.width,
                                         ctl.field.repeat1);
            break;

        case 3:
        case 11:
            // Real number.
            start_addr = e64_print_real(start_addr, end_addr, ctl.field.digits, ctl.field.width,
                                        ctl.field.repeat1);
            break;

        case 4:
        case 12:
            // Text in ITM encoding.
            start_addr = e64_print_itm(start_addr, end_addr);
            break;

        case 6:
        case 7:
        case 14:
        case 15:
            // Hexadecimal number.
            start_addr = e64_print_hex(start_addr, end_addr, ctl.field.digits, ctl.field.width,
                                       ctl.field.repeat1);
            break;
        }

        if (ctl.field.finish) {
            if (e64_position != 0 /*&& e64_position != LINE_WIDTH*/) {
                e64_emit_line();
            }

            if (end_addr && start_addr <= end_addr) {
                // Repeat printing task until all data expired.
                continue;
            }

            if (ctl.field.skip != 0) {
                e64_skip_lines = ctl.field.skip;
            }
            break;
        }

        // Check the limit of data pointer.
        if (end_addr && start_addr > end_addr) {
            e64_emit_line();
            break;
        }
    }
}
