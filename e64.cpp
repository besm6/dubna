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
#include <iostream>

#include "encoding.h"
#include "gost10859.h"
#include "machine.h"

static const bool TRACE_E64 = false;

//
// Byte pointer.
//
class BytePointer {
private:
    Memory &memory;

public:
    unsigned word_addr;
    unsigned byte_index;

    BytePointer(Memory &m, unsigned wa, unsigned bi = 0) : memory(m), word_addr(wa), byte_index(bi)
    {
    }

    unsigned get_byte()
    {
        const Word *ptr = memory.get_ptr(word_addr);
        unsigned ch     = *ptr >> (40 - byte_index * 8);

        byte_index++;
        if (byte_index == 6) {
            byte_index = 0;
            word_addr++;
        }
        return ch;
    }
};

static int line_flush(unsigned char *line)
{
    int i;

    for (i = 127; i >= 0; --i)
        if (line[i] != GOST_SPACE)
            break;

    if (i < 0)
        return 0;

    gost_write(line, i + 1);
    memset(line, GOST_SPACE, i + 1);
    return 1;
}

static void print_text_debug(unsigned addr0, unsigned addr1, bool itm_flag, int pos)
{
    // TODO: print text debug
#if 0
    BytePointer bp(memory, addr0);
    int c;

    printf("*** E64  %s ", itm_flag ? "itm" : "gost");
    for (;;) {
        if (!bp.word_addr) {
        done:
            printf("\n");
            return;
        }
        if (addr1 && bp.word_addr == addr1 + 1)
            goto done;

        c = bp.get_byte();
        printf("-%03o", c);

        // end of information
        if (itm_flag) {
            switch (c) {
            case 0140: // end of information
                goto done;
            case 0173: // repeat last symbol
                c = bp.get_byte();
                printf("-%03o", c);
                if (c == 040)
                    pos = 0;
                else
                    pos += c & 017;
                break;
            default:
                // No space left on line.
                if (!addr1 && pos == 128)
                    goto done;
                ++pos;
                if (pos == 128) {
                    // No space left on line.
                    std::cout << '/';
                    pos = 0;
                }
                break;
            }
        } else {
            switch (c) {
            case GOST_END_OF_INFORMATION:
            case 0231:
            case GOST_EOF:
                goto done;
            case 0201: // new page
                if (pos > 0)
                    pos = 0;
                ++pos;
                break;
            case GOST_CARRIAGE_RETURN:
            case GOST_NEWLINE:
                pos = 0;
                break;
            case 0143: // null width symbol
            case 0341:
                break;
            case GOST_SET_POSITION:
            case 0200: // set position
                c = bp.get_byte();
                printf("-%03o", c);
                pos = c % 128;
                break;
            default:
                // No space left on line.
                if (pos == 128)
                    goto done;
                ++pos;
                if (pos == 128) {
                    // No space left on line.
                    std::cout << '/';
                    if (addr1)
                        pos = 0;
                }
                break;
            }
        }
    }
#endif
}

static void print_char(unsigned char *line, int *pos, int sym)
{
    if (*pos == 128) {
        line_flush(line);
        std::cout << std::endl;
    }
    line[(*pos) & 127] = sym;
    ++(*pos);
}

static void print_opcode1(unsigned char *line, int *pos, unsigned cmd)
{
    print_char(line, pos, cmd >> 23 & 1);
    print_char(line, pos, cmd >> 20 & 7);
    print_char(line, pos, GOST_SPACE);
    if (cmd & 02000000) {
        // long address command
        print_char(line, pos, cmd >> 18 & 3);
        print_char(line, pos, cmd >> 15 & 7);
        print_char(line, pos, GOST_SPACE);
        print_char(line, pos, cmd >> 12 & 7);
    } else {
        // short address command
        print_char(line, pos, cmd >> 18 & 1);
        print_char(line, pos, cmd >> 15 & 7);
        print_char(line, pos, cmd >> 12 & 7);
        print_char(line, pos, GOST_SPACE);
    }
    print_char(line, pos, cmd >> 9 & 7);
    print_char(line, pos, cmd >> 6 & 7);
    print_char(line, pos, cmd >> 3 & 7);
    print_char(line, pos, cmd & 7);
}

#if 0
#define IS_DIGIT(c) (c >= '0' && c <= '9')
#define IS_CHAR(c)  ((c >= 0101 && c <= 0132) || (c >= 0140 && c <= 0136))

static void exform(void);

void putbyte(ptr *bp, unsigned char c)
{
    core[bp->word_addr].w_b[bp->byte_index++] = c;

    if (bp->byte_index == 6) {
        bp->byte_index = 0;
        ++bp->word_addr;
    }
}

uint64_t getword(ptr *bp)
{
    uint64_t w = 0;
    int i;

    if (bp->byte_index) {
        bp->byte_index = 0;
        ++bp->word_addr;
    }
    for (i = 0; i < 6; ++i)
        w = w << 8 | core[bp->word_addr].w_b[i];

    ++bp->word_addr;

    return w;
}

void cwadj(uinstr_t *ip)
{
    if (ip->i_opcode >= 0100) {
        ip->i_opcode = (ip->i_opcode - 060) << 3;
        ip->i_opcode |= ip->i_addr >> 12;
        ip->i_addr &= 0xfff;
    } else if (ip->i_addr & 070000) {
        ip->i_addr &= 07777;
        ip->i_opcode |= 0100;
    }
}

void terminate(void)
{
    unsigned u;

    for (u = 030; u < 070; ++u)
        if (disks[u].diskh)
            disk_close(disks[u].diskh);
}
#endif

//
// Extract decimal exponent from the real value.
// Return value in range 0.1 - 0.9(9).
// Input value must be nonzero positive.
//
static double real_exponent(double value, int *exponent)
{
    *exponent = 0;
    if (value <= 0)
        return 0; // cannot happen

    while (value >= 1000000) {
        *exponent += 6;
        value /= 1000000;
    }
    while (value >= 1) {
        ++*exponent;
        value /= 10;
    }
    while (value < 0.0000001) {
        *exponent -= 6;
        value *= 1000000;
    }
    while (value < 0.1) {
        --*exponent;
        value *= 10;
    }
    return value;
}

//
// Print string in ITM format.
// Return next data address.
//
unsigned Processor::e64_print_itm(unsigned addr0, unsigned addr1, unsigned char *line, int pos)
{
    BytePointer bp(memory, addr0);
    uint8_t lastc = GOST_SPACE;

    while (bp.word_addr) {
        // No data to print.
        if (addr1 && bp.word_addr == addr1 + 1) {
            return bp.word_addr;
        }

        // No space left on line.
        if (pos == 128) {
            if (!addr1) {
                if (bp.byte_index) {
                    ++bp.word_addr;
                }
                return bp.word_addr;
            }
            line_flush(line);
            std::cout << std::endl;
            pos = 0;
        }

        uint8_t c = bp.get_byte();
        switch (c) {
        case 0140: // end of information
            if (bp.byte_index) {
                ++bp.word_addr;
            }
            return bp.word_addr;

        case 040: // blank
            line[pos++] = GOST_SPACE;
            break;

        case 0173: // repeat last symbol
            c = bp.get_byte();
            if (c == 040) {
                // fill line by last symbol (?)
                memset(line, lastc, 128);
                line_flush(line);
                std::cout << std::endl;
                pos = 0;
            } else {
                while (c-- & 017) {
                    line[pos++] = lastc;
                }
            }
            break;

        default:
            lastc       = itm_to_gost[c];
            line[pos++] = lastc;
            break;
        }
    }
    return 0;
}

//
// Print word(s) in octal format.
// Return next data address.
//
unsigned Processor::e64_print_octal(unsigned addr0, unsigned addr1, unsigned char *line, int pos,
                                    int digits, int width, int repeat)
{
    if (digits > 16) {
        digits = 16;
    }
    while (addr0) {
        // No data to print.
        if (addr1 && addr0 == addr1 + 1) {
            return addr0;
        }

        // No space left on line.
        if (pos >= 128) {
            if (!addr1) {
                return 0;
            }
            return addr0;
        }
        Word word = machine.mem_load(addr0);
        ++addr0;

        word <<= 64 - digits * 3;
        for (int i = 0; i < digits; ++i) {
            print_char(line, &pos, (int)(word >> 61) & 7);
            word <<= 3;
        }

        if (!repeat) {
            return addr0;
        }
        --repeat;
        if (width) {
            pos += width - digits;
        }
    }
    return 0;
}

//
// Print CPU instruction(s).
// Return next data address.
//
unsigned Processor::e64_print_opcode(unsigned addr0, unsigned addr1, unsigned char *line, int pos,
                                     int width, int repeat)
{
    while (addr0) {
        // No data to print.
        if (addr1 && addr0 == addr1 + 1) {
            return addr0;
        }

        // No space left on line.
        if (pos >= 128) {
            if (!addr1) {
                return 0;
            }
            return addr0;
        }
        Word word  = machine.mem_load(addr0);
        unsigned a = word >> 24;
        unsigned b = word & BITS(24);
        ++addr0;

        print_opcode1(line, &pos, a);
        print_char(line, &pos, GOST_SPACE);
        print_opcode1(line, &pos, b);

        if (!repeat) {
            return addr0;
        }
        --repeat;
        if (width) {
            pos += width - 23;
        }
    }
    return 0;
}

//
// Print real number(s).
// Return next data address.
//
unsigned Processor::e64_print_real(unsigned addr0, unsigned addr1, unsigned char *line, int pos,
                                   int digits, int width, int repeat)
{
    if (digits > 20) {
        digits = 20;
    }
    while (addr0) {
        // No data to print.
        if (addr1 && addr0 == addr1 + 1) {
            return addr0;
        }

        // No space left on line.
        if (pos >= 128) {
            if (!addr1) {
                return 0;
            }
            return addr0;
        }

        Word word     = machine.mem_load(addr0);
        bool negative = (word & BIT41);
        double value;
        int exponent;
        if ((word & ~BIT41) == 0) {
            value    = 0;
            exponent = 0;
        } else {
            value = besm6_to_ieee(word);
            if (value < 0) {
                value = -value;
            }
            value = real_exponent(value, &exponent);
        }
        ++addr0;

        print_char(line, &pos, GOST_SPACE);
        print_char(line, &pos, negative ? GOST_MINUS : GOST_PLUS);

        for (int i = 0; i < digits - 4; ++i) {
            value     = value * 10;
            int digit = (int)value;
            print_char(line, &pos, digit);
            value -= digit;
        }
        print_char(line, &pos, GOST_LOWER_TEN);
        if (exponent >= 0) {
            print_char(line, &pos, GOST_PLUS);
        } else {
            print_char(line, &pos, GOST_MINUS);
            exponent = -exponent;
        }
        print_char(line, &pos, exponent / 10);
        print_char(line, &pos, exponent % 10);

        if (!repeat) {
            return addr0;
        }
        --repeat;
        if (width) {
            pos += width - digits - 2;
        }
    }
    return 0;
}

//
// Print string in GOST format.
// Return next data address.
//
unsigned Processor::e64_print_gost(unsigned addr0, unsigned addr1, unsigned char *line, int pos,
                                   bool *need_newline)
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
        switch (ch) {
        case GOST_EOF:
        case GOST_END_OF_INFORMATION:
        case 0231:
            if (pos == 0 || pos == 128)
                *need_newline = false;
            if (bp.byte_index != 0)
                ++bp.word_addr;
            return bp.word_addr;
        case 0201: // new page
            if (pos) {
                line_flush(line);
                pos = 0;
            }
            if (!isatty(1)) {
                std::cout << '\f';
            }
            line[pos++] = GOST_SPACE;
            break;
        case GOST_CARRIAGE_RETURN:
        case GOST_NEWLINE:
            if (pos == 128) {
                pos = 0;
                break;
            }
            if (pos) {
                line_flush(line);
                pos = 0;
            }
            std::cout << std::endl;
            break;
        case 0143: // null width symbol
        case 0341:
            break;
        case GOST_SET_POSITION:
        case 0200: // set position
            ch  = bp.get_byte();
            pos = ch % 128;
            break;
        case 0174:
        case 0265: // repeat last symbol
            ch = bp.get_byte();
            if (ch == 040) {
                // fill line by last symbol (?)
                memset(line, last_ch, 128);
                line_flush(line);
                std::cout << std::endl;
                pos = 0;
            } else {
                while (ch-- & 017) {
                    if (line[pos] == GOST_SPACE)
                        line[pos] = last_ch;
                    ++pos;
                }
            }
            break;
        case GOST_SPACE2: // blank
        case 0242:        // used as space by forex
            ch = GOST_SPACE;
            // fall through...
        default:
            if (pos == 128) {
                if (addr1) {
                    pos = 0;
                } else {
                    // No space left on line.
                    *need_newline = false;
                    if (bp.byte_index != 0)
                        ++bp.word_addr;
                    return bp.word_addr;
                }
            }
            if (line[pos] != GOST_SPACE) {
                line_flush(line);
                std::cout << std::endl;
            }
            line[pos] = ch;
            last_ch   = ch;
            ++pos;
            if (pos == 128) {
                // No space left on line.
                line_flush(line);
                std::cout << std::endl;
            }
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
    unsigned ctl_addr   = core.M[016];
    Word ctl_word       = machine.mem_load(ctl_addr);
    unsigned start_addr = ADDR(FIELD(ctl_word, 25, 15) +        // left address (long)
                               core.M[FIELD(ctl_word, 45, 4)]); // left register
    unsigned end_addr   = ADDR(FIELD(ctl_word, 1, 15) +         // right address (long)
                               core.M[FIELD(ctl_word, 21, 4)]); // right register
    if (end_addr <= start_addr)
        end_addr = 0;                                           // No limit
    if (start_addr == 0)
        throw Processor::Exception("Bad start_addr in extracode e64");

    // Execute every format word in order.
    unsigned char line[256];
    memset(line, GOST_SPACE, sizeof(line));
    for (;;) {
        // Get next control word.
        ctl_addr++;
        if (ctl_addr >= MEMORY_NWORDS)
            throw Processor::Exception("Unterminated info list in extracode e64");
        ctl_word = machine.mem_load(ctl_addr);

        // Extract fields.
        unsigned format     = FIELD(ctl_word, 45, 4);  // left register
        unsigned offset     = FIELD(ctl_word, 37, 8);  // left opcode
        unsigned digits     = FIELD(ctl_word, 25, 12); // left address (short)
        unsigned final_skip = FIELD(ctl_word, 21, 4);  // right register
        unsigned width      = FIELD(ctl_word, 13, 8);  // right opcode
        unsigned repeat     = FIELD(ctl_word, 1, 12);  // right address (short)
#if 0
        if (TRACE_E64) {
            printf("*** E64  %05o-%05o  format=%u offset=%u", start_addr, end_addr, format, offset);
            if (digits)
                printf(" digits=%d", digits);
            if (width)
                printf(" width=%d", width);
            if (repeat)
                printf(" repeat=%d", repeat);
            if (final_skip)
                printf(" final=%#o", final_skip);
            printf("\n");
        }
#endif
        bool need_newline = true;
        switch (format) {
        case 0:
            // Text in GOST encoding.
            if (TRACE_E64) {
                print_text_debug(start_addr, end_addr, 0, offset);
            }
            start_addr = e64_print_gost(start_addr, end_addr, line, offset, &need_newline);
            break;

        case 1:
            // CPU instruction.
            start_addr = e64_print_opcode(start_addr, end_addr, line, offset, width, repeat);
            break;

        case 2:
            // Octal number.
            start_addr = e64_print_octal(start_addr, end_addr, line, offset, digits, width, repeat);
            break;

        case 3:
            // Real number.
            start_addr = e64_print_real(start_addr, end_addr, line, offset, digits, width, repeat);
            break;

        case 4:
            // Text in ITM encoding.
            if (TRACE_E64) {
                print_text_debug(start_addr, end_addr, 1, offset);
            }
            start_addr = e64_print_itm(start_addr, end_addr, line, offset);
            break;
        }

        if (final_skip & 8) {
            final_skip &= 7;
            if (line_flush(line) || (need_newline && !final_skip))
                ++final_skip;

            if (end_addr && start_addr <= end_addr) {
                // Repeat printing task until all data expired.
                std::cout << std::endl;
                continue;
            }

            while (final_skip-- > 0)
                std::cout << std::endl;
            break;
        }

        // Check the limit of data pointer.
        if (end_addr && start_addr > end_addr) {
            line_flush(line);
            std::cout << std::endl;
            break;
        }
    }
}
