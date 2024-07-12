//
// Extracode 050 - elementary math functions and other services.
//
// Copyright (c) 2023-2024 Serge Vakulenko
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
#include <iomanip>
#include <cmath>

#include "machine.h"

//
// Check whether value is large enough to be printed
// in fixed format with given precision.
//
static bool good_for_fixed_format(double value, int precision)
{
    if (value < 0) {
        value = -value;
    }
    if (value == 0) {
        return true;
    }
    if (value >= 1) {
        return true;
    }
    value *= std::pow(10, precision);
    return (value >= 1);
}

//
// Extracode e50 15: format real number.
//
Word Processor::e50_format_real(Word input, unsigned &overflow)
{
    E50_Format_Info info;
    info.word = input;
    machine.trace_e50_format_real(info);

    const bool right_align   = info.field.right_align;
    const unsigned width     = info.field.width;
    const unsigned precision = info.field.precision;

    // Source value.
    const unsigned src_addr = ADDR(info.field.src_addr + core.M[info.field.src_reg]);
    const double value      = besm6_to_ieee(machine.mem_load(src_addr));

    // Destination pointer.
    static unsigned dest_addr;
    if (info.field.dest_addr != 0 || info.field.dest_reg != 0)
        dest_addr = ADDR(info.field.dest_addr + core.M[info.field.dest_reg]);
    BytePointer bp(memory, dest_addr);

    if (width == 0) {
        // Nothing to do.
        overflow = 0;
        return 0;
    }

    // Format as fixed point or as scientific.
    std::ostringstream scientific;
    scientific << std::scientific << std::uppercase << std::setprecision(precision) << value;
    std::string result = scientific.str();

    if (good_for_fixed_format(value, precision)) {
        std::ostringstream fixed_point;
        fixed_point << std::fixed << std::setprecision(precision) << value;

        if (fixed_point.str().size() <= scientific.str().size()) {
            // Fixed point format is shorter.
            result = fixed_point.str();
        }
    }

    overflow = (result.size() > width);
    if (!right_align) {
        // Align to the left.
        if (result.size() < width) {
            result.append(width - result.size(), ' ');
        } else if (overflow) {
            result.erase(width);
        }
    } else if (overflow) {
        // Align to the right, skip first few bytes.
        result.erase(0, result.size() - width);
    } else if (result.size() < width) {
        // Align to the right, fill with spaces.
        result.insert(0, width - result.size(), ' ');
    }

    for (char ch : result) {
        bp.put_byte(ch);
    }

    // Fill last word with zeroes.
    while (bp.byte_index) {
        bp.put_byte(' ');
    }
    return width;
}

//
// Extracode 050: elementary math functions and other services.
//
void Processor::e50()
{
    auto addr = core.M[016];
    switch (addr) {
    case 0:
        core.ACC = besm6_sqrt(core.ACC);
        break;
    case 1:
        core.ACC = besm6_sin(core.ACC);
        break;
    case 2:
        core.ACC = besm6_cos(core.ACC);
        break;
    case 3:
        core.ACC = besm6_arctan(core.ACC);
        break;
    case 4:
        core.ACC = besm6_arcsin(core.ACC);
        break;
    case 5:
        core.ACC = besm6_log(core.ACC);
        break;
    case 6:
        core.ACC = besm6_exp(core.ACC);
        break;
    case 7:
        core.ACC = besm6_floor(core.ACC);
        break;
    case 017:
        // Format real numbers.
        core.ACC = e50_format_real(core.ACC, core.M[14]);
        break;
    case 064:
        // Print some message.
        // TODO: print_iso(ADDR(core.ACC));
        break;
    case 066:
        // Get reply from operator.
        // Change page on plotter.
        machine.plotter.change_page();
        core.ACC = 0;
        break;
    case 067: {
        // DATE*, OS Dubna specific.
        // Always return the same date/time, for easy testing.
        static const Word     DAY   = 0x04;
        static const Word     MONTH = 0x07; // July
        static const Word     YEAR  = 0x24;
        static const unsigned HOUR  = 0x23;
        static const unsigned MIN   = 0x45;
        static const unsigned SEC   = 0x56;

        // Date: DD MON YY
        //        |  |   |
        //       42  34  26 - shift
        // Time: 00.00.00
        //        |  |  |
        //       20  16 4 - shift
        core.ACC = (DAY << 42) | (MONTH << 34) | (YEAR << 26) |
                   (HOUR << 20) | (MIN << 12) | (SEC << 4);
        break;
    }
    case 075:
        // Unknown.
        break;
    case 076:
        // Send message to operator.
        // TODO: print_iso(ADDR(core.ACC));
        break;
    case 0102:
        // Some conversion?
        break;
    case 0103:
        // TODO: Intercept авост, for Forex.
        break;
    case 0202:
        // Convert tape number from internal format into 2-10 format.
        break;
    case 0203:
        // Convert tape number from 2-10 format into internal format.
        break;
    case 0210:
        // TODO: Lock/release semaphores.
        break;
    case 0211:
        // Pause the task? Waiting for tape.
        throw Exception("Task paused waiting for tape");
        break;
    case 070077:
        // Get date?
        core.ACC = 0;
        break;
    case 070200:
        // Asking for some capabilities?
        core.ACC = 0'0010'0000;
        break;
    case 070210:
        // Get time?
        core.ACC = 0;
        break;
    case 070214:
        // Asking for шифр?
        core.ACC = 0'1234'5670'1234'5670;
        break;
    case 070217:
        // Unknown
        break;
    case 071223:
        // Unknown, for Forex.
        break;
    case 072200:
        // Unknown, for Dipol.
        break;
    case 072211:
        // Set time limit?
        // TODO: show time limit on core.ACC
        break;
    case 072214:
        // Set something for шифр?
        break;
    case 072216:
        // Set paper limit?
        // TODO: show paper limit on core.ACC
        break;
    case 074200:
        // Unknown, for *page
        break;
    case 074671:
        // Unknown
        break;
    case 074673:
        // Unknown
        break;
    case 076200:
        // Unknown, for *tape
        break;
    default:
        throw Exception("Unimplemented extracode *50 " + to_octal(addr));
    }
}
