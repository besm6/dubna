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
#include <cmath>
#include <cstring>
#include <iomanip>
#include <sstream>

#include "machine.h"

static inline bool IS_DIGIT(uint8_t c)
{
    return (c >= '0') && (c <= '9');
}

static inline bool IS_CHAR(uint8_t c)
{
    return (c >= 0101 && c <= 0132) ||
           (c >= 0140 && c <= 0176);
}

//
// Э50 014 - распознаватель текстовой строки.
// А.П.Сапожников 22/12/80
//
Word Processor::e50_parse(Word input, unsigned &result)
{
    //
    // Строка подается в isо или в соsу. Признак конца - байт '000' или '012'.
    // Информация для экстракода на сумматоре:
    //     1-15, 21-24 разряды - индексированный адрес начала строки или 0, если работа с
    //          текущего места строки.
    //     16 - признак поглощения пробелов
    //     17 - символы '*' и '/' суть буквы.
    //     18 - восьмеричные числа задаются без суффикса "в"
    //     19 - посимвольное сканирование
    //
    // Результатом работы является тип фрагмента в индекс-регистре 14
    // и сам фрагмент. Пробелы в начале фрагмента типа "число"
    // всегда сглатываются. Если фрагмент - число, то это число
    // выдается на сумматоре, а в РМР выдается (1-8 разряды) код
    // ограничителя и (25-33 разряды) позиция этого ограничителя во
    // входной строке. Если фрагмент - идентификатор или текст,
    // заключенный в апострофы, то он выдается на сумматоре (начало) и
    // в РМР (продолжение). Хвост текста заполняется пробелами. Если
    // текст более 12 символов, то этот факт запоминается, и остаток
    // текста будет выдан при следующем обращении.
    //
    // Возможные значения индекс-регистра 14:
    //     0 - ошибка, фрагмент не распознан
    //     1 - восьмеричное (123в, 99d, -10в)
    //     2 - целое (8000, -999)
    //     3 - вещественное (3.62, -3.141е-3)
    //     4 - идентификатор или текст в апострофах
    //     5 - то же, но длиной > 12 символов
    //     6 - пустой фрагмент
    //
    unsigned src_addr = input & 077777;
    // const int skip_spaces = (input >> 15) & 1;
    const int star_slash_flag = (input >> 16) & 1;
    // const int octal_flag = (input >> 17) & 1;
    const int char_mode = (input >> 18) & 1;
    const int src_reg  = (input >> 20) & 15;
    static int index;
    static uint8_t ident[128];
    static unsigned ident_len;
    uint64_t value;
    bool negate = false;
    static unsigned last_word_addr;
    static unsigned last_byte_index;
    BytePointer bp{memory, last_word_addr, last_byte_index};

    // std::cout << "--- e50_parse: acc = "; besm6_print_word_octal(std::cout, input); std::cout << '\n';
    if (src_reg) {
        src_addr = ADDR(src_addr + core.M[src_reg]);
    }
    if (src_addr != 0) {
        // Set source pointer.
        bp.word_addr = src_addr;
        bp.byte_index = 0;
        index = 0;
    } else {
        // Continue from current place.
        if (last_word_addr == 0) {
            // Parse error.
            result = 0;
            // std::cout << "--- Parse error\n";
            return 0;
        }
    }
#if 0
    BytePointer tp{memory, bp.word_addr, bp.byte_index};
    while (tp.word_addr != 0) {
            int c = tp.get_byte();
            // std::cout << '-' << std::oct << c << std::dec;
            std::cout << (c ? (char)c : '@');

            if (c == 0 || c == 012)
                    break;
    }
    std::cout << '\n';
#endif
    if (char_mode) {
        //
        // В режиме посимвольного сканирования текущий символ строки выдается на
        // сумматоре (прижат влево и дополнен пробелами) и в 1-8 разряды РМР.
        // В 25-33 разрядах РМР выдается номер позиции символа в строке.
        // Пробелы при необходимости поглощаются.
        //
        // Тип символа выдается в индекс-регистре 14:
        //     0 - конец строки ('000','012')
        //     1 - цифра (0 - 9)
        //     2 - буква (а-z-я, по заказу: /, *)
        //     3 - разделитель (не цифра и не буква)
        //
        for (;;) {
            if (bp.word_addr == 0) {
                // Error.
                result = 0;
                core.RMR = (Word)index << 24;
                // std::cout << "--- Error\n";
                return 0;
            }
            uint8_t c = bp.get_byte();
            index++;
            switch (c) {
            case 0:
            case 012:
                // End of line.
                // std::cout << "--- End of line, index = " << index << '\n';
                result = 0;
ret:
                core.RMR = (Word)index << 24 | c;
                return ((Word)c << 40) | ((Word)' ' << 32) | (' ' << 24) |
                        (' ' << 16) | (' ' << 8) | ' ';

            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                // Digit.
                result = 1;
                // std::cout << "--- Digit = " << (c-'0') << ", index = " << index << '\n';
                goto ret;

            case '*':
            case '/':
                if (star_slash_flag)
                    goto letter;
                goto delimiter;

            default:
                if (IS_CHAR(c)) {
                letter:
                    result = 2;
                    // std::cout << "--- Letter = '" << (char)c << "', index = " << index << '\n';
                    goto ret;
                }
            delimiter:
                result = 3;
                // std::cout << "--- Delimiter = '" << (char)c << "', index = " << index << '\n';
                goto ret;
            }
        }
    }

    for (;;) {
        if (bp.word_addr == 0) {
            // Error.
            result = 0;
            // std::cout << "--- Error\n";
            return 0;
        }
        uint8_t c = bp.get_byte();
        index++;
        switch (c) {
        case 0:
        case 012:
            // Empty fragment.
empty:
            result = 6;
            // std::cout << "--- Delimiter = '" << (char)c << "'\n";
            last_word_addr = bp.word_addr;
            last_byte_index = bp.byte_index;
            return c;

        case ' ':
            continue;

        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        number:
            value = c - '0';
            for (;;) {
                c = bp.get_byte();
                index++;

                if (!IS_DIGIT(c))
                    break;

                // Currently only octal numbers are supported.
                value = (value << 3) + (c - '0');
            }
            if (negate)
                value = -value;

            // Return value, delimiter and it's index.
            result = 1;
            core.RMR = (Word)index << 24 | c;
            // std::cout << "--- Number = 0" << std::oct << value << std::dec << ", delimiter = '" << (char)c << "', index = " << index << '\n';
            last_word_addr = bp.word_addr;
            last_byte_index = bp.byte_index;
            return value;

        case '-':
            c = bp.peek_byte();
            if (IS_DIGIT(c)) {
                negate = true;
                goto number;
            }
            c = '-';
            goto empty;

        case '*':
        case '/':
            if (star_slash_flag)
                goto ident;
            goto empty;

        default:
            if (!IS_CHAR(c))
                goto empty;

            // Get identifier.
ident:
            ident[0]  = c;
            ident_len = 1;
            memset(ident + 1, ' ', sizeof(ident) - 1);
            while (ident_len < sizeof(ident)) {
                c = bp.get_byte();
                index++;

                if (!IS_CHAR(c) && !IS_DIGIT(c) && (!star_slash_flag || (c != '*' && c != '/')))
                    break;

                // Currently only octal numbers are supported.
                ident[ident_len++] = c;
            }

            // Only short identifiers are supported for now.
            result = 4;
            // std::cout << "--- Identifier = '" << std::setw(12) << ident << "'\n";
            last_word_addr = bp.word_addr;
            last_byte_index = bp.byte_index;
            core.RMR = ((Word)ident[6] << 40) | ((Word)ident[7] << 32) | (ident[8] << 24) |
                       (ident[9] << 16) | (ident[10] << 8) | ident[11];
            return ((Word)ident[0] << 40) | ((Word)ident[1] << 32) | (ident[2] << 24) |
                   (ident[3] << 16) | (ident[4] << 8) | ident[5];
        }
    }
}

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
    case 014:
        // Parse a string.
        core.ACC = e50_parse(core.ACC, core.M[14]);
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
        machine.plotter_change_page();
        core.ACC = 0;
        break;
    case 067: {
        // DATE*, OS Dubna specific.
        // Always return the same date/time, for easy testing.
        static const Word DAY      = 0x04;
        static const Word MONTH    = 0x07; // July
        static const Word YEAR     = 0x24;
        static const unsigned HOUR = 0x23;
        static const unsigned MIN  = 0x45;
        static const unsigned SEC  = 0x56;

        // Date: DD MON YY
        //        |  |   |
        //       42  34  26 - shift
        // Time: 00.00.00
        //        |  |  |
        //       20  16 4 - shift
        core.ACC =
            (DAY << 42) | (MONTH << 34) | (YEAR << 26) | (HOUR << 20) | (MIN << 12) | (SEC << 4);
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
    case 0205:
        // Unknown, for Liza.
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
    case 070236:
        // Unknown, for DIPOL.
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
