//
// Execute extracodes.
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
#include <iostream>
#include <iomanip>
#include <sstream>

#include "machine.h"
#include "encoding.h"

//
// Execute extracode.
//
void Processor::extracode(unsigned opcode)
{
    // Return from extracode to the next machine word.
    if (core.right_instr_flag) {
        core.PC += 1;
        core.right_instr_flag = false;
    }

    switch (opcode) {
    case 050: // Elementary math functions and other services.
        e50();
        break;

    case 051: // sin() function.
        e51();
        break;

    case 052: // cos() function.
        e52();
        break;

    case 053: // arctan() function.
        e53();
        break;

    case 054: // arcsin() function.
        e54();
        break;

    case 055: // log() function.
        e55();
        break;

    case 056: // exp() function.
        e56();
        break;

    case 057: // Mount tapes.
        e57();
        break;

    case 060: // Punch cards?
        e60();
        break;

    case 061: // VT-340 control.
        e61();
        break;

    case 063: // OS Dubna specific.
        e63();
        break;

    case 064: // Text output.
        e64();
        break;

    case 065: // Read pult tumblers.
        e65();
        break;

    case 067: // Debug.
        e67();
        break;

    case 070: // Disk or drum i/o.
        e70();
        break;

    case 071: // Punch card r/w
        e71();
        break;

    case 072: // OS Dubna specific.
        e72();
        break;

    case 074: // Finish the job.
        throw Exception("");

    case 075: // Write to memory with instruction check bits.
        e75();
        break;

    case 076: // Call routine in kernel mode?
        e76();
        break;

    default:
        throw Exception("Unimplemented extracode " + to_octal(opcode));
    }
}

//
// Extracode 070: disk/drum read/write.
//
void Processor::e70()
{
    // Read control word at the executive address.
    // When address is zero - the control word is on accumulator.
    E70_Info info;
    info.word = (core.M[016] == 0) ? core.ACC : machine.mem_load(core.M[016]);

    machine.trace_e70(info);

    char op       = info.disk.read_op ? 'r' : 'w';
    unsigned addr = info.disk.page << 10;
    if (info.disk.unit >= 030 && info.disk.unit < 070) {
        //
        // Disk read/write.
        //
        if (info.disk.seek) {
            // Speculative operation: tape seek.
            return;
        }
        machine.disk_io(op, info.disk.unit - 030, info.disk.zone, 0, addr, 1024);
    } else {
        //
        // Drum read/write.
        //
        unsigned tract  = info.drum.tract;
        unsigned sector = info.drum.sector;
        if (info.drum.raw_sect & info.drum.sect_io) {
            // Raw sector index in lower bits.
            sector = info.disk.zone & 3;
            tract  = (info.disk.zone >> 2) & 037;
        }

        if (info.drum.sect_io) {
            addr += info.drum.paragraph << 8;
        }

        if (info.drum.phys_io) {
            // Remap to disk drive.
            unsigned this_drum   = info.drum.unit & 037;
            unsigned mapped_drum = machine.get_mapped_drum();

            if (this_drum >= mapped_drum) {
                unsigned disk_unit = machine.PHYS_IO_UNIT;
                unsigned zone      = tract + (this_drum - mapped_drum) * 040;

                if (info.drum.sect_io == 0) {
                    // Full page i/o with disk.
                    machine.disk_io(op, disk_unit, zone, 0, addr, 1024);
                } else {
                    // Sector i/o with disk (1/4 of page).
                    machine.disk_io(op, disk_unit, zone, sector, addr, 256);
                }
                return;
            }
        }

        if (info.drum.sect_io == 0) {
            // Full page i/o.
            machine.drum_io(op, info.drum.unit & 037, tract, 0, addr, 1024);
        } else {
            // Sector i/o (1/4 of page).
            machine.drum_io(op, info.drum.unit & 037, tract, sector, addr, 256);
        }
    }
}

//
// Extracode 075: write accumulator to memory with instruction check bits.
//
void Processor::e75()
{
    auto addr = core.M[016];
    if (addr > 0) {
        machine.mem_store(addr, core.ACC);

        if (addr == 020) {
            // Intercept arithmetic overflow and division by zero.
            intercept_count = 1;
        }
    }
}

//
// Check whether value is large enough to be printed
// in fixed format with given precision.
//
static bool large_enough_for_fixed_format(double value, int precision)
{
    if (value < 0) {
        value = -value;
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

    if (large_enough_for_fixed_format(value, precision)) {
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

//
// Extracode 051: sin() function.
//
void Processor::e51()
{
    auto addr = core.M[016];
    switch (addr) {
    case 0:
        core.ACC = besm6_sin(core.ACC);
        break;
    default:
        throw Exception("Unimplemented extracode *51 " + to_octal(addr));
    }
}

//
// Extracode 052: cos() function.
//
void Processor::e52()
{
    auto addr = core.M[016];
    switch (addr) {
    case 0:
        core.ACC = besm6_cos(core.ACC);
        break;
    default:
        throw Exception("Unimplemented extracode *52 " + to_octal(addr));
    }
}

//
// Extracode 053: arctan() function.
//
void Processor::e53()
{
    auto addr = core.M[016];
    switch (addr) {
    case 0:
        core.ACC = besm6_arctan(core.ACC);
        break;
    default:
        throw Exception("Unimplemented extracode *53 " + to_octal(addr));
    }
}

//
// Extracode 054: arcsin() function.
//
void Processor::e54()
{
    auto addr = core.M[016];
    switch (addr) {
    case 0:
        core.ACC = besm6_arcsin(core.ACC);
        break;
    default:
        throw Exception("Unimplemented extracode *54 " + to_octal(addr));
    }
}

//
// Extracode 055: log() function.
//
void Processor::e55()
{
    auto addr = core.M[016];
    switch (addr) {
    case 0:
        core.ACC = besm6_log(core.ACC);
        break;
    default:
        throw Exception("Unimplemented extracode *55 " + to_octal(addr));
    }
}

//
// Extracode 056: exp() function.
//
void Processor::e56()
{
    auto addr = core.M[016];
    switch (addr) {
    case 0:
        core.ACC = besm6_exp(core.ACC);
        break;
    default:
        throw Exception("Unimplemented extracode *56 " + to_octal(addr));
    }
}

//
// Extracode 063: manage time limit.
//
void Processor::e63()
{
    switch (core.M[016]) {
    case 4:
        // Get CPU time in 1/50 of seconds.
        // Use fixed value for easy testing.
        core.ACC = 412 / 2;
        return;
    case 7:
        // Get machine number in bits 36-34.
        core.ACC = 5ULL << 33;
        return;
    case 0502:
        // Get phys.address of process descriptor.
        core.ACC = 02000;
        return;
    case 0573:
        // Get phys.address of limits descriptor.
        core.ACC = 04000;
        return;
    case 0704:
        // Get page mode.
        // Bit 44 means disable page numbering.
        core.ACC = 1ull << 43;
        return;
    case 0760:
    case 0761:
        // Get phys.address of some other descriptor.
        core.ACC = 03000;
        return;
    case 0765:
        // Get name of organization in ISO.
        core.ACC = 0'3244'7513'2064'2554; // йоксел
        return;
    case 02000:
        // Get word #0 of process descriptor: task ID (шифр).
        core.ACC = 01234567;
        return;
    case 03000:
    case 03001:
    case 03010:
        // Get words from some other descriptor.
        core.ACC = 0;
        return;
    case 04000:
        // Get word #1 from limits descriptor.
        core.ACC = 0;
        return;
    default:
        throw Exception("Unimplemented extracode *63 " + to_octal(core.M[016]));
    }
}

//
// Extracode 072.
//
void Processor::e72()
{
    unsigned addr = core.M[016];
    if (addr >= 010) {
        // Request or release pages of memory.
        //std::cerr << "\n--- Ignore extracode *72 " + to_octal(addr) << std::endl;
        return;
    }

    switch (addr) {
    case 4:
        // Write the input card to the dayfile.
        // Address of MONCARD* array is in bits 15:1 of accumulator.
        // See source file dubna.ms, line 21300.
        // Ignore for now.
        return;
    default:
        throw Exception("Unimplemented extracode *72 " + to_octal(addr));
    }
}

//
// Extracode 065: read pult tumblers.
//
void Processor::e65()
{
    unsigned addr = core.M[016];
    switch (addr) {
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
        // All pult tumblers are off.
        core.ACC = 0;
        return;
    case 0502:
        // Get address of process descriptor.
        core.ACC = 02000;
        return;
    case 0526:
        // Get address of ALLTOISO encoding table.
        core.ACC = 06000;
        return;
    case 0560:
        // Get address of A/LINKP.
        core.ACC = 05000;
        return;
    case 0564:
        // Get address of user table.
        core.ACC = 01000;
        return;
    case 0575:
        // Unknown, for *DOS.
        core.ACC = 011000;
        return;
    case 0576:
        // Unknown, for *DOS.
        core.ACC = 07000;
        return;
    case 0760:
        // Get address of СТАТУС and ИПД.
        core.ACC = 0'0000'4000'0000'3000;
        return;
    case 0761:
        // Get address of INFBA.
        core.ACC = 04000;
        return;
    case 0762:
        // Get address of uknown table, for *DOS.
        core.ACC = 010000;
        return;
    case 0764:
        // Get version of Dubna OS.
        // core.ACC = 0'4050'0507'5341'2173; // return =R1.01
        core.ACC = 0'4050'1217'2702'4366; // return =R1.02
        return;
    case 0766:
        // Return 'OC ДYБ'.
        core.ACC = 0'2364'1440'3105'4542;
        return;
    case 01002:
        // Get drum/track of user info.
        core.ACC = 0710003;
        return;
    case 02000:
        // Get process id in upper 6 bits.
        core.ACC = 0;
        return;
    case 03000:
        // Get Ш/CЛYЖ.
        core.ACC = 0;
        return;
    case 03001:
        // Unknown?
        core.ACC = 0;
        return;
    case 03005:
        // Unknown, for *DOS
        core.ACC = 0;
        return;
    case 04000:
        // Get INFBA.
        core.ACC = 0;
        return;
    case 05001:
        // Get field SC/CONC of A/LINKP
        core.ACC = 0;
        return;
    case 011000:
    case 011001:
    case 011002:
    case 011003:
    case 011004:
    case 011005:
    case 011006:
    case 011007:
    case 011010:
    case 011011:
    case 011012:
    case 011013:
    case 011014:
    case 011015:
    case 011016:
    case 011017:
        // Unknown, for *DOS.
        core.ACC = 0;
        return;
    default:
        if (addr >= 06000 && addr < 06000 + 128) {
            // Read entry from ALLTOISO encoding table.
            core.ACC = all_to_iso[addr - 06000];
            return;
        }
        throw Exception("Unimplemented extracode *65 " + to_octal(addr));
    }
}

//
// Extracode 067: debug service.
//
void Processor::e67()
{
    auto word = machine.mem_load(core.M[016]);
    core.PC = (word >> 24) & 077777;
}

#if 0
static void print_instruction_word(unsigned addr, Word word)
{
    std::cerr << "--- [" << to_octal(addr) << "] = ";
    besm6_print_instruction_mnemonics(std::cerr, (word >> 24) & BITS(24));
    std::cerr << ", ";
    besm6_print_instruction_mnemonics(std::cerr, word & BITS(24));
    std::cerr << std::endl;
}
#endif

//
// Extracode 076: call routine in kernel mode?
//
void Processor::e76()
{
    auto addr = core.M[016];
    switch (addr) {
    case 0:
        // Cancel something.
        return;
    case 1:
        // Enable something.
        // Accumulator has some key 3053 4576 1634 0112.
        return;
    default:
        if (addr >= 10) {
            // Print warning.
            //std::cerr << "\n--- Ignore extracode *76 " + to_octal(addr) << std::endl;
            //print_instruction_word(addr, machine.mem_load(addr));
            //print_instruction_word(addr + 1, machine.mem_load(addr + 1));
            return;
        }
        throw Exception("Unimplemented extracode *76 " + to_octal(addr));
    }
}

//
// Extracode 057.
//
void Processor::e57()
{
    auto addr = core.M[016];
    switch (addr) {
    case 0:
        core.ACC = besm6_floor(core.ACC);
        return;
    case 2:
        // Unknown, for Grafor direct.
        core.ACC = 0;
        return;
    case 3:
    case 7:
        // Delay the task, presumably waiting for tape to be installed by operator.
        throw Exception("Task paused waiting for tape");
    case 5:
        // Unknown, for Forex.
        core.ACC = 0;
        return;
    case 077777:
        // Manage discs and files.
        e57_file();
        return;
    default:
        if (addr >= 010) {
            // Manage tapes.
            e57_tape();
            return;
        }
        throw Exception("Unimplemented extracode *57 " + to_octal(addr));
    }
}

//
// Extracode 057 10...7777: mount tapes.
//
void Processor::e57_tape()
{
    // Modes of *57 in address field.
    enum {
        NOTFOUND = 010,   // печать 'нет магнитной ленты'
        BUSY     = 020,   // печать 'занят магнитофон'
        READY    = 040,   // печать 'нe гoтов магнитофон'
        WRITE    = 0100,  // печать 'запись'
        READ     = 0200,  // печать 'чтение'
        NODIAG   = 0400,  // блокировка любой печати
        BYNAME   = 01000, // поиск только по имени (без N бобины)
        ASSIGN   = 02000, // поиск c захватом ленты (мат.номер ленты - в 13 регистре)
        RELEASE  = 04000, // отказ от cвоиx лент, заданныx на сумматоре битовой шкалой:
    };                    // 48 разряд - лента 30 для мат.задач
    auto addr = core.M[016];

    if (addr & ASSIGN) {
        //
        // Mount tape (by name and number) on given disk number.
        // Disk number is provided in register #13.
        //
        bool write_permit = addr & WRITE;
        machine.disk_mount(core.M[015], core.ACC, write_permit);
        core.ACC = core.M[015];
        return;
    }

    if (addr & RELEASE) {
        //
        // Release tapes according to bitmask on accumulator.
        //
        if (addr & READY) {
            // Flush output buffers?
        } else {
            machine.disk_release(core.ACC);
        }
        core.ACC = 0;
        return;
    }

    //
    // Find mounted tape (by name and number).
    // Return disk number in range 030-077.
    //
    core.ACC = machine.disk_find(core.ACC);
}

//
// Extracode 057 77777: mount files.
//
void Processor::e57_file()
{
    // Use control word at accumulator.
    E57_File_Info info;
    info.word = core.ACC;

    machine.trace_e57_file(info);

    if ((info.word & E57_File_Info::KEY_BITMASK) != E57_File_Info::KEY_VALUE) {
        throw Exception("Wrong access key in *57 77777");
    }
    switch (info.field.op) {
    case E57_File_Info::VOLUME_OPEN:
        // Disk name is located at addr+1.
        // Accept any disk name for now.
        core.ACC = 0;
//
// заказ обычных файлов
//
// формируем стек вот с такой структурой:
//     1. признак монополии на мд               0000 0000 0000 0000
//     2. имя пакета                            4657 5700 0000 0001
//     3. запрос координат одного файла
//     4. запрос координат след.файла
//        .........................
//        запрос координат последнего файла
//        бит 48                                4000 0000 0000 0000
//
// запрос координат файла выглядит так:
//     1. имя хозяина файла                     0000 0000 0000 0000
//     2. имя файла                             2044 0522 1002 0040
//     3. поле ответа (+бит 30, если запись)    0000 0040 0000 0000
//     4. бит 48 + мат.номер файла в 1:6 р.     4000 0000 0000 0041
//
        break;
    case E57_File_Info::VOLUME_RELEASE:
        throw Exception("Extracode *57 77777: operation 'Release Volume' not supported yet");
        break;
    case E57_File_Info::FILE_SEARCH:
        // File name is located at addr.
        //throw Exception("Extracode *57 77777: operation 'Search File' not supported yet");
        // Accept for now.
        core.ACC = 0;
//
// в поле ответа для каждого файла
// будут его координаты на дп
// а также признаки всяких бяк:
//     вiт48 - плохое обращение
//     вiт47 - файла нет на дп
//     вiт46 - нет доступа к файлу
//
        break;
    case E57_File_Info::FILE_OPEN:
        // File name is located at addr.
        //throw Exception("Extracode *57 77777: operation 'Open File' not supported yet");
        // Accept for now.
        core.ACC = 0;
//
// формируется стек для похода на заказ файлов (т=3)
//     1. имя дп
//     2. координаты первого файла (берутся из поля ответа предыдущего стека + мат.номер в 37:42 р.)
//        ...........................
//        координаты последнего файла
//        нулевая ячейка
//
// в ячейках по файлам:
//     бит 43 - нет пакета
//     бит 44 - занят мат.номер
//     бит 45 - занят файл
//     бит 46 - нет места в fата
//
        break;
    case E57_File_Info::SCRATCH_OPEN:
        throw Exception("Extracode *57 77777: operation 'Open Scratch' not supported yet");
//
// структура инф.поля такова:
//     1. хар-ка sсr-файла
//          1:5 - кол-во экстентов
//          37:42 - мат.номер
//     2. хар-ка следующего файла
//        ........................
//        нулевая ячейка
//
        break;
    case E57_File_Info::FILE_RELEASE:
        throw Exception("Extracode *57 77777: operation 'Release File' not supported yet");
        break;
    case E57_File_Info::ALL_RELEASE:
        throw Exception("Extracode *57 77777: operation 'Release All' not supported yet");
        break;
    case E57_File_Info::FILE_CONTROL:
        // File name is located at addr.
        throw Exception("Extracode *57 77777: operation 'Change File Status' not supported yet");
        break;
    default:
        throw Exception("Extracode *57 77777: unknown operation " + to_octal(info.field.op));
    }
}

//
// Extracode 061: VT-340 control.
//
void Processor::e61()
{
    core.ACC = 0;
}

//
// Extracode 071: punch card r/w.
//
void Processor::e71()
{
    core.ACC = 0;
}

//
// Extracode 060: punch card control?
//
void Processor::e60()
{
    throw Exception("Unimplemented extracode *60 " + to_octal(core.M[016]));
}
