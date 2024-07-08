//
// Instruction and register tracing.
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
#include <fstream>
#include <iomanip>
#include <iostream>

#include "machine.h"

//
// Flag to enable tracing.
//
bool Machine::debug_instructions; // trace machine instuctions
bool Machine::debug_extracodes;   // trace extracodes (except e75)
bool Machine::debug_print;        // trace extracode e64
bool Machine::debug_registers;    // trace CPU registers
bool Machine::debug_memory;       // trace memory read/write
bool Machine::debug_fetch;        // trace instruction fetch

//
// Emit trace to this stream.
//
std::ofstream Machine::trace_stream;

//
// Enable trace with given modes.
//  i - trace instructions
//  e - trace extracodes
//  f - trace fetch
//  r - trace registers
//  m - trace memory read/write
//  x - trace exceptions
//
void Machine::enable_trace(const char *trace_mode)
{
    // Disable all trace options.
    debug_instructions = false;
    debug_extracodes   = false;
    debug_print        = false;
    debug_registers    = false;
    debug_memory       = false;
    debug_fetch        = false;

    if (trace_mode) {
        // Parse the mode string and enable all requested trace flags.
        for (unsigned i = 0; trace_mode[i]; i++) {
            char ch = trace_mode[i];
            switch (ch) {
            case 'i':
                debug_instructions = true;
                break;
            case 'e':
                debug_extracodes = true;
                break;
            case 'p':
                debug_print = true;
                break;
            case 'f':
                debug_fetch = true;
                break;
            case 'm':
                debug_memory = true;
                break;
            case 'r':
                debug_registers = true;
                break;
            default:
                throw std::runtime_error("Wrong trace option: " + std::string(1, ch));
            }
        }
    }
}

//
// Redirect trace output to a given file.
//
void Machine::redirect_trace(const char *file_name, const char *default_mode)
{
    if (trace_stream.is_open()) {
        // Close previous file.
        trace_stream.close();
    }
    if (file_name && file_name[0]) {
        // Open new trace file.
        trace_stream.open(file_name);
        if (!trace_stream.is_open())
            throw std::runtime_error("Cannot write to " + std::string(file_name));
    }

    if (!trace_enabled()) {
        // Set default mode.
        enable_trace(default_mode);
    }
}

std::ostream &Machine::get_trace_stream()
{
    if (trace_stream.is_open()) {
        return trace_stream;
    }
    return std::cout;
}

void Machine::close_trace()
{
    if (trace_stream.is_open()) {
        // Close output.
        trace_stream.close();
    }

    // Disable trace options.
    enable_trace("");
}

//
// Trace output
//
void Machine::print_exception(const char *message)
{
    auto &out = Machine::get_trace_stream();
    out << "--- " << message << std::endl;
}

//
// Print instruction fetch.
//
void Machine::print_fetch(unsigned addr, Word val)
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    out << "      Fetch [" << std::oct << std::setfill('0') << std::setw(5) << addr << "] = ";
    besm6_print_instruction_octal(out, (val >> 24) & BITS(24));
    out << ' ';
    besm6_print_instruction_octal(out, val & BITS(24));
    out << std::endl;

    // Restore.
    out.flags(save_flags);
}

//
// Print memory read/write.
//
void Machine::print_memory_access(unsigned addr, Word val, const char *opname)
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    out << "      Memory " << opname << " [" << std::oct << std::setfill('0') << std::setw(5)
        << addr << "] = ";
    besm6_print_word_octal(out, val);
    out << std::endl;

    // Restore.
    out.flags(save_flags);
}

//
// Print instruction address, opcode from RK and mnemonics.
//
void Processor::print_instruction()
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    out << std::oct << std::setfill('0') << std::setw(5) << core.PC << ' '
        << (core.right_instr_flag ? 'R' : 'L') << ": ";
    besm6_print_instruction_octal(out, RK);
    out << ' ';
    besm6_print_instruction_mnemonics(out, RK);
    print_executive_address();
    out << std::endl;

    // Restore.
    out.flags(save_flags);
}

//
// Print executive address of extracode, optional.
//
void Processor::print_executive_address()
{
    if (RK & ONEBIT(20)) {
        // No extracodes in long commands.
        return;
    }
    auto opcode = (RK >> 12) & 077;
    if (is_extracode(opcode)) {
        // Extracode - print executive address,
        auto reg = (RK >> 20) & 017;
        if (reg != 0 || core.apply_mod_reg != 0) {
            auto addr = RK & 07777;
            if (RK & ONEBIT(19)) {
                addr |= 070000;
            }
            if (reg > 0) {
                addr = ADDR(core.M[reg]);
            }
            if (core.apply_mod_reg) {
                addr = ADDR(addr + core.MOD);
            }

            auto &out       = Machine::get_trace_stream();
            auto save_flags = out.flags();
            out << " = " << std::oct << addr;
            out.flags(save_flags);
        }
    }
}

//
// Print changes in CPU registers.
//
void Processor::print_registers()
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    if (core.ACC != prev.ACC) {
        out << "      ACC = ";
        besm6_print_word_octal(out, core.ACC);
        out << std::endl;
    }
    if (core.RMR != prev.RMR) {
        out << "      RMR = ";
        besm6_print_word_octal(out, core.RMR);
        out << std::endl;
    }
    for (unsigned i = 0; i < 16; i++) {
        if (core.M[i] != prev.M[i]) {
            out << "      M" << std::oct << i << " = " << std::setfill('0') << std::setw(5)
                << core.M[i] << std::endl;
        }
    }
    if (core.RAU != prev.RAU) {
        out << "      RAU = " << std::oct << std::setfill('0') << std::setw(2) << core.RAU
            << std::endl;
    }
    if (core.apply_mod_reg != prev.apply_mod_reg) {
        if (core.apply_mod_reg) {
            out << "      MOD = " << std::oct << std::setfill('0') << std::setw(5)
                << core.MOD;
        } else {
            out << "      Clear MOD";
        }
        out << std::endl;
    }

    // Update previous state.
    prev = core;

    // Restore output flags.
    out.flags(save_flags);
}

//
// Print details about extracode e70.
//
void Machine::print_e70(const E70_Info &info)
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    if (info.disk.unit >= 030 && info.disk.unit < 070) {
        //
        // Disk.
        //
        const char *opname = info.disk.seek ? "Seek" :
                             info.disk.read_op ? "Read" : "Write";
        out << "      Disk " << std::oct << info.disk.unit << ' ' << opname << " [";

        unsigned addr      = info.disk.page << 10;
        unsigned last_addr = addr + 1023;
        unsigned zone      = info.disk.zone;
        out << std::setfill('0') << std::setw(5) << addr << '-' << std::setfill('0') << std::setw(5)
            << last_addr << "] = Zone " << zone << std::endl;
    } else {
        //
        // Drum.
        //
        const char *opname = info.drum.phys_io ? (info.drum.read_op ? "PhysRead" : "PhysWrite")
                                               : (info.drum.read_op ? "Read" : "Write");
        out << "      Drum " << std::oct << info.drum.unit << ' ' << opname << " [";

        unsigned addr  = info.drum.page << 10;
        unsigned tract = info.drum.tract;
        if (info.drum.sect_io == 0) {
            // Full page i/o.
            unsigned last_addr = addr + 1023;
            out << std::setfill('0') << std::setw(5) << addr << '-' << std::setfill('0')
                << std::setw(5) << last_addr << "] = Zone " << tract << std::endl;
        } else {
            // Sector i/o (1/4 of page).
            addr += info.drum.paragraph << 8;
            unsigned last_addr = addr + 255;
            unsigned sector    = info.drum.sector;
            if (info.drum.raw_sect) {
                // Raw sector index in lower bits.
                sector = info.disk.zone & 3;
                tract  = (info.disk.zone >> 2) & 037;
            }
            out << std::setfill('0') << std::setw(5) << addr << '-' << std::setfill('0')
                << std::setw(5) << last_addr << "] = Zone " << tract << " Sector " << sector
                << std::endl;
        }
    }

    // Restore.
    out.flags(save_flags);
}

//
// Print details about extracode e64.
//
void Machine::print_e64(const E64_Info &info, unsigned start_addr, unsigned end_addr)
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    out << "      Print ";
    switch (info.field.format) {
        case 0:
        case 8:
            out << "Text";
            break;
        case 1:
        case 5:
        case 9:
        case 13:
            out << "Instruction";
            break;
        case 2:
        case 10:
            out << "Octal";
            break;
        case 3:
        case 11:
            out << "Real";
            break;
        case 4:
        case 12:
            out << "ITM";
            break;
        case 6:
        case 7:
        case 14:
        case 15:
            out << "Hex";
            break;
        default:
            out << "Unknown";
            break;
    }
    out << '(' << info.field.format
        << ") " << std::oct << std::setfill('0') << std::setw(5) << start_addr
        << '-' << std::setfill('0') << std::setw(5) << end_addr
        << " offset=" << std::dec << info.field.offset;
    if (info.field.digits)
        out << " digits=" << info.field.digits;
    if (info.field.width)
        out << " width=" << info.field.width;
    if (info.field.repeat1)
        out << " repeat=" << info.field.repeat1;
    if (info.field.skip)
        out << " skip=" << info.field.skip;
    if (info.field.finish)
        out << " finish";
    out << std::endl;

    if ((info.field.format & 3) == 0) {
        // Text in Gost or ITM encoding.
        int pos = info.field.offset;
        BytePointer bp(memory, start_addr);

        out << "      Text " << ((info.field.format == 4) ? "ITM" : "Gost") << std::oct;
        while (bp.word_addr) {
            if (end_addr && bp.word_addr == end_addr + 1) {
                goto done;
            }

            unsigned c = bp.get_byte();
            out << ' ' << std::setfill('0') << std::setw(3) << c;

            switch (info.field.format) {
            case 0:
            case 8:
                //
                // Gost encoding.
                //
                switch (c) {
                case GOST_END_OF_INFORMATION:
                case 0231:
                case GOST_EOF:
                    goto done;
                case 0201: // new page
                    if (pos > 0) {
                        pos = 0;
                    }
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
                    out << ' ' << std::setfill('0') << std::setw(3) << c;
                    pos = c % 128;
                    break;
                default:
                    // No space left on the line.
                    if (pos == 128) {
                        goto done;
                    }
                    ++pos;
                    if (pos == 128) {
                        // No space left on the line.
                        out << '/';
                        if (end_addr) {
                            pos = 0;
                        }
                    }
                    break;
                }
                break;
            case 4:
            case 12:
                //
                // ITM encoding.
                //
                switch (c) {
                case 0140: // end of information
                    goto done;
                case 0173: // repeat last symbol
                    c = bp.get_byte();
                    out << ' ' << std::setfill('0') << std::setw(3) << c;
                    if (c == 040) {
                        pos = 0;
                    } else {
                        pos += c & 017;
                    }
                    break;
                default:
                    // No space left on the line.
                    if (!end_addr && pos == 128) {
                        goto done;
                    }
                    ++pos;
                    if (pos == 128) {
                        // No space left on the line.
                        out << '/';
                        pos = 0;
                    }
                    break;
                }
                break;
            }
        }
done:
        out << std::endl;
    }

    // Restore.
    out.flags(save_flags);
}

//
// Print details about extracode e64 in Dubna mode.
//
void Machine::print_e64_dubna(unsigned start_addr, unsigned end_addr)
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    out << "      Print Dubna " << std::oct << std::setfill('0') << std::setw(5) << start_addr
        << '-' << std::setfill('0') << std::setw(5) << end_addr << std::dec << std::endl;

    // Text in Gost encoding.
    out << "      Text Gost" << std::oct;

    BytePointer bp(memory, start_addr);
    while (bp.word_addr) {
        if (end_addr && bp.word_addr == end_addr + 1) {
            goto done;
        }

        unsigned c = bp.get_byte();
        out << ' ' << std::setfill('0') << std::setw(3) << c;

        if (c == 0176) {
            goto done;
        }
    }
done:
    out << std::endl;

    // Restore.
    out.flags(save_flags);
}

//
// Print details about extracode e57 77777.
//
void Machine::print_e57_file(const E57_File_Info &info)
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    switch (info.field.op) {
    case E57_File_Info::VOLUME_OPEN:
        out << "      Open Volume";
        break;
    case E57_File_Info::VOLUME_RELEASE:
        out << "      Release Volume";
        break;
    case E57_File_Info::FILE_SEARCH:
        out << "      Search File";
        break;
    case E57_File_Info::FILE_OPEN:
        out << "      Open File";
        break;
    case E57_File_Info::SCRATCH_OPEN:
        out << "      Open Scratch";
        break;
    case E57_File_Info::FILE_RELEASE:
        out << "      Release File";
        break;
    case E57_File_Info::ALL_RELEASE:
        out << "      Release All";
        break;
    case E57_File_Info::FILE_CONTROL:
        out << "      Change File Status";
        break;
    default:
        out << "      Unknown";
        break;
    }
    out << " Addr=" << std::setfill('0') << std::setw(5) << std::oct << info.field.addr
        << " Flags=" << info.field.flags
        << " Key=" << ((info.word & E57_File_Info::KEY_BITMASK) >> 18)
        << std::endl;

    // Restore.
    out.flags(save_flags);
}
