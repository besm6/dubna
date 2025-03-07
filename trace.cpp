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
#include <sstream>

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
bool Machine::debug_dispak;       // trace in dispak format, to stderr

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
//  d - trace in dispak format, to stderr
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
            case 'e':
                debug_extracodes = true;
                break;
            case 'm':
                debug_memory = true;
                break;
            case 'i':
                debug_instructions = true;
                break;
            case 'r':
                debug_registers = true;
                break;
            case 'f':
                debug_fetch = true;
                break;
            case 'p':
                debug_print = true;
                break;
            case 'd':
                debug_dispak = true;
                break;
            default:
                throw std::runtime_error("Wrong trace option: " + std::string(1, ch));
            }
        }
    }
}

//
// Enable trace by bitmask,
// for example by VTM instruction with register 0.
//
void Machine::enable_trace(unsigned bitmask)
{
    debug_extracodes   = bitmask & 01;   // -d e
    debug_memory       = bitmask & 02;   // -d m
    debug_instructions = bitmask & 04;   // -d i
    debug_registers    = bitmask & 010;  // -d r
    debug_fetch        = bitmask & 020;  // -d f
    debug_print        = bitmask & 040;  // -d p
    debug_dispak       = bitmask & 0100; // -d d
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
// Print memory write in dispak format.
// "       00016: store 0003770010053377"
//
void Machine::print_memory_write_dispak(unsigned addr, Word val)
{
    auto &out       = std::cerr;
    auto save_flags = out.flags();

    out << std::oct << "       " << std::setfill('0') << std::setw(5) << addr << ": store "
        << std::setw(16) << val << '\n';
    out.flags(save_flags);
}

//
// Print instruction address, opcode from RK and mnemonics.
//
void Processor::print_instruction()
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    e64_finish();
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
// Print instruction in dispak format.
// "03652: xta 2157(1)          (=6040000000065366) acc=2031463100000000 r[1]=00141"
//
void Processor::print_instruction_dispak()
{
    auto &out       = std::cerr;
    auto save_flags = out.flags();
    auto reg        = RK >> 20;
    auto addr =
        (RK & ONEBIT(20)) ? (RK & BITS(15)) : ((RK & BITS(12)) | ((RK & ONEBIT(19)) ? 070000 : 0));
    auto word = machine.mem_load(ADDR(addr + core.M[reg]));
    std::ostringstream buf;

    besm6_print_instruction_mnemonics(buf, RK);
    out << std::oct << std::setfill('0') << std::setw(5) << core.PC << ": " << std::setfill(' ')
        << std::setw(20) << std::left << buf.str() << " (=" << std::right << std::setfill('0')
        << std::setw(16) << word << ") acc=" << std::setw(16) << core.ACC;
    if (reg) {
        out << " r[" << reg << "]=" << std::setw(5) << core.M[reg];
    }
    out << '\n';
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
            out << "      MOD = " << std::oct << std::setfill('0') << std::setw(5) << core.MOD;
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
        const char *opname = info.disk.seek ? "Seek" : info.disk.read_op ? "Read" : "Write";
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
    out << '(' << info.field.format << ") " << std::oct << std::setfill('0') << std::setw(5)
        << start_addr << '-' << std::setfill('0') << std::setw(5) << end_addr
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
void Machine::print_e57_request(const E57_Request_Info &info)
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    out << "      Op=" << info.field.op << " Addr=" << std::setfill('0') << std::setw(5) << std::oct
        << info.field.addr << " Flags=" << info.field.flags
        << " Key=" << ((info.word & E57_Request_Info::KEY_BITMASK) >> 18) << '\n';
    switch (info.field.op) {
    case E57_Request_Info::VOLUME_OPEN:
        out << "      Open Volume " << tape_name_string(mem_load(info.field.addr + 1)) << '\n';
        break;
    case E57_Request_Info::VOLUME_RELEASE:
        out << "      Release Volume\n";
        break;
    case E57_Request_Info::FILE_SEARCH:
        out << "      Search File '" << word_iso_string(mem_load(info.field.addr + 2))
            << "' on disk " << tape_name_string(mem_load(info.field.addr)) << '\n';
        break;
    case E57_Request_Info::FILE_OPEN:
        out << "      Open File " << tape_name_string(mem_load(info.field.addr)) << '\n';
        break;
    case E57_Request_Info::SCRATCH_OPEN:
        out << "      Open Scratch\n";
        break;
    case E57_Request_Info::FILE_RELEASE:
        out << "      Release File\n";
        break;
    case E57_Request_Info::ALL_RELEASE:
        out << "      Release All\n";
        break;
    case E57_Request_Info::FILE_CONTROL:
        out << "      Change File Status\n";
        break;
    }
    // Restore.
    out.flags(save_flags);

    out << "      Info0=";
    besm6_print_word_octal(out, mem_load(info.field.addr));
    out << '\n';
    out << "      Info1=";
    besm6_print_word_octal(out, mem_load(info.field.addr + 1));
    out << '\n';
    if (info.field.op == E57_Request_Info::VOLUME_OPEN)
        return;
    out << "      Info2=";
    besm6_print_word_octal(out, mem_load(info.field.addr + 2));
    out << '\n';
    out << "      Info3=";
    besm6_print_word_octal(out, mem_load(info.field.addr + 3));
    out << '\n';
    out << "      Info4=";
    besm6_print_word_octal(out, mem_load(info.field.addr + 4));
    out << '\n';
    out << "      Info5=";
    besm6_print_word_octal(out, mem_load(info.field.addr + 5));
    out << '\n';
}

void Machine::print_e57_search(const E57_Search_Info &info)
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    out << "      File='" << word_iso_string(info.field.file_name) << "' Disk=" << std::setw(2)
        << std::oct << info.field.disk_unit << std::dec
        << " Mode=" << (info.field.write_mode ? 'W' : 'R') << " Owner='"
        << word_iso_string(info.field.owner) << "'\n";
    out.flags(save_flags);
}

void Machine::print_e57_open(const E57_Open_Info &info)
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    out << "      File=" << info.field.offset << " Disk=" << std::setw(2) << std::oct
        << info.field.disk_unit << std::dec << " Mode=" << (info.field.write_mode ? 'W' : 'R')
        << "\n";
    out.flags(save_flags);
}

void Machine::print_e57_scratch(const E57_Scratch_Info &info)
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    out << "      Disk=" << std::setw(2) << std::oct << info.field.disk_unit << " Size=" << std::dec
        << info.field.size << '\n';
    out.flags(save_flags);
}

//
// Print details about extracode e50 015/016/017.
//
void Machine::print_e50_format_real(const E50_Format_Info &info)
{
    auto &out          = Machine::get_trace_stream();
    auto save_flags    = out.flags();
    unsigned src_addr  = ADDR(info.field.src_addr + cpu.get_m(info.field.src_reg));
    unsigned dest_addr = ADDR(info.field.dest_addr + cpu.get_m(info.field.dest_reg));

    out << "      Src=" << std::oct << info.field.src_addr << '(' << std::dec << info.field.src_reg
        << ')' << "=" << std::oct << src_addr << " Dest=" << std::oct << info.field.dest_addr << '('
        << std::dec << info.field.dest_reg << ')' << "=" << std::oct << dest_addr
        << " Width=" << std::dec << info.field.width << " Precision=" << info.field.precision
        << " Align=" << (info.field.right_align ? 'R' : 'L') << '\n';

    // Restore.
    out.flags(save_flags);
}

//
// Print details about extracode e71.
//
void Machine::print_e71(const E64_Pointer &info, unsigned start_addr, unsigned end_addr)
{
    auto &out       = Machine::get_trace_stream();
    auto save_flags = out.flags();

    switch (info.field.flags) {
    case 1:
        out << "      Punch";
        break;
    case 4:
        out << "      Tty Output";
        break;
    case 6:
        out << "      Tty Input";
        break;
    default:
        out << "      Unknown";
        break;
    }
    out << " (" << info.field.flags << ") " << std::oct << std::setfill('0') << std::setw(5)
        << start_addr << '-' << std::setfill('0') << std::setw(5) << end_addr << std::endl;

    BytePointer bp(memory, start_addr);
    while (bp.word_addr) {
        if (end_addr && bp.word_addr == end_addr + 1) {
            break;
        }

        unsigned c = bp.get_byte();
        out << ' ' << std::setfill('0') << std::setw(3) << c;
        if (c == '\0')
            break;
    }
    out << std::endl;

    // Restore.
    out.flags(save_flags);
}
