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
    case 0575:
        // Unknown, for DIPOL.
        core.ACC = 05000;
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
// Extracode 061: VT-340 control.
//
void Processor::e61()
{
    auto addr = core.M[016];
    switch (addr) {
    case 077777: {
        // Output to Watanabe or Tektronix plotter.
        BytePointer bp(memory, ADDR(core.ACC));
        switch (core.ACC >> 36) {
        case 0:
            // Watanabe.
            for (;;) {
                char ch = bp.get_byte();
                if (ch == 0)
                    break;
                machine.plotter.watanabe_putch(ch);
            }
            break;
        case 01400:
            // Tektronix.
            if (bp.word_addr == 0) {
                // Start new command.
            } else {
                for (;;) {
                    char ch = bp.get_byte();
                    if (ch == 0)
                        break;
                    machine.plotter.tektronix_putch(ch);
                }
            }
            break;
        default:
            throw Exception("Extracode *61 77777: unknown target " + to_octal(core.ACC >> 36));
        }
        core.ACC = 0;
        return;
    }
    default:
        // Unknown.
        core.ACC = 0;
    }
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
