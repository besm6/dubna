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

#include "machine.h"

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

    case 057: // Mount tapes.
        e57();
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
                unsigned disk_unit = machine.get_mapped_disk() - 030;
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
    }
}

//
// Extracode 050: elementary math functions and other services.
//
void Processor::e50()
{
    switch (core.M[016]) {
    case 064:
        // Print some message.
        // TODO: print_iso(ADDR(core.ACC));
        break;
    case 067:
        // DATE*, OS Dubna specific.
        core.ACC = 0'7707'7774'0000'0000; // mask
        break;
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
    default:
        throw Exception("Unimplemented extracode *50 " + to_octal(core.M[016]));
    }
}

//
// Extracode 063: manage time limit.
//
void Processor::e63()
{
    switch (core.M[016]) {
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
    switch (core.M[016]) {
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
    case 0560:
        // Get address of A/LINKP.
        core.ACC = 05000;
        return;
    case 0564:
        // Get address of user table.
        core.ACC = 01000;
        return;
    case 0760:
        // Get address of СТАТУС and ИПД.
        core.ACC = 0'0000'4000'0000'3000;
        return;
    case 0761:
        // Get address of INFBA.
        core.ACC = 04000;
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
    case 04000:
        // Get INFBA.
        core.ACC = 0;
        return;
    case 05001:
        // Get field SC/CONC of A/LINKP
        core.ACC = 0;
        return;
    default:
        throw Exception("Unimplemented extracode *65 " + to_octal(core.M[016]));
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
            std::cerr << "--- Ignore extracode *76 " + to_octal(core.M[016]) << std::endl;
            return;
        }
        throw Exception("Unimplemented extracode *76 " + to_octal(addr));
    }
}

//
// Extracode 057: mount tapes.
//
void Processor::e57()
{
    switch (core.M[016]) {
    case 03:
        // Some delay.
        core.ACC = 0;
        throw Exception("Task paused waiting for tape");
    case 050:
        // Unknown.
        core.ACC = 0;
        return;
    case 0400:
        // Ask for tape by name?
        core.ACC = 0; // failed
        return;
    case 02050:
        // Mount tape.
        core.ACC = 0; // failed
        return;
    case 04040:
        // Request tapes bitmask on accumulator.
        // core.ACC = BITS48;
        core.ACC = 0;
        return;
    default:
        throw Exception("Unimplemented extracode *57 " + to_octal(core.M[016]));
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
