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

//
// Execute extracode.
//
void Processor::extracode(unsigned opcode)
{
    switch (opcode) {
    case 070:
        // Disk or drum i/o.
        e70();
        break;

    case 074:
        // Finish the job.
        throw Exception("");

    default:
        throw Exception("Unimplemented extracode");
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

    char     op   = info.disk.read_op ? 'r' : 'w';
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
        unsigned tract = info.drum.tract;
        unsigned sector = info.drum.sector;
        if (info.drum.raw_sect & info.drum.sect_io) {
            // Raw sector index in lower bits.
            sector = info.disk.zone & 3;
            tract = (info.disk.zone >> 2) & 037;
        }

        if (info.drum.sect_io) {
            addr += info.drum.paragraph << 8;
        }

        if (info.drum.phys_io) {
            // Remap to disk drive.
            unsigned disk_unit = machine.get_mapped_disk() - 030;
            unsigned zone = tract + (info.drum.unit - machine.get_mapped_drum()) * 040;

            if (info.drum.sect_io == 0) {
                // Full page i/o with disk.
                machine.disk_io(op, disk_unit, zone, 0, addr, 1024);
            } else {
                // Sector i/o with disk (1/4 of page).
                machine.disk_io(op, disk_unit, zone, sector, addr, 256);
            }
        } else if (info.drum.sect_io == 0) {
            // Full page i/o.
            machine.drum_io(op, info.drum.unit & 037, tract, 0, addr, 1024);
        } else {
            // Sector i/o (1/4 of page).
            machine.drum_io(op, info.drum.unit & 037, tract, sector, addr, 256);
        }
    }
}
