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
#include <iostream>
#include <fstream>
#include "machine.h"

//
// Flag to enable tracing.
//
bool Machine::debug_instructions; // trace machine instuctions
bool Machine::debug_extracodes;   // trace extracodes (except e75)
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
    debug_extracodes = false;
    debug_registers = false;
    debug_memory = false;
    debug_fetch = false;

    if (trace_mode) {
        // Parse the mode string and enable all requested trace flags.
        for (unsigned i = 0; trace_mode[i]; i++) {
            char ch = trace_mode[i];
            switch (ch) {
            case 'i': debug_instructions = true; break;
            case 'e': debug_extracodes = true; break;
            case 'f': debug_fetch = true; break;
            case 'm': debug_memory = true; break;
            case 'r': debug_registers = true; break;
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

#if 0
//
// Print 48-bit value as octal.
//
static void print_48bits(FILE *of, Word value)
{
    fprintf(of, "%04o %04o %04o %04o",
        (int) (value >> 36) & 07777,
        (int) (value >> 24) & 07777,
        (int) (value >> 12) & 07777,
        (int) value & 07777);
}
#endif
//
// Trace output
//
void Machine::print_exception(const char *message)
{
    auto &out = Machine::get_trace_stream();
    out << "cpu --- " << message << std::endl;
}

//
// Print changes in CPU registers.
//
void Machine::print_registers()
{
    //TODO: print changed registers
#if 0
    auto &out = Machine::get_trace_stream();
    out << "cpu ???" << std::endl;
    int i;

    if (cpu->core.ACC != cpu->prev.ACC) {
        fprintf(cpu->log_output, "cpu       Write ACC = ");
        print_48bits(cpu->log_output, cpu->core.ACC);
        fprintf(cpu->log_output, "\n");
    }
    if (cpu->core.RMR != cpu->prev.RMR) {
        fprintf(cpu->log_output, "cpu       Write RMR = ");
        print_48bits(cpu->log_output, cpu->core.RMR);
        fprintf(cpu->log_output, "\n");
    }
    for (i = 0; i < 16; i++) {
        if (cpu->core.M[i] != cpu->prev.M[i])
            fprintf(cpu->log_output, "cpu       Write M%o = %05o\n", i, cpu->core.M[i]);
    }
    if (cpu->core.RAU != cpu->prev.RAU)
        fprintf(cpu->log_output, "cpu       Write RAU = %02o\n", cpu->core.RAU);
    if (cpu->core.apply_mod_reg != cpu->prev.apply_mod_reg) {
        if (cpu->core.apply_mod_reg) {
            fprintf(cpu->log_output, "cpu       Write MOD = %03o\n", cpu->core.M[020]);
        } else {
            fprintf(cpu->log_output, "cpu       Clear MOD\n");
        }
    }
    cpu->prev = cpu->core;
#endif
}

void Machine::print_instruction()
{
    // Print instruction address, opcode from core.RK and mnemonics.
#if 0
    auto &out = Machine::get_trace_stream();
    out << "cpu --- Exception" << std::endl;
    fprintf(cpu->log_output, "cpu %05o %07o %c: ", cpu->core.PC, paddr,
        (cpu->core.RUU & RUU_RIGHT_INSTR) ? 'R' : 'L');
    print_insn(cpu->log_output, cpu->RK);
    fprintf(cpu->log_output, " ");
    print_cmd(cpu->log_output, cpu->RK);
    fprintf(cpu->log_output, "\n");
#endif
}

void Machine::print_fetch(unsigned addr, Word val)
{
    //TODO: print fetch
#if 0
    fprintf(log_output, "cpu       Fetch [%05o] = ", addr);
    besm6_print_insn(log_output, (val >> 24) & BITS(24));
    besm6_print_insn(log_output, val & BITS(24));
    fprintf(log_output, "\n");
#endif
}

void Machine::print_memory_access(unsigned addr, Word val, const char *opname)
{
    //TODO: print memory read/write
#if 0
    fprintf(log_output, "cpu       Memory %s [%05o] = ", opname, addr);
    print_48bits(log_output, val);
    fprintf(log_output, "\n");
#endif
}
