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
#ifndef DUBNA_MACHINE_H
#define DUBNA_MACHINE_H

#include "memory.h"
#include "processor.h"

class Machine {
private:
    // Simulate this number of instructions.
    uint64_t instr_limit{ DEFAULT_LIMIT };

    // Enable a progress message to stderr.
    bool progress_message_enabled{ false };

    // Every few seconds, print a message to stderr, to track the simulation progress.
    void show_progress();

    // Time of last check.
    std::chrono::time_point<std::chrono::steady_clock> progress_time_last;

    // Last instr_count when progress message was printed.
    uint64_t progress_count{ 0 };

    // Trace output.
    static std::ofstream trace_stream;

    // Trace modes.
    static bool debug_instructions; // trace machine instuctions
    static bool debug_extracodes;   // trace extracodes (except e75)
    static bool debug_registers;    // trace CPU registers
    static bool debug_memory;       // trace memory read/write
    static bool debug_fetch;        // trace instruction fetch

    // Static stuff.
    static const uint64_t DEFAULT_LIMIT;    // Limit of instructions to simulate, by default
    static bool verbose;                    // Verbose flag for tracing
    static uint64_t simulated_instructions; // Count of instructions

public:
    // 32K words of virtual memory.
    Memory &memory;

    // BESM-6 processor.
    Processor cpu;

    // Constructor.
    explicit Machine(Memory &memory);

    // Load job input into machine.
    void load(const std::string &filename);
    void load(std::istream &input);

    // Run simulation.
    void run();

    // Enable a progress message to stderr.
    void enable_progress_message(bool on) { progress_message_enabled = on; }

    // Get instruction count.
    static uint64_t get_instr_count() { return simulated_instructions; }
    static void incr_simulated_instructions() { simulated_instructions++; }

    // Limit the simulation to this number of instructions.
    void set_limit(uint64_t count) { instr_limit = count; }
    static uint64_t get_default_limit() { return DEFAULT_LIMIT; }

    // Verbose flag for tracing.
    static void set_verbose(bool on) { verbose = on; }
    static bool get_verbose() { return verbose; }

    // Enable trace output to the given file,
    // or to std::cout when filename not present.
    static void enable_trace(const char *mode);
    static void redirect_trace(const char *file_name, const char *default_mode);
    static void close_trace();
    static bool trace_enabled() { return debug_instructions | debug_extracodes | debug_registers |
                                         debug_memory | debug_fetch; }

    // Emit trace to this stream.
    static std::ostream &get_trace_stream();

    //
    // Trace methods.
    //
    static void trace_exception(const char *message)
    {
        if (trace_enabled())
            print_exception(message);
    }

    static void trace_registers()
    {
        if (debug_registers)
            print_registers();
    }

    static void trace_instruction(unsigned opcode)
    {
        // Print e50...e77 except e75, and also e20, e21.
        if (debug_instructions ||
            (debug_extracodes && opcode != 075 && is_extracode(opcode)))
            print_instruction();
    }

    static void trace_fetch(unsigned addr, Word val)
    {
        if (debug_fetch)
            print_fetch(addr, val);
    }

    static void trace_memory_write(unsigned addr, Word val)
    {
        if (debug_memory)
            print_memory_access(addr, val, "Write");
    }

    static void trace_memory_read(unsigned addr, Word val)
    {
        if (debug_memory)
            print_memory_access(addr, val, "Read");
    }

    static void print_exception(const char *message);
    static void print_registers();
    static void print_instruction();
    static void print_fetch(unsigned addr, Word val);
    static void print_memory_access(unsigned addr, Word val, const char *opname);
};

#endif // DUBNA_MACHINE_H
