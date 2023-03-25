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

class Machine {
private:
    Memory &memory;

    // Simulate this number of instructions.
    uint64_t instr_limit{ DEFAULT_LIMIT };

    // Internal flag to stop the simulation.
    bool is_halted{ false };

    // Enable a progress message to stderr.
    bool progress_message_enabled{ false };

    // Every few seconds, print a message to stderr, to track the simulation progress.
    void show_progress();

    // Time of last check.
    std::chrono::time_point<std::chrono::steady_clock> progress_time_last;

    // Last instr_count when progress message was printed.
    uint64_t progress_count{ 0 };

    // The trace flag.
    static bool trace_flag;
    static std::ofstream trace_stream;

    // Static stuff.
    static const uint64_t DEFAULT_LIMIT;    // Limit of instructions to simulate, by default
    static bool verbose;                    // Verbose flag for tracing
    static uint64_t simulated_instructions; // Count of instructions

public:
    explicit Machine(Memory &memory);

    // Get reference to memory.
    Memory &get_memory() { return memory; }

    // Load job input into machine.
    void load(const std::string &filename);
    void load(std::istream &input);

    // Run simulation.
    bool run();

    // Simulate one instruction.
    bool advance();

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
    static void enable_trace(const char *file_name);
    static void close_trace();
    static bool trace_enabled() { return trace_flag; }

    // Emit trace to this stream.
    static std::ostream &get_trace_stream();

    // Backdoor access to memory.
    // No tracing.
    void mem_write(const Words &input, unsigned addr) { memory.debug_write(input, addr); }
    void mem_read(Words &output, unsigned nrows, unsigned addr) { memory.debug_read(output, nrows, addr); }
};

#endif // DUBNA_MACHINE_H
