//
// Implement BESM-6 machine.
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
#include "disk.h"
#include "drum.h"

class Machine {
private:
    // Disks and drums.
    std::array<std::unique_ptr<Disk>, NDISKS> disks;
    std::array<std::unique_ptr<Drum>, NDRUMS> drums;

    unsigned mapped_disk{};
    unsigned mapped_drum{};

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

    // Dump disk data for debug.
    bool dump_io_flag{}; // set to true to dump all disk reads
    unsigned dump_serial_num{};

    // Path to disk images, semicolon separated.
    std::string disk_search_path;

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

    // Destructor.
    ~Machine();

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

    // Memory access.
    Word mem_fetch(unsigned addr);
    Word mem_load(unsigned addr);
    void mem_store(unsigned addr, Word val);

    // Disk i/o.
    void disk_io(char op, unsigned disk_unit, unsigned zone, unsigned sector, unsigned addr, unsigned nwords);
    void disk_mount(unsigned disk, const std::string &filename, bool write_permit);
    std::string disk_find(const std::string &filename);

    // Drum i/o.
    void drum_io(char op, unsigned drum_unit, unsigned zone, unsigned sector, unsigned addr, unsigned nwords);
    void drum_init(unsigned drum_unit);
    void drum_write_word(unsigned drum_unit, unsigned offset, Word value);
    Word drum_read_word(unsigned drum_unit, unsigned offset);
    void drum_write_cosy(unsigned drum_unit, unsigned &offset, const std::string &line);

    // "Phys.io": redirect drum r/w to disk.
    void map_drum_to_disk(unsigned drum, unsigned disk);
    unsigned get_mapped_disk() const { return mapped_disk; }
    unsigned get_mapped_drum() const { return mapped_drum; }

    // Bootstrap the Monitoring System Dubna.
    void boot_ms_dubna();

    //
    // Trace methods.
    //
    static void trace_exception(const char *message)
    {
        if (trace_enabled())
            print_exception(message);
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

    void trace_instruction(unsigned opcode)
    {
        // Print e50...e77 except e75, and also e20, e21.
        if (debug_instructions ||
            (debug_extracodes && opcode != 075 && is_extracode(opcode)))
            cpu.print_instruction();
    }

    void trace_registers()
    {
        if (debug_registers)
            cpu.print_registers();
    }

    void trace_e70(const E70_Info &info)
    {
        if (debug_extracodes)
            print_e70(info);
    }

    static void print_exception(const char *message);
    static void print_fetch(unsigned addr, Word val);
    static void print_memory_access(unsigned addr, Word val, const char *opname);
    static void print_e70(const E70_Info &info);
};

#endif // DUBNA_MACHINE_H
