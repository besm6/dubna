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

#include <array>
#include <chrono>
#include <memory>
#include <vector>
#include <set>
#include <unordered_map>

#include "disk.h"
#include "drum.h"
#include "gost10859.h"
#include "plotter.h"
#include "processor.h"
#include "puncher.h"

class Machine {
private:
    // Disks and drums.
    std::array<std::unique_ptr<Disk>, NDISKS> disks;
    std::array<std::unique_ptr<Drum>, NDRUMS> drums;

    // Map drum to disk for so called phys.io.
    unsigned mapped_drum{};
    std::unique_ptr<Disk> mapped_disk;

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

    // List of all requested file names, for e57.
    std::vector<std::string> file_paths;

    // Enable wall clock and other sources of inpredictability.
    bool entropy_flag{};

    // Enable system load list from the start.
    bool system_load_list_flag{};

    // Get name of a resident program by address.
    std::unordered_map<unsigned, std::string> resident_name;

    // Addresses of resident programs.
    std::multiset<unsigned> resident_addr;

    // Load the table of resident programs from this file.
    std::string map_filename;

    // Trace output.
    static std::ofstream trace_stream;

    // Trace modes.
    static bool debug_instructions; // trace machine instuctions
    static bool debug_extracodes;   // trace extracodes (except e75)
    static bool debug_print;        // trace extracode e64
    static bool debug_registers;    // trace CPU registers
    static bool debug_memory;       // trace memory read/write
    static bool debug_fetch;        // trace instruction fetch
    static bool debug_dispak;       // trace trace in dispak format, to stderr
    bool after_call{};              // right after JVM instruction
    bool after_return{};            // right after UJ(13) instruction

    // Static stuff.
    static const uint64_t DEFAULT_LIMIT;    // Limit of instructions to simulate, by default
    static bool verbose;                    // Verbose flag for tracing
    static bool keep_temporary_files;       // Preserve scratch files and raw plotter output
    static uint64_t simulated_instructions; // Count of instructions

public:
    // 32K words of virtual memory.
    Memory &memory;

    // BESM-6 processor.
    Processor cpu;

    // Generic plotter interface.
    Plotter plotter;

    // Output to punched cards.
    Puncher puncher;

    // Pre-defined tapes.
    static const Word TAPE_MONSYS    = 055'57'56'63'71'63'00'11; // MONSYS in TEXT encoding, and number 9
    static const Word TAPE_SCRATCH   = 063'43'62'64'43'50'00'00;
    static const Word TAPE_LIBRAR_12 = 054'51'42'62'41'62'00'22;
    static const Word TAPE_LIBRAR_37 = 054'51'42'62'41'62'00'67;
    static const Word TAPE_BEMSH     = 044'51'63'60'41'43'33'31; // DISPAC 739
    static const Word TAPE_B         = 044'00'00'00'00'00'00'07;

    // Disc names
    static const Word DISC_LOCAL = 054'57'43'41'54'00'00'00;
    static const Word DISC_HOME  = 050'57'55'45'00'00'00'00;
    static const Word DISC_TMP   = 064'55'60'00'00'00'00'00;

    // Virtual disk unit for phys.io.
    static const unsigned PHYS_IO_UNIT = 0100;

    // Constructor.
    explicit Machine(Memory &memory);

    // Destructor.
    ~Machine();

    // Load script into machine.
    void load_script(const std::string &filename);
    void load_script(std::istream &input);

    // Run simulation.
    void run();

    // Save output files.
    void finish();

    void plotter_change_page() { plotter.change_page(keep_temporary_files); }

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

    // Keep temporary files.
    void preserve_temps(bool on) { keep_temporary_files = on; }

    // Use wall clock when entropy is enabled.
    bool is_entropy_enabled() { return entropy_flag; }
    void enable_entropy(bool on = true) { entropy_flag = on; }

    // System load list allows to see all loader actions.
    void enable_system_load_list(bool on = true) { system_load_list_flag = on; }

    // Enable trace output to the given file,
    // or to std::cout when filename not present.
    static void enable_trace(const char *mode);
    static void enable_trace(unsigned bitmask);
    static void redirect_trace(const char *file_name, const char *default_mode);
    static void close_trace();
    static bool trace_enabled()
    {
        return debug_instructions | debug_extracodes | debug_print | debug_registers |
               debug_memory | debug_fetch;
    }
    void set_after_call() { after_call = true; };
    void set_after_return() { after_return = true; };
    void set_map_file(const char *file_name);

    // Parse a load map as generated by Dubna linker.
    void parse_load_map(std::istream &file);

    // Emit trace to this stream.
    static std::ostream &get_trace_stream();

    // Memory access.
    Word mem_fetch(unsigned addr);
    Word mem_load(unsigned addr);
    void mem_store(unsigned addr, Word val);

    // Disk i/o.
    void disk_io(char op, unsigned disk_unit, unsigned zone, unsigned sector, unsigned addr,
                 unsigned nwords);
    std::string disk_path(Word tape_id);
    void disk_mount(unsigned disk, Word tape_id, bool write_permit);
    void disk_mount_readonly(unsigned disk, Word tape_id) { disk_mount(disk, tape_id, false); }
    void disk_release(Word mask);
    unsigned disk_find(Word tape_id);
    void scratch_mount(unsigned disk_unit, unsigned num_zones);
    unsigned file_search(Word disc_id, Word file_name, bool write_mode);
    unsigned file_mount(unsigned disk_unit, unsigned file_index, bool write_mode, unsigned file_offset = 0);

    // Drum i/o.
    void drum_io(char op, unsigned drum_unit, unsigned zone, unsigned sector, unsigned addr,
                 unsigned nwords);
    void drum_init(unsigned drum_unit);
    void drum_write_word(unsigned drum_unit, unsigned offset, Word value);
    Word drum_read_word(unsigned drum_unit, unsigned offset);
    void drum_write_cosy(unsigned drum_unit, unsigned &offset, const std::string &line);

    // "Phys.io": redirect drum r/w to disk.
    void map_drum_to_disk(unsigned drum, unsigned disk);
    unsigned get_mapped_drum() const { return mapped_drum; }

    // Bootstrap the Monitoring System Dubna.
    void boot_ms_dubna(const std::string &path = "");

    // Send message to operator's console.
    void print_iso_string(std::ostream &out, unsigned addr);

    // Check for binary program (overlay).
    // Return file offset for shebang.
    bool is_overlay(const std::string &filename, unsigned *file_offset_ptr);

    // Load binary program (overlay).
    void boot_overlay(const std::string &filename, unsigned file_offset, const std::string &path = "");

    // Load the table of resident programs, for tracing.
    void load_resident_directory(unsigned addr);

    // Find name of resident routine, by address.
    // Set at_start to true when address matches the start of the routine.
    bool find_resident_name(unsigned addr, std::string &name, bool &at_start);

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

    static void trace_memory_write_dispak(unsigned addr, Word val)
    {
        if (debug_dispak)
            print_memory_write_dispak(addr, val);
    }

    static void trace_memory_read(unsigned addr, Word val)
    {
        if (debug_memory)
            print_memory_access(addr, val, "Read");
    }

    void trace_instruction(unsigned opcode)
    {
        // Print e50...e77 except e75, and also e20, e21.
        if (debug_instructions || (debug_extracodes && opcode != 075 && is_extracode(opcode)))
            cpu.print_instruction();
        if (debug_dispak)
            cpu.print_instruction_dispak();
    }

    void trace_calls_returns()
    {
        if (debug_instructions)
            print_calls_returns();
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

    void trace_e64(const E64_Info &info, unsigned start_addr, unsigned end_addr)
    {
        if (debug_print)
            print_e64(info, start_addr, end_addr);
    }

    void trace_e71(const E64_Pointer &info, unsigned start_addr, unsigned end_addr)
    {
        if (debug_extracodes)
            print_e71(info, start_addr, end_addr);
    }

    void trace_e64_dubna(unsigned start_addr, unsigned end_addr)
    {
        if (debug_print)
            print_e64_dubna(start_addr, end_addr);
    }

    void trace_e57_request(const E57_Request_Info &info)
    {
        if (debug_extracodes)
            print_e57_request(info);
    }

    void trace_e57_scratch(const E57_Scratch_Info &info)
    {
        if (debug_extracodes)
            print_e57_scratch(info);
    }

    void trace_e57_search(const E57_Search_Info &info)
    {
        if (debug_extracodes)
            print_e57_search(info);
    }

    void trace_e57_open(const E57_Open_Info &info)
    {
        if (debug_extracodes)
            print_e57_open(info);
    }

    void trace_e50_format_real(const E50_Format_Info &info)
    {
        if (debug_extracodes)
            print_e50_format_real(info);
    }

    static void print_exception(const char *message);
    static void print_fetch(unsigned addr, Word val);
    static void print_memory_access(unsigned addr, Word val, const char *opname);
    static void print_memory_write_dispak(unsigned addr, Word val);
    static void print_e70(const E70_Info &info);
    void print_e64(const E64_Info &info, unsigned start_addr, unsigned end_addr);
    void print_e64_dubna(unsigned start_addr, unsigned end_addr);
    void print_e57_request(const E57_Request_Info &info);
    void print_e57_scratch(const E57_Scratch_Info &info);
    void print_e57_search(const E57_Search_Info &info);
    void print_e57_open(const E57_Open_Info &info);
    void print_e50_format_real(const E50_Format_Info &info);
    void print_e71(const E64_Pointer &info, unsigned start_addr, unsigned end_addr);
    void print_calls_returns();
};

//
// Return tape name as string.
//
std::string tape_name_string(Word w);

//
// Decode word as string in ISO format.
//
std::string word_iso_string(Word w);

//
// Decode word as string in TEXT format.
//
std::string word_text_string(Word w);

//
// Decode word as filename in ISO format.
// Remove trailing spaces, convert to lowercase.
//
std::string word_iso_filename(Word w);

#endif // DUBNA_MACHINE_H
