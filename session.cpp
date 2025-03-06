//
// Class Session: collect parameters for simulation and run the Machine instance.
// Hide all internals from user.
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
#include "session.h"

#include <unistd.h>

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>

#include "machine.h"

#ifdef _WIN32
#include <io.h>
#define ISATTY _isatty
#define FILENO _fileno
#else
#include <unistd.h>
#define ISATTY isatty
#define FILENO fileno
#endif

//
// Internal implementation of the simulation session, hidden from user.
//
class Session::Hidden {
private:
    Memory memory;
    Machine machine{ memory };

    // A name of the file to run.
    std::string job_file;

    // Status of the simulation.
    int exit_status{ EXIT_SUCCESS };

public:
    //
    // Instantiate the session.
    //
    explicit Hidden()
    {
        // Enable progress message only when error output goes to a user terminal.
        if (ISATTY(FILENO(stdin))) {
            // Print a progress message on stderr every few seconds.
            machine.enable_progress_message(true);
        }
    }

    //
    // Get status of simulation: either EXIT_SUCCESS (0) or
    // EXIT_FAILURE in case of errors.
    //
    int get_exit_status() const { return exit_status; }

    //
    // Set name of job file.
    //
    void set_job_file(const char *filename)
    {
        if (!job_file.empty()) {
            std::cerr << "Too many job files: " << filename << std::endl;
            ::exit(EXIT_FAILURE);
        }
        job_file = filename;
    }

    const std::string &get_job_file() const { return job_file; }

    //
    // Run simulation session with given parameters.
    //
    void run()
    {
        try {
            unsigned file_offset{};
            if (machine.is_overlay(job_file, &file_offset)) {
                run_overlay(file_offset);
            } else {
                run_script();
            }
        } catch (const std::exception &ex) {
            // Print exception message.
            std::cerr << "Error: " << ex.what() << std::endl;
            exit_status = EXIT_FAILURE;
        } catch (...) {
            // Assuming the exception message already printed.
            exit_status = EXIT_FAILURE;
        }
    }

    //
    // Run simulation session with given parameters.
    //
    void run_script()
    {
        // Load job control script.
        machine.load_script(job_file);

        // Boot the monitoring system.
        machine.boot_ms_dubna();
        if (Machine::trace_enabled()) {
            std::cout << "------------------------------------------------------------\n";
        }

        // Run simulation.
        using namespace std::chrono;
        auto t0 = steady_clock::now();
        machine.run();
        auto t1 = steady_clock::now();

        // Get duration in microseconds.
        auto usec = (double)duration_cast<microseconds>(t1 - t0).count();
        if (usec < 1)
            usec = 1;

        // Compute the simulation speed.
        auto sec           = usec / 1000000.0;
        auto instr_count   = Machine::get_instr_count();
        long instr_per_sec = std::lround(1000000.0 * instr_count / usec);

        // Print footer.
        print_footer(std::cout, sec, instr_per_sec);

        if (Machine::trace_enabled()) {
            // Print also to the trace file.
            auto &out = Machine::get_trace_stream();
            if (&out != &std::cout) {
                print_footer(out, sec, instr_per_sec);
            }
        }
        machine.finish();
    }

    //
    // Run simulation session with given parameters.
    //
    void run_overlay(unsigned file_offset)
    {
        // Load input data.
        if (ISATTY(FILENO(stdin))) {
            // Load empty file.
            std::stringstream input_data;
            input_data << "*end file\n";
            machine.load_script(input_data);
        } else {
            machine.load_script(std::cin);
        }

        // Load binary program.
        machine.boot_overlay(job_file, file_offset);

        // Run simulation.
        machine.run();
        machine.finish();
    }

    //
    // Finish simulation.
    // Close trace files.
    //
    void finish()
    {
        // Finish the trace output.
        Machine::close_trace();
    }

    //
    // Enable verbose mode.
    // Print more details to the trace log.
    //
    void set_verbose(bool on) { machine.set_verbose(on); }

    //
    // Keep temporary files.
    //
    void preserve_temps(bool on) { machine.preserve_temps(on); }

    //
    // Enable wall clock and other sources of inpredictability.
    //
    void enable_entropy(bool on) { machine.enable_entropy(on); }

    //
    // Enable verbose loader actions from the start.
    //
    void enable_system_load_list(bool on) { machine.enable_system_load_list(on); }

    //
    // Enable trace log to stdout.
    //
    void enable_trace(const char *mode)
    {
        if (mode && *mode) {
            Machine::enable_trace(mode);
        } else {
            Machine::close_trace();
        }
    }

    //
    // Enable trace log to the specified file.
    //
    void set_trace_file(const char *filename, const char *default_mode)
    {
        Machine::redirect_trace(filename, default_mode);
        Machine::get_trace_stream() << "Dubna Simulator Version: " << VERSION_STRING << "\n";
    }

    //
    // Fail after the specified number of instructions.
    //
    void set_limit(uint64_t count) { machine.set_limit(count); }

    //
    // Backdoor access to DRAM memory.
    // No tracing.
    //
    void mem_write(const Words &input, uint64_t addr) { memory.write_words(input, addr); }

    void mem_read(Words &output, unsigned nrows, uint64_t addr)
    {
        memory.read_words(output, nrows, addr);
    }

    //
    // Show available libraries.
    //
    void print_libraries(std::ostream &out)
    {
        // Mount tape image 9 as disk 30, read only.
        machine.disk_mount_readonly(030, machine.TAPE_MONSYS);

        // Read zone 6.
        static const unsigned ZONE = 6;
        machine.disk_io('r', 0, ZONE, 0, 0x0, 1024);

        // Print table of libraries.
        static const unsigned OFFSET = 01720;

        out << "Library        Tape         Zone\n";
        out << "--------------------------------\n";
        for (unsigned libno = 0; libno < 030; libno++) {
            Word tape_name = machine.mem_load(OFFSET + libno);
            if (tape_name == 0) {
                // No library with this number.
                continue;
            }
            Word location = machine.mem_load(OFFSET + libno + 030);

            out << "*library:" << std::left << std::setw(2) << std::oct << libno << "    "
                << std::setw(10) << tape_name_string(tape_name) << "   " << std::right
                << std::setw(4) << std::setfill('0') << (location >> 30) << std::setfill(' ')
                << std::dec << std::endl;
        }
    }

    //
    // Show resident entries of the monitor system kernel..
    //
    void print_resident(std::ostream &out)
    {
        // Mount tape image 9 as disk 30, read only.
        machine.disk_mount_readonly(030, machine.TAPE_MONSYS);

        // Read zone 1, sector 2.
        static const unsigned ZONE   = 1;
        static const unsigned SECTOR = 2;
        static const unsigned BASE   = 0x1000;
        machine.disk_io('r', 0, ZONE, SECTOR, BASE, 256);

        // Fix damaged IОLISТ*.
        Word iolist_star = machine.mem_load(BASE + 0363);
        machine.mem_store(BASE + 0100, iolist_star);

        out << "Name       Address Size    Flags\n";
        out << "----------------------------------\n";
        for (unsigned addr = 0; addr < 256; addr += 2) {
            Word name = machine.mem_load(BASE + addr);
            if (name == 0) {
                break;
            }
            Word location    = machine.mem_load(BASE + addr + 1);
            unsigned address = location & 077777;
            unsigned nwords  = (location >> 15) & 077777;
            unsigned flags   = (location >> 30) & 0777777;

            out << word_text_string(name) << "   " << std::oct << std::left << std::showbase
                << std::setw(6) << address << "  ";
            if (!nwords && !flags) {
                out << "-       entry";
            } else {
                if (nwords > 7) {
                    out << std::setw(6) << nwords;
                } else {
                    out << std::setw(6) << std::noshowbase << nwords << std::showbase;
                }
                if (flags) {
                    out << "  " << flags;
                } else {
                    out << "  data";
                }
            }
            out << std::dec << std::right << std::noshowbase << std::endl;
        }
    }

    //
    // Show available commands.
    //
    void print_commands(std::ostream &out)
    {
        // Mount tape image 9 as disk 30, read only.
        machine.disk_mount_readonly(030, machine.TAPE_MONSYS);

        // Create a list of available commands.
        std::vector<std::string> list;
        get_commands(list, 030, 01600, 01660);
        get_commands(list, 024, 0340, 0374);
        get_commands(list, 037, 060, 0110);

        // Sort the list, remove duplicates.
        std::sort(list.begin(), list.end());
        auto last = std::unique(list.begin(), list.end());
        list.erase(last, list.end());

        print_columns(out, list, 6);
    }

private:
    //
    // Print footer.
    //
    static void print_footer(std::ostream &out, double sec, long instr_per_sec)
    {
        auto instr_count   = Machine::get_instr_count();
        int time_precision = (sec < 1) ? 3 : (sec < 10) ? 2 : 1;

        out << "------------------------------------------------------------" << std::endl;
        out << "   Elapsed time: " << std::fixed << std::setprecision(time_precision) << sec
            << " seconds" << std::setprecision(6) << std::endl;
        out << "      Simulated: " << instr_count << " instructions" << std::endl;
        out << "Simulation rate: " << std::fixed << instr_per_sec << " instructions/sec"
            << std::setprecision(6) << std::endl;
    }

    //
    // Extract available commands from given zone and address range.
    //
    void get_commands(std::vector<std::string> &list, const unsigned zone,
                      const unsigned addr_start, const unsigned addr_finish)
    {
        machine.disk_io('r', 0, zone, 0, 0x0, 1024);
        for (unsigned addr = addr_start; addr < addr_finish; addr++) {
            Word word      = machine.mem_load(addr);
            char first_sym = word >> 40;
            if (first_sym == '*') {
                // First symbol must be star.
                std::string str{ first_sym,          (char)(word >> 32), (char)(word >> 24),
                                 (char)(word >> 16), (char)(word >> 8),  (char)word };
                list.push_back(str);
            }
        }
    }

    //
    // Print list in columns.
    //
    void print_columns(std::ostream &out, const std::vector<std::string> &list,
                       const unsigned num_columns)
    {
        unsigned const num_rows  = (list.size() + num_columns - 1) / num_columns;
        unsigned const col_width = 12;

        for (unsigned row = 0; row < num_rows; row++) {
            for (unsigned col = 0; col < num_columns; col++) {
                unsigned index = (col * num_rows) + row;
                if (index >= list.size()) {
                    break;
                }
                bool is_last = (index + num_rows) >= list.size();
                if (is_last) {
                    out << list[index];
                } else {
                    out << std::setw(col_width) << std::left << list[index];
                }
            }
            out << std::endl;
        }
    }
};

//
// Instaltiate the Session object.
// Allocate the internal implementation.
//
Session::Session() : internal(std::make_unique<Session::Hidden>())
{
}

//
// Destructor: implicitly delete the hidden object.
//
Session::~Session() = default;

//
// Set name of job file.
//
void Session::set_job_file(const char *filename)
{
    internal->set_job_file(filename);
}

std::string Session::get_job_file()
{
    return internal->get_job_file();
}

//
// Get status of simulation: either EXIT_SUCCESS (0) or
// EXIT_FAILURE in case of errors.
//
int Session::get_exit_status()
{
    return internal->get_exit_status();
}

//
// Run simulation session with given parameters.
//
void Session::run()
{
    internal->run();
}

//
// Finish simulation.
//
void Session::finish()
{
    internal->finish();
}

//
// Enable a trace log to stdout or to the specified file.
//
void Session::enable_trace(const char *mode)
{
    internal->enable_trace(mode);
}

void Session::set_trace_file(const char *filename, const char *default_mode)
{
    internal->set_trace_file(filename, default_mode);
}

//
// Enable verbose mode.
//
void Session::set_verbose(bool on)
{
    internal->set_verbose(on);
}

//
// Fail after the specified number of instructions.
//
void Session::set_limit(uint64_t count)
{
    internal->set_limit(count);
}

//
// Query the default limit of instructions.
//
uint64_t Session::get_default_limit()
{
    return Machine::get_default_limit();
}

//
// Keep temporary files.
//
void Session::preserve_temps(bool on)
{
    internal->preserve_temps(on);
}

//
// Enable wall clock and other sources of inpredictability.
//
void Session::enable_entropy(bool on)
{
    internal->enable_entropy(on);
}

//
// Enable verbose loader actions from the start.
//
void Session::enable_system_load_list(bool on)
{
    internal->enable_system_load_list(on);
}

//
// Get the number of simulated instructions.
//
uint64_t Session::get_instr_count()
{
    return Machine::get_instr_count();
}

//
// Get version of the simulator.
//
const char *Session::get_version()
{
    // Return string, obtained from CMakeLists.txt.
    return VERSION_STRING;
}

//
// Access to DRAM memory.
//
void Session::mem_write(const Words &input, unsigned addr)
{
    internal->mem_write(input, addr);
}

void Session::mem_read(Words &output, unsigned nrows, unsigned addr)
{
    internal->mem_read(output, nrows, addr);
}

//
// Show available libraries.
//
void Session::print_libraries(std::ostream &out)
{
    internal->print_libraries(out);
}

//
// Show available commands.
//
void Session::print_commands(std::ostream &out)
{
    internal->print_commands(out);
}

//
// Show resident routines.
//
void Session::print_resident(std::ostream &out)
{
    internal->print_resident(out);
}
