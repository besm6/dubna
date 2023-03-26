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
#include <vector>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <unistd.h>

#include "session.h"
#include "machine.h"

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
#ifndef _WIN32
        // Enable progress message only when error output goes to a user terminal.
        // No progress message on Windows.
        if (isatty(STDERR_FILENO)) {
            // Print a progress message on stderr every few seconds.
            machine.enable_progress_message(true);
        }
#endif
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

    std::string get_job_file() const { return job_file; }

    //
    // Run simulation session with given parameters.
    //
    void run()
    {
        // Load requested ELF file.
        try {
            std::cout << "Read " << job_file << std::endl;
            machine.load(job_file);

        } catch (std::exception &ex) {
            std::cerr << ex.what() << std::endl;
            std::cerr << "Reading input FAILED." << std::endl;
            exit_status = EXIT_FAILURE;
            return;
        }

        try {
            // Run simulation.
            std::cout << "Simulate." << std::endl;
            auto t0 = std::chrono::steady_clock::now();
            machine.run();
            auto t1 = std::chrono::steady_clock::now();

            // Get duration in microseconds.
            auto usec = (double) std::chrono::duration_cast<std::chrono::microseconds>(t1 - t0).count();
            if (usec < 1)
                usec = 1;

            // Compute the simulation speed.
            auto sec = usec / 1000000.0;
            auto instr_count = Machine::get_instr_count();
            long instr_per_sec = std::lround(1000000.0 * instr_count / usec);

            // Print footer.
            print_footer(std::cout, sec, instr_per_sec);

            if (Machine::trace_enabled()) {
                // Print also to the trace file.
                auto &out = Machine::get_trace_stream();
                if (&out != &std::cout) {
                    out << "----------------\n";
                    print_footer(out, sec, instr_per_sec);
                }
            }
        } catch (std::exception &ex) {
            std::cerr << ex.what() << std::endl;
            std::cerr << "Simulation FAILED." << std::endl;
            exit_status = EXIT_FAILURE;
        }
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
    void set_verbose(bool on)
    {
        machine.set_verbose(on);
    }

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
        Machine::get_trace_stream() << "Version: " << VERSION_STRING << "\n";
    }

    //
    // Fail after the specified number of instructions.
    //
    void set_limit(uint64_t count)
    {
        machine.set_limit(count);
    }

    //
    // Backdoor access to DRAM memory.
    // No tracing.
    //
    void mem_write(const Words &input, uint64_t addr)
    {
        memory.write_words(input, addr);
    }

    void mem_read(Words &output, unsigned nrows, uint64_t addr)
    {
        memory.read_words(output, nrows, addr);
    }

private:
    //
    // Print footer.
    //
    static void print_footer(std::ostream &out, double sec, long instr_per_sec)
    {
        auto instr_count = Machine::get_instr_count();
        int time_precision = (sec < 1) ? 3 : (sec < 10) ? 2 : 1;

        out << "   Elapsed time: " << std::fixed << std::setprecision(time_precision)
            << sec << " seconds" << std::setprecision(6) << std::endl;
        out << "      Simulated: " << instr_count << " instructions" << std::endl;
        out << "Simulation rate: " << std::fixed << instr_per_sec << " instructions/sec"
            << std::setprecision(6) << std::endl;
    }
};

//
// Instaltiate the Session object.
// Allocate the internal implementation.
//
Session::Session() :
    internal(std::make_unique<Session::Hidden>())
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
