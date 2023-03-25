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
#ifndef DUBNA_SESSION_H
#define DUBNA_SESSION_H

#include <string>
#include <memory>

#include "besm6_arch.h"

//
// External interface to the simulator.
//
class Session {
public:
    // Constructor.
    explicit Session();

    // Set name of executable file.
    void set_exec_file(const char *filename);
    void set_exec_file(const std::string &filename) { set_exec_file(filename.c_str()); }
    std::string get_exec_file();

    // Run simulation session with given parameters.
    void run();

    // Finish simulation.
    void finish();

    // Get status of simulation: either EXIT_SUCCESS (0) or
    // EXIT_FAILURE in case of errors.
    int get_exit_status();

    // Fail after the specified number of instructions.
    void set_limit(uint64_t count);
    static uint64_t get_default_limit();

    // Enable verbose mode: print more details to the trace log.
    void set_verbose(bool on = true);

    // Enable a trace log to stdout or to the specified file.
    void enable_trace(bool on = true);
    void set_trace_file(const char *filename);

    // Get the number of simulated instructions.
    uint64_t get_instr_count();

    // Get version of the simulator.
    static const char *get_version();

    // Access to memory.
    void mem_write(const Words &input, unsigned addr);
    void mem_read(Words &output, unsigned nwords, unsigned addr);

    // Destructor.
    virtual ~Session();

    // Delete copy/move constructors.
    Session(const Session&) = delete;
    Session& operator=(const Session&) = delete;
    Session(Session&&) = delete;
    Session& operator=(Session&&) = delete;

private:
    // Use a "pImpl" idiom to hide implementation details.
    // Place the object representation in a separate class,
    // accessed through an opaque pointer.
    class Hidden;
    std::unique_ptr<Hidden> internal;
};

#endif // DUBNA_SESSION_H
