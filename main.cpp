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
#include <getopt.h>
#include <iostream>
#include <cstring>

#include "session.h"

//
// CLI options.
//
static const struct option long_options[] = {
    // clang-format off
    { "help",                   no_argument,        nullptr,    'h' },
    { "version",                no_argument,        nullptr,    'V' },
    { "verbose",                no_argument,        nullptr,    'v' },
    { "limit",                  required_argument,  nullptr,    'l' },
    { "trace",                  optional_argument,  nullptr,    't' },
    { nullptr },
    // clang-format on
};

//
// Print usage message.
//
static void print_usage(std::ostream &out, const char *prog_name)
{
    out << "Dubna Simulator, Version " << Session::get_version() << "\n";
    out << "Usage:" << std::endl;
    out << "    " << prog_name << " [options...] filename" << std::endl;
    out << "Input files:" << std::endl;
    out << "    filename                Job file in MS Dubna format" << std::endl;
    out << "Options:" << std::endl;
    out << "    -h, --help              Display available options" << std::endl;
    out << "    -V, --version           Print the version number and exit" << std::endl;
    out << "    -v, --verbose           Verbose mode" << std::endl;
    out << "    -l NUM, --limit=NUM     Stop after this amount of instructions (default "
        << Session::get_default_limit() << ")" << std::endl;
    out << "    -t, --trace             Generate trace to stdout" << std::endl;
    out << "    -t FILE, --trace=FILE   Generate trace to the file" << std::endl;
}

//
// Main routine of the simulator,
// when invoked from a command line.
//
int main(int argc, char *argv[])
{
    // Get the program name.
    const char *prog_name = strrchr(argv[0], '/');
    if (prog_name == nullptr) {
        prog_name = argv[0];
    } else {
        prog_name++;
    }

    // Instantiate the session.
    Session session;

    // Parse command line options.
    for (;;) {
        switch (getopt_long(argc, argv, "-hVvl:t::", long_options, nullptr)) {
        case EOF:
            break;
        case 0:
            continue;
        case 1:
            // Regular argument.
            session.set_exec_file(optarg);
            continue;
        case 'h':
            // Show usage message and exit.
            print_usage(std::cout, prog_name);
            exit(EXIT_SUCCESS);
        case 'v':
            // Verbose.
            session.set_verbose(true);
            continue;
        case 'V':
            // Show version and exit.
            std::cout << "Dubna Simulator Version " << Session::get_version() << "\n";
            exit(EXIT_SUCCESS);
        case 'l':
            // Limit the cycle count.
            try {
                session.set_limit(std::stoull(optarg));
            } catch (...) {
                std::cerr << "Bad --limit option: " << optarg << std::endl;
                print_usage(std::cerr, prog_name);
                exit(EXIT_FAILURE);
            }
            continue;
        case 't':
            // Enable tracing, with optional file argument.
            if (optarg) {
                session.set_trace_file(optarg);
            } else {
                session.enable_trace();
            }
            continue;
        default:
            print_usage(std::cerr, prog_name);
            exit(EXIT_FAILURE);
        }
        break;
    }

    // Must specify a file to run.
    if (session.get_exec_file().empty()) {
        print_usage(std::cerr, prog_name);
        exit(EXIT_FAILURE);
    }

    // Simulate the last session.
    session.run();
    session.finish();

    return session.get_exit_status();
}
