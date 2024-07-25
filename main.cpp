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

#include <cstring>
#include <iostream>

#include "session.h"

//
// CLI options.
//
static const struct option long_options[] = {
    // clang-format off
    { "help",           no_argument,        nullptr,    'h' },
    { "help-libs",      no_argument,        nullptr,    'L' },
    { "help-commands",  no_argument,        nullptr,    'C' },
    { "version",        no_argument,        nullptr,    'V' },
    { "verbose",        no_argument,        nullptr,    'v' },
    { "keep",           no_argument,        nullptr,    'k' },
    { "random",         no_argument,        nullptr,    'r' },
    { "system",         no_argument,        nullptr,    's' },
    { "limit",          required_argument,  nullptr,    'l' },
    { "trace",          required_argument,  nullptr,    'T' },
    { "debug",          required_argument,  nullptr,    'd' },
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
    out << "    --help-libs             Show available libraries" << std::endl;
    out << "    --help-commands         Show available commands" << std::endl;
    out << "    -V, --version           Print the version number and exit" << std::endl;
    out << "    -v, --verbose           Verbose mode" << std::endl;
    out << "    -r, --random            Disable randomization" << std::endl;
    out << "    -s, --system            Enable system load list" << std::endl;
    out << "    -l NUM, --limit=NUM     Stop after so many instructions (default "
        << Session::get_default_limit() << ")" << std::endl;
    out << "    --keep                  Keep scratch files and raw plotter output" << std::endl;
    out << "    --trace=FILE            Redirect trace to the file" << std::endl;
    out << "    -d MODE, --debug=MODE   Select debug mode, default irm" << std::endl;
    out << "Debug modes:" << std::endl;
    out << "    i       Trace instructions" << std::endl;
    out << "    e       Trace extracodes" << std::endl;
    out << "    p       Trace printing (extracode e64)" << std::endl;
    out << "    f       Trace fetch" << std::endl;
    out << "    r       Trace registers" << std::endl;
    out << "    m       Trace memory read/write" << std::endl;
    out << "    d       Trace in dispak format, to stderr" << std::endl;
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
    // Enable wall clock by default.
    Session session;
    session.enable_entropy();

    // Parse command line options.
    for (;;) {
        switch (getopt_long(argc, argv, "-hVvl:tT:d:rs", long_options, nullptr)) {
        case EOF:
            break;

        case 0:
            continue;

        case 1:
            // Regular argument.
            session.set_job_file(optarg);
            continue;

        case 'h':
            // Show usage message and exit.
            print_usage(std::cout, prog_name);
            exit(EXIT_SUCCESS);

        case 'L':
            // Show available libraries.
            session.print_libraries(std::cout);
            exit(EXIT_SUCCESS);

        case 'C':
            // Show available commands.
            session.print_commands(std::cout);
            exit(EXIT_SUCCESS);

        case 'v':
            // Verbose.
            session.set_verbose(true);
            continue;

        case 'V':
            // Show version and exit.
            std::cout << "Version " << Session::get_version() << "\n";
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
            // Enable tracing of extracodes, to stdout by default.
            session.enable_trace("e");
            continue;

        case 'T':
            // Redirect tracing to a file.
            session.set_trace_file(optarg, "irm");
            continue;

        case 'd':
            // Set trace options.
            session.enable_trace(optarg);
            continue;

        case 'k':
            // Keep temporary files.
            session.preserve_temps();
            continue;

        case 'r':
            // Disable randomization.
            session.enable_entropy(false);
            continue;

        case 's':
            // Enable system load list.
            session.enable_system_load_list(true);
            continue;

        default:
            print_usage(std::cerr, prog_name);
            exit(EXIT_FAILURE);
        }
        break;
    }

    // Must specify a file to run.
    if (session.get_job_file().empty()) {
        print_usage(std::cerr, prog_name);
        exit(EXIT_FAILURE);
    }

    // Simulate the last session.
    session.run();
    session.finish();

    return session.get_exit_status();
}
