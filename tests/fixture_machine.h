//
// Gtest fixture 'dubna_machine': instantiate a fresh new Machine instance
// and random generator for each test.
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
#include "machine.h"
#include "util.h"

//
// Fixture with preallocated machine.
//
// For details, see: https://github.com/google/googletest/blob/main/docs/primer.md
//
class dubna_machine : public ::testing::Test {
    // DRAM is hidden from direct access.
    Memory memory;
    std::mt19937 random_generator;

protected:
    std::unique_ptr<Machine> machine;

    void SetUp() override
    {
        // Allocate fresh new Machine.
        machine = std::make_unique<Machine>(memory);

        // Reset the random generator.
        random_generator.seed(0);
    }

    void store_word(unsigned addr, uint64_t val) { machine->memory.store(addr, val); }

    //
    // Run test from Cern library.
    //
    void run_job_and_check_output(const std::string &expect_filename, const std::string &input)
    {
        std::string job_filename     = get_test_name() + ".dub";
        std::string expect_file_path = TEST_DIR "/" + expect_filename;

        // Create job file.
        create_file(job_filename, input);

        // This buffer must be static, otherwise in case of exception in machine->run(),
        // destructor of std::ios:base crashes trying to deallocate it.
        static std::ostringstream output;

        // Redirect stdout.
        std::streambuf *save_cout = std::cout.rdbuf();
        std::cout.rdbuf(output.rdbuf());

        // Run the job.
        machine->load_script(job_filename);
        machine->boot_ms_dubna();
        ASSERT_NO_THROW(machine->run());
        machine->finish();

        // Get output.
        std::cout.rdbuf(save_cout);
        std::string result = output.str();

        // Check result.
        auto expect = file_contents(expect_file_path);
        //check_output(result, expect);
        EXPECT_EQ(result, expect);

        if (::testing::Test::HasFailure()) {
            // Save result for debug.
            create_file(expect_filename, result);
        }
        output.clear();
    }

    //
    // Run test from Cern library.
    //
    void test_cernlib(unsigned lib_num, const std::string &file_base)
    {
        std::string job_filename    = get_test_name() + ".dub";
        std::string input_dir       = (lib_num == 1) ? (TEST_DIR "/lib1/") : (TEST_DIR "/lib2/");
        std::string input_filename  = input_dir + file_base + ".f";
        std::string expect_filename = input_dir + "/expect_" + file_base + ".txt";
        std::string prolog          = "*name " + file_base + "\n" +
                                      "*tape:12/librar,32" + "\n" + // for F311
                                      "*library:1,2,3,5,12,23\n" +
                                      "*call setftn:one,long\n" +
                                      "*no list\n" +
                                      "*no load list\n";
        std::string epilog          = "*end file\n";

        // This buffer must be static, otherwise in case of exception in machine->run(),
        // destructor of std::ios:base crashes trying to deallocate it.
        static std::ostringstream output;

        // Create job file.
        create_file(job_filename, prolog, input_filename, epilog);

        // Redirect stdout.
        std::streambuf *save_cout = std::cout.rdbuf();
        std::cout.rdbuf(output.rdbuf());

        // Run the job.
        machine->load_script(job_filename);
        machine->boot_ms_dubna();
        ASSERT_NO_THROW(machine->run());
        machine->finish();

        // Get output.
        std::cout.rdbuf(save_cout);
        std::string result = output.str();

        // Check result.
        auto expect = file_contents(expect_filename);
        //check_output(result, expect);
        EXPECT_EQ(result, expect);

        if (::testing::Test::HasFailure()) {
            // Save result for debug.
            create_file("expect_" + file_base + ".txt", result);
        }
        output.clear();
    }
};
