//
// Gtest fixture 'dubna_session': instantiate a fresh new Session instance
// for each test.
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
#include "util.h"

//
// Fixture with preallocated session.
//
// For details, see: https://github.com/google/googletest/blob/main/docs/primer.md
//
class dubna_session : public ::testing::Test {
protected:
    std::unique_ptr<Session> session;

    void SetUp() override
    {
        // Allocate fresh new Session.
        session = std::make_unique<Session>();

        // Set path to the disk images.
        EXPECT_EQ(setenv("BESM6_PATH", TEST_DIR "/../tapes", 1), 0);
    }

    //
    // Run a job and return captured output.
    //
    std::string run_job_and_capture_output(const std::string &input)
    {
        // Create job file.
        std::string job_filename = get_test_name() + ".dub";
        create_file(job_filename, input);
        session->set_job_file(job_filename);

        // Redirect stdout.
        std::streambuf *save_cout = std::cout.rdbuf();
        std::ostringstream output;
        std::cout.rdbuf(output.rdbuf());

        // Run the job.
        session->run();

        // Return output.
        std::cout.rdbuf(save_cout);
        return output.str();
    }

    //
    // Compare contents of two text files.
    //
    void check_contents(const std::string &output_str, const std::string &expect_str)
    {
        std::stringstream output(output_str);
        std::stringstream expect(expect_str);

        // Compare line by line.
        for (unsigned lineno = 1; expect.good(); lineno++) {
            EXPECT_TRUE(output.good()) << "Output is too short";

            std::string output_line;
            getline(output, output_line);
            if (output_line == "" && output.eof())
                break;

            // Remove trailing spaces.
            output_line.resize(1 + output_line.find_last_not_of(' '));

            std::string expect_line;
            getline(expect, expect_line);
            EXPECT_EQ(output_line, expect_line)
                << "line #" << lineno;
        }
    }

    //
    // Compare output of the program after *EXECUTE.
    // Ignore header and footer.
    //
    std::string extract_after_execute(const std::string &str)
    {
        // Skip all output until *EXECUTE word, but keep the newline.
        std::string output = str.substr(str.find("*EXECUTE"));

        // Remove everything after the program output.
        output.resize(output.find("------------------------------------------------------------"));

        //std::cout << "--- '" << output << "'\n";
        return output;
    }
};
