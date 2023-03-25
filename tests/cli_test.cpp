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
#include <cstdio>
#include <fstream>
#include "util.h"

TEST(cli, usage)
{
    // Run simulator via shell.
    FILE *pipe = popen("../dubna --help", "r");
    ASSERT_TRUE(pipe != nullptr);

    // Capture the output.
    std::string result = stream_contents(pipe);
    std::cout << result;

    // Check exit code.
    int exit_status = pclose(pipe);
    int exit_code = WEXITSTATUS(exit_status);
    ASSERT_NE(exit_status, -1);
    ASSERT_EQ(exit_code, 0);

    // Check output.
    EXPECT_NE(result.find("Version"), std::string::npos);
    EXPECT_NE(result.find("Usage:"), std::string::npos);
    EXPECT_NE(result.find("Options:"), std::string::npos);
}

TEST(cli, version)
{
    // Run simulator via shell.
    FILE *pipe = popen("../dubna --version", "r");
    ASSERT_TRUE(pipe != nullptr);

    // Capture the output.
    std::string result = stream_contents(pipe);
    std::cout << result;

    // Check exit code.
    int exit_status = pclose(pipe);
    int exit_code = WEXITSTATUS(exit_status);
    ASSERT_NE(exit_status, -1);
    ASSERT_EQ(exit_code, 0);

    // Check output.
    EXPECT_NE(result.find("Version"), std::string::npos);
    EXPECT_NE(result.find("."), std::string::npos);
    EXPECT_NE(result.find("-"), std::string::npos);
}

TEST(cli, DISABLED_trace_end_file)
{
    std::string base_name = get_test_name();
    std::string job_filename = base_name + ".dub";
    std::string trace_filename = base_name + ".trace";
    std::string command_line = "../dubna --trace=" + trace_filename + " " + job_filename;

    create_file(job_filename,
        "*name e74\n"
        "*end file\n"
    );

    // Run simulator via shell.
    FILE *pipe = popen(command_line.c_str(), "r");
    ASSERT_TRUE(pipe != nullptr);

    // Capture the output as vector of strings.
    auto output = stream_contents(pipe);
    std::cout << output;

    // Check exit code.
    int exit_status = pclose(pipe);
    int exit_code = WEXITSTATUS(exit_status);
    ASSERT_NE(exit_status, -1);
    ASSERT_EQ(exit_code, 0);

    // Read the trace.
    auto trace = file_contents_split(trace_filename);

    // Check output.
    //using std::string_literals::operator""s;
    //using namespace std::string_literals;
    ASSERT_GE(trace.size(), 4);
    EXPECT_TRUE(starts_with(trace[0], "Version"));
    EXPECT_STREQ(trace[1].c_str(), "TODO");
    EXPECT_STREQ(trace[2].c_str(), "TODO");
    EXPECT_STREQ(trace[3].c_str(), "----------------");
}
